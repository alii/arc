import arc/vm/builtins/common
import arc/vm/builtins/iter_protocol
import arc/vm/exec/generators
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/internal/ordered_entries
import arc/vm/key.{Index, Named}
import arc/vm/ops/array_iterator
import arc/vm/ops/object
import arc/vm/ops/property
import arc/vm/state.{type Heap, type State, type StepExit, State, Threw}
import arc/vm/value.{
  type JsValue, type Ref, ArrayObject, GeneratorObject, JsObject, JsUndefined,
  ObjectSlot,
}
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string

// ============================================================================
// Array manipulation helpers
// ============================================================================

/// Internal helper for ArrayFromWithHoles opcode — assigns values to
/// non-hole positions in a sparse array literal like `[1,,3]`.
///
/// Related to ES2024 §13.2.4.1 ArrayLiteral evaluation:
///   - ElementList : ElementList , Elision_opt AssignmentExpression
///     uses ArrayAccumulation which skips elision slots.
///
/// This function zips stack values with their non-hole indices.
/// `holes` is a sorted-ascending list of indices to skip. Walks index
/// 0,1,2,... — when index matches head of holes, skip it (consume from
/// holes); otherwise pair next value with that index. Accumulates in
/// reverse; caller doesn't care about order since result feeds a dict.
pub fn assign_non_hole_indices(
  values: List(JsValue),
  holes: List(Int),
  index: Int,
  acc: List(#(Int, JsValue)),
) -> List(#(Int, JsValue)) {
  case values {
    [] -> acc
    [v, ..vs] ->
      case holes {
        [h, ..hs] if h == index ->
          assign_non_hole_indices(values, hs, index + 1, acc)
        _ -> assign_non_hole_indices(vs, holes, index + 1, [#(index, v), ..acc])
      }
  }
}

/// Increment array length WITHOUT setting any element (creates a hole).
/// ArrayPushHole opcode helper.
///
/// Related to ES2024 §10.4.2.4 ArraySetLength — when length is increased
/// without setting an element, the spec allows holes (missing properties)
/// in the index range. DenseElements represents holes natively via its
/// JsUninitialized default, so no representation change is needed — the
/// gap between old length and new length is implicitly a hole. Later
/// ArrayPush appends past the hole leave it intact (tree_array default
/// slots stay unset). forEach/map correctly skip these per §23.1.3.
pub fn grow_array_length(h: Heap(host), ref: Ref) -> Heap(host) {
  use slot <- heap.update(h, ref)
  let assert ObjectSlot(kind: ArrayObject(length:), ..) = slot
    as "array op target is not an ArrayObject"
  ObjectSlot(..slot, kind: ArrayObject(length + 1))
}

/// Append one value to the end of an array (ArrayPush opcode helper).
/// Reads current length, sets element at that index, increments length.
/// The ref always names a compiler-emitted array literal, so a non-array slot
/// is a wiring bug — crash rather than silently dropping the element.
pub fn push_onto_array(h: Heap(host), ref: Ref, val: JsValue) -> Heap(host) {
  use slot <- heap.update(h, ref)
  let assert ObjectSlot(kind: ArrayObject(length:), elements:, ..) = slot
    as "array op target is not an ArrayObject"
  ObjectSlot(
    ..slot,
    kind: ArrayObject(length + 1),
    elements: elements.set(elements, length, val),
  )
}

/// Batch-append to target array with ONE heap read + ONE heap write for
/// `target_ref`, regardless of how many elements `fold` appends. Caller's
/// fold receives (heap, elements, length) and returns the same — the heap
/// param lets it do side allocations (e.g. entry-pair arrays for Map/Set
/// spreads) which never touch `target_ref`, so capturing the target slot
/// once up-front is safe. Replaces the old per-element push_onto_array
/// pattern which did n × (dict.get + dict.insert + ObjectSlot alloc).
fn batch_append(
  h: Heap(host),
  target_ref: Ref,
  fold: fn(Heap(host), value.JsElements, Int) ->
    #(Heap(host), value.JsElements, Int),
) -> Heap(host) {
  let assert Some(ObjectSlot(kind: ArrayObject(length:), elements:, ..) as slot) =
    heap.read(h, target_ref)
    as "array op target is not an ArrayObject"
  let #(h, elements, length) = fold(h, elements, length)
  heap.write(
    h,
    target_ref,
    ObjectSlot(..slot, kind: ArrayObject(length:), elements:),
  )
}

/// Append a list of values onto the target array — one heap read, one
/// heap write. Use this instead of `list.fold(.., push_onto_array)`.
pub fn append_list_to_array(
  h: Heap(host),
  target_ref: Ref,
  values: List(JsValue),
) -> Heap(host) {
  use h, els, len <- batch_append(h, target_ref)
  let #(els, len) =
    list.fold(values, #(els, len), fn(acc, v) {
      let #(els, len) = acc
      #(elements.set(els, len, v), len + 1)
    })
  #(h, els, len)
}

/// Bulk-append a range [idx, end) from source elements onto the target array.
/// Used for the array fast-path in ArraySpread — avoids creating an
/// ArrayIteratorObject when the source is a plain array.
pub fn append_range_to_array(
  h: Heap(host),
  target_ref: Ref,
  src_elements: value.JsElements,
  idx: Int,
  end: Int,
) -> Heap(host) {
  use h, els, len <- batch_append(h, target_ref)
  // elements.get returns JsUndefined for holes — matches the spec's
  // array iterator behavior (CreateIterResultObject(Get(array, idx), false)).
  let #(els, len) = copy_range(src_elements, idx, end, els, len)
  #(h, els, len)
}

fn copy_range(
  src: value.JsElements,
  idx: Int,
  end: Int,
  dst: value.JsElements,
  dst_len: Int,
) -> #(value.JsElements, Int) {
  case idx >= end {
    True -> #(dst, dst_len)
    False ->
      copy_range(
        src,
        idx + 1,
        end,
        elements.set(dst, dst_len, elements.get(src, idx)),
        dst_len + 1,
      )
  }
}

/// Collect typed-array elements [from, to) read through the live backing
/// store (out-of-range reads decode as undefined, matching detached reads).
fn typed_array_values_range(
  h: Heap(host),
  buffer: Ref,
  elem_kind: value.TypedArrayKind,
  byte_offset: Int,
  length: Int,
  from: Int,
  to: Int,
  acc: List(JsValue),
) -> List(JsValue) {
  case from >= to {
    True -> list.reverse(acc)
    False -> {
      let v =
        object.typed_array_element(
          h,
          buffer,
          elem_kind,
          byte_offset,
          length,
          from,
        )
        |> option.unwrap(JsUndefined)
      typed_array_values_range(
        h,
        buffer,
        elem_kind,
        byte_offset,
        length,
        from + 1,
        to,
        [v, ..acc],
      )
    }
  }
}

/// §23.1.5.1: shape a drained run of iteration results for the iterator's
/// kind — indices ("key"), the elements ("value"), or fresh [index, element]
/// pair arrays ("key+value"). `start` is the source index of the first value.
///
/// One drained element is shaped by exactly the same `array_iterator.shape_result`
/// the one-at-a-time stepper uses, so the spread fast path and the slow path
/// cannot drift apart. For a "value" iterator that shaping is the identity, so
/// the drained list is handed back untouched — this fast path exists precisely
/// to avoid per-element work.
fn shape_iter_values(
  h: Heap(host),
  array_proto: Ref,
  iter_kind: value.ArrayIterKind,
  start: Int,
  values: List(JsValue),
) -> #(Heap(host), List(JsValue)) {
  case iter_kind {
    value.ArrayIterValues -> #(h, values)
    value.ArrayIterKeys | value.ArrayIterEntries -> {
      let #(h, shaped) =
        list.index_fold(values, #(h, []), fn(acc, elem, i) {
          let #(h, shaped) = acc
          let #(h, out) =
            array_iterator.shape_result(
              h,
              array_proto,
              iter_kind,
              start + i,
              elem,
            )
          #(h, [out, ..shaped])
        })
      #(h, list.reverse(shaped))
    }
  }
}

/// Shared drain for the Set-iterator and Map-iterator spread fast paths — both
/// iterate an `OrderedEntries` store, so the shape is identical: take the live
/// entries of the source collection from `cursor` (none once the iterator is
/// exhausted), append one element per entry with `build`, then latch the
/// iterator to `latched_kind` (itself with `done: True`) so further `.next()`
/// calls answer done. `build` only allocates entry-pair arrays, never touching
/// `target_ref`, so `batch_append`'s single read/write of the target stays valid.
/// A Set iterator's `source` is always a Set (and a Map iterator's a Map), so
/// accepting either store kind here can't mis-bind.
fn spread_collection_iterator(
  state: State(host),
  src_ref: Ref,
  target_ref: Ref,
  source: Ref,
  cursor: Int,
  done: Bool,
  latched_kind: state.ExoticKind(host),
  build: fn(#(Heap(host), value.JsElements, Int), #(value.MapKey, JsValue)) ->
    #(Heap(host), value.JsElements, Int),
) -> Result(State(host), StepExit(host)) {
  let entries = case done {
    True -> []
    False ->
      case heap.read(state.heap, source) {
        Some(ObjectSlot(kind: value.SetObject(store:), ..))
        | Some(ObjectSlot(kind: value.MapObject(store:), ..)) ->
          ordered_entries.live_entries_from(store, cursor)
        _ -> []
      }
  }
  let heap = {
    use h, els, len <- batch_append(state.heap, target_ref)
    list.fold(entries, #(h, els, len), build)
  }
  let heap = heap.update_kind(heap, src_ref, latched_kind)
  Ok(State(..state, heap:))
}

/// Drain an iterable into the target array (ArraySpread opcode helper).
///
/// Per ES §13.2.4.1 ArrayAccumulation (SpreadElement):
///   1. spreadObj = ? Evaluate(AssignmentExpression)
///   2. iteratorRecord = ? GetIterator(spreadObj, sync)
///   3. Repeat: next = ? IteratorStepValue; if done return; CreateDataProperty(A, idx, next); idx++
///
/// Layout, from most to least specialized:
///   - GUARDED fast paths for Array/Arguments, ArrayIterator, Set/Map + their
///     iterators, Generator and String: a pure raw-heap copy/drain, taken only
///     when `intrinsic_iterator_guard` — a non-observable heap walk of the
///     source's @@iterator AND the `next` the fresh iterator would resolve —
///     proves it is equivalent to the spec's GetIterator + IteratorStepValue
///     loop. Array/Arguments additionally require the observable `length` to
///     match the stored one (`spread_length_of`) and no holes / index
///     overrides; ArrayIterator applies the same hole/override protector to
///     its source (`raw_iterator_source_copy_is_safe`). EVERY failed guard
///     falls to `spread_via_iterator`.
///   - `spread_via_iterator` — the real §7.4.3 GetIterator + §7.4.8
///     IteratorStepValue dance. It handles EVERYTHING with a callable
///     @@iterator (class instances with a generator @@iterator, Proxies,
///     userland `{ [Symbol.iterator]() {...} }` objects, patched
///     Array.prototype[Symbol.iterator], ...) and is where every failed
///     guard lands. Only an object whose @@iterator is missing or not
///     callable TypeErrors here.
pub fn spread_into_array(
  state: State(host),
  target_ref: Ref,
  iterable: JsValue,
  execute_inner: generators.ExecuteInnerFn(host),
) -> Result(State(host), StepExit(host)) {
  case iterable {
    JsObject(src_ref) ->
      case heap.read(state.heap, src_ref) {
        Some(ObjectSlot(
          kind: ArrayObject(_) as kind,
          elements:,
          properties:,
          ..,
        ))
        | Some(ObjectSlot(
            kind: value.ArgumentsObject(_) as kind,
            elements:,
            properties:,
            ..,
          )) ->
          case
            intrinsic_iterator_guard(
              state.heap,
              src_ref,
              array_values,
              state.builtins.array_iterator_proto,
              array_iterator_next,
            ),
            spread_length_of(kind, properties)
          {
            // (a) @@iterator was overridden (the raw copy would ignore it
            // entirely — `a[Symbol.iterator] = function*(){yield 9}` must
            // NOT still spread a's elements), (b)
            // %ArrayIteratorPrototype%.next was patched (the spec loop
            // calls it per element), or (c) an `arguments.length = 0`
            // assignment made the observable length disagree with the raw
            // element block → real iterator protocol.
            False, _ -> spread_via_iterator(state, iterable, target_ref)
            True, None -> spread_array_generic(state, src_ref, target_ref, 0)
            True, Some(length) ->
              case
                object.has_index_overrides(properties)
                || !is_dense(elements, length)
              {
                False -> {
                  // (a) intrinsic @@iterator + next, (b) observable length ==
                  // stored length, (c) no dict override for any index, (d)
                  // dense over [0, length): the raw element block IS what the
                  // spec iterator's per-index Get would return. Copy it in one
                  // heap write, no iterator object.
                  let heap =
                    append_range_to_array(
                      state.heap,
                      target_ref,
                      elements,
                      0,
                      length,
                    )
                  Ok(State(..state, heap:))
                }
                // @@iterator is still %Array.prototype.values%, but the raw
                // copy is unsafe: a defineProperty'd index (accessor or
                // attribute-modified data property lives in the dict, the
                // element slot reads as a hole) or a real hole (Get on a
                // hole consults the prototype chain — `Array.prototype[1]=x;
                // [...[0,,2]]` yields x, not undefined). Per-index Get is
                // exactly what the intrinsic iterator observably does.
                True -> spread_array_generic(state, src_ref, target_ref, 0)
              }
          }
        Some(ObjectSlot(kind: GeneratorObject(_), ..)) ->
          // Generators are self-iterators (@@iterator inherited from
          // %Iterator.prototype%). The raw drain re-enters the VM directly
          // instead of [[Call]]ing `.next`, so both must still be intrinsic.
          case
            intrinsic_iterator_guard(
              state.heap,
              src_ref,
              iterator_self,
              src_ref,
              CallFn(value.GeneratorNext),
            )
          {
            False -> spread_via_iterator(state, iterable, target_ref)
            True ->
              drain_generator_to_array(
                state,
                src_ref,
                target_ref,
                execute_inner,
              )
          }
        Some(ObjectSlot(
          kind: value.ArrayIteratorObject(source:, cursor:, iter_kind:),
          ..,
        )) -> {
          // Guard (both reads are pure heap walks, no getter runs):
          //   1. §7.4.3 GetIterator(it) first calls it[@@iterator]
          //      (inherited from %Iterator.prototype% — the `return this`
          //      self-iterator), so `it[Symbol.iterator] = ...` must be
          //      honored and must fail the guard.
          //   2. The raw drain below never calls `.next`, so it is only
          //      valid while `next` still resolves to the intrinsic
          //      %ArrayIteratorPrototype%.next (§23.1.5.2.1). An own
          //      `it.next = ...` or a patched %ArrayIteratorPrototype%.next
          //      must be honored per call.
          case
            intrinsic_iterator_guard(
              state.heap,
              src_ref,
              iterator_self,
              src_ref,
              array_iterator_next,
            )
          {
            False -> spread_via_iterator(state, iterable, target_ref)
            True ->
              spread_array_iterator(
                state,
                src_ref,
                target_ref,
                source,
                cursor,
                iter_kind,
              )
          }
        }
        Some(ObjectSlot(kind: value.SetObject(store:), ..)) ->
          // Set fast path — push values in insertion order. Guarded on
          // %Set.prototype.values% (the Set's @@iterator) and
          // %SetIteratorPrototype%.next.
          case
            intrinsic_iterator_guard(
              state.heap,
              src_ref,
              set_values,
              state.builtins.set_iterator_proto,
              set_iterator_next,
            )
          {
            False -> spread_via_iterator(state, iterable, target_ref)
            True -> {
              let values = ordered_entries.live_values(store)
              let heap = append_list_to_array(state.heap, target_ref, values)
              Ok(State(..state, heap:))
            }
          }
        Some(ObjectSlot(
          kind: value.SetIteratorObject(source:, cursor:, done:, kind:),
          ..,
        )) ->
          case
            intrinsic_iterator_guard(
              state.heap,
              src_ref,
              iterator_self,
              src_ref,
              set_iterator_next,
            )
          {
            False -> spread_via_iterator(state, iterable, target_ref)
            True -> {
              // Drain the LIVE iterator: forward insertion order of the source
              // from the cursor onward, then latch the iterator done.
              let proto = state.builtins.array.prototype
              use acc, e <- spread_collection_iterator(
                state,
                src_ref,
                target_ref,
                source,
                cursor,
                done,
                value.SetIteratorObject(source:, cursor:, done: True, kind:),
              )
              let #(h, els, len) = acc
              case kind {
                value.SetIterValues -> #(
                  h,
                  elements.set(els, len, e.1),
                  len + 1,
                )
                value.SetIterEntries -> {
                  let #(h, pair) = common.alloc_array(h, [e.1, e.1], proto)
                  #(h, elements.set(els, len, JsObject(pair)), len + 1)
                }
              }
            }
          }
        Some(ObjectSlot(kind: value.MapObject(store:), ..)) ->
          case
            intrinsic_iterator_guard(
              state.heap,
              src_ref,
              map_entries,
              state.builtins.map_iterator_proto,
              map_iterator_next,
            )
          {
            False -> spread_via_iterator(state, iterable, target_ref)
            True -> {
              // Map fast path — push [k,v] pairs in insertion order.
              let proto = state.builtins.array.prototype
              let heap = {
                use h, els, len <- batch_append(state.heap, target_ref)
                ordered_entries.live_entries(store)
                |> list.fold(#(h, els, len), fn(acc, e) {
                  let #(h, els, len) = acc
                  let #(h, pair) =
                    common.alloc_array(
                      h,
                      [value.map_key_to_js(e.0), e.1],
                      proto,
                    )
                  #(h, elements.set(els, len, JsObject(pair)), len + 1)
                })
              }
              Ok(State(..state, heap:))
            }
          }
        Some(ObjectSlot(
          kind: value.MapIteratorObject(source:, cursor:, done:, kind:),
          ..,
        )) ->
          case
            intrinsic_iterator_guard(
              state.heap,
              src_ref,
              iterator_self,
              src_ref,
              map_iterator_next,
            )
          {
            False -> spread_via_iterator(state, iterable, target_ref)
            True -> {
              let proto = state.builtins.array.prototype
              use acc, e <- spread_collection_iterator(
                state,
                src_ref,
                target_ref,
                source,
                cursor,
                done,
                value.MapIteratorObject(source:, cursor:, done: True, kind:),
              )
              let #(h, els, len) = acc
              let #(k, v) = #(value.map_key_to_js(e.0), e.1)
              case kind {
                value.MapIterKeys -> #(h, elements.set(els, len, k), len + 1)
                value.MapIterValues -> #(h, elements.set(els, len, v), len + 1)
                value.MapIterEntries -> {
                  let #(h, arr) = common.alloc_array(h, [k, v], proto)
                  #(h, elements.set(els, len, JsObject(arr)), len + 1)
                }
              }
            }
          }
        // Every other object — plain objects, class instances with a
        // generator @@iterator, Proxies, RegExp string iterators, userland
        // `{ [Symbol.iterator]() {...} }` objects — takes the real
        // GetIterator + IteratorStepValue path. Only an object whose
        // @@iterator is missing/not callable TypeErrors ("is not iterable").
        _ -> spread_via_iterator(state, iterable, target_ref)
      }
    // String primitive: spread iterates code points (ES §22.1.5), matching
    // the GetIterator string fast path used by for-of. `s[@@iterator]`
    // resolves through %String.prototype%, and Arc's string iterator inherits
    // %ArrayIteratorPrototype% — guard both, since
    // `String.prototype[Symbol.iterator] = function*(){yield 9}` is observable.
    value.JsString(s) ->
      case
        intrinsic_iterator_guard(
          state.heap,
          state.builtins.string.prototype,
          string_symbol_iterator,
          state.builtins.array_iterator_proto,
          array_iterator_next,
        )
      {
        False -> spread_via_iterator(state, iterable, target_ref)
        True -> {
          let values =
            string.to_utf_codepoints(s)
            |> list.map(fn(cp) {
              value.JsString(string.from_utf_codepoints([cp]))
            })
          let heap = append_list_to_array(state.heap, target_ref, values)
          Ok(State(..state, heap:))
        }
      }
    // null/undefined/other primitives: not iterable.
    _ -> {
      state.throw_type_error(
        state,
        object.inspect(iterable, state.heap) <> " is not iterable",
      )
    }
  }
}

/// Slow path for spreading an array/arguments object whose @@iterator is
/// still the intrinsic %Array.prototype.values% but whose raw element block
/// is NOT what that iterator would observe: an indexed property was moved
/// into the dict by defineProperty (accessor or attribute-modified data
/// property — the element slot reads as a hole), or the array has real
/// holes (Get on a hole consults the prototype chain). Mirrors the spec
/// array iterator (§23.1.5.1): re-read length each step — a getter can grow
/// or shrink the array mid-iteration — then Get(arr, idx), append, repeat.
fn spread_array_generic(
  state: State(host),
  src_ref: Ref,
  target_ref: Ref,
  idx: Int,
) -> Result(State(host), StepExit(host)) {
  // §7.1.20 LengthOfArrayLike: ? ToLength(? Get(O, "length")). Re-read every
  // step — the intrinsic array iterator does, and a getter can grow or shrink
  // the source mid-iteration. Going through [[Get]] (rather than the stored
  // ArrayObject/ArgumentsObject count) is what makes `arguments.length = 0;
  // [...arguments]` spread nothing.
  use #(length, state) <- result.try(array_like_length(state, src_ref))
  case idx >= length {
    True -> Ok(state)
    False ->
      case object.get_value_of(state, JsObject(src_ref), Index(idx)) {
        Ok(#(v, state)) -> {
          let heap = push_onto_array(state.heap, target_ref, v)
          spread_array_generic(
            State(..state, heap:),
            src_ref,
            target_ref,
            idx + 1,
          )
        }
        Error(#(thrown, state)) -> Error(Threw(thrown, state))
      }
  }
}

/// §7.1.20 LengthOfArrayLike(O), in this module's StepExit error shape — the
/// implementation itself is `property.length_of_array_like`.
fn array_like_length(
  state: State(host),
  ref: Ref,
) -> Result(#(Int, State(host)), StepExit(host)) {
  state.rethrow(property.length_of_array_like(state, ref, JsObject(ref)))
}

/// Raw drain of an ArrayIteratorObject whose @@iterator and `next` are still
/// the intrinsics (the caller has already proved this — see
/// `is_intrinsic_iterator_proto_iterator` / `is_intrinsic_array_iterator_next`).
/// Copies the remaining elements from the iterator's current position in bulk
/// and latches the iterator done, with no per-element .next() call or
/// {value, done} result allocation. A second protector on the SOURCE array
/// (`raw_iterator_source_copy_is_safe`) deopts holes / index overrides to the
/// real per-.next() drain.
fn spread_array_iterator(
  state: State(host),
  src_ref: Ref,
  target_ref: Ref,
  source: Ref,
  cursor: option.Option(Int),
  iter_kind: value.ArrayIterKind,
) -> Result(State(host), StepExit(host)) {
  case heap.read(state.heap, source) {
    // Typed-array source — §23.1.5.1: validate the buffer witness,
    // then drain the remaining elements from the live backing store.
    Some(ObjectSlot(
      kind: value.TypedArrayObject(buffer:, elem_kind:, byte_offset:, length:),
      ..,
    )) ->
      case
        object.typed_array_iter_length(
          state.heap,
          buffer,
          elem_kind,
          byte_offset,
          length,
        )
      {
        Error(err) -> object.throw_view_witness_error(state, err)
        Ok(len) -> {
          // `cursor: None` is the exhaustion latch — nothing to drain.
          let values = case cursor {
            None -> []
            Some(index) ->
              typed_array_values_range(
                state.heap,
                buffer,
                elem_kind,
                byte_offset,
                len,
                index,
                len,
                [],
              )
          }
          let #(heap, values) =
            shape_iter_values(
              state.heap,
              state.builtins.array.prototype,
              iter_kind,
              option.unwrap(cursor, 0),
              values,
            )
          let heap = append_list_to_array(heap, target_ref, values)
          let heap = array_iterator.exhaust_heap(heap, src_ref)
          Ok(State(..state, heap:))
        }
      }
    _ ->
      // Second protector, on the SOURCE array: the intrinsic next does
      // Get(source, i) per index (§23.1.5.1), which runs accessor getters
      // installed with defineProperty and consults the prototype chain for
      // holes. The raw block copy below can't see either, so it is only
      // equivalent when the source is a dense Array/Arguments with no dict
      // index override. Anything else (including a non-array source, e.g.
      // `Array.prototype.values.call(arrayLike)`) deopts to the real
      // per-.next() drain, which the caller already proved intrinsic.
      case raw_iterator_source_copy_is_safe(state.heap, source) {
        None -> spread_via_iterator(state, JsObject(src_ref), target_ref)
        // Drain remaining elements from the iterator's current position.
        Some(#(length, elems)) -> {
          let from = option.unwrap(cursor, 0)
          let heap = case iter_kind, cursor {
            _, None -> state.heap
            value.ArrayIterValues, Some(_) ->
              append_range_to_array(state.heap, target_ref, elems, from, length)
            _, Some(_) -> {
              let values = case from >= length {
                True -> []
                False ->
                  int.range(from:, to: length, with: [], run: fn(acc, i) {
                    [
                      elements.get_option(elems, i)
                        |> option.unwrap(JsUndefined),
                      ..acc
                    ]
                  })
                  |> list.reverse
              }
              let #(heap, values) =
                shape_iter_values(
                  state.heap,
                  state.builtins.array.prototype,
                  iter_kind,
                  from,
                  values,
                )
              append_list_to_array(heap, target_ref, values)
            }
          }
          let heap = array_iterator.exhaust_heap(heap, src_ref)
          Ok(State(..state, heap:))
        }
      }
  }
}

/// Protector for `spread_array_iterator`'s raw block copy of the SOURCE
/// array's element storage: Some(#(length, elements)) iff the source is an
/// Array/Arguments that is dense over [0, length) and has no dict index
/// override, i.e. the raw element block IS what the intrinsic next's
/// per-index Get would return (the block is returned so the caller does not
/// re-read the slot). Pure heap walk, never observable — a miss only deopts.
fn raw_iterator_source_copy_is_safe(
  h: Heap(host),
  source: Ref,
) -> option.Option(#(Int, value.JsElements)) {
  case heap.read(h, source) {
    Some(ObjectSlot(kind: ArrayObject(length:), elements:, properties:, ..))
    | Some(ObjectSlot(
        kind: value.ArgumentsObject(length:),
        elements:,
        properties:,
        ..,
      )) ->
      case
        !object.has_index_overrides(properties) && is_dense(elements, length)
      {
        True -> Some(#(length, elements))
        False -> None
      }
    _ -> None
  }
}

// ============================================================================
// Fully generic ArraySpread — §13.2.4.1 SpreadElement steps 2-4
// ============================================================================

/// The real iterator protocol for `[...x]`: §7.4.3 GetIterator(x, sync) —
/// GetMethod(x, @@iterator) may run a getter, and throws a TypeError iff
/// the method is missing or not callable — then drain the resulting
/// iterator with §7.4.8 IteratorStepValue, appending each value to the
/// target array. Reuses the SAME `get_iterator_sync`/`iterator_step_value`
/// machinery as the interpreter's for-of and the Iterator.prototype
/// helpers, so spread and for-of can never diverge on protocol details.
///
/// Every object that no guarded fast path in `spread_into_array` claims
/// lands here, as does every object whose fast-path guard fails.
fn spread_via_iterator(
  state: State(host),
  iterable: JsValue,
  target_ref: Ref,
) -> Result(State(host), StepExit(host)) {
  use #(rec, state) <- result.try(
    state.rethrow(iter_protocol.get_iterator_sync(state, iterable)),
  )
  spread_iterator_loop(state, rec, target_ref)
}

/// One IteratorStepValue per iteration until done. An abrupt completion from
/// next()/Get(done)/Get(value) propagates without IteratorClose (§7.4.8
/// already marked the iterator done), and appending to the target array
/// (CreateDataPropertyOrThrow on a fresh Array) can never throw, so no
/// close-on-abrupt handling is needed here.
fn spread_iterator_loop(
  state: State(host),
  rec: value.IteratorRecord,
  target_ref: Ref,
) -> Result(State(host), StepExit(host)) {
  let #(state, step) = iter_protocol.iterator_step_value(state, rec)
  case step {
    Error(thrown) -> Error(Threw(thrown, state))
    Ok(None) -> Ok(state)
    Ok(Some(v)) -> {
      let heap = push_onto_array(state.heap, target_ref, v)
      spread_iterator_loop(State(..state, heap:), rec, target_ref)
    }
  }
}

// ============================================================================
// V8-"protector"-style guards for the ArraySpread fast paths
//
// Each guard is a PURE heap walk (no getter runs, nothing user-observable),
// so a guard can never change program behaviour — a miss only deoptimises
// to `spread_via_iterator`. A hit proves the raw copy/drain above it is
// observationally identical to the spec's GetIterator + IteratorStepValue.
// ============================================================================

/// Names one intrinsic function object by its dispatch tag. Arc has a single
/// realm, so a tag identifies exactly one heap object — tag equality IS
/// intrinsic identity, and no separate ref needs to be retained in `Builtins`.
/// The two constructors mirror `value.NativeFnSlot`'s two builtin variants.
type IntrinsicFn {
  DispatchFn(value.NativeFn)
  CallFn(value.CallNativeFn)
}

/// True iff `prop` is a plain data property whose value is THE intrinsic
/// function object named by `tag`.
///
/// `None` (the property is absent on the whole chain) and accessor properties
/// both fail: absent means "not iterable" and an accessor's getter is
/// observable, so both must go through `spread_via_iterator`.
fn is_intrinsic_data_property(
  h: Heap(host),
  prop: option.Option(value.Property),
  tag: IntrinsicFn,
) -> Bool {
  case prop {
    Some(value.DataProperty(value: JsObject(fn_ref), ..)) ->
      case heap.read(h, fn_ref), tag {
        Some(ObjectSlot(kind: value.NativeFunction(value.Dispatch(fnv), ..), ..)),
          DispatchFn(want)
        -> fnv == want
        Some(ObjectSlot(kind: value.NativeFunction(value.Call(fnv), ..), ..)),
          CallFn(want)
        -> fnv == want
        _, _ -> False
      }
    _ -> False
  }
}

/// THE protector for every raw-storage spread fast path. Two pure heap walks
/// (no getter runs, nothing user-observable, so a MISS can only deoptimise):
///
///   1. `src_ref[@@iterator]` is still the intrinsic `expected_iter` data
///      property — no own override, no patched `Set.prototype[Symbol.iterator]`
///      / `Array.prototype[Symbol.iterator]`, no swapped [[Prototype]]. §7.4.3
///      GetIterator calls it, so a patched one MUST be honoured.
///   2. `next_ref.next` is still the intrinsic `expected_next`. The raw drain
///      never calls `.next`, so it is only equivalent while `next` resolves to
///      the intrinsic — for a collection, `next_ref` is the iterator prototype
///      the fresh iterator would inherit from; for an iterator object it is the
///      iterator itself (an own `it.next = ...` must be honoured too).
///
/// A hit means the raw copy is observationally identical to the spec's
/// GetIterator + IteratorStepValue loop; a miss deopts to
/// `spread_via_iterator`, the real protocol.
fn intrinsic_iterator_guard(
  h: Heap(host),
  src_ref: Ref,
  expected_iter: IntrinsicFn,
  next_ref: Ref,
  expected_next: IntrinsicFn,
) -> Bool {
  is_intrinsic_data_property(
    h,
    find_symbol_property(h, src_ref, value.symbol_iterator),
    expected_iter,
  )
  && is_intrinsic_data_property(
    h,
    object.find_property(h, next_ref, Named("next"))
      // A proxy on the chain is by definition not the intrinsic descriptor →
      // decline the fast path and let the real protocol fire its traps.
      |> object.or_when_proxy(None),
    expected_next,
  )
}

/// The intrinsic `%Array.prototype.values%` (§23.1.3.35) — the @@iterator of
/// Arrays and `arguments` objects.
const array_values = DispatchFn(value.ArrayNative(value.ArrayPrototypeValues))

/// The intrinsic `%ArrayIteratorPrototype%.next` (§23.1.5.2.1). Arc's string
/// iterators share %ArrayIteratorPrototype%, so this covers them too.
const array_iterator_next = CallFn(value.ArrayIteratorNext)

/// The intrinsic `%Iterator.prototype%[@@iterator]` (§27.1.4.1) — the
/// `return this` self-iterator every built-in iterator (and every generator)
/// inherits.
const iterator_self = DispatchFn(value.VmNative(value.IteratorSymbolIterator))

/// The intrinsic `%Set.prototype.values%` (§24.2.4.11) — a Set's @@iterator.
const set_values = DispatchFn(value.SetNative(value.SetPrototypeValues))

/// The intrinsic `%SetIteratorPrototype%.next` (§24.2.5.2.1).
const set_iterator_next = CallFn(value.SetIteratorNext)

/// The intrinsic `%Map.prototype.entries%` (§24.1.3.4) — a Map's @@iterator.
const map_entries = DispatchFn(value.MapNative(value.MapPrototypeEntries))

/// The intrinsic `%MapIteratorPrototype%.next` (§24.1.5.2.1).
const map_iterator_next = CallFn(value.MapIteratorNext)

/// The intrinsic `%String.prototype%[@@iterator]` (§22.1.3.36).
const string_symbol_iterator = DispatchFn(
  value.StringNative(value.StringPrototypeSymbolIterator),
)

/// Resolve a symbol-keyed property along the prototype chain WITHOUT running
/// any getter — the symbol analogue of `object.find_property`. Used only as
/// a protector read, never as an observable [[Get]].
fn find_symbol_property(
  h: Heap(host),
  ref: Ref,
  sym: value.SymbolId,
) -> option.Option(value.Property) {
  case heap.read(h, ref) {
    Some(ObjectSlot(symbol_properties:, prototype:, ..)) ->
      case list.key_find(symbol_properties, sym) {
        Ok(prop) -> Some(prop)
        Error(Nil) -> option.then(prototype, find_symbol_property(h, _, sym))
      }
    _ -> None
  }
}

/// The `length` the spec's array iterator would OBSERVE, when the raw element
/// block can stand in for it — `None` means "deopt to per-index [[Get]]".
///
/// An Array's `length` is virtual: it always tracks `ArrayObject(length)`, so
/// the stored value is the observable one. An `arguments` object's is a real
/// own data property seeded with the initial arg count and freely assignable
/// (`arguments.length = 0`), so the kind's count is only trustworthy while
/// that property still agrees with it. Anything else — a deleted or
/// redefined-to-accessor `length` — declines the fast path.
fn spread_length_of(
  kind: value.ExoticKind(State(host), host),
  properties: dict.Dict(key.PropertyKey, value.Property),
) -> option.Option(Int) {
  case kind {
    ArrayObject(length:) -> Some(length)
    value.ArgumentsObject(length:) ->
      case dict.get(properties, Named("length")) {
        Ok(value.DataProperty(value: v, ..)) ->
          case v == value.from_int(length) {
            True -> Some(length)
            False -> None
          }
        _ -> None
      }
    _ -> None
  }
}

/// True when every index in [0, length) has a present element — no holes.
/// A hole is NOT equivalent to `undefined` under the iterator protocol:
/// %Array.prototype.values%.next does Get(arr, i), which walks the prototype
/// chain for a missing own index (`Array.prototype[1] = x; [...[0,,2]]`
/// yields x, not undefined). The raw-element copy can't see that, so any
/// hole forces the per-index-Get path.
/// Only [0, length) counts: an `arguments` object stores out-of-range index
/// writes in `elements` without bumping its `length`, so a total count would
/// call a holey range dense (see `elements.count_present_below`).
/// Runs on every guarded array spread, so it must stay allocation-free.
fn is_dense(els: value.JsElements, length: Int) -> Bool {
  elements.is_dense_below(els, length)
}

/// Repeatedly resume the generator, pushing each yielded value onto the
/// target array until done=true. Each resume re-enters the VM via
/// resume_generator_next, so state must be threaded through. The internal
/// resume API yields #(done, value, state) directly — no per-element
/// {value, done} result object is allocated.
pub fn drain_generator_to_array(
  state: State(host),
  gen_ref: Ref,
  target_ref: Ref,
  execute_inner: generators.ExecuteInnerFn(host),
) -> Result(State(host), StepExit(host)) {
  use #(done, val, next_state) <- result.try(generators.resume_generator_next(
    state,
    JsObject(gen_ref),
    JsUndefined,
    execute_inner,
  ))
  // resume_generator_next advances pc past the current op; the ArraySpread
  // handler does that itself, so restore the caller's pc.
  let next_state = State(..next_state, pc: state.pc)
  case done {
    True -> Ok(next_state)
    False -> {
      let heap = push_onto_array(next_state.heap, target_ref, val)
      drain_generator_to_array(
        State(..next_state, heap:),
        gen_ref,
        target_ref,
        execute_inner,
      )
    }
  }
}

import gleam/result
