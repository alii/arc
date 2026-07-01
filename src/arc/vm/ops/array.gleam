import arc/vm/builtins/common
import arc/vm/exec/generators
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/ops/object
import arc/vm/state.{
  type Heap, type State, type StepResult, type VmError, State, Thrown,
}
import arc/vm/value.{
  type JsValue, type Ref, ArrayObject, GeneratorObject, JsObject, JsUndefined,
  ObjectSlot,
}
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/string

// ============================================================================
// Callback types for VM functions that can't be imported directly
// ============================================================================

pub type ExecuteInnerFn(host) =
  fn(State(host)) -> Result(#(completion.Completion, State(host)), VmError)

import arc/vm/completion

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
  case slot {
    ObjectSlot(kind: ArrayObject(length:), ..) ->
      ObjectSlot(..slot, kind: ArrayObject(length + 1))
    _ -> slot
  }
}

/// Append one value to the end of an array (ArrayPush opcode helper).
/// Reads current length, sets element at that index, increments length.
/// Non-array refs are a no-op — shouldn't happen for compiler-emitted literals.
pub fn push_onto_array(h: Heap(host), ref: Ref, val: JsValue) -> Heap(host) {
  use slot <- heap.update(h, ref)
  case slot {
    ObjectSlot(kind: ArrayObject(length:), elements:, ..) ->
      ObjectSlot(
        ..slot,
        kind: ArrayObject(length + 1),
        elements: elements.set(elements, length, val),
      )
    _ -> slot
  }
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
  case heap.read(h, target_ref) {
    Some(ObjectSlot(kind: ArrayObject(length:), elements:, ..) as slot) -> {
      let #(h, elements, length) = fold(h, elements, length)
      heap.write(
        h,
        target_ref,
        ObjectSlot(..slot, kind: ArrayObject(length:), elements:),
      )
    }
    _ -> h
  }
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
fn shape_iter_values(
  h: Heap(host),
  array_proto: Ref,
  iter_kind: value.ArrayIterKind,
  start: Int,
  values: List(JsValue),
) -> #(Heap(host), List(JsValue)) {
  case iter_kind {
    value.ArrayIterValues -> #(h, values)
    value.ArrayIterKeys -> #(
      h,
      list.index_map(values, fn(_, i) { value.from_int(start + i) }),
    )
    value.ArrayIterEntries -> {
      let #(h, rev) =
        list.index_fold(values, #(h, []), fn(acc, el, i) {
          let #(h, lst) = acc
          let #(h, pair_ref) =
            common.alloc_array(h, [value.from_int(start + i), el], array_proto)
          #(h, [JsObject(pair_ref), ..lst])
        })
      #(h, list.reverse(rev))
    }
  }
}

/// Latch an Array Iterator as exhausted (index -1) after a full drain —
/// further .next() calls answer done, matching the spec's
/// [[IteratedObject]] = undefined "already returned" state.
fn latch_array_iter_done(h: Heap(host), iter_ref: Ref) -> Heap(host) {
  case heap.read(h, iter_ref) {
    Some(
      ObjectSlot(kind: value.ArrayIteratorObject(source:, iter_kind:, ..), ..) as slot,
    ) ->
      heap.write(
        h,
        iter_ref,
        ObjectSlot(
          ..slot,
          kind: value.ArrayIteratorObject(source:, index: -1, iter_kind:),
        ),
      )
    _ -> h
  }
}

/// Drain an iterable into the target array (ArraySpread opcode helper).
/// Mirrors GetIterator's dispatch: ArrayObject fast-path, GeneratorObject
/// drain loop, everything else throws "is not iterable".
///
/// Per ES §13.2.4.1 ArrayAccumulation (SpreadElement):
///   1. spreadObj = ? Evaluate(AssignmentExpression)
///   2. iteratorRecord = ? GetIterator(spreadObj, sync)
///   3. Repeat: next = ? IteratorStepValue; if done return; CreateDataProperty(A, idx, next); idx++
///
/// Array fast-path is observationally equivalent for us — the spec's array
/// iterator reads Get(array, idx) which returns undefined for holes; so does
/// elements.get. V8 does the same shortcut.
pub fn spread_into_array(
  state: State(host),
  target_ref: Ref,
  iterable: JsValue,
  execute_inner: ExecuteInnerFn(host),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  case iterable {
    JsObject(src_ref) ->
      case heap.read(state.heap, src_ref) {
        Some(ObjectSlot(kind: ArrayObject(length:), elements:, properties:, ..))
        | Some(ObjectSlot(
            kind: value.ArgumentsObject(length:),
            elements:,
            properties:,
            ..,
          )) ->
          case object.has_index_overrides(properties) {
            False -> {
              // Fast path: copy all elements at once, no iterator slot.
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
            // defineProperty moved an element into the dict (accessor or
            // attribute-modified data property) — raw elements would read
            // the hole as undefined. Per-index Get honors the override.
            True -> spread_array_generic(state, src_ref, target_ref, 0)
          }
        Some(ObjectSlot(kind: GeneratorObject(_), ..)) ->
          // Generators are self-iterators. Drain via repeated .next().
          drain_generator_to_array(state, src_ref, target_ref, execute_inner)
        Some(ObjectSlot(
          kind: value.ArrayIteratorObject(source:, index:, iter_kind:),
          ..,
        )) ->
          case heap.read(state.heap, source) {
            // Typed-array source — §23.1.5.1: validate the buffer witness,
            // then drain the remaining elements from the live backing store.
            Some(ObjectSlot(
              kind: value.TypedArrayObject(
                buffer:,
                elem_kind:,
                byte_offset:,
                length:,
              ),
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
                Error(msg) -> state.throw_type_error(state, msg)
                Ok(len) -> {
                  // index < 0 is the exhaustion latch — nothing to drain.
                  let values = case index < 0 {
                    True -> []
                    False ->
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
                      int.max(index, 0),
                      values,
                    )
                  let heap = append_list_to_array(heap, target_ref, values)
                  let heap = latch_array_iter_done(heap, src_ref)
                  Ok(State(..state, heap:))
                }
              }
            _ -> {
              // Drain remaining elements from the iterator's current position.
              let #(length, elems) =
                heap.read_array_like(state.heap, source)
                |> option.unwrap(#(0, elements.new()))
              let from = int.max(index, 0)
              let heap = case iter_kind, index < 0 {
                _, True -> state.heap
                value.ArrayIterValues, False ->
                  append_range_to_array(
                    state.heap,
                    target_ref,
                    elems,
                    from,
                    length,
                  )
                _, False -> {
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
              let heap = latch_array_iter_done(heap, src_ref)
              Ok(State(..state, heap:))
            }
          }
        Some(ObjectSlot(kind: value.SetObject(data:, order:, ..), ..)) -> {
          // Set fast path — push values in insertion order.
          let values = value.set_live_values(data, order)
          let heap = append_list_to_array(state.heap, target_ref, values)
          Ok(State(..state, heap:))
        }
        Some(
          ObjectSlot(
            kind: value.SetIteratorObject(source:, cursor:, done:, kind:),
            ..,
          ) as slot,
        ) -> {
          // Drain the LIVE iterator: forward insertion order of the source
          // from the cursor onward, then latch the iterator done.
          let entries = case done {
            True -> []
            False ->
              case heap.read(state.heap, source) {
                Some(ObjectSlot(kind: value.SetObject(data:, order:, ..), ..)) ->
                  value.live_entries_from(data, order, cursor)
                _ -> []
              }
          }
          let proto = state.builtins.array.prototype
          let heap = {
            use h, els, len <- batch_append(state.heap, target_ref)
            list.fold(entries, #(h, els, len), fn(acc, e) {
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
            })
          }
          let heap =
            heap.write(
              heap,
              src_ref,
              ObjectSlot(
                ..slot,
                kind: value.SetIteratorObject(
                  source:,
                  cursor:,
                  done: True,
                  kind:,
                ),
              ),
            )
          Ok(State(..state, heap:))
        }
        Some(ObjectSlot(kind: value.MapObject(entries:, order:, ..), ..)) -> {
          // Map fast path — push [k,v] pairs in insertion order.
          let proto = state.builtins.array.prototype
          let heap = {
            use h, els, len <- batch_append(state.heap, target_ref)
            value.live_entries(entries, order)
            |> list.fold(#(h, els, len), fn(acc, e) {
              let #(h, els, len) = acc
              let #(h, pair) =
                common.alloc_array(h, [value.map_key_to_js(e.0), e.1], proto)
              #(h, elements.set(els, len, JsObject(pair)), len + 1)
            })
          }
          Ok(State(..state, heap:))
        }
        Some(
          ObjectSlot(
            kind: value.MapIteratorObject(source:, cursor:, done:, kind:),
            ..,
          ) as slot,
        ) -> {
          let entries = case done {
            True -> []
            False ->
              case heap.read(state.heap, source) {
                Some(ObjectSlot(kind: value.MapObject(entries:, order:, ..), ..)) ->
                  value.live_entries_from(entries, order, cursor)
                _ -> []
              }
          }
          let proto = state.builtins.array.prototype
          let heap = {
            use h, els, len <- batch_append(state.heap, target_ref)
            list.fold(entries, #(h, els, len), fn(acc, e) {
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
            })
          }
          let heap =
            heap.write(
              heap,
              src_ref,
              ObjectSlot(
                ..slot,
                kind: value.MapIteratorObject(
                  source:,
                  cursor:,
                  done: True,
                  kind:,
                ),
              ),
            )
          Ok(State(..state, heap:))
        }
        _ -> {
          state.throw_type_error(
            state,
            object.inspect(iterable, state.heap) <> " is not iterable",
          )
        }
      }
    // String primitive: spread iterates code points (ES §22.1.5), matching
    // the GetIterator string fast path used by for-of.
    value.JsString(s) -> {
      let values =
        string.to_utf_codepoints(s)
        |> list.map(fn(cp) { value.JsString(string.from_utf_codepoints([cp])) })
      let heap = append_list_to_array(state.heap, target_ref, values)
      Ok(State(..state, heap:))
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

/// Slow path for spreading an array/arguments object whose indexed
/// properties have dict overrides (e.g. an accessor installed via
/// Object.defineProperty). Mirrors the spec array iterator (§23.1.5.1):
/// re-read length each step — a getter can grow or shrink the array
/// mid-iteration — then Get(arr, idx), append, repeat.
fn spread_array_generic(
  state: State(host),
  src_ref: Ref,
  target_ref: Ref,
  idx: Int,
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  let length =
    heap.read_array_like(state.heap, src_ref)
    |> option.map(fn(p) { p.0 })
    |> option.unwrap(0)
  case idx >= length {
    True -> Ok(state)
    False ->
      case object.get_value_of(state, JsObject(src_ref), value.Index(idx)) {
        Ok(#(v, state)) -> {
          let heap = push_onto_array(state.heap, target_ref, v)
          spread_array_generic(
            State(..state, heap:),
            src_ref,
            target_ref,
            idx + 1,
          )
        }
        Error(#(thrown, state)) -> Error(#(Thrown, thrown, state))
      }
  }
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
  execute_inner: ExecuteInnerFn(host),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
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
