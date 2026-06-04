import arc/vm/builtins/common
import arc/vm/exec/generators
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/ops/object
import arc/vm/state.{
  type Heap, type State, type StepResult, type VmError, State,
}
import arc/vm/value.{
  type JsValue, type Ref, ArrayObject, GeneratorObject, JsObject, JsUndefined,
  ObjectSlot,
}
import gleam/dict
import gleam/list
import gleam/option.{Some}

// ============================================================================
// Callback types for VM functions that can't be imported directly
// ============================================================================

pub type ExecuteInnerFn =
  fn(State) -> Result(#(completion.Completion, State), VmError)

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
pub fn grow_array_length(h: Heap, ref: Ref) -> Heap {
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
pub fn push_onto_array(h: Heap, ref: Ref, val: JsValue) -> Heap {
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
  h: Heap,
  target_ref: Ref,
  fold: fn(Heap, value.JsElements, Int) -> #(Heap, value.JsElements, Int),
) -> Heap {
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
  h: Heap,
  target_ref: Ref,
  values: List(JsValue),
) -> Heap {
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
  h: Heap,
  target_ref: Ref,
  src_elements: value.JsElements,
  idx: Int,
  end: Int,
) -> Heap {
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
  state: State,
  target_ref: Ref,
  iterable: JsValue,
  execute_inner: ExecuteInnerFn,
) -> Result(State, #(StepResult, JsValue, State)) {
  case iterable {
    JsObject(src_ref) ->
      case heap.read(state.heap, src_ref) {
        Some(ObjectSlot(kind: ArrayObject(length:), elements:, ..))
        | Some(ObjectSlot(kind: value.ArgumentsObject(length:), elements:, ..)) -> {
          // Fast path: copy all elements at once, no iterator slot.
          let heap =
            append_range_to_array(state.heap, target_ref, elements, 0, length)
          Ok(State(..state, heap:))
        }
        Some(ObjectSlot(kind: GeneratorObject(_), ..)) ->
          // Generators are self-iterators. Drain via repeated .next().
          drain_generator_to_array(state, src_ref, target_ref, execute_inner)
        Some(ObjectSlot(kind: value.ArrayIteratorObject(source:, index:), ..)) -> {
          // Drain remaining elements from the iterator's current position.
          let #(length, elements) =
            heap.read_array_like(state.heap, source)
            |> option.unwrap(#(0, elements.new()))
          let heap =
            append_range_to_array(
              state.heap,
              target_ref,
              elements,
              index,
              length,
            )
          Ok(State(..state, heap:))
        }
        Some(ObjectSlot(kind: value.SetObject(data:, keys_rev:, keys_len:), ..)) -> {
          // Set fast path — push values in insertion order.
          let values = value.set_live_values(data, keys_rev, keys_len)
          let heap = append_list_to_array(state.heap, target_ref, values)
          Ok(State(..state, heap:))
        }
        Some(ObjectSlot(kind: value.SetIteratorObject(remaining:, kind:), ..)) -> {
          let proto = state.builtins.array.prototype
          let heap = {
            use h, els, len <- batch_append(state.heap, target_ref)
            list.fold(remaining, #(h, els, len), fn(acc, v) {
              let #(h, els, len) = acc
              case kind {
                value.SetIterValues -> #(h, elements.set(els, len, v), len + 1)
                value.SetIterEntries -> {
                  let #(h, pair) = common.alloc_array(h, [v, v], proto)
                  #(h, elements.set(els, len, JsObject(pair)), len + 1)
                }
              }
            })
          }
          Ok(State(..state, heap:))
        }
        Some(ObjectSlot(kind: value.MapObject(entries:, keys_rev:, ..), ..)) -> {
          // Map fast path — push [k,v] pairs in insertion order. keys_rev is
          // reversed with tombstones; flip + filter live entries.
          let proto = state.builtins.array.prototype
          let heap = {
            use h, els, len <- batch_append(state.heap, target_ref)
            list.reverse(keys_rev)
            |> list.fold(#(h, els, len), fn(acc, k) {
              case dict.get(entries, k) {
                Error(Nil) -> acc
                Ok(v) -> {
                  let #(h, els, len) = acc
                  let #(h, pair) =
                    common.alloc_array(h, [value.map_key_to_js(k), v], proto)
                  #(h, elements.set(els, len, JsObject(pair)), len + 1)
                }
              }
            })
          }
          Ok(State(..state, heap:))
        }
        Some(ObjectSlot(kind: value.MapIteratorObject(remaining:, kind:), ..)) -> {
          let proto = state.builtins.array.prototype
          let heap = {
            use h, els, len <- batch_append(state.heap, target_ref)
            list.fold(remaining, #(h, els, len), fn(acc, pair) {
              let #(h, els, len) = acc
              let #(k, v) = pair
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
          Ok(State(..state, heap:))
        }
        _ -> {
          state.throw_type_error(
            state,
            object.inspect(iterable, state.heap) <> " is not iterable",
          )
        }
      }
    // null/undefined/primitives: not iterable.
    // (Strings are iterable per spec but GetIterator doesn't handle them yet;
    //  will be fixed when Symbol.iterator is wired for string wrappers.)
    _ -> {
      state.throw_type_error(
        state,
        object.inspect(iterable, state.heap) <> " is not iterable",
      )
    }
  }
}

/// Repeatedly resume the generator, pushing each yielded value onto the
/// target array until done=true. Each resume re-enters the VM via
/// resume_generator_next, so state must be threaded through. The internal
/// resume API yields #(done, value, state) directly — no per-element
/// {value, done} result object is allocated.
pub fn drain_generator_to_array(
  state: State,
  gen_ref: Ref,
  target_ref: Ref,
  execute_inner: ExecuteInnerFn,
) -> Result(State, #(StepResult, JsValue, State)) {
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
