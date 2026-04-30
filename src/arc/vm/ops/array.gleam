import arc/vm/builtins/common
import arc/vm/exec/generators
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/ops/object
import arc/vm/state.{
  type Heap, type State, type StepResult, type VmError, State, StepVmError,
  Unimplemented,
}
import arc/vm/value.{
  type JsValue, type Ref, ArrayObject, DataProperty, GeneratorObject, JsBool,
  JsObject, JsUndefined, ObjectSlot,
}
import gleam/dict
import gleam/list
import gleam/option.{Some}

// ============================================================================
// Callback types for VM functions that can't be imported directly
// ============================================================================

pub type ExecuteInnerFn =
  fn(State) -> Result(#(completion.Completion, State), VmError)

pub type UnwindToCatchFn =
  fn(State, JsValue) -> option.Option(State)

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
  case idx >= end {
    True -> h
    False -> {
      // elements.get returns JsUndefined for holes — matches the spec's
      // array iterator behavior (CreateIterResultObject(Get(array, idx), false)).
      let h = push_onto_array(h, target_ref, elements.get(src_elements, idx))
      append_range_to_array(h, target_ref, src_elements, idx + 1, end)
    }
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
  unwind_to_catch: UnwindToCatchFn,
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
          drain_generator_to_array(
            state,
            src_ref,
            target_ref,
            execute_inner,
            unwind_to_catch,
          )
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
        Some(ObjectSlot(kind: value.SetObject(data:, keys:), ..)) -> {
          // Set fast path — push values in insertion order.
          let heap =
            list.reverse(keys)
            |> list.filter_map(dict.get(data, _))
            |> list.fold(state.heap, fn(h, v) {
              push_onto_array(h, target_ref, v)
            })
          Ok(State(..state, heap:))
        }
        Some(ObjectSlot(kind: value.SetIteratorObject(remaining:, kind:), ..)) -> {
          let heap = case kind {
            value.SetIterValues ->
              list.fold(remaining, state.heap, fn(h, v) {
                push_onto_array(h, target_ref, v)
              })
            value.SetIterEntries ->
              list.fold(remaining, state.heap, fn(h, v) {
                let #(h, pair) =
                  common.alloc_array(h, [v, v], state.builtins.array.prototype)
                push_onto_array(h, target_ref, JsObject(pair))
              })
          }
          Ok(State(..state, heap:))
        }
        Some(ObjectSlot(kind: value.MapObject(entries:, keys_rev:, ..), ..)) -> {
          // Map fast path — push [k,v] pairs in insertion order. keys_rev is
          // reversed with tombstones; flip + filter live entries.
          let heap =
            list.reverse(keys_rev)
            |> list.fold(state.heap, fn(h, k) {
              case dict.get(entries, k) {
                Error(Nil) -> h
                Ok(v) -> {
                  let #(h, pair) =
                    common.alloc_array(
                      h,
                      [value.map_key_to_js(k), v],
                      state.builtins.array.prototype,
                    )
                  push_onto_array(h, target_ref, JsObject(pair))
                }
              }
            })
          Ok(State(..state, heap:))
        }
        Some(ObjectSlot(kind: value.MapIteratorObject(remaining:, kind:), ..)) -> {
          let heap =
            list.fold(remaining, state.heap, fn(h, pair) {
              let #(k, v) = pair
              case kind {
                value.MapIterKeys -> push_onto_array(h, target_ref, k)
                value.MapIterValues -> push_onto_array(h, target_ref, v)
                value.MapIterEntries -> {
                  let #(h, arr) =
                    common.alloc_array(
                      h,
                      [k, v],
                      state.builtins.array.prototype,
                    )
                  push_onto_array(h, target_ref, JsObject(arr))
                }
              }
            })
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

/// Repeatedly call generator.next(), pushing each yielded value onto the
/// target array until done=true. Each .next() re-enters the VM via
/// call_native_generator_next, so state must be threaded through.
/// The generator's {value, done} result object is read from the returned
/// state's stack — call_native_generator_next pushes it there.
pub fn drain_generator_to_array(
  state: State,
  gen_ref: Ref,
  target_ref: Ref,
  execute_inner: ExecuteInnerFn,
  unwind_to_catch: UnwindToCatchFn,
) -> Result(State, #(StepResult, JsValue, State)) {
  // call_native_generator_next pushes the result object onto rest_stack.
  // We pass an empty rest_stack so the result is the only thing on the stack.
  use next_state <- result.try(generators.call_native_generator_next(
    state,
    JsObject(gen_ref),
    [],
    [],
    execute_inner,
    unwind_to_catch,
  ))
  case next_state.stack {
    [JsObject(result_ref), ..] ->
      case heap.read(next_state.heap, result_ref) {
        Some(ObjectSlot(properties: props, ..)) -> {
          let done = case dict.get(props, value.Named("done")) {
            Ok(DataProperty(value: JsBool(d), ..)) -> d
            _ -> False
          }
          case done {
            True ->
              // Generator exhausted. Restore heap but not stack — the caller
              // (ArraySpread handler) sets the stack explicitly.
              Ok(
                State(
                  ..state.merge_globals(state, next_state, []),
                  heap: next_state.heap,
                ),
              )
            False -> {
              let val = case dict.get(props, value.Named("value")) {
                Ok(DataProperty(value: v, ..)) -> v
                _ -> JsUndefined
              }
              let heap = push_onto_array(next_state.heap, target_ref, val)
              // Recurse with the post-next state but cleaned stack.
              drain_generator_to_array(
                State(..state.merge_globals(state, next_state, []), heap:),
                gen_ref,
                target_ref,
                execute_inner,
                unwind_to_catch,
              )
            }
          }
        }
        _ ->
          Error(#(
            StepVmError(Unimplemented(
              "ArraySpread: generator .next() returned non-object",
            )),
            JsUndefined,
            next_state,
          ))
      }
    _ ->
      Error(#(
        StepVmError(Unimplemented("ArraySpread: generator .next() empty stack")),
        JsUndefined,
        next_state,
      ))
  }
}

import gleam/result
