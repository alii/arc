//// ES2024 §23.1.5.1 CreateArrayIterator's step algorithm — the ONE
//// implementation of "advance an Array Iterator by one".
////
//// Both consumers step through `step`:
////   - `%ArrayIteratorPrototype%.next()` called as a method (exec/call)
////   - the `IteratorNext` opcode's in-VM fast path (exec/interpreter)
////
//// Exhaustion is latched by setting the iterator's `cursor` to `None` (the
//// spec's [[IteratedObject]] = undefined "already returned" state): once the
//// iterator reported done it stays done, even if the source later grows or
//// its buffer is resized.

import arc/vm/builtins/common
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/key.{Index}
import arc/vm/limits
import arc/vm/ops/object
import arc/vm/ops/property
import arc/vm/state.{
  type State, type StepExit, InternalError, State, VmFailed, rethrow,
}
import arc/vm/value.{
  type ArrayIterKind, type JsValue, type Ref, JsObject, JsUndefined, ObjectSlot,
}
import gleam/dict
import gleam/option.{type Option, None, Some}
import gleam/result

/// The outcome of one `.next()` on an Array Iterator: either a value shaped
/// for the iterator's kind, or the iterator is exhausted (and now latched).
pub type ArrayIterStep {
  Yielded(value: JsValue)
  Exhausted
}

/// Advance `iter_ref` (an `ArrayIteratorObject`) by one step: read the source's
/// live length, read the element (which may run getters / fire proxy traps),
/// shape the result for the iteration kind and move the cursor.
///
/// Errors are step-level throws (a detached typed-array buffer, an accessor or
/// proxy trap that threw). The caller decides how those interact with iterator
/// closing.
pub fn step(
  state: State(host),
  iter_ref: Ref,
) -> Result(#(ArrayIterStep, State(host)), StepExit(host)) {
  case heap.read(state.heap, iter_ref) {
    Some(ObjectSlot(kind: value.ArrayIteratorObject(cursor: None, ..), ..)) ->
      Ok(#(Exhausted, state))
    Some(
      ObjectSlot(
        kind: value.ArrayIteratorObject(
          source:,
          cursor: Some(index),
          iter_kind:,
        ),
        ..,
      ) as slot,
    ) ->
      // Pragmatic drain budget: an iterator over `Array(2**32-1)` or over a
      // hostile `{length: 1e300}` array-like is spec-legal to CREATE (§23.1.5.1
      // is ToObject and nothing else) but must not be allowed to spin the VM
      // for hours. Bail on the STEP that exceeds the budget, not at creation —
      // creation reads nothing and can never be observed to fail.
      case index >= limits.max_iteration {
        True ->
          rethrow(Error(state.range_error_value(state, "Invalid array length")))
        False -> step_at(state, iter_ref, slot, source, index, iter_kind)
      }
    _ ->
      Error(VmFailed(
        InternalError("ArrayIteratorNext", "not an array-iterator slot"),
        state,
      ))
  }
}

/// `slot` is the iterator's own heap slot, already read by `step` — threaded
/// through so the cursor write never re-reads it (`heap.read` is the hottest
/// function in the VM, and this is the inner loop of `for..of` over an array).
fn step_at(
  state: State(host),
  iter_ref: Ref,
  slot: state.HeapSlot(host),
  source: Ref,
  index: Int,
  iter_kind: ArrayIterKind,
) -> Result(#(ArrayIterStep, State(host)), StepExit(host)) {
  case heap.read(state.heap, source) {
    // Typed-array source — §23.1.5.1: re-validate the buffer witness and read
    // the element through the live backing store each step (mutation during
    // iteration is observed; detached/out-of-bounds views throw TypeError).
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
        Ok(len) ->
          case index >= len {
            True ->
              Ok(#(
                Exhausted,
                exhaust_direct(state, iter_ref, slot, source, iter_kind),
              ))
            False -> {
              let elem =
                object.typed_array_element(
                  state.heap,
                  buffer,
                  elem_kind,
                  byte_offset,
                  len,
                  index,
                )
                |> option.unwrap(JsUndefined)
              Ok(yield_at_direct(
                state,
                iter_ref,
                slot,
                source,
                iter_kind,
                index,
                elem,
              ))
            }
          }
      }
    _ ->
      case heap.read_array_like(state.heap, source) {
        // Array/Arguments source: `length` and the element block are right
        // there in the slot, no [[Get]] needed for the length.
        Some(#(length, elems)) ->
          case index >= length {
            True -> Ok(#(Exhausted, exhaust(state, iter_ref)))
            False ->
              // §23.1.5.1: Let elementValue be ? Get(array, elementKey). The
              // elements store is only a fast path for plain data values —
              // defineProperty can install accessor/attribute overrides at an
              // index (kept in the properties dict), and holes consult the
              // prototype chain, so fall back to the generic [[Get]] when the
              // element store has no entry or an override exists.
              case iter_kind {
                value.ArrayIterKeys ->
                  Ok(yield_at(state, iter_ref, iter_kind, index, JsUndefined))
                _ -> {
                  use #(elem, state) <- result.map(
                    case
                      element_without_override(state.heap, source, elems, index)
                    {
                      Some(v) -> Ok(#(v, state))
                      None ->
                        rethrow(object.get_value(
                          state,
                          source,
                          Index(index),
                          JsObject(source),
                        ))
                    },
                  )
                  yield_at(state, iter_ref, iter_kind, index, elem)
                }
              }
          }
        // Everything else — a Proxy, or any array-LIKE the iterator was
        // borrowed onto (`Array.prototype.values.call({length: 2, 0: "a"})`) —
        // is the spec's plain §23.1.5.1: ? Get(array, "length") then
        // ? Get(array, ToString(index)), both of which run getters / fire
        // proxy traps.
        None -> {
          use #(length, state) <- result.try(array_like_length(state, source))
          case index >= length {
            True -> Ok(#(Exhausted, exhaust(state, iter_ref)))
            False ->
              // §23.1.5.1 step 8.b.iii: a "key" iterator yields the index
              // WITHOUT performing Get(array, elementKey) — no get trap fires.
              case iter_kind {
                value.ArrayIterKeys ->
                  Ok(yield_at(state, iter_ref, iter_kind, index, JsUndefined))
                _ -> {
                  use #(elem, state) <- result.map(
                    rethrow(object.get_value(
                      state,
                      source,
                      Index(index),
                      JsObject(source),
                    )),
                  )
                  yield_at(state, iter_ref, iter_kind, index, elem)
                }
              }
          }
        }
      }
  }
}

/// §7.1.20 LengthOfArrayLike, in this module's StepExit error shape — the
/// implementation itself is `property.length_of_array_like`, so a hostile
/// `{length: 1e300}` / `{length: NaN}` source is clamped to [0, 2^53-1] the
/// same way every other array-like iteration in the runtime clamps it.
fn array_like_length(
  state: State(host),
  source: Ref,
) -> Result(#(Int, State(host)), StepExit(host)) {
  rethrow(property.length_of_array_like(state, source, JsObject(source)))
}

/// Bump the cursor past `index`, then shape one iteration result for the
/// iterator's kind (§23.1.5.1) — the index ("key"), the element ("value"), or a
/// fresh [index, element] pair array ("key+value").
fn yield_at(
  state: State(host),
  iter_ref: Ref,
  iter_kind: ArrayIterKind,
  index: Int,
  elem: JsValue,
) -> #(ArrayIterStep, State(host)) {
  let heap = set_cursor(state.heap, iter_ref, Some(index + 1))
  shape_yield(state, heap, iter_kind, index, elem)
}

/// `yield_at` for a caller holding a still-current iterator slot — see
/// `set_cursor_direct`.
fn yield_at_direct(
  state: State(host),
  iter_ref: Ref,
  slot: state.HeapSlot(host),
  source: Ref,
  iter_kind: ArrayIterKind,
  index: Int,
  elem: JsValue,
) -> #(ArrayIterStep, State(host)) {
  let heap =
    set_cursor_direct(
      state.heap,
      iter_ref,
      slot,
      source,
      iter_kind,
      Some(index + 1),
    )
  shape_yield(state, heap, iter_kind, index, elem)
}

fn shape_yield(
  state: State(host),
  heap: state.Heap(host),
  iter_kind: ArrayIterKind,
  index: Int,
  elem: JsValue,
) -> #(ArrayIterStep, State(host)) {
  let #(heap, out) =
    shape_result(heap, state.builtins.array.prototype, iter_kind, index, elem)
  #(Yielded(out), State(..state, heap:))
}

/// Latch the iterator done — `cursor: None`, the spec's [[IteratedObject]] =
/// undefined state.
fn exhaust(state: State(host), iter_ref: Ref) -> State(host) {
  State(..state, heap: exhaust_heap(state.heap, iter_ref))
}

/// `exhaust` for a caller that only holds a heap (the spread fast paths in
/// `ops/array`, which drain an iterator without ever building a `State`).
pub fn exhaust_heap(h: state.Heap(host), iter_ref: Ref) -> state.Heap(host) {
  set_cursor(h, iter_ref, None)
}

/// `exhaust` for a caller holding a still-current iterator slot — see
/// `set_cursor_direct`.
fn exhaust_direct(
  state: State(host),
  iter_ref: Ref,
  slot: state.HeapSlot(host),
  source: Ref,
  iter_kind: ArrayIterKind,
) -> State(host) {
  State(
    ..state,
    heap: set_cursor_direct(state.heap, iter_ref, slot, source, iter_kind, None),
  )
}

/// §23.1.5.1: shape one iteration result for the iterator's kind — the
/// index ("key"), the element ("value"), or a fresh [index, element] pair
/// array ("key+value").
pub fn shape_result(
  h: state.Heap(host),
  array_proto: Ref,
  iter_kind: ArrayIterKind,
  index: Int,
  elem: JsValue,
) -> #(state.Heap(host), JsValue) {
  case iter_kind {
    value.ArrayIterKeys -> #(h, value.from_int(index))
    value.ArrayIterValues -> #(h, elem)
    value.ArrayIterEntries -> {
      let #(h, pair_ref) =
        common.alloc_array(h, [value.from_int(index), elem], array_proto)
      #(h, JsObject(pair_ref))
    }
  }
}

/// Move an ArrayIteratorObject's cursor (preserving source and iteration
/// kind), re-reading the slot first: this step read the element through
/// [[Get]], so an accessor or a proxy trap may have run and mutated the
/// iterator object itself. No-op if the ref no longer holds an array-iterator
/// slot (a getter can swap the whole slot out).
fn set_cursor(
  h: state.Heap(host),
  iter_ref: Ref,
  cursor: Option(Int),
) -> state.Heap(host) {
  case heap.read(h, iter_ref) {
    Some(
      ObjectSlot(kind: value.ArrayIteratorObject(source:, iter_kind:, ..), ..) as slot,
    ) -> set_cursor_direct(h, iter_ref, slot, source, iter_kind, cursor)
    _ -> h
  }
}

/// `set_cursor` for a caller that already holds the iterator's slot and knows
/// no user code has run since it was read (the typed-array path reads the
/// element straight out of the backing store) — saves a `heap.read`, the VM's
/// hottest function, in the inner loop of `for (const x of typedArray)`.
fn set_cursor_direct(
  h: state.Heap(host),
  iter_ref: Ref,
  slot: state.HeapSlot(host),
  source: Ref,
  iter_kind: ArrayIterKind,
  cursor: Option(Int),
) -> state.Heap(host) {
  // Every caller matched an ObjectSlot to get here; a non-object slot means
  // the caller was miswired, and silently dropping the cursor write would
  // spin the iterator forever.
  let assert ObjectSlot(..) as obj = slot
    as "array_iterator: cursor target is not an object slot"
  heap.write(
    h,
    iter_ref,
    ObjectSlot(
      ..obj,
      kind: value.ArrayIteratorObject(source:, cursor:, iter_kind:),
    ),
  )
}

/// Fast-path element read for the array iterator: Some(value) only when the
/// index is a plain data value in the element store with no properties-dict
/// override (defineProperty can install accessor/attribute overrides at an
/// index). None → caller takes the generic [[Get]] path.
fn element_without_override(
  h: state.Heap(host),
  source: Ref,
  elems: value.JsElements,
  index: Int,
) -> Option(JsValue) {
  case heap.read(h, source) {
    Some(ObjectSlot(properties:, ..)) ->
      case dict.has_key(properties, Index(index)) {
        True -> None
        False -> elements.get_option(elems, index)
      }
    _ -> None
  }
}
