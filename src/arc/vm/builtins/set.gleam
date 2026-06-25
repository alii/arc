/// ES2024 §24.2 Set Objects
///
/// A Set is a collection of unique values. Key equality follows the
/// SameValueZero algorithm (NaN === NaN, +0 === -0).
///
/// Stores values in a Dict(MapKey, JsValue) mapping normalized MapKey →
/// original JsValue, plus the spec's append-only [[SetData]] order modelled
/// with monotonically increasing sequence numbers (`seqs`: key → seq,
/// `order`: seq → key, `next_seq`). delete() removes the record; the seq gap
/// is the spec's emptied record, so a deleted-then-re-added value gets a
/// fresh seq and is revisited by in-flight iterators per §24.2.5. Iteration
/// points call value.set_live_values to recover forward insertion order.
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers.{first_arg_or_undefined}
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsValue, type MapKey, type Ref, type SetNativeFn, Dispatch, Finite,
  Infinity, JsBool, JsNumber, JsObject, JsUndefined, NaN, Named, NegInfinity,
  ObjectSlot, SetConstructor, SetNative, SetObject, SetPrototypeAdd,
  SetPrototypeClear, SetPrototypeDelete, SetPrototypeDifference,
  SetPrototypeEntries, SetPrototypeForEach, SetPrototypeGetSize, SetPrototypeHas,
  SetPrototypeIntersection, SetPrototypeIsDisjointFrom, SetPrototypeIsSubsetOf,
  SetPrototypeIsSupersetOf, SetPrototypeSymmetricDifference, SetPrototypeUnion,
  SetPrototypeValues,
}
import gleam/dict
import gleam/float
import gleam/list
import gleam/option.{Some}
import gleam/result

/// Set up Set.prototype and Set constructor.
pub fn init(
  h: Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(host), BuiltinType) {
  // §24.2.3.12 Set.prototype.values doubles as §24.2.3.11 keys and
  // §24.2.3.13 [@@iterator]; §24.2.3.16 [@@toStringTag] = "Set".
  common.init_keyed_collection(
    h,
    object_proto,
    function_proto,
    [
      #("add", SetNative(SetPrototypeAdd), 1),
      #("has", SetNative(SetPrototypeHas), 1),
      #("delete", SetNative(SetPrototypeDelete), 1),
      #("clear", SetNative(SetPrototypeClear), 0),
      #("forEach", SetNative(SetPrototypeForEach), 1),
      #("union", SetNative(SetPrototypeUnion), 1),
      #("intersection", SetNative(SetPrototypeIntersection), 1),
      #("difference", SetNative(SetPrototypeDifference), 1),
      #("symmetricDifference", SetNative(SetPrototypeSymmetricDifference), 1),
      #("isSubsetOf", SetNative(SetPrototypeIsSubsetOf), 1),
      #("isSupersetOf", SetNative(SetPrototypeIsSupersetOf), 1),
      #("isDisjointFrom", SetNative(SetPrototypeIsDisjointFrom), 1),
      #("entries", SetNative(SetPrototypeEntries), 0),
    ],
    "values",
    SetNative(SetPrototypeValues),
    ["keys"],
    SetNative(SetPrototypeGetSize),
    fn(proto) { Dispatch(SetNative(SetConstructor(proto:))) },
    "Set",
  )
}

/// Per-module dispatch for Set native functions.
pub fn dispatch(
  native: SetNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case native {
    SetConstructor(_) -> construct(args, state)
    SetPrototypeAdd -> set_add(this, args, state)
    SetPrototypeHas -> set_has(this, args, state)
    SetPrototypeDelete -> set_delete(this, args, state)
    SetPrototypeClear -> set_clear(this, state)
    SetPrototypeForEach -> set_for_each(this, args, state)
    SetPrototypeGetSize -> set_size(this, state)
    SetPrototypeUnion -> set_union(this, args, state)
    SetPrototypeIntersection -> set_intersection(this, args, state)
    SetPrototypeDifference -> set_difference(this, args, state)
    SetPrototypeSymmetricDifference ->
      set_symmetric_difference(this, args, state)
    SetPrototypeIsSubsetOf -> set_is_subset_of(this, args, state)
    SetPrototypeIsSupersetOf -> set_is_superset_of(this, args, state)
    SetPrototypeIsDisjointFrom -> set_is_disjoint_from(this, args, state)
    SetPrototypeValues -> set_values(this, state)
    SetPrototypeEntries -> set_entries(this, state)
  }
}

/// ES2024 §24.2.1.1 Set ( [ iterable ] )
fn construct(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Gather initial values from array argument
  case args {
    [JsObject(ref), ..] ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(
          kind: value.ArrayObject(length:),
          elements: els,
          properties:,
          ..,
        )) ->
          case object.has_index_overrides(properties) {
            // Fast path: dense elements only — read them raw.
            False ->
              alloc_new_set_from_values(
                state,
                elements.to_list_padded(els, length),
              )
            // defineProperty moved an element into the dict (e.g. a getter)
            // — raw elements would read the hole as undefined. Per-index
            // Get honors the override; the getter can also resize the
            // array mid-iteration, so length is re-read each step.
            True ->
              case gather_indexed_values(state, ref, 0, []) {
                Ok(#(values, state)) -> alloc_new_set_from_values(state, values)
                Error(#(thrown, state)) -> #(state, Error(thrown))
              }
          }
        _ -> alloc_new_set_from_values(state, [])
      }
    _ -> alloc_new_set_from_values(state, [])
  }
}

/// Collect array values via per-index [[Get]] — the slow path used when an
/// element has been converted to a dict override (accessor etc.). Mirrors
/// the array iterator: length is re-read every step because a getter can
/// push onto or truncate the source array mid-iteration.
fn gather_indexed_values(
  state: State(host),
  ref: Ref,
  idx: Int,
  acc: List(JsValue),
) -> Result(#(List(JsValue), State(host)), #(JsValue, State(host))) {
  let length =
    heap.read_array(state.heap, ref)
    |> option.map(fn(p) { p.0 })
    |> option.unwrap(0)
  case idx >= length {
    True -> Ok(#(list.reverse(acc), state))
    False -> {
      use #(v, state) <- result.try(object.get_value_of(
        state,
        JsObject(ref),
        value.Index(idx),
      ))
      gather_indexed_values(state, ref, idx + 1, [v, ..acc])
    }
  }
}

/// Helper to update a SetObject's data on the heap.
fn update_set(
  h: Heap(host),
  ref: Ref,
  data: dict.Dict(MapKey, JsValue),
  seqs: dict.Dict(MapKey, Int),
  order: dict.Dict(Int, MapKey),
  next_seq: Int,
) -> Heap(host) {
  heap.update_kind(h, ref, SetObject(data:, seqs:, order:, next_seq:))
}

/// ES2024 §24.2.3.1 Set.prototype.add ( value )
fn set_add(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use data, seqs, order, next_seq, ref, state <- require_set(this, state)
  let val = first_arg_or_undefined(args)
  let key = value.js_to_map_key(val)
  let new_data = dict.insert(data, key, val)
  // An existing value keeps its insertion position (seq); a new one —
  // including a deleted-then-re-added one — appends at next_seq, past every
  // live iterator's cursor.
  let #(seqs, order, next_seq) = case dict.has_key(data, key) {
    True -> #(seqs, order, next_seq)
    False -> #(
      dict.insert(seqs, key, next_seq),
      dict.insert(order, next_seq, key),
      next_seq + 1,
    )
  }
  let heap = update_set(state.heap, ref, new_data, seqs, order, next_seq)
  #(State(..state, heap:), Ok(this))
}

/// ES2024 §24.2.3.4 Set.prototype.has ( value )
fn set_has(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use data, _seqs, _order, _next_seq, _ref, state <- require_set(this, state)
  let key = value.js_to_map_key(first_arg_or_undefined(args))
  #(state, Ok(JsBool(dict.has_key(data, key))))
}

/// ES2024 §24.2.3.3 Set.prototype.delete ( value )
///
/// Removes the record entirely; the seq gap left in `order` is the spec's
/// emptied record (skipped by iterator cursors in O(1)).
fn set_delete(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use data, seqs, order, next_seq, ref, state <- require_set(this, state)
  let key = value.js_to_map_key(first_arg_or_undefined(args))
  case dict.get(seqs, key) {
    Error(Nil) -> #(state, Ok(JsBool(False)))
    Ok(seq) -> {
      let heap =
        update_set(
          state.heap,
          ref,
          dict.delete(data, key),
          dict.delete(seqs, key),
          dict.delete(order, seq),
          next_seq,
        )
      #(State(..state, heap:), Ok(JsBool(True)))
    }
  }
}

/// ES2024 §24.2.3.2 Set.prototype.clear ()
fn set_clear(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use _data, _seqs, _order, next_seq, ref, state <- require_set(this, state)
  // next_seq is preserved: clear() empties the spec's records but appends
  // still land past in-flight iterator cursors, so they remain visited.
  let heap =
    update_set(state.heap, ref, dict.new(), dict.new(), dict.new(), next_seq)
  #(State(..state, heap:), Ok(JsUndefined))
}

/// ES2024 §24.2.3.5 get Set.prototype.size
fn set_size(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use data, _seqs, _order, _next_seq, _ref, state <- require_set(this, state)
  #(state, Ok(value.from_int(dict.size(data))))
}

/// ES2024 §24.2.3.6 Set.prototype.forEach ( callbackfn [ , thisArg ] )
fn set_for_each(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use _data, _seqs, _order, _next_seq, ref, state <- require_set(this, state)
  let callback = first_arg_or_undefined(args)
  let this_arg = case args {
    [_, ta, ..] -> ta
    _ -> JsUndefined
  }
  case helpers.is_callable(state.heap, callback) {
    False ->
      state.type_error(
        state,
        "Set.prototype.forEach callback is not a function",
      )
    True -> for_each_loop(state, ref, 0, callback, this_arg, this)
  }
}

/// Iterate over Set entries, calling callback(value, value, set) for each.
/// LIVE iteration by seq cursor — the source is re-read from the heap each
/// step, so entries the callback deletes before being reached are skipped
/// and entries it adds (including delete + re-add) are visited.
fn for_each_loop(
  state: State(host),
  ref: Ref,
  cursor: Int,
  callback: JsValue,
  this_arg: JsValue,
  set_this: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  let next = case heap.read(state.heap, ref) {
    Some(ObjectSlot(kind: SetObject(data:, order:, next_seq:, ..), ..)) ->
      value.entry_from_seq(data, order, cursor, next_seq)
    _ -> option.None
  }
  case next {
    option.None -> #(state, Ok(JsUndefined))
    Some(#(seq, _key, val)) ->
      case state.call(state, callback, this_arg, [val, val, set_this]) {
        Ok(#(_result, new_state)) ->
          for_each_loop(new_state, ref, seq + 1, callback, this_arg, set_this)
        Error(#(thrown, new_state)) -> #(new_state, Error(thrown))
      }
  }
}

/// ES2025 §24.2.3.14 Set.prototype.union ( other )
fn set_union(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use data, seqs, order, next_seq, _ref, state <- require_set(this, state)
  use rec, state <- get_set_record(first_arg_or_undefined(args), state)
  use other_values, state <- with_drained_keys(state, rec)
  // Copy this's records, then append other's values (in its iteration order)
  // that aren't already present.
  let #(data, seqs, order, next_seq) =
    list.fold(other_values, #(data, seqs, order, next_seq), fn(acc, v) {
      let #(d, sq, od, ns) = acc
      let key = value.js_to_map_key(v)
      case dict.has_key(d, key) {
        True -> acc
        False -> #(
          dict.insert(d, key, v),
          dict.insert(sq, key, ns),
          dict.insert(od, ns, key),
          ns + 1,
        )
      }
    })
  alloc_new_set(state, data, seqs, order, next_seq)
}

/// ES2025 §24.2.3.7 Set.prototype.intersection ( other )
fn set_intersection(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use data, _seqs, order, _next_seq, _ref, state <- require_set(this, state)
  use rec, state <- get_set_record(first_arg_or_undefined(args), state)
  // Iterate this's elements in forward order, keep those where other.has(e).
  let entries = value.set_live_values(data, order)
  use kept, state <- with_filtered_by_has(state, rec, entries, True)
  alloc_new_set_from_values(state, kept)
}

/// ES2025 §24.2.3.3 Set.prototype.difference ( other )
fn set_difference(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use data, _seqs, order, _next_seq, _ref, state <- require_set(this, state)
  use rec, state <- get_set_record(first_arg_or_undefined(args), state)
  // Iterate this's elements in forward order, keep those where !other.has(e).
  let entries = value.set_live_values(data, order)
  use kept, state <- with_filtered_by_has(state, rec, entries, False)
  alloc_new_set_from_values(state, kept)
}

/// ES2025 §24.2.3.13 Set.prototype.symmetricDifference ( other )
///
/// Spec algorithm: copy this → resultSetData, then drain other.keys();
/// for each nextValue, if it's in this remove it from result, else add it.
/// (Using `this` for the membership test, not the mutating result, so an
/// element appearing twice in other doesn't toggle back in.)
fn set_symmetric_difference(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use data, seqs, order, next_seq, _ref, state <- require_set(this, state)
  use rec, state <- get_set_record(first_arg_or_undefined(args), state)
  use other_values, state <- with_drained_keys(state, rec)
  let #(result_data, result_seqs, result_order, result_next) =
    list.fold(other_values, #(data, seqs, order, next_seq), fn(acc, v) {
      let #(d, sq, od, ns) = acc
      let key = value.js_to_map_key(v)
      case dict.has_key(data, key) {
        // In this → remove from result (the record may already be gone if
        // other yielded it twice; removal is a no-op then).
        True ->
          case dict.get(sq, key) {
            Ok(seq) -> #(
              dict.delete(d, key),
              dict.delete(sq, key),
              dict.delete(od, seq),
              ns,
            )
            Error(Nil) -> acc
          }
        // Not in this → add to result if not already added.
        False ->
          case dict.has_key(d, key) {
            True -> acc
            False -> #(
              dict.insert(d, key, v),
              dict.insert(sq, key, ns),
              dict.insert(od, ns, key),
              ns + 1,
            )
          }
      }
    })
  alloc_new_set(state, result_data, result_seqs, result_order, result_next)
}

/// ES2025 §24.2.3.9 Set.prototype.isSubsetOf ( other )
fn set_is_subset_of(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use data, _seqs, order, _next_seq, _ref, state <- require_set(this, state)
  use rec, state <- get_set_record(first_arg_or_undefined(args), state)
  // §24.2.3.9 step 4: if thisSize > otherRec.size, return false
  case dict.size(data) > rec.size {
    True -> #(state, Ok(JsBool(False)))
    False -> {
      let entries = value.set_live_values(data, order)
      all_match_has(state, rec, entries, True)
    }
  }
}

/// ES2025 §24.2.3.10 Set.prototype.isSupersetOf ( other )
fn set_is_superset_of(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use data, _seqs, _order, _next_seq, _ref, state <- require_set(this, state)
  use rec, state <- get_set_record(first_arg_or_undefined(args), state)
  // §24.2.3.10 step 4: if thisSize < otherRec.size, return false
  case dict.size(data) < rec.size {
    True -> #(state, Ok(JsBool(False)))
    False -> {
      use other_values, state <- with_drained_keys(state, rec)
      let is_superset =
        list.all(other_values, fn(v) {
          dict.has_key(data, value.js_to_map_key(v))
        })
      #(state, Ok(JsBool(is_superset)))
    }
  }
}

/// ES2025 §24.2.3.8 Set.prototype.isDisjointFrom ( other )
fn set_is_disjoint_from(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use data, _seqs, order, _next_seq, _ref, state <- require_set(this, state)
  use rec, state <- get_set_record(first_arg_or_undefined(args), state)
  // every element of this must NOT be in other
  let entries = value.set_live_values(data, order)
  all_match_has(state, rec, entries, False)
}

/// ES2024 §24.2.3.12 Set.prototype.values ()
/// Returns a new Set Iterator object (§24.2.5.1 CreateSetIterator).
fn set_values(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use _data, _seqs, _order, _next_seq, ref, state <- require_set(this, state)
  alloc_set_iterator(state, ref, value.SetIterValues)
}

/// ES2024 §24.2.3.5 Set.prototype.entries ()
fn set_entries(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use _data, _seqs, _order, _next_seq, ref, state <- require_set(this, state)
  alloc_set_iterator(state, ref, value.SetIterEntries)
}

/// Allocate a LIVE SetIteratorObject over the source Set (§24.2.5.1) —
/// entries added during iteration are visited.
fn alloc_set_iterator(
  state: State(host),
  source: Ref,
  kind: value.SetIterKind,
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(heap, ref) =
    common.alloc_wrapper(
      state.heap,
      value.SetIteratorObject(source:, cursor: 0, done: False, kind:),
      state.builtins.set_iterator_proto,
    )
  #(State(..state, heap:), Ok(JsObject(ref)))
}

/// Allocate a new Set object from its record dicts.
fn alloc_new_set(
  state: State(host),
  data: dict.Dict(MapKey, JsValue),
  seqs: dict.Dict(MapKey, Int),
  order: dict.Dict(Int, MapKey),
  next_seq: Int,
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(heap, ref) =
    common.alloc_wrapper(
      state.heap,
      SetObject(data:, seqs:, order:, next_seq:),
      state.builtins.set.prototype,
    )
  #(State(..state, heap:), Ok(JsObject(ref)))
}

/// Allocate a new Set from a forward-ordered list of values.
fn alloc_new_set_from_values(
  state: State(host),
  values: List(JsValue),
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(data, seqs, order, next_seq) =
    list.fold(values, #(dict.new(), dict.new(), dict.new(), 0), fn(acc, v) {
      let #(d, sq, od, ns) = acc
      let key = value.js_to_map_key(v)
      case dict.has_key(d, key) {
        True -> acc
        False -> #(
          dict.insert(d, key, v),
          dict.insert(sq, key, ns),
          dict.insert(od, ns, key),
          ns + 1,
        )
      }
    })
  alloc_new_set(state, data, seqs, order, next_seq)
}

// ---- GetSetRecord + protocol helpers ----

/// Spec's "Set Record" — captured size/has/keys from the other argument.
/// `size` is the post-ToIntegerOrInfinity integer (Infinity → max int).
type SetRecord {
  SetRecord(obj: JsValue, size: Int, has: JsValue, keys: JsValue)
}

/// ES2025 §24.2.1.2 GetSetRecord ( obj )
///
/// Validates `other` is set-like: reads .size (ToNumber → NaN check →
/// ToIntegerOrInfinity → negative check), then .has and .keys (both must be
/// callable). CPS-style — call with `use rec, state <- get_set_record(other, state)`.
fn get_set_record(
  other: JsValue,
  state: State(host),
  cont: fn(SetRecord, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case other {
    JsObject(ref) -> {
      // Step 2: rawSize = Get(obj, "size")
      use raw_size, state <- state.try_op(object.get_value(
        state,
        ref,
        Named("size"),
        other,
      ))
      // Step 3: numSize = ToNumber(rawSize). Route via ToPrimitive(NumberHint)
      // so {valueOf(){...}} works — value.to_number on a raw object yields NaN.
      use prim, state <- state.try_op(coerce.to_primitive(
        state,
        raw_size,
        coerce.NumberHint,
      ))
      case value.to_number(prim) {
        // Step 4: if numSize is NaN, throw TypeError
        Error(msg) -> state.type_error(state, msg)
        Ok(NaN) -> state.type_error(state, "size is NaN")
        Ok(num) -> {
          // Step 5: ToIntegerOrInfinity(numSize)
          let int_size = case num {
            Finite(f) -> float.truncate(f)
            Infinity -> 2_147_483_647
            NegInfinity -> -1
            NaN -> 0
          }
          // Step 6: if intSize < 0, throw RangeError
          case int_size < 0 {
            True -> state.range_error(state, "size is negative")
            False -> {
              // Step 7-8: has = Get(obj, "has"); IsCallable check
              use has, state <- state.try_op(object.get_value(
                state,
                ref,
                Named("has"),
                other,
              ))
              case helpers.is_callable(state.heap, has) {
                False -> state.type_error(state, "has is not a function")
                True -> {
                  // Step 9-10: keys = Get(obj, "keys"); IsCallable check
                  use keys, state <- state.try_op(object.get_value(
                    state,
                    ref,
                    Named("keys"),
                    other,
                  ))
                  case helpers.is_callable(state.heap, keys) {
                    False -> state.type_error(state, "keys is not a function")
                    True ->
                      cont(
                        SetRecord(obj: other, size: int_size, has:, keys:),
                        state,
                      )
                  }
                }
              }
            }
          }
        }
      }
    }
    _ -> state.type_error(state, "other is not an object")
  }
}

/// Call rec.keys(), then drain the returned iterator via .next() into a list.
/// CPS wrapper so callers can `use values, state <- with_drained_keys(...)`.
fn with_drained_keys(
  state: State(host),
  rec: SetRecord,
  cont: fn(List(JsValue), State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  use iter, state <- state.try_call(state, rec.keys, rec.obj, [])
  case iter {
    JsObject(iter_ref) -> {
      use next_fn, state <- state.try_op(object.get_value(
        state,
        iter_ref,
        Named("next"),
        iter,
      ))
      case helpers.is_callable(state.heap, next_fn) {
        False -> state.type_error(state, "iterator.next is not a function")
        True -> drain_loop(state, iter, next_fn, [], cont)
      }
    }
    _ -> state.type_error(state, "keys() did not return an object")
  }
}

fn drain_loop(
  state: State(host),
  iter: JsValue,
  next_fn: JsValue,
  acc: List(JsValue),
  cont: fn(List(JsValue), State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  use result_obj, state <- state.try_call(state, next_fn, iter, [])
  case result_obj {
    JsObject(rref) -> {
      use done, state <- state.try_op(object.get_value(
        state,
        rref,
        Named("done"),
        result_obj,
      ))
      case value.is_truthy(done) {
        True -> cont(list.reverse(acc), state)
        False -> {
          use v, state <- state.try_op(object.get_value(
            state,
            rref,
            Named("value"),
            result_obj,
          ))
          // §24.2.1.2 step 7.b.ii: if nextValue is -0, set nextValue to +0.
          let v = normalize_neg_zero(v)
          drain_loop(state, iter, next_fn, [v, ..acc], cont)
        }
      }
    }
    _ -> state.type_error(state, "iterator result is not an object")
  }
}

/// SetDataKeyToValue helper — -0 normalizes to +0 (SameValueZero semantics).
/// IEEE 754: -0.0 +. 0.0 == +0.0; identity for all other floats.
fn normalize_neg_zero(v: JsValue) -> JsValue {
  case v {
    JsNumber(Finite(f)) -> JsNumber(Finite(f +. 0.0))
    other -> other
  }
}

/// Call rec.has(v), ToBoolean the result.
fn set_record_has(
  state: State(host),
  rec: SetRecord,
  v: JsValue,
  cont: fn(Bool, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  use r, state <- state.try_call(state, rec.has, rec.obj, [v])
  cont(value.is_truthy(r), state)
}

/// Filter `entries` keeping those where ToBoolean(rec.has(e)) == keep_when.
/// State-threaded recursion since each .has() may mutate the heap or throw.
/// CPS — `use kept, state <- with_filtered_by_has(...)`. Result is forward order.
fn with_filtered_by_has(
  state: State(host),
  rec: SetRecord,
  entries: List(JsValue),
  keep_when: Bool,
  cont: fn(List(JsValue), State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  filter_loop(state, rec, entries, keep_when, [], cont)
}

fn filter_loop(
  state: State(host),
  rec: SetRecord,
  entries: List(JsValue),
  keep_when: Bool,
  acc: List(JsValue),
  cont: fn(List(JsValue), State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case entries {
    [] -> cont(list.reverse(acc), state)
    [v, ..rest] -> {
      use present, state <- set_record_has(state, rec, v)
      let acc = case present == keep_when {
        True -> [v, ..acc]
        False -> acc
      }
      filter_loop(state, rec, rest, keep_when, acc, cont)
    }
  }
}

/// Return JsBool(true) if every entry has rec.has(e) == expected, else false.
/// Short-circuits on first mismatch. State-threaded.
fn all_match_has(
  state: State(host),
  rec: SetRecord,
  entries: List(JsValue),
  expected: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  case entries {
    [] -> #(state, Ok(JsBool(True)))
    [v, ..rest] -> {
      use present, state <- set_record_has(state, rec, v)
      case present == expected {
        False -> #(state, Ok(JsBool(False)))
        True -> all_match_has(state, rec, rest, expected)
      }
    }
  }
}

// ---- helpers ----

/// Unwrap `this` as a Set or return a TypeError.
/// CPS-style — call with
/// `use data, seqs, order, next_seq, ref, state <- require_set(this, state)`.
fn require_set(
  this: JsValue,
  state: State(host),
  cont: fn(
    dict.Dict(MapKey, JsValue),
    dict.Dict(MapKey, Int),
    dict.Dict(Int, MapKey),
    Int,
    Ref,
    State(host),
  ) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  let err = "Method Set.prototype.* called on incompatible receiver"
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: SetObject(data:, seqs:, order:, next_seq:), ..)) ->
          cont(data, seqs, order, next_seq, ref, state)
        _ -> state.type_error(state, err)
      }
    _ -> state.type_error(state, err)
  }
}
