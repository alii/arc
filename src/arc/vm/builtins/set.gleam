/// ES2024 §24.2 Set Objects
///
/// A Set is a collection of unique values. Key equality follows the
/// SameValueZero algorithm (NaN === NaN, +0 === -0).
///
/// Stores values in a Dict(MapKey, JsValue) + List(MapKey) for insertion order.
/// The dict maps normalized MapKey → original JsValue.
/// The keys list is stored in REVERSE insertion order so add() is O(1) prepend;
/// iteration points call list.reverse() once to recover forward order.
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers.{first_arg_or_undefined}
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsValue, type MapKey, type Ref, type SetNativeFn, Dispatch, Finite,
  Infinity, JsBool, JsNull, JsNumber, JsObject, JsUndefined, NaN, Named,
  NegInfinity, ObjectSlot, SetConstructor, SetNative, SetObject, SetPrototypeAdd,
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
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, BuiltinType) {
  // Allocate prototype methods (values handled separately so it can alias
  // keys and [Symbol.iterator] to the SAME function object — test262
  // built-ins/Set/prototype/keys/keys.js asserts strict equality).
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
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
    ])

  // §24.2.3.11 Set.prototype.keys === §24.2.3.12 values === §24.2.3.13 [@@iterator]
  let #(h, values_fn) =
    common.alloc_native_fn(
      h,
      function_proto,
      SetNative(SetPrototypeValues),
      "values",
      0,
    )
  let values_prop = value.builtin_property(JsObject(values_fn))

  // size accessor property (getter, no setter)
  let #(h, getters) =
    common.alloc_getters(h, function_proto, [
      #("size", SetNative(SetPrototypeGetSize)),
    ])
  let proto_props =
    list.append(getters, [
      #("values", values_prop),
      #("keys", values_prop),
      ..proto_methods
    ])

  let #(h, bt) =
    common.init_type(
      h,
      object_proto,
      function_proto,
      proto_props,
      fn(proto) { Dispatch(SetNative(SetConstructor(proto:))) },
      "Set",
      0,
      [],
    )
  // §24.2.3.16 Set.prototype [ @@toStringTag ] = "Set"
  // { writable: false, enumerable: false, configurable: true }
  let h = common.add_to_string_tag(h, bt.prototype, "Set")
  // §24.2.3.13 Set.prototype [ @@iterator ] — same function object as .values
  let h =
    common.add_symbol_property(
      h,
      bt.prototype,
      value.symbol_iterator,
      values_prop,
    )
  #(h, bt)
}

/// Per-module dispatch for Set native functions.
pub fn dispatch(
  native: SetNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Gather initial values from array argument
  let initial_values = case args {
    [] -> []
    [JsUndefined, ..] -> []
    [JsNull, ..] -> []
    [JsObject(ref), ..] ->
      heap.read_array(state.heap, ref)
      |> option.map(fn(p) { read_array_elements(p.1, 0, p.0, []) })
      |> option.unwrap([])
    _ -> []
  }
  alloc_new_set_from_values(state, initial_values)
}

/// Read elements from array into a list.
fn read_array_elements(
  elements: value.JsElements,
  idx: Int,
  length: Int,
  acc: List(JsValue),
) -> List(JsValue) {
  case idx >= length {
    True -> list.reverse(acc)
    False -> {
      let val = elements.get(elements, idx)
      read_array_elements(elements, idx + 1, length, [val, ..acc])
    }
  }
}

/// Helper to update a SetObject's data on the heap.
fn update_set(
  h: Heap,
  ref: Ref,
  data: dict.Dict(MapKey, JsValue),
  keys: List(MapKey),
) -> Heap {
  heap.update_kind(h, ref, SetObject(data:, keys:))
}

/// ES2024 §24.2.3.1 Set.prototype.add ( value )
fn set_add(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, keys, ref, state <- require_set(this, state)
  let val = first_arg_or_undefined(args)
  let key = value.js_to_map_key(val)
  let new_data = dict.insert(data, key, val)
  let new_keys = case dict.has_key(data, key) {
    True -> keys
    False -> [key, ..keys]
  }
  let heap = update_set(state.heap, ref, new_data, new_keys)
  #(State(..state, heap:), Ok(this))
}

/// ES2024 §24.2.3.4 Set.prototype.has ( value )
fn set_has(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, _keys, _ref, state <- require_set(this, state)
  let key = value.js_to_map_key(first_arg_or_undefined(args))
  #(state, Ok(JsBool(dict.has_key(data, key))))
}

/// ES2024 §24.2.3.3 Set.prototype.delete ( value )
fn set_delete(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, keys, ref, state <- require_set(this, state)
  let key = value.js_to_map_key(first_arg_or_undefined(args))
  let had = dict.has_key(data, key)
  case had {
    False -> #(state, Ok(JsBool(False)))
    True -> {
      let new_data = dict.delete(data, key)
      let new_keys = list.filter(keys, fn(k) { k != key })
      let heap = update_set(state.heap, ref, new_data, new_keys)
      #(State(..state, heap:), Ok(JsBool(True)))
    }
  }
}

/// ES2024 §24.2.3.2 Set.prototype.clear ()
fn set_clear(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use _data, _keys, ref, state <- require_set(this, state)
  let heap = update_set(state.heap, ref, dict.new(), [])
  #(State(..state, heap:), Ok(JsUndefined))
}

/// ES2024 §24.2.3.5 get Set.prototype.size
fn set_size(this: JsValue, state: State) -> #(State, Result(JsValue, JsValue)) {
  use data, _keys, _ref, state <- require_set(this, state)
  #(state, Ok(value.from_int(dict.size(data))))
}

/// ES2024 §24.2.3.6 Set.prototype.forEach ( callbackfn [ , thisArg ] )
fn set_for_each(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, keys, _ref, state <- require_set(this, state)
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
    True -> {
      let entries =
        list.reverse(keys)
        |> list.filter_map(fn(key) {
          dict.get(data, key) |> result.map(fn(v) { #(key, v) })
        })
      for_each_loop(state, entries, callback, this_arg, this)
    }
  }
}

/// Iterate over Set entries, calling callback(value, value, set) for each.
fn for_each_loop(
  state: State,
  entries: List(#(MapKey, JsValue)),
  callback: JsValue,
  this_arg: JsValue,
  set_this: JsValue,
) -> #(State, Result(JsValue, JsValue)) {
  case entries {
    [] -> #(state, Ok(JsUndefined))
    [#(_key, val), ..rest] ->
      case state.call(state, callback, this_arg, [val, val, set_this]) {
        Ok(#(_result, new_state)) ->
          for_each_loop(new_state, rest, callback, this_arg, set_this)
        Error(#(thrown, new_state)) -> #(new_state, Error(thrown))
      }
  }
}

/// ES2025 §24.2.3.14 Set.prototype.union ( other )
fn set_union(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, keys, _ref, state <- require_set(this, state)
  use rec, state <- get_set_record(first_arg_or_undefined(args), state)
  use other_values, state <- with_drained_keys(state, rec)
  // Keys stored reversed. Iterate other's keys in forward order and prepend
  // new ones onto this's reversed keys — result stays reversed.
  let #(result_data, result_keys) =
    list.fold(other_values, #(data, keys), fn(acc, v) {
      let #(d, ks) = acc
      let key = value.js_to_map_key(v)
      case dict.has_key(d, key) {
        True -> #(d, ks)
        False -> #(dict.insert(d, key, v), [key, ..ks])
      }
    })
  alloc_new_set(state, result_data, result_keys)
}

/// ES2025 §24.2.3.7 Set.prototype.intersection ( other )
fn set_intersection(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, keys, _ref, state <- require_set(this, state)
  use rec, state <- get_set_record(first_arg_or_undefined(args), state)
  // Iterate this's elements in forward order, keep those where other.has(e).
  let entries = list.reverse(keys) |> list.filter_map(dict.get(data, _))
  use kept, state <- with_filtered_by_has(state, rec, entries, True)
  alloc_new_set_from_values(state, kept)
}

/// ES2025 §24.2.3.3 Set.prototype.difference ( other )
fn set_difference(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, keys, _ref, state <- require_set(this, state)
  use rec, state <- get_set_record(first_arg_or_undefined(args), state)
  // Iterate this's elements in forward order, keep those where !other.has(e).
  let entries = list.reverse(keys) |> list.filter_map(dict.get(data, _))
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, keys, _ref, state <- require_set(this, state)
  use rec, state <- get_set_record(first_arg_or_undefined(args), state)
  use other_values, state <- with_drained_keys(state, rec)
  let #(result_data, result_keys) =
    list.fold(other_values, #(data, keys), fn(acc, v) {
      let #(d, ks) = acc
      let key = value.js_to_map_key(v)
      case dict.has_key(data, key) {
        // In this → remove from result (key may already be gone if other
        // yielded it twice; delete is a no-op then). Leave keys as a
        // tombstone — alloc_new_set's consumers tolerate tombstones.
        True -> #(dict.delete(d, key), ks)
        // Not in this → add to result if not already added.
        False ->
          case dict.has_key(d, key) {
            True -> #(d, ks)
            False -> #(dict.insert(d, key, v), [key, ..ks])
          }
      }
    })
  // result_keys may contain tombstones for removed entries — strip them.
  let result_keys = list.filter(result_keys, dict.has_key(result_data, _))
  alloc_new_set(state, result_data, result_keys)
}

/// ES2025 §24.2.3.9 Set.prototype.isSubsetOf ( other )
fn set_is_subset_of(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, keys, _ref, state <- require_set(this, state)
  use rec, state <- get_set_record(first_arg_or_undefined(args), state)
  // §24.2.3.9 step 4: if thisSize > otherRec.size, return false
  case dict.size(data) > rec.size {
    True -> #(state, Ok(JsBool(False)))
    False -> {
      let entries = list.reverse(keys) |> list.filter_map(dict.get(data, _))
      all_match_has(state, rec, entries, True)
    }
  }
}

/// ES2025 §24.2.3.10 Set.prototype.isSupersetOf ( other )
fn set_is_superset_of(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, _keys, _ref, state <- require_set(this, state)
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, keys, _ref, state <- require_set(this, state)
  use rec, state <- get_set_record(first_arg_or_undefined(args), state)
  // every element of this must NOT be in other
  let entries = list.reverse(keys) |> list.filter_map(dict.get(data, _))
  all_match_has(state, rec, entries, False)
}

/// ES2024 §24.2.3.12 Set.prototype.values ()
/// Returns a new Set Iterator object (§24.2.5.1 CreateSetIterator).
fn set_values(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, keys, _ref, state <- require_set(this, state)
  let snapshot = list.reverse(keys) |> list.filter_map(dict.get(data, _))
  alloc_set_iterator(state, snapshot, value.SetIterValues)
}

/// ES2024 §24.2.3.5 Set.prototype.entries ()
fn set_entries(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, keys, _ref, state <- require_set(this, state)
  let snapshot = list.reverse(keys) |> list.filter_map(dict.get(data, _))
  alloc_set_iterator(state, snapshot, value.SetIterEntries)
}

/// Allocate a SetIteratorObject wrapping a forward-order snapshot.
fn alloc_set_iterator(
  state: State,
  remaining: List(JsValue),
  kind: value.SetIterKind,
) -> #(State, Result(JsValue, JsValue)) {
  let #(heap, ref) =
    common.alloc_wrapper(
      state.heap,
      value.SetIteratorObject(remaining:, kind:),
      state.builtins.set_iterator_proto,
    )
  #(State(..state, heap:), Ok(JsObject(ref)))
}

/// Allocate a new Set object from data + reversed keys.
fn alloc_new_set(
  state: State,
  data: dict.Dict(MapKey, JsValue),
  keys: List(MapKey),
) -> #(State, Result(JsValue, JsValue)) {
  let #(heap, ref) =
    common.alloc_wrapper(
      state.heap,
      SetObject(data:, keys:),
      state.builtins.set.prototype,
    )
  #(State(..state, heap:), Ok(JsObject(ref)))
}

/// Allocate a new Set from a forward-ordered list of values.
fn alloc_new_set_from_values(
  state: State,
  values: List(JsValue),
) -> #(State, Result(JsValue, JsValue)) {
  let #(data, keys) =
    list.fold(values, #(dict.new(), []), fn(acc, v) {
      let #(d, ks) = acc
      let key = value.js_to_map_key(v)
      case dict.has_key(d, key) {
        True -> #(d, ks)
        False -> #(dict.insert(d, key, v), [key, ..ks])
      }
    })
  alloc_new_set(state, data, keys)
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
  state: State,
  cont: fn(SetRecord, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
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
  state: State,
  rec: SetRecord,
  cont: fn(List(JsValue), State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
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
  state: State,
  iter: JsValue,
  next_fn: JsValue,
  acc: List(JsValue),
  cont: fn(List(JsValue), State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
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
  state: State,
  rec: SetRecord,
  v: JsValue,
  cont: fn(Bool, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  use r, state <- state.try_call(state, rec.has, rec.obj, [v])
  cont(value.is_truthy(r), state)
}

/// Filter `entries` keeping those where ToBoolean(rec.has(e)) == keep_when.
/// State-threaded recursion since each .has() may mutate the heap or throw.
/// CPS — `use kept, state <- with_filtered_by_has(...)`. Result is forward order.
fn with_filtered_by_has(
  state: State,
  rec: SetRecord,
  entries: List(JsValue),
  keep_when: Bool,
  cont: fn(List(JsValue), State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  filter_loop(state, rec, entries, keep_when, [], cont)
}

fn filter_loop(
  state: State,
  rec: SetRecord,
  entries: List(JsValue),
  keep_when: Bool,
  acc: List(JsValue),
  cont: fn(List(JsValue), State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
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
  state: State,
  rec: SetRecord,
  entries: List(JsValue),
  expected: Bool,
) -> #(State, Result(JsValue, JsValue)) {
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
/// CPS-style — call with `use data, keys, ref, state <- require_set(this, state)`.
fn require_set(
  this: JsValue,
  state: State,
  cont: fn(dict.Dict(MapKey, JsValue), List(MapKey), Ref, State) ->
    #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  let err = "Method Set.prototype.* called on incompatible receiver"
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: SetObject(data:, keys:), ..)) ->
          cont(data, keys, ref, state)
        _ -> state.type_error(state, err)
      }
    _ -> state.type_error(state, err)
  }
}
