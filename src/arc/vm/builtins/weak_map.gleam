/// ES2024 §24.3 WeakMap Objects
///
/// A WeakMap is a collection of key-value pairs where keys must be values
/// for which CanBeHeldWeakly is true: objects, or non-registered Symbols.
/// Keys are stored as canonical JsValues — `JsObject(ref)` compares by
/// object identity, `JsSymbol(id)` by symbol identity.
/// Not truly weak (GC doesn't collect entries) but API-compatible.
///
/// The key/member predicate (§9.13 CanBeHeldWeakly) is
/// `helpers.can_be_held_weakly`, shared with `weak_set` and
/// `finalization_registry`.
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers.{
  can_be_held_weakly, first_arg_or_undefined, is_callable, list_at,
}
import arc/vm/builtins/iterator
import arc/vm/heap
import arc/vm/key.{Named}
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsValue, type Ref, type WeakMapNativeFn, Dispatch, JsBool, JsNull,
  JsObject, JsUndefined, ObjectSlot, WeakMapConstructor, WeakMapNative,
  WeakMapObject, WeakMapPrototypeDelete, WeakMapPrototypeGet,
  WeakMapPrototypeGetOrInsert, WeakMapPrototypeGetOrInsertComputed,
  WeakMapPrototypeHas, WeakMapPrototypeSet,
}
import gleam/dict.{type Dict}
import gleam/option.{None, Some}

/// Set up WeakMap.prototype and WeakMap constructor.
pub fn init(
  h: Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(host), BuiltinType) {
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("get", WeakMapNative(WeakMapPrototypeGet), 1),
      #("set", WeakMapNative(WeakMapPrototypeSet), 2),
      #("has", WeakMapNative(WeakMapPrototypeHas), 1),
      #("delete", WeakMapNative(WeakMapPrototypeDelete), 1),
      #("getOrInsert", WeakMapNative(WeakMapPrototypeGetOrInsert), 2),
      #(
        "getOrInsertComputed",
        WeakMapNative(WeakMapPrototypeGetOrInsertComputed),
        2,
      ),
    ])

  let #(h, bt) =
    common.init_type(
      h,
      object_proto,
      function_proto,
      proto_methods,
      fn(proto) { Dispatch(WeakMapNative(WeakMapConstructor(proto:))) },
      "WeakMap",
      0,
      [],
    )
  let h = common.add_to_string_tag(h, bt.prototype, "WeakMap")
  #(h, bt)
}

/// Per-module dispatch for WeakMap native functions.
pub fn dispatch(
  native: WeakMapNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case native {
    WeakMapConstructor(proto:) -> construct(proto, args, state)
    WeakMapPrototypeGet -> weak_map_get(this, args, state)
    WeakMapPrototypeSet -> weak_map_set(this, args, state)
    WeakMapPrototypeHas -> weak_map_has(this, args, state)
    WeakMapPrototypeDelete -> weak_map_delete(this, args, state)
    WeakMapPrototypeGetOrInsert -> get_or_insert(this, args, state)
    WeakMapPrototypeGetOrInsertComputed ->
      get_or_insert_computed(this, args, state)
  }
}

/// Unwrap `this` as a WeakMap, or throw TypeError. CPS-style — call with
/// `use data, ref, state <- map_require(this, state)`.
fn map_require(
  this: JsValue,
  state: State(host),
  cont: fn(Dict(JsValue, JsValue), Ref, State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  let found = case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: WeakMapObject(data:), ..)) -> Some(#(data, ref))
        _ -> None
      }
    _ -> None
  }
  case found {
    Some(#(data, ref)) -> cont(data, ref, state)
    None ->
      state.type_error(
        state,
        "Method WeakMap.prototype called on incompatible receiver",
      )
  }
}

/// ES2024 §24.3.1.1 WeakMap ( [ iterable ] )
///
///   1. If NewTarget is undefined, throw a TypeError exception.
///   2. Let map be ? OrdinaryCreateFromConstructor(NewTarget,
///      "%WeakMap.prototype%", « [[WeakMapData]] »).
///   3. Set map.[[WeakMapData]] to a new empty List.
///   4. If iterable is undefined or null, return map.
///   5-6. adder = ? Get(map, "set"); must be callable.
///   7. Return ? AddEntriesFromIterable(map, iterable, adder).
fn construct(
  proto: Ref,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Steps 1-2: reject a plain call and resolve new.target.prototype.
  use proto, state <- helpers.require_new_target(state, "WeakMap", proto)
  let #(heap, map_ref) =
    common.alloc_wrapper(state.heap, WeakMapObject(data: dict.new()), proto)
  let state = State(..state, heap:)
  let map = JsObject(map_ref)
  case first_arg_or_undefined(args) {
    // Step 4: If iterable is undefined or null, return map.
    JsUndefined | JsNull -> #(state, Ok(map))
    iterable -> {
      // Steps 5-6: adder = ? Get(map, "set"); must be callable.
      use adder, state <- state.try_op(object.get_value_of(
        state,
        map,
        Named("set"),
      ))
      case is_callable(state.heap, adder) {
        False ->
          state.type_error(state, "'set' property of WeakMap is not a function")
        // Step 7: ? AddEntriesFromIterable(map, iterable, adder).
        True -> iterator.add_entries_from_iterable(state, map, iterable, adder)
      }
    }
  }
}

/// ES2024 §24.3.3.2 WeakMap.prototype.get ( key )
fn weak_map_get(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use data, _ref, state <- map_require(this, state)
  let key = first_arg_or_undefined(args)
  case can_be_held_weakly(state, key) {
    True ->
      case dict.get(data, key) {
        Ok(val) -> #(state, Ok(val))
        Error(Nil) -> #(state, Ok(JsUndefined))
      }
    False -> #(state, Ok(JsUndefined))
  }
}

/// ES2024 §24.3.3.5 WeakMap.prototype.set ( key, value )
fn weak_map_set(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use data, ref, state <- map_require(this, state)
  let key = first_arg_or_undefined(args)
  case can_be_held_weakly(state, key) {
    True -> {
      let val = list_at(args, 1) |> option.unwrap(JsUndefined)
      let state = write_data(state, ref, dict.insert(data, key, val))
      #(state, Ok(this))
    }
    False -> state.type_error(state, "Invalid value used as weak map key")
  }
}

/// ES2024 §24.3.3.4 WeakMap.prototype.has ( key )
fn weak_map_has(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use data, _ref, state <- map_require(this, state)
  let key = first_arg_or_undefined(args)
  #(state, Ok(JsBool(dict.has_key(data, key))))
}

/// ES2024 §24.3.3.3 WeakMap.prototype.delete ( key )
fn weak_map_delete(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use data, ref, state <- map_require(this, state)
  let key = first_arg_or_undefined(args)
  case dict.has_key(data, key) {
    True -> {
      let state = write_data(state, ref, dict.delete(data, key))
      #(state, Ok(JsBool(True)))
    }
    False -> #(state, Ok(JsBool(False)))
  }
}

/// Upsert proposal — WeakMap.prototype.getOrInsert ( key, value )
fn get_or_insert(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use data, ref, state <- map_require(this, state)
  let key = first_arg_or_undefined(args)
  case can_be_held_weakly(state, key) {
    False -> state.type_error(state, "Invalid value used as weak map key")
    True ->
      case dict.get(data, key) {
        Ok(existing) -> #(state, Ok(existing))
        Error(Nil) -> {
          let val = list_at(args, 1) |> option.unwrap(JsUndefined)
          let state = write_data(state, ref, dict.insert(data, key, val))
          #(state, Ok(val))
        }
      }
  }
}

/// Upsert proposal — WeakMap.prototype.getOrInsertComputed ( key, callbackfn )
/// Validation order per spec: receiver, then IsCallable(callbackfn), then
/// CanBeHeldWeakly(key). After the callback runs the map is re-read so an
/// insert made by the callback for the same key is overwritten.
fn get_or_insert_computed(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use data, _ref, state <- map_require(this, state)
  let key = first_arg_or_undefined(args)
  let callback = list_at(args, 1) |> option.unwrap(JsUndefined)
  case is_callable(state.heap, callback) {
    False ->
      state.type_error(
        state,
        object.inspect(callback, state.heap) <> " is not a function",
      )
    True ->
      case can_be_held_weakly(state, key) {
        False -> state.type_error(state, "Invalid value used as weak map key")
        True ->
          case dict.get(data, key) {
            Ok(existing) -> #(state, Ok(existing))
            Error(Nil) -> {
              use computed, state <- state.try_call(
                state,
                callback,
                JsUndefined,
                [key],
              )
              // The callback may have mutated the map — re-read and
              // overwrite any entry it inserted under this key.
              use data, ref, state <- map_require(this, state)
              let state =
                write_data(state, ref, dict.insert(data, key, computed))
              #(state, Ok(computed))
            }
          }
      }
  }
}

/// Write an updated entry dict back to the WeakMap's heap slot.
fn write_data(
  state: State(host),
  ref: Ref,
  data: Dict(JsValue, JsValue),
) -> State(host) {
  let heap = heap.update_kind(state.heap, ref, WeakMapObject(data:))
  State(..state, heap:)
}
