/// ES2024 §24.3 WeakMap Objects
///
/// A WeakMap is a collection of key-value pairs where keys must be values
/// for which CanBeHeldWeakly is true: objects, or non-registered Symbols.
/// Keys are stored as canonical JsValues — `JsObject(ref)` compares by
/// object identity, `JsSymbol(id)` by symbol identity.
/// Not truly weak (GC doesn't collect entries) but API-compatible.
///
/// Everything WeakMap shares with WeakSet — RequireInternalSlot, the
/// read/mutate discipline, `has`, `delete`, the constructor — lives in
/// `weak_collection`, parameterized by the `kind()` value below. This module
/// keeps only the WeakMap-specific surface: `get`, `set`, `getOrInsert`,
/// `getOrInsertComputed`.
///
/// The key predicate (§9.13 CanBeHeldWeakly) is `helpers.can_be_held_weakly`,
/// shared with `weak_set` and `finalization_registry`.
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers.{
  can_be_held_weakly, first_arg_or_undefined, is_callable, list_at,
}
import arc/vm/builtins/iterator
import arc/vm/builtins/weak_collection.{type WeakKind, WeakKind}
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State}
import arc/vm/value.{
  type JsValue, type Ref, type WeakMapNativeFn, Dispatch, JsUndefined,
  WeakMapConstructor, WeakMapNative, WeakMapObject, WeakMapPrototypeDelete,
  WeakMapPrototypeGet, WeakMapPrototypeGetOrInsert,
  WeakMapPrototypeGetOrInsertComputed, WeakMapPrototypeHas, WeakMapPrototypeSet,
}
import gleam/dict
import gleam/option.{None, Some}

/// The three things that make a WeakMap a WeakMap rather than a WeakSet.
fn kind() -> WeakKind(host, JsValue) {
  WeakKind(
    unwrap: fn(slot_kind) {
      case slot_kind {
        WeakMapObject(data:) -> Some(data)
        _ -> None
      }
    },
    wrap: WeakMapObject,
    type_name: "WeakMap",
  )
}

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
    WeakMapPrototypeHas -> weak_collection.has(kind(), this, args, state)
    WeakMapPrototypeDelete -> weak_collection.delete(kind(), this, args, state)
    WeakMapPrototypeGetOrInsert -> get_or_insert(this, args, state)
    WeakMapPrototypeGetOrInsertComputed ->
      get_or_insert_computed(this, args, state)
  }
}

/// ES2024 §24.3.1.1 WeakMap ( [ iterable ] ) — the shared skeleton, with
/// "set" as the adder and AddEntriesFromIterable as the iteration step.
fn construct(
  proto: Ref,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  weak_collection.construct(
    kind(),
    proto,
    args,
    state,
    "set",
    iterator.add_entries_from_iterable,
  )
}

/// ES2024 §24.3.3.2 WeakMap.prototype.get ( key )
fn weak_map_get(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- weak_collection.require(kind(), this, state, "get")
  let key = first_arg_or_undefined(args)
  case can_be_held_weakly(state, key) {
    True ->
      case dict.get(weak_collection.read_data(kind(), state, ref), key) {
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
  use ref, state <- weak_collection.require(kind(), this, state, "set")
  let key = first_arg_or_undefined(args)
  case can_be_held_weakly(state, key) {
    True -> {
      let val = list_at(args, 1) |> option.unwrap(JsUndefined)
      let state =
        weak_collection.mutate(kind(), state, ref, dict.insert(_, key, val))
      #(state, Ok(this))
    }
    False -> state.type_error(state, "Invalid value used as weak map key")
  }
}

/// Upsert proposal — WeakMap.prototype.getOrInsert ( key, value )
fn get_or_insert(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- weak_collection.require(kind(), this, state, "getOrInsert")
  let key = first_arg_or_undefined(args)
  case can_be_held_weakly(state, key) {
    False -> state.type_error(state, "Invalid value used as weak map key")
    True ->
      case dict.get(weak_collection.read_data(kind(), state, ref), key) {
        Ok(existing) -> #(state, Ok(existing))
        Error(Nil) -> {
          let val = list_at(args, 1) |> option.unwrap(JsUndefined)
          let state =
            weak_collection.mutate(kind(), state, ref, dict.insert(_, key, val))
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
  use ref, state <- weak_collection.require(
    kind(),
    this,
    state,
    "getOrInsertComputed",
  )
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
          case dict.get(weak_collection.read_data(kind(), state, ref), key) {
            Ok(existing) -> #(state, Ok(existing))
            Error(Nil) -> {
              use computed, state <- state.try_call(
                state,
                callback,
                JsUndefined,
                [key],
              )
              // The callback may have mutated the map — `mutate` re-reads the
              // live entry dict, so an entry it inserted under this key is
              // overwritten rather than the whole dict being reverted.
              let state =
                weak_collection.mutate(
                  kind(),
                  state,
                  ref,
                  dict.insert(_, key, computed),
                )
              #(state, Ok(computed))
            }
          }
      }
  }
}
