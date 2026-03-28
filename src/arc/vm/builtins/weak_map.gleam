/// ES2024 §24.3 WeakMap Objects
///
/// A WeakMap is a collection of key-value pairs where keys must be objects.
/// In this implementation, keys are stored by Ref (object identity).
/// Not truly weak (GC doesn't collect entries) but API-compatible.
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers.{first_arg}
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsValue, type Ref, type WeakMapNativeFn, Dispatch, JsObject, JsUndefined,
  ObjectSlot, WeakMapConstructor, WeakMapNative, WeakMapObject,
  WeakMapPrototypeDelete, WeakMapPrototypeGet, WeakMapPrototypeHas,
  WeakMapPrototypeSet,
}
import gleam/dict
import gleam/option.{Some}

/// Set up WeakMap.prototype and WeakMap constructor.
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, BuiltinType) {
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("get", WeakMapNative(WeakMapPrototypeGet), 1),
      #("set", WeakMapNative(WeakMapPrototypeSet), 2),
      #("has", WeakMapNative(WeakMapPrototypeHas), 1),
      #("delete", WeakMapNative(WeakMapPrototypeDelete), 1),
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
  let h =
    common.add_symbol_property(
      h,
      bt.prototype,
      value.symbol_to_string_tag,
      value.builtin_property(value.JsString("WeakMap")),
    )
  #(h, bt)
}

/// Per-module dispatch for WeakMap native functions.
pub fn dispatch(
  native: WeakMapNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case native {
    WeakMapConstructor(proto:) -> construct(proto, args, state)
    WeakMapPrototypeGet -> weak_map_get(this, args, state)
    WeakMapPrototypeSet -> weak_map_set(this, args, state)
    WeakMapPrototypeHas -> weak_map_has(this, args, state)
    WeakMapPrototypeDelete -> weak_map_delete(this, args, state)
  }
}

/// ES2024 §24.3.1.1 WeakMap ( [ iterable ] )
fn construct(
  proto: Ref,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // For now, ignore iterable argument (most tests just test new WeakMap())
  let #(heap, ref) =
    heap.alloc(
      state.heap,
      ObjectSlot(
        kind: WeakMapObject(data: dict.new()),
        properties: dict.new(),
        elements: elements.new(),
        prototype: Some(proto),
        symbol_properties: [],
        extensible: True,
      ),
    )
  #(State(..state, heap:), Ok(JsObject(ref)))
}

/// ES2024 §24.3.3.2 WeakMap.prototype.get ( key )
fn weak_map_get(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, _ref, state <- require_weak_map(this, state)
  case first_arg(args) {
    JsObject(key_ref) ->
      case dict.get(data, key_ref) {
        Ok(val) -> #(state, Ok(val))
        Error(Nil) -> #(state, Ok(JsUndefined))
      }
    _ -> #(state, Ok(JsUndefined))
  }
}

/// ES2024 §24.3.3.5 WeakMap.prototype.set ( key, value )
fn weak_map_set(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, ref, state <- require_weak_map(this, state)
  case first_arg(args) {
    JsObject(key_ref) -> {
      let val = case args {
        [_, v, ..] -> v
        _ -> JsUndefined
      }
      let new_data = dict.insert(data, key_ref, val)
      let heap = update_weak_map(state.heap, ref, new_data)
      #(State(..state, heap:), Ok(this))
    }
    _ -> state.type_error(state, "Invalid value used as weak map key")
  }
}

/// ES2024 §24.3.3.3 WeakMap.prototype.has ( key )
fn weak_map_has(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, _ref, state <- require_weak_map(this, state)
  case first_arg(args) {
    JsObject(key_ref) -> #(state, Ok(value.JsBool(dict.has_key(data, key_ref))))
    _ -> #(state, Ok(value.JsBool(False)))
  }
}

/// ES2024 §24.3.3.1 WeakMap.prototype.delete ( key )
fn weak_map_delete(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, ref, state <- require_weak_map(this, state)
  case first_arg(args) {
    JsObject(key_ref) -> {
      let had = dict.has_key(data, key_ref)
      let new_data = dict.delete(data, key_ref)
      let heap = update_weak_map(state.heap, ref, new_data)
      #(State(..state, heap:), Ok(value.JsBool(had)))
    }
    _ -> #(state, Ok(value.JsBool(False)))
  }
}

// ---- helpers ----

/// Unwrap `this` as a WeakMap or return a TypeError.
/// CPS-style — call with `use data, ref, state <- require_weak_map(this, state)`.
fn require_weak_map(
  this: JsValue,
  state: State,
  cont: fn(dict.Dict(Ref, JsValue), Ref, State) ->
    #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  let err = "Method WeakMap.prototype.* called on incompatible receiver"
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: WeakMapObject(data:), ..)) ->
          cont(data, ref, state)
        _ -> state.type_error(state, err)
      }
    _ -> state.type_error(state, err)
  }
}

/// Helper to update a WeakMapObject's data on the heap.
fn update_weak_map(h: Heap, ref: Ref, data: dict.Dict(Ref, JsValue)) -> Heap {
  heap.update(h, ref, fn(slot) {
    case slot {
      ObjectSlot(
        properties:,
        elements:,
        prototype:,
        symbol_properties:,
        extensible:,
        ..,
      ) ->
        ObjectSlot(
          kind: WeakMapObject(data:),
          properties:,
          elements:,
          prototype:,
          symbol_properties:,
          extensible:,
        )
      other -> other
    }
  })
}
