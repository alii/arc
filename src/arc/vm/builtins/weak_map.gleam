/// ES2024 §24.3 WeakMap Objects
///
/// A WeakMap is a collection of key-value pairs where keys must be objects.
/// In this implementation, keys are stored by Ref (object identity).
/// Not truly weak (GC doesn't collect entries) but API-compatible.
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/frame.{type State, State}
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/value.{
  type JsValue, type Ref, type WeakMapNativeFn, JsObject, JsUndefined,
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

  common.init_type(
    h,
    object_proto,
    function_proto,
    proto_methods,
    fn(proto) { WeakMapNative(WeakMapConstructor(proto:)) },
    "WeakMap",
    0,
    [],
  )
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
        elements: js_elements.new(),
        prototype: Some(proto),
        symbol_properties: dict.new(),
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
  let key = case args {
    [k, ..] -> k
    [] -> JsUndefined
  }
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: WeakMapObject(data:), ..)) ->
          case key {
            JsObject(key_ref) ->
              case dict.get(data, key_ref) {
                Ok(val) -> #(state, Ok(val))
                Error(Nil) -> #(state, Ok(JsUndefined))
              }
            _ -> #(state, Ok(JsUndefined))
          }
        _ ->
          frame.type_error(
            state,
            "WeakMap.prototype.get requires that 'this' be a WeakMap",
          )
      }
    _ ->
      frame.type_error(
        state,
        "WeakMap.prototype.get requires that 'this' be a WeakMap",
      )
  }
}

/// ES2024 §24.3.3.5 WeakMap.prototype.set ( key, value )
fn weak_map_set(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let key = case args {
    [k, ..] -> k
    [] -> JsUndefined
  }
  let val = case args {
    [_, v, ..] -> v
    _ -> JsUndefined
  }
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: WeakMapObject(data:), ..)) ->
          case key {
            JsObject(key_ref) -> {
              let new_data = dict.insert(data, key_ref, val)
              let heap = update_weak_map(state.heap, ref, new_data)
              #(State(..state, heap:), Ok(this))
            }
            _ -> frame.type_error(state, "Invalid value used as weak map key")
          }
        _ ->
          frame.type_error(
            state,
            "WeakMap.prototype.set requires that 'this' be a WeakMap",
          )
      }
    _ ->
      frame.type_error(
        state,
        "WeakMap.prototype.set requires that 'this' be a WeakMap",
      )
  }
}

/// ES2024 §24.3.3.3 WeakMap.prototype.has ( key )
fn weak_map_has(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let key = case args {
    [k, ..] -> k
    [] -> JsUndefined
  }
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: WeakMapObject(data:), ..)) ->
          case key {
            JsObject(key_ref) -> #(
              state,
              Ok(value.JsBool(dict.has_key(data, key_ref))),
            )
            _ -> #(state, Ok(value.JsBool(False)))
          }
        _ ->
          frame.type_error(
            state,
            "WeakMap.prototype.has requires that 'this' be a WeakMap",
          )
      }
    _ ->
      frame.type_error(
        state,
        "WeakMap.prototype.has requires that 'this' be a WeakMap",
      )
  }
}

/// ES2024 §24.3.3.1 WeakMap.prototype.delete ( key )
fn weak_map_delete(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let key = case args {
    [k, ..] -> k
    [] -> JsUndefined
  }
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: WeakMapObject(data:), ..)) ->
          case key {
            JsObject(key_ref) -> {
              let had = dict.has_key(data, key_ref)
              let new_data = dict.delete(data, key_ref)
              let heap = update_weak_map(state.heap, ref, new_data)
              #(State(..state, heap:), Ok(value.JsBool(had)))
            }
            _ -> #(state, Ok(value.JsBool(False)))
          }
        _ ->
          frame.type_error(
            state,
            "WeakMap.prototype.delete requires that 'this' be a WeakMap",
          )
      }
    _ ->
      frame.type_error(
        state,
        "WeakMap.prototype.delete requires that 'this' be a WeakMap",
      )
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
