/// ES2024 §24.4 WeakSet Objects
///
/// A WeakSet is a collection of objects. Only objects can be values.
/// In this implementation, values are stored by Ref (object identity).
/// Not truly weak (GC doesn't collect entries) but API-compatible.
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/frame.{type State, State}
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/value.{
  type JsValue, type Ref, type WeakSetNativeFn, JsBool, JsObject, JsUndefined,
  ObjectSlot, WeakSetConstructor, WeakSetNative, WeakSetObject,
  WeakSetPrototypeAdd, WeakSetPrototypeDelete, WeakSetPrototypeHas,
}
import gleam/dict
import gleam/option.{Some}

/// Set up WeakSet.prototype and WeakSet constructor.
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, BuiltinType) {
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("add", WeakSetNative(WeakSetPrototypeAdd), 1),
      #("has", WeakSetNative(WeakSetPrototypeHas), 1),
      #("delete", WeakSetNative(WeakSetPrototypeDelete), 1),
    ])

  common.init_type(
    h,
    object_proto,
    function_proto,
    proto_methods,
    fn(proto) { WeakSetNative(WeakSetConstructor(proto:)) },
    "WeakSet",
    0,
    [],
  )
}

/// Per-module dispatch for WeakSet native functions.
pub fn dispatch(
  native: WeakSetNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case native {
    WeakSetConstructor(proto:) -> construct(proto, args, state)
    WeakSetPrototypeAdd -> weak_set_add(this, args, state)
    WeakSetPrototypeHas -> weak_set_has(this, args, state)
    WeakSetPrototypeDelete -> weak_set_delete(this, args, state)
  }
}

/// ES2024 §24.4.1.1 WeakSet ( [ iterable ] )
fn construct(
  proto: Ref,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // For now, ignore iterable argument
  let #(heap, ref) =
    heap.alloc(
      state.heap,
      ObjectSlot(
        kind: WeakSetObject(data: dict.new()),
        properties: dict.new(),
        elements: js_elements.new(),
        prototype: Some(proto),
        symbol_properties: dict.new(),
        extensible: True,
      ),
    )
  #(State(..state, heap:), Ok(JsObject(ref)))
}

/// ES2024 §24.4.3.1 WeakSet.prototype.add ( value )
fn weak_set_add(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let val = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: WeakSetObject(data:), ..)) ->
          case val {
            JsObject(val_ref) -> {
              let new_data = dict.insert(data, val_ref, True)
              let heap = update_weak_set(state.heap, ref, new_data)
              #(State(..state, heap:), Ok(this))
            }
            _ -> frame.type_error(state, "Invalid value used in weak set")
          }
        _ ->
          frame.type_error(
            state,
            "WeakSet.prototype.add requires that 'this' be a WeakSet",
          )
      }
    _ ->
      frame.type_error(
        state,
        "WeakSet.prototype.add requires that 'this' be a WeakSet",
      )
  }
}

/// ES2024 §24.4.3.3 WeakSet.prototype.has ( value )
fn weak_set_has(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let val = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: WeakSetObject(data:), ..)) ->
          case val {
            JsObject(val_ref) -> #(
              state,
              Ok(JsBool(dict.has_key(data, val_ref))),
            )
            _ -> #(state, Ok(JsBool(False)))
          }
        _ ->
          frame.type_error(
            state,
            "WeakSet.prototype.has requires that 'this' be a WeakSet",
          )
      }
    _ ->
      frame.type_error(
        state,
        "WeakSet.prototype.has requires that 'this' be a WeakSet",
      )
  }
}

/// ES2024 §24.4.3.2 WeakSet.prototype.delete ( value )
fn weak_set_delete(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let val = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: WeakSetObject(data:), ..)) ->
          case val {
            JsObject(val_ref) -> {
              let had = dict.has_key(data, val_ref)
              let new_data = dict.delete(data, val_ref)
              let heap = update_weak_set(state.heap, ref, new_data)
              #(State(..state, heap:), Ok(JsBool(had)))
            }
            _ -> #(state, Ok(JsBool(False)))
          }
        _ ->
          frame.type_error(
            state,
            "WeakSet.prototype.delete requires that 'this' be a WeakSet",
          )
      }
    _ ->
      frame.type_error(
        state,
        "WeakSet.prototype.delete requires that 'this' be a WeakSet",
      )
  }
}

/// Helper to update a WeakSetObject's data on the heap.
fn update_weak_set(h: Heap, ref: Ref, data: dict.Dict(Ref, Bool)) -> Heap {
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
          kind: WeakSetObject(data:),
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
