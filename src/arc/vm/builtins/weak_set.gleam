/// ES2024 §24.4 WeakSet Objects
///
/// A WeakSet is a collection of objects and/or non-registered Symbols
/// (values for which CanBeHeldWeakly is true).
/// In this implementation, values are stored by Ref (object identity).
/// Not truly weak (GC doesn't collect entries) but API-compatible.
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers.{first_arg_or_undefined}
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsValue, type Ref, type WeakSetNativeFn, Dispatch, JsBool, JsObject,
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

  let #(h, bt) =
    common.init_type(
      h,
      object_proto,
      function_proto,
      proto_methods,
      fn(proto) { Dispatch(WeakSetNative(WeakSetConstructor(proto:))) },
      "WeakSet",
      0,
      [],
    )
  let h = common.add_to_string_tag(h, bt.prototype, "WeakSet")
  #(h, bt)
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
        elements: elements.new(),
        prototype: Some(proto),
        symbol_properties: [],
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
  use data, ref, state <- require_weak_set(this, state)
  case first_arg_or_undefined(args) {
    JsObject(val_ref) -> {
      let new_data = dict.insert(data, val_ref, True)
      let heap = update_weak_set(state.heap, ref, new_data)
      #(State(..state, heap:), Ok(this))
    }
    _ -> state.type_error(state, "Invalid value used in weak set")
  }
}

/// ES2024 §24.4.3.3 WeakSet.prototype.has ( value )
fn weak_set_has(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, _ref, state <- require_weak_set(this, state)
  case first_arg_or_undefined(args) {
    JsObject(val_ref) -> #(state, Ok(JsBool(dict.has_key(data, val_ref))))
    _ -> #(state, Ok(JsBool(False)))
  }
}

/// ES2024 §24.4.3.2 WeakSet.prototype.delete ( value )
fn weak_set_delete(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, ref, state <- require_weak_set(this, state)
  case first_arg_or_undefined(args) {
    JsObject(val_ref) -> {
      let had = dict.has_key(data, val_ref)
      let new_data = dict.delete(data, val_ref)
      let heap = update_weak_set(state.heap, ref, new_data)
      #(State(..state, heap:), Ok(JsBool(had)))
    }
    _ -> #(state, Ok(JsBool(False)))
  }
}

// ---- helpers ----

/// Unwrap `this` as a WeakSet or return a TypeError.
/// CPS-style — call with `use data, ref, state <- require_weak_set(this, state)`.
fn require_weak_set(
  this: JsValue,
  state: State,
  cont: fn(dict.Dict(Ref, Bool), Ref, State) ->
    #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  let err = "Method WeakSet.prototype.* called on incompatible receiver"
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: WeakSetObject(data:), ..)) ->
          cont(data, ref, state)
        _ -> state.type_error(state, err)
      }
    _ -> state.type_error(state, err)
  }
}

/// Helper to update a WeakSetObject's data on the heap.
fn update_weak_set(h: Heap, ref: Ref, data: dict.Dict(Ref, Bool)) -> Heap {
  heap.update_kind(h, ref, WeakSetObject(data:))
}
