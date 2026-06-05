/// ES2024 §24.4 WeakSet Objects
///
/// A WeakSet is a collection of objects and/or non-registered Symbols
/// (values for which CanBeHeldWeakly is true).
/// In this implementation, values are stored by Ref (object identity).
/// Not truly weak (GC doesn't collect entries) but API-compatible.
///
/// All operations delegate to the shared weak-collection core in `weak_map`
/// (a WeakSet is a WeakMap whose stored values are all `True`).
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/weak_map.{WeakKind}
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsValue, type Ref, type WeakSetNativeFn, Dispatch, JsObject,
  WeakSetConstructor, WeakSetNative, WeakSetObject, WeakSetPrototypeAdd,
  WeakSetPrototypeDelete, WeakSetPrototypeHas,
}
import gleam/dict
import gleam/option.{None, Some}

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
    WeakSetPrototypeAdd ->
      weak_map.weak_insert(this, args, True, state, set_kind())
    WeakSetPrototypeHas -> weak_map.weak_has(this, args, state, set_kind())
    WeakSetPrototypeDelete ->
      weak_map.weak_delete(this, args, state, set_kind())
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
    common.alloc_wrapper(state.heap, WeakSetObject(data: dict.new()), proto)
  #(State(..state, heap:), Ok(JsObject(ref)))
}

/// The WeakSet instantiation of the shared weak-collection core
/// (membership dict: present Refs map to True).
fn set_kind() -> weak_map.WeakKind(Bool) {
  WeakKind(
    receiver_err: "Method WeakSet.prototype.* called on incompatible receiver",
    key_err: "Invalid value used in weak set",
    extract: fn(kind) {
      case kind {
        WeakSetObject(data:) -> Some(data)
        _ -> None
      }
    },
    rebuild: WeakSetObject,
  )
}
