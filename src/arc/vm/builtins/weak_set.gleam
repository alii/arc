/// ES2024 §24.4 WeakSet Objects
///
/// A WeakSet is a collection of values for which CanBeHeldWeakly is true:
/// objects, or non-registered Symbols. Members are stored as canonical
/// JsValues — `JsObject(ref)` compares by object identity, `JsSymbol(id)`
/// by symbol identity — exactly like WeakMap keys.
/// Not truly weak (GC doesn't collect entries) but API-compatible.
///
/// Everything WeakSet shares with WeakMap — RequireInternalSlot, the
/// read/mutate discipline, `has`, `delete`, the constructor — lives in
/// `weak_collection`, parameterized by the `kind()` value below. This module
/// keeps only the WeakSet-specific surface: `add`.
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers.{first_arg_or_undefined}
import arc/vm/builtins/iter_protocol
import arc/vm/builtins/weak_collection.{type WeakKind, WeakKind}
import arc/vm/state.{type Heap, type State}
import arc/vm/value.{
  type JsValue, type Ref, type WeakSetNativeFn, Dispatch, WeakSetConstructor,
  WeakSetNative, WeakSetObject, WeakSetPrototypeAdd, WeakSetPrototypeDelete,
  WeakSetPrototypeHas,
}
import gleam/option.{None, Some}

/// The three things that make a WeakSet a WeakSet rather than a WeakMap.
fn kind() -> WeakKind(host, Nil) {
  WeakKind(
    unwrap: fn(slot_kind) {
      case slot_kind {
        WeakSetObject(data:) -> Some(data)
        _ -> None
      }
    },
    wrap: WeakSetObject,
    type_name: "WeakSet",
    invalid_key_message: "Invalid value used in weak set",
  )
}

/// Set up WeakSet.prototype and WeakSet constructor.
pub fn init(
  h: Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(host), BuiltinType) {
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
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case native {
    WeakSetConstructor(proto:) -> construct(proto, args, state)
    WeakSetPrototypeAdd -> weak_set_add(this, args, state)
    WeakSetPrototypeHas -> weak_collection.has(kind(), this, args, state)
    WeakSetPrototypeDelete -> weak_collection.delete(kind(), this, args, state)
  }
}

/// ES2024 §24.4.1.1 WeakSet ( [ iterable ] ) — the shared skeleton, with
/// "add" as the adder and value-iteration as the iteration step.
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
    "add",
    iter_protocol.add_values_from_iterable,
  )
}

/// ES2024 §24.4.3.1 WeakSet.prototype.add ( value )
/// Same §16.1 CanBeHeldWeakly gate as WeakMap keys: objects and
/// non-registered Symbols are valid members, everything else is a TypeError.
fn weak_set_add(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- weak_collection.require(kind(), this, state, "add")
  let val = first_arg_or_undefined(args)
  use val, state <- weak_collection.require_weak_key(ref, state, val)
  #(weak_collection.insert(state, ref, val, Nil), Ok(this))
}
