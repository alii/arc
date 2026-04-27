import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsValue, type Ref, Dispatch, DomExceptionConstructor, DomExceptionGetCode,
  ErrorNative, Finite, JsNumber, JsObject, JsString, JsUndefined, Named,
  ObjectSlot, OrdinaryObject,
}
import gleam/int
import gleam/option.{Some}

/// WebIDL §2.8.1 DOMException — Error-like with a `name` drawn from a fixed
/// table that maps to a legacy integer `code`. Prototype chain goes through
/// Error.prototype so `instanceof Error` holds.
pub fn init(
  h: Heap,
  function_proto: Ref,
  error_proto: Ref,
) -> #(Heap, BuiltinType) {
  let #(h, getters) =
    common.alloc_getters(h, function_proto, [
      #("code", ErrorNative(DomExceptionGetCode)),
    ])
  let #(h, builtin) =
    common.init_type(
      h,
      error_proto,
      function_proto,
      getters,
      fn(proto) { Dispatch(ErrorNative(DomExceptionConstructor(proto:))) },
      "DOMException",
      2,
      [],
    )
  let h = common.add_to_string_tag(h, builtin.prototype, "DOMException")
  #(h, builtin)
}

/// new DOMException(message = "", name = "Error")
/// Always installs own `name` and `message` (writable+configurable, not
/// enumerable) so the prototype `code` getter and Error.prototype.toString
/// resolve via ordinary Get.
pub fn construct(
  proto: Ref,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let #(msg_arg, name_arg) = case args {
    [m, n, ..] -> #(m, n)
    [m] -> #(m, JsUndefined)
    [] -> #(JsUndefined, JsUndefined)
  }
  use message, state <- arg_string(state, msg_arg, "")
  use name, state <- arg_string(state, name_arg, "Error")
  let #(heap, val) = alloc(state.heap, proto, name, message)
  #(State(..state, heap:), Ok(val))
}

fn arg_string(
  state: State,
  arg: JsValue,
  default: String,
  k: fn(String, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case arg {
    JsUndefined -> k(default, state)
    other -> {
      use s, state <- coerce.try_to_string(state, other)
      k(s, state)
    }
  }
}

/// Allocate a DOMException instance with own name+message data properties.
fn alloc(
  h: Heap,
  proto: Ref,
  name: String,
  message: String,
) -> #(Heap, JsValue) {
  let #(h, ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: OrdinaryObject,
        properties: common.named_props([
          #("message", value.builtin_property(JsString(message))),
          #("name", value.builtin_property(JsString(name))),
        ]),
        elements: elements.new(),
        prototype: Some(proto),
        symbol_properties: [],
        extensible: True,
      ),
    )
  #(h, JsObject(ref))
}

/// Allocate a DOMException for engine-internal throws (e.g. structuredClone
/// rejecting an uncloneable value with name "DataCloneError").
pub fn make(
  h: Heap,
  builtins: common.Builtins,
  name: String,
  message: String,
) -> #(Heap, JsValue) {
  alloc(h, builtins.dom_exception.prototype, name, message)
}

/// get DOMException.prototype.code — reads `this.name` and maps it through
/// the WebIDL legacy code table; unknown names yield 0.
pub fn get_code(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) -> {
      use name_val, state <- state.try_op(object.get_value(
        state,
        ref,
        Named("name"),
        this,
      ))
      use name, state <- coerce.try_to_string(state, name_val)
      #(state, Ok(JsNumber(Finite(int.to_float(legacy_code(name))))))
    }
    _ -> #(state, Ok(JsNumber(Finite(0.0))))
  }
}

/// WebIDL DOMException names table — maps `name` to legacy numeric `code`.
fn legacy_code(name: String) -> Int {
  case name {
    "IndexSizeError" -> 1
    "HierarchyRequestError" -> 3
    "WrongDocumentError" -> 4
    "InvalidCharacterError" -> 5
    "NoModificationAllowedError" -> 7
    "NotFoundError" -> 8
    "NotSupportedError" -> 9
    "InUseAttributeError" -> 10
    "InvalidStateError" -> 11
    "SyntaxError" -> 12
    "InvalidModificationError" -> 13
    "NamespaceError" -> 14
    "InvalidAccessError" -> 15
    "TypeMismatchError" -> 17
    "SecurityError" -> 18
    "NetworkError" -> 19
    "AbortError" -> 20
    "URLMismatchError" -> 21
    "QuotaExceededError" -> 22
    "TimeoutError" -> 23
    "InvalidNodeTypeError" -> 24
    "DataCloneError" -> 25
    _ -> 0
  }
}
