import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/frame.{type State, State}
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/value.{
  type JsValue, type Ref, JsObject, JsString, JsUndefined,
  NativeErrorConstructor, ObjectSlot, OrdinaryObject,
}
import gleam/dict
import gleam/option.{Some}

/// All error-related builtin types.
pub type ErrorBuiltins {
  ErrorBuiltins(
    error: BuiltinType,
    type_error: BuiltinType,
    reference_error: BuiltinType,
    range_error: BuiltinType,
    syntax_error: BuiltinType,
  )
}

/// Set up all error prototypes and constructors as NativeFunctions.
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, ErrorBuiltins) {
  // Error — base error type with name + message on prototype
  let #(h, error) =
    common.init_type(
      h,
      object_proto,
      function_proto,
      [
        #("name", value.builtin_property(JsString("Error"))),
        #("message", value.builtin_property(JsString(""))),
      ],
      fn(proto) { NativeErrorConstructor(proto:) },
      "Error",
      1,
      [],
    )

  // Error subclasses — each inherits from Error.prototype
  let #(h, type_error) =
    common.init_type(
      h,
      error.prototype,
      function_proto,
      [#("name", value.builtin_property(JsString("TypeError")))],
      fn(proto) { NativeErrorConstructor(proto:) },
      "TypeError",
      1,
      [],
    )
  let #(h, reference_error) =
    common.init_type(
      h,
      error.prototype,
      function_proto,
      [#("name", value.builtin_property(JsString("ReferenceError")))],
      fn(proto) { NativeErrorConstructor(proto:) },
      "ReferenceError",
      1,
      [],
    )
  let #(h, range_error) =
    common.init_type(
      h,
      error.prototype,
      function_proto,
      [#("name", value.builtin_property(JsString("RangeError")))],
      fn(proto) { NativeErrorConstructor(proto:) },
      "RangeError",
      1,
      [],
    )
  let #(h, syntax_error) =
    common.init_type(
      h,
      error.prototype,
      function_proto,
      [#("name", value.builtin_property(JsString("SyntaxError")))],
      fn(proto) { NativeErrorConstructor(proto:) },
      "SyntaxError",
      1,
      [],
    )

  #(
    h,
    ErrorBuiltins(
      error:,
      type_error:,
      reference_error:,
      range_error:,
      syntax_error:,
    ),
  )
}

/// Native error constructor: if (message !== undefined) this.message = message
/// Creates a new error object with the proto embedded in the NativeFn.
/// Per §20.5.6.3: "message" is writable+configurable but NOT enumerable.
pub fn call_native(
  proto: Ref,
  args: List(JsValue),
  _this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case args {
    [JsUndefined, ..] | [] -> alloc_error(state, proto, dict.new())
    [JsString(msg), ..] ->
      alloc_error(
        state,
        proto,
        dict.from_list([#("message", value.builtin_property(JsString(msg)))]),
      )
    [other, ..] ->
      case frame.to_string(state, other) {
        Ok(#(msg, state)) ->
          alloc_error(
            state,
            proto,
            dict.from_list([#("message", value.builtin_property(JsString(msg)))]),
          )
        Error(#(thrown, state)) -> #(state, Error(thrown))
      }
  }
}

fn alloc_error(
  state: State,
  proto: Ref,
  props: dict.Dict(String, value.Property),
) -> #(State, Result(JsValue, JsValue)) {
  let #(heap, ref) =
    heap.alloc(
      state.heap,
      ObjectSlot(
        kind: OrdinaryObject,
        properties: props,
        elements: js_elements.new(),
        prototype: Some(proto),
        symbol_properties: dict.new(),
        extensible: True,
      ),
    )
  #(State(..state, heap:), Ok(JsObject(ref)))
}
