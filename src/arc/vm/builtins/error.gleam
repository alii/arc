import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/frame.{type State, State}
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/object
import arc/vm/value.{
  type ErrorNativeFn, type JsValue, type Ref, ErrorConstructor, ErrorNative,
  JsNull, JsObject, JsString, JsUndefined, ObjectSlot, OrdinaryObject,
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
    eval_error: BuiltinType,
    uri_error: BuiltinType,
    aggregate_error: BuiltinType,
  )
}

/// Set up all error prototypes and constructors as NativeFunctions.
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, ErrorBuiltins) {
  // Allocate Error.prototype.toString method
  let #(h, to_string_methods) =
    common.alloc_methods(h, function_proto, [
      #("toString", ErrorNative(value.ErrorPrototypeToString), 0),
    ])

  // Error — base error type with name + message on prototype
  let #(h, error) =
    common.init_type(
      h,
      object_proto,
      function_proto,
      [
        #("name", value.builtin_property(JsString("Error"))),
        #("message", value.builtin_property(JsString(""))),
        ..to_string_methods
      ],
      fn(proto) { ErrorNative(ErrorConstructor(proto:)) },
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
      fn(proto) { ErrorNative(ErrorConstructor(proto:)) },
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
      fn(proto) { ErrorNative(ErrorConstructor(proto:)) },
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
      fn(proto) { ErrorNative(ErrorConstructor(proto:)) },
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
      fn(proto) { ErrorNative(ErrorConstructor(proto:)) },
      "SyntaxError",
      1,
      [],
    )

  let #(h, eval_error) =
    common.init_type(
      h,
      error.prototype,
      function_proto,
      [#("name", value.builtin_property(JsString("EvalError")))],
      fn(proto) { ErrorNative(ErrorConstructor(proto:)) },
      "EvalError",
      1,
      [],
    )
  let #(h, uri_error) =
    common.init_type(
      h,
      error.prototype,
      function_proto,
      [#("name", value.builtin_property(JsString("URIError")))],
      fn(proto) { ErrorNative(ErrorConstructor(proto:)) },
      "URIError",
      1,
      [],
    )
  let #(h, aggregate_error) =
    common.init_type(
      h,
      error.prototype,
      function_proto,
      [#("name", value.builtin_property(JsString("AggregateError")))],
      fn(proto) { ErrorNative(ErrorConstructor(proto:)) },
      "AggregateError",
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
      eval_error:,
      uri_error:,
      aggregate_error:,
    ),
  )
}

/// Per-module dispatch for Error native functions.
pub fn dispatch(
  native: ErrorNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case native {
    ErrorConstructor(proto:) -> call_native(proto, args, JsUndefined, state)
    value.ErrorPrototypeToString -> error_to_string(this, state)
  }
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

/// Error.prototype.toString ( ) — ES2024 §20.5.3.4
///
///   1. Let O be the this value.
///   2. If O is not an Object, throw a TypeError.
///   3. Let name be ? Get(O, "name").
///   4. If name is undefined, set name to "Error".
///   5. Else set name to ? ToString(name).
///   6. Let msg be ? Get(O, "message").
///   7. If msg is undefined, set msg to "".
///   8. Else set msg to ? ToString(msg).
///   9. If name is the empty String, return msg.
///  10. If msg is the empty String, return name.
///  11. Return name + ": " + msg.
///
fn error_to_string(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case this {
    JsNull | JsUndefined ->
      frame.type_error(state, "Error.prototype.toString called on non-object")
    JsObject(ref) -> {
      // Step 3: Let name be ? Get(O, "name").
      case object.get_value(state, ref, "name", this) {
        Error(#(thrown, state)) -> #(state, Error(thrown))
        Ok(#(name_val, state)) -> {
          // Steps 4-5: If undefined → "Error", else ToString(name).
          case name_val {
            JsUndefined -> error_to_string_msg(state, ref, this, "Error")
            _ ->
              case frame.to_string(state, name_val) {
                Error(#(thrown, state)) -> #(state, Error(thrown))
                Ok(#(name, state)) ->
                  error_to_string_msg(state, ref, this, name)
              }
          }
        }
      }
    }
    // Step 2: Non-object this → TypeError.
    _ ->
      frame.type_error(state, "Error.prototype.toString called on non-object")
  }
}

/// Helper: get "message" and produce the final toString string.
fn error_to_string_msg(
  state: State,
  ref: Ref,
  this: JsValue,
  name: String,
) -> #(State, Result(JsValue, JsValue)) {
  // Step 6: Let msg be ? Get(O, "message").
  case object.get_value(state, ref, "message", this) {
    Error(#(thrown, state)) -> #(state, Error(thrown))
    Ok(#(msg_val, state)) -> {
      // Steps 7-8: If undefined → "", else ToString(msg).
      case msg_val {
        JsUndefined -> error_to_string_combine(state, name, "")
        _ ->
          case frame.to_string(state, msg_val) {
            Error(#(thrown, state)) -> #(state, Error(thrown))
            Ok(#(msg, state)) -> error_to_string_combine(state, name, msg)
          }
      }
    }
  }
}

/// Helper: combine name and msg per §20.5.3.4 steps 9-11.
fn error_to_string_combine(
  state: State,
  name: String,
  msg: String,
) -> #(State, Result(JsValue, JsValue)) {
  let result_str = case name, msg {
    "", _ -> msg
    _, "" -> name
    _, _ -> name <> ": " <> msg
  }
  #(state, Ok(JsString(result_str)))
}
