import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/dom_exception
import arc/vm/heap
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type ErrorNativeFn, type JsValue, type Ref, DataProperty, Dispatch,
  ErrorConstructor, ErrorNative, JsNull, JsObject, JsString, JsUndefined, Named,
  ObjectSlot,
}
import gleam/dict
import gleam/option.{type Option, None, Some}

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

  // Static (constructor) members — V8 extensions, only on the base Error:
  //   Error.captureStackTrace(target [, constructorOpt])
  //   Error.stackTraceLimit = 10  (mutable cap honored when building traces)
  let #(h, capture_method) =
    common.alloc_methods(h, function_proto, [
      #("captureStackTrace", ErrorNative(value.ErrorCaptureStackTrace), 2),
    ])
  let error_static = [
    #("stackTraceLimit", value.builtin_property(value.JsNumber(value.Finite(10.0)))),
    ..capture_method
  ]

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
      fn(proto) { Dispatch(ErrorNative(ErrorConstructor(proto:))) },
      "Error",
      1,
      error_static,
    )

  // Error subclasses — each inherits from Error.prototype
  let subclass = fn(h, name, arity) {
    common.init_type(
      h,
      error.prototype,
      function_proto,
      [#("name", value.builtin_property(JsString(name)))],
      fn(proto) { Dispatch(ErrorNative(ErrorConstructor(proto:))) },
      name,
      arity,
      [],
    )
  }

  let #(h, type_error) = subclass(h, "TypeError", 1)
  let #(h, reference_error) = subclass(h, "ReferenceError", 1)
  let #(h, range_error) = subclass(h, "RangeError", 1)
  let #(h, syntax_error) = subclass(h, "SyntaxError", 1)
  let #(h, eval_error) = subclass(h, "EvalError", 1)
  let #(h, uri_error) = subclass(h, "URIError", 1)
  let #(h, aggregate_error) = subclass(h, "AggregateError", 2)

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
    value.ErrorCaptureStackTrace -> capture_stack_trace(args, state)
    value.DomExceptionConstructor(proto:) ->
      dom_exception.construct(proto, args, state)
    value.DomExceptionGetCode -> dom_exception.get_code(this, state)
  }
}

/// Error.captureStackTrace ( target [ , constructorOpt ] ) — V8 extension.
/// Installs a `stack` string on `target` describing the call site, then returns
/// undefined. `constructorOpt` (a function) tells V8 to omit all frames at and
/// above it; Arc's frame model can't reliably match by function identity, so it
/// is accepted but does not trim frames (a documented deviation).
///
/// The first line uses target's own `name`/`message` if it looks like an error,
/// matching V8's `${target.name}: ${target.message}` prefix.
fn capture_stack_trace(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case args {
    [JsObject(ref), ..] -> {
      let header = target_header(state.heap, ref)
      let state = state.attach_stack(state, JsObject(ref), header)
      #(state, Ok(JsUndefined))
    }
    // Per V8, a non-object target throws a TypeError.
    _ ->
      state.type_error(
        state,
        "Error.captureStackTrace requires that the first argument be an object",
      )
  }
}

/// First line for captureStackTrace: read the target's own `name` and `message`
/// data properties (if present) and combine, defaulting `name` to "Error".
fn target_header(h: Heap, ref: Ref) -> String {
  let read = fn(key) {
    case heap.read(h, ref) {
      Some(ObjectSlot(properties:, ..)) ->
        case dict.get(properties, Named(key)) {
          Ok(DataProperty(value: JsString(s), ..)) -> Some(s)
          _ -> None
        }
      _ -> None
    }
  }
  let name = option.unwrap(read("name"), "Error")
  state.error_header(name, option.unwrap(read("message"), ""))
}

/// Native error constructor: if (message !== undefined) this.message = message
/// Creates a new error object with the proto embedded in the NativeFn.
/// Per §20.5.6.3: "message" is writable+configurable but NOT enumerable.
fn call_native(
  proto: Ref,
  args: List(JsValue),
  _this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case args {
    [JsUndefined, ..] | [] -> alloc_error(state, proto, None)
    [JsString(msg), ..] -> alloc_error(state, proto, Some(msg))
    [other, ..] -> {
      use msg, state <- coerce.try_to_string(state, other)
      alloc_error(state, proto, Some(msg))
    }
  }
}

/// Allocate an error object with the given message (None = no `message`
/// property) and attach a `stack` trace captured from the current call stack.
fn alloc_error(
  state: State,
  proto: Ref,
  message: Option(String),
) -> #(State, Result(JsValue, JsValue)) {
  let props = case message {
    Some(msg) -> [#("message", value.builtin_property(JsString(msg)))]
    None -> []
  }
  let #(heap, ref) = common.alloc_pojo(state.heap, proto, props)
  let state = State(..state, heap:)
  let header =
    state.error_header(error_name(state.heap, proto), option.unwrap(message, ""))
  let state = state.attach_stack(state, JsObject(ref), header)
  #(state, Ok(JsObject(ref)))
}

/// Read the `name` data property off an error prototype (e.g. "TypeError"),
/// for the first line of the stack trace. Defaults to "Error".
fn error_name(h: Heap, proto: Ref) -> String {
  case heap.read(h, proto) {
    Some(ObjectSlot(properties:, ..)) ->
      case dict.get(properties, Named("name")) {
        Ok(DataProperty(value: JsString(n), ..)) -> n
        _ -> "Error"
      }
    _ -> "Error"
  }
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
      state.type_error(state, "Error.prototype.toString called on non-object")
    JsObject(ref) -> {
      // Step 3: Let name be ? Get(O, "name").
      use name_val, state <- state.try_op(object.get_value(
        state,
        ref,
        Named("name"),
        this,
      ))
      // Steps 4-5: If undefined → "Error", else ToString(name).
      case name_val {
        JsUndefined -> error_to_string_msg(state, ref, this, "Error")
        _ -> {
          use name, state <- coerce.try_to_string(state, name_val)
          error_to_string_msg(state, ref, this, name)
        }
      }
    }
    // Step 2: Non-object this → TypeError.
    _ ->
      state.type_error(state, "Error.prototype.toString called on non-object")
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
  use msg_val, state <- state.try_op(object.get_value(
    state,
    ref,
    Named("message"),
    this,
  ))
  // Steps 7-8: If undefined → "", else ToString(msg).
  case msg_val {
    JsUndefined -> error_to_string_combine(state, name, "")
    _ -> {
      use msg, state <- coerce.try_to_string(state, msg_val)
      error_to_string_combine(state, name, msg)
    }
  }
}

/// Helper: combine name and msg per §20.5.3.4 steps 7-9.
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
