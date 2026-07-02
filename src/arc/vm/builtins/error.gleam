import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/dom_exception
import arc/vm/builtins/object as builtins_object
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/key.{Named}
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type ErrorNativeFn, type JsValue, type Property, type Ref, DataProperty,
  Dispatch, ErrorConstructor, ErrorNative, JsBool, JsNull, JsObject, JsString,
  JsUndefined, ObjectSlot,
}
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

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
    suppressed_error: BuiltinType,
  )
}

/// Set up all error prototypes and constructors as NativeFunctions.
pub fn init(
  h: Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(host), ErrorBuiltins) {
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
      #("isError", ErrorNative(value.ErrorIsError), 1),
    ])
  let error_static = [
    #(
      "stackTraceLimit",
      value.builtin_property(value.JsNumber(value.Finite(10.0))),
    ),
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

  // Error.prototype.stack — accessor property per the error-stack-accessor
  // proposal: { get, set, enumerable: false, configurable: true }. Instances
  // carry the trace in their [[ErrorData]] slot, not as an own property.
  let #(h, stack_accessor) =
    common.alloc_get_set_accessor(
      h,
      function_proto,
      ErrorNative(value.ErrorStackGetter),
      ErrorNative(value.ErrorStackSetter(proto: error.prototype)),
      "stack",
    )
  let h =
    heap.update(h, error.prototype, fn(slot) {
      case slot {
        ObjectSlot(properties:, ..) ->
          ObjectSlot(
            ..slot,
            properties: dict.insert(properties, Named("stack"), stack_accessor),
          )
        other -> other
      }
    })

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

  // SuppressedError (Explicit Resource Management proposal) — inherits from
  // Error.prototype like the other NativeErrors, but its constructor takes
  // (error, suppressed, message), so it gets a dedicated native fn.
  let #(h, suppressed_error) =
    common.init_type(
      h,
      error.prototype,
      function_proto,
      [#("name", value.builtin_property(JsString("SuppressedError")))],
      fn(proto) {
        Dispatch(ErrorNative(value.SuppressedErrorConstructor(proto:)))
      },
      "SuppressedError",
      3,
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
      suppressed_error:,
    ),
  )
}

/// Per-module dispatch for Error native functions.
pub fn dispatch(
  native: ErrorNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case native {
    ErrorConstructor(proto:) -> call_native(proto, args, JsUndefined, state)
    value.SuppressedErrorConstructor(proto:) ->
      suppressed_error_native(proto, args, state)
    value.ErrorPrototypeToString -> error_to_string(this, state)
    value.ErrorCaptureStackTrace -> capture_stack_trace(args, state)
    value.ErrorStackGetter -> stack_getter(this, state)
    value.ErrorStackSetter(proto:) -> stack_setter(proto, this, args, state)
    value.ErrorIsError -> is_error_native(args, state)
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
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
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
fn target_header(h: Heap(host), ref: Ref) -> String {
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

/// Native error constructor — §20.5.1.1 Error ( message [ , options ] ):
/// if (message !== undefined) this.message = ToString(message), then
/// InstallErrorCause(O, options). Creates a new error object with the proto
/// embedded in the NativeFn.
/// Per §20.5.6.3: "message" is writable+configurable but NOT enumerable.
fn call_native(
  proto: Ref,
  args: List(JsValue),
  _this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let options = case args {
    [_, o, ..] -> o
    _ -> JsUndefined
  }
  case args {
    [JsUndefined, ..] | [] -> alloc_error(state, proto, None, options)
    [JsString(msg), ..] -> alloc_error(state, proto, Some(msg), options)
    [other, ..] -> {
      // Step 3a: ToString(message) — runs BEFORE the options "cause" get.
      use msg, state <- coerce.try_to_string(state, other)
      alloc_error(state, proto, Some(msg), options)
    }
  }
}

/// SuppressedError ( error, suppressed, message ) — Explicit Resource
/// Management proposal.
///
///   1. (skipped) If NewTarget is undefined, let newTarget be the active
///      function object. We allocate with the intrinsic prototype, matching
///      the other NativeError constructors in this module.
///   2. Let O be ? OrdinaryCreateFromConstructor(newTarget, "%SuppressedError.prototype%").
///   3. If message is not undefined, then
///      a. Let msg be ? ToString(message).
///      b. Perform CreateNonEnumerableDataPropertyOrThrow(O, "message", msg).
///   4. Perform ! CreateNonEnumerableDataPropertyOrThrow(O, "error", error).
///   5. Perform ! CreateNonEnumerableDataPropertyOrThrow(O, "suppressed", suppressed).
///   6. Return O.
fn suppressed_error_native(
  proto: Ref,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(err, suppressed, message) = case args {
    [] -> #(JsUndefined, JsUndefined, JsUndefined)
    [e] -> #(e, JsUndefined, JsUndefined)
    [e, s] -> #(e, s, JsUndefined)
    [e, s, m, ..] -> #(e, s, m)
  }
  case message {
    // Step 3: message is undefined — no "message" property
    JsUndefined -> {
      let #(state, v) = alloc_suppressed(state, proto, None, err, suppressed)
      #(state, Ok(v))
    }
    _ -> {
      use msg, state <- coerce.try_to_string(state, message)
      let #(state, v) =
        alloc_suppressed(state, proto, Some(msg), err, suppressed)
      #(state, Ok(v))
    }
  }
}

/// Allocate a SuppressedError instance with non-enumerable error/suppressed
/// (and optional message) data properties, plus a stack trace.
fn alloc_suppressed(
  state: State(host),
  proto: Ref,
  message: Option(String),
  err: JsValue,
  suppressed: JsValue,
) -> #(State(host), JsValue) {
  let props = [
    #("error", value.builtin_property(err)),
    #("suppressed", value.builtin_property(suppressed)),
  ]
  let props = case message {
    Some(msg) -> [#("message", value.builtin_property(JsString(msg))), ..props]
    None -> props
  }
  let #(heap, ref) = alloc_error_slot(state.heap, proto, props)
  let state = State(..state, heap:)
  let header = state.error_header("SuppressedError", option.unwrap(message, ""))
  let state = state.attach_stack(state, JsObject(ref), header)
  #(state, JsObject(ref))
}

/// DisposeResources step 2.b.i: "Let error be a newly created SuppressedError
/// object" with non-enumerable "error" (the new exception) and "suppressed"
/// (the previously pending exception) properties. Used by DisposableStack.
pub fn make_suppressed_error(
  state: State(host),
  err: JsValue,
  suppressed: JsValue,
) -> #(State(host), JsValue) {
  alloc_suppressed(
    state,
    state.builtins.suppressed_error.prototype,
    None,
    err,
    suppressed,
  )
}

/// Allocate an error object with the given message (None = no `message`
/// property), attach a `stack` trace captured from the current call stack,
/// and install a `cause` from the options bag (§20.5.8.1 InstallErrorCause).
fn alloc_error(
  state: State(host),
  proto: Ref,
  message: Option(String),
  options: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  let props = case message {
    Some(msg) -> [#("message", value.builtin_property(JsString(msg)))]
    None -> []
  }
  let #(heap, ref) = alloc_error_slot(state.heap, proto, props)
  let state = State(..state, heap:)
  let header =
    state.error_header(
      error_name(state.heap, proto),
      option.unwrap(message, ""),
    )
  let state = state.attach_stack(state, JsObject(ref), header)
  install_error_cause(state, ref, options)
}

/// §20.5.8.1 InstallErrorCause ( O, options ):
///
///   1. If options is an Object and ? HasProperty(options, "cause") is true:
///      a. Let cause be ? Get(options, "cause").
///      b. Perform ! CreateNonEnumerableDataPropertyOrThrow(O, "cause", cause).
///   2. Return unused.
///
/// Returns the error object `ref` on success (callers tail-call this).
fn install_error_cause(
  state: State(host),
  ref: Ref,
  options: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  case options {
    JsObject(opts_ref) -> {
      use has, state <- state.try_op(object.has_property_stateful(
        state,
        opts_ref,
        object.PkString(Named("cause")),
      ))
      case has {
        False -> #(state, Ok(JsObject(ref)))
        True -> {
          // Step 1a: Get(options, "cause") — may invoke a getter / throw.
          use cause, state <- state.try_op(object.get_value(
            state,
            opts_ref,
            Named("cause"),
            options,
          ))
          // Step 1b: non-enumerable {W:T, E:F, C:T} data property.
          let heap =
            heap.update(state.heap, ref, fn(slot) {
              case slot {
                ObjectSlot(properties:, ..) ->
                  ObjectSlot(
                    ..slot,
                    properties: dict.insert(
                      properties,
                      Named("cause"),
                      value.builtin_property(cause),
                    ),
                  )
                other -> other
              }
            })
          #(State(..state, heap:), Ok(JsObject(ref)))
        }
      }
    }
    _ -> #(state, Ok(JsObject(ref)))
  }
}

/// Allocate an error instance slot: an otherwise-ordinary object whose kind
/// is ErrorObject — the [[ErrorData]] internal slot (§20.5.4). The stack
/// string starts empty; state.attach_stack fills it in.
fn alloc_error_slot(
  h: Heap(host),
  proto: Ref,
  props: List(#(String, Property)),
) -> #(Heap(host), Ref) {
  heap.alloc(
    h,
    ObjectSlot(
      kind: value.ErrorObject(stack: ""),
      properties: common.named_props(props),
      elements: elements.new(),
      prototype: Some(proto),
      symbol_properties: [],
      extensible: True,
    ),
  )
}

/// Error.isError ( arg ) — Error.isError proposal:
///
///   1. If arg is not an Object, return false.
///   2. If arg has an [[ErrorData]] internal slot, return true.
///   3. Return false.
///
/// Proxies wrapping errors return false (no [[ErrorData]] of their own).
fn is_error_native(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let arg = case args {
    [x, ..] -> x
    [] -> JsUndefined
  }
  let result = case arg {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: value.ErrorObject(_), ..)) -> True
        _ -> False
      }
    _ -> False
  }
  #(state, Ok(JsBool(result)))
}

/// get Error.prototype.stack — error-stack-accessor proposal.
///
///   1. Let E be the this value.
///   2. If E is not an Object, throw a TypeError exception.
///   3. If E does not have an [[ErrorData]] internal slot, return undefined.
///   4. Return an implementation-defined string that represents the stack
///      trace of E.
fn stack_getter(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: value.ErrorObject(stack:), ..)) -> #(
          state,
          Ok(JsString(stack)),
        )
        // Step 3: no [[ErrorData]] (incl. proxies wrapping errors) → undefined.
        _ -> #(state, Ok(JsUndefined))
      }
    _ ->
      state.type_error(state, "get Error.prototype.stack called on non-object")
  }
}

/// set Error.prototype.stack — error-stack-accessor proposal.
///
///   1. Let E be the this value.
///   2. If E is not an Object, throw a TypeError exception.
///   3. If v is not a String, throw a TypeError exception.
///   4. Perform ? SetterThatIgnoresPrototypeProperties(E, %Error.prototype%,
///      "stack", v).
///   5. Return undefined.
fn stack_setter(
  proto: Ref,
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let v = case args {
    [x, ..] -> x
    [] -> JsUndefined
  }
  case this, v {
    // Step 2: E is not an Object → TypeError.
    JsNull, _ | JsUndefined, _ ->
      state.type_error(state, "set Error.prototype.stack called on non-object")
    JsObject(ref), JsString(s) ->
      set_stack_ignoring_prototype(state, proto, ref, s)
    // Step 3: v is not a String → TypeError.
    JsObject(_), _ ->
      state.type_error(state, "Error.prototype.stack value must be a string")
    _, _ ->
      state.type_error(state, "set Error.prototype.stack called on non-object")
  }
}

/// SetterThatIgnoresPrototypeProperties ( this, home, p, v ) — §B/proposal:
///
///   1. (checked by caller) If this is not an Object, throw a TypeError.
///   2. If SameValue(this, home) is true, throw a TypeError exception.
///   3. Let desc be ? this.[[GetOwnProperty]](p).
///   4. If desc is undefined, perform ? CreateDataPropertyOrThrow(this, p, v).
///   5. Else, perform ? Set(this, p, v, true).
///   6. Return unused.
fn set_stack_ignoring_prototype(
  state: State(host),
  proto: Ref,
  ref: Ref,
  s: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 2: assigning on %Error.prototype% itself emulates assignment to a
  // non-writable data property in strict mode — TypeError. `home` is the
  // setter function's own realm's %Error.prototype% (carried in the native
  // variant), NOT the current realm's — a cross-realm call must still detect
  // its own home object, otherwise step 5's Set() re-enters this setter.
  case ref.id == proto.id {
    True -> {
      // The proposal's %TypeError% here is the setter function's realm
      // intrinsic — resolve the owning realm's builtins via its
      // %Error.prototype% so a cross-realm call throws that realm's
      // TypeError, not the calling realm's.
      let realm_builtins =
        state.ctx.realms
        |> dict.values
        |> list.find(fn(b) { b.error.prototype.id == proto.id })
        |> result.unwrap(state.builtins)
      state.type_error_with_builtins(
        state,
        realm_builtins,
        "Cannot assign to read only property 'stack' of Error.prototype",
      )
    }
    False -> {
      // Step 3: desc = ? this.[[GetOwnProperty]]("stack") — proxy-aware.
      use own, state <- state.try_op(builtins_object.get_own_property_stateful(
        state,
        ref,
        JsString("stack"),
      ))
      case own {
        // Step 4: CreateDataPropertyOrThrow(this, "stack", v) — defines a
        // writable/enumerable/configurable data property; false → TypeError.
        None -> {
          use state <- builtins_object.create_data_property_or_throw(
            state,
            ref,
            JsString("stack"),
            JsString(s),
          )
          #(state, Ok(JsUndefined))
        }
        // Step 5: Set(this, "stack", v, true) — false → TypeError.
        Some(_) ->
          case
            object.set_value(
              state,
              ref,
              Named("stack"),
              JsString(s),
              JsObject(ref),
            )
          {
            Ok(#(state, True)) -> #(state, Ok(JsUndefined))
            Ok(#(state, False)) ->
              state.type_error(
                state,
                "Cannot assign to read only property 'stack'",
              )
            Error(#(thrown, state)) -> #(state, Error(thrown))
          }
      }
    }
  }
}

/// Read the `name` data property off an error prototype (e.g. "TypeError"),
/// for the first line of the stack trace. Defaults to "Error".
fn error_name(h: Heap(host), proto: Ref) -> String {
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
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
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
  state: State(host),
  ref: Ref,
  this: JsValue,
  name: String,
) -> #(State(host), Result(JsValue, JsValue)) {
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
  state: State(host),
  name: String,
  msg: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  let result_str = case name, msg {
    "", _ -> msg
    _, "" -> name
    _, _ -> name <> ": " <> msg
  }
  #(state, Ok(JsString(result_str)))
}
