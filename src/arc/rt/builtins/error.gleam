//// `rt_builtins/error` — Error + NativeError prototypes/constructors +
//// Error.prototype.toString / stack accessor (SPEC §7.M6
//// builtins-object-function-error).
////
//// Port of `arc/vm/builtins/error.gleam` init + dispatch, re-expressed over
//// the threaded `Agent` model. arc's `#(State, Result(v,e))` becomes
//// `#(JsVal, Agent)` with `Error(e)` → `t_throw(st, e)` (D7).
////
//// **Return-tuple order is `#(V, St')` — value FIRST (R1).**

import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/builtins/iter_protocol
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type ErrorNative, type Handle, type JsVal,
  AggregateErrorConstructor, DataProperty, ErrorCaptureStackTrace,
  ErrorConstructor, ErrorIsError, ErrorN, ErrorObj, ErrorPrototypeToString,
  ErrorStackGetter, ErrorStackSetter, JFloat, KHandle, KNull, KStr, KUndef,
  Named, ParsedDesc, SObject, StringKey, SuppressedErrorConstructor, classify,
  mk_bool, mk_number, mk_object, mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/dict
import gleam/option.{type Option, None, Some}

/// All error-related builtin types (arc `error.gleam:22-34`). Maps onto the
/// `Realm` record's error/type_error/.. fields.
pub type ErrorFamily {
  ErrorFamily(
    error: BuiltinPair,
    type_error: BuiltinPair,
    reference_error: BuiltinPair,
    range_error: BuiltinPair,
    syntax_error: BuiltinPair,
    eval_error: BuiltinPair,
    uri_error: BuiltinPair,
    aggregate_error: BuiltinPair,
  )
}

/// Set up all error prototypes and constructors as `KNative` cells.
pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
) -> #(ErrorFamily, Agent) {
  // Error.prototype.toString method.
  let #(to_string_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("toString", ErrorN(ErrorPrototypeToString), 0),
    ])
  // V8 static extensions on the base Error only.
  let #(capture_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("captureStackTrace", ErrorN(ErrorCaptureStackTrace), 2),
      #("isError", ErrorN(ErrorIsError), 1),
    ])
  let #(stl_prop, st) = common.builtin_property(st, mk_number(JFloat(10.0)))
  let error_static = [#("stackTraceLimit", stl_prop), ..capture_methods]
  // Error — base type with name + message on prototype.
  let #(name_prop, st) = common.builtin_property(st, mk_string("Error"))
  let #(msg_prop, st) = common.builtin_property(st, mk_string(""))
  let #(error, st) =
    common.init_type(
      st,
      object_proto,
      fn_proto,
      [#("name", name_prop), #("message", msg_prop), ..to_string_methods],
      fn(proto) { ErrorN(ErrorConstructor(proto:)) },
      "Error",
      1,
      error_static,
    )
  // Error.prototype.stack — accessor {get, set, E:F, C:T}.
  let #(stack_accessor, st) =
    common.alloc_get_set_accessor(
      st,
      fn_proto,
      ErrorN(ErrorStackGetter),
      ErrorN(ErrorStackSetter(proto: error.prototype)),
      "stack",
    )
  let st =
    common.add_named_property(st, error.prototype, "stack", stack_accessor)
  // Error subclasses: proto → %Error.prototype%, ctor.[[Prototype]] → %Error%
  // (§20.5.6.2).
  let #(type_error, st) = subclass(st, error, "TypeError", 1, ErrorConstructor)
  let #(reference_error, st) =
    subclass(st, error, "ReferenceError", 1, ErrorConstructor)
  let #(range_error, st) =
    subclass(st, error, "RangeError", 1, ErrorConstructor)
  let #(syntax_error, st) =
    subclass(st, error, "SyntaxError", 1, ErrorConstructor)
  let #(eval_error, st) = subclass(st, error, "EvalError", 1, ErrorConstructor)
  let #(uri_error, st) = subclass(st, error, "URIError", 1, ErrorConstructor)
  // AggregateError ( errors, message [ , options ] ) — §20.5.7.1.1.
  let #(aggregate_error, st) =
    subclass(st, error, "AggregateError", 2, AggregateErrorConstructor)
  #(
    ErrorFamily(
      error:,
      type_error:,
      reference_error:,
      range_error:,
      syntax_error:,
      eval_error:,
      uri_error:,
      aggregate_error:,
    ),
    st,
  )
}

/// One NativeError subclass — proto inherits from %Error.prototype%, ctor's
/// [[Prototype]] is %Error% (§20.5.6.2).
fn subclass(
  st: Agent,
  base: BuiltinPair,
  name: String,
  arity: Int,
  native: fn(Handle) -> ErrorNative,
) -> #(BuiltinPair, Agent) {
  let #(name_prop, st) = common.builtin_property(st, mk_string(name))
  common.init_type(
    st,
    base.prototype,
    base.constructor,
    [#("name", name_prop)],
    fn(proto) { ErrorN(native(proto)) },
    name,
    arity,
    [],
  )
}

// ── dispatch ────────────────────────────────────────────────────────────────

/// Per-module dispatch for Error native functions. `new_target` is `undefined`
/// for a plain call; `dispatch_native_construct` re-enters with it set.
pub fn dispatch(
  st: Agent,
  native: ErrorNative,
  this: JsVal,
  args: List(JsVal),
  new_target: JsVal,
) -> #(JsVal, Agent) {
  case native {
    ErrorConstructor(proto:) -> call_error_ctor(st, proto, args, new_target)
    AggregateErrorConstructor(proto:) ->
      aggregate_error_ctor(st, proto, args, new_target)
    SuppressedErrorConstructor(proto:) ->
      suppressed_error_ctor(st, proto, args, new_target)
    ErrorPrototypeToString -> error_to_string(st, this)
    ErrorCaptureStackTrace -> capture_stack_trace(st, args)
    ErrorStackGetter -> stack_getter(st, this)
    ErrorStackSetter(proto:) -> stack_setter(st, proto, this, args)
    ErrorIsError -> is_error(st, args)
  }
}

/// §20.5.1.1 Error ( message [ , options ] ) — steps 1-5.
fn call_error_ctor(
  st: Agent,
  fallback_proto: Handle,
  args: List(JsVal),
  new_target: JsVal,
) -> #(JsVal, Agent) {
  let #(message, options) = helpers.two_args_or_undefined(args)
  // Steps 1-2: OrdinaryCreateFromConstructor(newTarget, "%Error.prototype%").
  let #(proto, st) = proto_from_new_target(st, new_target, fallback_proto)
  // Step 3: If message !== undefined, this.message = ToString(message).
  case classify(message) {
    KUndef -> {
      let #(h, st) = alloc_error(st, proto, None, options)
      #(mk_object(h), st)
    }
    KStr(msg) -> {
      let #(h, st) = alloc_error(st, proto, Some(msg), options)
      #(mk_object(h), st)
    }
    _ -> {
      // Step 3a: ToString(message) — runs BEFORE options "cause" get.
      let #(msg, st) = rt_val.t_to_string(st, message)
      let #(h, st) = alloc_error(st, proto, Some(msg), options)
      #(mk_object(h), st)
    }
  }
}

/// §20.5.7.1.1 AggregateError ( errors, message [ , options ] ) — steps 1-7.
fn aggregate_error_ctor(
  st: Agent,
  fallback_proto: Handle,
  args: List(JsVal),
  new_target: JsVal,
) -> #(JsVal, Agent) {
  let #(errors, message, options) = helpers.three_args_or_undefined(args)
  let #(proto, st) = proto_from_new_target(st, new_target, fallback_proto)
  // Steps 3-4: message + cause.
  let #(h, st) = case classify(message) {
    KUndef -> alloc_error(st, proto, None, options)
    _ -> {
      let #(msg, st) = rt_val.t_to_string(st, message)
      alloc_error(st, proto, Some(msg), options)
    }
  }
  // Steps 5-6: IteratorToList(? GetIterator(errors, sync)) → fresh Array,
  // installed as "errors" {W:T, E:F, C:T}. arc error.gleam:284-298.
  let #(rec, st) = iter_protocol.get_iterator_sync(st, errors)
  let #(collected, st) = iter_protocol.iterator_to_list(st, rec)
  let #(arr_h, st) = common.alloc_array(st, collected, st.realm.array.prototype)
  let #(errors_prop, st) = common.builtin_property(st, mk_object(arr_h))
  let st = common.add_named_property(st, h, "errors", errors_prop)
  #(mk_object(h), st)
}

/// SuppressedError ( error, suppressed, message ) — Explicit Resource Mgmt.
fn suppressed_error_ctor(
  st: Agent,
  fallback_proto: Handle,
  args: List(JsVal),
  new_target: JsVal,
) -> #(JsVal, Agent) {
  let #(err, suppressed, message) = helpers.three_args_or_undefined(args)
  let #(proto, st) = proto_from_new_target(st, new_target, fallback_proto)
  let #(msg_opt, st) = case classify(message) {
    KUndef -> #(None, st)
    _ -> {
      let #(s, st) = rt_val.t_to_string(st, message)
      #(Some(s), st)
    }
  }
  let #(err_prop, st) = common.builtin_property(st, err)
  let #(sup_prop, st) = common.builtin_property(st, suppressed)
  let base = [#("error", err_prop), #("suppressed", sup_prop)]
  let #(props, st) = case msg_opt {
    Some(msg) -> {
      let #(mp, st) = common.builtin_property(st, mk_string(msg))
      #([#("message", mp), ..base], st)
    }
    None -> #(base, st)
  }
  let #(h, st) = common.alloc_error_slot(st, proto, props)
  let st = attach_stack(st, h, "SuppressedError", option.unwrap(msg_opt, ""))
  #(mk_object(h), st)
}

/// §10.1.13.2 GetPrototypeFromConstructor: `Get(newTarget, "prototype")` or
/// fall back to the intrinsic. `newTarget` undefined → intrinsic directly.
fn proto_from_new_target(
  st: Agent,
  new_target: JsVal,
  fallback: Handle,
) -> #(Handle, Agent) {
  case classify(new_target) {
    KUndef -> #(fallback, st)
    _ -> {
      let #(p, st) =
        rt_obj.t_get_prop(st, new_target, StringKey(Named("prototype")))
      case classify(p) {
        KHandle(h) -> #(h, st)
        _ -> #(fallback, st)
      }
    }
  }
}

/// Allocate an error object with optional `message` and install `cause` from
/// `options` (§20.5.8.1 InstallErrorCause). Attaches a stack header.
fn alloc_error(
  st: Agent,
  proto: Handle,
  message: Option(String),
  options: JsVal,
) -> #(Handle, Agent) {
  let #(props, st) = case message {
    Some(msg) -> {
      let #(mp, st) = common.builtin_property(st, mk_string(msg))
      #([#("message", mp)], st)
    }
    None -> #([], st)
  }
  let #(h, st) = common.alloc_error_slot(st, proto, props)
  let name = error_name(st, Some(proto), 100)
  let st = attach_stack(st, h, name, option.unwrap(message, ""))
  install_error_cause(st, h, options)
}

/// §20.5.8.1 InstallErrorCause ( O, options ).
fn install_error_cause(
  st: Agent,
  h: Handle,
  options: JsVal,
) -> #(Handle, Agent) {
  case classify(options) {
    KHandle(_) -> {
      let #(has, st) = rt_obj.t_has_prop(st, options, StringKey(Named("cause")))
      case has {
        False -> #(h, st)
        True -> {
          let #(cause, st) =
            rt_obj.t_get_prop(st, options, StringKey(Named("cause")))
          let #(cp, st) = common.builtin_property(st, cause)
          let st = common.add_named_property(st, h, "cause", cp)
          #(h, st)
        }
      }
    }
    _ -> #(h, st)
  }
}

/// Read the `name` data property off an error prototype (walks the chain,
/// bounded by `fuel`). Defaults to "Error".
fn error_name(st: Agent, proto: Option(Handle), fuel: Int) -> String {
  case proto {
    Some(h) if fuel > 0 ->
      case rt_obj.as_sobject(st, rt_store.t_cell_get(st, h)) {
        SObject(props:, proto: parent, ..) ->
          case dict.get(props, Named("name")) {
            Ok(DataProperty(value: v, ..)) ->
              case classify(v) {
                KStr(n) -> n
                _ -> error_name(st, parent, fuel - 1)
              }
            _ -> error_name(st, parent, fuel - 1)
          }
        _ -> "Error"
      }
    _ -> "Error"
  }
}

/// Write the `[[ErrorData]]` stack string. 2core has no call-stack
/// introspection yet (M-CALL threads no frame stack), so the stack is the
/// header line only.
fn attach_stack(st: Agent, h: Handle, name: String, msg: String) -> Agent {
  let header = case msg {
    "" -> name
    _ -> name <> ": " <> msg
  }
  // Non-error objects (Error.captureStackTrace targets) get a non-enumerable
  // own `stack` data property, matching V8. arc state.gleam:821-849.
  let #(stack_prop, st) = common.builtin_property(st, mk_string(header))
  let st = rt_obj.devolve(st, h)
  rt_store.t_cell_update(st, h, fn(slot) {
    case slot {
      SObject(kind: ErrorObj(..), ..) as s ->
        SObject(..s, kind: ErrorObj(stack: header))
      SObject(props:, ..) as s ->
        SObject(..s, props: dict.insert(props, Named("stack"), stack_prop))
      other -> other
    }
  })
}

/// Error.captureStackTrace ( target [ , constructorOpt ] ) — V8 extension.
fn capture_stack_trace(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  case classify(helpers.first_arg_or_undefined(args)) {
    KHandle(h) -> {
      let #(name, msg) = target_header_parts(st, h)
      let st = attach_stack(st, h, name, msg)
      #(mk_undefined(), st)
    }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "Error.captureStackTrace requires that the first argument be an object",
      )
  }
}

/// Read target's own `name`/`message` data properties for the stack header.
fn target_header_parts(st: Agent, h: Handle) -> #(String, String) {
  let read = fn(key) {
    case rt_obj.as_sobject(st, rt_store.t_cell_get(st, h)) {
      SObject(props:, ..) ->
        case dict.get(props, Named(key)) {
          Ok(DataProperty(value: v, ..)) ->
            case classify(v) {
              KStr(s) -> Some(s)
              _ -> None
            }
          _ -> None
        }
      _ -> None
    }
  }
  #(option.unwrap(read("name"), "Error"), option.unwrap(read("message"), ""))
}

/// Error.isError ( arg ) — proposal.
fn is_error(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let result = case classify(helpers.first_arg_or_undefined(args)) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: ErrorObj(..), ..) -> True
        _ -> False
      }
    _ -> False
  }
  #(mk_bool(result), st)
}

/// get Error.prototype.stack — error-stack-accessor proposal.
fn stack_getter(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  case classify(this) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: ErrorObj(stack:), ..) -> #(mk_string(stack), st)
        _ -> #(mk_undefined(), st)
      }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "get Error.prototype.stack called on non-object",
      )
  }
}

/// set Error.prototype.stack — error-stack-accessor proposal.
fn stack_setter(
  st: Agent,
  proto: Handle,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case classify(this), classify(helpers.first_arg_or_undefined(args)) {
    KNull, _ | KUndef, _ ->
      rt_val.t_throw_type_error(
        st,
        "set Error.prototype.stack called on non-object",
      )
    KHandle(h), KStr(s) -> set_stack_ignoring_prototype(st, proto, h, s)
    KHandle(_), _ ->
      rt_val.t_throw_type_error(
        st,
        "Error.prototype.stack value must be a string",
      )
    _, _ ->
      rt_val.t_throw_type_error(
        st,
        "set Error.prototype.stack called on non-object",
      )
  }
}

/// SetterThatIgnoresPrototypeProperties ( this, home, p, v ).
fn set_stack_ignoring_prototype(
  st: Agent,
  proto: Handle,
  h: Handle,
  s: String,
) -> #(JsVal, Agent) {
  case h == proto {
    True ->
      rt_val.t_throw_type_error(
        st,
        "Cannot assign to read only property 'stack' of Error.prototype",
      )
    False -> {
      // Step 3: desc = ? this.[[GetOwnProperty]]("stack").
      let has_own = case rt_obj.as_sobject(st, rt_store.t_cell_get(st, h)) {
        SObject(props:, ..) -> dict.has_key(props, Named("stack"))
        _ -> False
      }
      case has_own {
        // Step 5: Set(this, "stack", v, true) — false → TypeError.
        True -> {
          let #(ok, st) =
            rt_obj.t_set_prop(
              st,
              mk_object(h),
              StringKey(Named("stack")),
              mk_string(s),
            )
          case ok {
            True -> #(mk_undefined(), st)
            False ->
              rt_val.t_throw_type_error(
                st,
                "Cannot assign to read only property 'stack'",
              )
          }
        }
        // Step 4: CreateDataPropertyOrThrow(this, "stack", v) — {W:T,E:T,C:T}.
        False -> {
          let #(ok, st) =
            rt_obj.t_define_own_prop(
              st,
              h,
              StringKey(Named("stack")),
              ParsedDesc(
                value: Some(mk_string(s)),
                get: None,
                set: None,
                writable: Some(True),
                enumerable: Some(True),
                configurable: Some(True),
              ),
            )
          case ok {
            True -> #(mk_undefined(), st)
            False ->
              rt_val.t_throw_type_error(
                st,
                "Cannot assign to read only property 'stack'",
              )
          }
        }
      }
    }
  }
}

/// §20.5.3.4 Error.prototype.toString ( ).
fn error_to_string(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  case classify(this) {
    KNull | KUndef ->
      rt_val.t_throw_type_error(
        st,
        "Error.prototype.toString called on non-object",
      )
    KHandle(_) -> {
      // Step 3: Let name be ? Get(O, "name").
      let #(name_val, st) =
        rt_obj.t_get_prop(st, this, StringKey(Named("name")))
      // Steps 4-5.
      let #(name, st) = case classify(name_val) {
        KUndef -> #("Error", st)
        _ -> rt_val.t_to_string(st, name_val)
      }
      // Step 6: Let msg be ? Get(O, "message").
      let #(msg_val, st) =
        rt_obj.t_get_prop(st, this, StringKey(Named("message")))
      // Steps 7-8.
      let #(msg, st) = case classify(msg_val) {
        KUndef -> #("", st)
        _ -> rt_val.t_to_string(st, msg_val)
      }
      // Steps 9-11.
      let result = case name, msg {
        "", _ -> msg
        _, "" -> name
        _, _ -> name <> ": " <> msg
      }
      #(mk_string(result), st)
    }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "Error.prototype.toString called on non-object",
      )
  }
}
