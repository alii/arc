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
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/realm as rt_realm
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type ErrorNative, type FrameInfo, type Handle,
  type JsVal, type Realm, AggregateErrorConstructor, DataProperty,
  ErrorCaptureStackTrace, ErrorConstructor, ErrorIsError, ErrorN, ErrorObj,
  ErrorPrototypeToString, ErrorStackGetter, ErrorStackSetter, FrameInfo, JFloat,
  JInt, JNan, JNegInf, JPosInf, KHandle, KNull, KNum, KStr, KUndef, Named,
  ParsedDesc, SObject, StringKey, SuppressedErrorConstructor, classify, mk_bool,
  mk_number, mk_object, mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

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
    suppressed_error: BuiltinPair,
  )
}

/// Set up all error prototypes and constructors as `KNative` cells. `realm`
/// is the id of the realm being built (the stack setter is realm-attributed).
pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
  realm: Int,
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
      ErrorN(ErrorStackSetter(realm:)),
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
  // SuppressedError ( error, suppressed, message ) — Explicit Resource Mgmt.
  let #(suppressed_error, st) =
    subclass(st, error, "SuppressedError", 3, SuppressedErrorConstructor)
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
      suppressed_error:,
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
    ErrorStackSetter(realm:) -> stack_setter(st, realm, this, args)
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

/// SuppressedError ( error, suppressed, message ) — Explicit Resource
/// Management proposal.
///
///   1. If NewTarget is undefined, let newTarget be the active function object.
///   2. Let O be ? OrdinaryCreateFromConstructor(newTarget, "%SuppressedError.prototype%").
///   3. If message is not undefined, then
///      a. Let msg be ? ToString(message).
///      b. Perform CreateNonEnumerableDataPropertyOrThrow(O, "message", msg).
///   4. Perform ! CreateNonEnumerableDataPropertyOrThrow(O, "error", error).
///   5. Perform ! CreateNonEnumerableDataPropertyOrThrow(O, "suppressed", suppressed).
///   6. Return O.
fn suppressed_error_ctor(
  st: Agent,
  fallback_proto: Handle,
  args: List(JsVal),
  new_target: JsVal,
) -> #(JsVal, Agent) {
  let #(err, suppressed, message) = helpers.three_args_or_undefined(args)
  // Steps 1-2: OrdinaryCreateFromConstructor(newTarget, ...).
  let #(proto, st) = proto_from_new_target(st, new_target, fallback_proto)
  let #(msg_opt, st) = case classify(message) {
    // Step 3: message is undefined — no "message" property
    KUndef -> #(None, st)
    _ -> {
      let #(s, st) = rt_val.t_to_string(st, message)
      #(Some(s), st)
    }
  }
  alloc_suppressed(st, proto, msg_opt, err, suppressed)
}

/// Allocate a SuppressedError instance with non-enumerable error/suppressed
/// (and optional message) data properties, plus a stack trace.
fn alloc_suppressed(
  st: Agent,
  proto: Handle,
  message: Option(String),
  err: JsVal,
  suppressed: JsVal,
) -> #(JsVal, Agent) {
  // Steps 3-5 define "message" (when present) before "error"/"suppressed";
  // property creation order is the seq-stamp order.
  let #(msg_props, st) = case message {
    Some(msg) -> {
      let #(mp, st) = common.builtin_property(st, mk_string(msg))
      #([#("message", mp)], st)
    }
    None -> #([], st)
  }
  let #(err_prop, st) = common.builtin_property(st, err)
  let #(sup_prop, st) = common.builtin_property(st, suppressed)
  let props =
    list.append(msg_props, [#("error", err_prop), #("suppressed", sup_prop)])
  let #(h, st) = common.alloc_error_slot(st, proto, props)
  let st = attach_stack(st, h, "SuppressedError", option.unwrap(message, ""))
  #(mk_object(h), st)
}

/// DisposeResources step 1.b.i: "Let error be a newly created SuppressedError
/// object" with non-enumerable "error" (the new exception) and "suppressed"
/// (the previously pending exception) properties, in the running realm.
pub fn make_suppressed_error(
  st: Agent,
  err: JsVal,
  suppressed: JsVal,
) -> #(JsVal, Agent) {
  alloc_suppressed(
    st,
    st.realm.suppressed_error.prototype,
    None,
    err,
    suppressed,
  )
}

/// §20.5.1.1 / §20.5.6.1.1 steps 1-2: with NewTarget undefined, newTarget
/// is the active function object, whose own `prototype` is the `home`
/// intrinsic its token carries; otherwise GetPrototypeFromConstructor with
/// the same %NativeError.prototype% of newTarget's realm as the default.
fn proto_from_new_target(
  st: Agent,
  new_target: JsVal,
  home: Handle,
) -> #(Handle, Agent) {
  case classify(new_target) {
    KUndef -> #(home, st)
    _ ->
      rt_call.get_prototype_from_constructor(st, new_target, same_error_proto(
        st,
        home,
        _,
      ))
  }
}

/// The member of `realm`'s error family that `home` is of its own realm.
/// The token carries only the handle, so `home` is matched against every
/// registered realm's family to learn which member it is.
fn same_error_proto(st: Agent, home: Handle, realm: Realm) -> Handle {
  let wanted = error_protos(realm)
  [st.realm, ..dict.values(st.realms)]
  |> list.find_map(fn(r) {
    list.key_find(list.zip(error_protos(r), wanted), home)
  })
  |> result.unwrap(home)
}

fn error_protos(r: Realm) -> List(Handle) {
  [
    r.error.prototype,
    r.type_error.prototype,
    r.reference_error.prototype,
    r.range_error.prototype,
    r.syntax_error.prototype,
    r.eval_error.prototype,
    r.uri_error.prototype,
    r.aggregate_error.prototype,
    r.suppressed_error.prototype,
  ]
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

/// Default Error.stackTraceLimit (V8 parity). Used when the constructor's
/// `stackTraceLimit` property is missing or not a number.
const default_stack_limit = 10

/// Build a V8-style stack-trace string. `header` is the first line — the
/// error's `name: message` (or just `name`). The frames are `Agent.frames`
/// at the moment the error is constructed: the executing function first,
/// then its callers. Honors Error.stackTraceLimit. Port of arc
/// `state.build_stack_trace` (`state.gleam:764-790`). Lines look like:
///
///   TypeError: x is not a function
///       at inner (script:3)
///       at outer (script:7)
///       at script:10
///
fn build_stack_trace(st: Agent, header: String) -> String {
  let frames = list.take(st.frames, stack_trace_limit(st))
  case list.map(frames, format_frame) {
    [] -> header
    lines -> header <> "\n" <> string.join(lines, "\n")
  }
}

/// Format one frame: `    at name (script:line)`, or `    at script:line`
/// when the function is anonymous (e.g. the top-level script body).
fn format_frame(frame: FrameInfo) -> String {
  let FrameInfo(name:, script:, line:) = frame
  let loc = case line {
    0 -> script
    _ -> script <> ":" <> int.to_string(line)
  }
  case name {
    "" -> "    at " <> loc
    _ -> "    at " <> name <> " (" <> loc <> ")"
  }
}

/// Read Error.stackTraceLimit off the Error constructor. Non-numbers fall
/// back to the default; Infinity means "no limit"; negatives and NaN clamp to
/// 0 (no frames). Port of arc `state.stack_trace_limit`.
fn stack_trace_limit(st: Agent) -> Int {
  let ctor = rt_store.t_cell_get(st, st.realm.error.constructor)
  case rt_obj.as_sobject(st, ctor) {
    SObject(props:, ..) ->
      case dict.get(props, Named("stackTraceLimit")) {
        Ok(DataProperty(value: v, ..)) ->
          case classify(v) {
            KNum(JInt(n)) -> int.max(0, n)
            KNum(JFloat(f)) -> int.max(0, float.truncate(f))
            // Effectively unbounded — far above any real call depth.
            KNum(JPosInf) -> 1_000_000
            // -Infinity is a negative limit; NaN's ToIntegerOrInfinity is 0.
            KNum(JNegInf) | KNum(JNan) -> 0
            _ -> default_stack_limit
          }
        _ -> default_stack_limit
      }
    _ -> default_stack_limit
  }
}

/// Write the `[[ErrorData]]` stack string: the header plus one line per
/// `Agent.frames` entry (header-only when no frames are recorded).
pub fn attach_stack(st: Agent, h: Handle, name: String, msg: String) -> Agent {
  let header = case msg {
    "" -> name
    _ -> name <> ": " <> msg
  }
  let trace = build_stack_trace(st, header)
  // Non-error objects (Error.captureStackTrace targets) get a non-enumerable
  // own `stack` data property, matching V8. arc state.gleam:821-849.
  let #(stack_prop, st) = common.builtin_property(st, mk_string(trace))
  let st = rt_obj.devolve(st, h)
  rt_store.t_cell_update(st, h, fn(slot) {
    case slot {
      SObject(kind: ErrorObj(..), ..) as s ->
        SObject(..s, kind: ErrorObj(stack: trace))
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

/// set Error.prototype.stack — error-stack-accessor proposal. `realm` is
/// the setter's own realm (§10.3.1: a built-in's callee context takes
/// F.[[Realm]]): `home` is that realm's %Error.prototype% and every
/// TypeError the setter itself throws is that realm's, whichever realm called
/// it. The [[GetOwnProperty]] / [[Set]] / [[DefineOwnProperty]] steps stay in
/// the caller's realm, since a proxy trap or accessor they reach is user code.
fn stack_setter(
  st: Agent,
  realm: Int,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let proto = rt_call.realm_by_id(st, realm).error.prototype
  case classify(this), classify(helpers.first_arg_or_undefined(args)) {
    KNull, _ | KUndef, _ ->
      throw_type_error_in(
        st,
        realm,
        "set Error.prototype.stack called on non-object",
      )
    KHandle(h), KStr(s) -> set_stack_ignoring_prototype(st, realm, proto, h, s)
    KHandle(_), _ ->
      throw_type_error_in(
        st,
        realm,
        "Error.prototype.stack value must be a string",
      )
    _, _ ->
      throw_type_error_in(
        st,
        realm,
        "set Error.prototype.stack called on non-object",
      )
  }
}

/// Throw a TypeError of realm `realm` rather than of the current one.
fn throw_type_error_in(st: Agent, realm: Int, message: String) -> #(a, Agent) {
  use st <- rt_realm.with_realm(st, realm)
  rt_val.t_throw_type_error(st, message)
}

/// SetterThatIgnoresPrototypeProperties ( this, home, p, v ).
fn set_stack_ignoring_prototype(
  st: Agent,
  realm: Int,
  proto: Handle,
  h: Handle,
  s: String,
) -> #(JsVal, Agent) {
  case h == proto {
    True ->
      throw_type_error_in(
        st,
        realm,
        "Cannot assign to read only property 'stack' of Error.prototype",
      )
    False -> {
      // Step 3: desc = ? this.[[GetOwnProperty]]("stack") — proxy-aware.
      let #(own, st) =
        rt_obj.t_get_own_property(st, h, StringKey(Named("stack")))
      let #(ok, st) = case option.is_some(own) {
        // Step 5: Set(this, "stack", v, true) — false → TypeError.
        True ->
          rt_obj.t_set_prop(
            st,
            mk_object(h),
            StringKey(Named("stack")),
            mk_string(s),
          )
        // Step 4: CreateDataPropertyOrThrow(this, "stack", v) — {W:T,E:T,C:T}.
        False ->
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
      }
      case ok {
        True -> #(mk_undefined(), st)
        False ->
          throw_type_error_in(
            st,
            realm,
            "Cannot assign to read only property 'stack'",
          )
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
