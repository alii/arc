import arc/bytecode/key.{Named}
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
  JInt, JNan, JNegInf, JPosInf, KHandle, KNull, KNum, KStr, KUndef, ParsedDesc,
  SObject, StringKey, SuppressedErrorConstructor, classify, mk_bool, mk_number,
  mk_object, mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

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

pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
  realm: Int,
) -> #(ErrorFamily, Agent) {
  let #(to_string_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("toString", ErrorN(ErrorPrototypeToString), 0),
    ])
  let #(capture_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("captureStackTrace", ErrorN(ErrorCaptureStackTrace), 2),
      #("isError", ErrorN(ErrorIsError), 1),
    ])
  let #(stl_prop, st) = common.builtin_property(st, mk_number(JFloat(10.0)))
  let error_static = [#("stackTraceLimit", stl_prop), ..capture_methods]
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
  let #(type_error, st) = subclass(st, error, "TypeError", 1, ErrorConstructor)
  let #(reference_error, st) =
    subclass(st, error, "ReferenceError", 1, ErrorConstructor)
  let #(range_error, st) =
    subclass(st, error, "RangeError", 1, ErrorConstructor)
  let #(syntax_error, st) =
    subclass(st, error, "SyntaxError", 1, ErrorConstructor)
  let #(eval_error, st) = subclass(st, error, "EvalError", 1, ErrorConstructor)
  let #(uri_error, st) = subclass(st, error, "URIError", 1, ErrorConstructor)
  let #(aggregate_error, st) =
    subclass(st, error, "AggregateError", 2, AggregateErrorConstructor)
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

fn call_error_ctor(
  st: Agent,
  fallback_proto: Handle,
  args: List(JsVal),
  new_target: JsVal,
) -> #(JsVal, Agent) {
  let #(message, options) = helpers.two_args_or_undefined(args)
  let #(proto, st) = proto_from_new_target(st, new_target, fallback_proto)
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
      // tostring(message) runs before reading cause
      let #(msg, st) = rt_val.t_to_string(st, message)
      let #(h, st) = alloc_error(st, proto, Some(msg), options)
      #(mk_object(h), st)
    }
  }
}

fn aggregate_error_ctor(
  st: Agent,
  fallback_proto: Handle,
  args: List(JsVal),
  new_target: JsVal,
) -> #(JsVal, Agent) {
  let #(errors, message, options) = helpers.three_args_or_undefined(args)
  let #(proto, st) = proto_from_new_target(st, new_target, fallback_proto)
  let #(h, st) = case classify(message) {
    KUndef -> alloc_error(st, proto, None, options)
    _ -> {
      let #(msg, st) = rt_val.t_to_string(st, message)
      alloc_error(st, proto, Some(msg), options)
    }
  }
  let #(rec, st) = iter_protocol.get_iterator_sync(st, errors)
  let #(collected, st) = iter_protocol.iterator_to_list(st, rec)
  let #(arr_h, st) = common.alloc_array(st, collected, st.realm.array.prototype)
  let #(errors_prop, st) = common.builtin_property(st, mk_object(arr_h))
  let st = common.add_named_property(st, h, "errors", errors_prop)
  #(mk_object(h), st)
}

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
  alloc_suppressed(st, proto, msg_opt, err, suppressed)
}

fn alloc_suppressed(
  st: Agent,
  proto: Handle,
  message: Option(String),
  err: JsVal,
  suppressed: JsVal,
) -> #(JsVal, Agent) {
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

// maps home to the same error kind in realm
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

fn error_name(st: Agent, proto: Option(Handle), fuel: Int) -> String {
  case proto {
    Some(h) if fuel > 0 ->
      case rt_obj.as_sobject(rt_store.t_cell_get(st, h)) {
        SObject(proto: parent, ..) ->
          case rt_obj.t_ordinary_own_property(st, h, StringKey(Named("name"))) {
            Some(DataProperty(value: v, ..)) ->
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

const default_stack_limit = 10

fn build_stack_trace(st: Agent, header: String) -> String {
  let frames = list.take(st.frames, stack_trace_limit(st))
  case list.map(frames, format_frame) {
    [] -> header
    lines -> header <> "\n" <> string.join(lines, "\n")
  }
}

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

fn stack_trace_limit(st: Agent) -> Int {
  let ctor = rt_store.t_cell_get(st, st.realm.error.constructor)
  case rt_obj.as_sobject(ctor) {
    SObject(props:, ..) ->
      case dict.get(props, Named("stackTraceLimit")) {
        Ok(DataProperty(value: v, ..)) ->
          case classify(v) {
            KNum(JInt(n)) -> int.max(0, n)
            KNum(JFloat(f)) -> int.max(0, float.truncate(f))
            // effectively unbounded
            KNum(JPosInf) -> 1_000_000
            KNum(JNegInf) | KNum(JNan) -> 0
            _ -> default_stack_limit
          }
        _ -> default_stack_limit
      }
    _ -> default_stack_limit
  }
}

pub fn attach_stack(st: Agent, h: Handle, name: String, msg: String) -> Agent {
  let header = case msg {
    "" -> name
    _ -> name <> ": " <> msg
  }
  let trace = build_stack_trace(st, header)
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

fn target_header_parts(st: Agent, h: Handle) -> #(String, String) {
  let read = fn(key) {
    case rt_obj.as_sobject(rt_store.t_cell_get(st, h)) {
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

// throws in the setter's realm, property ops in the caller's
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

fn throw_type_error_in(st: Agent, realm: Int, message: String) -> #(a, Agent) {
  use st <- rt_realm.with_realm(st, realm)
  rt_val.t_throw_type_error(st, message)
}

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
      let #(own, st) =
        rt_obj.t_get_own_property(st, h, StringKey(Named("stack")))
      let #(ok, st) = case option.is_some(own) {
        True ->
          rt_obj.t_set_prop(
            st,
            mk_object(h),
            StringKey(Named("stack")),
            mk_string(s),
          )
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

// §20.5.3.4
fn error_to_string(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  case classify(this) {
    KNull | KUndef ->
      rt_val.t_throw_type_error(
        st,
        "Error.prototype.toString called on non-object",
      )
    KHandle(_) -> {
      let #(name_val, st) =
        rt_obj.t_get_prop(st, this, StringKey(Named("name")))
      let #(name, st) = case classify(name_val) {
        KUndef -> #("Error", st)
        _ -> rt_val.t_to_string(st, name_val)
      }
      let #(msg_val, st) =
        rt_obj.t_get_prop(st, this, StringKey(Named("message")))
      let #(msg, st) = case classify(msg_val) {
        KUndef -> #("", st)
        _ -> rt_val.t_to_string(st, msg_val)
      }
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
