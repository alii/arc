import arc/rt/builtins/common
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BooleanNative, type BuiltinPair, type Handle, type JsVal,
  BooleanConstructor, BooleanN, BooleanObj, BooleanPrototypeToString,
  BooleanPrototypeValueOf, KBool, KHandle, SObject, classify, mk_bool, mk_string,
}
import arc/rt/val as rt_val

pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
) -> #(BuiltinPair, Agent) {
  let #(proto_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("valueOf", BooleanN(BooleanPrototypeValueOf), 0),
      #("toString", BooleanN(BooleanPrototypeToString), 0),
    ])
  common.init_wrapper_type(
    st,
    object_proto,
    fn_proto,
    proto_methods,
    fn(_) { BooleanN(BooleanConstructor) },
    "Boolean",
    1,
    [],
    proto_kind: BooleanObj(value: False),
  )
}

pub fn dispatch(
  st: Agent,
  native: BooleanNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    BooleanConstructor -> call_as_function(st, args)
    BooleanPrototypeValueOf -> boolean_value_of(st, this)
    BooleanPrototypeToString -> boolean_to_string(st, this)
  }
}

fn call_as_function(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let b = case args {
    [] -> False
    [v, ..] -> rt_val.to_boolean(v)
  }
  #(mk_bool(b), st)
}

fn boolean_value_of(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  #(mk_bool(this_boolean_value(st, this, "valueOf")), st)
}

fn boolean_to_string(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  case this_boolean_value(st, this, "toString") {
    True -> #(mk_string("true"), st)
    False -> #(mk_string("false"), st)
  }
}

fn this_boolean_value(st: Agent, this: JsVal, method: String) -> Bool {
  case classify(this) {
    KBool(b) -> b
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: BooleanObj(value: b), ..) -> b
        _ -> not_a_boolean(st, method)
      }
    _ -> not_a_boolean(st, method)
  }
}

fn not_a_boolean(st: Agent, method: String) -> a {
  rt_val.t_throw_type_error(
    st,
    "Boolean.prototype." <> method <> " requires that 'this' be a Boolean",
  )
}
