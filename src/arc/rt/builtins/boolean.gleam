//// `rt_builtins/boolean` — Boolean constructor + %Boolean.prototype%
//// (ES2024 §20.3). Port of `arc/vm/builtins/boolean.gleam` over the
//// threaded `Agent` model (D7/R1).

import arc/rt/builtins/common
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BooleanNative, type BuiltinPair, type Handle, type JsVal,
  BooleanConstructor, BooleanN, BooleanObj, BooleanPrototypeToString,
  BooleanPrototypeValueOf, KBool, KHandle, SObject, classify, mk_bool, mk_string,
}
import arc/rt/val as rt_val

/// Set up Boolean constructor + Boolean.prototype (§20.3.2 / §20.3.3).
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
  // §20.3.3: the Boolean prototype object is itself a Boolean object with
  // [[BooleanData]] = false — hence init_wrapper_type.
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

/// Per-module dispatch for Boolean native functions.
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

/// §20.3.1.1 Boolean(value) called as a function. Step 1: b = ToBoolean(value);
/// step 2: NewTarget undefined → return b. `new Boolean` is intercepted in
/// `t_construct` before dispatch reaches here.
fn call_as_function(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let b = case args {
    [] -> False
    [v, ..] -> rt_val.to_boolean(v)
  }
  #(mk_bool(b), st)
}

/// §20.3.3.3 Boolean.prototype.valueOf ( ) — ? thisBooleanValue(this).
fn boolean_value_of(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  #(mk_bool(this_boolean_value(st, this, "valueOf")), st)
}

/// §20.3.3.2 Boolean.prototype.toString ( ) — ? thisBooleanValue(this),
/// then "true"/"false".
fn boolean_to_string(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  case this_boolean_value(st, this, "toString") {
    True -> #(mk_string("true"), st)
    False -> #(mk_string("false"), st)
  }
}

/// §20.3.3.1 thisBooleanValue(value): a Boolean primitive, or a Boolean
/// wrapper object's [[BooleanData]]; anything else → TypeError.
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
