import arc/bytecode/error_kind.{
  type ErrorKind, EvalError, RangeError, ReferenceError, SyntaxError, TypeError,
  UriError,
}
import arc/rt/builtins/common
import arc/rt/builtins/error as b_error
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type ObjKind, type Realm, BigIntObj,
  BooleanObj, KBig, KBool, KHandle, KNull, KNum, KStr, KSym, KTdz, KUndef,
  NumberObj, StringObj, SymbolObj, classify, mk_bool, mk_object, mk_string,
  plain_object,
}
import arc/rt/val as rt_val
import gleam/dict
import gleam/option.{Some}

fn error_kind_prototype(r: Realm, kind: ErrorKind) -> Handle {
  case kind {
    TypeError -> r.type_error.prototype
    RangeError -> r.range_error.prototype
    ReferenceError -> r.reference_error.prototype
    SyntaxError -> r.syntax_error.prototype
    UriError -> r.uri_error.prototype
    EvalError -> r.eval_error.prototype
  }
}

pub fn new_error(
  st: Agent,
  kind: ErrorKind,
  message: String,
) -> #(JsVal, Agent) {
  let proto = error_kind_prototype(st.realm, kind)
  let name = error_kind.name(kind)
  let #(msg_prop, st) = rt_store.builtin_property(st, mk_string(message))
  let #(h, st) = common.alloc_error_object(st, proto, [#("message", msg_prop)])
  let st = b_error.attach_stack(st, h, name, message)
  #(mk_object(h), st)
}

pub fn alloc_object(
  st: Agent,
  kind: ObjKind,
  proto: Handle,
) -> #(Handle, Agent) {
  rt_store.cell_new(st, plain_object(kind, Some(proto), dict.new()))
}

// §7.1.18 toobject
pub fn wrap_primitive(st: Agent, v: JsVal) -> #(Handle, Agent) {
  case classify(v) {
    KHandle(h) -> #(h, st)
    KStr(s) -> alloc_object(st, StringObj(s), st.realm.string.prototype)
    KNum(n) -> alloc_object(st, NumberObj(n), st.realm.number.prototype)
    KBool(b) -> alloc_object(st, BooleanObj(b), st.realm.boolean.prototype)
    KSym(id) -> alloc_object(st, SymbolObj(id), st.realm.symbol.prototype)
    KBig(n) -> alloc_object(st, BigIntObj(n), st.realm.bigint.prototype)
    KUndef | KNull ->
      rt_val.throw_type_error(st, "Cannot convert undefined or null to object")
    KTdz -> panic as "wrap_primitive: TDZ sentinel escaped into a JsVal"
  }
}

pub fn alloc_iter_result(
  st: Agent,
  value: JsVal,
  done done: Bool,
) -> #(JsVal, Agent) {
  let #(h, st) =
    common.alloc_plain_object(st, st.realm.object.prototype, [
      #("value", value),
      #("done", mk_bool(done)),
    ])
  #(mk_object(h), st)
}

pub fn alloc_array(st: Agent, values: List(JsVal)) -> #(Handle, Agent) {
  common.alloc_array(st, values, st.realm.array.prototype)
}

pub fn new_array(st: Agent, values: List(JsVal)) -> #(JsVal, Agent) {
  let #(h, st) = alloc_array(st, values)
  #(mk_object(h), st)
}
