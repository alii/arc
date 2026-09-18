import arc/rt/builtins/common
import arc/rt/builtins/error as b_error
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type ErrorKind, type Handle, type JsVal, type ObjKind, type Realm,
  BigIntObj, BooleanObj, KBig, KBool, KHandle, KNull, KNum, KStr, KSym, KTdz,
  KUndef, NoElements, NumberObj, RangeErr, ReferenceErr, SObject, StringObj,
  SymbolObj, SyntaxErr, TypeErr, classify, mk_bool, mk_object, mk_string,
}
import arc/rt/val as rt_val
import gleam/dict
import gleam/option.{Some}

pub fn error_kind_intrinsics(r: Realm, kind: ErrorKind) -> #(Handle, String) {
  case kind {
    TypeErr -> #(r.type_error.prototype, "TypeError")
    RangeErr -> #(r.range_error.prototype, "RangeError")
    ReferenceErr -> #(r.reference_error.prototype, "ReferenceError")
    SyntaxErr -> #(r.syntax_error.prototype, "SyntaxError")
  }
}

pub fn t_new_error(
  st: Agent,
  kind: ErrorKind,
  message: String,
) -> #(JsVal, Agent) {
  let #(proto, name) = error_kind_intrinsics(st.realm, kind)
  let #(msg_prop, st) = common.builtin_property(st, mk_string(message))
  let #(h, st) = common.alloc_error_slot(st, proto, [#("message", msg_prop)])
  let st = b_error.attach_stack(st, h, name, message)
  #(mk_object(h), st)
}

pub fn alloc_object(
  st: Agent,
  kind: ObjKind,
  proto: Handle,
) -> #(Handle, Agent) {
  rt_store.t_cell_new(
    st,
    SObject(
      kind:,
      proto: Some(proto),
      props: dict.new(),
      symbol_props: [],
      elements: NoElements,
      extensible: True,
    ),
  )
}

// §7.1.18 toobject
pub fn t_box_primitive(st: Agent, v: JsVal) -> #(Handle, Agent) {
  case classify(v) {
    KHandle(h) -> #(h, st)
    KStr(s) -> alloc_object(st, StringObj(s), st.realm.string.prototype)
    KNum(n) -> alloc_object(st, NumberObj(n), st.realm.number.prototype)
    KBool(b) -> alloc_object(st, BooleanObj(b), st.realm.boolean.prototype)
    KSym(id) -> alloc_object(st, SymbolObj(id), st.realm.symbol.prototype)
    KBig(n) -> alloc_object(st, BigIntObj(n), st.realm.bigint.prototype)
    KUndef | KNull ->
      rt_val.t_throw_type_error(
        st,
        "Cannot convert undefined or null to object",
      )
    KTdz -> panic as "t_box_primitive: TDZ sentinel escaped into a JsVal"
  }
}

pub fn alloc_iter_result(
  st: Agent,
  value: JsVal,
  done: Bool,
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
