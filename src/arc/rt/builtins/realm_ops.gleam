import arc/rt/builtins/common
import arc/rt/builtins/error as b_error
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type ErrorKind, type Handle, type JsVal,
  type ObjKind, type Realm, BigIntObj, BooleanObj, KBig, KBool, KHandle, KNull,
  KNum, KStr, KSym, KTdz, KUndef, NoElements, NumberObj, RangeErr, ReferenceErr,
  SObject, StringObj, SymbolObj, SyntaxErr, TypeErr, classify, mk_bool,
  mk_object, mk_string,
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

pub fn alloc_wrapper(
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
    KStr(s) -> alloc_wrapper(st, StringObj(s), st.realm.string.prototype)
    KNum(n) -> alloc_wrapper(st, NumberObj(n), st.realm.number.prototype)
    KBool(b) -> alloc_wrapper(st, BooleanObj(b), st.realm.boolean.prototype)
    KSym(id) -> alloc_wrapper(st, SymbolObj(id), st.realm.symbol.prototype)
    KBig(n) -> alloc_wrapper(st, BigIntObj(n), st.realm.bigint.prototype)
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
  let r = st.realm
  let #(h, st) =
    common.alloc_pojo(st, r.object.prototype, [
      #("value", value),
      #("done", mk_bool(done)),
    ])
  #(mk_object(h), st)
}

pub fn alloc_array(st: Agent, values: List(JsVal)) -> #(Handle, Agent) {
  common.alloc_array(st, values, st.realm.array.prototype)
}

fn pair(bt: BuiltinPair) -> List(Handle) {
  [bt.prototype, bt.constructor]
}

// must stay exhaustive over the realm record fields
pub fn realm_handles(r: Realm) -> List(Handle) {
  let ta =
    dict.fold(r.typed_arrays.by_kind, [], fn(acc, _k, bt) {
      [bt.prototype, bt.constructor, ..acc]
    })
  [
    pair(r.object),
    pair(r.function),
    pair(r.array),
    pair(r.string),
    pair(r.number),
    pair(r.boolean),
    pair(r.symbol),
    pair(r.bigint),
    pair(r.error),
    pair(r.type_error),
    pair(r.reference_error),
    pair(r.range_error),
    pair(r.syntax_error),
    pair(r.eval_error),
    pair(r.uri_error),
    pair(r.aggregate_error),
    pair(r.suppressed_error),
    pair(r.map),
    pair(r.set),
    pair(r.weak_map),
    pair(r.weak_set),
    pair(r.date),
    pair(r.regexp),
    pair(r.promise),
    pair(r.proxy),
    pair(r.array_buffer),
    pair(r.shared_array_buffer),
    pair(r.data_view),
    pair(r.iterator),
    pair(r.generator),
    pair(r.generator_fn),
    pair(r.async_fn),
    pair(r.async_gen),
    ta,
    [
      r.math,
      r.json,
      r.reflect,
      r.console,
      r.atomics,
      r.iterator_proto,
      r.array_iter_proto,
      r.string_iter_proto,
      r.map_iter_proto,
      r.set_iter_proto,
      r.async_iterator_proto,
      r.async_from_sync_proto,
      r.iterator_helper_proto,
      r.wrap_for_valid_proto,
      r.throw_type_error,
      r.global_object,
    ],
  ]
  |> flatten([])
}

fn flatten(lists: List(List(a)), acc: List(a)) -> List(a) {
  case lists {
    [] -> acc
    [l, ..rest] -> flatten(rest, prepend(l, acc))
  }
}

fn prepend(l: List(a), acc: List(a)) -> List(a) {
  case l {
    [] -> acc
    [x, ..rest] -> prepend(rest, [x, ..acc])
  }
}
