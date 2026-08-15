//// `rt_builtins/realm_ops` — realm-aware allocators + the concrete `JsOps`
//// bodies (SPEC §7.M6 common-and-scaffold(3)).
////
//// Port of `arc/vm/builtins/common.gleam:1044-1200` (`make_error`/`to_object`
//// / `alloc_array` / `create_iter_result`) re-expressed over the threaded
//// `Agent` model. Realm access is via `st.realm` (R15).
////
//// **Return-tuple order is `#(V, St')` — value FIRST (R1).** Undefined/null
//// paths in `t_box_primitive` RAISE via `rt_val.t_throw_type_error` (D7).

import arc/rt/builtins/common
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

// ── error allocation (arc common.gleam:1044-1091) ───────────────────────────

/// The prototype intrinsic + the `name` header for an ErrorKind. The single
/// place the pairing exists so intrinsic and stack-trace name cannot disagree.
pub fn error_kind_intrinsics(r: Realm, kind: ErrorKind) -> #(Handle, String) {
  case kind {
    TypeErr -> #(r.type_error.prototype, "TypeError")
    RangeErr -> #(r.range_error.prototype, "RangeError")
    ReferenceErr -> #(r.reference_error.prototype, "ReferenceError")
    SyntaxErr -> #(r.syntax_error.prototype, "SyntaxError")
  }
}

/// §20.5.6.1.1 NativeError(message) — allocate a native error instance of
/// `kind` with a `message` own property `{W:T, E:F, C:T}`. The concrete body
/// seeded into `JsOps.new_error` by M6 `init_realm`. arc `make_error`.
pub fn t_new_error(
  st: Agent,
  kind: ErrorKind,
  message: String,
) -> #(JsVal, Agent) {
  let #(proto, _name) = error_kind_intrinsics(st.realm, kind)
  let #(msg_prop, st) = common.builtin_property(st, mk_string(message))
  let #(h, st) = common.alloc_error_slot(st, proto, [#("message", msg_prop)])
  #(mk_object(h), st)
}

// ── ToObject / primitive boxing (arc common.gleam:1116-1171) ────────────────

/// Allocate a wrapper object for a primitive: an ordinary object with `kind`
/// carrying the type-specific internal slot. arc `alloc_wrapper`.
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

/// §7.1.18 ToObject — the concrete body seeded into `JsOps.to_object`.
/// Object → identity; primitive → wrapper cell with the realm's matching
/// prototype; undefined/null → RAISE TypeError; TDZ → engine panic.
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

// ── realm-aware convenience allocators ──────────────────────────────────────

/// §7.4.11 CreateIterResultObject(value, done) — allocates `{value, done}`
/// with the realm's `%Object.prototype%`. arc `create_iter_result`.
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

/// §10.4.2.2 ArrayCreate — a fresh dense JS array holding `values` with the
/// realm's `%Array.prototype%`. Thin wrapper on `common.alloc_array`.
pub fn alloc_array(st: Agent, values: List(JsVal)) -> #(Handle, Agent) {
  common.alloc_array(st, values, st.realm.array.prototype)
}

// ── GC pinning enumeration ──────────────────────────────────────────────────

fn pair(bt: BuiltinPair) -> List(Handle) {
  [bt.prototype, bt.constructor]
}

/// Every `Handle` reachable from a `Realm` record — a flat enumeration for M6
/// `init_realm` to `t_pin_root` in one pass, and for M2 GC's realm-root walk.
/// Exhaustive over the `Realm` record's fields (rt_types.gleam:860-906).
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
