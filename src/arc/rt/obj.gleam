//// `rt_obj` — object allocation + property MOP (SPEC §7.M4).
////
//// Port of `arc/vm/ops/object.gleam` OrdinaryGet/Set/Has/Delete +
//// `arc/vm/ops/mop.gleam` [[DefineOwnProperty]]/[[OwnPropertyKeys]]/
//// [[SetPrototypeOf]], re-expressed over the threaded `Agent` and
//// `rt_store` cell ops.
////
//// **Return-tuple order is `#(V, St')` — value FIRST (R1).**
////
//// **D7:** ops that throw JS errors RAISE via `rt_store.t_throw(st, err)`
//// (never `Result`) — the catching frame's threaded store already contains
//// the allocated Error object.
////
//// **D17:** NO import of `rt_call` (cycle — it imports us). Accessor
//// getter/setter invocation reaches `t_call_checked` through
//// `require_js(st).ops.call(st, callee, this, args)`; `init_realm` (M6
//// step 1) seeds the concrete fn. Primitive auto-boxing likewise goes
//// through `ops.to_object`.

import arc/rt/buffer
import arc/rt/elements
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsElements, type JsOps, type JsSlot,
  type JsStore, type JsVal, type ObjKind, type ObjectKey, type ParsedDesc,
  type Property, type PropertyKey, type SymbolId, type TypedArrayKind,
  AccessorProperty, Agent, ArgumentsObj, ArrayObj, DataProperty, Dense, Index,
  JsStore, KHandle, KNull, KTdz, KUndef, ModuleNamespace, Named, NoElements,
  Ordinary, ParsedDesc, Private, ProxyObj, SAsyncContext, SAsyncGen, SBox,
  SGenerator, SObject, SPromiseData, SShapedObject, ShapeDesc, StringKey,
  StringObj, SymbolKey, TypeErr, TypedArrayObj,
} as rt_types
import arc/rt/val as rt_val
import arc/vm/internal/tree_array
import arc/vm/js_string
import arc/vm/limits
import gleam/bit_array
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set
import gleam/string

// ── private access / throw helpers (u-skeleton-access) ──────────────────────

fn require_js(st: Agent) -> JsStore(Agent) {
  st.store
}

/// The seeded `JsOps` upcall table (D17). `init_realm` fills this before any
/// user code runs; unseeded stubs panic (`rt_store.unseeded_ops`).
fn js_ops(st: Agent) -> JsOps(Agent) {
  require_js(st).ops
}

/// Allocate a `TypeError(msg)` via the seeded `ops.new_error` and RAISE it
/// (D7 — never `Result`). Return type is universally quantified: this fn
/// never returns. Port of arc `state.type_error_op` re-expressed under D7.
fn throw_type_error(st: Agent, msg: String) -> a {
  let #(e, st) = js_ops(st).new_error(st, TypeErr, msg)
  rt_store.t_throw(st, e)
}

/// Read the object cell backing property MOP for `h`: an `SObject`, or an
/// `SShapedObject` returned as-is (hot-path callers handle it via
/// `own_property_shaped`; write-path callers `devolve` first, avoiding the
/// `as_sobject` dict.fold rebuild). Data cells (`SBox`, promise/generator
/// state, async contexts) are never a JS receiver.
fn read_object(st: Agent, h: Handle) -> JsSlot {
  case rt_store.t_cell_get(st, h) {
    SObject(..) as obj -> obj
    SShapedObject(..) as s -> s
    SBox(..)
    | SPromiseData(..)
    | SGenerator(..)
    | SAsyncGen(..)
    | SAsyncContext(..) ->
      panic as "rt_obj: internal data cell used as JS receiver (engine invariant)"
  }
}

/// Direct own-property lookup on an `SShapedObject` — the shaped-slot arm
/// avoiding `as_sobject`'s dict.fold rebuild. Shape keys are utf8 strings
/// only (no symbols/private); a miss falls through to `None` (proto walk).
fn own_property_shaped(
  st: Agent,
  shape_id: Int,
  slots: rt_types.ShapeSlots,
  key: PropertyKey,
) -> Option(Property) {
  case key {
    Private(_) -> None
    _ ->
      case dict.get(require_js(st).shapes, shape_id) {
        Ok(ShapeDesc(offsets:, ..)) ->
          case
            dict.get(offsets, bit_array.from_string(rt_types.key_to_text(key)))
          {
            Ok(off) ->
              Some(DataProperty(
                value: rt_types.shape_slots_get(slots, off),
                writable: True,
                enumerable: True,
                configurable: True,
                seq: off,
              ))
            Error(Nil) -> None
          }
        Error(Nil) -> None
      }
  }
}

/// `(own_property, proto)` for `h` under `key` — hot-path combining of
/// `read_object` + `own_property_of` with a direct `SShapedObject` arm.
fn read_own_and_proto(
  st: Agent,
  h: Handle,
  key: ObjectKey,
) -> #(Option(Property), Option(Handle)) {
  own_and_proto_of_slot(st, read_object(st, h), key)
}

/// `read_own_and_proto` on an already-read cell.
fn own_and_proto_of_slot(
  st: Agent,
  slot: JsSlot,
  key: ObjectKey,
) -> #(Option(Property), Option(Handle)) {
  case slot {
    SShapedObject(shape_id:, proto:, slots:) -> #(
      case key {
        StringKey(pk) -> own_property_shaped(st, shape_id, slots, pk)
        SymbolKey(_) -> None
      },
      proto,
    )
    SObject(kind:, proto:, props:, symbol_props:, elements:, ..) -> #(
      case key {
        StringKey(pk) -> own_property_of(st, kind, props, elements, pk)
        SymbolKey(sym) -> own_symbol_property_of(symbol_props, sym)
      },
      proto,
    )
    // read_object only returns SObject | SShapedObject.
    _ -> #(None, None)
  }
}

/// True when `key` is a canonical numeric index string on a TypedArray cell
/// — such keys are fully resolved by the integer-indexed exotic behaviour
/// (§10.4.5) and must never fall through to the prototype chain.
fn typed_array_absorbs(slot: JsSlot, key: ObjectKey) -> Bool {
  case slot, key {
    SObject(kind: TypedArrayObj(..), ..), StringKey(Index(_)) -> True
    SObject(kind: TypedArrayObj(..), ..), StringKey(Named(s)) ->
      buffer.is_canonical_numeric_string(s)
    _, _ -> False
  }
}

/// Materialize an `SShapedObject` as a plain `SObject` — rebuild the props
/// Dict from `ShapeDesc.offsets` + the slot array. Passthrough otherwise.
/// Slow-path READ helper (h-shape-slowpath-compat).
pub fn as_sobject(st: Agent, slot: JsSlot) -> JsSlot {
  case slot {
    SShapedObject(shape_id:, proto:, slots:) -> {
      let props = case dict.get(require_js(st).shapes, shape_id) {
        Ok(ShapeDesc(offsets:, ..)) ->
          dict.fold(offsets, dict.new(), fn(acc, key_bin, off) {
            let value = rt_types.shape_slots_get(slots, off)
            let key = case bit_array.to_string(key_bin) {
              Ok(s) -> rt_types.canonical_key(s)
              // shape keys are utf8 by construction
              Error(Nil) -> Named("")
            }
            dict.insert(
              acc,
              key,
              DataProperty(
                value:,
                writable: True,
                enumerable: True,
                configurable: True,
                seq: off,
              ),
            )
          })
        Error(Nil) -> dict.new()
      }
      SObject(
        kind: Ordinary,
        proto:,
        props:,
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      )
    }
    _ -> slot
  }
}

/// Devolve a shaped cell in-place to a plain `SObject`. No-op on non-shaped
/// cells. WRITE-path helper: define/delete/setProto/preventExtensions call
/// this before their `t_cell_update` so the closure sees a real `SObject`.
pub fn devolve(st: Agent, h: Handle) -> Agent {
  case rt_store.t_cell_get(st, h) {
    SShapedObject(..) as s -> rt_store.t_cell_set(st, h, as_sobject(st, s))
    _ -> st
  }
}

// ── private own-property / same_value helpers ───────────────────────────────

/// A fresh `{value, W:T, E:T, C:T}` data property with a threaded creation
/// seq — port of arc `value.data_property` (arc uses a global counter; we
/// thread it, so this returns `#(Property, St')`).
fn new_data_property(st: Agent, v: JsVal) -> #(Property, Agent) {
  let #(seq, st) = rt_store.t_next_prop_seq(st)
  #(
    DataProperty(
      value: v,
      writable: True,
      enumerable: True,
      configurable: True,
      seq:,
    ),
    st,
  )
}

/// **[[GetOwnProperty]](P)** on an already-read `SObject` — port of arc
/// `own_property_of_slot` (ordinary + Array/Arguments-index arms). Properties
/// dict is authoritative (holds accessor/attribute overrides); dense elements
/// is the fast-path data-value cache — check dict FIRST (arc invariant
/// `object.gleam:436-592`).
fn own_property_of(
  st: Agent,
  kind: ObjKind,
  props: Dict(PropertyKey, Property),
  elements: JsElements,
  key: PropertyKey,
) -> Option(Property) {
  case kind, key {
    // TypedArray (Integer-Indexed) exotic [[GetOwnProperty]] (§10.4.5.1):
    // canonical numeric index keys map to buffer elements — in-bounds →
    // { value, W:T, E:T, C:T }; out-of-bounds/detached → undefined WITHOUT
    // consulting the ordinary table. Non-integral canonical numeric strings
    // ("1.5", "-0", "NaN", …) are never valid indices, so they also yield
    // undefined. Immutable ArrayBuffer proposal
    // (sec-typedarray-getownproperty): over an immutable buffer the element
    // descriptor is { value, W:F, E:T, C:F }.
    TypedArrayObj(buffer: buf, elem_kind:, byte_offset:, length:), Index(idx) ->
      buffer.typed_array_element_live(
        st,
        buf,
        elem_kind,
        byte_offset,
        length,
        idx,
      )
      |> option.map(fn(v) {
        // seq: 0 — Index keys enumerate numerically, never by seq.
        case buffer.buffer_is_immutable(st, buf) {
          True ->
            DataProperty(
              value: v,
              writable: False,
              enumerable: True,
              configurable: False,
              seq: 0,
            )
          False ->
            DataProperty(
              value: v,
              writable: True,
              enumerable: True,
              configurable: True,
              seq: 0,
            )
        }
      })
    TypedArrayObj(..), Named(s) ->
      case buffer.is_canonical_numeric_string(s) {
        True -> None
        False -> dict.get(props, key) |> option.from_result
      }
    // Private elements are ordinary dict entries.
    TypedArrayObj(..), Private(_) -> dict.get(props, key) |> option.from_result
    // Array exotic virtual "length" (§10.4.2): a dict override holds the
    // attributes after defineProperty made it non-writable; the value always
    // tracks `ArrayObj(length)`. seq: 0 — never enumerated by seq.
    ArrayObj(length:), Named("length") ->
      case dict.get(props, key) {
        Ok(DataProperty(writable:, enumerable:, configurable:, ..)) ->
          Some(DataProperty(
            value: rt_types.mk_number(rt_types.JInt(length)),
            writable:,
            enumerable:,
            configurable:,
            seq: 0,
          ))
        _ ->
          Some(DataProperty(
            value: rt_types.mk_number(rt_types.JInt(length)),
            writable: True,
            enumerable: False,
            configurable: False,
            seq: 0,
          ))
      }
    // String exotic (§10.4.3.5 StringGetOwnProperty): "length" and in-range
    // integer index are virtual own props {E:F,C:F}/{E:T,C:F}; everything
    // else falls through to OrdinaryGetOwnProperty. Port of arc
    // `object.gleam:549-575`.
    StringObj(value: s), Named("length") -> Some(string_length_property(s))
    StringObj(value: s), Index(i) ->
      string_index_property(s, i)
      |> option.lazy_or(fn() { dict.get(props, key) |> option.from_result })
    // Array/Arguments Index: dict override wins, else elements store.
    ArrayObj(_), Index(i) | ArgumentsObj(..), Index(i) ->
      case dict.get(props, key) {
        Ok(prop) -> Some(prop)
        Error(Nil) ->
          elements.get_option(elements, i)
          |> option.map(fn(v) {
            // seq: 0 — Index keys enumerate numerically, never by seq.
            DataProperty(
              value: v,
              writable: True,
              enumerable: True,
              configurable: True,
              seq: 0,
            )
          })
      }
    // §10.1.5.1 OrdinaryGetOwnProperty. (Proxy / Module Namespace string
    // keys dispatch in `t_get_own_property` before reaching the slot read.)
    _, _ -> dict.get(props, key) |> option.from_result
  }
}

/// Own symbol-keyed property lookup — `symbol_props` is a creation-ordered
/// association list (arc `object.gleam:1877`).
fn own_symbol_property_of(
  symbol_props: List(#(SymbolId, Property)),
  sym: SymbolId,
) -> Option(Property) {
  list.key_find(symbol_props, sym) |> option.from_result
}

/// §10.4.3.4 StringCreate step 10: a String exotic's "length" is the own data
/// property { [[Value]]: len, W:F, E:F, C:F }. seq: 0 — synthesized, never
/// stored or enumerated through the seq-ordered named-key path.
fn string_length_property(s: String) -> Property {
  DataProperty(
    value: rt_types.mk_number(rt_types.JInt(js_string.length(s))),
    writable: False,
    enumerable: False,
    configurable: False,
    seq: 0,
  )
}

/// §10.4.3.5 StringGetOwnProperty steps 5-10: an in-range integer index
/// yields { [[Value]]: <code unit>, W:F, E:T, C:F }; out of range → None.
fn string_index_property(s: String, i: Int) -> Option(Property) {
  use ch <- option.map(js_string.char_at(s, i))
  DataProperty(
    value: rt_types.mk_string(ch),
    writable: False,
    enumerable: True,
    configurable: False,
    seq: 0,
  )
}

/// §7.2.10 SameValue — like `===`, but `NaN` equals `NaN` and `+0` differs
/// from `-0`. Built on `classify` (D16 — `JsVal` is opaque; no rt_val
/// import per task constraint). Gleam `==` on `Float` compiles to Erlang
/// `=:=`, which distinguishes `0.0` from `-0.0` (OTP 27+).
fn same_value(a: JsVal, b: JsVal) -> Bool {
  case rt_types.classify(a), rt_types.classify(b) {
    rt_types.KNum(x), rt_types.KNum(y) -> num_same_value(x, y)
    ka, kb -> ka == kb
  }
}

fn num_same_value(a: rt_types.JsNum, b: rt_types.JsNum) -> Bool {
  case a, b {
    rt_types.JNan, rt_types.JNan -> True
    rt_types.JInt(x), rt_types.JInt(y) -> x == y
    rt_types.JFloat(x), rt_types.JFloat(y) -> x == y
    // Mixed int/float: normalize the int side. `int.to_float(0) == -0.0` is
    // `0.0 =:= -0.0` → False, so SameValue's ±0 distinction is preserved.
    rt_types.JInt(x), rt_types.JFloat(y) -> int.to_float(x) == y
    rt_types.JFloat(x), rt_types.JInt(y) -> x == int.to_float(y)
    _, _ -> a == b
  }
}

/// §6.2.6.1 IsAccessorDescriptor.
fn desc_is_accessor(d: ParsedDesc) -> Bool {
  option.is_some(d.get) || option.is_some(d.set)
}

/// §6.2.6.2 IsDataDescriptor.
fn desc_is_data(d: ParsedDesc) -> Bool {
  option.is_some(d.value) || option.is_some(d.writable)
}

/// Render an `ObjectKey` for TypeError messages (V8's bare-key text).
fn key_text(key: ObjectKey) -> String {
  case key {
    StringKey(pk) -> rt_types.key_to_text(pk)
    SymbolKey(sym) -> rt_types.symbol_descriptive_string(sym)
  }
}

/// ObjectKey → quoted error-message text, for messages that name the key
/// mid-sentence ("Cannot redefine property: 'x'").
fn key_quoted(key: ObjectKey) -> String {
  case key {
    StringKey(pk) -> "'" <> rt_types.key_to_text(pk) <> "'"
    SymbolKey(_) -> "[symbol]"
  }
}

/// ObjectKey → JS-visible key value (String or Symbol). Used for proxy trap
/// arguments and wherever a key crosses back into JS.
pub fn object_key_value(key: ObjectKey) -> JsVal {
  case key {
    StringKey(pk) -> rt_types.mk_string(rt_types.key_to_text(pk))
    SymbolKey(sym) -> rt_types.mk_symbol(sym)
  }
}

/// JsVal → ObjectKey. `None` for anything that is not a String or a Symbol:
/// §6.1.7 says such a value is not a property key, so no object can have an
/// own property under it.
fn object_key_of_value(v: JsVal) -> Option(ObjectKey) {
  case rt_types.classify(v) {
    rt_types.KStr(s) -> Some(StringKey(rt_types.canonical_key(s)))
    rt_types.KSym(sym) -> Some(SymbolKey(sym))
    _ -> None
  }
}

fn throw_reference_error(st: Agent, msg: String) -> a {
  let #(e, st) = js_ops(st).new_error(st, rt_types.ReferenceErr, msg)
  rt_store.t_throw(st, e)
}

/// Allocate a plain `%Object.prototype%` object carrying `entries` as fresh
/// `{W:T, E:T, C:T}` data properties in list order (FromPropertyDescriptor's
/// result object; the ordinary counterpart of `common.alloc_pojo`, which this
/// module cannot import).
fn alloc_plain(st: Agent, entries: List(#(String, JsVal))) -> #(Handle, Agent) {
  let #(props, st) =
    list.fold(entries, #(dict.new(), st), fn(acc, entry) {
      let #(props, st) = acc
      let #(prop, st) = new_data_property(st, entry.1)
      #(dict.insert(props, Named(entry.0), prop), st)
    })
  rt_store.t_cell_new(
    st,
    SObject(
      kind: Ordinary,
      proto: Some(st.realm.object.prototype),
      props:,
      symbol_props: [],
      elements: NoElements,
      extensible: True,
    ),
  )
}

// ── allocation ──────────────────────────────────────────────────────────────

/// Allocate a fresh ordinary object with the given prototype. Empty props /
/// symbols / elements, `extensible: True`. Port of arc `heap.alloc_object`.
pub fn t_new_object(st: Agent, proto: Option(Handle)) -> #(Handle, Agent) {
  rt_store.t_cell_new(
    st,
    SObject(
      kind: Ordinary,
      proto:,
      props: dict.new(),
      symbol_props: [],
      elements: NoElements,
      extensible: True,
    ),
  )
}

/// SPEC§8 `new_object` op — §13.2.5 OrdinaryObjectCreate(%Object.prototype%).
/// Nullary (mirrors arc `opcode.NewObject`): the emitter cannot spell the
/// realm's prototype handle at IR time, so the default lives here.
pub fn t_new_object_literal(st: Agent) -> #(JsVal, Agent) {
  let #(h, st) = t_new_object(st, Some(st.realm.object.prototype))
  #(rt_types.mk_object(h), st)
}

// ── prototype ops (§10.1.1 / §10.1.2) ───────────────────────────────────────

/// **[[GetPrototypeOf]] ( )** — §10.1.1 for ordinary objects, §10.5.1 for
/// proxies (the `getPrototypeOf` trap). State threaded per R1 shape because
/// the trap may run user code.
pub fn t_get_prototype_of(st: Agent, obj: Handle) -> #(Option(Handle), Agent) {
  case read_object(st, obj) {
    SObject(kind: ProxyObj(target:, handler:, revoked:), ..) ->
      proxy_get_prototype_of(st, Proxy(target:, handler:, revoked:))
    SObject(proto:, ..) | SShapedObject(proto:, ..) -> #(proto, st)
    _ -> #(None, st)
  }
}

/// Why an object's [[SetPrototypeOf]] returned **false**. Callers that must
/// throw (Object.setPrototypeOf) turn each variant into its own TypeError;
/// callers that report a flag (Reflect.setPrototypeOf) collapse them all.
pub type SetProtoFail {
  NotExtensible
  Cyclic
  Immutable
  /// A proxy's `setPrototypeOf` trap returned falsish (§10.5.2 step 8).
  TrapRefused
}

/// The TypeError message Object.setPrototypeOf raises for each refusal.
pub fn set_proto_fail_message(fail: SetProtoFail) -> String {
  case fail {
    NotExtensible -> "Cannot set prototype of a non-extensible object"
    Cyclic -> "Cyclic __proto__ value"
    Immutable -> "Immutable prototype object cannot have its prototype set"
    TrapRefused -> "'setPrototypeOf' on proxy: trap returned falsish"
  }
}

/// **[[SetPrototypeOf]] ( V )** — §10.5.2 for proxies, §10.1.2.1
/// OrdinarySetPrototypeOf otherwise (with the §10.4.7 SetImmutablePrototype
/// check for %Object.prototype%). `Ok(Nil)` on success, `Error(reason)` when
/// rejected. THE single dispatch: `Object.setPrototypeOf`,
/// `Reflect.setPrototypeOf` and `__proto__`'s setter all route through it,
/// so a proxy is never handed to the ordinary algorithm. Port of arc
/// `mop.set_prototype_of_stateful` (`mop.gleam:1242-1327`).
pub fn t_set_prototype_of(
  st: Agent,
  obj: Handle,
  new_proto: Option(Handle),
) -> #(Result(Nil, SetProtoFail), Agent) {
  let st = devolve(st, obj)
  let assert SObject(kind:, proto: current, extensible:, ..) =
    read_object(st, obj)
  use <- proxy_or(kind, fn(p) {
    let #(ok, st) = proxy_set_prototype_of(st, p, new_proto)
    case ok {
      True -> #(Ok(Nil), st)
      False -> #(Error(TrapRefused), st)
    }
  })
  // Step 4: SameValue(V, current) → true (no-op).
  use <- bool.guard(new_proto == current, #(Ok(Nil), st))
  // §10.4.7.2 SetImmutablePrototype — Object.prototype is an Immutable
  // Prototype Exotic Object (§20.1.3): any change is rejected.
  use <- bool.guard(obj == st.realm.object.prototype, #(Error(Immutable), st))
  // Step 5: extensible false → false.
  use <- bool.guard(!extensible, #(Error(NotExtensible), st))
  // Step 7: cycle check.
  use <- bool.guard(would_create_cycle(st, obj, new_proto), #(Error(Cyclic), st))
  // Step 8: set [[Prototype]] to V.
  let st =
    rt_store.t_cell_update(st, obj, fn(slot) {
      let assert SObject(..) = slot
      SObject(..slot, proto: new_proto)
    })
  #(Ok(Nil), st)
}

/// `t_set_prototype_of` collapsed to the spec's Boolean: what
/// Reflect.setPrototypeOf, `__proto__` and a proxy's missing trap report.
pub fn t_set_prototype(
  st: Agent,
  obj: Handle,
  new_proto: Option(Handle),
) -> #(Bool, Agent) {
  let #(res, st) = t_set_prototype_of(st, obj, new_proto)
  case res {
    Ok(Nil) -> #(True, st)
    Error(_refused) -> #(False, st)
  }
}

/// SPEC §8 op-table spelling — thin alias for `t_get_prototype_of`.
pub fn t_get_proto(st: Agent, obj: Handle) -> #(Option(Handle), Agent) {
  t_get_prototype_of(st, obj)
}

/// Annex B §B.3.1 `__proto__: v` in an object literal (step 6.a): an object
/// or null `v` becomes the [[Prototype]]; anything else is ignored.
pub fn t_set_proto(st: Agent, obj: Handle, v: JsVal) -> #(Bool, Agent) {
  case rt_types.classify(v) {
    KHandle(p) -> t_set_prototype(st, obj, Some(p))
    KNull -> t_set_prototype(st, obj, None)
    _ -> #(False, st)
  }
}

/// §10.1.2.1 step 7: walk `new_proto`'s chain; if it reaches `target`, adding
/// the link would form a cycle. Proxies (whose [[GetPrototypeOf]] is a trap)
/// terminate the walk without a cycle (step 7.c.i).
fn would_create_cycle(
  st: Agent,
  target: Handle,
  new_proto: Option(Handle),
) -> Bool {
  case new_proto {
    None -> False
    Some(p) if p == target -> True
    Some(p) ->
      case read_object(st, p) {
        SObject(kind: ProxyObj(..), ..) -> False
        SObject(proto: next, ..) | SShapedObject(proto: next, ..) ->
          would_create_cycle(st, target, next)
        _ -> False
      }
  }
}

// ── [[Get]] (§10.1.8) ───────────────────────────────────────────────────────

/// §7.3.2 GetV / §10.1.8.1 OrdinaryGet — the observable `obj[key]`. Primitive
/// receivers auto-box via `ops.to_object` (D17); `null`/`undefined` throw
/// TypeError (SPEC §7.M4 invariant). Accessors invoke via `ops.call` (D17).
pub fn t_get_prop(st: Agent, recv: JsVal, key: ObjectKey) -> #(JsVal, Agent) {
  case rt_types.classify(recv) {
    KHandle(h) -> get_from(st, h, key, recv)
    KUndef | KNull ->
      throw_type_error(
        st,
        "Cannot read properties of "
          <> case rt_types.classify(recv) {
          KNull -> "null"
          _ -> "undefined"
        }
          <> " (reading '"
          <> key_text(key)
          <> "')",
      )
    // Bool/Num/Str/BigInt/Symbol → box to a wrapper Handle, walk from there
    // with the ORIGINAL primitive as Receiver (so accessor `this` is the
    // primitive, per §10.1.8.1).
    _ -> {
      let #(h, st) = js_ops(st).to_object(st, recv)
      get_from(st, h, key, recv)
    }
  }
}

/// §10.1.8.1 OrdinaryGet(O, P, Receiver) with `O` an object handle. Port of
/// arc `get_value` + `get_symbol_value` (arc `object.gleam:126-296,2872-2903`).
fn get_from(
  st: Agent,
  h: Handle,
  key: ObjectKey,
  receiver: JsVal,
) -> #(JsVal, Agent) {
  case read_object(st, h), key {
    // §10.5.8 Proxy [[Get]] — route through the trap machinery. Private
    // names live in the proxy's own [[PrivateElements]] and never trap.
    SObject(kind: ProxyObj(..), ..) as slot, StringKey(Private(_)) ->
      ordinary_get(st, slot, key, receiver)
    SObject(kind: ProxyObj(target:, handler:, revoked:), ..), _ ->
      proxy_get(st, Proxy(target:, handler:, revoked:), key, receiver)
    // §10.4.6.8 Module Namespace [[Get]]: read the live binding cell, throwing
    // ReferenceError if it is still uninitialized (TDZ). Non-export keys fall
    // through to undefined (null prototype, no inheritance). Symbol keys are
    // OrdinaryGet (step 1).
    SObject(kind: ModuleNamespace(exports:), ..), StringKey(pk) ->
      namespace_get(st, exports, pk)
    // TypedArray exotic [[Get]] (§10.4.5.4): a canonical numeric index is
    // IntegerIndexedElementGet — element value or undefined, never the
    // prototype chain.
    SObject(
      kind: TypedArrayObj(buffer: buf, elem_kind:, byte_offset:, length:),
      ..,
    ),
      StringKey(Index(idx))
    -> #(
      buffer.typed_array_element_live(
        st,
        buf,
        elem_kind,
        byte_offset,
        length,
        idx,
      )
        |> option.unwrap(rt_types.mk_undefined()),
      st,
    )
    SObject(kind: TypedArrayObj(..), ..) as slot, StringKey(Named(s)) ->
      case buffer.is_canonical_numeric_string(s) {
        True -> #(rt_types.mk_undefined(), st)
        False -> ordinary_get(st, slot, key, receiver)
      }
    slot, _ -> ordinary_get(st, slot, key, receiver)
  }
}

/// §10.1.8.1 OrdinaryGet steps 1-3 on an already-read cell.
fn ordinary_get(
  st: Agent,
  slot: JsSlot,
  key: ObjectKey,
  receiver: JsVal,
) -> #(JsVal, Agent) {
  // Step 1: desc = O.[[GetOwnProperty]](P).
  let #(own, proto) = own_and_proto_of_slot(st, slot, key)
  case own {
    // Steps 3-7: found — read value or invoke getter.
    Some(prop) -> property_get_value(st, prop, receiver)
    // Step 2: not own — walk prototype chain.
    None ->
      case proto {
        Some(parent) -> get_from(st, parent, key, receiver)
        None -> #(rt_types.mk_undefined(), st)
      }
  }
}

/// §10.1.8.1 steps 3-7 given a found descriptor: data → `[[Value]]`;
/// accessor → `Call(getter, Receiver)` (D17 upcall) or `undefined`.
fn property_get_value(
  st: Agent,
  prop: Property,
  receiver: JsVal,
) -> #(JsVal, Agent) {
  case prop {
    DataProperty(value: v, ..) -> #(v, st)
    AccessorProperty(get: Some(getter), ..) ->
      js_ops(st).call(st, getter, receiver, [])
    AccessorProperty(get: None, ..) -> #(rt_types.mk_undefined(), st)
  }
}

// ── [[Set]] (§10.1.9) ───────────────────────────────────────────────────────

/// §10.1.9.1 OrdinarySet — the observable `obj[key] = v`. Returns
/// `#(Bool, st')` where `False` means the set was rejected (non-writable,
/// setter-less accessor, non-extensible receiver). Port of arc `set_value` +
/// `set_symbol_value` + `set_property` (arc `object.gleam:606-1670`).
pub fn t_set_prop(
  st: Agent,
  recv: JsVal,
  key: ObjectKey,
  v: JsVal,
) -> #(Bool, Agent) {
  case rt_types.classify(recv) {
    KHandle(h) -> set_from(st, h, key, v, recv)
    KUndef | KNull ->
      throw_type_error(
        st,
        "Cannot set properties of "
          <> case rt_types.classify(recv) {
          KNull -> "null"
          _ -> "undefined"
        }
          <> " (setting '"
          <> key_text(key)
          <> "')",
      )
    // Primitive receiver: box to walk the proto chain for a setter; the
    // Receiver stays the primitive, so the receiver-write step (2.b —
    // "Receiver is not an Object → false") rejects if no setter is found.
    _ -> {
      let #(h, st) = js_ops(st).to_object(st, recv)
      set_from(st, h, key, v, recv)
    }
  }
}

/// §10.1.9.1 + §10.1.9.2 OrdinarySetWithOwnDescriptor.
fn set_from(
  st: Agent,
  h: Handle,
  key: ObjectKey,
  v: JsVal,
  receiver: JsVal,
) -> #(Bool, Agent) {
  case read_object(st, h), key {
    // §10.5.9 Proxy [[Set]] — route through the trap machinery (private
    // names never trap).
    SObject(kind: ProxyObj(..), ..) as slot, StringKey(Private(_)) ->
      ordinary_set(st, slot, key, v, receiver)
    SObject(kind: ProxyObj(target:, handler:, revoked:), ..), _ ->
      proxy_set(st, Proxy(target:, handler:, revoked:), key, v, receiver)
    // §10.4.6.9 Module Namespace [[Set]]: always returns false (read-only).
    SObject(kind: ModuleNamespace(..), ..), _ -> #(False, st)
    // TypedArray exotic [[Set]] (§10.4.5.5). Canonical numeric index,
    // SameValue(O, Receiver) → IntegerIndexedElementSet (§10.4.5.16): convert
    // the value (observable, may call user code), then store if the index is
    // valid; out-of-bounds/detached writes are silent no-ops. Receiver
    // differs from O (Reflect.set / prototype-chain set): step 1.b.ii —
    // invalid index → true with NO value conversion; valid index →
    // OrdinarySet creates the property on the Receiver, leaving the buffer
    // untouched.
    SObject(
      kind: TypedArrayObj(buffer: buf, elem_kind:, byte_offset:, length:),
      ..,
    ),
      StringKey(Index(idx))
    -> {
      let view = buffer.ViewSlot(buffer: buf, elem_kind:, byte_offset:, length:)
      case same_receiver(receiver, h) {
        True -> buffer.typed_array_store(st, view, Some(idx), v)
        False ->
          case
            buffer.typed_array_element_live(
              st,
              buf,
              elem_kind,
              byte_offset,
              length,
              idx,
            )
          {
            None -> #(True, st)
            Some(_) -> set_on_receiver(st, receiver, key, v)
          }
      }
    }
    SObject(
      kind: TypedArrayObj(buffer: buf, elem_kind:, byte_offset:, length:),
      ..,
    ) as slot,
      StringKey(Named(s))
    ->
      case buffer.is_canonical_numeric_string(s) {
        // Canonical numeric but never a valid index ("1.5", "-0", "NaN"):
        // with Receiver == O run the conversion for its side effects, then
        // succeed silently; with a foreign Receiver return true without any
        // conversion (§10.4.5.5 step 1.b.ii).
        True ->
          case same_receiver(receiver, h) {
            True ->
              buffer.typed_array_store(
                st,
                buffer.ViewSlot(buffer: buf, elem_kind:, byte_offset:, length:),
                None,
                v,
              )
            False -> #(True, st)
          }
        False -> ordinary_set(st, slot, key, v, receiver)
      }
    slot, _ -> ordinary_set(st, slot, key, v, receiver)
  }
}

/// SameValue(O, Receiver) for an object `O` at handle `h`.
fn same_receiver(receiver: JsVal, h: Handle) -> Bool {
  case rt_types.classify(receiver) {
    KHandle(r) -> r == h
    _ -> False
  }
}

/// §10.1.9.2 OrdinarySetWithOwnDescriptor on an already-read cell.
fn ordinary_set(
  st: Agent,
  slot: JsSlot,
  key: ObjectKey,
  v: JsVal,
  receiver: JsVal,
) -> #(Bool, Agent) {
  // Step 1: ownDesc = O.[[GetOwnProperty]](P).
  let #(own, proto) = own_and_proto_of_slot(st, slot, key)
  case own {
    // Step 1 (SetWithOwnDescriptor): ownDesc undefined → parent.[[Set]] or
    // fall through to receiver-write.
    None ->
      case proto {
        Some(parent) -> set_from(st, parent, key, v, receiver)
        None -> set_on_receiver(st, receiver, key, v)
      }
    // Step 2.a: non-writable data → false.
    Some(DataProperty(writable: False, ..)) -> #(False, st)
    // Steps 2.b-h: writable data → create/update own on Receiver.
    Some(DataProperty(writable: True, ..)) ->
      set_on_receiver(st, receiver, key, v)
    // Step 5: setter undefined → false.
    Some(AccessorProperty(set: None, ..)) -> #(False, st)
    // Steps 6-7: Call(setter, Receiver, «V»); return true.
    Some(AccessorProperty(set: Some(setter), ..)) -> {
      let #(_, st) = js_ops(st).call(st, setter, receiver, [v])
      #(True, st)
    }
  }
}

/// §10.1.9.2 steps 2.b-h: create/update an own data property on `receiver`.
/// Step 2.b: Receiver not an Object → false.
fn set_on_receiver(
  st: Agent,
  receiver: JsVal,
  key: ObjectKey,
  v: JsVal,
) -> #(Bool, Agent) {
  case rt_types.classify(receiver) {
    KHandle(recv_h) -> {
      case read_object(st, recv_h), key {
        SShapedObject(shape_id:, proto:, slots:), StringKey(Named(name)) ->
          set_own_shaped(st, recv_h, shape_id, proto, slots, name, v)
        // §10.1.9.2 steps 2.c-2.e with a PROXY receiver (Reflect.set with a
        // proxy receiver, or [[Set]] forwarded through a trapless proxy):
        // the GetOwnProperty/DefineOwnProperty pair must go through traps.
        SObject(kind: ProxyObj(..), ..), StringKey(Named(_))
        | SObject(kind: ProxyObj(..), ..), StringKey(Index(_))
        | SObject(kind: ProxyObj(..), ..), SymbolKey(_)
        -> set_on_proxy_receiver(st, recv_h, key, v)
        // §10.1.9.2 step 2.c: Receiver.[[GetOwnProperty]](P). For a module
        // namespace this performs [[Get]] on the binding, which throws a
        // ReferenceError when the export is still in TDZ. The set never
        // succeeds (namespaces aren't extensible and exports reject a value
        // change), so return False afterwards.
        SObject(kind: ModuleNamespace(exports:), ..), StringKey(pk) -> {
          let _existing = namespace_own_property(st, exports, pk)
          #(False, st)
        }
        _, _ -> {
          let st = devolve(st, recv_h)
          let assert SObject(
            kind:,
            props:,
            symbol_props:,
            elements:,
            extensible:,
            ..,
          ) = read_object(st, recv_h)
          case key {
            StringKey(pk) ->
              set_own_string(
                st,
                recv_h,
                kind,
                props,
                elements,
                extensible,
                pk,
                v,
              )
            SymbolKey(sym) ->
              set_own_symbol(st, recv_h, symbol_props, extensible, sym, v)
          }
        }
      }
    }
    _ -> #(False, st)
  }
}

/// §10.1.9.2 steps 2.c-e on a shaped receiver (always ordinary and
/// extensible): an existing slot is overwritten in place; a new named key
/// moves the object to the successor shape along the transition edge for
/// that key, minting the successor on first use.
fn set_own_shaped(
  st: Agent,
  h: Handle,
  shape_id: Int,
  proto: Option(Handle),
  slots: rt_types.ShapeSlots,
  name: String,
  v: JsVal,
) -> #(Bool, Agent) {
  let js = require_js(st)
  let key_bin = bit_array.from_string(name)
  case dict.get(js.shapes, shape_id) {
    Error(Nil) -> #(False, st)
    Ok(ShapeDesc(arity:, offsets:, transitions:) as from) ->
      case dict.get(offsets, key_bin) {
        Ok(off) -> {
          let slots = rt_types.shape_slots_set(slots, off, v)
          #(
            True,
            rt_store.t_cell_set(st, h, SShapedObject(shape_id:, proto:, slots:)),
          )
        }
        Error(Nil) -> {
          let #(to, st) = case dict.get(transitions, key_bin) {
            Ok(to) -> #(to, st)
            Error(Nil) -> {
              let to = js.next_shape
              let shapes =
                js.shapes
                |> dict.insert(
                  shape_id,
                  ShapeDesc(
                    ..from,
                    transitions: dict.insert(transitions, key_bin, to),
                  ),
                )
                |> dict.insert(
                  to,
                  ShapeDesc(
                    arity: arity + 1,
                    offsets: dict.insert(offsets, key_bin, arity),
                    transitions: dict.new(),
                  ),
                )
              #(
                to,
                Agent(..st, store: JsStore(..js, shapes:, next_shape: to + 1)),
              )
            }
          }
          let slots = rt_types.shape_slots_append(slots, v)
          #(
            True,
            rt_store.t_cell_set(
              st,
              h,
              SShapedObject(shape_id: to, proto:, slots:),
            ),
          )
        }
      }
  }
}

/// Receiver-side write for a string/index key — arc `set_property_on_slot`
/// (`object.gleam:1234-1365`) reduced to the ordinary + Array/Arguments arms.
fn set_own_string(
  st: Agent,
  h: Handle,
  kind: ObjKind,
  props: Dict(PropertyKey, Property),
  elements: JsElements,
  extensible: Bool,
  key: PropertyKey,
  v: JsVal,
) -> #(Bool, Agent) {
  case kind, key {
    // §10.1.9.2 step 2.a: non-writable "length" → false (no coercion);
    // otherwise §10.4.2.1 step 1: ArraySetLength (§10.4.2.4).
    ArrayObj(_), Named("length") ->
      case array_length_writable(props) {
        False -> #(False, st)
        True -> array_put_length(st, h, v)
      }
    // §10.4.2.1 step 2 / §10.4.4.2: array/arguments index write.
    ArrayObj(length:), Index(i) -> {
      let length_writable = array_length_writable(props)
      case dict.get(props, key) {
        // Dict override at this index — honor its writable, keep attributes.
        Ok(DataProperty(writable: True, enumerable:, configurable:, seq:, ..)) ->
          write_props(
            st,
            h,
            dict.insert(
              props,
              key,
              DataProperty(
                value: v,
                writable: True,
                enumerable:,
                configurable:,
                seq:,
              ),
            ),
          )
        Ok(_) -> #(False, st)
        Error(Nil) ->
          // §10.4.2.1 step 2.h: growing past a non-writable length or on a
          // non-extensible array → false.
          case
            i >= length
            && { !extensible || !length_writable }
            || !extensible
            && !elements.has(elements, i)
          {
            True -> #(False, st)
            False -> {
              let new_len = int.max(length, i + 1)
              let st =
                rt_store.t_cell_update(st, h, fn(slot) {
                  let assert SObject(elements: e, ..) = slot
                  SObject(
                    ..slot,
                    kind: ArrayObj(new_len),
                    elements: elements.set(e, i, v),
                  )
                })
              #(True, st)
            }
          }
      }
    }
    // Receiver is itself an Integer-Indexed object: the receiver half of
    // OrdinarySet routes numeric index keys through the receiver's
    // [[DefineOwnProperty]] (§10.4.5.3) → IntegerIndexedElementSet for a
    // valid index, false (no conversion) for an invalid one. Non-numeric
    // keys fall through to the ordinary dict write below.
    TypedArrayObj(buffer: buf, elem_kind:, byte_offset:, length:), Index(idx) ->
      case
        buffer.typed_array_element_live(
          st,
          buf,
          elem_kind,
          byte_offset,
          length,
          idx,
        )
      {
        Some(_) ->
          buffer.typed_array_store(
            st,
            buffer.ViewSlot(buffer: buf, elem_kind:, byte_offset:, length:),
            Some(idx),
            v,
          )
        None -> #(False, st)
      }
    TypedArrayObj(..), Named(s) ->
      case buffer.is_canonical_numeric_string(s) {
        // Canonical numeric, never a valid index → CreateDataProperty →
        // [[DefineOwnProperty]] → false, with no value conversion.
        True -> #(False, st)
        False -> set_ordinary_string(st, h, props, extensible, key, v)
      }
    ArgumentsObj(..), Index(i) ->
      case dict.get(props, key) {
        Ok(DataProperty(writable: True, enumerable:, configurable:, seq:, ..)) ->
          write_props(
            st,
            h,
            dict.insert(
              props,
              key,
              DataProperty(
                value: v,
                writable: True,
                enumerable:,
                configurable:,
                seq:,
              ),
            ),
          )
        Ok(_) -> #(False, st)
        Error(Nil) ->
          case !extensible && !elements.has(elements, i) {
            True -> #(False, st)
            False -> {
              let st =
                rt_store.t_cell_update(st, h, fn(slot) {
                  let assert SObject(elements: e, ..) = slot
                  SObject(..slot, elements: elements.set(e, i, v))
                })
              #(True, st)
            }
          }
      }
    // String exotic receiver (§10.4.3): step 2.c reads the synthesized
    // "length" / in-range index descriptor, which is non-writable → 2.d.ii
    // false. Out-of-range indices and other names are ordinary dict writes.
    StringObj(_), Named("length") -> #(False, st)
    StringObj(value: s), Index(i) ->
      case js_string.char_at(s, i) {
        Some(_) -> #(False, st)
        None -> set_ordinary_string(st, h, props, extensible, key, v)
      }
    _, _ -> set_ordinary_string(st, h, props, extensible, key, v)
  }
}

/// §10.1.9.2 steps 2.c-2.h on the props dict: define/update an own data
/// property under a string/private key.
fn set_ordinary_string(
  st: Agent,
  h: Handle,
  props: Dict(PropertyKey, Property),
  extensible: Bool,
  key: PropertyKey,
  v: JsVal,
) -> #(Bool, Agent) {
  case dict.get(props, key) {
    Ok(DataProperty(writable: True, enumerable:, configurable:, seq:, ..)) ->
      write_props(
        st,
        h,
        dict.insert(
          props,
          key,
          DataProperty(
            value: v,
            writable: True,
            enumerable:,
            configurable:,
            seq:,
          ),
        ),
      )
    Ok(_) -> #(False, st)
    Error(Nil) ->
      case extensible {
        False -> #(False, st)
        True -> {
          let #(prop, st) = new_data_property(st, v)
          write_props(st, h, dict.insert(props, key, prop))
        }
      }
  }
}

/// Receiver-side write for a symbol key — arc `define_symbol_data_on_receiver`
/// (`object.gleam:2976-3048`).
fn set_own_symbol(
  st: Agent,
  h: Handle,
  symbol_props: List(#(SymbolId, Property)),
  extensible: Bool,
  sym: SymbolId,
  v: JsVal,
) -> #(Bool, Agent) {
  case list.key_find(symbol_props, sym) {
    Ok(DataProperty(writable: True, enumerable:, configurable:, seq:, ..)) ->
      write_symbol_props(
        st,
        h,
        list.key_set(
          symbol_props,
          sym,
          DataProperty(
            value: v,
            writable: True,
            enumerable:,
            configurable:,
            seq:,
          ),
        ),
      )
    Ok(_) -> #(False, st)
    Error(Nil) ->
      case extensible {
        False -> #(False, st)
        True -> {
          let #(prop, st) = new_data_property(st, v)
          write_symbol_props(st, h, list.key_set(symbol_props, sym, prop))
        }
      }
  }
}

fn write_props(
  st: Agent,
  h: Handle,
  props: Dict(PropertyKey, Property),
) -> #(Bool, Agent) {
  let st =
    rt_store.t_cell_update(st, h, fn(slot) {
      let assert SObject(..) = slot
      SObject(..slot, props:)
    })
  #(True, st)
}

fn write_symbol_props(
  st: Agent,
  h: Handle,
  symbol_props: List(#(SymbolId, Property)),
) -> #(Bool, Agent) {
  let st =
    rt_store.t_cell_update(st, h, fn(slot) {
      let assert SObject(..) = slot
      SObject(..slot, symbol_props:)
    })
  #(True, st)
}

/// The Array "length" [[Writable]] attribute: a dict override holds it once
/// defineProperty froze it; absent means the default writable length.
fn array_length_writable(props: Dict(PropertyKey, Property)) -> Bool {
  case dict.get(props, Named("length")) {
    Ok(DataProperty(writable:, ..)) -> writable
    Ok(AccessorProperty(..)) | Error(Nil) -> True
  }
}

/// §10.4.2.4 ArraySetLength steps 3-5: newLen = ? ToUint32(Desc.[[Value]]),
/// numberLen = ? ToNumber(Desc.[[Value]]) (two observable coercions), then
/// RangeError unless SameValueZero(newLen, numberLen). Port of arc
/// `mop.array_define_length` + `parse_array_length` (`mop.gleam:590-644`).
fn to_array_length(st: Agent, v: JsVal) -> #(Int, Agent) {
  let #(new_len, st) = rt_val.t_to_uint32(st, v)
  let #(number_len, st) = rt_val.t_to_number(st, v)
  let same = case number_len {
    rt_types.JInt(n) -> n == new_len
    // `+. 0.0` folds -0.0 to +0.0 (SameValueZero).
    rt_types.JFloat(f) -> f +. 0.0 == int.to_float(new_len)
    rt_types.JNan | rt_types.JPosInf | rt_types.JNegInf -> False
  }
  case same {
    True -> #(new_len, st)
    False -> throw_range_error(st, "Invalid array length")
  }
}

/// §10.1.9.2 step 3 → §10.4.2.1 step 1 for `A.length = v`: the receiver's
/// own "length" was writable, so [[DefineOwnProperty]](A, "length",
/// {[[Value]]: v}) = ArraySetLength. Steps 3-5 coerce first; steps 7-12 then
/// re-read oldLenDesc, which the coercion may have frozen.
fn array_put_length(st: Agent, h: Handle, v: JsVal) -> #(Bool, Agent) {
  let #(new_len, st) = to_array_length(st, v)
  let assert SObject(kind: ArrayObj(length: old_len), props:, ..) =
    read_object(st, h)
  case array_length_writable(props) {
    True -> array_set_length(st, h, new_len, old_len)
    // Step 11.a with a non-writable current: true only for an unchanged
    // value; step 12: shrinking a non-writable length → false.
    False -> #(new_len == old_len, st)
  }
}

/// §10.4.2.4 ArraySetLength steps 11-19 for an already validated uint32
/// `new_len`. Shrinking truncates elements and dict Index overrides. Port of
/// arc `write_array_length` + `shrink_array` (`mop.gleam:693-807`).
fn array_set_length(
  st: Agent,
  h: Handle,
  new_len: Int,
  old_len: Int,
) -> #(Bool, Agent) {
  case new_len >= old_len {
    True -> {
      let st =
        rt_store.t_cell_update(st, h, fn(slot) {
          let assert SObject(..) = slot
          SObject(..slot, kind: ArrayObj(new_len))
        })
      #(True, st)
    }
    False -> {
      // Step 17-18: shrink — a non-configurable Index override stops the
      // truncation at that index + 1 and the define reports false.
      let assert SObject(props:, ..) = read_object(st, h)
      let blocked =
        dict.fold(props, None, fn(acc, k, prop) {
          case k {
            Index(i) if i >= new_len ->
              case rt_types.prop_configurable(prop) {
                False ->
                  Some(case acc {
                    Some(m) -> int.max(m, i)
                    None -> i
                  })
                True -> acc
              }
            _ -> acc
          }
        })
      let final_len = case blocked {
        Some(b) -> b + 1
        None -> new_len
      }
      let st =
        rt_store.t_cell_update(st, h, fn(slot) {
          let assert SObject(props: p, elements: e, ..) = slot
          SObject(
            ..slot,
            kind: ArrayObj(final_len),
            props: dict.filter(p, fn(k, _) {
              case k {
                Index(i) -> i < final_len
                _ -> True
              }
            }),
            elements: elements.truncate(e, final_len),
          )
        })
      #(option.is_none(blocked), st)
    }
  }
}

fn throw_range_error(st: Agent, msg: String) -> a {
  let #(e, st) = js_ops(st).new_error(st, rt_types.RangeErr, msg)
  rt_store.t_throw(st, e)
}

// ── [[DefineOwnProperty]] (§10.1.6) ─────────────────────────────────────────

/// **[[DefineOwnProperty]] ( P, Desc )** — the trap-aware internal method
/// returning the raw boolean status (the spec's `? O.[[DefineOwnProperty]]
/// (P, Desc)` expression). Proxies dispatch to the `defineProperty` trap
/// (§10.5.6); TypedArray / String / Module Namespace exotics validate against
/// their synthesized descriptors; everything else is §10.1.6.3
/// ValidateAndApplyPropertyDescriptor. `#(False, st)` is a validation
/// rejection (non-extensible + new key, or `Desc` incompatible with a
/// non-configurable current); genuine abrupt completions (ArraySetLength's
/// RangeError, trap throws, proxy invariant TypeErrors) raise. Callers decide
/// whether `False` throws (DefinePropertyOrThrow) or is returned
/// (Reflect.defineProperty, CreateDataProperty). Port of arc
/// `mop.define_own_property_bool` + `define_parsed` + `ordinary_define`
/// (`mop.gleam:210-336,814-1060,1900`). Array/Arguments index keys route
/// through the elements store.
pub fn t_define_own_prop(
  st: Agent,
  obj: Handle,
  key: ObjectKey,
  desc: ParsedDesc,
) -> #(Bool, Agent) {
  let st = devolve(st, obj)
  // §10.4.2.4 ArraySetLength steps 3-6 run before oldLenDesc is read (step
  // 7): the coercion may call user code that redefines "length" on A.
  let #(desc, new_len, st) = case read_object(st, obj), key, desc.value {
    SObject(kind: ArrayObj(_), ..), StringKey(Named("length")), Some(v) -> {
      let #(n, st) = to_array_length(st, v)
      let value = Some(rt_types.mk_number(rt_types.JInt(n)))
      #(ParsedDesc(..desc, value:), Some(n), st)
    }
    _, _, _ -> #(desc, None, st)
  }
  let assert SObject(kind:, props:, symbol_props:, elements:, extensible:, ..) =
    read_object(st, obj)
  use <- exotic_define(st, kind, key, desc)
  let indexed_kind = case kind {
    ArrayObj(_) | ArgumentsObj(..) -> True
    _ -> False
  }
  // §10.4.2.1 steps 2.b-c: an index at or past a non-writable length → false.
  let index_blocked = case kind, key {
    ArrayObj(length:), StringKey(Index(i)) ->
      i >= length && !array_length_writable(props)
    _, _ -> False
  }
  use <- bool.guard(index_blocked, #(False, st))
  // Step 1: current = O.[[GetOwnProperty]](P).
  let existing = case key {
    StringKey(pk) -> own_property_of(st, kind, props, elements, pk)
    SymbolKey(sym) -> own_symbol_property_of(symbol_props, sym)
  }
  // Step 2 / steps 5-11: is the change permitted?
  let ok = case existing {
    None -> extensible
    Some(cur) -> is_compatible_descriptor(desc, cur)
  }
  use <- bool.guard(!ok, #(False, st))
  // Merge Desc over current, defaulting absent fields.
  let #(seq, st) = case existing {
    Some(old) -> #(rt_types.prop_seq(old), st)
    None -> rt_store.t_next_prop_seq(st)
  }
  let enumerable =
    option.unwrap(desc.enumerable, case existing {
      Some(p) -> rt_types.prop_enumerable(p)
      None -> False
    })
  let configurable =
    option.unwrap(desc.configurable, case existing {
      Some(p) -> rt_types.prop_configurable(p)
      None -> False
    })
  let new_prop = merge_descriptor(desc, existing, enumerable, configurable, seq)
  case kind, key {
    // §10.4.2.1 step 2 → §10.4.2.4 ArraySetLength: value updates
    // `kind: ArrayObj(new_len)` and truncates elements; the dict entry only
    // carries the merged attribute override (its value field is ignored by
    // `own_property_of`). `is_compatible_descriptor` above already rejected
    // accessor/configurable/enumerable/non-writable violations (steps 1,
    // 11.a, 12, 15-16).
    ArrayObj(length: old_len), StringKey(Named("length") as pk) -> {
      let #(len_ok, st) = case new_len {
        Some(n) -> array_set_length(st, obj, n, old_len)
        None -> #(True, st)
      }
      let st =
        rt_store.t_cell_update(st, obj, fn(slot) {
          let assert SObject(props: p, ..) = slot
          SObject(..slot, props: dict.insert(p, pk, new_prop))
        })
      #(len_ok, st)
    }
    _, _ -> {
      // Write to the right store. Array/Arguments Index with default data
      // attributes stays in the fast elements store; anything else is a dict
      // override (the element copy is removed so exactly one store owns it).
      let st =
        rt_store.t_cell_update(st, obj, fn(slot) {
          let assert SObject(props: p, symbol_props: sp, elements: e, ..) = slot
          case key {
            StringKey(Index(i) as pk) if indexed_kind ->
              case new_prop {
                DataProperty(
                  value: v,
                  writable: True,
                  enumerable: True,
                  configurable: True,
                  ..,
                ) ->
                  SObject(
                    ..slot,
                    props: dict.delete(p, pk),
                    elements: elements.set(e, i, v),
                  )
                _ ->
                  SObject(
                    ..slot,
                    props: dict.insert(p, pk, new_prop),
                    elements: elements.delete(e, i),
                  )
              }
            StringKey(pk) ->
              SObject(..slot, props: dict.insert(p, pk, new_prop))
            SymbolKey(sym) ->
              SObject(..slot, symbol_props: list.key_set(sp, sym, new_prop))
          }
        })
      // §10.4.2.1 step 2.f-g: Array Index write past length bumps it.
      let st = case kind, key {
        ArrayObj(length:), StringKey(Index(i)) if i >= length ->
          rt_store.t_cell_update(st, obj, fn(slot) {
            let assert SObject(..) = slot
            SObject(..slot, kind: ArrayObj(i + 1))
          })
        _, _ -> st
      }
      #(True, st)
    }
  }
}

/// SPEC §8 op-table spelling — thin alias for `t_define_own_prop`.
pub fn t_define_prop(
  st: Agent,
  obj: Handle,
  key: ObjectKey,
  desc: ParsedDesc,
) -> #(Bool, Agent) {
  t_define_own_prop(st, obj, key, desc)
}

/// The exotic dispatch head of [[DefineOwnProperty]]: Proxy (§10.5.6),
/// Module Namespace (§10.4.6.6), String (§10.4.3.2) and TypedArray
/// (§10.4.5.3) absorb the keys they own; everything else continues with the
/// ordinary body.
fn exotic_define(
  st: Agent,
  kind: ObjKind,
  key: ObjectKey,
  desc: ParsedDesc,
  ordinary: fn() -> #(Bool, Agent),
) -> #(Bool, Agent) {
  case kind, key {
    // §10.5.6 Proxy [[DefineOwnProperty]] — private names live in the
    // proxy's own [[PrivateElements]] and never trap.
    ProxyObj(..), StringKey(Private(_)) -> ordinary()
    ProxyObj(target:, handler:, revoked:), _ ->
      proxy_define_own_property(
        st,
        Proxy(target:, handler:, revoked:),
        key,
        desc,
      )
    // §10.4.6.6 Module Namespace [[DefineOwnProperty]] step 1: a Symbol key
    // is OrdinaryDefineOwnProperty; string keys never define anything.
    ModuleNamespace(..), SymbolKey(_) -> ordinary()
    ModuleNamespace(exports:), StringKey(pk) ->
      namespace_define(st, exports, pk, desc)
    // §10.4.3.2 String exotic [[DefineOwnProperty]]: the synthesized
    // "length" and in-range index properties are non-writable and
    // non-configurable — step 2.b IsCompatiblePropertyDescriptor against
    // their fixed descriptors instead of writing to the dict (a compatible
    // Desc is a no-op redefinition). Everything else is ordinary (step 3).
    StringObj(value: s), StringKey(Named("length")) -> #(
      is_compatible_descriptor(desc, string_length_property(s)),
      st,
    )
    StringObj(value: s), StringKey(Index(i)) ->
      case string_index_property(s, i) {
        Some(cur) -> #(is_compatible_descriptor(desc, cur), st)
        None -> ordinary()
      }
    // §10.4.5.3 TypedArray (Integer-Indexed) [[DefineOwnProperty]]:
    // canonical numeric index keys never reach the ordinary property table —
    // they validate against the fixed {W:T, E:T, C:T} element descriptor and
    // store through IntegerIndexedElementSet. Everything else is ordinary.
    TypedArrayObj(buffer: buf, elem_kind:, byte_offset:, length:),
      StringKey(Index(idx))
    ->
      typed_array_define_index(
        st,
        buf,
        elem_kind,
        byte_offset,
        length,
        idx,
        desc,
      )
    TypedArrayObj(..), StringKey(Named(s)) ->
      case buffer.is_canonical_numeric_string(s) {
        // Step 1.b.i: a canonical numeric string that survived
        // canonical_key is never a valid integer index ("1.5", "-0",
        // "NaN", "-1", "1e+21", …) → false, with NO value conversion.
        True -> #(False, st)
        False -> ordinary()
      }
    _, _ -> ordinary()
  }
}

/// §10.4.5.3 TypedArray [[DefineOwnProperty]] — P is a canonical integer
/// index. Steps 1.b.i-vii:
///   i.   invalid index (out of bounds / detached / shrunk) → false
///   ii.  [[Configurable]] present and false → false
///   iii. [[Enumerable]] present and false → false
///   iv.  accessor descriptor → false
///   v.   [[Writable]] present and false → false
///   vi.  [[Value]] present → ? IntegerIndexedElementSet (value conversion
///        may run user code and throw; a buffer detached DURING conversion
///        makes the store a silent no-op, still true). Immutable-buffer
///        views never reach the element store: the define succeeds only if
///        [[Value]] is SameValue to the current element and Desc asks for
///        no [[Writable]]/[[Configurable]] upgrade; otherwise it is rejected.
///   vii. true
/// The checks run BEFORE any value conversion — an invalid index must not
/// trigger observable ToNumber/ToBigInt side effects.
fn typed_array_define_index(
  st: Agent,
  buf: Handle,
  elem_kind: TypedArrayKind,
  byte_offset: Int,
  length: Option(Int),
  idx: Int,
  desc: ParsedDesc,
) -> #(Bool, Agent) {
  let current =
    buffer.typed_array_element(
      st,
      buf,
      elem_kind,
      byte_offset,
      buffer.typed_array_view_length(st, buf, elem_kind, byte_offset, length),
      idx,
    )
  use <- bool.guard(option.is_none(current), #(False, st))
  use <- bool.guard(desc.configurable == Some(False), #(False, st))
  use <- bool.guard(desc.enumerable == Some(False), #(False, st))
  use <- bool.guard(desc_is_accessor(desc), #(False, st))
  use <- bool.guard(desc.writable == Some(False), #(False, st))
  case desc.value {
    None -> #(True, st)
    Some(v) -> {
      let #(stored, st) =
        buffer.typed_array_store(
          st,
          buffer.ViewSlot(buffer: buf, elem_kind:, byte_offset:, length:),
          Some(idx),
          v,
        )
      // Immutable ArrayBuffer proposal (sec-typedarray-defineownproperty):
      // an immutable-buffer-backed element behaves as a {[[Writable]]:
      // false, [[Enumerable]]: true, [[Configurable]]: false} data property,
      // so ValidateAndApplyPropertyDescriptor returns true iff Desc.[[Value]]
      // is SameValue to the current element AND Desc asks for no attribute
      // upgrade; anything else is false. The store refused (False) BEFORE
      // any ToNumber/ToBigInt conversion, so no user code ran and `current`
      // (read at entry) is still live.
      case stored {
        True -> #(True, st)
        False -> {
          let unchanged =
            option.map(current, same_value(v, _))
            |> option.unwrap(False)
          let widened =
            desc.writable == Some(True) || desc.configurable == Some(True)
          #(unchanged && !widened, st)
        }
      }
    }
  }
}

/// §10.1.6.2 IsCompatiblePropertyDescriptor — `desc` over a non-`None`
/// `current`. Port of arc `mop.is_compatible_descriptor` (`mop.gleam:1480`).
fn is_compatible_descriptor(desc: ParsedDesc, cur: Property) -> Bool {
  case rt_types.prop_configurable(cur) {
    True -> True
    False -> {
      // Step 4: reject configurable:true or an enumerable flip.
      let bad_configurable = desc.configurable == Some(True)
      let bad_enumerable = case desc.enumerable {
        Some(e) -> e != rt_types.prop_enumerable(cur)
        None -> False
      }
      use <- bool.guard(bad_configurable || bad_enumerable, False)
      let is_acc = desc_is_accessor(desc)
      let is_dat = desc_is_data(desc)
      // Step 5: generic descriptor — no further validation.
      use <- bool.guard(!is_acc && !is_dat, True)
      case cur {
        DataProperty(writable: cur_w, value: cur_v, ..) ->
          case is_acc {
            True -> False
            False ->
              case cur_w {
                True -> True
                False ->
                  desc.writable != Some(True)
                  && case desc.value {
                    Some(v) -> same_value(v, cur_v)
                    None -> True
                  }
              }
          }
        AccessorProperty(get: cur_g, set: cur_s, ..) ->
          case is_dat {
            True -> False
            False -> {
              let undef = rt_types.mk_undefined()
              let g_ok = case desc.get {
                Some(g) -> same_value(g, option.unwrap(cur_g, undef))
                None -> True
              }
              let s_ok = case desc.set {
                Some(s) -> same_value(s, option.unwrap(cur_s, undef))
                None -> True
              }
              g_ok && s_ok
            }
          }
      }
    }
  }
}

/// Build the merged `Property` (arc `mop.gleam:906-998`).
fn merge_descriptor(
  desc: ParsedDesc,
  existing: Option(Property),
  enumerable: Bool,
  configurable: Bool,
  seq: Int,
) -> Property {
  case desc_is_accessor(desc), desc_is_data(desc) {
    // Generic descriptor: keep existing kind/fields, update E/C only.
    False, False ->
      case existing {
        Some(DataProperty(value: v, writable: w, ..)) ->
          DataProperty(value: v, writable: w, enumerable:, configurable:, seq:)
        Some(AccessorProperty(get: g, set: s, ..)) ->
          AccessorProperty(get: g, set: s, enumerable:, configurable:, seq:)
        None ->
          DataProperty(
            value: rt_types.mk_undefined(),
            writable: False,
            enumerable:,
            configurable:,
            seq:,
          )
      }
    // Accessor descriptor: merge get/set with existing accessor (if any).
    True, _ -> {
      let getter =
        accessor_field(desc.get, case existing {
          Some(AccessorProperty(get: g, ..)) -> g
          _ -> None
        })
      let setter =
        accessor_field(desc.set, case existing {
          Some(AccessorProperty(set: s, ..)) -> s
          _ -> None
        })
      AccessorProperty(
        get: getter,
        set: setter,
        enumerable:,
        configurable:,
        seq:,
      )
    }
    // Data descriptor: merge value/writable with existing data (if any).
    False, True -> {
      let final_value = case desc.value {
        Some(v) -> v
        None ->
          case existing {
            Some(DataProperty(value: v, ..)) -> v
            _ -> rt_types.mk_undefined()
          }
      }
      let final_writable = case desc.writable {
        Some(w) -> w
        None ->
          case existing {
            Some(DataProperty(writable: w, ..)) -> w
            _ -> False
          }
      }
      DataProperty(
        value: final_value,
        writable: final_writable,
        enumerable:,
        configurable:,
        seq:,
      )
    }
  }
}

/// Normalize a `ParsedDesc` get/set field: `Some(undefined)` → `None`;
/// `None` inherits from the existing accessor.
fn accessor_field(
  field: Option(JsVal),
  inherit: Option(JsVal),
) -> Option(JsVal) {
  case field {
    Some(v) ->
      case rt_types.classify(v) {
        KUndef -> None
        _ -> Some(v)
      }
    None -> inherit
  }
}

// ── [[HasProperty]] (§10.1.7) ───────────────────────────────────────────────

/// §10.1.7.1 OrdinaryHasProperty — the observable `key in obj`. Primitive
/// receivers auto-box; `null`/`undefined` throw. Private keys are invisible
/// to ordinary [[HasProperty]] (they live in [[PrivateElements]], probed by
/// the `#x in o` opcode elsewhere). Port of arc `has_property` +
/// `has_symbol_property` (`object.gleam:1949-2003,1891`).
pub fn t_has_prop(st: Agent, recv: JsVal, key: ObjectKey) -> #(Bool, Agent) {
  case rt_types.classify(recv) {
    KHandle(h) -> has_from(st, h, key)
    KUndef | KNull ->
      throw_type_error(
        st,
        "Cannot use 'in' operator to search for '"
          <> key_text(key)
          <> "' in "
          <> case rt_types.classify(recv) {
          KNull -> "null"
          _ -> "undefined"
        },
      )
    _ -> {
      let #(h, st) = js_ops(st).to_object(st, recv)
      has_from(st, h, key)
    }
  }
}

/// §10.1.7.1 OrdinaryHasProperty / §10.5.7 Proxy [[HasProperty]] /
/// §10.4.6.7 Module Namespace [[HasProperty]] on an object handle. Recurses
/// through prototype chains so a proxy anywhere on the chain traps.
fn has_from(st: Agent, h: Handle, key: ObjectKey) -> #(Bool, Agent) {
  case read_object(st, h), key {
    // Private-element keys are invisible to ordinary [[HasProperty]] (the
    // brand check uses `t_ordinary_own_property` via the PrivateIn op).
    _, StringKey(Private(_)) -> #(False, st)
    SObject(kind: ProxyObj(target:, handler:, revoked:), ..), _ ->
      proxy_has(st, Proxy(target:, handler:, revoked:), key)
    // §10.4.6.7: exported names only; symbols are OrdinaryHasProperty over
    // `symbol_props` (null prototype, so no chain walk).
    SObject(kind: ModuleNamespace(exports:), symbol_props:, ..), _ -> #(
      case key {
        StringKey(pk) -> dict.has_key(exports, rt_types.key_to_text(pk))
        SymbolKey(sym) ->
          option.is_some(own_symbol_property_of(symbol_props, sym))
      },
      st,
    )
    slot, _ -> {
      // Step 1-2: Let hasOwn be O.[[GetOwnProperty]](P). If not undefined,
      // return true.
      let #(own, proto) = own_and_proto_of_slot(st, slot, key)
      case own {
        Some(_) -> #(True, st)
        // §10.4.5.2 TypedArray [[HasProperty]]: a canonical numeric index key
        // answers IsValidIntegerIndex directly — own_property_of already said
        // the index is invalid, so the answer is false WITHOUT consulting the
        // prototype chain (TypedArray.prototype["1.5"] is unreachable).
        None ->
          case typed_array_absorbs(slot, key) {
            True -> #(False, st)
            False ->
              // Step 3-5: parent.[[HasProperty]] or false.
              case proto {
                Some(parent) -> has_from(st, parent, key)
                None -> #(False, st)
              }
          }
      }
    }
  }
}

// ── [[Delete]] (§10.1.10) ───────────────────────────────────────────────────

/// §10.1.10.1 OrdinaryDelete — the observable `delete obj[key]`. Returns
/// `#(False, st)` when the property is non-configurable. Port of arc
/// `delete_property` + `delete_symbol_property` (`object.gleam:2118-2305`).
pub fn t_delete_prop(st: Agent, obj: Handle, key: ObjectKey) -> #(Bool, Agent) {
  let st = devolve(st, obj)
  let assert SObject(kind:, props:, symbol_props:, elements:, ..) =
    read_object(st, obj)
  case key {
    SymbolKey(sym) ->
      case kind {
        // §10.5.10 Proxy [[Delete]].
        ProxyObj(target:, handler:, revoked:) ->
          proxy_delete(st, Proxy(target:, handler:, revoked:), key)
        _ ->
          case list.key_pop(symbol_props, sym) {
            Ok(#(prop, rest)) ->
              case rt_types.prop_configurable(prop) {
                False -> #(False, st)
                True -> write_symbol_props(st, obj, rest)
              }
            Error(Nil) -> #(True, st)
          }
      }
    StringKey(pk) -> {
      // §10.1.10.1 OrdinaryDelete for the string/private-key case: shared by
      // every exotic arm below that falls back to ordinary behavior.
      let ordinary_delete = fn() {
        case dict.get(props, pk) {
          Ok(prop) ->
            case rt_types.prop_configurable(prop) {
              False -> #(False, st)
              True -> write_props(st, obj, dict.delete(props, pk))
            }
          Error(Nil) -> #(True, st)
        }
      }
      case kind, pk {
        // §10.5.10 Proxy [[Delete]] — private names live in the proxy's own
        // [[PrivateElements]] and never trap.
        ProxyObj(..), Private(_) -> ordinary_delete()
        ProxyObj(target:, handler:, revoked:), _ ->
          proxy_delete(st, Proxy(target:, handler:, revoked:), key)
        // §10.4.6.10 Module Namespace [[Delete]]: deleting an exported name
        // fails (non-configurable); a non-export "succeeds" vacuously.
        ModuleNamespace(exports:), _ -> #(
          !dict.has_key(exports, rt_types.key_to_text(pk)),
          st,
        )
        // Array virtual "length" is non-configurable.
        ArrayObj(_), Named("length") -> #(False, st)
        // Array/Arguments index: dict override wins; else elements.
        ArrayObj(_), Index(i) | ArgumentsObj(..), Index(i) ->
          case dict.get(props, pk) {
            Ok(prop) ->
              case rt_types.prop_configurable(prop) {
                False -> #(False, st)
                True -> {
                  let st =
                    rt_store.t_cell_update(st, obj, fn(slot) {
                      let assert SObject(props: p, elements: e, ..) = slot
                      SObject(
                        ..slot,
                        props: dict.delete(p, pk),
                        elements: elements.delete(e, i),
                      )
                    })
                  #(True, st)
                }
              }
            Error(Nil) ->
              case elements.has(elements, i) {
                False -> #(True, st)
                True -> {
                  let st =
                    rt_store.t_cell_update(st, obj, fn(slot) {
                      let assert SObject(elements: e, ..) = slot
                      SObject(..slot, elements: elements.delete(e, i))
                    })
                  #(True, st)
                }
              }
          }
        // §10.4.5.6 TypedArray [[Delete]]: canonical numeric index keys are
        // deletable iff they are NOT valid indices (nothing to delete); a
        // live element is non-configurable from delete's point of view.
        TypedArrayObj(buffer: buf, elem_kind:, byte_offset:, length:),
          Index(idx)
        ->
          case
            buffer.typed_array_element_live(
              st,
              buf,
              elem_kind,
              byte_offset,
              length,
              idx,
            )
          {
            Some(_) -> #(False, st)
            None -> #(True, st)
          }
        TypedArrayObj(..), Named(s) ->
          case buffer.is_canonical_numeric_string(s) {
            True -> #(True, st)
            False -> ordinary_delete()
          }
        // String exotic: "length" and in-range indices are synthesized
        // non-configurable properties (§10.4.3) — never deletable.
        StringObj(_), Named("length") -> #(False, st)
        StringObj(value: s), Index(i) ->
          case js_string.char_at(s, i) {
            Some(_) -> #(False, st)
            None -> ordinary_delete()
          }
        _, _ -> ordinary_delete()
      }
    }
  }
}

// ── [[OwnPropertyKeys]] (§10.1.11) ────────────────────────────────────────────

/// **[[OwnPropertyKeys]] ( )** — the trap-aware internal method: §10.5.11
/// for proxies (whatever order the `ownKeys` trap chose, validated), else
/// §10.1.11 OrdinaryOwnPropertyKeys order: integer-index ascending, then
/// string keys by insertion (`Property.seq`), then symbols (creation order —
/// `symbol_props` is an assoc list). Array/String exotics emit their
/// birth-time "length" before any user-named key; Module Namespaces emit
/// their exports sorted by code unit (§10.4.6.11). `Private(_)` keys are
/// never returned (SPEC §7.M4 invariant).
///
/// Every element is a String or a Symbol *by construction* — the ordinary
/// path builds them from the property tables, the proxy path validates the
/// trap result (§7.3.19). This is THE single funnel for own-key enumeration
/// order: for-in, Object.keys/values/entries/assign, getOwnPropertyNames,
/// Reflect.ownKeys, JSON.stringify and spread/rest all route through it.
/// Port of arc `mop.own_property_keys` + `own_string_keys_flagged` +
/// `collect_own_symbol_keys` (`object.gleam:2333-2410`, `mop.gleam:2176`).
pub fn t_own_keys(st: Agent, obj: Handle) -> #(List(ObjectKey), Agent) {
  // Enumeration needs the full props dict — materialize (slow-path only).
  let assert SObject(kind:, props:, symbol_props:, elements:, ..) =
    as_sobject(st, read_object(st, obj))
  use <- proxy_or(kind, proxy_own_keys(st, _))
  let has_virtual_length = case kind {
    ArrayObj(_) | StringObj(_) -> True
    _ -> False
  }
  // Elements-store indices — always own data properties. String exotic
  // synthesizes one index per code unit (§10.4.3.3 step 3); TypedArray its
  // live indices (§10.4.5.7 step 2.a).
  let ascending = fn(n) {
    int.range(from: n - 1, to: -1, with: [], run: fn(acc, i) { [i, ..acc] })
  }
  let elem_idx = case kind {
    ArrayObj(length:) | ArgumentsObj(length:, ..) ->
      elements.indices(elements) |> list.filter(fn(i) { i < length })
    StringObj(value: s) -> ascending(js_string.length(s))
    TypedArrayObj(buffer: buf, elem_kind:, byte_offset:, length:) ->
      ascending(buffer.typed_array_live_count(
        st,
        buf,
        elem_kind,
        byte_offset,
        length,
      ))
    _ -> []
  }
  // Split dict entries. Array's dict "length" only tracks frozen attributes;
  // the visible key is emitted as `length_key` below. A String wrapper's dict
  // may hold out-of-range indices only (in-range ones are synthesized).
  let #(dict_idx, named) =
    dict.fold(props, #([], []), fn(acc, k, prop) {
      let #(idx, named) = acc
      case k {
        Index(i) -> #([i, ..idx], named)
        Named("length") if has_virtual_length -> acc
        Private(_) -> acc
        Named(_) -> #(idx, [#(rt_types.prop_seq(prop), k), ..named])
      }
    })
  // §10.4.6.11 Module Namespace: the exported names sorted by code unit,
  // then (ordinary step 3) the symbol keys.
  let named = case kind {
    ModuleNamespace(exports:) ->
      list.sort(dict.keys(exports), string.compare)
      |> list.index_map(fn(name, i) { #(i, Named(name)) })
    _ -> named
  }
  // Step 1: array-index keys ascending. An index lives in exactly one store.
  let index_keys =
    list.append(elem_idx, dict_idx)
    |> list.sort(int.compare)
    |> list.map(fn(i) { StringKey(Index(i)) })
  // Array/String virtual "length" exists from birth — before any user Named
  // key (§10.4.2 / §10.4.3.4 StringCreate).
  let length_key = case has_virtual_length {
    True -> [StringKey(Named("length"))]
    False -> []
  }
  // Step 2: other string keys by creation seq.
  let named_keys =
    list.sort(named, fn(a, b) { int.compare(a.0, b.0) })
    |> list.map(fn(pair) { StringKey(pair.1) })
  // Step 3: symbol keys in creation order.
  let symbol_keys = list.map(symbol_props, fn(pair) { SymbolKey(pair.0) })
  #(list.flatten([index_keys, length_key, named_keys, symbol_keys]), st)
}

/// §7.3.23 EnumerableOwnProperties ( O, key ): own keys via
/// [[OwnPropertyKeys]] (trap), then per-key [[GetOwnProperty]] (trap) to
/// filter for enumerable STRING keys, in [[OwnPropertyKeys]] order. The
/// per-key descriptor read is observable (proxy `getOwnPropertyDescriptor`,
/// Module Namespace TDZ ReferenceError). Port of arc
/// `mop.enumerable_string_keys_stateful` (`mop.gleam:2431`).
pub fn t_enumerable_own_keys(
  st: Agent,
  obj: Handle,
) -> #(List(PropertyKey), Agent) {
  let #(keys, st) = t_own_keys(st, obj)
  let #(found, st) =
    list.fold(keys, #([], st), fn(acc, key) {
      let #(found, st) = acc
      case key {
        SymbolKey(_) -> acc
        StringKey(pk) -> {
          let #(prop, st) = t_get_own_property(st, obj, key)
          case prop {
            Some(p) ->
              case rt_types.prop_enumerable(p) {
                True -> #([pk, ..found], st)
                False -> #(found, st)
              }
            None -> #(found, st)
          }
        }
      }
    })
  #(list.reverse(found), st)
}

/// SPEC§8 `for_in_keys` — §14.7.5.9 EnumerateObjectProperties. Eager cons-list
/// of JS string values for `for (k in obj)`. `null`/`undefined` → `[]`
/// (§14.7.5.6 step 6.a); primitives box via `ops.to_object`. Own keys come
/// from [[OwnPropertyKeys]], enumerability from [[GetOwnProperty]] and the
/// next level from [[GetPrototypeOf]] — all three trap for a proxy anywhere
/// on the chain, and a Module Namespace's TDZ export throws ReferenceError
/// before iteration. Port of arc `enumerate_keys` + `mop.enumerate_keys
/// _stateful` (`object.gleam:2412-2460`, `mop.gleam:2492-2548`).
pub fn t_for_in_keys(st: Agent, obj: JsVal) -> #(List(JsVal), Agent) {
  case rt_types.classify(obj) {
    KUndef | KNull -> #([], st)
    KHandle(h) ->
      for_in_keys_loop(st, Some(h), set.new(), [], limits.max_prototype_depth)
    _ -> {
      let #(h, st) = js_ops(st).to_object(st, obj)
      for_in_keys_loop(st, Some(h), set.new(), [], limits.max_prototype_depth)
    }
  }
}

/// Proto-chain walk for `t_for_in_keys`. Per level: `t_own_keys` gives §10.1.11
/// order; symbols dropped; a non-enumerable own key still SHADOWS an enumerable
/// proto key (§14.7.5.9) — `seen` records both. A `getPrototypeOf` trap can
/// return a fresh proxy every hop, so the walk is bounded by `fuel`
/// (`limits.max_prototype_depth`) and stops as if the chain ended — V8 does
/// the same; §14.7.5.10's note leaves iteration mechanics
/// implementation-defined.
fn for_in_keys_loop(
  st: Agent,
  current: Option(Handle),
  seen: set.Set(String),
  acc: List(JsVal),
  fuel: Int,
) -> #(List(JsVal), Agent) {
  case current {
    Some(h) if fuel > 0 -> {
      let #(keys, st) = t_own_keys(st, h)
      let #(acc, seen, st) =
        list.fold(keys, #(acc, seen, st), fn(state, key) {
          let #(a, s, st) = state
          case key {
            SymbolKey(_) -> state
            StringKey(pk) -> {
              let name = rt_types.key_to_text(pk)
              case set.contains(s, name) {
                True -> state
                False -> {
                  let s = set.insert(s, name)
                  let #(prop, st) = t_get_own_property(st, h, key)
                  let enumerable =
                    option.map(prop, rt_types.prop_enumerable)
                    |> option.unwrap(False)
                  case enumerable {
                    True -> #([rt_types.mk_string(name), ..a], s, st)
                    False -> #(a, s, st)
                  }
                }
              }
            }
          }
        })
      let #(proto, st) = t_get_prototype_of(st, h)
      for_in_keys_loop(st, proto, seen, acc, fuel - 1)
    }
    _ -> #(list.reverse(acc), st)
  }
}

// ── receiver-aware / own-prop pub wrappers (M6/M7 seam) ─────────────────────
// ADDITIVE-only thin exports over the private MOP internals above so that
// `rt_class` (super get/set, private fields) and `rt_builtins`
// (Reflect.*, Object statics) can reach OrdinaryGet/Set with an explicit
// Receiver and the raw [[GetOwnProperty]]/[[IsExtensible]] slots without
// re-implementing the proto walk.

/// §10.1.8.1 OrdinaryGet(O, P, Receiver) with `O` a Handle and an explicit
/// `Receiver` — the `super.x` / `Reflect.get` entry point. Thin wrapper over
/// the private `get_from`.
pub fn t_get_prop_with_receiver(
  st: Agent,
  h: Handle,
  key: ObjectKey,
  receiver: JsVal,
) -> #(JsVal, Agent) {
  get_from(st, h, key, receiver)
}

/// §10.1.9.1 OrdinarySet(O, P, V, Receiver) with `O` a Handle and an explicit
/// `Receiver` — the `super.x = v` / `Reflect.set` entry point. Thin wrapper
/// over the private `set_from`.
pub fn t_set_prop_with_receiver(
  st: Agent,
  h: Handle,
  key: ObjectKey,
  v: JsVal,
  receiver: JsVal,
) -> #(Bool, Agent) {
  set_from(st, h, key, v, receiver)
}

/// **[[GetOwnProperty]] ( P )** — the trap-aware internal method: §10.5.5
/// for proxies (the `getOwnPropertyDescriptor` trap, invariants and all),
/// §10.4.6.5 for Module Namespaces, else `t_ordinary_own_property`. The
/// single entry point for getOwnPropertyDescriptor-style reflection on
/// possibly-proxy handles.
///
/// The engine's TDZ sentinel is filtered out HERE, once, so a returned
/// `Option(Property)` structurally cannot carry it: §10.4.6.5 says a module
/// namespace's [[GetOwnProperty]] performs [[Get]] on the binding, and an
/// uninitialized binding throws a ReferenceError before any descriptor
/// exists. Port of arc `mop.own_property_keyed` (`mop.gleam:1702`).
pub fn t_get_own_property(
  st: Agent,
  h: Handle,
  key: ObjectKey,
) -> #(Option(Property), Agent) {
  case read_object(st, h), key {
    // Private elements are stored in the ordinary table but never trap.
    slot, StringKey(Private(_)) -> #(own_and_proto_of_slot(st, slot, key).0, st)
    SObject(kind: ProxyObj(target:, handler:, revoked:), ..), _ ->
      proxy_get_own_property(st, Proxy(target:, handler:, revoked:), key)
    SObject(kind: ModuleNamespace(exports:), ..), StringKey(pk) -> #(
      namespace_own_property(st, exports, pk),
      st,
    )
    slot, _ -> #(own_and_proto_of_slot(st, slot, key).0, st)
  }
}

/// §10.1.5.1 OrdinaryGetOwnProperty ( O, P ) — the raw own-descriptor slot
/// read with NO prototype walk and NO trap dispatch. JRead: no state
/// threaded. Private-name lookup (M7 `t_private_get`/`t_private_in`) lands
/// here: private elements live in the object's own table even on a Proxy.
pub fn t_ordinary_own_property(
  st: Agent,
  h: Handle,
  key: ObjectKey,
) -> Option(Property) {
  let #(own, _proto) = read_own_and_proto(st, h, key)
  own
}

/// **IsExtensible ( O )** — §7.2.5 / §10.5.3 Proxy [[IsExtensible]] (the
/// `isExtensible` trap, whose result must agree with the target), else the
/// ordinary `[[Extensible]]` slot.
pub fn t_is_extensible(st: Agent, h: Handle) -> #(Bool, Agent) {
  case read_object(st, h) {
    SObject(kind: ProxyObj(target:, handler:, revoked:), ..) ->
      proxy_is_extensible(st, Proxy(target:, handler:, revoked:))
    slot -> #(slot_extensible(slot), st)
  }
}

/// §10.1.3.1 OrdinaryIsExtensible — the raw `[[Extensible]]` slot. JRead.
pub fn t_ordinary_is_extensible(st: Agent, h: Handle) -> Bool {
  slot_extensible(read_object(st, h))
}

fn slot_extensible(slot: JsSlot) -> Bool {
  case slot {
    SObject(extensible:, ..) -> extensible
    SShapedObject(..) -> True
    _ -> False
  }
}

/// **IsArray ( argument )** — §7.2.2 for an object argument: step 2 Array
/// exotic → true; step 3 Proxy → throw TypeError if revoked (3.a), else
/// recurse on [[ProxyTarget]] (3.b-c); step 4 false. Raises via D7.
pub fn t_is_array(st: Agent, h: Handle) -> Bool {
  case rt_store.t_cell_get(st, h) {
    SObject(kind: ArrayObj(_), ..) -> True
    SObject(kind: ProxyObj(revoked: True, ..), ..) ->
      throw_type_error(
        st,
        "Cannot perform 'IsArray' on a proxy that has been revoked",
      )
    SObject(kind: ProxyObj(target:, ..), ..) -> t_is_array(st, target)
    _ -> False
  }
}

/// **[[PreventExtensions]] ( )** — §10.5.4 for proxies (the
/// `preventExtensions` trap; `False` when the trap refuses), else §10.1.4.1
/// OrdinaryPreventExtensions (always `True`). Short-circuits when already
/// non-extensible (spec no-op).
pub fn t_prevent_extensions(st: Agent, h: Handle) -> #(Bool, Agent) {
  let st = devolve(st, h)
  let assert SObject(kind:, extensible:, ..) = read_object(st, h)
  use <- proxy_or(kind, proxy_prevent_extensions(st, _))
  use <- bool.guard(!extensible, #(True, st))
  let st =
    rt_store.t_cell_update(st, h, fn(slot) {
      let assert SObject(..) = slot
      SObject(..slot, extensible: False)
    })
  #(True, st)
}

// ── Module Namespace exotic objects (§10.4.6) ────────────────────────────────

/// Allocate a Module Namespace exotic object (§10.4.6.12 ModuleNamespaceCreate)
/// over `exports`: each name maps to the `SBox` cell holding that binding's
/// live value, so [[Get]] observes later assignments by the module body and
/// a still-uninitialized (TDZ) binding surfaces as ReferenceError. Null
/// prototype, non-extensible; the only symbol key is @@toStringTag =
/// "Module" {W:F, E:F, C:F} (§28.3.1).
pub fn t_new_module_namespace(
  st: Agent,
  exports: List(#(String, Handle)),
) -> #(Handle, Agent) {
  let to_string_tag =
    DataProperty(
      value: rt_types.mk_string("Module"),
      writable: False,
      enumerable: False,
      configurable: False,
      seq: 0,
    )
  rt_store.t_cell_new(
    st,
    SObject(
      kind: ModuleNamespace(exports: dict.from_list(exports)),
      proto: None,
      props: dict.new(),
      symbol_props: [#(rt_types.symbol_to_string_tag, to_string_tag)],
      elements: NoElements,
      extensible: False,
    ),
  )
}

/// Read the live value of a namespace export's binding cell, throwing
/// ReferenceError when the binding is still uninitialized (TDZ).
fn namespace_binding_value(st: Agent, name: String, cell: Handle) -> JsVal {
  let v = case rt_store.t_cell_get(st, cell) {
    SBox(value:) -> value
    _ -> rt_types.mk_undefined()
  }
  case rt_types.classify(v) {
    KTdz ->
      throw_reference_error(
        st,
        "Cannot access '" <> name <> "' before initialization",
      )
    _ -> v
  }
}

/// §10.4.6.8 Module Namespace [[Get]] for a string key. Resolves the export's
/// live binding cell and returns its value, throwing ReferenceError when the
/// binding is still uninitialized (TDZ). Unknown keys return undefined.
fn namespace_get(
  st: Agent,
  exports: Dict(String, Handle),
  key: PropertyKey,
) -> #(JsVal, Agent) {
  let name = rt_types.key_to_text(key)
  case dict.get(exports, name) {
    Error(Nil) -> #(rt_types.mk_undefined(), st)
    Ok(cell) -> #(namespace_binding_value(st, name, cell), st)
  }
}

/// §10.4.6.5 Module Namespace [[GetOwnProperty]] for a string key: a data
/// descriptor { value: <live binding>, writable: true, enumerable: true,
/// configurable: false }. Step 4 performs [[Get]], so an uninitialized (TDZ)
/// binding throws ReferenceError — even key-only operations (Object.keys,
/// hasOwnProperty, for-in) surface it. Unknown keys → None.
fn namespace_own_property(
  st: Agent,
  exports: Dict(String, Handle),
  key: PropertyKey,
) -> Option(Property) {
  let name = rt_types.key_to_text(key)
  use cell <- option.map(dict.get(exports, name) |> option.from_result)
  DataProperty(
    value: namespace_binding_value(st, name, cell),
    writable: True,
    enumerable: True,
    configurable: False,
    seq: 0,
  )
}

/// §10.4.6.6 Module Namespace [[DefineOwnProperty]] steps 2-9 for a string
/// key. Every export is a { writable: true, enumerable: true,
/// configurable: false } data property whose value is the live binding;
/// a request is honoured (true) iff it changes nothing.
fn namespace_define(
  st: Agent,
  exports: Dict(String, Handle),
  key: PropertyKey,
  desc: ParsedDesc,
) -> #(Bool, Agent) {
  let name = rt_types.key_to_text(key)
  case dict.get(exports, name) {
    // Steps 2-3: current = O.[[GetOwnProperty]](P); undefined → false.
    Error(Nil) -> #(False, st)
    Ok(cell) -> {
      // Step 4: Desc.[[Configurable]] present and true → false.
      // Step 5: Desc.[[Enumerable]] present and false → false.
      // Step 6: IsAccessorDescriptor(Desc) → false.
      // Step 7: Desc.[[Writable]] present and false → false.
      let incompatible =
        desc.configurable == Some(True)
        || desc.enumerable == Some(False)
        || desc_is_accessor(desc)
        || desc.writable == Some(False)
      use <- bool.guard(incompatible, #(False, st))
      case desc.value {
        // Step 9: no [[Value]] requested → true (nothing to change).
        None -> #(True, st)
        // Step 8: return SameValue(Desc.[[Value]], current.[[Value]]).
        // [[GetOwnProperty]] read the live binding at step 2; a TDZ binding
        // is a genuine abrupt completion, NOT a boolean-false define result:
        // Reflect.defineProperty(ns, ...) must throw it, not return false.
        Some(requested) -> #(
          same_value(requested, namespace_binding_value(st, name, cell)),
          st,
        )
      }
    }
  }
}

// ── Proxy exotic objects (§10.5) ────────────────────────────────────────────
// Every proxy internal method below (a) validates the proxy is not revoked
// and fetches the trap with GetMethod semantics (`proxy_trap`), (b) forwards
// to the TARGET's corresponding internal method when the trap is absent —
// via the public `t_*` entry so a proxy target traps in turn — and (c)
// enforces the §10.5 invariants against `? target.[[GetOwnProperty]](P)` /
// `? IsExtensible(target)`, both of which are the target's own (possibly
// trapping) internal methods, never raw slot reads. [[Call]]/[[Construct]]
// live in `rt_call`. Port of arc `object.gleam:3239-3844` +
// `mop.gleam:1719-2290`.

/// A Proxy exotic object's [[ProxyTarget]] / [[ProxyHandler]] slots plus its
/// revocation flag, lifted off the `ProxyObj` kind for the internal methods.
type Proxy {
  Proxy(target: Handle, handler: Handle, revoked: Bool)
}

/// Run `when_proxy` if `kind` is a Proxy, else the ordinary continuation.
fn proxy_or(
  kind: ObjKind,
  when_proxy: fn(Proxy) -> a,
  ordinary: fn() -> a,
) -> a {
  case kind {
    ProxyObj(target:, handler:, revoked:) ->
      when_proxy(Proxy(target:, handler:, revoked:))
    _ -> ordinary()
  }
}

/// §10.5.14 ValidateNonRevokedProxy + §7.3.10 GetMethod(handler, name).
/// Returns the trap function (`None` when the handler leaves it undefined
/// or null → forward to the target). TypeError on a revoked proxy or a
/// non-callable trap.
fn proxy_trap(st: Agent, p: Proxy, name: String) -> #(Option(JsVal), Agent) {
  use <- bool.lazy_guard(p.revoked, fn() {
    throw_type_error(
      st,
      "Cannot perform '" <> name <> "' on a proxy that has been revoked",
    )
  })
  // GetMethod step 1: Let func be ? GetV(V, P).
  let #(trap, st) =
    t_get_prop(st, rt_types.mk_object(p.handler), StringKey(Named(name)))
  case rt_types.classify(trap) {
    // GetMethod step 2: undefined or null → undefined.
    KUndef | KNull -> #(None, st)
    _ -> {
      // GetMethod step 3: If IsCallable(func) is false, throw TypeError.
      let #(callable, st) = rt_val.t_is_callable(st, trap)
      case callable {
        True -> #(Some(trap), st)
        False ->
          throw_type_error(
            st,
            "'" <> name <> "' trap of proxy handler is not a function",
          )
      }
    }
  }
}

/// `Call(trap, handler, args)` through the seeded `JsOps.call` (D17).
fn call_trap(
  st: Agent,
  p: Proxy,
  trap: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  js_ops(st).call(st, trap, rt_types.mk_object(p.handler), args)
}

/// §10.5.1 Proxy [[GetPrototypeOf]] ( ).
fn proxy_get_prototype_of(st: Agent, p: Proxy) -> #(Option(Handle), Agent) {
  // Steps 1-5: revocation check + GetMethod(handler, "getPrototypeOf").
  let #(trap, st) = proxy_trap(st, p, "getPrototypeOf")
  case trap {
    // Step 6: no trap → target.[[GetPrototypeOf]]().
    None -> t_get_prototype_of(st, p.target)
    Some(trap_fn) -> {
      // Step 7: handlerProto = ? Call(trap, handler, « target »).
      let #(res, st) = call_trap(st, p, trap_fn, [rt_types.mk_object(p.target)])
      // Step 8: neither Object nor Null → TypeError.
      let proto = case rt_types.classify(res) {
        KHandle(h) -> Some(h)
        KNull -> None
        _ ->
          throw_type_error(
            st,
            "'getPrototypeOf' on proxy: trap returned neither object nor null",
          )
      }
      // Step 9: ? IsExtensible(target) — traps for proxy targets.
      let #(ext, st) = t_is_extensible(st, p.target)
      // Step 10: extensible target → no invariant, return trap result.
      use <- bool.guard(ext, #(proto, st))
      // Steps 11-12: non-extensible target → must match actual proto.
      let #(target_proto, st) = t_get_prototype_of(st, p.target)
      case proto == target_proto {
        True -> #(proto, st)
        False ->
          throw_type_error(
            st,
            "'getPrototypeOf' on proxy: proxy target is non-extensible but the trap did not return its actual prototype",
          )
      }
    }
  }
}

/// §10.5.2 Proxy [[SetPrototypeOf]] ( V ).
fn proxy_set_prototype_of(
  st: Agent,
  p: Proxy,
  new_proto: Option(Handle),
) -> #(Bool, Agent) {
  let #(trap, st) = proxy_trap(st, p, "setPrototypeOf")
  case trap {
    // Step 6: no trap → target.[[SetPrototypeOf]](V).
    None -> t_set_prototype(st, p.target, new_proto)
    Some(trap_fn) -> {
      let proto_val = case new_proto {
        Some(h) -> rt_types.mk_object(h)
        None -> rt_types.mk_null()
      }
      // Step 7: ToBoolean(? Call(trap, handler, « target, V »)).
      let #(res, st) =
        call_trap(st, p, trap_fn, [rt_types.mk_object(p.target), proto_val])
      // Step 8: false → return false.
      use <- bool.guard(!rt_val.to_boolean(res), #(False, st))
      // Step 9: ? IsExtensible(target) — traps for proxy targets.
      let #(ext, st) = t_is_extensible(st, p.target)
      // Step 10: extensible target → true.
      use <- bool.guard(ext, #(True, st))
      // Steps 11-12: non-extensible target → V must be its actual proto.
      let #(target_proto, st) = t_get_prototype_of(st, p.target)
      case new_proto == target_proto {
        True -> #(True, st)
        False ->
          throw_type_error(
            st,
            "'setPrototypeOf' on proxy: trap returned truish for setting a new prototype on the non-extensible proxy target",
          )
      }
    }
  }
}

/// §10.5.3 Proxy [[IsExtensible]] ( ).
fn proxy_is_extensible(st: Agent, p: Proxy) -> #(Bool, Agent) {
  let #(trap, st) = proxy_trap(st, p, "isExtensible")
  case trap {
    // Step 6: no trap → ? IsExtensible(target).
    None -> t_is_extensible(st, p.target)
    Some(trap_fn) -> {
      // Step 7: ToBoolean(? Call(trap, handler, « target »)).
      let #(res, st) = call_trap(st, p, trap_fn, [rt_types.mk_object(p.target)])
      let b = rt_val.to_boolean(res)
      // Steps 8-9: result must equal ? IsExtensible(target).
      let #(target_ext, st) = t_is_extensible(st, p.target)
      case b == target_ext {
        True -> #(b, st)
        False ->
          throw_type_error(
            st,
            "'isExtensible' on proxy: trap result does not reflect extensibility of proxy target (which is '"
              <> case target_ext {
              True -> "true"
              False -> "false"
            }
              <> "')",
          )
      }
    }
  }
}

/// §10.5.4 Proxy [[PreventExtensions]] ( ).
fn proxy_prevent_extensions(st: Agent, p: Proxy) -> #(Bool, Agent) {
  let #(trap, st) = proxy_trap(st, p, "preventExtensions")
  case trap {
    // Step 6: no trap → target.[[PreventExtensions]]().
    None -> t_prevent_extensions(st, p.target)
    Some(trap_fn) -> {
      // Step 7: ToBoolean(? Call(trap, handler, « target »)).
      let #(res, st) = call_trap(st, p, trap_fn, [rt_types.mk_object(p.target)])
      use <- bool.guard(!rt_val.to_boolean(res), #(False, st))
      // Step 8: trap returned true → target must now be non-extensible.
      let #(target_ext, st) = t_is_extensible(st, p.target)
      case target_ext {
        True ->
          throw_type_error(
            st,
            "'preventExtensions' on proxy: trap returned truish but the proxy target is extensible",
          )
        False -> #(True, st)
      }
    }
  }
}

/// §10.5.5 Proxy [[GetOwnProperty]] ( P ).
fn proxy_get_own_property(
  st: Agent,
  p: Proxy,
  key: ObjectKey,
) -> #(Option(Property), Agent) {
  let #(trap, st) = proxy_trap(st, p, "getOwnPropertyDescriptor")
  case trap {
    // Step 6: no trap → target.[[GetOwnProperty]](P).
    None -> t_get_own_property(st, p.target, key)
    Some(trap_fn) -> {
      // Step 7: Call(trap, handler, « target, P »).
      let #(res, st) =
        call_trap(st, p, trap_fn, [
          rt_types.mk_object(p.target),
          object_key_value(key),
        ])
      case rt_types.classify(res) {
        // Steps 9-11: trap says "absent". Step 9 first reads
        // `? target.[[GetOwnProperty]](P)` — the target's OWN trap when the
        // target is itself a proxy, never a raw heap read.
        KUndef -> {
          let #(target_desc, st) = t_get_own_property(st, p.target, key)
          case target_desc {
            None -> #(None, st)
            Some(prop) ->
              case rt_types.prop_configurable(prop) {
                False ->
                  throw_type_error(
                    st,
                    "'getOwnPropertyDescriptor' on proxy: trap returned undefined for property "
                      <> key_quoted(key)
                      <> " which is non-configurable in the proxy target",
                  )
                True -> {
                  // Step 11.c: extensibleTarget = ? IsExtensible(target).
                  let #(ext, st) = t_is_extensible(st, p.target)
                  case ext {
                    False ->
                      throw_type_error(
                        st,
                        "'getOwnPropertyDescriptor' on proxy: trap returned undefined for property "
                          <> key_quoted(key)
                          <> " which exists in the non-extensible proxy target",
                      )
                    True -> #(None, st)
                  }
                }
              }
          }
        }
        // Steps 9-17: trap returned a descriptor object — validate.
        KHandle(_) -> {
          let #(target_desc, st) = t_get_own_property(st, p.target, key)
          // Step 12: extensibleTarget = ? IsExtensible(target).
          let #(ext, st) = t_is_extensible(st, p.target)
          // Steps 13-14: ? ToPropertyDescriptor, CompletePropertyDescriptor.
          let #(parsed, st) = t_to_property_descriptor(st, res)
          let completed = complete_descriptor(parsed)
          // Step 15: IsCompatiblePropertyDescriptor against the COMPLETED
          // descriptor.
          use <- bool.lazy_guard(
            !compatible_descriptor(
              ext,
              parsed_of_property(completed),
              target_desc,
            ),
            fn() {
              throw_type_error(
                st,
                "'getOwnPropertyDescriptor' on proxy: trap returned descriptor for property "
                  <> key_quoted(key)
                  <> " that is incompatible with the existing property in the proxy target",
              )
            },
          )
          // Step 16: resultDesc.[[Configurable]] false requires a matching
          // non-configurable target property.
          case rt_types.prop_configurable(completed), target_desc {
            True, _ -> #(Some(completed), st)
            False, None ->
              throw_type_error(
                st,
                "'getOwnPropertyDescriptor' on proxy: trap reported non-configurability for property "
                  <> key_quoted(key)
                  <> " which is non-existent in the proxy target",
              )
            False, Some(td) ->
              case rt_types.prop_configurable(td) {
                True ->
                  throw_type_error(
                    st,
                    "'getOwnPropertyDescriptor' on proxy: trap reported non-configurability for property "
                      <> key_quoted(key)
                      <> " which is configurable in the proxy target",
                  )
                False ->
                  // Step 16.b: IsDataDescriptor(resultDesc) with
                  // writable:false requires target's writable:false too.
                  // resultDesc is the COMPLETED descriptor from step 14 — an
                  // absent [[Writable]] in the trap result defaults to false
                  // and must still trip this invariant.
                  case completed, td {
                    DataProperty(writable: False, ..),
                      DataProperty(writable: True, ..)
                    ->
                      throw_type_error(
                        st,
                        "'getOwnPropertyDescriptor' on proxy: trap reported non-writability for property "
                          <> key_quoted(key)
                          <> " which is writable in the proxy target",
                      )
                    _, _ -> #(Some(completed), st)
                  }
              }
          }
        }
        // Step 8: neither Object nor Undefined → TypeError.
        _ ->
          throw_type_error(
            st,
            "'getOwnPropertyDescriptor' on proxy: trap returned neither object nor undefined for property "
              <> key_quoted(key),
          )
      }
    }
  }
}

/// §10.5.6 Proxy [[DefineOwnProperty]] ( P, Desc ). Returns the raw boolean —
/// callers decide whether false throws (DefinePropertyOrThrow) or not
/// (Reflect.defineProperty).
fn proxy_define_own_property(
  st: Agent,
  p: Proxy,
  key: ObjectKey,
  desc: ParsedDesc,
) -> #(Bool, Agent) {
  let #(trap, st) = proxy_trap(st, p, "defineProperty")
  case trap {
    // Step 6: no trap → target.[[DefineOwnProperty]](P, Desc). The raw
    // internal method returns a boolean — only a validation rejection maps to
    // `false` here (DefinePropertyOrThrow is applied by the CALLER of the
    // outermost proxy, not per level); genuine abrupt completions (e.g.
    // ArraySetLength's RangeError) propagate.
    None -> t_define_own_prop(st, p.target, key, desc)
    Some(trap_fn) -> {
      // Step 7: descObj = FromPropertyDescriptor(Desc) — a fresh object
      // carrying only the present fields.
      let #(desc_obj, st) = t_from_property_descriptor(st, desc)
      // Step 8: ToBoolean(? Call(trap, handler, « target, P, descObj »)).
      let #(res, st) =
        call_trap(st, p, trap_fn, [
          rt_types.mk_object(p.target),
          object_key_value(key),
          rt_types.mk_object(desc_obj),
        ])
      // Step 9: false → return false.
      use <- bool.guard(!rt_val.to_boolean(res), #(False, st))
      // Steps 10-11: invariants. Both reads are the target's own internal
      // methods (`? target.[[GetOwnProperty]](P)`, `? IsExtensible(target)`)
      // — traps fire when the target is itself a proxy.
      let #(target_desc, st) = t_get_own_property(st, p.target, key)
      let #(ext, st) = t_is_extensible(st, p.target)
      // Step 12-13: settingConfigFalse.
      let setting_config_false = desc.configurable == Some(False)
      case target_desc {
        // Step 14: targetDesc undefined.
        None -> {
          use <- bool.lazy_guard(!ext, fn() {
            throw_type_error(
              st,
              "'defineProperty' on proxy: trap returned truish for adding property "
                <> key_quoted(key)
                <> " to the non-extensible proxy target",
            )
          })
          use <- bool.lazy_guard(setting_config_false, fn() {
            throw_type_error(
              st,
              "'defineProperty' on proxy: trap returned truish for defining non-configurable property "
                <> key_quoted(key)
                <> " which is either non-existent or configurable in the proxy target",
            )
          })
          #(True, st)
        }
        // Step 15: targetDesc exists.
        Some(cur) -> {
          use <- bool.lazy_guard(
            !compatible_descriptor(ext, desc, Some(cur)),
            fn() {
              throw_type_error(
                st,
                "'defineProperty' on proxy: trap returned truish for adding property "
                  <> key_quoted(key)
                  <> " that is incompatible with the existing property in the proxy target",
              )
            },
          )
          use <- bool.lazy_guard(
            setting_config_false && rt_types.prop_configurable(cur),
            fn() {
              throw_type_error(
                st,
                "'defineProperty' on proxy: trap returned truish for defining non-configurable property "
                  <> key_quoted(key)
                  <> " which is either non-existent or configurable in the proxy target",
              )
            },
          )
          // Step 15.c: writable:false over a non-configurable writable data
          // property is rejected.
          case cur, desc.writable {
            DataProperty(configurable: False, writable: True, ..), Some(False)
            ->
              throw_type_error(
                st,
                "'defineProperty' on proxy: trap returned truish for defining non-writable property "
                  <> key_quoted(key)
                  <> " which is writable in the proxy target",
              )
            _, _ -> #(True, st)
          }
        }
      }
    }
  }
}

/// §10.5.7 Proxy [[HasProperty]] ( P ).
fn proxy_has(st: Agent, p: Proxy, key: ObjectKey) -> #(Bool, Agent) {
  let #(trap, st) = proxy_trap(st, p, "has")
  case trap {
    // Step 6: trap undefined → target.[[HasProperty]](P).
    None -> has_from(st, p.target, key)
    Some(trap_fn) -> {
      // Step 7: ToBoolean(? Call(trap, handler, « target, P »)).
      let #(res, st) =
        call_trap(st, p, trap_fn, [
          rt_types.mk_object(p.target),
          object_key_value(key),
        ])
      use <- bool.guard(rt_val.to_boolean(res), #(True, st))
      // Steps 8-9: invariants when the trap reports the key as absent,
      // starting from `? target.[[GetOwnProperty]](P)`.
      let #(target_desc, st) = t_get_own_property(st, p.target, key)
      case target_desc {
        None -> #(False, st)
        Some(prop) ->
          case rt_types.prop_configurable(prop) {
            False ->
              throw_type_error(
                st,
                "'has' on proxy: trap returned falsish for property "
                  <> key_quoted(key)
                  <> " which exists in the proxy target as non-configurable",
              )
            True -> {
              // Step 9.b.ii: ? IsExtensible(target) — traps when the target
              // is itself a proxy.
              let #(ext, st) = t_is_extensible(st, p.target)
              case ext {
                False ->
                  throw_type_error(
                    st,
                    "'has' on proxy: trap returned falsish for property "
                      <> key_quoted(key)
                      <> " but the proxy target is not extensible",
                  )
                True -> #(False, st)
              }
            }
          }
      }
    }
  }
}

/// §10.5.8 Proxy [[Get]] ( P, Receiver ).
fn proxy_get(
  st: Agent,
  p: Proxy,
  key: ObjectKey,
  receiver: JsVal,
) -> #(JsVal, Agent) {
  // Steps 1-5: revocation check + GetMethod(handler, "get").
  let #(trap, st) = proxy_trap(st, p, "get")
  case trap {
    // Step 6: trap undefined → target.[[Get]](P, Receiver).
    None -> get_from(st, p.target, key, receiver)
    Some(trap_fn) -> {
      // Step 7: Call(trap, handler, « target, P, Receiver »).
      let #(res, st) =
        call_trap(st, p, trap_fn, [
          rt_types.mk_object(p.target),
          object_key_value(key),
          receiver,
        ])
      // Steps 8-9: invariants against `? target.[[GetOwnProperty]](P)`.
      let #(target_desc, st) = t_get_own_property(st, p.target, key)
      case target_desc {
        Some(DataProperty(value: tv, writable: False, configurable: False, ..)) ->
          case same_value(res, tv) {
            True -> #(res, st)
            False ->
              throw_type_error(
                st,
                "'get' on proxy: property "
                  <> key_quoted(key)
                  <> " is a read-only and non-configurable data property on the proxy target but the proxy did not return its actual value",
              )
          }
        Some(AccessorProperty(get: None, configurable: False, ..)) ->
          case rt_types.classify(res) {
            KUndef -> #(res, st)
            _ ->
              throw_type_error(
                st,
                "'get' on proxy: property "
                  <> key_quoted(key)
                  <> " is a non-configurable accessor property on the proxy target without a getter, but the trap did not return undefined",
              )
          }
        _ -> #(res, st)
      }
    }
  }
}

/// §10.5.9 Proxy [[Set]] ( P, V, Receiver ).
fn proxy_set(
  st: Agent,
  p: Proxy,
  key: ObjectKey,
  v: JsVal,
  receiver: JsVal,
) -> #(Bool, Agent) {
  let #(trap, st) = proxy_trap(st, p, "set")
  case trap {
    // Step 6: trap undefined → target.[[Set]](P, V, Receiver).
    None -> set_from(st, p.target, key, v, receiver)
    Some(trap_fn) -> {
      // Step 7: ToBoolean(? Call(trap, handler, « target, P, V, Receiver »)).
      let #(res, st) =
        call_trap(st, p, trap_fn, [
          rt_types.mk_object(p.target),
          object_key_value(key),
          v,
          receiver,
        ])
      // Step 8: trap returned false → [[Set]] fails.
      use <- bool.guard(!rt_val.to_boolean(res), #(False, st))
      // Steps 9-10: invariants against `? target.[[GetOwnProperty]](P)`.
      let #(target_desc, st) = t_get_own_property(st, p.target, key)
      case target_desc {
        Some(DataProperty(value: tv, writable: False, configurable: False, ..)) ->
          case same_value(v, tv) {
            True -> #(True, st)
            False ->
              throw_type_error(
                st,
                "'set' on proxy: trap returned truish for property "
                  <> key_quoted(key)
                  <> " which exists in the proxy target as a non-configurable and non-writable data property with a different value",
              )
          }
        Some(AccessorProperty(set: None, configurable: False, ..)) ->
          throw_type_error(
            st,
            "'set' on proxy: trap returned truish for property "
              <> key_quoted(key)
              <> " which exists in the proxy target as a non-configurable accessor property without a setter",
          )
        _ -> #(True, st)
      }
    }
  }
}

/// §10.1.9.2 OrdinarySetWithOwnDescriptor steps 2.c-2.e with a Proxy
/// `Receiver`: existingDescriptor = ? Receiver.[[GetOwnProperty]](P) (the
/// §10.5.5 trap), then either false (accessor / non-writable existing),
/// ? Receiver.[[DefineOwnProperty]](P, { [[Value]]: V }) for an existing data
/// property, or ? CreateDataProperty(Receiver, P, V) — both the §10.5.6 trap.
fn set_on_proxy_receiver(
  st: Agent,
  recv_h: Handle,
  key: ObjectKey,
  v: JsVal,
) -> #(Bool, Agent) {
  let #(existing, st) = t_get_own_property(st, recv_h, key)
  case existing {
    // Step 2.d.i-ii: accessor or non-writable existing → false.
    Some(AccessorProperty(..)) -> #(False, st)
    Some(DataProperty(writable: False, ..)) -> #(False, st)
    // Step 2.d.iii-iv: Receiver.[[DefineOwnProperty]](P, { [[Value]]: V }).
    Some(DataProperty(..)) ->
      t_define_own_prop(
        st,
        recv_h,
        key,
        ParsedDesc(
          value: Some(v),
          get: None,
          set: None,
          writable: None,
          enumerable: None,
          configurable: None,
        ),
      )
    // Step 2.e: CreateDataProperty(Receiver, P, V).
    None -> t_define_own_data(st, recv_h, key, v, True, True, True)
  }
}

/// §10.5.10 Proxy [[Delete]] ( P ).
fn proxy_delete(st: Agent, p: Proxy, key: ObjectKey) -> #(Bool, Agent) {
  let #(trap, st) = proxy_trap(st, p, "deleteProperty")
  case trap {
    // Step 6: trap undefined → target.[[Delete]](P).
    None -> t_delete_prop(st, p.target, key)
    Some(trap_fn) -> {
      // Step 7: ToBoolean(? Call(trap, handler, « target, P »)).
      let #(res, st) =
        call_trap(st, p, trap_fn, [
          rt_types.mk_object(p.target),
          object_key_value(key),
        ])
      // Step 8: false → return false.
      use <- bool.guard(!rt_val.to_boolean(res), #(False, st))
      // Steps 9-13: invariants, starting from
      // `? target.[[GetOwnProperty]](P)`.
      let #(target_desc, st) = t_get_own_property(st, p.target, key)
      case target_desc {
        None -> #(True, st)
        Some(prop) ->
          case rt_types.prop_configurable(prop) {
            False ->
              throw_type_error(
                st,
                "'deleteProperty' on proxy: trap returned truish for property "
                  <> key_quoted(key)
                  <> " which is non-configurable in the proxy target",
              )
            True -> {
              // Step 12: ? IsExtensible(target).
              let #(ext, st) = t_is_extensible(st, p.target)
              case ext {
                False ->
                  throw_type_error(
                    st,
                    "'deleteProperty' on proxy: trap returned truish but the proxy target is not extensible",
                  )
                True -> #(True, st)
              }
            }
          }
      }
    }
  }
}

/// §10.5.11 Proxy [[OwnPropertyKeys]] ( ).
fn proxy_own_keys(st: Agent, p: Proxy) -> #(List(ObjectKey), Agent) {
  let #(trap, st) = proxy_trap(st, p, "ownKeys")
  case trap {
    // Step 6: no trap → target.[[OwnPropertyKeys]]().
    None -> t_own_keys(st, p.target)
    Some(trap_fn) -> {
      // Step 7: Call(trap, handler, « target »).
      let #(res, st) = call_trap(st, p, trap_fn, [rt_types.mk_object(p.target)])
      // Step 8: CreateListFromArrayLike(trapResultArray, property-key).
      let #(keys, st) = keys_from_array_like(st, res)
      // Step 9: duplicate entries are rejected.
      use <- bool.lazy_guard(has_duplicate_keys(keys, []), fn() {
        throw_type_error(
          st,
          "'ownKeys' on proxy: trap returned duplicate entries",
        )
      })
      // Step 10: extensibleTarget = ? IsExtensible(target).
      let #(ext, st) = t_is_extensible(st, p.target)
      // Steps 11-16: split target keys by configurability.
      let #(target_keys, st) = t_own_keys(st, p.target)
      let #(#(nonconf, conf), st) =
        partition_configurable(st, p.target, target_keys, [], [])
      // Step 17: extensible target with no non-configurable keys → done.
      use <- bool.guard(ext && nonconf == [], #(keys, st))
      // Step 19: every non-configurable target key must be reported.
      let missing = fn(required) {
        list.find(required, fn(k) { !list.contains(keys, k) })
      }
      use <- lazy_guard_found(missing(nonconf), fn(k) {
        throw_type_error(
          st,
          "'ownKeys' on proxy: trap result did not include "
            <> key_quoted(k)
            <> ", a non-configurable key of the proxy target",
        )
      })
      // Step 20: extensible target — no further checks.
      use <- bool.guard(ext, #(keys, st))
      // Step 21: every (configurable) target key must be reported…
      use <- lazy_guard_found(missing(conf), fn(k) {
        throw_type_error(
          st,
          "'ownKeys' on proxy: trap result did not include "
            <> key_quoted(k)
            <> ", a key of the non-extensible proxy target",
        )
      })
      // Step 22: …and no extra keys may be invented.
      case list.find(keys, fn(k) { !list.contains(target_keys, k) }) {
        Ok(_) ->
          throw_type_error(
            st,
            "'ownKeys' on proxy: trap returned extra keys but proxy target is non-extensible",
          )
        Error(Nil) -> #(keys, st)
      }
    }
  }
}

/// `use <- lazy_guard_found(search, on_found)`: divert to `on_found(x)` when
/// `search` found something, else continue.
fn lazy_guard_found(
  search: Result(a, Nil),
  on_found: fn(a) -> b,
  otherwise: fn() -> b,
) -> b {
  case search {
    Ok(x) -> on_found(x)
    Error(Nil) -> otherwise()
  }
}

/// §10.5.11 steps 11-16: partition `keys` into (non-configurable,
/// configurable), each key's descriptor read with
/// `? target.[[GetOwnProperty]](key)` — a trap when the target is a proxy
/// itself. Both lists come back in `keys` order.
fn partition_configurable(
  st: Agent,
  target: Handle,
  keys: List(ObjectKey),
  nonconf: List(ObjectKey),
  conf: List(ObjectKey),
) -> #(#(List(ObjectKey), List(ObjectKey)), Agent) {
  case keys {
    [] -> #(#(list.reverse(nonconf), list.reverse(conf)), st)
    [k, ..rest] -> {
      let #(prop, st) = t_get_own_property(st, target, k)
      let is_nonconf =
        option.map(prop, fn(p) { !rt_types.prop_configurable(p) })
        |> option.unwrap(False)
      case is_nonconf {
        True -> partition_configurable(st, target, rest, [k, ..nonconf], conf)
        False -> partition_configurable(st, target, rest, nonconf, [k, ..conf])
      }
    }
  }
}

/// True when `keys` contains the same String/Symbol value twice.
fn has_duplicate_keys(keys: List(ObjectKey), seen: List(ObjectKey)) -> Bool {
  case keys {
    [] -> False
    [k, ..rest] ->
      case list.contains(seen, k) {
        True -> True
        False -> has_duplicate_keys(rest, [k, ..seen])
      }
  }
}

/// §7.3.20 CreateListFromArrayLike ( obj, property-key ). Each element must
/// be a String or Symbol, else TypeError — so the result is a list of
/// ObjectKeys. Generic path: ? LengthOfArrayLike(obj), then ? Get(obj,
/// ToString(i)) for each index — honours array-like plain objects, getters
/// and proxies wrapping arrays.
fn keys_from_array_like(st: Agent, v: JsVal) -> #(List(ObjectKey), Agent) {
  case rt_types.classify(v) {
    KHandle(_) -> {
      // Step 2: Let len be ? LengthOfArrayLike(obj).
      let #(len_v, st) = t_get_prop(st, v, StringKey(Named("length")))
      let #(len, st) = rt_val.t_to_length(st, len_v)
      // One observable Get per index — bound by the iteration budget.
      use <- bool.lazy_guard(len > limits.max_iteration, fn() {
        throw_range_error(
          st,
          "'ownKeys' on proxy: trap result length exceeds iteration budget",
        )
      })
      gather_keys_via_get(st, v, 0, len, [])
    }
    // Step 1: not an Object → TypeError.
    _ -> throw_type_error(st, "CreateListFromArrayLike called on non-object")
  }
}

/// §7.3.20 steps 4-6: read indices 0..len-1 with observable Get, validating
/// each element is a String or Symbol.
fn gather_keys_via_get(
  st: Agent,
  obj: JsVal,
  idx: Int,
  len: Int,
  acc: List(ObjectKey),
) -> #(List(ObjectKey), Agent) {
  use <- bool.guard(idx >= len, #(list.reverse(acc), st))
  // Step 6.b: Let next be ? Get(obj, ToString(index)).
  let #(item, st) = t_get_prop(st, obj, StringKey(rt_types.index_key(idx)))
  // Step 6.c: validate the element type.
  case object_key_of_value(item) {
    Some(k) -> gather_keys_via_get(st, obj, idx + 1, len, [k, ..acc])
    None ->
      throw_type_error(
        st,
        "'ownKeys' on proxy: trap returned a non-String, non-Symbol key",
      )
  }
}

// ── Property Descriptor ⇄ Object (§6.2.6) ───────────────────────────────────

/// **ToPropertyDescriptor ( Obj )** — §6.2.6.5. Step 1 rejects a non-Object,
/// then the six fields are read (invoking getters / `has`+`get` traps),
/// get/set callability and the accessor/data conflict validated.
///
/// The read ORDER is normative and observable — a Proxy descriptor sees the
/// `has`/`get` traps in exactly this sequence: enumerable, configurable,
/// value, writable, get, set. Step 12.b's getter check also runs BEFORE
/// step 13 reads "set", so `{ get: 1, get set() { throw } }` is a plain
/// TypeError and never runs the `set` accessor. `None` = field absent (which
/// matters for descriptor merging), `Some(undefined)` = present. Port of arc
/// `mop.parse_descriptor` (`mop.gleam:1618-1676`).
pub fn t_to_property_descriptor(st: Agent, obj: JsVal) -> #(ParsedDesc, Agent) {
  // Step 1: If Obj is not an Object, throw a TypeError exception.
  case rt_types.classify(obj) {
    KHandle(_) -> Nil
    _ -> throw_type_error(st, "Property description must be an object")
  }
  // Steps 3-4.
  let #(enumerable, st) = read_desc_bool(st, obj, "enumerable")
  // Steps 5-6.
  let #(configurable, st) = read_desc_bool(st, obj, "configurable")
  // Steps 7-8.
  let #(value, st) = read_desc_field(st, obj, "value")
  // Steps 9-10.
  let #(writable, st) = read_desc_bool(st, obj, "writable")
  // Steps 11-12: read "get", then reject a non-callable one immediately.
  let #(get, st) = read_desc_field(st, obj, "get")
  let st = require_callable_accessor(st, get, "Getter")
  // Steps 13-14: only now is "set" observed at all.
  let #(set, st) = read_desc_field(st, obj, "set")
  let st = require_callable_accessor(st, set, "Setter")
  let desc =
    ParsedDesc(get:, set:, value:, writable:, enumerable:, configurable:)
  // Step 15: accessor and data attributes are mutually exclusive.
  case desc_is_accessor(desc) && desc_is_data(desc) {
    True ->
      throw_type_error(
        st,
        "Invalid property descriptor. Cannot both specify accessors and a value or writable attribute",
      )
    False -> #(desc, st)
  }
}

/// §6.2.6.5 steps 3/5/7/…: ? HasProperty(Obj, name), then — only if present
/// — ? Get(Obj, name). Interleaved per field, not batched.
fn read_desc_field(
  st: Agent,
  obj: JsVal,
  name: String,
) -> #(Option(JsVal), Agent) {
  let key = StringKey(Named(name))
  let #(present, st) = t_has_prop(st, obj, key)
  case present {
    False -> #(None, st)
    True -> {
      let #(v, st) = t_get_prop(st, obj, key)
      #(Some(v), st)
    }
  }
}

/// `read_desc_field` + ToBoolean (§7.1.2) for enumerable/configurable/
/// writable.
fn read_desc_bool(
  st: Agent,
  obj: JsVal,
  name: String,
) -> #(Option(Bool), Agent) {
  let #(field, st) = read_desc_field(st, obj, name)
  #(option.map(field, rt_val.to_boolean), st)
}

/// §6.2.6.5 steps 12.b / 14.b: a `get`/`set` field that is present, is not
/// undefined and is not callable is a TypeError. `role` is "Getter"/"Setter".
fn require_callable_accessor(
  st: Agent,
  field: Option(JsVal),
  role: String,
) -> Agent {
  case field {
    None -> st
    Some(f) ->
      case rt_types.classify(f) {
        KUndef -> st
        _ -> {
          let #(callable, st) = rt_val.t_is_callable(st, f)
          case callable {
            True -> st
            False -> throw_type_error(st, role <> " must be a function")
          }
        }
      }
  }
}

/// **FromPropertyDescriptor ( Desc )** — §6.2.6.4 on a possibly PARTIAL
/// descriptor: only fields present in `desc` become own `{W:T, E:T, C:T}`
/// data properties of a fresh `%Object.prototype%` object, in spec order
/// value, writable, get, set, enumerable, configurable. This is the object
/// the `defineProperty` trap receives (§10.5.6 step 7) and what
/// `Object.getOwnPropertyDescriptor` returns (via `parsed_of_property`).
pub fn t_from_property_descriptor(
  st: Agent,
  desc: ParsedDesc,
) -> #(Handle, Agent) {
  let field = fn(name, v: Option(JsVal)) {
    option.map(v, fn(x) { [#(name, x)] }) |> option.unwrap([])
  }
  let flag = fn(name, b: Option(Bool)) {
    field(name, option.map(b, rt_types.mk_bool))
  }
  alloc_plain(
    st,
    list.flatten([
      field("value", desc.value),
      flag("writable", desc.writable),
      field("get", desc.get),
      field("set", desc.set),
      flag("enumerable", desc.enumerable),
      flag("configurable", desc.configurable),
    ]),
  )
}

/// A stored `Property` as the fully-populated `ParsedDesc` it round-trips
/// to (accessor halves absent internally render as `undefined`).
pub fn parsed_of_property(prop: Property) -> ParsedDesc {
  case prop {
    DataProperty(value:, writable:, enumerable:, configurable:, ..) ->
      ParsedDesc(
        value: Some(value),
        writable: Some(writable),
        get: None,
        set: None,
        enumerable: Some(enumerable),
        configurable: Some(configurable),
      )
    AccessorProperty(get:, set:, enumerable:, configurable:, ..) ->
      ParsedDesc(
        value: None,
        writable: None,
        get: Some(option.unwrap(get, rt_types.mk_undefined())),
        set: Some(option.unwrap(set, rt_types.mk_undefined())),
        enumerable: Some(enumerable),
        configurable: Some(configurable),
      )
  }
}

/// §6.2.6.6 CompletePropertyDescriptor — fill absent fields with defaults,
/// yielding a concrete Property. seq: 0 — a trap-result descriptor is only
/// ever rendered or compared, never stored.
fn complete_descriptor(desc: ParsedDesc) -> Property {
  case desc_is_accessor(desc) {
    True ->
      AccessorProperty(
        get: accessor_field(desc.get, None),
        set: accessor_field(desc.set, None),
        enumerable: option.unwrap(desc.enumerable, False),
        configurable: option.unwrap(desc.configurable, False),
        seq: 0,
      )
    False ->
      DataProperty(
        value: option.unwrap(desc.value, rt_types.mk_undefined()),
        writable: option.unwrap(desc.writable, False),
        enumerable: option.unwrap(desc.enumerable, False),
        configurable: option.unwrap(desc.configurable, False),
        seq: 0,
      )
  }
}

/// §10.1.6.3 ValidateAndApplyPropertyDescriptor in validation-only mode
/// (IsCompatiblePropertyDescriptor §10.1.6.2): would defining `desc` over
/// `current` succeed on an object with the given extensibility?
fn compatible_descriptor(
  extensible: Bool,
  desc: ParsedDesc,
  current: Option(Property),
) -> Bool {
  case current {
    // Step 2: no current property — allowed iff extensible.
    None -> extensible
    Some(cur) -> is_compatible_descriptor(desc, cur)
  }
}

/// [[DefineOwnProperty]] with a fully-populated data descriptor
/// `{value, writable, enumerable, configurable}`. Thin `ParsedDesc` builder
/// over `t_define_own_prop` for method/field installation (M6/M7).
pub fn t_define_own_data(
  st: Agent,
  h: Handle,
  key: ObjectKey,
  value: JsVal,
  writable: Bool,
  enumerable: Bool,
  configurable: Bool,
) -> #(Bool, Agent) {
  t_define_own_prop(
    st,
    h,
    key,
    rt_types.ParsedDesc(
      value: Some(value),
      get: None,
      set: None,
      writable: Some(writable),
      enumerable: Some(enumerable),
      configurable: Some(configurable),
    ),
  )
}

/// [[DefineOwnProperty]] with an accessor descriptor `{get?, set?,
/// enumerable, configurable}`. `get`/`set` are `Option` so a lone getter or
/// setter half can be installed (M7 `t_define_method` merges halves).
pub fn t_define_own_accessor(
  st: Agent,
  h: Handle,
  key: ObjectKey,
  get: Option(JsVal),
  set: Option(JsVal),
  enumerable: Bool,
  configurable: Bool,
) -> #(Bool, Agent) {
  t_define_own_prop(
    st,
    h,
    key,
    rt_types.ParsedDesc(
      value: None,
      get:,
      set:,
      writable: None,
      enumerable: Some(enumerable),
      configurable: Some(configurable),
    ),
  )
}

// ── SPEC§8 op-table adapters (arc/emit_2core ABI) ───────────────────────────
// arc's M12/M14 emit `CallHost("js", op, args)` per the SPEC§8 table; the
// existing M4 primitives above have slightly different arg types (ObjectKey
// vs the wire PropertyKey/tuple arc emits, missing global/arguments helpers).
// These wrappers bridge the gap without touching the frozen arc modules.

/// arc emits static keys as bare `PropertyKey` (`{named,_}`/`{index,_}`) and
/// computed keys via `to_property_key` → `ObjectKey`. Normalise both to the
/// `ObjectKey` the M4 primitives take. Tagged-record tag matched at the wire
/// level (see `arc_rt_store_ffi:as_object_key/1`).
@external(erlang, "arc_rt_store_ffi", "as_object_key")
fn as_object_key(key: k) -> ObjectKey

@external(erlang, "arc_rt_store_ffi", "identity")
fn unsafe_coerce(a: a) -> b

@external(erlang, "erlang", "is_list")
fn is_list(a: a) -> Bool

/// SPEC§8 `get_prop` — [[Get]] with a wire-form key (arc emits both bare
/// `PropertyKey` for static `.x` and `ObjectKey` for computed `[e]`).
pub fn t_get_prop_any(st: Agent, recv: JsVal, key: k) -> #(JsVal, Agent) {
  t_get_prop(st, recv, as_object_key(key))
}

/// SPEC§8 `set_prop` — [[Set]] with a wire-form key.
pub fn t_set_prop_any(
  st: Agent,
  recv: JsVal,
  key: k,
  v: JsVal,
) -> #(Bool, Agent) {
  t_set_prop(st, recv, as_object_key(key), v)
}

/// SPEC§8 `define_prop` — §7.3.5 CreateDataProperty(OrThrow) with a wire-form
/// key. Object-literal `{k: v}` emits this with a raw JsVal `v` (NOT a
/// ParsedDesc), so route to `t_define_own_data` with all-true attributes.
pub fn t_create_data_prop(
  st: Agent,
  recv: JsVal,
  key: k,
  v: JsVal,
) -> #(Bool, Agent) {
  case rt_types.classify(recv) {
    KHandle(h) ->
      t_define_own_data(st, h, as_object_key(key), v, True, True, True)
    _ ->
      throw_type_error(
        st,
        "Cannot define property '"
          <> key_text(as_object_key(key))
          <> "' on "
          <> case rt_types.classify(recv) {
          KNull -> "null"
          KUndef -> "undefined"
          _ -> "primitive"
        },
      )
  }
}

/// SPEC§8 `global_get` — read `name` from the realm's global object. Throws
/// `ReferenceError` if the name is absent (§9.1.1.4.1 step 4) via M4's
/// ordinary [[Get]] returning `undefined`; arc's M12 handles the strict-mode
/// unresolved-reference throw at the emit layer, so this returns `undefined`
/// for a missing binding rather than throwing.
pub fn t_global_get(st: Agent, name: BitArray) -> #(JsVal, Agent) {
  let g = st.realm.global_object
  t_get_prop(st, rt_types.mk_object(g), StringKey(binary_key(name)))
}

/// SPEC§8 `global_set` — `PutValue` on the global object (§9.1.1.4.5). arc's
/// emit handles the strict-mode throw-on-failure; this drops the `Bool` result.
pub fn t_global_set(st: Agent, name: BitArray, v: JsVal) -> Agent {
  let g = st.realm.global_object
  let #(_, st) =
    t_set_prop(st, rt_types.mk_object(g), StringKey(binary_key(name)), v)
  st
}

/// SPEC§8 `global_typeof` — ES2024 §13.5.3 `typeof <ident>` where `<ident>` is
/// an unresolvable global Reference yields `"undefined"` without throwing. If
/// the binding exists on the global object, read it and delegate to `t_type_of`.
pub fn t_global_typeof(st: Agent, name: BitArray) -> #(String, Agent) {
  let g = st.realm.global_object
  let key = StringKey(binary_key(name))
  let #(has, st) = t_has_prop(st, rt_types.mk_object(g), key)
  case has {
    False -> #("undefined", st)
    True -> {
      let #(v, st) = t_get_prop(st, rt_types.mk_object(g), key)
      rt_val.t_type_of(st, v)
    }
  }
}

fn binary_key(name: BitArray) -> PropertyKey {
  case bit_array.to_string(name) {
    Ok(s) -> rt_types.canonical_key(s)
    Error(_) -> Named("")
  }
}

/// SPEC§8 `new_arguments` (M14) — allocate an Arguments exotic object.
/// `args` is the raw incoming `_args` list; `mapped` is either `undefined`
/// (unmapped: strict or non-simple params, §10.4.4.7) or a cons-list of
/// parameter cell handles (mapped, §10.4.4.6). Elements are the args in
/// creation order; `length` and `callee` are ordinary own props (`callee`
/// is the %ThrowTypeError% accessor when unmapped); `@@iterator` is
/// %Array.prototype.values%. `mapped` cell aliasing is handled by
/// [[Get]]/[[Set]] via the `ArgumentsObj` kind.
pub fn t_new_arguments(
  st: Agent,
  args: List(JsVal),
  mapped: m,
  callee: JsVal,
) -> #(JsVal, Agent) {
  let len = list.length(args)
  // `mapped` is either the atom `undefined` (unmapped/strict) or a cons-list
  // of param slot values (sloppy simple param list). Discriminate at wire
  // level — never `classify` (a list is not a `JsVal`).
  let mapped_cells = case is_list(mapped) {
    True -> Some(unsafe_coerce(mapped))
    False -> None
  }
  let elements = tree_array.from_list(args, rt_types.mk_hole())
  let #(seq, st) = rt_store.t_next_prop_seq(st)
  let length_prop =
    DataProperty(
      value: rt_types.mk_number(rt_types.JInt(len)),
      writable: True,
      enumerable: False,
      configurable: True,
      seq:,
    )
  let #(seq, st) = rt_store.t_next_prop_seq(st)
  let callee_prop = case mapped_cells {
    Some(_) ->
      DataProperty(
        value: callee,
        writable: True,
        enumerable: False,
        configurable: True,
        seq:,
      )
    None -> {
      let thrower = Some(rt_types.mk_object(st.realm.throw_type_error))
      AccessorProperty(
        get: thrower,
        set: thrower,
        enumerable: False,
        configurable: False,
        seq:,
      )
    }
  }
  let props =
    dict.from_list([
      #(Named("length"), length_prop),
      #(Named("callee"), callee_prop),
    ])
  let symbol_props = case
    t_ordinary_own_property(
      st,
      st.realm.array.prototype,
      SymbolKey(rt_types.symbol_iterator),
    )
  {
    Some(values_prop) -> [#(rt_types.symbol_iterator, values_prop)]
    None -> []
  }
  let #(h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: ArgumentsObj(length: len, mapped: mapped_cells),
        proto: Some(st.realm.object.prototype),
        props:,
        symbol_props:,
        elements: Dense(elements),
        extensible: True,
      ),
    )
  #(rt_types.mk_object(h), st)
}

/// SPEC§8 `new_array` (M12 array literal) — allocate an Array exotic with
/// `elems` as its dense elements and `length: |elems|`. An elision arrives as
/// `mk_hole()`, the store's own absent-index marker, and stays a hole.
pub fn t_new_array(st: Agent, elems: List(JsVal)) -> #(JsVal, Agent) {
  let len = list.length(elems)
  let elements = tree_array.from_list(elems, rt_types.mk_hole())
  let #(h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: ArrayObj(length: len),
        proto: Some(st.realm.array.prototype),
        props: dict.new(),
        symbol_props: [],
        elements: Dense(elements),
        extensible: True,
      ),
    )
  #(rt_types.mk_object(h), st)
}
