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
  JsStore, KHandle, KNull, KUndef, Named, NoElements, Ordinary, Private,
  ProxyObj, SAsyncGen, SBox, SGenerator, SObject, SPromise, SShapedObject,
  ShapeDesc, StringKey, StringObj, SymbolKey, TypeErr, TypedArrayObj,
} as rt_types
import arc/rt/val as rt_val
import arc/vm/internal/tree_array
import arc/vm/js_string
import gleam/bit_array
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set

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

/// Read the `SObject` cell backing property MOP for `h`. `SGenerator` /
/// `SAsyncGen` delegate to their `gen_cell` shell (a real `SObject` whose
/// proto reaches `%GeneratorPrototype%` / `%AsyncGeneratorPrototype%` — see
/// `rt_async.t_gen_start`/`t_asyncgen_start`). `SPromise` (single-cell,
/// no shell) synthesizes a proto-only view onto `%Promise.prototype%` with
/// `extensible: False` so every write path rejects BEFORE reaching a
/// `t_cell_update` that would panic on the non-`SObject` cell. `SBox` is an
/// internal capture cell — never a JS receiver.
fn read_object(st: Agent, h: Handle) -> JsSlot {
  case rt_store.t_cell_get(st, h) {
    SObject(..) as obj -> obj
    SGenerator(gen_cell:, ..) -> read_object(st, gen_cell)
    SAsyncGen(gen_cell:, ..) -> read_object(st, gen_cell)
    SPromise(..) ->
      SObject(
        kind: Ordinary,
        proto: Some(st.realm.promise.prototype),
        props: dict.new(),
        symbol_props: [],
        elements: NoElements,
        extensible: False,
      )
    // Shaped-direct: hot-path callers handle via `own_property_shaped`;
    // write-path callers `devolve` first. Avoids the `as_sobject` dict.fold
    // rebuild (~87% of the perf5 raytrace regression).
    SShapedObject(..) as s -> s
    SBox(..) ->
      panic as "rt_obj: SBox capture cell used as JS receiver (engine invariant)"
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

/// Resolve `h` to the `Handle` whose cell is the actual `SObject` that
/// `t_cell_update` should mutate for a property MOP write on `h`.
/// `SGenerator`/`SAsyncGen` redirect to their `gen_cell` shell so own-prop
/// writes land on the shell; `SObject`/`SPromise`/`SBox` return `h` unchanged
/// (`SPromise` writes never reach `t_cell_update` — `read_object` reports it
/// non-extensible so every write guard rejects first).
fn resolve_object_handle(st: Agent, h: Handle) -> Handle {
  case rt_store.t_cell_get(st, h) {
    SGenerator(gen_cell:, ..) -> resolve_object_handle(st, gen_cell)
    SAsyncGen(gen_cell:, ..) -> resolve_object_handle(st, gen_cell)
    _ -> h
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
    StringObj(value: s), Named("length") ->
      Some(DataProperty(
        value: rt_types.mk_number(rt_types.JInt(js_string.length(s))),
        writable: False,
        enumerable: False,
        configurable: False,
        seq: 0,
      ))
    StringObj(value: s), Index(i) ->
      case js_string.char_at(s, i) {
        Some(ch) ->
          Some(DataProperty(
            value: rt_types.mk_string(ch),
            writable: False,
            enumerable: True,
            configurable: False,
            seq: 0,
          ))
        None -> dict.get(props, key) |> option.from_result
      }
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
    // TODO(M6): ModuleNamespace/ProxyObj exotic [[GetOwnProperty]] — falls
    // through to §10.1.5.1 OrdinaryGetOwnProperty.
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

/// §10.1.1 [[GetPrototypeOf]]. Pure read; state threaded per R1 shape.
pub fn t_get_prototype_of(st: Agent, obj: Handle) -> #(Option(Handle), Agent) {
  case read_object(st, obj) {
    SObject(proto:, ..) | SShapedObject(proto:, ..) -> #(proto, st)
    _ -> #(None, st)
  }
}

/// §10.1.2.1 OrdinarySetPrototypeOf. Returns `#(True, st')` on success,
/// `#(False, st)` when rejected (non-extensible or would create a cycle).
/// Port of arc `mop.ordinary_set_prototype_of` (`mop.gleam:1278-1327`).
pub fn t_set_prototype(
  st: Agent,
  obj: Handle,
  new_proto: Option(Handle),
) -> #(Bool, Agent) {
  let obj = resolve_object_handle(st, obj)
  let st = devolve(st, obj)
  let assert SObject(proto: current, extensible:, ..) = read_object(st, obj)
  // Step 4: SameValue(V, current) → true (no-op).
  use <- bool.guard(new_proto == current, #(True, st))
  // Step 5: extensible false → false.
  use <- bool.guard(!extensible, #(False, st))
  // Step 7: cycle check.
  use <- bool.guard(would_create_cycle(st, obj, new_proto), #(False, st))
  // Step 8: set [[Prototype]] to V.
  let st =
    rt_store.t_cell_update(st, obj, fn(slot) {
      let assert SObject(..) = slot
      SObject(..slot, proto: new_proto)
    })
  #(True, st)
}

/// SPEC §8 op-table spelling — thin alias for `t_get_prototype_of`.
pub fn t_get_proto(st: Agent, obj: Handle) -> #(Option(Handle), Agent) {
  t_get_prototype_of(st, obj)
}

/// SPEC §8 op-table spelling — thin alias for `t_set_prototype`.
pub fn t_set_proto(
  st: Agent,
  obj: Handle,
  new_proto: Option(Handle),
) -> #(Bool, Agent) {
  t_set_prototype(st, obj, new_proto)
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
  // TODO(M6): ModuleNamespace/ProxyObj exotic [[Get]] dispatch on `kind`.
  case read_object(st, h), key {
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
  // TODO(M6): ModuleNamespace/ProxyObj exotic [[Set]] dispatch on `kind`.
  case read_object(st, h), key {
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
      let recv_h = resolve_object_handle(st, recv_h)
      case read_object(st, recv_h), key {
        SShapedObject(shape_id:, proto:, slots:), StringKey(Named(name)) ->
          set_own_shaped(st, recv_h, shape_id, proto, slots, name, v)
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
    // §10.4.2.1 step 1: Array "length" → ArraySetLength (§10.4.2.4).
    ArrayObj(length:), Named("length") -> {
      let length_writable = case dict.get(props, key) {
        Ok(DataProperty(writable: w, ..)) -> w
        _ -> True
      }
      case length_writable {
        False -> #(False, st)
        True -> array_set_length(st, h, v, length)
      }
    }
    // §10.4.2.1 step 2 / §10.4.4.2: array/arguments index write.
    ArrayObj(length:), Index(i) -> {
      let length_writable = case dict.get(props, Named("length")) {
        Ok(DataProperty(writable: w, ..)) -> w
        _ -> True
      }
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
    // TODO(M6): StringObj/ModuleNamespace exotic receiver-write — falls
    // through to §10.1.6.3 ordinary (arc `set_string_property`).
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

/// §10.4.2.4 ArraySetLength (value-only Desc). Shrinking truncates elements
/// and dict Index overrides. Throws RangeError (via D7 raise) on a non-uint32
/// value. Port of arc `array_set_length` (`object.gleam:1429-1509`).
fn array_set_length(
  st: Agent,
  h: Handle,
  v: JsVal,
  old_len: Int,
) -> #(Bool, Agent) {
  let new_len = case rt_types.classify(v) {
    rt_types.KNum(rt_types.JInt(n))
      if n >= 0 && n <= rt_types.max_array_length
    -> n
    rt_types.KNum(rt_types.JFloat(f)) ->
      case rt_types.array_index_of_float(f) {
        // array_index_of_float caps at 2^32-2; length may be 2^32-1.
        Some(n) -> n
        None ->
          case f == int.to_float(rt_types.max_array_length) {
            True -> rt_types.max_array_length
            False -> throw_range_error(st, "Invalid array length")
          }
      }
    _ -> throw_range_error(st, "Invalid array length")
  }
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

/// §10.1.6.3 ValidateAndApplyPropertyDescriptor — the ordinary
/// [[DefineOwnProperty]]. Returns `#(True, st')` on success, `#(False, st)`
/// on rejection (non-extensible + new key, or `Desc` incompatible with a
/// non-configurable current). Port of arc `mop.ordinary_define`
/// (`mop.gleam:814-1060`) with the throw replaced by a `False` return (spec
/// [[DefineOwnProperty]] returns Bool; DefinePropertyOrThrow is the caller's
/// job). Array/Arguments index keys route through the elements store.
pub fn t_define_own_prop(
  st: Agent,
  obj: Handle,
  key: ObjectKey,
  desc: ParsedDesc,
) -> #(Bool, Agent) {
  let obj = resolve_object_handle(st, obj)
  let st = devolve(st, obj)
  let assert SObject(kind:, props:, symbol_props:, elements:, extensible:, ..) =
    read_object(st, obj)
  // §10.4.5.3 TypedArray (Integer-Indexed) [[DefineOwnProperty]]: canonical
  // numeric index keys never reach the ordinary property table — they
  // validate against the fixed {W:T, E:T, C:T} element descriptor and store
  // through IntegerIndexedElementSet. Everything else is ordinary.
  use <- typed_array_define(st, kind, key, desc)
  let indexed_kind = case kind {
    ArrayObj(_) | ArgumentsObj(..) -> True
    _ -> False
  }
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
    // accessor/configurable/enumerable/non-writable violations.
    ArrayObj(length: old_len), StringKey(Named("length") as pk) -> {
      let #(len_ok, st) = case desc.value {
        Some(v) -> array_set_length(st, obj, v, old_len)
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

/// The §10.4.5.3 dispatch head of [[DefineOwnProperty]]: absorb canonical
/// numeric index keys on a TypedArray, else continue with the ordinary body.
fn typed_array_define(
  st: Agent,
  kind: ObjKind,
  key: ObjectKey,
  desc: ParsedDesc,
  ordinary: fn() -> #(Bool, Agent),
) -> #(Bool, Agent) {
  case kind, key {
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
    KHandle(h) -> #(has_from(st, h, key), st)
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
      #(has_from(st, h, key), st)
    }
  }
}

fn has_from(st: Agent, h: Handle, key: ObjectKey) -> Bool {
  case key {
    StringKey(Private(_)) -> False
    _ -> {
      // TODO(M6): ModuleNamespace/ProxyObj exotic [[HasProperty]] dispatch.
      let slot = read_object(st, h)
      // Step 1-2: Let hasOwn be O.[[GetOwnProperty]](P). If not undefined,
      // return true.
      let #(own, proto) = own_and_proto_of_slot(st, slot, key)
      case own {
        Some(_) -> True
        // §10.4.5.2 TypedArray [[HasProperty]]: a canonical numeric index key
        // answers IsValidIntegerIndex directly — own_property_of already said
        // the index is invalid, so the answer is false WITHOUT consulting the
        // prototype chain (TypedArray.prototype["1.5"] is unreachable).
        None ->
          case typed_array_absorbs(slot, key) {
            True -> False
            False ->
              // Step 3-5: parent.[[HasProperty]] or false.
              case proto {
                Some(parent) -> has_from(st, parent, key)
                None -> False
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
  let obj = resolve_object_handle(st, obj)
  let st = devolve(st, obj)
  let assert SObject(kind:, props:, symbol_props:, elements:, ..) =
    read_object(st, obj)
  case key {
    SymbolKey(sym) ->
      case list.key_pop(symbol_props, sym) {
        Ok(#(prop, rest)) ->
          case rt_types.prop_configurable(prop) {
            False -> #(False, st)
            True -> write_symbol_props(st, obj, rest)
          }
        Error(Nil) -> #(True, st)
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
        // TODO(M6): StringObj/ModuleNamespace/ProxyObj exotic [[Delete]] —
        // falls through to §10.1.10.1 OrdinaryDelete.
        _, _ -> ordinary_delete()
      }
    }
  }
}

// ── [[OwnPropertyKeys]] (§10.1.11) ────────────────────────────────────────────

/// §10.1.11 OrdinaryOwnPropertyKeys — ES enumeration order: integer-index
/// ascending, then string keys by insertion (`Property.seq`), then symbols
/// (creation order — `symbol_props` is an assoc list). `Private(_)` keys are
/// never returned (SPEC §7.M4 invariant). Port of arc
/// `own_string_keys_flagged` + `collect_own_symbol_keys`
/// (`object.gleam:2333-2410`, `mop.gleam:1201`).
pub fn t_own_keys(st: Agent, obj: Handle) -> #(List(ObjectKey), Agent) {
  // Enumeration needs the full props dict — materialize (slow-path only).
  let assert SObject(kind:, props:, symbol_props:, elements:, ..) =
    as_sobject(st, read_object(st, obj))
  let is_array = case kind {
    ArrayObj(_) -> True
    _ -> False
  }
  // Elements-store indices — always own data properties. TypedArray
  // synthesizes its live indices (§10.4.5.7 step 2.a).
  let elem_idx = case kind {
    ArrayObj(length:) | ArgumentsObj(length:, ..) ->
      elements.indices(elements) |> list.filter(fn(i) { i < length })
    TypedArrayObj(buffer: buf, elem_kind:, byte_offset:, length:) -> {
      let n =
        buffer.typed_array_live_count(st, buf, elem_kind, byte_offset, length)
      int.range(from: n - 1, to: -1, with: [], run: fn(acc, i) { [i, ..acc] })
    }
    // TODO(M6): StringObj/ModuleNamespace exotic [[OwnPropertyKeys]]
    // index-range synth — emits dict-only for those kinds.
    _ -> []
  }
  // Split dict entries. Array's dict "length" only tracks frozen attributes;
  // the visible key is emitted as `length_key` below.
  let #(dict_idx, named) =
    dict.fold(props, #([], []), fn(acc, k, prop) {
      let #(idx, named) = acc
      case k {
        Index(i) -> #([i, ..idx], named)
        Named("length") if is_array -> acc
        Private(_) -> acc
        Named(_) -> #(idx, [#(rt_types.prop_seq(prop), k), ..named])
      }
    })
  // Step 1: array-index keys ascending. An index lives in exactly one store.
  let index_keys =
    list.append(elem_idx, dict_idx)
    |> list.sort(int.compare)
    |> list.map(fn(i) { StringKey(Index(i)) })
  // Array virtual "length" exists from birth — before any user Named key.
  let length_key = case is_array {
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

/// SPEC§8 `for_in_keys` — §14.7.5.9 EnumerateObjectProperties. Eager cons-list
/// of JS string values for `for (k in obj)`. `null`/`undefined` → `[]`
/// (§14.7.5.6 step 6.a); primitives box via `ops.to_object`. Port of arc
/// `enumerate_keys` (`object.gleam:2412-2460`).
pub fn t_for_in_keys(st: Agent, obj: JsVal) -> #(List(JsVal), Agent) {
  case rt_types.classify(obj) {
    KUndef | KNull -> #([], st)
    KHandle(h) -> for_in_keys_loop(st, Some(h), set.new(), [])
    _ -> {
      let #(h, st) = js_ops(st).to_object(st, obj)
      for_in_keys_loop(st, Some(h), set.new(), [])
    }
  }
}

/// Proto-chain walk for `t_for_in_keys`. Per level: `t_own_keys` gives §10.1.11
/// order; symbols dropped; a non-enumerable own key still SHADOWS an enumerable
/// proto key (§14.7.5.9) — `seen` records both.
fn for_in_keys_loop(
  st: Agent,
  current: Option(Handle),
  seen: set.Set(String),
  acc: List(JsVal),
) -> #(List(JsVal), Agent) {
  case current {
    None -> #(list.reverse(acc), st)
    Some(h) -> {
      let #(keys, st) = t_own_keys(st, h)
      let #(acc, seen) =
        list.fold(keys, #(acc, seen), fn(state, key) {
          let #(a, s) = state
          case key {
            SymbolKey(_) -> state
            StringKey(pk) -> {
              let name = rt_types.key_to_text(pk)
              case set.contains(s, name) {
                True -> state
                False -> {
                  let s = set.insert(s, name)
                  let enumerable = case t_get_own_property(st, h, key) {
                    Some(prop) -> rt_types.prop_enumerable(prop)
                    None -> False
                  }
                  case enumerable {
                    True -> #([rt_types.mk_string(name), ..a], s)
                    False -> #(a, s)
                  }
                }
              }
            }
          }
        })
      let #(proto, st) = t_get_prototype_of(st, h)
      for_in_keys_loop(st, proto, seen, acc)
    }
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

/// §10.1.5.1 [[GetOwnProperty]](P) — the raw own-descriptor lookup with NO
/// prototype walk. JRead: no state threaded. Private-name lookup (M7
/// `t_private_get`/`t_private_in`) and `Object.getOwnPropertyDescriptor` /
/// `Reflect.getOwnPropertyDescriptor` land here.
pub fn t_get_own_property(
  st: Agent,
  h: Handle,
  key: ObjectKey,
) -> Option(Property) {
  let #(own, _proto) = read_own_and_proto(st, h, key)
  own
}

/// §10.1.3.1 [[IsExtensible]]. JRead: no state threaded.
pub fn t_is_extensible(st: Agent, h: Handle) -> Bool {
  case read_object(st, h) {
    SObject(extensible:, ..) -> extensible
    SShapedObject(..) -> True
    _ -> False
  }
}

/// §10.1.4.1 [[PreventExtensions]] — set `[[Extensible]]` to `false`.
/// Short-circuits when already non-extensible (spec no-op; keeps the
/// `SPromise`-never-reaches-`t_cell_update` invariant of `read_object`).
pub fn t_prevent_extensions(st: Agent, h: Handle) -> Agent {
  let h = resolve_object_handle(st, h)
  let st = devolve(st, h)
  let assert SObject(extensible:, ..) = read_object(st, h)
  use <- bool.guard(!extensible, st)
  rt_store.t_cell_update(st, h, fn(slot) {
    let assert SObject(..) = slot
    SObject(..slot, extensible: False)
  })
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
/// (unmapped/strict) or a cons-list of parameter cell handles (sloppy simple
/// param list, §10.4.4). Elements are the args in creation order; `length`
/// is a data prop; `mapped` cell aliasing is handled by [[Get]]/[[Set]] via
/// the `ArgumentsObj` kind. Returns the object handle as a `JsVal`.
pub fn t_new_arguments(
  st: Agent,
  args: List(JsVal),
  mapped: m,
) -> #(JsVal, Agent) {
  let len = list.length(args)
  // `mapped` is either the atom `undefined` (unmapped/strict) or a cons-list
  // of param slot values (sloppy simple param list). Discriminate at wire
  // level — never `classify` (a list is not a `JsVal`).
  let mapped_cells = case is_list(mapped) {
    True -> Some(unsafe_coerce(mapped))
    False -> None
  }
  let elements = tree_array.from_list(args, rt_types.mk_undefined())
  // §10.4.4.6/7 step 20/21: "length" is an ORDINARY own data prop
  // {W:T,E:F,C:T} seeded at construction (arc interpreter.gleam:4947) — not
  // synthesized in [[GetOwnProperty]], so delete + own-keys behave ordinarily.
  let #(seq, st) = rt_store.t_next_prop_seq(st)
  let props =
    dict.from_list([
      #(
        Named("length"),
        DataProperty(
          value: rt_types.mk_number(rt_types.JInt(len)),
          writable: True,
          enumerable: False,
          configurable: True,
          seq:,
        ),
      ),
    ])
  let #(h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: ArgumentsObj(length: len, mapped: mapped_cells),
        proto: Some(st.realm.object.prototype),
        props:,
        symbol_props: [],
        elements: Dense(elements),
        extensible: True,
      ),
    )
  #(rt_types.mk_object(h), st)
}

/// SPEC§8 `new_array` (M12 array literal) — allocate an Array exotic with
/// `elems` as its dense elements and `length: |elems|`.
pub fn t_new_array(st: Agent, elems: List(JsVal)) -> #(JsVal, Agent) {
  let len = list.length(elems)
  let elements = tree_array.from_list(elems, rt_types.mk_undefined())
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
