import arc/internal/tree_array
import arc/rt/buffer
import arc/rt/bytecode.{FuncTemplate}
import arc/rt/elements
import arc/rt/js_string
import arc/rt/limits
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsElements, type JsOps, type JsSlot,
  type JsStore, type JsVal, type ObjKind, type ObjectKey, type ParsedDesc,
  type Property, type PropertyKey, type SymbolId, type TypedArrayKind,
  AccessorProperty, Agent, ArgumentsObj, ArrayObj, BirthPending, BirthSettled,
  DataProperty, Dense, Index, JsStore, KBytecode, KCompiled, KHandle, KNull,
  KTdz, KUndef, ModuleNamespace, Named, NoElements, Ordinary, ParsedDesc,
  Private, ProxyObj, SAsyncContext, SAsyncGen, SBox, SDisposeCapability,
  SGenerator, SObject, SPromiseData, SShapedObject, ShapeDesc, StringKey,
  StringObj, SymbolKey, TypeErr, TypedArrayObj,
} as rt_types
import arc/rt/val as rt_val
import gleam/bit_array
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set
import gleam/string

fn require_js(st: Agent) -> JsStore(Agent) {
  st.store
}

// upcall table seeded by init_realm
fn js_ops(st: Agent) -> JsOps(Agent) {
  require_js(st).ops
}

fn throw_type_error(st: Agent, msg: String) -> a {
  let #(e, st) = js_ops(st).new_error(st, TypeErr, msg)
  rt_store.t_throw(st, e)
}

// returns SObject or SShapedObject only
fn read_object(st: Agent, h: Handle) -> JsSlot {
  case rt_store.t_cell_get(st, h) {
    SObject(..) as obj -> obj
    SShapedObject(..) as s -> s
    SBox(..)
    | SPromiseData(..)
    | SGenerator(..)
    | SAsyncGen(..)
    | SAsyncContext(..)
    | SDisposeCapability(..) ->
      panic as "rt_obj: internal data cell used as JS receiver (engine invariant)"
  }
}

fn own_property_shaped(
  offsets: Dict(BitArray, Int),
  slots: rt_types.ShapeSlots,
  key: PropertyKey,
) -> Option(Property) {
  case key {
    Private(_) -> None
    _ ->
      case dict.get(offsets, bit_array.from_string(rt_types.key_to_text(key))) {
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
  }
}

fn read_own_and_proto(
  st: Agent,
  h: Handle,
  key: ObjectKey,
) -> #(Option(Property), Option(Handle)) {
  own_and_proto_of_slot(st, read_object(st, h), key)
}

fn own_and_proto_of_slot(
  st: Agent,
  slot: JsSlot,
  key: ObjectKey,
) -> #(Option(Property), Option(Handle)) {
  case slot {
    SShapedObject(offsets:, proto:, slots:, ..) -> #(
      case key {
        StringKey(pk) -> own_property_shaped(offsets, slots, pk)
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
    _ -> #(None, None)
  }
}

// §10.4.5 numeric index keys never reach the proto chain
fn typed_array_absorbs(slot: JsSlot, key: ObjectKey) -> Bool {
  case slot, key {
    SObject(kind: TypedArrayObj(..), ..), StringKey(Index(_)) -> True
    SObject(kind: TypedArrayObj(..), ..), StringKey(Named(s)) ->
      buffer.is_canonical_numeric_string(s)
    _, _ -> False
  }
}

pub fn as_sobject(slot: JsSlot) -> JsSlot {
  case slot {
    SShapedObject(proto:, slots:, offsets:, ..) -> {
      let props =
        dict.fold(offsets, dict.new(), fn(acc, key_bin, off) {
          let value = rt_types.shape_slots_get(slots, off)
          let key = case bit_array.to_string(key_bin) {
            Ok(s) -> rt_types.canonical_key(s)
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

// write paths call this so the update sees a plain SObject
pub fn devolve(st: Agent, h: Handle) -> Agent {
  case rt_store.t_cell_get(st, h) {
    SShapedObject(..) as s -> rt_store.t_cell_set(st, h, as_sobject(s))
    _ -> st
  }
}

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

// props dict wins over dense elements, check it first
fn own_property_of(
  st: Agent,
  kind: ObjKind,
  props: Dict(PropertyKey, Property),
  elements: JsElements,
  key: PropertyKey,
) -> Option(Property) {
  case kind, key {
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
        // seq 0: index keys enumerate numerically
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
    TypedArrayObj(..), Private(_) -> dict.get(props, key) |> option.from_result
    // §10.4.2 length value tracks ArrayObj, dict holds attributes only
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
    // §10.4.3.5 string exotic virtual own props
    StringObj(value: s), Named("length") -> Some(string_length_property(s))
    StringObj(value: s), Index(i) ->
      string_index_property(s, i)
      |> option.lazy_or(fn() { dict.get(props, key) |> option.from_result })
    ArrayObj(_), Index(i) | ArgumentsObj(..), Index(i) ->
      case dict.get(props, key) {
        Ok(prop) -> Some(prop)
        Error(Nil) ->
          elements.get_option(elements, i)
          |> option.map(fn(v) {
            DataProperty(
              value: v,
              writable: True,
              enumerable: True,
              configurable: True,
              seq: 0,
            )
          })
      }
    KBytecode(template:, birth: BirthPending(_), ..), Named("length") ->
      Some(birth_prop(rt_types.mk_int(template.length), 0))
    KBytecode(template:, birth: BirthPending(_), ..), Named("name") ->
      Some(birth_prop(rt_types.mk_string(option.unwrap(template.name, "")), 1))
    KCompiled(length:, birth: BirthPending(_), ..), Named("length") ->
      Some(birth_prop(rt_types.mk_int(length), 0))
    KCompiled(name:, birth: BirthPending(_), ..), Named("name") ->
      Some(birth_prop(rt_types.mk_string(name), 1))
    _, _ -> dict.get(props, key) |> option.from_result
  }
}

pub type OwnIndex {
  OwnIndexValue(JsVal)
  OwnIndexProperty(Property)
  OwnIndexAbsent(proto: Option(Handle))
  OwnIndexExotic
}

pub fn t_get_own_index(st: Agent, h: Handle, idx: Int) -> OwnIndex {
  case read_object(st, h) {
    SObject(kind: ProxyObj(..), ..)
    | SObject(kind: ModuleNamespace(..), ..)
    | SObject(kind: TypedArrayObj(..), ..) -> OwnIndexExotic
    SObject(kind: ArrayObj(_), props:, elements:, proto:, ..)
    | SObject(kind: ArgumentsObj(..), props:, elements:, proto:, ..) ->
      case dict.get(props, Index(idx)) {
        Ok(prop) -> OwnIndexProperty(prop)
        Error(Nil) ->
          case elements.get_option(elements, idx) {
            Some(v) -> OwnIndexValue(v)
            None -> OwnIndexAbsent(proto)
          }
      }
    slot ->
      case own_and_proto_of_slot(st, slot, StringKey(Index(idx))) {
        #(Some(prop), _) -> OwnIndexProperty(prop)
        #(None, proto) -> OwnIndexAbsent(proto)
      }
  }
}

fn own_symbol_property_of(
  symbol_props: List(#(SymbolId, Property)),
  sym: SymbolId,
) -> Option(Property) {
  list.key_find(symbol_props, sym) |> option.from_result
}

// §10.4.3.4 step 10
fn string_length_property(s: String) -> Property {
  DataProperty(
    value: rt_types.mk_number(rt_types.JInt(js_string.length(s))),
    writable: False,
    enumerable: False,
    configurable: False,
    seq: 0,
  )
}

// §10.4.3.5 steps 5-10
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

// §7.2.10 samevalue; float == is =:= so 0.0 != -0.0
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
    rt_types.JInt(x), rt_types.JFloat(y) -> int.to_float(x) == y
    rt_types.JFloat(x), rt_types.JInt(y) -> x == int.to_float(y)
    _, _ -> a == b
  }
}

// §6.2.6.1
fn desc_is_accessor(d: ParsedDesc) -> Bool {
  option.is_some(d.get) || option.is_some(d.set)
}

// §6.2.6.2
fn desc_is_data(d: ParsedDesc) -> Bool {
  option.is_some(d.value) || option.is_some(d.writable)
}

fn key_text(key: ObjectKey) -> String {
  case key {
    StringKey(pk) -> rt_types.key_to_text(pk)
    SymbolKey(sym) -> rt_types.symbol_descriptive_string(sym)
  }
}

fn key_quoted(key: ObjectKey) -> String {
  case key {
    StringKey(pk) -> "'" <> rt_types.key_to_text(pk) <> "'"
    SymbolKey(_) -> "[symbol]"
  }
}

pub fn object_key_value(key: ObjectKey) -> JsVal {
  case key {
    StringKey(pk) -> rt_types.mk_string(rt_types.key_to_text(pk))
    SymbolKey(sym) -> rt_types.mk_symbol(sym)
  }
}

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

fn alloc_plain(st: Agent, entries: List(#(String, JsVal))) -> #(Handle, Agent) {
  let object_proto = st.realm.object.prototype
  use seq <- rt_store.t_cell_new_with(st, list.length(entries))
  let props =
    list.index_map(entries, fn(entry, i) {
      #(Named(entry.0), DataProperty(entry.1, True, True, True, seq + i))
    })
  SObject(
    kind: Ordinary,
    proto: Some(object_proto),
    props: dict.from_list(props),
    symbol_props: [],
    elements: NoElements,
    extensible: True,
  )
}

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

// §10.1.13 for `new`, born on the empty shape
pub fn t_new_receiver(st: Agent, proto: Handle) -> #(Handle, Agent) {
  rt_store.t_cell_new(
    st,
    SShapedObject(
      shape_id: 0,
      proto: Some(proto),
      slots: rt_types.shape_slots_new(),
      offsets: dict.new(),
    ),
  )
}

pub fn t_new_object_literal(st: Agent) -> #(JsVal, Agent) {
  let #(h, st) = t_new_object(st, Some(st.realm.object.prototype))
  #(rt_types.mk_object(h), st)
}

// fn birth props: length seq 0, name seq 1, prototype seq 2
pub const prototype_seq = 2

fn birth_prop(value: JsVal, seq: Int) -> Property {
  DataProperty(value, False, False, True, seq)
}

pub fn constructor_props(f: Handle) -> Dict(PropertyKey, Property) {
  dict.from_list([
    #(
      Named("constructor"),
      DataProperty(rt_types.mk_object(f), True, False, True, 0),
    ),
  ])
}

fn pending_birth(slot: JsSlot) -> Option(#(Int, String, Option(Handle))) {
  case slot {
    SObject(kind: KBytecode(template:, birth: BirthPending(parent), ..), ..) ->
      Some(#(template.length, option.unwrap(template.name, ""), parent))
    SObject(
      kind: KCompiled(length:, name:, birth: BirthPending(parent), ..),
      ..,
    ) -> Some(#(length, name, parent))
    _ -> None
  }
}

fn is_birth_key(key: ObjectKey) -> Bool {
  case key {
    StringKey(Named("length"))
    | StringKey(Named("name"))
    | StringKey(Named("prototype")) -> True
    _ -> False
  }
}

// moves pending length/name/prototype into props
fn settle_birth(
  st: Agent,
  f: Handle,
  slot: JsSlot,
  pending: #(Int, String, Option(Handle)),
) -> Agent {
  let #(length, name, parent) = pending
  let assert SObject(kind:, props:, ..) = slot
  let props =
    props
    |> dict.insert(Named("length"), birth_prop(rt_types.mk_int(length), 0))
    |> dict.insert(Named("name"), birth_prop(rt_types.mk_string(name), 1))
  let #(props, st) = case parent {
    None -> #(props, st)
    Some(parent) -> {
      let #(proto, st) =
        rt_store.t_cell_new(
          st,
          SObject(
            kind: Ordinary,
            proto: Some(parent),
            props: constructor_props(f),
            symbol_props: [],
            elements: NoElements,
            extensible: True,
          ),
        )
      let prototype =
        DataProperty(
          rt_types.mk_object(proto),
          True,
          False,
          False,
          prototype_seq,
        )
      #(dict.insert(props, Named("prototype"), prototype), st)
    }
  }
  let kind = case kind {
    KBytecode(..) -> KBytecode(..kind, birth: BirthSettled)
    KCompiled(..) -> KCompiled(..kind, birth: BirthSettled)
    _ -> kind
  }
  rt_store.t_cell_set(st, f, SObject(..slot, kind:, props:))
}

fn settle(st: Agent, h: Handle, slot: JsSlot) -> #(JsSlot, Agent) {
  case pending_birth(slot) {
    Some(pending) -> {
      let st = settle_birth(st, h, slot, pending)
      #(read_object(st, h), st)
    }
    None -> #(slot, st)
  }
}

fn read_settled(st: Agent, h: Handle, key: ObjectKey) -> #(JsSlot, Agent) {
  let slot = read_object(st, h)
  case is_birth_key(key) {
    True -> settle(st, h, slot)
    False -> #(slot, st)
  }
}

pub fn t_name_if_anonymous(st: Agent, f: Handle, name: String) -> Agent {
  use slot <- rt_store.t_cell_update(st, f)
  case slot {
    SObject(kind: KBytecode(template:, birth: BirthPending(_), ..) as kind, ..) ->
      case option.unwrap(template.name, "") {
        "" ->
          SObject(
            ..slot,
            kind: KBytecode(
              ..kind,
              template: FuncTemplate(..template, name: Some(name)),
            ),
          )
        _ -> slot
      }
    SObject(kind: KCompiled(name: "", birth: BirthPending(_), ..) as kind, ..) ->
      SObject(..slot, kind: KCompiled(..kind, name:))
    SObject(kind: KBytecode(..), props:, ..)
    | SObject(kind: KCompiled(..), props:, ..) ->
      case dict.get(props, Named("name")) {
        Ok(DataProperty(value: v, seq:, ..)) ->
          case rt_types.classify(v) {
            rt_types.KStr("") ->
              SObject(
                ..slot,
                props: dict.insert(
                  props,
                  Named("name"),
                  birth_prop(rt_types.mk_string(name), seq),
                ),
              )
            _ -> slot
          }
        _ -> slot
      }
    _ -> slot
  }
}

// §10.1.1 / §10.5.1
pub fn t_get_prototype_of(st: Agent, obj: Handle) -> #(Option(Handle), Agent) {
  case read_object(st, obj) {
    SObject(kind: ProxyObj(target:, handler:, revoked:), ..) ->
      proxy_get_prototype_of(st, Proxy(target:, handler:, revoked:))
    SObject(proto:, ..) | SShapedObject(proto:, ..) -> #(proto, st)
    _ -> #(None, st)
  }
}

pub type SetProtoFail {
  NotExtensible
  Cyclic
  Immutable
  TrapRefused
}

pub fn set_proto_fail_message(fail: SetProtoFail) -> String {
  case fail {
    NotExtensible -> "Cannot set prototype of a non-extensible object"
    Cyclic -> "Cyclic __proto__ value"
    Immutable -> "Immutable prototype object cannot have its prototype set"
    TrapRefused -> "'setPrototypeOf' on proxy: trap returned falsish"
  }
}

// §10.1.2.1 / §10.5.2, the single dispatch for every caller
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
  use <- bool.guard(new_proto == current, #(Ok(Nil), st))
  // §10.4.7.2 object.prototype has an immutable prototype
  use <- bool.guard(obj == st.realm.object.prototype, #(Error(Immutable), st))
  use <- bool.guard(!extensible, #(Error(NotExtensible), st))
  use <- bool.guard(would_create_cycle(st, obj, new_proto), #(Error(Cyclic), st))
  let st =
    rt_store.t_cell_update(st, obj, fn(slot) {
      let assert SObject(..) = slot
      SObject(..slot, proto: new_proto)
    })
  #(Ok(Nil), st)
}

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

pub fn t_get_proto(st: Agent, obj: Handle) -> #(Option(Handle), Agent) {
  t_get_prototype_of(st, obj)
}

// annex b §b.3.1 __proto__ in object literal
pub fn t_set_proto(st: Agent, obj: Handle, v: JsVal) -> #(Bool, Agent) {
  case rt_types.classify(v) {
    KHandle(p) -> t_set_prototype(st, obj, Some(p))
    KNull -> t_set_prototype(st, obj, None)
    _ -> #(False, st)
  }
}

// §10.1.2.1 step 7, proxies end the walk
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

// §10.1.8.1 ordinaryget; primitives read without boxing
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
    rt_types.KStr(s) -> primitive_string_get(st, s, key, recv)
    rt_types.KNum(_) -> get_from(st, st.realm.number.prototype, key, recv)
    rt_types.KBool(_) -> get_from(st, st.realm.boolean.prototype, key, recv)
    rt_types.KSym(_) -> get_from(st, st.realm.symbol.prototype, key, recv)
    rt_types.KBig(_) -> get_from(st, st.realm.bigint.prototype, key, recv)
    KTdz -> panic as "t_get_prop: TDZ sentinel escaped into a JsVal"
  }
}

fn primitive_string_get(
  st: Agent,
  s: String,
  key: ObjectKey,
  recv: JsVal,
) -> #(JsVal, Agent) {
  let own = case key {
    StringKey(Named("length")) -> Some(string_length_property(s))
    StringKey(Index(i)) -> string_index_property(s, i)
    _ -> None
  }
  case own {
    Some(prop) -> t_property_get_value(st, prop, recv)
    None -> get_from(st, st.realm.string.prototype, key, recv)
  }
}

// §10.1.8.1 ordinaryget(o, p, receiver)
fn get_from(
  st: Agent,
  h: Handle,
  key: ObjectKey,
  receiver: JsVal,
) -> #(JsVal, Agent) {
  case read_object(st, h), key {
    SObject(kind: ProxyObj(..), ..) as slot, StringKey(Private(_)) ->
      ordinary_get(st, slot, key, receiver)
    SObject(kind: ProxyObj(target:, handler:, revoked:), ..), _ ->
      proxy_get(st, Proxy(target:, handler:, revoked:), key, receiver)
    // §10.4.6.8, tdz export throws referenceerror
    SObject(kind: ModuleNamespace(exports:), ..), StringKey(pk) ->
      namespace_get(st, exports, pk)
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
    SObject(kind: KBytecode(birth: BirthPending(Some(_)), ..), ..) as slot,
      StringKey(Named("prototype"))
    | SObject(kind: KCompiled(birth: BirthPending(Some(_)), ..), ..) as slot,
      StringKey(Named("prototype"))
    -> {
      let #(_, st) = settle(st, h, slot)
      get_from(st, h, key, receiver)
    }
    slot, _ -> ordinary_get(st, slot, key, receiver)
  }
}

fn ordinary_get(
  st: Agent,
  slot: JsSlot,
  key: ObjectKey,
  receiver: JsVal,
) -> #(JsVal, Agent) {
  let #(own, proto) = own_and_proto_of_slot(st, slot, key)
  case own {
    Some(prop) -> t_property_get_value(st, prop, receiver)
    None ->
      case proto {
        Some(parent) -> get_from(st, parent, key, receiver)
        None -> #(rt_types.mk_undefined(), st)
      }
  }
}

// §10.1.8.1 steps 3-7
pub fn t_property_get_value(
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

// §10.1.9.1 ordinaryset, false means rejected
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
    // receiver stays the primitive so no setter means false
    rt_types.KStr(s) ->
      case key {
        StringKey(Named("length")) -> #(False, st)
        StringKey(Index(i)) ->
          case js_string.char_at(s, i) {
            Some(_) -> #(False, st)
            None -> set_from(st, st.realm.string.prototype, key, v, recv)
          }
        _ -> set_from(st, st.realm.string.prototype, key, v, recv)
      }
    rt_types.KNum(_) -> set_from(st, st.realm.number.prototype, key, v, recv)
    rt_types.KBool(_) -> set_from(st, st.realm.boolean.prototype, key, v, recv)
    rt_types.KSym(_) -> set_from(st, st.realm.symbol.prototype, key, v, recv)
    rt_types.KBig(_) -> set_from(st, st.realm.bigint.prototype, key, v, recv)
    KTdz -> panic as "t_set_prop: TDZ sentinel escaped into a JsVal"
  }
}

// §10.1.9.1 + §10.1.9.2
fn set_from(
  st: Agent,
  h: Handle,
  key: ObjectKey,
  v: JsVal,
  receiver: JsVal,
) -> #(Bool, Agent) {
  case read_object(st, h), key {
    SObject(kind: ProxyObj(..), ..) as slot, StringKey(Private(_)) ->
      ordinary_set(st, slot, key, v, receiver)
    SObject(kind: ProxyObj(target:, handler:, revoked:), ..), _ ->
      proxy_set(st, Proxy(target:, handler:, revoked:), key, v, receiver)
    SObject(kind: ModuleNamespace(..), ..), _ -> #(False, st)
    // §10.4.5.5; foreign receiver with invalid index converts nothing
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
    SObject(kind: KBytecode(birth: BirthPending(Some(_)), ..), ..) as slot,
      StringKey(Named("prototype"))
    | SObject(kind: KCompiled(birth: BirthPending(Some(_)), ..), ..) as slot,
      StringKey(Named("prototype"))
    -> {
      let #(_, st) = settle(st, h, slot)
      set_from(st, h, key, v, receiver)
    }
    slot, _ -> ordinary_set(st, slot, key, v, receiver)
  }
}

fn same_receiver(receiver: JsVal, h: Handle) -> Bool {
  case rt_types.classify(receiver) {
    KHandle(r) -> r == h
    _ -> False
  }
}

// §10.1.9.2 on an already-read cell
fn ordinary_set(
  st: Agent,
  slot: JsSlot,
  key: ObjectKey,
  v: JsVal,
  receiver: JsVal,
) -> #(Bool, Agent) {
  let #(own, proto) = own_and_proto_of_slot(st, slot, key)
  case own {
    None ->
      case proto {
        Some(parent) -> set_from(st, parent, key, v, receiver)
        None -> set_on_receiver(st, receiver, key, v)
      }
    Some(DataProperty(writable: False, ..)) -> #(False, st)
    Some(DataProperty(writable: True, ..)) ->
      set_on_receiver(st, receiver, key, v)
    Some(AccessorProperty(set: None, ..)) -> #(False, st)
    Some(AccessorProperty(set: Some(setter), ..)) -> {
      let #(_, st) = js_ops(st).call(st, setter, receiver, [v])
      #(True, st)
    }
  }
}

// §10.1.9.2 steps 2.b-h
fn set_on_receiver(
  st: Agent,
  receiver: JsVal,
  key: ObjectKey,
  v: JsVal,
) -> #(Bool, Agent) {
  case rt_types.classify(receiver) {
    KHandle(recv_h) -> {
      case read_object(st, recv_h), key {
        SShapedObject(shape_id:, proto:, slots:, offsets:),
          StringKey(Named(name))
        -> set_own_shaped(st, recv_h, shape_id, proto, slots, offsets, name, v)
        SObject(kind: ProxyObj(..), ..), StringKey(Named(_))
        | SObject(kind: ProxyObj(..), ..), StringKey(Index(_))
        | SObject(kind: ProxyObj(..), ..), SymbolKey(_)
        -> set_on_proxy_receiver(st, recv_h, key, v)
        SObject(kind: ModuleNamespace(exports:), ..), StringKey(pk) -> {
          let _existing = namespace_own_property(st, exports, pk)
          #(False, st)
        }
        _, _ -> {
          let st = devolve(st, recv_h)
          let #(slot, st) = read_settled(st, recv_h, key)
          let assert SObject(
            kind:,
            props:,
            symbol_props:,
            elements:,
            extensible:,
            ..,
          ) = slot
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

// new key moves to the successor shape, minted on first use
fn set_own_shaped(
  st: Agent,
  h: Handle,
  shape_id: Int,
  proto: Option(Handle),
  slots: rt_types.ShapeSlots,
  offsets: Dict(BitArray, Int),
  name: String,
  v: JsVal,
) -> #(Bool, Agent) {
  let js = require_js(st)
  let key_bin = bit_array.from_string(name)
  case dict.get(offsets, key_bin) {
    Ok(off) -> {
      let slots = rt_types.shape_slots_set(slots, off, v)
      #(
        True,
        rt_store.t_cell_set(
          st,
          h,
          SShapedObject(shape_id:, proto:, slots:, offsets:),
        ),
      )
    }
    Error(Nil) ->
      case dict.get(js.shapes, shape_id) {
        Error(Nil) -> #(False, st)
        Ok(ShapeDesc(arity:, transitions:, ..) as from) -> {
          let known =
            dict.get(transitions, key_bin)
            |> result.try(fn(to) {
              dict.get(js.shapes, to)
              |> result.map(fn(desc) { #(to, desc.offsets, st) })
            })
          let #(to, offsets, st) = case known {
            Ok(hit) -> hit
            Error(Nil) -> {
              let to = js.next_shape
              let offsets = dict.insert(offsets, key_bin, arity)
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
                  ShapeDesc(arity: arity + 1, offsets:, transitions: dict.new()),
                )
              #(
                to,
                offsets,
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
              SShapedObject(shape_id: to, proto:, slots:, offsets:),
            ),
          )
        }
      }
  }
}

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
    ArrayObj(_), Named("length") ->
      case array_length_writable(props) {
        False -> #(False, st)
        True -> array_put_length(st, h, v)
      }
    ArrayObj(length:), Index(i) -> {
      let length_writable = array_length_writable(props)
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
    StringObj(_), Named("length") -> #(False, st)
    StringObj(value: s), Index(i) ->
      case js_string.char_at(s, i) {
        Some(_) -> #(False, st)
        None -> set_ordinary_string(st, h, props, extensible, key, v)
      }
    _, _ -> set_ordinary_string(st, h, props, extensible, key, v)
  }
}

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

fn array_length_writable(props: Dict(PropertyKey, Property)) -> Bool {
  case dict.get(props, Named("length")) {
    Ok(DataProperty(writable:, ..)) -> writable
    Ok(AccessorProperty(..)) | Error(Nil) -> True
  }
}

// §10.4.2.4 steps 3-5, two observable coercions
fn to_array_length(st: Agent, v: JsVal) -> #(Int, Agent) {
  let #(new_len, st) = rt_val.t_to_uint32(st, v)
  let #(number_len, st) = rt_val.t_to_number(st, v)
  let same = case number_len {
    rt_types.JInt(n) -> n == new_len
    // +. 0.0 folds -0.0 to 0.0
    rt_types.JFloat(f) -> f +. 0.0 == int.to_float(new_len)
    rt_types.JNan | rt_types.JPosInf | rt_types.JNegInf -> False
  }
  case same {
    True -> #(new_len, st)
    False -> throw_range_error(st, "Invalid array length")
  }
}

// coercion runs first and may freeze length, so re-read after
fn array_put_length(st: Agent, h: Handle, v: JsVal) -> #(Bool, Agent) {
  let #(new_len, st) = to_array_length(st, v)
  let assert SObject(kind: ArrayObj(length: old_len), props:, ..) =
    read_object(st, h)
  case array_length_writable(props) {
    True -> array_set_length(st, h, new_len, old_len)
    False -> #(new_len == old_len, st)
  }
}

// §10.4.2.4 steps 11-19, new_len already validated
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
      // a non-configurable index stops the shrink at index + 1
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

// §10.1.6 trap-aware, returns the raw boolean
pub fn t_define_own_prop(
  st: Agent,
  obj: Handle,
  key: ObjectKey,
  desc: ParsedDesc,
) -> #(Bool, Agent) {
  let st = devolve(st, obj)
  // §10.4.2.4 steps 3-6 coerce before oldlendesc is read
  let #(desc, new_len, st) = case read_object(st, obj), key, desc.value {
    SObject(kind: ArrayObj(_), ..), StringKey(Named("length")), Some(v) -> {
      let #(n, st) = to_array_length(st, v)
      let value = Some(rt_types.mk_number(rt_types.JInt(n)))
      #(ParsedDesc(..desc, value:), Some(n), st)
    }
    _, _, _ -> #(desc, None, st)
  }
  let #(slot, st) = read_settled(st, obj, key)
  let assert SObject(kind:, props:, symbol_props:, elements:, extensible:, ..) =
    slot
  use <- exotic_define(st, kind, key, desc)
  let indexed_kind = case kind {
    ArrayObj(_) | ArgumentsObj(..) -> True
    _ -> False
  }
  let index_blocked = case kind, key {
    ArrayObj(length:), StringKey(Index(i)) ->
      i >= length && !array_length_writable(props)
    _, _ -> False
  }
  use <- bool.guard(index_blocked, #(False, st))
  let existing = case key {
    StringKey(pk) -> own_property_of(st, kind, props, elements, pk)
    SymbolKey(sym) -> own_symbol_property_of(symbol_props, sym)
  }
  let ok = case existing {
    None -> extensible
    Some(cur) -> is_compatible_descriptor(desc, cur)
  }
  use <- bool.guard(!ok, #(False, st))
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
      // exactly one store owns an index: elements or dict
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

pub fn t_define_prop(
  st: Agent,
  obj: Handle,
  key: ObjectKey,
  desc: ParsedDesc,
) -> #(Bool, Agent) {
  t_define_own_prop(st, obj, key, desc)
}

// exotic arms absorb their keys, rest is ordinary
fn exotic_define(
  st: Agent,
  kind: ObjKind,
  key: ObjectKey,
  desc: ParsedDesc,
  ordinary: fn() -> #(Bool, Agent),
) -> #(Bool, Agent) {
  case kind, key {
    ProxyObj(..), StringKey(Private(_)) -> ordinary()
    ProxyObj(target:, handler:, revoked:), _ ->
      proxy_define_own_property(
        st,
        Proxy(target:, handler:, revoked:),
        key,
        desc,
      )
    ModuleNamespace(..), SymbolKey(_) -> ordinary()
    ModuleNamespace(exports:), StringKey(pk) ->
      namespace_define(st, exports, pk, desc)
    StringObj(value: s), StringKey(Named("length")) -> #(
      is_compatible_descriptor(desc, string_length_property(s)),
      st,
    )
    StringObj(value: s), StringKey(Index(i)) ->
      case string_index_property(s, i) {
        Some(cur) -> #(is_compatible_descriptor(desc, cur), st)
        None -> ordinary()
      }
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
        True -> #(False, st)
        False -> ordinary()
      }
    _, _ -> ordinary()
  }
}

// §10.4.5.3 steps 1.b.i-vii
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
      // immutable buffer: true iff samevalue and no attribute upgrade
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

// §10.1.6.2
fn is_compatible_descriptor(desc: ParsedDesc, cur: Property) -> Bool {
  case rt_types.prop_configurable(cur) {
    True -> True
    False -> {
      let bad_configurable = desc.configurable == Some(True)
      let bad_enumerable = case desc.enumerable {
        Some(e) -> e != rt_types.prop_enumerable(cur)
        None -> False
      }
      use <- bool.guard(bad_configurable || bad_enumerable, False)
      let is_acc = desc_is_accessor(desc)
      let is_dat = desc_is_data(desc)
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

fn merge_descriptor(
  desc: ParsedDesc,
  existing: Option(Property),
  enumerable: Bool,
  configurable: Bool,
  seq: Int,
) -> Property {
  case desc_is_accessor(desc), desc_is_data(desc) {
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

// some(undefined) becomes none, none inherits existing
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

// §10.1.7.1; private keys are invisible here
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

fn has_from(st: Agent, h: Handle, key: ObjectKey) -> #(Bool, Agent) {
  case read_object(st, h), key {
    _, StringKey(Private(_)) -> #(False, st)
    SObject(kind: ProxyObj(target:, handler:, revoked:), ..), _ ->
      proxy_has(st, Proxy(target:, handler:, revoked:), key)
    SObject(kind: KBytecode(birth: BirthPending(Some(_)), ..), ..),
      StringKey(Named("prototype"))
    | SObject(kind: KCompiled(birth: BirthPending(Some(_)), ..), ..),
      StringKey(Named("prototype"))
    -> #(True, st)
    SObject(kind: ModuleNamespace(exports:), symbol_props:, ..), _ -> #(
      case key {
        StringKey(pk) -> dict.has_key(exports, rt_types.key_to_text(pk))
        SymbolKey(sym) ->
          option.is_some(own_symbol_property_of(symbol_props, sym))
      },
      st,
    )
    slot, _ -> {
      let #(own, proto) = own_and_proto_of_slot(st, slot, key)
      case own {
        Some(_) -> #(True, st)
        // §10.4.5.2 invalid index is false without the proto chain
        None ->
          case typed_array_absorbs(slot, key) {
            True -> #(False, st)
            False ->
              case proto {
                Some(parent) -> has_from(st, parent, key)
                None -> #(False, st)
              }
          }
      }
    }
  }
}

// §10.1.10.1, false when non-configurable
pub fn t_delete_prop(st: Agent, obj: Handle, key: ObjectKey) -> #(Bool, Agent) {
  let st = devolve(st, obj)
  let #(slot, st) = read_settled(st, obj, key)
  let assert SObject(kind:, props:, symbol_props:, elements:, ..) = slot
  case key {
    SymbolKey(sym) ->
      case kind {
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
        ProxyObj(..), Private(_) -> ordinary_delete()
        ProxyObj(target:, handler:, revoked:), _ ->
          proxy_delete(st, Proxy(target:, handler:, revoked:), key)
        ModuleNamespace(exports:), _ -> #(
          !dict.has_key(exports, rt_types.key_to_text(pk)),
          st,
        )
        ArrayObj(_), Named("length") -> #(False, st)
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

// §10.1.11 / §10.5.11; settles pending fn birth props first
pub fn t_own_keys(st: Agent, obj: Handle) -> #(List(ObjectKey), Agent) {
  case read_object(st, obj) {
    SShapedObject(offsets:, ..) -> #(shaped_own_keys(offsets), st)
    slot -> {
      let #(slot, st) = settle(st, obj, slot)
      sobject_own_keys(st, slot)
    }
  }
}

fn shaped_own_keys(offsets: Dict(BitArray, Int)) -> List(ObjectKey) {
  dict.to_list(offsets)
  |> list.sort(fn(a, b) { int.compare(a.1, b.1) })
  |> list.filter_map(fn(pair) {
    bit_array.to_string(pair.0)
    |> result.map(fn(name) { StringKey(Named(name)) })
  })
}

fn sobject_own_keys(st: Agent, slot: JsSlot) -> #(List(ObjectKey), Agent) {
  let assert SObject(kind:, props:, symbol_props:, elements:, ..) = slot
  use <- proxy_or(kind, proxy_own_keys(st, _))
  let has_virtual_length = case kind {
    ArrayObj(_) | StringObj(_) -> True
    _ -> False
  }
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
  let named = case kind {
    ModuleNamespace(exports:) ->
      list.sort(dict.keys(exports), string.compare)
      |> list.index_map(fn(name, i) { #(i, Named(name)) })
    _ -> named
  }
  let index_keys =
    list.append(elem_idx, dict_idx)
    |> list.sort(int.compare)
    |> list.map(fn(i) { StringKey(Index(i)) })
  let length_key = case has_virtual_length {
    True -> [StringKey(Named("length"))]
    False -> []
  }
  let named_keys =
    list.sort(named, fn(a, b) { int.compare(a.0, b.0) })
    |> list.map(fn(pair) { StringKey(pair.1) })
  let symbol_keys = list.map(symbol_props, fn(pair) { SymbolKey(pair.0) })
  #(list.flatten([index_keys, length_key, named_keys, symbol_keys]), st)
}

// §7.3.23, per-key descriptor read is observable
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

// §14.7.5.9 enumerateobjectproperties
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

// non-enumerable own key still shadows proto keys; fuel bounds trap loops
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

pub fn t_get_prop_with_receiver(
  st: Agent,
  h: Handle,
  key: ObjectKey,
  receiver: JsVal,
) -> #(JsVal, Agent) {
  get_from(st, h, key, receiver)
}

pub fn t_set_prop_with_receiver(
  st: Agent,
  h: Handle,
  key: ObjectKey,
  v: JsVal,
  receiver: JsVal,
) -> #(Bool, Agent) {
  set_from(st, h, key, v, receiver)
}

// §10.5.5 / §10.4.6.5 / ordinary, the trap-aware entry
pub fn t_get_own_property(
  st: Agent,
  h: Handle,
  key: ObjectKey,
) -> #(Option(Property), Agent) {
  case read_object(st, h), key {
    slot, StringKey(Private(_)) -> #(own_and_proto_of_slot(st, slot, key).0, st)
    SObject(kind: ProxyObj(target:, handler:, revoked:), ..), _ ->
      proxy_get_own_property(st, Proxy(target:, handler:, revoked:), key)
    SObject(kind: ModuleNamespace(exports:), ..), StringKey(pk) -> #(
      namespace_own_property(st, exports, pk),
      st,
    )
    SObject(kind: KBytecode(birth: BirthPending(Some(_)), ..), ..) as slot,
      StringKey(Named("prototype"))
    | SObject(kind: KCompiled(birth: BirthPending(Some(_)), ..), ..) as slot,
      StringKey(Named("prototype"))
    -> {
      let #(slot, st) = settle(st, h, slot)
      #(own_and_proto_of_slot(st, slot, key).0, st)
    }
    slot, _ -> #(own_and_proto_of_slot(st, slot, key).0, st)
  }
}

// §10.1.5.1 no proto walk, no traps, no birth settle
pub fn t_ordinary_own_property(
  st: Agent,
  h: Handle,
  key: ObjectKey,
) -> Option(Property) {
  let #(own, _proto) = read_own_and_proto(st, h, key)
  own
}

pub fn t_own_property(
  st: Agent,
  h: Handle,
  key: ObjectKey,
) -> #(Option(Property), Agent) {
  let #(slot, st) = read_settled(st, h, key)
  #(own_and_proto_of_slot(st, slot, key).0, st)
}

// §7.2.5 / §10.5.3
pub fn t_is_extensible(st: Agent, h: Handle) -> #(Bool, Agent) {
  case read_object(st, h) {
    SObject(kind: ProxyObj(target:, handler:, revoked:), ..) ->
      proxy_is_extensible(st, Proxy(target:, handler:, revoked:))
    slot -> #(slot_extensible(slot), st)
  }
}

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

// §7.2.2, throws on revoked proxy
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

// §10.5.4 / §10.1.4.1
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

// §10.4.6.12 exports map to live binding cells
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

// §10.4.6.5, tdz throws even for key-only operations
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

// §10.4.6.6 steps 2-9, true iff nothing changes
fn namespace_define(
  st: Agent,
  exports: Dict(String, Handle),
  key: PropertyKey,
  desc: ParsedDesc,
) -> #(Bool, Agent) {
  let name = rt_types.key_to_text(key)
  case dict.get(exports, name) {
    Error(Nil) -> #(False, st)
    Ok(cell) -> {
      let incompatible =
        desc.configurable == Some(True)
        || desc.enumerable == Some(False)
        || desc_is_accessor(desc)
        || desc.writable == Some(False)
      use <- bool.guard(incompatible, #(False, st))
      case desc.value {
        None -> #(True, st)
        // tdz here must throw, not return false
        Some(requested) -> #(
          same_value(requested, namespace_binding_value(st, name, cell)),
          st,
        )
      }
    }
  }
}

// a missing trap forwards via t_* so nested proxies trap
type Proxy {
  Proxy(target: Handle, handler: Handle, revoked: Bool)
}

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

// §10.5.14 + §7.3.10 getmethod, none means forward to target
fn proxy_trap(st: Agent, p: Proxy, name: String) -> #(Option(JsVal), Agent) {
  use <- bool.lazy_guard(p.revoked, fn() {
    throw_type_error(
      st,
      "Cannot perform '" <> name <> "' on a proxy that has been revoked",
    )
  })
  let #(trap, st) =
    t_get_prop(st, rt_types.mk_object(p.handler), StringKey(Named(name)))
  case rt_types.classify(trap) {
    KUndef | KNull -> #(None, st)
    _ -> {
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

fn call_trap(
  st: Agent,
  p: Proxy,
  trap: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  js_ops(st).call(st, trap, rt_types.mk_object(p.handler), args)
}

// §10.5.1
fn proxy_get_prototype_of(st: Agent, p: Proxy) -> #(Option(Handle), Agent) {
  let #(trap, st) = proxy_trap(st, p, "getPrototypeOf")
  case trap {
    None -> t_get_prototype_of(st, p.target)
    Some(trap_fn) -> {
      let #(res, st) = call_trap(st, p, trap_fn, [rt_types.mk_object(p.target)])
      let proto = case rt_types.classify(res) {
        KHandle(h) -> Some(h)
        KNull -> None
        _ ->
          throw_type_error(
            st,
            "'getPrototypeOf' on proxy: trap returned neither object nor null",
          )
      }
      let #(ext, st) = t_is_extensible(st, p.target)
      use <- bool.guard(ext, #(proto, st))
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

// §10.5.2
fn proxy_set_prototype_of(
  st: Agent,
  p: Proxy,
  new_proto: Option(Handle),
) -> #(Bool, Agent) {
  let #(trap, st) = proxy_trap(st, p, "setPrototypeOf")
  case trap {
    None -> t_set_prototype(st, p.target, new_proto)
    Some(trap_fn) -> {
      let proto_val = case new_proto {
        Some(h) -> rt_types.mk_object(h)
        None -> rt_types.mk_null()
      }
      let #(res, st) =
        call_trap(st, p, trap_fn, [rt_types.mk_object(p.target), proto_val])
      use <- bool.guard(!rt_val.to_boolean(res), #(False, st))
      let #(ext, st) = t_is_extensible(st, p.target)
      use <- bool.guard(ext, #(True, st))
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

// §10.5.3
fn proxy_is_extensible(st: Agent, p: Proxy) -> #(Bool, Agent) {
  let #(trap, st) = proxy_trap(st, p, "isExtensible")
  case trap {
    None -> t_is_extensible(st, p.target)
    Some(trap_fn) -> {
      let #(res, st) = call_trap(st, p, trap_fn, [rt_types.mk_object(p.target)])
      let b = rt_val.to_boolean(res)
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

// §10.5.4
fn proxy_prevent_extensions(st: Agent, p: Proxy) -> #(Bool, Agent) {
  let #(trap, st) = proxy_trap(st, p, "preventExtensions")
  case trap {
    None -> t_prevent_extensions(st, p.target)
    Some(trap_fn) -> {
      let #(res, st) = call_trap(st, p, trap_fn, [rt_types.mk_object(p.target)])
      use <- bool.guard(!rt_val.to_boolean(res), #(False, st))
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

// §10.5.5
fn proxy_get_own_property(
  st: Agent,
  p: Proxy,
  key: ObjectKey,
) -> #(Option(Property), Agent) {
  let #(trap, st) = proxy_trap(st, p, "getOwnPropertyDescriptor")
  case trap {
    None -> t_get_own_property(st, p.target, key)
    Some(trap_fn) -> {
      let #(res, st) =
        call_trap(st, p, trap_fn, [
          rt_types.mk_object(p.target),
          object_key_value(key),
        ])
      case rt_types.classify(res) {
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
        KHandle(_) -> {
          let #(target_desc, st) = t_get_own_property(st, p.target, key)
          let #(ext, st) = t_is_extensible(st, p.target)
          let #(parsed, st) = t_to_property_descriptor(st, res)
          let completed = complete_descriptor(parsed)
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
                  // completed desc: absent writable defaults to false and still counts
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

// §10.5.6, raw boolean
fn proxy_define_own_property(
  st: Agent,
  p: Proxy,
  key: ObjectKey,
  desc: ParsedDesc,
) -> #(Bool, Agent) {
  let #(trap, st) = proxy_trap(st, p, "defineProperty")
  case trap {
    // only validation rejection is false, real throws propagate
    None -> t_define_own_prop(st, p.target, key, desc)
    Some(trap_fn) -> {
      let #(desc_obj, st) = t_from_property_descriptor(st, desc)
      let #(res, st) =
        call_trap(st, p, trap_fn, [
          rt_types.mk_object(p.target),
          object_key_value(key),
          rt_types.mk_object(desc_obj),
        ])
      use <- bool.guard(!rt_val.to_boolean(res), #(False, st))
      let #(target_desc, st) = t_get_own_property(st, p.target, key)
      let #(ext, st) = t_is_extensible(st, p.target)
      let setting_config_false = desc.configurable == Some(False)
      case target_desc {
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

// §10.5.7
fn proxy_has(st: Agent, p: Proxy, key: ObjectKey) -> #(Bool, Agent) {
  let #(trap, st) = proxy_trap(st, p, "has")
  case trap {
    None -> has_from(st, p.target, key)
    Some(trap_fn) -> {
      let #(res, st) =
        call_trap(st, p, trap_fn, [
          rt_types.mk_object(p.target),
          object_key_value(key),
        ])
      use <- bool.guard(rt_val.to_boolean(res), #(True, st))
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

// §10.5.8
fn proxy_get(
  st: Agent,
  p: Proxy,
  key: ObjectKey,
  receiver: JsVal,
) -> #(JsVal, Agent) {
  let #(trap, st) = proxy_trap(st, p, "get")
  case trap {
    None -> get_from(st, p.target, key, receiver)
    Some(trap_fn) -> {
      let #(res, st) =
        call_trap(st, p, trap_fn, [
          rt_types.mk_object(p.target),
          object_key_value(key),
          receiver,
        ])
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

// §10.5.9
fn proxy_set(
  st: Agent,
  p: Proxy,
  key: ObjectKey,
  v: JsVal,
  receiver: JsVal,
) -> #(Bool, Agent) {
  let #(trap, st) = proxy_trap(st, p, "set")
  case trap {
    None -> set_from(st, p.target, key, v, receiver)
    Some(trap_fn) -> {
      let #(res, st) =
        call_trap(st, p, trap_fn, [
          rt_types.mk_object(p.target),
          object_key_value(key),
          v,
          receiver,
        ])
      use <- bool.guard(!rt_val.to_boolean(res), #(False, st))
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

// §10.1.9.2 steps 2.c-e with a proxy receiver
fn set_on_proxy_receiver(
  st: Agent,
  recv_h: Handle,
  key: ObjectKey,
  v: JsVal,
) -> #(Bool, Agent) {
  let #(existing, st) = t_get_own_property(st, recv_h, key)
  case existing {
    Some(AccessorProperty(..)) -> #(False, st)
    Some(DataProperty(writable: False, ..)) -> #(False, st)
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
    None -> t_define_own_data(st, recv_h, key, v, True, True, True)
  }
}

// §10.5.10
fn proxy_delete(st: Agent, p: Proxy, key: ObjectKey) -> #(Bool, Agent) {
  let #(trap, st) = proxy_trap(st, p, "deleteProperty")
  case trap {
    None -> t_delete_prop(st, p.target, key)
    Some(trap_fn) -> {
      let #(res, st) =
        call_trap(st, p, trap_fn, [
          rt_types.mk_object(p.target),
          object_key_value(key),
        ])
      use <- bool.guard(!rt_val.to_boolean(res), #(False, st))
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

// §10.5.11
fn proxy_own_keys(st: Agent, p: Proxy) -> #(List(ObjectKey), Agent) {
  let #(trap, st) = proxy_trap(st, p, "ownKeys")
  case trap {
    None -> t_own_keys(st, p.target)
    Some(trap_fn) -> {
      let #(res, st) = call_trap(st, p, trap_fn, [rt_types.mk_object(p.target)])
      let #(keys, st) = keys_from_array_like(st, res)
      use <- bool.lazy_guard(has_duplicate_keys(keys, []), fn() {
        throw_type_error(
          st,
          "'ownKeys' on proxy: trap returned duplicate entries",
        )
      })
      let #(ext, st) = t_is_extensible(st, p.target)
      let #(target_keys, st) = t_own_keys(st, p.target)
      let #(#(nonconf, conf), st) =
        partition_configurable(st, p.target, target_keys, [], [])
      use <- bool.guard(ext && nonconf == [], #(keys, st))
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
      use <- bool.guard(ext, #(keys, st))
      use <- lazy_guard_found(missing(conf), fn(k) {
        throw_type_error(
          st,
          "'ownKeys' on proxy: trap result did not include "
            <> key_quoted(k)
            <> ", a key of the non-extensible proxy target",
        )
      })
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

// §10.5.11 steps 11-16, both lists in keys order
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

// §7.3.20 createlistfromarraylike, property-key
fn keys_from_array_like(st: Agent, v: JsVal) -> #(List(ObjectKey), Agent) {
  case rt_types.classify(v) {
    KHandle(_) -> {
      let #(len_v, st) = t_get_prop(st, v, StringKey(Named("length")))
      let #(len, st) = rt_val.t_to_length(st, len_v)
      use <- bool.lazy_guard(len > limits.max_iteration, fn() {
        throw_range_error(
          st,
          "'ownKeys' on proxy: trap result length exceeds iteration budget",
        )
      })
      gather_keys_via_get(st, v, 0, len, [])
    }
    _ -> throw_type_error(st, "CreateListFromArrayLike called on non-object")
  }
}

fn gather_keys_via_get(
  st: Agent,
  obj: JsVal,
  idx: Int,
  len: Int,
  acc: List(ObjectKey),
) -> #(List(ObjectKey), Agent) {
  use <- bool.guard(idx >= len, #(list.reverse(acc), st))
  let #(item, st) = t_get_prop(st, obj, StringKey(rt_types.index_key(idx)))
  case object_key_of_value(item) {
    Some(k) -> gather_keys_via_get(st, obj, idx + 1, len, [k, ..acc])
    None ->
      throw_type_error(
        st,
        "'ownKeys' on proxy: trap returned a non-String, non-Symbol key",
      )
  }
}

// §6.2.6.5, field read order is observable
pub fn t_to_property_descriptor(st: Agent, obj: JsVal) -> #(ParsedDesc, Agent) {
  case rt_types.classify(obj) {
    KHandle(_) -> Nil
    _ -> throw_type_error(st, "Property description must be an object")
  }
  let #(enumerable, st) = read_desc_bool(st, obj, "enumerable")
  let #(configurable, st) = read_desc_bool(st, obj, "configurable")
  let #(value, st) = read_desc_field(st, obj, "value")
  let #(writable, st) = read_desc_bool(st, obj, "writable")
  let #(get, st) = read_desc_field(st, obj, "get")
  let st = require_callable_accessor(st, get, "Getter")
  let #(set, st) = read_desc_field(st, obj, "set")
  let st = require_callable_accessor(st, set, "Setter")
  let desc =
    ParsedDesc(get:, set:, value:, writable:, enumerable:, configurable:)
  case desc_is_accessor(desc) && desc_is_data(desc) {
    True ->
      throw_type_error(
        st,
        "Invalid property descriptor. Cannot both specify accessors and a value or writable attribute",
      )
    False -> #(desc, st)
  }
}

// hasproperty then get, interleaved per field
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

fn read_desc_bool(
  st: Agent,
  obj: JsVal,
  name: String,
) -> #(Option(Bool), Agent) {
  let #(field, st) = read_desc_field(st, obj, name)
  #(option.map(field, rt_val.to_boolean), st)
}

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

// §6.2.6.4 on a partial desc, present fields only
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

// §6.2.6.6
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

// §10.1.6.2 validation only
fn compatible_descriptor(
  extensible: Bool,
  desc: ParsedDesc,
  current: Option(Property),
) -> Bool {
  case current {
    None -> extensible
    Some(cur) -> is_compatible_descriptor(desc, cur)
  }
}

// all-true data desc on a shaped receiver stays shaped
pub fn t_define_own_data(
  st: Agent,
  h: Handle,
  key: ObjectKey,
  value: JsVal,
  writable: Bool,
  enumerable: Bool,
  configurable: Bool,
) -> #(Bool, Agent) {
  case key, writable && enumerable && configurable {
    StringKey(Named(name)), True ->
      case rt_store.t_cell_get(st, h) {
        SShapedObject(shape_id:, proto:, slots:, offsets:) ->
          set_own_shaped(st, h, shape_id, proto, slots, offsets, name, value)
        _ -> define_own_data(st, h, key, value, True, True, True)
      }
    _, _ ->
      define_own_data(st, h, key, value, writable, enumerable, configurable)
  }
}

fn define_own_data(
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

// accepts wire PropertyKey or ObjectKey
@external(erlang, "arc_rt_store_ffi", "as_object_key")
fn as_object_key(key: k) -> ObjectKey

@external(erlang, "arc_rt_store_ffi", "identity")
fn unsafe_coerce(a: a) -> b

@external(erlang, "erlang", "is_list")
fn is_list(a: a) -> Bool

pub fn t_get_prop_any(st: Agent, recv: JsVal, key: k) -> #(JsVal, Agent) {
  t_get_prop(st, recv, as_object_key(key))
}

pub fn t_set_prop_any(
  st: Agent,
  recv: JsVal,
  key: k,
  v: JsVal,
) -> #(Bool, Agent) {
  t_set_prop(st, recv, as_object_key(key), v)
}

// §13.15.2 strict putvalue throws on failed set
pub fn t_set_prop_strict(
  st: Agent,
  recv: JsVal,
  key: k,
  v: JsVal,
) -> #(Bool, Agent) {
  let okey = as_object_key(key)
  let #(ok, st) = t_set_prop(st, recv, okey, v)
  case ok {
    True -> #(True, st)
    False ->
      throw_type_error(
        st,
        "Cannot assign to read only property '" <> key_text(okey) <> "'",
      )
  }
}

// §13.5.1.2 strict delete throws on non-configurable
pub fn t_delete_prop_strict(
  st: Agent,
  obj: Handle,
  key: ObjectKey,
) -> #(Bool, Agent) {
  let #(deleted, st) = t_delete_prop(st, obj, key)
  case deleted {
    True -> #(True, st)
    False ->
      throw_type_error(st, "Cannot delete property '" <> key_text(key) <> "'")
  }
}

// §7.3.5 createdatapropertyorthrow, v is a raw value
@external(erlang, "arc_rt_obj_ffi", "t_create_data_prop")
pub fn t_create_data_prop(
  st: Agent,
  recv: JsVal,
  key: k,
  v: JsVal,
) -> #(Bool, Agent)

pub fn t_create_data_prop_slow(
  st: Agent,
  recv: JsVal,
  key: k,
  v: JsVal,
) -> #(Bool, Agent) {
  case rt_types.classify(recv) {
    KHandle(h) -> {
      let okey = as_object_key(key)
      let #(ok, st) = t_define_own_data(st, h, okey, v, True, True, True)
      case ok {
        True -> #(True, st)
        False ->
          throw_type_error(
            st,
            "Cannot define property '" <> key_text(okey) <> "'",
          )
      }
    }
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

// absent name throws referenceerror
pub fn t_global_get(st: Agent, name: BitArray) -> #(JsVal, Agent) {
  let g = rt_types.mk_object(st.realm.global_object)
  let key = StringKey(binary_key(name))
  let #(has, st) = t_has_prop(st, g, key)
  case has {
    True -> t_get_prop(st, g, key)
    False -> {
      let text = bit_array.to_string(name) |> result.unwrap("")
      throw_reference_error(st, text <> " is not defined")
    }
  }
}

pub fn t_global_this(st: Agent) -> JsVal {
  rt_types.mk_object(st.realm.global_object)
}

// sloppy: failed set ignored
pub fn t_global_set(st: Agent, name: BitArray, v: JsVal) -> Agent {
  let g = st.realm.global_object
  let #(_, st) =
    t_set_prop(st, rt_types.mk_object(g), StringKey(binary_key(name)), v)
  st
}

// strict: unresolvable throws referenceerror, failed set typeerror
pub fn t_global_set_strict(st: Agent, name: BitArray, v: JsVal) -> Agent {
  let g = rt_types.mk_object(st.realm.global_object)
  let key = StringKey(binary_key(name))
  let text = bit_array.to_string(name) |> result.unwrap("")
  let #(has, st) = t_has_prop(st, g, key)
  case has {
    False -> throw_reference_error(st, text <> " is not defined")
    True -> {
      let #(ok, st) = t_set_prop(st, g, key, v)
      case ok {
        True -> st
        False ->
          throw_type_error(
            st,
            "Cannot assign to read only property '" <> text <> "'",
          )
      }
    }
  }
}

// unresolvable global yields "undefined" without throwing
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

// mapped is undefined or a cons-list of param cells
pub fn t_new_arguments(
  st: Agent,
  args: List(JsVal),
  mapped: m,
  callee: JsVal,
) -> #(JsVal, Agent) {
  let len = list.length(args)
  // wire-level check, a list is not a JsVal
  let mapped_cells = case is_list(mapped) {
    True -> Some(unsafe_coerce(mapped))
    False -> None
  }
  let elements = tree_array.from_list(args)
  let realm = st.realm
  let symbol_props = case
    t_ordinary_own_property(
      st,
      realm.array.prototype,
      SymbolKey(rt_types.symbol_iterator),
    )
  {
    Some(values_prop) -> [#(rt_types.symbol_iterator, values_prop)]
    None -> []
  }
  let #(h, st) = {
    use seq <- rt_store.t_cell_new_with(st, 2)
    let length_prop =
      DataProperty(
        value: rt_types.mk_number(rt_types.JInt(len)),
        writable: True,
        enumerable: False,
        configurable: True,
        seq:,
      )
    let callee_prop = case mapped_cells {
      Some(_) ->
        DataProperty(
          value: callee,
          writable: True,
          enumerable: False,
          configurable: True,
          seq: seq + 1,
        )
      None -> {
        let thrower = Some(rt_types.mk_object(realm.throw_type_error))
        AccessorProperty(
          get: thrower,
          set: thrower,
          enumerable: False,
          configurable: False,
          seq: seq + 1,
        )
      }
    }
    SObject(
      kind: ArgumentsObj(length: len, mapped: mapped_cells),
      proto: Some(realm.object.prototype),
      props: dict.from_list([
        #(Named("length"), length_prop),
        #(Named("callee"), callee_prop),
      ]),
      symbol_props:,
      elements: Dense(elements),
      extensible: True,
    )
  }
  #(rt_types.mk_object(h), st)
}

// holes arrive as mk_hole() and stay holes
pub fn t_new_array(st: Agent, elems: List(JsVal)) -> #(JsVal, Agent) {
  let len = list.length(elems)
  let elements = tree_array.from_list(elems)
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
