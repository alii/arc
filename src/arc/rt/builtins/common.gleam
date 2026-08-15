//// `rt_builtins/common` — shared allocation substrate for realm bootstrap
//// (SPEC §7.M6 common-and-scaffold(1)).
////
//// Port of `arc/vm/builtins/common.gleam:291-1030` alloc helpers, re-expressed
//// over the threaded `Agent` model. Uses ONLY `rt_store` /
//// `rt_types` / `rt_call.t_native_new` so per-module builtin files can
//// import this without cycles.
////
//// **Return-tuple order is `#(V, St')` — value FIRST (R1).** Property-builder
//// helpers thread `t_next_prop_seq` per-prop (arc used a global counter).

import arc/rt/call as rt_call
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type Handle, type JsVal, type NativeToken,
  type ObjKind, type Property, type PropertyKey, type SymbolId, AccessorProperty,
  ArrayObj, BuiltinPair, DataProperty, Dense, ErrorObj, JInt, KNative, Named,
  NoElements, Ordinary, SObject, mk_number, mk_object, mk_string, mk_undefined,
} as rt_types
import arc/vm/internal/tree_array
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}

// ── property builders (port arc value.gleam:3824-3962; threaded seq) ────────

/// `{value, W:F, E:F, C:F}` with a fresh threaded seq — arc `value.data`.
pub fn data_prop(st: Agent, val: JsVal) -> #(Property, Agent) {
  let #(seq, st) = rt_store.t_next_prop_seq(st)
  #(
    DataProperty(
      value: val,
      writable: False,
      enumerable: False,
      configurable: False,
      seq:,
    ),
    st,
  )
}

/// `{value, W:T, E:T, C:T}` with a fresh threaded seq — arc `value.data_property`.
pub fn data_property(st: Agent, val: JsVal) -> #(Property, Agent) {
  let #(seq, st) = rt_store.t_next_prop_seq(st)
  #(
    DataProperty(
      value: val,
      writable: True,
      enumerable: True,
      configurable: True,
      seq:,
    ),
    st,
  )
}

/// Built-in method/prop: `{W:T, E:F, C:T}` — arc `value.builtin_property`.
pub fn builtin_property(st: Agent, val: JsVal) -> #(Property, Agent) {
  let #(seq, st) = rt_store.t_next_prop_seq(st)
  #(
    DataProperty(
      value: val,
      writable: True,
      enumerable: False,
      configurable: True,
      seq:,
    ),
    st,
  )
}

/// Accessor property builder with fresh threaded seq — arc `value.accessor`.
pub fn accessor_prop(
  st: Agent,
  get get: Option(JsVal),
  set set: Option(JsVal),
  enumerable enumerable: Bool,
  configurable configurable: Bool,
) -> #(Property, Agent) {
  let #(seq, st) = rt_store.t_next_prop_seq(st)
  #(AccessorProperty(get:, set:, enumerable:, configurable:, seq:), st)
}

/// Set configurable to True on an existing prop — arc `value.configurable`.
pub fn configurable(prop: Property) -> Property {
  case prop {
    DataProperty(value:, writable:, enumerable:, seq:, ..) ->
      DataProperty(value:, writable:, enumerable:, configurable: True, seq:)
    AccessorProperty(get:, set:, enumerable:, seq:, ..) ->
      AccessorProperty(get:, set:, enumerable:, configurable: True, seq:)
  }
}

/// Give an already-built descriptor a FRESH creation seq — arc `value.restamp`.
/// Two distinct keys must never share a Property record (equal seqs are an
/// enumeration-order tie).
pub fn restamp(st: Agent, prop: Property) -> #(Property, Agent) {
  let #(seq, st) = rt_store.t_next_prop_seq(st)
  let prop = case prop {
    DataProperty(value:, writable:, enumerable:, configurable:, ..) ->
      DataProperty(value:, writable:, enumerable:, configurable:, seq:)
    AccessorProperty(get:, set:, enumerable:, configurable:, ..) ->
      AccessorProperty(get:, set:, enumerable:, configurable:, seq:)
  }
  #(prop, st)
}

/// §20.2.2 Function `name` property — `{W:F, E:F, C:T}`. Threaded seq (arc
/// used constant seq 1 in a reserved band; 2core threads instead — see
/// rt_call.gleam:637-644 birth-time seq note).
pub fn fn_name_property(st: Agent, name: String) -> #(Property, Agent) {
  let #(seq, st) = rt_store.t_next_prop_seq(st)
  #(
    DataProperty(
      value: mk_string(name),
      writable: False,
      enumerable: False,
      configurable: True,
      seq:,
    ),
    st,
  )
}

/// §20.2.2 Function `length` property — `{W:F, E:F, C:T}`.
pub fn fn_length_property(st: Agent, arity: Int) -> #(Property, Agent) {
  let #(seq, st) = rt_store.t_next_prop_seq(st)
  #(
    DataProperty(
      value: mk_number(JInt(arity)),
      writable: False,
      enumerable: False,
      configurable: True,
      seq:,
    ),
    st,
  )
}

/// A built-in constructor's "prototype" property — `{W:F, E:F, C:F}`.
pub fn fn_prototype_property(st: Agent, proto: Handle) -> #(Property, Agent) {
  let #(seq, st) = rt_store.t_next_prop_seq(st)
  #(
    DataProperty(
      value: mk_object(proto),
      writable: False,
      enumerable: False,
      configurable: False,
      seq:,
    ),
    st,
  )
}

// ── dict / cell allocation helpers (port arc common.gleam:291-495) ──────────

/// Build a PropertyKey-keyed dict from String-keyed entries. Builtin init only
/// ever uses named keys.
pub fn named_props(
  props: List(#(String, Property)),
) -> Dict(PropertyKey, Property) {
  use acc, #(k, v) <- list.fold(props, dict.new())
  dict.insert(acc, Named(k), v)
}

/// Allocate an ordinary prototype object, root it, return `#(ref, st)`.
pub fn alloc_proto(
  st: Agent,
  proto: Option(Handle),
  props: Dict(PropertyKey, Property),
) -> #(Handle, Agent) {
  let #(h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: Ordinary,
        proto:,
        props:,
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  #(h, rt_store.t_pin_root(st, h))
}

/// Allocate an ordinary object with `%Object.prototype%` and named data props
/// `{W:T, E:T, C:T}` — arc `alloc_pojo`. Does NOT root.
pub fn alloc_pojo(
  st: Agent,
  object_proto: Handle,
  props: List(#(String, JsVal)),
) -> #(Handle, Agent) {
  let #(entries, st) =
    list.fold(props, #([], st), fn(acc, kv) {
      let #(entries, st) = acc
      let #(k, v) = kv
      let #(prop, st) = data_property(st, v)
      #([#(k, prop), ..entries], st)
    })
  rt_store.t_cell_new(
    st,
    SObject(
      kind: Ordinary,
      proto: Some(object_proto),
      props: named_props(list.reverse(entries)),
      symbol_props: [],
      elements: NoElements,
      extensible: True,
    ),
  )
}

/// Allocate a native function ROOTED — thin wrapper on
/// `rt_call.t_native_new` + `t_pin_root`. `constructible: False` (methods,
/// getters, standalone functions). Spec name; alias of
/// `alloc_rooted_native_fn`.
pub fn alloc_native_fn(
  st: Agent,
  fn_proto: Handle,
  tag: NativeToken,
  name: String,
  arity: Int,
) -> #(Handle, Agent) {
  alloc_rooted_native_fn(st, fn_proto, tag, name, arity)
}

/// Allocate a native function ROOTED — thin wrapper on
/// `rt_call.t_native_new` + `t_pin_root`. `constructible: False` (methods,
/// getters, standalone functions).
pub fn alloc_rooted_native_fn(
  st: Agent,
  fn_proto: Handle,
  tag: NativeToken,
  name: String,
  arity: Int,
) -> #(Handle, Agent) {
  let #(h, st) =
    rt_call.t_native_new(st, Some(fn_proto), tag, name, arity, False)
  #(h, rt_store.t_pin_root(st, h))
}

/// Batch-allocate native method function objects from `(name, tag, arity)`
/// specs, returning `builtin_property` entries — arc `alloc_methods`.
pub fn alloc_methods(
  st: Agent,
  fn_proto: Handle,
  specs: List(#(String, NativeToken, Int)),
) -> #(List(#(String, Property)), Agent) {
  list.fold(specs, #([], st), fn(acc, spec) {
    let #(props, st) = acc
    let #(name, tag, arity) = spec
    let #(fn_h, st) = alloc_rooted_native_fn(st, fn_proto, tag, name, arity)
    let #(prop, st) = builtin_property(st, mk_object(fn_h))
    #([#(name, prop), ..props], st)
  })
}

/// Batch-allocate native getter functions returning get-only AccessorProperty
/// entries `{E:F, C:T}` — arc `alloc_getters`.
pub fn alloc_getters(
  st: Agent,
  fn_proto: Handle,
  specs: List(#(String, NativeToken)),
) -> #(List(#(String, Property)), Agent) {
  list.fold(specs, #([], st), fn(acc, spec) {
    let #(props, st) = acc
    let #(name, tag) = spec
    let #(fn_h, st) =
      alloc_rooted_native_fn(st, fn_proto, tag, "get " <> name, 0)
    let #(prop, st) =
      accessor_prop(
        st,
        get: Some(mk_object(fn_h)),
        set: None,
        enumerable: False,
        configurable: True,
      )
    #([#(name, prop), ..props], st)
  })
}

/// Allocate a getter+setter native fn pair, return the AccessorProperty
/// `{E:F, C:T}` — arc `alloc_get_set_accessor`.
pub fn alloc_get_set_accessor(
  st: Agent,
  fn_proto: Handle,
  get: NativeToken,
  set: NativeToken,
  name: String,
) -> #(Property, Agent) {
  let #(get_h, st) =
    alloc_rooted_native_fn(st, fn_proto, get, "get " <> name, 0)
  let #(set_h, st) =
    alloc_rooted_native_fn(st, fn_proto, set, "set " <> name, 1)
  accessor_prop(
    st,
    get: Some(mk_object(get_h)),
    set: Some(mk_object(set_h)),
    enumerable: False,
    configurable: True,
  )
}

// ── proto/ctor cycle scaffold (port arc common.gleam:672-1000) ──────────────

/// Standard ctor properties list: `length` + `name` + `prototype` + extras.
fn ctor_properties(
  st: Agent,
  proto: Handle,
  name: String,
  arity: Int,
  extras: List(#(String, Property)),
) -> #(List(#(String, Property)), Agent) {
  let #(len_p, st) = fn_length_property(st, arity)
  let #(name_p, st) = fn_name_property(st, name)
  let #(proto_p, st) = fn_prototype_property(st, proto)
  // Restamp extras AFTER length/name/prototype so their seqs sort last —
  // matches arc's reserved-band seq=0/1/2 (arc common.gleam:528-560).
  let #(extras, st) =
    list.fold(extras, #([], st), fn(acc, kv) {
      let #(es, st) = acc
      let #(k, p) = kv
      let #(p, st) = restamp(st, p)
      #([#(k, p), ..es], st)
    })
  #(
    [
      #("length", len_p),
      #("name", name_p),
      #("prototype", proto_p),
      ..list.reverse(extras)
    ],
    st,
  )
}

/// Standard proto properties list: `constructor` + extras.
fn proto_properties(
  st: Agent,
  ctor: Handle,
  extras: List(#(String, Property)),
) -> #(List(#(String, Property)), Agent) {
  let #(ctor_p, st) = builtin_property(st, mk_object(ctor))
  #([#("constructor", ctor_p), ..extras], st)
}

/// Full proto-ctor cycle for a NEW builtin type — arc `init_type`. Reserves
/// the proto ref first, allocates ctor, then fills the proto (single write
/// each). `parent_proto` = the prototype's [[Prototype]]; `ctor_parent` = the
/// constructor's [[Prototype]] (§20.5.6.2 needs %Error% for NativeErrors).
pub fn init_type(
  st: Agent,
  parent_proto: Handle,
  ctor_parent: Handle,
  proto_props: List(#(String, Property)),
  ctor_tag: fn(Handle) -> NativeToken,
  name: String,
  arity: Int,
  ctor_props: List(#(String, Property)),
) -> #(BuiltinPair, Agent) {
  // Reserve proto address (empty ordinary object, patched below).
  let #(proto_h, st) = alloc_proto(st, Some(parent_proto), dict.new())
  // Allocate constructor with proto_h known.
  let #(ctor_all_props, st) =
    ctor_properties(st, proto_h, name, arity, ctor_props)
  let #(ctor_h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: KNative(
          tag: ctor_tag(proto_h),
          name:,
          length: arity,
          constructible: True,
        ),
        proto: Some(ctor_parent),
        props: named_props(ctor_all_props),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  let st = rt_store.t_pin_root(st, ctor_h)
  // Fill proto with constructor + proto_props.
  let #(all_proto_props, st) = proto_properties(st, ctor_h, proto_props)
  let st =
    rt_store.t_cell_update(st, proto_h, fn(slot) {
      let assert SObject(..) = slot
      SObject(..slot, props: named_props(all_proto_props))
    })
  #(BuiltinPair(prototype: proto_h, constructor: ctor_h), st)
}

/// A primitive-wrapper type (Boolean §20.3.3, Number §21.1.3, String §22.1.3).
/// Identical to `init_type` except `proto_kind` names the internal data slot
/// the spec puts on the PROTOTYPE object itself ([[BooleanData]] false,
/// [[NumberData]] +0, [[StringData]] ""). Routing wrappers here makes
/// "wrapper prototype without its data slot" a compile error. arc :775-798.
pub fn init_wrapper_type(
  st: Agent,
  parent_proto: Handle,
  ctor_parent: Handle,
  proto_props: List(#(String, Property)),
  ctor_tag: fn(Handle) -> NativeToken,
  name: String,
  arity: Int,
  ctor_props: List(#(String, Property)),
  proto_kind proto_kind: ObjKind,
) -> #(BuiltinPair, Agent) {
  let #(bt, st) =
    init_type(
      st,
      parent_proto,
      ctor_parent,
      proto_props,
      ctor_tag,
      name,
      arity,
      ctor_props,
    )
  let st =
    rt_store.t_cell_update(st, bt.prototype, fn(slot) {
      let assert SObject(..) = slot
      SObject(..slot, kind: proto_kind)
    })
  #(bt, st)
}

/// Allocate+root an ordinary object with `@@toStringTag = tag`. Covers
/// namespace globals (Math/JSON/Reflect/console/Atomics) and tagged
/// prototypes (Generator, Iterator Helper). arc `init_namespace` (:314-333).
pub fn init_namespace(
  st: Agent,
  object_proto: Handle,
  tag: String,
  props: List(#(String, Property)),
) -> #(Handle, Agent) {
  let #(tag_pair, st) = to_string_tag(st, tag)
  let #(h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: Ordinary,
        proto: Some(object_proto),
        props: named_props(props),
        symbol_props: [tag_pair],
        elements: NoElements,
        extensible: True,
      ),
    )
  #(h, rt_store.t_pin_root(st, h))
}

/// Proto-ctor cycle for a PRE-ALLOCATED prototype (Object, Function bootstrap)
/// — arc `init_type_on`. Read-modify-write merges proto_props onto existing.
pub fn init_type_on(
  st: Agent,
  proto_h: Handle,
  ctor_parent: Handle,
  proto_props: List(#(String, Property)),
  ctor_tag: fn(Handle) -> NativeToken,
  name: String,
  arity: Int,
  ctor_props: List(#(String, Property)),
  constructible: Bool,
) -> #(BuiltinPair, Agent) {
  let #(ctor_all_props, st) =
    ctor_properties(st, proto_h, name, arity, ctor_props)
  let #(ctor_h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: KNative(
          tag: ctor_tag(proto_h),
          name:,
          length: arity,
          constructible:,
        ),
        proto: Some(ctor_parent),
        props: named_props(ctor_all_props),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  let st = rt_store.t_pin_root(st, ctor_h)
  let #(all_proto_props, st) = proto_properties(st, ctor_h, proto_props)
  let st =
    rt_store.t_cell_update(st, proto_h, fn(slot) {
      let assert SObject(props: existing, ..) = slot
      let merged =
        list.fold(all_proto_props, existing, fn(acc, kv) {
          let #(k, v) = kv
          dict.insert(acc, Named(k), v)
        })
      SObject(..slot, props: merged)
    })
  #(BuiltinPair(prototype: proto_h, constructor: ctor_h), st)
}

/// Add a named property to an existing object — arc `add_named_property`.
/// `h` must be a live SObject (bootstrap invariant).
pub fn add_named_property(
  st: Agent,
  h: Handle,
  name: String,
  prop: Property,
) -> Agent {
  rt_store.t_cell_update(st, h, fn(slot) {
    let assert SObject(props:, ..) = slot
    SObject(..slot, props: dict.insert(props, Named(name), prop))
  })
}

/// Add a symbol-keyed property to an existing object — arc `add_symbol_property`.
pub fn add_symbol_property(
  st: Agent,
  h: Handle,
  sym: SymbolId,
  prop: Property,
) -> Agent {
  rt_store.t_cell_update(st, h, fn(slot) {
    let assert SObject(symbol_props:, ..) = slot
    SObject(..slot, symbol_props: list.key_set(symbol_props, sym, prop))
  })
}

/// `@@toStringTag` symbol-property pair `{W:F, E:F, C:T}` — arc `to_string_tag`.
pub fn to_string_tag(
  st: Agent,
  name: String,
) -> #(#(SymbolId, Property), Agent) {
  let #(prop, st) = data_prop(st, mk_string(name))
  #(#(rt_types.symbol_to_string_tag, configurable(prop)), st)
}

/// Add `@@toStringTag = name` to an existing object — arc `add_to_string_tag`.
pub fn add_to_string_tag(st: Agent, h: Handle, name: String) -> Agent {
  let #(#(sym, prop), st) = to_string_tag(st, name)
  add_symbol_property(st, h, sym, prop)
}

/// Add `get Constructor[@@species]` — an accessor whose getter returns `this`
/// (§25.1.5.3 / §25.2.4.2 / §23.2.2.4 / §27.2.4.9 — all identical). Token
/// parameterized (`return_this`) so this module needn't hardcode the concrete
/// `ReturnThis` variant that the native-tokens unit lands. arc :911-935.
pub fn add_species_accessor(
  st: Agent,
  fn_proto: Handle,
  ctor_h: Handle,
  return_this: NativeToken,
) -> Agent {
  let #(getter, st) =
    alloc_rooted_native_fn(st, fn_proto, return_this, "get [Symbol.species]", 0)
  let #(prop, st) =
    accessor_prop(
      st,
      get: Some(mk_object(getter)),
      set: None,
      enumerable: False,
      configurable: True,
    )
  add_symbol_property(st, ctor_h, rt_types.symbol_species, prop)
}

// ── error / array allocation (port arc common.gleam:1012-1229) ──────────────

/// Allocate an error instance slot (`kind: ErrorObj(stack:"")`, [[ErrorData]])
/// — arc `alloc_error_slot`. The single [[ErrorData]] slot shape.
pub fn alloc_error_slot(
  st: Agent,
  proto: Handle,
  props: List(#(String, Property)),
) -> #(Handle, Agent) {
  rt_store.t_cell_new(
    st,
    SObject(
      kind: ErrorObj(stack: ""),
      proto: Some(proto),
      props: named_props(props),
      symbol_props: [],
      elements: NoElements,
      extensible: True,
    ),
  )
}

/// Allocate a JS array from a list of values — arc `alloc_array` (§10.4.2.2
/// ArrayCreate). Does NOT root.
pub fn alloc_array(
  st: Agent,
  values: List(JsVal),
  array_proto: Handle,
) -> #(Handle, Agent) {
  let len = list.length(values)
  let elements = case values {
    [] -> NoElements
    _ -> Dense(tree_array.from_list(values, mk_undefined()))
  }
  rt_store.t_cell_new(
    st,
    SObject(
      kind: ArrayObj(length: len),
      proto: Some(array_proto),
      props: dict.new(),
      symbol_props: [],
      elements:,
      extensible: True,
    ),
  )
}
