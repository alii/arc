//// alloc and install kit for builtin objects

import arc/internal/tree_array
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type Handle, type JsVal, type NativeToken,
  type ObjKind, type Property, type PropertyKey, type SymbolId, AccessorProperty,
  ArrayObj, BuiltinPair, DataProperty, Dense, ErrorObj, Named, NativeFn,
  NoElements, Ordinary, SObject, mk_int, mk_object, mk_string,
} as rt_types
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}

pub fn frozen_property(st: Agent, val: JsVal) -> #(Property, Agent) {
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

pub fn plain_property(st: Agent, val: JsVal) -> #(Property, Agent) {
  let #(seq, st) = rt_store.t_next_prop_seq(st)
  #(plain_property_at(val, seq), st)
}

pub fn plain_property_at(val: JsVal, seq: Int) -> Property {
  DataProperty(
    value: val,
    writable: True,
    enumerable: True,
    configurable: True,
    seq:,
  )
}

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

pub fn make_configurable(prop: Property) -> Property {
  case prop {
    DataProperty(..) -> DataProperty(..prop, configurable: True)
    AccessorProperty(..) -> AccessorProperty(..prop, configurable: True)
  }
}

pub fn restamp(st: Agent, prop: Property) -> #(Property, Agent) {
  let #(seq, st) = rt_store.t_next_prop_seq(st)
  let prop = case prop {
    DataProperty(..) -> DataProperty(..prop, seq:)
    AccessorProperty(..) -> AccessorProperty(..prop, seq:)
  }
  #(prop, st)
}

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

pub fn fn_length_property(st: Agent, arity: Int) -> #(Property, Agent) {
  let #(seq, st) = rt_store.t_next_prop_seq(st)
  #(
    DataProperty(
      value: mk_int(arity),
      writable: False,
      enumerable: False,
      configurable: True,
      seq:,
    ),
    st,
  )
}

pub fn fn_prototype_property(st: Agent, proto: Handle) -> #(Property, Agent) {
  frozen_property(st, mk_object(proto))
}

pub fn named_props(
  props: List(#(String, Property)),
) -> Dict(PropertyKey, Property) {
  use acc, #(k, v) <- list.fold(props, dict.new())
  dict.insert(acc, Named(k), v)
}

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

pub fn alloc_plain_object(
  st: Agent,
  object_proto: Handle,
  props: List(#(String, JsVal)),
) -> #(Handle, Agent) {
  use seq <- rt_store.t_cell_new_with(st, list.length(props))
  let entries =
    list.index_map(props, fn(kv, i) {
      #(Named(kv.0), plain_property_at(kv.1, seq + i))
    })
  SObject(
    kind: Ordinary,
    proto: Some(object_proto),
    props: dict.from_list(entries),
    symbol_props: [],
    elements: NoElements,
    extensible: True,
  )
}

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

pub fn alloc_methods(
  st: Agent,
  fn_proto: Handle,
  specs: List(#(String, NativeToken, Int)),
) -> #(List(#(String, Property)), Agent) {
  use #(props, st), #(name, tag, arity) <- list.fold(specs, #([], st))
  let #(fn_h, st) = alloc_rooted_native_fn(st, fn_proto, tag, name, arity)
  let #(prop, st) = builtin_property(st, mk_object(fn_h))
  #([#(name, prop), ..props], st)
}

pub fn alloc_getters(
  st: Agent,
  fn_proto: Handle,
  specs: List(#(String, NativeToken)),
) -> #(List(#(String, Property)), Agent) {
  use #(props, st), #(name, tag) <- list.fold(specs, #([], st))
  let #(fn_h, st) = alloc_rooted_native_fn(st, fn_proto, tag, "get " <> name, 0)
  let #(prop, st) =
    accessor_prop(
      st,
      get: Some(mk_object(fn_h)),
      set: None,
      enumerable: False,
      configurable: True,
    )
  #([#(name, prop), ..props], st)
}

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
  // restamp extras so they sort after length/name/prototype
  let #(extras, st) = {
    use #(es, st), #(k, p) <- list.fold(extras, #([], st))
    let #(p, st) = restamp(st, p)
    #([#(k, p), ..es], st)
  }
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

fn proto_properties(
  st: Agent,
  ctor: Handle,
  extras: List(#(String, Property)),
) -> #(List(#(String, Property)), Agent) {
  let #(ctor_p, st) = builtin_property(st, mk_object(ctor))
  #([#("constructor", ctor_p), ..extras], st)
}

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
  let #(proto_h, st) = alloc_proto(st, Some(parent_proto), dict.new())
  init_type_on(
    st,
    proto_h,
    ctor_parent,
    proto_props,
    ctor_tag,
    name,
    arity,
    ctor_props,
    constructible: True,
  )
}

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
    rt_store.t_cell_update(st, bt.prototype, fn(cell) {
      let assert SObject(..) = cell
      SObject(..cell, kind: proto_kind)
    })
  #(bt, st)
}

pub fn init_namespace(
  st: Agent,
  object_proto: Handle,
  tag: String,
  props: List(#(String, Property)),
) -> #(Handle, Agent) {
  let #(tag_pair, st) = string_tag_property(st, tag)
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

// installs a constructor over an already allocated prototype
pub fn init_type_on(
  st: Agent,
  proto_h: Handle,
  ctor_parent: Handle,
  proto_props: List(#(String, Property)),
  ctor_tag: fn(Handle) -> NativeToken,
  name: String,
  arity: Int,
  ctor_props: List(#(String, Property)),
  constructible constructible: Bool,
) -> #(BuiltinPair, Agent) {
  let #(ctor_all_props, st) =
    ctor_properties(st, proto_h, name, arity, ctor_props)
  let #(ctor_h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: NativeFn(
          token: ctor_tag(proto_h),
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
    rt_store.t_cell_update(st, proto_h, fn(cell) {
      let assert SObject(props: existing, ..) = cell
      let merged = {
        use acc, #(k, v) <- list.fold(all_proto_props, existing)
        dict.insert(acc, Named(k), v)
      }
      SObject(..cell, props: merged)
    })
  #(BuiltinPair(prototype: proto_h, constructor: ctor_h), st)
}

pub fn add_named_property(
  st: Agent,
  h: Handle,
  name: String,
  prop: Property,
) -> Agent {
  rt_store.t_cell_update(st, h, fn(cell) {
    let assert SObject(props:, ..) = cell
    SObject(..cell, props: dict.insert(props, Named(name), prop))
  })
}

pub fn add_symbol_property(
  st: Agent,
  h: Handle,
  sym: SymbolId,
  prop: Property,
) -> Agent {
  rt_store.t_cell_update(st, h, fn(cell) {
    let assert SObject(symbol_props:, ..) = cell
    SObject(..cell, symbol_props: list.key_set(symbol_props, sym, prop))
  })
}

pub fn string_tag_property(
  st: Agent,
  name: String,
) -> #(#(SymbolId, Property), Agent) {
  let #(prop, st) = frozen_property(st, mk_string(name))
  #(#(rt_types.symbol_to_string_tag, make_configurable(prop)), st)
}

pub fn add_string_tag(st: Agent, h: Handle, name: String) -> Agent {
  let #(#(sym, prop), st) = string_tag_property(st, name)
  add_symbol_property(st, h, sym, prop)
}

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

// species lookup on pair still yields its ctor without user code
pub fn species_intact(st: Agent, pair: BuiltinPair) -> Bool {
  let BuiltinPair(prototype:, constructor:) = pair
  case
    rt_obj.t_ordinary_own_property(
      st,
      prototype,
      rt_types.StringKey(Named("constructor")),
    ),
    rt_obj.t_ordinary_own_property(
      st,
      constructor,
      rt_types.SymbolKey(rt_types.symbol_species),
    )
  {
    Some(DataProperty(value:, ..)),
      Some(AccessorProperty(get: Some(getter), ..))
    -> value == mk_object(constructor) && is_return_this(st, getter)
    _, _ -> False
  }
}

fn is_return_this(st: Agent, f: JsVal) -> Bool {
  case rt_types.classify(f) {
    rt_types.KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: NativeFn(token: rt_types.ReturnThis, ..), ..) -> True
        _ -> False
      }
    _ -> False
  }
}

pub fn alloc_error_object(
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

pub fn alloc_array(
  st: Agent,
  values: List(JsVal),
  array_proto: Handle,
) -> #(Handle, Agent) {
  let len = list.length(values)
  let elements = case values {
    [] -> NoElements
    _ -> Dense(tree_array.from_list(values))
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
