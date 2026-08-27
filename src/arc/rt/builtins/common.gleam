import arc/internal/tree_array
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type Handle, type JsVal, type NativeToken,
  type ObjKind, type Property, type PropertyKey, type SymbolId, AccessorProperty,
  ArrayObj, BuiltinPair, DataProperty, Dense, ErrorObj, JInt, KNative, Named,
  NoElements, Ordinary, SObject, mk_number, mk_object, mk_string,
} as rt_types
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}

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

pub fn configurable(prop: Property) -> Property {
  case prop {
    DataProperty(value:, writable:, enumerable:, seq:, ..) ->
      DataProperty(value:, writable:, enumerable:, configurable: True, seq:)
    AccessorProperty(get:, set:, enumerable:, seq:, ..) ->
      AccessorProperty(get:, set:, enumerable:, configurable: True, seq:)
  }
}

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
      value: mk_number(JInt(arity)),
      writable: False,
      enumerable: False,
      configurable: True,
      seq:,
    ),
    st,
  )
}

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

pub fn alloc_pojo(
  st: Agent,
  object_proto: Handle,
  props: List(#(String, JsVal)),
) -> #(Handle, Agent) {
  use seq <- rt_store.t_cell_new_with(st, list.length(props))
  let entries =
    list.index_map(props, fn(kv, i) {
      #(Named(kv.0), DataProperty(kv.1, True, True, True, seq + i))
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

pub fn alloc_native_fn(
  st: Agent,
  fn_proto: Handle,
  tag: NativeToken,
  name: String,
  arity: Int,
) -> #(Handle, Agent) {
  alloc_rooted_native_fn(st, fn_proto, tag, name, arity)
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
  list.fold(specs, #([], st), fn(acc, spec) {
    let #(props, st) = acc
    let #(name, tag, arity) = spec
    let #(fn_h, st) = alloc_rooted_native_fn(st, fn_proto, tag, name, arity)
    let #(prop, st) = builtin_property(st, mk_object(fn_h))
    #([#(name, prop), ..props], st)
  })
}

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
  let #(all_proto_props, st) = proto_properties(st, ctor_h, proto_props)
  let st =
    rt_store.t_cell_update(st, proto_h, fn(slot) {
      let assert SObject(..) = slot
      SObject(..slot, props: named_props(all_proto_props))
    })
  #(BuiltinPair(prototype: proto_h, constructor: ctor_h), st)
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
    rt_store.t_cell_update(st, bt.prototype, fn(slot) {
      let assert SObject(..) = slot
      SObject(..slot, kind: proto_kind)
    })
  #(bt, st)
}

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

pub fn to_string_tag(
  st: Agent,
  name: String,
) -> #(#(SymbolId, Property), Agent) {
  let #(prop, st) = data_prop(st, mk_string(name))
  #(#(rt_types.symbol_to_string_tag, configurable(prop)), st)
}

pub fn add_to_string_tag(st: Agent, h: Handle, name: String) -> Agent {
  let #(#(sym, prop), st) = to_string_tag(st, name)
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
        SObject(kind: KNative(tag: rt_types.ReturnThis, ..), ..) -> True
        _ -> False
      }
    _ -> False
  }
}

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

pub fn alloc_array(
  st: Agent,
  values: List(JsVal),
  array_proto: Handle,
) -> #(Handle, Agent) {
  let len = list.length(values)
  let elements = case values {
    [] -> NoElements
    _ -> Dense(tree_array.from_list(values, rt_types.mk_hole()))
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
