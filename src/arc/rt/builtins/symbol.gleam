import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type Handle, type JsVal, type SymbolId,
  type SymbolNative, BuiltinPair, KHandle, KNative, KSym, KUndef, NoElements,
  Ordinary, RegisteredSymbol, SObject, SymbolConstructor,
  SymbolDescriptionGetter, SymbolFor, SymbolKeyFor, SymbolN, SymbolObj,
  SymbolToPrimitive, SymbolToString, SymbolValueOf, UserSymbol, classify,
  mk_object, mk_string, mk_symbol, mk_undefined, symbol_description,
  symbol_descriptive_string,
} as rt_types
import arc/rt/val as rt_val
import gleam/dict
import gleam/option.{type Option, None, Some}

pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
) -> #(BuiltinPair, Agent) {
  let #(prototype, st) = common.alloc_proto(st, Some(object_proto), dict.new())
  let #(for_ref, st) =
    common.alloc_rooted_native_fn(st, fn_proto, SymbolN(SymbolFor), "for", 1)
  let #(key_for_ref, st) =
    common.alloc_rooted_native_fn(
      st,
      fn_proto,
      SymbolN(SymbolKeyFor),
      "keyFor",
      1,
    )
  let #(len_p, st) = common.fn_length_property(st, 0)
  let #(name_p, st) = common.fn_name_property(st, "Symbol")
  let #(proto_p, st) = common.fn_prototype_property(st, prototype)
  let #(for_p, st) = common.builtin_property(st, mk_object(for_ref))
  let #(key_for_p, st) = common.builtin_property(st, mk_object(key_for_ref))
  let #(wk_props, st) =
    well_known_properties(st, [
      #("toStringTag", rt_types.symbol_to_string_tag),
      #("iterator", rt_types.symbol_iterator),
      #("hasInstance", rt_types.symbol_has_instance),
      #("isConcatSpreadable", rt_types.symbol_is_concat_spreadable),
      #("toPrimitive", rt_types.symbol_to_primitive),
      #("species", rt_types.symbol_species),
      #("asyncIterator", rt_types.symbol_async_iterator),
      #("match", rt_types.symbol_match),
      #("matchAll", rt_types.symbol_match_all),
      #("replace", rt_types.symbol_replace),
      #("search", rt_types.symbol_search),
      #("split", rt_types.symbol_split),
      #("unscopables", rt_types.symbol_unscopables),
      #("dispose", rt_types.symbol_dispose),
      #("asyncDispose", rt_types.symbol_async_dispose),
    ])
  let ctor_props =
    common.named_props([
      #("length", len_p),
      #("name", name_p),
      #("prototype", proto_p),
      #("for", for_p),
      #("keyFor", key_for_p),
      ..wk_props
    ])
  let #(constructor, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: KNative(
          tag: SymbolN(SymbolConstructor),
          name: "Symbol",
          length: 0,
          constructible: True,
        ),
        proto: Some(fn_proto),
        props: ctor_props,
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  let st = rt_store.t_pin_root(st, constructor)
  let #(to_string_ref, st) =
    common.alloc_rooted_native_fn(
      st,
      fn_proto,
      SymbolN(SymbolToString),
      "toString",
      0,
    )
  let #(value_of_ref, st) =
    common.alloc_rooted_native_fn(
      st,
      fn_proto,
      SymbolN(SymbolValueOf),
      "valueOf",
      0,
    )
  let #(to_primitive_ref, st) =
    common.alloc_rooted_native_fn(
      st,
      fn_proto,
      SymbolN(SymbolToPrimitive),
      "[Symbol.toPrimitive]",
      1,
    )
  let #(description_get_ref, st) =
    common.alloc_rooted_native_fn(
      st,
      fn_proto,
      SymbolN(SymbolDescriptionGetter),
      "get description",
      0,
    )
  let #(ctor_p, st) = common.builtin_property(st, mk_object(constructor))
  let #(ts_p, st) = common.builtin_property(st, mk_object(to_string_ref))
  let #(vo_p, st) = common.builtin_property(st, mk_object(value_of_ref))
  let #(desc_p, st) =
    common.accessor_prop(
      st,
      get: Some(mk_object(description_get_ref)),
      set: None,
      enumerable: False,
      configurable: True,
    )
  let #(tag_pair, st) = common.to_string_tag(st, "Symbol")
  let #(to_prim_p, st) = common.data_prop(st, mk_object(to_primitive_ref))
  let st =
    rt_store.t_cell_update(st, prototype, fn(slot) {
      let assert SObject(..) = slot
      SObject(
        ..slot,
        kind: Ordinary,
        props: common.named_props([
          #("constructor", ctor_p),
          #("toString", ts_p),
          #("valueOf", vo_p),
          #("description", desc_p),
        ]),
        symbol_props: [
          tag_pair,
          #(rt_types.symbol_to_primitive, common.configurable(to_prim_p)),
        ],
      )
    })
  #(BuiltinPair(constructor:, prototype:), st)
}

fn well_known_properties(
  st: Agent,
  specs: List(#(String, SymbolId)),
) -> #(List(#(String, rt_types.Property)), Agent) {
  case specs {
    [] -> #([], st)
    [#(name, id), ..rest] -> {
      let #(prop, st) = common.data_prop(st, mk_symbol(id))
      let #(tail, st) = well_known_properties(st, rest)
      #([#(name, prop), ..tail], st)
    }
  }
}

pub fn new_symbol(
  st: Agent,
  description: Option(String),
) -> #(SymbolId, Agent) {
  let #(uid, st) = rt_store.t_next_symbol_uid(st)
  #(UserSymbol(uid:, description:), st)
}

pub fn dispatch(
  st: Agent,
  native: SymbolNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    SymbolConstructor -> call_as_function(st, args)
    SymbolFor -> symbol_for(st, args)
    SymbolKeyFor -> symbol_key_for(st, args)
    SymbolToString -> to_string(st, this)
    SymbolValueOf -> this_symbol_result(st, this, "valueOf")
    SymbolToPrimitive -> this_symbol_result(st, this, "[Symbol.toPrimitive]")
    SymbolDescriptionGetter -> description_getter(st, this)
  }
}

// §20.4.1.1 symbol call
fn call_as_function(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  case classify(helpers.first_arg_or_undefined(args)) {
    KUndef -> {
      let #(id, st) = new_symbol(st, None)
      #(mk_symbol(id), st)
    }
    _ -> {
      let #(s, st) =
        rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
      let #(id, st) = new_symbol(st, Some(s))
      #(mk_symbol(id), st)
    }
  }
}

// §20.4.2.2 registered symbols are equal by key, no registry
fn symbol_for(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(key, st) = rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
  #(mk_symbol(RegisteredSymbol(key:)), st)
}

// §20.4.2.6
fn symbol_key_for(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  case classify(helpers.first_arg_or_undefined(args)) {
    KSym(RegisteredSymbol(key:)) -> #(mk_string(key), st)
    KSym(_) -> #(mk_undefined(), st)
    _ ->
      rt_val.t_throw_type_error(st, "Symbol.keyFor requires a Symbol argument")
  }
}

fn to_string(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let id = this_symbol_value(st, this, "toString")
  #(mk_string(symbol_descriptive_string(id)), st)
}

fn description_getter(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let id = this_symbol_value(st, this, "description")
  case symbol_description(id) {
    Some(s) -> #(mk_string(s), st)
    None -> #(mk_undefined(), st)
  }
}

fn this_symbol_result(
  st: Agent,
  this: JsVal,
  method: String,
) -> #(JsVal, Agent) {
  #(mk_symbol(this_symbol_value(st, this, method)), st)
}

// §20.4.3 thissymbolvalue
fn this_symbol_value(st: Agent, this: JsVal, method: String) -> SymbolId {
  case classify(this) {
    KSym(id) -> id
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: SymbolObj(value: id), ..) -> id
        _ -> not_a_symbol(st, method)
      }
    _ -> not_a_symbol(st, method)
  }
}

fn not_a_symbol(st: Agent, method: String) -> a {
  rt_val.t_throw_type_error(
    st,
    "Symbol.prototype." <> method <> " requires that 'this' be a Symbol",
  )
}
