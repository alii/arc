//// `rt_builtins/symbol` — Symbol constructor, well-known symbol
//// properties, and %Symbol.prototype% (ES2024 §20.4) over the threaded
//// `Agent` model (D7/R1). D14: user symbols carry a threaded Int uid;
//// registered symbols are structural (`RegisteredSymbol(key)`),
//// so `Symbol.for("x") === Symbol.for("x")` by term equality with NO
//// registry dict on `JsStore`.

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

/// Set up the Symbol constructor (with well-known symbol properties) and
/// %Symbol.prototype% (§20.4.3) with toString/valueOf, the `description`
/// getter, @@toPrimitive and @@toStringTag.
pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
) -> #(BuiltinPair, Agent) {
  // Reserve the prototype ref first: constructor's `prototype` and the
  // prototype's `constructor` point at each other.
  let #(prototype, st) = common.alloc_proto(st, Some(object_proto), dict.new())
  // Symbol.for / Symbol.keyFor static function objects.
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
  // Symbol constructor function object with all properties. §20.4.1: Symbol
  // HAS [[Construct]] (may appear in `extends`), so `constructible: True`;
  // invoking as constructor throws in `t_construct`.
  let #(len_p, st) = common.fn_length_property(st, 0)
  let #(name_p, st) = common.fn_name_property(st, "Symbol")
  let #(proto_p, st) = common.fn_prototype_property(st, prototype)
  let #(for_p, st) = common.builtin_property(st, mk_object(for_ref))
  let #(key_for_p, st) = common.builtin_property(st, mk_object(key_for_ref))
  // Well-known symbol properties {W:F, E:F, C:F}.
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
  // %Symbol.prototype% methods (§20.4.3).
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
  // §20.4.3.5: @@toPrimitive {W:F,E:F,C:T}, name "[Symbol.toPrimitive]", len 1.
  let #(to_primitive_ref, st) =
    common.alloc_rooted_native_fn(
      st,
      fn_proto,
      SymbolN(SymbolToPrimitive),
      "[Symbol.toPrimitive]",
      1,
    )
  // §20.4.3.2: get-only accessor `description`.
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

/// Fold well-known-symbol constants into `{W:F,E:F,C:F}` data properties.
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

/// Mint a `Symbol(desc)` symbol — unique, never registered.
pub fn new_symbol(
  st: Agent,
  description: Option(String),
) -> #(SymbolId, Agent) {
  let #(uid, st) = rt_store.t_next_symbol_uid(st)
  #(UserSymbol(uid:, description:), st)
}

/// Per-module dispatch for Symbol native functions.
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
    // §20.4.3.5: @@toPrimitive ignores its hint argument entirely.
    SymbolToPrimitive -> this_symbol_result(st, this, "[Symbol.toPrimitive]")
    SymbolDescriptionGetter -> description_getter(st, this)
  }
}

/// §20.4.1.1 Symbol ( [ description ] ) — call semantics. Step 1 (NewTarget
/// throw) is handled in `t_construct`. Step 4 requires ToString(description)
/// on any non-undefined argument (can run user code and throw).
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

/// §20.4.2.2 Symbol.for ( key ) — global symbol registry lookup/insert.
/// D14: `RegisteredSymbol(key)` values are term-equal by key, so no registry
/// dict is needed — two calls yield structurally identical `SymbolId`s.
fn symbol_for(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(key, st) = rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
  #(mk_symbol(RegisteredSymbol(key:)), st)
}

/// §20.4.2.6 Symbol.keyFor ( sym ) — reverse registry lookup. A registered
/// symbol carries its own key (pure O(1) read).
fn symbol_key_for(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  case classify(helpers.first_arg_or_undefined(args)) {
    KSym(RegisteredSymbol(key:)) -> #(mk_string(key), st)
    KSym(_) -> #(mk_undefined(), st)
    _ ->
      rt_val.t_throw_type_error(st, "Symbol.keyFor requires a Symbol argument")
  }
}

/// §20.4.3.3 Symbol.prototype.toString — SymbolDescriptiveString(thisSymbolValue).
fn to_string(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let id = this_symbol_value(st, this, "toString")
  #(mk_string(symbol_descriptive_string(id)), st)
}

/// §20.4.3.2 get Symbol.prototype.description — [[Description]] or undefined.
fn description_getter(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let id = this_symbol_value(st, this, "description")
  case symbol_description(id) {
    Some(s) -> #(mk_string(s), st)
    None -> #(mk_undefined(), st)
  }
}

/// valueOf / @@toPrimitive: return thisSymbolValue as a JsSymbol.
fn this_symbol_result(
  st: Agent,
  this: JsVal,
  method: String,
) -> #(JsVal, Agent) {
  #(mk_symbol(this_symbol_value(st, this, method)), st)
}

/// §20.4.3 thisSymbolValue(value): a Symbol primitive, or a Symbol wrapper
/// object's [[SymbolData]]; anything else → TypeError.
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
