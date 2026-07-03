/// Symbol constructor and well-known symbol properties.
///
/// Symbol() is callable but NOT new-able (throws TypeError on `new Symbol()`).
/// Creates unique symbol values. Well-known symbols (Symbol.toStringTag, etc.)
/// are exposed as static properties on the Symbol function object.
import arc/vm/builtins/common
import arc/vm/heap.{type Heap}
import arc/vm/internal/elements
import arc/vm/value.{
  type JsValue, type Ref, JsObject, JsSymbol, ObjectSlot, SymbolConstructor,
  SymbolDescriptionGetter, SymbolFor, SymbolKeyFor, SymbolPrototypeToPrimitive,
  SymbolPrototypeToString, SymbolPrototypeValueOf,
}
import gleam/dict
import gleam/option.{type Option, None, Some}
import gleam/result

/// Set up the Symbol constructor (with well-known symbol properties) and a
/// dedicated %Symbol.prototype% (§20.4.3) carrying toString/valueOf, the
/// `description` getter, @@toPrimitive and @@toStringTag.
/// Returns #(heap, constructor_ref, prototype_ref).
pub fn init(
  h: Heap(ctx, host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(ctx, host), Ref, Ref) {
  // Reserve the prototype ref first: the constructor's `prototype` property
  // and the prototype's `constructor` property point at each other.
  let #(h, proto_ref) = heap.reserve(h)
  let h = heap.root(h, proto_ref)

  // Allocate Symbol.for and Symbol.keyFor static method function objects
  let #(h, for_ref) =
    common.alloc_call_fn(
      h,
      function_proto,
      SymbolFor,
      "for",
      1,
      constructible: False,
    )
  let #(h, key_for_ref) =
    common.alloc_call_fn(
      h,
      function_proto,
      SymbolKeyFor,
      "keyFor",
      1,
      constructible: False,
    )
  // Symbol constructor function object with all properties pre-built.
  // §20.4.1: Symbol HAS [[Construct]] (it may appear in an `extends`
  // clause), so IsConstructor(Symbol) is true — but invoking it as a
  // constructor always throws (do_construct / super() handle that).
  let #(h, ctor_ref) =
    common.alloc_call_fn_props(
      h,
      function_proto,
      SymbolConstructor,
      constructible: True,
      props: [
        #("length", common.fn_length_property(0)),
        #("name", common.fn_name_property("Symbol")),
        #("prototype", common.fn_prototype_property(proto_ref)),
        #("for", value.builtin_property(JsObject(for_ref))),
        #("keyFor", value.builtin_property(JsObject(key_for_ref))),
        // Well-known symbol properties
        #("toStringTag", value.data(JsSymbol(value.symbol_to_string_tag))),
        #("iterator", value.data(JsSymbol(value.symbol_iterator))),
        #("hasInstance", value.data(JsSymbol(value.symbol_has_instance))),
        #(
          "isConcatSpreadable",
          value.data(JsSymbol(value.symbol_is_concat_spreadable)),
        ),
        #("toPrimitive", value.data(JsSymbol(value.symbol_to_primitive))),
        #("species", value.data(JsSymbol(value.symbol_species))),
        #("asyncIterator", value.data(JsSymbol(value.symbol_async_iterator))),
        #("match", value.data(JsSymbol(value.symbol_match))),
        #("matchAll", value.data(JsSymbol(value.symbol_match_all))),
        #("replace", value.data(JsSymbol(value.symbol_replace))),
        #("search", value.data(JsSymbol(value.symbol_search))),
        #("split", value.data(JsSymbol(value.symbol_split))),
        #("unscopables", value.data(JsSymbol(value.symbol_unscopables))),
        #("dispose", value.data(JsSymbol(value.symbol_dispose))),
        #("asyncDispose", value.data(JsSymbol(value.symbol_async_dispose))),
      ],
    )
  let h = heap.root(h, ctor_ref)

  // %Symbol.prototype% methods (§20.4.3).
  let #(h, to_string_ref) =
    common.alloc_call_fn(
      h,
      function_proto,
      SymbolPrototypeToString,
      "toString",
      0,
      constructible: False,
    )
  let #(h, value_of_ref) =
    common.alloc_call_fn(
      h,
      function_proto,
      SymbolPrototypeValueOf,
      "valueOf",
      0,
      constructible: False,
    )
  // §20.4.3.5: @@toPrimitive { writable: false, enumerable: false,
  // configurable: true }, name "[Symbol.toPrimitive]", length 1.
  let #(h, to_primitive_ref) =
    common.alloc_call_fn(
      h,
      function_proto,
      SymbolPrototypeToPrimitive,
      "[Symbol.toPrimitive]",
      1,
      constructible: False,
    )
  // §20.4.3.2: get-only accessor `description`.
  let #(h, description_get_ref) =
    common.alloc_call_fn(
      h,
      function_proto,
      SymbolDescriptionGetter,
      "get description",
      0,
      constructible: False,
    )
  let h =
    heap.fill(
      h,
      proto_ref,
      ObjectSlot(
        kind: value.OrdinaryObject,
        properties: common.named_props([
          #("constructor", value.builtin_property(JsObject(ctor_ref))),
          #("toString", value.builtin_property(JsObject(to_string_ref))),
          #("valueOf", value.builtin_property(JsObject(value_of_ref))),
          #(
            "description",
            value.accessor(
              get: Some(JsObject(description_get_ref)),
              set: None,
              enumerable: False,
              configurable: True,
            ),
          ),
        ]),
        elements: elements.new(),
        prototype: Some(object_proto),
        symbol_properties: [
          common.to_string_tag("Symbol"),
          #(
            value.symbol_to_primitive,
            value.data(JsObject(to_primitive_ref)) |> value.configurable(),
          ),
        ],
        extensible: True,
      ),
    )

  #(h, ctor_ref, proto_ref)
}

@external(erlang, "erlang", "make_ref")
fn make_ref() -> value.ErlangRef

/// Create a new unique symbol reference (exposed for Symbol.for).
pub fn new_symbol_ref() -> value.ErlangRef {
  make_ref()
}

/// Symbol() call implementation. Creates a new unique symbol backed by
/// an Erlang reference — globally unique across the BEAM cluster.
///
/// §20.4.1.1 step 4 requires ToString(description) on any non-undefined
/// argument, which can run user code and throw — so the caller (the
/// SymbolConstructor dispatch arm) performs the coercion and hands us the
/// already-stringified description here.
pub fn call_symbol(
  description: Option(String),
  symbol_descriptions: dict.Dict(value.SymbolId, String),
) -> #(dict.Dict(value.SymbolId, String), JsValue) {
  let id = value.UserSymbol(make_ref())

  let new_descriptions = case description {
    Some(desc) -> dict.insert(symbol_descriptions, id, desc)
    None -> symbol_descriptions
  }

  #(new_descriptions, JsSymbol(id))
}

/// §20.4.3.3.1 SymbolDescriptiveString — "Symbol(" + description + ")".
/// Used by String(sym) and Symbol.prototype.toString. Unlike ToString(sym),
/// this never throws.
pub fn descriptive_string(
  id: value.SymbolId,
  symbol_descriptions: dict.Dict(value.SymbolId, String),
) -> String {
  let desc =
    value.well_known_symbol_description(id)
    |> option.lazy_unwrap(fn() {
      dict.get(symbol_descriptions, id) |> result.unwrap("")
    })
  "Symbol(" <> desc <> ")"
}
