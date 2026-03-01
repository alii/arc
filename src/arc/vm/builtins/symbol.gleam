/// Symbol constructor and well-known symbol properties.
///
/// Symbol() is callable but NOT new-able (throws TypeError on `new Symbol()`).
/// Creates unique symbol values. Well-known symbols (Symbol.toStringTag, etc.)
/// are exposed as static properties on the Symbol function object.
import arc/vm/builtins/common
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/value.{
  type JsValue, type Ref, Call, JsObject, JsString, JsSymbol, NativeFunction,
  ObjectSlot, SymbolConstructor, SymbolFor, SymbolKeyFor,
}
import gleam/dict
import gleam/option.{Some}

/// Set up Symbol constructor function with well-known symbol properties.
/// Returns #(heap, constructor_ref).
pub fn init(h: Heap, object_proto: Ref, function_proto: Ref) -> #(Heap, Ref) {
  // Allocate Symbol.for and Symbol.keyFor static method function objects
  let #(h, for_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(Call(SymbolFor)),
        properties: dict.from_list([
          #("name", common.fn_name_property("for")),
          #("length", common.fn_length_property(1)),
        ]),
        elements: js_elements.new(),
        prototype: Some(function_proto),
        symbol_properties: dict.new(),
        extensible: True,
      ),
    )
  let #(h, key_for_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(Call(SymbolKeyFor)),
        properties: dict.from_list([
          #("name", common.fn_name_property("keyFor")),
          #("length", common.fn_length_property(1)),
        ]),
        elements: js_elements.new(),
        prototype: Some(function_proto),
        symbol_properties: dict.new(),
        extensible: True,
      ),
    )
  // Symbol constructor function object with all properties pre-built
  let #(h, ctor_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(Call(SymbolConstructor)),
        properties: dict.from_list([
          #("name", common.fn_name_property("Symbol")),
          #("length", common.fn_length_property(0)),
          #("prototype", value.data(JsObject(object_proto))),
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
        ]),
        elements: js_elements.new(),
        prototype: Some(function_proto),
        symbol_properties: dict.new(),
        extensible: True,
      ),
    )
  let h = heap.root(h, ctor_ref)

  #(h, ctor_ref)
}

@external(erlang, "erlang", "make_ref")
fn make_ref() -> value.ErlangRef

/// Create a new unique symbol reference (exposed for Symbol.for).
pub fn new_symbol_ref() -> value.ErlangRef {
  make_ref()
}

/// Symbol() call implementation. Creates a new unique symbol backed by
/// an Erlang reference — globally unique across the BEAM cluster.
pub fn call_symbol(
  args: List(JsValue),
  symbol_descriptions: dict.Dict(value.SymbolId, String),
) -> #(dict.Dict(value.SymbolId, String), JsValue) {
  let id = value.UserSymbol(make_ref())

  // Optional description argument
  let new_descriptions = case args {
    [JsString(desc), ..] -> dict.insert(symbol_descriptions, id, desc)
    _ -> symbol_descriptions
  }

  #(new_descriptions, JsSymbol(id))
}
