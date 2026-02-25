/// Symbol constructor and well-known symbol properties.
///
/// Symbol() is callable but NOT new-able (throws TypeError on `new Symbol()`).
/// Creates unique symbol values. Well-known symbols (Symbol.toStringTag, etc.)
/// are exposed as static properties on the Symbol function object.
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/value.{
  type JsValue, type Ref, Finite, JsNumber, JsObject, JsString, JsSymbol,
  NativeFunction, NativeSymbolConstructor, ObjectSlot,
}
import gleam/dict
import gleam/option.{Some}

/// Set up Symbol constructor function with well-known symbol properties.
/// Returns #(heap, constructor_ref).
pub fn init(h: Heap, object_proto: Ref, function_proto: Ref) -> #(Heap, Ref) {
  // Symbol constructor function object with all properties pre-built
  let #(h, ctor_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(NativeSymbolConstructor),
        properties: dict.from_list([
          #("name", value.builtin_property(JsString("Symbol"))),
          #("length", value.builtin_property(JsNumber(Finite(0.0)))),
          #("prototype", value.data(JsObject(object_proto))),
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

/// Symbol() call implementation. Creates a new unique symbol.
/// Returns #(heap, next_symbol_id, result).
pub fn call_symbol(
  args: List(JsValue),
  next_symbol_id: Int,
  symbol_descriptions: dict.Dict(Int, String),
) -> #(Int, dict.Dict(Int, String), JsValue) {
  let id = value.SymbolId(next_symbol_id)
  let new_next = next_symbol_id + 1

  // Optional description argument
  let new_descriptions = case args {
    [JsString(desc), ..] ->
      dict.insert(symbol_descriptions, next_symbol_id, desc)
    _ -> symbol_descriptions
  }

  #(new_next, new_descriptions, JsSymbol(id))
}
