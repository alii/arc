/// Generator.prototype builtins — .next(), .return(), .throw()
///
/// Generators don't have a user-visible constructor (can't `new Generator()`).
/// They're created internally by calling a `function*` generator function.
/// Generator.prototype provides the iteration methods.
import arc/vm/builtins/common.{type GeneratorBuiltin, GeneratorBuiltin}
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/value.{
  type Ref, NativeGeneratorNext, NativeGeneratorReturn, NativeGeneratorThrow,
  ObjectSlot,
}
import gleam/dict
import gleam/option.{Some}

/// Set up Generator.prototype with .next(), .return(), .throw() methods.
/// Generator.prototype inherits from %IteratorPrototype% (not Object.prototype directly).
/// Returns the Generator.prototype ref (no constructor needed).
pub fn init(
  h: Heap,
  iterator_proto: Ref,
  function_proto: Ref,
) -> #(Heap, GeneratorBuiltin) {
  let #(h, methods) =
    common.alloc_methods(h, function_proto, [
      #("next", NativeGeneratorNext, 1),
      #("return", NativeGeneratorReturn, 1),
      #("throw", NativeGeneratorThrow, 1),
    ])

  let symbol_properties =
    dict.from_list([
      #(
        value.symbol_to_string_tag,
        value.data(value.JsString("Generator")) |> value.configurable(),
      ),
    ])

  let #(h, gen_proto) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: value.OrdinaryObject,
        properties: dict.from_list(methods),
        symbol_properties:,
        elements: js_elements.new(),
        prototype: Some(iterator_proto),
        extensible: True,
      ),
    )
  let h = heap.root(h, gen_proto)

  #(h, GeneratorBuiltin(prototype: gen_proto))
}
