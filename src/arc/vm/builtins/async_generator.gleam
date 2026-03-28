/// AsyncGenerator.prototype — .next(), .return(), .throw()
///
/// Like Generator.prototype but methods return promises and the prototype
/// chain goes through %AsyncIteratorPrototype% instead of %IteratorPrototype%.
import arc/vm/builtins/common.{type GeneratorBuiltin, GeneratorBuiltin}
import arc/vm/heap.{type Heap}
import arc/vm/internal/elements
import arc/vm/value.{
  type Ref, AsyncGeneratorNext, AsyncGeneratorReturn, AsyncGeneratorThrow,
  ObjectSlot,
}
import gleam/option.{Some}

/// Set up AsyncGenerator.prototype with .next/.return/.throw.
/// Inherits from %AsyncIteratorPrototype%.
pub fn init(
  h: Heap(ctx),
  async_iterator_proto: Ref,
  function_proto: Ref,
) -> #(Heap(ctx), GeneratorBuiltin) {
  let #(h, methods) =
    common.alloc_call_methods(h, function_proto, [
      #("next", AsyncGeneratorNext, 1),
      #("return", AsyncGeneratorReturn, 1),
      #("throw", AsyncGeneratorThrow, 1),
    ])

  let symbol_properties = [
    #(
      value.symbol_to_string_tag,
      value.data(value.JsString("AsyncGenerator")) |> value.configurable(),
    ),
  ]

  let #(h, proto) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: value.OrdinaryObject,
        properties: common.named_props(methods),
        symbol_properties:,
        elements: elements.new(),
        prototype: Some(async_iterator_proto),
        extensible: True,
      ),
    )
  let h = heap.root(h, proto)

  #(h, GeneratorBuiltin(prototype: proto))
}
