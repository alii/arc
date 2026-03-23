/// Generator.prototype builtins — .next(), .return(), .throw()
///
/// Generators don't have a user-visible constructor (can't `new Generator()`).
/// They're created internally by calling a `function*` generator function.
/// Generator.prototype provides the iteration methods.
import arc/vm/builtins/common.{type GeneratorBuiltin, GeneratorBuiltin}
import arc/vm/heap.{type Heap}
import arc/vm/internal/elements
import arc/vm/value.{
  type Ref, GeneratorNext, GeneratorReturn, GeneratorThrow, ObjectSlot,
}
import gleam/dict
import gleam/option.{Some}

/// Set up Generator.prototype with .next(), .return(), .throw() methods.
/// Generator.prototype inherits from %IteratorPrototype% (not Object.prototype directly).
/// Returns the Generator.prototype ref (no constructor needed).
pub fn init(
  h: Heap(ctx),
  iterator_proto: Ref,
  function_proto: Ref,
) -> #(Heap(ctx), GeneratorBuiltin) {
  let #(h, methods) =
    common.alloc_call_methods(h, function_proto, [
      #("next", GeneratorNext, 1),
      #("return", GeneratorReturn, 1),
      #("throw", GeneratorThrow, 1),
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
        properties: common.named_props(methods),
        symbol_properties:,
        elements: elements.new(),
        prototype: Some(iterator_proto),
        extensible: True,
      ),
    )
  let h = heap.root(h, gen_proto)

  #(h, GeneratorBuiltin(prototype: gen_proto))
}
