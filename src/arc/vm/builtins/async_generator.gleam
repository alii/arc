/// AsyncGenerator.prototype — .next(), .return(), .throw()
///
/// Like Generator.prototype but methods return promises and the prototype
/// chain goes through %AsyncIteratorPrototype% instead of %IteratorPrototype%.
import arc/vm/builtins/common.{type GeneratorBuiltin, GeneratorBuiltin}
import arc/vm/heap.{type Heap}
import arc/vm/value.{
  type Ref, AsyncGeneratorNext, AsyncGeneratorReturn, AsyncGeneratorThrow,
}

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

  let #(h, proto) =
    common.init_namespace(h, async_iterator_proto, "AsyncGenerator", methods)
  #(h, GeneratorBuiltin(prototype: proto))
}
