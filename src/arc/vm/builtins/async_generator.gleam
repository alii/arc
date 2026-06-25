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
  h: Heap(ctx, host),
  async_iterator_proto: Ref,
  function_proto: Ref,
  function_ctor: Ref,
) -> #(Heap(ctx, host), GeneratorBuiltin) {
  let #(h, methods) =
    common.alloc_call_methods(h, function_proto, [
      #("next", AsyncGeneratorNext, 1),
      #("return", AsyncGeneratorReturn, 1),
      #("throw", AsyncGeneratorThrow, 1),
    ])

  let #(h, proto) =
    common.init_namespace(h, async_iterator_proto, "AsyncGenerator", methods)
  // §27.4: %AsyncGeneratorFunction% + its .prototype (the [[Prototype]] of
  // async generator function objects).
  let #(h, fn_proto) =
    common.init_generator_function(
      h,
      "AsyncGeneratorFunction",
      value.VmNative(value.AsyncGeneratorFunctionConstructor),
      function_proto,
      function_ctor,
      proto,
    )
  #(h, GeneratorBuiltin(prototype: proto, fn_proto:))
}
