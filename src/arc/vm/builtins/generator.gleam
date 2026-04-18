/// Generator.prototype builtins — .next(), .return(), .throw()
///
/// Generators don't have a user-visible constructor (can't `new Generator()`).
/// They're created internally by calling a `function*` generator function.
/// Generator.prototype provides the iteration methods.
import arc/vm/builtins/common.{type GeneratorBuiltin, GeneratorBuiltin}
import arc/vm/heap.{type Heap}
import arc/vm/value.{type Ref, GeneratorNext, GeneratorReturn, GeneratorThrow}

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

  let #(h, proto) =
    common.init_namespace(h, iterator_proto, "Generator", methods)
  #(h, GeneratorBuiltin(prototype: proto))
}
