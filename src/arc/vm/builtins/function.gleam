import arc/vm/builtins/common.{type BuiltinType, alloc_proto}
import arc/vm/heap.{type Heap}
import arc/vm/value.{
  type Ref, NativeFunctionApply, NativeFunctionBind, NativeFunctionCall,
  NativeFunctionConstructor, NativeFunctionToString,
}
import gleam/dict
import gleam/option.{Some}

/// Set up Function.prototype and Function constructor.
pub fn init(h: Heap, object_proto: Ref) -> #(Heap, BuiltinType) {
  // Allocate func_proto first (empty) so call/apply/bind can reference it
  // as their [[Prototype]] from the start — no fix-up needed.
  let #(h, func_proto) = alloc_proto(h, Some(object_proto), dict.new())

  // Allocate methods with the real func_proto as their prototype
  let #(h, proto_methods) =
    common.alloc_methods(h, func_proto, [
      #("call", NativeFunctionCall, 1),
      #("apply", NativeFunctionApply, 2),
      #("bind", NativeFunctionBind, 1),
      #("toString", NativeFunctionToString, 0),
    ])

  // Constructor's [[Prototype]] is also func_proto (self-referencing bootstrap)
  common.init_type_on(
    h,
    func_proto,
    func_proto,
    proto_methods,
    fn(_) { NativeFunctionConstructor },
    "Function",
    1,
    [],
  )
}
