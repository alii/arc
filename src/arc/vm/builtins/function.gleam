import arc/vm/builtins/common.{type BuiltinType, alloc_proto}
import arc/vm/heap.{type Heap}
import arc/vm/value.{
  type Ref, Dispatch, FunctionApply, FunctionBind, FunctionCall,
  FunctionConstructor, FunctionToString, VmNative,
}
import gleam/dict
import gleam/list
import gleam/option.{Some}

/// Set up Function.prototype and Function constructor.
pub fn init(h: Heap(ctx), object_proto: Ref) -> #(Heap(ctx), BuiltinType) {
  // Allocate func_proto first (empty) so call/apply/bind can reference it
  // as their [[Prototype]] from the start — no fix-up needed.
  let #(h, func_proto) = alloc_proto(h, Some(object_proto), dict.new())

  // Allocate methods with the real func_proto as their prototype
  let #(h, proto_methods) =
    common.alloc_call_methods(h, func_proto, [
      #("call", FunctionCall, 1),
      #("apply", FunctionApply, 2),
      #("bind", FunctionBind, 1),
    ])
  let #(h, to_string_methods) =
    common.alloc_methods(h, func_proto, [
      #("toString", VmNative(FunctionToString), 0),
    ])

  // Constructor's [[Prototype]] is also func_proto (self-referencing bootstrap)
  common.init_type_on(
    h,
    func_proto,
    func_proto,
    list.append(proto_methods, to_string_methods),
    fn(_) { Dispatch(VmNative(FunctionConstructor)) },
    "Function",
    1,
    [],
  )
}
