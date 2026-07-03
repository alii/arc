import arc/vm/builtins/common.{type BuiltinType, alloc_proto}
import arc/vm/heap.{type Heap}
import arc/vm/internal/elements
import arc/vm/key.{Named}
import arc/vm/value.{
  type Ref, Dispatch, FunctionApply, FunctionBind, FunctionCall,
  FunctionConstructor, FunctionHasInstance, FunctionToString, JsObject,
  ThrowTypeErrorFn, VmNative,
}
import gleam/dict
import gleam/list
import gleam/option.{Some}

/// Set up Function.prototype and Function constructor.
///
/// Also allocates %ThrowTypeError% (§10.2.4.1) and hands its Ref back to the
/// caller: it is an intrinsic in its own right, referenced by the unmapped
/// arguments object's `callee` as well as by the restricted `caller`/
/// `arguments` accessors installed here, so it must not be sunk into the
/// heap and forgotten.
pub fn init(
  h: Heap(ctx, host),
  object_proto: Ref,
) -> #(Heap(ctx, host), BuiltinType, Ref) {
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

  // §10.2.4 AddRestrictedFunctionProperties: "caller" and "arguments" on
  // Function.prototype are accessors whose get AND set are the single
  // %ThrowTypeError% intrinsic (§10.2.4.1) — same function identity for
  // all four slots, non-enumerable, configurable. The native is
  // receiver-sensitive (V8/JSC legacy behavior): non-strict plain function
  // receivers get undefined instead of a throw — see
  // restricted_function_property in exec/call.gleam.
  //
  // §10.2.4.1: %ThrowTypeError% is unique among built-ins — [[Extensible]]
  // is false and its "length"/"name" properties are { W:F, E:F, C:F }, so
  // the function is frozen (test262: built-ins/ThrowTypeError/{extensible,
  // frozen,length,name}.js).
  let #(h, thrower_ref) =
    heap.alloc(
      h,
      value.ObjectSlot(
        kind: value.NativeFunction(
          Dispatch(VmNative(ThrowTypeErrorFn)),
          constructible: False,
        ),
        properties: dict.from_list([
          #(Named("length"), value.data(value.from_int(0))),
          #(Named("name"), value.data(value.JsString(""))),
        ]),
        elements: elements.new(),
        prototype: Some(func_proto),
        symbol_properties: [],
        extensible: False,
      ),
    )
  let h = heap.root(h, thrower_ref)
  let restricted =
    value.accessor(
      get: Some(JsObject(thrower_ref)),
      set: Some(JsObject(thrower_ref)),
      enumerable: False,
      configurable: True,
    )
  let restricted_props = [
    #("caller", restricted),
    #("arguments", restricted),
  ]

  // §20.2.3.6 Function.prototype [ @@hasInstance ] — {W:F, E:F, C:F}.
  let #(h, has_instance_ref) =
    common.alloc_native_fn(
      h,
      func_proto,
      VmNative(FunctionHasInstance),
      "[Symbol.hasInstance]",
      1,
    )
  let h =
    common.add_symbol_property(
      h,
      func_proto,
      value.symbol_has_instance,
      value.data(JsObject(has_instance_ref)),
    )

  // Constructor's [[Prototype]] is also func_proto (self-referencing bootstrap)
  let #(h, bt) =
    common.init_type_on(
      h,
      func_proto,
      func_proto,
      list.flatten([
        proto_methods,
        to_string_methods,
        restricted_props,
        // §20.2.3: Function.prototype has own "length" (0) and "name" ("")
        // properties like any function.
        [
          #("length", common.fn_length_property(0)),
          #("name", common.fn_name_property("")),
        ],
      ]),
      fn(_) { Dispatch(VmNative(FunctionConstructor)) },
      "Function",
      1,
      [],
      True,
    )

  // §20.2.3: "The Function prototype object … is itself a built-in function
  // object" that accepts any arguments and returns undefined when invoked.
  // Flip its slot kind from the plain-object bootstrap to a NativeFunction
  // (test262: built-ins/Function/prototype/S15.3.4_A1, S15.3.4_A2_T*).
  let h =
    heap.update(h, func_proto, fn(slot) {
      case slot {
        value.ObjectSlot(..) as slot ->
          value.ObjectSlot(
            ..slot,
            kind: value.NativeFunction(
              Dispatch(VmNative(value.FunctionPrototypeCall)),
              constructible: False,
            ),
          )
        other -> other
      }
    })

  #(h, bt, thrower_ref)
}
