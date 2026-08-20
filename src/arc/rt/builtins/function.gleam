//// `rt_builtins/function` — Function.prototype + Function constructor +
//// %ThrowTypeError% (SPEC §7.M6 builtins-object-function-error).
////
//// Init + the Function-native dispatch arms (`call`/`apply`/`bind`/
//// `toString`/`Symbol.hasInstance`, the `Function` constructor) over the
//// threaded `Agent` model with D7 raise semantics (`t_throw`).
////
//// **Return-tuple order is `#(V, St')` — value FIRST (R1).**

import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/ops as rt_ops
import arc/rt/realm as rt_realm
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type FunctionNative, type Handle, type JsVal,
  DataProperty, DynamicFunction, FunctionApply, FunctionBind, FunctionCall,
  FunctionConstructor, FunctionHasInstance, FunctionN, FunctionPrototypeCall,
  FunctionToString, JInt, KBound, KBytecode, KCompiled, KHandle, KNative, KNull,
  KStr, KUndef, Named, NoElements, ProxyObj, SObject, StringKey,
  ThrowTypeErrorFn, classify, mk_bool, mk_number, mk_object, mk_string,
  mk_undefined,
} as rt_types
import arc/rt/val as rt_val
import gleam/dict
import gleam/list
import gleam/option.{Some}
import gleam/string

/// Set up Function.prototype and Function constructor. Also allocates
/// %ThrowTypeError% (§10.2.4.1) and hands its Handle back to the caller: it
/// is an intrinsic in its own right, referenced by the unmapped arguments
/// object's `callee` and by the restricted `caller`/`arguments` accessors
/// installed here.
///
/// Returns `#(#(BuiltinPair, throw_type_error_h), st)`.
pub fn init(
  st: Agent,
  object_proto: Handle,
  realm: Int,
) -> #(#(BuiltinPair, Handle), Agent) {
  // Allocate func_proto first (empty) so call/apply/bind can reference it as
  // their [[Prototype]] from the start — no fix-up needed.
  let #(func_proto, st) = common.alloc_proto(st, Some(object_proto), dict.new())
  // Allocate methods with the real func_proto as their prototype.
  let #(proto_methods, st) =
    common.alloc_methods(st, func_proto, [
      #("call", FunctionN(FunctionCall), 1),
      #("apply", FunctionN(FunctionApply), 2),
      #("bind", FunctionN(FunctionBind), 1),
      #("toString", FunctionN(FunctionToString), 0),
    ])
  // §10.2.4.1: %ThrowTypeError% is unique — [[Extensible]] is false and its
  // "length"/"name" are {W:F, E:F, C:F}, so the function is frozen.
  let #(len_p, st) = common.data_prop(st, mk_number(JInt(0)))
  let #(name_p, st) = common.data_prop(st, mk_string(""))
  let #(thrower_h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: KNative(
          tag: FunctionN(ThrowTypeErrorFn),
          name: "",
          length: 0,
          constructible: False,
        ),
        proto: Some(func_proto),
        props: common.named_props([#("length", len_p), #("name", name_p)]),
        symbol_props: [],
        elements: NoElements,
        extensible: False,
      ),
    )
  let st = rt_store.t_pin_root(st, thrower_h)
  // §10.2.4 AddRestrictedFunctionProperties: "caller" and "arguments" on
  // Function.prototype are accessors whose get AND set are the single
  // %ThrowTypeError% intrinsic — same function identity for all four slots,
  // {E:F, C:T}.
  let #(restricted, st) =
    common.accessor_prop(
      st,
      get: Some(mk_object(thrower_h)),
      set: Some(mk_object(thrower_h)),
      enumerable: False,
      configurable: True,
    )
  // "caller" defined first (§10.2.4), so "arguments" gets the later seq.
  let #(restricted2, st) = common.restamp(st, restricted)
  let restricted_props = [
    #("caller", restricted),
    #("arguments", restricted2),
  ]
  // §20.2.3.6 Function.prototype[@@hasInstance] — {W:F, E:F, C:F}.
  let #(has_instance_h, st) =
    common.alloc_rooted_native_fn(
      st,
      func_proto,
      FunctionN(FunctionHasInstance),
      "[Symbol.hasInstance]",
      1,
    )
  let #(has_instance_prop, st) = common.data_prop(st, mk_object(has_instance_h))
  let st =
    common.add_symbol_property(
      st,
      func_proto,
      rt_types.symbol_has_instance,
      has_instance_prop,
    )
  // §20.2.3: Function.prototype has own "length" (0) and "name" ("").
  let #(proto_len, st) = common.fn_length_property(st, 0)
  let #(proto_name, st) = common.fn_name_property(st, "")
  // Constructor's [[Prototype]] is also func_proto (self-referencing bootstrap).
  let #(bt, st) =
    common.init_type_on(
      st,
      func_proto,
      func_proto,
      list.flatten([
        proto_methods,
        restricted_props,
        [#("length", proto_len), #("name", proto_name)],
      ]),
      fn(_) { FunctionN(FunctionConstructor(realm:)) },
      "Function",
      1,
      [],
      True,
    )
  // §20.2.3: Function.prototype is itself a built-in function object that
  // returns undefined when invoked. Flip its slot kind from Ordinary to
  // KNative(FunctionPrototypeCall).
  let st =
    rt_store.t_cell_update(st, func_proto, fn(slot) {
      case slot {
        SObject(..) as slot ->
          SObject(
            ..slot,
            kind: KNative(
              tag: FunctionN(FunctionPrototypeCall),
              name: "",
              length: 0,
              constructible: False,
            ),
          )
        other -> other
      }
    })
  #(#(bt, thrower_h), st)
}

/// Per-module dispatch for Function native functions. D7: an abrupt completion
/// RAISES via `t_throw`; the return is always the normal result value.
pub fn dispatch(
  st: Agent,
  native: FunctionNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    // §20.2.3.3 Function.prototype.call(thisArg, ...args)
    FunctionCall -> {
      let #(this_arg, call_args) = case args {
        [t, ..rest] -> #(t, rest)
        [] -> #(mk_undefined(), [])
      }
      rt_call.t_call_checked(st, this, this_arg, call_args)
    }
    // §20.2.3.1 Function.prototype.apply(thisArg, argArray)
    FunctionApply -> {
      let #(this_arg, arg_array) = helpers.two_args_or_undefined(args)
      let #(call_args, st) = case classify(arg_array) {
        // Step 3: undefined/null argArray → no args.
        KUndef | KNull -> #([], st)
        // Step 4: ? CreateListFromArrayLike(argArray).
        _ -> create_list_from_array_like(st, arg_array)
      }
      rt_call.t_call_checked(st, this, this_arg, call_args)
    }
    // §20.2.3.2 Function.prototype.bind(thisArg, ...args)
    FunctionBind -> {
      let #(this_arg, bound_args) = case args {
        [t, ..rest] -> #(t, rest)
        [] -> #(mk_undefined(), [])
      }
      // Step 2: If IsCallable(Target) is false, throw a TypeError.
      case rt_call.is_callable(st, this), classify(this) {
        True, KHandle(target_h) -> {
          // Steps 3-10 delegate to t_bound_new (rt_call.gleam:777-828).
          let #(h, st) = rt_call.t_bound_new(st, target_h, this_arg, bound_args)
          #(mk_object(h), st)
        }
        _, _ ->
          rt_val.t_throw_type_error(st, "Bind must be called on a function")
      }
    }
    // §20.2.3.5 Function.prototype.toString
    FunctionToString -> function_to_string(st, this)
    // §20.2.3.6 Function.prototype[@@hasInstance](V)
    FunctionHasInstance -> {
      let v = helpers.first_arg_or_undefined(args)
      // OrdinaryHasInstance step 1: If IsCallable(C) is false, return false.
      case classify(this) {
        KHandle(h) ->
          case rt_call.is_callable(st, this) {
            True -> {
              // rt_ops returns a WASM i32 truth value (0/1).
              let #(b, st) = rt_ops.t_ordinary_has_instance(st, h, v)
              #(mk_bool(b != 0), st)
            }
            False -> #(mk_bool(False), st)
          }
        _ -> #(mk_bool(False), st)
      }
    }
    // §10.2.4.1 %ThrowTypeError% — restricted "caller"/"arguments" accessor.
    ThrowTypeErrorFn -> restricted_function_property(st, this)
    // §20.2.3 calling Function.prototype itself returns undefined.
    FunctionPrototypeCall -> #(mk_undefined(), st)
    // §20.2.1.1 Function ( ...parameterArgs, bodyArg ) under [[Call]]:
    // NewTarget is undefined.
    FunctionConstructor(realm:) ->
      create_dynamic_function(st, realm, args, DynamicNormal, mk_undefined())
  }
}

/// [[Construct]] of %Function%: §20.2.1.1 with the original `new.target`.
pub fn dispatch_construct(
  st: Agent,
  native: FunctionNative,
  args: List(JsVal),
  new_target: JsVal,
) -> #(JsVal, Agent) {
  case native {
    FunctionConstructor(realm:) ->
      create_dynamic_function(st, realm, args, DynamicNormal, new_target)
    FunctionCall
    | FunctionApply
    | FunctionBind
    | FunctionToString
    | FunctionHasInstance
    | ThrowTypeErrorFn
    | FunctionPrototypeCall ->
      rt_val.t_throw_type_error(st, "not a constructor")
  }
}

/// §20.2.1.1.1 CreateDynamicFunction's `kind`.
pub type DynamicFunctionKind {
  DynamicNormal
  DynamicGenerator
  DynamicAsync
  DynamicAsyncGenerator
}

/// §20.2.1.1.1 CreateDynamicFunction(constructor, newTarget, kind, args),
/// shared by Function / GeneratorFunction / AsyncFunction /
/// AsyncGeneratorFunction. The last argument is the body, the ones before it
/// are parameter sources; all are ToString'd in order (steps 8-10) before
/// anything is parsed.
///
/// Step 16 assembles "function anonymous(" P "\n) {\n" body "\n}", but the
/// spec then calls OrdinaryFunctionCreate directly, so unlike a named
/// function expression there is NO self-name binding: `anonymous` must not
/// resolve inside the body (test262 staging/sm/Function/constructor-binding).
/// The source handed to the eval hook is therefore an ANONYMOUS function
/// expression, and step 29 SetFunctionName(F, "anonymous") is applied to the
/// result. The newline before ")" matters: a trailing line comment in the
/// last parameter must not swallow the ")" (test262
/// Function/prototype/toString/Function).
///
/// Steps 18-19: the closure's [[Prototype]] is
/// GetPrototypeFromConstructor(newTarget, fallbackProto). With NewTarget
/// undefined, newTarget is the constructor itself, whose `prototype` is the
/// intrinsic the eval hook already used; otherwise it is applied here.
///
/// The whole operation runs with the constructor's realm `realm` current
/// (§10.3.1 steps 6-7): the argument coercions, the parse, and the closure,
/// whose [[Realm]] and default [[Prototype]] are that realm's.
pub fn create_dynamic_function(
  st: Agent,
  realm: Int,
  args: List(JsVal),
  kind: DynamicFunctionKind,
  new_target: JsVal,
) -> #(JsVal, Agent) {
  use st <- rt_realm.with_realm(st, realm)
  let #(strs, st) =
    list.fold(args, #([], st), fn(acc, arg) {
      let #(done, st) = acc
      let #(s, st) = rt_val.t_to_string(st, arg)
      #([s, ..done], st)
    })
  let #(params, body) = case strs {
    [] -> #([], "")
    [b, ..params_rev] -> #(list.reverse(params_rev), b)
  }
  let keyword = case kind {
    DynamicNormal -> "function"
    DynamicGenerator -> "function*"
    DynamicAsync -> "async function"
    DynamicAsyncGenerator -> "async function*"
  }
  let source =
    "("
    <> keyword
    <> "("
    <> string.join(params, ",")
    <> "\n) {\n"
    <> body
    <> "\n})"
  let #(f, st) = st.store.ops.eval_hook(st, source, DynamicFunction)
  case classify(f) {
    KHandle(h) -> {
      let #(_, st) =
        rt_obj.t_define_own_data(
          st,
          h,
          StringKey(Named("name")),
          mk_string("anonymous"),
          False,
          False,
          True,
        )
      #(f, apply_new_target_prototype(st, h, kind, new_target))
    }
    _ -> #(f, st)
  }
}

/// §20.2.1.1.1 steps 18-19 for an object `new_target`: set the closure's
/// [[Prototype]] to `? Get(newTarget, "prototype")` when that is an object,
/// else to the `kind` intrinsic of newTarget's realm (§10.1.14
/// GetPrototypeFromConstructor via §7.3.24 GetFunctionRealm).
fn apply_new_target_prototype(
  st: Agent,
  h: Handle,
  kind: DynamicFunctionKind,
  new_target: JsVal,
) -> Agent {
  case classify(new_target) {
    KHandle(_) -> {
      let #(proto, st) =
        rt_call.get_prototype_from_constructor(st, new_target, fn(realm) {
          case kind {
            DynamicNormal -> realm.function.prototype
            DynamicGenerator -> realm.generator_fn.prototype
            DynamicAsync -> realm.async_fn.prototype
            DynamicAsyncGenerator ->
              rt_call.async_generator_fn_prototype(st, realm)
          }
        })
      // A fresh, extensible ordinary function cell: OrdinarySetPrototypeOf
      // cannot reject it.
      let #(_set, st) = rt_obj.t_set_prototype(st, h, Some(proto))
      st
    }
    _ -> st
  }
}

/// §7.3.19 CreateListFromArrayLike(obj) — used by Function.prototype.apply
/// and Reflect.apply/construct. Elements are read via `[[Get]]` for indices
/// [0, ToLength(Get(obj, "length"))).
pub fn create_list_from_array_like(
  st: Agent,
  arr: JsVal,
) -> #(List(JsVal), Agent) {
  case classify(arr) {
    KHandle(h) -> {
      let #(len, st) = case rt_store.t_cell_get(st, h) {
        SObject(kind: rt_types.ArrayObj(length:), ..) -> #(length, st)
        _ -> {
          let #(len_v, st) =
            rt_obj.t_get_prop(st, arr, StringKey(Named("length")))
          rt_val.t_to_length(st, len_v)
        }
      }
      collect_array_like(st, arr, 0, len, [])
    }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "CreateListFromArrayLike called on non-object",
      )
  }
}

fn collect_array_like(
  st: Agent,
  arr: JsVal,
  i: Int,
  len: Int,
  acc: List(JsVal),
) -> #(List(JsVal), Agent) {
  case i >= len {
    True -> #(list.reverse(acc), st)
    False -> {
      let #(v, st) = case helpers.own_element(st, arr, i) {
        helpers.Hit(v) -> #(v, st)
        helpers.Slow ->
          rt_obj.t_get_prop(st, arr, StringKey(rt_types.index_key(i)))
      }
      collect_array_like(st, arr, i + 1, len, [v, ..acc])
    }
  }
}

/// §20.2.3.5 Function.prototype.toString. `"function NAME() { [native code] }"`
/// for native/user functions and callable proxies; TypeError for non-callable.
fn function_to_string(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  case classify(this) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: KCompiled(..), props:, ..)
        | SObject(kind: KBytecode(..), props:, ..)
        | SObject(kind: KNative(..), props:, ..) -> {
          let name = case dict.get(props, Named("name")) {
            Ok(DataProperty(value: v, ..)) ->
              case classify(v) {
                KStr(n) -> n
                _ -> ""
              }
            _ -> ""
          }
          #(mk_string("function " <> name <> "() { [native code] }"), st)
        }
        // §20.2.3.5 step 3: bound functions get an implementation-defined
        // NativeFunction string. Like V8, omit the "bound f" name.
        SObject(kind: KBound(..), ..) -> #(
          mk_string("function () { [native code] }"),
          st,
        )
        // §20.2.3.5 step 4: any other object with [[Call]] (callable proxies).
        SObject(kind: ProxyObj(target:, ..), ..) ->
          case rt_call.is_callable(st, mk_object(target)) {
            True -> #(mk_string("function () { [native code] }"), st)
            False -> to_string_type_error(st)
          }
        _ -> to_string_type_error(st)
      }
    _ -> to_string_type_error(st)
  }
}

fn to_string_type_error(st: Agent) -> a {
  rt_val.t_throw_type_error(
    st,
    "Function.prototype.toString requires that 'this' be a Function",
  )
}

/// §10.2.4.1 %ThrowTypeError%, with the V8/JSC legacy relaxation: reading
/// "caller"/"arguments" through Function.prototype's restricted accessor on
/// a NON-strict plain function yields undefined instead of throwing (V8
/// returns null, JSC undefined; test262's features:[caller] tests accept
/// undefined). Everything else still throws: strict functions, class
/// constructors (always strict), arrows / generators / async functions /
/// methods (not constructors), bound and native functions.
fn restricted_function_property(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let is_legacy = case classify(this) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: KCompiled(flags:, ..), ..)
        | SObject(kind: KBytecode(flags:, ..), ..) ->
          flags.is_constructor && !flags.is_strict
        _ -> False
      }
    _ -> False
  }
  case is_legacy {
    True -> #(mk_undefined(), st)
    False ->
      rt_val.t_throw_type_error(
        st,
        "'caller', 'callee', and 'arguments' properties may not be "
          <> "accessed on strict mode functions or the arguments objects "
          <> "for calls to them",
      )
  }
}
