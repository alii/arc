import arc/bytecode/key.{Named, index_key}
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
  KStr, KUndef, NoElements, ProxyObj, SObject, StringKey, ThrowTypeErrorFn,
  classify, mk_bool, mk_number, mk_object, mk_string, mk_undefined,
} as rt_types
import arc/rt/val as rt_val
import gleam/dict
import gleam/list
import gleam/option.{Some}
import gleam/string

pub fn init(
  st: Agent,
  object_proto: Handle,
  realm: Int,
) -> #(#(BuiltinPair, Handle), Agent) {
  let #(func_proto, st) = common.alloc_proto(st, Some(object_proto), dict.new())
  let #(proto_methods, st) =
    common.alloc_methods(st, func_proto, [
      #("call", FunctionN(FunctionCall), 1),
      #("apply", FunctionN(FunctionApply), 2),
      #("bind", FunctionN(FunctionBind), 1),
      #("toString", FunctionN(FunctionToString), 0),
    ])
  // §10.2.4.1 %ThrowTypeError%, frozen
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
  // §10.2.4 caller and arguments share the one thrower
  let #(restricted, st) =
    common.accessor_prop(
      st,
      get: Some(mk_object(thrower_h)),
      set: Some(mk_object(thrower_h)),
      enumerable: False,
      configurable: True,
    )
  let #(restricted2, st) = common.restamp(st, restricted)
  let restricted_props = [
    #("caller", restricted),
    #("arguments", restricted2),
  ]
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
  let #(proto_len, st) = common.fn_length_property(st, 0)
  let #(proto_name, st) = common.fn_name_property(st, "")
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
  // function.prototype is itself callable, returns undefined
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

pub fn dispatch(
  st: Agent,
  native: FunctionNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    FunctionCall -> {
      let #(this_arg, call_args) = case args {
        [t, ..rest] -> #(t, rest)
        [] -> #(mk_undefined(), [])
      }
      rt_call.t_call_checked(st, this, this_arg, call_args)
    }
    FunctionApply -> {
      let #(this_arg, arg_array) = helpers.two_args_or_undefined(args)
      let #(call_args, st) = case classify(arg_array) {
        KUndef | KNull -> #([], st)
        _ -> create_list_from_array_like(st, arg_array)
      }
      rt_call.t_call_checked(st, this, this_arg, call_args)
    }
    FunctionBind -> {
      let #(this_arg, bound_args) = case args {
        [t, ..rest] -> #(t, rest)
        [] -> #(mk_undefined(), [])
      }
      case rt_call.is_callable(st, this), classify(this) {
        True, KHandle(target_h) -> {
          let #(h, st) = rt_call.t_bound_new(st, target_h, this_arg, bound_args)
          #(mk_object(h), st)
        }
        _, _ ->
          rt_val.t_throw_type_error(st, "Bind must be called on a function")
      }
    }
    FunctionToString -> function_to_string(st, this)
    FunctionHasInstance -> {
      let v = helpers.first_arg_or_undefined(args)
      case classify(this) {
        KHandle(h) ->
          case rt_call.is_callable(st, this) {
            True -> {
              let #(b, st) = rt_ops.t_ordinary_has_instance(st, h, v)
              #(mk_bool(b != 0), st)
            }
            False -> #(mk_bool(False), st)
          }
        _ -> #(mk_bool(False), st)
      }
    }
    ThrowTypeErrorFn -> restricted_function_property(st, this)
    FunctionPrototypeCall -> #(mk_undefined(), st)
    FunctionConstructor(realm:) ->
      create_dynamic_function(st, realm, args, DynamicNormal, mk_undefined())
  }
}

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

pub type DynamicFunctionKind {
  DynamicNormal
  DynamicGenerator
  DynamicAsync
  DynamicAsyncGenerator
}

// §20.2.1.1.1 createdynamicfunction
// anonymous expression: the body must not see a self name
// newline before ) so a param line comment can't eat it
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
      let #(_set, st) = rt_obj.t_set_prototype(st, h, Some(proto))
      st
    }
    _ -> st
  }
}

pub fn create_list_from_array_like(
  st: Agent,
  arr: JsVal,
) -> #(List(JsVal), Agent) {
  case arg_list(st, arr), classify(arr) {
    ArgsHit(args), _ -> #(args, st)
    ArgsSlow, KHandle(h) -> {
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
    ArgsSlow, _ ->
      rt_val.t_throw_type_error(
        st,
        "CreateListFromArrayLike called on non-object",
      )
  }
}

type ArgList {
  ArgsHit(List(JsVal))
  ArgsSlow
}

@external(erlang, "arc_rt_array_ffi", "arg_list")
fn arg_list(st: Agent, arr: JsVal) -> ArgList

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
        helpers.Slow -> rt_obj.t_get_prop(st, arr, StringKey(index_key(i)))
      }
      collect_array_like(st, arr, i + 1, len, [v, ..acc])
    }
  }
}

fn function_to_string(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  case classify(this) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: KCompiled(..), ..)
        | SObject(kind: KBytecode(..), ..)
        | SObject(kind: KNative(..), ..) -> {
          let name = case
            rt_obj.t_ordinary_own_property(st, h, StringKey(Named("name")))
          {
            Some(DataProperty(value: v, ..)) ->
              case classify(v) {
                KStr(n) -> n
                _ -> ""
              }
            _ -> ""
          }
          #(mk_string("function " <> name <> "() { [native code] }"), st)
        }
        SObject(kind: KBound(..), ..) -> #(
          mk_string("function () { [native code] }"),
          st,
        )
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

// sloppy plain functions get undefined, v8/jsc legacy
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
