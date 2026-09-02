import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/builtins/realm_ops
import arc/rt/call as rt_call
import arc/rt/name_keys as nk
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type ReflectNative, KHandle, KNull,
  ReflectApply, ReflectConstruct, ReflectDefineProperty, ReflectDeleteProperty,
  ReflectGet, ReflectGetOwnPropertyDescriptor, ReflectGetPrototypeOf, ReflectHas,
  ReflectIsExtensible, ReflectN, ReflectOwnKeys, ReflectPreventExtensions,
  ReflectSet, ReflectSetPrototypeOf, StringKey, classify, mk_bool, mk_null,
  mk_object, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/list
import gleam/option.{None, Some}

pub fn init(
  st: Agent,
  object_proto: Handle,
  function_proto: Handle,
) -> #(Handle, Agent) {
  let #(methods, st) =
    common.alloc_methods(st, function_proto, [
      #("apply", ReflectN(ReflectApply), 3),
      #("construct", ReflectN(ReflectConstruct), 2),
      #("defineProperty", ReflectN(ReflectDefineProperty), 3),
      #("deleteProperty", ReflectN(ReflectDeleteProperty), 2),
      #("get", ReflectN(ReflectGet), 2),
      #(
        "getOwnPropertyDescriptor",
        ReflectN(ReflectGetOwnPropertyDescriptor),
        2,
      ),
      #("getPrototypeOf", ReflectN(ReflectGetPrototypeOf), 1),
      #("has", ReflectN(ReflectHas), 2),
      #("isExtensible", ReflectN(ReflectIsExtensible), 1),
      #("ownKeys", ReflectN(ReflectOwnKeys), 1),
      #("preventExtensions", ReflectN(ReflectPreventExtensions), 1),
      #("set", ReflectN(ReflectSet), 3),
      #("setPrototypeOf", ReflectN(ReflectSetPrototypeOf), 2),
    ])

  common.init_namespace(st, object_proto, "Reflect", methods)
}

pub fn dispatch(
  st: Agent,
  native: ReflectNative,
  _this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    ReflectApply -> reflect_apply(args, st)
    ReflectConstruct -> reflect_construct(args, st)
    ReflectDefineProperty -> reflect_define_property(args, st)
    ReflectDeleteProperty -> reflect_delete_property(args, st)
    ReflectGet -> reflect_get(args, st)
    ReflectGetOwnPropertyDescriptor ->
      reflect_get_own_property_descriptor(args, st)
    ReflectGetPrototypeOf -> reflect_get_prototype_of(args, st)
    ReflectHas -> reflect_has(args, st)
    ReflectIsExtensible -> reflect_is_extensible(args, st)
    ReflectOwnKeys -> reflect_own_keys(args, st)
    ReflectPreventExtensions -> reflect_prevent_extensions(args, st)
    ReflectSet -> reflect_set(args, st)
    ReflectSetPrototypeOf -> reflect_set_prototype_of(args, st)
  }
}

fn require_object_target(
  args: List(JsVal),
  st: Agent,
  method: String,
  cont: fn(Handle, List(JsVal), Agent) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  case args {
    [first, ..rest] ->
      case classify(first) {
        KHandle(h) -> cont(h, rest, st)
        _ ->
          rt_val.t_throw_type_error(
            st,
            "Reflect." <> method <> " called on non-object",
          )
      }
    [] ->
      rt_val.t_throw_type_error(
        st,
        "Reflect." <> method <> " called on non-object",
      )
  }
}

fn reflect_apply(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  let #(target, this_arg, args_list) = helpers.three_args_or_undefined(args)
  case rt_call.is_callable(st, target) {
    False ->
      rt_val.t_throw_type_error(st, "Reflect.apply: target is not a function")
    True -> {
      let #(call_args, st) = create_list_from_array_like(st, args_list)
      rt_call.t_call_checked(st, target, this_arg, call_args)
    }
  }
}

fn reflect_construct(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  let #(target, args_list, new_target) = case args {
    [t, a, nt, ..] -> #(t, a, nt)
    [t, a] -> #(t, a, t)
    [t] -> #(t, mk_undefined(), t)
    [] -> #(mk_undefined(), mk_undefined(), mk_undefined())
  }
  case rt_call.is_constructor(st, target) {
    False ->
      rt_val.t_throw_type_error(
        st,
        "Reflect.construct: target is not a constructor",
      )
    True ->
      case rt_call.is_constructor(st, new_target) {
        False ->
          rt_val.t_throw_type_error(
            st,
            "Reflect.construct: newTarget is not a constructor",
          )
        True -> {
          let #(ctor_args, st) = create_list_from_array_like(st, args_list)
          let #(h, st) = rt_call.t_construct(st, target, ctor_args, new_target)
          #(mk_object(h), st)
        }
      }
  }
}

fn reflect_define_property(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use h, rest, st <- require_object_target(args, st, "defineProperty")
  let #(key_val, desc_val) = helpers.two_args_or_undefined(rest)
  let #(pk, st) = rt_val.t_to_property_key(st, key_val)
  let #(desc, st) = rt_obj.t_to_property_descriptor(st, desc_val)
  let #(ok, st) = rt_obj.t_define_own_prop(st, h, pk, desc)
  #(mk_bool(ok), st)
}

fn reflect_delete_property(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use h, rest, st <- require_object_target(args, st, "deleteProperty")
  let key_val = helpers.first_arg_or_undefined(rest)
  let #(pk, st) = rt_val.t_to_property_key(st, key_val)
  let #(ok, st) = rt_obj.t_delete_prop(st, h, pk)
  #(mk_bool(ok), st)
}

fn reflect_get(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use h, rest, st <- require_object_target(args, st, "get")
  let #(key_val, receiver) = case rest {
    [k, r, ..] -> #(k, r)
    [k] -> #(k, mk_object(h))
    [] -> #(mk_undefined(), mk_object(h))
  }
  case rt_val.t_find_property_key(st, key_val) {
    #(Ok(pk), st) -> rt_obj.t_get_prop_with_receiver(st, h, pk, receiver)
    #(Error(text), st) ->
      rt_obj.t_get_by_text_with_receiver(st, h, text, receiver)
  }
}

fn reflect_get_own_property_descriptor(
  args: List(JsVal),
  st: Agent,
) -> #(JsVal, Agent) {
  use h, rest, st <- require_object_target(args, st, "getOwnPropertyDescriptor")
  let key_val = helpers.first_arg_or_undefined(rest)
  let #(desc, st) = case rt_val.t_find_property_key(st, key_val) {
    #(Ok(pk), st) -> rt_obj.t_get_own_property(st, h, pk)
    #(Error(text), st) -> rt_obj.t_own_property_by_text(st, h, text)
  }
  case desc {
    Some(prop) -> {
      let #(dh, st) =
        rt_obj.t_from_property_descriptor(st, rt_obj.parsed_of_property(prop))
      #(mk_object(dh), st)
    }
    None -> #(mk_undefined(), st)
  }
}

fn reflect_get_prototype_of(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use h, _rest, st <- require_object_target(args, st, "getPrototypeOf")
  let #(proto, st) = rt_obj.t_get_prototype_of(st, h)
  case proto {
    Some(p) -> #(mk_object(p), st)
    None -> #(mk_null(), st)
  }
}

fn reflect_has(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use h, rest, st <- require_object_target(args, st, "has")
  let key_val = helpers.first_arg_or_undefined(rest)
  let #(found, st) = case rt_val.t_find_property_key(st, key_val) {
    #(Ok(pk), st) -> rt_obj.t_has_prop(st, mk_object(h), pk)
    #(Error(text), st) -> rt_obj.t_has_by_text(st, h, text)
  }
  #(mk_bool(found), st)
}

fn reflect_is_extensible(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use h, _rest, st <- require_object_target(args, st, "isExtensible")
  let #(extensible, st) = rt_obj.t_is_extensible(st, h)
  #(mk_bool(extensible), st)
}

fn reflect_own_keys(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use h, _rest, st <- require_object_target(args, st, "ownKeys")
  let #(keys, st) = rt_obj.t_own_keys(st, h)
  let #(arr, st) =
    realm_ops.alloc_array(st, list.map(keys, rt_obj.t_object_key_value(st, _)))
  #(mk_object(arr), st)
}

fn reflect_prevent_extensions(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use h, _rest, st <- require_object_target(args, st, "preventExtensions")
  let #(ok, st) = rt_obj.t_prevent_extensions(st, h)
  #(mk_bool(ok), st)
}

fn reflect_set(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use h, rest, st <- require_object_target(args, st, "set")
  let #(key_val, val, receiver) = case rest {
    [k, v, r, ..] -> #(k, v, r)
    [k, v] -> #(k, v, mk_object(h))
    [k] -> #(k, mk_undefined(), mk_object(h))
    [] -> #(mk_undefined(), mk_undefined(), mk_object(h))
  }
  let #(pk, st) = rt_val.t_to_property_key(st, key_val)
  let #(ok, st) = rt_obj.t_set_prop_with_receiver(st, h, pk, val, receiver)
  #(mk_bool(ok), st)
}

fn reflect_set_prototype_of(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use h, rest, st <- require_object_target(args, st, "setPrototypeOf")
  let proto_val = helpers.first_arg_or_undefined(rest)
  let new_proto = case classify(proto_val) {
    KHandle(p) -> Ok(Some(p))
    KNull -> Ok(None)
    _ -> Error(Nil)
  }
  case new_proto {
    Error(Nil) ->
      rt_val.t_throw_type_error(
        st,
        "Object prototype may only be an Object or null",
      )
    Ok(new_proto) -> {
      let #(ok, st) = rt_obj.t_set_prototype(st, h, new_proto)
      #(mk_bool(ok), st)
    }
  }
}

// §7.3.19, throws on any non-object
fn create_list_from_array_like(st: Agent, obj: JsVal) -> #(List(JsVal), Agent) {
  case classify(obj) {
    KHandle(_) -> {
      let #(len_v, st) = rt_obj.t_get_prop(st, obj, StringKey(nk.length))
      let #(len, st) = rt_val.t_to_length(st, len_v)
      collect_indexed(st, obj, 0, len, [])
    }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "CreateListFromArrayLike called on non-object",
      )
  }
}

fn collect_indexed(
  st: Agent,
  obj: JsVal,
  i: Int,
  len: Int,
  acc: List(JsVal),
) -> #(List(JsVal), Agent) {
  case i >= len {
    True -> #(list.reverse(acc), st)
    False -> {
      let #(v, st) = rt_obj.t_get_index(st, obj, i)
      collect_indexed(st, obj, i + 1, len, [v, ..acc])
    }
  }
}
