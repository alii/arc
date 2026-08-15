//// The `Reflect` global namespace (ES2024 §28.1).
////
//// Faithful port of arc/vm/builtins/reflect.gleam over 2core's threaded
//// Agent. Return-tuple order is `#(JsVal, Agent)` (R1).
//// Unlike Object.*, every Reflect method throws TypeError on a non-object
//// target (never coerces) and returns Bool where Object.* would throw.

import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/builtins/realm_ops
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type ReflectNative, KHandle, KNull, Named,
  ReflectApply, ReflectConstruct, ReflectDefineProperty, ReflectDeleteProperty,
  ReflectGet, ReflectGetOwnPropertyDescriptor, ReflectGetPrototypeOf, ReflectHas,
  ReflectIsExtensible, ReflectN, ReflectOwnKeys, ReflectPreventExtensions,
  ReflectSet, ReflectSetPrototypeOf, StringKey, classify, mk_bool, mk_null,
  mk_object, mk_undefined,
} as rt_types
import arc/rt/val as rt_val
import gleam/list
import gleam/option.{None, Some}

// ============================================================================
// Init — set up the Reflect global object
// ============================================================================

/// Set up the Reflect global object.
/// Reflect is NOT a constructor — it's a plain object with static methods
/// (like Math/JSON), per ES2024 §28.1.
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

// ============================================================================
// Dispatch
// ============================================================================

/// Per-module dispatch for Reflect native functions.
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

// ============================================================================
// Implementations
// ============================================================================

/// Helper: require the first argument be an Object handle, else TypeError.
/// All Reflect methods share this check per §28.1 — unlike Object.*, they
/// never coerce and always throw on non-object target.
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

/// Reflect.apply ( target, thisArgument, argumentsList ) — ES2024 §28.1.1
fn reflect_apply(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  let #(target, this_arg, args_list) = helpers.three_args_or_undefined(args)
  // Step 1: If IsCallable(target) is false, throw a TypeError.
  case rt_call.is_callable(st, target) {
    False ->
      rt_val.t_throw_type_error(st, "Reflect.apply: target is not a function")
    True -> {
      // Step 2: Let args be ? CreateListFromArrayLike(argumentsList).
      let #(call_args, st) = create_list_from_array_like(st, args_list)
      // Step 4: Return ? Call(target, thisArgument, args).
      rt_call.t_call_checked(st, target, this_arg, call_args)
    }
  }
}

/// Reflect.construct ( target, argumentsList [ , newTarget ] ) — ES2024 §28.1.2
fn reflect_construct(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  let #(target, args_list, new_target) = case args {
    [t, a, nt, ..] -> #(t, a, nt)
    [t, a] -> #(t, a, t)
    [t] -> #(t, mk_undefined(), t)
    [] -> #(mk_undefined(), mk_undefined(), mk_undefined())
  }
  // Step 1: If IsConstructor(target) is false, throw a TypeError.
  case rt_call.is_constructor(st, target) {
    False ->
      rt_val.t_throw_type_error(
        st,
        "Reflect.construct: target is not a constructor",
      )
    True ->
      // Step 3: If newTarget is not a constructor, throw a TypeError.
      case rt_call.is_constructor(st, new_target) {
        False ->
          rt_val.t_throw_type_error(
            st,
            "Reflect.construct: newTarget is not a constructor",
          )
        True -> {
          // Step 4: Let args be ? CreateListFromArrayLike(argumentsList).
          let #(ctor_args, st) = create_list_from_array_like(st, args_list)
          // Step 5: Return ? Construct(target, args, newTarget).
          let #(h, st) = rt_call.t_construct(st, target, ctor_args, new_target)
          #(mk_object(h), st)
        }
      }
  }
}

/// Reflect.defineProperty ( target, propertyKey, attributes ) — ES2024 §28.1.3
///
/// Unlike Object.defineProperty, returns Bool instead of throwing on failure.
fn reflect_define_property(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use h, rest, st <- require_object_target(args, st, "defineProperty")
  let #(key_val, desc_val) = helpers.two_args_or_undefined(rest)
  // Step 2: Let key be ? ToPropertyKey(propertyKey).
  let #(pk, st) = rt_val.t_to_property_key(st, key_val)
  // Step 3: Let desc be ? ToPropertyDescriptor(attributes).
  let #(desc, st) = rt_obj.t_to_property_descriptor(st, desc_val)
  // Step 4: Return ? target.[[DefineOwnProperty]](key, desc).
  let #(ok, st) = rt_obj.t_define_own_prop(st, h, pk, desc)
  #(mk_bool(ok), st)
}

/// Reflect.deleteProperty ( target, propertyKey ) — ES2024 §28.1.4
fn reflect_delete_property(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use h, rest, st <- require_object_target(args, st, "deleteProperty")
  let key_val = helpers.first_arg_or_undefined(rest)
  // Step 2: Let key be ? ToPropertyKey(propertyKey).
  let #(pk, st) = rt_val.t_to_property_key(st, key_val)
  // Step 3: Return ? target.[[Delete]](key).
  let #(ok, st) = rt_obj.t_delete_prop(st, h, pk)
  #(mk_bool(ok), st)
}

/// Reflect.get ( target, propertyKey [ , receiver ] ) — ES2024 §28.1.5
fn reflect_get(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use h, rest, st <- require_object_target(args, st, "get")
  let #(key_val, receiver) = case rest {
    [k, r, ..] -> #(k, r)
    [k] -> #(k, mk_object(h))
    [] -> #(mk_undefined(), mk_object(h))
  }
  // Step 2: Let key be ? ToPropertyKey(propertyKey).
  let #(pk, st) = rt_val.t_to_property_key(st, key_val)
  // Step 4: Return ? target.[[Get]](key, receiver).
  rt_obj.t_get_prop_with_receiver(st, h, pk, receiver)
}

/// Reflect.getOwnPropertyDescriptor ( target, propertyKey ) — ES2024 §28.1.6
fn reflect_get_own_property_descriptor(
  args: List(JsVal),
  st: Agent,
) -> #(JsVal, Agent) {
  use h, rest, st <- require_object_target(args, st, "getOwnPropertyDescriptor")
  let key_val = helpers.first_arg_or_undefined(rest)
  // Step 2: Let key be ? ToPropertyKey(propertyKey).
  let #(pk, st) = rt_val.t_to_property_key(st, key_val)
  // Step 3: Let desc be ? target.[[GetOwnProperty]](key).
  let #(desc, st) = rt_obj.t_get_own_property(st, h, pk)
  case desc {
    // Step 4: FromPropertyDescriptor(desc).
    Some(prop) -> {
      let #(dh, st) =
        rt_obj.t_from_property_descriptor(st, rt_obj.parsed_of_property(prop))
      #(mk_object(dh), st)
    }
    None -> #(mk_undefined(), st)
  }
}

/// Reflect.getPrototypeOf ( target ) — ES2024 §28.1.7
fn reflect_get_prototype_of(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use h, _rest, st <- require_object_target(args, st, "getPrototypeOf")
  let #(proto, st) = rt_obj.t_get_prototype_of(st, h)
  case proto {
    Some(p) -> #(mk_object(p), st)
    None -> #(mk_null(), st)
  }
}

/// Reflect.has ( target, propertyKey ) — ES2024 §28.1.8
fn reflect_has(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use h, rest, st <- require_object_target(args, st, "has")
  let key_val = helpers.first_arg_or_undefined(rest)
  // Step 2: Let key be ? ToPropertyKey(propertyKey).
  let #(pk, st) = rt_val.t_to_property_key(st, key_val)
  // Step 3: Return ? target.[[HasProperty]](key).
  let #(found, st) = rt_obj.t_has_prop(st, mk_object(h), pk)
  #(mk_bool(found), st)
}

/// Reflect.isExtensible ( target ) — ES2024 §28.1.9
fn reflect_is_extensible(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use h, _rest, st <- require_object_target(args, st, "isExtensible")
  let #(extensible, st) = rt_obj.t_is_extensible(st, h)
  #(mk_bool(extensible), st)
}

/// Reflect.ownKeys ( target ) — ES2024 §28.1.10
fn reflect_own_keys(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use h, _rest, st <- require_object_target(args, st, "ownKeys")
  let #(keys, st) = rt_obj.t_own_keys(st, h)
  let #(arr, st) =
    realm_ops.alloc_array(st, list.map(keys, rt_obj.object_key_value))
  #(mk_object(arr), st)
}

/// Reflect.preventExtensions ( target ) — ES2024 §28.1.11
fn reflect_prevent_extensions(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use h, _rest, st <- require_object_target(args, st, "preventExtensions")
  let #(ok, st) = rt_obj.t_prevent_extensions(st, h)
  #(mk_bool(ok), st)
}

/// Reflect.set ( target, propertyKey, V [ , receiver ] ) — ES2024 §28.1.12
fn reflect_set(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use h, rest, st <- require_object_target(args, st, "set")
  let #(key_val, val, receiver) = case rest {
    [k, v, r, ..] -> #(k, v, r)
    [k, v] -> #(k, v, mk_object(h))
    [k] -> #(k, mk_undefined(), mk_object(h))
    [] -> #(mk_undefined(), mk_undefined(), mk_object(h))
  }
  // Step 2: Let key be ? ToPropertyKey(propertyKey).
  let #(pk, st) = rt_val.t_to_property_key(st, key_val)
  // Step 4: Return ? target.[[Set]](key, V, receiver).
  let #(ok, st) = rt_obj.t_set_prop_with_receiver(st, h, pk, val, receiver)
  #(mk_bool(ok), st)
}

/// Reflect.setPrototypeOf ( target, proto ) — ES2024 §28.1.13
fn reflect_set_prototype_of(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use h, rest, st <- require_object_target(args, st, "setPrototypeOf")
  let proto_val = helpers.first_arg_or_undefined(rest)
  // Step 2: If proto is not an Object and proto is not null, throw a TypeError.
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
    // Step 3: ? target.[[SetPrototypeOf]](proto).
    Ok(new_proto) -> {
      let #(ok, st) = rt_obj.t_set_prototype(st, h, new_proto)
      #(mk_bool(ok), st)
    }
  }
}

// ── inline §7.3 helpers not yet on rt_obj ────────────────────────────────

/// §7.3.19 CreateListFromArrayLike — step 1 throws TypeError for ANY
/// non-Object (arc property.gleam matches only `JsObject(ref)`).
fn create_list_from_array_like(st: Agent, obj: JsVal) -> #(List(JsVal), Agent) {
  case classify(obj) {
    KHandle(_) -> {
      let #(len_v, st) = rt_obj.t_get_prop(st, obj, StringKey(Named("length")))
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
      let #(v, st) =
        rt_obj.t_get_prop(st, obj, StringKey(rt_types.index_key(i)))
      collect_indexed(st, obj, i + 1, len, [v, ..acc])
    }
  }
}
