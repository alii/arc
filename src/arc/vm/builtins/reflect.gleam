import arc/vm/builtins/common
import arc/vm/builtins/helpers
import arc/vm/ops/define_own
import arc/vm/ops/object
import arc/vm/ops/property
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsValue, type Ref, type ReflectNativeFn, JsBool, JsNull, JsObject,
  JsUndefined, ReflectApply, ReflectConstruct, ReflectDefineProperty,
  ReflectDeleteProperty, ReflectGet, ReflectGetOwnPropertyDescriptor,
  ReflectGetPrototypeOf, ReflectHas, ReflectIsExtensible, ReflectNative,
  ReflectOwnKeys, ReflectPreventExtensions, ReflectSet, ReflectSetPrototypeOf,
}
import gleam/bool
import gleam/option.{None, Some}
import gleam/result

// ============================================================================
// Init — set up the Reflect global object
// ============================================================================

/// Set up the Reflect global object.
/// Reflect is NOT a constructor — it's a plain object with static methods
/// (like Math/JSON), per ES2024 §28.1.
pub fn init(
  h: Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(host), Ref) {
  let #(h, methods) =
    common.alloc_methods(h, function_proto, [
      #("apply", ReflectNative(ReflectApply), 3),
      #("construct", ReflectNative(ReflectConstruct), 2),
      #("defineProperty", ReflectNative(ReflectDefineProperty), 3),
      #("deleteProperty", ReflectNative(ReflectDeleteProperty), 2),
      #("get", ReflectNative(ReflectGet), 2),
      #(
        "getOwnPropertyDescriptor",
        ReflectNative(ReflectGetOwnPropertyDescriptor),
        2,
      ),
      #("getPrototypeOf", ReflectNative(ReflectGetPrototypeOf), 1),
      #("has", ReflectNative(ReflectHas), 2),
      #("isExtensible", ReflectNative(ReflectIsExtensible), 1),
      #("ownKeys", ReflectNative(ReflectOwnKeys), 1),
      #("preventExtensions", ReflectNative(ReflectPreventExtensions), 1),
      #("set", ReflectNative(ReflectSet), 3),
      #("setPrototypeOf", ReflectNative(ReflectSetPrototypeOf), 2),
    ])

  common.init_namespace(h, object_proto, "Reflect", methods)
}

// ============================================================================
// Dispatch
// ============================================================================

/// Per-module dispatch for Reflect native functions.
pub fn dispatch(
  native: ReflectNativeFn,
  args: List(JsValue),
  _this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case native {
    ReflectApply -> reflect_apply(args, state)
    ReflectConstruct -> reflect_construct(args, state)
    ReflectDefineProperty -> reflect_define_property(args, state)
    ReflectDeleteProperty -> reflect_delete_property(args, state)
    ReflectGet -> reflect_get(args, state)
    ReflectGetOwnPropertyDescriptor ->
      reflect_get_own_property_descriptor(args, state)
    ReflectGetPrototypeOf -> reflect_get_prototype_of(args, state)
    ReflectHas -> reflect_has(args, state)
    ReflectIsExtensible -> reflect_is_extensible(args, state)
    ReflectOwnKeys -> reflect_own_keys(args, state)
    ReflectPreventExtensions -> reflect_prevent_extensions(args, state)
    ReflectSet -> reflect_set(args, state)
    ReflectSetPrototypeOf -> reflect_set_prototype_of(args, state)
  }
}

// ============================================================================
// Implementations
// ============================================================================

/// Helper: require the first argument be a JsObject, else TypeError.
/// All Reflect methods share this check per §28.1 — unlike Object.*, they
/// never coerce and always throw on non-object target.
fn require_object(
  args: List(JsValue),
  state: State(host),
  method: String,
  cont: fn(Ref, List(JsValue), State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case args {
    [JsObject(ref), ..rest] -> cont(ref, rest, state)
    _ ->
      state.type_error(state, "Reflect." <> method <> " called on non-object")
  }
}

/// Reflect.apply ( target, thisArgument, argumentsList ) — ES2024 §28.1.1
///
///   1. If IsCallable(target) is false, throw a TypeError exception.
///   2. Let args be ? CreateListFromArrayLike(argumentsList).
///   3. Perform PrepareForTailCall().
///   4. Return ? Call(target, thisArgument, args).
fn reflect_apply(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(target, this_arg, args_list) = helpers.three_args_or_undefined(args)
  // Step 1: If IsCallable(target) is false, throw a TypeError.
  use <- bool.guard(
    !helpers.is_callable(state.heap, target),
    state.type_error(state, "Reflect.apply: target is not a function"),
  )
  // Step 2: Let args be ? CreateListFromArrayLike(argumentsList) — the real
  // §7.3.19: length/element reads go through [[Get]] (proxy traps, accessors).
  use call_args, state <- state.try_op(property.create_list_from_array_like(
    state,
    args_list,
  ))
  // Step 4: Return ? Call(target, thisArgument, args).
  use result, state <- state.try_call(state, target, this_arg, call_args)
  #(state, Ok(result))
}

/// Reflect.construct ( target, argumentsList [ , newTarget ] ) — ES2024 §28.1.2
///
///   1. If IsConstructor(target) is false, throw a TypeError exception.
///   2. If newTarget is not present, set newTarget to target.
///   3. Else if IsConstructor(newTarget) is false, throw a TypeError exception.
///   4. Let args be ? CreateListFromArrayLike(argumentsList).
///   5. Return ? Construct(target, args, newTarget).
fn reflect_construct(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(target, args_list, new_target) = case args {
    [t, a, nt, ..] -> #(t, a, nt)
    [t, a] -> #(t, a, t)
    [t] -> #(t, JsUndefined, t)
    [] -> #(JsUndefined, JsUndefined, JsUndefined)
  }
  // Step 1: If IsConstructor(target) is false, throw a TypeError exception.
  use <- bool.lazy_guard(!object.is_constructor(state.heap, target), fn() {
    state.type_error(state, "Reflect.construct: target is not a constructor")
  })
  // Step 3: If newTarget is not a constructor, throw a TypeError exception.
  // (When absent, new_target defaults to target — already validated above.)
  use <- bool.lazy_guard(!object.is_constructor(state.heap, new_target), fn() {
    state.type_error(state, "Reflect.construct: newTarget is not a constructor")
  })
  // Step 4: Let args be ? CreateListFromArrayLike(argumentsList).
  use ctor_args, state <- state.try_op(property.create_list_from_array_like(
    state,
    args_list,
  ))
  // Step 5: state.construct_with_target runs [[Construct]] with newTarget.
  use result, state <- state.try_op(state.construct_with_target(
    state,
    target,
    ctor_args,
    new_target,
  ))
  #(state, Ok(result))
}

/// Reflect.defineProperty ( target, propertyKey, attributes ) — ES2024 §28.1.3
///
///   1. If target is not an Object, throw a TypeError exception.
///   2. Let key be ? ToPropertyKey(propertyKey).
///   3. Let desc be ? ToPropertyDescriptor(attributes).
///   4. Return ? target.[[DefineOwnProperty]](key, desc).
///
/// Unlike Object.defineProperty, returns Bool instead of throwing on failure.
fn reflect_define_property(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, rest, state <- require_object(args, state, "defineProperty")
  let #(key_val, desc_val) = helpers.two_args_or_undefined(rest)
  // Steps 2-4: ToPropertyKey, THEN ToPropertyDescriptor (which is what raises
  // the "not an object" TypeError, only after the key's user code has run),
  // then [[DefineOwnProperty]]. define_property_bool_value returns the raw
  // [[DefineOwnProperty]] boolean; proxy trap exceptions propagate, ordinary
  // validation failures → false.
  case define_own.define_property_bool_value(state, ref, key_val, desc_val) {
    Ok(#(state, ok)) -> #(state, Ok(JsBool(ok)))
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// Reflect.deleteProperty ( target, propertyKey ) — ES2024 §28.1.4
///
///   1. If target is not an Object, throw a TypeError exception.
///   2. Let key be ? ToPropertyKey(propertyKey).
///   3. Return ? target.[[Delete]](key).
fn reflect_delete_property(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, rest, state <- require_object(args, state, "deleteProperty")
  let key_val = helpers.first_arg_or_undefined(rest)
  // Step 2: Let key be ? ToPropertyKey(propertyKey).
  use pk, state <- state.try_op(property.to_prop_key(state, key_val))
  // Step 3: Return ? target.[[Delete]](key) — trap-aware for proxies.
  unwrap_set(object.delete_property_stateful(state, ref, pk))
}

/// Reflect.get ( target, propertyKey [ , receiver ] ) — ES2024 §28.1.5
///
///   1. If target is not an Object, throw a TypeError exception.
///   2. Let key be ? ToPropertyKey(propertyKey).
///   3. If receiver is not present, set receiver to target.
///   4. Return ? target.[[Get]](key, receiver).
fn reflect_get(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, rest, state <- require_object(args, state, "get")
  let #(key_val, receiver) = case rest {
    [k, r, ..] -> #(k, r)
    [k] -> #(k, JsObject(ref))
    [] -> #(JsUndefined, JsObject(ref))
  }
  // Step 2: Let key be ? ToPropertyKey(propertyKey).
  use pk, state <- state.try_op(property.to_prop_key(state, key_val))
  // Step 4: Return ? target.[[Get]](key, receiver).
  use val, state <- state.try_op(object.get_prop_value(state, ref, pk, receiver))
  #(state, Ok(val))
}

/// Reflect.getOwnPropertyDescriptor ( target, propertyKey ) — ES2024 §28.1.6
///
///   1. If target is not an Object, throw a TypeError exception.
///   2. Let key be ? ToPropertyKey(propertyKey).
///   3. Let desc be ? target.[[GetOwnProperty]](key).
///   4. Return FromPropertyDescriptor(desc).
fn reflect_get_own_property_descriptor(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, rest, state <- require_object(
    args,
    state,
    "getOwnPropertyDescriptor",
  )
  let key_val = helpers.first_arg_or_undefined(rest)
  let object_proto = state.builtins.object.prototype
  // Step 2: Let key be ? ToPropertyKey(propertyKey). A "#x" string is an
  // ordinary property key here — private elements live in their own keyspace
  // and are filtered on the ordinary lookup path (never for proxy traps).
  use pk, state <- state.try_op(property.to_prop_key(state, key_val))
  // Step 3: Let desc be ? target.[[GetOwnProperty]](key) — trap-aware.
  use own_prop, state <- state.try_op(define_own.get_own_property_stateful(
    state,
    ref,
    object.prop_key_value(pk),
  ))
  // Steps 3-4: [[GetOwnProperty]] + FromPropertyDescriptor.
  case own_prop {
    Some(prop) -> {
      let #(heap, desc_ref) =
        define_own.make_descriptor_object(state.heap, prop, object_proto)
      #(State(..state, heap:), Ok(JsObject(desc_ref)))
    }
    None -> #(state, Ok(JsUndefined))
  }
}

/// Reflect.getPrototypeOf ( target ) — ES2024 §28.1.7
///
///   1. If target is not an Object, throw a TypeError exception.
///   2. Return ? target.[[GetPrototypeOf]]().
fn reflect_get_prototype_of(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, _rest, state <- require_object(args, state, "getPrototypeOf")
  // Step 2: trap-aware [[GetPrototypeOf]].
  use proto, state <- state.try_op(object.get_prototype_of_stateful(state, ref))
  #(state, Ok(proto))
}

/// Reflect.has ( target, propertyKey ) — ES2024 §28.1.8
///
///   1. If target is not an Object, throw a TypeError exception.
///   2. Let key be ? ToPropertyKey(propertyKey).
///   3. Return ? target.[[HasProperty]](key).
fn reflect_has(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, rest, state <- require_object(args, state, "has")
  let key_val = helpers.first_arg_or_undefined(rest)
  // Step 2: Let key be ? ToPropertyKey(propertyKey).
  use pk, state <- state.try_op(property.to_prop_key(state, key_val))
  // Step 3: Return ? target.[[HasProperty]](key) — trap-aware.
  use found, state <- state.try_op(object.has_property_stateful(state, ref, pk))
  #(state, Ok(JsBool(found)))
}

/// Reflect.isExtensible ( target ) — ES2024 §28.1.9
///
///   1. If target is not an Object, throw a TypeError exception.
///   2. Return ? target.[[IsExtensible]]().
fn reflect_is_extensible(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, _rest, state <- require_object(args, state, "isExtensible")
  // Step 2: trap-aware [[IsExtensible]].
  use ext, state <- state.try_op(object.is_extensible_stateful(state, ref))
  #(state, Ok(JsBool(ext)))
}

/// Reflect.ownKeys ( target ) — ES2024 §28.1.10
///
///   1. If target is not an Object, throw a TypeError exception.
///   2. Let keys be ? target.[[OwnPropertyKeys]]().
///   3. Return CreateArrayFromList(keys).
///
/// Per §10.1.11, [[OwnPropertyKeys]] returns: integer indices (ascending),
/// then string keys (creation order), then symbol keys (creation order).
fn reflect_own_keys(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, _rest, state <- require_object(args, state, "ownKeys")
  // Step 2: trap-aware [[OwnPropertyKeys]] — indices ascending, then named
  // keys, then symbols (or the proxy trap's order).
  use all_keys, state <- state.try_op(define_own.own_keys_stateful(state, ref))
  state.ok_array(state, all_keys)
}

/// Reflect.preventExtensions ( target ) — ES2024 §28.1.11
///
///   1. If target is not an Object, throw a TypeError exception.
///   2. Return ? target.[[PreventExtensions]]().
///
/// OrdinaryPreventExtensions (§10.1.4.1) always returns true.
fn reflect_prevent_extensions(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, _rest, state <- require_object(args, state, "preventExtensions")
  // Step 2: trap-aware [[PreventExtensions]].
  unwrap_set(object.prevent_extensions_stateful(state, ref))
}

/// Reflect.set ( target, propertyKey, V [ , receiver ] ) — ES2024 §28.1.12
///
///   1. If target is not an Object, throw a TypeError exception.
///   2. Let key be ? ToPropertyKey(propertyKey).
///   3. If receiver is not present, set receiver to target.
///   4. Return ? target.[[Set]](key, V, receiver).
fn reflect_set(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, rest, state <- require_object(args, state, "set")
  let #(key_val, val, receiver) = case rest {
    [k, v, r, ..] -> #(k, v, r)
    [k, v] -> #(k, v, JsObject(ref))
    [k] -> #(k, JsUndefined, JsObject(ref))
    [] -> #(JsUndefined, JsUndefined, JsObject(ref))
  }
  // Step 2: Let key be ? ToPropertyKey(propertyKey).
  use pk, state <- state.try_op(property.to_prop_key(state, key_val))
  // Step 4: Return ? target.[[Set]](key, V, receiver).
  unwrap_set(object.set_prop_value(state, ref, pk, val, receiver))
}

/// Adapt set_value/set_symbol_value's `Result(#(State, Bool), #(JsValue, State))`
/// to the dispatch return shape.
fn unwrap_set(
  r: Result(#(State(host), Bool), #(JsValue, State(host))),
) -> #(State(host), Result(JsValue, JsValue)) {
  case r {
    Ok(#(state, ok)) -> #(state, Ok(JsBool(ok)))
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// Reflect.setPrototypeOf ( target, proto ) — ES2024 §28.1.13
///
///   1. If target is not an Object, throw a TypeError exception.
///   2. If proto is not an Object and proto is not null, throw a TypeError.
///   3. Return ? target.[[SetPrototypeOf]](proto).
///
/// Unlike Object.setPrototypeOf, returns Bool instead of throwing on failure.
fn reflect_set_prototype_of(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, rest, state <- require_object(args, state, "setPrototypeOf")
  let proto_val = helpers.first_arg_or_undefined(rest)
  // Step 2: If proto is not an Object and proto is not null, throw a TypeError.
  let new_proto = case proto_val {
    JsObject(p) -> Ok(Some(p))
    JsNull -> Ok(None)
    _ -> Error(Nil)
  }
  case new_proto {
    Error(Nil) ->
      state.type_error(state, "Object prototype may only be an Object or null")
    // Step 3: ? target.[[SetPrototypeOf]](proto) — the proxy-vs-ordinary
    // dispatch lives in ops/define_own; every refusal is just `false` here.
    Ok(new_proto) ->
      case define_own.set_prototype_of_stateful(state, ref, new_proto) {
        Ok(#(state, status)) -> #(state, Ok(JsBool(result.is_ok(status))))
        Error(#(thrown, state)) -> #(state, Error(thrown))
      }
  }
}
