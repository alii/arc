import arc/vm/builtins/common
import arc/vm/builtins/helpers
import arc/vm/builtins/object as builtins_object
import arc/vm/heap.{type Heap}
import arc/vm/internal/elements
import arc/vm/ops/object
import arc/vm/ops/property
import arc/vm/state.{type State, State}
import arc/vm/value.{
  type JsValue, type Ref, type ReflectNativeFn, JsBool, JsNull, JsObject,
  JsString, JsSymbol, JsUndefined, ObjectSlot, OrdinaryObject, ReflectApply,
  ReflectConstruct, ReflectDefineProperty, ReflectDeleteProperty, ReflectGet,
  ReflectGetOwnPropertyDescriptor, ReflectGetPrototypeOf, ReflectHas,
  ReflectIsExtensible, ReflectNative, ReflectOwnKeys, ReflectPreventExtensions,
  ReflectSet, ReflectSetPrototypeOf,
}
import gleam/bool
import gleam/dict
import gleam/list
import gleam/option.{None, Some}

// ============================================================================
// Init — set up the Reflect global object
// ============================================================================

/// Set up the Reflect global object.
/// Reflect is NOT a constructor — it's a plain object with static methods
/// (like Math/JSON), per ES2024 §28.1.
pub fn init(h: Heap, object_proto: Ref, function_proto: Ref) -> #(Heap, Ref) {
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

  let properties = common.named_props(methods)
  let symbol_properties =
    dict.from_list([
      #(
        value.symbol_to_string_tag,
        value.data(JsString("Reflect")) |> value.configurable(),
      ),
    ])

  let #(h, reflect_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: OrdinaryObject,
        properties:,
        elements: elements.new(),
        prototype: Some(object_proto),
        symbol_properties:,
        extensible: True,
      ),
    )
  let h = heap.root(h, reflect_ref)

  #(h, reflect_ref)
}

// ============================================================================
// Dispatch
// ============================================================================

/// Per-module dispatch for Reflect native functions.
pub fn dispatch(
  native: ReflectNativeFn,
  args: List(JsValue),
  _this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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
  state: State,
  method: String,
  cont: fn(Ref, List(JsValue), State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case args {
    [JsObject(ref), ..rest] -> cont(ref, rest, state)
    _ ->
      state.type_error(state, "Reflect." <> method <> " called on non-object")
  }
}

/// CreateListFromArrayLike ( obj ) — ES2024 §7.3.19
/// Simplified: reads indices 0..length from array/arguments objects.
/// Holes become undefined. Non-array-like objects produce [].
fn create_list_from_array_like(h: Heap, ref: Ref) -> List(JsValue) {
  heap.read_array_like(h, ref)
  |> option.map(fn(p) {
    let #(length, elems) = p
    gather_elements(elems, 0, length, [])
  })
  |> option.unwrap([])
}

fn gather_elements(
  elems: value.JsElements,
  idx: Int,
  length: Int,
  acc: List(JsValue),
) -> List(JsValue) {
  case idx >= length {
    True -> list.reverse(acc)
    False ->
      gather_elements(elems, idx + 1, length, [elements.get(elems, idx), ..acc])
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let #(target, this_arg, args_list) = case args {
    [t, th, a, ..] -> #(t, th, a)
    [t, th] -> #(t, th, JsUndefined)
    [t] -> #(t, JsUndefined, JsUndefined)
    [] -> #(JsUndefined, JsUndefined, JsUndefined)
  }
  // Step 1: If IsCallable(target) is false, throw a TypeError.
  use <- bool.guard(
    !helpers.is_callable(state.heap, target),
    state.type_error(state, "Reflect.apply: target is not a function"),
  )
  // Step 2: Let args be ? CreateListFromArrayLike(argumentsList).
  use call_args, state <- require_array_like(state, args_list)
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
///
/// TODO: newTarget (third arg) not yet wired — needs separate plumbing through
/// do_construct to override the prototype source.
fn reflect_construct(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let #(target, args_list) = case args {
    [t, a, ..] -> #(t, a)
    [t] -> #(t, JsUndefined)
    [] -> #(JsUndefined, JsUndefined)
  }
  // Step 4: Let args be ? CreateListFromArrayLike(argumentsList).
  use ctor_args, state <- require_array_like(state, args_list)
  // Steps 1, 5: state.construct validates target is an object and runs
  // [[Construct]]. Non-constructor objects throw inside do_construct.
  use result, state <- state.try_op(state.construct(state, target, ctor_args))
  #(state, Ok(result))
}

/// CreateListFromArrayLike per §7.3.19 — throws TypeError on non-object.
fn require_array_like(
  state: State,
  val: JsValue,
  cont: fn(List(JsValue), State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case val {
    JsObject(ref) -> cont(create_list_from_array_like(state.heap, ref), state)
    _ -> state.type_error(state, "CreateListFromArrayLike called on non-object")
  }
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use ref, rest, state <- require_object(args, state, "defineProperty")
  let #(key_val, desc_val) = case rest {
    [k, d, ..] -> #(k, d)
    [k] -> #(k, JsUndefined)
    [] -> #(JsUndefined, JsUndefined)
  }
  case desc_val {
    JsObject(desc_ref) -> {
      // Step 2: Let key be ? ToPropertyKey(propertyKey).
      use key_str, state <- state.try_to_string(state, key_val)
      // Steps 3-4: ToPropertyDescriptor + [[DefineOwnProperty]].
      // apply_descriptor throws on failure; we catch and return false.
      case builtins_object.apply_descriptor(state, ref, key_str, desc_ref) {
        Ok(state) -> #(state, Ok(JsBool(True)))
        // Validation failure → return false instead of re-throwing.
        Error(#(_thrown, state)) -> #(state, Ok(JsBool(False)))
      }
    }
    _ -> state.type_error(state, "Property description must be an object")
  }
}

/// Reflect.deleteProperty ( target, propertyKey ) — ES2024 §28.1.4
///
///   1. If target is not an Object, throw a TypeError exception.
///   2. Let key be ? ToPropertyKey(propertyKey).
///   3. Return ? target.[[Delete]](key).
fn reflect_delete_property(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use ref, rest, state <- require_object(args, state, "deleteProperty")
  let key_val = helpers.first_arg(rest)
  // Step 2: Let key be ? ToPropertyKey(propertyKey).
  case key_val {
    JsSymbol(sym) -> {
      // Symbol keys: delete from symbol_properties directly.
      let #(heap, ok) = delete_symbol_prop(state.heap, ref, sym)
      #(State(..state, heap:), Ok(JsBool(ok)))
    }
    _ -> {
      use pk, state <- state.try_op(property.to_property_key(state, key_val))
      // Step 3: Return ? target.[[Delete]](key).
      let #(heap, ok) = object.delete_property(state.heap, ref, pk)
      #(State(..state, heap:), Ok(JsBool(ok)))
    }
  }
}

/// Delete a symbol-keyed own property. Returns #(heap, success).
/// Success is False only for non-configurable properties (per §10.1.10.1).
fn delete_symbol_prop(h: Heap, ref: Ref, sym: value.SymbolId) -> #(Heap, Bool) {
  case heap.read(h, ref) {
    Some(ObjectSlot(symbol_properties:, ..) as slot) ->
      case dict.get(symbol_properties, sym) {
        Ok(value.DataProperty(configurable: True, ..))
        | Ok(value.AccessorProperty(configurable: True, ..)) -> #(
          heap.write(
            h,
            ref,
            ObjectSlot(
              ..slot,
              symbol_properties: dict.delete(symbol_properties, sym),
            ),
          ),
          True,
        )
        Ok(_) -> #(h, False)
        Error(_) -> #(h, True)
      }
    _ -> #(h, True)
  }
}

/// Reflect.get ( target, propertyKey [ , receiver ] ) — ES2024 §28.1.5
///
///   1. If target is not an Object, throw a TypeError exception.
///   2. Let key be ? ToPropertyKey(propertyKey).
///   3. If receiver is not present, set receiver to target.
///   4. Return ? target.[[Get]](key, receiver).
fn reflect_get(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use ref, rest, state <- require_object(args, state, "get")
  let #(key_val, receiver) = case rest {
    [k, r, ..] -> #(k, r)
    [k] -> #(k, JsObject(ref))
    [] -> #(JsUndefined, JsObject(ref))
  }
  // Step 2: Let key be ? ToPropertyKey(propertyKey).
  case key_val {
    JsSymbol(sym) -> {
      use val, state <- state.try_op(object.get_symbol_value(
        state,
        ref,
        sym,
        receiver,
      ))
      #(state, Ok(val))
    }
    _ -> {
      use pk, state <- state.try_op(property.to_property_key(state, key_val))
      // Step 4: Return ? target.[[Get]](key, receiver).
      use val, state <- state.try_op(object.get_value(state, ref, pk, receiver))
      #(state, Ok(val))
    }
  }
}

/// Reflect.getOwnPropertyDescriptor ( target, propertyKey ) — ES2024 §28.1.6
///
///   1. If target is not an Object, throw a TypeError exception.
///   2. Let key be ? ToPropertyKey(propertyKey).
///   3. Let desc be ? target.[[GetOwnProperty]](key).
///   4. Return FromPropertyDescriptor(desc).
fn reflect_get_own_property_descriptor(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use ref, rest, state <- require_object(
    args,
    state,
    "getOwnPropertyDescriptor",
  )
  let key_val = helpers.first_arg(rest)
  let object_proto = state.builtins.object.prototype
  // Step 2: Let key be ? ToPropertyKey(propertyKey).
  use own_prop, state <- state.try_op(case key_val {
    JsSymbol(sym) ->
      Ok(#(
        case heap.read(state.heap, ref) {
          Some(ObjectSlot(symbol_properties:, ..)) ->
            dict.get(symbol_properties, sym) |> option.from_result
          _ -> None
        },
        state,
      ))
    _ ->
      case property.to_property_key(state, key_val) {
        Ok(#(pk, state)) ->
          Ok(#(object.get_own_property(state.heap, ref, pk), state))
        Error(#(thrown, state)) -> Error(#(thrown, state))
      }
  })
  // Steps 3-4: [[GetOwnProperty]] + FromPropertyDescriptor.
  case own_prop {
    Some(prop) -> {
      let #(heap, desc_ref) =
        builtins_object.make_descriptor_object(state.heap, prop, object_proto)
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use ref, _rest, state <- require_object(args, state, "getPrototypeOf")
  let proto = case heap.read(state.heap, ref) {
    Some(ObjectSlot(prototype: Some(p), ..)) -> JsObject(p)
    _ -> JsNull
  }
  #(state, Ok(proto))
}

/// Reflect.has ( target, propertyKey ) — ES2024 §28.1.8
///
///   1. If target is not an Object, throw a TypeError exception.
///   2. Let key be ? ToPropertyKey(propertyKey).
///   3. Return ? target.[[HasProperty]](key).
fn reflect_has(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use ref, rest, state <- require_object(args, state, "has")
  let key_val = helpers.first_arg(rest)
  // Step 2: Let key be ? ToPropertyKey(propertyKey).
  case key_val {
    JsSymbol(sym) -> #(
      state,
      Ok(JsBool(has_symbol_property(state.heap, ref, sym))),
    )
    _ -> {
      use pk, state <- state.try_op(property.to_property_key(state, key_val))
      // Step 3: Return ? target.[[HasProperty]](key).
      #(state, Ok(JsBool(object.has_property(state.heap, ref, pk))))
    }
  }
}

/// Walk the prototype chain looking for a symbol-keyed property.
fn has_symbol_property(h: Heap, ref: Ref, sym: value.SymbolId) -> Bool {
  case heap.read(h, ref) {
    Some(ObjectSlot(symbol_properties:, prototype:, ..)) ->
      case dict.has_key(symbol_properties, sym) {
        True -> True
        False ->
          case prototype {
            Some(proto_ref) -> has_symbol_property(h, proto_ref, sym)
            None -> False
          }
      }
    _ -> False
  }
}

/// Reflect.isExtensible ( target ) — ES2024 §28.1.9
///
///   1. If target is not an Object, throw a TypeError exception.
///   2. Return ? target.[[IsExtensible]]().
fn reflect_is_extensible(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use ref, _rest, state <- require_object(args, state, "isExtensible")
  let ext = case heap.read(state.heap, ref) {
    Some(ObjectSlot(extensible:, ..)) -> extensible
    _ -> False
  }
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use ref, _rest, state <- require_object(args, state, "ownKeys")
  let array_proto = state.builtins.array.prototype
  // String keys: indices first (ascending), then named keys.
  let string_keys =
    builtins_object.collect_own_keys(state.heap, ref, False)
    |> list.map(JsString)
  // Symbol keys after all string keys.
  let symbol_keys =
    builtins_object.collect_own_symbol_keys(state.heap, ref, False)
    |> list.map(JsSymbol)
  let all_keys = list.append(string_keys, symbol_keys)
  let #(heap, arr_ref) = common.alloc_array(state.heap, all_keys, array_proto)
  #(State(..state, heap:), Ok(JsObject(arr_ref)))
}

/// Reflect.preventExtensions ( target ) — ES2024 §28.1.11
///
///   1. If target is not an Object, throw a TypeError exception.
///   2. Return ? target.[[PreventExtensions]]().
///
/// OrdinaryPreventExtensions (§10.1.4.1) always returns true.
fn reflect_prevent_extensions(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use ref, _rest, state <- require_object(args, state, "preventExtensions")
  let heap = {
    use slot <- heap.update(state.heap, ref)
    case slot {
      ObjectSlot(..) -> ObjectSlot(..slot, extensible: False)
      _ -> slot
    }
  }
  #(State(..state, heap:), Ok(JsBool(True)))
}

/// Reflect.set ( target, propertyKey, V [ , receiver ] ) — ES2024 §28.1.12
///
///   1. If target is not an Object, throw a TypeError exception.
///   2. Let key be ? ToPropertyKey(propertyKey).
///   3. If receiver is not present, set receiver to target.
///   4. Return ? target.[[Set]](key, V, receiver).
fn reflect_set(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use ref, rest, state <- require_object(args, state, "set")
  let #(key_val, val, receiver) = case rest {
    [k, v, r, ..] -> #(k, v, r)
    [k, v] -> #(k, v, JsObject(ref))
    [k] -> #(k, JsUndefined, JsObject(ref))
    [] -> #(JsUndefined, JsUndefined, JsObject(ref))
  }
  // Step 2: Let key be ? ToPropertyKey(propertyKey).
  case key_val {
    JsSymbol(sym) ->
      unwrap_set(object.set_symbol_value(state, ref, sym, val, receiver))
    _ -> {
      use pk, state <- state.try_op(property.to_property_key(state, key_val))
      // Step 4: Return ? target.[[Set]](key, V, receiver).
      unwrap_set(object.set_value(state, ref, pk, val, receiver))
    }
  }
}

/// Adapt set_value/set_symbol_value's `Result(#(State, Bool), #(JsValue, State))`
/// to the dispatch return shape.
fn unwrap_set(
  r: Result(#(State, Bool), #(JsValue, State)),
) -> #(State, Result(JsValue, JsValue)) {
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use ref, rest, state <- require_object(args, state, "setPrototypeOf")
  let proto_val = helpers.first_arg(rest)
  // Step 2: If proto is not an Object and proto is not null, throw a TypeError.
  let new_proto = case proto_val {
    JsObject(p) -> Ok(Some(p))
    JsNull -> Ok(None)
    _ -> Error(Nil)
  }
  case new_proto {
    Error(_) ->
      state.type_error(state, "Object prototype may only be an Object or null")
    Ok(new_proto) ->
      // Step 3: OrdinarySetPrototypeOf (§10.1.2.1).
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(prototype: current, extensible:, ..)) ->
          case new_proto == current {
            // §10.1.2.1 step 2: If SameValue(V, current) is true, return true.
            True -> #(state, Ok(JsBool(True)))
            False ->
              case extensible {
                // §10.1.2.1 step 4: If extensible is false, return false.
                False -> #(state, Ok(JsBool(False)))
                True ->
                  // §10.1.2.1 step 7: cycle detection.
                  case
                    builtins_object.would_create_cycle(
                      state.heap,
                      ref,
                      new_proto,
                    )
                  {
                    True -> #(state, Ok(JsBool(False)))
                    False -> {
                      // §10.1.2.1 step 8: Set O.[[Prototype]] to V.
                      let heap = {
                        use slot <- heap.update(state.heap, ref)
                        case slot {
                          ObjectSlot(..) ->
                            ObjectSlot(..slot, prototype: new_proto)
                          _ -> slot
                        }
                      }
                      #(State(..state, heap:), Ok(JsBool(True)))
                    }
                  }
              }
          }
        _ -> #(state, Ok(JsBool(False)))
      }
  }
}
