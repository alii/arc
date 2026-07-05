import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers.{first_arg_or_undefined}
import arc/vm/builtins/iter_protocol
import arc/vm/builtins/symbol
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/js_string
import arc/vm/key.{Index}
import arc/vm/ops/define_own.{type ParsedDesc, ParsedDesc}
import arc/vm/ops/object
import arc/vm/ops/property
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsValue, type ObjectKey, type ObjectNativeFn, type Ref, AccessorProperty,
  ArrayObject, DataProperty, Dispatch, FunctionObject, GeneratorObject, JsBool,
  JsNull, JsNumber, JsObject, JsString, JsSymbol, JsUndefined, NativeFunction,
  ObjectAssign, ObjectConstructor, ObjectCreate, ObjectDefineProperties,
  ObjectDefineProperty, ObjectEntries, ObjectFreeze, ObjectFromEntries,
  ObjectGetOwnPropertyDescriptor, ObjectGetOwnPropertyDescriptors,
  ObjectGetOwnPropertyNames, ObjectGetOwnPropertySymbols, ObjectGetPrototypeOf,
  ObjectHasOwn, ObjectIs, ObjectIsExtensible, ObjectIsFrozen, ObjectIsSealed,
  ObjectKeys, ObjectNative, ObjectPreventExtensions,
  ObjectPrototypeHasOwnProperty, ObjectPrototypePropertyIsEnumerable,
  ObjectPrototypeToString, ObjectPrototypeValueOf, ObjectSeal,
  ObjectSetPrototypeOf, ObjectSlot, ObjectValues, OrdinaryObject, StringPropKey,
  SymbolPropKey,
}
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import gleam/set

/// V8/Node's standard ToObject failure message.
const cannot_convert = "Cannot convert undefined or null to object"

/// CPS form of ToPropertyKey (§7.1.19) so callers can `use` before
/// `case this`, preserving spec ordering (hasOwnProperty does ToPropertyKey
/// step 1, ToObject step 2).
fn try_to_property_key(
  state: State(host),
  key_val: JsValue,
  cont: fn(ObjectKey, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case property.to_prop_key(state, key_val) {
    Error(#(thrown, state)) -> #(state, Error(thrown))
    Ok(#(dkey, state)) -> cont(dkey, state)
  }
}

/// Set up Object constructor and Object.prototype methods.
/// Object.prototype is already allocated (it's the root of all chains).
pub fn init(
  h: Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(host), BuiltinType) {
  let #(h, static_methods) =
    common.alloc_methods(h, function_proto, [
      #(
        "getOwnPropertyDescriptor",
        ObjectNative(ObjectGetOwnPropertyDescriptor),
        2,
      ),
      #("defineProperty", ObjectNative(ObjectDefineProperty), 3),
      #("defineProperties", ObjectNative(ObjectDefineProperties), 2),
      #("getOwnPropertyNames", ObjectNative(ObjectGetOwnPropertyNames), 1),
      #("keys", ObjectNative(ObjectKeys), 1),
      #("values", ObjectNative(ObjectValues), 1),
      #("entries", ObjectNative(ObjectEntries), 1),
      #("create", ObjectNative(ObjectCreate), 2),
      #("assign", ObjectNative(ObjectAssign), 2),
      #("is", ObjectNative(ObjectIs), 2),
      #("hasOwn", ObjectNative(ObjectHasOwn), 2),
      #("getPrototypeOf", ObjectNative(ObjectGetPrototypeOf), 1),
      #("setPrototypeOf", ObjectNative(ObjectSetPrototypeOf), 2),
      #("freeze", ObjectNative(ObjectFreeze), 1),
      #("isFrozen", ObjectNative(ObjectIsFrozen), 1),
      #("isExtensible", ObjectNative(ObjectIsExtensible), 1),
      #("preventExtensions", ObjectNative(ObjectPreventExtensions), 1),
      #("fromEntries", ObjectNative(ObjectFromEntries), 1),
      #("seal", ObjectNative(ObjectSeal), 1),
      #("isSealed", ObjectNative(ObjectIsSealed), 1),
      #(
        "getOwnPropertyDescriptors",
        ObjectNative(ObjectGetOwnPropertyDescriptors),
        1,
      ),
      #("getOwnPropertySymbols", ObjectNative(ObjectGetOwnPropertySymbols), 1),
      #("groupBy", ObjectNative(value.ObjectGroupBy), 2),
    ])
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("hasOwnProperty", ObjectNative(ObjectPrototypeHasOwnProperty), 1),
      #(
        "propertyIsEnumerable",
        ObjectNative(ObjectPrototypePropertyIsEnumerable),
        1,
      ),
      #("toString", ObjectNative(ObjectPrototypeToString), 0),
      #("valueOf", ObjectNative(ObjectPrototypeValueOf), 0),
      #("isPrototypeOf", ObjectNative(value.ObjectPrototypeIsPrototypeOf), 1),
      #("toLocaleString", ObjectNative(value.ObjectPrototypeToLocaleString), 0),
      // Annex B §B.2.2.2-B.2.2.5 — legacy accessor-management methods.
      #("__defineGetter__", ObjectNative(value.ObjectPrototypeDefineGetter), 2),
      #("__defineSetter__", ObjectNative(value.ObjectPrototypeDefineSetter), 2),
      #("__lookupGetter__", ObjectNative(value.ObjectPrototypeLookupGetter), 1),
      #("__lookupSetter__", ObjectNative(value.ObjectPrototypeLookupSetter), 1),
    ])
  // Annex B §B.2.2.1 — Object.prototype.__proto__ accessor property.
  let #(h, proto_accessor) =
    common.alloc_get_set_accessor(
      h,
      function_proto,
      ObjectNative(value.ObjectPrototypeProtoGetter),
      ObjectNative(value.ObjectPrototypeProtoSetter),
      "__proto__",
    )
  let proto_methods = [#("__proto__", proto_accessor), ..proto_methods]
  common.init_type_on(
    h,
    object_proto,
    function_proto,
    proto_methods,
    fn(_) { Dispatch(ObjectNative(ObjectConstructor)) },
    "Object",
    1,
    static_methods,
    True,
  )
}

/// Per-module dispatch for Object native functions.
pub fn dispatch(
  native: ObjectNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case native {
    value.ObjectConstructor -> call_native(args, this, state)
    value.ObjectGetOwnPropertyDescriptor ->
      get_own_property_descriptor(args, state)
    value.ObjectDefineProperty -> define_property(args, state)
    value.ObjectDefineProperties -> define_properties(args, state)
    value.ObjectGetOwnPropertyNames -> get_own_property_names(args, state)
    value.ObjectKeys -> keys(args, state)
    value.ObjectValues -> values(args, state)
    value.ObjectEntries -> entries(args, state)
    value.ObjectCreate -> create(args, state)
    value.ObjectAssign -> assign(args, state)
    value.ObjectIs -> is(args, state)
    value.ObjectHasOwn -> has_own(args, state)
    value.ObjectGetPrototypeOf -> get_prototype_of(args, state)
    value.ObjectSetPrototypeOf -> set_prototype_of(args, state)
    value.ObjectFreeze -> freeze(args, state)
    value.ObjectIsFrozen -> is_frozen(args, state)
    value.ObjectIsExtensible -> is_extensible(args, state)
    value.ObjectPreventExtensions -> prevent_extensions(args, state)
    value.ObjectPrototypeHasOwnProperty -> has_own_property(this, args, state)
    value.ObjectPrototypePropertyIsEnumerable ->
      property_is_enumerable(this, args, state)
    value.ObjectPrototypeToString -> object_to_string(this, args, state)
    value.ObjectPrototypeValueOf -> object_value_of(this, args, state)
    value.ObjectFromEntries -> from_entries(args, state)
    value.ObjectSeal -> seal(args, state)
    value.ObjectIsSealed -> is_sealed(args, state)
    value.ObjectGetOwnPropertyDescriptors ->
      get_own_property_descriptors(args, state)
    value.ObjectGetOwnPropertySymbols -> get_own_property_symbols(args, state)
    value.ObjectPrototypeIsPrototypeOf -> is_prototype_of(this, args, state)
    value.ObjectPrototypeToLocaleString ->
      object_to_locale_string(this, args, state)
    value.ObjectGroupBy -> group_by(args, state)
    value.ObjectPrototypeDefineGetter ->
      define_getter_setter(this, args, state, AsGetter)
    value.ObjectPrototypeDefineSetter ->
      define_getter_setter(this, args, state, AsSetter)
    value.ObjectPrototypeLookupGetter ->
      lookup_getter_setter(this, args, state, AsGetter)
    value.ObjectPrototypeLookupSetter ->
      lookup_getter_setter(this, args, state, AsSetter)
    value.ObjectPrototypeProtoGetter -> get_prototype_of([this], state)
    value.ObjectPrototypeProtoSetter -> proto_setter(this, args, state)
  }
}

/// Object() / new Object() constructor.
/// ES2024 §20.1.1.1 Object ( [ value ] )
///
///   1. If NewTarget is neither undefined nor the active function object, then
///      (skipped — no NewTarget tracking yet)
///   2. If value is undefined or null, return OrdinaryObjectCreate(%Object.prototype%).
///   3. Return ! ToObject(value).
fn call_native(
  args: List(JsValue),
  _this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let object_proto = state.builtins.object.prototype
  case args {
    // §20.1.1.1 step 3: If value is an Object, return it directly.
    [JsObject(_) as obj, ..] -> #(state, Ok(obj))
    // §20.1.1.1 step 3: Primitives → ToObject creates a wrapper.
    [JsString(_) as v, ..]
    | [JsNumber(_) as v, ..]
    | [JsBool(_) as v, ..]
    | [JsSymbol(_) as v, ..]
    | [value.JsBigInt(_) as v, ..] ->
      case common.to_object(state.heap, state.builtins, v) {
        Some(#(heap, ref)) -> #(State(..state, heap:), Ok(JsObject(ref)))
        // Should not happen — to_object handles all primitives
        None -> #(state, Ok(v))
      }
    // §20.1.1.1 step 2: undefined, null, or absent → new empty object.
    _ -> {
      let #(heap, ref) = common.alloc_pojo(state.heap, object_proto, [])
      #(State(..state, heap:), Ok(JsObject(ref)))
    }
  }
}

/// Object.getOwnPropertyDescriptor(O, P)
/// ES2024 §20.1.2.8
///
/// 1. Let obj be ? ToObject(O).
/// 2. Let key be ? ToPropertyKey(P).
/// 3. Let desc be ? obj.[[GetOwnProperty]](key).
/// 4. Return FromPropertyDescriptor(desc).
fn get_own_property_descriptor(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let object_proto = state.builtins.object.prototype
  let #(target, key_val) = helpers.two_args_or_undefined(args)
  // Step 1 STRICTLY BEFORE step 2: ToPropertyKey(P) can run user code (a
  // `toString`/`valueOf`/`@@toPrimitive` on P), and ToObject(null) must throw
  // its TypeError first — `Object.getOwnPropertyDescriptor(null, {toString(){
  // throw new Error()}})` throws the TypeError, not the Error.
  case target {
    // Step 1: ToObject throws TypeError for null/undefined.
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    JsObject(ref) -> {
      // Step 2: Let key be ? ToPropertyKey(P).
      use key, state <- try_to_property_key(state, key_val)
      // Step 3: Let desc be ? obj.[[GetOwnProperty]](key) — trap-aware. (A TDZ
      // namespace binding throws inside it, per §10.4.6.5.)
      use prop_opt, state <- state.try_op(define_own.own_property_keyed(
        state,
        ref,
        key,
      ))
      case prop_opt {
        Some(prop) -> {
          // Step 4: Return FromPropertyDescriptor(desc).
          // (desc is not undefined, so we build a descriptor object.)
          let #(heap, desc_ref) =
            define_own.make_descriptor_object(state.heap, prop, object_proto)
          #(State(..state, heap:), Ok(JsObject(desc_ref)))
        }
        // Step 4: desc is undefined, return undefined.
        None -> #(state, Ok(JsUndefined))
      }
    }
    // String primitives: index properties + "length" are own properties,
    // synthesized by the shared string-exotic own-property table.
    JsString(s) -> {
      use key, state <- try_to_property_key(state, key_val)
      case define_own.string_exotic_own_property(s, key) {
        // Step 4: Return FromPropertyDescriptor(desc).
        Some(prop) -> {
          let #(heap, desc_ref) =
            define_own.make_descriptor_object(state.heap, prop, object_proto)
          #(State(..state, heap:), Ok(JsObject(desc_ref)))
        }
        None -> #(state, Ok(JsUndefined))
      }
    }
    // Number/boolean/symbol wrappers have no own string-keyed properties, but
    // ToObject succeeded so step 2's ToPropertyKey still runs (observable).
    _ -> {
      use _key, state <- try_to_property_key(state, key_val)
      #(state, Ok(JsUndefined))
    }
  }
}

/// Object.defineProperty ( O, P, Attributes )
/// ES2024 §20.1.2.4
///
/// 1. If O is not an Object, throw a TypeError exception.
/// 2. Let key be ? ToPropertyKey(P).
/// 3. Let desc be ? ToPropertyDescriptor(Attributes).
/// 4. Perform ? DefinePropertyOrThrow(O, key, desc).
/// 5. Return O.
fn define_property(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case args {
    [JsObject(ref) as obj, ..rest] -> {
      let key_val = first_arg_or_undefined(rest)
      let desc_val = case rest {
        [_, d, ..] -> d
        _ -> JsUndefined
      }
      // Steps 2-4: ToPropertyKey + ToPropertyDescriptor + DefinePropertyOrThrow
      // (apply_descriptor combines all three steps, in that order — a
      // non-object `Attributes` is only rejected AFTER ToPropertyKey has run
      // any user `toString`/`valueOf` on P).
      use state <- state.try_state(define_own.apply_descriptor(
        state,
        ref,
        key_val,
        desc_val,
      ))
      // Step 5: Return O.
      #(state, Ok(obj))
    }
    // Step 1: If O is not an Object, throw a TypeError.
    _ -> state.type_error(state, "Object.defineProperty called on non-object")
  }
}

/// Object.getOwnPropertyNames ( O ) — ES2024 §20.1.2.9
///
/// Delegates to GetOwnPropertyKeys ( O, string ) — §20.1.2.11.1:
///   1. Let obj be ? ToObject(O).
///   2. Let keys be ? obj.[[OwnPropertyKeys]]().
///   3. Let nameList be a new empty List.
///   4. For each element nextKey of keys, do
///      a. If nextKey is a String, then
///         i. Append nextKey to nameList.
///   5. Return nameList.
/// (CreateArrayFromList is applied by the caller, Object.getOwnPropertyNames.)
///
fn get_own_property_names(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  own_keys_impl(args, state, False)
}

/// Object.keys ( O ) — ES2024 §20.1.2.16
///
///   1. Let obj be ? ToObject(O).
///   2. Let nameList be ? EnumerableOwnProperties(obj, key).
///   3. Return CreateArrayFromList(nameList).
///
fn keys(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  own_keys_impl(args, state, True)
}

/// Shared implementation for Object.getOwnPropertyNames and Object.keys.
///
/// Implements the shared pattern from:
///   - GetOwnPropertyKeys ( O, type ) — §20.1.2.11.1 (for getOwnPropertyNames)
///   - Object.keys — §20.1.2.16 calling EnumerableOwnProperties(obj, key)
///
/// Both follow the same structure:
///   1. Let obj be ? ToObject(O).           — null/undefined throw TypeError
///   2. Collect own keys (all or enumerable-only).
///   3. Return CreateArrayFromList(keys).   — alloc_array builds the result
fn own_keys_impl(
  args: List(JsValue),
  state: State(host),
  enumerable_only: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  case first_arg_or_undefined(args) {
    JsObject(ref) ->
      case object.as_proxy(state.heap, ref) {
        // Proxy: route through the ownKeys trap (plus per-key
        // getOwnPropertyDescriptor traps for the enumerable-only filter).
        Some(_) ->
          case enumerable_only {
            True -> {
              use ks, state <- state.try_op(
                define_own.enumerable_string_keys_stateful(state, ref),
              )
              state.ok_array(state, list.map(ks, JsString))
            }
            False -> {
              use all_keys, state <- state.try_op(define_own.own_property_keys(
                state,
                ref,
              ))
              // Step 4: keep the String keys, drop the Symbol ones.
              let strings =
                list.filter_map(all_keys, fn(k) {
                  case k {
                    StringPropKey(display:, ..) -> Ok(JsString(display))
                    SymbolPropKey(_) -> Error(Nil)
                  }
                })
              state.ok_array(state, strings)
            }
          }
        None -> {
          // Step 2: Collect own string-keyed properties
          let ks = define_own.collect_own_keys(state.heap, ref, enumerable_only)
          // Object.keys (EnumerableOwnProperties) calls [[GetOwnProperty]] per
          // key, which throws on a TDZ namespace binding. getOwnPropertyNames
          // only calls [[OwnPropertyKeys]] (no throw), so guard only the
          // enumerable path.
          let guard = case enumerable_only {
            True -> object.namespace_tdz_guard(state, ref, ks)
            False -> Ok(state)
          }
          case guard {
            Error(#(err, state)) -> #(state, Error(err))
            // Step 3: CreateArrayFromList(nameList)
            Ok(state) -> state.ok_array(state, list.map(ks, JsString))
          }
        }
      }
    // Step 1: ToObject — null/undefined throw TypeError
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    // String primitives: own keys are index chars + "length". The length
    // comes from js_string.length, the same source
    // string_exotic_own_property uses, so the key LIST and the per-key
    // [[GetOwnProperty]] lookups agree by construction.
    JsString(s) -> {
      let len = js_string.length(s)
      let index_keys = string_index_keys(0, len)
      // "length" is own but non-enumerable — include for getOwnPropertyNames, skip for keys
      let ks = case enumerable_only {
        True -> index_keys
        False -> list.append(index_keys, [JsString("length")])
      }
      state.ok_array(state, ks)
    }
    // Number/boolean/symbol wrappers have no own string-keyed properties.
    _ -> state.ok_array(state, [])
  }
}

/// Build list of JsString index keys ["0", "1", ..., "len-1"] for string primitives.
fn string_index_keys(i: Int, len: Int) -> List(JsValue) {
  case i >= len {
    True -> []
    False -> [JsString(int.to_string(i)), ..string_index_keys(i + 1, len)]
  }
}

/// The same index keys as ObjectKeys — the String exotic's [[OwnPropertyKeys]]
/// (§10.4.3.3 step 3) for callers that look each key back up.
fn string_index_object_keys(i: Int, len: Int) -> List(ObjectKey) {
  case i >= len {
    True -> []
    False -> [
      define_own.index_object_key(i),
      ..string_index_object_keys(i + 1, len)
    ]
  }
}

/// Object.prototype.hasOwnProperty(key)
/// Checks if the object has an own property with the given key (NOT prototype chain).
/// ES2024: ToObject(this) — throws on null/undefined, primitives coerce (→ false).
/// Object.prototype.hasOwnProperty ( V ) — ES2024 §20.1.3.2
///
///   1. Let P be ? ToPropertyKey(V).
///   2. Let O be ? ToObject(this value).
///   3. Return ? HasOwnProperty(O, P).
///
/// HasOwnProperty ( O, P ) — §7.3.12:
///   1. Assert: Type(O) is Object.
///   2. Assert: IsPropertyKey(P) is true.
///   3. Let desc be ? O.[[GetOwnProperty]](P).
///   4. If desc is undefined, return false.
///   5. Return true.
///
fn has_own_property(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let key_val = first_arg_or_undefined(args)
  // Step 1: Let P be ? ToPropertyKey(V).
  use key, state <- try_to_property_key(state, key_val)
  case this {
    // Step 2: Let O be ? ToObject(this value).
    // Step 3: Return ? HasOwnProperty(O, P) — trap-aware.
    JsObject(ref) -> {
      // (A TDZ namespace binding throws inside [[GetOwnProperty]].)
      use prop_opt, state <- state.try_op(define_own.own_property_keyed(
        state,
        ref,
        key,
      ))
      #(state, Ok(JsBool(option.is_some(prop_opt))))
    }
    // Step 2: ToObject throws TypeError on null/undefined.
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    // String primitives: own keys are index chars + "length" — the shared
    // string-exotic own-property table decides.
    JsString(s) -> #(
      state,
      Ok(JsBool(option.is_some(define_own.string_exotic_own_property(s, key)))),
    )
    // Number/boolean/symbol have no own string-keyed properties.
    _ -> #(state, Ok(JsBool(False)))
  }
}

/// Object.prototype.propertyIsEnumerable ( V ) — ES2024 §20.1.3.4
///
///   1. Let P be ? ToPropertyKey(V).
///   2. Let O be ? ToObject(this value).
///   3. Let desc be ? O.[[GetOwnProperty]](P).
///   4. If desc is undefined, return false.
///   5. Return desc.[[Enumerable]].
///
fn property_is_enumerable(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let key_val = first_arg_or_undefined(args)
  // Step 1: Let P be ? ToPropertyKey(V).
  use key, state <- try_to_property_key(state, key_val)
  case this {
    JsObject(ref) -> {
      // Steps 3-5: [[GetOwnProperty]] (trap-aware) → desc.[[Enumerable]] or
      // false. A namespace TDZ binding throws (its [[GetOwnProperty]]
      // performs [[Get]]).
      use prop_opt, state <- state.try_op(define_own.own_property_keyed(
        state,
        ref,
        key,
      ))
      case prop_opt {
        Some(p) -> #(state, Ok(JsBool(value.prop_enumerable(p))))
        None -> #(state, Ok(JsBool(False)))
      }
    }
    // Step 2: ToObject throws TypeError on null/undefined.
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    // String primitives: the shared string-exotic own-property table gives
    // [[Enumerable]] (index chars are enumerable, "length" is not).
    JsString(s) -> {
      let enumerable =
        define_own.string_exotic_own_property(s, key)
        |> option.map(value.prop_enumerable)
        |> option.unwrap(False)
      #(state, Ok(JsBool(enumerable)))
    }
    // Number/boolean/symbol have no own string-keyed properties.
    _ -> #(state, Ok(JsBool(False)))
  }
}

/// Object.prototype.toString ( ) — ES2024 §20.1.3.6
///
///   1. If the this value is undefined, return "[object Undefined]".
///   2. If the this value is null, return "[object Null]".
///   3. Let O be ! ToObject(this value).
///   4. Let isArray be ? IsArray(O).
///   5. If isArray is true, let builtinTag be "Array".
///   6. Else if O has a [[ParameterMap]] internal slot, let builtinTag be "Arguments".
///   7. Else if O has a [[Call]] internal method, let builtinTag be "Function".
///   8. Else if O has an [[ErrorData]] internal slot, let builtinTag be "Error".
///   9. Else if O has a [[BooleanData]] internal slot, let builtinTag be "Boolean".
///  10. Else if O has a [[NumberData]] internal slot, let builtinTag be "Number".
///  11. Else if O has a [[StringData]] internal slot, let builtinTag be "String".
///  12. Else if O has a [[DateValue]] internal slot, let builtinTag be "Date".
///  13. Else if O has a [[RegExpMatcher]] internal slot, let builtinTag be "RegExp".
///  14. Else, let builtinTag be "Object".
///  15. Let tag be ? Get(O, @@toStringTag).
///  16. If tag is not a String, set tag to builtinTag.
///  17. Return the string-concatenation of "[object ", tag, and "]".
///
/// TODO(Deviation): Steps 8, 12 (Error, Date) are not yet
/// implemented since those object kinds don't exist yet.
fn object_to_string(
  this: JsValue,
  _args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this {
    // Steps 1-2: undefined/null skip ToObject and the @@toStringTag lookup.
    JsUndefined | value.JsUninitialized -> #(
      state,
      Ok(JsString("[object Undefined]")),
    )
    JsNull -> #(state, Ok(JsString("[object Null]")))
    _ -> {
      // Step 4: Let isArray be ? IsArray(O) — pierces proxies to their
      // target (§7.2.2 step 3) and throws TypeError when revoked.
      use is_arr, state <- state.try_op(object.try_is_array(state, this))
      // Steps 5-14: builtinTag.
      let fallback = case is_arr, this {
        // Step 5: If isArray is true, builtinTag is "Array".
        True, _ -> "Array"
        // Step 3 for primitives: ToObject wraps — the wrapper's internal
        // slots ([[BooleanData]]/[[NumberData]]/[[StringData]]) drive
        // steps 9-11. Symbol/BigInt wrappers have no builtinTag step →
        // "Object" (step 14); Symbol stays "Symbol" while
        // %Symbol.prototype% lacks @@toStringTag (deviation — it currently
        // aliases %Object.prototype%).
        False, JsBool(_) -> "Boolean"
        False, JsNumber(_) -> "Number"
        False, JsString(_) -> "String"
        False, JsSymbol(_) -> "Symbol"
        // Steps 6-14: classify by internal slots / [[Call]].
        False, JsObject(ref) -> object_tag(state.heap, ref)
        // JsBigInt and any other primitive → step 14.
        False, _ -> "Object"
      }
      // Step 15: Let tag be ? Get(O, @@toStringTag) — a genuine [[Get]]:
      // walks the prototype chain, invokes getters, fires proxy traps, and
      // propagates abrupt completions.
      use tag_val, state <- state.try_op(object.get_symbol_value_of(
        state,
        this,
        value.symbol_to_string_tag,
      ))
      // Step 16: If tag is not a String, set tag to builtinTag.
      let t = case tag_val {
        JsString(t) -> t
        _ -> fallback
      }
      // Step 17: Return the string-concatenation of "[object ", tag, "]".
      #(state, Ok(JsString("[object " <> t <> "]")))
    }
  }
}

/// builtinTag classification — steps 6-14 of Object.prototype.toString
/// (§20.1.3.6). Step 4-5 (IsArray) is handled by the caller. This is ONLY the
/// fallback: step 15's ? Get(O, @@toStringTag) wins whenever it yields a
/// string (the builtin prototypes carry the spec @@toStringTag data
/// properties — "Map", "Set", "Promise", "Map Iterator", …).
///
/// Spec builtinTag set (steps 6-13): Arguments, Function, Error, Boolean,
/// Number, String, Date, RegExp; everything else is "Object" (step 14).
///
/// Deviations (kinds whose prototypes don't yet carry their spec
/// @@toStringTag — keep the legacy kind-derived tag so behavior is
/// preserved until those prototypes gain the property):
///   SymbolObject → "Symbol", Generator/AsyncGenerator, String Iterator,
///   Async-from-Sync Iterator, Iterator Helper, Wrap-for-valid-Iterator,
///   IteratorRecord, and the arc-specific Pid/Subject/Selector/Timer kinds.
fn object_tag(heap: Heap(host), ref: Ref) -> String {
  case heap.read(heap, ref) {
    Some(ObjectSlot(kind:, ..)) ->
      case kind {
        // Step 5 (defensive — caller already handled IsArray).
        ArrayObject(_) -> "Array"
        // Step 6: [[ParameterMap]] → "Arguments".
        value.ArgumentsObject(_) -> "Arguments"
        // Step 7: [[Call]] → "Function" (proxies report via their own
        // [[Call]] — callable proxy included).
        FunctionObject(..) | NativeFunction(..) -> "Function"
        value.ProxyObject(callable: True, ..) -> "Function"
        // Step 8: [[ErrorData]] → "Error".
        value.ErrorObject(_) -> "Error"
        // Step 9: [[BooleanData]]; Step 10: [[NumberData]];
        // Step 11: [[StringData]]; Step 12: [[DateValue]];
        // Step 13: [[RegExpMatcher]].
        value.BooleanObject(_) -> "Boolean"
        value.NumberObject(_) -> "Number"
        value.StringObject(_) -> "String"
        value.DateObject(_) -> "Date"
        value.RegExpObject(..) -> "RegExp"
        // Deviation: %Symbol.prototype% aliases %Object.prototype% and lacks
        // @@toStringTag = "Symbol" — keep the kind-derived tag for now.
        value.SymbolObject(_) -> "Symbol"
        // Deviations: these prototypes don't carry their spec @@toStringTag
        // data properties yet, so the [[Get]] in step 15 yields undefined.
        GeneratorObject(_) -> "Generator"
        value.AsyncGeneratorObject(_) -> "AsyncGenerator"
        value.StringIteratorObject(..) -> "String Iterator"
        value.AsyncFromSyncIteratorObject(..) -> "Async-from-Sync Iterator"
        value.WrapForValidIteratorObject(..) -> "Iterator"
        value.IteratorRecordObject(..) -> "Iterator"
        // Step 14: everything else → "Object". Their display names come from
        // @@toStringTag on their prototypes (Map, Set, WeakMap, WeakSet,
        // Promise, BigInt, Module, the iterator prototypes, the Temporal
        // prototypes, %TypedArray%.prototype's accessor, …) via step 15.
        _ -> "Object"
      }
    _ -> "Object"
  }
}

/// Object.prototype.valueOf ( ) — ES2024 §20.1.3.7
///
///   1. Return ? ToObject(this value).
///
/// Primitives are wrapped in their object form (Boolean/Number/String/…)
/// per ToObject (§7.1.18); null/undefined throw TypeError.
fn object_value_of(
  this: JsValue,
  _args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this {
    // Deviation: %Symbol.prototype% currently aliases %Object.prototype%, so
    // Symbol.prototype.valueOf (§20.4.3.4: return ? thisSymbolValue(this))
    // resolves here — return the symbol primitive, not a wrapper, until
    // Symbol gets a dedicated prototype.
    JsSymbol(_) -> #(state, Ok(this))
    // Step 1: Return ? ToObject(this value).
    _ ->
      case common.to_object(state.heap, state.builtins, this) {
        None -> state.type_error(state, cannot_convert)
        Some(#(heap, ref)) -> #(State(..state, heap:), Ok(JsObject(ref)))
      }
  }
}

// ============================================================================
// Object static methods — Task #2
// ============================================================================

/// Object.values(obj) — returns array of enumerable own property values.
/// ES2024: ToObject coercion (throws on null/undefined).
/// Object.values ( O ) — ES2024 §20.1.2.22
///
///   1. Let obj be ? ToObject(O).
///   2. Let nameList be ? EnumerableOwnProperties(obj, value).
///   3. Return CreateArrayFromList(nameList).
fn values(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject + EnumerableOwnProperties(obj, value)
  use vals, state <- own_enumerable_impl(
    args,
    state,
    fn(ch, _idx) { JsString(ch) },
    fn(_key, val) { val },
  )
  // Step 3: CreateArrayFromList(nameList)
  state.ok_array(state, vals)
}

/// Object.entries ( O ) — ES2024 §20.1.2.5
///
///   1. Let obj be ? ToObject(O).
///   2. Let nameList be ? EnumerableOwnProperties(obj, key+value).
///   3. Return CreateArrayFromList(nameList).
///
/// EnumerableOwnProperties with kind=key+value (§7.3.23 step 3.a.ii.2.c):
///   "Let entry be CreateArrayFromList(« key, value »)."
///   "Append entry to results."
fn entries(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  // Steps 1-2: ToObject + EnumerableOwnProperties(obj, key+value)
  use pairs, state <- own_enumerable_impl(
    args,
    state,
    fn(ch, idx) { #(int.to_string(idx), JsString(ch)) },
    pair.new,
  )
  // Step 3: Build the result — each entry is CreateArrayFromList(« key, value »)
  let #(heap, refs) =
    list.fold(pairs, #(state.heap, []), fn(acc, kv) {
      let #(h, rs) = acc
      let #(k, v) = kv
      // §7.3.23 step 3.a.ii.2.c.ii: CreateArrayFromList(« key, value »)
      let #(h, r) = common.alloc_array(h, [JsString(k), v], array_proto)
      #(h, [JsObject(r), ..rs])
    })
  // Outer CreateArrayFromList for the result array
  state.ok_array(State(..state, heap:), list.reverse(refs))
}

/// Shared driver implementing steps 1-2 of Object.entries / Object.values —
/// the entry-collection portion of EnumerableOwnProperties ( O, kind )
/// — §7.3.23:
///
///   1. Let ownKeys be ? O.[[OwnPropertyKeys]]().
///   2. Let properties be a new empty List.
///   3. For each element key of ownKeys, do
///      a. If key is a String, then
///         i. Let desc be ? O.[[GetOwnProperty]](key).
///         ii. If desc is not undefined and desc.[[Enumerable]] is true, then
///             1. Let value be ? Get(O, key).
///             2. Append value (kind=value) or « key, value » (kind=key+value)
///                to properties.
///   4. Return properties.
///
/// The kind is expressed by two callbacks so Object.values never allocates
/// key strings or #(key, value) tuples it would immediately discard:
/// `from_char(ch, idx)` builds a result item for a string-primitive's indexed
/// character, `combine(key, value)` builds one for an object's own property.
fn own_enumerable_impl(
  args: List(JsValue),
  state: State(host),
  from_char: fn(String, Int) -> a,
  combine: fn(String, JsValue) -> a,
  cont: fn(List(a), State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case first_arg_or_undefined(args) {
    // §7.3.23 step 1 via [[OwnPropertyKeys]] (the ownKeys trap for a proxy),
    // then per key the [[GetOwnProperty]] descriptor read immediately followed
    // by the [[Get]] — the spec's observable interleaving (desc:a, get:a,
    // desc:b, get:b, …), which test262 asserts. Ordinary objects go through the
    // very same loop: an earlier key's getter can make a later key
    // non-enumerable (or delete it), and step 3.a.i must observe that.
    JsObject(ref) as receiver -> {
      use keys, state <- state.try_op(define_own.own_property_keys(state, ref))
      use items, state <- state.try_op(
        collect_enumerable_own(state, ref, receiver, keys, combine, []),
      )
      cont(items, state)
    }
    // ToObject: null/undefined → TypeError
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    // String primitives: enumerable own properties are the index characters.
    // §7.1.18: ToObject(String) creates a String exotic object; the indexed
    // character properties (enumerable, 0..len-1) come from §10.4.3 String exotics.
    JsString(s) ->
      cont(list.index_map(string_own_index_values(s), from_char), state)
    // Number/boolean/symbol wrappers have no own enumerable string keys.
    _ -> cont([], state)
  }
}

/// The own index-property VALUES of a String primitive, in index order.
///
/// The exact counterpart of the index KEYS, which every other String-exotic
/// path derives from `js_string.length` (see `string_index_keys`,
/// `own_property_names`, `has_own`): both count CODEPOINTS, so
/// `Object.keys(s)` and `Object.values(s)` cannot disagree in length and
/// `Object.values(s)` cannot disagree with `s[i]` (`js_string.char_at`).
///
/// A grapheme cluster is not a JS string unit — `string.to_graphemes("e\u{301}")`
/// yields ONE entry where the engine has TWO indices — so it must never appear
/// on a String-exotic own-property path.
fn string_own_index_values(s: String) -> List(String) {
  js_string.explode(s)
}

/// EnumerableOwnProperties §7.3.23 step 3 — per String key of the (already
/// trap-aware) [[OwnPropertyKeys]] result:
///   3.a.i:    desc = ? O.[[GetOwnProperty]](key)   (descriptor read / trap)
///   3.a.ii.1: value = ? Get(O, key)                (get, immediately after)
/// Symbol keys are skipped (step 3.a "If key is a String"). Returns items in
/// key order.
///
/// The descriptor read is per key and *interleaved* with the Get on purpose:
/// a getter run for an earlier key can make a later key non-enumerable, or
/// delete it outright, and the spec observes that. Snapshotting enumerability
/// up front (a `define_own.collect_own_keys(..., enumerable_only: True)` pre-filter) would
/// silently emit properties the spec forbids — that is why ordinary objects and
/// proxies share this one loop.
fn collect_enumerable_own(
  state: State(host),
  ref: Ref,
  receiver: JsValue,
  keys: List(ObjectKey),
  combine: fn(String, JsValue) -> a,
  acc: List(a),
) -> Result(#(List(a), State(host)), #(JsValue, State(host))) {
  case keys {
    [] -> Ok(#(list.reverse(acc), state))
    // §7.3.23 step 3.a: symbol keys are not part of EnumerableOwnProperties.
    [SymbolPropKey(_), ..rest] ->
      collect_enumerable_own(state, ref, receiver, rest, combine, acc)
    [StringPropKey(pkey:, display: s) as skey, ..rest] -> {
      use #(prop, state) <- result.try(define_own.own_property_keyed(
        state,
        ref,
        skey,
      ))
      let enumerable =
        option.map(prop, value.prop_enumerable) |> option.unwrap(False)
      case enumerable {
        True -> {
          use #(val, state) <- result.try(object.get_value(
            state,
            ref,
            pkey,
            receiver,
          ))
          collect_enumerable_own(state, ref, receiver, rest, combine, [
            combine(s, val),
            ..acc
          ])
        }
        False ->
          collect_enumerable_own(state, ref, receiver, rest, combine, acc)
      }
    }
  }
}

/// Object.create ( O, Properties ) — ES2024 §20.1.2.2
///
///   1. If O is not an Object and O is not null, throw a TypeError exception.
///   2. Let obj be OrdinaryObjectCreate(O).
///   3. If Properties is not undefined, then
///     a. Return ? ObjectDefineProperties(obj, Properties).
///   4. Return obj.
///
fn create(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(proto_val, props_val) = helpers.two_args_or_undefined(args)
  // Step 1: If O is not an Object and O is not null, throw a TypeError.
  let proto = case proto_val {
    JsObject(ref) -> Ok(Some(ref))
    JsNull -> Ok(None)
    _ -> Error(Nil)
  }
  case proto {
    Error(Nil) ->
      state.type_error(state, "Object prototype may only be an Object or null")
    Ok(prototype) -> {
      // Step 2: Let obj be OrdinaryObjectCreate(O).
      let #(heap, ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: OrdinaryObject,
            properties: dict.new(),
            symbol_properties: [],
            elements: elements.new(),
            prototype:,
            extensible: True,
          ),
        )
      let state = State(..state, heap:)
      // Steps 3-4: If Properties is not undefined, return
      // ObjectDefineProperties(obj, Properties); otherwise return obj.
      case props_val {
        JsUndefined -> #(state, Ok(JsObject(ref)))
        _ -> define_properties_on(state, ref, props_val)
      }
    }
  }
}

/// Object.defineProperties ( O, Properties ) — ES2024 §20.1.2.3
///
///   1. If O is not an Object, throw a TypeError exception.
///   2. Return ? ObjectDefineProperties(O, Properties).
///
/// ObjectDefineProperties is the abstract operation at §20.1.2.3.1 —
/// implemented by define_properties_on + define_props_loop below.
fn define_properties(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(target, props_val) = helpers.two_args_or_undefined(args)
  // Step 1: If O is not an Object, throw a TypeError exception.
  case target {
    // Step 2: Return ? ObjectDefineProperties(O, Properties).
    JsObject(target_ref) -> define_properties_on(state, target_ref, props_val)
    _ -> state.type_error(state, "Object.defineProperties called on non-object")
  }
}

/// ObjectDefineProperties ( O, Properties ) — ES2024 §20.1.2.3.1
///
///   1. Let props be ? ToObject(Properties).
///   2. Let keys be ? props.[[OwnPropertyKeys]]().
///   3. Let descriptors be a new empty List.
///   4. For each element nextKey of keys, do
///     a. Let propDesc be ? props.[[GetOwnProperty]](nextKey).
///     b. If propDesc is not undefined and propDesc.[[Enumerable]] is true, then
///       i.  Let descObj be ? Get(props, nextKey).
///       ii. Let desc be ? ToPropertyDescriptor(descObj).
///       iii. Append the Record { [[Key]]: nextKey, [[Descriptor]]: desc } to descriptors.
///   5. For each element pair of descriptors, do
///     a. Perform ? DefinePropertyOrThrow(O, pair.[[Key]], pair.[[Descriptor]]).
///   6. Return O.
///
fn define_properties_on(
  state: State(host),
  target_ref: Ref,
  props_val: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  case props_val {
    JsObject(props_ref) -> {
      // Step 2: keys = ? props.[[OwnPropertyKeys]]() — trap-aware AND
      // symbol-inclusive, so `Object.create(p, {[Symbol.x]: {..}})` defines
      // the symbol property and a Proxy props object goes through its
      // ownKeys trap.
      use keys, state <- state.try_op(define_own.own_property_keys(
        state,
        props_ref,
      ))
      // Step 4: read + parse EVERY descriptor first — nothing is defined on
      // the target until all of them validate.
      use descriptors, state <- state.try_op(
        collect_descriptors(state, props_ref, keys, []),
      )
      // Step 5: only now apply them, in key order.
      apply_descriptors(state, target_ref, descriptors)
    }
    // Step 1: ToObject throws TypeError on null/undefined.
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    // Step 1: ToObject on primitives → wrapper with no own enumerable props → no-op.
    _ -> #(state, Ok(JsObject(target_ref)))
  }
}

/// ObjectDefineProperties §20.1.2.3.1 STEP 4 — read and parse every descriptor
/// BEFORE any of them is applied. This phase separation is observable: if a
/// later key's descriptor is invalid (`{a: {value: 1}, b: 1}`), the whole call
/// throws with `a` still undefined on the target.
///
/// `remaining` is the [[OwnPropertyKeys]] result — String and Symbol values.
/// For each remaining key:
///   Step 4.a: Let propDesc be ? props.[[GetOwnProperty]](nextKey) — trap-aware.
///   Step 4.b: skip when propDesc is undefined or not enumerable.
///   Step 4.b.i: Let descObj be ? Get(props, nextKey).
///   Step 4.b.ii: Let desc be ? ToPropertyDescriptor(descObj) — throws if
///     descObj is not an Object (define_own.parse_descriptor step 1).
///   Step 4.b.iii: Append { [[Key]], [[Descriptor]] } to descriptors.
fn collect_descriptors(
  state: State(host),
  props_ref: Ref,
  remaining: List(ObjectKey),
  acc: List(#(ObjectKey, ParsedDesc)),
) -> Result(
  #(List(#(ObjectKey, ParsedDesc)), State(host)),
  #(JsValue, State(host)),
) {
  case remaining {
    [] -> Ok(#(list.reverse(acc), state))
    [dkey, ..rest] -> {
      // Step 4.a: propDesc = ? props.[[GetOwnProperty]](nextKey) — trap-aware.
      use #(prop, state) <- result.try(define_own.own_property_keyed(
        state,
        props_ref,
        dkey,
      ))
      let enumerable =
        option.map(prop, value.prop_enumerable) |> option.unwrap(False)
      case enumerable {
        // Step 4.b: propDesc undefined or not enumerable → skip.
        False -> collect_descriptors(state, props_ref, rest, acc)
        True -> {
          // Step 4.b.i: descObj = Get(props, nextKey) — a real [[Get]], so
          // accessor descriptors have their getter invoked (and symbol keys
          // reach define_own.define_parsed instead of being silently dropped).
          use #(desc_val, state) <- result.try(object.get_prop_value(
            state,
            props_ref,
            dkey,
            JsObject(props_ref),
          ))
          // Step 4.b.ii: ToPropertyDescriptor(descObj).
          use #(parsed, state) <- result.try(define_own.parse_descriptor(
            state,
            desc_val,
          ))
          // Step 4.b.iii.
          collect_descriptors(state, props_ref, rest, [#(dkey, parsed), ..acc])
        }
      }
    }
  }
}

/// ObjectDefineProperties §20.1.2.3.1 STEP 5-6: apply the already-parsed
/// descriptors in key order, then return O.
fn apply_descriptors(
  state: State(host),
  target_ref: Ref,
  descriptors: List(#(ObjectKey, ParsedDesc)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case descriptors {
    // Step 6: Return O (all descriptors applied).
    [] -> #(state, Ok(JsObject(target_ref)))
    [#(dkey, parsed), ..rest] -> {
      // Step 5.a: Perform ? DefinePropertyOrThrow(O, key, desc).
      use state <- state.try_state(define_own.apply_parsed_or_throw(
        state,
        target_ref,
        dkey,
        parsed,
      ))
      apply_descriptors(state, target_ref, rest)
    }
  }
}

/// Object.assign ( target, ...sources ) — ES2024 §20.1.2.1
///
///   1. Let to be ? ToObject(target).
///   2. If only one argument was passed, return to.
///   3. For each element nextSource of sources, do
///     a. If nextSource is neither undefined nor null, then
///       i.   Let from be ! ToObject(nextSource).
///       ii.  Let keys be ? from.[[OwnPropertyKeys]]().
///       iii. For each element nextKey of keys, do
///         1. Let desc be ? from.[[GetOwnProperty]](nextKey).
///         2. If desc is not undefined and desc.[[Enumerable]] is true, then
///           a. Let propValue be ? Get(from, nextKey).
///           b. Perform ? Set(to, nextKey, propValue, true).
///   4. Return to.
///
fn assign(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case args {
    // Step 1: ToObject throws TypeError on null/undefined.
    [] | [JsNull, ..] | [JsUndefined, ..] ->
      state.type_error(state, cannot_convert)
    [target, ..sources] -> {
      // Step 1: Let to be ? ToObject(target).
      // For Objects: identity. For primitives: create a wrapper object.
      case common.to_object(state.heap, state.builtins, target) {
        None -> state.type_error(state, cannot_convert)
        Some(#(heap, target_ref)) -> {
          let state = State(..state, heap:)
          // Steps 3-4: Process each source, then return to.
          use state <- state.try_state(assign_sources(
            state,
            target_ref,
            sources,
          ))
          #(state, Ok(JsObject(target_ref)))
        }
      }
    }
  }
}

/// Loop helper for Object.assign (§20.1.2.1 step 3):
///   "For each element nextSource of sources, do ..."
fn assign_sources(
  state: State(host),
  target_ref: Ref,
  sources: List(JsValue),
) -> Result(State(host), #(JsValue, State(host))) {
  case sources {
    [] -> Ok(state)
    [source, ..rest] -> {
      use state <- result.try(assign_source(state, target_ref, source))
      assign_sources(state, target_ref, rest)
    }
  }
}

/// Process one source for Object.assign (§20.1.2.1 step 3.a):
///   "If nextSource is neither undefined nor null, then ..."
///
/// Step 3.a.i:   Let from be ! ToObject(nextSource).
/// Step 3.a.ii:  Let keys be ? from.[[OwnPropertyKeys]]().
/// Step 3.a.iii: For each element nextKey of keys, do ...
///
/// Per §9.1.11.1 OrdinaryOwnPropertyKeys, [[OwnPropertyKeys]] returns:
///   1. Integer index keys in ascending numeric order.
///   2. Non-index string keys in creation order.
///   3. Symbol keys in creation order.
/// String keys are copied first, then symbol keys.
///
fn assign_source(
  state: State(host),
  target_ref: Ref,
  source: JsValue,
) -> Result(State(host), #(JsValue, State(host))) {
  case source {
    // Step 3.a.ii-iii: [[OwnPropertyKeys]] / [[GetOwnProperty]] / [[Get]] all
    // run through the traps for a proxy source, and the per-key descriptor read
    // is re-done for ordinary sources too — a getter run for an earlier key can
    // make a later key non-enumerable or delete it.
    JsObject(src_ref) as receiver -> {
      use #(keys, state) <- result.try(define_own.own_property_keys(
        state,
        src_ref,
      ))
      assign_own_keys(state, target_ref, src_ref, receiver, keys)
    }
    // String sources: each index character is an enumerable own property.
    // "length" is own but non-enumerable, so it's excluded.
    JsString(s) ->
      assign_string_chars(state, target_ref, string_own_index_values(s), 0)
    // Step 3.a: null/undefined are skipped; number/boolean/symbol wrappers have
    // no own enumerable string-keyed properties.
    _ -> Ok(state)
  }
}

/// Key-copy loop for Object.assign (§20.1.2.1 step 3.a.iii):
///
///   For each element nextKey of keys, do
///     1. Let desc be ? from.[[GetOwnProperty]](nextKey).
///     2. If desc is not undefined and desc.[[Enumerable]] is true, then
///       a. Let propValue be ? Get(from, nextKey).
///       b. Perform ? Set(to, nextKey, propValue, true).
///
/// Copy string characters as indexed properties to the target object.
/// Used by Object.assign when a source is a string primitive.
fn assign_string_chars(
  state: State(host),
  target_ref: Ref,
  chars: List(String),
  idx: Int,
) -> Result(State(host), #(JsValue, State(host))) {
  case chars {
    [] -> Ok(state)
    [ch, ..rest] -> {
      // Step 2.b: Perform ? Set(to, ToString(idx), ch, true) — the throwing
      // [[Set]] path (invokes setters; a false result is a TypeError), not
      // the raw own-property store.
      use #(state, ok) <- result.try(object.set_value(
        state,
        target_ref,
        Index(idx),
        JsString(ch),
        JsObject(target_ref),
      ))
      use Nil <- result.try(case ok {
        True -> Ok(Nil)
        False -> assign_set_failed(state, int.to_string(idx))
      })
      assign_string_chars(state, target_ref, rest, idx + 1)
    }
  }
}

/// §20.1.2.1 step 3.a.iii.2.b `Perform ? Set(to, nextKey, propValue, true)`
/// with a false result — the TypeError required by the `throw` flag.
fn assign_set_failed(
  state: State(host),
  key: String,
) -> Result(a, #(JsValue, State(host))) {
  Error(state.type_error_value(
    state,
    "Cannot assign to read only property '" <> key <> "' of object",
  ))
}

/// Object.assign step 3.a.iii — per key of the source's [[OwnPropertyKeys]]:
///   1. Let desc be ? from.[[GetOwnProperty]](nextKey).   (descriptor / trap)
///   2. If desc is not undefined and desc.[[Enumerable]] is true, then
///     a. Let propValue be ? Get(from, nextKey).          (get / get trap)
///     b. Perform ? Set(to, nextKey, propValue, true).
///
/// Every source runs this loop, proxy or not: the descriptor read must be
/// re-done per key, after the previous key's getter/setter had its chance to
/// delete a later key or flip it non-enumerable.
fn assign_own_keys(
  state: State(host),
  target_ref: Ref,
  src_ref: Ref,
  receiver: JsValue,
  keys: List(ObjectKey),
) -> Result(State(host), #(JsValue, State(host))) {
  case keys {
    [] -> Ok(state)
    [k, ..rest] -> {
      // Step 1: desc = ? from.[[GetOwnProperty]](nextKey) — trap-aware.
      use #(prop, state) <- result.try(define_own.own_property_keyed(
        state,
        src_ref,
        k,
      ))
      let enumerable =
        option.map(prop, value.prop_enumerable) |> option.unwrap(False)
      use state <- result.try(case enumerable, k {
        // Not enumerable / not own — skipped per step 2.
        False, _ -> Ok(state)
        True, StringPropKey(pkey:, display: s) -> {
          // Step 2.a: Let propValue be ? Get(from, nextKey).
          use #(val, state) <- result.try(object.get_value(
            state,
            src_ref,
            pkey,
            receiver,
          ))
          // Step 2.b: Perform ? Set(to, nextKey, propValue, true) — a
          // false [[Set]] throws.
          use #(state, ok) <- result.try(object.set_value(
            state,
            target_ref,
            pkey,
            val,
            JsObject(target_ref),
          ))
          case ok {
            True -> Ok(state)
            False -> assign_set_failed(state, s)
          }
        }
        True, SymbolPropKey(sym) -> {
          use #(val, state) <- result.try(object.get_symbol_value(
            state,
            src_ref,
            sym,
            receiver,
          ))
          use #(state, ok) <- result.try(object.set_symbol_value(
            state,
            target_ref,
            sym,
            val,
            JsObject(target_ref),
          ))
          case ok {
            True -> Ok(state)
            False -> assign_set_failed(state, symbol.descriptive_string(sym))
          }
        }
      })
      assign_own_keys(state, target_ref, src_ref, receiver, rest)
    }
  }
}

/// §7.3.25 CopyDataProperties for a possibly-proxy source — routes
/// [[OwnPropertyKeys]] / [[GetOwnProperty]] / [[Get]] through the proxy traps
/// so `{...proxy}`, destructuring rest, and spread-like callers observe them.
/// Ordinary sources delegate to object.copy_data_properties_excluding.
pub fn copy_data_properties_stateful(
  state: State(host),
  target_ref: Ref,
  source: JsValue,
  excluded_keys: set.Set(key.PropertyKey),
  excluded_syms: set.Set(value.SymbolId),
) -> Result(State(host), #(JsValue, State(host))) {
  case source {
    JsObject(src_ref) ->
      case object.as_proxy(state.heap, src_ref) {
        Some(_) -> {
          // Step 3: Let keys be ? from.[[OwnPropertyKeys]]() — ownKeys trap
          // (throws TypeError when the proxy is revoked).
          use #(keys, state) <- result.try(define_own.own_property_keys(
            state,
            src_ref,
          ))
          copy_proxy_keys(
            state,
            src_ref,
            target_ref,
            keys,
            excluded_keys,
            excluded_syms,
          )
        }
        None ->
          object.copy_data_properties_excluding(
            state,
            target_ref,
            source,
            excluded_keys,
            excluded_syms,
          )
      }
    _ ->
      object.copy_data_properties_excluding(
        state,
        target_ref,
        source,
        excluded_keys,
        excluded_syms,
      )
  }
}

/// §7.3.25 CopyDataProperties step 4 for a proxy source — per trap key:
/// exclusion check (4.b), [[GetOwnProperty]] trap + enumerable filter (4.c.i-ii),
/// Get trap (4.c.ii.1), CreateDataProperty on the ordinary target (4.c.ii.2).
fn copy_proxy_keys(
  state: State(host),
  src_ref: Ref,
  target_ref: Ref,
  keys: List(ObjectKey),
  excluded_keys: set.Set(key.PropertyKey),
  excluded_syms: set.Set(value.SymbolId),
) -> Result(State(host), #(JsValue, State(host))) {
  case keys {
    [] -> Ok(state)
    [k, ..rest] -> {
      // Step 4.a-b: skip keys named in excludedItems.
      let excluded = case k {
        StringPropKey(pkey:, ..) -> set.contains(excluded_keys, pkey)
        SymbolPropKey(sym) -> set.contains(excluded_syms, sym)
      }
      use state <- result.try(case excluded {
        True -> Ok(state)
        False -> copy_one_proxy_key(state, src_ref, target_ref, k)
      })
      copy_proxy_keys(
        state,
        src_ref,
        target_ref,
        rest,
        excluded_keys,
        excluded_syms,
      )
    }
  }
}

/// CopyDataProperties step 4.c for one trap-provided key.
fn copy_one_proxy_key(
  state: State(host),
  src_ref: Ref,
  target_ref: Ref,
  k: ObjectKey,
) -> Result(State(host), #(JsValue, State(host))) {
  // Step 4.c.i: Let desc be ? from.[[GetOwnProperty]](nextKey) — trap-aware.
  use #(prop, state) <- result.try(define_own.own_property_keyed(
    state,
    src_ref,
    k,
  ))
  let enumerable =
    option.map(prop, value.prop_enumerable) |> option.unwrap(False)
  case enumerable, k {
    False, _ -> Ok(state)
    True, StringPropKey(pkey:, ..) -> {
      // Step 4.c.ii.1: Let propValue be ? Get(from, nextKey) — get trap.
      use #(val, state) <- result.map(object.get_value(
        state,
        src_ref,
        pkey,
        JsObject(src_ref),
      ))
      // Step 4.c.ii.2: CreateDataPropertyOrThrow(target, nextKey, propValue).
      let heap = object.create_data_property(state.heap, target_ref, pkey, val)
      State(..state, heap:)
    }
    True, SymbolPropKey(sym) -> {
      use #(val, state) <- result.map(object.get_symbol_value(
        state,
        src_ref,
        sym,
        JsObject(src_ref),
      ))
      let heap =
        object.define_symbol_property(
          state.heap,
          target_ref,
          sym,
          value.data_property(val),
        )
      State(..state, heap:)
    }
  }
}

/// Object.is ( value1, value2 ) — ES2024 §20.1.2.12
///
///   1. Return SameValue(value1, value2).
///
/// SameValue ( x, y ) — §6.1.6.1.14:
///   1. If Type(x) is not Type(y), return false.
///   2. If x is a Number, then
///     a. Return Number::sameValue(x, y).
///        (NaN === NaN is true; +0 !== -0)
///   3. Return SameValueNonNumber(x, y).
///
/// Delegates to `value.same_value` — the engine's single SameValue — rather
/// than leaning on structural `==`, whose ±0 and string-representation
/// behaviour is an implementation detail, not a spec guarantee.
fn is(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(a, b) = helpers.two_args_or_undefined(args)
  // Step 1: Return SameValue(value1, value2).
  #(state, Ok(JsBool(value.same_value(a, b))))
}

/// Object.hasOwn ( O, P ) — ES2024 §20.1.2.10
///
///   1. Let obj be ? ToObject(O).
///   2. Let key be ? ToPropertyKey(P).
///   3. Return ? HasOwnProperty(obj, key).
///
/// HasOwnProperty ( O, P ) — §7.3.12:
///   1. Assert: Type(O) is Object.
///   2. Assert: IsPropertyKey(P) is true.
///   3. Let desc be ? O.[[GetOwnProperty]](P).
///   4. If desc is undefined, return false.
///   5. Return true.
///
/// Note: The spec order is ToObject (step 1) before ToPropertyKey (step 2).
/// This differs from Object.prototype.hasOwnProperty which does ToPropertyKey
/// first (§20.1.3.2 step 1) then ToObject (step 2).
///
fn has_own(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(target, key_val) = helpers.two_args_or_undefined(args)
  case target {
    JsObject(ref) -> {
      // Step 1: ToObject(O) — identity for objects.
      // Steps 2-3: ToPropertyKey + HasOwnProperty(obj, key) — trap-aware.
      use key, state <- try_to_property_key(state, key_val)
      use prop_opt, state <- state.try_op(define_own.own_property_keyed(
        state,
        ref,
        key,
      ))
      #(state, Ok(JsBool(option.is_some(prop_opt))))
    }
    // Step 1: ToObject throws TypeError on null/undefined.
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    // String primitives: own keys are index chars + "length" — the shared
    // string-exotic own-property table decides.
    JsString(s) -> {
      use key, state <- try_to_property_key(state, key_val)
      #(
        state,
        Ok(
          JsBool(option.is_some(define_own.string_exotic_own_property(s, key))),
        ),
      )
    }
    // Number/Boolean/Symbol/BigInt: their wrappers have no own properties,
    // but Step 2 (ToPropertyKey) is observable — a key with a throwing
    // toString/@@toPrimitive must throw before we return false.
    _ -> {
      use _key, state <- try_to_property_key(state, key_val)
      #(state, Ok(JsBool(False)))
    }
  }
}

/// Object.getPrototypeOf ( O ) — ES2024 §20.1.2.7
///
///   1. Let obj be ? ToObject(O).
///   2. Return ? obj.[[GetPrototypeOf]]().
///
/// [[GetPrototypeOf]] for ordinary objects is OrdinaryGetPrototypeOf (§10.1.1.1):
///   1. Return O.[[Prototype]].
///
fn get_prototype_of(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case first_arg_or_undefined(args) {
    JsObject(ref) -> {
      // Step 2: obj.[[GetPrototypeOf]]() — OrdinaryGetPrototypeOf (§10.1.1.1)
      // for ordinary objects, the getPrototypeOf trap (§10.5.1) for proxies.
      use proto, state <- state.try_op(object.get_prototype_of_stateful(
        state,
        ref,
      ))
      #(state, Ok(proto))
    }
    // Step 1: ToObject (§7.1.18) — null/undefined throw TypeError.
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    // Step 1 cont: ToObject on primitives — return the wrapper's prototype.
    // §7.1.18: Number → NumberObject, String → StringObject, Boolean → BooleanObject
    // We skip allocating a wrapper and return the prototype directly.
    JsNumber(_) -> #(state, Ok(JsObject(state.builtins.number.prototype)))
    JsString(_) -> #(state, Ok(JsObject(state.builtins.string.prototype)))
    JsBool(_) -> #(state, Ok(JsObject(state.builtins.boolean.prototype)))
    value.JsSymbol(_) -> #(state, Ok(JsObject(state.builtins.symbol_proto)))
    value.JsBigInt(_) -> #(state, Ok(JsObject(state.builtins.bigint.prototype)))
    _ -> #(state, Ok(JsObject(state.builtins.object.prototype)))
  }
}

/// Object.setPrototypeOf ( O, proto ) — ES2024 §20.1.2.21
///
///   1. Set O to ? RequireObjectCoercible(O).
///   2. If proto is not an Object and proto is not null, throw a TypeError.
///   3. If O is not an Object, return O.
///   4. Let status be ? O.[[SetPrototypeOf]](proto).
///   5. If status is false, throw a TypeError ("cyclic proto" or non-extensible).
///   6. Return O.
///
/// [[SetPrototypeOf]] delegates to OrdinarySetPrototypeOf (§10.1.2.1):
///   1. Let current be O.[[Prototype]].
///   2. If SameValue(V, current) is true, return true.
///   3. Let extensible be O.[[Extensible]].
///   4. If extensible is false, return false.
///   5. Let p be V.
///   6. Let done be false.
///   7. Repeat, while done is false,
///      a. If p is null, set done to true.
///      b. Else if SameValue(p, O) is true, return false.
///      c. Else if p.[[GetPrototypeOf]] is not ordinary, set done to true.
///      d. Else set p to p.[[Prototype]].
///   8. Set O.[[Prototype]] to V.
///   9. Return true.
///
fn set_prototype_of(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(target, proto_val) = helpers.two_args_or_undefined(args)
  // §20.1.2.21 step 2: If proto is not an Object and proto is not null, throw TypeError.
  let proto = case proto_val {
    JsObject(ref) -> Ok(Some(ref))
    JsNull -> Ok(None)
    _ -> Error(Nil)
  }
  case target, proto {
    // §20.1.2.21 step 1: RequireObjectCoercible — null/undefined throw TypeError.
    JsNull, _ | JsUndefined, _ -> state.type_error(state, cannot_convert)
    _, Error(_) ->
      // §20.1.2.21 step 2: proto is not Object or null.
      state.type_error(state, "Object prototype may only be an Object or null")
    JsObject(ref), Ok(new_proto) -> {
      // §20.1.2.21 steps 4-6: O.[[SetPrototypeOf]](proto); throw if false.
      use #(state, status) <- try_op_pair(define_own.set_prototype_of_stateful(
        state,
        ref,
        new_proto,
      ))
      case status {
        Ok(Nil) -> #(state, Ok(target))
        Error(fail) ->
          state.type_error(state, define_own.set_proto_fail_message(fail))
      }
    }
    // §20.1.2.21 step 3: If O is not an Object, return O.
    _, Ok(_) -> #(state, Ok(target))
  }
}

/// Which accessor slot an Annex B §B.2.2 legacy method targets.
type AccessorKind {
  AsGetter
  AsSetter
}

/// set Object.prototype.__proto__ ( proto ) — Annex B §B.2.2.1.2
///
///   1. Let O be ? RequireObjectCoercible(this value).
///   2. If proto is not an Object and proto is not null, return undefined.
///   3. If O is not an Object, return undefined.
///   4. Let status be ? O.[[SetPrototypeOf]](proto).
///   5. If status is false, throw a TypeError exception.
///   6. Return undefined.
fn proto_setter(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let proto_val = first_arg_or_undefined(args)
  case this, proto_val {
    // Step 1: RequireObjectCoercible — null/undefined throw TypeError.
    JsNull, _ | JsUndefined, _ -> state.type_error(state, cannot_convert)
    // Steps 4-6: both O and proto qualify — delegate to [[SetPrototypeOf]]
    // (set_prototype_of shares the ordinary/proxy/immutable logic) and
    // discard its return value in favour of undefined.
    JsObject(_), JsObject(_) | JsObject(_), JsNull -> {
      let #(state, res) = set_prototype_of([this, proto_val], state)
      case res {
        Ok(_) -> #(state, Ok(JsUndefined))
        Error(thrown) -> #(state, Error(thrown))
      }
    }
    // Step 2: proto is neither an Object nor null — silently return undefined.
    // Step 3: O is a primitive — silently return undefined.
    _, _ -> #(state, Ok(JsUndefined))
  }
}

/// Object.prototype.__defineGetter__ / __defineSetter__ — Annex B §B.2.2.2-3
///
///   1. Let O be ? ToObject(this value).
///   2. If IsCallable(getter/setter) is false, throw a TypeError exception.
///   3. Let desc be PropertyDescriptor { [[Get]]/[[Set]]: fn,
///      [[Enumerable]]: true, [[Configurable]]: true }.
///   4. Let key be ? ToPropertyKey(P).
///   5. Perform ? DefinePropertyOrThrow(O, key, desc).
///   6. Return undefined.
fn define_getter_setter(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
  kind: AccessorKind,
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(key_val, accessor) = helpers.two_args_or_undefined(args)
  // Step 1: ToObject(this value) — null/undefined throw TypeError.
  case common.to_object(state.heap, state.builtins, this) {
    None -> state.type_error(state, cannot_convert)
    Some(#(heap, ref)) -> {
      let state = State(..state, heap:)
      // Step 2: IsCallable check happens BEFORE ToPropertyKey (step 4).
      case object.value_is_callable(state.heap, accessor) {
        False ->
          case kind {
            AsGetter -> state.type_error(state, "Getter must be a function")
            AsSetter -> state.type_error(state, "Setter must be a function")
          }
        True -> {
          // Step 4: ToPropertyKey(P) — may invoke user code and throw.
          use dkey, state <- try_to_property_key(state, key_val)
          // Steps 3 + 5: DefinePropertyOrThrow with the accessor descriptor.
          let parsed = case kind {
            AsGetter ->
              ParsedDesc(
                get: Some(accessor),
                set: None,
                value: None,
                writable: None,
                enumerable: Some(True),
                configurable: Some(True),
              )
            AsSetter ->
              ParsedDesc(
                get: None,
                set: Some(accessor),
                value: None,
                writable: None,
                enumerable: Some(True),
                configurable: Some(True),
              )
          }
          case define_own.define_parsed(state, ref, dkey, parsed) {
            // Step 6: Return undefined.
            Ok(state) -> #(state, Ok(JsUndefined))
            Error(#(failure, state)) -> #(
              state,
              Error(define_own.define_failure_value(failure)),
            )
          }
        }
      }
    }
  }
}

/// Object.prototype.__lookupGetter__ / __lookupSetter__ — Annex B §B.2.2.4-5
///
///   1. Let O be ? ToObject(this value).
///   2. Let key be ? ToPropertyKey(P).
///   3. Repeat,
///      a. Let desc be ? O.[[GetOwnProperty]](key).
///      b. If desc is not undefined, then
///         i.  If IsAccessorDescriptor(desc) is true, return desc.[[Get]]/[[Set]].
///         ii. Return undefined.
///      c. Set O to ? O.[[GetPrototypeOf]]().
///      d. If O is null, return undefined.
fn lookup_getter_setter(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
  kind: AccessorKind,
) -> #(State(host), Result(JsValue, JsValue)) {
  let key_val = first_arg_or_undefined(args)
  // Step 1: ToObject(this value) — null/undefined throw TypeError.
  case common.to_object(state.heap, state.builtins, this) {
    None -> state.type_error(state, cannot_convert)
    Some(#(heap, ref)) -> {
      let state = State(..state, heap:)
      // Step 2: ToPropertyKey(P) — may invoke user code and throw.
      use dkey, state <- try_to_property_key(state, key_val)
      lookup_accessor_chain(state, ref, dkey, kind)
    }
  }
}

/// Step 3 of §B.2.2.4-5 — walk the prototype chain with the trap-aware
/// [[GetOwnProperty]] / [[GetPrototypeOf]] so proxy traps (and their abrupt
/// completions) are honoured.
fn lookup_accessor_chain(
  state: State(host),
  ref: Ref,
  dkey: ObjectKey,
  kind: AccessorKind,
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 3.a: desc = ? O.[[GetOwnProperty]](key).
  case define_own.own_property_keyed(state, ref, dkey) {
    Error(#(thrown, state)) -> #(state, Error(thrown))
    // Step 3.b.i: accessor descriptor — return its [[Get]]/[[Set]] slot.
    Ok(#(Some(AccessorProperty(get:, set:, ..)), state)) -> {
      let slot = case kind {
        AsGetter -> get
        AsSetter -> set
      }
      #(state, Ok(option.unwrap(slot, JsUndefined)))
    }
    // Step 3.b.ii: data descriptor shadows the chain — return undefined.
    Ok(#(Some(DataProperty(..)), state)) -> #(state, Ok(JsUndefined))
    Ok(#(None, state)) -> {
      // Step 3.c: O = ? O.[[GetPrototypeOf]]().
      use proto, state <- state.try_op(object.get_prototype_of_stateful(
        state,
        ref,
      ))
      case proto {
        JsObject(proto_ref) ->
          lookup_accessor_chain(state, proto_ref, dkey, kind)
        // Step 3.d: O is null — return undefined.
        _ -> #(state, Ok(JsUndefined))
      }
    }
  }
}

/// The integrity level requested by Object.freeze/seal and tested by
/// Object.isFrozen/isSealed — §7.3.16 / §7.3.17's `level` parameter.
type IntegrityLevel {
  Sealed
  Frozen
}

/// The Object method name for a level, for TypeError messages.
fn integrity_level_verb(level: IntegrityLevel) -> String {
  case level {
    Sealed -> "seal"
    Frozen -> "freeze"
  }
}

/// §7.3.16 step 7.a / step 8.a.ii.1: { [[Configurable]]: false }.
const seal_level_desc = ParsedDesc(
  get: None,
  set: None,
  value: None,
  writable: None,
  enumerable: None,
  configurable: Some(False),
)

/// §7.3.16 step 8.a.ii.2: { [[Configurable]]: false, [[Writable]]: false }.
const freeze_data_desc = ParsedDesc(
  get: None,
  set: None,
  value: None,
  writable: Some(False),
  enumerable: None,
  configurable: Some(False),
)

/// §7.3.16 step 8.a.ii: the "frozen" descriptor for a current own property —
/// accessors only lose [[Configurable]], data properties also lose
/// [[Writable]].
fn frozen_level_desc(cur: value.Property) -> ParsedDesc {
  case cur {
    AccessorProperty(..) -> seal_level_desc
    DataProperty(..) -> freeze_data_desc
  }
}

/// SetIntegrityLevel ( O, level ) — ES2024 §7.3.16
///
/// Shared core of Object.freeze and Object.seal, routed entirely through the
/// trap-aware internal methods: a Proxy fires its preventExtensions, ownKeys
/// and defineProperty traps, and exotic receivers (arrays with dense
/// elements, string wrappers, typed arrays, module namespaces) get their own
/// [[OwnPropertyKeys]] / [[DefineOwnProperty]] semantics instead of a raw
/// rewrite of the named-property dict.
///
///   3. Let status be ? O.[[PreventExtensions]]().
///   4. If status is false, return false (→ TypeError, freeze/seal step 3).
///   6. Let keys be ? O.[[OwnPropertyKeys]]().
///   7-8. For each key, ? DefinePropertyOrThrow with the level descriptor.
///   9. Return true.
fn set_integrity_level(
  args: List(JsValue),
  state: State(host),
  level: IntegrityLevel,
) -> #(State(host), Result(JsValue, JsValue)) {
  let target = first_arg_or_undefined(args)
  case target {
    JsObject(ref) -> {
      // Step 3: ? O.[[PreventExtensions]]() — trap-aware.
      use #(state, ok) <- try_op_pair(object.prevent_extensions_stateful(
        state,
        ref,
      ))
      case ok {
        // Step 4 → §20.1.2.6/.20 step 3: status false → TypeError.
        False ->
          state.type_error(
            state,
            "Object." <> integrity_level_verb(level) <> " returned false",
          )
        True -> {
          // Step 6: ? O.[[OwnPropertyKeys]]() — trap-aware.
          use keys, state <- state.try_op(define_own.own_property_keys(
            state,
            ref,
          ))
          // Steps 7-8: ? DefinePropertyOrThrow per key.
          use state <- state.try_state(set_integrity_keys(
            state,
            ref,
            keys,
            level,
          ))
          // §20.1.2.6/§20.1.2.20 step 4: Return O.
          #(state, Ok(target))
        }
      }
    }
    // §20.1.2.6/§20.1.2.20 step 1: If O is not an Object, return O.
    _ -> #(state, Ok(target))
  }
}

/// §7.3.16 steps 7-8: DefinePropertyOrThrow(O, k, <level descriptor>) for
/// every own key, through the trap-aware [[GetOwnProperty]] /
/// [[DefineOwnProperty]] funnels.
///
/// Private elements never appear here — [[OwnPropertyKeys]]
/// (define_own.own_property_keys → define_own.collect_own_keys) excludes private names, so they
/// stay writable after freeze without a per-key filter.
fn set_integrity_keys(
  state: State(host),
  ref: Ref,
  keys: List(ObjectKey),
  level: IntegrityLevel,
) -> Result(State(host), #(JsValue, State(host))) {
  case keys {
    [] -> Ok(state)
    [dkey, ..rest] -> {
      use #(desc, state) <- result.try(case level {
        // Step 7.a: sealed → { [[Configurable]]: false } for every key.
        Sealed -> Ok(#(Some(seal_level_desc), state))
        // Steps 8.a.i-ii: frozen reads the current descriptor first
        // (trap-aware); an absent key (e.g. one an ownKeys trap invented) is
        // skipped per step 8.a.ii.
        Frozen -> {
          use #(cur, state) <- result.map(define_own.own_property_keyed(
            state,
            ref,
            dkey,
          ))
          #(option.map(cur, frozen_level_desc), state)
        }
      })
      use state <- result.try(case desc {
        None -> Ok(state)
        // Step 7.a.i / 8.a.ii.3: ? DefinePropertyOrThrow(O, k, desc).
        Some(parsed) ->
          define_own.define_parsed(state, ref, dkey, parsed)
          |> result.map_error(define_own.as_define_throw)
      })
      set_integrity_keys(state, ref, rest, level)
    }
  }
}

/// Object.freeze ( O ) — ES2024 §20.1.2.6
///
///   1. If O is not an Object, return O.
///   2. Let status be ? SetIntegrityLevel(O, frozen).
///   3. If status is false, throw a TypeError exception.
///   4. Return O.
fn freeze(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  set_integrity_level(args, state, Frozen)
}

/// Object.preventExtensions ( O ) — ES2024 §20.1.2.17
///
///   1. If O is not an Object, return O.
///   2. Let status be ? O.[[PreventExtensions]]().
///   3. If status is false, throw a TypeError exception.
///   4. Return O.
///
/// [[PreventExtensions]] for ordinary objects — §10.1.4.1:
///   1. Set O.[[Extensible]] to false.
///   2. Return true.
///
/// Ordinary [[PreventExtensions]] always returns true, so step 3 never fires.
fn prevent_extensions(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let target = first_arg_or_undefined(args)
  case target {
    JsObject(ref) -> {
      // §20.1.2.17 step 2: O.[[PreventExtensions]]() — trap-aware.
      use #(state, ok) <- try_op_pair(object.prevent_extensions_stateful(
        state,
        ref,
      ))
      case ok {
        // §20.1.2.17 step 4: Return O.
        True -> #(state, Ok(target))
        // §20.1.2.17 step 3: status false → TypeError.
        False ->
          state.type_error(state, "Object.preventExtensions returned false")
      }
    }
    // §20.1.2.17 step 1: If O is not an Object, return O.
    _ -> #(state, Ok(target))
  }
}

/// CPS adapter for ops returning `Result(#(State, a), #(JsValue, State))`
/// (state-first tuple, unlike state.try_op's value-first).
fn try_op_pair(
  result: Result(#(State(host), a), #(JsValue, State(host))),
  cont: fn(#(State(host), a)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case result {
    Ok(pair) -> cont(pair)
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// TestIntegrityLevel ( O, level ) — ES2024 §7.3.17
///
/// Shared core of Object.isFrozen and Object.isSealed, routed entirely
/// through the trap-aware internal methods: a Proxy fires its isExtensible,
/// ownKeys and getOwnPropertyDescriptor traps, and exotic own properties
/// (dense array elements, string-exotic indices, typed-array elements) are
/// actually inspected instead of only the named-property dict.
///
///   3. Let extensible be ? IsExtensible(O).
///   4. If extensible is true, return false.
///   6. Let keys be ? O.[[OwnPropertyKeys]]().
///   7-8. For each key, ? O.[[GetOwnProperty]](k) must satisfy the level.
///   9. Return true.
fn test_integrity_level(
  args: List(JsValue),
  state: State(host),
  level: IntegrityLevel,
) -> #(State(host), Result(JsValue, JsValue)) {
  case first_arg_or_undefined(args) {
    JsObject(ref) -> {
      // Step 3: ? IsExtensible(O) — trap-aware.
      use ext, state <- state.try_op(object.is_extensible_stateful(state, ref))
      case ext {
        // Step 4: an extensible object is neither sealed nor frozen.
        True -> #(state, Ok(JsBool(False)))
        False -> {
          // Step 6: ? O.[[OwnPropertyKeys]]() — trap-aware.
          use keys, state <- state.try_op(define_own.own_property_keys(
            state,
            ref,
          ))
          // Steps 7-9.
          use ok, state <- state.try_op(keys_at_integrity_level(
            state,
            ref,
            keys,
            level,
          ))
          #(state, Ok(JsBool(ok)))
        }
      }
    }
    // §20.1.2.14/§20.1.2.15 step 1: If O is not an Object, return true.
    _ -> #(state, Ok(JsBool(True)))
  }
}

/// §7.3.17 steps 7-8: every reported own descriptor must satisfy the level.
/// Short-circuits on the first violation. Keys whose [[GetOwnProperty]]
/// reports undefined (e.g. one an ownKeys trap invented) are skipped per
/// step 8.b. Private elements never appear — [[OwnPropertyKeys]] excludes
/// private names, matching their exclusion in SetIntegrityLevel.
fn keys_at_integrity_level(
  state: State(host),
  ref: Ref,
  keys: List(ObjectKey),
  level: IntegrityLevel,
) -> Result(#(Bool, State(host)), #(JsValue, State(host))) {
  case keys {
    [] -> Ok(#(True, state))
    [dkey, ..rest] -> {
      // Step 8.a: ? O.[[GetOwnProperty]](k) — trap-aware.
      use #(cur, state) <- result.try(define_own.own_property_keyed(
        state,
        ref,
        dkey,
      ))
      case option.map(cur, prop_at_integrity_level(_, level)) {
        Some(False) -> Ok(#(False, state))
        Some(True) | None -> keys_at_integrity_level(state, ref, rest, level)
      }
    }
  }
}

/// §7.3.17 step 8.b: sealed needs [[Configurable]]: false; frozen
/// additionally needs [[Writable]]: false on data descriptors.
fn prop_at_integrity_level(
  prop: value.Property,
  level: IntegrityLevel,
) -> Bool {
  case level, prop {
    // Step 8.b.iii: [[Configurable]] must be false at either level.
    _, AccessorProperty(configurable:, ..) -> !configurable
    Sealed, DataProperty(configurable:, ..) -> !configurable
    // Step 8.b.ii.1: frozen data properties must also be non-writable.
    Frozen, DataProperty(configurable:, writable:, ..) ->
      !configurable && !writable
  }
}

/// Object.isFrozen ( O ) — ES2024 §20.1.2.14
///
///   1. If O is not an Object, return true.
///   2. Return ? TestIntegrityLevel(O, frozen).
fn is_frozen(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  test_integrity_level(args, state, Frozen)
}

/// Object.isExtensible ( O ) — ES2024 §20.1.2.13
///
///   1. If O is not an Object, return false.
///   2. Return ? IsExtensible(O).
///
/// IsExtensible ( O ) — §7.2.5:
///   1. Return ? O.[[IsExtensible]]().
///
/// [[IsExtensible]] for ordinary objects — §10.1.3.1:
///   1. Return O.[[Extensible]].
fn is_extensible(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case first_arg_or_undefined(args) {
    JsObject(ref) -> {
      // §20.1.2.13 step 2: IsExtensible(O) — trap-aware for proxies.
      use ext, state <- state.try_op(object.is_extensible_stateful(state, ref))
      #(state, Ok(JsBool(ext)))
    }
    // §20.1.2.13 step 1: If O is not an Object, return false.
    _ -> #(state, Ok(JsBool(False)))
  }
}

/// Object.seal ( O ) — ES2024 §20.1.2.20
///
///   1. If O is not an Object, return O.
///   2. Let status be ? SetIntegrityLevel(O, sealed).
///   3. If status is false, throw a TypeError exception.
///   4. Return O.
fn seal(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  set_integrity_level(args, state, Sealed)
}

/// Object.isSealed ( O ) — ES2024 §20.1.2.15
///
///   1. If O is not an Object, return true.
///   2. Return ? TestIntegrityLevel(O, sealed).
fn is_sealed(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  test_integrity_level(args, state, Sealed)
}

/// Object.fromEntries ( iterable ) — ES2024 §20.1.2.7
///
///   1. Perform ? RequireObjectCoercible(iterable).
///   2. Let obj be OrdinaryObjectCreate(%Object.prototype%).
///   3. Let closure be a new Abstract Closure that performs
///      ! CreateDataPropertyOrThrow(obj, ToPropertyKey(key), value).
///   4. Return ? AddEntriesFromIterable(obj, iterable, adder).
///
/// The §7.4.9 drain (GetIterator, per-entry object check, Get "0"/"1",
/// IteratorClose on any abrupt completion) is `iter_protocol`'s ONE loop, the
/// same one the Map/WeakMap constructors use — so `Object.fromEntries(map)`,
/// `Object.fromEntries(gen())` and a patched `@@iterator` all work. Only the
/// per-entry sink differs: this one is a Gleam-level
/// CreateDataPropertyOrThrow, theirs is a JS [[Call]] of `Map.prototype.set`.
///
/// Property insertion order matches the iteration order of the iterable
/// (a duplicate key overwrites the value but KEEPS its original insertion
/// position, §10.1.6.3). Symbol keys are stored in symbol_properties.
fn from_entries(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let iterable = first_arg_or_undefined(args)
  case iterable {
    // Step 1: RequireObjectCoercible — null/undefined throw TypeError.
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    _ -> {
      // Step 2: obj = OrdinaryObjectCreate(%Object.prototype%).
      let #(heap, obj_ref) =
        common.alloc_pojo(state.heap, state.builtins.object.prototype, [])
      use state, key_val, val <- iter_protocol.add_entries_with_sink(
        State(..state, heap:),
        JsObject(obj_ref),
        iterable,
      )
      // Step 3: ! CreateDataPropertyOrThrow(obj, ToPropertyKey(key), value).
      define_own.create_data_property(state, obj_ref, key_val, val)
    }
  }
}

/// Object.getOwnPropertyDescriptors ( O ) — ES2024 §20.1.2.9
///
///   1. Let obj be ? ToObject(O).
///   2. Let ownKeys be ? obj.[[OwnPropertyKeys]]().
///   3. Let descriptors be OrdinaryObjectCreate(%Object.prototype%).
///   4. For each element key of ownKeys, do
///      a. Let desc be ? obj.[[GetOwnProperty]](key).
///      b. Let descriptor be FromPropertyDescriptor(desc).
///      c. If descriptor is not undefined, CreateDataPropertyOrThrow(descriptors, key, descriptor).
///   5. Return descriptors.
fn get_own_property_descriptors(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let object_proto = state.builtins.object.prototype
  let target = first_arg_or_undefined(args)
  case target {
    // Step 1: ToObject throws TypeError for null/undefined.
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    JsObject(ref) -> {
      // Step 2: ownKeys = ? obj.[[OwnPropertyKeys]]() — trap-aware, and the
      // one funnel that already yields fast-element indices, string-exotic /
      // typed-array indices and symbol keys in §10.1.11 order.
      use keys, state <- state.try_op(define_own.own_property_keys(state, ref))
      // Steps 3-5: per key ? obj.[[GetOwnProperty]](key) (trap-aware) →
      // FromPropertyDescriptor, accumulated in key order.
      descriptors_from_keys(
        state,
        keys,
        object_proto,
        fn(state, dkey) { define_own.own_property_keyed(state, ref, dkey) },
        dict.new(),
        [],
      )
    }
    // String primitives: [[OwnPropertyKeys]] is the index chars then
    // "length" (§10.4.3.3); descriptors come from the shared string-exotic
    // own-property table.
    JsString(s) -> {
      let keys =
        list.append(string_index_object_keys(0, js_string.length(s)), [
          define_own.named_object_key("length"),
        ])
      descriptors_from_keys(
        state,
        keys,
        object_proto,
        fn(state, dkey) {
          Ok(#(define_own.string_exotic_own_property(s, dkey), state))
        },
        dict.new(),
        [],
      )
    }
    // Number/Boolean/Symbol/BigInt: ToObject yields a wrapper with no own
    // keys, so step 4 iterates nothing and step 3's empty object is the
    // result — same funnel, empty key list.
    _ ->
      descriptors_from_keys(
        state,
        [],
        object_proto,
        fn(state, _dkey) { Ok(#(None, state)) },
        dict.new(),
        [],
      )
  }
}

/// §20.1.2.9 step 4: for each own key, `own_property` ([[GetOwnProperty]])
/// → FromPropertyDescriptor → CreateDataPropertyOrThrow on the result
/// object, in [[OwnPropertyKeys]] order.
///
/// String-keyed descriptors accumulate in `props` (index keys sort
/// numerically; named keys get a fresh creation seq per iteration, so
/// §10.1.11 enumeration order is preserved). Symbol-keyed descriptors
/// accumulate reversed in `syms`. Keys reported absent by `own_property`
/// (step 4.c — e.g. an ownKeys-trap key the getOwnPropertyDescriptor trap
/// denies) are skipped.
fn descriptors_from_keys(
  state: State(host),
  keys: List(ObjectKey),
  object_proto: Ref,
  own_property: fn(State(host), ObjectKey) ->
    Result(#(Option(value.Property), State(host)), #(JsValue, State(host))),
  props: dict.Dict(key.PropertyKey, value.Property),
  syms: List(#(value.SymbolId, value.Property)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case keys {
    // Steps 3 + 5: OrdinaryObjectCreate(%Object.prototype%) holding the
    // accumulated descriptors.
    [] -> {
      let #(heap, result_ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: OrdinaryObject,
            properties: props,
            symbol_properties: list.reverse(syms),
            elements: elements.new(),
            prototype: Some(object_proto),
            extensible: True,
          ),
        )
      #(State(..state, heap:), Ok(JsObject(result_ref)))
    }
    [dkey, ..rest] -> {
      // Step 4.a: desc = ? obj.[[GetOwnProperty]](key). (A TDZ namespace
      // binding throws inside it, per §10.4.6.5.)
      use prop_opt, state <- state.try_op(own_property(state, dkey))
      case prop_opt {
        // Steps 4.b-c: FromPropertyDescriptor + CreateDataPropertyOrThrow.
        Some(prop) -> {
          let #(heap, desc_ref) =
            define_own.make_descriptor_object(state.heap, prop, object_proto)
          let state = State(..state, heap:)
          let desc = value.data_property(JsObject(desc_ref))
          let #(props, syms) = case dkey {
            SymbolPropKey(sym) -> #(props, [#(sym, desc), ..syms])
            StringPropKey(pkey:, ..) -> #(dict.insert(props, pkey, desc), syms)
          }
          descriptors_from_keys(
            state,
            rest,
            object_proto,
            own_property,
            props,
            syms,
          )
        }
        // Step 4.c: descriptor is undefined — skip the key.
        None ->
          descriptors_from_keys(
            state,
            rest,
            object_proto,
            own_property,
            props,
            syms,
          )
      }
    }
  }
}

/// Object.getOwnPropertySymbols ( O ) — ES2024 §20.1.2.11 / GetOwnPropertyKeys(O, symbol)
///
///   1. Let obj be ? ToObject(O).
///   2. Let keys be ? obj.[[OwnPropertyKeys]]().
///   3. Let nameList be a new empty List.
///   4. For each element nextKey of keys, do
///      a. If Type(nextKey) is Symbol, append nextKey to nameList.
///   5. Return CreateArrayFromList(nameList).
///
/// For non-null/undefined primitives (strings, numbers, booleans, symbols),
/// ToObject creates a wrapper that has no own symbol-keyed properties,
/// so the result is always an empty array.
/// For null/undefined, throws TypeError.
fn get_own_property_symbols(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case first_arg_or_undefined(args) {
    // Step 1: ToObject — null/undefined throw TypeError.
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    JsObject(ref) ->
      case object.as_proxy(state.heap, ref) {
        // Proxy: [[OwnPropertyKeys]] trap, keep only Symbols.
        Some(_) -> {
          use all_keys, state <- state.try_op(define_own.own_property_keys(
            state,
            ref,
          ))
          let syms =
            list.filter_map(all_keys, fn(k) {
              case k {
                SymbolPropKey(sym) -> Ok(JsSymbol(sym))
                StringPropKey(..) -> Error(Nil)
              }
            })
          state.ok_array(state, syms)
        }
        None -> {
          // Step 4: Collect own symbol keys.
          let syms = define_own.collect_own_symbol_keys(state.heap, ref, False)
          // Step 5: CreateArrayFromList(nameList) — each element is a JsSymbol.
          state.ok_array(state, list.map(syms, JsSymbol))
        }
      }
    // For non-object primitives (string, number, boolean, symbol):
    // ToObject creates a wrapper with no own symbol properties → return [].
    _ -> state.ok_array(state, [])
  }
}

/// ES2024 §20.1.3.3 Object.prototype.isPrototypeOf ( V )
///
///   1. If V is not an Object, return false.
///   2. Let O be ? ToObject(this value).
///   3. Repeat,
///      a. Set V to ? V.[[GetPrototypeOf]]().
///      b. If V is null, return false.
///      c. If SameValue(O, V) is true, return true.
fn is_prototype_of(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let v = first_arg_or_undefined(args)
  // Step 1: If V is not an Object, return false (before the ToObject check).
  case v {
    JsObject(v_ref) -> {
      // Step 2: Let O be ? ToObject(this value).
      case this {
        JsNull | JsUndefined -> state.type_error(state, cannot_convert)
        JsObject(this_ref) -> is_prototype_of_loop(state, v_ref, this_ref)
        // Primitive this: ToObject creates a fresh wrapper, which can never
        // appear in V's existing prototype chain — always false.
        _ -> #(state, Ok(JsBool(False)))
      }
    }
    _ -> #(state, Ok(JsBool(False)))
  }
}

/// Step 3 — walk V's prototype chain with the trap-aware [[GetPrototypeOf]]
/// (proxy getPrototypeOf traps fire per iteration) looking for this_ref.
fn is_prototype_of_loop(
  state: State(host),
  v_ref: Ref,
  this_ref: Ref,
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 3.a: Set V to ? V.[[GetPrototypeOf]]().
  use proto, state <- state.try_op(object.get_prototype_of_stateful(
    state,
    v_ref,
  ))
  case proto {
    // Step 3.c: SameValue(O, V) — found it.
    JsObject(proto_ref) ->
      case proto_ref == this_ref {
        True -> #(state, Ok(JsBool(True)))
        False -> is_prototype_of_loop(state, proto_ref, this_ref)
      }
    // Step 3.b: V is null — return false.
    _ -> #(state, Ok(JsBool(False)))
  }
}

/// ES2024 §20.1.3.5 Object.prototype.toLocaleString ( )
///
///   1. Let O be the this value.
///   2. Return ? Invoke(O, "toString").
///
/// Invoke (§7.3.21) = GetV(O, "toString") + Call(func, O) — GetV's ToObject
/// throws on null/undefined, and a primitive `this` is passed through to the
/// toString method unwrapped (the method sees the primitive in strict mode).
fn object_to_locale_string(
  this: JsValue,
  _args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this {
    // Step 2: GetV performs ToObject(O) — TypeError on null/undefined.
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    _ -> {
      use to_string_fn, state <- state.try_op(object.get_value_of(
        state,
        this,
        key.canonical_key("toString"),
      ))
      case helpers.is_callable(state.heap, to_string_fn) {
        True -> {
          use result, state <- state.try_call(state, to_string_fn, this, [])
          #(state, Ok(result))
        }
        False ->
          state.type_error(state, "toLocaleString: toString is not callable")
      }
    }
  }
}

/// ES2024 §22.1.2.4 Object.groupBy ( items, callbackfn ) — GroupBy(items,
/// callback, property) with a null-prototype result object.
///
/// `items` is drained through the real iterator protocol (§7.4.3 GetIterator +
/// §7.4.8 IteratorStepValue), so a Set / Map / generator / `arguments` / any
/// userland `{ [Symbol.iterator]() {…} }` groups correctly, and a patched
/// `Array.prototype[Symbol.iterator]` is honoured. An abrupt completion from
/// the callback or from ToPropertyKey closes the iterator (IfAbruptCloseIterator).
fn group_by(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let items = first_arg_or_undefined(args)
  let callback = case args {
    [_, cb, ..] -> cb
    _ -> JsUndefined
  }
  case helpers.is_callable(state.heap, callback) {
    False -> state.type_error(state, "Object.groupBy callback is not callable")
    True -> {
      // §7.3.35 step 4: iteratorRecord = ? GetIterator(items, sync).
      use rec, state <- state.try_op(iter_protocol.get_iterator_sync(
        state,
        items,
      ))
      group_by_loop(state, rec, callback, 0, dict.new(), [])
    }
  }
}

/// §7.3.35 GroupBy(items, callback, property) loop.
///
/// `groups` maps each resolved ToPropertyKey (a JsString or JsSymbol) to its
/// members in reverse encounter order; `order` is the keys in reverse
/// FIRST-occurrence order. The spec's AddValueToKeyedGroup appends to an
/// existing group without moving it, so the result object's key order is
/// first-occurrence order — `order` carries that, while the dict keeps the
/// per-item lookup O(log n) instead of an O(groups) assoc-list scan.
fn group_by_loop(
  state: State(host),
  rec: value.IteratorRecord,
  callback: JsValue,
  index: Int,
  groups: dict.Dict(JsValue, List(JsValue)),
  order: List(JsValue),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 6.c: value = ? IteratorStepValue(iteratorRecord). An abrupt
  // completion here propagates WITHOUT close (§7.4.8 already marked it done).
  let #(state, step) = iter_protocol.iterator_step_value(state, rec)
  case step {
    Error(thrown) -> #(state, Error(thrown))
    // Object.groupBy steps 3-4: obj = OrdinaryObjectCreate(null), then for
    // each group ! CreateDataPropertyOrThrow(obj, g.[[Key]],
    // CreateArrayFromList(g.[[Elements]])).
    Ok(None) -> {
      let #(heap, obj_ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: OrdinaryObject,
            properties: dict.new(),
            elements: elements.new(),
            prototype: None,
            symbol_properties: [],
            extensible: True,
          ),
        )
      group_by_finish(
        State(..state, heap:),
        obj_ref,
        list.reverse(order),
        groups,
      )
    }
    Ok(Some(item)) -> {
      // Steps 6.e-6.f: key = Call(callback, undefined, «value, k»);
      // IfAbruptCloseIterator(key, iteratorRecord).
      use key_val, state <- iter_protocol.or_close(
        state.call(state, callback, JsUndefined, [item, value.from_int(index)]),
        rec.iterator,
      )
      // Step 6.g (property coercion): key = ? ToPropertyKey(key) — a symbol key
      // groups under the symbol, never a stringification. Abrupt →
      // IfAbruptCloseIterator.
      use dkey, state <- iter_protocol.or_close(
        property.to_prop_key(state, key_val),
        rec.iterator,
      )
      let key = object_key_value(dkey)
      // AddValueToKeyedGroup: append to the existing group without moving it,
      // so `order` only grows on a key's FIRST occurrence.
      let #(groups, order) = case dict.get(groups, key) {
        Ok(members) -> #(dict.insert(groups, key, [item, ..members]), order)
        Error(Nil) -> #(dict.insert(groups, key, [item]), [key, ..order])
      }
      group_by_loop(state, rec, callback, index + 1, groups, order)
    }
  }
}

/// Define each group's array on the (null-prototype) result object via
/// CreateDataPropertyOrThrow — writable/enumerable/configurable, so
/// `Object.keys(Object.groupBy(...))` sees every group — in first-occurrence
/// key order (`keys`).
fn group_by_finish(
  state: State(host),
  obj_ref: Ref,
  keys: List(JsValue),
  groups: dict.Dict(JsValue, List(JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case keys {
    [] -> #(state, Ok(JsObject(obj_ref)))
    [key, ..rest] -> {
      let members = dict.get(groups, key) |> result.unwrap([])
      let #(heap, arr_ref) =
        common.alloc_array(
          state.heap,
          list.reverse(members),
          state.builtins.array.prototype,
        )
      use state <- create_data_property_or_throw(
        State(..state, heap:),
        obj_ref,
        key,
        JsObject(arr_ref),
      )
      group_by_finish(state, obj_ref, rest, groups)
    }
  }
}

// ----------------------------------------------------------------------------
// Re-exports of the §10.x MOP core (moved to `ops/define_own`) for existing
// callers. New code should import `arc/vm/ops/define_own` directly.
// ----------------------------------------------------------------------------

pub const get_own_property_stateful = define_own.get_own_property_stateful

pub const own_property_keyed = define_own.own_property_keyed

pub const own_property_keys = define_own.own_property_keys

pub const create_data_property_or_throw = define_own.create_data_property_or_throw

pub const create_data_property_bool = define_own.create_data_property_bool

pub const define_property_bool = define_own.define_property_bool

pub const enumerable_string_keys_stateful = define_own.enumerable_string_keys_stateful

pub const object_key_value = define_own.object_key_value
