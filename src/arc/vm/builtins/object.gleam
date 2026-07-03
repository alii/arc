import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers.{first_arg_or_undefined}
import arc/vm/builtins/iter_protocol
import arc/vm/builtins/symbol
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/key.{Index, Named}
import arc/vm/limits
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/ops/property
import arc/vm/ops/typed_array_elements
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsElements, type JsValue, type ObjectKey, type ObjectNativeFn, type Ref,
  AccessorProperty, ArrayObject, DataProperty, Dispatch, FunctionObject,
  GeneratorObject, JsBool, JsNull, JsNumber, JsObject, JsString, JsSymbol,
  JsUndefined, NativeFunction, ObjectAssign, ObjectConstructor, ObjectCreate,
  ObjectDefineProperties, ObjectDefineProperty, ObjectEntries, ObjectFreeze,
  ObjectFromEntries, ObjectGetOwnPropertyDescriptor,
  ObjectGetOwnPropertyDescriptors, ObjectGetOwnPropertyNames,
  ObjectGetOwnPropertySymbols, ObjectGetPrototypeOf, ObjectHasOwn, ObjectIs,
  ObjectIsExtensible, ObjectIsFrozen, ObjectIsSealed, ObjectKeys, ObjectNative,
  ObjectPreventExtensions, ObjectPrototypeHasOwnProperty,
  ObjectPrototypePropertyIsEnumerable, ObjectPrototypeToString,
  ObjectPrototypeValueOf, ObjectSeal, ObjectSetPrototypeOf, ObjectSlot,
  ObjectValues, OrdinaryObject, StringPropKey, SymbolPropKey,
}
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import gleam/set

/// V8/Node's standard ToObject failure message.
const cannot_convert = "Cannot convert undefined or null to object"

/// Thrown when a Module Namespace operation touches a still-uninitialized
/// (TDZ) export binding (§10.4.6 [[Get]] / [[GetOwnProperty]]).
const tdz_message = "Cannot access module export before initialization"

/// ToPropertyKey (§7.1.19) resolved to a value.ObjectKey (a PropKey
/// plus a cached display string for error messages). The spec-order
/// conversion — ToPrimitive(argument, string) FIRST, then the Symbol check,
/// so `{[Symbol.toPrimitive]: () => sym}` used as a key produces a symbol
/// key — is property.to_prop_key; this only re-shapes the result.
fn to_object_key(
  state: State(host),
  key_val: JsValue,
) -> Result(#(ObjectKey, State(host)), #(JsValue, State(host))) {
  use #(pk, state) <- result.map(property.to_prop_key(state, key_val))
  #(prop_key_to_object_key(pk), state)
}

/// The ObjectKey form of an already-resolved PropKey.
///
/// The `display` string is NOT just for error messages: `object_key_value`
/// hands it to proxy `defineProperty` / `getOwnPropertyDescriptor` traps as
/// the property-key argument, and the String-exotic gOPD path compares it to
/// "length". So it must be the exact ToPropertyKey string — never
/// `key.key_to_string`, which is a *renderer* that strips the engine's
/// internal private-name NUL marker and would hand traps a mangled key.
fn prop_key_to_object_key(pk: object.PropKey) -> ObjectKey {
  case pk {
    object.PkSymbol(sym) -> SymbolPropKey(sym)
    object.PkString(pkey) ->
      StringPropKey(pkey, case pkey {
        Named(s) -> s
        Index(i) -> int.to_string(i)
      })
  }
}

/// CPS form of `to_object_key` so callers can `use` before `case this`,
/// preserving spec ordering (hasOwnProperty does ToPropertyKey step 1,
/// ToObject step 2).
fn try_to_property_key(
  state: State(host),
  key_val: JsValue,
  cont: fn(ObjectKey, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case to_object_key(state, key_val) {
    Error(#(thrown, state)) -> #(state, Error(thrown))
    Ok(#(dkey, state)) -> cont(dkey, state)
  }
}

/// [[GetOwnProperty]] dispatching on a resolved ObjectKey.
fn get_own_property_by_key(
  heap: Heap(host),
  ref: Ref,
  key: ObjectKey,
) -> Option(value.Property) {
  case key {
    SymbolPropKey(sym) -> object.get_own_symbol_property(heap, ref, sym)
    // Private elements (NUL-marker keys, see key.private_key) are stored in
    // the ordinary property table but are invisible to reflection (spec keeps
    // them in [[PrivateElements]]).
    StringPropKey(pkey: Named("\u{0}" <> _), ..) -> None
    StringPropKey(pkey:, ..) -> object.get_own_property(heap, ref, pkey)
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

/// §10.4.3.5 StringGetOwnProperty ( S, P ) plus the "length" own data
/// property created by §10.4.3.4 StringCreate — the synthesized own-property
/// table of a String primitive receiver, keyed by an ALREADY-RESOLVED
/// property key.
///
/// This is the single source of truth for which keys are own properties of a
/// String primitive: getOwnPropertyDescriptor(s), hasOwnProperty,
/// Object.hasOwn, propertyIsEnumerable and (via `object.string_length`, the
/// same length source) the [[OwnPropertyKeys]] listing in own_keys_impl all
/// derive from it, so they agree by construction. String *wrapper objects*
/// take the mirror-image path in ops/object.own_property_of_slot.
///
/// Matching on the resolved `ObjectKey` (not the display string) is
/// load-bearing: ToPropertyKey already canonicalized array indices, so
/// "01", "+1" or " 1" arrive as `Named(..)` and correctly report no own
/// property — re-parsing the display string with int.parse would fabricate
/// index properties for them.
fn string_exotic_own_property(
  s: String,
  key: ObjectKey,
) -> Option(value.Property) {
  // seq: 0 on both arms — synthesized descriptors, only rendered by
  // make_descriptor_object, never stored in a property table.
  case key {
    // §10.4.3.4 StringCreate step 10: "length" is { W:F, E:F, C:F }.
    StringPropKey(pkey: Named("length"), ..) ->
      Some(DataProperty(
        value: value.from_int(object.string_length(s)),
        writable: False,
        enumerable: False,
        configurable: False,
        seq: 0,
      ))
    // §10.4.3.5 steps 5-10: an in-range integer index yields
    // { value: <element>, W:F, E:T, C:F }; out of range → undefined.
    StringPropKey(pkey: Index(i), ..) -> {
      use ch <- option.map(object.string_char_at(s, i))
      DataProperty(
        value: JsString(ch),
        writable: False,
        enumerable: True,
        configurable: False,
        seq: 0,
      )
    }
    // Any other string name, and every symbol, is not an own property of a
    // String primitive.
    StringPropKey(pkey: Named(_), ..) | SymbolPropKey(_) -> None
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
  let #(target, key_val) = case args {
    [t, k, ..] -> #(t, k)
    [t] -> #(t, JsUndefined)
    [] -> #(JsUndefined, JsUndefined)
  }
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
      use prop_opt, state <- state.try_op(own_property_keyed(state, ref, key))
      case prop_opt {
        Some(prop) -> {
          // Step 4: Return FromPropertyDescriptor(desc).
          // (desc is not undefined, so we build a descriptor object.)
          let #(heap, desc_ref) =
            make_descriptor_object(state.heap, prop, object_proto)
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
      case string_exotic_own_property(s, key) {
        // Step 4: Return FromPropertyDescriptor(desc).
        Some(prop) -> {
          let #(heap, desc_ref) =
            make_descriptor_object(state.heap, prop, object_proto)
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

/// FromPropertyDescriptor ( Desc )
/// ES2024 §6.2.6.4
///
/// Converts an internal Property Descriptor to a plain object.
/// 1. If Desc is undefined, return undefined.  (handled by caller)
/// 2. Let obj be OrdinaryObjectCreate(%Object.prototype%).
/// 3. Assert: obj is extensible with no own properties.
/// 4. If Desc has a [[Value]] field, create "value" with Desc.[[Value]].
/// 5. If Desc has a [[Writable]] field, create "writable" with Desc.[[Writable]].
/// 6. If Desc has a [[Get]] field, create "get" with Desc.[[Get]].
/// 7. If Desc has a [[Set]] field, create "set" with Desc.[[Set]].
/// 8. If Desc has an [[Enumerable]] field, create "enumerable" with Desc.[[Enumerable]].
/// 9. If Desc has a [[Configurable]] field, create "configurable" with Desc.[[Configurable]].
/// 10. Return obj.
///
/// All created properties are {[[Writable]]: true, [[Enumerable]]: true,
/// [[Configurable]]: true} per spec. We use value.data_property which
/// defaults to writable=true, enumerable=true, configurable=true.
pub fn make_descriptor_object(
  heap: Heap(host),
  prop: value.Property,
  object_proto: Ref,
) -> #(Heap(host), Ref) {
  case prop {
    // Step 3: IsDataDescriptor — create "value" and "writable" properties.
    DataProperty(value: val, writable:, enumerable:, configurable:, ..) ->
      common.alloc_pojo(heap, object_proto, [
        // Step 3a: "value"
        #("value", value.data_property(val)),
        // Step 3b: "writable"
        #("writable", value.data_property(JsBool(writable))),
        // Step 5: "enumerable"
        #("enumerable", value.data_property(JsBool(enumerable))),
        // Step 6: "configurable"
        #("configurable", value.data_property(JsBool(configurable))),
      ])
    // Step 4: IsAccessorDescriptor — create "get" and "set" properties.
    AccessorProperty(get:, set:, enumerable:, configurable:, ..) -> {
      // Spec: Desc.[[Get]] / Desc.[[Set]] are stored as-is. If absent
      // internally (None), we emit undefined per convention.
      let get_val = option.unwrap(get, JsUndefined)
      let set_val = option.unwrap(set, JsUndefined)
      common.alloc_pojo(heap, object_proto, [
        // Step 4a: "get"
        #("get", value.data_property(get_val)),
        // Step 4b: "set"
        #("set", value.data_property(set_val)),
        // Step 5: "enumerable"
        #("enumerable", value.data_property(JsBool(enumerable))),
        // Step 6: "configurable"
        #("configurable", value.data_property(JsBool(configurable))),
      ])
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
    [JsObject(ref) as obj, key_val, JsObject(desc_ref), ..] -> {
      // Steps 2-4: ToPropertyKey + ToPropertyDescriptor + DefinePropertyOrThrow
      // (apply_descriptor combines all three steps.)
      use state <- state.try_state(apply_descriptor(
        state,
        ref,
        key_val,
        desc_ref,
      ))
      // Step 5: Return O.
      #(state, Ok(obj))
    }
    // Step 3 (implicit): Attributes is not an Object — TypeError.
    // Spec: ToPropertyDescriptor step 1 throws if Type(Obj) is not Object.
    [JsObject(_), _, ..] ->
      state.type_error(state, "Property description must be an object")
    // Step 1: If O is not an Object, throw a TypeError.
    _ -> state.type_error(state, "Object.defineProperty called on non-object")
  }
}

/// ToPropertyDescriptor ( Obj ) + DefinePropertyOrThrow combined.
/// ES2024 §6.2.6.5 (ToPropertyDescriptor) + §7.3.8 (DefinePropertyOrThrow)
///
/// ToPropertyDescriptor steps:
///   1. If Type(Obj) is not Object, throw TypeError. (checked by caller)
///   2. Let desc be a new empty Property Descriptor.
///   3. If Obj has "enumerable", set desc.[[Enumerable]] = ToBoolean(Get(Obj, "enumerable")).
///   4. If Obj has "configurable", set desc.[[Configurable]] = ToBoolean(Get(Obj, "configurable")).
///   5. If Obj has "value", set desc.[[Value]] = Get(Obj, "value").
///   6. If Obj has "writable", set desc.[[Writable]] = ToBoolean(Get(Obj, "writable")).
///   7. If Obj has "get", let getter = Get(Obj, "get"). If not callable and not undefined, throw TypeError. Set desc.[[Get]].
///   8. If Obj has "set", let setter = Get(Obj, "set"). If not callable and not undefined, throw TypeError. Set desc.[[Set]].
///   9. If desc has [[Get]] or [[Set]], and desc has [[Value]] or [[Writable]], throw TypeError.
///  10. Return desc.
///
/// TODO(Deviation): Does not fully check non-configurable constraints
/// (e.g. redefining a non-configurable property's attributes should throw
/// in some cases per ValidateAndApplyPropertyDescriptor).
pub fn apply_descriptor(
  state: State(host),
  target_ref: Ref,
  key_val: JsValue,
  desc_ref: Ref,
) -> Result(State(host), #(JsValue, State(host))) {
  use #(dkey, state) <- result.try(to_object_key(state, key_val))
  apply_descriptor_keyed(state, target_ref, dkey, desc_ref)
}

/// apply_descriptor for callers whose key is already a resolved ObjectKey (so
/// no ToPropertyKey step, which could otherwise re-run user code).
fn apply_descriptor_keyed(
  state: State(host),
  target_ref: Ref,
  dkey: ObjectKey,
  desc_ref: Ref,
) -> Result(State(host), #(JsValue, State(host))) {
  use #(parsed, state) <- result.try(parse_descriptor(state, JsObject(desc_ref)))
  // DefinePropertyOrThrow (§7.3.8): a false [[DefineOwnProperty]] result and
  // a genuine abrupt completion both surface as a throw here.
  define_parsed(state, target_ref, dkey, parsed)
  |> result.map_error(as_define_throw)
}

/// [[DefineOwnProperty]] dispatch for an already-parsed descriptor.
/// Arrays get the exotic algorithm (§10.4.2.1) which special-cases "length"
/// (ArraySetLength §10.4.2.4) and array indices; proxies trap (§10.5.6);
/// everything else is OrdinaryDefineOwnProperty (§10.1.6.1).
fn define_parsed(
  state: State(host),
  target_ref: Ref,
  dkey: ObjectKey,
  parsed: ParsedDesc,
) -> Result(State(host), #(DefineFailure, State(host))) {
  case heap.read(state.heap, target_ref) {
    Some(ObjectSlot(kind: ArrayObject(length:), ..)) ->
      case dkey {
        StringPropKey(pkey: Named("length"), ..) ->
          array_define_length(state, target_ref, parsed, length)
        // An array index is an integer in [0, 2^32-1) — "4294967295" is a
        // plain property name on arrays, not an index (§6.1.7).
        StringPropKey(pkey: Index(idx), ..) if idx < 4_294_967_295 ->
          array_define_index(state, target_ref, idx, parsed, length)
          |> result.map_error(as_rejected)
        _ ->
          ordinary_define(state, target_ref, dkey, parsed)
          |> result.map_error(as_rejected)
      }
    // §10.4.5.3 TypedArray (Integer-Indexed) [[DefineOwnProperty]]: canonical
    // numeric index keys never reach the ordinary property table — they
    // validate against the fixed {W:T, E:T, C:T} element descriptor and
    // store through IntegerIndexedElementSet. Everything else is ordinary.
    Some(ObjectSlot(
      kind: value.TypedArrayObject(buffer:, elem_kind:, byte_offset:, length:),
      ..,
    )) ->
      case dkey {
        StringPropKey(pkey: Index(idx), ..) ->
          typed_array_define_index(
            state,
            buffer,
            elem_kind,
            byte_offset,
            length,
            idx,
            parsed,
          )
        StringPropKey(pkey: Named(s), ..) ->
          case object.is_canonical_numeric_string(s) {
            // Step 1.b.i: a canonical numeric string that survived
            // canonical_key is never a valid integer index ("1.5", "-0",
            // "NaN", "-1", "1e+21", …) → false, with NO value conversion.
            True ->
              reject_define(state, "Invalid typed array index")
              |> result.map(fn(_) { state })
              |> result.map_error(as_rejected)
            False ->
              ordinary_define(state, target_ref, dkey, parsed)
              |> result.map_error(as_rejected)
          }
        SymbolPropKey(_) ->
          ordinary_define(state, target_ref, dkey, parsed)
          |> result.map_error(as_rejected)
      }
    // String exotic [[DefineOwnProperty]] (§10.4.3.2): the synthesized
    // "length" and in-range index properties are non-writable and
    // non-configurable — validate against their fixed descriptors instead of
    // writing to the dict.
    Some(ObjectSlot(kind: value.StringObject(value: s), extensible:, ..)) -> {
      let synthesized = case dkey {
        StringPropKey(pkey: Named("length"), ..) -> True
        StringPropKey(pkey: Index(i), ..) ->
          i >= 0 && i < object.string_length(s)
        _ -> False
      }
      case synthesized {
        True -> {
          let cur = get_own_property_by_key(state.heap, target_ref, dkey)
          case is_compatible_descriptor(extensible, parsed, cur) {
            // Compatible with a frozen descriptor = a no-op redefinition.
            True -> Ok(state)
            False ->
              reject_define(
                state,
                "Cannot redefine property: " <> object_key_label(dkey),
              )
              |> result.map(fn(_) { state })
              |> result.map_error(as_rejected)
          }
        }
        False ->
          ordinary_define(state, target_ref, dkey, parsed)
          |> result.map_error(as_rejected)
      }
    }
    // §10.4.6.6 Module Namespace [[DefineOwnProperty]] ( P, Desc ): string
    // keys never define anything — return true only when Desc is compatible
    // with the existing export binding's fixed descriptor (writable,
    // enumerable, non-configurable data property), false otherwise. The
    // throwing wrapper converts false to TypeError; define_property_bool
    // (Reflect.defineProperty) converts it back to false.
    Some(ObjectSlot(kind: value.ModuleNamespace(exports:), ..)) ->
      case dkey {
        // Step 1: If P is a Symbol, return OrdinaryDefineOwnProperty(O, P, Desc).
        SymbolPropKey(_) ->
          ordinary_define(state, target_ref, dkey, parsed)
          |> result.map_error(as_rejected)
        StringPropKey(pkey: _, display: name) ->
          namespace_define(state, exports, name, parsed)
          |> result.map_error(as_rejected)
      }
    // §10.5.6 Proxy [[DefineOwnProperty]] — the throwing wrapper
    // (DefinePropertyOrThrow) converts a false trap result to TypeError.
    // Errors out of the proxy machinery (trap call throws, invariant
    // TypeErrors §10.5.6 steps 12-16) are genuine abrupt completions.
    Some(ObjectSlot(kind: value.ProxyObject(target:, handler:, ..), ..)) -> {
      use #(state, ok) <- result.try(
        proxy_define_own_property(state, target, handler, dkey, parsed)
        |> result.map_error(as_thrown),
      )
      case ok {
        True -> Ok(state)
        False ->
          reject_define(
            state,
            "'defineProperty' on proxy: trap returned falsish for property "
              <> object_key_label(dkey),
          )
          |> result.map(fn(_) { state })
          |> result.map_error(as_rejected)
      }
    }
    _ ->
      ordinary_define(state, target_ref, dkey, parsed)
      |> result.map_error(as_rejected)
  }
}

/// §10.4.6.6 Module Namespace [[DefineOwnProperty]] steps 2-9 for a string
/// key. Every export is a { writable: true, enumerable: true,
/// configurable: false } data property whose value is the live binding;
/// a request is honoured (true) iff it changes nothing.
fn namespace_define(
  state: State(host),
  exports: dict.Dict(String, Ref),
  name: String,
  parsed: ParsedDesc,
) -> Result(State(host), #(JsValue, State(host))) {
  // Steps 2-3: current = O.[[GetOwnProperty]](P); undefined → false.
  use box <- result.try(case dict.get(exports, name) {
    Ok(box) -> Ok(box)
    Error(Nil) ->
      reject_define(
        state,
        "Cannot define property " <> name <> ", object is not extensible",
      )
  })
  // Step 4: Desc.[[Configurable]] present and true → false.
  // Step 5: Desc.[[Enumerable]] present and false → false.
  // Step 6: IsAccessorDescriptor(Desc) → false.
  // Step 7: Desc.[[Writable]] present and false → false.
  let incompatible =
    parsed.configurable == Some(True)
    || parsed.enumerable == Some(False)
    || desc_is_accessor(parsed)
    || parsed.writable == Some(False)
  use Nil <- result.try(case incompatible {
    True -> reject_define(state, "Cannot redefine property: " <> name)
    False -> Ok(Nil)
  })
  case parsed.value {
    // Step 9: no [[Value]] requested → true (nothing to change).
    None -> Ok(state)
    // Step 8: return SameValue(Desc.[[Value]], current.[[Value]]).
    Some(requested) -> {
      // [[GetOwnProperty]] reads the live binding; an uninitialized (TDZ)
      // binding throws ReferenceError (§10.4.6.5 performs [[Get]]).
      case heap.read_box(state.heap, box) {
        Some(value.JsUninitialized) ->
          Error(state.reference_error_value(
            state,
            "Cannot access '" <> name <> "' before initialization",
          ))
        current -> {
          let current_value = option.unwrap(current, JsUndefined)
          case value.same_value(requested, current_value) {
            True -> Ok(state)
            False -> reject_define(state, "Cannot redefine property: " <> name)
          }
        }
      }
    }
  }
}

/// Parsed ToPropertyDescriptor result (§6.2.6.5). `None` = field absent,
/// which matters for descriptor merging (absent fields are inherited).
type ParsedDesc {
  ParsedDesc(
    get: Option(JsValue),
    set: Option(JsValue),
    value: Option(JsValue),
    writable: Option(Bool),
    enumerable: Option(Bool),
    configurable: Option(Bool),
  )
}

/// IsAccessorDescriptor (§6.2.6.1).
fn desc_is_accessor(desc: ParsedDesc) -> Bool {
  option.is_some(desc.get) || option.is_some(desc.set)
}

/// IsDataDescriptor (§6.2.6.2).
fn desc_is_data(desc: ParsedDesc) -> Bool {
  option.is_some(desc.value) || option.is_some(desc.writable)
}

/// §10.4.2.1 Array exotic [[DefineOwnProperty]] — P is an array index.
fn array_define_index(
  state: State(host),
  target_ref: Ref,
  idx: Int,
  desc: ParsedDesc,
  old_len: Int,
) -> Result(State(host), #(JsValue, State(host))) {
  // Step 2.c: adding an index >= length requires length to be writable.
  let len_writable = array_length_writable(state.heap, target_ref)
  use Nil <- result.try(case idx >= old_len && !len_writable {
    True ->
      reject_define(
        state,
        "Cannot add property "
          <> int.to_string(idx)
          <> ", object is not extensible",
      )
    False -> Ok(Nil)
  })
  // Step 2.d: OrdinaryDefineOwnProperty(A, P, Desc) — throws on failure.
  use state <- result.try(ordinary_define(
    state,
    target_ref,
    StringPropKey(Index(idx), int.to_string(idx)),
    desc,
  ))
  // Steps 2.f-2.g: grow length to index + 1.
  case idx >= old_len {
    False -> Ok(state)
    True -> Ok(write_array_length(state, target_ref, idx + 1, False))
  }
}

/// §10.4.5.3 TypedArray [[DefineOwnProperty]] — P is a canonical integer
/// index. Steps 1.b.i-vii:
///   i.   invalid index (out of bounds / detached / shrunk) → false
///   ii.  [[Configurable]] present and false → false
///   iii. [[Enumerable]] present and false → false
///   iv.  accessor descriptor → false
///   v.   [[Writable]] present and false → false
///   vi.  [[Value]] present → ? IntegerIndexedElementSet (value conversion
///        may run user code and throw; a buffer detached DURING conversion
///        makes the store a silent no-op, still true). Immutable-buffer
///        views never reach the element store: the define succeeds only if
///        [[Value]] is SameValue to the current element and Desc asks for
///        no [[Writable]]/[[Configurable]] upgrade; otherwise it is rejected.
///   vii. true
/// The checks run BEFORE any value conversion — an invalid index must not
/// trigger observable ToNumber/ToBigInt side effects.
fn typed_array_define_index(
  state: State(host),
  buffer: Ref,
  elem_kind: value.TypedArrayKind,
  byte_offset: Int,
  length: option.Option(Int),
  idx: Int,
  desc: ParsedDesc,
) -> Result(State(host), #(DefineFailure, State(host))) {
  let current =
    object.typed_array_element(
      state.heap,
      buffer,
      elem_kind,
      byte_offset,
      object.typed_array_view_length(
        state.heap,
        buffer,
        elem_kind,
        byte_offset,
        length,
      ),
      idx,
    )
  let valid = option.is_some(current)
  let label = int.to_string(idx)
  let reject = fn(msg) {
    reject_define(state, msg)
    |> result.map(fn(_) { state })
    |> result.map_error(as_rejected)
  }
  use <- bool.lazy_guard(!valid, fn() {
    reject("Invalid typed array index: " <> label)
  })
  use <- bool.lazy_guard(desc.configurable == Some(False), fn() {
    reject("Cannot redefine property: " <> label)
  })
  use <- bool.lazy_guard(desc.enumerable == Some(False), fn() {
    reject("Cannot redefine property: " <> label)
  })
  use <- bool.lazy_guard(desc_is_accessor(desc), fn() {
    reject("Cannot redefine property: " <> label)
  })
  use <- bool.lazy_guard(desc.writable == Some(False), fn() {
    reject("Cannot redefine property: " <> label)
  })
  case desc.value {
    None -> Ok(state)
    Some(v) -> {
      use #(state, stored) <- result.try(
        typed_array_elements.typed_array_store(
          state,
          buffer,
          elem_kind,
          byte_offset,
          length,
          Some(idx),
          v,
        )
        |> result.map_error(as_thrown),
      )
      // Immutable ArrayBuffer proposal, [[DefineOwnProperty]]
      // (sec-typedarray-defineownproperty): an immutable-buffer-backed
      // element behaves as a {[[Writable]]: false, [[Enumerable]]: true,
      // [[Configurable]]: false} data property, so
      // ValidateAndApplyPropertyDescriptor returns true iff Desc.[[Value]]
      // is SameValue to the current element AND Desc asks for no attribute
      // upgrade (a true [[Writable]]/[[Configurable]] on a non-writable,
      // non-configurable current is always false); anything else is false.
      // The store refused (False) BEFORE any ToNumber/ToBigInt conversion,
      // so no user code ran and `current` (read at entry) is still live.
      case stored {
        True -> Ok(state)
        False -> {
          let unchanged =
            option.map(current, value.same_value(v, _))
            |> option.unwrap(False)
          let widened =
            desc.writable == Some(True) || desc.configurable == Some(True)
          case unchanged && !widened {
            True -> Ok(state)
            False -> reject("Cannot redefine property: " <> label)
          }
        }
      }
    }
  }
}

/// §10.4.2.4 ArraySetLength ( A, Desc ).
fn array_define_length(
  state: State(host),
  target_ref: Ref,
  desc: ParsedDesc,
  old_len: Int,
) -> Result(State(host), #(DefineFailure, State(host))) {
  let cur_writable = array_length_writable(state.heap, target_ref)
  case desc.value {
    // Step 1: [[Value]] absent → OrdinaryDefineOwnProperty(A, "length", Desc).
    None -> {
      use Nil <- result.try(
        validate_length_attrs(state, desc, cur_writable)
        |> result.map_error(as_rejected),
      )
      Ok(write_array_length(
        state,
        target_ref,
        old_len,
        desc.writable == Some(False),
      ))
    }
    Some(len_val) -> {
      // Steps 3-5: newLen = ToUint32(value); RangeError unless
      // SameValueZero(newLen, ToNumber(value)). Both the ToNumber call (user
      // valueOf/toString may throw) and the RangeError are genuine abrupt
      // completions — they propagate even through Reflect.defineProperty.
      use #(num, state) <- result.try(
        coerce.js_to_number(state, len_val) |> result.map_error(as_thrown),
      )
      use new_len <- result.try(
        parse_array_length(state, num) |> result.map_error(as_thrown),
      )
      // Steps 11/16: OrdinaryDefineOwnProperty validation against the current
      // length descriptor {[[Writable]]: cur, [[Enumerable]]: F, [[Configurable]]: F}.
      use Nil <- result.try(
        validate_length_attrs(state, desc, cur_writable)
        |> result.map_error(as_rejected),
      )
      // Steps 11-12: a non-writable length rejects any value change.
      use Nil <- result.try(
        case !cur_writable && new_len != old_len {
          True -> reject_define(state, "Cannot redefine property: length")
          False -> Ok(Nil)
        }
        |> result.map_error(as_rejected),
      )
      let freeze = desc.writable == Some(False)
      case new_len >= old_len {
        // Step 11: growing (or unchanged) — no deletions needed.
        True -> Ok(write_array_length(state, target_ref, new_len, freeze))
        // Steps 13-19: shrinking — delete indices >= newLen in descending
        // order, stopping at the first non-configurable one (step 17.b
        // returns false → a rejection, not a throw).
        False ->
          shrink_array(state, target_ref, new_len, freeze)
          |> result.map_error(as_rejected)
      }
    }
  }
}

/// ArraySetLength steps 3-5: validate a ToNumber result as a uint32 length.
/// The `+. 0.0` normalizes -0 (SameValueZero treats ±0 as equal, and
/// ToUint32(-0) is +0, so -0 is a valid length of 0).
fn parse_array_length(
  state: State(host),
  num: value.JsNum,
) -> Result(Int, #(JsValue, State(host))) {
  case num {
    value.Finite(f) -> {
      let f = f +. 0.0
      let n = value.float_to_int(f)
      case int.to_float(n) == f && n >= 0 && n <= 4_294_967_295 {
        True -> Ok(n)
        False -> reject_range(state, "Invalid array length")
      }
    }
    _ -> reject_range(state, "Invalid array length")
  }
}

/// OrdinaryDefineOwnProperty attribute validation for the array "length"
/// property, whose current descriptor is always a data property with
/// [[Enumerable]]: false, [[Configurable]]: false (§10.4.2, §10.1.6.3).
fn validate_length_attrs(
  state: State(host),
  desc: ParsedDesc,
  cur_writable: Bool,
) -> Result(Nil, #(JsValue, State(host))) {
  // §10.1.6.3 step 4.a: cannot make a non-configurable property configurable.
  use Nil <- result.try(case desc.configurable {
    Some(True) -> reject_define(state, "Cannot redefine property: length")
    _ -> Ok(Nil)
  })
  // §10.1.6.3 step 4.b: cannot change [[Enumerable]] (currently false).
  use Nil <- result.try(case desc.enumerable {
    Some(True) -> reject_define(state, "Cannot redefine property: length")
    _ -> Ok(Nil)
  })
  // §10.1.6.3 step 6: cannot convert non-configurable data → accessor.
  use Nil <- result.try(case desc_is_accessor(desc) {
    True -> reject_define(state, "Cannot redefine property: length")
    False -> Ok(Nil)
  })
  // §10.1.6.3 step 7.a.i: cannot change [[Writable]] false → true.
  case desc.writable, cur_writable {
    Some(True), False ->
      reject_define(state, "Cannot redefine property: length")
    _, _ -> Ok(Nil)
  }
}

/// Is the array's "length" writable? True unless a defineProperty call froze
/// it (stored as a Named("length") dict override consulted by all paths).
fn array_length_writable(h: Heap(host), ref: Ref) -> Bool {
  case heap.read(h, ref) {
    Some(ObjectSlot(properties:, ..)) ->
      case dict.get(properties, Named("length")) {
        Ok(DataProperty(writable:, ..)) -> writable
        _ -> True
      }
    _ -> True
  }
}

/// Write the array's tracked length, optionally freezing [[Writable]] to
/// false (ArraySetLength steps 14/19). The frozen flag lives as a
/// Named("length") dict override; its value is kept in sync with the kind.
fn write_array_length(
  state: State(host),
  target_ref: Ref,
  new_len: Int,
  freeze: Bool,
) -> State(host) {
  case heap.read(state.heap, target_ref) {
    Some(ObjectSlot(properties:, ..) as slot) -> {
      // seq: 0 / seq passthrough — array "length" never enumerates through
      // the seq-ordered named-key path (emitted as a special case).
      let properties = case freeze, dict.get(properties, Named("length")) {
        True, _ ->
          dict.insert(
            properties,
            Named("length"),
            DataProperty(
              value: value.from_int(new_len),
              writable: False,
              enumerable: False,
              configurable: False,
              seq: 0,
            ),
          )
        False, Ok(DataProperty(writable:, enumerable:, configurable:, seq:, ..))
        ->
          dict.insert(
            properties,
            Named("length"),
            DataProperty(
              value: value.from_int(new_len),
              writable:,
              enumerable:,
              configurable:,
              seq:,
            ),
          )
        False, _ -> properties
      }
      let h =
        heap.write(
          state.heap,
          target_ref,
          ObjectSlot(..slot, kind: ArrayObject(new_len), properties:),
        )
      State(..state, heap: h)
    }
    _ -> state
  }
}

/// ArraySetLength steps 13-19: shrink the array, deleting indices >= new_len
/// in descending order. Plain elements are implicitly configurable; dict
/// overrides may be non-configurable — the largest such index stops the
/// truncation (length becomes index + 1) and the operation throws TypeError
/// (step 17.b, surfaced via DefinePropertyOrThrow).
fn shrink_array(
  state: State(host),
  target_ref: Ref,
  new_len: Int,
  freeze: Bool,
) -> Result(State(host), #(JsValue, State(host))) {
  case heap.read(state.heap, target_ref) {
    Some(ObjectSlot(properties:, elements:, ..) as slot) -> {
      // Largest non-configurable index >= new_len blocks deletion at it.
      let blocked =
        dict.fold(properties, None, fn(acc, k, prop) {
          let non_configurable = !value.prop_configurable(prop)
          case k {
            Index(i) if i >= new_len ->
              case non_configurable {
                True ->
                  case acc {
                    Some(m) -> Some(int.max(m, i))
                    None -> Some(i)
                  }
                False -> acc
              }
            _ -> acc
          }
        })
      let final_len = case blocked {
        Some(b) -> b + 1
        None -> new_len
      }
      let elements = elements.truncate(elements, final_len)
      let properties =
        dict.filter(properties, fn(k, _prop) {
          case k {
            Index(i) -> i < final_len
            Named(_) -> True
          }
        })
      let h =
        heap.write(
          state.heap,
          target_ref,
          ObjectSlot(..slot, properties:, elements:),
        )
      let state =
        write_array_length(
          State(..state, heap: h),
          target_ref,
          final_len,
          freeze,
        )
      case blocked {
        // Step 17.b.iv: a delete failed — length stays at idx+1, then throw.
        Some(_) -> reject_define(state, "Cannot redefine property: length")
        None -> Ok(state)
      }
    }
    _ -> Ok(state)
  }
}

/// OrdinaryDefineOwnProperty (§10.1.6.1) + ValidateAndApplyPropertyDescriptor
/// (§10.1.6.3). For Array/Arguments index keys the "current" property may
/// live in the fast elements store (an implicit {w:T, e:T, c:T} data
/// property); results with exactly those attributes go back to elements,
/// anything else becomes a dict override (dict is checked first everywhere).
fn ordinary_define(
  state: State(host),
  target_ref: Ref,
  dkey: ObjectKey,
  desc: ParsedDesc,
) -> Result(State(host), #(JsValue, State(host))) {
  let is_accessor = desc_is_accessor(desc)
  let has_data = desc_is_data(desc)
  let desc_get = desc.get
  let desc_set = desc.set
  let desc_value = desc.value
  let desc_writable = desc.writable
  let desc_enumerable = desc.enumerable
  let desc_configurable = desc.configurable
  case heap.read(state.heap, target_ref) {
    Some(ObjectSlot(
      kind:,
      properties:,
      symbol_properties:,
      elements:,
      prototype:,
      extensible:,
    )) -> {
      let indexed_kind = case kind {
        ArrayObject(_) | value.ArgumentsObject(_) -> True
        _ -> False
      }
      let existing = case dkey {
        StringPropKey(pkey: Index(idx), ..) if indexed_kind ->
          case dict.get(properties, Index(idx)) {
            Ok(p) -> Ok(p)
            Error(Nil) ->
              case elements.get_option(elements, idx) {
                Some(v) ->
                  // seq: 0 — synthesized from the elements store; Index keys
                  // enumerate numerically, never by seq.
                  Ok(DataProperty(
                    value: v,
                    writable: True,
                    enumerable: True,
                    configurable: True,
                    seq: 0,
                  ))
                None -> Error(Nil)
              }
          }
        StringPropKey(pkey:, ..) -> dict.get(properties, pkey)
        SymbolPropKey(sym:) -> list.key_find(symbol_properties, sym)
      }
      // V8's error text: the bare key, no quotes.
      let key = case dkey {
        StringPropKey(display:, ..) -> display
        SymbolPropKey(_) -> "[Symbol]"
      }
      // §10.1.6.3 steps 2 + 5-11: is the change permitted at all? Exactly the
      // question `is_compatible_descriptor` (IsCompatiblePropertyDescriptor,
      // §10.1.6.2) answers — the same predicate the proxy and String-exotic
      // paths validate against, so all three agree on SameValue (notably that
      // -0 and +0 differ, which structural equality misses).
      use Nil <- result.try(case option.from_result(existing) {
        // Step 2: a new property needs an extensible object.
        None ->
          case extensible {
            True -> Ok(Nil)
            False ->
              reject_define(
                state,
                "Cannot define property " <> key <> ", object is not extensible",
              )
          }
        // Steps 5-11: a non-configurable property constrains the redefinition.
        Some(cur) ->
          case is_compatible_descriptor(extensible, desc, Some(cur)) {
            True -> Ok(Nil)
            False -> reject_define(state, "Cannot redefine property: " <> key)
          }
      })

      let enumerable =
        option.lazy_unwrap(desc_enumerable, fn() {
          existing |> result.map(value.prop_enumerable) |> result.unwrap(False)
        })
      let configurable =
        option.lazy_unwrap(desc_configurable, fn() {
          existing
          |> result.map(value.prop_configurable)
          |> result.unwrap(False)
        })
      // §10.1.11: redefining an existing key keeps its creation seq (and so
      // its enumeration position); a brand-new key gets a fresh one.
      let seq = case existing {
        Ok(old) -> value.prop_seq(old)
        Error(Nil) -> value.next_prop_seq()
      }
      let new_prop = case is_accessor, has_data {
        // Generic descriptor (§10.1.6.3): neither data nor accessor fields
        // present — keep the existing property's kind and fields, updating
        // only [[Enumerable]]/[[Configurable]]. A brand-new property created
        // from a generic descriptor is a data property with defaults.
        False, False ->
          case existing {
            Ok(DataProperty(value: v, writable: w, ..)) ->
              DataProperty(
                value: v,
                writable: w,
                enumerable:,
                configurable:,
                seq:,
              )
            Ok(AccessorProperty(get: g, set: s, ..)) ->
              AccessorProperty(get: g, set: s, enumerable:, configurable:, seq:)
            Error(Nil) ->
              DataProperty(
                value: JsUndefined,
                writable: False,
                enumerable:,
                configurable:,
                seq:,
              )
          }
        True, _ -> {
          // Accessor descriptor: merge get/set with existing accessor (if any).
          // Per §10.1.6.3 ValidateAndApplyPropertyDescriptor:
          // Fields not present in the new descriptor are inherited from the existing property.
          let getter = case desc_get {
            Some(JsUndefined) -> None
            None ->
              case existing {
                Ok(AccessorProperty(get: g, ..)) -> g
                _ -> None
              }
            Some(g) -> Some(g)
          }
          let setter = case desc_set {
            Some(JsUndefined) -> None
            None ->
              case existing {
                Ok(AccessorProperty(set: s, ..)) -> s
                _ -> None
              }
            Some(s) -> Some(s)
          }
          AccessorProperty(
            get: getter,
            set: setter,
            enumerable:,
            configurable:,
            seq:,
          )
        }
        False, True -> {
          // Data descriptor: merge value/writable with existing data property.
          // Per §10.1.6.3: absent fields inherit from existing property,
          // defaulting to undefined/false for new properties.
          let final_value = case desc_value {
            Some(v) -> v
            _ ->
              case existing {
                Ok(DataProperty(value: v, ..)) -> v
                _ -> JsUndefined
              }
          }
          let final_writable = case desc_writable {
            Some(w) -> w
            _ ->
              case existing {
                Ok(DataProperty(writable: w, ..)) -> w
                _ -> False
              }
          }
          DataProperty(
            value: final_value,
            writable: final_writable,
            enumerable:,
            configurable:,
            seq:,
          )
        }
      }

      // Write the new/updated property to the right store. Array/Arguments
      // index keys with default data attributes go to the fast elements
      // store; non-default attributes / accessors become dict overrides
      // (the element copy is removed so exactly one store owns the index).
      let #(properties, symbol_properties, elements) = case dkey {
        StringPropKey(pkey: Index(idx), ..) if indexed_kind ->
          case new_prop {
            DataProperty(
              value: v,
              writable: True,
              enumerable: True,
              configurable: True,
              ..,
            ) -> #(
              dict.delete(properties, Index(idx)),
              symbol_properties,
              elements.set(elements, idx, v),
            )
            _ -> #(
              dict.insert(properties, Index(idx), new_prop),
              symbol_properties,
              elements.delete(elements, idx),
            )
          }
        StringPropKey(pkey:, ..) -> #(
          dict.insert(properties, pkey, new_prop),
          symbol_properties,
          elements,
        )
        SymbolPropKey(sym:) -> #(
          properties,
          list.key_set(symbol_properties, sym, new_prop),
          elements,
        )
      }
      let h =
        heap.write(
          state.heap,
          target_ref,
          ObjectSlot(
            kind:,
            properties:,
            symbol_properties:,
            elements:,
            prototype:,
            extensible:,
          ),
        )
      Ok(State(..state, heap: h))
    }
    _ -> Ok(state)
  }
}

/// Failure channel for the [[DefineOwnProperty]] machinery (define_parsed and
/// friends), distinguishing the two ways a define can fail:
///
/// - `DefineRejected`: the define validated to boolean false (§10.1.6.3 /
///   §10.4.2.4 "return false"). DefinePropertyOrThrow throws the carried
///   TypeError; Reflect.defineProperty and the proxy no-trap path turn it
///   into `false`.
/// - `DefineThrew`: a genuine abrupt completion — ArraySetLength's RangeError
///   (§10.4.2.4 step 5), user code throwing during ToNumber of the length
///   value, or proxy trap/invariant errors. Always re-thrown, never `false`.
type DefineFailure {
  DefineRejected(JsValue)
  DefineThrew(JsValue)
}

/// The thrown value for contexts (DefinePropertyOrThrow) where both failure
/// kinds surface as a throw.
fn define_failure_value(failure: DefineFailure) -> JsValue {
  case failure {
    DefineRejected(err) -> err
    DefineThrew(err) -> err
  }
}

/// DefinePropertyOrThrow (§7.3.8) failure channel: a false
/// [[DefineOwnProperty]] result and a genuine abrupt completion both surface
/// as a throw. `result.map_error` adapter for `define_parsed`.
fn as_define_throw(
  err: #(DefineFailure, State(host)),
) -> #(JsValue, State(host)) {
  let #(failure, state) = err
  #(define_failure_value(failure), state)
}

/// Tag a raw thrown error as a validation rejection (boolean-false result).
fn as_rejected(err: #(JsValue, State(host))) -> #(DefineFailure, State(host)) {
  let #(thrown, state) = err
  #(DefineRejected(thrown), state)
}

/// Tag a raw thrown error as a genuine abrupt completion.
fn as_thrown(err: #(JsValue, State(host))) -> #(DefineFailure, State(host)) {
  let #(thrown, state) = err
  #(DefineThrew(thrown), state)
}

/// Helper to create a TypeError for defineProperty rejections.
fn reject_define(
  state: State(host),
  msg: String,
) -> Result(a, #(JsValue, State(host))) {
  Error(state.type_error_value(state, msg))
}

/// Helper to create a RangeError for ArraySetLength step 5 (§10.4.2.4).
fn reject_range(
  state: State(host),
  msg: String,
) -> Result(a, #(JsValue, State(host))) {
  Error(state.range_error_value(state, msg))
}

/// Helper for ToPropertyDescriptor: `if ? HasProperty(Obj, name) then
/// ? Get(Obj, name)` — §6.2.6.5 steps 3-8, in that order.
///
/// Returns #(Some(value), state) if the property exists, #(None, state) if absent.
/// The distinction matters because absent fields are not set in the descriptor,
/// while present-but-undefined fields ARE set (e.g. {value: undefined} is different
/// from {} — the former sets [[Value]] to undefined, the latter leaves it absent).
///
/// Both halves are trap-aware and can throw. The two are INTERLEAVED per field,
/// not batched: `Object.defineProperty(o, "k", new Proxy({}, {has(){…},
/// get(){…}}))` fires `has("enumerable")` and then, only if it claimed the
/// property, `get("enumerable")`, before `has("configurable")` is fired at all.
/// `parse_descriptor` owns the field order that makes that sequence normative.
fn read_desc_field(
  state: State(host),
  desc: JsValue,
  key: String,
) -> Result(#(option.Option(JsValue), State(host)), #(JsValue, State(host))) {
  case desc {
    JsObject(ref) -> {
      // Step 3/5/7/…: HasProperty(Obj, name).
      use #(present, state) <- result.try(object.has_property_stateful(
        state,
        ref,
        object.PkString(Named(key)),
      ))
      case present {
        False -> Ok(#(option.None, state))
        // Step 4/6/8/…: Get(Obj, name) — a getter or `get` trap runs here.
        True -> {
          use #(val, state) <- result.map(object.get_value_of(
            state,
            desc,
            Named(key),
          ))
          #(Some(val), state)
        }
      }
    }
    // ToPropertyDescriptor step 1 already rejected non-objects.
    _ -> Ok(#(option.None, state))
  }
}

/// Helper for ToPropertyDescriptor: reads a field and applies ToBoolean (§7.1.2).
/// Used for the "enumerable", "configurable", and "writable" fields.
///
/// Per §6.2.6.5 steps 4/6/10: the raw value is coerced via ToBoolean.
/// ToBoolean(x) returns false for: undefined, null, false, +0, -0, NaN, "".
/// Everything else (including objects, non-empty strings, non-zero numbers) is true.
fn read_desc_bool(
  state: State(host),
  desc: JsValue,
  key: String,
) -> Result(#(option.Option(Bool), State(host)), #(JsValue, State(host))) {
  use #(field, state) <- result.map(read_desc_field(state, desc, key))
  // Delegate to value.is_truthy for spec-correct ToBoolean — its numeric
  // check uses `!= 0.0` (Erlang `/=`) which correctly treats -0 as falsy,
  // unlike a `Finite(0.0)` pattern match (Erlang `=:=` distinguishes ±0).
  #(option.map(field, value.is_truthy), state)
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
              use ks, state <- state.try_op(enumerable_string_keys_stateful(
                state,
                ref,
              ))
              state.ok_array(state, list.map(ks, JsString))
            }
            False -> {
              use all_keys, state <- state.try_op(own_property_keys(state, ref))
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
          let ks = collect_own_keys(state.heap, ref, enumerable_only)
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
    // comes from object.string_length, the same source
    // string_exotic_own_property uses, so the key LIST and the per-key
    // [[GetOwnProperty]] lookups agree by construction.
    JsString(s) -> {
      let len = object.string_length(s)
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

/// Collect own property keys from an object — implements [[OwnPropertyKeys]]
/// (§10.1.11 for ordinary objects) with optional enumerable filtering.
///
/// [[OwnPropertyKeys]] ordering (§10.1.11):
///   1. For each own property key P of O that is an array index, in ascending
///      numeric index order, add P to keys.
///   2. For each own property key P of O that is a String but not an array index,
///      in ascending chronological order of property creation, add P to keys.
///   3. For each own property key P of O that is a Symbol, in ascending
///      chronological order of property creation, add P to keys.
///
/// When enumerable_only=True, this further implements the key-filtering from
/// EnumerableOwnProperties ( O, kind=key ) — §7.3.23:
///   3. For each element key of ownKeys, do
///      a. If key is a String, then
///         i. Let desc be ? O.[[GetOwnProperty]](key).
///         ii. If desc is not undefined and desc.[[Enumerable]] is true, then
///             — Append key to properties.
///
/// Symbol keys are not included here — they are stored separately in
/// symbol_properties (a creation-ordered list) and per spec step 3 ALL
/// symbol keys come after ALL string keys, so callers simply append them.
///
/// Ordering (including step-2 creation order for non-index string keys via
/// Property.seq) comes from object.own_string_keys_flagged — the single
/// engine-wide funnel for own string-key enumeration order.
pub fn collect_own_keys(
  heap: Heap(host),
  ref: Ref,
  enumerable_only: Bool,
) -> List(String) {
  let flagged = object.own_string_keys_flagged(heap, ref)
  case enumerable_only {
    True ->
      list.filter_map(flagged, fn(pair) {
        case pair {
          #(k, True) -> Ok(k)
          #(_, False) -> Error(Nil)
        }
      })
    False -> list.map(flagged, fn(pair) { pair.0 })
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
    False -> [index_object_key(i), ..string_index_object_keys(i + 1, len)]
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
      use prop_opt, state <- state.try_op(own_property_keyed(state, ref, key))
      #(state, Ok(JsBool(option.is_some(prop_opt))))
    }
    // Step 2: ToObject throws TypeError on null/undefined.
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    // String primitives: own keys are index chars + "length" — the shared
    // string-exotic own-property table decides.
    JsString(s) -> #(
      state,
      Ok(JsBool(option.is_some(string_exotic_own_property(s, key)))),
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
      use prop_opt, state <- state.try_op(own_property_keyed(state, ref, key))
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
        string_exotic_own_property(s, key)
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
    JsObject(ref) as receiver ->
      case object.as_proxy(state.heap, ref) {
        // Proxy: §7.3.23 step 1 via the ownKeys trap, then per key the
        // getOwnPropertyDescriptor trap immediately followed by the get
        // trap — the spec's observable interleaving (desc:a, get:a, desc:b,
        // get:b, …), which test262 asserts.
        Some(_) -> {
          use keys, state <- state.try_op(own_property_keys(state, ref))
          use items, state <- state.try_op(
            collect_enumerable_via_traps(
              state,
              ref,
              receiver,
              keys,
              combine,
              [],
            ),
          )
          cont(items, state)
        }
        None -> {
          // §7.3.23 steps 1 + 3.a/3.a.ii: own keys, pre-filtered here to enumerable string keys
          let ks = collect_own_keys(state.heap, ref, True)
          // §7.3.23 step 3: collect the per-property items
          use items, state <- state.try_op(
            collect_enumerable(state, ref, receiver, ks, combine, []),
          )
          cont(list.reverse(items), state)
        }
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
/// path derives from `object.string_length` (see `string_index_keys`,
/// `own_property_names`, `has_own`): both count CODEPOINTS, so
/// `Object.keys(s)` and `Object.values(s)` cannot disagree in length and
/// `Object.values(s)` cannot disagree with `s[i]` (`object.string_char_at`).
///
/// A grapheme cluster is not a JS string unit — `string.to_graphemes("e\u{301}")`
/// yields ONE entry where the engine has TWO indices — so it must never appear
/// on a String-exotic own-property path.
fn string_own_index_values(s: String) -> List(String) {
  object.string_explode(s)
}

/// EnumerableOwnProperties §7.3.23 step 3 for a proxy ref — per String key:
///   3.a.i:    desc = ? O.[[GetOwnProperty]](key)   (descriptor trap)
///   3.a.ii.1: value = ? Get(O, key)                (get trap, immediately)
/// Symbol keys are skipped (step 3.a "If key is a String"). Returns items in
/// trap-result order.
fn collect_enumerable_via_traps(
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
      collect_enumerable_via_traps(state, ref, receiver, rest, combine, acc)
    [StringPropKey(pkey:, display: s) as skey, ..rest] -> {
      use #(prop, state) <- result.try(own_property_keyed(state, ref, skey))
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
          collect_enumerable_via_traps(state, ref, receiver, rest, combine, [
            combine(s, val),
            ..acc
          ])
        }
        False ->
          collect_enumerable_via_traps(state, ref, receiver, rest, combine, acc)
      }
    }
  }
}

/// Collect own enumerable properties — implements §7.3.23 step 3.a.ii.2:
///   3.a.ii.2.a: "Let value be ? Get(O, key)."
///   then appends combine(key, value) to the accumulator.
/// Accumulates in reverse order (caller reverses).
fn collect_enumerable(
  state: State(host),
  ref: Ref,
  receiver: JsValue,
  keys: List(String),
  combine: fn(String, JsValue) -> a,
  acc: List(a),
) -> Result(#(List(a), State(host)), #(JsValue, State(host))) {
  case keys {
    [] -> Ok(#(acc, state))
    [k, ..rest] -> {
      // §7.3.23 step 3.a.ii.2.a: Let value be ? Get(O, key)
      use #(val, state) <- result.try(object.get_value(
        state,
        ref,
        key.canonical_key(k),
        receiver,
      ))
      collect_enumerable(state, ref, receiver, rest, combine, [
        combine(k, val),
        ..acc
      ])
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
  let #(proto_val, props_val) = case args {
    [p, q, ..] -> #(p, q)
    [p] -> #(p, JsUndefined)
    [] -> #(JsUndefined, JsUndefined)
  }
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
  let #(target, props_val) = case args {
    [t, p, ..] -> #(t, p)
    [t] -> #(t, JsUndefined)
    [] -> #(JsUndefined, JsUndefined)
  }
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
      use keys, state <- state.try_op(own_property_keys(state, props_ref))
      // Steps 4-5 (merged): For each key, read descriptor, validate, apply.
      define_props_loop(state, target_ref, props_ref, keys)
    }
    // Step 1: ToObject throws TypeError on null/undefined.
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    // Step 1: ToObject on primitives → wrapper with no own enumerable props → no-op.
    _ -> #(state, Ok(JsObject(target_ref)))
  }
}

/// Loop helper for ObjectDefineProperties (§20.1.2.3.1 steps 4-5 merged).
///
/// `remaining` is the [[OwnPropertyKeys]] result — String and Symbol values.
/// For each remaining key:
///   Step 4.a: Let propDesc be ? props.[[GetOwnProperty]](nextKey) — trap-aware.
///   Step 4.b: skip when propDesc is undefined or not enumerable.
///   Step 4.b.i: Let descObj be ? Get(props, nextKey).
///   Step 4.b.ii: Let desc be ? ToPropertyDescriptor(descObj).
///     — ToPropertyDescriptor (§6.2.6.5) requires descObj to be an Object;
///       if not, throw TypeError.
///   Step 5.a: Perform ? DefinePropertyOrThrow(O, key, desc).
///     — Handled by apply_descriptor.
///   Step 6: Return O.
fn define_props_loop(
  state: State(host),
  target_ref: Ref,
  props_ref: Ref,
  remaining: List(ObjectKey),
) -> #(State(host), Result(JsValue, JsValue)) {
  case remaining {
    // Step 6: Return O (all keys processed).
    [] -> #(state, Ok(JsObject(target_ref)))
    [dkey, ..rest] -> {
      // Step 4.a: propDesc = ? props.[[GetOwnProperty]](nextKey) — trap-aware.
      use prop, state <- state.try_op(own_property_keyed(state, props_ref, dkey))
      let enumerable =
        option.map(prop, value.prop_enumerable) |> option.unwrap(False)
      case enumerable {
        // Step 4.b: propDesc undefined or not enumerable → skip.
        False -> define_props_loop(state, target_ref, props_ref, rest)
        True -> {
          // Step 4.b.i: descObj = Get(props, nextKey) — a real [[Get]], so
          // accessor descriptors have their getter invoked (and symbol keys
          // reach define_parsed instead of being silently dropped).
          use desc_val, state <- state.try_op(object.get_prop_value(
            state,
            props_ref,
            object_key_prop_key(dkey),
            JsObject(props_ref),
          ))
          case desc_val {
            // Step 4.b.ii: ToPropertyDescriptor + step 5.a DefinePropertyOrThrow.
            JsObject(desc_ref) -> {
              use state <- state.try_state(apply_descriptor_keyed(
                state,
                target_ref,
                dkey,
                desc_ref,
              ))
              define_props_loop(state, target_ref, props_ref, rest)
            }
            // Step 4.b.ii: ToPropertyDescriptor throws if descObj is not an Object.
            _ ->
              state.type_error(state, "Property description must be an object")
          }
        }
      }
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
    JsObject(src_ref) as receiver ->
      case object.as_proxy(state.heap, src_ref) {
        // Proxy source: [[OwnPropertyKeys]] / [[GetOwnProperty]] / [[Get]]
        // must all run through the traps (§20.1.2.1 step 3.a.ii-iii).
        Some(_) -> {
          use #(keys, state) <- result.try(own_property_keys(state, src_ref))
          assign_proxy_keys(state, target_ref, src_ref, receiver, keys)
        }
        None -> {
          // Step 3.a.ii: Let keys be ? from.[[OwnPropertyKeys]]().
          // String keys first:
          let ks =
            collect_own_keys(state.heap, src_ref, True)
            |> list.map(key.canonical_key)
          // Step 3.a.iii: For each string key, copy it.
          use state <- result.try(
            assign_keys(
              state,
              target_ref,
              src_ref,
              receiver,
              ks,
              object.get_value,
              object.set_value,
              fn(_state, key) { key.key_to_string(key) },
            ),
          )
          // Symbol keys next (also enumerable-only):
          let sym_ks = collect_own_symbol_keys(state.heap, src_ref, True)
          assign_keys(
            state,
            target_ref,
            src_ref,
            receiver,
            sym_ks,
            object.get_symbol_value,
            object.set_symbol_value,
            symbol_key_label,
          )
        }
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

/// Key-copy loop for Object.assign. The [[GetOwnProperty]] + enumerable check
/// is pre-filtered by collect_own_keys / collect_own_symbol_keys
/// (enumerable_only=True), so we only iterate keys that are already known to
/// be own + enumerable. Generic over the key type: callers pass the matching
/// [[Get]] (invokes getters) and [[Set]] (invokes setters) — string-keyed
/// object.get_value/set_value or symbol-keyed get_symbol_value/set_symbol_value.
fn assign_keys(
  state: State(host),
  target_ref: Ref,
  src_ref: Ref,
  receiver: JsValue,
  keys: List(k),
  get: fn(State(host), Ref, k, JsValue) ->
    Result(#(JsValue, State(host)), #(JsValue, State(host))),
  set: fn(State(host), Ref, k, JsValue, JsValue) ->
    Result(#(State(host), Bool), #(JsValue, State(host))),
  label: fn(State(host), k) -> String,
) -> Result(State(host), #(JsValue, State(host))) {
  case keys {
    [] -> Ok(state)
    [k, ..rest] -> {
      // Step 2.a: Let propValue be ? Get(from, nextKey).
      use #(val, state) <- result.try(get(state, src_ref, k, receiver))
      // Step 2.b: Perform ? Set(to, nextKey, propValue, true) — the
      // throw=true flag: a false [[Set]] (frozen target, non-writable or
      // accessor-without-setter property) is a TypeError, not a silent skip.
      use #(state, ok) <- result.try(set(
        state,
        target_ref,
        k,
        val,
        JsObject(target_ref),
      ))
      use Nil <- result.try(case ok {
        True -> Ok(Nil)
        False -> assign_set_failed(state, label(state, k))
      })
      assign_keys(state, target_ref, src_ref, receiver, rest, get, set, label)
    }
  }
}

/// §20.1.2.1 step 3.a.iii.2.b `Perform ? Set(to, nextKey, propValue, true)`
/// with a false result — the TypeError required by the `throw` flag.
fn assign_set_failed(
  state: State(host),
  key: String,
) -> Result(a, #(JsValue, State(host))) {
  coerce.thrown_type_error(
    state,
    "Cannot assign to read only property '" <> key <> "' of object",
  )
}

/// Error-message label for a symbol key, e.g. "Symbol(foo)".
fn symbol_key_label(state: State(host), sym: value.SymbolId) -> String {
  symbol.descriptive_string(sym, state.ctx.symbol_descriptions)
}

/// Object.assign step 3.a.iii for a proxy source — per trap-provided key:
///   1. Let desc be ? from.[[GetOwnProperty]](nextKey).   (descriptor trap)
///   2. If desc is not undefined and desc.[[Enumerable]] is true, then
///     a. Let propValue be ? Get(from, nextKey).          (get trap)
///     b. Perform ? Set(to, nextKey, propValue, true).
fn assign_proxy_keys(
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
      use #(prop, state) <- result.try(own_property_keyed(state, src_ref, k))
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
            False -> assign_set_failed(state, symbol_key_label(state, sym))
          }
        }
      })
      assign_proxy_keys(state, target_ref, src_ref, receiver, rest)
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
          use #(keys, state) <- result.try(own_property_keys(state, src_ref))
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
  use #(prop, state) <- result.try(own_property_keyed(state, src_ref, k))
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

/// Collect enumerable own symbol keys from an object.
/// Implements the symbol portion of OrdinaryOwnPropertyKeys (§10.1.11.1 step 4)
/// with optional enumerable filtering.
pub fn collect_own_symbol_keys(
  heap: Heap(host),
  ref: Ref,
  enumerable_only: Bool,
) -> List(value.SymbolId) {
  case heap.read(heap, ref) {
    Some(ObjectSlot(symbol_properties:, ..)) ->
      symbol_properties
      |> list.filter_map(fn(pair) {
        let #(sym, prop) = pair
        case enumerable_only {
          True ->
            case prop {
              DataProperty(enumerable: True, ..)
              | AccessorProperty(enumerable: True, ..) -> Ok(sym)
              _ -> Error(Nil)
            }
          False -> Ok(sym)
        }
      })
    _ -> []
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
/// Implementation: Gleam's structural `==` on JsValue IS SameValue because:
///   - `JsNumber(NaN) == JsNumber(NaN)` is True (constructor equality on BEAM)
///   - `Finite(0.0) == Finite(-0.0)` is False (BEAM `=:=` distinguishes ±0)
///   - All other types use structural equality, matching SameValueNonNumber.
fn is(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(a, b) = case args {
    [x, y, ..] -> #(x, y)
    [x] -> #(x, JsUndefined)
    [] -> #(JsUndefined, JsUndefined)
  }
  // Step 1: Return SameValue(value1, value2).
  #(state, Ok(JsBool(a == b)))
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
  let #(target, key_val) = case args {
    [t, k, ..] -> #(t, k)
    [t] -> #(t, JsUndefined)
    [] -> #(JsUndefined, JsUndefined)
  }
  case target {
    JsObject(ref) -> {
      // Step 1: ToObject(O) — identity for objects.
      // Steps 2-3: ToPropertyKey + HasOwnProperty(obj, key) — trap-aware.
      use key, state <- try_to_property_key(state, key_val)
      use prop_opt, state <- state.try_op(own_property_keyed(state, ref, key))
      #(state, Ok(JsBool(option.is_some(prop_opt))))
    }
    // Step 1: ToObject throws TypeError on null/undefined.
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    // String primitives: own keys are index chars + "length" — the shared
    // string-exotic own-property table decides.
    JsString(s) -> {
      use key, state <- try_to_property_key(state, key_val)
      #(state, Ok(JsBool(option.is_some(string_exotic_own_property(s, key)))))
    }
    // Number/boolean/symbol have no own string-keyed properties.
    _ -> #(state, Ok(JsBool(False)))
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
  let #(target, proto_val) = case args {
    [t, p, ..] -> #(t, p)
    [t] -> #(t, JsUndefined)
    [] -> #(JsUndefined, JsUndefined)
  }
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
      use #(state, status) <- try_op_pair(set_prototype_of_stateful(
        state,
        ref,
        new_proto,
      ))
      case status {
        Ok(Nil) -> #(state, Ok(target))
        Error(fail) -> state.type_error(state, set_proto_fail_message(fail))
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
  let #(key_val, accessor) = case args {
    [k, a, ..] -> #(k, a)
    [k] -> #(k, JsUndefined)
    [] -> #(JsUndefined, JsUndefined)
  }
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
          case define_parsed(state, ref, dkey, parsed) {
            // Step 6: Return undefined.
            Ok(state) -> #(state, Ok(JsUndefined))
            Error(#(failure, state)) -> #(
              state,
              Error(define_failure_value(failure)),
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
  case own_property_keyed(state, ref, dkey) {
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

/// Why an object's [[SetPrototypeOf]] returned **false**. Callers that must
/// throw (Object.setPrototypeOf) turn each variant into its own TypeError;
/// callers that report a flag (Reflect.setPrototypeOf) collapse them all.
pub type SetProtoFail {
  NotExtensible
  Cyclic
  Immutable
  /// A proxy's `setPrototypeOf` trap returned falsish (§10.5.2 step 8).
  TrapRefused
}

/// Trap-aware **[[SetPrototypeOf]]** ( V ) — §10.5.2 for proxies, §10.1.2
/// OrdinarySetPrototypeOf otherwise. THE single dispatch: `Object
/// .setPrototypeOf`, `Reflect.setPrototypeOf` and `__proto__`'s setter all
/// route through it, so a proxy can never be handed to the ordinary
/// algorithm (which would rewrite the *proxy's* internal prototype slot and
/// never fire the trap).
pub fn set_prototype_of_stateful(
  state: State(host),
  ref: Ref,
  new_proto: Option(Ref),
) -> Result(#(State(host), Result(Nil, SetProtoFail)), #(JsValue, State(host))) {
  case object.as_proxy(state.heap, ref) {
    Some(#(target, handler)) -> {
      use #(state, ok) <- result.map(proxy_set_proto(
        state,
        target,
        handler,
        new_proto,
      ))
      case ok {
        True -> #(state, Ok(Nil))
        False -> #(state, Error(TrapRefused))
      }
    }
    None ->
      case ordinary_set_prototype_of(state, ref, new_proto) {
        Ok(state) -> Ok(#(state, Ok(Nil)))
        Error(fail) -> Ok(#(state, Error(fail)))
      }
  }
}

/// The TypeError message Object.setPrototypeOf raises for each refusal.
pub fn set_proto_fail_message(fail: SetProtoFail) -> String {
  case fail {
    NotExtensible -> "Cannot set prototype of a non-extensible object"
    Cyclic -> "Cyclic __proto__ value"
    Immutable -> "Immutable prototype object cannot have its prototype set"
    TrapRefused -> "'setPrototypeOf' on proxy: trap returned falsish"
  }
}

/// OrdinarySetPrototypeOf — ES2024 §10.1.2.1
/// (with §10.4.7 SetImmutablePrototype check for Object.prototype)
///
/// Ordinary objects only — proxies must go through `set_prototype_of_stateful`.
/// `TrapRefused` is unreachable here.
pub fn ordinary_set_prototype_of(
  state: State(host),
  ref: Ref,
  new_proto: Option(Ref),
) -> Result(State(host), SetProtoFail) {
  case heap.read(state.heap, ref) {
    Some(ObjectSlot(prototype: current, extensible:, ..))
      if new_proto != current
    -> {
      // §10.4.7.2 SetImmutablePrototype — Object.prototype is an Immutable
      // Prototype Exotic Object (§20.1.3): [[SetPrototypeOf]](V) returns
      // false unless SameValue(V, current). The same-value case already
      // fell through above, so any change here is rejected.
      use <- bool.guard(
        ref == state.builtins.object.prototype,
        Error(Immutable),
      )
      case extensible {
        False -> Error(NotExtensible)
        True ->
          case would_create_cycle(state.heap, ref, new_proto) {
            True -> Error(Cyclic)
            False -> {
              let heap = {
                use slot <- heap.update(state.heap, ref)
                case slot {
                  ObjectSlot(..) -> ObjectSlot(..slot, prototype: new_proto)
                  _ -> slot
                }
              }
              Ok(State(..state, heap:))
            }
          }
      }
    }
    // Same proto already, or ref isn't an ObjectSlot (unreachable) — no-op success.
    _ -> Ok(state)
  }
}

/// Cycle detection for OrdinarySetPrototypeOf — §10.1.2.1 step 7.
///
/// Implements the prototype chain walk:
///   7. Repeat, while done is false,
///      a. If p is null, set done to true.
///      b. Else if SameValue(p, O) is true, return false.
///      c. Else if p.[[GetPrototypeOf]] is not ordinary, set done to true.
///      d. Else set p to p.[[Prototype]].
///
/// Returns True if adding the link would create a cycle (step 7b triggers),
/// False if the chain terminates without hitting target (step 7a).
/// Step 7c (exotic objects) is not applicable — all our objects are ordinary.
fn would_create_cycle(
  heap: Heap(host),
  target_ref: Ref,
  new_proto: Option(Ref),
) -> Bool {
  case new_proto {
    // §10.1.2.1 step 7a: p is null → done, no cycle.
    None -> False
    // §10.1.2.1 step 7b: SameValue(p, O) → cycle detected.
    Some(p) if p == target_ref -> True
    Some(p) ->
      case heap.read(heap, p) {
        // §10.1.2.1 step 7c.i: p's [[GetPrototypeOf]] is not the ordinary
        // method (proxies trap it, §10.5.1) → done, no cycle detected.
        Some(ObjectSlot(kind: value.ProxyObject(..), ..)) -> False
        // §10.1.2.1 step 7c.ii: set p to p.[[Prototype]] and continue.
        Some(ObjectSlot(prototype: next, ..)) ->
          would_create_cycle(heap, target_ref, next)
        _ -> False
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
          use keys, state <- state.try_op(own_property_keys(state, ref))
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
/// (own_property_keys → collect_own_keys) excludes private names, so they
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
          use #(cur, state) <- result.map(own_property_keyed(state, ref, dkey))
          #(option.map(cur, frozen_level_desc), state)
        }
      })
      use state <- result.try(case desc {
        None -> Ok(state)
        // Step 7.a.i / 8.a.ii.3: ? DefinePropertyOrThrow(O, k, desc).
        Some(parsed) ->
          define_parsed(state, ref, dkey, parsed)
          |> result.map_error(as_define_throw)
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
          use keys, state <- state.try_op(own_property_keys(state, ref))
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
      use #(cur, state) <- result.try(own_property_keyed(state, ref, dkey))
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
      use state, key_val, val <- iter_protocol.add_entries_from_iterable(
        State(..state, heap:),
        JsObject(obj_ref),
        iterable,
      )
      // Step 3: ! CreateDataPropertyOrThrow(obj, ToPropertyKey(key), value).
      create_data_property(state, obj_ref, key_val, val)
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
      use keys, state <- state.try_op(own_property_keys(state, ref))
      // Steps 3-5: per key ? obj.[[GetOwnProperty]](key) (trap-aware) →
      // FromPropertyDescriptor, accumulated in key order.
      descriptors_from_keys(
        state,
        keys,
        object_proto,
        fn(state, dkey) { own_property_keyed(state, ref, dkey) },
        dict.new(),
        [],
      )
    }
    // String primitives: [[OwnPropertyKeys]] is the index chars then
    // "length" (§10.4.3.3); descriptors come from the shared string-exotic
    // own-property table.
    JsString(s) -> {
      let keys =
        list.append(string_index_object_keys(0, object.string_length(s)), [
          named_object_key("length"),
        ])
      descriptors_from_keys(
        state,
        keys,
        object_proto,
        fn(state, dkey) { Ok(#(string_exotic_own_property(s, dkey), state)) },
        dict.new(),
        [],
      )
    }
    // TODO(Deviation): number/boolean/symbol should produce {} (ToObject
    // wrapper with no own keys); we return undefined.
    _ -> #(state, Ok(JsUndefined))
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
            make_descriptor_object(state.heap, prop, object_proto)
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
          use all_keys, state <- state.try_op(own_property_keys(state, ref))
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
          let syms = collect_own_symbol_keys(state.heap, ref, False)
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
        to_object_key(state, key_val),
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

// ============================================================================
// Proxy trap machinery for descriptor/key operations — §10.5.5/.6/.11
// (lives here, not ops/object, because it needs the ParsedDesc/ObjectKey
// descriptor plumbing above)
// ============================================================================

/// ObjectKey → trap-argument JsValue (String or Symbol).
fn object_key_value(key: ObjectKey) -> JsValue {
  case key {
    StringPropKey(display:, ..) -> JsString(display)
    SymbolPropKey(sym) -> JsSymbol(sym)
  }
}

/// Human-readable key for invariant-violation error messages.
fn object_key_label(key: ObjectKey) -> String {
  case key {
    StringPropKey(display:, ..) -> "'" <> display <> "'"
    SymbolPropKey(_) -> "[symbol]"
  }
}

/// JsValue → ObjectKey, for the JS-visible entry points that still take a raw
/// value. `None` for anything that is not a String or a Symbol: §6.1.7 says
/// such a value is not a property key, so no object can have an own property
/// under it. (Never fabricate a key here — an empty-string stand-in would
/// alias a real "" property.)
fn object_key_of_value(val: JsValue) -> Option(ObjectKey) {
  case val {
    JsSymbol(sym) -> Some(SymbolPropKey(sym))
    JsString(s) -> Some(StringPropKey(key.canonical_key(s), s))
    _ -> None
  }
}

/// The `arc/vm/ops/object` PropKey view of an ObjectKey (drops the display
/// string) — for the ops-layer [[Get]]/[[Set]] entry points.
fn object_key_prop_key(k: ObjectKey) -> object.PropKey {
  case k {
    StringPropKey(pkey:, ..) -> object.PkString(pkey)
    SymbolPropKey(sym) -> object.PkSymbol(sym)
  }
}

/// The ObjectKey for a plain string key (no index parsing needed when the
/// caller already knows the shape).
fn named_object_key(name: String) -> ObjectKey {
  StringPropKey(Named(name), name)
}

/// The ObjectKey for an array-index key.
fn index_object_key(idx: Int) -> ObjectKey {
  StringPropKey(Index(idx), int.to_string(idx))
}

/// Keep the string keys of an [[OwnPropertyKeys]] result, paired with their
/// name — for the string-only reflections (Object.keys, for-in).
fn string_key_and_name(k: ObjectKey) -> Result(#(ObjectKey, String), Nil) {
  case k {
    StringPropKey(display:, ..) -> Ok(#(k, display))
    SymbolPropKey(_) -> Error(Nil)
  }
}

/// Non-trapping IsExtensible(target) read for invariant checks.
fn proxy_target_extensible(h: Heap(host), t: Ref) -> Bool {
  case heap.read(h, t) {
    Some(ObjectSlot(extensible:, ..)) -> extensible
    _ -> True
  }
}

/// §6.2.6.6 CompletePropertyDescriptor — fill absent fields with defaults,
/// yielding a concrete Property.
fn complete_descriptor(parsed: ParsedDesc) -> value.Property {
  case desc_is_accessor(parsed) {
    True ->
      AccessorProperty(
        get: case parsed.get {
          Some(JsUndefined) | None -> None
          Some(g) -> Some(g)
        },
        set: case parsed.set {
          Some(JsUndefined) | None -> None
          Some(s) -> Some(s)
        },
        enumerable: option.unwrap(parsed.enumerable, False),
        configurable: option.unwrap(parsed.configurable, False),
        seq: value.next_prop_seq(),
      )
    False ->
      DataProperty(
        value: option.unwrap(parsed.value, JsUndefined),
        writable: option.unwrap(parsed.writable, False),
        enumerable: option.unwrap(parsed.enumerable, False),
        configurable: option.unwrap(parsed.configurable, False),
        seq: value.next_prop_seq(),
      )
  }
}

/// §10.1.6.3 ValidateAndApplyPropertyDescriptor in validation-only mode
/// (IsCompatiblePropertyDescriptor §10.1.6.2): would defining `desc` over
/// `current` succeed on an object with the given extensibility?
fn is_compatible_descriptor(
  extensible: Bool,
  desc: ParsedDesc,
  current: Option(value.Property),
) -> Bool {
  case current {
    // Step 2: no current property — allowed iff extensible.
    None -> extensible
    Some(cur) ->
      case value.prop_configurable(cur) {
        // Configurable current property accepts any redefinition.
        True -> True
        False -> {
          // Step 4: non-configurable — reject configurable:true or an
          // enumerable flip.
          let bad_configurable = desc.configurable == Some(True)
          let bad_enumerable = case desc.enumerable {
            Some(e) -> e != value.prop_enumerable(cur)
            None -> False
          }
          use <- bool.guard(bad_configurable || bad_enumerable, False)
          let is_acc = desc_is_accessor(desc)
          let is_dat = desc_is_data(desc)
          // Step 5: generic descriptor — no further validation.
          use <- bool.guard(!is_acc && !is_dat, True)
          case cur {
            // Step 6: kind change on non-configurable is rejected.
            DataProperty(writable: cur_w, value: cur_v, ..) ->
              case is_acc {
                True -> False
                False ->
                  case cur_w {
                    True -> True
                    // Step 7: non-writable data — no writable:true, no
                    // value change (SameValue).
                    False ->
                      desc.writable != Some(True)
                      && case desc.value {
                        Some(v) -> value.same_value(v, cur_v)
                        None -> True
                      }
                  }
              }
            // Step 8: non-configurable accessor — get/set must be unchanged.
            AccessorProperty(get: cur_g, set: cur_s, ..) ->
              case is_dat {
                True -> False
                False ->
                  case desc.get {
                    Some(g) ->
                      value.same_value(g, option.unwrap(cur_g, JsUndefined))
                    None -> True
                  }
                  && case desc.set {
                    Some(s) ->
                      value.same_value(s, option.unwrap(cur_s, JsUndefined))
                    None -> True
                  }
              }
          }
        }
      }
  }
}

/// FromPropertyDescriptor (§6.2.6.4) on a PARTIAL descriptor — only fields
/// present in `parsed` become properties. This is the descriptor object the
/// defineProperty trap receives (§10.5.6 step 9).
fn from_parsed_descriptor(
  h: Heap(host),
  parsed: ParsedDesc,
  object_proto: Ref,
) -> #(Heap(host), Ref) {
  let entries =
    list.flatten([
      case parsed.value {
        Some(v) -> [#("value", value.data_property(v))]
        None -> []
      },
      case parsed.writable {
        Some(w) -> [#("writable", value.data_property(JsBool(w)))]
        None -> []
      },
      case parsed.get {
        Some(g) -> [#("get", value.data_property(g))]
        None -> []
      },
      case parsed.set {
        Some(s) -> [#("set", value.data_property(s))]
        None -> []
      },
      case parsed.enumerable {
        Some(e) -> [#("enumerable", value.data_property(JsBool(e)))]
        None -> []
      },
      case parsed.configurable {
        Some(c) -> [#("configurable", value.data_property(JsBool(c)))]
        None -> []
      },
    ])
  common.alloc_pojo(h, object_proto, entries)
}

/// §6.2.6.5 steps 12.b / 14.b: a `get`/`set` field that is present, is not
/// undefined and is not callable is a TypeError. `role` is "Getter"/"Setter".
///
/// Split out because each check must fire the instant its own field is read —
/// batching both until after all six reads would let a later field's getter or
/// `has` trap run after the spec has already thrown.
fn require_callable_accessor(
  state: State(host),
  field: Option(JsValue),
  role: String,
) -> Result(Nil, #(JsValue, State(host))) {
  case field {
    Some(f) ->
      case f == JsUndefined || helpers.is_callable(state.heap, f) {
        True -> Ok(Nil)
        False -> reject_define(state, role <> " must be a function")
      }
    None -> Ok(Nil)
  }
}

/// ToPropertyDescriptor (§6.2.6.5) on an arbitrary object value — reads the
/// six fields (invoking getters), validates get/set callability and the
/// accessor/data conflict.
///
/// The read ORDER is normative and observable — a Proxy descriptor sees the
/// `has`/`get` traps in exactly this sequence: enumerable, configurable, value,
/// writable, get, set. Step 12.b's getter check also runs BEFORE step 13 reads
/// "set", so `Object.defineProperty(o, "k", { get: 1, get set() { throw } })`
/// is a plain TypeError and never runs the `set` accessor.
fn parse_descriptor(
  state: State(host),
  desc_obj: JsValue,
) -> Result(#(ParsedDesc, State(host)), #(JsValue, State(host))) {
  // Steps 3-4.
  use #(desc_enumerable, state) <- result.try(read_desc_bool(
    state,
    desc_obj,
    "enumerable",
  ))
  // Steps 5-6.
  use #(desc_configurable, state) <- result.try(read_desc_bool(
    state,
    desc_obj,
    "configurable",
  ))
  // Steps 7-8.
  use #(desc_value, state) <- result.try(read_desc_field(
    state,
    desc_obj,
    "value",
  ))
  // Steps 9-10.
  use #(desc_writable, state) <- result.try(read_desc_bool(
    state,
    desc_obj,
    "writable",
  ))
  // Steps 11-12: read "get", then reject a non-callable one immediately.
  use #(desc_get, state) <- result.try(read_desc_field(state, desc_obj, "get"))
  use Nil <- result.try(require_callable_accessor(state, desc_get, "Getter"))
  // Steps 13-14: only now is "set" observed at all.
  use #(desc_set, state) <- result.try(read_desc_field(state, desc_obj, "set"))
  use Nil <- result.try(require_callable_accessor(state, desc_set, "Setter"))
  let parsed =
    ParsedDesc(
      get: desc_get,
      set: desc_set,
      value: desc_value,
      writable: desc_writable,
      enumerable: desc_enumerable,
      configurable: desc_configurable,
    )
  // Step 15: accessor and data attributes are mutually exclusive.
  use Nil <- result.map(case desc_is_accessor(parsed) && desc_is_data(parsed) {
    True ->
      reject_define(
        state,
        "Invalid property descriptor. Cannot both specify accessors and a value or writable attribute",
      )
    False -> Ok(Nil)
  })
  #(parsed, state)
}

/// Trap-aware [[GetOwnProperty]] — §10.1.5.1 for ordinary objects, §10.5.5
/// for proxies. The single entry point for getOwnPropertyDescriptor-style
/// reflection on possibly-proxy refs. A key that is neither String nor Symbol
/// (§6.1.7) can be no object's own property, so it reports undefined.
pub fn get_own_property_stateful(
  state: State(host),
  ref: Ref,
  key_val: JsValue,
) -> Result(#(Option(value.Property), State(host)), #(JsValue, State(host))) {
  case object_key_of_value(key_val) {
    Some(key) -> own_property_keyed(state, ref, key)
    None -> Ok(#(None, state))
  }
}

/// ObjectKey-keyed variant of get_own_property_stateful for in-module callers
/// that already resolved the key.
///
/// The engine's TDZ sentinel is filtered out HERE, once, so a returned
/// `Option(value.Property)` structurally cannot carry it: §10.4.6.5 says a
/// module namespace's [[GetOwnProperty]] performs [[Get]] on the binding, and
/// an uninitialized binding throws a ReferenceError before any descriptor
/// exists. Callers therefore never see `DataProperty(value: JsUninitialized)`
/// and cannot leak it (or the `undefined` it renders as) into JS.
pub fn own_property_keyed(
  state: State(host),
  ref: Ref,
  key: ObjectKey,
) -> Result(#(Option(value.Property), State(host)), #(JsValue, State(host))) {
  use #(prop, state) <- result.try(case object.as_proxy(state.heap, ref) {
    Some(#(target, handler)) ->
      proxy_get_own_property(state, target, handler, key)
    None -> Ok(#(get_own_property_by_key(state.heap, ref, key), state))
  })
  case prop {
    Some(DataProperty(value: value.JsUninitialized, ..)) ->
      Error(state.reference_error_value(state, tdz_message))
    _ -> Ok(#(prop, state))
  }
}

/// §10.5.5 Proxy [[GetOwnProperty]] ( P ).
fn proxy_get_own_property(
  state: State(host),
  target: Option(Ref),
  handler: Option(Ref),
  key: ObjectKey,
) -> Result(#(Option(value.Property), State(host)), #(JsValue, State(host))) {
  use #(t, h, trap, state) <- result.try(object.proxy_trap(
    state,
    target,
    handler,
    "getOwnPropertyDescriptor",
  ))
  case trap {
    // Step 6: no trap → target.[[GetOwnProperty]](P).
    None -> own_property_keyed(state, t, key)
    Some(trap_fn) -> {
      // Step 7: Call(trap, handler, « target, P »).
      use #(res, state) <- result.try(
        state.call(state, trap_fn, JsObject(h), [
          JsObject(t),
          object_key_value(key),
        ]),
      )
      let target_desc = get_own_property_by_key(state.heap, t, key)
      let ext = proxy_target_extensible(state.heap, t)
      case res {
        // Steps 10-11: trap says "absent".
        JsUndefined ->
          case target_desc {
            None -> Ok(#(None, state))
            Some(prop) ->
              case value.prop_configurable(prop), ext {
                False, _ -> {
                  use Nil <- result.map(reject_define(
                    state,
                    "'getOwnPropertyDescriptor' on proxy: trap returned undefined for property "
                      <> object_key_label(key)
                      <> " which is non-configurable in the proxy target",
                  ))
                  #(None, state)
                }
                True, False -> {
                  use Nil <- result.map(reject_define(
                    state,
                    "'getOwnPropertyDescriptor' on proxy: trap returned undefined for property "
                      <> object_key_label(key)
                      <> " which exists in the non-extensible proxy target",
                  ))
                  #(None, state)
                }
                True, True -> Ok(#(None, state))
              }
          }
        // Steps 12-17: trap returned a descriptor object — validate.
        JsObject(_) -> {
          use #(parsed, state) <- result.try(parse_descriptor(state, res))
          let completed = complete_descriptor(parsed)
          // Step 15: IsCompatiblePropertyDescriptor against the COMPLETED
          // descriptor (CompletePropertyDescriptor ran at step 14).
          let completed_parsed = case completed {
            DataProperty(
              value: v,
              writable: w,
              enumerable: e,
              configurable: c,
              ..,
            ) ->
              ParsedDesc(
                get: None,
                set: None,
                value: Some(v),
                writable: Some(w),
                enumerable: Some(e),
                configurable: Some(c),
              )
            AccessorProperty(get: g, set: s, enumerable: e, configurable: c, ..) ->
              ParsedDesc(
                get: Some(option.unwrap(g, JsUndefined)),
                set: Some(option.unwrap(s, JsUndefined)),
                value: None,
                writable: None,
                enumerable: Some(e),
                configurable: Some(c),
              )
          }
          use Nil <- result.try(
            case is_compatible_descriptor(ext, completed_parsed, target_desc) {
              False ->
                reject_define(
                  state,
                  "'getOwnPropertyDescriptor' on proxy: trap returned descriptor for property "
                    <> object_key_label(key)
                    <> " that is incompatible with the existing property in the proxy target",
                )
              True -> Ok(Nil)
            },
          )
          // Step 16: resultDesc.[[Configurable]] false requires a matching
          // non-configurable target property.
          use Nil <- result.try(case value.prop_configurable(completed) {
            True -> Ok(Nil)
            False ->
              case target_desc {
                None ->
                  reject_define(
                    state,
                    "'getOwnPropertyDescriptor' on proxy: trap reported non-configurability for property "
                      <> object_key_label(key)
                      <> " which is non-existent in the proxy target",
                  )
                Some(td) ->
                  case value.prop_configurable(td) {
                    True ->
                      reject_define(
                        state,
                        "'getOwnPropertyDescriptor' on proxy: trap reported non-configurability for property "
                          <> object_key_label(key)
                          <> " which is configurable in the proxy target",
                      )
                    False ->
                      // Step 16.b: writable:false requires target's
                      // writable:false too.
                      case parsed.writable, td {
                        Some(False), DataProperty(writable: True, ..) ->
                          reject_define(
                            state,
                            "'getOwnPropertyDescriptor' on proxy: trap reported non-writability for property "
                              <> object_key_label(key)
                              <> " which is writable in the proxy target",
                          )
                        _, _ -> Ok(Nil)
                      }
                  }
              }
          })
          Ok(#(Some(completed), state))
        }
        _ -> {
          use Nil <- result.map(reject_define(
            state,
            "'getOwnPropertyDescriptor' on proxy: trap returned neither object nor undefined for property "
              <> object_key_label(key),
          ))
          #(None, state)
        }
      }
    }
  }
}

/// §10.5.6 Proxy [[DefineOwnProperty]] ( P, Desc ). Returns the raw boolean —
/// callers decide whether false throws (DefinePropertyOrThrow) or not
/// (Reflect.defineProperty).
fn proxy_define_own_property(
  state: State(host),
  target: Option(Ref),
  handler: Option(Ref),
  key: ObjectKey,
  parsed: ParsedDesc,
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
  use #(t, h, trap, state) <- result.try(object.proxy_trap(
    state,
    target,
    handler,
    "defineProperty",
  ))
  case trap {
    // Step 6: no trap → target.[[DefineOwnProperty]](P, Desc). The raw
    // internal method returns a boolean — only a validation rejection maps to
    // `false` here (DefinePropertyOrThrow is applied by the CALLER of the
    // outermost proxy, not per level); genuine abrupt completions (e.g.
    // ArraySetLength's RangeError) propagate.
    None ->
      case object.as_proxy(state.heap, t) {
        Some(#(t2, h2)) -> proxy_define_own_property(state, t2, h2, key, parsed)
        None ->
          case define_parsed(state, t, key, parsed) {
            Ok(state) -> Ok(#(state, True))
            Error(#(DefineRejected(_), state)) -> Ok(#(state, False))
            Error(#(DefineThrew(thrown), state)) -> Error(#(thrown, state))
          }
      }
    Some(trap_fn) -> {
      // Step 9: descObj = FromPropertyDescriptor(Desc) — a fresh object
      // carrying only the present fields.
      let #(h2, desc_obj_ref) =
        from_parsed_descriptor(
          state.heap,
          parsed,
          state.builtins.object.prototype,
        )
      let state = State(..state, heap: h2)
      // Step 10: ToBoolean(Call(trap, handler, « target, P, descObj »)).
      use #(res, state) <- result.try(
        state.call(state, trap_fn, JsObject(h), [
          JsObject(t),
          object_key_value(key),
          JsObject(desc_obj_ref),
        ]),
      )
      case value.is_truthy(res) {
        False -> Ok(#(state, False))
        True -> {
          // Steps 12-16: invariants.
          let target_desc = get_own_property_by_key(state.heap, t, key)
          let ext = proxy_target_extensible(state.heap, t)
          let setting_config_false = parsed.configurable == Some(False)
          case target_desc {
            None -> {
              use Nil <- result.try(case ext {
                False ->
                  reject_define(
                    state,
                    "'defineProperty' on proxy: trap returned truish for adding property "
                      <> object_key_label(key)
                      <> " to the non-extensible proxy target",
                  )
                True -> Ok(Nil)
              })
              use Nil <- result.map(case setting_config_false {
                True ->
                  reject_define(
                    state,
                    "'defineProperty' on proxy: trap returned truish for defining non-configurable property "
                      <> object_key_label(key)
                      <> " which is either non-existent or configurable in the proxy target",
                  )
                False -> Ok(Nil)
              })
              #(state, True)
            }
            Some(cur) -> {
              use Nil <- result.try(
                case is_compatible_descriptor(ext, parsed, Some(cur)) {
                  False ->
                    reject_define(
                      state,
                      "'defineProperty' on proxy: trap returned truish for adding property "
                        <> object_key_label(key)
                        <> " that is incompatible with the existing property in the proxy target",
                    )
                  True -> Ok(Nil)
                },
              )
              use Nil <- result.try(
                case setting_config_false && value.prop_configurable(cur) {
                  True ->
                    reject_define(
                      state,
                      "'defineProperty' on proxy: trap returned truish for defining non-configurable property "
                        <> object_key_label(key)
                        <> " which is either non-existent or configurable in the proxy target",
                    )
                  False -> Ok(Nil)
                },
              )
              // Step 16.c: writable:false over a non-configurable writable
              // data property is rejected.
              use Nil <- result.map(case cur {
                DataProperty(configurable: False, writable: True, ..) ->
                  case parsed.writable {
                    Some(False) ->
                      reject_define(
                        state,
                        "'defineProperty' on proxy: trap returned truish for defining non-writable property "
                          <> object_key_label(key)
                          <> " which is writable in the proxy target",
                      )
                    _ -> Ok(Nil)
                  }
                _ -> Ok(Nil)
              })
              #(state, True)
            }
          }
        }
      }
    }
  }
}

/// Reflect.defineProperty semantics (§28.1.6): ? ToPropertyKey +
/// ? ToPropertyDescriptor (their throws propagate), then
/// ? target.[[DefineOwnProperty]] — a validation rejection becomes `false`,
/// while genuine abrupt completions (ArraySetLength's RangeError, user code
/// throwing, proxy trap/invariant errors) are re-thrown.
pub fn define_property_bool(
  state: State(host),
  ref: Ref,
  key_val: JsValue,
  desc_ref: Ref,
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
  use #(dkey, state) <- result.try(to_object_key(state, key_val))
  use #(parsed, state) <- result.try(parse_descriptor(state, JsObject(desc_ref)))
  case object.as_proxy(state.heap, ref) {
    Some(#(target, handler)) ->
      proxy_define_own_property(state, target, handler, dkey, parsed)
    None ->
      case define_parsed(state, ref, dkey, parsed) {
        Ok(state) -> Ok(#(state, True))
        // [[DefineOwnProperty]] validated to false → false (not rethrown).
        Error(#(DefineRejected(_), state)) -> Ok(#(state, False))
        Error(#(DefineThrew(thrown), state)) -> Error(#(thrown, state))
      }
  }
}

/// §7.3.7 CreateDataPropertyOrThrow ( O, P, V ) in the file's CPS shape.
///
/// CreateDataProperty (§7.3.5) is [[DefineOwnProperty]](P, {[[Value]]: V,
/// [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true}); the
/// OrThrow wrapper converts a `false` result (non-extensible target,
/// incompatible non-configurable existing key, falsy proxy `defineProperty`
/// trap) into the realm's TypeError. Builtins that create result properties
/// (Object.fromEntries, Object.groupBy, `stack` setter, ...) MUST funnel
/// through here rather than hand-rolling the descriptor — a hand-rolled copy
/// can silently get the attributes wrong (e.g. a non-enumerable property) or
/// drop the `false` result.
pub fn create_data_property_or_throw(
  state: State(host),
  ref: Ref,
  key_val: JsValue,
  val: JsValue,
  cont: fn(State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case create_data_property(state, ref, key_val, val) {
    Ok(state) -> cont(state)
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// §7.3.7 CreateDataPropertyOrThrow as a plain fallible op — the shape the
/// `iter_protocol` entry sinks want. `create_data_property_or_throw` is the
/// CPS wrapper over it.
pub fn create_data_property(
  state: State(host),
  ref: Ref,
  key_val: JsValue,
  val: JsValue,
) -> Result(State(host), #(JsValue, State(host))) {
  // §7.3.5 builds the Property Descriptor RECORD directly — it never runs
  // ToPropertyDescriptor over a descriptor object. Materializing a real JS
  // POJO here and re-parsing it would (a) make the internal descriptor
  // observably inherit `get`/`set` from a user-extended %Object.prototype%
  // (breaking Object.fromEntries / Object.groupBy under a plain
  // `Object.prototype.get = ...`), and (b) cost a heap allocation plus a
  // 6-field re-parse per created property inside builtin loops.
  let parsed =
    ParsedDesc(
      get: None,
      set: None,
      value: Some(val),
      writable: Some(True),
      enumerable: Some(True),
      configurable: Some(True),
    )
  use #(dkey, state) <- result.try(to_object_key(state, key_val))
  // Trap-aware [[DefineOwnProperty]] on the already-parsed record —
  // the same tail define_property_bool dispatches to after parsing.
  let defined = case object.as_proxy(state.heap, ref) {
    Some(#(target, handler)) ->
      proxy_define_own_property(state, target, handler, dkey, parsed)
    None ->
      case define_parsed(state, ref, dkey, parsed) {
        Ok(state) -> Ok(#(state, True))
        // [[DefineOwnProperty]] validated to false → false (not rethrown).
        Error(#(DefineRejected(_), state)) -> Ok(#(state, False))
        Error(#(DefineThrew(thrown), state)) -> Error(#(thrown, state))
      }
  }
  case defined {
    Ok(#(state, True)) -> Ok(state)
    // §7.3.7 step 3: success is false → throw a TypeError exception.
    Ok(#(state, False)) ->
      Error(state.type_error_value(
        state,
        "Cannot create property " <> object_key_label(dkey),
      ))
    Error(#(thrown, state)) -> Error(#(thrown, state))
  }
}

/// Trap-aware [[OwnPropertyKeys]] — §10.1.11 for ordinary objects, §10.5.11
/// for proxies. Returns the full key list: integer-index strings ascending,
/// then named strings, then symbols (or whatever order the proxy trap chose).
///
/// Every element is a String or a Symbol *by construction* — the ordinary path
/// builds them from the property tables, the proxy path validates the trap
/// result (§7.3.19). Hence `ObjectKey`, not `JsValue`: no caller has to invent
/// a "not a key" branch, and none can silently drop one.
pub fn own_property_keys(
  state: State(host),
  ref: Ref,
) -> Result(#(List(ObjectKey), State(host)), #(JsValue, State(host))) {
  case object.as_proxy(state.heap, ref) {
    Some(#(target, handler)) -> proxy_own_keys(state, target, handler)
    None -> {
      let strings =
        collect_own_keys(state.heap, ref, False)
        |> list.map(fn(s) { StringPropKey(key.canonical_key(s), s) })
      let syms =
        collect_own_symbol_keys(state.heap, ref, False)
        |> list.map(SymbolPropKey)
      Ok(#(list.append(strings, syms), state))
    }
  }
}

/// [[OwnPropertyKeys]] as JS values — CreateArrayFromList's input, i.e. the
/// JS-visible boundary (Reflect.ownKeys, the keyed Promise combinators).
/// Prefer `own_property_keys` for anything that then looks the key back up.
pub fn own_keys_stateful(
  state: State(host),
  ref: Ref,
) -> Result(#(List(JsValue), State(host)), #(JsValue, State(host))) {
  use #(keys, state) <- result.map(own_property_keys(state, ref))
  #(list.map(keys, object_key_value), state)
}

/// §10.5.11 Proxy [[OwnPropertyKeys]] ( ).
fn proxy_own_keys(
  state: State(host),
  target: Option(Ref),
  handler: Option(Ref),
) -> Result(#(List(ObjectKey), State(host)), #(JsValue, State(host))) {
  use #(t, h, trap, state) <- result.try(object.proxy_trap(
    state,
    target,
    handler,
    "ownKeys",
  ))
  case trap {
    // Step 6: no trap → target.[[OwnPropertyKeys]]().
    None -> own_property_keys(state, t)
    Some(trap_fn) -> {
      // Step 7: Call(trap, handler, « target »).
      use #(res, state) <- result.try(
        state.call(state, trap_fn, JsObject(h), [JsObject(t)]),
      )
      // Step 8: CreateListFromArrayLike(trapResultArray, property-key).
      use #(keys, state) <- result.try(keys_from_array_like(state, res))
      // Step 9: duplicate entries are rejected.
      use Nil <- result.try(case has_duplicate_keys(keys, []) {
        True ->
          reject_define(
            state,
            "'ownKeys' on proxy: trap returned duplicate entries",
          )
        False -> Ok(Nil)
      })
      let ext = proxy_target_extensible(state.heap, t)
      // Steps 11-14: split target keys by configurability.
      use #(target_keys, state) <- result.try(own_property_keys(state, t))
      let #(nonconf, conf) =
        list.partition(target_keys, fn(k) {
          case get_own_property_by_key(state.heap, t, k) {
            Some(prop) -> !value.prop_configurable(prop)
            None -> False
          }
        })
      // Step 17: every non-configurable target key must be reported.
      use Nil <- result.try(
        list.try_each(nonconf, fn(k) {
          case list.contains(keys, k) {
            True -> Ok(Nil)
            False ->
              reject_define(
                state,
                "'ownKeys' on proxy: trap result did not include "
                  <> object_key_label(k)
                  <> ", a non-configurable key of the proxy target",
              )
          }
        }),
      )
      case ext {
        // Step 18: extensible target — no further checks.
        True -> Ok(#(keys, state))
        False -> {
          // Step 19: every (configurable) target key must be reported…
          use Nil <- result.try(
            list.try_each(conf, fn(k) {
              case list.contains(keys, k) {
                True -> Ok(Nil)
                False ->
                  reject_define(
                    state,
                    "'ownKeys' on proxy: trap result did not include "
                      <> object_key_label(k)
                      <> ", a key of the non-extensible proxy target",
                  )
              }
            }),
          )
          // Steps 20-21: …and no extra keys may be invented.
          let extras =
            list.filter(keys, fn(k) { !list.contains(target_keys, k) })
          use Nil <- result.map(case extras {
            [] -> Ok(Nil)
            [_, ..] ->
              reject_define(
                state,
                "'ownKeys' on proxy: trap returned extra keys but proxy target is non-extensible",
              )
          })
          #(keys, state)
        }
      }
    }
  }
}

/// True when `keys` contains the same String/Symbol value twice.
fn has_duplicate_keys(keys: List(ObjectKey), seen: List(ObjectKey)) -> Bool {
  case keys {
    [] -> False
    [k, ..rest] ->
      case list.contains(seen, k) {
        True -> True
        False -> has_duplicate_keys(rest, [k, ..seen])
      }
  }
}

/// CreateListFromArrayLike(obj, property-key) — §7.3.19. Each element must be
/// a String or Symbol, else TypeError — so the result is a list of ObjectKeys.
fn keys_from_array_like(
  state: State(host),
  val: JsValue,
) -> Result(#(List(ObjectKey), State(host)), #(JsValue, State(host))) {
  case val {
    JsObject(arr_ref) ->
      case heap.read_array_like(state.heap, arr_ref) {
        // Fast path: genuine array-backed object — read elements directly.
        Some(#(length, els)) -> {
          let items = gather_array_like(els, 0, length, [])
          use keys <- result.map(
            list.try_map(items, validate_trap_key(state, _)),
          )
          #(keys, state)
        }
        // Generic §7.3.19 path: ToLength(? Get(obj, "length")), then
        // ? Get(obj, ToString(i)) for each index — honors array-like plain
        // objects ({0:'a', length:1}), getter-backed results, and proxies
        // wrapping arrays.
        None -> {
          // Step 3: Let len be ? LengthOfArrayLike(obj).
          use #(len_val, state) <- result.try(object.get_value(
            state,
            arr_ref,
            Named("length"),
            val,
          ))
          use #(len, state) <- result.try(trap_result_length(state, len_val))
          gather_keys_via_get(state, val, arr_ref, 0, len, [])
        }
      }
    _ -> {
      use Nil <- result.map(reject_define(
        state,
        "CreateListFromArrayLike called on non-object",
      ))
      #([], state)
    }
  }
}

/// §7.3.19 step 7.c (property-key element types): each element of the ownKeys
/// trap result must be a String or a Symbol — the one place a JsValue becomes
/// an ObjectKey, so downstream code never re-checks.
fn validate_trap_key(
  state: State(host),
  item: JsValue,
) -> Result(ObjectKey, #(JsValue, State(host))) {
  case object_key_of_value(item) {
    Some(k) -> Ok(k)
    None ->
      reject_define(
        state,
        "'ownKeys' on proxy: trap returned a non-String, non-Symbol key",
      )
  }
}

/// §7.1.20 ToLength for the ownKeys trap result, additionally bounded by the
/// iteration budget — the generic path performs one observable Get per index.
fn trap_result_length(
  state: State(host),
  val: JsValue,
) -> Result(#(Int, State(host)), #(JsValue, State(host))) {
  use #(n, state) <- result.try(coerce.js_to_number(state, val))
  let len = case n {
    value.Finite(f) ->
      int.clamp(value.float_to_int(f), 0, limits.max_safe_integer)
    value.Infinity -> limits.max_safe_integer
    value.NaN | value.NegInfinity -> 0
  }
  case len > limits.max_iteration {
    True ->
      Error(state.range_error_value(
        state,
        "'ownKeys' on proxy: trap result length exceeds iteration budget",
      ))
    False -> Ok(#(len, state))
  }
}

/// §7.3.19 steps 6-8 (generic path): read indices 0..len-1 with observable
/// Get, validating each element is a String or Symbol.
fn gather_keys_via_get(
  state: State(host),
  obj: JsValue,
  arr_ref: Ref,
  idx: Int,
  length: Int,
  acc: List(ObjectKey),
) -> Result(#(List(ObjectKey), State(host)), #(JsValue, State(host))) {
  case idx >= length {
    True -> Ok(#(list.reverse(acc), state))
    False -> {
      // Step 7.b: Let next be ? Get(obj, ToString(index)).
      use #(item, state) <- result.try(object.get_value(
        state,
        arr_ref,
        Index(idx),
        obj,
      ))
      // Step 7.c: validate the element type.
      use k <- result.try(validate_trap_key(state, item))
      gather_keys_via_get(state, obj, arr_ref, idx + 1, length, [k, ..acc])
    }
  }
}

fn gather_array_like(
  els: JsElements,
  idx: Int,
  length: Int,
  acc: List(JsValue),
) -> List(JsValue) {
  case idx >= length {
    True -> list.reverse(acc)
    False ->
      gather_array_like(els, idx + 1, length, [elements.get(els, idx), ..acc])
  }
}

/// EnumerableOwnProperties(O, key) for possibly-proxy objects: own keys via
/// [[OwnPropertyKeys]] (trap), then per-key [[GetOwnProperty]] (trap) to
/// filter for enumerable STRING keys.
pub fn enumerable_string_keys_stateful(
  state: State(host),
  ref: Ref,
) -> Result(#(List(String), State(host)), #(JsValue, State(host))) {
  use #(keys, state) <- result.try(own_property_keys(state, ref))
  let string_keys = list.filter_map(keys, string_key_and_name)
  list.try_fold(string_keys, #([], state), fn(acc, entry) {
    let #(found, state) = acc
    let #(k, s) = entry
    use #(prop, state) <- result.map(own_property_keyed(state, ref, k))
    case prop {
      Some(p) ->
        case value.prop_enumerable(p) {
          True -> #([s, ..found], state)
          False -> #(found, state)
        }
      None -> #(found, state)
    }
  })
  |> result.map(fn(acc) {
    let #(found, state) = acc
    #(list.reverse(found), state)
  })
}

/// §10.5.2 Proxy [[SetPrototypeOf]] — trap, falling through to
/// target.[[SetPrototypeOf]] (which may itself be a proxy) when no trap is
/// defined. Callers use `set_prototype_of_stateful`, which owns the
/// proxy-vs-ordinary dispatch this recurses back into.
fn proxy_set_proto(
  state: State(host),
  target: Option(Ref),
  handler: Option(Ref),
  new_proto: Option(Ref),
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
  let proto_val = case new_proto {
    Some(p) -> JsObject(p)
    None -> JsNull
  }
  use #(state, fallthrough, ok) <- result.try(object.proxy_set_prototype_of(
    state,
    target,
    handler,
    proto_val,
  ))
  case fallthrough {
    None -> Ok(#(state, ok))
    // Step 6: no trap → target.[[SetPrototypeOf]](V).
    Some(t) -> {
      use #(state, status) <- result.map(set_prototype_of_stateful(
        state,
        t,
        new_proto,
      ))
      #(state, result.is_ok(status))
    }
  }
}

/// EnumerateObjectProperties (§14.7.5.10) for proxy-containing chains: own
/// enumerable string keys via the traps at each prototype level, with
/// shadowing (a key seen at a shallower level hides deeper ones, enumerable
/// or not).
pub fn enumerate_keys_stateful(
  state: State(host),
  ref: Ref,
) -> Result(#(List(String), State(host)), #(JsValue, State(host))) {
  use #(acc_rev, _seen, state) <- result.map(
    enumerate_chain(state, ref, [], []),
  )
  #(list.reverse(acc_rev), state)
}

fn enumerate_chain(
  state: State(host),
  ref: Ref,
  seen: List(String),
  acc_rev: List(String),
) -> Result(#(List(String), List(String), State(host)), #(JsValue, State(host))) {
  use #(keys, state) <- result.try(own_property_keys(state, ref))
  let string_keys = list.filter_map(keys, string_key_and_name)
  use #(seen, acc_rev, state) <- result.try(
    list.try_fold(string_keys, #(seen, acc_rev, state), fn(st, entry) {
      let #(seen, acc_rev, state) = st
      let #(k, s) = entry
      case list.contains(seen, s) {
        True -> Ok(#(seen, acc_rev, state))
        False -> {
          use #(prop, state) <- result.map(own_property_keyed(state, ref, k))
          case prop {
            Some(p) ->
              case value.prop_enumerable(p) {
                True -> #([s, ..seen], [s, ..acc_rev], state)
                False -> #([s, ..seen], acc_rev, state)
              }
            None -> #([s, ..seen], acc_rev, state)
          }
        }
      }
    }),
  )
  use #(proto_val, state) <- result.try(object.get_prototype_of_stateful(
    state,
    ref,
  ))
  case proto_val {
    JsObject(p) -> enumerate_chain(state, p, seen, acc_rev)
    _ -> Ok(#(acc_rev, seen, state))
  }
}
