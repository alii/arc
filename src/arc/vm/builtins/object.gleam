import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers.{first_arg_or_undefined}
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/limits
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/ops/property
import arc/vm/ops/typed_array_elements
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsElements, type JsValue, type ObjectNativeFn, type Ref, AccessorProperty,
  ArrayObject, DataProperty, Dispatch, FunctionObject, GeneratorObject, Index,
  JsBool, JsNull, JsNumber, JsObject, JsString, JsSymbol, JsUndefined, Named,
  NativeFunction, ObjectAssign, ObjectConstructor, ObjectCreate,
  ObjectDefineProperties, ObjectDefineProperty, ObjectEntries, ObjectFreeze,
  ObjectFromEntries, ObjectGetOwnPropertyDescriptor,
  ObjectGetOwnPropertyDescriptors, ObjectGetOwnPropertyNames,
  ObjectGetOwnPropertySymbols, ObjectGetPrototypeOf, ObjectHasOwn, ObjectIs,
  ObjectIsExtensible, ObjectIsFrozen, ObjectIsSealed, ObjectKeys, ObjectNative,
  ObjectPreventExtensions, ObjectPrototypeHasOwnProperty,
  ObjectPrototypePropertyIsEnumerable, ObjectPrototypeToString,
  ObjectPrototypeValueOf, ObjectSeal, ObjectSetPrototypeOf, ObjectSlot,
  ObjectValues, OrdinaryObject,
}
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import gleam/set
import gleam/string

/// V8/Node's standard ToObject failure message.
const cannot_convert = "Cannot convert undefined or null to object"

/// Thrown when a Module Namespace operation touches a still-uninitialized
/// (TDZ) export binding (§10.4.6 [[Get]] / [[GetOwnProperty]]).
const tdz_message = "Cannot access module export before initialization"

/// Resolved ToPropertyKey result — unifies string and symbol keys for
/// apply_descriptor so it can look up/write to the right dict without
/// threading two code paths through the validation logic.
type DefineKey {
  StringKey(pkey: value.PropertyKey, display: String)
  SymbolKey(sym: value.SymbolId)
}

/// ToPropertyKey (§7.1.19) resolved to this module's DefineKey (a PropKey
/// plus a cached display string for error messages). The spec-order
/// conversion — ToPrimitive(argument, string) FIRST, then the Symbol check,
/// so `{[Symbol.toPrimitive]: () => sym}` used as a key produces a symbol
/// key — is property.to_prop_key; this only re-shapes the result.
fn to_define_key(
  state: State(host),
  key_val: JsValue,
) -> Result(#(DefineKey, State(host)), #(JsValue, State(host))) {
  use #(pk, state) <- result.map(property.to_prop_key(state, key_val))
  #(prop_key_to_define_key(pk), state)
}

/// The DefineKey form of an already-resolved PropKey.
fn prop_key_to_define_key(pk: object.PropKey) -> DefineKey {
  case pk {
    object.PkSymbol(sym) -> SymbolKey(sym)
    object.PkString(pkey) -> StringKey(pkey, value.key_to_string(pkey))
  }
}

/// CPS form of `to_define_key` so callers can `use` before `case this`,
/// preserving spec ordering (hasOwnProperty does ToPropertyKey step 1,
/// ToObject step 2).
fn try_to_property_key(
  state: State(host),
  key_val: JsValue,
  cont: fn(DefineKey, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case to_define_key(state, key_val) {
    Error(#(thrown, state)) -> #(state, Error(thrown))
    Ok(#(dkey, state)) -> cont(dkey, state)
  }
}

/// [[GetOwnProperty]] dispatching on a resolved DefineKey.
fn get_own_property_by_key(
  heap: Heap(host),
  ref: Ref,
  key: DefineKey,
) -> Option(value.Property) {
  case key {
    SymbolKey(sym) -> object.get_own_symbol_property(heap, ref, sym)
    // Private elements (NUL-marker keys, see value.private_key) are stored in
    // the ordinary property table but are invisible to reflection (spec keeps
    // them in [[PrivateElements]]).
    StringKey(pkey: Named("\u{0}" <> _), ..) -> None
    StringKey(pkey:, ..) -> object.get_own_property(heap, ref, pkey)
  }
}

/// CPS wrapper for `object.get_value`. Use with `use` syntax:
///   use val, state <- try_get(state, ref, key, receiver)
/// Propagates thrown errors as `#(state, Error(thrown))`.
fn try_get(
  state: State(host),
  ref: Ref,
  key: String,
  receiver: JsValue,
  cont: fn(JsValue, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case object.get_value(state, ref, value.canonical_key(key), receiver) {
    Ok(#(val, state)) -> cont(val, state)
    Error(#(thrown, state)) -> #(state, Error(thrown))
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
  let #(target, key_val) = case args {
    [t, k, ..] -> #(t, k)
    [t] -> #(t, JsUndefined)
    [] -> #(JsUndefined, JsUndefined)
  }
  // Step 2: Let key be ? ToPropertyKey(P). (Spec orders this after step 1
  // ToObject, but since our ToObject is a case match with no side effects
  // before the null/undefined throw, resolving the key first is observably
  // equivalent and keeps the code flat.)
  use key, state <- try_to_property_key(state, key_val)
  case target {
    JsObject(ref) -> {
      // Step 3: Let desc be ? obj.[[GetOwnProperty]](key) — trap-aware.
      use prop_opt, state <- state.try_op(own_property_via(state, ref, key))
      case prop_opt {
        // §10.4.6.5: a namespace [[GetOwnProperty]] performs [[Get]], so a TDZ
        // binding throws before a descriptor can be built.
        Some(DataProperty(value: value.JsUninitialized, ..)) ->
          state.reference_error(state, tdz_message)
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
    // Step 1: ToObject throws TypeError for null/undefined.
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    // String primitives: index properties + "length" are own properties.
    JsString(s) -> {
      let key_str = case key {
        StringKey(display:, ..) -> display
        SymbolKey(_) -> ""
      }
      let len = string.length(s)
      case key_str == "length" {
        True -> {
          let prop =
            // seq: 0 — synthesized descriptor, only rendered by
            // make_descriptor_object, never stored in a property table.
            DataProperty(
              value: value.from_int(len),
              writable: False,
              enumerable: False,
              configurable: False,
              seq: 0,
            )
          let #(heap, desc_ref) =
            make_descriptor_object(state.heap, prop, object_proto)
          #(State(..state, heap:), Ok(JsObject(desc_ref)))
        }
        False ->
          case int.parse(key_str) {
            Ok(i) if i >= 0 && i < len -> {
              let ch = string.slice(s, i, 1)
              let prop =
                // seq: 0 — synthesized descriptor, never stored.
                DataProperty(
                  value: JsString(ch),
                  writable: False,
                  enumerable: True,
                  configurable: False,
                  seq: 0,
                )
              let #(heap, desc_ref) =
                make_descriptor_object(state.heap, prop, object_proto)
              #(State(..state, heap:), Ok(JsObject(desc_ref)))
            }
            _ -> #(state, Ok(JsUndefined))
          }
      }
    }
    // Number/boolean/symbol have no own string-keyed properties.
    _ -> #(state, Ok(JsUndefined))
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
  use #(dkey, state) <- result.try(to_define_key(state, key_val))
  use #(parsed, state) <- result.try(parse_descriptor(state, JsObject(desc_ref)))
  // DefinePropertyOrThrow (§7.3.8): a false [[DefineOwnProperty]] result and
  // a genuine abrupt completion both surface as a throw here.
  case define_parsed(state, target_ref, dkey, parsed) {
    Ok(state) -> Ok(state)
    Error(#(failure, state)) -> Error(#(define_failure_value(failure), state))
  }
}

/// [[DefineOwnProperty]] dispatch for an already-parsed descriptor.
/// Arrays get the exotic algorithm (§10.4.2.1) which special-cases "length"
/// (ArraySetLength §10.4.2.4) and array indices; proxies trap (§10.5.6);
/// everything else is OrdinaryDefineOwnProperty (§10.1.6.1).
fn define_parsed(
  state: State(host),
  target_ref: Ref,
  dkey: DefineKey,
  parsed: ParsedDesc,
) -> Result(State(host), #(DefineFailure, State(host))) {
  case heap.read(state.heap, target_ref) {
    Some(ObjectSlot(kind: ArrayObject(length:), ..)) ->
      case dkey {
        StringKey(pkey: Named("length"), ..) ->
          array_define_length(state, target_ref, parsed, length)
        // An array index is an integer in [0, 2^32-1) — "4294967295" is a
        // plain property name on arrays, not an index (§6.1.7).
        StringKey(pkey: Index(idx), ..) if idx < 4_294_967_295 ->
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
        StringKey(pkey: Index(idx), ..) ->
          typed_array_define_index(
            state,
            buffer,
            elem_kind,
            byte_offset,
            length,
            idx,
            parsed,
          )
        StringKey(pkey: Named(s), ..) ->
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
        SymbolKey(_) ->
          ordinary_define(state, target_ref, dkey, parsed)
          |> result.map_error(as_rejected)
      }
    // String exotic [[DefineOwnProperty]] (§10.4.3.2): the synthesized
    // "length" and in-range index properties are non-writable and
    // non-configurable — validate against their fixed descriptors instead of
    // writing to the dict.
    Some(ObjectSlot(kind: value.StringObject(value: s), extensible:, ..)) -> {
      let synthesized = case dkey {
        StringKey(pkey: Named("length"), ..) -> True
        StringKey(pkey: Index(i), ..) -> i >= 0 && i < object.string_length(s)
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
                "Cannot redefine property: " <> define_key_label(dkey),
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
        SymbolKey(_) ->
          ordinary_define(state, target_ref, dkey, parsed)
          |> result.map_error(as_rejected)
        StringKey(pkey: _, display: name) ->
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
              <> define_key_label(dkey),
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
    StringKey(Index(idx), int.to_string(idx)),
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
///        makes the store a silent no-op, still true)
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
  let valid =
    option.is_some(object.typed_array_element(
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
    ))
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
    Some(v) ->
      typed_array_elements.typed_array_store(
        state,
        buffer,
        elem_kind,
        byte_offset,
        length,
        Some(idx),
        v,
      )
      |> result.map(fn(pair) { pair.0 })
      |> result.map_error(as_thrown)
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
  dkey: DefineKey,
  desc: ParsedDesc,
) -> Result(State(host), #(JsValue, State(host))) {
  let has_accessor = desc_is_accessor(desc)
  let has_data = desc_is_data(desc)
  let is_accessor = has_accessor
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
        StringKey(pkey: Index(idx), ..) if indexed_kind ->
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
        StringKey(pkey:, ..) -> dict.get(properties, pkey)
        SymbolKey(sym:) -> list.key_find(symbol_properties, sym)
      }
      let key = case dkey {
        StringKey(display:, ..) -> display
        SymbolKey(_) -> "[Symbol]"
      }

      // §10.1.6.3 step 2: If property doesn't exist and object is not extensible, reject.
      use Nil <- result.try(case existing, extensible {
        Error(Nil), False ->
          reject_define(
            state,
            "Cannot define property " <> key <> ", object is not extensible",
          )
        _, _ -> Ok(Nil)
      })

      // §10.1.6.3 ValidateAndApplyPropertyDescriptor step 5 (sub-steps 5a–5e):
      // Validate that the change is permitted on non-configurable properties.
      use Nil <- result.try(case existing {
        Ok(existing_prop) -> {
          let current_configurable = value.prop_configurable(existing_prop)
          case current_configurable {
            True -> Ok(Nil)
            False -> {
              // Step 7a: Cannot make a non-configurable property configurable.
              use Nil <- result.try(case desc_configurable {
                Some(True) ->
                  reject_define(state, "Cannot redefine property: " <> key)
                _ -> Ok(Nil)
              })
              // Step 7b: Cannot change enumerable on non-configurable property.
              let current_enumerable = value.prop_enumerable(existing_prop)
              use Nil <- result.try(case desc_enumerable {
                Some(e) if e != current_enumerable ->
                  reject_define(state, "Cannot redefine property: " <> key)
                _ -> Ok(Nil)
              })
              // Step 9a: Cannot change property kind (data <-> accessor) on non-configurable.
              let current_is_accessor = case existing_prop {
                AccessorProperty(..) -> True
                _ -> False
              }
              use Nil <- result.try(
                case
                  current_is_accessor != is_accessor
                  && { has_accessor || has_data }
                {
                  True ->
                    reject_define(state, "Cannot redefine property: " <> key)
                  False -> Ok(Nil)
                },
              )
              // Step 10a: Non-configurable data property checks.
              use Nil <- result.try(case existing_prop {
                DataProperty(writable: False, value: cur_val, ..)
                  if !current_is_accessor
                -> {
                  // Step 10a.i: Cannot change writable from false to true.
                  use Nil <- result.try(case desc_writable {
                    Some(True) ->
                      reject_define(state, "Cannot redefine property: " <> key)
                    _ -> Ok(Nil)
                  })
                  // Step 10a.ii: Cannot change value on non-writable.
                  use Nil <- result.try(case desc_value {
                    Some(v) if v != cur_val ->
                      reject_define(state, "Cannot redefine property: " <> key)
                    _ -> Ok(Nil)
                  })
                  Ok(Nil)
                }
                _ -> Ok(Nil)
              })
              // Step 11a: Non-configurable accessor property checks.
              use Nil <- result.try(case existing_prop {
                AccessorProperty(get: cur_get, set: cur_set, ..) -> {
                  // Cannot change getter on non-configurable accessor.
                  use Nil <- result.try(case desc_get {
                    Some(g) -> {
                      let cur_g = option.unwrap(cur_get, JsUndefined)
                      case g != cur_g {
                        True ->
                          reject_define(
                            state,
                            "Cannot redefine property: " <> key,
                          )
                        False -> Ok(Nil)
                      }
                    }
                    _ -> Ok(Nil)
                  })
                  // Cannot change setter on non-configurable accessor.
                  case desc_set {
                    Some(s) -> {
                      let cur_s = option.unwrap(cur_set, JsUndefined)
                      case s != cur_s {
                        True ->
                          reject_define(
                            state,
                            "Cannot redefine property: " <> key,
                          )
                        False -> Ok(Nil)
                      }
                    }
                    _ -> Ok(Nil)
                  }
                }
                _ -> Ok(Nil)
              })
              Ok(Nil)
            }
          }
        }
        _ -> Ok(Nil)
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
        StringKey(pkey: Index(idx), ..) if indexed_kind ->
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
        StringKey(pkey:, ..) -> #(
          dict.insert(properties, pkey, new_prop),
          symbol_properties,
          elements,
        )
        SymbolKey(sym:) -> #(
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
  let #(h, err) = common.make_type_error(state.heap, state.builtins, msg)
  Error(#(err, State(..state, heap: h)))
}

/// Helper to create a RangeError for ArraySetLength step 5 (§10.4.2.4).
fn reject_range(
  state: State(host),
  msg: String,
) -> Result(a, #(JsValue, State(host))) {
  let #(h, err) = common.make_range_error(state.heap, state.builtins, msg)
  Error(#(err, State(..state, heap: h)))
}

/// Helper for ToPropertyDescriptor: reads a field via [[Get]] (calls getters).
/// Implements the "If HasProperty(Obj, name)" + "Let val = Get(Obj, name)" pattern
/// from §6.2.6.5 steps 3-8.
///
/// Returns #(Some(value), state) if the property exists, #(None, state) if absent.
/// The distinction matters because absent fields are not set in the descriptor,
/// while present-but-undefined fields ARE set (e.g. {value: undefined} is different
/// from {} — the former sets [[Value]] to undefined, the latter leaves it absent).
fn read_desc_field(
  state: State(host),
  desc: JsValue,
  key: String,
) -> Result(#(option.Option(JsValue), State(host)), #(JsValue, State(host))) {
  use #(val, state) <- result.try(object.get_value_of(state, desc, Named(key)))
  case val {
    JsUndefined ->
      // get_value_of returns undefined for both "absent" and "present with value
      // undefined". We need HasProperty semantics (proto chain walk) to distinguish.
      case desc {
        JsObject(ref) ->
          case has_property(state.heap, ref, Named(key)) {
            True -> Ok(#(Some(JsUndefined), state))
            False -> Ok(#(option.None, state))
          }
        _ -> Ok(#(option.None, state))
      }
    _ -> Ok(#(Some(val), state))
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
              use all_keys, state <- state.try_op(own_keys_stateful(state, ref))
              let strings =
                list.filter(all_keys, fn(k) {
                  case k {
                    JsString(_) -> True
                    _ -> False
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
    // String primitives: own keys are index chars + "length"
    JsString(s) -> {
      let len = string.length(s)
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

/// [[HasProperty]] — §7.3.11. Walks the prototype chain looking for a string key.
/// Returns True if found as own property at any level, False if not found.
fn has_property(heap: Heap(host), ref: Ref, key: value.PropertyKey) -> Bool {
  case heap.read(heap, ref) {
    Some(ObjectSlot(properties:, prototype:, ..)) ->
      case dict.has_key(properties, key) {
        True -> True
        False ->
          case prototype {
            Some(proto_ref) -> has_property(heap, proto_ref, key)
            None -> False
          }
      }
    _ -> False
  }
}

/// Build list of JsString index keys ["0", "1", ..., "len-1"] for string primitives.
fn string_index_keys(i: Int, len: Int) -> List(JsValue) {
  case i >= len {
    True -> []
    False -> [JsString(int.to_string(i)), ..string_index_keys(i + 1, len)]
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
      use prop_opt, state <- state.try_op(own_property_via(state, ref, key))
      case prop_opt {
        // §10.4.6.5: a namespace [[GetOwnProperty]] performs [[Get]], so a TDZ
        // binding throws (only namespace descriptors carry JsUninitialized).
        Some(DataProperty(value: value.JsUninitialized, ..)) ->
          state.reference_error(state, tdz_message)
        prop -> #(state, Ok(JsBool(option.is_some(prop))))
      }
    }
    // Step 2: ToObject throws TypeError on null/undefined.
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    // String primitives: own keys are index chars + "length"
    JsString(s) -> #(state, Ok(JsBool(string_own_key(s, key))))
    // Number/boolean/symbol have no own string-keyed properties.
    _ -> #(state, Ok(JsBool(False)))
  }
}

/// Whether a DefineKey names an own property on a string primitive — only
/// "length" and valid indices; symbol keys are never own on string wrappers.
fn string_own_key(s: String, key: DefineKey) -> Bool {
  case key {
    SymbolKey(_) -> False
    StringKey(pkey: Named("length"), ..) -> True
    StringKey(pkey: Index(i), ..) -> i >= 0 && i < string.length(s)
    StringKey(pkey: Named(_), ..) -> False
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
      use prop_opt, state <- state.try_op(own_property_via(state, ref, key))
      case prop_opt {
        Some(DataProperty(value: value.JsUninitialized, ..)) ->
          state.reference_error(state, tdz_message)
        Some(p) -> #(state, Ok(JsBool(value.prop_enumerable(p))))
        None -> #(state, Ok(JsBool(False)))
      }
    }
    // Step 2: ToObject throws TypeError on null/undefined.
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    // String primitives: index properties are own+enumerable,
    // "length" is own but non-enumerable.
    JsString(s) -> {
      let result = case key {
        StringKey(pkey: Index(i), ..) -> i >= 0 && i < string.length(s)
        _ -> False
      }
      #(state, Ok(JsBool(result)))
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
  let heap = state.heap
  // Steps 1-14: early returns for undefined/null, else builtinTag.
  let builtin = case this {
    // Step 1: If the this value is undefined, return "[object Undefined]".
    JsUndefined | value.JsUninitialized -> Ok(Error("Undefined"))
    // Step 2: If the this value is null, return "[object Null]".
    JsNull -> Ok(Error("Null"))
    // Step 3 for primitives: ToObject wraps — the wrapper's internal slots
    // ([[BooleanData]]/[[NumberData]]/[[StringData]]) drive steps 9-11.
    // Symbol/BigInt wrappers have no builtinTag step → "Object" (step 14);
    // Symbol stays "Symbol" while %Symbol.prototype% lacks @@toStringTag
    // (deviation — it currently aliases %Object.prototype%).
    JsBool(_) -> Ok(Ok("Boolean"))
    JsNumber(_) -> Ok(Ok("Number"))
    JsString(_) -> Ok(Ok("String"))
    JsSymbol(_) -> Ok(Ok("Symbol"))
    value.JsBigInt(_) -> Ok(Ok("Object"))
    // Step 4: Let isArray be ? IsArray(O) — pierces proxies to their
    // target (§7.2.2 step 3) and throws TypeError when revoked.
    JsObject(ref) ->
      case object.is_array_ref(heap, ref) {
        Error(Nil) -> Error(Nil)
        // Step 5: If isArray is true, builtinTag is "Array".
        Ok(True) -> Ok(Ok("Array"))
        // Steps 6-14: classify by internal slots / [[Call]].
        Ok(False) -> Ok(Ok(object_tag(heap, ref)))
      }
  }
  case builtin {
    Error(Nil) ->
      state.type_error(
        state,
        "Cannot perform 'IsArray' on a proxy that has been revoked",
      )
    // Steps 1-2: undefined/null skip the @@toStringTag lookup entirely.
    Ok(Error(t)) -> #(state, Ok(JsString("[object " <> t <> "]")))
    Ok(Ok(fallback)) -> {
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
          use keys, state <- state.try_op(own_keys_stateful(state, ref))
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
      cont(list.index_map(string.to_graphemes(s), from_char), state)
    // Number/boolean/symbol wrappers have no own enumerable string keys.
    _ -> cont([], state)
  }
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
  keys: List(JsValue),
  combine: fn(String, JsValue) -> a,
  acc: List(a),
) -> Result(#(List(a), State(host)), #(JsValue, State(host))) {
  case keys {
    [] -> Ok(#(list.reverse(acc), state))
    [JsString(s) as key_val, ..rest] -> {
      use #(prop, state) <- result.try(get_own_property_stateful(
        state,
        ref,
        key_val,
      ))
      let enumerable =
        option.map(prop, value.prop_enumerable) |> option.unwrap(False)
      case enumerable {
        True -> {
          use #(val, state) <- result.try(object.get_value(
            state,
            ref,
            value.canonical_key(s),
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
    [_, ..rest] ->
      collect_enumerable_via_traps(state, ref, receiver, rest, combine, acc)
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
        value.canonical_key(k),
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
      use keys, state <- state.try_op(own_keys_stateful(state, props_ref))
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
  remaining: List(JsValue),
) -> #(State(host), Result(JsValue, JsValue)) {
  case remaining {
    // Step 6: Return O (all keys processed).
    [] -> #(state, Ok(JsObject(target_ref)))
    [key_val, ..rest] -> {
      // Step 4.a: propDesc = ? props.[[GetOwnProperty]](nextKey) — trap-aware.
      use prop, state <- state.try_op(get_own_property_stateful(
        state,
        props_ref,
        key_val,
      ))
      let enumerable =
        option.map(prop, value.prop_enumerable) |> option.unwrap(False)
      case enumerable {
        // Step 4.b: propDesc undefined or not enumerable → skip.
        False -> define_props_loop(state, target_ref, props_ref, rest)
        True -> {
          // key_val is a String/Symbol from [[OwnPropertyKeys]], so
          // ToPropertyKey is a pure fast path (no user code, cannot throw).
          use pk, state <- state.try_op(property.to_prop_key(
            state,
            key_val,
          ))
          // Step 4.b.i: descObj = Get(props, nextKey) — a real [[Get]], so
          // accessor descriptors have their getter invoked (and symbol keys
          // reach define_parsed instead of being silently dropped).
          use desc_val, state <- state.try_op(object.get_prop_value(
            state,
            props_ref,
            pk,
            JsObject(props_ref),
          ))
          case desc_val {
            // Step 4.b.ii: ToPropertyDescriptor + step 5.a DefinePropertyOrThrow.
            JsObject(desc_ref) -> {
              use state <- state.try_state(apply_descriptor(
                state,
                target_ref,
                key_val,
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
          use #(keys, state) <- result.try(own_keys_stateful(state, src_ref))
          assign_proxy_keys(state, target_ref, src_ref, receiver, keys)
        }
        None -> {
          // Step 3.a.ii: Let keys be ? from.[[OwnPropertyKeys]]().
          // String keys first:
          let ks =
            collect_own_keys(state.heap, src_ref, True)
            |> list.map(value.canonical_key)
          // Step 3.a.iii: For each string key, copy it.
          use state <- result.try(assign_keys(
            state,
            target_ref,
            src_ref,
            receiver,
            ks,
            object.get_value,
            object.set_value,
          ))
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
          )
        }
      }
    // String sources: each character is an enumerable own property.
    // "length" is own but non-enumerable, so it's excluded.
    JsString(s) -> {
      let chars = string.to_graphemes(s)
      assign_string_chars(state, target_ref, chars, 0)
    }
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
      let #(heap, _ok) =
        object.set_property(state.heap, target_ref, Index(idx), JsString(ch))
      assign_string_chars(State(..state, heap:), target_ref, rest, idx + 1)
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
) -> Result(State(host), #(JsValue, State(host))) {
  case keys {
    [] -> Ok(state)
    [k, ..rest] -> {
      // Step 2.a: Let propValue be ? Get(from, nextKey).
      use #(val, state) <- result.try(get(state, src_ref, k, receiver))
      // Step 2.b: Perform ? Set(to, nextKey, propValue, true).
      use #(state, _ok) <- result.try(set(
        state,
        target_ref,
        k,
        val,
        JsObject(target_ref),
      ))
      assign_keys(state, target_ref, src_ref, receiver, rest, get, set)
    }
  }
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
  keys: List(JsValue),
) -> Result(State(host), #(JsValue, State(host))) {
  case keys {
    [] -> Ok(state)
    [k, ..rest] -> {
      // Step 1: desc = ? from.[[GetOwnProperty]](nextKey) — trap-aware.
      use #(prop, state) <- result.try(get_own_property_stateful(
        state,
        src_ref,
        k,
      ))
      let enumerable =
        option.map(prop, value.prop_enumerable) |> option.unwrap(False)
      use state <- result.try(case enumerable, k {
        True, JsString(s) -> {
          let key = value.canonical_key(s)
          // Step 2.a: Let propValue be ? Get(from, nextKey).
          use #(val, state) <- result.try(object.get_value(
            state,
            src_ref,
            key,
            receiver,
          ))
          // Step 2.b: Perform ? Set(to, nextKey, propValue, true).
          use #(state, _ok) <- result.map(object.set_value(
            state,
            target_ref,
            key,
            val,
            JsObject(target_ref),
          ))
          state
        }
        True, JsSymbol(sym) -> {
          use #(val, state) <- result.try(object.get_symbol_value(
            state,
            src_ref,
            sym,
            receiver,
          ))
          use #(state, _ok) <- result.map(object.set_symbol_value(
            state,
            target_ref,
            sym,
            val,
            JsObject(target_ref),
          ))
          state
        }
        // Not enumerable / not own — skipped per step 2. (own_keys_stateful
        // only yields String/Symbol values, so other shapes are unreachable.)
        _, _ -> Ok(state)
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
  excluded_keys: set.Set(value.PropertyKey),
  excluded_syms: set.Set(value.SymbolId),
) -> Result(State(host), #(JsValue, State(host))) {
  case source {
    JsObject(src_ref) ->
      case object.as_proxy(state.heap, src_ref) {
        Some(_) -> {
          // Step 3: Let keys be ? from.[[OwnPropertyKeys]]() — ownKeys trap
          // (throws TypeError when the proxy is revoked).
          use #(keys, state) <- result.try(own_keys_stateful(state, src_ref))
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
  keys: List(JsValue),
  excluded_keys: set.Set(value.PropertyKey),
  excluded_syms: set.Set(value.SymbolId),
) -> Result(State(host), #(JsValue, State(host))) {
  case keys {
    [] -> Ok(state)
    [k, ..rest] -> {
      // Step 4.a-b: skip keys named in excludedItems.
      let excluded = case k {
        JsString(s) -> set.contains(excluded_keys, value.canonical_key(s))
        JsSymbol(sym) -> set.contains(excluded_syms, sym)
        _ -> True
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
  k: JsValue,
) -> Result(State(host), #(JsValue, State(host))) {
  // Step 4.c.i: Let desc be ? from.[[GetOwnProperty]](nextKey) — trap-aware.
  use #(prop, state) <- result.try(get_own_property_stateful(state, src_ref, k))
  let enumerable =
    option.map(prop, value.prop_enumerable) |> option.unwrap(False)
  case enumerable, k {
    False, _ -> Ok(state)
    True, JsString(s) -> {
      // Step 4.c.ii.1: Let propValue be ? Get(from, nextKey) — get trap.
      use #(val, state) <- result.map(object.get_value(
        state,
        src_ref,
        value.canonical_key(s),
        JsObject(src_ref),
      ))
      // Step 4.c.ii.2: CreateDataPropertyOrThrow(target, nextKey, propValue).
      let heap =
        object.create_data_property(
          state.heap,
          target_ref,
          value.canonical_key(s),
          val,
        )
      State(..state, heap:)
    }
    True, JsSymbol(sym) -> {
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
    // own_keys_stateful only yields String/Symbol values.
    True, _ -> Ok(state)
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
      use prop_opt, state <- state.try_op(own_property_via(state, ref, key))
      #(state, Ok(JsBool(option.is_some(prop_opt))))
    }
    // Step 1: ToObject throws TypeError on null/undefined.
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    // String primitives: own keys are index chars + "length"
    JsString(s) -> {
      use key, state <- try_to_property_key(state, key_val)
      #(state, Ok(JsBool(string_own_key(s, key))))
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
    value.JsBigInt(_) -> #(state, Ok(JsObject(state.builtins.bigint_proto)))
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
    JsObject(ref), Ok(new_proto) ->
      case object.as_proxy(state.heap, ref) {
        // §10.5.2: proxies trap [[SetPrototypeOf]].
        Some(#(p_target, p_handler)) -> {
          use #(state, ok) <- try_op_pair(proxy_set_proto(
            state,
            p_target,
            p_handler,
            new_proto,
          ))
          case ok {
            True -> #(state, Ok(target))
            False ->
              state.type_error(
                state,
                "'setPrototypeOf' on proxy: trap returned falsish",
              )
          }
        }
        None ->
          // §20.1.2.21 step 4-6: O.[[SetPrototypeOf]](proto); throw if false.
          case ordinary_set_prototype_of(state, ref, new_proto) {
            Ok(state) -> #(state, Ok(target))
            Error(NotExtensible) ->
              state.type_error(
                state,
                "Cannot set prototype of a non-extensible object",
              )
            Error(Cyclic) -> state.type_error(state, "Cyclic __proto__ value")
            Error(Immutable) ->
              state.type_error(
                state,
                "Immutable prototype object cannot have its prototype set",
              )
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
  dkey: DefineKey,
  kind: AccessorKind,
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 3.a: desc = ? O.[[GetOwnProperty]](key).
  case own_property_via(state, ref, dkey) {
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

pub type SetProtoFail {
  NotExtensible
  Cyclic
  Immutable
}

/// OrdinarySetPrototypeOf — ES2024 §10.1.2.1
/// (with §10.4.7 SetImmutablePrototype check for Object.prototype)
///
/// Shared core for Object.setPrototypeOf (§20.1.2.21, throws on fail)
/// and Reflect.setPrototypeOf (§28.1.13, returns Bool on fail).
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

/// Helper for SetIntegrityLevel "frozen" — §7.3.16 step 6.a-b.
///
/// For each own property descriptor:
///   6.a. If IsDataDescriptor(Desc) is true, then
///        i. If Desc has a [[Value]] field, set Desc.[[Writable]] to false.
///        (our data props always have [[Value]], so writable is always set)
///   6.b. Set Desc.[[Configurable]] to false.
///
/// Accessor properties only get configurable=false (step 6.b), writable
/// does not apply to accessors.
fn freeze_prop(prop: value.Property) -> value.Property {
  case prop {
    // §7.3.16 step 6.a: DataDescriptor → writable=false, configurable=false.
    // seq is preserved: freezing does not move keys (§10.1.11).
    DataProperty(value:, enumerable:, seq:, ..) ->
      DataProperty(
        value:,
        writable: False,
        enumerable:,
        configurable: False,
        seq:,
      )
    // §7.3.16 step 6.b: AccessorDescriptor → configurable=false only.
    AccessorProperty(get:, set:, enumerable:, seq:, ..) ->
      AccessorProperty(get:, set:, enumerable:, configurable: False, seq:)
  }
}

/// SetIntegrityLevel ( O, level ) — ES2024 §7.3.16
///
/// Shared core of Object.freeze and Object.seal. The `transform` callback
/// applies the level-specific descriptor change (freeze_prop / seal_prop).
///
///   1. Let status be ? O.[[PreventExtensions]]().
///   3. Let keys be ? O.[[OwnPropertyKeys]]().
///   4-6. For each key, transform its property descriptor.
///   7. Return true.
///
/// NOTE: Elements (dense indexed properties) are NOT transformed — only
/// named and symbol properties. This is a known simplification.
fn set_integrity_level(
  args: List(JsValue),
  state: State(host),
  transform: fn(value.Property) -> value.Property,
) -> #(State(host), Result(JsValue, JsValue)) {
  let target = first_arg_or_undefined(args)
  case target {
    JsObject(ref) -> {
      let heap = {
        use slot <- heap.update(state.heap, ref)
        case slot {
          ObjectSlot(kind:, properties:, symbol_properties:, ..) -> {
            let properties =
              dict.map_values(properties, fn(k, p) {
                // Private elements are unaffected by freeze/seal —
                // SetIntegrityLevel only touches [[OwnPropertyKeys]], which
                // excludes private names.
                case value.is_private_name(k) {
                  True -> p
                  False -> transform(p)
                }
              })
            // Array exotic: the virtual "length" property participates in
            // SetIntegrityLevel too (§7.3.16 — [[OwnPropertyKeys]] includes
            // "length"). Its attributes live in an OPTIONAL Named("length")
            // dict override consulted by array_length_writable and the
            // [[Set]]/[[DefineOwnProperty]] paths — materialize one so the
            // transform is recorded and a later Set(O, "length") on a frozen
            // array rejects (Set's Throw=true then raises TypeError).
            let properties = case kind {
              ArrayObject(length:) ->
                case dict.has_key(properties, Named("length")) {
                  True -> properties
                  False ->
                    dict.insert(
                      properties,
                      Named("length"),
                      // seq: 0 — array "length" never enumerates through the
                      // seq-ordered named-key path.
                      transform(DataProperty(
                        value: value.from_int(length),
                        writable: True,
                        enumerable: False,
                        configurable: False,
                        seq: 0,
                      )),
                    )
                }
              _ -> properties
            }
            ObjectSlot(
              ..slot,
              properties:,
              symbol_properties: list.map(symbol_properties, fn(pair) {
                #(pair.0, transform(pair.1))
              }),
              extensible: False,
            )
          }
          _ -> slot
        }
      }
      #(State(..state, heap:), Ok(target))
    }
    // §20.1.2.6/§20.1.2.20 step 1: If O is not an Object, return O.
    _ -> #(state, Ok(target))
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
  set_integrity_level(args, state, freeze_prop)
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

/// Helper for TestIntegrityLevel "frozen" — §7.3.17 step 4.b.
///
/// For each own property descriptor, checks the frozen invariant:
///   4.b.i.  If IsDataDescriptor(currentDesc) is true, then
///           1. If currentDesc.[[Writable]] is true, return false.
///   4.b.ii. If currentDesc.[[Configurable]] is true, return false.
///
/// Returns True only if ALL properties satisfy the frozen constraint.
fn all_frozen(props: List(value.Property)) -> Bool {
  use p <- list.all(props)
  case p {
    // §7.3.17 step 4.b.i + 4.b.ii: data prop must be non-writable AND non-configurable.
    DataProperty(writable: False, configurable: False, ..) -> True
    // §7.3.17 step 4.b.ii: accessor prop must be non-configurable.
    AccessorProperty(configurable: False, ..) -> True
    _ -> False
  }
}

/// TestIntegrityLevel ( O, level ) — ES2024 §7.3.17
///
/// Shared core of Object.isFrozen and Object.isSealed. The `check` callback
/// is the level-specific predicate over property lists (all_frozen / all_sealed).
///
///   1. Let extensible be ? IsExtensible(O).
///   2. If extensible is true, return false.
///   3-4. For each own property, apply the level check.
///   5. Return true.
///
/// TODO(Deviation): Elements (dense indexed properties) are not checked —
/// only named and symbol properties. Same simplification as SetIntegrityLevel.
fn test_integrity_level(
  args: List(JsValue),
  state: State(host),
  check: fn(List(value.Property)) -> Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  let result = case first_arg_or_undefined(args) {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(properties:, symbol_properties:, extensible: False, ..)) ->
          check(
            dict.to_list(properties)
            // Private elements stay writable after freeze — exclude them from
            // the integrity check, matching their exclusion in SetIntegrityLevel.
            |> list.filter_map(fn(pair) {
              case value.is_private_name(pair.0) {
                True -> Error(Nil)
                False -> Ok(pair.1)
              }
            }),
          )
          && check(list.map(symbol_properties, fn(p) { p.1 }))
        Some(ObjectSlot(extensible: True, ..)) -> False
        _ -> False
      }
    // §20.1.2.14/§20.1.2.15 step 1: If O is not an Object, return true.
    _ -> True
  }
  #(state, Ok(JsBool(result)))
}

/// Object.isFrozen ( O ) — ES2024 §20.1.2.14
///
///   1. If O is not an Object, return true.
///   2. Return ? TestIntegrityLevel(O, frozen).
fn is_frozen(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  test_integrity_level(args, state, all_frozen)
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
  set_integrity_level(args, state, seal_prop)
}

/// Helper for seal — make property non-configurable (but keep writable as-is).
fn seal_prop(prop: value.Property) -> value.Property {
  case prop {
    // seq is preserved: sealing does not move keys (§10.1.11).
    DataProperty(value:, writable:, enumerable:, seq:, ..) ->
      DataProperty(value:, writable:, enumerable:, configurable: False, seq:)
    AccessorProperty(get:, set:, enumerable:, seq:, ..) ->
      AccessorProperty(get:, set:, enumerable:, configurable: False, seq:)
  }
}

/// Object.isSealed ( O ) — ES2024 §20.1.2.15
///
///   1. If O is not an Object, return true.
///   2. Return ? TestIntegrityLevel(O, sealed).
fn is_sealed(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  test_integrity_level(args, state, all_sealed)
}

/// Check if all properties are non-configurable (sealed check).
fn all_sealed(props: List(value.Property)) -> Bool {
  use p <- list.all(props)
  case p {
    DataProperty(configurable: False, ..) -> True
    AccessorProperty(configurable: False, ..) -> True
    _ -> False
  }
}

/// Object.fromEntries ( iterable ) — ES2024 §20.1.2.8
///
///   1. Perform ? RequireObjectCoercible(iterable).
///   2. Let obj be OrdinaryObjectCreate(%Object.prototype%).
///   3. Let adder be CreateDataPropertyOnObject (i.e. for each entry [k, v],
///      set obj[k] = v using CreateDataPropertyOrThrow).
///   4. Return ? AddEntriesFromIterable(obj, iterable, adder).
///
/// AddEntriesFromIterable (§7.4.8):
///   - Iterates using GetIterator + IteratorStep.
///   - For each next item:
///     a. If item is not an Object, throw TypeError (and close iterator).
///     b. k = Get(item, "0"), v = Get(item, "1").
///     c. CreateDataPropertyOrThrow(obj, ToPropertyKey(k), v).
///
/// Property insertion order matches the iteration order of the iterable.
/// Symbol keys (when k is a JsSymbol) are stored in symbol_properties.
///
/// Simplified: handles Arrays of [key, value] pairs. General iterables
/// (objects with Symbol.iterator) are not yet supported.
fn from_entries(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let target = first_arg_or_undefined(args)
  case target {
    // Step 1: RequireObjectCoercible — null/undefined throw TypeError.
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    JsObject(ref) -> {
      // Read the source array/iterable
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: ArrayObject(..), elements:, ..)) -> {
          // Iterate over array elements to build object properties.
          // Use a list accumulator to preserve insertion order.
          let entry_values = elements.values(elements)
          from_entries_loop(entry_values, state, [], [])
        }
        _ -> state.type_error(state, "Object.fromEntries requires an iterable")
      }
    }
    _ -> state.type_error(state, "Object.fromEntries requires an iterable")
  }
}

/// Loop over iterable entries for Object.fromEntries.
///
/// `str_acc` is a list of #(String, Property) pairs in reverse insertion order
/// (prepended for O(1) accumulation). `sym_acc` is a dict of symbol-keyed properties.
fn from_entries_loop(
  entries: List(JsValue),
  state: State(host),
  str_acc: List(#(String, value.Property)),
  sym_acc: List(#(value.SymbolId, value.Property)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case entries {
    [] -> {
      // Build the result object.
      // String properties are inserted in iteration order: convert list to dict.
      // Since later entries with the same key should overwrite earlier ones,
      // we reverse (to restore insertion order) then fold left-to-right.
      let props =
        list.fold(list.reverse(str_acc), dict.new(), fn(d, pair) {
          dict.insert(d, value.canonical_key(pair.0), pair.1)
        })
      let #(heap, obj_ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: OrdinaryObject,
            properties: props,
            symbol_properties: sym_acc,
            elements: elements.new(),
            prototype: Some(state.builtins.object.prototype),
            extensible: True,
          ),
        )
      #(State(..state, heap:), Ok(JsObject(obj_ref)))
    }
    [JsObject(entry_ref) as entry, ..rest] -> {
      // Step b: k = Get(item, "0"), v = Get(item, "1")
      // Use object.get_value to invoke getters (handles accessor properties).
      use key_val, state <- try_get(state, entry_ref, "0", entry)
      use val, state <- try_get(state, entry_ref, "1", entry)
      // Step c: ToPropertyKey(k) — symbol keys go to symbol_properties.
      case key_val {
        JsSymbol(sym) ->
          from_entries_loop(
            rest,
            state,
            str_acc,
            list.key_set(sym_acc, sym, value.data_property(val)),
          )
        _ -> {
          // ToPropertyKey via ToString for non-symbol keys.
          use key_str, state <- coerce.try_to_string(state, key_val)
          from_entries_loop(
            rest,
            state,
            [#(key_str, value.data_property(val)), ..str_acc],
            sym_acc,
          )
        }
      }
    }
    [_, ..] -> state.type_error(state, "Iterator value is not an entry object")
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
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    JsObject(ref) -> {
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(properties:, ..)) -> {
          // Build descriptor objects for each own property
          // Iterate in §10.1.11 [[OwnPropertyKeys]] order so the result
          // object's properties are created (seq-stamped) in the same order.
          let #(heap, desc_props) =
            list.fold(
              value.ordered_property_pairs(properties),
              #(state.heap, dict.new()),
              fn(acc, pair) {
                let #(h, descs) = acc
                let #(key, prop) = pair
                // Private names ("#x") are invisible to reflection.
                use <- bool.guard(value.is_private_name(key), acc)
                let #(h, desc_ref) =
                  make_descriptor_object(h, prop, object_proto)
                #(
                  h,
                  dict.insert(
                    descs,
                    key,
                    value.data_property(JsObject(desc_ref)),
                  ),
                )
              },
            )
          let #(heap, result_ref) =
            heap.alloc(
              heap,
              ObjectSlot(
                kind: OrdinaryObject,
                properties: desc_props,
                symbol_properties: [],
                elements: elements.new(),
                prototype: Some(object_proto),
                extensible: True,
              ),
            )
          #(State(..state, heap:), Ok(JsObject(result_ref)))
        }
        _ -> #(state, Ok(JsUndefined))
      }
    }
    _ -> #(state, Ok(JsUndefined))
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
          use all_keys, state <- state.try_op(own_keys_stateful(state, ref))
          let syms =
            list.filter(all_keys, fn(k) {
              case k {
                JsSymbol(_) -> True
                _ -> False
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
        value.canonical_key("toString"),
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

/// ES2024 §22.1.2.4 Object.groupBy ( items, callbackfn )
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
      // Get elements from iterable
      case items {
        JsObject(ref) ->
          case heap.read_array(state.heap, ref) {
            Some(#(length, elements)) -> {
              let elems = extract_elements(elements, 0, length, [])
              group_by_loop(state, elems, callback, 0, dict.new())
            }
            None ->
              state.type_error(state, "Object.groupBy: items is not iterable")
          }
        _ -> state.type_error(state, "Object.groupBy: items is not iterable")
      }
    }
  }
}

fn group_by_loop(
  state: State(host),
  items: List(JsValue),
  callback: JsValue,
  index: Int,
  groups: dict.Dict(String, List(JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case items {
    [] -> {
      // Build result object from groups — allocate arrays for each group
      let #(heap, props) =
        list.fold(dict.to_list(groups), #(state.heap, []), fn(acc, entry) {
          let #(h, ps) = acc
          let #(key, values) = entry
          let #(h, arr_ref) =
            common.alloc_array(
              h,
              list.reverse(values),
              state.builtins.array.prototype,
            )
          #(h, [
            #(
              value.canonical_key(key),
              value.builtin_property(JsObject(arr_ref)),
            ),
            ..ps
          ])
        })
      let #(heap, obj_ref) =
        heap.alloc(
          heap,
          ObjectSlot(
            kind: OrdinaryObject,
            properties: dict.from_list(props),
            elements: elements.new(),
            prototype: None,
            symbol_properties: [],
            extensible: True,
          ),
        )
      #(State(..state, heap:), Ok(JsObject(obj_ref)))
    }
    [item, ..rest] -> {
      use key_val, state <- state.try_call(state, callback, JsUndefined, [
        item,
        value.from_int(index),
      ])
      use key, state <- coerce.try_to_string(state, key_val)
      let current = dict.get(groups, key) |> result.unwrap([])
      let groups = dict.insert(groups, key, [item, ..current])
      group_by_loop(state, rest, callback, index + 1, groups)
    }
  }
}

/// Helper: extract array elements as a list.
fn extract_elements(
  elements: JsElements,
  idx: Int,
  length: Int,
  acc: List(JsValue),
) -> List(JsValue) {
  case idx >= length {
    True -> list.reverse(acc)
    False -> {
      let val = elements.get(elements, idx)
      extract_elements(elements, idx + 1, length, [val, ..acc])
    }
  }
}

// ============================================================================
// Proxy trap machinery for descriptor/key operations — §10.5.5/.6/.11
// (lives here, not ops/object, because it needs the ParsedDesc/DefineKey
// descriptor plumbing above)
// ============================================================================

/// DefineKey → trap-argument JsValue (String or Symbol).
fn define_key_value(key: DefineKey) -> JsValue {
  case key {
    StringKey(display:, ..) -> JsString(display)
    SymbolKey(sym) -> JsSymbol(sym)
  }
}

/// Human-readable key for invariant-violation error messages.
fn define_key_label(key: DefineKey) -> String {
  case key {
    StringKey(display:, ..) -> "'" <> display <> "'"
    SymbolKey(_) -> "[symbol]"
  }
}

/// JsValue (String/Symbol) → DefineKey. Non-keys map to an empty string key
/// (callers validate first).
fn key_value_to_define_key(val: JsValue) -> DefineKey {
  case val {
    JsSymbol(sym) -> SymbolKey(sym)
    JsString(s) -> StringKey(value.canonical_key(s), s)
    _ -> StringKey(value.canonical_key(""), "")
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

/// ToPropertyDescriptor (§6.2.6.5) on an arbitrary object value — reads the
/// six fields (invoking getters), validates get/set callability and the
/// accessor/data conflict.
fn parse_descriptor(
  state: State(host),
  desc_obj: JsValue,
) -> Result(#(ParsedDesc, State(host)), #(JsValue, State(host))) {
  use #(desc_get, state) <- result.try(read_desc_field(state, desc_obj, "get"))
  use #(desc_set, state) <- result.try(read_desc_field(state, desc_obj, "set"))
  use #(desc_value, state) <- result.try(read_desc_field(
    state,
    desc_obj,
    "value",
  ))
  use #(desc_writable, state) <- result.try(read_desc_bool(
    state,
    desc_obj,
    "writable",
  ))
  use #(desc_enumerable, state) <- result.try(read_desc_bool(
    state,
    desc_obj,
    "enumerable",
  ))
  use #(desc_configurable, state) <- result.try(read_desc_bool(
    state,
    desc_obj,
    "configurable",
  ))
  use Nil <- result.try(case desc_get {
    Some(g) ->
      case g != JsUndefined && !helpers.is_callable(state.heap, g) {
        True -> reject_define(state, "Getter must be a function")
        False -> Ok(Nil)
      }
    _ -> Ok(Nil)
  })
  use Nil <- result.try(case desc_set {
    Some(s) ->
      case s != JsUndefined && !helpers.is_callable(state.heap, s) {
        True -> reject_define(state, "Setter must be a function")
        False -> Ok(Nil)
      }
    _ -> Ok(Nil)
  })
  let parsed =
    ParsedDesc(
      get: desc_get,
      set: desc_set,
      value: desc_value,
      writable: desc_writable,
      enumerable: desc_enumerable,
      configurable: desc_configurable,
    )
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
/// reflection on possibly-proxy refs.
pub fn get_own_property_stateful(
  state: State(host),
  ref: Ref,
  key_val: JsValue,
) -> Result(#(Option(value.Property), State(host)), #(JsValue, State(host))) {
  own_property_via(state, ref, key_value_to_define_key(key_val))
}

/// DefineKey-keyed variant of get_own_property_stateful for in-module callers
/// that already resolved the key.
fn own_property_via(
  state: State(host),
  ref: Ref,
  key: DefineKey,
) -> Result(#(Option(value.Property), State(host)), #(JsValue, State(host))) {
  case object.as_proxy(state.heap, ref) {
    Some(#(target, handler)) ->
      proxy_get_own_property(state, target, handler, key)
    None -> Ok(#(get_own_property_by_key(state.heap, ref, key), state))
  }
}

/// §10.5.5 Proxy [[GetOwnProperty]] ( P ).
fn proxy_get_own_property(
  state: State(host),
  target: Option(Ref),
  handler: Option(Ref),
  key: DefineKey,
) -> Result(#(Option(value.Property), State(host)), #(JsValue, State(host))) {
  use #(t, h, trap, state) <- result.try(object.proxy_trap(
    state,
    target,
    handler,
    "getOwnPropertyDescriptor",
  ))
  case trap {
    // Step 6: no trap → target.[[GetOwnProperty]](P).
    None -> get_own_property_stateful(state, t, define_key_value(key))
    Some(trap_fn) -> {
      // Step 7: Call(trap, handler, « target, P »).
      use #(res, state) <- result.try(
        state.call(state, trap_fn, JsObject(h), [
          JsObject(t),
          define_key_value(key),
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
                      <> define_key_label(key)
                      <> " which is non-configurable in the proxy target",
                  ))
                  #(None, state)
                }
                True, False -> {
                  use Nil <- result.map(reject_define(
                    state,
                    "'getOwnPropertyDescriptor' on proxy: trap returned undefined for property "
                      <> define_key_label(key)
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
                    <> define_key_label(key)
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
                      <> define_key_label(key)
                      <> " which is non-existent in the proxy target",
                  )
                Some(td) ->
                  case value.prop_configurable(td) {
                    True ->
                      reject_define(
                        state,
                        "'getOwnPropertyDescriptor' on proxy: trap reported non-configurability for property "
                          <> define_key_label(key)
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
                              <> define_key_label(key)
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
              <> define_key_label(key),
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
  key: DefineKey,
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
          define_key_value(key),
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
                      <> define_key_label(key)
                      <> " to the non-extensible proxy target",
                  )
                True -> Ok(Nil)
              })
              use Nil <- result.map(case setting_config_false {
                True ->
                  reject_define(
                    state,
                    "'defineProperty' on proxy: trap returned truish for defining non-configurable property "
                      <> define_key_label(key)
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
                        <> define_key_label(key)
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
                        <> define_key_label(key)
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
                          <> define_key_label(key)
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
  use #(dkey, state) <- result.try(to_define_key(state, key_val))
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

/// Trap-aware [[OwnPropertyKeys]] — §10.1.11 for ordinary objects, §10.5.11
/// for proxies. Returns the full key list: integer-index strings ascending,
/// then named strings, then symbols (or whatever order the proxy trap chose).
pub fn own_keys_stateful(
  state: State(host),
  ref: Ref,
) -> Result(#(List(JsValue), State(host)), #(JsValue, State(host))) {
  case object.as_proxy(state.heap, ref) {
    Some(#(target, handler)) -> proxy_own_keys(state, target, handler)
    None -> {
      let strings =
        collect_own_keys(state.heap, ref, False) |> list.map(JsString)
      let syms =
        collect_own_symbol_keys(state.heap, ref, False) |> list.map(JsSymbol)
      Ok(#(list.append(strings, syms), state))
    }
  }
}

/// §10.5.11 Proxy [[OwnPropertyKeys]] ( ).
fn proxy_own_keys(
  state: State(host),
  target: Option(Ref),
  handler: Option(Ref),
) -> Result(#(List(JsValue), State(host)), #(JsValue, State(host))) {
  use #(t, h, trap, state) <- result.try(object.proxy_trap(
    state,
    target,
    handler,
    "ownKeys",
  ))
  case trap {
    // Step 6: no trap → target.[[OwnPropertyKeys]]().
    None -> own_keys_stateful(state, t)
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
      use #(target_keys, state) <- result.try(own_keys_stateful(state, t))
      let #(nonconf, conf) =
        list.partition(target_keys, fn(k) {
          case
            get_own_property_by_key(state.heap, t, key_value_to_define_key(k))
          {
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
                  <> key_label_of_value(k)
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
                      <> key_label_of_value(k)
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

fn key_label_of_value(k: JsValue) -> String {
  case k {
    JsString(s) -> "'" <> s <> "'"
    _ -> "[symbol]"
  }
}

/// True when `keys` contains the same String/Symbol value twice.
fn has_duplicate_keys(keys: List(JsValue), seen: List(JsValue)) -> Bool {
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
/// a String or Symbol, else TypeError.
fn keys_from_array_like(
  state: State(host),
  val: JsValue,
) -> Result(#(List(JsValue), State(host)), #(JsValue, State(host))) {
  case val {
    JsObject(arr_ref) ->
      case heap.read_array_like(state.heap, arr_ref) {
        // Fast path: genuine array-backed object — read elements directly.
        Some(#(length, els)) -> {
          let items = gather_array_like(els, 0, length, [])
          use Nil <- result.map(
            list.try_each(items, validate_trap_key(state, _)),
          )
          #(items, state)
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
/// trap result must be a String or a Symbol.
fn validate_trap_key(
  state: State(host),
  item: JsValue,
) -> Result(Nil, #(JsValue, State(host))) {
  case item {
    JsString(_) | JsSymbol(_) -> Ok(Nil)
    _ ->
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
  acc: List(JsValue),
) -> Result(#(List(JsValue), State(host)), #(JsValue, State(host))) {
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
      use Nil <- result.try(validate_trap_key(state, item))
      gather_keys_via_get(state, obj, arr_ref, idx + 1, length, [item, ..acc])
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
  use #(keys, state) <- result.try(own_keys_stateful(state, ref))
  let string_keys =
    list.filter_map(keys, fn(k) {
      case k {
        JsString(s) -> Ok(s)
        _ -> Error(Nil)
      }
    })
  list.try_fold(string_keys, #([], state), fn(acc, s) {
    let #(found, state) = acc
    use #(prop, state) <- result.map(get_own_property_stateful(
      state,
      ref,
      JsString(s),
    ))
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

/// §10.5.2 Proxy [[SetPrototypeOf]] — trap, falling through to the ordinary
/// algorithm on the (possibly nested-proxy) target when no trap is defined.
pub fn proxy_set_proto(
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
    Some(t) ->
      case object.as_proxy(state.heap, t) {
        Some(#(t2, h2)) -> proxy_set_proto(state, t2, h2, new_proto)
        None ->
          case ordinary_set_prototype_of(state, t, new_proto) {
            Ok(state) -> Ok(#(state, True))
            Error(NotExtensible) | Error(Cyclic) | Error(Immutable) ->
              Ok(#(state, False))
          }
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
  use #(keys, state) <- result.try(own_keys_stateful(state, ref))
  let string_keys =
    list.filter_map(keys, fn(k) {
      case k {
        JsString(s) -> Ok(s)
        _ -> Error(Nil)
      }
    })
  use #(seen, acc_rev, state) <- result.try(
    list.try_fold(string_keys, #(seen, acc_rev, state), fn(st, s) {
      let #(seen, acc_rev, state) = st
      case list.contains(seen, s) {
        True -> Ok(#(seen, acc_rev, state))
        False -> {
          use #(prop, state) <- result.map(get_own_property_stateful(
            state,
            ref,
            JsString(s),
          ))
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
