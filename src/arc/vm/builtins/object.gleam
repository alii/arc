import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers.{first_arg_or_undefined}
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/ops/coerce
import arc/vm/ops/object
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
  ObjectValues, OrdinaryObject, PromiseObject,
}
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// V8/Node's standard ToObject failure message.
const cannot_convert = "Cannot convert undefined or null to object"

/// Resolved ToPropertyKey result — unifies string and symbol keys for
/// apply_descriptor so it can look up/write to the right dict without
/// threading two code paths through the validation logic.
type DefineKey {
  StringKey(pkey: value.PropertyKey, display: String)
  SymbolKey(sym: value.SymbolId)
}

/// ToPropertyKey (§7.1.19) — resolves a JsValue to either a SymbolId or a
/// canonical string key. CPS so callers can `use` before `case this`, preserving
/// spec ordering (hasOwnProperty does ToPropertyKey step 1, ToObject step 2).
///
/// Per spec: ToPrimitive(argument, string) first, THEN check if the result is
/// a Symbol. Covers `{[Symbol.toPrimitive]: () => sym}` used as a key.
fn try_to_property_key(
  state: State,
  key_val: JsValue,
  cont: fn(DefineKey, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case coerce.to_primitive(state, key_val, coerce.StringHint) {
    Error(#(thrown, state)) -> #(state, Error(thrown))
    Ok(#(JsSymbol(sym), state)) -> cont(SymbolKey(sym), state)
    Ok(#(prim, state)) -> {
      use s, state <- coerce.try_to_string(state, prim)
      cont(StringKey(value.canonical_key(s), s), state)
    }
  }
}

/// [[GetOwnProperty]] dispatching on a resolved DefineKey.
fn get_own_property_by_key(
  heap: Heap,
  ref: Ref,
  key: DefineKey,
) -> Option(value.Property) {
  case key {
    SymbolKey(sym) -> object.get_own_symbol_property(heap, ref, sym)
    StringKey(pkey:, ..) -> object.get_own_property(heap, ref, pkey)
  }
}

/// CPS wrapper for `object.get_value`. Use with `use` syntax:
///   use val, state <- try_get(state, ref, key, receiver)
/// Propagates thrown errors as `#(state, Error(thrown))`.
fn try_get(
  state: State,
  ref: Ref,
  key: String,
  receiver: JsValue,
  cont: fn(JsValue, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case object.get_value(state, ref, value.canonical_key(key), receiver) {
    Ok(#(val, state)) -> cont(val, state)
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// Set up Object constructor and Object.prototype methods.
/// Object.prototype is already allocated (it's the root of all chains).
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, BuiltinType) {
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
    ])
  common.init_type_on(
    h,
    object_proto,
    function_proto,
    proto_methods,
    fn(_) { Dispatch(ObjectNative(ObjectConstructor)) },
    "Object",
    1,
    static_methods,
  )
}

/// Per-module dispatch for Object native functions.
pub fn dispatch(
  native: ObjectNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let object_proto = state.builtins.object.prototype
  case args {
    // §20.1.1.1 step 3: If value is an Object, return it directly.
    [JsObject(_) as obj, ..] -> #(state, Ok(obj))
    // §20.1.1.1 step 3: Primitives → ToObject creates a wrapper.
    [JsString(_) as v, ..]
    | [JsNumber(_) as v, ..]
    | [JsBool(_) as v, ..]
    | [JsSymbol(_) as v, ..] ->
      case common.to_object(state.heap, state.builtins, v) {
        Some(#(heap, ref)) -> #(State(..state, heap:), Ok(JsObject(ref)))
        // Should not happen — to_object handles all primitives
        None -> #(state, Ok(v))
      }
    // §20.1.1.1 step 2: undefined, null, or absent → new empty object.
    _ -> {
      let #(heap, ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: OrdinaryObject,
            properties: dict.new(),
            symbol_properties: [],
            elements: elements.new(),
            prototype: Some(object_proto),
            extensible: True,
          ),
        )
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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
    JsObject(ref) ->
      // Step 3: Let desc be ? obj.[[GetOwnProperty]](key).
      case get_own_property_by_key(state.heap, ref, key) {
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
            DataProperty(
              value: JsNumber(value.Finite(int.to_float(len))),
              writable: False,
              enumerable: False,
              configurable: False,
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
                DataProperty(
                  value: JsString(ch),
                  writable: False,
                  enumerable: True,
                  configurable: False,
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
/// 3. If IsDataDescriptor(Desc):
///    a. Create "value" property with Desc.[[Value]]
///    b. Create "writable" property with Desc.[[Writable]]
/// 4. Else (IsAccessorDescriptor):
///    a. Create "get" property with Desc.[[Get]]
///    b. Create "set" property with Desc.[[Set]]
/// 5. Create "enumerable" property with Desc.[[Enumerable]]
/// 6. Create "configurable" property with Desc.[[Configurable]]
/// 7. Return obj.
///
/// All created properties are {[[Writable]]: true, [[Enumerable]]: true,
/// [[Configurable]]: true} per spec. We use value.data_property which
/// defaults to writable=true, enumerable=true, configurable=true.
pub fn make_descriptor_object(
  heap: Heap,
  prop: value.Property,
  object_proto: Ref,
) -> #(Heap, Ref) {
  case prop {
    // Step 3: IsDataDescriptor — create "value" and "writable" properties.
    DataProperty(value: val, writable:, enumerable:, configurable:) ->
      heap.alloc(
        heap,
        ObjectSlot(
          kind: OrdinaryObject,
          properties: common.named_props([
            // Step 3a: "value"
            #("value", value.data_property(val)),
            // Step 3b: "writable"
            #("writable", value.data_property(JsBool(writable))),
            // Step 5: "enumerable"
            #("enumerable", value.data_property(JsBool(enumerable))),
            // Step 6: "configurable"
            #("configurable", value.data_property(JsBool(configurable))),
          ]),
          symbol_properties: [],
          elements: elements.new(),
          prototype: Some(object_proto),
          extensible: True,
        ),
      )
    // Step 4: IsAccessorDescriptor — create "get" and "set" properties.
    AccessorProperty(get:, set:, enumerable:, configurable:) -> {
      // Spec: Desc.[[Get]] / Desc.[[Set]] are stored as-is. If absent
      // internally (None), we emit undefined per convention.
      let get_val = option.unwrap(get, JsUndefined)
      let set_val = option.unwrap(set, JsUndefined)
      heap.alloc(
        heap,
        ObjectSlot(
          kind: OrdinaryObject,
          properties: common.named_props([
            // Step 4a: "get"
            #("get", value.data_property(get_val)),
            // Step 4b: "set"
            #("set", value.data_property(set_val)),
            // Step 5: "enumerable"
            #("enumerable", value.data_property(JsBool(enumerable))),
            // Step 6: "configurable"
            #("configurable", value.data_property(JsBool(configurable))),
          ]),
          symbol_properties: [],
          elements: elements.new(),
          prototype: Some(object_proto),
          extensible: True,
        ),
      )
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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
  state: State,
  target_ref: Ref,
  key_val: JsValue,
  desc_ref: Ref,
) -> Result(State, #(JsValue, State)) {
  // ToPropertyKey (§7.1.19): Symbol → symbol key, else ToString.
  use #(dkey, state) <- result.try(case key_val {
    JsSymbol(sym) -> Ok(#(SymbolKey(sym), state))
    _ -> {
      use #(s, state) <- result.map(coerce.js_to_string(state, key_val))
      #(StringKey(value.canonical_key(s), s), state)
    }
  })
  let desc_obj = JsObject(desc_ref)
  // ToPropertyDescriptor steps 3-8: Read fields via [[Get]] (calls getters).
  // Step 7: If Obj has "get", let getter = Get(Obj, "get").
  use #(desc_get, state) <- result.try(read_desc_field(state, desc_obj, "get"))
  // Step 8: If Obj has "set", let setter = Get(Obj, "set").
  use #(desc_set, state) <- result.try(read_desc_field(state, desc_obj, "set"))
  // Step 5: If Obj has "value", set desc.[[Value]] = Get(Obj, "value").
  use #(desc_value, state) <- result.try(read_desc_field(
    state,
    desc_obj,
    "value",
  ))
  // Step 6: If Obj has "writable", set desc.[[Writable]] = ToBoolean(Get(Obj, "writable")).
  use #(desc_writable, state) <- result.try(read_desc_bool(
    state,
    desc_obj,
    "writable",
  ))
  // Step 3: If Obj has "enumerable", set desc.[[Enumerable]] = ToBoolean(Get(Obj, "enumerable")).
  use #(desc_enumerable, state) <- result.try(read_desc_bool(
    state,
    desc_obj,
    "enumerable",
  ))
  // Step 4: If Obj has "configurable", set desc.[[Configurable]] = ToBoolean(Get(Obj, "configurable")).
  use #(desc_configurable, state) <- result.try(read_desc_bool(
    state,
    desc_obj,
    "configurable",
  ))

  // Step 7: If get is not callable and not undefined, throw TypeError.
  use Nil <- result.try(case desc_get {
    Some(g) ->
      case g != JsUndefined && !helpers.is_callable(state.heap, g) {
        True -> reject_define(state, "Getter must be a function")
        False -> Ok(Nil)
      }
    _ -> Ok(Nil)
  })
  // Step 8: If set is not callable and not undefined, throw TypeError.
  use Nil <- result.try(case desc_set {
    Some(s) ->
      case s != JsUndefined && !helpers.is_callable(state.heap, s) {
        True -> reject_define(state, "Setter must be a function")
        False -> Ok(Nil)
      }
    _ -> Ok(Nil)
  })
  // Step 9: If desc has get/set AND value/writable, throw TypeError.
  let has_accessor = option.is_some(desc_get) || option.is_some(desc_set)
  let has_data = option.is_some(desc_value) || option.is_some(desc_writable)
  use Nil <- result.try(case has_accessor && has_data {
    True ->
      reject_define(
        state,
        "Invalid property descriptor. Cannot both specify accessors and a value or writable attribute",
      )
    False -> Ok(Nil)
  })

  // Determine descriptor type: accessor if "get" or "set" is present.
  let is_accessor = has_accessor

  // DefinePropertyOrThrow / OrdinaryDefineOwnProperty (§10.1.6):
  // Merge the parsed descriptor with the existing property (if any).
  case heap.read(state.heap, target_ref) {
    Some(ObjectSlot(
      kind:,
      properties:,
      symbol_properties:,
      elements:,
      prototype:,
      extensible:,
    )) -> {
      let existing = case dkey {
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

      // §10.1.6.3 ValidateAndApplyPropertyDescriptor steps 4-11:
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
      let new_prop = case is_accessor {
        True -> {
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
          AccessorProperty(get: getter, set: setter, enumerable:, configurable:)
        }
        False -> {
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
          )
        }
      }

      // Write the new/updated property to the right dict.
      let #(properties, symbol_properties) = case dkey {
        StringKey(pkey:, ..) -> #(
          dict.insert(properties, pkey, new_prop),
          symbol_properties,
        )
        SymbolKey(sym:) -> #(
          properties,
          list.key_set(symbol_properties, sym, new_prop),
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

/// Helper to create a TypeError for defineProperty rejections.
fn reject_define(state: State, msg: String) -> Result(Nil, #(JsValue, State)) {
  let #(h, err) = common.make_type_error(state.heap, state.builtins, msg)
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
  state: State,
  desc: JsValue,
  key: String,
) -> Result(#(option.Option(JsValue), State), #(JsValue, State)) {
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
/// Per §6.2.6.5 steps 3/4/6: the raw value is coerced via ToBoolean.
/// ToBoolean(x) returns false for: undefined, null, false, +0, -0, NaN, "".
/// Everything else (including objects, non-empty strings, non-zero numbers) is true.
fn read_desc_bool(
  state: State,
  desc: JsValue,
  key: String,
) -> Result(#(option.Option(Bool), State), #(JsValue, State)) {
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
///   5. Return CreateArrayFromList(nameList).
///
fn get_own_property_names(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  own_keys_impl(args, state, array_proto, False)
}

/// Object.keys ( O ) — ES2024 §20.1.2.16
///
///   1. Let obj be ? ToObject(O).
///   2. Let nameList be ? EnumerableOwnProperties(obj, key).
///   3. Return CreateArrayFromList(nameList).
///
fn keys(args: List(JsValue), state: State) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  own_keys_impl(args, state, array_proto, True)
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
  state: State,
  array_proto: Ref,
  enumerable_only: Bool,
) -> #(State, Result(JsValue, JsValue)) {
  case first_arg_or_undefined(args) {
    JsObject(ref) -> {
      // Step 2: Collect own string-keyed properties
      let ks = collect_own_keys(state.heap, ref, enumerable_only)
      // Step 3: CreateArrayFromList(nameList)
      let #(heap, arr_ref) =
        common.alloc_array(state.heap, list.map(ks, JsString), array_proto)
      #(State(..state, heap:), Ok(JsObject(arr_ref)))
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
      let #(heap, arr_ref) = common.alloc_array(state.heap, ks, array_proto)
      #(State(..state, heap:), Ok(JsObject(arr_ref)))
    }
    // Number/boolean/symbol wrappers have no own string-keyed properties.
    _ -> {
      let #(heap, arr_ref) = common.alloc_array(state.heap, [], array_proto)
      #(State(..state, heap:), Ok(JsObject(arr_ref)))
    }
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
/// TODO(Deviation): Symbol keys are excluded here because they are stored
/// separately in symbol_properties. The spec's step 3 (symbols) would need
/// to be interleaved with string keys in property creation order.
///
/// TODO(Deviation): dict.to_list does not guarantee insertion-order for string
/// keys. Gleam dicts are unordered, so property creation order is not preserved.
/// The spec requires chronological order for non-index string keys (step 2).
pub fn collect_own_keys(
  heap: Heap,
  ref: Ref,
  enumerable_only: Bool,
) -> List(String) {
  case heap.read(heap, ref) {
    Some(ObjectSlot(kind:, properties:, elements:, ..)) -> {
      // Step 1: Array index keys in ascending numeric order
      let index_keys = case kind {
        ArrayObject(length:) -> collect_index_keys(elements, length)
        value.ArgumentsObject(length:) -> collect_index_keys(elements, length)
        _ -> []
      }
      // Step 2: Non-index string property keys (with optional enumerable filter)
      let prop_keys =
        dict.to_list(properties)
        |> list.filter_map(fn(pair) {
          let #(key, prop) = pair
          case enumerable_only {
            True ->
              case prop {
                // §7.3.23 step 3.a.ii: only include if [[Enumerable]] is true
                DataProperty(enumerable: True, ..) ->
                  Ok(value.key_to_string(key))
                _ -> Error(Nil)
              }
            False -> Ok(value.key_to_string(key))
          }
        })
      // Array exotic: "length" is a non-enumerable own property (§10.4.2)
      // Include in getOwnPropertyNames but not Object.keys
      let length_key = case kind {
        ArrayObject(_) ->
          case enumerable_only {
            True -> []
            False -> ["length"]
          }
        _ -> []
      }
      list.flatten([index_keys, length_key, prop_keys])
    }
    _ -> []
  }
}

/// Collect string representations of array indices that exist in elements.
/// Implements the array-index portion of [[OwnPropertyKeys]] (§10.1.11 step 1):
///   "For each own property key P of O that is an array index, in ascending
///    numeric index order, add P to keys."
///
/// Only includes indices where the element actually exists (not holes).
/// This correctly handles sparse arrays: [1,,3] has indices "0" and "2" but
/// not "1", matching the spec behavior where holes are not own properties.
///
/// Iterates elements.indices() (O(k)) instead of probing 0..length — a sparse
/// array with length=1e9 and one entry now completes instantly instead of
/// hitting the max_iteration RangeError workaround.
fn collect_index_keys(elements: JsElements, length: Int) -> List(String) {
  elements.indices(elements)
  |> list.filter(fn(idx) { idx < length })
  |> list.map(int.to_string)
}

/// [[HasProperty]] — §7.3.11. Walks the prototype chain looking for a string key.
/// Returns True if found as own property at any level, False if not found.
fn has_property(heap: Heap, ref: Ref, key: value.PropertyKey) -> Bool {
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let key_val = first_arg_or_undefined(args)
  // Step 1: Let P be ? ToPropertyKey(V).
  use key, state <- try_to_property_key(state, key_val)
  case this {
    // Step 2: Let O be ? ToObject(this value).
    // Step 3: Return ? HasOwnProperty(O, P).
    JsObject(ref) -> #(
      state,
      Ok(JsBool(option.is_some(get_own_property_by_key(state.heap, ref, key)))),
    )
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let key_val = first_arg_or_undefined(args)
  // Step 1: Let P be ? ToPropertyKey(V).
  use key, state <- try_to_property_key(state, key_val)
  case this {
    JsObject(ref) -> {
      // Steps 3-5: [[GetOwnProperty]] → desc.[[Enumerable]] or false.
      let result = case get_own_property_by_key(state.heap, ref, key) {
        Some(p) -> JsBool(value.prop_enumerable(p))
        None -> JsBool(False)
      }
      #(state, Ok(result))
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let heap = state.heap
  let tag = case this {
    // Step 1: If the this value is undefined, return "[object Undefined]".
    JsUndefined -> "Undefined"
    // Step 2: If the this value is null, return "[object Null]".
    JsNull -> "Null"
    // Steps 3-17 for primitives: ToObject would create a wrapper whose
    // builtinTag matches the type name, and wrappers don't have @@toStringTag.
    JsBool(_) -> "Boolean"
    JsNumber(_) -> "Number"
    JsString(_) -> "String"
    JsSymbol(_) -> "Symbol"
    value.JsBigInt(_) -> "BigInt"
    // Steps 3-17 for objects — delegated to object_tag():
    JsObject(ref) -> object_tag(heap, ref)
    value.JsUninitialized -> "Undefined"
  }
  // Step 17: Return the string-concatenation of "[object ", tag, and "]".
  #(state, Ok(JsString("[object " <> tag <> "]")))
}

/// Determine the builtinTag / @@toStringTag for an object ref.
///
/// Implements steps 4-16 of Object.prototype.toString (§20.1.3.6) for objects:
///
///   Step 15: Let tag be ? Get(O, @@toStringTag).
///   Step 16: If tag is not a String, set tag to builtinTag.
///
/// We check @@toStringTag first -- if present and a string, it wins.
/// Otherwise we fall back to builtinTag via the kind-based classification
/// (steps 5-14).
///
/// builtinTag mapping (steps 5-14):
///   ArrayObject      -> "Array"       (step 5: IsArray)
///   ArgumentsObject  -> "Arguments"   (step 6: [[ParameterMap]])
///   FunctionObject   -> "Function"    (step 7: [[Call]])
///   NativeFunction   -> "Function"    (step 7: [[Call]])
///   StringObject     -> "String"      (step 11: [[StringData]])
///   NumberObject     -> "Number"      (step 10: [[NumberData]])
///   BooleanObject    -> "Boolean"     (step 9: [[BooleanData]])
///   PromiseObject    -> "Promise"     (no spec builtinTag -- uses @@toStringTag
///                                      in spec, but we provide it as fallback)
///   GeneratorObject  -> "Generator"   (same -- spec uses @@toStringTag)
///   SymbolObject     -> "Symbol"      (same -- no spec builtinTag step)
///   OrdinaryObject   -> "Object"      (step 14: else)
///
/// TODO(Deviation): The spec defines a builtinTag step for Error (step 8)
/// which is not yet implemented since that object kind doesn't exist.
fn object_tag(heap: Heap, ref: Ref) -> String {
  case heap.read(heap, ref) {
    Some(ObjectSlot(kind:, ..)) -> {
      // Step 15: Let tag be ? Get(O, @@toStringTag).
      // Step 16: If tag is not a String, set tag to builtinTag.
      // Walk the prototype chain for @@toStringTag.
      let tag = get_to_string_tag(heap, ref)
      case tag {
        Some(t) -> t
        None ->
          // Steps 5-14: Determine builtinTag from object kind.
          case kind {
            ArrayObject(_) -> "Array"
            value.ArgumentsObject(_) -> "Arguments"
            FunctionObject(..) | NativeFunction(_) -> "Function"
            PromiseObject(_) -> "Promise"
            GeneratorObject(_) -> "Generator"
            value.AsyncGeneratorObject(_) -> "AsyncGenerator"
            value.StringObject(_) -> "String"
            value.NumberObject(_) -> "Number"
            value.BooleanObject(_) -> "Boolean"
            value.SymbolObject(_) -> "Symbol"
            value.PidObject(_) -> "Pid"
            value.SubjectObject(..) -> "Subject"
            value.TimerObject(..) -> "Timer"
            value.MapObject(..) -> "Map"
            value.SetObject(..) -> "Set"
            value.WeakMapObject(_) -> "WeakMap"
            value.WeakSetObject(_) -> "WeakSet"
            value.RegExpObject(..) -> "RegExp"
            value.DateObject(_) -> "Date"
            value.ArrayIteratorObject(..) -> "Array Iterator"
            value.SetIteratorObject(..) -> "Set Iterator"
            value.MapIteratorObject(..) -> "Map Iterator"
            value.AsyncFromSyncIteratorObject(..) -> "Async-from-Sync Iterator"
            value.IteratorHelperObject(..) -> "Iterator Helper"
            value.WrapForValidIteratorObject(..) -> "Iterator"
            OrdinaryObject -> "Object"
          }
      }
    }
    _ -> "Object"
  }
}

/// Walk the prototype chain looking for @@toStringTag (a symbol-keyed property).
/// Returns Some(tag) if found as a string data property, None otherwise.
fn get_to_string_tag(heap: Heap, ref: Ref) -> option.Option(String) {
  case heap.read(heap, ref) {
    Some(ObjectSlot(symbol_properties:, prototype:, ..)) ->
      case list.key_find(symbol_properties, value.symbol_to_string_tag) {
        Ok(DataProperty(value: JsString(tag), ..)) -> Some(tag)
        _ ->
          case prototype {
            Some(proto_ref) -> get_to_string_tag(heap, proto_ref)
            None -> None
          }
      }
    _ -> None
  }
}

/// Object.prototype.valueOf ( ) — ES2024 §20.1.3.7
///
///   1. Return ? ToObject(this value).
///
/// TODO(Deviation): Primitives (number/string/boolean/symbol) are returned
/// as-is instead of creating a wrapper object. In practice this rarely matters
/// because primitive valueOf methods (e.g. Number.prototype.valueOf) override
/// this, and callers that receive a primitive can work with it directly.
fn object_value_of(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Step 1: Return ? ToObject(this value).
  // ToObject throws TypeError on null/undefined.
  case this {
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    _ -> #(state, Ok(this))
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  // Steps 1-2: ToObject + EnumerableOwnProperties(obj, value)
  use vals, state <- own_values_impl(args, state)
  // Step 3: CreateArrayFromList(nameList)
  let #(heap, arr_ref) = common.alloc_array(state.heap, vals, array_proto)
  #(State(..state, heap:), Ok(JsObject(arr_ref)))
}

/// Object.entries ( O ) — ES2024 §20.1.2.5
///
///   1. Let obj be ? ToObject(O).
///   2. Let nameList be ? EnumerableOwnProperties(obj, key+value).
///   3. Return CreateArrayFromList(nameList).
///
/// EnumerableOwnProperties with kind=key+value (§7.3.23 step 3.a.ii.2):
///   "Let entry be CreateArrayFromList(« key, value »)."
///   "Append entry to properties."
fn entries(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  // Steps 1-2: ToObject + EnumerableOwnProperties(obj, key+value)
  use pairs, state <- own_entries_impl(args, state)
  // Step 3: Build the result — each entry is CreateArrayFromList(« key, value »)
  let #(heap, refs) =
    list.fold(pairs, #(state.heap, []), fn(acc, kv) {
      let #(h, rs) = acc
      let #(k, v) = kv
      // §7.3.23 step 3.a.ii.2: CreateArrayFromList(« key, value »)
      let #(h, r) = common.alloc_array(h, [JsString(k), v], array_proto)
      #(h, [JsObject(r), ..rs])
    })
  // Outer CreateArrayFromList for the result array
  let #(heap, arr_ref) =
    common.alloc_array(heap, list.reverse(refs), array_proto)
  #(State(..state, heap:), Ok(JsObject(arr_ref)))
}

/// Shared driver implementing steps 1-2 of Object.values / the value-collection
/// portion of EnumerableOwnProperties ( O, value ) — §7.3.23:
///
///   1. Let ownKeys be ? O.[[OwnPropertyKeys]]().
///   2. Let properties be a new empty List.
///   3. For each element key of ownKeys, do
///      a. If key is a String, then
///         i. Let desc be ? O.[[GetOwnProperty]](key).
///         ii. If desc is not undefined and desc.[[Enumerable]] is true, then
///             1. Let value be ? Get(O, key).
///             2. Append value to properties.
///   4. Return properties.
///
/// The ToObject step (§20.1.2.22 step 1) is inlined: null/undefined throw,
/// primitives short-circuit to empty (wrapper objects not yet implemented).
///
fn own_values_impl(
  args: List(JsValue),
  state: State,
  cont: fn(List(JsValue), State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case first_arg_or_undefined(args) {
    JsObject(ref) as receiver -> {
      // §7.3.23 step 1: Let ownKeys be ? O.[[OwnPropertyKeys]]()
      // (filtered to enumerable-only string keys)
      let ks = collect_own_keys(state.heap, ref, True)
      // §7.3.23 step 3: For each key, Get(O, key) and collect
      use vals, state <- state.try_op(
        collect_values(state, ref, receiver, ks, []),
      )
      cont(list.reverse(vals), state)
    }
    // ToObject: null/undefined → TypeError
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    // String primitives: enumerable own properties are the index characters.
    // §7.1.18: ToObject(String) creates a String wrapper whose own enumerable
    // string-keyed properties are the individual characters at indices 0..len-1.
    JsString(s) -> {
      let chars = string.to_graphemes(s)
      cont(list.map(chars, JsString), state)
    }
    // Number/boolean/symbol wrappers have no own enumerable string keys.
    _ -> cont([], state)
  }
}

/// Collect values for Object.values — implements §7.3.23 step 3.a.ii.1:
///   "Let value be ? Get(O, key)."
///   "Append value to properties."
///
/// Uses object.get_value which is the [[Get]] internal method (§10.1.8),
/// properly invoking getter accessors and walking the prototype chain.
/// Accumulates in reverse order (caller reverses).
fn collect_values(
  state: State,
  ref: Ref,
  receiver: JsValue,
  keys: List(String),
  acc: List(JsValue),
) -> Result(#(List(JsValue), State), #(JsValue, State)) {
  case keys {
    [] -> Ok(#(acc, state))
    [k, ..rest] -> {
      // §7.3.23 step 3.a.ii.1: Let value be ? Get(O, key)
      use #(val, state) <- result.try(object.get_value(
        state,
        ref,
        value.canonical_key(k),
        receiver,
      ))
      collect_values(state, ref, receiver, rest, [val, ..acc])
    }
  }
}

/// Shared driver implementing steps 1-2 of Object.entries / the entry-collection
/// portion of EnumerableOwnProperties ( O, key+value ) — §7.3.23:
///
///   1. Let ownKeys be ? O.[[OwnPropertyKeys]]().
///   2. Let properties be a new empty List.
///   3. For each element key of ownKeys, do
///      a. If key is a String, then
///         i. Let desc be ? O.[[GetOwnProperty]](key).
///         ii. If desc is not undefined and desc.[[Enumerable]] is true, then
///             1. Let value be ? Get(O, key).
///             2. Let entry be CreateArrayFromList(« key, value »).
///             3. Append entry to properties.
///   4. Return properties.
///
fn own_entries_impl(
  args: List(JsValue),
  state: State,
  cont: fn(List(#(String, JsValue)), State) ->
    #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case first_arg_or_undefined(args) {
    JsObject(ref) as receiver -> {
      // §7.3.23 step 1: ownKeys (filtered to enumerable string keys)
      let ks = collect_own_keys(state.heap, ref, True)
      // §7.3.23 step 3: collect key+value pairs
      use pairs, state <- state.try_op(
        collect_entries(state, ref, receiver, ks, []),
      )
      cont(list.reverse(pairs), state)
    }
    // ToObject: null/undefined → TypeError
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    // String primitives: enumerable own properties are the index characters.
    // §7.1.18: ToObject(String) creates a String wrapper whose own enumerable
    // string-keyed properties are the individual characters at indices 0..len-1.
    JsString(s) -> {
      let chars = string.to_graphemes(s)
      let pairs =
        list.index_map(chars, fn(ch, idx) {
          #(int.to_string(idx), JsString(ch))
        })
      cont(pairs, state)
    }
    // Number/boolean/symbol wrappers have no own enumerable string keys.
    _ -> cont([], state)
  }
}

/// Collect entries for Object.entries — implements §7.3.23 step 3.a.ii (key+value):
///   "Let value be ? Get(O, key)."
///   "Let entry be CreateArrayFromList(« key, value »)."
///   "Append entry to properties."
///
/// Returns #(key, value) tuples; the caller (entries/own_entries_impl) wraps
/// each tuple into a [key, value] array via CreateArrayFromList.
/// Accumulates in reverse order (caller reverses).
fn collect_entries(
  state: State,
  ref: Ref,
  receiver: JsValue,
  keys: List(String),
  acc: List(#(String, JsValue)),
) -> Result(#(List(#(String, JsValue)), State), #(JsValue, State)) {
  case keys {
    [] -> Ok(#(acc, state))
    [k, ..rest] -> {
      // §7.3.23 step 3.a.ii.1: Let value be ? Get(O, key)
      use #(val, state) <- result.try(object.get_value(
        state,
        ref,
        value.canonical_key(k),
        receiver,
      ))
      collect_entries(state, ref, receiver, rest, [#(k, val), ..acc])
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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
  state: State,
  target_ref: Ref,
  props_val: JsValue,
) -> #(State, Result(JsValue, JsValue)) {
  case props_val {
    JsObject(props_ref) -> {
      // Steps 2-3: Get own keys (filtered to enumerable-only).
      let ks = collect_own_keys(state.heap, props_ref, True)
      // Steps 4-5 (merged): For each key, read descriptor, validate, apply.
      define_props_loop(state, target_ref, props_ref, ks)
    }
    // Step 1: ToObject throws TypeError on null/undefined.
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    // Step 1: ToObject on primitives → wrapper with no own enumerable props → no-op.
    _ -> #(state, Ok(JsObject(target_ref)))
  }
}

/// Loop helper for ObjectDefineProperties (§20.1.2.3.1 steps 4-5 merged).
///
/// For each remaining key:
///   Step 4.a: Let propDesc be ? props.[[GetOwnProperty]](nextKey).
///   Step 4.b.i: Let descObj be ? Get(props, nextKey).
///   Step 4.b.ii: Let desc be ? ToPropertyDescriptor(descObj).
///     — ToPropertyDescriptor (§6.2.6.5) requires descObj to be an Object;
///       if not, throw TypeError.
///   Step 5.a: Perform ? DefinePropertyOrThrow(O, key, desc).
///     — Handled by apply_descriptor.
///   Step 6: Return O.
fn define_props_loop(
  state: State,
  target_ref: Ref,
  props_ref: Ref,
  remaining: List(String),
) -> #(State, Result(JsValue, JsValue)) {
  case remaining {
    // Step 6: Return O (all keys processed).
    [] -> #(state, Ok(JsObject(target_ref)))
    [key, ..rest] ->
      case
        object.get_own_property(state.heap, props_ref, value.canonical_key(key))
      {
        // Steps 4.b.i + 4.b.ii: descObj is an Object → ToPropertyDescriptor.
        Some(DataProperty(value: JsObject(desc_ref), ..)) -> {
          // Step 5.a: DefinePropertyOrThrow(O, key, desc).
          use state <- state.try_state(apply_descriptor(
            state,
            target_ref,
            JsString(key),
            desc_ref,
          ))
          define_props_loop(state, target_ref, props_ref, rest)
        }
        // Step 4.b.ii: ToPropertyDescriptor throws if descObj is not an Object.
        Some(_) ->
          state.type_error(state, "Property description must be an object")
        // Step 4.a: propDesc is undefined → skip (key has no own property).
        None -> define_props_loop(state, target_ref, props_ref, rest)
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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
  state: State,
  target_ref: Ref,
  sources: List(JsValue),
) -> Result(State, #(JsValue, State)) {
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
  state: State,
  target_ref: Ref,
  source: JsValue,
) -> Result(State, #(JsValue, State)) {
  case source {
    JsObject(src_ref) as receiver -> {
      // Step 3.a.ii: Let keys be ? from.[[OwnPropertyKeys]]().
      // String keys first:
      let ks = collect_own_keys(state.heap, src_ref, True)
      // Step 3.a.iii: For each string key, copy it.
      use state <- result.try(assign_keys(
        state,
        target_ref,
        src_ref,
        receiver,
        ks,
      ))
      // Symbol keys next (also enumerable-only):
      let sym_ks = collect_own_symbol_keys(state.heap, src_ref, True)
      assign_symbol_keys(state, target_ref, src_ref, receiver, sym_ks)
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
  state: State,
  target_ref: Ref,
  chars: List(String),
  idx: Int,
) -> Result(State, #(JsValue, State)) {
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
/// is pre-filtered by collect_own_keys (enumerable_only=True), so we only
/// iterate keys that are already known to be own + enumerable. Uses
/// object.get_value ([[Get]]) which invokes getters, and object.set_value
/// ([[Set]]) which invokes setters.
fn assign_keys(
  state: State,
  target_ref: Ref,
  src_ref: Ref,
  receiver: JsValue,
  keys: List(String),
) -> Result(State, #(JsValue, State)) {
  case keys {
    [] -> Ok(state)
    [k, ..rest] -> {
      let pk = value.canonical_key(k)
      // Step 2.a: Let propValue be ? Get(from, nextKey).
      use #(val, state) <- result.try(object.get_value(
        state,
        src_ref,
        pk,
        receiver,
      ))
      // Step 2.b: Perform ? Set(to, nextKey, propValue, true).
      use #(state, _) <- result.try(object.set_value(
        state,
        target_ref,
        pk,
        val,
        JsObject(target_ref),
      ))
      assign_keys(state, target_ref, src_ref, receiver, rest)
    }
  }
}

/// Collect enumerable own symbol keys from an object.
/// Implements the symbol portion of [[OwnPropertyKeys]] (§10.1.11 step 3)
/// with optional enumerable filtering.
pub fn collect_own_symbol_keys(
  heap: Heap,
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

/// Symbol-key-copy loop for Object.assign — copies enumerable own symbol
/// properties from source to target.
fn assign_symbol_keys(
  state: State,
  target_ref: Ref,
  src_ref: Ref,
  receiver: JsValue,
  keys: List(value.SymbolId),
) -> Result(State, #(JsValue, State)) {
  case keys {
    [] -> Ok(state)
    [sym, ..rest] -> {
      use #(val, state) <- result.try(object.get_symbol_value(
        state,
        src_ref,
        sym,
        receiver,
      ))
      use #(state, _ok) <- result.try(object.set_symbol_value(
        state,
        target_ref,
        sym,
        val,
        JsObject(target_ref),
      ))
      assign_symbol_keys(state, target_ref, src_ref, receiver, rest)
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
/// Implementation: Gleam's structural `==` on JsValue IS SameValue because:
///   - `JsNumber(NaN) == JsNumber(NaN)` is True (constructor equality on BEAM)
///   - `Finite(0.0) == Finite(-0.0)` is False (BEAM `=:=` distinguishes ±0)
///   - All other types use structural equality, matching SameValueNonNumber.
fn is(args: List(JsValue), state: State) -> #(State, Result(JsValue, JsValue)) {
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let #(target, key_val) = case args {
    [t, k, ..] -> #(t, k)
    [t] -> #(t, JsUndefined)
    [] -> #(JsUndefined, JsUndefined)
  }
  case target {
    JsObject(ref) -> {
      // Step 1: ToObject(O) — identity for objects.
      // Steps 2-3: ToPropertyKey + HasOwnProperty(obj, key).
      use key, state <- try_to_property_key(state, key_val)
      #(
        state,
        Ok(
          JsBool(option.is_some(get_own_property_by_key(state.heap, ref, key))),
        ),
      )
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case first_arg_or_undefined(args) {
    JsObject(ref) -> {
      // Step 2: obj.[[GetPrototypeOf]]() — OrdinaryGetPrototypeOf (§10.1.1.1)
      // Returns O.[[Prototype]] (an Object or null).
      let proto = case heap.read(state.heap, ref) {
        Some(ObjectSlot(prototype: Some(p), ..)) -> JsObject(p)
        _ -> JsNull
      }
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
    // Symbol/BigInt wrappers — we don't have dedicated prototypes yet.
    // Symbol.prototype currently aliases Object.prototype; BigInt has none.
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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
      // §20.1.2.21 step 4: O.[[SetPrototypeOf]](proto)
      // OrdinarySetPrototypeOf §10.1.2.1:
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(prototype: current, extensible:, ..)) -> {
          // §10.1.2.1 step 2: If SameValue(V, current) is true, return true.
          case new_proto == current {
            True ->
              // Already the same — return O.
              #(state, Ok(target))
            False ->
              // §10.1.2.1 step 4: If extensible is false, return false.
              case extensible {
                False ->
                  state.type_error(
                    state,
                    "Cannot set prototype of a non-extensible object",
                  )
                True ->
                  // §10.1.2.1 step 7: cycle detection
                  case would_create_cycle(state.heap, ref, new_proto) {
                    // §20.1.2.21 step 5: status is false → throw TypeError.
                    True -> state.type_error(state, "Cyclic __proto__ value")
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
                      // §20.1.2.21 step 6: Return O.
                      #(State(..state, heap:), Ok(target))
                    }
                  }
              }
          }
        }
        _ ->
          // Should not happen — ref doesn't point to an ObjectSlot
          #(state, Ok(target))
      }
    // §20.1.2.21 step 3: If O is not an Object, return O.
    _, Ok(_) -> #(state, Ok(target))
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
pub fn would_create_cycle(
  heap: Heap,
  target_ref: Ref,
  new_proto: Option(Ref),
) -> Bool {
  case new_proto {
    // §10.1.2.1 step 7a: p is null → done, no cycle.
    None -> False
    // §10.1.2.1 step 7b: SameValue(p, O) → cycle detected.
    Some(p) if p == target_ref -> True
    // §10.1.2.1 step 7d: set p to p.[[Prototype]] and continue.
    Some(p) ->
      case heap.read(heap, p) {
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
    DataProperty(value:, enumerable:, ..) ->
      DataProperty(value:, writable: False, enumerable:, configurable: False)
    // §7.3.16 step 6.b: AccessorDescriptor → configurable=false only.
    AccessorProperty(get:, set:, enumerable:, ..) ->
      AccessorProperty(get:, set:, enumerable:, configurable: False)
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
  state: State,
  transform: fn(value.Property) -> value.Property,
) -> #(State, Result(JsValue, JsValue)) {
  let target = first_arg_or_undefined(args)
  case target {
    JsObject(ref) -> {
      let heap = {
        use slot <- heap.update(state.heap, ref)
        case slot {
          ObjectSlot(properties:, symbol_properties:, ..) ->
            ObjectSlot(
              ..slot,
              properties: dict.map_values(properties, fn(_, p) { transform(p) }),
              symbol_properties: list.map(symbol_properties, fn(pair) {
                #(pair.0, transform(pair.1))
              }),
              extensible: False,
            )
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
///   4. Return O.
fn freeze(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let target = first_arg_or_undefined(args)
  case target {
    JsObject(ref) -> {
      // §20.1.2.17 step 2: O.[[PreventExtensions]]()
      // §10.1.4.1 step 1: Set O.[[Extensible]] to false.
      let h = {
        use slot <- heap.update(state.heap, ref)
        case slot {
          ObjectSlot(..) -> ObjectSlot(..slot, extensible: False)
          _ -> slot
        }
      }
      // §20.1.2.17 step 4: Return O.
      #(State(..state, heap: h), Ok(target))
    }
    // §20.1.2.17 step 1: If O is not an Object, return O.
    _ -> #(state, Ok(target))
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
  state: State,
  check: fn(List(value.Property)) -> Bool,
) -> #(State, Result(JsValue, JsValue)) {
  let result = case first_arg_or_undefined(args) {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(properties:, symbol_properties:, extensible: False, ..)) ->
          check(dict.values(properties))
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let result = case first_arg_or_undefined(args) {
    JsObject(ref) ->
      // §20.1.2.13 step 2: IsExtensible(O) → O.[[Extensible]]
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(extensible:, ..)) -> extensible
        _ -> False
      }
    // §20.1.2.13 step 1: If O is not an Object, return false.
    _ -> False
  }
  #(state, Ok(JsBool(result)))
}

/// Object.seal ( O ) — ES2024 §20.1.2.20
///
///   1. If O is not an Object, return O.
///   2. Let status be ? SetIntegrityLevel(O, sealed).
///   4. Return O.
fn seal(args: List(JsValue), state: State) -> #(State, Result(JsValue, JsValue)) {
  set_integrity_level(args, state, seal_prop)
}

/// Helper for seal — make property non-configurable (but keep writable as-is).
fn seal_prop(prop: value.Property) -> value.Property {
  case prop {
    DataProperty(value:, writable:, enumerable:, ..) ->
      DataProperty(value:, writable:, enumerable:, configurable: False)
    AccessorProperty(get:, set:, enumerable:, ..) ->
      AccessorProperty(get:, set:, enumerable:, configurable: False)
  }
}

/// Object.isSealed ( O ) — ES2024 §20.1.2.15
///
///   1. If O is not an Object, return true.
///   2. Return ? TestIntegrityLevel(O, sealed).
fn is_sealed(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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
  state: State,
  str_acc: List(#(String, value.Property)),
  sym_acc: List(#(value.SymbolId, value.Property)),
) -> #(State, Result(JsValue, JsValue)) {
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let object_proto = state.builtins.object.prototype
  let target = first_arg_or_undefined(args)
  case target {
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    JsObject(ref) -> {
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(properties:, ..)) -> {
          // Build descriptor objects for each own property
          let #(heap, desc_props) =
            dict.fold(properties, #(state.heap, dict.new()), fn(acc, key, prop) {
              let #(h, descs) = acc
              let #(h, desc_ref) = make_descriptor_object(h, prop, object_proto)
              #(
                h,
                dict.insert(descs, key, value.data_property(JsObject(desc_ref))),
              )
            })
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  case first_arg_or_undefined(args) {
    // Step 1: ToObject — null/undefined throw TypeError.
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    JsObject(ref) -> {
      // Step 4: Collect own symbol keys.
      let syms = collect_own_symbol_keys(state.heap, ref, False)
      // Step 5: CreateArrayFromList(nameList) — each element is a JsSymbol.
      let #(heap, arr_ref) =
        common.alloc_array(state.heap, list.map(syms, JsSymbol), array_proto)
      #(State(..state, heap:), Ok(JsObject(arr_ref)))
    }
    // For non-object primitives (string, number, boolean, symbol):
    // ToObject creates a wrapper with no own symbol properties → return [].
    _ -> {
      let #(heap, arr_ref) = common.alloc_array(state.heap, [], array_proto)
      #(State(..state, heap:), Ok(JsObject(arr_ref)))
    }
  }
}

/// ES2024 §20.1.3.3 Object.prototype.isPrototypeOf ( V )
fn is_prototype_of(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let v = first_arg_or_undefined(args)
  // Step 1: If V is not an Object, return false.
  case v {
    JsObject(v_ref) -> {
      // Step 2: Let O be ? ToObject(this value).
      case this {
        JsObject(this_ref) -> is_prototype_of_loop(state, v_ref, this_ref)
        _ -> #(state, Ok(JsBool(False)))
      }
    }
    _ -> #(state, Ok(JsBool(False)))
  }
}

/// Walk the prototype chain of v_ref looking for this_ref.
fn is_prototype_of_loop(
  state: State,
  v_ref: Ref,
  this_ref: Ref,
) -> #(State, Result(JsValue, JsValue)) {
  case heap.read(state.heap, v_ref) {
    Some(ObjectSlot(prototype: Some(proto_ref), ..)) ->
      case proto_ref == this_ref {
        True -> #(state, Ok(JsBool(True)))
        False -> is_prototype_of_loop(state, proto_ref, this_ref)
      }
    _ -> #(state, Ok(JsBool(False)))
  }
}

/// ES2024 §20.1.3.5 Object.prototype.toLocaleString ( )
/// Default implementation: call this.toString().
fn object_to_locale_string(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) -> {
      use to_string_fn, state <- try_get(state, ref, "toString", this)
      case helpers.is_callable(state.heap, to_string_fn) {
        True -> {
          use result, state <- state.try_call(state, to_string_fn, this, [])
          #(state, Ok(result))
        }
        False ->
          state.type_error(state, "toLocaleString: toString is not callable")
      }
    }
    _ -> {
      use s, state <- coerce.try_to_string(state, this)
      #(state, Ok(JsString(s)))
    }
  }
}

/// ES2024 §22.1.2.4 Object.groupBy ( items, callbackfn )
fn group_by(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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
  state: State,
  items: List(JsValue),
  callback: JsValue,
  index: Int,
  groups: dict.Dict(String, List(JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
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
        value.JsNumber(value.Finite(int.to_float(index))),
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
