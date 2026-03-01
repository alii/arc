import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers
import arc/vm/frame.{type State, State}
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/object
import arc/vm/value.{
  type JsElements, type JsValue, type ObjectNativeFn, type Ref, AccessorProperty,
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
  ObjectSetPrototypeOf, ObjectSlot, ObjectValues, OrdinaryObject, PromiseObject,
}
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// V8/Node's standard ToObject failure message.
const cannot_convert = "Cannot convert undefined or null to object"

/// Extract the first argument, defaulting to undefined.
fn first_arg(args: List(JsValue)) -> JsValue {
  case args {
    [v, ..] -> v
    [] -> JsUndefined
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
  }
}

/// Object() / new Object() constructor.
/// ES2024 §20.1.1.1 Object ( [ value ] )
///
///   1. If NewTarget is neither undefined nor the active function object, then
///      (skipped — no NewTarget tracking yet)
///   2. If value is undefined or null, return OrdinaryObjectCreate(%Object.prototype%).
///   3. Return ! ToObject(value).
pub fn call_native(
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
            symbol_properties: dict.new(),
            elements: js_elements.new(),
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
pub fn get_own_property_descriptor(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let object_proto = state.builtins.object.prototype
  let #(target, key_val) = case args {
    [t, k, ..] -> #(t, k)
    [t] -> #(t, JsUndefined)
    [] -> #(JsUndefined, JsUndefined)
  }
  case target {
    JsObject(ref) -> {
      // Step 2: Let key be ? ToPropertyKey(P).
      // (We use ToString here; spec uses ToPropertyKey which also handles Symbols.)
      use key_str, state <- frame.try_to_string(state, key_val)
      // Step 3: Let desc be ? obj.[[GetOwnProperty]](key).
      case object.get_own_property(state.heap, ref, key_str) {
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
    JsNull | JsUndefined -> frame.type_error(state, cannot_convert)
    // String primitives: index properties + "length" are own properties.
    JsString(s) -> {
      use key_str, state <- frame.try_to_string(state, key_val)
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
fn make_descriptor_object(
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
          properties: dict.from_list([
            // Step 3a: "value"
            #("value", value.data_property(val)),
            // Step 3b: "writable"
            #("writable", value.data_property(JsBool(writable))),
            // Step 5: "enumerable"
            #("enumerable", value.data_property(JsBool(enumerable))),
            // Step 6: "configurable"
            #("configurable", value.data_property(JsBool(configurable))),
          ]),
          symbol_properties: dict.new(),
          elements: js_elements.new(),
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
          properties: dict.from_list([
            // Step 4a: "get"
            #("get", value.data_property(get_val)),
            // Step 4b: "set"
            #("set", value.data_property(set_val)),
            // Step 5: "enumerable"
            #("enumerable", value.data_property(JsBool(enumerable))),
            // Step 6: "configurable"
            #("configurable", value.data_property(JsBool(configurable))),
          ]),
          symbol_properties: dict.new(),
          elements: js_elements.new(),
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
pub fn define_property(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case args {
    [JsObject(ref) as obj, key_val, JsObject(desc_ref), ..] -> {
      // Step 2: Let key be ? ToPropertyKey(P).
      // (Uses ToString; spec uses ToPropertyKey which also handles Symbols.)
      use key_str, state <- frame.try_to_string(state, key_val)
      // Steps 3-4: ToPropertyDescriptor + DefinePropertyOrThrow
      // (apply_descriptor combines both steps.)
      case apply_descriptor(state, ref, key_str, desc_ref) {
        // Step 5: Return O.
        Ok(state) -> #(state, Ok(obj))
        Error(#(thrown, state)) -> #(state, Error(thrown))
      }
    }
    // Step 3 (implicit): Attributes is not an Object — TypeError.
    // Spec: ToPropertyDescriptor step 1 throws if Type(Obj) is not Object.
    [JsObject(_), _, ..] ->
      frame.type_error(state, "Property description must be an object")
    // Step 1: If O is not an Object, throw a TypeError.
    _ -> frame.type_error(state, "Object.defineProperty called on non-object")
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
fn apply_descriptor(
  state: State,
  target_ref: Ref,
  key: String,
  desc_ref: Ref,
) -> Result(State, #(JsValue, State)) {
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
        True -> {
          let #(h, err) =
            common.make_type_error(
              state.heap,
              state.builtins,
              "Getter must be a function",
            )
          Error(#(err, State(..state, heap: h)))
        }
        False -> Ok(Nil)
      }
    _ -> Ok(Nil)
  })
  // Step 8: If set is not callable and not undefined, throw TypeError.
  use Nil <- result.try(case desc_set {
    Some(s) ->
      case s != JsUndefined && !helpers.is_callable(state.heap, s) {
        True -> {
          let #(h, err) =
            common.make_type_error(
              state.heap,
              state.builtins,
              "Setter must be a function",
            )
          Error(#(err, State(..state, heap: h)))
        }
        False -> Ok(Nil)
      }
    _ -> Ok(Nil)
  })
  // Step 9: If desc has get/set AND value/writable, throw TypeError.
  let has_accessor = option.is_some(desc_get) || option.is_some(desc_set)
  let has_data = option.is_some(desc_value) || option.is_some(desc_writable)
  use Nil <- result.try(case has_accessor && has_data {
    True -> {
      let #(h, err) =
        common.make_type_error(
          state.heap,
          state.builtins,
          "Invalid property descriptor. Cannot both specify accessors and a value or writable attribute",
        )
      Error(#(err, State(..state, heap: h)))
    }
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
      let existing = dict.get(properties, key)

      // §10.1.6.3 step 2: If property doesn't exist and object is not extensible, reject.
      use Nil <- result.try(case existing, extensible {
        Error(_), False -> {
          let #(h, err) =
            common.make_type_error(
              state.heap,
              state.builtins,
              "Cannot define property " <> key <> ", object is not extensible",
            )
          Error(#(err, State(..state, heap: h)))
        }
        _, _ -> Ok(Nil)
      })

      // §10.1.6.3 ValidateAndApplyPropertyDescriptor steps 4-11:
      // Validate that the change is permitted on non-configurable properties.
      use Nil <- result.try(case existing {
        Ok(existing_prop) -> {
          let current_configurable = case existing_prop {
            DataProperty(configurable: c, ..)
            | AccessorProperty(configurable: c, ..) -> c
          }
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
              let current_enumerable = case existing_prop {
                DataProperty(enumerable: e, ..)
                | AccessorProperty(enumerable: e, ..) -> e
              }
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
          let enumerable = case desc_enumerable {
            Some(e) -> e
            _ ->
              case existing {
                Ok(DataProperty(enumerable: e, ..))
                | Ok(AccessorProperty(enumerable: e, ..)) -> e
                _ -> False
              }
          }
          let configurable = case desc_configurable {
            Some(c) -> c
            _ ->
              case existing {
                Ok(DataProperty(configurable: c, ..))
                | Ok(AccessorProperty(configurable: c, ..)) -> c
                _ -> False
              }
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
          let final_enumerable = case desc_enumerable {
            Some(e) -> e
            _ ->
              case existing {
                Ok(DataProperty(enumerable: e, ..))
                | Ok(AccessorProperty(enumerable: e, ..)) -> e
                _ -> False
              }
          }
          let final_configurable = case desc_configurable {
            Some(c) -> c
            _ ->
              case existing {
                Ok(DataProperty(configurable: c, ..))
                | Ok(AccessorProperty(configurable: c, ..)) -> c
                _ -> False
              }
          }
          DataProperty(
            value: final_value,
            writable: final_writable,
            enumerable: final_enumerable,
            configurable: final_configurable,
          )
        }
      }

      // Write the new/updated property to the object.
      let new_props = dict.insert(properties, key, new_prop)
      let h =
        heap.write(
          state.heap,
          target_ref,
          ObjectSlot(
            kind:,
            properties: new_props,
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
  use #(val, state) <- result.try(object.get_value_of(state, desc, key))
  case val {
    JsUndefined ->
      // get_value_of returns undefined for both "absent" and "present with value
      // undefined". We need HasProperty semantics (proto chain walk) to distinguish.
      case desc {
        JsObject(ref) ->
          case has_property(state.heap, ref, key) {
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
  case field {
    // ToBoolean: already a boolean
    Some(JsBool(b)) -> #(Some(b), state)
    // ToBoolean falsy values
    Some(JsUndefined) -> #(Some(False), state)
    Some(JsNull) -> #(Some(False), state)
    Some(JsNumber(value.Finite(0.0))) -> #(Some(False), state)
    Some(JsNumber(value.NaN)) -> #(Some(False), state)
    Some(JsString("")) -> #(Some(False), state)
    // ToBoolean: everything else is truthy (objects, non-empty strings, non-zero numbers, symbols)
    Some(_) -> #(Some(True), state)
    // Field absent — not set in descriptor (different from false!)
    option.None -> #(option.None, state)
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
///   5. Return CreateArrayFromList(nameList).
///
pub fn get_own_property_names(
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
pub fn keys(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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
  case first_arg(args) {
    JsObject(ref) -> {
      // Step 2: Collect own string-keyed properties
      let ks = collect_own_keys(state.heap, ref, enumerable_only)
      // Step 3: CreateArrayFromList(nameList)
      let #(heap, arr_ref) =
        common.alloc_array(state.heap, list.map(ks, JsString), array_proto)
      #(State(..state, heap:), Ok(JsObject(arr_ref)))
    }
    // Step 1: ToObject — null/undefined throw TypeError
    JsNull | JsUndefined -> frame.type_error(state, cannot_convert)
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
fn collect_own_keys(heap: Heap, ref: Ref, enumerable_only: Bool) -> List(String) {
  case heap.read(heap, ref) {
    Some(ObjectSlot(kind:, properties:, elements:, ..)) -> {
      // Step 1: Array index keys in ascending numeric order
      let index_keys = case kind {
        ArrayObject(length:) -> collect_index_keys(elements, 0, length, [])
        value.ArgumentsObject(length:) ->
          collect_index_keys(elements, 0, length, [])
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
                DataProperty(enumerable: True, ..) -> Ok(key)
                _ -> Error(Nil)
              }
            False -> Ok(key)
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
fn collect_index_keys(
  elements: JsElements,
  idx: Int,
  length: Int,
  acc: List(String),
) -> List(String) {
  case idx >= length {
    True -> list.reverse(acc)
    False ->
      case js_elements.has(elements, idx) {
        True ->
          collect_index_keys(elements, idx + 1, length, [
            int.to_string(idx),
            ..acc
          ])
        False -> collect_index_keys(elements, idx + 1, length, acc)
      }
  }
}

/// [[HasProperty]] — §7.3.11. Walks the prototype chain looking for a string key.
/// Returns True if found as own property at any level, False if not found.
fn has_property(heap: Heap, ref: Ref, key: String) -> Bool {
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
/// TODO(Deviation): ToPropertyKey is approximated by ToString — symbol
/// arguments are not yet handled as property keys.
pub fn has_own_property(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let key_val = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  // Step 1: Let P be ? ToPropertyKey(V).
  use key_str, state <- frame.try_to_string(state, key_val)
  case this {
    // Step 2: Let O be ? ToObject(this value).
    // Step 3: Return ? HasOwnProperty(O, P).
    JsObject(ref) -> {
      let result = case object.get_own_property(state.heap, ref, key_str) {
        Some(_) -> JsBool(True)
        None -> JsBool(False)
      }
      #(state, Ok(result))
    }
    // Step 2: ToObject throws TypeError on null/undefined.
    JsNull | JsUndefined -> frame.type_error(state, cannot_convert)
    // String primitives: own keys are index chars + "length"
    JsString(s) -> {
      let len = string.length(s)
      let result = case key_str == "length" {
        True -> JsBool(True)
        False ->
          case int.parse(key_str) {
            Ok(i) if i >= 0 && i < len -> JsBool(True)
            _ -> JsBool(False)
          }
      }
      #(state, Ok(result))
    }
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
/// TODO(Deviation): ToPropertyKey is approximated by ToString — symbol
/// keys are not yet handled as property keys.
pub fn property_is_enumerable(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let key_val = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  // Step 1: Let P be ? ToPropertyKey(V).
  use key_str, state <- frame.try_to_string(state, key_val)
  case this {
    JsObject(ref) -> {
      // Step 2: Let O be ? ToObject(this value).
      // Step 3: Let desc be ? O.[[GetOwnProperty]](P).
      // Steps 4-5: If desc is undefined return false, else return
      //   desc.[[Enumerable]].
      let result = case object.get_own_property(state.heap, ref, key_str) {
        Some(DataProperty(enumerable: e, ..))
        | Some(AccessorProperty(enumerable: e, ..)) -> JsBool(e)
        _ -> JsBool(False)
      }
      #(state, Ok(result))
    }
    // Step 2: ToObject throws TypeError on null/undefined.
    JsNull | JsUndefined -> frame.type_error(state, cannot_convert)
    // String primitives: index properties are own+enumerable.
    // "length" is own but non-enumerable.
    JsString(s) -> {
      let len = string.length(s)
      let result = case int.parse(key_str) {
        Ok(i) if i >= 0 && i < len -> JsBool(True)
        _ -> JsBool(False)
      }
      #(state, Ok(result))
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
pub fn object_to_string(
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
/// TODO(Deviation): The spec defines builtinTag steps for Error (step 8) and
/// Date (step 12) which are not yet implemented since those object kinds
/// don't exist in this runtime.
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
            value.StringObject(_) -> "String"
            value.NumberObject(_) -> "Number"
            value.BooleanObject(_) -> "Boolean"
            value.SymbolObject(_) -> "Symbol"
            value.PidObject(_) -> "Pid"
            value.MapObject(..) -> "Map"
            value.SetObject(..) -> "Set"
            value.WeakMapObject(_) -> "WeakMap"
            value.WeakSetObject(_) -> "WeakSet"
            value.RegExpObject(..) -> "RegExp"
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
      case dict.get(symbol_properties, value.symbol_to_string_tag) {
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
pub fn object_value_of(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Step 1: Return ? ToObject(this value).
  // ToObject throws TypeError on null/undefined.
  case this {
    JsNull | JsUndefined -> frame.type_error(state, cannot_convert)
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
pub fn values(
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
pub fn entries(
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
  case first_arg(args) {
    JsObject(ref) as receiver -> {
      // §7.3.23 step 1: Let ownKeys be ? O.[[OwnPropertyKeys]]()
      // (filtered to enumerable-only string keys)
      let ks = collect_own_keys(state.heap, ref, True)
      // §7.3.23 step 3: For each key, Get(O, key) and collect
      case collect_values(state, ref, receiver, ks, []) {
        Ok(#(vals, state)) -> cont(list.reverse(vals), state)
        Error(#(thrown, state)) -> #(state, Error(thrown))
      }
    }
    // ToObject: null/undefined → TypeError
    JsNull | JsUndefined -> frame.type_error(state, cannot_convert)
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
      use #(val, state) <- result.try(object.get_value(state, ref, k, receiver))
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
  case first_arg(args) {
    JsObject(ref) as receiver -> {
      // §7.3.23 step 1: ownKeys (filtered to enumerable string keys)
      let ks = collect_own_keys(state.heap, ref, True)
      // §7.3.23 step 3: collect key+value pairs
      case collect_entries(state, ref, receiver, ks, []) {
        Ok(#(pairs, state)) -> cont(list.reverse(pairs), state)
        Error(#(thrown, state)) -> #(state, Error(thrown))
      }
    }
    // ToObject: null/undefined → TypeError
    JsNull | JsUndefined -> frame.type_error(state, cannot_convert)
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
      use #(val, state) <- result.try(object.get_value(state, ref, k, receiver))
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
pub fn create(
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
      frame.type_error(state, "Object prototype may only be an Object or null")
    Ok(prototype) -> {
      // Step 2: Let obj be OrdinaryObjectCreate(O).
      let #(heap, ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: OrdinaryObject,
            properties: dict.new(),
            symbol_properties: dict.new(),
            elements: js_elements.new(),
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
pub fn define_properties(
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
    _ -> frame.type_error(state, "Object.defineProperties called on non-object")
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
    JsNull | JsUndefined -> frame.type_error(state, cannot_convert)
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
      case object.get_own_property(state.heap, props_ref, key) {
        // Steps 4.b.i + 4.b.ii: descObj is an Object → ToPropertyDescriptor.
        Some(DataProperty(value: JsObject(desc_ref), ..)) ->
          // Step 5.a: DefinePropertyOrThrow(O, key, desc).
          case apply_descriptor(state, target_ref, key, desc_ref) {
            Ok(state) -> define_props_loop(state, target_ref, props_ref, rest)
            Error(#(thrown, state)) -> #(state, Error(thrown))
          }
        // Step 4.b.ii: ToPropertyDescriptor throws if descObj is not an Object.
        Some(_) ->
          frame.type_error(state, "Property description must be an object")
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
pub fn assign(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case args {
    // Step 1: ToObject throws TypeError on null/undefined.
    [] | [JsNull, ..] | [JsUndefined, ..] ->
      frame.type_error(state, cannot_convert)
    [target, ..sources] -> {
      // Step 1: Let to be ? ToObject(target).
      // For Objects: identity. For primitives: create a wrapper object.
      case common.to_object(state.heap, state.builtins, target) {
        None -> frame.type_error(state, cannot_convert)
        Some(#(heap, target_ref)) -> {
          let state = State(..state, heap:)
          // Steps 3-4: Process each source, then return to.
          case assign_sources(state, target_ref, sources) {
            Ok(state) -> #(state, Ok(JsObject(target_ref)))
            Error(#(thrown, state)) -> #(state, Error(thrown))
          }
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
        object.set_property(
          state.heap,
          target_ref,
          int.to_string(idx),
          JsString(ch),
        )
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
      // Step 2.a: Let propValue be ? Get(from, nextKey).
      use #(val, state) <- result.try(object.get_value(
        state,
        src_ref,
        k,
        receiver,
      ))
      // Step 2.b: Perform ? Set(to, nextKey, propValue, true).
      use #(state, _) <- result.try(object.set_value(
        state,
        target_ref,
        k,
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
fn collect_own_symbol_keys(
  heap: Heap,
  ref: Ref,
  enumerable_only: Bool,
) -> List(value.SymbolId) {
  case heap.read(heap, ref) {
    Some(ObjectSlot(symbol_properties:, ..)) ->
      dict.to_list(symbol_properties)
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
pub fn is(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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
/// TODO(Deviation): ToPropertyKey is approximated by ToString — symbol keys
/// are not yet handled as property keys.
pub fn has_own(
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
      // Step 2: Let key be ? ToPropertyKey(P).
      use key_str, state <- frame.try_to_string(state, key_val)
      // Step 3: Return ? HasOwnProperty(obj, key).
      let result = case object.get_own_property(state.heap, ref, key_str) {
        Some(_) -> JsBool(True)
        None -> JsBool(False)
      }
      #(state, Ok(result))
    }
    // Step 1: ToObject throws TypeError on null/undefined.
    JsNull | JsUndefined -> frame.type_error(state, cannot_convert)
    // String primitives: own keys are index chars + "length"
    JsString(s) -> {
      use key_str, state <- frame.try_to_string(state, key_val)
      let len = string.length(s)
      let result = case key_str == "length" {
        True -> JsBool(True)
        False ->
          case int.parse(key_str) {
            Ok(i) if i >= 0 && i < len -> JsBool(True)
            _ -> JsBool(False)
          }
      }
      #(state, Ok(result))
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
pub fn get_prototype_of(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case first_arg(args) {
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
    JsNull | JsUndefined -> frame.type_error(state, cannot_convert)
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
pub fn set_prototype_of(
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
    JsNull, _ | JsUndefined, _ -> frame.type_error(state, cannot_convert)
    _, Error(_) ->
      // §20.1.2.21 step 2: proto is not Object or null.
      frame.type_error(state, "Object prototype may only be an Object or null")
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
                  frame.type_error(
                    state,
                    "Cannot set prototype of a non-extensible object",
                  )
                True ->
                  // §10.1.2.1 step 7: cycle detection
                  case would_create_cycle(state.heap, ref, new_proto) {
                    // §20.1.2.21 step 5: status is false → throw TypeError.
                    True -> frame.type_error(state, "Cyclic __proto__ value")
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
fn would_create_cycle(
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

/// Object.freeze ( O ) — ES2024 §20.1.2.6
///
///   1. If O is not an Object, return O.
///   2. Let status be ? SetIntegrityLevel(O, frozen).
///   3. If status is false, throw a TypeError exception.
///   4. Return O.
///
/// SetIntegrityLevel ( O, frozen ) — §7.3.16:
///   1. Let status be ? O.[[PreventExtensions]]().
///   2. If status is false, return false.
///   3. Let keys be ? O.[[OwnPropertyKeys]]().
///   4. (sealed branch — skipped for frozen)
///   5. (frozen branch):
///   6. For each element k of keys, do
///      a. Let currentDesc be ? O.[[GetOwnProperty]](k).
///      b. If currentDesc is not undefined, then
///         i.  If IsAccessorDescriptor(currentDesc), let desc be {[[Configurable]]: false}.
///         ii. Else, let desc be {[[Configurable]]: false, [[Writable]]: false}.
///         iii. Perform ? DefinePropertyOrThrow(O, k, desc).
///   7. Return true.
///
/// NOTE: Elements (indexed properties in our dense array) are NOT frozen —
/// only named and symbol properties. This is a known simplification.
pub fn freeze(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let target = first_arg(args)
  case target {
    JsObject(ref) -> {
      // §20.1.2.6 step 2: SetIntegrityLevel(O, frozen)
      let heap = {
        use slot <- heap.update(state.heap, ref)
        case slot {
          ObjectSlot(properties:, symbol_properties:, ..) ->
            ObjectSlot(
              ..slot,
              // §7.3.16 step 6: freeze each own property descriptor
              properties: dict.map_values(properties, fn(_, p) {
                freeze_prop(p)
              }),
              symbol_properties: dict.map_values(symbol_properties, fn(_, p) {
                freeze_prop(p)
              }),
              // §7.3.16 step 1: O.[[PreventExtensions]]()
              extensible: False,
            )
          _ -> slot
        }
      }
      // §20.1.2.6 step 4: Return O.
      #(State(..state, heap:), Ok(target))
    }
    // §20.1.2.6 step 1: If O is not an Object, return O.
    _ -> #(state, Ok(target))
  }
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
pub fn prevent_extensions(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let target = first_arg(args)
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
fn all_frozen(props: dict.Dict(k, value.Property)) -> Bool {
  dict.values(props)
  |> list.all(fn(p) {
    case p {
      // §7.3.17 step 4.b.i + 4.b.ii: data prop must be non-writable AND non-configurable.
      DataProperty(writable: False, configurable: False, ..) -> True
      // §7.3.17 step 4.b.ii: accessor prop must be non-configurable.
      AccessorProperty(configurable: False, ..) -> True
      _ -> False
    }
  })
}

/// Object.isFrozen ( O ) — ES2024 §20.1.2.14
///
///   1. If O is not an Object, return true.
///   2. Return ? TestIntegrityLevel(O, frozen).
///
/// TestIntegrityLevel ( O, frozen ) — §7.3.17:
///   1. Let extensible be ? IsExtensible(O).
///   2. If extensible is true, return false.
///   3. Let keys be ? O.[[OwnPropertyKeys]]().
///   4. For each element k of keys, do
///      a. Let currentDesc be ? O.[[GetOwnProperty]](k).
///      b. If currentDesc is not undefined, then
///         i.  If IsDataDescriptor(currentDesc) is true, then
///             1. If currentDesc.[[Writable]] is true, return false.
///         ii. If currentDesc.[[Configurable]] is true, return false.
///   5. Return true.
///
/// TODO(Deviation): Elements (dense indexed properties) are not checked —
/// only named and symbol properties. Same simplification as freeze.
pub fn is_frozen(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let result = case first_arg(args) {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(properties:, symbol_properties:, extensible: False, ..)) ->
          // §7.3.17 step 2: extensible is false, proceed to step 4.
          // §7.3.17 step 4: check each own property descriptor.
          all_frozen(properties) && all_frozen(symbol_properties)
        // §7.3.17 step 2: extensible is true → return false.
        Some(ObjectSlot(extensible: True, ..)) -> False
        _ -> False
      }
    // §20.1.2.14 step 1: If O is not an Object, return true.
    _ -> True
  }
  #(state, Ok(JsBool(result)))
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
pub fn is_extensible(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let result = case first_arg(args) {
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
///   3. If status is false, throw a TypeError exception.
///   4. Return O.
///
/// SetIntegrityLevel ( O, sealed ) — §7.3.16:
///   1. Let status be ? O.[[PreventExtensions]]().
///   3. For each key in keys, set configurable=false on all own properties.
pub fn seal(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let target = first_arg(args)
  case target {
    JsObject(ref) -> {
      let heap = {
        use slot <- heap.update(state.heap, ref)
        case slot {
          ObjectSlot(properties:, symbol_properties:, ..) ->
            ObjectSlot(
              ..slot,
              properties: dict.map_values(properties, fn(_, p) { seal_prop(p) }),
              symbol_properties: dict.map_values(symbol_properties, fn(_, p) {
                seal_prop(p)
              }),
              extensible: False,
            )
          _ -> slot
        }
      }
      #(State(..state, heap:), Ok(target))
    }
    _ -> #(state, Ok(target))
  }
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
///
/// TestIntegrityLevel ( O, sealed ) — §7.3.17:
///   1. If extensible is true, return false.
///   4. For each property, if configurable is true, return false.
///   5. Return true.
pub fn is_sealed(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let result = case first_arg(args) {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(properties:, symbol_properties:, extensible: False, ..)) ->
          all_sealed(properties) && all_sealed(symbol_properties)
        Some(ObjectSlot(extensible: True, ..)) -> False
        _ -> False
      }
    _ -> True
  }
  #(state, Ok(JsBool(result)))
}

/// Check if all properties are non-configurable (sealed check).
fn all_sealed(props: dict.Dict(k, value.Property)) -> Bool {
  dict.values(props)
  |> list.all(fn(p) {
    case p {
      DataProperty(configurable: False, ..) -> True
      AccessorProperty(configurable: False, ..) -> True
      _ -> False
    }
  })
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
pub fn from_entries(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let target = first_arg(args)
  case target {
    // Step 1: RequireObjectCoercible — null/undefined throw TypeError.
    JsNull | JsUndefined -> frame.type_error(state, cannot_convert)
    JsObject(ref) -> {
      // Read the source array/iterable
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: ArrayObject(..), elements:, ..)) -> {
          // Iterate over array elements to build object properties.
          // Use a list accumulator to preserve insertion order.
          let entry_values = js_elements.values(elements)
          from_entries_loop(entry_values, state, [], dict.new())
        }
        _ -> frame.type_error(state, "Object.fromEntries requires an iterable")
      }
    }
    _ -> frame.type_error(state, "Object.fromEntries requires an iterable")
  }
}

/// Loop over iterable entries for Object.fromEntries.
///
/// `str_acc` is a list of #(String, Property) pairs in insertion order.
/// `sym_acc` is a dict of symbol-keyed properties.
/// We use a list for string keys to preserve insertion order (dict loses it).
fn from_entries_loop(
  entries: List(JsValue),
  state: State,
  str_acc: List(#(String, value.Property)),
  sym_acc: dict.Dict(value.SymbolId, value.Property),
) -> #(State, Result(JsValue, JsValue)) {
  case entries {
    [] -> {
      // Build the result object.
      // String properties are inserted in iteration order: convert list to dict.
      // Since later entries with the same key should overwrite earlier ones,
      // we fold left-to-right.
      let props =
        list.fold(str_acc, dict.new(), fn(d, pair) {
          dict.insert(d, pair.0, pair.1)
        })
      let #(heap, obj_ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: OrdinaryObject,
            properties: props,
            symbol_properties: sym_acc,
            elements: js_elements.new(),
            prototype: Some(state.builtins.object.prototype),
            extensible: True,
          ),
        )
      #(State(..state, heap:), Ok(JsObject(obj_ref)))
    }
    [entry, ..rest] -> {
      case entry {
        JsObject(entry_ref) -> {
          // Step b: k = Get(item, "0"), v = Get(item, "1")
          // Use object.get_value to invoke getters (handles accessor properties).
          case object.get_value(state, entry_ref, "0", entry) {
            Error(#(thrown, state)) -> #(state, Error(thrown))
            Ok(#(key_val, state)) ->
              case object.get_value(state, entry_ref, "1", entry) {
                Error(#(thrown, state)) -> #(state, Error(thrown))
                Ok(#(val, state)) ->
                  // Step c: ToPropertyKey(k) — symbol keys go to symbol_properties.
                  case key_val {
                    JsSymbol(sym) ->
                      from_entries_loop(
                        rest,
                        state,
                        str_acc,
                        dict.insert(sym_acc, sym, value.data_property(val)),
                      )
                    _ -> {
                      // ToPropertyKey via ToString for non-symbol keys.
                      use key_str, state <- frame.try_to_string(state, key_val)
                      from_entries_loop(
                        rest,
                        state,
                        list.append(str_acc, [
                          #(key_str, value.data_property(val)),
                        ]),
                        sym_acc,
                      )
                    }
                  }
              }
          }
        }
        _ -> frame.type_error(state, "Iterator value is not an entry object")
      }
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
pub fn get_own_property_descriptors(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let object_proto = state.builtins.object.prototype
  let target = first_arg(args)
  case target {
    JsNull | JsUndefined -> frame.type_error(state, cannot_convert)
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
                symbol_properties: dict.new(),
                elements: js_elements.new(),
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
pub fn get_own_property_symbols(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  case first_arg(args) {
    // Step 1: ToObject — null/undefined throw TypeError.
    JsNull | JsUndefined -> frame.type_error(state, cannot_convert)
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
