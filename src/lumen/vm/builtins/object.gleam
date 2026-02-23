import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{Some}
import lumen/vm/builtins/common.{type BuiltinType, BuiltinType, set_constructor}
import lumen/vm/heap.{type Heap}
import lumen/vm/value.{
  type JsValue, type Ref, ArrayObject, DataProperty, JsBool, JsNull, JsNumber,
  JsObject, JsString, JsUndefined, NativeFunction, NativeObjectConstructor,
  NativeObjectDefineProperty, NativeObjectGetOwnPropertyDescriptor,
  NativeObjectGetOwnPropertyNames, NativeObjectKeys,
  NativeObjectPrototypeHasOwnProperty, NativeObjectPrototypePropertyIsEnumerable,
  ObjectSlot, OrdinaryObject,
}

/// Set up Object constructor and Object.prototype methods.
/// Object.prototype is already allocated (it's the root of all chains).
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, BuiltinType) {
  let #(h, ctor_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(NativeObjectConstructor),
        properties: dict.from_list([
          #("prototype", value.builtin_property(JsObject(object_proto))),
          #("name", value.builtin_property(JsString("Object"))),
        ]),
        elements: dict.new(),
        prototype: Some(function_proto),
      ),
    )
  let h = heap.root(h, ctor_ref)
  let h = set_constructor(h, object_proto, ctor_ref)

  // Object.getOwnPropertyDescriptor — static method
  let #(h, gopd_ref) =
    alloc_native_fn(
      h,
      function_proto,
      NativeObjectGetOwnPropertyDescriptor,
      "getOwnPropertyDescriptor",
      2,
    )
  let h = add_method(h, ctor_ref, "getOwnPropertyDescriptor", gopd_ref)

  // Object.defineProperty — static method
  let #(h, dp_ref) =
    alloc_native_fn(
      h,
      function_proto,
      NativeObjectDefineProperty,
      "defineProperty",
      3,
    )
  let h = add_method(h, ctor_ref, "defineProperty", dp_ref)

  // Object.getOwnPropertyNames — static method
  let #(h, gopn_ref) =
    alloc_native_fn(
      h,
      function_proto,
      NativeObjectGetOwnPropertyNames,
      "getOwnPropertyNames",
      1,
    )
  let h = add_method(h, ctor_ref, "getOwnPropertyNames", gopn_ref)

  // Object.keys — static method
  let #(h, keys_ref) =
    alloc_native_fn(h, function_proto, NativeObjectKeys, "keys", 1)
  let h = add_method(h, ctor_ref, "keys", keys_ref)

  // Object.prototype.hasOwnProperty — instance method on prototype
  let #(h, hop_ref) =
    alloc_native_fn(
      h,
      function_proto,
      NativeObjectPrototypeHasOwnProperty,
      "hasOwnProperty",
      1,
    )
  let h = add_method(h, object_proto, "hasOwnProperty", hop_ref)

  // Object.prototype.propertyIsEnumerable — instance method on prototype
  let #(h, pie_ref) =
    alloc_native_fn(
      h,
      function_proto,
      NativeObjectPrototypePropertyIsEnumerable,
      "propertyIsEnumerable",
      1,
    )
  let h = add_method(h, object_proto, "propertyIsEnumerable", pie_ref)

  #(h, BuiltinType(prototype: object_proto, constructor: ctor_ref))
}

/// Allocate a native function object on the heap, root it, and return the ref.
fn alloc_native_fn(
  h: Heap,
  function_proto: Ref,
  native: value.NativeFn,
  name: String,
  length: Int,
) -> #(Heap, Ref) {
  let #(h, ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(native),
        properties: dict.from_list([
          #("name", value.builtin_property(JsString(name))),
          #(
            "length",
            value.builtin_property(JsNumber(value.Finite(int.to_float(length)))),
          ),
        ]),
        elements: dict.new(),
        prototype: Some(function_proto),
      ),
    )
  let h = heap.root(h, ref)
  #(h, ref)
}

/// Add a non-enumerable method property to an object on the heap.
fn add_method(h: Heap, obj_ref: Ref, name: String, fn_ref: Ref) -> Heap {
  case heap.read(h, obj_ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, prototype:)) -> {
      let new_props =
        dict.insert(properties, name, value.builtin_property(JsObject(fn_ref)))
      heap.write(
        h,
        obj_ref,
        ObjectSlot(kind:, properties: new_props, elements:, prototype:),
      )
    }
    _ -> h
  }
}

/// Object() / new Object() — creates a plain empty object,
/// or returns the argument if it's already an object.
pub fn call_native(
  args: List(JsValue),
  _this: JsValue,
  heap: Heap,
  object_proto: Ref,
) -> #(Heap, Result(JsValue, JsValue)) {
  case args {
    [JsObject(_) as obj, ..] -> #(heap, Ok(obj))
    _ -> {
      let #(heap, ref) =
        heap.alloc(
          heap,
          ObjectSlot(
            kind: OrdinaryObject,
            properties: dict.new(),
            elements: dict.new(),
            prototype: Some(object_proto),
          ),
        )
      #(heap, Ok(JsObject(ref)))
    }
  }
}

/// Object.getOwnPropertyDescriptor(obj, key)
/// Returns a descriptor object {value, writable, enumerable, configurable} or undefined.
pub fn get_own_property_descriptor(
  args: List(JsValue),
  heap: Heap,
  object_proto: Ref,
) -> #(Heap, Result(JsValue, JsValue)) {
  case args {
    [JsObject(ref), key_val, ..] -> {
      let key_str = value.to_js_string(key_val)
      case get_own_property(heap, ref, key_str) {
        Ok(prop) -> {
          let #(heap, desc_ref) =
            make_descriptor_object(heap, prop, object_proto)
          #(heap, Ok(JsObject(desc_ref)))
        }
        Error(_) -> #(heap, Ok(JsUndefined))
      }
    }
    [JsObject(ref)] -> {
      // Missing key arg — key is undefined, convert to string "undefined"
      case get_own_property(heap, ref, "undefined") {
        Ok(prop) -> {
          let #(heap, desc_ref) =
            make_descriptor_object(heap, prop, object_proto)
          #(heap, Ok(JsObject(desc_ref)))
        }
        Error(_) -> #(heap, Ok(JsUndefined))
      }
    }
    _ -> #(heap, Ok(JsUndefined))
  }
}

/// Look up an OWN property (NOT prototype chain) by string key.
/// Handles arrays (numeric indices, "length") and regular objects.
fn get_own_property(
  heap: Heap,
  ref: Ref,
  key: String,
) -> Result(value.Property, Nil) {
  case heap.read(heap, ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, ..)) ->
      case kind {
        ArrayObject(length:) ->
          case key {
            "length" ->
              Ok(DataProperty(
                value: JsNumber(value.Finite(int.to_float(length))),
                writable: True,
                enumerable: False,
                configurable: False,
              ))
            _ ->
              case int.parse(key) {
                Ok(idx) ->
                  case dict.get(elements, idx) {
                    Ok(val) ->
                      Ok(DataProperty(
                        value: val,
                        writable: True,
                        enumerable: True,
                        configurable: True,
                      ))
                    Error(_) -> Error(Nil)
                  }
                Error(_) -> dict.get(properties, key)
              }
          }
        _ -> dict.get(properties, key)
      }
    _ -> Error(Nil)
  }
}

/// Allocate a property descriptor object: {value, writable, enumerable, configurable}
fn make_descriptor_object(
  heap: Heap,
  prop: value.Property,
  object_proto: Ref,
) -> #(Heap, Ref) {
  case prop {
    DataProperty(value: val, writable:, enumerable:, configurable:) ->
      heap.alloc(
        heap,
        ObjectSlot(
          kind: OrdinaryObject,
          properties: dict.from_list([
            #("value", value.data_property(val)),
            #("writable", value.data_property(JsBool(writable))),
            #("enumerable", value.data_property(JsBool(enumerable))),
            #("configurable", value.data_property(JsBool(configurable))),
          ]),
          elements: dict.new(),
          prototype: Some(object_proto),
        ),
      )
  }
}

/// Object.defineProperty(obj, key, descriptor)
/// Defines/modifies a property on an object from a descriptor.
/// Returns the target object.
pub fn define_property(
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  case args {
    [JsObject(ref) as obj, key_val, JsObject(desc_ref), ..] -> {
      let key_str = value.to_js_string(key_val)
      let heap = apply_descriptor(heap, ref, key_str, desc_ref)
      #(heap, Ok(obj))
    }
    [JsObject(_) as obj, _, _, ..] ->
      // descriptor is not an object — per spec should throw TypeError
      // but many engines are lenient; we'll just return obj unchanged
      #(heap, Ok(obj))
    _ -> #(heap, Ok(JsUndefined))
  }
}

/// Read the descriptor object's fields and apply to the target property.
fn apply_descriptor(
  heap: Heap,
  target_ref: Ref,
  key: String,
  desc_ref: Ref,
) -> Heap {
  // Read descriptor fields
  let desc_value = read_desc_field(heap, desc_ref, "value")
  let desc_writable = read_desc_bool(heap, desc_ref, "writable")
  let desc_enumerable = read_desc_bool(heap, desc_ref, "enumerable")
  let desc_configurable = read_desc_bool(heap, desc_ref, "configurable")

  case heap.read(heap, target_ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, prototype:)) -> {
      // Get existing property to merge with
      let existing = dict.get(properties, key)

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
            Ok(DataProperty(enumerable: e, ..)) -> e
            _ -> False
          }
      }
      let final_configurable = case desc_configurable {
        Some(c) -> c
        _ ->
          case existing {
            Ok(DataProperty(configurable: c, ..)) -> c
            _ -> False
          }
      }

      let new_prop =
        DataProperty(
          value: final_value,
          writable: final_writable,
          enumerable: final_enumerable,
          configurable: final_configurable,
        )
      let new_props = dict.insert(properties, key, new_prop)
      heap.write(
        heap,
        target_ref,
        ObjectSlot(kind:, properties: new_props, elements:, prototype:),
      )
    }
    _ -> heap
  }
}

/// Read a field from a descriptor object, returning Some(value) or None.
fn read_desc_field(
  heap: Heap,
  desc_ref: Ref,
  key: String,
) -> option.Option(JsValue) {
  case heap.read(heap, desc_ref) {
    Ok(ObjectSlot(properties:, ..)) ->
      case dict.get(properties, key) {
        Ok(DataProperty(value: val, ..)) -> Some(val)
        _ -> option.None
      }
    _ -> option.None
  }
}

/// Read a boolean field from a descriptor object.
fn read_desc_bool(heap: Heap, desc_ref: Ref, key: String) -> option.Option(Bool) {
  case read_desc_field(heap, desc_ref, key) {
    Some(JsBool(b)) -> Some(b)
    Some(JsUndefined) -> Some(False)
    Some(JsNull) -> Some(False)
    Some(JsNumber(value.Finite(0.0))) -> Some(False)
    Some(JsNumber(value.NaN)) -> Some(False)
    Some(JsString("")) -> Some(False)
    Some(_) -> Some(True)
    option.None -> option.None
  }
}

/// Object.getOwnPropertyNames(obj) — returns array of ALL own string keys
/// (both enumerable and non-enumerable).
pub fn get_own_property_names(
  args: List(JsValue),
  heap: Heap,
  array_proto: Ref,
) -> #(Heap, Result(JsValue, JsValue)) {
  case args {
    [JsObject(ref), ..] -> {
      let keys = collect_own_keys(heap, ref, False)
      let #(heap, arr_ref) = make_string_array(heap, keys, array_proto)
      #(heap, Ok(JsObject(arr_ref)))
    }
    _ -> #(heap, Ok(JsUndefined))
  }
}

/// Object.keys(obj) — returns array of enumerable own string keys.
pub fn keys(
  args: List(JsValue),
  heap: Heap,
  array_proto: Ref,
) -> #(Heap, Result(JsValue, JsValue)) {
  case args {
    [JsObject(ref), ..] -> {
      let ks = collect_own_keys(heap, ref, True)
      let #(heap, arr_ref) = make_string_array(heap, ks, array_proto)
      #(heap, Ok(JsObject(arr_ref)))
    }
    _ -> #(heap, Ok(JsUndefined))
  }
}

/// Collect own property keys from an object.
/// If enumerable_only is True, only includes enumerable keys.
fn collect_own_keys(heap: Heap, ref: Ref, enumerable_only: Bool) -> List(String) {
  case heap.read(heap, ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, ..)) -> {
      // For arrays: collect numeric index keys first, then "length", then string properties
      let index_keys = case kind {
        ArrayObject(length:) -> collect_index_keys(elements, 0, length, [])
        _ -> []
      }
      // String property keys
      let prop_keys =
        dict.to_list(properties)
        |> list.filter_map(fn(pair) {
          let #(key, prop) = pair
          case enumerable_only {
            True ->
              case prop {
                DataProperty(enumerable: True, ..) -> Ok(key)
                _ -> Error(Nil)
              }
            False -> Ok(key)
          }
        })
      // For arrays: "length" is non-enumerable, include in getOwnPropertyNames but not keys
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
fn collect_index_keys(
  elements: dict.Dict(Int, JsValue),
  idx: Int,
  length: Int,
  acc: List(String),
) -> List(String) {
  case idx >= length {
    True -> list.reverse(acc)
    False ->
      case dict.has_key(elements, idx) {
        True ->
          collect_index_keys(elements, idx + 1, length, [
            int.to_string(idx),
            ..acc
          ])
        False -> collect_index_keys(elements, idx + 1, length, acc)
      }
  }
}

/// Allocate a JS array from a list of strings.
fn make_string_array(
  heap: Heap,
  keys: List(String),
  array_proto: Ref,
) -> #(Heap, Ref) {
  let count = list.length(keys)
  let elems =
    keys
    |> list.index_map(fn(key, idx) { #(idx, JsString(key)) })
    |> dict.from_list()
  heap.alloc(
    heap,
    ObjectSlot(
      kind: ArrayObject(count),
      properties: dict.new(),
      elements: elems,
      prototype: Some(array_proto),
    ),
  )
}

/// Object.prototype.hasOwnProperty(key)
/// Checks if the object has an own property with the given key (NOT prototype chain).
pub fn has_own_property(
  this: JsValue,
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) -> {
      let key_str = case args {
        [key_val, ..] -> value.to_js_string(key_val)
        [] -> "undefined"
      }
      let result = case get_own_property(heap, ref, key_str) {
        Ok(_) -> JsBool(True)
        Error(_) -> JsBool(False)
      }
      #(heap, Ok(result))
    }
    _ -> #(heap, Ok(JsBool(False)))
  }
}

/// Object.prototype.propertyIsEnumerable(key)
/// Returns true if the object has an own enumerable property with the given key.
pub fn property_is_enumerable(
  this: JsValue,
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) -> {
      let key_str = case args {
        [key_val, ..] -> value.to_js_string(key_val)
        [] -> "undefined"
      }
      let result = case get_own_property(heap, ref, key_str) {
        Ok(DataProperty(enumerable: True, ..)) -> JsBool(True)
        _ -> JsBool(False)
      }
      #(heap, Ok(result))
    }
    _ -> #(heap, Ok(JsBool(False)))
  }
}
