import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/string
import lumen/vm/builtins/common.{
  type BuiltinType, BuiltinType, alloc_proto, set_constructor,
}
import lumen/vm/heap.{type Heap}
import lumen/vm/value.{
  type JsValue, type Ref, ArrayObject, JsBool, JsNull, JsNumber, JsObject,
  JsString, JsUndefined, NativeArrayConstructor, NativeArrayIsArray,
  NativeArrayPrototypeJoin, NativeArrayPrototypePush, NativeFunction, ObjectSlot,
}

/// Set up Array.prototype and Array constructor.
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, BuiltinType) {
  // Array.prototype — inherits from Object.prototype
  let #(h, array_proto) = alloc_proto(h, Some(object_proto), dict.new())

  // Array constructor — a NativeFunction
  let #(h, ctor_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(NativeArrayConstructor),
        properties: dict.from_list([
          #("prototype", value.builtin_property(JsObject(array_proto))),
          #("name", value.builtin_property(JsString("Array"))),
        ]),
        elements: dict.new(),
        prototype: Some(function_proto),
      ),
    )
  let h = heap.root(h, ctor_ref)

  // Array.isArray — static method on the constructor
  let #(h, is_array_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(NativeArrayIsArray),
        properties: dict.from_list([
          #("name", value.builtin_property(JsString("isArray"))),
        ]),
        elements: dict.new(),
        prototype: Some(function_proto),
      ),
    )
  let h = heap.root(h, is_array_ref)

  // Set Array.isArray on constructor (NOT enumerable)
  let h = case heap.read(h, ctor_ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, prototype:)) -> {
      let new_props =
        dict.insert(
          properties,
          "isArray",
          value.builtin_property(JsObject(is_array_ref)),
        )
      heap.write(
        h,
        ctor_ref,
        ObjectSlot(kind:, properties: new_props, elements:, prototype:),
      )
    }
    _ -> h
  }

  let h = set_constructor(h, array_proto, ctor_ref)

  // Array.prototype.join — instance method
  let #(h, join_ref) =
    alloc_native_fn(h, function_proto, NativeArrayPrototypeJoin, "join", 1)
  let h = add_method(h, array_proto, "join", join_ref)

  // Array.prototype.push — instance method
  let #(h, push_ref) =
    alloc_native_fn(h, function_proto, NativeArrayPrototypePush, "push", 1)
  let h = add_method(h, array_proto, "push", push_ref)

  #(h, BuiltinType(prototype: array_proto, constructor: ctor_ref))
}

/// Array() / new Array() — construct a new array.
pub fn construct(
  args: List(JsValue),
  heap: Heap,
  array_proto: Ref,
) -> #(Heap, Result(JsValue, JsValue)) {
  native_array_constructor(args, heap, array_proto)
}

/// Array.isArray(value) — check if a value is an array.
pub fn is_array(
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  #(heap, Ok(native_is_array(args, heap)))
}

/// Array() / new Array() constructor.
/// - No args: empty array
/// - One numeric arg: array with that length (empty elements)
/// - Otherwise: array from the given elements
fn native_array_constructor(
  args: List(JsValue),
  heap: Heap,
  array_proto: Ref,
) -> #(Heap, Result(JsValue, JsValue)) {
  let #(length, elements) = case args {
    [] -> #(0, dict.new())
    [JsNumber(value.Finite(n))] -> {
      let len = float_to_int(n)
      case len >= 0 && int.to_float(len) == n {
        True -> #(len, dict.new())
        // Non-integer or negative: treat as single element
        False -> #(1, dict.from_list([#(0, JsNumber(value.Finite(n)))]))
      }
    }
    _ -> {
      let count = list.length(args)
      let elems =
        args
        |> list.index_map(fn(val, idx) { #(idx, val) })
        |> dict.from_list()
      #(count, elems)
    }
  }
  let #(heap, ref) =
    heap.alloc(
      heap,
      ObjectSlot(
        kind: ArrayObject(length),
        properties: dict.new(),
        elements:,
        prototype: Some(array_proto),
      ),
    )
  #(heap, Ok(JsObject(ref)))
}

/// Array.isArray(value) — returns true if value is an ArrayObject on the heap.
fn native_is_array(args: List(JsValue), heap: Heap) -> JsValue {
  case args {
    [JsObject(ref), ..] ->
      case heap.read(heap, ref) {
        Ok(ObjectSlot(kind: ArrayObject(_), ..)) -> JsBool(True)
        _ -> JsBool(False)
      }
    _ -> JsBool(False)
  }
}

fn float_to_int(f: Float) -> Int {
  case f <. 0.0 {
    True -> 0 - float.truncate(float.negate(f))
    False -> float.truncate(f)
  }
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

/// Array.prototype.join(separator)
/// Joins array elements into a string with the given separator (default ",").
pub fn array_join(
  this: JsValue,
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) ->
      case heap.read(heap, ref) {
        Ok(ObjectSlot(kind: ArrayObject(length:), elements:, ..)) -> {
          let separator = case args {
            [JsUndefined, ..] -> ","
            [sep_val, ..] -> value.to_js_string(sep_val)
            [] -> ","
          }
          let result = join_elements(elements, 0, length, separator, [])
          #(heap, Ok(JsString(result)))
        }
        _ -> #(heap, Ok(JsString("")))
      }
    _ -> #(heap, Ok(JsString("")))
  }
}

/// Iterate elements 0..length-1, converting each to string.
/// undefined/null → empty string per spec.
fn join_elements(
  elements: dict.Dict(Int, JsValue),
  idx: Int,
  length: Int,
  separator: String,
  acc: List(String),
) -> String {
  case idx >= length {
    True -> acc |> list.reverse |> string.join(separator)
    False -> {
      let str = case dict.get(elements, idx) {
        Ok(JsUndefined) | Ok(JsNull) | Error(_) -> ""
        Ok(val) -> value.to_js_string(val)
      }
      join_elements(elements, idx + 1, length, separator, [str, ..acc])
    }
  }
}

/// Array.prototype.push(...items)
/// Appends items to the array and returns the new length.
pub fn array_push(
  this: JsValue,
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) ->
      case heap.read(heap, ref) {
        Ok(ObjectSlot(
          kind: ArrayObject(length:),
          properties:,
          elements:,
          prototype:,
        )) -> {
          let #(new_elements, new_length) =
            push_elements(elements, args, length)
          let heap =
            heap.write(
              heap,
              ref,
              ObjectSlot(
                kind: ArrayObject(new_length),
                properties:,
                elements: new_elements,
                prototype:,
              ),
            )
          #(heap, Ok(JsNumber(value.Finite(int.to_float(new_length)))))
        }
        _ -> #(heap, Ok(JsNumber(value.Finite(0.0))))
      }
    _ -> #(heap, Ok(JsNumber(value.Finite(0.0))))
  }
}

/// Insert args into elements dict starting at current length.
fn push_elements(
  elements: dict.Dict(Int, JsValue),
  args: List(JsValue),
  start: Int,
) -> #(dict.Dict(Int, JsValue), Int) {
  case args {
    [] -> #(elements, start)
    [val, ..rest] ->
      push_elements(dict.insert(elements, start, val), rest, start + 1)
  }
}
