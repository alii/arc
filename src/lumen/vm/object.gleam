import gleam/dict
import gleam/option.{None, Some}
import lumen/vm/builtins.{type Builtins}
import lumen/vm/heap.{type Heap}
import lumen/vm/value.{type JsValue, type Ref, JsObject, JsString, ObjectSlot}

/// Walk the prototype chain to find a property by key.
/// Checks own properties first, then follows the prototype link.
/// Returns Error(Nil) if the property is not found anywhere in the chain.
pub fn get_property(heap: Heap, ref: Ref, key: String) -> Result(JsValue, Nil) {
  case heap.read(heap, ref) {
    Ok(ObjectSlot(properties:, prototype:)) ->
      case dict.get(properties, key) {
        Ok(val) -> Ok(val)
        Error(_) ->
          case prototype {
            Some(proto_ref) -> get_property(heap, proto_ref, key)
            None -> Error(Nil)
          }
      }
    // Not an ObjectSlot or dangling ref
    _ -> Error(Nil)
  }
}

/// Set an own property on an object (does NOT walk the prototype chain).
/// Returns the updated heap. No-op if ref doesn't point to an ObjectSlot.
pub fn set_property(heap: Heap, ref: Ref, key: String, val: JsValue) -> Heap {
  case heap.read(heap, ref) {
    Ok(ObjectSlot(properties:, prototype:)) -> {
      let new_props = dict.insert(properties, key, val)
      heap.write(heap, ref, ObjectSlot(properties: new_props, prototype:))
    }
    _ -> heap
  }
}

/// Create a TypeError instance on the heap and return it as a JsObject value.
pub fn make_type_error(
  h: Heap,
  b: Builtins,
  message: String,
) -> #(Heap, JsValue) {
  make_error(h, b.type_error_prototype, message)
}

/// Create a ReferenceError instance on the heap.
pub fn make_reference_error(
  h: Heap,
  b: Builtins,
  message: String,
) -> #(Heap, JsValue) {
  make_error(h, b.reference_error_prototype, message)
}

/// Create a RangeError instance on the heap.
pub fn make_range_error(
  h: Heap,
  b: Builtins,
  message: String,
) -> #(Heap, JsValue) {
  make_error(h, b.range_error_prototype, message)
}

/// Create a SyntaxError instance on the heap.
pub fn make_syntax_error(
  h: Heap,
  b: Builtins,
  message: String,
) -> #(Heap, JsValue) {
  make_error(h, b.syntax_error_prototype, message)
}

/// Internal helper — allocates an error object with the given prototype and message.
fn make_error(h: Heap, proto: Ref, message: String) -> #(Heap, JsValue) {
  let #(h, ref) =
    heap.alloc(
      h,
      ObjectSlot(
        prototype: Some(proto),
        properties: dict.from_list([#("message", JsString(message))]),
      ),
    )
  #(h, JsObject(ref))
}
