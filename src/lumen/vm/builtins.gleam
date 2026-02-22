import gleam/dict
import gleam/option.{None, Some}
import lumen/vm/heap.{type Heap}
import lumen/vm/value.{type Ref, JsString, ObjectSlot}

/// Pre-allocated prototype objects for the JS built-in hierarchy.
/// All refs are rooted so GC never collects them.
pub type Builtins {
  Builtins(
    object_prototype: Ref,
    error_prototype: Ref,
    type_error_prototype: Ref,
    reference_error_prototype: Ref,
    range_error_prototype: Ref,
    syntax_error_prototype: Ref,
  )
}

/// Allocate and root all built-in prototype objects on the heap.
/// Must be called once before running any JS code.
///
/// Prototype chain:
///   Object.prototype        (prototype: None — end of chain)
///   Error.prototype         (prototype: Some(object_prototype))
///   TypeError.prototype     (prototype: Some(error_prototype))
///   ReferenceError.prototype(prototype: Some(error_prototype))
///   RangeError.prototype    (prototype: Some(error_prototype))
///   SyntaxError.prototype   (prototype: Some(error_prototype))
pub fn init(h: Heap) -> #(Heap, Builtins) {
  // Object.prototype — the root of all prototype chains
  let #(h, object_proto) =
    heap.alloc(h, ObjectSlot(properties: dict.new(), prototype: None))
  let h = heap.root(h, object_proto)

  // Error.prototype
  let #(h, error_proto) =
    heap.alloc(
      h,
      ObjectSlot(
        prototype: Some(object_proto),
        properties: dict.from_list([
          #("name", JsString("Error")),
          #("message", JsString("")),
        ]),
      ),
    )
  let h = heap.root(h, error_proto)

  // TypeError.prototype
  let #(h, type_error_proto) =
    heap.alloc(
      h,
      ObjectSlot(
        prototype: Some(error_proto),
        properties: dict.from_list([#("name", JsString("TypeError"))]),
      ),
    )
  let h = heap.root(h, type_error_proto)

  // ReferenceError.prototype
  let #(h, reference_error_proto) =
    heap.alloc(
      h,
      ObjectSlot(
        prototype: Some(error_proto),
        properties: dict.from_list([#("name", JsString("ReferenceError"))]),
      ),
    )
  let h = heap.root(h, reference_error_proto)

  // RangeError.prototype
  let #(h, range_error_proto) =
    heap.alloc(
      h,
      ObjectSlot(
        prototype: Some(error_proto),
        properties: dict.from_list([#("name", JsString("RangeError"))]),
      ),
    )
  let h = heap.root(h, range_error_proto)

  // SyntaxError.prototype
  let #(h, syntax_error_proto) =
    heap.alloc(
      h,
      ObjectSlot(
        prototype: Some(error_proto),
        properties: dict.from_list([#("name", JsString("SyntaxError"))]),
      ),
    )
  let h = heap.root(h, syntax_error_proto)

  #(
    h,
    Builtins(
      object_prototype: object_proto,
      error_prototype: error_proto,
      type_error_prototype: type_error_proto,
      reference_error_prototype: reference_error_proto,
      range_error_prototype: range_error_proto,
      syntax_error_prototype: syntax_error_proto,
    ),
  )
}
