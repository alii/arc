import gleam/dict.{type Dict}
import gleam/option.{type Option, None, Some}
import lumen/vm/heap.{type Heap}
import lumen/vm/value.{
  type JsValue, type Ref, JsString, ObjectSlot, OrdinaryObject,
}

/// Pre-allocated prototype objects for the JS built-in hierarchy.
/// All refs are rooted so GC never collects them.
pub type Builtins {
  Builtins(
    object_prototype: Ref,
    function_prototype: Ref,
    array_prototype: Ref,
    error_prototype: Ref,
    type_error_prototype: Ref,
    reference_error_prototype: Ref,
    range_error_prototype: Ref,
    syntax_error_prototype: Ref,
  )
}

/// Allocate an ordinary prototype object on the heap, root it, and return
/// the updated heap + ref. Every builtin prototype follows this pattern.
fn alloc_proto(
  h: Heap,
  prototype: Option(Ref),
  properties: Dict(String, JsValue),
) -> #(Heap, Ref) {
  let #(h, ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: OrdinaryObject,
        properties:,
        elements: dict.new(),
        prototype:,
      ),
    )
  let h = heap.root(h, ref)
  #(h, ref)
}

/// Shorthand: alloc a prototype with just a "name" property.
fn alloc_named_proto(h: Heap, parent: Ref, name: String) -> #(Heap, Ref) {
  alloc_proto(h, Some(parent), dict.from_list([#("name", JsString(name))]))
}

/// Allocate and root all built-in prototype objects on the heap.
/// Must be called once before running any JS code.
///
/// Prototype chain:
///   Object.prototype         → None (end of chain)
///   Function.prototype       → Object.prototype
///   Array.prototype          → Object.prototype
///   Error.prototype          → Object.prototype
///   TypeError.prototype      → Error.prototype
///   ReferenceError.prototype → Error.prototype
///   RangeError.prototype     → Error.prototype
///   SyntaxError.prototype    → Error.prototype
pub fn init(h: Heap) -> #(Heap, Builtins) {
  // Object.prototype — the root of all prototype chains
  let #(h, object_proto) = alloc_proto(h, None, dict.new())

  // Core prototypes inheriting from Object.prototype
  let #(h, function_proto) = alloc_proto(h, Some(object_proto), dict.new())
  let #(h, array_proto) = alloc_proto(h, Some(object_proto), dict.new())

  // Error.prototype — has both "name" and "message"
  let #(h, error_proto) =
    alloc_proto(
      h,
      Some(object_proto),
      dict.from_list([
        #("name", JsString("Error")),
        #("message", JsString("")),
      ]),
    )

  // Error subclass prototypes — each just sets "name", inherits from Error.prototype
  let #(h, type_error_proto) = alloc_named_proto(h, error_proto, "TypeError")
  let #(h, reference_error_proto) =
    alloc_named_proto(h, error_proto, "ReferenceError")
  let #(h, range_error_proto) = alloc_named_proto(h, error_proto, "RangeError")
  let #(h, syntax_error_proto) =
    alloc_named_proto(h, error_proto, "SyntaxError")

  #(
    h,
    Builtins(
      object_prototype: object_proto,
      function_prototype: function_proto,
      array_prototype: array_proto,
      error_prototype: error_proto,
      type_error_prototype: type_error_proto,
      reference_error_prototype: reference_error_proto,
      range_error_prototype: range_error_proto,
      syntax_error_prototype: syntax_error_proto,
    ),
  )
}
