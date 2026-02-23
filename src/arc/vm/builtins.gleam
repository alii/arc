import arc/vm/builtins/array as builtins_array
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/error as builtins_error
import arc/vm/builtins/function as builtins_function
import arc/vm/builtins/math as builtins_math
import arc/vm/builtins/object as builtins_object
import arc/vm/builtins/string as builtins_string
import arc/vm/heap.{type Heap}
import arc/vm/value
import gleam/dict
import gleam/option.{None}

/// Pre-allocated prototype objects and constructor functions for JS built-ins.
/// All refs are rooted so GC never collects them.
pub type Builtins {
  Builtins(
    object: BuiltinType,
    function: BuiltinType,
    array: BuiltinType,
    error: BuiltinType,
    type_error: BuiltinType,
    reference_error: BuiltinType,
    range_error: BuiltinType,
    syntax_error: BuiltinType,
    math: value.Ref,
    string_proto: value.Ref,
  )
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
  let #(h, object_proto) = common.alloc_proto(h, None, dict.new())

  // Core types
  let #(h, function) = builtins_function.init(h, object_proto)
  let #(h, object) = builtins_object.init(h, object_proto, function.prototype)
  let #(h, array) = builtins_array.init(h, object_proto, function.prototype)

  // Error types
  let #(h, errors) = builtins_error.init(h, object_proto, function.prototype)

  // Math global object
  let #(h, math) = builtins_math.init(h, object_proto, function.prototype)

  // String.prototype (for primitive property access)
  let #(h, string_proto) =
    builtins_string.init(h, object_proto, function.prototype)

  #(
    h,
    Builtins(
      object:,
      function:,
      array:,
      error: errors.error,
      type_error: errors.type_error,
      reference_error: errors.reference_error,
      range_error: errors.range_error,
      syntax_error: errors.syntax_error,
      math:,
      string_proto:,
    ),
  )
}

/// Standard ECMAScript global bindings (NaN, Infinity, undefined, Object, etc.)
/// Maps global variable names to their JsValue representations.
pub fn globals(b: Builtins) -> dict.Dict(String, value.JsValue) {
  dict.from_list([
    #("NaN", value.JsNumber(value.NaN)),
    #("Infinity", value.JsNumber(value.Infinity)),
    #("undefined", value.JsUndefined),
    #("Object", value.JsObject(b.object.constructor)),
    #("Function", value.JsObject(b.function.constructor)),
    #("Array", value.JsObject(b.array.constructor)),
    #("Error", value.JsObject(b.error.constructor)),
    #("TypeError", value.JsObject(b.type_error.constructor)),
    #("ReferenceError", value.JsObject(b.reference_error.constructor)),
    #("RangeError", value.JsObject(b.range_error.constructor)),
    #("SyntaxError", value.JsObject(b.syntax_error.constructor)),
    #("Math", value.JsObject(b.math)),
  ])
}
