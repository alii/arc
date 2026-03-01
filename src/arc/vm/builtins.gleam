import arc/vm/builtins/arc as builtins_arc
import arc/vm/builtins/array as builtins_array
import arc/vm/builtins/boolean as builtins_boolean
import arc/vm/builtins/common.{type Builtins, Builtins}
import arc/vm/builtins/error as builtins_error
import arc/vm/builtins/function as builtins_function
import arc/vm/builtins/generator as builtins_generator
import arc/vm/builtins/json as builtins_json
import arc/vm/builtins/map as builtins_map
import arc/vm/builtins/math as builtins_math
import arc/vm/builtins/number as builtins_number
import arc/vm/builtins/object as builtins_object
import arc/vm/builtins/promise as builtins_promise
import arc/vm/builtins/set as builtins_set
import arc/vm/builtins/string as builtins_string
import arc/vm/builtins/symbol as builtins_symbol
import arc/vm/builtins/weak_map as builtins_weak_map
import arc/vm/builtins/weak_set as builtins_weak_set
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/value.{JsObject, JsUndefined, ObjectSlot, OrdinaryObject}
import gleam/dict
import gleam/list
import gleam/option.{None, Some}

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

  // String constructor + prototype
  let #(h, string) = builtins_string.init(h, object_proto, function.prototype)

  // Number constructor + prototype + global utility functions
  let #(h, number, parse_int, parse_float, is_nan, is_finite) =
    builtins_number.init(h, object_proto, function.prototype)

  // Boolean constructor + prototype
  let #(h, boolean) = builtins_boolean.init(h, object_proto, function.prototype)

  // Promise constructor + prototype
  let #(h, promise) = builtins_promise.init(h, object_proto, function.prototype)

  // %IteratorPrototype% — shared base for all iterators
  // Has [Symbol.iterator]() { return this; } so iterators are iterable
  let #(h, iterator_symbol_iterator) =
    common.alloc_native_fn(
      h,
      function.prototype,
      value.VmNative(value.IteratorSymbolIterator),
      "[Symbol.iterator]",
      0,
    )
  let #(h, iterator_proto) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: OrdinaryObject,
        properties: dict.new(),
        symbol_properties: dict.from_list([
          #(
            value.symbol_iterator,
            value.builtin_property(JsObject(iterator_symbol_iterator)),
          ),
        ]),
        elements: js_elements.new(),
        prototype: Some(object_proto),
        extensible: True,
      ),
    )
  let h = heap.root(h, iterator_proto)

  // Generator.prototype → %IteratorPrototype% → Object.prototype
  let #(h, generator) =
    builtins_generator.init(h, iterator_proto, function.prototype)

  // Symbol constructor (callable, not new-able)
  let #(h, symbol) = builtins_symbol.init(h, object_proto, function.prototype)

  // Arc global — engine-specific utilities (Arc.peek, etc.)
  let #(h, arc) = builtins_arc.init(h, object_proto, function.prototype)

  // JSON global object
  let #(h, json) = builtins_json.init(h, object_proto, function.prototype)

  // Map constructor + prototype
  let #(h, map) = builtins_map.init(h, object_proto, function.prototype)

  // Set constructor + prototype
  let #(h, set) = builtins_set.init(h, object_proto, function.prototype)

  // WeakMap constructor + prototype
  let #(h, weak_map) =
    builtins_weak_map.init(h, object_proto, function.prototype)

  // WeakSet constructor + prototype
  let #(h, weak_set) =
    builtins_weak_set.init(h, object_proto, function.prototype)

  // Global utility functions: eval, URI functions
  let #(h, eval) =
    common.alloc_native_fn(
      h,
      function.prototype,
      value.VmNative(value.Eval),
      "eval",
      1,
    )
  let #(h, decode_uri) =
    common.alloc_native_fn(
      h,
      function.prototype,
      value.VmNative(value.DecodeURI),
      "decodeURI",
      1,
    )
  let #(h, encode_uri) =
    common.alloc_native_fn(
      h,
      function.prototype,
      value.VmNative(value.EncodeURI),
      "encodeURI",
      1,
    )
  let #(h, decode_uri_component) =
    common.alloc_native_fn(
      h,
      function.prototype,
      value.VmNative(value.DecodeURIComponent),
      "decodeURIComponent",
      1,
    )
  let #(h, encode_uri_component) =
    common.alloc_native_fn(
      h,
      function.prototype,
      value.VmNative(value.EncodeURIComponent),
      "encodeURIComponent",
      1,
    )
  let #(h, escape) =
    common.alloc_native_fn(
      h,
      function.prototype,
      value.VmNative(value.Escape),
      "escape",
      1,
    )
  let #(h, unescape) =
    common.alloc_native_fn(
      h,
      function.prototype,
      value.VmNative(value.Unescape),
      "unescape",
      1,
    )

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
      eval_error: errors.eval_error,
      uri_error: errors.uri_error,
      aggregate_error: errors.aggregate_error,
      math:,
      string:,
      number:,
      boolean:,
      parse_int:,
      parse_float:,
      is_nan:,
      is_finite:,
      promise:,
      generator:,
      symbol:,
      arc:,
      json:,
      map:,
      set:,
      weak_map:,
      weak_set:,
      eval:,
      decode_uri:,
      encode_uri:,
      decode_uri_component:,
      encode_uri_component:,
      escape:,
      unescape:,
    ),
  )
}

/// A global entry: name, JsValue, and how to wrap it as a property descriptor.
/// The value is the canonical data; the wrapper derives the property flags.
type GlobalEntry {
  /// §19.1: NaN, Infinity, undefined — {writable: false, enumerable: false, configurable: false}
  Immutable(name: String, val: value.JsValue)
  /// Normal builtin — {writable: true, enumerable: false, configurable: true}
  Builtin(name: String, val: value.JsValue)
}


fn global_entry_to_property(entry: GlobalEntry) -> #(String, value.Property) {
  case entry {
    Immutable(name:, val:) -> #(name, value.data(val))
    Builtin(name:, val:) -> #(name, value.builtin_property(val))
  }
}

/// Build the globalThis object on the heap with all built-in bindings.
/// Returns updated heap + Ref to the globalThis heap object.
/// The globalThis object IS the ObjectRecord of the Global Environment Record.
pub fn globals(
  b: Builtins,
  h: Heap,
) -> #(Heap, value.Ref) {
  let entries = [
    // §19.1: these are {writable: false, enumerable: false, configurable: false}
    Immutable("NaN", value.JsNumber(value.NaN)),
    Immutable("Infinity", value.JsNumber(value.Infinity)),
    Immutable("undefined", JsUndefined),
    // Normal builtins
    Builtin("Object", JsObject(b.object.constructor)),
    Builtin("Function", JsObject(b.function.constructor)),
    Builtin("Array", JsObject(b.array.constructor)),
    Builtin("Error", JsObject(b.error.constructor)),
    Builtin("TypeError", JsObject(b.type_error.constructor)),
    Builtin("ReferenceError", JsObject(b.reference_error.constructor)),
    Builtin("RangeError", JsObject(b.range_error.constructor)),
    Builtin("SyntaxError", JsObject(b.syntax_error.constructor)),
    Builtin("EvalError", JsObject(b.eval_error.constructor)),
    Builtin("URIError", JsObject(b.uri_error.constructor)),
    Builtin("AggregateError", JsObject(b.aggregate_error.constructor)),
    Builtin("Math", JsObject(b.math)),
    Builtin("String", JsObject(b.string.constructor)),
    Builtin("Number", JsObject(b.number.constructor)),
    Builtin("Boolean", JsObject(b.boolean.constructor)),
    Builtin("parseInt", JsObject(b.parse_int)),
    Builtin("parseFloat", JsObject(b.parse_float)),
    Builtin("isNaN", JsObject(b.is_nan)),
    Builtin("isFinite", JsObject(b.is_finite)),
    Builtin("Promise", JsObject(b.promise.constructor)),
    Builtin("Symbol", JsObject(b.symbol)),
    Builtin("Arc", JsObject(b.arc)),
    Builtin("JSON", JsObject(b.json)),
    Builtin("Map", JsObject(b.map.constructor)),
    Builtin("Set", JsObject(b.set.constructor)),
    Builtin("WeakMap", JsObject(b.weak_map.constructor)),
    Builtin("WeakSet", JsObject(b.weak_set.constructor)),
    Builtin("eval", JsObject(b.eval)),
    Builtin("decodeURI", JsObject(b.decode_uri)),
    Builtin("encodeURI", JsObject(b.encode_uri)),
    Builtin("decodeURIComponent", JsObject(b.decode_uri_component)),
    Builtin("encodeURIComponent", JsObject(b.encode_uri_component)),
    Builtin("escape", JsObject(b.escape)),
    Builtin("unescape", JsObject(b.unescape)),
  ]

  // globalThis heap object — property descriptors for JS-visible reflection
  let properties =
    list.map(entries, global_entry_to_property) |> dict.from_list()
  let #(h, global_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: OrdinaryObject,
        properties:,
        symbol_properties: dict.new(),
        elements: js_elements.new(),
        prototype: Some(b.object.prototype),
        extensible: True,
      ),
    )
  let h = heap.root(h, global_ref)

  // Add globalThis self-reference property
  let h =
    heap.update(h, global_ref, fn(slot) {
      case slot {
        ObjectSlot(properties: props, ..) ->
          ObjectSlot(
            ..slot,
            properties: dict.insert(
              props,
              "globalThis",
              value.builtin_property(JsObject(global_ref)),
            ),
          )
        _ -> slot
      }
    })

  #(h, global_ref)
}
