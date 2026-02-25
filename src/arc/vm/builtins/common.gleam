import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/value.{
  type ExoticKind, type JsValue, type NativeFn, type Property, type Ref,
  ArrayObject, JsObject, JsString, NativeFunction, ObjectSlot, OrdinaryObject,
}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

/// A prototype + constructor pair. Every JS builtin type has both.
pub type BuiltinType {
  BuiltinType(prototype: Ref, constructor: Ref)
}

/// Generator.prototype ref, used as the prototype for generator objects.
/// Generators don't have a user-visible constructor.
pub type GeneratorBuiltin {
  GeneratorBuiltin(prototype: Ref)
}

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
    math: Ref,
    string: BuiltinType,
    number: BuiltinType,
    boolean: BuiltinType,
    parse_int: Ref,
    parse_float: Ref,
    is_nan: Ref,
    is_finite: Ref,
    promise: BuiltinType,
    generator: GeneratorBuiltin,
    symbol: Ref,
    arc: Ref,
  )
}

/// Allocate an ordinary prototype object on the heap, root it, and return
/// the updated heap + ref. Shared bootstrap helper for all builtin modules.
///
/// Not a spec operation — internal helper for builtin initialization.
/// Creates the prototype object that will be used as [[Prototype]] for
/// instances of a builtin type.
pub fn alloc_proto(
  h: Heap,
  prototype: Option(Ref),
  properties: Dict(String, Property),
) -> #(Heap, Ref) {
  let #(h, ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: OrdinaryObject,
        properties:,
        elements: js_elements.new(),
        prototype:,
        symbol_properties: dict.new(),
        extensible: True,
      ),
    )
  let h = heap.root(h, ref)
  #(h, ref)
}

/// Allocate a NativeFunction ObjectSlot with standard name/length properties.
///
/// Not a spec operation — internal helper for builtin initialization.
/// Creates a function object with the correct .name and .length data
/// properties per §20.2.3 (Function instances).
pub fn alloc_native_fn(
  h: Heap,
  function_proto: Ref,
  native: NativeFn,
  name: String,
  arity: Int,
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
            value.builtin_property(
              value.JsNumber(value.Finite(int.to_float(arity))),
            ),
          ),
        ]),
        symbol_properties: dict.new(),
        elements: js_elements.new(),
        prototype: Some(function_proto),
        extensible: True,
      ),
    )
  let h = heap.root(h, ref)
  #(h, ref)
}

/// Allocate N native function objects from specs, returning builtin_property
/// entries. Replaces the identical fold duplicated across builtin modules.
///
/// Not a spec operation — internal helper that batch-allocates method
/// function objects for a prototype's property list.
pub fn alloc_methods(
  h: Heap,
  function_proto: Ref,
  specs: List(#(String, NativeFn, Int)),
) -> #(Heap, List(#(String, Property))) {
  list.fold(specs, #(h, []), fn(acc, spec) {
    let #(h, props) = acc
    let #(name, native, arity) = spec
    let #(h, fn_ref) = alloc_native_fn(h, function_proto, native, name, arity)
    #(h, [#(name, value.builtin_property(JsObject(fn_ref))), ..props])
  })
}

/// Build the standard ctor properties list: prototype + name + length + extras.
fn ctor_properties(
  proto: Ref,
  name: String,
  arity: Int,
  extras: List(#(String, Property)),
) -> List(#(String, Property)) {
  [
    #("prototype", value.builtin_property(JsObject(proto))),
    #("name", value.builtin_property(JsString(name))),
    #(
      "length",
      value.builtin_property(value.JsNumber(value.Finite(int.to_float(arity)))),
    ),
    ..extras
  ]
}

/// Build the standard proto properties list: constructor + extras.
fn proto_properties(
  ctor_ref: Ref,
  extras: List(#(String, Property)),
) -> List(#(String, Property)) {
  [#("constructor", value.builtin_property(JsObject(ctor_ref))), ..extras]
}

/// ES2024 §13.5.3 The typeof Operator
///
/// Table 41 — typeof Operator Results:
///
///   Type of val                              Result
///   ─────────────────────────────────────────────────────
///   Undefined                                "undefined"
///   Null                                     "object"
///   Boolean                                  "boolean"
///   Number                                   "number"
///   String                                   "string"
///   Symbol                                   "symbol"
///   BigInt                                   "bigint"
///   Object (does not implement [[Call]])      "object"
///   Object (implements [[Call]])              "function"
///
/// JsUninitialized (TDZ sentinel, not in spec) maps to "undefined".
/// This matches V8/SpiderMonkey behavior where accessing a TDZ variable throws
/// a ReferenceError before typeof ever runs, but our compiler may allow typeof
/// on uninitialized bindings as a defensive measure.
pub fn typeof_value(val: JsValue, heap: Heap) -> String {
  case val {
    // Table 41 row 1: Undefined → "undefined"
    // Also handles JsUninitialized (internal TDZ sentinel, not in spec)
    value.JsUndefined | value.JsUninitialized -> "undefined"
    // Table 41 row 2: Null → "object"
    value.JsNull -> "object"
    // Table 41 row 3: Boolean → "boolean"
    value.JsBool(_) -> "boolean"
    // Table 41 row 4: Number → "number"
    value.JsNumber(_) -> "number"
    // Table 41 row 5: String → "string"
    value.JsString(_) -> "string"
    // Table 41 row 8: BigInt → "bigint"
    value.JsBigInt(_) -> "bigint"
    // Table 41 row 7: Symbol → "symbol"
    value.JsSymbol(_) -> "symbol"
    // Table 41 rows 9-10: Object — check for [[Call]]
    value.JsObject(ref) ->
      case heap.read(heap, ref) {
        // Row 10: Object implements [[Call]] → "function"
        Some(ObjectSlot(kind: value.FunctionObject(..), ..)) -> "function"
        Some(ObjectSlot(kind: value.NativeFunction(..), ..)) -> "function"
        // Row 9: Object does not implement [[Call]] → "object"
        _ -> "object"
      }
  }
}

/// Full proto-ctor cycle for a new builtin type using forward references.
///
/// Not a spec operation — internal bootstrap helper.
/// Reserves the proto ref first, then allocates both objects in one pass —
/// no read-modify-write. Both proto and constructor are written exactly once.
/// This is the common case for most builtins.
pub fn init_type(
  h: Heap,
  parent_proto: Ref,
  function_proto: Ref,
  proto_props: List(#(String, Property)),
  ctor_fn: fn(Ref) -> NativeFn,
  name: String,
  arity: Int,
  ctor_props: List(#(String, Property)),
) -> #(Heap, BuiltinType) {
  // Reserve proto address — no data written yet
  let #(h, proto_ref) = heap.reserve(h)
  let h = heap.root(h, proto_ref)

  // Allocate constructor — proto_ref is already known via forward reference
  let #(h, ctor_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(ctor_fn(proto_ref)),
        properties: dict.from_list(ctor_properties(
          proto_ref,
          name,
          arity,
          ctor_props,
        )),
        elements: js_elements.new(),
        prototype: Some(function_proto),
        symbol_properties: dict.new(),
        extensible: True,
      ),
    )
  let h = heap.root(h, ctor_ref)

  // Fill reserved proto — ctor_ref is now known, single write with all properties
  let h =
    heap.fill(
      h,
      proto_ref,
      ObjectSlot(
        kind: OrdinaryObject,
        properties: dict.from_list(proto_properties(ctor_ref, proto_props)),
        elements: js_elements.new(),
        prototype: Some(parent_proto),
        symbol_properties: dict.new(),
        extensible: True,
      ),
    )

  #(h, BuiltinType(prototype: proto_ref, constructor: ctor_ref))
}

/// Proto-ctor cycle for a pre-allocated prototype (Object, Function bootstrap).
///
/// Not a spec operation — internal bootstrap helper.
/// The proto already exists on the heap (allocated empty for bootstrap reasons).
/// Reads its current state, merges in proto_props + constructor, writes back.
/// This read-modify-write is unavoidable for pre-existing protos.
pub fn init_type_on(
  h: Heap,
  proto: Ref,
  function_proto: Ref,
  proto_props: List(#(String, Property)),
  ctor_fn: fn(Ref) -> NativeFn,
  name: String,
  arity: Int,
  ctor_props: List(#(String, Property)),
) -> #(Heap, BuiltinType) {
  // Allocate constructor — proto ref already known
  let #(h, ctor_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(ctor_fn(proto)),
        properties: dict.from_list(ctor_properties(
          proto,
          name,
          arity,
          ctor_props,
        )),
        elements: js_elements.new(),
        prototype: Some(function_proto),
        symbol_properties: dict.new(),
        extensible: True,
      ),
    )
  let h = heap.root(h, ctor_ref)

  // Read-modify-write: merge proto_props + constructor onto existing proto
  let assert Some(ObjectSlot(
    kind:,
    properties:,
    elements:,
    prototype:,
    symbol_properties:,
    extensible:,
  )) = heap.read(h, proto)
  let new_props =
    list.fold(proto_properties(ctor_ref, proto_props), properties, fn(acc, p) {
      let #(key, val) = p
      dict.insert(acc, key, val)
    })
  let h =
    heap.write(
      h,
      proto,
      ObjectSlot(
        kind:,
        properties: new_props,
        elements:,
        prototype:,
        symbol_properties:,
        extensible:,
      ),
    )

  #(h, BuiltinType(prototype: proto, constructor: ctor_ref))
}

/// Allocate an error object with a message and given prototype.
///
/// ES2024 §20.5.6.1.1 NativeError ( message [ , options ] )
/// Simplified: we skip steps involving NewTarget / OrdinaryCreateFromConstructor
/// and the "options" parameter (InstallErrorCause). We directly allocate an
/// ordinary object with the NativeError prototype and set the "message" property.
///
/// Spec steps (simplified):
///   1. (skipped) If NewTarget is undefined, let newTarget be the active function.
///   2. (skipped) Let O be ? OrdinaryCreateFromConstructor(newTarget, ...).
///      We directly allocate with the correct prototype.
///   3. If message is not undefined, then
///      a. Let msg be ? ToString(message).
///      b. Perform CreateNonEnumerableDataPropertyOrThrow(O, "message", msg).
///      We always set "message" — callers pass a string directly.
///   4. (skipped) Perform ? InstallErrorCause(O, options).
///   5. Return O.
///
/// Local copy of object.make_error to avoid the import cycle
/// (object.gleam -> builtins -> builtins/* -> object).
fn alloc_error(h: Heap, proto: Ref, message: String) -> #(Heap, JsValue) {
  let #(h, ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: OrdinaryObject,
        // Step 3b: CreateNonEnumerableDataPropertyOrThrow(O, "message", msg)
        // Per §20.5.6.3: writable+configurable, NOT enumerable.
        properties: dict.from_list([
          #("message", value.builtin_property(JsString(message))),
        ]),
        elements: js_elements.new(),
        // Step 2: [[Prototype]] set to the NativeError prototype
        prototype: Some(proto),
        symbol_properties: dict.new(),
        extensible: True,
      ),
    )
  #(h, JsObject(ref))
}

/// ES2024 §20.5.6.1.1 NativeError ( message [ , options ] )
/// Allocates a TypeError instance. See alloc_error for spec step details.
pub fn make_type_error(
  h: Heap,
  b: Builtins,
  message: String,
) -> #(Heap, JsValue) {
  alloc_error(h, b.type_error.prototype, message)
}

/// ES2024 §20.5.6.1.1 NativeError ( message [ , options ] )
/// Allocates a RangeError instance. See alloc_error for spec step details.
pub fn make_range_error(
  h: Heap,
  b: Builtins,
  message: String,
) -> #(Heap, JsValue) {
  alloc_error(h, b.range_error.prototype, message)
}

/// ES2024 §20.5.6.1.1 NativeError ( message [ , options ] )
/// Allocates a ReferenceError instance. See alloc_error for spec step details.
pub fn make_reference_error(
  h: Heap,
  b: Builtins,
  message: String,
) -> #(Heap, JsValue) {
  alloc_error(h, b.reference_error.prototype, message)
}

/// ES2024 §20.5.6.1.1 NativeError ( message [ , options ] )
/// Allocates a SyntaxError instance. See alloc_error for spec step details.
pub fn make_syntax_error(
  h: Heap,
  b: Builtins,
  message: String,
) -> #(Heap, JsValue) {
  alloc_error(h, b.syntax_error.prototype, message)
}

/// ES2024 §7.1.18 ToObject ( argument )
///
/// Converts a JS value to an object. Used when a spec algorithm requires an
/// object but receives a primitive (e.g. Object.keys(primitive), property
/// access on primitives for method calls).
///
/// Table 15 — ToObject Conversions:
///
///   Argument Type    Result
///   ─────────────────────────────────────────────────────────────────────
///   Undefined        Throw a TypeError exception.
///   Null             Throw a TypeError exception.
///   Boolean          Return a new Boolean object (§20.3.4) with [[BooleanData]] = argument.
///   Number           Return a new Number object (§21.1.4) with [[NumberData]] = argument.
///   String           Return a new String object (§22.1.4) with [[StringData]] = argument.
///   Symbol           Return a new Symbol object (§20.4.4) with [[SymbolData]] = argument.
///   BigInt           Return a new BigInt object (§21.2.4) with [[BigIntData]] = argument.
///   Object           Return argument (no conversion needed).
///
/// Returns Option instead of Result — callers handle the TypeError themselves
/// because they need access to the Builtins to allocate the error object,
/// and this function already receives Builtins.
///
/// TODO(Deviation): SymbolObject uses Object.prototype instead of Symbol.prototype
///   (no dedicated Symbol.prototype with toString/valueOf/description yet).
/// TODO(Deviation): BigInt falls back to OrdinaryObject with Object.prototype (no
///   BigInt wrapper object kind or BigInt.prototype yet).
pub fn to_object(h: Heap, b: Builtins, val: JsValue) -> Option(#(Heap, Ref)) {
  case val {
    // Table 15 row 8: Object → return argument (identity)
    JsObject(ref) -> Some(#(h, ref))
    // Table 15 rows 1-2: Undefined/Null → TypeError (caller must throw)
    value.JsUndefined | value.JsNull -> None
    // Table 15 row 5: String → new String object with [[StringData]]
    JsString(s) ->
      Some(alloc_wrapper(h, value.StringObject(s), b.string.prototype))
    // Table 15 row 4: Number → new Number object with [[NumberData]]
    value.JsNumber(n) ->
      Some(alloc_wrapper(h, value.NumberObject(n), b.number.prototype))
    // Table 15 row 3: Boolean → new Boolean object with [[BooleanData]]
    value.JsBool(bv) ->
      Some(alloc_wrapper(h, value.BooleanObject(bv), b.boolean.prototype))
    // Table 15 row 6: Symbol → new Symbol object with [[SymbolData]]
    // TODO(Deviation): uses Object.prototype (no Symbol.prototype yet)
    value.JsSymbol(sym) ->
      Some(alloc_wrapper(h, value.SymbolObject(sym), b.object.prototype))
    // Table 15 row 7: BigInt → new BigInt object with [[BigIntData]]
    // TODO(Deviation): uses OrdinaryObject kind (no BigIntObject kind yet)
    value.JsBigInt(_) ->
      Some(alloc_wrapper(h, OrdinaryObject, b.object.prototype))
    // Internal: TDZ sentinel, not in spec — treat as Undefined (→ TypeError)
    value.JsUninitialized -> None
  }
}

/// Helper for ToObject (§7.1.18): allocate a wrapper object for a primitive.
///
/// Creates an ordinary object with the given ExoticKind (which carries the
/// [[PrimitiveData]] internal slot, e.g. StringObject(s) = [[StringData]])
/// and the appropriate builtin prototype.
fn alloc_wrapper(h: Heap, kind: ExoticKind, proto: Ref) -> #(Heap, Ref) {
  heap.alloc(
    h,
    ObjectSlot(
      kind:,
      properties: dict.new(),
      elements: js_elements.new(),
      prototype: Some(proto),
      symbol_properties: dict.new(),
      extensible: True,
    ),
  )
}

/// Allocate a JS array from a list of values.
///
/// Loosely corresponds to ES2024 §10.4.2.2 ArrayCreate ( length [ , proto ] ):
///   1. (Assert) length is a non-negative integer — enforced by list.length.
///   2. (skipped) If length > 2^32 - 1, throw RangeError — not enforced.
///   3. (skipped) If proto not present, set to %Array.prototype% — caller
///      must pass array_proto explicitly.
///   4. Let A be MakeBasicObject(« [[Prototype]], [[Extensible]] »).
///   5. Set A.[[Prototype]] to proto.
///   6. Set A.[[DefineOwnProperty]] to ArrayDefineOwnProperty (exotic).
///      We model this via ArrayObject(length) exotic kind.
///   7. Perform ! OrdinaryDefineOwnProperty(A, "length", { [[Value]]: length,
///      [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false }).
///      We store length in the ArrayObject(count) exotic kind; the virtual
///      "length" property is synthesized at read time.
///   8. Return A.
///
/// Note: does not enforce the 2^32-1 length limit (step 2).
pub fn alloc_array(
  h: Heap,
  values: List(JsValue),
  array_proto: Ref,
) -> #(Heap, Ref) {
  // Step 1: length = number of values
  let count = list.length(values)
  // Steps 4-8: create array exotic object
  heap.alloc(
    h,
    ObjectSlot(
      // Step 6: exotic [[DefineOwnProperty]] via ArrayObject kind
      // Step 7: length stored in ArrayObject(count)
      kind: ArrayObject(count),
      properties: dict.new(),
      elements: js_elements.from_list(values),
      // Step 5: [[Prototype]] = proto
      prototype: Some(array_proto),
      symbol_properties: dict.new(),
      // Step 4: [[Extensible]] = true
      extensible: True,
    ),
  )
}
