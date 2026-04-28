import arc/vm/heap.{type Heap}
import arc/vm/internal/elements
import arc/vm/value.{
  type CallNativeFn, type ExoticKind, type JsValue, type NativeFn,
  type NativeFnSlot, type Property, type PropertyKey, type Ref, AccessorProperty,
  ArrayObject, Call, Dispatch, JsObject, JsString, Named, NativeFunction,
  ObjectSlot, OrdinaryObject,
}
import gleam/dict.{type Dict}
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
    eval_error: BuiltinType,
    uri_error: BuiltinType,
    aggregate_error: BuiltinType,
    dom_exception: BuiltinType,
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
    async_generator: GeneratorBuiltin,
    symbol: Ref,
    arc: Ref,
    json: Ref,
    reflect: Ref,
    map: BuiltinType,
    set: BuiltinType,
    weak_map: BuiltinType,
    weak_set: BuiltinType,
    iterator: BuiltinType,
    iterator_helper_proto: Ref,
    wrap_for_valid_iterator_proto: Ref,
    regexp: BuiltinType,
    date: BuiltinType,
    eval: Ref,
    decode_uri: Ref,
    encode_uri: Ref,
    decode_uri_component: Ref,
    encode_uri_component: Ref,
    escape: Ref,
    unescape: Ref,
    structured_clone: Ref,
    array_iterator_proto: Ref,
    set_iterator_proto: Ref,
    map_iterator_proto: Ref,
    async_from_sync_iterator_proto: Ref,
  )
}

/// Allocate an ordinary prototype object on the heap, root it, and return
/// the updated heap + ref. Shared bootstrap helper for all builtin modules.
///
/// Not a spec operation — internal helper for builtin initialization.
/// Creates the prototype object that will be used as [[Prototype]] for
/// instances of a builtin type.
pub fn alloc_proto(
  h: Heap(ctx),
  prototype: Option(Ref),
  properties: Dict(PropertyKey, Property),
) -> #(Heap(ctx), Ref) {
  let #(h, ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: OrdinaryObject,
        properties:,
        elements: elements.new(),
        prototype:,
        symbol_properties: [],
        extensible: True,
      ),
    )
  let h = heap.root(h, ref)
  #(h, ref)
}

/// Allocate+root an OrdinaryObject with @@toStringTag = `tag`. Covers namespace
/// globals (Math/JSON/Reflect/Arc) and tagged prototypes (Generator, Iterator Helper).
pub fn init_namespace(
  h: Heap(ctx),
  proto: Ref,
  tag: String,
  props: List(#(String, Property)),
) -> #(Heap(ctx), Ref) {
  let #(h, ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: OrdinaryObject,
        properties: named_props(props),
        symbol_properties: [to_string_tag(tag)],
        elements: elements.new(),
        prototype: Some(proto),
        extensible: True,
      ),
    )
  #(heap.root(h, ref), ref)
}

pub fn alloc_pojo(
  heap: Heap(ctx),
  object_proto: Ref,
  props: List(#(String, value.Property)),
) -> #(Heap(ctx), Ref) {
  heap.alloc(
    heap,
    ObjectSlot(
      kind: OrdinaryObject,
      properties: named_props(props),
      symbol_properties: [],
      elements: elements.new(),
      prototype: Some(object_proto),
      extensible: True,
    ),
  )
}

/// Build a PropertyKey-keyed dict from String-keyed entries. Builtin init code
/// only ever uses named keys (never indices), so this wraps each key in Named.
pub fn named_props(
  props: List(#(String, Property)),
) -> Dict(PropertyKey, Property) {
  use acc, #(k, v) <- list.fold(props, dict.new())
  dict.insert(acc, Named(k), v)
}

/// Allocate a NativeFunction ObjectSlot with standard name/length properties.
///
/// Not a spec operation — internal helper for builtin initialization.
/// Creates a function object with the correct .name and .length data
/// properties per §20.2.3 (Function instances).
pub fn alloc_native_fn(
  h: Heap(ctx),
  function_proto: Ref,
  native: NativeFn,
  name: String,
  arity: Int,
) -> #(Heap(ctx), Ref) {
  alloc_native_fn_slot(h, function_proto, Dispatch(native), name, arity)
}

/// Allocate a NativeFunction ObjectSlot from a NativeFnSlot directly.
fn alloc_native_fn_slot(
  h: Heap(ctx),
  function_proto: Ref,
  slot: NativeFnSlot(ctx),
  name: String,
  arity: Int,
) -> #(Heap(ctx), Ref) {
  let #(h, ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(slot),
        properties: named_props([
          #("name", fn_name_property(name)),
          #("length", fn_length_property(arity)),
        ]),
        symbol_properties: [],
        elements: elements.new(),
        prototype: Some(function_proto),
        extensible: True,
      ),
    )
  let h = heap.root(h, ref)
  #(h, ref)
}

/// Allocate a host-provided native function. Same heap shape as built-in
/// natives (name/length properties, Function.prototype), but dispatches
/// to the embedder's closure via the Host variant.
pub fn alloc_host_fn(
  h: Heap(ctx),
  function_proto: Ref,
  impl: fn(List(JsValue), JsValue, ctx) -> #(ctx, Result(JsValue, JsValue)),
  name: String,
  arity: Int,
) -> #(Heap(ctx), Ref) {
  alloc_native_fn_slot(h, function_proto, value.Host(impl), name, arity)
}

/// ES2024 §20.2.2: Function name property — non-writable, non-enumerable, configurable.
pub fn fn_name_property(name: String) -> Property {
  value.data(JsString(name)) |> value.configurable()
}

/// ES2024 §20.2.2: Function length property — non-writable, non-enumerable, configurable.
pub fn fn_length_property(arity: Int) -> Property {
  value.data(value.from_int(arity)) |> value.configurable()
}

/// Allocate N native function objects from specs, returning builtin_property
/// entries. Replaces the identical fold duplicated across builtin modules.
///
/// Not a spec operation — internal helper that batch-allocates method
/// function objects for a prototype's property list.
pub fn alloc_methods(
  h: Heap(ctx),
  function_proto: Ref,
  specs: List(#(String, NativeFn, Int)),
) -> #(Heap(ctx), List(#(String, Property))) {
  list.fold(specs, #(h, []), fn(acc, spec) {
    let #(h, props) = acc
    let #(name, native, arity) = spec
    let #(h, fn_ref) = alloc_native_fn(h, function_proto, native, name, arity)
    #(h, [#(name, value.builtin_property(JsObject(fn_ref))), ..props])
  })
}

/// Allocate N getter function objects from specs, returning get-only
/// AccessorProperty entries (non-enumerable, configurable). Mirrors alloc_methods.
pub fn alloc_getters(
  h: Heap(ctx),
  function_proto: Ref,
  specs: List(#(String, NativeFn)),
) -> #(Heap(ctx), List(#(String, Property))) {
  list.fold(specs, #(h, []), fn(acc, spec) {
    let #(h, props) = acc
    let #(name, native) = spec
    let #(h, fn_ref) =
      alloc_native_fn(h, function_proto, native, "get " <> name, 0)
    let prop =
      AccessorProperty(
        get: Some(JsObject(fn_ref)),
        set: None,
        enumerable: False,
        configurable: True,
      )
    #(h, [#(name, prop), ..props])
  })
}

/// Allocate a getter+setter native fn pair and return the AccessorProperty
/// (non-enumerable, configurable). Getter arity 0 / "get <name>", setter
/// arity 1 / "set <name>". Used for SetterThatIgnoresPrototypeProperties.
pub fn alloc_get_set_accessor(
  h: Heap(ctx),
  function_proto: Ref,
  get: NativeFn,
  set: NativeFn,
  name: String,
) -> #(Heap(ctx), Property) {
  let #(h, get_ref) = alloc_native_fn(h, function_proto, get, "get " <> name, 0)
  let #(h, set_ref) = alloc_native_fn(h, function_proto, set, "set " <> name, 1)
  #(
    h,
    AccessorProperty(
      get: Some(JsObject(get_ref)),
      set: Some(JsObject(set_ref)),
      enumerable: False,
      configurable: True,
    ),
  )
}

/// Batch allocate call-level native method objects (Function.call/apply/bind,
/// Promise.then/catch, Generator.next/return/throw, etc.).
pub fn alloc_call_methods(
  h: Heap(ctx),
  function_proto: Ref,
  specs: List(#(String, CallNativeFn, Int)),
) -> #(Heap(ctx), List(#(String, Property))) {
  list.fold(specs, #(h, []), fn(acc, spec) {
    let #(h, props) = acc
    let #(name, native, arity) = spec
    let #(h, fn_ref) =
      alloc_native_fn_slot(h, function_proto, Call(native), name, arity)
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
    #("name", fn_name_property(name)),
    #("length", fn_length_property(arity)),
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
pub fn typeof_value(val: JsValue, heap: Heap(ctx)) -> String {
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
  h: Heap(ctx),
  parent_proto: Ref,
  function_proto: Ref,
  proto_props: List(#(String, Property)),
  ctor_fn: fn(Ref) -> NativeFnSlot(ctx),
  name: String,
  arity: Int,
  ctor_props: List(#(String, Property)),
) -> #(Heap(ctx), BuiltinType) {
  // Reserve proto address — no data written yet
  let #(h, proto_ref) = heap.reserve(h)
  let h = heap.root(h, proto_ref)

  // Allocate constructor — proto_ref is already known via forward reference
  let #(h, ctor_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(ctor_fn(proto_ref)),
        properties: named_props(ctor_properties(
          proto_ref,
          name,
          arity,
          ctor_props,
        )),
        elements: elements.new(),
        prototype: Some(function_proto),
        symbol_properties: [],
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
        properties: named_props(proto_properties(ctor_ref, proto_props)),
        elements: elements.new(),
        prototype: Some(parent_proto),
        symbol_properties: [],
        extensible: True,
      ),
    )

  #(h, BuiltinType(prototype: proto_ref, constructor: ctor_ref))
}

/// Add a symbol-keyed property to an existing object (typically a prototype).
/// Used after init_type to wire up Symbol.iterator, Symbol.toStringTag, etc.
pub fn add_symbol_property(
  h: Heap(ctx),
  ref: Ref,
  symbol: value.SymbolId,
  prop: Property,
) -> Heap(ctx) {
  heap.update(h, ref, fn(slot) {
    case slot {
      ObjectSlot(symbol_properties:, ..) ->
        ObjectSlot(
          ..slot,
          symbol_properties: list.key_set(symbol_properties, symbol, prop),
        )
      other -> other
    }
  })
}

/// @@toStringTag symbol-property pair: { writable: false, enumerable: false, configurable: true }.
pub fn to_string_tag(name: String) -> #(value.SymbolId, Property) {
  #(
    value.symbol_to_string_tag,
    value.data(JsString(name)) |> value.configurable(),
  )
}

/// Add @@toStringTag = name to an existing object (typically a prototype).
pub fn add_to_string_tag(h: Heap(ctx), ref: Ref, name: String) -> Heap(ctx) {
  add_symbol_property(
    h,
    ref,
    value.symbol_to_string_tag,
    value.data(JsString(name)) |> value.configurable(),
  )
}

/// Proto-ctor cycle for a pre-allocated prototype (Object, Function bootstrap).
///
/// Not a spec operation — internal bootstrap helper.
/// The proto already exists on the heap (allocated empty for bootstrap reasons).
/// Reads its current state, merges in proto_props + constructor, writes back.
/// This read-modify-write is unavoidable for pre-existing protos.
pub fn init_type_on(
  h: Heap(ctx),
  proto: Ref,
  function_proto: Ref,
  proto_props: List(#(String, Property)),
  ctor_fn: fn(Ref) -> NativeFnSlot(ctx),
  name: String,
  arity: Int,
  ctor_props: List(#(String, Property)),
) -> #(Heap(ctx), BuiltinType) {
  // Allocate constructor — proto ref already known
  let #(h, ctor_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(ctor_fn(proto)),
        properties: named_props(ctor_properties(proto, name, arity, ctor_props)),
        elements: elements.new(),
        prototype: Some(function_proto),
        symbol_properties: [],
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
      dict.insert(acc, Named(key), val)
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
fn alloc_error(
  h: Heap(ctx),
  proto: Ref,
  message: String,
) -> #(Heap(ctx), JsValue) {
  let #(h, ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: OrdinaryObject,
        // Step 3b: CreateNonEnumerableDataPropertyOrThrow(O, "message", msg)
        // Per §20.5.6.3: writable+configurable, NOT enumerable.
        properties: named_props([
          #("message", value.builtin_property(JsString(message))),
        ]),
        elements: elements.new(),
        // Step 2: [[Prototype]] set to the NativeError prototype
        prototype: Some(proto),
        symbol_properties: [],
        extensible: True,
      ),
    )
  #(h, JsObject(ref))
}

/// ES2024 §20.5.6.1.1 NativeError ( message [ , options ] )
/// Allocates a TypeError instance. See alloc_error for spec step details.
pub fn make_type_error(
  h: Heap(ctx),
  b: Builtins,
  message: String,
) -> #(Heap(ctx), JsValue) {
  alloc_error(h, b.type_error.prototype, message)
}

/// ES2024 §20.5.6.1.1 NativeError ( message [ , options ] )
/// Allocates a RangeError instance. See alloc_error for spec step details.
pub fn make_range_error(
  h: Heap(ctx),
  b: Builtins,
  message: String,
) -> #(Heap(ctx), JsValue) {
  alloc_error(h, b.range_error.prototype, message)
}

/// ES2024 §20.5.6.1.1 NativeError ( message [ , options ] )
/// Allocates a ReferenceError instance. See alloc_error for spec step details.
pub fn make_reference_error(
  h: Heap(ctx),
  b: Builtins,
  message: String,
) -> #(Heap(ctx), JsValue) {
  alloc_error(h, b.reference_error.prototype, message)
}

/// ES2024 §20.5.6.1.1 NativeError ( message [ , options ] )
/// Allocates a SyntaxError instance. See alloc_error for spec step details.
pub fn make_syntax_error(
  h: Heap(ctx),
  b: Builtins,
  message: String,
) -> #(Heap(ctx), JsValue) {
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
pub fn to_object(
  h: Heap(ctx),
  b: Builtins,
  val: JsValue,
) -> Option(#(Heap(ctx), Ref)) {
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

/// Helper for ToObject (§7.1.18) and primitive-wrapper constructors
/// (`new String/Number/Boolean`): allocate a wrapper object for a primitive.
///
/// Creates an ordinary object with the given ExoticKind (which carries the
/// type-specific internal slot — [[StringData]] / [[NumberData]] /
/// [[BooleanData]] etc.) and the appropriate builtin prototype.
pub fn alloc_wrapper(
  h: Heap(ctx),
  kind: ExoticKind(ctx),
  proto: Ref,
) -> #(Heap(ctx), Ref) {
  heap.alloc(
    h,
    ObjectSlot(
      kind:,
      properties: dict.new(),
      elements: elements.new(),
      prototype: Some(proto),
      symbol_properties: [],
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
  h: Heap(ctx),
  values: List(JsValue),
  array_proto: Ref,
) -> #(Heap(ctx), Ref) {
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
      elements: elements.from_list(values),
      // Step 5: [[Prototype]] = proto
      prototype: Some(array_proto),
      symbol_properties: [],
      // Step 4: [[Extensible]] = true
      extensible: True,
    ),
  )
}
