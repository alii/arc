import arc/vm/heap.{type Heap}
import arc/vm/internal/elements
import arc/vm/key.{type PropertyKey, Named}
import arc/vm/value.{
  type CallNativeFn, type ExoticKind, type JsElements, type JsValue,
  type NativeFn, type NativeFnSlot, type Property, type Ref, ArrayObject, Call,
  Dispatch, JsBool, JsObject, JsString, NativeFunction, ObjectSlot,
  OrdinaryObject,
}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}

/// A prototype + constructor pair. Every JS builtin type has both.
pub type BuiltinType {
  BuiltinType(prototype: Ref, constructor: Ref)
}

/// The 11 concrete TypedArray constructors — one field per element kind, so
/// the mapping is TOTAL: every kind has exactly one constructor, none can be
/// missing or duplicated, and adding a kind to `value.TypedArrayKind` breaks
/// the build here rather than at a runtime lookup.
pub type TypedArrays {
  TypedArrays(
    int8: BuiltinType,
    uint8: BuiltinType,
    uint8_clamped: BuiltinType,
    int16: BuiltinType,
    uint16: BuiltinType,
    int32: BuiltinType,
    uint32: BuiltinType,
    float32: BuiltinType,
    float64: BuiltinType,
    big_int64: BuiltinType,
    big_uint64: BuiltinType,
  )
}

/// The concrete constructor for a TypedArray element kind. Total — no lookup,
/// no failure mode.
pub fn typed_array_of(
  t: TypedArrays,
  kind: value.TypedArrayKind,
) -> BuiltinType {
  case kind {
    value.NumKind(value.Int8Kind) -> t.int8
    value.NumKind(value.Uint8Kind) -> t.uint8
    value.NumKind(value.Uint8ClampedKind) -> t.uint8_clamped
    value.NumKind(value.Int16Kind) -> t.int16
    value.NumKind(value.Uint16Kind) -> t.uint16
    value.NumKind(value.Int32Kind) -> t.int32
    value.NumKind(value.Uint32Kind) -> t.uint32
    value.NumKind(value.Float32Kind) -> t.float32
    value.NumKind(value.Float64Kind) -> t.float64
    value.BigKind(value.BigInt64Kind) -> t.big_int64
    value.BigKind(value.BigUint64Kind) -> t.big_uint64
  }
}

/// The 11 constructors keyed by kind, in global-installation order.
pub fn typed_array_entries(
  t: TypedArrays,
) -> List(#(value.TypedArrayKind, BuiltinType)) {
  use kind <- list.map(value.all_typed_array_kinds)
  #(kind, typed_array_of(t, kind))
}

/// Generator intrinsics. `prototype` is %GeneratorPrototype% — the prototype
/// for generator OBJECTS. `fn_proto` is %GeneratorFunction.prototype% — the
/// [[Prototype]] of generator FUNCTION objects (§27.3.3); its "constructor"
/// property is the %GeneratorFunction% dynamic constructor.
pub type GeneratorBuiltin {
  GeneratorBuiltin(prototype: Ref, fn_proto: Ref)
}

/// §27.3/§27.4: build a %GeneratorFunction%-style intrinsic pair — the dynamic
/// constructor plus the prototype that generator FUNCTION objects use as
/// [[Prototype]] (§27.3.2/§27.3.3). See `init_function_intrinsic` for the
/// layout; the generator flavor is the `Some(generator_proto)` case.
pub fn init_generator_function(
  h: Heap(ctx, host),
  name: String,
  native: NativeFn,
  function_proto: Ref,
  function_ctor: Ref,
  generator_proto: Ref,
) -> #(Heap(ctx, host), Ref) {
  init_function_intrinsic(
    h,
    name,
    native,
    function_proto,
    function_ctor,
    Some(generator_proto),
  )
}

/// §27.7: build the %AsyncFunction% intrinsic pair — the dynamic constructor
/// plus %AsyncFunction.prototype%, the [[Prototype]] of async FUNCTION objects
/// (§27.7.2/§27.7.3). See `init_function_intrinsic` for the layout; unlike the
/// generator flavor, fn_proto has NO "prototype" property (async functions are
/// not constructors), i.e. the `None` case.
pub fn init_async_function(
  h: Heap(ctx, host),
  name: String,
  native: NativeFn,
  function_proto: Ref,
  function_ctor: Ref,
) -> #(Heap(ctx, host), Ref) {
  init_function_intrinsic(h, name, native, function_proto, function_ctor, None)
}

/// Shared core of `init_generator_function` / `init_async_function`: build a
/// dynamic constructor plus the fn_proto that the corresponding FUNCTION
/// objects use as [[Prototype]]. Common layout:
///   ctor.[[Prototype]] = %Function% (the Function constructor)
///   ctor.prototype = fn_proto                 { W:F, E:F, C:F }
///   fn_proto.[[Prototype]] = Function.prototype
///   fn_proto.constructor = ctor               { W:F, E:F, C:T }
///   fn_proto[@@toStringTag] = name            { W:F, E:F, C:T }
///
/// The ONLY difference between the two intrinsics is `generator_proto`:
///   * `Some(gp)` — additionally fn_proto.prototype = gp { W:F, E:F, C:T },
///     and gp.constructor is backpatched to fn_proto (§27.5.1.1 / §27.6.1.1).
///   * `None`     — no `prototype` property, nothing to backpatch (§27.7.3).
///
/// Returns the fn_proto ref (the ctor is reachable via its "constructor").
fn init_function_intrinsic(
  h: Heap(ctx, host),
  name: String,
  native: NativeFn,
  function_proto: Ref,
  function_ctor: Ref,
  generator_proto: Option(Ref),
) -> #(Heap(ctx, host), Ref) {
  let #(h, fn_proto_ref) = heap.reserve(h)
  let h = heap.root(h, fn_proto_ref)
  let #(h, ctor_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(Dispatch(native), constructible: True),
        properties: named_props([
          #("length", fn_length_property(1)),
          #("name", fn_name_property(name)),
          #("prototype", fn_prototype_property(fn_proto_ref)),
        ]),
        elements: elements.new(),
        prototype: Some(function_ctor),
        symbol_properties: [],
        extensible: True,
      ),
    )
  let h = heap.root(h, ctor_ref)
  // `constructor` is stamped BEFORE `prototype` — `value.data` takes its `seq`
  // from a global counter at call time, and that seq is what orders
  // Object.getOwnPropertyNames. Hoisting this binding keeps the original
  // enumeration order regardless of where the case arms place the entries.
  let ctor_prop = value.data(JsObject(ctor_ref)) |> value.configurable
  let proto_props = case generator_proto {
    Some(gp) -> [
      #("constructor", ctor_prop),
      #("prototype", value.data(JsObject(gp)) |> value.configurable),
    ]
    None -> [#("constructor", ctor_prop)]
  }
  let h =
    heap.write(
      h,
      fn_proto_ref,
      ObjectSlot(
        kind: OrdinaryObject,
        properties: named_props(proto_props),
        elements: elements.new(),
        prototype: Some(function_proto),
        symbol_properties: [to_string_tag(name)],
        extensible: True,
      ),
    )
  // §27.5.1.1 / §27.6.1.1: Generator.prototype.constructor is the fn_proto
  // object itself ({ W:F, E:F, C:T }). Nothing to backpatch for %AsyncFunction%.
  let h = case generator_proto {
    Some(gp) ->
      add_named_property(
        h,
        gp,
        "constructor",
        value.data(JsObject(fn_proto_ref)) |> value.configurable,
      )
    None -> h
  }
  #(h, fn_proto_ref)
}

/// Pre-allocated prototype objects and constructor functions for JS built-ins.
/// All refs are rooted so GC never collects them.
pub type Builtins {
  Builtins(
    object: BuiltinType,
    function: BuiltinType,
    /// %ThrowTypeError% (§10.2.4.1) — the single frozen intrinsic shared by
    /// every restricted `caller`/`arguments` accessor and by an unmapped
    /// arguments object's `callee`. Held as a Ref so those install sites can
    /// name it directly instead of fishing it back out of the heap.
    throw_type_error: Ref,
    array: BuiltinType,
    /// The Proxy constructor function object (§28.2). No prototype property.
    proxy: Ref,
    error: BuiltinType,
    type_error: BuiltinType,
    reference_error: BuiltinType,
    range_error: BuiltinType,
    syntax_error: BuiltinType,
    eval_error: BuiltinType,
    uri_error: BuiltinType,
    aggregate_error: BuiltinType,
    suppressed_error: BuiltinType,
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
    /// %AsyncFunction.prototype% (§27.7.3) — the [[Prototype]] of async
    /// function objects. Its "constructor" is the %AsyncFunction% dynamic
    /// constructor.
    async_function_proto: Ref,
    symbol: Ref,
    /// %Symbol.prototype% — dedicated prototype object (toString/valueOf/
    /// description/@@toPrimitive). Distinct from %Object.prototype% so
    /// `x instanceof Symbol` walks the right chain.
    symbol_proto: Ref,
    console: Ref,
    json: Ref,
    reflect: Ref,
    map: BuiltinType,
    set: BuiltinType,
    weak_map: BuiltinType,
    weak_set: BuiltinType,
    finalization_registry: BuiltinType,
    disposable_stack: BuiltinType,
    async_disposable_stack: BuiltinType,
    array_buffer: BuiltinType,
    shared_array_buffer: BuiltinType,
    /// %TypedArray% — the abstract intrinsic (Object.getPrototypeOf(Int8Array)).
    typed_array: BuiltinType,
    /// The 11 concrete TypedArray constructors, one per element kind.
    typed_arrays: TypedArrays,
    /// The BigInt global function (§21.2.1.1, callable, not constructible)
    /// plus %BigInt.prototype% (§21.2.3) — primitive BigInt property access
    /// delegates to the prototype (toString/toLocaleString/valueOf).
    bigint: BuiltinType,
    data_view: BuiltinType,
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
    array_iterator_proto: Ref,
    set_iterator_proto: Ref,
    map_iterator_proto: Ref,
    async_from_sync_iterator_proto: Ref,
    intl: Ref,
  )
}

/// The concrete constructor for a TypedArray element kind, straight off the
/// realm's builtins. Total: no lookup, so no "kind was never installed" case.
pub fn typed_array_builtin(
  b: Builtins,
  kind: value.TypedArrayKind,
) -> BuiltinType {
  typed_array_of(b.typed_arrays, kind)
}

/// Allocate an ordinary prototype object on the heap, root it, and return
/// the updated heap + ref. Shared bootstrap helper for all builtin modules.
///
/// Not a spec operation — internal helper for builtin initialization.
/// Creates the prototype object that will be used as [[Prototype]] for
/// instances of a builtin type.
pub fn alloc_proto(
  h: Heap(ctx, host),
  prototype: Option(Ref),
  properties: Dict(PropertyKey, Property),
) -> #(Heap(ctx, host), Ref) {
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
/// globals (Math/JSON/Reflect/console) and tagged prototypes (Generator, Iterator Helper).
pub fn init_namespace(
  h: Heap(ctx, host),
  proto: Ref,
  tag: String,
  props: List(#(String, Property)),
) -> #(Heap(ctx, host), Ref) {
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
  heap: Heap(ctx, host),
  object_proto: Ref,
  props: List(#(String, value.Property)),
) -> #(Heap(ctx, host), Ref) {
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

// §10.1.13.1 OrdinaryCreateFromConstructor lives at `ops/object` — this layer
// cannot perform the real `? Get(newTarget, "prototype")` its step 2 requires
// (`state` imports `common`, not the other way round), so a copy here could
// only ever be a raw slot read that silently skips proxy traps and accessor
// `prototype` properties. Native constructors reach it via
// `helpers.require_new_target` / `ops_object.proto_from_new_target`.

/// CreateIterResultObject(value, done) — §7.4.11. Allocates `{value, done}`.
pub fn create_iter_result(
  h: Heap(ctx, host),
  builtins: Builtins,
  val: JsValue,
  done: Bool,
) -> #(Heap(ctx, host), JsValue) {
  let #(h, ref) =
    alloc_pojo(h, builtins.object.prototype, [
      #("value", value.data_property(val)),
      #("done", value.data_property(JsBool(done))),
    ])
  #(h, JsObject(ref))
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
  h: Heap(ctx, host),
  function_proto: Ref,
  native: NativeFn,
  name: String,
  arity: Int,
) -> #(Heap(ctx, host), Ref) {
  alloc_native_fn_slot(h, function_proto, Dispatch(native), name, arity)
}

/// Allocate a NativeFunction ObjectSlot from a NativeFnSlot directly.
fn alloc_native_fn_slot(
  h: Heap(ctx, host),
  function_proto: Ref,
  slot: NativeFnSlot(ctx),
  name: String,
  arity: Int,
) -> #(Heap(ctx, host), Ref) {
  // Ordinary built-ins (methods, standalone functions, host fns) are
  // not constructors. The constructor intrinsics take a separate path
  // (init_type / init_type_on), and bind copies its target's bit.
  let #(h, ref) =
    alloc_fn_slot(h, function_proto, slot, False, [
      #("length", fn_length_property(arity)),
      #("name", fn_name_property(name)),
    ])
  let h = heap.root(h, ref)
  #(h, ref)
}

/// Allocate a Call-dispatched NativeFunction object with standard name/length
/// properties, WITHOUT rooting it. For transient function objects (promise
/// resolving functions, reaction closures) whose lifetime is governed by
/// normal GC reachability — rooting them would leak.
pub fn alloc_call_fn(
  h: Heap(ctx, host),
  function_proto: Ref,
  native: CallNativeFn,
  name: String,
  arity: Int,
  constructible constructible: Bool,
) -> #(Heap(ctx, host), Ref) {
  alloc_fn_slot(h, function_proto, Call(native), constructible, [
    #("length", fn_length_property(arity)),
    #("name", fn_name_property(name)),
  ])
}

/// Like alloc_call_fn, but with a caller-supplied named property list — for
/// function objects whose properties aren't just name+length (the Symbol
/// constructor's well-known symbols, bound functions' name-only set).
/// Non-rooting; callers that need rooting do it themselves.
pub fn alloc_call_fn_props(
  h: Heap(ctx, host),
  function_proto: Ref,
  native: CallNativeFn,
  constructible constructible: Bool,
  props props: List(#(String, Property)),
) -> #(Heap(ctx, host), Ref) {
  alloc_fn_slot(h, function_proto, Call(native), constructible, props)
}

/// Shared core: allocate a NativeFunction ObjectSlot (non-rooting).
fn alloc_fn_slot(
  h: Heap(ctx, host),
  function_proto: Ref,
  slot: NativeFnSlot(ctx),
  constructible: Bool,
  props: List(#(String, Property)),
) -> #(Heap(ctx, host), Ref) {
  heap.alloc(
    h,
    ObjectSlot(
      kind: NativeFunction(slot, constructible:),
      properties: named_props(props),
      symbol_properties: [],
      elements: elements.new(),
      prototype: Some(function_proto),
      extensible: True,
    ),
  )
}

/// Allocate a host-provided native function. Same heap shape as built-in
/// natives (name/length properties, Function.prototype), but dispatches
/// to the embedder's closure via the Host variant.
pub fn alloc_host_fn(
  h: Heap(ctx, host),
  function_proto: Ref,
  impl: fn(List(JsValue), JsValue, ctx) -> #(ctx, Result(JsValue, JsValue)),
  name: String,
  arity: Int,
) -> #(Heap(ctx, host), Ref) {
  alloc_native_fn_slot(h, function_proto, value.Host(impl), name, arity)
}

/// ES2024 §20.2.2: Function name property — non-writable, non-enumerable, configurable.
///
/// seq: 1 (constant) — "length", "name" and "prototype" exist from
/// function-object birth, before any other named key can be created, so the
/// constant triple 0 ("length") < 1 ("name") < 2 ("prototype") gives the spec
/// §10.1.11 order (SetFunctionLength before SetFunctionName before
/// MakeConstructor) without paying three global counter reads per function
/// allocation (hot: every closure creation).
///
/// These constants live in a RESERVED range 0..15 that `next_prop_seq()` sits
/// strictly above (arc_vm_ffi.erl adds a +16 offset to the counter), so
/// "birth-time key < any later key" holds regardless of runtime state.
/// Redefinition paths preserve an existing key's seq, so a later
/// delete + re-add still moves the key to the end via a real counter value.
pub fn fn_name_property(name: String) -> Property {
  value.DataProperty(
    value: JsString(name),
    writable: False,
    enumerable: False,
    configurable: True,
    seq: 1,
  )
}

/// ES2024 §20.2.2: Function length property — non-writable, non-enumerable, configurable.
/// seq: 0 (constant) — see fn_name_property.
pub fn fn_length_property(arity: Int) -> Property {
  value.DataProperty(
    value: value.from_int(arity),
    writable: False,
    enumerable: False,
    configurable: True,
    seq: 0,
  )
}

/// A built-in constructor's "prototype" property — { W:F, E:F, C:F }
/// (test262: built-ins/Function/prototype/S15.3.3.1_A1, _A3).
///
/// seq: 2 (constant) — see fn_name_property. Must NOT be `value.data`, whose
/// fresh counter seq would sort "prototype" AFTER whichever static methods
/// were allocated first, breaking Object.getOwnPropertyNames order.
pub fn fn_prototype_property(proto: Ref) -> Property {
  value.DataProperty(
    value: JsObject(proto),
    writable: False,
    enumerable: False,
    configurable: False,
    seq: 2,
  )
}

/// Allocate N native function objects from specs, returning builtin_property
/// entries. Replaces the identical fold duplicated across builtin modules.
///
/// Not a spec operation — internal helper that batch-allocates method
/// function objects for a prototype's property list.
pub fn alloc_methods(
  h: Heap(ctx, host),
  function_proto: Ref,
  specs: List(#(String, NativeFn, Int)),
) -> #(Heap(ctx, host), List(#(String, Property))) {
  list.fold(specs, #(h, []), fn(acc, spec) {
    let #(h, props) = acc
    let #(name, native, arity) = spec
    let #(h, fn_ref) = alloc_native_fn(h, function_proto, native, name, arity)
    #(h, [#(name, value.builtin_property(JsObject(fn_ref))), ..props])
  })
}

/// Allocate N *host* function objects from `(name, arity, impl)` specs,
/// returning builtin_property entries — the host-closure counterpart to
/// `alloc_methods`. Shared by `engine.define_namespace` and
/// `beam.install_globals` so the "fold specs into named function properties"
/// dance lives in one place.
pub fn alloc_host_methods(
  h: Heap(ctx, host),
  function_proto: Ref,
  specs: List(
    #(
      String,
      Int,
      fn(List(JsValue), JsValue, ctx) -> #(ctx, Result(JsValue, JsValue)),
    ),
  ),
) -> #(Heap(ctx, host), List(#(String, Property))) {
  list.fold(specs, #(h, []), fn(acc, spec) {
    let #(h, props) = acc
    let #(name, arity, impl) = spec
    let #(h, fn_ref) = alloc_host_fn(h, function_proto, impl, name, arity)
    #(h, [#(name, value.builtin_property(JsObject(fn_ref))), ..props])
  })
}

/// Allocate N getter function objects from specs, returning get-only
/// AccessorProperty entries (non-enumerable, configurable). Mirrors alloc_methods.
pub fn alloc_getters(
  h: Heap(ctx, host),
  function_proto: Ref,
  specs: List(#(String, NativeFn)),
) -> #(Heap(ctx, host), List(#(String, Property))) {
  list.fold(specs, #(h, []), fn(acc, spec) {
    let #(h, props) = acc
    let #(name, native) = spec
    let #(h, fn_ref) =
      alloc_native_fn(h, function_proto, native, "get " <> name, 0)
    let prop =
      value.accessor(
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
  h: Heap(ctx, host),
  function_proto: Ref,
  get: NativeFn,
  set: NativeFn,
  name: String,
) -> #(Heap(ctx, host), Property) {
  let #(h, get_ref) = alloc_native_fn(h, function_proto, get, "get " <> name, 0)
  let #(h, set_ref) = alloc_native_fn(h, function_proto, set, "set " <> name, 1)
  #(
    h,
    value.accessor(
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
  h: Heap(ctx, host),
  function_proto: Ref,
  specs: List(#(String, CallNativeFn, Int)),
) -> #(Heap(ctx, host), List(#(String, Property))) {
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
    // §20.2.3 etc.: a built-in constructor's "prototype" property is
    // non-writable, non-enumerable, non-configurable.
    #("prototype", fn_prototype_property(proto)),
    #("length", fn_length_property(arity)),
    #("name", fn_name_property(name)),
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

/// Full proto-ctor cycle for a new builtin type using forward references.
///
/// Not a spec operation — internal bootstrap helper.
/// Reserves the proto ref first, then allocates both objects in one pass —
/// no read-modify-write. Both proto and constructor are written exactly once.
/// This is the common case for most builtins.
///
/// Two DIFFERENT prototypes go in here, and mixing them up is a silent
/// spec violation:
///   * `parent_proto` — the *prototype object's* [[Prototype]], i.e. what
///     instances inherit from beyond their own proto (%Object.prototype% for
///     most builtins, %Error.prototype% for the NativeErrors).
///   * `ctor_parent`  — the *constructor object's* [[Prototype]], i.e. what
///     `Object.getPrototypeOf(Ctor)` returns. %Function.prototype% for most
///     builtins, but §20.5.6.2 requires %Error% for every NativeError, so
///     error.gleam passes the Error constructor here.
pub fn init_type(
  h: Heap(ctx, host),
  parent_proto: Ref,
  ctor_parent: Ref,
  proto_props: List(#(String, Property)),
  ctor_fn: fn(Ref) -> NativeFnSlot(ctx),
  name: String,
  arity: Int,
  ctor_props: List(#(String, Property)),
) -> #(Heap(ctx, host), BuiltinType) {
  // Reserve proto address — no data written yet
  let #(h, proto_ref) = heap.reserve(h)
  let h = heap.root(h, proto_ref)

  // Allocate constructor — proto_ref is already known via forward reference
  let #(h, ctor_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        // init_type creates a constructor intrinsic — it has [[Construct]].
        kind: NativeFunction(ctor_fn(proto_ref), constructible: True),
        properties: named_props(ctor_properties(
          proto_ref,
          name,
          arity,
          ctor_props,
        )),
        elements: elements.new(),
        prototype: Some(ctor_parent),
        symbol_properties: [],
        extensible: True,
      ),
    )
  let h = heap.root(h, ctor_ref)

  // Fill reserved proto — ctor_ref is now known, single write with all properties
  let h =
    heap.write(
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

/// A *primitive wrapper* type (Boolean §20.3.3, Number §21.1.3, String
/// §22.1.3). Identical to `init_type`, except the caller must also name the
/// value of the internal data slot the spec puts on the PROTOTYPE object
/// itself — [[BooleanData]] false, [[NumberData]] +0, [[StringData]] "".
///
/// That slot is what makes `Number.prototype.valueOf()` return 0 instead of
/// throwing, and it is easy to forget: with plain `init_type` it is a second,
/// separate `heap.update_kind` call that nothing forces you to make. Routing
/// wrappers through here makes "registered a wrapper prototype without its
/// data slot" a missing-argument compile error.
pub fn init_wrapper_type(
  h: Heap(ctx, host),
  parent_proto: Ref,
  ctor_parent: Ref,
  proto_props: List(#(String, Property)),
  ctor_fn: fn(Ref) -> NativeFnSlot(ctx),
  name: String,
  arity: Int,
  ctor_props: List(#(String, Property)),
  proto_kind proto_kind: ExoticKind(ctx, host),
) -> #(Heap(ctx, host), BuiltinType) {
  let #(h, bt) =
    init_type(
      h,
      parent_proto,
      ctor_parent,
      proto_props,
      ctor_fn,
      name,
      arity,
      ctor_props,
    )
  #(heap.update_kind(h, bt.prototype, proto_kind), bt)
}

/// Shared init scaffold for keyed collections (Map/Set): allocates the
/// prototype methods, one iterator function installed under its own name plus
/// any `iter_aliases` AND [@@iterator] (all the SAME function object —
/// test262 asserts strict equality), a `size` getter, then runs the standard
/// init_type + @@toStringTag wiring.
pub fn init_keyed_collection(
  h: Heap(ctx, host),
  object_proto: Ref,
  function_proto: Ref,
  methods: List(#(String, NativeFn, Int)),
  iter_name: String,
  iter_native: NativeFn,
  iter_aliases: List(String),
  size_getter: NativeFn,
  ctor_fn: fn(Ref) -> NativeFnSlot(ctx),
  tag: String,
) -> #(Heap(ctx, host), BuiltinType) {
  let #(h, proto_methods) = alloc_methods(h, function_proto, methods)
  // Iterator fn allocated separately so all its property names (and
  // [@@iterator]) alias the SAME function object.
  let #(h, iter_ref) =
    alloc_native_fn(h, function_proto, iter_native, iter_name, 0)
  let iter_prop = value.builtin_property(JsObject(iter_ref))
  // Each named alias gets its OWN seq (value.restamp keeps the function-object
  // identity): two keys sharing one Property record is an enumeration-order
  // tie broken by map iteration order, not by creation order.
  let iter_props = [
    #(iter_name, iter_prop),
    ..list.map(iter_aliases, fn(n) { #(n, value.restamp(iter_prop)) })
  ]
  // size accessor property (getter, no setter)
  let #(h, getters) = alloc_getters(h, function_proto, [#("size", size_getter)])
  let proto_props = list.flatten([getters, iter_props, proto_methods])
  let #(h, bt) =
    init_type(h, object_proto, function_proto, proto_props, ctor_fn, tag, 0, [])
  // @@toStringTag = tag { writable: false, enumerable: false, configurable: true }
  let h = add_to_string_tag(h, bt.prototype, tag)
  // [@@iterator] — same function object as the named iterator method
  let h = add_symbol_property(h, bt.prototype, value.symbol_iterator, iter_prop)
  #(h, bt)
}

/// Add a named property to an existing object (typically a prototype).
/// Used after init_type / alloc to backpatch e.g. `constructor`.
///
/// `ref` must be a live object: builtins only ever backpatch refs they just
/// allocated, so a missing or non-ObjectSlot slot is a wiring bug — crash at
/// the desync rather than silently dropping the property.
pub fn add_named_property(
  h: Heap(ctx, host),
  ref: Ref,
  name: String,
  prop: Property,
) -> Heap(ctx, host) {
  let assert Some(ObjectSlot(properties:, ..) as slot) = heap.read(h, ref)
  heap.write(
    h,
    ref,
    ObjectSlot(..slot, properties: dict.insert(properties, Named(name), prop)),
  )
}

/// Add a symbol-keyed property to an existing object (typically a prototype).
/// Used after init_type to wire up Symbol.iterator, Symbol.toStringTag, etc.
///
/// Same invariant as `add_named_property`: the ref is always an object the
/// caller just allocated, so anything else is a wiring bug and must crash.
pub fn add_symbol_property(
  h: Heap(ctx, host),
  ref: Ref,
  symbol: value.SymbolId,
  prop: Property,
) -> Heap(ctx, host) {
  let assert Some(ObjectSlot(symbol_properties:, ..) as slot) =
    heap.read(h, ref)
  heap.write(
    h,
    ref,
    ObjectSlot(
      ..slot,
      symbol_properties: list.key_set(symbol_properties, symbol, prop),
    ),
  )
}

/// @@toStringTag symbol-property pair: { writable: false, enumerable: false, configurable: true }.
pub fn to_string_tag(name: String) -> #(value.SymbolId, Property) {
  #(
    value.symbol_to_string_tag,
    value.data(JsString(name)) |> value.configurable(),
  )
}

/// Add @@toStringTag = name to an existing object (typically a prototype).
pub fn add_to_string_tag(
  h: Heap(ctx, host),
  ref: Ref,
  name: String,
) -> Heap(ctx, host) {
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
  h: Heap(ctx, host),
  proto: Ref,
  function_proto: Ref,
  proto_props: List(#(String, Property)),
  ctor_fn: fn(Ref) -> NativeFnSlot(ctx),
  name: String,
  arity: Int,
  ctor_props: List(#(String, Property)),
  // Whether the created function has [[Construct]]. True for real constructors
  // (Object, Function); False for abstract/non-constructor types (Iterator).
  constructible: Bool,
) -> #(Heap(ctx, host), BuiltinType) {
  // Allocate constructor — proto ref already known
  let #(h, ctor_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(ctor_fn(proto), constructible:),
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
  h: Heap(ctx, host),
  proto: Ref,
  message: String,
) -> #(Heap(ctx, host), JsValue) {
  let #(h, ref) =
    heap.alloc(
      h,
      ObjectSlot(
        // [[ErrorData]] internal slot; the stack string is filled in by
        // state.attach_stack and surfaced via the Error.prototype.stack
        // accessor (error-stack-accessor proposal) — NOT an own property.
        kind: value.ErrorObject(stack: ""),
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
  h: Heap(ctx, host),
  b: Builtins,
  message: String,
) -> #(Heap(ctx, host), JsValue) {
  alloc_error(h, b.type_error.prototype, message)
}

/// ES2024 §20.5.6.1.1 NativeError ( message [ , options ] )
/// Allocates a RangeError instance. See alloc_error for spec step details.
pub fn make_range_error(
  h: Heap(ctx, host),
  b: Builtins,
  message: String,
) -> #(Heap(ctx, host), JsValue) {
  alloc_error(h, b.range_error.prototype, message)
}

/// ES2024 §20.5.6.1.1 NativeError ( message [ , options ] )
/// Allocates a ReferenceError instance. See alloc_error for spec step details.
pub fn make_reference_error(
  h: Heap(ctx, host),
  b: Builtins,
  message: String,
) -> #(Heap(ctx, host), JsValue) {
  alloc_error(h, b.reference_error.prototype, message)
}

/// ES2024 §20.5.6.1.1 NativeError ( message [ , options ] )
/// Allocates a SyntaxError instance. See alloc_error for spec step details.
pub fn make_syntax_error(
  h: Heap(ctx, host),
  b: Builtins,
  message: String,
) -> #(Heap(ctx, host), JsValue) {
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
/// and this function already receives Builtins. `None` has exactly one
/// meaning: the argument was undefined or null (Table 15 rows 1-2).
///
/// TODO(Deviation): SymbolObject uses Object.prototype instead of Symbol.prototype
///   (no dedicated Symbol.prototype with toString/valueOf/description yet).
pub fn to_object(
  h: Heap(ctx, host),
  b: Builtins,
  val: JsValue,
) -> Option(#(Heap(ctx, host), Ref)) {
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
    value.JsSymbol(sym) ->
      Some(alloc_wrapper(h, value.SymbolObject(sym), b.symbol_proto))
    // Table 15 row 7: BigInt → new BigInt object with [[BigIntData]]
    value.JsBigInt(bv) ->
      Some(alloc_wrapper(h, value.BigIntObject(bv), b.bigint.prototype))
    // Internal: the TDZ sentinel is not a JS value — reaching ToObject with it
    // is an engine bug (frame.gleam asserts the same invariant), not the spec's
    // "argument is undefined or null" TypeError that `None` means.
    value.JsUninitialized ->
      panic as "to_object: TDZ sentinel escaped into a JS value position"
  }
}

/// Helper for ToObject (§7.1.18) and primitive-wrapper constructors
/// (`new String/Number/Boolean`): allocate a wrapper object for a primitive.
///
/// Creates an ordinary object with the given ExoticKind (which carries the
/// type-specific internal slot — [[StringData]] / [[NumberData]] /
/// [[BooleanData]] etc.) and the appropriate builtin prototype.
pub fn alloc_wrapper(
  h: Heap(ctx, host),
  kind: ExoticKind(ctx, host),
  proto: Ref,
) -> #(Heap(ctx, host), Ref) {
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
  h: Heap(ctx, host),
  values: List(JsValue),
  array_proto: Ref,
) -> #(Heap(ctx, host), Ref) {
  // Step 1: length = number of values; steps 4-8 in alloc_array_from_elements
  alloc_array_from_elements(
    h,
    elements.from_list(values),
    list.length(values),
    array_proto,
  )
}

/// Allocate a JS array exotic object from pre-built elements with an explicit
/// length. Like alloc_array (ArrayCreate, §10.4.2.2) but takes JsElements
/// directly, so the length may exceed the stored element count (holes).
pub fn alloc_array_from_elements(
  h: Heap(ctx, host),
  els: JsElements,
  length: Int,
  array_proto: Ref,
) -> #(Heap(ctx, host), Ref) {
  heap.alloc(
    h,
    ObjectSlot(
      // Step 6: exotic [[DefineOwnProperty]] via ArrayObject kind
      // Step 7: length stored in ArrayObject(length)
      kind: ArrayObject(length),
      properties: dict.new(),
      elements: els,
      // Step 5: [[Prototype]] = proto
      prototype: Some(array_proto),
      symbol_properties: [],
      // Step 4: [[Extensible]] = true
      extensible: True,
    ),
  )
}
