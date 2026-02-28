import arc/vm/array.{type Array}
import arc/vm/opcode.{type Op}
import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

/// A reference to a heap slot. Public so heap.gleam can construct/destructure.
pub type Ref {
  Ref(id: Int)
}

/// Unique symbol identity. Not heap-allocated — symbols are value types on BEAM.
/// An opaque Erlang reference. Globally unique across the entire BEAM cluster.
/// Created via make_ref() FFI — no two calls ever return the same value.
pub type ErlangRef

/// Symbol identity. Well-known symbols use fixed integer IDs (compile-time
/// constants). User-created symbols use Erlang references for global uniqueness
/// across processes — no shared counter needed.
pub type SymbolId {
  WellKnownSymbol(id: Int)
  UserSymbol(ref: ErlangRef)
}

// Well-known symbol constants.
pub const symbol_to_string_tag = WellKnownSymbol(1)

pub const symbol_iterator = WellKnownSymbol(2)

pub const symbol_has_instance = WellKnownSymbol(3)

pub const symbol_is_concat_spreadable = WellKnownSymbol(4)

pub const symbol_to_primitive = WellKnownSymbol(5)

pub const symbol_species = WellKnownSymbol(6)

pub const symbol_async_iterator = WellKnownSymbol(7)

/// Get the description string for a well-known symbol.
pub fn well_known_symbol_description(id: SymbolId) -> Option(String) {
  case id {
    WellKnownSymbol(1) -> Some("Symbol.toStringTag")
    WellKnownSymbol(2) -> Some("Symbol.iterator")
    WellKnownSymbol(3) -> Some("Symbol.hasInstance")
    WellKnownSymbol(4) -> Some("Symbol.isConcatSpreadable")
    WellKnownSymbol(5) -> Some("Symbol.toPrimitive")
    WellKnownSymbol(6) -> Some("Symbol.species")
    WellKnownSymbol(7) -> Some("Symbol.asyncIterator")
    _ -> None
  }
}

/// Wrapper around BEAM's native arbitrary-precision integer.
pub type BigInt {
  BigInt(value: Int)
}

/// Compiled function definition. Stored directly on FunctionObject
/// per ES spec §10.2 (ordinary function objects carry [[ECMAScriptCode]]).
pub type FuncTemplate {
  FuncTemplate(
    name: Option(String),
    arity: Int,
    local_count: Int,
    bytecode: Array(Op),
    constants: Array(JsValue),
    functions: Array(FuncTemplate),
    env_descriptors: List(EnvCapture),
    is_strict: Bool,
    is_arrow: Bool,
    is_derived_constructor: Bool,
    is_generator: Bool,
    is_async: Bool,
  )
}

/// Describes how to capture one variable from the enclosing scope
/// when creating a closure.
pub type EnvCapture {
  /// Capture from parent's local frame at the given index.
  CaptureLocal(parent_index: Int)
  /// Capture from parent's EnvSlot at the given index (transitive).
  CaptureEnv(parent_env_index: Int)
}

/// An opaque Erlang process identifier. Only created/consumed via FFI.
pub type ErlangPid

/// A serializable message that can be sent between BEAM processes.
/// Materializes heap-allocated structures (objects, arrays) into
/// self-contained values that don't reference any specific VM heap.
pub type PortableMessage {
  PmUndefined
  PmNull
  PmBool(Bool)
  PmNumber(JsNum)
  PmString(String)
  PmBigInt(BigInt)
  PmArray(List(PortableMessage))
  PmObject(
    properties: List(#(String, PortableMessage)),
    symbol_properties: List(#(SymbolId, PortableMessage)),
  )
  PmPid(ErlangPid)
  PmSymbol(SymbolId)
}

/// JS number representation. BEAM floats can't represent NaN or Infinity,
/// so we use an explicit tagged type.
pub type JsNum {
  Finite(Float)
  NaN
  Infinity
  NegInfinity
}

/// Stack values — the things that live on the VM stack or inside object properties.
/// BEAM manages their lifecycle automatically, no GC involvement needed.
///
/// Everything heap-allocated is JsObject(Ref). The heap slot's `kind` tag
/// distinguishes ordinary objects, arrays, and functions. `typeof` reads the
/// heap to tell "function" from "object".
pub type JsValue {
  JsUndefined
  JsNull
  JsBool(Bool)
  JsNumber(JsNum)
  JsString(String)
  JsObject(Ref)
  JsSymbol(SymbolId)
  JsBigInt(BigInt)
  /// Internal sentinel for Temporal Dead Zone. Never exposed to JS code.
  /// GetLocal/GetEnvVar throw ReferenceError when they encounter this.
  JsUninitialized
}

/// Dual-representation JS array elements.
///
/// Dense: Erlang tuple — O(1) get, O(n) set (single BIF). For normal arrays.
/// Sparse: Dict — O(log n) get/set. For arrays with huge gaps (e.g. `a[100000] = 1`).
///
/// Operations on this type are in `arc/vm/js_elements`.
pub type JsElements {
  DenseElements(data: Array(JsValue))
  SparseElements(data: Dict(Int, JsValue))
}

/// Callback for ToString that supports ToPrimitive (VM re-entry for objects).
/// Builtins receive this from the dispatch layer to break the vm↔builtins cycle.
/// Identifies a built-in native function. Used by `NativeFunction` to dispatch
/// to the correct Gleam implementation when called from JS.
pub type NativeFn {
  NativeObjectConstructor
  NativeFunctionConstructor
  NativeArrayConstructor
  NativeArrayIsArray
  NativeErrorConstructor(proto: Ref)
  NativeFunctionCall
  NativeFunctionApply
  NativeFunctionBind
  /// A bound function created by Function.prototype.bind.
  /// When called, prepends bound_args to the call args and uses bound_this.
  NativeBoundFunction(
    target: Ref,
    bound_this: JsValue,
    bound_args: List(JsValue),
  )
  // Object static methods
  NativeObjectGetOwnPropertyDescriptor
  NativeObjectDefineProperty
  NativeObjectDefineProperties
  NativeObjectGetOwnPropertyNames
  NativeObjectKeys
  NativeObjectValues
  NativeObjectEntries
  NativeObjectCreate
  NativeObjectAssign
  NativeObjectIs
  NativeObjectHasOwn
  NativeObjectGetPrototypeOf
  NativeObjectSetPrototypeOf
  NativeObjectFreeze
  NativeObjectIsFrozen
  NativeObjectIsExtensible
  NativeObjectPreventExtensions
  // Object.prototype instance methods
  NativeObjectPrototypeHasOwnProperty
  NativeObjectPrototypePropertyIsEnumerable
  // Array.prototype instance methods
  NativeArrayPrototypeJoin
  NativeArrayPrototypePush
  NativeArrayPrototypePop
  NativeArrayPrototypeShift
  NativeArrayPrototypeUnshift
  NativeArrayPrototypeSlice
  NativeArrayPrototypeConcat
  NativeArrayPrototypeReverse
  NativeArrayPrototypeFill
  NativeArrayPrototypeAt
  NativeArrayPrototypeIndexOf
  NativeArrayPrototypeLastIndexOf
  NativeArrayPrototypeIncludes
  NativeArrayPrototypeForEach
  NativeArrayPrototypeMap
  NativeArrayPrototypeFilter
  NativeArrayPrototypeReduce
  NativeArrayPrototypeReduceRight
  NativeArrayPrototypeEvery
  NativeArrayPrototypeSome
  NativeArrayPrototypeFind
  NativeArrayPrototypeFindIndex
  // Math methods
  NativeMathPow
  NativeMathAbs
  NativeMathFloor
  NativeMathCeil
  NativeMathRound
  NativeMathTrunc
  NativeMathSqrt
  NativeMathMax
  NativeMathMin
  NativeMathLog
  NativeMathSin
  NativeMathCos
  // String.prototype methods
  NativeStringPrototypeCharAt
  NativeStringPrototypeCharCodeAt
  NativeStringPrototypeIndexOf
  NativeStringPrototypeLastIndexOf
  NativeStringPrototypeIncludes
  NativeStringPrototypeStartsWith
  NativeStringPrototypeEndsWith
  NativeStringPrototypeSlice
  NativeStringPrototypeSubstring
  NativeStringPrototypeToLowerCase
  NativeStringPrototypeToUpperCase
  NativeStringPrototypeTrim
  NativeStringPrototypeTrimStart
  NativeStringPrototypeTrimEnd
  NativeStringPrototypeSplit
  NativeStringPrototypeConcat
  NativeStringPrototypeToString
  NativeStringPrototypeValueOf
  NativeStringPrototypeRepeat
  NativeStringPrototypePadStart
  NativeStringPrototypePadEnd
  NativeStringPrototypeAt
  // String/Number/Boolean constructors (type coercion functions)
  NativeStringConstructor
  NativeNumberConstructor
  NativeBooleanConstructor
  // Global utility functions (these coerce via ToNumber first)
  NativeParseInt
  NativeParseFloat
  NativeIsNaN
  NativeIsFinite
  // Number static methods (strict — NO coercion)
  NativeNumberIsNaN
  NativeNumberIsFinite
  NativeNumberIsInteger
  NativeNumberParseInt
  NativeNumberParseFloat
  // Number.prototype methods — unwrap [[NumberData]] (thisNumberValue)
  NativeNumberPrototypeValueOf
  NativeNumberPrototypeToString
  // Boolean.prototype methods — unwrap [[BooleanData]] (thisBooleanValue)
  NativeBooleanPrototypeValueOf
  NativeBooleanPrototypeToString
  // Promise
  NativePromiseConstructor
  NativePromiseThen
  NativePromiseCatch
  NativePromiseFinally
  NativePromiseResolveStatic
  NativePromiseRejectStatic
  /// Internal resolve function created by CreateResolvingFunctions.
  /// Carries refs to the promise object, promise data slot, and shared
  /// already-resolved BoxSlot.
  NativePromiseResolveFunction(
    promise_ref: Ref,
    data_ref: Ref,
    already_resolved_ref: Ref,
  )
  /// Internal reject function created by CreateResolvingFunctions.
  NativePromiseRejectFunction(
    promise_ref: Ref,
    data_ref: Ref,
    already_resolved_ref: Ref,
  )
  /// Promise.prototype.finally wrapper: called on fulfill, calls onFinally()
  /// then returns a promise that resolves to the original value.
  NativePromiseFinallyFulfill(on_finally: JsValue)
  /// Promise.prototype.finally wrapper: called on reject, calls onFinally()
  /// then returns a promise that rejects with the original reason.
  NativePromiseFinallyReject(on_finally: JsValue)
  /// Thunk that ignores its argument and returns the captured value.
  NativePromiseFinallyValueThunk(value: JsValue)
  /// Thunk that ignores its argument and throws the captured reason.
  NativePromiseFinallyThrower(reason: JsValue)
  // Generator
  NativeGeneratorNext
  NativeGeneratorReturn
  NativeGeneratorThrow
  /// Async function resume: called when awaited promise settles.
  /// async_data_ref points to AsyncFunctionSlot on heap.
  /// is_reject: False → fulfilled (push resolved value), True → rejected (throw value)
  NativeAsyncResume(async_data_ref: Ref, is_reject: Bool)
  /// Symbol() constructor — callable but NOT new-able.
  NativeSymbolConstructor
  /// Object.prototype.toString() — ES2024 §19.1.3.6.
  NativeObjectPrototypeToString
  /// Object.prototype.valueOf() — ES2024 §19.1.3.7.
  NativeObjectPrototypeValueOf
  /// %IteratorPrototype%[Symbol.iterator]() — returns `this`.
  NativeIteratorSymbolIterator
  /// Arc.peek(promise) — synchronously inspect promise state.
  NativeArcPeek
  /// Arc.spawn(fn) — spawn a new BEAM process running the given JS function.
  NativeArcSpawn
  /// Arc.send(pid, message) — send a message to a BEAM process.
  NativeArcSend
  /// Arc.receive(timeout?) — receive a message from the current process mailbox.
  NativeArcReceive
  /// Arc.self() — return the current BEAM process's PID.
  NativeArcSelf
  /// Arc.log(...args) — print values to stdout (like console.log).
  NativeArcLog
  /// Arc.sleep(ms) — suspend the current BEAM process for ms milliseconds.
  NativeArcSleep
  /// Pid.prototype.toString — returns "Pid<<0.83.0>>" style string.
  NativePidToString
}

/// Distinguishes the kind of object stored in a unified ObjectSlot.
pub type ExoticKind {
  /// Plain JS object: `{}`, `new Object()`, error instances, prototypes, etc.
  OrdinaryObject
  /// JS array: `[]`, `new Array()`. `length` is tracked explicitly.
  ArrayObject(length: Int)
  /// Arguments object — `arguments` inside a non-arrow function. Structurally
  /// identical to ArrayObject (indexed elements + tracked length), but per spec
  /// it's an ordinary object with Object.prototype, NOT an array:
  /// - Array.isArray(arguments) → false
  /// - Object.prototype.toString.call(arguments) → "[object Arguments]"
  /// We only implement unmapped arguments (indices independent of params),
  /// which is what strict mode and functions with complex params get per
  /// ES §10.4.4.6 CreateUnmappedArgumentsObject.
  ArgumentsObject(length: Int)
  /// JS function (closure). Per ES spec, a function object carries its
  /// [[ECMAScriptCode]] directly. `func_template` is the compiled bytecode,
  /// `env` points to the EnvSlot holding captured variables.
  FunctionObject(func_template: FuncTemplate, env: Ref)
  /// Built-in function implemented in Gleam, not bytecode.
  /// Callable like any function but dispatches to native code.
  NativeFunction(native: NativeFn)
  /// Promise object. The visible JS object has this kind, pointing to
  /// an internal PromiseSlot that holds state/reactions.
  PromiseObject(promise_data: Ref)
  /// Generator object. Points to a GeneratorSlot that holds suspended state.
  GeneratorObject(generator_data: Ref)
  /// Boxed String primitive (`new String("x")`, `Object("x")`, or sloppy-mode
  /// this-boxing). Has [[StringData]] internal slot. Per spec §10.4.3 this is
  /// an exotic object with own index properties and `length`; we expose those
  /// virtually via the ExoticKind payload rather than materialising them on
  /// the properties dict.
  StringObject(value: String)
  /// Boxed Number primitive (`new Number(42)`, etc.). Has [[NumberData]].
  /// Ordinary object aside from the internal slot — no own properties.
  NumberObject(value: JsNum)
  /// Boxed Boolean primitive (`new Boolean(true)`, etc.). Has [[BooleanData]].
  BooleanObject(value: Bool)
  /// Boxed Symbol (`Object(sym)` only; `new Symbol()` is a TypeError).
  /// Has [[SymbolData]]. Ordinary object aside from the internal slot.
  SymbolObject(value: SymbolId)
  /// Erlang PID wrapper for Arc.spawn/self. Contains an opaque BEAM process
  /// identifier that can be used with Arc.send.
  PidObject(pid: ErlangPid)
}

/// Property descriptor — writable/enumerable/configurable flags per property.
/// Following QuickJS: bit-flags on every property. No accessor properties yet.
pub type Property {
  DataProperty(
    value: JsValue,
    writable: Bool,
    enumerable: Bool,
    configurable: Bool,
  )
  AccessorProperty(
    get: Option(JsValue),
    set: Option(JsValue),
    enumerable: Bool,
    configurable: Bool,
  )
}

/// Base builder: DataProperty with all flags False.
pub fn data(val: JsValue) -> Property {
  DataProperty(
    value: val,
    writable: False,
    enumerable: False,
    configurable: False,
  )
}

/// Set writable to True (data properties only).
pub fn writable(prop: Property) -> Property {
  case prop {
    DataProperty(value:, enumerable:, configurable:, ..) ->
      DataProperty(value:, writable: True, enumerable:, configurable:)
    AccessorProperty(..) -> panic as "Accessor property cannot be made writable"
  }
}

/// Set enumerable to True.
pub fn enumerable(prop: Property) -> Property {
  case prop {
    DataProperty(value:, writable:, configurable:, ..) ->
      DataProperty(value:, writable:, enumerable: True, configurable:)
    AccessorProperty(get:, set:, configurable:, ..) ->
      AccessorProperty(get:, set:, enumerable: True, configurable:)
  }
}

/// Set configurable to True.
pub fn configurable(prop: Property) -> Property {
  case prop {
    DataProperty(value:, writable:, enumerable:, ..) ->
      DataProperty(value:, writable:, enumerable:, configurable: True)
    AccessorProperty(get:, set:, enumerable:, ..) ->
      AccessorProperty(get:, set:, enumerable:, configurable: True)
  }
}

/// Normal assignment: all flags true (obj.x = val, object literals, etc.)
pub fn data_property(val: JsValue) -> Property {
  data(val) |> writable() |> enumerable() |> configurable()
}

/// Built-in methods/prototype props: writable+configurable, NOT enumerable.
/// This matches QuickJS and the spec for built-in function properties.
pub fn builtin_property(val: JsValue) -> Property {
  data(val) |> writable() |> configurable()
}

/// GC root tracing: extract heap refs reachable from a Property
/// (data value or accessor get/set slots).
fn refs_in_property(prop: Property) -> List(Ref) {
  case prop {
    DataProperty(value:, ..) -> refs_in_value(value)
    AccessorProperty(get:, set:, ..) -> {
      let g = case get {
        Some(v) -> refs_in_value(v)
        None -> []
      }
      let s = case set {
        Some(v) -> refs_in_value(v)
        None -> []
      }
      list.append(g, s)
    }
  }
}

/// A microtask job for the promise job queue.
pub type Job {
  /// Call handler(arg), then resolve/reject the child promise.
  PromiseReactionJob(
    handler: JsValue,
    arg: JsValue,
    resolve: JsValue,
    reject: JsValue,
  )
  /// Call thenable.then(resolve, reject) to assimilate a thenable.
  PromiseResolveThenableJob(
    thenable: JsValue,
    then_fn: JsValue,
    resolve: JsValue,
    reject: JsValue,
  )
}

/// Internal promise state (pending/fulfilled/rejected).
pub type PromiseState {
  PromisePending
  PromiseFulfilled(value: JsValue)
  PromiseRejected(reason: JsValue)
}

/// A stored reaction waiting for promise settlement.
pub type PromiseReaction {
  PromiseReaction(
    child_resolve: JsValue,
    child_reject: JsValue,
    handler: JsValue,
  )
}

/// Saved try-frame for generator suspension (mirrors TryFrame from frame.gleam).
pub type SavedTryFrame {
  SavedTryFrame(catch_target: Int, stack_depth: Int)
}

/// Saved finally-completion for generator suspension (mirrors FinallyCompletion).
pub type SavedFinallyCompletion {
  SavedNormalCompletion
  SavedThrowCompletion(value: JsValue)
  SavedReturnCompletion(value: JsValue)
}

/// Generator internal lifecycle state.
pub type GeneratorState {
  /// Created but body not yet entered (before first .next())
  SuspendedStart
  /// Paused at a yield point
  SuspendedYield
  /// Currently executing (re-entrant .next() on a running generator)
  Executing
  /// Finished (returned or threw)
  Completed
}

/// What lives in a heap slot.
pub type HeapSlot {
  /// Unified object slot — covers ordinary objects, arrays, and functions.
  /// - `properties`: string-keyed own properties with descriptor flags
  /// - `elements`: integer-keyed elements (primarily for ArrayObject, bare values)
  /// - `prototype`: link for prototype chain traversal
  ObjectSlot(
    kind: ExoticKind,
    properties: Dict(String, Property),
    elements: JsElements,
    prototype: Option(Ref),
    symbol_properties: Dict(SymbolId, Property),
    extensible: Bool,
  )
  /// Flat environment frame. Multiple closures in the same scope reference
  /// the same EnvSlot, so mutations to captured variables are visible across them.
  /// Compiler flattens the scope chain — no parent pointer, all captures are direct.
  /// Mutable captures stored as JsObject(box_ref) pointing to a BoxSlot.
  EnvSlot(slots: List(JsValue))
  /// Mutable variable cell for closure captures. When a variable is both captured
  /// by a closure AND mutated, both the local frame and EnvSlot hold a Ref to
  /// the same BoxSlot. Reads/writes go through this indirection.
  BoxSlot(value: JsValue)
  /// Iterator state for for-in loops. Eagerly snapshots enumerable keys
  /// upfront (per spec: prototype shadowing requires full collection).
  /// Stores pre-collected string keys as JsString values.
  ForInIteratorSlot(keys: List(JsValue))
  /// Iterator state for for-of loops over arrays. Lazy — holds a ref to
  /// the source array and reads elements one at a time (like QuickJS).
  /// Re-reads length each iteration to handle mutations during iteration.
  ArrayIteratorSlot(source: Ref, index: Int)
  /// Engine-internal promise state, separate from the JS-visible ObjectSlot.
  /// A promise needs both a normal object (for properties, prototype chain,
  /// .then/.catch lookup) AND internal state (pending/fulfilled/rejected,
  /// reaction queues) that must NOT be visible as JS properties. The ObjectSlot
  /// has `kind: PromiseObject(promise_data: Ref)` pointing here. Same approach
  /// as QuickJS's separate JSPromiseData.
  PromiseSlot(
    state: PromiseState,
    fulfill_reactions: List(PromiseReaction),
    reject_reactions: List(PromiseReaction),
    is_handled: Bool,
  )
  /// Engine-internal generator suspended state. The ObjectSlot has
  /// `kind: GeneratorObject(generator_data: Ref)` pointing here.
  /// Saves the full execution context so .next() can resume.
  GeneratorSlot(
    gen_state: GeneratorState,
    func_template: FuncTemplate,
    env_ref: Ref,
    saved_pc: Int,
    saved_locals: Array(JsValue),
    saved_stack: List(JsValue),
    saved_try_stack: List(SavedTryFrame),
    saved_finally_stack: List(SavedFinallyCompletion),
    saved_this: JsValue,
    saved_callee_ref: Option(Ref),
  )
  /// Engine-internal async function suspended state.
  /// Saves the full execution context so await can resume.
  AsyncFunctionSlot(
    promise_data_ref: Ref,
    resolve: JsValue,
    reject: JsValue,
    func_template: FuncTemplate,
    env_ref: Ref,
    saved_pc: Int,
    saved_locals: Array(JsValue),
    saved_stack: List(JsValue),
    saved_try_stack: List(SavedTryFrame),
    saved_finally_stack: List(SavedFinallyCompletion),
    saved_this: JsValue,
    saved_callee_ref: Option(Ref),
  )
}

/// GC root tracing: extract heap refs from a single JsValue.
/// Only JsObject carries heap refs; all primitives return [].
fn refs_in_value(value: JsValue) -> List(Ref) {
  case value {
    JsObject(ref) -> [ref]
    JsUndefined
    | JsNull
    | JsBool(_)
    | JsNumber(_)
    | JsString(_)
    | JsSymbol(_)
    | JsBigInt(_)
    | JsUninitialized -> []
  }
}

/// Extract all refs reachable from a heap slot by walking its JsValues.
pub fn refs_in_slot(slot: HeapSlot) -> List(Ref) {
  case slot {
    ObjectSlot(
      kind:,
      properties:,
      elements:,
      prototype:,
      symbol_properties:,
      extensible: _,
    ) -> {
      let prop_refs =
        dict.values(properties)
        |> list.flat_map(refs_in_property)
      let sym_prop_refs =
        dict.values(symbol_properties)
        |> list.flat_map(refs_in_property)
      let elem_refs = case elements {
        DenseElements(data) ->
          array.to_list(data) |> list.flat_map(refs_in_value)
        SparseElements(data) ->
          dict.values(data) |> list.flat_map(refs_in_value)
      }
      let proto_refs = case prototype {
        Some(ref) -> [ref]
        None -> []
      }
      let kind_refs = case kind {
        FunctionObject(env: env_ref, func_template: _) -> [env_ref]
        NativeFunction(NativeErrorConstructor(proto: ref)) -> [ref]
        NativeFunction(NativeBoundFunction(target:, bound_this:, bound_args:)) -> [
          target,
          ..list.flatten([
            refs_in_value(bound_this),
            list.flat_map(bound_args, refs_in_value),
          ])
        ]
        NativeFunction(NativePromiseResolveFunction(
          promise_ref:,
          data_ref:,
          already_resolved_ref:,
        )) -> [promise_ref, data_ref, already_resolved_ref]
        NativeFunction(NativePromiseRejectFunction(
          promise_ref:,
          data_ref:,
          already_resolved_ref:,
        )) -> [promise_ref, data_ref, already_resolved_ref]
        NativeFunction(NativePromiseFinallyFulfill(on_finally:)) ->
          refs_in_value(on_finally)
        NativeFunction(NativePromiseFinallyReject(on_finally:)) ->
          refs_in_value(on_finally)
        NativeFunction(NativePromiseFinallyValueThunk(value:)) ->
          refs_in_value(value)
        NativeFunction(NativePromiseFinallyThrower(reason:)) ->
          refs_in_value(reason)
        NativeFunction(NativeAsyncResume(async_data_ref:, ..)) -> [
          async_data_ref,
        ]
        PromiseObject(promise_data:) -> [promise_data]
        GeneratorObject(generator_data:) -> [generator_data]
        OrdinaryObject
        | ArrayObject(_)
        | ArgumentsObject(_)
        | NativeFunction(_)
        | StringObject(_)
        | NumberObject(_)
        | BooleanObject(_)
        | SymbolObject(_)
        | PidObject(_) -> []
      }
      list.flatten([prop_refs, sym_prop_refs, elem_refs, proto_refs, kind_refs])
    }
    EnvSlot(slots:) -> list.flat_map(slots, refs_in_value)
    BoxSlot(value:) -> refs_in_value(value)
    ForInIteratorSlot(keys:) -> list.flat_map(keys, refs_in_value)
    ArrayIteratorSlot(source:, ..) -> [source]
    PromiseSlot(state:, fulfill_reactions:, reject_reactions:, ..) -> {
      let state_refs = case state {
        PromiseFulfilled(value:) -> refs_in_value(value)
        PromiseRejected(reason:) -> refs_in_value(reason)
        PromisePending -> []
      }
      let reaction_refs = fn(reactions: List(PromiseReaction)) {
        list.flat_map(reactions, fn(r) {
          list.flatten([
            refs_in_value(r.child_resolve),
            refs_in_value(r.child_reject),
            refs_in_value(r.handler),
          ])
        })
      }
      list.flatten([
        state_refs,
        reaction_refs(fulfill_reactions),
        reaction_refs(reject_reactions),
      ])
    }
    GeneratorSlot(
      env_ref:,
      saved_locals:,
      saved_stack:,
      saved_finally_stack:,
      saved_this:,
      saved_callee_ref:,
      ..,
    ) -> {
      let finally_refs =
        list.flat_map(saved_finally_stack, fn(fc) {
          case fc {
            SavedThrowCompletion(value:) -> refs_in_value(value)
            SavedReturnCompletion(value:) -> refs_in_value(value)
            SavedNormalCompletion -> []
          }
        })
      list.flatten([
        [env_ref],
        array.to_list(saved_locals) |> list.flat_map(refs_in_value),
        list.flat_map(saved_stack, refs_in_value),
        finally_refs,
        refs_in_value(saved_this),
        option.map(saved_callee_ref, list.wrap) |> option.unwrap([]),
      ])
    }
    AsyncFunctionSlot(
      promise_data_ref:,
      resolve:,
      reject:,
      env_ref:,
      saved_locals:,
      saved_stack:,
      saved_finally_stack:,
      saved_this:,
      saved_callee_ref:,
      ..,
    ) -> {
      let finally_refs =
        list.flat_map(saved_finally_stack, fn(fc) {
          case fc {
            SavedThrowCompletion(value:) -> refs_in_value(value)
            SavedReturnCompletion(value:) -> refs_in_value(value)
            SavedNormalCompletion -> []
          }
        })
      list.flatten([
        [promise_data_ref],
        refs_in_value(resolve),
        refs_in_value(reject),
        [env_ref],
        array.to_list(saved_locals) |> list.flat_map(refs_in_value),
        list.flat_map(saved_stack, refs_in_value),
        finally_refs,
        refs_in_value(saved_this),
        option.map(saved_callee_ref, list.wrap) |> option.unwrap([]),
      ])
    }
  }
}

/// Format a JS number as a string. Integer-valued floats omit the decimal.
pub fn js_format_number(n: Float) -> String {
  // §6.1.6.1.20 Number::toString: -0 → "0"
  // BEAM =:= distinguishes -0.0 from 0.0, so normalize first.
  let n = n +. 0.0
  let truncated = float.truncate(n)
  case int.to_float(truncated) == n {
    True -> int.to_string(truncated)
    False -> float.to_string(n)
  }
}

/// JS ToBoolean: https://tc39.es/ecma262/#sec-toboolean
pub fn is_truthy(val: JsValue) -> Bool {
  case val {
    JsUndefined | JsNull | JsUninitialized -> False
    JsBool(b) -> b
    JsNumber(NaN) -> False
    JsNumber(Finite(n)) -> n != 0.0
    JsNumber(Infinity) | JsNumber(NegInfinity) -> True
    JsString(s) -> s != ""
    JsBigInt(BigInt(n)) -> n != 0
    JsObject(_) | JsSymbol(_) -> True
  }
}

/// Truncate a JS float to integer. Handles negatives correctly
/// (truncates toward zero, matching JS `Math.trunc` / `ToInt32` semantics).
pub fn float_to_int(f: Float) -> Int {
  case f <. 0.0 {
    True -> 0 - float.truncate(float.negate(f))
    False -> float.truncate(f)
  }
}

/// JS === (IsStrictlyEqual). NaN !== NaN; +0 === -0.
/// BEAM's =:= distinguishes ±0, so we normalize by adding 0.0 before comparing
/// (IEEE 754: -0.0 + 0.0 = +0.0).
pub fn strict_equal(left: JsValue, right: JsValue) -> Bool {
  case left, right {
    JsUndefined, JsUndefined -> True
    JsNull, JsNull -> True
    JsBool(a), JsBool(b) -> a == b
    // NaN !== NaN
    JsNumber(NaN), _ | _, JsNumber(NaN) -> False
    // +0 === -0: normalize -0 → +0 via IEEE addition before comparing
    JsNumber(Finite(a)), JsNumber(Finite(b)) -> a +. 0.0 == b +. 0.0
    JsNumber(a), JsNumber(b) -> a == b
    JsString(a), JsString(b) -> a == b
    JsBigInt(a), JsBigInt(b) -> a == b
    // Object identity (same Ref) — covers functions and arrays too
    JsObject(a), JsObject(b) -> a == b
    JsSymbol(a), JsSymbol(b) -> a == b
    _, _ -> False
  }
}

/// SameValueZero: like ===, but NaN equals NaN. ±0 are still equal.
/// Used by Array.prototype.includes, Map/Set key equality.
pub fn same_value_zero(left: JsValue, right: JsValue) -> Bool {
  case left, right {
    // NaN SameValueZero NaN → true (this is the only difference from ===)
    JsNumber(NaN), JsNumber(NaN) -> True
    _, _ -> strict_equal(left, right)
  }
}
