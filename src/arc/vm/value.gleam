import arc/vm/internal/tree_array.{type TreeArray}
import arc/vm/internal/tuple_array.{type TupleArray}
import arc/vm/opcode.{type Op}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set
import gleam/string

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

pub const symbol_match = WellKnownSymbol(8)

pub const symbol_match_all = WellKnownSymbol(9)

pub const symbol_replace = WellKnownSymbol(10)

pub const symbol_search = WellKnownSymbol(11)

pub const symbol_split = WellKnownSymbol(12)

pub const symbol_unscopables = WellKnownSymbol(13)

pub const symbol_dispose = WellKnownSymbol(14)

pub const symbol_async_dispose = WellKnownSymbol(15)

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
    WellKnownSymbol(8) -> Some("Symbol.match")
    WellKnownSymbol(9) -> Some("Symbol.matchAll")
    WellKnownSymbol(10) -> Some("Symbol.replace")
    WellKnownSymbol(11) -> Some("Symbol.search")
    WellKnownSymbol(12) -> Some("Symbol.split")
    WellKnownSymbol(13) -> Some("Symbol.unscopables")
    WellKnownSymbol(14) -> Some("Symbol.dispose")
    WellKnownSymbol(15) -> Some("Symbol.asyncDispose")
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
    bytecode: TupleArray(Op),
    constants: TupleArray(JsValue),
    functions: TupleArray(FuncTemplate),
    env_descriptors: List(EnvCapture),
    is_strict: Bool,
    is_arrow: Bool,
    is_derived_constructor: Bool,
    is_generator: Bool,
    is_async: Bool,
    /// Present only for functions that contain a direct eval call.
    /// Maps variable name → local slot index. All such locals are boxed
    /// (BoxSlot refs), so direct eval can read/write them by index.
    local_names: Option(List(#(String, Int))),
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

/// Opaque Erlang timer reference (from erlang:send_after).
pub type ErlangTimerRef

/// A serializable message that can be sent between BEAM processes.
/// Materializes heap-allocated structures (objects, arrays) into
/// self-contained values that don't reference any specific VM heap.
/// A serialized JS value graph, structured per the HTML spec's
/// StructuredSerializeInternal (§2.7.3): primitives are inlined, heap objects
/// are interned into `records` and referenced by integer id so that cycles
/// and shared identity survive the round-trip. Arc IPC uses this same shape
/// with extra record kinds (Pid, Subject) that the spec algorithm would
/// reject — see `CloneMode` at the serialize site.
pub type PortableMessage {
  PortableMessage(root: PortableValue, records: Dict(Int, PortableRecord))
}

/// Leaf in a serialized graph — either an inlined primitive or a pointer
/// into the message's `records` table.
pub type PortableValue {
  PvUndefined
  PvNull
  PvBool(Bool)
  PvNumber(JsNum)
  PvString(String)
  PvBigInt(BigInt)
  /// Arc extension: well-known symbols are sent by id.
  PvSymbol(id: Int)
  /// Reference into `PortableMessage.records`.
  PvRef(Int)
}

/// One interned heap object in a serialized graph. Children are
/// `PortableValue`s, so they may point back into the same table (cycles).
pub type PortableRecord {
  PrObject(
    properties: List(#(PropertyKey, PortableValue)),
    /// Arc extension; always empty in spec mode.
    symbol_properties: List(#(SymbolId, PortableValue)),
  )
  /// `properties` carries own enumerable string-keyed data props on the
  /// container itself (HTML §2.7.3 step 26 deep=true), separate from the
  /// type-specific contents.
  PrArray(
    items: List(PortableValue),
    length: Int,
    properties: List(#(PropertyKey, PortableValue)),
  )
  PrMap(
    entries: List(#(PortableValue, PortableValue)),
    properties: List(#(PropertyKey, PortableValue)),
  )
  PrSet(
    entries: List(PortableValue),
    properties: List(#(PropertyKey, PortableValue)),
  )
  PrDate(time_value: JsNum)
  PrRegExp(pattern: String, flags: String)
  PrBooleanObject(Bool)
  PrNumberObject(JsNum)
  PrStringObject(String)
  /// Arc extension.
  PrPid(ErlangPid)
  /// Arc extension.
  PrSubject(pid: ErlangPid, tag: ErlangRef)
}

/// Envelope for all messages that land in a VM process's mailbox. The event
/// loop blocks on these when the microtask queue drains but outstanding work
/// remains (pending receiveAsync, in-flight fetch, active timers).
pub type MailboxEvent {
  /// An external operation (fetch, timer) completed. Resolves or rejects the
  /// promise identified by `data_ref` (a PromiseSlot ref — just an Int, so
  /// safe to round-trip through a worker process that doesn't touch the heap).
  SettlePromise(
    data_ref: Ref,
    outcome: Result(PortableMessage, PortableMessage),
  )
  /// A `subject.receiveAsync(ms)` timeout fired. If `data_ref` is still in
  /// `pending_receivers`, resolves that promise with undefined and retires
  /// it; otherwise a no-op (a message already arrived).
  ReceiverTimeout(data_ref: Ref)
  /// A subject message matched by the event loop's selective receive.
  /// The `tag` identifies which subject it was sent to, `payload` is the
  /// serialized message. Produced by `receive_settle_or_subject` FFI.
  SubjectMessage(tag: ErlangRef, payload: PortableMessage)
}

/// JS number representation. BEAM floats can't represent NaN or Infinity,
/// so we use an explicit tagged type.
pub type JsNum {
  Finite(Float)
  NaN
  Infinity
  NegInfinity
}

/// Number::toString(x, radixMV) helper — ES2024 §21.1.3.6 step 5.
///
/// Per spec, NaN/+Infinity/-Infinity always use their canonical string forms
/// regardless of radix. For finite values:
///   - Radix 10: use standard Number::toString (decimal formatting).
///   - Other radix: convert the integer part to the specified base using
///     lowercase digits (a-z for 10-35).
///
/// Note: Non-integer values with non-10 radix fall back to decimal.
/// The spec requires proper fractional digit conversion (e.g. 3.5 in base 16
/// should produce "3.8"), but this is rarely used and complex to implement.
pub fn format_number_radix(n: JsNum, radix: Int) -> String {
  case n {
    NaN -> "NaN"
    Infinity -> "Infinity"
    NegInfinity -> "-Infinity"
    Finite(f) ->
      case radix {
        10 -> js_format_number(f)
        _ -> {
          let truncated = float.truncate(f)
          case int.to_float(truncated) == f {
            // Integer value — use radix conversion. int.to_base_string
            // returns uppercase (via erlang integer_to_binary/2); JS wants
            // lowercase. The function handles sign ("-ff" for -255) which
            // matches JS semantics.
            True ->
              int.to_base_string(truncated, radix)
              |> result.map(string.lowercase)
              // radix already validated as 2-36 by caller
              |> result.unwrap(js_format_number(f))
            // Non-integer with non-10 radix — fall back to decimal.
            False -> js_format_number(f)
          }
        }
      }
  }
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
  /// GetLocal throws ReferenceError when it encounters this.
  JsUninitialized
}

/// Tri-representation JS array elements.
///
/// None: zero-cost empty. Every non-array object (functions, promises, maps,
/// plain objects) starts here and never allocates element storage.
/// Dense: Erlang's `array` module — O(log n) get/set, ~5× tuple memory.
/// Sequential append is n·log(n) not n² (tuple set is O(n) copy on BEAM).
/// Holes are represented as unset slots (default = JsUninitialized sentinel),
/// so `delete arr[i]` stays dense via array:reset instead of O(n) dict promotion.
/// Sparse: Dict — O(log n) get/set, ~75× tuple memory. Only for arrays with
/// huge index gaps (e.g. `a[100000] = 1` on an empty array).
///
/// Operations on this type are in `arc/vm/internal/elements`.
pub type JsElements {
  NoElements
  DenseElements(data: TreeArray(JsValue))
  SparseElements(data: Dict(Int, JsValue))
}

/// Per-module sub-enums for native function dispatch.
/// These live in value.gleam (not in the builtin files) because Gleam
/// forbids circular imports and builtins import from value.gleam.
/// Math methods — pure functions, no `this`, no proto refs needed.
pub type MathNativeFn {
  MathPow
  MathAbs
  MathFloor
  MathCeil
  MathRound
  MathTrunc
  MathSqrt
  MathMax
  MathMin
  MathLog
  MathSin
  MathCos
  MathTan
  MathAsin
  MathAcos
  MathAtan
  MathAtan2
  MathExp
  MathLog2
  MathLog10
  MathRandom
  MathSign
  MathCbrt
  MathHypot
  MathFround
  MathClz32
  MathImul
  MathExpm1
  MathLog1p
  MathSinh
  MathCosh
  MathTanh
  MathAsinh
  MathAcosh
  MathAtanh
}

/// Boolean methods.
pub type BooleanNativeFn {
  BooleanConstructor
  BooleanPrototypeValueOf
  BooleanPrototypeToString
}

/// Number methods — includes static methods and global utility functions.
pub type NumberNativeFn {
  NumberConstructor
  NumberIsNaN
  NumberIsFinite
  NumberIsInteger
  NumberParseInt
  NumberParseFloat
  NumberPrototypeValueOf
  NumberPrototypeToString
  /// Global parseInt (coerces via ToNumber)
  GlobalParseInt
  /// Global parseFloat (coerces via ToNumber)
  GlobalParseFloat
  /// Global isNaN (coerces via ToNumber)
  GlobalIsNaN
  /// Global isFinite (coerces via ToNumber)
  GlobalIsFinite
  NumberIsSafeInteger
  NumberPrototypeToFixed
  NumberPrototypeToPrecision
  NumberPrototypeToExponential
}

/// String.prototype methods.
pub type StringNativeFn {
  StringPrototypeCharAt
  StringPrototypeCharCodeAt
  StringPrototypeIndexOf
  StringPrototypeLastIndexOf
  StringPrototypeIncludes
  StringPrototypeStartsWith
  StringPrototypeEndsWith
  StringPrototypeSlice
  StringPrototypeSubstring
  StringPrototypeToLowerCase
  StringPrototypeToUpperCase
  StringPrototypeToLocaleLowerCase
  StringPrototypeToLocaleUpperCase
  StringPrototypeTrim
  StringPrototypeTrimStart
  StringPrototypeTrimEnd
  StringPrototypeSplit
  StringPrototypeConcat
  StringPrototypeToString
  StringPrototypeValueOf
  StringPrototypeRepeat
  StringPrototypePadStart
  StringPrototypePadEnd
  StringPrototypeAt
  StringPrototypeCodePointAt
  StringPrototypeNormalize
  StringPrototypeMatch
  StringPrototypeSearch
  StringPrototypeReplace
  StringPrototypeReplaceAll
  StringPrototypeSubstr
  StringPrototypeLocaleCompare
  StringPrototypeMatchAll
  StringPrototypeIsWellFormed
  StringPrototypeToWellFormed
  /// Annex B HTML wrapper methods — all share a single implementation.
  StringPrototypeAnchor
  StringPrototypeBig
  StringPrototypeBlink
  StringPrototypeBold
  StringPrototypeFixed
  StringPrototypeFontcolor
  StringPrototypeFontsize
  StringPrototypeItalics
  StringPrototypeLink
  StringPrototypeSmall
  StringPrototypeStrike
  StringPrototypeSub
  StringPrototypeSup
  // Static methods
  StringRaw
  StringFromCharCode
  StringFromCodePoint
}

/// Error constructor — carries proto Ref.
pub type ErrorNativeFn {
  ErrorConstructor(proto: Ref)
  ErrorPrototypeToString
  DomExceptionConstructor(proto: Ref)
  DomExceptionGetCode
}

/// Array methods — includes constructor, static, and prototype methods.
pub type ArrayNativeFn {
  ArrayConstructor
  ArrayIsArray
  ArrayPrototypeJoin
  ArrayPrototypePush
  ArrayPrototypePop
  ArrayPrototypeShift
  ArrayPrototypeUnshift
  ArrayPrototypeSlice
  ArrayPrototypeConcat
  ArrayPrototypeReverse
  ArrayPrototypeFill
  ArrayPrototypeAt
  ArrayPrototypeIndexOf
  ArrayPrototypeLastIndexOf
  ArrayPrototypeIncludes
  ArrayPrototypeForEach
  ArrayPrototypeMap
  ArrayPrototypeFilter
  ArrayPrototypeReduce
  ArrayPrototypeReduceRight
  ArrayPrototypeEvery
  ArrayPrototypeSome
  ArrayPrototypeFind
  ArrayPrototypeFindIndex
  ArrayPrototypeSort
  ArrayPrototypeSplice
  ArrayPrototypeFindLast
  ArrayPrototypeFindLastIndex
  ArrayPrototypeFlat
  ArrayPrototypeFlatMap
  ArrayPrototypeCopyWithin
  ArrayPrototypeToSpliced
  ArrayPrototypeWith
  ArrayPrototypeToSorted
  ArrayPrototypeToReversed
  ArrayPrototypeToString
  ArrayPrototypeToLocaleString
  ArrayPrototypeKeys
  ArrayPrototypeValues
  ArrayPrototypeEntries
  ArrayFrom
  ArrayOf
}

/// Object methods — static + prototype methods.
pub type ObjectNativeFn {
  ObjectConstructor
  ObjectGetOwnPropertyDescriptor
  ObjectDefineProperty
  ObjectDefineProperties
  ObjectGetOwnPropertyNames
  ObjectKeys
  ObjectValues
  ObjectEntries
  ObjectCreate
  ObjectAssign
  ObjectIs
  ObjectHasOwn
  ObjectGetPrototypeOf
  ObjectSetPrototypeOf
  ObjectFreeze
  ObjectIsFrozen
  ObjectIsExtensible
  ObjectPreventExtensions
  ObjectPrototypeHasOwnProperty
  ObjectPrototypePropertyIsEnumerable
  ObjectPrototypeToString
  ObjectPrototypeValueOf
  ObjectFromEntries
  ObjectSeal
  ObjectIsSealed
  ObjectGetOwnPropertyDescriptors
  ObjectGetOwnPropertySymbols
  ObjectPrototypeIsPrototypeOf
  ObjectPrototypeToLocaleString
  ObjectGroupBy
}

/// Arc methods — non-standard engine-specific utilities.
pub type ArcNativeFn {
  ArcPeek
  ArcSetTimeout
  ArcClearTimeout
  ArcSelf
  ArcLog
  ArcSleep
  ArcPidToString
  ArcSubject
  ArcSelect
  ArcSelectorOn
  ArcSelectorReceive
  ArcSubjectSend
  ArcSubjectReceive
  ArcSubjectReceiveAsync
  ArcSubjectToString
  ArcStructuredClone
}

/// JSON methods — JSON.parse and JSON.stringify.
pub type JsonNativeFn {
  JsonParse
  JsonStringify
}

/// Reflect static methods — ES2024 §28.1.
/// Thin wrappers over internal object operations. Unlike Object.* counterparts,
/// all throw TypeError if target isn't an Object (no coercion), and the
/// mutation methods (defineProperty/deleteProperty/set/setPrototypeOf/
/// preventExtensions) return Bool instead of throwing on failure.
pub type ReflectNativeFn {
  ReflectApply
  ReflectConstruct
  ReflectDefineProperty
  ReflectDeleteProperty
  ReflectGet
  ReflectGetOwnPropertyDescriptor
  ReflectGetPrototypeOf
  ReflectHas
  ReflectIsExtensible
  ReflectOwnKeys
  ReflectPreventExtensions
  ReflectSet
  ReflectSetPrototypeOf
}

/// Map key type — normalizes JS values for use as Dict keys.
/// Per ES2024 §24.1.3.1, Map uses SameValueZero for key comparison:
///   - NaN equals NaN (unlike ===)
///   - +0 equals -0 (like ===)
///   - Objects compared by identity (heap Ref)
pub type MapKey {
  StringKey(String)
  /// -0 normalized to +0 per SameValueZero.
  NumberKey(Float)
  /// NaN is a valid key that equals itself (SameValueZero: NaN === NaN).
  NanKey
  InfinityKey
  NegInfinityKey
  BoolKey(Bool)
  NullKey
  UndefinedKey
  ObjectKey(Ref)
  SymbolKey(SymbolId)
  BigIntKey(BigInt)
}

/// Convert a JsValue to a MapKey for use in Dict-based Map storage.
/// Implements SameValueZero normalization: -0 → +0, NaN → NanKey.
pub fn js_to_map_key(val: JsValue) -> MapKey {
  case val {
    JsString(s) -> StringKey(s)
    JsNumber(NaN) -> NanKey
    // Normalize -0 to +0: IEEE 754 -0.0 + 0.0 = +0.0
    JsNumber(Finite(f)) -> NumberKey(f +. 0.0)
    JsNumber(Infinity) -> InfinityKey
    JsNumber(NegInfinity) -> NegInfinityKey
    JsBool(b) -> BoolKey(b)
    JsNull -> NullKey
    JsUndefined -> UndefinedKey
    JsObject(ref) -> ObjectKey(ref)
    JsSymbol(id) -> SymbolKey(id)
    JsBigInt(bi) -> BigIntKey(bi)
    JsUninitialized -> UndefinedKey
  }
}

/// Inverse of js_to_map_key. Lossless except for the -0 → +0 normalization,
/// which is exactly what ES2024 §24.1.3.9 step 4 requires ("If key is -0𝔽,
/// set key to +0𝔽"). Used by Map forEach/entries to reconstruct the original
/// JS key without needing a second Dict(MapKey, JsValue) lookup table.
pub fn map_key_to_js(key: MapKey) -> JsValue {
  case key {
    StringKey(s) -> JsString(s)
    NumberKey(f) -> JsNumber(Finite(f))
    NanKey -> JsNumber(NaN)
    InfinityKey -> JsNumber(Infinity)
    NegInfinityKey -> JsNumber(NegInfinity)
    BoolKey(b) -> JsBool(b)
    NullKey -> JsNull
    UndefinedKey -> JsUndefined
    ObjectKey(ref) -> JsObject(ref)
    SymbolKey(id) -> JsSymbol(id)
    BigIntKey(bi) -> JsBigInt(bi)
  }
}

pub fn from_int(n: Int) -> JsValue {
  JsNumber(Finite(int.to_float(n)))
}

/// Map methods — constructor, prototype methods, size getter.
pub type MapNativeFn {
  MapConstructor(proto: Ref)
  MapPrototypeGet
  MapPrototypeSet
  MapPrototypeHas
  MapPrototypeDelete
  MapPrototypeClear
  MapPrototypeForEach
  MapPrototypeGetSize
  MapPrototypeKeys
  MapPrototypeValues
  MapPrototypeEntries
}

/// Set methods — constructor, prototype methods, size getter.
pub type SetNativeFn {
  SetConstructor(proto: Ref)
  SetPrototypeAdd
  SetPrototypeHas
  SetPrototypeDelete
  SetPrototypeClear
  SetPrototypeForEach
  SetPrototypeGetSize
  SetPrototypeUnion
  SetPrototypeIntersection
  SetPrototypeDifference
  SetPrototypeSymmetricDifference
  SetPrototypeIsSubsetOf
  SetPrototypeIsSupersetOf
  SetPrototypeIsDisjointFrom
  SetPrototypeValues
  SetPrototypeEntries
}

/// What a Set iterator yields on each .next() — values or [v,v] pairs.
/// (Set has no separate "keys" kind — keys === values per §24.2.5.1.)
pub type SetIterKind {
  SetIterValues
  SetIterEntries
}

/// What a Map iterator yields on each .next().
pub type MapIterKind {
  MapIterKeys
  MapIterValues
  MapIterEntries
}

/// WeakMap methods — constructor, get, set, has, delete.
pub type WeakMapNativeFn {
  WeakMapConstructor(proto: Ref)
  WeakMapPrototypeGet
  WeakMapPrototypeSet
  WeakMapPrototypeHas
  WeakMapPrototypeDelete
}

/// WeakSet methods — constructor, add, has, delete.
pub type WeakSetNativeFn {
  WeakSetConstructor(proto: Ref)
  WeakSetPrototypeAdd
  WeakSetPrototypeHas
  WeakSetPrototypeDelete
}

/// Iterator Helper kind — ES2025 §27.1.3. Which lazy combinator created
/// this %IteratorHelper% object. Stored in IteratorHelperObject.kind.
pub type IteratorHelperKind {
  HelperMap
  HelperFilter
  HelperTake
  HelperDrop
  HelperFlatMap
}

/// Iterator methods — ES2025 §27.1. Iterator constructor, Iterator.from,
/// Iterator.prototype helper methods, and the per-helper next/return.
pub type IteratorNativeFn {
  IteratorConstructor
  IteratorFrom
  // Iterator.prototype eager consumers
  IteratorPrototypeToArray
  IteratorPrototypeForEach
  IteratorPrototypeReduce
  IteratorPrototypeSome
  IteratorPrototypeEvery
  IteratorPrototypeFind
  // Iterator.prototype lazy producers (all create IteratorHelperObject)
  IteratorPrototypeMap
  IteratorPrototypeFilter
  IteratorPrototypeTake
  IteratorPrototypeDrop
  IteratorPrototypeFlatMap
  // %IteratorHelperPrototype%.next / .return
  IteratorHelperNext
  IteratorHelperReturn
  // %WrapForValidIteratorPrototype%.next / .return
  WrapForValidIteratorNext
  WrapForValidIteratorReturn
  // get/set %Iterator.prototype%[@@toStringTag] + .constructor
  IteratorProtoGetToStringTag
  IteratorProtoSetToStringTag
  IteratorProtoGetConstructor
  IteratorProtoSetConstructor
}

/// RegExp methods — constructor, prototype methods, accessor getters.
pub type RegExpNativeFn {
  RegExpConstructor
  RegExpPrototypeTest
  RegExpPrototypeExec
  RegExpPrototypeToString
  RegExpGetSource
  RegExpGetFlags
  RegExpGetGlobal
  RegExpGetIgnoreCase
  RegExpGetMultiline
  RegExpGetDotAll
  RegExpGetSticky
  RegExpGetUnicode
  RegExpGetHasIndices
  RegExpSymbolMatch
  RegExpSymbolReplace
  RegExpSymbolSearch
  RegExpSymbolSplit
}

/// Date methods — constructor, static, prototype getters/setters/stringifiers.
pub type DateNativeFn {
  DateConstructor(proto: Ref)
  DateNow
  DateParse
  DateUTC
  DatePrototypeValueOf
  DatePrototypeGetTime
  DatePrototypeGetTimezoneOffset
  DatePrototypeGetFullYear
  DatePrototypeGetUTCFullYear
  DatePrototypeGetMonth
  DatePrototypeGetUTCMonth
  DatePrototypeGetDate
  DatePrototypeGetUTCDate
  DatePrototypeGetDay
  DatePrototypeGetUTCDay
  DatePrototypeGetHours
  DatePrototypeGetUTCHours
  DatePrototypeGetMinutes
  DatePrototypeGetUTCMinutes
  DatePrototypeGetSeconds
  DatePrototypeGetUTCSeconds
  DatePrototypeGetMilliseconds
  DatePrototypeGetUTCMilliseconds
  DatePrototypeSetTime
  DatePrototypeSetMilliseconds
  DatePrototypeSetUTCMilliseconds
  DatePrototypeSetSeconds
  DatePrototypeSetUTCSeconds
  DatePrototypeSetMinutes
  DatePrototypeSetUTCMinutes
  DatePrototypeSetHours
  DatePrototypeSetUTCHours
  DatePrototypeSetDate
  DatePrototypeSetUTCDate
  DatePrototypeSetMonth
  DatePrototypeSetUTCMonth
  DatePrototypeSetFullYear
  DatePrototypeSetUTCFullYear
  DatePrototypeGetYear
  DatePrototypeSetYear
  DatePrototypeToString
  DatePrototypeToDateString
  DatePrototypeToTimeString
  DatePrototypeToISOString
  DatePrototypeToUTCString
  DatePrototypeToLocaleString
  DatePrototypeToLocaleDateString
  DatePrototypeToLocaleTimeString
  DatePrototypeToJSON
  DatePrototypeSymbolToPrimitive
}

/// What's stored in NativeFunction — either a dispatch-level or call-level native.
/// Dispatch-level natives are handled by dispatch_native (simple return value).
/// Call-level natives are handled by call_native (need stack manipulation, VM re-entry).
/// Host-level natives are closures supplied by the embedder at engine-init time.
///
/// The `ctx` parameter is the VM state type. It's generic here because `State`
/// lives above value.gleam in the import DAG; it's instantiated as `State` at
/// the state.gleam layer. Only one concrete instantiation exists.
pub type NativeFnSlot(ctx) {
  Dispatch(NativeFn)
  Call(CallNativeFn)
  Host(fn(List(JsValue), JsValue, ctx) -> #(ctx, Result(JsValue, JsValue)))
}

/// Identifies a dispatch-level built-in native function.
/// Routed through dispatch_native → per-module dispatch functions.
pub type NativeFn {
  // Per-module dispatch wrappers
  MathNative(MathNativeFn)
  BooleanNative(BooleanNativeFn)
  NumberNative(NumberNativeFn)
  StringNative(StringNativeFn)
  ErrorNative(ErrorNativeFn)
  ArrayNative(ArrayNativeFn)
  ObjectNative(ObjectNativeFn)
  ArcNative(ArcNativeFn)
  JsonNative(JsonNativeFn)
  ReflectNative(ReflectNativeFn)
  MapNative(MapNativeFn)
  SetNative(SetNativeFn)
  WeakMapNative(WeakMapNativeFn)
  WeakSetNative(WeakSetNativeFn)
  IteratorNative(IteratorNativeFn)
  RegExpNative(RegExpNativeFn)
  DateNative(DateNativeFn)
  /// VM-level natives handled in dispatch_native — don't need stack manipulation.
  VmNative(VmNativeFn)
}

/// Native functions handled in call_native — need stack manipulation,
/// call frame pushing, or VM re-entry that dispatch_native can't do.
pub type CallNativeFn {
  FunctionCall
  FunctionApply
  FunctionBind
  /// A bound function created by Function.prototype.bind.
  BoundFunction(target: Ref, bound_this: JsValue, bound_args: List(JsValue))
  // String constructor (type coercion — needs ToPrimitive)
  StringConstructor
  // Promise
  PromiseConstructor
  PromiseThen
  PromiseCatch
  PromiseFinally
  PromiseResolveStatic
  PromiseRejectStatic
  PromiseAllStatic
  PromiseRaceStatic
  PromiseAllSettledStatic
  PromiseAnyStatic
  /// Per-element resolve handler for Promise.all.
  /// Captures: index, remaining_ref (BoxSlot counter), values_ref (array),
  /// already_called_ref (BoxSlot bool), capability resolve/reject.
  PromiseAllResolveElement(
    index: Int,
    remaining_ref: Ref,
    values_ref: Ref,
    already_called_ref: Ref,
    resolve: JsValue,
    reject: JsValue,
  )
  /// Per-element resolve handler for Promise.allSettled — stores {status:"fulfilled",value}.
  PromiseAllSettledResolveElement(
    index: Int,
    remaining_ref: Ref,
    values_ref: Ref,
    already_called_ref: Ref,
    resolve: JsValue,
  )
  /// Per-element reject handler for Promise.allSettled — stores {status:"rejected",reason}.
  PromiseAllSettledRejectElement(
    index: Int,
    remaining_ref: Ref,
    values_ref: Ref,
    already_called_ref: Ref,
    resolve: JsValue,
  )
  /// Per-element reject handler for Promise.any — collects errors for AggregateError.
  PromiseAnyRejectElement(
    index: Int,
    remaining_ref: Ref,
    errors_ref: Ref,
    already_called_ref: Ref,
    resolve: JsValue,
    reject: JsValue,
  )
  /// Internal resolve function created by CreateResolvingFunctions.
  PromiseResolveFunction(
    promise_ref: Ref,
    data_ref: Ref,
    already_resolved_ref: Ref,
  )
  /// Internal reject function created by CreateResolvingFunctions.
  PromiseRejectFunction(
    promise_ref: Ref,
    data_ref: Ref,
    already_resolved_ref: Ref,
  )
  /// Promise.prototype.finally wrapper: called on fulfill.
  PromiseFinallyFulfill(on_finally: JsValue)
  /// Promise.prototype.finally wrapper: called on reject.
  PromiseFinallyReject(on_finally: JsValue)
  /// Thunk that ignores its argument and returns the captured value.
  PromiseFinallyValueThunk(value: JsValue)
  /// Thunk that ignores its argument and throws the captured reason.
  PromiseFinallyThrower(reason: JsValue)
  // Generator
  GeneratorNext
  GeneratorReturn
  GeneratorThrow
  /// %ArrayIteratorPrototype%.next() — ES §23.1.5.2.1
  ArrayIteratorNext
  /// %SetIteratorPrototype%.next() — ES §24.2.5.2.1
  SetIteratorNext
  /// %MapIteratorPrototype%.next() — ES §24.1.5.2.1
  MapIteratorNext
  /// Async function resume: called when awaited promise settles.
  AsyncResume(async_data_ref: Ref, is_reject: Bool)
  // Async generator
  AsyncGeneratorNext
  AsyncGeneratorReturn
  AsyncGeneratorThrow
  /// Async generator resume: called when an internal await settles.
  /// is_return distinguishes the AwaitingReturn microtask from a body await.
  AsyncGeneratorResume(data_ref: Ref, is_reject: Bool, is_return: Bool)
  /// %AsyncFromSyncIteratorPrototype%.next/return/throw — ES §27.1.4.2
  AsyncFromSyncNext
  AsyncFromSyncReturn
  AsyncFromSyncThrow
  /// onFulfilled closure for AsyncFromSyncIteratorContinuation: wraps the
  /// awaited value back into `{value, done}`.
  AsyncFromSyncUnwrap(done: Bool)
  /// onRejected closure for AsyncFromSyncIteratorContinuation: closes the
  /// underlying sync iterator then rethrows the rejection reason.
  AsyncFromSyncClose(sync_iter: Ref)
  /// Symbol() constructor — callable but NOT new-able.
  SymbolConstructor
  /// Symbol.for(key) — global symbol registry lookup/insert.
  SymbolFor
  /// Symbol.keyFor(sym) — reverse lookup in global symbol registry.
  SymbolKeyFor
}

/// VM-level natives handled in dispatch_native — don't need stack manipulation.
pub type VmNativeFn {
  FunctionConstructor
  FunctionToString
  /// %IteratorPrototype%[Symbol.iterator]() — returns `this`.
  IteratorSymbolIterator
  /// Arc.spawn(fn) — needs VM internals (execute_inner, drain_jobs).
  ArcSpawn
  // Global functions
  Eval
  DecodeURI
  EncodeURI
  DecodeURIComponent
  EncodeURIComponent
  /// AnnexB legacy escape/unescape functions (B.2.1.1 / B.2.1.2)
  Escape
  Unescape
  /// $262.evalScript(source) — parse + execute a script in a specific realm.
  EvalScript
  /// $262.createRealm() — create a new realm, return its $262.
  CreateRealm
  /// $262.gc() — no-op garbage collection hint.
  Gc
}

/// Distinguishes the kind of object stored in a unified ObjectSlot.
/// Generic over `ctx` because NativeFunction carries a NativeFnSlot(ctx).
pub type ExoticKind(ctx) {
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
  NativeFunction(native: NativeFnSlot(ctx))
  /// Promise object. The visible JS object has this kind, pointing to
  /// an internal PromiseSlot that holds state/reactions.
  PromiseObject(promise_data: Ref)
  /// Generator object. Points to a GeneratorSlot that holds suspended state.
  GeneratorObject(generator_data: Ref)
  /// Async generator object. Points to an AsyncGeneratorSlot.
  AsyncGeneratorObject(generator_data: Ref)
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
  /// Subject — a typed channel bound to a process. Wraps a PID (the owner)
  /// and a unique ref (the tag). Messages sent to a subject are enveloped as
  /// `{tag, PortableMessage}` in the BEAM mailbox, enabling efficient selective
  /// receive via `is_map_key` guards. Maps to Gleam's `process.Subject`.
  SubjectObject(pid: ErlangPid, tag: ErlangRef)
  /// Selector — built by Arc.select(). Holds the (tag → handler) entries from
  /// the builder so `.receive()` can be called repeatedly without rebuilding.
  SelectorObject(entries: List(#(ErlangRef, JsValue)))
  /// Timer handle returned by Arc.setTimeout — wraps the Erlang timer ref
  /// and the promise data_ref so clearTimeout can cancel cleanly.
  TimerObject(timer_ref: ErlangTimerRef, data_ref: Ref)
  /// Map object — ES2024 §24.1 Map Objects.
  /// Stores key-value pairs using SameValueZero equality.
  /// `entries` maps normalized MapKey → value. Original JS keys are
  /// reconstructed via `map_key_to_js` (lossless inverse modulo -0→+0, which
  /// §24.1.3.9 step 4 mandates anyway), so no second dict is needed.
  /// `keys_rev` is insertion order REVERSED so set() is O(1) prepend instead
  /// of O(n) append; iteration points reverse once on read. Deleted keys stay
  /// in the list as tombstones (skipped at iteration via dict lookup) — delete
  /// is O(log n) dict-only. `keys_len` tracks list length for O(1) compaction
  /// checks; when it exceeds 2× dict.size we rebuild to drop tombstones/dupes.
  MapObject(
    entries: Dict(MapKey, JsValue),
    keys_rev: List(MapKey),
    keys_len: Int,
  )
  /// Set object — ES2024 §24.2 Set Objects.
  /// Stores unique values using SameValueZero equality.
  /// `keys` is stored in REVERSE insertion order so Set.prototype.add is O(1)
  /// prepend instead of O(n) append; iteration points reverse once on read.
  SetObject(data: Dict(MapKey, JsValue), keys: List(MapKey))
  /// WeakMap object — ES2024 §24.3 WeakMap Objects.
  /// Uses object refs as keys. No iteration, no size.
  /// Not truly weak (GC doesn't collect entries) but API-compatible.
  WeakMapObject(data: Dict(Ref, JsValue))
  /// WeakSet object — ES2024 §24.4 WeakSet Objects.
  /// Uses object refs as values. No iteration, no size.
  WeakSetObject(data: Dict(Ref, Bool))
  /// RegExp object — ES2024 §22.2 RegExp Objects.
  /// Stores the source pattern and flags strings. Actual matching
  /// is delegated to Erlang's `re` module (PCRE) via FFI.
  RegExpObject(pattern: String, flags: String)
  /// Date object — ES2024 §21.4. Has [[DateValue]] internal slot holding the
  /// time value (ms since epoch) as a JsNum. NaN represents an invalid date.
  /// After TimeClip only Finite or NaN are possible, but the type stays JsNum.
  DateObject(time_value: JsNum)
  /// Array iterator — ES2024 §23.1.5 Array Iterator Objects.
  /// Created by Array.prototype[Symbol.iterator](), values(), keys(), entries().
  /// Lazy — re-reads source length each .next() to handle mutation.
  ArrayIteratorObject(source: Ref, index: Int)
  /// Set iterator — ES2024 §24.2.5. Snapshots forward-order entries at
  /// creation time. Snapshot is simpler than live source+index because Set
  /// stores keys reversed with tombstones; re-deriving order each .next()
  /// would be O(n²). Only mutation-during-iteration tests observe the diff.
  SetIteratorObject(remaining: List(JsValue), kind: SetIterKind)
  /// Map iterator — ES2024 §24.1.5. Snapshots forward-order (key, value)
  /// pairs at creation time. Same snapshot rationale as SetIteratorObject.
  MapIteratorObject(remaining: List(#(JsValue, JsValue)), kind: MapIterKind)
  /// Async-from-Sync Iterator — ES2024 §27.1.4. Created by GetIterator(async)
  /// when the source has only Symbol.iterator. next/return/throw await the
  /// sync result's `.value` and close the sync iterator on rejection.
  AsyncFromSyncIteratorObject(sync_iter: Ref)
  /// Iterator Helper — ES2025 §27.1.3.2. Created by
  /// Iterator.prototype.{map,filter,take,drop,flatMap}.
  /// `next_method` is cached once per GetIteratorDirect (spec §7.4.9) so
  /// monkey-patching `underlying.next` after creation has no effect.
  /// `count` is remaining limit (take/drop) OR running counter
  /// (map/filter/flatMap). `inner` is flatMap's current inner iterator.
  IteratorHelperObject(
    kind: IteratorHelperKind,
    underlying: JsValue,
    next_method: JsValue,
    func: JsValue,
    inner: JsValue,
    inner_next: JsValue,
    count: Int,
    done: Bool,
  )
  /// Wrap For Valid Iterator — ES2025 §27.1.2.1.2. Created by Iterator.from
  /// when the source isn't already an instance of %Iterator.prototype%.
  WrapForValidIteratorObject(iterated: JsValue, next_method: JsValue)
}

/// Canonical property key. Per spec, property keys are String | Symbol, but
/// we distinguish array-index strings (canonical numeric strings in [0, 2^32-1))
/// at the type level so `arr[5]` never round-trips through string conversion.
/// Symbols are stored separately in `symbol_properties` so they're not here.
pub type PropertyKey {
  /// Canonical array index — a non-negative integer whose ToString form equals
  /// the original key. `"5"` → `Index(5)`, but `"05"` stays `Named("05")`.
  Index(Int)
  /// Any other string key.
  Named(String)
}

/// Canonicalize a string key. Implements CanonicalNumericIndexString (§7.1.21)
/// combined with the array-index range check: if `s` parses to a non-negative
/// int and `int.to_string(n) == s`, it's `Index(n)`; otherwise `Named(s)`.
pub fn canonical_key(s: String) -> PropertyKey {
  case int.parse(s) {
    Ok(n) if n >= 0 ->
      case int.to_string(n) == s {
        True -> Index(n)
        False -> Named(s)
      }
    _ -> Named(s)
  }
}

/// Render a PropertyKey back to its spec string form (for error messages,
/// for-in enumeration, etc.).
pub fn key_to_string(key: PropertyKey) -> String {
  case key {
    Index(n) -> int.to_string(n)
    Named(s) -> s
  }
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

pub fn prop_enumerable(prop: Property) -> Bool {
  case prop {
    DataProperty(enumerable: e, ..) | AccessorProperty(enumerable: e, ..) -> e
  }
}

pub fn prop_configurable(prop: Property) -> Bool {
  case prop {
    DataProperty(configurable: c, ..) | AccessorProperty(configurable: c, ..) ->
      c
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
    AccessorProperty(get:, set:, ..) ->
      list.append(
        get |> option.map(refs_in_value) |> option.unwrap([]),
        set |> option.map(refs_in_value) |> option.unwrap([]),
      )
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

/// Saved try-frame for generator suspension (mirrors TryFrame from state.gleam).
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

/// Async generator internal lifecycle state (ES §27.6.3.1).
/// Unlike sync generators, async gens queue requests and can be awaiting.
pub type AsyncGeneratorState {
  AGSuspendedStart
  AGSuspendedYield
  /// Running — any .next()/.return()/.throw() just enqueues.
  AGExecuting
  /// .return(v) on a completed gen awaits Promise.resolve(v) first.
  AGAwaitingReturn
  AGCompleted
}

/// Kind of request enqueued on an async generator (next/return/throw).
pub type AsyncGenCompletion {
  AGNext
  AGReturn
  AGThrow
}

/// A pending .next()/.return()/.throw() call on an async generator.
/// Each carries the promise capability that will settle when the request runs.
pub type AsyncGenRequest {
  AsyncGenRequest(
    completion: AsyncGenCompletion,
    value: JsValue,
    resolve: JsValue,
    reject: JsValue,
  )
}

/// What lives in a heap slot.
/// Generic over `ctx` because ObjectSlot.kind carries an ExoticKind(ctx).
pub type HeapSlot(ctx) {
  /// Unified object slot — covers ordinary objects, arrays, and functions.
  ObjectSlot(
    kind: ExoticKind(ctx),
    properties: Dict(PropertyKey, Property),
    elements: JsElements,
    prototype: Option(Ref),
    /// Keyword list, not Dict — preserves ES §10.1.11 property-creation order
    /// and `[]` is a zero-alloc shared atom (99% of objects have no symbol props).
    symbol_properties: List(#(SymbolId, Property)),
    extensible: Bool,
  )
  /// Flat environment state. Multiple closures in the same scope reference
  /// the same EnvSlot, so mutations to captured variables are visible across them.
  /// Compiler flattens the scope chain — no parent pointer, all captures are direct.
  /// Mutable captures stored as JsObject(box_ref) pointing to a BoxSlot.
  EnvSlot(slots: List(JsValue))
  /// Mutable variable cell for closure captures. When a variable is both captured
  /// by a closure AND mutated, both the local frame and EnvSlot hold a Ref to
  /// the same BoxSlot. Reads/writes go through this indirection.
  BoxSlot(value: JsValue)
  /// Sloppy-mode direct-eval var-injection dict. Per spec §19.2.1.1, `var`
  /// declarations inside a sloppy direct eval land in the caller's variable
  /// environment. Since caller locals are indexed at compile time, new names
  /// introduced at runtime go here. GetEvalVar/PutEvalVar check this before
  /// falling through to globals. Frame-local — saved/restored on call/return.
  EvalEnvSlot(vars: Dict(String, JsValue))
  /// Iterator state for for-in loops. Eagerly snapshots enumerable keys
  /// upfront (per spec: prototype shadowing requires full collection).
  /// Stores pre-collected string keys as JsString values.
  ForInIteratorSlot(keys: List(JsValue))
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
    saved_locals: TupleArray(JsValue),
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
    saved_locals: TupleArray(JsValue),
    saved_stack: List(JsValue),
    saved_try_stack: List(SavedTryFrame),
    saved_finally_stack: List(SavedFinallyCompletion),
    saved_this: JsValue,
    saved_callee_ref: Option(Ref),
  )
  /// Engine-internal async generator state. The ObjectSlot has
  /// `kind: AsyncGeneratorObject(generator_data: Ref)` pointing here.
  /// Unlike sync generators, .next()/.return()/.throw() enqueue requests
  /// and return promises; yield settles the head request, await suspends
  /// without settling.
  AsyncGeneratorSlot(
    gen_state: AsyncGeneratorState,
    queue: List(AsyncGenRequest),
    func_template: FuncTemplate,
    env_ref: Ref,
    saved_pc: Int,
    saved_locals: TupleArray(JsValue),
    saved_stack: List(JsValue),
    saved_try_stack: List(SavedTryFrame),
    saved_finally_stack: List(SavedFinallyCompletion),
    saved_this: JsValue,
    saved_callee_ref: Option(Ref),
  )
  /// Stores realm context for $262 methods.
  /// evalScript and createRealm read this to know which realm to operate in.
  /// Builtins are stored in State.realms (keyed by this slot's Ref) to avoid
  /// an import cycle (value.gleam cannot import builtins/common.gleam).
  RealmSlot(
    global_object: Ref,
    lexical_globals: Dict(String, JsValue),
    const_lexical_globals: set.Set(String),
    symbol_descriptions: Dict(SymbolId, String),
    symbol_registry: Dict(String, SymbolId),
  )
}

fn indent(lines: List(List(String)), indent: Int) -> String {
  let indent = string.repeat("\t", indent)
  use acc, line <- list.fold(lines, "")
  let line = indent <> string.join(line, " ")

  case acc {
    "" -> line
    acc -> acc <> "\n" <> line
  }
}

fn property_debug_lines(property: Property) -> List(List(String)) {
  case property {
    DataProperty(value:, writable:, enumerable:, configurable:) -> [
      [],
      ["value:", string.inspect(value)],
      ["writable:", bool.to_string(writable)],
      ["enumerable:", bool.to_string(enumerable)],
      ["configurable:", bool.to_string(configurable)],
    ]
    AccessorProperty(get:, set:, enumerable:, configurable:) -> [
      [],
      ["get:", string.inspect(get)],
      ["set:", string.inspect(set)],
      ["enumerable:", bool.to_string(enumerable)],
      ["configurable:", bool.to_string(configurable)],
    ]
  }
}

// will probably get rid of this function or move it and remake it.s
pub fn heap_slot_to_string(slot: HeapSlot(ctx)) -> String {
  case slot {
    ObjectSlot(
      kind:,
      properties:,
      elements:,
      prototype:,
      symbol_properties:,
      extensible:,
    ) -> {
      [
        "ObjectSlot(",
        [
          ["kind:", string.inspect(kind)],
          [
            "properties:",

            "\n"
              <> dict.fold(properties, [], fn(acc, key, property) {
              [
                [
                  key_to_string(key) <> ":",
                  property_debug_lines(property) |> indent(4),
                ],
                ..acc
              ]
            })
            |> indent(3),
          ],
          ["elements:", string.inspect(elements)],
          [
            "symbol properties:",

            "\n"
              <> list.fold(symbol_properties, [], fn(acc, pair) {
              let #(key, property) = pair
              [
                [
                  string.inspect(key),
                  property_debug_lines(property) |> indent(4),
                ],
                ..acc
              ]
            })
            |> indent(3),
          ],
          ["prototype:", string.inspect(prototype)],
          ["extensible:", string.inspect(extensible)],
        ]
          |> indent(2),
        ")",
      ]
      |> string.join("\n")
    }
    _ -> "<internal>"
  }
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

fn refs_in_saved_frame(
  env_ref: Ref,
  saved_locals: TupleArray(JsValue),
  saved_stack: List(JsValue),
  saved_finally_stack: List(SavedFinallyCompletion),
  saved_this: JsValue,
  saved_callee_ref: Option(Ref),
) -> List(Ref) {
  list.flatten([
    [env_ref],
    tuple_array.to_list(saved_locals) |> list.flat_map(refs_in_value),
    list.flat_map(saved_stack, refs_in_value),
    list.flat_map(saved_finally_stack, fn(fc) {
      case fc {
        SavedThrowCompletion(value:) | SavedReturnCompletion(value:) ->
          refs_in_value(value)
        SavedNormalCompletion -> []
      }
    }),
    refs_in_value(saved_this),
    option.map(saved_callee_ref, list.wrap) |> option.unwrap([]),
  ])
}

/// Extract all refs reachable from a heap slot by walking its JsValues.
pub fn refs_in_slot(slot: HeapSlot(ctx)) -> List(Ref) {
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
        list.flat_map(symbol_properties, fn(pair) {
          let #(_, prop) = pair
          refs_in_property(prop)
        })
      let elem_refs = case elements {
        NoElements -> []
        DenseElements(data) ->
          tree_array.to_list(data) |> list.flat_map(refs_in_value)
        SparseElements(data) ->
          dict.values(data) |> list.flat_map(refs_in_value)
      }
      let proto_refs = prototype |> option.map(list.wrap) |> option.unwrap([])
      let kind_refs = case kind {
        FunctionObject(env: env_ref, func_template: _) -> [env_ref]
        NativeFunction(Dispatch(ErrorNative(ErrorConstructor(proto: ref))))
        | NativeFunction(Dispatch(ErrorNative(DomExceptionConstructor(
            proto: ref,
          )))) -> [ref]
        NativeFunction(Dispatch(MapNative(MapConstructor(proto: ref)))) -> [ref]
        NativeFunction(Dispatch(DateNative(DateConstructor(proto: ref)))) -> [
          ref,
        ]
        NativeFunction(Dispatch(SetNative(SetConstructor(proto: ref)))) -> [ref]
        NativeFunction(Dispatch(WeakMapNative(WeakMapConstructor(proto: ref)))) -> [
          ref,
        ]
        NativeFunction(Dispatch(WeakSetNative(WeakSetConstructor(proto: ref)))) -> [
          ref,
        ]
        NativeFunction(Call(BoundFunction(target:, bound_this:, bound_args:))) -> [
          target,
          ..list.flatten([
            refs_in_value(bound_this),
            list.flat_map(bound_args, refs_in_value),
          ])
        ]
        NativeFunction(Call(PromiseResolveFunction(
          promise_ref:,
          data_ref:,
          already_resolved_ref:,
        ))) -> [promise_ref, data_ref, already_resolved_ref]
        NativeFunction(Call(PromiseRejectFunction(
          promise_ref:,
          data_ref:,
          already_resolved_ref:,
        ))) -> [promise_ref, data_ref, already_resolved_ref]
        NativeFunction(Call(PromiseFinallyFulfill(on_finally:))) ->
          refs_in_value(on_finally)
        NativeFunction(Call(PromiseFinallyReject(on_finally:))) ->
          refs_in_value(on_finally)
        NativeFunction(Call(PromiseFinallyValueThunk(value:))) ->
          refs_in_value(value)
        NativeFunction(Call(PromiseFinallyThrower(reason:))) ->
          refs_in_value(reason)
        NativeFunction(Call(AsyncResume(async_data_ref:, ..))) -> [
          async_data_ref,
        ]
        NativeFunction(Call(AsyncGeneratorResume(data_ref:, ..))) -> [data_ref]
        NativeFunction(Call(AsyncFromSyncClose(sync_iter:))) -> [sync_iter]
        PromiseObject(promise_data:) -> [promise_data]
        GeneratorObject(generator_data:) -> [generator_data]
        AsyncGeneratorObject(generator_data:) -> [generator_data]
        ArrayIteratorObject(source:, ..) -> [source]
        SetIteratorObject(remaining:, ..) ->
          list.flat_map(remaining, refs_in_value)
        MapIteratorObject(remaining:, ..) ->
          list.flat_map(remaining, fn(p) {
            list.append(refs_in_value(p.0), refs_in_value(p.1))
          })
        AsyncFromSyncIteratorObject(sync_iter:) -> [sync_iter]
        IteratorHelperObject(
          underlying:,
          next_method:,
          func:,
          inner:,
          inner_next:,
          ..,
        ) ->
          list.flat_map(
            [underlying, next_method, func, inner, inner_next],
            refs_in_value,
          )
        WrapForValidIteratorObject(iterated:, next_method:) ->
          list.flat_map([iterated, next_method], refs_in_value)
        MapObject(entries:, keys_rev:, keys_len: _) -> {
          // Trace refs in map values
          let value_refs = dict.values(entries) |> list.flat_map(refs_in_value)
          // Trace refs in keys (object keys hold heap refs). Reconstruct
          // original JsValue via map_key_to_js — the -0→+0 normalization
          // doesn't affect ref tracing since numbers carry no refs.
          let key_refs =
            list.flat_map(keys_rev, fn(k) { refs_in_value(map_key_to_js(k)) })
          list.append(value_refs, key_refs)
        }
        SetObject(data:, ..) -> {
          // Trace refs in set values (stored as dict values)
          dict.values(data) |> list.flat_map(refs_in_value)
        }
        WeakMapObject(data:) -> {
          // Trace refs in weak map keys and values
          let key_refs = dict.keys(data)
          let value_refs = dict.values(data) |> list.flat_map(refs_in_value)
          list.append(key_refs, value_refs)
        }
        WeakSetObject(data:) -> {
          // Trace refs in weak set keys
          dict.keys(data)
        }
        SelectorObject(entries:) ->
          list.flat_map(entries, fn(e) { refs_in_value(e.1) })
        OrdinaryObject
        | ArrayObject(_)
        | ArgumentsObject(_)
        | NativeFunction(_)
        | StringObject(_)
        | NumberObject(_)
        | BooleanObject(_)
        | SymbolObject(_)
        | PidObject(_)
        | SubjectObject(..)
        | TimerObject(..)
        | DateObject(_)
        | RegExpObject(..) -> []
      }
      list.flatten([prop_refs, sym_prop_refs, elem_refs, proto_refs, kind_refs])
    }
    EnvSlot(slots:) -> list.flat_map(slots, refs_in_value)
    BoxSlot(value:) -> refs_in_value(value)
    EvalEnvSlot(vars:) -> dict.values(vars) |> list.flat_map(refs_in_value)
    ForInIteratorSlot(keys:) -> list.flat_map(keys, refs_in_value)
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
    ) ->
      refs_in_saved_frame(
        env_ref,
        saved_locals,
        saved_stack,
        saved_finally_stack,
        saved_this,
        saved_callee_ref,
      )
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
    ) ->
      list.flatten([
        [promise_data_ref],
        refs_in_value(resolve),
        refs_in_value(reject),
        refs_in_saved_frame(
          env_ref,
          saved_locals,
          saved_stack,
          saved_finally_stack,
          saved_this,
          saved_callee_ref,
        ),
      ])
    AsyncGeneratorSlot(
      queue:,
      env_ref:,
      saved_locals:,
      saved_stack:,
      saved_finally_stack:,
      saved_this:,
      saved_callee_ref:,
      ..,
    ) ->
      list.flatten([
        list.flat_map(queue, fn(r) {
          list.flatten([
            refs_in_value(r.value),
            refs_in_value(r.resolve),
            refs_in_value(r.reject),
          ])
        }),
        refs_in_saved_frame(
          env_ref,
          saved_locals,
          saved_stack,
          saved_finally_stack,
          saved_this,
          saved_callee_ref,
        ),
      ])
    RealmSlot(
      global_object:,
      lexical_globals:,
      symbol_descriptions: _,
      symbol_registry: _,
      ..,
    ) -> {
      let lexical_refs =
        dict.values(lexical_globals) |> list.flat_map(refs_in_value)
      [global_object, ..lexical_refs]
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

/// Return "null" or "undefined" for error messages.
pub fn nullish_label(val: JsValue) -> String {
  case val {
    JsNull -> "null"
    _ -> "undefined"
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

pub fn abstract_equal(left: JsValue, right: JsValue) -> Bool {
  case left, right {
    // Same type — use strict equality
    JsNull, JsNull
    | JsUndefined, JsUndefined
    | JsNull, JsUndefined
    | JsUndefined, JsNull
    -> True
    JsNumber(_), JsNumber(_)
    | JsBool(_), JsBool(_)
    | JsString(_), JsString(_)
    | JsObject(_), JsObject(_)
    | JsSymbol(_), JsSymbol(_)
    | JsBigInt(_), JsBigInt(_)
    -> strict_equal(left, right)
    // Number vs String — coerce string to number
    JsNumber(_), JsString(s) ->
      to_number(JsString(s))
      |> result.map(fn(n) { strict_equal(left, JsNumber(n)) })
      |> result.unwrap(False)
    JsString(_), JsNumber(_) -> abstract_equal(right, left)
    // Bool vs anything — coerce bool to number
    JsBool(_), _ ->
      to_number(left)
      |> result.map(fn(n) { abstract_equal(JsNumber(n), right) })
      |> result.unwrap(False)
    _, JsBool(_) -> abstract_equal(right, left)
    _, _ -> False
  }
}

pub fn to_number(val: JsValue) -> Result(JsNum, String) {
  case val {
    JsNumber(n) -> Ok(n)
    JsUndefined -> Ok(NaN)
    JsNull -> Ok(Finite(0.0))
    JsBool(True) -> Ok(Finite(1.0))
    JsBool(False) -> Ok(Finite(0.0))
    JsString("") -> Ok(Finite(0.0))
    JsString(s) ->
      float.parse(s)
      |> result.or(int.parse(s) |> result.map(int.to_float))
      |> result.map(Finite)
      |> result.unwrap(NaN)
      |> Ok
    JsBigInt(_) -> Error("Cannot convert BigInt to number")
    JsSymbol(_) -> Error("Cannot convert Symbol to number")
    JsObject(_) -> Ok(NaN)
    JsUninitialized -> Error("Cannot access before initialization")
  }
}

/// JS ToNumber: https://tc39.es/ecma262/#sec-tonumber
/// JS == (abstract equality, simplified)
/// SameValueZero: like ===, but NaN equals NaN. ±0 are still equal.
/// Used by Array.prototype.includes, Map/Set key equality.
pub fn same_value_zero(left: JsValue, right: JsValue) -> Bool {
  case left, right {
    // NaN SameValueZero NaN → true (this is the only difference from ===)
    JsNumber(NaN), JsNumber(NaN) -> True
    _, _ -> strict_equal(left, right)
  }
}
