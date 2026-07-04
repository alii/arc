import arc/vm/internal/ordered_entries.{type OrderedEntries}
import arc/vm/internal/temporal_calendar.{type Calendar}
import arc/vm/internal/tree_array.{type TreeArray}
import arc/vm/internal/tuple_array.{type TupleArray}
import arc/vm/key.{type PropertyKey, Index, Named}
import arc/vm/opcode.{type Op, type TryKind}
import gleam/bit_array
import gleam/bool
import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// A reference to a heap slot. Public so heap.gleam can construct/destructure.
pub type Ref {
  Ref(id: Int)
}

/// The uninhabited type (no constructors) — Gleam's `Never`. Used as the
/// default `host` type parameter for engines that mint no opaque host values:
/// `HostObject(Empty)` is unconstructable, so a default engine provably has
/// none, at zero cost. An embedder that wants host values instantiates `host`
/// with its own type instead.
pub type Empty

/// Unique symbol identity. Not heap-allocated — symbols are value types on BEAM.
/// An opaque Erlang reference. Globally unique across the entire BEAM cluster.
/// Created via make_ref() FFI — no two calls ever return the same value.
pub type ErlangRef

/// The closed set of well-known symbols (ES2024 §6.1.5.1) the engine mints.
/// Being a sum type, a fabricated well-known symbol is unrepresentable, and
/// adding a member forces every `case` over it to be revisited. Engine-internal
/// per-object state belongs in a typed `ObjectKind` variant (see e.g.
/// `RegExpStringIteratorObject`), never in a symbol-keyed property.
pub type WellKnown {
  SymToStringTag
  SymIterator
  SymHasInstance
  SymIsConcatSpreadable
  SymToPrimitive
  SymSpecies
  SymAsyncIterator
  SymMatch
  SymMatchAll
  SymReplace
  SymSearch
  SymSplit
  SymUnscopables
  SymDispose
  SymAsyncDispose
}

/// Symbol identity. Well-known symbols are members of the closed `WellKnown`
/// sum. User-created symbols use Erlang references for global uniqueness
/// across processes — no shared counter needed.
///
/// A `UserSymbol` carries its own `[[Description]]`: it is fixed at creation
/// (`Symbol(desc)` / `Symbol.for(key)`) and never changes, so it is a pure
/// function of `ref` and term equality / dict-key semantics still hinge on
/// `ref` alone. Keeping it here means a symbol cannot reach a realm, a
/// ShadowRealm or a serialized state that has "forgotten" its description.
pub type SymbolId {
  WellKnownSymbol(which: WellKnown)
  UserSymbol(ref: ErlangRef, description: Option(String))
}

// Well-known symbol constants.
pub const symbol_to_string_tag = WellKnownSymbol(SymToStringTag)

pub const symbol_iterator = WellKnownSymbol(SymIterator)

pub const symbol_has_instance = WellKnownSymbol(SymHasInstance)

pub const symbol_is_concat_spreadable = WellKnownSymbol(SymIsConcatSpreadable)

pub const symbol_to_primitive = WellKnownSymbol(SymToPrimitive)

pub const symbol_species = WellKnownSymbol(SymSpecies)

pub const symbol_async_iterator = WellKnownSymbol(SymAsyncIterator)

pub const symbol_match = WellKnownSymbol(SymMatch)

pub const symbol_match_all = WellKnownSymbol(SymMatchAll)

pub const symbol_replace = WellKnownSymbol(SymReplace)

pub const symbol_search = WellKnownSymbol(SymSearch)

pub const symbol_split = WellKnownSymbol(SymSplit)

pub const symbol_unscopables = WellKnownSymbol(SymUnscopables)

pub const symbol_dispose = WellKnownSymbol(SymDispose)

pub const symbol_async_dispose = WellKnownSymbol(SymAsyncDispose)

/// The description string of a well-known symbol, e.g. "Symbol.iterator".
/// Exhaustive over `WellKnown`, so a new member cannot be added without
/// naming it here.
pub fn well_known_description(which: WellKnown) -> String {
  case which {
    SymToStringTag -> "Symbol.toStringTag"
    SymIterator -> "Symbol.iterator"
    SymHasInstance -> "Symbol.hasInstance"
    SymIsConcatSpreadable -> "Symbol.isConcatSpreadable"
    SymToPrimitive -> "Symbol.toPrimitive"
    SymSpecies -> "Symbol.species"
    SymAsyncIterator -> "Symbol.asyncIterator"
    SymMatch -> "Symbol.match"
    SymMatchAll -> "Symbol.matchAll"
    SymReplace -> "Symbol.replace"
    SymSearch -> "Symbol.search"
    SymSplit -> "Symbol.split"
    SymUnscopables -> "Symbol.unscopables"
    SymDispose -> "Symbol.dispose"
    SymAsyncDispose -> "Symbol.asyncDispose"
  }
}

/// Get the description string for a well-known symbol. `None` for `UserSymbol`.
pub fn well_known_symbol_description(id: SymbolId) -> Option(String) {
  case id {
    WellKnownSymbol(which) -> Some(well_known_description(which))
    UserSymbol(..) -> None
  }
}

/// §20.4 [[Description]] of any symbol: the canonical name for a well-known
/// symbol, or the (optional) description a user symbol was created with.
pub fn symbol_description(id: SymbolId) -> Option(String) {
  case id {
    WellKnownSymbol(which) -> Some(well_known_description(which))
    UserSymbol(description:, ..) -> description
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
    /// §15.1.5 ExpectedArgumentCount — the value of the function's `length`
    /// property: formal parameters before the first one with a default
    /// initializer (rest params excluded). Distinct from `arity`, which
    /// counts ALL fixed params for positional local-slot binding.
    length: Int,
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
    /// Stored [[Construct]] capability (ES2024 §7.2.4), the user-function
    /// analogue of NativeFunction's `constructible`. True for normal function
    /// declarations/expressions and class constructors; False for arrows,
    /// generators, async functions, and methods/getters/setters. Computed at
    /// compile time from the function's syntactic kind (cf. QuickJS
    /// `is_constructor` / JSC `ConstructAbility`).
    is_constructor: Bool,
    /// §10.2.1 [[Call]] step 2 [[IsClassConstructor]]: class constructors
    /// (base AND derived) throw TypeError when invoked without `new`.
    is_class_constructor: Bool,
    /// Present only for functions that contain a direct eval call.
    /// Maps variable name → local slot index. All such locals are boxed
    /// (BoxSlot refs), so direct eval can read/write them by index.
    local_names: Option(List(#(String, Int))),
    /// Local-slot indices for the §9.1.1.3 FunctionEnvironmentRecord quartet
    /// (`this` / `[[FunctionObject]]` / `[[HomeObject]]` / `[[NewTarget]]`).
    /// Non-arrows own all four (setup_locals writes them at frame entry);
    /// arrows that reference a given binding hold the env_descriptors capture
    /// index for it instead (setup_locals does NOT write — is_arrow guards
    /// that). None means unallocated/unreferenced.
    lexical: opcode.LexicalSlots,
    /// What syntax is allowed in *this* body's direct eval / nested arrow
    /// scope (§19.2.1.1 PerformEval step 6). Mirrors QuickJS's
    /// JSFunctionBytecode.{new_target,super,super_call,arguments}_allowed.
    syntax_perms: opcode.SyntaxPerms,
  )
}

/// Describes how to capture one variable from the enclosing scope when
/// creating a closure: copy the parent frame's local at `parent_index`.
///
/// This is the ONLY capture mode the compiler emits. Transitive captures
/// (a grandchild reading a grandparent's variable) never need their own
/// mode because scope.analyze_captures flattens them: every intermediate
/// function declares its own CaptureBinding for the name, so each
/// MakeClosure only ever reaches ONE frame up. For boxed (mutated-
/// after-capture) variables the parent local already holds the
/// JsObject(box_ref), so copying it shares the BoxSlot.
pub type EnvCapture {
  /// Capture from parent's local frame at the given index.
  CaptureLocal(parent_index: Int)
}

/// JS number representation. BEAM floats can't represent NaN or Infinity,
/// so we use an explicit tagged type.
pub type JsNum {
  Finite(Float)
  NaN
  Infinity
  NegInfinity
}

/// A validated `toString` radix: an integer in the inclusive range 2..36
/// (Number.prototype.toString §21.1.3.6 step 4, BigInt.prototype.toString
/// §21.2.3.3 step 3). Minted only by `radix`, so the range check lives in
/// exactly one place and the digit-conversion helpers below cannot be handed
/// a base they can't render.
pub opaque type Radix {
  Radix(base: Int)
}

/// Validate a radix (2..36). `Error(Nil)` is the caller's cue to throw a
/// RangeError.
pub fn radix(base: Int) -> Result(Radix, Nil) {
  case base >= 2 && base <= 36 {
    True -> Ok(Radix(base))
    False -> Error(Nil)
  }
}

/// Base 10 — the default radix, and the one that routes to the FFI's
/// JS-compatible decimal formatting.
pub fn decimal_radix() -> Radix {
  Radix(10)
}

/// The digit alphabet: 0-9 then lowercase a-z for digit values 10..35.
const radix_alphabet = "0123456789abcdefghijklmnopqrstuvwxyz"

fn radix_digit(d: Int) -> String {
  string.slice(radix_alphabet, d, 1)
}

/// Number::toString(x) — ES2024 §6.1.6.1.20 with radix 10.
pub fn format_number(n: JsNum) -> String {
  case n {
    NaN -> "NaN"
    Infinity -> "Infinity"
    NegInfinity -> "-Infinity"
    Finite(f) -> js_format_number(f)
  }
}

/// Number::toString(x, radixMV) — ES2024 §6.1.6.1.20.
///
/// NaN/±Infinity always use their canonical string forms regardless of radix.
/// Radix 10 uses standard decimal formatting; other radices render the integer
/// part with lowercase digits and, for non-integers, the fractional part digit
/// by digit until the emitted digits round-trip back to `f` (the same
/// shortest-representation criterion V8's DoubleToRadixCString and QuickJS's
/// js_dtoa_radix use), so `(3.5).toString(16)` is `"3.8"`.
pub fn format_number_radix(n: JsNum, r: Radix) -> String {
  case n, r.base {
    _, 10 -> format_number(n)
    NaN, _ -> "NaN"
    Infinity, _ -> "Infinity"
    NegInfinity, _ -> "-Infinity"
    Finite(f), base -> format_float_radix(f, base)
  }
}

/// BigInt::toString(x, radixMV) — ES2024 §6.1.6.2.24. Integers only, so no
/// fractional part and no rounding.
pub fn format_bigint_radix(n: Int, r: Radix) -> String {
  case r.base {
    10 -> int.to_string(n)
    // int.to_base_string only rejects bases outside 2..36, which `Radix`
    // already excludes — the Ok is total.
    base -> {
      let assert Ok(s) = int.to_base_string(n, base)
      string.lowercase(s)
    }
  }
}

/// The digits of a finite float in `base` (2..36, never 10 here).
fn format_float_radix(f: Float, base: Int) -> String {
  let sign = case f <. 0.0 {
    True -> "-"
    False -> ""
  }
  let value = float.absolute_value(f)
  let integer = float.floor(value)
  let fraction = value -. integer
  let #(fraction_digits, carry) = case fraction >. 0.0 {
    False -> #([], False)
    True -> {
      // delta = half a ULP: the point past which extra digits no longer
      // distinguish `value` from its neighbouring double.
      let delta =
        float.max(0.5 *. { next_double(value) -. value }, next_double(0.0))
      case fraction >=. delta {
        False -> #([], False)
        True -> fraction_loop(fraction, delta, base, [])
      }
    }
  }
  let integer_part = case carry {
    True -> float.truncate(integer) + 1
    False -> float.truncate(integer)
  }
  let assert Ok(integer_str) = int.to_base_string(integer_part, base)
  let fraction_str = case fraction_digits {
    [] -> ""
    ds -> "." <> string.concat(list.map(ds, radix_digit))
  }
  sign <> string.lowercase(integer_str) <> fraction_str
}

/// Emit fractional digits (most-significant first, accumulated reversed) until
/// they pin down `value` to within half a ULP, rounding the last digit up when
/// the remainder demands it. Returns the digits plus a carry into the integer
/// part (`(0.9999…).toString(2)` rounds up to "1").
fn fraction_loop(
  fraction: Float,
  delta: Float,
  base: Int,
  acc: List(Int),
) -> #(List(Int), Bool) {
  let base_f = int.to_float(base)
  let scaled = fraction *. base_f
  let delta = delta *. base_f
  let digit = float.truncate(scaled)
  let fraction = scaled -. int.to_float(digit)
  let acc = [digit, ..acc]
  let round_up = fraction >. 0.5 || { fraction == 0.5 && int.is_odd(digit) }
  case round_up && fraction +. delta >. 1.0 {
    True -> propagate_carry(acc, base)
    False ->
      case fraction >=. delta {
        True -> fraction_loop(fraction, delta, base, acc)
        False -> #(list.reverse(acc), False)
      }
  }
}

/// Round the last emitted digit up, dropping digits that overflow the base
/// (…,base-1 becomes …+1 with the trailing digit gone). An empty list means
/// the carry escapes into the integer part.
fn propagate_carry(rev_digits: List(Int), base: Int) -> #(List(Int), Bool) {
  case rev_digits {
    [] -> #([], True)
    [d, ..rest] ->
      case d + 1 < base {
        True -> #(list.reverse([d + 1, ..rest]), False)
        False -> propagate_carry(rest, base)
      }
  }
}

/// The next representable double above `f` (f >= 0). Used to size the
/// half-ULP termination threshold; never called on values large enough for
/// the successor to be Infinity, since those have no fractional part.
fn next_double(f: Float) -> Float {
  let assert <<bits:size(64)>> = <<f:float-size(64)>>
  let successor_bits = bits + 1
  let assert <<next:float-size(64)>> = <<successor_bits:size(64)>>
  next
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

/// The primitive half of `JsValue`: everything ES2024 §7.1.1 ToPrimitive can
/// return, and nothing else. Carrying it as its own type is what makes
/// "ToPrimitive handed me back an object" a compile error rather than a
/// caller-side re-dispatch that could recurse forever — see
/// `arc/vm/ops/coerce.to_primitive_prim`, whose consumers (ToString, ToNumber,
/// ToBigInt) match on it TOTALLY instead of self-recursing.
///
/// There is deliberately no `PUninitialized`: the TDZ sentinel is not a JS
/// value, so it cannot survive a coercion (`value_to_primitive` rejects it).
pub type JsPrimitive {
  PUndefined
  PNull
  PBool(Bool)
  PNumber(JsNum)
  PString(String)
  PSymbol(SymbolId)
  PBigInt(BigInt)
}

/// Widen a primitive back into the full `JsValue` domain. Total, lossless.
pub fn primitive_to_value(prim: JsPrimitive) -> JsValue {
  case prim {
    PUndefined -> JsUndefined
    PNull -> JsNull
    PBool(b) -> JsBool(b)
    PNumber(n) -> JsNumber(n)
    PString(s) -> JsString(s)
    PSymbol(s) -> JsSymbol(s)
    PBigInt(b) -> JsBigInt(b)
  }
}

/// Narrow a `JsValue` to a `JsPrimitive`, or `None` when it is not one:
/// `JsObject` (the case ToPrimitive must reject/retry) and `JsUninitialized`
/// (the TDZ sentinel, not a JS value at all).
pub fn value_to_primitive(val: JsValue) -> Option(JsPrimitive) {
  case val {
    JsUndefined -> Some(PUndefined)
    JsNull -> Some(PNull)
    JsBool(b) -> Some(PBool(b))
    JsNumber(n) -> Some(PNumber(n))
    JsString(s) -> Some(PString(s))
    JsSymbol(s) -> Some(PSymbol(s))
    JsBigInt(b) -> Some(PBigInt(b))
    JsObject(_) | JsUninitialized -> None
  }
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
  /// §22.1.3.36 String.prototype [ @@iterator ] ( )
  StringPrototypeSymbolIterator
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
  /// Error / NativeError ( message [ , options ] ) — §20.5.1.1, §20.5.6.1.1.
  ErrorConstructor(proto: Ref)
  /// AggregateError ( errors, message [ , options ] ) — §20.5.7.1.1. `errors`
  /// is drained through the iterator protocol into an own "errors" array;
  /// arg0 is NOT the message, so it cannot share ErrorConstructor.
  AggregateErrorConstructor(proto: Ref)
  /// SuppressedError ( error, suppressed, message ) — Explicit Resource
  /// Management proposal. Different argument order from NativeError.
  SuppressedErrorConstructor(proto: Ref)
  ErrorPrototypeToString
  /// V8 extension Error.captureStackTrace(target [, constructorOpt]) — sets a
  /// `stack` property on `target` from the current call stack.
  ErrorCaptureStackTrace
  /// get Error.prototype.stack — error-stack-accessor proposal. Returns the
  /// [[ErrorData]] stack string, undefined for non-error objects.
  ErrorStackGetter
  /// set Error.prototype.stack — error-stack-accessor proposal.
  /// SetterThatIgnoresPrototypeProperties(this, %Error.prototype%, "stack", v).
  /// Carries its own realm's %Error.prototype% as `proto` — using the current
  /// realm's intrinsic instead breaks cross-realm calls (and step 5's Set()
  /// would re-enter this setter forever).
  ErrorStackSetter(proto: Ref)
  /// Error.isError ( arg ) — Error.isError proposal: true iff arg is an
  /// Object with an [[ErrorData]] internal slot.
  ErrorIsError
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
  /// Annex B §B.2.2.2 Object.prototype.__defineGetter__
  ObjectPrototypeDefineGetter
  /// Annex B §B.2.2.3 Object.prototype.__defineSetter__
  ObjectPrototypeDefineSetter
  /// Annex B §B.2.2.4 Object.prototype.__lookupGetter__
  ObjectPrototypeLookupGetter
  /// Annex B §B.2.2.5 Object.prototype.__lookupSetter__
  ObjectPrototypeLookupSetter
  /// Annex B §B.2.2.1.1 get Object.prototype.__proto__
  ObjectPrototypeProtoGetter
  /// Annex B §B.2.2.1.2 set Object.prototype.__proto__
  ObjectPrototypeProtoSetter
}

/// Arc methods — non-standard engine-specific utilities.
pub type ConsoleNativeFn {
  /// console.{log,info,debug} → stdout
  ConsoleLog
  /// console.{warn,error} → stderr
  ConsoleLogError
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

/// [[ArrayIterationKind]] of an Array Iterator — ES2024 §23.1.5.1
/// CreateArrayIterator. Decides what each .next() yields: the index ("key"),
/// the element ("value"), or a fresh two-element array ("key+value").
pub type ArrayIterKind {
  ArrayIterKeys
  ArrayIterValues
  ArrayIterEntries
}

/// The nine element types whose [[ContentType]] is Number — ES2024 §23.2
/// Table 69. A value of this type is PROOF the element domain is JsNum.
pub type NumberKind {
  Int8Kind
  Uint8Kind
  Uint8ClampedKind
  Int16Kind
  Uint16Kind
  Int32Kind
  Uint32Kind
  Float32Kind
  Float64Kind
}

/// The two element types whose [[ContentType]] is BigInt — ES2024 §23.2
/// Table 69. A value of this type is PROOF the element domain is BigInt.
pub type BigIntKind {
  BigInt64Kind
  BigUint64Kind
}

/// Element type of a TypedArray — ES2024 §23.2 Table 69, split by
/// [[ContentType]] so a `case` on the kind hands the arms a witness of the
/// element domain. There is no boolean "is bigint" predicate: matching
/// `NumKind(_)` / `BigKind(_)` is the ONE spelling of §23.2's ContentType.
pub type TypedArrayKind {
  NumKind(NumberKind)
  BigKind(BigIntKind)
}

/// All TypedArray kinds, in the order the global constructors are installed.
pub const all_typed_array_kinds = [
  NumKind(Int8Kind),
  NumKind(Uint8Kind),
  NumKind(Uint8ClampedKind),
  NumKind(Int16Kind),
  NumKind(Uint16Kind),
  NumKind(Int32Kind),
  NumKind(Uint32Kind),
  NumKind(Float32Kind),
  NumKind(Float64Kind),
  BigKind(BigInt64Kind),
  BigKind(BigUint64Kind),
]

/// Element size in bytes of a Number content type — §23.2 Table 69.
pub fn number_kind_size(kind: NumberKind) -> Int {
  case kind {
    Int8Kind | Uint8Kind | Uint8ClampedKind -> 1
    Int16Kind | Uint16Kind -> 2
    Int32Kind | Uint32Kind | Float32Kind -> 4
    Float64Kind -> 8
  }
}

/// Element size in bytes — §23.2 Table 69. Both BigInt kinds are 8 bytes.
pub fn typed_array_element_size(kind: TypedArrayKind) -> Int {
  case kind {
    NumKind(k) -> number_kind_size(k)
    BigKind(_) -> 8
  }
}

/// [[TypedArrayName]] — the constructor's global name.
pub fn typed_array_name(kind: TypedArrayKind) -> String {
  case kind {
    NumKind(Int8Kind) -> "Int8Array"
    NumKind(Uint8Kind) -> "Uint8Array"
    NumKind(Uint8ClampedKind) -> "Uint8ClampedArray"
    NumKind(Int16Kind) -> "Int16Array"
    NumKind(Uint16Kind) -> "Uint16Array"
    NumKind(Int32Kind) -> "Int32Array"
    NumKind(Uint32Kind) -> "Uint32Array"
    NumKind(Float32Kind) -> "Float32Array"
    NumKind(Float64Kind) -> "Float64Array"
    BigKind(BigInt64Kind) -> "BigInt64Array"
    BigKind(BigUint64Kind) -> "BigUint64Array"
  }
}

/// TypedArray natives — %TypedArray% intrinsic, the 11 concrete constructors,
/// and %TypedArray%.prototype accessors/methods.
pub type TypedArrayNativeFn {
  /// %TypedArray% — the abstract intrinsic. Constructing or calling throws.
  TypedArrayIntrinsicConstructor
  /// One of the 11 concrete constructors (Int8Array .. BigUint64Array).
  TypedArrayConstructor(kind: TypedArrayKind, proto: Ref)
  TypedArrayGetBuffer
  TypedArrayGetByteLength
  TypedArrayGetByteOffset
  TypedArrayGetLength
  /// get %TypedArray%.prototype[@@toStringTag] — the [[TypedArrayName]].
  TypedArrayGetToStringTag
  /// get %TypedArray%[Symbol.species] — returns `this`.
  TypedArrayGetSpecies
  TypedArrayPrototypeFill
  TypedArrayPrototypeSet
  TypedArrayPrototypeSubarray
  TypedArrayPrototypeSlice
  TypedArrayPrototypeJoin
  TypedArrayPrototypeIndexOf
  TypedArrayPrototypeIncludes
  TypedArrayPrototypeKeys
  TypedArrayPrototypeValues
  TypedArrayPrototypeEntries
  TypedArrayPrototypeAt
  TypedArrayPrototypeToString
  TypedArrayPrototypeCopyWithin
  TypedArrayPrototypeEvery
  TypedArrayPrototypeSome
  TypedArrayPrototypeForEach
  TypedArrayPrototypeMap
  TypedArrayPrototypeFilter
  TypedArrayPrototypeFind
  TypedArrayPrototypeFindIndex
  TypedArrayPrototypeFindLast
  TypedArrayPrototypeFindLastIndex
  TypedArrayPrototypeLastIndexOf
  TypedArrayPrototypeReduce
  TypedArrayPrototypeReduceRight
  TypedArrayPrototypeReverse
  TypedArrayPrototypeToReversed
  TypedArrayPrototypeSort
  TypedArrayPrototypeToSorted
  TypedArrayPrototypeToLocaleString
  TypedArrayPrototypeWith
  /// %TypedArray%.from ( source [ , mapfn [ , thisArg ] ] ) — §23.2.2.1
  TypedArrayFrom
  /// %TypedArray%.of ( ...items ) — §23.2.2.2
  TypedArrayOf
  /// Uint8Array.prototype.toBase64 — proposal-arraybuffer-base64
  Uint8ArrayPrototypeToBase64
  /// Uint8Array.prototype.toHex — proposal-arraybuffer-base64
  Uint8ArrayPrototypeToHex
  /// Uint8Array.prototype.setFromBase64 — proposal-arraybuffer-base64
  Uint8ArrayPrototypeSetFromBase64
  /// Uint8Array.prototype.setFromHex — proposal-arraybuffer-base64
  Uint8ArrayPrototypeSetFromHex
  /// Uint8Array.fromBase64 — static on the Uint8Array constructor
  Uint8ArrayFromBase64
  /// Uint8Array.fromHex — static on the Uint8Array constructor
  Uint8ArrayFromHex
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
  JsNumber(num_from_int(n))
}

const nf_two52 = 4_503_599_627_370_496

const nf_two53 = 9_007_199_254_740_992

/// Integer → Number with correct rounding (round-to-nearest, ties-to-even).
/// Erlang's float/1 mis-rounds integers wider than 53 bits, so reduce to a
/// 53-bit mantissa ourselves and convert the (exactly representable) result.
pub fn num_from_int(n: Int) -> JsNum {
  let a = int.absolute_value(n)
  case a < nf_two53 {
    True -> Finite(int.to_float(n))
    False -> {
      let s = nf_bit_length(a, 0) - 53
      let q0 = int.bitwise_shift_right(a, s)
      let r = a - int.bitwise_shift_left(q0, s)
      let half = int.bitwise_shift_left(1, s - 1)
      let q = case r > half || { r == half && q0 % 2 == 1 } {
        True -> q0 + 1
        False -> q0
      }
      let #(q, s) = case q == nf_two53 {
        True -> #(nf_two52, s + 1)
        False -> #(q, s)
      }
      case 53 + s > 1024 {
        True ->
          case n < 0 {
            True -> NegInfinity
            False -> Infinity
          }
        False -> {
          let f = int.to_float(int.bitwise_shift_left(q, s))
          case n < 0 {
            True -> Finite(0.0 -. f)
            False -> Finite(f)
          }
        }
      }
    }
  }
}

fn nf_bit_length(n: Int, acc: Int) -> Int {
  case n == 0 {
    True -> acc
    False -> nf_bit_length(int.bitwise_shift_right(n, 1), acc + 1)
  }
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

/// WeakMap methods — constructor, get, set, has, delete, getOrInsert,
/// getOrInsertComputed.
pub type WeakMapNativeFn {
  WeakMapConstructor(proto: Ref)
  WeakMapPrototypeGet
  WeakMapPrototypeSet
  WeakMapPrototypeHas
  WeakMapPrototypeDelete
  WeakMapPrototypeGetOrInsert
  WeakMapPrototypeGetOrInsertComputed
}

/// WeakSet methods — constructor, add, has, delete.
pub type WeakSetNativeFn {
  WeakSetConstructor(proto: Ref)
  WeakSetPrototypeAdd
  WeakSetPrototypeHas
  WeakSetPrototypeDelete
}

/// FinalizationRegistry methods — ES2021 §26.2.
pub type FinalizationRegistryNativeFn {
  FinalizationRegistryConstructor(proto: Ref)
  FinalizationRegistryPrototypeRegister
  FinalizationRegistryPrototypeUnregister
}

/// One [[Cells]] record of a FinalizationRegistry — §26.2.1.1.
/// `target` is [[WeakRefTarget]] (object or non-registered symbol),
/// `held` is [[HeldValue]], `token` is [[UnregisterToken]] (None = ~empty~).
pub type FinRegCell {
  FinRegCell(target: JsValue, held: JsValue, token: Option(JsValue))
}

/// DisposableStack methods — Explicit Resource Management proposal §12.3.
pub type DisposableStackNativeFn {
  /// §12.3.1.1 DisposableStack ( )
  DisposableStackConstructor(proto: Ref)
  /// §12.3.3.3 DisposableStack.prototype.dispose ( )
  DisposableStackPrototypeDispose
  /// §12.3.3.6 DisposableStack.prototype.use ( value )
  DisposableStackPrototypeUse
  /// §12.3.3.1 DisposableStack.prototype.adopt ( value, onDispose )
  DisposableStackPrototypeAdopt
  /// §12.3.3.2 DisposableStack.prototype.defer ( onDispose )
  DisposableStackPrototypeDefer
  /// §12.3.3.5 DisposableStack.prototype.move ( ) — proto is the intrinsic
  /// %DisposableStack.prototype% (the new stack is NOT created from new.target).
  DisposableStackPrototypeMove(proto: Ref)
  /// §12.3.3.4 get DisposableStack.prototype.disposed
  DisposableStackDisposedGetter
  /// §12.4.1.1 AsyncDisposableStack ( )
  AsyncDisposableStackConstructor(proto: Ref)
  /// §12.4.3.3 AsyncDisposableStack.prototype.disposeAsync ( )
  AsyncDisposableStackPrototypeDisposeAsync
  /// §12.4.3.6 AsyncDisposableStack.prototype.use ( value )
  AsyncDisposableStackPrototypeUse
  /// §12.4.3.1 AsyncDisposableStack.prototype.adopt ( value, onDisposeAsync )
  AsyncDisposableStackPrototypeAdopt
  /// §12.4.3.2 AsyncDisposableStack.prototype.defer ( onDisposeAsync )
  AsyncDisposableStackPrototypeDefer
  /// §12.4.3.5 AsyncDisposableStack.prototype.move ( ) — proto is the
  /// intrinsic %AsyncDisposableStack.prototype%.
  AsyncDisposableStackPrototypeMove(proto: Ref)
  /// §12.4.3.4 get AsyncDisposableStack.prototype.disposed
  AsyncDisposableStackDisposedGetter
  /// Await continuation for the disposeAsync resource loop: invoked when an
  /// awaited dispose result settles. Carries the not-yet-disposed tail of the
  /// resource stack, the pending throw completion (DisposeResources'
  /// `completion`), and the disposeAsync promise capability to settle at the
  /// end. `is_reject` distinguishes the fulfill/reject reaction handler.
  AsyncDisposeContinue(
    remaining: List(DisposeResource),
    pending: option.Option(JsValue),
    resolve: JsValue,
    reject: JsValue,
    is_reject: Bool,
  )
  /// `using`/`await using` desugar: the disposer produced by the GetDisposer
  /// opcode (CreateDisposableResource). Calling it performs
  /// Call(method, value). When `discard` is true (async hint falling back to
  /// a sync @@dispose method — GetDisposeMethod step 1.b.ii's closure), the
  /// call result is dropped and undefined returned, so the desugared `await`
  /// awaits undefined rather than the sync method's result.
  UsingDisposer(method: JsValue, value: JsValue, discard: Bool)
}

/// A stack's [[DisposableState]] / [[AsyncDisposableState]] together with its
/// [[DisposeCapability]] — the two are only ever meaningful in tandem, so they
/// live in one sum type. A disposed stack has its [[DisposeCapability]]
/// emptied by construction: `Disposed` carries no resources, so "disposed but
/// still holding disposers" is unrepresentable.
pub type DisposableState {
  /// State is pending: `resources` is the [[DisposableResourceStack]], stored
  /// NEWEST-FIRST (O(1) prepend on add); dispose() walks it head-first, which
  /// is the spec's reverse list order.
  Pending(resources: List(DisposeResource))
  /// State is disposed: the resource stack has been taken (dispose/move).
  Disposed
}

/// One entry of a [[DisposableResourceStack]] — a DisposableResource Record
/// (Explicit Resource Management proposal §3.1).
pub type DisposeResource {
  /// From use(): [[ResourceValue]] = value, [[DisposeMethod]] = method.
  /// Dispose calls `method` with `value` as this and no arguments.
  /// On an async stack the call result is awaited.
  SyncDispose(value: JsValue, method: JsValue)
  /// From adopt()/defer(): the spec wraps the user callback in a built-in
  /// closure with [[ResourceValue]] = undefined. We store the callback and
  /// its argument list directly (the closure is not observable from JS).
  /// Dispose calls `callback` with undefined this and `args`, DISCARDS the
  /// result (the spec closure returns undefined); an async stack then awaits
  /// undefined.
  DisposeCallback(callback: JsValue, args: List(JsValue))
  /// Async use() fallback when only @@dispose exists (GetDisposeMethod's
  /// wrapper closure): call `method` with `value` as this, DISCARD the
  /// result, then await undefined.
  AsyncFallbackDispose(value: JsValue, method: JsValue)
  /// Async use(null/undefined): [[ResourceValue]] and [[DisposeMethod]] are
  /// both undefined. Nothing is called; DisposeResources sets needsAwait.
  NullDispose
}

/// ArrayBuffer / SharedArrayBuffer methods — ES2024 §25.1/§25.2.
/// One dispatch family covers both: the same internal slot layout
/// (ArrayBufferObject exotic kind) backs both, distinguished by the storage
/// kind (`buffer_is_shared(data)` — `BufShared` vs `BufBytes`).
pub type ArrayBufferNativeFn {
  /// §25.1.4.1 ArrayBuffer ( length [ , options ] )
  ArrayBufferConstructor(proto: Ref)
  /// §25.1.5.1 ArrayBuffer.isView ( arg )
  ArrayBufferIsView
  /// §25.1.5.3 get ArrayBuffer [ @@species ]
  ArrayBufferGetSpecies
  /// §25.1.6.2 get ArrayBuffer.prototype.byteLength
  ArrayBufferGetByteLength
  /// §25.1.6.3 get ArrayBuffer.prototype.detached
  ArrayBufferGetDetached
  /// §25.1.6.4 get ArrayBuffer.prototype.maxByteLength
  ArrayBufferGetMaxByteLength
  /// §25.1.6.5 get ArrayBuffer.prototype.resizable
  ArrayBufferGetResizable
  /// §25.1.6.6 ArrayBuffer.prototype.resize ( newLength )
  ArrayBufferResize
  /// §25.1.6.7 ArrayBuffer.prototype.slice ( start, end )
  ArrayBufferSlice
  /// §25.1.6.8 ArrayBuffer.prototype.transfer ( [ newLength ] )
  ArrayBufferTransfer
  /// §25.1.6.9 ArrayBuffer.prototype.transferToFixedLength ( [ newLength ] )
  ArrayBufferTransferToFixedLength
  /// Immutable ArrayBuffer proposal: get ArrayBuffer.prototype.immutable
  ArrayBufferGetImmutable
  /// Immutable ArrayBuffer proposal:
  /// ArrayBuffer.prototype.sliceToImmutable ( start, end )
  ArrayBufferSliceToImmutable
  /// Immutable ArrayBuffer proposal:
  /// ArrayBuffer.prototype.transferToImmutable ( [ newLength ] )
  ArrayBufferTransferToImmutable
  /// §25.2.3.1 SharedArrayBuffer ( length [ , options ] )
  SharedArrayBufferConstructor(proto: Ref)
  /// §25.2.4.2 get SharedArrayBuffer [ @@species ]
  SharedArrayBufferGetSpecies
  /// §25.2.5.2 get SharedArrayBuffer.prototype.byteLength
  SharedArrayBufferGetByteLength
  /// §25.2.5.3 SharedArrayBuffer.prototype.grow ( newLength )
  SharedArrayBufferGrow
  /// §25.2.5.4 get SharedArrayBuffer.prototype.growable
  SharedArrayBufferGetGrowable
  /// §25.2.5.5 get SharedArrayBuffer.prototype.maxByteLength
  SharedArrayBufferGetMaxByteLength
  /// §25.2.5.6 SharedArrayBuffer.prototype.slice ( start, end )
  SharedArrayBufferSlice
  /// test262 host hook: $262.detachArrayBuffer ( buffer )
  DetachArrayBuffer262
}

/// Atomics namespace functions — ES2024 §25.4.
pub type AtomicsNativeFn {
  /// §25.4.5 Atomics.add ( typedArray, index, value )
  AtomicsAdd
  /// §25.4.6 Atomics.and ( typedArray, index, value )
  AtomicsAnd
  /// §25.4.7 Atomics.compareExchange ( typedArray, index, expected, replacement )
  AtomicsCompareExchange
  /// §25.4.8 Atomics.exchange ( typedArray, index, value )
  AtomicsExchange
  /// §25.4.9 Atomics.isLockFree ( size )
  AtomicsIsLockFree
  /// §25.4.10 Atomics.load ( typedArray, index )
  AtomicsLoad
  /// §25.4.11 Atomics.or ( typedArray, index, value )
  AtomicsOr
  /// §25.4.12 Atomics.store ( typedArray, index, value )
  AtomicsStore
  /// §25.4.13 Atomics.sub ( typedArray, index, value )
  AtomicsSub
  /// §25.4.14 Atomics.wait ( typedArray, index, value, timeout )
  AtomicsWait
  /// §25.4.15 Atomics.waitAsync ( typedArray, index, value, timeout )
  AtomicsWaitAsync
  /// §25.4.16 Atomics.notify ( typedArray, index, count )
  AtomicsNotify
  /// Atomics.pause ( [ iterationNumber ] ) — microwait proposal.
  AtomicsPause
  /// §25.4.17 Atomics.xor ( typedArray, index, value )
  AtomicsXor
}

/// Opaque cross-process identity of a buffer's WaiterList (§25.4.3.6
/// GetWaiterList) — an Erlang term: the SAB's atomics ref for shared storage,
/// a pid-scoped heap id otherwise (see
/// arc_waiter_ffi:shared_buffer_key/local_buffer_key). Compared structurally;
/// safe to send between processes.
///
/// It lives here rather than in arc/vm/state because `AtomicsWaiter` below
/// carries one, and it is the ONE notion of waiter identity: the shared ETS
/// registry keys tokens by it, so anything that reconciles State's waiters
/// against that registry must match on it too. `arc/vm/state` re-exports it
/// as the host-capability contract type.
pub type WaiterKey

/// A pending Atomics.waitAsync waiter: a promise to resolve with "ok" when
/// Atomics.notify hits the same waiter-list slot.
///
/// `key` is the WaiterList identity of `buffer`, computed ONCE at
/// registration. It — not `buffer` — is what the shared ETS registry keys
/// this waiter's token by, so every reconciliation against that registry
/// (notify's self-token settle, a cross-process wake injection, an expiring
/// waiter withdrawing its token) matches on `key` and cannot disagree with
/// the registry about which waiters it is talking about. `buffer` stays for
/// GC rooting.
///
/// The waiter deliberately does NOT store the handle of the registry token it
/// registered: this agent's tokens at one (key, byte offset) are FUNGIBLE.
/// A cross-process wake message carries no per-waiter identity into core
/// (`event_loop.inject_notify` settles the FIRST matching waiter), so what
/// must stay balanced is the COUNT of live tokens against the count of
/// pending waiters — withdrawing "one of ours" is exactly the right
/// operation, and withdrawing "mine specifically" would let a claimed waiter
/// settle itself AND leave the notifier's in-flight wake to settle a second
/// waiter (two "ok"s for one claim, plus a stale token).
///
/// `promise_data` is the PromiseSlot ref (for settling); `promise` is the
/// visible Promise object ref (kept rooted while the waiter is pending).
/// `deadline` is the absolute monotonic-clock millisecond at which the waiter
/// times out and must be settled with "timed-out" (§25.4.3.14 DoWait,
/// EnqueueAtomicsWaitAsyncTimeoutJob) — None for an infinite timeout.
pub type AtomicsWaiter {
  AtomicsWaiter(
    key: WaiterKey,
    buffer: Ref,
    byte_offset: Int,
    promise_data: Ref,
    promise: Ref,
    deadline: option.Option(Int),
  )
}

/// DataView element types whose JS value is a Number: SetViewValue coerces
/// with ToNumber, GetViewValue produces a JsNumber.
pub type ViewNumElement {
  VInt8
  VUint8
  VInt16
  VUint16
  VInt32
  VUint32
  VFloat16
  VFloat32
  VFloat64
}

/// DataView element types whose JS value is a BigInt: SetViewValue coerces
/// with ToBigInt, GetViewValue produces a JsBigInt.
pub type ViewBigElement {
  VBigInt64
  VBigUint64
}

/// Element type read/written by DataView.prototype get*/set* methods.
/// Table "The TypedArray Constructors" element sizes apply (1/2/4/8 bytes).
///
/// The Number/BigInt split lives in the type rather than a comment: it decides
/// which coercion (ToNumber vs ToBigInt) and which encoder each get*/set* uses,
/// so a bigint element cannot reach the number encoder.
pub type ViewElementType {
  VNum(ViewNumElement)
  VBig(ViewBigElement)
}

/// DataView methods — ES2024 Section 25.3. Constructor, accessor getters,
/// and the get/set methods parametrized by element type.
pub type DataViewNativeFn {
  /// Section 25.3.2.1 DataView ( buffer [ , byteOffset [ , byteLength ] ] )
  DataViewConstructor(proto: Ref)
  /// Section 25.3.4.1 get DataView.prototype.buffer
  DataViewGetBuffer
  /// Section 25.3.4.2 get DataView.prototype.byteLength
  DataViewGetByteLength
  /// Section 25.3.4.3 get DataView.prototype.byteOffset
  DataViewGetByteOffset
  /// DataView.prototype.get<Type> ( byteOffset [ , littleEndian ] )
  DataViewGet(element: ViewElementType)
  /// DataView.prototype.set<Type> ( byteOffset, value [ , littleEndian ] )
  DataViewSet(element: ViewElementType)
}

/// Iterator Helper kind — ES2025 §27.1.3. Which lazy combinator created
/// this %IteratorHelper% object, together with that combinator's own state.
/// Stored in IteratorHelperObject.kind. Keeping the per-kind payload inside
/// the variant means take/drop provably have no callback and map/filter/
/// flatMap provably have one — there is no "func is JsUndefined" sentinel.
pub type IteratorHelperKind {
  HelperMap(func: JsValue)
  HelperFilter(func: JsValue)
  /// `remaining` counts down; at 0 the underlying iterator is closed.
  HelperTake(remaining: Int)
  /// `remaining` counts down; once it hits 0 every step is yielded.
  HelperDrop(remaining: Int)
  /// `inner` is the currently-open inner iterator record — GetIteratorFlattenable
  /// (§7.4.13) caches `next` once. `None` between inner iterators, so a
  /// half-initialized inner (an iterator without its cached next method) is
  /// unrepresentable.
  HelperFlatMap(func: JsValue, inner: Option(IteratorRecord))
}

/// An OPENED Iterator Record — ES2024 §7.4: the iterator object plus its
/// [[NextMethod]], read exactly once by GetIteratorDirect (§7.4.9) so that
/// monkey-patching `iterator.next` mid-iteration has no effect.
///
/// This is deliberately NOT a bare `#(JsValue, JsValue)`: `ConcatItem` below is
/// a same-shaped-but-completely-different pair, and the two used to flow
/// through the same functions as adjacent parameters, where transposing them
/// still typechecked.
pub type IteratorRecord {
  IteratorRecord(iterator: JsValue, next_method: JsValue)
}

/// One UNOPENED `Iterator.concat` argument — the tc39 iterator-sequencing
/// proposal validates each argument's `[Symbol.iterator]` up front but only
/// CALLS it lazily, when the previous iterable is exhausted. So `open_method`
/// is a not-yet-invoked `[Symbol.iterator]` and `iterable` is its receiver;
/// calling `open_method` on `iterable` is what produces an `IteratorRecord`.
pub type ConcatItem {
  ConcatItem(open_method: JsValue, iterable: JsValue)
}

/// Iterator.zip / Iterator.zipKeyed mode — tc39 joint-iteration proposal.
pub type ZipMode {
  ZipShortest
  ZipLongest
  ZipStrict
}

/// Per-input state for an Iterator.zip/zipKeyed helper. Index-aligned with the
/// keys list (zipKeyed only). Each member CARRIES its own "longest"-mode
/// padding value rather than the helper holding a parallel padding list, so a
/// padding entry can never go missing for a member. Outside "longest" mode the
/// spec never reads padding at all — `alloc_zip` seeds it with `undefined`.
pub type ZipMember {
  ZipOpen(record: IteratorRecord, padding: JsValue)
  /// Exhausted in "longest" mode — yields its padding value from now on.
  /// (Removed from the spec's openIters list; iters[i] set to null.)
  ZipExhausted(padding: JsValue)
}

/// Iterator methods — ES2025 §27.1. Iterator constructor, Iterator.from,
/// Iterator.prototype helper methods, and the per-helper next/return.
pub type IteratorNativeFn {
  IteratorConstructor
  IteratorFrom
  // Iterator.zip / Iterator.zipKeyed — tc39 joint-iteration proposal
  IteratorZip
  IteratorZipKeyed
  // Iterator.concat — tc39 iterator-sequencing proposal
  IteratorConcat
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

/// One of %RegExp%'s legacy internal slots (tc39
/// proposal-regexp-legacy-features): [[RegExpInput]], [[RegExpLastMatch]],
/// [[RegExpLastParen]], [[RegExpLeftContext]], [[RegExpRightContext]], and
/// [[RegExpParenN]] for N in 1..9. Names the field of `LegacyStatics` a
/// `RegExpLegacyGetter` reads. Every paren index is a distinct constructor,
/// so a mis-numbered slot (`$0`, `$10`) is not representable and reading a
/// slot is total — no "unknown slot" fallback anywhere.
pub type LegacySlot {
  LegacyInput
  LegacyLastMatch
  LegacyLastParen
  LegacyLeftContext
  LegacyRightContext
  LegacyParen1
  LegacyParen2
  LegacyParen3
  LegacyParen4
  LegacyParen5
  LegacyParen6
  LegacyParen7
  LegacyParen8
  LegacyParen9
}

/// The tc39 legacy-regexp proposal's %RegExp% internal slots, all present at
/// once. InitializeLegacyRegExpStaticProperties sets every one to "" — hence
/// `empty_legacy_statics()` — and UpdateLegacyRegExpStaticProperties rewrites
/// every one on each successful builtin exec, so there is no such thing as an
/// "unset" slot to distinguish from an empty one.
pub type LegacyStatics {
  LegacyStatics(
    input: String,
    last_match: String,
    last_paren: String,
    left_context: String,
    right_context: String,
    paren1: String,
    paren2: String,
    paren3: String,
    paren4: String,
    paren5: String,
    paren6: String,
    paren7: String,
    paren8: String,
    paren9: String,
  )
}

/// InitializeLegacyRegExpStaticProperties — every slot the empty String.
pub fn empty_legacy_statics() -> LegacyStatics {
  LegacyStatics(
    input: "",
    last_match: "",
    last_paren: "",
    left_context: "",
    right_context: "",
    paren1: "",
    paren2: "",
    paren3: "",
    paren4: "",
    paren5: "",
    paren6: "",
    paren7: "",
    paren8: "",
    paren9: "",
  )
}

/// Read one legacy slot. Total — every `LegacySlot` names a real field.
pub fn legacy_slot(statics: LegacyStatics, slot: LegacySlot) -> String {
  case slot {
    LegacyInput -> statics.input
    LegacyLastMatch -> statics.last_match
    LegacyLastParen -> statics.last_paren
    LegacyLeftContext -> statics.left_context
    LegacyRightContext -> statics.right_context
    LegacyParen1 -> statics.paren1
    LegacyParen2 -> statics.paren2
    LegacyParen3 -> statics.paren3
    LegacyParen4 -> statics.paren4
    LegacyParen5 -> statics.paren5
    LegacyParen6 -> statics.paren6
    LegacyParen7 -> statics.paren7
    LegacyParen8 -> statics.paren8
    LegacyParen9 -> statics.paren9
  }
}

/// RegExp methods — constructor, prototype methods, accessor getters.
pub type RegExpNativeFn {
  /// `legacy` holds the tc39 legacy-regexp proposal's internal slots
  /// ([[RegExpInput]], [[RegExpLastMatch]], [[RegExpParen1]], …) as one typed
  /// record. Living inside the constructor's NativeFunction kind keeps the
  /// state per-realm (each realm has its own %RegExp% object) while staying
  /// invisible to OrdinaryOwnPropertyKeys — internal slots must never show up
  /// in Object.getOwnPropertySymbols(RegExp) / Reflect.ownKeys(RegExp).
  RegExpConstructor(legacy: LegacyStatics)
  RegExpPrototypeTest
  RegExpPrototypeExec
  RegExpPrototypeToString
  /// Annex B §B.2.4.1 RegExp.prototype.compile
  RegExpPrototypeCompile
  RegExpGetSource
  RegExpGetFlags
  RegExpGetGlobal
  RegExpGetIgnoreCase
  RegExpGetMultiline
  RegExpGetDotAll
  RegExpGetSticky
  RegExpGetUnicode
  RegExpGetUnicodeSets
  RegExpGetHasIndices
  RegExpSymbolMatch
  RegExpSymbolMatchAll
  RegExpSymbolReplace
  RegExpSymbolSearch
  RegExpSymbolSplit
  /// %RegExpStringIteratorPrototype%.next() — ES2024 §22.2.9.2.1
  RegExpStringIteratorNext
  /// Legacy static accessor getter (tc39 proposal-regexp-legacy-features):
  /// RegExp.input/$_, lastMatch/$&, lastParen/$+, leftContext/$`,
  /// rightContext/$', $1-$9. `ctor` is the owning realm's %RegExp%
  /// (GetLegacyRegExpStaticProperty's SameValue receiver check); `slot` is
  /// the field of the RegExpConstructor kind's `legacy` record it reads.
  RegExpLegacyGetter(ctor: Ref, slot: LegacySlot)
  /// Legacy static accessor setter — only RegExp.input/$_ has one.
  RegExpLegacyInputSetter(ctor: Ref)
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

/// Refs to all eight Temporal type prototypes, captured inside each Temporal
/// native-function token at init time so methods can allocate instances of
/// sibling types (e.g. PlainDate.prototype.toPlainDateTime needs the
/// PlainDateTime prototype). All refs are rooted at builtin init.
pub type TemporalProtos {
  TemporalProtos(
    plain_date: Ref,
    plain_time: Ref,
    plain_date_time: Ref,
    plain_year_month: Ref,
    plain_month_day: Ref,
    duration: Ref,
    instant: Ref,
    zoned_date_time: Ref,
  )
}

/// Which Temporal type a native function belongs to.
pub type TemporalKind {
  TemporalPlainDateKind
  TemporalPlainTimeKind
  TemporalPlainDateTimeKind
  TemporalPlainYearMonthKind
  TemporalPlainMonthDayKind
  TemporalDurationKind
  TemporalInstantKind
  TemporalZonedDateTimeKind
}

/// Date fields shared by PlainDate, PlainDateTime and ZonedDateTime.
pub type TemporalDateGetter {
  DgCalendarId
  DgEra
  DgEraYear
  DgYear
  DgMonth
  DgMonthCode
  DgDay
  DgDayOfWeek
  DgDayOfYear
  DgWeekOfYear
  DgYearOfWeek
  DgDaysInWeek
  DgDaysInMonth
  DgDaysInYear
  DgMonthsInYear
  DgInLeapYear
}

/// Wall-clock fields shared by PlainTime, PlainDateTime and ZonedDateTime.
pub type TemporalTimeGetter {
  TgHour
  TgMinute
  TgSecond
  TgMillisecond
  TgMicrosecond
  TgNanosecond
}

pub type TemporalDateTimeGetter {
  DtDate(TemporalDateGetter)
  DtTime(TemporalTimeGetter)
}

pub type TemporalYearMonthGetter {
  YmCalendarId
  YmEra
  YmEraYear
  YmYear
  YmMonth
  YmMonthCode
  YmDaysInYear
  YmDaysInMonth
  YmMonthsInYear
  YmInLeapYear
}

pub type TemporalMonthDayGetter {
  MdCalendarId
  MdMonthCode
  MdDay
}

pub type TemporalDurationGetter {
  DrYears
  DrMonths
  DrWeeks
  DrDays
  DrHours
  DrMinutes
  DrSeconds
  DrMilliseconds
  DrMicroseconds
  DrNanoseconds
  DrSign
  DrBlank
}

pub type TemporalInstantGetter {
  InEpochMilliseconds
  InEpochNanoseconds
}

pub type TemporalZonedGetter {
  ZgTimeZoneId
  ZgEpochMilliseconds
  ZgEpochNanoseconds
  ZgOffsetNanoseconds
  ZgOffset
  ZgHoursInDay
  ZgDate(TemporalDateGetter)
  ZgTime(TemporalTimeGetter)
}

/// Every Temporal prototype getter, closed per type. The type a getter belongs
/// to is the outer variant, so a getter that is registered on a prototype but
/// has no dispatch arm (or vice versa) is a compile error, not `undefined`.
pub type TemporalGetter {
  PlainDateGetter(TemporalDateGetter)
  PlainTimeGetter(TemporalTimeGetter)
  PlainDateTimeGetter(TemporalDateTimeGetter)
  PlainYearMonthGetter(TemporalYearMonthGetter)
  PlainMonthDayGetter(TemporalMonthDayGetter)
  DurationGetter(TemporalDurationGetter)
  InstantGetter(TemporalInstantGetter)
  ZonedDateTimeGetter(TemporalZonedGetter)
}

pub type PlainDateMethod {
  PdToPlainYearMonth
  PdToPlainMonthDay
  PdToPlainDateTime
  PdToZonedDateTime
  PdAdd
  PdSubtract
  PdWith
  PdWithCalendar
  PdUntil
  PdSince
  PdEquals
  PdToString
  PdToLocaleString
  PdToJson
  PdValueOf
}

pub type PlainTimeMethod {
  PtAdd
  PtSubtract
  PtWith
  PtUntil
  PtSince
  PtRound
  PtEquals
  PtToString
  PtToLocaleString
  PtToJson
  PtValueOf
}

pub type PlainDateTimeMethod {
  PdtWith
  PdtWithPlainTime
  PdtWithCalendar
  PdtAdd
  PdtSubtract
  PdtUntil
  PdtSince
  PdtRound
  PdtEquals
  PdtToString
  PdtToLocaleString
  PdtToJson
  PdtValueOf
  PdtToPlainDate
  PdtToPlainTime
  PdtToZonedDateTime
}

pub type PlainYearMonthMethod {
  PymWith
  PymAdd
  PymSubtract
  PymUntil
  PymSince
  PymEquals
  PymToString
  PymToLocaleString
  PymToJson
  PymValueOf
  PymToPlainDate
}

pub type PlainMonthDayMethod {
  PmdWith
  PmdEquals
  PmdToString
  PmdToLocaleString
  PmdToJson
  PmdValueOf
  PmdToPlainDate
}

pub type DurationMethod {
  DmWith
  DmNegated
  DmAbs
  DmAdd
  DmSubtract
  DmRound
  DmTotal
  DmToString
  DmToJson
  DmToLocaleString
  DmValueOf
}

pub type InstantMethod {
  ImAdd
  ImSubtract
  ImUntil
  ImSince
  ImRound
  ImEquals
  ImToString
  ImToLocaleString
  ImToJson
  ImValueOf
  ImToZonedDateTimeIso
}

pub type ZonedDateTimeMethod {
  ZmWithTimeZone
  ZmWithCalendar
  ZmWithPlainTime
  ZmWith
  ZmAdd
  ZmSubtract
  ZmUntil
  ZmSince
  ZmRound
  ZmEquals
  ZmToString
  ZmToLocaleString
  ZmToJson
  ZmValueOf
  ZmStartOfDay
  ZmGetTimeZoneTransition
  ZmToInstant
  ZmToPlainDate
  ZmToPlainTime
  ZmToPlainDateTime
}

/// Every Temporal prototype method, closed per type — same shape as
/// `TemporalGetter`, so the method a prototype registers and the arm that
/// implements it are checked against each other by the compiler.
pub type TemporalMethodName {
  PlainDateMethodName(PlainDateMethod)
  PlainTimeMethodName(PlainTimeMethod)
  PlainDateTimeMethodName(PlainDateTimeMethod)
  PlainYearMonthMethodName(PlainYearMonthMethod)
  PlainMonthDayMethodName(PlainMonthDayMethod)
  DurationMethodName(DurationMethod)
  InstantMethodName(InstantMethod)
  ZonedDateTimeMethodName(ZonedDateTimeMethod)
}

/// Temporal natives. Constructors and statics dispatch on `kind` (statics also
/// on a name); getters and methods carry a closed enum that already names the
/// Temporal type they belong to, so registration and dispatch cannot drift.
pub type TemporalNativeFn {
  /// `new Temporal.<Type>(...)`
  TemporalCtor(kind: TemporalKind, protos: TemporalProtos)
  /// Static method, e.g. Temporal.PlainDate.from / .compare
  TemporalStatic(kind: TemporalKind, name: String, protos: TemporalProtos)
  /// Prototype getter, e.g. get Temporal.PlainDate.prototype.year
  TemporalGetterFn(getter: TemporalGetter, protos: TemporalProtos)
  /// Prototype method, e.g. Temporal.PlainDate.prototype.add
  TemporalMethod(method: TemporalMethodName, protos: TemporalProtos)
  /// Temporal.Now.* functions
  TemporalNowFn(name: String, protos: TemporalProtos)
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
  ConsoleNative(ConsoleNativeFn)
  JsonNative(JsonNativeFn)
  ReflectNative(ReflectNativeFn)
  MapNative(MapNativeFn)
  SetNative(SetNativeFn)
  WeakMapNative(WeakMapNativeFn)
  WeakSetNative(WeakSetNativeFn)
  FinalizationRegistryNative(FinalizationRegistryNativeFn)
  DisposableStackNative(DisposableStackNativeFn)
  IteratorNative(IteratorNativeFn)
  RegExpNative(RegExpNativeFn)
  DateNative(DateNativeFn)
  IntlNative(IntlNativeFn)
  ArrayBufferNative(ArrayBufferNativeFn)
  AtomicsNative(AtomicsNativeFn)
  TypedArrayNative(TypedArrayNativeFn)
  DataViewNative(DataViewNativeFn)
  TemporalNative(TemporalNativeFn)
  ShadowRealmNative(ShadowRealmNativeFn)
  /// VM-level natives handled in dispatch_native — don't need stack manipulation.
  VmNative(VmNativeFn)
}

/// ShadowRealm natives (proposal-shadowrealm).
pub type ShadowRealmNativeFn {
  /// ShadowRealm ( ) — the constructor. Creates a fresh realm.
  ShadowRealmConstructor(proto: Ref)
  /// ShadowRealm.prototype.evaluate ( sourceText ). `fn_proto` is the
  /// %Function.prototype% of the realm this method object belongs to —
  /// a unique per-realm marker used to recover the method's own realm
  /// (the spec's callerRealm for wrapping/errors) at dispatch time.
  ShadowRealmEvaluate(fn_proto: Ref)
  /// ShadowRealm.prototype.importValue ( specifier, exportName ). `fn_proto`
  /// as in ShadowRealmEvaluate.
  ShadowRealmImportValue(fn_proto: Ref)
  /// Wrapped function exotic object [[Call]] (proposal §2.1).
  /// `target` is [[WrappedTargetFunction]], `caller_realm` is the wrapped
  /// function's [[Realm]] (a RealmSlot ref — the realm the wrapper lives in),
  /// `target_realm` is the realm the target function belongs to.
  WrappedFunctionCall(target: JsValue, caller_realm: Ref, target_realm: Ref)
}

/// Which Intl service an Intl instance object (or native fn) belongs to.
/// Used both as the brand for [[InitializedX]] internal-slot checks and to
/// route shared method implementations (resolvedOptions, supportedLocalesOf).
pub type IntlService {
  IntlLocale
  IntlCollator
  IntlNumberFormat
  IntlDateTimeFormat
  IntlPluralRules
  IntlListFormat
  IntlRelativeTimeFormat
  IntlSegmenter
  IntlDisplayNames
  IntlDurationFormat
  /// %SegmentsPrototype% instances returned by Segmenter.prototype.segment.
  IntlSegments
  /// %SegmentIteratorPrototype% instances.
  IntlSegmentIterator
}

// ---------------------------------------------------------------------------
// Intl per-service resolved state (ECMA-402 internal slots)
// ---------------------------------------------------------------------------
//
// Each Intl service stores its resolved constructor options in a dedicated
// record — one field per internal slot, with its real type. This replaces the
// old `Dict(String, JsValue)` slot bag, where a mistyped key or a slot written
// with the wrong JsValue shape silently read back as None.
//
// Enum-valued options (style, notation, roundingMode, …) stay `String` here;
// they are validated against a closed value list at construction time.

/// The resolved state carried by an `IntlObject`. The variant IS the brand:
/// `Intl.NumberFormat.prototype.format` cannot be handed Collator state.
pub type IntlData {
  LocaleData(LocaleState)
  CollatorData(CollatorState)
  NumberFormatData(NumberFormatState)
  DateTimeFormatData(DateTimeFormatState)
  PluralRulesData(PluralRulesState)
  ListFormatData(ListFormatState)
  RelativeTimeFormatData(RelativeTimeFormatState)
  SegmenterData(SegmenterState)
  DisplayNamesData(DisplayNamesState)
  DurationFormatData(DurationFormatState)
  SegmentsData(SegmentsState)
  SegmentIteratorData(SegmentIteratorState)
}

/// The service tag for a given Intl state — the two are 1:1.
pub fn intl_service(data: IntlData) -> IntlService {
  case data {
    LocaleData(_) -> IntlLocale
    CollatorData(_) -> IntlCollator
    NumberFormatData(_) -> IntlNumberFormat
    DateTimeFormatData(_) -> IntlDateTimeFormat
    PluralRulesData(_) -> IntlPluralRules
    ListFormatData(_) -> IntlListFormat
    RelativeTimeFormatData(_) -> IntlRelativeTimeFormat
    SegmenterData(_) -> IntlSegmenter
    DisplayNamesData(_) -> IntlDisplayNames
    DurationFormatData(_) -> IntlDurationFormat
    SegmentsData(_) -> IntlSegments
    SegmentIteratorData(_) -> IntlSegmentIterator
  }
}

/// Intl.Locale — the canonicalized `[[Locale]]` tag. Every getter/method is
/// derived by re-parsing it.
pub type LocaleState {
  LocaleState(locale: String)
}

/// Intl.Collator resolved options (§10.1.2 InitializeCollator).
/// `bound_compare` caches the bound function returned by the `compare` getter.
pub type CollatorState {
  CollatorState(
    locale: String,
    usage: String,
    sensitivity: String,
    ignore_punctuation: Bool,
    collation: String,
    numeric: Bool,
    case_first: CaseFirst,
    bound_compare: Option(Ref),
  )
}

/// `[[CaseFirst]]` (§10.1.2 InitializeCollator, UTS 35 `kf`). Parsed once at
/// construction so the collator's tertiary (case) level can dispatch on it
/// exhaustively — an unrecognised spelling cannot reach the comparator and be
/// silently treated as the default.
pub type CaseFirst {
  CaseFirstUpper
  CaseFirstLower
  CaseFirstFalse
}

pub fn case_first_to_js_string(v: CaseFirst) -> String {
  case v {
    CaseFirstUpper -> "upper"
    CaseFirstLower -> "lower"
    CaseFirstFalse -> "false"
  }
}

/// The `kf` u-extension / `caseFirst` option spellings.
pub fn case_first_from_js_string(s: String) -> Option(CaseFirst) {
  case s {
    "upper" -> Some(CaseFirstUpper)
    "lower" -> Some(CaseFirstLower)
    "false" -> Some(CaseFirstFalse)
    _ -> None
  }
}

// --- Intl.NumberFormat closed option sets (§15.1) -------------------------
//
// Each of these options admits a fixed set of spellings. They are parsed
// (and validated) exactly once, in the constructors in `intl.gleam`; the
// formatting engine then dispatches on the variants exhaustively, so an
// out-of-set value cannot reach — or be silently defaulted by — a formatter.
// The `*_to_js_string` functions render the spec spelling for
// resolvedOptions.

/// `[[Style]]` (§15.1.3 SetNumberFormatUnitOptions). The style-conditional
/// slots live *inside* the variant that selects them: `[[Currency]]` /
/// `[[CurrencyDisplay]]` / `[[CurrencySign]]` exist exactly when the style is
/// currency, `[[Unit]]` / `[[UnitDisplay]]` exactly when it is unit. A
/// currency style without a currency code is therefore not representable, and
/// no formatter has to invent a default for a slot the style did not select.
pub type NumStyle {
  StyleDecimal
  StylePercent
  StyleCurrency(currency: String, display: CurrencyDisplay, sign: CurrencySign)
  StyleUnit(unit: String, display: UnitDisplay)
}

pub fn num_style_to_js_string(v: NumStyle) -> String {
  case v {
    StyleDecimal -> "decimal"
    StylePercent -> "percent"
    StyleCurrency(..) -> "currency"
    StyleUnit(..) -> "unit"
  }
}

/// `[[Notation]]` — shared by NumberFormat and PluralRules. `[[CompactDisplay]]`
/// only exists under compact notation, so it lives in that variant.
pub type Notation {
  NotationStandard
  NotationScientific
  NotationEngineering
  NotationCompact(display: CompactDisplay)
}

pub fn notation_to_js_string(v: Notation) -> String {
  case v {
    NotationStandard -> "standard"
    NotationScientific -> "scientific"
    NotationEngineering -> "engineering"
    NotationCompact(..) -> "compact"
  }
}

/// `[[CompactDisplay]]` — only meaningful under compact notation.
pub type CompactDisplay {
  CompactShort
  CompactLong
}

pub fn compact_display_to_js_string(v: CompactDisplay) -> String {
  case v {
    CompactShort -> "short"
    CompactLong -> "long"
  }
}

/// `[[SignDisplay]]`.
pub type SignDisplay {
  SignAuto
  SignNever
  SignAlways
  SignExceptZero
  SignNegative
}

pub fn sign_display_to_js_string(v: SignDisplay) -> String {
  case v {
    SignAuto -> "auto"
    SignNever -> "never"
    SignAlways -> "always"
    SignExceptZero -> "exceptZero"
    SignNegative -> "negative"
  }
}

/// `[[CurrencyDisplay]]` — only meaningful for the currency style.
pub type CurrencyDisplay {
  CurCode
  CurSymbol
  CurNarrowSymbol
  CurName
}

pub fn currency_display_to_js_string(v: CurrencyDisplay) -> String {
  case v {
    CurCode -> "code"
    CurSymbol -> "symbol"
    CurNarrowSymbol -> "narrowSymbol"
    CurName -> "name"
  }
}

/// `[[CurrencySign]]` — only meaningful for the currency style.
pub type CurrencySign {
  CurStandard
  CurAccounting
}

pub fn currency_sign_to_js_string(v: CurrencySign) -> String {
  case v {
    CurStandard -> "standard"
    CurAccounting -> "accounting"
  }
}

/// `[[UnitDisplay]]` — only meaningful for the unit style.
pub type UnitDisplay {
  UnitShort
  UnitNarrow
  UnitLong
}

pub fn unit_display_to_js_string(v: UnitDisplay) -> String {
  case v {
    UnitShort -> "short"
    UnitNarrow -> "narrow"
    UnitLong -> "long"
  }
}

/// `[[RoundingMode]]` (§15.5.2).
pub type RoundingMode {
  RoundCeil
  RoundFloor
  RoundExpand
  RoundTrunc
  RoundHalfCeil
  RoundHalfFloor
  RoundHalfExpand
  RoundHalfTrunc
  RoundHalfEven
}

pub fn rounding_mode_to_js_string(v: RoundingMode) -> String {
  case v {
    RoundCeil -> "ceil"
    RoundFloor -> "floor"
    RoundExpand -> "expand"
    RoundTrunc -> "trunc"
    RoundHalfCeil -> "halfCeil"
    RoundHalfFloor -> "halfFloor"
    RoundHalfExpand -> "halfExpand"
    RoundHalfTrunc -> "halfTrunc"
    RoundHalfEven -> "halfEven"
  }
}

/// `[[RoundingType]]` selection priority (§15.1.6).
pub type RoundingPriority {
  PriorityAuto
  PriorityMorePrecision
  PriorityLessPrecision
}

pub fn rounding_priority_to_js_string(v: RoundingPriority) -> String {
  case v {
    PriorityAuto -> "auto"
    PriorityMorePrecision -> "morePrecision"
    PriorityLessPrecision -> "lessPrecision"
  }
}

/// `[[TrailingZeroDisplay]]`.
pub type TrailingZeroDisplay {
  TzdAuto
  TzdStripIfInteger
}

pub fn trailing_zero_display_to_js_string(v: TrailingZeroDisplay) -> String {
  case v {
    TzdAuto -> "auto"
    TzdStripIfInteger -> "stripIfInteger"
  }
}

/// SetNumberFormatDigitOptions result (§15.1.6) — shared by NumberFormat and
/// PluralRules. The fraction/significant pairs are absent when that rounding
/// kind was not requested, and resolvedOptions omits absent pairs.
pub type IntlDigitOptions {
  IntlDigitOptions(
    minimum_integer_digits: Int,
    minimum_fraction_digits: Option(Int),
    maximum_fraction_digits: Option(Int),
    minimum_significant_digits: Option(Int),
    maximum_significant_digits: Option(Int),
    rounding_increment: Int,
    rounding_mode: RoundingMode,
    rounding_priority: RoundingPriority,
    trailing_zero_display: TrailingZeroDisplay,
  )
}

/// `[[UseGrouping]]` — spec-wise either the boolean `false` or one of the
/// strings "min2" / "auto" / "always" (a `true` option normalizes to
/// "always", `false` to never). resolvedOptions must surface never as the
/// boolean `false`, the rest as their string spelling.
pub type IntlUseGrouping {
  GroupingAuto
  GroupingAlways
  GroupingMin2
  GroupingNever
}

/// Intl.NumberFormat resolved options (§15.1.2 InitializeNumberFormat).
/// The style-/notation-conditional slots (currency*, unit*, compactDisplay)
/// live inside the `NumStyle` / `Notation` variant that selects them.
/// `bound_format` caches the `format` getter's bound function.
pub type NumberFormatState {
  NumberFormatState(
    locale: String,
    numbering_system: String,
    style: NumStyle,
    digits: IntlDigitOptions,
    use_grouping: IntlUseGrouping,
    notation: Notation,
    sign_display: SignDisplay,
    bound_format: Option(Ref),
  )
}

/// A DateTimeFormat formatting component: one option name of the §11.1.2
/// component table. A closed enum so the fixed component tables in
/// `intl.gleam` cannot name a component that does not exist.
pub type DtfComponent {
  DtfWeekday
  DtfEra
  DtfYear
  DtfMonth
  DtfDay
  DtfDayPeriod
  DtfHour
  DtfMinute
  DtfSecond
  DtfFractionalSecondDigits
  DtfTimeZoneName
}

// --- DateTimeFormat closed option sets (§11.1.2 component table) -----------
//
// Each component admits its own fixed set of widths — the §11.1.2 table gives
// weekday/era/dayPeriod « narrow, short, long », year/day/hour/minute/second
// « 2-digit, numeric », month all five, timeZoneName its own six. Modelling
// them as one `String` let a formatter silently default a width the validator
// had accepted; each is now its own closed sum, so a formatter that forgets a
// width does not compile.

/// Widths of the numeric-only components (year, day, hour, minute, second).
pub type NumericWidth {
  WNumeric
  WTwoDigit
}

pub fn numeric_width_to_js_string(v: NumericWidth) -> String {
  case v {
    WNumeric -> "numeric"
    WTwoDigit -> "2-digit"
  }
}

/// Widths of the name components (weekday, era, dayPeriod).
pub type NameWidth {
  WLong
  WShort
  WNarrow
}

pub fn name_width_to_js_string(v: NameWidth) -> String {
  case v {
    WLong -> "long"
    WShort -> "short"
    WNarrow -> "narrow"
  }
}

/// `month` is the one component that admits both a numeric and a name width.
pub type MonthWidth {
  MonthNum(NumericWidth)
  MonthName(NameWidth)
}

pub fn month_width_to_js_string(v: MonthWidth) -> String {
  case v {
    MonthNum(w) -> numeric_width_to_js_string(w)
    MonthName(w) -> name_width_to_js_string(w)
  }
}

/// `timeZoneName` widths (§11.1.2).
pub type TimeZoneNameWidth {
  TzShort
  TzLong
  TzShortOffset
  TzLongOffset
  TzShortGeneric
  TzLongGeneric
}

pub fn time_zone_name_width_to_js_string(v: TimeZoneNameWidth) -> String {
  case v {
    TzShort -> "short"
    TzLong -> "long"
    TzShortOffset -> "shortOffset"
    TzLongOffset -> "longOffset"
    TzShortGeneric -> "shortGeneric"
    TzLongGeneric -> "longGeneric"
  }
}

/// `[[HourCycle]]` (§11.1.2).
pub type HourCycle {
  H11
  H12
  H23
  H24
}

pub fn hour_cycle_to_js_string(v: HourCycle) -> String {
  case v {
    H11 -> "h11"
    H12 -> "h12"
    H23 -> "h23"
    H24 -> "h24"
  }
}

/// `[[DateStyle]]`.
pub type DateStyle {
  DsFull
  DsLong
  DsMedium
  DsShort
}

pub fn date_style_to_js_string(v: DateStyle) -> String {
  case v {
    DsFull -> "full"
    DsLong -> "long"
    DsMedium -> "medium"
    DsShort -> "short"
  }
}

/// `[[TimeStyle]]`.
pub type TimeStyle {
  TsFull
  TsLong
  TsMedium
  TsShort
}

pub fn time_style_to_js_string(v: TimeStyle) -> String {
  case v {
    TsFull -> "full"
    TsLong -> "long"
    TsMedium -> "medium"
    TsShort -> "short"
  }
}

/// The active DateTimeFormat formatting components — which date/time fields
/// the output contains and in which width (§11.5 DateTimeFormat records).
/// `None` means the component is not part of the format.
pub type DtfComponents {
  DtfComponents(
    weekday: Option(NameWidth),
    era: Option(NameWidth),
    year: Option(NumericWidth),
    month: Option(MonthWidth),
    day: Option(NumericWidth),
    day_period: Option(NameWidth),
    hour: Option(NumericWidth),
    minute: Option(NumericWidth),
    second: Option(NumericWidth),
    /// `fractionalSecondDigits` is a digit count in 1..3, not a width.
    fractional_second_digits: Option(Int),
    time_zone_name: Option(TimeZoneNameWidth),
  )
}

/// A `DtfComponents` with every component absent.
pub const empty_dtf_components = DtfComponents(
  weekday: None,
  era: None,
  year: None,
  month: None,
  day: None,
  day_period: None,
  hour: None,
  minute: None,
  second: None,
  fractional_second_digits: None,
  time_zone_name: None,
)

/// Intl.DateTimeFormat resolved options (§11.1.2 CreateDateTimeFormat).
///
/// The `weekday` … `time_style` fields are the resolvedOptions view (the
/// component options as the user requested them, plus locale defaults);
/// `components` is the effective formatting table (style expansion applied,
/// and re-derived per Temporal type at format time). The two intentionally
/// differ once dateStyle/timeStyle is involved.
///
/// `explicit` lists the component options the user provided explicitly —
/// GetDateTimeFormat needs it for Temporal ~relevant~ inheritance.
/// `tz_system` marks the host default zone: the offset is then resolved live
/// per formatted instant instead of from `tz_offset_minutes`.
pub type DateTimeFormatState {
  DateTimeFormatState(
    locale: String,
    calendar: String,
    numbering_system: String,
    time_zone: String,
    tz_offset_minutes: Int,
    tz_system: Bool,
    hour_cycle: Option(HourCycle),
    weekday: Option(NameWidth),
    era: Option(NameWidth),
    year: Option(NumericWidth),
    month: Option(MonthWidth),
    day: Option(NumericWidth),
    day_period: Option(NameWidth),
    hour: Option(NumericWidth),
    minute: Option(NumericWidth),
    second: Option(NumericWidth),
    fractional_second_digits: Option(Int),
    time_zone_name: Option(TimeZoneNameWidth),
    date_style: Option(DateStyle),
    time_style: Option(TimeStyle),
    explicit: List(DtfComponent),
    components: DtfComponents,
    bound_format: Option(Ref),
  )
}

/// Intl.PluralRules resolved options (§16.1.2 InitializePluralRules).
/// `[[CompactDisplay]]` rides on the compact `Notation` variant.
pub type PluralRulesState {
  PluralRulesState(
    locale: String,
    plural_type: String,
    notation: Notation,
    digits: IntlDigitOptions,
  )
}

/// Intl.ListFormat resolved options (§13.1.2).
pub type ListFormatState {
  ListFormatState(locale: String, list_type: String, style: String)
}

/// Intl.RelativeTimeFormat resolved options (§17.1.2). `numeric` here is the
/// "always"/"auto" enum option, not a number.
pub type RelativeTimeFormatState {
  RelativeTimeFormatState(
    locale: String,
    style: String,
    numeric: String,
    numbering_system: String,
  )
}

/// Intl.Segmenter resolved options (§18.1.2).
pub type SegmenterState {
  SegmenterState(locale: String, granularity: String)
}

/// Intl.DisplayNames resolved options (§12.1.2). `language_display` is only
/// present for type "language".
pub type DisplayNamesState {
  DisplayNamesState(
    locale: String,
    style: String,
    display_type: String,
    fallback: String,
    language_display: Option(String),
  )
}

/// A DurationFormat unit's resolved `[[<Unit>Style]]`. `DurFractional` is the
/// internal-only style a sub-second unit folds into when it rides on the
/// preceding numeric unit's fraction; resolvedOptions spells it "numeric"
/// (see `duration_unit_style_to_js_string`).
pub type DurationUnitStyle {
  DurLong
  DurShort
  DurNarrow
  DurNumeric
  DurTwoDigit
  DurFractional
}

/// The resolvedOptions spelling of a `[[<Unit>Style]]`.
pub fn duration_unit_style_to_js_string(v: DurationUnitStyle) -> String {
  case v {
    DurLong -> "long"
    DurShort -> "short"
    DurNarrow -> "narrow"
    DurNumeric -> "numeric"
    DurTwoDigit -> "2-digit"
    DurFractional -> "numeric"
  }
}

/// A DurationFormat unit's resolved `[[<Unit>Display]]`.
pub type DurationDisplay {
  DisplayAuto
  DisplayAlways
}

/// The resolvedOptions spelling of a `[[<Unit>Display]]`.
pub fn duration_display_to_js_string(v: DurationDisplay) -> String {
  case v {
    DisplayAuto -> "auto"
    DisplayAlways -> "always"
  }
}

/// The DurationFormat `style` option (`[[Style]]`).
pub type DurationBaseStyle {
  BsLong
  BsShort
  BsNarrow
  BsDigital
}

/// The resolvedOptions spelling of a `[[Style]]`.
pub fn duration_base_style_to_js_string(v: DurationBaseStyle) -> String {
  case v {
    BsLong -> "long"
    BsShort -> "short"
    BsNarrow -> "narrow"
    BsDigital -> "digital"
  }
}

/// One DurationFormat unit's resolved `[[<Unit>Style]]` / `[[<Unit>Display]]`
/// pair (GetDurationUnitOptions). `style` is the INTERNAL style.
pub type DurationUnitOptions {
  DurationUnitOptions(style: DurationUnitStyle, display: DurationDisplay)
}

/// Intl.DurationFormat resolved options (Intl.DurationFormat §1.1.3), one
/// `DurationUnitOptions` field per duration unit.
pub type DurationFormatState {
  DurationFormatState(
    locale: String,
    numbering_system: String,
    style: DurationBaseStyle,
    years: DurationUnitOptions,
    months: DurationUnitOptions,
    weeks: DurationUnitOptions,
    days: DurationUnitOptions,
    hours: DurationUnitOptions,
    minutes: DurationUnitOptions,
    seconds: DurationUnitOptions,
    milliseconds: DurationUnitOptions,
    microseconds: DurationUnitOptions,
    nanoseconds: DurationUnitOptions,
    fractional_digits: Option(Int),
  )
}

/// %SegmentsPrototype% instance state: the segmenter's granularity plus the
/// string being segmented.
pub type SegmentsState {
  SegmentsState(string: String, granularity: String)
}

/// %SegmentIteratorPrototype% instance state: a `SegmentsState` plus the
/// UTF-16 code-unit position the next segment starts at.
pub type SegmentIteratorState {
  SegmentIteratorState(string: String, granularity: String, position: Int)
}

/// Identifies an Intl native function (ECMA-402).
pub type IntlNativeFn {
  /// Intl.getCanonicalLocales(locales)
  IntlGetCanonicalLocales
  /// Intl.supportedValuesOf(key)
  IntlSupportedValuesOf
  /// new Intl.<Service>(locales, options) — proto is the intrinsic prototype.
  IntlConstructor(service: IntlService, proto: Ref)
  /// Intl.<Service>.supportedLocalesOf(locales, options)
  IntlSupportedLocalesOf(service: IntlService)
  /// Intl.<Service>.prototype.resolvedOptions()
  IntlResolvedOptions(service: IntlService)
  /// Accessor getter for NumberFormat/DateTimeFormat .format and
  /// Collator .compare — returns (and caches) a bound method.
  IntlBoundGetter(service: IntlService)
  /// The bound method produced by IntlBoundGetter — target is the instance.
  IntlBoundMethod(service: IntlService, target: Ref)
  /// Named prototype method (format/formatToParts/select/of/…). The
  /// receiver's brand (`service`) plus `method` pick the implementation
  /// inside the intl builtins module.
  IntlMethod(service: IntlService, method: IntlMethodName)
  /// ECMA-402 §17-19 locale-sensitive overrides installed on
  /// Number.prototype / String.prototype / Date.prototype — these are not
  /// Intl.* prototype methods and have no Intl brand check.
  IntlHostOverride(which: HostOverride)
  /// Segmenter.prototype.segment — needs the %SegmentsPrototype% ref.
  IntlSegmenterSegment(segments_proto: Ref)
  /// %SegmentsPrototype%[Symbol.iterator] — needs %SegmentIteratorPrototype%.
  IntlSegmentsIterator(iter_proto: Ref)
  /// Intl.Locale.prototype getter (language/script/region/baseName/…).
  IntlLocaleGetter(name: LocaleGetterName)
  /// Intl.Locale.prototype method needing the Locale prototype to allocate
  /// result Locale objects (maximize/minimize) or plain (toString).
  IntlLocaleMethod(method: LocaleMethodName, proto: Ref)
}

/// The Intl.<Service>.prototype methods registered via `IntlMethod` —
/// one variant per method name so a registration typo is a compile error,
/// not a silent fallthrough at dispatch time.
pub type IntlMethodName {
  /// ListFormat/RelativeTimeFormat/DurationFormat.prototype.format
  IntlFormat
  /// NumberFormat/DateTimeFormat/ListFormat/RelativeTimeFormat/
  /// DurationFormat.prototype.formatToParts
  IntlFormatToParts
  /// NumberFormat/DateTimeFormat.prototype.formatRange
  IntlFormatRange
  /// NumberFormat/DateTimeFormat.prototype.formatRangeToParts
  IntlFormatRangeToParts
  /// PluralRules.prototype.select
  IntlSelect
  /// PluralRules.prototype.selectRange
  IntlSelectRange
  /// DisplayNames.prototype.of
  IntlOf
  /// %SegmentIteratorPrototype%.next
  IntlSegmentIteratorNext
  /// %SegmentsPrototype%.containing
  IntlSegmentsContaining
}

/// The ECMA-402 locale-sensitive host overrides (§17-19) installed on the
/// Number / String / Date prototypes at Intl init.
pub type HostOverride {
  /// Number.prototype.toLocaleString (§18.2.1)
  NumberToLocaleString
  /// String.prototype.localeCompare (§19.1.1)
  StringLocaleCompare
  /// String.prototype.toLocaleLowerCase (§19.1.2)
  StringToLocaleLowerCase
  /// String.prototype.toLocaleUpperCase (§19.1.3)
  StringToLocaleUpperCase
  /// Date.prototype.toLocaleString (§17.4.1)
  DateToLocaleString
  /// Date.prototype.toLocaleDateString (§17.4.2)
  DateToLocaleDateString
  /// Date.prototype.toLocaleTimeString (§17.4.3)
  DateToLocaleTimeString
}

/// The Intl.Locale.prototype accessor getters.
pub type LocaleGetterName {
  LocaleBaseName
  LocaleCalendar
  LocaleCaseFirst
  LocaleCollation
  LocaleFirstDayOfWeek
  LocaleHourCycle
  LocaleNumeric
  LocaleNumberingSystem
  LocaleLanguage
  LocaleScript
  LocaleRegion
  LocaleVariants
}

/// The Intl.Locale.prototype methods.
pub type LocaleMethodName {
  LocaleToString
  LocaleMaximize
  LocaleMinimize
  LocaleGetCalendars
  LocaleGetCollations
  LocaleGetHourCycles
  LocaleGetNumberingSystems
  LocaleGetTimeZones
  LocaleGetTextInfo
  LocaleGetWeekInfo
}

/// Which way an element of an allSettled-style combinator settled. Names the
/// pair of strings the result wrapper object uses, so a `#(status, field)`
/// tuple can never be built with a swapped or mismatched pair.
pub type SettledOutcome {
  Fulfilled
  Rejected
}

/// The `#(status, value-field-name)` pair for an allSettled result wrapper:
/// `{status: "fulfilled", value: v}` / `{status: "rejected", reason: r}`.
pub fn settled_keys(outcome: SettledOutcome) -> #(String, String) {
  case outcome {
    Fulfilled -> #("fulfilled", "value")
    Rejected -> #("rejected", "reason")
  }
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
  /// Promise.allKeyed(promises) — await-dictionary proposal.
  PromiseAllKeyedStatic
  /// Promise.allSettledKeyed(promises) — await-dictionary proposal.
  PromiseAllSettledKeyedStatic
  /// Per-element handler for Promise.allKeyed / Promise.allSettledKeyed
  /// (PerformPromiseAllKeyed fulfilled/rejected element closures).
  /// `status_field` is None for the allKeyed fulfill handler (stores the raw
  /// value); Some(Fulfilled) / Some(Rejected) for the allSettledKeyed handlers
  /// (stores a status wrapper object built from `settled_keys`).
  PromiseKeyedElement(
    index: Int,
    remaining_ref: Ref,
    keys_ref: Ref,
    values_ref: Ref,
    already_called_ref: Ref,
    resolve: JsValue,
    status_field: Option(SettledOutcome),
  )
  /// GetCapabilitiesExecutor (§27.2.1.5.1) for NewPromiseCapability with a
  /// custom constructor — the boxes receive the resolve/reject functions the
  /// constructor passes to its executor.
  PromiseCapabilityExecutor(resolve_box: Ref, reject_box: Ref)
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
  /// kind distinguishes body-await / AwaitingReturn / yield*-delegate awaits.
  AsyncGeneratorResume(data_ref: Ref, is_reject: Bool, kind: AGResumeKind)
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
  /// Array.fromAsync(asyncItems [, mapfn [, thisArg]]) — §23.1.2.1.
  ArrayFromAsync
  /// fromAsync iterator path: onFulfilled for the awaited next() result.
  ArrayFromAsyncOnNext(ctx: FromAsyncCtx)
  /// fromAsync iterator path: onFulfilled for the awaited mapfn result.
  ArrayFromAsyncOnMapped(ctx: FromAsyncCtx)
  /// fromAsync iterator path: onRejected that performs AsyncIteratorClose
  /// on `iter` and then rejects with the rejection reason.
  ArrayFromAsyncCloseReject(iter: JsValue, reject: JsValue)
  /// fromAsync: rejects with the captured original error regardless of its
  /// argument — used after awaiting AsyncIteratorClose's return() result.
  ArrayFromAsyncRejectWith(error: JsValue, reject: JsValue)
  /// fromAsync array-like path: onFulfilled for the awaited element value.
  ArrayFromAsyncLikeOnValue(ctx: FromAsyncLikeCtx)
  /// fromAsync array-like path: onFulfilled for the awaited mapfn result.
  ArrayFromAsyncLikeOnMapped(ctx: FromAsyncLikeCtx)
  /// Symbol() constructor — callable but NOT new-able.
  SymbolConstructor
  /// Proxy(target, handler) constructor — new-able but NOT callable (§28.2.1).
  ProxyConstructor
  /// Proxy.revocable(target, handler) — §28.2.2.1.
  ProxyRevocable
  /// The revoke closure returned by Proxy.revocable. Carries the proxy's Ref;
  /// invoking it nulls the proxy's [[ProxyTarget]]/[[ProxyHandler]].
  ProxyRevoke(proxy: Ref)
  /// Symbol.for(key) — global symbol registry lookup/insert.
  SymbolFor
  /// Symbol.keyFor(sym) — reverse lookup in global symbol registry.
  SymbolKeyFor
  /// §20.4.3.3 Symbol.prototype.toString — SymbolDescriptiveString(thisSymbolValue).
  SymbolPrototypeToString
  /// §20.4.3.4 Symbol.prototype.valueOf — thisSymbolValue.
  SymbolPrototypeValueOf
  /// §20.4.3.2 get Symbol.prototype.description — [[Description]] or undefined.
  SymbolDescriptionGetter
  /// §20.4.3.5 Symbol.prototype[Symbol.toPrimitive] — thisSymbolValue (hint ignored).
  SymbolPrototypeToPrimitive
}

/// Captured state for Array.fromAsync's async-iterator loop continuations.
/// `map_fn` is JsUndefined when no mapping function was supplied.
pub type FromAsyncCtx {
  FromAsyncCtx(
    iter: JsValue,
    next_method: JsValue,
    map_fn: JsValue,
    this_arg: JsValue,
    target: JsValue,
    k: Int,
    resolve: JsValue,
    reject: JsValue,
  )
}

/// Captured state for Array.fromAsync's array-like loop continuations.
pub type FromAsyncLikeCtx {
  FromAsyncLikeCtx(
    items: JsValue,
    map_fn: JsValue,
    this_arg: JsValue,
    target: JsValue,
    k: Int,
    len: Int,
    resolve: JsValue,
    reject: JsValue,
  )
}

/// VM-level natives handled in dispatch_native — don't need stack manipulation.
pub type VmNativeFn {
  FunctionConstructor
  /// §27.3.1.1 GeneratorFunction ( ...parameterArgs, bodyArg ) — like
  /// Function but builds `function* anonymous(...)`.
  GeneratorFunctionConstructor
  /// §27.4.1.1 AsyncGeneratorFunction ( ...parameterArgs, bodyArg ).
  AsyncGeneratorFunctionConstructor
  /// §27.7.1.1 AsyncFunction ( ...parameterArgs, bodyArg ) — like Function
  /// but builds `async function anonymous(...)`.
  AsyncFunctionConstructor
  FunctionToString
  /// %IteratorPrototype%[Symbol.iterator]() — returns `this`.
  IteratorSymbolIterator
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
  /// BigInt ( value ) — §21.2.1.1. Callable only (new BigInt throws).
  BigIntGlobal
  /// BigInt.prototype.toString ( [ radix ] ) — §21.2.3.3.
  BigIntPrototypeToString
  /// BigInt.prototype.toLocaleString ( [ reserved1 [ , reserved2 ] ] ) —
  /// §21.2.3.2. NOT toString: its first argument is `locales`, not a radix.
  BigIntPrototypeToLocaleString
  /// BigInt.prototype.valueOf ( ) — §21.2.3.4.
  BigIntPrototypeValueOf
  /// %ThrowTypeError% (§10.2.4.1) — the poison-pill accessor installed for
  /// Function.prototype's restricted "caller"/"arguments" properties.
  ThrowTypeErrorFn
  /// Function.prototype [ @@hasInstance ] ( V ) — §20.2.3.6
  /// OrdinaryHasInstance(this, V).
  FunctionHasInstance
  /// §20.2.3 "Function.prototype … accepts any arguments and returns
  /// undefined when invoked" — the [[Call]] of %Function.prototype% itself.
  FunctionPrototypeCall
}

/// Opaque handle to an Erlang `atomics` array (see arc_sab_ffi.erl). Atomics
/// refs pass between BEAM processes by reference, so every process sharing
/// the ref reads and writes the SAME mutable cells — exactly the semantics a
/// SharedArrayBuffer needs across real agent processes.
pub type AtomicsRef

/// Backing storage of an ArrayBuffer/SharedArrayBuffer ([[ArrayBufferData]]).
///
/// Non-shared ArrayBuffers keep the original immutable-BitArray
/// representation (`BufBytes`) — the fast path is a single one-constructor
/// unwrap. SharedArrayBuffers live in an Erlang `atomics` array
/// (`BufShared`): one unsigned 64-bit cell per 8 bytes, little-endian within
/// the cell, sub-word writes via a compare_exchange retry loop (the cell
/// mapping is documented in arc_sab_ffi.erl). Growable SABs pre-allocate
/// max_byte_length cells up front. `BufShared` carries NOTHING but the ref:
/// the CURRENT [[ArrayBufferByteLength]] of a SAB lives in the shared cells
/// too (§25.2.2.1 makes it a shared 8-byte block), so a `grow` in one agent
/// is observed by every other agent holding the same buffer.
pub type BufferData {
  BufBytes(bytes: BitArray)
  BufShared(ref: AtomicsRef)
}

/// Allocate a fresh zero-filled shared storage able to hold
/// `max_byte_length` bytes, whose current [[ArrayBufferByteLength]] is
/// `byte_length` (atomics data cells are zero-initialized by the VM).
@external(erlang, "arc_sab_ffi", "new")
pub fn sab_new(max_byte_length: Int, byte_length: Int) -> AtomicsRef

/// The SAB's current [[ArrayBufferByteLength]], read out of the shared cell
/// that holds it — every agent sees the same value.
@external(erlang, "arc_sab_ffi", "byte_length")
pub fn sab_byte_length(ref: AtomicsRef) -> Int

/// Result of a §25.2.2.2 GrowSharedArrayBuffer on the shared length cell.
/// `TooSmall` means another agent already grew the buffer past `new_length`
/// (the length is monotonic), which is a RangeError for the caller.
pub type SabGrowResult {
  Grown
  TooSmall
}

/// Publish a new (larger) [[ArrayBufferByteLength]] into the shared length
/// cell via a monotonic compare-exchange loop.
@external(erlang, "arc_sab_ffi", "grow")
pub fn sab_grow(ref: AtomicsRef, new_byte_length: Int) -> SabGrowResult

/// Read `count` bytes starting at `byte_offset` out of shared storage.
@external(erlang, "arc_sab_ffi", "read_bytes")
pub fn sab_read_bytes(ref: AtomicsRef, byte_offset: Int, count: Int) -> BitArray

/// Write `bytes` into shared storage at `byte_offset`. Whole-cell spans use
/// atomics:put; partial cells merge via a CAS loop so concurrent writers of
/// neighbouring bytes in the same cell are never clobbered.
@external(erlang, "arc_sab_ffi", "write_bytes")
pub fn sab_write_bytes(
  ref: AtomicsRef,
  byte_offset: Int,
  bytes: BitArray,
) -> Nil

/// Whether the storage is shared across agents (SharedArrayBuffer backing).
/// This is THE definition of shared-ness — there is no separate flag; a
/// buffer is shared iff its storage is `BufShared`.
pub fn buffer_is_shared(data: BufferData) -> Bool {
  case data {
    BufShared(..) -> True
    BufBytes(..) -> False
  }
}

/// [[ArrayBufferByteLength]] of a storage value.
pub fn buffer_byte_size(data: BufferData) -> Int {
  case data {
    BufBytes(bytes:) -> bit_array.byte_size(bytes)
    BufShared(ref:) -> sab_byte_length(ref)
  }
}

/// Snapshot the live buffer contents as a BitArray. For `BufBytes` this is
/// the (immutable) backing binary itself — zero cost. For `BufShared` it
/// copies the current bytes out of the atomics cells.
pub fn buffer_bits(data: BufferData) -> BitArray {
  case data {
    BufBytes(bytes:) -> bytes
    BufShared(ref:) -> sab_read_bytes(ref, 0, sab_byte_length(ref))
  }
}

/// Persist a full-buffer image `new_bits`, but for SHARED storage write back
/// only the bytes in [byte_offset, byte_offset+count) — the region the
/// caller actually modified. Other regions of a shared buffer may be
/// concurrently mutated by other agent processes; writing the whole snapshot
/// back would clobber their updates.
///
/// The region MUST lie inside `new_bits`: every caller has already validated
/// the write range against the live buffer, so an out-of-range region is an
/// arithmetic bug in the caller — crash rather than silently drop the store.
pub fn buffer_store_region(
  data: BufferData,
  new_bits: BitArray,
  byte_offset: Int,
  count: Int,
) -> BufferData {
  case data {
    BufBytes(bytes: _) -> BufBytes(bytes: new_bits)
    BufShared(ref:) -> {
      let assert Ok(region) = bit_array.slice(new_bits, byte_offset, count)
        as "buffer_store_region: write range outside the new buffer image"
      let Nil = sab_write_bytes(ref, byte_offset, region)
      BufShared(ref:)
    }
  }
}

/// Distinguishes the kind of object stored in a unified ObjectSlot.
/// Generic over `ctx` because NativeFunction carries a NativeFnSlot(ctx).
pub type ExoticKind(ctx, host) {
  /// Plain JS object: `{}`, `new Object()`, prototypes, etc.
  OrdinaryObject
  /// Error instance — has the [[ErrorData]] internal slot (ES2024 §20.5.4).
  /// Otherwise an ordinary object. `stack` is the captured stack-trace string
  /// surfaced by the `Error.prototype.stack` accessor (error-stack-accessor
  /// proposal); instances carry NO own "stack" data property.
  ErrorObject(stack: String)
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
  /// `env` points to the EnvSlot holding captured variables. Arrows capture
  /// the enclosing frame's `this` via `env` — see FuncTemplate.this_slot.
  /// `home_object` is the ES [[HomeObject]]: the object a method was defined
  /// on (a class prototype, the constructor for statics, or an object literal).
  /// `super.x` resolves against its [[Prototype]]. None for plain functions.
  FunctionObject(
    func_template: FuncTemplate,
    env: Ref,
    home_object: option.Option(Ref),
  )
  /// Built-in function implemented in Gleam, not bytecode.
  /// Callable like any function but dispatches to native code.
  /// `constructible` is the stored [[Construct]] capability (ES2024 §7.2.4):
  /// True for the constructor intrinsics (Array, Map, …) and for bound
  /// functions whose target is a constructor; False for ordinary built-ins
  /// (Math.*, prototype methods, promise reaction jobs, …).
  NativeFunction(native: NativeFnSlot(ctx), constructible: Bool)
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
  /// Boxed BigInt primitive (`Object(1n)` only; `new BigInt()` is a
  /// TypeError). Has [[BigIntData]] (§21.2.4). Ordinary object aside from
  /// the internal slot.
  BigIntObject(value: BigInt)
  /// Boxed Symbol (`Object(sym)` only; `new Symbol()` is a TypeError).
  /// Has [[SymbolData]]. Ordinary object aside from the internal slot.
  SymbolObject(value: SymbolId)
  /// ShadowRealm instance (proposal-shadowrealm). `realm_ref` is the
  /// [[ShadowRealm]] internal slot — a RealmSlot ref on the heap whose
  /// builtins are registered in state.ctx.realms.
  ShadowRealmObject(realm_ref: Ref)
  /// Opaque, embedder-owned host value. `value` is the embedder's own typed
  /// type (`host`) — the engine never inspects it, only ferries it and renders
  /// it via the prototype's `@@toStringTag`. Minted with `host.alloc_host_object`
  /// (typically a null prototype + no own properties) and read back typed via
  /// `host.read_host` — no `Dynamic`, no coerce. Any heap refs the value needs
  /// live in the object's properties (traced for free), not in the payload.
  /// The default `host = Empty` is uninhabited, so this is unconstructable in a
  /// default engine.
  HostObject(value: host)
  /// Map object — ES2024 §24.1 Map Objects.
  /// Stores key-value pairs using SameValueZero equality.
  /// `store` maps normalized MapKey → value and models the spec's append-only
  /// [[MapData]] insertion order (see `arc/vm/internal/ordered_entries`).
  /// Original JS keys are reconstructed via `map_key_to_js` (lossless inverse
  /// modulo -0→+0, which §24.1.3.9 step 4 mandates anyway), so no second dict
  /// is needed. delete() leaves a seq gap (the spec's emptied record) and a
  /// re-added key appends past every live iterator's cursor, so it is
  /// revisited per §24.1.5.
  MapObject(store: OrderedEntries(MapKey, JsValue))
  /// Set object — ES2024 §24.2 Set Objects.
  /// Stores unique values using SameValueZero equality.
  /// `store` maps normalized MapKey → original JsValue and models the spec's
  /// append-only [[SetData]] insertion order, exactly as MapObject does.
  SetObject(store: OrderedEntries(MapKey, JsValue))
  /// WeakMap object — ES2024 §24.3 WeakMap Objects.
  /// Keys are canonical JsValues for which CanBeHeldWeakly is true:
  /// `JsObject(ref)` (identity) or `JsSymbol(id)` (non-registered symbols).
  /// No iteration, no size.
  /// Not truly weak (GC doesn't collect entries) but API-compatible.
  WeakMapObject(data: Dict(JsValue, JsValue))
  /// WeakSet object — ES2024 §24.4 WeakSet Objects.
  /// Members are canonical JsValues for which CanBeHeldWeakly is true:
  /// `JsObject(ref)` (identity) or `JsSymbol(id)` (non-registered symbols).
  /// No iteration, no size.
  /// Not truly weak (GC doesn't collect entries) but API-compatible.
  WeakSetObject(data: Dict(JsValue, Nil))
  /// FinalizationRegistry object — ES2021 §26.2 FinalizationRegistry Objects.
  /// `cells` is [[Cells]] (each cell holds [[WeakRefTarget]], [[HeldValue]],
  /// [[UnregisterToken]]); `callback` is [[CleanupCallback]].
  /// GC never empties cells in this implementation (objects are not collected
  /// while reachable from a registry), so cleanup callbacks never fire — but
  /// register/unregister bookkeeping is fully implemented.
  FinalizationRegistryObject(cells: List(FinRegCell), callback: JsValue)
  /// DisposableStack / AsyncDisposableStack object — Explicit Resource
  /// Management proposal §12.3 / §12.4. `async` distinguishes the two brands
  /// ([[DisposableState]] vs [[AsyncDisposableState]] internal slots).
  /// `state` is that slot's value plus the [[DisposeCapability]] it governs
  /// (see `DisposableState`).
  DisposableStackObject(async: Bool, state: DisposableState)
  /// ArrayBuffer / SharedArrayBuffer — ES2024 §25.1/§25.2.
  /// [[ArrayBufferData]] is `data`: `None` is the spec's null (a DETACHED
  /// buffer — there is no separate `detached` flag, so "detached but still
  /// holding bytes" is unrepresentable and every read of a buffer's bytes is
  /// an unwrap the compiler forces you to handle). `Some(BufBytes(_))` is an
  /// immutable BEAM binary (non-shared buffers); `Some(BufShared(_))` is an
  /// Erlang `atomics` array shared across BEAM processes
  /// (SharedArrayBuffers). [[ArrayBufferByteLength]] is derived
  /// (`buffer_byte_size(data)`).
  /// `max_byte_length` is Some for resizable (AB) / growable (SAB) buffers.
  /// Shared-ness (SharedArrayBuffer, never detachable) is NOT a separate
  /// flag — it is derived from the storage: `buffer_is_shared(data)` is True
  /// iff `data` is `BufShared`. This makes "flagged shared but backed by
  /// process-local bytes" unrepresentable.
  /// `immutable` is the TC39 Immutable ArrayBuffer proposal's
  /// IsImmutableBuffer state (transferToImmutable / sliceToImmutable
  /// results): always BufBytes, never shared, never detachable, never
  /// resizable; every write path (Atomics, TypedArray stores) rejects it.
  ArrayBufferObject(
    data: option.Option(BufferData),
    max_byte_length: option.Option(Int),
    immutable: Bool,
  )
  /// Integer-Indexed (TypedArray) exotic object — ES2024 §10.4.5 / §23.2.
  /// [[ViewedArrayBuffer]] is `buffer` (an ArrayBufferObject slot),
  /// [[TypedArrayName]]/[[ContentType]] derive from `elem_kind`,
  /// [[ByteOffset]] is `byte_offset`, [[ArrayLength]] is `length` (elements,
  /// not bytes). `length: None` is [[ArrayLength]] = AUTO — a length-tracking
  /// view over a resizable buffer whose element count follows the buffer's
  /// live byte length (§10.4.5.13 TypedArrayLength). Element reads/writes go
  /// through the buffer's BitArray.
  TypedArrayObject(
    buffer: Ref,
    elem_kind: TypedArrayKind,
    byte_offset: Int,
    length: option.Option(Int),
  )
  /// RegExp object — ES2024 §22.2 RegExp Objects.
  /// Stores the source pattern and flags strings. Actual matching
  /// is delegated to Erlang's `re` module (PCRE) via FFI.
  RegExpObject(pattern: String, flags: String)
  /// Date object — ES2024 §21.4. Has [[DateValue]] internal slot holding the
  /// time value (ms since epoch) as a JsNum. NaN represents an invalid date.
  /// After TimeClip only Finite or NaN are possible, but the type stays JsNum.
  DateObject(time_value: JsNum)
  /// Intl service instance (ECMA-402) — Intl.Locale, Intl.NumberFormat, etc.
  /// `data` holds the resolved internal slots ([[Locale]], [[Style]], …) as
  /// a typed per-service record; the brand is `intl_service(data)`.
  IntlObject(data: IntlData)
  /// DataView object -- ES2024 Section 25.3. [[ViewedArrayBuffer]] is `buffer`,
  /// [[ByteOffset]] is `byte_offset`. `byte_length: None` means byte-length
  /// auto-tracking (view over a resizable buffer with no explicit length).
  DataViewObject(buffer: Ref, byte_offset: Int, byte_length: option.Option(Int))
  /// Temporal.PlainDate — ISO calendar date plus its calendar.
  /// year/month/day are always the ISO 8601 date.
  ///
  /// `calendar` (here and on the other Temporal slots) is the closed
  /// `temporal_calendar.Calendar` type, only ever minted by
  /// `temporal_calendar.canonicalize` — a slot cannot hold an unsupported
  /// calendar, and reading one back needs no re-parse.
  TemporalDateSlot(year: Int, month: Int, day: Int, calendar: Calendar)
  /// Temporal.PlainTime — wall-clock time, nanosecond precision.
  TemporalTimeSlot(
    hour: Int,
    minute: Int,
    second: Int,
    millisecond: Int,
    microsecond: Int,
    nanosecond: Int,
  )
  /// Temporal.PlainDateTime — combined ISO date + wall-clock time.
  TemporalDateTimeSlot(
    year: Int,
    month: Int,
    day: Int,
    hour: Int,
    minute: Int,
    second: Int,
    millisecond: Int,
    microsecond: Int,
    nanosecond: Int,
    calendar: Calendar,
  )
  /// Temporal.PlainYearMonth. `year`/`month`/`day` are the ISO date of the
  /// reference day; `calendar` as on TemporalDateSlot.
  TemporalYearMonthSlot(year: Int, month: Int, day: Int, calendar: Calendar)
  /// Temporal.PlainMonthDay. `month`/`day`/`ref_year` are the ISO date of
  /// the reference day; `calendar` as on TemporalDateSlot.
  TemporalMonthDaySlot(month: Int, day: Int, ref_year: Int, calendar: Calendar)
  /// Temporal.Duration — ten integral fields, all the same sign.
  TemporalDurationSlot(
    years: Int,
    months: Int,
    weeks: Int,
    days: Int,
    hours: Int,
    minutes: Int,
    seconds: Int,
    milliseconds: Int,
    microseconds: Int,
    nanoseconds: Int,
  )
  /// Temporal.Instant — exact time as nanoseconds since the epoch
  /// (BEAM Ints are arbitrary precision, so the full ±8.64e21 range fits).
  TemporalInstantSlot(epoch_ns: Int)
  /// Temporal.ZonedDateTime — exact time + time zone identifier. Only "UTC"
  /// and fixed-offset zones (canonical "±HH:MM" form) are supported.
  TemporalZonedDateTimeSlot(
    epoch_ns: Int,
    time_zone: String,
    calendar: Calendar,
  )
  /// Array iterator — ES2024 §23.1.5 Array Iterator Objects.
  /// Created by Array.prototype[Symbol.iterator](), values(), keys(), entries()
  /// (and the %TypedArray%.prototype counterparts).
  /// Lazy — re-reads source length each .next() to handle mutation.
  /// `iter_kind` is the [[ArrayIterationKind]] internal slot: what each
  /// .next() yields (index, element, or a fresh [index, element] pair).
  /// `cursor` is the next index to yield; `None` latches exhaustion — the
  /// spec's [[IteratedObject]] = undefined "already returned" state — so a
  /// done iterator stays done even if the source later grows or its buffer
  /// is resized.
  ArrayIteratorObject(
    source: Ref,
    cursor: Option(Int),
    iter_kind: ArrayIterKind,
  )
  /// String iterator — ES2024 §22.1.5 String Iterator Objects.
  /// Snapshots the string's code points at creation (strings are immutable,
  /// so unlike arrays there's no mutation to observe). O(1) per .next()
  /// instead of an O(i) UTF-8 walk per indexed read.
  StringIteratorObject(remaining: List(UtfCodepoint))
  /// RegExp String Iterator — ES2024 §22.2.9, created by
  /// RegExp.prototype[Symbol.matchAll]. `matcher` is [[IteratingRegExp]] —
  /// the object returned by Construct(C, ...), so always a Ref (any object
  /// with a callable `exec`, not necessarily a RegExpObject); `string` is
  /// [[IteratedString]]; `global`/`unicode` cache [[Global]]/[[Unicode]]
  /// from the flags read at creation; `done` latches [[Done]] once the
  /// iterator is exhausted so later .next() calls short-circuit.
  RegExpStringIteratorObject(
    matcher: Ref,
    string: String,
    global: Bool,
    unicode: Bool,
    done: Bool,
  )
  /// Set iterator — ES2024 §24.2.5. `cursor` is the iterator's index into
  /// the source Set's insertion-sequence space: .next() yields the first
  /// live entry with seq >= cursor and resumes at seq + 1 (amortized O(1)).
  /// Entries added during iteration — including delete + re-add, which
  /// assigns a fresh seq — are visited; entries deleted before being reached
  /// leave a gap that is skipped. `done` latches the spec's "generator
  /// returned" state so later additions never revive an exhausted iterator.
  SetIteratorObject(source: Ref, cursor: Int, done: Bool, kind: SetIterKind)
  /// Map iterator — ES2024 §24.1.5. Same cursor design as SetIteratorObject.
  MapIteratorObject(source: Ref, cursor: Int, done: Bool, kind: MapIterKind)
  /// Async-from-Sync Iterator — ES2024 §27.1.4. Created by GetIterator(async)
  /// when the source has only Symbol.iterator. next/return/throw await the
  /// sync result's `.value` and close the sync iterator on rejection.
  /// `sync_next` is the sync iterator record's [[NextMethod]], cached by
  /// GetIteratorFromMethod (§7.4.4) — .next() must NOT re-Get it per call.
  AsyncFromSyncIteratorObject(sync_iter: Ref, sync_next: JsValue)
  /// Iterator Helper — ES2025 §27.1.3.2. Created by
  /// Iterator.prototype.{map,filter,take,drop,flatMap}.
  /// `underlying`'s next method is cached once per GetIteratorDirect (spec
  /// §7.4.9) so monkey-patching `underlying.next` after creation has no effect.
  /// `kind` carries the combinator's own state (callback, take/drop
  /// remaining, flatMap inner iterator). `counter` is the spec's running
  /// element index passed to map/filter/flatMap callbacks; take/drop's
  /// countdown lives in their variants instead. `gen_state` is the closure
  /// generator's [[GeneratorState]]: GeneratorValidate rejects a reentrant
  /// next/return while it is `Executing`, and `Completed` latches forever.
  IteratorHelperObject(
    kind: IteratorHelperKind,
    underlying: IteratorRecord,
    counter: Int,
    gen_state: GeneratorState,
  )
  /// Wrap For Valid Iterator — ES2025 §27.1.2.1.2. Created by Iterator.from
  /// when the source isn't already an instance of %Iterator.prototype%.
  WrapForValidIteratorObject(iterated: JsValue, next_method: JsValue)
  /// Iterator.zip / Iterator.zipKeyed result — tc39 joint-iteration
  /// proposal's IteratorZip generator, modelled as a data-driven helper on
  /// %IteratorHelperPrototype%. `members` is the spec's iters list (a
  /// ZipExhausted member is the spec's null entry, and each member carries its
  /// own padding value); `keys` is None for Iterator.zip (array results) and
  /// Some(property keys) for Iterator.zipKeyed (null-proto object results).
  /// `gen_state` is the IteratorZip closure generator's [[GeneratorState]] —
  /// `Executing` makes a reentrant next/return a TypeError, and suspended-start
  /// vs suspended-yield picks the .return() path.
  IteratorZipObject(
    members: List(ZipMember),
    mode: ZipMode,
    keys: Option(List(ObjectKey)),
    gen_state: GeneratorState,
  )
  /// Iterator.concat result — tc39 iterator-sequencing proposal, modelled as
  /// a data-driven helper on %IteratorHelperPrototype%. `remaining` holds the
  /// not-yet-opened items; `inner` is the currently open iterator record. The
  /// two are different types on purpose — a `ConcatItem` cannot be handed to
  /// something expecting an already-opened `IteratorRecord`, or vice versa.
  /// `gen_state` is the closure generator's [[GeneratorState]] — a reentrant
  /// next/return while `Executing` is a TypeError.
  IteratorConcatObject(
    remaining: List(ConcatItem),
    inner: Option(IteratorRecord),
    gen_state: GeneratorState,
  )
  /// Module Namespace Exotic Object — ES2024 §10.4.6. `exports` maps each
  /// exported name to the BoxSlot ref holding the binding's live value, so
  /// [[Get]] re-reads the cell (and throws ReferenceError on a TDZ binding).
  /// String keys come from `exports` (sorted in [[OwnPropertyKeys]]); the only
  /// symbol key is @@toStringTag = "Module" (in `symbol_properties`). The
  /// object has a null prototype, is non-extensible, and is read-only.
  ModuleNamespace(exports: Dict(String, Ref))
  /// Proxy exotic object — ES2024 §10.5. `target`/`handler` are the
  /// [[ProxyTarget]]/[[ProxyHandler]] internal slots; both become None when
  /// the proxy is revoked (Proxy.revocable's revoke function). `callable` and
  /// `constructable` record whether the proxy has [[Call]]/[[Construct]] —
  /// fixed at creation time from the target (§10.5.15 ProxyCreate steps 4-7)
  /// and still meaningful after revocation (typeof of a revoked function
  /// proxy stays "function").
  ProxyObject(
    target: Option(Ref),
    handler: Option(Ref),
    callable: Bool,
    constructable: Bool,
  )
  /// Internal Iterator Record — ES2024 §7.4.1 GetIterator builds
  /// {Iterator, NextMethod, Done} with `next` fetched ONCE. The GetIterator
  /// opcode wraps user-defined iterators in this so IteratorNext calls the
  /// cached `next_method` instead of re-walking the prototype chain every
  /// iteration. Never exposed to JS: IteratorClose/CloseThrow/Rest/YieldStar
  /// unwrap to `iterated` before any dynamic .return/.throw lookup.
  IteratorRecordObject(iterated: JsValue, next_method: JsValue)
}

@external(erlang, "arc_vm_ffi", "unique_positive_integer")
fn unique_positive_integer() -> Int

/// §15.7.14 ClassDefinitionEvaluation step 5/6: mint the storage-key text for
/// a fresh per-class-evaluation PrivateName. The text is carried at runtime as
/// a JsString bound to a class-scope const named after the source text ("#m");
/// access ops wrap it back into a key with `key.private_key_from_text`. The
/// format and the hidden namespace it belongs to are owned by
/// `key.private_key_text`.
pub fn mint_private_key(name: String) -> String {
  key.private_key_text(name, unique_positive_integer())
}

/// Source-text name ("#m") from a minted private storage-key text, for error
/// messages. Alias for `key.private_display_name`: the runtime carries minted
/// keys as bare `String`s, so error sites reach for it here.
pub fn private_display_name(key_text: String) -> String {
  key.private_display_name(key_text)
}

/// Whether a PropertyKey is a class private element. Reflection sites call
/// this to skip private keys. See `key.is_private_key`.
pub fn is_private_name(key: PropertyKey) -> Bool {
  key.is_private_key(key)
}

/// A property key as it can actually exist on an object: §6.1.7 says that is
/// exactly a String or a Symbol, so this closed sum is what
/// [[OwnPropertyKeys]] / [[GetOwnProperty]] / [[DefineOwnProperty]] speak.
/// A `JsValue` key would also admit numbers, objects, `undefined`, … — none of
/// which any internal method can ever see, yet every `case` would have to
/// invent a fallback for them (and those fallbacks silently dropped keys).
///
/// The `display` string is NOT just for error messages: proxy
/// `defineProperty` / `getOwnPropertyDescriptor` traps receive it as the
/// property-key argument, so it must be the exact ToPropertyKey string —
/// `key.key_to_text`, never `key.key_display_string`.
pub type ObjectKey {
  StringPropKey(pkey: PropertyKey, display: String)
  SymbolPropKey(sym: SymbolId)
}

/// Property descriptor — writable/enumerable/configurable flags per property.
/// Following QuickJS: bit-flags on every property.
///
/// `seq` is the property-creation sequence number implementing §10.1.11
/// OrdinaryOwnPropertyKeys step 3: own string keys that are NOT array indices
/// enumerate in ascending chronological order of property creation. Stamped
/// from a runtime-global monotonic counter when the property record is built
/// (next_prop_seq); only relative order within one object matters.
/// INVARIANT: redefining/updating an EXISTING key must KEEP the old seq (the
/// key keeps its enumeration position); delete + re-add gets a fresh seq (the
/// key moves to the end). Index keys and symbol keys ignore seq — indices
/// sort numerically, symbols live in the creation-ordered symbol_properties
/// list.
pub type Property {
  DataProperty(
    value: JsValue,
    writable: Bool,
    enumerable: Bool,
    configurable: Bool,
    seq: Int,
  )
  AccessorProperty(
    get: Option(JsValue),
    set: Option(JsValue),
    enumerable: Bool,
    configurable: Bool,
    seq: Int,
  )
}

/// Fresh property-creation sequence number — see Property.seq.
@external(erlang, "arc_vm_ffi", "next_prop_seq")
pub fn next_prop_seq() -> Int

/// Creation-order sequence number of a property — see Property.seq.
pub fn prop_seq(prop: Property) -> Int {
  case prop {
    DataProperty(seq:, ..) | AccessorProperty(seq:, ..) -> seq
  }
}

/// Give an already-built descriptor a FRESH creation seq, keeping everything
/// else (including function-object identity in accessor get/set slots).
///
/// Two distinct keys must never share a Property record: equal seqs are an
/// ordering tie whose resolution depends on the property table's iteration
/// order. When one descriptor value is installed under two keys (e.g.
/// Function.prototype's "caller" and "arguments", which share the single
/// %ThrowTypeError% intrinsic), restamp the second one.
pub fn restamp(prop: Property) -> Property {
  case prop {
    DataProperty(value:, writable:, enumerable:, configurable:, ..) ->
      DataProperty(
        value:,
        writable:,
        enumerable:,
        configurable:,
        seq: next_prop_seq(),
      )
    AccessorProperty(get:, set:, enumerable:, configurable:, ..) ->
      AccessorProperty(
        get:,
        set:,
        enumerable:,
        configurable:,
        seq: next_prop_seq(),
      )
  }
}

/// Carry an existing property's seq onto a replacement descriptor — used by
/// update/redefine paths, which must keep the key's enumeration position.
pub fn with_seq_of(prop: Property, old: Property) -> Property {
  let seq = prop_seq(old)
  case prop {
    DataProperty(value:, writable:, enumerable:, configurable:, ..) ->
      DataProperty(value:, writable:, enumerable:, configurable:, seq:)
    AccessorProperty(get:, set:, enumerable:, configurable:, ..) ->
      AccessorProperty(get:, set:, enumerable:, configurable:, seq:)
  }
}

/// Base builder: DataProperty with all flags False. Stamps a fresh creation
/// seq — builders are called at property-creation time, so build order
/// matches creation order.
pub fn data(val: JsValue) -> Property {
  DataProperty(
    value: val,
    writable: False,
    enumerable: False,
    configurable: False,
    seq: next_prop_seq(),
  )
}

/// AccessorProperty builder with a fresh creation seq.
pub fn accessor(
  get get: Option(JsValue),
  set set: Option(JsValue),
  enumerable enumerable: Bool,
  configurable configurable: Bool,
) -> Property {
  AccessorProperty(get:, set:, enumerable:, configurable:, seq: next_prop_seq())
}

/// Set writable to True (data properties only).
pub fn writable(prop: Property) -> Property {
  case prop {
    DataProperty(value:, enumerable:, configurable:, seq:, ..) ->
      DataProperty(value:, writable: True, enumerable:, configurable:, seq:)

    AccessorProperty(..) -> panic as "Accessor property cannot be made writable"
  }
}

/// Set enumerable to True.
pub fn enumerable(prop: Property) -> Property {
  case prop {
    DataProperty(value:, writable:, configurable:, seq:, ..) ->
      DataProperty(value:, writable:, enumerable: True, configurable:, seq:)

    AccessorProperty(get:, set:, configurable:, seq:, ..) ->
      AccessorProperty(get:, set:, enumerable: True, configurable:, seq:)
  }
}

/// Set configurable to True.
pub fn configurable(prop: Property) -> Property {
  case prop {
    DataProperty(value:, writable:, enumerable:, seq:, ..) ->
      DataProperty(value:, writable:, enumerable:, configurable: True, seq:)

    AccessorProperty(get:, set:, enumerable:, seq:, ..) ->
      AccessorProperty(get:, set:, enumerable:, configurable: True, seq:)
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

/// A properties dict's entries in §10.1.11 OrdinaryOwnPropertyKeys order:
/// array-index keys ascending numerically, then other string keys in
/// ascending property-creation order (Property.seq). Private names ("#x")
/// are INCLUDED (sorted by seq with the named keys) — callers doing JS-level
/// reflection must filter them with is_private_name; heap-level copy paths
/// (structured clone) must keep them.
///
/// Dict entries only — objects with an elements fast store (Array/Arguments)
/// have their element indices merged in by ops/object.own_string_keys_flagged,
/// the key-enumeration funnel.
pub fn ordered_property_pairs(
  properties: Dict(PropertyKey, Property),
) -> List(#(PropertyKey, Property)) {
  let #(idx, named) =
    dict.fold(properties, #([], []), fn(acc, key, prop) {
      let #(idx, named) = acc
      case key {
        Index(i) -> #([#(i, prop), ..idx], named)
        Named(_) | key.Private(_) -> #(idx, [#(key, prop), ..named])
      }
    })
  let idx =
    list.sort(idx, fn(a, b) { int.compare(a.0, b.0) })
    |> list.map(fn(pair) { #(Index(pair.0), pair.1) })
  let named =
    list.sort(named, fn(a, b) { int.compare(prop_seq(a.1), prop_seq(b.1)) })
  list.append(idx, named)
}

/// Build a properties dict from key/prop pairs, giving a key that appears
/// multiple times the FIRST occurrence's creation seq and the LAST
/// occurrence's descriptor — [[DefineOwnProperty]] redefinition semantics
/// (§10.1.11: a redefined key keeps its enumeration position). Use instead of
/// dict.from_list when the pair list may contain duplicate keys (e.g.
/// JSON.parse input).
pub fn props_dict_from_pairs(
  pairs: List(#(PropertyKey, Property)),
) -> Dict(PropertyKey, Property) {
  list.fold(pairs, dict.new(), fn(d, pair) {
    let #(key, prop) = pair
    case dict.get(d, key) {
      Ok(old) -> dict.insert(d, key, with_seq_of(prop, old))
      Error(Nil) -> dict.insert(d, key, prop)
    }
  })
}

/// Normal assignment: all flags true (obj.x = val, object literals, etc.)
pub fn data_property(val: JsValue) -> Property {
  // Direct construction (not the builder chain) — this is the hottest
  // property-creation path (object literals, element promotion, copies).
  DataProperty(
    value: val,
    writable: True,
    enumerable: True,
    configurable: True,
    seq: next_prop_seq(),
  )
}

/// Built-in methods/prototype props: writable+configurable, NOT enumerable.
/// This matches QuickJS and the spec for built-in function properties.
pub fn builtin_property(val: JsValue) -> Property {
  DataProperty(
    value: val,
    writable: True,
    enumerable: False,
    configurable: True,
    seq: next_prop_seq(),
  )
}

/// GC root tracing: prepend the heap ref held by a reaction handler's
/// callable, if any, onto `acc`. Pass-through handlers hold no value.
fn push_reaction_handler_ref(
  handler: ReactionHandler,
  acc: List(Ref),
) -> List(Ref) {
  case handler {
    Handler(fun:) -> push_value_ref(fun, acc)
    IdentityPassThrough | ThrowerPassThrough -> acc
  }
}

/// GC root tracing: prepend heap refs reachable from a Property onto `acc`.
fn push_property_refs(prop: Property, acc: List(Ref)) -> List(Ref) {
  case prop {
    DataProperty(value:, ..) -> push_value_ref(value, acc)
    AccessorProperty(get:, set:, ..) -> {
      let acc = case get {
        Some(v) -> push_value_ref(v, acc)
        None -> acc
      }
      case set {
        Some(v) -> push_value_ref(v, acc)
        None -> acc
      }
    }
  }
}

/// The [[Handler]] of a promise reaction (ES2024 §27.2.1.2).
///
/// The spec's "empty" handler (a `.then()` argument that is not callable)
/// is a distinct case, not a JsValue: a fulfill value can legitimately be
/// `undefined`, so it must never double as a "no handler" marker.
pub type ReactionHandler {
  /// A callable onFulfilled/onRejected — call it with the settled value.
  Handler(fun: JsValue)
  /// Empty onFulfilled: resolve the derived promise with the value as-is.
  IdentityPassThrough
  /// Empty onRejected: reject the derived promise with the reason as-is.
  ThrowerPassThrough
}

/// A microtask job for the promise job queue.
pub type Job {
  /// Run the reaction handler on `arg`, then resolve/reject the child promise.
  PromiseReactionJob(
    handler: ReactionHandler,
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
    handler: ReactionHandler,
  )
}

/// Saved try-frame for generator suspension (mirrors TryFrame from state.gleam).
pub type SavedTryFrame {
  SavedTryFrame(catch_target: Int, stack_depth: Int, kind: TryKind)
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

/// The two methods a `yield*` delegation forwards to the inner iterator.
/// `next` is deliberately absent: a next request never leaves the delegating
/// loop (the YieldStar / AsyncYieldStarNext opcodes call `.next` inline), so
/// only `.return`/`.throw` are ever forwarded. Naming exactly those two turns
/// what used to be a doc-comment invariant ("AGNext never reaches here") into
/// a compile error.
pub type DelegateMethod {
  DelegateReturn
  DelegateThrow
}

/// What an AsyncGeneratorResume callback is waiting on.
pub type AGResumeKind {
  /// Body `await` settled — push value / throw into body, continue executing.
  AGResumeBody
  /// .return(v) on a completed gen — settle the head request with awaited v.
  AGResumeAwaitingReturn
  /// yield* delegated iter.return()/iter.throw() result settled.
  /// Carries which of the two was forwarded, for done-dispatch.
  AGResumeDelegate(method: DelegateMethod)
  /// AsyncIteratorClose await settled — inner .throw was missing, so we
  /// closed the iter and now throw a TypeError into the body. Result/error
  /// from the close await is discarded per spec (outer abrupt completion
  /// takes precedence, ES §7.4.12 step 5).
  AGResumeDelegateClose
  /// yield* .return where the inner iterator has no `return` method: the
  /// spec's intermediate `Await(received.[[Value]])` settled (§27.5.3.8
  /// step 7.c.ii.2). Fulfil → unwind the outer generator's finally blocks
  /// with the awaited value; reject → throw into the body instead.
  AGResumeReturnUnwind
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
pub type HeapSlot(ctx, host) {
  /// Unified object slot — covers ordinary objects, arrays, and functions.
  ObjectSlot(
    kind: ExoticKind(ctx, host),
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
  /// Mutable integer cell — the promise combinators' remainingElementsCount
  /// Record { [[Value]]: n }. A dedicated slot rather than a `BoxSlot` holding
  /// a `JsNumber`, so the counter can never hold a non-number and silently
  /// stop decrementing (a `Promise.all` that never resolves).
  CounterSlot(count: Int)
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
    /// The body frame's own sloppy-direct-eval var dict and the line it
    /// suspended on. Both are per-frame `State` fields, so a resume that fails
    /// to restore them silently adopts the RESUMER's: `var`s a direct eval
    /// introduced inside the body vanish across a yield, and the resumed frame
    /// reports the resumer's line in stack traces.
    saved_eval_env: Option(Ref),
    saved_line: Int,
  )
  /// Engine-internal async function suspended state.
  /// Saves the full execution context so await can resume. There is no
  /// `env_ref`: the closure environment is read once at frame setup (its
  /// captures are copied into `saved_locals`) and never again, so a
  /// suspended body has no function environment to carry — module bodies,
  /// which suspend here on top-level await, have none at all.
  AsyncFunctionSlot(
    promise_data_ref: Ref,
    resolve: JsValue,
    reject: JsValue,
    func_template: FuncTemplate,
    saved_pc: Int,
    saved_locals: TupleArray(JsValue),
    saved_stack: List(JsValue),
    saved_try_stack: List(SavedTryFrame),
  )
  /// Engine-internal async generator state. The ObjectSlot has
  /// `kind: AsyncGeneratorObject(generator_data: Ref)` pointing here.
  /// Unlike sync generators, .next()/.return()/.throw() enqueue requests
  /// and return promises; yield settles the head request, await suspends
  /// without settling.
  AsyncGeneratorSlot(
    gen_state: AsyncGeneratorState,
    /// Two-list FIFO (Okasaki) as `#(front, back)`: front in dequeue order,
    /// back reversed (newest first). Enqueue prepends to back (O(1)); dequeue
    /// pops front, reversing back→front when front empties — O(1) amortized
    /// vs `list.append`'s O(n) per enqueue. Empty is `#([], [])`.
    ///
    /// IMPORTANT: this queue is mutated re-entrantly — user code running inside
    /// the generator body (or a getter run while settling) can call
    /// .next()/.return()/.throw(), which enqueues onto `back`. Writers in
    /// async_generators.gleam therefore never write a queue captured before
    /// user code ran; they re-read the live slot at write time (`write_live`).
    queue: #(List(AsyncGenRequest), List(AsyncGenRequest)),
    func_template: FuncTemplate,
    env_ref: Ref,
    saved_pc: Int,
    saved_locals: TupleArray(JsValue),
    saved_stack: List(JsValue),
    saved_try_stack: List(SavedTryFrame),
    /// See `GeneratorSlot.saved_eval_env` / `saved_line`.
    saved_eval_env: Option(Ref),
    saved_line: Int,
  )
  /// Stores realm context for $262 methods.
  /// evalScript and createRealm read this to know which realm to operate in.
  /// Builtins are stored in State.realms (keyed by this slot's Ref) to avoid
  /// an import cycle (value.gleam cannot import builtins/common.gleam).
  RealmSlot(
    global_object: Ref,
    lexical_globals: Dict(String, LexicalGlobal),
    symbol_registry: Dict(String, SymbolId),
  )
}

/// A binding in the global declarative record (let/const at global scope).
/// The wrapped value is `JsUninitialized` while in TDZ, then the bound value
/// after initialization. `Const` bindings reject assignment — PutGlobal throws
/// a TypeError.
pub type LexicalGlobal {
  Let(JsValue)
  Const(JsValue)
}

/// The bound value of a lexical global, regardless of let/const-ness.
pub fn lexical_global_value(g: LexicalGlobal) -> JsValue {
  case g {
    Let(v) | Const(v) -> v
  }
}

/// Replace the bound value of a lexical global, preserving let/const-ness.
/// Used by InitGlobalLex to fill a TDZ slot without losing its const flag.
pub fn set_lexical_global_value(
  g: LexicalGlobal,
  value: JsValue,
) -> LexicalGlobal {
  case g {
    Let(_) -> Let(value)
    Const(_) -> Const(value)
  }
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
    DataProperty(value:, writable:, enumerable:, configurable:, ..) -> [
      [],
      ["value:", string.inspect(value)],
      ["writable:", bool.to_string(writable)],
      ["enumerable:", bool.to_string(enumerable)],
      ["configurable:", bool.to_string(configurable)],
    ]
    AccessorProperty(get:, set:, enumerable:, configurable:, ..) -> [
      [],
      ["get:", string.inspect(get)],
      ["set:", string.inspect(set)],
      ["enumerable:", bool.to_string(enumerable)],
      ["configurable:", bool.to_string(configurable)],
    ]
  }
}

// will probably get rid of this function or move it and remake it.s
pub fn heap_slot_to_string(slot: HeapSlot(ctx, host)) -> String {
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
                  key.key_display_string(key) <> ":",
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

/// GC root tracing: prepend the heap ref carried by `value` (if any) onto `acc`.
/// Zero-alloc for primitives.
fn push_value_ref(value: JsValue, acc: List(Ref)) -> List(Ref) {
  case value {
    JsObject(ref) -> [ref, ..acc]
    JsUndefined
    | JsNull
    | JsBool(_)
    | JsNumber(_)
    | JsString(_)
    | JsSymbol(_)
    | JsBigInt(_)
    | JsUninitialized -> acc
  }
}

fn push_option_ref(r: Option(Ref), acc: List(Ref)) -> List(Ref) {
  case r {
    Some(ref) -> [ref, ..acc]
    None -> acc
  }
}

/// Both halves of an Iterator Record are heap-reachable: the iterator object
/// itself and its cached [[NextMethod]] (a bound function keeps its target and
/// receiver alive).
fn push_iter_record(rec: IteratorRecord, acc: List(Ref)) -> List(Ref) {
  push_value_ref(rec.next_method, push_value_ref(rec.iterator, acc))
}

fn push_option_value(v: Option(JsValue), acc: List(Ref)) -> List(Ref) {
  case v {
    Some(val) -> push_value_ref(val, acc)
    None -> acc
  }
}

/// GC root tracing for a [[DisposableResourceStack]]: every resource value,
/// dispose method, callback, and callback argument it holds.
fn push_dispose_resources(
  resources: List(DisposeResource),
  acc: List(Ref),
) -> List(Ref) {
  list.fold(resources, acc, fn(a, r) {
    case r {
      SyncDispose(value: v, method: m)
      | AsyncFallbackDispose(value: v, method: m) ->
        push_value_ref(m, push_value_ref(v, a))
      DisposeCallback(callback:, args:) ->
        list.fold(args, push_value_ref(callback, a), fn(a2, v) {
          push_value_ref(v, a2)
        })
      NullDispose -> a
    }
  })
}

fn push_saved_frame_refs(
  env_ref: Ref,
  saved_locals: TupleArray(JsValue),
  saved_stack: List(JsValue),
  acc: List(Ref),
) -> List(Ref) {
  push_saved_locals_stack_refs(saved_locals, saved_stack, [env_ref, ..acc])
}

/// Root an optional ref (a suspended frame's sloppy-direct-eval var dict): the
/// EnvSlot is reachable ONLY from the frame that owns it, so a suspended
/// generator's copy has to be walked or GC would free it out from under a
/// later resume.
fn push_opt_ref(ref: Option(Ref), acc: List(Ref)) -> List(Ref) {
  case ref {
    Some(r) -> [r, ..acc]
    None -> acc
  }
}

/// Same, for a suspended frame that owns no environment ref (an async
/// function's captures live in `saved_locals`; a module body has none).
fn push_saved_locals_stack_refs(
  saved_locals: TupleArray(JsValue),
  saved_stack: List(JsValue),
  acc: List(Ref),
) -> List(Ref) {
  let acc =
    list.fold(tuple_array.to_list(saved_locals), acc, fn(a, v) {
      push_value_ref(v, a)
    })
  list.fold(saved_stack, acc, fn(a, v) { push_value_ref(v, a) })
}

/// Prepend all refs reachable from a heap slot onto `acc`. 
pub fn refs_in_slot(
  slot: HeapSlot(ctx, host),
  host_refs: fn(host) -> List(Ref),
) -> List(Ref) {
  do_refs_in_slot(slot, host_refs, [])
}

fn do_refs_in_slot(
  slot: HeapSlot(ctx, host),
  host_refs: fn(host) -> List(Ref),
  acc: List(Ref),
) -> List(Ref) {
  case slot {
    ObjectSlot(
      kind:,
      properties:,
      elements:,
      prototype:,
      symbol_properties:,
      extensible: _,
    ) -> {
      let acc =
        dict.fold(properties, acc, fn(a, _k, prop) {
          push_property_refs(prop, a)
        })
      let acc =
        list.fold(symbol_properties, acc, fn(a, pair) {
          push_property_refs(pair.1, a)
        })
      let acc = case elements {
        NoElements -> acc
        DenseElements(data) ->
          tree_array.sparse_fold(
            fn(_i, v, a) { push_value_ref(v, a) },
            acc,
            data,
          )
        SparseElements(data) ->
          dict.fold(data, acc, fn(a, _i, v) { push_value_ref(v, a) })
      }
      let acc = push_option_ref(prototype, acc)
      case kind {
        FunctionObject(env: env_ref, home_object: home, ..) ->
          push_option_ref(home, [env_ref, ..acc])
        // Native functions: every dispatch/call payload is traced by the
        // exhaustive per-enum tracers below (native_fn_refs and friends), so
        // adding a Ref-carrying native-fn variant without teaching the GC
        // about it is a compile error, not a premature-collection bug.
        NativeFunction(native:, ..) -> native_fn_refs(native, acc)
        // ShadowRealm instances keep their realm record alive.
        ShadowRealmObject(realm_ref:) -> [realm_ref, ..acc]
        ProxyObject(target:, handler:, ..) ->
          push_option_ref(target, push_option_ref(handler, acc))
        PromiseObject(promise_data:) -> [promise_data, ..acc]
        GeneratorObject(generator_data:) -> [generator_data, ..acc]
        AsyncGeneratorObject(generator_data:) -> [generator_data, ..acc]
        ArrayIteratorObject(source:, ..) -> [source, ..acc]
        // The iterated matcher is the only heap reference the RegExp String
        // Iterator's internal state can hold; the rest is scalar.
        RegExpStringIteratorObject(matcher:, ..) -> [matcher, ..acc]
        SetIteratorObject(source:, ..) | MapIteratorObject(source:, ..) -> [
          source,
          ..acc
        ]
        AsyncFromSyncIteratorObject(sync_iter:, sync_next:) ->
          push_value_ref(sync_next, [sync_iter, ..acc])
        IteratorHelperObject(kind:, underlying:, ..) -> {
          let acc = push_iter_record(underlying, acc)
          case kind {
            HelperTake(remaining: _) | HelperDrop(remaining: _) -> acc
            HelperMap(func:)
            | HelperFilter(func:)
            | HelperFlatMap(func:, inner: None) -> push_value_ref(func, acc)
            HelperFlatMap(func:, inner: Some(inner)) ->
              push_iter_record(inner, push_value_ref(func, acc))
          }
        }
        WrapForValidIteratorObject(iterated:, next_method:)
        | IteratorRecordObject(iterated:, next_method:) ->
          push_value_ref(next_method, push_value_ref(iterated, acc))
        // `keys` holds no heap refs (an ObjectKey is a string or a symbol),
        // so only the members (iterator record + its padding value) are traced.
        IteratorZipObject(members:, ..) ->
          list.fold(members, acc, fn(a, m) {
            case m {
              ZipOpen(record:, padding:) ->
                push_value_ref(padding, push_iter_record(record, a))
              ZipExhausted(padding:) -> push_value_ref(padding, a)
            }
          })
        IteratorConcatObject(remaining:, inner:, ..) -> {
          let acc =
            list.fold(remaining, acc, fn(a, item) {
              push_value_ref(item.iterable, push_value_ref(item.open_method, a))
            })
          case inner {
            Some(inner) -> push_iter_record(inner, acc)
            None -> acc
          }
        }
        MapObject(store:) ->
          ordered_entries.fold(store, acc, fn(a, k, v) {
            push_value_ref(v, push_value_ref(map_key_to_js(k), a))
          })
        SetObject(store:) ->
          ordered_entries.fold(store, acc, fn(a, k, v) {
            push_value_ref(v, push_value_ref(map_key_to_js(k), a))
          })
        WeakMapObject(data:) ->
          dict.fold(data, acc, fn(a, k, v) {
            push_value_ref(v, push_value_ref(k, a))
          })
        WeakSetObject(data:) ->
          dict.fold(data, acc, fn(a, k, _v) { push_value_ref(k, a) })
        FinalizationRegistryObject(cells:, callback:) ->
          list.fold(cells, push_value_ref(callback, acc), fn(a, cell) {
            let FinRegCell(target:, held:, token:) = cell
            let a = push_value_ref(held, push_value_ref(target, a))
            case token {
              Some(t) -> push_value_ref(t, a)
              None -> a
            }
          })
        DisposableStackObject(state: Pending(resources:), ..) ->
          push_dispose_resources(resources, acc)
        DisposableStackObject(state: Disposed, ..) -> acc
        // The namespace's live bindings are BoxSlot refs reachable via exports.
        ModuleNamespace(exports:) ->
          dict.fold(exports, acc, fn(a, _name, box_ref) { [box_ref, ..a] })
        // The only heap refs an Intl instance can hold are the cached bound
        // `format`/`compare` function objects; everything else is scalar.
        // Exhaustive on purpose — a new IntlData variant that carries a Ref
        // MUST be added here or its target can be collected while reachable.
        IntlObject(data:) ->
          case data {
            CollatorData(CollatorState(bound_compare: Some(r), ..))
            | NumberFormatData(NumberFormatState(bound_format: Some(r), ..))
            | DateTimeFormatData(DateTimeFormatState(bound_format: Some(r), ..)) -> [
              r,
              ..acc
            ]
            // Ref-free: no cached bound function, or no heap slot at all.
            CollatorData(CollatorState(bound_compare: None, ..))
            | NumberFormatData(NumberFormatState(bound_format: None, ..))
            | DateTimeFormatData(DateTimeFormatState(bound_format: None, ..))
            | LocaleData(_)
            | PluralRulesData(_)
            | ListFormatData(_)
            | RelativeTimeFormatData(_)
            | SegmenterData(_)
            | DisplayNamesData(_)
            | DurationFormatData(_)
            | SegmentsData(_)
            | SegmentIteratorData(_) -> acc
          }
        // Ask the embedder for any engine refs reachable from the opaque
        // host value, so GC traces host objects that point back into the JS
        // heap. Pure host terms (pids, fds) return [] — the default hook.
        HostObject(value: h) -> list.append(host_refs(h), acc)
        OrdinaryObject
        | ErrorObject(_)
        | ArrayObject(_)
        | ArgumentsObject(_)
        | StringObject(_)
        | NumberObject(_)
        | BooleanObject(_)
        | BigIntObject(_)
        | SymbolObject(_)
        | DateObject(_)
        | TemporalDateSlot(..)
        | TemporalTimeSlot(..)
        | TemporalDateTimeSlot(..)
        | TemporalYearMonthSlot(..)
        | TemporalMonthDaySlot(..)
        | TemporalDurationSlot(..)
        | TemporalInstantSlot(..)
        | TemporalZonedDateTimeSlot(..)
        | RegExpObject(..)
        | ArrayBufferObject(..)
        | StringIteratorObject(_) -> acc
        // DataView keeps its viewed ArrayBuffer alive.
        DataViewObject(buffer:, ..) -> [buffer, ..acc]
        TypedArrayObject(buffer:, ..) -> [buffer, ..acc]
      }
    }
    EnvSlot(slots:) -> list.fold(slots, acc, fn(a, v) { push_value_ref(v, a) })
    BoxSlot(value:) -> push_value_ref(value, acc)
    // A plain Int — no reachable refs.
    CounterSlot(count: _) -> acc
    EvalEnvSlot(vars:) ->
      dict.fold(vars, acc, fn(a, _k, v) { push_value_ref(v, a) })
    ForInIteratorSlot(keys:) ->
      list.fold(keys, acc, fn(a, v) { push_value_ref(v, a) })
    PromiseSlot(state:, fulfill_reactions:, reject_reactions:, ..) -> {
      let acc = case state {
        PromiseFulfilled(value:) -> push_value_ref(value, acc)
        PromiseRejected(reason:) -> push_value_ref(reason, acc)
        PromisePending -> acc
      }
      let push_reactions = fn(reactions, a) {
        list.fold(reactions, a, fn(a, r: PromiseReaction) {
          a
          |> push_value_ref(r.child_resolve, _)
          |> push_value_ref(r.child_reject, _)
          |> push_reaction_handler_ref(r.handler, _)
        })
      }
      push_reactions(reject_reactions, push_reactions(fulfill_reactions, acc))
    }
    GeneratorSlot(env_ref:, saved_locals:, saved_stack:, saved_eval_env:, ..) ->
      push_saved_frame_refs(
        env_ref,
        saved_locals,
        saved_stack,
        push_opt_ref(saved_eval_env, acc),
      )
    AsyncFunctionSlot(
      promise_data_ref:,
      resolve:,
      reject:,
      saved_locals:,
      saved_stack:,
      ..,
    ) -> {
      let acc =
        [promise_data_ref, ..acc]
        |> push_value_ref(resolve, _)
        |> push_value_ref(reject, _)
      push_saved_locals_stack_refs(saved_locals, saved_stack, acc)
    }
    AsyncGeneratorSlot(
      queue: #(queue_front, queue_back),
      env_ref:,
      saved_locals:,
      saved_stack:,
      saved_eval_env:,
      ..,
    ) -> {
      // Both halves of the two-list FIFO hold live requests — walk both.
      let push_request = fn(a, r: AsyncGenRequest) {
        a
        |> push_value_ref(r.value, _)
        |> push_value_ref(r.resolve, _)
        |> push_value_ref(r.reject, _)
      }
      let acc = list.fold(queue_front, acc, push_request)
      let acc = list.fold(queue_back, acc, push_request)
      push_saved_frame_refs(
        env_ref,
        saved_locals,
        saved_stack,
        push_opt_ref(saved_eval_env, acc),
      )
    }
    RealmSlot(global_object:, lexical_globals:, symbol_registry: _) ->
      dict.fold(lexical_globals, [global_object, ..acc], fn(a, _k, v) {
        push_value_ref(lexical_global_value(v), a)
      })
  }
}

/// GC root tracing for a NativeFunction's payload.
///
/// This family of tracers (native_fn_refs → dispatch_native_fn_refs /
/// call_native_fn_refs → one function per *NativeFn enum) is deliberately
/// written with NO wildcard arms and NO `..` in constructor patterns:
/// exhaustiveness is the point. Adding a native-fn variant — or adding a Ref
/// or JsValue field to an existing one — fails to compile until the new edge
/// is traced (or the variant is explicitly listed as carrying no refs), so a
/// payload can never be silently dropped by the mark phase and collected out
/// from under a live built-in.
fn native_fn_refs(native: NativeFnSlot(ctx), acc: List(Ref)) -> List(Ref) {
  case native {
    Dispatch(d) -> dispatch_native_fn_refs(d, acc)
    Call(c) -> call_native_fn_refs(c, acc)
    // Host closures are opaque to the engine; the embedder owns any values
    // they capture and must keep them alive (same contract as HostObject,
    // which at least exposes a host_refs hook — closures expose nothing).
    Host(_) -> acc
  }
}

/// One arm per dispatch module, each delegating to that module's exhaustive
/// per-variant tracer.
fn dispatch_native_fn_refs(native: NativeFn, acc: List(Ref)) -> List(Ref) {
  case native {
    MathNative(f) -> math_native_refs(f, acc)
    BooleanNative(f) -> boolean_native_refs(f, acc)
    NumberNative(f) -> number_native_refs(f, acc)
    StringNative(f) -> string_native_refs(f, acc)
    ErrorNative(f) -> error_native_refs(f, acc)
    ArrayNative(f) -> array_native_refs(f, acc)
    ObjectNative(f) -> object_native_refs(f, acc)
    ConsoleNative(f) -> console_native_refs(f, acc)
    JsonNative(f) -> json_native_refs(f, acc)
    ReflectNative(f) -> reflect_native_refs(f, acc)
    MapNative(f) -> map_native_refs(f, acc)
    SetNative(f) -> set_native_refs(f, acc)
    WeakMapNative(f) -> weak_map_native_refs(f, acc)
    WeakSetNative(f) -> weak_set_native_refs(f, acc)
    FinalizationRegistryNative(f) -> finalization_registry_native_refs(f, acc)
    DisposableStackNative(f) -> disposable_stack_native_refs(f, acc)
    IteratorNative(f) -> iterator_native_refs(f, acc)
    RegExpNative(f) -> regexp_native_refs(f, acc)
    DateNative(f) -> date_native_refs(f, acc)
    IntlNative(f) -> intl_native_refs(f, acc)
    ArrayBufferNative(f) -> array_buffer_native_refs(f, acc)
    AtomicsNative(f) -> atomics_native_refs(f, acc)
    TypedArrayNative(f) -> typed_array_native_refs(f, acc)
    DataViewNative(f) -> data_view_native_refs(f, acc)
    TemporalNative(f) -> temporal_native_refs(f, acc)
    ShadowRealmNative(f) -> shadow_realm_native_refs(f, acc)
    VmNative(f) -> vm_native_refs(f, acc)
  }
}

fn math_native_refs(f: MathNativeFn, acc: List(Ref)) -> List(Ref) {
  case f {
    MathPow
    | MathAbs
    | MathFloor
    | MathCeil
    | MathRound
    | MathTrunc
    | MathSqrt
    | MathMax
    | MathMin
    | MathLog
    | MathSin
    | MathCos
    | MathTan
    | MathAsin
    | MathAcos
    | MathAtan
    | MathAtan2
    | MathExp
    | MathLog2
    | MathLog10
    | MathRandom
    | MathSign
    | MathCbrt
    | MathHypot
    | MathFround
    | MathClz32
    | MathImul
    | MathExpm1
    | MathLog1p
    | MathSinh
    | MathCosh
    | MathTanh
    | MathAsinh
    | MathAcosh
    | MathAtanh -> acc
  }
}

fn boolean_native_refs(f: BooleanNativeFn, acc: List(Ref)) -> List(Ref) {
  case f {
    BooleanConstructor | BooleanPrototypeValueOf | BooleanPrototypeToString ->
      acc
  }
}

fn number_native_refs(f: NumberNativeFn, acc: List(Ref)) -> List(Ref) {
  case f {
    NumberConstructor
    | NumberIsNaN
    | NumberIsFinite
    | NumberIsInteger
    | NumberPrototypeValueOf
    | NumberPrototypeToString
    | GlobalParseInt
    | GlobalParseFloat
    | GlobalIsNaN
    | GlobalIsFinite
    | NumberIsSafeInteger
    | NumberPrototypeToFixed
    | NumberPrototypeToPrecision
    | NumberPrototypeToExponential -> acc
  }
}

fn string_native_refs(f: StringNativeFn, acc: List(Ref)) -> List(Ref) {
  case f {
    StringPrototypeSymbolIterator
    | StringPrototypeCharAt
    | StringPrototypeCharCodeAt
    | StringPrototypeIndexOf
    | StringPrototypeLastIndexOf
    | StringPrototypeIncludes
    | StringPrototypeStartsWith
    | StringPrototypeEndsWith
    | StringPrototypeSlice
    | StringPrototypeSubstring
    | StringPrototypeToLowerCase
    | StringPrototypeToUpperCase
    | StringPrototypeToLocaleLowerCase
    | StringPrototypeToLocaleUpperCase
    | StringPrototypeTrim
    | StringPrototypeTrimStart
    | StringPrototypeTrimEnd
    | StringPrototypeSplit
    | StringPrototypeConcat
    | StringPrototypeToString
    | StringPrototypeValueOf
    | StringPrototypeRepeat
    | StringPrototypePadStart
    | StringPrototypePadEnd
    | StringPrototypeAt
    | StringPrototypeCodePointAt
    | StringPrototypeNormalize
    | StringPrototypeMatch
    | StringPrototypeSearch
    | StringPrototypeReplace
    | StringPrototypeReplaceAll
    | StringPrototypeSubstr
    | StringPrototypeLocaleCompare
    | StringPrototypeMatchAll
    | StringPrototypeIsWellFormed
    | StringPrototypeToWellFormed
    | StringPrototypeAnchor
    | StringPrototypeBig
    | StringPrototypeBlink
    | StringPrototypeBold
    | StringPrototypeFixed
    | StringPrototypeFontcolor
    | StringPrototypeFontsize
    | StringPrototypeItalics
    | StringPrototypeLink
    | StringPrototypeSmall
    | StringPrototypeStrike
    | StringPrototypeSub
    | StringPrototypeSup
    | StringRaw
    | StringFromCharCode
    | StringFromCodePoint -> acc
  }
}

fn error_native_refs(f: ErrorNativeFn, acc: List(Ref)) -> List(Ref) {
  case f {
    ErrorConstructor(proto:)
    | AggregateErrorConstructor(proto:)
    | SuppressedErrorConstructor(proto:)
    | ErrorStackSetter(proto:)
    | DomExceptionConstructor(proto:) -> [proto, ..acc]
    ErrorPrototypeToString
    | ErrorCaptureStackTrace
    | ErrorStackGetter
    | ErrorIsError
    | DomExceptionGetCode -> acc
  }
}

fn array_native_refs(f: ArrayNativeFn, acc: List(Ref)) -> List(Ref) {
  case f {
    ArrayConstructor
    | ArrayIsArray
    | ArrayPrototypeJoin
    | ArrayPrototypePush
    | ArrayPrototypePop
    | ArrayPrototypeShift
    | ArrayPrototypeUnshift
    | ArrayPrototypeSlice
    | ArrayPrototypeConcat
    | ArrayPrototypeReverse
    | ArrayPrototypeFill
    | ArrayPrototypeAt
    | ArrayPrototypeIndexOf
    | ArrayPrototypeLastIndexOf
    | ArrayPrototypeIncludes
    | ArrayPrototypeForEach
    | ArrayPrototypeMap
    | ArrayPrototypeFilter
    | ArrayPrototypeReduce
    | ArrayPrototypeReduceRight
    | ArrayPrototypeEvery
    | ArrayPrototypeSome
    | ArrayPrototypeFind
    | ArrayPrototypeFindIndex
    | ArrayPrototypeSort
    | ArrayPrototypeSplice
    | ArrayPrototypeFindLast
    | ArrayPrototypeFindLastIndex
    | ArrayPrototypeFlat
    | ArrayPrototypeFlatMap
    | ArrayPrototypeCopyWithin
    | ArrayPrototypeToSpliced
    | ArrayPrototypeWith
    | ArrayPrototypeToSorted
    | ArrayPrototypeToReversed
    | ArrayPrototypeToString
    | ArrayPrototypeToLocaleString
    | ArrayPrototypeKeys
    | ArrayPrototypeValues
    | ArrayPrototypeEntries
    | ArrayFrom
    | ArrayOf -> acc
  }
}

fn object_native_refs(f: ObjectNativeFn, acc: List(Ref)) -> List(Ref) {
  case f {
    ObjectConstructor
    | ObjectGetOwnPropertyDescriptor
    | ObjectDefineProperty
    | ObjectDefineProperties
    | ObjectGetOwnPropertyNames
    | ObjectKeys
    | ObjectValues
    | ObjectEntries
    | ObjectCreate
    | ObjectAssign
    | ObjectIs
    | ObjectHasOwn
    | ObjectGetPrototypeOf
    | ObjectSetPrototypeOf
    | ObjectFreeze
    | ObjectIsFrozen
    | ObjectIsExtensible
    | ObjectPreventExtensions
    | ObjectPrototypeHasOwnProperty
    | ObjectPrototypePropertyIsEnumerable
    | ObjectPrototypeToString
    | ObjectPrototypeValueOf
    | ObjectFromEntries
    | ObjectSeal
    | ObjectIsSealed
    | ObjectGetOwnPropertyDescriptors
    | ObjectGetOwnPropertySymbols
    | ObjectPrototypeIsPrototypeOf
    | ObjectPrototypeToLocaleString
    | ObjectGroupBy
    | ObjectPrototypeDefineGetter
    | ObjectPrototypeDefineSetter
    | ObjectPrototypeLookupGetter
    | ObjectPrototypeLookupSetter
    | ObjectPrototypeProtoGetter
    | ObjectPrototypeProtoSetter -> acc
  }
}

fn console_native_refs(f: ConsoleNativeFn, acc: List(Ref)) -> List(Ref) {
  case f {
    ConsoleLog | ConsoleLogError -> acc
  }
}

fn json_native_refs(f: JsonNativeFn, acc: List(Ref)) -> List(Ref) {
  case f {
    JsonParse | JsonStringify -> acc
  }
}

fn reflect_native_refs(f: ReflectNativeFn, acc: List(Ref)) -> List(Ref) {
  case f {
    ReflectApply
    | ReflectConstruct
    | ReflectDefineProperty
    | ReflectDeleteProperty
    | ReflectGet
    | ReflectGetOwnPropertyDescriptor
    | ReflectGetPrototypeOf
    | ReflectHas
    | ReflectIsExtensible
    | ReflectOwnKeys
    | ReflectPreventExtensions
    | ReflectSet
    | ReflectSetPrototypeOf -> acc
  }
}

fn map_native_refs(f: MapNativeFn, acc: List(Ref)) -> List(Ref) {
  case f {
    MapConstructor(proto:) -> [proto, ..acc]
    MapPrototypeGet
    | MapPrototypeSet
    | MapPrototypeHas
    | MapPrototypeDelete
    | MapPrototypeClear
    | MapPrototypeForEach
    | MapPrototypeGetSize
    | MapPrototypeKeys
    | MapPrototypeValues
    | MapPrototypeEntries -> acc
  }
}

fn set_native_refs(f: SetNativeFn, acc: List(Ref)) -> List(Ref) {
  case f {
    SetConstructor(proto:) -> [proto, ..acc]
    SetPrototypeAdd
    | SetPrototypeHas
    | SetPrototypeDelete
    | SetPrototypeClear
    | SetPrototypeForEach
    | SetPrototypeGetSize
    | SetPrototypeUnion
    | SetPrototypeIntersection
    | SetPrototypeDifference
    | SetPrototypeSymmetricDifference
    | SetPrototypeIsSubsetOf
    | SetPrototypeIsSupersetOf
    | SetPrototypeIsDisjointFrom
    | SetPrototypeValues
    | SetPrototypeEntries -> acc
  }
}

fn weak_map_native_refs(f: WeakMapNativeFn, acc: List(Ref)) -> List(Ref) {
  case f {
    WeakMapConstructor(proto:) -> [proto, ..acc]
    WeakMapPrototypeGet
    | WeakMapPrototypeSet
    | WeakMapPrototypeHas
    | WeakMapPrototypeDelete
    | WeakMapPrototypeGetOrInsert
    | WeakMapPrototypeGetOrInsertComputed -> acc
  }
}

fn weak_set_native_refs(f: WeakSetNativeFn, acc: List(Ref)) -> List(Ref) {
  case f {
    WeakSetConstructor(proto:) -> [proto, ..acc]
    WeakSetPrototypeAdd | WeakSetPrototypeHas | WeakSetPrototypeDelete -> acc
  }
}

fn finalization_registry_native_refs(
  f: FinalizationRegistryNativeFn,
  acc: List(Ref),
) -> List(Ref) {
  case f {
    FinalizationRegistryConstructor(proto:) -> [proto, ..acc]
    FinalizationRegistryPrototypeRegister
    | FinalizationRegistryPrototypeUnregister -> acc
  }
}

fn disposable_stack_native_refs(
  f: DisposableStackNativeFn,
  acc: List(Ref),
) -> List(Ref) {
  case f {
    DisposableStackConstructor(proto:)
    | DisposableStackPrototypeMove(proto:)
    | AsyncDisposableStackConstructor(proto:)
    | AsyncDisposableStackPrototypeMove(proto:) -> [proto, ..acc]
    // disposeAsync continuation handlers keep the remaining resources,
    // pending error, and the capability's resolve/reject alive.
    AsyncDisposeContinue(remaining:, pending:, resolve:, reject:, is_reject: _) ->
      push_dispose_resources(
        remaining,
        push_value_ref(
          resolve,
          push_value_ref(reject, push_option_value(pending, acc)),
        ),
      )
    // using-declaration disposers keep their method and resource alive.
    UsingDisposer(method:, value:, discard: _) ->
      push_value_ref(method, push_value_ref(value, acc))
    DisposableStackPrototypeDispose
    | DisposableStackPrototypeUse
    | DisposableStackPrototypeAdopt
    | DisposableStackPrototypeDefer
    | DisposableStackDisposedGetter
    | AsyncDisposableStackPrototypeDisposeAsync
    | AsyncDisposableStackPrototypeUse
    | AsyncDisposableStackPrototypeAdopt
    | AsyncDisposableStackPrototypeDefer
    | AsyncDisposableStackDisposedGetter -> acc
  }
}

fn iterator_native_refs(f: IteratorNativeFn, acc: List(Ref)) -> List(Ref) {
  case f {
    IteratorConstructor
    | IteratorFrom
    | IteratorZip
    | IteratorZipKeyed
    | IteratorConcat
    | IteratorPrototypeToArray
    | IteratorPrototypeForEach
    | IteratorPrototypeReduce
    | IteratorPrototypeSome
    | IteratorPrototypeEvery
    | IteratorPrototypeFind
    | IteratorPrototypeMap
    | IteratorPrototypeFilter
    | IteratorPrototypeTake
    | IteratorPrototypeDrop
    | IteratorPrototypeFlatMap
    | IteratorHelperNext
    | IteratorHelperReturn
    | WrapForValidIteratorNext
    | WrapForValidIteratorReturn
    | IteratorProtoGetToStringTag
    | IteratorProtoSetToStringTag
    | IteratorProtoGetConstructor
    | IteratorProtoSetConstructor -> acc
  }
}

fn regexp_native_refs(f: RegExpNativeFn, acc: List(Ref)) -> List(Ref) {
  case f {
    // Legacy static accessors keep their owning realm's %RegExp% alive.
    RegExpLegacyGetter(ctor:, slot: _) | RegExpLegacyInputSetter(ctor:) -> [
      ctor,
      ..acc
    ]
    // The constructor's legacy state is all Strings — no heap edges.
    RegExpConstructor(legacy: _)
    | RegExpPrototypeTest
    | RegExpPrototypeExec
    | RegExpPrototypeToString
    | RegExpPrototypeCompile
    | RegExpGetSource
    | RegExpGetFlags
    | RegExpGetGlobal
    | RegExpGetIgnoreCase
    | RegExpGetMultiline
    | RegExpGetDotAll
    | RegExpGetSticky
    | RegExpGetUnicode
    | RegExpGetUnicodeSets
    | RegExpGetHasIndices
    | RegExpSymbolMatch
    | RegExpSymbolMatchAll
    | RegExpSymbolReplace
    | RegExpSymbolSearch
    | RegExpSymbolSplit
    | RegExpStringIteratorNext -> acc
  }
}

fn date_native_refs(f: DateNativeFn, acc: List(Ref)) -> List(Ref) {
  case f {
    DateConstructor(proto:) -> [proto, ..acc]
    DateNow
    | DateParse
    | DateUTC
    | DatePrototypeValueOf
    | DatePrototypeGetTime
    | DatePrototypeGetTimezoneOffset
    | DatePrototypeGetFullYear
    | DatePrototypeGetUTCFullYear
    | DatePrototypeGetMonth
    | DatePrototypeGetUTCMonth
    | DatePrototypeGetDate
    | DatePrototypeGetUTCDate
    | DatePrototypeGetDay
    | DatePrototypeGetUTCDay
    | DatePrototypeGetHours
    | DatePrototypeGetUTCHours
    | DatePrototypeGetMinutes
    | DatePrototypeGetUTCMinutes
    | DatePrototypeGetSeconds
    | DatePrototypeGetUTCSeconds
    | DatePrototypeGetMilliseconds
    | DatePrototypeGetUTCMilliseconds
    | DatePrototypeSetTime
    | DatePrototypeSetMilliseconds
    | DatePrototypeSetUTCMilliseconds
    | DatePrototypeSetSeconds
    | DatePrototypeSetUTCSeconds
    | DatePrototypeSetMinutes
    | DatePrototypeSetUTCMinutes
    | DatePrototypeSetHours
    | DatePrototypeSetUTCHours
    | DatePrototypeSetDate
    | DatePrototypeSetUTCDate
    | DatePrototypeSetMonth
    | DatePrototypeSetUTCMonth
    | DatePrototypeSetFullYear
    | DatePrototypeSetUTCFullYear
    | DatePrototypeGetYear
    | DatePrototypeSetYear
    | DatePrototypeToString
    | DatePrototypeToDateString
    | DatePrototypeToTimeString
    | DatePrototypeToISOString
    | DatePrototypeToUTCString
    | DatePrototypeToLocaleString
    | DatePrototypeToLocaleDateString
    | DatePrototypeToLocaleTimeString
    | DatePrototypeToJSON
    | DatePrototypeSymbolToPrimitive -> acc
  }
}

fn intl_native_refs(f: IntlNativeFn, acc: List(Ref)) -> List(Ref) {
  case f {
    IntlConstructor(service: _, proto:) | IntlLocaleMethod(method: _, proto:) -> [
      proto,
      ..acc
    ]
    // Bound Intl methods keep their target instance alive.
    IntlBoundMethod(service: _, target:) -> [target, ..acc]
    // %SegmentsPrototype% and %SegmentIteratorPrototype% have no other
    // object-graph edge (ECMA-402 exposes no path to them besides these
    // payloads); today they survive GC only because alloc_proto /
    // init_namespace mark them as persistent roots, so scanning here keeps
    // the mark phase honest rather than relying on that rooting policy.
    IntlSegmenterSegment(segments_proto:) -> [segments_proto, ..acc]
    IntlSegmentsIterator(iter_proto:) -> [iter_proto, ..acc]
    IntlGetCanonicalLocales
    | IntlSupportedValuesOf
    | IntlSupportedLocalesOf(service: _)
    | IntlResolvedOptions(service: _)
    | IntlBoundGetter(service: _)
    | IntlMethod(service: _, method: _)
    | IntlHostOverride(which: _)
    | IntlLocaleGetter(name: _) -> acc
  }
}

fn array_buffer_native_refs(
  f: ArrayBufferNativeFn,
  acc: List(Ref),
) -> List(Ref) {
  case f {
    ArrayBufferConstructor(proto:) | SharedArrayBufferConstructor(proto:) -> [
      proto,
      ..acc
    ]
    ArrayBufferIsView
    | ArrayBufferGetSpecies
    | ArrayBufferGetByteLength
    | ArrayBufferGetDetached
    | ArrayBufferGetMaxByteLength
    | ArrayBufferGetResizable
    | ArrayBufferResize
    | ArrayBufferSlice
    | ArrayBufferTransfer
    | ArrayBufferTransferToFixedLength
    | ArrayBufferGetImmutable
    | ArrayBufferSliceToImmutable
    | ArrayBufferTransferToImmutable
    | SharedArrayBufferGetSpecies
    | SharedArrayBufferGetByteLength
    | SharedArrayBufferGrow
    | SharedArrayBufferGetGrowable
    | SharedArrayBufferGetMaxByteLength
    | SharedArrayBufferSlice
    | DetachArrayBuffer262 -> acc
  }
}

fn atomics_native_refs(f: AtomicsNativeFn, acc: List(Ref)) -> List(Ref) {
  case f {
    AtomicsAdd
    | AtomicsAnd
    | AtomicsCompareExchange
    | AtomicsExchange
    | AtomicsIsLockFree
    | AtomicsLoad
    | AtomicsOr
    | AtomicsStore
    | AtomicsSub
    | AtomicsWait
    | AtomicsWaitAsync
    | AtomicsNotify
    | AtomicsPause
    | AtomicsXor -> acc
  }
}

fn typed_array_native_refs(f: TypedArrayNativeFn, acc: List(Ref)) -> List(Ref) {
  case f {
    TypedArrayConstructor(kind: _, proto:) -> [proto, ..acc]
    TypedArrayIntrinsicConstructor
    | TypedArrayGetBuffer
    | TypedArrayGetByteLength
    | TypedArrayGetByteOffset
    | TypedArrayGetLength
    | TypedArrayGetToStringTag
    | TypedArrayGetSpecies
    | TypedArrayPrototypeFill
    | TypedArrayPrototypeSet
    | TypedArrayPrototypeSubarray
    | TypedArrayPrototypeSlice
    | TypedArrayPrototypeJoin
    | TypedArrayPrototypeIndexOf
    | TypedArrayPrototypeIncludes
    | TypedArrayPrototypeKeys
    | TypedArrayPrototypeValues
    | TypedArrayPrototypeEntries
    | TypedArrayPrototypeAt
    | TypedArrayPrototypeToString
    | TypedArrayPrototypeCopyWithin
    | TypedArrayPrototypeEvery
    | TypedArrayPrototypeSome
    | TypedArrayPrototypeForEach
    | TypedArrayPrototypeMap
    | TypedArrayPrototypeFilter
    | TypedArrayPrototypeFind
    | TypedArrayPrototypeFindIndex
    | TypedArrayPrototypeFindLast
    | TypedArrayPrototypeFindLastIndex
    | TypedArrayPrototypeLastIndexOf
    | TypedArrayPrototypeReduce
    | TypedArrayPrototypeReduceRight
    | TypedArrayPrototypeReverse
    | TypedArrayPrototypeToReversed
    | TypedArrayPrototypeSort
    | TypedArrayPrototypeToSorted
    | TypedArrayPrototypeToLocaleString
    | TypedArrayPrototypeWith
    | TypedArrayFrom
    | TypedArrayOf
    | Uint8ArrayPrototypeToBase64
    | Uint8ArrayPrototypeToHex
    | Uint8ArrayPrototypeSetFromBase64
    | Uint8ArrayPrototypeSetFromHex
    | Uint8ArrayFromBase64
    | Uint8ArrayFromHex -> acc
  }
}

fn data_view_native_refs(f: DataViewNativeFn, acc: List(Ref)) -> List(Ref) {
  case f {
    DataViewConstructor(proto:) -> [proto, ..acc]
    DataViewGetBuffer
    | DataViewGetByteLength
    | DataViewGetByteOffset
    | DataViewGet(element: _)
    | DataViewSet(element: _) -> acc
  }
}

fn temporal_native_refs(f: TemporalNativeFn, acc: List(Ref)) -> List(Ref) {
  case f {
    // Every Temporal native carries the eight sibling type prototypes.
    TemporalCtor(kind: _, protos:)
    | TemporalStatic(kind: _, name: _, protos:)
    | TemporalGetterFn(getter: _, protos:)
    | TemporalMethod(method: _, protos:)
    | TemporalNowFn(name: _, protos:) -> temporal_protos_refs(protos, acc)
  }
}

fn temporal_protos_refs(protos: TemporalProtos, acc: List(Ref)) -> List(Ref) {
  let TemporalProtos(
    plain_date:,
    plain_time:,
    plain_date_time:,
    plain_year_month:,
    plain_month_day:,
    duration:,
    instant:,
    zoned_date_time:,
  ) = protos
  [
    plain_date,
    plain_time,
    plain_date_time,
    plain_year_month,
    plain_month_day,
    duration,
    instant,
    zoned_date_time,
    ..acc
  ]
}

fn shadow_realm_native_refs(
  f: ShadowRealmNativeFn,
  acc: List(Ref),
) -> List(Ref) {
  case f {
    ShadowRealmConstructor(proto:) -> [proto, ..acc]
    // The prototype methods keep their own realm's %Function.prototype%
    // alive — it's the marker used to recover the caller realm at dispatch.
    ShadowRealmEvaluate(fn_proto:) | ShadowRealmImportValue(fn_proto:) -> [
      fn_proto,
      ..acc
    ]
    // Wrapped functions keep their target and both realm records alive.
    WrappedFunctionCall(target:, caller_realm:, target_realm:) ->
      push_value_ref(target, [caller_realm, target_realm, ..acc])
  }
}

fn vm_native_refs(f: VmNativeFn, acc: List(Ref)) -> List(Ref) {
  case f {
    FunctionConstructor
    | GeneratorFunctionConstructor
    | AsyncGeneratorFunctionConstructor
    | AsyncFunctionConstructor
    | FunctionToString
    | IteratorSymbolIterator
    | Eval
    | DecodeURI
    | EncodeURI
    | DecodeURIComponent
    | EncodeURIComponent
    | Escape
    | Unescape
    | EvalScript
    | CreateRealm
    | Gc
    | BigIntGlobal
    | BigIntPrototypeToString
    | BigIntPrototypeToLocaleString
    | BigIntPrototypeValueOf
    | ThrowTypeErrorFn
    | FunctionHasInstance
    | FunctionPrototypeCall -> acc
  }
}

/// Exhaustive tracer for call-level natives (bound functions, promise
/// reaction closures, async continuations, …). Same no-wildcard rule as
/// dispatch_native_fn_refs.
fn call_native_fn_refs(f: CallNativeFn, acc: List(Ref)) -> List(Ref) {
  case f {
    BoundFunction(target:, bound_this:, bound_args:) ->
      list.fold(
        bound_args,
        push_value_ref(bound_this, [target, ..acc]),
        fn(a, v) { push_value_ref(v, a) },
      )
    PromiseResolveFunction(promise_ref:, data_ref:, already_resolved_ref:)
    | PromiseRejectFunction(promise_ref:, data_ref:, already_resolved_ref:) -> [
      promise_ref,
      data_ref,
      already_resolved_ref,
      ..acc
    ]
    PromiseFinallyFulfill(on_finally:) | PromiseFinallyReject(on_finally:) ->
      push_value_ref(on_finally, acc)
    PromiseCapabilityExecutor(resolve_box:, reject_box:) -> [
      resolve_box,
      reject_box,
      ..acc
    ]
    PromiseAllResolveElement(
      index: _,
      remaining_ref:,
      values_ref:,
      already_called_ref:,
      resolve:,
      reject:,
    ) ->
      push_value_ref(
        reject,
        push_value_ref(resolve, [
          remaining_ref,
          values_ref,
          already_called_ref,
          ..acc
        ]),
      )
    PromiseAllSettledResolveElement(
      index: _,
      remaining_ref:,
      values_ref:,
      already_called_ref:,
      resolve:,
    )
    | PromiseAllSettledRejectElement(
        index: _,
        remaining_ref:,
        values_ref:,
        already_called_ref:,
        resolve:,
      ) ->
      push_value_ref(resolve, [
        remaining_ref,
        values_ref,
        already_called_ref,
        ..acc
      ])
    PromiseAnyRejectElement(
      index: _,
      remaining_ref:,
      errors_ref:,
      already_called_ref:,
      resolve:,
      reject:,
    ) ->
      push_value_ref(
        reject,
        push_value_ref(resolve, [
          remaining_ref,
          errors_ref,
          already_called_ref,
          ..acc
        ]),
      )
    PromiseKeyedElement(
      index: _,
      remaining_ref:,
      keys_ref:,
      values_ref:,
      already_called_ref:,
      resolve:,
      status_field: _,
    ) ->
      push_value_ref(resolve, [
        remaining_ref,
        keys_ref,
        values_ref,
        already_called_ref,
        ..acc
      ])
    PromiseFinallyValueThunk(value:) -> push_value_ref(value, acc)
    PromiseFinallyThrower(reason:) -> push_value_ref(reason, acc)
    AsyncResume(async_data_ref:, is_reject: _) -> [async_data_ref, ..acc]
    AsyncGeneratorResume(data_ref:, is_reject: _, kind: _) -> [data_ref, ..acc]
    AsyncFromSyncClose(sync_iter:) -> [sync_iter, ..acc]
    ArrayFromAsyncOnNext(ctx:) | ArrayFromAsyncOnMapped(ctx:) ->
      from_async_ctx_refs(ctx, acc)
    ArrayFromAsyncLikeOnValue(ctx:) | ArrayFromAsyncLikeOnMapped(ctx:) ->
      from_async_like_ctx_refs(ctx, acc)
    ArrayFromAsyncCloseReject(iter:, reject:) ->
      acc
      |> push_value_ref(iter, _)
      |> push_value_ref(reject, _)
    ArrayFromAsyncRejectWith(error:, reject:) ->
      acc
      |> push_value_ref(error, _)
      |> push_value_ref(reject, _)
    ProxyRevoke(proxy:) -> [proxy, ..acc]
    FunctionCall
    | FunctionApply
    | FunctionBind
    | StringConstructor
    | PromiseConstructor
    | PromiseThen
    | PromiseCatch
    | PromiseFinally
    | PromiseResolveStatic
    | PromiseRejectStatic
    | PromiseAllStatic
    | PromiseRaceStatic
    | PromiseAllSettledStatic
    | PromiseAnyStatic
    | PromiseAllKeyedStatic
    | PromiseAllSettledKeyedStatic
    | GeneratorNext
    | GeneratorReturn
    | GeneratorThrow
    | ArrayIteratorNext
    | SetIteratorNext
    | MapIteratorNext
    | AsyncGeneratorNext
    | AsyncGeneratorReturn
    | AsyncGeneratorThrow
    | AsyncFromSyncNext
    | AsyncFromSyncReturn
    | AsyncFromSyncThrow
    | AsyncFromSyncUnwrap(done: _)
    | ArrayFromAsync
    | SymbolConstructor
    | ProxyConstructor
    | ProxyRevocable
    | SymbolFor
    | SymbolKeyFor
    | SymbolPrototypeToString
    | SymbolPrototypeValueOf
    | SymbolDescriptionGetter
    | SymbolPrototypeToPrimitive -> acc
  }
}

/// Refs held by Array.fromAsync's async-iterator continuation context.
/// Destructured (rather than field-accessed) so a new JsValue field in
/// FromAsyncCtx is a compile error here until it is traced.
fn from_async_ctx_refs(ctx: FromAsyncCtx, acc: List(Ref)) -> List(Ref) {
  let FromAsyncCtx(
    iter:,
    next_method:,
    map_fn:,
    this_arg:,
    target:,
    k: _,
    resolve:,
    reject:,
  ) = ctx
  acc
  |> push_value_ref(iter, _)
  |> push_value_ref(next_method, _)
  |> push_value_ref(map_fn, _)
  |> push_value_ref(this_arg, _)
  |> push_value_ref(target, _)
  |> push_value_ref(resolve, _)
  |> push_value_ref(reject, _)
}

/// Refs held by Array.fromAsync's array-like continuation context.
fn from_async_like_ctx_refs(
  ctx: FromAsyncLikeCtx,
  acc: List(Ref),
) -> List(Ref) {
  let FromAsyncLikeCtx(
    items:,
    map_fn:,
    this_arg:,
    target:,
    k: _,
    len: _,
    resolve:,
    reject:,
  ) = ctx
  acc
  |> push_value_ref(items, _)
  |> push_value_ref(map_fn, _)
  |> push_value_ref(this_arg, _)
  |> push_value_ref(target, _)
  |> push_value_ref(resolve, _)
  |> push_value_ref(reject, _)
}

/// Format a JS number as a string per ES2024 §6.1.6.1.20 Number::toString.
/// Delegates to Erlang FFI for proper JS-compatible output (e.g., 1e21 → "1e+21",
/// 1e-6 → "0.000001", -0 → "0").
@external(erlang, "arc_number_ffi", "js_number_to_string")
pub fn js_format_number(n: Float) -> String

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
    JsObject(_) -> True
    JsSymbol(_) -> True
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

/// `Some(i)` iff `f` is an integral Number (§4.4.31 IsIntegralNumber) whose
/// value is the Int `i`; `None` for a fractional `f`. THE way to ask "is this
/// Number integral, and what Int is it" — `float_to_int` alone silently
/// truncates.
///
/// ±0-safe: `+. 0.0` normalizes -0.0 to +0.0 on both sides before comparing.
/// The naive `int.to_float(i) == f` compiles to BEAM `=:=`, for which
/// `0.0 =:= -0.0` is False, so it reads -0 as non-integral.
pub fn integral_int(f: Float) -> Option(Int) {
  let i = float_to_int(f)
  case int.to_float(i) +. 0.0 == f +. 0.0 {
    True -> Some(i)
    False -> None
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

/// ES2024 §7.2.11 SameValue. Like ===, except NaN equals NaN and +0 does NOT
/// equal -0. Used by Proxy invariant checks.
pub fn same_value(left: JsValue, right: JsValue) -> Bool {
  case left, right {
    JsNumber(NaN), JsNumber(NaN) -> True
    // Erlang term equality (=:=) distinguishes -0.0 from +0.0 (OTP 27+) and
    // compares floats exactly — precisely SameValue's number semantics.
    JsNumber(Finite(a)), JsNumber(Finite(b)) -> float_same_term(a, b)
    _, _ -> strict_equal(left, right)
  }
}

/// Erlang =:= on floats: exact term equality, distinguishes -0.0 from +0.0.
@external(erlang, "arc_vm_ffi", "float_same_term")
fn float_same_term(a: Float, b: Float) -> Bool

/// §7.1.14 StringToBigInt — decimal (with sign) or 0x/0o/0b prefixed;
/// empty/whitespace-only → 0; anything else fails (None).
///
/// Lives here, next to `BigInt`, because both `ops/operators` (loose equality,
/// relational comparison) and `ops/coerce` (ToBigInt) need it and `operators`
/// cannot import `coerce`. One copy: a fix lands everywhere.
pub fn string_to_bigint(s: String) -> Option(Int) {
  let s = string.trim(s)
  case s {
    "" -> Some(0)
    "0x" <> rest | "0X" <> rest -> parse_bigint_radix_digits(rest, 16)
    "0o" <> rest | "0O" <> rest -> parse_bigint_radix_digits(rest, 8)
    "0b" <> rest | "0B" <> rest -> parse_bigint_radix_digits(rest, 2)
    _ -> int.parse(s) |> option.from_result
  }
}

/// Digits after a 0x/0o/0b prefix. The grammar (§7.1.14
/// NonDecimalIntegerLiteral) has no SignedInteger, so a sign is a failure
/// even though int.base_parse would accept it.
fn parse_bigint_radix_digits(digits: String, base: Int) -> Option(Int) {
  case digits {
    "-" <> _ | "+" <> _ -> None
    _ -> int.base_parse(digits, base) |> option.from_result
  }
}

/// The two values §7.1.4 ToNumber refuses to convert. Both are spec-mandated
/// TypeErrors, but the *caller* names the class and the message: a future
/// conversion failure that the spec classes as something else (a RangeError,
/// say) becomes a new variant here, and every caller stops compiling until it
/// says what to throw. That is the whole point of not returning a String.
///
/// The engine-invariant failures ToNumber used to report as strings — an
/// object that skipped ToPrimitive, the TDZ sentinel — are `panic`s inside
/// `to_number`, not values a caller has to render.
pub type ToNumberError {
  BigIntNotConvertible
  SymbolNotConvertible
}

/// ES2024 §7.1.4 ToNumber, primitives ONLY.
///
/// Callers must run ToPrimitive (coerce.to_primitive / coerce.js_to_number /
/// coerce.try_to_number) on objects first — an object here is a bug, never
/// "NaN": the spec requires ToPrimitive (which runs user valueOf/toString and
/// can throw) before the conversion table is consulted.
pub fn to_number(val: JsValue) -> Result(JsNum, ToNumberError) {
  case val {
    JsNumber(n) -> Ok(n)
    JsUndefined -> Ok(NaN)
    JsNull -> Ok(Finite(0.0))
    JsBool(True) -> Ok(Finite(1.0))
    JsBool(False) -> Ok(Finite(0.0))
    JsString(s) -> Ok(string_to_number(s))
    JsBigInt(_) -> Error(BigIntNotConvertible)
    JsSymbol(_) -> Error(SymbolNotConvertible)
    JsObject(_) ->
      panic as "ToNumber on an object: caller must run ToPrimitive first"
    // elements.get/get_option promise the hole sentinel never leaks, and every
    // TDZ load throws ReferenceError before the value reaches an operand.
    JsUninitialized -> panic as "ToNumber on the TDZ sentinel"
  }
}

/// ES2024 §7.1.4.1.1 StringToNumber: trims whitespace, accepts leading + or -,
/// Infinity, scientific notation, leading/trailing decimal point.
/// Hex/oct/bin prefixes not handled here.
pub fn string_to_number(s: String) -> JsNum {
  // Fast path: a plain run of ASCII digits (optional single leading '-').
  // Such strings have no whitespace to trim and can't be float/hex literals,
  // so the general path's failed float.parse attempts (caught badargs) and
  // binary pattern compiles are pure overhead. Very hot via canonical numeric
  // index conversion of array-like string keys ("0", "1", ...).
  case parse_plain_digits(s) {
    Ok(n) -> n
    Error(Nil) -> string_to_number_slow(s)
  }
}

/// Parse a string that is exactly an optional '-' followed by 1..15 ASCII
/// digits. 15 digits keeps the value exactly representable as a float, so the
/// result is identical to the general path. Anything else falls through.
fn parse_plain_digits(s: String) -> Result(JsNum, Nil) {
  case bit_array.from_string(s) {
    // '-' prefix: negate via float.negate so "-0" yields -0.0 like the
    // general path does.
    <<0x2d, rest:bytes>> -> {
      use n <- result.map(accumulate_digits(rest, 0, 0))
      Finite(float.negate(int.to_float(n)))
    }
    bytes -> {
      use n <- result.map(accumulate_digits(bytes, 0, 0))
      Finite(int.to_float(n))
    }
  }
}

fn accumulate_digits(
  bytes: BitArray,
  acc: Int,
  count: Int,
) -> Result(Int, Nil) {
  case bytes {
    <<>> if count >= 1 && count <= 15 -> Ok(acc)
    <<d, rest:bytes>> if d >= 0x30 && d <= 0x39 ->
      accumulate_digits(rest, acc * 10 + d - 0x30, count + 1)
    _ -> Error(Nil)
  }
}

/// Single-pass byte-walk over the string: hand-rolled StrWhiteSpace trim
/// (no pattern compiles), grammar validated while scanning so float parsing
/// runs at most once on a known-well-formed literal (no caught badargs).
fn string_to_number_slow(s: String) -> JsNum {
  let bytes = trim_string_ws(bit_array.from_string(s))
  case bytes {
    <<>> -> Finite(0.0)
    // NonDecimalIntegerLiteral (§7.1.4.1 StringNumericLiteral): hex/octal/
    // binary prefixes. No sign is permitted with these forms.
    <<"0x":utf8, digits:bytes>> | <<"0X":utf8, digits:bytes>> ->
      parse_radix_literal(digits, 16)
    <<"0o":utf8, digits:bytes>> | <<"0O":utf8, digits:bytes>> ->
      parse_radix_literal(digits, 8)
    <<"0b":utf8, digits:bytes>> | <<"0B":utf8, digits:bytes>> ->
      parse_radix_literal(digits, 2)
    <<"-":utf8, rest:bytes>> ->
      case parse_unsigned_literal(rest) {
        Ok(n) -> negate_js_num(n)
        Error(Nil) -> NaN
      }
    <<"+":utf8, rest:bytes>> ->
      case parse_unsigned_literal(rest) {
        Ok(n) -> n
        Error(Nil) -> NaN
      }
    _ ->
      case parse_unsigned_literal(bytes) {
        Ok(n) -> n
        Error(Nil) -> NaN
      }
  }
}

fn negate_js_num(n: JsNum) -> JsNum {
  case n {
    Finite(f) -> Finite(float.negate(f))
    Infinity -> NegInfinity
    NegInfinity -> Infinity
    NaN -> NaN
  }
}

/// Trim StrWhiteSpaceChar (§7.1.4.1: WhiteSpace ∪ LineTerminator) from both
/// ends. Note this is NOT Unicode White_Space: U+0085 NEL is excluded and
/// U+FEFF ZWNBSP included, matching the JS spec.
fn trim_string_ws(bytes: BitArray) -> BitArray {
  let bytes = drop_leading_string_ws(bytes)
  let keep = content_length(bytes, 0, 0)
  case keep == bit_array.byte_size(bytes) {
    True -> bytes
    False ->
      case bit_array.slice(bytes, 0, keep) {
        Ok(trimmed) -> trimmed
        // Unreachable: keep is always <= byte_size.
        Error(Nil) -> bytes
      }
  }
}

fn drop_leading_string_ws(bytes: BitArray) -> BitArray {
  case bytes {
    // TAB LF VT FF CR SP
    <<b, rest:bytes>>
      if b == 0x09
      || b == 0x0a
      || b == 0x0b
      || b == 0x0c
      || b == 0x0d
      || b == 0x20
    -> drop_leading_string_ws(rest)
    // U+00A0 NBSP
    <<0xc2, 0xa0, rest:bytes>> -> drop_leading_string_ws(rest)
    // U+1680 OGHAM SPACE MARK
    <<0xe1, 0x9a, 0x80, rest:bytes>> -> drop_leading_string_ws(rest)
    // U+2000..U+200A spaces, U+2028 LS, U+2029 PS, U+202F NNBSP
    <<0xe2, 0x80, b, rest:bytes>>
      if b >= 0x80 && b <= 0x8a || b == 0xa8 || b == 0xa9 || b == 0xaf
    -> drop_leading_string_ws(rest)
    // U+205F MMSP
    <<0xe2, 0x81, 0x9f, rest:bytes>> -> drop_leading_string_ws(rest)
    // U+3000 IDEOGRAPHIC SPACE
    <<0xe3, 0x80, 0x80, rest:bytes>> -> drop_leading_string_ws(rest)
    // U+FEFF ZWNBSP
    <<0xef, 0xbb, 0xbf, rest:bytes>> -> drop_leading_string_ws(rest)
    _ -> bytes
  }
}

/// Byte length of `bytes` up to and including the last byte that is not part
/// of a StrWhiteSpaceChar — i.e. the length after trimming trailing JS
/// whitespace.
fn content_length(bytes: BitArray, idx: Int, last: Int) -> Int {
  case bytes {
    <<>> -> last
    <<b, rest:bytes>>
      if b == 0x09
      || b == 0x0a
      || b == 0x0b
      || b == 0x0c
      || b == 0x0d
      || b == 0x20
    -> content_length(rest, idx + 1, last)
    <<0xc2, 0xa0, rest:bytes>> -> content_length(rest, idx + 2, last)
    <<0xe1, 0x9a, 0x80, rest:bytes>> -> content_length(rest, idx + 3, last)
    <<0xe2, 0x80, b, rest:bytes>>
      if b >= 0x80 && b <= 0x8a || b == 0xa8 || b == 0xa9 || b == 0xaf
    -> content_length(rest, idx + 3, last)
    <<0xe2, 0x81, 0x9f, rest:bytes>> -> content_length(rest, idx + 3, last)
    <<0xe3, 0x80, 0x80, rest:bytes>> -> content_length(rest, idx + 3, last)
    <<0xef, 0xbb, 0xbf, rest:bytes>> -> content_length(rest, idx + 3, last)
    <<_, rest:bytes>> -> content_length(rest, idx + 1, idx + 1)
    // Unreachable: input is a UTF-8 string, always byte-aligned.
    _ -> last
  }
}

/// Parse a StrUnsignedDecimalLiteral (any sign already stripped by the
/// caller): "Infinity", digits[.digits][exp], .digits[exp] or digits.[exp].
fn parse_unsigned_literal(bytes: BitArray) -> Result(JsNum, Nil) {
  case bytes {
    <<"Infinity":utf8>> -> Ok(Infinity)
    _ -> {
      let #(icount, after_int) = scan_ascii_digits(bytes, 0)
      case after_int {
        // Entirely digits: an integer literal.
        <<>> if icount > 0 -> parse_integer_literal(bytes)
        <<".":utf8, after_dot:bytes>> -> {
          let #(fcount, after_frac) = scan_ascii_digits(after_dot, 0)
          case icount > 0 || fcount > 0 {
            False -> Error(Nil)
            True -> {
              use exp <- result.try(scan_exponent_part(after_frac))
              build_and_parse_float(bytes, icount, fcount, exp)
            }
          }
        }
        // No dot, trailing bytes after the digits: must be an ExponentPart.
        _ if icount > 0 -> {
          use exp <- result.try(scan_exponent_part(after_int))
          build_and_parse_float(bytes, icount, 0, exp)
        }
        _ -> Error(Nil)
      }
    }
  }
}

fn scan_ascii_digits(bytes: BitArray, count: Int) -> #(Int, BitArray) {
  case bytes {
    <<d, rest:bytes>> if d >= 0x30 && d <= 0x39 ->
      scan_ascii_digits(rest, count + 1)
    _ -> #(count, bytes)
  }
}

/// Validate an optional ExponentPart and return it normalised: "" when
/// absent, otherwise "e" followed by the (possibly signed) digits.
/// binary_to_float accepts "e+5"/"E5" exponent forms, so the digits are kept
/// verbatim.
fn scan_exponent_part(bytes: BitArray) -> Result(String, Nil) {
  case bytes {
    <<>> -> Ok("")
    <<e, digits:bytes>> if e == 0x65 || e == 0x45 -> {
      let valid = case digits {
        <<"+":utf8, ds:bytes>> | <<"-":utf8, ds:bytes>> ->
          nonempty_all_digits(ds)
        _ -> nonempty_all_digits(digits)
      }
      case valid {
        False -> Error(Nil)
        True ->
          bit_array.to_string(digits)
          |> result.map(fn(d) { "e" <> d })
      }
    }
    _ -> Error(Nil)
  }
}

fn nonempty_all_digits(bytes: BitArray) -> Bool {
  case bytes {
    <<d>> if d >= 0x30 && d <= 0x39 -> True
    <<d, rest:bytes>> if d >= 0x30 && d <= 0x39 -> nonempty_all_digits(rest)
    _ -> False
  }
}

/// Build an Erlang-acceptable float literal ("both sides of the dot") from
/// the validated mantissa bytes and normalised exponent, then parse it.
/// icount/fcount are the integer/fraction digit counts; the dot (when both
/// are present) sits between them at offset icount.
fn build_and_parse_float(
  bytes: BitArray,
  icount: Int,
  fcount: Int,
  exp: String,
) -> Result(JsNum, Nil) {
  use mant <- result.try(case icount > 0, fcount > 0 {
    True, True ->
      bit_array.slice(bytes, 0, icount + 1 + fcount)
      |> result.try(bit_array.to_string)
    // "5." / "5e3" — fraction digits absent: supply ".0".
    True, False ->
      bit_array.slice(bytes, 0, icount)
      |> result.try(bit_array.to_string)
      |> result.map(fn(i) { i <> ".0" })
    // ".5" — integer digits absent: prepend "0".
    False, True ->
      bit_array.slice(bytes, 0, 1 + fcount)
      |> result.try(bit_array.to_string)
      |> result.map(fn(f) { "0" <> f })
    False, False -> Error(Nil)
  })
  case float.parse(mant <> exp) {
    Ok(f) -> Ok(Finite(f))
    // Out-of-double-range literal (e.g. "1e999"): binary_to_float rejects
    // it. Preserves the previous behaviour (NaN).
    Error(Nil) -> Ok(NaN)
  }
}

/// Parse an all-digits integer literal. Goes through float syntax first;
/// only literals beyond double range (~1e308, 309+ digits) fall back to
/// arbitrary-precision integer parsing, which `num_from_int` saturates to
/// Infinity per §7.1.4.1 instead of crashing in erlang:float/1.
fn parse_integer_literal(bytes: BitArray) -> Result(JsNum, Nil) {
  use digits <- result.try(bit_array.to_string(bytes))
  case float.parse(digits <> ".0") {
    Ok(f) -> Ok(Finite(f))
    Error(Nil) -> int.parse(digits) |> result.map(num_from_int)
  }
}

/// Parse the digits of a NonDecimalIntegerLiteral ("0x.." / "0o.." / "0b..").
/// Empty or signed digit sequences are NaN per §7.1.4.1.
fn parse_radix_literal(digits: BitArray, radix: Int) -> JsNum {
  case digits {
    <<>> | <<"-":utf8, _:bytes>> | <<"+":utf8, _:bytes>> -> NaN
    _ -> {
      let parsed =
        bit_array.to_string(digits)
        |> result.try(int.base_parse(_, radix))
      case parsed {
        Ok(n) -> num_from_int(n)
        Error(Nil) -> NaN
      }
    }
  }
}

/// JS SameValueZero: https://tc39.es/ecma262/#sec-samevaluezero
/// Like ===, but NaN equals NaN. ±0 are still equal.
/// Used by Array.prototype.includes, Map/Set key equality.
pub fn same_value_zero(left: JsValue, right: JsValue) -> Bool {
  case left, right {
    // NaN SameValueZero NaN → true (this is the only difference from ===)
    JsNumber(NaN), JsNumber(NaN) -> True
    _, _ -> strict_equal(left, right)
  }
}
