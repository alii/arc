//// one-letter constructor families: K* JsValKind (what classify gives),
//// J* JsNum (the number cases inside KNum), S* Cell (what a handle holds);
//// mk_* builds the JsVal each K* case reads back, mk_object(h) <-> KHandle(h)

import arc/bytecode/error_kind.{type ErrorKind}
import arc/bytecode/key.{type PropertyKey}
import arc/host_hooks.{type ConsoleLevel, type HostHooks}
import arc/internal/ordered_entries.{type OrderedEntries}
import arc/internal/tree_array.{type TreeArray}
import arc/rt/arena.{type Arena}
import arc/rt/bytecode.{type EnvTuple, type FuncTemplate, type SuspendedFrame}
import arc/rt/intl_data.{
  type BoundGetterService, type ConstructibleService, type IntlData,
  type IntlService,
}
import arc/rt/limits
import arc/rt/temporal_data.{type TemporalData}
import arc/rt/wire
import arc/time_zone
import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/option.{type Option, Some}
import gleam/set.{type Set}

pub type JsVal =
  wire.JsVal

pub type JsNum {
  JInt(Int)
  JFloat(Float)
  JNan
  JPosInf
  JNegInf
}

pub type Handle {
  Handle(id: Int)
}

pub type JsValKind {
  KUndef
  KNull
  KBool(Bool)
  KNum(JsNum)
  KStr(String)
  KBig(Int)
  KSym(SymbolId)
  KHandle(Handle)
  KTdz
}

@external(erlang, "arc_rt_val_ffi", "classify")
pub fn classify(v: JsVal) -> JsValKind

@external(erlang, "arc_rt_val_ffi", "mk_undefined")
pub fn mk_undefined() -> JsVal

// dense-array hole marker, not a js value
@external(erlang, "arc_rt_val_ffi", "mk_hole")
pub fn mk_hole() -> JsVal

// nested array literal marker, not a js value
@external(erlang, "arc_rt_val_ffi", "mk_array_lit")
pub fn mk_array_lit(elems: List(JsVal)) -> JsVal

@external(erlang, "arc_rt_val_ffi", "mk_null")
pub fn mk_null() -> JsVal

@external(erlang, "arc_rt_val_ffi", "mk_bool")
pub fn mk_bool(b: Bool) -> JsVal

@external(erlang, "arc_rt_val_ffi", "mk_number")
pub fn mk_number(n: JsNum) -> JsVal

@external(erlang, "arc_rt_val_ffi", "mk_int")
pub fn mk_int(n: Int) -> JsVal

@external(erlang, "arc_rt_val_ffi", "mk_string")
pub fn mk_string(s: String) -> JsVal

@external(erlang, "arc_rt_val_ffi", "mk_bigint")
pub fn mk_bigint(n: Int) -> JsVal

@external(erlang, "arc_rt_val_ffi", "mk_symbol")
pub fn mk_symbol(id: SymbolId) -> JsVal

@external(erlang, "arc_rt_val_ffi", "mk_object")
pub fn mk_object(h: Handle) -> JsVal

@external(erlang, "arc_rt_val_ffi", "mk_tdz")
pub fn mk_tdz() -> JsVal

pub type ToPrimHint {
  HintDefault
  HintString
  HintNumber
}

pub type ObjectKey {
  StringKey(PropertyKey)
  SymbolKey(SymbolId)
}

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

pub type SymbolId {
  WellKnownSymbol(which: WellKnown)
  UserSymbol(uid: Int, description: Option(String))
  RegisteredSymbol(key: String)
}

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

fn well_known_description(which: WellKnown) -> String {
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

pub fn symbol_description(id: SymbolId) -> Option(String) {
  case id {
    WellKnownSymbol(which) -> Some(well_known_description(which))
    UserSymbol(description:, ..) -> description
    RegisteredSymbol(key:) -> Some(key)
  }
}

pub fn is_registered_symbol(id: SymbolId) -> Bool {
  case id {
    RegisteredSymbol(..) -> True
    WellKnownSymbol(_) | UserSymbol(..) -> False
  }
}

pub type WeakKey {
  WeakObjKey(id: Int)
  WeakSymKey(id: SymbolId)
}

pub fn symbol_descriptive_string(id: SymbolId) -> String {
  "Symbol(" <> option.unwrap(symbol_description(id), "") <> ")"
}

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

pub type BigIntKind {
  BigInt64Kind
  BigUint64Kind
}

pub type TypedArrayKind {
  NumKind(NumberKind)
  BigKind(BigIntKind)
}

pub type ViewNumElement {
  ViewInt8
  ViewUint8
  ViewInt16
  ViewUint16
  ViewInt32
  ViewUint32
  ViewFloat16
  ViewFloat32
  ViewFloat64
}

pub type ViewBigElement {
  ViewBigInt64
  ViewBigUint64
}

pub type ViewElementType {
  ViewNum(ViewNumElement)
  ViewBig(ViewBigElement)
}

pub type BufferStorage {
  Detached(max_byte_length: Option(Int))
  Bytes(bytes: BitArray, max_byte_length: Option(Int))
  Immutable(bytes: BitArray)
  Shared(block: SharedBlock, max_byte_length: Option(Int))
}

pub type SharedBlock {
  LocalBlock(bytes: BitArray)
  // byte_length is only a lower bound when growable
  OwnerBlock(owner: SabOwner, byte_length: Int)
}

pub type SabOwner

pub type WaiterRef

pub type CompiledCode

pub type DirectEntry {
  DirectEntry(code: CompiledCode, arity: Int, takes_this: Bool)
}

pub type CompiledRegExp

pub type Property {
  DataProperty(
    value: JsVal,
    writable: Bool,
    enumerable: Bool,
    configurable: Bool,
    seq: Int,
  )
  AccessorProperty(
    get: Option(JsVal),
    set: Option(JsVal),
    enumerable: Bool,
    configurable: Bool,
    seq: Int,
  )
}

// writable, enumerable, configurable all false
pub fn frozen_property(value: JsVal, seq: Int) -> Property {
  DataProperty(
    value:,
    writable: False,
    enumerable: False,
    configurable: False,
    seq:,
  )
}

// writable, enumerable, configurable all true
pub fn plain_property(value: JsVal, seq: Int) -> Property {
  DataProperty(
    value:,
    writable: True,
    enumerable: True,
    configurable: True,
    seq:,
  )
}

// §18 builtin data property: writable, not enumerable, configurable
pub fn builtin_property(value: JsVal, seq: Int) -> Property {
  DataProperty(
    value:,
    writable: True,
    enumerable: False,
    configurable: True,
    seq:,
  )
}

pub fn prop_seq(prop: Property) -> Int {
  case prop {
    DataProperty(seq:, ..) | AccessorProperty(seq:, ..) -> seq
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

pub type ParsedDesc {
  ParsedDesc(
    value: Option(JsVal),
    get: Option(JsVal),
    set: Option(JsVal),
    writable: Option(Bool),
    enumerable: Option(Bool),
    configurable: Option(Bool),
  )
}

pub type JsElements {
  NoElements
  Dense(TreeArray(JsVal))
  Sparse(Dict(Int, JsVal))
}

pub type FnBirth {
  BirthPending(prototype_parent: Option(Handle))
  BirthSettled
}

pub type FnFlags {
  FnFlags(
    is_constructor: Bool,
    is_class_constructor: Bool,
    is_derived_constructor: Bool,
    is_arrow: Bool,
    is_generator: Bool,
    is_async: Bool,
    is_strict: Bool,
  )
}

pub type MapKey {
  MapKeyString(String)
  MapKeyNumber(Float)
  MapKeyNan
  MapKeyInfinity
  MapKeyNegInfinity
  MapKeyBool(Bool)
  MapKeyNull
  MapKeyUndefined
  MapKeyObject(Handle)
  MapKeySymbol(SymbolId)
  MapKeyBigInt(Int)
}

pub fn js_to_map_key(v: JsVal) -> MapKey {
  case classify(v) {
    KStr(s) -> MapKeyString(s)
    KNum(JNan) -> MapKeyNan
    KNum(JPosInf) -> MapKeyInfinity
    KNum(JNegInf) -> MapKeyNegInfinity
    // +. 0.0 turns -0.0 into +0.0
    KNum(JFloat(f)) -> MapKeyNumber(f +. 0.0)
    KNum(JInt(n)) -> MapKeyNumber(int.to_float(n) +. 0.0)
    KBool(b) -> MapKeyBool(b)
    KNull -> MapKeyNull
    KUndef -> MapKeyUndefined
    KHandle(h) -> MapKeyObject(h)
    KSym(id) -> MapKeySymbol(id)
    KBig(n) -> MapKeyBigInt(n)
    KTdz -> panic as "js_to_map_key on the TDZ sentinel"
  }
}

// called by name from arc_rt_lang_ffi
pub fn map_key_to_js(key: MapKey) -> JsVal {
  case key {
    MapKeyString(s) -> mk_string(s)
    MapKeyNumber(f) -> mk_number(integral_key_number(f))
    MapKeyNan -> mk_number(JNan)
    MapKeyInfinity -> mk_number(JPosInf)
    MapKeyNegInfinity -> mk_number(JNegInf)
    MapKeyBool(b) -> mk_bool(b)
    MapKeyNull -> mk_null()
    MapKeyUndefined -> mk_undefined()
    MapKeyObject(h) -> mk_object(h)
    MapKeySymbol(id) -> mk_symbol(id)
    MapKeyBigInt(n) -> mk_bigint(n)
  }
}

fn integral_key_number(f: Float) -> JsNum {
  let n = float.truncate(f)
  let exact =
    int.to_float(n) == f
    && n <= limits.max_safe_integer
    && n >= -limits.max_safe_integer
  case exact {
    True -> JInt(n)
    False -> JFloat(f)
  }
}

pub type ArrayIterKind {
  ArrayIterKeys
  ArrayIterValues
  ArrayIterEntries
}

pub type MapIterKind {
  MapIterKeys
  MapIterValues
  MapIterEntries
}

pub type SetIterKind {
  SetIterValues
  SetIterEntries
}

pub type MethodInstallKind {
  InstallMethod
  InstallGetter
  InstallSetter
}

// new tokens: prototype methods are <Type><Method>, statics end in Static
pub type NativeToken {
  PromiseResolveFn(promise: Handle, already_resolved: Handle)
  PromiseRejectFn(promise: Handle, already_resolved: Handle)
  AsyncGenResume(gen: Handle, is_throw: Bool, kind: AsyncGenResumeKind)
  ObjectN(ObjectNative)
  FunctionN(FunctionNative)
  ErrorN(ErrorNative)
  DateN(DateNative)
  RegExpN(RegExpNative)
  ArrayBufferN(ArrayBufferNative)
  TypedArrayN(TypedArrayNative)
  DataViewN(DataViewNative)
  AtomicsN(AtomicsNative)
  ProxyN(ProxyNative)
  ReturnThis
  PromiseN(PromiseNative)
  IteratorN(IteratorNative)
  GeneratorN(GeneratorNative)
  MapN(MapNative)
  SetN(SetNative)
  WeakN(WeakNative)
  ArrayN(ArrayNative)
  StringN(StringNative)
  NumberN(NumberNative)
  BooleanN(BooleanNative)
  SymbolN(SymbolNative)
  BigIntN(BigIntNative)
  MathN(MathNative)
  JsonN(JsonNative)
  ReflectN(ReflectNative)
  ConsoleN(ConsoleNative)
  GlobalN(GlobalNative)
  HostFn(id: Int)
  Test262N(Test262Native)
  DomExceptionN(DomExceptionNative)
  IntlN(IntlNative)
  TemporalN(TemporalNative)
  DisposableStackN(DisposableStackNative)
  FinalizationRegistryN(FinalizationRegistryNative)
  WeakRefN(WeakRefNative)
  ShadowRealmN(ShadowRealmNative)
}

pub type WeakRefNative {
  WeakRefConstructor
  WeakRefPrototypeDeref
}

pub type FinalizationRegistryNative {
  FinalizationRegistryConstructor
  FinalizationRegistryPrototypeRegister
  FinalizationRegistryPrototypeUnregister
}

// target and unregister_token are weak (untraced), held is strong
pub type Registration {
  Registration(target: JsVal, held: JsVal, unregister_token: Option(JsVal))
}

pub type DisposableStackNative {
  DisposableStackConstructor(proto: Handle)
  DisposableStackPrototypeDispose
  DisposableStackPrototypeUse
  DisposableStackPrototypeAdopt
  DisposableStackPrototypeDefer
  DisposableStackPrototypeMove(proto: Handle)
  DisposableStackDisposedGetter
  AsyncDisposableStackConstructor(proto: Handle)
  AsyncDisposableStackPrototypeDisposeAsync
  AsyncDisposableStackPrototypeUse
  AsyncDisposableStackPrototypeAdopt
  AsyncDisposableStackPrototypeDefer
  AsyncDisposableStackPrototypeMove(proto: Handle)
  AsyncDisposableStackDisposedGetter
  AsyncDisposeContinue(
    remaining: List(DisposeResource),
    pending: Option(JsVal),
    resolve: JsVal,
    reject: JsVal,
    is_reject: Bool,
  )
}

pub type DisposeResource {
  MethodDispose(value: JsVal, method: JsVal)
  DisposeCallback(callback: JsVal, args: List(JsVal))
  AsyncFallbackDispose(value: JsVal, method: JsVal)
  NullDispose
}

pub type DisposableState {
  Pending(capability: Handle)
  Disposed
}

pub type DomExceptionNative {
  DomExceptionConstructor(proto: Handle)
  DomExceptionGetCode
}

pub type Test262Native {
  Test262EvalScript(realm: Int)
  Test262CreateRealm(realm: Int)
  Test262Gc
}

pub type ShadowRealmNative {
  ShadowRealmConstructor(proto: Handle)
  ShadowRealmEvaluate(realm: Int)
  ShadowRealmImportValue(realm: Int)
  WrappedFunctionCall(target: Handle, caller_realm: Int, target_realm: Int)
}

pub type PromiseNative {
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
  PromiseAllKeyedStatic
  PromiseAllSettledKeyedStatic
  PromiseCapabilityExecutor(resolve_box: Handle, reject_box: Handle)
  PromiseAllResolveElement(
    index: Int,
    remaining: Handle,
    values: Handle,
    already_called: Handle,
    resolve: JsVal,
  )
  PromiseAllSettledElement(
    fulfilled: Bool,
    index: Int,
    remaining: Handle,
    values: Handle,
    already_called: Handle,
    resolve: JsVal,
  )
  PromiseAnyRejectElement(
    index: Int,
    remaining: Handle,
    errors: Handle,
    already_called: Handle,
    reject: JsVal,
  )
  PromiseKeyedElement(
    kind: PromiseKeyedKind,
    index: Int,
    remaining: Handle,
    keys: Handle,
    values: Handle,
    already_called: Handle,
    resolve: JsVal,
  )
  PromiseFinallyFn(rejecting: Bool, on_finally: JsVal, constructor: JsVal)
  PromiseFinallyValueThunk(value: JsVal)
  PromiseFinallyThrower(reason: JsVal)
}

pub type PromiseKeyedKind {
  KeyedValue
  KeyedFulfilled
  KeyedRejected
}

pub type IteratorNative {
  AsyncFromSyncNext
  AsyncFromSyncReturn
  AsyncFromSyncThrow
  AsyncFromSyncUnwrap(done: Bool)
  AsyncFromSyncClose(sync_iter: Handle)
  IteratorConstructor
  IteratorFrom
  IteratorZip
  IteratorZipKeyed
  IteratorConcat
  IteratorPrototypeToArray
  IteratorPrototypeForEach
  IteratorPrototypeReduce
  IteratorPrototypeSome
  IteratorPrototypeEvery
  IteratorPrototypeFind
  IteratorPrototypeMap
  IteratorPrototypeFilter
  IteratorPrototypeTake
  IteratorPrototypeDrop
  IteratorPrototypeFlatMap
  IteratorPrototypeChunks
  IteratorPrototypeWindows
  IteratorPrototypeIncludes
  IteratorPrototypeJoin
  IteratorPrototypeDispose
  AsyncIteratorPrototypeAsyncDispose
  IteratorHelperNext
  IteratorHelperReturn
  WrapForValidIteratorNext
  WrapForValidIteratorReturn
  IteratorProtoGetToStringTag
  IteratorProtoSetToStringTag
  IteratorProtoGetConstructor
  IteratorProtoSetConstructor
  ArrayIteratorNext
  MapIteratorNext
  SetIteratorNext
  StringIteratorNext
}

pub type GeneratorNative {
  GeneratorNext
  GeneratorReturn
  GeneratorThrow
  AsyncGeneratorNext
  AsyncGeneratorReturn
  AsyncGeneratorThrow
  GeneratorFunctionCtor(realm: Int)
  AsyncGeneratorFunctionCtor(realm: Int)
  AsyncFunctionCtor(realm: Int)
}

pub type ObjectNative {
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
  ObjectPrototypeDefineGetter
  ObjectPrototypeDefineSetter
  ObjectPrototypeLookupGetter
  ObjectPrototypeLookupSetter
  ObjectPrototypeProtoGetter
  ObjectPrototypeProtoSetter
}

pub type FunctionNative {
  FunctionConstructor(realm: Int)
  FunctionApply
  FunctionBind
  FunctionCall
  FunctionToString
  FunctionHasInstance
  FunctionPrototypeCall
  ThrowTypeErrorFn
}

pub type ErrorNative {
  ErrorConstructor(proto: Handle)
  AggregateErrorConstructor(proto: Handle)
  SuppressedErrorConstructor(proto: Handle)
  ErrorPrototypeToString
  ErrorCaptureStackTrace
  ErrorStackGetter
  ErrorStackSetter(realm: Int)
  ErrorIsError
}

pub type DateNative {
  DateConstructor
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

pub type RegExpFlag {
  HasIndicesFlag
  GlobalFlag
  IgnoreCaseFlag
  MultilineFlag
  DotAllFlag
  UnicodeFlag
  UnicodeSetsFlag
  StickyFlag
}

pub type LegacyStatic {
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

pub type LegacyStatics {
  LegacyStatics(
    input: String,
    subject: String,
    whole: #(Int, Int),
    groups: List(#(Int, Int)),
  )
}

pub fn empty_legacy_statics() -> LegacyStatics {
  LegacyStatics(input: "", subject: "", whole: #(0, 0), groups: [])
}

pub type RegExpNative {
  // per-realm regexp caches: pristine prototype props and compiled patterns
  RegExpConstructor(
    legacy: LegacyStatics,
    proto_props: Option(Dict(PropertyKey, Property)),
    compiled: Dict(String, CompiledRegExp),
  )
  RegExpLegacyGetter(ctor: Handle, which: LegacyStatic)
  RegExpLegacyInputSetter(ctor: Handle)
  RegExpPrototypeExec
  RegExpPrototypeTest
  RegExpPrototypeToString
  RegExpPrototypeCompile
  RegExpGetSource
  RegExpGetFlags
  RegExpGetFlag(flag: RegExpFlag)
  RegExpSymbolMatch
  RegExpSymbolMatchAll
  RegExpSymbolReplace
  RegExpSymbolSearch
  RegExpSymbolSplit
  RegExpStringIteratorNext
}

pub type ArrayBufferNative {
  ArrayBufferConstructor
  ArrayBufferIsView
  ArrayBufferGetByteLength
  ArrayBufferGetDetached
  ArrayBufferGetMaxByteLength
  ArrayBufferGetResizable
  ArrayBufferSlice
  ArrayBufferResize
  ArrayBufferTransfer
  ArrayBufferTransferToFixedLength
  ArrayBufferGetImmutable
  ArrayBufferSliceToImmutable
  ArrayBufferTransferToImmutable
  ArrayBufferDetach262
  SharedArrayBufferConstructor
  SharedArrayBufferGetByteLength
  SharedArrayBufferGrow
  SharedArrayBufferGetGrowable
  SharedArrayBufferGetMaxByteLength
  SharedArrayBufferSlice
}

pub type TypedArrayNative {
  TypedArrayIntrinsicConstructor
  TypedArrayConstructor(kind: TypedArrayKind)
  TypedArrayFrom
  TypedArrayOf
  TypedArrayGetBuffer
  TypedArrayGetByteLength
  TypedArrayGetByteOffset
  TypedArrayGetLength
  TypedArrayGetToStringTag
  TypedArrayPrototypeAt
  TypedArrayPrototypeCopyWithin
  TypedArrayPrototypeEntries
  TypedArrayPrototypeEvery
  TypedArrayPrototypeFill
  TypedArrayPrototypeFilter
  TypedArrayPrototypeFind
  TypedArrayPrototypeFindIndex
  TypedArrayPrototypeFindLast
  TypedArrayPrototypeFindLastIndex
  TypedArrayPrototypeForEach
  TypedArrayPrototypeIncludes
  TypedArrayPrototypeIndexOf
  TypedArrayPrototypeJoin
  TypedArrayPrototypeKeys
  TypedArrayPrototypeLastIndexOf
  TypedArrayPrototypeMap
  TypedArrayPrototypeReduce
  TypedArrayPrototypeReduceRight
  TypedArrayPrototypeReverse
  TypedArrayPrototypeSet
  TypedArrayPrototypeSlice
  TypedArrayPrototypeSome
  TypedArrayPrototypeSort
  TypedArrayPrototypeSubarray
  TypedArrayPrototypeToLocaleString
  TypedArrayPrototypeToReversed
  TypedArrayPrototypeToSorted
  TypedArrayPrototypeValues
  TypedArrayPrototypeWith
  Uint8ArrayPrototypeToBase64
  Uint8ArrayPrototypeToHex
  Uint8ArrayPrototypeSetFromBase64
  Uint8ArrayPrototypeSetFromHex
  Uint8ArrayFromBase64
  Uint8ArrayFromHex
}

pub type DataViewNative {
  DataViewConstructor
  DataViewGetBuffer
  DataViewGetByteLength
  DataViewGetByteOffset
  DataViewGet(element: ViewElementType)
  DataViewSet(element: ViewElementType)
}

pub type AtomicsNative {
  AtomicsAdd
  AtomicsAnd
  AtomicsCompareExchange
  AtomicsExchange
  AtomicsIsLockFree
  AtomicsLoad
  AtomicsNotify
  AtomicsOr
  AtomicsPause
  AtomicsStore
  AtomicsSub
  AtomicsWait
  AtomicsWaitAsync
  AtomicsXor
}

pub type ProxyNative {
  ProxyConstructor
  ProxyRevocable
  ProxyRevoke(proxy: Handle)
}

pub type MathNative {
  MathAbs
  MathAcos
  MathAcosh
  MathAsin
  MathAsinh
  MathAtan
  MathAtan2
  MathAtanh
  MathCbrt
  MathCeil
  MathClz32
  MathCos
  MathCosh
  MathExp
  MathExpm1
  MathFloor
  MathFround
  MathHypot
  MathImul
  MathLog
  MathLog10
  MathLog1p
  MathLog2
  MathMax
  MathMin
  MathPow
  MathRandom
  MathRound
  MathSign
  MathSin
  MathSinh
  MathSqrt
  MathTan
  MathTanh
  MathTrunc
}

pub type JsonNative {
  JsonParse(realm: Int)
  JsonStringify(realm: Int)
  JsonRawJson(realm: Int)
  JsonIsRawJson(realm: Int)
}

pub type ReflectNative {
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

pub type ConsoleNative {
  ConsolePrint(level: ConsoleLevel)
}

pub type GlobalNative {
  GlobalEval(realm: Int)
  GlobalParseInt
  GlobalParseFloat
  GlobalIsNaN
  GlobalIsFinite
  GlobalEncodeUri
  GlobalEncodeUriComponent
  GlobalDecodeUri
  GlobalDecodeUriComponent
  GlobalEscape
  GlobalUnescape
}

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

pub type MapNative {
  MapConstructor
  MapGroupBy
  MapGet
  MapSet
  MapHas
  MapDelete
  MapClear
  MapForEach
  MapGetSize
  MapKeys
  MapValues
  MapEntries
  MapGetOrInsert
  MapGetOrInsertComputed
}

pub type SetNative {
  SetConstructor
  SetAdd
  SetHas
  SetDelete
  SetClear
  SetForEach
  SetGetSize
  SetValues
  SetEntries
  SetUnion
  SetIntersection
  SetDifference
  SetSymmetricDifference
  SetIsSubsetOf
  SetIsSupersetOf
  SetIsDisjointFrom
}

pub type WeakNative {
  WeakMapConstructor
  WeakMapGet
  WeakMapSet
  WeakMapHas
  WeakMapDelete
  WeakMapGetOrInsert
  WeakMapGetOrInsertComputed
  WeakSetConstructor
  WeakSetAdd
  WeakSetHas
  WeakSetDelete
}

pub type FromAsyncContext {
  FromAsyncContext(
    iter: JsVal,
    next_method: JsVal,
    map_fn: Option(JsVal),
    this_arg: JsVal,
    target: JsVal,
    k: Int,
    resolve: JsVal,
    reject: JsVal,
  )
}

pub type FromAsyncLikeContext {
  FromAsyncLikeContext(
    items: JsVal,
    map_fn: Option(JsVal),
    this_arg: JsVal,
    target: JsVal,
    k: Int,
    len: Int,
    resolve: JsVal,
    reject: JsVal,
  )
}

pub type ArrayNative {
  ArrayConstructor
  ArrayIsArray
  ArrayFrom
  ArrayFromAsync
  ArrayFromAsyncOnNext(ctx: FromAsyncContext)
  ArrayFromAsyncOnMapped(ctx: FromAsyncContext)
  ArrayFromAsyncCloseReject(iter: JsVal, reject: JsVal)
  ArrayFromAsyncRejectWith(error: JsVal, reject: JsVal)
  ArrayFromAsyncLikeOnValue(ctx: FromAsyncLikeContext)
  ArrayFromAsyncLikeOnMapped(ctx: FromAsyncLikeContext)
  ArrayOf
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
  ArrayPrototypeFindLast
  ArrayPrototypeFindLastIndex
  ArrayPrototypeSort
  ArrayPrototypeSplice
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
}

pub type StringNative {
  StringConstructor
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
  StringRaw
  StringFromCharCode
  StringFromCodePoint
}

pub type NumberNative {
  NumberConstructor
  NumberIsNaN
  NumberIsFinite
  NumberIsInteger
  NumberIsSafeInteger
  NumberPrototypeValueOf
  NumberPrototypeToString
  NumberPrototypeToFixed
  NumberPrototypeToPrecision
  NumberPrototypeToExponential
  NumberPrototypeToLocaleString
}

pub type BooleanNative {
  BooleanConstructor
  BooleanPrototypeValueOf
  BooleanPrototypeToString
}

pub type SymbolNative {
  SymbolConstructor
  SymbolFor
  SymbolKeyFor
  SymbolToString
  SymbolValueOf
  SymbolToPrimitive
  SymbolDescriptionGetter
}

pub type BigIntNative {
  BigIntGlobal
  BigIntAsIntN
  BigIntAsUintN
  BigIntPrototypeToString
  BigIntPrototypeToLocaleString
  BigIntPrototypeValueOf
}

pub type IntlNative {
  IntlGetCanonicalLocales
  IntlSupportedValuesOf
  IntlConstructor(service: ConstructibleService, proto: Handle)
  IntlSupportedLocalesOf
  IntlResolvedOptions(service: IntlService)
  IntlBoundGetter(service: BoundGetterService)
  IntlBoundMethod(service: BoundGetterService, target: Handle)
  IntlMethod(service: IntlService, method: IntlMethodName)
  IntlHostOverride(which: IntlHostOverrideName)
  IntlSegmenterSegment(segments_proto: Handle)
  IntlSegmentsIterator(iter_proto: Handle)
  IntlLocaleGetter(name: LocaleGetterName)
  IntlLocaleMethod(method: LocaleMethodName, proto: Handle)
}

pub type IntlMethodName {
  IntlFormat
  IntlFormatToParts
  IntlFormatRange
  IntlFormatRangeToParts
  IntlSelect
  IntlSelectRange
  IntlOf
  IntlSegmentIteratorNext
  IntlSegmentsContaining
}

pub type IntlHostOverrideName {
  NumberToLocaleString
  BigIntToLocaleString
  StringLocaleCompare
  StringToLocaleLowerCase
  StringToLocaleUpperCase
  DateToLocaleString
  DateToLocaleDateString
  DateToLocaleTimeString
}

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

pub type TemporalNative {
  TemporalInstantCtor(protos: TemporalProtos)
  TemporalInstantStatic(name: InstantStaticName, protos: TemporalProtos)
  TemporalInstantGetter(getter: InstantGetterName)
  TemporalInstantMethod(method: InstantMethodName, protos: TemporalProtos)
  TemporalNowFn(name: TemporalNowName, protos: TemporalProtos)
  TemporalPlainDateTimeCtor(protos: TemporalProtos)
  TemporalPlainDateTimeStatic(name: TemporalStaticName, protos: TemporalProtos)
  TemporalPlainDateTimeGetter(getter: TemporalDateTimeGetter)
  TemporalPlainDateTimeMethod(
    method: PlainDateTimeMethod,
    protos: TemporalProtos,
  )
  TemporalPlainTimeCtor(protos: TemporalProtos)
  TemporalPlainTimeStatic(name: TemporalStaticName, protos: TemporalProtos)
  TemporalPlainTimeGetter(getter: TemporalTimeGetter)
  TemporalPlainTimeMethod(method: PlainTimeMethod, protos: TemporalProtos)
  TemporalDurationCtor(protos: TemporalProtos)
  TemporalDurationStatic(name: TemporalStaticName, protos: TemporalProtos)
  TemporalDurationGetter(getter: TemporalDurationGetter)
  TemporalDurationMethod(method: DurationMethod, protos: TemporalProtos)
  TemporalPlainDateCtor(protos: TemporalProtos)
  TemporalPlainDateStatic(name: TemporalStaticName, protos: TemporalProtos)
  TemporalPlainDateGetter(getter: TemporalDateGetter)
  TemporalPlainDateMethod(method: PlainDateMethod, protos: TemporalProtos)
  TemporalPlainYearMonthCtor(protos: TemporalProtos)
  TemporalPlainYearMonthStatic(name: TemporalStaticName, protos: TemporalProtos)
  TemporalPlainYearMonthGetter(getter: TemporalYearMonthGetter)
  TemporalPlainYearMonthMethod(
    method: PlainYearMonthMethod,
    protos: TemporalProtos,
  )
  TemporalPlainMonthDayCtor(protos: TemporalProtos)
  TemporalPlainMonthDayStatic(protos: TemporalProtos)
  TemporalPlainMonthDayGetter(getter: TemporalMonthDayGetter)
  TemporalPlainMonthDayMethod(
    method: PlainMonthDayMethod,
    protos: TemporalProtos,
  )
  TemporalZonedDateTimeCtor(protos: TemporalProtos)
  TemporalZonedDateTimeStatic(name: TemporalStaticName, protos: TemporalProtos)
  TemporalZonedDateTimeGetter(getter: TemporalZonedGetter)
  TemporalZonedDateTimeMethod(
    method: ZonedDateTimeMethod,
    protos: TemporalProtos,
  )
}

pub type TemporalZonedGetter {
  ZonedTimeZoneId
  ZonedEpochMilliseconds
  ZonedEpochNanoseconds
  ZonedOffsetNanoseconds
  ZonedOffset
  ZonedHoursInDay
  ZonedDate(TemporalDateGetter)
  ZonedTime(TemporalTimeGetter)
}

pub type ZonedDateTimeMethod {
  ZonedDateTimeWithTimeZone
  ZonedDateTimeWithCalendar
  ZonedDateTimeWithPlainTime
  ZonedDateTimeWith
  ZonedDateTimeAdd
  ZonedDateTimeSubtract
  ZonedDateTimeUntil
  ZonedDateTimeSince
  ZonedDateTimeRound
  ZonedDateTimeEquals
  ZonedDateTimeToString
  ZonedDateTimeToLocaleString
  ZonedDateTimeToJson
  ZonedDateTimeValueOf
  ZonedDateTimeStartOfDay
  ZonedDateTimeGetTimeZoneTransition
  ZonedDateTimeToInstant
  ZonedDateTimeToPlainDate
  ZonedDateTimeToPlainTime
  ZonedDateTimeToPlainDateTime
}

pub type TemporalYearMonthGetter {
  YearMonthCalendarId
  YearMonthEra
  YearMonthEraYear
  YearMonthYear
  YearMonthMonth
  YearMonthMonthCode
  YearMonthDaysInYear
  YearMonthDaysInMonth
  YearMonthMonthsInYear
  YearMonthInLeapYear
}

pub type TemporalMonthDayGetter {
  MonthDayCalendarId
  MonthDayMonthCode
  MonthDayDay
}

pub type PlainYearMonthMethod {
  PlainYearMonthWith
  PlainYearMonthAdd
  PlainYearMonthSubtract
  PlainYearMonthUntil
  PlainYearMonthSince
  PlainYearMonthEquals
  PlainYearMonthToString
  PlainYearMonthToLocaleString
  PlainYearMonthToJson
  PlainYearMonthValueOf
  PlainYearMonthToPlainDate
}

pub type PlainMonthDayMethod {
  PlainMonthDayWith
  PlainMonthDayEquals
  PlainMonthDayToString
  PlainMonthDayToLocaleString
  PlainMonthDayToJson
  PlainMonthDayValueOf
  PlainMonthDayToPlainDate
}

pub type PlainDateMethod {
  PlainDateToPlainYearMonth
  PlainDateToPlainMonthDay
  PlainDateToPlainDateTime
  PlainDateToZonedDateTime
  PlainDateAdd
  PlainDateSubtract
  PlainDateWith
  PlainDateWithCalendar
  PlainDateUntil
  PlainDateSince
  PlainDateEquals
  PlainDateToString
  PlainDateToLocaleString
  PlainDateToJson
  PlainDateValueOf
}

pub type PlainTimeMethod {
  PlainTimeAdd
  PlainTimeSubtract
  PlainTimeWith
  PlainTimeUntil
  PlainTimeSince
  PlainTimeRound
  PlainTimeEquals
  PlainTimeToString
  PlainTimeToLocaleString
  PlainTimeToJson
  PlainTimeValueOf
}

pub type TemporalProtos {
  TemporalProtos(
    plain_date: Handle,
    plain_time: Handle,
    plain_date_time: Handle,
    plain_year_month: Handle,
    plain_month_day: Handle,
    duration: Handle,
    instant: Handle,
    zoned_date_time: Handle,
  )
}

pub type TemporalDurationGetter {
  DurationYears
  DurationMonths
  DurationWeeks
  DurationDays
  DurationHours
  DurationMinutes
  DurationSeconds
  DurationMilliseconds
  DurationMicroseconds
  DurationNanoseconds
  DurationSign
  DurationBlank
}

pub type DurationMethod {
  DurationWith
  DurationNegated
  DurationAbs
  DurationAdd
  DurationSubtract
  DurationRound
  DurationTotal
  DurationToString
  DurationToJson
  DurationToLocaleString
  DurationValueOf
}

pub type TemporalStaticName {
  FromStatic
  CompareStatic
}

pub type TemporalDateGetter {
  DateCalendarId
  DateEra
  DateEraYear
  DateYear
  DateMonth
  DateMonthCode
  DateDay
  DateDayOfWeek
  DateDayOfYear
  DateWeekOfYear
  DateYearOfWeek
  DateDaysInWeek
  DateDaysInMonth
  DateDaysInYear
  DateMonthsInYear
  DateInLeapYear
}

pub type TemporalTimeGetter {
  TimeHour
  TimeMinute
  TimeSecond
  TimeMillisecond
  TimeMicrosecond
  TimeNanosecond
}

pub type TemporalDateTimeGetter {
  DateTimeDate(TemporalDateGetter)
  DateTimeTime(TemporalTimeGetter)
}

pub type PlainDateTimeMethod {
  PlainDateTimeWith
  PlainDateTimeWithPlainTime
  PlainDateTimeWithCalendar
  PlainDateTimeAdd
  PlainDateTimeSubtract
  PlainDateTimeUntil
  PlainDateTimeSince
  PlainDateTimeRound
  PlainDateTimeEquals
  PlainDateTimeToString
  PlainDateTimeToLocaleString
  PlainDateTimeToJson
  PlainDateTimeValueOf
  PlainDateTimeToPlainDate
  PlainDateTimeToPlainTime
  PlainDateTimeToZonedDateTime
}

pub type InstantStaticName {
  InstantFrom
  InstantFromEpochMilliseconds
  InstantFromEpochNanoseconds
  InstantCompare
}

pub type InstantGetterName {
  InstantEpochMilliseconds
  InstantEpochNanoseconds
}

pub type InstantMethodName {
  InstantAdd
  InstantSubtract
  InstantUntil
  InstantSince
  InstantRound
  InstantEquals
  InstantToString
  InstantToLocaleString
  InstantToJson
  InstantValueOf
  InstantToZonedDateTimeIso
}

pub type TemporalNowName {
  NowInstant
  NowTimeZoneId
  NowPlainDateISO
  NowPlainTimeISO
  NowPlainDateTimeISO
  NowZonedDateTimeISO
}

pub type AsyncGenResumeKind {
  AwaitingReturnKind
  ReturnUnwindKind
}

pub type IteratorRecord {
  IteratorRecord(iterator: JsVal, next_method: JsVal)
}

pub type IteratorHelperKind {
  HelperMap(func: JsVal)
  HelperFilter(func: JsVal)
  HelperTake(remaining: Int)
  HelperDrop(remaining: Int)
  HelperFlatMap(func: JsVal, inner: Option(IteratorRecord))
  HelperChunks(size: Int)
  HelperWindows(size: Int, undersized: Undersized, buffer: List(JsVal))
}

pub type Undersized {
  OnlyFull
  AllowPartial
}

pub type ZipMode {
  ZipShortest
  ZipLongest
  ZipStrict
}

pub type ZipMember {
  ZipOpen(record: IteratorRecord, padding: JsVal)
  ZipExhausted(padding: JsVal)
}

pub type ConcatItem {
  ConcatItem(open_method: JsVal, iterable: JsVal)
}

pub type HelperBody {
  ClassicHelper(
    kind: IteratorHelperKind,
    underlying: IteratorRecord,
    counter: Int,
  )
  ZipHelper(
    members: List(ZipMember),
    mode: ZipMode,
    keys: Option(List(ObjectKey)),
  )
  ConcatHelper(remaining: List(ConcatItem), inner: Option(IteratorRecord))
}

pub type ObjKind {
  Ordinary
  // a realm's global object, plain but writes bump the store's global_epoch
  GlobalObj
  ArrayObj(length: Int)
  ArgumentsObj(length: Int, mapped: Option(List(Handle)))
  StringObj(value: String)
  NumberObj(value: JsNum)
  BooleanObj(value: Bool)
  BigIntObj(value: Int)
  SymbolObj(value: SymbolId)
  CompiledFn(
    code: CompiledCode,
    home_object: Option(Handle),
    flags: FnFlags,
    fields_init: Option(Handle),
    direct_entry: Option(DirectEntry),
    name: String,
    length: Int,
    birth: FnBirth,
  )
  BytecodeFn(
    template: FuncTemplate,
    env: EnvTuple,
    home_object: Option(Handle),
    flags: FnFlags,
    fields_init: Option(Handle),
    realm: Int,
    unit_id: Int,
    birth: FnBirth,
  )
  NativeFn(token: NativeToken, name: String, length: Int, constructible: Bool)
  BoundFn(target: Handle, bound_this: JsVal, bound_args: List(JsVal))
  HostObj(payload: HostTerm)
  ErrorObj(stack: String)
  MapObj(entries: OrderedEntries(MapKey, JsVal))
  SetObj(entries: OrderedEntries(MapKey, JsVal))
  WeakMapObj(entries: Dict(WeakKey, JsVal))
  WeakSetObj(entries: Set(WeakKey))
  DateObj(ms: JsNum)
  RegExpObj(source: String, flags: String, compiled: CompiledRegExp)
  ArrayBufferObj(storage: BufferStorage)
  TypedArrayObj(
    buffer: Handle,
    elem_kind: TypedArrayKind,
    byte_offset: Int,
    // None = length-tracking over a resizable buffer
    length: Option(Int),
  )
  // byte_length None = auto-tracking
  DataViewObj(buffer: Handle, byte_offset: Int, byte_length: Option(Int))
  RawJsonObj(raw: String)
  ModuleNamespace(exports: Dict(String, Handle))
  ProxyObj(target: Handle, handler: Handle, revoked: Bool)
  ArrayIterator(target: Handle, index: Int, kind: ArrayIterKind)
  MapIterator(target: Handle, index: Int, kind: MapIterKind)
  SetIterator(target: Handle, index: Int, kind: SetIterKind)
  StringIterator(source: String, index: Int)
  PromiseObj(data: Handle)
  GeneratorObj(data: Handle)
  AsyncGeneratorObj(data: Handle)
  AsyncFromSyncIterator(sync_rec: Handle)
  IteratorHelperObj(gen_state: GeneratorState, body: HelperBody)
  WrapForValidIteratorObj(record: IteratorRecord)
  IntlObj(data: IntlData, bound: Option(Handle))
  TemporalObj(data: TemporalData)
  DisposableStackObj(async: Bool, state: DisposableState)
  FinalizationRegistryObj(callback: JsVal, registrations: List(Registration))
  // weak: gc does not trace target
  WeakRefObj(target: Option(JsVal))
  ShadowRealmObj(realm: Int)
}

pub type Cell {
  SObject(
    kind: ObjKind,
    proto: Option(Handle),
    props: Dict(PropertyKey, Property),
    symbol_props: List(#(SymbolId, Property)),
    elements: JsElements,
    extensible: Bool,
  )
  SBox(value: JsVal)
  SPromiseData(state: PromiseState, is_handled: Bool)
  SGenerator(state: GeneratorState, resume: Resume)
  SAsyncGen(
    state: AsyncGenState,
    resume: Resume,
    front: List(AsyncGenRequest),
    back: List(AsyncGenRequest),
  )
  SAsyncContext(resume: Resume, promise: Handle)
  SDisposeCapability(resources: List(DisposeResource))
  // offsets mirrors the shape so reads skip the table
  SShapedObject(
    shape_id: Int,
    proto: Option(Handle),
    slots: ShapeSlots,
    offsets: Dict(BitArray, Int),
  )
}

// extensible ordinary-shaped cell with no symbol props or elements
pub fn plain_object(
  kind: ObjKind,
  proto: Option(Handle),
  props: Dict(PropertyKey, Property),
) -> Cell {
  SObject(
    kind:,
    proto:,
    props:,
    symbol_props: [],
    elements: NoElements,
    extensible: True,
  )
}

pub type ShapeSlots

pub type ShapeDesc {
  ShapeDesc(
    slot_count: Int,
    offsets: Dict(BitArray, Int),
    transitions: Dict(BitArray, Int),
  )
}

// droppable cache, not a gc root; see arc_rt_call_ic_ffi
pub type IcEntry {
  IcRead(key: BitArray, offsets: Dict(Int, Int))
  IcCall(
    key: BitArray,
    ways: Dict(IcCallMatch, IcCallWay),
    shaped: Dict(Int, Dict(Int, IcCallWay)),
  )
  IcInit(from: Int, to: Int, blank: Cell, chain: List(#(Int, Cell)))
  IcGlobal(key: BitArray, epoch: Int, value: JsVal, refills: Int)
  IcOff
}

// bare {chain, callee, kind} tuple as arc_rt_call_ic_ffi ic_fill builds it
type IcCallWay =
  #(List(#(Int, Cell)), Handle, ObjKind)

// shaped receivers key by shape id then proto id in IcCall.shaped
pub type IcCallMatch {
  IcPlain(proto_id: Int)
  IcOwn(id: Int)
  IcPrim(wrapper: Int, proto_id: Int)
}

pub type ReactionHandler {
  Handler(fun: JsVal)
  IdentityPassThrough
  ThrowerPassThrough
}

pub type PromiseReaction {
  PromiseReaction(
    on_fulfill: ReactionHandler,
    on_reject: ReactionHandler,
    child_resolve: JsVal,
    child_reject: JsVal,
  )
}

pub type PromiseState {
  PromisePending(reactions: List(PromiseReaction))
  PromiseFulfilled(JsVal)
  PromiseRejected(JsVal)
}

pub type Job {
  ReactionJob(
    handler: ReactionHandler,
    arg: JsVal,
    resolve: JsVal,
    reject: JsVal,
  )
  ResolveThenableJob(
    thenable: JsVal,
    then_fn: JsVal,
    resolve: JsVal,
    reject: JsVal,
  )
  HostJob(run: fn(Agent) -> Agent)
}

pub type GeneratorCompletion {
  GenNext
  GenReturn
  GenThrow
}

pub type GeneratorState {
  GenSuspendedStart
  GenSuspendedYield
  GenExecuting
  GenCompleted
}

pub type AsyncGenState {
  AsyncGenSuspendedStart
  AsyncGenSuspendedYield
  AsyncGenExecuting
  AsyncGenAwaitingReturn
  AsyncGenCompleted
}

pub type AsyncGenRequest {
  AsyncGenRequest(
    completion: GeneratorCompletion,
    value: JsVal,
    resolve: JsVal,
    reject: JsVal,
  )
}

// aot coroutine body compiled to a resumable state machine
pub type StateMachine

// the state machine's saved locals tuple
pub type Locals

pub type Resume {
  ResumeCompiled(machine: StateMachine, resume_point: Int, locals: Locals)
  ResumeFrame(frame: SuspendedFrame)
}

pub type Step {
  StepReturn(JsVal)
  StepThrow(JsVal)
  StepYield(value: JsVal, resume: Resume)
  StepAwait(value: JsVal, resume: Resume)
}

pub type JobQueue

@external(erlang, "arc_rt_job_queue_ffi", "job_queue_new")
pub fn job_queue_new() -> JobQueue

@external(erlang, "arc_rt_job_queue_ffi", "job_queue_push")
pub fn job_queue_push(queue: JobQueue, item: Job) -> JobQueue

@external(erlang, "arc_rt_job_queue_ffi", "job_queue_pop")
pub fn job_queue_pop(queue: JobQueue) -> Option(#(Job, JobQueue))

@external(erlang, "arc_rt_job_queue_ffi", "job_queue_to_list")
pub fn job_queue_to_list(queue: JobQueue) -> List(Job)

pub type BuiltinPair {
  BuiltinPair(prototype: Handle, constructor: Handle)
}

pub type TypedArrays {
  TypedArrays(by_kind: Dict(TypedArrayKind, BuiltinPair))
}

pub type LexicalGlobal {
  Let(JsVal)
  Const(JsVal)
}

pub fn lexical_global_value(g: LexicalGlobal) -> JsVal {
  case g {
    Let(v) | Const(v) -> v
  }
}

pub fn lexical_global_with_value(g: LexicalGlobal, v: JsVal) -> LexicalGlobal {
  case g {
    Let(_) -> Let(v)
    Const(_) -> Const(v)
  }
}

pub type Realm {
  Realm(
    object: BuiltinPair,
    function: BuiltinPair,
    array: BuiltinPair,
    string: BuiltinPair,
    number: BuiltinPair,
    boolean: BuiltinPair,
    symbol: BuiltinPair,
    bigint: BuiltinPair,
    error: BuiltinPair,
    type_error: BuiltinPair,
    reference_error: BuiltinPair,
    range_error: BuiltinPair,
    syntax_error: BuiltinPair,
    eval_error: BuiltinPair,
    uri_error: BuiltinPair,
    aggregate_error: BuiltinPair,
    map: BuiltinPair,
    set: BuiltinPair,
    weak_map: BuiltinPair,
    weak_set: BuiltinPair,
    weak_ref: BuiltinPair,
    finalization_registry: BuiltinPair,
    date: BuiltinPair,
    regexp: BuiltinPair,
    promise: BuiltinPair,
    proxy: BuiltinPair,
    array_buffer: BuiltinPair,
    data_view: BuiltinPair,
    typed_arrays: TypedArrays,
    math: Handle,
    json: Handle,
    reflect: Handle,
    console: Handle,
    atomics: Handle,
    iterator_proto: Handle,
    array_iter_proto: Handle,
    string_iter_proto: Handle,
    map_iter_proto: Handle,
    set_iter_proto: Handle,
    async_iterator_proto: Handle,
    async_from_sync_proto: Handle,
    iterator: BuiltinPair,
    iterator_helper_proto: Handle,
    wrap_for_valid_proto: Handle,
    generator: BuiltinPair,
    generator_fn: BuiltinPair,
    async_fn: BuiltinPair,
    async_gen: BuiltinPair,
    throw_type_error: Handle,
    global_object: Handle,
    // fields above are abi (arc_rt_layout.hrl), append only
    shared_array_buffer: BuiltinPair,
    id: Int,
    lexical_globals: Dict(String, LexicalGlobal),
    suppressed_error: BuiltinPair,
  )
}

pub fn unset_realm() -> Realm {
  let h = Handle(-1)
  let p = BuiltinPair(prototype: h, constructor: h)
  Realm(
    object: p,
    function: p,
    array: p,
    string: p,
    number: p,
    boolean: p,
    symbol: p,
    bigint: p,
    error: p,
    type_error: p,
    reference_error: p,
    range_error: p,
    syntax_error: p,
    eval_error: p,
    uri_error: p,
    aggregate_error: p,
    map: p,
    set: p,
    weak_map: p,
    weak_set: p,
    weak_ref: p,
    finalization_registry: p,
    date: p,
    regexp: p,
    promise: p,
    proxy: p,
    array_buffer: p,
    data_view: p,
    typed_arrays: TypedArrays(by_kind: dict.new()),
    math: h,
    json: h,
    reflect: h,
    console: h,
    atomics: h,
    iterator_proto: h,
    array_iter_proto: h,
    string_iter_proto: h,
    map_iter_proto: h,
    set_iter_proto: h,
    async_iterator_proto: h,
    async_from_sync_proto: h,
    iterator: p,
    iterator_helper_proto: h,
    wrap_for_valid_proto: h,
    generator: p,
    generator_fn: p,
    async_fn: p,
    async_gen: p,
    throw_type_error: h,
    global_object: h,
    shared_array_buffer: p,
    id: -1,
    lexical_globals: dict.new(),
    suppressed_error: p,
  )
}

pub type EvalKind {
  IndirectEval
  DynamicFunction
  ScriptEval
}

// late-bound upcalls seeded by builtins.seed_ops and interp entry
pub type JsOps {
  JsOps(
    get_prop: fn(Agent, JsVal, ObjectKey) -> #(JsVal, Agent),
    call: fn(Agent, JsVal, JsVal, List(JsVal)) -> #(JsVal, Agent),
    to_object: fn(Agent, JsVal) -> #(Handle, Agent),
    new_error: fn(Agent, ErrorKind, String) -> #(JsVal, Agent),
    eval_hook: fn(Agent, String, EvalKind) -> #(JsVal, Agent),
    call_bytecode: fn(Agent, Handle, ObjKind, JsVal, List(JsVal)) ->
      #(Result(JsVal, JsVal), Agent),
    prepare_call: fn(Agent, Handle, ObjKind, JsVal) ->
      fn(Agent, List(JsVal)) -> #(JsVal, Agent),
    construct_bytecode: fn(Agent, Handle, List(JsVal), JsVal) ->
      #(Handle, Agent),
    resume_frame: fn(Agent, SuspendedFrame, #(Int, JsVal)) -> #(Step, Agent),
  )
}

// field order is abi (arc_rt_layout.hrl); hot fields only, rest in meta
pub type Store {
  Store(
    cells: Arena(Cell),
    next_id: Int,
    alloc_since_gc: Int,
    gc_threshold: Int,
    prop_seq: Int,
    shapes: Dict(Int, ShapeDesc),
    next_shape: Int,
    ics: Dict(Int, IcEntry),
    // proto id to whether its chain takes plain named writes
    plain_write_protos: Dict(Int, Nil),
    // bumped on any write to a global object cell, for global read caches
    global_epoch: Int,
    ops: JsOps,
    microtasks: JobQueue,
    pinned_roots: Set(Int),
    meta: StoreMeta,
  )
}

// rarely written, kept out of the record every heap write copies
pub type StoreMeta {
  StoreMeta(
    live_count: Int,
    next_private_id: Int,
    next_symbol_id: Int,
    next_unit_id: Int,
    unhandled_rejections: List(Int),
    // cells as of the last gc, ids below young_start are the old generation
    old_gen: Arena(Cell),
    young_start: Int,
    // old cells holding weak refs, pruned each minor gc
    old_weak_ids: List(Int),
    // live_count right after the last major gc
    major_live: Int,
    minors_since_major: Int,
  )
}

// field order is abi (arc_rt_layout.hrl), append only
pub type Agent {
  Agent(
    store: Store,
    realm: Realm,
    template_objects: Dict(String, Handle),
    frames: List(FrameInfo),
    hooks: HostHooks,
    host_fns: Dict(Int, HostFnEntry),
    // stale for the current realm, read `realm` instead
    realms: Dict(Int, Realm),
    import_hook: Option(HostFnEntry),
    waiters: List(AsyncWaiter),
    // gc only collects when this is 0
    call_depth: Int,
    // zones loaded through hooks.load_time_zone, by proper id
    tz_zones: Dict(String, time_zone.Zone),
  )
}

pub type AsyncWaiter {
  AsyncWaiter(
    owner: SabOwner,
    ref: WaiterRef,
    promise: Handle,
    deadline: Option(Int),
  )
}

pub type FrameInfo {
  FrameInfo(name: String, script: String, line: Int)
}

pub type HostTerm

pub type HostFnEntry {
  HostFnEntry(
    call: fn(Agent, List(JsVal), JsVal, JsVal) -> #(Result(JsVal, JsVal), Agent),
  )
}
