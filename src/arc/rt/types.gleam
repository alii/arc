import arc/bytecode/key.{type Key}
import arc/host_hooks.{type ConsoleLevel, type HostHooks}
import arc/internal/ordered_entries.{type OrderedEntries}
import arc/internal/temporal_calendar.{type Calendar}
import arc/internal/tree_array.{type TreeArray}
import arc/rt/arena.{type Arena}
import arc/rt/builtins/temporal_tz
import arc/rt/bytecode.{type EnvTuple, type FuncTemplate, type SuspendedFrame}
import arc/rt/intl_data.{
  type BoundGetterService, type ConstructibleService, type IntlData,
  type IntlService,
}
import arc/rt/wire
import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/option.{type Option, None, Some}
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
  JsCell(id: Int)
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

// a class private name, not a js value
@external(erlang, "arc_rt_val_ffi", "mk_private")
pub fn mk_private(k: Key) -> JsVal

@external(erlang, "arc_rt_val_ffi", "private_key_of")
pub fn private_key_of(v: JsVal) -> Key

pub type ToPrimHint {
  HintDefault
  HintString
  HintNumber
}

pub type IterHint {
  IterSync
  IterAsync
}

pub type ObjectKey {
  StringKey(Key)
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

pub fn well_known_symbol_description(id: SymbolId) -> Option(String) {
  case id {
    WellKnownSymbol(which) -> Some(well_known_description(which))
    UserSymbol(..) | RegisteredSymbol(..) -> None
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

pub type ViewBigElement {
  VBigInt64
  VBigUint64
}

pub type ViewElementType {
  VNum(ViewNumElement)
  VBig(ViewBigElement)
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

@external(erlang, "arc_rt_sab_ffi", "byte_length")
fn sab_byte_length(owner: SabOwner) -> Int

@external(erlang, "arc_rt_sab_ffi", "read")
fn sab_read(owner: SabOwner) -> BitArray

@external(erlang, "arc_rt_sab_ffi", "write")
fn sab_write(owner: SabOwner, byte_offset: Int, chunk: BitArray) -> Nil

pub fn buffer_is_shared(storage: BufferStorage) -> Bool {
  case storage {
    Shared(..) -> True
    Bytes(..) | Immutable(..) | Detached(..) -> False
  }
}

pub fn buffer_is_detached(storage: BufferStorage) -> Bool {
  case storage {
    Detached(..) -> True
    Bytes(..) | Immutable(..) | Shared(..) -> False
  }
}

pub fn buffer_is_immutable(storage: BufferStorage) -> Bool {
  case storage {
    Immutable(..) -> True
    Bytes(..) | Shared(..) | Detached(..) -> False
  }
}

pub fn buffer_max_byte_length(storage: BufferStorage) -> Option(Int) {
  case storage {
    Detached(max_byte_length:)
    | Bytes(max_byte_length:, ..)
    | Shared(max_byte_length:, ..) -> max_byte_length
    Immutable(..) -> None
  }
}

pub fn buffer_byte_size(storage: BufferStorage) -> Int {
  case storage {
    Detached(..) -> 0
    Bytes(bytes:, ..)
    | Immutable(bytes:)
    | Shared(block: LocalBlock(bytes:), ..) -> bit_array.byte_size(bytes)
    Shared(block: OwnerBlock(byte_length:, ..), max_byte_length: None) ->
      byte_length
    Shared(block: OwnerBlock(owner:, ..), max_byte_length: Some(_)) ->
      sab_byte_length(owner)
  }
}

pub fn buffer_bits(storage: BufferStorage) -> Option(BitArray) {
  case storage {
    Detached(..) -> None
    Bytes(bytes:, ..)
    | Immutable(bytes:)
    | Shared(block: LocalBlock(bytes:), ..) -> Some(bytes)
    Shared(block: OwnerBlock(owner:, ..), ..) -> Some(sab_read(owner))
  }
}

pub fn buffer_store_region(
  storage: BufferStorage,
  new_bits: BitArray,
  byte_offset: Int,
  count: Int,
) -> BufferStorage {
  case storage {
    Bytes(bytes: _, max_byte_length:) ->
      Bytes(bytes: new_bits, max_byte_length:)
    Shared(block:, max_byte_length:) -> {
      let assert True =
        byte_offset >= 0
        && count >= 0
        && byte_offset + count <= bit_array.byte_size(new_bits)
        as "buffer_store_region: write range outside the new buffer image"
      case block {
        LocalBlock(_) ->
          Shared(block: LocalBlock(bytes: new_bits), max_byte_length:)
        OwnerBlock(owner:, ..) -> {
          let assert Ok(chunk) = bit_array.slice(new_bits, byte_offset, count)
            as "buffer_store_region: region checked above"
          let Nil = sab_write(owner, byte_offset, chunk)
          storage
        }
      }
    }
    Immutable(..) | Detached(..) -> storage
  }
}

pub type CompiledFn

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

pub fn with_seq_of(prop: Property, old: Property) -> Property {
  let seq = prop_seq(old)
  case prop {
    DataProperty(value:, writable:, enumerable:, configurable:, ..) ->
      DataProperty(value:, writable:, enumerable:, configurable:, seq:)
    AccessorProperty(get:, set:, enumerable:, configurable:, ..) ->
      AccessorProperty(get:, set:, enumerable:, configurable:, seq:)
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
    is_method: Bool,
    is_generator: Bool,
    is_async: Bool,
    is_strict: Bool,
  )
}

pub type MapKey {
  MKString(String)
  MKNumber(Float)
  MKNan
  MKInfinity
  MKNegInfinity
  MKBool(Bool)
  MKNull
  MKUndefined
  MKObject(Handle)
  MKSymbol(SymbolId)
  MKBigInt(Int)
}

pub fn js_to_map_key(v: JsVal) -> MapKey {
  case classify(v) {
    KStr(s) -> MKString(s)
    KNum(JNan) -> MKNan
    KNum(JPosInf) -> MKInfinity
    KNum(JNegInf) -> MKNegInfinity
    // +. 0.0 turns -0.0 into +0.0
    KNum(JFloat(f)) -> MKNumber(f +. 0.0)
    KNum(JInt(n)) -> MKNumber(int.to_float(n) +. 0.0)
    KBool(b) -> MKBool(b)
    KNull -> MKNull
    KUndef -> MKUndefined
    KHandle(h) -> MKObject(h)
    KSym(id) -> MKSymbol(id)
    KBig(n) -> MKBigInt(n)
    KTdz -> panic as "js_to_map_key on the TDZ sentinel"
  }
}

pub fn map_key_to_js(key: MapKey) -> JsVal {
  case key {
    MKString(s) -> mk_string(s)
    MKNumber(f) -> mk_number(integral_key_number(f))
    MKNan -> mk_number(JNan)
    MKInfinity -> mk_number(JPosInf)
    MKNegInfinity -> mk_number(JNegInf)
    MKBool(b) -> mk_bool(b)
    MKNull -> mk_null()
    MKUndefined -> mk_undefined()
    MKObject(h) -> mk_object(h)
    MKSymbol(id) -> mk_symbol(id)
    MKBigInt(n) -> mk_bigint(n)
  }
}

fn integral_key_number(f: Float) -> JsNum {
  let n = float.truncate(f)
  let exact =
    int.to_float(n) == f
    && n <= 9_007_199_254_740_991
    && n >= -9_007_199_254_740_991
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
  MIMethod
  MIGetter
  MISetter
  MIStatic
  MIStaticGetter
  MIStaticSetter
}

pub type NativeToken {
  PromiseResolveFn(promise: Handle, already_resolved: Handle)
  PromiseRejectFn(promise: Handle, already_resolved: Handle)
  AsyncGenResume(gen: Handle, is_throw: Bool, kind: AGResumeKind)
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
  ThrowTypeErrorPoison
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
  FinalizationRegistryConstructor(proto: Handle)
  FinalizationRegistryPrototypeRegister
  FinalizationRegistryPrototypeUnregister
}

// target and token are weak (untraced), held is strong
pub type FinRegCell {
  FinRegCell(target: JsVal, held: JsVal, token: Option(JsVal))
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
  DateConstructor(proto: Handle)
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
  RFHasIndices
  RFGlobal
  RFIgnoreCase
  RFMultiline
  RFDotAll
  RFUnicode
  RFUnicodeSets
  RFSticky
}

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
  // per-realm regexp caches ride on the constructor: the last
  // RegExp.prototype props seen pristine and compiled patterns
  RegExpConstructor(
    legacy: LegacyStatics,
    proto_props: Option(Dict(Key, Property)),
    compiled: Dict(String, CompiledRegExp),
  )
  RegExpLegacyGetter(ctor: Handle, slot: LegacySlot)
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
  ArrayBufferConstructor(proto: Handle)
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
  SharedArrayBufferConstructor(proto: Handle)
  SharedArrayBufferGetByteLength
  SharedArrayBufferGrow
  SharedArrayBufferGetGrowable
  SharedArrayBufferGetMaxByteLength
  SharedArrayBufferSlice
}

pub type TypedArrayNative {
  TypedArrayIntrinsicConstructor
  TypedArrayConstructor(kind: TypedArrayKind, proto: Handle)
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
  DataViewConstructor(proto: Handle)
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
  MapConstructor(proto: Handle)
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
  SetConstructor(proto: Handle)
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
  WeakMapConstructor(proto: Handle)
  WeakMapGet
  WeakMapSet
  WeakMapHas
  WeakMapDelete
  WeakMapGetOrInsert
  WeakMapGetOrInsertComputed
  WeakSetConstructor(proto: Handle)
  WeakSetAdd
  WeakSetHas
  WeakSetDelete
}

pub type FromAsyncCtx {
  FromAsyncCtx(
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

pub type FromAsyncLikeCtx {
  FromAsyncLikeCtx(
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
  ArrayFromAsyncOnNext(ctx: FromAsyncCtx)
  ArrayFromAsyncOnMapped(ctx: FromAsyncCtx)
  ArrayFromAsyncCloseReject(iter: JsVal, reject: JsVal)
  ArrayFromAsyncRejectWith(error: JsVal, reject: JsVal)
  ArrayFromAsyncLikeOnValue(ctx: FromAsyncLikeCtx)
  ArrayFromAsyncLikeOnMapped(ctx: FromAsyncLikeCtx)
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
  IntlSupportedLocalesOf(service: IntlService)
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
  TemporalPlainMonthDayStatic(name: TemporalStaticName, protos: TemporalProtos)
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
  ZgTimeZoneId
  ZgEpochMilliseconds
  ZgEpochNanoseconds
  ZgOffsetNanoseconds
  ZgOffset
  ZgHoursInDay
  ZgDate(TemporalDateGetter)
  ZgTime(TemporalTimeGetter)
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

pub type TemporalStaticName {
  TsFrom
  TsCompare
}

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

pub type TimeZone {
  TzUtc
  TzOffset(ns: Int)
  TzNamed(zone: temporal_tz.Zone)
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

pub type TemporalData {
  TemporalInstant(epoch_ns: Int)
  TemporalDate(year: Int, month: Int, day: Int, calendar: Calendar)
  TemporalTime(
    hour: Int,
    minute: Int,
    second: Int,
    millisecond: Int,
    microsecond: Int,
    nanosecond: Int,
  )
  TemporalDateTime(
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
  TemporalYearMonth(year: Int, month: Int, day: Int, calendar: Calendar)
  TemporalMonthDay(month: Int, day: Int, ref_year: Int, calendar: Calendar)
  TemporalDuration(
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
  TemporalZonedDateTime(epoch_ns: Int, time_zone: TimeZone, calendar: Calendar)
}

pub type AGResumeKind {
  AGResumeAwaitingReturn
  AGResumeReturnUnwind
}

// gc: every handle a token closes over
pub fn native_token_refs(tok: NativeToken) -> List(Handle) {
  case tok {
    PromiseResolveFn(promise:, already_resolved:)
    | PromiseRejectFn(promise:, already_resolved:) -> [
      promise,
      already_resolved,
    ]
    AsyncGenResume(gen:, ..) -> [gen]
    ObjectN(_) | FunctionN(_) | ReturnThis -> []
    ErrorN(n) -> error_native_refs(n)
    DomExceptionN(DomExceptionConstructor(proto:)) -> [proto]
    DomExceptionN(DomExceptionGetCode) -> []
    IntlN(n) -> intl_native_refs(n)
    TemporalN(n) -> temporal_native_refs(n)
    DisposableStackN(n) -> disposable_stack_native_refs(n)
    FinalizationRegistryN(FinalizationRegistryConstructor(proto:)) -> [proto]
    FinalizationRegistryN(FinalizationRegistryPrototypeRegister)
    | FinalizationRegistryN(FinalizationRegistryPrototypeUnregister) -> []
    WeakRefN(_) -> []
    ShadowRealmN(n) -> shadow_realm_native_refs(n)
    DateN(n) -> date_native_refs(n)
    RegExpN(n) -> regexp_native_refs(n)
    AtomicsN(_) -> []
    ArrayBufferN(n) -> array_buffer_native_refs(n)
    TypedArrayN(n) -> typed_array_native_refs(n)
    DataViewN(n) -> data_view_native_refs(n)
    ProxyN(n) -> proxy_native_refs(n)
    PromiseN(n) -> promise_native_refs(n)
    IteratorN(n) -> iterator_native_refs(n)
    GeneratorN(_) -> []
    MapN(n) -> map_native_refs(n)
    SetN(n) -> set_native_refs(n)
    WeakN(n) -> weak_native_refs(n)
    ArrayN(_)
    | StringN(_)
    | NumberN(_)
    | BooleanN(_)
    | SymbolN(_)
    | BigIntN(_)
    | MathN(_)
    | JsonN(_)
    | ReflectN(_)
    | ConsoleN(_)
    | GlobalN(_)
    | ThrowTypeErrorPoison
    | HostFn(_)
    | Test262N(_) -> []
  }
}

pub fn shadow_realm_native_refs(n: ShadowRealmNative) -> List(Handle) {
  case n {
    ShadowRealmConstructor(proto:) -> [proto]
    WrappedFunctionCall(target:, ..) -> [target]
    ShadowRealmEvaluate(_) | ShadowRealmImportValue(_) -> []
  }
}

pub fn map_native_refs(n: MapNative) -> List(Handle) {
  case n {
    MapConstructor(proto:) -> [proto]
    MapGroupBy
    | MapGet
    | MapSet
    | MapHas
    | MapDelete
    | MapClear
    | MapForEach
    | MapGetOrInsert
    | MapGetOrInsertComputed
    | MapGetSize
    | MapKeys
    | MapValues
    | MapEntries -> []
  }
}

pub fn set_native_refs(n: SetNative) -> List(Handle) {
  case n {
    SetConstructor(proto:) -> [proto]
    SetAdd
    | SetHas
    | SetDelete
    | SetClear
    | SetForEach
    | SetGetSize
    | SetValues
    | SetEntries
    | SetUnion
    | SetIntersection
    | SetDifference
    | SetSymmetricDifference
    | SetIsSubsetOf
    | SetIsSupersetOf
    | SetIsDisjointFrom -> []
  }
}

pub fn weak_native_refs(n: WeakNative) -> List(Handle) {
  case n {
    WeakMapConstructor(proto:) | WeakSetConstructor(proto:) -> [proto]
    WeakMapGet
    | WeakMapSet
    | WeakMapHas
    | WeakMapDelete
    | WeakMapGetOrInsert
    | WeakMapGetOrInsertComputed
    | WeakSetAdd
    | WeakSetHas
    | WeakSetDelete -> []
  }
}

pub fn promise_native_refs(n: PromiseNative) -> List(Handle) {
  case n {
    PromiseCapabilityExecutor(resolve_box:, reject_box:) -> [
      resolve_box,
      reject_box,
    ]
    PromiseAllResolveElement(remaining:, values:, already_called:, ..) -> [
      remaining,
      values,
      already_called,
    ]
    PromiseAllSettledElement(remaining:, values:, already_called:, ..) -> [
      remaining,
      values,
      already_called,
    ]
    PromiseAnyRejectElement(remaining:, errors:, already_called:, ..) -> [
      remaining,
      errors,
      already_called,
    ]
    PromiseKeyedElement(remaining:, keys:, values:, already_called:, ..) -> [
      remaining,
      keys,
      values,
      already_called,
    ]
    PromiseConstructor
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
    | PromiseFinallyFn(..)
    | PromiseFinallyValueThunk(..)
    | PromiseFinallyThrower(..) -> []
  }
}

pub fn iterator_native_refs(n: IteratorNative) -> List(Handle) {
  case n {
    AsyncFromSyncClose(sync_iter:) -> [sync_iter]
    AsyncFromSyncNext
    | AsyncFromSyncReturn
    | AsyncFromSyncThrow
    | AsyncFromSyncUnwrap(..)
    | IteratorConstructor
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
    | IteratorProtoSetConstructor
    | ArrayIteratorNext
    | MapIteratorNext
    | SetIteratorNext
    | StringIteratorNext -> []
  }
}

pub fn error_native_refs(n: ErrorNative) -> List(Handle) {
  case n {
    ErrorConstructor(proto:)
    | AggregateErrorConstructor(proto:)
    | SuppressedErrorConstructor(proto:) -> [proto]
    ErrorPrototypeToString
    | ErrorCaptureStackTrace
    | ErrorStackGetter
    | ErrorStackSetter(_)
    | ErrorIsError -> []
  }
}

pub fn disposable_stack_native_refs(n: DisposableStackNative) -> List(Handle) {
  case n {
    DisposableStackConstructor(proto:)
    | DisposableStackPrototypeMove(proto:)
    | AsyncDisposableStackConstructor(proto:)
    | AsyncDisposableStackPrototypeMove(proto:) -> [proto]
    DisposableStackPrototypeDispose
    | DisposableStackPrototypeUse
    | DisposableStackPrototypeAdopt
    | DisposableStackPrototypeDefer
    | DisposableStackDisposedGetter
    | AsyncDisposableStackPrototypeDisposeAsync
    | AsyncDisposableStackPrototypeUse
    | AsyncDisposableStackPrototypeAdopt
    | AsyncDisposableStackPrototypeDefer
    | AsyncDisposableStackDisposedGetter
    | AsyncDisposeContinue(..) -> []
  }
}

pub fn regexp_native_refs(n: RegExpNative) -> List(Handle) {
  case n {
    RegExpLegacyGetter(ctor:, ..) | RegExpLegacyInputSetter(ctor:) -> [ctor]
    _ -> []
  }
}

pub fn date_native_refs(n: DateNative) -> List(Handle) {
  case n {
    DateConstructor(proto:) -> [proto]
    _ -> []
  }
}

pub fn intl_native_refs(n: IntlNative) -> List(Handle) {
  case n {
    IntlConstructor(proto:, ..) -> [proto]
    IntlBoundMethod(target:, ..) -> [target]
    IntlSegmenterSegment(segments_proto:) -> [segments_proto]
    IntlSegmentsIterator(iter_proto:) -> [iter_proto]
    IntlLocaleMethod(proto:, ..) -> [proto]
    IntlGetCanonicalLocales
    | IntlSupportedValuesOf
    | IntlSupportedLocalesOf(_)
    | IntlResolvedOptions(_)
    | IntlBoundGetter(_)
    | IntlMethod(..)
    | IntlHostOverride(_)
    | IntlLocaleGetter(_) -> []
  }
}

pub fn temporal_native_refs(n: TemporalNative) -> List(Handle) {
  case n {
    TemporalInstantCtor(protos:)
    | TemporalInstantStatic(protos:, ..)
    | TemporalInstantMethod(protos:, ..)
    | TemporalNowFn(protos:, ..) -> temporal_protos_refs(protos)
    TemporalInstantGetter(_)
    | TemporalPlainDateTimeGetter(_)
    | TemporalPlainTimeGetter(_) -> []
    TemporalPlainDateTimeCtor(protos:)
    | TemporalPlainDateTimeStatic(protos:, ..)
    | TemporalPlainDateTimeMethod(protos:, ..)
    | TemporalPlainTimeCtor(protos:)
    | TemporalPlainTimeStatic(protos:, ..)
    | TemporalPlainTimeMethod(protos:, ..) -> temporal_protos_refs(protos)
    TemporalDurationGetter(_) -> []
    TemporalDurationCtor(protos:)
    | TemporalDurationStatic(protos:, ..)
    | TemporalDurationMethod(protos:, ..) -> temporal_protos_refs(protos)
    TemporalPlainDateGetter(_) -> []
    TemporalPlainDateCtor(protos:)
    | TemporalPlainDateStatic(protos:, ..)
    | TemporalPlainDateMethod(protos:, ..) -> temporal_protos_refs(protos)
    TemporalPlainYearMonthGetter(_) | TemporalPlainMonthDayGetter(_) -> []
    TemporalPlainYearMonthCtor(protos:)
    | TemporalPlainYearMonthStatic(protos:, ..)
    | TemporalPlainYearMonthMethod(protos:, ..)
    | TemporalPlainMonthDayCtor(protos:)
    | TemporalPlainMonthDayStatic(protos:, ..)
    | TemporalPlainMonthDayMethod(protos:, ..) -> temporal_protos_refs(protos)
    TemporalZonedDateTimeGetter(_) -> []
    TemporalZonedDateTimeCtor(protos:)
    | TemporalZonedDateTimeStatic(protos:, ..)
    | TemporalZonedDateTimeMethod(protos:, ..) -> temporal_protos_refs(protos)
  }
}

pub fn temporal_protos_refs(p: TemporalProtos) -> List(Handle) {
  [
    p.plain_date,
    p.plain_time,
    p.plain_date_time,
    p.plain_year_month,
    p.plain_month_day,
    p.duration,
    p.instant,
    p.zoned_date_time,
  ]
}

pub fn array_buffer_native_refs(n: ArrayBufferNative) -> List(Handle) {
  case n {
    ArrayBufferConstructor(proto:) | SharedArrayBufferConstructor(proto:) -> [
      proto,
    ]
    _ -> []
  }
}

pub fn typed_array_native_refs(n: TypedArrayNative) -> List(Handle) {
  case n {
    TypedArrayConstructor(proto:, ..) -> [proto]
    _ -> []
  }
}

pub fn data_view_native_refs(n: DataViewNative) -> List(Handle) {
  case n {
    DataViewConstructor(proto:) -> [proto]
    _ -> []
  }
}

pub fn proxy_native_refs(n: ProxyNative) -> List(Handle) {
  case n {
    ProxyRevoke(proxy:) -> [proxy]
    ProxyConstructor | ProxyRevocable -> []
  }
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
  KCompiled(
    code: CompiledFn,
    home_object: Option(Handle),
    flags: FnFlags,
    fields_init: Option(Handle),
    simple: Option(#(CompiledFn, Int, Bool)),
    name: String,
    length: Int,
    birth: FnBirth,
  )
  KBytecode(
    template: FuncTemplate(Key),
    env: EnvTuple,
    home_object: Option(Handle),
    flags: FnFlags,
    fields_init: Option(Handle),
    realm: Int,
    unit: Int,
    birth: FnBirth,
  )
  KNative(tag: NativeToken, name: String, length: Int, constructible: Bool)
  KBound(target: Handle, bound_this: JsVal, bound_args: List(JsVal))
  KHost(payload: HostTerm)
  ErrorObj(stack: String)
  MapObj(entries: OrderedEntries(MapKey, JsVal))
  SetObj(entries: OrderedEntries(MapKey, JsVal))
  WeakMapObj(entries: Dict(WeakKey, JsVal))
  WeakSetObj(entries: Set(WeakKey))
  DateObj(ms: JsNum)
  RegExpObj(
    source: String,
    flags: String,
    last_index: Int,
    compiled: CompiledRegExp,
  )
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
  ForInIterator(remaining: List(String))
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
  FinalizationRegistryObj(callback: JsVal, cells: List(FinRegCell))
  // weak: gc does not trace target
  WeakRefObj(target: Option(JsVal))
  ShadowRealmObj(realm: Int)
}

pub type JsSlot {
  SObject(
    kind: ObjKind,
    proto: Option(Handle),
    props: Dict(Key, Property),
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
    queue: #(List(AsyncGenRequest), List(AsyncGenRequest)),
  )
  SAsyncContext(resume: Resume, promise: Handle)
  SDisposeCapability(resources: List(DisposeResource))
  // offsets mirrors the shape so reads skip the table
  SShapedObject(
    shape_id: Int,
    proto: Option(Handle),
    slots: ShapeSlots,
    offsets: Dict(Key, Int),
  )
}

pub type ShapeSlots

@external(erlang, "arc_rt_obj_ffi", "shape_slots_new")
pub fn shape_slots_new() -> ShapeSlots

@external(erlang, "arc_rt_obj_ffi", "shape_slots_get")
pub fn shape_slots_get(slots: ShapeSlots, off: Int) -> JsVal

@external(erlang, "arc_rt_obj_ffi", "shape_slots_set")
pub fn shape_slots_set(slots: ShapeSlots, off: Int, v: JsVal) -> ShapeSlots

@external(erlang, "arc_rt_obj_ffi", "shape_slots_append")
pub fn shape_slots_append(slots: ShapeSlots, v: JsVal) -> ShapeSlots

@external(erlang, "arc_rt_obj_ffi", "shape_slots_fold")
pub fn shape_slots_fold(
  slots: ShapeSlots,
  acc: a,
  f: fn(Int, JsVal, a) -> a,
) -> a

pub type ShapeDesc {
  ShapeDesc(arity: Int, offsets: Dict(Key, Int), transitions: Dict(Key, Int))
}

// droppable cache, not a gc root; see arc_rt_call_fast_ffi
pub type IcEntry {
  IcRead(key: Key, offsets: Dict(Int, Int))
  IcCall(
    key: Key,
    ways: Dict(IcCallMatch, IcCallWay),
    shaped: Dict(Int, Dict(Int, IcCallWay)),
  )
  IcInit(from: Int, to: Int, blank: JsSlot, chain: List(#(Int, JsSlot)))
  IcGlobal(key: Key, epoch: Int, value: JsVal, refills: Int)
  IcOff
}

pub type IcCallWay {
  IcCallWay(chain: List(#(Int, JsSlot)), callee: Handle, kind: ObjKind)
}

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
  AGSuspendedStart
  AGSuspendedYield
  AGExecuting
  AGAwaitingReturn
  AGCompleted
}

pub type AsyncGenRequest {
  AsyncGenRequest(
    completion: GeneratorCompletion,
    value: JsVal,
    resolve: JsVal,
    reject: JsVal,
  )
}

pub type SmFn

pub type Loc

pub type Resume {
  ResumeCompiled(sm: SmFn, rs: Int, loc: Loc)
  ResumeFrame(frame: SuspendedFrame)
}

pub type Step {
  StepReturn(JsVal)
  StepThrow(JsVal)
  StepYield(value: JsVal, resume: Resume)
  StepAwait(value: JsVal, resume: Resume)
}

pub type JobQueue

@external(erlang, "arc_job_queue_ffi", "job_queue_new")
pub fn jq_new() -> JobQueue

@external(erlang, "arc_job_queue_ffi", "job_queue_push")
pub fn jq_push(queue: JobQueue, item: Job) -> JobQueue

@external(erlang, "arc_job_queue_ffi", "job_queue_pop")
pub fn jq_pop(queue: JobQueue) -> Option(#(Job, JobQueue))

@external(erlang, "arc_job_queue_ffi", "job_queue_is_empty")
pub fn jq_is_empty(queue: JobQueue) -> Bool

@external(erlang, "arc_job_queue_ffi", "job_queue_to_list")
pub fn jq_to_list(queue: JobQueue) -> List(Job)

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
    lexical_globals: Dict(Key, LexicalGlobal),
    suppressed_error: BuiltinPair,
  )
}

pub fn unset_realm() -> Realm {
  let h = JsCell(-1)
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

pub type ErrorKind {
  TypeErr
  RangeErr
  ReferenceErr
  SyntaxErr
}

pub type EvalKind {
  IndirectEval
  DynamicFunction
  ScriptEval
}

pub type JsOps(st) {
  JsOps(
    get_prop: fn(st, JsVal, ObjectKey) -> #(JsVal, st),
    call: fn(st, JsVal, JsVal, List(JsVal)) -> #(JsVal, st),
    to_object: fn(st, JsVal) -> #(Handle, st),
    new_error: fn(st, ErrorKind, String) -> #(JsVal, st),
    eval_hook: fn(st, String, EvalKind) -> #(JsVal, st),
    call_bytecode: fn(st, Handle, ObjKind, JsVal, List(JsVal)) ->
      #(Result(JsVal, JsVal), st),
    bind_call: fn(st, Handle, ObjKind, JsVal) ->
      fn(st, List(JsVal)) -> #(JsVal, st),
    construct_bytecode: fn(st, Handle, List(JsVal), JsVal) -> #(Handle, st),
    resume_frame: fn(st, SuspendedFrame, #(Int, JsVal)) -> #(Step, st),
  )
}

pub type JsStore(st) {
  JsStore(
    data: Arena(JsSlot),
    next: Int,
    pinned_roots: Set(Int),
    alloc_since_gc: Int,
    gc_threshold: Int,
    gc_live: Int,
    prop_seq: Int,
    private_uid: Int,
    symbol_uid: Int,
    ops: JsOps(st),
    microtasks: JobQueue,
    unhandled_rejections: List(Int),
    shapes: Dict(Int, ShapeDesc),
    next_shape: Int,
    unit_uid: Int,
    ics: Dict(Int, IcEntry),
    // proto id to whether its chain takes plain named writes
    free_protos: Dict(Int, Nil),
    // bumped on any write to a global object cell, for global read caches
    global_epoch: Int,
    names: NameTable,
  )
}

// the heap's numbering of property names, see arc/rt/store
pub type NameTable {
  NameTable(
    numbers: Dict(String, Int),
    // text of dynamic name keys and private keys
    texts: Dict(Key, String),
    next: Int,
    // keys held where the gc cannot look, kept for good
    pinned: Dict(Key, Nil),
    // texts size after the last name sweep
    swept: Int,
  )
}

// field order is abi (arc_rt_layout.hrl), append only
pub type Agent {
  Agent(
    store: JsStore(Agent),
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
  )
}

pub type AsyncWaiter {
  AsyncWaiter(
    owner: SabOwner,
    ref: WaiterRef,
    promise: Handle,
    resolve: JsVal,
    reject: JsVal,
    deadline: Option(Int),
  )
}

pub type FrameInfo {
  FrameInfo(name: String, script: String, line: Int)
}

pub type HostTerm

pub type HostFnEntry {
  HostFnEntry(
    name: String,
    call: fn(Agent, List(JsVal), JsVal, JsVal) -> #(Agent, Result(JsVal, JsVal)),
  )
}
