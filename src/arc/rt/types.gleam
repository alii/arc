//// `rt_types` — LEAF module holding every type reachable from `JsSlot`
//// (SPEC §2.2-§2.5, D8/D16/D17). Imports ONLY `gleam/*` + arc internals so
//// `rt_store`, `rt_gc`, `rt_obj`, `rt_async` can all import it
//// without cycles. arc precedent: `arc/vm/value.gleam` (4884-line leaf).
////
//// Section order (readability; Gleam allows forward type refs in-module):
////   value-ABI → keys/symbols → property/heap → ObjKind/JsSlot → async →
////   realm → JsOps/JsStore → Agent.

import arc/host_hooks.{type ConsoleLevel, type HostHooks}
import arc/rt/bytecode.{
  type EnvTuple, type FrameStep, type FuncTemplate, type SuspendedFrame,
}
import arc/vm/internal/ordered_entries.{type OrderedEntries}
import arc/vm/internal/tree_array.{type TreeArray}
import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}

// ──────────────────────── §2.3/§2.4 VALUE ABI (D16) ────────────────────────
// The opaque `JsVal` + its `classify/mk_*` FFI + the `JsValKind` sum every
// rt_js Gleam function pattern-matches on. SPEC §2.3 is the WIRE encoding;
// Gleam NEVER matches wire terms — `arc_rt_val_ffi` is the ONE
// encode/decode point, so a wire change touches only that .erl file.

/// Opaque JS value — the §2.3 wire term. Gleam NEVER matches on the wire
/// shape; `classify/1` (arc_rt_val_ffi) is the ONE decode point (D16).
pub type JsVal

/// A JS Number's classified numeric shape. `JInt` is exact integer-valued;
/// `JFloat` covers everything else finite; the three non-finites are their
/// own constructors so Gleam pattern-matching cannot forget them.
pub type JsNum {
  JInt(Int)
  JFloat(Float)
  JNan
  JPosInf
  JNegInf
}

/// Heap cell handle. Wire term = `{js_cell, Int}` (R4) — deliberately the SAME
/// tuple as the object wire row, so `mk_object` is identity on the FFI side.
pub type Handle {
  JsCell(id: Int)
}

/// The result of `classify(JsVal)` — exactly one variant per §2.3 wire row.
/// Every rt_js Gleam function pattern-matches on THIS, never the wire term.
/// `KTdz` is the TDZ sentinel (internal — never a JS-visible value; every
/// coercion on it is an engine panic).
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

/// Decode a `JsVal` wire term into `JsValKind` — the ONE decode point (D16).
@external(erlang, "arc_rt_val_ffi", "classify")
pub fn classify(v: JsVal) -> JsValKind

/// The `undefined` value.
@external(erlang, "arc_rt_val_ffi", "mk_undefined")
pub fn mk_undefined() -> JsVal

/// The `null` value.
@external(erlang, "arc_rt_val_ffi", "mk_null")
pub fn mk_null() -> JsVal

/// A JS boolean.
@external(erlang, "arc_rt_val_ffi", "mk_bool")
pub fn mk_bool(b: Bool) -> JsVal

/// A JS number from a `JsNum` (finite → bare integer/float; non-finite → the
/// §2.3 sentinel atom).
@external(erlang, "arc_rt_val_ffi", "mk_number")
pub fn mk_number(n: JsNum) -> JsVal

/// A JS string. Gleam `String` is already the UTF-8 binary wire form (D10).
@external(erlang, "arc_rt_val_ffi", "mk_string")
pub fn mk_string(s: String) -> JsVal

/// A JS bigint from a BEAM arbitrary-precision integer.
@external(erlang, "arc_rt_val_ffi", "mk_bigint")
pub fn mk_bigint(n: Int) -> JsVal

/// A JS symbol. Position 2 of the wire tuple is always the `SymbolId` sum's
/// own wire form — well-known symbols are NOT flattened to a bare atom.
@external(erlang, "arc_rt_val_ffi", "mk_symbol")
pub fn mk_symbol(id: SymbolId) -> JsVal

/// A JS object/function reference. `Handle`'s wire form IS the object wire
/// form (R4), so this is identity on the FFI side.
@external(erlang, "arc_rt_val_ffi", "mk_object")
pub fn mk_object(h: Handle) -> JsVal

/// The TDZ sentinel. Internal — never a JS value; reading it is a
/// `ReferenceError` at the use site.
@external(erlang, "arc_rt_val_ffi", "mk_tdz")
pub fn mk_tdz() -> JsVal

/// §7.1.1 ToPrimitive `preferredType` hint.
pub type ToPrimHint {
  HintDefault
  HintString
  HintNumber
}

/// §7.4.3 GetIterator `kind` — sync vs async iteration protocol.
pub type IterHint {
  IterSync
  IterAsync
}

// ────────────────── §2.4 KEYS + SYMBOLS (types-keys-symbols) ───────────────
// THE single canonicalizer for string → PropertyKey, shared by the emitter
// (bakes canonical keys into GetField/PutField opcodes) and the runtime
// (dynamic `obj[expr]` access, JSON, Object.keys). One implementation is
// load-bearing: if compile-time and runtime canonicalized differently, the
// same property would land in two dict slots. Faithful port of
// arc/vm/key.gleam:19-219 (D9: `Private(BitArray)`) + arc/vm/value.gleam:37-168
// (D14: `UserSymbol` uid is a threaded `Int`, not `ErlangRef`).

/// Largest valid array index (§6.1.7): an array index is an integer in
/// [0, 2^32-1), i.e. at most 2^32 - 2. Anything larger is an ordinary
/// string-named property even when it looks numeric.
pub const max_array_index = 4_294_967_294

/// Largest valid array length (§10.4.2.4 ArraySetLength): 2^32 - 1. Exactly
/// one more than `max_array_index`.
pub const max_array_length = 4_294_967_295

/// Canonical property key. Per spec, property keys are String | Symbol, but
/// we distinguish array-index strings at the type level so `arr[5]` never
/// round-trips through string conversion. Symbols live in `symbol_props`.
pub type PropertyKey {
  /// Canonical array index — a non-negative integer whose ToString form
  /// equals the original key. `"5"` → `Index(5)`; `"05"` stays `Named("05")`.
  Index(n: Int)
  /// Any other string key.
  Named(name: String)
  /// A class private element ("#x"). D9: raw `BitArray`
  /// (`<<"#name", 0, UidText/binary>>`) NOT `String`, so a private key is
  /// unforgeable from user string ops and structurally distinct from
  /// `Named("#x")`. Only the `private_key*` constructors below build one.
  Private(text: BitArray)
}

/// Property key including the symbol namespace — the full key domain of an
/// object's own-property lookup. `SObject` stores string- and symbol-keyed
/// properties in separate dicts, but callers addressing either carry this.
pub type ObjectKey {
  StringKey(PropertyKey)
  SymbolKey(SymbolId)
}

/// Canonicalize a string key. Implements CanonicalNumericIndexString
/// (§7.1.21) plus the array-index range check: if `s` parses to a
/// non-negative int and `int.to_string(n) == s`, it's `Index(n)`; else
/// `Named(s)`. Returns only `Index | Named` — no user text canonicalizes
/// to `Private`.
pub fn canonical_key(s: String) -> PropertyKey {
  // Cheap leading-byte guard: a canonical array index must start with a
  // digit. Without it, every non-numeric key raises+catches badarg in
  // int.parse (binary_to_integer wrapped in try/catch on BEAM).
  case bit_array.from_string(s) {
    <<c, _:bytes>> if c >= 48 && c <= 57 ->
      case int.parse(s) {
        // Array-index range check (§6.1.7). BEAM ints are arbitrary
        // precision; without the cap "1000000000000000000000" would
        // round-trip and wrongly become an Index.
        Ok(n) if n >= 0 && n <= max_array_index ->
          case int.to_string(n) == s {
            True -> Index(n)
            False -> Named(s)
          }
        _ -> Named(s)
      }
    _ -> Named(s)
  }
}

/// Canonical PropertyKey for an integer index — the Int-side sibling of
/// `canonical_key`. Anything outside [0, 2^32-2] is stored under its
/// ToString form as `Named`.
pub fn index_key(n: Int) -> PropertyKey {
  case n >= 0 && n <= max_array_index {
    True -> Index(n)
    False -> Named(int.to_string(n))
  }
}

/// Canonical array index of a *number* key — the Float-side sibling of
/// `canonical_key`. `Some(i)` iff `f` is an integral value in [0, 2^32-2].
/// -0.0 is normalized to +0.0 first so `a[-0]` is `a[0]`.
pub fn array_index_of_float(f: Float) -> Option(Int) {
  let n = f +. 0.0
  let i = float.truncate(n)
  case int.to_float(i) == n && i >= 0 && i <= max_array_index {
    True -> Some(i)
    False -> None
  }
}

/// The exact property-name text of a PropertyKey — the true inverse of
/// `canonical_key`. Use wherever the string is *data*: proxy trap arguments,
/// `Object.keys`, for-in bindings. Private keys never reach ordinary
/// reflection (callers filter with `is_private_key`).
pub fn key_to_text(key: PropertyKey) -> String {
  case key {
    Index(n) -> int.to_string(n)
    Named(s) -> s
    Private(text) ->
      // Storage form is `<<"#name", 0, UidText>>` — always valid UTF-8.
      case bit_array.to_string(text) {
        Ok(s) -> s
        Error(Nil) -> ""
      }
  }
}

/// Render a PropertyKey the way a human should see it (error messages,
/// `inspect`, function names). Mangles private keys down to their source
/// text ("#x"), dropping the per-evaluation uid.
pub fn key_display_string(key: PropertyKey) -> String {
  case key {
    Index(n) -> int.to_string(n)
    Named(name) -> name
    Private(text) -> private_display_name(text)
  }
}

/// Whether a PropertyKey lives in the private-element namespace. Reflection
/// sites call this to skip private keys.
pub fn is_private_key(key: PropertyKey) -> Bool {
  case key {
    Private(_) -> True
    Index(_) | Named(_) -> False
  }
}

/// Build the storage key for a class private element ("#x") with no uid
/// suffix. See `private_key_text` for the minted per-evaluation form.
pub fn private_key(name: String) -> PropertyKey {
  Private(bit_array.from_string(name))
}

/// Wrap minted PrivateName storage bytes back into a key — the only way a
/// runtime-carried private-name binary re-enters the private namespace.
pub fn private_key_from_text(text: BitArray) -> PropertyKey {
  Private(text)
}

/// Storage-key *bytes* for a freshly minted PrivateName (§15.7.14
/// ClassDefinitionEvaluation): `<<Source, 0, UidText>>` (D9). The uid makes
/// each class evaluation's names distinct. See `t_new_private_name`.
pub fn private_key_text(name: String, uid: Int) -> BitArray {
  <<name:utf8, 0, int.to_string(uid):utf8>>
}

/// Source-text name ("#x") of a private storage key — uid suffix stripped.
pub fn private_display_name(key_text: BitArray) -> String {
  case split_at_nul(key_text, <<>>) {
    Some(name) -> name
    None ->
      case bit_array.to_string(key_text) {
        Ok(s) -> s
        Error(Nil) -> ""
      }
  }
}

/// Scan for the first NUL byte and return the UTF-8 prefix before it.
fn split_at_nul(rest: BitArray, acc: BitArray) -> Option(String) {
  case rest {
    <<0, _:bytes>> ->
      case bit_array.to_string(acc) {
        Ok(s) -> Some(s)
        Error(Nil) -> None
      }
    <<b, tail:bytes>> -> split_at_nul(tail, <<acc:bits, b>>)
    _ -> None
  }
}

/// The closed set of well-known symbols (ES2024 §6.1.5.1). Being a sum type,
/// a fabricated well-known symbol is unrepresentable, and adding a member
/// forces every `case` over it to be revisited.
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

/// Symbol identity. Well-known symbols are the closed `WellKnown` sum. User
/// symbols carry a threaded Int uid (D14 — replaces arc's `ErlangRef`) plus
/// their `[[Description]]`. Registered symbols carry ONLY their key: two
/// `Symbol.for("x")` calls produce term-equal `RegisteredSymbol("x")`.
pub type SymbolId {
  WellKnownSymbol(which: WellKnown)
  /// Minted by `Symbol(desc)` — never in the GlobalSymbolRegistry.
  UserSymbol(uid: Int, description: Option(String))
  /// Minted by `Symbol.for(key)`; `[[Description]]` is `key` (§20.4.2.2).
  RegisteredSymbol(key: String)
}

// Well-known symbol constants — one per `WellKnown` member.
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
/// Exhaustive over `WellKnown` — a new member cannot be added without
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

/// Description of a well-known symbol; `None` for user/registered symbols.
pub fn well_known_symbol_description(id: SymbolId) -> Option(String) {
  case id {
    WellKnownSymbol(which) -> Some(well_known_description(which))
    UserSymbol(..) | RegisteredSymbol(..) -> None
  }
}

/// §20.4 [[Description]] of any symbol: canonical name for well-known,
/// optional description for user, or the registry key for `Symbol.for`.
pub fn symbol_description(id: SymbolId) -> Option(String) {
  case id {
    WellKnownSymbol(which) -> Some(well_known_description(which))
    UserSymbol(description:, ..) -> description
    RegisteredSymbol(key:) -> Some(key)
  }
}

/// §9.13 CanBeHeldWeakly / §20.4.2.6 KeyForSymbol: true iff `id` was minted
/// by `Symbol.for`. Pure — no registry consulted.
pub fn is_registered_symbol(id: SymbolId) -> Bool {
  case id {
    RegisteredSymbol(..) -> True
    WellKnownSymbol(_) | UserSymbol(..) -> False
  }
}

/// §20.4.3.3.1 SymbolDescriptiveString — "Symbol(" + description + ")".
pub fn symbol_descriptive_string(id: SymbolId) -> String {
  "Symbol(" <> option.unwrap(symbol_description(id), "") <> ")"
}

// ─────────────────────── §2.4 PROPERTY / HEAP / ObjKind ────────────────────
// Property/ParsedDesc/JsElements/FnFlags/ObjKind/JsSlot — every type
// reachable from `JsSlot` (arc value.gleam:3814-3890 + heap type surface).

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

/// DataView element types whose JS value is a Number: SetViewValue coerces
/// with ToNumber, GetViewValue produces a Number.
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
/// with ToBigInt, GetViewValue produces a BigInt.
pub type ViewBigElement {
  VBigInt64
  VBigUint64
}

/// Element type read/written by DataView.prototype get*/set* methods.
/// The Number/BigInt split lives in the type rather than a comment: it decides
/// which coercion (ToNumber vs ToBigInt) and which encoder each get*/set* uses,
/// so a bigint element cannot reach the number encoder.
pub type ViewElementType {
  VNum(ViewNumElement)
  VBig(ViewBigElement)
}

/// Backing storage of an ArrayBuffer/SharedArrayBuffer — the whole
/// [[ArrayBufferData]] / [[ArrayBufferMaxByteLength]] / IsImmutableBuffer
/// state as ONE sum type, so the four combinations the spec forbids
/// (immutable+shared, immutable+resizable, immutable+detached,
/// shared+detached) cannot be written down at all.
///
/// * `Detached` — [[ArrayBufferData]] is null (§25.1.3.5 DetachArrayBuffer).
///   There is no leftover byte array to read; [[ArrayBufferMaxByteLength]]
///   survives so the `resizable` getter keeps reporting true.
/// * `Bytes` — a plain (non-shared, mutable) ArrayBuffer: an immutable BEAM
///   binary. `max_byte_length: Some(_)` iff resizable.
/// * `Immutable` — the TC39 Immutable ArrayBuffer proposal's
///   IsImmutableBuffer state (transferToImmutable / sliceToImmutable
///   results): never shared, never resizable, never detachable, and every
///   write path (Atomics, TypedArray/DataView stores) rejects it.
/// * `Shared` — a SharedArrayBuffer: bytes held in this agent's store
///   exactly like `Bytes` (current [[ArrayBufferByteLength]] =
///   `byte_size(bytes)`). `max_byte_length: Some(_)` iff growable.
///
/// Shared-ness is not a flag: a buffer is shared iff its storage is
/// `Shared`. Detached-ness is not a flag: a buffer is detached iff its
/// storage is `Detached`.
pub type BufferStorage {
  Detached(max_byte_length: Option(Int))
  Bytes(bytes: BitArray, max_byte_length: Option(Int))
  Immutable(bytes: BitArray)
  /// Multi-agent sharing is not supported in this runtime.
  Shared(bytes: BitArray, max_byte_length: Option(Int))
}

/// Whether the storage is a SharedArrayBuffer backing. This is THE definition
/// of shared-ness — there is no separate flag.
pub fn buffer_is_shared(storage: BufferStorage) -> Bool {
  case storage {
    Shared(..) -> True
    Bytes(..) | Immutable(..) | Detached(..) -> False
  }
}

/// IsDetachedBuffer(O) — [[ArrayBufferData]] is null.
pub fn buffer_is_detached(storage: BufferStorage) -> Bool {
  case storage {
    Detached(..) -> True
    Bytes(..) | Immutable(..) | Shared(..) -> False
  }
}

/// IsImmutableBuffer(O) (immutable-arraybuffer proposal).
pub fn buffer_is_immutable(storage: BufferStorage) -> Bool {
  case storage {
    Immutable(..) -> True
    Bytes(..) | Shared(..) | Detached(..) -> False
  }
}

/// [[ArrayBufferMaxByteLength]], absent for fixed-length buffers. An
/// immutable buffer is fixed-length by construction, so it never has one.
pub fn buffer_max_byte_length(storage: BufferStorage) -> Option(Int) {
  case storage {
    Detached(max_byte_length:)
    | Bytes(max_byte_length:, ..)
    | Shared(max_byte_length:, ..) -> max_byte_length
    Immutable(..) -> None
  }
}

/// [[ArrayBufferByteLength]] of a storage value — +0 for a detached buffer,
/// which is exactly what §25.1.6.2 / §25.1.3.4 want.
pub fn buffer_byte_size(storage: BufferStorage) -> Int {
  case storage {
    Detached(..) -> 0
    Bytes(bytes:, ..) | Immutable(bytes:) | Shared(bytes:, ..) ->
      bit_array.byte_size(bytes)
  }
}

/// The live buffer contents, or None when the buffer is DETACHED — there are
/// no bytes to hand out, and the compiler makes every reader say what it
/// does about that. Zero cost: the (immutable) backing binary itself.
pub fn buffer_bits(storage: BufferStorage) -> Option(BitArray) {
  case storage {
    Detached(..) -> None
    Bytes(bytes:, ..) | Immutable(bytes:) | Shared(bytes:, ..) -> Some(bytes)
  }
}

/// Persist a full-buffer image `new_bits`. `byte_offset`/`count` name the
/// region the caller actually modified (§6.2.9.3 CopyDataBlockBytes writes
/// exactly that range); with in-store shared bytes the whole image is the
/// new storage either way, but the region MUST lie inside `new_bits`: every
/// caller has already validated the write range against the live buffer, so
/// an out-of-range region is an arithmetic bug in the caller — crash rather
/// than silently drop the store.
///
/// `Detached` and `Immutable` storage have nothing to write into: every write
/// path rejects them BEFORE getting here (a detached store is a spec no-op,
/// an immutable store is a TypeError), so the store is dropped rather than
/// forging bytes into a buffer that must not have any. Rebuilding the
/// storage from its own variant is what makes "forgot to preserve
/// max_byte_length on write-back" unwritable.
pub fn buffer_store_region(
  storage: BufferStorage,
  new_bits: BitArray,
  byte_offset: Int,
  count: Int,
) -> BufferStorage {
  case storage {
    Bytes(bytes: _, max_byte_length:) ->
      Bytes(bytes: new_bits, max_byte_length:)
    Shared(bytes: _, max_byte_length:) -> {
      let assert True =
        byte_offset >= 0
        && count >= 0
        && byte_offset + count <= bit_array.byte_size(new_bits)
        as "buffer_store_region: write range outside the new buffer image"
      Shared(bytes: new_bits, max_byte_length:)
    }
    Immutable(..) | Detached(..) -> storage
  }
}

/// Compiled JS function body: BEAM `fun(St, Frame, Args) -> {V, St'}`
/// (D4/D5). Opaque so Gleam cannot call it directly — invocation goes
/// through `t_call_checked` (M-CALL), which owns arity/frame marshalling.
pub type CompiledFn

/// Opaque handle to the vendored regex engine's compiled pattern (§10).
pub type CompiledRegExp

/// A stored own-property in an object's `props` / `symbol_props` dict. `seq`
/// is the creation-order stamp (D14) so ownKeys/for-in enumerate in
/// insertion order without a separate order list.
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

/// Creation-order sequence number of a property (arc `value.gleam:3777`).
pub fn prop_seq(prop: Property) -> Int {
  case prop {
    DataProperty(seq:, ..) | AccessorProperty(seq:, ..) -> seq
  }
}

/// Read `[[Enumerable]]` of either descriptor kind.
pub fn prop_enumerable(prop: Property) -> Bool {
  case prop {
    DataProperty(enumerable: e, ..) | AccessorProperty(enumerable: e, ..) -> e
  }
}

/// Read `[[Configurable]]` of either descriptor kind.
pub fn prop_configurable(prop: Property) -> Bool {
  case prop {
    DataProperty(configurable: c, ..) | AccessorProperty(configurable: c, ..) ->
      c
  }
}

/// Carry an existing property's `seq` onto a replacement descriptor — used by
/// update/redefine paths, which must keep the key's enumeration position
/// (arc `value.gleam:3814`).
pub fn with_seq_of(prop: Property, old: Property) -> Property {
  let seq = prop_seq(old)
  case prop {
    DataProperty(value:, writable:, enumerable:, configurable:, ..) ->
      DataProperty(value:, writable:, enumerable:, configurable:, seq:)
    AccessorProperty(get:, set:, enumerable:, configurable:, ..) ->
      AccessorProperty(get:, set:, enumerable:, configurable:, seq:)
  }
}

/// §6.2.6 Property Descriptor after `ToPropertyDescriptor` — every field
/// is `Option` (absent ≠ undefined). Consumed by `t_define_own_property`.
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

/// An object's array-indexed elements storage. `Dense` is the fast path;
/// falls back to `Sparse` when holes appear or indices go large.
pub type JsElements {
  NoElements
  Dense(TreeArray(JsVal))
  Sparse(Dict(Int, JsVal))
}

/// Function creation-time flags. Fixed at closure creation; never mutated.
pub type FnFlags {
  FnFlags(
    is_constructor: Bool,
    is_class_constructor: Bool,
    is_derived_constructor: Bool,
    is_arrow: Bool,
    is_method: Bool,
    is_generator: Bool,
    is_async: Bool,
    /// §10.2 [[ThisMode]] is strict: OrdinaryCallBindThis passes
    /// thisArgument through uncoerced.
    is_strict: Bool,
  )
}

/// SameValueZero-normalized Map/Set key (arc `value.gleam:967-1027`):
/// -0 → +0, NaN equals NaN, objects by Handle identity.
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

/// Convert a `JsVal` to a `MapKey`. Implements SameValueZero normalization:
/// -0 → +0, NaN → `MKNan`. Panics on the TDZ sentinel — a hole reaching
/// Map/Set is an engine bug (arc `value.gleam:989-1008`).
pub fn js_to_map_key(v: JsVal) -> MapKey {
  case classify(v) {
    KStr(s) -> MKString(s)
    KNum(JNan) -> MKNan
    KNum(JPosInf) -> MKInfinity
    KNum(JNegInf) -> MKNegInfinity
    // Normalize -0 to +0: IEEE 754 -0.0 + 0.0 = +0.0.
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

/// Inverse of `js_to_map_key`. Lossless except -0 → +0 (§24.1.3.9 step 4
/// requires exactly that). Used by Map forEach/entries to reconstruct the
/// original JS key (arc `value.gleam:1014-1027`).
pub fn map_key_to_js(key: MapKey) -> JsVal {
  case key {
    MKString(s) -> mk_string(s)
    MKNumber(f) -> mk_number(JFloat(f))
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

/// Array iterator flavour (ES2024 §23.1.5.1).
pub type ArrayIterKind {
  ArrayIterKeys
  ArrayIterValues
  ArrayIterEntries
}

/// Map iterator flavour (ES2024 §24.1.5.1).
pub type MapIterKind {
  MapIterKeys
  MapIterValues
  MapIterEntries
}

/// Set iterator flavour (ES2024 §24.2.5.1). Sets have no key/value split.
pub type SetIterKind {
  SetIterValues
  SetIterEntries
}

/// R11: how a class member installs on the target (instance vs static ×
/// method vs getter vs setter).
pub type MethodInstallKind {
  MIMethod
  MIGetter
  MISetter
  MIStatic
  MIStaticGetter
  MIStaticSetter
}

/// R10/G20: `KNative` dispatch key. NOT `Int`. Full ~180-variant body is
/// enumerated by M6 (one per built-in native); the type lives here so
/// `ObjKind` can reference it. Variants that CLOSE OVER heap state carry
/// `Handle`/`JsVal` fields (M6.md §2 / arc `value.gleam:2956-3055`) — traced
/// via `native_token_refs` below. M6 appends the full enumeration.
pub type NativeToken {
  NativeUnseeded
  /// §27.2.1.3.2 Promise Resolve Function — `[[Promise]]` + shared
  /// `[[AlreadyResolved]]` box (an `SBox(mk_bool)` cell).
  PromiseResolveFn(promise: Handle, already_resolved: Handle)
  /// §27.2.1.3.1 Promise Reject Function — same closure fields as resolve.
  PromiseRejectFn(promise: Handle, already_resolved: Handle)
  /// Async-function await resumption (§27.7.5.3 steps 3c/5c): re-drives
  /// `gen`'s pinned `resume` (`{Sm,Rs,Loc}`) with `Sent = {mode, args[0]}`.
  AsyncResume(gen: Handle, is_throw: Bool)
  /// Async-generator internal-await resumption (§27.6.3.5 machinery).
  /// `kind` distinguishes body-await vs the two driver-level return awaits.
  AsyncGenResume(gen: Handle, is_throw: Bool, kind: AGResumeKind)
  // ── M6 per-module dispatch wrappers (arc value.gleam:2072-2103) ───────────
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
  /// `get [Symbol.species]` etc — returns `this` unmodified.
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
  /// %ThrowTypeError% (§10.2.4.1) — poison-pill for restricted
  /// `caller`/`arguments` accessors on `Function.prototype`.
  ThrowTypeErrorPoison
}

/// §27.2 Promise built-in dispatch tokens. Handle-carrying variants are the
/// per-element closures the combinators mint (traced via `native_token_refs`).
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
  /// §27.2.1.5.1 GetCapabilitiesExecutor — writes into two `SBox` cells.
  PromiseCapabilityExecutor(resolve_box: Handle, reject_box: Handle)
  /// §27.2.4.1.3 Promise.all Resolve Element (per-index closure).
  PromiseAllResolveElement(
    index: Int,
    remaining: Handle,
    values: Handle,
    already_called: Handle,
    resolve: JsVal,
  )
  /// §27.2.4.2.2/.3 Promise.allSettled element (fulfil vs reject arm).
  PromiseAllSettledElement(
    fulfilled: Bool,
    index: Int,
    remaining: Handle,
    values: Handle,
    already_called: Handle,
    resolve: JsVal,
  )
  /// §27.2.4.3.2 Promise.any Reject Element.
  PromiseAnyRejectElement(
    index: Int,
    remaining: Handle,
    errors: Handle,
    already_called: Handle,
    reject: JsVal,
  )
  /// §27.2.5.3.1/.2 finally wrapper — captures onFinally + species C.
  PromiseFinallyFn(rejecting: Bool, on_finally: JsVal, constructor: JsVal)
  /// §27.2.5.3.1 step 4 value thunk — `() => value`.
  PromiseFinallyValueThunk(value: JsVal)
  /// §27.2.5.3.2 step 4 thrower — `() => { throw reason }`.
  PromiseFinallyThrower(reason: JsVal)
}

/// §27.1 Iteration built-in dispatch tokens. Scoped to the base
/// %IteratorPrototype% / %AsyncIteratorPrototype% + Async-from-Sync wrap.
pub type IteratorNative {
  /// %AsyncFromSyncIteratorPrototype%.next / .return / .throw (§27.1.4.2).
  AsyncFromSyncNext
  AsyncFromSyncReturn
  AsyncFromSyncThrow
  /// AsyncFromSync onFulfilled — `v => ({value: v, done})` (§27.1.4.4).
  AsyncFromSyncUnwrap(done: Bool)
  /// AsyncFromSync onRejected — close inner then rethrow (§27.1.4.4).
  AsyncFromSyncClose(sync_iter: Handle)
  // ── ES2025 Iterator constructor + statics + prototype helpers ─────────────
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
  /// %IteratorHelperPrototype%.next / .return.
  IteratorHelperNext
  IteratorHelperReturn
  /// %WrapForValidIteratorPrototype%.next / .return.
  WrapForValidIteratorNext
  WrapForValidIteratorReturn
  /// get/set %Iterator.prototype%[@@toStringTag] + .constructor.
  IteratorProtoGetToStringTag
  IteratorProtoSetToStringTag
  IteratorProtoGetConstructor
  IteratorProtoSetConstructor
  // ── Per-collection %XIteratorPrototype%.next() ────────────────────────────
  ArrayIteratorNext
  MapIteratorNext
  SetIteratorNext
  StringIteratorNext
}

/// §27.3-§27.7 Generator/AsyncGenerator/AsyncFunction built-in dispatch
/// tokens. `next/return/throw` route to `rt_async.t_gen_*`/`t_asyncgen_*`.
pub type GeneratorNative {
  GeneratorNext
  GeneratorReturn
  GeneratorThrow
  AsyncGeneratorNext
  AsyncGeneratorReturn
  AsyncGeneratorThrow
  /// Dynamic constructors — `GeneratorFunction("a", "yield a")` etc. Throw
  /// TypeError until M19 wires the eval hook; kept for prototype-shape parity.
  GeneratorFunctionCtor
  AsyncGeneratorFunctionCtor
  AsyncFunctionCtor
}

/// Object static + prototype methods (arc `ObjectNativeFn` value.gleam:737-780).
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
  /// Annex B §B.2.2.2-5 legacy accessor management.
  ObjectPrototypeDefineGetter
  ObjectPrototypeDefineSetter
  ObjectPrototypeLookupGetter
  ObjectPrototypeLookupSetter
  /// Annex B §B.2.2.1 `__proto__` accessor.
  ObjectPrototypeProtoGetter
  ObjectPrototypeProtoSetter
}

/// Function methods + %ThrowTypeError% (arc `VmNativeFn`/`CallNativeFn` subset).
pub type FunctionNative {
  /// §20.2.1.1 Function ( ...args, bodyArg ) — the dynamic constructor.
  FunctionConstructor
  /// §20.2.3.1 Function.prototype.apply.
  FunctionApply
  /// §20.2.3.2 Function.prototype.bind.
  FunctionBind
  /// §20.2.3.3 Function.prototype.call.
  FunctionCall
  /// §20.2.3.5 Function.prototype.toString.
  FunctionToString
  /// §20.2.3.6 Function.prototype[@@hasInstance].
  FunctionHasInstance
  /// §20.2.3 Function.prototype is itself a function that returns undefined.
  FunctionPrototypeCall
  /// §10.2.4.1 %ThrowTypeError% — restricted `caller`/`arguments` accessor.
  ThrowTypeErrorFn
}

/// Error natives (arc `ErrorNativeFn` value.gleam:660-688). `proto` is the
/// intrinsic prototype fallback for OrdinaryCreateFromConstructor.
pub type ErrorNative {
  /// §20.5.1.1 / §20.5.6.1.1 Error / NativeError ( message [ , options ] ).
  ErrorConstructor(proto: Handle)
  /// §20.5.7.1.1 AggregateError ( errors, message [ , options ] ).
  AggregateErrorConstructor(proto: Handle)
  /// SuppressedError ( error, suppressed, message ) — Explicit Resource Mgmt.
  SuppressedErrorConstructor(proto: Handle)
  /// §20.5.3.4 Error.prototype.toString.
  ErrorPrototypeToString
  /// V8 extension Error.captureStackTrace(target [, constructorOpt]).
  ErrorCaptureStackTrace
  /// get Error.prototype.stack — error-stack-accessor proposal.
  ErrorStackGetter
  /// set Error.prototype.stack — carries own realm's %Error.prototype%.
  ErrorStackSetter(proto: Handle)
  /// Error.isError ( arg ) — proposal.
  ErrorIsError
}

/// Date natives (arc `DateNativeFn` value.gleam:1034-1111). `proto` is the
/// intrinsic prototype fallback for OrdinaryCreateFromConstructor.
pub type DateNative {
  /// §21.4.2.1 Date ( ...values ).
  DateConstructor(proto: Handle)
  /// §21.4.3.1 Date.now ( ) — reads `st.hooks.wall_clock_ms`.
  DateNow
  /// §21.4.3.2 Date.parse ( string ).
  DateParse
  /// §21.4.3.4 Date.UTC ( year, month, date, hours, minutes, seconds, ms ).
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
  /// Annex B §B.2.3.1/2 getYear/setYear.
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
  /// §21.4.4.45 Date.prototype[@@toPrimitive] ( hint ).
  DatePrototypeSymbolToPrimitive
}

/// Which RegExp flag a per-flag getter reads (arc `RegExpFlag` value.gleam).
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

/// RegExp natives (arc `RegExpNativeFn` value.gleam:1113-1170).
pub type RegExpNative {
  /// §22.2.4.1 RegExp ( pattern, flags ).
  RegExpConstructor
  RegExpPrototypeExec
  RegExpPrototypeTest
  RegExpPrototypeToString
  /// Annex B §B.2.4.1 RegExp.prototype.compile ( pattern, flags ).
  RegExpPrototypeCompile
  RegExpGetSource
  RegExpGetFlags
  RegExpGetFlag(flag: RegExpFlag)
  /// §22.2.6.8-12 RegExp.prototype[@@match/matchAll/replace/search/split].
  RegExpSymbolMatch
  RegExpSymbolMatchAll
  RegExpSymbolReplace
  RegExpSymbolSearch
  RegExpSymbolSplit
  /// %RegExpStringIteratorPrototype%.next.
  RegExpStringIteratorNext
}

/// ArrayBuffer / SharedArrayBuffer methods — ES2024 §25.1/§25.2. One
/// dispatch family covers both: the same internal slot layout
/// (`ArrayBufferObj` exotic kind) backs both, distinguished by the storage
/// kind (`buffer_is_shared(storage)`).
pub type ArrayBufferNative {
  /// §25.1.4.1 ArrayBuffer ( length [ , options ] )
  ArrayBufferConstructor(proto: Handle)
  /// §25.1.5.1 ArrayBuffer.isView ( arg )
  ArrayBufferIsView
  /// §25.1.6.2 get ArrayBuffer.prototype.byteLength
  ArrayBufferGetByteLength
  /// §25.1.6.3 get ArrayBuffer.prototype.detached
  ArrayBufferGetDetached
  /// §25.1.6.4 get ArrayBuffer.prototype.maxByteLength
  ArrayBufferGetMaxByteLength
  /// §25.1.6.5 get ArrayBuffer.prototype.resizable
  ArrayBufferGetResizable
  /// §25.1.6.7 ArrayBuffer.prototype.slice ( start, end )
  ArrayBufferSlice
  /// §25.1.6.6 ArrayBuffer.prototype.resize ( newLength )
  ArrayBufferResize
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
  SharedArrayBufferConstructor(proto: Handle)
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
}

/// TypedArray natives — %TypedArray% intrinsic, the 11 concrete constructors,
/// and %TypedArray%.prototype accessors/methods (arc `TypedArrayNativeFn`).
pub type TypedArrayNative {
  /// %TypedArray% — the abstract intrinsic. Constructing or calling throws.
  TypedArrayIntrinsicConstructor
  /// One of the 11 concrete constructors (Int8Array .. BigUint64Array).
  TypedArrayConstructor(kind: TypedArrayKind, proto: Handle)
  /// §23.2.2.1 %TypedArray%.from ( source [ , mapFn [ , thisArg ] ] ).
  TypedArrayFrom
  /// §23.2.2.2 %TypedArray%.of ( ...items ).
  TypedArrayOf
  TypedArrayGetBuffer
  TypedArrayGetByteLength
  TypedArrayGetByteOffset
  TypedArrayGetLength
  /// §23.2.3.38 get %TypedArray%.prototype[@@toStringTag].
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
}

/// DataView methods — ES2024 §25.3. Constructor, accessor getters, and the
/// get/set methods parametrized by element type (arc `DataViewNativeFn`).
pub type DataViewNative {
  /// §25.3.2.1 DataView ( buffer [ , byteOffset [ , byteLength ] ] )
  DataViewConstructor(proto: Handle)
  /// §25.3.4.1 get DataView.prototype.buffer
  DataViewGetBuffer
  /// §25.3.4.2 get DataView.prototype.byteLength
  DataViewGetByteLength
  /// §25.3.4.3 get DataView.prototype.byteOffset
  DataViewGetByteOffset
  /// DataView.prototype.get<Type> ( byteOffset [ , littleEndian ] )
  DataViewGet(element: ViewElementType)
  /// DataView.prototype.set<Type> ( byteOffset, value [ , littleEndian ] )
  DataViewSet(element: ViewElementType)
}

/// Atomics namespace functions — ES2024 §25.4 (arc `AtomicsNativeFn`).
pub type AtomicsNative {
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
  /// §25.4.16 Atomics.notify ( typedArray, index, count )
  AtomicsNotify
  /// §25.4.11 Atomics.or ( typedArray, index, value )
  AtomicsOr
  /// Atomics.pause ( [ iterationNumber ] ) — microwait proposal.
  AtomicsPause
  /// §25.4.12 Atomics.store ( typedArray, index, value )
  AtomicsStore
  /// §25.4.13 Atomics.sub ( typedArray, index, value )
  AtomicsSub
  /// §25.4.14 Atomics.wait ( typedArray, index, value, timeout )
  AtomicsWait
  /// §25.4.15 Atomics.waitAsync ( typedArray, index, value, timeout )
  AtomicsWaitAsync
  /// §25.4.17 Atomics.xor ( typedArray, index, value )
  AtomicsXor
}

/// Proxy natives (arc `CallNativeFn` proxy subset value.gleam:2956-2970).
pub type ProxyNative {
  /// §28.2.1.1 Proxy ( target, handler ) — new-able only.
  ProxyConstructor
  /// §28.2.2.1 Proxy.revocable ( target, handler ).
  ProxyRevocable
  /// The revocation function returned by `revocable` — closes over the proxy.
  ProxyRevoke(proxy: Handle)
}

/// §21.3 Math namespace natives (arc `MathNativeFn`). No Handle-carrying
/// variants — all pure numeric ops.
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

/// §25.5 JSON namespace natives (arc `JsonNativeFn`). No Handle-carrying
/// variants in 2core (arc per-token `fn_proto` realm marker dropped;
/// 2core is single-realm per SPEC §2.5).
pub type JsonNative {
  JsonParse
  JsonStringify
  JsonRawJson
  JsonIsRawJson
}

/// §28.1 Reflect namespace natives (arc `ReflectNativeFn`). No
/// Handle-carrying variants.
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

/// WHATWG Console natives: one Logger per method, tagged with the level it
/// hands to `HostHooks.print`.
pub type ConsoleNative {
  ConsolePrint(level: ConsoleLevel)
}

/// §19.2 Global function natives — parseInt/parseFloat/isNaN/isFinite plus
/// the §19.2.6 URI codecs and Annex B escape/unescape (arc splits these
/// across `NumberNativeFn` + `arc/vm/exec/call` URI wrappers; 2core unifies
/// them under one `GlobalN` wrapper). No Handle-carrying variants.
pub type GlobalNative {
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

// Element size in bytes (§23.2 Table 69) is NOT here: it lives in
// arc/rt/typed_array_ffi as `elem_size`, derived from the same
// `elem_of_kind` table the read/write codecs use. A second width table here
// is exactly how a kind's width and its codec drift apart.

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

/// §24.1 Map built-in dispatch tokens (arc `MapNativeFn`). `MapConstructor`
/// carries its own intrinsic prototype for the OrdinaryCreateFromConstructor
/// fallback.
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

/// §24.2 Set built-in dispatch tokens (arc `SetNativeFn`). `SetConstructor`
/// carries its own intrinsic prototype for the OrdinaryCreateFromConstructor
/// fallback.
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

/// §24.3/§24.4 WeakMap + WeakSet built-in dispatch tokens (arc
/// `WeakMapNativeFn` + `WeakSetNativeFn`, merged — one `weak.gleam` handles
/// both). Constructors carry their own intrinsic prototype fallback.
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

/// Array natives (arc `ArrayNativeFn` value.gleam:691-735). No Handle-bearing
/// variants — constructor stores no closed-over state.
pub type ArrayNative {
  ArrayConstructor
  ArrayIsArray
  ArrayFrom
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

/// String constructor, statics, and prototype methods (§22.1) — arc
/// `value.gleam:601-656` `StringNativeFn`.
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
  // Annex B §B.2.2 HTML wrapper methods
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
  // Statics
  StringRaw
  StringFromCharCode
  StringFromCodePoint
}

/// Number constructor, statics, prototype methods, plus the four coercing
/// globals (§21.1 / §19.2) — arc `value.gleam:579-598` `NumberNativeFn`.
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

/// Boolean constructor + prototype methods (§20.3) — arc
/// `value.gleam:554-558` `BooleanNativeFn`.
pub type BooleanNative {
  BooleanConstructor
  BooleanPrototypeValueOf
  BooleanPrototypeToString
}

/// Symbol constructor, statics and prototype methods (§20.4) — arc
/// `value.gleam:561-577` `SymbolNativeFn`.
pub type SymbolNative {
  /// Symbol() — callable but NOT new-able (do_construct intercepts and throws).
  SymbolConstructor
  /// Symbol.for(key) — global symbol registry lookup/insert.
  SymbolFor
  /// Symbol.keyFor(sym) — reverse lookup in global symbol registry.
  SymbolKeyFor
  /// §20.4.3.3 Symbol.prototype.toString — SymbolDescriptiveString.
  SymbolToString
  /// §20.4.3.4 Symbol.prototype.valueOf — thisSymbolValue.
  SymbolValueOf
  /// §20.4.3.5 Symbol.prototype[@@toPrimitive] — thisSymbolValue.
  SymbolToPrimitive
  /// §20.4.3.2 get Symbol.prototype.description — [[Description]].
  SymbolDescriptionGetter
}

/// BigInt global function + prototype methods (§21.2) — arc `VmNative`
/// `BigInt*` variants.
pub type BigIntNative {
  BigIntGlobal
  BigIntAsIntN
  BigIntAsUintN
  BigIntPrototypeToString
  BigIntPrototypeToLocaleString
  BigIntPrototypeValueOf
}

/// Which async-generator suspension the settled `AsyncGenResume` await was for
/// (arc `value.gleam:4126`). Delegate variants dropped — `yield*` is lowered
/// entirely inside the sm (SPEC §18.6 / Q6).
pub type AGResumeKind {
  /// Body `await` settled — resume the sm with the awaited value (mode 0/1).
  AGResumeBody
  /// `.return(v)` on a completed gen (§27.6.3.9) — settle the head request.
  AGResumeAwaitingReturn
  /// `.return(v)` at a suspended yield (§27.6.3.10 step 8): the driver's
  /// `Await(resumptionValue)` settled — resume the sm with mode 2 + AWAITED v.
  AGResumeReturnUnwind
}

/// M6.md §7 GC-trace hook: every `Handle` a `NativeToken` closes over.
/// Folded into `refs_in_kind` at `rt_gc.gleam` for `KNative`. Exhaustive on
/// `NativeToken` — adding a Handle-carrying top-level variant is a compile
/// error here. `JsVal`-carrying sub-variants are traced separately via
/// `rt_gc.push_term_refs` on the whole tag.
pub fn native_token_refs(tok: NativeToken) -> List(Handle) {
  case tok {
    NativeUnseeded -> []
    PromiseResolveFn(promise:, already_resolved:)
    | PromiseRejectFn(promise:, already_resolved:) -> [
      promise,
      already_resolved,
    ]
    AsyncResume(gen:, is_throw: _) | AsyncGenResume(gen:, ..) -> [gen]
    ObjectN(_) | FunctionN(_) | ReturnThis -> []
    ErrorN(n) -> error_native_refs(n)
    DateN(n) -> date_native_refs(n)
    RegExpN(_) | AtomicsN(_) -> []
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
    // Primitive-wrapper builtins carry no heap state.
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
    | ThrowTypeErrorPoison -> []
  }
}

/// GC-trace hook for `MapNative` — only the constructor closes over a Handle.
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

/// GC-trace hook for `SetNative` — only the constructor closes over a Handle.
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

/// GC-trace hook for `WeakNative` — only the constructors close over a Handle.
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

/// GC-trace hook for `PromiseNative` — combinator element closures close over
/// counter/array/box `Handle`s + a resolve/reject `JsVal` (traced via
/// `refs_in_term`; only Handles are enumerated here).
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
    | PromiseFinallyFn(..)
    | PromiseFinallyValueThunk(..)
    | PromiseFinallyThrower(..) -> []
  }
}

/// GC-trace hook for `IteratorNative`.
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

/// GC-trace hook for `ErrorNative` — the constructor variants close over their
/// intrinsic prototype handle.
pub fn error_native_refs(n: ErrorNative) -> List(Handle) {
  case n {
    ErrorConstructor(proto:)
    | AggregateErrorConstructor(proto:)
    | SuppressedErrorConstructor(proto:)
    | ErrorStackSetter(proto:) -> [proto]
    ErrorPrototypeToString
    | ErrorCaptureStackTrace
    | ErrorStackGetter
    | ErrorIsError -> []
  }
}

/// GC-trace hook for `DateNative` — only the constructor closes over its
/// intrinsic prototype handle.
pub fn date_native_refs(n: DateNative) -> List(Handle) {
  case n {
    DateConstructor(proto:) -> [proto]
    _ -> []
  }
}

/// GC-trace hook for `ArrayBufferNative` — only the constructor closes over
/// its intrinsic prototype handle.
pub fn array_buffer_native_refs(n: ArrayBufferNative) -> List(Handle) {
  case n {
    ArrayBufferConstructor(proto:) | SharedArrayBufferConstructor(proto:) -> [
      proto,
    ]
    _ -> []
  }
}

/// GC-trace hook for `TypedArrayNative` — only the concrete constructors
/// close over their intrinsic prototype handle.
pub fn typed_array_native_refs(n: TypedArrayNative) -> List(Handle) {
  case n {
    TypedArrayConstructor(proto:, ..) -> [proto]
    _ -> []
  }
}

/// GC-trace hook for `DataViewNative` — only the constructor closes over its
/// intrinsic prototype handle.
pub fn data_view_native_refs(n: DataViewNative) -> List(Handle) {
  case n {
    DataViewConstructor(proto:) -> [proto]
    _ -> []
  }
}

/// GC-trace hook for `ProxyNative` — the revocation closure closes over the
/// proxy handle it revokes.
pub fn proxy_native_refs(n: ProxyNative) -> List(Handle) {
  case n {
    ProxyRevoke(proxy:) -> [proxy]
    ProxyConstructor | ProxyRevocable -> []
  }
}

// ── ES2025 §27.1 Iterator Helper heap payloads (arc value.gleam:1439-1525) ──

/// §7.4.1 Iterator Record — `{[[Iterator]], [[NextMethod]]}`. `[[Done]]` is
/// implicit under D7 (throws diverge; catch sites mark done). Gleam-level
/// value stored inside `ObjKind` payloads; NOT itself a heap cell.
pub type IteratorRecord {
  IteratorRecord(iterator: JsVal, next_method: JsVal)
}

/// Per-kind payload of a classic %IteratorHelper% (map/filter/take/drop/
/// flatMap). arc `value.gleam:1442`.
pub type IteratorHelperKind {
  HelperMap(func: JsVal)
  HelperFilter(func: JsVal)
  HelperTake(remaining: Int)
  HelperDrop(remaining: Int)
  /// `inner` is the currently-open flatMap sub-iterator, if any.
  HelperFlatMap(func: JsVal, inner: Option(IteratorRecord))
}

/// [[Mode]] of an Iterator.zip helper — the `mode` option's parsed value.
pub type ZipMode {
  ZipShortest
  ZipLongest
  ZipStrict
}

/// One column of an Iterator.zip helper — either still open (record + its
/// longest-mode padding value) or already exhausted (only padding survives).
pub type ZipMember {
  ZipOpen(record: IteratorRecord, padding: JsVal)
  ZipExhausted(padding: JsVal)
}

/// A queued (open_method, iterable) pair for Iterator.concat — the iterable's
/// `@@iterator` was validated at concat-call time; Call(open_method, iterable)
/// is what produces the inner IteratorRecord when its turn comes.
pub type ConcatItem {
  ConcatItem(open_method: JsVal, iterable: JsVal)
}

/// The per-flavour body of an %IteratorHelper% — everything about a
/// %IteratorHelperPrototype% object EXCEPT its [[GeneratorState]] (a sibling
/// field on `IteratorHelperObj` so lifecycle writes can never clobber body).
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

/// The exotic-behaviour discriminator on an `SObject` cell — its internal
/// slot record (SPEC §2.4). One variant per ES2024 exotic object family.
pub type ObjKind {
  Ordinary
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
    /// Simple-ABI fast-path variant `(closure, declared_arity, needs_this)` —
    /// a positional-args body that skips Frame/args-list build. `needs_this`
    /// True ⇒ closure is `fun(St,This,P0..Pn-1)`; False ⇒ `fun(St,P0..Pn-1)`.
    simple: Option(#(CompiledFn, Int, Bool)),
  )
  /// An interpreted function: bytecode `template` closed over `env`.
  /// [[Call]]/[[Construct]] go through `JsOps.call_bytecode` /
  /// `construct_bytecode`; everything else treats it exactly like
  /// `KCompiled`.
  KBytecode(
    template: FuncTemplate,
    env: EnvTuple,
    home_object: Option(Handle),
    flags: FnFlags,
    fields_init: Option(Handle),
  )
  KNative(tag: NativeToken, name: String, length: Int, constructible: Bool)
  KBound(target: Handle, bound_this: JsVal, bound_args: List(JsVal))
  ErrorObj(stack: String)
  MapObj(entries: OrderedEntries(MapKey, JsVal))
  SetObj(entries: OrderedEntries(MapKey, JsVal))
  WeakMapObj(entries: Dict(Int, JsVal))
  WeakSetObj(entries: Set(Int))
  DateObj(ms: JsNum)
  RegExpObj(
    source: String,
    flags: String,
    last_index: Int,
    compiled: CompiledRegExp,
  )
  /// ArrayBuffer / SharedArrayBuffer — ES2024 §25.1/§25.2. All of
  /// [[ArrayBufferData]], [[ArrayBufferMaxByteLength]] and IsImmutableBuffer
  /// live in ONE `BufferStorage` sum type: detached-ness, shared-ness and
  /// immutability are variants, not flags, so the four spec-forbidden
  /// combinations cannot be constructed. [[ArrayBufferByteLength]] is
  /// derived (`buffer_byte_size(storage)`).
  ArrayBufferObj(storage: BufferStorage)
  /// Integer-Indexed (TypedArray) exotic object — ES2024 §10.4.5 / §23.2.
  /// [[ViewedArrayBuffer]] is `buffer` (an ArrayBufferObj cell),
  /// [[TypedArrayName]]/[[ContentType]] derive from `elem_kind`,
  /// [[ByteOffset]] is `byte_offset`, [[ArrayLength]] is `length` (elements,
  /// not bytes). `length: None` is [[ArrayLength]] = AUTO — a length-tracking
  /// view over a resizable buffer whose element count follows the buffer's
  /// live byte length (§10.4.5.13 TypedArrayLength).
  TypedArrayObj(
    buffer: Handle,
    elem_kind: TypedArrayKind,
    byte_offset: Int,
    length: Option(Int),
  )
  /// DataView object — ES2024 §25.3. [[ViewedArrayBuffer]] is `buffer`,
  /// [[ByteOffset]] is `byte_offset`. `byte_length: None` means byte-length
  /// auto-tracking (view over a resizable buffer with no explicit length).
  DataViewObj(buffer: Handle, byte_offset: Int, byte_length: Option(Int))
  /// Raw JSON box produced by `JSON.rawJSON(text)` (proposal-json-parse-with-
  /// source). `raw` is the [[IsRawJSON]] internal slot's payload: the exact,
  /// already-validated JSON source text, which `JSON.stringify` emits verbatim
  /// with no re-quoting or escaping. The box itself is a null-prototype, frozen
  /// object whose only own property is the data property `"rawJSON"`.
  RawJsonObj(raw: String)
  /// Module Namespace Exotic Object — ES2024 §10.4.6. `exports` maps each
  /// exported name to the `SBox` cell holding the binding's live value, so
  /// [[Get]] re-reads the cell (and throws ReferenceError on a TDZ binding).
  /// String keys come from `exports` (sorted in [[OwnPropertyKeys]]); the only
  /// symbol key is @@toStringTag = "Module" (in `symbol_props`). The object
  /// has a null prototype, is non-extensible, and is read-only.
  ModuleNamespace(exports: Dict(String, Handle))
  /// Proxy exotic object — ES2024 §10.5. `target`/`handler` are the paired
  /// [[ProxyTarget]]/[[ProxyHandler]] slots; `revoked` is set by
  /// Proxy.revocable's revoke function (§28.2.2.1.1). [[Call]]/[[Construct]]
  /// are answered from `target` (§10.5.14 steps 6-7) and so survive
  /// revocation: `typeof` a revoked function proxy stays "function".
  ProxyObj(target: Handle, handler: Handle, revoked: Bool)
  ForInIterator(remaining: List(String))
  ArrayIterator(target: Handle, index: Int, kind: ArrayIterKind)
  MapIterator(target: Handle, index: Int, kind: MapIterKind)
  SetIterator(target: Handle, index: Int, kind: SetIterKind)
  StringIterator(source: String, index: Int)
  AsyncFromSyncIterator(sync_rec: Handle)
  /// ES2025 §27.1.4 %IteratorHelper% — map/filter/take/drop/flatMap/zip/
  /// concat. `gen_state` is that closure generator's [[GeneratorState]].
  IteratorHelperObj(gen_state: GeneratorState, body: HelperBody)
  /// ES2025 §27.1.5.2 wrapped-iterator object from `Iterator.from`.
  WrapForValidIteratorObj(record: IteratorRecord)
}

/// A heap cell's contents. `SObject` is the common case; the others are
/// non-object cells (boxed captured bindings, promises, generators). M2's
/// `refs_in_cell` matches this WITHOUT a wildcard — adding a variant is a
/// compile error there by design (SPEC §7.M1a invariant).
pub type JsSlot {
  SObject(
    kind: ObjKind,
    proto: Option(Handle),
    props: Dict(PropertyKey, Property),
    symbol_props: List(#(SymbolId, Property)),
    elements: JsElements,
    extensible: Bool,
  )
  SBox(value: JsVal)
  SPromise(state: PromiseState, is_handled: Bool)
  SGenerator(state: GeneratorState, resume: CompiledFn, gen_cell: Handle)
  SAsyncGen(
    state: AsyncGenState,
    resume: CompiledFn,
    queue: #(List(AsyncGenRequest), List(AsyncGenRequest)),
    gen_cell: Handle,
  )
  /// Hidden-class fast object: props are a flat slot array indexed by
  /// `ShapeDesc.offsets`. Devolves to `SObject` on delete/accessor/etc.
  SShapedObject(shape_id: Int, proto: Option(Handle), slots: ShapeSlots)
}

/// Opaque slot storage for `SShapedObject` — a plain Erlang tuple on the
/// wire (arity = ShapeDesc.arity). Read via `element(off+1, slots)` / write
/// via `setelement` — both BIFs, so the inlined per-site prop-IC warm hit is
/// zero `call_ext`. Gleam accesses it only through the `shape_slots_*` FFI.
pub type ShapeSlots

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

/// Hidden-class descriptor for `SShapedObject`. `offsets` maps a prop key
/// (utf8 BitArray) → slot index; `transitions` maps an added key → the
/// successor shape_id.
///
/// The shape table lives on `JsStore.shapes` + `JsStore.next_shape`
/// (`?STORE_SHAPES`/`?STORE_NEXT_SHAPE`): shapes are pure structural metadata
/// (no Handle refs, GC-invisible) threaded with the store so an Agent stays
/// self-contained. A ShapeDesc is immutable once created except for
/// `transitions` gaining an edge.
pub type ShapeDesc {
  ShapeDesc(
    arity: Int,
    offsets: Dict(BitArray, Int),
    transitions: Dict(BitArray, Int),
  )
}

// ───────────────────────────────── §2.4 ASYNC ──────────────────────────────
// Promise / generator / async-generator / job types (SPEC §2.4 lines
// 284-291; ports of arc `value.gleam:3964-4155`).

/// The `[[Handler]]` of a promise reaction (ES2024 §27.2.1.2). The spec's
/// "empty" handler (a `.then()` argument that is not callable) is a distinct
/// case, not a `JsVal` — `undefined` is a legitimate fulfil value.
pub type ReactionHandler {
  /// A callable onFulfilled/onRejected — call it with the settled value.
  Handler(fun: JsVal)
  /// Empty onFulfilled: resolve the derived promise with the value as-is.
  IdentityPassThrough
  /// Empty onRejected: reject the derived promise with the reason as-is.
  ThrowerPassThrough
}

/// A stored reaction waiting for promise settlement. `child_resolve` /
/// `child_reject` are the derived-promise capability's resolve/reject fns.
pub type PromiseReaction {
  PromiseReaction(
    on_fulfill: ReactionHandler,
    on_reject: ReactionHandler,
    child_resolve: JsVal,
    child_reject: JsVal,
  )
}

/// Internal promise state. `PromisePending` carries the reaction list so a
/// settled promise structurally cannot hold stale reactions (SPEC §2.4).
pub type PromiseState {
  PromisePending(reactions: List(PromiseReaction))
  PromiseFulfilled(JsVal)
  PromiseRejected(JsVal)
}

/// A microtask job for the promise job queue.
pub type Job {
  /// Run `handler(arg)`, then resolve/reject the child promise.
  ReactionJob(
    handler: ReactionHandler,
    arg: JsVal,
    resolve: JsVal,
    reject: JsVal,
  )
  /// Call `thenable.then_fn(resolve, reject)` to assimilate a thenable.
  ResolveThenableJob(
    thenable: JsVal,
    then_fn: JsVal,
    resolve: JsVal,
    reject: JsVal,
  )
  /// An embedder/interpreter step interleaved FIFO with promise jobs. Any
  /// handle it closes over is traced through the closure environment.
  HostJob(run: fn(Agent) -> Agent)
}

/// Which method a queued (async-)generator request represents.
pub type GeneratorCompletion {
  GenNext
  GenReturn
  GenThrow
}

/// Generator internal lifecycle state (ES2024 §27.5.3.1).
pub type GeneratorState {
  GenSuspendedStart
  GenSuspendedYield
  GenExecuting
  GenCompleted
}

/// Async-generator internal lifecycle state (ES2024 §27.6.3.1). Unlike sync
/// generators, async gens queue requests and can be awaiting a `.return`.
pub type AsyncGenState {
  AGSuspendedStart
  AGSuspendedYield
  AGExecuting
  AGAwaitingReturn
  AGCompleted
}

/// A pending `.next()`/`.return()`/`.throw()` call on an async generator.
/// Carries the promise capability that settles when the request runs.
pub type AsyncGenRequest {
  AsyncGenRequest(
    completion: GeneratorCompletion,
    value: JsVal,
    resolve: JsVal,
    reject: JsVal,
  )
}

/// Opaque Erlang `:queue.queue(Job)`. Constructed/drained via
/// `arc_job_queue_ffi` only (M8). Non-generic: always holds `Job`s.
pub type JobQueue

/// Empty job queue.
@external(erlang, "arc_job_queue_ffi", "job_queue_new")
pub fn jq_new() -> JobQueue

/// Enqueue at the back. O(1).
@external(erlang, "arc_job_queue_ffi", "job_queue_push")
pub fn jq_push(queue: JobQueue, item: Job) -> JobQueue

/// Dequeue from the front. O(1) amortized. `None` when empty.
@external(erlang, "arc_job_queue_ffi", "job_queue_pop")
pub fn jq_pop(queue: JobQueue) -> Option(#(Job, JobQueue))

/// True when the queue has no items. O(1).
@external(erlang, "arc_job_queue_ffi", "job_queue_is_empty")
pub fn jq_is_empty(queue: JobQueue) -> Bool

/// All queued items in front-to-back order. O(n). For GC's `refs_in_term`.
@external(erlang, "arc_job_queue_ffi", "job_queue_to_list")
pub fn jq_to_list(queue: JobQueue) -> List(Job)

// ───────────────────────────────── §2.5 REALM ──────────────────────────────
// SPEC §2.5; derived from arc `builtins/common.gleam:197-274`. Invariant:
// `init_realm` is deterministic — same handle ids every run.

/// A `(prototype, constructor)` pair for one built-in class.
pub type BuiltinPair {
  BuiltinPair(prototype: Handle, constructor: Handle)
}

/// Realm's typed-array constructor/prototype pairs, indexed by kind.
pub type TypedArrays {
  TypedArrays(by_kind: Dict(TypedArrayKind, BuiltinPair))
}

/// A realm's intrinsics: every built-in prototype/constructor handle. NOT a
/// field on `JsStore` (G18) — `t_store_new` returns a realm-less store; M6's
/// `init_realm` allocates this INTO the store and returns it separately.
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
    // APPENDED after `global_object` so `?REALM_GLOBAL` stays put.
    shared_array_buffer: BuiltinPair,
  )
}

/// A `Realm` with every handle pointing at cell -1. Only used to construct
/// an `Agent` before `init_realm` has run; `init_realm` replaces it.
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
  )
}

// ─────────────────── §2.2 STORE RECORD (types-store-record) ────────────────
// SPEC §2.2 + §2.4 + D17 + G18. `JsStore(st)` LIVES HERE (D17 — moved from
// rt_store so `JsOps` can name it and rt_types stays a leaf); it is
// pub NON-opaque so `rt_store.t_store_new` can construct it and `Agent`
// (below) can field-access it. `Agent` ties the knot: `store: JsStore(Agent)`.

/// Native error constructor selector for `ops.new_error`. Faithful port of
/// arc `builtins/common.gleam:1035` — the intrinsic and the stack-trace
/// header name are paired at the M6 dispatch site so they cannot disagree.
pub type ErrorKind {
  TypeErr
  RangeErr
  ReferenceErr
  SyntaxErr
}

/// D17: rt_val (leaf) needs to call rt_obj.get_prop / rt_call.call
/// for `ToPrimitive`/`OrdinaryToPrimitive`, but importing them is a cycle.
/// Type-parameterized over the threaded state; the concrete instantiation
/// is `JsOps(Agent)`. M6 `init_realm` seeds `store.ops` once with the
/// concrete M4/M-CALL fns; rt_val calls `store.ops.get_prop(st, recv, k)`.
pub type JsOps(st) {
  JsOps(
    /// OrdinaryGet — proto-walk, accessor invocation, primitive auto-box.
    get_prop: fn(st, JsVal, ObjectKey) -> #(JsVal, st),
    /// `t_call_checked(st, callee, this, args)`.
    call: fn(st, JsVal, JsVal, List(JsVal)) -> #(JsVal, st),
    /// §7.1.18 ToObject — primitive → wrapper cell.
    to_object: fn(st, JsVal) -> #(Handle, st),
    /// Allocate a native error of `kind` with `message` and stack.
    new_error: fn(st, ErrorKind, String) -> #(JsVal, st),
    /// Indirect eval / `Function()` / `$262.evalScript`: compile `source`
    /// to bytecode on the shared heap and run it. Interpreter-seeded.
    eval_hook: fn(st, String) -> #(JsVal, st),
    /// [[Call]] of a `KBytecode` cell: `(callee, this, args, new_target)`.
    /// Runs a fresh activation to completion; re-raises a throw.
    call_bytecode: fn(st, Handle, JsVal, List(JsVal), JsVal) -> #(JsVal, st),
    /// [[Construct]] of a `KBytecode` cell: `(callee, args, new_target)`.
    construct_bytecode: fn(st, Handle, List(JsVal), JsVal) -> #(Handle, st),
    /// Resume a suspended interpreter frame with `sent` = `#(mode, value)`
    /// (the coroutine driver's Sent pair).
    resume_frame: fn(st, SuspendedFrame, #(Int, JsVal)) -> #(FrameStep, st),
  )
}

/// The threaded JS heap + counters + upcall table. G18: NO `realm` /
/// `global_object` / `symbol_registry` field — a `JsStore` exists before any
/// realm does; M6's `init_realm` allocates the realm INTO `data` and returns
/// the `Realm` handle-record separately. Generic over the threaded state
/// `st` (D17); the concrete instantiation is `JsStore(Agent)`.
pub type JsStore(st) {
  JsStore(
    // ── cell arena (arc heap.gleam:21-45) ──
    /// Live cells by id.
    data: Dict(Int, JsSlot),
    /// Recycled ids, LIFO.
    free: List(Int),
    /// Next never-used id (starts 0).
    next: Int,
    /// Permanent GC roots: realm intrinsics + captured-binding cells.
    pinned_roots: Set(Int),
    // ── GC trigger (M2) ──
    /// Bumped by `t_cell_new`; reset by `t_collect`.
    alloc_since_gc: Int,
    /// `t_maybe_collect` fires when `alloc_since_gc >= gc_threshold`.
    /// Default 65_536 (arc `interpreter.gleam:5796`).
    gc_threshold: Int,
    /// ++ on `t_call_checked` entry, -- on exit; `t_maybe_collect`
    /// gate — only collects at `call_depth == 0` (D11).
    call_depth: Int,
    // ── threaded counters (D9, D14) ──
    /// Property creation-order stamp (replaces arc_vm_ffi:next_prop_seq).
    prop_seq: Int,
    /// `t_new_private_name` counter (D9).
    private_uid: Int,
    /// `UserSymbol` id counter (replaces arc's `make_ref`).
    symbol_uid: Int,
    // ── cycle-breaking upcalls (D17, G16) ──
    /// fn-record: rt_val→rt_obj upcalls without an import cycle.
    ops: JsOps(st),
    // ── async (M8) ──
    /// Opaque Erlang `:queue` via `arc_job_queue_ffi`.
    microtasks: JobQueue,
    /// Promise cell ids rejected with no handler attached.
    unhandled_rejections: List(Int),
    // ── hidden classes (H) ──
    /// shape_id → descriptor. Shape 0 = the empty shape.
    shapes: Dict(Int, ShapeDesc),
    /// Next never-used shape_id.
    next_shape: Int,
  )
}

// ───────────────────────────── AGENT (threaded state) ───────────────────────

/// The threaded state every `t_*` op takes and returns. Field order is ABI
/// for the hand-written Erlang (`arc_rt_layout.hrl`: store = element 2,
/// realm = element 3); new fields are appended, never inserted.
pub type Agent {
  Agent(
    store: JsStore(Agent),
    realm: Realm,
    /// §13.2.8.4 [[TemplateMap]]: site id -> pinned template array.
    template_objects: Dict(String, Handle),
    /// Active call chain, innermost first, as the interpreter pushes it on
    /// Call and pops it on Return. `Error` construction renders it into
    /// `stack`. Compiled code keeps it empty.
    frames: List(FrameInfo),
    /// Embedder capabilities (clocks, PRNG, console sink, uncaught-report
    /// sink). Not part of the heap: excluded from GC and serialization.
    hooks: HostHooks,
  )
}

/// One `Error.prototype.stack` frame: `at name (script:line)`. `name` is ""
/// for anonymous code (the top-level script body); `line` 0 is unknown.
pub type FrameInfo {
  FrameInfo(name: String, script: String, line: Int)
}
