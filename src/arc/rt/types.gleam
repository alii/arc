//// `rt_types` — LEAF module holding every type reachable from `JsSlot`
//// (SPEC §2.2-§2.5, D8/D16/D17). Imports ONLY `gleam/*` + arc internals so
//// `rt_store`, `rt_gc`, `rt_obj`, `rt_async` can all import it
//// without cycles. arc precedent: `arc/vm/value.gleam` (4884-line leaf).
////
//// Section order (readability; Gleam allows forward type refs in-module):
////   value-ABI → keys/symbols → property/heap → ObjKind/JsSlot → async →
////   realm → JsOps/JsStore → Agent.

import arc/host_hooks.{type ConsoleLevel, type HostHooks}
import arc/internal/ordered_entries.{type OrderedEntries}
import arc/internal/temporal_calendar.{type Calendar}
import arc/internal/tree_array.{type TreeArray}
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

// ──────────────────────── §2.3/§2.4 VALUE ABI (D16) ────────────────────────
// The opaque `JsVal` + its `classify/mk_*` FFI + the `JsValKind` sum every
// rt_js Gleam function pattern-matches on. SPEC §2.3 is the WIRE encoding;
// Gleam NEVER matches wire terms — `arc_rt_val_ffi` is the ONE
// encode/decode point, so a wire change touches only that .erl file.

/// Opaque JS value — the §2.3 wire term. Gleam NEVER matches on the wire
/// shape; `classify/1` (arc_rt_val_ffi) is the ONE decode point (D16).
/// Declared in the `wire` leaf so `bytecode` can name it too.
pub type JsVal =
  wire.JsVal

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

/// The dense element store's marker for an absent index (a hole). Typed as a
/// `JsVal` only so it can sit in a `TreeArray(JsVal)` as the default; it is
/// not a JS value (`classify` rejects it) and element readers surface it as
/// `None`. An array-literal elision arrives from compiled code as this term.
@external(erlang, "arc_rt_val_ffi", "mk_hole")
pub fn mk_hole() -> JsVal

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
// arc/bytecode/key.gleam:19-219 (D9: `Private(BitArray)`) + arc/vm/value.gleam:37-168
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

/// Identity of a value that passed §9.13 CanBeHeldWeakly: an object's cell id
/// or a non-registered symbol. Keys `WeakMapObj`/`WeakSetObj` entries.
pub type WeakKey {
  WeakObjKey(id: Int)
  WeakSymKey(id: SymbolId)
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
/// * `Shared` — a SharedArrayBuffer's Shared Data Block (§6.2.9), wherever
///   it lives (`SharedBlock`). `max_byte_length: Some(_)` iff growable.
///
/// Shared-ness is not a flag: a buffer is shared iff its storage is
/// `Shared`. Detached-ness is not a flag: a buffer is detached iff its
/// storage is `Detached`.
pub type BufferStorage {
  Detached(max_byte_length: Option(Int))
  Bytes(bytes: BitArray, max_byte_length: Option(Int))
  Immutable(bytes: BitArray)
  Shared(block: SharedBlock, max_byte_length: Option(Int))
}

/// Where a SharedArrayBuffer's bytes (and its WaiterList) live.
///
/// * `LocalBlock` — no other agent has ever been handed this buffer, so its
///   bytes sit in this agent's store exactly like `Bytes` and every access
///   is the same pure binary read/rebuild. A block only this agent can see
///   has no waiters either: registering one promotes it first.
/// * `OwnerBlock` — the block has been shared (broadcast to another agent,
///   or waited on): an owner PROCESS (`arc_rt_sab_ffi`) holds the bytes and
///   the WaiterList, every agent holding the buffer holds this same pid, and
///   each read / write / read-modify-write / wait / notify is a synchronous
///   message to it. `byte_length` is the length at hand-off: authoritative
///   for a fixed-length buffer, a lower bound for a growable one (whose live
///   length is the owner's, `buffer_byte_size`).
pub type SharedBlock {
  LocalBlock(bytes: BitArray)
  OwnerBlock(owner: SabOwner, byte_length: Int)
}

/// Pid of a shared block's owner process (`arc_rt_sab_ffi:spawn_owner/1`).
/// An ordinary Erlang pid: it travels between processes inside the
/// `BufferStorage` term and aliases the same block wherever it lands.
pub type SabOwner

/// Identity of one waiter registered with an owner (an Erlang reference,
/// `arc_rt_sab_ffi:make_waiter_ref/0`). The owner's wake message names it.
pub type WaiterRef

@external(erlang, "arc_rt_sab_ffi", "byte_length")
fn sab_byte_length(owner: SabOwner) -> Int

@external(erlang, "arc_rt_sab_ffi", "read")
fn sab_read(owner: SabOwner) -> BitArray

@external(erlang, "arc_rt_sab_ffi", "write")
fn sab_write(owner: SabOwner, byte_offset: Int, chunk: BitArray) -> Nil

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
/// which is exactly what §25.1.6.2 / §25.1.3.4 want. A growable owner-held
/// block is asked for its live length (§25.2.2.5 step 4: the length of a
/// growable SAB is read from the shared block); a fixed-length one never
/// changes, so the hand-off length stands.
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

/// The live buffer contents, or None when the buffer is DETACHED — there are
/// no bytes to hand out, and the compiler makes every reader say what it
/// does about that. Zero cost for in-store bytes (the backing binary
/// itself); an owner-held shared block answers with a snapshot of its bytes
/// (one message round trip), which is a valid unordered read of every byte.
pub fn buffer_bits(storage: BufferStorage) -> Option(BitArray) {
  case storage {
    Detached(..) -> None
    Bytes(bytes:, ..)
    | Immutable(bytes:)
    | Shared(block: LocalBlock(bytes:), ..) -> Some(bytes)
    Shared(block: OwnerBlock(owner:, ..), ..) -> Some(sab_read(owner))
  }
}

/// Persist a full-buffer image `new_bits`. `byte_offset`/`count` name the
/// region the caller actually modified (§6.2.9.3 CopyDataBlockBytes writes
/// exactly that range). With in-store bytes the whole image is the new
/// storage; an owner-held shared block is sent ONLY that region, so bytes
/// another agent wrote elsewhere since this agent's snapshot are not
/// clobbered. Either way the region MUST lie inside `new_bits`: every caller
/// has already validated the write range against the live buffer, so an
/// out-of-range region is an arithmetic bug in the caller — crash rather
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
  /// Async-function await resumption (§27.7.5.3 steps 3c/5c): continues the
  /// `SAsyncContext` at `gen` with `Sent = {mode, args[0]}`.
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
  /// An embedder native: `id` keys `Agent.host_fns`. The cell carries no
  /// closure, so it serializes; the embedder re-registers in the same order
  /// after `deserialize` and the ids line up again.
  HostFn(id: Int)
  /// The test262 host-defined `$262` methods (INTERPRETING.md).
  Test262N(Test262Native)
  DomExceptionN(DomExceptionNative)
  IntlN(IntlNative)
  TemporalN(TemporalNative)
  DisposableStackN(DisposableStackNative)
  FinalizationRegistryN(FinalizationRegistryNative)
  ShadowRealmN(ShadowRealmNative)
}

/// §26.2 FinalizationRegistry natives. The constructor closes over the
/// intrinsic %FinalizationRegistry.prototype% fallback.
pub type FinalizationRegistryNative {
  FinalizationRegistryConstructor(proto: Handle)
  FinalizationRegistryPrototypeRegister
  FinalizationRegistryPrototypeUnregister
}

/// One [[Cells]] record of a FinalizationRegistry (§26.2.1.1):
/// [[WeakRefTarget]], [[HeldValue]], [[UnregisterToken]] (`None` = empty).
/// `target` and `token` passed CanBeHeldWeakly and are held WEAKLY (GC does
/// not trace them; a cell whose target died is pruned post-sweep); `held` is
/// held strongly.
pub type FinRegCell {
  FinRegCell(target: JsVal, held: JsVal, token: Option(JsVal))
}

/// DisposableStack / AsyncDisposableStack natives (Explicit Resource
/// Management §12.3 / §12.4). Constructors and `move` close over the
/// intrinsic prototype; `AsyncDisposeContinue` is the promise reaction that
/// resumes the async DisposeResources loop after an Await settles.
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
  /// Reaction handler continuing AsyncDisposableStack.prototype.disposeAsync
  /// after an awaited disposer result settles. `pending` is the throw
  /// completion accumulated so far (SuppressedError chain).
  AsyncDisposeContinue(
    remaining: List(DisposeResource),
    pending: Option(JsVal),
    resolve: JsVal,
    reject: JsVal,
    is_reject: Bool,
  )
}

/// A DisposableResource record on a (Async)DisposableStack's
/// [[DisposableResourceStack]]. The variant fixes how Dispose invokes it.
pub type DisposeResource {
  /// use(value): Call(method, value). `method` undefined only for the async
  /// null/undefined resource, which is `NullDispose` instead.
  MethodDispose(value: JsVal, method: JsVal)
  /// adopt(value, onDispose) / defer(onDispose): Call(callback, undefined, args).
  DisposeCallback(callback: JsVal, args: List(JsVal))
  /// async use(value) whose @@asyncDispose was missing: GetDisposeMethod step
  /// 1.b.ii wrapper — Call(method, value), discard the result, Await(undefined).
  AsyncFallbackDispose(value: JsVal, method: JsVal)
  /// async use(null/undefined): no method, only forces one Await(undefined).
  NullDispose
}

/// [[DisposableState]] / [[AsyncDisposableState]] together with the
/// [[DisposeCapability]]. The capability is its own `SDisposeCapability` cell
/// so it keeps its identity when `move()` hands it to a new stack. `Disposed`
/// drops the reference: a disposed stack's capability is never disposed
/// again, so nothing appended to it is observable.
pub type DisposableState {
  Pending(capability: Handle)
  Disposed
}

/// WebIDL §2.8.1 DOMException natives. `proto` is the intrinsic prototype
/// fallback for OrdinaryCreateFromConstructor.
pub type DomExceptionNative {
  /// new DOMException ( message, name ).
  DomExceptionConstructor(proto: Handle)
  /// get DOMException.prototype.code — the legacy code for `this.name`.
  DomExceptionGetCode
}

/// `$262` methods. `realm` is the id of the realm whose `$262` object the
/// function sits on: `evalScript` runs its source there and `createRealm`
/// copies that `$262`'s `agent` onto the child it builds.
pub type Test262Native {
  /// `$262.evalScript(source)` — §16.1.6 ScriptEvaluation in `realm`.
  Test262EvalScript(realm: Int)
  /// `$262.createRealm()` — a fresh realm; returns its `$262`.
  Test262CreateRealm(realm: Int)
  /// `$262.gc()` — a hint; collection only happens at safepoints.
  Test262Gc
}

/// proposal-shadowrealm natives. `realm` is the id of the realm whose
/// %ShadowRealm.prototype% the method sits on: the spec's callerRealm, whose
/// intrinsics brand every error and wrapper the method produces.
pub type ShadowRealmNative {
  /// ShadowRealm ( ) — closes over the intrinsic prototype fallback.
  ShadowRealmConstructor(proto: Handle)
  /// ShadowRealm.prototype.evaluate ( sourceText )
  ShadowRealmEvaluate(realm: Int)
  /// ShadowRealm.prototype.importValue ( specifier, exportName )
  ShadowRealmImportValue(realm: Int)
  /// [[Call]] of a wrapped function exotic object (§2.1): `target` is
  /// [[WrappedTargetFunction]], `caller_realm` is [[Realm]] and
  /// `target_realm` the realm `target` came from, entered to run it.
  WrappedFunctionCall(target: Handle, caller_realm: Int, target_realm: Int)
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
  /// Await-dictionary proposal: Promise.allKeyed / Promise.allSettledKeyed.
  PromiseAllKeyedStatic
  PromiseAllSettledKeyedStatic
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
  /// PerformPromiseAllKeyed fulfilledSteps / rejectedSteps closure. `keys`
  /// and `values` are the shared array cells; `kind` selects raw value
  /// (~all~) or the {status, value/reason} wrap (~all-settled~).
  PromiseKeyedElement(
    kind: PromiseKeyedKind,
    index: Int,
    remaining: Handle,
    keys: Handle,
    values: Handle,
    already_called: Handle,
    resolve: JsVal,
  )
  /// §27.2.5.3.1/.2 finally wrapper — captures onFinally + species C.
  PromiseFinallyFn(rejecting: Bool, on_finally: JsVal, constructor: JsVal)
  /// §27.2.5.3.1 step 4 value thunk — `() => value`.
  PromiseFinallyValueThunk(value: JsVal)
  /// §27.2.5.3.2 step 4 thrower — `() => { throw reason }`.
  PromiseFinallyThrower(reason: JsVal)
}

/// What a `PromiseKeyedElement` stores at its index.
pub type PromiseKeyedKind {
  /// Promise.allKeyed onFulfilled — the value as-is.
  KeyedValue
  /// Promise.allSettledKeyed onFulfilled — `{status: "fulfilled", value}`.
  KeyedFulfilled
  /// Promise.allSettledKeyed onRejected — `{status: "rejected", reason}`.
  KeyedRejected
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
  /// Dynamic constructors — `GeneratorFunction("a", "yield a")` etc. Each
  /// carries the id of the realm it belongs to: CreateDynamicFunction runs,
  /// and the closure it makes lives, in that realm (§10.3.1 step 6-7).
  GeneratorFunctionCtor(realm: Int)
  AsyncGeneratorFunctionCtor(realm: Int)
  AsyncFunctionCtor(realm: Int)
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
  /// §20.2.1.1 Function ( ...args, bodyArg ) — the dynamic constructor,
  /// attributed to its realm like the generator-family constructors.
  FunctionConstructor(realm: Int)
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
  /// set Error.prototype.stack — `realm` is the setter's own realm: its
  /// %Error.prototype% is the `home` object and its %TypeError% brands the
  /// step-2 throw, whichever realm calls it.
  ErrorStackSetter(realm: Int)
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

/// One of %RegExp%'s legacy internal slots (tc39
/// proposal-regexp-legacy-features): [[RegExpInput]], [[RegExpLastMatch]],
/// [[RegExpLastParen]], [[RegExpLeftContext]], [[RegExpRightContext]], and
/// [[RegExpParenN]] for N in 1..9. Names the field of `LegacyStatics` a
/// `RegExpLegacyGetter` reads. Every paren index is a distinct constructor,
/// so a mis-numbered slot (`$0`, `$10`) is not representable and reading a
/// slot is total.
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
/// once. InitializeLegacyRegExpStaticProperties sets every one to "" (hence
/// `empty_legacy_statics()`) and UpdateLegacyRegExpStaticProperties rewrites
/// every one on each successful builtin exec, so there is no "unset" slot to
/// distinguish from an empty one.
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

/// InitializeLegacyRegExpStaticProperties: every slot the empty String.
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

/// Read one legacy slot. Total: every `LegacySlot` names a real field.
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

/// RegExp natives (arc `RegExpNativeFn` value.gleam:1113-1170).
pub type RegExpNative {
  /// §22.2.4.1 RegExp ( pattern, flags ). `legacy` holds the tc39
  /// legacy-regexp proposal's internal slots ([[RegExpInput]],
  /// [[RegExpLastMatch]], [[RegExpParen1]], ...) as one typed record. Living
  /// inside the constructor's `KNative` kind keeps the state per-realm (each
  /// realm has its own %RegExp% object) and invisible to
  /// OrdinaryOwnPropertyKeys.
  RegExpConstructor(legacy: LegacyStatics)
  /// GetLegacyRegExpStaticProperty for `slot`; `ctor` is the %RegExp% the
  /// accessor was installed on (the SameValue(C, thisValue) operand).
  RegExpLegacyGetter(ctor: Handle, slot: LegacySlot)
  /// SetLegacyRegExpStaticProperty for [[RegExpInput]] (`input` / `$_`).
  RegExpLegacyInputSetter(ctor: Handle)
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
  /// test262 `$262.detachArrayBuffer(buffer)` — §25.1.3.5 DetachArrayBuffer.
  ArrayBufferDetach262
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
  /// proposal-arraybuffer-base64: Uint8Array.prototype.toBase64 ( [ options ] )
  Uint8ArrayPrototypeToBase64
  /// proposal-arraybuffer-base64: Uint8Array.prototype.toHex ( )
  Uint8ArrayPrototypeToHex
  /// proposal-arraybuffer-base64:
  /// Uint8Array.prototype.setFromBase64 ( string [ , options ] )
  Uint8ArrayPrototypeSetFromBase64
  /// proposal-arraybuffer-base64: Uint8Array.prototype.setFromHex ( string )
  Uint8ArrayPrototypeSetFromHex
  /// proposal-arraybuffer-base64: Uint8Array.fromBase64 ( string [ , options ] )
  Uint8ArrayFromBase64
  /// proposal-arraybuffer-base64: Uint8Array.fromHex ( string )
  Uint8ArrayFromHex
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

/// §25.5 JSON namespace natives (arc `JsonNativeFn`). Every variant carries
/// `realm`: the id of the realm the function object was created in (its
/// [[Realm]]), whose intrinsics brand the errors it throws and the objects
/// it allocates however it was reached (`otherRealm.JSON.parse('{')` throws
/// `otherRealm.SyntaxError`).
pub type JsonNative {
  JsonParse(realm: Int)
  JsonStringify(realm: Int)
  JsonRawJson(realm: Int)
  JsonIsRawJson(realm: Int)
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

/// §19.2 Global function natives — eval/parseInt/parseFloat/isNaN/isFinite
/// plus the §19.2.6 URI codecs and Annex B escape/unescape (arc splits these
/// across `NumberNativeFn` + `arc/vm/exec/call` URI wrappers; 2core unifies
/// them under one `GlobalN` wrapper). No Handle-carrying variants.
pub type GlobalNative {
  /// §19.2.1 eval(x) reached through [[Call]] — always an INDIRECT eval
  /// (`JsOps.eval_hook`), run in `realm`: the id of the realm this %eval%
  /// belongs to (§19.2.1.1 evalRealm). Direct eval is recognised by the
  /// interpreter at the call site (CallEval opcode) as the CURRENT realm's
  /// %eval% (§13.3.6.1 step 6.a) and never dispatches here.
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

/// Array.fromAsync loop state (iterator path), threaded through the native
/// promise-reaction continuation closures (§23.1.2.1 step 3.j).
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

/// Array.fromAsync loop state (array-like path, §23.1.2.1 step 3.k).
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

/// Array natives (arc `ArrayNativeFn` value.gleam:691-735). No Handle-bearing
/// variants — constructor stores no closed-over state; the `fromAsync`
/// continuations close over `JsVal`s only (traced via the whole-tag walk).
pub type ArrayNative {
  ArrayConstructor
  ArrayIsArray
  ArrayFrom
  ArrayFromAsync
  /// Array.fromAsync await continuations (§23.1.2.1).
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

/// ECMA-402 natives. `proto` on a constructor is the intrinsic prototype
/// fallback for OrdinaryCreateFromConstructor.
pub type IntlNative {
  /// Intl.getCanonicalLocales(locales)
  IntlGetCanonicalLocales
  /// Intl.supportedValuesOf(key)
  IntlSupportedValuesOf
  /// new Intl.<Service>(locales, options)
  IntlConstructor(service: ConstructibleService, proto: Handle)
  /// Intl.<Service>.supportedLocalesOf(locales, options)
  IntlSupportedLocalesOf(service: IntlService)
  /// Intl.<Service>.prototype.resolvedOptions()
  IntlResolvedOptions(service: IntlService)
  /// Accessor getter for NumberFormat/DateTimeFormat .format and Collator
  /// .compare: returns (and caches on the receiver) a bound method.
  IntlBoundGetter(service: BoundGetterService)
  /// The bound method produced by `IntlBoundGetter`; `target` is the instance.
  IntlBoundMethod(service: BoundGetterService, target: Handle)
  /// Named prototype method (format/formatToParts/select/of/...). The
  /// receiver's brand (`service`) plus `method` pick the implementation.
  IntlMethod(service: IntlService, method: IntlMethodName)
  /// ECMA-402 §17-19 locale-sensitive overrides installed on the Number /
  /// BigInt / String / Date prototypes: not Intl.* methods, no Intl brand
  /// check.
  IntlHostOverride(which: IntlHostOverrideName)
  /// Segmenter.prototype.segment — needs the %SegmentsPrototype% handle.
  IntlSegmenterSegment(segments_proto: Handle)
  /// %SegmentsPrototype%[Symbol.iterator] — needs %SegmentIteratorPrototype%.
  IntlSegmentsIterator(iter_proto: Handle)
  /// Intl.Locale.prototype getter (language/script/region/baseName/...).
  IntlLocaleGetter(name: LocaleGetterName)
  /// Intl.Locale.prototype method needing the Locale prototype to allocate
  /// result Locale objects (maximize/minimize) or plain (toString).
  IntlLocaleMethod(method: LocaleMethodName, proto: Handle)
}

/// The Intl.<Service>.prototype methods registered via `IntlMethod`: one
/// variant per method name so a registration typo is a compile error.
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

/// The ECMA-402 host overrides (§17-19) installed on the Number / BigInt /
/// String / Date prototypes at Intl init.
pub type IntlHostOverrideName {
  /// Number.prototype.toLocaleString (§18.2.1)
  NumberToLocaleString
  /// BigInt.prototype.toLocaleString (§18.3.1)
  BigIntToLocaleString
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

/// Temporal natives (proposal-temporal §8 Temporal.Instant). `proto` is the
/// intrinsic %Temporal.Instant.prototype% the operation allocates results
/// with (and the OrdinaryCreateFromConstructor fallback for the constructor).
pub type TemporalNative {
  /// new Temporal.Instant(epochNanoseconds)
  TemporalInstantCtor(protos: TemporalProtos)
  /// Temporal.Instant.from / fromEpochMilliseconds / fromEpochNanoseconds /
  /// compare
  TemporalInstantStatic(name: InstantStaticName, protos: TemporalProtos)
  /// get Temporal.Instant.prototype.epochMilliseconds / epochNanoseconds
  TemporalInstantGetter(getter: InstantGetterName)
  /// Temporal.Instant.prototype methods
  TemporalInstantMethod(method: InstantMethodName, protos: TemporalProtos)
  /// Temporal.Now.* functions
  TemporalNowFn(name: TemporalNowName, protos: TemporalProtos)
  /// new Temporal.PlainDateTime(y, mo, d, h, mi, s, ms, us, ns [, calendar])
  TemporalPlainDateTimeCtor(protos: TemporalProtos)
  /// Temporal.PlainDateTime.from / compare
  TemporalPlainDateTimeStatic(name: TemporalStaticName, protos: TemporalProtos)
  /// get Temporal.PlainDateTime.prototype.<field>
  TemporalPlainDateTimeGetter(getter: TemporalDateTimeGetter)
  /// Temporal.PlainDateTime.prototype methods
  TemporalPlainDateTimeMethod(
    method: PlainDateTimeMethod,
    protos: TemporalProtos,
  )
  /// new Temporal.PlainTime(h, mi, s, ms, us, ns)
  TemporalPlainTimeCtor(protos: TemporalProtos)
  /// Temporal.PlainTime.from / compare
  TemporalPlainTimeStatic(name: TemporalStaticName, protos: TemporalProtos)
  /// get Temporal.PlainTime.prototype.<field>
  TemporalPlainTimeGetter(getter: TemporalTimeGetter)
  /// Temporal.PlainTime.prototype methods
  TemporalPlainTimeMethod(method: PlainTimeMethod, protos: TemporalProtos)
  /// new Temporal.Duration(y, mo, w, d, h, mi, s, ms, us, ns)
  TemporalDurationCtor(protos: TemporalProtos)
  /// Temporal.Duration.from / compare
  TemporalDurationStatic(name: TemporalStaticName, protos: TemporalProtos)
  /// get Temporal.Duration.prototype.years ... blank
  TemporalDurationGetter(getter: TemporalDurationGetter)
  /// Temporal.Duration.prototype methods
  TemporalDurationMethod(method: DurationMethod, protos: TemporalProtos)
  /// new Temporal.PlainDate(year, month, day [, calendar])
  TemporalPlainDateCtor(protos: TemporalProtos)
  /// Temporal.PlainDate.from / compare
  TemporalPlainDateStatic(name: TemporalStaticName, protos: TemporalProtos)
  /// get Temporal.PlainDate.prototype.<field>
  TemporalPlainDateGetter(getter: TemporalDateGetter)
  /// Temporal.PlainDate.prototype methods
  TemporalPlainDateMethod(method: PlainDateMethod, protos: TemporalProtos)
  /// new Temporal.PlainYearMonth(year, month [, calendar [, referenceISODay]])
  TemporalPlainYearMonthCtor(protos: TemporalProtos)
  /// Temporal.PlainYearMonth.from / compare
  TemporalPlainYearMonthStatic(name: TemporalStaticName, protos: TemporalProtos)
  /// get Temporal.PlainYearMonth.prototype.<field>
  TemporalPlainYearMonthGetter(getter: TemporalYearMonthGetter)
  /// Temporal.PlainYearMonth.prototype methods
  TemporalPlainYearMonthMethod(
    method: PlainYearMonthMethod,
    protos: TemporalProtos,
  )
  /// new Temporal.PlainMonthDay(month, day [, calendar [, referenceISOYear]])
  TemporalPlainMonthDayCtor(protos: TemporalProtos)
  /// Temporal.PlainMonthDay.from (PlainMonthDay has no compare)
  TemporalPlainMonthDayStatic(name: TemporalStaticName, protos: TemporalProtos)
  /// get Temporal.PlainMonthDay.prototype.<field>
  TemporalPlainMonthDayGetter(getter: TemporalMonthDayGetter)
  /// Temporal.PlainMonthDay.prototype methods
  TemporalPlainMonthDayMethod(
    method: PlainMonthDayMethod,
    protos: TemporalProtos,
  )
  /// new Temporal.ZonedDateTime(epochNanoseconds, timeZone [, calendar])
  TemporalZonedDateTimeCtor(protos: TemporalProtos)
  /// Temporal.ZonedDateTime.from / compare
  TemporalZonedDateTimeStatic(name: TemporalStaticName, protos: TemporalProtos)
  /// get Temporal.ZonedDateTime.prototype.<field>
  TemporalZonedDateTimeGetter(getter: TemporalZonedGetter)
  /// Temporal.ZonedDateTime.prototype methods
  TemporalZonedDateTimeMethod(
    method: ZonedDateTimeMethod,
    protos: TemporalProtos,
  )
}

/// ZonedDateTime getters: its own exact-time/zone fields plus the shared
/// date and time field sets.
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

/// Temporal.PlainDate.prototype methods.
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

/// Handles of all eight Temporal type prototypes, captured inside each
/// Temporal native token at init time so methods can allocate instances of
/// sibling types (e.g. PlainDateTime.prototype.toPlainDate needs the
/// PlainDate prototype). All are rooted at builtin init.
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

/// Static methods shared by the Temporal type constructors.
pub type TemporalStaticName {
  TsFrom
  TsCompare
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

/// A Temporal.ZonedDateTime's [[TimeZone]], resolved once at construction.
/// There is no "unknown" variant: an unrecognised identifier is a RangeError
/// at parse time.
pub type TimeZone {
  /// The distinguished "UTC" zone (offset 0, no transitions).
  TzUtc
  /// A fixed numeric offset ("+05:30"), stored in nanoseconds. No transitions.
  TzOffset(ns: Int)
  /// A named IANA zone, validated against the system tzdata.
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

/// Temporal.Now.* functions.
pub type TemporalNowName {
  NowInstant
  NowTimeZoneId
  NowPlainDateISO
  NowPlainTimeISO
  NowPlainDateTimeISO
  NowZonedDateTimeISO
}

/// The internal slots of a Temporal object. The variant IS the brand.
pub type TemporalData {
  /// [[EpochNanoseconds]] of a Temporal.Instant, |ns| <= 8.64e21.
  TemporalInstant(epoch_ns: Int)
  /// Temporal.PlainDate: ISO calendar date plus its calendar. `calendar`
  /// (here and on the other Temporal slots) is the closed CLDR calendar set.
  TemporalDate(year: Int, month: Int, day: Int, calendar: Calendar)
  /// Temporal.PlainTime: wall-clock time, nanosecond precision.
  TemporalTime(
    hour: Int,
    minute: Int,
    second: Int,
    millisecond: Int,
    microsecond: Int,
    nanosecond: Int,
  )
  /// Temporal.PlainDateTime: combined ISO date + wall-clock time.
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
  /// Temporal.PlainYearMonth. `year`/`month`/`day` are the ISO date of the
  /// reference day.
  TemporalYearMonth(year: Int, month: Int, day: Int, calendar: Calendar)
  /// Temporal.PlainMonthDay. `month`/`day`/`ref_year` are the ISO date of the
  /// reference day.
  TemporalMonthDay(month: Int, day: Int, ref_year: Int, calendar: Calendar)
  /// Temporal.Duration: ten integral fields, all the same sign.
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
  /// Temporal.ZonedDateTime: exact time + resolved time zone.
  TemporalZonedDateTime(epoch_ns: Int, time_zone: TimeZone, calendar: Calendar)
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
    DomExceptionN(DomExceptionConstructor(proto:)) -> [proto]
    DomExceptionN(DomExceptionGetCode) -> []
    IntlN(n) -> intl_native_refs(n)
    TemporalN(n) -> temporal_native_refs(n)
    DisposableStackN(n) -> disposable_stack_native_refs(n)
    FinalizationRegistryN(FinalizationRegistryConstructor(proto:)) -> [proto]
    FinalizationRegistryN(FinalizationRegistryPrototypeRegister)
    | FinalizationRegistryN(FinalizationRegistryPrototypeUnregister) -> []
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
    | ThrowTypeErrorPoison
    | HostFn(_)
    | Test262N(_) -> []
  }
}

/// GC-trace hook for `ShadowRealmNative`: the constructor's prototype and a
/// wrapped function's [[WrappedTargetFunction]]. Realm ids name registered
/// realms, whose intrinsics are pinned for the agent's lifetime.
pub fn shadow_realm_native_refs(n: ShadowRealmNative) -> List(Handle) {
  case n {
    ShadowRealmConstructor(proto:) -> [proto]
    WrappedFunctionCall(target:, ..) -> [target]
    ShadowRealmEvaluate(_) | ShadowRealmImportValue(_) -> []
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
    | SuppressedErrorConstructor(proto:) -> [proto]
    ErrorPrototypeToString
    | ErrorCaptureStackTrace
    | ErrorStackGetter
    | ErrorStackSetter(_)
    | ErrorIsError -> []
  }
}

/// GC-trace hook for `DisposableStackNative`: constructors and `move` close
/// over the intrinsic prototype. `AsyncDisposeContinue` carries `JsVal`s only
/// (its resource list, pending error and capability functions), which the
/// whole-tag term walk traces.
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

/// GC-trace hook for `RegExpNative`: the legacy static accessors close over
/// the %RegExp% constructor they were installed on.
pub fn regexp_native_refs(n: RegExpNative) -> List(Handle) {
  case n {
    RegExpLegacyGetter(ctor:, ..) | RegExpLegacyInputSetter(ctor:) -> [ctor]
    _ -> []
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

/// GC-trace hook for `IntlNative`: constructors close over their intrinsic
/// prototype, bound methods over their receiver.
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

/// GC-trace hook for `TemporalNative`: every allocating operation closes
/// over the intrinsic prototype it allocates with.
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

/// Every prototype handle a `TemporalProtos` record closes over.
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
    /// [[Realm]] (§10.2 table 30): the id of the realm the closure was
    /// created in. [[Call]] runs with that realm current.
    realm: Int,
  )
  KNative(tag: NativeToken, name: String, length: Int, constructible: Bool)
  KBound(target: Handle, bound_this: JsVal, bound_args: List(JsVal))
  /// An opaque embedder-owned object (arc HostObject). No own behaviour
  /// beyond Ordinary; `payload` is the embedder's value, type-erased here
  /// and read back typed by `arc/host`.
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
  /// §27.2 Promise instance; [[PromiseState]]/[[PromiseIsHandled]] and the
  /// reactions live in the `SPromiseData` cell at `data`.
  PromiseObj(data: Handle)
  /// §27.5 Generator instance; [[GeneratorState]] and the suspension live in
  /// the `SGenerator` cell at `data`.
  GeneratorObj(data: Handle)
  /// §27.6 AsyncGenerator instance; state, suspension and the request queue
  /// live in the `SAsyncGen` cell at `data`.
  AsyncGeneratorObj(data: Handle)
  AsyncFromSyncIterator(sync_rec: Handle)
  /// ES2025 §27.1.4 %IteratorHelper% — map/filter/take/drop/flatMap/zip/
  /// concat. `gen_state` is that closure generator's [[GeneratorState]].
  IteratorHelperObj(gen_state: GeneratorState, body: HelperBody)
  /// ES2025 §27.1.5.2 wrapped-iterator object from `Iterator.from`.
  WrapForValidIteratorObj(record: IteratorRecord)
  /// An ECMA-402 service instance. `data` is the resolved per-service state
  /// (the brand); `bound` caches the `format` getter's bound function so the
  /// getter is idempotent (§15.3.3).
  IntlObj(data: IntlData, bound: Option(Handle))
  /// A Temporal object; `data` carries its internal slots.
  TemporalObj(data: TemporalData)
  /// DisposableStack (`async: False`) / AsyncDisposableStack (`async: True`)
  /// — Explicit Resource Management §12.3/§12.4. `async` is the brand;
  /// `state` is [[DisposableState]] plus the [[DisposeCapability]] handle.
  DisposableStackObj(async: Bool, state: DisposableState)
  /// §26.2 FinalizationRegistry — [[CleanupCallback]] (held strongly) and
  /// [[Cells]] (newest first; each cell's target/token weak, held strong).
  FinalizationRegistryObj(callback: JsVal, cells: List(FinRegCell))
  /// A ShadowRealm instance (proposal-shadowrealm). `realm` is the
  /// [[ShadowRealm]] slot: the id of the realm the constructor made for it.
  ShadowRealmObj(realm: Int)
}

/// A heap cell's contents. `SObject`/`SShapedObject` are the JS-visible
/// objects; the others are internal data cells an object (or a native
/// closure) points at and are never a JS receiver. `gc.refs_in_cell` matches
/// this WITHOUT a wildcard — adding a variant is a compile error there by
/// design.
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
  /// Behind a `PromiseObj`.
  SPromiseData(state: PromiseState, is_handled: Bool)
  /// Behind a `GeneratorObj`.
  SGenerator(state: GeneratorState, resume: Resume)
  /// Behind an `AsyncGeneratorObj`.
  SAsyncGen(
    state: AsyncGenState,
    resume: Resume,
    queue: #(List(AsyncGenRequest), List(AsyncGenRequest)),
  )
  /// A running async function: where its body resumes after the current
  /// `await`, and the result promise object it settles. Reachable only from
  /// the `AsyncResume` closures of that await.
  SAsyncContext(resume: Resume, promise: Handle)
  /// A pending `DisposableStackObj`'s [[DisposeCapability]]: its
  /// [[DisposableResourceStack]], newest first. `move()` re-points a new
  /// stack at the same cell.
  SDisposeCapability(resources: List(DisposeResource))
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

/// A compiled coroutine body lowered to a state machine:
/// `fun(St, Rs, Sent, Loc) -> {StepWire, St'}` where `Rs` is the resume-state
/// index, `Sent` the injected `#(mode, value)` and `Loc` the suspended locals.
pub type SmFn

/// The suspended-locals tuple of a compiled state machine. Built and read
/// only by the state machine; traced by the GC term walk.
pub type Loc

/// Where a suspended coroutine picks up: a compiled state machine at
/// `(rs, loc)`, or a parked interpreter frame. Built by `arc_rt_async_ffi`
/// (`?RESUME_COMPILED_TAG`) and by the interpreter.
pub type Resume {
  ResumeCompiled(sm: SmFn, rs: Int, loc: Loc)
  ResumeFrame(frame: SuspendedFrame)
}

/// One turn of a coroutine body: it finished, threw, or suspended at a
/// `yield`/`await` with the `Resume` to continue from. Built by
/// `arc_rt_async_ffi` from the state machine's wire step (`?STEP_*` tags)
/// and by `JsOps.resume_frame`.
pub type Step {
  StepReturn(JsVal)
  StepThrow(JsVal)
  StepYield(value: JsVal, resume: Resume)
  StepAwait(value: JsVal, resume: Resume)
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

/// A global `let`/`const`/`class` binding (§9.1.1.4 declarative part of the
/// global Environment Record). Lives on `Realm.lexical_globals`, not on the
/// global object. `mk_tdz()` as the value marks an uninitialized binding.
pub type LexicalGlobal {
  Let(JsVal)
  Const(JsVal)
}

/// The bound value of a lexical global, let or const.
pub fn lexical_global_value(g: LexicalGlobal) -> JsVal {
  case g {
    Let(v) | Const(v) -> v
  }
}

/// Replace the bound value of a lexical global, preserving let/const-ness.
pub fn lexical_global_with_value(g: LexicalGlobal, v: JsVal) -> LexicalGlobal {
  case g {
    Let(_) -> Let(v)
    Const(_) -> Const(v)
  }
}

/// A Realm Record (§9.3): every intrinsic prototype/constructor handle, the
/// global object, the global lexical bindings, and its `id` in
/// `Agent.realms`. NOT a field on `JsStore` (G18) — `t_store_new` returns a
/// realm-less store; `init_realm` allocates this INTO the store.
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
    /// Key of this realm in `Agent.realms`; realm-attributed native tokens
    /// carry it. The bootstrap realm is 0.
    id: Int,
    /// Global `let`/`const`/`class` bindings (§9.1.1.4 [[DeclarativeRecord]]).
    lexical_globals: Dict(String, LexicalGlobal),
    /// %SuppressedError% — DisposeResources folds a second disposal error
    /// into `new SuppressedError(error, suppressed)` of the running realm.
    suppressed_error: BuiltinPair,
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
    id: -1,
    lexical_globals: dict.new(),
    suppressed_error: p,
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

/// What `JsOps.eval_hook` compiles its source text as. All three run in the
/// CURRENT realm's global environment with `this` = the global object and
/// return the completion value (a throw raises).
pub type EvalKind {
  /// §19.2.1.1 PerformEval with direct = false: eval code, so
  /// EvalDeclarationInstantiation makes its `var`/function globals
  /// configurable (§B.3.2.3 / §19.2.1.3 D = true).
  IndirectEval
  /// §20.2.1.1.1 CreateDynamicFunction: the source is one parenthesised
  /// anonymous function expression assembled by the Function-family
  /// constructor; evaluated as eval code, the result is the closure. The
  /// hook also applies step 29 SetFunctionName(F, "anonymous") to the parts
  /// of the function only the interpreter can reach (its code template).
  DynamicFunction
  /// §16.1.6 ScriptEvaluation ($262.evalScript): script code, so
  /// GlobalDeclarationInstantiation makes `var`/function globals
  /// non-configurable, and microtasks are drained before returning.
  ScriptEval
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
    /// to bytecode on the shared heap and run it as `kind` says. A parse
    /// error raises SyntaxError. Interpreter-seeded; the runtime's own seed
    /// raises TypeError (no compiler linked).
    eval_hook: fn(st, String, EvalKind) -> #(JsVal, st),
    /// [[Call]] of a `KBytecode` cell: `(callee, this, args, new_target)`.
    /// Runs a fresh activation to completion; re-raises a throw.
    call_bytecode: fn(st, Handle, JsVal, List(JsVal), JsVal) -> #(JsVal, st),
    /// [[Construct]] of a `KBytecode` cell: `(callee, args, new_target)`.
    construct_bytecode: fn(st, Handle, List(JsVal), JsVal) -> #(Handle, st),
    /// Resume a suspended interpreter frame with `sent` = `#(mode, value)`
    /// (the coroutine driver's Sent pair).
    resume_frame: fn(st, SuspendedFrame, #(Int, JsVal)) -> #(Step, st),
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
    /// The current Realm Record (the running execution context's realm).
    /// Authoritative for its own id: `realms` may hold a stale copy of it.
    realm: Realm,
    /// §13.2.8.4 [[TemplateMap]]: `"<realm id>:<site id>"` -> pinned
    /// template array, so each realm caches its own template objects.
    template_objects: Dict(String, Handle),
    /// Active call chain, innermost first, as the interpreter pushes it on
    /// Call and pops it on Return. `Error` construction renders it into
    /// `stack`. Compiled code keeps it empty.
    frames: List(FrameInfo),
    /// Embedder capabilities (clocks, PRNG, console sink, uncaught-report
    /// sink). Not part of the heap: excluded from GC and serialization.
    hooks: HostHooks,
    /// Embedder natives by `NativeToken.HostFn(id)`, dense from 0 in
    /// registration order (`arc/host` is the only writer). Closures, so
    /// excluded from serialization like `hooks`; the embedder re-registers
    /// them in the same order.
    host_fns: Dict(Int, HostFnEntry),
    /// Every realm of this agent by `Realm.id`. The entry for `realm.id`
    /// is refreshed only when another realm is entered (`rt/realm`), so read
    /// the current realm through `realm`, never through this map.
    realms: Dict(Int, Realm),
    /// The dynamic-import host hook (HostLoadImportedModule), installed by
    /// `arc/module_host`. Kept apart from `host_fns` so installing it never
    /// shifts embedder ids; excluded from serialization like them.
    import_hook: Option(HostFnEntry),
    /// Pending `Atomics.waitAsync` waiters of this agent, oldest first
    /// (§25.4.3.14 DoWait, async mode): this agent's side of the WaiterLists
    /// its waiters joined in the blocks' owner processes. Rooted by `rt/gc`;
    /// settled by `rt/async.drain` (owner wake messages and timeout jobs).
    /// The wakes are sent to the BEAM process that registered the waiter,
    /// so an agent holding any is drained from that process.
    waiters: List(AsyncWaiter),
  )
}

/// This agent's record of one `Atomics.waitAsync` WaiterRecord (§25.4.3.5)
/// registered with a block's owner: which registration it is and the
/// promise capability its wake settles. [[AgentSignifier]] is implicit (the
/// agent whose `waiters` holds it) and [[Result]] is "ok" once notified.
pub type AsyncWaiter {
  AsyncWaiter(
    /// The owner process whose WaiterList holds the registration, and the
    /// registration's identity there (named by the owner's wake message and
    /// by the timeout job's withdrawal).
    owner: SabOwner,
    ref: WaiterRef,
    /// [[PromiseCapability]]: `promise` is the result object's `value`;
    /// `resolve`/`reject` are its resolving functions, called by the
    /// timeout job and by NotifyWaiter's resolve-in-agent job.
    promise: Handle,
    resolve: JsVal,
    reject: JsVal,
    /// [[TimeoutTime]] on the host's monotonic clock (ms); `None` = +∞,
    /// no timeout job armed.
    deadline: Option(Int),
  )
}

/// One `Error.prototype.stack` frame: `at name (script:line)`. `name` is ""
/// for anonymous code (the top-level script body); `line` 0 is unknown.
pub type FrameInfo {
  FrameInfo(name: String, script: String, line: Int)
}

/// The embedder's own value inside a `KHost` cell. `Agent` is deliberately
/// not generic over it: the parameter would thread through every runtime
/// signature for one variant nothing in the runtime inspects. `arc/host`
/// erases to this on allocation and reads it back typed under a `Key(host)`.
pub type HostTerm

/// A registered embedder native. `call` takes `(agent, args, this,
/// new_target)`; `new_target` is `undefined` under [[Call]]. A returned
/// `Error(thrown)` is raised by the dispatcher, so the embedder-facing
/// contract stays Result-shaped while the runtime's stays raise-shaped.
pub type HostFnEntry {
  HostFnEntry(
    name: String,
    call: fn(Agent, List(JsVal), JsVal, JsVal) -> #(Agent, Result(JsVal, JsVal)),
  )
}
