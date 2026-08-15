//// ArrayBuffer storage access and TypedArray (Integer-Indexed exotic)
//// element reads and stores — ES2024 §10.4.5 / §25.1.2.
////
//// This module owns both halves of typed-array element access:
//// §10.4.5.15 IntegerIndexedElementGet, §10.4.5.13 TypedArrayLength,
//// §7.1.21 CanonicalNumericIndexString (the read half the MOP arms in
//// `arc/rt/obj` need), and §10.4.5.16 IntegerIndexedElementSet with the
//// §25.1.2.12 SetValueInBuffer element encoders plus the pure bulk-encode
//// fast paths used by the %TypedArray% builtins. The buffer storage rewrite
//// (`store_region`) lives here exactly once, so no call site can rebuild an
//// ArrayBufferObj cell and lose part of its state on the way back in.
////
//// Value coercion is the CANONICAL one (`rt_val.t_to_number` /
//// `rt_val.t_to_bigint`). Do NOT re-introduce a private ToNumber/ToBigInt.

import arc/rt/elements
import arc/rt/store as rt_store
import arc/rt/typed_array_ffi.{
  U8, ta_clamp_uint8, ta_get_float, ta_get_int, ta_set_float, ta_set_int,
  ta_zeroed,
}
import arc/rt/types.{
  type Agent, type BigIntKind, type BufferStorage, type Handle, type JsElements,
  type JsNum, type JsVal, type NumberKind, type Property, type PropertyKey,
  type TypedArrayKind, AccessorProperty, ArgumentsObj, ArrayBufferObj, ArrayObj,
  BigKind, DataProperty, Index, JFloat, JInt, JNan, JNegInf, JPosInf, KBig,
  KHandle, KNum, NumKind, Ordinary, SObject, SShapedObject, classify, mk_bigint,
  mk_number,
}
import arc/rt/val as rt_val
import gleam/bit_array
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

// ── buffer storage access ───────────────────────────────────────────────────

/// The [[ArrayBufferData]] storage of the ArrayBufferObj cell at `buffer`, or
/// None when the handle does not hold a buffer at all.
pub fn buffer_storage(st: Agent, buffer: Handle) -> Option(BufferStorage) {
  case rt_store.t_cell_get(st, buffer) {
    SObject(kind: ArrayBufferObj(storage:), ..) -> Some(storage)
    _ -> None
  }
}

/// The backing store of a non-detached ArrayBuffer cell. None when the
/// handle isn't an ArrayBuffer or the buffer is detached.
pub fn buffer_bytes(st: Agent, buffer: Handle) -> Option(BitArray) {
  buffer_storage(st, buffer) |> option.then(types.buffer_bits)
}

/// Immutable ArrayBuffer proposal: True when `buffer` is a live
/// ArrayBufferObj whose [[ArrayBufferData]] is immutable.
pub fn buffer_is_immutable(st: Agent, buffer: Handle) -> Bool {
  buffer_storage(st, buffer)
  |> option.map(types.buffer_is_immutable)
  |> option.unwrap(False)
}

/// Byte size of the buffer currently in the store; 0 for a detached buffer or
/// a handle that isn't a buffer.
fn live_byte_size(st: Agent, buffer: Handle) -> Int {
  buffer_storage(st, buffer)
  |> option.map(types.buffer_byte_size)
  |> option.unwrap(0)
}

/// Replace the whole [[ArrayBufferData]] storage of the buffer cell.
/// `buffer` is always a validated ArrayBuffer handle, so a cell of any other
/// shape is a wiring bug: crash rather than silently drop the write.
pub fn set_storage(st: Agent, buffer: Handle, storage: BufferStorage) -> Agent {
  use slot <- rt_store.t_cell_update(st, buffer)
  let assert SObject(kind: ArrayBufferObj(..), ..) = slot
    as "buffer.set_storage: handle does not hold an ArrayBuffer"
  SObject(..slot, kind: ArrayBufferObj(storage:))
}

/// Persist `new_bits` (a whole new image of the buffer's bytes) into the
/// ArrayBuffer cell at `buffer`; `byte_offset`/`count` name the bytes the
/// caller wrote (a caller that owns the whole buffer passes the full range).
/// Detached and immutable buffers keep whatever they had — every write path
/// rejects them before getting here (see `types.buffer_store_region`).
pub fn store_region(
  st: Agent,
  buffer: Handle,
  new_bits: BitArray,
  byte_offset: Int,
  count: Int,
) -> Agent {
  use slot <- rt_store.t_cell_update(st, buffer)
  let assert SObject(kind: ArrayBufferObj(storage:), ..) = slot
    as "buffer.store_region: handle does not hold an ArrayBuffer"
  SObject(
    ..slot,
    kind: ArrayBufferObj(storage: types.buffer_store_region(
      storage,
      new_bits,
      byte_offset,
      count,
    )),
  )
}

// ── view records ────────────────────────────────────────────────────────────

/// The four internal slots that identify a typed-array view of a buffer —
/// [[ViewedArrayBuffer]], [[TypedArrayName]]'s element kind, [[ByteOffset]],
/// and [[ArrayLength]] (None for AUTO on a length-tracking view). Bundled so
/// the store path takes ONE view identity, not four positionals.
pub type ViewSlot {
  ViewSlot(
    buffer: Handle,
    elem_kind: TypedArrayKind,
    byte_offset: Int,
    length: Option(Int),
  )
}

/// A typed-array view whose §10.4.5.13 TypedArrayLength has ALREADY been
/// resolved against a specific live byte size — the four numbers a bounds
/// check needs, bundled so they cannot be mixed and matched. Opaque, and only
/// `resolve_view`/`fixed_view` build one, so `len` is always TypedArrayLength
/// of *these* `byte_size`/`elem_size`/`byte_offset`.
pub opaque type ResolvedView {
  ResolvedView(byte_size: Int, elem_size: Int, byte_offset: Int, len: Int)
}

/// §10.4.5.13 TypedArrayLength, resolved against a live byte size the caller
/// ALREADY has in hand. `length: None` is [[ArrayLength]] = AUTO — a
/// length-tracking view over a resizable buffer, whose element count follows
/// the live byte length. Detached buffers and tracking views whose byte
/// offset lies past the end of a shrunk buffer resolve to 0.
pub fn resolve_view(
  byte_size: Int,
  elem_kind: TypedArrayKind,
  byte_offset: Int,
  length: Option(Int),
) -> ResolvedView {
  let elem_size = typed_array_ffi.elem_size(elem_kind)
  ResolvedView(
    byte_size:,
    elem_size:,
    byte_offset:,
    len: resolve_len(byte_size, elem_size, byte_offset, length),
  )
}

/// `resolve_view` for a view whose [[ArrayLength]] is already a plain Int
/// (never AUTO).
pub fn fixed_view(
  byte_size: Int,
  elem_kind: TypedArrayKind,
  byte_offset: Int,
  len: Int,
) -> ResolvedView {
  ResolvedView(
    byte_size:,
    elem_size: typed_array_ffi.elem_size(elem_kind),
    byte_offset:,
    len:,
  )
}

/// The TypedArrayLength arithmetic on its own.
fn resolve_len(
  byte_size: Int,
  elem_size: Int,
  byte_offset: Int,
  length: Option(Int),
) -> Int {
  case length {
    Some(n) -> n
    None -> int.max(0, { byte_size - byte_offset } / elem_size)
  }
}

/// The resolved TypedArrayLength of the view.
pub fn view_len(view: ResolvedView) -> Int {
  view.len
}

/// Byte offset of element `idx` — derived from the view's own element size,
/// so a bounds check and the read/write it guards can never disagree about
/// which bytes the element occupies.
pub fn view_element_offset(view: ResolvedView, idx: Int) -> Int {
  view.byte_offset + idx * view.elem_size
}

/// True when the whole view still fits inside the live buffer. False for a
/// fixed view over a resizable buffer that shrank below it — which per
/// §10.4.5.14 has NO valid indices, even for elements whose bytes still exist.
pub fn view_in_bounds(view: ResolvedView) -> Bool {
  view.byte_offset + view.len * view.elem_size <= view.byte_size
}

/// §10.4.5.13 TypedArrayLength against the buffer currently in the store. A
/// detached buffer reads as zero bytes, so a length-tracking view over one
/// resolves to 0 (a fixed view keeps its declared [[ArrayLength]] — the
/// bounds check, not the length, is what rejects its indices).
pub fn view_length(st: Agent, view: ViewSlot) -> Int {
  resolve_len(
    live_byte_size(st, view.buffer),
    typed_array_ffi.elem_size(view.elem_kind),
    view.byte_offset,
    view.length,
  )
}

/// `resolve_view` against the buffer currently in the store. None when the
/// buffer is DETACHED (or the handle isn't a buffer at all): per §10.4.5.14 a
/// detached buffer has no valid indices at all, so there is no live view to
/// resolve. Every `ResolvedView` therefore describes bytes that really exist.
pub fn live_view(st: Agent, view: ViewSlot) -> Option(ResolvedView) {
  let ViewSlot(buffer:, elem_kind:, byte_offset:, length:) = view
  use data <- option.map(buffer_bytes(st, buffer))
  resolve_view(bit_array.byte_size(data), elem_kind, byte_offset, length)
}

/// §10.4.5.14 IsValidIntegerIndex, against the byte size the view was
/// resolved against. The whole view is checked, not just this element (see
/// `view_in_bounds`). Both the read half and the write half go through here:
/// the two bounds checks cannot drift apart.
pub fn valid_integer_index(view: ResolvedView, idx: Int) -> Bool {
  idx >= 0 && idx < view.len && view_in_bounds(view)
}

// ── element reads ───────────────────────────────────────────────────────────

/// §10.4.5.13 TypedArrayLength — current [[ArrayLength]] of a typed-array
/// view, positional spelling for the MOP arms.
pub fn typed_array_view_length(
  st: Agent,
  buffer: Handle,
  elem_kind: TypedArrayKind,
  byte_offset: Int,
  length: Option(Int),
) -> Int {
  view_length(st, ViewSlot(buffer:, elem_kind:, byte_offset:, length:))
}

/// §10.4.5.15 IntegerIndexedElementGet — element at `idx`, or None when the
/// index is invalid (negative, >= length, or the buffer is detached).
pub fn typed_array_element(
  st: Agent,
  buffer: Handle,
  elem_kind: TypedArrayKind,
  byte_offset: Int,
  length: Int,
  idx: Int,
) -> Option(JsVal) {
  use <- bool.guard(idx < 0 || idx >= length, None)
  case buffer_bytes(st, buffer) {
    None -> None
    Some(data) ->
      element_of_view(
        data,
        fixed_view(bit_array.byte_size(data), elem_kind, byte_offset, length),
        elem_kind,
        idx,
      )
  }
}

/// §10.4.5.15 IntegerIndexedElementGet against the CURRENT view length: the
/// declared `length` (None for a length-tracking view) is resolved through
/// TypedArrayLength first. This is what every MOP element read wants — the
/// declared length alone is stale the moment a resizable buffer changes size.
/// The buffer is read ONCE and both TypedArrayLength and IsValidIntegerIndex
/// answer against those bytes.
pub fn typed_array_element_live(
  st: Agent,
  buffer: Handle,
  elem_kind: TypedArrayKind,
  byte_offset: Int,
  length: Option(Int),
  idx: Int,
) -> Option(JsVal) {
  use <- bool.guard(idx < 0, None)
  case buffer_bytes(st, buffer) {
    None -> None
    Some(data) ->
      element_of_view(
        data,
        resolve_view(bit_array.byte_size(data), elem_kind, byte_offset, length),
        elem_kind,
        idx,
      )
  }
}

/// The tail both element reads share: §10.4.5.14 IsValidIntegerIndex against
/// the CURRENT backing store — the SAME predicate the write half applies.
fn element_of_view(
  data: BitArray,
  view: ResolvedView,
  elem_kind: TypedArrayKind,
  idx: Int,
) -> Option(JsVal) {
  case valid_integer_index(view, idx) {
    True ->
      Some(decode_typed_element(data, view_element_offset(view, idx), elem_kind))
    False -> None
  }
}

/// Why a typed-array view failed its buffer witness. The cases are NOT
/// interchangeable — a detached buffer has no bytes at all, an out-of-bounds
/// view is a resizable buffer that shrank below the view, and a non-view is a
/// receiver that never had a buffer — so callers get a category, never a
/// pre-worded string. The %TypedArray% builtins raise these too.
pub type ViewWitnessError {
  /// The view's `ArrayBuffer` was detached (transferred, or `.transfer()`d).
  BufferDetached
  /// The buffer is live but no longer covers the view's byte range.
  OutOfBoundsView
  /// The receiver is not a TypedArray at all (RequireInternalSlot failed).
  NotAView
}

/// The ONE place a `ViewWitnessError` becomes prose.
pub fn view_witness_error_message(err: ViewWitnessError) -> String {
  case err {
    BufferDetached -> "Cannot perform operation on a detached ArrayBuffer"
    OutOfBoundsView -> "TypedArray is out of bounds"
    NotAView -> "Method invoked on an object that is not a TypedArray"
  }
}

/// §23.1.5.1 CreateArrayIterator buffer-witness check for typed-array
/// sources: each `.next()` re-validates the view against the CURRENT buffer
/// (MakeTypedArrayWithBufferWitnessRecord + IsTypedArrayOutOfBounds) and
/// fails on a detached buffer or an out-of-bounds view. Ok(length) otherwise.
pub fn typed_array_iter_length(
  st: Agent,
  buffer: Handle,
  elem_kind: TypedArrayKind,
  byte_offset: Int,
  length: Option(Int),
) -> Result(Int, ViewWitnessError) {
  case buffer_bytes(st, buffer) {
    None -> Error(BufferDetached)
    Some(data) -> {
      let view =
        resolve_view(bit_array.byte_size(data), elem_kind, byte_offset, length)
      case view_in_bounds(view) {
        False -> Error(OutOfBoundsView)
        True -> Ok(view_len(view))
      }
    }
  }
}

/// Number of valid element indices of the view against the CURRENT buffer —
/// `length` when the view is fully in bounds, 0 when the buffer is detached
/// or the view is out of bounds: per §10.4.5.14 an out-of-bounds view has NO
/// valid indices, even for elements whose bytes still exist.
pub fn typed_array_live_length(
  st: Agent,
  buffer: Handle,
  elem_kind: TypedArrayKind,
  byte_offset: Int,
  length: Int,
) -> Int {
  case buffer_bytes(st, buffer) {
    None -> 0
    Some(data) -> {
      let view =
        fixed_view(bit_array.byte_size(data), elem_kind, byte_offset, length)
      case view_in_bounds(view) {
        True -> length
        False -> 0
      }
    }
  }
}

/// `typed_array_live_length` for a view whose declared `length` may still be
/// AUTO: the number of indices the view actually has right now — 0 when
/// detached or out of bounds.
pub fn typed_array_live_count(
  st: Agent,
  buffer: Handle,
  elem_kind: TypedArrayKind,
  byte_offset: Int,
  length: Option(Int),
) -> Int {
  typed_array_iter_length(st, buffer, elem_kind, byte_offset, length)
  |> result.unwrap(0)
}

/// Decode one element from the backing store (§25.1.2.10 GetValueFromBuffer).
fn decode_typed_element(
  data: BitArray,
  off: Int,
  elem_kind: TypedArrayKind,
) -> JsVal {
  // The kind -> codec mapping is typed_array_ffi.elem_of_kind's job; all this
  // adds is the wrapper the content type calls for (Number vs BigInt).
  case elem_kind {
    BigKind(k) ->
      mk_bigint(ta_get_int(data, off, typed_array_ffi.bigint_elem(k)))
    NumKind(_) ->
      case typed_array_ffi.elem_of_kind(elem_kind) {
        typed_array_ffi.Int(e) -> mk_number(JInt(ta_get_int(data, off, e)))
        typed_array_ffi.Float(e) -> mk_number(ta_get_float(data, off, e))
      }
  }
}

/// §7.1.21 CanonicalNumericIndexString: "-0", or a string that round-trips
/// through ToNumber → ToString. Such keys on a TypedArray NEVER reach the
/// ordinary property table (§10.4.5).
pub fn is_canonical_numeric_string(s: String) -> Bool {
  // Fast reject on the first byte: a canonical numeric string is the ToString
  // of a Number, which always starts with a digit, '-' (negatives/-0/
  // -Infinity), 'I' (Infinity) or 'N' (NaN). Ordinary property names
  // ("length", "buffer", "constructor", method names, …) bail out here
  // without the ToNumber → ToString round-trip.
  case s {
    "0" <> _
    | "1" <> _
    | "2" <> _
    | "3" <> _
    | "4" <> _
    | "5" <> _
    | "6" <> _
    | "7" <> _
    | "8" <> _
    | "9" <> _
    | "-" <> _
    | "+" <> _
    | "." <> _
    | "I" <> _
    | "N" <> _ ->
      s == "-0" || rt_val.jsnum_to_string(rt_val.string_to_number(s)) == s
    _ -> False
  }
}

// ── element stores ──────────────────────────────────────────────────────────

/// ToIntegerOrInfinity-style truncation for integer element stores:
/// NaN/±Infinity → 0 (the mod-2^n wrap in the FFI handles the rest).
fn jsnum_to_store_int(n: JsNum) -> Int {
  case n {
    JInt(i) -> i
    JFloat(f) -> rt_val.float_to_int(f)
    JNan | JPosInf | JNegInf -> 0
  }
}

/// §10.4.5.16 IntegerIndexedElementSet: convert the value first (observable —
/// valueOf / toString / @@toPrimitive may run user code), then store it if
/// `idx` is Some(valid index) and the buffer is live. Returns True for
/// out-of-bounds/detached writes (silent no-ops), but False — BEFORE any
/// value coercion — when the viewed buffer is immutable (Immutable
/// ArrayBuffer proposal, sec-typedarray-set), so strict-mode assignment
/// throws and valueOf/toString side effects never run.
pub fn typed_array_store(
  st: Agent,
  view: ViewSlot,
  idx: Option(Int),
  val: JsVal,
) -> #(Bool, Agent) {
  // Immutable ArrayBuffer proposal, [[Set]] (sec-typedarray-set): "If
  // IsImmutableBuffer(O.[[ViewedArrayBuffer]]) is true, return false" sits
  // BEFORE TypedArraySetElement, so the ToNumber/ToBigInt conversion (and
  // any user code it would run) must not happen.
  use <- bool.guard(buffer_is_immutable(st, view.buffer), #(False, st))
  case view.elem_kind {
    BigKind(big_kind) -> {
      let #(n, st) = rt_val.t_to_bigint(st, val)
      do_typed_store(st, view, idx, fn(data, off) {
        ta_set_int(data, off, typed_array_ffi.bigint_elem(big_kind), n)
      })
    }
    NumKind(num_kind) -> {
      let #(num, st) = rt_val.t_to_number(st, val)
      do_typed_store(st, view, idx, fn(data, off) {
        encode_typed_number(data, off, num_kind, num)
      })
    }
  }
}

/// Shared store tail: bounds/detach check, then rebuild the buffer binary.
fn do_typed_store(
  st: Agent,
  view: ViewSlot,
  idx: Option(Int),
  write: fn(BitArray, Int) -> BitArray,
) -> #(Bool, Agent) {
  case idx {
    Some(i) ->
      case rt_store.t_cell_get(st, view.buffer) {
        SObject(kind: ArrayBufferObj(storage:), ..) as slot -> {
          let size = typed_array_ffi.elem_size(view.elem_kind)
          // §10.4.5.13/§10.4.5.14 against the LIVE buffer, resolved HERE (not
          // at [[Set]] entry): the ToNumber/ToBigInt conversion above may have
          // run user code that resized the buffer. Same two primitives the
          // read half uses. A detached buffer measures 0 bytes, so it fails
          // this check too — the silent no-op of §10.4.5.16 step 2 covers both.
          let resolved =
            resolve_view(
              types.buffer_byte_size(storage),
              view.elem_kind,
              view.byte_offset,
              view.length,
            )
          let off = view_element_offset(resolved, i)
          use <- bool.guard(!valid_integer_index(resolved, i), #(True, st))
          // Immutable ArrayBuffer proposal: typed_array_store already reported
          // [[Set]] failure before value coercion, and a live buffer can never
          // become immutable in place, so this guard is unreachable. Kept as a
          // defensive failure — immutable writes report False, never a silent
          // success like detached/out-of-bounds.
          use <- bool.guard(types.buffer_is_immutable(storage), #(False, st))
          case types.buffer_bits(storage) {
            // Unreachable: detached storage has no in-bounds indices.
            None -> #(True, st)
            Some(data) -> {
              let new_bits = write(data, off)
              let new_storage =
                types.buffer_store_region(storage, new_bits, off, size)
              let st =
                rt_store.t_cell_set(
                  st,
                  view.buffer,
                  SObject(..slot, kind: ArrayBufferObj(storage: new_storage)),
                )
              #(True, st)
            }
          }
        }
        // Detached (or not a buffer): silent no-op per §10.4.5.16 step 2.
        _ -> #(True, st)
      }
    // Out of bounds / non-integral canonical index: silent no-op.
    None -> #(True, st)
  }
}

/// §25.1.2.12 SetValueInBuffer for Number content types. Total over
/// `NumberKind`, so the BigInt kinds cannot even be passed here.
fn encode_typed_number(
  data: BitArray,
  off: Int,
  elem_kind: NumberKind,
  num: JsNum,
) -> BitArray {
  // The store-direction codec table, NOT the read one: Uint8Clamped comes
  // back as its own `StoreClampedU8` case, so an unclamped Uint8Clamped
  // store cannot be written here by accident.
  case typed_array_ffi.store_elem_of_kind(NumKind(elem_kind)) {
    typed_array_ffi.StoreClampedU8 ->
      ta_set_int(data, off, U8, ta_clamp_uint8(num))
    typed_array_ffi.StoreInt(e) ->
      ta_set_int(data, off, e, jsnum_to_store_int(num))
    typed_array_ffi.StoreFloat(e) -> ta_set_float(data, off, e, num)
  }
}

/// An element value ALREADY converted to a typed array's content-type domain
/// (§10.4.5's "numValue"), TOGETHER with the element kind it was converted
/// for. This is the ONLY value shape the encoders accept — an unconverted
/// JsVal cannot reach a buffer write, and neither can a BigInt paired with an
/// Int8Array slot: the pairing is the constructor.
pub type TypedElement {
  NumberElement(kind: NumberKind, num: JsNum)
  BigIntElement(kind: BigIntKind, int: Int)
}

/// Byte width of the slot an element fits.
fn element_size(el: TypedElement) -> Int {
  case el {
    NumberElement(kind:, ..) -> typed_array_ffi.elem_size(NumKind(kind))
    BigIntElement(..) -> 8
  }
}

/// Re-classify a value READ back out of a typed array (always a Number for
/// the Number content types, a BigInt for the BigInt ones) as a TypedElement
/// destined for a `kind` slot. None when the value's shape does not match
/// `kind`'s content type — element reads never produce one, so callers treat
/// it like a missing element.
pub fn decoded_element(
  kind: TypedArrayKind,
  val: JsVal,
) -> Option(TypedElement) {
  case kind, classify(val) {
    NumKind(k), KNum(n) -> Some(NumberElement(k, n))
    BigKind(k), KBig(n) -> Some(BigIntElement(k, n))
    _, _ -> None
  }
}

/// Encode an ALREADY-CONVERTED element into the backing store at byte offset
/// `off`. No coercion, no user code — used by TypedArray bulk operations
/// (fill/slice/constructor copies).
pub fn typed_array_encode_value(
  data: BitArray,
  off: Int,
  el: TypedElement,
) -> BitArray {
  // Guard against writes past the CURRENT backing store (a resizable
  // ArrayBuffer may have shrunk below the view) — out-of-bounds typed-array
  // writes are silent no-ops, never crashes.
  use <- bool.guard(off + element_size(el) > bit_array.byte_size(data), data)
  case el {
    NumberElement(kind:, num:) -> encode_typed_number(data, off, kind, num)
    BigIntElement(kind:, int:) ->
      ta_set_int(data, off, typed_array_ffi.bigint_elem(kind), int)
  }
}

/// Encode a run of values into one concatenated element-region binary for a
/// bulk typed-array store — only when EVERY conversion is pure: primitives
/// whose ToNumber cannot throw (or, for BigInt views, BigInt values).
/// Returns None when any value needs the observable per-element path
/// (object coercion may run user code; Symbol/BigInt mismatches throw, and
/// the per-element loop raises that error at the right index).
pub fn typed_array_encode_primitives(
  elem_kind: TypedArrayKind,
  values: List(JsVal),
) -> Option(BitArray) {
  let size = typed_array_ffi.elem_size(elem_kind)
  encode_primitives_loop(elem_kind, size, values, [])
}

fn encode_primitives_loop(
  elem_kind: TypedArrayKind,
  size: Int,
  values: List(JsVal),
  acc: List(BitArray),
) -> Option(BitArray) {
  case values {
    [] -> Some(bit_array.concat(list.reverse(acc)))
    [v, ..rest] -> {
      let seg = case elem_kind, classify(v) {
        BigKind(k), KBig(n) ->
          Some(ta_set_int(ta_zeroed(size), 0, typed_array_ffi.bigint_elem(k), n))
        // Anything else → ToBigInt may throw (or run user code on objects).
        BigKind(_), _ -> None
        NumKind(_), KHandle(_) -> None
        NumKind(k), _ ->
          case rt_val.prim_to_number(v) {
            Ok(num) -> Some(encode_typed_number(ta_zeroed(size), 0, k, num))
            // Symbol/BigInt → TypeError; decline so the per-element path
            // raises it at the right index (with the right error class).
            Error(rt_val.BigIntToNumber)
            | Error(rt_val.SymbolToNumber)
            | Error(rt_val.NeedsToPrimitive) -> None
          }
      }
      case seg {
        Some(s) -> encode_primitives_loop(elem_kind, size, rest, [s, ..acc])
        None -> None
      }
    }
  }
}

/// Read indices 0..len-1 of `h` as plain own data values WITHOUT running any
/// user code: Array/Arguments dense elements (with defineProperty data
/// overrides honored) and plain-object data properties qualify. Returns None
/// as soon as an index would need an accessor, a proxy trap, a prototype
/// walk (hole), or any exotic [[Get]] — callers must then use the observable
/// per-element path. Excludes object values, so a later ToNumber/ToBigInt of
/// the extracted values cannot run user code either.
pub fn plain_indexed_values(
  st: Agent,
  h: Handle,
  len: Int,
) -> Option(List(JsVal)) {
  case rt_store.t_cell_get(st, h) {
    SObject(kind:, props:, elements:, ..) ->
      case kind {
        // Same own-lookup order as the Array/Arguments Index read path:
        // props-dict override first, dense elements otherwise. Plain objects
        // keep indexed props in the dict only (elements stays empty).
        ArrayObj(_) | ArgumentsObj(..) | Ordinary ->
          plain_indexed_loop(props, elements, len - 1, [])
        _ -> None
      }
    // A shaped object holds named keys only: index 0 is already a hole.
    SShapedObject(..) ->
      case len {
        0 -> Some([])
        _ -> None
      }
    _ -> None
  }
}

fn plain_indexed_loop(
  props: dict.Dict(PropertyKey, Property),
  elements: JsElements,
  k: Int,
  acc: List(JsVal),
) -> Option(List(JsVal)) {
  case k < 0 {
    True -> Some(acc)
    False -> {
      let v = case dict.get(props, Index(k)) {
        Ok(DataProperty(value: v, ..)) -> Some(v)
        Ok(AccessorProperty(..)) -> None
        Error(Nil) -> elements.get_option(elements, k)
      }
      case v {
        None -> None
        Some(v) ->
          case classify(v) {
            // Object values are excluded — converting them can run user code.
            KHandle(_) -> None
            _ -> plain_indexed_loop(props, elements, k - 1, [v, ..acc])
          }
      }
    }
  }
}
