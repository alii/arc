//// TypedArray (Integer-Indexed exotic) element stores — ES2024 §10.4.5.
////
//// This module owns the WRITE half of typed-array element access:
//// §10.4.5.16 IntegerIndexedElementSet (the observable ToNumber/ToBigInt of
//// the assigned value, then the bounds-checked buffer write), the
//// §25.1.2.12 SetValueInBuffer element encoders, and the pure bulk-encode
//// fast paths used by the %TypedArray% builtins. The read half
//// (IntegerIndexedElementGet, TypedArrayLength, CanonicalNumericIndexString)
//// stays with the rest of the object MOP in ops/object, which imports this
//// module for its TypedArray [[Set]] / [[DefineOwnProperty]] arms.
////
//// Value coercion here is the CANONICAL one: ToNumber and ToBigInt are
//// reached through `state.ctx.to_number_fn` / `state.ctx.to_bigint_fn`,
//// installed once at VM boot with `coerce.js_to_number` / `coerce.to_bigint`.
//// ops/coerce is built on top of the object MOP, so it cannot be imported
//// from here — the ctx hook is the same inversion as `ctx.call_fn`. Do NOT
//// re-introduce a private ToPrimitive/ToNumber/ToBigInt in this module.

import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/internal/typed_array_ffi.{
  U8, ta_clamp_uint8, ta_set_float, ta_set_int, ta_zeroed,
}
import arc/vm/key.{type PropertyKey, Index}
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsElements, type JsValue, type Property, type Ref, AccessorProperty,
  ArrayObject, DataProperty, Finite, JsNumber, JsObject, ObjectSlot,
}
import gleam/bit_array
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

/// ToIntegerOrInfinity-style truncation for integer element stores:
/// NaN/±Infinity → 0 (the mod-2^n wrap in the FFI handles the rest).
fn jsnum_to_store_int(n: value.JsNum) -> Int {
  case n {
    Finite(f) -> value.float_to_int(f)
    _ -> 0
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
  state: State(host),
  buffer: Ref,
  elem_kind: value.TypedArrayKind,
  byte_offset: Int,
  length: Option(Int),
  idx: Option(Int),
  val: JsValue,
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
  // Immutable ArrayBuffer proposal, [[Set]] (sec-typedarray-set): "If
  // IsImmutableBuffer(O.[[ViewedArrayBuffer]]) is true, return false" sits
  // BEFORE TypedArraySetElement, so the ToNumber/ToBigInt conversion (and
  // any user code it would run) must not happen. Per the proposal NOTE,
  // immutable is the one case where assignment failure for a canonical
  // numeric string property IS reported — unlike detached/out-of-bounds,
  // which stay silent successes.
  use <- bool.guard(
    buffer_is_immutable(state.heap, buffer),
    Ok(#(state, False)),
  )
  case elem_kind {
    value.BigKind(big_kind) -> {
      // §7.1.13 ToBigInt via the canonical hook (coerce.to_bigint).
      let to_bigint = state.ctx.to_bigint_fn
      use #(n, state) <- result.try(to_bigint(state, val))
      Ok(
        do_typed_store(
          state,
          buffer,
          elem_kind,
          byte_offset,
          length,
          idx,
          fn(data, off) {
            ta_set_int(data, off, typed_array_ffi.bigint_elem(big_kind), n)
          },
        ),
      )
    }
    value.NumKind(num_kind) -> {
      // §7.1.4 ToNumber via the canonical hook (coerce.js_to_number).
      let to_number = state.ctx.to_number_fn
      use #(num, state) <- result.try(to_number(state, val))
      Ok(
        do_typed_store(
          state,
          buffer,
          elem_kind,
          byte_offset,
          length,
          idx,
          fn(data, off) { encode_typed_number(data, off, num_kind, num) },
        ),
      )
    }
  }
}

/// Immutable ArrayBuffer proposal: True when `buffer` is a live
/// ArrayBufferObject whose [[ArrayBufferData]] is immutable.
pub fn buffer_is_immutable(h: Heap(host), buffer: Ref) -> Bool {
  case heap.read(h, buffer) {
    Some(ObjectSlot(kind: value.ArrayBufferObject(immutable:, ..), ..)) ->
      immutable
    _ -> False
  }
}

/// Read a snapshot of the backing store of a non-detached ArrayBuffer slot.
/// None when the ref isn't an ArrayBuffer or the buffer is detached. For
/// shared (atomics-backed) buffers this copies the live bytes out of the
/// shared cells; for plain buffers it is the backing binary itself.
pub fn buffer_bytes(h: Heap(host), buffer: Ref) -> Option(BitArray) {
  case heap.read(h, buffer) {
    Some(ObjectSlot(kind: value.ArrayBufferObject(data: Some(data), ..), ..)) ->
      Some(value.buffer_bits(data))
    _ -> None
  }
}

/// §10.4.5.13 TypedArrayLength, resolved against a live byte size the caller
/// ALREADY has in hand (the store path reads the buffer slot once and must
/// not read it twice). `length: None` is [[ArrayLength]] = AUTO — a
/// length-tracking view over a resizable buffer, whose element count follows
/// the live byte length. Detached buffers and tracking views whose byte
/// offset lies past the end of a shrunk buffer resolve to 0.
pub fn resolved_view_length(
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

/// §10.4.5.13 TypedArrayLength against the buffer currently in the heap.
pub fn view_length(
  h: Heap(host),
  buffer: Ref,
  elem_kind: value.TypedArrayKind,
  byte_offset: Int,
  length: Option(Int),
) -> Int {
  let byte_size =
    buffer_bytes(h, buffer)
    |> option.map(bit_array.byte_size)
    |> option.unwrap(0)
  resolved_view_length(
    byte_size,
    typed_array_ffi.elem_size(elem_kind),
    byte_offset,
    length,
  )
}

/// §10.4.5.14 IsValidIntegerIndex, against a live byte size. `len` is the
/// already-resolved TypedArrayLength (see `resolved_view_length`). An
/// out-of-bounds view — a fixed view over a resizable buffer that shrank
/// below it — has NO valid indices, even for elements whose bytes still
/// exist, so the whole view is checked, not just this element. Both the read
/// half (`ops/object`'s IntegerIndexedElementGet) and the write half
/// (`do_typed_store` below) go through here: the two bounds checks cannot
/// drift apart.
pub fn valid_integer_index(
  byte_size: Int,
  elem_size: Int,
  byte_offset: Int,
  len: Int,
  idx: Int,
) -> Bool {
  idx >= 0 && idx < len && byte_offset + len * elem_size <= byte_size
}

/// Shared store tail: bounds/detach check, then rebuild the buffer binary.
fn do_typed_store(
  state: State(host),
  buffer: Ref,
  elem_kind: value.TypedArrayKind,
  byte_offset: Int,
  length: Option(Int),
  idx: Option(Int),
  write: fn(BitArray, Int) -> BitArray,
) -> #(State(host), Bool) {
  case idx {
    Some(i) ->
      case heap.read(state.heap, buffer) {
        Some(
          ObjectSlot(
            kind: value.ArrayBufferObject(
              data: Some(data),
              max_byte_length:,
              immutable:,
            ),
            ..,
          ) as slot,
        ) -> {
          let size = typed_array_ffi.elem_size(elem_kind)
          let byte_size = value.buffer_byte_size(data)
          // §10.4.5.13/§10.4.5.14 against the LIVE buffer, resolved HERE (not
          // at [[Set]] entry): the ToNumber/ToBigInt conversion above may have
          // run user code that resized the buffer. Same two primitives the
          // read half uses, so an out-of-bounds write can never be accepted by
          // one and rejected by the other.
          let len = resolved_view_length(byte_size, size, byte_offset, length)
          let off = byte_offset + i * size
          case valid_integer_index(byte_size, size, byte_offset, len, i) {
            False -> #(state, True)
            True ->
              case immutable {
                // Immutable ArrayBuffer proposal: typed_array_store already
                // reported [[Set]] failure (False) before value coercion, and
                // a live buffer ref can never become immutable in place
                // (transferToImmutable detaches the source and allocates a
                // fresh ref), so this arm is unreachable. Kept as a defensive
                // failure — immutable writes report False, never a silent
                // success like detached/out-of-bounds.
                True -> #(state, False)
                False -> {
                  let new_bits = write(value.buffer_bits(data), off)
                  // Shared storage: persist only the element's bytes — other
                  // regions may be concurrently written by other agents.
                  let new_data =
                    value.buffer_store_region(data, new_bits, off, size)
                  let h =
                    heap.write(
                      state.heap,
                      buffer,
                      ObjectSlot(
                        ..slot,
                        kind: value.ArrayBufferObject(
                          data: Some(new_data),
                          max_byte_length:,
                          immutable: False,
                        ),
                      ),
                    )
                  #(State(..state, heap: h), True)
                }
              }
          }
        }
        // Detached (or not a buffer): silent no-op per §10.4.5.16 step 2.
        _ -> #(state, True)
      }
    // Out of bounds / non-integral canonical index: silent no-op.
    _ -> #(state, True)
  }
}

/// §25.1.2.12 SetValueInBuffer for Number content types. Total over
/// `NumberKind`, so the BigInt kinds cannot even be passed here.
fn encode_typed_number(
  data: BitArray,
  off: Int,
  elem_kind: value.NumberKind,
  num: value.JsNum,
) -> BitArray {
  case elem_kind {
    // The ONE kind whose store differs from its codec: Uint8Clamped reads as
    // U8 but writes through §7.1.12 ToUint8Clamp instead of the ToInt wrap.
    value.Uint8ClampedKind -> ta_set_int(data, off, U8, ta_clamp_uint8(num))
    // Every other kind stores through its own read codec. Spelled out rather
    // than caught by `_` so a future kind whose store diverges from its codec
    // (another clamped/saturating variant) is a compile error here, not a
    // silently wrong store.
    value.Int8Kind
    | value.Uint8Kind
    | value.Int16Kind
    | value.Uint16Kind
    | value.Int32Kind
    | value.Uint32Kind
    | value.Float32Kind
    | value.Float64Kind ->
      case typed_array_ffi.elem_of_kind(value.NumKind(elem_kind)) {
        typed_array_ffi.Int(e) ->
          ta_set_int(data, off, e, jsnum_to_store_int(num))
        typed_array_ffi.Float(e) -> ta_set_float(data, off, e, num)
      }
  }
}

/// An element value ALREADY converted to a typed array's content-type domain
/// (§10.4.5's "numValue"), TOGETHER with the element kind it was converted
/// for: the ToNumber result for a Number content type, the ToBigInt result
/// for BigInt64/BigUint64. This is the ONLY value shape the encoders accept —
/// an unconverted JsValue (a string, an object, …) cannot reach a buffer
/// write, and neither can a BigInt paired with an Int8Array slot: the pairing
/// is the constructor.
pub type TypedElement {
  NumberElement(kind: value.NumberKind, num: value.JsNum)
  BigIntElement(kind: value.BigIntKind, int: Int)
}

/// Byte width of the slot an element fits — read straight off the element's
/// own kind, so the bounds guard below can never disagree with the encoder.
fn element_size(el: TypedElement) -> Int {
  case el {
    NumberElement(kind:, ..) -> typed_array_ffi.elem_size(value.NumKind(kind))
    BigIntElement(..) -> 8
  }
}

/// Re-classify a value READ back out of a typed array (always a JsNumber for
/// the Number content types, a JsBigInt for the BigInt ones) as a
/// TypedElement destined for a `kind` slot. None when the value's shape does
/// not match `kind`'s content type — element reads never produce one, so
/// callers treat it like a missing element.
pub fn decoded_element(
  kind: value.TypedArrayKind,
  val: JsValue,
) -> Option(TypedElement) {
  case kind, val {
    value.NumKind(k), JsNumber(n) -> Some(NumberElement(k, n))
    value.BigKind(k), value.JsBigInt(value.BigInt(n)) ->
      Some(BigIntElement(k, n))
    _, _ -> None
  }
}

/// Encode an ALREADY-CONVERTED element into the backing store at byte offset
/// `off`. No coercion, no user code — used by TypedArray bulk operations
/// (fill/slice/constructor copies). The element carries its own kind, so the
/// bounds guard and the encoder can never disagree about the slot width.
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
/// whose ToNumber cannot throw (or, for BigInt views, JsBigInt values).
/// Returns None when any value needs the observable per-element path
/// (object coercion may run user code; Symbol/BigInt mismatches throw, and
/// the per-element loop raises that error at the right index).
pub fn typed_array_encode_primitives(
  elem_kind: value.TypedArrayKind,
  values: List(JsValue),
) -> Option(BitArray) {
  let size = typed_array_ffi.elem_size(elem_kind)
  encode_primitives_loop(elem_kind, size, values, [])
}

fn encode_primitives_loop(
  elem_kind: value.TypedArrayKind,
  size: Int,
  values: List(JsValue),
  acc: List(BitArray),
) -> Option(BitArray) {
  case values {
    [] -> Some(bit_array.concat(list.reverse(acc)))
    [v, ..rest] -> {
      let seg = case elem_kind, v {
        value.BigKind(k), value.JsBigInt(value.BigInt(n)) ->
          Some(ta_set_int(ta_zeroed(size), 0, typed_array_ffi.bigint_elem(k), n))
        // Anything else → ToBigInt may throw (or run user code on objects).
        value.BigKind(_), _ -> None
        value.NumKind(_), JsObject(_) -> None
        value.NumKind(k), _ ->
          case value.to_number(v) {
            Ok(num) -> Some(encode_typed_number(ta_zeroed(size), 0, k, num))
            // Symbol/BigInt → TypeError; decline so the per-element path
            // raises it at the right index (with the right error class).
            Error(value.BigIntNotConvertible)
            | Error(value.SymbolNotConvertible) -> None
          }
      }
      case seg {
        Some(s) -> encode_primitives_loop(elem_kind, size, rest, [s, ..acc])
        None -> None
      }
    }
  }
}

/// Read indices 0..len-1 of `ref` as plain own data values WITHOUT running
/// any user code: Array/Arguments dense elements (with defineProperty data
/// overrides honored) and plain-object data properties qualify. Returns None
/// as soon as an index would need an accessor, a proxy trap, a prototype
/// walk (hole), or any exotic [[Get]] — callers must then use the observable
/// per-element path. Excludes object values, so a later ToNumber/ToBigInt of
/// the extracted values cannot run user code either.
pub fn plain_indexed_values(
  h: Heap(host),
  ref: Ref,
  len: Int,
) -> Option(List(JsValue)) {
  case heap.read(h, ref) {
    Some(ObjectSlot(kind:, properties:, elements:, ..)) ->
      case kind {
        // Same own-lookup order as the Array/Arguments Index read fast path:
        // properties-dict override first, dense elements otherwise. Plain
        // objects keep indexed props in the dict only (elements stays empty),
        // so the shared loop mirrors their ordinary [[Get]] too.
        ArrayObject(_) | value.ArgumentsObject(_) | value.OrdinaryObject ->
          plain_indexed_loop(properties, elements, len - 1, [])
        _ -> None
      }
    _ -> None
  }
}

fn plain_indexed_loop(
  properties: dict.Dict(PropertyKey, Property),
  elements: JsElements,
  k: Int,
  acc: List(JsValue),
) -> Option(List(JsValue)) {
  case k < 0 {
    True -> Some(acc)
    False -> {
      let v = case dict.get(properties, Index(k)) {
        Ok(DataProperty(value: v, ..)) -> Some(v)
        Ok(AccessorProperty(..)) -> None
        Error(Nil) -> elements.get_option(elements, k)
      }
      case v {
        // Object values are excluded — converting them can run user code.
        Some(JsObject(_)) | None -> None
        Some(v) -> plain_indexed_loop(properties, elements, k - 1, [v, ..acc])
      }
    }
  }
}
