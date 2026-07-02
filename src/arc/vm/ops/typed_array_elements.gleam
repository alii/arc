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
  ta_clamp_uint8, ta_set_float, ta_set_int, ta_zeroed,
}
import arc/vm/key.{Index}
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsElements, type JsValue, type Property, type PropertyKey, type Ref,
  AccessorProperty, ArrayObject, DataProperty, Finite, JsNumber, JsObject,
  ObjectSlot,
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
  case value.typed_array_is_bigint(elem_kind) {
    True -> {
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
          fn(data, off) { ta_set_int(data, off, 64, n) },
        ),
      )
    }
    False -> {
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
          fn(data, off) { encode_typed_number(data, off, elem_kind, num) },
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
    Some(i) if i >= 0 ->
      case heap.read(state.heap, buffer) {
        Some(
          ObjectSlot(
            kind: value.ArrayBufferObject(
              data:,
              detached: False,
              max_byte_length:,
              immutable:,
            ),
            ..,
          ) as slot,
        ) -> {
          let size = value.typed_array_element_size(elem_kind)
          let byte_size = value.buffer_byte_size(data)
          // §10.4.5.14 IsValidIntegerIndex against the LIVE buffer, resolved
          // HERE (not at [[Set]] entry): the ToNumber/ToBigInt conversion
          // above may have run user code that resized the buffer. A
          // length-tracking view (None) follows the live byte length; a
          // fixed view that no longer fits is wholly out of bounds and the
          // write is a silent no-op, like a detached buffer.
          let len = case length {
            Some(n) -> n
            None -> int.max(0, { byte_size - byte_offset } / size)
          }
          let off = byte_offset + i * size
          case i < len && byte_offset + len * size <= byte_size {
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
                          data: new_data,
                          detached: False,
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

/// §25.1.2.12 SetValueInBuffer for Number content types.
fn encode_typed_number(
  data: BitArray,
  off: Int,
  elem_kind: value.TypedArrayKind,
  num: value.JsNum,
) -> BitArray {
  case elem_kind {
    value.Uint8ClampedKind -> ta_set_int(data, off, 8, ta_clamp_uint8(num))
    value.Float32Kind -> ta_set_float(data, off, 32, num)
    value.Float64Kind -> ta_set_float(data, off, 64, num)
    value.Int8Kind | value.Uint8Kind ->
      ta_set_int(data, off, 8, jsnum_to_store_int(num))
    value.Int16Kind | value.Uint16Kind ->
      ta_set_int(data, off, 16, jsnum_to_store_int(num))
    value.Int32Kind | value.Uint32Kind ->
      ta_set_int(data, off, 32, jsnum_to_store_int(num))
    // BigInt kinds never reach here (typed_array_store routes them through
    // the ToBigInt hook), but keep the encode total just in case.
    value.BigInt64Kind | value.BigUint64Kind ->
      ta_set_int(data, off, 64, jsnum_to_store_int(num))
  }
}

/// An element value ALREADY converted to a typed array's content-type domain
/// (§10.4.5's "numValue"): the ToNumber result for the Number content types,
/// the ToBigInt result for BigInt64/BigUint64. This is the ONLY value shape
/// the encoders accept — an unconverted JsValue (a string, an object, …)
/// cannot reach a buffer write.
pub type TypedElement {
  NumberElement(value.JsNum)
  BigIntElement(Int)
}

/// Re-classify a value READ back out of a typed array (always a JsNumber for
/// the Number content types, a JsBigInt for the BigInt ones) as a
/// TypedElement for re-encoding. None for any other value — element reads
/// never produce one, so callers treat it like a missing element.
pub fn decoded_element(val: JsValue) -> Option(TypedElement) {
  case val {
    JsNumber(n) -> Some(NumberElement(n))
    value.JsBigInt(value.BigInt(n)) -> Some(BigIntElement(n))
    _ -> None
  }
}

/// Encode an ALREADY-CONVERTED element into the backing store at byte offset
/// `off`. No coercion, no user code — used by TypedArray bulk operations
/// (fill/slice/constructor copies).
pub fn typed_array_encode_value(
  data: BitArray,
  off: Int,
  elem_kind: value.TypedArrayKind,
  el: TypedElement,
) -> BitArray {
  // Guard against writes past the CURRENT backing store (a resizable
  // ArrayBuffer may have shrunk below the view) — out-of-bounds typed-array
  // writes are silent no-ops, never crashes.
  use <- bool.guard(
    off + value.typed_array_element_size(elem_kind) > bit_array.byte_size(data),
    data,
  )
  case el {
    NumberElement(n) -> encode_typed_number(data, off, elem_kind, n)
    BigIntElement(n) -> ta_set_int(data, off, 64, n)
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
  let size = value.typed_array_element_size(elem_kind)
  encode_primitives_loop(
    elem_kind,
    size,
    value.typed_array_is_bigint(elem_kind),
    values,
    [],
  )
}

fn encode_primitives_loop(
  elem_kind: value.TypedArrayKind,
  size: Int,
  bigint: Bool,
  values: List(JsValue),
  acc: List(BitArray),
) -> Option(BitArray) {
  case values {
    [] -> Some(bit_array.concat(list.reverse(acc)))
    [v, ..rest] -> {
      let seg = case bigint, v {
        True, value.JsBigInt(value.BigInt(n)) ->
          Some(ta_set_int(ta_zeroed(size), 0, 64, n))
        // Anything else → ToBigInt may throw (or run user code on objects).
        True, _ -> None
        False, JsObject(_) -> None
        False, _ ->
          case value.to_number(v) {
            Ok(num) ->
              Some(encode_typed_number(ta_zeroed(size), 0, elem_kind, num))
            // Symbol/BigInt → TypeError; decline so the per-element path
            // raises it at the right index.
            Error(_to_number_throws) -> None
          }
      }
      case seg {
        Some(s) ->
          encode_primitives_loop(elem_kind, size, bigint, rest, [s, ..acc])
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
