//// The ONE binding surface for `arc_typed_array_ffi.erl`.
////
//// Every `@external` declaration for that Erlang module lives here and
//// nowhere else. Previously ops/object, ops/typed_array_elements,
//// builtins/typed_array and builtins/atomics each re-declared the bindings
//// they used privately — two independent, unchecked assertions of the same
//// Erlang signature is how an arity or type change ships silently. Now a
//// signature exists exactly once and the compiler checks every caller
//// against it.
////
//// Float elements speak `value.JsNum` directly (`{finite, F} | na_n |
//// infinity | neg_infinity` on the Erlang side) — the same technique as
//// `arc_math_ffi`. There is no int-tag side channel: the compiler forces
//// every caller to handle the non-finite constructors.

import arc/vm/value.{type JsNum}
import gleam/bit_array
import gleam/int
import gleam/result

/// The integer element widths+signednesses the codecs implement, as one
/// closed set. `size_bits: Int` + `signed: Bool` used to be two independent
/// scalars, so `ta_get_int(data, off, 24, True)` type-checked and read a
/// nonsense width; now every call names an element the Erlang side has a
/// clause for.
pub type IntElem {
  I8
  U8
  I16
  U16
  I32
  U32
  I64
  U64
}

/// The two float element widths the codecs implement.
pub type FloatElem {
  F32
  F64
}

/// Allocate an all-zero binary of `byte_len` bytes (ArrayBuffer backing
/// store). Non-positive lengths yield the empty binary.
@external(erlang, "arc_typed_array_ffi", "ta_zeroed")
pub fn ta_zeroed(byte_len: Int) -> BitArray

/// Replace `byte_size(region)` bytes of `data` at `byte_off` with `region`
/// in ONE rebuild. PRIVATE: the Erlang clause `badmatch`es when the region
/// runs off the end of `data`, and that precondition is enforced once, by
/// `splice_clamped` below, instead of by every call site.
@external(erlang, "arc_typed_array_ffi", "ta_splice")
fn ta_splice(data: BitArray, byte_off: Int, region: BitArray) -> BitArray

/// The single-pass primitive for bulk typed-array writes (fill/set/slice/
/// sort/copyWithin): splice as much of `region` into `data` at `byte_off` as
/// actually FITS, and report how many bytes were written.
///
/// Every typed-array bulk write is already an out-of-bounds silent no-op past
/// the live buffer, so clamping — rather than crashing — is the semantics the
/// call sites want; they used to hand-write the clamp-and-size dance
/// themselves and a miss was a BEAM `badmatch` inside the FFI. Callers still
/// clamp `region` to whole ELEMENTS beforehand (byte-granularity truncation
/// would write a partial element); this is the byte-granularity backstop that
/// makes the FFI's precondition unbreakable.
pub fn splice_clamped(
  data: BitArray,
  byte_off: Int,
  region: BitArray,
) -> #(BitArray, Int) {
  let capacity = bit_array.byte_size(data) - byte_off
  let written = int.min(bit_array.byte_size(region), int.max(capacity, 0))
  case byte_off < 0 || written <= 0 {
    True -> #(data, 0)
    False -> {
      let region = case written == bit_array.byte_size(region) {
        True -> region
        False -> bit_array.slice(region, 0, written) |> result.unwrap(<<>>)
      }
      #(ta_splice(data, byte_off, region), written)
    }
  }
}

/// Write `count` copies of the encoded element `elem` at `byte_off`.
/// O(byte_size(data) + count * elem) — one rebuild for the whole fill.
@external(erlang, "arc_typed_array_ffi", "ta_fill_region")
pub fn ta_fill_region(
  data: BitArray,
  byte_off: Int,
  count: Int,
  elem: BitArray,
) -> BitArray

/// Byte width of an integer element.
pub fn int_elem_size(elem: IntElem) -> Int {
  case elem {
    I8 | U8 -> 1
    I16 | U16 -> 2
    I32 | U32 -> 4
    I64 | U64 -> 8
  }
}

/// Bit width of an integer element — the modulus exponent for the two's
/// complement wrap the codecs apply.
pub fn int_elem_bits(elem: IntElem) -> Int {
  int_elem_size(elem) * 8
}

/// Whether reads of this element sign-extend.
pub fn int_elem_signed(elem: IntElem) -> Bool {
  case elem {
    I8 | I16 | I32 | I64 -> True
    U8 | U16 | U32 | U64 -> False
  }
}

/// Read a little-endian integer element.
@external(erlang, "arc_typed_array_ffi", "ta_get_int")
pub fn ta_get_int(data: BitArray, byte_off: Int, elem: IntElem) -> Int

/// Write a little-endian integer element. Erlang truncates `val` mod
/// 2^bits when encoding, which is exactly the ToInt8/ToUint32/... wrap
/// semantics.
@external(erlang, "arc_typed_array_ffi", "ta_set_int")
pub fn ta_set_int(
  data: BitArray,
  byte_off: Int,
  elem: IntElem,
  val: Int,
) -> BitArray

/// Read a float element (§25.1.2.10 GetValueFromBuffer). Returns the
/// `value.JsNum` directly — NaN and the infinities come back as their own
/// constructors, so callers cannot forget them.
@external(erlang, "arc_typed_array_ffi", "ta_get_float")
pub fn ta_get_float(data: BitArray, byte_off: Int, elem: FloatElem) -> JsNum

/// Write a float element (§25.1.2.12 SetValueInBuffer). Finite values that
/// overflow the 32-bit range round to the correctly-signed infinity
/// (IEEE 754 round-to-nearest), matching Float32Array store semantics.
@external(erlang, "arc_typed_array_ffi", "ta_set_float")
pub fn ta_set_float(
  data: BitArray,
  byte_off: Int,
  elem: FloatElem,
  val: JsNum,
) -> BitArray

/// ES2024 §7.1.12 ToUint8Clamp: clamp to [0,255] with round-half-to-EVEN.
/// NaN → 0, +Infinity → 255, -Infinity → 0.
@external(erlang, "arc_typed_array_ffi", "ta_clamp_uint8")
pub fn ta_clamp_uint8(val: JsNum) -> Int
