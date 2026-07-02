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

/// Allocate an all-zero binary of `byte_len` bytes (ArrayBuffer backing
/// store). Non-positive lengths yield the empty binary.
@external(erlang, "arc_typed_array_ffi", "ta_zeroed")
pub fn ta_zeroed(byte_len: Int) -> BitArray

/// Replace `byte_size(region)` bytes of `data` at `byte_off` with `region`
/// in ONE rebuild — the single-pass primitive for bulk typed-array writes
/// (fill/set/slice). Caller guarantees the region fits inside `data`.
@external(erlang, "arc_typed_array_ffi", "ta_splice")
pub fn ta_splice(data: BitArray, byte_off: Int, region: BitArray) -> BitArray

/// Write `count` copies of the encoded element `elem` at `byte_off`.
/// O(byte_size(data) + count * elem) — one rebuild for the whole fill.
@external(erlang, "arc_typed_array_ffi", "ta_fill_region")
pub fn ta_fill_region(
  data: BitArray,
  byte_off: Int,
  count: Int,
  elem: BitArray,
) -> BitArray

/// Read a little-endian integer element. `size_bits` in {8,16,32,64}.
@external(erlang, "arc_typed_array_ffi", "ta_get_int")
pub fn ta_get_int(
  data: BitArray,
  byte_off: Int,
  size_bits: Int,
  signed: Bool,
) -> Int

/// Write a little-endian integer element. Erlang truncates `val` mod
/// 2^size_bits when encoding, which is exactly the ToInt8/ToUint32/... wrap
/// semantics.
@external(erlang, "arc_typed_array_ffi", "ta_set_int")
pub fn ta_set_int(
  data: BitArray,
  byte_off: Int,
  size_bits: Int,
  val: Int,
) -> BitArray

/// Read a float element (§25.1.2.10 GetValueFromBuffer). `size_bits` is 32
/// or 64. Returns the `value.JsNum` directly — NaN and the infinities come
/// back as their own constructors, so callers cannot forget them.
@external(erlang, "arc_typed_array_ffi", "ta_get_float")
pub fn ta_get_float(data: BitArray, byte_off: Int, size_bits: Int) -> JsNum

/// Write a float element (§25.1.2.12 SetValueInBuffer). Finite values that
/// overflow the 32-bit range round to the correctly-signed infinity
/// (IEEE 754 round-to-nearest), matching Float32Array store semantics.
@external(erlang, "arc_typed_array_ffi", "ta_set_float")
pub fn ta_set_float(
  data: BitArray,
  byte_off: Int,
  size_bits: Int,
  val: JsNum,
) -> BitArray

/// ES2024 §7.1.12 ToUint8Clamp: clamp to [0,255] with round-half-to-EVEN.
/// NaN → 0, +Infinity → 255, -Infinity → 0.
@external(erlang, "arc_typed_array_ffi", "ta_clamp_uint8")
pub fn ta_clamp_uint8(val: JsNum) -> Int
