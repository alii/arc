//// The ONE binding surface for `arc_rt_typed_array_ffi.erl`.
////
//// Every `@external` declaration for that Erlang module lives here and
//// nowhere else, so a signature exists exactly once and the compiler
//// checks every caller against it.
////
//// It also owns the ONE `TypedArrayKind -> element codec` mapping — read
//// direction (`elem_of_kind`), store direction (`store_elem_of_kind`) — and
//// the byte width derived from it (`elem_size`), so a new element kind cannot
//// be given a codec in one table and a width in another.
////
//// Float elements speak `types.JsNum` directly. There is no int-tag side
//// channel: the compiler forces every caller to handle the non-finite
//// constructors.

import arc/rt/types.{
  type BigIntKind, type JsNum, type TypedArrayKind, BigInt64Kind, BigKind,
  BigUint64Kind, Float32Kind, Float64Kind, Int16Kind, Int32Kind, Int8Kind, JInt,
  NumKind, Uint16Kind, Uint32Kind, Uint8ClampedKind, Uint8Kind,
}
import arc/rt/val as rt_val
import gleam/bit_array
import gleam/int

/// The integer element widths+signednesses the codecs implement, as one
/// closed set, so every call names an element the Erlang side has a clause
/// for.
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
@external(erlang, "arc_rt_typed_array_ffi", "ta_zeroed")
pub fn ta_zeroed(byte_len: Int) -> BitArray

/// Replace `byte_size(region)` bytes of `data` at `byte_off` with `region`
/// in ONE rebuild. PRIVATE: the Erlang clause `badmatch`es when the region
/// runs off the end of `data`, and that precondition is enforced once, by
/// `splice_clamped` below, instead of by every call site.
@external(erlang, "arc_rt_typed_array_ffi", "ta_splice")
fn ta_splice(data: BitArray, byte_off: Int, region: BitArray) -> BitArray

/// `binary:copy/2` — `n` concatenated copies of `elem`.
@external(erlang, "binary", "copy")
fn binary_copy(elem: BitArray, n: Int) -> BitArray

/// The single-pass primitive for bulk typed-array writes (fill/set/slice/
/// sort/copyWithin): splice as much of `region` into `data` at `byte_off` as
/// actually FITS, and report how many bytes were written.
///
/// Every typed-array bulk write is already an out-of-bounds silent no-op past
/// the live buffer, so clamping — rather than crashing — is the semantics the
/// call sites want. Callers still clamp `region` to whole ELEMENTS
/// beforehand (byte-granularity truncation would write a partial element);
/// this is the byte-granularity backstop that makes the FFI's precondition
/// unbreakable.
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
        // Invariant on this branch: 0 < written < byte_size(region), so the
        // slice CANNOT fail. Silently substituting `<<>>` here would splice
        // nothing yet still report `written` bytes, so a broken invariant
        // must crash instead.
        False -> {
          let assert Ok(region) = bit_array.slice(region, 0, written)
          region
        }
      }
      #(ta_splice(data, byte_off, region), written)
    }
  }
}

/// Write as many of `count` copies of the encoded element `elem` at
/// `byte_off` as FIT in the live buffer, and report the bytes written — the
/// fill sibling of `splice_clamped`, and the only public path to a bulk fill.
/// O(byte_size(data) + count * elem): one region build, one rebuild.
pub fn fill_clamped(
  data: BitArray,
  byte_off: Int,
  count: Int,
  elem: BitArray,
) -> #(BitArray, Int) {
  case count <= 0 {
    True -> #(data, 0)
    False -> splice_clamped(data, byte_off, binary_copy(elem, count))
  }
}

/// Which FFI codec an element kind speaks: an integer element (widths and
/// signednesses per `IntElem`) or a float element. The ONE place the
/// `TypedArrayKind -> codec` mapping lives; `elem_size` derives the byte
/// width from it, so the width table cannot drift from the codec table.
pub type Elem {
  Int(IntElem)
  Float(FloatElem)
}

/// The codec for a BigInt content type — always a 64-bit integer element.
pub fn bigint_elem(kind: BigIntKind) -> IntElem {
  case kind {
    BigInt64Kind -> I64
    BigUint64Kind -> U64
  }
}

/// The codec for any typed-array element kind. Total: adding a kind is a
/// compile error here rather than a wrong-width read somewhere downstream.
///
/// READ direction only. Uint8Clamped decodes exactly like Uint8 (`U8`); its
/// *store* path differs (§7.1.12 ToUint8Clamp), so stores must go through
/// `store_elem_of_kind` instead — never through this table.
pub fn elem_of_kind(kind: TypedArrayKind) -> Elem {
  case kind {
    NumKind(Int8Kind) -> Int(I8)
    NumKind(Uint8Kind) | NumKind(Uint8ClampedKind) -> Int(U8)
    NumKind(Int16Kind) -> Int(I16)
    NumKind(Uint16Kind) -> Int(U16)
    NumKind(Int32Kind) -> Int(I32)
    NumKind(Uint32Kind) -> Int(U32)
    NumKind(Float32Kind) -> Float(F32)
    NumKind(Float64Kind) -> Float(F64)
    BigKind(k) -> Int(bigint_elem(k))
  }
}

/// Which codec a *store* into an element kind speaks. Deliberately NOT the
/// same type as `Elem`: Uint8Clamped reads as `Int(U8)` but writes through
/// §7.1.12 ToUint8Clamp. A store site that reaches for the codec gets a
/// `StoreClampedU8` it must handle, so an unclamped Uint8Clamped store
/// cannot type-check.
pub type StoreElem {
  StoreInt(IntElem)
  StoreFloat(FloatElem)
  StoreClampedU8
}

/// The codec a store into `kind` speaks (§25.1.2.12 SetValueInBuffer). Total,
/// like `elem_of_kind`, and spelled out kind by kind rather than delegating
/// with a `_` arm, so a future kind whose store diverges from its read codec
/// has to be classified rather than silently inheriting `StoreInt`.
pub fn store_elem_of_kind(kind: TypedArrayKind) -> StoreElem {
  case kind {
    // The ONE kind whose store differs from its read codec.
    NumKind(Uint8ClampedKind) -> StoreClampedU8
    NumKind(Int8Kind) -> StoreInt(I8)
    NumKind(Uint8Kind) -> StoreInt(U8)
    NumKind(Int16Kind) -> StoreInt(I16)
    NumKind(Uint16Kind) -> StoreInt(U16)
    NumKind(Int32Kind) -> StoreInt(I32)
    NumKind(Uint32Kind) -> StoreInt(U32)
    NumKind(Float32Kind) -> StoreFloat(F32)
    NumKind(Float64Kind) -> StoreFloat(F64)
    BigKind(k) -> StoreInt(bigint_elem(k))
  }
}

/// Byte width of a float element.
pub fn float_elem_size(elem: FloatElem) -> Int {
  case elem {
    F32 -> 4
    F64 -> 8
  }
}

/// Element size in bytes — §23.2 Table 69, derived from `elem_of_kind` so
/// there is exactly one table.
pub fn elem_size(kind: TypedArrayKind) -> Int {
  case elem_of_kind(kind) {
    Int(e) -> int_elem_size(e)
    Float(e) -> float_elem_size(e)
  }
}

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
@external(erlang, "arc_rt_typed_array_ffi", "ta_get_int")
pub fn ta_get_int(data: BitArray, byte_off: Int, elem: IntElem) -> Int

/// Write a little-endian integer element. Erlang truncates `val` mod
/// 2^bits when encoding, which is exactly the ToInt8/ToUint32/... wrap
/// semantics.
@external(erlang, "arc_rt_typed_array_ffi", "ta_set_int")
pub fn ta_set_int(
  data: BitArray,
  byte_off: Int,
  elem: IntElem,
  val: Int,
) -> BitArray

/// Read a float element (§25.1.2.10 GetValueFromBuffer). Returns the
/// `JsNum` directly — NaN and the infinities come back as their own
/// constructors, so callers cannot forget them.
@external(erlang, "arc_rt_typed_array_ffi", "ta_get_float")
pub fn ta_get_float(data: BitArray, byte_off: Int, elem: FloatElem) -> JsNum

@external(erlang, "arc_rt_typed_array_ffi", "ta_set_float")
fn ffi_set_float(
  data: BitArray,
  byte_off: Int,
  elem: FloatElem,
  val: JsNum,
) -> BitArray

/// Write a float element (§25.1.2.12 SetValueInBuffer). Finite values that
/// overflow the 32-bit range round to the correctly-signed infinity
/// (IEEE 754 round-to-nearest), matching Float32Array store semantics.
pub fn ta_set_float(
  data: BitArray,
  byte_off: Int,
  elem: FloatElem,
  val: JsNum,
) -> BitArray {
  ffi_set_float(data, byte_off, elem, as_double(val))
}

/// An exact `JInt` as the double the rest of the runtime rounds it to
/// (`num_from_int`, ties-to-even past 2^53), so a stored integer reads back
/// as the same Number arithmetic would produce.
fn as_double(n: JsNum) -> JsNum {
  case n {
    JInt(i) -> rt_val.num_from_int(i)
    _ -> n
  }
}

/// ES2024 §7.1.12 ToUint8Clamp: clamp to [0,255] with round-half-to-EVEN.
/// NaN → 0, +Infinity → 255, -Infinity → 0.
@external(erlang, "arc_rt_typed_array_ffi", "ta_clamp_uint8")
pub fn ta_clamp_uint8(val: JsNum) -> Int

@external(erlang, "arc_rt_typed_array_ffi", "f32_bits")
fn ffi_f32_bits(n: JsNum) -> Int

/// Encode a `JsNum` as its IEEE 754 binary32 bit pattern (an integer). Finite
/// values that overflow the 32-bit range round to ±infinity's bits (BEAM's
/// native round-to-nearest). The ONE place the NaN/±Inf f32 constants live.
pub fn f32_bits(n: JsNum) -> Int {
  ffi_f32_bits(as_double(n))
}

@external(erlang, "arc_rt_typed_array_ffi", "f64_bits")
fn ffi_f64_bits(n: JsNum) -> Int

/// Encode a `JsNum` as its IEEE 754 binary64 bit pattern (an integer).
/// The ONE place the NaN/±Inf f64 constants live.
pub fn f64_bits(n: JsNum) -> Int {
  ffi_f64_bits(as_double(n))
}

/// Decode an IEEE 754 binary32 bit pattern (an integer) into a `JsNum`.
/// Inverse of `f32_bits`.
@external(erlang, "arc_rt_typed_array_ffi", "decode_f32_bits")
pub fn decode_f32_bits(bits: Int) -> JsNum

/// Decode an IEEE 754 binary64 bit pattern (an integer) into a `JsNum`.
/// Inverse of `f64_bits`.
@external(erlang, "arc_rt_typed_array_ffi", "decode_f64_bits")
pub fn decode_f64_bits(bits: Int) -> JsNum
