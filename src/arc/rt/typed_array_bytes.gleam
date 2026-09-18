import arc/rt/types.{
  type BigIntKind, type JsNum, type TypedArrayKind, BigInt64Kind, BigKind,
  BigUint64Kind, Float32Kind, Float64Kind, Int16Kind, Int32Kind, Int8Kind,
  JFloat, JInt, JNan, JNegInf, JPosInf, NumKind, Uint16Kind, Uint32Kind,
  Uint8ClampedKind, Uint8Kind,
}
import arc/rt/val as rt_val
import gleam/bit_array
import gleam/float
import gleam/int

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

pub type FloatElem {
  F32
  F64
}

@external(erlang, "arc_rt_typed_array_bytes_ffi", "zeroed")
pub fn zeroed(byte_len: Int) -> BitArray

@external(erlang, "arc_rt_typed_array_bytes_ffi", "splice")
fn splice(data: BitArray, byte_off: Int, region: BitArray) -> BitArray

@external(erlang, "binary", "copy")
fn binary_copy(elem: BitArray, n: Int) -> BitArray

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
        // 0 < written < size here, must crash not splice nothing
        False -> {
          let assert Ok(region) = bit_array.slice(region, 0, written)
          region
        }
      }
      #(splice(data, byte_off, region), written)
    }
  }
}

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

pub type Elem {
  Int(IntElem)
  Float(FloatElem)
}

pub fn bigint_elem(kind: BigIntKind) -> IntElem {
  case kind {
    BigInt64Kind -> I64
    BigUint64Kind -> U64
  }
}

// read direction only, stores use store_elem_of_kind
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

pub type StoreElem {
  StoreInt(IntElem)
  StoreFloat(FloatElem)
  StoreClampedU8
}

pub fn store_elem_of_kind(kind: TypedArrayKind) -> StoreElem {
  case kind {
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

pub fn float_elem_size(elem: FloatElem) -> Int {
  case elem {
    F32 -> 4
    F64 -> 8
  }
}

pub fn elem_size(kind: TypedArrayKind) -> Int {
  case elem_of_kind(kind) {
    Int(e) -> int_elem_size(e)
    Float(e) -> float_elem_size(e)
  }
}

pub fn int_elem_size(elem: IntElem) -> Int {
  case elem {
    I8 | U8 -> 1
    I16 | U16 -> 2
    I32 | U32 -> 4
    I64 | U64 -> 8
  }
}

pub fn int_elem_bits(elem: IntElem) -> Int {
  int_elem_size(elem) * 8
}

pub fn int_elem_signed(elem: IntElem) -> Bool {
  case elem {
    I8 | I16 | I32 | I64 -> True
    U8 | U16 | U32 | U64 -> False
  }
}

@external(erlang, "arc_rt_typed_array_bytes_ffi", "get_int")
pub fn get_int(data: BitArray, byte_off: Int, elem: IntElem) -> Int

// erlang wraps val mod 2^bits, matching toint8 etc
@external(erlang, "arc_rt_typed_array_bytes_ffi", "set_int")
pub fn set_int(
  data: BitArray,
  byte_off: Int,
  elem: IntElem,
  val: Int,
) -> BitArray

@external(erlang, "arc_rt_typed_array_bytes_ffi", "get_float")
pub fn get_float(data: BitArray, byte_off: Int, elem: FloatElem) -> JsNum

@external(erlang, "arc_rt_typed_array_bytes_ffi", "set_double")
fn set_double(
  data: BitArray,
  byte_off: Int,
  elem: FloatElem,
  val: JsNum,
) -> BitArray

pub fn set_float(
  data: BitArray,
  byte_off: Int,
  elem: FloatElem,
  val: JsNum,
) -> BitArray {
  set_double(data, byte_off, elem, as_double(val))
}

fn as_double(n: JsNum) -> JsNum {
  case n {
    JInt(i) -> rt_val.num_from_int(i)
    _ -> n
  }
}

// §7.1.12 touint8clamp, round half to even
@external(erlang, "arc_rt_typed_array_bytes_ffi", "clamp_uint8")
pub fn clamp_uint8(val: JsNum) -> Int

@external(erlang, "arc_rt_typed_array_bytes_ffi", "f32_bits_of_double")
fn f32_bits_of_double(n: JsNum) -> Int

pub fn f32_bits(n: JsNum) -> Int {
  f32_bits_of_double(as_double(n))
}

@external(erlang, "arc_rt_typed_array_bytes_ffi", "f64_bits_of_double")
fn f64_bits_of_double(n: JsNum) -> Int

pub fn f64_bits(n: JsNum) -> Int {
  f64_bits_of_double(as_double(n))
}

@external(erlang, "arc_rt_typed_array_bytes_ffi", "decode_f32_bits")
pub fn decode_f32_bits(bits: Int) -> JsNum

@external(erlang, "arc_rt_typed_array_bytes_ffi", "decode_f64_bits")
pub fn decode_f64_bits(bits: Int) -> JsNum

// binary16: 1 sign, 5 exponent, 10 mantissa
pub fn decode_f16_bits(u: Int) -> JsNum {
  let sign = int.bitwise_shift_right(u, 15)
  let exp = int.bitwise_and(int.bitwise_shift_right(u, 10), 0x1F)
  let mant = int.bitwise_and(u, 0x3FF)
  case exp {
    0x1F ->
      case mant == 0, sign == 0 {
        True, True -> JPosInf
        True, False -> JNegInf
        False, _ -> JNan
      }
    0 -> JFloat(apply_sign(int.to_float(mant) *. pow2(-24), sign))
    _ -> JFloat(apply_sign(int.to_float(1024 + mant) *. pow2(exp - 25), sign))
  }
}

fn apply_sign(f: Float, sign: Int) -> Float {
  case sign {
    0 -> f
    _ -> f *. -1.0
  }
}

fn pow2(e: Int) -> Float {
  let assert Ok(f) = float.power(2.0, int.to_float(e))
    as "data_view: 2^e is undefined"
  f
}

// round to nearest even from the binary64 bits
pub fn f16_bits(num: JsNum) -> Int {
  case num {
    JNan -> 0x7E00
    JPosInf -> 0x7C00
    JNegInf -> 0xFC00
    JInt(i) -> f16_bits(rt_val.num_from_int(i))
    JFloat(f) -> {
      let assert <<b:size(64)>> = <<f:float-size(64)>>
        as "data_view: 64-bit float is not 64 bits wide"
      let sign_bits = int.bitwise_shift_left(int.bitwise_shift_right(b, 63), 15)
      let exp = int.bitwise_and(int.bitwise_shift_right(b, 52), 0x7FF)
      let mant = int.bitwise_and(b, 0xFFFFFFFFFFFFF)
      let e16 = exp - 1008
      case e16 >= 0x1F, e16 >= 1 {
        True, _ -> int.bitwise_or(sign_bits, 0x7C00)
        False, True -> {
          let kept = int.bitwise_shift_right(mant, 42)
          let rest = int.bitwise_and(mant, 0x3FFFFFFFFFF)
          let half = 0x20000000000
          let rounded = case
            rest > half || { rest == half && int.is_odd(kept) }
          {
            True -> kept + 1
            False -> kept
          }
          let combined = int.bitwise_shift_left(e16, 10) + rounded
          case combined >= 0x7C00 {
            True -> int.bitwise_or(sign_bits, 0x7C00)
            False -> int.bitwise_or(sign_bits, combined)
          }
        }
        False, False -> {
          let drop = 42 + 1 - e16
          case exp == 0 && mant == 0, drop > 63 {
            True, _ -> sign_bits
            False, True -> sign_bits
            False, False -> {
              let full = int.bitwise_or(mant, 0x10000000000000)
              let kept = int.bitwise_shift_right(full, drop)
              let rest =
                int.bitwise_and(full, int.bitwise_shift_left(1, drop) - 1)
              let half = int.bitwise_shift_left(1, drop - 1)
              let rounded = case
                rest > half || { rest == half && int.is_odd(kept) }
              {
                True -> kept + 1
                False -> kept
              }
              int.bitwise_or(sign_bits, rounded)
            }
          }
        }
      }
    }
  }
}
