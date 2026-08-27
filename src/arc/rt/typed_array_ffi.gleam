import arc/rt/types.{
  type BigIntKind, type JsNum, type TypedArrayKind, BigInt64Kind, BigKind,
  BigUint64Kind, Float32Kind, Float64Kind, Int16Kind, Int32Kind, Int8Kind, JInt,
  NumKind, Uint16Kind, Uint32Kind, Uint8ClampedKind, Uint8Kind,
}
import arc/rt/val as rt_val
import gleam/bit_array
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

@external(erlang, "arc_rt_typed_array_ffi", "ta_zeroed")
pub fn ta_zeroed(byte_len: Int) -> BitArray

@external(erlang, "arc_rt_typed_array_ffi", "ta_splice")
fn ta_splice(data: BitArray, byte_off: Int, region: BitArray) -> BitArray

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
      #(ta_splice(data, byte_off, region), written)
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

@external(erlang, "arc_rt_typed_array_ffi", "ta_get_int")
pub fn ta_get_int(data: BitArray, byte_off: Int, elem: IntElem) -> Int

// erlang wraps val mod 2^bits, matching toint8 etc
@external(erlang, "arc_rt_typed_array_ffi", "ta_set_int")
pub fn ta_set_int(
  data: BitArray,
  byte_off: Int,
  elem: IntElem,
  val: Int,
) -> BitArray

@external(erlang, "arc_rt_typed_array_ffi", "ta_get_float")
pub fn ta_get_float(data: BitArray, byte_off: Int, elem: FloatElem) -> JsNum

@external(erlang, "arc_rt_typed_array_ffi", "ta_set_float")
fn ffi_set_float(
  data: BitArray,
  byte_off: Int,
  elem: FloatElem,
  val: JsNum,
) -> BitArray

pub fn ta_set_float(
  data: BitArray,
  byte_off: Int,
  elem: FloatElem,
  val: JsNum,
) -> BitArray {
  ffi_set_float(data, byte_off, elem, as_double(val))
}

fn as_double(n: JsNum) -> JsNum {
  case n {
    JInt(i) -> rt_val.num_from_int(i)
    _ -> n
  }
}

// §7.1.12 touint8clamp, round half to even
@external(erlang, "arc_rt_typed_array_ffi", "ta_clamp_uint8")
pub fn ta_clamp_uint8(val: JsNum) -> Int

@external(erlang, "arc_rt_typed_array_ffi", "f32_bits")
fn ffi_f32_bits(n: JsNum) -> Int

pub fn f32_bits(n: JsNum) -> Int {
  ffi_f32_bits(as_double(n))
}

@external(erlang, "arc_rt_typed_array_ffi", "f64_bits")
fn ffi_f64_bits(n: JsNum) -> Int

pub fn f64_bits(n: JsNum) -> Int {
  ffi_f64_bits(as_double(n))
}

@external(erlang, "arc_rt_typed_array_ffi", "decode_f32_bits")
pub fn decode_f32_bits(bits: Int) -> JsNum

@external(erlang, "arc_rt_typed_array_ffi", "decode_f64_bits")
pub fn decode_f64_bits(bits: Int) -> JsNum
