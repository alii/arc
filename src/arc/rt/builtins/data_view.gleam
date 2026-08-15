//// ES2024 §25.3 DataView Objects
////
//// A DataView is a byte-level accessor over an ArrayBuffer (or
//// SharedArrayBuffer). It carries [[ViewedArrayBuffer]], [[ByteOffset]] and
//// [[ByteLength]] internal slots; all get*/set* methods funnel through
//// GetViewValue / SetViewValue (§25.3.1.1 / §25.3.1.2).
////
//// Numeric encode/decode uses BEAM bit syntax. Float32/Float64 route through
//// the ONE JsNum ↔ IEEE-754-bits codec in `arc/rt/typed_array_ffi`
//// (`f32_bits`/`f64_bits` and their `decode_*` inverses) so the NaN/±Infinity
//// bit constants live in exactly one place. Float16 is decoded/encoded
//// manually (sign/exp/mantissa) because BEAM bit syntax has no 16-bit float
//// segment.

import arc/rt/buffer
import arc/rt/builtins/common
import arc/rt/builtins/helpers.{arg_at, first_arg_or_undefined}
import arc/rt/builtins/realm_ops
import arc/rt/call as rt_call
import arc/rt/typed_array_ffi.{splice_clamped}
import arc/rt/types.{
  type Agent, type BuiltinPair, type DataViewNative, type Handle, type JsNum,
  type JsVal, type ObjKind, type ViewBigElement, type ViewElementType,
  type ViewNumElement, DataViewConstructor, DataViewGet, DataViewGetBuffer,
  DataViewGetByteLength, DataViewGetByteOffset, DataViewN, DataViewObj,
  DataViewSet, Detached, JFloat, JInt, JNan, JNegInf, JPosInf, KHandle, KUndef,
  VBig, VBigInt64, VBigUint64, VFloat16, VFloat32, VFloat64, VInt16, VInt32,
  VInt8, VNum, VUint16, VUint32, VUint8, classify, mk_bigint, mk_number,
  mk_object, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/bit_array
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

/// Set up DataView.prototype and the DataView constructor.
pub fn init(
  st: Agent,
  object_proto: Handle,
  function_proto: Handle,
) -> #(BuiltinPair, Agent) {
  let #(getters, st) =
    common.alloc_getters(st, function_proto, [
      #("buffer", DataViewN(DataViewGetBuffer)),
      #("byteLength", DataViewN(DataViewGetByteLength)),
      #("byteOffset", DataViewN(DataViewGetByteOffset)),
    ])
  let #(methods, st) =
    common.alloc_methods(st, function_proto, [
      #("getInt8", DataViewN(DataViewGet(VNum(VInt8))), 1),
      #("getUint8", DataViewN(DataViewGet(VNum(VUint8))), 1),
      #("getInt16", DataViewN(DataViewGet(VNum(VInt16))), 1),
      #("getUint16", DataViewN(DataViewGet(VNum(VUint16))), 1),
      #("getInt32", DataViewN(DataViewGet(VNum(VInt32))), 1),
      #("getUint32", DataViewN(DataViewGet(VNum(VUint32))), 1),
      #("getFloat16", DataViewN(DataViewGet(VNum(VFloat16))), 1),
      #("getFloat32", DataViewN(DataViewGet(VNum(VFloat32))), 1),
      #("getFloat64", DataViewN(DataViewGet(VNum(VFloat64))), 1),
      #("getBigInt64", DataViewN(DataViewGet(VBig(VBigInt64))), 1),
      #("getBigUint64", DataViewN(DataViewGet(VBig(VBigUint64))), 1),
      #("setInt8", DataViewN(DataViewSet(VNum(VInt8))), 2),
      #("setUint8", DataViewN(DataViewSet(VNum(VUint8))), 2),
      #("setInt16", DataViewN(DataViewSet(VNum(VInt16))), 2),
      #("setUint16", DataViewN(DataViewSet(VNum(VUint16))), 2),
      #("setInt32", DataViewN(DataViewSet(VNum(VInt32))), 2),
      #("setUint32", DataViewN(DataViewSet(VNum(VUint32))), 2),
      #("setFloat16", DataViewN(DataViewSet(VNum(VFloat16))), 2),
      #("setFloat32", DataViewN(DataViewSet(VNum(VFloat32))), 2),
      #("setFloat64", DataViewN(DataViewSet(VNum(VFloat64))), 2),
      #("setBigInt64", DataViewN(DataViewSet(VBig(VBigInt64))), 2),
      #("setBigUint64", DataViewN(DataViewSet(VBig(VBigUint64))), 2),
    ])
  let proto_props = list.append(getters, methods)
  let #(bt, st) =
    common.init_type(
      st,
      object_proto,
      function_proto,
      proto_props,
      fn(proto) { DataViewN(DataViewConstructor(proto:)) },
      "DataView",
      1,
      [],
    )
  let st = common.add_to_string_tag(st, bt.prototype, "DataView")
  // §25.3.3.1: DataView.prototype is { writable: false, enumerable: false,
  // configurable: false } — installed that way by common.init_type.
  #(bt, st)
}

/// Per-module [[Call]] dispatch. `DataView()` without `new` throws
/// (§25.3.2.1 step 1).
pub fn dispatch(
  st: Agent,
  native: DataViewNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    DataViewConstructor(..) ->
      rt_val.t_throw_type_error(st, "Constructor DataView requires 'new'")
    DataViewGetBuffer -> get_buffer(st, this)
    DataViewGetByteLength -> get_byte_length(st, this)
    DataViewGetByteOffset -> get_byte_offset(st, this)
    DataViewGet(element) -> get_view_value(st, this, args, element)
    DataViewSet(element) -> set_view_value(st, this, args, element)
  }
}

/// Per-module [[Construct]] dispatch.
pub fn dispatch_construct(
  st: Agent,
  native: DataViewNative,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case native {
    DataViewConstructor(..) -> construct(st, args, new_target)
    _ -> rt_val.t_throw_type_error(st, "not a constructor")
  }
}

// ============================================================================
// §25.3.2.1 DataView ( buffer [ , byteOffset [ , byteLength ] ] )
// ============================================================================

fn construct(
  st: Agent,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  // Step 2: RequireInternalSlot(buffer, [[ArrayBufferData]])
  use buf_h <- helpers.some_or(
    as_array_buffer(st, first_arg_or_undefined(args)),
    fn() {
      rt_val.t_throw_type_error(
        st,
        "First argument to DataView constructor must be an ArrayBuffer",
      )
    },
  )
  // Step 3: offset = ToIndex(byteOffset)
  let #(offset, st) =
    rt_val.t_to_index(st, arg_at(args, 1), "Invalid DataView offset")
  // Step 4: re-check detached — ToIndex may have run user code.
  let #(buf_len, resizable) = live_buffer_info(st, buf_h)
  // Step 5-6: offset > bufferByteLength → RangeError
  use Nil <- helpers.guard(offset <= buf_len, fn() {
    rt_val.t_throw_range_error(
      st,
      "Start offset "
        <> int.to_string(offset)
        <> " is outside the bounds of the buffer",
    )
  })
  // Steps 8-10: resolve view byte length
  let len_arg = arg_at(args, 2)
  let #(view_len, st) = case classify(len_arg) {
    // byteLength absent: fixed buffer → span to end; resizable → auto-track.
    KUndef ->
      case resizable {
        False -> #(Some(buf_len - offset), st)
        True -> #(None, st)
      }
    _ -> {
      let #(view_len, st) =
        rt_val.t_to_index(st, len_arg, "Invalid DataView length")
      // Step 9.b: check against the buffer length captured BEFORE
      // ToIndex(byteLength) ran user code (a poisoned valueOf may have grown
      // a resizable buffer). The re-check below sees the fresh length
      // (step 14).
      use Nil <- helpers.guard(offset + view_len <= buf_len, fn() {
        rt_val.t_throw_range_error(st, "Invalid DataView length")
      })
      #(Some(view_len), st)
    }
  }
  // Step 11: OrdinaryCreateFromConstructor — reads NewTarget.prototype,
  // which may run user code.
  let #(proto, st) =
    rt_call.get_prototype_from_constructor(st, new_target, fn(r) {
      r.data_view.prototype
    })
  // Step 12-14: re-check detached and length against the CURRENT buffer.
  let #(buf_len, _) = live_buffer_info(st, buf_h)
  use Nil <- helpers.guard(
    case view_len {
      Some(l) -> offset + l <= buf_len
      None -> offset <= buf_len
    },
    fn() { rt_val.t_throw_range_error(st, "Invalid DataView length") },
  )
  realm_ops.alloc_wrapper(
    st,
    DataViewObj(buffer: buf_h, byte_offset: offset, byte_length: view_len),
    proto,
  )
}

// ============================================================================
// §25.3.4.1-3 prototype accessors: buffer / byteLength / byteOffset
// ============================================================================

fn get_buffer(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let view = require_data_view(st, this)
  #(mk_object(view.buffer), st)
}

fn get_byte_length(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let view = require_data_view(st, this)
  #(mk_number(JInt(view_size(st, view))), st)
}

fn get_byte_offset(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let view = require_data_view(st, this)
  let _size = view_size(st, view)
  #(mk_number(JInt(view.byte_offset)), st)
}

// ============================================================================
// §25.3.1.1 GetViewValue ( view, requestIndex, isLittleEndian, type )
// ============================================================================

fn get_view_value(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  element: ViewElementType,
) -> #(JsVal, Agent) {
  let #(view, get_index, st) = view_and_index(st, this, args)
  let little = rt_val.to_boolean(arg_at(args, 1))
  let elem_size = element_size(element)
  let #(data, pos) = checked_view_bytes(st, view, get_index, elem_size)
  let assert Ok(chunk) = bit_array.slice(data, pos, elem_size)
    as "data_view: checked_view_bytes let slice run past buffer"
  #(decode(element, chunk, little), st)
}

// ============================================================================
// §25.3.1.2 SetViewValue ( view, requestIndex, isLittleEndian, type, value )
// ============================================================================

fn set_view_value(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  element: ViewElementType,
) -> #(JsVal, Agent) {
  // SetViewValue step 3 (immutable-arraybuffer proposal): an immutable
  // viewed buffer is a TypeError BEFORE the ToIndex/ToNumber coercions run
  // any user code (observable; test262 checks it).
  let view0 = require_data_view(st, this)
  require_mutable_buffer(st, view0.buffer)
  let #(view, get_index, st) = view_and_index(st, this, args)
  // Step 3: numberValue = ToBigInt(value) / ToNumber(value) — spec-mandated
  // BEFORE the bounds check, so it cannot fold into checked_view_bytes.
  let #(encoded, st) = encode_value(st, element, arg_at(args, 1))
  let little = rt_val.to_boolean(arg_at(args, 2))
  let elem_size = element_size(element)
  let #(data, pos) = checked_view_bytes(st, view, get_index, elem_size)
  let chunk = to_endian(encoded, little, elem_size)
  let #(new_data, written) = splice_clamped(data, pos, chunk)
  #(
    mk_undefined(),
    buffer.store_region(st, view.buffer, new_data, pos, written),
  )
}

// ============================================================================
// Internal helpers — receiver/buffer validation
// ============================================================================

/// The [[DataView]] internal slots we operate on.
type ViewRecord {
  ViewRecord(buffer: Handle, byte_offset: Int, byte_length: Option(Int))
}

/// Unwrap `this` as a DataView or throw TypeError.
fn require_data_view(st: Agent, this: JsVal) -> ViewRecord {
  case helpers.brand_of(st, this, view_record_of) {
    Some(#(view, _ref)) -> view
    None ->
      rt_val.t_throw_type_error(
        st,
        "Method called on incompatible receiver: expected a DataView",
      )
  }
}

/// The [[DataView]] extractor handed to `brand_of` — a named function (not
/// an inline lambda) so the brand check builds no closure per call.
fn view_record_of(kind: ObjKind) -> Option(ViewRecord) {
  case kind {
    DataViewObj(buffer:, byte_offset:, byte_length:) ->
      Some(ViewRecord(buffer:, byte_offset:, byte_length:))
    _ -> None
  }
}

/// Immutable ArrayBuffer proposal — SetViewValue step 3: writes through a
/// DataView over an immutable buffer throw TypeError.
fn require_mutable_buffer(st: Agent, buf: Handle) -> Nil {
  case buffer.buffer_is_immutable(st, buf) {
    True ->
      rt_val.t_throw_type_error(
        st,
        "Cannot modify a DataView backed by an immutable ArrayBuffer",
      )
    False -> Nil
  }
}

/// Shared prologue of Get/SetViewValue (§25.3.1.1-2 steps 1-2): unwrap the
/// DataView receiver, then getIndex = ToIndex(requestIndex).
fn view_and_index(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(ViewRecord, Int, Agent) {
  let view = require_data_view(st, this)
  let #(get_index, st) =
    rt_val.t_to_index(
      st,
      first_arg_or_undefined(args),
      "Invalid DataView offset",
    )
  #(view, get_index, st)
}

/// Shared epilogue of Get/SetViewValue (steps 5+): out-of-bounds (incl.
/// detached) → TypeError, then RangeError; yields the live buffer bytes and
/// the absolute byte position of the element.
fn checked_view_bytes(
  st: Agent,
  view: ViewRecord,
  get_index: Int,
  elem_size: Int,
) -> #(BitArray, Int) {
  let size = view_size(st, view)
  use Nil <- helpers.guard(get_index + elem_size <= size, fn() {
    rt_val.t_throw_range_error(
      st,
      "Offset is outside the bounds of the DataView",
    )
  })
  let data = buffer_data(st, view.buffer)
  #(data, view.byte_offset + get_index)
}

/// Read `val` as an ArrayBuffer/SharedArrayBuffer handle ([[ArrayBufferData]]).
fn as_array_buffer(st: Agent, val: JsVal) -> Option(Handle) {
  case classify(val) {
    KHandle(h) ->
      case buffer.buffer_storage(st, h) {
        Some(_) -> Some(h)
        None -> None
      }
    _ -> None
  }
}

/// Read the live (non-detached) buffer's #(byte_length, resizable) or throw
/// TypeError if detached / not a buffer.
fn live_buffer_info(st: Agent, buf: Handle) -> #(Int, Bool) {
  case buffer.buffer_storage(st, buf) {
    // `Detached` is a detached buffer — [[ArrayBufferData]] is null.
    Some(Detached(..)) ->
      rt_val.t_throw_type_error(
        st,
        "Cannot perform operation on a detached ArrayBuffer",
      )
    Some(storage) -> #(
      types.buffer_byte_size(storage),
      option.is_some(types.buffer_max_byte_length(storage)),
    )
    None ->
      rt_val.t_throw_type_error(st, "DataView buffer is not an ArrayBuffer")
  }
}

/// Read the live buffer's data BitArray (TypeError if detached).
fn buffer_data(st: Agent, buf: Handle) -> BitArray {
  case buffer.buffer_bytes(st, buf) {
    Some(bits) -> bits
    None ->
      rt_val.t_throw_type_error(
        st,
        "Cannot perform operation on a detached ArrayBuffer",
      )
  }
}

/// GetViewByteLength + IsViewOutOfBounds (§25.3.1.1-25.3.1.3 helpers):
/// detached or out-of-bounds (resizable buffer shrunk under the view) →
/// TypeError; otherwise the current view size in bytes.
fn view_size(st: Agent, view: ViewRecord) -> Int {
  let #(buf_len, _resizable) = live_buffer_info(st, view.buffer)
  case view.byte_length {
    Some(len) ->
      case view.byte_offset + len <= buf_len {
        True -> len
        False ->
          rt_val.t_throw_type_error(
            st,
            "DataView is outside the bounds of its buffer",
          )
      }
    None ->
      case view.byte_offset <= buf_len {
        True -> buf_len - view.byte_offset
        False ->
          rt_val.t_throw_type_error(
            st,
            "DataView is outside the bounds of its buffer",
          )
      }
  }
}

// ============================================================================
// Element encode/decode
// ============================================================================

/// Table 71 element sizes, in bytes.
fn element_size(element: ViewElementType) -> Int {
  case element {
    VNum(VInt8) | VNum(VUint8) -> 1
    VNum(VInt16) | VNum(VUint16) | VNum(VFloat16) -> 2
    VNum(VInt32) | VNum(VUint32) | VNum(VFloat32) -> 4
    VNum(VFloat64) | VBig(VBigInt64) | VBig(VBigUint64) -> 8
  }
}

/// Read the chunk's bytes as an unsigned big/little-endian integer.
fn read_uint(chunk: BitArray, little: Bool) -> Int {
  case little, chunk {
    _, <<v:size(8)>> -> v
    True, <<v:size(16)-little>> -> v
    False, <<v:size(16)>> -> v
    True, <<v:size(32)-little>> -> v
    False, <<v:size(32)>> -> v
    True, <<v:size(64)-little>> -> v
    False, <<v:size(64)>> -> v
    // The chunk width always matches element_size; anything else would decode
    // as a bogus 0 rather than the bytes actually in the buffer.
    _, _ -> panic as "data_view: element chunk is not 1, 2, 4 or 8 bytes wide"
  }
}

/// Reinterpret an unsigned integer of `bits` width as two's-complement.
fn to_signed(u: Int, bits: Int) -> Int {
  let half = int.bitwise_shift_left(1, bits - 1)
  case u >= half {
    True -> u - int.bitwise_shift_left(half, 1)
    False -> u
  }
}

/// RawBytesToNumeric (§25.1.2.13): decode element bytes to a JsVal.
fn decode(element: ViewElementType, chunk: BitArray, little: Bool) -> JsVal {
  let u = read_uint(chunk, little)
  case element {
    VNum(e) -> decode_number(e, u)
    VBig(e) -> decode_bigint(e, u)
  }
}

/// RawBytesToNumeric for the Number-valued elements.
fn decode_number(element: ViewNumElement, u: Int) -> JsVal {
  case element {
    VUint8 -> mk_number(JInt(u))
    VUint16 -> mk_number(JInt(u))
    VUint32 -> mk_number(JInt(u))
    VInt8 -> mk_number(JInt(to_signed(u, 8)))
    VInt16 -> mk_number(JInt(to_signed(u, 16)))
    VInt32 -> mk_number(JInt(to_signed(u, 32)))
    VFloat16 -> mk_number(f16_from_bits(u))
    VFloat32 -> mk_number(typed_array_ffi.decode_f32_bits(u))
    VFloat64 -> mk_number(typed_array_ffi.decode_f64_bits(u))
  }
}

/// RawBytesToNumeric for the BigInt-valued elements.
fn decode_bigint(element: ViewBigElement, u: Int) -> JsVal {
  case element {
    VBigUint64 -> mk_bigint(u)
    VBigInt64 -> mk_bigint(to_signed(u, 64))
  }
}

/// Decode IEEE 754 binary16 bits manually (1 sign, 5 exponent, 10 mantissa).
fn f16_from_bits(u: Int) -> JsNum {
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
    // Multiply (not subtract from 0.0) so that -0.0 is produced for f = 0.0.
    _ -> f *. -1.0
  }
}

/// 2^e as a Float for the small exponent range half-floats need.
fn pow2(e: Int) -> Float {
  // Total for e in [-24, 5]: the base is positive.
  let assert Ok(f) = float.power(2.0, int.to_float(e))
    as "data_view: 2^e is undefined"
  f
}

/// Coerce + encode the value for SetViewValue. Produces the element's raw
/// bytes in BIG-endian order (to_endian flips later if needed).
fn encode_value(
  st: Agent,
  element: ViewElementType,
  val: JsVal,
) -> #(BitArray, Agent) {
  case element {
    VBig(e) -> {
      let #(n, st) = rt_val.t_to_bigint(st, val)
      #(encode_bigint(e, n), st)
    }
    VNum(e) -> {
      let #(num, st) = rt_val.t_to_number(st, val)
      #(encode_number(e, num), st)
    }
  }
}

/// NumericToRawBytes (§25.1.2.14) for the BigInt types, big-endian.
fn encode_bigint(element: ViewBigElement, n: Int) -> BitArray {
  case element {
    // ToBigInt64 and ToBigUint64 both reduce modulo 2^64, and Erlang bit
    // construction wraps to that same 64-bit two's-complement pattern.
    VBigInt64 | VBigUint64 -> <<n:size(64)>>
  }
}

/// NumericToRawBytes (§25.1.2.14) for the Number types, big-endian.
fn encode_number(element: ViewNumElement, num: JsNum) -> BitArray {
  case element {
    VInt8 | VUint8 -> <<to_int_wrap(num):size(8)>>
    VInt16 | VUint16 -> <<to_int_wrap(num):size(16)>>
    VInt32 | VUint32 -> <<to_int_wrap(num):size(32)>>
    VFloat64 -> <<typed_array_ffi.f64_bits(num):size(64)>>
    VFloat32 -> <<typed_array_ffi.f32_bits(num):size(32)>>
    VFloat16 -> <<f16_to_bits(num):size(16)>>
  }
}

/// ToIntN/ToUintN truncation step: NaN/±Infinity → 0, else truncate toward
/// zero. Modulo wrapping is left to Erlang bit-syntax construction.
fn to_int_wrap(num: JsNum) -> Int {
  case num {
    JInt(i) -> i
    JFloat(f) -> rt_val.float_to_int(f)
    JNan | JPosInf | JNegInf -> 0
  }
}

/// Encode a Number to IEEE 754 binary16 bits with round-to-nearest-even.
/// Works on the exact binary64 bit pattern so no double rounding occurs.
fn f16_to_bits(num: JsNum) -> Int {
  case num {
    JNan -> 0x7E00
    JPosInf -> 0x7C00
    JNegInf -> 0xFC00
    // `num_from_int` yields the correctly-rounded double (or ±Infinity past
    // its range) and never a JInt, so this recurs exactly once.
    JInt(i) -> f16_to_bits(rt_val.num_from_int(i))
    JFloat(f) -> {
      let assert <<b:size(64)>> = <<f:float-size(64)>>
        as "data_view: 64-bit float is not 64 bits wide"
      let sign_bits = int.bitwise_shift_left(int.bitwise_shift_right(b, 63), 15)
      let exp = int.bitwise_and(int.bitwise_shift_right(b, 52), 0x7FF)
      let mant = int.bitwise_and(b, 0xFFFFFFFFFFFFF)
      // Unbiased exponent for binary16: e16 = e64 - 1023 + 15
      let e16 = exp - 1008
      case e16 >= 0x1F, e16 >= 1 {
        // Overflow → ±Infinity (covers values ≥ 65520 after rounding via
        // the e16 == 0x1E carry below; e16 ≥ 31 here is plain overflow).
        True, _ -> int.bitwise_or(sign_bits, 0x7C00)
        False, True -> {
          // Normal range: keep top 10 mantissa bits, round ties-to-even on
          // the remaining 42, carry may bump exponent (and overflow to inf).
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
          // Subnormal or zero: value = (2^52 + mant) · 2^(exp-1075), target
          // grid is 2^-24. Shift = 42 + (1 - e16) extra bits dropped.
          let drop = 42 + 1 - e16
          case exp == 0 && mant == 0, drop > 63 {
            // ±0
            True, _ -> sign_bits
            // Too small to round up to the smallest subnormal
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
              // A carry out of the subnormal range lands exactly on the
              // smallest normal (exponent field becomes 1) — already correct.
              int.bitwise_or(sign_bits, rounded)
            }
          }
        }
      }
    }
  }
}

/// Flip a big-endian element chunk to the requested endianness.
fn to_endian(chunk: BitArray, little: Bool, size: Int) -> BitArray {
  case little, size {
    False, _ -> chunk
    True, 1 -> chunk
    True, _ ->
      case chunk {
        <<v:size(16)>> -> <<v:size(16)-little>>
        <<v:size(32)>> -> <<v:size(32)-little>>
        <<v:size(64)>> -> <<v:size(64)-little>>
        // Returning `chunk` unflipped here would silently store big-endian
        // bytes for a little-endian write.
        _ -> panic as "data_view: element chunk is not 2, 4 or 8 bytes wide"
      }
  }
}
