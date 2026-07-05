//// ES2024 §25.3 DataView Objects
////
//// A DataView is a byte-level accessor over an ArrayBuffer (or
//// SharedArrayBuffer). It carries [[ViewedArrayBuffer]], [[ByteOffset]] and
//// [[ByteLength]] internal slots; all get*/set* methods funnel through
//// GetViewValue / SetViewValue (§25.3.1.1 / §25.3.1.2).
////
//// Numeric encode/decode uses BEAM bit syntax. Erlang float segments only
//// match *finite* values, so NaN/Infinity decoding falls through to integer
//// bit-pattern inspection; encoding writes the canonical bit patterns.
//// Float16 is decoded/encoded manually (sign/exp/mantissa) because Gleam bit
//// arrays don't support 16-bit float segments.

import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers.{arg_at, first_arg_or_undefined}
import arc/vm/heap
import arc/vm/internal/typed_array_ffi.{splice_clamped}
import arc/vm/ops/buffer as ops_buffer
import arc/vm/ops/coerce
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type DataViewNativeFn, type JsValue, type Ref, type ViewBigElement,
  type ViewElementType, type ViewNumElement, ArrayBufferObject, BigInt,
  DataViewConstructor, DataViewGet, DataViewGetBuffer, DataViewGetByteLength,
  DataViewGetByteOffset, DataViewNative, DataViewObject, DataViewSet, Dispatch,
  Finite, Infinity, JsBigInt, JsNumber, JsObject, JsUndefined, NaN, NegInfinity,
  ObjectSlot, VBig, VBigInt64, VBigUint64, VFloat16, VFloat32, VFloat64, VInt16,
  VInt32, VInt8, VNum, VUint16, VUint32, VUint8,
}
import gleam/bit_array
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

/// Set up DataView.prototype and the DataView constructor.
pub fn init(
  h: Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(host), BuiltinType) {
  let #(h, getters) =
    common.alloc_getters(h, function_proto, [
      #("buffer", DataViewNative(DataViewGetBuffer)),
      #("byteLength", DataViewNative(DataViewGetByteLength)),
      #("byteOffset", DataViewNative(DataViewGetByteOffset)),
    ])
  let #(h, methods) =
    common.alloc_methods(h, function_proto, [
      #("getInt8", DataViewNative(DataViewGet(VNum(VInt8))), 1),
      #("getUint8", DataViewNative(DataViewGet(VNum(VUint8))), 1),
      #("getInt16", DataViewNative(DataViewGet(VNum(VInt16))), 1),
      #("getUint16", DataViewNative(DataViewGet(VNum(VUint16))), 1),
      #("getInt32", DataViewNative(DataViewGet(VNum(VInt32))), 1),
      #("getUint32", DataViewNative(DataViewGet(VNum(VUint32))), 1),
      #("getFloat16", DataViewNative(DataViewGet(VNum(VFloat16))), 1),
      #("getFloat32", DataViewNative(DataViewGet(VNum(VFloat32))), 1),
      #("getFloat64", DataViewNative(DataViewGet(VNum(VFloat64))), 1),
      #("getBigInt64", DataViewNative(DataViewGet(VBig(VBigInt64))), 1),
      #("getBigUint64", DataViewNative(DataViewGet(VBig(VBigUint64))), 1),
      #("setInt8", DataViewNative(DataViewSet(VNum(VInt8))), 2),
      #("setUint8", DataViewNative(DataViewSet(VNum(VUint8))), 2),
      #("setInt16", DataViewNative(DataViewSet(VNum(VInt16))), 2),
      #("setUint16", DataViewNative(DataViewSet(VNum(VUint16))), 2),
      #("setInt32", DataViewNative(DataViewSet(VNum(VInt32))), 2),
      #("setUint32", DataViewNative(DataViewSet(VNum(VUint32))), 2),
      #("setFloat16", DataViewNative(DataViewSet(VNum(VFloat16))), 2),
      #("setFloat32", DataViewNative(DataViewSet(VNum(VFloat32))), 2),
      #("setFloat64", DataViewNative(DataViewSet(VNum(VFloat64))), 2),
      #("setBigInt64", DataViewNative(DataViewSet(VBig(VBigInt64))), 2),
      #("setBigUint64", DataViewNative(DataViewSet(VBig(VBigUint64))), 2),
    ])
  let proto_props = list.append(getters, methods)
  let #(h, bt) =
    common.init_type(
      h,
      object_proto,
      function_proto,
      proto_props,
      fn(proto) { Dispatch(DataViewNative(DataViewConstructor(proto:))) },
      "DataView",
      1,
      [],
    )
  let h = common.add_to_string_tag(h, bt.prototype, "DataView")
  // §25.3.3.1: DataView.prototype is { writable: false, enumerable: false,
  // configurable: false } — installed that way by common.init_type.
  #(h, bt)
}

/// Per-module dispatch for DataView native functions.
pub fn dispatch(
  native: DataViewNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case native {
    DataViewConstructor(proto:) -> construct(proto, args, state)
    DataViewGetBuffer -> get_buffer(this, state)
    DataViewGetByteLength -> get_byte_length(this, state)
    DataViewGetByteOffset -> get_byte_offset(this, state)
    DataViewGet(element) -> get_view_value(this, args, element, state)
    DataViewSet(element) -> set_view_value(this, args, element, state)
  }
}

// ============================================================================
// §25.3.2.1 DataView ( buffer [ , byteOffset [ , byteLength ] ] )
// ============================================================================

fn construct(
  proto: Ref,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 1: NewTarget undefined → TypeError. do_construct sets
  // state.new_target before native dispatch; a plain call leaves JsUndefined.
  use Nil <- helpers.guard(state.new_target != JsUndefined, fn() {
    state.type_error(state, "Constructor DataView requires 'new'")
  })
  // Step 2: RequireInternalSlot(buffer, [[ArrayBufferData]])
  use buf_ref <- helpers.some_or(
    as_array_buffer(state, first_arg_or_undefined(args)),
    fn() {
      state.type_error(
        state,
        "First argument to DataView constructor must be an ArrayBuffer",
      )
    },
  )
  // Step 3: offset = ToIndex(byteOffset)
  use offset, state <- coerce.to_index_cps(
    state,
    arg_at(args, 1),
    "Invalid DataView offset",
  )
  // Step 4: re-check detached — ToIndex may have run user code.
  use #(buf_len, resizable), state <- live_buffer_info(state, buf_ref)
  // Step 5-6: offset > bufferByteLength → RangeError
  use Nil <- helpers.guard(offset <= buf_len, fn() {
    state.range_error(
      state,
      "Start offset "
        <> int.to_string(offset)
        <> " is outside the bounds of the buffer",
    )
  })
  // Steps 8-10: resolve view byte length
  let len_arg = arg_at(args, 2)
  let resolve = fn(view_len: Option(Int), state) {
    // Step 12: re-check detached (OrdinaryCreateFromConstructor can run user
    // code in full ES; ours cannot, but a poisoned ToIndex above already can).
    use #(buf_len, _), state <- live_buffer_info(state, buf_ref)
    use Nil <- helpers.guard(
      case view_len {
        Some(l) -> offset + l <= buf_len
        None -> offset <= buf_len
      },
      fn() { state.range_error(state, "Invalid DataView length") },
    )
    let #(heap, ref) =
      common.alloc_wrapper(
        state.heap,
        DataViewObject(
          buffer: buf_ref,
          byte_offset: offset,
          byte_length: view_len,
        ),
        proto,
      )
    #(State(..state, heap:), Ok(JsObject(ref)))
  }
  case len_arg {
    // byteLength absent: fixed buffer → span to end; resizable → auto-track.
    JsUndefined ->
      case resizable {
        False -> resolve(Some(buf_len - offset), state)
        True -> resolve(None, state)
      }
    _ -> {
      use view_len, state <- coerce.to_index_cps(
        state,
        len_arg,
        "Invalid DataView length",
      )
      // Step 9.b: check against the buffer length captured BEFORE
      // ToIndex(byteLength) ran user code (a poisoned valueOf may have grown
      // a resizable buffer). resolve() re-checks the fresh length (step 14).
      use Nil <- helpers.guard(offset + view_len <= buf_len, fn() {
        state.range_error(state, "Invalid DataView length")
      })
      resolve(Some(view_len), state)
    }
  }
}

// ============================================================================
// §25.3.4.1-3 prototype accessors: buffer / byteLength / byteOffset
// ============================================================================

fn get_buffer(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- require_data_view(this, state)
  #(state, Ok(JsObject(view.buffer)))
}

fn get_byte_length(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- require_data_view(this, state)
  use size, state <- view_size(state, view)
  #(state, Ok(value.from_int(size)))
}

fn get_byte_offset(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- require_data_view(this, state)
  use _size, state <- view_size(state, view)
  #(state, Ok(value.from_int(view.byte_offset)))
}

// ============================================================================
// §25.3.1.1 GetViewValue ( view, requestIndex, isLittleEndian, type )
// ============================================================================

fn get_view_value(
  this: JsValue,
  args: List(JsValue),
  element: ViewElementType,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, get_index, state <- view_and_index(this, args, state)
  let little = value.is_truthy(arg_at(args, 1))
  let elem_size = element_size(element)
  use data, pos, state <- checked_view_bytes(state, view, get_index, elem_size)
  case bit_array.slice(data, pos, elem_size) {
    Ok(chunk) -> #(state, Ok(decode(element, chunk, little)))
    Error(Nil) ->
      // Unreachable: bounds were validated above against the live buffer.
      state.range_error(state, "Offset is outside the bounds of the DataView")
  }
}

// ============================================================================
// §25.3.1.2 SetViewValue ( view, requestIndex, isLittleEndian, type, value )
// ============================================================================

fn set_view_value(
  this: JsValue,
  args: List(JsValue),
  element: ViewElementType,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // SetViewValue step 3 (immutable-arraybuffer proposal): an immutable
  // viewed buffer is a TypeError BEFORE the ToIndex/ToNumber coercions run
  // any user code (observable; test262 checks it).
  use view0, state <- require_data_view(this, state)
  use Nil, state <- require_mutable_buffer(state, view0.buffer)
  use view, get_index, state <- view_and_index(this, args, state)
  // Step 3: numberValue = ToBigInt(value) / ToNumber(value) — spec-mandated
  // BEFORE the bounds check, so it cannot fold into checked_view_bytes.
  let value_arg = arg_at(args, 1)
  use encoded, state <- encode_value(state, element, value_arg)
  let little = value.is_truthy(arg_at(args, 2))
  let elem_size = element_size(element)
  use data, pos, state <- checked_view_bytes(state, view, get_index, elem_size)
  let chunk = to_endian(encoded, little, elem_size)
  let #(new_data, written) = splice_clamped(data, pos, chunk)
  let heap =
    ops_buffer.store_region(state.heap, view.buffer, new_data, pos, written)
  #(State(..state, heap:), Ok(JsUndefined))
}

// ============================================================================
// Internal helpers — receiver/buffer validation
// ============================================================================

/// The [[DataView]] internal slots we operate on.
type ViewRecord {
  ViewRecord(buffer: Ref, byte_offset: Int, byte_length: Option(Int))
}

/// Unwrap `this` as a DataView or throw TypeError. CPS for `use`.
fn require_data_view(
  this: JsValue,
  state: State(host),
  cont: fn(ViewRecord, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, _ref, state <- helpers.require_brand(
    this,
    state,
    fn() { "Method called on incompatible receiver: expected a DataView" },
    view_record_of,
  )
  cont(view, state)
}

/// The [[DataView]] extractor handed to `require_brand` — a named function
/// (not an inline lambda) so the brand check builds no closure per call.
fn view_record_of(kind: state.ExoticKind(host)) -> Option(ViewRecord) {
  case kind {
    DataViewObject(buffer:, byte_offset:, byte_length:) ->
      Some(ViewRecord(buffer:, byte_offset:, byte_length:))
    _ -> None
  }
}

/// Immutable ArrayBuffer proposal — SetViewValue step 3: writes through a
/// DataView over an immutable buffer throw TypeError.
fn require_mutable_buffer(
  state: State(host),
  buffer: Ref,
  cont: fn(Nil, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case heap.read(state.heap, buffer) {
    Some(ObjectSlot(kind: ArrayBufferObject(storage: value.Immutable(..)), ..)) ->
      state.type_error(
        state,
        "Cannot modify a DataView backed by an immutable ArrayBuffer",
      )
    _ -> cont(Nil, state)
  }
}

/// Shared prologue of Get/SetViewValue (§25.3.1.1-2 steps 1-2): unwrap the
/// DataView receiver, then getIndex = ToIndex(requestIndex). CPS for `use`.
fn view_and_index(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
  cont: fn(ViewRecord, Int, State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- require_data_view(this, state)
  use get_index, state <- coerce.to_index_cps(
    state,
    first_arg_or_undefined(args),
    "Invalid DataView offset",
  )
  cont(view, get_index, state)
}

/// Shared epilogue of Get/SetViewValue (steps 5+): out-of-bounds (incl.
/// detached) → TypeError, then RangeError; yields the live buffer bytes and
/// the absolute byte position of the element. CPS for `use`.
fn checked_view_bytes(
  state: State(host),
  view: ViewRecord,
  get_index: Int,
  elem_size: Int,
  cont: fn(BitArray, Int, State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  use size, state <- view_size(state, view)
  use Nil <- helpers.guard(get_index + elem_size <= size, fn() {
    state.range_error(state, "Offset is outside the bounds of the DataView")
  })
  use data, state <- buffer_data(state, view.buffer)
  cont(data, view.byte_offset + get_index, state)
}

/// Read `val` as an ArrayBuffer/SharedArrayBuffer ref ([[ArrayBufferData]]).
fn as_array_buffer(state: State(host), val: JsValue) -> Option(Ref) {
  case val {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: ArrayBufferObject(..), ..)) -> Some(ref)
        _ -> None
      }
    _ -> None
  }
}

/// Read the live (non-detached) buffer's #(byte_length, resizable) or throw
/// TypeError if detached / not a buffer. CPS for `use`.
fn live_buffer_info(
  state: State(host),
  buf_ref: Ref,
  cont: fn(#(Int, Bool), State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case heap.read(state.heap, buf_ref) {
    // `value.Detached` is a detached buffer — [[ArrayBufferData]] is null.
    Some(ObjectSlot(kind: ArrayBufferObject(storage: value.Detached(..)), ..)) ->
      state.type_error(
        state,
        "Cannot perform operation on a detached ArrayBuffer",
      )
    Some(ObjectSlot(kind: ArrayBufferObject(storage:), ..)) ->
      cont(
        #(
          value.buffer_byte_size(storage),
          option.is_some(value.buffer_max_byte_length(storage)),
        ),
        state,
      )
    _ -> state.type_error(state, "DataView buffer is not an ArrayBuffer")
  }
}

/// Read the live buffer's data BitArray (TypeError if detached). CPS.
fn buffer_data(
  state: State(host),
  buf_ref: Ref,
  cont: fn(BitArray, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  let bits = case heap.read(state.heap, buf_ref) {
    Some(ObjectSlot(kind: ArrayBufferObject(storage:), ..)) ->
      value.buffer_bits(storage)
    _ -> None
  }
  case bits {
    Some(bits) -> cont(bits, state)
    None ->
      state.type_error(
        state,
        "Cannot perform operation on a detached ArrayBuffer",
      )
  }
}

/// GetViewByteLength + IsViewOutOfBounds (§25.3.1.1-25.3.1.3 helpers):
/// detached or out-of-bounds (resizable buffer shrunk under the view) →
/// TypeError; otherwise the current view size in bytes. CPS for `use`.
fn view_size(
  state: State(host),
  view: ViewRecord,
  cont: fn(Int, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  use #(buf_len, _resizable), state <- live_buffer_info(state, view.buffer)
  case view.byte_length {
    Some(len) ->
      case view.byte_offset + len <= buf_len {
        True -> cont(len, state)
        False ->
          state.type_error(
            state,
            "DataView is outside the bounds of its buffer",
          )
      }
    None ->
      case view.byte_offset <= buf_len {
        True -> cont(buf_len - view.byte_offset, state)
        False ->
          state.type_error(
            state,
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

/// RawBytesToNumeric (§25.1.2.13): decode element bytes to a JsValue.
fn decode(element: ViewElementType, chunk: BitArray, little: Bool) -> JsValue {
  let u = read_uint(chunk, little)
  case element {
    VNum(e) -> decode_number(e, u)
    VBig(e) -> decode_bigint(e, u)
  }
}

/// RawBytesToNumeric for the Number-valued elements.
fn decode_number(element: ViewNumElement, u: Int) -> JsValue {
  case element {
    VUint8 -> value.from_int(u)
    VUint16 -> value.from_int(u)
    VUint32 -> value.from_int(u)
    VInt8 -> value.from_int(to_signed(u, 8))
    VInt16 -> value.from_int(to_signed(u, 16))
    VInt32 -> value.from_int(to_signed(u, 32))
    VFloat16 -> JsNumber(f16_from_bits(u))
    VFloat32 -> JsNumber(f32_from_bits(u))
    VFloat64 -> JsNumber(f64_from_bits(u))
  }
}

/// RawBytesToNumeric for the BigInt-valued elements.
fn decode_bigint(element: ViewBigElement, u: Int) -> JsValue {
  case element {
    VBigUint64 -> JsBigInt(BigInt(u))
    VBigInt64 -> JsBigInt(BigInt(to_signed(u, 64)))
  }
}

/// Decode IEEE 754 binary32 bits. Erlang float segments only match finite
/// values, so NaN/±Infinity fall through to bit inspection.
fn f32_from_bits(u: Int) -> value.JsNum {
  case <<u:size(32)>> {
    <<f:float-size(32)>> -> Finite(f)
    _ ->
      case int.bitwise_and(u, 0x7FFFFF) == 0 {
        True ->
          case int.bitwise_and(u, 0x80000000) == 0 {
            True -> Infinity
            False -> NegInfinity
          }
        False -> NaN
      }
  }
}

/// Decode IEEE 754 binary64 bits — same fall-through strategy as binary32.
fn f64_from_bits(u: Int) -> value.JsNum {
  case <<u:size(64)>> {
    <<f:float-size(64)>> -> Finite(f)
    _ ->
      case int.bitwise_and(u, 0xFFFFFFFFFFFFF) == 0 {
        True ->
          case int.bitwise_shift_right(u, 63) == 0 {
            True -> Infinity
            False -> NegInfinity
          }
        False -> NaN
      }
  }
}

/// Decode IEEE 754 binary16 bits manually (1 sign, 5 exponent, 10 mantissa).
fn f16_from_bits(u: Int) -> value.JsNum {
  let sign = int.bitwise_shift_right(u, 15)
  let exp = int.bitwise_and(int.bitwise_shift_right(u, 10), 0x1F)
  let mant = int.bitwise_and(u, 0x3FF)
  case exp {
    0x1F ->
      case mant == 0, sign == 0 {
        True, True -> Infinity
        True, False -> NegInfinity
        False, _ -> NaN
      }
    0 -> Finite(apply_sign(int.to_float(mant) *. pow2(-24), sign))
    _ -> Finite(apply_sign(int.to_float(1024 + mant) *. pow2(exp - 25), sign))
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
/// bytes in BIG-endian order (to_endian flips later if needed). CPS.
fn encode_value(
  state: State(host),
  element: ViewElementType,
  val: JsValue,
  cont: fn(BitArray, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case element {
    VBig(e) -> {
      use n, state <- coerce.to_bigint_cps(state, val)
      cont(encode_bigint(e, n), state)
    }
    VNum(e) ->
      case coerce.js_to_number(state, val) {
        Error(#(thrown, state)) -> #(state, Error(thrown))
        Ok(#(num, state)) -> cont(encode_number(e, num), state)
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
fn encode_number(element: ViewNumElement, num: value.JsNum) -> BitArray {
  case element {
    VInt8 | VUint8 -> <<to_int_wrap(num):size(8)>>
    VInt16 | VUint16 -> <<to_int_wrap(num):size(16)>>
    VInt32 | VUint32 -> <<to_int_wrap(num):size(32)>>
    VFloat64 ->
      case num {
        Finite(f) -> <<f:float-size(64)>>
        NaN -> <<0x7FF8000000000000:size(64)>>
        Infinity -> <<0x7FF0000000000000:size(64)>>
        NegInfinity -> <<0xFFF0000000000000:size(64)>>
      }
    VFloat32 ->
      case num {
        // Erlang rounds double→single (ties to even) and overflows to ±inf.
        Finite(f) -> <<f:float-size(32)>>
        NaN -> <<0x7FC00000:size(32)>>
        Infinity -> <<0x7F800000:size(32)>>
        NegInfinity -> <<0xFF800000:size(32)>>
      }
    VFloat16 -> <<f16_to_bits(num):size(16)>>
  }
}

/// ToIntN/ToUintN truncation step: NaN/±Infinity → 0, else truncate toward
/// zero. Modulo wrapping is left to Erlang bit-syntax construction.
fn to_int_wrap(num: value.JsNum) -> Int {
  case num {
    Finite(f) -> value.float_to_int(f)
    NaN | Infinity | NegInfinity -> 0
  }
}

/// Encode a double to IEEE 754 binary16 bits with round-to-nearest-even.
/// Works on the exact binary64 bit pattern so no double rounding occurs.
fn f16_to_bits(num: value.JsNum) -> Int {
  case num {
    NaN -> 0x7E00
    Infinity -> 0x7C00
    NegInfinity -> 0xFC00
    Finite(f) -> {
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
