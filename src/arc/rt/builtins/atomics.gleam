//// ES2024 §25.4 The Atomics Object.
////
//// This runtime runs one agent per store and keeps SharedArrayBuffer bytes
//// in that store (`types.Shared`), so every read-modify-write here is a
//// plain read-compute-write over the snapshot: with no second agent able to
//// observe the buffer, that IS sequentially consistent. There is no
//// WaiterList: `Atomics.wait` validates and coerces exactly as the spec
//// orders, then throws at DoWait step 10 (this agent's [[CanBlock]] is
//// false — nothing could ever notify it), `Atomics.waitAsync` is
//// unsupported (TypeError), and `Atomics.notify` always finds zero waiters.
////
//// Validation order follows the spec (cross-checked against QuickJS
//// js_atomics_get_buf / js_atomics_op):
////   typedArray type check (TypeError) → shared check for wait modes
////   (TypeError) → detached check (TypeError) → ToIndex (RangeError) →
////   bounds (RangeError) → value coercion (user code!) → revalidate
////   (detached → TypeError, shrunk → RangeError).

import arc/rt/buffer
import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/store as rt_store
import arc/rt/typed_array_ffi.{
  type IntElem, I16, I32, I64, I8, U16, U32, U64, U8, int_elem_bits,
  int_elem_signed, int_elem_size, ta_get_int, ta_set_int,
}
import arc/rt/types.{
  type Agent, type AtomicsNative, type BufferStorage, type Handle, type JsVal,
  type TypedArrayKind, AtomicsAdd, AtomicsAnd, AtomicsCompareExchange,
  AtomicsExchange, AtomicsIsLockFree, AtomicsLoad, AtomicsN, AtomicsNotify,
  AtomicsOr, AtomicsPause, AtomicsStore, AtomicsSub, AtomicsWait,
  AtomicsWaitAsync, AtomicsXor, BigInt64Kind, BigKind, BigUint64Kind, Int16Kind,
  Int32Kind, Int8Kind, JFloat, JInt, JNan, JNegInf, JPosInf, KHandle, KNum,
  KUndef, NumKind, SObject, TypedArrayObj, Uint16Kind, Uint32Kind, Uint8Kind,
  classify, mk_bigint, mk_bool, mk_number, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/bit_array
import gleam/float
import gleam/int
import gleam/option.{type Option, None, Some}

// ============================================================================
// Init
// ============================================================================

/// Set up the Atomics namespace object. Like Math and JSON it's a plain
/// object — not callable, not constructable — with @@toStringTag "Atomics".
pub fn init(
  st: Agent,
  object_proto: Handle,
  function_proto: Handle,
) -> #(Handle, Agent) {
  let #(methods, st) =
    common.alloc_methods(st, function_proto, [
      #("add", AtomicsN(AtomicsAdd), 3),
      #("and", AtomicsN(AtomicsAnd), 3),
      #("compareExchange", AtomicsN(AtomicsCompareExchange), 4),
      #("exchange", AtomicsN(AtomicsExchange), 3),
      #("isLockFree", AtomicsN(AtomicsIsLockFree), 1),
      #("load", AtomicsN(AtomicsLoad), 2),
      #("notify", AtomicsN(AtomicsNotify), 3),
      #("or", AtomicsN(AtomicsOr), 3),
      #("pause", AtomicsN(AtomicsPause), 0),
      #("store", AtomicsN(AtomicsStore), 3),
      #("sub", AtomicsN(AtomicsSub), 3),
      #("wait", AtomicsN(AtomicsWait), 4),
      #("waitAsync", AtomicsN(AtomicsWaitAsync), 4),
      #("xor", AtomicsN(AtomicsXor), 3),
    ])
  common.init_namespace(st, object_proto, "Atomics", methods)
}

// ============================================================================
// Dispatch
// ============================================================================

/// Per-module dispatch for Atomics native functions.
pub fn dispatch(
  st: Agent,
  native: AtomicsNative,
  _this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    AtomicsAdd -> rmw(st, args, fn(old, v) { old + v })
    AtomicsAnd -> rmw(st, args, int.bitwise_and)
    AtomicsOr -> rmw(st, args, int.bitwise_or)
    AtomicsXor -> rmw(st, args, int.bitwise_exclusive_or)
    AtomicsSub -> rmw(st, args, fn(old, v) { old - v })
    AtomicsExchange -> rmw(st, args, fn(_old, v) { v })
    AtomicsCompareExchange -> compare_exchange(st, args)
    AtomicsLoad -> atomic_load(st, args)
    AtomicsStore -> atomic_store(st, args)
    AtomicsIsLockFree -> is_lock_free(st, args)
    AtomicsNotify -> notify(st, args)
    AtomicsWait -> do_wait(st, args)
    AtomicsWaitAsync ->
      rt_val.t_throw_type_error(
        st,
        "Atomics.waitAsync is not supported in this runtime",
      )
    AtomicsPause -> pause(st, args)
  }
}

// ============================================================================
// Validation — §25.4.3.1 ValidateIntegerTypedArray + §25.4.3.2/3
// ============================================================================

/// The four validation shapes an Atomics operation can take, so a
/// nonsensical combination (a waitable write, a shared-required
/// non-waitable) is unrepresentable rather than a call site the reader has
/// to squint at.
type AtomicAccess {
  /// add/and/or/xor/sub/exchange/compareExchange/store: any integer kind,
  /// buffer need not be shared, ~write~ accessMode (immutable → TypeError).
  RmwAccess
  /// load: any integer kind, buffer need not be shared, ~read~ accessMode.
  LoadAccess
  /// wait: Int32/BigInt64 only, buffer MUST be shared, ~read~.
  WaitAccess
  /// notify: Int32/BigInt64 only, buffer need not be shared, ~read~.
  NotifyAccess
}

/// Everything Atomics needs to know about a validated integer TypedArray.
type TaInfo {
  TaInfo(
    buffer: Handle,
    /// [[TypedArrayName]] / [[ContentType]] — matched as `BigKind(_)` /
    /// `NumKind(_)` wherever the content type decides between ToBigInt and
    /// ToNumber. There is no second encoding of ContentType here.
    elem_kind: TypedArrayKind,
    byte_offset: Int,
    /// The integer element type. Passed to the FFI codecs as-is (never as a
    /// bits/signed pair), so an element width the FFI has no clause for is
    /// unrepresentable.
    elem: IntElem,
    /// IsSharedArrayBuffer of the viewed buffer, captured ONCE from the
    /// validated buffer's [[ArrayBufferData]].
    shared: Bool,
  )
}

/// Allowed element kinds per Table 60 (§25.4.3.1): the 8 integer kinds for
/// general ops; only Int32/BigInt64 when `waitable`.
fn atomics_elem(kind: TypedArrayKind, waitable: Bool) -> Option(IntElem) {
  case waitable, kind {
    True, NumKind(Int32Kind) -> Some(I32)
    True, BigKind(BigInt64Kind) -> Some(I64)
    True, _ -> None
    False, NumKind(Int8Kind) -> Some(I8)
    False, NumKind(Uint8Kind) -> Some(U8)
    False, NumKind(Int16Kind) -> Some(I16)
    False, NumKind(Uint16Kind) -> Some(U16)
    False, NumKind(Int32Kind) -> Some(I32)
    False, NumKind(Uint32Kind) -> Some(U32)
    False, BigKind(BigInt64Kind) -> Some(I64)
    False, BigKind(BigUint64Kind) -> Some(U64)
    False, _ -> None
  }
}

/// Byte width of one element of the validated view.
fn elem_size(info: TaInfo) -> Int {
  int_elem_size(info.elem)
}

/// §25.4.3.1 ValidateIntegerTypedArray + §25.4.3.3 ValidateAtomicAccessOn-
/// IntegerTypedArray: validate `args[0]` as an integer TypedArray of an
/// allowed kind, then coerce `args[1]` with ToIndex and bounds-check it.
/// `mode` picks the validation shape — the checks below derive from it, so
/// the wait-mode shared-buffer check (non-shared → TypeError before the
/// index is even coerced; observable, test262 checks it) and the
/// immutable-arraybuffer proposal's ~write~ accessMode check
/// (ValidateTypedArray step 4: mutating op on an immutable buffer →
/// TypeError before index coercion; also observable) each fire for exactly
/// one variant.
fn with_ta_and_index(
  st: Agent,
  args: List(JsVal),
  mode mode: AtomicAccess,
) -> #(TaInfo, Int, Agent) {
  let waitable = case mode {
    WaitAccess | NotifyAccess -> True
    RmwAccess | LoadAccess -> False
  }
  let require_shared = case mode {
    WaitAccess -> True
    RmwAccess | LoadAccess | NotifyAccess -> False
  }
  let write = case mode {
    RmwAccess -> True
    LoadAccess | WaitAccess | NotifyAccess -> False
  }
  let ta_val = helpers.first_arg_or_undefined(args)
  use view <- helpers.some_or(read_typed_array(st, ta_val), fn() {
    rt_val.t_throw_type_error(
      st,
      "Atomics operation needs an integer TypedArray",
    )
  })
  use elem <- helpers.some_or(atomics_elem(view.elem_kind, waitable), fn() {
    rt_val.t_throw_type_error(
      st,
      "Invalid TypedArray element type for Atomics operation",
    )
  })
  use storage <- helpers.some_or(buffer.buffer_storage(st, view.buffer), fn() {
    rt_val.t_throw_type_error(st, "TypedArray is not attached")
  })
  let shared = types.buffer_is_shared(storage)
  use Nil <- helpers.guard(!require_shared || shared, fn() {
    rt_val.t_throw_type_error(
      st,
      "Atomics.wait requires a SharedArrayBuffer TypedArray",
    )
  })
  use buf <- helpers.some_or(live_buffer(storage), fn() {
    rt_val.t_throw_type_error(st, "ArrayBuffer is detached")
  })
  // ValidateTypedArray step 4 (immutable-arraybuffer proposal):
  // accessMode ~write~ on an immutable buffer → TypeError.
  use Nil <- helpers.guard(!write || !buf.immutable, fn() {
    rt_val.t_throw_type_error(
      st,
      "Atomics operation cannot write to an immutable ArrayBuffer",
    )
  })
  // Live length: a resizable buffer may have shrunk below the view
  // (§10.4.5.12 IsTypedArrayOutOfBounds folds into this).
  let size = int_elem_size(elem)
  let avail = { bit_array.byte_size(buf.bits) - view.byte_offset } / size
  let live = int.clamp(avail, 0, view.length)
  let info =
    TaInfo(
      buffer: view.buffer,
      elem_kind: view.elem_kind,
      byte_offset: view.byte_offset,
      elem:,
      shared:,
    )
  // §25.4.3.2 ValidateAtomicAccess: ToIndex then bounds check.
  let #(idx, st) =
    rt_val.t_to_index(
      st,
      helpers.arg_at(args, 1),
      "Invalid atomic access index",
    )
  use Nil <- helpers.guard(idx < live, fn() {
    rt_val.t_throw_range_error(st, "Atomics access index out of range")
  })
  #(info, idx, st)
}

/// The internal slots of the TypedArray under validation (§25.4.3.1
/// step 1): its viewed buffer, element kind, view byte offset and view
/// element count.
type TaView {
  TaView(
    buffer: Handle,
    elem_kind: TypedArrayKind,
    byte_offset: Int,
    length: Int,
  )
}

/// Pull the TypedArray internal slots out of a value, or None.
fn read_typed_array(st: Agent, val: JsVal) -> Option(TaView) {
  case classify(val) {
    KHandle(ref) ->
      case rt_store.t_cell_get(st, ref) {
        SObject(
          kind: TypedArrayObj(buffer:, elem_kind:, byte_offset:, length:),
          ..,
        ) ->
          Some(TaView(
            buffer:,
            elem_kind:,
            byte_offset:,
            length: buffer.typed_array_view_length(
              st,
              buffer,
              elem_kind,
              byte_offset,
              length,
            ),
          ))
        _ -> None
      }
    _ -> None
  }
}

/// A buffer projected onto its LIVE storage: only reachable for a
/// non-detached buffer, so nothing downstream has to re-check that.
type BufferInfo {
  BufferInfo(
    /// The live [[ArrayBufferData]] storage — never `Detached`.
    data: BufferStorage,
    /// The bytes of `data`.
    bits: BitArray,
    immutable: Bool,
  )
}

/// IsDetachedBuffer(O) is false — project a storage value onto its live
/// bytes. `Detached` IS the detached case: there are no bytes to hand out.
fn live_buffer(storage: BufferStorage) -> Option(BufferInfo) {
  use bits <- option.map(types.buffer_bits(storage))
  BufferInfo(
    data: storage,
    bits:,
    immutable: types.buffer_is_immutable(storage),
  )
}

/// §25.4.3.4 RevalidateAtomicAccess — the value coercion may have run user
/// code that detached or shrank the buffer. Hands back the live buffer it
/// just destructured; `write_element` persists into THAT, so a second store
/// read that disagrees with this one (and would silently drop the write)
/// cannot exist.
fn revalidate(st: Agent, info: TaInfo, idx: Int) -> BufferInfo {
  use storage <- helpers.some_or(buffer.buffer_storage(st, info.buffer), fn() {
    rt_val.t_throw_type_error(st, "TypedArray is not attached")
  })
  use buf <- helpers.some_or(live_buffer(storage), fn() {
    rt_val.t_throw_type_error(st, "ArrayBuffer is detached")
  })
  let size = elem_size(info)
  let byte_off = info.byte_offset + idx * size
  use Nil <- helpers.guard(
    byte_off + size <= bit_array.byte_size(buf.bits),
    fn() { rt_val.t_throw_range_error(st, "Atomics access index out of range") },
  )
  buf
}

// ============================================================================
// Value coercion
// ============================================================================

/// Coerce the operand for a read-modify-write/store per the array's content
/// type: ToBigInt for BigInt64/BigUint64, ToIntegerOrInfinity for the
/// integer kinds. ±∞ must be matched on the raw JsNum BEFORE the saturated
/// `jsnum_to_integer_or_infinity`: §7.1.7-§7.1.11 (ToInt8..ToUint32) all map
/// a non-finite ToIntegerOrInfinity result to +0, not to a huge saturated
/// integer whose low bits would be stored.
fn to_operand(st: Agent, info: TaInfo, val: JsVal) -> #(Int, Agent) {
  case info.elem_kind {
    BigKind(_) -> rt_val.t_to_bigint(st, val)
    NumKind(_) -> {
      let #(num, st) = rt_val.t_to_number(st, val)
      case num {
        JPosInf | JNegInf -> #(0, st)
        _ -> #(rt_val.jsnum_to_integer_or_infinity(num), st)
      }
    }
  }
}

// ============================================================================
// Raw element read/write
// ============================================================================

/// Truncate an arbitrary integer to the element's bit pattern, then
/// reinterpret per the element's signedness (two's complement).
fn wrap_to_kind(v: Int, elem: IntElem) -> Int {
  let bits = int_elem_bits(elem)
  let modulus = int.bitwise_shift_left(1, bits)
  let m = int.bitwise_and(v, modulus - 1)
  case int_elem_signed(elem) && m >= modulus / 2 {
    True -> m - modulus
    False -> m
  }
}

/// Read element `idx` (already validated) from fresh buffer data.
fn read_element(data: BitArray, info: TaInfo, idx: Int) -> Int {
  let off = info.byte_offset + idx * elem_size(info)
  ta_get_int(data, off, info.elem)
}

/// Write raw integer `v` (truncated mod 2^bits by the FFI) at element `idx`
/// and persist it into the buffer `revalidate` just witnessed (`buf`) — NOT
/// into a second store read that could disagree with the revalidation and
/// silently drop the store.
fn write_element(
  st: Agent,
  info: TaInfo,
  buf: BufferInfo,
  idx: Int,
  v: Int,
) -> Agent {
  let size = elem_size(info)
  let off = info.byte_offset + idx * size
  let new_bits = ta_set_int(buf.bits, off, info.elem, v)
  buffer.set_storage(
    st,
    info.buffer,
    types.buffer_store_region(buf.data, new_bits, off, size),
  )
}

/// Old element value → JS value per content type.
fn element_to_js(info: TaInfo, raw: Int) -> JsVal {
  case info.elem_kind {
    BigKind(_) -> mk_bigint(raw)
    NumKind(_) -> mk_number(JInt(raw))
  }
}

// ============================================================================
// §25.4.3.17 AtomicReadModifyWrite — add/and/or/xor/sub/exchange
// ============================================================================

fn rmw(
  st: Agent,
  args: List(JsVal),
  op: fn(Int, Int) -> Int,
) -> #(JsVal, Agent) {
  let #(info, idx, st) = with_ta_and_index(st, args, mode: RmwAccess)
  let #(operand, st) = to_operand(st, info, helpers.arg_at(args, 2))
  let buf = revalidate(st, info, idx)
  // Single-agent storage: no interleaving is possible, the snapshot
  // read-compute-write is trivially atomic.
  let old = read_element(buf.bits, info, idx)
  let st = write_element(st, info, buf, idx, op(old, operand))
  #(element_to_js(info, old), st)
}

// §25.4.7 Atomics.compareExchange ( typedArray, index, expected, replacement )
fn compare_exchange(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(info, idx, st) = with_ta_and_index(st, args, mode: RmwAccess)
  let #(expected, st) = to_operand(st, info, helpers.arg_at(args, 2))
  let #(replacement, st) = to_operand(st, info, helpers.arg_at(args, 3))
  let buf = revalidate(st, info, idx)
  let wrapped_expected = wrap_to_kind(expected, info.elem)
  let old = read_element(buf.bits, info, idx)
  let st = case old == wrapped_expected {
    True -> write_element(st, info, buf, idx, replacement)
    False -> st
  }
  #(element_to_js(info, old), st)
}

// §25.4.10 Atomics.load ( typedArray, index )
fn atomic_load(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(info, idx, st) = with_ta_and_index(st, args, mode: LoadAccess)
  // The index coercion may have run user code — revalidate (§25.4.10 step 2).
  let buf = revalidate(st, info, idx)
  #(element_to_js(info, read_element(buf.bits, info, idx)), st)
}

// §25.4.12 Atomics.store ( typedArray, index, value ) — returns the COERCED
// value (ToIntegerOrInfinity / ToBigInt result), not the stored bit pattern.
fn atomic_store(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(info, idx, st) = with_ta_and_index(st, args, mode: RmwAccess)
  case info.elem_kind {
    BigKind(_) -> {
      let #(v, st) = rt_val.t_to_bigint(st, helpers.arg_at(args, 2))
      let buf = revalidate(st, info, idx)
      let st = write_element(st, info, buf, idx, v)
      #(mk_bigint(v), st)
    }
    NumKind(_) -> {
      // §25.4.12 step 3: v = 𝔽(? ToIntegerOrInfinity(value)) — and v itself
      // is the return value, so ±∞ must survive the coercion (the STORED
      // element is ToIntN(±∞) = +0). Match on the raw JsNum rather than the
      // saturated Int to keep both.
      let #(num, st) = rt_val.t_to_number(st, helpers.arg_at(args, 2))
      let buf = revalidate(st, info, idx)
      let #(stored, ret) = case num {
        JPosInf -> #(0, mk_number(JPosInf))
        JNegInf -> #(0, mk_number(JNegInf))
        _ -> {
          let n = rt_val.jsnum_to_integer_or_infinity(num)
          #(n, mk_number(JInt(n)))
        }
      }
      let st = write_element(st, info, buf, idx, stored)
      #(ret, st)
    }
  }
}

// §25.4.9 Atomics.isLockFree ( size ) — must be consistent across calls;
// hardware lock-free sizes on every BEAM target are 1/2/4/8.
fn is_lock_free(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(n, st) =
    rt_val.t_to_integer_or_infinity(st, helpers.first_arg_or_undefined(args))
  let ok = case n {
    1 | 2 | 4 | 8 -> True
    _ -> False
  }
  #(mk_bool(ok), st)
}

// Atomics.pause ( [ iterationNumber ] ) — microwait proposal. No coercion:
// anything other than undefined or an integral Number is a TypeError. The
// pause itself is a no-op (single agent; nothing to spin-wait for).
fn pause(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  case classify(helpers.first_arg_or_undefined(args)) {
    KUndef -> #(mk_undefined(), st)
    KNum(JInt(_)) -> #(mk_undefined(), st)
    KNum(JFloat(f)) ->
      case rt_val.integral_int(f) {
        Some(_) -> #(mk_undefined(), st)
        None ->
          rt_val.t_throw_type_error(st, "Atomics.pause: not an integral number")
      }
    _ -> rt_val.t_throw_type_error(st, "Atomics.pause: not an integral number")
  }
}

// ============================================================================
// §25.4.3.14 DoWait — Atomics.wait (sync)
// ============================================================================

fn do_wait(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(info, _idx, st) = with_ta_and_index(st, args, mode: WaitAccess)
  // Step 6/7: v = ToBigInt64(value) | ToInt32(value).
  let #(_v, st) = wait_value(st, info, helpers.arg_at(args, 2))
  // Step 8/9: t = ToNumber(timeout); NaN/undefined → +∞; clamp ≥ 0.
  let #(_timeout_ms, st) = wait_timeout(st, helpers.arg_at(args, 3))
  // Step 10: if mode is sync and AgentCanSuspend() is false, throw a
  // TypeError. Sits after the value/timeout coercions (steps 6-9) per the
  // current spec text — the position is observable via valueOf side effects.
  // This agent's [[CanBlock]] is false: no other agent shares its buffers,
  // so a suspended wait could never be notified.
  rt_val.t_throw_type_error(st, "Atomics.wait cannot be called in this agent")
}

/// DoWait steps 6/7: Int32Array → ToInt32, BigInt64Array → ToBigInt64.
fn wait_value(st: Agent, info: TaInfo, val: JsVal) -> #(Int, Agent) {
  case info.elem_kind {
    BigKind(_) -> {
      let #(n, st) = rt_val.t_to_bigint(st, val)
      #(wrap_to_kind(n, I64), st)
    }
    NumKind(_) -> rt_val.t_to_int32(st, val)
  }
}

/// DoWait steps 8/9: timeout in milliseconds. None = +∞.
fn wait_timeout(st: Agent, val: JsVal) -> #(Option(Int), Agent) {
  let #(num, st) = rt_val.t_to_number(st, val)
  let t = case num {
    JNan | JPosInf -> None
    JNegInf -> Some(0)
    JInt(i) -> Some(int.max(i, 0))
    JFloat(f) ->
      case f <=. 0.0 {
        True -> Some(0)
        False -> Some(float.round(float.ceiling(f)))
      }
  }
  #(t, st)
}

// ============================================================================
// §25.4.11 Atomics.notify ( typedArray, index, count )
// ============================================================================

fn notify(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(_info, _idx, st) = with_ta_and_index(st, args, mode: NotifyAccess)
  // Step 3: count — undefined → +∞, else ToIntegerOrInfinity clamped ≥ 0.
  // Coerced BEFORE the non-shared early return (observable, test262 checks).
  let #(_count, st) = notify_count(st, helpers.arg_at(args, 2))
  // Step 6: non-shared buffers can have no waiters → +0. Shared buffers in
  // this single-agent runtime have an always-empty WaiterList → +0 too.
  #(mk_number(JInt(0)), st)
}

/// Notify count (§25.4.11 step 3): undefined → effectively unbounded;
/// otherwise max(ToIntegerOrInfinity(count), 0).
fn notify_count(st: Agent, val: JsVal) -> #(Int, Agent) {
  case classify(val) {
    KUndef -> #(rt_val.max_safe_integer, st)
    _ -> {
      let #(n, st) = rt_val.t_to_integer_or_infinity(st, val)
      #(int.max(n, 0), st)
    }
  }
}
