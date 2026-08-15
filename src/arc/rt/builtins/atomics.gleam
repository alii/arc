//// ES2024 §25.4 The Atomics Object.
////
//// A SharedArrayBuffer only this agent has ever seen keeps its bytes in the
//// store (`types.LocalBlock`), and there every read-modify-write is a plain
//// read-compute-write over the snapshot: with no second agent able to
//// observe the buffer, that IS sequentially consistent. Once the buffer has
//// been handed to another agent (or waited on) its block lives in an owner
//// process (`arc/rt/sab`, `types.OwnerBlock`) together with the spec's
//// WaiterList, and load / store / RMW / compareExchange / wait / notify are
//// each ONE synchronous message to that process — the owner's mailbox order
//// is the critical section of §25.4.3.17 AtomicReadModifyWrite and §25.4.1
//// WaiterList Records. A sync `Atomics.wait` then blocks this BEAM process
//// in a selective receive until notified or timed out; `Atomics.waitAsync`
//// registers a waiter that joins `Agent.waiters`, and the agent's microtask
//// drain (`arc/rt/async.drain`) takes its wake from this process's mailbox
//// or runs its timeout job, settling the promise.
////
//// AgentCanSuspend() (§25.4.3.14 step 10) is the agent's [[CanBlock]],
//// `st.hooks.can_block`: an embedder that cannot afford to have
//// this process parked leaves it false and a sync wait throws instead.
////
//// Validation order follows the spec (cross-checked against QuickJS
//// js_atomics_get_buf / js_atomics_op):
////   typedArray type check (TypeError) → shared check for wait modes
////   (TypeError) → detached check (TypeError) → ToIndex (RangeError) →
////   bounds (RangeError) → value coercion (user code!) → revalidate
////   (detached → TypeError, shrunk → RangeError).

import arc/rt/async as rt_async
import arc/rt/buffer
import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/sab
import arc/rt/store as rt_store
import arc/rt/typed_array_ffi.{
  type IntElem, I16, I32, I64, I8, U16, U32, U64, U8, int_elem_bits,
  int_elem_signed, int_elem_size, ta_get_int, ta_set_int, ta_zeroed,
}
import arc/rt/types.{
  type Agent, type AtomicsNative, type BufferStorage, type Handle, type JsVal,
  type SabOwner, type TypedArrayKind, AtomicsAdd, AtomicsAnd,
  AtomicsCompareExchange, AtomicsExchange, AtomicsIsLockFree, AtomicsLoad,
  AtomicsN, AtomicsNotify, AtomicsOr, AtomicsPause, AtomicsStore, AtomicsSub,
  AtomicsWait, AtomicsWaitAsync, AtomicsXor, BigInt64Kind, BigKind,
  BigUint64Kind, Detached, Int16Kind, Int32Kind, Int8Kind, JFloat, JInt, JNan,
  JNegInf, JPosInf, KHandle, KNum, KUndef, NumKind, OwnerBlock, SObject, Shared,
  TypedArrayObj, Uint16Kind, Uint32Kind, Uint8Kind, classify, mk_bigint, mk_bool,
  mk_number, mk_object, mk_undefined,
}
import arc/rt/val as rt_val
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
    AtomicsWait -> do_wait(st, args, sync: True)
    AtomicsWaitAsync -> do_wait(st, args, sync: False)
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
  use Nil <- helpers.guard(
    !require_shared || types.buffer_is_shared(storage),
    fn() {
      rt_val.t_throw_type_error(
        st,
        "Atomics.wait requires a SharedArrayBuffer TypedArray",
      )
    },
  )
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
  let avail = { buf.byte_size - view.byte_offset } / size
  let live = int.clamp(avail, 0, view.length)
  let info =
    TaInfo(
      buffer: view.buffer,
      elem_kind: view.elem_kind,
      byte_offset: view.byte_offset,
      elem:,
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
    /// Where the live [[ArrayBufferData]] bytes are — never detached.
    data: LiveData,
    /// [[ArrayBufferByteLength]] of `data`.
    byte_size: Int,
    immutable: Bool,
  )
}

/// The two places live bytes can be. In-store bytes are read and rebuilt
/// right here; an owner-held shared block is only ever touched through
/// messages to its owner, one per element operation.
type LiveData {
  StoreData(storage: BufferStorage, bits: BitArray)
  OwnerData(owner: SabOwner)
}

/// IsDetachedBuffer(O) is false — project a storage value onto its live
/// bytes. `Detached` IS the detached case: there are no bytes to hand out.
fn live_buffer(storage: BufferStorage) -> Option(BufferInfo) {
  case storage {
    Detached(..) -> None
    Shared(block: OwnerBlock(owner:, ..), ..) ->
      Some(BufferInfo(
        data: OwnerData(owner:),
        byte_size: types.buffer_byte_size(storage),
        immutable: False,
      ))
    _ -> {
      use bits <- option.map(types.buffer_bits(storage))
      BufferInfo(
        data: StoreData(storage:, bits:),
        byte_size: types.buffer_byte_size(storage),
        immutable: types.buffer_is_immutable(storage),
      )
    }
  }
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
  use Nil <- helpers.guard(byte_off + size <= buf.byte_size, fn() {
    rt_val.t_throw_range_error(st, "Atomics access index out of range")
  })
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

/// Byte offset of element `idx` of the validated view.
fn element_offset(info: TaInfo, idx: Int) -> Int {
  info.byte_offset + idx * elem_size(info)
}

/// The element's bytes on their own: `v` (truncated mod 2^bits by the FFI)
/// encoded into a fresh element-sized binary. What an owner is sent for a
/// store, and what a wait compares the live element against.
fn element_bytes(info: TaInfo, v: Int) -> BitArray {
  ta_set_int(ta_zeroed(elem_size(info)), 0, info.elem, v)
}

/// Read element `idx` (already validated) from the live buffer.
fn read_element(buf: BufferInfo, info: TaInfo, idx: Int) -> Int {
  let off = element_offset(info, idx)
  case buf.data {
    StoreData(bits:, ..) -> ta_get_int(bits, off, info.elem)
    OwnerData(owner:) ->
      ta_get_int(sab.read_part(owner, off, elem_size(info)), 0, info.elem)
  }
}

/// Write raw integer `v` at element `idx` of the buffer `revalidate` just
/// witnessed (`buf`) — for in-store bytes NOT into a second store read that
/// could disagree with the revalidation and silently drop the store; for an
/// owner-held block as one store message of exactly the element's bytes.
fn write_element(
  st: Agent,
  info: TaInfo,
  buf: BufferInfo,
  idx: Int,
  v: Int,
) -> Agent {
  let size = elem_size(info)
  let off = element_offset(info, idx)
  case buf.data {
    StoreData(storage:, bits:) ->
      buffer.set_storage(
        st,
        info.buffer,
        types.buffer_store_region(
          storage,
          ta_set_int(bits, off, info.elem, v),
          off,
          size,
        ),
      )
    OwnerData(owner:) -> {
      let Nil = sab.write(owner, off, element_bytes(info, v))
      st
    }
  }
}

/// §25.4.3.17 AtomicReadModifyWrite steps 8-11 / §25.4.6 compareExchange
/// steps 11-14: read the element, decide what replaces it (`Some(new)`) or
/// that it stays (`None`), write, and hand back the value READ. In-store
/// bytes: no other agent can interleave, so read-compute-write over the
/// snapshot is trivially atomic. Owner-held block: the whole step runs
/// inside the owner as one `sab.update`.
fn modify_element(
  st: Agent,
  info: TaInfo,
  buf: BufferInfo,
  idx: Int,
  op: fn(Int) -> Option(Int),
) -> #(Int, Agent) {
  case buf.data {
    StoreData(..) -> {
      let old = read_element(buf, info, idx)
      case op(old) {
        Some(new) -> #(old, write_element(st, info, buf, idx, new))
        None -> #(old, st)
      }
    }
    OwnerData(owner:) -> {
      let old = {
        use old_bits <- sab.update(
          owner,
          element_offset(info, idx),
          elem_size(info),
        )
        let old = ta_get_int(old_bits, 0, info.elem)
        case op(old) {
          Some(new) -> #(old, ta_set_int(old_bits, 0, info.elem, new))
          None -> #(old, old_bits)
        }
      }
      #(old, st)
    }
  }
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
  let #(old, st) =
    modify_element(st, info, buf, idx, fn(old) { Some(op(old, operand)) })
  #(element_to_js(info, old), st)
}

// §25.4.6 Atomics.compareExchange ( typedArray, index, expected, replacement )
fn compare_exchange(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(info, idx, st) = with_ta_and_index(st, args, mode: RmwAccess)
  let #(expected, st) = to_operand(st, info, helpers.arg_at(args, 2))
  let #(replacement, st) = to_operand(st, info, helpers.arg_at(args, 3))
  let buf = revalidate(st, info, idx)
  let wrapped_expected = wrap_to_kind(expected, info.elem)
  let #(old, st) =
    modify_element(st, info, buf, idx, fn(old) {
      case old == wrapped_expected {
        True -> Some(replacement)
        False -> None
      }
    })
  #(element_to_js(info, old), st)
}

// §25.4.9 Atomics.load ( typedArray, index )
fn atomic_load(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(info, idx, st) = with_ta_and_index(st, args, mode: LoadAccess)
  // The index coercion may have run user code — revalidate (§25.4.9 step 2).
  let buf = revalidate(st, info, idx)
  #(element_to_js(info, read_element(buf, info, idx)), st)
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

// §25.4.8 Atomics.isLockFree ( size ) — must be consistent across calls;
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
// pause itself is a no-op hint.
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
// §25.4.3.14 DoWait — Atomics.wait (sync) and Atomics.waitAsync (async)
// ============================================================================

fn do_wait(st: Agent, args: List(JsVal), sync sync: Bool) -> #(JsVal, Agent) {
  let #(info, idx, st) = with_ta_and_index(st, args, mode: WaitAccess)
  // Step 6/7: v = ToBigInt64(value) | ToInt32(value).
  let #(v, st) = wait_value(st, info, helpers.arg_at(args, 2))
  // Step 8/9: t = ToNumber(timeout); NaN/undefined → +∞; clamp ≥ 0.
  let #(timeout_ms, st) = wait_timeout(st, helpers.arg_at(args, 3))
  // Step 10: if mode is sync and AgentCanSuspend() is false — this agent's
  // [[CanBlock]] is false — throw a TypeError. Sits after the value/timeout
  // coercions (steps 6-9) per the current spec text; the position is
  // observable via valueOf side effects. waitAsync never blocks, so async
  // mode is exempt.
  use Nil <- helpers.guard(!sync || st.hooks.can_block, fn() {
    rt_val.t_throw_type_error(st, "Atomics.wait cannot be called in this agent")
  })
  // SharedArrayBuffers are never detached and never shrink, so this cannot
  // fail; it fixes the byte index the waiter is keyed on (steps 11-13).
  let _buf = revalidate(st, info, idx)
  let byte_off = element_offset(info, idx)
  let expected = element_bytes(info, v)
  // Steps 14-16: the WaiterList lives with the block's owner; a block only
  // this agent could see so far gets its owner now.
  let #(owner, st) = sab.share(st, info.buffer)
  let assert Some(owner) = owner
    as "Atomics.wait: WaitAccess validated a SharedArrayBuffer"
  case sync, timeout_ms {
    // Steps 17-20 + 22-31, sync: compare-and-add-waiter inside the owner's
    // critical section, then suspend THIS process until notified or timed
    // out (None = +∞). Even t = 0 goes through the WaiterList: a notify
    // that lands between AddWaiter and the timeout has woken this waiter and
    // counted it, so "ok" is the only answer consistent with that count.
    True, _ -> {
      let outcome =
        sab.wait_sync(owner, byte_off, expected, option.unwrap(timeout_ms, -1))
      #(rt_async.wait_result_js(outcome), st)
    }
    // Async, t = 0 (steps 17-21): compare, then { async: false, value:
    // "not-equal" | "timed-out" } WITHOUT ever joining the WaiterList — a
    // waiter that was never added is one no notify can count.
    False, Some(0) -> {
      let live = sab.read_part(owner, byte_off, elem_size(info))
      let outcome = case live == expected {
        True -> rt_async.TimedOut
        False -> rt_async.NotEqual
      }
      wait_result_object(st, False, rt_async.wait_result_js(outcome))
    }
    // Async (steps 22-32): compare-and-AddWaiter in the owner; the waiter
    // joins `st.waiters`, and this agent's drain wakes it (the owner's
    // message queues NotifyWaiter's resolve job) or runs its timeout job at
    // `deadline` (`rt/async.drain`).
    False, _ -> {
      let deadline =
        option.map(timeout_ms, fn(ms) { st.hooks.monotonic_now() + ms })
      case sab.register_async(st, owner, byte_off, expected, deadline) {
        #(Some(promise), st) -> wait_result_object(st, True, mk_object(promise))
        #(None, st) ->
          wait_result_object(
            st,
            False,
            rt_async.wait_result_js(rt_async.NotEqual),
          )
      }
    }
  }
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

/// §25.4.3.14 steps 4/20/21/29: the { async, value } record waitAsync
/// returns, on %Object.prototype%.
fn wait_result_object(
  st: Agent,
  is_async: Bool,
  value: JsVal,
) -> #(JsVal, Agent) {
  let #(h, st) =
    common.alloc_pojo(st, st.realm.object.prototype, [
      #("async", mk_bool(is_async)),
      #("value", value),
    ])
  #(mk_object(h), st)
}

// ============================================================================
// §25.4.11 Atomics.notify ( typedArray, index, count )
// ============================================================================

fn notify(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(info, idx, st) = with_ta_and_index(st, args, mode: NotifyAccess)
  // Step 3: count — undefined → +∞, else ToIntegerOrInfinity clamped ≥ 0.
  // Coerced BEFORE the non-shared early return (observable, test262 checks).
  let #(count, st) = notify_count(st, helpers.arg_at(args, 2))
  // Step 6: a non-shared buffer has no waiters → +0 (no revalidation: a
  // buffer detached or shrunk by the coercions still answers 0). Neither
  // has a shared block no other agent has seen and nobody has waited on:
  // any waiter would have moved it to an owner first.
  case buffer.buffer_storage(st, info.buffer) {
    // Steps 7-12: RemoveWaiters + NotifyWaiter, FIFO, inside the owner.
    Some(Shared(block: OwnerBlock(owner:, ..), ..)) -> {
      let n = sab.notify(owner, element_offset(info, idx), count)
      #(mk_number(JInt(n)), st)
    }
    Some(_) | None -> #(mk_number(JInt(0)), st)
  }
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
