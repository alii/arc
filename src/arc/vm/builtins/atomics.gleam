//// ES2024 §25.4 The Atomics Object.
////
//// Each agent's JS runs on one BEAM process, but agents share
//// SharedArrayBuffer cells, so the read-modify-write operations
//// (add/sub/and/or/xor/exchange/compareExchange) on shared storage are
//// performed by arc_sab_ffi's compare_exchange retry loop on the containing
//// 64-bit atomics cell — a Gleam-side read-compute-write over a byte
//// snapshot would lose updates raced by another agent process. Non-shared
//// buffers are only ever touched by their own process, so the snapshot
//// path remains trivially atomic there. The spec's WaiterList lives
//// in a shared, DATA-ONLY ETS table (arc_waiter_ffi): a blocking
//// `Atomics.wait` registers itself there, then delegates the actual
//// blocking to the embedder's `state.ctx.host_hooks.sync_wait` capability —
//// core never executes a mailbox receive. `Atomics.notify` atomically
//// CLAIMS waiters from the table (the claim is the spec's "woken" count)
//// and hands remote ones to `state.ctx.host_hooks.deliver_wake` for message
//// delivery — core never sends wake messages either. See arc/host.gleam
//// for the full capability contract. `Atomics.waitAsync` waiters are
//// kept on State (FIFO, where the promise lives) plus an interchangeable
//// token in the same ETS table, so notify can count and wake them across
//// processes: same-process notify settles them directly; cross-process
//// notify wakes arrive in the owning EMBEDDER's mailbox and are injected
//// via exec/event_loop.inject_notify.
////
//// Validation order follows the spec (cross-checked against QuickJS
//// js_atomics_get_buf / js_atomics_op):
////   typedArray type check (TypeError) → shared check for wait modes
////   (TypeError) → detached check (TypeError) → ToIndex (RangeError) →
////   bounds (RangeError) → value coercion (user code!) → revalidate
////   (detached → TypeError, shrunk → RangeError).

import arc/vm/builtins/common
import arc/vm/builtins/helpers
import arc/vm/builtins/promise as builtins_promise
import arc/vm/heap
import arc/vm/limits
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type AtomicsNativeFn, type JsValue, type Ref, type TypedArrayKind, AtomicsAdd,
  AtomicsAnd, AtomicsCompareExchange, AtomicsExchange, AtomicsIsLockFree,
  AtomicsLoad, AtomicsNative, AtomicsNotify, AtomicsOr, AtomicsPause,
  AtomicsStore, AtomicsSub, AtomicsWait, AtomicsWaitAsync, AtomicsWaiter,
  AtomicsXor, BigInt, Finite, Infinity, JsBigInt, JsBool, JsNumber, JsObject,
  JsString, JsUndefined, NaN, NegInfinity, ObjectSlot,
}
import gleam/bit_array
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{None, Some}

// ============================================================================
// Init
// ============================================================================

/// Set up the Atomics namespace object. Like Math and JSON it's a plain
/// object — not callable, not constructable — with @@toStringTag "Atomics".
pub fn init(
  h: Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(host), Ref) {
  let #(h, methods) =
    common.alloc_methods(h, function_proto, [
      #("add", AtomicsNative(AtomicsAdd), 3),
      #("and", AtomicsNative(AtomicsAnd), 3),
      #("compareExchange", AtomicsNative(AtomicsCompareExchange), 4),
      #("exchange", AtomicsNative(AtomicsExchange), 3),
      #("isLockFree", AtomicsNative(AtomicsIsLockFree), 1),
      #("load", AtomicsNative(AtomicsLoad), 2),
      #("notify", AtomicsNative(AtomicsNotify), 3),
      #("or", AtomicsNative(AtomicsOr), 3),
      #("pause", AtomicsNative(AtomicsPause), 0),
      #("store", AtomicsNative(AtomicsStore), 3),
      #("sub", AtomicsNative(AtomicsSub), 3),
      #("wait", AtomicsNative(AtomicsWait), 4),
      #("waitAsync", AtomicsNative(AtomicsWaitAsync), 4),
      #("xor", AtomicsNative(AtomicsXor), 3),
    ])
  common.init_namespace(h, object_proto, "Atomics", methods)
}

// ============================================================================
// Dispatch
// ============================================================================

/// Per-module dispatch for Atomics native functions.
pub fn dispatch(
  native: AtomicsNativeFn,
  args: List(JsValue),
  _this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case native {
    AtomicsAdd -> rmw(args, state, fn(old, v) { old + v })
    AtomicsAnd -> rmw(args, state, int.bitwise_and)
    AtomicsOr -> rmw(args, state, int.bitwise_or)
    AtomicsXor -> rmw(args, state, int.bitwise_exclusive_or)
    AtomicsSub -> rmw(args, state, fn(old, v) { old - v })
    AtomicsExchange -> rmw(args, state, fn(_old, v) { v })
    AtomicsCompareExchange -> compare_exchange(args, state)
    AtomicsLoad -> atomic_load(args, state)
    AtomicsStore -> atomic_store(args, state)
    AtomicsIsLockFree -> is_lock_free(args, state)
    AtomicsNotify -> notify(args, state)
    AtomicsWait -> do_wait(args, state, sync: True)
    AtomicsWaitAsync -> do_wait(args, state, sync: False)
    AtomicsPause -> pause(args, state)
  }
}

// ============================================================================
// FFI — element access on the buffer binary + blocking sleep
// ============================================================================

@external(erlang, "arc_typed_array_ffi", "ta_get_int")
fn ta_get_int(
  data: BitArray,
  byte_off: Int,
  size_bits: Int,
  signed: Bool,
) -> Int

@external(erlang, "arc_typed_array_ffi", "ta_set_int")
fn ta_set_int(
  data: BitArray,
  byte_off: Int,
  size_bits: Int,
  val: Int,
) -> BitArray

/// Atomic read-modify-write of one element in shared (atomics-backed)
/// storage: the FFI reads the containing 64-bit cell, applies `op` to the
/// witnessed old element value, and CAS-es the cell back — retrying the
/// whole read-compute-write on conflict. Returns the witnessed old value.
@external(erlang, "arc_sab_ffi", "rmw_element")
fn sab_rmw_element(
  ref: value.AtomicsRef,
  byte_offset: Int,
  size_bits: Int,
  signed: Bool,
  op: fn(Int) -> Int,
) -> Int

/// Atomic compareExchange of one element in shared storage: stores
/// `replacement` only when the element currently equals `expected` (already
/// wrapped to the element domain). Returns the witnessed old value.
@external(erlang, "arc_sab_ffi", "cas_element")
fn sab_cas_element(
  ref: value.AtomicsRef,
  byte_offset: Int,
  size_bits: Int,
  signed: Bool,
  expected: Int,
  replacement: Int,
) -> Int

@external(erlang, "arc_atomics_ffi", "sleep")
fn ffi_sleep(ms: Int) -> Nil

/// Raw BEAM monotonic clock in milliseconds. The VM itself reads the clock
/// through `state.ctx.host_hooks.monotonic_now` (waitAsync deadline
/// bookkeeping, the event loop's deadline arithmetic) so an embedder can
/// virtualise time; this external is the real clock behind the DEFAULT hook
/// and stays public for embedder natives (e.g. the $262 agent helpers) that
/// want the same wall clock outside any State.
@external(erlang, "arc_atomics_ffi", "monotonic_now")
pub fn monotonic_now() -> Int

/// Raw blocking sleep (`timer:sleep/1`, no-op for ms <= 0). The event loop
/// sleeps via `state.ctx.host_hooks.sleep_ms`; this is the real sleep behind
/// the DEFAULT hook, public for embedder natives ($262 agent `sleep`).
pub fn sleep_ms(ms: Int) -> Nil {
  ffi_sleep(ms)
}

/// Set the host agent's [[CanBlock]] (§9.7) for the calling process —
/// interpreter.new_state seeds State.can_block from it at realm boot.
/// Defaults to True; public for the test262 runner, which sets False for
/// tests flagged CanBlockIsFalse before booting the test realm.
@external(erlang, "arc_atomics_ffi", "set_can_block")
pub fn set_can_block(can: Bool) -> Nil

// ============================================================================
// FFI — cross-process WaiterList (arc_waiter_ffi.erl)
// ============================================================================

/// Cross-process identity of a buffer's WaiterList (an Erlang term: the
/// SAB's atomics ref for shared storage, a pid-scoped heap id otherwise).
/// Alias of the contract type in arc/vm/state (State's capability fields
/// reference it); re-exported here for existing callers.
pub type WaiterKey =
  state.WaiterKey

/// Opaque handle to one registered waiterlist entry (its ETS key plus the
/// unique message ref the notifier will address). Contract type from
/// arc/vm/state — handed to the embedder inside a WaitRequest.
type WaiterHandle =
  state.WaiterHandle

@external(erlang, "arc_waiter_ffi", "local_buffer_key")
fn ffi_local_buffer_key(id: Int) -> WaiterKey

@external(erlang, "arc_waiter_ffi", "shared_buffer_key")
fn ffi_shared_buffer_key(ref: value.AtomicsRef) -> WaiterKey

@external(erlang, "arc_waiter_ffi", "insert_waiter")
fn ffi_insert_waiter(
  key: WaiterKey,
  byte_index: Int,
  is_async: Bool,
) -> WaiterHandle

/// Withdraw our own waiterlist entry (data-only ets:take). Returns True
/// when a notifier had ALREADY claimed the entry — its wake message is in
/// flight to this process's mailbox and the caller must flush it via the
/// embedder capability (see sync_block's not-equal arm), or it will
/// spuriously settle the next waiter at the same (key, byte index).
@external(erlang, "arc_waiter_ffi", "cancel_waiter")
fn ffi_cancel_waiter(handle: WaiterHandle) -> Bool

/// Atomically claim up to `count` waiters FIFO (data-only ets:take loop).
/// Returns the CLAIMED remote waiters — delivery of their wake messages is
/// the embedder's job via state.ctx.host_hooks.deliver_wake — plus the count of our
/// own waitAsync tokens taken (settled directly on State by the caller).
@external(erlang, "arc_waiter_ffi", "take_waiters")
fn ffi_take_waiters(
  key: WaiterKey,
  byte_index: Int,
  count: Int,
) -> #(List(state.ClaimedWaiter), Int)

@external(erlang, "arc_waiter_ffi", "remove_async_token")
fn ffi_remove_async_token(key: WaiterKey, byte_index: Int) -> Nil

/// WaiterList identity of a buffer (§25.4.3.6 GetWaiterList): the shared
/// storage's atomics ref — identical in every process observing the SAB —
/// or a pid-scoped local key for buffers without process-independent
/// storage (those can only ever have same-process waiters).
fn buffer_key(state: State(host), buffer: Ref) -> WaiterKey {
  case heap.read(state.heap, buffer) {
    Some(ObjectSlot(
      kind: value.ArrayBufferObject(data: value.BufShared(ref:, ..), ..),
      ..,
    )) -> ffi_shared_buffer_key(ref)
    _ -> {
      let value.Ref(id) = buffer
      ffi_local_buffer_key(id)
    }
  }
}

// ============================================================================
// Validation — §25.4.3.1 ValidateIntegerTypedArray + §25.4.3.2/3
// ============================================================================

/// Everything Atomics needs to know about a validated integer TypedArray.
type TaInfo {
  TaInfo(
    buffer: Ref,
    elem_kind: TypedArrayKind,
    byte_offset: Int,
    /// Live element count at validation time (clamped for shrunk resizable
    /// buffers).
    length: Int,
    /// Bit width of one element (8/16/32/64).
    bits: Int,
    /// Whether reads should sign-extend (Int8/Int16/Int32/BigInt64).
    signed: Bool,
    /// The atomics ref of the view's shared (cross-process) storage, or
    /// None for plain process-local byte storage — captured ONCE from the
    /// validated buffer's [[ArrayBufferData]], the single source of truth
    /// for shared-ness. Some(_) means an RMW must go through the FFI's
    /// cell-CAS loop (cross-process atomicity); None means the snapshot
    /// path is trivially atomic (single process).
    storage: option.Option(value.AtomicsRef),
  )
}

/// Allowed element kinds per Table 60 (§25.4.3.1): the 8 integer kinds for
/// general ops; only Int32/BigInt64 when `waitable`. Returns #(bits, signed).
fn kind_bits(
  kind: TypedArrayKind,
  waitable: Bool,
) -> option.Option(#(Int, Bool)) {
  case waitable, kind {
    True, value.Int32Kind -> Some(#(32, True))
    True, value.BigInt64Kind -> Some(#(64, True))
    True, _ -> None
    False, value.Int8Kind -> Some(#(8, True))
    False, value.Uint8Kind -> Some(#(8, False))
    False, value.Int16Kind -> Some(#(16, True))
    False, value.Uint16Kind -> Some(#(16, False))
    False, value.Int32Kind -> Some(#(32, True))
    False, value.Uint32Kind -> Some(#(32, False))
    False, value.BigInt64Kind -> Some(#(64, True))
    False, value.BigUint64Kind -> Some(#(64, False))
    False, _ -> None
  }
}

/// §25.4.3.1 ValidateIntegerTypedArray + §25.4.3.3 ValidateAtomicAccessOn-
/// IntegerTypedArray: validate `args[0]` as an integer TypedArray of an
/// allowed kind, then coerce `args[1]` with ToIndex and bounds-check it.
/// `require_shared` is the wait/waitAsync mode (non-shared → TypeError before
/// the index is even coerced — observable, test262 checks it).
/// `write` is the immutable-arraybuffer proposal's ~write~ accessMode
/// (ValidateTypedArray step 4): a mutating op on a view over an immutable
/// buffer is a TypeError BEFORE the index is coerced — also observable.
fn with_ta_and_index(
  state: State(host),
  args: List(JsValue),
  waitable waitable: Bool,
  require_shared require_shared: Bool,
  write write: Bool,
  cont cont: fn(TaInfo, Int, State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  let ta_val = helpers.first_arg_or_undefined(args)
  use view <- some_or(read_typed_array(state, ta_val), fn() {
    state.type_error(state, "Atomics operation needs an integer TypedArray")
  })
  use #(bits, signed) <- some_or(kind_bits(view.elem_kind, waitable), fn() {
    state.type_error(
      state,
      "Invalid TypedArray element type for Atomics operation",
    )
  })
  use buf <- some_or(read_buffer(state, view.buffer), fn() {
    state.type_error(state, "TypedArray is not attached")
  })
  use Nil <- require(!require_shared || option.is_some(buf.storage), fn() {
    state.type_error(
      state,
      "Atomics.wait requires a SharedArrayBuffer TypedArray",
    )
  })
  use Nil <- require(!buf.detached, fn() {
    state.type_error(state, "ArrayBuffer is detached")
  })
  // ValidateTypedArray step 4 (immutable-arraybuffer proposal):
  // accessMode ~write~ on an immutable buffer → TypeError.
  use Nil <- require(!write || !buf.immutable, fn() {
    state.type_error(
      state,
      "Atomics operation cannot write to an immutable ArrayBuffer",
    )
  })
  // Live length: a resizable buffer may have shrunk below the view
  // (§10.4.5.12 IsTypedArrayOutOfBounds folds into this).
  let size = bits / 8
  let avail = { bit_array.byte_size(buf.bits) - view.byte_offset } / size
  let live = int.clamp(avail, 0, view.length)
  let info =
    TaInfo(
      buffer: view.buffer,
      elem_kind: view.elem_kind,
      byte_offset: view.byte_offset,
      length: live,
      bits:,
      signed:,
      storage: buf.storage,
    )
  // §25.4.3.2 ValidateAtomicAccess: ToIndex then bounds check.
  use idx, state <- coerce.to_index_cps(
    state,
    arg(args, 1),
    "Invalid atomic access index",
  )
  use Nil <- require(idx < live, fn() {
    state.range_error(state, "Atomics access index out of range")
  })
  cont(info, idx, state)
}

/// `bool.guard`-shaped helper for the dispatch-result shape: call `or` when
/// the condition fails, otherwise continue.
fn require(
  cond: Bool,
  alt: fn() -> #(State(host), Result(JsValue, JsValue)),
  cont: fn(Nil) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case cond {
    True -> cont(Nil)
    False -> alt()
  }
}

/// `require`'s Option twin for the dispatch-result shape: call `alt` when
/// the value is absent, otherwise continue with it.
fn some_or(
  opt: option.Option(a),
  alt: fn() -> #(State(host), Result(JsValue, JsValue)),
  cont: fn(a) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case opt {
    Some(v) -> cont(v)
    None -> alt()
  }
}

/// The internal slots of the TypedArray under validation (§25.4.3.1
/// step 1): its viewed buffer, element kind, view byte offset and view
/// element count.
type TaView {
  TaView(buffer: Ref, elem_kind: TypedArrayKind, byte_offset: Int, length: Int)
}

/// Pull the TypedArray internal slots out of a value, or None.
fn read_typed_array(state: State(host), val: JsValue) -> option.Option(TaView) {
  case val {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(
          kind: value.TypedArrayObject(
            buffer:,
            elem_kind:,
            byte_offset:,
            length:,
          ),
          ..,
        )) ->
          Some(TaView(
            buffer:,
            elem_kind:,
            byte_offset:,
            length: object.typed_array_view_length(
              state.heap,
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

/// One heap read of a viewed ArrayBuffer slot, fully destructured. This is
/// the ONLY place Atomics inspects [[ArrayBufferData]]: `storage` is
/// derived from `data` right here, so a "shared" flag can never disagree
/// with the storage it describes, and a write-back rebuilds the slot from
/// THESE fields rather than from a second heap read.
type BufferInfo {
  BufferInfo(
    /// The live [[ArrayBufferData]] storage (BufBytes | BufShared).
    data: value.BufferData,
    /// Byte snapshot of `data`. For shared (atomics-backed) storage this is
    /// a fresh copy of the live cells, so a re-read observes other agents'
    /// writes.
    bits: BitArray,
    /// The atomics ref of shared (cross-process) storage, or None for plain
    /// process-local byte storage. Derived from `data`, never stored as a
    /// separate flag. Some(_) means an RMW must go through the FFI's
    /// cell-CAS loop (cross-process atomicity); None means the snapshot
    /// path is trivially atomic (single process).
    storage: option.Option(value.AtomicsRef),
    detached: Bool,
    max_byte_length: option.Option(Int),
    immutable: Bool,
  )
}

/// Read the viewed buffer's slot, or None when `buffer` does not hold an
/// ArrayBuffer at all.
fn read_buffer(state: State(host), buffer: Ref) -> option.Option(BufferInfo) {
  case heap.read(state.heap, buffer) {
    Some(ObjectSlot(
      kind: value.ArrayBufferObject(
        data:,
        detached:,
        max_byte_length:,
        immutable:,
      ),
      ..,
    )) ->
      Some(BufferInfo(
        data:,
        bits: value.buffer_bits(data),
        storage: buffer_storage(data),
        detached:,
        max_byte_length:,
        immutable:,
      ))
    _ -> None
  }
}

/// The atomics ref backing shared (cross-process) storage, or None for
/// plain process-local byte storage. [[ArrayBufferData]] itself is the
/// single source of truth for shared-ness.
fn buffer_storage(data: value.BufferData) -> option.Option(value.AtomicsRef) {
  case data {
    value.BufShared(ref:, ..) -> Some(ref)
    value.BufBytes(..) -> None
  }
}

/// §25.4.3.4 RevalidateAtomicAccess — the value coercion may have run user
/// code that detached or shrank the buffer. Hands the continuation the live
/// buffer it just destructured; `write_element` persists into THAT, so a
/// second heap read that disagrees with this one (and would silently drop
/// the store) cannot exist.
fn revalidate(
  state: State(host),
  info: TaInfo,
  idx: Int,
  cont: fn(BufferInfo, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  use buf <- some_or(read_buffer(state, info.buffer), fn() {
    state.type_error(state, "TypedArray is not attached")
  })
  use Nil <- require(!buf.detached, fn() {
    state.type_error(state, "ArrayBuffer is detached")
  })
  let size = info.bits / 8
  let byte_off = info.byte_offset + idx * size
  use Nil <- require(byte_off + size <= bit_array.byte_size(buf.bits), fn() {
    state.range_error(state, "Atomics access index out of range")
  })
  cont(buf, state)
}

// ============================================================================
// Value coercion
// ============================================================================

fn arg(args: List(JsValue), idx: Int) -> JsValue {
  helpers.list_at(args, idx) |> option.unwrap(JsUndefined)
}

/// Coerce the operand for a read-modify-write/store per the array's content
/// type: ToBigInt for BigInt64/BigUint64, ToIntegerOrInfinity for the
/// integer kinds. ±∞ must be matched on the raw JsNum BEFORE the saturated
/// `coerce.jsnum_to_integer_or_infinity`: §7.1.7-§7.1.11 (ToInt8..ToUint32)
/// all map a non-finite ToIntegerOrInfinity result to +0, not to a huge
/// saturated integer whose low bits would be stored.
fn to_operand(
  state: State(host),
  info: TaInfo,
  val: JsValue,
  cont: fn(Int, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case info.bits {
    64 -> coerce.to_bigint_cps(state, val, cont)
    _ -> {
      use num, state <- coerce.try_to_number(state, val)
      case num {
        Infinity | NegInfinity -> cont(0, state)
        _ -> cont(coerce.jsnum_to_integer_or_infinity(num), state)
      }
    }
  }
}

// ============================================================================
// Raw element read/write
// ============================================================================

/// Truncate an arbitrary integer to the element's bit pattern, then
/// reinterpret per the element's signedness (two's complement).
fn wrap_to_kind(v: Int, bits: Int, signed: Bool) -> Int {
  let modulus = int.bitwise_shift_left(1, bits)
  let m = int.bitwise_and(v, modulus - 1)
  case signed && m >= modulus / 2 {
    True -> m - modulus
    False -> m
  }
}

/// Read element `idx` (already validated) from fresh buffer data.
fn read_element(data: BitArray, info: TaInfo, idx: Int) -> Int {
  let off = info.byte_offset + idx * { info.bits / 8 }
  ta_get_int(data, off, info.bits, info.signed)
}

/// Write raw integer `v` (truncated mod 2^bits by the FFI) at element `idx`
/// and persist it into the buffer slot `revalidate` just witnessed (`buf`) —
/// NOT into a second heap read that could disagree with the revalidation
/// and silently drop the store. Shared (atomics-backed) storage writes only
/// the element's bytes into the shared cells — other regions may be
/// concurrently written by other agent processes.
fn write_element(
  state: State(host),
  info: TaInfo,
  buf: BufferInfo,
  idx: Int,
  v: Int,
) -> State(host) {
  let size = info.bits / 8
  let off = info.byte_offset + idx * size
  let new_bits = ta_set_int(buf.bits, off, info.bits, v)
  let kind =
    value.ArrayBufferObject(
      data: value.buffer_store_region(buf.data, new_bits, off, size),
      detached: buf.detached,
      max_byte_length: buf.max_byte_length,
      immutable: buf.immutable,
    )
  State(..state, heap: heap.update_kind(state.heap, info.buffer, kind))
}

/// Old element value → JS value per content type.
fn element_to_js(info: TaInfo, raw: Int) -> JsValue {
  case info.elem_kind {
    value.BigInt64Kind | value.BigUint64Kind -> JsBigInt(BigInt(raw))
    _ -> value.from_int(raw)
  }
}

// ============================================================================
// §25.4.3.12 AtomicReadModifyWrite — add/and/or/xor/sub/exchange
// ============================================================================

fn rmw(
  args: List(JsValue),
  state: State(host),
  op: fn(Int, Int) -> Int,
) -> #(State(host), Result(JsValue, JsValue)) {
  use info, idx, state <- with_ta_and_index(
    state,
    args,
    waitable: False,
    require_shared: False,
    write: True,
  )
  use operand, state <- to_operand(state, info, arg(args, 2))
  use buf, state <- revalidate(state, info, idx)
  case info.storage {
    // Shared storage: other agent processes may race this element — the
    // whole read-compute-write must be one atomic step (§25.4.3.12), so the
    // FFI runs it as a CAS retry loop on the containing cell.
    Some(ref) -> {
      let off = info.byte_offset + idx * { info.bits / 8 }
      let old =
        sab_rmw_element(ref, off, info.bits, info.signed, fn(old) {
          op(old, operand)
        })
      #(state, Ok(element_to_js(info, old)))
    }
    // Process-local storage: no interleaving is possible, the snapshot
    // read-compute-write is trivially atomic.
    None -> {
      let old = read_element(buf.bits, info, idx)
      let state = write_element(state, info, buf, idx, op(old, operand))
      #(state, Ok(element_to_js(info, old)))
    }
  }
}

// §25.4.7 Atomics.compareExchange ( typedArray, index, expected, replacement )
fn compare_exchange(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use info, idx, state <- with_ta_and_index(
    state,
    args,
    waitable: False,
    require_shared: False,
    write: True,
  )
  use expected, state <- to_operand(state, info, arg(args, 2))
  use replacement, state <- to_operand(state, info, arg(args, 3))
  use buf, state <- revalidate(state, info, idx)
  let wrapped_expected = wrap_to_kind(expected, info.bits, info.signed)
  case info.storage {
    // Shared storage: compare and (conditional) store must be one atomic
    // step across agent processes — the FFI CAS-es the containing cell
    // against the value the comparison witnessed.
    Some(ref) -> {
      let off = info.byte_offset + idx * { info.bits / 8 }
      let old =
        sab_cas_element(
          ref,
          off,
          info.bits,
          info.signed,
          wrapped_expected,
          replacement,
        )
      #(state, Ok(element_to_js(info, old)))
    }
    // Process-local storage: snapshot compare-then-write is atomic.
    None -> {
      let old = read_element(buf.bits, info, idx)
      let state = case old == wrapped_expected {
        True -> write_element(state, info, buf, idx, replacement)
        False -> state
      }
      #(state, Ok(element_to_js(info, old)))
    }
  }
}

// §25.4.10 Atomics.load ( typedArray, index )
fn atomic_load(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use info, idx, state <- with_ta_and_index(
    state,
    args,
    waitable: False,
    require_shared: False,
    write: False,
  )
  // The index coercion may have run user code — revalidate (§25.4.10 step 2).
  use buf, state <- revalidate(state, info, idx)
  #(state, Ok(element_to_js(info, read_element(buf.bits, info, idx))))
}

// §25.4.12 Atomics.store ( typedArray, index, value ) — returns the COERCED
// value (ToIntegerOrInfinity / ToBigInt result), not the stored bit pattern.
fn atomic_store(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use info, idx, state <- with_ta_and_index(
    state,
    args,
    waitable: False,
    require_shared: False,
    write: True,
  )
  case info.bits {
    64 -> {
      use v, state <- coerce.to_bigint_cps(state, arg(args, 2))
      use buf, state <- revalidate(state, info, idx)
      let state = write_element(state, info, buf, idx, v)
      #(state, Ok(JsBigInt(BigInt(v))))
    }
    _ -> {
      // §25.4.10 step 3: v = 𝔽(? ToIntegerOrInfinity(value)) — and v itself
      // is the return value, so ±∞ must survive the coercion (the STORED
      // element is ToIntN(±∞) = +0). Match on the raw JsNum rather than the
      // saturated Int to keep both.
      use num, state <- coerce.try_to_number(state, arg(args, 2))
      use buf, state <- revalidate(state, info, idx)
      let #(stored, ret) = case num {
        Infinity -> #(0, JsNumber(Infinity))
        NegInfinity -> #(0, JsNumber(NegInfinity))
        _ -> {
          let n = coerce.jsnum_to_integer_or_infinity(num)
          #(n, value.from_int(n))
        }
      }
      let state = write_element(state, info, buf, idx, stored)
      #(state, Ok(ret))
    }
  }
}

// §25.4.9 Atomics.isLockFree ( size ) — must be consistent across calls;
// hardware lock-free sizes on every BEAM target are 1/2/4/8.
fn is_lock_free(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use n, state <- coerce.try_to_integer_or_infinity(
    state,
    helpers.first_arg_or_undefined(args),
  )
  let ok = case n {
    1 | 2 | 4 | 8 -> True
    _ -> False
  }
  #(state, Ok(JsBool(ok)))
}

// Atomics.pause ( [ iterationNumber ] ) — microwait proposal. No coercion:
// anything other than undefined or an integral Number is a TypeError. The
// pause itself is a no-op (single agent; nothing to spin-wait for).
fn pause(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case helpers.first_arg_or_undefined(args) {
    JsUndefined -> #(state, Ok(JsUndefined))
    JsNumber(Finite(f)) ->
      // `+. 0.0` normalizes -0.0 to +0.0: Gleam's == is Erlang =:=, which
      // distinguishes float zero signs, but -0 IS an integral Number.
      case f +. 0.0 == int.to_float(value.float_to_int(f)) {
        True -> #(state, Ok(JsUndefined))
        False ->
          state.type_error(state, "Atomics.pause: not an integral number")
      }
    _ -> state.type_error(state, "Atomics.pause: not an integral number")
  }
}

// ============================================================================
// §25.4.3.14 DoWait — Atomics.wait (sync) and Atomics.waitAsync (async)
// ============================================================================

/// The three observable outcomes of a wait (§25.4.3.14 DoWait): the woken,
/// timed-out and value-mismatch results. Every completion — the sync return
/// value, a waitAsync promise settlement, a cross-process wake — goes
/// through `wait_result_js`, so the spec's three result strings are spelled
/// exactly once.
type WaitResult {
  WaitedOk
  TimedOut
  NotEqual
}

/// The spec string a WaitResult surfaces to JS as.
fn wait_result_js(result: WaitResult) -> JsValue {
  JsString(case result {
    WaitedOk -> "ok"
    TimedOut -> "timed-out"
    NotEqual -> "not-equal"
  })
}

fn do_wait(
  args: List(JsValue),
  state: State(host),
  sync sync: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  use info, idx, state <- with_ta_and_index(
    state,
    args,
    waitable: True,
    require_shared: True,
    write: False,
  )
  // Step 6/7: v = ToBigInt64(value) | ToInt32(value).
  use v, state <- wait_value(state, info, arg(args, 2))
  // Step 8/9: t = ToNumber(timeout); NaN/undefined → +∞; clamp ≥ 0.
  use timeout_ms, state <- wait_timeout(state, arg(args, 3))
  // Step 10: if mode is sync and AgentCanSuspend() is false, throw a
  // TypeError. Sits after the value/timeout coercions (steps 6-9) per the
  // current spec text — the position is observable via valueOf side effects.
  use Nil, state <- check_agent_can_suspend(state, sync)
  // SharedArrayBuffers are never detached and never shrink, so the
  // validation-time data would do — but re-read for the freshest bytes
  // (the coercions above may have run Atomics.store via user code).
  use buf, state <- revalidate(state, info, idx)
  let w = read_element(buf.bits, info, idx)
  case w != v, timeout_ms, sync {
    // Value mismatch → "not-equal" (sync: string; async: {async:false,...}).
    True, _, True -> #(state, Ok(wait_result_js(NotEqual)))
    True, _, False -> wait_result_object(state, False, wait_result_js(NotEqual))
    // Zero timeout → immediate "timed-out".
    False, Some(0), True -> #(state, Ok(wait_result_js(TimedOut)))
    False, Some(0), False ->
      wait_result_object(state, False, wait_result_js(TimedOut))
    // Sync block (§25.4.3.14 steps 11+): register on the shared WaiterList
    // and suspend in a receive until a notify message or the timeout.
    False, Some(ms), True -> sync_block(state, info, idx, v, Some(ms))
    False, None, True -> sync_block(state, info, idx, v, None)
    // Async waiter: park a promise on State's waiter list (plus a token on
    // the shared WaiterList so notify — from any process — can count and
    // wake it). Un-notified infinite waiters stay pending, like a pending
    // host task.
    False, _, False -> {
      let #(h, promise_ref, data_ref) =
        builtins_promise.create_promise(
          state.heap,
          state.builtins.promise.prototype,
        )
      let size = info.bits / 8
      // §25.4.3.14 step 21: a finite timeout arms a timeout job that settles
      // the waiter with "timed-out"; the event loop fires it at `deadline`.
      let deadline = case timeout_ms {
        Some(ms) -> Some(state.ctx.host_hooks.monotonic_now() + ms)
        None -> None
      }
      let byte_off = info.byte_offset + idx * size
      let waiter =
        AtomicsWaiter(
          buffer: info.buffer,
          byte_offset: byte_off,
          promise_data: data_ref,
          promise: promise_ref,
          deadline:,
        )
      // Register an interchangeable token on the shared WaiterList: tokens
      // at the same (key, byte index, pid) are equivalent — settlement
      // always picks the first matching State waiter FIFO — so the handle
      // is deliberately discarded (expiry removes a token by match).
      let _token =
        ffi_insert_waiter(buffer_key(state, info.buffer), byte_off, True)
      let state =
        State(
          ..state,
          heap: h,
          atomics_waiters: list.append(state.atomics_waiters, [waiter]),
        )
      wait_result_object(state, True, JsObject(promise_ref))
    }
  }
}

/// §25.4.3.14 DoWait steps 11-27, sync mode: the blocking wait.
///
/// Ordering is the lock-free analogue of the spec's EnterCriticalSection →
/// AddWaiter → SuspendThisAgent sequence: we INSERT our waiterlist entry
/// first, then re-read the cell, then block. A racing store+notify that
/// lands before the insert is caught by the re-read ("not-equal"); one that
/// lands after the insert finds our entry and messages us — so no wakeup
/// can be lost. If the re-read disagrees, the entry is withdrawn (data-only
/// ets take) before returning.
///
/// The BLOCKING itself is not core's: the registered entry is handed to
/// the embedder's `state.ctx.host_hooks.sync_wait` capability (contract
/// clause 1, arc/host.gleam), which suspends in ITS mailbox until the
/// entry's wake message arrives or the timeout elapses — resolving the
/// notify-vs-timeout race exactly as the old in-core receive did.
fn sync_block(
  state: State(host),
  info: TaInfo,
  idx: Int,
  v: Int,
  timeout_ms: option.Option(Int),
) -> #(State(host), Result(JsValue, JsValue)) {
  let byte_off = info.byte_offset + idx * { info.bits / 8 }
  let key = buffer_key(state, info.buffer)
  let handle = ffi_insert_waiter(key, byte_off, False)
  // Fresh post-insert read: read_buffer snapshots the live shared cells.
  use buf, state <- revalidate(state, info, idx)
  case read_element(buf.bits, info, idx) == v {
    False -> {
      // Withdraw the entry (data-only ets:take). If a notifier claimed it
      // in this same instant (cancel returns True), its in-flight
      // {arc_notify, ref, ...} wake must NOT be left in our mailbox:
      // embedder loops match wakes by (key, byte index) — not ref — so a
      // stale wake would spuriously settle the next waitAsync waiter this
      // agent registers at the same address. Delegate the flush to the
      // embedder's `state.ctx.host_hooks.sync_wait` capability: its
      // await_notify selectively
      // receives on the entry's exact ref, and on the claimed path
      // (ets:take of our own key finds nothing) performs the same
      // safety-bounded flush receive the old in-core cancel did. Core
      // still performs no receive of its own.
      case ffi_cancel_waiter(handle) {
        True ->
          case state.ctx.host_hooks.sync_wait {
            Some(wait) -> {
              let _flushed: state.WaitOutcome =
                wait(state.WaitRequest(
                  handle:,
                  key:,
                  byte_index: byte_off,
                  timeout_ms: Some(0),
                ))
              Nil
            }
            // Unreachable: check_agent_can_suspend rejected sync mode
            // before AddWaiter when no capability is installed — and with
            // no embedder there is no mailbox loop to poison anyway.
            None -> Nil
          }
        False -> Nil
      }
      #(state, Ok(wait_result_js(NotEqual)))
    }
    True -> {
      case state.ctx.host_hooks.sync_wait {
        Some(wait) -> {
          let outcome =
            wait(state.WaitRequest(
              handle:,
              key:,
              byte_index: byte_off,
              // None = infinity; the embedder clamps to its receive ceiling.
              timeout_ms:,
            ))
          let result = case outcome {
            state.WaitOk -> WaitedOk
            state.WaitTimedOut -> TimedOut
          }
          #(state, Ok(wait_result_js(result)))
        }
        // Unreachable: check_agent_can_suspend rejected sync mode before
        // AddWaiter when no capability is installed. Fail safe by
        // withdrawing the entry rather than blocking on nothing (with no
        // capability there is no embedder mailbox loop to flush, so the
        // claimed/unclaimed distinction is discarded).
        None -> {
          let _claimed = ffi_cancel_waiter(handle)
          #(state, Ok(wait_result_js(TimedOut)))
        }
      }
    }
  }
}

/// DoWait step 10: if mode is sync and AgentCanSuspend() is false — the
/// agent's [[CanBlock]] is false — throw a TypeError. arc's main agent and
/// spawned agent children can always block; the flag is only False when the
/// host opted out (test262's CanBlockIsFalse), threaded in via
/// State.can_block at realm boot. A host that supplied no
/// `state.ctx.host_hooks.sync_wait` capability cannot suspend the agent
/// either (contract clause 1): treated identically to [[CanBlock]] = false.
/// waitAsync never blocks, so async mode is exempt.
fn check_agent_can_suspend(
  state: State(host),
  sync: Bool,
  cont: fn(Nil, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  let host_can_suspend = option.is_some(state.ctx.host_hooks.sync_wait)
  case sync && { !state.can_block || !host_can_suspend } {
    True ->
      state.type_error(state, "Atomics.wait cannot be called in this agent")
    False -> cont(Nil, state)
  }
}

/// DoWait steps 6/7: Int32Array → ToInt32, BigInt64Array → ToBigInt64.
fn wait_value(
  state: State(host),
  info: TaInfo,
  val: JsValue,
  cont: fn(Int, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case info.bits {
    64 -> {
      use n, state <- coerce.to_bigint_cps(state, val)
      cont(wrap_to_kind(n, 64, True), state)
    }
    _ ->
      case coerce.js_to_number(state, val) {
        Error(#(thrown, state)) -> #(state, Error(thrown))
        Ok(#(num, state)) ->
          cont(wrap_to_kind(jsnum_to_int(num), 32, True), state)
      }
  }
}

/// ToInt32's ToNumber tail: NaN/±∞ → 0, finite → truncate (wrap happens in
/// wrap_to_kind).
fn jsnum_to_int(num: value.JsNum) -> Int {
  case num {
    Finite(f) -> value.float_to_int(f)
    _ -> 0
  }
}

/// DoWait steps 8/9: timeout in milliseconds. None = +∞.
fn wait_timeout(
  state: State(host),
  val: JsValue,
  cont: fn(option.Option(Int), State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case coerce.js_to_number(state, val) {
    Error(#(thrown, state)) -> #(state, Error(thrown))
    Ok(#(num, state)) ->
      case num {
        NaN | Infinity -> cont(None, state)
        NegInfinity -> cont(Some(0), state)
        Finite(f) ->
          case f <=. 0.0 {
            True -> cont(Some(0), state)
            False -> cont(Some(float.round(float.ceiling(f))), state)
          }
      }
  }
}

/// §25.4.3.13 CreateResultObject for waitAsync: { async, value } on
/// %Object.prototype%.
fn wait_result_object(
  state: State(host),
  is_async: Bool,
  val: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(h, ref) =
    common.alloc_pojo(state.heap, state.builtins.object.prototype, [
      #("async", value.data(JsBool(is_async))),
      #("value", value.data(val)),
    ])
  #(State(..state, heap: h), Ok(JsObject(ref)))
}

// ============================================================================
// §25.4.11 Atomics.notify ( typedArray, index, count )
// ============================================================================

fn notify(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use info, idx, state <- with_ta_and_index(
    state,
    args,
    waitable: True,
    require_shared: False,
    write: False,
  )
  // Step 3: count — undefined → +∞, else ToIntegerOrInfinity clamped ≥ 0.
  // Coerced BEFORE the non-shared early return (observable, test262 checks).
  use count, state <- notify_count(state, arg(args, 2))
  // Step 6: non-shared buffers can have no waiters → +0.
  case info.storage {
    None -> #(state, Ok(value.from_int(0)))
    Some(sab_ref) -> {
      let byte_off = info.byte_offset + idx * { info.bits / 8 }
      // Atomically claim up to `count` waiters FIFO from the shared
      // WaiterList (data-only ets:take loop). Claiming is the spec's
      // "woken" accounting (§25.4.3.11 NotifyWaiter): remote waiters
      // (blocked sync waits and other agents' waitAsync tokens) come back
      // as claims whose wake-message DELIVERY is the embedder's, via the
      // `state.ctx.host_hooks.deliver_wake` capability; our own waitAsync
      // tokens come back as a count to settle on State right here (pure
      // data, no message).
      let #(claimed, self_async) =
        ffi_take_waiters(ffi_shared_buffer_key(sab_ref), byte_off, count)
      let Nil = case claimed, state.ctx.host_hooks.deliver_wake {
        [], _ -> Nil
        _, Some(deliver) -> deliver(claimed)
        // No embedder capability: claims still count as woken, but with no
        // capability supplied nothing remote can be blocked on us anyway
        // (sync wait requires state.ctx.host_hooks.sync_wait; cross-process
        // waitAsync requires a multi-agent embedder).
        _, None -> Nil
      }
      let remote_woken = list.length(claimed)
      let #(state, settled) =
        settle_n_matching(state, info.buffer, byte_off, self_async)
      #(state, Ok(value.from_int(remote_woken + settled)))
    }
  }
}

/// Settle the first `n` pending State waitAsync waiters on
/// (buffer, byte_off) as woken, FIFO. Returns how many were settled
/// (equal to `n` unless tokens and State momentarily disagree, in which
/// case the settled count is the truthful one for this agent).
fn settle_n_matching(
  state: State(host),
  buffer: Ref,
  byte_off: Int,
  n: Int,
) -> #(State(host), Int) {
  let #(woken, kept, _) =
    list.fold(state.atomics_waiters, #([], [], n), fn(acc, waiter) {
      let #(woken, kept, remaining) = acc
      let matches = waiter.buffer == buffer && waiter.byte_offset == byte_off
      case matches && remaining > 0 {
        True -> #([waiter, ..woken], kept, remaining - 1)
        False -> #(woken, [waiter, ..kept], remaining)
      }
    })
  let woken = list.reverse(woken)
  let state =
    list.fold(
      woken,
      State(..state, atomics_waiters: list.reverse(kept)),
      fn(state, waiter) { settle_waiter(state, waiter, WaitedOk) },
    )
  #(state, list.length(woken))
}

/// Fulfill a waitAsync waiter's promise with the given wait outcome,
/// appending its reaction jobs to the job queue.
fn settle_waiter(
  state: State(host),
  waiter: value.AtomicsWaiter,
  result: WaitResult,
) -> State(host) {
  builtins_promise.fulfill_promise(
    state,
    waiter.promise_data,
    wait_result_js(result),
  )
}

/// Notify count (§25.4.13 step 3): undefined → effectively unbounded;
/// otherwise max(ToIntegerOrInfinity(count), 0). The saturated +∞
/// (2^53 - 1) is already "unbounded" for any realistic waiter list, and
/// -∞ saturates negative so the max(_, 0) clamps it to 0.
fn notify_count(
  state: State(host),
  val: JsValue,
  cont: fn(Int, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case val {
    JsUndefined -> cont(limits.max_safe_integer, state)
    _ -> {
      use n, state <- coerce.try_to_integer_or_infinity(state, val)
      cont(int.max(n, 0), state)
    }
  }
}

// ============================================================================
// waitAsync timeout jobs — fired by the event loop between microtasks
// ============================================================================

/// Settle every pending waitAsync waiter whose deadline has passed as
/// timed out (§25.4.3.14 DoWait timeout path). Reaction jobs are appended
/// to the job queue; the caller's drain loop runs them.
pub fn settle_expired_waiters(state: State(host)) -> State(host) {
  case state.atomics_waiters {
    [] -> state
    waiters -> {
      let now = state.ctx.host_hooks.monotonic_now()
      let #(expired, pending) =
        list.partition(waiters, fn(w) {
          case w.deadline {
            Some(d) -> d <= now
            None -> False
          }
        })
      list.fold(
        expired,
        State(..state, atomics_waiters: pending),
        fn(state, waiter) {
          // Drop one of our tokens from the shared WaiterList so a future
          // notify cannot count this settled waiter. (If a notifier in
          // another process claimed the token in this same instant, its
          // in-flight message finds no pending State waiter and is
          // dropped — see the race note in arc_waiter_ffi.erl.)
          let Nil =
            ffi_remove_async_token(
              buffer_key(state, waiter.buffer),
              waiter.byte_offset,
            )
          settle_waiter(state, waiter, TimedOut)
        },
      )
    }
  }
}

/// Settle this agent's first pending waitAsync waiter on (key, byte index)
/// as woken — called by the event loop when its bounded dry-queue receive
/// is woken by a cross-process Atomics.notify message. A stale message
/// (the waiter already expired) settles nothing.
pub fn settle_notified_waiter(
  state: State(host),
  key: WaiterKey,
  byte_index: Int,
) -> State(host) {
  case pop_first_matching(state.atomics_waiters, key, byte_index, state, []) {
    None -> state
    Some(#(waiter, rest)) ->
      settle_waiter(State(..state, atomics_waiters: rest), waiter, WaitedOk)
  }
}

/// First State waiter whose buffer maps to `key` at `byte_index`, plus the
/// remaining list in order.
fn pop_first_matching(
  waiters: List(value.AtomicsWaiter),
  key: WaiterKey,
  byte_index: Int,
  state: State(host),
  seen: List(value.AtomicsWaiter),
) -> option.Option(#(value.AtomicsWaiter, List(value.AtomicsWaiter))) {
  case waiters {
    [] -> None
    [w, ..rest] ->
      case w.byte_offset == byte_index && buffer_key(state, w.buffer) == key {
        True -> Some(#(w, list.append(list.reverse(seen), rest)))
        False -> pop_first_matching(rest, key, byte_index, state, [w, ..seen])
      }
  }
}

/// Earliest finite deadline among pending waitAsync waiters, if any. The
/// event loop sleeps until this when the microtask queue runs dry.
pub fn earliest_waiter_deadline(state: State(host)) -> option.Option(Int) {
  list.fold(state.atomics_waiters, None, fn(acc, w) {
    case acc, w.deadline {
      None, d -> d
      Some(a), Some(d) -> Some(int.min(a, d))
      Some(a), None -> Some(a)
    }
  })
}
