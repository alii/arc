//// ES2024 §25.4 The Atomics Object.
////
//// Arc is single-agent: all JS runs on one BEAM process, so the atomic
//// read-modify-write operations are trivially atomic (no interleaving is
//// possible). `Atomics.wait` can block the (only) agent — the main agent's
//// [[CanBlock]] is true, matching QuickJS — but since no other agent exists
//// a sync wait can only ever time out. `Atomics.waitAsync` waiters are kept
//// on State (FIFO) and settled with "ok" by `Atomics.notify` from the same
//// agent, or never (no timer infrastructure in the core event loop).
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
import arc/vm/internal/job_queue
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
pub fn init(h: Heap, object_proto: Ref, function_proto: Ref) -> #(Heap, Ref) {
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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

@external(erlang, "arc_atomics_ffi", "sleep")
fn ffi_sleep(ms: Int) -> Nil

@external(erlang, "arc_atomics_ffi", "sleep_forever")
fn ffi_sleep_forever() -> Nil

/// Monotonic clock in milliseconds. Public: the event loop and the $262
/// agent natives use the same clock as waitAsync deadline bookkeeping.
@external(erlang, "arc_atomics_ffi", "monotonic_now")
pub fn monotonic_now() -> Int

/// Blocking sleep, public for the event loop's waitAsync-timeout wait.
pub fn sleep_ms(ms: Int) -> Nil {
  ffi_sleep(ms)
}

/// True while a cooperative $262.agent broadcast callback runs (set by
/// realm.agent_broadcast_native). Infinite sync waits are bounded in that
/// mode — nothing can ever notify them, so blocking forever would deadlock
/// the host process.
@external(erlang, "arc_atomics_ffi", "in_agent_callback_mode")
fn in_agent_callback_mode() -> Bool

/// Flip the cooperative-agent-callback flag. Public for realm.gleam.
@external(erlang, "arc_atomics_ffi", "set_agent_callback_mode")
pub fn set_agent_callback_mode(on: Bool) -> Nil

/// Bound for an unnotifiable infinite sync wait inside a cooperative agent
/// callback. Long enough to be observably "a real wait", short enough not
/// to stall a test run.
const agent_infinite_wait_cap_ms: Int = 1000

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
    shared: Bool,
  )
}

/// Allowed element kinds per Table 60 (§25.4.3.1): the 8 integer kinds for
/// general ops; only Int32/BigInt64 when `waitable`. Returns #(bits, signed).
fn kind_bits(
  kind: TypedArrayKind,
  waitable: Bool,
) -> Result(#(Int, Bool), Nil) {
  case waitable, kind {
    True, value.Int32Kind -> Ok(#(32, True))
    True, value.BigInt64Kind -> Ok(#(64, True))
    True, _ -> Error(Nil)
    False, value.Int8Kind -> Ok(#(8, True))
    False, value.Uint8Kind -> Ok(#(8, False))
    False, value.Int16Kind -> Ok(#(16, True))
    False, value.Uint16Kind -> Ok(#(16, False))
    False, value.Int32Kind -> Ok(#(32, True))
    False, value.Uint32Kind -> Ok(#(32, False))
    False, value.BigInt64Kind -> Ok(#(64, True))
    False, value.BigUint64Kind -> Ok(#(64, False))
    False, _ -> Error(Nil)
  }
}

/// §25.4.3.1 ValidateIntegerTypedArray + §25.4.3.3 ValidateAtomicAccessOn-
/// IntegerTypedArray: validate `args[0]` as an integer TypedArray of an
/// allowed kind, then coerce `args[1]` with ToIndex and bounds-check it.
/// `require_shared` is the wait/waitAsync mode (non-shared → TypeError before
/// the index is even coerced — observable, test262 checks it).
fn with_ta_and_index(
  state: State,
  args: List(JsValue),
  waitable waitable: Bool,
  require_shared require_shared: Bool,
  cont cont: fn(TaInfo, Int, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  let ta_val = helpers.first_arg_or_undefined(args)
  case read_typed_array(state, ta_val) {
    None ->
      state.type_error(state, "Atomics operation needs an integer TypedArray")
    Some(#(buffer, elem_kind, byte_offset, length)) ->
      case kind_bits(elem_kind, waitable) {
        Error(Nil) ->
          state.type_error(
            state,
            "Invalid TypedArray element type for Atomics operation",
          )
        Ok(#(bits, signed)) ->
          case read_buffer(state, buffer) {
            None -> state.type_error(state, "TypedArray is not attached")
            Some(#(data, detached, shared)) -> {
              use Nil <- require(!require_shared || shared, fn() {
                state.type_error(
                  state,
                  "Atomics.wait requires a SharedArrayBuffer TypedArray",
                )
              })
              use Nil <- require(!detached, fn() {
                state.type_error(state, "ArrayBuffer is detached")
              })
              // Live length: a resizable buffer may have shrunk below the
              // view (§10.4.5.12 IsTypedArrayOutOfBounds folds into this).
              let size = bits / 8
              let avail = { bit_array.byte_size(data) - byte_offset } / size
              let live = int.clamp(avail, 0, length)
              let info =
                TaInfo(
                  buffer:,
                  elem_kind:,
                  byte_offset:,
                  length: live,
                  bits:,
                  signed:,
                  shared:,
                )
              // §25.4.3.2 ValidateAtomicAccess: ToIndex then bounds check.
              use idx, state <- to_index(state, arg(args, 1))
              case idx < live {
                False ->
                  state.range_error(state, "Atomics access index out of range")
                True -> cont(info, idx, state)
              }
            }
          }
      }
  }
}

/// `bool.guard`-shaped helper for the dispatch-result shape: call `or` when
/// the condition fails, otherwise continue.
fn require(
  cond: Bool,
  alt: fn() -> #(State, Result(JsValue, JsValue)),
  cont: fn(Nil) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case cond {
    True -> cont(Nil)
    False -> alt()
  }
}

/// Pull the TypedArray internal slots out of a value, or None.
fn read_typed_array(
  state: State,
  val: JsValue,
) -> option.Option(#(Ref, TypedArrayKind, Int, Int)) {
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
          Some(#(
            buffer,
            elem_kind,
            byte_offset,
            object.typed_array_view_length(
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

/// Read the viewed buffer's (data, detached, shared) triple.
fn read_buffer(
  state: State,
  buffer: Ref,
) -> option.Option(#(BitArray, Bool, Bool)) {
  case heap.read(state.heap, buffer) {
    Some(ObjectSlot(
      kind: value.ArrayBufferObject(data:, detached:, shared:, ..),
      ..,
    )) -> Some(#(data, detached, shared))
    _ -> None
  }
}

/// §25.4.3.4 RevalidateAtomicAccess — the value coercion may have run user
/// code that detached or shrank the buffer. Returns the fresh data on success.
fn revalidate(
  state: State,
  info: TaInfo,
  idx: Int,
  cont: fn(BitArray, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case read_buffer(state, info.buffer) {
    None -> state.type_error(state, "TypedArray is not attached")
    Some(#(_, True, _)) -> state.type_error(state, "ArrayBuffer is detached")
    Some(#(data, False, _)) -> {
      let size = info.bits / 8
      let byte_off = info.byte_offset + idx * size
      case byte_off + size <= bit_array.byte_size(data) {
        False -> state.range_error(state, "Atomics access index out of range")
        True -> cont(data, state)
      }
    }
  }
}

// ============================================================================
// Value coercion
// ============================================================================

fn arg(args: List(JsValue), idx: Int) -> JsValue {
  helpers.list_at(args, idx) |> option.unwrap(JsUndefined)
}

/// §7.1.22 ToIndex (CPS): undefined → 0; else ToIntegerOrInfinity with a
/// RangeError outside [0, 2^53-1].
fn to_index(
  state: State,
  val: JsValue,
  cont: fn(Int, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case val {
    JsUndefined -> cont(0, state)
    _ ->
      case coerce.js_to_number(state, val) {
        Error(#(thrown, state)) -> #(state, Error(thrown))
        Ok(#(num, state)) ->
          case num {
            Finite(f) -> {
              let i = value.float_to_int(f)
              case i < 0 || i > 9_007_199_254_740_991 {
                True -> state.range_error(state, "Invalid atomic access index")
                False -> cont(i, state)
              }
            }
            NaN -> cont(0, state)
            Infinity | NegInfinity ->
              state.range_error(state, "Invalid atomic access index")
          }
      }
  }
}

/// §7.1.5 ToIntegerOrInfinity (CPS) — keeps ±∞ distinct (callers decide how
/// to clamp). NaN/±0 → 0.
fn to_integer_or_infinity(
  state: State,
  val: JsValue,
  cont: fn(IntOrInf, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case coerce.js_to_number(state, val) {
    Error(#(thrown, state)) -> #(state, Error(thrown))
    Ok(#(num, state)) ->
      case num {
        Finite(f) -> cont(IntVal(value.float_to_int(f)), state)
        NaN -> cont(IntVal(0), state)
        Infinity -> cont(PosInf, state)
        NegInfinity -> cont(NegInf, state)
      }
  }
}

type IntOrInf {
  IntVal(Int)
  PosInf
  NegInf
}

/// Coerce the operand for a read-modify-write/store per the array's content
/// type: ToBigInt for BigInt64/BigUint64, ToIntegerOrInfinity (±∞ → 0 on
/// store) for the integer kinds.
fn to_operand(
  state: State,
  info: TaInfo,
  val: JsValue,
  cont: fn(Int, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case info.bits {
    64 -> coerce.to_bigint_cps(state, val, cont)
    _ ->
      to_integer_or_infinity(state, val, fn(i, state) {
        case i {
          IntVal(n) -> cont(n, state)
          PosInf | NegInf -> cont(0, state)
        }
      })
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
/// and persist the new binary into the buffer slot.
fn write_element(
  state: State,
  info: TaInfo,
  data: BitArray,
  idx: Int,
  v: Int,
) -> State {
  let off = info.byte_offset + idx * { info.bits / 8 }
  let new_data = ta_set_int(data, off, info.bits, v)
  case heap.read(state.heap, info.buffer) {
    Some(
      ObjectSlot(
        kind: value.ArrayBufferObject(detached:, max_byte_length:, shared:, ..),
        ..,
      ) as slot,
    ) -> {
      let h =
        heap.write(
          state.heap,
          info.buffer,
          ObjectSlot(
            ..slot,
            kind: value.ArrayBufferObject(
              data: new_data,
              detached:,
              max_byte_length:,
              shared:,
            ),
          ),
        )
      State(..state, heap: h)
    }
    // Unreachable: revalidate() just read this slot as a live buffer.
    _ -> state
  }
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
  state: State,
  op: fn(Int, Int) -> Int,
) -> #(State, Result(JsValue, JsValue)) {
  use info, idx, state <- with_ta_and_index(
    state,
    args,
    waitable: False,
    require_shared: False,
  )
  use operand, state <- to_operand(state, info, arg(args, 2))
  use data, state <- revalidate(state, info, idx)
  let old = read_element(data, info, idx)
  let state = write_element(state, info, data, idx, op(old, operand))
  #(state, Ok(element_to_js(info, old)))
}

// §25.4.7 Atomics.compareExchange ( typedArray, index, expected, replacement )
fn compare_exchange(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use info, idx, state <- with_ta_and_index(
    state,
    args,
    waitable: False,
    require_shared: False,
  )
  use expected, state <- to_operand(state, info, arg(args, 2))
  use replacement, state <- to_operand(state, info, arg(args, 3))
  use data, state <- revalidate(state, info, idx)
  let old = read_element(data, info, idx)
  let state = case old == wrap_to_kind(expected, info.bits, info.signed) {
    True -> write_element(state, info, data, idx, replacement)
    False -> state
  }
  #(state, Ok(element_to_js(info, old)))
}

// §25.4.10 Atomics.load ( typedArray, index )
fn atomic_load(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use info, idx, state <- with_ta_and_index(
    state,
    args,
    waitable: False,
    require_shared: False,
  )
  // The index coercion may have run user code — revalidate (§25.4.10 step 2).
  use data, state <- revalidate(state, info, idx)
  #(state, Ok(element_to_js(info, read_element(data, info, idx))))
}

// §25.4.12 Atomics.store ( typedArray, index, value ) — returns the COERCED
// value (ToIntegerOrInfinity / ToBigInt result), not the stored bit pattern.
fn atomic_store(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use info, idx, state <- with_ta_and_index(
    state,
    args,
    waitable: False,
    require_shared: False,
  )
  case info.bits {
    64 -> {
      use v, state <- coerce.to_bigint_cps(state, arg(args, 2))
      use data, state <- revalidate(state, info, idx)
      let state = write_element(state, info, data, idx, v)
      #(state, Ok(JsBigInt(BigInt(v))))
    }
    _ -> {
      use i, state <- to_integer_or_infinity(state, arg(args, 2))
      use data, state <- revalidate(state, info, idx)
      let #(stored, ret) = case i {
        IntVal(n) -> #(n, value.from_int(n))
        PosInf -> #(0, JsNumber(Infinity))
        NegInf -> #(0, JsNumber(NegInfinity))
      }
      let state = write_element(state, info, data, idx, stored)
      #(state, Ok(ret))
    }
  }
}

// §25.4.9 Atomics.isLockFree ( size ) — must be consistent across calls;
// hardware lock-free sizes on every BEAM target are 1/2/4/8.
fn is_lock_free(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use n, state <- to_integer_or_infinity(
    state,
    helpers.first_arg_or_undefined(args),
  )
  let ok = case n {
    IntVal(1) | IntVal(2) | IntVal(4) | IntVal(8) -> True
    _ -> False
  }
  #(state, Ok(JsBool(ok)))
}

// Atomics.pause ( [ iterationNumber ] ) — microwait proposal. No coercion:
// anything other than undefined or an integral Number is a TypeError. The
// pause itself is a no-op (single agent; nothing to spin-wait for).
fn pause(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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

fn do_wait(
  args: List(JsValue),
  state: State,
  sync sync: Bool,
) -> #(State, Result(JsValue, JsValue)) {
  use info, idx, state <- with_ta_and_index(
    state,
    args,
    waitable: True,
    require_shared: True,
  )
  // Step 6/7: v = ToBigInt64(value) | ToInt32(value).
  use v, state <- wait_value(state, info, arg(args, 2))
  // Step 8/9: t = ToNumber(timeout); NaN/undefined → +∞; clamp ≥ 0.
  use timeout_ms, state <- wait_timeout(state, arg(args, 3))
  // Step 10: main agent [[CanBlock]] is true in arc (like QuickJS) — no
  // TypeError for sync mode.
  // SharedArrayBuffers are never detached and never shrink, so the
  // validation-time data would do — but re-read for the freshest bytes
  // (the coercions above may have run Atomics.store via user code).
  use data, state <- revalidate(state, info, idx)
  let w = read_element(data, info, idx)
  case w != v, timeout_ms, sync {
    // Value mismatch → "not-equal" (sync: string; async: {async:false,...}).
    True, _, True -> #(state, Ok(JsString("not-equal")))
    True, _, False -> wait_result_object(state, False, JsString("not-equal"))
    // Zero timeout → immediate "timed-out".
    False, Some(0), True -> #(state, Ok(JsString("timed-out")))
    False, Some(0), False ->
      wait_result_object(state, False, JsString("timed-out"))
    // Sync block: arc is single-agent, so nobody can ever notify a sync
    // waiter — suspend for the timeout, then report "timed-out". An infinite
    // sync wait deadlocks by construction (same as any engine whose only
    // agent waits forever).
    False, Some(ms), True -> {
      let Nil = ffi_sleep(ms)
      #(state, Ok(JsString("timed-out")))
    }
    False, None, True ->
      case in_agent_callback_mode() {
        // Cooperative agent callback: the main script is suspended until
        // this callback returns, so no notify can ever arrive — bound the
        // wait instead of deadlocking the host process.
        True -> {
          let Nil = ffi_sleep(agent_infinite_wait_cap_ms)
          #(state, Ok(JsString("timed-out")))
        }
        False -> {
          let Nil = ffi_sleep_forever()
          // Unreachable: sleep_forever never returns.
          #(state, Ok(JsString("timed-out")))
        }
      }
    // Async waiter: park a promise on the waiter list; Atomics.notify from
    // this agent settles it with "ok". (No timer infrastructure in the core
    // event loop — un-notified finite waiters stay pending, like a pending
    // host task.)
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
        Some(ms) -> Some(monotonic_now() + ms)
        None -> None
      }
      let waiter =
        AtomicsWaiter(
          buffer: info.buffer,
          byte_offset: info.byte_offset + idx * size,
          promise_data: data_ref,
          promise: promise_ref,
          deadline:,
        )
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

/// DoWait steps 6/7: Int32Array → ToInt32, BigInt64Array → ToBigInt64.
fn wait_value(
  state: State,
  info: TaInfo,
  val: JsValue,
  cont: fn(Int, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
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
  state: State,
  val: JsValue,
  cont: fn(option.Option(Int), State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
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
  state: State,
  is_async: Bool,
  val: JsValue,
) -> #(State, Result(JsValue, JsValue)) {
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use info, idx, state <- with_ta_and_index(
    state,
    args,
    waitable: True,
    require_shared: False,
  )
  // Step 3: count — undefined → +∞, else ToIntegerOrInfinity clamped ≥ 0.
  // Coerced BEFORE the non-shared early return (observable, test262 checks).
  use count, state <- notify_count(state, arg(args, 2))
  // Step 6: non-shared buffers can have no waiters → +0.
  case info.shared {
    False -> #(state, Ok(value.from_int(0)))
    True -> {
      let byte_off = info.byte_offset + idx * { info.bits / 8 }
      let #(woken, kept, _) =
        list.fold(state.atomics_waiters, #([], [], count), fn(acc, waiter) {
          let #(woken, kept, remaining) = acc
          let matches =
            waiter.buffer == info.buffer && waiter.byte_offset == byte_off
          case matches && remaining > 0 {
            True -> #([waiter, ..woken], kept, remaining - 1)
            False -> #(woken, [waiter, ..kept], remaining)
          }
        })
      let woken = list.reverse(woken)
      let kept = list.reverse(kept)
      // §25.4.3.11 NotifyWaiter: settle each woken promise with "ok".
      let state =
        list.fold(
          woken,
          State(..state, atomics_waiters: kept),
          fn(state, waiter) {
            let #(h, jobs) =
              builtins_promise.fulfill_promise(
                state.heap,
                waiter.promise_data,
                JsString("ok"),
              )
            State(
              ..state,
              heap: h,
              job_queue: job_queue.append(state.job_queue, jobs),
            )
          },
        )
      #(state, Ok(value.from_int(list.length(woken))))
    }
  }
}

/// Notify count: undefined → effectively unbounded; negative → 0.
fn notify_count(
  state: State,
  val: JsValue,
  cont: fn(Int, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case val {
    JsUndefined -> cont(9_007_199_254_740_991, state)
    _ ->
      to_integer_or_infinity(state, val, fn(i, state) {
        case i {
          IntVal(n) -> cont(int.max(n, 0), state)
          PosInf -> cont(9_007_199_254_740_991, state)
          NegInf -> cont(0, state)
        }
      })
  }
}

// ============================================================================
// waitAsync timeout jobs — fired by the event loop between microtasks
// ============================================================================

/// Settle every pending waitAsync waiter whose deadline has passed with
/// "timed-out" (§25.4.3.14 DoWait timeout path). Reaction jobs are appended
/// to the job queue; the caller's drain loop runs them.
pub fn settle_expired_waiters(state: State) -> State {
  case state.atomics_waiters {
    [] -> state
    waiters -> {
      let now = monotonic_now()
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
          let #(h, jobs) =
            builtins_promise.fulfill_promise(
              state.heap,
              waiter.promise_data,
              JsString("timed-out"),
            )
          State(
            ..state,
            heap: h,
            job_queue: job_queue.append(state.job_queue, jobs),
          )
        },
      )
    }
  }
}

/// Earliest finite deadline among pending waitAsync waiters, if any. The
/// event loop sleeps until this when the microtask queue runs dry.
pub fn earliest_waiter_deadline(state: State) -> option.Option(Int) {
  list.fold(state.atomics_waiters, None, fn(acc, w) {
    case acc, w.deadline {
      None, d -> d
      Some(a), Some(d) -> Some(int.min(a, d))
      Some(a), None -> Some(a)
    }
  })
}
