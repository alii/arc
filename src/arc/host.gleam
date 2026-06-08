//// Helpers for writing host functions.
////
//// Validators — strict type checks that throw TypeError on mismatch,
//// designed for `use` syntax. Modeled after Node's `internal/validators`.
//// Error format:
////   The "NAME" argument must be of type EXPECTED. Received type ACTUAL
////
//// Usage:
////
////     fn host_repeat(args, _this, s) {
////       case args {
////         [str, n, ..] -> {
////           use str, s <- host.validate_string(s, str, "str")
////           use n, s <- host.validate_integer(s, n, "count", 0, 1_000_000)
////           #(s, Ok(JsString(string.repeat(str, n))))
////         }
////         _ -> state.type_error(s, "repeat: expected (str, count)")
////       }
////     }

import arc/vm/builtins/common
import arc/vm/builtins/helpers
import arc/vm/builtins/promise as builtins_promise
import arc/vm/internal/job_queue
import arc/vm/state.{type State}
import arc/vm/value.{
  type JsValue, type Ref, Finite, JsBool, JsNumber, JsObject, JsString,
}
import gleam/int
import gleam/list
import gleam/option

// -- Validators --------------------------------------------------------------

/// Reject unless `val` is a JS string. Unwraps to the Gleam `String`.
pub fn validate_string(
  s: State,
  val: JsValue,
  name: String,
  cont: fn(String, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case val {
    JsString(str) -> cont(str, s)
    _ -> invalid_arg_type(s, name, "string", val)
  }
}

/// Reject unless `val` is callable. Passes the value through unchanged —
/// hand it to `state.try_call` to invoke. Use this when you call the function
/// more than once (validate once, call many). For one-shot calls, `try_call`
/// does both in one step.
pub fn validate_function(
  s: State,
  val: JsValue,
  name: String,
  cont: fn(JsValue, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case helpers.is_callable(s.heap, val) {
    True -> cont(val, s)
    False -> invalid_arg_type(s, name, "function", val)
  }
}

/// Validate callability AND call — one-shot combination of `validate_function`
/// and `state.try_call`. If `callee` isn't callable, throws TypeError with
/// the arg name; otherwise calls it and propagates the result or any throw.
pub fn try_call(
  s: State,
  callee: JsValue,
  name: String,
  this_val: JsValue,
  args: List(JsValue),
  cont: fn(JsValue, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case helpers.is_callable(s.heap, callee) {
    True -> state.try_call(s, callee, this_val, args, cont)
    False -> invalid_arg_type(s, name, "function", callee)
  }
}

/// Reject unless `val` is an integer-valued JS number within `[min, max]`.
/// Unwraps to `Int`. Throws RangeError if out of bounds, TypeError if not
/// a number.
pub fn validate_integer(
  s: State,
  val: JsValue,
  name: String,
  min: Int,
  max: Int,
  cont: fn(Int, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case val {
    JsNumber(Finite(n)) -> {
      let i = value.float_to_int(n)
      case int.to_float(i) == n {
        False -> invalid_arg_type(s, name, "integer", val)
        True ->
          case i >= min && i <= max {
            True -> cont(i, s)
            False ->
              state.range_error(
                s,
                "The value of \""
                  <> name
                  <> "\" is out of range. It must be >= "
                  <> int.to_string(min)
                  <> " and <= "
                  <> int.to_string(max)
                  <> ". Received "
                  <> int.to_string(i),
              )
          }
      }
    }
    _ -> invalid_arg_type(s, name, "integer", val)
  }
}

/// Reject unless `val` is a JS boolean. Unwraps to `Bool`.
pub fn validate_boolean(
  s: State,
  val: JsValue,
  name: String,
  cont: fn(Bool, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case val {
    JsBool(b) -> cont(b, s)
    _ -> invalid_arg_type(s, name, "boolean", val)
  }
}

// -- Suspend / resume --------------------------------------------------------
//
// The macrotask loop is the embedder's. Core only knows about Promises and
// the microtask queue. These two functions are the bridge: a host function
// hands JS a pending Promise and walks away with a settle handle; later,
// from its own loop (BEAM mailbox, libuv, epoll, whatever), it calls
// `resume` to settle that Promise and re-enters `drain_jobs`.
//
//     fn fetch(args, _this, s) {
//       let #(s, promise, ticket) = host.suspend(s)
//       kick_off_http(url, on_done: my_queue.push(ticket, _))
//       #(s, Ok(promise))
//     }
//     fn my_loop(s) {
//       let s = event_loop.drain_jobs(s)
//       case state.outstanding(s) {
//         0 -> s
//         _ -> {
//           let #(ticket, result) = my_queue.block()
//           my_loop(host.resume(s, ticket, result))
//         }
//       }
//     }

/// Create a pending Promise and bump `outstanding`. Return the JsValue from
/// your host function so JS can `await` it; keep the `Ref` to pass to
/// `resume` once your external work completes.
pub fn suspend(s: State) -> #(State, JsValue, Ref) {
  let #(heap, obj_ref, data_ref) =
    builtins_promise.create_promise(s.heap, s.builtins.promise.prototype)
  #(
    state.State(..s, heap:, outstanding: s.outstanding + 1),
    JsObject(obj_ref),
    data_ref,
  )
}

/// Settle a Promise created by `suspend` — fulfils on `Ok`, rejects on
/// `Error`, decrements `outstanding`, and enqueues the reaction microtasks.
/// Call from your event-loop driver, then re-drain.
pub fn resume(
  s: State,
  ticket: Ref,
  outcome: Result(JsValue, JsValue),
) -> State {
  let s = case outcome {
    Ok(v) -> {
      let #(heap, jobs) = builtins_promise.fulfill_promise(s.heap, ticket, v)
      state.State(..s, heap:, job_queue: job_queue.append(s.job_queue, jobs))
    }
    Error(reason) -> builtins_promise.reject_promise(s, ticket, reason)
  }
  state.State(..s, outstanding: s.outstanding - 1)
}

// -- Atomics host capabilities -----------------------------------------------
//
// The blocking-wait / wake-delivery contract for Atomics.wait, waitAsync
// and notify. Same inversion of control as suspend/resume above: core owns
// the data (the ETS waiterlist registry, the SAB cells, State's FIFO of
// waitAsync waiters), the EMBEDDER owns every mailbox interaction. Core
// never executes a `receive` and never sends a wake message; instead it
// calls capability functions the embedder installed on State.
//
// The concrete types live in arc/vm/state (State's fields reference them);
// they are re-exported here as aliases so embedders can build against
// arc/host alone. Mirrors V8's split between the engine and the
// v8::Platform/d8 layer: d8 implements the actual futex park/unpark, the
// engine only asks for it.
//
// THE CONTRACT (five clauses; the numbered units of the Atomics refactor
// implement against exactly these):
//
// 1. Sync wait (Atomics.wait, DoWait steps 11-27). Core registers the
//    waiterlist entry (data-only ETS insert via arc_waiter_ffi), re-reads
//    the cell ("not-equal" short-circuits, cancelling the entry), then
//    calls `State.host_sync_wait` with a `WaitRequest`. The capability
//    blocks IN THE EMBEDDER until notified or timed out and returns
//    `WaitOk` / `WaitTimedOut` (JS "ok" / "timed-out"). The CanBlock
//    TypeError check (DoWait step 10) stays first and unchanged; a missing
//    capability is treated identically to `can_block == False`.
//    The notify-vs-timeout race is resolved by the embedder exactly as the
//    old arc_waiter_ffi:await_notify did: on timeout, ets:take your own
//    entry — got it = nobody claimed you = TimedOut; gone = a notifier
//    claimed you and its message is in flight = bounded flush-receive,
//    then Ok.
//
// 2. Wake delivery (Atomics.notify). Core's waiterlist take
//    (arc_waiter_ffi:take_waiters) atomically CLAIMS up to `count` waiters
//    FIFO and RETURNS the claimed remote waiters instead of messaging
//    them. Claiming is the spec's "woken" count; delivery is
//    `State.host_deliver_wake(claimed)`, which sends
//    `Pid ! {arc_notify, Ref, Key, ByteIndex}` per claimed waiter.
//    Same-process waitAsync settles stay in core (pure data, no message).
//
// 3. Wake injection. When an `{arc_notify, Ref, Key, ByteIndex}` message
//    lands in an EMBEDDER's mailbox, the embedder injects it into core via
//    the public entry point in arc/vm/exec/event_loop:
//
//        event_loop.inject_notify(state: State, key: WaiterKey,
//                                 byte_index: Int) -> State
//
//    which wraps builtins_atomics.settle_notified_waiter (settles this
//    agent's first matching State waitAsync waiter with "ok"; a wake whose
//    waiter already expired finds no match and settles nothing). Wakes for
//    cancelled SYNC entries never get here — clause 1's zero-timeout flush
//    consumes them — leaving only the documented accepted race in
//    arc_waiter_ffi's module doc (async timeout vs. cross-process claim).
//    Embedder receive loops bound their blocking
//    receive with `event_loop.next_deadline_timeout` so host timers and
//    waitAsync deadlines still fire on time, and re-drain after injecting
//    (see `beam.wait_settle_step` / `beam.settle_pending_wakes`, the
//    reusable helper the test262 harness also drives).
//
// 4. FFI module layout. arc_waiter_ffi.erl (under src/arc/vm/) is
//    DATA-ONLY: insert_waiter, take_waiters (returning claims — no send),
//    remove_async_token, local_buffer_key/shared_buffer_key, cancel_waiter
//    (ETS delete only) and the table-owner sync-join handshake. Zero
//    event-driven receives. The receive-based operations live in embedder
//    FFI — src/arc/arc_beam_ffi.erl for the beam embedder, mirrored in
//    test/test262_exec_ffi.erl for the harness:
//
//        await_notify(Handle, TimeoutMs) -> <<"ok">> | <<"timed-out">>
//            (blocking receive + the timeout-race resolution of clause 1;
//             TimeoutMs < 0 = infinity, clamped to the BEAM receive cap.
//             TimeoutMs = 0 doubles as the post-cancel flush: core's
//             "not-equal" arm calls host_sync_wait with a zero timeout
//             when its data-only cancel found the entry already claimed,
//             and the claimed branch consumes the in-flight wake — bounded
//             by a safety timeout — so it can't pollute a later receive)
//        deliver_wakes(Claimed) -> nil
//            (clause 2's sends, one per claimed remote waiter)
//        wait_for_notify(Ms) -> {some, {Key, ByteIndex}} | none
//            (bounded dry-queue receive for embedder loops; feeds
//             clause 3's inject_notify)
//
// 5. Installation. Embedders call `install_atomics_capabilities` below
//    once per booted State (beam: at run/install setup; harness: per-test
//    worker setup) — both capabilities together, since a host that can
//    block but not deliver wakes (or vice versa) deadlocks its peers.
//    `State.can_block` remains separate per-agent embedder config: it is
//    spec policy ([[CanBlock]]), not capability presence.

/// Re-export: one blocking sync Atomics.wait handed to the embedder.
/// See arc/vm/state.WaitRequest for field semantics.
pub type WaitRequest =
  state.WaitRequest

/// Re-export: result of an embedder blocking wait — `WaitOk` (notified)
/// or `WaitTimedOut`.
pub type WaitOutcome =
  state.WaitOutcome

/// Re-export: the blocking-wait capability, `fn(WaitRequest) -> WaitOutcome`.
pub type SyncWaitFn =
  state.SyncWaitFn

/// Re-export: the wake-delivery capability for claimed remote waiters.
pub type DeliverWakeFn =
  state.DeliverWakeFn

/// Re-export: opaque claimed-waiter term (pid + ref + key + byte index).
pub type ClaimedWaiter =
  state.ClaimedWaiter

/// Re-export: opaque cross-process WaiterList identity.
pub type WaiterKey =
  state.WaiterKey

/// Re-export: opaque handle to one registered waiterlist entry.
pub type WaiterHandle =
  state.WaiterHandle

/// Install the Atomics blocking-wait and wake-delivery capabilities on a
/// State (contract clause 5). Both together, always: a host that blocks
/// but cannot deliver wakes (or vice versa) deadlocks its peer agents.
/// Leaves `State.can_block` untouched — that is per-agent spec policy,
/// not capability presence.
pub fn install_atomics_capabilities(
  s: State,
  sync_wait sync_wait: state.SyncWaitFn,
  deliver_wake deliver_wake: state.DeliverWakeFn,
) -> State {
  state.State(
    ..s,
    host_sync_wait: option.Some(sync_wait),
    host_deliver_wake: option.Some(deliver_wake),
  )
}

// -- Constructors ------------------------------------------------------------

/// Allocate a JS array from Gleam values. Uses the correct Array.prototype.
pub fn array(s: State, values: List(JsValue)) -> #(State, JsValue) {
  let #(heap, ref) =
    common.alloc_array(s.heap, values, s.builtins.array.prototype)
  #(state.State(..s, heap:), JsObject(ref))
}

/// Allocate a plain JS object from a property list. Uses Object.prototype.
pub fn object(s: State, props: List(#(String, JsValue))) -> #(State, JsValue) {
  let prop_list = list.map(props, fn(p) { #(p.0, value.data_property(p.1)) })
  let #(heap, ref) =
    common.alloc_pojo(s.heap, s.builtins.object.prototype, prop_list)
  #(state.State(..s, heap:), JsObject(ref))
}

// -- Internal ----------------------------------------------------------------

fn invalid_arg_type(
  s: State,
  name: String,
  expected: String,
  received: JsValue,
) -> #(State, Result(JsValue, JsValue)) {
  state.type_error(
    s,
    "The \""
      <> name
      <> "\" argument must be of type "
      <> expected
      <> ". Received type "
      <> common.typeof_value(received, s.heap),
  )
}
