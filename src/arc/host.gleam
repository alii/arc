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
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/ops/operators
import arc/vm/state.{type Heap, type HostFn, type State, State}
import arc/vm/value.{
  type JsValue, type Ref, Finite, HostObject, JsBool, JsNumber, JsObject,
  JsString, ObjectSlot,
}
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option}

// -- Argument access ----------------------------------------------------------

/// The i-th argument a host function was called with, or `undefined` — JS's
/// rule that a missing argument is undefined. Pair it with a validator to
/// reject the wrong type instead of silently defaulting:
///
///     use text, s <- host.validate_string(s, host.arg_at(args, 0), "text")
pub fn arg_at(args: List(JsValue), idx: Int) -> JsValue {
  helpers.arg_at(args, idx)
}

// -- Validators --------------------------------------------------------------

/// Reject unless `val` is a JS string. Unwraps to the Gleam `String`.
pub fn validate_string(
  s: State(host),
  val: JsValue,
  name: String,
  cont: fn(String, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
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
  s: State(host),
  val: JsValue,
  name: String,
  cont: fn(JsValue, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case helpers.is_callable(s.heap, val) {
    True -> cont(val, s)
    False -> invalid_arg_type(s, name, "function", val)
  }
}

/// Validate callability AND call — one-shot combination of `validate_function`
/// and `state.try_call`. If `callee` isn't callable, throws TypeError with
/// the arg name; otherwise calls it and propagates the result or any throw.
pub fn try_call(
  s: State(host),
  callee: JsValue,
  name: String,
  this_val: JsValue,
  args: List(JsValue),
  cont: fn(JsValue, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case helpers.is_callable(s.heap, callee) {
    True -> state.try_call(s, callee, this_val, args, cont)
    False -> invalid_arg_type(s, name, "function", callee)
  }
}

/// Reject unless `val` is an integer-valued JS number within `[min, max]`.
/// Unwraps to `Int`. Throws RangeError if out of bounds, TypeError if not
/// a number.
pub fn validate_integer(
  s: State(host),
  val: JsValue,
  name: String,
  min: Int,
  max: Int,
  cont: fn(Int, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
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
  s: State(host),
  val: JsValue,
  name: String,
  cont: fn(Bool, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case val {
    JsBool(b) -> cont(b, s)
    _ -> invalid_arg_type(s, name, "boolean", val)
  }
}

// -- Suspend / resume --------------------------------------------------------
//
// The macrotask loop is the embedder's. Core only knows about Promises and
// the microtask queue. These two functions are the bridge: a host function
// hands JS a pending Promise and walks away with a settle `Ticket`; later,
// from its own loop (BEAM mailbox, libuv, epoll, whatever), it calls
// `resume` with that Ticket to settle the Promise and re-enters
// `drain_jobs`.
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

/// Opaque settle handle for one `suspend`ed Promise. The ONLY way to get one
/// is from `suspend`, and the only thing to do with it is hand it back to
/// `resume` — so the two classic embedder mistakes (passing the Promise
/// OBJECT's `Ref` to `resume`, or passing some unrelated `Ref`) are compile
/// errors, not silent heap corruption. Internally it wraps the promise's
/// PromiseSlot data ref; that never leaks.
pub opaque type Ticket {
  Ticket(data_ref: Ref)
}

/// Create a pending Promise and bump `outstanding`. Return the JsValue from
/// your host function so JS can `await` it; keep the `Ticket` to pass to
/// `resume` once your external work completes.
pub fn suspend(s: State(host)) -> #(State(host), JsValue, Ticket) {
  let #(heap, obj_ref, data_ref) =
    builtins_promise.create_promise(s.heap, s.builtins.promise.prototype)
  #(
    state.State(..s, heap:, outstanding: s.outstanding + 1),
    JsObject(obj_ref),
    Ticket(data_ref:),
  )
}

/// Settle the Promise behind a `suspend` Ticket — fulfils on `Ok`, rejects
/// on `Error`, enqueues the reaction microtasks, and decrements
/// `outstanding`. Call from your event-loop driver, then re-drain.
///
/// Resuming an already-settled ticket (a double resume) is a no-op: the
/// Promise stays as first settled and `outstanding` is NOT decremented
/// again, so the counter can never go negative and the embedder's
/// `outstanding(s) == 0` drain condition stays honest.
pub fn resume(
  s: State(host),
  ticket: Ticket,
  outcome: Result(JsValue, JsValue),
) -> State(host) {
  let Ticket(data_ref:) = ticket
  let #(s, did_settle) = builtins_promise.settle_outcome(s, data_ref, outcome)
  case did_settle {
    True -> state.State(..s, outstanding: s.outstanding - 1)
    False -> s
  }
}

// -- Atomics host capabilities -----------------------------------------------
//
// The blocking-wait / wake-delivery contract for Atomics.wait, waitAsync
// and notify. Same inversion of control as suspend/resume above: core owns
// the data (the ETS waiterlist registry, the SAB cells, State's FIFO of
// waitAsync waiters), the EMBEDDER owns every mailbox interaction. Core
// never executes a `receive` and never sends a wake message; instead it
// calls the capability functions the embedder supplied once at engine/
// realm construction in the realm's `HostHooks` record.
//
// The concrete types live in arc/vm/state (RealmCtx's `host_hooks` record
// references them); they are re-exported here as aliases so embedders can
// build against arc/host alone. Mirrors V8's split between the engine and
// the v8::Platform/d8 layer: d8 implements the actual futex park/unpark,
// the engine only asks for it.
//
// THE CONTRACT (five clauses; the numbered units of the Atomics refactor
// implement against exactly these):
//
// 1. Sync wait (Atomics.wait, DoWait steps 11-27). Core registers the
//    waiterlist entry (data-only ETS insert via arc_waiter_ffi), re-reads
//    the cell ("not-equal" short-circuits, cancelling the entry), then
//    calls the realm's `host_hooks.atomics` sync_wait capability with a
//    `WaitRequest`. The
//    capability blocks IN THE EMBEDDER until notified or timed out and returns
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
//    them. Claiming is the spec's "woken" count; delivery is the realm's
//    `host_hooks.atomics` deliver_wake capability, which sends
//    `Pid ! {arc_notify, Ref, Key, ByteIndex}` per claimed waiter.
//    Same-process waitAsync settles stay in core (pure data, no message).
//    Because a claim is a PROMISE to wake, take_waiters is only reachable
//    from a notifier holding deliver_wake; a realm with no `atomics`
//    capabilities takes only its OWN async tokens
//    (arc_waiter_ffi:take_self_async_tokens) and leaves other agents'
//    waiters registered rather than claiming waiters it cannot wake.
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
//    (see `wait_settle_step` / `settle_pending_wakes` in the test262
//    harness, the canonical bounded wait-settle-drain loop).
//
// 4. FFI module layout. arc_waiter_ffi.erl (under src/arc/vm/) is
//    DATA-ONLY: insert_waiter, take_waiters (returning claims — no send),
//    take_self_async_tokens, local_buffer_key/shared_buffer_key,
//    cancel_waiter (ETS take only, reporting withdrew | already_claimed) and
//    start_registry (the table-owner sync-join handshake, run once at realm
//    boot from interpreter.new_state — no waiterlist operation creates the
//    table lazily). Zero event-driven receives. The receive-based operations
//    live in embedder FFI — test/test262_exec_ffi.erl for the test262
//    harness, the in-tree reference embedder:
//
//        await_notify(Handle, TimeoutMs) -> <<"ok">> | <<"timed-out">>
//            (blocking receive + the timeout-race resolution of clause 1;
//             TimeoutMs < 0 = infinity, clamped to the BEAM receive cap.
//             TimeoutMs = 0 doubles as the post-cancel flush: core's
//             "not-equal" arm calls the sync_wait capability with a zero timeout
//             when its data-only cancel found the entry already claimed,
//             and the claimed branch consumes the in-flight wake — bounded
//             by a safety timeout — so it can't pollute a later receive)
//        deliver_wakes(Claimed) -> nil
//            (clause 2's sends, one per claimed remote waiter)
//        wait_for_notify(Ms) -> {some, {Key, ByteIndex}} | none
//            (bounded dry-queue receive for embedder loops; feeds
//             clause 3's inject_notify)
//
// 5. Construction. Both capabilities live in ONE `AtomicsCapabilities`
//    record, stored as a single `Option` on the `HostHooks` record
//    (re-exported below) carried on the per-realm `RealmCtx`. Embedders
//    build it ONCE with `atomics_capabilities` below and hand it to the
//    engine/realm constructor (the `host_hooks` argument of the entry/
//    module/engine boot APIs) — never to an already-running State. Every
//    State derived from that realm — eval/Function realms,
//    $262.createRealm and $262.agent children, ShadowRealms, module
//    bodies including dynamic import — inherits the record, so a
//    forgotten install site is a COMPILE error, not a silent "cannot
//    block". Both capabilities come together, since a host that can
//    block but not deliver wakes (or vice versa) deadlocks its peers; a
//    host with neither passes `state.default_host_hooks()` (the default
//    the convenience entry points already use), which means "cannot
//    block". `State.can_block` remains separate per-agent embedder
//    config OUTSIDE the record: it is spec policy ([[CanBlock]]), not
//    capability presence.

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

/// Re-export: the bundled blocking-wait + wake-delivery capability pair.
/// `HostHooks.atomics` holds `Option(AtomicsCapabilities)` — both
/// capabilities or neither, never one without the other.
pub type AtomicsCapabilities =
  state.AtomicsCapabilities

/// Re-export: the embedder host-capability record carried on every realm's
/// `RealmCtx`. Build one with `atomics_capabilities` below (or
/// `state.default_host_hooks()` for "no capabilities") and hand it to the
/// engine/realm constructor; every derived State inherits it.
pub type HostHooks =
  state.HostHooks

/// Build the Atomics blocking-wait + wake-delivery capability record
/// (contract clause 5). Both together, always: a host that blocks but
/// cannot deliver wakes (or vice versa) deadlocks its peer agents. This
/// is enforced by construction — `HostHooks.atomics` is one
/// `Option(AtomicsCapabilities)`, so the half-configured embedder is not
/// representable.
///
/// Hand the result to the engine/realm constructor ONCE — it is a value,
/// not a State mutation — and every State derived from that realm
/// (eval/Function realms, $262.createRealm / $262.agent children,
/// ShadowRealms, module bodies including dynamic import) inherits it.
/// Says nothing about `State.can_block` — that stays per-agent spec
/// policy ([[CanBlock]]), not capability presence.
pub fn atomics_capabilities(
  sync_wait sync_wait: state.SyncWaitFn,
  deliver_wake deliver_wake: state.DeliverWakeFn,
) -> HostHooks {
  state.HostHooks(
    ..state.default_host_hooks(),
    atomics: option.Some(state.AtomicsCapabilities(sync_wait:, deliver_wake:)),
  )
}

// -- Constructors ------------------------------------------------------------

/// Allocate a JS array from Gleam values. Uses the correct Array.prototype.
pub fn array(s: State(host), values: List(JsValue)) -> #(State(host), JsValue) {
  let #(heap, ref) =
    common.alloc_array(s.heap, values, s.builtins.array.prototype)
  #(state.State(..s, heap:), JsObject(ref))
}

/// Allocate a plain JS object from a property list. Uses Object.prototype.
pub fn object(
  s: State(host),
  props: List(#(String, JsValue)),
) -> #(State(host), JsValue) {
  let prop_list = list.map(props, fn(p) { #(p.0, value.data_property(p.1)) })
  let #(heap, ref) =
    common.alloc_pojo(s.heap, s.builtins.object.prototype, prop_list)
  #(state.State(..s, heap:), JsObject(ref))
}

// -- Opaque host values ------------------------------------------------------

/// Allocate an opaque, embedder-owned heap object wrapping `value` (the
/// embedder's own type). The engine never inspects `value` — it only ferries
/// it and renders the object via the prototype's `@@toStringTag`. The object
/// has no own properties; pass `Some(proto)` to give it methods/a tag, or
/// `None` for a maximally-opaque, null-prototype value. Read it back, typed
/// and coerce-free, with `read_host`.
///
/// Any engine heap `Ref`s the value needs should live in the object's
/// properties (GC traces those), not in `value` itself.
pub fn alloc_host_object(
  s: State(host),
  value: host,
  prototype: Option(Ref),
) -> #(State(host), JsValue) {
  let #(heap, ref) =
    heap.alloc(
      s.heap,
      ObjectSlot(
        kind: HostObject(value:),
        properties: dict.new(),
        elements: elements.new(),
        prototype:,
        symbol_properties: [],
        extensible: True,
      ),
    )
  #(State(..s, heap:), JsObject(ref))
}

/// Read the embedder value out of a host object — fully typed, no `Dynamic`,
/// no coerce, no decode. `None` if `ref` is not a `HostObject`. The embedder
/// `case`-matches the returned `host` with full exhaustiveness checking.
pub fn read_host(h: Heap(host), ref: Ref) -> Option(host) {
  case heap.read(h, ref) {
    option.Some(ObjectSlot(kind: HostObject(value:), ..)) -> option.Some(value)
    _ -> option.None
  }
}

/// Mint a standalone native function as a `JsValue` — for building methods or
/// returning callables. `impl` is an arbitrary closure, so it can capture any
/// typed host data.
pub fn function(
  s: State(host),
  name: String,
  arity: Int,
  impl: HostFn(host),
) -> #(State(host), JsValue) {
  let #(heap, ref) =
    common.alloc_host_fn(
      s.heap,
      s.builtins.function.prototype,
      impl,
      name,
      arity,
    )
  #(State(..s, heap:), JsObject(ref))
}

// -- Internal ----------------------------------------------------------------

fn invalid_arg_type(
  s: State(host),
  name: String,
  expected: String,
  received: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  state.type_error(
    s,
    "The \""
      <> name
      <> "\" argument must be of type "
      <> expected
      <> ". Received type "
      <> operators.typeof(s.heap, received),
  )
}
