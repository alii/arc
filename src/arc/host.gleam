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
import arc/vm/host_hooks
import arc/vm/internal/elements
import arc/vm/ops/operators
import arc/vm/state.{type HostFn, type State, State}
import arc/vm/value.{
  type JsValue, type Ref, Finite, HostObject, Infinity, JsBool, JsNumber,
  JsObject, JsString, NaN, NegInfinity, ObjectSlot,
}
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option}

// -- Argument access ---------------------------------------------------------
//
// Embedders read positional arguments the same way builtins do — a missing
// argument is `undefined`, per JS semantics. Re-exported here so example /
// embedder code doesn't reach into `arc/vm/builtins/helpers` (an internal
// module).

/// The first argument, or `undefined` when the caller passed none.
pub const first_arg = helpers.first_arg_or_undefined

/// The i-th argument (0-based), or `undefined` when the caller passed fewer.
pub const arg_at = helpers.arg_at

/// Throw a `TypeError` with `msg`. The dispatch-shaped `#(state, Error(...))`.
pub fn type_error(
  s: State(host),
  msg: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  state.type_error(s, msg)
}

/// Throw a `RangeError` with `msg`. The dispatch-shaped `#(state, Error(...))`.
pub fn range_error(
  s: State(host),
  msg: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  state.range_error(s, msg)
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
/// Unwraps to `Int`. Three rejections, and they are NOT the same error:
///
///   * not a number at all (`"3"`, `{}`, `undefined`) → **TypeError**, "must
///     be of type integer. Received type <typeof>";
///   * a number, but not an integer (`1.5`, `NaN`, `Infinity`) →
///     **RangeError**, "must be an integer";
///   * an integer outside `[min, max]` → **RangeError**, "must be >= min and
///     <= max".
///
/// A non-integral number IS of type number, so calling that a type error
/// (as this used to) told the embedder's user the wrong thing.
pub fn validate_integer(
  s: State(host),
  val: JsValue,
  name: String,
  min: Int,
  max: Int,
  cont: fn(Int, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case val {
    JsNumber(Finite(n)) ->
      case value.integral_int(n) {
        option.None -> not_an_integer(s, name, value.js_format_number(n))
        option.Some(i) ->
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
    // A number, just not an integral one — out of the integer domain, not
    // out of the type.
    JsNumber(NaN) -> not_an_integer(s, name, "NaN")
    JsNumber(Infinity) -> not_an_integer(s, name, "Infinity")
    JsNumber(NegInfinity) -> not_an_integer(s, name, "-Infinity")
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
//           let #(s, _outcome) = host.resume(s, ticket, result)
//           my_loop(s)
//         }
//       }
//     }

/// What `resume` did with the ticket. Embedders that don't care can bind
/// `_outcome`; embedders that want to detect their own bugs match on it.
pub type ResumeOutcome {
  /// The promise was pending and is now settled; `outstanding` decremented.
  Resumed
  /// The ticket had already been resumed once. Nothing changed.
  AlreadySettled
  /// The ticket's promise slot no longer exists — an embedder bug (it was
  /// dropped by `shrink_for_handoff`). Nothing was settled and `outstanding`
  /// still counts a suspend that can never complete.
  StaleTicket
}

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
  let #(heap, builtins_promise.PromiseRefs(promise:, data:)) =
    builtins_promise.create_promise(s.heap, s.builtins.promise.prototype)
  #(
    state.State(..s, heap:, outstanding: s.outstanding + 1),
    JsObject(promise),
    Ticket(data_ref: data),
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
///
/// Resuming a ticket whose promise slot no longer exists (it was dropped by
/// `shrink_for_handoff`) settles nothing and cannot be silently confused with
/// a double resume: it is returned as `StaleTicket` so the embedder can act
/// on it — the library never writes to stderr behind the embedder's back.
pub fn resume(
  s: State(host),
  ticket: Ticket,
  outcome: Result(JsValue, JsValue),
) -> #(State(host), ResumeOutcome) {
  let Ticket(data_ref:) = ticket
  let #(s, settle_outcome) =
    builtins_promise.settle_outcome(s, data_ref, outcome)
  case settle_outcome {
    // The one settle per suspend: balance the counter.
    builtins_promise.Transitioned(_) -> #(
      state.State(..s, outstanding: s.outstanding - 1),
      Resumed,
    )
    // A double resume of the same ticket: legitimately does nothing.
    builtins_promise.AlreadySettled -> #(s, AlreadySettled)
    // A stale ticket: the promise it named is gone, so nothing was settled
    // and `outstanding` still counts a suspend that can never complete.
    builtins_promise.NotAPromiseSlot -> #(s, StaleTicket)
  }
}

// -- Atomics host capabilities -----------------------------------------------
//
// Same inversion of control as suspend/resume above: core owns the data (the
// ETS waiterlist registry, the SAB cells, State's FIFO of waitAsync waiters),
// the EMBEDDER owns every mailbox interaction — core never executes a
// `receive` and never sends a wake message. Mirrors V8's split between the
// engine and the v8::Platform/d8 layer.
//
// What an embedder must know to use the aliases below:
//
//   * BOTH capabilities or neither. `sync_wait` (block this agent) and
//     `deliver_wake` (send the wakes core has claimed for you) come as one
//     `AtomicsCapabilities` record under a single `Option`, because a host
//     that can block but not deliver wakes — or vice versa — deadlocks its
//     peers.
//   * Install ONCE, at construction. Compose them onto your `HostHooks` with
//     `with_atomics` and pass that record to the engine/realm constructor —
//     never to an already-running State. Every derived State (eval/Function
//     realms, $262 children, ShadowRealms, module bodies) inherits it, so a
//     forgotten install site is a compile error, not a silent "cannot block".
//     A host with neither passes `default_host_hooks()`, which means
//     "cannot block": sync `Atomics.wait` throws instead of hanging.
//   * `can_block` is NOT in here. Agent [[CanBlock]] (§9.7) is per-agent spec
//     policy, threaded to `interpreter.new_state` at realm boot and carried on
//     `State.can_block`.
//   * Wakes come back in through `event_loop.inject_notify(state, key,
//     byte_index)` when an `{arc_notify, Ref, Key, ByteIndex}` message lands
//     in your mailbox.
//
// The full contract lives next to its implementation: the capability
// semantics on `arc/vm/host_hooks.AtomicsCapabilities`, the claim/settle
// accounting in `arc/vm/builtins/atomics`, and the registry's ordering rules
// in `arc/vm/builtins/arc_waiter_ffi.erl`.

/// Re-export: one blocking sync Atomics.wait handed to the embedder.
/// See arc/vm/host_hooks.WaitRequest for field semantics.
pub type WaitRequest =
  host_hooks.WaitRequest

/// Re-export: result of an embedder blocking wait — `WaitOk` (notified)
/// or `WaitTimedOut`.
pub type WaitOutcome =
  host_hooks.WaitOutcome

/// Re-export: the blocking-wait capability, `fn(WaitRequest) -> WaitOutcome`.
pub type SyncWaitFn =
  host_hooks.SyncWaitFn

/// Re-export: the wake-delivery capability for claimed remote waiters.
pub type DeliverWakeFn =
  host_hooks.DeliverWakeFn

/// Re-export: opaque claimed-waiter term (pid + ref + key + byte index).
pub type ClaimedWaiter =
  host_hooks.ClaimedWaiter

/// Re-export: opaque cross-process WaiterList identity.
pub type WaiterKey =
  host_hooks.WaiterKey

/// Re-export: opaque handle to one registered waiterlist entry.
pub type WaiterHandle =
  host_hooks.WaiterHandle

/// Re-export: the bundled blocking-wait + wake-delivery capability pair.
/// `HostHooks.atomics` holds `Option(AtomicsCapabilities)` — both
/// capabilities or neither, never one without the other.
pub type AtomicsCapabilities =
  host_hooks.AtomicsCapabilities

/// Re-export: the embedder host-capability record carried on every realm's
/// `RealmCtx`. Start from `default_host_hooks()` (or your own record), add
/// capabilities, and hand it to the engine/realm constructor; every derived
/// State inherits it.
pub type HostHooks =
  host_hooks.HostHooks

/// Re-export: the capability-free default — no Atomics capabilities (sync
/// `Atomics.wait` throws instead of hanging), no dynamic-import hook, and the
/// real BEAM monotonic clock / sleep. The starting point for `with_atomics`.
pub fn default_host_hooks() -> HostHooks {
  host_hooks.default_host_hooks()
}

/// Install the Atomics blocking-wait + wake-delivery capabilities on `hooks`,
/// leaving every OTHER hook (`monotonic_now`, `sleep_ms`, `import_hook`, …)
/// exactly as the caller configured it. Both capabilities together, always: a
/// host that blocks but cannot deliver wakes (or vice versa) deadlocks its
/// peer agents, and `HostHooks.atomics` is one `Option(AtomicsCapabilities)`
/// so the half-configured embedder is not representable.
///
/// Hand the result to the engine/realm constructor ONCE — it is a value, not
/// a State mutation — and every State derived from that realm inherits it.
/// Says nothing about the agent's [[CanBlock]] (`State.can_block`), which is
/// spec policy, not capability presence.
pub fn with_atomics(
  hooks: HostHooks,
  sync_wait sync_wait: SyncWaitFn,
  deliver_wake deliver_wake: DeliverWakeFn,
) -> HostHooks {
  host_hooks.HostHooks(
    ..hooks,
    atomics: option.Some(host_hooks.AtomicsCapabilities(
      sync_wait:,
      deliver_wake:,
    )),
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
/// no coerce, no decode. `None` if `val` is not a `HostObject`. The embedder
/// `case`-matches the returned `host` with full exhaustiveness checking.
pub fn read_host(s: State(host), val: JsValue) -> Option(host) {
  case val {
    JsObject(ref) ->
      case heap.read(s.heap, ref) {
        option.Some(ObjectSlot(kind: HostObject(value:), ..)) ->
          option.Some(value)
        _ -> option.None
      }
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

/// A number that is not an integer (1.5, NaN, ±Infinity): the VALUE is out of
/// range, the type is fine — RangeError, mirroring the out-of-bounds message.
fn not_an_integer(
  s: State(host),
  name: String,
  received: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  state.range_error(
    s,
    "The value of \""
      <> name
      <> "\" is out of range. It must be an integer. Received "
      <> received,
  )
}

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
