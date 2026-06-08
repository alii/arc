//// BEAM/OTP primitives as opt-in host functions, plus the Erlang-mailbox
//// macrotask loop that drives them.
////
//// Arc the language has no built-in process model and no macrotask loop —
//// core only knows about Promises and the microtask queue. This module is
//// one embedder: it exposes Erlang processes/subjects/timers as `HostFn`s,
//// and `run` is the loop that blocks on the BEAM mailbox to settle the
//// promises those HostFns hand out via `host.suspend`.
////
////     // everything, under `Arc.*`, with the BEAM loop driving eval
////     let eng = engine.new() |> beam.install("Arc")
////     let assert Ok(#(comp, eng)) = engine.eval_with(eng, src, beam.run)
////
////     // or à la carte
////     let eng =
////       engine.new()
////       |> engine.define_namespace("proc", [
////         #("spawn", 1, beam.spawn),
////         #("self", 0, beam.self),
////       ])

import arc/engine.{type Engine}
import arc/host
import arc/vm/builtins/atomics as builtins_atomics
import arc/vm/builtins/common.{type Builtins}
import arc/vm/builtins/console
import arc/vm/builtins/process_objects
import arc/vm/builtins/promise as builtins_promise
import arc/vm/builtins/structured_clone
import arc/vm/exec/event_loop
import arc/vm/exec/interpreter
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/internal/tuple_array
import arc/vm/ops/object
import arc/vm/state.{type Heap, type HostFn, type State, State}
import arc/vm/value.{
  type JsValue, type PortableMessage, type Ref, JsNumber, JsObject, JsString,
  JsUndefined, Named, ObjectSlot, PortableMessage, PromiseFulfilled,
  PromisePending, PromiseRejected, PvUndefined, SelectorObject,
}
import gleam/dict
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set

// -- Mailbox event protocol --------------------------------------------------

/// Envelope for every message this loop's selective receive accepts. The
/// Erlang side (`arc_beam_ffi.erl`) constructs these as bare tagged tuples,
/// so the variant names here must stay snake_case-compatible.
pub type MailboxEvent {
  /// `setTimeout` / external worker completed: settle the suspended promise.
  SettlePromise(
    data_ref: Ref,
    outcome: Result(PortableMessage, PortableMessage),
  )
  /// `receiveAsync(ms)` deadline fired. If the receiver is still pending,
  /// resolve it with `undefined`; otherwise the message already arrived.
  ReceiverTimeout(data_ref: Ref)
  /// A subject message matched by selective receive while a `receiveAsync`
  /// is pending on that tag.
  SubjectMessage(tag: value.ErlangRef, payload: PortableMessage)
  /// A cross-process Atomics.notify claimed one of this process's waiters
  /// and delivered its wake here (`{arc_notify, Ref, Key, ByteIndex}` in
  /// the raw mailbox, retagged by the FFI receive). Injected into core via
  /// `event_loop.inject_notify`, which settles this agent's first pending
  /// waitAsync waiter at that (key, byte index). Wakes for cancelled sync
  /// entries never reach this loop: core's "not-equal" cancel arm flushes
  /// a claimed entry's in-flight wake through a zero-timeout
  /// `host_sync_wait` call before returning (see `atomics_sync_wait`).
  AtomicsNotify(key: state.WaiterKey, byte_index: Int)
}

// -- FFI ---------------------------------------------------------------------

@external(erlang, "erlang", "spawn")
fn erlang_spawn(fun: fn() -> Nil) -> value.ErlangPid

@external(erlang, "erlang", "self")
fn ffi_self() -> value.ErlangPid

@external(erlang, "arc_beam_ffi", "sleep")
fn ffi_sleep(ms: Int) -> Nil

@external(erlang, "arc_beam_ffi", "send_after")
fn ffi_send_after(
  ms: Int,
  pid: value.ErlangPid,
  msg: MailboxEvent,
) -> value.ErlangTimerRef

@external(erlang, "arc_beam_ffi", "cancel_timer")
fn ffi_cancel_timer(tref: value.ErlangTimerRef) -> Bool

@external(erlang, "erlang", "make_ref")
fn ffi_make_ref() -> value.ErlangRef

@external(erlang, "arc_beam_ffi", "select_message")
fn ffi_select(
  ref_map: dict.Dict(value.ErlangRef, Bool),
) -> #(value.ErlangRef, PortableMessage)

@external(erlang, "arc_beam_ffi", "select_message_timeout")
fn ffi_select_timeout(
  ref_map: dict.Dict(value.ErlangRef, Bool),
  timeout: Int,
) -> Result(#(value.ErlangRef, PortableMessage), Nil)

@external(erlang, "arc_beam_ffi", "receive_settle_only")
fn ffi_receive_settle_only() -> MailboxEvent

@external(erlang, "arc_beam_ffi", "receive_settle_or_subject")
fn ffi_receive_settle_or_subject(
  ref_map: dict.Dict(value.ErlangRef, List(Ref)),
) -> MailboxEvent

@external(erlang, "arc_beam_ffi", "receive_settle_only_timeout")
fn ffi_receive_settle_only_timeout(timeout: Int) -> Result(MailboxEvent, Nil)

@external(erlang, "arc_beam_ffi", "receive_settle_or_subject_timeout")
fn ffi_receive_settle_or_subject_timeout(
  ref_map: dict.Dict(value.ErlangRef, List(Ref)),
  timeout: Int,
) -> Result(MailboxEvent, Nil)

/// Pending `receiveAsync` tickets, maintained incrementally in the process
/// dictionary as a pair of maps: subject tag -> FIFO list of settle refs
/// (multiple `receiveAsync` calls may be outstanding on one subject), and
/// settle ref -> tag for `ReceiverTimeout` removal.
type Receivers =
  #(dict.Dict(value.ErlangRef, List(Ref)), dict.Dict(Ref, value.ErlangRef))

@external(erlang, "arc_beam_ffi", "receivers_get")
fn receivers_get() -> Receivers

@external(erlang, "arc_beam_ffi", "receivers_put")
fn receivers_put(r: Receivers) -> Nil

// -- Macrotask loop ----------------------------------------------------------

/// The BEAM-mailbox macrotask loop. Installs this module's Atomics host
/// capabilities (blocking sync wait + wake delivery — contract clause 5,
/// arc/host.gleam; a no-op when the embedder already installed them at
/// boot, which beam-driven runs must do — see `namespace`), drains
/// microtasks, and — while `host.suspend` promises are outstanding or core
/// deadlines (host timers, waitAsync timeouts) are pending — blocks on the
/// Erlang mailbox for the next `MailboxEvent`, settles it, and repeats.
/// Cross-process Atomics wakes (`arc_notify`) are injected into core via
/// `event_loop.inject_notify`.
///
/// Pass to `engine.eval_prepared_with` / `entry.run_prepared` as the
/// `finish` driver, with `install_atomics_capabilities` as `prepare`.
pub fn run(s: State) -> State {
  run_loop(install_atomics_capabilities(s))
}

fn run_loop(s: State) -> State {
  let s = event_loop.drain_jobs_yielding(s)
  let deadline = event_loop.next_deadline_timeout(s)
  case state.outstanding(s) > 0, deadline {
    // Nothing the mailbox could wake: done. (Pending deadline-free
    // waitAsync waiters alone don't hold the loop open — matching the
    // pre-capability dry-queue semantics.)
    False, None -> s
    _, _ -> {
      let receivers = receivers_get()
      let #(tag_map, _) = receivers
      // With host-timer / Atomics.waitAsync deadlines pending,
      // drain_jobs_yielding returns instead of sleeping (it would starve
      // the mailbox); bound the receive by the earliest deadline so
      // mailbox messages, host timers, and waitAsync deadlines interleave.
      // A receive timeout just means a deadline is due — loop so the
      // drain fires it.
      case deadline {
        None -> {
          let event = case dict.is_empty(tag_map) {
            True -> ffi_receive_settle_only()
            False -> ffi_receive_settle_or_subject(tag_map)
          }
          run_loop(handle_event(s, event, receivers))
        }
        Some(timeout) -> {
          let event = case dict.is_empty(tag_map) {
            True -> ffi_receive_settle_only_timeout(timeout)
            False -> ffi_receive_settle_or_subject_timeout(tag_map, timeout)
          }
          case event {
            Ok(event) -> run_loop(handle_event(s, event, receivers))
            Error(Nil) -> run_loop(s)
          }
        }
      }
    }
  }
}

fn handle_event(
  state: State,
  event: MailboxEvent,
  receivers: Receivers,
) -> State {
  let #(tag_map, ref_map) = receivers
  case event {
    SettlePromise(data_ref:, outcome: Ok(pm)) -> {
      let #(heap, val) =
        structured_clone.deserialize(state.heap, state.builtins, pm)
      host.resume(State(..state, heap:), data_ref, Ok(val))
    }
    SettlePromise(data_ref:, outcome: Error(pm)) -> {
      let #(heap, reason) =
        structured_clone.deserialize(state.heap, state.builtins, pm)
      host.resume(State(..state, heap:), data_ref, Error(reason))
    }
    ReceiverTimeout(data_ref:) ->
      case dict.get(ref_map, data_ref) {
        // Message already arrived and retired the receiver — timeout is stale.
        Error(Nil) -> state
        Ok(tag) -> {
          let queue =
            dict.get(tag_map, tag)
            |> result.unwrap([])
            |> list.filter(fn(r) { r != data_ref })
          let tag_map = case queue {
            [] -> dict.delete(tag_map, tag)
            _ -> dict.insert(tag_map, tag, queue)
          }
          receivers_put(#(tag_map, dict.delete(ref_map, data_ref)))
          host.resume(state, data_ref, Ok(JsUndefined))
        }
      }
    SubjectMessage(tag:, payload: pm) ->
      case dict.get(tag_map, tag) |> result.unwrap([]) {
        [] -> state
        // FIFO: the earliest-registered receiver gets the message.
        [data_ref, ..rest] -> {
          let tag_map = case rest {
            [] -> dict.delete(tag_map, tag)
            _ -> dict.insert(tag_map, tag, rest)
          }
          receivers_put(#(tag_map, dict.delete(ref_map, data_ref)))
          let #(heap, val) =
            structured_clone.deserialize(state.heap, state.builtins, pm)
          host.resume(State(..state, heap:), data_ref, Ok(val))
        }
      }
    AtomicsNotify(key:, byte_index:) ->
      event_loop.inject_notify(state, key, byte_index)
  }
}

// -- Atomics host capabilities (the BEAM-mailbox implementations) -------------
//
// The embedder side of the host capability contract in arc/host.gleam:
// clause 1 (blocking sync wait) and clause 2 (wake delivery) as
// State-installed capability functions, clause 5 (installation) below.
// Core registers waiterlist entries and claims waiters as pure ETS data
// (arc_waiter_ffi); every receive of — and every send into — the
// `{arc_notify, Ref, Key, ByteIndex}` wake protocol happens HERE, via
// arc_beam_ffi.erl.

/// Clause 1's blocking receive, relocated from the old in-core
/// arc_waiter_ffi:await_notify: selective receive for the entry's wake,
/// with the notify-vs-timeout race resolved by ets:take of our own entry
/// (negative timeout = infinity). Returns the JS "ok" / "timed-out".
@external(erlang, "arc_beam_ffi", "await_notify")
fn ffi_await_notify(handle: state.WaiterHandle, timeout_ms: Int) -> String

/// BEAM implementation of the blocking-wait capability
/// (`State.host_sync_wait`, contract clause 1): suspend this process in a
/// selective receive until the registered waiterlist entry is woken or
/// the timeout elapses. `timeout_ms: None` = wait forever (the FFI clamps
/// to the BEAM receive ceiling).
///
/// A `Some(0)` timeout doubles as contract clause 4's cancel flush: core's
/// sync_block "not-equal" arm calls this with a zero timeout when its
/// data-only cancel found the entry already claimed by a notifier — the
/// FFI's claimed path (ets:take of our own entry finds nothing) then
/// performs the bounded flush receive, consuming the in-flight wake so it
/// cannot pollute a later receive or spuriously settle a future waitAsync
/// waiter at the same (key, byte index).
pub fn atomics_sync_wait(req: state.WaitRequest) -> state.WaitOutcome {
  let state.WaitRequest(handle:, timeout_ms:, key: _, byte_index: _) = req
  case ffi_await_notify(handle, option.unwrap(timeout_ms, -1)) {
    "timed-out" -> state.WaitTimedOut
    _ -> state.WaitOk
  }
}

/// BEAM implementation of the wake-delivery capability
/// (`State.host_deliver_wake`, contract clause 2): send
/// `{arc_notify, Ref, Key, ByteIndex}` to each remote waiter claimed by
/// Atomics.notify's waiterlist take. Claiming already counted them as
/// woken; this delivery is what a blocked sync wait (or a peer's embedder
/// loop, for waitAsync) actually receives.
@external(erlang, "arc_beam_ffi", "deliver_wakes")
pub fn atomics_deliver_wake(claimed: List(state.ClaimedWaiter)) -> Nil

/// Install the BEAM-mailbox Atomics capabilities on a State — both
/// together, per contract clause 5 (a host that can block but not deliver
/// wakes, or vice versa, deadlocks its peers). `run` installs them at loop
/// entry and `spawn` children at boot; embedders that stand up their own
/// State in a BEAM process and drive the event loop directly (the test262
/// harness workers) call this during setup. Leaves `State.can_block`
/// untouched — that is per-agent spec policy, not capability presence.
pub fn install_atomics_capabilities(s: State) -> State {
  host.install_atomics_capabilities(
    s,
    sync_wait: atomics_sync_wait,
    deliver_wake: atomics_deliver_wake,
  )
}

// -- Atomics wake settling (reusable embedder helper) -------------------------
//
// Contract clause 4 (arc/host): the bounded dry-queue receive for
// arc_notify messages is embedder FFI, not core FFI.
@external(erlang, "arc_beam_ffi", "wait_for_notify")
fn ffi_wait_for_notify(ms: Int) -> Option(#(state.WaiterKey, Int))

// Contract clause 3 of the Atomics host-capability contract (arc/host):
// cross-process Atomics.notify wakes arrive as `{arc_notify, Ref, Key,
// ByteIndex}` messages in the EMBEDDER's mailbox, never in core. These two
// helpers are the canonical 'bounded mailbox wait, settle wakes, re-drain'
// loop. They live here — not in event_loop — because they receive; the
// test262 harness (which drives event_loop directly instead of beam.run)
// calls the same helpers from its per-test worker process.

/// One bounded wait-settle-drain step: block at most `timeout_ms` for a
/// cross-process `arc_notify` message; if one arrives, settle this agent's
/// first matching waitAsync waiter with "ok" and re-drain microtasks.
/// Returns the updated state and `True` if a wake was consumed (`False` =
/// the timeout elapsed, i.e. a deadline is due — the caller's next drain
/// fires it). Bound `timeout_ms` with `event_loop.next_deadline_timeout`
/// so waitAsync timeouts and host timers still fire on time.
pub fn wait_settle_step(s: State, timeout_ms: Int) -> #(State, Bool) {
  case ffi_wait_for_notify(timeout_ms) {
    Some(#(key, byte_index)) -> {
      let s = builtins_atomics.settle_notified_waiter(s, key, byte_index)
      #(event_loop.drain_jobs_yielding(s), True)
    }
    None -> #(s, False)
  }
}

/// Drive pending Atomics.waitAsync waiters to settlement: drain, then loop
/// `wait_settle_step` bounded by the earliest pending deadline until no
/// settleable deadline remains. Mirrors event_loop.drain_jobs' dry-queue
/// semantics: when only deadline-free (infinite) waiters remain and no
/// wake arrives, the loop returns — a mailbox loop cannot distinguish a
/// never-notified infinite waiter from quiescence, and parking forever
/// here would hang the embedder.
pub fn settle_pending_wakes(s: State) -> State {
  let s = event_loop.drain_jobs_yielding(s)
  case s.atomics_waiters {
    [] -> s
    _ ->
      case event_loop.next_deadline_timeout(s) {
        None -> s
        Some(timeout) -> {
          let #(s, _woke) = wait_settle_step(s, timeout)
          settle_pending_wakes(s)
        }
      }
  }
}

// -- install -----------------------------------------------------------------

/// Install every BEAM primitive under one namespace object. Passing `"Arc"`
/// reproduces the legacy baked-in `Arc` global.
pub fn install(eng: Engine, name: String) -> Engine {
  engine.define_namespace(eng, name, namespace())
}

/// Heap-level equivalent of `install` for callers that bootstrap with
/// `builtins.init`/`builtins.globals` directly instead of `engine.new()`
/// (the REPL, the test harness).
pub fn install_globals(
  h: Heap,
  b: Builtins,
  global: Ref,
  name: String,
) -> Heap {
  let #(h, methods) =
    common.alloc_host_methods(h, b.function.prototype, namespace())
  let #(h, ns_ref) = common.init_namespace(h, b.object.prototype, name, methods)
  object.define_method_property(h, global, value.Named(name), JsObject(ns_ref))
}

/// Method specs for `engine.define_namespace`. Exposed so embedders can
/// concat their own functions onto the same namespace.
///
/// Every method ensures the Atomics host capabilities are installed on the
/// State it runs against (clause 5 at `install` level): `run` only takes
/// over AFTER the top-level script returns, but a script can hit a
/// blocking `Atomics.wait` or a cross-process `Atomics.notify` mid-script.
/// Any CROSS-PROCESS scenario necessarily goes through one of these host
/// functions first (`spawn` is the only way to get a second process), so
/// those are covered — but a single-process top-level sync `Atomics.wait`
/// (e.g. a timed wait used as a sleep) calls no `Arc.*` function and is
/// NOT: beam-driven runs must install the capabilities at boot, before
/// the top-level script executes, by passing `install_atomics_capabilities`
/// as the eval `prepare` hook (`engine.eval_prepared_with` /
/// `entry.run_prepared`; arc's `--event-loop` runner and the test262
/// harness both do this). This wrapper remains as defense in depth for
/// embedders that compose `namespace()` without a prepared boot.
pub fn namespace() -> List(#(String, Int, HostFn)) {
  [
    #("spawn", 1, with_atomics_capabilities(spawn)),
    #("self", 0, with_atomics_capabilities(self)),
    #("sleep", 1, with_atomics_capabilities(sleep)),
    #("subject", 0, with_atomics_capabilities(subject)),
    #("select", 0, with_atomics_capabilities(select)),
    #("peek", 1, with_atomics_capabilities(peek)),
    #("setTimeout", 2, with_atomics_capabilities(set_timeout)),
    #("clearTimeout", 1, with_atomics_capabilities(clear_timeout)),
    #("log", 1, with_atomics_capabilities(log)),
  ]
}

/// Wrap a HostFn so the BEAM Atomics capabilities are present on its State
/// (no-op when already installed — one Option check on the hot path).
fn with_atomics_capabilities(f: HostFn) -> HostFn {
  fn(args, this, s: State) {
    let s = case s.host_sync_wait {
      Some(_) -> s
      None -> install_atomics_capabilities(s)
    }
    f(args, this, s)
  }
}

// -- spawn -------------------------------------------------------------------

/// `spawn(fn)` — fork a new BEAM process running the JS closure `fn` against
/// a snapshot of the current heap. Returns a `Pid` object.
///
/// The child gets its own VM state and runs to completion (microtasks +
/// this module's mailbox loop) independently. Closed-over values are visible
/// because the heap is copied; mutations after the fork are not.
///
/// Only JS closures are accepted — to spawn a native function, wrap it:
/// `spawn(() => nativeFn())`.
pub fn spawn(
  args: List(JsValue),
  _this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let fn_arg = case args {
    [a, ..] -> a
    [] -> JsUndefined
  }
  case read_closure(state, fn_arg) {
    None -> state.type_error(state, "spawn: argument is not a function")
    Some(#(template, env_ref)) -> {
      let #(heap, pid_val) =
        process_objects.alloc_pid_object(
          state.heap,
          state.builtins.object.prototype,
          state.builtins.function.prototype,
          erlang_spawn(make_spawner(state, template, env_ref)),
        )
      #(State(..state, heap:), Ok(pid_val))
    }
  }
}

fn read_closure(
  state: State,
  v: JsValue,
) -> option.Option(#(value.FuncTemplate, value.Ref)) {
  use ref <- option.then(case v {
    JsObject(r) -> Some(r)
    _ -> None
  })
  case heap.read(state.heap, ref) {
    Some(ObjectSlot(kind: value.FunctionObject(func_template:, env:, ..), ..)) ->
      Some(#(func_template, env))
    _ -> None
  }
}

/// Build the thunk handed to `erlang:spawn/1`. Captures a (possibly GC'd)
/// heap snapshot plus everything `interpreter.new_state` needs to stand up a
/// fresh VM in the child, then drives it with this module's `run` loop.
fn make_spawner(
  state: State,
  template: value.FuncTemplate,
  env_ref: value.Ref,
) -> fn() -> Nil {
  // Only pay for a GC if the heap has grown well past the root set —
  // otherwise we'd just be copying builtins.
  let spawn_heap = case
    heap.size(state.heap) > set.size(heap.root_set(state.heap)) * 2
  {
    True -> heap.collect_with_roots(state.heap, set.from_list([env_ref.id]))
    False -> state.heap
  }
  let builtins = state.builtins
  let global_object = state.ctx.global_object
  let lexical_globals = state.ctx.lexical_globals
  let symbol_descriptions = state.ctx.symbol_descriptions
  let symbol_registry = state.ctx.symbol_registry
  fn() {
    let env_values = heap.read_env(spawn_heap, env_ref) |> option.unwrap([])
    let env_count = list.length(env_values)
    let remaining = template.local_count - env_count - template.arity
    let locals =
      list.flatten([
        env_values,
        list.repeat(JsUndefined, template.arity),
        list.repeat(JsUndefined, remaining),
      ])
      |> tuple_array.from_list
    // Atomics capabilities go in BEFORE the script runs — a child may hit
    // a blocking Atomics.wait at its top level, not just in `run`'s
    // macrotask phase.
    let child =
      interpreter.new_state(
        template,
        locals,
        spawn_heap,
        builtins,
        global_object,
        lexical_globals,
        symbol_descriptions,
        symbol_registry,
      )
      |> install_atomics_capabilities
    case interpreter.execute_inner(child) {
      Ok(#(_, final)) -> {
        let _ = run(final)
        Nil
      }
      Error(_vm_err) -> Nil
    }
  }
}

// -- self --------------------------------------------------------------------

/// `self()` — Pid of the current BEAM process.
pub fn self(
  _args: List(JsValue),
  _this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let pid = ffi_self()
  let #(heap, pid_val) =
    process_objects.alloc_pid_object(
      state.heap,
      state.builtins.object.prototype,
      state.builtins.function.prototype,
      pid,
    )
  #(State(..state, heap:), Ok(pid_val))
}

// -- sleep -------------------------------------------------------------------

/// `sleep(ms)` — block the current process via `timer:sleep/1`.
pub fn sleep(
  args: List(JsValue),
  _this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let ms = case args {
    [JsNumber(value.Finite(n)), ..] -> value.float_to_int(n)
    _ -> 0
  }
  case ms > 0 {
    True -> ffi_sleep(ms)
    False -> Nil
  }
  #(state, Ok(JsUndefined))
}

// -- subject -----------------------------------------------------------------

/// `subject()` — new Subject bound to this process (Erlang ref-tagged
/// mailbox channel with `.send` / `.receive` / `.receiveAsync`). The subject
/// has a unique tag (via `erlang:make_ref`) so selective receive on it is
/// O(1) for the common case.
pub fn subject(
  _args: List(JsValue),
  _this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let pid = ffi_self()
  let tag = ffi_make_ref()
  let #(heap, subject_val) =
    structured_clone.alloc_subject_object(
      state.heap,
      state.builtins.object.prototype,
      state.builtins.function.prototype,
      pid,
      tag,
    )
  // `receiveAsync` is added here rather than in `alloc_subject_object`
  // because it depends on this module's macrotask loop. Subjects that
  // arrive via deserialize (from another process) don't get it — you can
  // only async-receive on a subject your own process owns.
  let assert JsObject(subject_ref) = subject_val
  let #(heap, ra_ref) =
    common.alloc_host_fn(
      heap,
      state.builtins.function.prototype,
      subject_receive_async,
      "receiveAsync",
      0,
    )
  let heap =
    heap.update(heap, subject_ref, fn(slot) {
      case slot {
        ObjectSlot(properties:, ..) ->
          ObjectSlot(
            ..slot,
            properties: dict.insert(
              properties,
              Named("receiveAsync"),
              value.builtin_property(JsObject(ra_ref)),
            ),
          )
        other -> other
      }
    })
  #(State(..state, heap:), Ok(subject_val))
}

/// `subject.receiveAsync(timeout?)` — Promise that resolves with the next
/// message on this subject. Non-blocking: `host.suspend` hands out the
/// promise, the (settle ref, subject tag) pair is parked in the process
/// dictionary, and `run` resolves it when the message (or `ReceiverTimeout`)
/// lands in the mailbox.
fn subject_receive_async(
  args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let tag = case this {
    JsObject(ref) ->
      heap.read_subject(state.heap, ref) |> option.map(fn(p) { p.1 })
    _ -> None
  }
  case tag {
    None ->
      state.type_error(state, "Subject.receiveAsync: this is not a Subject")
    Some(tag) -> {
      let #(state, promise, data_ref) = host.suspend(state)
      let #(tag_map, ref_map) = receivers_get()
      let queue = dict.get(tag_map, tag) |> result.unwrap([])
      receivers_put(#(
        dict.insert(tag_map, tag, list.append(queue, [data_ref])),
        dict.insert(ref_map, data_ref, tag),
      ))
      case args {
        [JsNumber(value.Finite(n)), ..] -> {
          let ms = value.float_to_int(n)
          case ms >= 0 {
            True -> {
              let _ = ffi_send_after(ms, ffi_self(), ReceiverTimeout(data_ref))
              Nil
            }
            False -> Nil
          }
        }
        _ -> Nil
      }
      #(state, Ok(promise))
    }
  }
}

// -- select ------------------------------------------------------------------

/// `select()` — empty Selector; chain `.on(subject, mapper?)` then
/// `.receive(timeout?)` to block until a message arrives on any of them.
pub fn select(
  _args: List(JsValue),
  _this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let #(heap, selector) = alloc_selector_object(state, [])
  #(State(..state, heap:), Ok(selector))
}

fn alloc_selector_object(
  state: State,
  entries: List(#(value.ErlangRef, JsValue)),
) -> #(Heap, JsValue) {
  let function_proto = state.builtins.function.prototype
  let #(heap, on_ref) =
    common.alloc_host_fn(state.heap, function_proto, selector_on, "on", 1)
  let #(heap, receive_ref) =
    common.alloc_host_fn(heap, function_proto, selector_receive, "receive", 0)
  let #(heap, ref) =
    heap.alloc(
      heap,
      ObjectSlot(
        kind: SelectorObject(entries:),
        properties: common.named_props([
          #("on", value.builtin_property(JsObject(on_ref))),
          #("receive", value.builtin_property(JsObject(receive_ref))),
        ]),
        elements: elements.new(),
        prototype: Some(state.builtins.object.prototype),
        symbol_properties: [common.to_string_tag("Selector")],
        extensible: True,
      ),
    )
  #(heap, JsObject(ref))
}

/// selector.on(subject, mapper?) — return a NEW selector with `subject`
/// registered. `mapper` defaults to identity (the raw message is returned).
fn selector_on(
  args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let entries = case this {
    JsObject(ref) -> heap.read_selector(state.heap, ref)
    _ -> None
  }
  case entries {
    None -> state.type_error(state, "Selector.on: this is not a Selector")
    Some(entries) -> {
      let #(subj_arg, mapper_arg) = case args {
        [s, m, ..] -> #(s, m)
        [s] -> #(s, JsUndefined)
        [] -> #(JsUndefined, JsUndefined)
      }
      let tag = case subj_arg {
        JsObject(ref) ->
          heap.read_subject(state.heap, ref)
          |> option.map(fn(p) { p.1 })
        _ -> None
      }
      case tag {
        None ->
          state.type_error(state, "Selector.on: argument is not a Subject")
        Some(tag) -> {
          let #(heap, selector) =
            alloc_selector_object(state, [#(tag, mapper_arg), ..entries])
          #(State(..state, heap:), Ok(selector))
        }
      }
    }
  }
}

/// selector.receive(timeout?) — block until a message arrives on any
/// registered subject, run its mapper (or identity), and return the result.
/// With a timeout, returns `undefined` if it elapses.
fn selector_receive(
  args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let entries = case this {
    JsObject(ref) -> heap.read_selector(state.heap, ref)
    _ -> None
  }
  case entries {
    None -> state.type_error(state, "Selector.receive: this is not a Selector")
    Some([]) ->
      state.type_error(state, "Selector.receive: no subjects registered")
    Some(entries) -> {
      let #(ref_map, handler_map) =
        list.fold(entries, #(dict.new(), dict.new()), fn(acc, entry) {
          let #(rm, hm) = acc
          let #(tag, handler) = entry
          #(dict.insert(rm, tag, True), dict.insert(hm, tag, handler))
        })
      case args {
        [JsNumber(value.Finite(n)), ..] -> {
          let ms = value.float_to_int(n)
          case ms >= 0 {
            False -> #(state, Ok(JsUndefined))
            True ->
              case ffi_select_timeout(ref_map, ms) {
                Ok(#(matched_ref, pm)) ->
                  select_handle_match(state, matched_ref, pm, handler_map)
                Error(Nil) -> #(state, Ok(JsUndefined))
              }
          }
        }
        _ -> {
          let #(matched_ref, pm) = ffi_select(ref_map)
          select_handle_match(state, matched_ref, pm, handler_map)
        }
      }
    }
  }
}

/// Deserialize the matched message and call its mapper if one was registered.
fn select_handle_match(
  state: State,
  matched_ref: value.ErlangRef,
  pm: PortableMessage,
  handler_map: dict.Dict(value.ErlangRef, JsValue),
) -> #(State, Result(JsValue, JsValue)) {
  let #(heap, val) =
    structured_clone.deserialize(state.heap, state.builtins, pm)
  let state = State(..state, heap:)
  case dict.get(handler_map, matched_ref) |> result.unwrap(JsUndefined) {
    JsUndefined -> #(state, Ok(val))
    mapper ->
      case state.call(state, mapper, JsUndefined, [val]) {
        Ok(#(result, state)) -> #(state, Ok(result))
        Error(#(thrown, state)) -> #(state, Error(thrown))
      }
  }
}

// -- peek --------------------------------------------------------------------

/// `peek(promise)` — synchronous promise introspection:
/// `{type: 'pending' | 'resolved' | 'rejected', value?, reason?}`.
pub fn peek(
  args: List(JsValue),
  _this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let arg = case args {
    [a, ..] -> a
    [] -> JsUndefined
  }

  case read_promise_state(state.heap, arg) {
    Some(promise_state) -> {
      let props = case promise_state {
        PromisePending -> [#("type", value.data_property(JsString("pending")))]
        PromiseFulfilled(value:) -> [
          #("type", value.data_property(JsString("resolved"))),
          #("value", value.data_property(value)),
        ]
        PromiseRejected(reason:) -> [
          #("type", value.data_property(JsString("rejected"))),
          #("reason", value.data_property(reason)),
        ]
      }

      let #(heap, result_ref) =
        common.alloc_pojo(state.heap, state.builtins.object.prototype, props)
      #(State(..state, heap:), Ok(JsObject(result_ref)))
    }
    None -> state.type_error(state, "peek: argument is not a Promise")
  }
}

fn read_promise_state(
  h: Heap,
  val: JsValue,
) -> option.Option(value.PromiseState) {
  use ref <- option.then(case val {
    JsObject(r) -> Some(r)
    _ -> None
  })
  use data_ref <- option.then(heap.read_promise_data_ref(h, ref))
  heap.read_promise_state(h, data_ref)
}

// -- setTimeout --------------------------------------------------------------

/// `setTimeout(fn, ms)` — `host.suspend` a promise, attach `fn` as its
/// fulfil handler, and have `erlang:send_after/3` post a `SettlePromise`
/// back to this process after `ms`. The `run` loop turns that into
/// `host.resume`, which schedules the reaction job that calls `fn`.
pub fn set_timeout(
  args: List(JsValue),
  _this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let #(callback, ms) = case args {
    [cb, JsNumber(value.Finite(n)), ..] -> #(cb, value.float_to_int(n))
    [cb, ..] -> #(cb, 0)
    [] -> #(JsUndefined, 0)
  }
  let ms = case ms < 0 {
    True -> 0
    False -> ms
  }
  let #(state, _promise, data_ref) = host.suspend(state)
  let state =
    builtins_promise.perform_promise_then(
      state,
      data_ref,
      callback,
      JsUndefined,
      JsUndefined,
      JsUndefined,
    )
  let timer_ref =
    ffi_send_after(
      ms,
      ffi_self(),
      SettlePromise(
        data_ref,
        Ok(PortableMessage(root: PvUndefined, records: dict.new())),
      ),
    )
  let #(heap, timer_obj) =
    heap.alloc(
      state.heap,
      ObjectSlot(
        kind: value.TimerObject(timer_ref:, data_ref:),
        properties: dict.new(),
        elements: elements.new(),
        prototype: Some(state.builtins.object.prototype),
        symbol_properties: [],
        extensible: True,
      ),
    )
  #(State(..state, heap:), Ok(JsObject(timer_obj)))
}

// -- clearTimeout ------------------------------------------------------------

/// `clearTimeout(timer)` — cancel a pending timer from `setTimeout`. If the
/// timer hasn't fired yet, the callback will not be invoked and `outstanding`
/// is decremented. If it already fired (or `timer` isn't a timer), this is a
/// no-op.
pub fn clear_timeout(
  args: List(JsValue),
  _this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let cancelled =
    args
    |> list.first
    |> option.from_result
    |> option.then(as_timer_ref(state.heap, _))
    |> option.map(ffi_cancel_timer)
    |> option.unwrap(False)
  let state = case cancelled {
    True -> State(..state, outstanding: state.outstanding - 1)
    False -> state
  }
  #(state, Ok(JsUndefined))
}

fn as_timer_ref(h: Heap, val: JsValue) -> Option(value.ErlangTimerRef) {
  case val {
    JsObject(ref) ->
      case heap.read(h, ref) {
        Some(ObjectSlot(kind: value.TimerObject(timer_ref:, ..), ..)) ->
          Some(timer_ref)
        _ -> None
      }
    _ -> None
  }
}

// -- log ---------------------------------------------------------------------

/// `log(...args)` — print to stdout. Kept here for spawned processes that
/// don't have `console`; prefer the built-in `console.log` otherwise.
pub fn log(
  args: List(JsValue),
  _this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  console.print(args, state, io.println)
}
