// ============================================================================
// Promise microtask queue draining.
//
// Core owns no timer wheel and defines no `setTimeout` — the ONLY deadline
// source it knows about is Atomics.waitAsync waiter timeouts. `drain_jobs`
// flushes the Promise reaction queue, settling expired waiters between
// flushes, and sleeps until the earliest pending waiter deadline before
// exiting. Everything else that could produce a macrotask — timers, IO,
// process messages — is an embedder concern layered on top; see
// `arc/beam.run` for the Erlang-mailbox version, built on
// `host.suspend`/`host.resume`.
// ============================================================================

import arc/vm/builtins/atomics as builtins_atomics
import arc/vm/exec/job_call
import arc/vm/heap
import arc/vm/host_hooks
import arc/vm/internal/job_queue
import arc/vm/ops/object
import arc/vm/state.{type State, State}
import arc/vm/value.{type JsValue, JsUndefined}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}

/// Drain the microtask queue.
pub fn finish(state: State(host)) -> State(host) {
  drain_jobs(state)
}

/// Print warnings for any promises that were rejected without a handler.
/// Called after all jobs have been drained (like QuickJS's
/// js_std_promise_rejection_check).
fn report_unhandled_rejections(state: State(host)) -> Nil {
  list.each(state.unhandled_rejections, fn(data_ref) {
    case heap.read_promise_state(state.heap, data_ref) {
      Some(value.PromiseRejected(reason)) ->
        io.println_error(
          "Uncaught (in promise) " <> object.format_error(reason, state.heap),
        )
      _ -> Nil
    }
  })
}

/// Drain all jobs in the job queue, processing any new jobs that get enqueued
/// during execution. Loops until the queue is empty. When empty, reports any
/// unhandled promise rejections (like Node.js checking after each microtask flush).
///
/// The only deadlines the core loop honors are Atomics.waitAsync timeouts:
/// each pass settles waiters whose deadline already passed, and when the
/// queue runs dry with waiter deadlines still pending, the loop sleeps until
/// the earliest one instead of exiting. Core has no timers of its own —
/// embedders that want `setTimeout` schedule it themselves around this drain.
pub fn drain_jobs(state: State(host)) -> State(host) {
  do_drain_jobs(state, False)
}

/// `drain_jobs` for embedder macrotask loops that own a mailbox receive
/// (arc/beam.run): when an embedder-visible wake source is pending —
/// outstanding `host.suspend` promises, or pending Atomics.waitAsync
/// waiters whose cross-process notify wakes arrive in the EMBEDDER's
/// mailbox — and the job queue is dry, return instead of sleeping (or
/// exiting). Sleeping here would starve the embedder's mailbox (IO,
/// process messages, arc_notify wakes) until the deadline. The embedder
/// bounds its blocking receive with `next_deadline_timeout`, injects
/// notify wakes via `inject_notify`, and re-drains so waitAsync
/// deadlines and mailbox events interleave.
pub fn drain_jobs_yielding(state: State(host)) -> State(host) {
  do_drain_jobs(state, True)
}

/// Wake injection — the embedder side of a cross-process Atomics.notify
/// (the wake-injection side of `host_hooks.AtomicsCapabilities`). When an
/// `{arc_notify, Ref, Key, ByteIndex}` message lands in the EMBEDDER's
/// mailbox (core owns no receive), the embedder hands the wake to core
/// here: settles this agent's first pending waitAsync waiter on
/// (key, byte index) with "ok"; a stale wake — the waiter already expired
/// or was cancelled — settles nothing. Re-drain afterwards
/// (`drain_jobs` / `drain_jobs_yielding`) so the reaction jobs run.
pub fn inject_notify(
  state: State(host),
  key: host_hooks.WaiterKey,
  byte_index: Int,
) -> State(host) {
  builtins_atomics.settle_notified_waiter(state, key, byte_index)
}

fn do_drain_jobs(state: State(host), yield_to_embedder: Bool) -> State(host) {
  let state = case state.atomics_waiters {
    [] -> state
    _ -> builtins_atomics.settle_expired_waiters(state)
  }
  case job_queue.pop(state.job_queue) {
    None -> {
      // Embedder-visible wake sources: `host.suspend` promises settle
      // from the embedder's mailbox, and pending waitAsync waiters can
      // be woken by a cross-process Atomics.notify whose wake message
      // also lands in the embedder's mailbox (inject_notify). Core
      // owns no receive, so a yielding drain must hand control back
      // whenever either is pending.
      let embedder_wake_pending =
        state.outstanding > 0 || state.atomics_waiters != []
      // The hand-back is decided BEFORE the deadline dispatch: a pending
      // embedder wake with no waiter deadline (an outstanding `host.suspend`
      // promise, say) still means this drain is not done — falling through to
      // `finish_drain` there would terminally exit and report rejections the
      // resumed job may still handle.
      //
      // NOT a drain exit: control comes back here (the embedder consumes
      // its wake and re-drains), so this return deliberately does NOT run
      // `finish_drain`. INVARIANT: unhandled-rejection reporting happens
      // only where the drain is done for good, because a wake-driven job
      // that runs after this hand-back can still attach a handler to a
      // promise sitting in `unhandled_rejections` — reporting (and
      // clearing) it here would print a rejection that ends up handled.
      // The list is threaded through untouched and reported by whichever
      // later drain reaches `finish_drain`.
      use <- hand_back_if(yield_to_embedder && embedder_wake_pending, state)
      case earliest_deadline(state) {
        Some(deadline) -> {
          let wait_ms =
            int.max(deadline - state.ctx.host_hooks.monotonic_now(), 0) + 1
          // Plain bounded sleep until the earliest deadline — never a
          // mailbox receive in core. A cross-process notify that lands
          // during the sleep sits in the owning embedder's mailbox
          // until its loop consumes it (embedder loops use the
          // yielding drain, so they don't reach this arm with waiters
          // pending). CAVEAT: a NON-yielding drain reaching this arm
          // sleeps through any such wake — pending waiters settle
          // "timed-out" at their deadline and infinite waiters exit
          // the drain unsettled. That is only correct when the driver
          // guarantees no cross-process wake source exists; a driver
          // that exposes agent spawning (e.g. Arc.spawn) MUST use a
          // notify-consuming embedder loop (beam.run / the yielding
          // drain + wait_for_notify + inject_notify), not this one.
          let Nil = state.ctx.host_hooks.sleep_ms(wait_ms)
          do_drain_jobs(
            builtins_atomics.settle_expired_waiters(state),
            yield_to_embedder,
          )
        }
        None -> finish_drain(state)
      }
    }
    Some(#(job, rest)) -> {
      let state = State(..state, job_queue: rest)
      let state = execute_job(state, job)
      do_drain_jobs(state, yield_to_embedder)
    }
  }
}

/// The yielding drain's non-terminal hand-back: `state` as-is (no
/// `finish_drain`) when the embedder has a wake pending, otherwise carry on.
fn hand_back_if(
  pending: Bool,
  state: State(host),
  otherwise: fn() -> State(host),
) -> State(host) {
  case pending {
    True -> state
    False -> otherwise()
  }
}

/// The drain's TERMINAL exit — the queue is dry and there is nothing left for
/// this drain to wait on. The one place unhandled rejections are reported and
/// the pending list cleared, so "did we report?" cannot depend on which
/// return the drain took. The other return out of `do_drain_jobs` (the
/// yielding hand-back) is deliberately not terminal — see the comment there.
fn finish_drain(state: State(host)) -> State(host) {
  report_unhandled_rejections(state)
  State(..state, unhandled_rejections: [])
}

/// Earliest pending Atomics.waitAsync waiter deadline, if any.
fn earliest_deadline(state: State(host)) -> Option(Int) {
  builtins_atomics.earliest_waiter_deadline(state)
}

/// Milliseconds until the earliest pending Atomics.waitAsync deadline, if any.
/// Embedder macrotask loops that own a mailbox receive use this as their
/// receive timeout so waitAsync deadlines and mailbox IO interleave correctly.
pub fn next_deadline_timeout(state: State(host)) -> Option(Int) {
  case earliest_deadline(state) {
    Some(deadline) ->
      Some(int.max(deadline - state.ctx.host_hooks.monotonic_now(), 0) + 1)
    None -> None
  }
}

/// Execute a single job from the promise job queue.
fn execute_job(state: State(host), job: value.Job) -> State(host) {
  case job {
    value.PromiseReactionJob(handler:, arg:, resolve:, reject:) ->
      execute_reaction_job(state, handler, arg, resolve, reject)
    value.PromiseResolveThenableJob(thenable:, then_fn:, resolve:, reject:) ->
      execute_thenable_job(state, thenable, then_fn, resolve, reject)
    value.HostJob(run:) -> execute_host_job(state, run)
  }
}

/// Execute a host job: call `run()` for its effects. The job carries no child
/// promise capability, so there is nothing to settle with the return value —
/// it is discarded, and an abrupt completion is reported like any other
/// job-level throw.
fn execute_host_job(state: State(host), run: JsValue) -> State(host) {
  job_call.call_settlement_fn(state, run, [])
}

/// Execute a promise reaction job (ES2024 §27.2.2.1 NewPromiseReactionJob):
/// - Handler(f): call f(arg), resolve the child with the result, or reject
///   the child if f throws
/// - IdentityPassThrough: resolve the child with arg as-is
/// - ThrowerPassThrough: reject the child with arg as-is
fn execute_reaction_job(
  state: State(host),
  handler: value.ReactionHandler,
  arg: JsValue,
  resolve: JsValue,
  reject: JsValue,
) -> State(host) {
  case handler {
    value.IdentityPassThrough ->
      job_call.call_settlement_fn(state, resolve, [arg])
    value.ThrowerPassThrough ->
      job_call.call_settlement_fn(state, reject, [arg])
    value.Handler(fun) -> {
      // Call handler(arg)
      case state.call(state, fun, JsUndefined, [arg]) {
        Ok(#(return_val, new_state)) ->
          // Resolve child with handler's return value
          job_call.call_settlement_fn(new_state, resolve, [return_val])
        Error(#(thrown, new_state)) ->
          // Handler threw — reject child
          job_call.call_settlement_fn(new_state, reject, [thrown])
      }
    }
  }
}

/// Execute a thenable job: call thenable.then(resolve, reject)
fn execute_thenable_job(
  state: State(host),
  thenable: JsValue,
  then_fn: JsValue,
  resolve: JsValue,
  reject: JsValue,
) -> State(host) {
  let result = state.call(state, then_fn, thenable, [resolve, reject])
  case result {
    Ok(#(_return_val, new_state)) -> new_state
    Error(#(thrown, new_state)) ->
      // then() threw — reject the promise
      job_call.call_settlement_fn(new_state, reject, [thrown])
  }
}
