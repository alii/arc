// ============================================================================
// Promise microtask queue draining + handler execution.
//
// The only macrotasks core knows about are host timers (the global
// setTimeout) and Atomics.waitAsync deadlines. `drain_jobs` flushes the
// Promise reaction queue, fires due timers between flushes, and sleeps
// until the earliest pending deadline before exiting. Embedders that need
// IO / process messages run their own loop on top — see `arc/beam.run` for
// the Erlang-mailbox version, built on `host.suspend`/`host.resume`.
// ============================================================================

import arc/vm/builtins/atomics as builtins_atomics
import arc/vm/builtins/common
import arc/vm/builtins/helpers
import arc/vm/completion.{NormalCompletion, ThrowCompletion}
import arc/vm/exec/frame
import arc/vm/heap
import arc/vm/internal/job_queue
import arc/vm/ops/object
import arc/vm/state.{
  type NativeFnSlot, type State, type StepResult, type VmError, State,
  StepVmError, Thrown,
}
import arc/vm/value.{
  type FuncTemplate, type JsValue, type Ref, FunctionObject, JsNull, JsObject,
  JsUndefined, NativeFunction, ObjectSlot,
}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}

pub type ExecuteInnerFn(host) =
  fn(State(host)) ->
    Result(#(completion.Completion(host), State(host)), VmError)

pub type CallNativeFn(host) =
  fn(State(host), NativeFnSlot(host), List(JsValue), List(JsValue), JsValue) ->
    Result(State(host), #(StepResult, JsValue, State(host)))

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
/// The core loop honors two kinds of deadlines: Atomics.waitAsync timeouts
/// (each pass settles waiters whose deadline already passed) and host timers
/// from the global setTimeout (fired one at a time when the microtask queue
/// is empty). When the queue runs dry with deadlines still pending, the loop
/// sleeps until the earliest one instead of exiting.
pub fn drain_jobs(state: State(host)) -> State(host) {
  do_drain_jobs(state, False)
}

/// `drain_jobs` for embedder macrotask loops that own a mailbox receive
/// (arc/beam.run): when an embedder-visible wake source is pending —
/// outstanding `host.suspend` promises, or pending Atomics.waitAsync
/// waiters whose cross-process notify wakes arrive in the EMBEDDER's
/// mailbox — and only future deadlines remain, return instead of
/// sleeping. Sleeping here would starve the embedder's mailbox (IO,
/// process messages, arc_notify wakes) until the deadline. The embedder
/// bounds its blocking receive with `next_deadline_timeout`, injects
/// notify wakes via `inject_notify`, and re-drains so host timers and
/// mailbox events interleave.
pub fn drain_jobs_yielding(state: State(host)) -> State(host) {
  do_drain_jobs(state, True)
}

/// Wake injection — the embedder side of a cross-process Atomics.notify
/// (host contract clause 3, arc/host.gleam). When an
/// `{arc_notify, Ref, Key, ByteIndex}` message lands in the EMBEDDER's
/// mailbox (core owns no receive), the embedder hands the wake to core
/// here: settles this agent's first pending waitAsync waiter on
/// (key, byte index) with "ok"; a stale wake — the waiter already expired
/// or was cancelled — settles nothing. Re-drain afterwards
/// (`drain_jobs` / `drain_jobs_yielding`) so the reaction jobs run.
pub fn inject_notify(
  state: State(host),
  key: builtins_atomics.WaiterKey,
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
      case earliest_deadline(state) {
        Some(_) if yield_to_embedder && embedder_wake_pending -> state
        Some(deadline) -> {
          let wait_ms =
            int.max(deadline - builtins_atomics.monotonic_now(), 0) + 1
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
          let Nil = builtins_atomics.sleep_ms(wait_ms)
          do_drain_jobs(
            builtins_atomics.settle_expired_waiters(state),
            yield_to_embedder,
          )
        }
        None -> {
          report_unhandled_rejections(state)
          State(..state, unhandled_rejections: [])
        }
      }
    }
    Some(#(job, rest)) -> {
      let state = State(..state, job_queue: rest)
      let state = execute_job(state, job)
      do_drain_jobs(state, yield_to_embedder)
    }
  }
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
      Some(int.max(deadline - builtins_atomics.monotonic_now(), 0) + 1)
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
  }
}

/// Helper: Call a function via state.call during job execution (fire-and-forget).
/// Used for calling resolve/reject on child promises after a handler runs.
fn call_for_job(
  state: State(host),
  target: JsValue,
  args: List(JsValue),
) -> State(host) {
  case state.call(state, target, JsUndefined, args) {
    Ok(#(_, new_state)) -> new_state
    Error(#(_, new_state)) -> new_state
  }
}

/// Execute a promise reaction job:
/// - If handler is undefined/not callable: pass-through (resolve/reject with arg)
/// - If handler is callable: call handler(arg), resolve child with result,
///   or reject child if handler throws
fn execute_reaction_job(
  state: State(host),
  handler: JsValue,
  arg: JsValue,
  resolve: JsValue,
  reject: JsValue,
) -> State(host) {
  case helpers.is_callable(state.heap, handler) {
    False -> {
      // JsUndefined = fulfill pass-through, JsNull = reject pass-through
      let target = case handler {
        JsNull -> reject
        _ -> resolve
      }
      call_for_job(state, target, [arg])
    }
    True -> {
      // Call handler(arg)
      let result = state.call(state, handler, JsUndefined, [arg])
      case result {
        Ok(#(return_val, new_state)) ->
          // Resolve child with handler's return value
          call_for_job(new_state, resolve, [return_val])
        Error(#(thrown, new_state)) ->
          // Handler threw — reject child
          call_for_job(new_state, reject, [thrown])
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
      call_for_job(new_state, reject, [thrown])
  }
}

// ============================================================================
// Handler execution (for call_fn_callback / re-entrant calls)
// ============================================================================

/// Call a JS function value with `this` and `args`, running it (and anything
/// synchronous it triggers) to completion. **Lossless**: hands back the full
/// `Completion` — or a `VmError` — and panics on nothing, so each caller narrows
/// only as far as its own contract demands (`call_fn_callback` to the host-fn
/// value/throw, panicking on the impossible; `entry.run_export` not at all).
/// Parameterized over `execute_inner`/`call_native_fn` to avoid a cycle with the
/// interpreter.
pub fn call_to_completion(
  state: State(host),
  callee: JsValue,
  this_val: JsValue,
  args: List(JsValue),
  execute_inner: ExecuteInnerFn(host),
  call_native_fn: CallNativeFn(host),
) -> Result(#(completion.Completion(host), State(host)), VmError) {
  case callee {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(
          kind: FunctionObject(func_template:, env: env_ref, home_object:),
          ..,
        )) ->
          call_closure(
            state,
            ref,
            env_ref,
            home_object,
            func_template,
            args,
            this_val,
            execute_inner,
          )
        Some(ObjectSlot(kind: NativeFunction(native, ..), ..)) ->
          call_native_to_completion(
            state,
            native,
            args,
            this_val,
            call_native_fn,
          )
        // §10.5.12 Proxy [[Call]] — run the apply trap (or forward to the
        // target) and recurse, so re-entrant native calls can invoke proxies.
        Some(ObjectSlot(
          kind: value.ProxyObject(target:, handler:, callable: True, ..),
          ..,
        )) ->
          case object.proxy_trap(state, target, handler, "apply") {
            Error(#(thrown, state)) ->
              Ok(#(ThrowCompletion(thrown, state.heap), state))
            Ok(#(t, h, trap, state)) ->
              case trap {
                None ->
                  call_to_completion(
                    state,
                    JsObject(t),
                    this_val,
                    args,
                    execute_inner,
                    call_native_fn,
                  )
                Some(trap_fn) -> {
                  let #(heap, args_arr) =
                    common.alloc_array(
                      state.heap,
                      args,
                      state.builtins.array.prototype,
                    )
                  call_to_completion(
                    State(..state, heap:),
                    trap_fn,
                    JsObject(h),
                    [JsObject(t), this_val, JsObject(args_arr)],
                    execute_inner,
                    call_native_fn,
                  )
                }
              }
          }
        // Not callable: preserve the legacy pass-through (undefined, no throw)
        // that promise reactions and friends rely on.
        _ -> Ok(#(NormalCompletion(JsUndefined, state.heap), state))
      }
    _ -> Ok(#(NormalCompletion(JsUndefined, state.heap), state))
  }
}

/// Native arm of `call_to_completion`: run the native synchronously and wrap its
/// stack result (or thrown value) as a Completion. A `VmError` propagates.
fn call_native_to_completion(
  state: State(host),
  native: NativeFnSlot(host),
  args: List(JsValue),
  this_val: JsValue,
  call_native_fn: CallNativeFn(host),
) -> Result(#(completion.Completion(host), State(host)), VmError) {
  let job_state =
    State(
      ..state,
      stack: [],
      pc: 0,
      code: frame.return_code_sentinel(),
      call_stack: [],
      try_stack: [],
    )
  case call_native_fn(job_state, native, args, [], this_val) {
    Ok(new_state) -> {
      let merged =
        State(..state.merge_globals(state, new_state, []), heap: new_state.heap)
      let result = case new_state.stack {
        [r, ..] -> r
        [] -> JsUndefined
      }
      Ok(#(NormalCompletion(result, merged.heap), merged))
    }
    Error(#(StepVmError(vm_err), _, _post)) -> Error(vm_err)
    Error(#(Thrown, thrown, post)) ->
      Ok(#(ThrowCompletion(thrown, post.heap), State(..state, heap: post.heap)))
    Error(#(_step, _value, post)) ->
      Ok(#(
        ThrowCompletion(JsUndefined, post.heap),
        State(..state, heap: post.heap),
      ))
  }
}

/// Closure arm of `call_to_completion`: set up the callee as the sole frame and
/// drive it; the completion (and its heap) flow straight through, untouched.
fn call_closure(
  state: State(host),
  fn_ref: Ref,
  env_ref: Ref,
  home_object: Option(Ref),
  callee_template: FuncTemplate,
  args: List(JsValue),
  this_val: JsValue,
  execute_inner: ExecuteInnerFn(host),
) -> Result(#(completion.Completion(host), State(host)), VmError) {
  let #(heap, new_this) = frame.bind_this(state, callee_template, this_val)
  // Job re-entry never has a new.target, so seed it with undefined.
  let locals =
    frame.setup_locals(
      heap,
      env_ref,
      fn_ref,
      home_object,
      callee_template,
      args,
      new_this,
      JsUndefined,
    )
  let job_state =
    State(
      ..state,
      heap:,
      stack: [],
      locals:,
      constants: callee_template.constants,
      func: callee_template,
      code: callee_template.bytecode,
      pc: 0,
      call_stack: [],
      try_stack: [],
      new_target: JsUndefined,
      call_args: args,
    )
  case execute_inner(job_state) {
    Ok(#(comp, final_state)) ->
      Ok(#(
        comp,
        State(
          ..state.merge_globals(state, final_state, []),
          heap: final_state.heap,
        ),
      ))
    Error(vm_err) -> Error(vm_err)
  }
}
