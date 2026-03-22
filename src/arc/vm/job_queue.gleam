// ============================================================================
// Promise job queue draining
// ============================================================================

import arc/vm/builtins/arc as builtins_arc
import arc/vm/builtins/promise as builtins_promise
import arc/vm/coerce
import arc/vm/frame.{type State, State}
import arc/vm/heap
import arc/vm/object
import arc/vm/value.{type JsValue, JsNull, JsUndefined}
import gleam/io
import gleam/list
import gleam/option.{Some}

/// Drain jobs, using the event loop if enabled on the state, otherwise
/// just flushing the microtask queue.
pub fn finish(state: State) -> State {
  case state.event_loop {
    True -> run_event_loop(state)
    False -> drain_jobs(state)
  }
}

/// Print warnings for any promises that were rejected without a handler.
/// Called after all jobs have been drained (like QuickJS's
/// js_std_promise_rejection_check).
fn report_unhandled_rejections(state: State) -> Nil {
  list.each(state.unhandled_rejections, fn(data_ref) {
    case heap.read(state.heap, data_ref) {
      Some(value.PromiseSlot(state: value.PromiseRejected(reason), ..)) ->
        io.println_error(
          "Uncaught (in promise): " <> object.inspect(reason, state.heap),
        )
      _ -> Nil
    }
  })
}

/// Drain all jobs in the job queue, processing any new jobs that get enqueued
/// during execution. Loops until the queue is empty. When empty, reports any
/// unhandled promise rejections (like Node.js checking after each microtask flush).
pub fn drain_jobs(state: State) -> State {
  case state.job_queue {
    [] -> {
      report_unhandled_rejections(state)
      State(..state, unhandled_rejections: [])
    }
    [job, ..rest] -> {
      let state = State(..state, job_queue: rest)
      let state = execute_job(state, job)
      drain_jobs(state)
    }
  }
}

@external(erlang, "arc_vm_ffi", "receive_any_event")
fn ffi_receive_any() -> value.MailboxEvent

@external(erlang, "arc_vm_ffi", "receive_settle_only")
fn ffi_receive_settle_only() -> value.MailboxEvent

/// Mailbox-backed event loop. Runs drain microtasks -> block on BEAM mailbox
/// -> handle event -> repeat, until `outstanding` hits zero. With no outstanding
/// work this is identical to `drain_jobs` (never touches the mailbox).
///
/// This is what lets `await Arc.receiveAsync()` suspend the current async
/// function while other async functions keep running -- the BEAM mailbox IS
/// the macrotask queue, and every arrival resolves a promise which schedules
/// a PromiseReactionJob that resumes whoever was waiting.
///
/// Selective receive: when no receivers are pending we only accept
/// `SettlePromise`, leaving `UserMessage` in the BEAM mailbox for blocking
/// `Arc.receive()` or a future `receiveAsync` to pick up.
pub fn run_event_loop(state: State) -> State {
  let state = drain_jobs(state)
  case state.outstanding {
    0 -> state
    _ -> {
      let event = case state.pending_receivers {
        [] -> ffi_receive_settle_only()
        [_, ..] -> ffi_receive_any()
      }
      let state = handle_mailbox_event(state, event)
      run_event_loop(state)
    }
  }
}

/// Apply a single mailbox event to VM state: resolve the right promise,
/// enqueue its reaction jobs, adjust the outstanding count.
fn handle_mailbox_event(state: State, event: value.MailboxEvent) -> State {
  case event {
    value.UserMessage(pm) -> {
      // Selective receive guarantees pending_receivers is non-empty here.
      let assert [data_ref, ..rest] = state.pending_receivers
      let #(heap, val) =
        builtins_arc.deserialize(state.heap, state.builtins, pm)
      let #(heap, jobs) = builtins_promise.fulfill_promise(heap, data_ref, val)
      State(
        ..state,
        heap:,
        pending_receivers: rest,
        outstanding: state.outstanding - 1,
        job_queue: list.append(state.job_queue, jobs),
      )
    }
    value.SettlePromise(data_ref:, outcome: Ok(pm)) -> {
      let #(heap, val) =
        builtins_arc.deserialize(state.heap, state.builtins, pm)
      let #(heap, jobs) = builtins_promise.fulfill_promise(heap, data_ref, val)
      State(
        ..state,
        heap:,
        outstanding: state.outstanding - 1,
        job_queue: list.append(state.job_queue, jobs),
      )
    }
    value.SettlePromise(data_ref:, outcome: Error(pm)) -> {
      let #(heap, reason) =
        builtins_arc.deserialize(state.heap, state.builtins, pm)
      let state =
        builtins_promise.reject_promise(State(..state, heap:), data_ref, reason)
      State(..state, outstanding: state.outstanding - 1)
    }
    value.ReceiverTimeout(data_ref:) ->
      case list.contains(state.pending_receivers, data_ref) {
        False -> state
        True -> {
          let #(heap, jobs) =
            builtins_promise.fulfill_promise(state.heap, data_ref, JsUndefined)
          State(
            ..state,
            heap:,
            pending_receivers: list.filter(state.pending_receivers, fn(r) {
              r != data_ref
            }),
            outstanding: state.outstanding - 1,
            job_queue: list.append(state.job_queue, jobs),
          )
        }
      }
  }
}

/// Execute a single job from the promise job queue.
fn execute_job(state: State, job: value.Job) -> State {
  case job {
    value.PromiseReactionJob(handler:, arg:, resolve:, reject:) ->
      execute_reaction_job(state, handler, arg, resolve, reject)
    value.PromiseResolveThenableJob(thenable:, then_fn:, resolve:, reject:) ->
      execute_thenable_job(state, thenable, then_fn, resolve, reject)
  }
}

/// Helper: Call a function via frame.call during job execution (fire-and-forget).
/// Used for calling resolve/reject on child promises after a handler runs.
fn call_for_job(state: State, target: JsValue, args: List(JsValue)) -> State {
  case frame.call(state, target, JsUndefined, args) {
    Ok(#(_, new_state)) -> new_state
    Error(#(_, new_state)) -> new_state
  }
}

/// Execute a promise reaction job:
/// - If handler is undefined/not callable: pass-through (resolve/reject with arg)
/// - If handler is callable: call handler(arg), resolve child with result,
///   or reject child if handler throws
fn execute_reaction_job(
  state: State,
  handler: JsValue,
  arg: JsValue,
  resolve: JsValue,
  reject: JsValue,
) -> State {
  case coerce.is_callable_value(state.heap, handler) {
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
      let result = frame.call(state, handler, JsUndefined, [arg])
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
  state: State,
  thenable: JsValue,
  then_fn: JsValue,
  resolve: JsValue,
  reject: JsValue,
) -> State {
  let result = frame.call(state, then_fn, thenable, [resolve, reject])
  case result {
    Ok(#(_return_val, new_state)) -> new_state
    Error(#(thrown, new_state)) ->
      // then() threw — reject the promise
      call_for_job(new_state, reject, [thrown])
  }
}
