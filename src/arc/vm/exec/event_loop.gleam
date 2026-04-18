// ============================================================================
// Promise job queue draining + handler execution
// ============================================================================

import arc/vm/builtins/arc as builtins_arc
import arc/vm/builtins/common
import arc/vm/builtins/helpers
import arc/vm/builtins/promise as builtins_promise
import arc/vm/completion.{NormalCompletion, ThrowCompletion, YieldCompletion}
import arc/vm/heap
import arc/vm/internal/job_queue
import arc/vm/internal/tuple_array
import arc/vm/opcode
import arc/vm/ops/object
import arc/vm/state.{
  type Heap, type NativeFnSlot, type State, type StepResult, type VmError, State,
  StepVmError, Thrown,
}
import arc/vm/value.{
  type FuncTemplate, type JsValue, type Ref, FunctionObject, JsNull, JsObject,
  JsUndefined, NativeFunction, ObjectSlot,
}
import gleam/dict
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string

pub type ExecuteInnerFn =
  fn(State) -> Result(#(completion.Completion, State), VmError)

pub type CallNativeFn =
  fn(State, NativeFnSlot, List(JsValue), List(JsValue), JsValue) ->
    Result(State, #(StepResult, JsValue, Heap))

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
    case heap.read_promise_state(state.heap, data_ref) {
      Some(value.PromiseRejected(reason)) ->
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
  case job_queue.pop(state.job_queue) {
    None -> {
      report_unhandled_rejections(state)
      State(..state, unhandled_rejections: [])
    }
    Some(#(job, rest)) -> {
      let state = State(..state, job_queue: rest)
      let state = execute_job(state, job)
      drain_jobs(state)
    }
  }
}

@external(erlang, "arc_vm_ffi", "receive_settle_only")
fn ffi_receive_settle_only() -> value.MailboxEvent

@external(erlang, "arc_vm_ffi", "receive_settle_or_subject")
fn ffi_receive_settle_or_subject(
  ref_map: dict.Dict(value.ErlangRef, Bool),
) -> value.MailboxEvent

/// Mailbox-backed event loop. Runs drain microtasks -> block on BEAM mailbox
/// -> handle event -> repeat, until `outstanding` hits zero. With no outstanding
/// work this is identical to `drain_jobs` (never touches the mailbox).
///
/// This is what lets `await Arc.receiveAsync()` suspend the current async
/// function while other async functions keep running -- the BEAM mailbox IS
/// the macrotask queue, and every arrival resolves a promise which schedules
/// a PromiseReactionJob that resumes whoever was waiting.
///
/// Selective receive: when pending subject receivers exist, accept their
/// subject messages alongside SettlePromise/ReceiverTimeout. Otherwise only
/// accept SettlePromise/ReceiverTimeout.
pub fn run_event_loop(state: State) -> State {
  let state = drain_jobs(state)
  case state.outstanding {
    0 -> state
    _ -> {
      let event = case state.pending_receivers {
        [] -> ffi_receive_settle_only()
        receivers -> {
          let ref_map =
            list.fold(receivers, dict.new(), fn(acc, entry) {
              let #(_data_ref, subject_tag) = entry
              dict.insert(acc, subject_tag, True)
            })
          ffi_receive_settle_or_subject(ref_map)
        }
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
    value.SubjectMessage(tag:, payload: pm) -> {
      // Find the pending receiver for this subject tag.
      case find_receiver_by_tag(state.pending_receivers, tag) {
        Ok(#(data_ref, rest)) -> {
          let #(heap, val) =
            builtins_arc.deserialize(state.heap, state.builtins, pm)
          let #(heap, jobs) =
            builtins_promise.fulfill_promise(heap, data_ref, val)
          State(
            ..state,
            heap:,
            pending_receivers: rest,
            outstanding: state.outstanding - 1,
            job_queue: job_queue.append(state.job_queue, jobs),
          )
        }
        Error(Nil) -> state
      }
    }
    value.SettlePromise(data_ref:, outcome: Ok(pm)) -> {
      let #(heap, val) =
        builtins_arc.deserialize(state.heap, state.builtins, pm)
      let #(heap, jobs) = builtins_promise.fulfill_promise(heap, data_ref, val)
      State(
        ..state,
        heap:,
        outstanding: state.outstanding - 1,
        job_queue: job_queue.append(state.job_queue, jobs),
      )
    }
    value.SettlePromise(data_ref:, outcome: Error(pm)) -> {
      let #(heap, reason) =
        builtins_arc.deserialize(state.heap, state.builtins, pm)
      let state =
        builtins_promise.reject_promise(State(..state, heap:), data_ref, reason)
      State(..state, outstanding: state.outstanding - 1)
    }
    value.ReceiverTimeout(data_ref:) -> {
      let has_receiver =
        list.any(state.pending_receivers, fn(entry) { entry.0 == data_ref })
      case has_receiver {
        False -> state
        True -> {
          let #(heap, jobs) =
            builtins_promise.fulfill_promise(state.heap, data_ref, JsUndefined)
          State(
            ..state,
            heap:,
            pending_receivers: list.filter(state.pending_receivers, fn(entry) {
              entry.0 != data_ref
            }),
            outstanding: state.outstanding - 1,
            job_queue: job_queue.append(state.job_queue, jobs),
          )
        }
      }
    }
  }
}

/// Find the first pending receiver matching a subject tag, returning its
/// promise data_ref and the remaining list with it removed.
fn find_receiver_by_tag(
  receivers: List(#(Ref, value.ErlangRef)),
  tag: value.ErlangRef,
) -> Result(#(Ref, List(#(Ref, value.ErlangRef))), Nil) {
  find_receiver_by_tag_inner(receivers, tag, [])
}

fn find_receiver_by_tag_inner(
  receivers: List(#(Ref, value.ErlangRef)),
  tag: value.ErlangRef,
  acc: List(#(Ref, value.ErlangRef)),
) -> Result(#(Ref, List(#(Ref, value.ErlangRef))), Nil) {
  case receivers {
    [] -> Error(Nil)
    [#(data_ref, t), ..rest] if t == tag ->
      Ok(#(data_ref, list.append(list.reverse(acc), rest)))
    [entry, ..rest] -> find_receiver_by_tag_inner(rest, tag, [entry, ..acc])
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

/// Helper: Call a function via state.call during job execution (fire-and-forget).
/// Used for calling resolve/reject on child promises after a handler runs.
fn call_for_job(state: State, target: JsValue, args: List(JsValue)) -> State {
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
  state: State,
  handler: JsValue,
  arg: JsValue,
  resolve: JsValue,
  reject: JsValue,
) -> State {
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
  state: State,
  thenable: JsValue,
  then_fn: JsValue,
  resolve: JsValue,
  reject: JsValue,
) -> State {
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

/// Run a JS handler function with a this value and args.
/// Returns Ok(return_value, state) on success, Error(thrown, state) on throw.
pub fn run_handler_with_this(
  state: State,
  handler: JsValue,
  this_val: JsValue,
  args: List(JsValue),
  execute_inner: ExecuteInnerFn,
  call_native_fn: CallNativeFn,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  case handler {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: FunctionObject(func_template:, env: env_ref), ..)) ->
          run_closure_for_job(
            state,
            ref,
            env_ref,
            func_template,
            args,
            this_val,
            execute_inner,
          )
        Some(ObjectSlot(kind: NativeFunction(native), ..)) -> {
          // For native functions (like resolve/reject), call directly
          let job_state =
            State(
              ..state,
              stack: [],
              pc: 0,
              code: tuple_array.from_list([opcode.Return]),
              call_stack: [],
              try_stack: [],
            )
          case call_native_fn(job_state, native, args, [], this_val) {
            Ok(new_state) -> {
              let merged =
                State(
                  ..state.merge_globals(state, new_state, []),
                  heap: new_state.heap,
                )
              case new_state.stack {
                [result, ..] -> Ok(#(result, merged))
                [] -> Ok(#(JsUndefined, merged))
              }
            }
            Error(#(Thrown, thrown, h)) ->
              Error(#(thrown, State(..state, heap: h)))
            Error(#(StepVmError(vm_err), _, _heap)) ->
              panic as {
                "VM error in native call during job: " <> string.inspect(vm_err)
              }
            Error(#(_step, _value, h)) ->
              Error(#(JsUndefined, State(..state, heap: h)))
          }
        }
        _ -> Ok(#(JsUndefined, state))
      }
    _ -> Ok(#(JsUndefined, state))
  }
}

/// Run a JS closure for a job. Sets up a temporary execution context.
fn run_closure_for_job(
  state: State,
  fn_ref: Ref,
  env_ref: Ref,
  callee_template: FuncTemplate,
  args: List(JsValue),
  this_val: JsValue,
  execute_inner: ExecuteInnerFn,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  let env_values = heap.read_env(state.heap, env_ref) |> option.unwrap([])
  let env_count = list.length(env_values)
  let padded_args = pad_args(args, callee_template.arity)
  let remaining =
    callee_template.local_count - env_count - callee_template.arity
  let locals =
    list.flatten([
      env_values,
      padded_args,
      list.repeat(JsUndefined, remaining),
    ])
    |> tuple_array.from_list
  let #(heap, new_this) = bind_this(state, callee_template, this_val)
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
      finally_stack: [],
      this_binding: new_this,
      callee_ref: Some(fn_ref),
      call_args: args,
    )
  case execute_inner(job_state) {
    Ok(#(NormalCompletion(val, h), final_state)) ->
      Ok(#(val, State(..state.merge_globals(state, final_state, []), heap: h)))
    Ok(#(ThrowCompletion(thrown, h), final_state)) ->
      Error(#(
        thrown,
        State(..state.merge_globals(state, final_state, []), heap: h),
      ))
    Ok(#(YieldCompletion(_, _), _)) | Ok(#(completion.AwaitCompletion(_, _), _)) ->
      panic as "Yield/Await completion should not appear in job execution"
    Error(vm_err) ->
      panic as { "VM error in promise job: " <> string.inspect(vm_err) }
  }
}

// ============================================================================
// Inlined helpers (avoid circular dependency with call.gleam)
// ============================================================================

/// Pad args to exactly `arity` length.
fn pad_args(args: List(JsValue), arity: Int) -> List(JsValue) {
  let len = list.length(args)
  case len >= arity {
    True -> list.take(args, arity)
    False -> list.append(args, list.repeat(JsUndefined, arity - len))
  }
}

/// Resolve `this` for a function call per ES2024 S10.2.1.2 OrdinaryCallBindThis.
fn bind_this(
  state: State,
  callee: FuncTemplate,
  this_arg: JsValue,
) -> #(Heap, JsValue) {
  case callee.is_arrow {
    True -> #(state.heap, state.this_binding)
    False ->
      case callee.is_strict {
        True -> #(state.heap, this_arg)
        False ->
          case this_arg {
            JsUndefined | JsNull -> #(state.heap, JsObject(state.global_object))
            JsObject(_) -> #(state.heap, this_arg)
            _ ->
              case common.to_object(state.heap, state.builtins, this_arg) {
                Some(#(heap, ref)) -> #(heap, JsObject(ref))
                None -> #(state.heap, this_arg)
              }
          }
      }
  }
}
