/// Async generator driver — ES §27.6.
///
/// Unlike sync generators where .next() directly runs the body, async gens
/// enqueue requests and return promises. The `resume_next` driver pulls
/// requests off the queue and settles them:
///   - yield   → resolve head request with {value, done:false}, stay suspended
///   - await   → suspend without settling, resume via microtask, stays Executing
///   - return  → resolve head request with {value, done:true}, complete
///   - throw   → reject head request, complete
///
/// The request queue is the key difference: callers can fire next();next();next()
/// before any settle, and each gets its own promise.
import arc/vm/builtins/common
import arc/vm/builtins/helpers
import arc/vm/builtins/iter_protocol
import arc/vm/builtins/promise as builtins_promise
import arc/vm/completion.{
  type Outcome, Completed, NormalCompletion, Suspended, ThrowCompletion,
}
import arc/vm/exec/generators.{type Drive}
import arc/vm/exec/promises
import arc/vm/heap
import arc/vm/internal/tuple_array
import arc/vm/key.{Named}
import arc/vm/opcode.{AsyncYieldStarNext}
import arc/vm/ops/object as object_ops
import arc/vm/state.{
  type Heap, type HeapSlot, type State, type StepExit, InternalError, State,
  VmFailed,
}
import arc/vm/value.{
  type AGResumeKind, type AsyncGenCompletion, type AsyncGenRequest,
  type DelegateMethod, type JsValue, type Ref, AGAwaitingReturn, AGCompleted,
  AGExecuting, AGNext, AGResumeAwaitingReturn, AGResumeBody, AGResumeDelegate,
  AGResumeDelegateClose, AGResumeReturnUnwind, AGReturn, AGSuspendedStart,
  AGSuspendedYield, AGThrow, AsyncGenRequest, AsyncGeneratorObject,
  AsyncGeneratorSlot, DelegateReturn, DelegateThrow, JsNull, JsObject,
  JsUndefined, ObjectSlot,
}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import gleam/string

/// Bundle of per-request context threaded through the body-execution helpers.
///
/// `frame` is the suspended body snapshot and `req` the head request that was
/// peeled off the queue — the queue itself is deliberately absent, because
/// everything downstream of here runs user code that can enqueue re-entrantly.
/// Slot writes go through `write_live`, which re-reads the queue from the heap.
type Run(host) {
  Run(
    data_ref: Ref,
    frame: AsyncGenFrame,
    req: AsyncGenRequest,
    drive: Drive(host),
  )
}

/// AsyncGenerator.prototype.{next,return,throw} — shared entry point.
/// Per spec §27.6.1.2-4: create a promise capability, validate `this`
/// (reject on failure, don't throw sync), enqueue request, kick driver
/// if not already executing, return promise.
pub fn call_native_method(
  state: State(host),
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
  completion: AsyncGenCompletion,
  drive: Drive(host),
) -> Result(State(host), StepExit(host)) {
  let arg = helpers.first_arg_or_undefined(args)
  let #(h, cap) =
    builtins_promise.new_promise_capability(state.heap, state.builtins)
  let state = State(..state, heap: h)
  let ret = fn(state: State(host)) {
    Ok(
      State(
        ..state,
        stack: [JsObject(cap.promise), ..rest_stack],
        pc: state.pc + 1,
      ),
    )
  }
  case get_async_gen_data(state.heap, this) {
    Error(NotAnAsyncGenerator) -> {
      // Per spec: don't throw synchronously — reject the returned promise.
      let #(err, state) =
        state.type_error_value(
          state,
          "AsyncGenerator method called on incompatible receiver",
        )
      let state = reject_with(state, cap.reject, err)
      ret(state)
    }
    Error(SlotCorrupt) ->
      Error(VmFailed(
        InternalError("call_native_method", "async gen slot missing"),
        state,
      ))
    Ok(#(data_ref, AsyncGenLive(gen_state:, ..))) -> {
      let req =
        AsyncGenRequest(
          completion:,
          value: arg,
          resolve: cap.resolve,
          reject: cap.reject,
        )
      let enqueued = enqueue(state, data_ref, req)
      let stepped = case gen_state {
        AGExecuting | AGAwaitingReturn -> Ok(enqueued)
        _ -> resume_next(enqueued, data_ref, drive)
      }
      case stepped {
        Ok(state) -> ret(state)
        Error(vm_err) -> Error(VmFailed(vm_err, state))
      }
    }
  }
}

/// The core driver loop — ES AsyncGeneratorResumeNext.
/// Pulls the head request and acts on it based on current state.
/// Loops until queue is empty or we hit an await (which suspends via microtask).
///
/// Errors with a `VmError` when the body execution hits an engine bug; the
/// public entry points surface it as `StepVmError`, exactly like every other
/// execution path.
fn resume_next(
  state: State(host),
  data_ref: Ref,
  drive: Drive(host),
) -> Result(State(host), state.VmError) {
  case option.map(read_slot(state.heap, data_ref), normalize_queue) {
    // The data_ref of a live async generator always holds an
    // AsyncGeneratorSlot; anything else is an engine bug. Returning Ok here
    // would leave the generator permanently suspended instead of crashing.
    None -> Error(InternalError("resume_next", "async gen slot missing"))
    // Only `frame` and `req` escape the match — neither the queue nor
    // `gen_state` is in scope for the body-execution helpers below, which run
    // user code (and move `gen_state` on to AGExecuting under them).
    Some(AsyncGenLive(queue_front: [], ..)) -> Ok(state)
    Some(AsyncGenLive(gen_state:, frame:, queue_front: [req, ..], ..)) -> {
      let run = Run(data_ref:, frame:, req:, drive:)
      case gen_state {
        AGExecuting | AGAwaitingReturn -> Ok(state)

        AGCompleted ->
          case req.completion {
            AGNext -> {
              // Resolve {undefined, done:true}, dequeue, loop
              let state = settle_head(state, data_ref)
              let state = fulfill_iter(state, req.resolve, JsUndefined, True)
              resume_next(state, data_ref, drive)
            }
            AGThrow -> {
              let state = settle_head(state, data_ref)
              let state = reject_with(state, req.reject, req.value)
              resume_next(state, data_ref, drive)
            }
            AGReturn -> {
              // Spec: await Promise.resolve(value) first, then settle.
              let state = set_gen_state(state, data_ref, AGAwaitingReturn)
              Ok(setup_await(state, data_ref, req.value, AGResumeAwaitingReturn))
            }
          }

        AGSuspendedStart ->
          case req.completion {
            // return/throw on a never-started gen: complete immediately,
            // then fall through to the Completed logic above.
            AGReturn | AGThrow -> {
              let state = set_gen_state(state, data_ref, AGCompleted)
              resume_next(state, data_ref, drive)
            }
            AGNext -> run_body(state, run, False)
          }

        AGSuspendedYield -> run_body(state, run, True)
      }
    }
  }
}

/// Resume the generator body from a suspended state. Dispatches on the
/// request kind to push the arg / throw / inject return, then runs until
/// the body yields, awaits, returns, or throws.
fn run_body(
  state: State(host),
  run: Run(host),
  push_arg: Bool,
) -> Result(State(host), state.VmError) {
  let Run(data_ref:, frame:, req:, ..) = run
  // Mark executing FIRST so concurrent calls enqueue. saved_pc/stack preserved.
  let state = set_gen_state(state, data_ref, AGExecuting)

  // yield* delegation? Only relevant when resuming from a yield (push_arg=True)
  // with a return/throw request AND saved_pc is AsyncYieldStarNext. This is
  // the one place a request kind narrows to a `DelegateMethod`; everything
  // downstream carries that instead of re-deriving it from `req.completion`.
  let delegated = case push_arg, req.completion {
    True, AGReturn ->
      pair_delegate(DelegateReturn, async_delegate_iterator(frame))
    True, AGThrow ->
      pair_delegate(DelegateThrow, async_delegate_iterator(frame))
    _, _ -> None
  }
  case delegated {
    // The saved slot may be an internal Iterator Record (cached next) —
    // .return/.throw must be looked up and called on the real iterator.
    Some(#(method, iter_ref)) ->
      forward_async_delegate(
        state,
        run,
        method,
        unwrap_record_ref(state.heap, iter_ref),
      )
    None ->
      case req.completion {
        AGNext -> {
          let gen_stack = case push_arg {
            True -> [req.value, ..frame.saved.stack]
            False -> frame.saved.stack
          }
          let exec_state =
            build_exec_state(state, frame, gen_stack, frame.saved.pc)
          handle_exec_result(state, run, run.drive.execute_inner(exec_state))
        }
        AGThrow -> throw_into_gen_body(state, run, req.value)
        AGReturn ->
          // §27.6.3.8 AsyncGeneratorUnwrapYieldResumption: a return completion
          // injected at a suspended yield is Await(resumptionValue) FIRST, so a
          // thenable handed to `.return()` is unwrapped before it becomes the
          // generator's return value. The gen stays AGExecuting and the request
          // stays at the queue head; the AGResumeReturnUnwind branch of
          // `call_native_resume` performs the actual unwind (through enclosing
          // finally blocks / for-of iterator closes) with the AWAITED value —
          // exactly as the delegating path (`delegate_method_missing`) does.
          Ok(setup_await(state, data_ref, req.value, AGResumeReturnUnwind))
      }
  }
}

/// Build a body-execution State from a saved suspended snapshot.
/// Shared by run_body, call_native_resume, and the yield*-delegate paths.
/// Does NOT write AGExecuting — caller is responsible for slot state.
fn build_exec_state(
  state: State(host),
  frame: AsyncGenFrame,
  stack: List(JsValue),
  pc: Int,
) -> State(host) {
  State(
    ..state,
    stack:,
    pc:,
    locals: frame.saved.locals,
    func: frame.func_template,
    code: frame.func_template.bytecode,
    constants: frame.func_template.constants,
    call_stack: [],
    try_stack: frame.saved.try_stack,
    new_target: JsUndefined,
    call_args: [],
    // Per-frame fields: without these the body would inherit the RESUMER's
    // eval_env (losing direct-eval `var`s across a yield) and its line number.
    eval_env: frame.saved.eval_env,
    current_line: frame.saved.line,
  )
}

// ============================================================================
// yield* delegation — ES §15.5.5 step 8.b/8.c
// ============================================================================

/// Tag a delegated-iterator lookup with the method being forwarded to it.
fn pair_delegate(
  method: DelegateMethod,
  iter: Option(Ref),
) -> Option(#(DelegateMethod, Ref)) {
  option.map(iter, fn(iter_ref) { #(method, iter_ref) })
}

/// The `yield*` delegation the body is suspended in, if any: the delegated
/// iterator (top of the saved stack) plus the `after_pc` operand of the
/// `AsyncYieldStarNext` it is parked on — the emitter-resolved label of the
/// instruction the delegation falls out to.
/// saved_pc was a valid dispatch target — always in bounds.
fn async_delegate_site(frame: AsyncGenFrame) -> Option(#(Ref, Int)) {
  case tuple_array.unsafe_get(frame.saved.pc, frame.func_template.bytecode) {
    AsyncYieldStarNext(after_pc:) ->
      case frame.saved.stack {
        [JsObject(iter_ref), ..] -> Some(#(iter_ref, after_pc))
        _ -> None
      }
    _ -> None
  }
}

/// The delegated iterator ref, when the body is suspended mid-`yield*`.
fn async_delegate_iterator(frame: AsyncGenFrame) -> Option(Ref) {
  option.map(async_delegate_site(frame), pair.first)
}

/// Ref-typed adapter over `iter_protocol.unwrap_record_value` — the yield*
/// slot is stored as a Ref and `forward_async_delegate` needs one back.
fn unwrap_record_ref(h: heap.Heap(State(host), host), ref: Ref) -> Ref {
  case iter_protocol.unwrap_record_value(h, JsObject(ref)) {
    JsObject(real) -> real
    _ ->
      panic as "async_generators: Iterator Record [[Iterator]] is not an object (VM invariant)"
  }
}

/// Forward a .return/.throw to the delegated async iterator.
/// Per spec: GetMethod(iter, name) → if missing handle specially; if present
/// call it, then AWAIT the result (unlike sync). The await suspends via
/// setup_await with AGResumeDelegate; settlement is handled in
/// resume_after_delegate.
fn forward_async_delegate(
  state: State(host),
  run: Run(host),
  method: DelegateMethod,
  iter_ref: Ref,
) -> Result(State(host), state.VmError) {
  let iter = JsObject(iter_ref)
  let method_name = case method {
    DelegateThrow -> "throw"
    DelegateReturn -> "return"
  }
  case object_ops.get_value(state, iter_ref, Named(method_name), iter) {
    Error(#(thrown, state)) -> throw_into_gen_body(state, run, thrown)
    Ok(#(JsUndefined, state)) | Ok(#(JsNull, state)) ->
      delegate_method_missing(state, run, method, iter_ref)
    Ok(#(method_fn, state)) ->
      case state.call(state, method_fn, iter, [run.req.value]) {
        Error(#(thrown, state)) -> throw_into_gen_body(state, run, thrown)
        Ok(#(result, state)) ->
          // Await the result. Gen stays AGExecuting, req stays at queue head,
          // saved_pc/stack unchanged. Resume via AGResumeDelegate.
          Ok(setup_await(state, run.data_ref, result, AGResumeDelegate(method)))
      }
  }
}

/// Inner iterator lacks .return/.throw. Per ES §15.5.5:
///   - missing throw → AsyncIteratorClose(iter), throw TypeError into body
///   - missing return → return completion propagates out of the yield*
///     (through the outer generator's finally blocks)
fn delegate_method_missing(
  state: State(host),
  run: Run(host),
  method: DelegateMethod,
  iter_ref: Ref,
) -> Result(State(host), state.VmError) {
  case method {
    DelegateThrow ->
      // §7.b.iii.1-4: AsyncIteratorClose(iter, NormalCompletion(empty)),
      // then throw TypeError into body. closeCompletion is NORMAL here, so
      // per AsyncIteratorClose steps 4-6 an error from GetMethod(iter,
      // "return") / Call(return) / the awaited close result PROPAGATES as
      // the yield* completion, replacing the missing-throw TypeError.
      case close_async_iterator(state, iter_ref) {
        Ok(#(state, Some(close_result))) ->
          // Await the close. Gen stays AGExecuting; settlement is handled
          // in the AGResumeDelegateClose branch of call_native_resume.
          Ok(setup_await(
            state,
            run.data_ref,
            close_result,
            AGResumeDelegateClose,
          ))
        Ok(#(state, None)) ->
          // No .return on iter — skip the await, straight to the TypeError.
          throw_missing_throw_type_error(state, run)
        Error(#(thrown, state)) ->
          // GetMethod/Call threw — propagate into the body (steps 4 & 6).
          throw_into_gen_body(state, run, thrown)
      }
    DelegateReturn ->
      // §7.c.ii.2-3: no .return → Await(received.[[Value]]) first, THEN the
      // return completion propagates out of the yield*, unwinding the OUTER
      // generator's enclosing finally blocks / iterator closes (shared with
      // the sync driver). Gen stays AGExecuting, req stays at queue head;
      // settlement is handled in the AGResumeReturnUnwind branch of
      // call_native_resume.
      Ok(setup_await(state, run.data_ref, run.req.value, AGResumeReturnUnwind))
  }
}

/// Throw a TypeError("does not provide a 'throw' method") into the generator
/// body. Shared by delegate_method_missing's no-.return-on-iter fast path and
/// the AGResumeDelegateClose settlement.
fn throw_missing_throw_type_error(
  state: State(host),
  run: Run(host),
) -> Result(State(host), state.VmError) {
  let #(err, state) =
    state.type_error_value(
      state,
      "The iterator does not provide a 'throw' method.",
    )
  throw_into_gen_body(state, run, err)
}

/// Throw a value into the generator body at saved_pc (through its try/catch),
/// then dispatch via handle_exec_result.
fn throw_into_gen_body(
  state: State(host),
  run: Run(host),
  thrown: JsValue,
) -> Result(State(host), state.VmError) {
  let exec_state =
    build_exec_state(
      state,
      run.frame,
      run.frame.saved.stack,
      run.frame.saved.pc,
    )
  let exec_result = case run.drive.unwind_to_catch(exec_state, thrown) {
    Some(caught) -> run.drive.execute_inner(caught)
    None -> Ok(#(Completed(ThrowCompletion(thrown)), exec_state))
  }
  handle_exec_result(state, run, exec_result)
}

/// Inject a RETURN completion carrying `value` at the generator's saved
/// suspension point (§27.5.3.4): unwind the enclosing finally blocks /
/// iterator closes via the shared unwinder, then dispatch through
/// handle_exec_result. Counterpart to `throw_into_gen_body`.
fn unwind_return_into_body(
  state: State(host),
  run: Run(host),
  value: JsValue,
) -> Result(State(host), state.VmError) {
  let exec_state =
    build_exec_state(
      state,
      run.frame,
      run.frame.saved.stack,
      run.frame.saved.pc,
    )
  handle_exec_result(
    state,
    run,
    generators.unwind_return(exec_state, value, run.drive),
  )
}

/// AsyncIteratorClose steps 1-4 with a NORMAL closeCompletion (the yield*
/// missing-`throw` path): GetMethod(iter, "return"); if present, call it with
/// no args. Ok(Some(result)) → caller must Await it (steps 5-7 happen at
/// settlement). Ok(None) → no .return present. Error → GetMethod/Call threw;
/// per steps 4 & 6 this propagates as the yield* completion (closeCompletion
/// is normal, so nothing takes precedence over innerResult).
fn close_async_iterator(
  state: State(host),
  iter_ref: Ref,
) -> Result(#(State(host), Option(JsValue)), #(JsValue, State(host))) {
  let iter = JsObject(iter_ref)
  use #(ret_fn, state) <- result.try(object_ops.get_value(
    state,
    iter_ref,
    Named("return"),
    iter,
  ))
  case ret_fn {
    JsUndefined | JsNull -> Ok(#(state, None))
    _ -> {
      use #(close_result, state) <- result.map(
        state.call(state, ret_fn, iter, []),
      )
      #(state, Some(close_result))
    }
  }
}

/// Handle the awaited result of a delegated iter.return()/iter.throw() call.
/// Called from call_native_resume's AGResumeDelegate branch.
fn resume_after_delegate(
  state: State(host),
  run: Run(host),
  method: DelegateMethod,
  is_reject: Bool,
  settled: JsValue,
) -> Result(State(host), state.VmError) {
  // Awaited promise rejected → throw into body.
  case is_reject {
    True -> throw_into_gen_body(state, run, settled)
    False ->
      case iter_protocol.read_iter_result(state, settled) {
        Error(#(thrown, state)) -> throw_into_gen_body(state, run, thrown)
        Ok(#(False, val, state)) -> {
          // Still delegating — yield val out, stay suspended at SAME
          // saved_pc/stack (AsyncYieldStarNext, [iter,..]).
          // Live re-read matters here: read_iter_result just ran user
          // getters that may have enqueued more requests.
          let state =
            write_live(state, run.data_ref, fn(live) {
              live |> drop_head |> with_state(AGSuspendedYield)
            })
          let state = fulfill_iter(state, run.req.resolve, val, False)
          resume_next(state, run.data_ref, run.drive)
        }
        Ok(#(True, val, state)) -> delegate_done(state, run, method, val)
      }
  }
}

/// Delegated iterator returned {done:true} from a forwarded return/throw.
///   throw → inner caught the throw and finished — yield* expr evaluates to
///           val; resume the body past the whole yield* sequence.
///   return → inner returned done — outer gen returns val.
fn delegate_done(
  state: State(host),
  run: Run(host),
  method: DelegateMethod,
  val: JsValue,
) -> Result(State(host), state.VmError) {
  let frame = run.frame
  case method {
    DelegateThrow ->
      // The resume target is `AsyncYieldStarNext`'s `after_pc` operand — a
      // label the emitter placed after the sequence, so nothing here depends
      // on how many opcodes yield* lowers to.
      case async_delegate_site(frame) {
        None ->
          Error(InternalError(
            "delegate_done",
            "not suspended at AsyncYieldStarNext",
          ))
        Some(#(_iter_ref, after_pc)) -> {
          let stack_after = case frame.saved.stack {
            [_iter, ..rest] -> [val, ..rest]
            [] -> [val]
          }
          let exec_state = build_exec_state(state, frame, stack_after, after_pc)
          handle_exec_result(state, run, run.drive.execute_inner(exec_state))
        }
      }
    DelegateReturn ->
      // §27.5.3.8 step 7.c.viii: the inner iterator's return() reported
      // done — a return completion (carrying its value) propagates out of
      // the yield*, so unwind the outer generator's enclosing finally
      // blocks / iterator closes (shared with the sync driver).
      unwind_return_into_body(state, run, val)
  }
}

/// Dispatch on the body's completion: yield/await/return/throw.
///
/// A `VmError` from the body is an engine bug, not a guest throw — it is
/// propagated to the caller unchanged (never converted into a rejection the
/// guest could observe).
fn handle_exec_result(
  outer: State(host),
  run: Run(host),
  result: Result(#(Outcome, State(host)), state.VmError),
) -> Result(State(host), state.VmError) {
  let Run(data_ref:, req:, drive:, ..) = run
  case result {
    Ok(#(Suspended(completion.Yield, value), suspended)) -> {
      // Body yielded — save suspended state, dequeue + resolve request, loop.
      let state = state.adopt_child(outer, suspended)
      let state = save_suspended(state, data_ref, suspended, AGSuspendedYield)
      let state = settle_head(state, data_ref)
      let state = fulfill_iter(state, req.resolve, value, False)
      resume_next(state, data_ref, drive)
    }
    Ok(#(Suspended(completion.Await, value), suspended)) -> {
      // Body hit await — save state (still Executing), set up promise callback.
      // Do NOT dequeue — the same request stays at head until a yield/return/throw.
      let state = state.adopt_child(outer, suspended)
      let state = save_suspended(state, data_ref, suspended, AGExecuting)
      Ok(setup_await(state, data_ref, value, AGResumeBody))
    }
    Ok(#(Completed(NormalCompletion(value)), final_state)) -> {
      let state = state.adopt_child(outer, final_state)
      let state = complete(state, data_ref)
      let state = fulfill_iter(state, req.resolve, value, True)
      resume_next(state, data_ref, drive)
    }
    Ok(#(Completed(ThrowCompletion(thrown)), final_state)) -> {
      let state = state.adopt_child(outer, final_state)
      let state = complete(state, data_ref)
      let state = reject_with(state, req.reject, thrown)
      resume_next(state, data_ref, drive)
    }
    Error(vm_err) -> Error(vm_err)
  }
}

/// AsyncGeneratorResume — called when an internal await's promise settles.
/// `kind` distinguishes: body await, AwaitingReturn microtask, or a yield*
/// delegated iter.return()/throw() result.
pub fn call_native_resume(
  state: State(host),
  data_ref: Ref,
  is_reject: Bool,
  kind: AGResumeKind,
  args: List(JsValue),
  rest_stack: List(JsValue),
  drive: Drive(host),
) -> Result(State(host), StepExit(host)) {
  let settled = helpers.first_arg_or_undefined(args)
  let ret = fn(state: State(host)) {
    Ok(State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1))
  }
  case option.map(read_slot(state.heap, data_ref), normalize_queue) {
    None ->
      Error(VmFailed(
        InternalError("call_native_resume", "async gen slot missing"),
        state,
      ))
    // Only `frame` and `req` escape the match — see resume_next.
    Some(AsyncGenLive(queue_front: [], ..)) -> ret(state)
    Some(AsyncGenLive(frame:, queue_front: [req, ..], ..)) -> {
      let run = Run(data_ref:, frame:, req:, drive:)
      let stepped = case kind {
        AGResumeAwaitingReturn -> {
          // AwaitingReturn callback: settle the head return request.
          let state = complete(state, data_ref)
          let state = case is_reject {
            False -> fulfill_iter(state, req.resolve, settled, True)
            True -> reject_with(state, req.reject, settled)
          }
          resume_next(state, data_ref, drive)
        }
        AGResumeBody ->
          // Body await resumed — push settled value and run, or throw it in.
          case is_reject {
            True -> throw_into_gen_body(state, run, settled)
            False -> {
              let stack = [settled, ..frame.saved.stack]
              let es = build_exec_state(state, frame, stack, frame.saved.pc)
              handle_exec_result(state, run, drive.execute_inner(es))
            }
          }
        AGResumeDelegate(method) ->
          resume_after_delegate(state, run, method, is_reject, settled)
        AGResumeDelegateClose ->
          // AsyncIteratorClose await settled (closeCompletion was
          // normal): rejection propagates (step 6), non-object fulfilment
          // is its own TypeError (step 7), otherwise fall through to the
          // yield* missing-throw TypeError.
          case is_reject, settled {
            True, _ -> throw_into_gen_body(state, run, settled)
            False, JsObject(_) -> throw_missing_throw_type_error(state, run)
            False, _non_obj -> {
              let #(err, state) =
                state.type_error_value(
                  state,
                  "Iterator result is not an object",
                )
              throw_into_gen_body(state, run, err)
            }
          }
        AGResumeReturnUnwind ->
          // yield*-with-no-.return: Await(received.[[Value]]) settled
          // (§27.5.3.8 step 7.c.ii.2). Fulfil → unwind the outer
          // generator's finallys with the AWAITED value; reject → the
          // abrupt Await replaces the return completion and is thrown
          // into the body.
          case is_reject {
            True -> throw_into_gen_body(state, run, settled)
            False -> unwind_return_into_body(state, run, settled)
          }
      }
      case stepped {
        Ok(state) -> ret(state)
        Error(vm_err) -> Error(VmFailed(vm_err, state))
      }
    }
  }
}

// ============================================================================
// Await wiring — shared scaffold in promises.setup_await, with
// AsyncGeneratorResume callbacks instead of AsyncResume.
// ============================================================================

fn setup_await(
  state: State(host),
  data_ref: Ref,
  awaited: JsValue,
  kind: AGResumeKind,
) -> State(host) {
  use is_reject <- promises.setup_await(state, awaited)
  value.AsyncGeneratorResume(data_ref:, is_reject:, kind:)
}

// ============================================================================
// Slot read/write helpers
// ============================================================================

/// The suspended body of an async generator: everything needed to rebuild an
/// execution State, and nothing else. IMMUTABLE for the duration of a driver
/// step — carries neither the queue nor `gen_state`, both of which the slot
/// mutates while the body runs. A frame is handed to the body-execution
/// helpers, which run user code and therefore must never see (let alone write
/// back) a snapshot of mutable slot state captured before that ran.
type AsyncGenFrame {
  AsyncGenFrame(
    func_template: value.FuncTemplate,
    env_ref: Ref,
    /// The suspended body snapshot — the same record the heap slot stores.
    saved: value.SuspendedFrame,
  )
}

/// A whole decoded AsyncGeneratorSlot: the mutable slot state (`gen_state`,
/// the request queue) plus the immutable body snapshot. Only ever produced by
/// `read_slot` and consumed by `encode_slot`, so the mutable half exists
/// exactly where a live slot is being read or written — the drivers peel the
/// head request off and thereafter hold only a frame, so a body-executing path
/// CANNOT read a stale `gen_state` (e.g. the pre-`AGExecuting` value the step
/// started from): the field is not reachable from what it holds.
type AsyncGenLive {
  AsyncGenLive(
    /// [[AsyncGeneratorState]] — mutated by the driver as the body runs.
    gen_state: value.AsyncGeneratorState,
    frame: AsyncGenFrame,
    /// Decoded two-list FIFO — see AsyncGeneratorSlot.queue in value.gleam.
    queue_front: List(AsyncGenRequest),
    queue_back: List(AsyncGenRequest),
  )
}

/// Why a `this` value did not yield an async-generator slot.
type SlotLookupError {
  /// `this` is not an async generator object — a guest TypeError.
  NotAnAsyncGenerator
  /// It IS one, but its data_ref does not hold an AsyncGeneratorSlot — an
  /// engine bug, never a guest-observable condition.
  SlotCorrupt
}

fn get_async_gen_data(
  h: Heap(host),
  this: JsValue,
) -> Result(#(Ref, AsyncGenLive), SlotLookupError) {
  case this {
    JsObject(ref) ->
      case heap.read(h, ref) {
        Some(ObjectSlot(kind: AsyncGeneratorObject(generator_data: dr), ..)) ->
          case read_slot(h, dr) {
            Some(live) -> Ok(#(dr, live))
            None -> Error(SlotCorrupt)
          }
        _ -> Error(NotAnAsyncGenerator)
      }
    _ -> Error(NotAnAsyncGenerator)
  }
}

fn read_slot(h: Heap(host), data_ref: Ref) -> Option(AsyncGenLive) {
  case heap.read(h, data_ref) {
    Some(AsyncGeneratorSlot(
      gen_state:,
      queue:,
      func_template:,
      env_ref:,
      frame:,
    )) -> {
      let #(queue_front, queue_back) = queue
      Some(AsyncGenLive(
        gen_state:,
        frame: AsyncGenFrame(func_template:, env_ref:, saved: frame),
        queue_front:,
        queue_back:,
      ))
    }
    _ -> None
  }
}

/// If front is empty, reverse back into front. Called at dequeue sites so the
/// head pattern-match `[req, ..rest]` always sees the oldest request.
fn normalize_queue(live: AsyncGenLive) -> AsyncGenLive {
  case live.queue_front, live.queue_back {
    [], [_, ..] ->
      AsyncGenLive(
        ..live,
        queue_front: list.reverse(live.queue_back),
        queue_back: [],
      )
    _, _ -> live
  }
}

/// Encode a decoded AsyncGenLive back into its heap slot — the exact inverse
/// of `read_slot`.
fn encode_slot(live: AsyncGenLive) -> HeapSlot(host) {
  let AsyncGenLive(gen_state:, frame:, queue_front:, queue_back:) = live
  AsyncGeneratorSlot(
    gen_state:,
    queue: #(queue_front, queue_back),
    func_template: frame.func_template,
    env_ref: frame.env_ref,
    frame: frame.saved,
  )
}

// ---------------------------------------------------------------------------
// The ONLY slot-write path. Everything below funnels through `write_live`.
// ---------------------------------------------------------------------------

/// Re-read the LIVE slot at write time, apply a pure update, write it back.
///
/// User code running inside the generator body (or a getter run while settling
/// an iterator result) can call .next()/.return()/.throw() re-entrantly, which
/// enqueues onto the live slot's queue_back. Re-reading here is what stops a
/// stale queue from being written back over it — and since the drivers only
/// ever hold an `AsyncGenFrame` once user code is in play, they have no queue
/// to write back even if they wanted to.
///
/// The slot must exist: every caller is inside a driver step that just read
/// it, and nothing deallocates AsyncGeneratorSlots mid-step.
fn write_live(
  state: State(host),
  data_ref: Ref,
  update: fn(AsyncGenLive) -> AsyncGenLive,
) -> State(host) {
  let assert Some(live) = read_slot(state.heap, data_ref)
    as "async gen slot vanished between read and write"
  let h = heap.write(state.heap, data_ref, encode_slot(update(live)))
  State(..state, heap: h)
}

/// Enqueue a request: O(1) prepend to the (live) back list.
fn enqueue(
  state: State(host),
  data_ref: Ref,
  req: AsyncGenRequest,
) -> State(host) {
  use live <- write_live(state, data_ref)
  AsyncGenLive(..live, queue_back: [req, ..live.queue_back])
}

/// Transition gen_state only; queue and saved frame untouched.
fn set_gen_state(
  state: State(host),
  data_ref: Ref,
  s: value.AsyncGeneratorState,
) -> State(host) {
  use live <- write_live(state, data_ref)
  with_state(live, s)
}

/// Dequeue the settled head request; gen_state and saved frame untouched.
fn settle_head(state: State(host), data_ref: Ref) -> State(host) {
  write_live(state, data_ref, drop_head)
}

/// Dequeue the settled head request and mark the generator completed.
fn complete(state: State(host), data_ref: Ref) -> State(host) {
  use live <- write_live(state, data_ref)
  live |> drop_head |> with_state(AGCompleted)
}

/// Record the body's suspension point (pc/locals/stack/try) and transition to
/// `new_state`. Does NOT touch the queue — yield paths follow up with
/// `settle_head`; await paths keep the head request queued.
fn save_suspended(
  state: State(host),
  data_ref: Ref,
  suspended: State(host),
  new_state: value.AsyncGeneratorState,
) -> State(host) {
  use live <- write_live(state, data_ref)
  AsyncGenLive(
    ..live,
    gen_state: new_state,
    frame: AsyncGenFrame(
      ..live.frame,
      saved: generators.suspended_frame(suspended),
    ),
  )
}

// -- pure AsyncGenLive updaters, composed inside `write_live` callbacks ------

fn with_state(
  live: AsyncGenLive,
  s: value.AsyncGeneratorState,
) -> AsyncGenLive {
  AsyncGenLive(..live, gen_state: s)
}

/// Drop the head (oldest) request — the one the caller just settled. Any
/// requests enqueued re-entrantly after it are preserved.
fn drop_head(live: AsyncGenLive) -> AsyncGenLive {
  let live = normalize_queue(live)
  case live.queue_front {
    [_head, ..rest] -> AsyncGenLive(..live, queue_front: rest)
    [] -> live
  }
}

// ============================================================================
// Promise helpers
// ============================================================================

/// Call resolve({value, done}) via state.call.
fn fulfill_iter(
  state: State(host),
  resolve: JsValue,
  val: JsValue,
  done: Bool,
) -> State(host) {
  let #(h, result) =
    common.create_iter_result(state.heap, state.builtins, val, done)
  call_fn(State(..state, heap: h), resolve, [result])
}

fn reject_with(
  state: State(host),
  reject: JsValue,
  reason: JsValue,
) -> State(host) {
  call_fn(state, reject, [reason])
}

fn call_fn(state: State(host), f: JsValue, args: List(JsValue)) -> State(host) {
  case state.call(state, f, JsUndefined, args) {
    Ok(#(_, state)) -> state
    // f is always a resolve/reject fn from CreateResolvingFunctions.
    // Per spec §27.2.1.3 they no-op on double-call via [[AlreadyResolved]],
    // never throw. If this fires, the VM is broken.
    Error(#(thrown, _)) ->
      panic as {
        "async gen resolve/reject threw (VM bug): " <> string.inspect(thrown)
      }
  }
}
