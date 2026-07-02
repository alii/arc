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
import arc/vm/builtins/promise as builtins_promise
import arc/vm/completion.{
  type Completion, AwaitCompletion, NormalCompletion, ThrowCompletion,
  YieldCompletion,
}
import arc/vm/exec/generators
import arc/vm/exec/promises
import arc/vm/heap
import arc/vm/internal/tuple_array
import arc/vm/opcode.{AsyncYieldStarNext}
import arc/vm/ops/object as object_ops
import arc/vm/state.{
  type Heap, type HeapSlot, type State, type StepResult, State, StepVmError,
  Unimplemented,
}
import arc/vm/value.{
  type AGResumeKind, type AsyncGenCompletion, type AsyncGenRequest, type JsValue,
  type Ref, AGAwaitingReturn, AGCompleted, AGExecuting, AGNext,
  AGResumeAwaitingReturn, AGResumeBody, AGResumeDelegate, AGResumeDelegateClose,
  AGResumeReturnUnwind, AGReturn, AGSuspendedStart, AGSuspendedYield, AGThrow,
  AsyncGenRequest, AsyncGeneratorObject, AsyncGeneratorSlot, JsNull, JsObject,
  JsUndefined, Named, ObjectSlot,
}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub type ExecuteInnerFn(host) =
  fn(State(host)) -> Result(#(Completion, State(host)), state.VmError)

pub type UnwindToCatchFn(host) =
  fn(State(host), JsValue) -> Option(State(host))

/// Bundle of per-request context threaded through the body-execution helpers.
///
/// `gen` is a READ-ONLY snapshot taken before user code runs: it is safe to
/// read saved_pc / saved_stack / func_template from it, but its queue fields
/// must never be written back — user code running inside the body can enqueue
/// more requests re-entrantly. All slot writes go through `write_live`, which
/// re-reads the heap at write time. That is why none of the writers below
/// accept a queue.
type Run(host) {
  Run(
    data_ref: Ref,
    gen: AsyncGenData,
    req: AsyncGenRequest,
    execute_inner: ExecuteInnerFn(host),
    unwind_to_catch: UnwindToCatchFn(host),
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
  execute_inner: ExecuteInnerFn(host),
  unwind_to_catch: UnwindToCatchFn(host),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
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
    None -> {
      // Per spec: don't throw synchronously — reject the returned promise.
      let #(err, state) =
        state.type_error_value(
          state,
          "AsyncGenerator method called on incompatible receiver",
        )
      let state = reject_with(state, cap.reject, err)
      ret(state)
    }
    Some(gen) -> {
      let req =
        AsyncGenRequest(
          completion:,
          value: arg,
          resolve: cap.resolve,
          reject: cap.reject,
        )
      let state = enqueue(state, gen.data_ref, req)
      let state = case gen.gen_state {
        AGExecuting | AGAwaitingReturn -> state
        _ -> resume_next(state, gen.data_ref, execute_inner, unwind_to_catch)
      }
      ret(state)
    }
  }
}

/// The core driver loop — ES AsyncGeneratorResumeNext.
/// Pulls the head request and acts on it based on current state.
/// Loops until queue is empty or we hit an await (which suspends via microtask).
fn resume_next(
  state: State(host),
  data_ref: Ref,
  execute_inner: ExecuteInnerFn(host),
  unwind_to_catch: UnwindToCatchFn(host),
) -> State(host) {
  case read_slot(state.heap, data_ref) {
    None -> state
    Some(gen) -> {
      let gen = normalize_queue(gen)
      case gen.queue_front {
        [] -> state
        [req, ..] -> {
          let run = Run(data_ref:, gen:, req:, execute_inner:, unwind_to_catch:)
          case gen.gen_state {
            AGExecuting | AGAwaitingReturn -> state

            AGCompleted ->
              case req.completion {
                AGNext -> {
                  // Resolve {undefined, done:true}, dequeue, loop
                  let state = settle_head(state, data_ref)
                  let state =
                    fulfill_iter(state, req.resolve, JsUndefined, True)
                  resume_next(state, data_ref, execute_inner, unwind_to_catch)
                }
                AGThrow -> {
                  let state = settle_head(state, data_ref)
                  let state = reject_with(state, req.reject, req.value)
                  resume_next(state, data_ref, execute_inner, unwind_to_catch)
                }
                AGReturn -> {
                  // Spec: await Promise.resolve(value) first, then settle.
                  let state = set_gen_state(state, data_ref, AGAwaitingReturn)
                  setup_await(
                    state,
                    data_ref,
                    req.value,
                    AGResumeAwaitingReturn,
                  )
                }
              }

            AGSuspendedStart ->
              case req.completion {
                // return/throw on a never-started gen: complete immediately,
                // then fall through to the Completed logic above.
                AGReturn | AGThrow -> {
                  let state = set_gen_state(state, data_ref, AGCompleted)
                  resume_next(state, data_ref, execute_inner, unwind_to_catch)
                }
                AGNext -> run_body(state, run, False)
              }

            AGSuspendedYield -> run_body(state, run, True)
          }
        }
      }
    }
  }
}

/// Resume the generator body from a suspended state. Dispatches on the
/// request kind to push the arg / throw / inject return, then runs until
/// the body yields, awaits, returns, or throws.
fn run_body(state: State(host), run: Run(host), push_arg: Bool) -> State(host) {
  let Run(data_ref:, gen:, req:, ..) = run
  // Mark executing FIRST so concurrent calls enqueue. saved_pc/stack preserved.
  let state = set_gen_state(state, data_ref, AGExecuting)

  // yield* delegation? Only relevant when resuming from a yield (push_arg=True)
  // with a return/throw request AND saved_pc is AsyncYieldStarNext.
  let delegated = case push_arg, req.completion {
    True, AGReturn | True, AGThrow -> async_delegate_iterator(gen)
    _, _ -> None
  }
  case delegated {
    // The saved slot may be an internal Iterator Record (cached next) —
    // .return/.throw must be looked up and called on the real iterator.
    Some(iter_ref) ->
      forward_async_delegate(
        state,
        run,
        unwrap_record_ref(state.heap, iter_ref),
      )
    None -> {
      let gen_stack = case push_arg, req.completion {
        True, AGNext -> [req.value, ..gen.saved_stack]
        _, _ -> gen.saved_stack
      }
      let exec_state = build_exec_state(state, gen, gen_stack, gen.saved_pc)
      let exec_result = case req.completion {
        AGNext -> run.execute_inner(exec_state)
        AGThrow ->
          case run.unwind_to_catch(exec_state, req.value) {
            Some(caught) -> run.execute_inner(caught)
            None -> Ok(#(ThrowCompletion(req.value), exec_state))
          }
        AGReturn ->
          // Inject a return completion at the suspended yield (§27.6.3.8):
          // unwind through enclosing finally blocks / for-of iterator closes
          // before completing. Shared with the sync driver
          // (generators.unwind_return) so the two flavours cannot diverge.
          // NOTE: the spec's Await of the resumption value (§27.6.3.8
          // AsyncGeneratorUnwrapYieldResumption) is not performed on this
          // NON-delegating path, so a thenable passed to .return() is not
          // unwrapped here (test262 return-suspendedYield-promise.js). The
          // delegating path (delegate_method_missing) does await it.
          generators.unwind_return(
            exec_state,
            req.value,
            run.execute_inner,
            run.unwind_to_catch,
          )
      }
      handle_exec_result(state, run, exec_result)
    }
  }
}

/// Build a body-execution State from a saved suspended snapshot.
/// Shared by run_body, call_native_resume, and the yield*-delegate paths.
/// Does NOT write AGExecuting — caller is responsible for slot state.
fn build_exec_state(
  state: State(host),
  gen: AsyncGenData,
  stack: List(JsValue),
  pc: Int,
) -> State(host) {
  let restored_try = generators.restore_stacks(gen.saved_try_stack)
  State(
    ..state,
    stack:,
    pc:,
    locals: gen.saved_locals,
    func: gen.func_template,
    code: gen.func_template.bytecode,
    constants: gen.func_template.constants,
    call_stack: [],
    try_stack: restored_try,
    new_target: JsUndefined,
    call_args: [],
  )
}

// ============================================================================
// yield* delegation — ES §15.5.5 step 8.b/8.c
// ============================================================================

/// If suspended at AsyncYieldStarNext, return the delegated iterator ref.
/// saved_pc was a valid dispatch target — always in bounds.
fn async_delegate_iterator(gen: AsyncGenData) -> Option(Ref) {
  case tuple_array.unsafe_get(gen.saved_pc, gen.func_template.bytecode) {
    AsyncYieldStarNext ->
      case gen.saved_stack {
        [JsObject(iter_ref), ..] -> Some(iter_ref)
        _ -> None
      }
    _ -> None
  }
}

/// The yield* iter slot may hold an internal Iterator Record wrapper
/// (cached `next` from GetIteratorFromMethod) — resolve it to the real
/// iterator for .return/.throw lookups.
fn unwrap_record_ref(h: heap.Heap(State(host), host), ref: Ref) -> Ref {
  case heap.read(h, ref) {
    Some(ObjectSlot(
      kind: value.IteratorRecordObject(iterated: JsObject(real), ..),
      ..,
    )) -> real
    _ -> ref
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
  iter_ref: Ref,
) -> State(host) {
  let iter = JsObject(iter_ref)
  let method_name = case run.req.completion {
    AGThrow -> "throw"
    // AGReturn (AGNext never reaches here by construction)
    _ -> "return"
  }
  case object_ops.get_value(state, iter_ref, Named(method_name), iter) {
    Error(#(thrown, state)) -> throw_into_gen_body(state, run, thrown)
    Ok(#(JsUndefined, state)) | Ok(#(JsNull, state)) ->
      delegate_method_missing(state, run, iter_ref)
    Ok(#(method_fn, state)) ->
      case state.call(state, method_fn, iter, [run.req.value]) {
        Error(#(thrown, state)) -> throw_into_gen_body(state, run, thrown)
        Ok(#(result, state)) ->
          // Await the result. Gen stays AGExecuting, req stays at queue head,
          // saved_pc/stack unchanged. Resume via AGResumeDelegate.
          setup_await(
            state,
            run.data_ref,
            result,
            AGResumeDelegate(run.req.completion),
          )
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
  iter_ref: Ref,
) -> State(host) {
  case run.req.completion {
    AGThrow ->
      // §7.b.iii.1-4: AsyncIteratorClose(iter, NormalCompletion(empty)),
      // then throw TypeError into body. closeCompletion is NORMAL here, so
      // per AsyncIteratorClose steps 4-6 an error from GetMethod(iter,
      // "return") / Call(return) / the awaited close result PROPAGATES as
      // the yield* completion, replacing the missing-throw TypeError.
      case close_async_iterator(state, iter_ref) {
        Ok(#(state, Some(close_result))) ->
          // Await the close. Gen stays AGExecuting; settlement is handled
          // in the AGResumeDelegateClose branch of call_native_resume.
          setup_await(state, run.data_ref, close_result, AGResumeDelegateClose)
        Ok(#(state, None)) ->
          // No .return on iter — skip the await, straight to the TypeError.
          throw_missing_throw_type_error(state, run)
        Error(#(thrown, state)) ->
          // GetMethod/Call threw — propagate into the body (steps 4 & 6).
          throw_into_gen_body(state, run, thrown)
      }
    AGReturn | AGNext ->
      // §7.c.ii.2-3: no .return → Await(received.[[Value]]) first, THEN the
      // return completion propagates out of the yield*, unwinding the OUTER
      // generator's enclosing finally blocks / iterator closes (shared with
      // the sync driver). Gen stays AGExecuting, req stays at queue head;
      // settlement is handled in the AGResumeReturnUnwind branch of
      // call_native_resume.
      setup_await(state, run.data_ref, run.req.value, AGResumeReturnUnwind)
  }
}

/// Throw a TypeError("does not provide a 'throw' method") into the generator
/// body. Shared by delegate_method_missing's no-.return-on-iter fast path and
/// the AGResumeDelegateClose settlement.
fn throw_missing_throw_type_error(
  state: State(host),
  run: Run(host),
) -> State(host) {
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
) -> State(host) {
  let exec_state =
    build_exec_state(state, run.gen, run.gen.saved_stack, run.gen.saved_pc)
  let exec_result = case run.unwind_to_catch(exec_state, thrown) {
    Some(caught) -> run.execute_inner(caught)
    None -> Ok(#(ThrowCompletion(thrown), exec_state))
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
) -> State(host) {
  let exec_state =
    build_exec_state(state, run.gen, run.gen.saved_stack, run.gen.saved_pc)
  handle_exec_result(
    state,
    run,
    generators.unwind_return(
      exec_state,
      value,
      run.execute_inner,
      run.unwind_to_catch,
    ),
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

/// Read {done, value} off an awaited iterator-result object. Returns
/// Error(#(thrown, state)) for non-object results or property-get throws.
fn read_iter_result(
  state: State(host),
  settled: JsValue,
) -> Result(#(Bool, JsValue, State(host)), #(JsValue, State(host))) {
  case settled {
    JsObject(rref) -> {
      use #(done_v, state) <- result.try(object_ops.get_value(
        state,
        rref,
        Named("done"),
        settled,
      ))
      use #(val, state) <- result.map(object_ops.get_value(
        state,
        rref,
        Named("value"),
        settled,
      ))
      #(value.is_truthy(done_v), val, state)
    }
    _non_obj ->
      Error(state.type_error_value(state, "Iterator result is not an object"))
  }
}

/// Handle the awaited result of a delegated iter.return()/iter.throw() call.
/// Called from call_native_resume's AGResumeDelegate branch.
fn resume_after_delegate(
  state: State(host),
  run: Run(host),
  method: AsyncGenCompletion,
  is_reject: Bool,
  settled: JsValue,
) -> State(host) {
  // Awaited promise rejected → throw into body.
  case is_reject {
    True -> throw_into_gen_body(state, run, settled)
    False ->
      case read_iter_result(state, settled) {
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
          resume_next(
            state,
            run.data_ref,
            run.execute_inner,
            run.unwind_to_catch,
          )
        }
        Ok(#(True, val, state)) -> delegate_done(state, run, method, val)
      }
  }
}

/// Delegated iterator returned {done:true} from a forwarded return/throw.
///   throw → inner caught the throw and finished — yield* expr evaluates to
///           val; resume body at saved_pc+3 (past Next/Await/Resume).
///   return → inner returned done — outer gen returns val.
fn delegate_done(
  state: State(host),
  run: Run(host),
  method: AsyncGenCompletion,
  val: JsValue,
) -> State(host) {
  let gen = run.gen
  case method {
    AGThrow -> {
      // Bytecode layout (emit.gleam): N=AsyncYieldStarNext, N+1=Await,
      // N+2=AsyncYieldStarResume, N+3=<after yield*>. saved_pc==N.
      let stack_after = case gen.saved_stack {
        [_iter, ..rest] -> [val, ..rest]
        [] -> [val]
      }
      let exec_state =
        build_exec_state(state, gen, stack_after, gen.saved_pc + 3)
      handle_exec_result(state, run, run.execute_inner(exec_state))
    }
    AGReturn | AGNext ->
      // §27.5.3.8 step 7.c.viii: the inner iterator's return() reported
      // done — a return completion (carrying its value) propagates out of
      // the yield*, so unwind the outer generator's enclosing finally
      // blocks / iterator closes (shared with the sync driver).
      unwind_return_into_body(state, run, val)
  }
}

/// Dispatch on the body's completion: yield/await/return/throw.
fn handle_exec_result(
  outer: State(host),
  run: Run(host),
  result: Result(#(Completion, State(host)), state.VmError),
) -> State(host) {
  let Run(data_ref:, req:, execute_inner:, unwind_to_catch:, ..) = run
  case result {
    Ok(#(YieldCompletion(value), suspended)) -> {
      // Body yielded — save suspended state, dequeue + resolve request, loop.
      let state =
        State(..state.merge_globals(outer, suspended, []), heap: suspended.heap)
      let state = save_suspended(state, data_ref, suspended, AGSuspendedYield)
      let state = settle_head(state, data_ref)
      let state = fulfill_iter(state, req.resolve, value, False)
      resume_next(state, data_ref, execute_inner, unwind_to_catch)
    }
    Ok(#(AwaitCompletion(value), suspended)) -> {
      // Body hit await — save state (still Executing), set up promise callback.
      // Do NOT dequeue — the same request stays at head until a yield/return/throw.
      let state =
        State(..state.merge_globals(outer, suspended, []), heap: suspended.heap)
      let state = save_suspended(state, data_ref, suspended, AGExecuting)
      setup_await(state, data_ref, value, AGResumeBody)
    }
    Ok(#(NormalCompletion(value), final_state)) -> {
      let state =
        State(
          ..state.merge_globals(outer, final_state, []),
          heap: final_state.heap,
        )
      let state = complete(state, data_ref)
      let state = fulfill_iter(state, req.resolve, value, True)
      resume_next(state, data_ref, execute_inner, unwind_to_catch)
    }
    Ok(#(ThrowCompletion(thrown), final_state)) -> {
      let state =
        State(
          ..state.merge_globals(outer, final_state, []),
          heap: final_state.heap,
        )
      let state = complete(state, data_ref)
      let state = reject_with(state, req.reject, thrown)
      resume_next(state, data_ref, execute_inner, unwind_to_catch)
    }
    Error(vm_err) -> {
      let #(err, outer) =
        state.type_error_value(
          outer,
          "async generator execution failed: " <> state.vm_error_message(vm_err),
        )
      let state = complete(outer, data_ref)
      reject_with(state, req.reject, err)
    }
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
  execute_inner: ExecuteInnerFn(host),
  unwind_to_catch: UnwindToCatchFn(host),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  let settled = helpers.first_arg_or_undefined(args)
  let ret = fn(state: State(host)) {
    Ok(State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1))
  }
  case read_slot(state.heap, data_ref) {
    None ->
      Error(#(
        StepVmError(Unimplemented("async gen resume: slot missing")),
        JsUndefined,
        state,
      ))
    Some(gen) -> {
      let gen = normalize_queue(gen)
      case gen.queue_front {
        [] -> ret(state)
        [req, ..] -> {
          let run = Run(data_ref:, gen:, req:, execute_inner:, unwind_to_catch:)
          let state = case kind {
            AGResumeAwaitingReturn -> {
              // AwaitingReturn callback: settle the head return request.
              let state = complete(state, data_ref)
              let state = case is_reject {
                False -> fulfill_iter(state, req.resolve, settled, True)
                True -> reject_with(state, req.reject, settled)
              }
              resume_next(state, data_ref, execute_inner, unwind_to_catch)
            }
            AGResumeBody ->
              // Body await resumed — push settled value and run, or throw it in.
              case is_reject {
                True -> throw_into_gen_body(state, run, settled)
                False -> {
                  let stack = [settled, ..gen.saved_stack]
                  let es = build_exec_state(state, gen, stack, gen.saved_pc)
                  handle_exec_result(state, run, execute_inner(es))
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
          ret(state)
        }
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

/// A decoded AsyncGeneratorSlot. Two roles, one type:
///
///   READ — a snapshot taken at the top of resume_next / call_native_resume.
///   Its `queue_front`/`queue_back` are only ever used to DECIDE (is there a
///   head request? is more work pending?), never written back: user code can
///   enqueue re-entrantly at any point after the snapshot was taken, so a
///   snapshot queue is stale by the time we get to a write.
///
///   WRITE — `write_live` re-reads the slot into a fresh AsyncGenData at
///   write time, applies a pure update, and encodes it back. No writer in
///   this module accepts a queue argument, so it is impossible to overwrite
///   the live queue with a stale snapshot.
type AsyncGenData {
  AsyncGenData(
    data_ref: Ref,
    gen_state: value.AsyncGeneratorState,
    /// Decoded two-list FIFO — see AsyncGeneratorSlot.queue in value.gleam.
    queue_front: List(AsyncGenRequest),
    queue_back: List(AsyncGenRequest),
    func_template: value.FuncTemplate,
    env_ref: Ref,
    saved_pc: Int,
    saved_locals: tuple_array.TupleArray(JsValue),
    saved_stack: List(JsValue),
    saved_try_stack: List(value.SavedTryFrame),
  )
}

fn get_async_gen_data(h: Heap(host), this: JsValue) -> Option(AsyncGenData) {
  case this {
    JsObject(ref) ->
      case heap.read(h, ref) {
        Some(ObjectSlot(kind: AsyncGeneratorObject(generator_data: dr), ..)) ->
          read_slot(h, dr)
        _ -> None
      }
    _ -> None
  }
}

fn read_slot(h: Heap(host), data_ref: Ref) -> Option(AsyncGenData) {
  case heap.read(h, data_ref) {
    Some(AsyncGeneratorSlot(
      gen_state:,
      queue:,
      func_template:,
      env_ref:,
      saved_pc:,
      saved_locals:,
      saved_stack:,
      saved_try_stack:,
    )) -> {
      let #(queue_front, queue_back) = queue
      Some(AsyncGenData(
        data_ref:,
        gen_state:,
        queue_front:,
        queue_back:,
        func_template:,
        env_ref:,
        saved_pc:,
        saved_locals:,
        saved_stack:,
        saved_try_stack:,
      ))
    }
    _ -> None
  }
}

/// If front is empty, reverse back into front. Called at dequeue sites so the
/// head pattern-match `[req, ..rest]` always sees the oldest request.
fn normalize_queue(gen: AsyncGenData) -> AsyncGenData {
  case gen.queue_front, gen.queue_back {
    [], [_, ..] ->
      AsyncGenData(
        ..gen,
        queue_front: list.reverse(gen.queue_back),
        queue_back: [],
      )
    _, _ -> gen
  }
}

/// Encode a decoded AsyncGenData back into its heap slot — the exact inverse
/// of `read_slot`.
fn encode_slot(gen: AsyncGenData) -> HeapSlot(host) {
  AsyncGeneratorSlot(
    gen_state: gen.gen_state,
    queue: #(gen.queue_front, gen.queue_back),
    func_template: gen.func_template,
    env_ref: gen.env_ref,
    saved_pc: gen.saved_pc,
    saved_locals: gen.saved_locals,
    saved_stack: gen.saved_stack,
    saved_try_stack: gen.saved_try_stack,
  )
}

// ---------------------------------------------------------------------------
// The ONLY slot-write path. Everything below funnels through `write_live`.
// ---------------------------------------------------------------------------

/// Re-read the LIVE slot at write time, apply a pure update, write it back.
///
/// This is the structural fix for the stale-snapshot bug: user code running
/// inside the generator body (or a getter run while settling an iterator
/// result) can call .next()/.return()/.throw() re-entrantly, which enqueues
/// onto the live slot's queue_back. If a writer used a queue captured before
/// that user code ran, the re-entrant request would be silently overwritten.
/// Because every writer goes through here — and none of them takes a queue
/// argument — that overwrite is now impossible.
///
/// The slot must exist: every caller is inside a driver step that just read
/// it, and nothing deallocates AsyncGeneratorSlots mid-step.
fn write_live(
  state: State(host),
  data_ref: Ref,
  update: fn(AsyncGenData) -> AsyncGenData,
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
  AsyncGenData(..live, queue_back: [req, ..live.queue_back])
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
  AsyncGenData(
    ..live,
    gen_state: new_state,
    saved_pc: suspended.pc,
    saved_locals: suspended.locals,
    saved_stack: suspended.stack,
    saved_try_stack: generators.save_stacks(suspended.try_stack),
  )
}

// -- pure AsyncGenData updaters, composed inside `write_live` callbacks ------

fn with_state(gen: AsyncGenData, s: value.AsyncGeneratorState) -> AsyncGenData {
  AsyncGenData(..gen, gen_state: s)
}

/// Drop the head (oldest) request — the one the caller just settled. Any
/// requests enqueued re-entrantly after it are preserved.
fn drop_head(gen: AsyncGenData) -> AsyncGenData {
  let gen = normalize_queue(gen)
  case gen.queue_front {
    [_head, ..rest] -> AsyncGenData(..gen, queue_front: rest)
    [] -> gen
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
