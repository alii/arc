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
import arc/vm/internal/elements
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
  AGReturn, AGSuspendedStart, AGSuspendedYield, AGThrow, AsyncGenRequest,
  AsyncGeneratorObject, AsyncGeneratorSlot, JsNull, JsObject, JsUndefined, Named,
  NativeFunction, ObjectSlot,
}
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub type ExecuteInnerFn =
  fn(State) -> Result(#(Completion, State), state.VmError)

pub type UnwindToCatchFn =
  fn(State, JsValue) -> Option(State)

/// AsyncGenerator.prototype.{next,return,throw} — shared entry point.
/// Per spec §27.6.1.2-4: create a promise capability, validate `this`
/// (reject on failure, don't throw sync), enqueue request, kick driver
/// if not already executing, return promise.
pub fn call_native_method(
  state: State,
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
  completion: AsyncGenCompletion,
  execute_inner: ExecuteInnerFn,
  unwind_to_catch: UnwindToCatchFn,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let arg = helpers.first_arg_or_undefined(args)
  let #(h, promise_ref, _data_ref, resolve, reject) =
    new_promise_capability(state.heap, state.builtins)
  let state = State(..state, heap: h)
  let ret = fn(state: State) {
    Ok(
      State(
        ..state,
        stack: [JsObject(promise_ref), ..rest_stack],
        pc: state.pc + 1,
      ),
    )
  }
  case get_async_gen_data(state.heap, this) {
    None -> {
      // Per spec: don't throw synchronously — reject the returned promise.
      let #(h, err) =
        common.make_type_error(
          state.heap,
          state.builtins,
          "AsyncGenerator method called on incompatible receiver",
        )
      let state = reject_with(State(..state, heap: h), reject, err)
      ret(state)
    }
    Some(gen) -> {
      let req = AsyncGenRequest(completion:, value: arg, resolve:, reject:)
      let new_queue = list.append(gen.queue, [req])
      let h =
        heap.write(state.heap, gen.data_ref, slot_with_queue(gen, new_queue))
      let state = State(..state, heap: h)
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
  state: State,
  data_ref: Ref,
  execute_inner: ExecuteInnerFn,
  unwind_to_catch: UnwindToCatchFn,
) -> State {
  case read_slot(state.heap, data_ref) {
    None -> state
    Some(gen) ->
      case gen.queue {
        [] -> state
        [req, ..rest_queue] ->
          case gen.gen_state {
            AGExecuting | AGAwaitingReturn -> state

            AGCompleted ->
              case req.completion {
                AGNext -> {
                  // Resolve {undefined, done:true}, dequeue, loop
                  let state = settle_head(state, data_ref, rest_queue, req)
                  let state =
                    fulfill_iter(state, req.resolve, JsUndefined, True)
                  resume_next(state, data_ref, execute_inner, unwind_to_catch)
                }
                AGThrow -> {
                  let state = settle_head(state, data_ref, rest_queue, req)
                  let state = reject_with(state, req.reject, req.value)
                  resume_next(state, data_ref, execute_inner, unwind_to_catch)
                }
                AGReturn -> {
                  // Spec: await Promise.resolve(value) first, then settle.
                  let h =
                    heap.write(
                      state.heap,
                      data_ref,
                      slot_with_state(gen, AGAwaitingReturn),
                    )
                  setup_await(
                    State(..state, heap: h),
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
                  let h =
                    heap.write(
                      state.heap,
                      data_ref,
                      slot_with_state(gen, AGCompleted),
                    )
                  resume_next(
                    State(..state, heap: h),
                    data_ref,
                    execute_inner,
                    unwind_to_catch,
                  )
                }
                AGNext ->
                  run_body(
                    state,
                    data_ref,
                    gen,
                    req,
                    rest_queue,
                    False,
                    execute_inner,
                    unwind_to_catch,
                  )
              }

            AGSuspendedYield ->
              run_body(
                state,
                data_ref,
                gen,
                req,
                rest_queue,
                True,
                execute_inner,
                unwind_to_catch,
              )
          }
      }
  }
}

/// Resume the generator body from a suspended state. Dispatches on the
/// request kind to push the arg / throw / inject return, then runs until
/// the body yields, awaits, returns, or throws.
fn run_body(
  state: State,
  data_ref: Ref,
  gen: AsyncGenData,
  req: AsyncGenRequest,
  rest_queue: List(AsyncGenRequest),
  push_arg: Bool,
  execute_inner: ExecuteInnerFn,
  unwind_to_catch: UnwindToCatchFn,
) -> State {
  // Mark executing FIRST so concurrent calls enqueue. saved_pc/stack preserved.
  let h = heap.write(state.heap, data_ref, slot_with_state(gen, AGExecuting))
  let state = State(..state, heap: h)

  // yield* delegation? Only relevant when resuming from a yield (push_arg=True)
  // with a return/throw request AND saved_pc is AsyncYieldStarNext.
  let delegated = case push_arg, req.completion {
    True, AGReturn | True, AGThrow -> async_delegate_iterator(gen)
    _, _ -> None
  }
  case delegated {
    Some(iter_ref) ->
      forward_async_delegate(
        state,
        data_ref,
        gen,
        req,
        rest_queue,
        iter_ref,
        execute_inner,
        unwind_to_catch,
      )
    None -> {
      let gen_stack = case push_arg, req.completion {
        True, AGNext -> [req.value, ..gen.saved_stack]
        True, AGReturn -> gen.saved_stack
        True, AGThrow -> gen.saved_stack
        False, _ -> gen.saved_stack
      }
      let exec_state = build_exec_state(state, gen, gen_stack, gen.saved_pc)
      let exec_result = case req.completion {
        AGNext -> execute_inner(exec_state)
        AGThrow ->
          case unwind_to_catch(exec_state, req.value) {
            Some(caught) -> execute_inner(caught)
            None ->
              Ok(#(ThrowCompletion(req.value, exec_state.heap), exec_state))
          }
        AGReturn ->
          // Inject a return: if there are finally blocks they should run.
          // For now, treat as normal completion with the return value.
          // TODO: proper finally unwinding like sync gen's process_generator_return.
          Ok(#(NormalCompletion(req.value, exec_state.heap), exec_state))
      }
      handle_exec_result(
        state,
        data_ref,
        gen,
        req,
        rest_queue,
        exec_result,
        execute_inner,
        unwind_to_catch,
      )
    }
  }
}

/// Build a body-execution State from a saved suspended snapshot.
/// Shared by run_body, call_native_resume, and the yield*-delegate paths.
/// Does NOT write AGExecuting — caller is responsible for slot state.
fn build_exec_state(
  state: State,
  gen: AsyncGenData,
  stack: List(JsValue),
  pc: Int,
) -> State {
  let #(restored_try, restored_finally) =
    generators.restore_stacks(gen.saved_try_stack, gen.saved_finally_stack)
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
    finally_stack: restored_finally,
    this_binding: gen.saved_this,
    callee_ref: gen.saved_callee_ref,
    call_args: [],
  )
}

// ============================================================================
// yield* delegation — ES §15.5.5 step 7.b/7.c
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

/// Forward a .return/.throw to the delegated async iterator.
/// Per spec: GetMethod(iter, name) → if missing handle specially; if present
/// call it, then AWAIT the result (unlike sync). The await suspends via
/// setup_await with AGResumeDelegate; settlement is handled in
/// resume_after_delegate.
fn forward_async_delegate(
  state: State,
  data_ref: Ref,
  gen: AsyncGenData,
  req: AsyncGenRequest,
  rest_queue: List(AsyncGenRequest),
  iter_ref: Ref,
  execute_inner: ExecuteInnerFn,
  unwind_to_catch: UnwindToCatchFn,
) -> State {
  let iter = JsObject(iter_ref)
  let method_name = case req.completion {
    AGThrow -> "throw"
    // AGReturn (AGNext never reaches here by construction)
    _ -> "return"
  }
  case object_ops.get_value(state, iter_ref, Named(method_name), iter) {
    Error(#(thrown, state)) ->
      throw_into_gen_body(
        state,
        data_ref,
        gen,
        req,
        rest_queue,
        thrown,
        execute_inner,
        unwind_to_catch,
      )
    Ok(#(JsUndefined, state)) | Ok(#(JsNull, state)) ->
      delegate_method_missing(
        state,
        data_ref,
        gen,
        req,
        rest_queue,
        iter_ref,
        execute_inner,
        unwind_to_catch,
      )
    Ok(#(method_fn, state)) ->
      case state.call(state, method_fn, iter, [req.value]) {
        Error(#(thrown, state)) ->
          throw_into_gen_body(
            state,
            data_ref,
            gen,
            req,
            rest_queue,
            thrown,
            execute_inner,
            unwind_to_catch,
          )
        Ok(#(result, state)) ->
          // Await the result. Gen stays AGExecuting, req stays at queue head,
          // saved_pc/stack unchanged. Resume via AGResumeDelegate.
          setup_await(state, data_ref, result, AGResumeDelegate(req.completion))
      }
  }
}

/// Inner iterator lacks .return/.throw. Per ES §15.5.5:
///   - missing throw → AsyncIteratorClose(iter), throw TypeError into body
///   - missing return → Await(received), then ReturnCompletion out of body
fn delegate_method_missing(
  state: State,
  data_ref: Ref,
  gen: AsyncGenData,
  req: AsyncGenRequest,
  rest_queue: List(AsyncGenRequest),
  iter_ref: Ref,
  execute_inner: ExecuteInnerFn,
  unwind_to_catch: UnwindToCatchFn,
) -> State {
  case req.completion {
    AGThrow ->
      // §7.b.iii.1-4: AsyncIteratorClose(iter, NormalCompletion(unused)),
      // then throw TypeError into body. Close calls iter.return() and AWAITS
      // it; result/error are discarded (outer abrupt completion wins).
      case close_async_iterator(state, iter_ref) {
        #(state, Some(close_result)) ->
          // Await the close. Gen stays AGExecuting; settle via
          // AGResumeDelegateClose which throws the TypeError into the body.
          setup_await(state, data_ref, close_result, AGResumeDelegateClose)
        #(state, None) ->
          // No .return on iter (or its lookup threw) — skip the await.
          throw_missing_throw_type_error(
            state,
            data_ref,
            gen,
            req,
            rest_queue,
            execute_inner,
            unwind_to_catch,
          )
      }
    AGReturn | AGNext -> {
      // §7.c.ii.1-3: no .return → Await(received), then ReturnCompletion out.
      // Reuse AGResumeAwaitingReturn: on settle it fulfils the head request
      // with {value: awaited, done: true} and marks AGCompleted.
      // NOTE: skips outer try/finally — same pre-existing limitation as the
      // non-delegating AGReturn path in run_body (see TODO there).
      let h =
        heap.write(state.heap, data_ref, slot_with_state(gen, AGAwaitingReturn))
      setup_await(
        State(..state, heap: h),
        data_ref,
        req.value,
        AGResumeAwaitingReturn,
      )
    }
  }
}

/// Throw a TypeError("does not provide a 'throw' method") into the generator
/// body. Shared by delegate_method_missing's no-.return-on-iter fast path and
/// the AGResumeDelegateClose settlement.
fn throw_missing_throw_type_error(
  state: State,
  data_ref: Ref,
  gen: AsyncGenData,
  req: AsyncGenRequest,
  rest_queue: List(AsyncGenRequest),
  execute_inner: ExecuteInnerFn,
  unwind_to_catch: UnwindToCatchFn,
) -> State {
  let #(h, err) =
    common.make_type_error(
      state.heap,
      state.builtins,
      "The iterator does not provide a 'throw' method.",
    )
  throw_into_gen_body(
    State(..state, heap: h),
    data_ref,
    gen,
    req,
    rest_queue,
    err,
    execute_inner,
    unwind_to_catch,
  )
}

/// Throw a value into the generator body at saved_pc (through its try/catch),
/// then dispatch via handle_exec_result. Shared by forward_async_delegate and
/// resume_after_delegate.
fn throw_into_gen_body(
  state: State,
  data_ref: Ref,
  gen: AsyncGenData,
  req: AsyncGenRequest,
  rest_queue: List(AsyncGenRequest),
  thrown: JsValue,
  execute_inner: ExecuteInnerFn,
  unwind_to_catch: UnwindToCatchFn,
) -> State {
  let exec_state = build_exec_state(state, gen, gen.saved_stack, gen.saved_pc)
  let exec_result = case unwind_to_catch(exec_state, thrown) {
    Some(caught) -> execute_inner(caught)
    None -> Ok(#(ThrowCompletion(thrown, exec_state.heap), exec_state))
  }
  handle_exec_result(
    state,
    data_ref,
    gen,
    req,
    rest_queue,
    exec_result,
    execute_inner,
    unwind_to_catch,
  )
}

/// AsyncIteratorClose step 1-4: GetMethod(iter, "return"); if present, call it
/// with no args. Returns Some(result) for the caller to await per ES §7.4.12
/// step 5. Errors from GetMethod/Call are swallowed (caller already has an
/// abrupt completion that takes precedence). None if no .return present.
fn close_async_iterator(
  state: State,
  iter_ref: Ref,
) -> #(State, Option(JsValue)) {
  let iter = JsObject(iter_ref)
  case object_ops.get_value(state, iter_ref, Named("return"), iter) {
    Ok(#(JsUndefined, state)) | Ok(#(JsNull, state)) -> #(state, None)
    Ok(#(ret_fn, state)) ->
      case state.call(state, ret_fn, iter, []) {
        Ok(#(result, state)) -> #(state, Some(result))
        Error(#(_thrown, state)) -> #(state, None)
      }
    Error(#(_thrown, state)) -> #(state, None)
  }
}

/// Read {done, value} off an awaited iterator-result object. Returns
/// Error(#(thrown, state)) for non-object results or property-get throws.
fn read_iter_result(
  state: State,
  settled: JsValue,
) -> Result(#(Bool, JsValue, State), #(JsValue, State)) {
  case settled {
    JsObject(rref) ->
      case object_ops.get_value(state, rref, Named("done"), settled) {
        Error(err) -> Error(err)
        Ok(#(done_v, state)) ->
          case object_ops.get_value(state, rref, Named("value"), settled) {
            Error(err) -> Error(err)
            Ok(#(val, state)) -> Ok(#(value.is_truthy(done_v), val, state))
          }
      }
    _non_obj -> {
      let #(h, err) =
        common.make_type_error(
          state.heap,
          state.builtins,
          "Iterator result is not an object",
        )
      Error(#(err, State(..state, heap: h)))
    }
  }
}

/// Handle the awaited result of a delegated iter.return()/iter.throw() call.
/// Called from call_native_resume's AGResumeDelegate branch.
fn resume_after_delegate(
  state: State,
  data_ref: Ref,
  gen: AsyncGenData,
  req: AsyncGenRequest,
  rest_queue: List(AsyncGenRequest),
  method: AsyncGenCompletion,
  is_reject: Bool,
  settled: JsValue,
  execute_inner: ExecuteInnerFn,
  unwind_to_catch: UnwindToCatchFn,
) -> State {
  // Awaited promise rejected → throw into body.
  case is_reject {
    True ->
      throw_into_gen_body(
        state,
        data_ref,
        gen,
        req,
        rest_queue,
        settled,
        execute_inner,
        unwind_to_catch,
      )
    False ->
      case read_iter_result(state, settled) {
        Error(#(thrown, state)) ->
          throw_into_gen_body(
            state,
            data_ref,
            gen,
            req,
            rest_queue,
            thrown,
            execute_inner,
            unwind_to_catch,
          )
        Ok(#(False, val, state)) -> {
          // Still delegating — yield val out, stay suspended at SAME
          // saved_pc/stack (AsyncYieldStarNext, [iter,..]).
          let h =
            heap.write(
              state.heap,
              data_ref,
              slot_with(gen, AGSuspendedYield, rest_queue),
            )
          let state = State(..state, heap: h)
          let state = fulfill_iter(state, req.resolve, val, False)
          resume_next(state, data_ref, execute_inner, unwind_to_catch)
        }
        Ok(#(True, val, state)) ->
          delegate_done(
            state,
            data_ref,
            gen,
            req,
            rest_queue,
            method,
            val,
            execute_inner,
            unwind_to_catch,
          )
      }
  }
}

/// Delegated iterator returned {done:true} from a forwarded return/throw.
///   throw → inner caught the throw and finished — yield* expr evaluates to
///           val; resume body at saved_pc+3 (past Next/Await/Resume).
///   return → inner returned done — outer gen returns val.
fn delegate_done(
  state: State,
  data_ref: Ref,
  gen: AsyncGenData,
  req: AsyncGenRequest,
  rest_queue: List(AsyncGenRequest),
  method: AsyncGenCompletion,
  val: JsValue,
  execute_inner: ExecuteInnerFn,
  unwind_to_catch: UnwindToCatchFn,
) -> State {
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
      handle_exec_result(
        state,
        data_ref,
        gen,
        req,
        rest_queue,
        execute_inner(exec_state),
        execute_inner,
        unwind_to_catch,
      )
    }
    AGReturn | AGNext -> {
      // Inner returned done — outer gen returns val.
      // (TODO finally-unwind applies here too, same as run_body's AGReturn.)
      let exec_state =
        build_exec_state(state, gen, gen.saved_stack, gen.saved_pc)
      handle_exec_result(
        state,
        data_ref,
        gen,
        req,
        rest_queue,
        Ok(#(NormalCompletion(val, exec_state.heap), exec_state)),
        execute_inner,
        unwind_to_catch,
      )
    }
  }
}

/// Dispatch on the body's completion: yield/await/return/throw.
fn handle_exec_result(
  outer: State,
  data_ref: Ref,
  gen: AsyncGenData,
  req: AsyncGenRequest,
  rest_queue: List(AsyncGenRequest),
  result: Result(#(Completion, State), state.VmError),
  execute_inner: ExecuteInnerFn,
  unwind_to_catch: UnwindToCatchFn,
) -> State {
  case result {
    Ok(#(YieldCompletion(value, h), suspended)) -> {
      // Body yielded — save suspended state, dequeue + resolve request, loop.
      let state = State(..state.merge_globals(outer, suspended, []), heap: h)
      let state =
        save_suspended(
          state,
          data_ref,
          gen,
          suspended,
          AGSuspendedYield,
          rest_queue,
        )
      let state = fulfill_iter(state, req.resolve, value, False)
      resume_next(state, data_ref, execute_inner, unwind_to_catch)
    }
    Ok(#(AwaitCompletion(value, h), suspended)) -> {
      // Body hit await — save state (still Executing), set up promise callback.
      // Do NOT dequeue — the same request stays at head until a yield/return/throw.
      let state = State(..state.merge_globals(outer, suspended, []), heap: h)
      let state =
        save_suspended(state, data_ref, gen, suspended, AGExecuting, [
          req,
          ..rest_queue
        ])
      setup_await(state, data_ref, value, AGResumeBody)
    }
    Ok(#(NormalCompletion(value, h), final_state)) -> {
      let state = State(..state.merge_globals(outer, final_state, []), heap: h)
      let state = complete(state, data_ref, gen, rest_queue)
      let state = fulfill_iter(state, req.resolve, value, True)
      resume_next(state, data_ref, execute_inner, unwind_to_catch)
    }
    Ok(#(ThrowCompletion(thrown, h), final_state)) -> {
      let state = State(..state.merge_globals(outer, final_state, []), heap: h)
      let state = complete(state, data_ref, gen, rest_queue)
      let state = reject_with(state, req.reject, thrown)
      resume_next(state, data_ref, execute_inner, unwind_to_catch)
    }
    Error(vm_err) -> {
      let #(h, err) =
        common.make_type_error(
          outer.heap,
          outer.builtins,
          "async generator execution failed: " <> string.inspect(vm_err),
        )
      let state = complete(State(..outer, heap: h), data_ref, gen, rest_queue)
      reject_with(state, req.reject, err)
    }
  }
}

/// AsyncGeneratorResume — called when an internal await's promise settles.
/// `kind` distinguishes: body await, AwaitingReturn microtask, or a yield*
/// delegated iter.return()/throw() result.
pub fn call_native_resume(
  state: State,
  data_ref: Ref,
  is_reject: Bool,
  kind: AGResumeKind,
  args: List(JsValue),
  rest_stack: List(JsValue),
  execute_inner: ExecuteInnerFn,
  unwind_to_catch: UnwindToCatchFn,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let settled = helpers.first_arg_or_undefined(args)
  let ret = fn(state: State) {
    Ok(State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1))
  }
  case read_slot(state.heap, data_ref) {
    None ->
      Error(#(
        StepVmError(Unimplemented("async gen resume: slot missing")),
        JsUndefined,
        state.heap,
      ))
    Some(gen) ->
      case kind {
        AGResumeAwaitingReturn ->
          // AwaitingReturn callback: settle the head return request.
          case gen.queue {
            [req, ..rest] -> {
              let h =
                heap.write(
                  state.heap,
                  data_ref,
                  slot_with(gen, AGCompleted, rest),
                )
              let state = State(..state, heap: h)
              let state = case is_reject {
                False -> fulfill_iter(state, req.resolve, settled, True)
                True -> reject_with(state, req.reject, settled)
              }
              ret(resume_next(state, data_ref, execute_inner, unwind_to_catch))
            }
            [] -> ret(state)
          }
        AGResumeBody ->
          // Body await resumed — restore, push settled value (or throw), run.
          case gen.queue {
            [req, ..rest_queue] -> {
              let resume_stack = case is_reject {
                False -> [settled, ..gen.saved_stack]
                True -> gen.saved_stack
              }
              let exec_state =
                build_exec_state(state, gen, resume_stack, gen.saved_pc)
              let exec_result = case is_reject {
                False -> execute_inner(exec_state)
                True ->
                  case unwind_to_catch(exec_state, settled) {
                    Some(caught) -> execute_inner(caught)
                    None ->
                      Ok(#(
                        ThrowCompletion(settled, exec_state.heap),
                        exec_state,
                      ))
                  }
              }
              ret(handle_exec_result(
                state,
                data_ref,
                gen,
                req,
                rest_queue,
                exec_result,
                execute_inner,
                unwind_to_catch,
              ))
            }
            [] -> ret(state)
          }
        AGResumeDelegate(method) ->
          case gen.queue {
            [req, ..rest_queue] ->
              ret(resume_after_delegate(
                state,
                data_ref,
                gen,
                req,
                rest_queue,
                method,
                is_reject,
                settled,
                execute_inner,
                unwind_to_catch,
              ))
            [] -> ret(state)
          }
        AGResumeDelegateClose ->
          // AsyncIteratorClose await settled — discard settled/is_reject
          // (outer abrupt completion wins), throw the missing-throw TypeError.
          case gen.queue {
            [req, ..rest_queue] ->
              ret(throw_missing_throw_type_error(
                state,
                data_ref,
                gen,
                req,
                rest_queue,
                execute_inner,
                unwind_to_catch,
              ))
            [] -> ret(state)
          }
      }
  }
}

// ============================================================================
// Await wiring — mirrors call.gleam's async_setup_await but with
// AsyncGeneratorResume callbacks instead of AsyncResume.
// ============================================================================

fn setup_await(
  state: State,
  data_ref: Ref,
  awaited: JsValue,
  kind: AGResumeKind,
) -> State {
  let h = state.heap
  let b = state.builtins
  let existing = case awaited {
    JsObject(ref) -> heap.read_promise_data_ref(h, ref)
    _ -> None
  }
  let #(h, promise_data) = case existing {
    Some(dr) -> #(h, dr)
    None -> {
      let #(h, _, dr) = promises.create_resolved_promise(h, b, awaited)
      #(h, dr)
    }
  }
  let #(h, on_fulfill) =
    alloc_resume(h, b.function.prototype, data_ref, False, kind)
  let #(h, on_reject) =
    alloc_resume(h, b.function.prototype, data_ref, True, kind)
  let #(h, child_ref, child_data) =
    builtins_promise.create_promise(h, b.promise.prototype)
  let #(h, child_resolve, child_reject) =
    builtins_promise.create_resolving_functions(
      h,
      b.function.prototype,
      child_ref,
      child_data,
    )
  builtins_promise.perform_promise_then(
    State(..state, heap: h),
    promise_data,
    JsObject(on_fulfill),
    JsObject(on_reject),
    child_resolve,
    child_reject,
  )
}

fn alloc_resume(
  h: Heap,
  function_proto: Ref,
  data_ref: Ref,
  is_reject: Bool,
  kind: AGResumeKind,
) -> #(Heap, Ref) {
  heap.alloc(
    h,
    ObjectSlot(
      kind: NativeFunction(
        value.Call(value.AsyncGeneratorResume(data_ref:, is_reject:, kind:)),
      ),
      properties: dict.new(),
      elements: elements.new(),
      prototype: Some(function_proto),
      symbol_properties: [],
      extensible: True,
    ),
  )
}

// ============================================================================
// Slot read/write helpers
// ============================================================================

type AsyncGenData {
  AsyncGenData(
    data_ref: Ref,
    gen_state: value.AsyncGeneratorState,
    queue: List(AsyncGenRequest),
    func_template: value.FuncTemplate,
    env_ref: Ref,
    saved_pc: Int,
    saved_locals: tuple_array.TupleArray(JsValue),
    saved_stack: List(JsValue),
    saved_try_stack: List(value.SavedTryFrame),
    saved_finally_stack: List(value.SavedFinallyCompletion),
    saved_this: JsValue,
    saved_callee_ref: Option(Ref),
  )
}

fn get_async_gen_data(h: Heap, this: JsValue) -> Option(AsyncGenData) {
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

fn read_slot(h: Heap, data_ref: Ref) -> Option(AsyncGenData) {
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
      saved_finally_stack:,
      saved_this:,
      saved_callee_ref:,
    )) ->
      Some(AsyncGenData(
        data_ref:,
        gen_state:,
        queue:,
        func_template:,
        env_ref:,
        saved_pc:,
        saved_locals:,
        saved_stack:,
        saved_try_stack:,
        saved_finally_stack:,
        saved_this:,
        saved_callee_ref:,
      ))
    _ -> None
  }
}

fn slot_with_state(
  gen: AsyncGenData,
  s: value.AsyncGeneratorState,
) -> HeapSlot {
  slot_with(gen, s, gen.queue)
}

fn slot_with_queue(gen: AsyncGenData, q: List(AsyncGenRequest)) -> HeapSlot {
  slot_with(gen, gen.gen_state, q)
}

fn slot_with(
  gen: AsyncGenData,
  s: value.AsyncGeneratorState,
  q: List(AsyncGenRequest),
) -> HeapSlot {
  AsyncGeneratorSlot(
    gen_state: s,
    queue: q,
    func_template: gen.func_template,
    env_ref: gen.env_ref,
    saved_pc: gen.saved_pc,
    saved_locals: gen.saved_locals,
    saved_stack: gen.saved_stack,
    saved_try_stack: gen.saved_try_stack,
    saved_finally_stack: gen.saved_finally_stack,
    saved_this: gen.saved_this,
    saved_callee_ref: gen.saved_callee_ref,
  )
}

fn save_suspended(
  state: State,
  data_ref: Ref,
  gen: AsyncGenData,
  suspended: State,
  new_state: value.AsyncGeneratorState,
  queue: List(AsyncGenRequest),
) -> State {
  let #(saved_try, saved_finally) =
    generators.save_stacks(suspended.try_stack, suspended.finally_stack)
  let h =
    heap.write(
      state.heap,
      data_ref,
      AsyncGeneratorSlot(
        gen_state: new_state,
        queue:,
        func_template: gen.func_template,
        env_ref: gen.env_ref,
        saved_pc: suspended.pc,
        saved_locals: suspended.locals,
        saved_stack: suspended.stack,
        saved_try_stack: saved_try,
        saved_finally_stack: saved_finally,
        saved_this: suspended.this_binding,
        saved_callee_ref: suspended.callee_ref,
      ),
    )
  State(..state, heap: h)
}

fn complete(
  state: State,
  data_ref: Ref,
  gen: AsyncGenData,
  queue: List(AsyncGenRequest),
) -> State {
  let h = heap.write(state.heap, data_ref, slot_with(gen, AGCompleted, queue))
  State(..state, heap: h)
}

fn settle_head(
  state: State,
  data_ref: Ref,
  rest_queue: List(AsyncGenRequest),
  _req: AsyncGenRequest,
) -> State {
  case read_slot(state.heap, data_ref) {
    Some(gen) -> {
      let h = heap.write(state.heap, data_ref, slot_with_queue(gen, rest_queue))
      State(..state, heap: h)
    }
    None -> state
  }
}

// ============================================================================
// Promise helpers
// ============================================================================

fn new_promise_capability(
  h: Heap,
  b: common.Builtins,
) -> #(Heap, Ref, Ref, JsValue, JsValue) {
  let #(h, promise_ref, data_ref) =
    builtins_promise.create_promise(h, b.promise.prototype)
  let #(h, resolve, reject) =
    builtins_promise.create_resolving_functions(
      h,
      b.function.prototype,
      promise_ref,
      data_ref,
    )
  #(h, promise_ref, data_ref, resolve, reject)
}

/// Call resolve({value, done}) via state.call.
fn fulfill_iter(
  state: State,
  resolve: JsValue,
  val: JsValue,
  done: Bool,
) -> State {
  let #(h, result) =
    generators.create_iterator_result(state.heap, state.builtins, val, done)
  call_fn(State(..state, heap: h), resolve, [result])
}

fn reject_with(state: State, reject: JsValue, reason: JsValue) -> State {
  call_fn(state, reject, [reason])
}

fn call_fn(state: State, f: JsValue, args: List(JsValue)) -> State {
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
