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
import arc/vm/state.{
  type Heap, type HeapSlot, type State, type StepResult, State, StepVmError,
  Unimplemented,
}
import arc/vm/value.{
  type AsyncGenCompletion, type AsyncGenRequest, type JsValue, type Ref,
  AGAwaitingReturn, AGCompleted, AGExecuting, AGNext, AGReturn, AGSuspendedStart,
  AGSuspendedYield, AGThrow, AsyncGenRequest, AsyncGeneratorObject,
  AsyncGeneratorSlot, JsObject, JsUndefined, NativeFunction, ObjectSlot,
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
  let arg = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
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
                    True,
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
  let h = heap.write(state.heap, data_ref, slot_with_state(gen, AGExecuting))
  let #(restored_try, restored_finally) =
    generators.restore_stacks(gen.saved_try_stack, gen.saved_finally_stack)
  let gen_stack = case push_arg, req.completion {
    True, AGNext -> [req.value, ..gen.saved_stack]
    True, AGReturn -> gen.saved_stack
    True, AGThrow -> gen.saved_stack
    False, _ -> gen.saved_stack
  }
  let exec_state =
    State(
      ..state,
      heap: h,
      stack: gen_stack,
      locals: gen.saved_locals,
      func: gen.func_template,
      code: gen.func_template.bytecode,
      constants: gen.func_template.constants,
      pc: gen.saved_pc,
      call_stack: [],
      try_stack: restored_try,
      finally_stack: restored_finally,
      this_binding: gen.saved_this,
      callee_ref: gen.saved_callee_ref,
      call_args: [],
    )
  let exec_result = case req.completion {
    AGNext -> execute_inner(exec_state)
    AGThrow ->
      case unwind_to_catch(exec_state, req.value) {
        Some(caught) -> execute_inner(caught)
        None -> Ok(#(ThrowCompletion(req.value, exec_state.heap), exec_state))
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
      setup_await(state, data_ref, value, False)
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
/// Resumes the body (if is_return=False) or settles the pending return
/// request (if is_return=True, from the AwaitingReturn state).
pub fn call_native_resume(
  state: State,
  data_ref: Ref,
  is_reject: Bool,
  is_return: Bool,
  args: List(JsValue),
  rest_stack: List(JsValue),
  execute_inner: ExecuteInnerFn,
  unwind_to_catch: UnwindToCatchFn,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let settled = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
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
      case is_return {
        True -> {
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
        }
        False -> {
          // Body await resumed — restore, push settled value (or throw), run.
          case gen.queue {
            [req, ..rest_queue] -> {
              let #(restored_try, restored_finally) =
                generators.restore_stacks(
                  gen.saved_try_stack,
                  gen.saved_finally_stack,
                )
              let resume_stack = case is_reject {
                False -> [settled, ..gen.saved_stack]
                True -> gen.saved_stack
              }
              let exec_state =
                State(
                  ..state,
                  stack: resume_stack,
                  locals: gen.saved_locals,
                  func: gen.func_template,
                  code: gen.func_template.bytecode,
                  constants: gen.func_template.constants,
                  pc: gen.saved_pc,
                  call_stack: [],
                  try_stack: restored_try,
                  finally_stack: restored_finally,
                  this_binding: gen.saved_this,
                  callee_ref: gen.saved_callee_ref,
                  call_args: [],
                )
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
              let state =
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
              ret(state)
            }
            [] -> ret(state)
          }
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
  is_return: Bool,
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
    alloc_resume(h, b.function.prototype, data_ref, False, is_return)
  let #(h, on_reject) =
    alloc_resume(h, b.function.prototype, data_ref, True, is_return)
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
  is_return: Bool,
) -> #(Heap, Ref) {
  heap.alloc(
    h,
    ObjectSlot(
      kind: NativeFunction(
        value.Call(value.AsyncGeneratorResume(data_ref:, is_reject:, is_return:)),
      ),
      properties: dict.new(),
      elements: elements.new(),
      prototype: Some(function_proto),
      symbol_properties: dict.new(),
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

fn slot_with_state(gen: AsyncGenData, s: value.AsyncGeneratorState) -> HeapSlot {
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
