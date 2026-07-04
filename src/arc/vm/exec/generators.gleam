import arc/vm/builtins/common
import arc/vm/builtins/helpers
import arc/vm/builtins/iterator as builtins_iterator
import arc/vm/completion.{
  type Outcome, Completed, NormalCompletion, Suspended, ThrowCompletion,
}
import arc/vm/heap
import arc/vm/internal/tuple_array
import arc/vm/key.{Named}
import arc/vm/opcode.{CatchOnly, Finally, IterCloseGuard, YieldStar}
import arc/vm/ops/object as object_ops
import arc/vm/state.{
  type Heap, type HeapSlot, type State, type StepExit, type TryFrame,
  InternalError, State, Threw, TryFrame, VmFailed,
}
import arc/vm/value.{
  type DelegateMethod, type FuncTemplate, type JsValue, type Ref, DelegateReturn,
  DelegateThrow, GeneratorObject, GeneratorSlot, JsObject, JsUndefined,
  ObjectSlot,
}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

// ============================================================================
// Callback types for VM functions that can't be imported directly
// ============================================================================

pub type ExecuteInnerFn(host) =
  fn(State(host)) -> Result(#(Outcome, State(host)), state.VmError)

pub type UnwindToCatchFn(host) =
  fn(State(host), JsValue) -> Option(State(host))

// ============================================================================
// Generator native function implementations
// ============================================================================

/// Generator.prototype.next(value) -- resume a suspended generator.
/// JS-visible entry point: allocates the {value, done} result object.
pub fn call_native_generator_next(
  state: State(host),
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
  execute_inner: ExecuteInnerFn(host),
  _unwind_to_catch: UnwindToCatchFn(host),
) -> Result(State(host), StepExit(host)) {
  let next_arg = helpers.first_arg_or_undefined(args)
  resume_generator_next(state, this, next_arg, execute_inner)
  |> alloc_iter_result(rest_stack)
}

/// Internal resume API — like `call_native_generator_next` but returns
/// #(done, value, state) directly instead of allocating a JS {value, done}
/// result object. Used by for-of (IteratorNext) and array spread, which would
/// otherwise allocate a result object per iteration only to read it back and
/// discard it (the heap is never GC'd mid-script, so each one grows the heap
/// permanently). The returned state has heap/globals merged and pc advanced
/// past the current op; its stack is the caller's stack, untouched.
pub fn resume_generator_next(
  state: State(host),
  this: JsValue,
  next_arg: JsValue,
  execute_inner: ExecuteInnerFn(host),
) -> Result(#(Bool, JsValue, State(host)), StepExit(host)) {
  case get_generator_data(state.heap, this) {
    Some(gen) ->
      case gen.gen_state {
        value.Completed ->
          // Already done -- {value: undefined, done: true} without alloc.
          Ok(#(True, JsUndefined, State(..state, pc: state.pc + 1)))
        value.Executing -> {
          state.throw_type_error(state, "Generator is already running")
        }
        value.SuspendedStart | value.SuspendedYield -> {
          // For SuspendedYield, push the .next() arg onto the saved stack
          // (the Yield opcode left pc pointing past Yield, stack has value popped)
          let gen_stack = case gen.gen_state {
            value.SuspendedYield -> [next_arg, ..gen.saved_stack]
            _ -> gen.saved_stack
          }
          let gen_exec_state =
            build_resumed_state(state, gen, gen_stack, gen.saved_pc)
          run_to_completion(gen_exec_state, state, gen, execute_inner)
        }
      }
    None -> {
      state.throw_type_error(state, "not a generator object")
    }
  }
}

/// Wrap an internal #(done, value, state) resume result into the JS-visible
/// convention: allocate {value, done} and push it onto rest_stack.
fn alloc_iter_result(
  res: Result(#(Bool, JsValue, State(host)), StepExit(host)),
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  use #(done, val, st) <- result.map(res)
  let #(h, obj) = common.create_iter_result(st.heap, st.builtins, val, done)
  State(..st, heap: h, stack: [obj, ..rest_stack])
}

/// Generator.prototype.return(value) -- complete the generator with a return value.
pub fn call_native_generator_return(
  state: State(host),
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
  execute_inner: ExecuteInnerFn(host),
  unwind_to_catch: UnwindToCatchFn(host),
) -> Result(State(host), StepExit(host)) {
  let return_val = helpers.first_arg_or_undefined(args)
  case get_generator_data(state.heap, this) {
    Some(gen) ->
      case gen.gen_state {
        value.Completed | value.SuspendedStart -> {
          // Mark completed and return {value, done: true}
          let state = complete(state, gen)
          let #(h, result) =
            common.create_iter_result(
              state.heap,
              state.builtins,
              return_val,
              True,
            )
          Ok(
            State(
              ..state,
              heap: h,
              stack: [result, ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        }
        value.Executing -> {
          state.throw_type_error(state, "Generator is already running")
        }
        value.SuspendedYield ->
          case delegate_iterator(gen) {
            Some(iter_ref) ->
              forward_delegate(
                state,
                gen,
                iter_ref,
                DelegateReturn,
                return_val,
                rest_stack,
                execute_inner,
                fn(state) {
                  // Inner iterator has no .return — §27.5.3.8 step 7.c.iii:
                  // exit delegation and let the outer return proceed normally.
                  do_return_resume(
                    state,
                    gen,
                    return_val,
                    rest_stack,
                    execute_inner,
                    unwind_to_catch,
                  )
                },
              )
            None ->
              do_return_resume(
                state,
                gen,
                return_val,
                rest_stack,
                execute_inner,
                unwind_to_catch,
              )
          }
      }
    None -> {
      state.throw_type_error(state, "not a generator object")
    }
  }
}

/// Resume a suspended generator with a return completion — restore its
/// execution state and run through any enclosing finally blocks.
fn do_return_resume(
  state: State(host),
  gen: GenData,
  return_val: JsValue,
  rest_stack: List(JsValue),
  execute_inner: ExecuteInnerFn(host),
  unwind_to_catch: UnwindToCatchFn(host),
) -> Result(State(host), StepExit(host)) {
  let gen_exec_state =
    build_resumed_state(state, gen, gen.saved_stack, gen.saved_pc)
  unwind_return(gen_exec_state, return_val, execute_inner, unwind_to_catch)
  |> settle_completion(state, gen)
  |> alloc_iter_result(rest_stack)
}

/// Generator.prototype.throw(exception) -- throw into the generator.
pub fn call_native_generator_throw(
  state: State(host),
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
  execute_inner: ExecuteInnerFn(host),
  unwind_to_catch: UnwindToCatchFn(host),
) -> Result(State(host), StepExit(host)) {
  let throw_val = helpers.first_arg_or_undefined(args)
  case get_generator_data(state.heap, this) {
    Some(gen) ->
      case gen.gen_state {
        value.Completed | value.SuspendedStart ->
          // Mark completed and throw the exception
          complete_and_throw(state, gen, throw_val)
        value.Executing -> {
          state.throw_type_error(state, "Generator is already running")
        }
        value.SuspendedYield ->
          case delegate_iterator(gen) {
            Some(iter_ref) ->
              forward_delegate(
                state,
                gen,
                iter_ref,
                DelegateThrow,
                throw_val,
                rest_stack,
                execute_inner,
                fn(state) {
                  // Inner iterator has no .throw — §27.5.3.8 step 7.b.iii-vi:
                  // ? IteratorClose(iteratorRecord, NormalCompletion(empty)),
                  // then throw a TypeError. The `?` matters: a `.return` that
                  // is not callable, throws, or yields a non-object propagates
                  // ITS error instead of the TypeError.
                  let #(state, closed) =
                    builtins_iterator.iterator_close_normal(
                      state,
                      JsObject(iter_ref),
                    )
                  let state = complete(state, gen)
                  case closed {
                    Error(thrown) -> Error(Threw(thrown, state))
                    Ok(Nil) ->
                      state.throw_type_error(
                        state,
                        "The iterator does not provide a 'throw' method.",
                      )
                  }
                },
              )
            None -> {
              let gen_exec_state =
                build_resumed_state(state, gen, gen.saved_stack, gen.saved_pc)
              // Try to unwind to a catch handler within the generator
              case unwind_to_catch(gen_exec_state, throw_val) {
                Some(caught_state) ->
                  // The generator caught it -- continue executing
                  run_to_completion(caught_state, state, gen, execute_inner)
                  |> alloc_iter_result(rest_stack)
                None ->
                  // No catch handler -- mark completed and propagate the throw.
                  // Keep the body's heap (build_resumed_state wrote Executing).
                  complete_and_throw(
                    State(..state, heap: gen_exec_state.heap),
                    gen,
                    throw_val,
                  )
              }
            }
          }
      }
    None -> {
      state.throw_type_error(state, "not a generator object")
    }
  }
}

/// Extract the GeneratorSlot from a generator `this` value.
/// Extracted generator data -- avoids Gleam's "don't know type of variant field" issue.
type GenData {
  GenData(
    data_ref: Ref,
    gen_state: value.GeneratorState,
    func_template: FuncTemplate,
    env_ref: Ref,
    saved_pc: Int,
    saved_locals: tuple_array.TupleArray(JsValue),
    saved_stack: List(JsValue),
    saved_try_stack: List(value.SavedTryFrame),
    saved_eval_env: Option(Ref),
    saved_line: Int,
  )
}

fn get_generator_data(h: Heap(host), this: JsValue) -> Option(GenData) {
  case this {
    JsObject(obj_ref) ->
      case heap.read(h, obj_ref) {
        Some(ObjectSlot(kind: GeneratorObject(generator_data: data_ref), ..)) ->
          case heap.read(h, data_ref) {
            Some(GeneratorSlot(
              gen_state:,
              func_template:,
              env_ref:,
              saved_pc:,
              saved_locals:,
              saved_stack:,
              saved_try_stack:,
              saved_eval_env:,
              saved_line:,
            )) ->
              Some(GenData(
                data_ref:,
                gen_state:,
                func_template:,
                env_ref:,
                saved_pc:,
                saved_locals:,
                saved_stack:,
                saved_try_stack:,
                saved_eval_env:,
                saved_line:,
              ))
            _ -> None
          }
        _ -> None
      }
    _ -> None
  }
}

/// Create a GeneratorSlot with only the gen_state changed.
fn gen_with_state(
  gen: GenData,
  new_state: value.GeneratorState,
) -> HeapSlot(host) {
  GeneratorSlot(
    gen_state: new_state,
    func_template: gen.func_template,
    env_ref: gen.env_ref,
    saved_pc: gen.saved_pc,
    saved_locals: gen.saved_locals,
    saved_stack: gen.saved_stack,
    saved_try_stack: gen.saved_try_stack,
    saved_eval_env: gen.saved_eval_env,
    saved_line: gen.saved_line,
  )
}

/// Mark the generator Completed in `state`'s heap. The ONLY way this module
/// completes a generator — "marked done but forgot to write the heap" (or
/// wrote it to the wrong ref) is not expressible through it.
fn complete(state: State(host), gen: GenData) -> State(host) {
  State(
    ..state,
    heap: heap.write(
      state.heap,
      gen.data_ref,
      gen_with_state(gen, value.Completed),
    ),
  )
}

/// Complete the generator and propagate `thrown` out of the driver.
fn complete_and_throw(
  state: State(host),
  gen: GenData,
  thrown: JsValue,
) -> Result(a, StepExit(host)) {
  Error(Threw(thrown, complete(state, gen)))
}

/// Mark a generator Executing and restore its saved execution context into a
/// fresh State for resumption. `stack` and `pc` vary per resume mode.
fn build_resumed_state(
  outer: State(host),
  gen: GenData,
  stack: List(JsValue),
  pc: Int,
) -> State(host) {
  let h =
    heap.write(outer.heap, gen.data_ref, gen_with_state(gen, value.Executing))
  let restored_try = restore_stacks(gen.saved_try_stack)
  State(
    ..outer,
    heap: h,
    stack:,
    locals: gen.saved_locals,
    func: gen.func_template,
    code: gen.func_template.bytecode,
    constants: gen.func_template.constants,
    pc:,
    call_stack: [],
    try_stack: restored_try,
    new_target: JsUndefined,
    call_args: [],
    // Per-frame fields: without these the body would inherit the RESUMER's
    // eval_env (losing direct-eval `var`s across a yield) and its line number.
    eval_env: gen.saved_eval_env,
    current_line: gen.saved_line,
  )
}

/// If suspended at a YieldStar opcode, return the delegated iterator (top of
/// saved_stack). YieldStar keeps pc unchanged on yield, so this check is
/// exact — no extra delegate slot needed.
fn delegate_iterator(gen: GenData) -> Option(Ref) {
  // saved_pc was a valid dispatch target — always in bounds.
  case tuple_array.unsafe_get(gen.saved_pc, gen.func_template.bytecode) {
    YieldStar ->
      case gen.saved_stack {
        [JsObject(iter_ref), ..] -> Some(iter_ref)
        _ -> None
      }
    _ -> None
  }
}

/// Forward a .throw/.return to the delegated iterator. If the iterator has
/// the method, call it and dispatch on done:
///   - done → push value onto stack past YieldStar, resume generator body
///   - !done → still delegating; yield the inner value back out
/// If the iterator lacks .throw, per §27.5.3.8 close it and throw TypeError.
/// If it lacks .return, exit delegation and let the outer return proceed.
fn forward_delegate(
  state: State(host),
  gen: GenData,
  iter_ref: Ref,
  method: DelegateMethod,
  arg: JsValue,
  rest_stack: List(JsValue),
  execute_inner: ExecuteInnerFn(host),
  on_missing: fn(State(host)) -> Result(State(host), StepExit(host)),
) -> Result(State(host), StepExit(host)) {
  let iter = JsObject(iter_ref)
  case object_ops.get_value(state, iter_ref, delegate_key(method), iter) {
    Error(#(thrown, state)) -> complete_and_throw(state, gen, thrown)
    Ok(#(JsUndefined, state)) | Ok(#(value.JsNull, state)) -> on_missing(state)
    Ok(#(method_fn, state)) ->
      case state.call(state, method_fn, iter, [arg]) {
        Error(#(thrown, state)) -> complete_and_throw(state, gen, thrown)
        Ok(#(res, state)) -> {
          // §27.5.3.2 steps 7.a/7.b: IteratorComplete + IteratorValue on the
          // forwarded result. Both property reads can run user getters, so
          // thread their state; a getter throw (or a non-object result)
          // marks the generator Completed and propagates, matching the
          // surrounding error arms.
          use #(done, val, state) <- result.try(
            result.map_error(
              builtins_iterator.read_iter_result(state, res),
              state.map_exit_state(_, fn(st) { complete(st, gen) }),
            ),
          )
          case done {
            False -> {
              // Still delegating — save state (pc stays at YieldStar, iter
              // still on stack) and yield val out.
              let h =
                heap.write(
                  state.heap,
                  gen.data_ref,
                  gen_with_state(gen, value.SuspendedYield),
                )
              let #(h, result) =
                common.create_iter_result(h, state.builtins, val, False)
              Ok(
                State(
                  ..state,
                  heap: h,
                  stack: [result, ..rest_stack],
                  pc: state.pc + 1,
                ),
              )
            }
            True ->
              // Delegation finished — resume generator body past YieldStar
              // with val on stack. For .return, per spec this is a return
              // completion so we ALSO need to return out of the generator;
              // for .throw, we continue normally.
              resume_after_delegate(
                state,
                gen,
                val,
                method,
                rest_stack,
                execute_inner,
              )
          }
        }
      }
  }
}

/// The property key a delegated method is looked up under.
fn delegate_key(method: DelegateMethod) -> key.PropertyKey {
  case method {
    DelegateReturn -> Named("return")
    DelegateThrow -> Named("throw")
  }
}

/// Delegated iterator returned {done:true}. For .throw, resume the generator
/// body normally past YieldStar with result.value on stack. For .return, the
/// outer generator must ALSO return — complete it with that value.
fn resume_after_delegate(
  state: State(host),
  gen: GenData,
  val: JsValue,
  method: DelegateMethod,
  rest_stack: List(JsValue),
  execute_inner: ExecuteInnerFn(host),
) -> Result(State(host), StepExit(host)) {
  case method {
    DelegateReturn -> {
      // §27.5.3.8 step 7.c.viii: if the inner iterator's return completed,
      // perform a return completion on the outer generator too.
      let state = complete(state, gen)
      let #(h, result) =
        common.create_iter_result(state.heap, state.builtins, val, True)
      Ok(
        State(..state, heap: h, stack: [result, ..rest_stack], pc: state.pc + 1),
      )
    }
    DelegateThrow -> {
      // .throw forwarded and inner is done — continue outer body past
      // YieldStar with val on stack (the yield* expression's value).
      let stack_after = case gen.saved_stack {
        [_iter, ..rest] -> [val, ..rest]
        _ -> [val]
      }
      let resumed =
        build_resumed_state(state, gen, stack_after, gen.saved_pc + 1)
      run_to_completion(resumed, state, gen, execute_inner)
      |> alloc_iter_result(rest_stack)
    }
  }
}

/// Run a resumed generator to its next suspension/completion and marshal the
/// result back to the caller as #(done, value, state) — no result-object
/// allocation. Shared tail for delegate-forward continuations; JS-visible
/// callers wrap with `alloc_iter_result`.
fn run_to_completion(
  resumed: State(host),
  outer: State(host),
  gen: GenData,
  execute_inner: ExecuteInnerFn(host),
) -> Result(#(Bool, JsValue, State(host)), StepExit(host)) {
  settle_completion(execute_inner(resumed), outer, gen)
}

/// Marshal a body completion into the sync driver's #(done, value, state)
/// convention: save the generator on yield, mark it Completed on
/// return/throw. Split out of `run_to_completion` so return-unwinding
/// (`unwind_return`), which produces the completion itself, shares the exact
/// same settlement.
fn settle_completion(
  exec_result: Result(#(Outcome, State(host)), state.VmError),
  outer: State(host),
  gen: GenData,
) -> Result(#(Bool, JsValue, State(host)), StepExit(host)) {
  case exec_result {
    Ok(#(Suspended(completion.Yield, yv), suspended)) -> {
      let st = save_stacks(suspended.try_stack)
      let h =
        heap.write(
          suspended.heap,
          gen.data_ref,
          GeneratorSlot(
            gen_state: value.SuspendedYield,
            func_template: gen.func_template,
            env_ref: gen.env_ref,
            saved_pc: suspended.pc,
            saved_locals: suspended.locals,
            saved_stack: suspended.stack,
            saved_try_stack: st,
            saved_eval_env: suspended.eval_env,
            saved_line: suspended.current_line,
          ),
        )
      Ok(#(
        False,
        yv,
        State(..state.adopt_child(outer, suspended), heap: h, pc: outer.pc + 1),
      ))
    }
    Ok(#(Completed(NormalCompletion(rv)), final_state)) -> {
      let final_state = complete(final_state, gen)
      Ok(#(
        True,
        rv,
        State(..state.adopt_child(outer, final_state), pc: outer.pc + 1),
      ))
    }
    Ok(#(Completed(ThrowCompletion(thrown)), thrown_state)) -> {
      let thrown_state = complete(thrown_state, gen)
      // Adopt the throwing body, exactly like the Normal / Yield arms: jobs it
      // enqueued and ctx mutations must survive the throw, not just its heap.
      Error(Threw(thrown, state.adopt_child(outer, thrown_state)))
    }
    Ok(#(Suspended(completion.Await, _), _)) ->
      Error(VmFailed(
        InternalError("settle_completion", "await in sync generator"),
        outer,
      ))
    Error(vm_err) -> Error(VmFailed(vm_err, complete(outer, gen)))
  }
}

/// A try frame that a return completion must run code for while unwinding:
/// either a try/finally body, or an iterator-guarding frame (for-of loop /
/// array-destructuring scaffold) whose iterator must be closed (§7.4.9).
type ReturnHandler {
  /// try/finally: enter the finally subroutine at `fin_label`.
  FinallyHandler(fin_label: Int, stack_depth: Int, rest: List(TryFrame))
  /// for-of / array-destructuring close guard: the value at the recorded
  /// stack depth is the live iterator — call its `return` method.
  IterCloseHandler(stack_depth: Int, rest: List(TryFrame))
}

/// Walk the try_stack, skipping catch-only entries, looking for the first
/// frame a return completion must visit — §27.5.3.4 GeneratorResumeAbrupt: a
/// return propagating out of a yield runs enclosing finallys AND closes any
/// live for-of / destructuring iterators.
///
/// Each frame's `TryKind` was supplied by the emitter at the site that knew
/// which of the three it was building, so this never has to disassemble the
/// instruction sitting at `catch_target` to guess.
fn find_next_return_handler(
  try_stack: List(TryFrame),
) -> Option(ReturnHandler) {
  case try_stack {
    [] -> None
    [TryFrame(kind: Finally(fin_label:), stack_depth:, ..), ..rest] ->
      Some(FinallyHandler(fin_label, stack_depth, rest))
    [TryFrame(kind: IterCloseGuard, stack_depth:, ..), ..rest] ->
      Some(IterCloseHandler(stack_depth, rest))
    [TryFrame(kind: CatchOnly, ..), ..rest] -> find_next_return_handler(rest)
  }
}

/// Resume a suspended generator body with a *return completion* — the core of
/// GeneratorResumeAbrupt (§27.5.3.4) shared by the sync driver
/// (`do_return_resume`) and the async driver's AGReturn paths
/// (async_generators.gleam), so the two flavours cannot diverge.
///
/// Given the body State restored at its suspension point, walk the try_stack
/// running each enclosing finally block (innermost → outermost) and closing
/// any live for-of / destructuring iterators (§7.4.9). The result has the
/// same shape as `execute_inner`, so each driver settles it with its normal
/// completion dispatch (`settle_completion` / `handle_exec_result`):
/// - Completed(NormalCompletion(return_val)): nothing intercepted the return
///   — the generator completes with the requested value.
/// - Suspended(Yield/Await, ..): a finally block suspended; the returned
///   State is the new suspension point.
/// - Completed(ThrowCompletion(..)): a finally block or iterator close threw
///   and nothing inside the generator caught it.
pub fn unwind_return(
  gen_state: State(host),
  return_val: JsValue,
  execute_inner: ExecuteInnerFn(host),
  unwind_to_catch: UnwindToCatchFn(host),
) -> Result(#(Outcome, State(host)), state.VmError) {
  case find_next_return_handler(gen_state.try_stack) {
    // No more finally blocks / iterator guards: the return completes the body.
    None -> Ok(#(Completed(NormalCompletion(return_val)), gen_state))
    // §7.4.9 IteratorClose with a return completion: a for-of loop or
    // array-destructuring scaffold suspended at a yield has its live
    // iterator at the frame's recorded stack depth. Call its `return`
    // method, then keep unwinding. Errors raised by the close (getter
    // throw, call throw, non-object result) replace the return completion
    // with a throw completion that unwinds through the REMAINING frames
    // (catchable by the generator's own try/catch).
    Some(IterCloseHandler(stack_depth, remaining_try)) -> {
      let restored_stack = truncate_stack(gen_state.stack, stack_depth)
      case restored_stack {
        [JsObject(slot_ref), ..base] -> {
          let st = State(..gen_state, try_stack: remaining_try, stack: base)
          // GetIterator slots may hold an internal IteratorRecordObject
          // wrapper (cached `next`) — close the REAL iterator behind it.
          let iter = case heap.read(st.heap, slot_ref) {
            Some(ObjectSlot(kind: value.IteratorRecordObject(iterated:, ..), ..)) ->
              iterated
            _ -> JsObject(slot_ref)
          }
          case iter {
            JsObject(iter_ref) ->
              close_for_return(
                st,
                iter_ref,
                return_val,
                execute_inner,
                unwind_to_catch,
              )
            // Not an object (defensive) — nothing to close.
            _ -> unwind_return(st, return_val, execute_inner, unwind_to_catch)
          }
        }
        // Slot is not a live iterator (for-of's [[Done]] sentinel writes
        // undefined into it) — nothing to close, keep unwinding.
        [_, ..base] ->
          unwind_return(
            State(..gen_state, try_stack: remaining_try, stack: base),
            return_val,
            execute_inner,
            unwind_to_catch,
          )
        [] ->
          unwind_return(
            State(..gen_state, try_stack: remaining_try, stack: []),
            return_val,
            execute_inner,
            unwind_to_catch,
          )
      }
    }
    Some(FinallyHandler(fin_label, stack_depth, remaining_try)) -> {
      // Found a finally handler. Enter the finally subroutine directly with
      // the gosub calling convention: stack = [retpc, slot, ..base]. The slot
      // is the return value; retpc = -1 is a sentinel that Ret recognises as
      // "complete the frame with slot" (interpreter.gleam Ret case).
      let restored_stack = truncate_stack(gen_state.stack, stack_depth)
      let finally_state =
        State(
          ..gen_state,
          try_stack: remaining_try,
          stack: [value.from_int(-1), return_val, ..restored_stack],
          pc: fin_label,
        )
      case execute_inner(finally_state) {
        Ok(#(Completed(NormalCompletion(val)), final_state)) -> {
          // Finally completed normally. `val` is what the frame completed
          // with: the -1 sentinel makes Ret hand back the slot value, so a
          // finally that just falls off the end yields `return_val` right
          // back — but a `return x` INSIDE the finally overrides it
          // (§14.15.3: the finally's own abrupt completion wins). Keep `val`,
          // not `return_val`, and carry it into the outer finally blocks.
          let updated_gen_state =
            State(
              ..state.adopt_child(gen_state, final_state),
              try_stack: final_state.try_stack,
              stack: final_state.stack,
              locals: final_state.locals,
              eval_env: final_state.eval_env,
              current_line: final_state.current_line,
            )
          unwind_return(updated_gen_state, val, execute_inner, unwind_to_catch)
        }
        // Yield / await inside the finally block (the body suspends there),
        // a throw out of it, or a VM error: that IS the body's outcome —
        // hand it to the driver unchanged. It settles it exactly as it
        // settles the same outcome from a plain resume.
        other -> other
      }
    }
  }
}

/// §7.4.9 IteratorClose with a return completion: look up the iterator's
/// `return` method and call it. undefined/null method → keep unwinding the
/// return; a throw from the getter or the call, or a non-object call result,
/// replaces the return completion with a throw completion.
fn close_for_return(
  st: State(host),
  iter_ref: Ref,
  return_val: JsValue,
  execute_inner: ExecuteInnerFn(host),
  unwind_to_catch: UnwindToCatchFn(host),
) -> Result(#(Outcome, State(host)), state.VmError) {
  let iter = JsObject(iter_ref)
  let continue_return = fn(st: State(host)) {
    unwind_return(st, return_val, execute_inner, unwind_to_catch)
  }
  let continue_throw = fn(st: State(host), thrown: JsValue) {
    replace_return_with_throw(st, thrown, execute_inner, unwind_to_catch)
  }
  case object_ops.get_value(st, iter_ref, Named("return"), iter) {
    Ok(#(JsUndefined, st)) | Ok(#(value.JsNull, st)) -> continue_return(st)
    Ok(#(ret_fn, st)) ->
      case state.call(st, ret_fn, iter, []) {
        // §7.4.9 step 9: a non-object result from `return()` is a
        // TypeError (the return completion is replaced by a throw).
        Ok(#(JsObject(_), st)) -> continue_return(st)
        Ok(#(_non_object, st)) -> {
          let #(err, st) =
            state.type_error_value(st, "Iterator result is not an object")
          continue_throw(st, err)
        }
        Error(#(thrown, st)) -> continue_throw(st, thrown)
      }
    Error(#(thrown, st)) -> continue_throw(st, thrown)
  }
}

/// An iterator close ran during return-unwinding and threw (or produced a
/// non-object result): the return completion is replaced by a throw
/// completion (§7.4.9 steps 8-9), which unwinds through the generator's
/// REMAINING try frames — so an enclosing try/catch (or another iterator
/// close guard) inside the generator can observe it. If nothing catches,
/// the body's completion is the throw.
fn replace_return_with_throw(
  gen_state: State(host),
  thrown: JsValue,
  execute_inner: ExecuteInnerFn(host),
  unwind_to_catch: UnwindToCatchFn(host),
) -> Result(#(Outcome, State(host)), state.VmError) {
  // unwind_to_catch expects the throw site's stack; gen_state.stack is
  // already truncated to the frame base and try_stack holds the remaining
  // frames, so the unwind lands on the next handler in spec order.
  case unwind_to_catch(gen_state, thrown) {
    // The generator caught it — continue executing from the handler.
    Some(caught_state) -> execute_inner(caught_state)
    None -> Ok(#(Completed(ThrowCompletion(thrown)), gen_state))
  }
}

/// Save try-stack to serializable form for generator suspension.
pub fn save_stacks(try_stack: List(TryFrame)) -> List(value.SavedTryFrame) {
  use tf <- list.map(try_stack)
  value.SavedTryFrame(
    catch_target: tf.catch_target,
    stack_depth: tf.stack_depth,
    kind: tf.kind,
  )
}

/// Restore saved try-stack back to frame types for generator resumption.
pub fn restore_stacks(
  saved_try_stack: List(value.SavedTryFrame),
) -> List(TryFrame) {
  use stf <- list.map(saved_try_stack)
  TryFrame(
    catch_target: stf.catch_target,
    stack_depth: stf.stack_depth,
    kind: stf.kind,
  )
}

/// Truncate stack to a given depth.
fn truncate_stack(stack: List(JsValue), depth: Int) -> List(JsValue) {
  let excess = list.length(stack) - depth
  case excess > 0 {
    True -> list.drop(stack, excess)
    False -> stack
  }
}
