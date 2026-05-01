import arc/vm/builtins/common
import arc/vm/builtins/helpers
import arc/vm/completion.{
  type Completion, AwaitCompletion, NormalCompletion, ThrowCompletion,
  YieldCompletion,
}
import arc/vm/heap
import arc/vm/internal/tuple_array
import arc/vm/opcode.{type Op, Gosub, YieldStar}
import arc/vm/ops/object as object_ops
import arc/vm/state.{
  type Heap, type HeapSlot, type State, type StepResult, type TryFrame, State,
  StepVmError, Thrown, TryFrame, Unimplemented,
}
import arc/vm/value.{
  type FuncTemplate, type JsValue, type Ref, GeneratorObject, GeneratorSlot,
  JsObject, JsUndefined, Named, ObjectSlot,
}
import gleam/list
import gleam/option.{type Option, None, Some}

// ============================================================================
// Callback types for VM functions that can't be imported directly
// ============================================================================

pub type ExecuteInnerFn =
  fn(State) -> Result(#(Completion, State), state.VmError)

pub type UnwindToCatchFn =
  fn(State, JsValue) -> Option(State)

// ============================================================================
// Generator native function implementations
// ============================================================================

/// Generator.prototype.next(value) -- resume a suspended generator.
pub fn call_native_generator_next(
  state: State,
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
  execute_inner: ExecuteInnerFn,
  _unwind_to_catch: UnwindToCatchFn,
) -> Result(State, #(StepResult, JsValue, State)) {
  let next_arg = helpers.first_arg_or_undefined(args)
  case get_generator_data(state.heap, this) {
    Some(gen) ->
      case gen.gen_state {
        value.Completed -> {
          // Already done -- return {value: undefined, done: true}
          let #(h, result) =
            common.create_iter_result(
              state.heap,
              state.builtins,
              JsUndefined,
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
        value.SuspendedStart | value.SuspendedYield -> {
          // For SuspendedYield, push the .next() arg onto the saved stack
          // (the Yield opcode left pc pointing past Yield, stack has value popped)
          let gen_stack = case gen.gen_state {
            value.SuspendedYield -> [next_arg, ..gen.saved_stack]
            _ -> gen.saved_stack
          }
          let gen_exec_state =
            build_resumed_state(state, gen, gen_stack, gen.saved_pc)
          run_to_completion(
            gen_exec_state,
            state,
            gen,
            rest_stack,
            execute_inner,
          )
        }
      }
    None -> {
      state.throw_type_error(state, "not a generator object")
    }
  }
}

/// Generator.prototype.return(value) -- complete the generator with a return value.
pub fn call_native_generator_return(
  state: State,
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
  execute_inner: ExecuteInnerFn,
) -> Result(State, #(StepResult, JsValue, State)) {
  let return_val = helpers.first_arg_or_undefined(args)
  case get_generator_data(state.heap, this) {
    Some(gen) ->
      case gen.gen_state {
        value.Completed | value.SuspendedStart -> {
          // Mark completed and return {value, done: true}
          let h =
            heap.write(
              state.heap,
              gen.data_ref,
              gen_with_state(gen, value.Completed),
            )
          let #(h, result) =
            common.create_iter_result(h, state.builtins, return_val, True)
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
                "return",
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
  state: State,
  gen: GenData,
  return_val: JsValue,
  rest_stack: List(JsValue),
  execute_inner: ExecuteInnerFn,
) -> Result(State, #(StepResult, JsValue, State)) {
  let gen_exec_state =
    build_resumed_state(state, gen, gen.saved_stack, gen.saved_pc)
  process_generator_return(
    gen_exec_state,
    state,
    gen,
    return_val,
    rest_stack,
    execute_inner,
  )
}

/// Generator.prototype.throw(exception) -- throw into the generator.
pub fn call_native_generator_throw(
  state: State,
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
  execute_inner: ExecuteInnerFn,
  unwind_to_catch: UnwindToCatchFn,
) -> Result(State, #(StepResult, JsValue, State)) {
  let throw_val = helpers.first_arg_or_undefined(args)
  case get_generator_data(state.heap, this) {
    Some(gen) ->
      case gen.gen_state {
        value.Completed | value.SuspendedStart -> {
          // Mark completed and throw the exception
          let h =
            heap.write(
              state.heap,
              gen.data_ref,
              gen_with_state(gen, value.Completed),
            )
          Error(#(Thrown, throw_val, State(..state, heap: h)))
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
                "throw",
                throw_val,
                rest_stack,
                execute_inner,
                fn(state) {
                  // Inner iterator has no .throw — §27.5.3.8 step 7.b.iii:
                  // close it, then throw a TypeError.
                  let state = close_iterator(state, iter_ref)
                  let h =
                    heap.write(
                      state.heap,
                      gen.data_ref,
                      gen_with_state(gen, value.Completed),
                    )
                  state.throw_type_error(
                    State(..state, heap: h),
                    "The iterator does not provide a 'throw' method.",
                  )
                },
              )
            None -> {
              let gen_exec_state =
                build_resumed_state(state, gen, gen.saved_stack, gen.saved_pc)
              // Try to unwind to a catch handler within the generator
              case unwind_to_catch(gen_exec_state, throw_val) {
                Some(caught_state) ->
                  // The generator caught it -- continue executing
                  run_to_completion(
                    caught_state,
                    state,
                    gen,
                    rest_stack,
                    execute_inner,
                  )
                None -> {
                  // No catch handler -- mark completed and propagate the throw
                  let h2 =
                    heap.write(
                      gen_exec_state.heap,
                      gen.data_ref,
                      gen_with_state(gen, value.Completed),
                    )
                  Error(#(Thrown, throw_val, State(..state, heap: h2)))
                }
              }
            }
          }
      }
    None -> {
      state.throw_type_error(state, "not a generator object")
    }
  }
}

/// Best-effort iterator close — call .return() if present, swallow errors.
fn close_iterator(state: State, iter_ref: Ref) -> State {
  let iter = JsObject(iter_ref)
  case object_ops.get_value(state, iter_ref, Named("return"), iter) {
    Ok(#(JsUndefined, state)) | Ok(#(value.JsNull, state)) -> state
    Ok(#(ret_fn, state)) ->
      case state.call(state, ret_fn, iter, []) {
        Ok(#(_, state)) -> state
        Error(#(_, state)) -> state
      }
    Error(#(_, state)) -> state
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
    saved_this: JsValue,
    saved_callee_ref: option.Option(Ref),
  )
}

fn get_generator_data(h: Heap, this: JsValue) -> Option(GenData) {
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
              saved_this:,
              saved_callee_ref:,
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
                saved_this:,
                saved_callee_ref:,
              ))
            _ -> None
          }
        _ -> None
      }
    _ -> None
  }
}

/// Create a GeneratorSlot with only the gen_state changed.
fn gen_with_state(gen: GenData, new_state: value.GeneratorState) -> HeapSlot {
  GeneratorSlot(
    gen_state: new_state,
    func_template: gen.func_template,
    env_ref: gen.env_ref,
    saved_pc: gen.saved_pc,
    saved_locals: gen.saved_locals,
    saved_stack: gen.saved_stack,
    saved_try_stack: gen.saved_try_stack,
    saved_this: gen.saved_this,
    saved_callee_ref: gen.saved_callee_ref,
  )
}

/// Mark a generator Executing and restore its saved execution context into a
/// fresh State for resumption. `stack` and `pc` vary per resume mode.
fn build_resumed_state(
  outer: State,
  gen: GenData,
  stack: List(JsValue),
  pc: Int,
) -> State {
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
    this_binding: gen.saved_this,
    callee_ref: gen.saved_callee_ref,
    call_args: [],
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
  state: State,
  gen: GenData,
  iter_ref: Ref,
  method: String,
  arg: JsValue,
  rest_stack: List(JsValue),
  execute_inner: ExecuteInnerFn,
  on_missing: fn(State) -> Result(State, #(StepResult, JsValue, State)),
) -> Result(State, #(StepResult, JsValue, State)) {
  let iter = JsObject(iter_ref)
  case object_ops.get_value(state, iter_ref, Named(method), iter) {
    Error(#(thrown, state)) -> {
      let h =
        heap.write(
          state.heap,
          gen.data_ref,
          gen_with_state(gen, value.Completed),
        )
      Error(#(Thrown, thrown, State(..state, heap: h)))
    }
    Ok(#(JsUndefined, state)) | Ok(#(value.JsNull, state)) -> on_missing(state)
    Ok(#(method_fn, state)) ->
      case state.call(state, method_fn, iter, [arg]) {
        Error(#(thrown, state)) -> {
          let h =
            heap.write(
              state.heap,
              gen.data_ref,
              gen_with_state(gen, value.Completed),
            )
          Error(#(Thrown, thrown, State(..state, heap: h)))
        }
        Ok(#(JsObject(rref) as res, state)) -> {
          let done = case
            object_ops.get_value(state, rref, Named("done"), res)
          {
            Ok(#(d, _)) -> value.is_truthy(d)
            Error(_) -> False
          }
          let #(val, state) = case
            object_ops.get_value(state, rref, Named("value"), res)
          {
            Ok(#(v, s)) -> #(v, s)
            Error(#(_, s)) -> #(JsUndefined, s)
          }
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
        Ok(#(_non_obj, state)) -> {
          let h =
            heap.write(
              state.heap,
              gen.data_ref,
              gen_with_state(gen, value.Completed),
            )
          state.throw_type_error(
            State(..state, heap: h),
            "Iterator result is not an object",
          )
        }
      }
  }
}

/// Delegated iterator returned {done:true}. For .throw, resume the generator
/// body normally past YieldStar with result.value on stack. For .return, the
/// outer generator must ALSO return — complete it with that value.
fn resume_after_delegate(
  state: State,
  gen: GenData,
  val: JsValue,
  method: String,
  rest_stack: List(JsValue),
  execute_inner: ExecuteInnerFn,
) -> Result(State, #(StepResult, JsValue, State)) {
  case method {
    "return" -> {
      // §27.5.3.8 step 7.c.viii: if the inner iterator's return completed,
      // perform a return completion on the outer generator too.
      let h =
        heap.write(
          state.heap,
          gen.data_ref,
          gen_with_state(gen, value.Completed),
        )
      let #(h, result) = common.create_iter_result(h, state.builtins, val, True)
      Ok(
        State(..state, heap: h, stack: [result, ..rest_stack], pc: state.pc + 1),
      )
    }
    _ -> {
      // .throw forwarded and inner is done — continue outer body past
      // YieldStar with val on stack (the yield* expression's value).
      let stack_after = case gen.saved_stack {
        [_iter, ..rest] -> [val, ..rest]
        _ -> [val]
      }
      let resumed =
        build_resumed_state(state, gen, stack_after, gen.saved_pc + 1)
      run_to_completion(resumed, state, gen, rest_stack, execute_inner)
    }
  }
}

/// Run a resumed generator to its next suspension/completion and marshal the
/// result back to the caller. Shared tail for delegate-forward continuations.
fn run_to_completion(
  resumed: State,
  outer: State,
  gen: GenData,
  rest_stack: List(JsValue),
  execute_inner: ExecuteInnerFn,
) -> Result(State, #(StepResult, JsValue, State)) {
  case execute_inner(resumed) {
    Ok(#(YieldCompletion(yv, h), suspended)) -> {
      let st = save_stacks(suspended.try_stack)
      let h =
        heap.write(
          h,
          gen.data_ref,
          GeneratorSlot(
            gen_state: value.SuspendedYield,
            func_template: gen.func_template,
            env_ref: gen.env_ref,
            saved_pc: suspended.pc,
            saved_locals: suspended.locals,
            saved_stack: suspended.stack,
            saved_try_stack: st,
            saved_this: suspended.this_binding,
            saved_callee_ref: suspended.callee_ref,
          ),
        )
      let #(h, result) = common.create_iter_result(h, outer.builtins, yv, False)
      Ok(
        State(
          ..state.merge_globals(outer, suspended, []),
          heap: h,
          stack: [result, ..rest_stack],
          pc: outer.pc + 1,
        ),
      )
    }
    Ok(#(NormalCompletion(rv, h), final_state)) -> {
      let h = heap.write(h, gen.data_ref, gen_with_state(gen, value.Completed))
      let #(h, result) = common.create_iter_result(h, outer.builtins, rv, True)
      Ok(
        State(
          ..state.merge_globals(outer, final_state, []),
          heap: h,
          stack: [result, ..rest_stack],
          pc: outer.pc + 1,
        ),
      )
    }
    Ok(#(ThrowCompletion(thrown, h), _)) -> {
      let h = heap.write(h, gen.data_ref, gen_with_state(gen, value.Completed))
      Error(#(Thrown, thrown, State(..outer, heap: h)))
    }
    Ok(#(AwaitCompletion(_, _), _)) ->
      Error(#(
        StepVmError(Unimplemented("await in sync generator")),
        JsUndefined,
        outer,
      ))
    Error(vm_err) -> {
      let h =
        heap.write(
          outer.heap,
          gen.data_ref,
          gen_with_state(gen, value.Completed),
        )
      Error(#(StepVmError(vm_err), JsUndefined, State(..outer, heap: h)))
    }
  }
}

/// Walk the try_stack, skipping catch-only / for-of entries, looking for the
/// first try/finally handler. Since the gosub/ret rewrite (wave 3) the throw
/// entry of a try/finally is `Gosub(fin_label); Throw`, so a finally-owning
/// frame is identified by `Gosub` at its catch_target. Returns
/// Some(#(fin_label, stack_depth, remaining_try_stack)) or None.
fn find_next_finally(
  code: tuple_array.TupleArray(Op),
  try_stack: List(TryFrame),
) -> Option(#(Int, Int, List(TryFrame))) {
  case try_stack {
    [] -> None
    [TryFrame(catch_target:, stack_depth:), ..rest] ->
      // catch_target is a compiler-resolved label PC — always in bounds.
      case tuple_array.unsafe_get(catch_target, code) {
        Gosub(fin_label) -> Some(#(fin_label, stack_depth, rest))
        _ -> find_next_finally(code, rest)
      }
  }
}

/// Process generator.return(val) by unwinding through any enclosing finally blocks.
/// This runs each finally block in order (innermost to outermost) and handles:
/// - Normal completion: continue to next finally, or mark completed
/// - Yield inside finally: save generator state, return {value, done: false}
/// - Throw inside finally: mark completed, propagate the throw
fn process_generator_return(
  gen_state: State,
  outer_state: State,
  gen: GenData,
  return_val: JsValue,
  rest_stack: List(JsValue),
  execute_inner: ExecuteInnerFn,
) -> Result(State, #(StepResult, JsValue, State)) {
  case find_next_finally(gen_state.code, gen_state.try_stack) {
    None -> {
      // No more finally blocks. Mark completed and return {value, done: true}.
      let h =
        heap.write(
          gen_state.heap,
          gen.data_ref,
          gen_with_state(gen, value.Completed),
        )
      let #(h, result) =
        common.create_iter_result(h, outer_state.builtins, return_val, True)
      Ok(
        State(
          ..outer_state,
          heap: h,
          stack: [result, ..rest_stack],
          pc: outer_state.pc + 1,
          lexical_globals: gen_state.lexical_globals,
          const_lexical_globals: gen_state.const_lexical_globals,
          job_queue: gen_state.job_queue,
        ),
      )
    }
    Some(#(fin_label, stack_depth, remaining_try)) -> {
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
        Ok(#(NormalCompletion(_val, h2), final_state)) -> {
          // Finally completed normally — Ret hit the -1 sentinel → Done(return_val).
          // Continue processing any remaining outer finally blocks.
          let updated_gen_state =
            State(
              ..state.merge_globals(gen_state, final_state, []),
              heap: h2,
              try_stack: final_state.try_stack,
              stack: final_state.stack,
              locals: final_state.locals,
            )
          process_generator_return(
            updated_gen_state,
            outer_state,
            gen,
            return_val,
            rest_stack,
            execute_inner,
          )
        }
        Ok(#(YieldCompletion(yielded_value, h2), suspended)) -> {
          // Generator yielded from inside the finally block.
          // Save state so next .next() resumes inside the finally.
          let saved_try2 = save_stacks(suspended.try_stack)
          let h3 =
            heap.write(
              h2,
              gen.data_ref,
              GeneratorSlot(
                gen_state: value.SuspendedYield,
                func_template: gen.func_template,
                env_ref: gen.env_ref,
                saved_pc: suspended.pc,
                saved_locals: suspended.locals,
                saved_stack: suspended.stack,
                saved_try_stack: saved_try2,
                saved_this: suspended.this_binding,
                saved_callee_ref: suspended.callee_ref,
              ),
            )
          let #(h3, result) =
            common.create_iter_result(
              h3,
              outer_state.builtins,
              yielded_value,
              False,
            )
          Ok(
            State(
              ..state.merge_globals(outer_state, suspended, []),
              heap: h3,
              stack: [result, ..rest_stack],
              pc: outer_state.pc + 1,
            ),
          )
        }
        Ok(#(ThrowCompletion(thrown, h2), _suspended)) -> {
          // Finally block threw. Mark completed and propagate the throw.
          let h3 =
            heap.write(h2, gen.data_ref, gen_with_state(gen, value.Completed))
          Error(#(Thrown, thrown, State(..outer_state, heap: h3)))
        }
        Ok(#(AwaitCompletion(_, _), _)) ->
          Error(#(
            StepVmError(Unimplemented("await in sync generator")),
            JsUndefined,
            gen_state,
          ))
        Error(vm_err) -> {
          let h2 =
            heap.write(
              gen_state.heap,
              gen.data_ref,
              gen_with_state(gen, value.Completed),
            )
          Error(#(
            StepVmError(vm_err),
            JsUndefined,
            State(..gen_state, heap: h2),
          ))
        }
      }
    }
  }
}

/// Save try-stack to serializable form for generator suspension.
pub fn save_stacks(try_stack: List(TryFrame)) -> List(value.SavedTryFrame) {
  use tf <- list.map(try_stack)
  value.SavedTryFrame(
    catch_target: tf.catch_target,
    stack_depth: tf.stack_depth,
  )
}

/// Restore saved try-stack back to frame types for generator resumption.
pub fn restore_stacks(
  saved_try_stack: List(value.SavedTryFrame),
) -> List(TryFrame) {
  use stf <- list.map(saved_try_stack)
  TryFrame(catch_target: stf.catch_target, stack_depth: stf.stack_depth)
}

/// Truncate stack to a given depth.
fn truncate_stack(stack: List(JsValue), depth: Int) -> List(JsValue) {
  let excess = list.length(stack) - depth
  case excess > 0 {
    True -> list.drop(stack, excess)
    False -> stack
  }
}
