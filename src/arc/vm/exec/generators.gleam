import arc/vm/builtins/common.{type Builtins}
import arc/vm/completion.{
  type Completion, AwaitCompletion, NormalCompletion, ThrowCompletion,
  YieldCompletion,
}
import arc/vm/heap.{type Heap}
import arc/vm/internal/elements
import arc/vm/internal/tuple_array
import arc/vm/opcode.{type Op, EnterFinallyThrow}
import arc/vm/state.{
  type FinallyCompletion, type State, type StepResult, type TryFrame, State,
  StepVmError, Thrown, TryFrame, Unimplemented,
}
import arc/vm/value.{
  type FuncTemplate, type JsValue, type Ref, GeneratorObject, GeneratorSlot,
  JsBool, JsObject, JsUndefined, Named, ObjectSlot, OrdinaryObject,
}
import gleam/dict
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
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let next_arg = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  case get_generator_data(state.heap, this) {
    Some(gen) ->
      case gen.gen_state {
        value.Completed -> {
          // Already done -- return {value: undefined, done: true}
          let #(h, result) =
            create_iterator_result(
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
          // Mark as executing
          let h =
            heap.write(
              state.heap,
              gen.data_ref,
              gen_with_state(gen, value.Executing),
            )
          // Restore the generator's execution state
          let #(restored_try, restored_finally) =
            restore_stacks(gen.saved_try_stack, gen.saved_finally_stack)
          // For SuspendedYield, push the .next() arg onto the saved stack
          // (the Yield opcode left pc pointing past Yield, stack has value popped)
          let gen_stack = case gen.gen_state {
            value.SuspendedYield -> [next_arg, ..gen.saved_stack]
            _ -> gen.saved_stack
          }
          let gen_exec_state =
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
              // arguments was created before InitialYield; post-resume never needs call_args
              call_args: [],
            )
          // Execute until yield/return/throw
          case execute_inner(gen_exec_state) {
            Ok(#(YieldCompletion(yielded_value, h2), suspended)) -> {
              // Generator yielded -- save state back
              let #(saved_try2, saved_finally2) =
                save_stacks(suspended.try_stack, suspended.finally_stack)
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
                    saved_finally_stack: saved_finally2,
                    saved_this: suspended.this_binding,
                    saved_callee_ref: suspended.callee_ref,
                  ),
                )
              let #(h3, result) =
                create_iterator_result(h3, state.builtins, yielded_value, False)
              Ok(
                State(
                  ..state.merge_globals(state, suspended, []),
                  heap: h3,
                  stack: [result, ..rest_stack],
                  pc: state.pc + 1,
                ),
              )
            }
            Ok(#(NormalCompletion(return_value, h2), final_state)) -> {
              // Generator returned -- mark completed
              let h3 =
                heap.write(
                  h2,
                  gen.data_ref,
                  gen_with_state(gen, value.Completed),
                )
              let #(h3, result) =
                create_iterator_result(h3, state.builtins, return_value, True)
              Ok(
                State(
                  ..state.merge_globals(state, final_state, []),
                  heap: h3,
                  stack: [result, ..rest_stack],
                  pc: state.pc + 1,
                ),
              )
            }
            Ok(#(ThrowCompletion(thrown, h2), _final_state)) -> {
              // Generator threw -- mark completed and propagate
              let h3 =
                heap.write(
                  h2,
                  gen.data_ref,
                  gen_with_state(gen, value.Completed),
                )
              Error(#(Thrown, thrown, h3))
            }
            Ok(#(AwaitCompletion(_, _), _)) ->
              Error(#(
                StepVmError(Unimplemented("await in sync generator")),
                JsUndefined,
                state.heap,
              ))
            Error(_vm_err) -> {
              let h2 =
                heap.write(
                  state.heap,
                  gen.data_ref,
                  gen_with_state(gen, value.Completed),
                )
              Error(#(
                StepVmError(Unimplemented("generator execution failed")),
                JsUndefined,
                h2,
              ))
            }
          }
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
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let return_val = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
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
            create_iterator_result(h, state.builtins, return_val, True)
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
        value.SuspendedYield -> {
          // Full spec: resume with return completion so finally blocks run.
          // Mark as executing, restore generator state, then process through
          // any enclosing finally blocks before completing.
          let h =
            heap.write(
              state.heap,
              gen.data_ref,
              gen_with_state(gen, value.Executing),
            )
          // Restore the generator's execution state
          let #(restored_try, restored_finally) =
            restore_stacks(gen.saved_try_stack, gen.saved_finally_stack)
          let gen_exec_state =
            State(
              ..state,
              heap: h,
              stack: gen.saved_stack,
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
          // Process through any enclosing finally blocks, then complete.
          process_generator_return(
            gen_exec_state,
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

/// Generator.prototype.throw(exception) -- throw into the generator.
pub fn call_native_generator_throw(
  state: State,
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
  execute_inner: ExecuteInnerFn,
  unwind_to_catch: UnwindToCatchFn,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let throw_val = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
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
          Error(#(Thrown, throw_val, h))
        }
        value.Executing -> {
          state.throw_type_error(state, "Generator is already running")
        }
        value.SuspendedYield -> {
          // Mark as executing
          let h =
            heap.write(
              state.heap,
              gen.data_ref,
              gen_with_state(gen, value.Executing),
            )
          // Restore the generator's execution state
          let #(restored_try, restored_finally) =
            restore_stacks(gen.saved_try_stack, gen.saved_finally_stack)
          let gen_exec_state =
            State(
              ..state,
              heap: h,
              stack: gen.saved_stack,
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
          // Try to unwind to a catch handler within the generator
          case unwind_to_catch(gen_exec_state, throw_val) {
            Some(caught_state) ->
              // The generator caught it -- continue executing
              case execute_inner(caught_state) {
                Ok(#(YieldCompletion(yielded_value, h2), suspended)) -> {
                  let #(saved_try2, saved_finally2) =
                    save_stacks(suspended.try_stack, suspended.finally_stack)
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
                        saved_finally_stack: saved_finally2,
                        saved_this: suspended.this_binding,
                        saved_callee_ref: suspended.callee_ref,
                      ),
                    )
                  let #(h3, result) =
                    create_iterator_result(
                      h3,
                      state.builtins,
                      yielded_value,
                      False,
                    )
                  Ok(
                    State(
                      ..state.merge_globals(state, suspended, []),
                      heap: h3,
                      stack: [result, ..rest_stack],
                      pc: state.pc + 1,
                    ),
                  )
                }
                Ok(#(NormalCompletion(return_value, h2), final_state)) -> {
                  let h3 =
                    heap.write(
                      h2,
                      gen.data_ref,
                      gen_with_state(gen, value.Completed),
                    )
                  let #(h3, result) =
                    create_iterator_result(
                      h3,
                      state.builtins,
                      return_value,
                      True,
                    )
                  Ok(
                    State(
                      ..state.merge_globals(state, final_state, []),
                      heap: h3,
                      stack: [result, ..rest_stack],
                      pc: state.pc + 1,
                    ),
                  )
                }
                Ok(#(ThrowCompletion(thrown, h2), _final_state)) -> {
                  let h3 =
                    heap.write(
                      h2,
                      gen.data_ref,
                      gen_with_state(gen, value.Completed),
                    )
                  Error(#(Thrown, thrown, h3))
                }
                Ok(#(AwaitCompletion(_, _), _)) ->
                  Error(#(
                    StepVmError(Unimplemented("await in sync generator")),
                    JsUndefined,
                    state.heap,
                  ))
                Error(_vm_err) -> {
                  let h2 =
                    heap.write(
                      state.heap,
                      gen.data_ref,
                      gen_with_state(gen, value.Completed),
                    )
                  Error(#(
                    StepVmError(Unimplemented(
                      "generator throw execution failed",
                    )),
                    JsUndefined,
                    h2,
                  ))
                }
              }
            None -> {
              // No catch handler -- mark completed and propagate the throw
              let h2 =
                heap.write(
                  h,
                  gen.data_ref,
                  gen_with_state(gen, value.Completed),
                )
              Error(#(Thrown, throw_val, h2))
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
    saved_locals: tuple_array.Array(JsValue),
    saved_stack: List(JsValue),
    saved_try_stack: List(value.SavedTryFrame),
    saved_finally_stack: List(value.SavedFinallyCompletion),
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
              saved_finally_stack:,
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
                saved_finally_stack:,
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
fn gen_with_state(
  gen: GenData,
  new_state: value.GeneratorState,
) -> value.HeapSlot {
  GeneratorSlot(
    gen_state: new_state,
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

/// Walk the try_stack, skipping catch-only entries, looking for the first
/// try/finally handler (identified by EnterFinallyThrow at catch_target).
/// Returns Some(#(catch_target, stack_depth, remaining_try_stack)) or None.
fn find_next_finally(
  code: tuple_array.Array(Op),
  try_stack: List(TryFrame),
) -> Option(#(Int, Int, List(TryFrame))) {
  case try_stack {
    [] -> None
    [TryFrame(catch_target:, stack_depth:), ..rest] ->
      case tuple_array.get(catch_target, code) {
        Some(EnterFinallyThrow) -> Some(#(catch_target, stack_depth, rest))
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
) -> Result(State, #(StepResult, JsValue, Heap)) {
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
        create_iterator_result(h, outer_state.builtins, return_val, True)
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
    Some(#(catch_target, stack_depth, remaining_try)) -> {
      // Found a finally handler. Set up state to execute the finally body.
      // Skip EnterFinallyThrow (at catch_target), jump to catch_target + 1.
      // Push ReturnCompletion onto finally_stack so LeaveFinally knows to return.
      let restored_stack = truncate_stack(gen_state.stack, stack_depth)
      let finally_state =
        State(
          ..gen_state,
          try_stack: remaining_try,
          stack: restored_stack,
          finally_stack: [
            state.ReturnCompletion(return_val),
            ..gen_state.finally_stack
          ],
          pc: catch_target + 1,
        )
      case execute_inner(finally_state) {
        Ok(#(NormalCompletion(_val, h2), final_state)) -> {
          // Finally completed normally. LeaveFinally saw ReturnCompletion -> Done.
          // Continue processing any remaining outer finally blocks.
          let updated_gen_state =
            State(
              ..state.merge_globals(gen_state, final_state, []),
              heap: h2,
              try_stack: final_state.try_stack,
              finally_stack: final_state.finally_stack,
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
          let #(saved_try2, saved_finally2) =
            save_stacks(suspended.try_stack, suspended.finally_stack)
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
                saved_finally_stack: saved_finally2,
                saved_this: suspended.this_binding,
                saved_callee_ref: suspended.callee_ref,
              ),
            )
          let #(h3, result) =
            create_iterator_result(
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
          Error(#(Thrown, thrown, h3))
        }
        Ok(#(AwaitCompletion(_, _), _)) ->
          Error(#(
            StepVmError(Unimplemented("await in sync generator")),
            JsUndefined,
            gen_state.heap,
          ))
        Error(vm_err) -> {
          let h2 =
            heap.write(
              gen_state.heap,
              gen.data_ref,
              gen_with_state(gen, value.Completed),
            )
          Error(#(StepVmError(vm_err), JsUndefined, h2))
        }
      }
    }
  }
}

/// Create a {value: val, done: bool} iterator result object.
pub fn create_iterator_result(
  h: Heap,
  builtins: Builtins,
  val: JsValue,
  done: Bool,
) -> #(Heap, JsValue) {
  let #(h, ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: OrdinaryObject,
        properties: dict.from_list([
          #(Named("value"), value.data_property(val)),
          #(Named("done"), value.data_property(JsBool(done))),
        ]),
        elements: elements.new(),
        prototype: Some(builtins.object.prototype),
        symbol_properties: dict.new(),
        extensible: True,
      ),
    )
  #(h, JsObject(ref))
}

/// Save try/finally stacks to serializable form for generator suspension.
pub fn save_stacks(
  try_stack: List(TryFrame),
  finally_stack: List(FinallyCompletion),
) -> #(List(value.SavedTryFrame), List(value.SavedFinallyCompletion)) {
  let saved_try =
    list.map(try_stack, fn(tf) {
      value.SavedTryFrame(
        catch_target: tf.catch_target,
        stack_depth: tf.stack_depth,
      )
    })
  let saved_finally = list.map(finally_stack, convert_finally_completion)
  #(saved_try, saved_finally)
}

/// Restore saved try/finally stacks back to frame types for generator resumption.
pub fn restore_stacks(
  saved_try_stack: List(value.SavedTryFrame),
  saved_finally_stack: List(value.SavedFinallyCompletion),
) -> #(List(TryFrame), List(FinallyCompletion)) {
  let restored_try =
    list.map(saved_try_stack, fn(stf) {
      TryFrame(catch_target: stf.catch_target, stack_depth: stf.stack_depth)
    })
  let restored_finally =
    list.map(saved_finally_stack, restore_finally_completion)
  #(restored_try, restored_finally)
}

/// Convert state.FinallyCompletion to value.SavedFinallyCompletion for storage.
fn convert_finally_completion(
  fc: FinallyCompletion,
) -> value.SavedFinallyCompletion {
  case fc {
    state.NormalCompletion -> value.SavedNormalCompletion
    state.ThrowCompletion(v) -> value.SavedThrowCompletion(v)
    state.ReturnCompletion(v) -> value.SavedReturnCompletion(v)
  }
}

/// Convert value.SavedFinallyCompletion back to state.FinallyCompletion.
fn restore_finally_completion(
  sfc: value.SavedFinallyCompletion,
) -> FinallyCompletion {
  case sfc {
    value.SavedNormalCompletion -> state.NormalCompletion
    value.SavedThrowCompletion(v) -> state.ThrowCompletion(v)
    value.SavedReturnCompletion(v) -> state.ReturnCompletion(v)
  }
}

/// Truncate stack to a given depth.
fn truncate_stack(stack: List(JsValue), depth: Int) -> List(JsValue) {
  case list.length(stack) > depth {
    True -> truncate_stack(list.drop(stack, 1), depth)
    False -> stack
  }
}
