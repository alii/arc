import arc/vm/array
import arc/vm/array_ops
import arc/vm/builtins/common.{type Builtins}
import arc/vm/builtins/regexp as builtins_regexp
import arc/vm/call
import arc/vm/coerce
import arc/vm/completion.{
  type Completion, NormalCompletion, ThrowCompletion, YieldCompletion,
}
import arc/vm/event_loop
import arc/vm/frame.{
  type State, type StepResult, type VmError, Done, LocalIndexOutOfBounds,
  SavedFrame, StackUnderflow, State, StepVmError, Thrown, TryFrame,
  Unimplemented, Yielded,
}
import arc/vm/generators
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/object
import arc/vm/opcode.{
  type Op, Add, ArrayFrom, ArrayFromWithHoles, ArrayPush, ArrayPushHole,
  ArraySpread, Await, BinOp, BoxLocal, Call, CallApply, CallConstructor,
  CallConstructorApply, CallMethod, CallMethodApply, CallSuper, CreateArguments,
  DeclareGlobalLex, DeclareGlobalVar, DefineAccessor, DefineAccessorComputed,
  DefineField, DefineFieldComputed, DefineMethod, DeleteElem, DeleteField, Dup,
  ForInNext, ForInStart, GetBoxed, GetElem, GetElem2, GetField, GetField2,
  GetGlobal, GetIterator, GetLocal, GetThis, InitGlobalLex, InitialYield,
  IteratorClose, IteratorNext, Jump, JumpIfFalse, JumpIfNullish, JumpIfTrue,
  MakeClosure, NewObject, NewRegExp, ObjectSpread, Pop, PushConst, PushTry,
  PutBoxed, PutElem, PutField, PutGlobal, PutLocal, Return, SetupDerivedClass,
  Swap, TypeOf, TypeofGlobal, UnaryOp, Yield,
}
import arc/vm/operators
import arc/vm/property_access
import arc/vm/value.{
  type FuncTemplate, type JsValue, type Ref, ArrayIteratorSlot, ArrayObject,
  DataProperty, Finite, ForInIteratorSlot, FunctionObject, GeneratorObject,
  JsBool, JsNull, JsNumber, JsObject, JsString, JsUndefined, JsUninitialized,
  NativeFunction, ObjectSlot, OrdinaryObject,
}
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set
import gleam/string

// ============================================================================
// Internal state (types defined in frame.gleam for cross-module access)
// ============================================================================

/// The js_to_string callback that gets stored in State.
/// Delegates to coerce.js_to_string for ToPrimitive + ToString.
fn js_to_string_callback(
  state: State,
  val: JsValue,
) -> Result(#(String, State), #(JsValue, State)) {
  coerce.js_to_string(state, val)
}

/// The call_fn callback that gets stored in State.
/// Delegates to event_loop.run_handler_with_this for re-entrant JS function calls
/// from native code (e.g. Array.prototype.map's callback invocation).
fn call_fn_callback(
  state: State,
  callee: JsValue,
  this_val: JsValue,
  args: List(JsValue),
) -> Result(#(JsValue, State), #(JsValue, State)) {
  event_loop.run_handler_with_this(
    state,
    callee,
    this_val,
    args,
    execute_inner,
    call_native,
  )
}

/// Create a fresh VM state from a function template.
/// Most callers can use this directly; override fields with `State(..new_state(...), ...)`
/// for cases that need non-default this_binding or symbol_descriptions.
pub fn new_state(
  func: FuncTemplate,
  locals: array.Array(JsValue),
  heap: Heap,
  builtins: Builtins,
  global_object: Ref,
  lexical_globals: dict.Dict(String, JsValue),
  const_lexical_globals: set.Set(String),
  symbol_descriptions: dict.Dict(value.SymbolId, String),
  symbol_registry: dict.Dict(String, value.SymbolId),
  event_loop: Bool,
) -> State {
  State(
    stack: [],
    locals:,
    constants: func.constants,
    lexical_globals:,
    const_lexical_globals:,
    global_object:,
    func:,
    code: func.bytecode,
    heap:,
    pc: 0,
    call_stack: [],
    try_stack: [],
    finally_stack: [],
    builtins:,
    this_binding: JsUndefined,
    callee_ref: None,
    call_args: [],
    job_queue: [],
    unhandled_rejections: [],
    pending_receivers: [],
    outstanding: 0,
    symbol_descriptions:,
    symbol_registry:,
    realms: dict.new(),
    js_to_string: js_to_string_callback,
    call_fn: call_fn_callback,
    call_depth: 0,
    event_loop:,
  )
}

pub fn init_state(
  func: FuncTemplate,
  heap: Heap,
  builtins: Builtins,
  global_object: Ref,
  is_module: Bool,
  event_loop: Bool,
) -> State {
  let locals = array.repeat(JsUndefined, func.local_count)
  // ES §16.2.1.5.2 ModuleEvaluation: module `this` is undefined.
  // ES §16.1.6 ScriptEvaluation: the script's this is the global object,
  // regardless of strict mode. (Strict only affects function-body this.)
  let this_binding = case is_module {
    True -> JsUndefined
    False -> JsObject(global_object)
  }
  State(
    ..new_state(
      func,
      locals,
      heap,
      builtins,
      global_object,
      dict.new(),
      set.new(),
      dict.new(),
      dict.new(),
      event_loop,
    ),
    this_binding:,
  )
}

// ============================================================================
// Execution loop
// ============================================================================

/// Main execution loop. Tail-recursive.
/// Returns the completion and the final state (for job queue access).
pub fn execute_inner(state: State) -> Result(#(Completion, State), VmError) {
  case array.get(state.pc, state.code) {
    None -> {
      // Reached end of bytecode — return top of stack or undefined
      case state.stack {
        [top, ..] -> Ok(#(NormalCompletion(top, state.heap), state))
        [] -> Ok(#(NormalCompletion(JsUndefined, state.heap), state))
      }
    }
    Some(op) -> {
      case step(state, op) {
        Ok(new_state) -> execute_inner(new_state)
        Error(#(Done, result, heap)) ->
          Ok(#(NormalCompletion(result, heap), State(..state, heap:)))
        Error(#(StepVmError(err), _, _)) -> Error(err)
        Error(#(Yielded, yielded_value, heap)) -> {
          // Generator yielded or async awaited — build suspended state.
          // For Yield/Await: pop the yielded value from stack, advance pc.
          // For InitialYield: stack unchanged, just advance pc.
          let suspended_state = case op {
            Yield | Await ->
              State(
                ..state,
                heap:,
                stack: case state.stack {
                  [_, ..rest] -> rest
                  [] -> []
                },
                pc: state.pc + 1,
              )
            _ -> State(..state, heap:, pc: state.pc + 1)
          }
          Ok(#(YieldCompletion(yielded_value, heap), suspended_state))
        }
        Error(#(Thrown, thrown_value, heap)) -> {
          // Try to unwind to a catch handler
          let updated_state = State(..state, heap:)
          case unwind_to_catch(updated_state, thrown_value) {
            Some(caught_state) -> execute_inner(caught_state)
            None ->
              Ok(#(ThrowCompletion(thrown_value, heap), State(..state, heap:)))
          }
        }
      }
    }
  }
}

/// Wrapper that discards the state for backward compatibility.
/// Try to find a catch handler on the try_stack.
/// If found: restore stack to saved depth, push thrown value, jump to catch_target.
/// If not found: return Error(Nil) → uncaught exception.
fn unwind_to_catch(state: State, thrown_value: JsValue) -> Option(State) {
  case state.try_stack {
    [] -> None
    [TryFrame(catch_target:, stack_depth:), ..rest_try] -> {
      let restored_stack = truncate_stack(state.stack, stack_depth)
      Some(
        State(
          ..state,
          stack: [thrown_value, ..restored_stack],
          try_stack: rest_try,
          pc: catch_target,
        ),
      )
    }
  }
}

/// Truncate stack to a given depth.
fn truncate_stack(stack: List(JsValue), depth: Int) -> List(JsValue) {
  case list.length(stack) > depth {
    True -> truncate_stack(list.drop(stack, 1), depth)
    False -> stack
  }
}

/// Pop top of stack and jump to `target` if `condition(value)` is true,
/// otherwise advance to next instruction.
fn conditional_jump(
  state: State,
  target: Int,
  condition: fn(JsValue) -> Bool,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case state.stack {
    [top, ..rest] ->
      case condition(top) {
        True -> Ok(State(..state, stack: rest, pc: target))
        False -> Ok(State(..state, stack: rest, pc: state.pc + 1))
      }
    [] ->
      Error(#(
        StepVmError(StackUnderflow("ConditionalJump")),
        JsUndefined,
        state.heap,
      ))
  }
}

// ============================================================================
// Step — single instruction dispatch
// ============================================================================

/// Execute a single instruction. Returns Ok(new_state) to continue,
/// or Error(#(signal, value, heap)) to stop.
fn step(state: State, op: Op) -> Result(State, #(StepResult, JsValue, Heap)) {
  case op {
    // Stack operations
    PushConst(_) | Pop | Dup | Swap -> step_stack(state, op)

    // Local variable access
    GetLocal(_) | PutLocal(_) | BoxLocal(_) | GetBoxed(_) | PutBoxed(_) ->
      step_locals(state, op)

    // Global variable access
    GetGlobal(_)
    | PutGlobal(_)
    | DeclareGlobalVar(_)
    | DeclareGlobalLex(_, _)
    | InitGlobalLex(_)
    | TypeOf
    | TypeofGlobal(_) -> step_globals(state, op)

    // Operators
    BinOp(_) | UnaryOp(_) -> step_operators(state, op)

    // Control flow
    Return
    | Jump(_)
    | JumpIfFalse(_)
    | JumpIfTrue(_)
    | JumpIfNullish(_)
    | PushTry(_)
    | opcode.PopTry
    | opcode.Throw
    | opcode.EnterFinally
    | opcode.EnterFinallyThrow
    | opcode.LeaveFinally -> step_control_flow(state, op)

    // Object property access
    NewObject
    | GetField(_)
    | GetField2(_)
    | PutField(_)
    | DefineField(_)
    | DefineMethod(_)
    | DefineAccessor(_, _)
    | DefineAccessorComputed(_)
    | DefineFieldComputed
    | ObjectSpread
    | DeleteField(_)
    | DeleteElem
    | SetupDerivedClass
    | GetThis -> step_objects(state, op)

    // Array operations
    ArrayFrom(_)
    | ArrayFromWithHoles(_, _)
    | GetElem
    | GetElem2
    | PutElem
    | ArrayPush
    | ArrayPushHole
    | ArraySpread -> step_arrays(state, op)

    // Function calls
    Call(_)
    | CallMethod(_, _)
    | CallConstructor(_)
    | CallSuper(_)
    | CallApply
    | CallMethodApply
    | CallConstructorApply
    | MakeClosure(_) -> step_calls(state, op)

    // Iteration
    ForInStart | ForInNext | GetIterator | IteratorNext | IteratorClose ->
      step_iteration(state, op)

    // Generator/async
    InitialYield | Yield | Await -> step_generators(state, op)

    // Special
    CreateArguments | NewRegExp -> step_special(state, op)

    _ ->
      Error(#(
        StepVmError(Unimplemented("opcode: " <> string.inspect(op))),
        JsUndefined,
        state.heap,
      ))
  }
}

// ============================================================================
// Step sub-functions by opcode category
// ============================================================================

fn step_stack(
  state: State,
  op: Op,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case op {
    PushConst(index) -> {
      case array.get(index, state.constants) {
        Some(value) ->
          Ok(State(..state, stack: [value, ..state.stack], pc: state.pc + 1))
        None -> {
          frame.throw_range_error(
            state,
            "constant index out of bounds: " <> int.to_string(index),
          )
        }
      }
    }

    Pop -> {
      case state.stack {
        [_, ..rest] -> Ok(State(..state, stack: rest, pc: state.pc + 1))
        [] ->
          Error(#(StepVmError(StackUnderflow("Pop")), JsUndefined, state.heap))
      }
    }

    Dup -> {
      case state.stack {
        [top, ..] ->
          Ok(State(..state, stack: [top, ..state.stack], pc: state.pc + 1))
        [] ->
          Error(#(StepVmError(StackUnderflow("Dup")), JsUndefined, state.heap))
      }
    }

    Swap -> {
      case state.stack {
        [a, b, ..rest] ->
          Ok(State(..state, stack: [b, a, ..rest], pc: state.pc + 1))
        _ ->
          Error(#(StepVmError(StackUnderflow("Swap")), JsUndefined, state.heap))
      }
    }

    _ ->
      Error(#(
        StepVmError(Unimplemented("step_stack: " <> string.inspect(op))),
        JsUndefined,
        state.heap,
      ))
  }
}

fn step_locals(
  state: State,
  op: Op,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case op {
    GetLocal(index) -> {
      case array.get(index, state.locals) {
        Some(JsUninitialized) -> {
          frame.throw_reference_error(
            state,
            "Cannot access variable before initialization (TDZ)",
          )
        }
        Some(value) ->
          Ok(State(..state, stack: [value, ..state.stack], pc: state.pc + 1))
        None ->
          Error(#(
            StepVmError(LocalIndexOutOfBounds(index)),
            JsUndefined,
            state.heap,
          ))
      }
    }

    PutLocal(index) -> {
      case state.stack {
        [value, ..rest] -> {
          case array.set(index, value, state.locals) {
            Ok(new_locals) ->
              Ok(
                State(
                  ..state,
                  stack: rest,
                  locals: new_locals,
                  pc: state.pc + 1,
                ),
              )
            Error(_) ->
              Error(#(
                StepVmError(LocalIndexOutOfBounds(index)),
                JsUndefined,
                state.heap,
              ))
          }
        }
        [] ->
          Error(#(
            StepVmError(StackUnderflow("PutLocal")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    BoxLocal(index) -> {
      // Wrap the current value in locals[index] into a BoxSlot on the heap.
      // Replace the local with a JsObject(box_ref).
      case array.get(index, state.locals) {
        Some(current_value) -> {
          let #(heap, box_ref) =
            heap.alloc(state.heap, value.BoxSlot(current_value))
          case array.set(index, JsObject(box_ref), state.locals) {
            Ok(locals) -> Ok(State(..state, heap:, locals:, pc: state.pc + 1))
            Error(_) ->
              Error(#(
                StepVmError(LocalIndexOutOfBounds(index)),
                JsUndefined,
                heap,
              ))
          }
        }
        None ->
          Error(#(
            StepVmError(LocalIndexOutOfBounds(index)),
            JsUndefined,
            state.heap,
          ))
      }
    }

    GetBoxed(index) -> {
      // Read locals[index] (a JsObject(box_ref)), dereference BoxSlot, push value.
      case array.get(index, state.locals) {
        Some(JsObject(box_ref)) ->
          case heap.read_box(state.heap, box_ref) {
            Some(val) ->
              Ok(State(..state, stack: [val, ..state.stack], pc: state.pc + 1))
            None ->
              Error(#(
                StepVmError(Unimplemented("GetBoxed: not a BoxSlot")),
                JsUndefined,
                state.heap,
              ))
          }
        _ ->
          Error(#(
            StepVmError(Unimplemented("GetBoxed: local is not a box ref")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    PutBoxed(index) -> {
      // Pop value from stack, write into the BoxSlot pointed to by locals[index].
      case state.stack {
        [new_value, ..rest_stack] -> {
          case array.get(index, state.locals) {
            Some(JsObject(box_ref)) -> {
              let heap =
                heap.write(state.heap, box_ref, value.BoxSlot(new_value))
              Ok(State(..state, heap:, stack: rest_stack, pc: state.pc + 1))
            }
            _ ->
              Error(#(
                StepVmError(Unimplemented("PutBoxed: local is not a box ref")),
                JsUndefined,
                state.heap,
              ))
          }
        }
        [] ->
          Error(#(
            StepVmError(StackUnderflow("PutBoxed")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    _ ->
      Error(#(
        StepVmError(Unimplemented("step_locals: " <> string.inspect(op))),
        JsUndefined,
        state.heap,
      ))
  }
}

fn step_globals(
  state: State,
  op: Op,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case op {
    // §9.1.1.4.4 GetBindingValue — two-phase: declarative then object record
    GetGlobal(name) -> {
      case dict.get(state.lexical_globals, name) {
        // Lexical binding exists — check for TDZ
        Ok(JsUninitialized) ->
          frame.throw_reference_error(
            state,
            "Cannot access '" <> name <> "' before initialization",
          )
        Ok(value) ->
          Ok(State(..state, stack: [value, ..state.stack], pc: state.pc + 1))
        // Not in lexical → try object record (globalThis)
        Error(_) ->
          case object.get_own_property(state.heap, state.global_object, name) {
            Some(DataProperty(value: val, ..)) ->
              Ok(State(..state, stack: [val, ..state.stack], pc: state.pc + 1))
            Some(value.AccessorProperty(get: Some(getter), ..)) ->
              case
                frame.call(state, getter, JsObject(state.global_object), [])
              {
                Ok(#(val, state)) ->
                  Ok(
                    State(
                      ..state,
                      stack: [val, ..state.stack],
                      pc: state.pc + 1,
                    ),
                  )
                Error(#(thrown, state)) -> Error(#(Thrown, thrown, state.heap))
              }
            Some(value.AccessorProperty(get: None, ..)) ->
              Ok(
                State(
                  ..state,
                  stack: [JsUndefined, ..state.stack],
                  pc: state.pc + 1,
                ),
              )
            None ->
              // Check prototype chain
              case object.has_property(state.heap, state.global_object, name) {
                True ->
                  case
                    object.get_value_of(
                      state,
                      JsObject(state.global_object),
                      name,
                    )
                  {
                    Ok(#(val, state)) ->
                      Ok(
                        State(
                          ..state,
                          stack: [val, ..state.stack],
                          pc: state.pc + 1,
                        ),
                      )
                    Error(#(thrown, state)) ->
                      Error(#(Thrown, thrown, state.heap))
                  }
                False ->
                  frame.throw_reference_error(state, name <> " is not defined")
              }
          }
      }
    }

    // §9.1.1.4.5 SetMutableBinding — two-phase: declarative then object record
    PutGlobal(name) -> {
      case state.stack {
        [value, ..rest] -> {
          // 1. Check const lexical
          case set.contains(state.const_lexical_globals, name) {
            True ->
              frame.throw_type_error(state, "Assignment to constant variable.")
            False ->
              // 2. Check lexical globals
              case dict.get(state.lexical_globals, name) {
                Ok(JsUninitialized) ->
                  frame.throw_reference_error(
                    state,
                    "Cannot access '" <> name <> "' before initialization",
                  )
                Ok(_) ->
                  Ok(
                    State(
                      ..state,
                      stack: rest,
                      lexical_globals: dict.insert(
                        state.lexical_globals,
                        name,
                        value,
                      ),
                      pc: state.pc + 1,
                    ),
                  )
                // 3. Object record path
                Error(_) ->
                  case state.func.is_strict {
                    True ->
                      // Strict mode: must exist on globalThis or throw
                      case
                        object.has_property(
                          state.heap,
                          state.global_object,
                          name,
                        )
                      {
                        False ->
                          frame.throw_reference_error(
                            state,
                            name <> " is not defined",
                          )
                        True ->
                          case
                            object.set_value(
                              State(..state, stack: rest),
                              state.global_object,
                              name,
                              value,
                              JsObject(state.global_object),
                            )
                          {
                            Ok(#(state, True)) ->
                              Ok(State(..state, pc: state.pc + 1))
                            Ok(#(state, False)) ->
                              frame.throw_type_error(
                                state,
                                "Cannot assign to read only property '"
                                  <> name
                                  <> "' of object '#<Object>'",
                              )
                            Error(#(thrown, state)) ->
                              Error(#(Thrown, thrown, state.heap))
                          }
                      }
                    False ->
                      // Sloppy mode: set on globalThis (creates if needed,
                      // returns False for non-writable → silently ignore)
                      case
                        object.set_value(
                          State(..state, stack: rest),
                          state.global_object,
                          name,
                          value,
                          JsObject(state.global_object),
                        )
                      {
                        Ok(#(state, _)) -> Ok(State(..state, pc: state.pc + 1))
                        Error(#(thrown, state)) ->
                          Error(#(Thrown, thrown, state.heap))
                      }
                  }
              }
          }
        }
        [] ->
          Error(#(
            StepVmError(StackUnderflow("PutGlobal")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    // §9.1.1.4.17 CreateGlobalVarBinding — create var on globalThis
    DeclareGlobalVar(name) -> {
      case object.has_property(state.heap, state.global_object, name) {
        True ->
          // Already exists — no-op
          Ok(State(..state, pc: state.pc + 1))
        False -> {
          let #(heap, _) =
            object.set_property(
              state.heap,
              state.global_object,
              name,
              JsUndefined,
            )
          Ok(State(..state, heap:, pc: state.pc + 1))
        }
      }
    }

    // §9.1.1.4.16 CreateGlobalLexBinding — create let/const in lexical record
    DeclareGlobalLex(name, is_const) -> {
      let state =
        State(
          ..state,
          lexical_globals: dict.insert(
            state.lexical_globals,
            name,
            JsUninitialized,
          ),
          const_lexical_globals: case is_const {
            True -> set.insert(state.const_lexical_globals, name)
            False -> set.delete(state.const_lexical_globals, name)
          },
          pc: state.pc + 1,
        )
      Ok(state)
    }

    // Initialize a lexical global (TDZ → value)
    InitGlobalLex(name) -> {
      case state.stack {
        [value, ..rest] ->
          Ok(
            State(
              ..state,
              stack: rest,
              lexical_globals: dict.insert(state.lexical_globals, name, value),
              pc: state.pc + 1,
            ),
          )
        [] ->
          Error(#(
            StepVmError(StackUnderflow("InitGlobalLex")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    TypeOf -> {
      case state.stack {
        [val, ..rest] -> {
          Ok(
            State(
              ..state,
              stack: [JsString(common.typeof_value(val, state.heap)), ..rest],
              pc: state.pc + 1,
            ),
          )
        }
        [] ->
          Error(#(
            StepVmError(StackUnderflow("TypeOf")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    // §9.1.1.4: typeof on globals — TDZ throws, undeclared returns "undefined"
    TypeofGlobal(name) -> {
      case dict.get(state.lexical_globals, name) {
        // TDZ — typeof on uninitialized lexical still throws per spec
        Ok(JsUninitialized) ->
          frame.throw_reference_error(
            state,
            "Cannot access '" <> name <> "' before initialization",
          )
        Ok(val) ->
          Ok(
            State(
              ..state,
              stack: [
                JsString(common.typeof_value(val, state.heap)),
                ..state.stack
              ],
              pc: state.pc + 1,
            ),
          )
        Error(_) -> {
          // Object record: try globalThis, return "undefined" if not found
          let val = case
            object.get_own_property(state.heap, state.global_object, name)
          {
            Some(DataProperty(value: v, ..)) -> v
            _ ->
              case object.has_property(state.heap, state.global_object, name) {
                True ->
                  // Property exists on proto chain — use get_value_of for correct result
                  case
                    object.get_value_of(
                      state,
                      JsObject(state.global_object),
                      name,
                    )
                  {
                    Ok(#(v, _)) -> v
                    Error(_) -> JsUndefined
                  }
                False -> JsUndefined
              }
          }
          Ok(
            State(
              ..state,
              stack: [
                JsString(common.typeof_value(val, state.heap)),
                ..state.stack
              ],
              pc: state.pc + 1,
            ),
          )
        }
      }
    }

    _ ->
      Error(#(
        StepVmError(Unimplemented("step_globals: " <> string.inspect(op))),
        JsUndefined,
        state.heap,
      ))
  }
}

fn step_operators(
  state: State,
  op: Op,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case op {
    BinOp(kind) -> {
      case state.stack {
        [right, left, ..rest] -> {
          // instanceof and in need heap access
          case kind {
            opcode.InstanceOf -> {
              use #(result, state) <- result.map(
                frame.rethrow(coerce.js_instanceof(state, left, right)),
              )
              State(..state, stack: [JsBool(result), ..rest], pc: state.pc + 1)
            }
            opcode.In -> {
              // left = key, right = object
              case right {
                JsObject(ref) ->
                  case coerce.js_to_string(state, left) {
                    Ok(#(key_str, state)) -> {
                      let result = object.has_property(state.heap, ref, key_str)
                      Ok(
                        State(
                          ..state,
                          stack: [JsBool(result), ..rest],
                          pc: state.pc + 1,
                        ),
                      )
                    }
                    Error(#(thrown, state)) ->
                      Error(#(Thrown, thrown, state.heap))
                  }
                _ ->
                  frame.throw_type_error(
                    state,
                    "Cannot use 'in' operator to search for '"
                      <> object.inspect(left, state.heap)
                      <> "' in "
                      <> object.inspect(right, state.heap),
                  )
              }
            }
            // Add needs ToPrimitive for object operands (ES2024 §13.15.3)
            Add ->
              case left, right {
                // Fast path: both primitives — no ToPrimitive needed
                JsObject(_), _ | _, JsObject(_) ->
                  binop_add_with_to_primitive(state, left, right, rest)
                // Both primitives — fast path, no ToPrimitive needed
                JsString(a), JsString(b) ->
                  Ok(
                    State(
                      ..state,
                      stack: [JsString(a <> b), ..rest],
                      pc: state.pc + 1,
                    ),
                  )
                JsString(a), _ ->
                  case coerce.js_to_string(state, right) {
                    Ok(#(b, state)) ->
                      Ok(
                        State(
                          ..state,
                          stack: [JsString(a <> b), ..rest],
                          pc: state.pc + 1,
                        ),
                      )
                    Error(#(thrown, state)) ->
                      Error(#(Thrown, thrown, state.heap))
                  }
                _, JsString(b) ->
                  case coerce.js_to_string(state, left) {
                    Ok(#(a, state)) ->
                      Ok(
                        State(
                          ..state,
                          stack: [JsString(a <> b), ..rest],
                          pc: state.pc + 1,
                        ),
                      )
                    Error(#(thrown, state)) ->
                      Error(#(Thrown, thrown, state.heap))
                  }
                _, _ ->
                  case operators.num_binop(left, right, operators.num_add) {
                    Ok(result) ->
                      Ok(
                        State(
                          ..state,
                          stack: [result, ..rest],
                          pc: state.pc + 1,
                        ),
                      )
                    Error(msg) -> {
                      frame.throw_type_error(state, msg)
                    }
                  }
              }
            _ ->
              case operators.exec_binop(kind, left, right) {
                Ok(result) ->
                  Ok(State(..state, stack: [result, ..rest], pc: state.pc + 1))
                Error(msg) -> {
                  frame.throw_type_error(state, msg)
                }
              }
          }
        }
        _ ->
          Error(#(StepVmError(StackUnderflow("BinOp")), JsUndefined, state.heap))
      }
    }

    UnaryOp(kind) -> {
      case state.stack {
        [operand, ..rest] -> {
          case operators.exec_unaryop(kind, operand) {
            Ok(result) ->
              Ok(State(..state, stack: [result, ..rest], pc: state.pc + 1))
            Error(msg) -> {
              frame.throw_type_error(state, msg)
            }
          }
        }
        [] ->
          Error(#(
            StepVmError(StackUnderflow("UnaryOp")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    _ ->
      Error(#(
        StepVmError(Unimplemented("step_operators: " <> string.inspect(op))),
        JsUndefined,
        state.heap,
      ))
  }
}

fn step_control_flow(
  state: State,
  op: Op,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case op {
    Return -> {
      let return_value = case state.stack {
        [value, ..] -> value
        [] -> JsUndefined
      }
      case state.call_stack {
        // No caller — top-level return, we're done
        [] -> Error(#(Done, return_value, state.heap))
        // Pop call frame, restore caller, push return value onto caller's stack
        [
          SavedFrame(
            func:,
            locals:,
            stack:,
            pc:,
            try_stack:,
            this_binding: saved_this,
            constructor_this:,
            callee_ref: saved_callee_ref,
            call_args: saved_call_args,
          ),
          ..rest_frames
        ] -> {
          // Constructor return semantics
          case constructor_this {
            Some(constructed_obj) -> {
              // Base constructor: use the constructed object unless the
              // function explicitly returned an object.
              let effective_return = case return_value {
                JsObject(_) -> return_value
                _ -> constructed_obj
              }
              Ok(
                State(
                  ..state,
                  stack: [effective_return, ..stack],
                  locals:,
                  func:,
                  code: func.bytecode,
                  constants: func.constants,
                  pc:,
                  call_stack: rest_frames,
                  call_depth: state.call_depth - 1,
                  try_stack:,
                  this_binding: saved_this,
                  callee_ref: saved_callee_ref,
                  call_args: saved_call_args,
                ),
              )
            }
            None ->
              case state.func.is_derived_constructor {
                True -> {
                  case return_value {
                    JsObject(_) ->
                      Ok(
                        State(
                          ..state,
                          stack: [return_value, ..stack],
                          locals:,
                          func:,
                          code: func.bytecode,
                          constants: func.constants,
                          pc:,
                          call_stack: rest_frames,
                          call_depth: state.call_depth - 1,
                          try_stack:,
                          this_binding: saved_this,
                          callee_ref: saved_callee_ref,
                          call_args: saved_call_args,
                        ),
                      )
                    JsUndefined ->
                      case state.this_binding {
                        JsUninitialized -> {
                          frame.throw_reference_error(
                            state,
                            "Must call super constructor in derived class before returning from derived constructor",
                          )
                        }
                        this_val ->
                          Ok(
                            State(
                              ..state,
                              stack: [this_val, ..stack],
                              locals:,
                              func:,
                              code: func.bytecode,
                              constants: func.constants,
                              pc:,
                              call_stack: rest_frames,
                              call_depth: state.call_depth - 1,
                              try_stack:,
                              this_binding: saved_this,
                              callee_ref: saved_callee_ref,
                              call_args: saved_call_args,
                            ),
                          )
                      }
                    _ -> {
                      frame.throw_type_error(
                        state,
                        "Derived constructors may only return object or undefined",
                      )
                    }
                  }
                }
                False ->
                  // Regular function return
                  Ok(
                    State(
                      ..state,
                      stack: [return_value, ..stack],
                      locals:,
                      func:,
                      code: func.bytecode,
                      constants: func.constants,
                      pc:,
                      call_stack: rest_frames,
                      call_depth: state.call_depth - 1,
                      try_stack:,
                      this_binding: saved_this,
                      callee_ref: saved_callee_ref,
                    ),
                  )
              }
          }
        }
      }
    }

    Jump(target) -> Ok(State(..state, pc: target))

    JumpIfFalse(target) -> {
      use v <- conditional_jump(state, target)
      !value.is_truthy(v)
    }

    JumpIfTrue(target) -> conditional_jump(state, target, value.is_truthy)

    JumpIfNullish(target) -> {
      use v <- conditional_jump(state, target)
      case v {
        JsNull | JsUndefined -> True
        _ -> False
      }
    }

    // -- Exception handling --
    PushTry(catch_target) -> {
      let frame = TryFrame(catch_target:, stack_depth: list.length(state.stack))
      Ok(
        State(..state, try_stack: [frame, ..state.try_stack], pc: state.pc + 1),
      )
    }

    opcode.PopTry -> {
      case state.try_stack {
        [_, ..rest] -> Ok(State(..state, try_stack: rest, pc: state.pc + 1))
        [] ->
          Error(#(
            StepVmError(StackUnderflow("PopTry: empty try_stack")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    opcode.Throw -> {
      case state.stack {
        [value, ..] -> Error(#(Thrown, value, state.heap))
        [] ->
          Error(#(StepVmError(StackUnderflow("Throw")), JsUndefined, state.heap))
      }
    }

    opcode.EnterFinally -> {
      Ok(
        State(
          ..state,
          finally_stack: [frame.NormalCompletion, ..state.finally_stack],
          pc: state.pc + 1,
        ),
      )
    }

    opcode.EnterFinallyThrow -> {
      // Pop thrown value from stack, push ThrowCompletion to finally_stack
      case state.stack {
        [thrown_value, ..rest_stack] ->
          Ok(
            State(
              ..state,
              stack: rest_stack,
              finally_stack: [
                frame.ThrowCompletion(thrown_value),
                ..state.finally_stack
              ],
              pc: state.pc + 1,
            ),
          )
        [] ->
          Error(#(
            StepVmError(StackUnderflow("EnterFinallyThrow")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    opcode.LeaveFinally -> {
      case state.finally_stack {
        [frame.NormalCompletion, ..rest] ->
          Ok(State(..state, finally_stack: rest, pc: state.pc + 1))
        [frame.ThrowCompletion(value:), ..] ->
          Error(#(Thrown, value, state.heap))
        [frame.ReturnCompletion(value:), ..] ->
          Error(#(Done, value, state.heap))
        [] ->
          Error(#(
            StepVmError(StackUnderflow("LeaveFinally: empty finally_stack")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    _ ->
      Error(#(
        StepVmError(Unimplemented("step_control_flow: " <> string.inspect(op))),
        JsUndefined,
        state.heap,
      ))
  }
}

fn step_objects(
  state: State,
  op: Op,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case op {
    NewObject -> {
      let #(heap, ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: OrdinaryObject,
            properties: dict.new(),
            elements: js_elements.new(),
            prototype: Some(state.builtins.object.prototype),
            symbol_properties: dict.new(),
            extensible: True,
          ),
        )
      Ok(
        State(
          ..state,
          heap:,
          stack: [JsObject(ref), ..state.stack],
          pc: state.pc + 1,
        ),
      )
    }

    GetField(name) -> {
      case state.stack {
        [JsNull as v, ..] | [JsUndefined as v, ..] ->
          frame.throw_type_error(
            state,
            "Cannot read properties of "
              <> value.nullish_label(v)
              <> " (reading '"
              <> name
              <> "')",
          )
        [receiver, ..rest] -> {
          use #(val, state) <- result.map(
            frame.rethrow(object.get_value_of(state, receiver, name)),
          )
          State(..state, stack: [val, ..rest], pc: state.pc + 1)
        }
        [] ->
          Error(#(
            StepVmError(StackUnderflow("GetField")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    GetField2(name) -> {
      // Like GetField but keeps the object on the stack for CallMethod.
      // Stack: [obj, ..rest] → [prop_value, obj, ..rest]
      case state.stack {
        [JsNull as v, ..] | [JsUndefined as v, ..] ->
          frame.throw_type_error(
            state,
            "Cannot read properties of "
              <> value.nullish_label(v)
              <> " (reading '"
              <> name
              <> "')",
          )
        [receiver, ..rest] -> {
          use #(val, state) <- result.map(
            frame.rethrow(object.get_value_of(state, receiver, name)),
          )
          State(..state, stack: [val, receiver, ..rest], pc: state.pc + 1)
        }
        [] ->
          Error(#(
            StepVmError(StackUnderflow("GetField2")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    PutField(name) -> {
      // Consumes [value, obj] and pushes value back (assignment is an expression).
      // Consistent with PutElem which also leaves the value on the stack.
      case state.stack {
        [value, JsObject(ref) as receiver, ..rest] -> {
          // set_value walks proto chain, calls setters, handles non-writable.
          // Sloppy mode: ignore failure (strict mode TypeError is a TODO).
          use #(state, _ok) <- result.map(
            frame.rethrow(object.set_value(state, ref, name, value, receiver)),
          )
          State(..state, stack: [value, ..rest], pc: state.pc + 1)
        }
        [value, _, ..rest] -> {
          // PutField on non-object: silently ignore, still return value
          Ok(State(..state, stack: [value, ..rest], pc: state.pc + 1))
        }
        _ ->
          Error(#(
            StepVmError(StackUnderflow("PutField")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    DefineField(name) -> {
      // Like PutField but keeps the object on the stack (for object literal construction)
      case state.stack {
        [value, JsObject(ref) as obj, ..rest] -> {
          let #(heap, _) = object.set_property(state.heap, ref, name, value)
          Ok(State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1))
        }
        [_, _, ..] -> {
          // DefineField on non-object: no-op, keep object on stack
          Ok(State(..state, pc: state.pc + 1))
        }
        _ ->
          Error(#(
            StepVmError(StackUnderflow("DefineField")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    DefineMethod(name) -> {
      // Like DefineField but creates a non-enumerable property (for class methods)
      case state.stack {
        [value, JsObject(ref) as obj, ..rest] -> {
          let heap = object.define_method_property(state.heap, ref, name, value)
          Ok(State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1))
        }
        [_, _, ..] -> Ok(State(..state, pc: state.pc + 1))
        _ ->
          Error(#(
            StepVmError(StackUnderflow("DefineMethod")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    DefineAccessor(name, kind) -> {
      // Object literal getter/setter: { get x() {}, set x(v) {} }
      // Stack: [fn, obj, ...] → [obj, ...]
      // Defines or updates an AccessorProperty on the object.
      case state.stack {
        [func, JsObject(ref) as obj, ..rest] -> {
          let heap = object.define_accessor(state.heap, ref, name, func, kind)
          Ok(State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1))
        }
        [_, _, ..] -> Ok(State(..state, pc: state.pc + 1))
        _ ->
          Error(#(
            StepVmError(StackUnderflow("DefineAccessor")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    DefineAccessorComputed(kind) -> {
      // Computed getter/setter: { get [expr]() {} }
      // Stack: [fn, key, obj, ...] → [obj, ...]
      case state.stack {
        [func, key, JsObject(ref) as obj, ..rest] -> {
          use #(key_str, state) <- result.map(
            frame.rethrow(coerce.js_to_string(state, key)),
          )
          let heap =
            object.define_accessor(state.heap, ref, key_str, func, kind)
          State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1)
        }
        [_, _, _, ..] -> Ok(State(..state, pc: state.pc + 1))
        _ ->
          Error(#(
            StepVmError(StackUnderflow("DefineAccessorComputed")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    DefineFieldComputed -> {
      // Object literal computed key: {[key]: value}
      // Stack: [value, key, obj, ...] → [obj, ...]
      // Key goes through ToPropertyKey (Symbol preserved, else ToString).
      // put_elem_value already implements this (symbol → symbol_properties,
      // array index → elements, else → js_to_string → properties).
      case state.stack {
        [val, key, JsObject(ref) as obj, ..rest] -> {
          use state <- result.map(
            frame.rethrow(property_access.put_elem_value(state, ref, key, val)),
          )
          State(..state, stack: [obj, ..rest], pc: state.pc + 1)
        }
        [_, _, _, ..rest] ->
          // Non-object target: shouldn't happen for literals, but pop and keep going.
          Ok(State(..state, stack: rest, pc: state.pc + 1))
        _ ->
          Error(#(
            StepVmError(StackUnderflow("DefineFieldComputed")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    ObjectSpread -> {
      // Object spread: {...source}
      // Stack: [source, obj, ...] → [obj, ...]
      // CopyDataProperties: own enumerable props of source → target.
      // null/undefined/primitives → no-op per spec (unlike assign target).
      case state.stack {
        [source, JsObject(ref) as obj, ..rest] -> {
          use state <- result.map(
            frame.rethrow(object.copy_data_properties(state, ref, source)),
          )
          State(..state, stack: [obj, ..rest], pc: state.pc + 1)
        }
        [_, _, ..rest] -> Ok(State(..state, stack: rest, pc: state.pc + 1))
        _ ->
          Error(#(
            StepVmError(StackUnderflow("ObjectSpread")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    // -- Delete operator --
    DeleteField(name) -> {
      case state.stack {
        [obj, ..rest] ->
          case obj {
            JsObject(ref) -> {
              let #(heap, success) =
                object.delete_property(state.heap, ref, name)
              Ok(
                State(
                  ..state,
                  stack: [JsBool(success), ..rest],
                  heap:,
                  pc: state.pc + 1,
                ),
              )
            }
            // delete on non-object returns true
            _ ->
              Ok(
                State(..state, stack: [JsBool(True), ..rest], pc: state.pc + 1),
              )
          }
        _ ->
          Error(#(
            StepVmError(StackUnderflow("DeleteField")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    DeleteElem -> {
      case state.stack {
        [key, obj, ..rest] ->
          case obj {
            JsObject(ref) -> {
              use #(key_str, state) <- result.map(
                frame.rethrow(coerce.js_to_string(state, key)),
              )
              let #(heap, success) =
                object.delete_property(state.heap, ref, key_str)
              State(
                ..state,
                stack: [JsBool(success), ..rest],
                heap:,
                pc: state.pc + 1,
              )
            }
            _ ->
              Ok(
                State(..state, stack: [JsBool(True), ..rest], pc: state.pc + 1),
              )
          }
        _ ->
          Error(#(
            StepVmError(StackUnderflow("DeleteElem")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    // -- Class Inheritance --
    SetupDerivedClass -> {
      // Stack: [ctor, parent, ..rest] → [ctor, ..rest]
      // Wire ctor.prototype.__proto__ = parent.prototype
      // Wire ctor.__proto__ = parent (for static inheritance)
      case state.stack {
        [JsObject(ctor_ref), JsObject(parent_ref), ..rest] -> {
          case heap.read(state.heap, parent_ref) {
            Some(ObjectSlot(kind: FunctionObject(..), ..)) -> {
              let parent_proto =
                get_field_ref(state.heap, parent_ref, "prototype")
                |> option.unwrap(state.builtins.object.prototype)
              // Set ctor.prototype.__proto__ = parent.prototype
              let heap =
                get_field_ref(state.heap, ctor_ref, "prototype")
                |> option.map(set_slot_prototype(
                  state.heap,
                  _,
                  Some(parent_proto),
                ))
                |> option.unwrap(state.heap)
              // Set ctor.__proto__ = parent (for static inheritance)
              let heap = set_slot_prototype(heap, ctor_ref, Some(parent_ref))
              Ok(
                State(
                  ..state,
                  heap:,
                  stack: [JsObject(ctor_ref), ..rest],
                  pc: state.pc + 1,
                ),
              )
            }
            _ ->
              Ok(
                State(
                  ..state,
                  stack: [JsObject(ctor_ref), ..rest],
                  pc: state.pc + 1,
                ),
              )
          }
        }
        [JsObject(ctor_ref), JsNull, ..rest] -> {
          // extends null — ctor.prototype.__proto__ = null
          let heap =
            get_field_ref(state.heap, ctor_ref, "prototype")
            |> option.map(set_slot_prototype(state.heap, _, None))
            |> option.unwrap(state.heap)
          Ok(
            State(
              ..state,
              heap:,
              stack: [JsObject(ctor_ref), ..rest],
              pc: state.pc + 1,
            ),
          )
        }
        _ -> {
          frame.throw_type_error(
            state,
            "Class extends value is not a constructor or null",
          )
        }
      }
    }

    GetThis ->
      case state.this_binding {
        // TDZ check: in derived constructors, this is uninitialized until super() is called
        JsUninitialized -> {
          frame.throw_reference_error(
            state,
            "Must call super constructor in derived class before accessing 'this'",
          )
        }
        _ ->
          Ok(
            State(
              ..state,
              stack: [state.this_binding, ..state.stack],
              pc: state.pc + 1,
            ),
          )
      }

    _ ->
      Error(#(
        StepVmError(Unimplemented("step_objects: " <> string.inspect(op))),
        JsUndefined,
        state.heap,
      ))
  }
}

fn step_arrays(
  state: State,
  op: Op,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case op {
    // -- Array construction --
    ArrayFrom(count) -> {
      case pop_n(state.stack, count) {
        Some(#(elements, rest)) -> {
          // elements are in order [first, ..., last]
          let #(heap, ref) =
            heap.alloc(
              state.heap,
              ObjectSlot(
                kind: ArrayObject(count),
                properties: dict.new(),
                elements: js_elements.from_list(elements),
                prototype: Some(state.builtins.array.prototype),
                symbol_properties: dict.new(),
                extensible: True,
              ),
            )
          Ok(
            State(
              ..state,
              heap:,
              stack: [JsObject(ref), ..rest],
              pc: state.pc + 1,
            ),
          )
        }
        None ->
          Error(#(
            StepVmError(StackUnderflow("ArrayFrom")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    ArrayFromWithHoles(count, holes) -> {
      // Pop only the non-hole values (count - len(holes)), then zip them with
      // the non-hole indices and build a SparseElements-backed array.
      // The emitter guarantees `holes` is non-empty (empty → ArrayFrom used),
      // sorted ascending, and all indices are in [0, count).
      let value_count = count - list.length(holes)
      case pop_n(state.stack, value_count) {
        Some(#(values, rest)) -> {
          // values are in order [first_non_hole, ..., last_non_hole]
          let indexed = assign_non_hole_indices(values, holes, 0, [])
          let #(heap, ref) =
            heap.alloc(
              state.heap,
              ObjectSlot(
                kind: ArrayObject(count),
                properties: dict.new(),
                elements: js_elements.from_indexed(indexed),
                prototype: Some(state.builtins.array.prototype),
                symbol_properties: dict.new(),
                extensible: True,
              ),
            )
          Ok(
            State(
              ..state,
              heap:,
              stack: [JsObject(ref), ..rest],
              pc: state.pc + 1,
            ),
          )
        }
        None ->
          Error(#(
            StepVmError(StackUnderflow("ArrayFromWithHoles")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    // -- Computed property access --
    GetElem -> {
      case state.stack {
        [key, JsObject(ref), ..rest] -> {
          use #(val, state) <- result.map(
            frame.rethrow(property_access.get_elem_value(state, ref, key)),
          )
          State(..state, stack: [val, ..rest], pc: state.pc + 1)
        }
        [_, JsNull as v, ..] | [_, JsUndefined as v, ..] ->
          frame.throw_type_error(
            state,
            "Cannot read properties of " <> value.nullish_label(v),
          )
        [key, receiver, ..rest] -> {
          // Primitive receiver: stringify key, delegate to get_value_of
          use #(key_str, state) <- result.try(
            frame.rethrow(coerce.js_to_string(state, key)),
          )
          use #(val, state) <- result.map(
            frame.rethrow(object.get_value_of(state, receiver, key_str)),
          )
          State(..state, stack: [val, ..rest], pc: state.pc + 1)
        }
        _ ->
          Error(#(
            StepVmError(StackUnderflow("GetElem")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    GetElem2 -> {
      // Like GetElem but keeps obj+key on stack: [key, obj, ...] -> [value, key, obj, ...]
      case state.stack {
        [key, JsObject(ref) as obj, ..rest] -> {
          use #(val, state) <- result.map(
            frame.rethrow(property_access.get_elem_value(state, ref, key)),
          )
          State(..state, stack: [val, key, obj, ..rest], pc: state.pc + 1)
        }
        [key, receiver, ..rest] -> {
          use #(key_str, state) <- result.try(
            frame.rethrow(coerce.js_to_string(state, key)),
          )
          use #(val, state) <- result.map(
            frame.rethrow(object.get_value_of(state, receiver, key_str)),
          )
          State(..state, stack: [val, key, receiver, ..rest], pc: state.pc + 1)
        }
        _ ->
          Error(#(
            StepVmError(StackUnderflow("GetElem2")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    PutElem -> {
      // Stack: [value, key, obj, ...rest]
      case state.stack {
        [val, key, JsObject(ref), ..rest] -> {
          use state <- result.map(
            frame.rethrow(property_access.put_elem_value(state, ref, key, val)),
          )
          State(..state, stack: [val, ..rest], pc: state.pc + 1)
        }
        [_, _, _, ..rest] -> {
          // PutElem on non-object: silently ignore (JS sloppy mode)
          Ok(State(..state, stack: rest, pc: state.pc + 1))
        }
        _ ->
          Error(#(
            StepVmError(StackUnderflow("PutElem")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    // -- Spread element support (array literals + calls) --
    ArrayPush -> {
      // [val, arr] → [arr]; arr[arr.length] = val, length++.
      case state.stack {
        [val, JsObject(ref) as arr, ..rest] -> {
          let heap = push_onto_array(state.heap, ref, val)
          Ok(State(..state, heap:, stack: [arr, ..rest], pc: state.pc + 1))
        }
        _ ->
          Error(#(
            StepVmError(StackUnderflow("ArrayPush")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    ArrayPushHole -> {
      // [arr] → [arr]; length++ WITHOUT setting any element.
      case state.stack {
        [JsObject(ref) as arr, ..rest] -> {
          let heap = grow_array_length(state.heap, ref)
          Ok(State(..state, heap:, stack: [arr, ..rest], pc: state.pc + 1))
        }
        _ ->
          Error(#(
            StepVmError(StackUnderflow("ArrayPushHole")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    ArraySpread -> {
      // [iterable, arr] → [arr]; drain iterable via the iterator protocol.
      case state.stack {
        [iterable, JsObject(arr_ref) as arr, ..rest] -> {
          use state <- result.map(spread_into_array(state, arr_ref, iterable))
          State(..state, stack: [arr, ..rest], pc: state.pc + 1)
        }
        _ ->
          Error(#(
            StepVmError(StackUnderflow("ArraySpread")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    _ ->
      Error(#(
        StepVmError(Unimplemented("step_arrays: " <> string.inspect(op))),
        JsUndefined,
        state.heap,
      ))
  }
}

fn step_calls(
  state: State,
  op: Op,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case op {
    Call(arity) -> {
      // Stack layout: [arg_n, ..., arg_1, callee, ...rest]
      // Pop arity args, then callee
      case pop_n(state.stack, arity) {
        Some(#(args, after_args)) -> {
          case after_args {
            [JsObject(obj_ref), ..rest_stack] -> {
              case heap.read(state.heap, obj_ref) {
                Some(ObjectSlot(
                  kind: FunctionObject(func_template:, env: env_ref),
                  ..,
                )) ->
                  call_function(
                    state,
                    obj_ref,
                    env_ref,
                    func_template,
                    args,
                    rest_stack,
                    JsUndefined,
                    None,
                    None,
                  )
                Some(ObjectSlot(kind: NativeFunction(native), ..)) ->
                  call_native(state, native, args, rest_stack, JsUndefined)
                _ ->
                  frame.throw_type_error(
                    state,
                    object.inspect(JsObject(obj_ref), state.heap)
                      <> " is not a function",
                  )
              }
            }
            [non_func, ..] ->
              frame.throw_type_error(
                state,
                object.inspect(non_func, state.heap) <> " is not a function",
              )
            [] ->
              Error(#(
                StepVmError(StackUnderflow("Call: no callee")),
                JsUndefined,
                state.heap,
              ))
          }
        }
        None ->
          Error(#(
            StepVmError(StackUnderflow("Call: not enough args")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    CallMethod(_name, arity) -> {
      // Stack: [arg_n, ..., arg_1, method, receiver, ...rest]
      // Pop arity args, then method, then receiver
      case pop_n(state.stack, arity) {
        Some(#(args, after_args)) -> {
          case after_args {
            [JsObject(method_ref), receiver, ..rest_stack] -> {
              case heap.read(state.heap, method_ref) {
                Some(ObjectSlot(
                  kind: FunctionObject(func_template:, env: env_ref),
                  ..,
                )) ->
                  call_function(
                    state,
                    method_ref,
                    env_ref,
                    func_template,
                    args,
                    rest_stack,
                    // Method call: this = receiver
                    receiver,
                    None,
                    None,
                  )
                Some(ObjectSlot(kind: NativeFunction(native), ..)) ->
                  call_native(state, native, args, rest_stack, receiver)
                _ ->
                  frame.throw_type_error(
                    state,
                    object.inspect(JsObject(method_ref), state.heap)
                      <> " is not a function",
                  )
              }
            }
            [non_func, _, ..] ->
              frame.throw_type_error(
                state,
                object.inspect(non_func, state.heap) <> " is not a function",
              )
            _ ->
              Error(#(
                StepVmError(StackUnderflow("CallMethod")),
                JsUndefined,
                state.heap,
              ))
          }
        }
        None ->
          Error(#(
            StepVmError(StackUnderflow("CallMethod: not enough args")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    CallConstructor(arity) -> {
      // Stack: [arg_n, ..., arg_1, constructor, ...rest]
      case pop_n(state.stack, arity) {
        Some(#(args, [JsObject(ctor_ref), ..rest_stack])) ->
          do_construct(state, ctor_ref, args, rest_stack)
        Some(#(_, [non_func, ..])) -> {
          frame.throw_type_error(
            state,
            object.inspect(non_func, state.heap) <> " is not a constructor",
          )
        }
        Some(#(_, [])) ->
          Error(#(
            StepVmError(StackUnderflow("CallConstructor")),
            JsUndefined,
            state.heap,
          ))
        None ->
          Error(#(
            StepVmError(StackUnderflow("CallConstructor: not enough args")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    CallSuper(arity) -> {
      // Stack: [arg_n, ..., arg_1, ..rest] → [new_obj, ..rest]
      // Find parent constructor via callee_ref.__proto__
      case state.callee_ref {
        Some(my_ctor_ref) -> {
          case pop_n(state.stack, arity) {
            Some(#(args, rest_stack)) -> {
              // Find parent constructor: callee_ref.__proto__
              case heap.read(state.heap, my_ctor_ref) {
                Some(ObjectSlot(
                  prototype: Some(parent_ref),
                  properties: my_props,
                  ..,
                )) -> {
                  // For multi-level inheritance: only allocate a new object
                  // when this_binding == JsUninitialized (first super() in chain).
                  // Intermediate derived constructors reuse the existing this.
                  let #(heap, this_val) = case state.this_binding {
                    JsUninitialized -> {
                      // First super() in chain — allocate new object
                      let derived_proto = case dict.get(my_props, "prototype") {
                        Ok(DataProperty(value: JsObject(dp_ref), ..)) ->
                          Some(dp_ref)
                        _ -> Some(state.builtins.object.prototype)
                      }
                      let #(h, new_obj_ref) =
                        heap.alloc(
                          state.heap,
                          ObjectSlot(
                            kind: OrdinaryObject,
                            properties: dict.new(),
                            elements: js_elements.new(),
                            prototype: derived_proto,
                            symbol_properties: dict.new(),
                            extensible: True,
                          ),
                        )
                      #(h, JsObject(new_obj_ref))
                    }
                    existing_this -> {
                      // Intermediate super() — reuse existing this
                      #(state.heap, existing_this)
                    }
                  }
                  // Call parent constructor, passing parent_ref as callee_ref
                  // so further super() calls in the chain can find their parent
                  case heap.read(heap, parent_ref) {
                    Some(ObjectSlot(
                      kind: FunctionObject(func_template:, env: env_ref),
                      ..,
                    )) ->
                      call_function(
                        State(..state, heap:, this_binding: this_val),
                        parent_ref,
                        env_ref,
                        func_template,
                        args,
                        rest_stack,
                        this_val,
                        Some(this_val),
                        Some(parent_ref),
                      )
                    Some(ObjectSlot(kind: NativeFunction(native), ..)) ->
                      call_native(
                        State(..state, heap:, this_binding: this_val),
                        native,
                        args,
                        rest_stack,
                        this_val,
                      )
                    _ ->
                      frame.throw_type_error(
                        State(..state, heap:),
                        "Super constructor is not a constructor",
                      )
                  }
                }
                _ ->
                  frame.throw_type_error(
                    state,
                    "Super constructor is not a constructor",
                  )
              }
            }
            None ->
              Error(#(
                StepVmError(StackUnderflow("CallSuper: not enough args")),
                JsUndefined,
                state.heap,
              ))
          }
        }
        None -> {
          frame.throw_reference_error(state, "'super' keyword unexpected here")
        }
      }
    }

    CallApply -> {
      // [args_array, callee] → [result]; this=undefined.
      case state.stack {
        [JsObject(args_ref), callee, ..rest] -> {
          let args = extract_array_args(state.heap, args_ref)
          call_value(State(..state, stack: rest), callee, args, JsUndefined)
        }
        [_, callee, ..] -> {
          frame.throw_type_error(
            state,
            object.inspect(callee, state.heap) <> " is not a function",
          )
        }
        _ ->
          Error(#(
            StepVmError(StackUnderflow("CallApply")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    CallMethodApply -> {
      // [args_array, method, receiver] → [result]; this=receiver.
      case state.stack {
        [JsObject(args_ref), method, receiver, ..rest] -> {
          let args = extract_array_args(state.heap, args_ref)
          call_value(State(..state, stack: rest), method, args, receiver)
        }
        _ ->
          Error(#(
            StepVmError(StackUnderflow("CallMethodApply")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    CallConstructorApply -> {
      // [args_array, ctor] → [new instance]. Spread-new path.
      case state.stack {
        [JsObject(args_ref), JsObject(ctor_ref), ..rest] -> {
          let args = extract_array_args(state.heap, args_ref)
          do_construct(state, ctor_ref, args, rest)
        }
        [_, non_ctor, ..] -> {
          frame.throw_type_error(
            state,
            object.inspect(non_ctor, state.heap) <> " is not a constructor",
          )
        }
        _ ->
          Error(#(
            StepVmError(StackUnderflow("CallConstructorApply")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    MakeClosure(func_index) -> {
      case array.get(func_index, state.func.functions) {
        Some(child_template) -> {
          // Capture values from current frame according to env_descriptors.
          // For boxed captured vars, the local holds a JsObject(box_ref) —
          // copying that ref means the closure shares the same BoxSlot.
          let captured_values =
            list.map(child_template.env_descriptors, fn(desc) {
              case desc {
                value.CaptureLocal(parent_index) ->
                  array.get(parent_index, state.locals)
                  |> option.unwrap(JsUndefined)
                value.CaptureEnv(_parent_env_index) ->
                  // Transitive capture not yet implemented
                  JsUndefined
              }
            })
          let #(heap, env_ref) =
            heap.alloc(state.heap, value.EnvSlot(captured_values))
          // For non-arrow functions, pre-populate .prototype with a fresh object
          // so that `Foo.prototype.bar = ...` and `new Foo()` work.
          // .constructor on prototype is set after we have the closure ref.
          let name_prop =
            common.fn_name_property(option.unwrap(child_template.name, ""))
          let length_prop = common.fn_length_property(child_template.arity)
          let #(heap, fn_properties, proto_ref) = case child_template.is_arrow {
            True -> #(
              heap,
              dict.from_list([#("name", name_prop), #("length", length_prop)]),
              None,
            )
            False -> {
              let #(h, proto_obj_ref) =
                heap.alloc(
                  heap,
                  ObjectSlot(
                    kind: OrdinaryObject,
                    properties: dict.new(),
                    elements: js_elements.new(),
                    prototype: Some(state.builtins.object.prototype),
                    symbol_properties: dict.new(),
                    extensible: True,
                  ),
                )
              #(
                h,
                dict.from_list([
                  #(
                    "prototype",
                    value.data(JsObject(proto_obj_ref)) |> value.writable(),
                  ),
                  #("name", name_prop),
                  #("length", length_prop),
                ]),
                Some(proto_obj_ref),
              )
            }
          }
          let #(heap, closure_ref) =
            heap.alloc(
              heap,
              ObjectSlot(
                kind: FunctionObject(
                  func_template: child_template,
                  env: env_ref,
                ),
                properties: fn_properties,
                elements: js_elements.new(),
                prototype: Some(state.builtins.function.prototype),
                symbol_properties: dict.new(),
                extensible: True,
              ),
            )
          // Set .constructor on the prototype pointing back to this function
          let heap = case proto_ref {
            Some(pr) -> {
              use slot <- heap.update(heap, pr)
              case slot {
                ObjectSlot(properties: props, ..) ->
                  ObjectSlot(
                    ..slot,
                    properties: dict.insert(
                      props,
                      "constructor",
                      value.builtin_property(JsObject(closure_ref)),
                    ),
                  )
                _ -> slot
              }
            }
            None -> heap
          }
          Ok(
            State(
              ..state,
              heap:,
              stack: [JsObject(closure_ref), ..state.stack],
              pc: state.pc + 1,
            ),
          )
        }
        None -> {
          frame.throw_range_error(
            state,
            "invalid function index: " <> int.to_string(func_index),
          )
        }
      }
    }

    _ ->
      Error(#(
        StepVmError(Unimplemented("step_calls: " <> string.inspect(op))),
        JsUndefined,
        state.heap,
      ))
  }
}

fn step_iteration(
  state: State,
  op: Op,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case op {
    ForInStart -> {
      case state.stack {
        [obj, ..rest] -> {
          // Collect enumerable keys from the object (or empty for non-objects)
          let keys = case obj {
            JsObject(ref) -> object.enumerate_keys(state.heap, ref)
            // for-in on null/undefined produces no iterations
            JsNull | JsUndefined -> []
            // Primitives: no enumerable properties
            _ -> []
          }
          // Wrap string keys as JsString values for ForInIteratorSlot
          let key_values = list.map(keys, JsString)
          let #(heap, iter_ref) =
            heap.alloc(state.heap, ForInIteratorSlot(keys: key_values))
          Ok(
            State(
              ..state,
              stack: [JsObject(iter_ref), ..rest],
              heap:,
              pc: state.pc + 1,
            ),
          )
        }
        _ ->
          Error(#(
            StepVmError(StackUnderflow("ForInStart")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    ForInNext -> {
      case state.stack {
        [JsObject(iter_ref), ..rest] ->
          case heap.read(state.heap, iter_ref) {
            Some(ForInIteratorSlot(keys:)) ->
              case keys {
                [val, ..remaining] -> {
                  // Advance the iterator
                  let heap =
                    heap.write(
                      state.heap,
                      iter_ref,
                      ForInIteratorSlot(keys: remaining),
                    )
                  // Push: iterator stays, key, done=false
                  Ok(
                    State(
                      ..state,
                      stack: [JsBool(False), val, JsObject(iter_ref), ..rest],
                      heap:,
                      pc: state.pc + 1,
                    ),
                  )
                }
                [] -> {
                  // No more keys — push undefined + done=true
                  Ok(
                    State(
                      ..state,
                      stack: [
                        JsBool(True),
                        JsUndefined,
                        JsObject(iter_ref),
                        ..rest
                      ],
                      pc: state.pc + 1,
                    ),
                  )
                }
              }
            _ ->
              Error(#(
                StepVmError(Unimplemented("ForInNext: not a ForInIteratorSlot")),
                JsUndefined,
                state.heap,
              ))
          }
        _ ->
          Error(#(
            StepVmError(StackUnderflow("ForInNext")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    GetIterator -> {
      case state.stack {
        [iterable, ..rest] ->
          case iterable {
            JsObject(ref) ->
              case heap.read(state.heap, ref) {
                // Array/Arguments fast path: use ArrayIteratorSlot when
                // Symbol.iterator hasn't been overridden on the instance.
                Some(ObjectSlot(kind: ArrayObject(_), ..))
                | Some(ObjectSlot(kind: value.ArgumentsObject(_), ..)) ->
                  get_iterator_array_fast_path(state, ref, iterable, rest)

                Some(ObjectSlot(kind: GeneratorObject(_), ..)) -> {
                  // Generators are their own iterators
                  Ok(
                    State(
                      ..state,
                      stack: [JsObject(ref), ..rest],
                      pc: state.pc + 1,
                    ),
                  )
                }
                // Any other object (or array with overridden Symbol.iterator):
                // look up Symbol.iterator per §7.4.1 GetIterator
                Some(ObjectSlot(..)) ->
                  get_iterator_via_symbol(state, ref, iterable, rest)
                _ ->
                  frame.throw_type_error(
                    state,
                    object.inspect(iterable, state.heap) <> " is not iterable",
                  )
              }
            // String primitive: iterate UTF-16 code units
            JsString(_) ->
              case common.to_object(state.heap, state.builtins, iterable) {
                Some(#(h, wrapper_ref)) -> {
                  let #(h, iter_ref) =
                    heap.alloc(
                      h,
                      ArrayIteratorSlot(source: wrapper_ref, index: 0),
                    )
                  Ok(
                    State(
                      ..state,
                      stack: [JsObject(iter_ref), ..rest],
                      heap: h,
                      pc: state.pc + 1,
                    ),
                  )
                }
                None ->
                  frame.throw_type_error(
                    state,
                    object.inspect(iterable, state.heap) <> " is not iterable",
                  )
              }
            _ ->
              frame.throw_type_error(
                state,
                object.inspect(iterable, state.heap) <> " is not iterable",
              )
          }
        _ ->
          Error(#(
            StepVmError(StackUnderflow("GetIterator")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    IteratorNext -> {
      case state.stack {
        [JsObject(iter_ref), ..rest] ->
          case heap.read(state.heap, iter_ref) {
            Some(ArrayIteratorSlot(source:, index:)) -> {
              // Re-read the source length each time (handles mutations during iteration)
              let #(length, elements) =
                heap.read_array_like(state.heap, source)
                |> option.unwrap(#(0, js_elements.new()))
              case index >= length {
                True ->
                  // Done — push undefined + done=true
                  Ok(
                    State(
                      ..state,
                      stack: [
                        JsBool(True),
                        JsUndefined,
                        JsObject(iter_ref),
                        ..rest
                      ],
                      pc: state.pc + 1,
                    ),
                  )
                False -> {
                  // Read element at current index
                  let val = js_elements.get(elements, index)
                  // Advance iterator index
                  let heap =
                    heap.write(
                      state.heap,
                      iter_ref,
                      ArrayIteratorSlot(source:, index: index + 1),
                    )
                  Ok(
                    State(
                      ..state,
                      stack: [JsBool(False), val, JsObject(iter_ref), ..rest],
                      heap:,
                      pc: state.pc + 1,
                    ),
                  )
                }
              }
            }
            Some(ObjectSlot(kind: GeneratorObject(_), ..)) -> {
              // Generator iterator: call .next() and extract {value, done}
              {
                use next_state <- result.try(
                  generators.call_native_generator_next(
                    state,
                    JsObject(iter_ref),
                    [],
                    [],
                    execute_inner,
                    unwind_to_catch,
                  ),
                )
                // next_state.stack has [result_obj, ...], extract value and done
                case next_state.stack {
                  [JsObject(result_ref), ..] ->
                    case heap.read(next_state.heap, result_ref) {
                      Some(ObjectSlot(properties: props, ..)) -> {
                        let val = case dict.get(props, "value") {
                          Ok(DataProperty(value: v, ..)) -> v
                          _ -> JsUndefined
                        }
                        let done = case dict.get(props, "done") {
                          Ok(DataProperty(value: JsBool(d), ..)) -> d
                          _ -> False
                        }
                        Ok(
                          State(
                            ..frame.merge_globals(state, next_state, []),
                            heap: next_state.heap,
                            stack: [
                              JsBool(done),
                              val,
                              JsObject(iter_ref),
                              ..rest
                            ],
                            pc: state.pc + 1,
                          ),
                        )
                      }
                      _ ->
                        Error(#(
                          StepVmError(Unimplemented(
                            "IteratorNext: generator .next() returned non-object",
                          )),
                          JsUndefined,
                          next_state.heap,
                        ))
                    }
                  _ ->
                    Error(#(
                      StepVmError(Unimplemented(
                        "IteratorNext: generator .next() empty stack",
                      )),
                      JsUndefined,
                      next_state.heap,
                    ))
                }
              }
            }
            _ ->
              Error(#(
                StepVmError(Unimplemented("IteratorNext: not an iterator")),
                JsUndefined,
                state.heap,
              ))
          }
        _ ->
          Error(#(
            StepVmError(StackUnderflow("IteratorNext")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    IteratorClose -> {
      // MVP: just pop the iterator from the stack
      case state.stack {
        [_, ..rest] -> Ok(State(..state, stack: rest, pc: state.pc + 1))
        _ ->
          Error(#(
            StepVmError(StackUnderflow("IteratorClose")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    _ ->
      Error(#(
        StepVmError(Unimplemented("step_iteration: " <> string.inspect(op))),
        JsUndefined,
        state.heap,
      ))
  }
}

fn step_generators(
  state: State,
  op: Op,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case op {
    InitialYield ->
      // Suspend immediately at start of generator body.
      // PC advances past InitialYield so resumption starts at the next op.
      Error(#(Yielded, JsUndefined, state.heap))

    Yield -> {
      // Pop value from stack and suspend the generator.
      // On resume, .next(arg) value will be pushed onto the stack.
      case state.stack {
        [yielded_value, ..] -> Error(#(Yielded, yielded_value, state.heap))
        [] -> Error(#(Yielded, JsUndefined, state.heap))
      }
    }

    Await -> {
      // Pop the awaited value from the stack and suspend the async function.
      case state.stack {
        [awaited_value, ..] -> Error(#(Yielded, awaited_value, state.heap))
        [] -> Error(#(Yielded, JsUndefined, state.heap))
      }
    }

    _ ->
      Error(#(
        StepVmError(Unimplemented("step_generators: " <> string.inspect(op))),
        JsUndefined,
        state.heap,
      ))
  }
}

fn step_special(
  state: State,
  op: Op,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case op {
    CreateArguments -> {
      // Allocate an unmapped arguments object from state.call_args.
      let args = state.call_args
      let length = list.length(args)
      let callee =
        state.callee_ref |> option.map(JsObject) |> option.unwrap(JsUndefined)
      let props =
        dict.from_list([
          #(
            "length",
            value.data(JsNumber(Finite(int.to_float(length))))
              |> value.writable
              |> value.configurable,
          ),
          #(
            "callee",
            value.data(callee) |> value.writable |> value.configurable,
          ),
        ])
      let #(heap, ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: value.ArgumentsObject(length:),
            properties: props,
            elements: js_elements.from_list(args),
            prototype: Some(state.builtins.object.prototype),
            symbol_properties: dict.new(),
            extensible: True,
          ),
        )
      Ok(
        State(
          ..state,
          heap:,
          stack: [JsObject(ref), ..state.stack],
          pc: state.pc + 1,
        ),
      )
    }

    // -- RegExp literal --
    NewRegExp -> {
      case state.stack {
        [JsString(flags), JsString(pattern), ..rest] -> {
          let #(heap, ref) =
            builtins_regexp.alloc_regexp(
              state.heap,
              state.builtins.regexp.prototype,
              pattern,
              flags,
            )
          Ok(
            State(
              ..state,
              stack: [JsObject(ref), ..rest],
              heap:,
              pc: state.pc + 1,
            ),
          )
        }
        _ ->
          Error(#(
            StepVmError(StackUnderflow("NewRegExp")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    _ ->
      Error(#(
        StepVmError(Unimplemented("step_special: " <> string.inspect(op))),
        JsUndefined,
        state.heap,
      ))
  }
}

// ============================================================================
// Call helpers
// ============================================================================

/// ES2024 §10.2.1.2 OrdinaryCallBindThis ( F, thisArgument )
///
/// The abstract operation OrdinaryCallBindThis binds the `this` value for an
/// ordinary function call based on the function's [[ThisMode]] internal slot.
///
/// Spec steps:
///   1. Let thisMode be F.[[ThisMode]].
///   2. If thisMode is LEXICAL, return unused.
///   3. Let calleeRealm be F.[[Realm]].
///   4. Let localEnv be the LexicalEnvironment of calleeContext.
///   5. If thisMode is STRICT, let thisValue be thisArgument.
///   6. Else (sloppy),
///      a. If thisArgument is undefined or null, then
///         i. Let globalEnv be calleeRealm.[[GlobalEnv]].
///         ii. Let thisValue be globalEnv.[[GlobalThisValue]].
///      b. Else, let thisValue be ! ToObject(thisArgument).
///   7-8. (Assertions about localEnv — not applicable here.)
///   9. Perform ! localEnv.BindThisValue(thisValue).
///   10. Return unused.
///
/// Our implementation threads the returned thisValue into the new call frame
/// directly via call_function. Arrow functions use is_arrow instead of
/// [[ThisMode]] = LEXICAL, inheriting the caller's this_binding from state.
/// Thin wrapper: delegates to call.call_function with execute_inner/unwind_to_catch.
fn call_function(
  state: State,
  fn_ref: value.Ref,
  env_ref: value.Ref,
  callee_template: FuncTemplate,
  args: List(JsValue),
  rest_stack: List(JsValue),
  this_val: JsValue,
  constructor_this: option.Option(JsValue),
  new_callee_ref: option.Option(Ref),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  call.call_function(
    state,
    fn_ref,
    env_ref,
    callee_template,
    args,
    rest_stack,
    this_val,
    constructor_this,
    new_callee_ref,
    execute_inner,
    unwind_to_catch,
  )
}

/// Thin wrapper: delegates to call.call_native with execute_inner/unwind_to_catch/dispatch_native.
fn call_native(
  state: State,
  native: value.NativeFnSlot,
  args: List(JsValue),
  rest_stack: List(JsValue),
  this: JsValue,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  call.call_native(
    state,
    native,
    args,
    rest_stack,
    this,
    execute_inner,
    unwind_to_catch,
    dispatch_native,
  )
}

/// Thin wrapper: delegates to call.do_construct with execute_inner/unwind_to_catch/dispatch_native.
fn do_construct(
  state: State,
  ctor_ref: Ref,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  call.do_construct(
    state,
    ctor_ref,
    args,
    rest_stack,
    execute_inner,
    unwind_to_catch,
    dispatch_native,
  )
}

/// Thin wrapper: delegates to call.call_value with execute_inner/unwind_to_catch/dispatch_native.
fn call_value(
  state: State,
  callee: JsValue,
  args: List(JsValue),
  this_val: JsValue,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  call.call_value(
    state,
    callee,
    args,
    this_val,
    execute_inner,
    unwind_to_catch,
    dispatch_native,
  )
}

/// reverse; caller doesn't care about order since result feeds a dict.
/// Thin wrapper: delegates to array_ops.assign_non_hole_indices.
fn assign_non_hole_indices(
  values: List(JsValue),
  holes: List(Int),
  index: Int,
  acc: List(#(Int, JsValue)),
) -> List(#(Int, JsValue)) {
  array_ops.assign_non_hole_indices(values, holes, index, acc)
}

/// Thin wrapper: delegates to array_ops.grow_array_length.
fn grow_array_length(h: Heap, ref: Ref) -> Heap {
  array_ops.grow_array_length(h, ref)
}

/// Thin wrapper: delegates to array_ops.push_onto_array.
fn push_onto_array(h: Heap, ref: Ref, val: JsValue) -> Heap {
  array_ops.push_onto_array(h, ref, val)
}

/// Thin wrapper: delegates to array_ops.spread_into_array with execute_inner/unwind_to_catch.
fn spread_into_array(
  state: State,
  target_ref: Ref,
  iterable: JsValue,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  array_ops.spread_into_array(
    state,
    target_ref,
    iterable,
    execute_inner,
    unwind_to_catch,
  )
}

/// Thin wrapper: delegates to call.extract_array_args.
fn extract_array_args(h: Heap, ref: Ref) -> List(JsValue) {
  call.extract_array_args(h, ref)
}

fn dispatch_native(
  native: value.NativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  call.dispatch_native(
    native,
    args,
    this,
    state,
    execute_inner,
    call_native,
    new_state,
  )
}

/// Get the Ref of a named property's JsObject value from a heap object.
/// Returns Error(Nil) if the object doesn't exist, the property is missing,
/// or the property value is not a JsObject.
fn get_field_ref(h: Heap, obj_ref: value.Ref, name: String) -> Option(value.Ref) {
  use slot <- option.then(heap.read(h, obj_ref))
  case slot {
    ObjectSlot(properties: props, ..) ->
      case dict.get(props, name) {
        Ok(DataProperty(value: JsObject(ref), ..)) -> Some(ref)
        _ -> None
      }
    _ -> None
  }
}

/// Update the prototype of a heap object in-place, returning the new heap.
/// If the ref doesn't point to an ObjectSlot, returns the heap unchanged.
fn set_slot_prototype(
  h: Heap,
  ref: value.Ref,
  new_proto: option.Option(value.Ref),
) -> Heap {
  use slot <- heap.update(h, ref)
  case slot {
    ObjectSlot(..) -> ObjectSlot(..slot, prototype: new_proto)
    _ -> slot
  }
}

/// Pop n items from stack. Returns #(popped_items_in_order, remaining_stack).
fn pop_n(
  stack: List(JsValue),
  n: Int,
) -> Option(#(List(JsValue), List(JsValue))) {
  pop_n_loop(stack, n, [])
}

fn pop_n_loop(
  stack: List(JsValue),
  remaining: Int,
  acc: List(JsValue),
) -> Option(#(List(JsValue), List(JsValue))) {
  case remaining {
    0 -> Some(#(acc, stack))
    _ ->
      case stack {
        [top, ..rest] -> pop_n_loop(rest, remaining - 1, [top, ..acc])
        [] -> None
      }
  }
}

/// BinOp Add with ToPrimitive for object operands.
/// ES2024 §13.15.3: ToPrimitive(default) both sides, then string-concat or numeric-add.
fn binop_add_with_to_primitive(
  state: State,
  left: JsValue,
  right: JsValue,
  rest: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  use #(lprim, s1) <- result.try(
    frame.rethrow(coerce.to_primitive(state, left, coerce.DefaultHint)),
  )
  use #(rprim, s2) <- result.try(
    frame.rethrow(coerce.to_primitive(s1, right, coerce.DefaultHint)),
  )
  case lprim, rprim {
    JsString(a), JsString(b) ->
      Ok(State(..s2, stack: [JsString(a <> b), ..rest], pc: state.pc + 1))
    JsString(a), _ -> {
      use #(b, s3) <- result.map(frame.rethrow(coerce.js_to_string(s2, rprim)))
      State(..s3, stack: [JsString(a <> b), ..rest], pc: state.pc + 1)
    }
    _, JsString(b) -> {
      use #(a, s3) <- result.map(frame.rethrow(coerce.js_to_string(s2, lprim)))
      State(..s3, stack: [JsString(a <> b), ..rest], pc: state.pc + 1)
    }
    _, _ -> {
      let a = operators.to_number_for_binop(lprim)
      let b = operators.to_number_for_binop(rprim)
      Ok(
        State(
          ..s2,
          stack: [JsNumber(operators.num_add(a, b)), ..rest],
          pc: state.pc + 1,
        ),
      )
    }
  }
}

/// Array/Arguments fast path for GetIterator: use ArrayIteratorSlot directly
/// if Symbol.iterator hasn't been overridden on the instance. If it has been
/// overridden, fall through to the spec-compliant Symbol.iterator lookup.
fn get_iterator_array_fast_path(
  state: State,
  ref: value.Ref,
  iterable: JsValue,
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let has_override = case heap.read(state.heap, ref) {
    Some(ObjectSlot(symbol_properties: sym_props, ..)) ->
      dict.has_key(sym_props, value.symbol_iterator)
    _ -> False
  }
  case has_override {
    False -> {
      let #(h, iter_ref) =
        heap.alloc(state.heap, ArrayIteratorSlot(source: ref, index: 0))
      Ok(
        State(
          ..state,
          stack: [JsObject(iter_ref), ..rest_stack],
          heap: h,
          pc: state.pc + 1,
        ),
      )
    }
    True -> get_iterator_via_symbol(state, ref, iterable, rest_stack)
  }
}

/// ES2024 §7.4.1 GetIterator(obj, kind) — look up Symbol.iterator and call it.
/// Used when the fast path (ArrayObject without overridden Symbol.iterator) doesn't apply.
fn get_iterator_via_symbol(
  state: State,
  ref: value.Ref,
  iterable: JsValue,
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  // Step 1: Let method be ? GetMethod(obj, @@iterator)
  case object.get_symbol_value(state, ref, value.symbol_iterator, iterable) {
    Ok(#(method, state)) ->
      case coerce.is_callable_value(state.heap, method) {
        True ->
          // Step 2: Let iterator be ? Call(method, obj)
          case frame.call(state, method, iterable, []) {
            Ok(#(iterator, state)) ->
              case iterator {
                JsObject(iter_ref) ->
                  Ok(
                    State(
                      ..state,
                      stack: [JsObject(iter_ref), ..rest_stack],
                      pc: state.pc + 1,
                    ),
                  )
                _ ->
                  frame.throw_type_error(
                    state,
                    "Iterator result is not an object",
                  )
              }
            Error(#(thrown, state)) -> Error(#(Thrown, thrown, state.heap))
          }
        False ->
          frame.throw_type_error(
            state,
            object.inspect(iterable, state.heap) <> " is not iterable",
          )
      }
    Error(#(_thrown, state)) ->
      frame.throw_type_error(
        state,
        object.inspect(iterable, state.heap) <> " is not iterable",
      )
  }
}
