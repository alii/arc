import arc/compiler
import arc/parser
import arc/vm/array
import arc/vm/builtins
import arc/vm/builtins/arc as builtins_arc
import arc/vm/builtins/array as builtins_array
import arc/vm/builtins/boolean as builtins_boolean
import arc/vm/builtins/common.{type Builtins}
import arc/vm/builtins/error as builtins_error
import arc/vm/builtins/json as builtins_json
import arc/vm/builtins/map as builtins_map
import arc/vm/builtins/math as builtins_math
import arc/vm/builtins/number as builtins_number
import arc/vm/builtins/object as builtins_object
import arc/vm/builtins/promise as builtins_promise
import arc/vm/builtins/regexp as builtins_regexp
import arc/vm/builtins/set as builtins_set
import arc/vm/builtins/string as builtins_string
import arc/vm/builtins/symbol as builtins_symbol
import arc/vm/builtins/weak_map as builtins_weak_map
import arc/vm/builtins/weak_set as builtins_weak_set
import arc/vm/coerce
import arc/vm/completion.{
  type Completion, NormalCompletion, ThrowCompletion, YieldCompletion,
}
import arc/vm/frame.{
  type State, type StepResult, type VmError, Done, LocalIndexOutOfBounds,
  SavedFrame, StackUnderflow, State, StepVmError, Thrown, TryFrame,
  Unimplemented, Yielded,
}
import arc/vm/generators
import arc/vm/heap.{type Heap}
import arc/vm/job_queue
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
import arc/vm/promises
import arc/vm/property_access
import arc/vm/value.{
  type FuncTemplate, type JsValue, type Ref, ArrayIteratorSlot, ArrayObject,
  AsyncFunctionSlot, DataProperty, Finite, ForInIteratorSlot, FunctionObject,
  GeneratorObject, GeneratorSlot, JsBool, JsNull, JsNumber, JsObject, JsString,
  JsUndefined, JsUninitialized, NativeFunction, ObjectSlot, OrdinaryObject,
  PromiseObject,
}
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set
import gleam/string

// ============================================================================
// Public types
// ============================================================================

/// Result of module evaluation — includes locals for export extraction.
pub type ModuleResult {
  ModuleOk(value: JsValue, heap: Heap, locals: array.Array(JsValue))
  ModuleThrow(value: JsValue, heap: Heap)
  ModuleError(error: VmError)
}

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
/// Delegates to run_handler_with_this for re-entrant JS function calls
/// from native code (e.g. Array.prototype.map's callback invocation).
fn call_fn_callback(
  state: State,
  callee: JsValue,
  this_val: JsValue,
  args: List(JsValue),
) -> Result(#(JsValue, State), #(JsValue, State)) {
  run_handler_with_this(state, callee, this_val, args)
}

// ============================================================================
// Public API
// ============================================================================

/// Create a fresh VM state from a function template.
/// Most callers can use this directly; override fields with `State(..new_state(...), ...)`
/// for cases that need non-default this_binding or symbol_descriptions.
fn new_state(
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

fn init_state(
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

/// Run a function template with a globalThis object, then drain jobs.
/// When event_loop is True, runs the mailbox-backed event loop (blocking
/// until `outstanding` hits zero); otherwise just drains the microtask queue.
pub fn run(
  func: FuncTemplate,
  heap: Heap,
  builtins: Builtins,
  global_object: Ref,
  event_loop: Bool,
) -> Result(Completion, VmError) {
  let result =
    init_state(func, heap, builtins, global_object, False, event_loop)
    |> execute_inner()
  use #(completion, final_state) <- result.try(result)
  let drained_state = job_queue.finish(final_state)
  case completion {
    NormalCompletion(val, _) -> Ok(NormalCompletion(val, drained_state.heap))
    ThrowCompletion(val, _) -> Ok(ThrowCompletion(val, drained_state.heap))
    YieldCompletion(_, _) ->
      panic as "YieldCompletion should not appear at script level"
  }
}

/// Run a module template with imports as lexical globals.
/// Module `this` is undefined per ES §16.2.1.5.2.
pub fn run_module_with_imports(
  func: FuncTemplate,
  heap: Heap,
  builtins: Builtins,
  global_object: Ref,
  import_globals: dict.Dict(String, JsValue),
  event_loop: Bool,
) -> ModuleResult {
  let locals = array.repeat(JsUndefined, func.local_count)
  let state =
    State(
      ..new_state(
        func,
        locals,
        heap,
        builtins,
        global_object,
        import_globals,
        set.new(),
        dict.new(),
        dict.new(),
        event_loop,
      ),
      this_binding: JsUndefined,
    )
  let result = execute_inner(state)
  case result {
    Error(vm_err) -> ModuleError(error: vm_err)
    Ok(#(completion, final_state)) -> {
      let drained_state = job_queue.finish(final_state)
      case completion {
        NormalCompletion(val, _) ->
          ModuleOk(
            value: val,
            heap: drained_state.heap,
            locals: drained_state.locals,
          )
        ThrowCompletion(val, _) ->
          ModuleThrow(value: val, heap: drained_state.heap)
        YieldCompletion(_, _) ->
          panic as "YieldCompletion should not appear at module level"
      }
    }
  }
}

/// Persistent REPL environment carried between evaluations.
pub type ReplEnv {
  ReplEnv(
    global_object: Ref,
    lexical_globals: dict.Dict(String, JsValue),
    const_lexical_globals: set.Set(String),
    symbol_descriptions: dict.Dict(value.SymbolId, String),
    symbol_registry: dict.Dict(String, value.SymbolId),
    /// Realm builtins registry, keyed by RealmSlot ref.
    /// Persisted so $262.evalScript/createRealm work across REPL evaluations.
    realms: dict.Dict(Ref, Builtins),
  )
}

/// Like vm.run, but persists globals across calls.
/// Used by the REPL so var declarations and function definitions survive.
pub fn run_and_drain_repl(
  func: FuncTemplate,
  heap: Heap,
  builtins: Builtins,
  env: ReplEnv,
) -> Result(#(Completion, ReplEnv), VmError) {
  let locals = array.repeat(JsUndefined, func.local_count)
  let state =
    State(
      ..new_state(
        func,
        locals,
        heap,
        builtins,
        env.global_object,
        env.lexical_globals,
        env.const_lexical_globals,
        env.symbol_descriptions,
        env.symbol_registry,
        False,
      ),
      realms: env.realms,
    )
  use #(completion, final_state) <- result.try(execute_inner(state))
  let drained_state = job_queue.drain_jobs(final_state)
  let new_env =
    ReplEnv(
      global_object: drained_state.global_object,
      lexical_globals: drained_state.lexical_globals,
      const_lexical_globals: drained_state.const_lexical_globals,
      symbol_descriptions: drained_state.symbol_descriptions,
      symbol_registry: drained_state.symbol_registry,
      realms: drained_state.realms,
    )
  case completion {
    NormalCompletion(val, _) ->
      Ok(#(NormalCompletion(val, drained_state.heap), new_env))
    ThrowCompletion(val, _) ->
      Ok(#(ThrowCompletion(val, drained_state.heap), new_env))
    YieldCompletion(_, _) ->
      panic as "YieldCompletion should not appear at script level"
  }
}

/// Get the fulfilled value of a promise JsValue, or Error if not fulfilled.
pub fn promise_result(h: Heap, val: JsValue) -> Option(JsValue) {
  case val {
    JsObject(ref) ->
      case heap.read(h, ref) {
        Some(ObjectSlot(kind: value.PromiseObject(promise_data:), ..)) ->
          case heap.read(h, promise_data) {
            Some(value.PromiseSlot(state: value.PromiseFulfilled(v), ..)) ->
              Some(v)
            Some(value.PromiseSlot(state: value.PromiseRejected(r), ..)) ->
              Some(r)
            _ -> None
          }
        _ -> None
      }
    _ -> None
  }
}

// ============================================================================
// Execution loop
// ============================================================================

/// Main execution loop. Tail-recursive.
/// Returns the completion and the final state (for job queue access).
fn execute_inner(state: State) -> Result(#(Completion, State), VmError) {
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
                  case array.get(parent_index, state.locals) {
                    Some(val) -> val
                    None -> JsUndefined
                  }
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
          let #(heap, fn_properties, proto_ref) = case child_template.is_arrow {
            True -> #(
              heap,
              dict.from_list([
                #(
                  "name",
                  common.fn_name_property(case child_template.name {
                    Some(n) -> n
                    None -> ""
                  }),
                ),
                #("length", common.fn_length_property(child_template.arity)),
              ]),
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
                  #(
                    "name",
                    common.fn_name_property(case child_template.name {
                      Some(n) -> n
                      None -> ""
                    }),
                  ),
                  #("length", common.fn_length_property(child_template.arity)),
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
        Some(JsObject(box_ref)) -> {
          case heap.read(state.heap, box_ref) {
            Some(value.BoxSlot(val)) ->
              Ok(State(..state, stack: [val, ..state.stack], pc: state.pc + 1))
            _ ->
              Error(#(
                StepVmError(Unimplemented("GetBoxed: not a BoxSlot")),
                JsUndefined,
                state.heap,
              ))
          }
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

    // -- Property access --
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

    // -- this / method calls / constructors --
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

    // -- Iteration --
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
                Some(ObjectSlot(kind: ArrayObject(_), ..))
                | Some(ObjectSlot(kind: value.ArgumentsObject(_), ..)) -> {
                  // Lazy iterator — stores source ref + index, reads elements one at a time.
                  // ArgumentsObject uses the same iterator slot shape (ArrayIteratorSlot
                  // reads from elements dict via js_elements.get, which works for both).
                  let #(heap, iter_ref) =
                    heap.alloc(
                      state.heap,
                      ArrayIteratorSlot(source: ref, index: 0),
                    )
                  Ok(
                    State(
                      ..state,
                      stack: [JsObject(iter_ref), ..rest],
                      heap:,
                      pc: state.pc + 1,
                    ),
                  )
                }
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
                // Non-array object — throw TypeError
                _ ->
                  frame.throw_type_error(
                    state,
                    object.inspect(JsObject(ref), state.heap)
                      <> " is not iterable",
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
              let #(length, elements) = case heap.read(state.heap, source) {
                Some(ObjectSlot(kind: ArrayObject(len), elements: elems, ..))
                | Some(ObjectSlot(
                    kind: value.ArgumentsObject(len),
                    elements: elems,
                    ..,
                  )) -> #(len, elems)
                _ -> #(0, js_elements.new())
              }
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
              // Use a temporary stack with just the iterator, since
              // call_native_generator_next will push result onto rest_stack.
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
                            ..state,
                            heap: next_state.heap,
                            stack: [
                              JsBool(done),
                              val,
                              JsObject(iter_ref),
                              ..rest
                            ],
                            pc: state.pc + 1,
                            lexical_globals: next_state.lexical_globals,
                            const_lexical_globals: next_state.const_lexical_globals,
                            job_queue: next_state.job_queue,
                            pending_receivers: next_state.pending_receivers,
                            outstanding: next_state.outstanding,
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

    // -- Generator / Async suspension --
    InitialYield ->
      // Suspend immediately at start of generator body.
      // PC advances past InitialYield so resumption starts at the next op.
      Error(#(Yielded, JsUndefined, state.heap))

    Yield -> {
      // Pop value from stack and suspend the generator.
      // On resume, .next(arg) value will be pushed onto the stack.
      // Note: the actual stack pop happens in execute_inner's Yielded handler,
      // not here — we just extract the value to return.
      case state.stack {
        [yielded_value, ..] -> Error(#(Yielded, yielded_value, state.heap))
        [] -> Error(#(Yielded, JsUndefined, state.heap))
      }
    }

    Await -> {
      // Pop the awaited value from the stack and suspend the async function.
      // The caller wraps it in Promise.resolve() and attaches .then() callbacks
      // to resume execution when settled.
      // Note: the actual stack pop happens in execute_inner's Yielded handler.
      case state.stack {
        [awaited_value, ..] -> Error(#(Yielded, awaited_value, state.heap))
        [] -> Error(#(Yielded, JsUndefined, state.heap))
      }
    }

    CreateArguments -> {
      // Allocate an unmapped arguments object from state.call_args.
      // ES §10.4.4.7 CreateUnmappedArgumentsObject — no param aliasing.
      // Properties: indices (w:t, e:t, c:t), length (w:t, e:f, c:t),
      // callee (w:t, e:f, c:t) — pointing to the current function ref.
      // Prototype: Object.prototype. Tag: "Arguments" (via ExoticKind).
      let args = state.call_args
      let length = list.length(args)
      let callee = case state.callee_ref {
        Some(r) -> JsObject(r)
        None -> JsUndefined
      }
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

    // -- Spread element support (array literals + calls) --
    // These are emitted only when a SpreadElement appears; the no-spread
    // paths still use the static-arity ArrayFrom/Call/CallMethod/CallConstructor.
    ArrayPush -> {
      // [val, arr] → [arr]; arr[arr.length] = val, length++.
      // Used for non-spread elements that appear after a spread in an array
      // literal (e.g. the `3` in `[1, ...x, 3]`).
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
      // [arr] → [arr]; length++ WITHOUT setting any element. Creates a hole
      // at the previous length. Used for elisions after the first spread in
      // an array literal (e.g. the hole in `[1, ...x, , 3]`).
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
      // [iterable, arr] → [arr]; drain iterable via the iterator protocol,
      // appending each yielded value to arr. Per ArrayAccumulation (ES §13.2.4.1):
      // GetIterator(spreadObj, sync), then loop IteratorStepValue → CreateDataProperty.
      // Unlike object spread (CopyDataProperties), null/undefined throw.
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

    CallApply -> {
      // [args_array, callee] → [result]; this=undefined.
      // Spread-call path: f(a, ...b, c) builds an args array and calls here.
      // extract_array_args reads holes as undefined, matching spec for arg-list
      // spread (iterator visits all indices).
      case state.stack {
        [JsObject(args_ref), callee, ..rest] -> {
          let args = extract_array_args(state.heap, args_ref)
          call_value(State(..state, stack: rest), callee, args, JsUndefined)
        }
        [_, callee, ..] -> {
          // args "array" is not an object — shouldn't happen for compiler-emitted
          // spread, but handle gracefully: zero args.
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
      // Spread-method-call path: obj.m(...x).
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
      // [args_array, ctor] → [new instance]. Spread-new path: new F(...x).
      // Shares full constructor dispatch (derived/base/bound/native) with
      // the static-arity CallConstructor via do_construct.
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

    _ ->
      Error(#(
        StepVmError(Unimplemented("opcode: " <> string.inspect(op))),
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
fn bind_this(
  state: State,
  callee: FuncTemplate,
  this_arg: JsValue,
) -> #(Heap, JsValue) {
  case callee.is_arrow {
    // Step 2: thisMode is LEXICAL → return caller's this_binding unchanged.
    True -> #(state.heap, state.this_binding)
    False ->
      case callee.is_strict {
        // Step 5: thisMode is STRICT → thisValue = thisArgument (no coercion).
        True -> #(state.heap, this_arg)
        // Step 6: Sloppy mode coercion.
        False ->
          case this_arg {
            // Step 6a: undefined/null → globalThis.
            JsUndefined | value.JsNull -> #(
              state.heap,
              JsObject(state.global_object),
            )
            // Step 6b: Objects pass through (ToObject is identity for objects).
            JsObject(_) -> #(state.heap, this_arg)
            _ ->
              // Step 6b: Primitives → ToObject wrapper (boxing).
              // to_object only errors on null/undefined which we handled above.
              case common.to_object(state.heap, state.builtins, this_arg) {
                Some(#(heap, ref)) -> #(heap, JsObject(ref))
                None -> #(state.heap, this_arg)
              }
          }
      }
  }
}

/// Shared logic for Call, CallMethod, and CallConstructor.
/// Looks up the callee template, saves the caller frame, sets up locals,
/// and transitions to the callee's code.
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
  let #(heap, this_val) = bind_this(state, callee_template, this_val)
  let state = State(..state, heap:)
  case callee_template.is_generator, callee_template.is_async {
    // Note: async generators (True, True) not yet implemented — they'll
    // fall through to call_generator_function which is incorrect but harmless
    // until async generators are properly supported.
    True, _ ->
      call_generator_function(
        state,
        fn_ref,
        env_ref,
        callee_template,
        args,
        rest_stack,
        this_val,
      )
    _, True ->
      call_async_function(
        state,
        fn_ref,
        env_ref,
        callee_template,
        args,
        rest_stack,
        this_val,
      )
    _, _ ->
      call_regular_function(
        state,
        fn_ref,
        env_ref,
        callee_template,
        args,
        rest_stack,
        this_val,
        constructor_this,
        new_callee_ref,
      )
  }
}

/// Regular (non-generator) function call: save frame, enter callee.
const max_call_depth: Int = 10_000

fn call_regular_function(
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
  use <- bool.lazy_guard(state.call_depth >= max_call_depth, fn() {
    frame.throw_range_error(state, "Maximum call stack size exceeded")
  })
  // Save caller frame
  let saved =
    SavedFrame(
      func: state.func,
      locals: state.locals,
      stack: rest_stack,
      pc: state.pc + 1,
      try_stack: state.try_stack,
      this_binding: state.this_binding,
      constructor_this:,
      callee_ref: state.callee_ref,
      call_args: state.call_args,
    )
  let locals = setup_locals(state.heap, env_ref, callee_template, args)
  // Arrow functions inherit this from their enclosing scope
  let new_this = case callee_template.is_arrow {
    True -> state.this_binding
    False -> this_val
  }
  // For arguments.callee: constructors already pass new_callee_ref=Some(ctor_ref),
  // regular calls pass None — fall back to fn_ref so arguments.callee works.
  let effective_callee_ref = case new_callee_ref {
    Some(_) -> new_callee_ref
    None -> Some(fn_ref)
  }
  Ok(
    State(
      ..state,
      stack: [],
      locals:,
      func: callee_template,
      code: callee_template.bytecode,
      constants: callee_template.constants,
      pc: 0,
      call_stack: [saved, ..state.call_stack],
      call_depth: state.call_depth + 1,
      try_stack: [],
      this_binding: new_this,
      callee_ref: effective_callee_ref,
      call_args: args,
    ),
  )
}

/// Generator function call: execute until InitialYield, save state to
/// GeneratorSlot, create GeneratorObject, return it to caller.
fn call_generator_function(
  state: State,
  fn_ref: value.Ref,
  env_ref: value.Ref,
  callee_template: FuncTemplate,
  args: List(JsValue),
  rest_stack: List(JsValue),
  this_val: JsValue,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let locals = setup_locals(state.heap, env_ref, callee_template, args)
  // Set up an isolated execution state for the generator body
  let gen_state =
    State(
      ..state,
      stack: [],
      locals:,
      func: callee_template,
      code: callee_template.bytecode,
      constants: callee_template.constants,
      pc: 0,
      call_stack: [],
      try_stack: [],
      finally_stack: [],
      this_binding: this_val,
      callee_ref: Some(fn_ref),
      call_args: args,
    )
  // Execute until InitialYield (which fires immediately at the start)
  case execute_inner(gen_state) {
    Ok(#(YieldCompletion(_, _), suspended)) -> {
      // Save the suspended state into a GeneratorSlot on the heap
      let #(saved_try, saved_finally) =
        generators.save_stacks(suspended.try_stack, suspended.finally_stack)
      let #(h, data_ref) =
        heap.alloc(
          suspended.heap,
          GeneratorSlot(
            gen_state: value.SuspendedStart,
            func_template: callee_template,
            env_ref:,
            saved_pc: suspended.pc,
            saved_locals: suspended.locals,
            saved_stack: suspended.stack,
            saved_try_stack: saved_try,
            saved_finally_stack: saved_finally,
            saved_this: suspended.this_binding,
            saved_callee_ref: suspended.callee_ref,
          ),
        )
      // Create the generator object with Generator.prototype
      let #(h, gen_obj_ref) =
        heap.alloc(
          h,
          ObjectSlot(
            kind: GeneratorObject(generator_data: data_ref),
            properties: dict.new(),
            elements: js_elements.new(),
            prototype: Some(state.builtins.generator.prototype),
            symbol_properties: dict.new(),
            extensible: True,
          ),
        )
      // Return to caller with the generator object on the stack
      Ok(
        State(
          ..state,
          heap: h,
          stack: [JsObject(gen_obj_ref), ..rest_stack],
          pc: state.pc + 1,
          lexical_globals: suspended.lexical_globals,
          const_lexical_globals: suspended.const_lexical_globals,
          job_queue: suspended.job_queue,
          pending_receivers: suspended.pending_receivers,
          outstanding: suspended.outstanding,
        ),
      )
    }
    Ok(#(NormalCompletion(_, h), _)) -> {
      // Generator returned without yielding — shouldn't happen with InitialYield
      // but handle gracefully: create a completed generator
      let #(h, data_ref) =
        heap.alloc(
          h,
          GeneratorSlot(
            gen_state: value.Completed,
            func_template: callee_template,
            env_ref:,
            saved_pc: 0,
            saved_locals: array.from_list([]),
            saved_stack: [],
            saved_try_stack: [],
            saved_finally_stack: [],
            saved_this: JsUndefined,
            saved_callee_ref: None,
          ),
        )
      let #(h, gen_obj_ref) =
        heap.alloc(
          h,
          ObjectSlot(
            kind: GeneratorObject(generator_data: data_ref),
            properties: dict.new(),
            elements: js_elements.new(),
            prototype: Some(state.builtins.generator.prototype),
            symbol_properties: dict.new(),
            extensible: True,
          ),
        )
      Ok(
        State(
          ..state,
          heap: h,
          stack: [JsObject(gen_obj_ref), ..rest_stack],
          pc: state.pc + 1,
        ),
      )
    }
    Ok(#(ThrowCompletion(thrown, h), _)) -> Error(#(Thrown, thrown, h))
    Error(vm_err) -> Error(#(StepVmError(vm_err), JsUndefined, state.heap))
  }
}

/// Async function call: create a promise, execute body eagerly.
/// If the body completes synchronously, resolve/reject the promise immediately.
/// If the body hits `await`, save state and set up promise callbacks to resume.
fn call_async_function(
  state: State,
  fn_ref: value.Ref,
  env_ref: value.Ref,
  callee_template: FuncTemplate,
  args: List(JsValue),
  rest_stack: List(JsValue),
  this_val: JsValue,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  // Create the outer promise that the async function returns
  let #(h, promise_ref, data_ref) =
    builtins_promise.create_promise(
      state.heap,
      state.builtins.promise.prototype,
    )
  let #(h, resolve_fn, reject_fn) =
    builtins_promise.create_resolving_functions(
      h,
      state.builtins.function.prototype,
      promise_ref,
      data_ref,
    )
  // Set up locals and execute body eagerly
  let locals = setup_locals(h, env_ref, callee_template, args)
  let async_state =
    State(
      ..state,
      heap: h,
      stack: [],
      locals:,
      func: callee_template,
      code: callee_template.bytecode,
      constants: callee_template.constants,
      pc: 0,
      call_stack: [],
      try_stack: [],
      finally_stack: [],
      this_binding: this_val,
      callee_ref: Some(fn_ref),
      call_args: args,
    )
  case execute_inner(async_state) {
    Ok(#(YieldCompletion(awaited_value, h2), suspended)) -> {
      // Body hit `await` — save state, set up promise resolution
      let #(saved_try, saved_finally) =
        generators.save_stacks(suspended.try_stack, suspended.finally_stack)
      let #(h2, async_data_ref) =
        heap.alloc(
          h2,
          AsyncFunctionSlot(
            promise_data_ref: data_ref,
            resolve: resolve_fn,
            reject: reject_fn,
            func_template: callee_template,
            env_ref:,
            saved_pc: suspended.pc,
            saved_locals: suspended.locals,
            saved_stack: suspended.stack,
            saved_try_stack: saved_try,
            saved_finally_stack: saved_finally,
            saved_this: suspended.this_binding,
            saved_callee_ref: suspended.callee_ref,
          ),
        )
      let state =
        async_setup_await(
          State(
            ..state,
            heap: h2,
            lexical_globals: suspended.lexical_globals,
            const_lexical_globals: suspended.const_lexical_globals,
            job_queue: suspended.job_queue,
            pending_receivers: suspended.pending_receivers,
            outstanding: suspended.outstanding,
          ),
          async_data_ref,
          awaited_value,
        )
      Ok(
        State(
          ..state,
          stack: [JsObject(promise_ref), ..rest_stack],
          pc: state.pc + 1,
        ),
      )
    }
    Ok(#(NormalCompletion(return_value, h2), final_state)) -> {
      // Async function completed without awaiting — resolve the promise
      let #(h2, jobs) =
        builtins_promise.fulfill_promise(h2, data_ref, return_value)
      Ok(
        State(
          ..state,
          heap: h2,
          stack: [JsObject(promise_ref), ..rest_stack],
          pc: state.pc + 1,
          lexical_globals: final_state.lexical_globals,
          const_lexical_globals: final_state.const_lexical_globals,
          job_queue: list.append(final_state.job_queue, jobs),
          pending_receivers: final_state.pending_receivers,
          outstanding: final_state.outstanding,
        ),
      )
    }
    Ok(#(ThrowCompletion(thrown, h2), final_state)) -> {
      // Async function threw without awaiting — reject the promise
      let state =
        builtins_promise.reject_promise(
          State(
            ..state,
            heap: h2,
            lexical_globals: final_state.lexical_globals,
            const_lexical_globals: final_state.const_lexical_globals,
            pending_receivers: final_state.pending_receivers,
            outstanding: final_state.outstanding,
          ),
          data_ref,
          thrown,
        )
      Ok(
        State(
          ..state,
          stack: [JsObject(promise_ref), ..rest_stack],
          pc: state.pc + 1,
        ),
      )
    }
    Error(vm_err) -> Error(#(StepVmError(vm_err), JsUndefined, state.heap))
  }
}

/// Set up promise resolution for an awaited value in an async function.
/// Wraps the value in Promise.resolve(), creates resume callbacks, attaches .then().
/// Returns the updated state with heap and job_queue modified.
fn async_setup_await(
  state: State,
  async_data_ref: Ref,
  awaited_value: JsValue,
) -> State {
  let h = state.heap
  let builtins = state.builtins
  // Wrap awaited_value in Promise.resolve() if not already a promise
  let #(h, promise_data_ref) = case awaited_value {
    JsObject(ref) ->
      case heap.read(h, ref) {
        Some(ObjectSlot(kind: PromiseObject(pdata_ref), ..)) -> #(h, pdata_ref)
        _ -> {
          let #(h, _, dr) =
            promises.create_resolved_promise(h, builtins, awaited_value)
          #(h, dr)
        }
      }
    _ -> {
      let #(h, _, dr) =
        promises.create_resolved_promise(h, builtins, awaited_value)
      #(h, dr)
    }
  }
  // Create NativeAsyncResume callbacks
  let #(h, fulfill_resume_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(
          value.Call(value.AsyncResume(async_data_ref:, is_reject: False)),
        ),
        properties: dict.new(),
        elements: js_elements.new(),
        prototype: Some(builtins.function.prototype),
        symbol_properties: dict.new(),
        extensible: True,
      ),
    )
  let #(h, reject_resume_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(
          value.Call(value.AsyncResume(async_data_ref:, is_reject: True)),
        ),
        properties: dict.new(),
        elements: js_elements.new(),
        prototype: Some(builtins.function.prototype),
        symbol_properties: dict.new(),
        extensible: True,
      ),
    )
  // Attach .then(fulfillResume, rejectResume) to the awaited promise
  let #(h, child_ref, child_data_ref) =
    builtins_promise.create_promise(h, builtins.promise.prototype)
  let #(h, child_resolve, child_reject) =
    builtins_promise.create_resolving_functions(
      h,
      builtins.function.prototype,
      child_ref,
      child_data_ref,
    )
  builtins_promise.perform_promise_then(
    State(..state, heap: h),
    promise_data_ref,
    JsObject(fulfill_resume_ref),
    JsObject(reject_resume_ref),
    child_resolve,
    child_reject,
  )
}

/// NativeAsyncResume handler: called when an awaited promise settles.
/// Restores the async function's execution state and continues.
fn call_native_async_resume(
  state: State,
  async_data_ref: Ref,
  is_reject: Bool,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let settled_value = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  case heap.read(state.heap, async_data_ref) {
    Some(AsyncFunctionSlot(
      promise_data_ref:,
      resolve: slot_resolve,
      reject: slot_reject,
      func_template:,
      env_ref: slot_env_ref,
      saved_pc:,
      saved_locals:,
      saved_stack:,
      saved_try_stack:,
      saved_finally_stack:,
      saved_this:,
      saved_callee_ref:,
    )) -> {
      // Restore try/finally stacks
      let #(restored_try, restored_finally) =
        generators.restore_stacks(saved_try_stack, saved_finally_stack)
      // Build the resume stack: push resolved value for fulfillment
      let resume_stack = case is_reject {
        False -> [settled_value, ..saved_stack]
        True -> saved_stack
      }
      let exec_state =
        State(
          ..state,
          stack: resume_stack,
          locals: saved_locals,
          func: func_template,
          code: func_template.bytecode,
          constants: func_template.constants,
          pc: saved_pc,
          call_stack: [],
          try_stack: restored_try,
          finally_stack: restored_finally,
          this_binding: saved_this,
          callee_ref: saved_callee_ref,
          // arguments was created before first await; post-resume never needs call_args
          call_args: [],
        )
      // For rejection, throw the value so try/catch inside async fn can handle it
      let exec_result = case is_reject {
        False -> execute_inner(exec_state)
        True -> {
          case unwind_to_catch(exec_state, settled_value) {
            Some(caught_state) -> execute_inner(caught_state)
            None ->
              Ok(#(ThrowCompletion(settled_value, exec_state.heap), exec_state))
          }
        }
      }
      case exec_result {
        Ok(#(NormalCompletion(return_value, h2), final_state)) -> {
          // Async function completed — resolve the outer promise
          let #(h2, jobs) =
            builtins_promise.fulfill_promise(h2, promise_data_ref, return_value)
          Ok(
            State(
              ..state,
              heap: h2,
              stack: [JsUndefined, ..rest_stack],
              pc: state.pc + 1,
              lexical_globals: final_state.lexical_globals,
              const_lexical_globals: final_state.const_lexical_globals,
              job_queue: list.append(final_state.job_queue, jobs),
              pending_receivers: final_state.pending_receivers,
              outstanding: final_state.outstanding,
            ),
          )
        }
        Ok(#(ThrowCompletion(thrown, h2), final_state)) -> {
          // Async function threw — reject the outer promise
          let state =
            builtins_promise.reject_promise(
              State(
                ..state,
                heap: h2,
                lexical_globals: final_state.lexical_globals,
                const_lexical_globals: final_state.const_lexical_globals,
                pending_receivers: final_state.pending_receivers,
                outstanding: final_state.outstanding,
              ),
              promise_data_ref,
              thrown,
            )
          Ok(
            State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1),
          )
        }
        Ok(#(YieldCompletion(awaited_value, h2), suspended)) -> {
          // Hit another `await` — save state and set up promise resolution
          let #(saved_try, saved_finally) =
            generators.save_stacks(suspended.try_stack, suspended.finally_stack)
          let h2 =
            heap.write(
              h2,
              async_data_ref,
              AsyncFunctionSlot(
                promise_data_ref:,
                resolve: slot_resolve,
                reject: slot_reject,
                func_template:,
                env_ref: slot_env_ref,
                saved_pc: suspended.pc,
                saved_locals: suspended.locals,
                saved_stack: suspended.stack,
                saved_try_stack: saved_try,
                saved_finally_stack: saved_finally,
                saved_this: suspended.this_binding,
                saved_callee_ref: suspended.callee_ref,
              ),
            )
          let state =
            async_setup_await(
              State(
                ..state,
                heap: h2,
                lexical_globals: suspended.lexical_globals,
                const_lexical_globals: suspended.const_lexical_globals,
                job_queue: suspended.job_queue,
                pending_receivers: suspended.pending_receivers,
                outstanding: suspended.outstanding,
              ),
              async_data_ref,
              awaited_value,
            )
          Ok(
            State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1),
          )
        }
        Error(vm_err) -> Error(#(StepVmError(vm_err), JsUndefined, state.heap))
      }
    }
    _ ->
      Error(#(
        StepVmError(Unimplemented(
          "async resume: invalid slot for ref "
          <> string.inspect(async_data_ref),
        )),
        JsUndefined,
        state.heap,
      ))
  }
}

/// Set up locals for a function call: [env_values, padded_args, uninitialized].
fn setup_locals(
  h: Heap,
  env_ref: value.Ref,
  callee_template: FuncTemplate,
  args: List(JsValue),
) -> array.Array(JsValue) {
  let env_values = case heap.read(h, env_ref) {
    Some(value.EnvSlot(slots)) -> slots
    _ -> []
  }
  let env_count = list.length(env_values)
  let padded_args = pad_args(args, callee_template.arity)
  let remaining =
    callee_template.local_count - env_count - callee_template.arity
  list.flatten([
    env_values,
    padded_args,
    list.repeat(JsUndefined, remaining),
  ])
  |> array.from_list
}

/// Call a native (Gleam-implemented) function. Most natives execute synchronously
/// and push their result onto the stack. However, call/apply/bind need special
/// handling because they invoke other functions (potentially pushing call frames).
fn call_native(
  state: State,
  native: value.NativeFnSlot,
  args: List(JsValue),
  rest_stack: List(JsValue),
  this: JsValue,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case native {
    // Function.prototype.call(thisArg, ...args)
    // `this` is the target function, args[0] is the thisArg
    value.Call(value.FunctionCall) -> {
      let #(this_arg, call_args) = case args {
        [t, ..rest] -> #(t, rest)
        [] -> #(JsUndefined, [])
      }
      call_value(State(..state, stack: rest_stack), this, call_args, this_arg)
    }
    // Function.prototype.apply(thisArg, argsArray)
    // `this` is the target function, args[0] is thisArg, args[1] is array
    value.Call(value.FunctionApply) -> {
      let this_arg = case args {
        [t, ..] -> t
        _ -> JsUndefined
      }
      let call_args = case args {
        [_, JsObject(arr_ref), ..] -> extract_array_args(state.heap, arr_ref)
        // null/undefined argsArray → no args
        _ -> []
      }
      call_value(State(..state, stack: rest_stack), this, call_args, this_arg)
    }
    // Function.prototype.bind(thisArg, ...args)
    // Creates a bound function object
    value.Call(value.FunctionBind) -> {
      let #(this_arg, bound_args) = case args {
        [t, ..rest] -> #(t, rest)
        [] -> #(JsUndefined, [])
      }
      case this {
        JsObject(target_ref) -> {
          // Get the target's name for "bound <name>"
          let name = case heap.read(state.heap, target_ref) {
            Some(ObjectSlot(properties:, ..)) ->
              case dict.get(properties, "name") {
                Ok(DataProperty(value: JsString(n), ..)) -> "bound " <> n
                _ -> "bound "
              }
            _ -> "bound "
          }
          let #(h, bound_ref) =
            heap.alloc(
              state.heap,
              ObjectSlot(
                kind: NativeFunction(
                  value.Call(value.BoundFunction(
                    target: target_ref,
                    bound_this: this_arg,
                    bound_args:,
                  )),
                ),
                properties: dict.from_list([
                  #("name", common.fn_name_property(name)),
                ]),
                elements: js_elements.new(),
                prototype: Some(state.builtins.function.prototype),
                symbol_properties: dict.new(),
                extensible: True,
              ),
            )
          Ok(
            State(
              ..state,
              heap: h,
              stack: [JsObject(bound_ref), ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        }
        _ -> {
          frame.throw_type_error(state, "Bind must be called on a function")
        }
      }
    }
    // Bound function: prepend bound_args, use bound_this
    value.Call(value.BoundFunction(target:, bound_this:, bound_args:)) -> {
      let final_args = list.append(bound_args, args)
      call_value(
        State(..state, stack: rest_stack),
        JsObject(target),
        final_args,
        bound_this,
      )
    }
    // Promise constructor: new Promise(executor)
    value.Call(value.PromiseConstructor) ->
      promises.call_native_promise_constructor(state, args, rest_stack)
    // Promise resolve/reject internal functions
    value.Call(value.PromiseResolveFunction(
      promise_ref:,
      data_ref:,
      already_resolved_ref:,
    )) ->
      promises.call_native_promise_resolve_fn(
        state,
        promise_ref,
        data_ref,
        already_resolved_ref,
        args,
        rest_stack,
      )
    value.Call(value.PromiseRejectFunction(
      promise_ref: _,
      data_ref:,
      already_resolved_ref:,
    )) ->
      promises.call_native_promise_reject_fn(
        state,
        data_ref,
        already_resolved_ref,
        args,
        rest_stack,
      )
    // Promise.prototype.then(onFulfilled, onRejected)
    value.Call(value.PromiseThen) ->
      promises.call_native_promise_then(state, this, args, rest_stack)
    // Promise.prototype.catch(onRejected) — sugar for .then(undefined, onRejected)
    value.Call(value.PromiseCatch) ->
      promises.call_native_promise_then(
        state,
        this,
        [JsUndefined, ..args],
        rest_stack,
      )
    // Promise.prototype.finally(onFinally)
    value.Call(value.PromiseFinally) ->
      promises.call_native_promise_finally(state, this, args, rest_stack)
    // Promise.resolve(value)
    value.Call(value.PromiseResolveStatic) ->
      promises.call_native_promise_resolve_static(state, args, rest_stack)
    // Promise.reject(reason)
    value.Call(value.PromiseRejectStatic) ->
      promises.call_native_promise_reject_static(state, args, rest_stack)
    // Promise.all(iterable)
    value.Call(value.PromiseAllStatic) ->
      promises.call_native_promise_all(state, args, rest_stack)
    // Promise.race(iterable)
    value.Call(value.PromiseRaceStatic) ->
      promises.call_native_promise_race(state, args, rest_stack)
    // Promise.allSettled(iterable)
    value.Call(value.PromiseAllSettledStatic) ->
      promises.call_native_promise_all_settled(state, args, rest_stack)
    // Promise.any(iterable)
    value.Call(value.PromiseAnyStatic) ->
      promises.call_native_promise_any(state, args, rest_stack)
    // Promise.all per-element resolve handler
    value.Call(value.PromiseAllResolveElement(
      index:,
      remaining_ref:,
      values_ref:,
      already_called_ref:,
      resolve:,
      reject: _,
    )) ->
      promises.call_native_promise_all_resolve_element(
        state,
        args,
        rest_stack,
        index,
        remaining_ref,
        values_ref,
        already_called_ref,
        resolve,
      )
    // Promise.allSettled per-element resolve handler
    value.Call(value.PromiseAllSettledResolveElement(
      index:,
      remaining_ref:,
      values_ref:,
      already_called_ref:,
      resolve:,
    )) ->
      promises.call_native_promise_all_settled_resolve_element(
        state,
        args,
        rest_stack,
        index,
        remaining_ref,
        values_ref,
        already_called_ref,
        resolve,
      )
    // Promise.allSettled per-element reject handler
    value.Call(value.PromiseAllSettledRejectElement(
      index:,
      remaining_ref:,
      values_ref:,
      already_called_ref:,
      resolve:,
    )) ->
      promises.call_native_promise_all_settled_reject_element(
        state,
        args,
        rest_stack,
        index,
        remaining_ref,
        values_ref,
        already_called_ref,
        resolve,
      )
    // Promise.any per-element reject handler
    value.Call(value.PromiseAnyRejectElement(
      index:,
      remaining_ref:,
      errors_ref:,
      already_called_ref:,
      resolve: _,
      reject:,
    )) ->
      promises.call_native_promise_any_reject_element(
        state,
        args,
        rest_stack,
        index,
        remaining_ref,
        errors_ref,
        already_called_ref,
        reject,
      )
    // Promise.prototype.finally wrapper functions
    value.Call(value.PromiseFinallyFulfill(on_finally:)) ->
      promises.call_native_finally_fulfill(state, on_finally, args, rest_stack)
    value.Call(value.PromiseFinallyReject(on_finally:)) ->
      promises.call_native_finally_reject(state, on_finally, args, rest_stack)
    value.Call(value.PromiseFinallyValueThunk(value: captured_value)) -> {
      // Ignore argument, return the captured value
      Ok(
        State(..state, stack: [captured_value, ..rest_stack], pc: state.pc + 1),
      )
    }
    value.Call(value.PromiseFinallyThrower(reason:)) -> {
      // Ignore argument, throw the captured reason
      Error(#(Thrown, reason, state.heap))
    }
    // Async function resume (called when awaited promise settles)
    value.Call(value.AsyncResume(async_data_ref:, is_reject:)) ->
      call_native_async_resume(
        state,
        async_data_ref,
        is_reject,
        args,
        rest_stack,
      )
    // Generator prototype methods
    value.Call(value.GeneratorNext) ->
      generators.call_native_generator_next(
        state,
        this,
        args,
        rest_stack,
        execute_inner,
        unwind_to_catch,
      )
    value.Call(value.GeneratorReturn) ->
      generators.call_native_generator_return(
        state,
        this,
        args,
        rest_stack,
        execute_inner,
      )
    value.Call(value.GeneratorThrow) ->
      generators.call_native_generator_throw(
        state,
        this,
        args,
        rest_stack,
        execute_inner,
        unwind_to_catch,
      )
    // Symbol() constructor — callable but NOT new-able
    value.Call(value.SymbolConstructor) -> {
      let #(new_descs, sym_val) =
        builtins_symbol.call_symbol(args, state.symbol_descriptions)
      Ok(
        State(
          ..state,
          stack: [sym_val, ..rest_stack],
          pc: state.pc + 1,
          symbol_descriptions: new_descs,
        ),
      )
    }
    // Symbol.for(key) — global symbol registry
    value.Call(value.SymbolFor) -> {
      // Step 1: Let stringKey be ? ToString(key).
      let key_val = case args {
        [k, ..] -> k
        [] -> value.JsUndefined
      }
      use #(key_str, state) <- result.try(
        frame.rethrow(coerce.js_to_string(state, key_val)),
      )
      // Step 2-4: Look up in GlobalSymbolRegistry, return existing or create new.
      case dict.get(state.symbol_registry, key_str) {
        Ok(existing_id) ->
          Ok(
            State(
              ..state,
              stack: [value.JsSymbol(existing_id), ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        Error(Nil) -> {
          let id = value.UserSymbol(builtins_symbol.new_symbol_ref())
          let new_registry = dict.insert(state.symbol_registry, key_str, id)
          let new_descs = dict.insert(state.symbol_descriptions, id, key_str)
          Ok(
            State(
              ..state,
              stack: [value.JsSymbol(id), ..rest_stack],
              pc: state.pc + 1,
              symbol_registry: new_registry,
              symbol_descriptions: new_descs,
            ),
          )
        }
      }
    }
    // Symbol.keyFor(sym) — reverse lookup in global registry
    value.Call(value.SymbolKeyFor) -> {
      case args {
        [value.JsSymbol(id), ..] -> {
          // Search the registry for this symbol ID
          let result =
            dict.to_list(state.symbol_registry)
            |> list.find(fn(pair) { pair.1 == id })
          let val = case result {
            Ok(#(key, _)) -> value.JsString(key)
            Error(Nil) -> value.JsUndefined
          }
          Ok(State(..state, stack: [val, ..rest_stack], pc: state.pc + 1))
        }
        _ ->
          frame.rethrow(coerce.thrown_type_error(
            state,
            "Symbol.keyFor requires a Symbol argument",
          ))
      }
    }
    // String() constructor — uses full ToString (ToPrimitive for objects)
    value.Call(value.StringConstructor) ->
      case args {
        [] ->
          Ok(
            State(
              ..state,
              stack: [JsString(""), ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        [val, ..] ->
          case coerce.js_to_string(state, val) {
            Ok(#(s, new_state)) ->
              Ok(
                State(
                  ..new_state,
                  stack: [JsString(s), ..rest_stack],
                  pc: state.pc + 1,
                ),
              )
            Error(#(thrown, new_state)) ->
              Error(#(Thrown, thrown, new_state.heap))
          }
      }
    // All other native functions: synchronous dispatch via Dispatch slot
    value.Dispatch(native) -> {
      let #(new_state, result) = dispatch_native(native, args, this, state)
      case result {
        Ok(return_value) ->
          Ok(
            State(
              ..new_state,
              stack: [return_value, ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        Error(thrown) -> Error(#(Thrown, thrown, new_state.heap))
      }
    }
  }
}

/// Full constructor invocation — handles derived constructors, base constructors,
/// bound functions, and native constructors. Extracted from the CallConstructor
/// opcode handler so CallConstructorApply (spread path) can share it.
fn do_construct(
  state: State,
  ctor_ref: Ref,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case heap.read(state.heap, ctor_ref) {
    Some(ObjectSlot(
      kind: FunctionObject(func_template:, env: env_ref),
      properties:,
      ..,
    )) -> {
      // Check if this is a derived constructor
      let is_derived = func_template.is_derived_constructor
      case is_derived {
        True ->
          // Derived constructor: don't allocate object yet.
          // this = JsUninitialized (TDZ until super() is called).
          // constructor_this = None signals derived constructor mode.
          call_function(
            state,
            ctor_ref,
            env_ref,
            func_template,
            args,
            rest_stack,
            JsUninitialized,
            None,
            Some(ctor_ref),
          )
        False -> {
          // Base constructor: allocate the new object
          let proto = case dict.get(properties, "prototype") {
            Ok(DataProperty(value: JsObject(proto_ref), ..)) -> Some(proto_ref)
            _ -> Some(state.builtins.object.prototype)
          }
          let #(heap, new_obj_ref) =
            heap.alloc(
              state.heap,
              ObjectSlot(
                kind: OrdinaryObject,
                properties: dict.new(),
                elements: js_elements.new(),
                prototype: proto,
                symbol_properties: dict.new(),
                extensible: True,
              ),
            )
          let new_obj = JsObject(new_obj_ref)
          call_function(
            State(..state, heap:),
            ctor_ref,
            env_ref,
            func_template,
            args,
            rest_stack,
            new_obj,
            Some(new_obj),
            None,
          )
        }
      }
    }
    // Bound function used as constructor: resolve target, prepend args
    Some(ObjectSlot(
      kind: NativeFunction(value.Call(value.BoundFunction(
        target:,
        bound_args:,
        ..,
      ))),
      ..,
    )) -> {
      let final_args = list.append(bound_args, args)
      construct_value(state, target, final_args, rest_stack)
    }
    Some(ObjectSlot(kind: NativeFunction(native), ..)) ->
      call_native(state, native, args, rest_stack, JsUndefined)
    _ ->
      frame.throw_type_error(
        state,
        object.inspect(JsObject(ctor_ref), state.heap)
          <> " is not a constructor",
      )
  }
}

/// Construct a new object using the target function ref.
/// Used by CallConstructor when the constructor is a bound function.
fn construct_value(
  state: State,
  target_ref: Ref,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case heap.read(state.heap, target_ref) {
    Some(ObjectSlot(
      kind: FunctionObject(func_template:, env: env_ref),
      properties:,
      ..,
    )) -> {
      let proto = case dict.get(properties, "prototype") {
        Ok(DataProperty(value: JsObject(proto_ref), ..)) -> Some(proto_ref)
        _ -> Some(state.builtins.object.prototype)
      }
      let #(h, new_obj_ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: OrdinaryObject,
            properties: dict.new(),
            elements: js_elements.new(),
            prototype: proto,
            symbol_properties: dict.new(),
            extensible: True,
          ),
        )
      let new_obj = JsObject(new_obj_ref)
      call_function(
        State(..state, heap: h),
        target_ref,
        env_ref,
        func_template,
        args,
        rest_stack,
        new_obj,
        Some(new_obj),
        None,
      )
    }
    // Chained bound function: resolve further
    Some(ObjectSlot(
      kind: NativeFunction(value.Call(value.BoundFunction(
        target:,
        bound_args:,
        ..,
      ))),
      ..,
    )) -> {
      let final_args = list.append(bound_args, args)
      construct_value(state, target, final_args, rest_stack)
    }
    Some(ObjectSlot(kind: NativeFunction(native), ..)) ->
      call_native(state, native, args, rest_stack, JsUndefined)
    _ ->
      frame.throw_type_error(
        state,
        object.inspect(JsObject(target_ref), state.heap)
          <> " is not a constructor",
      )
  }
}

/// Call an arbitrary JsValue as a function with the given this and args.
/// Used by Function.prototype.call/apply and bound function invocation.
fn call_value(
  state: State,
  callee: JsValue,
  args: List(JsValue),
  this_val: JsValue,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case callee {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: FunctionObject(func_template:, env: env_ref), ..)) ->
          call_function(
            state,
            ref,
            env_ref,
            func_template,
            args,
            state.stack,
            this_val,
            None,
            None,
          )
        Some(ObjectSlot(kind: NativeFunction(native), ..)) ->
          call_native(state, native, args, state.stack, this_val)
        _ ->
          frame.throw_type_error(
            state,
            object.inspect(callee, state.heap) <> " is not a function",
          )
      }
    _ ->
      frame.throw_type_error(
        state,
        object.inspect(callee, state.heap) <> " is not a function",
      )
  }
}

/// Internal helper for ArrayFromWithHoles opcode — assigns values to
/// non-hole positions in a sparse array literal like `[1,,3]`.
///
/// Related to ES2024 §13.2.4.1 ArrayLiteral evaluation:
///   - ElementList : ElementList , Elision_opt AssignmentExpression
///     uses ArrayAccumulation which skips elision slots.
///
/// This function zips stack values with their non-hole indices.
/// `holes` is a sorted-ascending list of indices to skip. Walks index
/// 0,1,2,... — when index matches head of holes, skip it (consume from
/// holes); otherwise pair next value with that index. Accumulates in
/// reverse; caller doesn't care about order since result feeds a dict.
fn assign_non_hole_indices(
  values: List(JsValue),
  holes: List(Int),
  index: Int,
  acc: List(#(Int, JsValue)),
) -> List(#(Int, JsValue)) {
  case values {
    [] -> acc
    [v, ..vs] ->
      case holes {
        [h, ..hs] if h == index ->
          assign_non_hole_indices(values, hs, index + 1, acc)
        _ -> assign_non_hole_indices(vs, holes, index + 1, [#(index, v), ..acc])
      }
  }
}

/// Increment array length WITHOUT setting any element (creates a hole).
/// ArrayPushHole opcode helper.
///
/// Related to ES2024 §10.4.2.4 ArraySetLength — when length is increased
/// without setting an element, the spec allows holes (missing properties)
/// in the index range. Dense backing must be sparsified so the hole
/// survives later ArrayPush appends (otherwise the dense tuple would
/// fill the gap with undefined, violating hole semantics for methods
/// like forEach/map that skip holes per §23.1.3).
fn grow_array_length(h: Heap, ref: Ref) -> Heap {
  use slot <- heap.update(h, ref)
  case slot {
    ObjectSlot(kind: ArrayObject(length:), elements:, ..) -> {
      // Force sparse representation so the hole survives later appends.
      let elements = case elements {
        value.DenseElements(_) -> js_elements.delete(elements, length)
        value.SparseElements(_) -> elements
      }
      ObjectSlot(..slot, kind: ArrayObject(length + 1), elements:)
    }
    _ -> slot
  }
}

/// Append one value to the end of an array (ArrayPush opcode helper).
/// Reads current length, sets element at that index, increments length.
/// Non-array refs are a no-op — shouldn't happen for compiler-emitted literals.
fn push_onto_array(h: Heap, ref: Ref, val: JsValue) -> Heap {
  use slot <- heap.update(h, ref)
  case slot {
    ObjectSlot(kind: ArrayObject(length:), elements:, ..) ->
      ObjectSlot(
        ..slot,
        kind: ArrayObject(length + 1),
        elements: js_elements.set(elements, length, val),
      )
    _ -> slot
  }
}

/// Bulk-append a range [idx, end) from source elements onto the target array.
/// Used for the array fast-path in ArraySpread — avoids creating an
/// ArrayIteratorSlot when the source is a plain array.
fn append_range_to_array(
  h: Heap,
  target_ref: Ref,
  src_elements: value.JsElements,
  idx: Int,
  end: Int,
) -> Heap {
  case idx >= end {
    True -> h
    False -> {
      // js_elements.get returns JsUndefined for holes — matches the spec's
      // array iterator behavior (CreateIterResultObject(Get(array, idx), false)).
      let h = push_onto_array(h, target_ref, js_elements.get(src_elements, idx))
      append_range_to_array(h, target_ref, src_elements, idx + 1, end)
    }
  }
}

/// Drain an iterable into the target array (ArraySpread opcode helper).
/// Mirrors GetIterator's dispatch: ArrayObject fast-path, GeneratorObject
/// drain loop, everything else throws "is not iterable".
///
/// Per ES §13.2.4.1 ArrayAccumulation (SpreadElement):
///   1. spreadObj = ? Evaluate(AssignmentExpression)
///   2. iteratorRecord = ? GetIterator(spreadObj, sync)
///   3. Repeat: next = ? IteratorStepValue; if done return; CreateDataProperty(A, idx, next); idx++
///
/// Array fast-path is observationally equivalent for us — the spec's array
/// iterator reads Get(array, idx) which returns undefined for holes; so does
/// js_elements.get. V8 does the same shortcut.
fn spread_into_array(
  state: State,
  target_ref: Ref,
  iterable: JsValue,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case iterable {
    JsObject(src_ref) ->
      case heap.read(state.heap, src_ref) {
        Some(ObjectSlot(kind: ArrayObject(length:), elements:, ..))
        | Some(ObjectSlot(kind: value.ArgumentsObject(length:), elements:, ..)) -> {
          // Fast path: copy all elements at once, no iterator slot.
          let heap =
            append_range_to_array(state.heap, target_ref, elements, 0, length)
          Ok(State(..state, heap:))
        }
        Some(ObjectSlot(kind: GeneratorObject(_), ..)) ->
          // Generators are self-iterators. Drain via repeated .next().
          drain_generator_to_array(state, src_ref, target_ref)
        _ -> {
          frame.throw_type_error(
            state,
            object.inspect(iterable, state.heap) <> " is not iterable",
          )
        }
      }
    // null/undefined/primitives: not iterable.
    // (Strings are iterable per spec but GetIterator doesn't handle them yet;
    //  will be fixed when Symbol.iterator is wired for string wrappers.)
    _ -> {
      frame.throw_type_error(
        state,
        object.inspect(iterable, state.heap) <> " is not iterable",
      )
    }
  }
}

/// Repeatedly call generator.next(), pushing each yielded value onto the
/// target array until done=true. Each .next() re-enters the VM via
/// call_native_generator_next, so state must be threaded through.
/// The generator's {value, done} result object is read from the returned
/// state's stack — call_native_generator_next pushes it there.
fn drain_generator_to_array(
  state: State,
  gen_ref: Ref,
  target_ref: Ref,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  // call_native_generator_next pushes the result object onto rest_stack.
  // We pass an empty rest_stack so the result is the only thing on the stack.
  use next_state <- result.try(generators.call_native_generator_next(
    state,
    JsObject(gen_ref),
    [],
    [],
    execute_inner,
    unwind_to_catch,
  ))
  case next_state.stack {
    [JsObject(result_ref), ..] ->
      case heap.read(next_state.heap, result_ref) {
        Some(ObjectSlot(properties: props, ..)) -> {
          let done = case dict.get(props, "done") {
            Ok(DataProperty(value: JsBool(d), ..)) -> d
            _ -> False
          }
          case done {
            True ->
              // Generator exhausted. Restore heap but not stack — the caller
              // (ArraySpread handler) sets the stack explicitly.
              Ok(
                State(
                  ..state,
                  heap: next_state.heap,
                  lexical_globals: next_state.lexical_globals,
                  const_lexical_globals: next_state.const_lexical_globals,
                  job_queue: next_state.job_queue,
                  pending_receivers: next_state.pending_receivers,
                  outstanding: next_state.outstanding,
                ),
              )
            False -> {
              let val = case dict.get(props, "value") {
                Ok(DataProperty(value: v, ..)) -> v
                _ -> JsUndefined
              }
              let heap = push_onto_array(next_state.heap, target_ref, val)
              // Recurse with the post-next state but cleaned stack.
              drain_generator_to_array(
                State(
                  ..state,
                  heap:,
                  lexical_globals: next_state.lexical_globals,
                  const_lexical_globals: next_state.const_lexical_globals,
                  job_queue: next_state.job_queue,
                  pending_receivers: next_state.pending_receivers,
                  outstanding: next_state.outstanding,
                ),
                gen_ref,
                target_ref,
              )
            }
          }
        }
        _ ->
          Error(#(
            StepVmError(Unimplemented(
              "ArraySpread: generator .next() returned non-object",
            )),
            JsUndefined,
            next_state.heap,
          ))
      }
    _ ->
      Error(#(
        StepVmError(Unimplemented("ArraySpread: generator .next() empty stack")),
        JsUndefined,
        next_state.heap,
      ))
  }
}

/// Extract elements from an array object as a list of JsValues.
/// Used by Function.prototype.apply to unpack the args array.
fn extract_array_args(h: Heap, ref: Ref) -> List(JsValue) {
  case heap.read(h, ref) {
    Some(ObjectSlot(kind: ArrayObject(length:), elements:, ..))
    | Some(ObjectSlot(kind: value.ArgumentsObject(length:), elements:, ..)) ->
      extract_elements_loop(elements, 0, length, [])
    _ -> []
  }
}

fn extract_elements_loop(
  elements: value.JsElements,
  idx: Int,
  length: Int,
  acc: List(JsValue),
) -> List(JsValue) {
  case idx >= length {
    True -> list.reverse(acc)
    False -> {
      let val = js_elements.get(elements, idx)
      extract_elements_loop(elements, idx + 1, length, [val, ..acc])
    }
  }
}

/// Function.prototype.toString — ES2024 §20.2.3.5
///
/// For native functions: "function NAME() { [native code] }"
/// For user-defined functions: "function NAME() { [native code] }" (simplified)
fn function_to_string(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: FunctionObject(func_template:, ..), ..)) -> {
          let name = case func_template.name {
            option.Some(n) -> n
            option.None -> "anonymous"
          }
          #(state, Ok(JsString("function " <> name <> "() { [native code] }")))
        }
        Some(ObjectSlot(kind: NativeFunction(_), properties:, ..)) -> {
          let name =
            dict.get(properties, "name")
            |> result.map(fn(p) {
              case p {
                DataProperty(value: JsString(n), ..) -> n
                _ -> ""
              }
            })
            |> result.unwrap("")
          #(state, Ok(JsString("function " <> name <> "() { [native code] }")))
        }
        _ ->
          frame.type_error(
            state,
            "Function.prototype.toString requires that 'this' be a Function",
          )
      }
    _ ->
      frame.type_error(
        state,
        "Function.prototype.toString requires that 'this' be a Function",
      )
  }
}

/// Route a NativeFn call to the correct builtin module.
fn dispatch_native(
  native: value.NativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case native {
    // Per-module dispatch
    value.ObjectNative(n) -> builtins_object.dispatch(n, args, this, state)
    value.ArrayNative(n) -> builtins_array.dispatch(n, args, this, state)
    value.StringNative(n) -> builtins_string.dispatch(n, args, this, state)
    value.NumberNative(n) -> builtins_number.dispatch(n, args, this, state)
    value.BooleanNative(n) -> builtins_boolean.dispatch(n, args, this, state)
    value.MathNative(n) -> builtins_math.dispatch(n, args, this, state)
    value.ErrorNative(n) -> builtins_error.dispatch(n, args, this, state)
    value.ArcNative(n) -> builtins_arc.dispatch(n, args, this, state)
    value.VmNative(value.ArcSpawn) -> arc_spawn(args, state)
    value.VmNative(value.EvalScript) -> eval_script_native(args, this, state)
    value.VmNative(value.CreateRealm) -> create_realm_native(this, state)
    value.VmNative(value.Gc) -> #(state, Ok(JsUndefined))
    value.JsonNative(n) -> builtins_json.dispatch(n, args, this, state)
    value.MapNative(n) -> builtins_map.dispatch(n, args, this, state)
    value.SetNative(n) -> builtins_set.dispatch(n, args, this, state)
    value.WeakMapNative(n) -> builtins_weak_map.dispatch(n, args, this, state)
    value.WeakSetNative(n) -> builtins_weak_set.dispatch(n, args, this, state)
    value.RegExpNative(n) -> builtins_regexp.dispatch(n, args, this, state)
    // Standalone VM-level natives
    value.VmNative(value.FunctionConstructor) ->
      frame.type_error(state, "Function constructor is not supported")
    value.VmNative(value.IteratorSymbolIterator) -> #(state, Ok(this))
    value.VmNative(value.FunctionToString) -> function_to_string(this, state)
    // Global functions: eval, URI encoding/decoding
    value.VmNative(value.Eval) ->
      frame.type_error(state, "eval is not supported")
    value.VmNative(value.DecodeURI) -> {
      let arg = case args {
        [s, ..] -> s
        [] -> value.JsUndefined
      }
      use str, state <- frame.try_to_string(state, arg)
      #(state, Ok(value.JsString(operators.uri_decode(str))))
    }
    value.VmNative(value.EncodeURI) -> {
      let arg = case args {
        [s, ..] -> s
        [] -> value.JsUndefined
      }
      use str, state <- frame.try_to_string(state, arg)
      #(state, Ok(value.JsString(operators.uri_encode(str, True))))
    }
    value.VmNative(value.DecodeURIComponent) -> {
      let arg = case args {
        [s, ..] -> s
        [] -> value.JsUndefined
      }
      use str, state <- frame.try_to_string(state, arg)
      #(state, Ok(value.JsString(operators.uri_decode(str))))
    }
    value.VmNative(value.EncodeURIComponent) -> {
      let arg = case args {
        [s, ..] -> s
        [] -> value.JsUndefined
      }
      use str, state <- frame.try_to_string(state, arg)
      #(state, Ok(value.JsString(operators.uri_encode(str, False))))
    }
    // AnnexB B.2.1.1 escape ( string )
    value.VmNative(value.Escape) -> {
      let arg = case args {
        [s, ..] -> s
        [] -> value.JsUndefined
      }
      use str, state <- frame.try_to_string(state, arg)
      #(state, Ok(value.JsString(operators.js_escape(str))))
    }
    // AnnexB B.2.1.2 unescape ( string )
    value.VmNative(value.Unescape) -> {
      let arg = case args {
        [s, ..] -> s
        [] -> value.JsUndefined
      }
      use str, state <- frame.try_to_string(state, arg)
      #(state, Ok(value.JsString(operators.js_unescape(str))))
    }
  }
}

// ============================================================================
// Arc.spawn — spawn a new BEAM process running a JS closure
// ============================================================================

@external(erlang, "erlang", "spawn")
fn spawn(fun: fn() -> Nil) -> value.ErlangPid

/// Non-standard: Arc.spawn(fn)
/// Spawns a new BEAM process that executes the given JS function.
/// The spawned process gets a snapshot of the current heap, builtins,
/// and closure templates. Returns a Pid object.
fn arc_spawn(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let fn_arg = case args {
    [a, ..] -> a
    [] -> JsUndefined
  }

  let result = {
    use fn_ref <- result.try(case fn_arg {
      JsObject(fn_ref) -> Ok(fn_ref)
      _ -> Error("Arc.spawn: argument is not a function object")
    })

    use spawner <- result.try(case heap.read(state.heap, fn_ref) {
      Some(ObjectSlot(
        kind: FunctionObject(func_template: callee_template, env: env_ref),
        ..,
      )) ->
        Ok(fn() {
          run_spawned_closure(
            callee_template,
            env_ref,
            state.heap,
            state.builtins,
            state.global_object,
            state.lexical_globals,
            state.const_lexical_globals,
            state.symbol_descriptions,
            state.symbol_registry,
            state.event_loop,
          )
        })
      Some(ObjectSlot(kind: NativeFunction(native), ..)) ->
        Ok(fn() {
          run_spawned_native(
            native,
            state.func,
            state.heap,
            state.builtins,
            state.global_object,
            state.lexical_globals,
            state.const_lexical_globals,
            state.symbol_descriptions,
            state.symbol_registry,
            state.event_loop,
          )
        })
      _ -> Error("Arc.spawn: argument is not a function")
    })

    let #(heap, pid_val) =
      builtins_arc.alloc_pid_object(
        state.heap,
        state.builtins.object.prototype,
        state.builtins.function.prototype,
        spawn(spawner),
      )

    Ok(#(State(..state, heap:), Ok(pid_val)))
  }

  case result {
    Ok(ret) -> ret
    Error(msg) -> frame.type_error(state, msg)
  }
}

/// Run a JS closure in a standalone BEAM process. Sets up a fresh VM
/// state from the snapshot and executes the function to completion.
fn run_spawned_closure(
  callee_template: FuncTemplate,
  env_ref: value.Ref,
  heap: Heap,
  builtins: Builtins,
  global_object: Ref,
  lexical_globals: dict.Dict(String, JsValue),
  const_lexical_globals: set.Set(String),
  symbol_descriptions: dict.Dict(value.SymbolId, String),
  symbol_registry: dict.Dict(String, value.SymbolId),
  event_loop: Bool,
) -> Nil {
  let env_values = case heap.read(heap, env_ref) {
    Some(value.EnvSlot(slots)) -> slots
    _ -> []
  }
  let env_count = list.length(env_values)
  let remaining =
    callee_template.local_count - env_count - callee_template.arity
  let padded_args = list.repeat(JsUndefined, callee_template.arity)
  let locals =
    list.flatten([env_values, padded_args, list.repeat(JsUndefined, remaining)])
    |> array.from_list

  let state =
    new_state(
      callee_template,
      locals,
      heap,
      builtins,
      global_object,
      lexical_globals,
      const_lexical_globals,
      symbol_descriptions,
      symbol_registry,
      event_loop,
    )

  case execute_inner(state) {
    Ok(#(_, final_state)) -> {
      let _ = job_queue.finish(final_state)
      Nil
    }
    Error(_) -> Nil
  }
}

/// Run a native function in a standalone BEAM process.
/// Sets up a full VM state (using the caller's func template as context)
/// and drains the job queue after execution.
fn run_spawned_native(
  native: value.NativeFnSlot,
  caller_func: FuncTemplate,
  heap: Heap,
  builtins: Builtins,
  global_object: Ref,
  lexical_globals: dict.Dict(String, JsValue),
  const_lexical_globals: set.Set(String),
  symbol_descriptions: dict.Dict(value.SymbolId, String),
  symbol_registry: dict.Dict(String, value.SymbolId),
  event_loop: Bool,
) -> Nil {
  let locals = array.repeat(JsUndefined, caller_func.local_count)
  let state =
    new_state(
      caller_func,
      locals,
      heap,
      builtins,
      global_object,
      lexical_globals,
      const_lexical_globals,
      symbol_descriptions,
      symbol_registry,
      event_loop,
    )

  case call_native(state, native, [], state.stack, JsUndefined) {
    Ok(final_state) -> {
      let _ = job_queue.finish(final_state)
      Nil
    }
    Error(_) -> Nil
  }
}

// ============================================================================
// $262 — test262 host-defined realm functions
// ============================================================================

/// $262.evalScript(source) — parse and execute a script in the realm
/// associated with the $262 object's __realm__ property.
fn eval_script_native(
  args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let source = case args {
    [s, ..] -> s
    [] -> JsUndefined
  }
  use source_str, state <- frame.try_to_string(state, source)

  // Read the __realm__ property from the $262 object to find the realm
  let realm_result = case this {
    JsObject(this_ref) ->
      case object.get_own_property(state.heap, this_ref, "__realm__") {
        Some(DataProperty(value: JsObject(realm_ref), ..)) ->
          case heap.read(state.heap, realm_ref) {
            Some(value.RealmSlot(
              global_object: realm_global,
              lexical_globals:,
              const_lexical_globals:,
              symbol_descriptions:,
              symbol_registry:,
            )) ->
              case dict.get(state.realms, realm_ref) {
                Ok(realm_builtins) ->
                  Ok(#(
                    realm_builtins,
                    realm_global,
                    realm_ref,
                    lexical_globals,
                    const_lexical_globals,
                    symbol_descriptions,
                    symbol_registry,
                  ))
                Error(Nil) -> Error("evalScript: realm builtins not found")
              }
            _ -> Error("evalScript: __realm__ is not a RealmSlot")
          }
        _ -> Error("evalScript: $262 has no __realm__ property")
      }
    _ -> Error("evalScript: this is not an object")
  }

  case realm_result {
    Error(msg) -> frame.type_error(state, msg)
    Ok(#(
      realm_builtins,
      realm_global,
      realm_ref,
      lexical_globals,
      const_lexical_globals,
      symbol_descriptions,
      symbol_registry,
    )) ->
      case parser.parse(source_str, parser.Script) {
        Error(err) -> {
          let #(heap, syntax_err) =
            common.make_syntax_error(
              state.heap,
              realm_builtins,
              parser.parse_error_to_string(err),
            )
          #(State(..state, heap:), Error(syntax_err))
        }
        Ok(program) ->
          case compiler.compile_repl(program) {
            Error(err) -> {
              let #(heap, syntax_err) =
                common.make_syntax_error(
                  state.heap,
                  realm_builtins,
                  string.inspect(err),
                )
              #(State(..state, heap:), Error(syntax_err))
            }
            Ok(template) -> {
              let locals = array.repeat(JsUndefined, template.local_count)
              let eval_state =
                State(
                  ..new_state(
                    template,
                    locals,
                    state.heap,
                    realm_builtins,
                    realm_global,
                    lexical_globals,
                    const_lexical_globals,
                    symbol_descriptions,
                    symbol_registry,
                    False,
                  ),
                  job_queue: state.job_queue,
                  realms: state.realms,
                )
              case execute_inner(eval_state) {
                Error(vm_err) ->
                  frame.type_error(
                    state,
                    "evalScript: VM error: " <> string.inspect(vm_err),
                  )
                Ok(#(completion, final_eval_state)) -> {
                  // Drain microtasks in the eval realm
                  let drained = job_queue.drain_jobs(final_eval_state)
                  // Update the realm slot with potentially modified lexical globals
                  let updated_realm =
                    value.RealmSlot(
                      global_object: realm_global,
                      lexical_globals: drained.lexical_globals,
                      const_lexical_globals: drained.const_lexical_globals,
                      symbol_descriptions: drained.symbol_descriptions,
                      symbol_registry: drained.symbol_registry,
                    )
                  let heap = heap.write(drained.heap, realm_ref, updated_realm)
                  // Propagate heap and job queue back to caller
                  let state =
                    State(
                      ..state,
                      heap:,
                      job_queue: drained.job_queue,
                      pending_receivers: drained.pending_receivers,
                      outstanding: drained.outstanding,
                      realms: drained.realms,
                    )
                  case completion {
                    NormalCompletion(val, _) -> #(state, Ok(val))
                    ThrowCompletion(thrown, _) -> #(state, Error(thrown))
                    YieldCompletion(_, _) ->
                      frame.type_error(state, "evalScript: unexpected yield")
                  }
                }
              }
            }
          }
      }
  }
}

/// $262.createRealm() — create a fresh realm and return its $262 object.
fn create_realm_native(
  _this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Initialize fresh builtins and global object for the new realm
  let #(heap, new_builtins) = builtins.init(state.heap)
  let #(heap, new_global_ref) = builtins.globals(new_builtins, heap)

  // Allocate a RealmSlot for the new realm
  let #(heap, realm_ref) =
    heap.alloc(
      heap,
      value.RealmSlot(
        global_object: new_global_ref,
        lexical_globals: dict.new(),
        const_lexical_globals: set.new(),
        symbol_descriptions: dict.new(),
        symbol_registry: dict.new(),
      ),
    )
  let heap = heap.root(heap, realm_ref)

  // Build the $262 object for the new realm
  let #(heap, dollar_262_ref) =
    build_262(heap, new_builtins, new_global_ref, realm_ref)

  // Install $262 on the new realm's global object
  let #(heap, _) =
    object.set_property(heap, new_global_ref, "$262", JsObject(dollar_262_ref))

  // Register the realm's builtins
  let realms = dict.insert(state.realms, realm_ref, new_builtins)

  #(State(..state, heap:, realms:), Ok(JsObject(dollar_262_ref)))
}

/// Build a $262 object with evalScript, createRealm, gc methods and a global
/// property. The realm_ref points to a RealmSlot on the heap.
/// Public so test262_exec.gleam can use it for initial test setup.
pub fn build_262(
  h: Heap,
  b: Builtins,
  global_ref: Ref,
  realm_ref: Ref,
) -> #(Heap, Ref) {
  let func_proto = b.function.prototype

  // Allocate method function objects
  let #(h, eval_script_fn) =
    common.alloc_native_fn(
      h,
      func_proto,
      value.VmNative(value.EvalScript),
      "evalScript",
      1,
    )
  let #(h, create_realm_fn) =
    common.alloc_native_fn(
      h,
      func_proto,
      value.VmNative(value.CreateRealm),
      "createRealm",
      0,
    )
  let #(h, gc_fn) =
    common.alloc_native_fn(h, func_proto, value.VmNative(value.Gc), "gc", 0)

  // Build the $262 object
  let #(h, ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: OrdinaryObject,
        properties: dict.from_list([
          #("global", value.builtin_property(JsObject(global_ref))),
          #("evalScript", value.builtin_property(JsObject(eval_script_fn))),
          #("createRealm", value.builtin_property(JsObject(create_realm_fn))),
          #("gc", value.builtin_property(JsObject(gc_fn))),
          // __realm__ is non-enumerable internal property
          #(
            "__realm__",
            value.data(JsObject(realm_ref)) |> value.configurable(),
          ),
        ]),
        symbol_properties: dict.new(),
        elements: js_elements.new(),
        prototype: Some(b.object.prototype),
        extensible: True,
      ),
    )
  let h = heap.root(h, ref)
  #(h, ref)
}

/// Run a JS handler function with a this value and args.
/// Returns Ok(return_value, state) on success, Error(thrown, state) on throw.
fn run_handler_with_this(
  state: State,
  handler: JsValue,
  this_val: JsValue,
  args: List(JsValue),
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
          )
        Some(ObjectSlot(kind: NativeFunction(native), ..)) -> {
          // For native functions (like resolve/reject), call directly
          let job_state =
            State(
              ..state,
              stack: [],
              pc: 0,
              code: array.from_list([]),
              call_stack: [],
              try_stack: [],
            )
          case call_native(job_state, native, args, [], this_val) {
            Ok(new_state) ->
              case new_state.stack {
                [result, ..] ->
                  Ok(#(
                    result,
                    State(
                      ..state,
                      heap: new_state.heap,
                      job_queue: new_state.job_queue,
                      pending_receivers: new_state.pending_receivers,
                      outstanding: new_state.outstanding,
                    ),
                  ))
                [] ->
                  Ok(#(
                    JsUndefined,
                    State(
                      ..state,
                      heap: new_state.heap,
                      job_queue: new_state.job_queue,
                      pending_receivers: new_state.pending_receivers,
                      outstanding: new_state.outstanding,
                    ),
                  ))
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
  fn_ref: value.Ref,
  env_ref: value.Ref,
  callee_template: FuncTemplate,
  args: List(JsValue),
  this_val: JsValue,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  let env_values = case heap.read(state.heap, env_ref) {
    Some(value.EnvSlot(slots)) -> slots
    _ -> []
  }
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
    |> array.from_list
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
      Ok(#(
        val,
        State(
          ..state,
          heap: h,
          job_queue: final_state.job_queue,
          lexical_globals: final_state.lexical_globals,
          const_lexical_globals: final_state.const_lexical_globals,
          pending_receivers: final_state.pending_receivers,
          outstanding: final_state.outstanding,
        ),
      ))
    Ok(#(ThrowCompletion(thrown, h), final_state)) ->
      Error(#(
        thrown,
        State(
          ..state,
          heap: h,
          job_queue: final_state.job_queue,
          lexical_globals: final_state.lexical_globals,
          const_lexical_globals: final_state.const_lexical_globals,
          pending_receivers: final_state.pending_receivers,
          outstanding: final_state.outstanding,
        ),
      ))
    Ok(#(YieldCompletion(_, _), _)) ->
      panic as "YieldCompletion should not appear in job execution"
    Error(vm_err) ->
      panic as { "VM error in promise job: " <> string.inspect(vm_err) }
  }
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

/// Pad args to exactly `arity` length — truncate extras, fill missing with undefined.
fn pad_args(args: List(JsValue), arity: Int) -> List(JsValue) {
  let len = list.length(args)
  case len >= arity {
    True -> list.take(args, arity)
    False -> list.append(args, list.repeat(JsUndefined, arity - len))
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
