import arc/vm/builtins/common.{type Builtins}
import arc/vm/builtins/helpers
import arc/vm/builtins/iterator as builtins_iterator
import arc/vm/builtins/regexp as builtins_regexp
import arc/vm/completion.{
  type Completion, AwaitCompletion, NormalCompletion, ThrowCompletion,
  YieldCompletion,
}
import arc/vm/exec/call
import arc/vm/exec/event_loop
import arc/vm/exec/generators
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/internal/job_queue
import arc/vm/internal/tuple_array
import arc/vm/opcode.{
  type Op, Add, ArrayFrom, ArrayFromWithHoles, ArrayPush, ArrayPushHole,
  ArraySpread, AsyncYieldStarNext, AsyncYieldStarResume, Await, BinOp, BoxLocal,
  Call, CallApply, CallConstructor, CallConstructorApply, CallEval, CallMethod,
  CallMethodApply, CallSuper, CallSuperApply, CheckSuperThis, CreateArguments,
  CreateRestArray, DeclareEvalVar,
  DeclareGlobalLex, DeclareGlobalVar, DefineAccessor, DefineAccessorComputed,
  DefineField, DefineFieldComputed, DefineMethod, DefineMethodComputed,
  DefineMethodField, DefineMethodFieldComputed,
  DeleteElem, DeleteField, Dup, ForInNext, ForInStart, GetAsyncIterator,
  GetBoxed, GetElem, GetElem2, GetEvalVar, GetField, GetField2, GetGlobal,
  GetIterator, GetLocal, GetPrivateField, GetPrivateField2, GetSuperProp,
  GetSuperProp2, GetSuperPropComputed, GetSuperPropComputed2, InitGlobalLex,
  InitialYield, IteratorCheckObject, IteratorClose, IteratorCloseThrow,
  IteratorNext, IteratorRest, Jump, JumpIfFalse, JumpIfNullish, JumpIfTrue,
  MakeClosure, NewObject, NewRegExp, ObjectRestCopy, ObjectSpread, Pop,
  PrivateIn, PushConst, PushTry, PutBoxed, PutElem, PutEvalVar, PutField,
  PutGlobal, PutLocal, PutPrivateField, PutSuperProp, PutSuperPropComputed,
  Return, SetLine, SetupDerivedClass, Swap, TypeOf,
  TypeofEvalVar, TypeofGlobal, UnaryOp, Yield, YieldStar,
}
import arc/vm/ops/array as array_ops
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/ops/operators
import arc/vm/ops/property
import arc/vm/realm
import arc/vm/state.{
  type Heap, type NativeFnSlot, type State, type StepResult, type VmError,
  Awaited, Done, SavedFrame, StackUnderflow, State, StepVmError, Thrown,
  TryFrame, Unimplemented, Yielded,
}
import arc/vm/value.{
  type FuncTemplate, type JsValue, type Ref, ArrayIteratorObject, ArrayObject,
  DataProperty, EvalEnvSlot, ForInIteratorSlot, FunctionObject, GeneratorObject,
  JsBool, JsNull, JsObject, JsString, JsUndefined, JsUninitialized, Named,
  NativeFunction, ObjectSlot, OrdinaryObject,
}
import gleam/dict
import gleam/float
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set
import gleam/string

// ============================================================================
// Internal state (types defined in state.gleam for cross-module access)
// ============================================================================

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

/// The construct_fn callback that gets stored in State.
/// Wraps do_construct for re-entrant `new target(...args)` from native code
/// (e.g. Reflect.construct).
///
/// Sets up an isolated frame with a sentinel empty-bytecode func so that when
/// the constructor body returns, execute_inner hits end-of-code and yields
/// NormalCompletion with the constructed object on top of stack. The sentinel
/// func is required because Return restores `code` from SavedFrame.func.bytecode,
/// not from the state's code field directly.
fn construct_fn_callback(
  state: State,
  target: JsValue,
  args: List(JsValue),
) -> Result(#(JsValue, State), #(JsValue, State)) {
  case target {
    JsObject(ref) -> {
      // Sentinel bytecode: do_construct saves pc+1 into the SavedFrame (or
      // advances pc+1 for native constructors), so we need Return at index 1.
      // Index 0 is never dispatched but present for belt-and-braces.
      let sentinel_code = tuple_array.from_list([Return, Return])
      let sentinel_func =
        value.FuncTemplate(
          name: None,
          arity: 0,
          local_count: 0,
          bytecode: sentinel_code,
          constants: tuple_array.from_list([]),
          functions: tuple_array.from_list([]),
          env_descriptors: [],
          is_strict: True,
          is_arrow: False,
          is_derived_constructor: False,
          is_generator: False,
          is_async: False,
          is_constructor: False,
          local_names: None,
          this_slot: None,
        )
      let isolated =
        State(
          ..state,
          stack: [],
          pc: 0,
          func: sentinel_func,
          code: sentinel_code,
          call_stack: [],
          try_stack: [],
        )
      // do_construct either:
      //  - pushes a SavedFrame and switches to the constructor's bytecode
      //    (regular function path), or
      //  - runs synchronously and leaves the result on stack at pc+1
      //    (native constructor path).
      // Either way, execute_inner drives to completion.
      case do_construct(isolated, ref, args, []) {
        Ok(entered) ->
          case execute_inner(entered) {
            Ok(#(NormalCompletion(val, h), final_state)) ->
              Ok(#(
                val,
                State(..state.merge_globals(state, final_state, []), heap: h),
              ))
            Ok(#(ThrowCompletion(thrown, h), final_state)) ->
              Error(#(
                thrown,
                State(..state.merge_globals(state, final_state, []), heap: h),
              ))
            Ok(#(YieldCompletion(_, _), _)) | Ok(#(AwaitCompletion(_, _), _)) ->
              panic as "Yield/Await completion during construct"
            Error(vm_err) ->
              panic as {
                "VM error during construct: " <> string.inspect(vm_err)
              }
          }
        Error(#(Thrown, thrown, post)) ->
          Error(#(thrown, State(..state, heap: post.heap)))
        Error(#(StepVmError(vm_err), _, _)) ->
          panic as { "VM error in do_construct: " <> string.inspect(vm_err) }
        Error(#(other, _, _post)) ->
          panic as {
            "Unexpected step result from do_construct: "
            <> string.inspect(other)
          }
      }
    }
    _ ->
      coerce.thrown_type_error(
        state,
        object.inspect(target, state.heap) <> " is not a constructor",
      )
  }
}

/// Create a fresh VM state from a function template.
/// Most callers can use this directly; override fields with `State(..new_state(...), ...)`
/// for cases that need non-default symbol_descriptions.
pub fn new_state(
  func: FuncTemplate,
  locals: tuple_array.TupleArray(JsValue),
  heap: Heap,
  builtins: Builtins,
  global_object: Ref,
  lexical_globals: dict.Dict(String, JsValue),
  const_lexical_globals: set.Set(String),
  symbol_descriptions: dict.Dict(value.SymbolId, String),
  symbol_registry: dict.Dict(String, value.SymbolId),
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
    builtins:,
    callee_ref: None,
    call_args: [],
    job_queue: job_queue.new(),
    unhandled_rejections: [],
    outstanding: 0,
    symbol_descriptions:,
    symbol_registry:,
    realms: dict.new(),
    call_fn: call_fn_callback,
    construct_fn: construct_fn_callback,
    call_depth: 0,
    eval_env: None,
    current_line: 0,
  )
}

pub fn init_state(
  func: FuncTemplate,
  heap: Heap,
  builtins: Builtins,
  global_object: Ref,
  is_module: Bool,
) -> State {
  // ES §16.2.1.5.2 ModuleEvaluation: module `this` is undefined.
  // ES §16.1.6 ScriptEvaluation: the script's this is the global object,
  // regardless of strict mode. (Strict only affects function-body this.)
  let this_val = case is_module {
    True -> JsUndefined
    False -> JsObject(global_object)
  }
  let locals = init_top_level_locals(func, this_val)
  new_state(
    func,
    locals,
    heap,
    builtins,
    global_object,
    dict.new(),
    set.new(),
    dict.new(),
    dict.new(),
  )
}

/// Build the locals array for a top-level (script/module/eval/REPL) template:
/// JsUndefined everywhere, then seed the lexical-`this` slot if present.
/// Function bodies use call.setup_locals instead — this is only for entries
/// that don't go through the call protocol.
pub fn init_top_level_locals(
  func: FuncTemplate,
  this_val: JsValue,
) -> tuple_array.TupleArray(JsValue) {
  let locals = tuple_array.repeat(JsUndefined, func.local_count)
  case func.this_slot {
    Some(idx) -> tuple_array.set_unchecked(idx, this_val, locals)
    None -> locals
  }
}

/// Build the locals array for a module body. Module `this` is undefined
/// (§16.2.1.5.2). `seeds` places pre-allocated BoxSlot refs into specific local
/// slots: import bindings into capture slots 0..N-1 (each the exporting
/// module's live cell), plus this module's own export cells into their declared
/// slots. The body reads/writes both through GetBoxed/PutBoxed.
pub fn init_module_locals(
  func: FuncTemplate,
  seeds: List(#(Int, JsValue)),
) -> tuple_array.TupleArray(JsValue) {
  let locals = tuple_array.repeat(JsUndefined, func.local_count)
  list.fold(seeds, locals, fn(acc, seed) {
    let #(index, box) = seed
    tuple_array.set_unchecked(index, box, acc)
  })
}

/// Call a function `callee` with `this` and `args` from a fresh top-level
/// frame, running the call (and anything synchronous it triggers) to
/// completion. This is the embedder analogue of the `Call` opcode and the
/// counterpart to `run`/`run_with` for a value you already hold — e.g. a module
/// export read off a namespace — rather than a script template.
///
/// A thrown value comes back as a `ThrowCompletion`; a malformed-bytecode
/// `VmError` propagates as `Error` (unlike `state.call`, which is the
/// re-entrant call used from within host functions and panics on `VmError`).
/// Calling an async function returns its pending Promise as the
/// `NormalCompletion` value — drive the returned State with a `finish` driver
/// (microtask/macrotask draining) to settle it.
pub fn call_to_completion(
  callee: JsValue,
  this_val: JsValue,
  args: List(JsValue),
  heap: Heap,
  builtins: Builtins,
  global_object: Ref,
) -> Result(#(Completion, State), VmError) {
  let seed = bare_call_state(heap, builtins, global_object)
  case call_value(seed, callee, args, this_val) {
    // call_value either set up the callee's frame (regular function) or ran it
    // synchronously (native), leaving control to resume at the sentinel Return;
    // execute_inner drives that to NormalCompletion with the result on top.
    Ok(ready) -> execute_inner(ready)
    Error(#(StepVmError(vm_err), _, _)) -> Error(vm_err)
    // Thrown at call setup (e.g. callee not callable, or a native threw); the
    // other StepResults can't escape call_value's setup, handled defensively.
    Error(#(_step, thrown, errored)) ->
      Ok(#(ThrowCompletion(thrown, errored.heap), errored))
  }
}

/// A do-nothing top-level frame for `call_to_completion`. Mirrors
/// `construct_fn_callback`'s sentinel: when the called function returns, the
/// `Return` op restores `code` from the saved frame's template (not the live
/// `code` field), and `call_function` saved the caller pc as pc+1 = 1, so a
/// `Return` must sit at index 1. Index 0 is never dispatched — execution starts
/// in the callee's frame, not here.
fn bare_call_state(heap: Heap, builtins: Builtins, global_object: Ref) -> State {
  let sentinel_code = tuple_array.from_list([Return, Return])
  let sentinel_func =
    value.FuncTemplate(
      name: None,
      arity: 0,
      local_count: 0,
      bytecode: sentinel_code,
      constants: tuple_array.from_list([]),
      functions: tuple_array.from_list([]),
      env_descriptors: [],
      is_strict: True,
      is_arrow: False,
      is_derived_constructor: False,
      is_generator: False,
      is_async: False,
      is_constructor: False,
      local_names: None,
      this_slot: None,
    )
  new_state(
    sentinel_func,
    tuple_array.from_list([]),
    heap,
    builtins,
    global_object,
    dict.new(),
    set.new(),
    dict.new(),
    dict.new(),
  )
}

/// Allocate a closure (FunctionObject) for `child_template`, capturing
/// `captured_values` (gathered per the template's env_descriptors). Builds the
/// env, the `name`/`length` props, the `.prototype` object (+ `.constructor`)
/// for non-arrows, and links the function prototype. Shared by the MakeClosure
/// op and module link-time hoisting (so cyclic function exports are callable
/// before bodies run).
pub fn make_closure(
  heap: Heap,
  builtins: Builtins,
  child_template: FuncTemplate,
  captured_values: List(JsValue),
) -> #(Heap, Ref) {
  let #(heap, env_ref) = heap.alloc(heap, value.EnvSlot(captured_values))
  // For non-arrow functions, pre-populate .prototype with a fresh object so
  // `Foo.prototype.bar = ...` and `new Foo()` work; .constructor is set after.
  let name_prop =
    common.fn_name_property(option.unwrap(child_template.name, ""))
  let length_prop = common.fn_length_property(child_template.arity)
  let #(heap, fn_properties, proto_ref) = case child_template.is_arrow {
    True -> #(
      heap,
      dict.from_list([
        #(value.Named("name"), name_prop),
        #(value.Named("length"), length_prop),
      ]),
      None,
    )
    False -> {
      let #(h, proto_obj_ref) =
        common.alloc_wrapper(heap, OrdinaryObject, builtins.object.prototype)
      #(
        h,
        dict.from_list([
          #(
            value.Named("prototype"),
            value.data(JsObject(proto_obj_ref)) |> value.writable(),
          ),
          #(value.Named("name"), name_prop),
          #(value.Named("length"), length_prop),
        ]),
        Some(proto_obj_ref),
      )
    }
  }
  let #(heap, closure_ref) =
    heap.alloc(
      heap,
      ObjectSlot(
        // A class constructor's [[HomeObject]] is its own .prototype, so
        // `super.x` inside the constructor resolves against the parent
        // prototype. Concise methods/accessors get their home re-set by
        // DefineMethod/DefineAccessor; for plain functions `super` is a syntax
        // error so this is never read.
        kind: FunctionObject(
          func_template: child_template,
          env: env_ref,
          home_object: proto_ref,
        ),
        properties: fn_properties,
        elements: elements.new(),
        prototype: Some(builtins.function.prototype),
        symbol_properties: [],
        extensible: True,
      ),
    )
  // Set .constructor on the prototype pointing back to this function.
  let heap = case proto_ref {
    Some(pr) -> {
      use slot <- heap.update(heap, pr)
      case slot {
        ObjectSlot(properties: props, ..) ->
          ObjectSlot(
            ..slot,
            properties: dict.insert(
              props,
              value.Named("constructor"),
              value.builtin_property(JsObject(closure_ref)),
            ),
          )
        _ -> slot
      }
    }
    None -> heap
  }
  #(heap, closure_ref)
}

/// Read the current frame's lexical `this`. Unboxes when the slot is boxed
/// (captured by an inner arrow, or the frame IS an arrow reading its capture).
/// Returns JsUndefined when this_slot is None (defensive — `this` unreferenced).
pub fn read_this_local(state: State) -> JsValue {
  case state.func.this_slot {
    None -> JsUndefined
    Some(idx) ->
      case tuple_array.unsafe_get(idx, state.locals) {
        JsObject(ref) as raw ->
          case heap.read_box(state.heap, ref) {
            Some(boxed) -> boxed
            None -> raw
          }
        raw -> raw
      }
  }
}

// ============================================================================
// Execution loop
// ============================================================================

/// Main execution loop. Tail-recursive.
/// Returns the completion and the final state (for job queue access).
///
/// Every bytecode stream ends with a sentinel Return (appended by
/// resolve.gleam), so fetch uses unchecked element/2 — no Option box,
/// no bounds check. Termination flows through the Return handler.
pub fn execute_inner(state: State) -> Result(#(Completion, State), VmError) {
  let op = tuple_array.unsafe_get(state.pc, state.code)
  case step(state, op) {
    Ok(new_state) -> execute_inner(new_state)
    Error(#(Done, result, post)) ->
      Ok(#(NormalCompletion(result, post.heap), post))
    Error(#(StepVmError(err), _, _)) -> Error(err)
    Error(#(Yielded, yielded_value, post)) -> {
      // Generator yielded — build suspended state.
      // For Yield: pop the yielded value from stack, advance pc.
      // For YieldStar: pop arg (keep iter), DON'T advance pc — resume
      //   re-executes YieldStar with [resume_val, iter, ..].
      // For InitialYield: stack unchanged, just advance pc.
      // Yield/Await handlers don't mutate stack before erroring; pc/stack
      // surgery here is written against pre-step state.
      let heap = post.heap
      let suspended_state = case op {
        Yield ->
          State(
            ..state,
            heap:,
            stack: case state.stack {
              [_, ..rest] -> rest
              [] -> []
            },
            pc: state.pc + 1,
          )
        YieldStar ->
          State(..state, heap:, stack: case state.stack {
            [_arg, ..rest] -> rest
            [] -> []
          })
        AsyncYieldStarResume(next_pc:) ->
          // Pre-step stack is [result_obj, iter, ..]. result_obj was fully
          // consumed by step_generators (its .value is yielded_value, .done
          // was false). Drop it so saved stack = [iter, ..]; resume pushes
          // the .next(v) arg → [v, iter, ..] and pc jumps back to Next.
          State(..state, heap:, pc: next_pc, stack: case state.stack {
            [_result_obj, ..rest] -> rest
            [] -> []
          })
        _ -> State(..state, heap:, pc: state.pc + 1)
      }
      Ok(#(YieldCompletion(yielded_value, heap), suspended_state))
    }
    Error(#(Awaited, awaited_value, post)) -> {
      // Async function/generator hit await — pop value, advance pc.
      let heap = post.heap
      let suspended_state =
        State(
          ..state,
          heap:,
          stack: case state.stack {
            [_, ..rest] -> rest
            [] -> []
          },
          pc: state.pc + 1,
        )
      Ok(#(AwaitCompletion(awaited_value, heap), suspended_state))
    }
    Error(#(Thrown, thrown_value, post)) -> {
      // Try to unwind to a catch handler. The handler's full post-step state
      // (stack/pc included) is threaded here so opcodes can mutate stack
      // before throwing (e.g., undef the iter slot then propagate).
      case unwind_to_catch(post, thrown_value) {
        Some(caught_state) -> execute_inner(caught_state)
        None -> Ok(#(ThrowCompletion(thrown_value, post.heap), post))
      }
    }
  }
}

/// Try to find a catch handler for a thrown value. Walks up call_stack when
/// the current frame's try_stack is exhausted, so throws from a callee can be
/// caught by a try/catch in the caller.
fn unwind_to_catch(state: State, thrown_value: JsValue) -> Option(State) {
  case state.try_stack {
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
    [] ->
      case state.call_stack {
        [] -> None
        [
          SavedFrame(
            func:,
            locals:,
            stack:,
            pc:,
            try_stack:,
            callee_ref:,
            call_args:,
            eval_env:,
            current_line:,
            ..,
          ),
          ..rest_frames
        ] ->
          unwind_to_catch(
            State(
              ..state,
              func:,
              code: func.bytecode,
              constants: func.constants,
              locals:,
              stack:,
              pc:,
              try_stack:,
              callee_ref:,
              call_args:,
              eval_env:,
              current_line:,
              call_stack: rest_frames,
              call_depth: state.call_depth - 1,
            ),
            thrown_value,
          )
      }
  }
}

/// Truncate stack to a given depth.
fn truncate_stack(stack: List(JsValue), depth: Int) -> List(JsValue) {
  let excess = list.length(stack) - depth
  case excess > 0 {
    True -> list.drop(stack, excess)
    False -> stack
  }
}

fn underflow(
  state: State,
  op: String,
) -> Result(State, #(StepResult, JsValue, State)) {
  Error(#(StepVmError(StackUnderflow(op)), JsUndefined, state))
}

/// Pop top of stack and jump to `target` if `condition(value)` is true,
/// otherwise advance to next instruction.
fn conditional_jump(
  state: State,
  target: Int,
  condition: fn(JsValue) -> Bool,
) -> Result(State, #(StepResult, JsValue, State)) {
  case state.stack {
    [top, ..rest] ->
      case condition(top) {
        True -> Ok(State(..state, stack: rest, pc: target))
        False -> Ok(State(..state, stack: rest, pc: state.pc + 1))
      }
    [] -> underflow(state, "ConditionalJump")
  }
}

// ============================================================================
// Step — single instruction dispatch
// ============================================================================

/// Execute a single instruction. Returns Ok(new_state) to continue,
/// or Error(#(signal, value, heap)) to stop.
fn step(state: State, op: Op) -> Result(State, #(StepResult, JsValue, State)) {
  case op {
    // ---- Source mapping ----------------------------------------------
    SetLine(line) -> Ok(State(..state, current_line: line, pc: state.pc + 1))

    // ---- Stack operations --------------------------------------------
    PushConst(index) -> {
      let value = tuple_array.unsafe_get(index, state.constants)
      Ok(State(..state, stack: [value, ..state.stack], pc: state.pc + 1))
    }

    Pop -> {
      case state.stack {
        [_, ..rest] -> Ok(State(..state, stack: rest, pc: state.pc + 1))
        [] -> underflow(state, "Pop")
      }
    }

    Dup -> {
      case state.stack {
        [top, ..] ->
          Ok(State(..state, stack: [top, ..state.stack], pc: state.pc + 1))
        [] -> underflow(state, "Dup")
      }
    }

    Swap -> {
      case state.stack {
        [a, b, ..rest] ->
          Ok(State(..state, stack: [b, a, ..rest], pc: state.pc + 1))
        _ -> underflow(state, "Swap")
      }
    }

    // ---- Local variable access ---------------------------------------
    GetLocal(index) -> {
      case tuple_array.unsafe_get(index, state.locals) {
        JsUninitialized ->
          state.throw_reference_error(
            state,
            "Cannot access variable before initialization (TDZ)",
          )
        value ->
          Ok(State(..state, stack: [value, ..state.stack], pc: state.pc + 1))
      }
    }

    PutLocal(index) -> {
      case state.stack {
        [value, ..rest] -> {
          let locals = tuple_array.set_unchecked(index, value, state.locals)
          Ok(State(..state, stack: rest, locals:, pc: state.pc + 1))
        }
        [] -> underflow(state, "PutLocal")
      }
    }

    BoxLocal(index) -> {
      let current_value = tuple_array.unsafe_get(index, state.locals)
      let #(heap, box_ref) =
        heap.alloc(state.heap, value.BoxSlot(current_value))
      let locals =
        tuple_array.set_unchecked(index, JsObject(box_ref), state.locals)
      Ok(State(..state, heap:, locals:, pc: state.pc + 1))
    }

    GetBoxed(index) -> {
      case tuple_array.unsafe_get(index, state.locals) {
        JsObject(box_ref) ->
          case heap.read_box(state.heap, box_ref) {
            Some(JsUninitialized) ->
              state.throw_reference_error(
                state,
                "Cannot access variable before initialization (TDZ)",
              )
            Some(val) ->
              Ok(State(..state, stack: [val, ..state.stack], pc: state.pc + 1))
            None ->
              Error(#(
                StepVmError(Unimplemented("GetBoxed: not a BoxSlot")),
                JsUndefined,
                state,
              ))
          }
        _ ->
          Error(#(
            StepVmError(Unimplemented("GetBoxed: local is not a box ref")),
            JsUndefined,
            state,
          ))
      }
    }

    PutBoxed(index) -> {
      case state.stack {
        [new_value, ..rest_stack] -> {
          case tuple_array.unsafe_get(index, state.locals) {
            JsObject(box_ref) -> {
              let heap =
                heap.write(state.heap, box_ref, value.BoxSlot(new_value))
              Ok(State(..state, heap:, stack: rest_stack, pc: state.pc + 1))
            }
            _ ->
              Error(#(
                StepVmError(Unimplemented("PutBoxed: local is not a box ref")),
                JsUndefined,
                state,
              ))
          }
        }
        [] -> underflow(state, "PutBoxed")
      }
    }

    // ---- Global variable access --------------------------------------
    // §9.1.1.4.4 GetBindingValue — two-phase: declarative then object record
    GetGlobal(name) -> {
      case dict.get(state.lexical_globals, name) {
        // Lexical binding exists — check for TDZ
        Ok(JsUninitialized) ->
          state.throw_reference_error(
            state,
            "Cannot access '" <> name <> "' before initialization",
          )
        Ok(value) ->
          Ok(State(..state, stack: [value, ..state.stack], pc: state.pc + 1))
        // Not in lexical → try object record (globalThis)
        Error(_) -> {
          let key = Named(name)
          case object.get_own_property(state.heap, state.global_object, key) {
            Some(DataProperty(value: val, ..)) ->
              Ok(State(..state, stack: [val, ..state.stack], pc: state.pc + 1))
            Some(value.AccessorProperty(get: Some(getter), ..)) ->
              case
                state.call(state, getter, JsObject(state.global_object), [])
              {
                Ok(#(val, state)) ->
                  Ok(
                    State(
                      ..state,
                      stack: [val, ..state.stack],
                      pc: state.pc + 1,
                    ),
                  )
                Error(#(thrown, state)) -> Error(#(Thrown, thrown, state))
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
              case object.has_property(state.heap, state.global_object, key) {
                True ->
                  case
                    object.get_value_of(
                      state,
                      JsObject(state.global_object),
                      key,
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
                    Error(#(thrown, state)) -> Error(#(Thrown, thrown, state))
                  }
                False ->
                  state.throw_reference_error(state, name <> " is not defined")
              }
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
              state.throw_type_error(state, "Assignment to constant variable.")
            False ->
              // 2. Check lexical globals
              case dict.get(state.lexical_globals, name) {
                Ok(JsUninitialized) ->
                  state.throw_reference_error(
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
                Error(_) -> {
                  let key = Named(name)
                  case state.func.is_strict {
                    True ->
                      // Strict mode: must exist on globalThis or throw
                      case
                        object.has_property(
                          state.heap,
                          state.global_object,
                          key,
                        )
                      {
                        False ->
                          state.throw_reference_error(
                            state,
                            name <> " is not defined",
                          )
                        True ->
                          case
                            object.set_value(
                              State(..state, stack: rest),
                              state.global_object,
                              key,
                              value,
                              JsObject(state.global_object),
                            )
                          {
                            Ok(#(state, True)) ->
                              Ok(State(..state, pc: state.pc + 1))
                            Ok(#(state, False)) ->
                              state.throw_type_error(
                                state,
                                "Cannot assign to read only property '"
                                  <> name
                                  <> "' of object '#<Object>'",
                              )
                            Error(#(thrown, state)) ->
                              Error(#(Thrown, thrown, state))
                          }
                      }
                    False ->
                      // Sloppy mode: set on globalThis (creates if needed,
                      // returns False for non-writable → silently ignore)
                      case
                        object.set_value(
                          State(..state, stack: rest),
                          state.global_object,
                          key,
                          value,
                          JsObject(state.global_object),
                        )
                      {
                        Ok(#(state, _)) -> Ok(State(..state, pc: state.pc + 1))
                        Error(#(thrown, state)) ->
                          Error(#(Thrown, thrown, state))
                      }
                  }
                }
              }
          }
        }
        [] -> underflow(state, "PutGlobal")
      }
    }

    // §9.1.1.4.17 CreateGlobalVarBinding — create var on globalThis
    DeclareGlobalVar(name) -> {
      let key = Named(name)
      case object.has_property(state.heap, state.global_object, key) {
        True ->
          // Already exists — no-op
          Ok(State(..state, pc: state.pc + 1))
        False -> {
          let #(heap, _) =
            object.set_property(
              state.heap,
              state.global_object,
              key,
              JsUndefined,
            )
          Ok(State(..state, heap:, pc: state.pc + 1))
        }
      }
    }

    // Sloppy direct-eval var access: check eval_env dict, fall through to globals.
    GetEvalVar(name) -> {
      case lookup_eval_env(state, name) {
        Some(v) ->
          Ok(State(..state, stack: [v, ..state.stack], pc: state.pc + 1))
        None -> step(state, GetGlobal(name))
      }
    }

    // typeof on a name that might live in eval_env.
    TypeofEvalVar(name) -> {
      case lookup_eval_env(state, name) {
        Some(v) ->
          Ok(
            State(
              ..state,
              stack: [
                JsString(common.typeof_value(v, state.heap)),
                ..state.stack
              ],
              pc: state.pc + 1,
            ),
          )
        None -> step(state, TypeofGlobal(name))
      }
    }

    // Sloppy direct-eval var write: update eval_env if key exists, else PutGlobal.
    PutEvalVar(name) -> {
      case state.eval_env, state.stack {
        Some(ref), [v, ..rest] -> {
          let vars =
            heap.read_eval_env(state.heap, ref) |> option.unwrap(dict.new())
          case dict.has_key(vars, name) {
            False -> step(state, PutGlobal(name))
            True -> {
              let heap =
                heap.write(
                  state.heap,
                  ref,
                  EvalEnvSlot(dict.insert(vars, name, v)),
                )
              Ok(State(..state, heap:, stack: rest, pc: state.pc + 1))
            }
          }
        }
        _, _ -> step(state, PutGlobal(name))
      }
    }

    // Sloppy direct-eval var declaration: seed name=undefined into eval_env.
    DeclareEvalVar(name) -> {
      case state.eval_env {
        None -> step(state, DeclareGlobalVar(name))
        Some(ref) -> {
          let vars =
            heap.read_eval_env(state.heap, ref) |> option.unwrap(dict.new())
          let heap = case dict.has_key(vars, name) {
            True -> state.heap
            False ->
              heap.write(
                state.heap,
                ref,
                EvalEnvSlot(dict.insert(vars, name, JsUndefined)),
              )
          }
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
        [] -> underflow(state, "InitGlobalLex")
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
        [] -> underflow(state, "TypeOf")
      }
    }

    // §9.1.1.4: typeof on globals — TDZ throws, undeclared returns "undefined"
    TypeofGlobal(name) -> {
      case dict.get(state.lexical_globals, name) {
        // TDZ — typeof on uninitialized lexical still throws per spec
        Ok(JsUninitialized) ->
          state.throw_reference_error(
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
          let key = Named(name)
          let val = case
            object.get_own_property(state.heap, state.global_object, key)
          {
            Some(DataProperty(value: v, ..)) -> v
            _ ->
              case object.has_property(state.heap, state.global_object, key) {
                True ->
                  // Property exists on proto chain — use get_value_of for correct result
                  case
                    object.get_value_of(
                      state,
                      JsObject(state.global_object),
                      key,
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

    // ---- Operators ---------------------------------------------------
    BinOp(kind) -> {
      case state.stack {
        [right, left, ..rest] -> {
          // instanceof and in need heap access
          case kind {
            opcode.InstanceOf -> {
              use #(result, state) <- result.map(
                state.rethrow(coerce.js_instanceof(state, left, right)),
              )
              State(..state, stack: [JsBool(result), ..rest], pc: state.pc + 1)
            }
            opcode.In -> {
              // left = key, right = object
              case right {
                JsObject(ref) -> {
                  use #(result, state) <- result.map(case left {
                    value.JsSymbol(sym) ->
                      Ok(#(
                        object.has_symbol_property(state.heap, ref, sym),
                        state,
                      ))
                    _ ->
                      case property.to_property_key(state, left) {
                        Ok(#(pk, state)) ->
                          Ok(#(object.has_property(state.heap, ref, pk), state))
                        Error(#(thrown, state)) ->
                          Error(#(Thrown, thrown, state))
                      }
                  })
                  State(
                    ..state,
                    stack: [JsBool(result), ..rest],
                    pc: state.pc + 1,
                  )
                }
                _ ->
                  state.throw_type_error(
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
                JsObject(_), _ | _, JsObject(_) ->
                  binop_add_with_to_primitive(state, left, right, rest)
                _, _ -> add_primitives(state, left, right, rest)
              }
            // Strict equality compares object references — never coerce.
            opcode.StrictEq | opcode.StrictNotEq ->
              binop_direct(state, kind, left, right, rest)
            // Loose equality: §7.2.14 step 12 only ToPrimitives the object
            // side when the other is Number/String/BigInt/Symbol. Bool is
            // first ToNumber'd (step 10) so it ends up here too. For
            // object×object (reference equality) and object×nullish (always
            // false) we stay on the direct path.
            opcode.Eq | opcode.NotEq ->
              case is_eq_coercible(left, right) {
                True -> binop_with_to_primitive(state, kind, left, right, rest)
                False -> binop_direct(state, kind, left, right, rest)
              }
            // All remaining ops are numeric/relational/bitwise: ToNumeric →
            // ToPrimitive(number) on both operands (§13.15.4).
            _ ->
              case left, right {
                JsObject(_), _ | _, JsObject(_) ->
                  binop_with_to_primitive(state, kind, left, right, rest)
                _, _ -> binop_direct(state, kind, left, right, rest)
              }
          }
        }
        _ -> underflow(state, "BinOp")
      }
    }

    UnaryOp(kind) -> {
      case state.stack {
        [operand, ..rest] ->
          case operand, kind {
            JsObject(_), opcode.Neg
            | JsObject(_), opcode.Pos
            | JsObject(_), opcode.BitNot
            -> unaryop_with_to_primitive(state, kind, operand, rest)
            _, _ ->
              case operators.exec_unaryop(kind, operand) {
                Ok(result) ->
                  Ok(State(..state, stack: [result, ..rest], pc: state.pc + 1))
                Error(msg) -> state.throw_type_error(state, msg)
              }
          }
        [] -> underflow(state, "UnaryOp")
      }
    }

    // ---- Control flow ------------------------------------------------
    Return -> {
      let return_value = case state.stack {
        [value, ..] -> value
        [] -> JsUndefined
      }
      case state.call_stack {
        // No caller — top-level return, we're done
        [] -> Error(#(Done, return_value, state))
        // Pop call frame, restore caller, push return value onto caller's stack
        [
          SavedFrame(
            func:,
            locals:,
            stack:,
            pc:,
            try_stack:,
            constructor_this:,
            callee_ref: saved_callee_ref,
            call_args: saved_call_args,
            eval_env: saved_eval_env,
            current_line: saved_current_line,
          ),
          ..rest_frames
        ] -> {
          // Restore the caller's source line before any reconstruction below
          // (they all spread `..state`), so a throw in the remainder of the
          // caller's statement reports the caller's line, not the callee's.
          let state = State(..state, current_line: saved_current_line)
          // Constructor return semantics
          case constructor_this {
            Some(constructed_obj) -> {
              // Base constructor: use the constructed object unless the
              // function explicitly returned an object.
              // §13.3.7.1 SuperCall step 8 BindThisValue(result) is now handled
              // at the call site: emit's `CallSuper; Dup; IrSetThis` writes
              // effective_return into the caller's lexical-`this` slot.
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
                  callee_ref: saved_callee_ref,
                  call_args: saved_call_args,
                  eval_env: saved_eval_env,
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
                          callee_ref: saved_callee_ref,
                          call_args: saved_call_args,
                          eval_env: saved_eval_env,
                        ),
                      )
                    JsUndefined ->
                      case read_this_local(state) {
                        JsUninitialized -> {
                          state.throw_reference_error(
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
                              callee_ref: saved_callee_ref,
                              call_args: saved_call_args,
                              eval_env: saved_eval_env,
                            ),
                          )
                      }
                    _ -> {
                      state.throw_type_error(
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
                      callee_ref: saved_callee_ref,
                      eval_env: saved_eval_env,
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

    // QuickJS OP_gosub: push return-PC as a tagged number, jump to finally body.
    opcode.Gosub(target) ->
      Ok(
        State(
          ..state,
          stack: [value.from_int(state.pc + 1), ..state.stack],
          pc: target,
        ),
      )

    // QuickJS OP_ret: pop return-PC, jump back to it. A negative retpc is the
    // sentinel pushed by generator .return() finally-unwinding (see
    // process_generator_return) — it means "the slot below me is a return
    // value, complete the frame with it".
    opcode.Ret ->
      case state.stack {
        [value.JsNumber(value.Finite(f)), slot, ..rest] if f <. 0.0 ->
          Error(#(Done, slot, State(..state, stack: rest)))
        [value.JsNumber(value.Finite(f)), ..rest] ->
          Ok(State(..state, stack: rest, pc: float.truncate(f)))
        _ -> underflow(state, "Ret")
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
        [] -> underflow(state, "PopTry: empty try_stack")
      }
    }

    opcode.Throw -> {
      case state.stack {
        [value, ..] -> Error(#(Thrown, value, state))
        [] -> underflow(state, "Throw")
      }
    }

    // ---- Object property access --------------------------------------
    NewObject -> {
      let #(heap, ref) =
        common.alloc_wrapper(
          state.heap,
          OrdinaryObject,
          state.builtins.object.prototype,
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
      let key = value.canonical_key(name)
      case state.stack {
        [JsNull as v, ..] | [JsUndefined as v, ..] ->
          state.throw_type_error(
            state,
            "Cannot read properties of "
              <> value.nullish_label(v)
              <> " (reading '"
              <> name
              <> "')",
          )
        [receiver, ..rest] -> {
          use #(val, state) <- result.map(
            state.rethrow(object.get_value_of(state, receiver, key)),
          )
          State(..state, stack: [val, ..rest], pc: state.pc + 1)
        }
        [] -> underflow(state, "GetField")
      }
    }

    GetField2(name) -> {
      // Like GetField but keeps the object on the stack for CallMethod.
      // Stack: [obj, ..rest] → [prop_value, obj, ..rest]
      let key = value.canonical_key(name)
      case state.stack {
        [JsNull as v, ..] | [JsUndefined as v, ..] ->
          state.throw_type_error(
            state,
            "Cannot read properties of "
              <> value.nullish_label(v)
              <> " (reading '"
              <> name
              <> "')",
          )
        [receiver, ..rest] -> {
          use #(val, state) <- result.map(
            state.rethrow(object.get_value_of(state, receiver, key)),
          )
          State(..state, stack: [val, receiver, ..rest], pc: state.pc + 1)
        }
        [] -> underflow(state, "GetField2")
      }
    }

    PutField(name) -> {
      // Consumes [value, obj] and pushes value back (assignment is an expression).
      // Consistent with PutElem which also leaves the value on the stack.
      let key = value.canonical_key(name)
      case state.stack {
        [value, JsObject(ref) as receiver, ..rest] -> {
          // set_value walks proto chain, calls setters, handles non-writable.
          // Sloppy mode: ignore failure (strict mode TypeError is a TODO).
          use #(state, _ok) <- result.map(
            state.rethrow(object.set_value(state, ref, key, value, receiver)),
          )
          State(..state, stack: [value, ..rest], pc: state.pc + 1)
        }
        [value, _, ..rest] -> {
          // PutField on non-object: silently ignore, still return value
          Ok(State(..state, stack: [value, ..rest], pc: state.pc + 1))
        }
        _ -> underflow(state, "PutField")
      }
    }

    GetPrivateField(name) -> {
      let key = value.canonical_key(name)
      case state.stack {
        [JsObject(ref) as receiver, ..rest] ->
          case object.has_property(state.heap, ref, key) {
            True -> {
              use #(val, state) <- result.map(
                state.rethrow(object.get_value_of(state, receiver, key)),
              )
              State(..state, stack: [val, ..rest], pc: state.pc + 1)
            }
            False ->
              state.throw_type_error(
                state,
                "Cannot read private member "
                  <> name
                  <> " from an object whose class did not declare it",
              )
          }
        [_, ..] ->
          state.throw_type_error(
            state,
            "Cannot read private member " <> name <> " on non-object",
          )
        [] -> underflow(state, "GetPrivateField")
      }
    }

    GetPrivateField2(name) -> {
      let key = value.canonical_key(name)
      case state.stack {
        [JsObject(ref) as receiver, ..rest] ->
          case object.has_property(state.heap, ref, key) {
            True -> {
              use #(val, state) <- result.map(
                state.rethrow(object.get_value_of(state, receiver, key)),
              )
              State(..state, stack: [val, receiver, ..rest], pc: state.pc + 1)
            }
            False ->
              state.throw_type_error(
                state,
                "Cannot read private member "
                  <> name
                  <> " from an object whose class did not declare it",
              )
          }
        [_, ..] ->
          state.throw_type_error(
            state,
            "Cannot read private member " <> name <> " on non-object",
          )
        [] -> underflow(state, "GetPrivateField2")
      }
    }

    PutPrivateField(name) -> {
      let key = value.canonical_key(name)
      case state.stack {
        [val, JsObject(ref) as receiver, ..rest] ->
          case object.has_property(state.heap, ref, key) {
            True -> {
              use #(state, _ok) <- result.map(
                state.rethrow(object.set_value(state, ref, key, val, receiver)),
              )
              State(..state, stack: [val, ..rest], pc: state.pc + 1)
            }
            False ->
              state.throw_type_error(
                state,
                "Cannot write private member "
                  <> name
                  <> " to an object whose class did not declare it",
              )
          }
        [_, _, ..] ->
          state.throw_type_error(
            state,
            "Cannot write private member " <> name <> " on non-object",
          )
        _ -> underflow(state, "PutPrivateField")
      }
    }

    PrivateIn(name) -> {
      let key = value.canonical_key(name)
      case state.stack {
        [JsObject(ref), ..rest] ->
          Ok(
            State(
              ..state,
              stack: [JsBool(object.has_property(state.heap, ref, key)), ..rest],
              pc: state.pc + 1,
            ),
          )
        [_, ..] ->
          state.throw_type_error(
            state,
            "Cannot use 'in' operator to search for private name "
              <> name
              <> " in non-object",
          )
        [] -> underflow(state, "PrivateIn")
      }
    }

    DefineField(name) -> {
      // Like PutField but keeps the object on the stack (for object literal construction)
      let key = value.canonical_key(name)
      case state.stack {
        [value, JsObject(ref) as obj, ..rest] -> {
          let #(heap, _) = object.set_property(state.heap, ref, key, value)
          Ok(State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1))
        }
        [_, _, ..] -> {
          // DefineField on non-object: no-op, keep object on stack
          Ok(State(..state, pc: state.pc + 1))
        }
        _ -> underflow(state, "DefineField")
      }
    }

    DefineMethod(name) -> {
      // Like DefineField but creates a non-enumerable property (for class methods)
      let key = value.canonical_key(name)
      case state.stack {
        [value, JsObject(ref) as obj, ..rest] -> {
          let heap = set_home_object(state.heap, value, ref)
          let heap = object.define_method_property(heap, ref, key, value)
          Ok(State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1))
        }
        [_, _, ..] -> Ok(State(..state, pc: state.pc + 1))
        _ -> underflow(state, "DefineMethod")
      }
    }

    DefineMethodComputed -> {
      // Computed class method: class { [expr]() {} }
      // Stack: [fn, key, obj, ...] → [obj, ...]
      // Non-enumerable data property (writable, configurable).
      case state.stack {
        [func, value.JsSymbol(sym), JsObject(ref) as obj, ..rest] -> {
          let heap = set_home_object(state.heap, func, ref)
          let heap =
            object.define_symbol_property(
              heap,
              ref,
              sym,
              value.builtin_property(func),
            )
          Ok(State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1))
        }
        [func, key, JsObject(ref) as obj, ..rest] -> {
          use #(pk, state) <- result.map(
            state.rethrow(property.to_property_key(state, key)),
          )
          let heap = set_home_object(state.heap, func, ref)
          let heap = object.define_method_property(heap, ref, pk, func)
          State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1)
        }
        [_, _, _, ..] -> Ok(State(..state, pc: state.pc + 1))
        _ -> underflow(state, "DefineMethodComputed")
      }
    }

    DefineAccessor(name, kind) -> {
      // Object literal getter/setter: { get x() {}, set x(v) {} }
      // Stack: [fn, obj, ...] → [obj, ...]
      // Defines or updates an AccessorProperty on the object.
      let key = value.canonical_key(name)
      case state.stack {
        [func, JsObject(ref) as obj, ..rest] -> {
          let heap = set_home_object(state.heap, func, ref)
          let heap = object.define_accessor(heap, ref, key, func, kind)
          Ok(State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1))
        }
        [_, _, ..] -> Ok(State(..state, pc: state.pc + 1))
        _ -> underflow(state, "DefineAccessor")
      }
    }

    DefineAccessorComputed(kind) -> {
      // Computed getter/setter: { get [expr]() {} }
      // Stack: [fn, key, obj, ...] → [obj, ...]
      case state.stack {
        [func, value.JsSymbol(sym), JsObject(ref) as obj, ..rest] -> {
          let heap = set_home_object(state.heap, func, ref)
          let heap = object.define_symbol_accessor(heap, ref, sym, func, kind)
          Ok(State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1))
        }
        [func, key, JsObject(ref) as obj, ..rest] -> {
          use #(pk, state) <- result.map(
            state.rethrow(property.to_property_key(state, key)),
          )
          let heap = set_home_object(state.heap, func, ref)
          let heap = object.define_accessor(heap, ref, pk, func, kind)
          State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1)
        }
        [_, _, _, ..] -> Ok(State(..state, pc: state.pc + 1))
        _ -> underflow(state, "DefineAccessorComputed")
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
            state.rethrow(property.put_elem_value(state, ref, key, val)),
          )
          State(..state, stack: [obj, ..rest], pc: state.pc + 1)
        }
        [_, _, _, ..rest] ->
          // Non-object target: shouldn't happen for literals, but pop and keep going.
          Ok(State(..state, stack: rest, pc: state.pc + 1))
        _ -> underflow(state, "DefineFieldComputed")
      }
    }

    DefineMethodField(name) -> {
      // Object-literal concise method: enumerable data property (like
      // DefineField) but records the method's [[HomeObject]] for `super`.
      let key = value.canonical_key(name)
      case state.stack {
        [value, JsObject(ref) as obj, ..rest] -> {
          let heap = set_home_object(state.heap, value, ref)
          let #(heap, _) = object.set_property(heap, ref, key, value)
          Ok(State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1))
        }
        [_, _, ..] -> Ok(State(..state, pc: state.pc + 1))
        _ -> underflow(state, "DefineMethodField")
      }
    }

    DefineMethodFieldComputed -> {
      // Object-literal concise method with a numeric/computed key.
      case state.stack {
        [val, key, JsObject(ref) as obj, ..rest] -> {
          let heap = set_home_object(state.heap, val, ref)
          use state <- result.map(
            state.rethrow(property.put_elem_value(
              State(..state, heap:),
              ref,
              key,
              val,
            )),
          )
          State(..state, stack: [obj, ..rest], pc: state.pc + 1)
        }
        [_, _, _, ..rest] -> Ok(State(..state, stack: rest, pc: state.pc + 1))
        _ -> underflow(state, "DefineMethodFieldComputed")
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
            state.rethrow(object.copy_data_properties(state, ref, source)),
          )
          State(..state, stack: [obj, ..rest], pc: state.pc + 1)
        }
        [_, _, ..rest] -> Ok(State(..state, stack: rest, pc: state.pc + 1))
        _ -> underflow(state, "ObjectSpread")
      }
    }

    // Destructuring rest: `let {a, b, ...rest} = src`
    // Stack: [src, key_n, ..., key_1, ...] → [rest_obj, ...]
    // §13.15.5.3 RestBindingInitialization → CopyDataProperties with
    // excludedNames = the n keys already bound.
    ObjectRestCopy(excluded_count) ->
      case state.stack {
        [source, ..below] ->
          case pop_n(below, excluded_count) {
            Some(#(raw_keys, rest)) -> {
              let state = State(..state, stack: rest)
              // §8.6.2 RequireObjectCoercible — unlike object-spread,
              // `let {...x} = null` MUST throw TypeError.
              case source {
                JsNull ->
                  state.throw_type_error(
                    state,
                    "Cannot destructure 'null' as it is null.",
                  )
                JsUndefined ->
                  state.throw_type_error(
                    state,
                    "Cannot destructure 'undefined' as it is undefined.",
                  )
                _ -> {
                  // ToPropertyKey each excluded key (computed keys arrive
                  // as raw JsValue; static keys are already JsString).
                  use #(ex_keys, ex_syms, state) <- result.try(
                    state.rethrow(build_exclusion_sets(state, raw_keys)),
                  )
                  let #(heap, ref) =
                    common.alloc_wrapper(
                      state.heap,
                      OrdinaryObject,
                      state.builtins.object.prototype,
                    )
                  let state = State(..state, heap:)
                  use state <- result.map(
                    state.rethrow(object.copy_data_properties_excluding(
                      state,
                      ref,
                      source,
                      ex_keys,
                      ex_syms,
                    )),
                  )
                  State(
                    ..state,
                    stack: [JsObject(ref), ..state.stack],
                    pc: state.pc + 1,
                  )
                }
              }
            }
            None -> underflow(state, "ObjectRestCopy")
          }
        _ -> underflow(state, "ObjectRestCopy")
      }

    // -- Delete operator --
    DeleteField(name) -> {
      let key = value.canonical_key(name)
      case state.stack {
        [obj, ..rest] ->
          case obj {
            JsObject(ref) -> {
              let #(heap, success) =
                object.delete_property(state.heap, ref, key)
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
        _ -> underflow(state, "DeleteField")
      }
    }

    DeleteElem -> {
      case state.stack {
        [key, obj, ..rest] ->
          case obj {
            JsObject(ref) ->
              case key {
                value.JsSymbol(sym) -> {
                  let #(heap, success) =
                    object.delete_symbol_property(state.heap, ref, sym)
                  Ok(
                    State(
                      ..state,
                      stack: [JsBool(success), ..rest],
                      heap:,
                      pc: state.pc + 1,
                    ),
                  )
                }
                _ -> {
                  use #(pk, state) <- result.map(
                    state.rethrow(property.to_property_key(state, key)),
                  )
                  let #(heap, success) =
                    object.delete_property(state.heap, ref, pk)
                  State(
                    ..state,
                    stack: [JsBool(success), ..rest],
                    heap:,
                    pc: state.pc + 1,
                  )
                }
              }
            _ ->
              Ok(
                State(..state, stack: [JsBool(True), ..rest], pc: state.pc + 1),
              )
          }
        _ -> underflow(state, "DeleteElem")
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
            Some(ObjectSlot(kind: FunctionObject(..), ..))
            | Some(ObjectSlot(kind: NativeFunction(..), ..)) -> {
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
          state.throw_type_error(
            state,
            "Class extends value is not a constructor or null",
          )
        }
      }
    }

    // ---- Array operations --------------------------------------------
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
                elements: elements.from_list(elements),
                prototype: Some(state.builtins.array.prototype),
                symbol_properties: [],
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
        None -> underflow(state, "ArrayFrom")
      }
    }

    ArrayFromWithHoles(count, holes) -> {
      // Pop only the non-hole values (count - len(holes)), then zip them with
      // the non-hole indices and build a SparseElements-backed tuple_array.
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
                elements: elements.from_indexed(indexed),
                prototype: Some(state.builtins.array.prototype),
                symbol_properties: [],
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
        None -> underflow(state, "ArrayFromWithHoles")
      }
    }

    // -- Computed property access --
    GetElem -> {
      case state.stack {
        [key, JsObject(ref), ..rest] -> {
          use #(val, state) <- result.map(
            state.rethrow(property.get_elem_value(state, ref, key)),
          )
          State(..state, stack: [val, ..rest], pc: state.pc + 1)
        }
        [_, JsNull as v, ..] | [_, JsUndefined as v, ..] ->
          state.throw_type_error(
            state,
            "Cannot read properties of " <> value.nullish_label(v),
          )
        [key, receiver, ..rest] -> {
          // Primitive receiver: canonicalize key, delegate to get_value_of
          use #(val, state) <- result.map(
            state.rethrow(get_elem_on_primitive(state, receiver, key)),
          )
          State(..state, stack: [val, ..rest], pc: state.pc + 1)
        }
        _ -> underflow(state, "GetElem")
      }
    }

    GetElem2 -> {
      // Like GetElem but keeps obj+key on stack: [key, obj, ...] -> [value, key, obj, ...]
      case state.stack {
        [key, JsObject(ref) as obj, ..rest] -> {
          use #(val, state) <- result.map(
            state.rethrow(property.get_elem_value(state, ref, key)),
          )
          State(..state, stack: [val, key, obj, ..rest], pc: state.pc + 1)
        }
        [key, receiver, ..rest] -> {
          use #(val, state) <- result.map(
            state.rethrow(get_elem_on_primitive(state, receiver, key)),
          )
          State(..state, stack: [val, key, receiver, ..rest], pc: state.pc + 1)
        }
        _ -> underflow(state, "GetElem2")
      }
    }

    PutElem -> {
      // Stack: [value, key, obj, ...rest]
      case state.stack {
        [val, key, JsObject(ref), ..rest] -> {
          use state <- result.map(
            state.rethrow(property.put_elem_value(state, ref, key, val)),
          )
          State(..state, stack: [val, ..rest], pc: state.pc + 1)
        }
        [_, _, _, ..rest] -> {
          // PutElem on non-object: silently ignore (JS sloppy mode)
          Ok(State(..state, stack: rest, pc: state.pc + 1))
        }
        _ -> underflow(state, "PutElem")
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
        _ -> underflow(state, "ArrayPush")
      }
    }

    ArrayPushHole -> {
      // [arr] → [arr]; length++ WITHOUT setting any element.
      case state.stack {
        [JsObject(ref) as arr, ..rest] -> {
          let heap = grow_array_length(state.heap, ref)
          Ok(State(..state, heap:, stack: [arr, ..rest], pc: state.pc + 1))
        }
        _ -> underflow(state, "ArrayPushHole")
      }
    }

    ArraySpread -> {
      // [iterable, arr] → [arr]; drain iterable via the iterator protocol.
      case state.stack {
        [iterable, JsObject(arr_ref) as arr, ..rest] -> {
          use state <- result.map(spread_into_array(state, arr_ref, iterable))
          State(..state, stack: [arr, ..rest], pc: state.pc + 1)
        }
        _ -> underflow(state, "ArraySpread")
      }
    }

    // ---- Function calls ----------------------------------------------
    CallEval(arity) -> {
      // Syntactic `eval(...)` call. Runtime identity check: if the callee
      // resolves to the intrinsic eval function, do a DIRECT eval (sees
      // caller's locals via state.func.local_names + boxed slots). If eval
      // was shadowed/rebound, fall through to regular Call semantics.
      case pop_n(state.stack, arity) {
        Some(#(args, [JsObject(callee_ref), ..rest_stack]))
          if callee_ref == state.builtins.eval
        -> {
          let #(new_state, result) =
            realm.direct_eval_native(
              args,
              State(..state, stack: rest_stack),
              execute_inner,
              new_state,
            )
          case result {
            Ok(val) ->
              Ok(
                State(
                  ..new_state,
                  stack: [val, ..new_state.stack],
                  pc: state.pc + 1,
                ),
              )
            // Unwind directly with new_state so eval_env (possibly just
            // lazy-allocated in run_direct_eval) threads through. The step
            // error return #(Thrown, val, Heap) can't carry it.
            Error(thrown) ->
              case unwind_to_catch(new_state, thrown) {
                Some(caught) -> Ok(caught)
                None -> Error(#(Thrown, thrown, new_state))
              }
          }
        }
        // Not the intrinsic eval — regular call semantics.
        _ -> step(state, Call(arity))
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
                  kind: FunctionObject(func_template:, env: env_ref, ..),
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
                Some(ObjectSlot(kind: NativeFunction(native, ..), ..)) ->
                  call_native(state, native, args, rest_stack, JsUndefined)
                _ ->
                  state.throw_type_error(
                    state,
                    object.inspect(JsObject(obj_ref), state.heap)
                      <> " is not a function",
                  )
              }
            }
            [non_func, ..] ->
              state.throw_type_error(
                state,
                object.inspect(non_func, state.heap) <> " is not a function",
              )
            [] -> underflow(state, "Call: no callee")
          }
        }
        None -> underflow(state, "Call: not enough args")
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
                  kind: FunctionObject(func_template:, env: env_ref, ..),
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
                Some(ObjectSlot(kind: NativeFunction(native, ..), ..)) ->
                  call_native(state, native, args, rest_stack, receiver)
                _ ->
                  state.throw_type_error(
                    state,
                    object.inspect(JsObject(method_ref), state.heap)
                      <> " is not a function",
                  )
              }
            }
            [non_func, _, ..] ->
              state.throw_type_error(
                state,
                object.inspect(non_func, state.heap) <> " is not a function",
              )
            _ -> underflow(state, "CallMethod")
          }
        }
        None -> underflow(state, "CallMethod: not enough args")
      }
    }

    CallConstructor(arity) -> {
      // Stack: [arg_n, ..., arg_1, constructor, ...rest]
      case pop_n(state.stack, arity) {
        Some(#(args, [JsObject(ctor_ref), ..rest_stack])) ->
          do_construct(state, ctor_ref, args, rest_stack)
        Some(#(_, [non_func, ..])) -> {
          state.throw_type_error(
            state,
            object.inspect(non_func, state.heap) <> " is not a constructor",
          )
        }
        Some(#(_, [])) -> underflow(state, "CallConstructor")
        None -> underflow(state, "CallConstructor: not enough args")
      }
    }

    CallSuper(arity) ->
      // Stack: [arg_n, ..., arg_1, ..rest] → [new_obj, ..rest]
      case pop_n(state.stack, arity) {
        Some(#(args, rest_stack)) -> do_call_super(state, args, rest_stack)
        None -> underflow(state, "CallSuper: not enough args")
      }

    CallSuperApply ->
      // Stack: [args_array, ..rest] → [new_obj, ..rest]
      // Spread-super path: args were collected into a runtime array.
      case state.stack {
        [JsObject(args_ref), ..rest] -> {
          let args = extract_array_args(state.heap, args_ref)
          do_call_super(state, args, rest)
        }
        _ -> underflow(state, "CallSuperApply")
      }

    // -- Super property access (§13.3.7) --
    CheckSuperThis ->
      // §13.3.7.2: GetThisBinding throws when `this` is uninitialized.
      case read_this_local(state) {
        JsUninitialized ->
          state.throw_reference_error(
            state,
            "Must call super constructor in derived class before accessing 'super' or a member of 'this'",
          )
        _ -> Ok(State(..state, pc: state.pc + 1))
      }

    GetSuperProp(name) -> {
      let key = value.canonical_key(name)
      get_super_value(state, key, fn(val, state) {
        State(..state, stack: [val, ..state.stack], pc: state.pc + 1)
      })
    }

    GetSuperProp2(name) -> {
      // Method call: leave [method, this, ..] for CallMethod (this is the
      // receiver, NOT the super base — §13.3.7.3 passes actualThis through).
      let key = value.canonical_key(name)
      let this_val = read_this_local(state)
      get_super_value(state, key, fn(method, state) {
        State(..state, stack: [method, this_val, ..state.stack], pc: state.pc + 1)
      })
    }

    GetSuperPropComputed ->
      case state.stack {
        [key_val, ..rest] -> {
          use #(pk, state) <- result.try(
            state.rethrow(property.to_property_key(
              State(..state, stack: rest),
              key_val,
            )),
          )
          get_super_value(state, pk, fn(val, state) {
            State(..state, stack: [val, ..state.stack], pc: state.pc + 1)
          })
        }
        [] -> underflow(state, "GetSuperPropComputed")
      }

    GetSuperPropComputed2 ->
      case state.stack {
        [key_val, ..rest] -> {
          let this_val = read_this_local(state)
          use #(pk, state) <- result.try(
            state.rethrow(property.to_property_key(
              State(..state, stack: rest),
              key_val,
            )),
          )
          get_super_value(state, pk, fn(method, state) {
            State(
              ..state,
              stack: [method, this_val, ..state.stack],
              pc: state.pc + 1,
            )
          })
        }
        [] -> underflow(state, "GetSuperPropComputed2")
      }

    PutSuperProp(name) ->
      case state.stack {
        [val, ..rest] -> {
          let key = value.canonical_key(name)
          put_super_value(State(..state, stack: rest), key, val, rest)
        }
        [] -> underflow(state, "PutSuperProp")
      }

    PutSuperPropComputed ->
      case state.stack {
        [val, key_val, ..rest] -> {
          use #(pk, state) <- result.try(
            state.rethrow(property.to_property_key(
              State(..state, stack: rest),
              key_val,
            )),
          )
          put_super_value(state, pk, val, rest)
        }
        _ -> underflow(state, "PutSuperPropComputed")
      }

    CallApply -> {
      // [args_array, callee] → [result]; this=undefined.
      case state.stack {
        [JsObject(args_ref), callee, ..rest] -> {
          let args = extract_array_args(state.heap, args_ref)
          call_value(State(..state, stack: rest), callee, args, JsUndefined)
        }
        [_, callee, ..] -> {
          state.throw_type_error(
            state,
            object.inspect(callee, state.heap) <> " is not a function",
          )
        }
        _ -> underflow(state, "CallApply")
      }
    }

    CallMethodApply -> {
      // [args_array, method, receiver] → [result]; this=receiver.
      case state.stack {
        [JsObject(args_ref), method, receiver, ..rest] -> {
          let args = extract_array_args(state.heap, args_ref)
          call_value(State(..state, stack: rest), method, args, receiver)
        }
        _ -> underflow(state, "CallMethodApply")
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
          state.throw_type_error(
            state,
            object.inspect(non_ctor, state.heap) <> " is not a constructor",
          )
        }
        _ -> underflow(state, "CallConstructorApply")
      }
    }

    MakeClosure(func_index) -> {
      // Compiler-generated index into the function table — always in bounds.
      let child_template =
        tuple_array.unsafe_get(func_index, state.func.functions)
      // Capture values from current frame according to env_descriptors.
      // For boxed captured vars, the local holds a JsObject(box_ref) —
      // copying that ref means the closure shares the same BoxSlot.
      let captured_values =
        list.map(child_template.env_descriptors, fn(desc) {
          case desc {
            value.CaptureLocal(parent_index) ->
              tuple_array.unsafe_get(parent_index, state.locals)
            value.CaptureEnv(_parent_env_index) ->
              // Transitive capture not yet implemented
              JsUndefined
          }
        })
      let #(heap, closure_ref) =
        make_closure(
          state.heap,
          state.builtins,
          child_template,
          captured_values,
        )
      // Arrows have no own [[HomeObject]]; they reference `super` through the
      // enclosing method's home object (§13.3.7), mirroring lexical `this`.
      let heap = case child_template.is_arrow, callee_home_object(state) {
        True, Some(home) -> set_home_object(heap, JsObject(closure_ref), home)
        _, _ -> heap
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

    // ---- Iteration ---------------------------------------------------
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
          // EnumerateObjectProperties checks each key via [[GetOwnProperty]], so
          // a TDZ namespace binding throws ReferenceError before iteration.
          let guard = case obj {
            JsObject(ref) -> object.namespace_tdz_guard(state, ref, keys)
            _ -> Ok(state)
          }
          use state <- result.try(state.rethrow(guard))
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
        _ -> underflow(state, "ForInStart")
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
                state,
              ))
          }
        _ -> underflow(state, "ForInNext")
      }
    }

    GetIterator -> {
      case state.stack {
        [iterable, ..rest] ->
          case iterable {
            JsObject(ref) ->
              case heap.read(state.heap, ref) {
                // Iterators are their own iterator — [Symbol.iterator]() on
                // %IteratorPrototype% returns `this`. Skip the proto walk.
                Some(ObjectSlot(kind: GeneratorObject(_), ..))
                | Some(ObjectSlot(kind: ArrayIteratorObject(..), ..))
                | Some(ObjectSlot(kind: value.SetIteratorObject(..), ..))
                | Some(ObjectSlot(kind: value.MapIteratorObject(..), ..)) ->
                  Ok(
                    State(
                      ..state,
                      stack: [JsObject(ref), ..rest],
                      pc: state.pc + 1,
                    ),
                  )
                // All other objects: look up Symbol.iterator per §7.4.1.
                // No array fast path — must honor deleted/overridden
                // Symbol.iterator (test262 destructuring tests rely on this).
                Some(ObjectSlot(..)) ->
                  get_iterator_via_symbol(state, ref, iterable, rest)
                _ ->
                  state.throw_type_error(
                    state,
                    object.inspect(iterable, state.heap) <> " is not iterable",
                  )
              }
            // String primitive: iterate UTF-16 code units
            JsString(_) ->
              case common.to_object(state.heap, state.builtins, iterable) {
                Some(#(h, wrapper_ref)) -> {
                  let #(h, iter_ref) =
                    alloc_array_iterator(h, state.builtins, wrapper_ref)
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
                  state.throw_type_error(
                    state,
                    object.inspect(iterable, state.heap) <> " is not iterable",
                  )
              }
            _ ->
              state.throw_type_error(
                state,
                object.inspect(iterable, state.heap) <> " is not iterable",
              )
          }
        _ -> underflow(state, "GetIterator")
      }
    }

    GetAsyncIterator -> {
      case state.stack {
        [iterable, ..rest] ->
          case iterable {
            JsObject(ref) ->
              get_async_iterator_via_symbol(state, ref, iterable, rest)
            _ ->
              state.throw_type_error(
                state,
                object.inspect(iterable, state.heap) <> " is not async iterable",
              )
          }
        _ -> underflow(state, "GetAsyncIterator")
      }
    }

    IteratorNext -> {
      // [[Done]] tracking (QuickJS-style): on done=true OR .next() abrupt,
      // the iter slot becomes JsUndefined so subsequent IteratorNext
      // short-circuits and IteratorClose/CloseThrow no-op (§7.4.11/.6).
      let mark_done = fn(rest) {
        fn(e: #(StepResult, JsValue, State)) {
          let #(r, v, s) = e
          #(r, v, State(..s, stack: [JsUndefined, ..rest]))
        }
      }
      case state.stack {
        // Iter already exhausted/aborted — short-circuit to done.
        [JsUndefined, ..rest] ->
          Ok(
            State(
              ..state,
              stack: [JsBool(True), JsUndefined, JsUndefined, ..rest],
              pc: state.pc + 1,
            ),
          )
        [JsObject(iter_ref), ..rest] ->
          case heap.read(state.heap, iter_ref) {
            Some(
              ObjectSlot(kind: ArrayIteratorObject(source:, index:), ..) as slot,
            ) -> {
              // Re-read the source length each time (handles mutations during iteration)
              let #(length, elements) =
                heap.read_array_like(state.heap, source)
                |> option.unwrap(#(0, elements.new()))
              case index >= length {
                True ->
                  Ok(
                    State(
                      ..state,
                      stack: [JsBool(True), JsUndefined, JsUndefined, ..rest],
                      pc: state.pc + 1,
                    ),
                  )
                False -> {
                  let val = elements.get(elements, index)
                  let heap =
                    heap.write(
                      state.heap,
                      iter_ref,
                      ObjectSlot(
                        ..slot,
                        kind: ArrayIteratorObject(source:, index: index + 1),
                      ),
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
              use next_state <- result.try(
                generators.call_native_generator_next(
                  state,
                  JsObject(iter_ref),
                  [],
                  [],
                  execute_inner,
                  unwind_to_catch,
                )
                |> result.map_error(mark_done(rest)),
              )
              // next_state.stack has [result_obj, ...], extract value and done
              case next_state.stack {
                [JsObject(result_ref), ..] ->
                  case heap.read(next_state.heap, result_ref) {
                    Some(ObjectSlot(properties: props, ..)) -> {
                      let val = case dict.get(props, Named("value")) {
                        Ok(DataProperty(value: v, ..)) -> v
                        _ -> JsUndefined
                      }
                      let done = case dict.get(props, Named("done")) {
                        Ok(DataProperty(value: JsBool(d), ..)) -> d
                        _ -> False
                      }
                      let iter_slot = case done {
                        True -> JsUndefined
                        False -> JsObject(iter_ref)
                      }
                      Ok(
                        State(
                          ..state.merge_globals(state, next_state, []),
                          heap: next_state.heap,
                          stack: [JsBool(done), val, iter_slot, ..rest],
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
                        next_state,
                      ))
                  }
                _ ->
                  Error(#(
                    StepVmError(Unimplemented(
                      "IteratorNext: generator .next() empty stack",
                    )),
                    JsUndefined,
                    next_state,
                  ))
              }
            }
            // Generic iterator: any object with .next(). Call it, extract {value, done}.
            Some(ObjectSlot(..)) ->
              step_generic_iterator(state, iter_ref, rest)
              |> result.map_error(mark_done(rest))
            _ ->
              state.throw_type_error(
                state,
                object.inspect(JsObject(iter_ref), state.heap)
                  <> " is not an iterator",
              )
          }
        _ -> underflow(state, "IteratorNext")
      }
    }

    IteratorClose -> {
      // §7.4.11 normal-completion close. Stack: [iter, ..rest] → [..rest].
      // iter slot is JsUndefined when [[Done]] (set by IteratorNext) → no-op.
      case state.stack {
        [JsObject(_) as iter, ..rest] -> {
          let state = State(..state, stack: rest, pc: state.pc + 1)
          case builtins_iterator.iterator_close_normal(state, iter) {
            #(state, Ok(Nil)) -> Ok(state)
            #(state, Error(thrown)) -> Error(#(Thrown, thrown, state))
          }
        }
        [_, ..rest] -> Ok(State(..state, stack: rest, pc: state.pc + 1))
        [] -> underflow(state, "IteratorClose")
      }
    }

    IteratorCloseThrow -> {
      // §7.4.11 throw-completion close. Stack: [thrown, iter, ..] → rethrows.
      // unwind_to_catch truncated to PushTry-time depth (iter on top) then
      // pushed thrown, so layout is guaranteed. iter slot is JsUndefined when
      // [[Done]] (set by IteratorNext on done/abrupt) → skip .return().
      case state.stack {
        [thrown, JsObject(_) as iter, ..rest] -> {
          let state = State(..state, stack: rest)
          // Original error wins regardless of what .return() does.
          let #(state, _inner) = builtins_iterator.call_return(state, iter)
          Error(#(Thrown, thrown, state))
        }
        [thrown, _, ..rest] ->
          Error(#(Thrown, thrown, State(..state, stack: rest)))
        _ -> underflow(state, "IteratorCloseThrow")
      }
    }

    IteratorRest -> {
      // §13.15.5.3 / §14.3.3 rest element. [iter, ..rest] → [arr, ..rest].
      // Drains via .next() loop without re-GetIterator. Emitter pops the
      // close-guard try frame before this op so a .next() throw propagates
      // without IteratorClose (spec: [[Done]]=true → no close on rest abrupt).
      case state.stack {
        [JsObject(_) as iter, ..rest] -> {
          let state = State(..state, stack: rest, pc: state.pc + 1)
          case builtins_iterator.iterator_rest(state, iter) {
            #(state, Ok(arr)) -> Ok(State(..state, stack: [arr, ..rest]))
            #(state, Error(thrown)) -> Error(#(Thrown, thrown, state))
          }
        }
        // [[Done]] sentinel — a prior IteratorNext exhausted/aborted the
        // iterator and undef'd the slot. Rest result is an empty array.
        [_, ..rest] -> {
          let #(heap, ref) =
            common.alloc_array(state.heap, [], state.builtins.array.prototype)
          Ok(
            State(
              ..state,
              heap:,
              stack: [JsObject(ref), ..rest],
              pc: state.pc + 1,
            ),
          )
        }
        [] -> underflow(state, "IteratorRest")
      }
    }

    IteratorCheckObject ->
      // §7.4.12 step 6 / §14.7.5.6 step 6.c: awaited iterator result must be
      // an Object. Peeks; leaves the value on the stack on success.
      case state.stack {
        [JsObject(_), ..] -> Ok(State(..state, pc: state.pc + 1))
        [_, ..] ->
          state.throw_type_error(state, "Iterator result is not an object")
        [] -> underflow(state, "IteratorCheckObject")
      }

    // ---- Generator/async ---------------------------------------------
    InitialYield ->
      // Suspend immediately at start of generator body.
      // PC advances past InitialYield so resumption starts at the next op.
      Error(#(Yielded, JsUndefined, state))

    Yield -> {
      // Pop value from stack and suspend the generator.
      // On resume, .next(arg) value will be pushed onto the stack.
      case state.stack {
        [yielded_value, ..] -> Error(#(Yielded, yielded_value, state))
        [] -> Error(#(Yielded, JsUndefined, state))
      }
    }

    YieldStar -> {
      // Self-looping delegate: [arg, iter, ..rest]. Calls iter.next(arg).
      // done → push value, pc+1. !done → yield value; execute_inner keeps pc
      // here so next resume re-enters with [resume_val, iter].
      case state.stack {
        [arg, JsObject(iter_ref) as iter, ..rest] -> {
          use #(next_fn, state) <- result.try(
            state.rethrow(object.get_value(state, iter_ref, Named("next"), iter)),
          )
          use #(res, state) <- result.try(
            state.rethrow(state.call(state, next_fn, iter, [arg])),
          )
          use #(done, val, state) <- result.try(read_iter_result(state, res))
          case done {
            True -> Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
            False ->
              // execute_inner's YieldStar arm strips arg from the
              // original stack and keeps pc here, so resume loops back
              // with [resume_val, iter, ..rest].
              Error(#(Yielded, val, state))
          }
        }
        _ -> underflow(state, "YieldStar")
      }
    }

    AsyncYieldStarNext ->
      // [arg, iter, ..rest]. Call iter.next(arg), replace arg with result →
      // [result, iter, ..rest], pc+1. The following Await op suspends on it.
      case state.stack {
        [arg, JsObject(iter_ref) as iter, ..rest] -> {
          use #(next_fn, state) <- result.try(
            state.rethrow(object.get_value(state, iter_ref, Named("next"), iter)),
          )
          use #(res, state) <- result.try(
            state.rethrow(state.call(state, next_fn, iter, [arg])),
          )
          Ok(State(..state, stack: [res, iter, ..rest], pc: state.pc + 1))
        }
        _ -> underflow(state, "AsyncYieldStarNext")
      }

    AsyncYieldStarResume(_) ->
      // [result_obj, iter, ..rest]. done? → push value, pc+1 : Yielded(value).
      case state.stack {
        [res, _iter, ..rest] -> {
          use #(done, val, state) <- result.try(read_iter_result(state, res))
          case done {
            True -> Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
            False -> Error(#(Yielded, val, state))
          }
        }
        _ -> underflow(state, "AsyncYieldStarResume")
      }

    Await -> {
      // Pop the awaited value from the stack and suspend the async function.
      case state.stack {
        [awaited_value, ..] -> Error(#(Awaited, awaited_value, state))
        [] -> Error(#(Awaited, JsUndefined, state))
      }
    }

    // ---- Special -----------------------------------------------------
    CreateArguments -> {
      // Allocate an unmapped arguments object from state.call_args.
      let args = state.call_args
      let length = list.length(args)
      let callee =
        state.callee_ref |> option.map(JsObject) |> option.unwrap(JsUndefined)
      let props =
        dict.from_list([
          #(
            Named("length"),
            value.data(value.from_int(length))
              |> value.writable
              |> value.configurable,
          ),
          #(
            Named("callee"),
            value.data(callee) |> value.writable |> value.configurable,
          ),
        ])
      // §10.4.4.6: [@@iterator] = %Array.prototype.values%
      let sym_props = case
        heap.read(state.heap, state.builtins.array.prototype)
      {
        Some(ObjectSlot(symbol_properties: arr_syms, ..)) ->
          case list.key_find(arr_syms, value.symbol_iterator) {
            Ok(values_fn) -> [#(value.symbol_iterator, values_fn)]
            Error(Nil) -> []
          }
        _ -> []
      }
      let #(heap, ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: value.ArgumentsObject(length:),
            properties: props,
            elements: elements.from_list(args),
            prototype: Some(state.builtins.object.prototype),
            symbol_properties: sym_props,
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

    CreateRestArray(from_index) -> {
      // §10.4.4 rest parameter: a plain Array of the call args from
      // `from_index` onward (the params before the rest are bound positionally).
      let rest_args = list.drop(state.call_args, from_index)
      let #(heap, ref) =
        common.alloc_array(state.heap, rest_args, state.builtins.array.prototype)
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
        _ -> underflow(state, "NewRegExp")
      }
    }
  }
}

fn lookup_eval_env(state: State, name: String) -> Option(JsValue) {
  option.then(state.eval_env, heap.read_eval_env(state.heap, _))
  |> option.then(fn(vars) { dict.get(vars, name) |> option.from_result })
}

/// GetElem on a primitive receiver — ToPropertyKey (Symbol → symbol lookup
/// on prototype, else ToString → string lookup) then delegate to get_value_of.
fn get_elem_on_primitive(
  state: State,
  receiver: JsValue,
  key: JsValue,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  case key {
    value.JsSymbol(sym) -> object.get_symbol_value_of(state, receiver, sym)
    _ -> {
      use #(pk, state) <- result.try(property.to_property_key(state, key))
      object.get_value_of(state, receiver, pk)
    }
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
/// [[ThisMode]] = LEXICAL, capturing the enclosing frame's `this` slot.
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
) -> Result(State, #(StepResult, JsValue, State)) {
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
  native: NativeFnSlot,
  args: List(JsValue),
  rest_stack: List(JsValue),
  this: JsValue,
) -> Result(State, #(StepResult, JsValue, State)) {
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

/// ES2024 §13.3.7.1 SuperCall — `super(args)` inside a derived constructor.
///
/// Find the parent constructor via callee_ref.[[Prototype]], invoke it with
/// the supplied args, and bind the result as `this`. Shared between CallSuper
/// (fixed arity, args on stack) and CallSuperApply (spread, args from array).
fn do_call_super(
  state: State,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, State)) {
  case state.callee_ref {
    None ->
      state.throw_reference_error(state, "'super' keyword unexpected here")
    Some(my_ctor_ref) ->
      case heap.read(state.heap, my_ctor_ref) {
        Some(ObjectSlot(prototype: Some(parent_ref), properties: my_props, ..)) -> {
          // Compute newTarget.prototype — the [[Prototype]] for the new
          // instance. First super() in a chain reads my_ctor.prototype;
          // intermediate super() reuses the existing this's proto (which is
          // already the leaf subclass's prototype).
          let derived_proto = case read_this_local(state) {
            JsUninitialized ->
              case dict.get(my_props, Named("prototype")) {
                Ok(DataProperty(value: JsObject(dp_ref), ..)) -> Some(dp_ref)
                _ -> Some(state.builtins.object.prototype)
              }
            JsObject(existing_ref) ->
              case heap.read(state.heap, existing_ref) {
                Some(ObjectSlot(prototype: p, ..)) -> p
                _ -> Some(state.builtins.object.prototype)
              }
            _ -> Some(state.builtins.object.prototype)
          }
          do_call_super_dispatch(
            state,
            parent_ref,
            derived_proto,
            args,
            rest_stack,
          )
        }
        _ ->
          state.throw_type_error(
            state,
            "Super constructor is not a constructor",
          )
      }
  }
}

/// SuperCall step 5: Construct(func, argList, newTarget). Dispatches on the
/// resolved parent constructor kind. Split out so a bound-function parent can
/// recurse on its [[BoundTargetFunction]] with prepended args.
fn do_call_super_dispatch(
  state: State,
  parent_ref: Ref,
  derived_proto: Option(Ref),
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, State)) {
  case heap.read(state.heap, parent_ref) {
    Some(ObjectSlot(kind: FunctionObject(func_template:, env: env_ref, ..), ..)) -> {
      // JS-defined parent: allocate (or reuse) an OrdinaryObject as `this` and
      // enter the parent body. parent_ref becomes the new callee_ref so further
      // super() in the chain finds *its* parent.
      let #(heap, this_val) = case read_this_local(state) {
        JsUninitialized -> {
          let #(h, ref) =
            heap.alloc(
              state.heap,
              ObjectSlot(
                kind: OrdinaryObject,
                properties: dict.new(),
                elements: elements.new(),
                prototype: derived_proto,
                symbol_properties: [],
                extensible: True,
              ),
            )
          #(h, JsObject(ref))
        }
        existing -> #(state.heap, existing)
      }
      call_function(
        State(..state, heap:),
        parent_ref,
        env_ref,
        func_template,
        args,
        rest_stack,
        this_val,
        Some(this_val),
        Some(parent_ref),
      )
    }
    // Bound function as superclass (§10.4.1.2): construct the
    // [[BoundTargetFunction]] with [[BoundArguments]] prepended; bound `this`
    // is ignored under [[Construct]].
    Some(ObjectSlot(
      kind: NativeFunction(
        value.Call(value.BoundFunction(target:, bound_args:, ..)),
        ..,
      ),
      ..,
    )) ->
      do_call_super_dispatch(
        state,
        target,
        derived_proto,
        list.append(bound_args, args),
        rest_stack,
      )
    // §20.4.1: Symbol has no [[Construct]]; super() into Symbol must throw.
    Some(ObjectSlot(
      kind: NativeFunction(value.Call(value.SymbolConstructor), ..),
      ..,
    )) -> state.throw_type_error(state, "Symbol is not a constructor")
    Some(ObjectSlot(kind: NativeFunction(native, ..), ..)) -> {
      // Built-in parent (Array, Map, Error, …): per spec the result of
      // Construct(func, args, newTarget) becomes `this`. Arc's native ctors
      // don't thread newTarget, so pre-allocate the derived instance and pass
      // it as `this`. Natives that return `this` (abstract Iterator) keep it;
      // natives that allocate their own exotic object (Map, Array, …) ignore
      // it and we re-prototype the result below.
      let #(heap, this_ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: OrdinaryObject,
            properties: dict.new(),
            elements: elements.new(),
            prototype: derived_proto,
            symbol_properties: [],
            extensible: True,
          ),
        )
      let this_obj = JsObject(this_ref)
      use new_state <- result.try(call_native(
        State(..state, heap:),
        native,
        args,
        rest_stack,
        this_obj,
      ))
      case new_state.stack {
        [JsObject(result_ref), ..] -> {
          let heap =
            set_slot_prototype(new_state.heap, result_ref, derived_proto)
          Ok(State(..new_state, heap:))
        }
        [_, ..tail] -> Ok(State(..new_state, stack: [this_obj, ..tail]))
        [] -> underflow(new_state, "CallSuper: native returned nothing")
      }
    }
    _ -> state.throw_type_error(state, "Super constructor is not a constructor")
  }
}

/// Thin wrapper: delegates to call.do_construct with execute_inner/unwind_to_catch/dispatch_native.
fn do_construct(
  state: State,
  ctor_ref: Ref,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, State)) {
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
) -> Result(State, #(StepResult, JsValue, State)) {
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

/// The [[HomeObject]] of the running function, if any. Arrows carry the home
/// object copied from their enclosing method (set at MakeClosure), so `super`
/// works inside an arrow nested in a method.
fn callee_home_object(state: State) -> Option(Ref) {
  case state.callee_ref {
    Some(fn_ref) ->
      case heap.read(state.heap, fn_ref) {
        Some(ObjectSlot(kind: FunctionObject(home_object: home, ..), ..)) -> home
        _ -> None
      }
    None -> None
  }
}

/// §13.3.7.3 GetSuperBase: the [[Prototype]] of the running method's
/// [[HomeObject]]. None when there is no home object (plain function — `super`
/// is then a syntax error and never reached) or the home's prototype is null.
fn super_base_ref(state: State) -> Option(Ref) {
  case callee_home_object(state) {
    Some(home) ->
      case heap.read(state.heap, home) {
        Some(ObjectSlot(prototype: proto, ..)) -> proto
        _ -> None
      }
    None -> None
  }
}

/// Read `super.<key>` and hand the value + threaded state to `build`. The
/// lookup walks from the super base but uses the current `this` as the [[Get]]
/// receiver (so inherited getters see the right `this`). A null base yields
/// undefined.
fn get_super_value(
  state: State,
  key: value.PropertyKey,
  build: fn(JsValue, State) -> State,
) -> Result(State, #(StepResult, JsValue, State)) {
  let this_val = read_this_local(state)
  case super_base_ref(state) {
    Some(base) -> {
      use #(val, state) <- result.map(
        state.rethrow(object.get_value(state, base, key, this_val)),
      )
      build(val, state)
    }
    None -> Ok(build(JsUndefined, state))
  }
}

/// Write `super.<key> = val`. [[Set]] starts at the super base but the receiver
/// is the current `this`, so own data lands on `this` while parent setters
/// still fire (§13.3.7). Leaves `val` on the stack (assignment is an expr).
fn put_super_value(
  state: State,
  key: value.PropertyKey,
  val: JsValue,
  rest: List(JsValue),
) -> Result(State, #(StepResult, JsValue, State)) {
  let this_val = read_this_local(state)
  case super_base_ref(state) {
    Some(base) -> {
      use #(state, _ok) <- result.map(
        state.rethrow(object.set_value(state, base, key, val, this_val)),
      )
      State(..state, stack: [val, ..rest], pc: state.pc + 1)
    }
    None ->
      state.throw_type_error(
        state,
        "Cannot set super property of null prototype",
      )
  }
}

/// Record the [[HomeObject]] of a method/accessor as it is installed, so that
/// `super` inside its body resolves against `home`'s [[Prototype]] (§13.3.7).
/// `home` is the class prototype for instance members, the constructor for
/// statics, or the object for object-literal methods. No-op for non-functions.
fn set_home_object(h: Heap, fn_val: JsValue, home: Ref) -> Heap {
  case fn_val {
    JsObject(fn_ref) ->
      case heap.read(h, fn_ref) {
        Some(ObjectSlot(kind: FunctionObject(func_template:, env:, ..), ..)) ->
          heap.update_kind(
            h,
            fn_ref,
            FunctionObject(func_template:, env:, home_object: Some(home)),
          )
        _ -> h
      }
    _ -> h
  }
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
) -> Result(State, #(StepResult, JsValue, State)) {
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
  call.dispatch_native(native, args, this, state, execute_inner, new_state)
}

/// Get the Ref of a named property's JsObject value from a heap object.
/// Returns Error(Nil) if the object doesn't exist, the property is missing,
/// or the property value is not a JsObject.
fn get_field_ref(
  h: Heap,
  obj_ref: value.Ref,
  name: String,
) -> Option(value.Ref) {
  use slot <- option.then(heap.read(h, obj_ref))
  case slot {
    ObjectSlot(properties: props, ..) ->
      case dict.get(props, Named(name)) {
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

/// Partition raw JsValue keys (from ObjectRestCopy stack) into PropertyKey
/// (string/index) and SymbolId sets for CopyDataProperties exclusion.
/// Non-symbol keys go through ToPropertyKey (§7.1.19) for canonical form.
fn build_exclusion_sets(
  state: State,
  keys: List(JsValue),
) -> Result(
  #(set.Set(value.PropertyKey), set.Set(value.SymbolId), State),
  #(JsValue, State),
) {
  use #(pks, syms, state), key <- list.try_fold(keys, #(
    set.new(),
    set.new(),
    state,
  ))
  case key {
    value.JsSymbol(id) -> Ok(#(pks, set.insert(syms, id), state))
    _ -> {
      use #(pk, state) <- result.map(property.to_property_key(state, key))
      #(set.insert(pks, pk), syms, state)
    }
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
fn binop_direct(
  state: State,
  kind: opcode.BinOpKind,
  left: JsValue,
  right: JsValue,
  rest: List(JsValue),
) -> Result(State, #(StepResult, JsValue, State)) {
  case operators.exec_binop(kind, left, right) {
    Ok(result) -> Ok(State(..state, stack: [result, ..rest], pc: state.pc + 1))
    Error(msg) -> state.throw_type_error(state, msg)
  }
}

/// §7.2.14 IsLooselyEqual: ToPrimitive fires only for object × {Number,
/// String, BigInt, Symbol, Bool}. Object×object and object×nullish go
/// straight to abstract_equal.
fn is_eq_coercible(left: JsValue, right: JsValue) -> Bool {
  case left, right {
    JsObject(_), JsObject(_) -> False
    JsObject(_), JsNull | JsObject(_), JsUndefined -> False
    JsNull, JsObject(_) | JsUndefined, JsObject(_) -> False
    JsObject(_), _ | _, JsObject(_) -> True
    _, _ -> False
  }
}

/// ES2024 §13.15.4 / §7.2.14: ToPrimitive both operands before delegating
/// to the pure operator. The fast path already short-circuits on
/// primitive×primitive. Relational/numeric ops use number hint; loose
/// equality uses default hint (matters for Date @@toPrimitive).
fn binop_with_to_primitive(
  state: State,
  kind: opcode.BinOpKind,
  left: JsValue,
  right: JsValue,
  rest: List(JsValue),
) -> Result(State, #(StepResult, JsValue, State)) {
  let hint = case kind {
    opcode.Eq | opcode.NotEq -> coerce.DefaultHint
    _ -> coerce.NumberHint
  }
  use #(lprim, s1) <- result.try(
    state.rethrow(coerce.to_primitive(state, left, hint)),
  )
  use #(rprim, s2) <- result.try(
    state.rethrow(coerce.to_primitive(s1, right, hint)),
  )
  case operators.exec_binop(kind, lprim, rprim) {
    Ok(result) -> Ok(State(..s2, stack: [result, ..rest], pc: state.pc + 1))
    Error(msg) -> state.throw_type_error(s2, msg)
  }
}

/// ES2024 §13.5.4/5/6: numeric unary ops call ToNumber → ToPrimitive on
/// object operands. LogicalNot/Void are handled in the fast path (they do
/// not coerce).
fn unaryop_with_to_primitive(
  state: State,
  kind: opcode.UnaryOpKind,
  operand: JsValue,
  rest: List(JsValue),
) -> Result(State, #(StepResult, JsValue, State)) {
  use #(prim, s1) <- result.try(
    state.rethrow(coerce.to_primitive(state, operand, coerce.NumberHint)),
  )
  case operators.exec_unaryop(kind, prim) {
    Ok(result) -> Ok(State(..s1, stack: [result, ..rest], pc: state.pc + 1))
    Error(msg) -> state.throw_type_error(s1, msg)
  }
}

/// ES2024 §13.15.4 step 2–7: apply `+` to two already-primitive operands.
fn add_primitives(
  state: State,
  lprim: JsValue,
  rprim: JsValue,
  rest: List(JsValue),
) -> Result(State, #(StepResult, JsValue, State)) {
  case lprim, rprim {
    JsString(a), JsString(b) ->
      Ok(State(..state, stack: [JsString(a <> b), ..rest], pc: state.pc + 1))
    JsString(a), _ -> {
      use #(b, state) <- result.map(
        state.rethrow(coerce.js_to_string(state, rprim)),
      )
      State(..state, stack: [JsString(a <> b), ..rest], pc: state.pc + 1)
    }
    _, JsString(b) -> {
      use #(a, state) <- result.map(
        state.rethrow(coerce.js_to_string(state, lprim)),
      )
      State(..state, stack: [JsString(a <> b), ..rest], pc: state.pc + 1)
    }
    _, _ ->
      case operators.num_binop(lprim, rprim, operators.num_add) {
        Ok(result) ->
          Ok(State(..state, stack: [result, ..rest], pc: state.pc + 1))
        Error(msg) -> state.throw_type_error(state, msg)
      }
  }
}

fn binop_add_with_to_primitive(
  state: State,
  left: JsValue,
  right: JsValue,
  rest: List(JsValue),
) -> Result(State, #(StepResult, JsValue, State)) {
  use #(lprim, s1) <- result.try(
    state.rethrow(coerce.to_primitive(state, left, coerce.DefaultHint)),
  )
  use #(rprim, s2) <- result.try(
    state.rethrow(coerce.to_primitive(s1, right, coerce.DefaultHint)),
  )
  add_primitives(s2, lprim, rprim, rest)
}

fn alloc_array_iterator(
  h: Heap,
  builtins: common.Builtins,
  source: value.Ref,
) -> #(Heap, value.Ref) {
  common.alloc_wrapper(
    h,
    ArrayIteratorObject(source:, index: 0),
    builtins.array_iterator_proto,
  )
}

/// ES IteratorComplete + IteratorValue: read {done, value} from an iterator
/// result; TypeError if not an object.
fn read_iter_result(
  state: State,
  res: JsValue,
) -> Result(#(Bool, JsValue, State), #(StepResult, JsValue, State)) {
  case res {
    JsObject(rref) -> {
      use #(done, state) <- result.try(
        state.rethrow(object.get_value(state, rref, Named("done"), res)),
      )
      use #(val, state) <- result.map(
        state.rethrow(object.get_value(state, rref, Named("value"), res)),
      )
      #(value.is_truthy(done), val, state)
    }
    _ -> state.throw_type_error(state, "Iterator result is not an object")
  }
}

/// IteratorNext fallback for user-defined iterators: call .next(), extract
/// {value, done}, push [done, value, iter] onto stack. On done=true the iter
/// slot becomes JsUndefined ([[Done]] tracking — see IteratorNext); abrupt
/// completion is undef'd by the caller's mark_done wrapper.
fn step_generic_iterator(
  state: State,
  iter_ref: Ref,
  rest: List(JsValue),
) -> Result(State, #(StepResult, JsValue, State)) {
  let iter = JsObject(iter_ref)
  use #(next_fn, state) <- result.try(
    state.rethrow(object.get_value(state, iter_ref, Named("next"), iter)),
  )
  use #(result_obj, state) <- result.try(
    state.rethrow(state.call(state, next_fn, iter, [])),
  )
  use #(done, val, state) <- result.map(read_iter_result(state, result_obj))
  let iter_slot = case done {
    True -> JsUndefined
    False -> iter
  }
  State(
    ..state,
    stack: [JsBool(done), val, iter_slot, ..rest],
    pc: state.pc + 1,
  )
}

/// ES2024 §7.4.1 GetIterator(obj, kind) — look up Symbol.iterator and call it.
/// Used when the fast path (ArrayObject without overridden Symbol.iterator) doesn't apply.
fn get_iterator_via_symbol(
  state: State,
  ref: value.Ref,
  iterable: JsValue,
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, State)) {
  // Step 1: Let method be ? GetMethod(obj, @@iterator)
  case object.get_symbol_value(state, ref, value.symbol_iterator, iterable) {
    Ok(#(method, state)) ->
      case helpers.is_callable(state.heap, method) {
        True ->
          // Step 2: Let iterator be ? Call(method, obj)
          case state.call(state, method, iterable, []) {
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
                  state.throw_type_error(
                    state,
                    "Iterator result is not an object",
                  )
              }
            Error(#(thrown, state)) -> Error(#(Thrown, thrown, state))
          }
        False ->
          state.throw_type_error(
            state,
            object.inspect(iterable, state.heap) <> " is not iterable",
          )
      }
    Error(#(_thrown, state)) ->
      state.throw_type_error(
        state,
        object.inspect(iterable, state.heap) <> " is not iterable",
      )
  }
}

/// ES §7.4.3 GetIterator(obj, async). Tries Symbol.asyncIterator, falls back
/// to Symbol.iterator wrapped via CreateAsyncFromSyncIterator (§27.1.6.1).
fn get_async_iterator_via_symbol(
  state: State,
  ref: Ref,
  iterable: JsValue,
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, State)) {
  case try_iterator_symbol(state, ref, iterable, value.symbol_async_iterator) {
    Ok(#(iter, state)) ->
      Ok(State(..state, stack: [iter, ..rest_stack], pc: state.pc + 1))
    Error(state) ->
      case try_iterator_symbol(state, ref, iterable, value.symbol_iterator) {
        Ok(#(JsObject(sync_iter), state)) -> {
          let #(h, wrapped) =
            common.alloc_wrapper(
              state.heap,
              value.AsyncFromSyncIteratorObject(sync_iter:),
              state.builtins.async_from_sync_iterator_proto,
            )
          Ok(
            State(
              ..state,
              heap: h,
              stack: [JsObject(wrapped), ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        }
        Ok(#(_, state)) | Error(state) ->
          state.throw_type_error(
            state,
            object.inspect(iterable, state.heap) <> " is not async iterable",
          )
      }
  }
}

fn try_iterator_symbol(
  state: State,
  ref: Ref,
  iterable: JsValue,
  sym: value.SymbolId,
) -> Result(#(JsValue, State), State) {
  case object.get_symbol_value(state, ref, sym, iterable) {
    Ok(#(method, state)) ->
      case helpers.is_callable(state.heap, method) {
        True ->
          case state.call(state, method, iterable, []) {
            Ok(#(JsObject(r), state)) -> Ok(#(JsObject(r), state))
            Ok(#(_, state)) -> Error(state)
            Error(#(_, state)) -> Error(state)
          }
        False -> Error(state)
      }
    Error(#(_, state)) -> Error(state)
  }
}
