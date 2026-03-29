import arc/vm/builtins/arc as builtins_arc
import arc/vm/builtins/array as builtins_array
import arc/vm/builtins/boolean as builtins_boolean
import arc/vm/builtins/common
import arc/vm/builtins/error as builtins_error
import arc/vm/builtins/json as builtins_json
import arc/vm/builtins/map as builtins_map
import arc/vm/builtins/math as builtins_math
import arc/vm/builtins/number as builtins_number
import arc/vm/builtins/object as builtins_object
import arc/vm/builtins/promise as builtins_promise
import arc/vm/builtins/reflect as builtins_reflect
import arc/vm/builtins/regexp as builtins_regexp
import arc/vm/builtins/set as builtins_set
import arc/vm/builtins/string as builtins_string
import arc/vm/builtins/symbol as builtins_symbol
import arc/vm/builtins/weak_map as builtins_weak_map
import arc/vm/builtins/weak_set as builtins_weak_set
import arc/vm/completion.{
  type Completion, AwaitCompletion, NormalCompletion, ThrowCompletion,
  YieldCompletion,
}
import arc/vm/exec/async_generators
import arc/vm/exec/generators
import arc/vm/exec/promises
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/internal/tuple_array
import arc/vm/limits
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/ops/operators
import arc/vm/realm
import arc/vm/state.{
  type Heap, type NativeFnSlot, type State, type StepResult, type VmError,
  SavedFrame, State, StepVmError, Thrown, Unimplemented,
}
import arc/vm/value.{
  type FuncTemplate, type JsValue, type Ref, AsyncFunctionSlot,
  AsyncGeneratorObject, AsyncGeneratorSlot, DataProperty, FunctionObject,
  GeneratorObject, GeneratorSlot, JsNull, JsObject, JsString, JsUndefined,
  JsUninitialized, Named, NativeFunction, ObjectSlot, OrdinaryObject,
}
import gleam/bool
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

// ============================================================================
// Callback types for VM functions that can't be imported directly
// ============================================================================

pub type ExecuteInnerFn =
  fn(State) -> Result(#(Completion, State), VmError)

pub type UnwindToCatchFn =
  fn(State, JsValue) -> Option(State)

pub type DispatchNativeFn =
  fn(value.NativeFn, List(JsValue), JsValue, State) ->
    #(State, Result(JsValue, JsValue))

// ============================================================================
// Function calling infrastructure
// ============================================================================

/// Resolve `this` for a function call per ES2024 §10.2.1.2 OrdinaryCallBindThis.
pub fn bind_this(
  state: State,
  callee: FuncTemplate,
  this_arg: JsValue,
) -> #(Heap, JsValue) {
  case callee.is_arrow {
    // Step 2: thisMode is LEXICAL -> return caller's this_binding unchanged.
    True -> #(state.heap, state.this_binding)
    False ->
      case callee.is_strict {
        // Step 5: thisMode is STRICT -> thisValue = thisArgument (no coercion).
        True -> #(state.heap, this_arg)
        // Step 6: Sloppy mode coercion.
        False ->
          case this_arg {
            // Step 6a: undefined/null -> globalThis.
            JsUndefined | JsNull -> #(state.heap, JsObject(state.global_object))
            // Step 6b: Objects pass through (ToObject is identity for objects).
            JsObject(_) -> #(state.heap, this_arg)
            _ ->
              // Step 6b: Primitives -> ToObject wrapper (boxing).
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
pub fn call_function(
  state: State,
  fn_ref: value.Ref,
  env_ref: value.Ref,
  callee_template: FuncTemplate,
  args: List(JsValue),
  rest_stack: List(JsValue),
  this_val: JsValue,
  constructor_this: option.Option(JsValue),
  new_callee_ref: option.Option(Ref),
  execute_inner: ExecuteInnerFn,
  unwind_to_catch: UnwindToCatchFn,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let #(heap, this_val) = bind_this(state, callee_template, this_val)
  let state = State(..state, heap:)
  case callee_template.is_generator, callee_template.is_async {
    True, True ->
      call_async_generator_function(
        state,
        fn_ref,
        env_ref,
        callee_template,
        args,
        rest_stack,
        this_val,
        execute_inner,
      )
    True, False ->
      call_generator_function(
        state,
        fn_ref,
        env_ref,
        callee_template,
        args,
        rest_stack,
        this_val,
        execute_inner,
      )
    False, True ->
      call_async_function(
        state,
        fn_ref,
        env_ref,
        callee_template,
        args,
        rest_stack,
        this_val,
        execute_inner,
        unwind_to_catch,
      )
    False, False ->
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
  use <- bool.lazy_guard(state.call_depth >= limits.max_call_depth, fn() {
    state.throw_range_error(state, "Maximum call stack size exceeded")
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
      eval_env: state.eval_env,
    )
  let locals = setup_locals(state.heap, env_ref, callee_template, args)
  // Arrow functions inherit this from their enclosing scope
  let new_this = case callee_template.is_arrow {
    True -> state.this_binding
    False -> this_val
  }
  // For arguments.callee: constructors already pass new_callee_ref=Some(ctor_ref),
  // regular calls pass None -- fall back to fn_ref so arguments.callee works.
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
      eval_env: None,
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
  execute_inner: ExecuteInnerFn,
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
            elements: elements.new(),
            prototype: Some(state.builtins.generator.prototype),
            symbol_properties: [],
            extensible: True,
          ),
        )
      // Return to caller with the generator object on the stack
      Ok(
        State(
          ..state.merge_globals(state, suspended, []),
          heap: h,
          stack: [JsObject(gen_obj_ref), ..rest_stack],
          pc: state.pc + 1,
        ),
      )
    }
    Ok(#(NormalCompletion(_, h), _)) -> {
      // Generator returned without yielding -- shouldn't happen with InitialYield
      // but handle gracefully: create a completed generator
      let #(h, data_ref) =
        heap.alloc(
          h,
          GeneratorSlot(
            gen_state: value.Completed,
            func_template: callee_template,
            env_ref:,
            saved_pc: 0,
            saved_locals: tuple_array.from_list([]),
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
            elements: elements.new(),
            prototype: Some(state.builtins.generator.prototype),
            symbol_properties: [],
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
    Ok(#(AwaitCompletion(_, _), _)) ->
      Error(#(
        StepVmError(Unimplemented("await in sync generator")),
        JsUndefined,
        state.heap,
      ))
    Error(vm_err) -> Error(#(StepVmError(vm_err), JsUndefined, state.heap))
  }
}

/// Async generator call: run to InitialYield, allocate AsyncGeneratorSlot with
/// empty request queue, return AsyncGeneratorObject. Body doesn't actually
/// execute until the first .next() — that's when the driver loop kicks in.
fn call_async_generator_function(
  state: State,
  fn_ref: value.Ref,
  env_ref: value.Ref,
  callee_template: FuncTemplate,
  args: List(JsValue),
  rest_stack: List(JsValue),
  this_val: JsValue,
  execute_inner: ExecuteInnerFn,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let locals = setup_locals(state.heap, env_ref, callee_template, args)
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
  case execute_inner(gen_state) {
    Ok(#(YieldCompletion(_, _), suspended)) -> {
      let #(saved_try, saved_finally) =
        generators.save_stacks(suspended.try_stack, suspended.finally_stack)
      let #(h, data_ref) =
        heap.alloc(
          suspended.heap,
          AsyncGeneratorSlot(
            gen_state: value.AGSuspendedStart,
            queue: [],
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
      let #(h, gen_obj_ref) =
        heap.alloc(
          h,
          ObjectSlot(
            kind: AsyncGeneratorObject(generator_data: data_ref),
            properties: dict.new(),
            elements: elements.new(),
            prototype: Some(state.builtins.async_generator.prototype),
            symbol_properties: [],
            extensible: True,
          ),
        )
      Ok(
        State(
          ..state.merge_globals(state, suspended, []),
          heap: h,
          stack: [JsObject(gen_obj_ref), ..rest_stack],
          pc: state.pc + 1,
        ),
      )
    }
    Ok(#(ThrowCompletion(thrown, h), _)) -> Error(#(Thrown, thrown, h))
    Ok(#(NormalCompletion(_, _), _)) | Ok(#(AwaitCompletion(_, _), _)) ->
      // InitialYield is first op — body never runs before it. Unreachable.
      Error(#(
        StepVmError(Unimplemented("async generator didn't hit InitialYield")),
        JsUndefined,
        state.heap,
      ))
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
  execute_inner: ExecuteInnerFn,
  _unwind_to_catch: UnwindToCatchFn,
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
    Ok(#(AwaitCompletion(awaited_value, h2), suspended)) -> {
      // Body hit `await` -- save state, set up promise resolution
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
          State(..state.merge_globals(state, suspended, []), heap: h2),
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
      // Async function completed without awaiting -- resolve the promise
      let #(h2, jobs) =
        builtins_promise.fulfill_promise(h2, data_ref, return_value)
      Ok(
        State(
          ..state.merge_globals(state, final_state, jobs),
          heap: h2,
          stack: [JsObject(promise_ref), ..rest_stack],
          pc: state.pc + 1,
        ),
      )
    }
    Ok(#(ThrowCompletion(thrown, h2), final_state)) -> {
      // Async function threw without awaiting -- reject the promise
      let state =
        builtins_promise.reject_promise(
          State(..state.merge_globals(state, final_state, []), heap: h2),
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
    Ok(#(YieldCompletion(_, _), _)) ->
      Error(#(
        StepVmError(Unimplemented("yield in non-generator async function")),
        JsUndefined,
        state.heap,
      ))
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
  let existing_data_ref = case awaited_value {
    JsObject(ref) -> heap.read_promise_data_ref(h, ref)
    _ -> None
  }
  let #(h, promise_data_ref) = case existing_data_ref {
    Some(dr) -> #(h, dr)
    None -> {
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
        elements: elements.new(),
        prototype: Some(builtins.function.prototype),
        symbol_properties: [],
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
        elements: elements.new(),
        prototype: Some(builtins.function.prototype),
        symbol_properties: [],
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
pub fn call_native_async_resume(
  state: State,
  async_data_ref: Ref,
  is_reject: Bool,
  args: List(JsValue),
  rest_stack: List(JsValue),
  execute_inner: ExecuteInnerFn,
  unwind_to_catch: UnwindToCatchFn,
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
          // Async function completed -- resolve the outer promise
          let #(h2, jobs) =
            builtins_promise.fulfill_promise(h2, promise_data_ref, return_value)
          Ok(
            State(
              ..state.merge_globals(state, final_state, jobs),
              heap: h2,
              stack: [JsUndefined, ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        }
        Ok(#(ThrowCompletion(thrown, h2), final_state)) -> {
          // Async function threw -- reject the outer promise
          let state =
            builtins_promise.reject_promise(
              State(..state.merge_globals(state, final_state, []), heap: h2),
              promise_data_ref,
              thrown,
            )
          Ok(
            State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1),
          )
        }
        Ok(#(AwaitCompletion(awaited_value, h2), suspended)) -> {
          // Hit another `await` -- save state and set up promise resolution
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
              State(..state.merge_globals(state, suspended, []), heap: h2),
              async_data_ref,
              awaited_value,
            )
          Ok(
            State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1),
          )
        }
        Ok(#(YieldCompletion(_, _), _)) ->
          Error(#(
            StepVmError(Unimplemented("yield in non-generator async function")),
            JsUndefined,
            state.heap,
          ))
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
pub fn setup_locals(
  h: Heap,
  env_ref: value.Ref,
  callee_template: FuncTemplate,
  args: List(JsValue),
) -> tuple_array.TupleArray(JsValue) {
  let env_values = heap.read_env(h, env_ref) |> option.unwrap([])
  let env_count = list.length(env_values)
  let padded_args = pad_args(args, callee_template.arity)
  let remaining =
    callee_template.local_count - env_count - callee_template.arity
  list.flatten([env_values, padded_args, list.repeat(JsUndefined, remaining)])
  |> tuple_array.from_list
}

/// Call a native (Gleam-implemented) function. Most natives execute synchronously
/// and push their result onto the stack. However, call/apply/bind need special
/// handling because they invoke other functions (potentially pushing call frames).
pub fn call_native(
  state: State,
  native: NativeFnSlot,
  args: List(JsValue),
  rest_stack: List(JsValue),
  this: JsValue,
  execute_inner: ExecuteInnerFn,
  unwind_to_catch: UnwindToCatchFn,
  dispatch_fn: DispatchNativeFn,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case native {
    // Function.prototype.call(thisArg, ...args)
    // `this` is the target function, args[0] is the thisArg
    value.Call(value.FunctionCall) -> {
      let #(this_arg, call_args) = case args {
        [t, ..rest] -> #(t, rest)
        [] -> #(JsUndefined, [])
      }
      call_value(
        State(..state, stack: rest_stack),
        this,
        call_args,
        this_arg,
        execute_inner,
        unwind_to_catch,
        dispatch_fn,
      )
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
        // null/undefined argsArray -> no args
        _ -> []
      }
      call_value(
        State(..state, stack: rest_stack),
        this,
        call_args,
        this_arg,
        execute_inner,
        unwind_to_catch,
        dispatch_fn,
      )
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
              case dict.get(properties, Named("name")) {
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
                  #(Named("name"), common.fn_name_property(name)),
                ]),
                elements: elements.new(),
                prototype: Some(state.builtins.function.prototype),
                symbol_properties: [],
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
          state.throw_type_error(state, "Bind must be called on a function")
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
        execute_inner,
        unwind_to_catch,
        dispatch_fn,
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
    // Promise.prototype.catch(onRejected) -- sugar for .then(undefined, onRejected)
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
        execute_inner,
        unwind_to_catch,
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
    value.Call(value.ArrayIteratorNext) ->
      call_array_iterator_next(state, this, rest_stack)
    value.Call(value.GeneratorThrow) ->
      generators.call_native_generator_throw(
        state,
        this,
        args,
        rest_stack,
        execute_inner,
        unwind_to_catch,
      )
    // Async generator prototype methods — enqueue a request, return a promise
    value.Call(value.AsyncGeneratorNext) ->
      async_generators.call_native_method(
        state,
        this,
        args,
        rest_stack,
        value.AGNext,
        execute_inner,
        unwind_to_catch,
      )
    value.Call(value.AsyncGeneratorReturn) ->
      async_generators.call_native_method(
        state,
        this,
        args,
        rest_stack,
        value.AGReturn,
        execute_inner,
        unwind_to_catch,
      )
    value.Call(value.AsyncGeneratorThrow) ->
      async_generators.call_native_method(
        state,
        this,
        args,
        rest_stack,
        value.AGThrow,
        execute_inner,
        unwind_to_catch,
      )
    value.Call(value.AsyncGeneratorResume(data_ref:, is_reject:, is_return:)) ->
      async_generators.call_native_resume(
        state,
        data_ref,
        is_reject,
        is_return,
        args,
        rest_stack,
        execute_inner,
        unwind_to_catch,
      )
    // Symbol() constructor -- callable but NOT new-able
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
    // Symbol.for(key) -- global symbol registry
    value.Call(value.SymbolFor) -> {
      // Step 1: Let stringKey be ? ToString(key).
      let key_val = case args {
        [k, ..] -> k
        [] -> value.JsUndefined
      }
      use #(key_str, state) <- result.try(
        state.rethrow(coerce.js_to_string(state, key_val)),
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
    // Symbol.keyFor(sym) -- reverse lookup in global registry
    value.Call(value.SymbolKeyFor) -> {
      case args {
        [value.JsSymbol(id), ..] -> {
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
          state.rethrow(coerce.thrown_type_error(
            state,
            "Symbol.keyFor requires a Symbol argument",
          ))
      }
    }
    // String() constructor -- uses full ToString (ToPrimitive for objects).
    // §22.1.1.1 step 1.a: if value is a Symbol, return SymbolDescriptiveString
    // (does NOT throw — only implicit ToString on a Symbol throws).
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
        [value.JsSymbol(id), ..] -> {
          let s =
            builtins_symbol.descriptive_string(id, state.symbol_descriptions)
          Ok(
            State(..state, stack: [JsString(s), ..rest_stack], pc: state.pc + 1),
          )
        }
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
      let #(new_state, result) = dispatch_fn(native, args, this, state)
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
    // Host-provided native: call the embedder's closure directly
    value.Host(f) -> {
      let #(new_state, result) = f(args, this, state)
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

/// Full constructor invocation -- handles derived constructors, base constructors,
/// bound functions, and native constructors. Extracted from the CallConstructor
/// opcode handler so CallConstructorApply (spread path) can share it.
pub fn do_construct(
  state: State,
  ctor_ref: Ref,
  args: List(JsValue),
  rest_stack: List(JsValue),
  execute_inner: ExecuteInnerFn,
  unwind_to_catch: UnwindToCatchFn,
  dispatch_fn: DispatchNativeFn,
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
            execute_inner,
            unwind_to_catch,
          )
        False -> {
          // Base constructor: allocate the new object
          let proto = case dict.get(properties, Named("prototype")) {
            Ok(DataProperty(value: JsObject(proto_ref), ..)) -> Some(proto_ref)
            _ -> Some(state.builtins.object.prototype)
          }
          let #(heap, new_obj_ref) =
            heap.alloc(
              state.heap,
              ObjectSlot(
                kind: OrdinaryObject,
                properties: dict.new(),
                elements: elements.new(),
                prototype: proto,
                symbol_properties: [],
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
            execute_inner,
            unwind_to_catch,
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
      construct_value(
        state,
        target,
        final_args,
        rest_stack,
        execute_inner,
        unwind_to_catch,
        dispatch_fn,
      )
    }
    // `new String(sym)` must throw (§22.1.1.1 — the Symbol→descriptive-string
    // special case only applies when NewTarget is undefined). Intercept here so
    // the Symbol arg hits ToString and throws, instead of the non-throwing path
    // in call_native's StringConstructor handler.
    Some(ObjectSlot(
      kind: NativeFunction(value.Call(value.StringConstructor)),
      ..,
    )) ->
      case args {
        [value.JsSymbol(_), ..] ->
          state.rethrow(coerce.thrown_type_error(
            state,
            "Cannot convert a Symbol value to a string",
          ))
        _ ->
          call_native(
            state,
            value.Call(value.StringConstructor),
            args,
            rest_stack,
            JsUndefined,
            execute_inner,
            unwind_to_catch,
            dispatch_fn,
          )
      }
    Some(ObjectSlot(kind: NativeFunction(native), ..)) ->
      call_native(
        state,
        native,
        args,
        rest_stack,
        JsUndefined,
        execute_inner,
        unwind_to_catch,
        dispatch_fn,
      )
    _ ->
      state.throw_type_error(
        state,
        object.inspect(JsObject(ctor_ref), state.heap)
          <> " is not a constructor",
      )
  }
}

/// Construct a new object using the target function ref.
/// Used by CallConstructor when the constructor is a bound function.
pub fn construct_value(
  state: State,
  target_ref: Ref,
  args: List(JsValue),
  rest_stack: List(JsValue),
  execute_inner: ExecuteInnerFn,
  unwind_to_catch: UnwindToCatchFn,
  dispatch_fn: DispatchNativeFn,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case heap.read(state.heap, target_ref) {
    Some(ObjectSlot(
      kind: FunctionObject(func_template:, env: env_ref),
      properties:,
      ..,
    )) -> {
      let proto = case dict.get(properties, Named("prototype")) {
        Ok(DataProperty(value: JsObject(proto_ref), ..)) -> Some(proto_ref)
        _ -> Some(state.builtins.object.prototype)
      }
      let #(h, new_obj_ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: OrdinaryObject,
            properties: dict.new(),
            elements: elements.new(),
            prototype: proto,
            symbol_properties: [],
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
        execute_inner,
        unwind_to_catch,
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
      construct_value(
        state,
        target,
        final_args,
        rest_stack,
        execute_inner,
        unwind_to_catch,
        dispatch_fn,
      )
    }
    Some(ObjectSlot(kind: NativeFunction(native), ..)) ->
      call_native(
        state,
        native,
        args,
        rest_stack,
        JsUndefined,
        execute_inner,
        unwind_to_catch,
        dispatch_fn,
      )
    _ ->
      state.throw_type_error(
        state,
        object.inspect(JsObject(target_ref), state.heap)
          <> " is not a constructor",
      )
  }
}

/// Call an arbitrary JsValue as a function with the given this and args.
/// Used by Function.prototype.call/apply and bound function invocation.
pub fn call_value(
  state: State,
  callee: JsValue,
  args: List(JsValue),
  this_val: JsValue,
  execute_inner: ExecuteInnerFn,
  unwind_to_catch: UnwindToCatchFn,
  dispatch_fn: DispatchNativeFn,
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
            execute_inner,
            unwind_to_catch,
          )
        Some(ObjectSlot(kind: NativeFunction(native), ..)) ->
          call_native(
            state,
            native,
            args,
            state.stack,
            this_val,
            execute_inner,
            unwind_to_catch,
            dispatch_fn,
          )
        _ ->
          state.throw_type_error(
            state,
            object.inspect(callee, state.heap) <> " is not a function",
          )
      }
    _ ->
      state.throw_type_error(
        state,
        object.inspect(callee, state.heap) <> " is not a function",
      )
  }
}

/// Extract elements from an array object as a list of JsValues.
/// Used by Function.prototype.apply to unpack the args tuple_array.
pub fn extract_array_args(h: Heap, ref: Ref) -> List(JsValue) {
  heap.read_array_like(h, ref)
  |> option.map(fn(p) { extract_elements_loop(p.1, 0, p.0, []) })
  |> option.unwrap([])
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
      let val = elements.get(elements, idx)
      extract_elements_loop(elements, idx + 1, length, [val, ..acc])
    }
  }
}

/// ES §23.1.5.2.1 %ArrayIteratorPrototype%.next()
fn call_array_iterator_next(
  state: State,
  this: JsValue,
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case this {
    JsObject(iter_ref) ->
      case heap.read(state.heap, iter_ref) {
        Some(
          ObjectSlot(kind: value.ArrayIteratorObject(source:, index:), ..) as slot,
        ) -> {
          let #(length, elems) =
            heap.read_array_like(state.heap, source)
            |> option.unwrap(#(0, elements.new()))
          case index >= length {
            True -> {
              let #(h, result) =
                generators.create_iterator_result(
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
            False -> {
              let val = elements.get(elems, index)
              let h =
                heap.write(
                  state.heap,
                  iter_ref,
                  ObjectSlot(
                    ..slot,
                    kind: value.ArrayIteratorObject(source:, index: index + 1),
                  ),
                )
              let #(h, result) =
                generators.create_iterator_result(h, state.builtins, val, False)
              Ok(
                State(
                  ..state,
                  heap: h,
                  stack: [result, ..rest_stack],
                  pc: state.pc + 1,
                ),
              )
            }
          }
        }
        _ ->
          state.throw_type_error(
            state,
            "Array Iterator next called on incompatible receiver",
          )
      }
    _ ->
      state.throw_type_error(
        state,
        "Array Iterator next called on incompatible receiver",
      )
  }
}

/// Function.prototype.toString -- ES2024 S20.2.3.5
///
/// For native functions: "function NAME() { [native code] }"
/// For user-defined functions: "function NAME() { [native code] }" (simplified)
pub fn function_to_string(
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
            dict.get(properties, Named("name"))
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
          state.type_error(
            state,
            "Function.prototype.toString requires that 'this' be a Function",
          )
      }
    _ ->
      state.type_error(
        state,
        "Function.prototype.toString requires that 'this' be a Function",
      )
  }
}

/// Pad args to exactly `arity` length -- truncate extras, fill missing with undefined.
pub fn pad_args(args: List(JsValue), arity: Int) -> List(JsValue) {
  let len = list.length(args)
  case len >= arity {
    True -> list.take(args, arity)
    False -> list.append(args, list.repeat(JsUndefined, arity - len))
  }
}

// ============================================================================
// Native dispatch — route NativeFn to the correct builtins module
// ============================================================================

pub fn dispatch_native(
  native: value.NativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State,
  execute_inner: ExecuteInnerFn,
  call_native_fn: fn(State, NativeFnSlot, List(JsValue), List(JsValue), JsValue) ->
    Result(State, #(StepResult, JsValue, Heap)),
  new_state_fn: realm.NewStateFn,
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
    value.VmNative(value.ArcSpawn) ->
      realm.arc_spawn(args, state, execute_inner, call_native_fn, new_state_fn)
    value.VmNative(value.EvalScript) ->
      realm.eval_script_native(args, this, state, execute_inner, new_state_fn)
    value.VmNative(value.CreateRealm) -> realm.create_realm_native(this, state)
    value.VmNative(value.Gc) -> #(state, Ok(JsUndefined))
    value.JsonNative(n) -> builtins_json.dispatch(n, args, this, state)
    value.ReflectNative(n) -> builtins_reflect.dispatch(n, args, this, state)
    value.MapNative(n) -> builtins_map.dispatch(n, args, this, state)
    value.SetNative(n) -> builtins_set.dispatch(n, args, this, state)
    value.WeakMapNative(n) -> builtins_weak_map.dispatch(n, args, this, state)
    value.WeakSetNative(n) -> builtins_weak_set.dispatch(n, args, this, state)
    value.RegExpNative(n) -> builtins_regexp.dispatch(n, args, this, state)
    // Standalone VM-level natives
    value.VmNative(value.FunctionConstructor) ->
      realm.function_constructor_native(
        args,
        state,
        execute_inner,
        new_state_fn,
      )
    value.VmNative(value.IteratorSymbolIterator) -> #(state, Ok(this))
    value.VmNative(value.FunctionToString) -> function_to_string(this, state)
    // Global functions: eval, URI encoding/decoding
    value.VmNative(value.Eval) ->
      realm.eval_native(args, state, execute_inner, new_state_fn)
    value.VmNative(value.DecodeURI) -> {
      let arg = case args {
        [s, ..] -> s
        [] -> value.JsUndefined
      }
      use str, state <- state.try_to_string(state, arg)
      #(state, Ok(value.JsString(operators.uri_decode(str))))
    }
    value.VmNative(value.EncodeURI) -> {
      let arg = case args {
        [s, ..] -> s
        [] -> value.JsUndefined
      }
      use str, state <- state.try_to_string(state, arg)
      #(state, Ok(value.JsString(operators.uri_encode(str, True))))
    }
    value.VmNative(value.DecodeURIComponent) -> {
      let arg = case args {
        [s, ..] -> s
        [] -> value.JsUndefined
      }
      use str, state <- state.try_to_string(state, arg)
      #(state, Ok(value.JsString(operators.uri_decode(str))))
    }
    value.VmNative(value.EncodeURIComponent) -> {
      let arg = case args {
        [s, ..] -> s
        [] -> value.JsUndefined
      }
      use str, state <- state.try_to_string(state, arg)
      #(state, Ok(value.JsString(operators.uri_encode(str, False))))
    }
    // AnnexB B.2.1.1 escape ( string )
    value.VmNative(value.Escape) -> {
      let arg = case args {
        [s, ..] -> s
        [] -> value.JsUndefined
      }
      use str, state <- state.try_to_string(state, arg)
      #(state, Ok(value.JsString(operators.js_escape(str))))
    }
    // AnnexB B.2.1.2 unescape ( string )
    value.VmNative(value.Unescape) -> {
      let arg = case args {
        [s, ..] -> s
        [] -> value.JsUndefined
      }
      use str, state <- state.try_to_string(state, arg)
      #(state, Ok(value.JsString(operators.js_unescape(str))))
    }
  }
}
