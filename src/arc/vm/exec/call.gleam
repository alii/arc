import arc/vm/builtins/array as builtins_array
import arc/vm/builtins/boolean as builtins_boolean
import arc/vm/builtins/common
import arc/vm/builtins/console as builtins_console
import arc/vm/builtins/date as builtins_date
import arc/vm/builtins/error as builtins_error
import arc/vm/builtins/helpers
import arc/vm/builtins/iterator as builtins_iterator
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
import arc/vm/builtins/structured_clone
import arc/vm/builtins/symbol as builtins_symbol
import arc/vm/builtins/weak_map as builtins_weak_map
import arc/vm/builtins/weak_set as builtins_weak_set
import arc/vm/completion.{
  type Completion, AwaitCompletion, NormalCompletion, ThrowCompletion,
  YieldCompletion,
}
import arc/vm/exec/async_generators
import arc/vm/exec/frame
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
  type ExoticKind, type Heap, type HeapSlot, type NativeFnSlot, type State,
  type StepResult, type VmError, SavedFrame, State, StepVmError, Thrown,
  Unimplemented,
}
import arc/vm/value.{
  type FuncTemplate, type JsValue, type Ref, AsyncFunctionSlot,
  AsyncGeneratorObject, AsyncGeneratorSlot, DataProperty, FunctionObject,
  GeneratorObject, GeneratorSlot, JsObject, JsString, JsUndefined,
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

/// Shared logic for Call, CallMethod, and CallConstructor.
/// Looks up the callee template, saves the caller frame, sets up locals,
/// and transitions to the callee's code.
pub fn call_function(
  state: State,
  fn_ref: value.Ref,
  env_ref: value.Ref,
  home_object: Option(value.Ref),
  callee_template: FuncTemplate,
  args: List(JsValue),
  rest_stack: List(JsValue),
  this_val: JsValue,
  constructor_this: option.Option(JsValue),
  new_target: JsValue,
  execute_inner: ExecuteInnerFn,
  unwind_to_catch: UnwindToCatchFn,
) -> Result(State, #(StepResult, JsValue, State)) {
  let #(heap, this_val) = frame.bind_this(state, callee_template, this_val)
  let state = State(..state, heap:)
  let locals =
    frame.setup_locals(
      state.heap,
      env_ref,
      fn_ref,
      home_object,
      callee_template,
      args,
      this_val,
      new_target,
    )
  case callee_template.is_generator, callee_template.is_async {
    True, True ->
      call_async_generator_function(
        state,
        env_ref,
        callee_template,
        args,
        rest_stack,
        locals,
        execute_inner,
      )
    True, False ->
      call_generator_function(
        state,
        env_ref,
        callee_template,
        args,
        rest_stack,
        locals,
        execute_inner,
      )
    False, True ->
      call_async_function(
        state,
        env_ref,
        callee_template,
        args,
        rest_stack,
        locals,
        execute_inner,
        unwind_to_catch,
      )
    False, False ->
      call_regular_function(
        state,
        callee_template,
        args,
        rest_stack,
        locals,
        constructor_this,
        new_target,
      )
  }
}

/// Regular (non-generator) function call: save frame, enter callee.
fn call_regular_function(
  state: State,
  callee_template: FuncTemplate,
  args: List(JsValue),
  rest_stack: List(JsValue),
  locals: tuple_array.TupleArray(JsValue),
  constructor_this: option.Option(JsValue),
  new_target: JsValue,
) -> Result(State, #(StepResult, JsValue, State)) {
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
      constructor_this:,
      new_target: state.new_target,
      call_args: state.call_args,
      eval_env: state.eval_env,
      current_line: state.current_line,
    )
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
      new_target:,
      call_args: args,
      eval_env: None,
    ),
  )
}

/// Set up an isolated execution state for a (sync or async) generator body.
fn generator_initial_state(
  state: State,
  callee_template: FuncTemplate,
  args: List(JsValue),
  locals: tuple_array.TupleArray(JsValue),
) -> State {
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
    new_target: JsUndefined,
    call_args: args,
  )
}

/// Allocate the suspended-at-InitialYield slot and wrap it in a generator
/// object, returning to the caller with the object on the stack. Shared by
/// the sync and async generator call paths.
fn alloc_suspended_generator(
  state: State,
  suspended: State,
  rest_stack: List(JsValue),
  slot: HeapSlot,
  make_kind: fn(value.Ref) -> ExoticKind,
  prototype: value.Ref,
) -> Result(State, #(StepResult, JsValue, State)) {
  let #(h, data_ref) = heap.alloc(suspended.heap, slot)
  let #(h, gen_obj_ref) =
    common.alloc_wrapper(h, make_kind(data_ref), prototype)
  Ok(
    State(
      ..state.merge_globals(state, suspended, []),
      heap: h,
      stack: [JsObject(gen_obj_ref), ..rest_stack],
      pc: state.pc + 1,
    ),
  )
}

/// Generator function call: execute until InitialYield, save state to
/// GeneratorSlot, create GeneratorObject, return it to caller.
fn call_generator_function(
  state: State,
  env_ref: value.Ref,
  callee_template: FuncTemplate,
  args: List(JsValue),
  rest_stack: List(JsValue),
  locals: tuple_array.TupleArray(JsValue),
  execute_inner: ExecuteInnerFn,
) -> Result(State, #(StepResult, JsValue, State)) {
  // Execute until InitialYield (which fires immediately at the start)
  case
    execute_inner(generator_initial_state(state, callee_template, args, locals))
  {
    Ok(#(YieldCompletion(_, _), suspended)) ->
      alloc_suspended_generator(
        state,
        suspended,
        rest_stack,
        GeneratorSlot(
          gen_state: value.SuspendedStart,
          func_template: callee_template,
          env_ref:,
          saved_pc: suspended.pc,
          saved_locals: suspended.locals,
          saved_stack: suspended.stack,
          saved_try_stack: generators.save_stacks(suspended.try_stack),
        ),
        GeneratorObject,
        state.builtins.generator.prototype,
      )
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
          ),
        )
      let #(h, gen_obj_ref) =
        common.alloc_wrapper(
          h,
          GeneratorObject(generator_data: data_ref),
          state.builtins.generator.prototype,
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
    Ok(#(ThrowCompletion(thrown, h), _)) ->
      Error(#(Thrown, thrown, State(..state, heap: h)))
    Ok(#(AwaitCompletion(_, _), _)) ->
      Error(#(
        StepVmError(Unimplemented("await in sync generator")),
        JsUndefined,
        state,
      ))
    Error(vm_err) -> Error(#(StepVmError(vm_err), JsUndefined, state))
  }
}

/// Async generator call: run to InitialYield, allocate AsyncGeneratorSlot with
/// empty request queue, return AsyncGeneratorObject. Body doesn't actually
/// execute until the first .next() — that's when the driver loop kicks in.
fn call_async_generator_function(
  state: State,
  env_ref: value.Ref,
  callee_template: FuncTemplate,
  args: List(JsValue),
  rest_stack: List(JsValue),
  locals: tuple_array.TupleArray(JsValue),
  execute_inner: ExecuteInnerFn,
) -> Result(State, #(StepResult, JsValue, State)) {
  case
    execute_inner(generator_initial_state(state, callee_template, args, locals))
  {
    Ok(#(YieldCompletion(_, _), suspended)) ->
      alloc_suspended_generator(
        state,
        suspended,
        rest_stack,
        AsyncGeneratorSlot(
          gen_state: value.AGSuspendedStart,
          queue: [],
          func_template: callee_template,
          env_ref:,
          saved_pc: suspended.pc,
          saved_locals: suspended.locals,
          saved_stack: suspended.stack,
          saved_try_stack: generators.save_stacks(suspended.try_stack),
        ),
        AsyncGeneratorObject,
        state.builtins.async_generator.prototype,
      )
    Ok(#(ThrowCompletion(thrown, h), _)) ->
      Error(#(Thrown, thrown, State(..state, heap: h)))
    Ok(#(NormalCompletion(_, _), _)) | Ok(#(AwaitCompletion(_, _), _)) ->
      // InitialYield is first op — body never runs before it. Unreachable.
      Error(#(
        StepVmError(Unimplemented("async generator didn't hit InitialYield")),
        JsUndefined,
        state,
      ))
    Error(vm_err) -> Error(#(StepVmError(vm_err), JsUndefined, state))
  }
}

/// Async function call: create a promise, execute body eagerly.
/// If the body completes synchronously, resolve/reject the promise immediately.
/// If the body hits `await`, save state and set up promise callbacks to resume.
fn call_async_function(
  state: State,
  env_ref: value.Ref,
  callee_template: FuncTemplate,
  args: List(JsValue),
  rest_stack: List(JsValue),
  locals: tuple_array.TupleArray(JsValue),
  execute_inner: ExecuteInnerFn,
  _unwind_to_catch: UnwindToCatchFn,
) -> Result(State, #(StepResult, JsValue, State)) {
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
  // Execute body eagerly
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
      new_target: JsUndefined,
      call_args: args,
    )
  finish_async_execution(
    state,
    execute_inner(async_state),
    data_ref,
    resolve_fn,
    reject_fn,
    callee_template,
    env_ref,
    None,
    JsObject(promise_ref),
    rest_stack,
  )
}

/// Shared completion handling for an async function body execution.
/// Resolves/rejects the outer promise on completion, or saves the suspension
/// state and attaches resume callbacks on `await`. `existing_slot_ref` is
/// `Some` to overwrite an existing AsyncFunctionSlot on re-suspension, `None`
/// to allocate a fresh slot on first await.
fn finish_async_execution(
  state: State,
  exec_result: Result(#(Completion, State), VmError),
  promise_data_ref: Ref,
  resolve: JsValue,
  reject: JsValue,
  func_template: FuncTemplate,
  env_ref: Ref,
  existing_slot_ref: Option(Ref),
  result_value: JsValue,
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, State)) {
  case exec_result {
    Ok(#(AwaitCompletion(awaited_value, h2), suspended)) -> {
      // Body hit `await` -- save state, set up promise resolution
      let saved_try = generators.save_stacks(suspended.try_stack)
      let slot =
        AsyncFunctionSlot(
          promise_data_ref:,
          resolve:,
          reject:,
          func_template:,
          env_ref:,
          saved_pc: suspended.pc,
          saved_locals: suspended.locals,
          saved_stack: suspended.stack,
          saved_try_stack: saved_try,
        )
      let #(h2, async_data_ref) = case existing_slot_ref {
        Some(slot_ref) -> #(heap.write(h2, slot_ref, slot), slot_ref)
        None -> heap.alloc(h2, slot)
      }
      let state =
        async_setup_await(
          State(..state.merge_globals(state, suspended, []), heap: h2),
          async_data_ref,
          awaited_value,
        )
      Ok(State(..state, stack: [result_value, ..rest_stack], pc: state.pc + 1))
    }
    Ok(#(NormalCompletion(return_value, h2), final_state)) -> {
      // Async function completed -- resolve the outer promise
      let #(h2, jobs) =
        builtins_promise.fulfill_promise(h2, promise_data_ref, return_value)
      Ok(
        State(
          ..state.merge_globals(state, final_state, jobs),
          heap: h2,
          stack: [result_value, ..rest_stack],
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
      Ok(State(..state, stack: [result_value, ..rest_stack], pc: state.pc + 1))
    }
    Ok(#(YieldCompletion(_, _), _)) ->
      Error(#(
        StepVmError(Unimplemented("yield in non-generator async function")),
        JsUndefined,
        state,
      ))
    Error(vm_err) -> Error(#(StepVmError(vm_err), JsUndefined, state))
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
  use is_reject <- promises.setup_await(state, awaited_value)
  value.AsyncResume(async_data_ref:, is_reject:)
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
) -> Result(State, #(StepResult, JsValue, State)) {
  let settled_value = helpers.first_arg_or_undefined(args)
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
    )) -> {
      // Restore try-stack
      let restored_try = generators.restore_stacks(saved_try_stack)
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
          new_target: JsUndefined,
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
      finish_async_execution(
        state,
        exec_result,
        promise_data_ref,
        slot_resolve,
        slot_reject,
        func_template,
        slot_env_ref,
        Some(async_data_ref),
        JsUndefined,
        rest_stack,
      )
    }
    _ ->
      Error(#(
        StepVmError(Unimplemented(
          "async resume: invalid slot for ref "
          <> string.inspect(async_data_ref),
        )),
        JsUndefined,
        state,
      ))
  }
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
) -> Result(State, #(StepResult, JsValue, State)) {
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
            common.alloc_call_fn_props(
              state.heap,
              state.builtins.function.prototype,
              value.BoundFunction(
                target: target_ref,
                bound_this: this_arg,
                bound_args:,
              ),
              // §10.4.1.3 step 6: the bound function has [[Construct]] iff
              // its target does. Copy the target's bit at bind time.
              constructible: object.is_constructor(
                state.heap,
                JsObject(target_ref),
              ),
              props: [#("name", common.fn_name_property(name))],
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
      promises.call_native_promise_all_settled_element(
        state,
        args,
        rest_stack,
        index,
        remaining_ref,
        values_ref,
        already_called_ref,
        resolve,
        #("fulfilled", "value"),
      )
    // Promise.allSettled per-element reject handler
    value.Call(value.PromiseAllSettledRejectElement(
      index:,
      remaining_ref:,
      values_ref:,
      already_called_ref:,
      resolve:,
    )) ->
      promises.call_native_promise_all_settled_element(
        state,
        args,
        rest_stack,
        index,
        remaining_ref,
        values_ref,
        already_called_ref,
        resolve,
        #("rejected", "reason"),
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
      Error(#(Thrown, reason, state))
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
    value.Call(value.SetIteratorNext) ->
      call_set_iterator_next(state, this, rest_stack)
    value.Call(value.MapIteratorNext) ->
      call_map_iterator_next(state, this, rest_stack)
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
    value.Call(value.AsyncFromSyncNext) ->
      promises.call_native_async_from_sync(
        state,
        this,
        args,
        rest_stack,
        promises.AfsNext,
      )
    value.Call(value.AsyncFromSyncReturn) ->
      promises.call_native_async_from_sync(
        state,
        this,
        args,
        rest_stack,
        promises.AfsReturn,
      )
    value.Call(value.AsyncFromSyncThrow) ->
      promises.call_native_async_from_sync(
        state,
        this,
        args,
        rest_stack,
        promises.AfsThrow,
      )
    value.Call(value.AsyncFromSyncUnwrap(done:)) ->
      promises.call_native_async_from_sync_unwrap(state, done, args, rest_stack)
    value.Call(value.AsyncFromSyncClose(sync_iter:)) ->
      promises.call_native_async_from_sync_close(state, sync_iter, args)
    value.Call(value.AsyncGeneratorResume(data_ref:, is_reject:, kind:)) ->
      async_generators.call_native_resume(
        state,
        data_ref,
        is_reject,
        kind,
        args,
        rest_stack,
        execute_inner,
        unwind_to_catch,
      )
    // Symbol() constructor -- callable but NOT new-able
    value.Call(value.SymbolConstructor) -> {
      let #(new_descs, sym_val) =
        builtins_symbol.call_symbol(args, state.ctx.symbol_descriptions)
      Ok(
        State(
          ..state,
          stack: [sym_val, ..rest_stack],
          pc: state.pc + 1,
          ctx: state.RealmCtx(..state.ctx, symbol_descriptions: new_descs),
        ),
      )
    }
    // Symbol.for(key) -- global symbol registry
    value.Call(value.SymbolFor) -> {
      // Step 1: Let stringKey be ? ToString(key).
      let key_val = helpers.first_arg_or_undefined(args)
      use #(key_str, state) <- result.try(
        state.rethrow(coerce.js_to_string(state, key_val)),
      )
      // Step 2-4: Look up in GlobalSymbolRegistry, return existing or create new.
      case dict.get(state.ctx.symbol_registry, key_str) {
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
          let new_registry = dict.insert(state.ctx.symbol_registry, key_str, id)
          let new_descs =
            dict.insert(state.ctx.symbol_descriptions, id, key_str)
          Ok(
            State(
              ..state,
              stack: [value.JsSymbol(id), ..rest_stack],
              pc: state.pc + 1,
              ctx: state.RealmCtx(
                ..state.ctx,
                symbol_registry: new_registry,
                symbol_descriptions: new_descs,
              ),
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
            dict.to_list(state.ctx.symbol_registry)
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
    // §22.1.1.1 step 2.a: if NewTarget is undefined and value is a Symbol,
    // return SymbolDescriptiveString (does NOT throw — only implicit ToString
    // on a Symbol throws).
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
            builtins_symbol.descriptive_string(
              id,
              state.ctx.symbol_descriptions,
            )
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
            Error(#(thrown, new_state)) -> Error(#(Thrown, thrown, new_state))
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
        Error(thrown) -> Error(#(Thrown, thrown, new_state))
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
        Error(thrown) -> Error(#(Thrown, thrown, new_state))
      }
    }
  }
}

/// Shared tail for `new String/Number/Boolean`: allocate the primitive
/// wrapper exotic, push it, advance pc. Prototype is read from newTarget
/// (§10.1.13.2) so subclass instances get the derived prototype.
fn push_wrapper(
  state: State,
  rest_stack: List(JsValue),
  kind,
  new_target_ref: Ref,
  intrinsic_proto: Ref,
) {
  let proto =
    prototype_from_new_target_or(state, new_target_ref, intrinsic_proto)
  let #(heap, ref) = common.alloc_wrapper(state.heap, kind, proto)
  Ok(
    State(
      ..state,
      heap:,
      stack: [JsObject(ref), ..rest_stack],
      pc: state.pc + 1,
    ),
  )
}

/// Full constructor invocation -- handles derived constructors, base constructors,
/// bound functions, and native constructors. Extracted from the CallConstructor
/// opcode handler so CallConstructorApply (spread path) can share it.
/// `new_target_ref` is §10.1.13 newTarget — equals `ctor_ref` for plain
/// `new X()`, the leaf-derived-ctor for `super()`, or argv[2] for
/// Reflect.construct.
pub fn do_construct(
  state: State,
  ctor_ref: Ref,
  args: List(JsValue),
  rest_stack: List(JsValue),
  new_target_ref: Ref,
  execute_inner: ExecuteInnerFn,
  unwind_to_catch: UnwindToCatchFn,
  dispatch_fn: DispatchNativeFn,
) -> Result(State, #(StepResult, JsValue, State)) {
  let new_target = JsObject(new_target_ref)
  // §7.2.4 IsConstructor gate — runs AFTER ArgumentListEvaluation per
  // §13.3.7.2 step 5, so `super(sideEffect())` with a non-ctor parent still
  // evaluates args before throwing (call-proto-not-ctor.js).
  use <- bool.lazy_guard(
    !object.is_constructor(state.heap, JsObject(ctor_ref)),
    fn() {
      state.throw_type_error(
        state,
        object.inspect(JsObject(ctor_ref), state.heap)
          <> " is not a constructor",
      )
    },
  )
  case heap.read(state.heap, ctor_ref) {
    // Functions lacking [[Construct]] — arrows, generators, async functions,
    // and methods/getters/setters — throw when used with `new` (§7.2.4). The
    // capability is stored on the template (set at compile from the syntactic
    // kind); class constructors and normal functions carry it.
    Some(ObjectSlot(kind: FunctionObject(func_template:, ..), ..))
      if !func_template.is_constructor
    ->
      state.throw_type_error(
        state,
        object.inspect(JsObject(ctor_ref), state.heap)
          <> " is not a constructor",
      )
    Some(ObjectSlot(
      kind: FunctionObject(func_template:, env: env_ref, home_object:),
      ..,
    )) ->
      case func_template.is_derived_constructor {
        True ->
          // Derived constructor: don't allocate object yet.
          // this = JsUninitialized (TDZ until super() is called).
          // constructor_this = None signals derived constructor mode.
          call_function(
            state,
            ctor_ref,
            env_ref,
            home_object,
            func_template,
            args,
            rest_stack,
            JsUninitialized,
            None,
            new_target,
            execute_inner,
            unwind_to_catch,
          )
        False -> {
          // Base constructor: §10.1.13.1 OrdinaryCreateFromConstructor —
          // proto comes from newTarget.prototype, NOT ctor.prototype.
          let proto = prototype_from_new_target(state, new_target_ref)
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
            home_object,
            func_template,
            args,
            rest_stack,
            new_obj,
            Some(new_obj),
            new_target,
            execute_inner,
            unwind_to_catch,
          )
        }
      }
    // Bound function used as constructor (§10.4.1.2): construct
    // [[BoundTargetFunction]] with [[BoundArguments]] prepended; if
    // SameValue(F, newTarget) then newTarget ← target.
    Some(ObjectSlot(
      kind: NativeFunction(
        value.Call(value.BoundFunction(target:, bound_args:, ..)),
        ..,
      ),
      ..,
    )) -> {
      let nt = case new_target_ref == ctor_ref {
        True -> target
        False -> new_target_ref
      }
      do_construct(
        state,
        target,
        list.append(bound_args, args),
        rest_stack,
        nt,
        execute_inner,
        unwind_to_catch,
        dispatch_fn,
      )
    }
    // new String(value) — §22.1.1.1: s = ToString(value), then return a
    // String exotic wrapper with [[StringData]] = s. Unlike String(value),
    // the Symbol→descriptive-string shortcut does NOT apply when NewTarget
    // is defined, so a Symbol arg flows into ToString and throws.
    Some(ObjectSlot(
      kind: NativeFunction(value.Call(value.StringConstructor), ..),
      ..,
    )) -> {
      let coerced = case args {
        [] -> Ok(#("", state))
        [v, ..] -> coerce.js_to_string(state, v)
      }
      case coerced {
        Error(#(thrown, st)) -> Error(#(Thrown, thrown, st))
        Ok(#(s, state)) ->
          push_wrapper(
            state,
            rest_stack,
            value.StringObject(s),
            new_target_ref,
            state.builtins.string.prototype,
          )
      }
    }
    // new Number(value) — §21.1.1.1: prim = ToNumeric(value); if prim is a
    // BigInt let n = 𝔽(ℝ(prim)), else n = prim; then return a wrapper object
    // with [[NumberData]] = n.
    Some(ObjectSlot(
      kind: NativeFunction(
        value.Dispatch(value.NumberNative(value.NumberConstructor)),
        ..,
      ),
      ..,
    )) -> {
      let n = case args {
        [] -> value.Finite(0.0)
        [v, ..] -> builtins_math.to_number(v)
      }
      push_wrapper(
        state,
        rest_stack,
        value.NumberObject(n),
        new_target_ref,
        state.builtins.number.prototype,
      )
    }
    // new Boolean(value) — §20.3.1.1: b = ToBoolean(value), then return a
    // wrapper object with [[BooleanData]] = b.
    Some(ObjectSlot(
      kind: NativeFunction(
        value.Dispatch(value.BooleanNative(value.BooleanConstructor)),
        ..,
      ),
      ..,
    )) -> {
      let b = case args {
        [] -> False
        [v, ..] -> value.is_truthy(v)
      }
      push_wrapper(
        state,
        rest_stack,
        value.BooleanObject(b),
        new_target_ref,
        state.builtins.boolean.prototype,
      )
    }
    // §20.4.1.1: Symbol has [[Construct]] (so IsConstructor is true and it can
    // appear in `extends`), but invoking it as a constructor always throws.
    Some(ObjectSlot(
      kind: NativeFunction(value.Call(value.SymbolConstructor), ..),
      ..,
    )) -> state.throw_type_error(state, "Symbol is not a constructor")
    Some(ObjectSlot(kind: NativeFunction(native, ..), ..)) ->
      case object.is_constructor(state.heap, JsObject(ctor_ref)) {
        True -> {
          // Thread newTarget for the native body and re-prototype the result so
          // subclassing builtins (`class M extends Map {}`) gets the derived
          // prototype. Only re-prototype when newTarget actually has its own
          // .prototype data property — natives that should honour a custom
          // newTarget themselves (or whose newTarget.prototype is an accessor)
          // are left untouched so the native's own choice stands.
          use new_state <- result.try(call_native(
            State(..state, new_target:),
            native,
            args,
            rest_stack,
            JsUndefined,
            execute_inner,
            unwind_to_catch,
            dispatch_fn,
          ))
          let new_state = State(..new_state, new_target: state.new_target)
          let derived_proto = own_data_prototype(new_state.heap, new_target_ref)
          case new_target_ref == ctor_ref, derived_proto, new_state.stack {
            // Plain `new Builtin()` — native already chose the right prototype.
            True, _, _ -> Ok(new_state)
            False, Some(proto), [JsObject(result_ref), ..] -> {
              let heap = set_prototype(new_state.heap, result_ref, Some(proto))
              Ok(State(..new_state, heap:))
            }
            False, _, _ -> Ok(new_state)
          }
        }
        False ->
          state.throw_type_error(
            state,
            object.inspect(JsObject(ctor_ref), state.heap)
              <> " is not a constructor",
          )
      }
    _ ->
      state.throw_type_error(
        state,
        object.inspect(JsObject(ctor_ref), state.heap)
          <> " is not a constructor",
      )
  }
}

/// §10.1.13.2 GetPrototypeFromConstructor: read newTarget.prototype, fall
/// back to %Object.prototype% if not an object.
fn prototype_from_new_target(state: State, new_target_ref: Ref) -> Option(Ref) {
  Some(prototype_from_new_target_or(
    state,
    new_target_ref,
    state.builtins.object.prototype,
  ))
}

/// Read newTarget's own `.prototype` only when it's a DataProperty pointing
/// at an object — accessor prototypes and missing slots return None so the
/// caller can leave the native ctor's own prototype choice in place.
fn own_data_prototype(h: Heap, new_target_ref: Ref) -> Option(Ref) {
  case heap.read(h, new_target_ref) {
    Some(ObjectSlot(properties:, ..)) ->
      case dict.get(properties, Named("prototype")) {
        Ok(DataProperty(value: JsObject(proto_ref), ..)) -> Some(proto_ref)
        _ -> None
      }
    _ -> None
  }
}

/// §10.1.13.2 GetPrototypeFromConstructor with explicit intrinsic fallback.
fn prototype_from_new_target_or(
  state: State,
  new_target_ref: Ref,
  intrinsic_proto: Ref,
) -> Ref {
  case heap.read(state.heap, new_target_ref) {
    Some(ObjectSlot(properties:, ..)) ->
      case dict.get(properties, Named("prototype")) {
        Ok(DataProperty(value: JsObject(proto_ref), ..)) -> proto_ref
        _ -> intrinsic_proto
      }
    _ -> intrinsic_proto
  }
}

fn set_prototype(h: Heap, ref: Ref, proto: Option(Ref)) -> Heap {
  case heap.read(h, ref) {
    Some(ObjectSlot(..) as slot) ->
      heap.write(h, ref, ObjectSlot(..slot, prototype: proto))
    _ -> h
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
) -> Result(State, #(StepResult, JsValue, State)) {
  case callee {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(
          kind: FunctionObject(func_template:, env: env_ref, home_object:),
          ..,
        )) ->
          call_function(
            state,
            ref,
            env_ref,
            home_object,
            func_template,
            args,
            state.stack,
            this_val,
            None,
            JsUndefined,
            execute_inner,
            unwind_to_catch,
          )
        Some(ObjectSlot(kind: NativeFunction(native, ..), ..)) ->
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
  |> option.map(fn(p) { elements.to_list_padded(p.1, p.0) })
  |> option.unwrap([])
}

/// Allocate a {value, done} iterator-result object, push it, advance pc.
fn push_iter_result(
  state: State,
  rest_stack: List(JsValue),
  h: Heap,
  val: JsValue,
  done: Bool,
) -> Result(State, #(StepResult, JsValue, State)) {
  let #(h, result) = common.create_iter_result(h, state.builtins, val, done)
  Ok(State(..state, heap: h, stack: [result, ..rest_stack], pc: state.pc + 1))
}

fn iter_incompatible(
  state: State,
  tag: String,
) -> Result(State, #(StepResult, JsValue, State)) {
  state.throw_type_error(
    state,
    tag <> " Iterator next called on incompatible receiver",
  )
}

/// ES §23.1.5.2.1 %ArrayIteratorPrototype%.next()
fn call_array_iterator_next(
  state: State,
  this: JsValue,
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, State)) {
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
            True ->
              push_iter_result(state, rest_stack, state.heap, JsUndefined, True)
            False -> {
              let h =
                heap.write(
                  state.heap,
                  iter_ref,
                  ObjectSlot(
                    ..slot,
                    kind: value.ArrayIteratorObject(source:, index: index + 1),
                  ),
                )
              push_iter_result(
                state,
                rest_stack,
                h,
                elements.get(elems, index),
                False,
              )
            }
          }
        }
        // String iterators share %ArrayIteratorPrototype%'s next — the
        // GetIterator fast path allocates them with that prototype.
        Some(
          ObjectSlot(kind: value.StringIteratorObject(remaining:), ..) as slot,
        ) ->
          case remaining {
            [] ->
              push_iter_result(state, rest_stack, state.heap, JsUndefined, True)
            [cp, ..remaining] -> {
              let h =
                heap.write(
                  state.heap,
                  iter_ref,
                  ObjectSlot(
                    ..slot,
                    kind: value.StringIteratorObject(remaining:),
                  ),
                )
              push_iter_result(
                state,
                rest_stack,
                h,
                JsString(string.from_utf_codepoints([cp])),
                False,
              )
            }
          }
        _ -> iter_incompatible(state, "Array")
      }
    _ -> iter_incompatible(state, "Array")
  }
}

/// Allocate the [a, b] pair array yielded by "entries" iterators.
fn alloc_entry_pair(state: State, a: JsValue, b: JsValue) -> #(Heap, JsValue) {
  let #(h, pair_ref) =
    common.alloc_array(state.heap, [a, b], state.builtins.array.prototype)
  #(h, JsObject(pair_ref))
}

/// ES §24.2.5.2.1 %SetIteratorPrototype%.next()
fn call_set_iterator_next(
  state: State,
  this: JsValue,
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, State)) {
  case this {
    JsObject(iter_ref) ->
      case heap.read(state.heap, iter_ref) {
        Some(
          ObjectSlot(kind: value.SetIteratorObject(remaining:, kind:), ..) as slot,
        ) ->
          case remaining {
            [] ->
              push_iter_result(state, rest_stack, state.heap, JsUndefined, True)
            [v, ..rest] -> {
              // For "entries" yield [v, v]; for "values"/"keys" yield v.
              let #(h, yielded) = case kind {
                value.SetIterValues -> #(state.heap, v)
                value.SetIterEntries -> alloc_entry_pair(state, v, v)
              }
              let h =
                heap.write(
                  h,
                  iter_ref,
                  ObjectSlot(
                    ..slot,
                    kind: value.SetIteratorObject(remaining: rest, kind:),
                  ),
                )
              push_iter_result(state, rest_stack, h, yielded, False)
            }
          }
        _ -> iter_incompatible(state, "Set")
      }
    _ -> iter_incompatible(state, "Set")
  }
}

/// ES §24.1.5.2.1 %MapIteratorPrototype%.next()
fn call_map_iterator_next(
  state: State,
  this: JsValue,
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, State)) {
  case this {
    JsObject(iter_ref) ->
      case heap.read(state.heap, iter_ref) {
        Some(
          ObjectSlot(kind: value.MapIteratorObject(remaining:, kind:), ..) as slot,
        ) ->
          case remaining {
            [] ->
              push_iter_result(state, rest_stack, state.heap, JsUndefined, True)
            [#(k, v), ..rest] -> {
              let #(h, yielded) = case kind {
                value.MapIterKeys -> #(state.heap, k)
                value.MapIterValues -> #(state.heap, v)
                value.MapIterEntries -> alloc_entry_pair(state, k, v)
              }
              let h =
                heap.write(
                  h,
                  iter_ref,
                  ObjectSlot(
                    ..slot,
                    kind: value.MapIteratorObject(remaining: rest, kind:),
                  ),
                )
              push_iter_result(state, rest_stack, h, yielded, False)
            }
          }
        _ -> iter_incompatible(state, "Map")
      }
    _ -> iter_incompatible(state, "Map")
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
        Some(ObjectSlot(kind: NativeFunction(_, ..), properties:, ..)) -> {
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

// ============================================================================
// Native dispatch — route NativeFn to the correct builtins module
// ============================================================================

pub fn dispatch_native(
  native: value.NativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State,
  execute_inner: ExecuteInnerFn,
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
    value.ConsoleNative(n) -> builtins_console.dispatch(n, args, this, state)
    value.VmNative(value.StructuredClone) ->
      structured_clone.structured_clone(args, state)
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
    value.IteratorNative(n) -> builtins_iterator.dispatch(n, args, this, state)
    value.RegExpNative(n) -> builtins_regexp.dispatch(n, args, this, state)
    value.DateNative(n) -> builtins_date.dispatch(n, args, this, state)
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
    value.VmNative(value.DecodeURI) ->
      string_global(args, state, operators.uri_decode)
    value.VmNative(value.EncodeURI) ->
      string_global(args, state, operators.uri_encode(_, True))
    value.VmNative(value.DecodeURIComponent) ->
      string_global(args, state, operators.uri_decode)
    value.VmNative(value.EncodeURIComponent) ->
      string_global(args, state, operators.uri_encode(_, False))
    // AnnexB B.2.1.1 escape ( string )
    value.VmNative(value.Escape) ->
      string_global(args, state, operators.js_escape)
    // AnnexB B.2.1.2 unescape ( string )
    value.VmNative(value.Unescape) ->
      string_global(args, state, operators.js_unescape)
  }
}

/// Shared shape of the global String->String functions
/// (decodeURI, encodeURI, escape, ...): coerce the first arg to a string,
/// apply `f`, return the result as a JsString.
fn string_global(
  args: List(JsValue),
  state: State,
  f: fn(String) -> String,
) -> #(State, Result(JsValue, JsValue)) {
  let arg = helpers.first_arg_or_undefined(args)
  use str, state <- coerce.try_to_string(state, arg)
  #(state, Ok(JsString(f(str))))
}
