import arc/vm/builtins/array as builtins_array
import arc/vm/builtins/array_buffer as builtins_array_buffer
import arc/vm/builtins/atomics as builtins_atomics
import arc/vm/builtins/boolean as builtins_boolean
import arc/vm/builtins/common
import arc/vm/builtins/console as builtins_console
import arc/vm/builtins/data_view as builtins_data_view
import arc/vm/builtins/date as builtins_date
import arc/vm/builtins/disposable_stack as builtins_disposable_stack
import arc/vm/builtins/error as builtins_error
import arc/vm/builtins/finalization_registry as builtins_finalization_registry
import arc/vm/builtins/helpers
import arc/vm/builtins/intl as builtins_intl
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
import arc/vm/builtins/symbol as builtins_symbol
import arc/vm/builtins/temporal as builtins_temporal
import arc/vm/builtins/typed_array as builtins_typed_array
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
  GeneratorObject, GeneratorSlot, JsBool, JsObject, JsString, JsUndefined,
  JsUninitialized, Named, NativeFunction, ObjectSlot, OrdinaryObject,
}
import gleam/bool
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

// ============================================================================
// Callback types for VM functions that can't be imported directly
// ============================================================================

pub type ExecuteInnerFn(host) =
  fn(State(host)) -> Result(#(Completion(host), State(host)), VmError)

pub type UnwindToCatchFn(host) =
  fn(State(host), JsValue) -> Option(State(host))

pub type DispatchNativeFn(host) =
  fn(value.NativeFn, List(JsValue), JsValue, State(host)) ->
    #(State(host), Result(JsValue, JsValue))

// ============================================================================
// Function calling infrastructure
// ============================================================================

/// Shared logic for Call, CallMethod, and CallConstructor.
/// Looks up the callee template, saves the caller frame, sets up locals,
/// and transitions to the callee's code.
pub fn call_function(
  state: State(host),
  fn_ref: value.Ref,
  env_ref: value.Ref,
  home_object: Option(value.Ref),
  callee_template: FuncTemplate,
  args: List(JsValue),
  rest_stack: List(JsValue),
  this_val: JsValue,
  constructor_this: option.Option(JsValue),
  new_target: JsValue,
  execute_inner: ExecuteInnerFn(host),
  unwind_to_catch: UnwindToCatchFn(host),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  // §10.2.1 [[Call]] step 2: [[IsClassConstructor]] → TypeError. Construct
  // paths (new / super() / Reflect.construct) always pass an object
  // new_target; only plain calls arrive with JsUndefined.
  // Hot path: plain `case` (not result.try) so no continuation closure is
  // allocated per call.
  case callee_template.is_class_constructor && new_target == value.JsUndefined {
    True ->
      state.throw_type_error(
        state,
        "Class constructor "
          <> option.unwrap(callee_template.name, "")
          <> " cannot be invoked without 'new'",
      )
    False -> {
      let #(heap, this_val) = frame.bind_this(state, callee_template, this_val)
      let locals =
        frame.setup_locals(
          heap,
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
            State(..state, heap:),
            env_ref,
            callee_template,
            args,
            rest_stack,
            locals,
            execute_inner,
          )
        True, False ->
          call_generator_function(
            State(..state, heap:),
            env_ref,
            callee_template,
            args,
            rest_stack,
            locals,
            execute_inner,
          )
        False, True ->
          call_async_function(
            State(..state, heap:),
            env_ref,
            callee_template,
            args,
            rest_stack,
            locals,
            execute_inner,
            unwind_to_catch,
          )
        False, False ->
          // `heap` rides along separately so the callee-frame State update
          // inside call_regular_function is the only full State copy here.
          call_regular_function(
            state,
            heap,
            callee_template,
            args,
            rest_stack,
            locals,
            constructor_this,
            new_target,
          )
      }
    }
  }
}

/// Regular (non-generator) function call: save frame, enter callee.
/// `heap` is the post-bind_this heap, passed separately so the callee-frame
/// State update below is the single full State copy on this hot path.
fn call_regular_function(
  state: State(host),
  heap: Heap(host),
  callee_template: FuncTemplate,
  args: List(JsValue),
  rest_stack: List(JsValue),
  locals: tuple_array.TupleArray(JsValue),
  constructor_this: option.Option(JsValue),
  new_target: JsValue,
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  // Plain `case` (not bool.lazy_guard) — avoids a per-call closure alloc.
  case state.call_depth >= limits.max_call_depth {
    True ->
      state.throw_range_error(
        State(..state, heap:),
        "Maximum call stack size exceeded",
      )
    False -> {
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
          heap:,
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
  }
}

/// Set up an isolated execution state for a (sync or async) generator body.
fn generator_initial_state(
  state: State(host),
  callee_template: FuncTemplate,
  args: List(JsValue),
  locals: tuple_array.TupleArray(JsValue),
) -> State(host) {
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
  state: State(host),
  suspended: State(host),
  rest_stack: List(JsValue),
  slot: HeapSlot(host),
  make_kind: fn(value.Ref) -> ExoticKind(host),
  prototype: value.Ref,
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
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
  state: State(host),
  env_ref: value.Ref,
  callee_template: FuncTemplate,
  args: List(JsValue),
  rest_stack: List(JsValue),
  locals: tuple_array.TupleArray(JsValue),
  execute_inner: ExecuteInnerFn(host),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
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
  state: State(host),
  env_ref: value.Ref,
  callee_template: FuncTemplate,
  args: List(JsValue),
  rest_stack: List(JsValue),
  locals: tuple_array.TupleArray(JsValue),
  execute_inner: ExecuteInnerFn(host),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
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
  state: State(host),
  env_ref: value.Ref,
  callee_template: FuncTemplate,
  args: List(JsValue),
  rest_stack: List(JsValue),
  locals: tuple_array.TupleArray(JsValue),
  execute_inner: ExecuteInnerFn(host),
  _unwind_to_catch: UnwindToCatchFn(host),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
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
  state: State(host),
  exec_result: Result(#(Completion(host), State(host)), VmError),
  promise_data_ref: Ref,
  resolve: JsValue,
  reject: JsValue,
  func_template: FuncTemplate,
  env_ref: Ref,
  existing_slot_ref: Option(Ref),
  result_value: JsValue,
  rest_stack: List(JsValue),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
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
      // Async function completed -- resolve the outer promise through the
      // resolving function (§27.7.5.1 AsyncFunctionStart performs
      // Call(promiseCapability.[[Resolve]], ...)), so a thenable return value
      // is adopted (§27.2.1.3.2) instead of fulfilling with the thenable raw.
      let state = State(..state.merge_globals(state, final_state, []), heap: h2)
      use #(_resolved, state) <- result.try(
        state.rethrow(state.call(state, resolve, JsUndefined, [return_value])),
      )
      Ok(State(..state, stack: [result_value, ..rest_stack], pc: state.pc + 1))
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
  state: State(host),
  async_data_ref: Ref,
  awaited_value: JsValue,
) -> State(host) {
  use is_reject <- promises.setup_await(state, awaited_value)
  value.AsyncResume(async_data_ref:, is_reject:)
}

/// NativeAsyncResume handler: called when an awaited promise settles.
/// Restores the async function's execution state and continues.
pub fn call_native_async_resume(
  state: State(host),
  async_data_ref: Ref,
  is_reject: Bool,
  args: List(JsValue),
  rest_stack: List(JsValue),
  execute_inner: ExecuteInnerFn(host),
  unwind_to_catch: UnwindToCatchFn(host),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
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
  state: State(host),
  native: NativeFnSlot(host),
  args: List(JsValue),
  rest_stack: List(JsValue),
  this: JsValue,
  execute_inner: ExecuteInnerFn(host),
  unwind_to_catch: UnwindToCatchFn(host),
  dispatch_fn: DispatchNativeFn(host),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
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
    // Function.prototype.apply(thisArg, argsArray) — §20.2.3.1
    // `this` is the target function, args[0] is thisArg, args[1] is array
    value.Call(value.FunctionApply) -> {
      let this_arg = case args {
        [t, ..] -> t
        _ -> JsUndefined
      }
      // Step 3: undefined/null argArray → no args. Step 4:
      // ? CreateListFromArrayLike(argArray) — TypeError for other
      // non-objects, element/length reads go through [[Get]].
      use #(call_args, state) <- result.try(case args {
        [] | [_] -> Ok(#([], state))
        [_, JsUndefined, ..] | [_, value.JsNull, ..] -> Ok(#([], state))
        [_, arg_array, ..] ->
          state.rethrow(list_from_array_like(state, arg_array))
      })
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
    // Function.prototype.bind(thisArg, ...args) — §20.2.3.2
    // Creates a bound function object
    value.Call(value.FunctionBind) -> {
      let #(this_arg, bound_args) = case args {
        [t, ..rest] -> #(t, rest)
        [] -> #(JsUndefined, [])
      }
      // Step 2: If IsCallable(Target) is false, throw a TypeError.
      case helpers.is_callable(state.heap, this), this {
        True, JsObject(target_ref) -> {
          // Steps 4-6: L = 0 unless Target has an own "length" whose value
          // is a Number: L = max(ToIntegerOrInfinity(targetLen) - nArgs, 0).
          // The Get goes through accessors/proxies and can throw
          // (test262: bind/instance-length-* , bind/get-fn-length-error).
          use #(target_len, state) <- result.try(
            case
              object.get_own_property(state.heap, target_ref, Named("length"))
            {
              Some(_) ->
                state.rethrow(object.get_value(
                  state,
                  target_ref,
                  Named("length"),
                  this,
                ))
              None -> Ok(#(JsUndefined, state))
            },
          )
          let num_args = list.length(bound_args)
          let length_js = case target_len {
            value.JsNumber(value.Infinity) -> value.JsNumber(value.Infinity)
            value.JsNumber(value.Finite(f)) ->
              value.from_int(int.max(float.truncate(f) - num_args, 0))
            // NaN / -Infinity / non-Number → 0 (steps 4, 6.b.ii).
            _ -> value.from_int(0)
          }
          // Steps 8-10: targetName = ? Get(Target, "name"); non-strings
          // become "" (test262: bind/instance-name-error, instance-name-non-string).
          use #(target_name, state) <- result.try(
            state.rethrow(object.get_value(
              state,
              target_ref,
              Named("name"),
              this,
            )),
          )
          let name = case target_name {
            JsString(n) -> "bound " <> n
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
              props: [
                // seq: 0 — birth-time "length", pairs with fn_name_property's
                // constant seq 1 (see common.fn_name_property).
                #(
                  "length",
                  value.DataProperty(
                    value: length_js,
                    writable: False,
                    enumerable: False,
                    configurable: True,
                    seq: 0,
                  ),
                ),
                #("name", common.fn_name_property(name)),
              ],
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
        _, _ -> {
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
      promises.call_native_promise_resolve_static(state, this, args, rest_stack)
    // Promise.reject(reason)
    value.Call(value.PromiseRejectStatic) ->
      promises.call_native_promise_reject_static(state, this, args, rest_stack)
    // Promise.all(iterable)
    value.Call(value.PromiseAllStatic) ->
      promises.call_native_promise_all(state, this, args, rest_stack)
    // Promise.race(iterable)
    value.Call(value.PromiseRaceStatic) ->
      promises.call_native_promise_race(state, this, args, rest_stack)
    // Promise.allSettled(iterable)
    value.Call(value.PromiseAllSettledStatic) ->
      promises.call_native_promise_all_settled(state, this, args, rest_stack)
    // Promise.any(iterable)
    value.Call(value.PromiseAnyStatic) ->
      promises.call_native_promise_any(state, this, args, rest_stack)
    // Promise.allKeyed(promises) — await-dictionary proposal
    value.Call(value.PromiseAllKeyedStatic) ->
      promises.call_native_promise_all_keyed(
        state,
        this,
        args,
        rest_stack,
        settled: False,
      )
    // Promise.allSettledKeyed(promises) — await-dictionary proposal
    value.Call(value.PromiseAllSettledKeyedStatic) ->
      promises.call_native_promise_all_keyed(
        state,
        this,
        args,
        rest_stack,
        settled: True,
      )
    // Promise.allKeyed / Promise.allSettledKeyed per-element handler
    value.Call(value.PromiseKeyedElement(
      index:,
      remaining_ref:,
      keys_ref:,
      values_ref:,
      already_called_ref:,
      resolve:,
      status_field:,
    )) ->
      promises.call_native_promise_keyed_element(
        state,
        args,
        rest_stack,
        index,
        remaining_ref,
        keys_ref,
        values_ref,
        already_called_ref,
        resolve,
        status_field,
      )
    // GetCapabilitiesExecutor for NewPromiseCapability with a custom constructor
    value.Call(value.PromiseCapabilityExecutor(resolve_box:, reject_box:)) ->
      promises.call_native_promise_capability_executor(
        state,
        resolve_box,
        reject_box,
        args,
        rest_stack,
      )
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
        unwind_to_catch,
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
    // Array.fromAsync and its await-continuation closures
    value.Call(value.ArrayFromAsync) ->
      promises.call_native_array_from_async(state, this, args, rest_stack)
    value.Call(value.ArrayFromAsyncOnNext(ctx:)) ->
      promises.call_native_from_async_on_next(state, ctx, args, rest_stack)
    value.Call(value.ArrayFromAsyncOnMapped(ctx:)) ->
      promises.call_native_from_async_on_mapped(state, ctx, args, rest_stack)
    value.Call(value.ArrayFromAsyncCloseReject(iter:, reject:)) ->
      promises.call_native_from_async_close_reject(
        state,
        iter,
        reject,
        args,
        rest_stack,
      )
    value.Call(value.ArrayFromAsyncRejectWith(error:, reject:)) ->
      promises.call_native_from_async_reject_with(
        state,
        error,
        reject,
        rest_stack,
      )
    value.Call(value.ArrayFromAsyncLikeOnValue(ctx:)) ->
      promises.call_native_from_async_like_on_value(
        state,
        ctx,
        args,
        rest_stack,
      )
    value.Call(value.ArrayFromAsyncLikeOnMapped(ctx:)) ->
      promises.call_native_from_async_like_on_mapped(
        state,
        ctx,
        args,
        rest_stack,
      )
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
    // §28.2.1: Proxy called without `new` throws TypeError.
    value.Call(value.ProxyConstructor) ->
      state.throw_type_error(state, "Constructor Proxy requires 'new'")
    // §28.2.2.1 Proxy.revocable(target, handler).
    value.Call(value.ProxyRevocable) ->
      case proxy_create(state, args) {
        Error(msg) -> state.throw_type_error(state, msg)
        Ok(#(h, proxy_ref)) -> {
          // Step 2: revoker closure with [[RevocableProxy]] = proxy.
          let #(h, revoke_ref) =
            common.alloc_call_fn(
              h,
              state.builtins.function.prototype,
              value.ProxyRevoke(proxy_ref),
              "",
              0,
              constructible: False,
            )
          // Steps 3-6: result object { proxy, revoke }.
          let #(h, result_ref) =
            common.alloc_pojo(h, state.builtins.object.prototype, [
              #("proxy", value.data_property(JsObject(proxy_ref))),
              #("revoke", value.data_property(JsObject(revoke_ref))),
            ])
          Ok(
            State(
              ..state,
              heap: h,
              stack: [JsObject(result_ref), ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        }
      }
    // The revoke closure from Proxy.revocable — nulls [[ProxyTarget]] and
    // [[ProxyHandler]]; idempotent; returns undefined.
    value.Call(value.ProxyRevoke(proxy_ref)) -> {
      let h = {
        use slot <- heap.update(state.heap, proxy_ref)
        case slot {
          ObjectSlot(kind: value.ProxyObject(callable:, constructable:, ..), ..) ->
            ObjectSlot(
              ..slot,
              kind: value.ProxyObject(
                target: None,
                handler: None,
                callable:,
                constructable:,
              ),
            )
          _ -> slot
        }
      }
      Ok(
        State(
          ..state,
          heap: h,
          stack: [JsUndefined, ..rest_stack],
          pc: state.pc + 1,
        ),
      )
    }
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
    // §20.4.3.3 Symbol.prototype.toString — SymbolDescriptiveString(thisSymbolValue)
    value.Call(value.SymbolPrototypeToString) -> {
      use #(id, state) <- result.try(this_symbol_value(state, this, "toString"))
      let s =
        builtins_symbol.descriptive_string(id, state.ctx.symbol_descriptions)
      Ok(State(..state, stack: [JsString(s), ..rest_stack], pc: state.pc + 1))
    }
    // §20.4.3.4 Symbol.prototype.valueOf / §20.4.3.5 @@toPrimitive — both
    // return thisSymbolValue (toPrimitive ignores its hint argument).
    value.Call(value.SymbolPrototypeValueOf) -> {
      use #(id, state) <- result.try(this_symbol_value(state, this, "valueOf"))
      Ok(
        State(
          ..state,
          stack: [value.JsSymbol(id), ..rest_stack],
          pc: state.pc + 1,
        ),
      )
    }
    value.Call(value.SymbolPrototypeToPrimitive) -> {
      use #(id, state) <- result.try(this_symbol_value(
        state,
        this,
        "[Symbol.toPrimitive]",
      ))
      Ok(
        State(
          ..state,
          stack: [value.JsSymbol(id), ..rest_stack],
          pc: state.pc + 1,
        ),
      )
    }
    // §20.4.3.2 get Symbol.prototype.description — [[Description]] or undefined
    value.Call(value.SymbolDescriptionGetter) -> {
      use #(id, state) <- result.try(this_symbol_value(
        state,
        this,
        "description",
      ))
      let val = case value.well_known_symbol_description(id) {
        option.Some(s) -> JsString(s)
        option.None ->
          case dict.get(state.ctx.symbol_descriptions, id) {
            Ok(s) -> JsString(s)
            Error(Nil) -> JsUndefined
          }
      }
      Ok(State(..state, stack: [val, ..rest_stack], pc: state.pc + 1))
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
  state: State(host),
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
  state: State(host),
  ctor_ref: Ref,
  args: List(JsValue),
  rest_stack: List(JsValue),
  new_target_ref: Ref,
  execute_inner: ExecuteInnerFn(host),
  unwind_to_catch: UnwindToCatchFn(host),
  dispatch_fn: DispatchNativeFn(host),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
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
          // §10.1.13.2 performs ? Get(newTarget, "prototype"), observable
          // for proxy newTargets — route those through [[Get]].
          use #(proto, state) <- result.try(
            state.rethrow(prototype_from_new_target_stateful(
              state,
              new_target_ref,
            )),
          )
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
    // §28.2.1 new Proxy(target, handler) — ProxyCreate.
    Some(ObjectSlot(
      kind: NativeFunction(value.Call(value.ProxyConstructor), ..),
      ..,
    )) ->
      case proxy_create(state, args) {
        Error(msg) -> state.throw_type_error(state, msg)
        Ok(#(h, proxy_ref)) ->
          Ok(
            State(
              ..state,
              heap: h,
              stack: [JsObject(proxy_ref), ..rest_stack],
              pc: state.pc + 1,
            ),
          )
      }
    // §10.5.13 Proxy [[Construct]] ( argumentsList, newTarget ).
    Some(ObjectSlot(kind: value.ProxyObject(target:, handler:, ..), ..)) -> {
      // Steps 1-5: revocation check + GetMethod(handler, "construct").
      use #(t, h, trap, state) <- result.try(
        state.rethrow(object.proxy_trap(state, target, handler, "construct")),
      )
      case trap {
        // Step 7: no trap → Construct(target, argumentsList, newTarget).
        None ->
          do_construct(
            state,
            t,
            args,
            rest_stack,
            new_target_ref,
            execute_inner,
            unwind_to_catch,
            dispatch_fn,
          )
        Some(trap_fn) -> {
          // Steps 8-9: Call(trap, handler, « target, argArray, newTarget »).
          let #(heap, args_arr) =
            common.alloc_array(state.heap, args, state.builtins.array.prototype)
          let state = State(..state, heap:)
          use #(res, state) <- result.try(
            state.rethrow(
              state.call(state, trap_fn, JsObject(h), [
                JsObject(t),
                JsObject(args_arr),
                JsObject(new_target_ref),
              ]),
            ),
          )
          // Step 10: If newObj is not an Object, throw a TypeError.
          case res {
            JsObject(_) ->
              Ok(State(..state, stack: [res, ..rest_stack], pc: state.pc + 1))
            _ ->
              state.throw_type_error(
                state,
                "'construct' on proxy: trap returned non-object",
              )
          }
        }
      }
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
      let coerced = case args {
        [] -> Ok(#(value.Finite(0.0), state))
        [v, ..] -> coerce.js_to_number(state, v)
      }
      case coerced {
        Error(#(thrown, st)) -> Error(#(Thrown, thrown, st))
        Ok(#(n, state)) ->
          push_wrapper(
            state,
            rest_stack,
            value.NumberObject(n),
            new_target_ref,
            state.builtins.number.prototype,
          )
      }
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
    // §20.1.1.1 step 1: when NewTarget is neither undefined nor the active
    // function (subclass `super()`, or Reflect.construct with a different
    // newTarget), Object ignores its argument entirely and returns
    // OrdinaryCreateFromConstructor(NewTarget, %Object.prototype%). This must
    // NOT fall through to the generic native path: Object(value) returns the
    // argument's identity (ToObject), and the generic re-prototyping step
    // would silently rewrite a pre-existing object's [[Prototype]].
    Some(ObjectSlot(
      kind: NativeFunction(
        value.Dispatch(value.ObjectNative(value.ObjectConstructor)),
        ..,
      ),
      ..,
    ))
      if new_target_ref != ctor_ref
    -> {
      use #(proto, state) <- result.try(
        state.rethrow(prototype_from_new_target_stateful(state, new_target_ref)),
      )
      let #(heap, ref) =
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
      Ok(
        State(
          ..state,
          heap:,
          stack: [JsObject(ref), ..rest_stack],
          pc: state.pc + 1,
        ),
      )
    }
    Some(ObjectSlot(kind: NativeFunction(native, ..), ..)) ->
      case object.is_constructor(state.heap, JsObject(ctor_ref)) {
        True -> {
          // Thread newTarget for the native body and re-prototype the result so
          // subclassing builtins (`class M extends Map {}`) gets the derived
          // prototype. Only re-prototype when newTarget actually has its own
          // .prototype data property — natives that should honour a custom
          // newTarget themselves (or whose newTarget.prototype is an accessor)
          // are left untouched so the native's own choice stands.
          // Restore the caller's newTarget on BOTH completion paths — a
          // throwing native constructor must not leak its newTarget into
          // subsequent plain calls (which check it to reject call-without-new).
          use new_state <- result.try(
            call_native(
              State(..state, new_target:),
              native,
              args,
              rest_stack,
              JsUndefined,
              execute_inner,
              unwind_to_catch,
              dispatch_fn,
            )
            |> result.map_error(fn(err) {
              let #(step, thrown, err_state) = err
              #(step, thrown, State(..err_state, new_target: state.new_target))
            }),
          )
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

/// §10.1.13.2 GetPrototypeFromConstructor for possibly-proxy newTargets:
/// `? Get(newTarget, "prototype")` through the proxy's get trap; ordinary
/// newTargets use the fast slot read.
fn prototype_from_new_target_stateful(
  state: State(host),
  new_target_ref: Ref,
) -> Result(#(Option(Ref), State(host)), #(JsValue, State(host))) {
  case object.as_proxy(state.heap, new_target_ref) {
    Some(_) -> {
      use #(proto_val, state) <- result.map(object.get_value(
        state,
        new_target_ref,
        Named("prototype"),
        JsObject(new_target_ref),
      ))
      case proto_val {
        JsObject(p) -> #(Some(p), state)
        _ -> #(Some(state.builtins.object.prototype), state)
      }
    }
    None -> Ok(#(prototype_from_new_target(state, new_target_ref), state))
  }
}

/// §10.1.13.2 GetPrototypeFromConstructor: read newTarget.prototype, fall
/// back to %Object.prototype% if not an object.
fn prototype_from_new_target(
  state: State(host),
  new_target_ref: Ref,
) -> Option(Ref) {
  Some(prototype_from_new_target_or(
    state,
    new_target_ref,
    state.builtins.object.prototype,
  ))
}

/// Read newTarget's own `.prototype` only when it's a DataProperty pointing
/// at an object — accessor prototypes and missing slots return None so the
/// caller can leave the native ctor's own prototype choice in place.
fn own_data_prototype(h: Heap(host), new_target_ref: Ref) -> Option(Ref) {
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
  state: State(host),
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

fn set_prototype(h: Heap(host), ref: Ref, proto: Option(Ref)) -> Heap(host) {
  case heap.read(h, ref) {
    Some(ObjectSlot(..) as slot) ->
      heap.write(h, ref, ObjectSlot(..slot, prototype: proto))
    _ -> h
  }
}

/// Call an arbitrary JsValue as a function with the given this and args.
/// Used by Function.prototype.call/apply and bound function invocation.
pub fn call_value(
  state: State(host),
  callee: JsValue,
  args: List(JsValue),
  this_val: JsValue,
  execute_inner: ExecuteInnerFn(host),
  unwind_to_catch: UnwindToCatchFn(host),
  dispatch_fn: DispatchNativeFn(host),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
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
        // §10.5.12 Proxy [[Call]] ( thisArgument, argumentsList ).
        Some(ObjectSlot(
          kind: value.ProxyObject(target:, handler:, callable:, ..),
          ..,
        )) ->
          case callable {
            False ->
              state.throw_type_error(
                state,
                object.inspect(callee, state.heap) <> " is not a function",
              )
            True -> {
              // Steps 1-5: revocation check + GetMethod(handler, "apply").
              use #(t, h, trap, state) <- result.try(
                state.rethrow(object.proxy_trap(state, target, handler, "apply")),
              )
              case trap {
                // Step 7: no trap → Call(target, thisArgument, argumentsList).
                None ->
                  call_value(
                    state,
                    JsObject(t),
                    args,
                    this_val,
                    execute_inner,
                    unwind_to_catch,
                    dispatch_fn,
                  )
                // Steps 8-9: Call(trap, handler, « target, thisArg, argArray »).
                Some(trap_fn) -> {
                  let #(heap, args_arr) =
                    common.alloc_array(
                      state.heap,
                      args,
                      state.builtins.array.prototype,
                    )
                  call_value(
                    State(..state, heap:),
                    trap_fn,
                    [JsObject(t), this_val, JsObject(args_arr)],
                    JsObject(h),
                    execute_inner,
                    unwind_to_catch,
                    dispatch_fn,
                  )
                }
              }
            }
          }
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

/// §10.5.15 ProxyCreate ( target, handler ) — validates both are objects and
/// allocates the proxy exotic object. The [[Call]]/[[Construct]] capabilities
/// are snapshotted from the target (steps 4-7).
fn proxy_create(
  state: State(host),
  args: List(JsValue),
) -> Result(#(Heap(host), Ref), String) {
  case args {
    [JsObject(t), JsObject(handler_ref), ..] -> {
      let callable = object.value_is_callable(state.heap, JsObject(t))
      let constructable = object.is_constructor(state.heap, JsObject(t))
      Ok(heap.alloc(
        state.heap,
        ObjectSlot(
          kind: value.ProxyObject(
            target: Some(t),
            handler: Some(handler_ref),
            callable:,
            constructable:,
          ),
          properties: dict.new(),
          elements: elements.new(),
          // Proxies have no own [[Prototype]] — [[GetPrototypeOf]] is
          // always answered via the trap machinery, never this field.
          prototype: None,
          symbol_properties: [],
          extensible: True,
        ),
      ))
    }
    _ -> Error("Cannot create proxy with a non-object as target or handler")
  }
}

/// Extract elements from an array object as a list of JsValues.
/// Used by Function.prototype.apply to unpack the args tuple_array.
/// §7.3.19 CreateListFromArrayLike ( obj ) — stateful: the "length" and
/// element reads go through [[Get]] (accessor properties, proxy traps,
/// typed-array indices) and can throw (test262: Function/prototype/apply/
/// get-length-abrupt, get-index-abrupt, argarray-not-object, resizable-buffer).
fn list_from_array_like(
  state: State(host),
  arg: JsValue,
) -> Result(#(List(JsValue), State(host)), #(JsValue, State(host))) {
  case arg {
    JsObject(ref) -> {
      // Step 3: Let len be ? LengthOfArrayLike(obj) = ToLength(? Get(obj, "length")).
      use #(len_val, state) <- result.try(object.get_value(
        state,
        ref,
        Named("length"),
        arg,
      ))
      use #(len_num, state) <- result.try(coerce.js_to_number(state, len_val))
      let len = case len_num {
        value.Finite(f) -> int.max(float.truncate(f), 0)
        value.Infinity -> limits.max_safe_integer
        _ -> 0
      }
      case len > limits.max_iteration {
        True ->
          Error(state.range_error_value(
            state,
            "Too many arguments in function call",
          ))
        False -> gather_args_stateful(state, ref, arg, 0, len, [])
      }
    }
    _ ->
      Error(state.type_error_value(
        state,
        "CreateListFromArrayLike called on non-object",
      ))
  }
}

fn gather_args_stateful(
  state: State(host),
  ref: Ref,
  receiver: JsValue,
  idx: Int,
  len: Int,
  acc: List(JsValue),
) -> Result(#(List(JsValue), State(host)), #(JsValue, State(host))) {
  case idx >= len {
    True -> Ok(#(list.reverse(acc), state))
    False -> {
      // Step 7.b-c: Let next be ? Get(obj, ToString(F(index))).
      use #(v, state) <- result.try(object.get_value(
        state,
        ref,
        value.Index(idx),
        receiver,
      ))
      gather_args_stateful(state, ref, receiver, idx + 1, len, [v, ..acc])
    }
  }
}

pub fn extract_array_args(h: Heap(host), ref: Ref) -> List(JsValue) {
  heap.read_array_like(h, ref)
  |> option.map(fn(p) { elements.to_list_padded(p.1, p.0) })
  |> option.unwrap([])
}

/// Allocate a {value, done} iterator-result object, push it, advance pc.
fn push_iter_result(
  state: State(host),
  rest_stack: List(JsValue),
  h: Heap(host),
  val: JsValue,
  done: Bool,
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  let #(h, result) = common.create_iter_result(h, state.builtins, val, done)
  Ok(State(..state, heap: h, stack: [result, ..rest_stack], pc: state.pc + 1))
}

fn iter_incompatible(
  state: State(host),
  tag: String,
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  state.throw_type_error(
    state,
    tag <> " Iterator next called on incompatible receiver",
  )
}

/// ES §23.1.5.2.1 %ArrayIteratorPrototype%.next()
fn call_array_iterator_next(
  state: State(host),
  this: JsValue,
  rest_stack: List(JsValue),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  case this {
    JsObject(iter_ref) ->
      case heap.read(state.heap, iter_ref) {
        // index < 0 latches exhaustion: once the iterator reported done it
        // stays done even if the source later grows or its buffer is resized
        // (spec sets [[IteratedObject]] to undefined — "generator already
        // returned" state).
        Some(ObjectSlot(kind: value.ArrayIteratorObject(index:, ..), ..))
          if index < 0
        -> push_iter_result(state, rest_stack, state.heap, JsUndefined, True)
        Some(
          ObjectSlot(
            kind: value.ArrayIteratorObject(source:, index:, iter_kind:),
            ..,
          ) as slot,
        ) ->
          case heap.read(state.heap, source) {
            // Typed-array source — §23.1.5.1: re-validate the buffer witness
            // and read the element through the live backing store each step
            // (mutation during iteration is observed; detached/out-of-bounds
            // views throw TypeError).
            Some(ObjectSlot(
              kind: value.TypedArrayObject(
                buffer:,
                elem_kind:,
                byte_offset:,
                length:,
              ),
              ..,
            )) ->
              case
                object.typed_array_iter_length(
                  state.heap,
                  buffer,
                  elem_kind,
                  byte_offset,
                  length,
                )
              {
                Error(msg) -> state.throw_type_error(state, msg)
                Ok(len) ->
                  case index >= len {
                    True -> {
                      let h =
                        bump_array_iter_index(state.heap, iter_ref, source, -1)
                      push_iter_result(state, rest_stack, h, JsUndefined, True)
                    }
                    False -> {
                      let elem =
                        object.typed_array_element(
                          state.heap,
                          buffer,
                          elem_kind,
                          byte_offset,
                          len,
                          index,
                        )
                        |> option.unwrap(JsUndefined)
                      let h =
                        heap.write(
                          state.heap,
                          iter_ref,
                          ObjectSlot(
                            ..slot,
                            kind: value.ArrayIteratorObject(
                              source:,
                              index: index + 1,
                              iter_kind:,
                            ),
                          ),
                        )
                      let #(h, out) =
                        array_iter_out(h, state, iter_kind, index, elem)
                      push_iter_result(state, rest_stack, h, out, False)
                    }
                  }
              }
            _ ->
              step_array_iterator_generic(
                state,
                iter_ref,
                source,
                index,
                iter_kind,
                rest_stack,
              )
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

/// %ArrayIteratorPrototype%.next() step for plain-array/arguments and Proxy
/// sources (§23.1.5.1 generic path).
fn step_array_iterator_generic(
  state: State(host),
  iter_ref: value.Ref,
  source: value.Ref,
  index: Int,
  iter_kind: value.ArrayIterKind,
  rest_stack: List(JsValue),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  case object.as_proxy(state.heap, source) {
    // Proxy source — §23.1.5.1 generic path: ? Get(array, "length")
    // and ? Get(array, ToString(index)) fire the proxy's get trap.
    Some(_) -> {
      use #(len_val, state) <- result.try(
        state.rethrow(object.get_value(
          state,
          source,
          Named("length"),
          JsObject(source),
        )),
      )
      let length = case value.to_number(len_val) {
        Ok(value.Finite(f)) -> int.max(0, value.float_to_int(f))
        _ -> 0
      }
      case index >= length {
        True -> {
          let h = bump_array_iter_index(state.heap, iter_ref, source, -1)
          push_iter_result(state, rest_stack, h, JsUndefined, True)
        }
        False -> {
          // §23.1.5.1 step 8.b.iii: a "key" iterator yields the index
          // WITHOUT performing Get(array, elementKey) — no get trap fires.
          use #(out, state) <- result.try(case iter_kind {
            value.ArrayIterKeys -> Ok(#(value.from_int(index), state))
            _ -> {
              use #(elem, state) <- result.try(
                state.rethrow(object.get_value(
                  state,
                  source,
                  value.Index(index),
                  JsObject(source),
                )),
              )
              let #(h, out) =
                array_iter_out(state.heap, state, iter_kind, index, elem)
              Ok(#(out, State(..state, heap: h)))
            }
          })
          let h = bump_array_iter_index(state.heap, iter_ref, source, index + 1)
          push_iter_result(state, rest_stack, h, out, False)
        }
      }
    }
    None -> {
      let #(length, elems) =
        heap.read_array_like(state.heap, source)
        |> option.unwrap(#(0, elements.new()))
      case index >= length {
        True -> {
          let h = bump_array_iter_index(state.heap, iter_ref, source, -1)
          push_iter_result(state, rest_stack, h, JsUndefined, True)
        }
        False -> {
          // §23.1.5.1: Let elementValue be ? Get(array, elementKey).
          // The elements store is only a fast path for plain data
          // values — defineProperty can install accessor/attribute
          // overrides at an index (kept in the properties dict), and
          // holes consult the prototype chain, so fall back to the
          // generic [[Get]] when the element store has no entry or
          // an override exists. "key" iterators skip the Get entirely
          // (§23.1.5.1 step 8.b.iii).
          use #(out, state) <- result.try(case iter_kind {
            value.ArrayIterKeys -> Ok(#(value.from_int(index), state))
            _ -> {
              use #(elem, state) <- result.try(
                case
                  element_without_override(state.heap, source, elems, index)
                {
                  Some(v) -> Ok(#(v, state))
                  None ->
                    state.rethrow(object.get_value(
                      state,
                      source,
                      value.Index(index),
                      JsObject(source),
                    ))
                },
              )
              let #(h, out) =
                array_iter_out(state.heap, state, iter_kind, index, elem)
              Ok(#(out, State(..state, heap: h)))
            }
          })
          let h = bump_array_iter_index(state.heap, iter_ref, source, index + 1)
          push_iter_result(state, rest_stack, h, out, False)
        }
      }
    }
  }
}

/// §23.1.5.1: shape one iteration result for the iterator's kind — the
/// index ("key"), the element ("value"), or a fresh [index, element] pair
/// array ("key+value").
fn array_iter_out(
  h: state.Heap(host),
  state: State(host),
  iter_kind: value.ArrayIterKind,
  index: Int,
  elem: JsValue,
) -> #(state.Heap(host), JsValue) {
  case iter_kind {
    value.ArrayIterKeys -> #(h, value.from_int(index))
    value.ArrayIterValues -> #(h, elem)
    value.ArrayIterEntries -> {
      let #(h, pair_ref) =
        common.alloc_array(
          h,
          [value.from_int(index), elem],
          state.builtins.array.prototype,
        )
      #(h, JsObject(pair_ref))
    }
  }
}

/// Bump an ArrayIteratorObject's index in place (preserving the iteration
/// kind). No-op if the ref no longer holds an array-iterator slot.
fn bump_array_iter_index(
  h: state.Heap(host),
  iter_ref: value.Ref,
  source: value.Ref,
  index: Int,
) -> state.Heap(host) {
  case heap.read(h, iter_ref) {
    Some(
      ObjectSlot(kind: value.ArrayIteratorObject(iter_kind:, ..), ..) as slot,
    ) ->
      heap.write(
        h,
        iter_ref,
        ObjectSlot(
          ..slot,
          kind: value.ArrayIteratorObject(source:, index:, iter_kind:),
        ),
      )
    _ -> h
  }
}

/// Fast-path element read for the array iterator: Some(value) only when the
/// index is a plain data value in the element store with no properties-dict
/// override (defineProperty can install accessor/attribute overrides at an
/// index). None → caller takes the generic [[Get]] path.
fn element_without_override(
  h: state.Heap(host),
  source: value.Ref,
  elems: value.JsElements,
  index: Int,
) -> Option(JsValue) {
  case heap.read(h, source) {
    Some(ObjectSlot(properties:, ..)) ->
      case dict.has_key(properties, value.Index(index)) {
        True -> None
        False -> elements.get_option(elems, index)
      }
    _ -> None
  }
}

/// Allocate the [a, b] pair array yielded by "entries" iterators.
fn alloc_entry_pair(
  state: State(host),
  a: JsValue,
  b: JsValue,
) -> #(Heap(host), JsValue) {
  let #(h, pair_ref) =
    common.alloc_array(state.heap, [a, b], state.builtins.array.prototype)
  #(h, JsObject(pair_ref))
}

/// ES §24.2.5.2.1 %SetIteratorPrototype%.next() — LIVE iteration by seq
/// cursor: yield the source Set's first live record at seq >= cursor and
/// resume at seq + 1 (amortized O(1) per step). Entries added during
/// iteration — including delete + re-add, which assigns a fresh seq — are
/// visited; entries deleted before being reached leave a gap that's skipped.
fn call_set_iterator_next(
  state: State(host),
  this: JsValue,
  rest_stack: List(JsValue),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  case this {
    JsObject(iter_ref) ->
      case heap.read(state.heap, iter_ref) {
        Some(
          ObjectSlot(
            kind: value.SetIteratorObject(source:, cursor:, done:, kind:),
            ..,
          ) as slot,
        ) -> {
          use <- bool.lazy_guard(done, fn() {
            push_iter_result(state, rest_stack, state.heap, JsUndefined, True)
          })
          let next = case heap.read(state.heap, source) {
            Some(ObjectSlot(
              kind: value.SetObject(data:, order:, next_seq:, ..),
              ..,
            )) -> value.entry_from_seq(data, order, cursor, next_seq)
            _ -> None
          }
          case next {
            None -> {
              let h =
                heap.write(
                  state.heap,
                  iter_ref,
                  ObjectSlot(
                    ..slot,
                    kind: value.SetIteratorObject(
                      source:,
                      cursor:,
                      done: True,
                      kind:,
                    ),
                  ),
                )
              push_iter_result(state, rest_stack, h, JsUndefined, True)
            }
            Some(#(seq, _k, v)) -> {
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
                    kind: value.SetIteratorObject(
                      source:,
                      cursor: seq + 1,
                      done: False,
                      kind:,
                    ),
                  ),
                )
              push_iter_result(state, rest_stack, h, yielded, False)
            }
          }
        }
        _ -> iter_incompatible(state, "Set")
      }
    _ -> iter_incompatible(state, "Set")
  }
}

/// ES §24.1.5.2.1 %MapIteratorPrototype%.next() — same LIVE cursor design as
/// call_set_iterator_next.
fn call_map_iterator_next(
  state: State(host),
  this: JsValue,
  rest_stack: List(JsValue),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  case this {
    JsObject(iter_ref) ->
      case heap.read(state.heap, iter_ref) {
        Some(
          ObjectSlot(
            kind: value.MapIteratorObject(source:, cursor:, done:, kind:),
            ..,
          ) as slot,
        ) -> {
          use <- bool.lazy_guard(done, fn() {
            push_iter_result(state, rest_stack, state.heap, JsUndefined, True)
          })
          let next = case heap.read(state.heap, source) {
            Some(ObjectSlot(
              kind: value.MapObject(entries:, order:, next_seq:, ..),
              ..,
            )) -> value.entry_from_seq(entries, order, cursor, next_seq)
            _ -> None
          }
          case next {
            None -> {
              let h =
                heap.write(
                  state.heap,
                  iter_ref,
                  ObjectSlot(
                    ..slot,
                    kind: value.MapIteratorObject(
                      source:,
                      cursor:,
                      done: True,
                      kind:,
                    ),
                  ),
                )
              push_iter_result(state, rest_stack, h, JsUndefined, True)
            }
            Some(#(seq, k, v)) -> {
              let #(h, yielded) = case kind {
                value.MapIterKeys -> #(state.heap, value.map_key_to_js(k))
                value.MapIterValues -> #(state.heap, v)
                value.MapIterEntries ->
                  alloc_entry_pair(state, value.map_key_to_js(k), v)
              }
              let h =
                heap.write(
                  h,
                  iter_ref,
                  ObjectSlot(
                    ..slot,
                    kind: value.MapIteratorObject(
                      source:,
                      cursor: seq + 1,
                      done: False,
                      kind:,
                    ),
                  ),
                )
              push_iter_result(state, rest_stack, h, yielded, False)
            }
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
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
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
        Some(ObjectSlot(kind: NativeFunction(slot, ..), properties:, ..)) -> {
          // §20.2.3.5 step 3: bound functions get an implementation-defined
          // NativeFunction string. Like V8, omit the "bound f" name — it is
          // not a valid NativeFunction IdentifierName.
          let name = case slot {
            value.Call(value.BoundFunction(..)) -> ""
            _ ->
              dict.get(properties, Named("name"))
              |> result.map(fn(p) {
                case p {
                  DataProperty(value: JsString(n), ..) -> n
                  _ -> ""
                }
              })
              |> result.unwrap("")
          }
          #(state, Ok(JsString("function " <> name <> "() { [native code] }")))
        }
        // §20.2.3.5 step 4: any other object with a [[Call]] internal method
        // (callable proxies) also gets a NativeFunction string.
        Some(ObjectSlot(kind: value.ProxyObject(callable: True, ..), ..)) -> #(
          state,
          Ok(JsString("function () { [native code] }")),
        )
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

/// §20.2.3.6 Function.prototype [ @@hasInstance ] ( V )
fn function_has_instance(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let v = case args {
    [first, ..] -> first
    [] -> JsUndefined
  }
  // OrdinaryHasInstance step 1: If IsCallable(C) is false, return false.
  case this {
    JsObject(ref) ->
      case helpers.is_callable(state.heap, this) {
        True ->
          case coerce.ordinary_has_instance(state, ref, v) {
            Ok(#(result, state)) -> #(state, Ok(JsBool(result)))
            Error(#(err, state)) -> #(state, Error(err))
          }
        False -> #(state, Ok(JsBool(False)))
      }
    _ -> #(state, Ok(JsBool(False)))
  }
}

/// §10.2.4.1 %ThrowTypeError%, with the V8/JSC legacy relaxation: reading
/// "caller"/"arguments" through Function.prototype's restricted accessor on
/// a NON-strict plain function yields undefined instead of throwing (V8
/// returns null, JSC undefined; test262 features:[caller] tests accept
/// undefined — see language/arguments-object/10.6-13-a-2.js "Extension not
/// supported - fake it"). Everything else still throws: strict functions,
/// class constructors (always strict), arrows / generators / async functions
/// / methods (is_constructor=False, matching V8's non-"legacy" function
/// kinds), bound and native functions, and arguments objects.
fn restricted_function_property(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let is_legacy_function = case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: FunctionObject(func_template:, ..), ..)) ->
          !func_template.is_strict && func_template.is_constructor
        _ -> False
      }
    _ -> False
  }
  case is_legacy_function {
    True -> #(state, Ok(JsUndefined))
    False ->
      state.type_error(
        state,
        "'caller', 'callee', and 'arguments' properties may not be "
          <> "accessed on strict mode functions or the arguments objects "
          <> "for calls to them",
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
  state: State(host),
  execute_inner: ExecuteInnerFn(host),
  new_state_fn: realm.NewStateFn(host),
) -> #(State(host), Result(JsValue, JsValue)) {
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
    value.VmNative(value.EvalScript) ->
      realm.eval_script_native(args, this, state, execute_inner, new_state_fn)
    value.VmNative(value.CreateRealm) -> realm.create_realm_native(this, state)
    value.VmNative(value.Gc) -> #(state, Ok(JsUndefined))
    // $262.IsHTMLDDA() — returns null when called (Annex B §B.3.6 / test262
    // INTERPRETING.md). The exotic typeof/equality behaviors are not modeled.
    value.VmNative(value.IsHTMLDDA) -> #(state, Ok(value.JsNull))
    value.AtomicsNative(n) -> builtins_atomics.dispatch(n, args, this, state)
    value.JsonNative(n) -> builtins_json.dispatch(n, args, this, state)
    value.ReflectNative(n) -> builtins_reflect.dispatch(n, args, this, state)
    value.MapNative(n) -> builtins_map.dispatch(n, args, this, state)
    value.SetNative(n) -> builtins_set.dispatch(n, args, this, state)
    value.WeakMapNative(n) -> builtins_weak_map.dispatch(n, args, this, state)
    value.WeakSetNative(n) -> builtins_weak_set.dispatch(n, args, this, state)
    value.FinalizationRegistryNative(n) ->
      builtins_finalization_registry.dispatch(n, args, this, state)
    value.DisposableStackNative(n) ->
      builtins_disposable_stack.dispatch(n, args, this, state)
    value.IteratorNative(n) -> builtins_iterator.dispatch(n, args, this, state)
    value.RegExpNative(n) -> builtins_regexp.dispatch(n, args, this, state)
    value.DateNative(n) -> builtins_date.dispatch(n, args, this, state)
    value.IntlNative(n) -> builtins_intl.dispatch(n, args, this, state)
    value.TemporalNative(n) -> builtins_temporal.dispatch(n, args, this, state)
    value.ShadowRealmNative(n) ->
      realm.shadow_realm_dispatch(
        n,
        args,
        this,
        state,
        execute_inner,
        new_state_fn,
      )
    value.ArrayBufferNative(n) ->
      builtins_array_buffer.dispatch(n, args, this, state)
    value.DataViewNative(n) -> builtins_data_view.dispatch(n, args, this, state)
    value.TypedArrayNative(n) ->
      builtins_typed_array.dispatch(n, args, this, state)
    // Standalone VM-level natives
    value.VmNative(value.FunctionConstructor) ->
      realm.function_constructor_native(
        args,
        "function",
        state,
        execute_inner,
        new_state_fn,
      )
    value.VmNative(value.GeneratorFunctionConstructor) ->
      realm.function_constructor_native(
        args,
        "function*",
        state,
        execute_inner,
        new_state_fn,
      )
    value.VmNative(value.AsyncGeneratorFunctionConstructor) ->
      realm.function_constructor_native(
        args,
        "async function*",
        state,
        execute_inner,
        new_state_fn,
      )
    value.VmNative(value.AsyncFunctionConstructor) ->
      realm.function_constructor_native(
        args,
        "async function",
        state,
        execute_inner,
        new_state_fn,
      )
    value.VmNative(value.IteratorSymbolIterator) -> #(state, Ok(this))
    // §10.2.4.1 %ThrowTypeError% — restricted "caller"/"arguments" accessor.
    value.VmNative(value.ThrowTypeErrorFn) ->
      restricted_function_property(this, state)
    value.VmNative(value.FunctionToString) -> function_to_string(this, state)
    // §20.2.3.6 Function.prototype [ @@hasInstance ] ( V )
    value.VmNative(value.FunctionHasInstance) ->
      function_has_instance(this, args, state)
    // §20.2.3 calling Function.prototype itself returns undefined.
    value.VmNative(value.FunctionPrototypeCall) -> #(state, Ok(JsUndefined))
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
    // §21.2.1.1 BigInt ( value )
    value.VmNative(value.BigIntGlobal) ->
      builtins_typed_array.bigint_global(args, state)
    // §21.2.3.3 BigInt.prototype.toString ( [ radix ] )
    value.VmNative(value.BigIntPrototypeToString) ->
      builtins_typed_array.bigint_proto_to_string(this, args, state)
    // §21.2.3.4 BigInt.prototype.valueOf ( )
    value.VmNative(value.BigIntPrototypeValueOf) ->
      builtins_typed_array.bigint_proto_value_of(this, args, state)
  }
}

/// Shared shape of the global String->String functions
/// (decodeURI, encodeURI, escape, ...): coerce the first arg to a string,
/// apply `f`, return the result as a JsString.
fn string_global(
  args: List(JsValue),
  state: State(host),
  f: fn(String) -> String,
) -> #(State(host), Result(JsValue, JsValue)) {
  let arg = helpers.first_arg_or_undefined(args)
  use str, state <- coerce.try_to_string(state, arg)
  #(state, Ok(JsString(f(str))))
}

/// §20.4.3 thisSymbolValue(value): a Symbol primitive, or a Symbol wrapper
/// object's [[SymbolData]]; anything else is a TypeError. Step-error shaped
/// for use inside call_native.
fn this_symbol_value(
  state: State(host),
  this: JsValue,
  method: String,
) -> Result(#(value.SymbolId, State(host)), #(StepResult, JsValue, State(host))) {
  case this {
    value.JsSymbol(id) -> Ok(#(id, state))
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: value.SymbolObject(value: id), ..)) ->
          Ok(#(id, state))
        _ ->
          state.throw_type_error(
            state,
            "Symbol.prototype." <> method <> " requires that 'this' be a Symbol",
          )
      }
    _ ->
      state.throw_type_error(
        state,
        "Symbol.prototype." <> method <> " requires that 'this' be a Symbol",
      )
  }
}
