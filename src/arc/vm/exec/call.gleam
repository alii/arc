import arc/vm/builtins/array as builtins_array
import arc/vm/builtins/array_buffer as builtins_array_buffer
import arc/vm/builtins/atomics as builtins_atomics
import arc/vm/builtins/bigint as builtins_bigint
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
import arc/vm/builtins/uri as builtins_uri
import arc/vm/builtins/weak_map as builtins_weak_map
import arc/vm/builtins/weak_set as builtins_weak_set
import arc/vm/completion.{
  type Outcome, Completed, NormalCompletion, Suspended, ThrowCompletion,
}
import arc/vm/exec/async_generators
import arc/vm/exec/frame
import arc/vm/exec/generators
import arc/vm/exec/promises
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/internal/ordered_entries
import arc/vm/internal/tuple_array
import arc/vm/key.{Named}
import arc/vm/limits
import arc/vm/ops/array_iterator
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/ops/property
import arc/vm/realm
import arc/vm/state.{
  type Heap, type NativeFnSlot, type State, type StepExit, type VmError,
  InternalError, SavedFrame, State, Threw, VmFailed,
}
import arc/vm/value.{
  type FuncTemplate, type JsValue, type Ref, AsyncFunctionSlot,
  AsyncGeneratorObject, AsyncGeneratorSlot, DataProperty, FunctionObject,
  GeneratorObject, GeneratorSlot, JsBool, JsObject, JsString, JsUndefined,
  JsUninitialized, NativeFunction, ObjectSlot, OrdinaryObject,
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
  fn(State(host)) -> Result(#(Outcome, State(host)), VmError)

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
) -> Result(State(host), StepExit(host)) {
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
      let #(heap, locals) =
        frame.setup_frame(
          state,
          env_ref,
          fn_ref,
          home_object,
          callee_template,
          args,
          this_val,
          new_target,
        )
      case callee_template.is_generator, callee_template.is_async {
        True, is_async ->
          call_coroutine_function(
            State(..state, heap:),
            callee_template,
            args,
            rest_stack,
            locals,
            env_ref,
            execute_inner,
            is_async,
          )
        False, True ->
          call_async_function(
            State(..state, heap:),
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
) -> Result(State(host), StepExit(host)) {
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

/// Set up an isolated execution state to run a coroutine body from its start:
/// generators, async generators, and async functions all enter this way (empty
/// stack, pc 0, no caller frames, no try frames, no new.target). The ONE place
/// that spelling lives — three field-for-field copies used to.
fn coroutine_initial_state(
  state: State(host),
  callee_template: FuncTemplate,
  args: List(JsValue),
  locals: tuple_array.TupleArray(JsValue),
) -> State(host) {
  coroutine_resume_state(state, callee_template, locals, [], 0, [])
  |> fn(s) { State(..s, call_args: args) }
}

/// Set up an isolated execution state to RESUME a coroutine body from a saved
/// suspension point. Same isolation as `coroutine_initial_state` (no caller
/// frames, no new.target, no call_args — the arguments object was built before
/// the first suspension), but with the saved stack/pc/try_stack restored.
fn coroutine_resume_state(
  state: State(host),
  func_template: FuncTemplate,
  locals: tuple_array.TupleArray(JsValue),
  stack: List(JsValue),
  pc: Int,
  try_stack: List(state.TryFrame),
) -> State(host) {
  State(
    ..state,
    stack:,
    locals:,
    func: func_template,
    code: func_template.bytecode,
    constants: func_template.constants,
    pc:,
    call_stack: [],
    try_stack:,
    new_target: JsUndefined,
    call_args: [],
    // A coroutine body is its OWN frame: it must not inherit the caller's
    // sloppy-direct-eval var dict (`call_regular_function` resets it too).
    // Resumes overwrite this with the frame's saved eval_env.
    eval_env: None,
  )
}

/// Generator / async-generator call: run the body until InitialYield (the very
/// first op, so the body proper never runs), stash the suspended frame in a
/// slot, wrap it in a generator object and hand that back to the caller. The
/// two flavours differ only in the slot they build, the exotic kind, the
/// prototype and the InternalError label — everything else, including the
/// "can't have completed or awaited yet" impossibility arms, is identical.
/// All four are derived from `is_async` here, so a mismatched slot/kind/
/// prototype trio is unrepresentable at the call site.
fn call_coroutine_function(
  state: State(host),
  callee_template: FuncTemplate,
  args: List(JsValue),
  rest_stack: List(JsValue),
  locals: tuple_array.TupleArray(JsValue),
  env_ref: value.Ref,
  execute_inner: ExecuteInnerFn(host),
  is_async: Bool,
) -> Result(State(host), StepExit(host)) {
  case
    execute_inner(coroutine_initial_state(state, callee_template, args, locals))
  {
    Ok(#(Suspended(completion.Yield, _), suspended)) -> {
      let frame = generators.suspended_frame(suspended)
      let slot = case is_async {
        True ->
          AsyncGeneratorSlot(
            gen_state: value.AGSuspendedStart,
            queue: #([], []),
            func_template: callee_template,
            env_ref:,
            frame:,
          )
        False ->
          GeneratorSlot(
            gen_state: value.SuspendedStart,
            func_template: callee_template,
            env_ref:,
            frame:,
          )
      }
      let #(h, data_ref) = heap.alloc(suspended.heap, slot)
      let #(kind, prototype) = case is_async {
        True -> #(
          AsyncGeneratorObject(data_ref),
          state.builtins.async_generator.prototype,
        )
        False -> #(
          GeneratorObject(data_ref),
          state.builtins.generator.prototype,
        )
      }
      let #(h, gen_obj_ref) = common.alloc_wrapper(h, kind, prototype)
      Ok(
        State(
          ..state.adopt_child(state, suspended),
          heap: h,
          stack: [JsObject(gen_obj_ref), ..rest_stack],
          pc: state.pc + 1,
        ),
      )
    }
    Ok(#(Completed(ThrowCompletion(thrown)), thrown_state)) ->
      Error(Threw(thrown, State(..state, heap: thrown_state.heap)))
    // InitialYield is the first op — the body can neither complete nor await
    // before it. Either arriving here is a VM bug.
    Ok(#(Completed(NormalCompletion(_)), _))
    | Ok(#(Suspended(completion.Await, _), _)) -> {
      let label = case is_async {
        True -> "call_async_generator_function"
        False -> "call_generator_function"
      }
      Error(VmFailed(InternalError(label, "didn't hit InitialYield"), state))
    }
    Error(vm_err) -> Error(VmFailed(vm_err, state))
  }
}

/// Async function call: create a promise, execute body eagerly.
/// If the body completes synchronously, resolve/reject the promise immediately.
/// If the body hits `await`, save state and set up promise callbacks to resume.
fn call_async_function(
  state: State(host),
  callee_template: FuncTemplate,
  args: List(JsValue),
  rest_stack: List(JsValue),
  locals: tuple_array.TupleArray(JsValue),
  execute_inner: ExecuteInnerFn(host),
  _unwind_to_catch: UnwindToCatchFn(host),
) -> Result(State(host), StepExit(host)) {
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
  finish_async_execution(
    state,
    execute_inner(coroutine_initial_state(
      State(..state, heap: h),
      callee_template,
      args,
      locals,
    )),
    AsyncCtx(
      promise_data_ref: data_ref,
      resolve: resolve_fn,
      reject: reject_fn,
      func_template: callee_template,
    ),
    FirstAwait(promise: JsObject(promise_ref)),
    rest_stack,
  )
}

/// Everything an in-flight async function needs to settle or re-suspend —
/// exactly the fields `AsyncFunctionSlot` already stores alongside the saved
/// frame. Bundled so `finish_async_execution` takes a record instead of four
/// loose positional arguments that must stay in the right order.
type AsyncCtx {
  AsyncCtx(
    promise_data_ref: Ref,
    resolve: JsValue,
    reject: JsValue,
    func_template: FuncTemplate,
  )
}

/// Which entry point drove the body, which decides two co-varying things:
/// whether an AsyncFunctionSlot must be allocated or overwritten, and what the
/// caller's stack gets pushed. Encoding them as one sum makes the illegal
/// combinations (fresh call that overwrites a slot; resume that pushes a
/// second promise) unrepresentable.
type AsyncEntry {
  /// The initial call: allocate a fresh slot on the first await, and hand the
  /// caller the freshly created promise.
  FirstAwait(promise: JsValue)
  /// A resume from a settled await: overwrite `slot`, and push `undefined`
  /// (the AsyncResume native's return value — nobody reads it).
  Resumed(slot: Ref)
}

/// Shared completion handling for an async function body execution.
/// Resolves/rejects the outer promise on completion, or saves the suspension
/// state and attaches resume callbacks on `await`.
fn finish_async_execution(
  state: State(host),
  exec_result: Result(#(Outcome, State(host)), VmError),
  ctx: AsyncCtx,
  entry: AsyncEntry,
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  let AsyncCtx(promise_data_ref:, resolve:, reject:, func_template:) = ctx
  let result_value = case entry {
    FirstAwait(promise:) -> promise
    Resumed(_) -> JsUndefined
  }
  case exec_result {
    Ok(#(Suspended(completion.Await, awaited_value), suspended)) -> {
      // Body hit `await` -- save state, set up promise resolution
      let h2 = suspended.heap
      let saved_try = generators.save_stacks(suspended.try_stack)
      let slot =
        AsyncFunctionSlot(
          promise_data_ref:,
          resolve:,
          reject:,
          func_template:,
          saved_pc: suspended.pc,
          saved_locals: suspended.locals,
          saved_stack: suspended.stack,
          saved_try_stack: saved_try,
        )
      let #(h2, async_data_ref) = case entry {
        Resumed(slot_ref) -> #(heap.write(h2, slot_ref, slot), slot_ref)
        FirstAwait(_) -> heap.alloc(h2, slot)
      }
      let state =
        async_setup_await(
          State(..state.adopt_child(state, suspended), heap: h2),
          async_data_ref,
          awaited_value,
        )
      Ok(State(..state, stack: [result_value, ..rest_stack], pc: state.pc + 1))
    }
    Ok(#(Completed(NormalCompletion(return_value)), final_state)) -> {
      // Async function completed -- resolve the outer promise through the
      // resolving function (§27.7.5.1 AsyncFunctionStart performs
      // Call(promiseCapability.[[Resolve]], ...)), so a thenable return value
      // is adopted (§27.2.1.3.2) instead of fulfilling with the thenable raw.
      let state = state.adopt_child(state, final_state)
      use #(_resolved, state) <- result.try(
        state.rethrow(state.call(state, resolve, JsUndefined, [return_value])),
      )
      Ok(State(..state, stack: [result_value, ..rest_stack], pc: state.pc + 1))
    }
    Ok(#(Completed(ThrowCompletion(thrown)), final_state)) -> {
      // Async function threw -- reject the outer promise
      let state =
        builtins_promise.reject_promise(
          state.adopt_child(state, final_state),
          promise_data_ref,
          thrown,
        )
      Ok(State(..state, stack: [result_value, ..rest_stack], pc: state.pc + 1))
    }
    Ok(#(Suspended(completion.Yield, _), _)) ->
      Error(VmFailed(
        InternalError(
          "finish_async_execution",
          "yield in non-generator async function",
        ),
        state,
      ))
    Error(vm_err) -> Error(VmFailed(vm_err, state))
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
) -> Result(State(host), StepExit(host)) {
  let settled_value = helpers.first_arg_or_undefined(args)
  case heap.read(state.heap, async_data_ref) {
    Some(AsyncFunctionSlot(
      promise_data_ref:,
      resolve: slot_resolve,
      reject: slot_reject,
      func_template:,
      saved_pc:,
      saved_locals:,
      saved_stack:,
      saved_try_stack:,
    )) -> {
      // Build the resume stack: push resolved value for fulfillment
      let resume_stack = case is_reject {
        False -> [settled_value, ..saved_stack]
        True -> saved_stack
      }
      let exec_state =
        coroutine_resume_state(
          state,
          func_template,
          saved_locals,
          resume_stack,
          saved_pc,
          generators.restore_stacks(saved_try_stack),
        )
      // For rejection, throw the value so try/catch inside async fn can handle it
      let exec_result = case is_reject {
        False -> execute_inner(exec_state)
        True -> {
          case unwind_to_catch(exec_state, settled_value) {
            Some(caught_state) -> execute_inner(caught_state)
            None -> Ok(#(Completed(ThrowCompletion(settled_value)), exec_state))
          }
        }
      }
      finish_async_execution(
        state,
        exec_result,
        AsyncCtx(
          promise_data_ref:,
          resolve: slot_resolve,
          reject: slot_reject,
          func_template:,
        ),
        Resumed(slot: async_data_ref),
        rest_stack,
      )
    }
    _ ->
      Error(VmFailed(
        InternalError(
          "call_native_async_resume",
          "invalid slot for ref " <> string.inspect(async_data_ref),
        ),
        state,
      ))
  }
}

/// Call a native (Gleam-implemented) function. Most natives execute synchronously
/// and push their result onto the stack. However, call/apply/bind need special
/// handling because they invoke other functions (potentially pushing call frames).
///
/// `new_target` is §9.4.5 GetNewTarget for THIS invocation: `do_construct`
/// passes the real newTarget; every plain-[[Call]] site passes JsUndefined.
/// It is applied only around the synchronous native-body branches (Dispatch /
/// Host) — the frame-pushing delegation natives (call/apply/bound) must keep
/// the caller frame's ambient `state.new_target` intact so the SavedFrame they
/// push saves/restores the right value.
pub fn call_native(
  state: State(host),
  native: NativeFnSlot(host),
  args: List(JsValue),
  rest_stack: List(JsValue),
  this: JsValue,
  new_target: JsValue,
  execute_inner: ExecuteInnerFn(host),
  unwind_to_catch: UnwindToCatchFn(host),
  dispatch_fn: DispatchNativeFn(host),
) -> Result(State(host), StepExit(host)) {
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
          state.rethrow(property.create_list_from_array_like(state, arg_array))
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
          // Step 5's HasOwnProperty and step 6.a's Get both go through the
          // internal methods — accessors AND proxy traps — and can throw
          // (test262: bind/instance-length-* , bind/get-fn-length-error,
          // proxy/bind: `getOwnPropertyDescriptor` fires before `get`).
          use #(has_length, state) <- result.try(
            state.rethrow(builtins_object.get_own_property_stateful(
              state,
              target_ref,
              JsString("length"),
            )),
          )
          use #(target_len, state) <- result.try(case has_length {
            Some(_) ->
              state.rethrow(object.get_value(
                state,
                target_ref,
                Named("length"),
                this,
              ))
            None -> Ok(#(JsUndefined, state))
          })
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
        value.Fulfilled,
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
        value.Rejected,
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
      Error(Threw(reason, state))
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
      // §20.4.1.1 step 4: If description is undefined, descString is
      // undefined; else descString = ? ToString(description). The coercion
      // is observable — Symbol({toString(){throw x}}) must throw x.
      use #(description, state) <- result.try(
        case helpers.first_arg_or_undefined(args) {
          JsUndefined -> Ok(#(None, state))
          desc -> {
            use #(s, state) <- result.map(
              state.rethrow(coerce.js_to_string(state, desc)),
            )
            #(Some(s), state)
          }
        },
      )
      let sym_val = builtins_symbol.call_symbol(description)
      Ok(State(..state, stack: [sym_val, ..rest_stack], pc: state.pc + 1))
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
          // §20.4.2.2 step 4.a: the registered symbol's [[Description]] IS
          // the registry key.
          let id = builtins_symbol.new_symbol(option.Some(key_str))
          let new_registry = dict.insert(state.ctx.symbol_registry, key_str, id)
          Ok(
            State(
              ..state,
              stack: [value.JsSymbol(id), ..rest_stack],
              pc: state.pc + 1,
              ctx: state.RealmCtx(..state.ctx, symbol_registry: new_registry),
            ),
          )
        }
      }
    }
    // §20.4.3.3 Symbol.prototype.toString — SymbolDescriptiveString(thisSymbolValue)
    value.Call(value.SymbolPrototypeToString) -> {
      use #(id, state) <- result.try(this_symbol_value(state, this, "toString"))
      let s = builtins_symbol.descriptive_string(id)
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
      let val = case value.symbol_description(id) {
        option.Some(s) -> JsString(s)
        option.None -> JsUndefined
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
          let s = builtins_symbol.descriptive_string(id)
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
            Error(#(thrown, new_state)) -> Error(Threw(thrown, new_state))
          }
      }
    // All other native functions: synchronous dispatch via Dispatch slot.
    // §10.2.1: the native body observes THIS invocation's newTarget
    // (JsUndefined for a plain [[Call]], the real one from do_construct) —
    // never the enclosing frame's ambient value. Dispatch natives run
    // synchronously in the current frame, so the caller's new_target is
    // restored on both the return and the throw path.
    value.Dispatch(native) -> {
      let #(new_state, result) =
        dispatch_fn(native, args, this, State(..state, new_target:))
      let new_state = State(..new_state, new_target: state.new_target)
      case result {
        Ok(return_value) ->
          Ok(
            State(
              ..new_state,
              stack: [return_value, ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        Error(thrown) -> Error(Threw(thrown, new_state))
      }
    }
    // Host-provided native: call the embedder's closure directly. Same
    // newTarget set/restore as the Dispatch branch — host natives are
    // synchronous too.
    value.Host(f) -> {
      let #(new_state, result) = f(args, this, State(..state, new_target:))
      let new_state = State(..new_state, new_target: state.new_target)
      case result {
        Ok(return_value) ->
          Ok(
            State(
              ..new_state,
              stack: [return_value, ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        Error(thrown) -> Error(Threw(thrown, new_state))
      }
    }
  }
}

/// Shared tail for `new String/Number/Boolean`: allocate the primitive
/// wrapper exotic, push it, advance pc. Prototype comes from
/// §10.1.13.2 GetPrototypeFromConstructor, so subclass instances get the
/// derived prototype — and a proxy newTarget's `get` trap or an accessor
/// `prototype` property may throw out of here.
fn push_wrapper(
  state: State(host),
  rest_stack: List(JsValue),
  kind,
  new_target_ref: Ref,
  intrinsic_proto: Ref,
) {
  use #(proto, state) <- result.try(
    state.rethrow(object.get_prototype_from_constructor(
      state,
      new_target_ref,
      intrinsic_proto,
    )),
  )
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
) -> Result(State(host), StepExit(host)) {
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
  // Every branch below runs with IsConstructor(ctor) already true — arrows,
  // generators, methods, non-constructible natives and revoked-at-birth proxies
  // were all rejected by the guard above, so no branch re-checks it.
  case heap.read(state.heap, ctor_ref) {
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
            state.rethrow(object.get_prototype_from_constructor(
              state,
              new_target_ref,
              state.builtins.object.prototype,
            )),
          )
          let #(heap, new_obj_ref) =
            heap.alloc(
              state.heap,
              ObjectSlot(
                kind: OrdinaryObject,
                properties: dict.new(),
                elements: elements.new(),
                prototype: Some(proto),
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
        Error(#(thrown, st)) -> Error(Threw(thrown, st))
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
        Error(#(thrown, st)) -> Error(Threw(thrown, st))
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
        state.rethrow(object.get_prototype_from_constructor(
          state,
          new_target_ref,
          state.builtins.object.prototype,
        )),
      )
      let #(heap, ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: OrdinaryObject,
            properties: dict.new(),
            elements: elements.new(),
            prototype: Some(proto),
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
    Some(ObjectSlot(kind: NativeFunction(native, ..), ..)) -> {
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
          new_target,
          execute_inner,
          unwind_to_catch,
          dispatch_fn,
        )
        |> result.map_error(fn(exit) {
          use err_state <- state.map_exit_state(exit)
          State(..err_state, new_target: state.new_target)
        }),
      )
      let new_state = State(..new_state, new_target: state.new_target)
      let derived_proto =
        object.own_data_prototype(new_state.heap, new_target_ref)
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
    // Unreachable: the IsConstructor guard above admitted only FunctionObject /
    // NativeFunction / ProxyObject slots, and every one of those has a branch.
    _ ->
      Error(VmFailed(
        InternalError(
          "do_construct",
          "IsConstructor passed but slot kind unhandled",
        ),
        state,
      ))
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
) -> Result(State(host), StepExit(host)) {
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
          // Plain [[Call]] of a native: NewTarget is undefined (§10.2.1),
          // regardless of the caller frame's ambient `state.new_target`.
          call_native(
            state,
            native,
            args,
            state.stack,
            this_val,
            JsUndefined,
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

/// Allocate a {value, done} iterator-result object, push it, advance pc.
fn push_iter_result(
  state: State(host),
  rest_stack: List(JsValue),
  h: Heap(host),
  val: JsValue,
  done: Bool,
) -> Result(State(host), StepExit(host)) {
  let #(h, result) = common.create_iter_result(h, state.builtins, val, done)
  Ok(State(..state, heap: h, stack: [result, ..rest_stack], pc: state.pc + 1))
}

fn iter_incompatible(
  state: State(host),
  tag: String,
) -> Result(State(host), StepExit(host)) {
  state.throw_type_error(
    state,
    tag <> " Iterator next called on incompatible receiver",
  )
}

/// ES §23.1.5.2.1 %ArrayIteratorPrototype%.next()
/// The stepping algorithm itself lives in `ops/array_iterator` — the
/// `IteratorNext` opcode's in-VM fast path steps through the same `step`.
fn call_array_iterator_next(
  state: State(host),
  this: JsValue,
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  case this {
    JsObject(iter_ref) ->
      case heap.read(state.heap, iter_ref) {
        Some(ObjectSlot(kind: value.ArrayIteratorObject(..), ..)) -> {
          use #(step, state) <- result.try(array_iterator.step(state, iter_ref))
          case step {
            array_iterator.Exhausted ->
              push_iter_result(state, rest_stack, state.heap, JsUndefined, True)
            array_iterator.Yielded(val) ->
              push_iter_result(state, rest_stack, state.heap, val, False)
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
fn alloc_entry_pair(
  h: Heap(host),
  array_proto: value.Ref,
  a: JsValue,
  b: JsValue,
) -> #(Heap(host), JsValue) {
  let #(h, pair_ref) = common.alloc_array(h, [a, b], array_proto)
  #(h, JsObject(pair_ref))
}

/// ES §24.2.5.2.1 %SetIteratorPrototype%.next() — LIVE iteration by seq
/// cursor: yield the source Set's first live record at seq >= cursor and
/// resume at the cursor `next_from` hands back (amortized O(1) per step).
/// Entries added during
/// iteration — including delete + re-add, which assigns a fresh seq — are
/// visited; entries deleted before being reached leave a gap that's skipped.
fn call_set_iterator_next(
  state: State(host),
  this: JsValue,
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
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
            Some(ObjectSlot(kind: value.SetObject(store:), ..)) ->
              ordered_entries.next_from(store, cursor)
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
            Some(#(next_cursor, _k, v)) -> {
              // For "entries" yield [v, v]; for "values"/"keys" yield v.
              let #(h, yielded) = case kind {
                value.SetIterValues -> #(state.heap, v)
                value.SetIterEntries ->
                  alloc_entry_pair(
                    state.heap,
                    state.builtins.array.prototype,
                    v,
                    v,
                  )
              }
              let h =
                heap.write(
                  h,
                  iter_ref,
                  ObjectSlot(
                    ..slot,
                    kind: value.SetIteratorObject(
                      source:,
                      cursor: next_cursor,
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
) -> Result(State(host), StepExit(host)) {
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
            Some(ObjectSlot(kind: value.MapObject(store:), ..)) ->
              ordered_entries.next_from(store, cursor)
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
            Some(#(next_cursor, k, v)) -> {
              let #(h, yielded) = case kind {
                value.MapIterKeys -> #(state.heap, value.map_key_to_js(k))
                value.MapIterValues -> #(state.heap, v)
                value.MapIterEntries ->
                  alloc_entry_pair(
                    state.heap,
                    state.builtins.array.prototype,
                    value.map_key_to_js(k),
                    v,
                  )
              }
              let h =
                heap.write(
                  h,
                  iter_ref,
                  ObjectSlot(
                    ..slot,
                    kind: value.MapIteratorObject(
                      source:,
                      cursor: next_cursor,
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

/// `run_to_completion` is the SUSPENSION-NARROWED step loop
/// (`interpreter.execute_to_completion`): every native that runs code here
/// (eval, evalScript, the Function constructors, ShadowRealm.evaluate) runs a
/// whole non-coroutine frame, so realm.gleam only ever receives a
/// `Completion` — a suspension escaping such a frame surfaces as an
/// `InternalError` inside the callback, never as a value.
pub fn dispatch_native(
  native: value.NativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
  run_to_completion: realm.RunToCompletionFn(host),
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
      realm.eval_script_native(
        args,
        this,
        state,
        run_to_completion,
        new_state_fn,
      )
    value.VmNative(value.CreateRealm) -> realm.create_realm_native(this, state)
    value.VmNative(value.Gc) -> #(state, Ok(JsUndefined))
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
        run_to_completion,
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
        run_to_completion,
        new_state_fn,
      )
    value.VmNative(value.GeneratorFunctionConstructor) ->
      realm.function_constructor_native(
        args,
        "function*",
        state,
        run_to_completion,
        new_state_fn,
      )
    value.VmNative(value.AsyncGeneratorFunctionConstructor) ->
      realm.function_constructor_native(
        args,
        "async function*",
        state,
        run_to_completion,
        new_state_fn,
      )
    value.VmNative(value.AsyncFunctionConstructor) ->
      realm.function_constructor_native(
        args,
        "async function",
        state,
        run_to_completion,
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
      realm.eval_native(args, state, run_to_completion, new_state_fn)
    // §19.2.6.2 decodeURI ( encodedURI ) — reserved escapes are preserved.
    value.VmNative(value.DecodeURI) ->
      uri_decode_global(args, state, preserve_reserved: True)
    value.VmNative(value.EncodeURI) ->
      string_global(args, state, builtins_uri.uri_encode(_, True))
    // §19.2.6.3 decodeURIComponent ( encodedURIComponent )
    value.VmNative(value.DecodeURIComponent) ->
      uri_decode_global(args, state, preserve_reserved: False)
    value.VmNative(value.EncodeURIComponent) ->
      string_global(args, state, builtins_uri.uri_encode(_, False))
    // AnnexB B.2.1.1 escape ( string )
    value.VmNative(value.Escape) ->
      string_global(args, state, builtins_uri.js_escape)
    // AnnexB B.2.1.2 unescape ( string )
    value.VmNative(value.Unescape) ->
      string_global(args, state, builtins_uri.js_unescape)
    // §21.2.1.1 BigInt ( value )
    value.VmNative(value.BigIntGlobal) ->
      builtins_bigint.bigint_global(args, state)
    // §21.2.3.3 BigInt.prototype.toString ( [ radix ] )
    value.VmNative(value.BigIntPrototypeToString) ->
      builtins_bigint.bigint_proto_to_string(this, args, state)
    // §21.2.3.2 BigInt.prototype.toLocaleString ( [ reserved1 [, reserved2 ] ] )
    value.VmNative(value.BigIntPrototypeToLocaleString) ->
      builtins_bigint.bigint_proto_to_locale_string(this, args, state)
    // §21.2.3.4 BigInt.prototype.valueOf ( )
    value.VmNative(value.BigIntPrototypeValueOf) ->
      builtins_bigint.bigint_proto_value_of(this, args, state)
  }
}

/// Shared shape of the infallible global String->String functions
/// (encodeURI, escape, ...): coerce the first arg to a string, apply `f`,
/// return the result as a JsString.
fn string_global(
  args: List(JsValue),
  state: State(host),
  f: fn(String) -> String,
) -> #(State(host), Result(JsValue, JsValue)) {
  let arg = helpers.first_arg_or_undefined(args)
  use str, state <- coerce.try_to_string(state, arg)
  #(state, Ok(JsString(f(str))))
}

/// §19.2.6.2/.3 decodeURI / decodeURIComponent: coerce the argument to a
/// string, decode it, and throw a URIError for a malformed escape sequence
/// ("Throw a URIError exception" in the Decode abstract operation).
fn uri_decode_global(
  args: List(JsValue),
  state: State(host),
  preserve_reserved preserve_reserved: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  let arg = helpers.first_arg_or_undefined(args)
  use str, state <- coerce.try_to_string(state, arg)
  case builtins_uri.uri_decode(str, preserve_reserved) {
    Ok(decoded) -> #(state, Ok(JsString(decoded)))
    Error(builtins_uri.Malformed(offset:)) ->
      throw_uri_error(
        state,
        "URI malformed at position " <> int.to_string(offset),
      )
  }
}

/// Allocate a URIError and return it as a thrown completion. decodeURI /
/// decodeURIComponent are the only URIError producers in the engine, so
/// this goes through the %URIError% constructor dispatch (which also
/// attaches the stack trace) rather than a dedicated `state.ErrorKind`.
fn throw_uri_error(
  state: State(host),
  msg: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(state, created) =
    builtins_error.dispatch(
      value.ErrorConstructor(proto: state.builtins.uri_error.prototype),
      [JsString(msg)],
      JsUndefined,
      state,
    )
  // Constructing the error cannot itself fail here (a plain string message),
  // but if it ever did, the thrown value is still the right completion.
  case created {
    Ok(err) | Error(err) -> #(state, Error(err))
  }
}

/// §20.4.3 thisSymbolValue(value): a Symbol primitive, or a Symbol wrapper
/// object's [[SymbolData]]; anything else is a TypeError. Step-error shaped
/// for use inside call_native.
fn this_symbol_value(
  state: State(host),
  this: JsValue,
  method: String,
) -> Result(#(value.SymbolId, State(host)), StepExit(host)) {
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
