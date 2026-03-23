import arc/vm/builtins/common.{type Builtins}
import arc/vm/builtins/promise as builtins_promise
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State, type StepResult, State, Thrown}
import arc/vm/value.{
  type JsValue, type Ref, ArrayObject, Finite, JsBool, JsNumber, JsObject,
  JsString, JsUndefined, Named, NativeFunction, ObjectSlot, OrdinaryObject,
}
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result

// ============================================================================
// Heap-threading helpers
// ============================================================================

/// ES spec NewPromiseCapability — create a promise and its resolve/reject
/// functions in one step. Collapses the most common 2-step heap-threading
/// chain in this file (create_promise + create_resolving_functions).
///
/// Returns #(heap, promise_ref, data_ref, resolve_fn, reject_fn).
fn new_promise_capability(
  h: Heap,
  b: Builtins,
) -> #(Heap, Ref, Ref, JsValue, JsValue) {
  let #(h, promise_ref, data_ref) =
    builtins_promise.create_promise(h, b.promise.prototype)
  let #(h, resolve, reject) =
    builtins_promise.create_resolving_functions(
      h,
      b.function.prototype,
      promise_ref,
      data_ref,
    )
  #(h, promise_ref, data_ref, resolve, reject)
}

/// Allocate shared state for Promise.all/allSettled/any combinators:
/// a values array (pre-filled with undefined) and a remaining-count box.
/// Returns #(heap, values_ref, remaining_ref).
fn alloc_combinator_state(h: Heap, b: Builtins, count: Int) -> #(Heap, Ref, Ref) {
  let #(h, values_ref) =
    common.alloc_array(h, list.repeat(JsUndefined, count), b.array.prototype)
  let #(h, remaining_ref) =
    heap.alloc(
      h,
      value.BoxSlot(value: JsNumber(Finite(int.to_float(count + 1)))),
    )
  #(h, values_ref, remaining_ref)
}

// ============================================================================
// Promise native function implementations
// ============================================================================

/// new Promise(executor) — create promise, call executor(resolve, reject),
/// catch throws and reject.
pub fn call_native_promise_constructor(
  state: State,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let executor = case args {
    [f, ..] -> f
    [] -> JsUndefined
  }

  // Verify executor is callable
  case coerce.is_callable_value(state.heap, executor) {
    False -> {
      state.throw_type_error(state, "Promise resolver is not a function")
    }
    True -> {
      let #(h, promise_ref, data_ref, resolve_fn, reject_fn) =
        new_promise_capability(state.heap, state.builtins)
      // Run executor inline — its return value is discarded, the promise is the result.
      let new_state = State(..state, heap: h, stack: rest_stack)
      case
        state.call(new_state, executor, JsUndefined, [
          resolve_fn,
          reject_fn,
        ])
      {
        Ok(#(_, after_state)) ->
          Ok(
            State(
              ..after_state,
              stack: [JsObject(promise_ref), ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        Error(#(thrown, after_state)) -> {
          let state =
            builtins_promise.reject_promise(after_state, data_ref, thrown)
          Ok(
            State(
              ..state,
              stack: [JsObject(promise_ref), ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        }
      }
    }
  }
}

/// Internal resolve function — check already-resolved, then fulfill/reject.
pub fn call_native_promise_resolve_fn(
  state: State,
  promise_ref: Ref,
  data_ref: Ref,
  already_resolved_ref: Ref,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let resolution = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }

  // Check if already resolved
  case heap.read_box(state.heap, already_resolved_ref) == Some(JsBool(True)) {
    True ->
      // Already resolved — ignore
      Ok(State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1))

    False -> {
      // Mark as resolved
      let h =
        heap.write(
          state.heap,
          already_resolved_ref,
          value.BoxSlot(value: JsBool(True)),
        )

      use <- bool.guard(resolution == JsObject(promise_ref), {
        let #(h, err) =
          common.make_type_error(
            h,
            state.builtins,
            "Chaining cycle detected for promise",
          )

        let state =
          builtins_promise.reject_promise(
            State(..state, heap: h),
            data_ref,
            err,
          )

        Ok(State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1))
      })

      // Check if resolution is a thenable
      case
        builtins_promise.get_thenable_then(State(..state, heap: h), resolution)
      {
        Ok(#(then_fn, state)) -> {
          // Create resolving functions for assimilation
          let #(h, resolve_fn, reject_fn) =
            builtins_promise.create_resolving_functions(
              state.heap,
              state.builtins.function.prototype,
              promise_ref,
              data_ref,
            )
          let job =
            value.PromiseResolveThenableJob(
              thenable: resolution,
              then_fn:,
              resolve: resolve_fn,
              reject: reject_fn,
            )

          State(
            ..state,
            heap: h,
            stack: [JsUndefined, ..rest_stack],
            pc: state.pc + 1,
            job_queue: list.append(state.job_queue, [job]),
          )
          |> Ok
        }

        Error(#(option.Some(thrown), state)) -> {
          // Getter threw — reject promise with the error (spec 25.6.1.3.2 step 9)
          let state = builtins_promise.reject_promise(state, data_ref, thrown)

          State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1)
          |> Ok
        }

        Error(#(option.None, state)) -> {
          // Not a thenable — fulfill directly
          let #(h, jobs) =
            builtins_promise.fulfill_promise(state.heap, data_ref, resolution)

          State(
            ..state,
            heap: h,
            stack: [JsUndefined, ..rest_stack],
            pc: state.pc + 1,
            job_queue: list.append(state.job_queue, jobs),
          )
          |> Ok
        }
      }
    }
  }
}

/// Internal reject function — check already-resolved, then reject.
pub fn call_native_promise_reject_fn(
  state: State,
  data_ref: Ref,
  already_resolved_ref: Ref,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let reason = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  // Check if already resolved
  case heap.read_box(state.heap, already_resolved_ref) == Some(JsBool(True)) {
    True ->
      Ok(State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1))
    False -> {
      let h =
        heap.write(
          state.heap,
          already_resolved_ref,
          value.BoxSlot(value: JsBool(True)),
        )
      let state =
        builtins_promise.reject_promise(
          State(..state, heap: h),
          data_ref,
          reason,
        )
      Ok(State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1))
    }
  }
}

/// Promise.prototype.then(onFulfilled, onRejected)
pub fn call_native_promise_then(
  state: State,
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let on_fulfilled = case args {
    [f, ..] -> f
    [] -> JsUndefined
  }
  let on_rejected = case args {
    [_, r, ..] -> r
    _ -> JsUndefined
  }

  use this_ref <- result.try(case this {
    JsObject(this_ref) -> Ok(this_ref)
    _ -> {
      let #(h, err) =
        common.make_type_error(
          state.heap,
          state.builtins,
          "then called on non-promise",
        )
      Error(#(Thrown, err, h))
    }
  })

  case builtins_promise.get_data_ref(state.heap, this_ref) {
    Some(data_ref) -> {
      // Create child promise (the one returned by .then)
      let #(h, child_ref, _child_data_ref, child_resolve, child_reject) =
        new_promise_capability(state.heap, state.builtins)

      // Perform the .then logic
      let state =
        builtins_promise.perform_promise_then(
          State(..state, heap: h),
          data_ref,
          on_fulfilled,
          on_rejected,
          child_resolve,
          child_reject,
        )

      Ok(
        State(
          ..state,
          stack: [JsObject(child_ref), ..rest_stack],
          pc: state.pc + 1,
        ),
      )
    }
    None -> {
      state.throw_type_error(state, "then called on non-promise")
    }
  }
}

/// Promise.prototype.finally(onFinally) — per spec, wraps the handler
/// to preserve the resolution value. Creates wrapper functions that call
/// onFinally(), then pass through the original value/reason via
/// Promise.resolve(result).then(thunk).
pub fn call_native_promise_finally(
  state: State,
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let on_finally = case args {
    [f, ..] -> f
    [] -> JsUndefined
  }
  // If onFinally is not callable, pass-through (like .then(onFinally, onFinally))
  case coerce.is_callable_value(state.heap, on_finally) {
    False ->
      call_native_promise_then(
        state,
        this,
        [on_finally, on_finally],
        rest_stack,
      )
    True -> {
      // Create fulfill wrapper: calls onFinally(), then returns original value
      let #(h, fulfill_ref) =
        heap.alloc(
          state.heap,
          value.ObjectSlot(
            kind: value.NativeFunction(
              value.Call(value.PromiseFinallyFulfill(on_finally:)),
            ),
            properties: dict.new(),
            elements: elements.new(),
            prototype: Some(state.builtins.function.prototype),
            symbol_properties: dict.new(),
            extensible: True,
          ),
        )
      // Create reject wrapper: calls onFinally(), then re-throws original reason
      let #(h, reject_ref) =
        heap.alloc(
          h,
          value.ObjectSlot(
            kind: value.NativeFunction(
              value.Call(value.PromiseFinallyReject(on_finally:)),
            ),
            properties: dict.new(),
            elements: elements.new(),
            prototype: Some(state.builtins.function.prototype),
            symbol_properties: dict.new(),
            extensible: True,
          ),
        )
      call_native_promise_then(
        State(..state, heap: h),
        this,
        [JsObject(fulfill_ref), JsObject(reject_ref)],
        rest_stack,
      )
    }
  }
}

/// Promise.prototype.finally fulfill wrapper — called when promise fulfills.
/// Calls onFinally(), then Promise.resolve(result).then(() => original_value).
pub fn call_native_finally_fulfill(
  state: State,
  on_finally: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let original_value = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  // Call onFinally() with no arguments
  let result = state.call(state, on_finally, JsUndefined, [])
  case result {
    Ok(#(finally_result, new_state)) ->
      // Create Promise.resolve(finally_result).then(value_thunk)
      finally_chain_value(new_state, finally_result, original_value, rest_stack)
    Error(#(thrown, new_state)) ->
      // onFinally() threw — propagate the throw
      Error(#(Thrown, thrown, new_state.heap))
  }
}

/// Promise.prototype.finally reject wrapper — called when promise rejects.
/// Calls onFinally(), then Promise.resolve(result).then(() => { throw reason }).
pub fn call_native_finally_reject(
  state: State,
  on_finally: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let original_reason = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  // Call onFinally() with no arguments
  let result = state.call(state, on_finally, JsUndefined, [])
  case result {
    Ok(#(finally_result, new_state)) ->
      // Create Promise.resolve(finally_result).then(thrower)
      finally_chain_throw(
        new_state,
        finally_result,
        original_reason,
        rest_stack,
      )
    Error(#(thrown, new_state)) ->
      // onFinally() threw — propagate the throw (overrides original reason)
      Error(#(Thrown, thrown, new_state.heap))
  }
}

/// Create Promise.resolve(value).then(thunk) where thunk returns captured_value.
fn finally_chain_value(
  state: State,
  resolve_value: JsValue,
  captured_value: JsValue,
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  // Promise.resolve(resolve_value)
  let #(h, resolved_ref, _, resolve_fn, _) =
    new_promise_capability(state.heap, state.builtins)
  // Call resolve(resolve_value)
  let state1 =
    call_native_for_job(State(..state, heap: h), resolve_fn, [
      resolve_value,
    ])
  // Create the value thunk
  let #(h2, thunk_ref) =
    heap.alloc(
      state1.heap,
      value.ObjectSlot(
        kind: value.NativeFunction(
          value.Call(value.PromiseFinallyValueThunk(value: captured_value)),
        ),
        properties: dict.new(),
        elements: elements.new(),
        prototype: Some(state.builtins.function.prototype),
        symbol_properties: dict.new(),
        extensible: True,
      ),
    )
  // Chain .then(thunk) on the resolved promise
  call_native_promise_then(
    State(..state1, heap: h2),
    JsObject(resolved_ref),
    [JsObject(thunk_ref), JsUndefined],
    rest_stack,
  )
}

/// Create Promise.resolve(value).then(thrower) where thrower re-throws captured_reason.
fn finally_chain_throw(
  state: State,
  resolve_value: JsValue,
  captured_reason: JsValue,
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  // Promise.resolve(resolve_value)
  let #(h, resolved_ref, _, resolve_fn, _) =
    new_promise_capability(state.heap, state.builtins)
  // Call resolve(resolve_value)
  let state1 =
    call_native_for_job(State(..state, heap: h), resolve_fn, [
      resolve_value,
    ])
  // Create the thrower
  let #(h2, thrower_ref) =
    heap.alloc(
      state1.heap,
      value.ObjectSlot(
        kind: value.NativeFunction(
          value.Call(value.PromiseFinallyThrower(reason: captured_reason)),
        ),
        properties: dict.new(),
        elements: elements.new(),
        prototype: Some(state.builtins.function.prototype),
        symbol_properties: dict.new(),
        extensible: True,
      ),
    )
  // Chain .then(thrower) on the resolved promise
  call_native_promise_then(
    State(..state1, heap: h2),
    JsObject(resolved_ref),
    [JsObject(thrower_ref), JsUndefined],
    rest_stack,
  )
}

/// Promise.resolve(value) — if value is already a promise with same constructor,
/// return it. Otherwise create and resolve a new promise.
pub fn call_native_promise_resolve_static(
  state: State,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let val = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  // If val is already a Promise, return it directly
  case builtins_promise.is_promise(state.heap, val) {
    True -> Ok(State(..state, stack: [val, ..rest_stack], pc: state.pc + 1))
    False -> {
      // Create new promise and resolve it
      let #(h, promise_ref, data_ref) =
        builtins_promise.create_promise(
          state.heap,
          state.builtins.promise.prototype,
        )
      // Check for thenable
      case builtins_promise.get_thenable_then(State(..state, heap: h), val) {
        Ok(#(then_fn, state)) -> {
          let #(h, resolve_fn, reject_fn) =
            builtins_promise.create_resolving_functions(
              state.heap,
              state.builtins.function.prototype,
              promise_ref,
              data_ref,
            )
          let job =
            value.PromiseResolveThenableJob(
              thenable: val,
              then_fn:,
              resolve: resolve_fn,
              reject: reject_fn,
            )
          Ok(
            State(
              ..state,
              heap: h,
              stack: [JsObject(promise_ref), ..rest_stack],
              pc: state.pc + 1,
              job_queue: list.append(state.job_queue, [job]),
            ),
          )
        }
        Error(#(option.Some(thrown), state)) -> {
          // Getter threw — reject promise with the error
          let state = builtins_promise.reject_promise(state, data_ref, thrown)
          Ok(
            State(
              ..state,
              stack: [JsObject(promise_ref), ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        }
        Error(#(option.None, state)) -> {
          let #(h, jobs) =
            builtins_promise.fulfill_promise(state.heap, data_ref, val)
          Ok(
            State(
              ..state,
              heap: h,
              stack: [JsObject(promise_ref), ..rest_stack],
              pc: state.pc + 1,
              job_queue: list.append(state.job_queue, jobs),
            ),
          )
        }
      }
    }
  }
}

/// Promise.reject(reason) — create a new rejected promise.
pub fn call_native_promise_reject_static(
  state: State,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let reason = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  let #(h, promise_ref, data_ref) =
    builtins_promise.create_promise(
      state.heap,
      state.builtins.promise.prototype,
    )
  let state =
    builtins_promise.reject_promise(State(..state, heap: h), data_ref, reason)
  Ok(
    State(
      ..state,
      stack: [JsObject(promise_ref), ..rest_stack],
      pc: state.pc + 1,
    ),
  )
}

/// ES2024 §27.2.4.1 Promise.all(iterable)
///
/// Creates a promise that resolves when all input promises resolve,
/// or rejects when any input promise rejects. The resolved value is
/// an array of all the input promises' resolved values.
pub fn call_native_promise_all(
  state: State,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  // 1. Create result promise capability
  let #(h, promise_ref, data_ref, cap_resolve, cap_reject) =
    new_promise_capability(state.heap, state.builtins)

  // 2. Get iterable elements (array fast-path)
  let iterable = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  case get_iterable_elements(h, iterable) {
    Error(msg) -> {
      let #(h, err) = common.make_type_error(h, state.builtins, msg)
      let state =
        builtins_promise.reject_promise(State(..state, heap: h), data_ref, err)
      Ok(
        State(
          ..state,
          stack: [JsObject(promise_ref), ..rest_stack],
          pc: state.pc + 1,
        ),
      )
    }
    Ok(#(h, elements)) -> {
      let count = list.length(elements)
      // Empty iterable → resolve immediately with []
      case count {
        0 -> {
          let #(h, arr_ref) =
            common.alloc_array(h, [], state.builtins.array.prototype)
          let #(h, jobs) =
            builtins_promise.fulfill_promise(h, data_ref, JsObject(arr_ref))
          Ok(
            State(
              ..state,
              heap: h,
              stack: [JsObject(promise_ref), ..rest_stack],
              pc: state.pc + 1,
              job_queue: list.append(state.job_queue, jobs),
            ),
          )
        }
        _ -> {
          // Allocate shared state: values array + remaining counter
          // (remaining = count + 1; extra 1 for the "completion" step per spec)
          let #(h, values_ref, remaining_ref) =
            alloc_combinator_state(h, state.builtins, count)

          // For each element, call Promise.resolve(elem).then(resolveElement, reject)
          let state = State(..state, heap: h)
          let state =
            promise_all_loop(
              state,
              elements,
              0,
              remaining_ref,
              values_ref,
              cap_resolve,
              cap_reject,
            )

          // Decrement remaining by 1 (the completion step)
          let state =
            promise_combinator_decrement_and_maybe_resolve(
              state,
              remaining_ref,
              values_ref,
              cap_resolve,
            )

          Ok(
            State(
              ..state,
              stack: [JsObject(promise_ref), ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        }
      }
    }
  }
}

/// Loop body for Promise.all — process each element.
fn promise_all_loop(
  state: State,
  elements: List(JsValue),
  index: Int,
  remaining_ref: Ref,
  values_ref: Ref,
  cap_resolve: JsValue,
  cap_reject: JsValue,
) -> State {
  case elements {
    [] -> state
    [elem, ..rest] -> {
      // Create per-element already-called flag
      let #(h, already_called_ref) =
        heap.alloc(state.heap, value.BoxSlot(value: JsBool(False)))
      // Create resolve element function
      let #(h, resolve_fn_ref) =
        heap.alloc(
          h,
          ObjectSlot(
            kind: NativeFunction(
              value.Call(value.PromiseAllResolveElement(
                index:,
                remaining_ref:,
                values_ref:,
                already_called_ref:,
                resolve: cap_resolve,
                reject: cap_reject,
              )),
            ),
            properties: dict.from_list([
              #(Named("name"), common.fn_name_property("")),
              #(Named("length"), common.fn_length_property(1)),
            ]),
            elements: elements.new(),
            prototype: Some(state.builtins.function.prototype),
            symbol_properties: dict.new(),
            extensible: True,
          ),
        )
      // Resolve the element via Promise.resolve(elem)
      let state = State(..state, heap: h)
      let state =
        promise_resolve_and_then(
          state,
          elem,
          JsObject(resolve_fn_ref),
          cap_reject,
        )
      promise_all_loop(
        state,
        rest,
        index + 1,
        remaining_ref,
        values_ref,
        cap_resolve,
        cap_reject,
      )
    }
  }
}

/// ES2024 §27.2.4.5 Promise.race(iterable)
///
/// Returns a promise that settles with the first input promise to settle.
pub fn call_native_promise_race(
  state: State,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let #(h, promise_ref, data_ref, cap_resolve, cap_reject) =
    new_promise_capability(state.heap, state.builtins)

  let iterable = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  case get_iterable_elements(h, iterable) {
    Error(msg) -> {
      let #(h, err) = common.make_type_error(h, state.builtins, msg)
      let state =
        builtins_promise.reject_promise(State(..state, heap: h), data_ref, err)
      Ok(
        State(
          ..state,
          stack: [JsObject(promise_ref), ..rest_stack],
          pc: state.pc + 1,
        ),
      )
    }
    Ok(#(h, elements)) -> {
      // For each element: Promise.resolve(elem).then(resolve, reject)
      let state = State(..state, heap: h)
      let state = promise_race_loop(state, elements, cap_resolve, cap_reject)
      Ok(
        State(
          ..state,
          stack: [JsObject(promise_ref), ..rest_stack],
          pc: state.pc + 1,
        ),
      )
    }
  }
}

/// Loop body for Promise.race — each element uses the same resolve/reject.
fn promise_race_loop(
  state: State,
  elements: List(JsValue),
  cap_resolve: JsValue,
  cap_reject: JsValue,
) -> State {
  case elements {
    [] -> state
    [elem, ..rest] -> {
      let state = promise_resolve_and_then(state, elem, cap_resolve, cap_reject)
      promise_race_loop(state, rest, cap_resolve, cap_reject)
    }
  }
}

/// ES2024 §27.2.4.2 Promise.allSettled(iterable)
///
/// Returns a promise that resolves when all input promises settle (either
/// fulfill or reject). The result is an array of objects with shape
/// {status: "fulfilled", value: v} or {status: "rejected", reason: r}.
pub fn call_native_promise_all_settled(
  state: State,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let #(h, promise_ref, data_ref, cap_resolve, _cap_reject) =
    new_promise_capability(state.heap, state.builtins)

  let iterable = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  case get_iterable_elements(h, iterable) {
    Error(msg) -> {
      let #(h, err) = common.make_type_error(h, state.builtins, msg)
      let state =
        builtins_promise.reject_promise(State(..state, heap: h), data_ref, err)
      Ok(
        State(
          ..state,
          stack: [JsObject(promise_ref), ..rest_stack],
          pc: state.pc + 1,
        ),
      )
    }
    Ok(#(h, elements)) -> {
      let count = list.length(elements)
      case count {
        0 -> {
          let #(h, arr_ref) =
            common.alloc_array(h, [], state.builtins.array.prototype)
          let #(h, jobs) =
            builtins_promise.fulfill_promise(h, data_ref, JsObject(arr_ref))
          Ok(
            State(
              ..state,
              heap: h,
              stack: [JsObject(promise_ref), ..rest_stack],
              pc: state.pc + 1,
              job_queue: list.append(state.job_queue, jobs),
            ),
          )
        }
        _ -> {
          let #(h, values_ref, remaining_ref) =
            alloc_combinator_state(h, state.builtins, count)

          let state = State(..state, heap: h)
          let state =
            promise_all_settled_loop(
              state,
              elements,
              0,
              remaining_ref,
              values_ref,
              cap_resolve,
            )

          let state =
            promise_combinator_decrement_and_maybe_resolve(
              state,
              remaining_ref,
              values_ref,
              cap_resolve,
            )

          Ok(
            State(
              ..state,
              stack: [JsObject(promise_ref), ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        }
      }
    }
  }
}

/// Loop body for Promise.allSettled.
fn promise_all_settled_loop(
  state: State,
  elements: List(JsValue),
  index: Int,
  remaining_ref: Ref,
  values_ref: Ref,
  cap_resolve: JsValue,
) -> State {
  case elements {
    [] -> state
    [elem, ..rest] -> {
      // Create per-element already-called flags (one for resolve, one for reject)
      let #(h, already_called_resolve_ref) =
        heap.alloc(state.heap, value.BoxSlot(value: JsBool(False)))
      let #(h, already_called_reject_ref) =
        heap.alloc(h, value.BoxSlot(value: JsBool(False)))
      // Create resolve element function
      let #(h, resolve_fn_ref) =
        heap.alloc(
          h,
          ObjectSlot(
            kind: NativeFunction(
              value.Call(value.PromiseAllSettledResolveElement(
                index:,
                remaining_ref:,
                values_ref:,
                already_called_ref: already_called_resolve_ref,
                resolve: cap_resolve,
              )),
            ),
            properties: dict.from_list([
              #(Named("name"), common.fn_name_property("")),
              #(Named("length"), common.fn_length_property(1)),
            ]),
            elements: elements.new(),
            prototype: Some(state.builtins.function.prototype),
            symbol_properties: dict.new(),
            extensible: True,
          ),
        )
      // Create reject element function
      let #(h, reject_fn_ref) =
        heap.alloc(
          h,
          ObjectSlot(
            kind: NativeFunction(
              value.Call(value.PromiseAllSettledRejectElement(
                index:,
                remaining_ref:,
                values_ref:,
                already_called_ref: already_called_reject_ref,
                resolve: cap_resolve,
              )),
            ),
            properties: dict.from_list([
              #(Named("name"), common.fn_name_property("")),
              #(Named("length"), common.fn_length_property(1)),
            ]),
            elements: elements.new(),
            prototype: Some(state.builtins.function.prototype),
            symbol_properties: dict.new(),
            extensible: True,
          ),
        )
      let state = State(..state, heap: h)
      let state =
        promise_resolve_and_then(
          state,
          elem,
          JsObject(resolve_fn_ref),
          JsObject(reject_fn_ref),
        )
      promise_all_settled_loop(
        state,
        rest,
        index + 1,
        remaining_ref,
        values_ref,
        cap_resolve,
      )
    }
  }
}

/// ES2024 §27.2.4.3 Promise.any(iterable)
///
/// Returns a promise that resolves with the first input promise to fulfill.
/// If all input promises reject, rejects with an AggregateError.
pub fn call_native_promise_any(
  state: State,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let #(h, promise_ref, data_ref, cap_resolve, cap_reject) =
    new_promise_capability(state.heap, state.builtins)

  let iterable = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  case get_iterable_elements(h, iterable) {
    Error(msg) -> {
      let #(h, err) = common.make_type_error(h, state.builtins, msg)
      let state =
        builtins_promise.reject_promise(State(..state, heap: h), data_ref, err)
      Ok(
        State(
          ..state,
          stack: [JsObject(promise_ref), ..rest_stack],
          pc: state.pc + 1,
        ),
      )
    }
    Ok(#(h, elements)) -> {
      let count = list.length(elements)
      case count {
        0 -> {
          // Empty iterable → reject with AggregateError
          let #(h, err) =
            make_aggregate_error(
              h,
              state.builtins,
              [],
              "All promises were rejected",
            )
          let state =
            builtins_promise.reject_promise(
              State(..state, heap: h),
              data_ref,
              err,
            )
          Ok(
            State(
              ..state,
              stack: [JsObject(promise_ref), ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        }
        _ -> {
          let #(h, errors_ref, remaining_ref) =
            alloc_combinator_state(h, state.builtins, count)

          let state = State(..state, heap: h)
          let state =
            promise_any_loop(
              state,
              elements,
              0,
              remaining_ref,
              errors_ref,
              cap_resolve,
              cap_reject,
            )

          // Decrement remaining by 1 (the completion step)
          let state =
            promise_any_decrement_and_maybe_reject(
              state,
              remaining_ref,
              errors_ref,
              cap_reject,
            )

          Ok(
            State(
              ..state,
              stack: [JsObject(promise_ref), ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        }
      }
    }
  }
}

/// Loop body for Promise.any.
fn promise_any_loop(
  state: State,
  elements: List(JsValue),
  index: Int,
  remaining_ref: Ref,
  errors_ref: Ref,
  cap_resolve: JsValue,
  cap_reject: JsValue,
) -> State {
  case elements {
    [] -> state
    [elem, ..rest] -> {
      let #(h, already_called_ref) =
        heap.alloc(state.heap, value.BoxSlot(value: JsBool(False)))
      // Create reject element function
      let #(h, reject_fn_ref) =
        heap.alloc(
          h,
          ObjectSlot(
            kind: NativeFunction(
              value.Call(value.PromiseAnyRejectElement(
                index:,
                remaining_ref:,
                errors_ref:,
                already_called_ref:,
                resolve: cap_resolve,
                reject: cap_reject,
              )),
            ),
            properties: dict.from_list([
              #(Named("name"), common.fn_name_property("")),
              #(Named("length"), common.fn_length_property(1)),
            ]),
            elements: elements.new(),
            prototype: Some(state.builtins.function.prototype),
            symbol_properties: dict.new(),
            extensible: True,
          ),
        )
      let state = State(..state, heap: h)
      // For Promise.any: resolve handler is the capability resolve (first one wins),
      // reject handler is the per-element reject element function.
      let state =
        promise_resolve_and_then(
          state,
          elem,
          cap_resolve,
          JsObject(reject_fn_ref),
        )
      promise_any_loop(
        state,
        rest,
        index + 1,
        remaining_ref,
        errors_ref,
        cap_resolve,
        cap_reject,
      )
    }
  }
}

/// Shared helper: Promise.resolve(elem), then attach .then(on_fulfilled, on_rejected)
/// using perform_promise_then directly.
pub fn promise_resolve_and_then(
  state: State,
  elem: JsValue,
  on_fulfilled: JsValue,
  on_rejected: JsValue,
) -> State {
  let h = state.heap
  // If elem is already a promise, use it directly; otherwise wrap via Promise.resolve logic
  case builtins_promise.is_promise(h, elem) {
    True -> {
      let assert JsObject(elem_ref) = elem
      case builtins_promise.get_data_ref(h, elem_ref) {
        Some(elem_data_ref) -> {
          // Create child promise for the .then chain
          let #(h, _, _, child_resolve, child_reject) =
            new_promise_capability(h, state.builtins)
          builtins_promise.perform_promise_then(
            State(..state, heap: h),
            elem_data_ref,
            on_fulfilled,
            on_rejected,
            child_resolve,
            child_reject,
          )
        }
        None -> state
      }
    }
    False -> {
      // Wrap non-promise value: create a resolved promise, then attach .then
      let #(h, wrap_ref, wrap_data_ref) =
        builtins_promise.create_promise(h, state.builtins.promise.prototype)
      // Check for thenable
      case builtins_promise.get_thenable_then(State(..state, heap: h), elem) {
        Ok(#(then_fn, state)) -> {
          let #(h, resolve_fn, reject_fn) =
            builtins_promise.create_resolving_functions(
              state.heap,
              state.builtins.function.prototype,
              wrap_ref,
              wrap_data_ref,
            )
          let job =
            value.PromiseResolveThenableJob(
              thenable: elem,
              then_fn:,
              resolve: resolve_fn,
              reject: reject_fn,
            )
          // Create child for .then
          let #(h, _, _, child_resolve, child_reject) =
            new_promise_capability(h, state.builtins)
          let state =
            builtins_promise.perform_promise_then(
              State(
                ..state,
                heap: h,
                job_queue: list.append(state.job_queue, [job]),
              ),
              wrap_data_ref,
              on_fulfilled,
              on_rejected,
              child_resolve,
              child_reject,
            )
          state
        }
        Error(#(Some(thrown), state)) -> {
          // Thenable getter threw — reject the wrapper promise
          let state =
            builtins_promise.reject_promise(state, wrap_data_ref, thrown)
          // Still attach .then to propagate to result
          let #(h, _, _, child_resolve, child_reject) =
            new_promise_capability(state.heap, state.builtins)
          builtins_promise.perform_promise_then(
            State(..state, heap: h),
            wrap_data_ref,
            on_fulfilled,
            on_rejected,
            child_resolve,
            child_reject,
          )
        }
        Error(#(None, state)) -> {
          // Not a thenable — fulfill the wrapper promise directly
          let #(h, jobs) =
            builtins_promise.fulfill_promise(state.heap, wrap_data_ref, elem)
          // Now attach .then
          let #(h, _, _, child_resolve, child_reject) =
            new_promise_capability(h, state.builtins)
          builtins_promise.perform_promise_then(
            State(
              ..state,
              heap: h,
              job_queue: list.append(state.job_queue, jobs),
            ),
            wrap_data_ref,
            on_fulfilled,
            on_rejected,
            child_resolve,
            child_reject,
          )
        }
      }
    }
  }
}

/// Extract elements from an iterable value (array fast-path).
/// Returns Error(message) if not iterable.
pub fn get_iterable_elements(
  h: Heap,
  iterable: JsValue,
) -> Result(#(Heap, List(JsValue)), String) {
  case iterable {
    JsObject(ref) ->
      case heap.read_array_like(h, ref) {
        Some(#(length, elements)) ->
          Ok(#(h, extract_elements_loop(elements, 0, length, [])))
        None -> Error(object.inspect(iterable, h) <> " is not iterable")
      }
    _ -> Error(object.inspect(iterable, h) <> " is not iterable")
  }
}

/// Promise.all resolve element function — stores value and checks if all done.
pub fn call_native_promise_all_resolve_element(
  state: State,
  args: List(JsValue),
  rest_stack: List(JsValue),
  index: Int,
  remaining_ref: Ref,
  values_ref: Ref,
  already_called_ref: Ref,
  resolve: JsValue,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  // Check and set already-called flag
  case heap.read_box(state.heap, already_called_ref) == Some(JsBool(True)) {
    True ->
      Ok(State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1))
    False -> {
      let h =
        heap.write(
          state.heap,
          already_called_ref,
          value.BoxSlot(value: JsBool(True)),
        )
      let val = case args {
        [v, ..] -> v
        [] -> JsUndefined
      }
      // Store value at index in the values array
      let h = set_array_element(h, values_ref, index, val)
      // Decrement remaining and maybe resolve
      let state =
        promise_combinator_decrement_and_maybe_resolve(
          State(..state, heap: h),
          remaining_ref,
          values_ref,
          resolve,
        )
      Ok(State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1))
    }
  }
}

/// Promise.allSettled resolve element — stores {status:"fulfilled", value:v}.
pub fn call_native_promise_all_settled_resolve_element(
  state: State,
  args: List(JsValue),
  rest_stack: List(JsValue),
  index: Int,
  remaining_ref: Ref,
  values_ref: Ref,
  already_called_ref: Ref,
  resolve: JsValue,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case heap.read_box(state.heap, already_called_ref) == Some(JsBool(True)) {
    True ->
      Ok(State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1))
    False -> {
      let h =
        heap.write(
          state.heap,
          already_called_ref,
          value.BoxSlot(value: JsBool(True)),
        )
      let val = case args {
        [v, ..] -> v
        [] -> JsUndefined
      }
      // Create {status: "fulfilled", value: val}
      let #(h, obj_ref) =
        common.alloc_pojo(h, state.builtins.object.prototype, [
          #("status", value.builtin_property(JsString("fulfilled"))),
          #("value", value.builtin_property(val)),
        ])
      let h = set_array_element(h, values_ref, index, JsObject(obj_ref))
      let state =
        promise_combinator_decrement_and_maybe_resolve(
          State(..state, heap: h),
          remaining_ref,
          values_ref,
          resolve,
        )
      Ok(State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1))
    }
  }
}

/// Promise.allSettled reject element — stores {status:"rejected", reason:r}.
pub fn call_native_promise_all_settled_reject_element(
  state: State,
  args: List(JsValue),
  rest_stack: List(JsValue),
  index: Int,
  remaining_ref: Ref,
  values_ref: Ref,
  already_called_ref: Ref,
  resolve: JsValue,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case heap.read_box(state.heap, already_called_ref) == Some(JsBool(True)) {
    True ->
      Ok(State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1))
    False -> {
      let h =
        heap.write(
          state.heap,
          already_called_ref,
          value.BoxSlot(value: JsBool(True)),
        )
      let reason = case args {
        [v, ..] -> v
        [] -> JsUndefined
      }
      // Create {status: "rejected", reason: reason}
      let #(h, obj_ref) =
        common.alloc_pojo(h, state.builtins.object.prototype, [
          #("status", value.builtin_property(JsString("rejected"))),
          #("reason", value.builtin_property(reason)),
        ])
      let h = set_array_element(h, values_ref, index, JsObject(obj_ref))
      let state =
        promise_combinator_decrement_and_maybe_resolve(
          State(..state, heap: h),
          remaining_ref,
          values_ref,
          resolve,
        )
      Ok(State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1))
    }
  }
}

/// Promise.any reject element — collects error and maybe rejects with AggregateError.
pub fn call_native_promise_any_reject_element(
  state: State,
  args: List(JsValue),
  rest_stack: List(JsValue),
  index: Int,
  remaining_ref: Ref,
  errors_ref: Ref,
  already_called_ref: Ref,
  reject: JsValue,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case heap.read_box(state.heap, already_called_ref) == Some(JsBool(True)) {
    True ->
      Ok(State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1))
    False -> {
      let h =
        heap.write(
          state.heap,
          already_called_ref,
          value.BoxSlot(value: JsBool(True)),
        )
      let reason = case args {
        [v, ..] -> v
        [] -> JsUndefined
      }
      let h = set_array_element(h, errors_ref, index, reason)
      let state =
        promise_any_decrement_and_maybe_reject(
          State(..state, heap: h),
          remaining_ref,
          errors_ref,
          reject,
        )
      Ok(State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1))
    }
  }
}

/// Shared helper: decrement remaining counter; if it reaches 0, call resolve with values array.
fn promise_combinator_decrement_and_maybe_resolve(
  state: State,
  remaining_ref: Ref,
  values_ref: Ref,
  resolve: JsValue,
) -> State {
  case heap.read(state.heap, remaining_ref) {
    Some(value.BoxSlot(value: JsNumber(Finite(n)))) -> {
      let new_count = n -. 1.0
      let h =
        heap.write(
          state.heap,
          remaining_ref,
          value.BoxSlot(value: JsNumber(Finite(new_count))),
        )
      case new_count <=. 0.0 {
        True -> {
          // All elements resolved — call resolve(values)
          let state = State(..state, heap: h)
          case
            state.call(state, resolve, JsUndefined, [
              JsObject(values_ref),
            ])
          {
            Ok(#(_, after_state)) -> after_state
            Error(#(_, after_state)) -> after_state
          }
        }
        False -> State(..state, heap: h)
      }
    }
    _ -> state
  }
}

/// Shared helper for Promise.any: decrement remaining; if 0, reject with AggregateError.
fn promise_any_decrement_and_maybe_reject(
  state: State,
  remaining_ref: Ref,
  errors_ref: Ref,
  reject: JsValue,
) -> State {
  case heap.read(state.heap, remaining_ref) {
    Some(value.BoxSlot(value: JsNumber(Finite(n)))) -> {
      let new_count = n -. 1.0
      let h =
        heap.write(
          state.heap,
          remaining_ref,
          value.BoxSlot(value: JsNumber(Finite(new_count))),
        )
      case new_count <=. 0.0 {
        True -> {
          // All elements rejected — reject with AggregateError
          let errors = extract_array_args(h, errors_ref)
          let #(h, err) =
            make_aggregate_error(
              h,
              state.builtins,
              errors,
              "All promises were rejected",
            )
          let state = State(..state, heap: h)
          case state.call(state, reject, JsUndefined, [err]) {
            Ok(#(_, after_state)) -> after_state
            Error(#(_, after_state)) -> after_state
          }
        }
        False -> State(..state, heap: h)
      }
    }
    _ -> state
  }
}

/// Set an element in a heap-allocated array at a specific index.
pub fn set_array_element(
  h: Heap,
  arr_ref: Ref,
  index: Int,
  val: JsValue,
) -> Heap {
  use slot <- heap.update(h, arr_ref)
  case slot {
    ObjectSlot(kind: ArrayObject(length:), elements:, ..) ->
      ObjectSlot(
        ..slot,
        elements: elements.set(elements, index, val),
        kind: ArrayObject(int.max(length, index + 1)),
      )
    _ -> slot
  }
}

/// Create an AggregateError with an errors array and message.
pub fn make_aggregate_error(
  h: Heap,
  b: Builtins,
  errors: List(JsValue),
  message: String,
) -> #(Heap, JsValue) {
  let #(h, errors_arr_ref) = common.alloc_array(h, errors, b.array.prototype)
  let #(h, ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: OrdinaryObject,
        properties: dict.from_list([
          #(Named("message"), value.builtin_property(JsString(message))),
          #(Named("errors"), value.builtin_property(JsObject(errors_arr_ref))),
        ]),
        elements: elements.new(),
        prototype: Some(b.aggregate_error.prototype),
        symbol_properties: dict.new(),
        extensible: True,
      ),
    )
  #(h, JsObject(ref))
}

/// Helper: create a resolved promise wrapping a value.
/// Note: _jobs is always [] because this is a brand-new promise with no reactions.
pub fn create_resolved_promise(
  h: Heap,
  builtins: Builtins,
  val: JsValue,
) -> #(Heap, JsValue, Ref) {
  let #(h, promise_ref, data_ref) =
    builtins_promise.create_promise(h, builtins.promise.prototype)
  let #(h, _jobs) = builtins_promise.fulfill_promise(h, data_ref, val)
  #(h, JsObject(promise_ref), data_ref)
}

// ============================================================================
// Private helpers
// ============================================================================

/// Call a function for its side effects (return value discarded).
/// Used by finally chain helpers.
fn call_native_for_job(
  state: State,
  target: JsValue,
  args: List(JsValue),
) -> State {
  case state.call(state, target, JsUndefined, args) {
    Ok(#(_, new_state)) -> new_state
    Error(#(_, new_state)) -> new_state
  }
}

/// Extract array elements from a heap-allocated array object.
fn extract_array_args(h: Heap, ref: Ref) -> List(JsValue) {
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
