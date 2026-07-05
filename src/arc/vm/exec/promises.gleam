import arc/vm/builtins/common.{type Builtins}
import arc/vm/builtins/helpers
import arc/vm/builtins/iter_protocol
import arc/vm/builtins/object as builtins_object
import arc/vm/builtins/promise as builtins_promise
import arc/vm/exec/job_call
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/key.{Index, Named}
import arc/vm/limits
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/ops/operators
import arc/vm/state.{type Heap, type State, type StepExit, State, Threw}
import arc/vm/value.{
  type IteratorRecord, type JsValue, type ObjectKey, type Ref, ArrayObject,
  Finite, JsBool, JsObject, JsString, JsUndefined, ObjectSlot, OrdinaryObject,
  StringPropKey, SymbolPropKey,
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

/// Push the result promise onto the stack in place of the consumed operands
/// and advance past the current instruction — the common tail of every
/// promise-returning opcode/builtin.
pub fn return_promise(
  state: State(host),
  promise_ref: Ref,
  rest_stack: List(JsValue),
) -> Result(State(host), e) {
  Ok(
    State(
      ..state,
      stack: [JsObject(promise_ref), ..rest_stack],
      pc: state.pc + 1,
    ),
  )
}

// ============================================================================
// Promise combinator machinery — spec records (ES2024 §27.2.4.1-.5)
// ============================================================================

/// PromiseCapability Record (§27.2.1.1): the result promise plus its
/// resolve/reject functions. For custom constructors these are whatever
/// functions the constructor handed to the GetCapabilitiesExecutor.
type Capability {
  Capability(promise: JsValue, resolve: JsValue, reject: JsValue)
}

/// One IteratorStepValue (§7.4.8) outcome: exhausted, or produced a value.
type IterStep {
  IterDone
  IterValue(JsValue)
}

/// The spec's iterator [[Done]] flag as seen by a combinator's error path:
/// does the thrown value leave an iterator that still needs IteratorClose?
pub type IterState {
  /// The iterator is still open — the caller must IteratorClose it before
  /// rejecting the capability promise.
  IteratorOpen
  /// The iterator is done (exhausted, or already closed by the step that
  /// threw) — reject with the thrown value as-is, no close.
  IteratorDone
}

/// Error shape inside PerformPromiseAll/AllSettled/Any/Race.
type CombinatorError(host) {
  CombinatorError(thrown: JsValue, iterator: IterState, state: State(host))
}

/// `use` adapter lifting a #(thrown, state) error into the combinator loop
/// error shape with the iterator still open (caller must close it).
fn open_iter(
  r: Result(#(a, State(host)), #(JsValue, State(host))),
  cont: fn(a, State(host)) -> Result(b, CombinatorError(host)),
) -> Result(b, CombinatorError(host)) {
  case r {
    Ok(#(v, state)) -> cont(v, state)
    Error(#(err, state)) -> Error(CombinatorError(err, IteratorOpen, state))
  }
}

/// Same as open_iter, but marks the iterator done — per IteratorStepValue
/// (§7.4.8) any abrupt completion while stepping marks the iterator done
/// (no close).
fn done_iter(
  r: Result(#(a, State(host)), #(JsValue, State(host))),
  cont: fn(a, State(host)) -> Result(b, CombinatorError(host)),
) -> Result(b, CombinatorError(host)) {
  case r {
    Ok(#(v, state)) -> cont(v, state)
    Error(#(err, state)) -> Error(CombinatorError(err, IteratorDone, state))
  }
}

/// §27.2.1.5 NewPromiseCapability(C). The intrinsic %Promise% constructor
/// takes the (unobservable) fast path; any other value must be a constructor
/// and is invoked as `new C(executor)` with a GetCapabilitiesExecutor that
/// captures the resolve/reject pair.
fn new_capability_from_constructor(
  state: State(host),
  c: JsValue,
) -> Result(#(Capability, State(host)), #(JsValue, State(host))) {
  case c == JsObject(state.builtins.promise.constructor) {
    True -> {
      let #(h, cap) =
        builtins_promise.new_promise_capability(state.heap, state.builtins)
      Ok(#(
        Capability(
          promise: JsObject(cap.promise),
          resolve: cap.resolve,
          reject: cap.reject,
        ),
        State(..state, heap: h),
      ))
    }
    False ->
      case object.is_constructor(state.heap, c) {
        False ->
          Error(state.type_error_value(
            state,
            "Promise capability requires a constructor",
          ))
        True -> {
          let #(h, resolve_box) =
            heap.alloc(state.heap, value.BoxSlot(value: JsUndefined))
          let #(h, reject_box) =
            heap.alloc(h, value.BoxSlot(value: JsUndefined))
          let #(h, executor_ref) =
            common.alloc_call_fn(
              h,
              state.builtins.function.prototype,
              value.PromiseCapabilityExecutor(resolve_box:, reject_box:),
              "",
              2,
              constructible: False,
            )
          use #(promise_obj, state) <- result.try(
            state.construct(State(..state, heap: h), c, [JsObject(executor_ref)]),
          )
          let resolve =
            heap.read_box(state.heap, resolve_box) |> option.unwrap(JsUndefined)
          let reject =
            heap.read_box(state.heap, reject_box) |> option.unwrap(JsUndefined)
          case
            helpers.is_callable(state.heap, resolve)
            && helpers.is_callable(state.heap, reject)
          {
            True ->
              Ok(#(Capability(promise: promise_obj, resolve:, reject:), state))
            False ->
              Error(state.type_error_value(
                state,
                "Promise resolve or reject function is not callable",
              ))
          }
        }
      }
  }
}

/// §27.2.4.1.2 GetPromiseResolve(C): Get(C, "resolve"), require callable.
fn get_promise_resolve(
  state: State(host),
  c: JsValue,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  use #(resolve_fn, state) <- result.try(object.get_value_of(
    state,
    c,
    Named("resolve"),
  ))
  case helpers.is_callable(state.heap, resolve_fn) {
    True -> Ok(#(resolve_fn, state))
    False ->
      Error(state.type_error_value(state, "Promise resolve is not a function"))
  }
}

/// §7.4.8 IteratorStepValue: call next(), check done, read value. Any abrupt
/// completion marks the iterator done (the caller must not IteratorClose).
fn iterator_step_value_native(
  state: State(host),
  rec: IteratorRecord,
) -> Result(#(IterStep, State(host)), CombinatorError(host)) {
  use result_val, state <- done_iter(
    state.call(state, rec.next_method, rec.iterator, []),
  )
  case result_val {
    JsObject(result_ref) -> {
      use done_val, state <- done_iter(object.get_value(
        state,
        result_ref,
        Named("done"),
        result_val,
      ))
      case value.is_truthy(done_val) {
        True -> Ok(#(IterDone, state))
        False -> {
          use v, state <- done_iter(object.get_value(
            state,
            result_ref,
            Named("value"),
            result_val,
          ))
          Ok(#(IterValue(v), state))
        }
      }
    }
    _ -> {
      let #(err, state) =
        state.type_error_value(state, "Iterator result is not an object")
      Error(CombinatorError(err, IteratorDone, state))
    }
  }
}

/// The scaffold every promise combinator (`Promise.all`/`allSettled`/`any`/
/// `race` and the keyed variants) shares: NewPromiseCapability(this) — whose
/// abrupt completion throws synchronously — then run `perform`, whose abrupt
/// completion goes through IfAbruptRejectPromise (Call(cap.[[Reject]], «err»),
/// which itself may throw), and finally push cap.[[Promise]].
fn with_capability(
  state: State(host),
  this: JsValue,
  rest_stack: List(JsValue),
  perform: fn(State(host), Capability) ->
    Result(State(host), #(JsValue, State(host))),
) -> Result(State(host), StepExit(host)) {
  // Drop the args from the operand stack before re-entrant calls; the
  // capability promise is pushed when the combinator completes.
  let state = State(..state, stack: rest_stack)
  case new_capability_from_constructor(state, this) {
    Error(#(err, state)) -> Error(Threw(err, state))
    Ok(#(cap, state)) ->
      case perform(state, cap) {
        Ok(state) -> push_combinator_result(state, cap, rest_stack)
        Error(#(err, state)) ->
          // IfAbruptRejectPromise: Perform ? Call(cap.[[Reject]], undefined, «err»).
          case state.call(state, cap.reject, JsUndefined, [err]) {
            Ok(#(_reject_result, state)) ->
              push_combinator_result(state, cap, rest_stack)
            Error(#(thrown, state)) -> Error(Threw(thrown, state))
          }
      }
  }
}

/// `with_capability` plus the iterable half of §27.2.4.1 steps 1-8:
/// GetPromiseResolve(C), GetIterator(iterable), then run `perform` — closing
/// the iterator first when the perform loop left it open.
fn with_spec_combinator(
  state: State(host),
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
  perform: fn(State(host), IteratorRecord, JsValue, Capability, JsValue) ->
    Result(State(host), CombinatorError(host)),
) -> Result(State(host), StepExit(host)) {
  use state, cap <- with_capability(state, this, rest_stack)
  let iterable = helpers.first_arg_or_undefined(args)
  combinator_prepare_and_perform(state, this, iterable, cap, perform)
}

fn push_combinator_result(
  state: State(host),
  cap: Capability,
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  Ok(State(..state, stack: [cap.promise, ..rest_stack], pc: state.pc + 1))
}

/// GetPromiseResolve + GetIterator + the perform loop, mapping a loop error
/// with an open iterator through IteratorClose.
fn combinator_prepare_and_perform(
  state: State(host),
  c: JsValue,
  iterable: JsValue,
  cap: Capability,
  perform: fn(State(host), IteratorRecord, JsValue, Capability, JsValue) ->
    Result(State(host), CombinatorError(host)),
) -> Result(State(host), #(JsValue, State(host))) {
  use #(promise_resolve, state) <- result.try(get_promise_resolve(state, c))
  use #(rec, state) <- result.try(iter_protocol.get_iterator_sync(
    state,
    iterable,
  ))
  case perform(state, rec, c, cap, promise_resolve) {
    Ok(state) -> Ok(state)
    Error(CombinatorError(thrown:, iterator: IteratorDone, state:)) ->
      Error(#(thrown, state))
    Error(CombinatorError(thrown:, iterator: IteratorOpen, state:)) ->
      Error(iter_protocol.close_and_throw(state, rec.iterator, thrown))
  }
}

/// Shared iteration loop for PerformPromiseAll/AllSettled/Any/Race
/// (§27.2.4.1.1 step 4): step the iterator; on done run `on_done`; per value
/// call promiseResolve.call(C, value), build the per-element handlers, and
/// Invoke(nextPromise, "then", «onFulfilled, onRejected»).
fn perform_combinator_loop(
  state: State(host),
  rec: IteratorRecord,
  c: JsValue,
  promise_resolve: JsValue,
  index: Int,
  make_handlers: fn(Heap(host), Int) -> #(Heap(host), JsValue, JsValue),
  on_done: fn(State(host)) -> Result(State(host), CombinatorError(host)),
) -> Result(State(host), CombinatorError(host)) {
  use #(step, state) <- result.try(iterator_step_value_native(state, rec))
  case step {
    IterDone -> on_done(state)
    IterValue(v) -> {
      // Step 4.h: nextPromise = ? Call(promiseResolve, C, «nextValue»).
      use next_promise, state <- open_iter(
        state.call(state, promise_resolve, c, [v]),
      )
      let #(h, on_fulfilled, on_rejected) = make_handlers(state.heap, index)
      let state = State(..state, heap: h)
      // Step 4.s: ? Invoke(nextPromise, "then", «onFulfilled, onRejected»).
      use then_fn, state <- open_iter(object.get_value_of(
        state,
        next_promise,
        Named("then"),
      ))
      use _then_result, state <- open_iter(
        state.call(state, then_fn, next_promise, [on_fulfilled, on_rejected]),
      )
      perform_combinator_loop(
        state,
        rec,
        c,
        promise_resolve,
        index + 1,
        make_handlers,
        on_done,
      )
    }
  }
}

/// IteratorStep done arm for all/allSettled: remaining -= 1; at zero,
/// ? Call(cap.[[Resolve]], undefined, «valuesArray»).
fn final_resolve_values(
  state: State(host),
  remaining_ref: Ref,
  values_ref: Ref,
  resolve: JsValue,
) -> Result(State(host), CombinatorError(host)) {
  let #(h, is_zero) = heap.decrement_counter(state.heap, remaining_ref)
  let state = State(..state, heap: h)
  case is_zero {
    False -> Ok(state)
    True ->
      case state.call(state, resolve, JsUndefined, [JsObject(values_ref)]) {
        Ok(#(_resolve_result, state)) -> Ok(state)
        Error(#(err, state)) -> Error(CombinatorError(err, IteratorDone, state))
      }
  }
}

/// Once-only guard for combinator element functions: if already_called is set,
/// skip (returning undefined); otherwise set it, extract first arg, run body.
/// The body returns the element function's return value (the spec's
/// `? Call(cap.[[Resolve]], ...)` result when the counter hits zero, undefined
/// otherwise) — abrupt completions from that call propagate to our caller.
///
/// The body receives a `State` whose heap already carries the already-called
/// write, so no caller can accidentally keep using the pre-write `state`.
fn with_element_once(
  state: State(host),
  args: List(JsValue),
  rest_stack: List(JsValue),
  already_called_ref: Ref,
  body: fn(State(host), JsValue) ->
    Result(#(State(host), JsValue), StepExit(host)),
) -> Result(State(host), StepExit(host)) {
  use #(state, return_value) <- result.map(
    case heap.read_box(state.heap, already_called_ref) == Some(JsBool(True)) {
      True -> Ok(#(state, JsUndefined))
      False -> {
        let h =
          heap.write(
            state.heap,
            already_called_ref,
            value.BoxSlot(value: JsBool(True)),
          )
        body(
          State(..state, heap: h),
          list.first(args) |> result.unwrap(JsUndefined),
        )
      }
    },
  )
  State(..state, stack: [return_value, ..rest_stack], pc: state.pc + 1)
}

// ============================================================================
// Promise native function implementations
// ============================================================================

/// new Promise(executor) — create promise, call executor(resolve, reject),
/// catch a throwing executor with the capability's reject function.
pub fn call_native_promise_constructor(
  state: State(host),
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  let executor = helpers.first_arg_or_undefined(args)

  // Verify executor is callable
  case helpers.is_callable(state.heap, executor) {
    False -> {
      state.throw_type_error(state, "Promise resolver is not a function")
    }
    True -> {
      let #(h, cap) =
        builtins_promise.new_promise_capability(state.heap, state.builtins)
      // Run executor inline — its return value is discarded, the promise is the result.
      let new_state = State(..state, heap: h, stack: rest_stack)
      case
        state.call(new_state, executor, JsUndefined, [
          cap.resolve,
          cap.reject,
        ])
      {
        Ok(#(_, after_state)) ->
          return_promise(after_state, cap.promise, rest_stack)
        Error(#(thrown, after_state)) ->
          // §27.2.3.1 step 10.a: a throwing executor goes through the reject
          // FUNCTION — `? Call(resolvingFunctions.[[Reject]], undefined,
          // «thrown»)` — never RejectPromise directly. Its [[AlreadyResolved]]
          // flag makes `resolve(x); throw e` keep the resolution instead of
          // overwriting it with a rejection.
          case state.call(after_state, cap.reject, JsUndefined, [thrown]) {
            Ok(#(_, after_state)) ->
              return_promise(after_state, cap.promise, rest_stack)
            // Step 10.b: the reject call's own abrupt completion propagates.
            Error(#(reject_thrown, after_state)) ->
              Error(Threw(reject_thrown, after_state))
          }
      }
    }
  }
}

/// Internal resolve function — check already-resolved, then fulfill/reject.
pub fn call_native_promise_resolve_fn(
  state: State(host),
  promise_ref: Ref,
  data_ref: Ref,
  already_resolved_ref: Ref,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  let resolution = helpers.first_arg_or_undefined(args)

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
        let #(err, state) =
          state.type_error_value(
            State(..state, heap: h),
            "Chaining cycle detected for promise",
          )

        let state = builtins_promise.reject_promise(state, data_ref, err)

        Ok(State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1))
      })

      // §27.2.1.3.2 steps 8-13: thenable → job, throwing `then` getter →
      // reject, anything else → fulfill.
      let state =
        builtins_promise.resolve_promise(
          State(..state, heap: h),
          promise_ref,
          data_ref,
          resolution,
        )
      Ok(State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1))
    }
  }
}

/// Internal reject function — check already-resolved, then reject.
pub fn call_native_promise_reject_fn(
  state: State(host),
  data_ref: Ref,
  already_resolved_ref: Ref,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  let reason = helpers.first_arg_or_undefined(args)
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

/// §27.2.5.4 Promise.prototype.then(onFulfilled, onRejected).
///
/// Steps 3-4: the result promise comes from
/// NewPromiseCapability(? SpeciesConstructor(this, %Promise%)), so a
/// subclass instance's .then/.catch/.finally returns a subclass promise
/// (`class P extends Promise {}` → `P.resolve(1).then(f) instanceof P`).
pub fn call_native_promise_then(
  state: State(host),
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  let on_fulfilled = helpers.first_arg_or_undefined(args)
  let on_rejected = helpers.list_at(args, 1) |> option.unwrap(JsUndefined)

  use this_ref <- result.try(case this {
    JsObject(this_ref) -> Ok(this_ref)
    _ -> state.throw_type_error(state, "then called on non-promise")
  })

  case builtins_promise.get_data_ref(state.heap, this_ref) {
    Some(data_ref) -> {
      // Step 3: C = ? SpeciesConstructor(promise, %Promise%).
      use #(ctor_ref, state) <- result.try(
        state.rethrow(object.species_constructor(
          state,
          this,
          state.builtins.promise.constructor,
        )),
      )
      // Step 4: resultCapability = ? NewPromiseCapability(C).
      use #(cap, state) <- result.try(
        state.rethrow(new_capability_from_constructor(state, JsObject(ctor_ref))),
      )
      // Step 5: PerformPromiseThen(promise, onFulfilled, onRejected, cap).
      let state =
        builtins_promise.perform_promise_then(
          state,
          data_ref,
          on_fulfilled,
          on_rejected,
          cap.resolve,
          cap.reject,
        )

      Ok(State(..state, stack: [cap.promise, ..rest_stack], pc: state.pc + 1))
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
  state: State(host),
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  let on_finally = helpers.first_arg_or_undefined(args)
  // If onFinally is not callable, pass-through (like .then(onFinally, onFinally))
  case helpers.is_callable(state.heap, on_finally) {
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
        common.alloc_wrapper(
          state.heap,
          value.NativeFunction(
            value.Call(value.PromiseFinallyFulfill(on_finally:)),
            constructible: False,
          ),
          state.builtins.function.prototype,
        )
      // Create reject wrapper: calls onFinally(), then re-throws original reason
      let #(h, reject_ref) =
        common.alloc_wrapper(
          h,
          value.NativeFunction(
            value.Call(value.PromiseFinallyReject(on_finally:)),
            constructible: False,
          ),
          state.builtins.function.prototype,
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
  state: State(host),
  on_finally: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  call_native_finally(state, on_finally, args, rest_stack, fn(v) {
    value.PromiseFinallyValueThunk(value: v)
  })
}

/// Promise.prototype.finally reject wrapper — called when promise rejects.
/// Calls onFinally(), then Promise.resolve(result).then(() => { throw reason }).
pub fn call_native_finally_reject(
  state: State(host),
  on_finally: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  call_native_finally(state, on_finally, args, rest_stack, fn(r) {
    value.PromiseFinallyThrower(reason: r)
  })
}

/// Shared body for the finally wrappers: calls onFinally(), then chains
/// Promise.resolve(result).then(make_handler(original settled value)).
fn call_native_finally(
  state: State(host),
  on_finally: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
  make_handler: fn(JsValue) -> value.CallNativeFn,
) -> Result(State(host), StepExit(host)) {
  let original = helpers.first_arg_or_undefined(args)
  // Call onFinally() with no arguments
  let result = state.call(state, on_finally, JsUndefined, [])
  case result {
    Ok(#(finally_result, new_state)) ->
      finally_chain(
        new_state,
        finally_result,
        make_handler(original),
        rest_stack,
      )
    Error(#(thrown, new_state)) ->
      // onFinally() threw — propagate the throw (overrides original reason)
      Error(Threw(thrown, new_state))
  }
}

/// Create Promise.resolve(resolve_value).then(handler) where handler is the
/// given native call (a value thunk or a re-thrower).
fn finally_chain(
  state: State(host),
  resolve_value: JsValue,
  native_call: value.CallNativeFn,
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  // Promise.resolve(resolve_value)
  let #(h, cap) =
    builtins_promise.new_promise_capability(state.heap, state.builtins)
  // Call resolve(resolve_value)
  let state1 =
    job_call.call_settlement_fn(State(..state, heap: h), cap.resolve, [
      resolve_value,
    ])
  // Create the handler
  let #(h2, handler_ref) =
    common.alloc_wrapper(
      state1.heap,
      value.NativeFunction(value.Call(native_call), constructible: False),
      state.builtins.function.prototype,
    )
  // Chain .then(handler) on the resolved promise
  call_native_promise_then(
    State(..state1, heap: h2),
    JsObject(cap.promise),
    [JsObject(handler_ref), JsUndefined],
    rest_stack,
  )
}

/// Promise.resolve(value) — if value is already a promise with same constructor,
/// return it. Otherwise create and resolve a new promise.
pub fn call_native_promise_resolve_static(
  state: State(host),
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  let val = helpers.first_arg_or_undefined(args)
  // §27.2.4.7 step 2: If C is not an Object, throw a TypeError.
  use <- bool.lazy_guard(
    case this {
      JsObject(_) -> False
      _ -> True
    },
    fn() {
      state.throw_type_error(state, "Promise.resolve called on non-object")
    },
  )
  // §27.2.4.7.1 PromiseResolve step 1: if x is a promise whose constructor
  // is C, return x unchanged.
  case builtins_promise.is_promise(state.heap, val) {
    True ->
      case object.get_value_of(state, val, Named("constructor")) {
        Error(#(err, state)) -> Error(Threw(err, state))
        Ok(#(ctor, state)) ->
          case ctor == this {
            True ->
              Ok(State(..state, stack: [val, ..rest_stack], pc: state.pc + 1))
            False ->
              resolve_static_with_constructor(state, this, val, rest_stack)
          }
      }
    False -> resolve_static_with_constructor(state, this, val, rest_stack)
  }
}

/// PromiseResolve(C, x) steps 2-4 for a value that is not C's own promise:
/// the intrinsic %Promise% keeps the direct internal path; custom
/// constructors go through NewPromiseCapability + Call(cap.[[Resolve]], x).
fn resolve_static_with_constructor(
  state: State(host),
  c: JsValue,
  val: JsValue,
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  case c == JsObject(state.builtins.promise.constructor) {
    True -> resolve_static_intrinsic(state, val, rest_stack)
    False ->
      case new_capability_from_constructor(state, c) {
        Error(#(err, state)) -> Error(Threw(err, state))
        Ok(#(cap, state)) ->
          case state.call(state, cap.resolve, JsUndefined, [val]) {
            Ok(#(_resolve_result, state)) ->
              Ok(
                State(
                  ..state,
                  stack: [cap.promise, ..rest_stack],
                  pc: state.pc + 1,
                ),
              )
            Error(#(thrown, state)) -> Error(Threw(thrown, state))
          }
      }
  }
}

/// Intrinsic %Promise% fast path for Promise.resolve: create a native
/// promise and resolve it with `val` directly (thenables via job).
fn resolve_static_intrinsic(
  state: State(host),
  val: JsValue,
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  {
    {
      // Create new promise and resolve it (thenable → job, throwing `then`
      // getter → reject, anything else → fulfill).
      let #(h, promise_ref, data_ref) =
        builtins_promise.create_promise(
          state.heap,
          state.builtins.promise.prototype,
        )
      builtins_promise.resolve_promise(
        State(..state, heap: h),
        promise_ref,
        data_ref,
        val,
      )
      |> return_promise(promise_ref, rest_stack)
    }
  }
}

/// Promise.reject(reason) — create a new rejected promise.
pub fn call_native_promise_reject_static(
  state: State(host),
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  let reason = helpers.first_arg_or_undefined(args)
  // §27.2.4.6: C = this; the intrinsic %Promise% keeps the direct path,
  // custom constructors go through NewPromiseCapability + cap.[[Reject]].
  case this == JsObject(state.builtins.promise.constructor) {
    True -> {
      let #(h, promise_ref, data_ref) =
        builtins_promise.create_promise(
          state.heap,
          state.builtins.promise.prototype,
        )
      builtins_promise.reject_promise(State(..state, heap: h), data_ref, reason)
      |> return_promise(promise_ref, rest_stack)
    }
    False ->
      case new_capability_from_constructor(state, this) {
        Error(#(err, state)) -> Error(Threw(err, state))
        Ok(#(cap, state)) ->
          case state.call(state, cap.reject, JsUndefined, [reason]) {
            Ok(#(_reject_result, state)) ->
              Ok(
                State(
                  ..state,
                  stack: [cap.promise, ..rest_stack],
                  pc: state.pc + 1,
                ),
              )
            Error(#(thrown, state)) -> Error(Threw(thrown, state))
          }
      }
  }
}

/// ES2024 §27.2.4.1 Promise.all(iterable)
///
/// Creates a promise that resolves when all input promises resolve,
/// or rejects when any input promise rejects. The resolved value is
/// an array of all the input promises' resolved values.
pub fn call_native_promise_all(
  state: State(host),
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  use state, rec, c, cap, promise_resolve <- with_spec_combinator(
    state,
    this,
    args,
    rest_stack,
  )
  // §27.2.4.1.1 PerformPromiseAll
  let #(h, values_ref) =
    common.alloc_array(state.heap, [], state.builtins.array.prototype)
  let #(h, remaining_ref) = heap.alloc_counter(h, 1)
  let b = state.builtins
  perform_combinator_loop(
    State(..state, heap: h),
    rec,
    c,
    promise_resolve,
    0,
    fn(h, index) {
      // Step 4.j: append undefined to values.
      let h = set_array_element(h, values_ref, index, JsUndefined)
      // Steps 4.k-4.q: Promise.all Resolve Element function.
      let #(h, already_called_ref) =
        heap.alloc(h, value.BoxSlot(value: JsBool(False)))
      let #(h, resolve_fn) =
        alloc_closure(
          h,
          b,
          value.PromiseAllResolveElement(
            index:,
            remaining_ref:,
            values_ref:,
            already_called_ref:,
            resolve: cap.resolve,
            reject: cap.reject,
          ),
        )
      // Step 4.r: remainingElementsCount += 1.
      let h = heap.increment_counter(h, remaining_ref)
      #(h, resolve_fn, cap.reject)
    },
    fn(state) {
      final_resolve_values(state, remaining_ref, values_ref, cap.resolve)
    },
  )
}

/// ES2024 §27.2.4.5 Promise.race(iterable)
///
/// Returns a promise that settles with the first input promise to settle.
pub fn call_native_promise_race(
  state: State(host),
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  use state, rec, c, cap, promise_resolve <- with_spec_combinator(
    state,
    this,
    args,
    rest_stack,
  )
  // §27.2.4.5.1 PerformPromiseRace — every element shares the capability's
  // resolve/reject; iterator exhaustion settles nothing.
  perform_combinator_loop(
    state,
    rec,
    c,
    promise_resolve,
    0,
    fn(h, _index) { #(h, cap.resolve, cap.reject) },
    fn(state) { Ok(state) },
  )
}

/// ES2024 §27.2.4.2 Promise.allSettled(iterable)
///
/// Returns a promise that resolves when all input promises settle (either
/// fulfill or reject). The result is an array of objects with shape
/// {status: "fulfilled", value: v} or {status: "rejected", reason: r}.
pub fn call_native_promise_all_settled(
  state: State(host),
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  use state, rec, c, cap, promise_resolve <- with_spec_combinator(
    state,
    this,
    args,
    rest_stack,
  )
  // §27.2.4.2.1 PerformPromiseAllSettled
  let #(h, values_ref) =
    common.alloc_array(state.heap, [], state.builtins.array.prototype)
  let #(h, remaining_ref) = heap.alloc_counter(h, 1)
  let b = state.builtins
  perform_combinator_loop(
    State(..state, heap: h),
    rec,
    c,
    promise_resolve,
    0,
    fn(h, index) {
      let h = set_array_element(h, values_ref, index, JsUndefined)
      // Step 4.j: ONE alreadyCalled record shared by the resolve and reject
      // element functions of this iteration.
      let #(h, already_called_ref) =
        heap.alloc(h, value.BoxSlot(value: JsBool(False)))
      let #(h, resolve_fn) =
        alloc_closure(
          h,
          b,
          value.PromiseAllSettledResolveElement(
            index:,
            remaining_ref:,
            values_ref:,
            already_called_ref:,
            resolve: cap.resolve,
          ),
        )
      let #(h, reject_fn) =
        alloc_closure(
          h,
          b,
          value.PromiseAllSettledRejectElement(
            index:,
            remaining_ref:,
            values_ref:,
            already_called_ref:,
            resolve: cap.resolve,
          ),
        )
      let h = heap.increment_counter(h, remaining_ref)
      #(h, resolve_fn, reject_fn)
    },
    fn(state) {
      final_resolve_values(state, remaining_ref, values_ref, cap.resolve)
    },
  )
}

/// ES2024 §27.2.4.3 Promise.any(iterable)
///
/// Returns a promise that resolves with the first input promise to fulfill.
/// If all input promises reject, rejects with an AggregateError.
pub fn call_native_promise_any(
  state: State(host),
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  use state, rec, c, cap, promise_resolve <- with_spec_combinator(
    state,
    this,
    args,
    rest_stack,
  )
  // §27.2.4.3.1 PerformPromiseAny
  let #(h, errors_ref) =
    common.alloc_array(state.heap, [], state.builtins.array.prototype)
  let #(h, remaining_ref) = heap.alloc_counter(h, 1)
  let b = state.builtins
  perform_combinator_loop(
    State(..state, heap: h),
    rec,
    c,
    promise_resolve,
    0,
    fn(h, index) {
      let h = set_array_element(h, errors_ref, index, JsUndefined)
      let #(h, already_called_ref) =
        heap.alloc(h, value.BoxSlot(value: JsBool(False)))
      let #(h, reject_fn) =
        alloc_closure(
          h,
          b,
          value.PromiseAnyRejectElement(
            index:,
            remaining_ref:,
            errors_ref:,
            already_called_ref:,
            resolve: cap.resolve,
            reject: cap.reject,
          ),
        )
      let h = heap.increment_counter(h, remaining_ref)
      #(h, cap.resolve, reject_fn)
    },
    fn(state) {
      // Step 4.d.ii: remaining -= 1; at zero, throw AggregateError — the
      // abrupt completion reaches IfAbruptRejectPromise with [[Done]] = true.
      let #(h, is_zero) = heap.decrement_counter(state.heap, remaining_ref)
      let errors = heap.read_array_values(h, errors_ref)
      case is_zero {
        False -> Ok(State(..state, heap: h))
        True -> {
          let #(h, err) =
            make_aggregate_error(
              h,
              state.builtins,
              errors,
              "All promises were rejected",
            )
          Error(CombinatorError(err, IteratorDone, State(..state, heap: h)))
        }
      }
    },
  )
}

/// Await-dictionary proposal: Promise.allKeyed(promises) and
/// Promise.allSettledKeyed(promises).
///
/// Spec steps (both methods, `settled` selects the ~all-settled~ variant):
///   1. Let ctor be the this value.
///   2. Let promiseCapability be ? NewPromiseCapability(ctor).
///   3. Let promiseResolve be Completion(GetPromiseResolve(ctor)).
///   4. IfAbruptRejectPromise(promiseResolve, promiseCapability).
///   5. If promises is not an Object, reject with a TypeError.
///   6. Let result be Completion(PerformPromiseAllKeyed(variant, promises,
///      ctor, promiseCapability, promiseResolve)).
///   7. IfAbruptRejectPromise(result, promiseCapability).
///   8. Return promiseCapability.[[Promise]].
pub fn call_native_promise_all_keyed(
  state: State(host),
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
  settled settled: Bool,
) -> Result(State(host), StepExit(host)) {
  // Steps 2 and 4/5/7 (NewPromiseCapability + IfAbruptRejectPromise) are
  // `with_capability`; steps 3/5/6 are the perform below.
  use state, cap <- with_capability(state, this, rest_stack)
  let promises = helpers.first_arg_or_undefined(args)
  perform_promise_all_keyed(state, this, promises, cap, settled)
}

/// PerformPromiseAllKeyed steps 1-5 plus the method's steps 3 and 5
/// (GetPromiseResolve and the is-Object check): collect [[OwnPropertyKeys]],
/// allocate the shared keys/values stores and remaining counter, then run
/// the per-key loop.
fn perform_promise_all_keyed(
  state: State(host),
  c: JsValue,
  promises: JsValue,
  cap: Capability,
  settled: Bool,
) -> Result(State(host), #(JsValue, State(host))) {
  // Method step 3: GetPromiseResolve(ctor).
  use #(promise_resolve, state) <- result.try(get_promise_resolve(state, c))
  case promises {
    JsObject(promises_ref) -> {
      // Step 1: Let allKeys be ? promises.[[OwnPropertyKeys]]() — trap-aware.
      use #(all_keys, state) <- result.try(builtins_object.own_property_keys(
        state,
        promises_ref,
      ))
      // Steps 2-4: keys/values lists (shared, mutable from element fns) and
      // the Record { [[Value]]: 1 } remaining-elements counter.
      let #(h, keys_ref) =
        common.alloc_array(state.heap, [], state.builtins.array.prototype)
      let #(h, values_ref) =
        common.alloc_array(h, [], state.builtins.array.prototype)
      let #(h, remaining_ref) = heap.alloc_counter(h, 1)
      let loop =
        KeyedLoop(
          c:,
          promises:,
          promises_ref:,
          cap:,
          promise_resolve:,
          settled:,
          keys_ref:,
          values_ref:,
          remaining_ref:,
        )
      perform_keyed_loop(State(..state, heap: h), loop, all_keys, 0)
    }
    // Method step 5: promises is not an Object — reject with TypeError.
    _ ->
      Error(state.type_error_value(
        state,
        "Promise keyed combinator argument must be an object",
      ))
  }
}

/// Shared per-call context of the PerformPromiseAllKeyed key loop.
type KeyedLoop {
  KeyedLoop(
    c: JsValue,
    promises: JsValue,
    promises_ref: Ref,
    cap: Capability,
    promise_resolve: JsValue,
    settled: Bool,
    keys_ref: Ref,
    values_ref: Ref,
    remaining_ref: Ref,
  )
}

/// PerformPromiseAllKeyed steps 6-8: for each own key, check the descriptor
/// is present and enumerable, Get the value, wrap via promiseResolve, attach
/// the keyed element handlers, then on loop exit decrement the counter and
/// resolve with the keyed result object if it hit zero.
fn perform_keyed_loop(
  state: State(host),
  loop: KeyedLoop,
  all_keys: List(ObjectKey),
  index: Int,
) -> Result(State(host), #(JsValue, State(host))) {
  case all_keys {
    [] -> {
      // Step 7: remainingElementsCount -= 1.
      let #(h, is_zero) = heap.decrement_counter(state.heap, loop.remaining_ref)
      let state = State(..state, heap: h)
      case is_zero {
        False -> Ok(state)
        // Step 8: result = CreateKeyedPromiseCombinatorResultObject(keys,
        // values); ? Call(cap.[[Resolve]], undefined, «result»).
        True -> {
          let #(h, result_obj) =
            create_keyed_result(state.heap, loop.keys_ref, loop.values_ref)
          let state = State(..state, heap: h)
          use #(_resolve_result, state) <- result.map(
            state.call(state, loop.cap.resolve, JsUndefined, [result_obj]),
          )
          state
        }
      }
    }
    [key, ..rest] -> {
      // Step 6.a: Let propertyDesc be ? promises.[[GetOwnProperty]](key).
      use #(desc, state) <- result.try(builtins_object.own_property_keyed(
        state,
        loop.promises_ref,
        key,
      ))
      // Step 6.b: skip absent / non-enumerable properties.
      let enumerable =
        option.map(desc, value.prop_enumerable) |> option.unwrap(False)
      case enumerable {
        False -> perform_keyed_loop(state, loop, rest, index)
        True -> {
          // Step 6.b.i: Let propertyValue be ? Get(promises, key).
          use #(prop_value, state) <- result.try(get_keyed_value(
            state,
            loop,
            key,
          ))
          // Steps 6.b.ii-iii: append key to keys, undefined to values.
          let h =
            set_array_element(
              state.heap,
              loop.keys_ref,
              index,
              builtins_object.object_key_value(key),
            )
          let h = set_array_element(h, loop.values_ref, index, JsUndefined)
          // Step 6.b.iv: nextPromise = ? Call(promiseResolve, ctor,
          // «propertyValue»).
          use #(next_promise, state) <- result.try(
            state.call(State(..state, heap: h), loop.promise_resolve, loop.c, [
              prop_value,
            ]),
          )
          // Steps 6.b.v-ix: alreadyCalled record + onFulfilled closure.
          let b = state.builtins
          let #(h, already_called_ref) =
            heap.alloc(state.heap, value.BoxSlot(value: JsBool(False)))
          let #(h, on_fulfilled) =
            alloc_closure(
              h,
              b,
              value.PromiseKeyedElement(
                index:,
                remaining_ref: loop.remaining_ref,
                keys_ref: loop.keys_ref,
                values_ref: loop.values_ref,
                already_called_ref:,
                resolve: loop.cap.resolve,
                status_field: case loop.settled {
                  True -> Some(value.Fulfilled)
                  False -> None
                },
              ),
            )
          // Steps 6.b.x-xi: onRejected is cap.[[Reject]] for ~all~, a keyed
          // rejected-element closure (same alreadyCalled record) for
          // ~all-settled~.
          let #(h, on_rejected) = case loop.settled {
            False -> #(h, loop.cap.reject)
            True ->
              alloc_closure(
                h,
                b,
                value.PromiseKeyedElement(
                  index:,
                  remaining_ref: loop.remaining_ref,
                  keys_ref: loop.keys_ref,
                  values_ref: loop.values_ref,
                  already_called_ref:,
                  resolve: loop.cap.resolve,
                  status_field: Some(value.Rejected),
                ),
              )
          }
          // Step 6.b.xii: remainingElementsCount += 1.
          let h = heap.increment_counter(h, loop.remaining_ref)
          let state = State(..state, heap: h)
          // Step 6.b.xiii: ? Invoke(nextPromise, "then",
          // «onFulfilled, onRejected»).
          use #(then_fn, state) <- result.try(object.get_value_of(
            state,
            next_promise,
            Named("then"),
          ))
          use #(_then_result, state) <- result.try(
            state.call(state, then_fn, next_promise, [on_fulfilled, on_rejected]),
          )
          // Step 6.b.xiv: index += 1.
          perform_keyed_loop(state, loop, rest, index + 1)
        }
      }
    }
  }
}

/// Step 6.b.i Get(promises, key) for a property key produced by
/// [[OwnPropertyKeys]].
fn get_keyed_value(
  state: State(host),
  loop: KeyedLoop,
  key: ObjectKey,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  case key {
    StringPropKey(pkey:, ..) ->
      object.get_value(state, loop.promises_ref, pkey, loop.promises)
    SymbolPropKey(sym) ->
      object.get_symbol_value(state, loop.promises_ref, sym, loop.promises)
  }
}

/// CreateKeyedPromiseCombinatorResultObject(keys, values): a null-prototype
/// ordinary object with one enumerable data property per collected key.
///
/// The spec requires the result object's own keys to come back in keys-list
/// (property-creation) order; [[OwnPropertyKeys]] sorts named keys by their
/// creation seq (Property.seq), and the properties are built below in
/// keys-list order, so each gets an ascending seq and the order round-trips.
fn create_keyed_result(
  h: Heap(host),
  keys_ref: Ref,
  values_ref: Ref,
) -> #(Heap(host), JsValue) {
  let read_list = heap.read_array_values(h, _)
  let keys = read_list(keys_ref)
  let values = read_list(values_ref)
  // Step 2: Let obj be OrdinaryObjectCreate(null).
  let #(h, obj_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: OrdinaryObject,
        properties: dict.new(),
        elements: elements.new(),
        prototype: None,
        symbol_properties: [],
        extensible: True,
      ),
    )
  // Step 3: CreateDataPropertyOrThrow(obj, keys[i], values[i]) for each i.
  let h =
    list.zip(keys, values)
    |> list.fold(h, fn(h, kv) {
      let #(k, v) = kv
      case k {
        JsString(s) ->
          object.create_data_property(h, obj_ref, key.canonical_key(s), v)
        value.JsSymbol(sym) ->
          object.define_symbol_property(h, obj_ref, sym, value.data_property(v))
        // Keys come from [[OwnPropertyKeys]] — String/Symbol only.
        _ -> h
      }
    })
  #(h, JsObject(obj_ref))
}

/// Keyed combinator element handler (the fulfilledSteps / rejectedSteps
/// closures of PerformPromiseAllKeyed): once-only, store the (possibly
/// status-wrapped) value at the captured index, decrement the counter, and
/// at zero resolve the capability with the keyed result object.
pub fn call_native_promise_keyed_element(
  state: State(host),
  args: List(JsValue),
  rest_stack: List(JsValue),
  index: Int,
  remaining_ref: Ref,
  keys_ref: Ref,
  values_ref: Ref,
  already_called_ref: Ref,
  resolve: JsValue,
  status_field: option.Option(value.SettledOutcome),
) -> Result(State(host), StepExit(host)) {
  let b = state.builtins
  use state, val <- with_element_once(
    state,
    args,
    rest_stack,
    already_called_ref,
  )
  // ~all~ stores the raw value; ~all-settled~ wraps it in
  // {status, value/reason} with %Object.prototype%.
  let #(h, stored) = case status_field {
    None -> #(state.heap, val)
    Some(outcome) -> {
      let #(status, field) = value.settled_keys(outcome)
      let #(h, obj_ref) =
        common.alloc_pojo(state.heap, b.object.prototype, [
          #("status", value.data_property(JsString(status))),
          #(field, value.data_property(val)),
        ])
      #(h, JsObject(obj_ref))
    }
  }
  let h = set_array_element(h, values_ref, index, stored)
  // remainingElementsCount -= 1; at zero,
  // ? Call(cap.[[Resolve]], undefined, «result»).
  use state <- promise_combinator_decrement(
    State(..state, heap: h),
    remaining_ref,
  )
  let #(h, result_obj) = create_keyed_result(state.heap, keys_ref, values_ref)
  let state = State(..state, heap: h)
  case state.call(state, resolve, JsUndefined, [result_obj]) {
    Ok(#(call_result, after_state)) -> Ok(#(after_state, call_result))
    Error(#(thrown, after_state)) -> Error(Threw(thrown, after_state))
  }
}

/// §27.2.1.5.1 GetCapabilitiesExecutor — stores the resolve/reject pair the
/// custom constructor hands to its executor; throws if either is already set.
pub fn call_native_promise_capability_executor(
  state: State(host),
  resolve_box: Ref,
  reject_box: Ref,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  let resolve_set = heap.read_box(state.heap, resolve_box) != Some(JsUndefined)
  let reject_set = heap.read_box(state.heap, reject_box) != Some(JsUndefined)
  case resolve_set || reject_set {
    True ->
      state.throw_type_error(
        state,
        "Promise executor has already been invoked with non-undefined arguments",
      )
    False -> {
      let resolve = helpers.first_arg_or_undefined(args)
      let reject = list.first(list.drop(args, 1)) |> result.unwrap(JsUndefined)
      let h = heap.write(state.heap, resolve_box, value.BoxSlot(value: resolve))
      let h = heap.write(h, reject_box, value.BoxSlot(value: reject))
      Ok(
        State(
          ..state,
          heap: h,
          stack: [JsUndefined, ..rest_stack],
          pc: state.pc + 1,
        ),
      )
    }
  }
}

/// Promise.all resolve element function — stores value and checks if all done.
pub fn call_native_promise_all_resolve_element(
  state: State(host),
  args: List(JsValue),
  rest_stack: List(JsValue),
  index: Int,
  remaining_ref: Ref,
  values_ref: Ref,
  already_called_ref: Ref,
  resolve: JsValue,
) -> Result(State(host), StepExit(host)) {
  use state, val <- with_element_once(
    state,
    args,
    rest_stack,
    already_called_ref,
  )
  let h = set_array_element(state.heap, values_ref, index, val)
  promise_combinator_decrement_and_maybe_resolve(
    State(..state, heap: h),
    remaining_ref,
    values_ref,
    resolve,
  )
}

/// Promise.allSettled element handler — stores {status, value/reason}.
/// Resolve passes `Fulfilled`; reject passes `Rejected`.
pub fn call_native_promise_all_settled_element(
  state: State(host),
  args: List(JsValue),
  rest_stack: List(JsValue),
  index: Int,
  remaining_ref: Ref,
  values_ref: Ref,
  already_called_ref: Ref,
  resolve: JsValue,
  outcome: value.SettledOutcome,
) -> Result(State(host), StepExit(host)) {
  let #(status, field) = value.settled_keys(outcome)
  use state, val <- with_element_once(
    state,
    args,
    rest_stack,
    already_called_ref,
  )
  let #(h, obj_ref) =
    common.alloc_pojo(state.heap, state.builtins.object.prototype, [
      #("status", value.data_property(JsString(status))),
      #(field, value.data_property(val)),
    ])
  let h = set_array_element(h, values_ref, index, JsObject(obj_ref))
  promise_combinator_decrement_and_maybe_resolve(
    State(..state, heap: h),
    remaining_ref,
    values_ref,
    resolve,
  )
}

/// Promise.any reject element — collects error and maybe rejects with AggregateError.
pub fn call_native_promise_any_reject_element(
  state: State(host),
  args: List(JsValue),
  rest_stack: List(JsValue),
  index: Int,
  remaining_ref: Ref,
  errors_ref: Ref,
  already_called_ref: Ref,
  reject: JsValue,
) -> Result(State(host), StepExit(host)) {
  use state, reason <- with_element_once(
    state,
    args,
    rest_stack,
    already_called_ref,
  )
  let h = set_array_element(state.heap, errors_ref, index, reason)
  promise_any_decrement_and_maybe_reject(
    State(..state, heap: h),
    remaining_ref,
    errors_ref,
    reject,
  )
}

/// Shared helper: decrement remaining counter; if it reaches 0, run `on_zero`
/// and return its outcome (the element function's return value, or a thrown
/// completion). Otherwise return undefined per spec.
fn promise_combinator_decrement(
  state: State(host),
  remaining_ref: Ref,
  on_zero: fn(State(host)) -> Result(#(State(host), JsValue), StepExit(host)),
) -> Result(#(State(host), JsValue), StepExit(host)) {
  let #(h, is_zero) = heap.decrement_counter(state.heap, remaining_ref)
  let state = State(..state, heap: h)
  case is_zero {
    True -> on_zero(state)
    False -> Ok(#(state, JsUndefined))
  }
}

/// Shared helper: decrement remaining counter; if it reaches 0, return
/// `? Call(resolve, undefined, «values»)` — propagating throws from a
/// user-supplied capability resolve function.
fn promise_combinator_decrement_and_maybe_resolve(
  state: State(host),
  remaining_ref: Ref,
  values_ref: Ref,
  resolve: JsValue,
) -> Result(#(State(host), JsValue), StepExit(host)) {
  // All elements resolved — call resolve(values)
  use state <- promise_combinator_decrement(state, remaining_ref)
  case state.call(state, resolve, JsUndefined, [JsObject(values_ref)]) {
    Ok(#(call_result, after_state)) -> Ok(#(after_state, call_result))
    Error(#(thrown, after_state)) -> Error(Threw(thrown, after_state))
  }
}

/// Shared helper for Promise.any: decrement remaining; if 0, return
/// `? Call(reject, undefined, «AggregateError»)` — propagating throws from a
/// user-supplied capability reject function.
fn promise_any_decrement_and_maybe_reject(
  state: State(host),
  remaining_ref: Ref,
  errors_ref: Ref,
  reject: JsValue,
) -> Result(#(State(host), JsValue), StepExit(host)) {
  // All elements rejected — reject with AggregateError
  use state <- promise_combinator_decrement(state, remaining_ref)
  let errors = heap.read_array_values(state.heap, errors_ref)
  let #(h, err) =
    make_aggregate_error(
      state.heap,
      state.builtins,
      errors,
      "All promises were rejected",
    )
  let state = State(..state, heap: h)
  case state.call(state, reject, JsUndefined, [err]) {
    Ok(#(call_result, after_state)) -> Ok(#(after_state, call_result))
    Error(#(thrown, after_state)) -> Error(Threw(thrown, after_state))
  }
}

/// Set an element in a heap-allocated array at a specific index.
pub fn set_array_element(
  h: Heap(host),
  arr_ref: Ref,
  index: Int,
  val: JsValue,
) -> Heap(host) {
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
  h: Heap(host),
  b: Builtins,
  errors: List(JsValue),
  message: String,
) -> #(Heap(host), JsValue) {
  let #(h, errors_arr_ref) = common.alloc_array(h, errors, b.array.prototype)
  let #(h, ref) =
    heap.alloc(
      h,
      ObjectSlot(
        // [[ErrorData]] internal slot — AggregateError is an Error instance.
        kind: value.ErrorObject(stack: ""),
        properties: dict.from_list([
          #(Named("message"), value.builtin_property(JsString(message))),
          #(Named("errors"), value.builtin_property(JsObject(errors_arr_ref))),
        ]),
        elements: elements.new(),
        prototype: Some(b.aggregate_error.prototype),
        symbol_properties: [],
        extensible: True,
      ),
    )
  #(h, JsObject(ref))
}

/// Shared await scaffold for async functions and async generators:
/// wrap the awaited value in a resolved promise if needed, alloc
/// fulfill/reject resume wrappers via `make_resume(is_reject)`, create a
/// child promise + resolving functions, and attach .then(fulfill, reject).
pub fn setup_await(
  state: State(host),
  awaited: JsValue,
  make_resume: fn(Bool) -> value.CallNativeFn,
) -> State(host) {
  let b = state.builtins
  let existing = case awaited {
    JsObject(ref) -> heap.read_promise_data_ref(state.heap, ref)
    _ -> None
  }
  let #(state, promise_data) = case existing {
    Some(dr) -> #(state, dr)
    // Await §27.7.5.3 step 2: PromiseResolve(%Promise%, value). Thenables
    // must resolve through their own `then` (PromiseResolveThenableJob), a
    // throwing `then` getter rejects, plain values fulfill directly.
    None -> {
      let #(h, promise_ref, dr) =
        builtins_promise.create_promise(state.heap, b.promise.prototype)
      let state =
        builtins_promise.resolve_promise(
          State(..state, heap: h),
          promise_ref,
          dr,
          awaited,
        )
      #(state, dr)
    }
  }
  let h = state.heap
  let #(h, on_fulfill) = alloc_resume(h, b, make_resume(False))
  let #(h, on_reject) = alloc_resume(h, b, make_resume(True))
  let #(h, child_ref, child_data) =
    builtins_promise.create_promise(h, b.promise.prototype)
  let #(h, child_resolve, child_reject) =
    builtins_promise.create_resolving_functions(
      h,
      b.function.prototype,
      child_ref,
      child_data,
    )
  builtins_promise.perform_promise_then(
    State(..state, heap: h),
    promise_data,
    JsObject(on_fulfill),
    JsObject(on_reject),
    child_resolve,
    child_reject,
  )
}

fn alloc_resume(
  h: Heap(host),
  b: Builtins,
  native: value.CallNativeFn,
) -> #(Heap(host), Ref) {
  common.alloc_wrapper(
    h,
    value.NativeFunction(value.Call(native), constructible: False),
    b.function.prototype,
  )
}

// ============================================================================
// %AsyncFromSyncIteratorPrototype% — ES2024 §27.1.4
// ============================================================================

pub type AsyncFromSyncKind {
  AfsNext
  AfsReturn
  AfsThrow
}

/// Shared body for %AsyncFromSyncIteratorPrototype%.next/return/throw.
pub fn call_native_async_from_sync(
  state: State(host),
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
  kind: AsyncFromSyncKind,
) -> Result(State(host), StepExit(host)) {
  let #(h, cap) =
    builtins_promise.new_promise_capability(state.heap, state.builtins)
  let state = State(..state, heap: h)

  let state = case
    do_async_from_sync(
      state,
      this,
      args,
      kind,
      cap.data,
      cap.resolve,
      cap.reject,
    )
  {
    Ok(state) -> state
    Error(#(thrown, state)) ->
      builtins_promise.reject_promise(state, cap.data, thrown)
  }
  return_promise(state, cap.promise, rest_stack)
}

fn do_async_from_sync(
  state: State(host),
  this: JsValue,
  args: List(JsValue),
  kind: AsyncFromSyncKind,
  data_ref: Ref,
  cap_resolve: JsValue,
  cap_reject: JsValue,
) -> Result(State(host), #(JsValue, State(host))) {
  use #(sync_iter, sync_next) <- result.try(case this {
    JsObject(this_ref) ->
      case heap.read(state.heap, this_ref) {
        Some(ObjectSlot(
          kind: value.AsyncFromSyncIteratorObject(sync_iter:, sync_next:),
          ..,
        )) -> Ok(#(sync_iter, sync_next))
        _ -> coerce.thrown_type_error(state, "not an Async-from-Sync Iterator")
      }
    _ -> coerce.thrown_type_error(state, "not an Async-from-Sync Iterator")
  })

  let sync_iter_val = JsObject(sync_iter)
  // §27.1.6.2.1 .next() uses the sync iterator record's cached [[NextMethod]]
  // (IteratorNext, §7.4.3) — no re-Get. return/throw are looked up per call
  // (§27.1.6.2.2/.3 GetMethod).
  use #(method, state) <- result.try(case kind {
    AfsNext -> Ok(#(sync_next, state))
    AfsReturn ->
      object.get_value(state, sync_iter, Named("return"), sync_iter_val)
    AfsThrow ->
      object.get_value(state, sync_iter, Named("throw"), sync_iter_val)
  })

  case kind, helpers.is_callable(state.heap, method) {
    AfsReturn, False -> {
      let arg = list.first(args) |> result.unwrap(JsUndefined)
      let #(h, iter_result) =
        common.create_iter_result(state.heap, state.builtins, arg, True)
      Ok(builtins_promise.fulfill_promise(
        State(..state, heap: h),
        data_ref,
        iter_result,
      ))
    }
    AfsThrow, False -> {
      use state <- result.try(iterator_close_normal(state, sync_iter))
      coerce.thrown_type_error(
        state,
        "The iterator does not provide a 'throw' method",
      )
    }
    _, _ -> {
      use #(result_val, state) <- result.try(state.call(
        state,
        method,
        sync_iter_val,
        args,
      ))
      use result_ref <- result.try(case result_val {
        JsObject(r) -> Ok(r)
        _ -> coerce.thrown_type_error(state, "Iterator result is not an object")
      })
      let close_on_rejection = case kind {
        AfsReturn -> False
        AfsNext | AfsThrow -> True
      }
      async_from_sync_continuation(
        state,
        result_ref,
        sync_iter,
        close_on_rejection,
        cap_resolve,
        cap_reject,
      )
    }
  }
}

fn async_from_sync_continuation(
  state: State(host),
  result_ref: Ref,
  sync_iter: Ref,
  close_on_rejection: Bool,
  cap_resolve: JsValue,
  cap_reject: JsValue,
) -> Result(State(host), #(JsValue, State(host))) {
  let result_val = JsObject(result_ref)
  use #(done_val, state) <- result.try(object.get_value(
    state,
    result_ref,
    Named("done"),
    result_val,
  ))
  let done = value.is_truthy(done_val)
  use #(inner, state) <- result.try(object.get_value(
    state,
    result_ref,
    Named("value"),
    result_val,
  ))

  let #(h, on_fulfilled) =
    alloc_closure(state.heap, state.builtins, value.AsyncFromSyncUnwrap(done:))
  let #(h, on_rejected) = case done || !close_on_rejection {
    True -> #(h, JsUndefined)
    False ->
      alloc_closure(h, state.builtins, value.AsyncFromSyncClose(sync_iter:))
  }

  Ok(promise_resolve_then_with_capability(
    State(..state, heap: h),
    inner,
    on_fulfilled,
    on_rejected,
    cap_resolve,
    cap_reject,
  ))
}

/// §7.4.11 IteratorClose with a normal completion, in this module's
/// `Result(State, #(thrown, State))` shape. The close itself is
/// `iter_protocol.iterator_close_normal` — the ONE implementation of the
/// GetMethod(return) + Call + result-is-an-Object rules.
fn iterator_close_normal(
  state: State(host),
  sync_iter: Ref,
) -> Result(State(host), #(JsValue, State(host))) {
  case iter_protocol.iterator_close_normal(state, JsObject(sync_iter)) {
    #(state, Ok(Nil)) -> Ok(state)
    #(state, Error(thrown)) -> Error(#(thrown, state))
  }
}

fn alloc_closure(
  h: Heap(host),
  b: Builtins,
  native: value.CallNativeFn,
) -> #(Heap(host), JsValue) {
  // Promise reaction job functions — not constructors.
  let #(h, ref) =
    common.alloc_call_fn(
      h,
      b.function.prototype,
      native,
      "",
      1,
      constructible: False,
    )
  #(h, JsObject(ref))
}

/// AsyncFromSyncUnwrap onFulfilled — `v => ({value: v, done})`.
pub fn call_native_async_from_sync_unwrap(
  state: State(host),
  done: Bool,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  let v = list.first(args) |> result.unwrap(JsUndefined)
  let #(h, iter_result) =
    common.create_iter_result(state.heap, state.builtins, v, done)
  Ok(
    State(
      ..state,
      heap: h,
      stack: [iter_result, ..rest_stack],
      pc: state.pc + 1,
    ),
  )
}

/// AsyncFromSyncClose onRejected — `err => { syncIter.return?.(); throw err }`.
pub fn call_native_async_from_sync_close(
  state: State(host),
  sync_iter: Ref,
  args: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  let err = list.first(args) |> result.unwrap(JsUndefined)
  // §7.4.11 IteratorClose with a THROW completion: the original error wins, so
  // anything the close itself throws is observed and dropped (that policy lives
  // in `iter_protocol.close_and_throw`, not re-hand-rolled here).
  let #(err, state) =
    iter_protocol.close_and_throw(state, JsObject(sync_iter), err)
  Error(Threw(err, state))
}

/// PromiseResolve(value) then PerformPromiseThen with the supplied capability.
fn promise_resolve_then_with_capability(
  state: State(host),
  inner: JsValue,
  on_fulfilled: JsValue,
  on_rejected: JsValue,
  cap_resolve: JsValue,
  cap_reject: JsValue,
) -> State(host) {
  let h = state.heap
  case builtins_promise.is_promise(h, inner) {
    True -> {
      let assert JsObject(inner_ref) = inner
      let assert Some(inner_data_ref) =
        builtins_promise.get_data_ref(h, inner_ref)
      builtins_promise.perform_promise_then(
        state,
        inner_data_ref,
        on_fulfilled,
        on_rejected,
        cap_resolve,
        cap_reject,
      )
    }
    False -> {
      let #(h, wrap_ref, wrap_data_ref) =
        builtins_promise.create_promise(h, state.builtins.promise.prototype)
      let state =
        builtins_promise.resolve_promise(
          State(..state, heap: h),
          wrap_ref,
          wrap_data_ref,
          inner,
        )
      builtins_promise.perform_promise_then(
        state,
        wrap_data_ref,
        on_fulfilled,
        on_rejected,
        cap_resolve,
        cap_reject,
      )
    }
  }
}

// ============================================================================
// Private helpers
// ============================================================================

// ============================================================================
// Array.fromAsync — §23.1.2.1 (proposal-array-from-async, ES2026)
// ============================================================================
//
// The whole body runs as the spec's fromAsyncClosure: any synchronous abrupt
// completion rejects the returned promise instead of throwing. Await points
// are modelled as native promise-reaction closures (FromAsyncCtx carries the
// loop state), mirroring the AsyncFromSync machinery above. Continuation
// handlers run with no result capability, so they route every failure through
// the captured `reject` function explicitly.

/// Array.fromAsync(asyncItems [, mapfn [, thisArg]]) — entry point.
pub fn call_native_array_from_async(
  state: State(host),
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  // Step 2: promiseCapability = \! NewPromiseCapability(%Promise%).
  let #(h, cap) =
    builtins_promise.new_promise_capability(state.heap, state.builtins)
  let state = State(..state, heap: h)
  // Steps 3-4: run the closure; abrupt completion rejects the promise.
  let state = case
    from_async_closure(state, this, args, cap.resolve, cap.reject)
  {
    Ok(state) -> state
    Error(#(thrown, state)) ->
      builtins_promise.reject_promise(state, cap.data, thrown)
  }
  // Step 5: return promiseCapability.[[Promise]].
  return_promise(state, cap.promise, rest_stack)
}

/// Steps 3.a-3.k of the fromAsyncClosure, up to (and including) kicking off
/// the first iteration / element await.
fn from_async_closure(
  state: State(host),
  c: JsValue,
  args: List(JsValue),
  resolve: JsValue,
  reject: JsValue,
) -> Result(State(host), #(JsValue, State(host))) {
  let #(items, map_fn, this_arg) = helpers.three_args_or_undefined(args)
  // Steps 3.a-3.b: mapping check (inside the closure → rejects, not throws).
  use Nil <- result.try(case map_fn {
    JsUndefined -> Ok(Nil)
    mf ->
      case helpers.is_callable(state.heap, mf) {
        True -> Ok(Nil)
        False ->
          coerce.thrown_type_error(
            state,
            operators.typeof(state.heap, mf) <> " is not a function",
          )
      }
  })
  // Step 3.c GetMethod(asyncItems, @@asyncIterator): GetV on null/undefined
  // throws TypeError (ToObject coercion).
  use Nil <- result.try(case items {
    JsUndefined | value.JsNull ->
      coerce.thrown_type_error(
        state,
        "Cannot convert " <> operators.typeof(state.heap, items) <> " to object",
      )
    _ -> Ok(Nil)
  })
  use #(async_method, state) <- result.try(from_async_get_method(
    state,
    items,
    value.symbol_async_iterator,
  ))
  case async_method {
    JsUndefined -> {
      // Step 3.d: usingSyncIterator = ? GetMethod(asyncItems, @@iterator).
      use #(sync_method, state) <- result.try(from_async_get_method(
        state,
        items,
        value.symbol_iterator,
      ))
      case sync_method {
        // Step 3.k: not iterable at all — array-like path.
        JsUndefined ->
          from_async_array_like(
            state,
            c,
            items,
            map_fn,
            this_arg,
            resolve,
            reject,
          )
        method -> {
          // Step 3.i: GetIteratorFromMethod (sync), then wrap via
          // CreateAsyncFromSyncIterator (§27.1.6.1).
          use #(sync_iter_val, state) <- result.try(
            state.call(state, method, items, []),
          )
          use sync_iter <- result.try(case sync_iter_val {
            JsObject(r) -> Ok(r)
            _ ->
              coerce.thrown_type_error(state, "The iterator is not an object")
          })
          // §7.4.3 step 4: nextMethod = ? Get(iterator, "next") — observable
          // once here; the wrapper's .next() reuses the cached method.
          use #(sync_next, state) <- result.try(object.get_value(
            state,
            sync_iter,
            Named("next"),
            sync_iter_val,
          ))
          let #(h, wrapped) =
            common.alloc_wrapper(
              state.heap,
              value.AsyncFromSyncIteratorObject(sync_iter:, sync_next:),
              state.builtins.async_from_sync_iterator_proto,
            )
          let state = State(..state, heap: h)
          let iter_val = JsObject(wrapped)
          use #(next_method, state) <- result.try(object.get_value(
            state,
            wrapped,
            Named("next"),
            iter_val,
          ))
          from_async_iterate(
            state,
            c,
            iter_val,
            next_method,
            map_fn,
            this_arg,
            resolve,
            reject,
          )
        }
      }
    }
    method -> {
      // Step 3.h: GetIteratorFromMethod (async): iterator = ? Call(method).
      use #(iter_val, state) <- result.try(state.call(state, method, items, []))
      use iter_ref <- result.try(case iter_val {
        JsObject(r) -> Ok(r)
        _ -> coerce.thrown_type_error(state, "The iterator is not an object")
      })
      use #(next_method, state) <- result.try(object.get_value(
        state,
        iter_ref,
        Named("next"),
        iter_val,
      ))
      from_async_iterate(
        state,
        c,
        iter_val,
        next_method,
        map_fn,
        this_arg,
        resolve,
        reject,
      )
    }
  }
}

/// §7.3.10 GetMethod(V, P) for a symbol key: undefined/null → undefined,
/// non-callable → TypeError.
fn from_async_get_method(
  state: State(host),
  v: JsValue,
  sym: value.SymbolId,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  use #(method, state) <- result.try(object.get_symbol_value_of(state, v, sym))
  case method {
    JsUndefined | value.JsNull -> Ok(#(JsUndefined, state))
    _ ->
      case helpers.is_callable(state.heap, method) {
        True -> Ok(#(method, state))
        False ->
          coerce.thrown_type_error(
            state,
            operators.typeof(state.heap, method) <> " is not a function",
          )
      }
  }
}

/// Step 3.j.i: A = IsConstructor(C) ? Construct(C) : ArrayCreate(0), then
/// start the iteration loop.
fn from_async_iterate(
  state: State(host),
  c: JsValue,
  iter: JsValue,
  next_method: JsValue,
  map_fn: JsValue,
  this_arg: JsValue,
  resolve: JsValue,
  reject: JsValue,
) -> Result(State(host), #(JsValue, State(host))) {
  use #(target, state) <- result.try(case object.is_constructor(state.heap, c) {
    True -> state.construct(state, c, [])
    False -> from_async_array_create(state, 0)
  })
  from_async_request_next(
    state,
    value.FromAsyncCtx(
      iter:,
      next_method:,
      map_fn:,
      this_arg:,
      target:,
      k: 0,
      resolve:,
      reject:,
    ),
  )
}

/// Loop step 3.j.ii.2-3: nextResult = ? Call(next, iterator), then
/// Await(nextResult) with the OnNext continuation. A rejection of the await
/// rejects the outer promise directly (no AsyncIteratorClose — §23.1.2.1).
fn from_async_request_next(
  state: State(host),
  ctx: value.FromAsyncCtx,
) -> Result(State(host), #(JsValue, State(host))) {
  use #(next_result, state) <- result.try(
    state.call(state, ctx.next_method, ctx.iter, []),
  )
  Ok(from_async_await(
    state,
    next_result,
    value.ArrayFromAsyncOnNext(ctx),
    ctx.reject,
  ))
}

/// Await(v): PromiseResolve(%Promise%, v) then attach the native continuation
/// with no result capability. Continuations must route their own failures
/// through the captured reject function (handler throws are unobservable).
fn from_async_await(
  state: State(host),
  v: JsValue,
  on_fulfilled: value.CallNativeFn,
  on_rejected: JsValue,
) -> State(host) {
  let #(h, on_f) = alloc_closure(state.heap, state.builtins, on_fulfilled)
  promise_resolve_then_with_capability(
    State(..state, heap: h),
    v,
    on_f,
    on_rejected,
    JsUndefined,
    JsUndefined,
  )
}

/// onFulfilled for the awaited next() result (steps 3.j.ii.4-8).
pub fn call_native_from_async_on_next(
  state: State(host),
  ctx: value.FromAsyncCtx,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  let next_result = list.first(args) |> result.unwrap(JsUndefined)
  let state = case from_async_next_steps(state, ctx, next_result) {
    Ok(state) -> state
    // Plain rejection — these abrupt completions do NOT close the iterator.
    Error(#(thrown, state)) ->
      job_call.call_settlement_fn(state, ctx.reject, [thrown])
  }
  Ok(State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1))
}

fn from_async_next_steps(
  state: State(host),
  ctx: value.FromAsyncCtx,
  next_result: JsValue,
) -> Result(State(host), #(JsValue, State(host))) {
  // Step 3.j.ii.5: If nextResult is not an Object, throw TypeError.
  use result_ref <- result.try(case next_result {
    JsObject(r) -> Ok(r)
    _ -> coerce.thrown_type_error(state, "Iterator result is not an object")
  })
  // Step 3.j.ii.6: done = ? IteratorComplete(nextResult).
  use #(done_val, state) <- result.try(object.get_value(
    state,
    result_ref,
    Named("done"),
    next_result,
  ))
  case value.is_truthy(done_val) {
    // Step 3.j.ii.7: done — Set(A, "length", k, true), resolve with A.
    True -> {
      use state <- result.map(from_async_set_length(state, ctx.target, ctx.k))
      job_call.call_settlement_fn(state, ctx.resolve, [ctx.target])
    }
    False -> {
      // Step 3.j.ii.8: nextValue = ? IteratorValue(nextResult).
      use #(next_value, state) <- result.try(object.get_value(
        state,
        result_ref,
        Named("value"),
        next_result,
      ))
      case ctx.map_fn {
        // Step 3.j.ii.10: no mapping — mappedValue is nextValue (not awaited).
        JsUndefined -> from_async_define_and_continue(state, ctx, next_value)
        map_fn ->
          // Step 3.j.ii.9: mappedValue = Call(mapper, thisArg, «nextValue, k»),
          // IfAbruptCloseAsyncIterator, then Await with close-on-rejection.
          case
            state.call(state, map_fn, ctx.this_arg, [
              next_value,
              value.from_int(ctx.k),
            ])
          {
            Ok(#(mapped, state)) -> {
              let #(h, on_r) =
                alloc_closure(
                  state.heap,
                  state.builtins,
                  value.ArrayFromAsyncCloseReject(
                    iter: ctx.iter,
                    reject: ctx.reject,
                  ),
                )
              Ok(from_async_await(
                State(..state, heap: h),
                mapped,
                value.ArrayFromAsyncOnMapped(ctx),
                on_r,
              ))
            }
            Error(#(thrown, state)) ->
              Ok(from_async_close_then_reject(
                state,
                ctx.iter,
                thrown,
                ctx.reject,
              ))
          }
      }
    }
  }
}

/// onFulfilled for the awaited mapfn result (iterator path).
pub fn call_native_from_async_on_mapped(
  state: State(host),
  ctx: value.FromAsyncCtx,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  let mapped = list.first(args) |> result.unwrap(JsUndefined)
  let state = case from_async_define_and_continue(state, ctx, mapped) {
    Ok(state) -> state
    Error(#(thrown, state)) ->
      job_call.call_settlement_fn(state, ctx.reject, [thrown])
  }
  Ok(State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1))
}

/// Steps 3.j.ii.11-13: CreateDataPropertyOrThrow(A, k, v) (abrupt → close +
/// reject), k += 1, request the next iteration.
fn from_async_define_and_continue(
  state: State(host),
  ctx: value.FromAsyncCtx,
  v: JsValue,
) -> Result(State(host), #(JsValue, State(host))) {
  case from_async_define_own(state, ctx.target, ctx.k, v) {
    Error(#(thrown, state)) ->
      Ok(from_async_close_then_reject(state, ctx.iter, thrown, ctx.reject))
    Ok(state) ->
      from_async_request_next(state, value.FromAsyncCtx(..ctx, k: ctx.k + 1))
  }
}

/// onRejected closure that closes the async iterator, then rejects.
pub fn call_native_from_async_close_reject(
  state: State(host),
  iter: JsValue,
  reject: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  let err = list.first(args) |> result.unwrap(JsUndefined)
  let state = from_async_close_then_reject(state, iter, err, reject)
  Ok(State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1))
}

/// Rejects with the captured original error regardless of its argument —
/// runs after awaiting AsyncIteratorClose's return() result.
pub fn call_native_from_async_reject_with(
  state: State(host),
  error: JsValue,
  reject: JsValue,
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  let state = job_call.call_settlement_fn(state, reject, [error])
  Ok(State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1))
}

/// §7.4.13 AsyncIteratorClose(iteratorRecord, throwCompletion(err)) followed
/// by rejecting with err. With a throw completion the original error always
/// wins (steps 4-5), so failures from GetMethod/Call(return) are deliberately
/// dropped after being observed.
fn from_async_close_then_reject(
  state: State(host),
  iter: JsValue,
  err: JsValue,
  reject: JsValue,
) -> State(host) {
  case iter {
    JsObject(iter_ref) ->
      case object.get_value(state, iter_ref, Named("return"), iter) {
        // GetMethod threw — original error wins (§7.4.13 step 4).
        Error(#(_inner_thrown, state)) ->
          job_call.call_settlement_fn(state, reject, [err])
        Ok(#(ret_fn, state)) ->
          case helpers.is_callable(state.heap, ret_fn) {
            False -> job_call.call_settlement_fn(state, reject, [err])
            True ->
              case state.call(state, ret_fn, iter, []) {
                // return() threw — original error wins (§7.4.13 step 4).
                Error(#(_inner_thrown, state)) ->
                  job_call.call_settlement_fn(state, reject, [err])
                Ok(#(inner, state)) -> {
                  // Await(innerResult), then reject with the original error
                  // whichever way it settles.
                  let #(h, rw) =
                    alloc_closure(
                      state.heap,
                      state.builtins,
                      value.ArrayFromAsyncRejectWith(error: err, reject:),
                    )
                  promise_resolve_then_with_capability(
                    State(..state, heap: h),
                    inner,
                    rw,
                    rw,
                    JsUndefined,
                    JsUndefined,
                  )
                }
              }
          }
      }
    _ -> job_call.call_settlement_fn(state, reject, [err])
  }
}

// ----------------------------------------------------------------------------
// Array-like path (step 3.k)
// ----------------------------------------------------------------------------

fn from_async_array_like(
  state: State(host),
  c: JsValue,
  items: JsValue,
  map_fn: JsValue,
  this_arg: JsValue,
  resolve: JsValue,
  reject: JsValue,
) -> Result(State(host), #(JsValue, State(host))) {
  // Step 3.k.iii: len = ? LengthOfArrayLike(arrayLike).
  use #(len_val, state) <- result.try(object.get_value_of(
    state,
    items,
    Named("length"),
  ))
  use #(len, state) <- result.try(from_async_to_length(state, len_val))
  // Step 3.k.iv: A = IsConstructor(C) ? Construct(C, «len») : ArrayCreate(len).
  use #(target, state) <- result.try(case object.is_constructor(state.heap, c) {
    True -> state.construct(state, c, [value.from_int(len)])
    False -> from_async_array_create(state, len)
  })
  from_async_like_step(
    state,
    value.FromAsyncLikeCtx(
      items:,
      map_fn:,
      this_arg:,
      target:,
      k: 0,
      len:,
      resolve:,
      reject:,
    ),
  )
}

/// Loop step 3.k.vi: while k < len, kValue = ? Get(arrayLike, Pk) then
/// Await(kValue); at the end Set(A, "length", len, true) and resolve.
fn from_async_like_step(
  state: State(host),
  ctx: value.FromAsyncLikeCtx,
) -> Result(State(host), #(JsValue, State(host))) {
  case ctx.k < ctx.len {
    False -> {
      use state <- result.map(from_async_set_length(state, ctx.target, ctx.len))
      job_call.call_settlement_fn(state, ctx.resolve, [ctx.target])
    }
    True -> {
      use #(k_val, state) <- result.try(object.get_value_of(
        state,
        ctx.items,
        Index(ctx.k),
      ))
      Ok(from_async_await(
        state,
        k_val,
        value.ArrayFromAsyncLikeOnValue(ctx),
        ctx.reject,
      ))
    }
  }
}

/// onFulfilled for the awaited element value (array-like path).
pub fn call_native_from_async_like_on_value(
  state: State(host),
  ctx: value.FromAsyncLikeCtx,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  let v = list.first(args) |> result.unwrap(JsUndefined)
  let state = case from_async_like_value_steps(state, ctx, v) {
    Ok(state) -> state
    Error(#(thrown, state)) ->
      job_call.call_settlement_fn(state, ctx.reject, [thrown])
  }
  Ok(State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1))
}

fn from_async_like_value_steps(
  state: State(host),
  ctx: value.FromAsyncLikeCtx,
  v: JsValue,
) -> Result(State(host), #(JsValue, State(host))) {
  case ctx.map_fn {
    // Step 3.k.vi.5: no mapping — mappedValue is kValue.
    JsUndefined -> from_async_like_define_and_continue(state, ctx, v)
    map_fn -> {
      // Step 3.k.vi.4: mappedValue = ? Call(mapper, thisArg, «kValue, k»),
      // then Await — no iterator to close in this path.
      use #(mapped, state) <- result.try(
        state.call(state, map_fn, ctx.this_arg, [v, value.from_int(ctx.k)]),
      )
      Ok(from_async_await(
        state,
        mapped,
        value.ArrayFromAsyncLikeOnMapped(ctx),
        ctx.reject,
      ))
    }
  }
}

/// onFulfilled for the awaited mapfn result (array-like path).
pub fn call_native_from_async_like_on_mapped(
  state: State(host),
  ctx: value.FromAsyncLikeCtx,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State(host), StepExit(host)) {
  let mapped = list.first(args) |> result.unwrap(JsUndefined)
  let state = case from_async_like_define_and_continue(state, ctx, mapped) {
    Ok(state) -> state
    Error(#(thrown, state)) ->
      job_call.call_settlement_fn(state, ctx.reject, [thrown])
  }
  Ok(State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1))
}

fn from_async_like_define_and_continue(
  state: State(host),
  ctx: value.FromAsyncLikeCtx,
  v: JsValue,
) -> Result(State(host), #(JsValue, State(host))) {
  // Step 3.k.vi.6: ? CreateDataPropertyOrThrow(A, Pk, mappedValue).
  use state <- result.try(from_async_define_own(state, ctx.target, ctx.k, v))
  from_async_like_step(state, value.FromAsyncLikeCtx(..ctx, k: ctx.k + 1))
}

// ----------------------------------------------------------------------------
// Shared fromAsync helpers
// ----------------------------------------------------------------------------

/// §10.4.2.2 ArrayCreate(len): RangeError above 2^32-1, else a fresh array
/// with the given length and no elements.
fn from_async_array_create(
  state: State(host),
  len: Int,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  case len > key.max_array_length {
    True -> Error(state.range_error_value(state, "Invalid array length"))
    False -> {
      let #(h, ref) =
        common.alloc_array_from_elements(
          state.heap,
          elements.new(),
          len,
          state.builtins.array.prototype,
        )
      Ok(#(JsObject(ref), State(..state, heap: h)))
    }
  }
}

/// §7.1.17 ToLength via ToNumber (observable valueOf/toString coercion).
fn from_async_to_length(
  state: State(host),
  val: JsValue,
) -> Result(#(Int, State(host)), #(JsValue, State(host))) {
  use #(num, state) <- result.map(coerce.js_to_number(state, val))
  let len = case num {
    Finite(f) ->
      int.max(0, int.min(value.float_to_int(f), limits.max_safe_integer))
    value.Infinity -> limits.max_safe_integer
    _ -> 0
  }
  #(len, state)
}

/// §7.3.7 CreateDataPropertyOrThrow(A, k, v) with the descriptor
/// {value: v, writable: true, enumerable: true, configurable: true}.
/// Fast-paths the clean dense write on plain arrays (the ArrayCreate case);
/// everything else goes through the trap-aware [[DefineOwnProperty]] so
/// proxy defineProperty traps fire and non-configurable conflicts reject.
fn from_async_define_own(
  state: State(host),
  target: JsValue,
  k: Int,
  v: JsValue,
) -> Result(State(host), #(JsValue, State(host))) {
  use ref <- result.try(case target {
    JsObject(r) -> Ok(r)
    _ ->
      coerce.thrown_type_error(state, "Cannot define property on a primitive")
  })
  let fast = case heap.read(state.heap, ref) {
    Some(ObjectSlot(kind: ArrayObject(_), properties:, extensible: True, ..)) ->
      // No descriptor override at the index and no redefined "length"
      // (a non-writable length must reject growth via ArraySetLength).
      result.is_error(dict.get(properties, Index(k)))
      && result.is_error(dict.get(properties, Named("length")))
    _ -> False
  }
  case fast {
    // Dense array write — also grows the array's length.
    True -> Ok(State(..state, heap: set_array_element(state.heap, ref, k, v)))
    False -> {
      let #(h, desc_ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: OrdinaryObject,
            properties: dict.from_list([
              #(Named("value"), value.data_property(v)),
              #(Named("writable"), value.data_property(JsBool(True))),
              #(Named("enumerable"), value.data_property(JsBool(True))),
              #(Named("configurable"), value.data_property(JsBool(True))),
            ]),
            elements: elements.new(),
            prototype: Some(state.builtins.object.prototype),
            symbol_properties: [],
            extensible: True,
          ),
        )
      use #(state, ok) <- result.try(builtins_object.define_property_bool(
        State(..state, heap: h),
        ref,
        JsString(int.to_string(k)),
        desc_ref,
      ))
      case ok {
        True -> Ok(state)
        False ->
          coerce.thrown_type_error(
            state,
            "Cannot define property " <> int.to_string(k) <> " on object",
          )
      }
    }
  }
}

/// Steps 3.j.ii.7.a / 3.k.vii: Perform ? Set(A, "length", n, true).
fn from_async_set_length(
  state: State(host),
  target: JsValue,
  n: Int,
) -> Result(State(host), #(JsValue, State(host))) {
  case target {
    JsObject(ref) -> {
      use #(state, ok) <- result.try(object.set_value(
        state,
        ref,
        Named("length"),
        value.from_int(n),
        target,
      ))
      case ok {
        True -> Ok(state)
        False ->
          coerce.thrown_type_error(
            state,
            "Cannot set property length, it is read-only",
          )
      }
    }
    _ -> Ok(state)
  }
}
