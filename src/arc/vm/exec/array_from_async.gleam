import arc/vm/builtins/common
import arc/vm/builtins/helpers
import arc/vm/builtins/promise as builtins_promise
import arc/vm/exec/job_call
import arc/vm/exec/promises
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/key.{Index, Named}
import arc/vm/ops/coerce
import arc/vm/ops/mop
import arc/vm/ops/object
import arc/vm/ops/operators
import arc/vm/state.{type State, type StepExit, State}
import arc/vm/value.{
  type JsValue, ArrayObject, JsBool, JsObject, JsString, JsUndefined, ObjectSlot,
  OrdinaryObject,
}
import gleam/dict
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result

// ============================================================================
// Array.fromAsync — §23.1.2.1 (proposal-array-from-async, ES2026)
// ============================================================================
//
// The whole body runs as the spec's fromAsyncClosure: any synchronous abrupt
// completion rejects the returned promise instead of throwing. Await points
// are modelled as native promise-reaction closures (FromAsyncCtx carries the
// loop state), mirroring the AsyncFromSync machinery in promises.gleam.
// Continuation handlers run with no result capability, so they route every
// failure through the captured `reject` function explicitly.

/// Shared shell for the four onFulfilled continuation handlers: extract the
/// awaited argument, run the body, route any abrupt completion through the
/// captured reject function, then push undefined and advance.
fn from_async_handler(
  state: State(host),
  args: List(JsValue),
  rest_stack: List(JsValue),
  reject: JsValue,
  body: fn(State(host), JsValue) -> Result(State(host), #(JsValue, State(host))),
) -> Result(State(host), StepExit(host)) {
  let arg = helpers.first_arg_or_undefined(args)
  let state = case body(state, arg) {
    Ok(state) -> state
    Error(#(thrown, state)) ->
      job_call.call_settlement_fn(state, reject, [thrown])
  }
  Ok(State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1))
}

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
  promises.return_promise(state, cap.promise, rest_stack)
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
  use map_fn <- result.try(case map_fn {
    JsUndefined -> Ok(None)
    mf ->
      case helpers.is_callable(state.heap, mf) {
        True -> Ok(Some(mf))
        False ->
          state.type_error_op(
            state,
            operators.typeof(state.heap, mf) <> " is not a function",
          )
      }
  })
  // Step 3.c GetMethod(asyncItems, @@asyncIterator): GetV on null/undefined
  // throws TypeError (ToObject coercion).
  use Nil <- result.try(case items {
    JsUndefined | value.JsNull ->
      state.type_error_op(
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
            _ -> state.type_error_op(state, "The iterator is not an object")
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
        _ -> state.type_error_op(state, "The iterator is not an object")
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
          state.type_error_op(
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
  map_fn: Option(JsValue),
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
  let #(h, on_f) =
    promises.alloc_closure(state.heap, state.builtins, on_fulfilled)
  promises.promise_resolve_then_with_capability(
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
  use state, next_result <- from_async_handler(
    state,
    args,
    rest_stack,
    ctx.reject,
  )
  from_async_next_steps(state, ctx, next_result)
}

fn from_async_next_steps(
  state: State(host),
  ctx: value.FromAsyncCtx,
  next_result: JsValue,
) -> Result(State(host), #(JsValue, State(host))) {
  // Step 3.j.ii.5: If nextResult is not an Object, throw TypeError.
  use result_ref <- result.try(case next_result {
    JsObject(r) -> Ok(r)
    _ -> state.type_error_op(state, "Iterator result is not an object")
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
        None -> from_async_define_and_continue(state, ctx, next_value)
        Some(map_fn) ->
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
                promises.alloc_closure(
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
  use state, mapped <- from_async_handler(state, args, rest_stack, ctx.reject)
  from_async_define_and_continue(state, ctx, mapped)
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
  let err = helpers.first_arg_or_undefined(args)
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
                    promises.alloc_closure(
                      state.heap,
                      state.builtins,
                      value.ArrayFromAsyncRejectWith(error: err, reject:),
                    )
                  promises.promise_resolve_then_with_capability(
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
  map_fn: Option(JsValue),
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
  use #(num, state) <- result.try(coerce.js_to_number(state, len_val))
  let len = coerce.jsnum_to_length(num)
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
  use state, v <- from_async_handler(state, args, rest_stack, ctx.reject)
  from_async_like_value_steps(state, ctx, v)
}

fn from_async_like_value_steps(
  state: State(host),
  ctx: value.FromAsyncLikeCtx,
  v: JsValue,
) -> Result(State(host), #(JsValue, State(host))) {
  case ctx.map_fn {
    // Step 3.k.vi.5: no mapping — mappedValue is kValue.
    None -> from_async_like_define_and_continue(state, ctx, v)
    Some(map_fn) -> {
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
  use state, mapped <- from_async_handler(state, args, rest_stack, ctx.reject)
  from_async_like_define_and_continue(state, ctx, mapped)
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
    True -> state.range_error_op(state, "Invalid array length")
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
    _ -> state.type_error_op(state, "Cannot define property on a primitive")
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
    True ->
      Ok(
        State(..state, heap: promises.set_array_element(state.heap, ref, k, v)),
      )
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
      use #(state, ok) <- result.try(mop.define_property_bool(
        State(..state, heap: h),
        ref,
        JsString(int.to_string(k)),
        desc_ref,
      ))
      case ok {
        True -> Ok(state)
        False ->
          state.type_error_op(
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
          state.type_error_op(
            state,
            "Cannot set property length, it is read-only",
          )
      }
    }
    _ -> Ok(state)
  }
}
