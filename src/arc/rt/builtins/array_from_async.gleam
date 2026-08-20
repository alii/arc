//// Array.fromAsync — §23.1.2.1 (proposal-array-from-async, ES2026).
////
//// The whole body runs as the spec's fromAsyncClosure: any synchronous abrupt
//// completion rejects the returned promise instead of throwing. Await points
//// are native promise-reaction closures (`FromAsyncCtx` carries the loop
//// state), the same machinery as the AsyncFromSync wrappers.
//// Continuation handlers run with a throwaway result capability, so they
//// route every failure through the captured `reject` function explicitly.

import arc/rt/async as rt_async
import arc/rt/builtins/helpers
import arc/rt/builtins/iter_protocol
import arc/rt/call as rt_call
import arc/rt/elements
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type FromAsyncCtx, type FromAsyncLikeCtx, type JsVal,
  type NativeToken, ArrayFromAsyncCloseReject, ArrayFromAsyncLikeOnMapped,
  ArrayFromAsyncLikeOnValue, ArrayFromAsyncOnMapped, ArrayFromAsyncOnNext,
  ArrayFromAsyncRejectWith, ArrayN, ArrayObj, FromAsyncCtx, FromAsyncLikeCtx,
  JInt, KHandle, KNull, KUndef, Named, SObject, StringKey, SymbolKey, classify,
  index_key, max_array_length, mk_number, mk_object, mk_undefined,
  symbol_async_iterator, symbol_iterator,
} as rt_types
import arc/rt/val as rt_val
import gleam/dict
import gleam/int
import gleam/option.{type Option, None, Some}

/// Wire-compatible with `rt_call.Completion` but generic over the normal
/// value, so a threaded `Agent`-only body can pass through the FFI catch.
type ProtOut(a) {
  NormalCompletion(a)
  ThrowCompletion(JsVal)
}

@external(erlang, "arc_rt_call_ffi", "t_apply_protected")
fn protected(st: Agent, body: fn(Agent) -> #(a, Agent)) -> #(ProtOut(a), Agent)

/// Run an `Agent`-only body under the throw catch: `Ok(st)` on normal
/// completion, `Error(#(thrown, st))` on a JS throw.
fn attempt(
  st: Agent,
  body: fn(Agent) -> Agent,
) -> Result(Agent, #(JsVal, Agent)) {
  case protected(st, fn(st) { #(Nil, body(st)) }) {
    #(NormalCompletion(Nil), st) -> Ok(st)
    #(ThrowCompletion(thrown), st) -> Error(#(thrown, st))
  }
}

/// As `attempt`, for a body that also yields a value.
fn attempt_value(
  st: Agent,
  body: fn(Agent) -> #(a, Agent),
) -> Result(#(a, Agent), #(JsVal, Agent)) {
  case protected(st, body) {
    #(NormalCompletion(v), st) -> Ok(#(v, st))
    #(ThrowCompletion(thrown), st) -> Error(#(thrown, st))
  }
}

fn from_int(n: Int) -> JsVal {
  mk_number(JInt(n))
}

/// Invoke a captured capability resolve/reject function, discarding its
/// (always undefined) result.
fn settle(st: Agent, target: JsVal, arg: JsVal) -> Agent {
  let #(_, st) = rt_call.t_call_checked(st, target, mk_undefined(), [arg])
  st
}

fn alloc_closure(st: Agent, tag: NativeToken) -> #(JsVal, Agent) {
  let #(h, st) =
    rt_call.t_native_new(
      st,
      Some(st.realm.function.prototype),
      tag,
      "",
      1,
      False,
    )
  #(mk_object(h), st)
}

/// Name for error messages: typeof, except null is "null" (typeof says
/// "object").
fn type_name(st: Agent, v: JsVal) -> String {
  case classify(v) {
    KNull -> "null"
    _ -> {
      let #(ty, _) = rt_val.t_type_of(st, v)
      ty
    }
  }
}

/// Shared shell for the four onFulfilled continuation handlers: extract the
/// awaited argument, run the body, route any abrupt completion through the
/// captured reject function, then return undefined.
fn from_async_handler(
  st: Agent,
  args: List(JsVal),
  reject: JsVal,
  body: fn(Agent, JsVal) -> Agent,
) -> #(JsVal, Agent) {
  let arg = helpers.first_arg_or_undefined(args)
  let st = case attempt(st, fn(st) { body(st, arg) }) {
    Ok(st) -> st
    Error(#(thrown, st)) -> settle(st, reject, thrown)
  }
  #(mk_undefined(), st)
}

/// Array.fromAsync(asyncItems [, mapfn [, thisArg]]) — entry point.
pub fn from_async(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  // Step 2: promiseCapability = ! NewPromiseCapability(%Promise%).
  let #(#(promise_h, resolve_h, reject_h), st) =
    rt_async.t_new_promise_capability(st)
  let resolve = mk_object(resolve_h)
  let reject = mk_object(reject_h)
  // Steps 3-4: run the closure; abrupt completion rejects the promise.
  let st = case
    attempt(st, fn(st) { from_async_closure(st, this, args, resolve, reject) })
  {
    Ok(st) -> st
    Error(#(thrown, st)) -> rt_async.t_promise_reject(st, promise_h, thrown)
  }
  // Step 5: return promiseCapability.[[Promise]].
  #(mk_object(promise_h), st)
}

/// Steps 3.a-3.k of the fromAsyncClosure, up to (and including) kicking off
/// the first iteration / element await.
fn from_async_closure(
  st: Agent,
  c: JsVal,
  args: List(JsVal),
  resolve: JsVal,
  reject: JsVal,
) -> Agent {
  let #(items, map_fn, this_arg) = helpers.three_args_or_undefined(args)
  // Steps 3.a-3.b: mapping check (inside the closure → rejects, not throws).
  let map_fn = case classify(map_fn) {
    KUndef -> None
    _ ->
      case rt_call.is_callable(st, map_fn) {
        True -> Some(map_fn)
        False ->
          rt_val.t_throw_type_error(
            st,
            type_name(st, map_fn) <> " is not a function",
          )
      }
  }
  // Step 3.c GetMethod(asyncItems, @@asyncIterator): GetV on null/undefined
  // throws TypeError (ToObject coercion).
  let st = case classify(items) {
    KUndef | KNull ->
      rt_val.t_throw_type_error(
        st,
        "Cannot convert " <> type_name(st, items) <> " to object",
      )
    _ -> st
  }
  let #(async_method, st) =
    from_async_get_method(st, items, SymbolKey(symbol_async_iterator))
  case classify(async_method) {
    KUndef -> {
      // Step 3.d: usingSyncIterator = ? GetMethod(asyncItems, @@iterator).
      let #(sync_method, st) =
        from_async_get_method(st, items, SymbolKey(symbol_iterator))
      case classify(sync_method) {
        // Step 3.k: not iterable at all — array-like path.
        KUndef ->
          from_async_array_like(st, c, items, map_fn, this_arg, resolve, reject)
        _ -> {
          // Step 3.i: GetIteratorFromMethod (sync) — §7.4.3 step 4 reads
          // `next` once here — then wrap via CreateAsyncFromSyncIterator
          // (§27.1.6.1); the wrapper's .next() reuses the cached method.
          let #(sync_rec, st) =
            iter_protocol.get_iterator_from_method(st, items, sync_method)
          let #(rec, st) = iter_protocol.create_async_from_sync(st, sync_rec)
          from_async_iterate(
            st,
            c,
            rec.iterator,
            rec.next_method,
            map_fn,
            this_arg,
            resolve,
            reject,
          )
        }
      }
    }
    _ -> {
      // Step 3.h: GetIteratorFromMethod (async): iterator = ? Call(method).
      let #(iter_val, st) = rt_call.t_call_checked(st, async_method, items, [])
      let st = case classify(iter_val) {
        KHandle(_) -> st
        _ -> rt_val.t_throw_type_error(st, "The iterator is not an object")
      }
      let #(next_method, st) =
        rt_obj.t_get_prop(st, iter_val, StringKey(Named("next")))
      from_async_iterate(
        st,
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
  st: Agent,
  v: JsVal,
  key: rt_types.ObjectKey,
) -> #(JsVal, Agent) {
  let #(method, st) = rt_obj.t_get_prop(st, v, key)
  case classify(method) {
    KUndef | KNull -> #(mk_undefined(), st)
    _ ->
      case rt_call.is_callable(st, method) {
        True -> #(method, st)
        False ->
          rt_val.t_throw_type_error(
            st,
            type_name(st, method) <> " is not a function",
          )
      }
  }
}

/// Step 3.j.i: A = IsConstructor(C) ? Construct(C) : ArrayCreate(0), then
/// start the iteration loop.
fn from_async_iterate(
  st: Agent,
  c: JsVal,
  iter: JsVal,
  next_method: JsVal,
  map_fn: Option(JsVal),
  this_arg: JsVal,
  resolve: JsVal,
  reject: JsVal,
) -> Agent {
  let #(target, st) = case rt_call.is_constructor(st, c) {
    True -> {
      let #(h, st) = rt_call.t_construct(st, c, [], c)
      #(mk_object(h), st)
    }
    False -> from_async_array_create(st, 0)
  }
  from_async_request_next(
    st,
    FromAsyncCtx(
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
fn from_async_request_next(st: Agent, ctx: FromAsyncCtx) -> Agent {
  let #(next_result, st) =
    rt_call.t_call_checked(st, ctx.next_method, ctx.iter, [])
  from_async_await(
    st,
    next_result,
    ArrayN(ArrayFromAsyncOnNext(ctx)),
    ctx.reject,
  )
}

/// Await(v): PromiseResolve(%Promise%, v) then attach the native continuation
/// with a throwaway result capability. Continuations must route their own
/// failures through the captured reject function.
fn from_async_await(
  st: Agent,
  v: JsVal,
  on_fulfilled: NativeToken,
  on_rejected: JsVal,
) -> Agent {
  let #(on_f, st) = alloc_closure(st, on_fulfilled)
  let #(awaited_h, st) = rt_async.promise_resolve_static(st, v)
  let #(_child, st) = rt_async.t_promise_then(st, awaited_h, on_f, on_rejected)
  st
}

/// onFulfilled for the awaited next() result (steps 3.j.ii.4-8).
pub fn on_next(
  st: Agent,
  ctx: FromAsyncCtx,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use st, next_result <- from_async_handler(st, args, ctx.reject)
  from_async_next_steps(st, ctx, next_result)
}

fn from_async_next_steps(
  st: Agent,
  ctx: FromAsyncCtx,
  next_result: JsVal,
) -> Agent {
  // Step 3.j.ii.5: If nextResult is not an Object, throw TypeError.
  let st = case classify(next_result) {
    KHandle(_) -> st
    _ -> rt_val.t_throw_type_error(st, "Iterator result is not an object")
  }
  // Step 3.j.ii.6: done = ? IteratorComplete(nextResult).
  let #(done_val, st) =
    rt_obj.t_get_prop(st, next_result, StringKey(Named("done")))
  case rt_val.to_boolean(done_val) {
    // Step 3.j.ii.7: done — Set(A, "length", k, true), resolve with A.
    True -> {
      let st = from_async_set_length(st, ctx.target, ctx.k)
      settle(st, ctx.resolve, ctx.target)
    }
    False -> {
      // Step 3.j.ii.8: nextValue = ? IteratorValue(nextResult).
      let #(next_value, st) =
        rt_obj.t_get_prop(st, next_result, StringKey(Named("value")))
      case ctx.map_fn {
        // Step 3.j.ii.10: no mapping — mappedValue is nextValue (not awaited).
        None -> from_async_define_and_continue(st, ctx, next_value)
        Some(map_fn) ->
          // Step 3.j.ii.9: mappedValue = Call(mapper, thisArg, «nextValue, k»),
          // IfAbruptCloseAsyncIterator, then Await with close-on-rejection.
          case
            attempt_value(st, fn(st) {
              rt_call.t_call_checked(st, map_fn, ctx.this_arg, [
                next_value,
                from_int(ctx.k),
              ])
            })
          {
            Ok(#(mapped, st)) -> {
              let #(on_r, st) =
                alloc_closure(
                  st,
                  ArrayN(ArrayFromAsyncCloseReject(
                    iter: ctx.iter,
                    reject: ctx.reject,
                  )),
                )
              from_async_await(
                st,
                mapped,
                ArrayN(ArrayFromAsyncOnMapped(ctx)),
                on_r,
              )
            }
            Error(#(thrown, st)) ->
              from_async_close_then_reject(st, ctx.iter, thrown, ctx.reject)
          }
      }
    }
  }
}

/// onFulfilled for the awaited mapfn result (iterator path).
pub fn on_mapped(
  st: Agent,
  ctx: FromAsyncCtx,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use st, mapped <- from_async_handler(st, args, ctx.reject)
  from_async_define_and_continue(st, ctx, mapped)
}

/// Steps 3.j.ii.11-13: CreateDataPropertyOrThrow(A, k, v) (abrupt → close +
/// reject), k += 1, request the next iteration.
fn from_async_define_and_continue(
  st: Agent,
  ctx: FromAsyncCtx,
  v: JsVal,
) -> Agent {
  case attempt(st, fn(st) { from_async_define_own(st, ctx.target, ctx.k, v) }) {
    Error(#(thrown, st)) ->
      from_async_close_then_reject(st, ctx.iter, thrown, ctx.reject)
    Ok(st) -> from_async_request_next(st, FromAsyncCtx(..ctx, k: ctx.k + 1))
  }
}

/// onRejected closure that closes the async iterator, then rejects.
pub fn close_reject(
  st: Agent,
  iter: JsVal,
  reject: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let err = helpers.first_arg_or_undefined(args)
  #(mk_undefined(), from_async_close_then_reject(st, iter, err, reject))
}

/// Rejects with the captured original error regardless of its argument —
/// runs after awaiting AsyncIteratorClose's return() result.
pub fn reject_with(st: Agent, error: JsVal, reject: JsVal) -> #(JsVal, Agent) {
  #(mk_undefined(), settle(st, reject, error))
}

/// §7.4.13 AsyncIteratorClose(iteratorRecord, throwCompletion(err)) followed
/// by rejecting with err. With a throw completion the original error always
/// wins (steps 4-5), so failures from GetMethod/Call(return) are deliberately
/// dropped after being observed.
fn from_async_close_then_reject(
  st: Agent,
  iter: JsVal,
  err: JsVal,
  reject: JsVal,
) -> Agent {
  case call_return_method(st, iter) {
    #(None, st) -> settle(st, reject, err)
    #(Some(inner), st) -> {
      // Await(innerResult), then reject with the original error whichever
      // way it settles.
      let #(rw, st) =
        alloc_closure(st, ArrayN(ArrayFromAsyncRejectWith(error: err, reject:)))
      let #(inner_h, st) = rt_async.promise_resolve_static(st, inner)
      let #(_child, st) = rt_async.t_promise_then(st, inner_h, rw, rw)
      st
    }
  }
}

/// §7.4.13 steps 3-4 under a throw completion: GetMethod(iterator, "return")
/// and Call it. `None` when there is nothing to await (not an object, no
/// callable return, or GetMethod/return() threw, since the original error
/// wins); `Some(innerResult)` when return() completed normally.
fn call_return_method(st: Agent, iter: JsVal) -> #(Option(JsVal), Agent) {
  case classify(iter) {
    KHandle(_) -> {
      let got =
        attempt_value(st, fn(st) {
          rt_obj.t_get_prop(st, iter, StringKey(Named("return")))
        })
      case got {
        Error(#(_inner_thrown, st)) -> #(None, st)
        Ok(#(ret_fn, st)) -> call_if_callable(st, ret_fn, iter)
      }
    }
    _ -> #(None, st)
  }
}

fn call_if_callable(
  st: Agent,
  ret_fn: JsVal,
  iter: JsVal,
) -> #(Option(JsVal), Agent) {
  case rt_call.is_callable(st, ret_fn) {
    False -> #(None, st)
    True ->
      case rt_call.t_call(st, ret_fn, iter, []) {
        #(rt_call.ThrowCompletion(_inner_thrown), st) -> #(None, st)
        #(rt_call.NormalCompletion(inner), st) -> #(Some(inner), st)
      }
  }
}

// ----------------------------------------------------------------------------
// Array-like path (step 3.k)
// ----------------------------------------------------------------------------

fn from_async_array_like(
  st: Agent,
  c: JsVal,
  items: JsVal,
  map_fn: Option(JsVal),
  this_arg: JsVal,
  resolve: JsVal,
  reject: JsVal,
) -> Agent {
  // Step 3.k.iii: len = ? LengthOfArrayLike(arrayLike).
  let #(len_val, st) = rt_obj.t_get_prop(st, items, StringKey(Named("length")))
  let #(len, st) = rt_val.t_to_length(st, len_val)
  // Step 3.k.iv: A = IsConstructor(C) ? Construct(C, «len») : ArrayCreate(len).
  let #(target, st) = case rt_call.is_constructor(st, c) {
    True -> {
      let #(h, st) = rt_call.t_construct(st, c, [from_int(len)], c)
      #(mk_object(h), st)
    }
    False -> from_async_array_create(st, len)
  }
  from_async_like_step(
    st,
    FromAsyncLikeCtx(
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
fn from_async_like_step(st: Agent, ctx: FromAsyncLikeCtx) -> Agent {
  case ctx.k < ctx.len {
    False -> {
      let st = from_async_set_length(st, ctx.target, ctx.len)
      settle(st, ctx.resolve, ctx.target)
    }
    True -> {
      let #(k_val, st) =
        rt_obj.t_get_prop(st, ctx.items, StringKey(index_key(ctx.k)))
      from_async_await(
        st,
        k_val,
        ArrayN(ArrayFromAsyncLikeOnValue(ctx)),
        ctx.reject,
      )
    }
  }
}

/// onFulfilled for the awaited element value (array-like path).
pub fn like_on_value(
  st: Agent,
  ctx: FromAsyncLikeCtx,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use st, v <- from_async_handler(st, args, ctx.reject)
  from_async_like_value_steps(st, ctx, v)
}

fn from_async_like_value_steps(
  st: Agent,
  ctx: FromAsyncLikeCtx,
  v: JsVal,
) -> Agent {
  case ctx.map_fn {
    // Step 3.k.vi.5: no mapping — mappedValue is kValue.
    None -> from_async_like_define_and_continue(st, ctx, v)
    Some(map_fn) -> {
      // Step 3.k.vi.4: mappedValue = ? Call(mapper, thisArg, «kValue, k»),
      // then Await — no iterator to close in this path.
      let #(mapped, st) =
        rt_call.t_call_checked(st, map_fn, ctx.this_arg, [v, from_int(ctx.k)])
      from_async_await(
        st,
        mapped,
        ArrayN(ArrayFromAsyncLikeOnMapped(ctx)),
        ctx.reject,
      )
    }
  }
}

/// onFulfilled for the awaited mapfn result (array-like path).
pub fn like_on_mapped(
  st: Agent,
  ctx: FromAsyncLikeCtx,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use st, mapped <- from_async_handler(st, args, ctx.reject)
  from_async_like_define_and_continue(st, ctx, mapped)
}

fn from_async_like_define_and_continue(
  st: Agent,
  ctx: FromAsyncLikeCtx,
  v: JsVal,
) -> Agent {
  // Step 3.k.vi.6: ? CreateDataPropertyOrThrow(A, Pk, mappedValue).
  let st = from_async_define_own(st, ctx.target, ctx.k, v)
  from_async_like_step(st, FromAsyncLikeCtx(..ctx, k: ctx.k + 1))
}

// ----------------------------------------------------------------------------
// Shared fromAsync helpers
// ----------------------------------------------------------------------------

/// §10.4.2.2 ArrayCreate(len): RangeError above 2^32-1, else a fresh array
/// with the given length and no elements.
fn from_async_array_create(st: Agent, len: Int) -> #(JsVal, Agent) {
  case len > max_array_length {
    True -> rt_val.t_throw_range_error(st, "Invalid array length")
    False -> {
      let #(h, st) =
        rt_store.t_cell_new(
          st,
          SObject(
            kind: ArrayObj(len),
            proto: Some(st.realm.array.prototype),
            props: dict.new(),
            symbol_props: [],
            elements: elements.new(),
            extensible: True,
          ),
        )
      #(mk_object(h), st)
    }
  }
}

/// §7.3.7 CreateDataPropertyOrThrow(A, k, v) with the descriptor
/// {value: v, writable: true, enumerable: true, configurable: true} through
/// the trap-aware [[DefineOwnProperty]], so proxy defineProperty traps fire
/// and non-configurable conflicts throw.
fn from_async_define_own(st: Agent, target: JsVal, k: Int, v: JsVal) -> Agent {
  let ref = case classify(target) {
    KHandle(r) -> r
    _ -> rt_val.t_throw_type_error(st, "Cannot define property on a primitive")
  }
  let #(ok, st) =
    rt_obj.t_define_own_data(
      st,
      ref,
      StringKey(index_key(k)),
      v,
      True,
      True,
      True,
    )
  case ok {
    True -> st
    False ->
      rt_val.t_throw_type_error(
        st,
        "Cannot define property " <> int.to_string(k) <> " on object",
      )
  }
}

/// Steps 3.j.ii.7.a / 3.k.vii: Perform ? Set(A, "length", n, true).
fn from_async_set_length(st: Agent, target: JsVal, n: Int) -> Agent {
  case classify(target) {
    KHandle(_) -> {
      let #(ok, st) =
        rt_obj.t_set_prop(st, target, StringKey(Named("length")), from_int(n))
      case ok {
        True -> st
        False ->
          rt_val.t_throw_type_error(
            st,
            "Cannot set property length, it is read-only",
          )
      }
    }
    _ -> st
  }
}
