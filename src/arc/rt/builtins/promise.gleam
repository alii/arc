//// `rt_builtins/promise` — %Promise% constructor + prototype + statics
//// (SPEC §7.M6 builtin-control §27.2). Port of arc `builtins/promise.gleam`
//// (init) + arc `exec/promises.gleam` (dispatch bodies), re-expressed over
//// threaded `Agent`. Promise state-machine primitives
//// (`t_new_promise_capability` / `t_promise_then` / `t_promise_resolve` /
//// `t_promise_reject` / `t_enqueue_job` / `promise_resolve_static`) live in
//// `rt_async`; this module only builds the JS-visible objects and routes
//// dispatch through them.
////
//// **Return-tuple order is `#(V, St')` — value FIRST (R1).** Errors go through
//// `ops.new_error` + `t_throw` (D7).

import arc/internal/tree_array
import arc/rt/async as rt_async
import arc/rt/builtins/common
import arc/rt/builtins/helpers.{first_arg_or_undefined, two_args_or_undefined}
import arc/rt/builtins/iter_protocol.{
  type IteratorRecord, close_and_throw, get_iterator_sync, iterator_step_value,
}
import arc/rt/call.{
  NormalCompletion, ThrowCompletion, is_callable, is_constructor, t_call,
  t_call_checked, t_call_method, t_construct,
} as rt_call
import arc/rt/elements
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type Handle, type JsVal, type ObjectKey,
  type PromiseKeyedKind, type PromiseNative, ArrayObj, Dense, JInt, KHandle,
  KeyedFulfilled, KeyedRejected, KeyedValue, Named, NoElements, Ordinary,
  PromiseAllKeyedStatic, PromiseAllResolveElement, PromiseAllSettledElement,
  PromiseAllSettledKeyedStatic, PromiseAllSettledStatic, PromiseAllStatic,
  PromiseAnyRejectElement, PromiseAnyStatic, PromiseCapabilityExecutor,
  PromiseCatch, PromiseConstructor, PromiseFinally, PromiseFinallyFn,
  PromiseFinallyThrower, PromiseFinallyValueThunk, PromiseKeyedElement, PromiseN,
  PromiseRaceStatic, PromiseRejectStatic, PromiseResolveStatic, PromiseThen,
  ReturnThis, SBox, SObject, StringKey, SymbolKey, TypeErr, classify, mk_bool,
  mk_number, mk_object, mk_string, mk_undefined,
} as rt_types
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}

// ── init (arc builtins/promise.gleam:22-72) ─────────────────────────────────

/// §27.2.4/§27.2.5 — Promise constructor + prototype setup.
/// Instance methods: then/catch/finally. Statics: resolve/reject/all/race/
/// allSettled/any. `[@@toStringTag]` = "Promise", `[@@species]` returns `this`.
pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
) -> #(BuiltinPair, Agent) {
  let #(proto_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("then", PromiseN(PromiseThen), 2),
      #("catch", PromiseN(PromiseCatch), 1),
      #("finally", PromiseN(PromiseFinally), 1),
    ])
  let #(static_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("resolve", PromiseN(PromiseResolveStatic), 1),
      #("reject", PromiseN(PromiseRejectStatic), 1),
      #("all", PromiseN(PromiseAllStatic), 1),
      #("race", PromiseN(PromiseRaceStatic), 1),
      #("allSettled", PromiseN(PromiseAllSettledStatic), 1),
      #("any", PromiseN(PromiseAnyStatic), 1),
      // Await-dictionary proposal: keyed promise combinators.
      #("allKeyed", PromiseN(PromiseAllKeyedStatic), 1),
      #("allSettledKeyed", PromiseN(PromiseAllSettledKeyedStatic), 1),
    ])
  let #(bt, st) =
    common.init_type(
      st,
      object_proto,
      fn_proto,
      proto_methods,
      fn(_) { PromiseN(PromiseConstructor) },
      "Promise",
      1,
      static_methods,
    )
  let st = common.add_to_string_tag(st, bt.prototype, "Promise")
  let st = common.add_species_accessor(st, fn_proto, bt.constructor, ReturnThis)
  #(bt, st)
}

// ── dispatch ────────────────────────────────────────────────────────────────

/// Route a `PromiseNative` token to its body. `PromiseConstructor` handles
/// BOTH `new Promise(executor)` and (per §27.2.3.1 step 1) throws when called
/// without `new` — the split happens in `dispatch_construct` below.
pub fn dispatch(
  st: Agent,
  n: PromiseNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case n {
    PromiseConstructor ->
      // §27.2.3.1 step 1: NewTarget is undefined → TypeError. Reaching here
      // means [[Call]], not [[Construct]] (dispatch_native_construct routes
      // there separately).
      throw_type_error(st, "Promise constructor requires 'new'")
    PromiseThen -> then(st, this, args)
    PromiseCatch -> {
      // §27.2.5.1: Return ? Invoke(this, "then", « undefined, onRejected »).
      let on_rejected = first_arg_or_undefined(args)
      t_call_method(st, this, StringKey(Named("then")), [
        mk_undefined(),
        on_rejected,
      ])
    }
    PromiseFinally -> finally(st, this, args)
    PromiseResolveStatic -> resolve_static(st, this, args)
    PromiseRejectStatic -> reject_static(st, this, args)
    PromiseAllStatic -> combinator(st, this, args, CombAll)
    PromiseRaceStatic -> combinator(st, this, args, CombRace)
    PromiseAllSettledStatic -> combinator(st, this, args, CombAllSettled)
    PromiseAnyStatic -> combinator(st, this, args, CombAny)
    PromiseAllKeyedStatic -> keyed_combinator(st, this, args, settled: False)
    PromiseAllSettledKeyedStatic ->
      keyed_combinator(st, this, args, settled: True)
    // ── minted-closure natives ───────────────────────────────────────────────
    PromiseCapabilityExecutor(resolve_box:, reject_box:) ->
      capability_executor(st, resolve_box, reject_box, args)
    PromiseAllResolveElement(
      index:,
      remaining:,
      values:,
      already_called:,
      resolve:,
    ) ->
      all_element(st, args, index, remaining, values, already_called, resolve)
    PromiseAllSettledElement(
      fulfilled:,
      index:,
      remaining:,
      values:,
      already_called:,
      resolve:,
    ) ->
      all_settled_element(
        st,
        args,
        fulfilled,
        index,
        remaining,
        values,
        already_called,
        resolve,
      )
    PromiseAnyRejectElement(
      index:,
      remaining:,
      errors:,
      already_called:,
      reject:,
    ) ->
      any_reject_element(
        st,
        args,
        index,
        remaining,
        errors,
        already_called,
        reject,
      )
    PromiseKeyedElement(
      kind:,
      index:,
      remaining:,
      keys:,
      values:,
      already_called:,
      resolve:,
    ) ->
      keyed_element(
        st,
        args,
        kind,
        index,
        remaining,
        keys,
        values,
        already_called,
        resolve,
      )
    PromiseFinallyFn(rejecting:, on_finally:, constructor:) ->
      finally_wrapper(st, args, rejecting, on_finally, constructor)
    PromiseFinallyValueThunk(value:) -> #(value, st)
    PromiseFinallyThrower(reason:) -> rt_store.t_throw(st, reason)
  }
}

/// `new Promise(executor)` — §27.2.3.1. Called via `dispatch_native_construct`.
/// Returns the promise object handle, created from `new_target`'s prototype
/// so subclass instances get the subclass prototype.
pub fn dispatch_construct(
  st: Agent,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  let executor = first_arg_or_undefined(args)
  // Step 2: If IsCallable(executor) is false, throw TypeError.
  case is_callable(st, executor) {
    False -> throw_type_error(st, "Promise resolver is not a function")
    True -> {
      // Steps 3-7: OrdinaryCreateFromConstructor(NewTarget,
      // "%Promise.prototype%") + internal slots; step 8:
      // CreateResolvingFunctions.
      let #(proto, st) =
        rt_call.get_prototype_from_constructor(st, new_target, fn(r) {
          r.promise.prototype
        })
      let #(promise_h, st) = rt_async.t_new_promise_with_proto(st, Some(proto))
      let #(#(resolve_h, reject_h), st) =
        rt_async.alloc_resolving_fns(st, promise_h)
      let resolve = mk_object(resolve_h)
      let reject = mk_object(reject_h)
      // Step 9: Completion(Call(executor, undefined, « resolve, reject »)).
      let #(outcome, st) =
        t_call(st, executor, mk_undefined(), [resolve, reject])
      // Step 10: abrupt → Call(reject, undefined, « thrown ») — via the reject
      // FUNCTION so [[AlreadyResolved]] gates a resolve()-then-throw executor.
      let st = case outcome {
        NormalCompletion(_) -> st
        ThrowCompletion(e) -> {
          let #(_, st) = t_call_checked(st, reject, mk_undefined(), [e])
          st
        }
      }
      #(promise_h, st)
    }
  }
}

// ── §27.2.5.4 Promise.prototype.then ────────────────────────────────────────

fn then(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(on_fulfilled, on_rejected) = two_args_or_undefined(args)
  // Step 2: IsPromise(this).
  let promise_h = require_promise(st, this, "Promise.prototype.then")
  // Step 3: C = ? SpeciesConstructor(promise, %Promise%).
  let #(c, st) = species_constructor(st, this)
  case c == mk_object(st.realm.promise.constructor) {
    // Steps 4-5 for NewPromiseCapability(%Promise%): its resolving functions
    // never reach user code, so the child promise is the reaction's target.
    True -> {
      let #(child, st) =
        rt_async.t_promise_then(st, promise_h, on_fulfilled, on_rejected)
      #(mk_object(child), st)
    }
    False -> {
      // Step 4: resultCapability = ? NewPromiseCapability(C).
      let #(cap, st) = new_capability_from_constructor(st, c)
      // Step 5: PerformPromiseThen(promise, onFulfilled, onRejected, cap).
      let st =
        rt_async.t_perform_then(
          st,
          promise_h,
          on_fulfilled,
          on_rejected,
          cap.resolve,
          cap.reject,
        )
      #(cap.promise, st)
    }
  }
}

// ── §27.2.5.3 Promise.prototype.finally ─────────────────────────────────────

fn finally(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let on_finally = first_arg_or_undefined(args)
  // Steps 1-2: this must be an Object.
  case classify(this) {
    KHandle(_) -> Nil
    _ -> throw_type_error(st, "Promise.prototype.finally called on non-object")
  }
  // Step 3: C = ? SpeciesConstructor(promise, %Promise%).
  let #(c, st) = species_constructor(st, this)
  // Steps 5-6: wrap onFinally if callable; else pass through as-is.
  let #(then_finally, catch_finally, st) = case is_callable(st, on_finally) {
    False -> #(on_finally, on_finally, st)
    True -> {
      let #(tf, st) =
        alloc_closure(
          st,
          PromiseN(PromiseFinallyFn(
            rejecting: False,
            on_finally:,
            constructor: c,
          )),
        )
      let #(cf, st) =
        alloc_closure(
          st,
          PromiseN(PromiseFinallyFn(
            rejecting: True,
            on_finally:,
            constructor: c,
          )),
        )
      #(tf, cf, st)
    }
  }
  // Step 7: Return ? Invoke(promise, "then", « thenFinally, catchFinally »).
  t_call_method(st, this, StringKey(Named("then")), [
    then_finally,
    catch_finally,
  ])
}

/// §27.2.5.3.1/.2 Then/Catch Finally Function — `onFinally()`, then chain
/// PromiseResolve(C, result).then(thunk-or-thrower(original)).
fn finally_wrapper(
  st: Agent,
  args: List(JsVal),
  rejecting: Bool,
  on_finally: JsVal,
  constructor: JsVal,
) -> #(JsVal, Agent) {
  let original = first_arg_or_undefined(args)
  // Step 1: result = ? Call(onFinally, undefined).
  let #(result, st) = t_call_checked(st, on_finally, mk_undefined(), [])
  // Step 2: p = ? PromiseResolve(C, result).
  let #(p, st) = promise_resolve(st, constructor, result)
  // Step 4: handler = CreateBuiltinFunction(() => original | throw original,
  // 0, "", « »).
  let #(handler, st) = case rejecting {
    False ->
      alloc_closure_n(st, PromiseN(PromiseFinallyValueThunk(original)), 0)
    True -> alloc_closure_n(st, PromiseN(PromiseFinallyThrower(original)), 0)
  }
  // Step 5: Return ? Invoke(p, "then", « handler »).
  t_call_method(st, p, StringKey(Named("then")), [handler])
}

// ── §27.2.4.7 Promise.resolve / §27.2.4.6 Promise.reject ────────────────────

fn resolve_static(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let val = first_arg_or_undefined(args)
  // Step 2: If C is not an Object, throw a TypeError.
  case classify(this) {
    KHandle(_) -> promise_resolve(st, this, val)
    _ -> throw_type_error(st, "Promise.resolve called on non-object")
  }
}

/// §27.2.4.7.1 PromiseResolve(C, x): `x` itself when it is a promise whose
/// `constructor` is C, else a new C capability resolved with `x`.
fn promise_resolve(st: Agent, c: JsVal, x: JsVal) -> #(JsVal, Agent) {
  // Step 1: IsPromise(x) and SameValue(x.constructor, C) → return x.
  case rt_async.as_promise(st, x) {
    Some(_) -> {
      let #(ctor, st) =
        rt_obj.t_get_prop(st, x, StringKey(Named("constructor")))
      case ctor == c {
        True -> #(x, st)
        False -> resolve_with_constructor(st, c, x)
      }
    }
    None -> resolve_with_constructor(st, c, x)
  }
}

/// PromiseResolve steps 2-4: NewPromiseCapability(C), resolve it with `val`.
fn resolve_with_constructor(
  st: Agent,
  c: JsVal,
  val: JsVal,
) -> #(JsVal, Agent) {
  case c == mk_object(st.realm.promise.constructor) {
    // Intrinsic %Promise%: the capability's resolve function is unobservable.
    True -> {
      let #(h, st) = rt_async.t_new_promise(st)
      #(mk_object(h), rt_async.t_promise_resolve(st, h, val))
    }
    False -> {
      let #(cap, st) = new_capability_from_constructor(st, c)
      let #(_, st) = t_call_checked(st, cap.resolve, mk_undefined(), [val])
      #(cap.promise, st)
    }
  }
}

fn reject_static(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let reason = first_arg_or_undefined(args)
  case this == mk_object(st.realm.promise.constructor) {
    // Steps 2-3 for the intrinsic %Promise%: the reject function is
    // unobservable, so reject the fresh promise in place.
    True -> {
      let #(h, st) = rt_async.t_new_promise(st)
      #(mk_object(h), rt_async.t_promise_reject(st, h, reason))
    }
    False -> {
      // Step 2: capability = ? NewPromiseCapability(C).
      let #(cap, st) = new_capability_from_constructor(st, this)
      // Step 3: ? Call(cap.[[Reject]], undefined, « r »).
      let #(_, st) = t_call_checked(st, cap.reject, mk_undefined(), [reason])
      #(cap.promise, st)
    }
  }
}

// ── §27.2.4.1-.5 combinators (all/allSettled/any/race) ──────────────────────

type CombKind {
  CombAll
  CombRace
  CombAllSettled
  CombAny
}

/// Shared scaffold: NewPromiseCapability(this) — abrupt throws sync — then
/// GetPromiseResolve + GetIterator + perform loop; abrupt in the loop goes
/// through IfAbruptRejectPromise (Call(cap.reject, «err»)).
fn combinator(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  kind: CombKind,
) -> #(JsVal, Agent) {
  let #(cap, st) = new_capability_from_constructor(st, this)
  let iterable = first_arg_or_undefined(args)
  let #(outcome, st) =
    protected(st, fn(st) {
      let #(promise_resolve, st) = get_promise_resolve(st, this)
      let #(rec, st) = get_iterator_sync(st, iterable)
      // arc `IteratorOpen | IteratorDone` — carried through `t_throw` via a
      // heap box so the catch below sees the flag as of the throw point.
      let #(open_h, st) = alloc_box(st, mk_bool(True))
      let #(loop_outcome, st) =
        protected(st, fn(st) {
          perform_combinator(st, rec, this, cap, promise_resolve, kind, open_h)
        })
      case loop_outcome {
        NormalCompletion(v) -> #(v, st)
        // §27.2.4.1 step 6: IfAbruptCloseIterator only when the record is
        // still open (arc promises.gleam:326-333). §7.4.8 marks the record
        // done on abrupt-during-step, so `.return()` must NOT be called then.
        ThrowCompletion(e) ->
          case read_box(st, open_h) == mk_bool(True) {
            True -> {
              let #(e, st) = close_and_throw(st, rec.iterator, e)
              rt_store.t_throw(st, e)
            }
            False -> rt_store.t_throw(st, e)
          }
      }
    })
  let st = case outcome {
    NormalCompletion(_) -> st
    ThrowCompletion(e) -> {
      // IfAbruptRejectPromise: ? Call(cap.[[Reject]], undefined, «e»).
      let #(_, st) = t_call_checked(st, cap.reject, mk_undefined(), [e])
      st
    }
  }
  #(cap.promise, st)
}

fn perform_combinator(
  st: Agent,
  rec: IteratorRecord,
  c: JsVal,
  cap: Capability,
  promise_resolve: JsVal,
  kind: CombKind,
  open_h: Handle,
) -> #(JsVal, Agent) {
  let realm = st.realm
  case kind {
    CombRace ->
      // §27.2.4.5.1: every element uses cap.resolve/cap.reject; done → nothing.
      combinator_loop(
        st,
        rec,
        c,
        promise_resolve,
        open_h,
        0,
        fn(st, _i) { #(cap.resolve, cap.reject, st) },
        fn(st) { #(mk_undefined(), st) },
      )
    CombAll -> {
      let #(values_h, st) = alloc_empty_array(st, realm.array.prototype)
      let #(remaining_h, st) = alloc_counter(st, 1)
      combinator_loop(
        st,
        rec,
        c,
        promise_resolve,
        open_h,
        0,
        fn(st, i) {
          let st = set_array_element(st, values_h, i, mk_undefined())
          let #(already_called, st) = alloc_box(st, mk_bool(False))
          let #(resolve_fn, st) =
            alloc_closure(
              st,
              PromiseN(PromiseAllResolveElement(
                index: i,
                remaining: remaining_h,
                values: values_h,
                already_called:,
                resolve: cap.resolve,
              )),
            )
          let st = increment_counter(st, remaining_h)
          #(resolve_fn, cap.reject, st)
        },
        fn(st) { final_resolve_values(st, remaining_h, values_h, cap.resolve) },
      )
    }
    CombAllSettled -> {
      let #(values_h, st) = alloc_empty_array(st, realm.array.prototype)
      let #(remaining_h, st) = alloc_counter(st, 1)
      combinator_loop(
        st,
        rec,
        c,
        promise_resolve,
        open_h,
        0,
        fn(st, i) {
          let st = set_array_element(st, values_h, i, mk_undefined())
          let #(already_called, st) = alloc_box(st, mk_bool(False))
          let #(resolve_fn, st) =
            alloc_closure(
              st,
              PromiseN(PromiseAllSettledElement(
                fulfilled: True,
                index: i,
                remaining: remaining_h,
                values: values_h,
                already_called:,
                resolve: cap.resolve,
              )),
            )
          let #(reject_fn, st) =
            alloc_closure(
              st,
              PromiseN(PromiseAllSettledElement(
                fulfilled: False,
                index: i,
                remaining: remaining_h,
                values: values_h,
                already_called:,
                resolve: cap.resolve,
              )),
            )
          let st = increment_counter(st, remaining_h)
          #(resolve_fn, reject_fn, st)
        },
        fn(st) { final_resolve_values(st, remaining_h, values_h, cap.resolve) },
      )
    }
    CombAny -> {
      let #(errors_h, st) = alloc_empty_array(st, realm.array.prototype)
      let #(remaining_h, st) = alloc_counter(st, 1)
      combinator_loop(
        st,
        rec,
        c,
        promise_resolve,
        open_h,
        0,
        fn(st, i) {
          let st = set_array_element(st, errors_h, i, mk_undefined())
          let #(already_called, st) = alloc_box(st, mk_bool(False))
          let #(reject_fn, st) =
            alloc_closure(
              st,
              PromiseN(PromiseAnyRejectElement(
                index: i,
                remaining: remaining_h,
                errors: errors_h,
                already_called:,
                reject: cap.reject,
              )),
            )
          let st = increment_counter(st, remaining_h)
          #(cap.resolve, reject_fn, st)
        },
        fn(st) { final_reject_aggregate(st, remaining_h, errors_h, cap.reject) },
      )
    }
  }
}

/// §27.2.4.1.1 step 4: iterate; per value nextPromise = Call(promiseResolve,
/// C, «v»), then Invoke(nextPromise, "then", «onFulfilled, onRejected»).
/// `open_h` mirrors arc `IteratorOpen | IteratorDone` (promises.gleam:220-253):
/// abrupt during IteratorStepValue or after done → no close; abrupt during
/// resolve/then with the iterator still open → close.
fn combinator_loop(
  st: Agent,
  rec: IteratorRecord,
  c: JsVal,
  promise_resolve: JsVal,
  open_h: Handle,
  index: Int,
  make_handlers: fn(Agent, Int) -> #(JsVal, JsVal, Agent),
  on_done: fn(Agent) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  // §7.4.8: abrupt during step marks [[Done]]=true — flag done BEFORE stepping.
  let st = rt_store.t_cell_set(st, open_h, SBox(mk_bool(False)))
  let #(step, st) = iterator_step_value(st, rec)
  case step {
    None -> on_done(st)
    Some(v) -> {
      // Step succeeded → iterator open again for the resolve/then phase.
      let st = rt_store.t_cell_set(st, open_h, SBox(mk_bool(True)))
      // Step 4.h: nextPromise = ? Call(promiseResolve, C, «v»).
      let #(next_promise, st) = t_call_checked(st, promise_resolve, c, [v])
      let #(on_fulfilled, on_rejected, st) = make_handlers(st, index)
      // Step 4.s: ? Invoke(nextPromise, "then", «onFulfilled, onRejected»).
      let #(_, st) =
        t_call_method(st, next_promise, StringKey(Named("then")), [
          on_fulfilled,
          on_rejected,
        ])
      combinator_loop(
        st,
        rec,
        c,
        promise_resolve,
        open_h,
        index + 1,
        make_handlers,
        on_done,
      )
    }
  }
}

fn final_resolve_values(
  st: Agent,
  remaining_h: Handle,
  values_h: Handle,
  resolve: JsVal,
) -> #(JsVal, Agent) {
  let #(is_zero, st) = decrement_counter(st, remaining_h)
  case is_zero {
    False -> #(mk_undefined(), st)
    True -> t_call_checked(st, resolve, mk_undefined(), [mk_object(values_h)])
  }
}

fn final_reject_aggregate(
  st: Agent,
  remaining_h: Handle,
  errors_h: Handle,
  reject: JsVal,
) -> #(JsVal, Agent) {
  let #(is_zero, st) = decrement_counter(st, remaining_h)
  case is_zero {
    False -> #(mk_undefined(), st)
    True -> {
      let #(err, st) = make_aggregate_error(st, errors_h)
      t_call_checked(st, reject, mk_undefined(), [err])
    }
  }
}

// ── Promise.allKeyed / Promise.allSettledKeyed (await-dictionary proposal) ──

/// Shared per-call context of the PerformPromiseAllKeyed key loop.
type KeyedLoop {
  KeyedLoop(
    c: JsVal,
    promises: JsVal,
    promises_h: Handle,
    cap: Capability,
    promise_resolve: JsVal,
    settled: Bool,
    keys_h: Handle,
    values_h: Handle,
    remaining_h: Handle,
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
fn keyed_combinator(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  settled settled: Bool,
) -> #(JsVal, Agent) {
  // Step 2: an abrupt NewPromiseCapability throws synchronously.
  let #(cap, st) = new_capability_from_constructor(st, this)
  let promises = first_arg_or_undefined(args)
  // Steps 3-7: any later abrupt completion rejects the capability.
  let #(outcome, st) =
    protected(st, fn(st) { perform_all_keyed(st, this, promises, cap, settled) })
  let st = case outcome {
    NormalCompletion(_) -> st
    ThrowCompletion(e) -> {
      // IfAbruptRejectPromise: ? Call(cap.[[Reject]], undefined, «e»).
      let #(_, st) = t_call_checked(st, cap.reject, mk_undefined(), [e])
      st
    }
  }
  #(cap.promise, st)
}

/// PerformPromiseAllKeyed steps 1-5 plus the method's steps 3 and 5
/// (GetPromiseResolve and the is-Object check): collect [[OwnPropertyKeys]],
/// allocate the shared keys/values stores and remaining counter, then run
/// the per-key loop.
fn perform_all_keyed(
  st: Agent,
  c: JsVal,
  promises: JsVal,
  cap: Capability,
  settled: Bool,
) -> #(JsVal, Agent) {
  // Method step 3: GetPromiseResolve(ctor).
  let #(promise_resolve, st) = get_promise_resolve(st, c)
  case classify(promises) {
    KHandle(promises_h) -> {
      // Step 1: Let allKeys be ? promises.[[OwnPropertyKeys]]() — trap-aware.
      let #(all_keys, st) = rt_obj.t_own_keys(st, promises_h)
      // Steps 2-4: keys/values lists (shared, mutable from element fns) and
      // the Record { [[Value]]: 1 } remaining-elements counter.
      let realm = st.realm
      let #(keys_h, st) = alloc_empty_array(st, realm.array.prototype)
      let #(values_h, st) = alloc_empty_array(st, realm.array.prototype)
      let #(remaining_h, st) = alloc_counter(st, 1)
      let loop =
        KeyedLoop(
          c:,
          promises:,
          promises_h:,
          cap:,
          promise_resolve:,
          settled:,
          keys_h:,
          values_h:,
          remaining_h:,
        )
      keyed_loop(st, loop, all_keys, 0)
    }
    // Method step 5: promises is not an Object — reject with TypeError.
    _ ->
      throw_type_error(
        st,
        "Promise keyed combinator argument must be an object",
      )
  }
}

/// PerformPromiseAllKeyed steps 6-8: for each own key, check the descriptor
/// is present and enumerable, Get the value, wrap via promiseResolve, attach
/// the keyed element handlers, then on loop exit decrement the counter and
/// resolve with the keyed result object if it hit zero.
fn keyed_loop(
  st: Agent,
  loop: KeyedLoop,
  all_keys: List(ObjectKey),
  index: Int,
) -> #(JsVal, Agent) {
  case all_keys {
    // Steps 7-8: remainingElementsCount -= 1; at zero resolve with the
    // CreateKeyedPromiseCombinatorResultObject(keys, values).
    [] ->
      keyed_final_resolve(
        st,
        loop.remaining_h,
        loop.keys_h,
        loop.values_h,
        loop.cap.resolve,
      )
    [key, ..rest] -> {
      // Step 6.a: Let propertyDesc be ? promises.[[GetOwnProperty]](key).
      let #(desc, st) = rt_obj.t_get_own_property(st, loop.promises_h, key)
      // Step 6.b: skip absent / non-enumerable properties.
      let enumerable =
        option.map(desc, rt_types.prop_enumerable) |> option.unwrap(False)
      case enumerable {
        False -> keyed_loop(st, loop, rest, index)
        True -> {
          // Step 6.b.i: Let propertyValue be ? Get(promises, key).
          let #(prop_value, st) = rt_obj.t_get_prop(st, loop.promises, key)
          // Steps 6.b.ii-iii: append key to keys, undefined to values.
          let st =
            set_array_element(
              st,
              loop.keys_h,
              index,
              rt_obj.object_key_value(key),
            )
          let st = set_array_element(st, loop.values_h, index, mk_undefined())
          // Step 6.b.iv: nextPromise = ? Call(promiseResolve, ctor,
          // «propertyValue»).
          let #(next_promise, st) =
            t_call_checked(st, loop.promise_resolve, loop.c, [prop_value])
          // Steps 6.b.v-ix: alreadyCalled record + onFulfilled closure.
          let #(already_called, st) = alloc_box(st, mk_bool(False))
          let element = fn(st, kind) {
            alloc_closure(
              st,
              PromiseN(PromiseKeyedElement(
                kind:,
                index:,
                remaining: loop.remaining_h,
                keys: loop.keys_h,
                values: loop.values_h,
                already_called:,
                resolve: loop.cap.resolve,
              )),
            )
          }
          let fulfilled_kind = case loop.settled {
            True -> KeyedFulfilled
            False -> KeyedValue
          }
          let #(on_fulfilled, st) = element(st, fulfilled_kind)
          // Steps 6.b.x-xi: onRejected is cap.[[Reject]] for ~all~, a keyed
          // rejected-element closure (same alreadyCalled record) for
          // ~all-settled~.
          let #(on_rejected, st) = case loop.settled {
            False -> #(loop.cap.reject, st)
            True -> element(st, KeyedRejected)
          }
          // Step 6.b.xii: remainingElementsCount += 1.
          let st = increment_counter(st, loop.remaining_h)
          // Step 6.b.xiii: ? Invoke(nextPromise, "then",
          // «onFulfilled, onRejected»).
          let #(_, st) =
            t_call_method(st, next_promise, StringKey(Named("then")), [
              on_fulfilled,
              on_rejected,
            ])
          // Step 6.b.xiv: index += 1.
          keyed_loop(st, loop, rest, index + 1)
        }
      }
    }
  }
}

/// remainingElementsCount -= 1; at zero,
/// ? Call(cap.[[Resolve]], undefined, «keyed result object»).
fn keyed_final_resolve(
  st: Agent,
  remaining_h: Handle,
  keys_h: Handle,
  values_h: Handle,
  resolve: JsVal,
) -> #(JsVal, Agent) {
  let #(is_zero, st) = decrement_counter(st, remaining_h)
  case is_zero {
    False -> #(mk_undefined(), st)
    True -> {
      let #(result_h, st) = create_keyed_result(st, keys_h, values_h)
      t_call_checked(st, resolve, mk_undefined(), [mk_object(result_h)])
    }
  }
}

/// CreateKeyedPromiseCombinatorResultObject(keys, values): a null-prototype
/// ordinary object with one enumerable data property per collected key,
/// defined in keys-list order so [[OwnPropertyKeys]] round-trips it.
fn create_keyed_result(
  st: Agent,
  keys_h: Handle,
  values_h: Handle,
) -> #(Handle, Agent) {
  let keys = read_array_values(st, keys_h)
  let values = read_array_values(st, values_h)
  // Step 2: Let obj be OrdinaryObjectCreate(null).
  let #(h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: Ordinary,
        proto: None,
        props: dict.new(),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  // Step 3: ! CreateDataPropertyOrThrow(obj, keys[i], values[i]) for each i.
  let st =
    list.zip(keys, values)
    |> list.fold(st, fn(st, kv) {
      let #(k, v) = kv
      case key_of_value(k) {
        Some(key) -> {
          let #(_created, st) =
            rt_obj.t_define_own_data(st, h, key, v, True, True, True)
          st
        }
        // Keys come from [[OwnPropertyKeys]] — String/Symbol only.
        None -> st
      }
    })
  #(h, st)
}

/// Inverse of `rt_obj.object_key_value` for the String/Symbol keys the
/// keys list holds.
fn key_of_value(v: JsVal) -> option.Option(ObjectKey) {
  case classify(v) {
    rt_types.KStr(s) -> Some(StringKey(rt_types.canonical_key(s)))
    rt_types.KSym(sym) -> Some(SymbolKey(sym))
    _ -> None
  }
}

// ── element functions (per-index closures the combinators mint) ─────────────

fn all_element(
  st: Agent,
  args: List(JsVal),
  index: Int,
  remaining: Handle,
  values: Handle,
  already_called: Handle,
  resolve: JsVal,
) -> #(JsVal, Agent) {
  use val, st <- with_element_once(st, args, already_called)
  let st = set_array_element(st, values, index, val)
  final_resolve_values(st, remaining, values, resolve)
}

fn all_settled_element(
  st: Agent,
  args: List(JsVal),
  fulfilled: Bool,
  index: Int,
  remaining: Handle,
  values: Handle,
  already_called: Handle,
  resolve: JsVal,
) -> #(JsVal, Agent) {
  use val, st <- with_element_once(st, args, already_called)
  let #(record, st) = settled_record(st, fulfilled, val)
  let st = set_array_element(st, values, index, record)
  final_resolve_values(st, remaining, values, resolve)
}

/// `{status: "fulfilled", value}` / `{status: "rejected", reason}` with
/// %Object.prototype% (§27.2.4.2.2 steps 9-11 / §27.2.4.2.3 steps 9-11).
fn settled_record(st: Agent, fulfilled: Bool, val: JsVal) -> #(JsVal, Agent) {
  let #(status, field) = case fulfilled {
    True -> #("fulfilled", "value")
    False -> #("rejected", "reason")
  }
  let #(obj_h, st) =
    common.alloc_pojo(st, st.realm.object.prototype, [
      #("status", mk_string(status)),
      #(field, val),
    ])
  #(mk_object(obj_h), st)
}

/// Keyed combinator element handler (the fulfilledSteps / rejectedSteps
/// closures of PerformPromiseAllKeyed): once-only, store the (possibly
/// status-wrapped) value at the captured index, decrement the counter, and
/// at zero resolve the capability with the keyed result object.
fn keyed_element(
  st: Agent,
  args: List(JsVal),
  kind: PromiseKeyedKind,
  index: Int,
  remaining: Handle,
  keys: Handle,
  values: Handle,
  already_called: Handle,
  resolve: JsVal,
) -> #(JsVal, Agent) {
  use val, st <- with_element_once(st, args, already_called)
  // ~all~ stores the raw value; ~all-settled~ wraps it in
  // {status, value/reason} with %Object.prototype%.
  let #(stored, st) = case kind {
    KeyedValue -> #(val, st)
    KeyedFulfilled -> settled_record(st, True, val)
    KeyedRejected -> settled_record(st, False, val)
  }
  let st = set_array_element(st, values, index, stored)
  keyed_final_resolve(st, remaining, keys, values, resolve)
}

fn any_reject_element(
  st: Agent,
  args: List(JsVal),
  index: Int,
  remaining: Handle,
  errors: Handle,
  already_called: Handle,
  reject: JsVal,
) -> #(JsVal, Agent) {
  use reason, st <- with_element_once(st, args, already_called)
  let st = set_array_element(st, errors, index, reason)
  final_reject_aggregate(st, remaining, errors, reject)
}

/// Once-only guard: if already_called is set → undefined; else set it, run body.
fn with_element_once(
  st: Agent,
  args: List(JsVal),
  already_called: Handle,
  body: fn(JsVal, Agent) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  let js_true = mk_bool(True)
  case rt_store.t_cell_get(st, already_called) {
    SBox(v) if v == js_true -> #(mk_undefined(), st)
    _ -> {
      let st = rt_store.t_cell_set(st, already_called, SBox(js_true))
      body(first_arg_or_undefined(args), st)
    }
  }
}

// ── §27.2.1.5 NewPromiseCapability + GetCapabilitiesExecutor ────────────────

/// PromiseCapability Record (§27.2.1.1) — `promise` is a `JsVal` (may be
/// any object a user constructor returned).
type Capability {
  Capability(promise: JsVal, resolve: JsVal, reject: JsVal)
}

/// §27.2.1.5 NewPromiseCapability(C). Intrinsic %Promise% takes the fast path
/// (rt_async.t_new_promise_capability); any other value must be a
/// constructor and is invoked as `new C(executor)` with a
/// GetCapabilitiesExecutor that captures resolve/reject into two SBox cells.
fn new_capability_from_constructor(
  st: Agent,
  c: JsVal,
) -> #(Capability, Agent) {
  let realm = st.realm
  case c == mk_object(realm.promise.constructor) {
    True -> {
      let #(#(p, r, j), st) = rt_async.t_new_promise_capability(st)
      #(
        Capability(
          promise: mk_object(p),
          resolve: mk_object(r),
          reject: mk_object(j),
        ),
        st,
      )
    }
    False -> {
      case is_constructor(st, c) {
        False ->
          throw_type_error(st, "Promise capability requires a constructor")
        True -> {
          let #(resolve_box, st) = alloc_box(st, mk_undefined())
          let #(reject_box, st) = alloc_box(st, mk_undefined())
          let #(executor, st) =
            alloc_closure2(
              st,
              PromiseN(PromiseCapabilityExecutor(resolve_box:, reject_box:)),
            )
          let #(promise_h, st) = t_construct(st, c, [executor], c)
          let resolve = read_box(st, resolve_box)
          let reject = read_box(st, reject_box)
          case is_callable(st, resolve) && is_callable(st, reject) {
            True -> #(
              Capability(promise: mk_object(promise_h), resolve:, reject:),
              st,
            )
            False ->
              throw_type_error(
                st,
                "Promise resolve or reject function is not callable",
              )
          }
        }
      }
    }
  }
}

/// §27.2.1.5.1 GetCapabilitiesExecutor — write args into the two SBox cells;
/// throw if either is already set.
fn capability_executor(
  st: Agent,
  resolve_box: Handle,
  reject_box: Handle,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let already_set =
    read_box(st, resolve_box) != mk_undefined()
    || read_box(st, reject_box) != mk_undefined()
  case already_set {
    True ->
      throw_type_error(
        st,
        "Promise executor has already been invoked with non-undefined arguments",
      )
    False -> {
      let #(resolve, reject) = two_args_or_undefined(args)
      let st = rt_store.t_cell_set(st, resolve_box, SBox(resolve))
      let st = rt_store.t_cell_set(st, reject_box, SBox(reject))
      #(mk_undefined(), st)
    }
  }
}

/// §27.2.4.1.2 GetPromiseResolve(C): Get(C, "resolve"), require callable.
fn get_promise_resolve(st: Agent, c: JsVal) -> #(JsVal, Agent) {
  let #(resolve_fn, st) = rt_obj.t_get_prop(st, c, StringKey(Named("resolve")))
  case is_callable(st, resolve_fn) {
    True -> #(resolve_fn, st)
    False -> throw_type_error(st, "Promise resolve is not a function")
  }
}

/// §7.3.22 SpeciesConstructor(O, %Promise%). Reads `O.constructor[@@species]`;
/// falls back to %Promise% on undefined/null at any step.
fn species_constructor(st: Agent, o: JsVal) -> #(JsVal, Agent) {
  let default = mk_object(st.realm.promise.constructor)
  case intrinsic_species(st, o) {
    True -> #(default, st)
    False -> species_constructor_generic(st, o, default)
  }
}

/// The SpeciesConstructor(O, %Promise%) protocol answers %Promise% without
/// running anything when every step is a plain data read of the intrinsics
/// as `init` left them: `O` a plain %Promise% instance with no own
/// `constructor`, `%Promise.prototype%.constructor` a data property holding
/// %Promise%, and `%Promise%[@@species]` a `ReturnThis` getter (which would
/// return its receiver, %Promise%). Anything else takes the generic protocol.
fn intrinsic_species(st: Agent, o: JsVal) -> Bool {
  is_plain_promise(st, o) && common.species_intact(st, st.realm.promise)
}

/// `o` is a %Promise% instance whose `constructor` lookup reaches
/// %Promise.prototype%: that prototype, and no own `constructor`.
fn is_plain_promise(st: Agent, o: JsVal) -> Bool {
  case classify(o) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: rt_types.PromiseObj(..), proto: Some(p), props:, ..) ->
          p == st.realm.promise.prototype
          && !dict.has_key(props, Named("constructor"))
        _ -> False
      }
    _ -> False
  }
}

fn species_constructor_generic(
  st: Agent,
  o: JsVal,
  default: JsVal,
) -> #(JsVal, Agent) {
  let #(c, st) = rt_obj.t_get_prop(st, o, StringKey(Named("constructor")))
  case classify(c) {
    rt_types.KUndef -> #(default, st)
    KHandle(_) -> {
      let #(s, st) =
        rt_obj.t_get_prop(st, c, rt_types.SymbolKey(rt_types.symbol_species))
      case classify(s) {
        rt_types.KUndef | rt_types.KNull -> #(default, st)
        _ ->
          case is_constructor(st, s) {
            True -> #(s, st)
            False ->
              throw_type_error(
                st,
                "Promise[Symbol.species] is not a constructor",
              )
          }
      }
    }
    _ -> throw_type_error(st, ".constructor is not an object")
  }
}

// ── local helpers ───────────────────────────────────────────────────────────

@external(erlang, "arc_rt_call_ffi", "t_apply_protected")
fn protected(
  st: Agent,
  body: fn(Agent) -> #(JsVal, Agent),
) -> #(rt_call.Completion, Agent)

fn alloc_closure(st: Agent, tag: rt_types.NativeToken) -> #(JsVal, Agent) {
  alloc_closure_n(st, tag, 1)
}

fn alloc_closure2(st: Agent, tag: rt_types.NativeToken) -> #(JsVal, Agent) {
  alloc_closure_n(st, tag, 2)
}

/// Anonymous non-constructible builtin closure with `length` = `len`.
fn alloc_closure_n(
  st: Agent,
  tag: rt_types.NativeToken,
  len: Int,
) -> #(JsVal, Agent) {
  let #(h, st) =
    rt_call.t_native_new(
      st,
      Some(st.realm.function.prototype),
      tag,
      "",
      len,
      False,
    )
  #(mk_object(h), st)
}

fn alloc_box(st: Agent, v: JsVal) -> #(Handle, Agent) {
  rt_store.t_cell_new(st, SBox(v))
}

fn read_box(st: Agent, h: Handle) -> JsVal {
  case rt_store.t_cell_get(st, h) {
    SBox(v) -> v
    _ -> mk_undefined()
  }
}

/// remainingElementsCount `SBox(JInt(n))` — arc used a dedicated CounterSlot;
/// 2core has no such slot, so an SBox holding a number stands in.
fn alloc_counter(st: Agent, n: Int) -> #(Handle, Agent) {
  rt_store.t_cell_new(st, SBox(mk_number(JInt(n))))
}

fn adjust_counter(st: Agent, h: Handle, delta: Int) -> #(Int, Agent) {
  case rt_store.t_cell_get(st, h) {
    SBox(v) ->
      case classify(v) {
        rt_types.KNum(JInt(n)) -> {
          let n2 = n + delta
          #(n2, rt_store.t_cell_set(st, h, SBox(mk_number(JInt(n2)))))
        }
        _ -> panic as "promise combinator counter not an int"
      }
    _ -> panic as "promise combinator counter not an SBox"
  }
}

fn increment_counter(st: Agent, h: Handle) -> Agent {
  let #(_, st) = adjust_counter(st, h, 1)
  st
}

fn decrement_counter(st: Agent, h: Handle) -> #(Bool, Agent) {
  let #(n, st) = adjust_counter(st, h, -1)
  #(n <= 0, st)
}

fn alloc_empty_array(st: Agent, array_proto: Handle) -> #(Handle, Agent) {
  common.alloc_array(st, [], array_proto)
}

/// Set element at `index` in a heap-allocated `ArrayObj`, growing `length`.
fn set_array_element(
  st: Agent,
  arr_h: Handle,
  index: Int,
  val: JsVal,
) -> Agent {
  rt_store.t_cell_update(st, arr_h, fn(slot) {
    case slot {
      SObject(kind: ArrayObj(length:), elements:, ..) -> {
        let ta = case elements {
          Dense(t) -> t
          _ -> tree_array.new(rt_types.mk_hole())
        }
        SObject(
          ..slot,
          kind: ArrayObj(int.max(length, index + 1)),
          elements: Dense(tree_array.set(index, val, ta)),
        )
      }
      other -> other
    }
  })
}

/// Elements `0..length-1` of a heap `ArrayObj`, holes read as undefined.
fn read_array_values(st: Agent, arr_h: Handle) -> List(JsVal) {
  case rt_store.t_cell_get(st, arr_h) {
    SObject(kind: ArrayObj(length:), elements: els, ..) ->
      collect_elements(els, length - 1, [])
    _ -> []
  }
}

fn collect_elements(
  els: rt_types.JsElements,
  i: Int,
  acc: List(JsVal),
) -> List(JsVal) {
  case i < 0 {
    True -> acc
    False -> collect_elements(els, i - 1, [elements.get(els, i), ..acc])
  }
}

fn make_aggregate_error(st: Agent, errors_h: Handle) -> #(JsVal, Agent) {
  let realm = st.realm
  let #(msg_p, st) =
    common.builtin_property(st, mk_string("All promises were rejected"))
  let #(errs_p, st) = common.builtin_property(st, mk_object(errors_h))
  let #(h, st) =
    common.alloc_error_slot(st, realm.aggregate_error.prototype, [
      #("message", msg_p),
      #("errors", errs_p),
    ])
  #(mk_object(h), st)
}

fn require_promise(st: Agent, this: JsVal, name: String) -> Handle {
  case rt_async.as_promise(st, this) {
    Some(h) -> h
    None -> throw_type_error(st, name <> " called on non-promise")
  }
}

fn throw_type_error(st: Agent, msg: String) -> a {
  let js = st.store
  let #(e, st) = js.ops.new_error(st, TypeErr, msg)
  rt_store.t_throw(st, e)
}
