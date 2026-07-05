import arc/vm/builtins/common.{type BuiltinType, type Builtins}
import arc/vm/builtins/helpers
import arc/vm/heap
import arc/vm/internal/job_queue
import arc/vm/key.{Named}
import arc/vm/ops/object
import arc/vm/state.{type Heap}
import arc/vm/value.{
  type Job, type JsValue, type Ref, BoxSlot, Call, JsBool,
  JsObject, PromiseCatch, PromiseConstructor, PromiseFinally, PromiseObject,
  PromiseReaction, PromiseRejectFunction, PromiseRejectStatic,
  PromiseResolveFunction, PromiseResolveStatic, PromiseSlot, PromiseThen,
}
import gleam/list
import gleam/option.{type Option, None, Some}

/// ES2024 §27.2.4 Properties of the Promise Constructor &
/// ES2024 §27.2.5 Properties of the Promise Prototype Object
///
/// Sets up Promise.prototype with instance methods (then, catch, finally)
/// and the Promise constructor with static methods (resolve, reject).
pub fn init(
  h: Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(host), BuiltinType) {
  // §27.2.5.4 Promise.prototype.then(onFulfilled, onRejected)
  // §27.2.5.1 Promise.prototype.catch(onRejected)
  // §27.2.5.3 Promise.prototype.finally(onFinally)
  let #(h, proto_methods) =
    common.alloc_call_methods(h, function_proto, [
      #("then", PromiseThen, 2),
      #("catch", PromiseCatch, 1),
      #("finally", PromiseFinally, 1),
    ])
  // §27.2.4.5 Promise.resolve(x)
  // §27.2.4.4 Promise.reject(r)
  // §27.2.4.1 Promise.all(iterable)
  // §27.2.4.2 Promise.allSettled(iterable)
  // §27.2.4.3 Promise.any(iterable)
  // §27.2.4.5 Promise.race(iterable)
  let #(h, static_methods) =
    common.alloc_call_methods(h, function_proto, [
      #("resolve", PromiseResolveStatic, 1),
      #("reject", PromiseRejectStatic, 1),
      #("all", value.PromiseAllStatic, 1),
      #("race", value.PromiseRaceStatic, 1),
      #("allSettled", value.PromiseAllSettledStatic, 1),
      #("any", value.PromiseAnyStatic, 1),
      // Await-dictionary proposal: keyed promise combinators.
      #("allKeyed", value.PromiseAllKeyedStatic, 1),
      #("allSettledKeyed", value.PromiseAllSettledKeyedStatic, 1),
    ])
  let #(h, bt) =
    common.init_type(
      h,
      object_proto,
      function_proto,
      proto_methods,
      fn(_) { Call(PromiseConstructor) },
      "Promise",
      1,
      static_methods,
    )
  // §27.2.5.6 Promise.prototype [ @@toStringTag ] = "Promise"
  let h = common.add_to_string_tag(h, bt.prototype, "Promise")
  // §27.2.4.9 get Promise [ @@species ] — an accessor returning `this`, so
  // SpeciesConstructor(promise, %Promise%) resolves to the SUBCLASS through
  // inheritance (`class P extends Promise {}` → Get(P, @@species) is P).
  // Reuses the VM's generic return-`this` native (IteratorSymbolIterator).
  let #(h, species_getter) =
    common.alloc_native_fn(
      h,
      function_proto,
      value.VmNative(value.IteratorSymbolIterator),
      "get [Symbol.species]",
      0,
    )
  let h =
    common.add_symbol_property(
      h,
      bt.constructor,
      value.symbol_species,
      value.accessor(
        get: Some(JsObject(species_getter)),
        set: None,
        enumerable: False,
        configurable: True,
      ),
    )
  #(h, bt)
}

/// Partial ES2024 §27.2.3.1 Promise(executor) — the object allocation part.
///
/// Corresponds to steps 3-7 of the Promise constructor:
///   3. Let promise be ? OrdinaryCreateFromConstructor(NewTarget, "%Promise.prototype%",
///      << [[PromiseState]], [[PromiseResult]], [[PromiseFulfillReactions]],
///         [[PromiseRejectReactions]], [[PromiseIsHandled]] >>).
///   4. Set promise.[[PromiseState]] to pending.
///   5. Set promise.[[PromiseFulfillReactions]] to a new empty List.
///   6. Set promise.[[PromiseRejectReactions]] to a new empty List.
///   7. Set promise.[[PromiseIsHandled]] to false.
///
/// The actual executor call (steps 8-11) happens in the VM's
/// PromiseConstructor handler, not here. This function only creates
/// the promise object. We split the internal slots into a separate PromiseSlot
/// heap entry (data_ref) pointed to by the ObjectSlot's PromiseObject kind.
///
/// Returns (heap, object_ref, data_ref).
pub fn create_promise(
  h: Heap(host),
  promise_proto: Ref,
) -> #(Heap(host), Ref, Ref) {
  // Steps 4-7: Initialize internal slots
  let #(h, data_ref) =
    heap.alloc(
      h,
      PromiseSlot(
        state: value.PromisePending,
        // Step 4: [[PromiseState]] = pending
        fulfill_reactions: [],
        // Step 5: [[PromiseFulfillReactions]] = empty
        reject_reactions: [],
        // Step 6: [[PromiseRejectReactions]] = empty
        is_handled: False,
        // Step 7: [[PromiseIsHandled]] = false
      ),
    )
  // Step 3: OrdinaryCreateFromConstructor — allocate the visible object
  let #(h, obj_ref) =
    common.alloc_wrapper(
      h,
      PromiseObject(promise_data: data_ref),
      promise_proto,
    )
  #(h, obj_ref, data_ref)
}

/// ES2024 §27.2.1.3 CreateResolvingFunctions(promise)
///
/// Spec steps:
///   1. Let alreadyResolved be the Record { [[Value]]: false }.
///   2. Let stepsResolve be the algorithm steps defined in Promise Resolve
///      Functions (§27.2.1.3.2).
///   3. Let lengthResolve be the number of non-optional parameters of stepsResolve.
///   4. Let resolve be CreateBuiltinFunction(stepsResolve, lengthResolve, "",
///      << [[Promise]], [[AlreadyResolved]] >>).
///   5. Set resolve.[[Promise]] to promise.
///   6. Set resolve.[[AlreadyResolved]] to alreadyResolved.
///   7. Let stepsReject be the algorithm steps defined in Promise Reject
///      Functions (§27.2.1.3.1).
///   8-12. (Same pattern for reject function.)
///   13. Return the Record { [[Resolve]]: resolve, [[Reject]]: reject }.
///
/// The [[AlreadyResolved]] record is implemented as a BoxSlot on the heap
/// (mutable shared reference). The [[Promise]] slot is captured as both
/// promise_ref (object) and data_ref (internal PromiseSlot) to avoid
/// re-traversing the heap.
///
/// Returns (heap, resolve_value, reject_value).
pub fn create_resolving_functions(
  h: Heap(host),
  function_proto: Ref,
  promise_ref: Ref,
  data_ref: Ref,
) -> #(Heap(host), JsValue, JsValue) {
  // Step 1: Let alreadyResolved be the Record { [[Value]]: false }.
  let #(h, already_resolved_ref) = heap.alloc(h, BoxSlot(value: JsBool(False)))

  // Steps 2-6: Create the resolve function with [[Promise]] and [[AlreadyResolved]]
  let #(h, resolve_fn_ref) =
    common.alloc_call_fn(
      h,
      function_proto,
      PromiseResolveFunction(promise_ref:, data_ref:, already_resolved_ref:),
      "",
      1,
      constructible: False,
    )

  // Steps 7-12: Create the reject function (same [[AlreadyResolved]] record)
  let #(h, reject_fn_ref) =
    common.alloc_call_fn(
      h,
      function_proto,
      PromiseRejectFunction(promise_ref:, data_ref:, already_resolved_ref:),
      "",
      1,
      constructible: False,
    )

  // Step 13: Return the Record { [[Resolve]]: resolve, [[Reject]]: reject }.
  #(h, JsObject(resolve_fn_ref), JsObject(reject_fn_ref))
}

/// ES2024 §27.2.1.5 PromiseCapability Record.
///
/// - `promise`: the visible promise object ([[Promise]])
/// - `data`: the internal PromiseSlot backing that object
/// - `resolve` / `reject`: the resolving functions ([[Resolve]] / [[Reject]])
///
/// Labelled fields so a promise/data or resolve/reject swap cannot
/// type-check silently the way it could with a bare tuple.
pub type PromiseCapability {
  PromiseCapability(promise: Ref, data: Ref, resolve: JsValue, reject: JsValue)
}

/// ES spec NewPromiseCapability — create a promise and its resolve/reject
/// functions in one step (create_promise + create_resolving_functions).
pub fn new_promise_capability(
  h: Heap(host),
  b: Builtins,
) -> #(Heap(host), PromiseCapability) {
  let #(h, promise_ref, data_ref) = create_promise(h, b.promise.prototype)
  let #(h, resolve, reject) =
    create_resolving_functions(h, b.function.prototype, promise_ref, data_ref)
  #(
    h,
    PromiseCapability(promise: promise_ref, data: data_ref, resolve:, reject:),
  )
}

/// ES2024 §27.2.1.4 FulfillPromise(promise, value)
///
/// Spec steps:
///   1. Assert: The value of promise.[[PromiseState]] is pending.
///   2. Let reactions be promise.[[PromiseFulfillReactions]].
///   3. Set promise.[[PromiseResult]] to value.
///   4. Set promise.[[PromiseFulfillReactions]] to undefined.
///   5. Set promise.[[PromiseRejectReactions]] to undefined.
///   6. Set promise.[[PromiseState]] to fulfilled.
///   7. Perform TriggerPromiseReactions(reactions, value).
///   8. Return unused.
///
/// Step 1 assertion is a soft check — if not pending, this is a no-op.
/// Step 7 jobs are appended to state.job_queue (mirrors reject_promise, so
/// no caller can settle the promise and forget to enqueue its reactions).
pub fn fulfill_promise(
  state: state.State(host),
  data_ref: Ref,
  result_value: JsValue,
) -> state.State(host) {
  let #(state, _did_settle) =
    fulfill_promise_tracked(state, data_ref, result_value)
  state
}

/// FulfillPromise, additionally reporting how the settle went. See
/// `SettleOutcome` and `settle_outcome`.
fn fulfill_promise_tracked(
  state: state.State(host),
  data_ref: Ref,
  result_value: JsValue,
) -> #(state.State(host), SettleOutcome) {
  let #(h, jobs, outcome) =
    settle_promise(
      state.heap,
      data_ref,
      result_value,
      value.PromiseFulfilled(result_value),
    )
  case outcome {
    // Not a promise slot: nothing to settle, report it back to the caller.
    NotAPromiseSlot -> #(state, outcome)
    // Legitimate soft no-op (the spec's pending Assert), or the transition.
    AlreadySettled | Transitioned(_) -> #(
      state.State(
        ..state,
        heap: h,
        job_queue: job_queue.append(state.job_queue, jobs),
      ),
      outcome,
    )
  }
}

/// Settle a promise from a host outcome: FulfillPromise on `Ok`,
/// RejectPromise on `Error`. Reports HOW the settle went, so a caller can
/// tell the three cases apart: `arc/host.resume` decrements its `outstanding`
/// counter only on `Transitioned`, treats `AlreadySettled` (a double resume)
/// as a silent no-op, and surfaces `NotAPromiseSlot` (a stale ticket) as the
/// embedder bug it is. `fulfill_promise`/`reject_promise` are thin wrappers
/// that discard the outcome for internal callers.
pub fn settle_outcome(
  state: state.State(host),
  data_ref: Ref,
  outcome: Result(JsValue, JsValue),
) -> #(state.State(host), SettleOutcome) {
  case outcome {
    Ok(v) -> fulfill_promise_tracked(state, data_ref, v)
    Error(reason) -> reject_promise_tracked(state, data_ref, reason)
  }
}

/// The three ways a settle attempt can end (see `settle_promise`).
pub type SettleOutcome {
  /// The promise was pending and this call moved it to fulfilled/rejected.
  /// `is_handled` is the promise's [[PromiseIsHandled]] at that moment.
  Transitioned(is_handled: Bool)
  /// The promise was already settled — a legitimate no-op, which the spec
  /// models as an Assert (e.g. `resolve(1); reject(2)`).
  AlreadySettled
  /// The ref does not point at a PromiseSlot at all. No JS program can cause
  /// this, but an embedder can: a `Ticket` outlives the promise slot it names
  /// once `shrink_for_handoff` has dropped that slot, so resuming a stale
  /// ticket lands here. Nothing was settled; the caller decides what to do
  /// (`arc/host.resume` reports it — it is an embedder bug, not a no-op).
  NotAPromiseSlot
}

/// Shared settle core of FulfillPromise/RejectPromise (steps 2-6 plus
/// TriggerPromiseReactions): if the promise is pending, pick the reaction
/// list that matches `settled_state`, build reaction jobs from it, and write
/// the settled PromiseSlot with cleared reaction lists.
///
/// The `SettleOutcome` distinguishes the transition, the legitimate
/// already-settled no-op, and a ref that is not a promise at all.
fn settle_promise(
  h: Heap(host),
  data_ref: Ref,
  result_value: JsValue,
  settled_state: value.PromiseState,
) -> #(Heap(host), List(Job), SettleOutcome) {
  case heap.read(h, data_ref) {
    Some(PromiseSlot(
      state: value.PromisePending,
      fulfill_reactions:,
      reject_reactions:,
      is_handled:,
    )) -> {
      // TriggerPromiseReactions(reactions, value) — build job list.
      // Reactions are stored newest-first (see perform_promise_then), so
      // reverse once here to enqueue jobs in attachment order per spec.
      // Which list fires is fully determined by the settled state, so derive
      // it here rather than trusting the caller to pass a matching selector.
      let reactions = case settled_state {
        value.PromiseFulfilled(_) -> fulfill_reactions
        value.PromiseRejected(_) -> reject_reactions
        // Unreachable: both callers pass a settled variant.
        value.PromisePending -> []
      }
      let jobs =
        list.map(list.reverse(reactions), fn(r) {
          value.PromiseReactionJob(
            handler: r.handler,
            arg: result_value,
            resolve: r.child_resolve,
            reject: r.child_reject,
          )
        })
      // Transition state to settled, clear both reaction lists
      let h =
        heap.write(
          h,
          data_ref,
          PromiseSlot(
            state: settled_state,
            fulfill_reactions: [],
            reject_reactions: [],
            is_handled:,
          ),
        )
      #(h, jobs, Transitioned(is_handled:))
    }
    // Soft assertion: not pending -> no-op (spec says Assert)
    Some(PromiseSlot(..)) -> #(h, [], AlreadySettled)
    // Not a promise's internal slot at all (see `NotAPromiseSlot`).
    Some(_) | None -> #(h, [], NotAPromiseSlot)
  }
}

/// ES2024 §27.2.1.7 RejectPromise(promise, reason)
///
/// Spec steps:
///   1. Assert: The value of promise.[[PromiseState]] is pending.
///   2. Let reactions be promise.[[PromiseRejectReactions]].
///   3. Set promise.[[PromiseResult]] to reason.
///   4. Set promise.[[PromiseFulfillReactions]] to undefined.
///   5. Set promise.[[PromiseRejectReactions]] to undefined.
///   6. Set promise.[[PromiseState]] to rejected.
///   7. If promise.[[PromiseIsHandled]] is false, perform
///      HostPromiseRejectionTracker(promise, "reject").
///   8. Perform TriggerPromiseReactions(reactions, reason).
///   9. Return unused.
///
/// Step 1 assertion is a soft check (no-op if not pending).
/// Step 7: HostPromiseRejectionTracker — tracks unhandled rejections on State.
/// Step 8 jobs are appended to state.job_queue.
pub fn reject_promise(
  state: state.State(host),
  data_ref: Ref,
  reason: JsValue,
) -> state.State(host) {
  let #(state, _did_settle) = reject_promise_tracked(state, data_ref, reason)
  state
}

/// RejectPromise, additionally reporting how the settle went. See
/// `SettleOutcome` and `settle_outcome`.
fn reject_promise_tracked(
  state: state.State(host),
  data_ref: Ref,
  reason: JsValue,
) -> #(state.State(host), SettleOutcome) {
  let #(h, jobs, outcome) =
    settle_promise(
      state.heap,
      data_ref,
      reason,
      value.PromiseRejected(reason),
    )
  case outcome {
    Transitioned(is_handled:) -> {
      // Step 7: HostPromiseRejectionTracker(promise, "reject")
      let unhandled_rejections = case is_handled {
        False -> [data_ref, ..state.unhandled_rejections]
        True -> state.unhandled_rejections
      }
      #(
        state.State(
          ..state,
          heap: h,
          job_queue: job_queue.append(state.job_queue, jobs),
          unhandled_rejections:,
        ),
        outcome,
      )
    }
    // Soft assertion: not pending -> no-op (spec says Assert)
    AlreadySettled -> #(state, outcome)
    // Not a promise slot: nothing to settle, report it back to the caller.
    NotAPromiseSlot -> #(state, outcome)
  }
}

/// ES2024 §27.2.5.4.1 PerformPromiseThen(promise, onFulfilled, onRejected,
///                                        resultCapability)
///
/// Spec steps:
///   1. Assert: IsPromise(promise) is true.
///   2. If resultCapability is not present, set resultCapability to undefined.
///   3. If IsCallable(onFulfilled) is false, let onFulfilledJobCallback be empty.
///   4. Else, let onFulfilledJobCallback be HostMakeJobCallback(onFulfilled).
///   5. If IsCallable(onRejected) is false, let onRejectedJobCallback be empty.
///   6. Else, let onRejectedJobCallback be HostMakeJobCallback(onRejected).
///   7. Let fulfillReaction be the PromiseReaction Record { [[Capability]]:
///      resultCapability, [[Type]]: fulfill, [[Handler]]: onFulfilledJobCallback }.
///   8. Let rejectReaction be the PromiseReaction Record { [[Capability]]:
///      resultCapability, [[Type]]: reject, [[Handler]]: onRejectedJobCallback }.
///   9. If promise.[[PromiseState]] is pending, then
///      a. Append fulfillReaction to promise.[[PromiseFulfillReactions]].
///      b. Append rejectReaction to promise.[[PromiseRejectReactions]].
///   10. Else if promise.[[PromiseState]] is fulfilled, then
///      a. Let value be promise.[[PromiseResult]].
///      b. Let fulfillJob be NewPromiseReactionJob(fulfillReaction, value).
///      c. Perform HostEnqueuePromiseJob(fulfillJob.[[Job]], fulfillJob.[[Realm]]).
///   11. Else (rejected),
///      a. Assert: The value of promise.[[PromiseState]] is rejected.
///      b. Let reason be promise.[[PromiseResult]].
///      c. If promise.[[PromiseIsHandled]] is false, perform
///         HostPromiseRejectionTracker(promise, "handle").
///      d. Let rejectJob be NewPromiseReactionJob(rejectReaction, reason).
///      e. Perform HostEnqueuePromiseJob(rejectJob.[[Job]], rejectJob.[[Realm]]).
///   12. Set promise.[[PromiseIsHandled]] to true.
///   13. If resultCapability is undefined, return undefined.
///   14. Return resultCapability.[[Promise]].
///
/// Non-callable handlers become the explicit `IdentityPassThrough` /
/// `ThrowerPassThrough` reaction handlers (the spec's "empty" concept).
/// Jobs are appended to state.job_queue.
/// Step 11c: HostPromiseRejectionTracker — untracks previously-unhandled
/// rejections on State when a handler is attached.
pub fn perform_promise_then(
  state: state.State(host),
  data_ref: Ref,
  on_fulfilled: JsValue,
  on_rejected: JsValue,
  child_resolve: JsValue,
  child_reject: JsValue,
) -> state.State(host) {
  let h = state.heap
  // §27.2.5.4 steps 3-6: a non-callable onFulfilled/onRejected is the
  // spec's "empty" handler — an explicit pass-through, never a JsValue.
  let fulfill_handler = case helpers.is_callable(h, on_fulfilled) {
    True -> value.Handler(on_fulfilled)
    False -> value.IdentityPassThrough
  }
  let reject_handler = case helpers.is_callable(h, on_rejected) {
    True -> value.Handler(on_rejected)
    False -> value.ThrowerPassThrough
  }
  case heap.read(h, data_ref) {
    // Step 6: If promise.[[PromiseState]] is pending
    Some(PromiseSlot(
      state: value.PromisePending,
      fulfill_reactions:,
      reject_reactions:,
      is_handled: _,
    )) -> {
      // Steps 4-5: Create PromiseReaction records
      let fulfill_reaction =
        PromiseReaction(child_resolve:, child_reject:, handler: fulfill_handler)
      let reject_reaction =
        PromiseReaction(child_resolve:, child_reject:, handler: reject_handler)
      // Step 6a-b: Append reactions to lists; Step 9: set [[PromiseIsHandled]]
      // Perf: reactions are stored newest-first (O(1) prepend) and reversed
      // once at settle time in fulfill_promise/reject_promise to preserve
      // spec ordering — avoids O(n²) for n .then() on a pending promise.
      let h =
        heap.write(
          h,
          data_ref,
          PromiseSlot(
            state: value.PromisePending,
            fulfill_reactions: [fulfill_reaction, ..fulfill_reactions],
            reject_reactions: [reject_reaction, ..reject_reactions],
            is_handled: True,
          ),
        )
      state.State(..state, heap: h)
    }
    // Step 7: Else if promise.[[PromiseState]] is fulfilled
    Some(PromiseSlot(state: value.PromiseFulfilled(val), ..)) -> {
      // Step 9: Set [[PromiseIsHandled]] to true
      let h = mark_handled(h, data_ref)
      // Step 7b-c: NewPromiseReactionJob + HostEnqueuePromiseJob
      state.State(
        ..state,
        heap: h,
        job_queue: job_queue.push(
          state.job_queue,
          value.PromiseReactionJob(
            handler: fulfill_handler,
            arg: val,
            resolve: child_resolve,
            reject: child_reject,
          ),
        ),
      )
    }
    // Step 8: Else (rejected)
    Some(PromiseSlot(state: value.PromiseRejected(reason), is_handled:, ..)) -> {
      // Step 9: Set [[PromiseIsHandled]] to true
      let h = mark_handled(h, data_ref)
      // Step 8c: HostPromiseRejectionTracker(promise, "handle")
      let unhandled_rejections = case is_handled {
        False ->
          list.filter(state.unhandled_rejections, fn(r) { r != data_ref })
        True -> state.unhandled_rejections
      }
      // Step 8d-e: NewPromiseReactionJob + HostEnqueuePromiseJob
      state.State(
        ..state,
        heap: h,
        job_queue: job_queue.push(
          state.job_queue,
          value.PromiseReactionJob(
            handler: reject_handler,
            arg: reason,
            resolve: child_resolve,
            reject: child_reject,
          ),
        ),
        unhandled_rejections:,
      )
    }
    _ -> state
  }
}

/// ES2024 §27.2.1.6 IsPromise(x)
///
/// Spec steps:
///   1. If x is not an Object, return false.
///   2. If x does not have a [[PromiseState]] internal slot, return false.
///   3. Return true.
///
/// We check for PromiseObject kind on the ObjectSlot rather than a
/// [[PromiseState]] slot directly, since our representation stores promise
/// state in a separate PromiseSlot referenced by the PromiseObject kind tag.
pub fn is_promise(h: Heap(host), val: JsValue) -> Bool {
  case val {
    // Step 1: If x is not an Object, return false.
    // Step 2-3: Check for [[PromiseState]] via PromiseObject kind tag.
    JsObject(ref) -> heap.read_promise_data_ref(h, ref) |> option.is_some
    _ -> False
  }
}

/// Non-spec utility: extract the internal PromiseSlot data ref from a promise
/// object ref. Used to access [[PromiseState]]/[[PromiseResult]] etc.
/// Returns None if the ref is not a promise object.
pub fn get_data_ref(h: Heap(host), promise_ref: Ref) -> Option(Ref) {
  heap.read_promise_data_ref(h, promise_ref)
}

/// Partial ES2024 §27.2.1.3.2 Promise Resolve Functions, steps 8-13
/// (the "thenable resolution" part).
///
/// Relevant spec steps from Promise Resolve Functions:
///   8. If resolution is not an Object, then
///      a. Perform FulfillPromise(promise, resolution).
///      b. Return undefined.
///   9. Let then be Completion(Get(resolution, "then")).
///   10. If then is an abrupt completion, then
///       a. Perform RejectPromise(promise, then.[[Value]]).
///       b. Return undefined.
///   11. Let thenAction be then.[[Value]].
///   12. If IsCallable(thenAction) is false, then
///       a. Perform FulfillPromise(promise, resolution).
///       b. Return undefined.
///   13. Let thenJobCallback be HostMakeJobCallback(thenAction).
///       ... (enqueue PromiseResolveThenableJob)
///
/// This function implements steps 8-12's checks and returns the then function
/// if found. The caller handles steps 8a/10a/12a (fulfill or reject) and
/// step 13 (enqueue thenable job). Most callers should just use
/// `resolve_promise`, which performs those steps.
pub fn get_thenable_then(
  state: state.State(host),
  val: JsValue,
) -> #(ThenLookup, state.State(host)) {
  case val {
    // Step 8: If resolution is not an Object -> NotThenable (caller fulfills)
    JsObject(ref) ->
      // Step 9: Let then be Completion(Get(resolution, "then"))
      case object.get_value(state, ref, Named("then"), val) {
        Ok(#(then_val, state)) ->
          // Step 12: If IsCallable(thenAction) is false -> NotThenable
          case helpers.is_callable(state.heap, then_val) {
            True -> #(Thenable(then_val), state)
            False -> #(NotThenable, state)
          }
        // Step 10: If then is an abrupt completion -> ThenAccessThrew
        Error(#(thrown, state)) -> #(ThenAccessThrew(thrown), state)
      }
    _ -> #(NotThenable, state)
  }
}

/// Outcome of looking up `then` on a resolution value (Promise Resolve
/// Functions §27.2.1.3.2 steps 8-12). The three cases lead to OPPOSITE
/// promise outcomes, so they are distinct variants — a caller must decide
/// each one explicitly.
pub type ThenLookup {
  /// Step 13 path: the value is an object with a callable `then`.
  /// The caller must enqueue a PromiseResolveThenableJob.
  Thenable(then_fn: JsValue)
  /// Step 8 / step 12 path: not an object, or `then` is not callable.
  /// The caller must FULFILL the promise with the value itself.
  NotThenable
  /// Step 10 path: Get(resolution, "then") threw.
  /// The caller must REJECT the promise with the thrown value.
  ThenAccessThrew(thrown: JsValue)
}

/// ES2024 §27.2.1.3.2 Promise Resolve Functions, steps 8-13 — resolve
/// `promise_ref`/`data_ref` with `resolution` (which the caller has already
/// checked is not the promise itself, step 7):
///
///   - thenable            → enqueue PromiseResolveThenableJob (step 13-14)
///   - `then` getter threw → RejectPromise with the thrown value (step 10)
///   - anything else       → FulfillPromise with the resolution (steps 8, 12)
pub fn resolve_promise(
  state: state.State(host),
  promise_ref: Ref,
  data_ref: Ref,
  resolution: JsValue,
) -> state.State(host) {
  let #(lookup, state) = get_thenable_then(state, resolution)
  case lookup {
    Thenable(then_fn:) -> {
      let #(h, resolve, reject) =
        create_resolving_functions(
          state.heap,
          state.builtins.function.prototype,
          promise_ref,
          data_ref,
        )
      let job =
        value.PromiseResolveThenableJob(
          thenable: resolution,
          then_fn:,
          resolve:,
          reject:,
        )
      state.State(
        ..state,
        heap: h,
        job_queue: job_queue.push(state.job_queue, job),
      )
    }
    ThenAccessThrew(thrown:) -> reject_promise(state, data_ref, thrown)
    NotThenable -> fulfill_promise(state, data_ref, resolution)
  }
}

/// Non-spec utility: set [[PromiseIsHandled]] to true on a PromiseSlot.
/// Corresponds to §27.2.5.4.1 step 12 and §27.2.1.7 step 7 context.
/// Used for unhandled rejection tracking, and by any driver that inspects a
/// capability's settled state itself instead of attaching a JS handler (module
/// top-level await) — such a promise IS handled, so its rejection must not also
/// be reported as unhandled.
pub fn mark_handled(h: Heap(host), data_ref: Ref) -> Heap(host) {
  use slot <- heap.update(h, data_ref)
  case slot {
    PromiseSlot(..) -> PromiseSlot(..slot, is_handled: True)
    _ -> slot
  }
}
