import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers
import arc/vm/frame
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/object
import arc/vm/value.{
  type Job, type JsValue, type Ref, BoxSlot, CallNative, JsBool, JsObject,
  NativeFunction, ObjectSlot, PromiseCatch, PromiseConstructor, PromiseFinally,
  PromiseObject, PromiseReaction, PromiseRejectFunction, PromiseRejectStatic,
  PromiseResolveFunction, PromiseResolveStatic, PromiseSlot, PromiseThen,
}
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}

/// ES2024 §27.2.4 Properties of the Promise Constructor &
/// ES2024 §27.2.5 Properties of the Promise Prototype Object
///
/// Sets up Promise.prototype with instance methods (then, catch, finally)
/// and the Promise constructor with static methods (resolve, reject).
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, BuiltinType) {
  // §27.2.5.4 Promise.prototype.then(onFulfilled, onRejected)
  // §27.2.5.1 Promise.prototype.catch(onRejected)
  // §27.2.5.3 Promise.prototype.finally(onFinally)
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("then", CallNative(PromiseThen), 2),
      #("catch", CallNative(PromiseCatch), 1),
      #("finally", CallNative(PromiseFinally), 1),
    ])
  // §27.2.4.5 Promise.resolve(x)
  // §27.2.4.4 Promise.reject(r)
  let #(h, static_methods) =
    common.alloc_methods(h, function_proto, [
      #("resolve", CallNative(PromiseResolveStatic), 1),
      #("reject", CallNative(PromiseRejectStatic), 1),
    ])
  common.init_type(
    h,
    object_proto,
    function_proto,
    proto_methods,
    fn(_) { CallNative(PromiseConstructor) },
    "Promise",
    1,
    static_methods,
  )
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
pub fn create_promise(h: Heap, promise_proto: Ref) -> #(Heap, Ref, Ref) {
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
    heap.alloc(
      h,
      ObjectSlot(
        kind: PromiseObject(promise_data: data_ref),
        properties: dict.new(),
        elements: js_elements.new(),
        prototype: Some(promise_proto),
        symbol_properties: dict.new(),
        extensible: True,
      ),
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
  h: Heap,
  function_proto: Ref,
  promise_ref: Ref,
  data_ref: Ref,
) -> #(Heap, JsValue, JsValue) {
  // Step 1: Let alreadyResolved be the Record { [[Value]]: false }.
  let #(h, already_resolved_ref) = heap.alloc(h, BoxSlot(value: JsBool(False)))

  // Steps 2-6: Create the resolve function with [[Promise]] and [[AlreadyResolved]]
  let #(h, resolve_fn_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(
          CallNative(PromiseResolveFunction(
            promise_ref:,
            data_ref:,
            already_resolved_ref:,
          )),
        ),
        properties: dict.from_list([
          #("name", common.fn_name_property("")),
          #("length", common.fn_length_property(1)),
        ]),
        elements: js_elements.new(),
        prototype: Some(function_proto),
        symbol_properties: dict.new(),
        extensible: True,
      ),
    )

  // Steps 7-12: Create the reject function (same [[AlreadyResolved]] record)
  let #(h, reject_fn_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(
          CallNative(PromiseRejectFunction(
            promise_ref:,
            data_ref:,
            already_resolved_ref:,
          )),
        ),
        properties: dict.from_list([
          #("name", common.fn_name_property("")),
          #("length", common.fn_length_property(1)),
        ]),
        elements: js_elements.new(),
        prototype: Some(function_proto),
        symbol_properties: dict.new(),
        extensible: True,
      ),
    )

  // Step 13: Return the Record { [[Resolve]]: resolve, [[Reject]]: reject }.
  #(h, JsObject(resolve_fn_ref), JsObject(reject_fn_ref))
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
/// Step 1 assertion is a soft check — if not pending, we return an empty
/// job list instead of asserting. Step 7 is deferred: instead of calling
/// TriggerPromiseReactions directly, we return the jobs as a list for the
/// VM's job queue to drain.
pub fn fulfill_promise(
  h: Heap,
  data_ref: Ref,
  result_value: JsValue,
) -> #(Heap, List(Job)) {
  case heap.read(h, data_ref) {
    Some(PromiseSlot(
      state: value.PromisePending,
      fulfill_reactions: reactions,
      is_handled:,
      ..,
    )) -> {
      // Step 7: TriggerPromiseReactions(reactions, value) — build job list
      let jobs =
        list.map(reactions, fn(r) {
          value.PromiseReactionJob(
            handler: r.handler,
            arg: result_value,
            resolve: r.child_resolve,
            reject: r.child_reject,
          )
        })
      // Steps 3-6: Transition state to fulfilled, clear reaction lists
      let h =
        heap.write(
          h,
          data_ref,
          PromiseSlot(
            state: value.PromiseFulfilled(result_value),
            // Steps 3, 6
            fulfill_reactions: [],
            // Step 4
            reject_reactions: [],
            // Step 5
            is_handled:,
          ),
        )
      #(h, jobs)
    }
    // Soft assertion: not pending -> no-op (spec says Assert)
    _ -> #(h, [])
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
/// TODO(Deviation): Step 7 (HostPromiseRejectionTracker) is not yet implemented.
/// Step 8 is deferred — jobs returned for the VM queue.
pub fn reject_promise(
  h: Heap,
  data_ref: Ref,
  reason: JsValue,
) -> #(Heap, List(Job)) {
  case heap.read(h, data_ref) {
    Some(PromiseSlot(
      state: value.PromisePending,
      reject_reactions: reactions,
      is_handled:,
      ..,
    )) -> {
      // Step 8: TriggerPromiseReactions(reactions, reason) — build job list
      let jobs =
        list.map(reactions, fn(r) {
          value.PromiseReactionJob(
            handler: r.handler,
            arg: reason,
            resolve: r.child_resolve,
            reject: r.child_reject,
          )
        })
      // Steps 3-6: Transition state to rejected, clear reaction lists
      // Step 7: TODO — HostPromiseRejectionTracker not implemented
      let h =
        heap.write(
          h,
          data_ref,
          PromiseSlot(
            state: value.PromiseRejected(reason),
            // Steps 3, 6
            fulfill_reactions: [],
            // Step 4
            reject_reactions: [],
            // Step 5
            is_handled:,
          ),
        )
      #(h, jobs)
    }
    // Soft assertion: not pending -> no-op (spec says Assert)
    _ -> #(h, [])
  }
}

/// ES2024 §27.2.5.4.1 PerformPromiseThen(promise, onFulfilled, onRejected,
///                                        resultCapability)
///
/// Spec steps:
///   1. Assert: IsPromise(promise) is true.
///   2. If resultCapability is undefined, then
///      a. Let onFulfilledJobCallback be empty.
///      b. Let onRejectedJobCallback be empty.
///   3. Else,
///      a. Let onFulfilledJobCallback be HostMakeJobCallback(onFulfilled).
///      b. Let onRejectedJobCallback be HostMakeJobCallback(onRejected).
///   4. Let fulfillReaction be the PromiseReaction Record { [[Capability]]:
///      resultCapability, [[Type]]: fulfill, [[Handler]]: onFulfilledJobCallback }.
///   5. Let rejectReaction be the PromiseReaction Record { [[Capability]]:
///      resultCapability, [[Type]]: reject, [[Handler]]: onRejectedJobCallback }.
///   6. If promise.[[PromiseState]] is pending, then
///      a. Append fulfillReaction to promise.[[PromiseFulfillReactions]].
///      b. Append rejectReaction to promise.[[PromiseRejectReactions]].
///   7. Else if promise.[[PromiseState]] is fulfilled, then
///      a. Let value be promise.[[PromiseResult]].
///      b. Let fulfillJob be NewPromiseReactionJob(fulfillReaction, value).
///      c. Perform HostEnqueuePromiseJob(fulfillJob.[[Job]], fulfillJob.[[Realm]]).
///   8. Else (rejected),
///      a. Assert: The value of promise.[[PromiseState]] is rejected.
///      b. Let reason be promise.[[PromiseResult]].
///      c. If promise.[[PromiseIsHandled]] is false, perform
///         HostPromiseRejectionTracker(promise, "handle").
///      d. Let rejectJob be NewPromiseReactionJob(rejectReaction, reason).
///      e. Perform HostEnqueuePromiseJob(rejectJob.[[Job]], rejectJob.[[Realm]]).
///   9. Set promise.[[PromiseIsHandled]] to true.
///   10. If resultCapability is undefined, return undefined.
///   11. Return resultCapability.[[Promise]].
///
/// Non-callable handlers are replaced with sentinel values (JsUndefined =
/// identity pass-through, JsNull = thrower pass-through) rather than using
/// the spec's "empty" concept. Jobs are returned as a list rather than
/// directly enqueued via HostEnqueuePromiseJob.
pub fn perform_promise_then(
  h: Heap,
  data_ref: Ref,
  on_fulfilled: JsValue,
  on_rejected: JsValue,
  child_resolve: JsValue,
  child_reject: JsValue,
) -> #(Heap, List(Job)) {
  // §27.2.5.4 steps 3-4: If IsCallable(onFulfilled/onRejected) is false,
  // set to undefined. We use sentinel values instead of the spec's "empty".
  let fulfill_handler = case helpers.is_callable(h, on_fulfilled) {
    True -> on_fulfilled
    False -> value.JsUndefined
  }
  let reject_handler = case helpers.is_callable(h, on_rejected) {
    True -> on_rejected
    False -> value.JsNull
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
      let h =
        heap.write(
          h,
          data_ref,
          PromiseSlot(
            state: value.PromisePending,
            fulfill_reactions: list.append(fulfill_reactions, [
              fulfill_reaction,
            ]),
            reject_reactions: list.append(reject_reactions, [reject_reaction]),
            is_handled: True,
          ),
        )
      #(h, [])
    }
    // Step 7: Else if promise.[[PromiseState]] is fulfilled
    Some(PromiseSlot(state: value.PromiseFulfilled(val), ..)) -> {
      // Step 9: Set [[PromiseIsHandled]] to true
      let h = mark_handled(h, data_ref)
      // Step 7b-c: NewPromiseReactionJob + HostEnqueuePromiseJob
      #(h, [
        value.PromiseReactionJob(
          handler: fulfill_handler,
          arg: val,
          resolve: child_resolve,
          reject: child_reject,
        ),
      ])
    }
    // Step 8: Else (rejected)
    Some(PromiseSlot(state: value.PromiseRejected(reason), ..)) -> {
      // Step 9: Set [[PromiseIsHandled]] to true
      // Step 8c: TODO — HostPromiseRejectionTracker(promise, "handle")
      let h = mark_handled(h, data_ref)
      // Step 8d-e: NewPromiseReactionJob + HostEnqueuePromiseJob
      #(h, [
        value.PromiseReactionJob(
          handler: reject_handler,
          arg: reason,
          resolve: child_resolve,
          reject: child_reject,
        ),
      ])
    }
    _ -> #(h, [])
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
pub fn is_promise(h: Heap, val: JsValue) -> Bool {
  case val {
    // Step 1: If x is not an Object, return false.
    JsObject(ref) ->
      case heap.read(h, ref) {
        // Step 2-3: Check for [[PromiseState]] via PromiseObject kind tag
        Some(ObjectSlot(kind: PromiseObject(_), ..)) -> True
        _ -> False
      }
    _ -> False
  }
}

/// Non-spec utility: extract the internal PromiseSlot data ref from a promise
/// object ref. Used to access [[PromiseState]]/[[PromiseResult]] etc.
/// Returns None if the ref is not a promise object.
pub fn get_data_ref(h: Heap, promise_ref: Ref) -> Option(Ref) {
  case heap.read(h, promise_ref) {
    Some(ObjectSlot(kind: PromiseObject(promise_data:), ..)) ->
      Some(promise_data)
    _ -> None
  }
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
/// step 13 (enqueue thenable job).
///
/// Returns:
///   Ok(#(then_fn, state)) — value is a thenable (step 13 path)
///   Error(#(None, state)) — not an object (step 8) or then not callable (step 12)
///   Error(#(Some(thrown), state)) — Get(resolution, "then") threw (step 10)
pub fn get_thenable_then(
  state: frame.State,
  val: JsValue,
) -> Result(#(JsValue, frame.State), #(option.Option(JsValue), frame.State)) {
  case val {
    // Step 8: If resolution is not an Object -> Error(None) (caller fulfills)
    JsObject(ref) ->
      // Step 9: Let then be Completion(Get(resolution, "then"))
      case object.get_value(state, ref, "then", val) {
        Ok(#(then_val, state)) ->
          // Step 12: If IsCallable(thenAction) is false -> Error(None)
          case helpers.is_callable(state.heap, then_val) {
            True -> Ok(#(then_val, state))
            False -> Error(#(option.None, state))
          }
        // Step 10: If then is an abrupt completion -> Error(Some(thrown))
        Error(#(thrown, state)) -> Error(#(option.Some(thrown), state))
      }
    _ -> Error(#(option.None, state))
  }
}

/// Non-spec utility: set [[PromiseIsHandled]] to true on a PromiseSlot.
/// Corresponds to §27.2.5.4.1 step 9 and §27.2.1.7 step 7 context.
/// Used for unhandled rejection tracking.
fn mark_handled(h: Heap, data_ref: Ref) -> Heap {
  use slot <- heap.update(h, data_ref)
  case slot {
    PromiseSlot(..) -> PromiseSlot(..slot, is_handled: True)
    _ -> slot
  }
}
