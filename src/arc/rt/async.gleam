//// Coroutine drivers (generators, async functions, async generators),
//// promise core and the microtask drain for the shared runtime.
////
//// A coroutine body runs one turn at a time and reports a `Step`. Compiled
//// bodies are state machines applied through `apply_sm`; interpreted bodies
//// are parked frames resumed through `JsOps.resume_frame`. Either way the
//// suspension point comes back as a `Resume` inside the `Step`, is stored on
//// the coroutine's data cell, and is handed to `apply_resume` with the next
//// `Sent = #(mode, value)` (0 = next, 1 = throw, 2 = return).
////
//// A promise reaction's result capability is stored as a settle target, not
//// as a pair of resolving functions: `undefined` for no child, the child
//// promise itself when the capability is an internal %Promise% one, the
//// coroutine data cell an `await` continues, or a user capability's
//// resolve/reject function. `settle` is the one place that is cased on.
////
//// **Return-tuple order is `#(V, St')` — value FIRST (R1).**

import arc/rt/call.{
  type Completion, type Frame, NormalCompletion, ThrowCompletion, is_callable,
  t_call,
}
import arc/rt/gc as rt_gc
import arc/rt/inspect
import arc/rt/limits
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type AGResumeKind, type Agent, type AsyncGenRequest, type AsyncGenState,
  type AsyncWaiter, type GeneratorCompletion, type Handle, type Job, type JsSlot,
  type JsStore, type JsVal, type Loc, type NativeToken, type PromiseReaction,
  type PromiseState, type ReactionHandler, type Resume, type SabOwner, type SmFn,
  type Step, type WaiterRef, AGAwaitingReturn, AGCompleted, AGExecuting,
  AGResumeAwaitingReturn, AGResumeBody, AGResumeReturnUnwind, AGSuspendedStart,
  AGSuspendedYield, Agent, AsyncGenRequest, AsyncGenResume, AsyncGeneratorObj,
  AsyncWaiter, DataProperty, GenCompleted, GenExecuting, GenNext, GenReturn,
  GenSuspendedStart, GenSuspendedYield, GenThrow, GeneratorObj, Handler, HostJob,
  IdentityPassThrough, JsCell, JsStore, KHandle, Named, NoElements, Ordinary,
  PromiseFulfilled, PromiseObj, PromisePending, PromiseReaction, PromiseRejectFn,
  PromiseRejected, PromiseResolveFn, RangeErr, ReactionJob, ResolveThenableJob,
  ResumeCompiled, ResumeFrame, SAsyncContext, SAsyncGen, SBox, SGenerator,
  SObject, SPromiseData, StepAwait, StepReturn, StepThrow, StepYield, StringKey,
  ThrowerPassThrough, TypeErr, classify, jq_pop, jq_push, mk_bool, mk_object,
  mk_string, mk_undefined,
} as rt_types
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

// ── Sent modes ──────────────────────────────────────────────────────────────

/// `Sent` mode 0: normal resumption — `.next(v)` or await-fulfilled.
pub const sent_next = 0

/// `Sent` mode 1: throw injection — `.throw(e)` or await-rejected. A state
/// machine's per-arm mode-dispatch routes to the enclosing try-region's
/// catch-state or, if none, returns `{throw, sent_v}`.
pub const sent_throw = 1

/// `Sent` mode 2: return injection — `.return(v)`. Routes to the enclosing
/// finally-state with `pending = {return, v}`.
pub const sent_return = 2

/// The `Sent` pair for the initial invocation: `{0, undefined}` — state 0
/// ignores it.
pub fn sent_start() -> #(Int, JsVal) {
  #(sent_next, mk_undefined())
}

// ── running one turn ────────────────────────────────────────────────────────

/// Invoke a compiled state machine at `(rs, loc)` with `sent`: catches a JS
/// throw into `StepThrow` and decodes the wire step, folding the next
/// `(sm, rs, loc)` into the `Step`'s `ResumeCompiled`.
@external(erlang, "arc_rt_async_ffi", "apply_sm")
pub fn apply_sm(
  st: Agent,
  sm: SmFn,
  rs: Int,
  sent: #(Int, JsVal),
  loc: Loc,
) -> #(Step, Agent)

/// Continue a suspended coroutine from `resume` with a fresh `sent`.
pub fn apply_resume(
  st: Agent,
  resume: Resume,
  sent: #(Int, JsVal),
) -> #(Step, Agent) {
  case resume {
    ResumeCompiled(sm:, rs:, loc:) -> apply_sm(st, sm, rs, sent, loc)
    ResumeFrame(frame:) -> require_js(st).ops.resume_frame(st, frame, sent)
  }
}

// ── native-closure allocation ───────────────────────────────────────────────
// Data-carrying `NativeToken` variants (arc `value.gleam:3020-3055`):
// `KNative` stays payload-free; the closed-over `Handle`s ride on the tag,
// reach `dispatch_native` directly and are traced via `native_token_refs`.

/// Allocate a `KNative` function object with proto %Function.prototype% and
/// own `length` then `name` (§10.3.4 CreateBuiltinFunction steps 9-11).
/// Port of arc `common.alloc_call_fn`.
fn alloc_native_fn(
  st: Agent,
  tag: NativeToken,
  name: String,
  length: Int,
) -> #(Handle, Agent) {
  call.t_native_new(
    st,
    Some(st.realm.function.prototype),
    tag,
    name,
    length,
    False,
  )
}

/// §27.2.1.3 CreateResolvingFunctions(promise). Allocates the shared
/// `[[AlreadyResolved]]` `SBox` + the resolve/reject `KNative` pair closing
/// over `(promise_h, already_resolved_h)`. Port of arc
/// `builtins/promise.gleam:152-190`.
pub fn alloc_resolving_fns(
  st: Agent,
  promise_h: Handle,
) -> #(#(Handle, Handle), Agent) {
  let #(already_resolved, st) = rt_store.t_cell_new(st, SBox(mk_bool(False)))
  let #(resolve_h, st) =
    alloc_native_fn(st, PromiseResolveFn(promise_h, already_resolved), "", 1)
  let #(reject_h, st) =
    alloc_native_fn(st, PromiseRejectFn(promise_h, already_resolved), "", 1)
  #(#(resolve_h, reject_h), st)
}

/// The on-fulfilled/on-rejected `KNative` closure for an async generator's
/// driver-level `.return(v)` awaits (§27.6.3.9, §27.6.3.10 step 8), whose
/// `kind` the data cell alone cannot tell apart. A body `await` needs none:
/// its reaction continues the data cell directly (`settle`).
fn alloc_asyncgen_resume(
  st: Agent,
  gen_h: Handle,
  is_throw: Bool,
  kind: AGResumeKind,
) -> #(Handle, Agent) {
  alloc_native_fn(st, AsyncGenResume(gen: gen_h, is_throw:, kind:), "", 1)
}

// ── store access (private; mirrors rt_store) ─────────────────────────────

fn require_js(st: Agent) -> JsStore(Agent) {
  st.store
}

fn with_js(st: Agent, js: JsStore(Agent)) -> Agent {
  Agent(..st, store: js)
}

// ── Microtask queue (SPEC §7.M8; port of arc event_loop.gleam:84-243) ───────

/// Enqueue a `Job` on the microtask queue. Port of arc `state.enqueue_job`.
pub fn t_enqueue_job(st: Agent, job: Job) -> Agent {
  let js = require_js(st)
  with_js(st, JsStore(..js, microtasks: jq_push(js.microtasks, job)))
}

/// Drain the microtask queue to empty (§8.6 "perform a microtask
/// checkpoint"), then report and clear unhandled rejections. Port of arc
/// `event_loop.drain_jobs` + `finish_drain`. Between-jobs `t_maybe_collect`
/// is THE D11 GC safepoint: `call_depth` is zero here, so a collection can
/// only fire between jobs. Called by the runner after `js_main` and by the
/// engine after each eval/call; NEVER mid-expression.
///
/// Pending `Atomics.waitAsync` waiters are the drain's other event source
/// (§9.9 forward progress): between jobs it applies every owner wake for
/// one of this agent's waiters already delivered to this process and fires
/// every timeout job whose deadline passed; when the queue runs dry with a
/// timeout still armed it blocks in a receive for a wake until the earliest
/// deadline instead of exiting. Waiters with no timeout do not hold the
/// drain open: their wakes are picked up by the next drain (the embedder's
/// loop decides when that runs), exactly like an unsettled host promise.
/// Wakes are addressed to the BEAM process that registered the waiter, so
/// an agent with waiters pending is drained from that same process.
pub fn drain(st: Agent) -> Agent {
  let st = case st.waiters {
    [] -> st
    [_, ..] -> service_waiters(st)
  }
  let js = require_js(st)
  case jq_pop(js.microtasks) {
    None ->
      case earliest_deadline(st) {
        Some(deadline) -> drain(idle_until(st, deadline))
        None -> finish_drain(st)
      }
    Some(#(job, rest)) -> {
      let st = with_js(st, JsStore(..js, microtasks: rest))
      let st = execute_job(st, job)
      let st = rt_gc.t_maybe_collect(st)
      drain(st)
    }
  }
}

// ── Atomics.waitAsync waiters (§25.4.3.14 DoWait, async mode) ───────────────
//
// The WaiterList itself lives in the block's owner process (`rt/sab`,
// `arc_rt_sab_ffi`); `Agent.waiters` is this agent's side of it: which
// registrations are still pending here, their promise capabilities and
// deadlines. The owner wakes a registration by message
// (`{arc_sab_wake, Ref, _}` to the process that registered it); the timeout
// job withdraws it (`cancel`), and an `AlreadyWoken` answer means the wake
// beat the timeout and is already in this mailbox: the notifier counted it,
// so it is "ok".
//
// Process affinity. A wake goes to a mailbox, not to an `Agent` value, so an
// agent's waiters are serviced by draining it in the process that
// registered them. The drain only ever receives wakes for refs in its own
// `waiters` (a selective receive), so a second agent drained from the same
// process finds its wakes still queued rather than eaten.

/// The three observable outcomes of a wait (§25.4.3.14 DoWait): woken,
/// timed out, value mismatch. Every completion (the sync return value, a
/// waitAsync result object, a promise settlement) goes through
/// `wait_result_js`, so the spec's three result strings are spelled once.
/// `rt/sab.wait_sync` returns one straight from the FFI, whose reply atoms
/// are these constructors.
pub type WaitResult {
  Woken
  TimedOut
  NotEqual
}

/// The spec string a `WaitResult` surfaces to JS as.
pub fn wait_result_js(result: WaitResult) -> JsVal {
  mk_string(case result {
    Woken -> "ok"
    TimedOut -> "timed-out"
    NotEqual -> "not-equal"
  })
}

/// The longest a single `receive ... after` may wait (Erlang rejects a
/// timeout above 16#FFFFFFFF ms); a longer idle goes round the drain again.
const max_receive_ms = 0xFFFFFFFF

/// The owner's answer to a timeout job withdrawing its registration.
type Cancellation {
  /// Withdrawn before any notify reached it: "timed-out".
  Cancelled
  /// A notify already removed (and counted) it; its wake is in flight to
  /// this process, ahead of this answer.
  AlreadyWoken
}

/// Withdraw registration `ref` from `owner`'s WaiterList.
@external(erlang, "arc_rt_sab_ffi", "cancel")
fn cancel_waiter(owner: SabOwner, ref: WaiterRef) -> Cancellation

/// The oldest owner wake for one of `refs` delivered to this process,
/// waiting at most `timeout_ms` for one (negative = only what is already
/// here). Wakes for any other ref are left in the mailbox.
@external(erlang, "arc_rt_sab_ffi", "take_wake")
fn take_wake(refs: List(WaiterRef), timeout_ms: Int) -> Option(WaiterRef)

/// Consume the wake for `ref` the owner reported `AlreadyWoken`.
@external(erlang, "arc_rt_sab_ffi", "await_wake")
fn consume_wake(ref: WaiterRef) -> Nil

/// §25.4.3.14 DoWait steps 16 and 26-29, async mode, for a registration
/// (`ref`) the owner has just accepted onto its WaiterList:
/// NewPromiseCapability(%Promise%), record the WaiterRecord on this agent,
/// and arm its timeout job for `deadline` (`None` = +∞, nothing armed).
/// Returns promiseCapability.[[Promise]] — the result object's `value`.
pub fn t_add_waiter(
  st: Agent,
  owner: SabOwner,
  ref: WaiterRef,
  deadline: Option(Int),
) -> #(Handle, Agent) {
  let #(promise, st) = t_new_promise(st)
  let target = mk_object(promise)
  let waiter =
    AsyncWaiter(
      owner:,
      ref:,
      promise:,
      resolve: target,
      reject: target,
      deadline:,
    )
  #(promise, Agent(..st, waiters: list.append(st.waiters, [waiter])))
}

/// Between jobs: apply every wake already delivered, then run the timeout
/// jobs that are due by the host clock.
fn service_waiters(st: Agent) -> Agent {
  fire_due_waiters(apply_pending_wakes(st), st.hooks.monotonic_now())
}

/// The refs of this agent's pending registrations: the only wakes it takes.
fn pending_refs(st: Agent) -> List(WaiterRef) {
  list.map(st.waiters, fn(w) { w.ref })
}

/// Apply every wake for one of this agent's waiters already in this
/// process's mailbox.
fn apply_pending_wakes(st: Agent) -> Agent {
  case take_wake(pending_refs(st), -1) {
    None -> st
    Some(ref) -> apply_pending_wakes(t_wake_waiter(st, ref))
  }
}

/// Dry queue with a timeout job armed for `deadline`: block for a wake for
/// one of this agent's waiters until then. A wake that arrives is applied
/// (its resolve job refills the queue). Otherwise the whole span has passed
/// on the BEAM's own clock, and every job armed for `deadline` runs now
/// whatever `hooks.monotonic_now` reads: the receive waited in real time,
/// so a host clock that does not advance by itself (virtualised, frozen)
/// is not what decides the job is due, or the drain would idle here again
/// and again with nothing able to move it on.
fn idle_until(st: Agent, deadline: Int) -> Agent {
  let wait_ms = int.max(deadline - st.hooks.monotonic_now(), 0) + 1
  case take_wake(pending_refs(st), wait_ms) {
    Some(ref) -> t_wake_waiter(st, ref)
    None if wait_ms <= max_receive_ms -> fire_due_waiters(st, deadline)
    // One receive cannot wait that long: not there yet, go round again.
    None -> st
  }
}

/// §25.4.3.12 NotifyWaiter as it lands on the waiting agent: the
/// registration leaves `waiters` (which cancels its timeout job) and
/// EnqueueResolveInAgentJob queues the job that resolves its promise with
/// "ok": a JOB, never a synchronous settle; drain afterwards. A wake naming
/// no registration pending on THIS agent changes nothing (and is lost to
/// whichever agent it did belong to). The drain takes its own wakes; this
/// is for an embedder idle loop that received an
/// `{arc_sab_wake, Ref, async}` message of its own accord, in the process
/// that registered the waiter, while the agent had nothing to run.
pub fn t_wake_waiter(st: Agent, ref: WaiterRef) -> Agent {
  case list.partition(st.waiters, fn(w) { w.ref == ref }) {
    #([], _) -> st
    #([w, ..], kept) -> enqueue_resolve_ok(Agent(..st, waiters: kept), w)
  }
}

fn enqueue_resolve_ok(st: Agent, w: AsyncWaiter) -> Agent {
  t_enqueue_job(
    st,
    ReactionJob(
      handler: IdentityPassThrough,
      arg: wait_result_js(Woken),
      resolve: w.resolve,
      reject: w.reject,
    ),
  )
}

/// Run every armed waitAsync timeout job whose deadline is at or before
/// `cutoff` (§25.4.3.14 step 29.b's job body): withdraw the registration
/// from the owner and `! Call(promiseCapability.[[Resolve]], undefined,
/// « "timed-out" »)` right here, between microtasks. If a notify got to
/// the registration first the owner says so; that wake is consumed now and
/// the waiter is woken instead, so a counted notify is never lost.
fn fire_due_waiters(st: Agent, cutoff: Int) -> Agent {
  let #(due, pending) =
    list.partition(st.waiters, fn(w) {
      case w.deadline {
        Some(d) -> d <= cutoff
        None -> False
      }
    })
  list.fold(due, Agent(..st, waiters: pending), fn(st, w) {
    case cancel_waiter(w.owner, w.ref) {
      Cancelled -> settle(st, w.resolve, Fulfil, wait_result_js(TimedOut))
      AlreadyWoken -> {
        let Nil = consume_wake(w.ref)
        enqueue_resolve_ok(st, w)
      }
    }
  })
}

/// The earliest deadline among the armed waitAsync timeout jobs, if any.
fn earliest_deadline(st: Agent) -> Option(Int) {
  list.fold(st.waiters, None, fn(acc, w) {
    case acc, w.deadline {
      None, d -> d
      Some(a), Some(d) -> Some(int.min(a, d))
      Some(a), None -> Some(a)
    }
  })
}

/// The drain's terminal exit: report every promise still rejected with no
/// handler (newest first, as tracked) and clear the list. Port of arc
/// `event_loop.finish_drain` / `report_unhandled_rejections`.
fn finish_drain(st: Agent) -> Agent {
  let js = require_js(st)
  list.each(js.unhandled_rejections, fn(id) {
    case rt_store.t_cell_get(st, JsCell(id)) {
      SPromiseData(state: PromiseRejected(reason), ..) ->
        st.hooks.report_uncaught(
          "Uncaught (in promise) " <> describe_thrown(st, reason),
        )
      _ -> Nil
    }
  })
  with_js(st, JsStore(..js, unhandled_rejections: []))
}

/// Which of a result capability's two functions a settlement stands for.
type Side {
  Fulfil
  Reject
}

/// Settle a reaction's result capability, stored as `target` (the module doc
/// lists the shapes). Resolving functions (a species constructor's, or ones
/// minted by `t_new_promise_capability`) are called; the internal shapes
/// settle in place, which is all their resolving functions could ever have
/// done: nothing else holds them, so `[[AlreadyResolved]]` reduces to the
/// pending check.
fn settle(st: Agent, target: JsVal, side: Side, value: JsVal) -> Agent {
  case classify(target) {
    rt_types.KUndef -> st
    KHandle(h) ->
      case rt_store.t_cell_get(st, h), side {
        SObject(kind: PromiseObj(..), ..), Fulfil ->
          t_promise_resolve(st, h, value)
        SObject(kind: PromiseObj(..), ..), Reject ->
          t_promise_reject(st, h, value)
        SAsyncContext(resume:, promise:), _ -> {
          use st <- resume_from_job(st)
          let #(step, st) = apply_resume(st, resume, sent_of(side, value))
          drive_async_step(st, Some(h), promise, step)
        }
        SAsyncGen(..), _ -> {
          use st <- resume_from_job(st)
          resume_asyncgen(st, h, sent_of(side, value))
        }
        _, _ -> call_settle(st, target, [value])
      }
    _ -> call_settle(st, target, [value])
  }
}

/// §27.7.5.3 Await steps 3.c / 5.c: the `Sent` pair a settled await resumes
/// its coroutine with.
fn sent_of(side: Side, value: JsVal) -> #(Int, JsVal) {
  case side {
    Fulfil -> #(sent_next, value)
    Reject -> #(sent_throw, value)
  }
}

/// Run a coroutine turn from a reaction job the way calling a resume closure
/// would have: inside the call-depth bracket (D11: an interpreter body only
/// collects at its own returns while it owns every unit of `call_depth`) and
/// under the job's catch, so a throw escaping the turn is reported rather
/// than unwound through the drain.
fn resume_from_job(st: Agent, turn: fn(Agent) -> Agent) -> Agent {
  let st = rt_store.t_enter_call(st)
  let #(outcome, st) = protected(st, fn(st) { #(mk_undefined(), turn(st)) })
  report_job_throw(#(outcome, rt_store.t_leave_call(st)))
}

/// Fire-and-forget invoke of a promise-capability resolve/reject fn during
/// job execution: the return value is discarded (a job has no continuation
/// to hand it to) and an abrupt completion is reported rather than
/// propagated (a job has no caller to propagate to). Port of arc
/// `job_call.call_settlement_fn`.
fn call_settle(st: Agent, target: JsVal, args: List(JsVal)) -> Agent {
  report_job_throw(t_call(st, target, mk_undefined(), args))
}

/// `target` in `call_settle` is typically a promise-capability resolve/reject
/// function. The native ones never throw, but `Promise.prototype.then` builds
/// the child capability with NewPromiseCapability(SpeciesConstructor(this)),
/// so a user species constructor hands us arbitrary user callables. There is
/// no promise to blame the throw on (it happened AFTER the reaction settled),
/// so `unhandled_rejections` cannot carry it; report it through the host sink
/// instead of letting it vanish. A throwing `HostJob` lands here too.
fn report_job_throw(outcome: #(Completion, Agent)) -> Agent {
  case outcome {
    #(NormalCompletion(_), st) -> st
    #(ThrowCompletion(thrown), st) -> {
      st.hooks.report_uncaught(
        "Uncaught (in promise job) " <> describe_thrown(st, thrown),
      )
      st
    }
  }
}

/// Render a thrown value for a host report without running user code: an
/// Error's recorded stack (its `name: message` header), a string as-is,
/// anything else by its primitive rendering.
fn describe_thrown(st: Agent, thrown: JsVal) -> String {
  inspect.format_error(st, thrown)
}

/// Run one microtask job. Port of arc `event_loop.execute_job` +
/// `execute_reaction_job` + `execute_thenable_job` + `execute_host_job`.
fn execute_job(st: Agent, job: Job) -> Agent {
  case job {
    // §27.2.2.1 NewPromiseReactionJob.
    ReactionJob(handler:, arg:, resolve:, reject:) ->
      case handler {
        IdentityPassThrough -> settle(st, resolve, Fulfil, arg)
        ThrowerPassThrough -> settle(st, reject, Reject, arg)
        Handler(fun) ->
          case t_call(st, fun, mk_undefined(), [arg]) {
            #(NormalCompletion(v), st) -> settle(st, resolve, Fulfil, v)
            #(ThrowCompletion(e), st) -> settle(st, reject, Reject, e)
          }
      }
    // §27.2.2.2 NewPromiseResolveThenableJob.
    ResolveThenableJob(thenable:, then_fn:, resolve:, reject:) ->
      case t_call(st, then_fn, thenable, [resolve, reject]) {
        #(NormalCompletion(_), st) -> st
        #(ThrowCompletion(e), st) -> call_settle(st, reject, [e])
      }
    HostJob(run:) ->
      report_job_throw(protected(st, fn(st) { #(mk_undefined(), run(st)) }))
  }
}

// ── error helper (mirror of rt_obj.throw_type_error) ─────────────────────

/// Allocate a `TypeError(msg)` via the seeded `ops.new_error` and RAISE it
/// (D7 — never `Result`). Never returns.
fn throw_type_error(st: Agent, msg: String) -> a {
  let #(e, st) = require_js(st).ops.new_error(st, TypeErr, msg)
  rt_store.t_throw(st, e)
}

// ── §7.4.11 CreateIterResultObject ──────────────────────────────────────────

/// §7.4.11 CreateIterResultObject(value, done) — allocate a fresh ordinary
/// `{value, done}` with proto = `%Object.prototype%`. Port of arc
/// `common.create_iter_result` re-expressed over threaded `t_next_prop_seq`.
pub fn alloc_iter_result(
  st: Agent,
  value: JsVal,
  done: Bool,
) -> #(Handle, Agent) {
  let #(seq0, st) = rt_store.t_next_prop_seq(st)
  let #(seq1, st) = rt_store.t_next_prop_seq(st)
  let props =
    dict.from_list([
      #(Named("value"), DataProperty(value, True, True, True, seq0)),
      #(Named("done"), DataProperty(mk_bool(done), True, True, True, seq1)),
    ])
  rt_store.t_cell_new(
    st,
    SObject(
      kind: Ordinary,
      proto: Some(st.realm.object.prototype),
      props:,
      symbol_props: [],
      elements: NoElements,
      extensible: True,
    ),
  )
}

/// Allocate the JS-visible object of `kind` over an already-allocated data
/// cell: ordinary, extensible, `[[Prototype]] = proto`.
fn alloc_shell(
  st: Agent,
  kind: rt_types.ObjKind,
  proto: Option(Handle),
) -> #(Handle, Agent) {
  rt_store.t_cell_new(
    st,
    SObject(
      kind:,
      proto:,
      props: dict.new(),
      symbol_props: [],
      elements: NoElements,
      extensible: True,
    ),
  )
}

// ── sync generator driver (port arc generators.gleam:52-331) ───────────────
// Two cells: the JS-visible `SObject(kind: GeneratorObj(data))` and the
// `SGenerator` data cell. `t_gen_start` returns the object; the prototype
// methods brand-check the object (`generator_data`) and drive the data cell.
// One resume covers arc's `run_to_completion` / `unwind_return` / catch-
// unwinding: `apply_resume` re-enters the body with `Sent = {mode, value}` and
// the body itself routes to the enclosing catch/finally, so this driver never
// walks a try-stack. `yield*` is lowered inside the body — no delegate arm.

/// §27.5.3.2 GeneratorValidate brand check: the `SGenerator` data handle
/// behind generator object `this`, or a TypeError.
pub fn generator_data(st: Agent, this: JsVal) -> Handle {
  let data = case classify(this) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: GeneratorObj(data:), ..) -> Some(data)
        _ -> None
      }
    _ -> None
  }
  case data {
    Some(data) -> data
    None ->
      throw_type_error(
        st,
        "Generator.prototype method called on incompatible receiver",
      )
  }
}

/// Read the `SGenerator` data cell at `gen_h`.
fn read_generator(st: Agent, gen_h: Handle) -> JsSlot {
  case rt_store.t_cell_get(st, gen_h) {
    SGenerator(..) as gen -> gen
    _ -> panic as "rt_async: Handle is not an SGenerator cell (engine invariant)"
  }
}

/// Write `SGenerator` back with only `state` changed. Port of arc
/// `gen_with_state` + `heap.write` composed.
fn set_gen_state(
  st: Agent,
  gen_h: Handle,
  gen: JsSlot,
  new_state: rt_types.GeneratorState,
) -> Agent {
  let assert SGenerator(resume:, ..) = gen
  rt_store.t_cell_set(st, gen_h, SGenerator(state: new_state, resume:))
}

/// §27.5.1.2 GeneratorStart — allocate the generator for a call to a compiled
/// generator function: the `SGenerator` data cell plus the JS-visible
/// `GeneratorObj`, whose prototype is the function's own `prototype` object
/// (§27.3.3.1 EvaluateGeneratorBody: OrdinaryCreateFromConstructor), else
/// `%GeneratorPrototype%`. Captures are already curried into `sm` by
/// `MakeClosure` and the arguments are already packed into `loc0` by the
/// outer prologue, so `_args` is accepted for parity with the op-table's
/// 4-arg `gen_start` row but never read. Returns the object handle.
pub fn t_gen_start(
  st: Agent,
  sm: SmFn,
  frame: Frame,
  _args: List(JsVal),
  loc0: Loc,
) -> #(Handle, Agent) {
  t_gen_new(
    st,
    call.frame_active_func(frame),
    ResumeCompiled(sm:, rs: 0, loc: loc0),
  )
}

/// GeneratorStart for a body that begins at `resume` (a state machine at
/// state 0, or an interpreter frame parked at its InitialYield): the
/// `SGenerator` data cell in SuspendedStart plus the object, whose prototype
/// comes from the generator function `callee`. Returns the object handle.
pub fn t_gen_new(st: Agent, callee: JsVal, resume: Resume) -> #(Handle, Agent) {
  let #(data, st) =
    rt_store.t_cell_new(st, SGenerator(state: GenSuspendedStart, resume:))
  let proto = generator_prototype(st, callee, fn(r) { r.generator.prototype })
  alloc_shell(st, GeneratorObj(data:), Some(proto))
}

/// §10.1.14 GetPrototypeFromConstructor for a generator function's call:
/// its own `prototype` data property when that holds an object, else the
/// `intrinsic` of the function's realm (§7.3.24 GetFunctionRealm). The
/// property is non-configurable data, so the own-slot read is the whole of
/// the observable Get.
fn generator_prototype(
  st: Agent,
  callee: JsVal,
  intrinsic: fn(rt_types.Realm) -> Handle,
) -> Handle {
  case classify(callee) {
    KHandle(fn_h) ->
      case
        rt_obj.t_ordinary_own_property(st, fn_h, StringKey(Named("prototype")))
      {
        Some(DataProperty(value:, ..)) ->
          case classify(value) {
            KHandle(p) -> p
            _ -> intrinsic(call.function_realm(st, fn_h))
          }
        _ -> intrinsic(call.function_realm(st, fn_h))
      }
    _ -> intrinsic(st.realm)
  }
}

/// §27.5.3.3 GeneratorResume — `Generator.prototype.next(value)` on the data
/// cell `gen_h`. Returns a fresh iter-result `{value, done}` handle. Port of
/// arc `call_native_generator_next` (arc:52-62).
pub fn t_gen_next(st: Agent, gen_h: Handle, sent: JsVal) -> #(Handle, Agent) {
  let #(#(done, v), st) = t_gen_step(st, gen_h, sent)
  alloc_iter_result(st, v, done)
}

/// GeneratorResume as a bare `#(done, value)`: what `t_gen_next` builds its
/// result object from. The interpreter's `for..of` / `yield*` over a native
/// generator take this directly instead of allocating the object and reading
/// it straight back. Port of arc `resume_generator_next` (arc:71-102).
pub fn t_gen_step(
  st: Agent,
  gen_h: Handle,
  sent: JsVal,
) -> #(#(Bool, JsVal), Agent) {
  let gen = read_generator(st, gen_h)
  let assert SGenerator(state:, resume:) = gen
  case state {
    GenCompleted -> #(#(True, mk_undefined()), st)
    GenExecuting -> throw_type_error(st, "Generator is already running")
    // SuspendedStart: the first turn ignores `sent` but is otherwise a
    // normal resume.
    GenSuspendedStart | GenSuspendedYield ->
      gen_resume(st, gen_h, gen, resume, #(sent_next, sent))
  }
}

/// §27.5.3.4 GeneratorResumeAbrupt with a return completion —
/// `Generator.prototype.return(value)`. Port of arc
/// `call_native_generator_return` (arc:116-199) with `unwind_return` folded
/// into the body's mode-2 dispatch.
pub fn t_gen_return(st: Agent, gen_h: Handle, v: JsVal) -> #(Handle, Agent) {
  let gen = read_generator(st, gen_h)
  let assert SGenerator(state:, resume:) = gen
  case state {
    GenExecuting -> throw_type_error(st, "Generator is already running")
    // §27.5.3.4 step 5: SuspendedStart → Completed, no body run. Also covers
    // an already-Completed generator (step 8.a with return completion).
    GenCompleted | GenSuspendedStart -> {
      let st = set_gen_state(st, gen_h, gen, GenCompleted)
      alloc_iter_result(st, v, True)
    }
    GenSuspendedYield -> {
      let #(#(done, v), st) =
        gen_resume(st, gen_h, gen, resume, #(sent_return, v))
      alloc_iter_result(st, v, done)
    }
  }
}

/// §27.5.3.4 GeneratorResumeAbrupt with a throw completion —
/// `Generator.prototype.throw(exception)`. Port of arc
/// `call_native_generator_throw` (arc:202-283) with `unwind_to_catch` folded
/// into the body's mode-1 dispatch.
pub fn t_gen_throw(st: Agent, gen_h: Handle, e: JsVal) -> #(Handle, Agent) {
  let gen = read_generator(st, gen_h)
  let assert SGenerator(state:, resume:) = gen
  case state {
    GenExecuting -> throw_type_error(st, "Generator is already running")
    // §27.5.3.4 step 5 + step 8.b: SuspendedStart / Completed → mark
    // Completed and propagate the throw (arc `complete_and_throw`).
    GenCompleted | GenSuspendedStart -> {
      let st = set_gen_state(st, gen_h, gen, GenCompleted)
      rt_store.t_throw(st, e)
    }
    GenSuspendedYield -> {
      let #(#(done, v), st) =
        gen_resume(st, gen_h, gen, resume, #(sent_throw, e))
      alloc_iter_result(st, v, done)
    }
  }
}

/// Resume a suspended generator with `sent` and marshal the `Step` back into
/// the sync-driver convention as `#(done, value)`. Port of arc
/// `build_resumed_state` + `run_to_completion` + `settle_completion`
/// (arc:388-610). Bracketed with `t_enter_call`/`t_leave_call` — arc bumps
/// `call_depth` for the exact same D11 reason (arc:382).
fn gen_resume(
  st: Agent,
  gen_h: Handle,
  gen: JsSlot,
  resume: Resume,
  sent: #(Int, JsVal),
) -> #(#(Bool, JsVal), Agent) {
  let st = set_gen_state(st, gen_h, gen, GenExecuting)
  let st = rt_store.t_enter_call(st)
  let #(step, st) = apply_resume(st, resume, sent)
  let st = rt_store.t_leave_call(st)
  case step {
    StepReturn(v) -> #(#(True, v), set_gen_state(st, gen_h, gen, GenCompleted))
    StepThrow(e) -> {
      let st = set_gen_state(st, gen_h, gen, GenCompleted)
      rt_store.t_throw(st, e)
    }
    StepYield(value:, resume:) -> {
      let st =
        rt_store.t_cell_set(
          st,
          gen_h,
          SGenerator(state: GenSuspendedYield, resume:),
        )
      #(#(False, value), st)
    }
    // The bytecode compiler and the state-machine lowering never emit an
    // await step for a sync generator body.
    StepAwait(..) ->
      panic as "rt_async: sync generator body produced an await step"
  }
}

// ── Promise core (§27.2; port arc builtins/promise.gleam:89-718 +
//    exec/promises.gleam:483-563) ──────────────────────────────────────────────
// Two cells per promise: the JS-visible `SObject(kind: PromiseObj(data))`
// (prototype, own properties, extensible like any object) and the
// `SPromiseData` cell holding [[PromiseState]] / [[PromiseIsHandled]]. Every
// API here takes the OBJECT handle and hops to the data cell;
// `unhandled_rejections` tracks data-cell ids.

/// Run a threaded thunk under the same `{wasm_exn,0,[St,V]}` try/catch as
/// `t_call_protected` — used to catch a throwing `.then` accessor during
/// thenable resolution (§27.2.1.3.2 step 10). Bound to the call-FFI
/// `t_apply_protected/2` so no new Erlang is written.
@external(erlang, "arc_rt_call_ffi", "t_apply_protected")
fn protected(
  st: Agent,
  body: fn(Agent) -> #(JsVal, Agent),
) -> #(Completion, Agent)

/// IsPromise(v) (§27.2.1.6): the promise OBJECT handle when `v` is one.
pub fn as_promise(st: Agent, v: JsVal) -> Option(Handle) {
  case classify(v) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: PromiseObj(..), ..) -> Some(h)
        _ -> None
      }
    _ -> None
  }
}

/// The data cell behind promise object `promise_h`:
/// `#(data_h, state, is_handled)`.
pub fn promise_data(
  st: Agent,
  promise_h: Handle,
) -> #(Handle, PromiseState, Bool) {
  case rt_store.t_cell_get(st, promise_h) {
    SObject(kind: PromiseObj(data:), ..) ->
      case rt_store.t_cell_get(st, data) {
        SPromiseData(state:, is_handled:) -> #(data, state, is_handled)
        _ ->
          panic as "rt_async: PromiseObj data is not SPromiseData (engine invariant)"
      }
    // Every caller reaches here via a handle it minted with `t_new_promise`
    // or brand-checked with `as_promise`; anything else is heap corruption.
    _ -> panic as "rt_async: Handle is not a promise object (engine invariant)"
  }
}

/// Allocate a fresh pending promise (§27.2.3.1 steps 3-7) whose
/// [[Prototype]] is `proto` — OrdinaryCreateFromConstructor for a subclass
/// `new_target`. Returns the object handle.
pub fn t_new_promise_with_proto(
  st: Agent,
  proto: Option(Handle),
) -> #(Handle, Agent) {
  let #(data, st) =
    rt_store.t_cell_new(st, SPromiseData(PromisePending([]), False))
  alloc_shell(st, PromiseObj(data:), proto)
}

/// A fresh pending %Promise% instance. Port of arc `create_promise`
/// (builtins/promise.gleam:89-116).
pub fn t_new_promise(st: Agent) -> #(Handle, Agent) {
  t_new_promise_with_proto(st, Some(st.realm.promise.prototype))
}

/// §27.2.1.5 NewPromiseCapability(%Promise%) — `t_new_promise` +
/// `alloc_resolving_fns`. Returns `#(#(promise_h, resolve_h, reject_h), st)`.
/// Port of arc `new_promise_capability` (builtins/promise.gleam:207-216).
pub fn t_new_promise_capability(
  st: Agent,
) -> #(#(Handle, Handle, Handle), Agent) {
  let #(promise_h, st) = t_new_promise(st)
  let #(#(resolve_h, reject_h), st) = alloc_resolving_fns(st, promise_h)
  #(#(promise_h, resolve_h, reject_h), st)
}

/// §27.2.1.4 FulfillPromise(promise, value). Pending-only guard is a soft
/// no-op (spec says Assert). Enqueues one `ReactionJob` per stored reaction
/// in attachment order (reactions stored newest-first — reverse once here).
/// Port of arc `fulfill_promise` + `settle_promise`
/// (builtins/promise.gleam:233-367).
fn fulfill_promise(st: Agent, promise_h: Handle, value: JsVal) -> Agent {
  case promise_data(st, promise_h) {
    #(data, PromisePending(reactions), is_handled) -> {
      let st =
        rt_store.t_cell_set(
          st,
          data,
          SPromiseData(PromiseFulfilled(value), is_handled),
        )
      enqueue_reactions(st, reactions, value, on_fulfill_handler)
    }
    _ -> st
  }
}

/// §27.2.1.7 RejectPromise(promise, reason). Step 7: on `is_handled == False`,
/// prepend the data-cell id to `unhandled_rejections`
/// (HostPromiseRejectionTracker "reject"). Port of arc `reject_promise`
/// (builtins/promise.gleam:386-426).
pub fn t_promise_reject(st: Agent, promise_h: Handle, reason: JsVal) -> Agent {
  case promise_data(st, promise_h) {
    #(data, PromisePending(reactions), is_handled) -> {
      let st =
        rt_store.t_cell_set(
          st,
          data,
          SPromiseData(PromiseRejected(reason), is_handled),
        )
      let st = case is_handled {
        False -> {
          let js = require_js(st)
          with_js(
            st,
            JsStore(..js, unhandled_rejections: [
              data.id,
              ..js.unhandled_rejections
            ]),
          )
        }
        True -> st
      }
      enqueue_reactions(st, reactions, reason, on_reject_handler)
    }
    _ -> st
  }
}

/// §27.2.1.8 TriggerPromiseReactions — enqueue one `ReactionJob` per stored
/// reaction. `pick` selects `on_fulfill` vs `on_reject` from each record
/// (both handlers are stored per reaction; arc uses two lists).
fn enqueue_reactions(
  st: Agent,
  reactions: List(PromiseReaction),
  arg: JsVal,
  pick: fn(PromiseReaction) -> ReactionHandler,
) -> Agent {
  list.fold(list.reverse(reactions), st, fn(st, r) {
    t_enqueue_job(
      st,
      ReactionJob(
        handler: pick(r),
        arg:,
        resolve: r.child_resolve,
        reject: r.child_reject,
      ),
    )
  })
}

fn on_fulfill_handler(r: PromiseReaction) -> ReactionHandler {
  r.on_fulfill
}

fn on_reject_handler(r: PromiseReaction) -> ReactionHandler {
  r.on_reject
}

/// §27.2.1.3.2 Promise Resolve Functions steps 7-16 — the resolve-function
/// body minus the `[[AlreadyResolved]]` gate (the `PromiseResolveFn`
/// dispatch owns that; the pending-only guard in `fulfill_promise` is the
/// belt-and-suspenders). Self-resolution → reject with TypeError; thenable →
/// enqueue `ResolveThenableJob`; throwing `.then` accessor → reject; else
/// fulfill. Port of arc `resolve_promise` + `get_thenable_then` +
/// `call_native_promise_resolve_fn`
/// (builtins/promise.gleam:627-704, exec/promises.gleam:483-532).
pub fn t_promise_resolve(
  st: Agent,
  promise_h: Handle,
  resolution: JsVal,
) -> Agent {
  case classify(resolution) {
    // Step 7: SameValue(resolution, promise) → self-resolution TypeError.
    KHandle(h) if h == promise_h -> {
      let #(e, st) =
        require_js(st).ops.new_error(
          st,
          TypeErr,
          "Chaining cycle detected for promise",
        )
      t_promise_reject(st, promise_h, e)
    }
    // Steps 8-16: object → look up `.then`; anything else → fulfill.
    KHandle(h) -> resolve_with_handle(st, promise_h, resolution, h)
    _ -> fulfill_promise(st, promise_h, resolution)
  }
}

/// §27.2.1.3.2 steps 9-16 for a `KHandle` resolution. A JS object (plain,
/// shaped, or a native promise) runs the thenable protocol; an internal data
/// cell has no `.then` and fulfills as-is.
fn resolve_with_handle(
  st: Agent,
  promise_h: Handle,
  resolution: JsVal,
  h: Handle,
) -> Agent {
  case rt_store.t_cell_get(st, h) {
    SObject(..) | rt_types.SShapedObject(..) -> {
      // Step 9: then = Completion(Get(resolution, "then")).
      let #(outcome, st) =
        protected(st, fn(st) {
          rt_obj.t_get_prop(st, resolution, StringKey(Named("then")))
        })
      case outcome {
        // Step 10: abrupt → RejectPromise(promise, then.[[Value]]).
        ThrowCompletion(e) -> t_promise_reject(st, promise_h, e)
        NormalCompletion(then_val) ->
          case is_callable(st, then_val) {
            // Step 12: not callable → FulfillPromise(promise, resolution).
            False -> fulfill_promise(st, promise_h, resolution)
            // Steps 13-15: enqueue PromiseResolveThenableJob.
            True -> {
              let #(#(resolve_h, reject_h), st) =
                alloc_resolving_fns(st, promise_h)
              t_enqueue_job(
                st,
                ResolveThenableJob(
                  thenable: resolution,
                  then_fn: then_val,
                  resolve: mk_object(resolve_h),
                  reject: mk_object(reject_h),
                ),
              )
            }
          }
      }
    }
    _ -> fulfill_promise(st, promise_h, resolution)
  }
}

/// §27.2.4.7.1 PromiseResolve(%Promise%, x). If `v` is already a promise
/// object, return its handle unchanged (step 2.b — the SameValue constructor
/// check collapses to an IsPromise check for the intrinsic %Promise%); else
/// allocate a fresh pending promise and `t_promise_resolve` it with `v`. Port
/// of arc `promise_resolve` (builtins/promise.gleam:720-744).
pub fn promise_resolve_static(st: Agent, v: JsVal) -> #(Handle, Agent) {
  case as_promise(st, v) {
    Some(h) -> #(h, st)
    None -> {
      let #(h, st) = t_new_promise(st)
      #(h, t_promise_resolve(st, h, v))
    }
  }
}

/// §27.2.5.4.1 PerformPromiseThen(promise, onFulfilled, onRejected,
/// resultCapability) with NewPromiseCapability(%Promise%) as the result.
/// Returns the child promise handle. The capability's resolving functions
/// could never reach user code, so the child promise itself is the reaction's
/// settle target and none are allocated. Port of arc `perform_promise_then`
/// (builtins/promise.gleam:465-568).
pub fn t_promise_then(
  st: Agent,
  promise_h: Handle,
  on_fulfilled: JsVal,
  on_rejected: JsVal,
) -> #(Handle, Agent) {
  let #(child_h, st) = t_new_promise(st)
  let child = mk_object(child_h)
  #(
    child_h,
    t_perform_then(st, promise_h, on_fulfilled, on_rejected, child, child),
  )
}

/// §27.2.5.4.1 PerformPromiseThen with a caller-supplied capability: attach
/// the reaction with `resolve`/`reject` as the child's settle targets (its
/// resolving functions, or one of the internal shapes `settle` takes).
/// Shared by `t_promise_then`, `Promise.prototype.then` (species capability)
/// and §27.1.4.4 AsyncFromSyncIteratorContinuation.
pub fn t_perform_then(
  st: Agent,
  promise_h: Handle,
  on_fulfilled: JsVal,
  on_rejected: JsVal,
  resolve: JsVal,
  reject: JsVal,
) -> Agent {
  // Steps 3-6: non-callable → the spec's "empty" handler.
  let fulfill_handler = to_handler(st, on_fulfilled, IdentityPassThrough)
  let reject_handler = to_handler(st, on_rejected, ThrowerPassThrough)
  perform_then(st, promise_h, fulfill_handler, reject_handler, resolve, reject)
}

/// §27.7.5.3 Await steps 2-7 / §27.6.3.8: PromiseResolve(%Promise%,
/// `awaited`), then PerformPromiseThen with no result capability whose
/// onFulfilled/onRejected continue the coroutine behind data cell `data_h`.
/// The value passes straight through to `settle`, which resumes the cell, so
/// no closure pair is allocated per await. Always enqueues (an
/// already-settled promise still resumes asynchronously).
fn await_into(st: Agent, data_h: Handle, awaited: JsVal) -> Agent {
  let #(awaited_h, st) = promise_resolve_static(st, awaited)
  let target = mk_object(data_h)
  perform_then(
    st,
    awaited_h,
    IdentityPassThrough,
    ThrowerPassThrough,
    target,
    target,
  )
}

/// PerformPromiseThen steps 7-13 over already-classified handlers.
fn perform_then(
  st: Agent,
  promise_h: Handle,
  fulfill_handler: ReactionHandler,
  reject_handler: ReactionHandler,
  resolve: JsVal,
  reject: JsVal,
) -> Agent {
  case promise_data(st, promise_h) {
    // Step 9: pending → append reaction; step 12: [[PromiseIsHandled]] = true.
    // Stored newest-first (O(1) prepend), reversed once at settle time.
    #(data, PromisePending(reactions), _) ->
      rt_store.t_cell_set(
        st,
        data,
        SPromiseData(
          PromisePending([
            PromiseReaction(
              on_fulfill: fulfill_handler,
              on_reject: reject_handler,
              child_resolve: resolve,
              child_reject: reject,
            ),
            ..reactions
          ]),
          True,
        ),
      )
    // Step 10: fulfilled → mark handled + enqueue fulfill reaction job.
    #(data, PromiseFulfilled(value) as state, _) -> {
      let st = rt_store.t_cell_set(st, data, SPromiseData(state, True))
      t_enqueue_job(
        st,
        ReactionJob(handler: fulfill_handler, arg: value, resolve:, reject:),
      )
    }
    // Step 11: rejected → mark handled + untrack rejection + enqueue.
    #(data, PromiseRejected(reason) as state, is_handled) -> {
      let st = rt_store.t_cell_set(st, data, SPromiseData(state, True))
      let st = case is_handled {
        False -> untrack_rejection(st, data)
        True -> st
      }
      t_enqueue_job(
        st,
        ReactionJob(handler: reject_handler, arg: reason, resolve:, reject:),
      )
    }
  }
}

/// Steps 3-6 helper: `Handler(v)` if callable, else the given pass-through.
fn to_handler(
  st: Agent,
  v: JsVal,
  otherwise: ReactionHandler,
) -> ReactionHandler {
  case is_callable(st, v) {
    True -> Handler(v)
    False -> otherwise
  }
}

/// HostPromiseRejectionTracker(promise, "handle") — drop the data cell's id
/// from `unhandled_rejections` (§27.2.5.4.1 step 11c).
fn untrack_rejection(st: Agent, data: Handle) -> Agent {
  let js = require_js(st)
  with_js(
    st,
    JsStore(
      ..js,
      unhandled_rejections: list.filter(js.unhandled_rejections, fn(r) {
        r != data.id
      }),
    ),
  )
}

// ════════════════════════════════════════════════════════════════════════════
// Async-generator driver — ES2024 §27.6
// ════════════════════════════════════════════════════════════════════════════
//
// Unlike sync generators (`.next()` runs the body synchronously), async gens
// enqueue requests and return promises. `drain_queue` pulls requests off and
// settles them:
//   yield  → resolve head with {value, done:false}, SuspendedYield, drain
//   await  → suspend (state stays Executing), resume via microtask
//   return → resolve head with {value, done:true}, Completed, drain rest
//   throw  → reject head, Completed, drain rest
//
// The request queue is the key difference: callers can fire next();next();
// next() before any settle, and each gets its own promise.
//
// Two cells (matches `t_gen_start`): the JS-visible
// `SObject(kind: AsyncGeneratorObj(data))` and the `SAsyncGen` data cell.
// The prototype methods brand-check the object; everything below drives the
// data handle. `yield*` delegation is lowered inside the body — no delegate
// arms here.

/// §27.6.3.1 AsyncGeneratorStart. Alloc the `SAsyncGen` data cell in
/// `SuspendedStart` plus the `AsyncGeneratorObj`, whose prototype comes from
/// the function's own `prototype` as for `t_gen_start`. Args accepted for
/// op-table 4-arg parity, unused (already packed into loc0). Returns the
/// object handle.
pub fn t_asyncgen_start(
  st: Agent,
  sm: SmFn,
  frame: Frame,
  _args: List(JsVal),
  loc0: Loc,
) -> #(Handle, Agent) {
  t_asyncgen_new(
    st,
    call.frame_active_func(frame),
    ResumeCompiled(sm:, rs: 0, loc: loc0),
  )
}

/// AsyncGeneratorStart for a body that begins at `resume`: the `SAsyncGen`
/// data cell in SuspendedStart with an empty request queue plus the object,
/// whose prototype comes from the function `callee`. Returns the object.
pub fn t_asyncgen_new(
  st: Agent,
  callee: JsVal,
  resume: Resume,
) -> #(Handle, Agent) {
  let #(data, st) =
    rt_store.t_cell_new(
      st,
      SAsyncGen(state: AGSuspendedStart, resume:, queue: #([], [])),
    )
  let proto = generator_prototype(st, callee, fn(r) { r.async_gen.prototype })
  alloc_shell(st, AsyncGeneratorObj(data:), Some(proto))
}

/// §27.6.1.2 `%AsyncGeneratorPrototype%.next(value)`. Returns a promise handle.
pub fn t_asyncgen_next(
  st: Agent,
  this: JsVal,
  value: JsVal,
) -> #(Handle, Agent) {
  asyncgen_method(st, this, GenNext, value)
}

/// §27.6.1.3 `%AsyncGeneratorPrototype%.return(value)`.
pub fn t_asyncgen_return(
  st: Agent,
  this: JsVal,
  value: JsVal,
) -> #(Handle, Agent) {
  asyncgen_method(st, this, GenReturn, value)
}

/// §27.6.1.4 `%AsyncGeneratorPrototype%.throw(exception)`.
pub fn t_asyncgen_throw(
  st: Agent,
  this: JsVal,
  exception: JsVal,
) -> #(Handle, Agent) {
  asyncgen_method(st, this, GenThrow, exception)
}

/// Shared body for next/return/throw — port of arc `call_native_method`
/// (async_generators.gleam:65-121). §27.6.1.2-4: create a promise capability,
/// brand-check `this` (REJECT on failure — never throw sync), enqueue request,
/// drain if not already running.
fn asyncgen_method(
  st: Agent,
  this: JsVal,
  completion: GeneratorCompletion,
  value: JsVal,
) -> #(Handle, Agent) {
  // NewPromiseCapability(%Promise%): internal, so the promise is the target.
  let #(promise_h, st) = t_new_promise(st)
  case asyncgen_data_of(st, this) {
    Error(Nil) -> {
      // §27.6.1.2 step 4: brand check fails → reject the returned promise.
      let #(e, st) =
        require_js(st).ops.new_error(
          st,
          TypeErr,
          "AsyncGenerator method called on incompatible receiver",
        )
      #(promise_h, t_promise_reject(st, promise_h, e))
    }
    Ok(#(gen_h, ag)) -> {
      let promise = mk_object(promise_h)
      let req =
        AsyncGenRequest(completion:, value:, resolve: promise, reject: promise)
      let st = put_asyncgen(st, gen_h, ag_enqueue(ag, req))
      let st = case ag.state {
        AGExecuting | AGAwaitingReturn -> st
        _ -> drain_queue(st, gen_h)
      }
      #(promise_h, st)
    }
  }
}

/// Brand check: `this` is an `AsyncGeneratorObj`; yields its data handle and
/// the decoded data cell.
fn asyncgen_data_of(st: Agent, this: JsVal) -> Result(#(Handle, AGLive), Nil) {
  case classify(this) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: AsyncGeneratorObj(data:), ..) ->
          Ok(#(data, read_asyncgen(st, data)))
        _ -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

// ── driver loop (§27.6.3.5 AsyncGeneratorResumeNext) ────────────────────────

/// Pull the head request and act on it based on current state. Loops until
/// queue is empty or an await suspends via microtask. Port of arc
/// `resume_next` (async_generators.gleam:130-184). No user code runs between
/// the read and each arm's first write, so that write persists the decoded
/// `ag` (normalised queue included) rather than reading the cell again.
fn drain_queue(st: Agent, gen_h: Handle) -> Agent {
  let ag = ag_normalize(read_asyncgen(st, gen_h))
  case ag.front {
    [] -> st
    [req, ..] ->
      case ag.state {
        AGExecuting | AGAwaitingReturn -> st
        AGCompleted ->
          case req.completion {
            GenNext -> {
              let st = put_asyncgen(st, gen_h, ag_drop_head(ag))
              let st = fulfill_iter(st, req.resolve, mk_undefined(), True)
              drain_queue(st, gen_h)
            }
            GenThrow -> {
              let st = put_asyncgen(st, gen_h, ag_drop_head(ag))
              let st = settle(st, req.reject, Reject, req.value)
              drain_queue(st, gen_h)
            }
            GenReturn -> {
              // §27.6.3.5 step 5.b: Await(completion.[[Value]]) first.
              let st =
                put_asyncgen(st, gen_h, ag_set_state(ag, AGAwaitingReturn))
              setup_return_await(st, gen_h, req.value, AGResumeAwaitingReturn)
            }
          }
        AGSuspendedStart ->
          case req.completion {
            // §27.6.3.5 step 5.a: return/throw on a never-started gen →
            // Completed, then fall through on next loop.
            GenReturn | GenThrow -> {
              let st = put_asyncgen(st, gen_h, ag_set_state(ag, AGCompleted))
              drain_queue(st, gen_h)
            }
            GenNext -> run_asyncgen_body(st, gen_h, ag, req, sent_start())
          }
        AGSuspendedYield ->
          case req.completion {
            GenNext ->
              run_asyncgen_body(st, gen_h, ag, req, #(sent_next, req.value))
            GenThrow ->
              run_asyncgen_body(st, gen_h, ag, req, #(sent_throw, req.value))
            GenReturn -> {
              // §27.6.3.10 step 8: the DRIVER does Await(resumptionValue)
              // FIRST (arc `run_body` AGReturn :231-240; the body emits no
              // await for mode 2). State stays Executing; the
              // AGResumeReturnUnwind arm injects mode 2 with the AWAITED v.
              let st = put_asyncgen(st, gen_h, ag_set_state(ag, AGExecuting))
              setup_return_await(st, gen_h, req.value, AGResumeReturnUnwind)
            }
          }
      }
  }
}

/// Mark `Executing`, run one turn, dispatch outcome. Port of arc `run_body`
/// (async_generators.gleam:189-243) with the `yield*`-delegate branch dropped.
/// `ag` is the cell as `drain_queue` just read it.
fn run_asyncgen_body(
  st: Agent,
  gen_h: Handle,
  ag: AGLive,
  req: AsyncGenRequest,
  sent: #(Int, JsVal),
) -> Agent {
  // Mark executing FIRST so re-entrant next()/return()/throw() enqueue.
  let st = put_asyncgen(st, gen_h, ag_set_state(ag, AGExecuting))
  let #(step, st) = asyncgen_turn(st, ag.resume, sent)
  drive_asyncgen_step(st, gen_h, req, step)
}

/// Run one asyncgen body turn under the `call_depth` gate. The generator is
/// already `Executing` with `req` at the queue head, so at
/// `limits.max_call_depth` the turn is refused as a `StepThrow(RangeError)`:
/// the driver then completes the generator and rejects `req` rather than the
/// raise escaping the reaction job with the state stuck `Executing`.
fn asyncgen_turn(
  st: Agent,
  resume: Resume,
  sent: #(Int, JsVal),
) -> #(Step, Agent) {
  case st.call_depth >= limits.max_call_depth {
    True -> {
      let #(e, st) =
        require_js(st).ops.new_error(
          st,
          RangeErr,
          "Maximum call stack size exceeded",
        )
      #(StepThrow(e), st)
    }
    False -> {
      let st = rt_store.t_enter_call(st)
      let #(step, st) = apply_resume(st, resume, sent)
      #(step, rt_store.t_leave_call(st))
    }
  }
}

/// Outcome dispatch for one asyncgen body turn — port of arc
/// `handle_exec_result` (async_generators.gleam:546-579). User code ran in
/// the turn, so every write re-reads the live cell (`write_asyncgen`).
fn drive_asyncgen_step(
  st: Agent,
  gen_h: Handle,
  req: AsyncGenRequest,
  step: Step,
) -> Agent {
  case step {
    StepReturn(v) -> {
      let st = write_asyncgen(st, gen_h, ag_complete_drop_head)
      let st = fulfill_iter(st, req.resolve, v, True)
      drain_queue(st, gen_h)
    }
    StepThrow(e) -> {
      let st = write_asyncgen(st, gen_h, ag_complete_drop_head)
      let st = settle(st, req.reject, Reject, e)
      drain_queue(st, gen_h)
    }
    StepYield(value:, resume:) -> {
      // Store resume, dequeue + resolve request, loop.
      let st =
        write_asyncgen(st, gen_h, fn(ag) {
          AGLive(..ag, resume:, state: AGSuspendedYield) |> ag_drop_head
        })
      let st = fulfill_iter(st, req.resolve, value, False)
      drain_queue(st, gen_h)
    }
    StepAwait(value:, resume:) -> {
      // State stays Executing; do NOT dequeue — same request stays at head
      // until a yield/return/throw settles it. The reaction continues the
      // data cell itself (`resume_asyncgen`).
      let st = write_asyncgen(st, gen_h, fn(ag) { AGLive(..ag, resume:) })
      await_into(st, gen_h, value)
    }
  }
}

/// A body `await` settled (its reaction's target is the data cell): re-drive
/// the body with `sent` for the request still at the queue head.
fn resume_asyncgen(st: Agent, gen_h: Handle, sent: #(Int, JsVal)) -> Agent {
  let ag = ag_normalize(read_asyncgen(st, gen_h))
  case ag.front {
    [] -> st
    [req, ..] -> redrive_asyncgen(st, gen_h, ag, req, sent)
  }
}

/// Run one more body turn from `ag.resume` and dispatch its outcome.
fn redrive_asyncgen(
  st: Agent,
  gen_h: Handle,
  ag: AGLive,
  req: AsyncGenRequest,
  sent: #(Int, JsVal),
) -> Agent {
  let #(step, st) = asyncgen_turn(st, ag.resume, sent)
  drive_asyncgen_step(st, gen_h, req, step)
}

/// AsyncGeneratorAwaitReturn / body-await / return-unwind settlement. Called
/// from `dispatch_native` for `AsyncGenResume(gen_h, is_throw, kind)` with
/// `settled` = the awaited value/reason. Port of arc `call_native_resume`
/// (async_generators.gleam:584-663) with delegate arms dropped.
pub fn t_asyncgen_resume(
  st: Agent,
  gen_h: Handle,
  is_throw: Bool,
  kind: AGResumeKind,
  settled: JsVal,
) -> Agent {
  let ag = ag_normalize(read_asyncgen(st, gen_h))
  case ag.front {
    [] -> st
    [req, ..] ->
      case kind, is_throw {
        // Completed-gen `.return(v)` await settled: settle head, drain.
        AGResumeAwaitingReturn, _ -> {
          let st = put_asyncgen(st, gen_h, ag_complete_drop_head(ag))
          let st = case is_throw {
            False -> fulfill_iter(st, req.resolve, settled, True)
            True -> settle(st, req.reject, Reject, settled)
          }
          drain_queue(st, gen_h)
        }
        // Body await OR §27.6.3.10 return-unwind await: re-drive the body.
        // Only the fulfil-mode differs — return-unwind injects mode 2 with
        // the AWAITED value (arc AGResumeReturnUnwind :646-655).
        _, True -> redrive_asyncgen(st, gen_h, ag, req, #(sent_throw, settled))
        AGResumeBody, False ->
          redrive_asyncgen(st, gen_h, ag, req, #(sent_next, settled))
        AGResumeReturnUnwind, False ->
          redrive_asyncgen(st, gen_h, ag, req, #(sent_return, settled))
      }
  }
}

// ── await wiring (§27.7.5.3 Await, specialized to asyncgen) ─────────────────

/// The driver's own `Await(v)` for a `.return(v)` request (§27.6.3.9 on a
/// completed generator, §27.6.3.10 step 8 at a yield): PromiseResolve then
/// PerformPromiseThen with `AsyncGenResume(kind)` closures and no result
/// capability. Port of arc `promises.setup_await` (promises.gleam:1715-1766).
fn setup_return_await(
  st: Agent,
  gen_h: Handle,
  awaited: JsVal,
  kind: AGResumeKind,
) -> Agent {
  // Step 2: PromiseResolve(%Promise%, value).
  let #(promise_h, st) = promise_resolve_static(st, awaited)
  let #(on_fulfill, st) = alloc_asyncgen_resume(st, gen_h, False, kind)
  let #(on_reject, st) = alloc_asyncgen_resume(st, gen_h, True, kind)
  perform_then(
    st,
    promise_h,
    Handler(mk_object(on_fulfill)),
    Handler(mk_object(on_reject)),
    mk_undefined(),
    mk_undefined(),
  )
}

/// AsyncGeneratorCompleteStep: resolve the request's promise with a fresh
/// `{value, done}`. Port of arc `fulfill_iter` (async_generators.gleam:893).
fn fulfill_iter(st: Agent, resolve: JsVal, value: JsVal, done: Bool) -> Agent {
  let #(result_h, st) = alloc_iter_result(st, value, done)
  settle(st, resolve, Fulfil, mk_object(result_h))
}

// ── SAsyncGen slot helpers (port arc slot read/write helpers :681-886) ──────
// Decoded live view: mutable state/queue exposed only at the read/write seam,
// so a body-executing path never holds a stale queue snapshot.

type AGLive {
  AGLive(
    state: AsyncGenState,
    resume: Resume,
    front: List(AsyncGenRequest),
    back: List(AsyncGenRequest),
  )
}

fn read_asyncgen(st: Agent, gen_h: Handle) -> AGLive {
  case rt_store.t_cell_get(st, gen_h) {
    SAsyncGen(state:, resume:, queue: #(front, back)) ->
      AGLive(state:, resume:, front:, back:)
    _ -> panic as "rt_async: Handle is not an SAsyncGen cell (engine invariant)"
  }
}

fn encode_asyncgen(ag: AGLive) -> JsSlot {
  SAsyncGen(state: ag.state, resume: ag.resume, queue: #(ag.front, ag.back))
}

/// Re-read the LIVE slot at write time, apply a pure update, write it back.
/// Re-reading here is what stops a stale queue (captured before user code ran
/// that enqueued re-entrantly) from being written back over the live one.
/// Port of arc `write_live` (async_generators.gleam:807-816).
fn write_asyncgen(
  st: Agent,
  gen_h: Handle,
  update: fn(AGLive) -> AGLive,
) -> Agent {
  put_asyncgen(st, gen_h, update(read_asyncgen(st, gen_h)))
}

/// Write `ag` back as-is: only for a value read since user code last ran.
fn put_asyncgen(st: Agent, gen_h: Handle, ag: AGLive) -> Agent {
  rt_store.t_cell_set(st, gen_h, encode_asyncgen(ag))
}

// -- pure AGLive updaters, composed inside `write_asyncgen` callbacks --------

/// If front is empty, reverse back into front so the head match sees oldest.
fn ag_normalize(ag: AGLive) -> AGLive {
  case ag.front, ag.back {
    [], [_, ..] -> AGLive(..ag, front: list.reverse(ag.back), back: [])
    _, _ -> ag
  }
}

fn ag_enqueue(ag: AGLive, req: AsyncGenRequest) -> AGLive {
  AGLive(..ag, back: [req, ..ag.back])
}

fn ag_set_state(ag: AGLive, s: AsyncGenState) -> AGLive {
  AGLive(..ag, state: s)
}

fn ag_drop_head(ag: AGLive) -> AGLive {
  let ag = ag_normalize(ag)
  case ag.front {
    [_, ..rest] -> AGLive(..ag, front: rest)
    [] -> ag
  }
}

fn ag_complete_drop_head(ag: AGLive) -> AGLive {
  ag |> ag_drop_head |> ag_set_state(AGCompleted)
}

// ── §27.7.5 Async function driver: t_async_start / t_await ──────────────────
// Port of arc `exec/call.gleam:324-543 call_async_function` /
// `finish_async_execution` / `call_native_async_resume` +
// `exec/promises.gleam:1715-1766 setup_await`. The running body's context is
// an `SAsyncContext(resume, promise)` cell reachable only as the settle
// target of its current await's reaction.

/// First argument or `undefined` — the `arguments[0]` a resolving/resume
/// function's body reads (arc `helpers.first_arg_or_undefined`).
fn first_arg(args: List(JsVal)) -> JsVal {
  case args {
    [v, ..] -> v
    [] -> mk_undefined()
  }
}

/// §27.7.5.1 AsyncFunctionStart. Allocate the result promise + the
/// `SAsyncContext` cell, run the body's first turn, drive its outcome, and
/// return the result promise (R1 value-first). Port of arc
/// `call.gleam:324-366 call_async_function`. Frame/args accepted for
/// op-table 4-arg parity, unused (already packed into `loc0` by the outer
/// prologue).
pub fn t_async_start(
  st: Agent,
  sm: SmFn,
  _frame: Frame,
  _args: List(JsVal),
  loc0: Loc,
) -> #(Handle, Agent) {
  t_async_run(st, ResumeCompiled(sm:, rs: 0, loc: loc0))
}

/// §27.7.5.1 EvaluateAsyncFunctionBody / §15.9.3 EvaluateAsyncConciseBody
/// step 2-3: FunctionDeclarationInstantiation threw, so the call's result
/// promise is rejected with the thrown value instead of the throw escaping.
pub fn t_async_reject(st: Agent, reason: JsVal) -> #(Handle, Agent) {
  let #(promise_h, st) = t_new_promise(st)
  let st = t_promise_reject(st, promise_h, reason)
  #(promise_h, st)
}

/// AsyncFunctionStart for a body that begins at `resume`: allocate the
/// result promise, run the first turn now, drive its outcome, and return the
/// result promise. The `SAsyncContext` is only allocated once the body first
/// awaits.
pub fn t_async_run(st: Agent, resume: Resume) -> #(Handle, Agent) {
  let #(promise_h, st) = t_new_promise(st)
  let #(step, st) = apply_resume(st, resume, sent_start())
  #(promise_h, drive_async_step(st, None, promise_h, step))
}

/// Shared completion handling for one async-fn turn; `ctx` is the body's
/// context cell if an earlier await already made one. Port of arc
/// `call.gleam:398-465 finish_async_execution`. `StepYield` in a plain async
/// function is an engine bug.
fn drive_async_step(
  st: Agent,
  ctx: Option(Handle),
  promise_h: Handle,
  step: Step,
) -> Agent {
  case step {
    // §27.7.5.2 step 3.d: fulfil via Resolve so a thenable return is adopted.
    StepReturn(v) -> t_promise_resolve(st, promise_h, v)
    // §27.7.5.2 step 3.f: reject the result promise.
    StepThrow(e) -> t_promise_reject(st, promise_h, e)
    // Body hit `await` — store where to resume and hand off to `t_await`.
    StepAwait(value:, resume:) -> {
      let context = SAsyncContext(resume:, promise: promise_h)
      let #(ctx_h, st) = case ctx {
        Some(h) -> #(h, rt_store.t_cell_set(st, h, context))
        None -> rt_store.t_cell_new(st, context)
      }
      t_await(st, ctx_h, value)
    }
    StepYield(..) ->
      panic as "rt_async: plain async function body produced a yield step"
  }
}

/// §27.7.5.3 Await for the async function whose context cell is `ctx_h`:
/// its stored `Resume` continues with `{mode, settled}` once `awaited`
/// settles. Port of arc `promises.gleam:1715-1766 setup_await`.
pub fn t_await(st: Agent, ctx_h: Handle, awaited: JsVal) -> Agent {
  await_into(st, ctx_h, awaited)
}

// ── native-token dispatch bodies (called by dispatch_native) ────────────────

/// `PromiseResolveFn` body — §27.2.1.3.2 Promise Resolve Functions. Checks
/// and sets the shared `[[AlreadyResolved]]` box, then `t_promise_resolve`.
pub fn do_resolve_fn(
  st: Agent,
  promise_h: Handle,
  already_h: Handle,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case check_already_resolved(st, already_h) {
    #(True, st) -> #(mk_undefined(), st)
    #(False, st) -> #(
      mk_undefined(),
      t_promise_resolve(st, promise_h, first_arg(args)),
    )
  }
}

/// `PromiseRejectFn` body — §27.2.1.3.1 Promise Reject Functions.
pub fn do_reject_fn(
  st: Agent,
  promise_h: Handle,
  already_h: Handle,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case check_already_resolved(st, already_h) {
    #(True, st) -> #(mk_undefined(), st)
    #(False, st) -> #(
      mk_undefined(),
      t_promise_reject(st, promise_h, first_arg(args)),
    )
  }
}

/// §27.2.1.3.1/.2 steps 3-4: read `[[AlreadyResolved]]`; if true return
/// `#(True, st)` (caller no-ops); else set it true and return `#(False, st)`.
fn check_already_resolved(st: Agent, already_h: Handle) -> #(Bool, Agent) {
  case rt_store.t_cell_get(st, already_h) {
    SBox(value: v) ->
      case classify(v) {
        rt_types.KBool(True) -> #(True, st)
        _ -> #(
          False,
          rt_store.t_cell_set(st, already_h, SBox(value: mk_bool(True))),
        )
      }
    _ ->
      panic as "rt_async: [[AlreadyResolved]] handle is not SBox (engine invariant)"
  }
}

/// `AsyncResume` body — §27.7.5.3 Await onFulfilled/onRejected. Continues the
/// `Resume` stored on the context cell with `Sent = {mode, settled_value}` and
/// drives the resulting step. Port of arc `call.gleam:481-543
/// call_native_async_resume`.
pub fn do_async_resume(
  st: Agent,
  ctx_h: Handle,
  is_throw: Bool,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case rt_store.t_cell_get(st, ctx_h) {
    SAsyncContext(resume:, promise:) -> {
      let mode = case is_throw {
        False -> sent_next
        True -> sent_throw
      }
      let #(step, st) = apply_resume(st, resume, #(mode, first_arg(args)))
      #(mk_undefined(), drive_async_step(st, Some(ctx_h), promise, step))
    }
    _ ->
      panic as "rt_async: AsyncResume target is not SAsyncContext (engine invariant)"
  }
}
