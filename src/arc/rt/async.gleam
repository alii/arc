import arc/rt/call.{
  type Completion, type Frame, NormalCompletion, ThrowCompletion, is_callable,
  t_call,
}
import arc/rt/gc as rt_gc
import arc/rt/inspect
import arc/rt/limits
import arc/rt/name_keys as nk
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type AGResumeKind, type Agent, type AsyncGenRequest, type AsyncGenState,
  type AsyncWaiter, type GeneratorCompletion, type Handle, type Job, type JsSlot,
  type JsStore, type JsVal, type Loc, type NativeToken, type PromiseReaction,
  type PromiseState, type ReactionHandler, type Resume, type SabOwner, type SmFn,
  type Step, type WaiterRef, AGAwaitingReturn, AGCompleted, AGExecuting,
  AGResumeAwaitingReturn, AGResumeReturnUnwind, AGSuspendedStart,
  AGSuspendedYield, Agent, AsyncGenRequest, AsyncGenResume, AsyncGeneratorObj,
  AsyncWaiter, DataProperty, GenCompleted, GenExecuting, GenNext, GenReturn,
  GenSuspendedStart, GenSuspendedYield, GenThrow, GeneratorObj, Handler, HostJob,
  IdentityPassThrough, JsCell, JsStore, KHandle, NoElements, Ordinary,
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

// sent = #(mode, value): 0 next, 1 throw, 2 return
pub const sent_next = 0

pub const sent_throw = 1

pub const sent_return = 2

pub fn sent_start() -> #(Int, JsVal) {
  #(sent_next, mk_undefined())
}

@external(erlang, "arc_rt_async_ffi", "apply_sm")
pub fn apply_sm(
  st: Agent,
  sm: SmFn,
  rs: Int,
  sent: #(Int, JsVal),
  loc: Loc,
) -> #(Step, Agent)

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

fn alloc_asyncgen_resume(
  st: Agent,
  gen_h: Handle,
  is_throw: Bool,
  kind: AGResumeKind,
) -> #(Handle, Agent) {
  alloc_native_fn(st, AsyncGenResume(gen: gen_h, is_throw:, kind:), "", 1)
}

fn require_js(st: Agent) -> JsStore(Agent) {
  st.store
}

fn with_js(st: Agent, js: JsStore(Agent)) -> Agent {
  Agent(..st, store: js)
}

pub fn t_enqueue_job(st: Agent, job: Job) -> Agent {
  let js = require_js(st)
  with_js(st, JsStore(..js, microtasks: jq_push(js.microtasks, job)))
}

// the gc safepoint: collects only between jobs, never mid-expression
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

pub type WaitResult {
  Woken
  TimedOut
  NotEqual
}

pub fn wait_result_js(result: WaitResult) -> JsVal {
  mk_string(case result {
    Woken -> "ok"
    TimedOut -> "timed-out"
    NotEqual -> "not-equal"
  })
}

// erlang receive after rejects larger timeouts
const max_receive_ms = 0xFFFFFFFF

type Cancellation {
  Cancelled
  AlreadyWoken
}

@external(erlang, "arc_rt_sab_ffi", "cancel")
fn cancel_waiter(owner: SabOwner, ref: WaiterRef) -> Cancellation

@external(erlang, "arc_rt_sab_ffi", "take_wake")
fn take_wake(refs: List(WaiterRef), timeout_ms: Int) -> Option(WaiterRef)

@external(erlang, "arc_rt_sab_ffi", "await_wake")
fn consume_wake(ref: WaiterRef) -> Nil

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

fn service_waiters(st: Agent) -> Agent {
  fire_due_waiters(apply_pending_wakes(st), st.hooks.monotonic_now())
}

fn pending_refs(st: Agent) -> List(WaiterRef) {
  list.map(st.waiters, fn(w) { w.ref })
}

fn apply_pending_wakes(st: Agent) -> Agent {
  case take_wake(pending_refs(st), -1) {
    None -> st
    Some(ref) -> apply_pending_wakes(t_wake_waiter(st, ref))
  }
}

// waits in real time so a frozen host clock cannot stall the drain
fn idle_until(st: Agent, deadline: Int) -> Agent {
  let wait_ms = int.max(deadline - st.hooks.monotonic_now(), 0) + 1
  case take_wake(pending_refs(st), wait_ms) {
    Some(ref) -> t_wake_waiter(st, ref)
    None if wait_ms <= max_receive_ms -> fire_due_waiters(st, deadline)
    None -> st
  }
}

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

fn earliest_deadline(st: Agent) -> Option(Int) {
  list.fold(st.waiters, None, fn(acc, w) {
    case acc, w.deadline {
      None, d -> d
      Some(a), Some(d) -> Some(int.min(a, d))
      Some(a), None -> Some(a)
    }
  })
}

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

type Side {
  Fulfil
  Reject
}

// target: undefined, child promise, coroutine data cell, or user fn
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

fn sent_of(side: Side, value: JsVal) -> #(Int, JsVal) {
  case side {
    Fulfil -> #(sent_next, value)
    Reject -> #(sent_throw, value)
  }
}

fn resume_from_job(st: Agent, turn: fn(Agent) -> Agent) -> Agent {
  let st = rt_store.t_enter_call(st)
  let #(outcome, st) = protected(st, fn(st) { #(mk_undefined(), turn(st)) })
  report_job_throw(#(outcome, rt_store.t_leave_call(st)))
}

fn call_settle(st: Agent, target: JsVal, args: List(JsVal)) -> Agent {
  report_job_throw(t_call(st, target, mk_undefined(), args))
}

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

fn describe_thrown(st: Agent, thrown: JsVal) -> String {
  inspect.format_error(st, thrown)
}

fn execute_job(st: Agent, job: Job) -> Agent {
  case job {
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
    ResolveThenableJob(thenable:, then_fn:, resolve:, reject:) ->
      case t_call(st, then_fn, thenable, [resolve, reject]) {
        #(NormalCompletion(_), st) -> st
        #(ThrowCompletion(e), st) -> call_settle(st, reject, [e])
      }
    HostJob(run:) ->
      report_job_throw(protected(st, fn(st) { #(mk_undefined(), run(st)) }))
  }
}

fn throw_type_error(st: Agent, msg: String) -> a {
  let #(e, st) = require_js(st).ops.new_error(st, TypeErr, msg)
  rt_store.t_throw(st, e)
}

// §7.4.11 createiterresultobject
pub fn alloc_iter_result(
  st: Agent,
  value: JsVal,
  done: Bool,
) -> #(Handle, Agent) {
  let object_proto = st.realm.object.prototype
  use seq <- rt_store.t_cell_new_with(st, 2)
  SObject(
    kind: Ordinary,
    proto: Some(object_proto),
    props: dict.from_list([
      #(nk.value, DataProperty(value, True, True, True, seq)),
      #(nk.done, DataProperty(mk_bool(done), True, True, True, seq + 1)),
    ]),
    symbol_props: [],
    elements: NoElements,
    extensible: True,
  )
}

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

// §27.5.3.2 generatorvalidate
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

fn read_generator(st: Agent, gen_h: Handle) -> JsSlot {
  case rt_store.t_cell_get(st, gen_h) {
    SGenerator(..) as gen -> gen
    _ -> panic as "rt_async: Handle is not an SGenerator cell (engine invariant)"
  }
}

fn set_gen_state(
  st: Agent,
  gen_h: Handle,
  gen: JsSlot,
  new_state: rt_types.GeneratorState,
) -> Agent {
  let assert SGenerator(resume:, ..) = gen
  rt_store.t_cell_set(st, gen_h, SGenerator(state: new_state, resume:))
}

// §27.5.1.2 generatorstart
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

pub fn t_gen_new(st: Agent, callee: JsVal, resume: Resume) -> #(Handle, Agent) {
  let #(data, st) =
    rt_store.t_cell_new(st, SGenerator(state: GenSuspendedStart, resume:))
  let proto = generator_prototype(st, callee, fn(r) { r.generator.prototype })
  alloc_shell(st, GeneratorObj(data:), Some(proto))
}

// §10.1.14 getprototypefromconstructor
fn generator_prototype(
  st: Agent,
  callee: JsVal,
  intrinsic: fn(rt_types.Realm) -> Handle,
) -> Handle {
  case classify(callee) {
    KHandle(fn_h) ->
      case rt_obj.t_ordinary_own_property(st, fn_h, StringKey(nk.prototype)) {
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

// §27.5.3.3 generatorresume
pub fn t_gen_next(st: Agent, gen_h: Handle, sent: JsVal) -> #(Handle, Agent) {
  let #(#(done, v), st) = t_gen_step(st, gen_h, sent)
  alloc_iter_result(st, v, done)
}

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
    GenSuspendedStart | GenSuspendedYield ->
      gen_resume(st, gen_h, gen, resume, #(sent_next, sent))
  }
}

// §27.5.3.4 generatorresumeabrupt, return
pub fn t_gen_return(st: Agent, gen_h: Handle, v: JsVal) -> #(Handle, Agent) {
  let gen = read_generator(st, gen_h)
  let assert SGenerator(state:, resume:) = gen
  case state {
    GenExecuting -> throw_type_error(st, "Generator is already running")
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

// §27.5.3.4 generatorresumeabrupt, throw
pub fn t_gen_throw(st: Agent, gen_h: Handle, e: JsVal) -> #(Handle, Agent) {
  let gen = read_generator(st, gen_h)
  let assert SGenerator(state:, resume:) = gen
  case state {
    GenExecuting -> throw_type_error(st, "Generator is already running")
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
    StepAwait(..) ->
      panic as "rt_async: sync generator body produced an await step"
  }
}

@external(erlang, "arc_rt_call_ffi", "t_apply_protected")
fn protected(
  st: Agent,
  body: fn(Agent) -> #(JsVal, Agent),
) -> #(Completion, Agent)

// §27.2.1.6 ispromise
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
    _ -> panic as "rt_async: Handle is not a promise object (engine invariant)"
  }
}

pub fn t_new_promise_with_proto(
  st: Agent,
  proto: Option(Handle),
) -> #(Handle, Agent) {
  let #(data, st) =
    rt_store.t_cell_new(st, SPromiseData(PromisePending([]), False))
  alloc_shell(st, PromiseObj(data:), proto)
}

pub fn t_new_promise(st: Agent) -> #(Handle, Agent) {
  t_new_promise_with_proto(st, Some(st.realm.promise.prototype))
}

// §27.2.1.5 newpromisecapability
pub fn t_new_promise_capability(
  st: Agent,
) -> #(#(Handle, Handle, Handle), Agent) {
  let #(promise_h, st) = t_new_promise(st)
  let #(#(resolve_h, reject_h), st) = alloc_resolving_fns(st, promise_h)
  #(#(promise_h, resolve_h, reject_h), st)
}

// §27.2.1.4 fulfillpromise
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

// §27.2.1.7 rejectpromise
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

// §27.2.1.8 triggerpromisereactions
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

// §27.2.1.3.2 promise resolve functions steps 7-16
pub fn t_promise_resolve(
  st: Agent,
  promise_h: Handle,
  resolution: JsVal,
) -> Agent {
  case classify(resolution) {
    KHandle(h) if h == promise_h -> {
      let #(e, st) =
        require_js(st).ops.new_error(
          st,
          TypeErr,
          "Chaining cycle detected for promise",
        )
      t_promise_reject(st, promise_h, e)
    }
    KHandle(h) -> resolve_with_handle(st, promise_h, resolution, h)
    _ -> fulfill_promise(st, promise_h, resolution)
  }
}

fn resolve_with_handle(
  st: Agent,
  promise_h: Handle,
  resolution: JsVal,
  h: Handle,
) -> Agent {
  case rt_store.t_cell_get(st, h) {
    SObject(..) | rt_types.SShapedObject(..) -> {
      let #(outcome, st) =
        protected(st, fn(st) {
          rt_obj.t_get_prop(st, resolution, StringKey(nk.then))
        })
      case outcome {
        ThrowCompletion(e) -> t_promise_reject(st, promise_h, e)
        NormalCompletion(then_val) ->
          case is_callable(st, then_val) {
            False -> fulfill_promise(st, promise_h, resolution)
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

// §27.2.4.7.1 promiseresolve
pub fn promise_resolve_static(st: Agent, v: JsVal) -> #(Handle, Agent) {
  case as_promise(st, v) {
    Some(h) -> #(h, st)
    None -> {
      let #(h, st) = t_new_promise(st)
      #(h, t_promise_resolve(st, h, v))
    }
  }
}

// §27.2.5.4.1 performpromisethen
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

pub fn t_perform_then(
  st: Agent,
  promise_h: Handle,
  on_fulfilled: JsVal,
  on_rejected: JsVal,
  resolve: JsVal,
  reject: JsVal,
) -> Agent {
  let fulfill_handler = to_handler(st, on_fulfilled, IdentityPassThrough)
  let reject_handler = to_handler(st, on_rejected, ThrowerPassThrough)
  perform_then(st, promise_h, fulfill_handler, reject_handler, resolve, reject)
}

// §27.7.5.3 await; always enqueues
pub fn t_await(st: Agent, data_h: Handle, awaited: JsVal) -> Agent {
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

fn perform_then(
  st: Agent,
  promise_h: Handle,
  fulfill_handler: ReactionHandler,
  reject_handler: ReactionHandler,
  resolve: JsVal,
  reject: JsVal,
) -> Agent {
  case promise_data(st, promise_h) {
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
    #(data, PromiseFulfilled(value) as state, _) -> {
      let st = rt_store.t_cell_set(st, data, SPromiseData(state, True))
      t_enqueue_job(
        st,
        ReactionJob(handler: fulfill_handler, arg: value, resolve:, reject:),
      )
    }
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

// §27.6.3.1 asyncgeneratorstart
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

pub fn t_asyncgen_next(
  st: Agent,
  this: JsVal,
  value: JsVal,
) -> #(Handle, Agent) {
  asyncgen_method(st, this, GenNext, value)
}

pub fn t_asyncgen_return(
  st: Agent,
  this: JsVal,
  value: JsVal,
) -> #(Handle, Agent) {
  asyncgen_method(st, this, GenReturn, value)
}

pub fn t_asyncgen_throw(
  st: Agent,
  this: JsVal,
  exception: JsVal,
) -> #(Handle, Agent) {
  asyncgen_method(st, this, GenThrow, exception)
}

// brand check failure rejects the promise, never throws sync
fn asyncgen_method(
  st: Agent,
  this: JsVal,
  completion: GeneratorCompletion,
  value: JsVal,
) -> #(Handle, Agent) {
  let #(promise_h, st) = t_new_promise(st)
  case asyncgen_data_of(st, this) {
    Error(Nil) -> {
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

// §27.6.3.5 asyncgeneratorresumenext
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
              let st =
                put_asyncgen(st, gen_h, ag_set_state(ag, AGAwaitingReturn))
              setup_return_await(st, gen_h, req.value, AGResumeAwaitingReturn)
            }
          }
        AGSuspendedStart ->
          case req.completion {
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
              let st = put_asyncgen(st, gen_h, ag_set_state(ag, AGExecuting))
              setup_return_await(st, gen_h, req.value, AGResumeReturnUnwind)
            }
          }
      }
  }
}

// marks executing first so re-entrant calls enqueue
fn run_asyncgen_body(
  st: Agent,
  gen_h: Handle,
  ag: AGLive,
  req: AsyncGenRequest,
  sent: #(Int, JsVal),
) -> Agent {
  let st = put_asyncgen(st, gen_h, ag_set_state(ag, AGExecuting))
  let #(step, st) = asyncgen_turn(st, ag.resume, sent)
  drive_asyncgen_step(st, gen_h, req, step)
}

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
      let st =
        write_asyncgen(st, gen_h, fn(ag) {
          AGLive(..ag, resume:, state: AGSuspendedYield) |> ag_drop_head
        })
      let st = fulfill_iter(st, req.resolve, value, False)
      drain_queue(st, gen_h)
    }
    StepAwait(value:, resume:) -> {
      let st = write_asyncgen(st, gen_h, fn(ag) { AGLive(..ag, resume:) })
      t_await(st, gen_h, value)
    }
  }
}

fn resume_asyncgen(st: Agent, gen_h: Handle, sent: #(Int, JsVal)) -> Agent {
  let ag = ag_normalize(read_asyncgen(st, gen_h))
  case ag.front {
    [] -> st
    [req, ..] -> redrive_asyncgen(st, gen_h, ag, req, sent)
  }
}

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
        AGResumeAwaitingReturn, _ -> {
          let st = put_asyncgen(st, gen_h, ag_complete_drop_head(ag))
          let st = case is_throw {
            False -> fulfill_iter(st, req.resolve, settled, True)
            True -> settle(st, req.reject, Reject, settled)
          }
          drain_queue(st, gen_h)
        }
        AGResumeReturnUnwind, True ->
          redrive_asyncgen(st, gen_h, ag, req, #(sent_throw, settled))
        AGResumeReturnUnwind, False ->
          redrive_asyncgen(st, gen_h, ag, req, #(sent_return, settled))
      }
  }
}

fn setup_return_await(
  st: Agent,
  gen_h: Handle,
  awaited: JsVal,
  kind: AGResumeKind,
) -> Agent {
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

fn fulfill_iter(st: Agent, resolve: JsVal, value: JsVal, done: Bool) -> Agent {
  let #(result_h, st) = alloc_iter_result(st, value, done)
  settle(st, resolve, Fulfil, mk_object(result_h))
}

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

// re-reads the live cell so re-entrant enqueues are not lost
fn write_asyncgen(
  st: Agent,
  gen_h: Handle,
  update: fn(AGLive) -> AGLive,
) -> Agent {
  put_asyncgen(st, gen_h, update(read_asyncgen(st, gen_h)))
}

fn put_asyncgen(st: Agent, gen_h: Handle, ag: AGLive) -> Agent {
  rt_store.t_cell_set(st, gen_h, encode_asyncgen(ag))
}

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

fn first_arg(args: List(JsVal)) -> JsVal {
  case args {
    [v, ..] -> v
    [] -> mk_undefined()
  }
}

// §27.7.5.1 asyncfunctionstart
pub fn t_async_start(
  st: Agent,
  sm: SmFn,
  _frame: Frame,
  _args: List(JsVal),
  loc0: Loc,
) -> #(Handle, Agent) {
  t_async_run(st, ResumeCompiled(sm:, rs: 0, loc: loc0))
}

pub fn t_async_reject(st: Agent, reason: JsVal) -> #(Handle, Agent) {
  let #(promise_h, st) = t_new_promise(st)
  let st = t_promise_reject(st, promise_h, reason)
  #(promise_h, st)
}

pub fn t_async_run(st: Agent, resume: Resume) -> #(Handle, Agent) {
  let #(promise_h, st) = t_new_promise(st)
  let #(step, st) = apply_resume(st, resume, sent_start())
  #(promise_h, drive_async_step(st, None, promise_h, step))
}

fn drive_async_step(
  st: Agent,
  ctx: Option(Handle),
  promise_h: Handle,
  step: Step,
) -> Agent {
  case step {
    StepReturn(v) -> t_promise_resolve(st, promise_h, v)
    StepThrow(e) -> t_promise_reject(st, promise_h, e)
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

// §27.2.1.3.2 promise resolve functions
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

// §27.2.1.3.1 promise reject functions
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
