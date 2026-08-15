//// `rt_async` — Step protocol + state-machine resume driver (SPEC §7.M8).
////
//// The `Step` type + `apply_sm`/`drive_step` seam shared by every coroutine
//// kind (async fn / generator / async-generator). A compiled `<name>__sm`
//// function (M18) has BEAM signature `fun(St, Rs, Sent, Loc) -> {StepWire,
//// St'}` where `Rs` is the resume-state Int, `Sent` is `#(mode, value)`
//// injected by next/throw/return (SPEC:1795 — 0=normal, 1=throw, 2=return),
//// and `Loc` is the suspended-locals tuple. `apply_sm` invokes it and decodes
//// the wire step; `drive_step` dispatches to the caller-supplied continuation.
////
//// **(Rs, Loc) storage** (u-resume-ffi-pin resolution): `SGenerator` /
//// `SAsyncGen` carry only `resume: CompiledFn` — no explicit rs/loc fields
//// (rt_types is FROZEN). So the stored `resume` is a 3-tuple
//// `{SmFun, Rs, Loc}` (opaque as `CompiledFn`), built via `mk_resume`; each
//// yield/await re-pins with `repin_resume(old, ns, loc')`. GC-safe:
//// `refs_in_term` (rt_js_gc_ffi) already walks tuples + fun envs, so both
//// `SmFun`'s captures and every Handle inside `Loc` stay traced.
////
//// **Return-tuple order is `#(V, St')` — value FIRST (R1).**

import arc/rt/call.{
  type Completion, type Frame, NormalCompletion, ThrowCompletion, is_callable,
  t_call,
}
import arc/rt/gc as rt_gc
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type AGResumeKind, type Agent, type AsyncGenRequest, type AsyncGenState,
  type CompiledFn, type GeneratorCompletion, type Handle, type Job, type JsSlot,
  type JsStore, type JsVal, type NativeToken, type PromiseReaction,
  type ReactionHandler, AGAwaitingReturn, AGCompleted, AGExecuting,
  AGResumeAwaitingReturn, AGResumeBody, AGResumeReturnUnwind, AGSuspendedStart,
  AGSuspendedYield, Agent, AsyncGenRequest, AsyncGenResume, AsyncResume,
  DataProperty, ErrorObj, GenCompleted, GenExecuting, GenNext, GenReturn,
  GenSuspendedStart, GenSuspendedYield, GenThrow, Handler, IdentityPassThrough,
  JsCell, JsStore, KHandle, KNative, KStr, KSym, Named, NoElements, Ordinary,
  PromiseFulfilled, PromisePending, PromiseReaction, PromiseRejectFn,
  PromiseRejected, PromiseResolveFn, ReactionJob, ResolveThenableJob, SAsyncGen,
  SBox, SGenerator, SObject, SPromise, StringKey, ThrowerPassThrough, TypeErr,
  classify, jq_pop, jq_push, mk_bool, mk_object, mk_undefined,
} as rt_types
import arc/rt/val as rt_val
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// ── Step protocol (SPEC §7.M18 wire type) ───────────────────────────────────

/// The suspended-locals snapshot tuple. Opaque: built by the compiled sm
/// (`MakeTuple` of every hoisted local, SPEC §18.3), read only by the sm on
/// resume. Gleam never destructures it — it round-trips through
/// `mk_resume`/`apply_sm` and is traced by `refs_in_term`.
pub type Loc

/// One turn of a compiled state-machine. Wire form (returned by the sm's
/// `Return([…])`) is `{return,V} | {throw,V} | {yield,V,Ns,Loc'} |
/// {await,V,Ns,Loc'}` (SPEC:1509); `step_classify` (FFI) decodes to this sum.
/// NO `Delegate` variant — `yield*` is lowered as a self-looping arm inside
/// the sm (SPEC §18.6 / Q6).
pub type Step {
  StepReturn(JsVal)
  StepThrow(JsVal)
  StepYield(value: JsVal, next_state: Int, loc: Loc)
  StepAwait(value: JsVal, next_state: Int, loc: Loc)
}

/// `Sent` mode 0: normal resumption — `.next(v)` or await-fulfilled.
pub const sent_next = 0

/// `Sent` mode 1: throw injection — `.throw(e)` or await-rejected. The sm's
/// per-arm mode-dispatch (SPEC §18.4 step 2) routes to the enclosing try-
/// region's catch-state or, if none, returns `{throw, sent_v}`.
pub const sent_throw = 1

/// `Sent` mode 2: return injection — `.return(v)`. Routes to the enclosing
/// finally-state with `pending = {return, v}` (SPEC §18.5).
pub const sent_return = 2

/// Build the `Sent` pair for the initial invocation: `{0, undefined}` — state
/// 0 ignores it (SPEC §18 invariant).
pub fn sent_start() -> #(Int, JsVal) {
  #(sent_next, mk_undefined())
}

// ── FFI: sm invocation + resume pinning (arc_rt_async_ffi) ───────────

/// Invoke a compiled sm closure directly: `Sm(St, Rs, Sent, Loc)`, catch
/// `{wasm_exn, 0, [St', E]}` into `StepThrow(E)` (R2 payload order), and
/// classify the wire step. Captures are already curried into `sm` by
/// `MakeClosure` (SPEC §18.1 — closure arity 3 + St).
@external(erlang, "arc_rt_async_ffi", "apply_sm")
pub fn apply_sm(
  st: Agent,
  sm: CompiledFn,
  rs: Int,
  sent: #(Int, JsVal),
  loc: Loc,
) -> #(Step, Agent)

/// Build the opaque `resume` term stored on `SGenerator` / `SAsyncGen`:
/// `{Sm, Rs, Loc}`. Sits in the `CompiledFn`-typed field so rt_types stays
/// unchanged; only ever unpacked by `apply_resume` / `repin_resume`.
@external(erlang, "arc_rt_async_ffi", "mk_resume")
pub fn mk_resume(sm: CompiledFn, rs: Int, loc: Loc) -> CompiledFn

/// Re-pin a stored `resume` at a new `(Rs, Loc)` after a yield/await. Extracts
/// the base sm from the old term so callers never carry `sm` separately.
@external(erlang, "arc_rt_async_ffi", "repin_resume")
pub fn repin_resume(resume: CompiledFn, rs: Int, loc: Loc) -> CompiledFn

/// Invoke a stored `resume` term with a fresh `Sent`. Unpacks `{Sm, Rs, Loc}`
/// and delegates to `apply_sm`.
@external(erlang, "arc_rt_async_ffi", "apply_resume")
pub fn apply_resume(
  st: Agent,
  resume: CompiledFn,
  sent: #(Int, JsVal),
) -> #(Step, Agent)

/// The initial empty locals tuple (`{}`). The outer function's prologue
/// normally builds `initial_locals_tuple` from `_args` (SPEC §18.1) and passes
/// it to `<kind>_start`; this is the zero-arity fallback for a body with no
/// hoisted locals.
@external(erlang, "arc_rt_async_ffi", "loc_empty")
pub fn loc_empty() -> Loc

// ── drive_step: generic step dispatcher ─────────────────────────────────────

/// Per-driver continuation table. Each of `t_async_start` / `t_gen_next` /
/// `t_asyncgen_*` builds one and hands it to `drive_step` — the ONE place a
/// `Step` is cased on. Generic in `r` because the three drivers settle to
/// different shapes (bare `Agent` for async/asyncgen; `#(Handle, St)`
/// for a sync generator's iter-result).
pub type StepCtx(r) {
  StepCtx(
    on_return: fn(Agent, JsVal) -> r,
    on_throw: fn(Agent, JsVal) -> r,
    on_yield: fn(Agent, JsVal, Int, Loc) -> r,
    on_await: fn(Agent, JsVal, Int, Loc) -> r,
  )
}

/// Dispatch a `Step` to `ctx`'s matching continuation. Total; every arm
/// forwards the threaded `st` (R1).
pub fn drive_step(st: Agent, ctx: StepCtx(r), step: Step) -> r {
  case step {
    StepReturn(v) -> ctx.on_return(st, v)
    StepThrow(e) -> ctx.on_throw(st, e)
    StepYield(v, ns, loc) -> ctx.on_yield(st, v, ns, loc)
    StepAwait(v, ns, loc) -> ctx.on_await(st, v, ns, loc)
  }
}

/// Run one sm turn from a stored `resume` and dispatch the outcome. The
/// composed `apply_resume → drive_step` used by every `.next` / `.throw` /
/// `.return` / await-resume path.
pub fn resume_and_drive(
  st: Agent,
  resume: CompiledFn,
  sent: #(Int, JsVal),
  ctx: StepCtx(r),
) -> r {
  let #(step, st) = apply_resume(st, resume, sent)
  drive_step(st, ctx, step)
}

/// A `StepCtx` continuation for the arm a driver cannot legitimately reach —
/// e.g. `on_await` in a sync generator, `on_yield` in a plain async function.
/// Engine-bug panic (E3 posture; matches `rt_store.require_js`): the M18
/// lowerer never emits the wrong step kind for a given fn flavour.
pub fn step_unreachable(_st: Agent, _v: JsVal, _ns: Int, _loc: Loc) -> r {
  panic as "rt_async: unreachable Step variant for this coroutine kind"
}

// ── native-closure allocation (u-native-closure-encoding, arc port) ─────────
// CHOSEN encoding: data-carrying `NativeToken` variants (M6.md §2/§7 + arc
// `value.gleam:3020-3055`). `KNative` stays payload-free; the closed-over
// `Handle`s ride on the tag and reach `dispatch_native` directly. GC-trace
// hook is `rt_types.native_token_refs` — NOT YET WIRED at rt_gc:175.

/// Allocate a `KNative` function object: `SObject{kind: KNative(tag,…),
/// proto: %Function.prototype%}`. Port of arc `common.alloc_call_fn`.
fn alloc_native_fn(
  st: Agent,
  tag: NativeToken,
  name: String,
  length: Int,
) -> #(Handle, Agent) {
  rt_store.t_cell_new(
    st,
    SObject(
      kind: KNative(tag:, name:, length:, constructible: False),
      proto: Some(st.realm.function.prototype),
      props: dict.new(),
      symbol_props: [],
      elements: NoElements,
      extensible: True,
    ),
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

/// §27.7.5.3 Await steps 3-5: allocate the on-fulfilled/on-rejected `KNative`
/// resume closure for a plain async function. `(Rs, Loc)` are already pinned
/// on `gen_h`'s `SGenerator.resume` via `repin_resume` — the closure carries
/// only `(gen_h, is_throw)`, matching arc `AsyncResume(data_ref, is_reject)`.
pub fn alloc_resume(
  st: Agent,
  gen_h: Handle,
  is_throw: Bool,
) -> #(Handle, Agent) {
  alloc_native_fn(st, AsyncResume(gen: gen_h, is_throw:), "", 1)
}

/// Async-generator counterpart of `alloc_resume` — dispatches to the
/// request-queue driver instead of settling a result promise directly.
pub fn alloc_asyncgen_resume(
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
/// checkpoint"). Port of arc `event_loop.do_drain_jobs` reduced to the
/// SPEC §7.M8 shape — no atomics-waiter / embedder-yield handling here.
/// Between-jobs `t_maybe_collect` is THE D11 GC safepoint: `call_depth`
/// is zero at this point, so a collection can only fire between jobs.
/// Called by M19 `js_main` after top-level eval and by the host after
/// each callback; NEVER mid-expression.
pub fn t_drain_microtasks(st: Agent) -> Agent {
  let js = require_js(st)
  case jq_pop(js.microtasks) {
    None -> st
    Some(#(job, rest)) -> {
      let st = with_js(st, JsStore(..js, microtasks: rest))
      let st = execute_job(st, job)
      let st = rt_gc.t_maybe_collect(st)
      t_drain_microtasks(st)
    }
  }
}

/// Fire-and-forget invoke of a promise-capability resolve/reject fn during
/// job execution: the return value is discarded (a job has no continuation
/// to hand it to) and an abrupt completion is reported rather than
/// propagated (a job has no caller to propagate to). Port of arc
/// `job_call.call_settlement_fn`.
fn call_settle(st: Agent, target: JsVal, args: List(JsVal)) -> Agent {
  case t_call(st, target, mk_undefined(), args) {
    #(NormalCompletion(_), st) -> st
    // `target` is typically a promise-capability resolve/reject function. The
    // native ones never throw, but `Promise.prototype.then` builds the child
    // capability with NewPromiseCapability(SpeciesConstructor(this)), so a
    // user species constructor hands us arbitrary user callables here. There
    // is no promise to blame the throw on (it happened AFTER the reaction
    // settled), so `unhandled_rejections` cannot carry it; report it through
    // the host sink instead of letting it vanish.
    #(ThrowCompletion(thrown), st) -> {
      require_js(st).host_hooks.print(
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
  case classify(thrown) {
    KStr(s) -> s
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: ErrorObj(stack:), ..) if stack != "" -> stack
        _ -> "[object Object]"
      }
    KSym(id) -> rt_types.symbol_descriptive_string(id)
    _ ->
      case rt_val.prim_to_string(thrown) {
        Ok(s) -> s
        Error(e) -> string.inspect(e)
      }
  }
}

/// Run one microtask job. Port of arc `event_loop.execute_job` +
/// `execute_reaction_job` + `execute_thenable_job`.
fn execute_job(st: Agent, job: Job) -> Agent {
  case job {
    // §27.2.2.1 NewPromiseReactionJob.
    ReactionJob(handler:, arg:, resolve:, reject:) ->
      case handler {
        IdentityPassThrough -> call_settle(st, resolve, [arg])
        ThrowerPassThrough -> call_settle(st, reject, [arg])
        Handler(fun) ->
          case t_call(st, fun, mk_undefined(), [arg]) {
            #(NormalCompletion(v), st) -> call_settle(st, resolve, [v])
            #(ThrowCompletion(e), st) -> call_settle(st, reject, [e])
          }
      }
    // §27.2.2.2 NewPromiseResolveThenableJob.
    ResolveThenableJob(thenable:, then_fn:, resolve:, reject:) ->
      case t_call(st, then_fn, thenable, [resolve, reject]) {
        #(NormalCompletion(_), st) -> st
        #(ThrowCompletion(e), st) -> call_settle(st, reject, [e])
      }
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

// ── sync generator driver (SPEC §7.M8; port arc generators.gleam:52-331) ────
// The SM model collapses arc's `run_to_completion` / `unwind_return` / catch-
// unwinding into ONE resume: `apply_resume` re-enters the compiled sm with a
// `Sent = {mode, value}` and the sm's own per-arm mode-dispatch (SPEC §18.4
// step 2) routes to the enclosing catch/finally state, so this driver never
// walks a try-stack. `yield*` is likewise inside-SM (§18.6) — no delegate arm.

/// Read the `SGenerator` at `gen_h`, or raise TypeError. Port of arc
/// `get_generator_data` (D7 — arc's `None` becomes a raise here).
fn require_generator(st: Agent, gen_h: Handle) -> JsSlot {
  case rt_store.t_cell_get(st, gen_h) {
    SGenerator(..) as gen -> gen
    _ -> throw_type_error(st, "not a generator object")
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
  let assert SGenerator(resume:, gen_cell:, ..) = gen
  rt_store.t_cell_set(
    st,
    gen_h,
    SGenerator(state: new_state, resume:, gen_cell:),
  )
}

/// §27.5.1.2 GeneratorStart — allocate the generator object for a call to a
/// generator function. Captures are already curried into `sm` by
/// `MakeClosure` and args/frame are already packed into `loc0` by the outer
/// prologue (SPEC §18.1), so `_frame`/`_args` are accepted for signature
/// parity with `t_async_start` and the §8 op-table's 4-arg `gen_start` row
/// but never read. Returns the `SGenerator` cell handle (SPEC:1494 —
/// "h = generator cell").
pub fn t_gen_start(
  st: Agent,
  sm: CompiledFn,
  _frame: Frame,
  _args: List(JsVal),
  loc0: Loc,
) -> #(Handle, Agent) {
  // Shell: the JS-visible object whose proto chain reaches
  // `%GeneratorPrototype%`'s `next`/`return`/`throw` (M6 native delegates).
  let #(shell_h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: Ordinary,
        proto: Some(st.realm.generator.prototype),
        props: dict.new(),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  // Data: the internal state cell. `resume` pins {sm, rs=0, loc0}.
  rt_store.t_cell_new(
    st,
    SGenerator(
      state: GenSuspendedStart,
      resume: mk_resume(sm, 0, loc0),
      gen_cell: shell_h,
    ),
  )
}

/// §27.5.3.3 GeneratorResume — `Generator.prototype.next(value)`. Returns a
/// fresh iter-result `{value, done}` handle. Port of arc
/// `resume_generator_next` + `alloc_iter_result` (arc:52-113).
pub fn t_gen_next(st: Agent, gen_h: Handle, sent: JsVal) -> #(Handle, Agent) {
  let gen = require_generator(st, gen_h)
  let assert SGenerator(state:, resume:, ..) = gen
  case state {
    GenCompleted -> alloc_iter_result(st, mk_undefined(), True)
    GenExecuting -> throw_type_error(st, "Generator is already running")
    // SuspendedStart: state 0 ignores `sent` (SPEC §18 invariant) but is
    // otherwise a normal resume — arc's AtStart branch differs only in NOT
    // pushing sent onto the saved stack, which the sm handles itself.
    GenSuspendedStart | GenSuspendedYield ->
      gen_resume(st, gen_h, gen, resume, #(sent_next, sent))
  }
}

/// §27.5.3.4 GeneratorResumeAbrupt with a return completion —
/// `Generator.prototype.return(value)`. Port of arc
/// `call_native_generator_return` (arc:116-199) with `unwind_return` folded
/// into the sm's mode-2 dispatch (SPEC §18.5 — finally states, `yield*`
/// forwarding all inside-SM).
pub fn t_gen_return(st: Agent, gen_h: Handle, v: JsVal) -> #(Handle, Agent) {
  let gen = require_generator(st, gen_h)
  let assert SGenerator(state:, resume:, ..) = gen
  case state {
    GenExecuting -> throw_type_error(st, "Generator is already running")
    // §27.5.3.4 step 5: SuspendedStart → Completed, no body run. Also covers
    // an already-Completed generator (step 8.a with return completion).
    GenCompleted | GenSuspendedStart -> {
      let st = set_gen_state(st, gen_h, gen, GenCompleted)
      alloc_iter_result(st, v, True)
    }
    GenSuspendedYield -> gen_resume(st, gen_h, gen, resume, #(sent_return, v))
  }
}

/// §27.5.3.4 GeneratorResumeAbrupt with a throw completion —
/// `Generator.prototype.throw(exception)`. Port of arc
/// `call_native_generator_throw` (arc:202-283) with `unwind_to_catch` folded
/// into the sm's mode-1 dispatch (SPEC §18.4 step 2).
pub fn t_gen_throw(st: Agent, gen_h: Handle, e: JsVal) -> #(Handle, Agent) {
  let gen = require_generator(st, gen_h)
  let assert SGenerator(state:, resume:, ..) = gen
  case state {
    GenExecuting -> throw_type_error(st, "Generator is already running")
    // §27.5.3.4 step 5 + step 8.b: SuspendedStart / Completed → mark
    // Completed and propagate the throw (arc `complete_and_throw`).
    GenCompleted | GenSuspendedStart -> {
      let st = set_gen_state(st, gen_h, gen, GenCompleted)
      rt_store.t_throw(st, e)
    }
    GenSuspendedYield -> gen_resume(st, gen_h, gen, resume, #(sent_throw, e))
  }
}

/// Resume a suspended generator with `sent` and marshal the sm's `Step` back
/// into the sync-driver convention. Port of arc `build_resumed_state` +
/// `run_to_completion` + `settle_completion` (arc:388-610). Bracketed with
/// `t_enter_call`/`t_leave_call` — arc bumps `call_depth` for the exact same
/// D11 reason (arc:382).
fn gen_resume(
  st: Agent,
  gen_h: Handle,
  gen: JsSlot,
  resume: CompiledFn,
  sent: #(Int, JsVal),
) -> #(Handle, Agent) {
  let st = set_gen_state(st, gen_h, gen, GenExecuting)
  let st = rt_store.t_enter_call(st)
  let #(step, st) = apply_resume(st, resume, sent)
  let st = rt_store.t_leave_call(st)
  // Re-read: `apply_resume` runs user JS which may observe/mutate `gen_h`
  // (e.g. a nested `.return()` on itself would hit the Executing guard, but
  // the cell identity is the invariant, not the pre-resume snapshot).
  let gen = require_generator(st, gen_h)
  drive_step(
    st,
    StepCtx(
      on_return: fn(st, v) {
        let st = set_gen_state(st, gen_h, gen, GenCompleted)
        alloc_iter_result(st, v, True)
      },
      on_throw: fn(st, e) {
        let st = set_gen_state(st, gen_h, gen, GenCompleted)
        rt_store.t_throw(st, e)
      },
      on_yield: fn(st, v, ns, loc) {
        let assert SGenerator(gen_cell:, ..) = gen
        let st =
          rt_store.t_cell_set(
            st,
            gen_h,
            SGenerator(
              state: GenSuspendedYield,
              resume: repin_resume(resume, ns, loc),
              gen_cell:,
            ),
          )
        alloc_iter_result(st, v, False)
      },
      on_await: step_unreachable,
    ),
    step,
  )
}

// ── Promise core (SPEC §7.M8; port arc builtins/promise.gleam:89-718 +
//    exec/promises.gleam:483-563) ──────────────────────────────────────────────
// SPromise-vs-SObject (u-promise-core resolution): 2core's `SPromise` is a
// `JsSlot` PEER of `SObject` (rt_types:680) with NO proto/props, and
// `rt_obj.t_get_prop` panics on non-`SObject` cells. Single-cell model:
// `promise_h` IS the `SPromise` cell. The runtime `t_promise_*` fns operate
// on it directly (emitted `await` / M6 `Promise.prototype.then` call these);
// JS-visible `.then` prototype-lookup glue is M6's concern.

/// Run a threaded thunk under the same `{wasm_exn,0,[St,V]}` try/catch as
/// `t_call_protected` — used to catch a throwing `.then` accessor during
/// thenable resolution (§27.2.1.3.2 step 10). Bound to the call-FFI
/// `t_apply_protected/2` so no new Erlang is written.
@external(erlang, "arc_rt_call_ffi", "t_apply_protected")
fn protected(
  st: Agent,
  body: fn(Agent) -> #(JsVal, Agent),
) -> #(Completion, Agent)

/// Allocate a fresh pending promise cell (§27.2.3.1 steps 3-7 internal-slot
/// init). Returns the `SPromise` handle. Port of arc `create_promise`
/// (builtins/promise.gleam:89-116) collapsed to the single-cell model.
pub fn t_new_promise(st: Agent) -> #(Handle, Agent) {
  rt_store.t_cell_new(st, SPromise(PromisePending([]), False))
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
  case rt_store.t_cell_get(st, promise_h) {
    SPromise(state: PromisePending(reactions), is_handled:) -> {
      let st =
        rt_store.t_cell_set(
          st,
          promise_h,
          SPromise(PromiseFulfilled(value), is_handled),
        )
      enqueue_reactions(st, reactions, value, on_fulfill_handler)
    }
    // Already settled (soft assert) or not a promise → no-op.
    _ -> st
  }
}

/// §27.2.1.7 RejectPromise(promise, reason). Step 7: on `is_handled == False`,
/// prepend the cell id to `unhandled_rejections` (HostPromiseRejectionTracker
/// "reject" op). Port of arc `reject_promise` (builtins/promise.gleam:386-426).
pub fn t_promise_reject(st: Agent, promise_h: Handle, reason: JsVal) -> Agent {
  case rt_store.t_cell_get(st, promise_h) {
    SPromise(state: PromisePending(reactions), is_handled:) -> {
      let st =
        rt_store.t_cell_set(
          st,
          promise_h,
          SPromise(PromiseRejected(reason), is_handled),
        )
      let st = case is_handled {
        False -> {
          let js = require_js(st)
          let JsCell(id) = promise_h
          with_js(
            st,
            JsStore(..js, unhandled_rejections: [id, ..js.unhandled_rejections]),
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
/// (2core stores both handlers per reaction; arc uses two lists).
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
/// body minus the `[[AlreadyResolved]]` gate (M6's `PromiseResolveFn`
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

/// §27.2.1.3.2 steps 9-16 for a `KHandle` resolution. `SObject` runs the
/// thenable protocol; `SPromise` (proto-less single-cell) adopts directly via
/// `t_promise_then` — arc parity, where a native promise IS a thenable. Every
/// other `JsSlot` is an internal cell with no `.then` — fulfill as-is.
fn resolve_with_handle(
  st: Agent,
  promise_h: Handle,
  resolution: JsVal,
  h: Handle,
) -> Agent {
  case rt_store.t_cell_get(st, h) {
    // Native promise: adopt its state by attaching `promise_h`'s resolving fns
    // as reactions (semantically the ResolveThenableJob outcome for a native
    // promise; child capability from `t_promise_then` is discarded).
    SPromise(..) -> {
      let #(#(resolve_h, reject_h), st) = alloc_resolving_fns(st, promise_h)
      let #(_, st) =
        t_promise_then(st, h, mk_object(resolve_h), mk_object(reject_h))
      st
    }
    SObject(..) -> {
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
    // `SBox`/`SGenerator`/…: internal cell — no `.then`, fulfill with value.
    _ -> fulfill_promise(st, promise_h, resolution)
  }
}

/// §27.2.4.7.1 PromiseResolve(%Promise%, x). If `v` is already an `SPromise`
/// cell, return its handle unchanged (step 2.b — SameValue constructor check
/// collapses to an IsPromise check under the single-realm intrinsic %Promise%
/// model); else allocate a fresh pending promise and `t_promise_resolve` it
/// with `v`. Port of arc `promise_resolve` (builtins/promise.gleam:720-744).
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
/// resultCapability) with a fresh %Promise% capability (steps 1-14). Returns
/// the child promise handle. Port of arc `perform_promise_then`
/// (builtins/promise.gleam:465-568).
pub fn t_promise_then(
  st: Agent,
  promise_h: Handle,
  on_fulfilled: JsVal,
  on_rejected: JsVal,
) -> #(Handle, Agent) {
  // resultCapability = NewPromiseCapability(%Promise%).
  let #(#(child_h, resolve_h, reject_h), st) = t_new_promise_capability(st)
  let child_resolve = mk_object(resolve_h)
  let child_reject = mk_object(reject_h)
  // Steps 3-6: non-callable → the spec's "empty" handler.
  let fulfill_handler = to_handler(st, on_fulfilled, IdentityPassThrough)
  let reject_handler = to_handler(st, on_rejected, ThrowerPassThrough)
  let st = case rt_store.t_cell_get(st, promise_h) {
    // Step 9: pending → append reaction; step 12: [[PromiseIsHandled]] = true.
    // Stored newest-first (O(1) prepend), reversed once at settle time.
    SPromise(state: PromisePending(reactions), ..) ->
      rt_store.t_cell_set(
        st,
        promise_h,
        SPromise(
          PromisePending([
            PromiseReaction(
              on_fulfill: fulfill_handler,
              on_reject: reject_handler,
              child_resolve:,
              child_reject:,
            ),
            ..reactions
          ]),
          True,
        ),
      )
    // Step 10: fulfilled → mark handled + enqueue fulfill reaction job.
    SPromise(state: PromiseFulfilled(value), ..) -> {
      let st = mark_handled(st, promise_h)
      t_enqueue_job(
        st,
        ReactionJob(
          handler: fulfill_handler,
          arg: value,
          resolve: child_resolve,
          reject: child_reject,
        ),
      )
    }
    // Step 11: rejected → mark handled + untrack rejection + enqueue.
    SPromise(state: PromiseRejected(reason), is_handled:) -> {
      let st = mark_handled(st, promise_h)
      let st = case is_handled {
        False -> untrack_rejection(st, promise_h)
        True -> st
      }
      t_enqueue_job(
        st,
        ReactionJob(
          handler: reject_handler,
          arg: reason,
          resolve: child_resolve,
          reject: child_reject,
        ),
      )
    }
    // Not an `SPromise` — VM invariant. Every caller reaches here via a handle
    // it minted with `t_new_promise`; a non-promise slot is heap corruption,
    // and a silent no-op would hang the child forever (arc:565 posture).
    _ -> panic as "t_promise_then: Handle is not an SPromise cell"
  }
  #(child_h, st)
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

/// Set `[[PromiseIsHandled]] = true` (§27.2.5.4.1 step 12). No-op on a
/// non-`SPromise` cell. Port of arc `mark_handled`
/// (builtins/promise.gleam:712-718).
fn mark_handled(st: Agent, promise_h: Handle) -> Agent {
  rt_store.t_cell_update(st, promise_h, fn(slot) {
    case slot {
      SPromise(state:, ..) -> SPromise(state:, is_handled: True)
      other -> other
    }
  })
}

/// HostPromiseRejectionTracker(promise, "handle") — drop `promise_h`'s id from
/// `unhandled_rejections` (§27.2.5.4.1 step 11c).
fn untrack_rejection(st: Agent, promise_h: Handle) -> Agent {
  let js = require_js(st)
  let JsCell(id) = promise_h
  with_js(
    st,
    JsStore(
      ..js,
      unhandled_rejections: list.filter(js.unhandled_rejections, fn(r) {
        r != id
      }),
    ),
  )
}

// ════════════════════════════════════════════════════════════════════════════
// Async-generator driver — ES2024 §27.6 (port arc/vm/exec/async_generators.gleam)
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
// Cell layout (matches `t_gen_start`): shell `SObject` (proto
// `%AsyncGeneratorPrototype%`) + `SAsyncGen` data cell; `gen_cell` = shell
// handle; DATA handle returned. Every `t_asyncgen_*` op reads it as
// `SAsyncGen`. `yield*` delegation is inside-SM (SPEC §18.6) — no delegate
// arms here.

/// §27.6.3.1 AsyncGeneratorStart. Alloc the `SObject` shell + `SAsyncGen`
/// data cell in `SuspendedStart`; `resume = {sm, 0, loc0}`. Frame/args
/// accepted for §8 op-table 4-arg parity, unused (already packed into loc0).
pub fn t_asyncgen_start(
  st: Agent,
  sm: CompiledFn,
  _frame: Frame,
  _args: List(JsVal),
  loc0: Loc,
) -> #(Handle, Agent) {
  let #(shell_h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: Ordinary,
        proto: Some(st.realm.async_gen.prototype),
        props: dict.new(),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  rt_store.t_cell_new(
    st,
    SAsyncGen(
      state: AGSuspendedStart,
      resume: mk_resume(sm, 0, loc0),
      queue: #([], []),
      gen_cell: shell_h,
    ),
  )
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
  let #(#(promise_h, resolve_h, reject_h), st) = t_new_promise_capability(st)
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
    Ok(#(gen_h, gen_state)) -> {
      let req =
        AsyncGenRequest(
          completion:,
          value:,
          resolve: mk_object(resolve_h),
          reject: mk_object(reject_h),
        )
      let st = write_asyncgen(st, gen_h, ag_enqueue(_, req))
      let st = case gen_state {
        AGExecuting | AGAwaitingReturn -> st
        _ -> drain_queue(st, gen_h)
      }
      #(promise_h, st)
    }
  }
}

/// Brand check: `this` classifies to a `Handle` whose cell is `SAsyncGen`.
fn asyncgen_data_of(
  st: Agent,
  this: JsVal,
) -> Result(#(Handle, AsyncGenState), Nil) {
  case classify(this) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SAsyncGen(state:, ..) -> Ok(#(h, state))
        _ -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

// ── driver loop (§27.6.3.5 AsyncGeneratorResumeNext) ────────────────────────

/// Pull the head request and act on it based on current state. Loops until
/// queue is empty or an await suspends via microtask. Port of arc
/// `resume_next` (async_generators.gleam:130-184).
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
              let st = write_asyncgen(st, gen_h, ag_drop_head)
              let st = fulfill_iter(st, req.resolve, mk_undefined(), True)
              drain_queue(st, gen_h)
            }
            GenThrow -> {
              let st = write_asyncgen(st, gen_h, ag_drop_head)
              let st = call_settle(st, req.reject, [req.value])
              drain_queue(st, gen_h)
            }
            GenReturn -> {
              // §27.6.3.5 step 5.b: Await(completion.[[Value]]) first.
              let st =
                write_asyncgen(st, gen_h, ag_set_state(_, AGAwaitingReturn))
              setup_asyncgen_await(st, gen_h, req.value, AGResumeAwaitingReturn)
            }
          }
        AGSuspendedStart ->
          case req.completion {
            // §27.6.3.5 step 5.a: return/throw on a never-started gen →
            // Completed, then fall through on next loop.
            GenReturn | GenThrow -> {
              let st = write_asyncgen(st, gen_h, ag_set_state(_, AGCompleted))
              drain_queue(st, gen_h)
            }
            GenNext -> run_asyncgen_body(st, gen_h, req, sent_start())
          }
        AGSuspendedYield ->
          case req.completion {
            GenNext ->
              run_asyncgen_body(st, gen_h, req, #(sent_next, req.value))
            GenThrow ->
              run_asyncgen_body(st, gen_h, req, #(sent_throw, req.value))
            GenReturn -> {
              // §27.6.3.10 step 8: the DRIVER does Await(resumptionValue)
              // FIRST (arc `run_body` AGReturn :231-240 / SPEC §18.4 step 2
              // emits no await for mode 2). State stays Executing; the
              // AGResumeReturnUnwind arm injects mode 2 with the AWAITED v.
              let st = write_asyncgen(st, gen_h, ag_set_state(_, AGExecuting))
              setup_asyncgen_await(st, gen_h, req.value, AGResumeReturnUnwind)
            }
          }
      }
  }
}

/// Mark `Executing`, run one sm turn, dispatch outcome. Port of arc `run_body`
/// (async_generators.gleam:189-243) with the `yield*`-delegate branch dropped.
fn run_asyncgen_body(
  st: Agent,
  gen_h: Handle,
  req: AsyncGenRequest,
  sent: #(Int, JsVal),
) -> Agent {
  // Mark executing FIRST so re-entrant next()/return()/throw() enqueue.
  let st = write_asyncgen(st, gen_h, ag_set_state(_, AGExecuting))
  let resume = { read_asyncgen(st, gen_h) }.resume
  let st = rt_store.t_enter_call(st)
  let #(step, st) = apply_resume(st, resume, sent)
  let st = rt_store.t_leave_call(st)
  drive_step(st, asyncgen_ctx(gen_h, req), step)
}

/// The `StepCtx` for the asyncgen body — port of arc `handle_exec_result`
/// (async_generators.gleam:546-579).
fn asyncgen_ctx(gen_h: Handle, req: AsyncGenRequest) -> StepCtx(Agent) {
  StepCtx(
    on_return: fn(st, v) {
      let st = write_asyncgen(st, gen_h, ag_complete_drop_head)
      let st = fulfill_iter(st, req.resolve, v, True)
      drain_queue(st, gen_h)
    },
    on_throw: fn(st, e) {
      let st = write_asyncgen(st, gen_h, ag_complete_drop_head)
      let st = call_settle(st, req.reject, [e])
      drain_queue(st, gen_h)
    },
    on_yield: fn(st, v, ns, loc) {
      // Repin resume, dequeue + resolve request, loop.
      let st =
        write_asyncgen(st, gen_h, fn(ag) {
          ag
          |> ag_repin(ns, loc)
          |> ag_set_state(AGSuspendedYield)
          |> ag_drop_head
        })
      let st = fulfill_iter(st, req.resolve, v, False)
      drain_queue(st, gen_h)
    },
    on_await: fn(st, v, ns, loc) {
      // Repin (state stays Executing); do NOT dequeue — same request stays at
      // head until a yield/return/throw settles it.
      let st = write_asyncgen(st, gen_h, ag_repin(_, ns, loc))
      setup_asyncgen_await(st, gen_h, v, AGResumeBody)
    },
  )
}

/// AsyncGeneratorAwaitReturn / body-await / return-unwind settlement. Called
/// from M6's `dispatch_native` for `AsyncGenResume(gen_h, is_throw, kind)`
/// with `settled` = the awaited value/reason. Port of arc `call_native_resume`
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
      case kind {
        AGResumeAwaitingReturn -> {
          // Completed-gen `.return(v)` await settled: settle head, drain.
          let st = write_asyncgen(st, gen_h, ag_complete_drop_head)
          let st = case is_throw {
            False -> fulfill_iter(st, req.resolve, settled, True)
            True -> call_settle(st, req.reject, [settled])
          }
          drain_queue(st, gen_h)
        }
        // Body await OR §27.6.3.10 return-unwind await: re-drive the sm.
        // Only the fulfil-mode differs — return-unwind injects mode 2 with
        // the AWAITED value (arc AGResumeReturnUnwind :646-655).
        AGResumeBody ->
          redrive_asyncgen(
            st,
            gen_h,
            req,
            ag.resume,
            is_throw,
            sent_next,
            settled,
          )
        AGResumeReturnUnwind ->
          redrive_asyncgen(
            st,
            gen_h,
            req,
            ag.resume,
            is_throw,
            sent_return,
            settled,
          )
      }
  }
}

/// Shared body of `t_asyncgen_resume`'s `AGResumeBody`/`AGResumeReturnUnwind`
/// arms: run one sm turn with `Sent = {sent_throw|fulfil_mode, settled}`.
fn redrive_asyncgen(
  st: Agent,
  gen_h: Handle,
  req: AsyncGenRequest,
  resume: CompiledFn,
  is_throw: Bool,
  fulfil_mode: Int,
  settled: JsVal,
) -> Agent {
  let mode = case is_throw {
    True -> sent_throw
    False -> fulfil_mode
  }
  let st = rt_store.t_enter_call(st)
  let #(step, st) = apply_resume(st, resume, #(mode, settled))
  let st = rt_store.t_leave_call(st)
  drive_step(st, asyncgen_ctx(gen_h, req), step)
}

// ── await wiring (§27.7.5.3 Await, specialized to asyncgen) ─────────────────

/// PromiseResolve(awaited).then(on_fulfill, on_reject) with `AsyncGenResume`
/// closures. Port of arc `promises.setup_await` (promises.gleam:1715-1766).
/// The child promise `t_promise_then` allocates is discarded (spec's step 6
/// PerformPromiseThen has no result capability; the extra cell is inert).
fn setup_asyncgen_await(
  st: Agent,
  gen_h: Handle,
  awaited: JsVal,
  kind: AGResumeKind,
) -> Agent {
  // Step 2: PromiseResolve(%Promise%, value).
  let #(promise_h, st) = promise_resolve_static(st, awaited)
  let #(on_fulfill, st) = alloc_asyncgen_resume(st, gen_h, False, kind)
  let #(on_reject, st) = alloc_asyncgen_resume(st, gen_h, True, kind)
  let #(_, st) =
    t_promise_then(st, promise_h, mk_object(on_fulfill), mk_object(on_reject))
  st
}

/// Call `resolve({value, done})`. Port of arc `fulfill_iter`
/// (async_generators.gleam:893-902).
fn fulfill_iter(st: Agent, resolve: JsVal, value: JsVal, done: Bool) -> Agent {
  let #(result_h, st) = alloc_iter_result(st, value, done)
  call_settle(st, resolve, [mk_object(result_h)])
}

// ── SAsyncGen slot helpers (port arc slot read/write helpers :681-886) ──────
// Decoded live view: mutable state/queue exposed only at the read/write seam,
// so a body-executing path never holds a stale queue snapshot.

type AGLive {
  AGLive(
    state: AsyncGenState,
    resume: CompiledFn,
    front: List(AsyncGenRequest),
    back: List(AsyncGenRequest),
    gen_cell: Handle,
  )
}

fn read_asyncgen(st: Agent, gen_h: Handle) -> AGLive {
  case rt_store.t_cell_get(st, gen_h) {
    SAsyncGen(state:, resume:, queue: #(front, back), gen_cell:) ->
      AGLive(state:, resume:, front:, back:, gen_cell:)
    _ -> panic as "rt_async: Handle is not an SAsyncGen cell (engine invariant)"
  }
}

fn encode_asyncgen(ag: AGLive) -> JsSlot {
  SAsyncGen(
    state: ag.state,
    resume: ag.resume,
    queue: #(ag.front, ag.back),
    gen_cell: ag.gen_cell,
  )
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
  rt_store.t_cell_set(
    st,
    gen_h,
    encode_asyncgen(update(read_asyncgen(st, gen_h))),
  )
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

fn ag_repin(ag: AGLive, ns: Int, loc: Loc) -> AGLive {
  AGLive(..ag, resume: repin_resume(ag.resume, ns, loc))
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
// `exec/promises.gleam:1715-1766 setup_await`, re-expressed under the M18 sm
// ABI. The internal `SGenerator` cell is REUSED as the async-context slot:
// `resume = {sm, rs, loc}`, `gen_cell = result_promise_h` (SPEC §7.M8 note).

/// First argument or `undefined` — the `arguments[0]` a resolving/resume
/// function's body reads (arc `helpers.first_arg_or_undefined`).
fn first_arg(args: List(JsVal)) -> JsVal {
  case args {
    [v, ..] -> v
    [] -> mk_undefined()
  }
}

/// `Some(h)` iff `v` is a Handle to an `SPromise` cell — IsPromise + data-ref
/// extraction in one (arc `promise.gleam:589-596 as_promise_data`).
fn as_promise(st: Agent, v: JsVal) -> Option(Handle) {
  case classify(v) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SPromise(..) -> Some(h)
        _ -> None
      }
    _ -> None
  }
}

/// §27.7.5.1 AsyncFunctionStart. Allocate the result promise + an internal
/// `SGenerator` state cell (reused as the async context; `gen_cell` = result
/// promise handle), run the sm's first turn, drive its outcome, and return
/// the result promise (R1 value-first). Port of arc
/// `call.gleam:324-366 call_async_function`. Frame/args accepted for §8
/// op-table 4-arg parity, unused (already packed into `loc0` by the outer
/// prologue — SPEC §18.1).
pub fn t_async_start(
  st: Agent,
  sm: CompiledFn,
  _frame: Frame,
  _args: List(JsVal),
  loc0: Loc,
) -> #(Handle, Agent) {
  let #(promise_h, st) = t_new_promise(st)
  // Internal state cell: `resume` = `{sm, 0, loc0}`; `gen_cell` links to the
  // result promise so `do_async_resume` can settle it.
  let #(gen_h, st) =
    rt_store.t_cell_new(
      st,
      SGenerator(
        state: GenExecuting,
        resume: mk_resume(sm, 0, loc0),
        gen_cell: promise_h,
      ),
    )
  let #(step, st) = apply_sm(st, sm, 0, sent_start(), loc0)
  let st = drive_async_step(st, gen_h, promise_h, step)
  #(promise_h, st)
}

/// Shared completion handling for one async-fn sm turn. Port of arc
/// `call.gleam:398-465 finish_async_execution`. `StepYield` in a plain async
/// function is an engine bug (M18 never emits it for `is_generator: False`).
fn drive_async_step(
  st: Agent,
  gen_h: Handle,
  promise_h: Handle,
  step: Step,
) -> Agent {
  drive_step(
    st,
    StepCtx(
      // §27.7.5.2 step 3.d: fulfil via Resolve so a thenable return is adopted.
      on_return: fn(st, v) {
        let st = complete_async(st, gen_h)
        t_promise_resolve(st, promise_h, v)
      },
      // §27.7.5.2 step 3.f: reject the result promise.
      on_throw: fn(st, e) {
        let st = complete_async(st, gen_h)
        t_promise_reject(st, promise_h, e)
      },
      on_yield: step_unreachable,
      // Body hit `await` — re-pin the resume state and hand off to `t_await`.
      on_await: fn(st, awaited, ns, loc) {
        let st =
          rt_store.t_cell_update(st, gen_h, fn(slot) {
            case slot {
              SGenerator(resume:, gen_cell:, ..) ->
                SGenerator(
                  state: GenExecuting,
                  resume: repin_resume(resume, ns, loc),
                  gen_cell:,
                )
              other -> other
            }
          })
        t_await(st, gen_h, awaited, ns, loc)
      },
    ),
    step,
  )
}

/// Mark the async-context cell completed (releases its `Loc`/captures for GC
/// on the next turn-boundary collect).
fn complete_async(st: Agent, gen_h: Handle) -> Agent {
  rt_store.t_cell_update(st, gen_h, fn(slot) {
    case slot {
      SGenerator(resume:, gen_cell:, ..) ->
        SGenerator(state: GenCompleted, resume:, gen_cell:)
      other -> other
    }
  })
}

/// §27.7.5.3 Await. `PromiseResolve(%Promise%, awaited)` then
/// `PerformPromiseThen` with `AsyncResume` on-fulfill/on-reject closures that
/// re-invoke `gen_h`'s stored sm at `(next_state, {mode, sent}, locals)`. Port
/// of arc `promises.gleam:1715-1766 setup_await`. `next_state`/`locals` are
/// carried per SPEC signature but the resume closure reads them from
/// `gen_h.resume` (already re-pinned by `drive_async_step`), so they are
/// unused here beyond documenting the ABI. Always enqueues (§27.5.3 — an
/// already-settled promise still resumes asynchronously).
pub fn t_await(
  st: Agent,
  gen_h: Handle,
  awaited: JsVal,
  _next_state: Int,
  _locals: Loc,
) -> Agent {
  // §27.7.5.3 step 2: PromiseResolve(%Promise%, value).
  let #(awaited_h, st) = promise_resolve_static(st, awaited)
  // Steps 3-4: onFulfilled/onRejected close over the async-context handle.
  let #(on_fulfill, st) = alloc_resume(st, gen_h, False)
  let #(on_reject, st) = alloc_resume(st, gen_h, True)
  // Step 5: PerformPromiseThen with a throwaway capability (§27.7.5.3 note —
  // the child promise is never observed; arc allocates one anyway).
  let #(_child, st) =
    t_promise_then(st, awaited_h, mk_object(on_fulfill), mk_object(on_reject))
  st
}

// ── native-token dispatch bodies (called by M6 dispatch_native) ─────────────

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

/// `AsyncResume` body — §27.7.5.3 Await onFulfilled/onRejected. Reads the
/// stored `{sm, rs, loc}` from `gen_h`, re-invokes the sm with
/// `Sent = {mode, settled_value}`, and drives the resulting step. Port of
/// arc `call.gleam:481-543 call_native_async_resume`.
pub fn do_async_resume(
  st: Agent,
  gen_h: Handle,
  is_throw: Bool,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case rt_store.t_cell_get(st, gen_h) {
    SGenerator(resume:, gen_cell: promise_h, ..) -> {
      let mode = case is_throw {
        False -> sent_next
        True -> sent_throw
      }
      let #(step, st) = apply_resume(st, resume, #(mode, first_arg(args)))
      #(mk_undefined(), drive_async_step(st, gen_h, promise_h, step))
    }
    _ ->
      panic as "rt_async: AsyncResume target is not SGenerator (engine invariant)"
  }
}
