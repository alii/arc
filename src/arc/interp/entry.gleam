//// Entry points of the bytecode interpreter: run a script, run one bytecode
//// function to completion for the shared runtime, resume a parked coroutine
//// frame, and `link`, which seeds those into an `Agent`'s `JsOps` so the
//// runtime's builtins and compiled code can call back into bytecode.
////
//// Every root activation runs under ONE backstop `try` (`ffi.guard1`): the
//// step loop is Result-based and catches runtime throws at each guarded
//// call, so a `wasm_exn` reaching the backstop means an unguarded raise
//// slipped through. It is still folded into a throw completion carrying the
//// agent the exception travelled with, and the activation's `Error.stack`
//// frames and depth are trued up to what they were on entry.

import arc/interp/call
import arc/interp/eval
import arc/interp/ffi
import arc/interp/interpreter.{Completed, Suspended}
import arc/interp/park
import arc/interp/state.{type State, type VmError, State, SuspensionLeak}
import arc/rt/async as rt_async
import arc/rt/builtins/iter_protocol
import arc/rt/bytecode.{
  type FuncTemplate, type ParkedAt, type SuspendedFrame, ParkedDelegateClose,
  ParkedDelegateReturn, ParkedOp, ParkedReturnValue, ParkedStart, TryFrame,
}
import arc/rt/call.{
  type Completion, type Frame, NormalCompletion, ThrowCompletion,
} as rt_call
import arc/rt/lang as rt_lang
import arc/rt/obj as rt_obj
import arc/rt/realm as rt_realm
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type EvalKind, type FrameInfo, type Handle, type IteratorRecord,
  type JsOps, type JsVal, type Step, Agent, JInt, JsOps, JsStore, KBytecode,
  KHandle, KNull, KUndef, Named, RangeErr, ResumeFrame, SObject, StepAwait,
  StepReturn, StepThrow, StepYield, StringKey, TypeErr, classify, mk_number,
  mk_object, mk_undefined,
}
import arc/rt/val as rt_val
import arc/vm/internal/tuple_array.{type TupleArray}
import arc/vm/lexical
import arc/vm/limits
import arc/vm/opcode.{
  AsyncYieldStarNext, CatchOnly, Finally, IterCloseGuard, Pc, YieldStar,
}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

// -- Linking -------------------------------------------------------------------

/// Seed the interpreter into `agent`: from here the runtime's `[[Call]]` /
/// `[[Construct]]` of a `KBytecode` cell, its coroutine drivers and
/// `eval`/`Function()` reach the functions below. Idempotent.
pub fn link(agent: Agent) -> Agent {
  let store = agent.store
  Agent(..agent, store: JsStore(..store, ops: linked_ops(store.ops)))
}

fn linked_ops(ops: JsOps(Agent)) -> JsOps(Agent) {
  JsOps(
    ..ops,
    eval_hook: eval_source,
    call_bytecode:,
    construct_bytecode:,
    resume_frame:,
  )
}

// -- Driving one activation --------------------------------------------------------

/// How a root activation's turn ended once its own call stack emptied: it
/// returned or threw, or (coroutine bodies only) parked at a `yield`/`await`
/// in the state given.
type Outcome {
  Finished(Result(JsVal, JsVal), State)
  Parked(state.SuspendKind, JsVal, State)
}

/// An engine fault has no channel of its own out of a `#(Completion, Agent)`
/// entry point: it surfaces as a thrown TypeError naming the fault.
fn fault(s: State, err: VmError) -> #(Result(JsVal, JsVal), State) {
  let #(e, s) =
    state.new_error(
      s,
      TypeErr,
      "internal error: " <> state.vm_error_message(err),
    )
  #(Error(e), s)
}

/// The loop's callbacks into this module.
fn drive() -> call.Drive {
  call.Drive(start_coroutine:)
}

/// Drive `state` until its call stack empties or it parks.
fn execute(state: State) -> Outcome {
  case interpreter.execute_inner(state, drive()) {
    Ok(#(Completed(NormalCompletion(v)), s)) -> Finished(Ok(v), s)
    Ok(#(Completed(ThrowCompletion(e)), s)) -> Finished(Error(e), s)
    Ok(#(Suspended(kind, v), s)) -> Parked(kind, v, s)
    Error(err) -> {
      let #(res, s) = fault(state, err)
      Finished(res, s)
    }
  }
}

/// `execute` for a frame that cannot resume a suspension (script, eval,
/// plain function): a park escaping it is an engine fault.
fn complete(state: State, site: String) -> #(Result(JsVal, JsVal), State) {
  case execute(state) {
    Finished(res, s) -> #(res, s)
    Parked(kind, _, s) -> fault(s, SuspensionLeak(site:, kind:))
  }
}

/// What the backstop restores: the `Error.stack` frames and call depth the
/// agent had before this activation pushed anything.
type EntryMark {
  EntryMark(frames: List(FrameInfo), call_depth: Int)
}

fn mark(agent: Agent) -> EntryMark {
  EntryMark(frames: agent.frames, call_depth: agent.store.call_depth)
}

/// True `agent` up to `m`. On the ordinary paths this is a no-op (every
/// frame pushed has been popped); after a backstop catch, or a throw that
/// unwound out of the root frame, it discards what the abandoned frames
/// left behind.
fn settle(agent: Agent, m: EntryMark) -> Agent {
  let store = agent.store
  case agent.frames == m.frames && store.call_depth == m.call_depth {
    True -> agent
    False ->
      Agent(
        ..agent,
        frames: m.frames,
        store: JsStore(..store, call_depth: m.call_depth),
      )
  }
}

/// Run `body` from `agent` under the backstop, yielding its result and the
/// agent trued up to `m`.
fn backstopped(
  agent: Agent,
  m: EntryMark,
  body: fn(Agent) -> #(a, Agent),
  on_escape: fn(JsVal) -> a,
) -> #(a, Agent) {
  case ffi.guard1(body, agent) {
    ffi.Ok(value:, agent:) -> #(value, settle(agent, m))
    ffi.Threw(agent:, thrown:) -> #(on_escape(thrown), settle(agent, m))
  }
}

/// Run a prepared root activation (script body, eval code, module body)
/// until its call stack empties: `Ok(value)` / `Error(thrown)` and the agent
/// it finished in, with the activation's `Error.stack` frame pushed for the
/// duration. This is the `Run` the eval machinery is handed.
pub fn run(state: State) -> #(Result(JsVal, JsVal), Agent) {
  let m = mark(state.agent)
  let agent = call.push_frame_info(state.agent, state.func)
  let body = fn(agent) {
    let #(res, s) = complete(State(..state, agent:), "run")
    #(res, s.agent)
  }
  backstopped(agent, m, body, Error)
}

fn to_completion(res: Result(JsVal, JsVal)) -> Completion {
  case res {
    Ok(v) -> NormalCompletion(v)
    Error(e) -> ThrowCompletion(e)
  }
}

// -- Scripts ---------------------------------------------------------------------

/// Locals for global code: all `undefined` except the lexical `this` slot,
/// which holds the global object (§9.1.1.4.11 GetThisBinding).
fn top_level_locals(template: FuncTemplate, this: JsVal) -> TupleArray(JsVal) {
  let locals = tuple_array.repeat(mk_undefined(), template.local_count)
  case lexical.lexical_slot(template.lexical, lexical.RefThis) {
    Some(idx) -> tuple_array.set_unchecked(idx, this, locals)
    None -> locals
  }
}

/// A root activation of `template` in the current realm's global environment
/// (§16.1.6 ScriptEvaluation steps 1-11): `this` is the global object.
pub fn script_state(agent: Agent, template: FuncTemplate) -> State {
  let this = mk_object(agent.realm.global_object)
  State(
    agent:,
    pc: 0,
    stack: [],
    locals: top_level_locals(template, this),
    code: template.bytecode,
    constants: template.constants,
    func: template,
    call_stack: [],
    try_stack: [],
    this:,
    new_target: mk_undefined(),
    home_object: mk_undefined(),
    call_args: [],
    eval_env: None,
  )
}

/// §16.1.6 ScriptEvaluation of a compiled script in `agent`'s current realm.
/// Global declarations are instantiated by the script's own prologue
/// opcodes; lexical globals persist on the realm, so consecutive scripts on
/// the returned agent share one global environment. Microtasks are NOT
/// drained here: the engine's turn epilogue owns the one drain.
pub fn run_script(
  agent: Agent,
  template: FuncTemplate,
) -> #(Completion, Agent) {
  let #(res, agent) = run(script_state(agent, template))
  #(to_completion(res), agent)
}

// -- JsOps.call_bytecode / construct_bytecode ----------------------------------

@external(erlang, "erlang", "element")
fn frame_element(n: Int, frame: Frame) -> JsVal

/// Run the bytecode function `cell` as a fresh root activation over the
/// runtime call `frame` (`{this, active_func, home_object, new_target}`) and
/// `args`, until ITS call stack empties. See `run_root`.
pub fn run_bytecode(
  st: Agent,
  cell: Handle,
  frame: Frame,
  args: List(JsVal),
) -> #(Completion, Agent) {
  run_root(st, cell, frame_element(1, frame), args, frame_element(4, frame))
}

/// `new_target` undefined is a [[Call]]; otherwise a [[Construct]] whose
/// receiver `enter_root` creates, with the constructor return rules applied
/// to the result. A generator or async body is started through the
/// coroutine driver and completes with its generator object / promise. The
/// enclosing `t_call` (or `construct_bytecode`) owns the depth bracket;
/// `enter_root`/`finish_root` own the `Error.stack` frame; this owns the
/// backstop.
///
/// §10.2.1.1 PrepareForOrdinaryCall step 5: the callee's [[Realm]] is the
/// running realm while its body runs and the caller's is restored once it
/// unwinds (also on a throw, which arrives here as a completion). §10.2.2
/// [[Construct]] steps 10-13 — the return-override / uninitialised-`this`
/// checks — run after that restore, so their errors are the caller's.
fn run_root(
  st: Agent,
  cell: Handle,
  this: JsVal,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Completion, Agent) {
  let #(outcome, st) = {
    use st <- rt_realm.with_realm(st, closure_realm(st, cell))
    run_root_body(st, cell, this, args, new_target)
  }
  case outcome {
    RootSettled(c) -> #(c, st)
    RootReturned(kind, v, final) ->
      case call.finish_root(kind, v, State(..final, agent: st)) {
        Ok(#(v, agent)) -> #(NormalCompletion(v), agent)
        Error(#(e, agent)) -> #(ThrowCompletion(e), agent)
      }
  }
}

/// How a root activation's body ended: already a completion (a throw, or a
/// coroutine's generator object / promise), or a plain `Return` whose value
/// still owes the constructor return rules.
type RootOutcome {
  RootSettled(Completion)
  RootReturned(call.RootKind, JsVal, State)
}

/// The [[Realm]] of the bytecode cell `cell`; anything else counts as the
/// current realm.
fn closure_realm(st: Agent, cell: Handle) -> Int {
  case rt_store.t_cell_get(st, cell) {
    SObject(kind: KBytecode(realm:, ..), ..) -> realm
    _ -> st.realm.id
  }
}

fn run_root_body(
  st: Agent,
  cell: Handle,
  this: JsVal,
  args: List(JsVal),
  new_target: JsVal,
) -> #(RootOutcome, Agent) {
  let m = mark(st)
  case call.enter_root(st, cell, this, args, new_target) {
    Error(#(thrown, agent)) -> #(
      RootSettled(ThrowCompletion(thrown)),
      settle(agent, m),
    )
    Ok(#(state, kind, coroutine)) ->
      case state.func.is_generator || state.func.is_async {
        // `start_coroutine` gives the body its own `Error.stack` frame:
        // drop the one `enter_root` pushed rather than show it twice.
        True -> {
          let agent = call.pop_frame_info(state.agent)
          let body = fn(agent) {
            let #(res, s) =
              start_coroutine_root(State(..state, agent:), coroutine)
            #(RootSettled(to_completion(res)), s.agent)
          }
          backstopped(agent, m, body, escaped)
        }
        False -> {
          let body = fn(agent) {
            let #(res, s) = complete(State(..state, agent:), "run_bytecode")
            let agent = call.pop_frame_info(s.agent)
            case res {
              Ok(v) -> #(RootReturned(kind, v, s), agent)
              Error(e) -> #(RootSettled(ThrowCompletion(e)), agent)
            }
          }
          backstopped(state.agent, m, body, escaped)
        }
      }
  }
}

fn escaped(thrown: JsVal) -> RootOutcome {
  RootSettled(ThrowCompletion(thrown))
}

/// A generator / async root call (never a [[Construct]]: neither kind is a
/// constructor): the driver turns the laid-out frame into its generator
/// object or promise and pushes it onto the (empty) root stack; the body
/// itself never runs to a Return here.
fn start_coroutine_root(
  st: State,
  coroutine: call.CoroutineCall,
) -> #(Result(JsVal, JsVal), State) {
  case start_coroutine(st, coroutine) {
    Ok(s) ->
      case s.stack {
        [v, ..] -> #(Ok(v), s)
        [] -> #(Ok(mk_undefined()), s)
      }
    Error(state.Threw(e, s)) -> #(Error(e), s)
    Error(state.Returned(v, s)) -> #(Ok(v), s)
    Error(state.VmFailed(err, s)) -> fault(s, err)
    Error(state.Yielded(_, _, s)) ->
      fault(s, SuspensionLeak("run_bytecode", state.Yield))
    Error(state.Awaited(_, s)) ->
      fault(s, SuspensionLeak("run_bytecode", state.Await))
  }
}

/// `JsOps.call_bytecode`: [[Call]] (or, with `new_target` set, the body of
/// a [[Construct]]) of the bytecode cell `fn_h`, re-raising a throw so it
/// propagates through the runtime like any other. Runs inside the caller's
/// `t_call` bracket.
pub fn call_bytecode(
  st: Agent,
  fn_h: Handle,
  this: JsVal,
  args: List(JsVal),
  new_target: JsVal,
) -> #(JsVal, Agent) {
  case run_root(st, fn_h, this, args, new_target) {
    #(NormalCompletion(v), st) -> #(v, st)
    #(ThrowCompletion(e), st) -> rt_store.t_throw(st, e)
  }
}

/// `JsOps.construct_bytecode`: §10.2.2 [[Construct]] of the bytecode cell
/// `fn_h` (IsConstructor already checked by `t_construct`). `t_construct`
/// dispatches here unbracketed, so the construct's unit of `call_depth` is
/// taken here, as `rt/call.apply_ctor` does for a compiled constructor:
/// RangeError at `limits.max_call_depth`, and the body's root-`Return`
/// safepoint kept shut over the caller's registers. The result is always an
/// object: `enter_root`/`finish_root` create the receiver and apply the
/// return override, so a non-object here is an engine fault.
pub fn construct_bytecode(
  st: Agent,
  fn_h: Handle,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  let st = rt_store.t_enter_call(st)
  let #(completion, st) = run_root(st, fn_h, mk_undefined(), args, new_target)
  let st = rt_store.t_leave_call(st)
  let #(v, st) = case completion {
    NormalCompletion(v) -> #(v, st)
    ThrowCompletion(e) -> rt_store.t_throw(st, e)
  }
  case classify(v) {
    KHandle(h) -> #(h, st)
    _ -> {
      let #(e, st) =
        st.store.ops.new_error(
          st,
          TypeErr,
          "internal error: constructor completed with a non-object",
        )
      rt_store.t_throw(st, e)
    }
  }
}

// -- Coroutine calls (Drive.start_coroutine) --------------------------------------

/// A call whose callee is a generator, async function or async generator,
/// with its frame already laid out by `call`. The body becomes a root
/// activation of its own; what the caller receives is pushed onto
/// `rest_stack` and the caller moves past its Call opcode.
///
/// - generator / async generator (§27.5.3.1 GeneratorStart): the body runs
///   now up to its `InitialYield` (FunctionDeclarationInstantiation is call
///   time work) and is parked there; the runtime allocates the generator over
///   `ResumeFrame(frame)`. A throw before InitialYield is the call's throw.
/// - async function (§27.7.5.1 AsyncFunctionStart): the frame is parked at
///   pc 0 unrun and handed to `t_async_run`, which runs the first turn
///   through `resume_frame` and returns the result promise.
///
/// Either way that first stretch of the body is a nested activation run
/// while the caller's registers live only in Gleam variables, so it holds
/// one unit of `call_depth` (`nested`), given back by settling to the mark.
fn start_coroutine(
  caller: State,
  c: call.CoroutineCall,
) -> Result(State, state.StepExit) {
  let call.CoroutineCall(
    fn_h:,
    template:,
    locals:,
    this:,
    home_object:,
    args:,
    rest_stack:,
  ) = c
  let caller = State(..caller, stack: rest_stack)
  let m = mark(caller.agent)
  use agent <- nested(caller)
  // The body's own `Error.stack` frame, so neither its first stretch nor
  // the parked snapshot's line reads the caller's.
  let body =
    State(
      agent: call.push_frame_info(agent, template),
      pc: 0,
      stack: [],
      locals:,
      code: template.bytecode,
      constants: template.constants,
      func: template,
      call_stack: [],
      try_stack: [],
      this:,
      new_target: mk_undefined(),
      home_object:,
      call_args: args,
      eval_env: None,
    )
  let callee = mk_object(fn_h)
  let resume = fn(agent: Agent, value: JsVal) {
    let agent = settle(agent, m)
    Ok(State(..caller, agent:, stack: [value, ..rest_stack], pc: caller.pc + 1))
  }
  let threw = fn(agent: Agent, thrown: JsVal) {
    Error(state.Threw(thrown, State(..caller, agent: settle(agent, m))))
  }
  case template.is_generator {
    False -> {
      // Only the snapshot is taken from `body`; `resume_frame` pushes the
      // live frame when `t_async_run` runs the first turn on `agent`.
      let frame = park.park(body, ParkedStart)
      case ffi.guard2(rt_async.t_async_run, agent, ResumeFrame(frame)) {
        ffi.Ok(value: promise, agent:) -> resume(agent, mk_object(promise))
        ffi.Threw(agent:, thrown:) -> threw(agent, thrown)
      }
    }
    True ->
      case execute(body) {
        Parked(state.Yield, _, s) -> {
          let frame = ResumeFrame(park.park(s, ParkedStart))
          let agent = settle(s.agent, m)
          let #(obj, agent) = case template.is_async {
            False -> rt_async.t_gen_new(agent, callee, frame)
            True -> rt_async.t_asyncgen_new(agent, callee, frame)
          }
          resume(agent, mk_object(obj))
        }
        Finished(Error(thrown), s) -> threw(s.agent, thrown)
        // InitialYield is the body's first suspension point: it can neither
        // complete nor await before reaching it.
        Finished(Ok(_), s) | Parked(state.Await, _, s) ->
          Error(state.VmFailed(
            state.InternalError("start_coroutine", "body missed InitialYield"),
            State(..caller, agent: settle(s.agent, m)),
          ))
      }
  }
}

/// Take one unit of `call_depth` for a nested activation entered straight
/// from an opcode arm (the convention `eval.run_bracketed` states): the
/// caller's frame is not among the collector's roots while it runs, and a
/// positive depth is what keeps the root-`Return` safepoint shut. The same
/// unit bounds recursion through such entries: at `limits.max_call_depth`
/// the entry is refused with a RangeError thrown in the caller.
fn nested(
  caller: State,
  k: fn(Agent) -> Result(State, state.StepExit),
) -> Result(State, state.StepExit) {
  let store = caller.agent.store
  case store.call_depth >= limits.max_call_depth {
    True -> {
      let #(err, caller) =
        state.new_error(caller, RangeErr, "Maximum call stack size exceeded")
      Error(state.Threw(err, caller))
    }
    False ->
      k(
        Agent(
          ..caller.agent,
          store: JsStore(..store, call_depth: store.call_depth + 1),
        ),
      )
  }
}

// -- JsOps.eval_hook -----------------------------------------------------------

/// `JsOps.eval_hook`: indirect eval, `Function()` bodies and
/// `$262.evalScript`, driven by `run`.
pub fn eval_source(
  st: Agent,
  source: String,
  kind: EvalKind,
) -> #(JsVal, Agent) {
  eval.eval_hook(st, source, kind, run)
}

// -- JsOps.resume_frame ----------------------------------------------------------

/// `JsOps.resume_frame`: continue the parked coroutine body `frame` with
/// `sent` = `#(mode, value)`, mode 0 `.next(value)` / await fulfilled, 1
/// `.throw(value)` / await rejected, 2 `.return(value)`, for one turn, and
/// report how the turn ended; a turn that parks again hands back the new
/// frame inside the `Step`. The driver (`rt/async`) owns the generator state
/// transitions and the depth bracket; this owns the body's `Error.stack`
/// frame and the backstop.
pub fn resume_frame(
  st: Agent,
  frame: SuspendedFrame,
  sent: #(Int, JsVal),
) -> #(Step, Agent) {
  let m = mark(st)
  let agent = call.push_frame_info(st, frame.template)
  let #(mode, value) = sent
  let turn = fn(agent) {
    let s = park.unpark(agent, frame)
    case frame.parked, mode {
      ParkedStart, 0 -> step_of(execute(s))
      ParkedOp, 0 -> step_of(execute(State(..s, stack: [value, ..s.stack])))
      ParkedOp, 1 -> inject_throw(s, value)
      ParkedOp, _ -> inject_return(s, value)
      ParkedDelegateReturn, 0 -> delegate_returned(s, value)
      ParkedReturnValue, 0 -> step_of(return_into(s, value))
      ParkedDelegateClose, 0 -> delegate_closed(s, value)
      // A rejected delegate await (or an abrupt completion delivered before
      // the body ran) lands at the parked point like any other.
      _, 1 -> step_of(throw_into(s, value))
      _, _ -> step_of(return_into(s, value))
    }
  }
  backstopped(agent, m, turn, StepThrow)
}

/// A turn's outcome as the coroutine driver's `Step`; a park hands back the
/// frame to resume.
fn step_of(outcome: Outcome) -> #(Step, Agent) {
  case outcome {
    Finished(Ok(v), s) -> #(StepReturn(v), s.agent)
    Finished(Error(e), s) -> #(StepThrow(e), s.agent)
    Parked(state.Yield, v, s) -> #(
      StepYield(v, ResumeFrame(park.park(s, ParkedOp))),
      s.agent,
    )
    Parked(state.Await, v, s) -> await_at(s, v, ParkedOp)
  }
}

/// The turn ends awaiting `v`; its settlement resumes `s` as `parked` says.
fn await_at(s: State, v: JsVal, parked: ParkedAt) -> #(Step, Agent) {
  #(StepAwait(v, ResumeFrame(park.park(s, parked))), s.agent)
}

/// Run a prepared root activation for its first turn: until its call stack
/// empties or the body parks, with the activation's `Error.stack` frame
/// pushed for the duration. This is how a module body runs (§16.2.1.5.3.4
/// ExecuteAsyncModule): it may park on a top-level `await`, and the `Step`
/// then carries the frame `resume_frame` continues like any async body.
pub fn run_turn(state: State) -> #(Step, Agent) {
  let m = mark(state.agent)
  let agent = call.push_frame_info(state.agent, state.func)
  let turn = fn(agent) { step_of(execute(State(..state, agent:))) }
  backstopped(agent, m, turn, StepThrow)
}

/// Deliver a throw completion at the body's current point: it lands on the
/// innermost enclosing handler, or ends the body.
fn throw_into(s: State, thrown: JsVal) -> Outcome {
  case interpreter.unwind_to_catch(s, thrown) {
    Some(caught) -> execute(caught)
    None -> Finished(Error(thrown), s)
  }
}

/// `throw_into` with a fresh TypeError.
fn throw_type_into(s: State, msg: String) -> #(Step, Agent) {
  let #(e, s) = state.new_error(s, TypeErr, msg)
  step_of(throw_into(s, e))
}

// -- yield* delegation (§27.5.3.8 steps 7.b / 7.c) -----------------------------
// A body parked mid-`yield*` sits ON its delegation opcode with the delegate's
// Iterator Record on top of the operand stack: `YieldStar` in a generator,
// `AsyncYieldStarNext` (followed by `Await; AsyncYieldStarResume`) in an
// async generator. `.next(v)` re-runs the opcode, which calls the delegate's
// `next` itself; `.throw(v)` / `.return(v)` are forwarded to the delegate
// here, since the opcode only ever sees normal resumptions.

const missing_throw = "The iterator does not provide a 'throw' method."

/// The delegation a parked body is in: the record, the stack under it, and
/// for the async lowering the pc of the `Await` after the site.
type DelegateSite {
  SyncSite(record: IteratorRecord, rest: List(JsVal))
  AsyncSite(record: IteratorRecord, rest: List(JsVal), await_pc: Int)
}

fn delegate_site(s: State) -> Option(DelegateSite) {
  case tuple_array.get_unchecked(s.pc, s.code), s.stack {
    YieldStar, [rec, ..rest] ->
      rt_lang.record_parts(s.agent, rec)
      |> option.map(SyncSite(_, rest))
    AsyncYieldStarNext(..), [rec, ..rest] ->
      rt_lang.record_parts(s.agent, rec)
      |> option.map(AsyncSite(_, rest, s.pc + 1))
    _, _ -> None
  }
}

fn site_record(site: DelegateSite) -> IteratorRecord {
  case site {
    SyncSite(record:, ..) -> record
    AsyncSite(record:, ..) -> record
  }
}

/// §7.3.10 GetMethod(iterator, name) on the delegate: `Ok(None)` when the
/// property is undefined or null. A callable check is left to the call.
fn delegate_method(
  s: State,
  site: DelegateSite,
  name: String,
) -> Result(#(Option(JsVal), State), state.StepExit) {
  let iterator = site_record(site).iterator
  use #(method, s) <- result.map(ffi.guarded(
    ffi.guard3(rt_obj.t_get_prop, s.agent, iterator, StringKey(Named(name))),
    s,
  ))
  case classify(method) {
    KUndef | KNull -> #(None, s)
    _ -> #(Some(method), s)
  }
}

/// Call(method, iterator, «value») on the delegate.
fn call_delegate(
  s: State,
  site: DelegateSite,
  method: JsVal,
  value: JsVal,
) -> Result(#(JsVal, State), state.StepExit) {
  let iterator = site_record(site).iterator
  ffi.guarded(
    ffi.guard4(rt_call.t_call_checked, s.agent, method, iterator, [value]),
    s,
  )
}

/// A throw completion arriving at the body: forwarded to the delegate when
/// parked mid-`yield*` (step 7.b), else delivered at the parked point.
fn inject_throw(s: State, thrown: JsVal) -> #(Step, Agent) {
  case delegate_site(s) {
    None -> step_of(throw_into(s, thrown))
    Some(site) -> forward_throw(s, site, thrown)
  }
}

/// A return completion arriving at the body: forwarded to the delegate when
/// parked mid-`yield*` (step 7.c), else unwound from the parked point.
fn inject_return(s: State, value: JsVal) -> #(Step, Agent) {
  case delegate_site(s) {
    None -> step_of(return_into(s, value))
    Some(site) -> forward_return(s, site, value)
  }
}

/// A `StepExit` from a guarded delegate call: a throw becomes the `yield*`
/// expression's completion inside the body; anything else cannot come out of
/// a single guarded runtime call.
fn delegate_exit(exit: state.StepExit) -> #(Step, Agent) {
  case exit {
    state.Threw(thrown, s) -> step_of(throw_into(s, thrown))
    state.Returned(_, s)
    | state.Yielded(_, _, s)
    | state.Awaited(_, s)
    | state.VmFailed(_, s) -> {
      let #(res, s) =
        fault(s, state.InternalError("yield* delegate", "unexpected step exit"))
      step_of(Finished(res, s))
    }
  }
}

fn or_delegate_exit(
  res: Result(a, state.StepExit),
  k: fn(a) -> #(Step, Agent),
) -> #(Step, Agent) {
  case res {
    Ok(v) -> k(v)
    Error(exit) -> delegate_exit(exit)
  }
}

/// Step 7.b: `throw = GetMethod(iterator, "throw")`.
/// - present: `Call(throw, iterator, «thrown»)`; a generator reads the
///   result now (done: the `yield*` evaluates to its value; not done: yield
///   it and keep delegating); an async generator continues into the site's
///   own `Await; AsyncYieldStarResume`, which do exactly that once the
///   result settles.
/// - absent: close the delegate (IteratorClose / AsyncIteratorClose with a
///   normal completion, whose own errors win), then throw a TypeError at
///   the site.
fn forward_throw(
  s: State,
  site: DelegateSite,
  thrown: JsVal,
) -> #(Step, Agent) {
  use #(method, s) <- or_delegate_exit(delegate_method(s, site, "throw"))
  case method, site {
    Some(method), SyncSite(rest:, ..) -> {
      use #(res, s) <- or_delegate_exit(call_delegate(s, site, method, thrown))
      delegate_result(s, res, rest, fn(s, val) {
        step_of(execute(State(..s, stack: [val, ..rest], pc: s.pc + 1)))
      })
    }
    Some(method), AsyncSite(await_pc:, ..) -> {
      use #(res, s) <- or_delegate_exit(call_delegate(s, site, method, thrown))
      step_of(execute(State(..s, stack: [res, ..s.stack], pc: await_pc)))
    }
    None, SyncSite(record:, ..) -> {
      use s <- or_delegate_exit(
        call.guarded_unit(s, iter_protocol.iterator_close_normal(
          _,
          record.iterator,
        )),
      )
      throw_type_into(s, missing_throw)
    }
    None, AsyncSite(record:, ..) -> {
      // AsyncIteratorClose steps 3-4: GetMethod + Call, both caught.
      use #(closed, s) <- or_delegate_exit(
        call.guarded(s, iter_protocol.call_return(_, record.iterator)),
      )
      case closed {
        Ok(iter_protocol.NoReturnMethod) -> throw_type_into(s, missing_throw)
        // Steps 5-7 run when the result settles.
        Ok(iter_protocol.Returned(result)) ->
          await_at(s, result, ParkedDelegateClose)
        Error(thrown) -> step_of(throw_into(s, thrown))
      }
    }
  }
}

/// Step 7.c: `return = GetMethod(iterator, "return")`.
/// - absent: the return completion carries on out of the `yield*`; an async
///   generator awaits the value once more first (step 7.c.iii.2, on top of
///   the driver's AsyncGeneratorUnwrapYieldResumption).
/// - present: `Call(return, iterator, «value»)`; a generator reads the
///   result now (done: return its value out of the body; not done: yield it
///   and keep delegating); an async generator awaits it first.
fn forward_return(
  s: State,
  site: DelegateSite,
  value: JsVal,
) -> #(Step, Agent) {
  use #(method, s) <- or_delegate_exit(delegate_method(s, site, "return"))
  case method, site {
    None, SyncSite(..) -> step_of(return_into(s, value))
    None, AsyncSite(..) -> await_at(s, value, ParkedReturnValue)
    Some(method), SyncSite(rest:, ..) -> {
      use #(res, s) <- or_delegate_exit(call_delegate(s, site, method, value))
      delegate_result(s, res, rest, fn(s, val) { step_of(return_into(s, val)) })
    }
    Some(method), AsyncSite(..) -> {
      use #(res, s) <- or_delegate_exit(call_delegate(s, site, method, value))
      await_at(s, res, ParkedDelegateReturn)
    }
  }
}

/// IteratorComplete / IteratorValue on a forwarded call's result `res` with
/// the body still parked at the site: not done yields the value and keeps
/// delegating; done hands the value to `on_done`.
fn delegate_result(
  s: State,
  res: JsVal,
  rest: List(JsVal),
  on_done: fn(State, JsVal) -> #(Step, Agent),
) -> #(Step, Agent) {
  use #(#(done, val), s) <- or_delegate_exit(ffi.guarded(
    ffi.guard2(iter_protocol.read_iter_result, s.agent, res),
    s,
  ))
  case done {
    False -> step_of(Parked(state.Yield, val, s))
    True -> on_done(State(..s, stack: rest), val)
  }
}

/// The delegate's `.return(v)` result settled (step 7.c.v-viii): done ends
/// the body with a return completion of its value, not done yields it.
fn delegate_returned(s: State, settled: JsVal) -> #(Step, Agent) {
  let rest = case s.stack {
    [_rec, ..rest] -> rest
    [] -> []
  }
  delegate_result(s, settled, rest, fn(s, val) { step_of(return_into(s, val)) })
}

/// AsyncIteratorClose settled after a missing `.throw` (steps 6-7 of
/// §7.4.13, then step 7.b.iii.6): a non-object result is its own TypeError,
/// otherwise the missing-method TypeError is thrown at the site.
fn delegate_closed(s: State, settled: JsVal) -> #(Step, Agent) {
  case rt_val.is_object(settled) {
    True -> throw_type_into(s, missing_throw)
    False -> throw_type_into(s, "Iterator result is not an object")
  }
}

// -- Return injection (§27.5.3.4 GeneratorResumeAbrupt, return) --------------
// Walk the parked body's try stack outwards, running each enclosing
// `finally` block and closing each live for-of / destructuring iterator
// (§7.4.9), until nothing intercepts the return.

/// The innermost try frame that must react to a return completion, with the
/// frames outside it.
type ReturnHandler {
  FinallyHandler(fin_pc: Int, stack_depth: Int, rest: List(bytecode.TryFrame))
  IterCloseHandler(stack_depth: Int, rest: List(bytecode.TryFrame))
}

fn find_return_handler(
  try_stack: List(bytecode.TryFrame),
) -> Option(ReturnHandler) {
  case try_stack {
    [] -> None
    [TryFrame(kind: Finally(fin_label: Pc(fin_pc)), stack_depth:, ..), ..rest] ->
      Some(FinallyHandler(fin_pc, stack_depth, rest))
    [TryFrame(kind: IterCloseGuard, stack_depth:, ..), ..rest] ->
      Some(IterCloseHandler(stack_depth, rest))
    [TryFrame(kind: CatchOnly, ..), ..rest] -> find_return_handler(rest)
  }
}

fn truncate_stack(stack: List(JsVal), depth: Int) -> List(JsVal) {
  let excess = list.length(stack) - depth
  case excess > 0 {
    True -> list.drop(stack, excess)
    False -> stack
  }
}

/// Deliver a return completion of `value` at the body's current point:
/// - nothing intercepts it: the body finishes with `value`;
/// - a `finally` runs and falls off its end: keep unwinding with what the
///   subroutine completed with (a `return x` inside it wins, §14.15.3);
/// - a `finally` parks or throws: that is the turn's outcome;
/// - an iterator close throws or yields a non-object: the return becomes
///   that throw, unwinding through the REMAINING frames.
fn return_into(s: State, value: JsVal) -> Outcome {
  case find_return_handler(s.try_stack) {
    None -> Finished(Ok(value), s)
    Some(IterCloseHandler(stack_depth, rest)) ->
      case truncate_stack(s.stack, stack_depth) {
        [slot, ..base] -> {
          let s = State(..s, try_stack: rest, stack: base)
          case classify(slot) {
            KHandle(_) -> close_for_return(s, slot, value)
            // The loop's [[Done]] path leaves a non-object in the slot:
            // nothing to close.
            _ -> return_into(s, value)
          }
        }
        [] -> return_into(State(..s, try_stack: rest, stack: []), value)
      }
    Some(FinallyHandler(fin_pc, stack_depth, rest)) -> {
      // Enter the finally subroutine with the gosub convention: stack =
      // [retpc, slot, ..base]; retpc -1 tells Ret to complete the frame
      // with the slot value.
      let base = truncate_stack(s.stack, stack_depth)
      let fin =
        State(
          ..s,
          try_stack: rest,
          stack: [mk_number(JInt(-1)), value, ..base],
          pc: fin_pc,
        )
      case execute(fin) {
        Finished(Ok(v), s) -> return_into(s, v)
        other -> other
      }
    }
  }
}

/// §7.4.9 IteratorClose(record, return completion) with the
/// normal-completion rules: a throwing or non-object `.return()` result
/// replaces the return with that throw, which the body's remaining handlers
/// may catch. A record already marked done is left alone.
fn close_for_return(s: State, record: JsVal, value: JsVal) -> Outcome {
  case call.guarded_unit(s, rt_lang.t_iter_close(_, record, False)) {
    Ok(s) -> return_into(s, value)
    Error(state.Threw(thrown, s)) -> throw_into(s, thrown)
    Error(state.Returned(v, s)) -> Finished(Ok(v), s)
    Error(state.Yielded(..) as exit)
    | Error(state.Awaited(..) as exit)
    | Error(state.VmFailed(..) as exit) -> {
      let #(res, s) =
        fault(
          exit_state(exit),
          state.InternalError("close_for_return", "unexpected step exit"),
        )
      Finished(res, s)
    }
  }
}

fn exit_state(exit: state.StepExit) -> State {
  case exit {
    state.Threw(_, s) -> s
    state.Returned(_, s) -> s
    state.Yielded(_, _, s) -> s
    state.Awaited(_, s) -> s
    state.VmFailed(_, s) -> s
  }
}
