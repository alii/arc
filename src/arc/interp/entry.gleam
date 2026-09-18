import arc/bytecode/opcode.{
  AsyncYieldStarNext, CatchOnly, Finally, IterCloseGuard, Pc, YieldStar,
}
import arc/internal/tuple_array
import arc/interp/call
import arc/interp/eval
import arc/interp/interpreter.{Completed, Suspended}
import arc/interp/kernel
import arc/interp/park
import arc/interp/state.{
  type State, type StepExit, InternalError, State, SuspensionLeak,
}
import arc/rt/async as rt_async
import arc/rt/builtins/iter_protocol
import arc/rt/bytecode.{
  type FuncTemplate, type ParkedAt, type SuspendedFrame, ParkedDelegateClose,
  ParkedDelegateReturn, ParkedOp, ParkedReturnValue, ParkedStart, TryFrame,
}
import arc/rt/call.{type Completion, NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/lang as rt_lang
import arc/rt/limits
import arc/rt/obj as rt_obj
import arc/rt/realm as rt_realm
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type EvalKind, type FrameInfo, type Handle, type IteratorRecord,
  type JsOps, type JsVal, type Step, Agent, BytecodeFn, JInt, JsOps, JsStore,
  KHandle, KNull, KUndef, Named, ResumeFrame, SObject, StepAwait, StepReturn,
  StepThrow, StepYield, StringKey, TypeErr, classify, mk_number, mk_object,
  mk_undefined,
}
import arc/rt/val as rt_val
import gleam/bool
import gleam/option.{type Option, None, Some}
import gleam/result

pub fn link(agent: Agent) -> Agent {
  let store = agent.store
  Agent(..agent, store: JsStore(..store, ops: linked_ops(store.ops)))
}

fn linked_ops(ops: JsOps(Agent)) -> JsOps(Agent) {
  JsOps(
    ..ops,
    eval_hook: eval_source,
    call_bytecode:,
    bind_call:,
    construct_bytecode:,
    resume_frame:,
  )
}

type Outcome {
  Finished(Result(JsVal, JsVal), State)
  Parked(state.SuspendKind, JsVal, State)
}

fn to_outcome(outcome: #(Result(JsVal, JsVal), State)) -> Outcome {
  Finished(outcome.0, outcome.1)
}

const drive = call.Drive(start_coroutine:)

fn execute(state: State) -> Outcome {
  case interpreter.execute(state, drive) {
    Ok(#(Completed(NormalCompletion(v)), s)) -> Finished(Ok(v), s)
    Ok(#(Completed(ThrowCompletion(e)), s)) -> Finished(Error(e), s)
    Ok(#(Suspended(kind, v), s)) -> Parked(kind, v, s)
    Error(err) -> to_outcome(state.internal_fault(state, err))
  }
}

fn complete(state: State, site: String) -> #(Result(JsVal, JsVal), State) {
  case execute(state) {
    Finished(res, s) -> #(res, s)
    Parked(kind, _, s) -> state.internal_fault(s, SuspensionLeak(site:, kind:))
  }
}

fn complete_call(state: State) -> #(Result(JsVal, JsVal), Agent) {
  case interpreter.execute(state, drive) {
    Ok(#(Completed(NormalCompletion(v)), s)) -> #(Ok(v), s.agent)
    Ok(#(Completed(ThrowCompletion(e)), s)) -> #(Error(e), s.agent)
    Ok(#(Suspended(kind, _), s)) -> {
      let #(res, s) =
        state.internal_fault(s, SuspensionLeak(site: "run_bytecode", kind:))
      #(res, s.agent)
    }
    Error(err) -> {
      let #(res, s) = state.internal_fault(state, err)
      #(res, s.agent)
    }
  }
}

type EntryMark {
  EntryMark(frames: List(FrameInfo), call_depth: Int)
}

fn mark(agent: Agent) -> EntryMark {
  EntryMark(frames: agent.frames, call_depth: agent.call_depth)
}

// restore entry frames and depth
fn settle(agent: Agent, m: EntryMark) -> Agent {
  resettle(agent, m.frames, m.call_depth)
}

// a callee that never synced left frames and depth untouched
fn resettle(agent: Agent, frames: List(FrameInfo), depth: Int) -> Agent {
  case agent.call_depth == depth && agent.frames == frames {
    True -> agent
    False -> Agent(..agent, frames:, call_depth: depth)
  }
}

fn backstopped(
  agent: Agent,
  m: EntryMark,
  body: fn(Agent) -> #(a, Agent),
  on_escape: fn(JsVal) -> a,
) -> #(a, Agent) {
  case kernel.guard1(body, agent) {
    kernel.Ok(value:, agent:) -> #(value, settle(agent, m))
    kernel.Threw(agent:, thrown:) -> #(on_escape(thrown), settle(agent, m))
  }
}

pub fn run(state: State) -> #(Result(JsVal, JsVal), Agent) {
  let m = mark(state.agent)
  let agent = call.push_frame_info(state.agent, state.func)
  let body = fn(agent) {
    let #(res, s) = complete(State(..state, agent:), "run")
    #(res, s.agent)
  }
  backstopped(agent, m, body, Error)
}

fn to_completion(res: Result(JsVal, JsVal)) -> Completion(JsVal) {
  case res {
    Ok(v) -> NormalCompletion(v)
    Error(e) -> ThrowCompletion(e)
  }
}

pub fn run_script(
  agent: Agent,
  template: FuncTemplate,
) -> #(Completion(JsVal), Agent) {
  let #(res, agent) = run(eval.script_activation(agent, template))
  #(to_completion(res), agent)
}

pub fn call_bytecode(
  st: Agent,
  fn_h: Handle,
  kind: types.ObjKind,
  this: JsVal,
  args: List(JsVal),
) -> #(Result(JsVal, JsVal), Agent) {
  let assert BytecodeFn(
    template:,
    env:,
    home_object:,
    flags:,
    realm:,
    unit:,
    ..,
  ) = kind
    as "call_bytecode: not a BytecodeFn kind"
  case st.call_depth >= limits.max_call_depth, realm == st.realm.id {
    True, _ -> depth_exceeded(st)
    False, True ->
      run_call(st, fn_h, template, env, home_object, flags, unit, this, args)
    False, False -> {
      use st <- rt_realm.with_realm(st, realm)
      run_call(st, fn_h, template, env, home_object, flags, unit, this, args)
    }
  }
}

pub fn bind_call(
  st: Agent,
  fn_h: Handle,
  kind: types.ObjKind,
  this: JsVal,
) -> fn(Agent, List(JsVal)) -> #(JsVal, Agent) {
  let assert BytecodeFn(
    template:,
    env:,
    home_object:,
    flags:,
    realm:,
    unit:,
    ..,
  ) = kind
    as "bind_call: not a BytecodeFn kind"
  case
    realm == st.realm.id
    && !template.is_generator
    && !template.is_async
    && !template.is_class_constructor
  {
    True -> {
      let callee =
        call.root_callee(fn_h, template, env, home_object, flags, unit)
      let new_target = mk_undefined()
      fn(st, args) { call_bound(st, callee, this, args, new_target) }
    }
    False -> fn(st, args) { raised(call_bytecode(st, fn_h, kind, this, args)) }
  }
}

fn call_bound(
  st: Agent,
  callee: call.RootCallee,
  this: JsVal,
  args: List(JsVal),
  new_target: JsVal,
) -> #(JsVal, Agent) {
  let frames = st.frames
  let depth = st.call_depth
  case depth >= limits.max_call_depth {
    True -> raised(depth_exceeded(st))
    False -> {
      let state = call.root_state(st, callee, this, args, new_target)
      case kernel.guard1(complete_call, state) {
        kernel.Ok(value: Ok(v), agent:) -> #(v, resettle(agent, frames, depth))
        kernel.Ok(value: Error(e), agent:) ->
          rt_store.t_throw(resettle(agent, frames, depth), e)
        kernel.Threw(agent:, thrown:) ->
          rt_store.t_throw(resettle(agent, frames, depth), thrown)
      }
    }
  }
}

fn raised(outcome: #(Result(JsVal, JsVal), Agent)) -> #(JsVal, Agent) {
  case outcome {
    #(Ok(v), st) -> #(v, st)
    #(Error(e), st) -> rt_store.t_throw(st, e)
  }
}

fn depth_exceeded(st: Agent) -> #(Result(JsVal, JsVal), Agent) {
  let #(e, st) = state.stack_overflow_error(st)
  #(Error(e), st)
}

fn run_call(
  st: Agent,
  fn_h: Handle,
  template: FuncTemplate,
  env: bytecode.EnvTuple,
  home_object: Option(Handle),
  flags: types.FnFlags,
  unit: Int,
  this: JsVal,
  args: List(JsVal),
) -> #(Result(JsVal, JsVal), Agent) {
  let callee = call.root_callee(fn_h, template, env, home_object, flags, unit)
  case template.is_generator || template.is_async {
    False -> run_plain_call(st, callee, this, args)
    True -> {
      let m = mark(st)
      case call.enter_root(st, callee, this, args, mk_undefined()) {
        Error(#(thrown, st)) -> #(Error(thrown), st)
        Ok(state) -> {
          let agent = state.agent
          let body = fn(agent) {
            let #(res, s) =
              start_coroutine_root(
                State(..state, agent:),
                call.root_coroutine(state, fn_h),
              )
            #(res, s.agent)
          }
          backstopped(agent, m, body, Error)
        }
      }
    }
  }
}

fn run_plain_call(
  st: Agent,
  callee: call.RootCallee,
  this: JsVal,
  args: List(JsVal),
) -> #(Result(JsVal, JsVal), Agent) {
  let frames = st.frames
  let depth = st.call_depth
  case call.enter_root(st, callee, this, args, mk_undefined()) {
    Error(#(thrown, st)) -> #(Error(thrown), st)
    Ok(state) ->
      case kernel.guard1(complete_call, state) {
        kernel.Ok(value:, agent:) -> #(value, resettle(agent, frames, depth))
        kernel.Threw(agent:, thrown:) -> #(
          Error(thrown),
          resettle(agent, frames, depth),
        )
      }
  }
}

pub fn construct_bytecode(
  st: Agent,
  fn_h: Handle,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  use <- bool.lazy_guard(st.call_depth >= limits.max_call_depth, fn() {
    let #(e, st) = state.stack_overflow_error(st)
    rt_store.t_throw(st, e)
  })
  let #(completion, st) = run_construct(st, fn_h, args, new_target)
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

fn run_construct(
  st: Agent,
  callee_h: Handle,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Completion(JsVal), Agent) {
  let assert SObject(
    kind: BytecodeFn(template:, env:, home_object:, flags:, realm:, unit:, ..),
    ..,
  ) = rt_store.t_cell_get(st, callee_h)
    as "construct_bytecode: handle is not a BytecodeFn cell"
  let m = mark(st)
  case call.root_this(st, template, new_target) {
    Error(#(thrown, st)) -> #(ThrowCompletion(thrown), settle(st, m))
    Ok(#(this, kind, st)) -> {
      let #(outcome, st) = {
        use st <- rt_realm.with_realm(st, realm)
        let callee =
          call.root_callee(callee_h, template, env, home_object, flags, unit)
        case call.enter_root(st, callee, this, args, new_target) {
          Error(#(thrown, agent)) -> #(
            RootSettled(ThrowCompletion(thrown)),
            agent,
          )
          Ok(state) -> {
            let body = fn(agent) {
              let #(res, s) = complete(State(..state, agent:), "run_bytecode")
              case res {
                Ok(v) -> #(RootReturned(v, s), s.agent)
                Error(e) -> #(RootSettled(ThrowCompletion(e)), s.agent)
              }
            }
            backstopped(state.agent, m, body, escaped)
          }
        }
      }
      case outcome {
        RootSettled(c) -> #(c, st)
        RootReturned(v, final) ->
          case call.finish_root(kind, v, State(..final, agent: st)) {
            Ok(#(v, agent)) -> #(NormalCompletion(v), agent)
            Error(#(e, agent)) -> #(ThrowCompletion(e), agent)
          }
      }
    }
  }
}

type RootOutcome {
  RootSettled(Completion(JsVal))
  RootReturned(JsVal, State)
}

fn escaped(thrown: JsVal) -> RootOutcome {
  RootSettled(ThrowCompletion(thrown))
}

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
    Error(state.VmFailed(err, s)) -> state.internal_fault(s, err)
    Error(state.Yielded(_, _, s)) ->
      state.internal_fault(s, SuspensionLeak("run_bytecode", state.Yield))
    Error(state.Awaited(_, s)) ->
      state.internal_fault(s, SuspensionLeak("run_bytecode", state.Await))
  }
}

fn start_coroutine(
  caller: State,
  c: call.CoroutineCall,
) -> Result(State, StepExit) {
  let call.CoroutineCall(
    fn_h:,
    template:,
    unit:,
    locals:,
    this:,
    home_object:,
    args:,
    rest_stack:,
  ) = c
  let caller = State(..caller, stack: rest_stack)
  let m = mark(caller.agent)
  use agent <- nested(caller)
  let body =
    State(
      agent: call.push_frame_info(agent, template),
      pc: 0,
      stack: [],
      locals:,
      func: template,
      unit:,
      call_stack: [],
      outer_depth: agent.call_depth,
      depth: agent.call_depth,
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
      let frame = park.park(body, ParkedStart)
      case kernel.guard2(rt_async.t_async_run, agent, ResumeFrame(frame)) {
        kernel.Ok(value: promise, agent:) -> resume(agent, mk_object(promise))
        kernel.Threw(agent:, thrown:) -> threw(agent, thrown)
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
        Finished(Ok(_), s) | Parked(state.Await, _, s) ->
          Error(state.VmFailed(
            InternalError("start_coroutine", "body missed InitialYield"),
            State(..caller, agent: settle(s.agent, m)),
          ))
      }
  }
}

// caller frame is not a gc root while nested runs
fn nested(
  caller: State,
  k: fn(Agent) -> Result(State, StepExit),
) -> Result(State, StepExit) {
  let agent = caller.agent
  case agent.call_depth >= limits.max_call_depth {
    True -> state.throw_stack_overflow(caller)
    False -> k(Agent(..agent, call_depth: agent.call_depth + 1))
  }
}

pub fn eval_source(
  st: Agent,
  source: String,
  kind: EvalKind,
) -> #(JsVal, Agent) {
  eval.eval_hook(st, source, kind, run)
}

// mode 0 next, 1 throw, 2 return
pub fn resume_frame(
  st: Agent,
  frame: SuspendedFrame,
  sent: #(Int, JsVal),
) -> #(Step, Agent) {
  use st <- rt_realm.with_realm(st, frame.realm)
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
      _, 1 -> step_of(throw_into(s, value))
      _, _ -> step_of(return_into(s, value))
    }
  }
  backstopped(agent, m, turn, StepThrow)
}

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

fn await_at(s: State, v: JsVal, parked: ParkedAt) -> #(Step, Agent) {
  #(StepAwait(v, ResumeFrame(park.park(s, parked))), s.agent)
}

pub fn run_turn(state: State) -> #(Step, Agent) {
  let m = mark(state.agent)
  let agent = call.push_frame_info(state.agent, state.func)
  let turn = fn(agent) { step_of(execute(State(..state, agent:))) }
  backstopped(agent, m, turn, StepThrow)
}

fn throw_into(s: State, thrown: JsVal) -> Outcome {
  case interpreter.unwind_to_catch(s, thrown) {
    Some(caught) -> execute(caught)
    None -> Finished(Error(thrown), s)
  }
}

fn throw_type_into(s: State, msg: String) -> #(Step, Agent) {
  let #(e, s) = state.new_error(s, TypeErr, msg)
  step_of(throw_into(s, e))
}

// §27.5.3.8 step 7.b/7.c yield* delegation

const missing_throw = "The iterator does not provide a 'throw' method."

type DelegateSite {
  SyncSite(record: IteratorRecord, rest: List(JsVal))
  AsyncSite(record: IteratorRecord, rest: List(JsVal), await_pc: Int)
}

fn delegate_site(s: State) -> Option(DelegateSite) {
  case tuple_array.get_unchecked(s.pc, s.func.bytecode), s.stack {
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

fn delegate_method(
  s: State,
  site: DelegateSite,
  name: String,
) -> Result(#(Option(JsVal), State), StepExit) {
  let iterator = site_record(site).iterator
  use #(method, s) <- result.map(kernel.guarded(
    kernel.guard3(rt_obj.t_get_prop, s.agent, iterator, StringKey(Named(name))),
    s,
  ))
  case classify(method) {
    KUndef | KNull -> #(None, s)
    _ -> #(Some(method), s)
  }
}

fn call_delegate(
  s: State,
  site: DelegateSite,
  method: JsVal,
  value: JsVal,
) -> Result(#(JsVal, State), StepExit) {
  let iterator = site_record(site).iterator
  kernel.guarded(
    kernel.guard4(rt_call.t_call_checked, s.agent, method, iterator, [value]),
    s,
  )
}

fn inject_throw(s: State, thrown: JsVal) -> #(Step, Agent) {
  case delegate_site(s) {
    None -> step_of(throw_into(s, thrown))
    Some(site) -> forward_throw(s, site, thrown)
  }
}

fn inject_return(s: State, value: JsVal) -> #(Step, Agent) {
  case delegate_site(s) {
    None -> step_of(return_into(s, value))
    Some(site) -> forward_return(s, site, value)
  }
}

fn delegate_exit(exit: StepExit) -> #(Step, Agent) {
  case exit {
    state.Threw(thrown, s) -> step_of(throw_into(s, thrown))
    state.Returned(_, s)
    | state.Yielded(_, _, s)
    | state.Awaited(_, s)
    | state.VmFailed(_, s) ->
      step_of(to_outcome(unexpected_exit(s, "yield* delegate")))
  }
}

fn unexpected_exit(s: State, site: String) -> #(Result(JsVal, JsVal), State) {
  state.internal_fault(s, InternalError(site, "unexpected step exit"))
}

fn or_delegate_exit(
  res: Result(a, StepExit),
  k: fn(a) -> #(Step, Agent),
) -> #(Step, Agent) {
  case res {
    Ok(v) -> k(v)
    Error(exit) -> delegate_exit(exit)
  }
}

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
      use #(closed, s) <- or_delegate_exit(
        call.guarded(s, iter_protocol.call_return(_, record.iterator)),
      )
      case closed {
        Ok(iter_protocol.NoReturnMethod) -> throw_type_into(s, missing_throw)
        Ok(iter_protocol.Returned(result)) ->
          await_at(s, result, ParkedDelegateClose)
        Error(thrown) -> step_of(throw_into(s, thrown))
      }
    }
  }
}

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

fn delegate_result(
  s: State,
  res: JsVal,
  rest: List(JsVal),
  on_done: fn(State, JsVal) -> #(Step, Agent),
) -> #(Step, Agent) {
  use #(#(done, val), s) <- or_delegate_exit(kernel.guarded(
    kernel.guard2(iter_protocol.read_iter_result, s.agent, res),
    s,
  ))
  case done {
    False -> step_of(Parked(state.Yield, val, s))
    True -> on_done(State(..s, stack: rest), val)
  }
}

fn delegate_returned(s: State, settled: JsVal) -> #(Step, Agent) {
  let rest = case s.stack {
    [_rec, ..rest] -> rest
    [] -> []
  }
  delegate_result(s, settled, rest, fn(s, val) { step_of(return_into(s, val)) })
}

fn delegate_closed(s: State, settled: JsVal) -> #(Step, Agent) {
  case rt_val.is_object(settled) {
    True -> throw_type_into(s, missing_throw)
    False -> throw_type_into(s, "Iterator result is not an object")
  }
}

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

// §27.5.3.4 return: run finallys, close iterators outwards
fn return_into(s: State, value: JsVal) -> Outcome {
  case find_return_handler(s.try_stack) {
    None -> Finished(Ok(value), s)
    Some(IterCloseHandler(stack_depth, rest)) ->
      case state.truncate_stack(s.stack, stack_depth) {
        [iter, ..base] -> {
          let s = State(..s, try_stack: rest, stack: base)
          case interpreter.closable_record(s, iter) {
            Ok(#(iter, s)) ->
              case classify(iter) {
                KHandle(_) -> close_for_return(s, iter, value)
                _ -> return_into(s, value)
              }
            Error(exit) -> exit_outcome(exit, "return_into")
          }
        }
        [] -> return_into(State(..s, try_stack: rest, stack: []), value)
      }
    Some(FinallyHandler(fin_pc, stack_depth, rest)) -> {
      let base = state.truncate_stack(s.stack, stack_depth)
      let fin =
        State(
          ..s,
          try_stack: rest,
          // retpc -1 tells Ret to complete with the value
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

fn close_for_return(s: State, record: JsVal, value: JsVal) -> Outcome {
  case call.guarded_unit(s, rt_lang.t_iter_close(_, record, False)) {
    Ok(s) -> return_into(s, value)
    Error(exit) -> exit_outcome(exit, "close_for_return")
  }
}

// a step exit met while completing a return outside the loop
fn exit_outcome(exit: StepExit, site: String) -> Outcome {
  case exit {
    state.Threw(thrown, s) -> throw_into(s, thrown)
    state.Returned(v, s) -> Finished(Ok(v), s)
    state.Yielded(_, _, s) | state.Awaited(_, s) | state.VmFailed(_, s) ->
      to_outcome(unexpected_exit(s, site))
  }
}
