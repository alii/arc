import arc/bytecode/error_kind.{TypeError}
import arc/bytecode/key.{Named}
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
  type JsOps, type JsVal, type Step, Agent, BytecodeFn, JsOps, KHandle, KNull,
  KUndef, ResumeFrame, SObject, StepAwait, StepReturn, StepThrow, StepYield,
  Store, StringKey, classify, mk_int, mk_object, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/bool
import gleam/option.{type Option, None, Some}
import gleam/result

pub fn link(agent: Agent) -> Agent {
  let store = agent.store
  Agent(..agent, store: Store(..store, ops: linked_ops(store.ops)))
}

fn linked_ops(ops: JsOps) -> JsOps {
  JsOps(
    ..ops,
    eval_hook: eval_source,
    call_bytecode:,
    prepare_call:,
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
    Ok(#(Completed(NormalCompletion(v)), state)) -> Finished(Ok(v), state)
    Ok(#(Completed(ThrowCompletion(e)), state)) -> Finished(Error(e), state)
    Ok(#(Suspended(kind, v), state)) -> Parked(kind, v, state)
    Error(err) -> to_outcome(state.internal_fault(state, err))
  }
}

fn complete(state: State, site: String) -> #(Result(JsVal, JsVal), State) {
  case execute(state) {
    Finished(res, state) -> #(res, state)
    Parked(kind, _, state) ->
      state.internal_fault(state, SuspensionLeak(site:, kind:))
  }
}

fn complete_call(state: State) -> #(Result(JsVal, JsVal), Agent) {
  case interpreter.execute(state, drive) {
    Ok(#(Completed(NormalCompletion(v)), state)) -> #(Ok(v), state.agent)
    Ok(#(Completed(ThrowCompletion(e)), state)) -> #(Error(e), state.agent)
    Ok(#(Suspended(kind, _), state)) -> {
      let #(res, state) =
        state.internal_fault(state, SuspensionLeak(site: "run_bytecode", kind:))
      #(res, state.agent)
    }
    Error(err) -> {
      let #(res, state) = state.internal_fault(state, err)
      #(res, state.agent)
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
    let #(res, state) = complete(State(..state, agent:), "run")
    #(res, state.agent)
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
  agent: Agent,
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
    unit_id:,
    ..,
  ) = kind
    as "call_bytecode: not a BytecodeFn kind"
  case agent.call_depth >= limits.max_call_depth, realm == agent.realm.id {
    True, _ -> depth_exceeded(agent)
    False, True ->
      run_call(
        agent,
        fn_h,
        template,
        env,
        home_object,
        flags,
        unit_id,
        this,
        args,
      )
    False, False -> {
      use agent <- rt_realm.with_realm(agent, realm)
      run_call(
        agent,
        fn_h,
        template,
        env,
        home_object,
        flags,
        unit_id,
        this,
        args,
      )
    }
  }
}

pub fn prepare_call(
  agent: Agent,
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
    unit_id:,
    ..,
  ) = kind
    as "prepare_call: not a BytecodeFn kind"
  case
    realm == agent.realm.id
    && !template.is_generator
    && !template.is_async
    && !template.is_class_constructor
  {
    True -> {
      let callee =
        call.root_callee(fn_h, template, env, home_object, flags, unit_id)
      let new_target = mk_undefined()
      fn(agent, args) { call_prepared(agent, callee, this, args, new_target) }
    }
    False -> fn(agent, args) {
      raised(call_bytecode(agent, fn_h, kind, this, args))
    }
  }
}

fn call_prepared(
  agent: Agent,
  callee: call.RootCallee,
  this: JsVal,
  args: List(JsVal),
  new_target: JsVal,
) -> #(JsVal, Agent) {
  let frames = agent.frames
  let depth = agent.call_depth
  case depth >= limits.max_call_depth {
    True -> raised(depth_exceeded(agent))
    False -> {
      let state = call.root_state(agent, callee, this, args, new_target)
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
    #(Ok(v), agent) -> #(v, agent)
    #(Error(e), agent) -> rt_store.t_throw(agent, e)
  }
}

fn depth_exceeded(agent: Agent) -> #(Result(JsVal, JsVal), Agent) {
  let #(e, agent) = state.stack_overflow_error(agent)
  #(Error(e), agent)
}

fn run_call(
  agent: Agent,
  fn_h: Handle,
  template: FuncTemplate,
  env: bytecode.EnvTuple,
  home_object: Option(Handle),
  flags: types.FnFlags,
  unit_id: Int,
  this: JsVal,
  args: List(JsVal),
) -> #(Result(JsVal, JsVal), Agent) {
  let callee =
    call.root_callee(fn_h, template, env, home_object, flags, unit_id)
  case template.is_generator || template.is_async {
    False -> run_plain_call(agent, callee, this, args)
    True -> {
      let m = mark(agent)
      case call.enter_root(agent, callee, this, args, mk_undefined()) {
        Error(#(thrown, agent)) -> #(Error(thrown), agent)
        Ok(state) -> {
          let agent = state.agent
          let body = fn(agent) {
            let #(res, state) =
              start_coroutine_root(
                State(..state, agent:),
                call.root_coroutine(state, fn_h),
              )
            #(res, state.agent)
          }
          backstopped(agent, m, body, Error)
        }
      }
    }
  }
}

fn run_plain_call(
  agent: Agent,
  callee: call.RootCallee,
  this: JsVal,
  args: List(JsVal),
) -> #(Result(JsVal, JsVal), Agent) {
  let frames = agent.frames
  let depth = agent.call_depth
  case call.enter_root(agent, callee, this, args, mk_undefined()) {
    Error(#(thrown, agent)) -> #(Error(thrown), agent)
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
  agent: Agent,
  fn_h: Handle,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  use <- bool.lazy_guard(agent.call_depth >= limits.max_call_depth, fn() {
    let #(e, agent) = state.stack_overflow_error(agent)
    rt_store.t_throw(agent, e)
  })
  let #(completion, agent) = run_construct(agent, fn_h, args, new_target)
  let #(v, agent) = case completion {
    NormalCompletion(v) -> #(v, agent)
    ThrowCompletion(e) -> rt_store.t_throw(agent, e)
  }
  case classify(v) {
    KHandle(h) -> #(h, agent)
    _ -> {
      let #(e, agent) =
        rt_val.t_new_error(
          agent,
          TypeError,
          "internal error: constructor completed with a non-object",
        )
      rt_store.t_throw(agent, e)
    }
  }
}

fn run_construct(
  agent: Agent,
  callee_h: Handle,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Completion(JsVal), Agent) {
  let assert SObject(
    kind: BytecodeFn(
      template:,
      env:,
      home_object:,
      flags:,
      realm:,
      unit_id:,
      ..,
    ),
    ..,
  ) = rt_store.t_cell_get(agent, callee_h)
    as "construct_bytecode: handle is not a BytecodeFn cell"
  let m = mark(agent)
  case call.root_this(agent, template, new_target) {
    Error(#(thrown, agent)) -> #(ThrowCompletion(thrown), settle(agent, m))
    Ok(#(this, kind, agent)) -> {
      let #(outcome, agent) = {
        use agent <- rt_realm.with_realm(agent, realm)
        let callee =
          call.root_callee(callee_h, template, env, home_object, flags, unit_id)
        case call.enter_root(agent, callee, this, args, new_target) {
          Error(#(thrown, agent)) -> #(
            RootSettled(ThrowCompletion(thrown)),
            agent,
          )
          Ok(state) -> {
            let body = fn(agent) {
              let #(res, state) =
                complete(State(..state, agent:), "run_bytecode")
              case res {
                Ok(v) -> #(RootReturned(v, state), state.agent)
                Error(e) -> #(RootSettled(ThrowCompletion(e)), state.agent)
              }
            }
            backstopped(state.agent, m, body, escaped)
          }
        }
      }
      case outcome {
        RootSettled(c) -> #(c, agent)
        RootReturned(v, final) ->
          case call.finish_root(kind, v, State(..final, agent:)) {
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
  state: State,
  coroutine: call.CoroutineCall,
) -> #(Result(JsVal, JsVal), State) {
  case start_coroutine(state, coroutine) {
    Ok(state) ->
      case state.stack {
        [v, ..] -> #(Ok(v), state)
        [] -> #(Ok(mk_undefined()), state)
      }
    Error(state.Threw(e, state)) -> #(Error(e), state)
    Error(state.Returned(v, state)) -> #(Ok(v), state)
    Error(state.VmFailed(err, state)) -> state.internal_fault(state, err)
    Error(state.Yielded(_, _, state)) ->
      state.internal_fault(state, SuspensionLeak("run_bytecode", state.Yield))
    Error(state.Awaited(_, state)) ->
      state.internal_fault(state, SuspensionLeak("run_bytecode", state.Await))
  }
}

fn start_coroutine(
  caller: State,
  c: call.CoroutineCall,
) -> Result(State, StepExit) {
  let call.CoroutineCall(
    fn_h:,
    template:,
    unit_id:,
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
      unit_id:,
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
      case kernel.guard2(rt_async.t_run, agent, ResumeFrame(frame)) {
        kernel.Ok(value: promise, agent:) -> resume(agent, mk_object(promise))
        kernel.Threw(agent:, thrown:) -> threw(agent, thrown)
      }
    }
    True ->
      case execute(body) {
        Parked(state.Yield, _, state) -> {
          let frame = ResumeFrame(park.park(state, ParkedStart))
          let agent = settle(state.agent, m)
          let #(obj, agent) = case template.is_async {
            False -> rt_async.t_gen_new(agent, callee, frame)
            True -> rt_async.t_asyncgen_new(agent, callee, frame)
          }
          resume(agent, mk_object(obj))
        }
        Finished(Error(thrown), state) -> threw(state.agent, thrown)
        Finished(Ok(_), state) | Parked(state.Await, _, state) ->
          Error(state.VmFailed(
            InternalError("start_coroutine", "body missed InitialYield"),
            State(..caller, agent: settle(state.agent, m)),
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
  agent: Agent,
  source: String,
  kind: EvalKind,
) -> #(JsVal, Agent) {
  eval.hook(agent, source, kind, run)
}

// mode 0 next, 1 throw, 2 return
pub fn resume_frame(
  agent: Agent,
  frame: SuspendedFrame,
  sent: #(Int, JsVal),
) -> #(Step, Agent) {
  use agent <- rt_realm.with_realm(agent, frame.realm)
  let m = mark(agent)
  let agent = call.push_frame_info(agent, frame.template)
  let #(mode, value) = sent
  let turn = fn(agent) {
    let state = park.unpark(agent, frame)
    case frame.parked, mode {
      ParkedStart, m if m == rt_async.sent_next -> step_of(execute(state))
      ParkedOp, m if m == rt_async.sent_next ->
        step_of(execute(State(..state, stack: [value, ..state.stack])))
      ParkedOp, m if m == rt_async.sent_throw -> inject_throw(state, value)
      ParkedOp, _ -> inject_return(state, value)
      ParkedDelegateReturn, m if m == rt_async.sent_next ->
        delegate_returned(state, value)
      ParkedReturnValue, m if m == rt_async.sent_next ->
        step_of(return_into(state, value))
      ParkedDelegateClose, m if m == rt_async.sent_next ->
        delegate_closed(state, value)
      _, m if m == rt_async.sent_throw -> step_of(throw_into(state, value))
      _, _ -> step_of(return_into(state, value))
    }
  }
  backstopped(agent, m, turn, StepThrow)
}

fn step_of(outcome: Outcome) -> #(Step, Agent) {
  case outcome {
    Finished(Ok(v), state) -> #(StepReturn(v), state.agent)
    Finished(Error(e), state) -> #(StepThrow(e), state.agent)
    Parked(state.Yield, v, state) -> #(
      StepYield(v, ResumeFrame(park.park(state, ParkedOp))),
      state.agent,
    )
    Parked(state.Await, v, state) -> await_at(state, v, ParkedOp)
  }
}

fn await_at(state: State, v: JsVal, parked: ParkedAt) -> #(Step, Agent) {
  #(StepAwait(v, ResumeFrame(park.park(state, parked))), state.agent)
}

pub fn run_turn(state: State) -> #(Step, Agent) {
  let m = mark(state.agent)
  let agent = call.push_frame_info(state.agent, state.func)
  let turn = fn(agent) { step_of(execute(State(..state, agent:))) }
  backstopped(agent, m, turn, StepThrow)
}

fn throw_into(state: State, thrown: JsVal) -> Outcome {
  case interpreter.unwind_to_catch(state, thrown) {
    Some(caught) -> execute(caught)
    None -> Finished(Error(thrown), state)
  }
}

fn throw_type_into(state: State, msg: String) -> #(Step, Agent) {
  let #(e, state) = state.new_error(state, TypeError, msg)
  step_of(throw_into(state, e))
}

// §27.5.3.8 step 7.b/7.c yield* delegation

const missing_throw = "The iterator does not provide a 'throw' method."

type DelegateSite {
  SyncSite(record: IteratorRecord, rest: List(JsVal))
  AsyncSite(record: IteratorRecord, rest: List(JsVal), await_pc: Int)
}

fn delegate_site(state: State) -> Option(DelegateSite) {
  case tuple_array.get_unchecked(state.pc, state.func.bytecode), state.stack {
    YieldStar, [rec, ..rest] ->
      rt_lang.record_parts(state.agent, rec)
      |> option.map(SyncSite(_, rest))
    AsyncYieldStarNext(..), [rec, ..rest] ->
      rt_lang.record_parts(state.agent, rec)
      |> option.map(AsyncSite(_, rest, state.pc + 1))
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
  state: State,
  site: DelegateSite,
  name: String,
) -> Result(#(Option(JsVal), State), StepExit) {
  let iterator = site_record(site).iterator
  use #(method, state) <- result.map(kernel.guarded(
    kernel.guard3(
      rt_obj.t_get_prop,
      state.agent,
      iterator,
      StringKey(Named(name)),
    ),
    state,
  ))
  case classify(method) {
    KUndef | KNull -> #(None, state)
    _ -> #(Some(method), state)
  }
}

fn call_delegate(
  state: State,
  site: DelegateSite,
  method: JsVal,
  value: JsVal,
) -> Result(#(JsVal, State), StepExit) {
  let iterator = site_record(site).iterator
  kernel.guarded(
    kernel.guard4(rt_call.t_call, state.agent, method, iterator, [value]),
    state,
  )
}

fn inject_throw(state: State, thrown: JsVal) -> #(Step, Agent) {
  case delegate_site(state) {
    None -> step_of(throw_into(state, thrown))
    Some(site) -> forward_throw(state, site, thrown)
  }
}

fn inject_return(state: State, value: JsVal) -> #(Step, Agent) {
  case delegate_site(state) {
    None -> step_of(return_into(state, value))
    Some(site) -> forward_return(state, site, value)
  }
}

fn delegate_exit(exit: StepExit) -> #(Step, Agent) {
  case exit {
    state.Threw(thrown, state) -> step_of(throw_into(state, thrown))
    state.Returned(_, state)
    | state.Yielded(_, _, state)
    | state.Awaited(_, state)
    | state.VmFailed(_, state) ->
      step_of(to_outcome(unexpected_exit(state, "yield* delegate")))
  }
}

fn unexpected_exit(
  state: State,
  site: String,
) -> #(Result(JsVal, JsVal), State) {
  state.internal_fault(state, InternalError(site, "unexpected step exit"))
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
  state: State,
  site: DelegateSite,
  thrown: JsVal,
) -> #(Step, Agent) {
  use #(method, state) <- or_delegate_exit(delegate_method(state, site, "throw"))
  case method, site {
    Some(method), SyncSite(rest:, ..) -> {
      use #(res, state) <- or_delegate_exit(call_delegate(
        state,
        site,
        method,
        thrown,
      ))
      delegate_result(state, res, rest, fn(state, val) {
        step_of(execute(State(..state, stack: [val, ..rest], pc: state.pc + 1)))
      })
    }
    Some(method), AsyncSite(await_pc:, ..) -> {
      use #(res, state) <- or_delegate_exit(call_delegate(
        state,
        site,
        method,
        thrown,
      ))
      step_of(execute(State(..state, stack: [res, ..state.stack], pc: await_pc)))
    }
    None, SyncSite(record:, ..) -> {
      use state <- or_delegate_exit(
        call.guarded_unit(state, iter_protocol.iterator_close_normal(
          _,
          record.iterator,
        )),
      )
      throw_type_into(state, missing_throw)
    }
    None, AsyncSite(record:, ..) -> {
      use #(closed, state) <- or_delegate_exit(
        call.guarded(state, iter_protocol.call_return(_, record.iterator)),
      )
      case closed {
        Ok(iter_protocol.NoReturnMethod) ->
          throw_type_into(state, missing_throw)
        Ok(iter_protocol.Returned(result)) ->
          await_at(state, result, ParkedDelegateClose)
        Error(thrown) -> step_of(throw_into(state, thrown))
      }
    }
  }
}

fn forward_return(
  state: State,
  site: DelegateSite,
  value: JsVal,
) -> #(Step, Agent) {
  use #(method, state) <- or_delegate_exit(delegate_method(
    state,
    site,
    "return",
  ))
  case method, site {
    None, SyncSite(..) -> step_of(return_into(state, value))
    None, AsyncSite(..) -> await_at(state, value, ParkedReturnValue)
    Some(method), SyncSite(rest:, ..) -> {
      use #(res, state) <- or_delegate_exit(call_delegate(
        state,
        site,
        method,
        value,
      ))
      delegate_result(state, res, rest, fn(state, val) {
        step_of(return_into(state, val))
      })
    }
    Some(method), AsyncSite(..) -> {
      use #(res, state) <- or_delegate_exit(call_delegate(
        state,
        site,
        method,
        value,
      ))
      await_at(state, res, ParkedDelegateReturn)
    }
  }
}

fn delegate_result(
  state: State,
  res: JsVal,
  rest: List(JsVal),
  on_done: fn(State, JsVal) -> #(Step, Agent),
) -> #(Step, Agent) {
  use #(#(done, val), state) <- or_delegate_exit(kernel.guarded(
    kernel.guard2(iter_protocol.read_iter_result, state.agent, res),
    state,
  ))
  case done {
    False -> step_of(Parked(state.Yield, val, state))
    True -> on_done(State(..state, stack: rest), val)
  }
}

fn delegate_returned(state: State, settled: JsVal) -> #(Step, Agent) {
  let rest = case state.stack {
    [_rec, ..rest] -> rest
    [] -> []
  }
  delegate_result(state, settled, rest, fn(state, val) {
    step_of(return_into(state, val))
  })
}

fn delegate_closed(state: State, settled: JsVal) -> #(Step, Agent) {
  case rt_val.is_object(settled) {
    True -> throw_type_into(state, missing_throw)
    False -> throw_type_into(state, "Iterator result is not an object")
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
fn return_into(state: State, value: JsVal) -> Outcome {
  case find_return_handler(state.try_stack) {
    None -> Finished(Ok(value), state)
    Some(IterCloseHandler(stack_depth, rest)) ->
      case state.truncate_stack(state.stack, stack_depth) {
        [iter, ..base] -> {
          let state = State(..state, try_stack: rest, stack: base)
          case interpreter.closable_record(state, iter) {
            Ok(#(iter, state)) ->
              case classify(iter) {
                KHandle(_) -> close_for_return(state, iter, value)
                _ -> return_into(state, value)
              }
            Error(exit) -> exit_outcome(exit, "return_into")
          }
        }
        [] -> return_into(State(..state, try_stack: rest, stack: []), value)
      }
    Some(FinallyHandler(fin_pc, stack_depth, rest)) -> {
      let base = state.truncate_stack(state.stack, stack_depth)
      let fin =
        State(
          ..state,
          try_stack: rest,
          stack: [mk_int(bytecode.return_retpc), value, ..base],
          pc: fin_pc,
        )
      case execute(fin) {
        Finished(Ok(v), state) -> return_into(state, v)
        other -> other
      }
    }
  }
}

fn close_for_return(state: State, record: JsVal, value: JsVal) -> Outcome {
  case
    call.guarded_unit(state, rt_lang.t_iter_close(_, record, abrupt: False))
  {
    Ok(state) -> return_into(state, value)
    Error(exit) -> exit_outcome(exit, "close_for_return")
  }
}

// a step exit met while completing a return outside the loop
fn exit_outcome(exit: StepExit, site: String) -> Outcome {
  case exit {
    state.Threw(thrown, state) -> throw_into(state, thrown)
    state.Returned(v, state) -> Finished(Ok(v), state)
    state.Yielded(_, _, state)
    | state.Awaited(_, state)
    | state.VmFailed(_, state) -> to_outcome(unexpected_exit(state, site))
  }
}
