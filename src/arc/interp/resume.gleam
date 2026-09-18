import arc/bytecode/error_kind.{TypeError}
import arc/bytecode/key.{Named}
import arc/bytecode/opcode.{
  AsyncYieldStarNext, CatchOnly, Finally, IterCloseGuard, Pc, YieldStar,
}
import arc/internal/tuple_array
import arc/interp/call
import arc/interp/frames
import arc/interp/guard
import arc/interp/interpreter.{Completed, Suspended}
import arc/interp/park
import arc/interp/state.{
  type State, type StepExit, InternalError, State, SuspensionLeak,
}
import arc/rt/async as rt_async
import arc/rt/builtins/iter_protocol
import arc/rt/bytecode.{
  type ParkedAt, type SuspendedFrame, ParkedDelegateClose, ParkedDelegateReturn,
  ParkedOp, ParkedReturnValue, ParkedStart, TryFrame,
}
import arc/rt/call.{NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/lang as rt_lang
import arc/rt/limits
import arc/rt/obj as rt_obj
import arc/rt/realm as rt_realm
import arc/rt/types.{
  type Agent, type IteratorRecord, type JsVal, type Step, Agent, KHandle, KNull,
  KUndef, ResumeFrame, StepAwait, StepReturn, StepThrow, StepYield, StringKey,
  classify, mk_int, mk_object, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/option.{type Option, None, Some}
import gleam/result

pub type Executed {
  Finished(Result(JsVal, JsVal), State)
  Parked(state.SuspendKind, JsVal, State)
}

fn to_executed(outcome: #(Result(JsVal, JsVal), State)) -> Executed {
  Finished(outcome.0, outcome.1)
}

pub const drive = call.Drive(start_coroutine:)

pub fn executed(state: State) -> Executed {
  case interpreter.execute(state, drive) {
    Ok(#(Completed(NormalCompletion(v)), state)) -> Finished(Ok(v), state)
    Ok(#(Completed(ThrowCompletion(e)), state)) -> Finished(Error(e), state)
    Ok(#(Suspended(kind, v), state)) -> Parked(kind, v, state)
    Error(err) -> to_executed(state.internal_fault(state, err))
  }
}

pub fn complete(state: State, site: String) -> #(Result(JsVal, JsVal), State) {
  case executed(state) {
    Finished(res, state) -> #(res, state)
    Parked(kind, _, state) ->
      state.internal_fault(state, SuspensionLeak(site:, kind:))
  }
}

pub fn complete_call(state: State) -> #(Result(JsVal, JsVal), Agent) {
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

pub fn backstopped(
  agent: Agent,
  m: frames.EntryMark,
  body: fn(Agent) -> #(a, Agent),
  on_escape: fn(JsVal) -> a,
) -> #(a, Agent) {
  case guard.guard1(body, agent) {
    guard.Value(value:, agent:) -> #(value, frames.settle(agent, m))
    guard.Thrown(agent:, thrown:) -> #(
      on_escape(thrown),
      frames.settle(agent, m),
    )
  }
}

pub fn start_coroutine_root(
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
  let m = frames.mark(caller.agent)
  use agent <- with_call_depth(caller)
  let body =
    State(
      agent: frames.push_frame_info(agent, template),
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
    let agent = frames.settle(agent, m)
    Ok(State(..caller, agent:, stack: [value, ..rest_stack], pc: caller.pc + 1))
  }
  let threw = fn(agent: Agent, thrown: JsVal) {
    Error(state.Threw(thrown, State(..caller, agent: frames.settle(agent, m))))
  }
  case template.is_generator {
    False -> {
      let frame = park.park(body, ParkedStart)
      case guard.guard2(rt_async.run, agent, ResumeFrame(frame)) {
        guard.Value(value: promise, agent:) -> resume(agent, mk_object(promise))
        guard.Thrown(agent:, thrown:) -> threw(agent, thrown)
      }
    }
    True ->
      case executed(body) {
        Parked(state.Yield, _, state) -> {
          let frame = ResumeFrame(park.park(state, ParkedStart))
          let agent = frames.settle(state.agent, m)
          let #(obj, agent) = case template.is_async {
            False -> rt_async.gen_new(agent, callee, frame)
            True -> rt_async.asyncgen_new(agent, callee, frame)
          }
          resume(agent, mk_object(obj))
        }
        Finished(Error(thrown), state) -> threw(state.agent, thrown)
        Finished(Ok(_), state) | Parked(state.Await, _, state) ->
          Error(state.VmFailed(
            InternalError("start_coroutine", "body missed InitialYield"),
            State(..caller, agent: frames.settle(state.agent, m)),
          ))
      }
  }
}

// caller frame is not a gc root while the callee runs
fn with_call_depth(
  caller: State,
  k: fn(Agent) -> Result(State, StepExit),
) -> Result(State, StepExit) {
  let agent = caller.agent
  case agent.call_depth >= limits.max_call_depth {
    True -> state.throw_stack_overflow(caller)
    False -> k(Agent(..agent, call_depth: agent.call_depth + 1))
  }
}

// mode 0 next, 1 throw, 2 return
pub fn resume_frame(
  agent: Agent,
  frame: SuspendedFrame,
  sent: #(Int, JsVal),
) -> #(Step, Agent) {
  use agent <- rt_realm.with_realm(agent, frame.realm)
  let m = frames.mark(agent)
  let agent = frames.push_frame_info(agent, frame.template)
  let #(mode, value) = sent
  let turn = fn(agent) {
    let state = park.unpark(agent, frame)
    case frame.parked, mode {
      ParkedStart, m if m == rt_async.sent_next -> step_of(executed(state))
      ParkedOp, m if m == rt_async.sent_next ->
        step_of(executed(State(..state, stack: [value, ..state.stack])))
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

pub fn step_of(outcome: Executed) -> #(Step, Agent) {
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

fn throw_into(state: State, thrown: JsVal) -> Executed {
  case interpreter.unwind_to_catch(state, thrown) {
    Some(caught) -> executed(caught)
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
  use #(method, state) <- result.map(guard.guard_state(
    guard.guard3(rt_obj.get_prop, state.agent, iterator, StringKey(Named(name))),
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
  guard.guard_state(
    guard.guard4(rt_call.call, state.agent, method, iterator, [value]),
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
      step_of(to_executed(unexpected_exit(state, "yield* delegate")))
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
        step_of(executed(State(..state, stack: [val, ..rest], pc: state.pc + 1)))
      })
    }
    Some(method), AsyncSite(await_pc:, ..) -> {
      use #(res, state) <- or_delegate_exit(call_delegate(
        state,
        site,
        method,
        thrown,
      ))
      step_of(executed(
        State(..state, stack: [res, ..state.stack], pc: await_pc),
      ))
    }
    None, SyncSite(record:, ..) -> {
      use state <- or_delegate_exit(
        guard.guarded_unit(state, iter_protocol.iterator_close_normal(
          _,
          record.iterator,
        )),
      )
      throw_type_into(state, missing_throw)
    }
    None, AsyncSite(record:, ..) -> {
      use #(closed, state) <- or_delegate_exit(
        guard.guarded(state, iter_protocol.call_return(_, record.iterator)),
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
  use #(#(done, val), state) <- or_delegate_exit(guard.guard_state(
    guard.guard2(iter_protocol.read_iter_result, state.agent, res),
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
fn return_into(state: State, value: JsVal) -> Executed {
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
            Error(exit) -> exit_executed(exit, "return_into")
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
      case executed(fin) {
        Finished(Ok(v), state) -> return_into(state, v)
        other -> other
      }
    }
  }
}

fn close_for_return(state: State, record: JsVal, value: JsVal) -> Executed {
  case guard.guarded_unit(state, rt_lang.iter_close(_, record, abrupt: False)) {
    Ok(state) -> return_into(state, value)
    Error(exit) -> exit_executed(exit, "close_for_return")
  }
}

// a step exit met while completing a return outside the loop
fn exit_executed(exit: StepExit, site: String) -> Executed {
  case exit {
    state.Threw(thrown, state) -> throw_into(state, thrown)
    state.Returned(v, state) -> Finished(Ok(v), state)
    state.Yielded(_, _, state)
    | state.Awaited(_, state)
    | state.VmFailed(_, state) -> to_executed(unexpected_exit(state, site))
  }
}
