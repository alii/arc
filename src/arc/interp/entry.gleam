import arc/bytecode/error_kind.{TypeError}
import arc/interp/call
import arc/interp/eval
import arc/interp/frames
import arc/interp/guard
import arc/interp/resume
import arc/interp/state.{type State, State}
import arc/rt/bytecode.{type FuncTemplate}
import arc/rt/call.{type Completion, NormalCompletion, ThrowCompletion} as _
import arc/rt/limits
import arc/rt/realm as rt_realm
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type EvalKind, type Handle, type JsOps, type JsVal, type Step,
  Agent, BytecodeFn, JsOps, KHandle, SObject, StepThrow, Store, classify,
  mk_undefined,
}
import arc/rt/val as rt_val
import gleam/bool
import gleam/option.{type Option}

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
    resume_frame: resume.resume_frame,
  )
}

pub fn run(state: State) -> #(Result(JsVal, JsVal), Agent) {
  let m = frames.mark(state.agent)
  let agent = frames.push_frame_info(state.agent, state.func)
  let body = fn(agent) {
    let #(res, state) = resume.complete(State(..state, agent:), "run")
    #(res, state.agent)
  }
  resume.backstopped(agent, m, body, Error)
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
      case guard.guard1(resume.complete_call, state) {
        guard.Value(value: Ok(v), agent:) -> #(
          v,
          frames.resettle(agent, frames, depth),
        )
        guard.Value(value: Error(e), agent:) ->
          rt_store.throw(frames.resettle(agent, frames, depth), e)
        guard.Thrown(agent:, thrown:) ->
          rt_store.throw(frames.resettle(agent, frames, depth), thrown)
      }
    }
  }
}

fn raised(outcome: #(Result(JsVal, JsVal), Agent)) -> #(JsVal, Agent) {
  case outcome {
    #(Ok(v), agent) -> #(v, agent)
    #(Error(e), agent) -> rt_store.throw(agent, e)
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
      let m = frames.mark(agent)
      case call.enter_root(agent, callee, this, args, mk_undefined()) {
        Error(#(thrown, agent)) -> #(Error(thrown), agent)
        Ok(state) -> {
          let agent = state.agent
          let body = fn(agent) {
            let #(res, state) =
              resume.start_coroutine_root(
                State(..state, agent:),
                call.root_coroutine(state, fn_h),
              )
            #(res, state.agent)
          }
          resume.backstopped(agent, m, body, Error)
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
      case guard.guard1(resume.complete_call, state) {
        guard.Value(value:, agent:) -> #(
          value,
          frames.resettle(agent, frames, depth),
        )
        guard.Thrown(agent:, thrown:) -> #(
          Error(thrown),
          frames.resettle(agent, frames, depth),
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
    rt_store.throw(agent, e)
  })
  let #(completion, agent) = run_construct(agent, fn_h, args, new_target)
  let #(v, agent) = case completion {
    NormalCompletion(v) -> #(v, agent)
    ThrowCompletion(e) -> rt_store.throw(agent, e)
  }
  case classify(v) {
    KHandle(h) -> #(h, agent)
    _ -> {
      let #(e, agent) =
        rt_val.new_error(
          agent,
          TypeError,
          "internal error: constructor completed with a non-object",
        )
      rt_store.throw(agent, e)
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
  ) = rt_store.cell_get(agent, callee_h)
    as "construct_bytecode: handle is not a BytecodeFn cell"
  let m = frames.mark(agent)
  case call.root_this(agent, template, new_target) {
    Error(#(thrown, agent)) -> #(
      ThrowCompletion(thrown),
      frames.settle(agent, m),
    )
    Ok(#(this, kind, agent)) -> {
      let #(outcome, agent) = {
        use agent <- rt_realm.with_realm(agent, realm)
        let callee =
          call.root_callee(callee_h, template, env, home_object, flags, unit_id)
        case call.enter_root(agent, callee, this, args, new_target) {
          Error(#(thrown, agent)) -> #(
            ActivationSettled(ThrowCompletion(thrown)),
            agent,
          )
          Ok(state) -> {
            let body = fn(agent) {
              let #(res, state) =
                resume.complete(State(..state, agent:), "run_bytecode")
              case res {
                Ok(v) -> #(ActivationReturned(v, state), state.agent)
                Error(e) -> #(
                  ActivationSettled(ThrowCompletion(e)),
                  state.agent,
                )
              }
            }
            resume.backstopped(state.agent, m, body, escaped)
          }
        }
      }
      case outcome {
        ActivationSettled(c) -> #(c, agent)
        ActivationReturned(v, final) ->
          case call.finish_root(kind, v, State(..final, agent:)) {
            Ok(#(v, agent)) -> #(NormalCompletion(v), agent)
            Error(#(e, agent)) -> #(ThrowCompletion(e), agent)
          }
      }
    }
  }
}

type ActivationOutcome {
  ActivationSettled(Completion(JsVal))
  ActivationReturned(JsVal, State)
}

fn escaped(thrown: JsVal) -> ActivationOutcome {
  ActivationSettled(ThrowCompletion(thrown))
}

pub fn eval_source(
  agent: Agent,
  source: String,
  kind: EvalKind,
) -> #(JsVal, Agent) {
  eval.hook(agent, source, kind, run)
}

pub fn run_turn(state: State) -> #(Step, Agent) {
  let m = frames.mark(state.agent)
  let agent = frames.push_frame_info(state.agent, state.func)
  let turn = fn(agent) {
    resume.step_of(resume.executed(State(..state, agent:)))
  }
  resume.backstopped(agent, m, turn, StepThrow)
}
