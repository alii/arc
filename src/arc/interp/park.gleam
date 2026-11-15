import arc/internal/tuple_array
import arc/interp/frames
import arc/interp/kernel
import arc/interp/state.{type State, Frame, Outermost, State}
import arc/rt/bytecode.{type ParkedAt, type SuspendedFrame, SuspendedFrame}
import arc/rt/types.{type Agent, type JsVal, Agent, FrameInfo, mk_object}
import gleam/option

pub fn park(state: State, parked: ParkedAt) -> SuspendedFrame {
  SuspendedFrame(
    template: state.frame.func,
    pc: state.pc,
    locals: state.locals,
    stack: state.stack,
    try_stack: state.frame.try_stack,
    eval_env: option.map(state.frame.eval_env, mk_object),
    parked:,
    call_args: state.frame.call_args,
    realm: state.frame.realm,
    unit_id: state.frame.unit_id,
  )
}

pub fn unpark(agent: Agent, frame: SuspendedFrame) -> State {
  unpark_with(agent, frame, frame.stack)
}

pub fn unpark_with(
  agent: Agent,
  frame: SuspendedFrame,
  stack: List(JsVal),
) -> State {
  let SuspendedFrame(
    template:,
    pc:,
    locals:,
    stack: _,
    try_stack:,
    eval_env:,
    parked: _,
    call_args:,
    realm: _,
    unit_id:,
  ) = frame
  let line = tuple_array.element(pc, template.lines)
  // callers push this frame first
  let agent = case agent.frames {
    [FrameInfo(line: l, ..), ..] if l == line -> agent
    [_, ..] -> Agent(..agent, frames: frames.set_top_line(agent.frames, line))
    [] -> agent
  }
  State(
    agent:,
    pc:,
    stack:,
    locals:,
    frame: Frame(
      func: template,
      unit_id:,
      return_to: Outermost,
      outer_depth: agent.call_depth,
      depth: agent.call_depth,
      try_stack:,
      call_args:,
      eval_env: option.map(eval_env, fn(env) {
        kernel.to_handle_unchecked([env])
      }),
      realm: agent.realm.id,
    ),
  )
}
