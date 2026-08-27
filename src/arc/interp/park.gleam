//// Parking and unparking coroutine activations: the one place a live
//// `State` becomes a `SuspendedFrame` and back, so the two stay exact
//// inverses and a field added to either is a compile error here.

import arc/internal/tuple_array
import arc/interp/ffi
import arc/interp/state.{type State, State}
import arc/rt/bytecode.{type ParkedAt, type SuspendedFrame, SuspendedFrame}
import arc/rt/types.{type Agent, type JsVal, Agent, FrameInfo, JsCell}
import gleam/option.{None, Some}

/// Snapshot `state` (already fixed up by the suspending opcode) as a frame
/// `JsOps.resume_frame` can rebuild from an `Agent` alone. `parked` says
/// how the next resumption is delivered to it. A bytecode body only ever
/// runs with its [[Realm]] current, so the running realm is the frame's.
pub fn park(state: State, parked: ParkedAt) -> SuspendedFrame {
  SuspendedFrame(
    template: state.func,
    pc: state.pc,
    locals: state.locals,
    stack: state.stack,
    try_stack: state.try_stack,
    this: state.this,
    home_object: state.home_object,
    eval_env: case state.eval_env {
      Some(h) -> Some(h.id)
      None -> None
    },
    parked:,
    call_args: state.call_args,
    realm: state.agent.realm.id,
    unit: state.unit,
  )
}

/// Rebuild the activation `frame` describes on top of `agent` as a root
/// activation (no caller frames, no `new.target`: a coroutine body is never
/// constructed). The caller pushes the body's `Error.stack` frame and has
/// entered `frame.realm`; the line it parked at is written onto it here.
pub fn unpark(agent: Agent, frame: SuspendedFrame) -> State {
  unpark_with(agent, frame, frame.stack)
}

/// `unpark` with the operand stack the resumption delivers (the parked one
/// with the sent value pushed, or as parked).
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
    this:,
    home_object:,
    eval_env:,
    parked: _,
    call_args:,
    realm: _,
    unit:,
  ) = frame
  let line = tuple_array.element(pc + 1, template.lines)
  let agent = case agent.frames {
    [FrameInfo(line: l, ..), ..] if l == line -> agent
    [top, ..rest] -> Agent(..agent, frames: [FrameInfo(..top, line:), ..rest])
    [] -> agent
  }
  State(
    agent:,
    pc:,
    stack:,
    locals:,
    code: template.bytecode,
    constants: template.constants,
    func: template,
    unit:,
    call_stack: [],
    outer_depth: agent.call_depth,
    try_stack:,
    this:,
    new_target: ffi.val([ffi.Undefined]),
    home_object:,
    call_args:,
    eval_env: case eval_env {
      Some(id) -> Some(JsCell(id))
      None -> None
    },
  )
}
