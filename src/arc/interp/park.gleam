//// Parking and unparking coroutine activations: the one place a live
//// `State` becomes a `SuspendedFrame` and back, so the two stay exact
//// inverses and a field added to either is a compile error here.

import arc/interp/call
import arc/interp/state.{type State, State}
import arc/rt/bytecode.{type ParkedAt, type SuspendedFrame, SuspendedFrame}
import arc/rt/types.{type Agent, JsCell, mk_undefined}
import gleam/option

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
    eval_env: option.map(state.eval_env, fn(h) { h.id }),
    line: call.current_line(state.agent),
    parked:,
    call_args: state.call_args,
    realm: state.agent.realm.id,
    unit: state.unit,
  )
}

/// Rebuild the activation `frame` describes on top of `agent` as a root
/// activation (no caller frames, no `new.target`: a coroutine body is never
/// constructed). The caller pushes the body's `Error.stack` frame and has
/// entered `frame.realm`; the parked line is written onto it here.
pub fn unpark(agent: Agent, frame: SuspendedFrame) -> State {
  let SuspendedFrame(
    template:,
    pc:,
    locals:,
    stack:,
    try_stack:,
    this:,
    home_object:,
    eval_env:,
    line:,
    parked: _,
    call_args:,
    realm: _,
    unit:,
  ) = frame
  State(
    agent: call.set_line(agent, line),
    pc:,
    stack:,
    locals:,
    code: template.bytecode,
    constants: template.constants,
    func: template,
    unit:,
    call_stack: [],
    outer_depth: agent.store.call_depth,
    try_stack:,
    this:,
    new_target: mk_undefined(),
    home_object:,
    call_args:,
    eval_env: option.map(eval_env, JsCell),
  )
}
