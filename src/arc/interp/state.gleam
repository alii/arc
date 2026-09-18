import arc/bytecode/error_kind.{
  type ErrorKind, RangeError, ReferenceError, TypeError,
}
import arc/internal/tuple_array.{type TupleArray}
import arc/rt/bytecode.{type FuncTemplate, type TryFrame}
import arc/rt/gc as rt_gc
import arc/rt/types.{type Agent, type Handle, type JsVal, Handle}
import arc/rt/val as rt_val
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

pub type State {
  State(
    agent: Agent,
    pc: Int,
    stack: List(JsVal),
    locals: TupleArray(JsVal),
    func: FuncTemplate,
    unit_id: Int,
    call_stack: List(SavedFrame),
    outer_depth: Int,
    // agent.call_depth may lag this inside the loop
    depth: Int,
    try_stack: List(TryFrame),
    this: JsVal,
    new_target: JsVal,
    home_object: JsVal,
    call_args: List(JsVal),
    eval_env: Option(Handle),
  )
}

// restore pc/stack/locals from here, not from caller
pub type SavedFrame {
  SavedFrame(
    caller: State,
    pc: Int,
    stack: List(JsVal),
    locals: TupleArray(JsVal),
    constructor_this: Option(JsVal),
  )
  // r0/r1 are the register-cached locals, written back on restore
  SavedRegFrame(
    caller: State,
    pc: Int,
    stack: List(JsVal),
    locals: TupleArray(JsVal),
    constructor_this: Option(JsVal),
    r0: JsVal,
    r1: JsVal,
  )
  // an op that had to call user code midway; cont finishes it with the
  // result; locals already hold the registers like SavedFrame
  SavedCont(
    caller: State,
    pc: Int,
    stack: List(JsVal),
    locals: TupleArray(JsVal),
    constructor_this: Option(JsVal),
    cont: fn(State, JsVal) -> Result(State, StepExit),
  )
}

pub fn with_agent(state: State, agent: Agent) -> State {
  State(..state, agent:)
}

// exhaustive destructures on purpose: classify new fields as roots or not
pub fn frame_roots(state: State) -> List(Handle) {
  let State(
    agent: _,
    pc: _,
    stack:,
    locals:,
    func: _,
    unit_id: _,
    call_stack:,
    outer_depth: _,
    depth: _,
    try_stack: _,
    this:,
    new_target:,
    home_object:,
    call_args:,
    eval_env:,
  ) = state
  let acc =
    push_frame_roots(
      [],
      stack,
      locals,
      this,
      new_target,
      home_object,
      call_args,
      eval_env,
    )
  list.fold(call_stack, acc, push_saved_frame_roots)
  |> list.map(Handle)
}

fn push_frame_roots(
  acc: List(Int),
  stack: List(JsVal),
  locals: TupleArray(JsVal),
  this: JsVal,
  new_target: JsVal,
  home_object: JsVal,
  call_args: List(JsVal),
  eval_env: Option(Handle),
) -> List(Int) {
  acc
  |> rt_gc.push_refs(stack, _)
  |> rt_gc.push_refs(locals, _)
  |> rt_gc.push_refs(this, _)
  |> rt_gc.push_refs(new_target, _)
  |> rt_gc.push_refs(home_object, _)
  |> rt_gc.push_refs(call_args, _)
  |> push_optional_handle(eval_env)
}

// caller.call_stack is the fold's own tail, not walked again
fn push_saved_frame_roots(acc: List(Int), frame: SavedFrame) -> List(Int) {
  case frame {
    SavedFrame(caller:, pc: _, stack:, locals:, constructor_this:) ->
      push_caller_roots(acc, caller, stack, locals, constructor_this)
    SavedRegFrame(caller:, pc: _, stack:, locals:, constructor_this:, r0:, r1:) ->
      push_caller_roots(acc, caller, stack, locals, constructor_this)
      |> rt_gc.push_refs(r0, _)
      |> rt_gc.push_refs(r1, _)
    SavedCont(caller:, pc: _, stack:, locals:, constructor_this:, cont:) ->
      push_caller_roots(acc, caller, stack, locals, constructor_this)
      |> rt_gc.push_refs(cont, _)
  }
}

fn push_caller_roots(
  acc: List(Int),
  caller: State,
  stack: List(JsVal),
  locals: TupleArray(JsVal),
  constructor_this: Option(JsVal),
) -> List(Int) {
  push_frame_roots(
    acc,
    stack,
    locals,
    caller.this,
    caller.new_target,
    caller.home_object,
    caller.call_args,
    caller.eval_env,
  )
  |> push_optional_val(constructor_this)
}

fn push_optional_val(acc: List(Int), ov: Option(JsVal)) -> List(Int) {
  case ov {
    Some(v) -> rt_gc.push_refs(v, acc)
    None -> acc
  }
}

fn push_optional_handle(acc: List(Int), oh: Option(Handle)) -> List(Int) {
  case oh {
    Some(h) -> [h.id, ..acc]
    None -> acc
  }
}

pub type SuspendKind {
  Yield
  Await
}

pub type VmError {
  PcOutOfBounds(pc: Int)
  StackUnderflow(op: String)
  SuspensionLeak(site: String, kind: SuspendKind)
  InternalError(site: String, detail: String)
}

pub fn vm_error_message(err: VmError) -> String {
  case err {
    PcOutOfBounds(pc) -> "pc out of bounds: " <> int.to_string(pc)
    StackUnderflow(op) -> "stack underflow in " <> op
    SuspensionLeak(site:, kind:) ->
      "internal error at "
      <> site
      <> ": "
      <> suspend_kind_name(kind)
      <> " suspension escaped a non-coroutine frame"
    InternalError(site:, detail:) ->
      "internal error at " <> site <> ": " <> detail
  }
}

fn suspend_kind_name(kind: SuspendKind) -> String {
  case kind {
    Yield -> "yield"
    Await -> "await"
  }
}

pub type StepExit {
  Threw(JsVal, State)
  Returned(JsVal, State)
  Yielded(YieldKind, JsVal, State)
  Awaited(JsVal, State)
  VmFailed(VmError, State)
}

pub type YieldKind {
  InitialSuspend
  PlainYield
  DelegateYield
  AsyncDelegateResume(next_pc: Int)
}

pub fn map_exit_state(exit: StepExit, f: fn(State) -> State) -> StepExit {
  case exit {
    Threw(v, s) -> Threw(v, f(s))
    Returned(v, s) -> Returned(v, f(s))
    Yielded(k, v, s) -> Yielded(k, v, f(s))
    Awaited(v, s) -> Awaited(v, f(s))
    VmFailed(e, s) -> VmFailed(e, f(s))
  }
}

pub fn truncate_stack(stack: List(JsVal), depth: Int) -> List(JsVal) {
  let excess = list.length(stack) - depth
  case excess > 0 {
    True -> list.drop(stack, excess)
    False -> stack
  }
}

pub fn new_error(
  state: State,
  kind: ErrorKind,
  msg: String,
) -> #(JsVal, State) {
  let agent = state.agent
  let #(err, agent) = rt_val.t_new_error(agent, kind, msg)
  #(err, State(..state, agent:))
}

pub fn throw_error(
  state: State,
  kind: ErrorKind,
  msg: String,
) -> Result(a, StepExit) {
  let #(err, state) = new_error(state, kind, msg)
  Error(Threw(err, state))
}

pub fn throw_type_error(state: State, msg: String) -> Result(a, StepExit) {
  throw_error(state, TypeError, msg)
}

pub fn throw_reference_error(state: State, msg: String) -> Result(a, StepExit) {
  throw_error(state, ReferenceError, msg)
}

pub fn stack_overflow_error(agent: Agent) -> #(JsVal, Agent) {
  rt_val.t_new_error(agent, RangeError, "Maximum call stack size exceeded")
}

pub fn throw_stack_overflow(state: State) -> Result(a, StepExit) {
  let #(err, agent) = stack_overflow_error(state.agent)
  Error(Threw(err, State(..state, agent:)))
}

// a vm error surfaces as a typeerror completion
pub fn internal_fault(
  state: State,
  err: VmError,
) -> #(Result(JsVal, JsVal), State) {
  let #(e, state) =
    new_error(state, TypeError, "internal error: " <> vm_error_message(err))
  #(Error(e), state)
}
