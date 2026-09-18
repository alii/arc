import arc/internal/tuple_array.{type TupleArray}
import arc/rt/bytecode.{type FuncTemplate, type TryFrame}
import arc/rt/gc as rt_gc
import arc/rt/types.{
  type Agent, type ErrorKind, type Handle, type JsVal, JsCell, RangeErr,
  ReferenceErr, TypeErr,
}
import gleam/dynamic.{type Dynamic}
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
    unit: Int,
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

@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: anything) -> Dynamic

// exhaustive destructures on purpose: classify new fields as roots or not
pub fn frame_roots(state: State) -> List(Handle) {
  let State(
    agent: _,
    pc: _,
    stack:,
    locals:,
    func: _,
    unit: _,
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
    acc_frame(
      [],
      stack,
      locals,
      this,
      new_target,
      home_object,
      call_args,
      eval_env,
    )
  list.fold(call_stack, acc, push_saved_frame)
  |> list.map(JsCell)
}

fn acc_frame(
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
  |> push_vals(stack)
  |> push_term(locals)
  |> push_val(this)
  |> push_val(new_target)
  |> push_val(home_object)
  |> push_vals(call_args)
  |> push_opt_handle(eval_env)
}

// caller.call_stack is the fold's own tail, not walked again
fn push_saved_frame(acc: List(Int), frame: SavedFrame) -> List(Int) {
  case frame {
    SavedFrame(caller:, pc: _, stack:, locals:, constructor_this:) ->
      push_caller(acc, caller, stack, locals, constructor_this)
    SavedRegFrame(caller:, pc: _, stack:, locals:, constructor_this:, r0:, r1:) ->
      push_caller(acc, caller, stack, locals, constructor_this)
      |> push_val(r0)
      |> push_val(r1)
    SavedCont(caller:, pc: _, stack:, locals:, constructor_this:, cont:) ->
      push_caller(acc, caller, stack, locals, constructor_this)
      |> rt_gc.push_term_refs(to_dynamic(cont), _)
  }
}

fn push_caller(
  acc: List(Int),
  caller: State,
  stack: List(JsVal),
  locals: TupleArray(JsVal),
  constructor_this: Option(JsVal),
) -> List(Int) {
  acc_frame(
    acc,
    stack,
    locals,
    caller.this,
    caller.new_target,
    caller.home_object,
    caller.call_args,
    caller.eval_env,
  )
  |> push_opt_val(constructor_this)
}

fn push_val(acc: List(Int), v: JsVal) -> List(Int) {
  rt_gc.push_val_refs(v, acc)
}

fn push_vals(acc: List(Int), vs: List(JsVal)) -> List(Int) {
  list.fold(vs, acc, push_val)
}

fn push_term(acc: List(Int), t: TupleArray(JsVal)) -> List(Int) {
  rt_gc.push_term_refs(to_dynamic(t), acc)
}

fn push_opt_val(acc: List(Int), ov: Option(JsVal)) -> List(Int) {
  case ov {
    Some(v) -> push_val(acc, v)
    None -> acc
  }
}

fn push_opt_handle(acc: List(Int), oh: Option(Handle)) -> List(Int) {
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
  let #(err, agent) = agent.store.ops.new_error(agent, kind, msg)
  #(err, State(..state, agent:))
}

fn throw_error(
  state: State,
  kind: ErrorKind,
  msg: String,
) -> Result(a, StepExit) {
  let #(err, state) = new_error(state, kind, msg)
  Error(Threw(err, state))
}

pub fn throw_type_error(state: State, msg: String) -> Result(a, StepExit) {
  throw_error(state, TypeErr, msg)
}

pub fn throw_reference_error(state: State, msg: String) -> Result(a, StepExit) {
  throw_error(state, ReferenceErr, msg)
}

pub fn stack_overflow_error(agent: Agent) -> #(JsVal, Agent) {
  agent.store.ops.new_error(agent, RangeErr, "Maximum call stack size exceeded")
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
    new_error(state, TypeErr, "internal error: " <> vm_error_message(err))
  #(Error(e), state)
}
