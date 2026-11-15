import arc/bytecode/error_kind.{
  type ErrorKind, RangeError, ReferenceError, TypeError,
}
import arc/internal/tuple_array.{type TupleArray}
import arc/interp/kernel
import arc/rt/bytecode.{type Code, type FuncTemplate, type TryFrame}
import arc/rt/call.{type Completion}
import arc/rt/gc as rt_gc
import arc/rt/types.{type Agent, type Handle, type JsVal, Handle}
import arc/rt/val as rt_val
import gleam/list
import gleam/option.{type Option, None, Some}

pub type State {
  State(
    agent: Agent,
    pc: Int,
    stack: List(JsVal),
    locals: TupleArray(JsVal),
    frame: Frame,
  )
}

// what stays put while a call runs, so stepping copies little
pub type Frame {
  Frame(
    func: FuncTemplate,
    unit_id: Int,
    return_to: ReturnTo,
    outer_depth: Int,
    // agent.call_depth may lag this inside threaded code
    depth: Int,
    try_stack: List(TryFrame),
    call_args: List(JsVal),
    eval_env: Option(Handle),
    // the id of the realm the frame runs in
    realm: Int,
  )
}

pub type ReturnTo {
  // the run ends with this frame
  Outermost
  // the same, for threaded code a builtin called, which wants just the value
  Direct
  SavedFrame(
    caller: Frame,
    pc: Int,
    stack: List(JsVal),
    locals: TupleArray(JsVal),
    constructor_this: Option(JsVal),
  )
  // threaded code to go on with, in a function that has no registers
  SavedThread(
    caller: Frame,
    pc: Int,
    stack: List(JsVal),
    locals: TupleArray(JsVal),
    constructor_this: Option(JsVal),
    next: Code,
    code: TupleArray(Code),
  )
  // the same with the locals held in registers, which `locals` has stale
  SavedRegs(
    caller: Frame,
    pc: Int,
    stack: List(JsVal),
    locals: TupleArray(JsVal),
    constructor_this: Option(JsVal),
    next: Code,
    code: TupleArray(Code),
    r0: JsVal,
    r1: JsVal,
    r2: JsVal,
    r3: JsVal,
  )
  // an op paused on user code; cont finishes it
  SavedCont(
    caller: Frame,
    pc: Int,
    stack: List(JsVal),
    locals: TupleArray(JsVal),
    constructor_this: Option(JsVal),
    cont: fn(JsVal, State) -> Result(State, StepExit),
    // filled in by the threaded code the op was run from
    resume: Option(Resumption),
  )
}

// where threaded code goes on after an op, and the registers it held
pub type Resumption {
  Resumption(
    next: Code,
    takes_top: Bool,
    r0: JsVal,
    r1: JsVal,
    r2: JsVal,
    r3: JsVal,
  )
}

pub fn with_agent(state: State, agent: Agent) -> State {
  State(..state, agent:)
}

// exhaustive destructures on purpose: classify new fields as roots or not
pub fn frame_roots(state: State) -> List(Handle) {
  let State(agent: _, pc: _, stack:, locals:, frame:) = state
  let acc = push_frame_roots([], frame, stack, locals)
  push_caller_roots(acc, frame.return_to) |> list.map(Handle)
}

fn push_frame_roots(
  acc: List(Int),
  frame: Frame,
  stack: List(JsVal),
  locals: TupleArray(JsVal),
) -> List(Int) {
  let Frame(
    func: _,
    unit_id: _,
    return_to: _,
    outer_depth: _,
    depth: _,
    try_stack: _,
    call_args:,
    eval_env:,
    realm: _,
  ) = frame
  acc
  |> rt_gc.push_refs(stack, _)
  |> rt_gc.push_refs(locals, _)
  |> rt_gc.push_refs(call_args, _)
  |> push_optional_handle(eval_env)
}

fn push_caller_roots(acc: List(Int), saved: ReturnTo) -> List(Int) {
  case saved {
    Outermost | Direct -> acc
    SavedFrame(caller:, pc: _, stack:, locals:, constructor_this:) ->
      push_frame_roots(acc, caller, stack, locals)
      |> push_optional_val(constructor_this)
      |> push_caller_roots(caller.return_to)
    SavedThread(
      caller:,
      pc: _,
      stack:,
      locals:,
      constructor_this:,
      next: _,
      code: _,
    ) ->
      push_frame_roots(acc, caller, stack, locals)
      |> push_optional_val(constructor_this)
      |> push_caller_roots(caller.return_to)
    SavedRegs(
      caller:,
      pc: _,
      stack:,
      locals:,
      constructor_this:,
      next: _,
      code: _,
      r0:,
      r1:,
      r2:,
      r3:,
    ) ->
      // what locals has where a register is may be long dead
      kernel.flush_registers(locals, caller.func.regs, r0, r1, r2, r3)
      |> push_frame_roots(acc, caller, stack, _)
      |> push_optional_val(constructor_this)
      |> push_caller_roots(caller.return_to)
    SavedCont(
      caller:,
      pc: _,
      stack:,
      locals:,
      constructor_this:,
      cont:,
      resume:,
    ) ->
      resumed_locals(locals, caller, resume)
      |> push_frame_roots(acc, caller, stack, _)
      |> push_optional_val(constructor_this)
      |> rt_gc.push_refs(cont, _)
      |> push_caller_roots(caller.return_to)
  }
}

fn resumed_locals(
  locals: TupleArray(JsVal),
  caller: Frame,
  resume: Option(Resumption),
) -> TupleArray(JsVal) {
  case resume {
    Some(Resumption(next: _, takes_top: _, r0:, r1:, r2:, r3:)) ->
      kernel.flush_registers(locals, caller.func.regs, r0, r1, r2, r3)
    None -> locals
  }
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

pub type Outcome {
  Completed(Completion(JsVal))
  Suspended(SuspendKind, JsVal)
}

pub type SuspendKind {
  Yield
  Await
}

pub type VmError {
  StackUnderflow(op: String)
  SuspensionLeak(site: String, kind: SuspendKind)
  InternalError(site: String, detail: String)
}

pub fn vm_error_message(err: VmError) -> String {
  case err {
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
  VmFailed(VmError, State)
}

pub type YieldKind {
  DelegateYield
  AsyncDelegateResume(next_pc: Int)
}

pub fn map_exit(exit: StepExit, f: fn(State) -> State) -> StepExit {
  case exit {
    Threw(v, state) -> Threw(v, f(state))
    Returned(v, state) -> Returned(v, f(state))
    Yielded(k, v, state) -> Yielded(k, v, f(state))
    VmFailed(e, state) -> VmFailed(e, f(state))
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
  let #(err, agent) = rt_val.new_error(agent, kind, msg)
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
  throw_error(state, TypeError, msg)
}

pub fn throw_syntax_error(state: State, msg: String) -> Result(a, StepExit) {
  throw_error(state, error_kind.SyntaxError, msg)
}

pub fn throw_reference_error(state: State, msg: String) -> Result(a, StepExit) {
  throw_error(state, ReferenceError, msg)
}

pub fn stack_overflow_error(agent: Agent) -> #(JsVal, Agent) {
  rt_val.new_error(agent, RangeError, "Maximum call stack size exceeded")
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
  let #(e, agent) = internal_error(state.agent, err)
  #(Error(e), State(..state, agent:))
}

pub fn internal_error(agent: Agent, err: VmError) -> #(JsVal, Agent) {
  rt_val.new_error(
    agent,
    TypeError,
    "internal error: " <> vm_error_message(err),
  )
}
