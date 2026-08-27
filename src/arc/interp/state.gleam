import arc/bytecode/opcode.{type Op}
import arc/internal/tuple_array.{type TupleArray}
import arc/rt/bytecode.{type FuncTemplate, type TryFrame}
import arc/rt/gc as rt_gc
import arc/rt/types.{
  type Agent, type ErrorKind, type Handle, type JsVal, JsCell, RangeErr,
  ReferenceErr, SyntaxErr, TypeErr,
}
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

pub type State {
  State(
    agent: Agent,
    pc: Int,
    stack: List(JsVal),
    locals: TupleArray(JsVal),
    code: TupleArray(Op),
    constants: TupleArray(JsVal),
    func: FuncTemplate,
    unit: Int,
    call_stack: List(SavedFrame),
    // call_depth == outer_depth + length(call_stack)
    outer_depth: Int,
    try_stack: List(TryFrame),
    this: JsVal,
    new_target: JsVal,
    home_object: JsVal,
    call_args: List(JsVal),
    eval_env: Option(Handle),
  )
}

pub type SavedFrame {
  SavedFrame(
    func: FuncTemplate,
    unit: Int,
    locals: TupleArray(JsVal),
    stack: List(JsVal),
    pc: Int,
    try_stack: List(TryFrame),
    constructor_this: Option(JsVal),
    this: JsVal,
    new_target: JsVal,
    home_object: JsVal,
    call_args: List(JsVal),
    eval_env: Option(Handle),
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
    code: _,
    constants: _,
    func: _,
    unit: _,
    call_stack:,
    outer_depth: _,
    try_stack: _,
    this:,
    new_target:,
    home_object:,
    call_args:,
    eval_env:,
  ) = state
  let acc =
    []
    |> push_vals(stack)
    |> push_term(locals)
    |> push_val(this)
    |> push_val(new_target)
    |> push_val(home_object)
    |> push_vals(call_args)
    |> push_opt_handle(eval_env)
  list.fold(call_stack, acc, push_saved_frame)
  |> list.map(JsCell)
}

fn push_saved_frame(acc: List(Int), frame: SavedFrame) -> List(Int) {
  let SavedFrame(
    func: _,
    unit: _,
    locals:,
    stack:,
    pc: _,
    try_stack: _,
    constructor_this:,
    this:,
    new_target:,
    home_object:,
    call_args:,
    eval_env:,
  ) = frame
  acc
  |> push_vals(stack)
  |> push_term(locals)
  |> push_opt_val(constructor_this)
  |> push_val(this)
  |> push_val(new_target)
  |> push_val(home_object)
  |> push_vals(call_args)
  |> push_opt_handle(eval_env)
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

pub fn throw_range_error(state: State, msg: String) -> Result(a, StepExit) {
  throw_error(state, RangeErr, msg)
}

pub fn throw_reference_error(state: State, msg: String) -> Result(a, StepExit) {
  throw_error(state, ReferenceErr, msg)
}

pub fn throw_syntax_error(state: State, msg: String) -> Result(a, StepExit) {
  throw_error(state, SyntaxErr, msg)
}

pub fn throw_value(state: State, value: JsVal) -> Result(a, StepExit) {
  Error(Threw(value, state))
}

pub fn rethrow(res: Result(a, #(JsVal, State))) -> Result(a, StepExit) {
  result.map_error(res, fn(err) {
    let #(thrown, state) = err
    Threw(thrown, state)
  })
}
