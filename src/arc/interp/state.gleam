//// The bytecode interpreter's activation record and the shapes that hang off
//// it: the saved caller frame pushed on Call, the reasons one `step` stops,
//// and the GC root enumerator for a live activation.
////
//// `State` splits cleanly in two. `agent` is the shared runtime's threaded
//// state: the heap, the realm, the microtask queue, `Error.stack` frames.
//// It is the ONLY thing a builtin, a compiled function or a nested
//// activation can observe or change, and it travels inside every raised
//// `wasm_exn` payload, so after any runtime call the interpreter adopts the
//// returned `Agent` and nothing else. Every other field is private to this
//// activation (program counter, operand stack, locals, try frames) and no
//// code outside the interpreter can hold a stale copy of it.

import arc/rt/bytecode.{type FuncTemplate, type TryFrame}
import arc/rt/gc as rt_gc
import arc/rt/types.{
  type Agent, type ErrorKind, type Handle, type JsVal, JsCell, RangeErr,
  ReferenceErr, SyntaxErr, TypeErr,
}
import arc/vm/internal/tuple_array.{type TupleArray}
import arc/vm/opcode.{type Op}
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

/// One interpreter activation. `agent` is shared, threaded runtime state;
/// the rest is this activation's own registers.
pub type State {
  State(
    /// Heap, realm(s), microtasks, `Error.stack` frames, host hooks. Adopted
    /// wholesale from whatever runtime call last returned (or threw) it.
    agent: Agent,
    pc: Int,
    stack: List(JsVal),
    locals: TupleArray(JsVal),
    /// `func.bytecode`, hoisted so dispatch is one tuple read.
    code: TupleArray(Op),
    /// `func.constants`, hoisted likewise. Compile-time literals only.
    constants: TupleArray(JsVal),
    /// The template this activation is executing.
    func: FuncTemplate,
    /// Caller frames of THIS activation, innermost first. A nested
    /// `run_bytecode` entered from a builtin starts a fresh, empty list.
    call_stack: List(SavedFrame),
    try_stack: List(TryFrame),
    /// §9.1.1.3 [[ThisValue]] the frame was entered with, after
    /// OrdinaryCallBindThis for non-arrows. Also seeded into the lexical
    /// `this` slot; kept here so natives and frame builders can read it
    /// without knowing the slot layout.
    this: JsVal,
    /// §13.3.12 NewTarget: the constructor object under `[[Construct]]`,
    /// `undefined` under `[[Call]]`.
    new_target: JsVal,
    /// §9.1.1.3 [[HomeObject]] of the running function, `undefined` if none.
    home_object: JsVal,
    /// Arguments of the current call, for the CreateArguments opcode.
    call_args: List(JsVal),
    /// Sloppy direct-eval var-injection object for this frame, allocated the
    /// first time a sloppy direct eval runs in it. Frame-local: saved on Call,
    /// restored on Return.
    eval_env: Option(Handle),
  )
}

/// A caller frame parked on `State.call_stack` while its callee runs.
/// The caller's source line is not here: it is the matching entry of
/// `Agent.frames`, which the interpreter pushes and pops in step with this.
pub type SavedFrame {
  SavedFrame(
    func: FuncTemplate,
    locals: TupleArray(JsVal),
    stack: List(JsVal),
    /// Where the caller resumes (the instruction after its Call).
    pc: Int,
    try_stack: List(TryFrame),
    /// Base-constructor calls: the freshly created receiver to hand back if
    /// the body does not explicitly return an object.
    constructor_this: Option(JsVal),
    this: JsVal,
    new_target: JsVal,
    home_object: JsVal,
    call_args: List(JsVal),
    eval_env: Option(Handle),
  )
}

/// Adopt the `Agent` a runtime call handed back.
pub fn with_agent(state: State, agent: Agent) -> State {
  State(..state, agent:)
}

// -- GC roots ----------------------------------------------------------------

@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: anything) -> Dynamic

/// Every heap handle a live activation keeps reachable that the shared
/// collector cannot see on its own: operand stack, locals, saved caller
/// frames, `this`/`new.target`/home object, call args, the eval env. Passed
/// as `extra_roots` to `rt/gc.t_collect` when the interpreter triggers a
/// collection. `agent` is deliberately not walked: its roots are the
/// collector's own `roots_of_state`.
///
/// Both destructures are EXHAUSTIVE (no `..`): adding a field to `State` or
/// `SavedFrame` is a compile error here until it has been classified as
/// carrying handles (walk it) or not (bind it to `_` with a note).
pub fn frame_roots(state: State) -> List(Handle) {
  let State(
    agent: _,
    // Plain index.
    pc: _,
    stack:,
    locals:,
    // Bytecode: no heap handles.
    code: _,
    // Compile-time literal pool: strings and numbers, never a handle.
    constants: _,
    // Template metadata: no heap handles.
    func: _,
    call_stack:,
    // Scalar: pc offsets and a stack depth.
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

/// Walk a whole locals tuple in one pass on the FFI side.
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

// -- Step exits ----------------------------------------------------------------

/// Which coroutine primitive suspended a frame.
pub type SuspendKind {
  Yield
  Await
}

/// Internal VM error: a bug in the interpreter, never a JS-level error.
pub type VmError {
  /// Tried to read past the end of the bytecode.
  PcOutOfBounds(pc: Int)
  StackUnderflow(op: String)
  /// A `yield`/`await` suspension escaped a frame that cannot resume it
  /// (top-level script, eval, module body, re-entrant native call). `site`
  /// names the driver that received it.
  SuspensionLeak(site: String, kind: SuspendKind)
  /// An engine invariant was breached. `site` names the detecting location;
  /// `detail` says what was expected vs. found.
  InternalError(site: String, detail: String)
}

/// Canonical human-readable rendering of a `VmError`.
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

/// Why one bytecode step stopped the frame instead of continuing to the next
/// instruction. Every variant carries the State it stopped in and its own
/// payload.
pub type StepExit {
  /// A JS exception was raised. `unwind_to_catch` decides where it lands.
  Threw(JsVal, State)
  /// The outermost frame executed `Return`: the activation's normal
  /// completion.
  Returned(JsVal, State)
  /// A generator suspended. `YieldKind` says which opcode did it, and hence
  /// which stack/pc fixup the suspended frame needs.
  Yielded(YieldKind, JsVal, State)
  /// An async function/generator hit `await`, waiting on a promise.
  Awaited(JsVal, State)
  /// An engine invariant broke. Never observable by guest code.
  VmFailed(VmError, State)
}

/// Which suspension opcode raised a `Yielded`, and therefore how the suspended
/// frame's stack/pc must be fixed up before it is saved. Carrying this on the
/// exit lets the loop avoid re-reading `code[pc]` after the step returned.
pub type YieldKind {
  /// `InitialYield`: stack unchanged, pc advances past the opcode.
  InitialSuspend
  /// `Yield`: pop the yielded value, pc advances.
  PlainYield
  /// `YieldStar`: pop the `.next()` arg but keep the iterator, pc stays put
  /// so the resume re-executes the same opcode.
  DelegateYield
  /// `AsyncYieldStarResume`: drop the consumed result object and jump back
  /// to the `AsyncYieldStarNext` at `next_pc`.
  AsyncDelegateResume(next_pc: Int)
}

/// Rewrite the `State` a `StepExit` carries, leaving its tag and payload
/// alone (used where an error path must still commit an agent write).
pub fn map_exit_state(exit: StepExit, f: fn(State) -> State) -> StepExit {
  case exit {
    Threw(v, s) -> Threw(v, f(s))
    Returned(v, s) -> Returned(v, f(s))
    Yielded(k, v, s) -> Yielded(k, v, f(s))
    Awaited(v, s) -> Awaited(v, f(s))
    VmFailed(e, s) -> VmFailed(e, f(s))
  }
}

// -- Interpreter-originated throws --------------------------------------------
// The Throw opcode, TDZ reads, not-callable and friends do not raise: they
// allocate the error through the non-raising `JsOps.new_error` (which also
// attaches the stack from `Agent.frames`) and return `Error(Threw(..))`.

/// Allocate a native error of `kind` in the current realm. Returns the error
/// value and the state holding it.
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

/// Throw an existing JS value from this state.
pub fn throw_value(state: State, value: JsVal) -> Result(a, StepExit) {
  Error(Threw(value, state))
}

/// Bridge from helpers returning `Result(a, #(JsVal, State))` to the step
/// function's `Result(a, StepExit)`.
pub fn rethrow(res: Result(a, #(JsVal, State))) -> Result(a, StepExit) {
  result.map_error(res, fn(err) {
    let #(thrown, state) = err
    Threw(thrown, state)
  })
}
