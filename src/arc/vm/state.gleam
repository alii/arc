import arc/vm/builtins/common.{type Builtins}
import arc/vm/heap.{type Heap}
import arc/vm/internal/tuple_array.{type Array}
import arc/vm/limits
import arc/vm/opcode.{type Op}
import arc/vm/value.{type FuncTemplate, type JsValue, type Ref}
import gleam/dict
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/set

/// A single call frame on the VM call stack.
pub type CallFrame {
  CallFrame(
    func: FuncTemplate,
    locals: Array(JsValue),
    this: JsValue,
    env: Option(Ref),
    pc: Int,
    try_stack: List(TryFrame),
  )
}

/// Exception handler frame, pushed by PushTry.
pub type TryFrame {
  TryFrame(catch_target: Int, stack_depth: Int)
}

/// Why we entered a finally block. Saved by EnterFinally, consumed by LeaveFinally.
pub type FinallyCompletion {
  /// Normal completion — continue after finally.
  NormalCompletion
  /// An exception was thrown — re-throw after finally.
  ThrowCompletion(value: JsValue)
  /// A return was interrupted by finally — resume return after.
  ReturnCompletion(value: JsValue)
}

/// The full VM state.
pub type Vm {
  Vm(
    stack: List(JsValue),
    call_stack: List(CallFrame),
    code: Array(Op),
    pc: Int,
    finally_stack: List(FinallyCompletion),
  )
}

/// A saved caller frame, pushed onto call_stack when Call enters a function.
pub type SavedFrame {
  SavedFrame(
    func: FuncTemplate,
    locals: Array(JsValue),
    stack: List(JsValue),
    pc: Int,
    try_stack: List(TryFrame),
    this_binding: JsValue,
    /// For constructor calls: the newly created object to return if the
    /// constructor doesn't explicitly return an object.
    constructor_this: Option(JsValue),
    /// The heap ref of the currently-executing function (needed by CallSuper
    /// to find the parent constructor via callee_ref.__proto__).
    callee_ref: Option(Ref),
    /// Original args passed to this frame's call (for arguments object creation).
    call_args: List(JsValue),
  )
}

/// The internal VM executor state. Public so builtins can receive and return it,
/// giving them full access to the runtime (including js_to_string for ToPrimitive).
pub type State {
  State(
    stack: List(JsValue),
    locals: Array(JsValue),
    constants: Array(JsValue),
    /// DeclarativeRecord: let/const at global scope. NOT on globalThis. Checked first.
    lexical_globals: dict.Dict(String, JsValue),
    /// Tracks which lexical globals are const (PutGlobal throws TypeError).
    const_lexical_globals: set.Set(String),
    /// ObjectRecord: Ref to globalThis heap object. var/function/builtins live here.
    global_object: Ref,
    func: FuncTemplate,
    code: Array(Op),
    heap: Heap,
    pc: Int,
    call_stack: List(SavedFrame),
    try_stack: List(TryFrame),
    finally_stack: List(FinallyCompletion),
    builtins: Builtins,
    /// The current `this` binding. Set by CallMethod/CallConstructor,
    /// defaults to JsUndefined for regular calls.
    this_binding: JsValue,
    /// The heap ref of the currently-executing function (for derived constructors
    /// and arguments.callee).
    callee_ref: Option(Ref),
    /// Original arguments passed to the current function call. Consumed by
    /// CreateArguments opcode to build the arguments object.
    call_args: List(JsValue),
    /// Promise microtask job queue. Jobs enqueued during promise operations,
    /// drained after script completes.
    job_queue: List(value.Job),
    /// ES2024 HostPromiseRejectionTracker: data_refs of promises rejected while
    /// [[PromiseIsHandled]] was false. Removed when a handler is later attached.
    /// Any remaining after job draining are reported as unhandled rejections.
    unhandled_rejections: List(Ref),
    /// PromiseSlot data_refs created by `Arc.receiveAsync()` waiting for a
    /// `UserMessage` to arrive. FIFO — first caller gets first message. When
    /// this is empty, the event loop uses selective receive to leave any
    /// `UserMessage` in the BEAM mailbox so blocking `Arc.receive()` can pick
    /// it up later.
    pending_receivers: List(Ref),
    /// Count of in-flight external operations: each `receiveAsync`,
    /// `setTimeout`, `fetch`, etc. increments this. The event loop blocks
    /// on the BEAM mailbox while outstanding > 0; exits when it hits 0.
    outstanding: Int,
    /// Descriptions for user-created symbols (Symbol("desc")).
    symbol_descriptions: dict.Dict(value.SymbolId, String),
    /// Global symbol registry for Symbol.for() / Symbol.keyFor().
    symbol_registry: dict.Dict(String, value.SymbolId),
    /// Maps RealmSlot refs to their Builtins. Used by $262.evalScript/createRealm
    /// to resolve realm-specific builtins (stored separately from heap to avoid
    /// import cycle between value.gleam and builtins/common.gleam).
    realms: dict.Dict(Ref, Builtins),
    /// ES2024 ToString — converts any JsValue to a string, including objects
    /// via ToPrimitive with VM re-entry. Set by the VM executor.
    js_to_string: fn(State, JsValue) ->
      Result(#(String, State), #(JsValue, State)),
    /// Re-entrant call mechanism — invoke a JS callable with (this, args).
    /// Returns Ok(result, state) on normal completion, Error(thrown, state) on throw.
    /// Set by the VM executor (wraps run_handler_with_this).
    call_fn: fn(State, JsValue, JsValue, List(JsValue)) ->
      Result(#(JsValue, State), #(JsValue, State)),
    /// Re-entrant construct mechanism — `new target(...args)` from native code.
    /// Same shape as call_fn. Set by the VM executor (wraps do_construct).
    construct_fn: fn(State, JsValue, List(JsValue)) ->
      Result(#(JsValue, State), #(JsValue, State)),
    /// Current call stack depth. Incremented on function entry, decremented on return.
    /// Throws RangeError when exceeding limits.max_call_depth.
    call_depth: Int,
    /// Whether the event loop is active. When False, APIs that require the
    /// event loop (Arc.receiveAsync, Arc.setTimeout) throw a TypeError.
    event_loop: Bool,
  )
}

/// Thread VM-global state from a child execution back to parent.
/// Covers job_queue, event loop state, lexical globals. Does NOT thread
/// heap (caller handles separately since it's often further mutated).
pub fn merge_globals(
  parent: State,
  child: State,
  extra_jobs: List(value.Job),
) -> State {
  State(
    ..parent,
    lexical_globals: child.lexical_globals,
    const_lexical_globals: child.const_lexical_globals,
    job_queue: list.append(child.job_queue, extra_jobs),
    pending_receivers: child.pending_receivers,
    outstanding: child.outstanding,
  )
}

/// Call state.js_to_string, handling the function field access.
pub fn to_string(
  state: State,
  val: JsValue,
) -> Result(#(String, State), #(JsValue, State)) {
  let f = state.js_to_string
  f(state, val)
}

/// Convert a value to string or propagate error. Use with `use` syntax:
///   use str, state <- state.try_to_string(state, val)
pub fn try_to_string(
  state: State,
  val: JsValue,
  cont: fn(String, State) -> #(State, Result(b, JsValue)),
) -> #(State, Result(b, JsValue)) {
  case to_string(state, val) {
    Ok(#(str, state)) -> cont(str, state)
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// Call state.call_fn (re-entrant JS function call), handling the function field access.
pub fn call(
  state: State,
  callee: JsValue,
  this_val: JsValue,
  args: List(JsValue),
) -> Result(#(JsValue, State), #(JsValue, State)) {
  let f = state.call_fn
  f(state, callee, this_val, args)
}

/// Call state.construct_fn (re-entrant `new target(...args)`).
pub fn construct(
  state: State,
  target: JsValue,
  args: List(JsValue),
) -> Result(#(JsValue, State), #(JsValue, State)) {
  let f = state.construct_fn
  f(state, target, args)
}

/// Call a function or propagate thrown error. Use with `use` syntax:
///   use result, state <- state.try_call(state, callback, this_arg, [element, idx, arr])
pub fn try_call(
  state: State,
  callee: JsValue,
  this_val: JsValue,
  args: List(JsValue),
  cont: fn(JsValue, State) -> #(State, Result(b, JsValue)),
) -> #(State, Result(b, JsValue)) {
  case call(state, callee, this_val, args) {
    Ok(#(result, state)) -> cont(result, state)
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// Generic CPS helper for any fallible state operation returning
/// `Result(#(a, State), #(JsValue, State))`. Use with `use` syntax:
///   use val, state <- state.try_op(some_operation(state, ...))
/// Polymorphic in both the unwrapped value type and the continuation's result
/// type, so it works in loops returning non-JsValue results too.
pub fn try_op(
  result: Result(#(a, State), #(JsValue, State)),
  cont: fn(a, State) -> #(State, Result(b, JsValue)),
) -> #(State, Result(b, JsValue)) {
  case result {
    Ok(#(val, state)) -> cont(val, state)
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// CPS helper for fallible state operations that return only an updated State
/// (no extra value). Use with `use` syntax:
///   use state <- state.try_state(some_operation(state, ...))
pub fn try_state(
  result: Result(State, #(JsValue, State)),
  cont: fn(State) -> #(State, Result(b, JsValue)),
) -> #(State, Result(b, JsValue)) {
  case result {
    Ok(state) -> cont(state)
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// Convenience wrapper: allocate a TypeError on the heap and return it as
/// an Error result. Shared by all builtin modules to avoid boilerplate
/// around common.make_type_error + state threading.
pub fn type_error(
  state: State,
  msg: String,
) -> #(State, Result(JsValue, JsValue)) {
  let #(heap, err) = common.make_type_error(state.heap, state.builtins, msg)
  #(State(..state, heap:), Error(err))
}

pub fn range_error(
  state: State,
  msg: String,
) -> #(State, Result(JsValue, JsValue)) {
  let #(heap, err) = common.make_range_error(state.heap, state.builtins, msg)
  #(State(..state, heap:), Error(err))
}

/// Guard against array length exceeding Number.MAX_SAFE_INTEGER.
/// Throws TypeError (per spec §23.1.3.23/31/33) if length > 2^53-1.
pub fn guard_safe_length(
  state: State,
  length: Int,
  cont: fn() -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case length > limits.max_safe_integer {
    True -> type_error(state, "Array length exceeds maximum safe integer")
    False -> cont()
  }
}

// ============================================================================
// VM error types (shared across vm modules)
// ============================================================================

/// Internal VM error — these are bugs in the VM, not JS-level errors.
pub type VmError {
  /// Tried to read past end of bytecode
  PcOutOfBounds(pc: Int)
  /// Stack underflow
  StackUnderflow(op: String)
  /// Local variable index out of bounds
  LocalIndexOutOfBounds(index: Int)
  /// Unimplemented opcode
  Unimplemented(op: String)
}

/// Signals from step() — either continue with new state, or stop.
pub type StepResult {
  Done
  StepVmError(VmError)
  Thrown
  /// Generator suspension — yielded a value (or initial suspend).
  Yielded
  /// Async suspension — hit `await`, waiting on a promise.
  Awaited
}

// ============================================================================
// Step-level error helpers
// ============================================================================

/// Allocate a JS TypeError and return it as a step-level thrown error.
pub fn throw_type_error(
  state: State,
  msg: String,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let #(heap, err) = common.make_type_error(state.heap, state.builtins, msg)
  Error(#(Thrown, err, heap))
}

/// Allocate a JS RangeError and return it as a step-level thrown error.
pub fn throw_range_error(
  state: State,
  msg: String,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let #(heap, err) = common.make_range_error(state.heap, state.builtins, msg)
  Error(#(Thrown, err, heap))
}

/// Allocate a JS ReferenceError and return it as a step-level thrown error.
pub fn throw_reference_error(
  state: State,
  msg: String,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let #(heap, err) =
    common.make_reference_error(state.heap, state.builtins, msg)
  Error(#(Thrown, err, heap))
}

/// Bridge from inner helpers that return Result(a, #(JsValue, State))
/// to the step function's Result(a, #(StepResult, JsValue, Heap)).
pub fn rethrow(
  res: Result(a, #(JsValue, State)),
) -> Result(a, #(StepResult, JsValue, Heap)) {
  result.map_error(res, fn(err) {
    let #(thrown, state) = err
    #(Thrown, thrown, state.heap)
  })
}
