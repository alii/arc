import arc/vm/array.{type Array}
import arc/vm/builtins/common.{type Builtins}
import arc/vm/heap.{type Heap}
import arc/vm/opcode.{type Op}
import arc/vm/value.{type FuncTemplate, type JsValue, type Ref}
import gleam/dict
import gleam/option.{type Option}
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
    /// drained after script completes (or by run_and_drain).
    job_queue: List(value.Job),
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
///   use str, state <- frame.try_to_string(state, val)
pub fn try_to_string(
  state: State,
  val: JsValue,
  cont: fn(String, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
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

/// Call a function or propagate thrown error. Use with `use` syntax:
///   use result, state <- frame.try_call(state, callback, this_arg, [element, idx, arr])
pub fn try_call(
  state: State,
  callee: JsValue,
  this_val: JsValue,
  args: List(JsValue),
  cont: fn(JsValue, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case call(state, callee, this_val, args) {
    Ok(#(result, state)) -> cont(result, state)
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
