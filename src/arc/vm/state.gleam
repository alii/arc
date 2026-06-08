import arc/vm/builtins/common.{type Builtins}
import arc/vm/heap
import arc/vm/internal/job_queue.{type JobQueue}
import arc/vm/internal/tuple_array.{type TupleArray}
import arc/vm/limits
import arc/vm/opcode.{type Op}
import arc/vm/value.{type FuncTemplate, type JsValue, type Ref}
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/string

// -- Concrete type aliases ----------------------------------------------------
// heap.gleam and value.gleam are generic over `ctx` so NativeFnSlot can carry
// host-function closures typed against State without an import cycle. This is
// where the recursive knot gets tied — State refers to itself through Heap.

pub type Heap =
  heap.Heap(State)

pub type HeapSlot =
  value.HeapSlot(State)

pub type ExoticKind =
  value.ExoticKind(State)

pub type NativeFnSlot =
  value.NativeFnSlot(State)

/// Signature for host-provided native functions installed via engine.define_fn
/// or engine.define_namespace. Receives (args, this, state), returns
/// (new_state, Ok(return_value) | Error(thrown_value)).
pub type HostFn =
  fn(List(JsValue), JsValue, State) -> #(State, Result(JsValue, JsValue))

/// Exception handler frame, pushed by PushTry.
pub type TryFrame {
  TryFrame(catch_target: Int, stack_depth: Int)
}

/// A saved caller frame, pushed onto call_stack when Call enters a function.
pub type SavedFrame {
  SavedFrame(
    func: FuncTemplate,
    locals: TupleArray(JsValue),
    stack: List(JsValue),
    pc: Int,
    try_stack: List(TryFrame),
    /// For constructor calls: the newly created object to return if the
    /// constructor doesn't explicitly return an object.
    constructor_this: Option(JsValue),
    /// §13.3.12 NewTarget — `JsObject(ref)` for `[[Construct]]` calls,
    /// `JsUndefined` for `[[Call]]`. Seeded into the RefNewTarget lexical
    /// slot at frame entry. Saved/restored across calls.
    new_target: JsValue,
    /// Original args passed to this frame's call (for arguments object creation).
    call_args: List(JsValue),
    /// Caller's eval_env ref (sloppy direct-eval var-injection dict).
    /// Restored on Return so eval-created vars survive the callee's lifetime.
    eval_env: Option(Ref),
    /// Source line this frame was executing when it made the call (set by the
    /// most recent SetLine before the Call). Restored into state.current_line
    /// on Return, and read when building a stack trace.
    current_line: Int,
  )
}

/// Per-realm, rarely-mutated execution context, split out of State so the
/// per-instruction `State(..state, ...)` copy in step() stays small. These
/// fields change only on rare operations: realm setup, global let/const
/// declaration, and Symbol.for. Mutations pay one extra small record copy.
pub type RealmCtx {
  RealmCtx(
    /// DeclarativeRecord: let/const at global scope. NOT on globalThis. Checked
    /// first. Each binding is `Let`/`Const` (const rejects assignment) wrapping
    /// `JsUninitialized` while in TDZ, then its bound value.
    lexical_globals: dict.Dict(String, value.LexicalGlobal),
    /// ObjectRecord: Ref to globalThis heap object. var/function/builtins live here.
    global_object: Ref,
    /// Descriptions for user-created symbols (Symbol("desc")).
    symbol_descriptions: dict.Dict(value.SymbolId, String),
    /// Global symbol registry for Symbol.for() / Symbol.keyFor().
    symbol_registry: dict.Dict(String, value.SymbolId),
    /// §13.2.8.4 GetTemplateObject cache: tagged-template call-site id →
    /// template object ref. Site ids are globally unique (baked in at
    /// compile time), so one dict serves all realms. Entries are rooted in
    /// the heap and live for the VM's lifetime, matching the spec's
    /// per-Parse-Node template object identity.
    template_objects: dict.Dict(Int, Ref),
    /// Maps RealmSlot refs to their Builtins. Used by $262.evalScript/createRealm
    /// to resolve realm-specific builtins (stored separately from heap to avoid
    /// import cycle between value.gleam and builtins/common.gleam).
    realms: dict.Dict(Ref, Builtins),
    /// Re-entrant call mechanism — invoke a JS callable with (this, args).
    /// Returns Ok(result, state) on normal completion, Error(thrown, state) on throw.
    /// Set by the VM executor (wraps run_handler_with_this).
    call_fn: fn(State, JsValue, JsValue, List(JsValue)) ->
      Result(#(JsValue, State), #(JsValue, State)),
    /// Re-entrant construct mechanism — `new target(...args)` from native code.
    /// 4th arg is newTarget (§10.1.13). Set by the VM executor (wraps do_construct).
    construct_fn: fn(State, JsValue, List(JsValue), JsValue) ->
      Result(#(JsValue, State), #(JsValue, State)),
    /// Pre-built sentinel frame template (bytecode = [Return, Return]) used by
    /// the re-entrant call/construct callbacks to drive a callee to completion
    /// on an isolated stack. Built once at state init — these callbacks run
    /// per element in hot paths (Array.prototype.map etc.), so rebuilding the
    /// template each call is measurable.
    callback_sentinel: FuncTemplate,
  )
}

// ============================================================================
// Host Atomics capabilities — the blocking-wait / wake-delivery contract.
//
// Core never blocks on the BEAM mailbox and never sends wake messages
// itself. Both event-driven sides of Atomics.wait/notify are inverted into
// embedder-installed capability functions carried on State (the same
// inversion as `host.suspend`/`host.resume` for promises). The types live
// here (not in arc/host.gleam, which re-exports them) because State's
// fields reference them and host.gleam imports state.
//
// The opaque terms (WaiterKey, WaiterHandle, ClaimedWaiter) are produced
// exclusively by the data-only ETS registry arc_waiter_ffi.erl and are
// safe to send between processes. See arc/host.gleam for the full written
// contract, including the FFI module-layout rules and the wake-injection
// entry point.
// ============================================================================

/// Opaque cross-process identity of a buffer's WaiterList (an Erlang term:
/// the SAB's atomics ref for shared storage, a pid-scoped heap id
/// otherwise — see arc_waiter_ffi:shared_buffer_key/local_buffer_key).
/// Compared structurally; safe to send between processes.
pub type WaiterKey

/// Opaque handle to one registered waiterlist entry (its ETS key plus the
/// unique message ref a notifier will address). Produced by
/// arc_waiter_ffi:insert_waiter; consumed by the embedder's blocking wait.
pub type WaiterHandle

/// A remote waiter atomically claimed by Atomics.notify's waiterlist take
/// (opaque Erlang term: the waiter's pid, message ref, WaiterKey and byte
/// index). Claiming is what counts the waiter as woken; DELIVERING the
/// wake message is the embedder's job via `host_deliver_wake`.
pub type ClaimedWaiter

/// One blocking sync Atomics.wait, handed to the embedder's `SyncWaitFn`
/// after core has registered the waiterlist entry and re-read the cell
/// (so no wakeup can be lost — see arc_waiter_ffi.erl's module doc for
/// the lock-free insert/re-read/block ordering).
pub type WaitRequest {
  WaitRequest(
    /// The waiterlist entry to block on. The embedder resolves the
    /// notify-vs-timeout race by ets:take of this entry on timeout
    /// (took it ourselves = TimedOut; gone = a notifier claimed us and
    /// its message is in flight = Ok).
    handle: WaiterHandle,
    /// WaiterList identity, for diagnostics and embedder-side policy.
    key: WaiterKey,
    /// Byte offset within the buffer (matches the notify side).
    byte_index: Int,
    /// Milliseconds to block; `None` = infinite (the embedder may clamp —
    /// e.g. the BEAM `receive after` 2^32-1 ms ceiling).
    timeout_ms: Option(Int),
  )
}

/// Result of an embedder blocking wait: woken by a notify, or deadline
/// elapsed with no claim. Maps 1:1 to the JS "ok"/"timed-out" strings
/// ("not-equal" is decided by core before the capability is called).
pub type WaitOutcome {
  WaitOk
  WaitTimedOut
}

/// Embedder capability: block the calling agent until the waiterlist entry
/// in the request is notified or the timeout elapses. The BLOCKING happens
/// in the embedder (BEAM selective receive in arc/beam, the harness's
/// worker mailbox in test262) — never in core.
pub type SyncWaitFn =
  fn(WaitRequest) -> WaitOutcome

/// Embedder capability: deliver wake messages to remote waiters claimed by
/// Atomics.notify. Core claims atomically (data-only ETS take) and counts;
/// the embedder owns the actual `Pid ! {arc_notify, Ref, Key, ByteIndex}`
/// sends.
pub type DeliverWakeFn =
  fn(List(ClaimedWaiter)) -> Nil

/// The internal VM executor state. Public so builtins can receive and return it,
/// giving them full access to the runtime.
pub type State {
  State(
    stack: List(JsValue),
    locals: TupleArray(JsValue),
    constants: TupleArray(JsValue),
    func: FuncTemplate,
    code: TupleArray(Op),
    heap: Heap,
    pc: Int,
    call_stack: List(SavedFrame),
    try_stack: List(TryFrame),
    builtins: Builtins,
    /// Per-realm constants and near-constants. Nested so the per-instruction
    /// State copy doesn't pay for fields that almost never change.
    ctx: RealmCtx,
    /// §13.3.12 NewTarget for the current frame — `JsObject(ref)` when entered
    /// via `[[Construct]]`, `JsUndefined` for `[[Call]]`. Read by setup_locals
    /// to seed the RefNewTarget lexical slot, and by native ctors directly.
    new_target: JsValue,
    /// Original arguments passed to the current function call. Consumed by
    /// CreateArguments opcode to build the arguments object.
    call_args: List(JsValue),
    /// Promise microtask job queue. Jobs enqueued during promise operations,
    /// drained after script completes.
    job_queue: JobQueue(value.Job),
    /// ES2024 HostPromiseRejectionTracker: data_refs of promises rejected while
    /// [[PromiseIsHandled]] was false. Removed when a handler is later attached.
    /// Any remaining after job draining are reported as unhandled rejections.
    unhandled_rejections: List(Ref),
    /// §25.4.3.10 AddWaiter: pending Atomics.waitAsync waiters in FIFO order
    /// (oldest first). Atomics.notify settles matching entries with "ok".
    atomics_waiters: List(value.AtomicsWaiter),
    /// Pending host timers scheduled by the global setTimeout, in insertion
    /// order. The event loop fires due timers (earliest deadline first) when
    /// the microtask queue is empty, and sleeps until the earliest deadline
    /// instead of exiting while timers are pending.
    timers: List(value.HostTimer),
    /// Next id handed out by setTimeout (monotonically increasing from 1).
    next_timer_id: Int,
    /// Count of in-flight external promises created via `host.suspend` and
    /// not yet settled via `host.resume`. Core never blocks on this — it's
    /// the embedder's macrotask loop that reads it to decide when to stop.
    outstanding: Int,
    /// Current call stack depth. Incremented on function entry, decremented on return.
    /// Throws RangeError when exceeding limits.max_call_depth.
    call_depth: Int,
    /// Sloppy direct-eval var-injection dict (EvalEnvSlot ref). Allocated the
    /// first time a sloppy direct eval runs in this frame. `var` declarations
    /// in the eval'd code write here; subsequent reads/writes in the caller
    /// check here before global. Frame-local — saved to SavedFrame on call,
    /// restored on return. None for frames with no direct eval.
    eval_env: Option(Ref),
    /// Source line of the instruction currently executing, updated by the
    /// SetLine opcode. Captured (per active frame) when an Error object is
    /// constructed to build `Error.prototype.stack`. 0 before any SetLine.
    current_line: Int,
    /// Agent Record [[CanBlock]] (§9.7): whether this agent may block
    /// (suspend) in a sync Atomics.wait. True for the main agent and spawned
    /// agent children; the test262 runner sets it False for tests flagged
    /// CanBlockIsFalse. Read by DoWait step 10 (§25.4.3.14): sync mode
    /// throws a TypeError when AgentCanSuspend() is false.
    can_block: Bool,
    /// Embedder-installed blocking-wait capability for sync Atomics.wait
    /// (§25.4.3.14 DoWait steps 11-27). Core registers the waiterlist
    /// entry and re-reads the cell, then calls this to block. `None` =
    /// this host cannot suspend the agent: sync wait is treated exactly
    /// like `can_block == False` (DoWait step 10 TypeError) — there is no
    /// bounded fallback, because a wait that silently can't be woken is
    /// worse than an eager error. Installed by arc/beam (BEAM mailbox
    /// receive) and the test262 harness; default None.
    host_sync_wait: Option(SyncWaitFn),
    /// Embedder-installed wake delivery for Atomics.notify. Core's
    /// waiterlist take CLAIMS remote waiters atomically (so they count as
    /// woken, §25.4.11 step 10) and hands them here for actual message
    /// delivery. `None` = claimed remote wakes are dropped undelivered
    /// (only headless states lack the capability; every shipped embedder
    /// installs it alongside host_sync_wait); default None.
    host_deliver_wake: Option(DeliverWakeFn),
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
    ctx: RealmCtx(
      ..parent.ctx,
      lexical_globals: child.ctx.lexical_globals,
      template_objects: child.ctx.template_objects,
      // Realms registered during the child execution (ShadowRealm /
      // $262.createRealm constructors) must survive the merge.
      realms: child.ctx.realms,
    ),
    job_queue: job_queue.append(child.job_queue, extra_jobs),
    outstanding: child.outstanding,
    atomics_waiters: child.atomics_waiters,
    timers: child.timers,
    next_timer_id: child.next_timer_id,
  )
}

/// Count of unsettled `host.suspend` promises. Embedder loops exit at 0.
pub fn outstanding(s: State) -> Int {
  s.outstanding
}

/// Call ctx.call_fn (re-entrant JS function call), handling the function field access.
pub fn call(
  state: State,
  callee: JsValue,
  this_val: JsValue,
  args: List(JsValue),
) -> Result(#(JsValue, State), #(JsValue, State)) {
  let f = state.ctx.call_fn
  f(state, callee, this_val, args)
}

/// Call ctx.construct_fn (re-entrant `new target(...args)`). newTarget = target.
pub fn construct(
  state: State,
  target: JsValue,
  args: List(JsValue),
) -> Result(#(JsValue, State), #(JsValue, State)) {
  let f = state.ctx.construct_fn
  f(state, target, args, target)
}

/// Call ctx.construct_fn with explicit newTarget (Reflect.construct).
pub fn construct_with_target(
  state: State,
  target: JsValue,
  args: List(JsValue),
  new_target: JsValue,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  let f = state.ctx.construct_fn
  f(state, target, args, new_target)
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

// ============================================================================
// Stack traces (Error.prototype.stack)
// ============================================================================

/// Pseudo-filename used in stack frames. Arc has no real source paths reaching
/// the VM yet, so every frame is attributed to "script:<line>".
const stack_source = "script"

/// Default Error.stackTraceLimit (V8 parity). Used when the constructor's
/// `stackTraceLimit` property is missing or not a number.
const default_stack_limit = 10

/// Build a V8-style stack-trace string. `header` is the first line — the error's
/// `name: message` (or just `name`). The frames are the active call chain at the
/// moment the error is constructed: the executing function first, then its
/// callers. Honors Error.stackTraceLimit. Lines look like:
///
///   TypeError: x is not a function
///       at inner (script:3)
///       at outer (script:7)
///       at script:10
///
pub fn build_stack_trace(state: State, header: String) -> String {
  let limit = stack_trace_limit(state)
  let frames =
    [
      #(state.func.name, state.current_line),
      ..list.map(state.call_stack, fn(f) { #(f.func.name, f.current_line) })
    ]
    |> list.take(limit)
  case list.map(frames, format_frame) {
    [] -> header
    lines -> header <> "\n" <> string.join(lines, "\n")
  }
}

/// Format one frame: `    at name (script:line)`, or `    at script:line` when
/// the function is anonymous (e.g. the top-level script body).
fn format_frame(frame: #(Option(String), Int)) -> String {
  let #(name, line) = frame
  let loc = case line {
    0 -> stack_source
    _ -> stack_source <> ":" <> int.to_string(line)
  }
  case name {
    option.Some(n) -> "    at " <> n <> " (" <> loc <> ")"
    option.None -> "    at " <> loc
  }
}

/// Read Error.stackTraceLimit off the Error constructor. Non-numbers fall back
/// to the default; Infinity means "no limit"; negatives clamp to 0 (no frames).
fn stack_trace_limit(state: State) -> Int {
  case heap.read(state.heap, state.builtins.error.constructor) {
    option.Some(value.ObjectSlot(properties:, ..)) ->
      case dict.get(properties, value.Named("stackTraceLimit")) {
        Ok(value.DataProperty(value: value.JsNumber(value.Finite(n)), ..)) ->
          int.max(0, float.truncate(n))
        Ok(value.DataProperty(value: value.JsNumber(value.Infinity), ..)) ->
          // Effectively unbounded — far above any real call depth.
          1_000_000
        _ -> default_stack_limit
      }
    _ -> default_stack_limit
  }
}

/// Build a stack trace from the current call chain and store it on `err`.
///
/// Error instances (kind ErrorObject, the [[ErrorData]] internal slot) keep
/// the trace IN the slot — surfaced by the `Error.prototype.stack` accessor
/// (error-stack-accessor proposal), so instances have no own `stack` property.
/// Non-error objects (Error.captureStackTrace targets) get a non-enumerable
/// own `stack` data property, matching V8. No-op when `err` is not an object.
pub fn attach_stack(state: State, err: JsValue, header: String) -> State {
  case err {
    value.JsObject(ref) -> {
      let trace = build_stack_trace(state, header)
      let heap =
        heap.update(state.heap, ref, fn(slot) {
          case slot {
            value.ObjectSlot(kind: value.ErrorObject(_), ..) ->
              value.ObjectSlot(..slot, kind: value.ErrorObject(stack: trace))
            value.ObjectSlot(properties:, ..) ->
              value.ObjectSlot(
                ..slot,
                properties: dict.insert(
                  properties,
                  value.Named("stack"),
                  value.builtin_property(value.JsString(trace)),
                ),
              )
            _ -> slot
          }
        })
      State(..state, heap:)
    }
    _ -> state
  }
}

/// First line of a stack trace / error toString: `name: message`, or just
/// `name` when the message is empty.
pub fn error_header(name: String, msg: String) -> String {
  case msg {
    "" -> name
    _ -> name <> ": " <> msg
  }
}

/// Core error allocator: allocate a JS error via `make`, attach a stack
/// trace headed by `name: msg`, and return the updated state plus the
/// error value. All error helpers below are thin shells over this.
fn alloc_error(
  state: State,
  make: fn(Heap, Builtins, String) -> #(Heap, JsValue),
  name: String,
  msg: String,
) -> #(State, JsValue) {
  let #(heap, err) = make(state.heap, state.builtins, msg)
  let state = attach_stack(State(..state, heap:), err, error_header(name, msg))
  #(state, err)
}

/// Convenience wrapper: allocate a TypeError on the heap and return it as
/// an Error result. Shared by all builtin modules to avoid boilerplate
/// around common.make_type_error + state threading.
pub fn type_error(
  state: State,
  msg: String,
) -> #(State, Result(JsValue, JsValue)) {
  let #(state, err) =
    alloc_error(state, common.make_type_error, "TypeError", msg)
  #(state, Error(err))
}

pub fn range_error(
  state: State,
  msg: String,
) -> #(State, Result(JsValue, JsValue)) {
  let #(state, err) =
    alloc_error(state, common.make_range_error, "RangeError", msg)
  #(state, Error(err))
}

/// Like type_error, but allocates the TypeError from the given realm's
/// builtins instead of the current realm's. Cross-realm-aware natives
/// (e.g. set Error.prototype.stack) must throw the %TypeError% of the
/// realm the native function belongs to, not the calling realm's.
pub fn type_error_with_builtins(
  state: State,
  builtins: Builtins,
  msg: String,
) -> #(State, Result(JsValue, JsValue)) {
  let #(heap, err) = common.make_type_error(state.heap, builtins, msg)
  let state =
    attach_stack(State(..state, heap:), err, error_header("TypeError", msg))
  #(state, Error(err))
}

/// Allocate a TypeError as the bare #(thrown, state) tuple used by ops-level
/// results `Result(_, #(JsValue, State))` (e.g. set_value's error arm).
pub fn type_error_value(state: State, msg: String) -> #(JsValue, State) {
  let #(state, err) =
    alloc_error(state, common.make_type_error, "TypeError", msg)
  #(err, state)
}

/// Allocate a RangeError as the bare #(thrown, state) tuple used by ops-level
/// results `Result(_, #(JsValue, State))`.
pub fn range_error_value(state: State, msg: String) -> #(JsValue, State) {
  let #(state, err) =
    alloc_error(state, common.make_range_error, "RangeError", msg)
  #(err, state)
}

/// Allocate a ReferenceError and return it as the bare #(thrown, state) tuple
/// used by ops-level results `Result(_, #(JsValue, State))` (e.g. get_value's
/// error arm). Used for module-namespace TDZ access (§10.4.6 [[Get]]).
pub fn reference_error_value(state: State, msg: String) -> #(JsValue, State) {
  let #(state, err) =
    alloc_error(state, common.make_reference_error, "ReferenceError", msg)
  #(err, state)
}

/// Allocate a ReferenceError in the builtin-shape `#(State, Result)`.
pub fn reference_error(
  state: State,
  msg: String,
) -> #(State, Result(JsValue, JsValue)) {
  let #(state, err) =
    alloc_error(state, common.make_reference_error, "ReferenceError", msg)
  #(state, Error(err))
}

/// Allocate a JS Array with the given values and return it as Ok.
/// Collapses the common alloc_array → State(..state, heap:) → Ok(JsObject) triple.
pub fn ok_array(
  state: State,
  values: List(JsValue),
) -> #(State, Result(JsValue, JsValue)) {
  let #(heap, ref) =
    common.alloc_array(state.heap, values, state.builtins.array.prototype)
  #(State(..state, heap:), Ok(value.JsObject(ref)))
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
) -> Result(a, #(StepResult, JsValue, State)) {
  let #(state, err) =
    alloc_error(state, common.make_type_error, "TypeError", msg)
  Error(#(Thrown, err, state))
}

/// Allocate a JS RangeError and return it as a step-level thrown error.
pub fn throw_range_error(
  state: State,
  msg: String,
) -> Result(a, #(StepResult, JsValue, State)) {
  let #(state, err) =
    alloc_error(state, common.make_range_error, "RangeError", msg)
  Error(#(Thrown, err, state))
}

/// Allocate a JS ReferenceError and return it as a step-level thrown error.
pub fn throw_reference_error(
  state: State,
  msg: String,
) -> Result(a, #(StepResult, JsValue, State)) {
  let #(state, err) =
    alloc_error(state, common.make_reference_error, "ReferenceError", msg)
  Error(#(Thrown, err, state))
}

/// Bridge from inner helpers that return Result(a, #(JsValue, State))
/// to the step function's Result(a, #(StepResult, JsValue, State)).
pub fn rethrow(
  res: Result(a, #(JsValue, State)),
) -> Result(a, #(StepResult, JsValue, State)) {
  result.map_error(res, fn(err) {
    let #(thrown, state) = err
    #(Thrown, thrown, state)
  })
}
