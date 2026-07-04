import arc/vm/builtins/common.{type Builtins}
import arc/vm/completion.{type SuspendKind}
import arc/vm/heap
import arc/vm/internal/job_queue.{type JobQueue}
import arc/vm/internal/tuple_array.{type TupleArray}
import arc/vm/key.{Named}
import arc/vm/limits
import arc/vm/opcode.{type Op, type TryKind}
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
// host-function closures typed against State without an import cycle, and over
// `host` for embedder-defined opaque heap values (HostObject). This is where
// the recursive knot gets tied — State refers to itself through Heap, with
// `host` threaded alongside. The default embedding uses `host = value.Empty`.

pub type Heap(host) =
  heap.Heap(State(host), host)

pub type HeapSlot(host) =
  value.HeapSlot(State(host), host)

pub type ExoticKind(host) =
  value.ExoticKind(State(host), host)

pub type NativeFnSlot(host) =
  value.NativeFnSlot(State(host))

/// Signature for host-provided native functions installed via engine.define_fn
/// or engine.define_namespace. Receives (args, this, state), returns
/// (new_state, Ok(return_value) | Error(thrown_value)).
pub type HostFn(host) =
  fn(List(JsValue), JsValue, State(host)) ->
    #(State(host), Result(JsValue, JsValue))

/// Exception handler frame, pushed by PushTry. `kind` is copied straight off
/// the opcode: it says whether unwinding a *return* completion past this frame
/// must run a finally subroutine, close a live iterator, or just skip it
/// (see `opcode.TryKind` and `generators.find_next_return_handler`).
pub type TryFrame {
  TryFrame(catch_target: Int, stack_depth: Int, kind: TryKind)
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
pub type RealmCtx(host) {
  RealmCtx(
    /// DeclarativeRecord: let/const at global scope. NOT on globalThis. Checked
    /// first. Each binding is `Let`/`Const` (const rejects assignment) wrapping
    /// `JsUninitialized` while in TDZ, then its bound value.
    lexical_globals: dict.Dict(String, value.LexicalGlobal),
    /// ObjectRecord: Ref to globalThis heap object. var/function/builtins live here.
    global_object: Ref,
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
    call_fn: fn(State(host), JsValue, JsValue, List(JsValue)) ->
      Result(#(JsValue, State(host)), #(JsValue, State(host))),
    /// Re-entrant construct mechanism — `new target(...args)` from native code.
    /// 4th arg is newTarget (§10.1.13). Set by the VM executor (wraps do_construct).
    construct_fn: fn(State(host), JsValue, List(JsValue), JsValue) ->
      Result(#(JsValue, State(host)), #(JsValue, State(host))),
    /// Canonical §7.1.4 ToNumber, re-entrant (an object argument runs
    /// @@toPrimitive/valueOf/toString user code). Set by the VM executor to
    /// `coerce.js_to_number`. Exists for the same reason as `call_fn`: the
    /// integer-indexed [[Set]] (§10.4.5.16 IntegerIndexedElementSet, in
    /// ops/typed_array_elements, reached from the MOP in ops/object) must
    /// convert the stored value, but ops/coerce is built ON TOP of the MOP
    /// and cannot be imported from below it. Routing through this field is
    /// what keeps the engine at exactly ONE ToNumber — never a private
    /// re-implementation on the store path.
    to_number_fn: fn(State(host), JsValue) ->
      Result(#(value.JsNum, State(host)), #(JsValue, State(host))),
    /// Canonical §7.1.13 ToBigInt, re-entrant. Set by the VM executor to
    /// `coerce.to_bigint`. Same layering inversion as `to_number_fn`; used
    /// by BigInt64/BigUint64 element stores.
    to_bigint_fn: fn(State(host), JsValue) ->
      Result(#(Int, State(host)), #(JsValue, State(host))),
    /// Pre-built sentinel frame template (bytecode = [Return, Return]) used by
    /// the re-entrant call/construct callbacks to drive a callee to completion
    /// on an isolated stack. Built once at state init — these callbacks run
    /// per element in hot paths (Array.prototype.map etc.), so rebuilding the
    /// template each call is measurable.
    callback_sentinel: FuncTemplate,
    /// Embedder host capabilities (Atomics blocking wait / wake delivery),
    /// supplied ONCE at engine/realm construction and inherited by every
    /// derived State — eval/Function realms, $262.createRealm and agent
    /// children, ShadowRealms, and module bodies (static and dynamic import)
    /// all `..spread` or re-thread this RealmCtx, so a forgotten install is
    /// a compile error rather than a silent "cannot block".
    host_hooks: HostHooks,
  )
}

// ============================================================================
// Host Atomics capabilities — the blocking-wait / wake-delivery contract.
//
// Core never blocks on the BEAM mailbox and never sends wake messages
// itself. Both event-driven sides of Atomics.wait/notify are inverted into
// embedder-supplied capability functions, bundled into the `HostHooks`
// record carried on `RealmCtx.host_hooks` (the same inversion as
// `host.suspend`/`host.resume` for promises). The types live here (not in
// arc/host.gleam, which re-exports them) because RealmCtx's field
// references them and host.gleam imports state.
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
///
/// Alias of `value.WaiterKey`: `value.AtomicsWaiter` stores one, so the type
/// has to be declared where the waiter record can see it. Named here as well
/// because it is part of the host-capability contract (`WaitRequest.key`).
pub type WaiterKey =
  value.WaiterKey

/// Opaque handle to one registered waiterlist entry (its ETS key plus the
/// unique message ref a notifier will address). Produced by
/// arc_waiter_ffi:insert_waiter; consumed by the embedder's blocking wait.
pub type WaiterHandle

/// A remote waiter atomically claimed by Atomics.notify's waiterlist take
/// (opaque Erlang term: the waiter's pid, message ref, WaiterKey and byte
/// index). Claiming is what counts the waiter as woken; DELIVERING the
/// wake message is the embedder's job via the `deliver_wake` hook.
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

/// The two Atomics embedder capabilities, bundled. Both together, always
/// (arc/host contract clause 5): a host that can block an agent but cannot
/// deliver wakes — or vice versa — deadlocks its peer agents. Making them
/// one record under a single `Option` makes that half-configured embedder
/// unrepresentable: `HostHooks.atomics` is either BOTH capabilities or
/// neither.
pub type AtomicsCapabilities {
  AtomicsCapabilities(
    /// Blocking-wait capability for sync Atomics.wait (§25.4.3.14 DoWait
    /// steps 11-27). Core registers the waiterlist entry and re-reads the
    /// cell, then calls this to block IN THE EMBEDDER.
    sync_wait: SyncWaitFn,
    /// Wake delivery for Atomics.notify. Core's waiterlist take CLAIMS
    /// remote waiters atomically (so they count as woken, §25.4.11 step 10)
    /// and hands them here for actual message delivery.
    deliver_wake: DeliverWakeFn,
  )
}

/// The embedder's host capabilities, bundled into one record carried on
/// `RealmCtx.host_hooks`. Supplied exactly once at engine/realm construction
/// (an explicit argument to `interpreter.new_state`) and inherited by every
/// derived State via the `RealmCtx(..spread)`s, replacing the old pair of
/// loose, post-boot-installed `State.host_sync_wait` / `State.host_deliver_wake`
/// fields. NOT generic over `host`: no field mentions the embedder's
/// heap-value type.
///
/// `can_block` (Agent Record [[CanBlock]], §9.7) is deliberately NOT in here —
/// it is per-agent spec POLICY, not an embedder capability, and stays a
/// separate field on `State`. AgentCanSuspend() remains
/// `can_block && option.is_some(host_hooks.atomics)`.
pub type HostHooks {
  HostHooks(
    /// The Atomics blocking-wait + wake-delivery capability pair
    /// (`AtomicsCapabilities`), installed as one unit or not at all.
    /// `None` = this host cannot suspend the agent: sync Atomics.wait is
    /// treated exactly like `can_block == False` (DoWait step 10
    /// TypeError) — there is no bounded fallback, because a wait that
    /// silently can't be woken is worse than an eager error — and claimed
    /// remote wakes have nobody to deliver to (with no capability nothing
    /// remote can be blocked on this agent anyway).
    atomics: Option(AtomicsCapabilities),
    /// Monotonic clock in milliseconds. Used for Atomics waitAsync deadline
    /// bookkeeping and the event loop's timer wheel. NOT optional — every
    /// host has a clock — so it defaults to the BEAM monotonic clock
    /// (`arc_clock_ffi:monotonic_now/0`). An embedder overrides it to
    /// virtualise time (deterministic / mocked clocks).
    monotonic_now: fn() -> Int,
    /// Blocking sleep for the given number of milliseconds (ms <= 0 returns
    /// immediately). Used by the event loop to idle until the next timer /
    /// waitAsync deadline. Defaults to `timer:sleep/1`
    /// (`arc_clock_ffi:sleep/1`); an embedder overrides it alongside
    /// `monotonic_now` for a virtual clock, or to yield to its own scheduler
    /// instead of blocking the OS thread.
    sleep_ms: fn(Int) -> Nil,
    /// §16.2.1.8 HostLoadImportedModule: the embedder's dynamic-import host
    /// hook — a host function (see `arc/module_host.install_import_hook`)
    /// called with `(specifier, referrer?, phase?, resolve?, reject?)` that
    /// loads/links/evaluates the requested module graph. `None` = this host
    /// does not support dynamic import: `import()` rejects with a TypeError.
    ///
    /// For the eager phase the hook's return value settles the import
    /// promise. For the `defer` phase the hook is handed the promise's
    /// resolving functions and MUST settle through them itself — its return
    /// value is ignored (see `arc/vm/exec/dynamic_import.DeferHookOutcome`
    /// for the full contract, and `throw` to reject in either phase).
    ///
    /// This is ENGINE state, not a globalThis property: guest JS can neither
    /// read nor replace it. The `Ref` inside the `JsValue` is pinned with
    /// `heap.root` at install time (like `RealmCtx.template_objects` entries),
    /// because nothing else in the heap reaches it.
    import_hook: Option(JsValue),
    /// §16.2.1.8 referencingScriptOrModule: the resolved specifier of the
    /// module whose body is currently executing, set by the module evaluator
    /// on each body's freshly booted State (`arc/module.run_module_with_referrer`)
    /// and read at ImportCall time so a nested `import()` resolves relative to
    /// the importing MODULE. `None` = script code: the host hook falls back to
    /// its install-time entry referrer.
    ///
    /// ENGINE state, never a globalThis property — guest JS cannot forge a
    /// referrer to escape the module loader's resolution root.
    import_referrer: Option(String),
  )
}

/// Default `monotonic_now` capability: the BEAM monotonic clock in
/// milliseconds — the same `arc_clock_ffi:monotonic_now/0` external the
/// event loop and the Atomics builtin use, so a default-hooks host is
/// behaviourally identical to one that never thinks about clocks.
@external(erlang, "arc_clock_ffi", "monotonic_now")
fn host_monotonic_now() -> Int

/// Default `sleep_ms` capability: `timer:sleep/1`, clamped to a no-op for
/// ms <= 0 — the same `arc_clock_ffi:sleep/1` external the event loop uses.
/// Always a BOUNDED idle: an untimed sync Atomics.wait blocks through
/// `HostHooks.atomics`'s embedder capability, never through this.
@external(erlang, "arc_clock_ffi", "sleep")
fn host_sleep_ms(ms: Int) -> Nil

/// The capability-free default: a host that can neither block an agent nor
/// deliver wakes, and that has no dynamic-import hook (so `import()` rejects
/// with a TypeError). "No capabilities" is the safe baseline — sync
/// Atomics.wait throws (AgentCanSuspend is false) rather than hanging. The
/// clock and sleep hooks are NOT capability-gated: they default to the real
/// `arc_clock_ffi` monotonic clock and `timer:sleep`, which is what every
/// host wants unless it is virtualising time.
pub fn default_host_hooks() -> HostHooks {
  HostHooks(
    atomics: option.None,
    monotonic_now: host_monotonic_now,
    sleep_ms: host_sleep_ms,
    import_hook: option.None,
    import_referrer: option.None,
  )
}

/// The internal VM executor state. Public so builtins can receive and return it,
/// giving them full access to the runtime.
pub type State(host) {
  State(
    stack: List(JsValue),
    locals: TupleArray(JsValue),
    constants: TupleArray(JsValue),
    func: FuncTemplate,
    code: TupleArray(Op),
    heap: Heap(host),
    pc: Int,
    call_stack: List(SavedFrame),
    try_stack: List(TryFrame),
    builtins: Builtins,
    /// Per-realm constants and near-constants. Nested so the per-instruction
    /// State copy doesn't pay for fields that almost never change.
    ctx: RealmCtx(host),
    /// §13.3.12 NewTarget for the current frame — `JsObject(ref)` when entered
    /// via `[[Construct]]`, `JsUndefined` for `[[Call]]`. Read by setup_frame
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
    /// throws a TypeError when AgentCanSuspend() is false. Spec POLICY, not
    /// an embedder capability — the capabilities themselves (blocking wait,
    /// wake delivery) live on `ctx.host_hooks`. AgentCanSuspend() is
    /// `can_block && option.is_some(ctx.host_hooks.atomics)`.
    can_block: Bool,
  )
}

// -- Child-execution seed / merge --------------------------------------------
// A child execution state (eval / Function realm, $262.evalScript,
// ShadowRealm.prototype.evaluate, generator/async resumption, job re-entry)
// runs with its OWN copy of the agent-wide event-loop queues and its own
// `RealmCtx`. `seed_child` is the ONE place that defines what a child
// inherits from its caller; `merge_child` is its exact mirror, threading the
// same set back after the child has run. Every call site must use the pair —
// a hand-rolled field-by-field copy is how a queue gets silently reset
// (dropping the caller's outstanding host promises, Atomics waiters, or
// pending unhandled-rejection reports) or an agent-wide table gets forked
// (giving a child realm its own tagged-template cache).
//
// EXCEPTION: children that finish with a NESTED `event_loop.drain_jobs`
// ($262.evalScript, ShadowRealm.prototype.evaluate) must use the
// `seed_draining_child` / `merge_draining_child` pair below instead — see
// its doc for why the caller's Atomics waiters and pending
// unhandled-rejection reports must never reach a nested drain.
//
// WHICH `RealmCtx` FIELDS THE PAIRS COPY, and why:
//
//   AGENT-WIDE — one table per agent, shared by every realm; seeded in from
//   the caller and threaded back out here, so no boot site can fork them:
//     * `realms`           — Ref → Builtins for every live realm slot.
//     * `template_objects` — GetTemplateObject cache (§13.2.8.4), keyed by
//                            globally unique compile-time site ids.
//
//   AGENT-WIDE, but NOT copied here — the cross-realm callers rebuild it
//   from the target `value.RealmSlot` before booting the child (evalScript
//   adopts the child realm's table, ShadowRealm the union), so it is
//   passed positionally into `new_state` instead of spread from the caller.
//   The one place it is unconditionally adopted from a finished child is
//   `merge_globals` (same-realm children, whose table only ever grows):
//     * `symbol_registry` (§20.4.2.2)
//   Being per-State rather than heap-resident is why a boot site that starts
//   from `dict.new()` (`exec/entry.run_module`) can still reset it. A user
//   symbol's description travels inside `value.UserSymbol`, so it never
//   depends on any of these tables.
//
//   REALM-LOCAL — belong to the child's own realm and must NOT leak into the
//   caller's ctx (a cross-realm merge that adopted them would splice a
//   foreign realm's globals into the caller):
//     * `lexical_globals`, `global_object`
//
//   ENGINE PLUMBING — installed once by `new_state` / passed positionally,
//   never spread from a caller State:
//     * `call_fn`, `construct_fn`, `to_number_fn`, `to_bigint_fn`,
//       `callback_sentinel`, `host_hooks`

/// Seed the agent-wide state a child execution must inherit from its caller:
/// the pending job queue, the outstanding `host.suspend` promise count, the
/// Atomics.waitAsync waiter list, the pending unhandled-rejection list, and
/// the agent-wide `RealmCtx` tables (realm registry, tagged-template cache).
/// MUST stay the mirror image of `merge_child`.
pub fn seed_child(child: State(host), caller: State(host)) -> State(host) {
  State(
    ..child,
    ctx: RealmCtx(
      ..child.ctx,
      realms: caller.ctx.realms,
      template_objects: caller.ctx.template_objects,
    ),
    job_queue: caller.job_queue,
    outstanding: caller.outstanding,
    atomics_waiters: caller.atomics_waiters,
    unhandled_rejections: caller.unhandled_rejections,
  )
}

/// The one place the non-draining merges name the event-loop fields a
/// finished child threads back. `ctx` and `queue` are passed in so each merge
/// builds exactly ONE `RealmCtx` and ONE `State` — `merge_globals` sits on
/// the generator-resume / await-suspend path, so a discarded record copy per
/// call is not free.
fn merge_child_into(
  caller: State(host),
  child: State(host),
  ctx: RealmCtx(host),
  queue: JobQueue(value.Job),
) -> State(host) {
  State(
    ..caller,
    ctx:,
    job_queue: queue,
    outstanding: child.outstanding,
    atomics_waiters: child.atomics_waiters,
    unhandled_rejections: child.unhandled_rejections,
  )
}

/// Thread the agent-wide state back from a finished child execution to its
/// caller. MUST stay the mirror image of `seed_child`. The child's
/// realm-LOCAL ctx (lexical globals, global object) is deliberately not
/// adopted: same-realm callers want `merge_globals`, while cross-realm
/// callers (evalScript / ShadowRealm) write the child realm's lexical globals
/// back to its own `value.RealmSlot`.
pub fn merge_child(caller: State(host), child: State(host)) -> State(host) {
  merge_child_into(
    caller,
    child,
    RealmCtx(
      ..caller.ctx,
      // Realms registered and template objects cached during the child
      // execution are agent-wide and must survive the merge.
      realms: child.ctx.realms,
      template_objects: child.ctx.template_objects,
    ),
    child.job_queue,
  )
}

/// `seed_child` variant for the two child executions whose completion runs a
/// NESTED, non-yielding `event_loop.drain_jobs` ($262.evalScript,
/// ShadowRealm.prototype.evaluate). Only the job queue and the outstanding
/// `host.suspend` count are handed over. The caller's Atomics.waitAsync
/// waiters and pending unhandled-rejection reports MUST stay behind:
/// - a nested drain that inherited the caller's waiters would sleep until
///   their deadlines (blocking the agent inside the synchronous eval) and
///   force-settle them "timed-out", even though the caller's own — yielding —
///   loop would have delivered the notify in time;
/// - a nested drain that inherited the caller's pending rejection refs would
///   report them to stderr mid-job and clear the list, defeating the
///   "handler attached later in the same turn" case. The unhandled-rejection
///   checkpoint belongs to the caller's own end-of-job drain.
/// The child starts both lists empty; whatever it registers is unioned back
/// by `merge_draining_child`, the mirror of this function. The agent-wide
/// `RealmCtx` tables are seeded exactly as in `seed_child`.
pub fn seed_draining_child(
  child: State(host),
  caller: State(host),
) -> State(host) {
  State(
    ..child,
    ctx: RealmCtx(
      ..child.ctx,
      realms: caller.ctx.realms,
      template_objects: caller.ctx.template_objects,
    ),
    job_queue: caller.job_queue,
    outstanding: caller.outstanding,
    atomics_waiters: [],
    unhandled_rejections: [],
  )
}

/// Mirror of `seed_draining_child`: thread the job queue, outstanding count
/// and agent-wide `RealmCtx` tables back, and APPEND the Atomics waiters /
/// unhandled-rejection refs the child registered (and its nested drain left
/// unsettled / unreported) to the caller's own lists — which the child never
/// saw and must not clobber.
pub fn merge_draining_child(
  caller: State(host),
  child: State(host),
) -> State(host) {
  State(
    ..caller,
    ctx: RealmCtx(
      ..caller.ctx,
      realms: child.ctx.realms,
      template_objects: child.ctx.template_objects,
    ),
    job_queue: child.job_queue,
    outstanding: child.outstanding,
    atomics_waiters: list.append(caller.atomics_waiters, child.atomics_waiters),
    unhandled_rejections: list.append(
      caller.unhandled_rejections,
      child.unhandled_rejections,
    ),
  )
}

/// Thread VM-global state from a SAME-REALM child execution back to parent:
/// everything `merge_child` covers (event-loop set + agent-wide RealmCtx
/// tables), plus the two realm-local/agent-wide extras only a same-realm
/// caller may adopt — the child's lexical globals and its symbol tables.
/// Does NOT thread heap (caller handles separately since it's often further
/// mutated).
pub fn merge_globals(
  parent: State(host),
  child: State(host),
  extra_jobs: List(value.Job),
) -> State(host) {
  merge_child_into(
    parent,
    child,
    RealmCtx(
      ..parent.ctx,
      // The agent-wide tables `merge_child` threads back, spelled out here so
      // this hot path builds the ctx exactly once.
      realms: child.ctx.realms,
      template_objects: child.ctx.template_objects,
      // Same realm, so the child's global lexical bindings ARE the parent's.
      lexical_globals: child.ctx.lexical_globals,
      // The Symbol.for registry is agent-wide and only ever grows, so the
      // child's table is a superset of the parent's: symbols registered
      // during the child execution must survive the merge.
      symbol_registry: child.ctx.symbol_registry,
    ),
    job_queue.append(child.job_queue, extra_jobs),
  )
}

/// Resume the parent after a same-realm child execution finished: everything
/// `merge_globals` threads back, PLUS the child's heap. `merge_globals` alone
/// deliberately leaves the heap alone, so forgetting the `heap:` re-attach
/// silently reverts every object the child allocated — this makes it
/// unrepresentable.
pub fn adopt_child(parent: State(host), child: State(host)) -> State(host) {
  State(..merge_globals(parent, child, []), heap: child.heap)
}

/// Count of unsettled `host.suspend` promises. Embedder loops exit at 0.
pub fn outstanding(s: State(host)) -> Int {
  s.outstanding
}

// ----------------------------------------------------------------------------
// Realm lookup — `ctx.realms` lives here, so the search over it does too.
// ----------------------------------------------------------------------------

/// The realm whose intrinsics satisfy `matches`, as `#(realm slot ref, its
/// Builtins)`. `None` means no REGISTERED realm matched — which for the
/// running realm is normal (a realm is only entered into `ctx.realms` once it
/// has been reified as a `RealmSlot`), so callers must decide for themselves
/// whether falling back to `state.builtins` is right, instead of an
/// `unwrap` quietly deciding it for them.
fn find_realm(
  state: State(host),
  matches: fn(Builtins) -> Bool,
) -> Option(#(Ref, Builtins)) {
  use acc, realm_ref, b <- dict.fold(state.ctx.realms, option.None)
  case acc {
    option.Some(_) -> acc
    option.None ->
      case matches(b) {
        True -> option.Some(#(realm_ref, b))
        False -> option.None
      }
  }
}

/// Find a realm by its `%Function.prototype%` — the marker every
/// realm-attributed native token carries, unique per `Builtins`. Used to
/// attribute a builtin method to the realm it was DEFINED in rather than the
/// realm that happens to be running (`otherRealm.JSON.parse('{')` throws
/// `otherRealm.SyntaxError`).
pub fn builtins_of_function_proto(
  state: State(host),
  fn_proto: Ref,
) -> Option(#(Ref, Builtins)) {
  find_realm(state, fn(b) { b.function.prototype == fn_proto })
}

/// Find a realm by its `%Error.prototype%` — the marker the `Error.prototype`
/// stack accessor carries.
pub fn builtins_of_error_proto(
  state: State(host),
  error_proto: Ref,
) -> Option(#(Ref, Builtins)) {
  find_realm(state, fn(b) { b.error.prototype.id == error_proto.id })
}

/// Call ctx.call_fn (re-entrant JS function call), handling the function field access.
pub fn call(
  state: State(host),
  callee: JsValue,
  this_val: JsValue,
  args: List(JsValue),
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  let f = state.ctx.call_fn
  f(state, callee, this_val, args)
}

/// Call ctx.construct_fn (re-entrant `new target(...args)`). newTarget = target.
pub fn construct(
  state: State(host),
  target: JsValue,
  args: List(JsValue),
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  let f = state.ctx.construct_fn
  f(state, target, args, target)
}

/// Call ctx.construct_fn with explicit newTarget (Reflect.construct).
pub fn construct_with_target(
  state: State(host),
  target: JsValue,
  args: List(JsValue),
  new_target: JsValue,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  let f = state.ctx.construct_fn
  f(state, target, args, new_target)
}

/// Call a function or propagate thrown error. Use with `use` syntax:
///   use result, state <- state.try_call(state, callback, this_arg, [element, idx, arr])
pub fn try_call(
  state: State(host),
  callee: JsValue,
  this_val: JsValue,
  args: List(JsValue),
  cont: fn(JsValue, State(host)) -> #(State(host), Result(b, JsValue)),
) -> #(State(host), Result(b, JsValue)) {
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
  result: Result(#(a, State(host)), #(JsValue, State(host))),
  cont: fn(a, State(host)) -> #(State(host), Result(b, JsValue)),
) -> #(State(host), Result(b, JsValue)) {
  case result {
    Ok(#(val, state)) -> cont(val, state)
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// CPS helper for fallible state operations that return only an updated State
/// (no extra value). Use with `use` syntax:
///   use state <- state.try_state(some_operation(state, ...))
pub fn try_state(
  result: Result(State(host), #(JsValue, State(host))),
  cont: fn(State(host)) -> #(State(host), Result(b, JsValue)),
) -> #(State(host), Result(b, JsValue)) {
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
pub fn build_stack_trace(state: State(host), header: String) -> String {
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
fn stack_trace_limit(state: State(host)) -> Int {
  case heap.read(state.heap, state.builtins.error.constructor) {
    option.Some(value.ObjectSlot(properties:, ..)) ->
      case dict.get(properties, Named("stackTraceLimit")) {
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
pub fn attach_stack(
  state: State(host),
  err: JsValue,
  header: String,
) -> State(host) {
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
                  Named("stack"),
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

/// The kind of native JS error to allocate. VM-raised errors go through
/// `alloc_error` with one of these, so the constructor intrinsic and the
/// stack-trace header can never disagree (there is no way to pair
/// `%RangeError%` with the name "TypeError"), and the thrown error gets a
/// stack trace attached.
///
/// Do NOT call `common.make_*_error` from a context that has a `State` —
/// that skips `attach_stack` and produces an error with no `.stack`. The
/// only legitimate direct callers are `kind_ctor` below and module linking
/// (`module.gleam`), which runs at the heap level before any call stack
/// exists.
pub type ErrorKind {
  TypeErr
  RangeErr
  ReferenceErr
  SyntaxErr
}

/// Map an ErrorKind to its heap allocator and its `name` (the first word of
/// the stack-trace header). This is the single place the pairing exists.
fn kind_ctor(
  kind: ErrorKind,
) -> #(fn(Heap(host), Builtins, String) -> #(Heap(host), JsValue), String) {
  case kind {
    TypeErr -> #(common.make_type_error, "TypeError")
    RangeErr -> #(common.make_range_error, "RangeError")
    ReferenceErr -> #(common.make_reference_error, "ReferenceError")
    SyntaxErr -> #(common.make_syntax_error, "SyntaxError")
  }
}

/// Core error allocator: allocate a JS error of `kind`, attach a stack
/// trace headed by `name: msg`, and return the updated state plus the
/// error value. All error helpers below are thin shells over this.
fn alloc_error(
  state: State(host),
  kind: ErrorKind,
  msg: String,
) -> #(State(host), JsValue) {
  alloc_error_with_builtins(state, state.builtins, kind, msg)
}

/// Like alloc_error, but allocates from an explicit realm's builtins.
/// Cross-realm-aware natives must throw the intrinsic of the realm the
/// native function belongs to, not the calling realm's.
fn alloc_error_with_builtins(
  state: State(host),
  builtins: Builtins,
  kind: ErrorKind,
  msg: String,
) -> #(State(host), JsValue) {
  let #(make, name) = kind_ctor(kind)
  let #(heap, err) = make(state.heap, builtins, msg)
  let state = attach_stack(State(..state, heap:), err, error_header(name, msg))
  #(state, err)
}

/// Convenience wrapper: allocate a TypeError on the heap and return it as
/// an Error result. Shared by all builtin modules to avoid boilerplate
/// around error allocation + state threading.
pub fn type_error(
  state: State(host),
  msg: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(state, err) = alloc_error(state, TypeErr, msg)
  #(state, Error(err))
}

pub fn range_error(
  state: State(host),
  msg: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(state, err) = alloc_error(state, RangeErr, msg)
  #(state, Error(err))
}

/// Like type_error, but allocates the TypeError from the given realm's
/// builtins instead of the current realm's. Cross-realm-aware natives
/// (e.g. set Error.prototype.stack) must throw the %TypeError% of the
/// realm the native function belongs to, not the calling realm's.
pub fn type_error_with_builtins(
  state: State(host),
  builtins: Builtins,
  msg: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(state, err) = alloc_error_with_builtins(state, builtins, TypeErr, msg)
  #(state, Error(err))
}

/// Allocate an error of `kind` from an explicit realm's builtins as the bare
/// #(thrown, state) tuple. Cross-realm-aware natives that hand the error to
/// something other than the throw path (e.g. rejecting a promise) use this.
pub fn error_value_with_builtins(
  state: State(host),
  builtins: Builtins,
  kind: ErrorKind,
  msg: String,
) -> #(JsValue, State(host)) {
  let #(state, err) = alloc_error_with_builtins(state, builtins, kind, msg)
  #(err, state)
}

/// Allocate a TypeError as the bare #(thrown, state) tuple used by ops-level
/// results `Result(_, #(JsValue, State))` (e.g. set_value's error arm).
pub fn type_error_value(
  state: State(host),
  msg: String,
) -> #(JsValue, State(host)) {
  let #(state, err) = alloc_error(state, TypeErr, msg)
  #(err, state)
}

/// Allocate a RangeError as the bare #(thrown, state) tuple used by ops-level
/// results `Result(_, #(JsValue, State))`.
pub fn range_error_value(
  state: State(host),
  msg: String,
) -> #(JsValue, State(host)) {
  let #(state, err) = alloc_error(state, RangeErr, msg)
  #(err, state)
}

/// Allocate a ReferenceError and return it as the bare #(thrown, state) tuple
/// used by ops-level results `Result(_, #(JsValue, State))` (e.g. get_value's
/// error arm). Used for module-namespace TDZ access (§10.4.6 [[Get]]).
pub fn reference_error_value(
  state: State(host),
  msg: String,
) -> #(JsValue, State(host)) {
  let #(state, err) = alloc_error(state, ReferenceErr, msg)
  #(err, state)
}

/// Allocate a SyntaxError as the bare #(thrown, state) tuple used by ops-level
/// results `Result(_, #(JsValue, State))` (e.g. ToBigInt on a bad string).
pub fn syntax_error_value(
  state: State(host),
  msg: String,
) -> #(JsValue, State(host)) {
  let #(state, err) = alloc_error(state, SyntaxErr, msg)
  #(err, state)
}

/// Allocate a ReferenceError in the builtin-shape `#(State, Result)`.
pub fn reference_error(
  state: State(host),
  msg: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(state, err) = alloc_error(state, ReferenceErr, msg)
  #(state, Error(err))
}

/// Allocate a SyntaxError in the builtin-shape `#(State, Result)`.
pub fn syntax_error(
  state: State(host),
  msg: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(state, err) = alloc_error(state, SyntaxErr, msg)
  #(state, Error(err))
}

/// Allocate a JS Array with the given values and return it as Ok.
/// Collapses the common alloc_array → State(..state, heap:) → Ok(JsObject) triple.
pub fn ok_array(
  state: State(host),
  values: List(JsValue),
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(heap, ref) =
    common.alloc_array(state.heap, values, state.builtins.array.prototype)
  #(State(..state, heap:), Ok(value.JsObject(ref)))
}

/// Guard against array length exceeding Number.MAX_SAFE_INTEGER.
/// Throws TypeError (per spec §23.1.3.23/31/33) if length > 2^53-1.
pub fn guard_safe_length(
  state: State(host),
  length: Int,
  cont: fn() -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
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
  /// A coroutine suspension (`yield`/`await`) escaped a frame that cannot
  /// resume it — top-level script, eval, module body, re-entrant native call.
  /// `site` names the driver that received it.
  SuspensionLeak(site: String, kind: SuspendKind)
  /// An engine invariant was breached (a bug in the VM, never something a
  /// guest program can trigger). `site` names the code location that detected
  /// it; `detail` says what was expected vs. found.
  InternalError(site: String, detail: String)
}

/// Canonical human-readable rendering of a `VmError`. Every layer that
/// surfaces one to a user must go through this.
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
    completion.Yield -> "yield"
    completion.Await -> "await"
  }
}

/// Why one bytecode step stopped the frame instead of continuing to the next
/// instruction. Every variant carries the State it stopped in AND its own
/// payload — there is no shared `JsValue` slot whose meaning depends on the
/// tag (and none to fabricate for a `VmFailed`).
pub type StepExit(host) {
  /// A JS exception was raised. `unwind_to_catch` decides where it lands.
  Threw(JsValue, State(host))
  /// The outermost frame executed `Return` — the frame's normal completion.
  Returned(JsValue, State(host))
  /// A generator suspended. `YieldKind` says which opcode did it, and hence
  /// which stack/pc fixup the suspended state needs.
  Yielded(YieldKind, JsValue, State(host))
  /// An async function/generator hit `await`, waiting on a promise.
  Awaited(JsValue, State(host))
  /// An engine invariant broke. Never observable by guest code.
  VmFailed(VmError, State(host))
}

/// Which suspension opcode raised a `Yielded`, and therefore how the suspended
/// state's stack/pc must be fixed up before it is saved. Carrying this on the
/// exit is what lets the loop avoid re-reading `state.code[state.pc]` after
/// the step already returned.
pub type YieldKind {
  /// `InitialYield` — stack unchanged, pc advances past the opcode.
  InitialSuspend
  /// `Yield` — pop the yielded value, pc advances.
  PlainYield
  /// `YieldStar` — pop the `.next()` arg but keep the iterator, pc stays put
  /// so the resume re-executes the same opcode.
  DelegateYield
  /// `AsyncYieldStarResume` — drop the consumed result object and jump back
  /// to the `AsyncYieldStarNext` at `next_pc`.
  AsyncDelegateResume(next_pc: Int)
}

/// Rewrite the `State` a `StepExit` carries, leaving its tag and payload
/// alone (used where an error path must still commit a heap write).
pub fn map_exit_state(
  exit: StepExit(host),
  f: fn(State(host)) -> State(host),
) -> StepExit(host) {
  case exit {
    Threw(v, s) -> Threw(v, f(s))
    Returned(v, s) -> Returned(v, f(s))
    Yielded(k, v, s) -> Yielded(k, v, f(s))
    Awaited(v, s) -> Awaited(v, f(s))
    VmFailed(e, s) -> VmFailed(e, f(s))
  }
}

// ============================================================================
// Step-level error helpers
// ============================================================================

/// Allocate a JS TypeError and return it as a step-level thrown error.
pub fn throw_type_error(
  state: State(host),
  msg: String,
) -> Result(a, StepExit(host)) {
  let #(state, err) = alloc_error(state, TypeErr, msg)
  Error(Threw(err, state))
}

/// Allocate a JS RangeError and return it as a step-level thrown error.
pub fn throw_range_error(
  state: State(host),
  msg: String,
) -> Result(a, StepExit(host)) {
  let #(state, err) = alloc_error(state, RangeErr, msg)
  Error(Threw(err, state))
}

/// Allocate a JS ReferenceError and return it as a step-level thrown error.
pub fn throw_reference_error(
  state: State(host),
  msg: String,
) -> Result(a, StepExit(host)) {
  let #(state, err) = alloc_error(state, ReferenceErr, msg)
  Error(Threw(err, state))
}

/// Bridge from inner helpers that return Result(a, #(JsValue, State))
/// to the step function's Result(a, StepExit(host)).
pub fn rethrow(
  res: Result(a, #(JsValue, State(host))),
) -> Result(a, StepExit(host)) {
  result.map_error(res, fn(err) {
    let #(thrown, state) = err
    Threw(thrown, state)
  })
}
