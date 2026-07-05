//// The embedder capability contract: everything a host supplies ONCE at
//// engine/realm construction and that every derived State inherits through
//// `state.RealmCtx.host_hooks`.
////
//// Core never blocks on the BEAM mailbox and never sends wake messages
//// itself. Both event-driven sides of `Atomics.wait`/`notify` are inverted
//// into embedder-supplied capability functions (the same inversion as
//// `host.suspend`/`host.resume` for promises), and the clock is a hook so a
//// host can virtualise time.
////
//// This module is deliberately tiny and dependency-light — it sits BELOW
//// `arc/vm/state` (which stores a `HostHooks` on every realm) and is
//// re-exported by `arc/host` for embedders.
////
//// The opaque terms (`WaiterKey`, `WaiterHandle`, `ClaimedWaiter`) are
//// produced exclusively by the data-only ETS registry
//// `arc/vm/builtins/arc_waiter_ffi.erl` and are safe to send between
//// processes.

import arc/vm/internal/clock_ffi
import arc/vm/value.{type JsValue}
import gleam/option.{type Option}

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

/// The two Atomics embedder capabilities, bundled — always installed together
/// or not at all: a host that can block an agent but cannot deliver wakes (or
/// vice versa) deadlocks its peer agents. Making them one record under a
/// single `Option` on `HostHooks.atomics` makes the half-configured embedder
/// unrepresentable.
///
/// Construction: build ONE record and hand it to the engine/realm constructor
/// (the `host_hooks` argument of the entry/module/engine boot APIs) — never
/// to an already-running State. Every State derived from that realm —
/// eval/Function realms, `$262.createRealm` and `$262.agent` children,
/// ShadowRealms, module bodies including dynamic import — inherits the
/// record, so a forgotten install site is a COMPILE error, not a silent
/// "cannot block".
///
/// `can_block` (Agent Record [[CanBlock]], §9.7) is a SEPARATE, per-agent
/// spec policy carried on `State.can_block`. AgentCanSuspend() is
/// `can_block && option.is_some(host_hooks.atomics)`.
pub type AtomicsCapabilities {
  AtomicsCapabilities(
    /// Blocking-wait capability for sync Atomics.wait (§25.4.3.14 DoWait
    /// steps 11-27). Core registers the waiterlist entry (data-only ETS
    /// insert) and re-reads the cell — "not-equal" short-circuits, cancelling
    /// the entry — then calls this to block IN THE EMBEDDER until notified or
    /// timed out. The CanBlock TypeError check (DoWait step 10) stays first
    /// and unchanged; a MISSING capability is treated identically to
    /// `can_block == False` — there is no bounded fallback, because a wait
    /// that silently can't be woken is worse than an eager error.
    ///
    /// The embedder resolves the notify-vs-timeout race: on timeout, ets:take
    /// your own entry — got it = nobody claimed you = `WaitTimedOut`; gone =
    /// a notifier claimed you and its message is in flight = bounded
    /// flush-receive, then `WaitOk`.
    sync_wait: SyncWaitFn,
    /// Wake delivery for Atomics.notify. Core's waiterlist take CLAIMS
    /// remote waiters atomically FIFO (so they count as woken, §25.4.11
    /// step 10) and hands them here for actual message delivery
    /// (`Pid ! {arc_notify, Ref, Key, ByteIndex}` per claimed waiter).
    /// Because a claim is a PROMISE to wake, a realm with no capabilities
    /// never claims another agent's waiter — see `builtins/atomics.notify`.
    ///
    /// When such a message lands in the embedder's mailbox it is injected
    /// back into core with `exec/event_loop.inject_notify`.
    deliver_wake: DeliverWakeFn,
  )
}

/// The embedder's host capabilities, bundled into one record carried on
/// `state.RealmCtx.host_hooks`. Supplied exactly once at engine/realm
/// construction (an explicit argument to `interpreter.new_state`) and
/// inherited by every derived State via the `RealmCtx(..spread)`s. NOT
/// generic over `host`: no field mentions the embedder's heap-value type.
pub type HostHooks {
  HostHooks(
    /// The Atomics blocking-wait + wake-delivery capability pair
    /// (`AtomicsCapabilities`), installed as one unit or not at all.
    /// `None` = this host cannot suspend the agent: sync Atomics.wait is
    /// treated exactly like `can_block == False` (DoWait step 10
    /// TypeError), and claimed remote wakes have nobody to deliver to (with
    /// no capability nothing remote can be blocked on this agent anyway).
    atomics: Option(AtomicsCapabilities),
    /// Monotonic clock in milliseconds. Used for Atomics waitAsync deadline
    /// bookkeeping and the event loop's timer wheel. NOT optional — every
    /// host has a clock — so it defaults to the BEAM monotonic clock
    /// (`internal/clock_ffi.monotonic_now`). An embedder overrides it to
    /// virtualise time (deterministic / mocked clocks).
    monotonic_now: fn() -> Int,
    /// Blocking sleep for the given number of milliseconds (ms <= 0 returns
    /// immediately). Used by the event loop to idle until the next timer /
    /// waitAsync deadline. Defaults to `internal/clock_ffi.sleep_ms`; an
    /// embedder overrides it alongside `monotonic_now` for a virtual clock,
    /// or to yield to its own scheduler instead of blocking the OS thread.
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
  )
}

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
    monotonic_now: clock_ffi.monotonic_now,
    sleep_ms: clock_ffi.sleep_ms,
    import_hook: option.None,
  )
}
