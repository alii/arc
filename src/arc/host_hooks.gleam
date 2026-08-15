//// The embedder capability contract: everything a host supplies ONCE at
//// engine construction, carried on `Agent.hooks`: the clocks, the PRNG, the
//// console and uncaught-error sinks, the local time zone, and the agent's
//// [[CanBlock]] policy (§9.7) for `Atomics.wait`.
////
//// This module is a dependency-light leaf below `arc/rt/types`, re-exported
//// by `arc/host` for embedders. No field mentions a JS value type; the
//// dynamic-import host function is engine state on `Agent.import_hook` (see
//// `arc/module_host.install_import_hook`), not a hook here.
////
//// Atomics on the shared runtime (`arc/rt`) needs nothing from the embedder
//// but that policy: a SharedArrayBuffer another agent can see lives in an
//// owner process (`arc/rt/sab`) with its WaiterList, a sync `Atomics.wait`
//// blocks the calling BEAM process in a receive, and `Atomics.notify` has the
//// owner message the waiters it wakes. The `AtomicsCapabilities` closures
//// (`sync_wait` / `deliver_wake` and the `WaitRequest` family) are the
//// retired bytecode VM's protocol (`arc/vm/builtins/atomics`), where the
//// EMBEDDER did the blocking and the wake delivery; they stay only while
//// that tree still compiles, and the shared runtime never calls them. What
//// it reads is whether the record is installed at all — `can_block`.

import arc/internal/host_time
import arc/vm/internal/clock_ffi
import gleam/float
import gleam/io
import gleam/option.{type Option, None, Some}

/// Which `console` method produced a line. The default `print` hook sends
/// `LogLevel`/`InfoLevel`/`DebugLevel` to stdout and `WarnLevel`/`ErrorLevel`
/// to stderr.
pub type ConsoleLevel {
  LogLevel
  InfoLevel
  WarnLevel
  ErrorLevel
  DebugLevel
}

/// Retired-VM protocol: opaque WaiterList identity of a buffer, as the old
/// bytecode VM keyed it. Never produced on the shared runtime.
pub type WaiterKey

/// Retired-VM protocol: opaque handle to one WaiterList entry the old VM
/// registered before handing the block to the embedder.
pub type WaiterHandle

/// Retired-VM protocol: a remote waiter the old VM's `Atomics.notify`
/// claimed and handed to the embedder for wake delivery.
pub type ClaimedWaiter

/// Retired-VM protocol: one blocking sync `Atomics.wait` the old VM handed
/// to the embedder's `SyncWaitFn` after registering `handle`.
pub type WaitRequest {
  WaitRequest(
    handle: WaiterHandle,
    key: WaiterKey,
    /// Byte offset within the buffer (matches the notify side).
    byte_index: Int,
    /// Milliseconds to block; `None` = infinite.
    timeout_ms: Option(Int),
  )
}

/// Retired-VM protocol: how an embedder-side blocking wait ended ("ok" /
/// "timed-out"; "not-equal" was decided before the embedder was called).
pub type WaitOutcome {
  WaitOk
  WaitTimedOut
}

/// Retired-VM protocol: block the calling agent on `WaitRequest.handle`.
pub type SyncWaitFn =
  fn(WaitRequest) -> WaitOutcome

/// Retired-VM protocol: deliver wake messages to claimed remote waiters.
pub type DeliverWakeFn =
  fn(List(ClaimedWaiter)) -> Nil

/// The Atomics record on `HostHooks.atomics`. Its PRESENCE is the agent's
/// [[CanBlock]] (§9.7): read it with `can_block`, set it with
/// `with_can_block`. The two closures are the retired bytecode VM's
/// embedder-driven wait/wake protocol and are never called by the shared
/// runtime, whose blocking and waking happen in the SharedArrayBuffer's
/// owner process (`arc/rt/sab`); they remain fields only while `arc/vm`
/// still compiles against this record.
pub type AtomicsCapabilities {
  AtomicsCapabilities(sync_wait: SyncWaitFn, deliver_wake: DeliverWakeFn)
}

/// The embedder's host capabilities, bundled into one record carried on
/// `Agent.hooks`. Supplied exactly once at engine construction
/// (`rt/builtins.new_agent(hooks)`) and shared by everything that runs on that
/// agent. NOT generic over `host`: no field mentions a JS value type.
pub type HostHooks {
  HostHooks(
    /// Agent Record [[CanBlock]] (§9.7): `Some(_)` = this agent may be
    /// suspended by a sync `Atomics.wait` (§25.4.3.14 DoWait step 10,
    /// AgentCanSuspend); `None` = it may not, and a sync wait throws a
    /// TypeError after its argument coercions. `Atomics.waitAsync` and
    /// `Atomics.notify` never consult it. Use `can_block` / `with_can_block`
    /// rather than the record itself (see `AtomicsCapabilities`).
    atomics: Option(AtomicsCapabilities),
    /// Monotonic clock in milliseconds: `Atomics.waitAsync` deadlines and the
    /// embedder's timers. NOT optional — every host has a clock — so it
    /// defaults to the BEAM monotonic clock
    /// (`internal/clock_ffi.monotonic_now`). An embedder overrides it to
    /// virtualise time (deterministic / mocked clocks).
    monotonic_now: fn() -> Int,
    /// Blocking sleep for the given number of milliseconds (ms <= 0 returns
    /// immediately), for an embedder loop idling until its next timer.
    /// Defaults to `internal/clock_ffi.sleep_ms`; an embedder overrides it
    /// alongside `monotonic_now` for a virtual clock, or to yield to its own
    /// scheduler instead of blocking the OS thread.
    sleep_ms: fn(Int) -> Nil,
    /// Sink for uncaught job-level errors — an unhandled promise rejection
    /// after the microtask drain, or a throw from a user-supplied
    /// resolve/reject during a reaction job. Core has no caller to propagate
    /// these to, so it hands the formatted message here. Defaults to
    /// `io.println_error`; an embedder overrides it to capture reports (test
    /// harness assertions, structured logging) instead of writing to stderr.
    report_uncaught: fn(String) -> Nil,
    /// Wall clock: milliseconds since the Unix epoch. Backs `Date.now` and
    /// `new Date()`. Defaults to `erlang:system_time(millisecond)`.
    wall_clock_ms: fn() -> Int,
    /// The local time zone behind Date's LocalTZA (local getters/setters,
    /// `getTimezoneOffset`, string rendering). Resolved once by the host and
    /// carried as a value; defaults to the host's own zone
    /// (`host_time.host_time_zone`: `TZ`, else /etc/localtime, else UTC).
    time_zone: host_time.TimeZone,
    /// Uniform Float in [0, 1) behind `Math.random`. Defaults to
    /// `float.random`; a harness seeds a deterministic PRNG here.
    random: fn() -> Float,
    /// The `console.*` sink: one formatted line (no trailing newline) and
    /// the level of the method that produced it.
    print: fn(ConsoleLevel, String) -> Nil,
  )
}

/// The default `print` hook: log/info/debug lines to stdout, warn/error
/// lines to stderr.
pub fn default_print(level: ConsoleLevel, line: String) -> Nil {
  case level {
    LogLevel | InfoLevel | DebugLevel -> io.println(line)
    WarnLevel | ErrorLevel -> io.println_error(line)
  }
}

/// The default hooks: [[CanBlock]] false — a sync `Atomics.wait` throws
/// rather than parking an embedder thread that never asked for it — and the
/// real BEAM clocks, `timer:sleep`, the host's time zone, `float.random`,
/// stdout/stderr sinks.
pub fn default_host_hooks() -> HostHooks {
  HostHooks(
    atomics: None,
    monotonic_now: clock_ffi.monotonic_now,
    sleep_ms: clock_ffi.sleep_ms,
    report_uncaught: io.println_error,
    wall_clock_ms: host_time.now_ms,
    time_zone: host_time.host_time_zone(),
    random: float.random,
    print: default_print,
  )
}

/// Agent Record [[CanBlock]] (§9.7): whether a sync `Atomics.wait` may
/// suspend this agent (AgentCanSuspend, §25.4.3.14 DoWait step 10).
pub fn can_block(hooks: HostHooks) -> Bool {
  option.is_some(hooks.atomics)
}

/// `hooks` with [[CanBlock]] set to `can_block`, every other hook unchanged.
/// A blocking agent parks its BEAM process inside `Atomics.wait` until
/// another agent sharing the buffer notifies it or the timeout passes, so
/// grant it only to agents running on a process the embedder can afford to
/// have blocked (test262's main and `$262.agent` workers; not a server's
/// request scheduler).
pub fn with_can_block(hooks: HostHooks, can_block: Bool) -> HostHooks {
  let atomics = case can_block {
    True -> Some(blocking_agent())
    False -> None
  }
  HostHooks(..hooks, atomics:)
}

/// The [[CanBlock]] = true marker. Its closures belong to the retired VM's
/// protocol (see `AtomicsCapabilities`) and are unreachable from the shared
/// runtime; should that VM ever run on these hooks they answer "nobody woke
/// you" and deliver nothing, which is safe.
fn blocking_agent() -> AtomicsCapabilities {
  AtomicsCapabilities(
    sync_wait: fn(_request) { WaitTimedOut },
    deliver_wake: fn(_claimed) { Nil },
  )
}
