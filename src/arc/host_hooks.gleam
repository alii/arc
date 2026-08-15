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
//// Atomics needs nothing from the embedder but that policy: a
//// SharedArrayBuffer another agent can see lives in an owner process
//// (`arc/rt/sab`) with its WaiterList, a sync `Atomics.wait` blocks the
//// calling BEAM process in a receive, and `Atomics.notify` has the owner
//// message the waiters it wakes.

import arc/internal/clock
import arc/internal/host_time
import gleam/float
import gleam/io

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

/// The embedder's host capabilities, bundled into one record carried on
/// `Agent.hooks`. Supplied exactly once at engine construction
/// (`rt/builtins.new_agent(hooks)`) and shared by everything that runs on that
/// agent. NOT generic over `host`: no field mentions a JS value type.
pub type HostHooks {
  HostHooks(
    /// Agent Record [[CanBlock]] (§9.7): whether this agent may be suspended
    /// by a sync `Atomics.wait` (§25.4.3.14 DoWait step 10, AgentCanSuspend).
    /// When `False` a sync wait throws a TypeError after its argument
    /// coercions; `Atomics.waitAsync` and `Atomics.notify` never consult it.
    /// A blocking agent parks its BEAM process inside `Atomics.wait` until
    /// another agent sharing the buffer notifies it or the timeout passes,
    /// so grant it only to agents running on a process the embedder can
    /// afford to have blocked (test262's main and `$262.agent` workers; not
    /// a server's request scheduler).
    can_block: Bool,
    /// Monotonic clock in milliseconds: `Atomics.waitAsync` deadlines and the
    /// embedder's timers. NOT optional — every host has a clock — so it
    /// defaults to the BEAM monotonic clock
    /// (`internal/clock.monotonic_now`). An embedder overrides it to
    /// virtualise time (deterministic / mocked clocks).
    monotonic_now: fn() -> Int,
    /// Blocking sleep for the given number of milliseconds (ms <= 0 returns
    /// immediately), for an embedder loop idling until its next timer.
    /// Defaults to `internal/clock.sleep_ms`; an embedder overrides it
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
    can_block: False,
    monotonic_now: clock.monotonic_now,
    sleep_ms: clock.sleep_ms,
    report_uncaught: io.println_error,
    wall_clock_ms: host_time.now_ms,
    time_zone: host_time.host_time_zone(),
    random: float.random,
    print: default_print,
  )
}
