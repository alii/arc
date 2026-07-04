//// The BEAM clock, as raw FFI (`arc_clock_ffi.erl`).
////
//// Nothing in the VM should call these directly on a hot path: the engine
//// reads the clock through `host_hooks.HostHooks.monotonic_now` /
//// `.sleep_ms` so an embedder can virtualise time. These externals are the
//// REAL clock behind the DEFAULT hooks (see `host_hooks.default_host_hooks`),
//// and are public for embedder natives (e.g. the $262 agent helpers) that
//// want the same wall clock outside any State.

/// Monotonic BEAM clock in milliseconds (`erlang:monotonic_time/1`).
@external(erlang, "arc_clock_ffi", "monotonic_now")
pub fn monotonic_now() -> Int

/// Blocking sleep (`timer:sleep/1`, a no-op for `ms <= 0`). Always a BOUNDED
/// idle: an untimed sync `Atomics.wait` blocks in the embedder's
/// `AtomicsCapabilities.sync_wait`, never here.
@external(erlang, "arc_clock_ffi", "sleep")
pub fn sleep_ms(ms: Int) -> Nil
