// the engine should read time via host_hooks, not these

@external(erlang, "arc_clock_ffi", "monotonic_now")
pub fn monotonic_now() -> Int

@external(erlang, "arc_clock_ffi", "sleep")
pub fn sleep_ms(ms: Int) -> Nil
