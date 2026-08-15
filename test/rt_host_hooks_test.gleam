//// The shared runtime reads clocks, the PRNG and the console sink from
//// `Agent.hooks`.

import arc/host_hooks.{
  DebugLevel, ErrorLevel, HostHooks, InfoLevel, LogLevel, WarnLevel,
}
import arc/rt/builtins as rt_builtins
import arc/rt/call as rt_call
import arc/rt/types.{
  JFloat, JInt, KNum, classify, mk_number, mk_object, mk_string,
}
import gleam/list
import rt_helpers

pub fn date_now_uses_wall_clock_test() {
  let st =
    rt_builtins.new_agent(
      HostHooks(
        ..rt_helpers.quiet_hooks(),
        wall_clock_ms: fn() { 1_234_567 },
        monotonic_now: fn() { 99 },
      ),
    )
  let #(date, st) = rt_helpers.global(st, "Date")
  let #(now, st) = rt_helpers.call_method(st, date, "now", [])
  assert classify(now) == KNum(JInt(1_234_567))
  let #(d, st) = rt_call.t_construct(st, date, [], date)
  let #(t, _) = rt_helpers.call_method(st, mk_object(d), "getTime", [])
  assert classify(t) == KNum(JInt(1_234_567))
}

pub fn math_random_uses_hook_test() {
  let st =
    rt_builtins.new_agent(
      HostHooks(..rt_helpers.quiet_hooks(), random: fn() { 0.25 }),
    )
  let #(math, st) = rt_helpers.global(st, "Math")
  let #(r, _) = rt_helpers.call_method(st, math, "random", [])
  assert classify(r) == KNum(JFloat(0.25))
}

pub fn console_levels_test() {
  let assert [] = rt_helpers.recorded()
  let st =
    rt_builtins.new_agent(
      HostHooks(..rt_helpers.quiet_hooks(), print: fn(level, line) {
        rt_helpers.record(#(level, line))
      }),
    )
  let #(console, st) = rt_helpers.global(st, "console")
  let st =
    list.fold(["log", "info", "debug", "warn", "error"], st, fn(st, m) {
      let #(_, st) =
        rt_helpers.call_method(st, console, m, [
          mk_string(m),
          mk_number(JInt(1)),
        ])
      st
    })
  let _ = st
  assert rt_helpers.recorded()
    == [
      #(LogLevel, "log 1"),
      #(InfoLevel, "info 1"),
      #(DebugLevel, "debug 1"),
      #(WarnLevel, "warn 1"),
      #(ErrorLevel, "error 1"),
    ]
}
