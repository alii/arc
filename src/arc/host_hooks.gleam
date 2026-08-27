import arc/internal/clock
import arc/internal/host_time
import gleam/float
import gleam/io

pub type ConsoleLevel {
  LogLevel
  InfoLevel
  WarnLevel
  ErrorLevel
  DebugLevel
}

/// host capabilities supplied once at engine construction
pub type HostHooks {
  HostHooks(
    // §9.7 [[CanBlock]]: may sync Atomics.wait park this process
    can_block: Bool,
    monotonic_now: fn() -> Int,
    sleep_ms: fn(Int) -> Nil,
    report_uncaught: fn(String) -> Nil,
    wall_clock_ms: fn() -> Int,
    time_zone: host_time.TimeZone,
    // [0, 1) behind Math.random
    random: fn() -> Float,
    print: fn(ConsoleLevel, String) -> Nil,
  )
}

pub fn default_print(level: ConsoleLevel, line: String) -> Nil {
  case level {
    LogLevel | InfoLevel | DebugLevel -> io.println(line)
    WarnLevel | ErrorLevel -> io.println_error(line)
  }
}

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
