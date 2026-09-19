import arc/internal/clock
import arc/time_zone.{type Rules, type TimeZone, type TzError}
import arc/zoneinfo
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
    // local time for Date
    time_zone: TimeZone,
    // tz database rules for a canonical iana id, asked once per zone
    load_time_zone: fn(String) -> Result(Rules, TzError),
    // canonical ids the host has rules for
    time_zone_ids: fn() -> List(String),
    // [0, 1) behind Math.random
    random: fn() -> Float,
    print: fn(ConsoleLevel, String) -> Nil,
  )
}

fn default_print(level: ConsoleLevel, line: String) -> Nil {
  case level {
    LogLevel | InfoLevel | DebugLevel -> io.println(line)
    WarnLevel | ErrorLevel -> io.println_error(line)
  }
}

pub fn default() -> HostHooks {
  HostHooks(
    can_block: False,
    monotonic_now: clock.monotonic_now,
    sleep_ms: clock.sleep_ms,
    report_uncaught: io.println_error,
    wall_clock_ms: clock.now_ms,
    time_zone: zoneinfo.system_time_zone(),
    load_time_zone: zoneinfo.load,
    time_zone_ids: zoneinfo.available_ids,
    random: float.random,
    print: default_print,
  )
}
