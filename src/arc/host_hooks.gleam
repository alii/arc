import arc/internal/clock
import arc/internal/host_time
import arc/rt/builtins/temporal_tz
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
    // local time for Date; arc/zoneinfo has the os backed versions of
    // this and the next two, the defaults here know only utc
    time_zone: host_time.TimeZone,
    // tz database rules for a canonical iana id, asked once per zone
    load_time_zone: fn(String) -> Result(temporal_tz.Rules, temporal_tz.TzError),
    // canonical ids the host has rules for
    time_zone_ids: fn() -> List(String),
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
    time_zone: host_time.utc_time_zone(),
    load_time_zone: fn(_) { Error(temporal_tz.NoZoneinfo) },
    time_zone_ids: fn() { [] },
    random: float.random,
    print: default_print,
  )
}
