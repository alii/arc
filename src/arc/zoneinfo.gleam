// the os tz database, which arc only reaches through HostHooks

import arc/time_zone.{type Rules, type TimeZone, type TzError}
import gleam/option.{type Option}

// tries the TZ env var, then /etc/localtime, then /etc/timezone, then utc
@external(erlang, "arc_zoneinfo_ffi", "system_time_zone")
pub fn system_time_zone() -> TimeZone

@external(erlang, "arc_zoneinfo_ffi", "time_zone_named")
pub fn time_zone_named(name: String) -> Option(TimeZone)

@external(erlang, "arc_zoneinfo_ffi", "load")
pub fn load(canonical_id: String) -> Result(Rules, TzError)

@external(erlang, "arc_zoneinfo_ffi", "available_ids")
pub fn available_ids() -> List(String)
