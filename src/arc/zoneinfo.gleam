// os tz database; reached only through HostHooks

import arc/time_zone.{type Rules, type TimeZone, type TzError}
import gleam/option.{type Option}

// tz env var, else /etc/localtime, else /etc/timezone, else utc
@external(erlang, "arc_zoneinfo_ffi", "system_time_zone")
pub fn system_time_zone() -> TimeZone

@external(erlang, "arc_zoneinfo_ffi", "time_zone_named")
pub fn time_zone_named(name: String) -> Option(TimeZone)

@external(erlang, "arc_zoneinfo_ffi", "load")
pub fn load(canonical_id: String) -> Result(Rules, TzError)

@external(erlang, "arc_zoneinfo_ffi", "available_ids")
pub fn available_ids() -> List(String)
