//// the operating system's tz database as host hooks; nothing in the
//// runtime reads the disk unless a host installs these

import arc/host_hooks.{type HostHooks, HostHooks}
import arc/internal/host_time.{type TimeZone}
import arc/rt/builtins/temporal_tz.{type Rules, type TzError}

// hooks with local time, named zones and the zone list backed by zoneinfo
pub fn hooks(base: HostHooks) -> HostHooks {
  HostHooks(
    ..base,
    time_zone: system_time_zone(),
    load_time_zone: load,
    time_zone_ids: available_ids,
  )
}

// tz env var, else /etc/localtime, else /etc/timezone, else utc
@external(erlang, "arc_zoneinfo_ffi", "system_zone")
pub fn system_time_zone() -> TimeZone

@external(erlang, "arc_zoneinfo_ffi", "zone_named")
pub fn time_zone_named(name: String) -> Result(TimeZone, Nil)

@external(erlang, "arc_zoneinfo_ffi", "load")
pub fn load(canonical_id: String) -> Result(Rules, TzError)

@external(erlang, "arc_zoneinfo_ffi", "available_zones")
pub fn available_ids() -> List(String)
