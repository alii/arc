pub type TimeZone

@external(erlang, "arc_tz_ffi", "utc_time_zone")
pub fn utc_time_zone() -> TimeZone

@external(erlang, "arc_tz_ffi", "time_zone_id")
pub fn time_zone_id(zone: TimeZone) -> Result(String, Nil)

// minutes, local minus utc, at a utc instant
@external(erlang, "arc_tz_ffi", "zone_offset_at_utc_ms")
pub fn zone_offset_at_utc_ms(zone: TimeZone, epoch_ms: Int) -> Int

// §21.4.1.25 localtza isutc=false, takes a wall clock not an instant
@external(erlang, "arc_tz_ffi", "zone_offset_at_local_ms")
pub fn zone_offset_at_local_ms(zone: TimeZone, local_ms: Int) -> Int
