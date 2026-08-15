//// The host clock and the host time zone — the runtime's only two windows
//// onto "what time is it here, right now".
////
//// This is the ONLY place these Erlang contracts are declared. Date, Intl and
//// Temporal all read the wall clock and the local zone offset; each of them
//// importing this leaf keeps `builtins/date` from being a dependency of
//// `builtins/intl` and `builtins/temporal` just to reach an FFI. A private
//// redeclaration of an @external can silently lie about the return type
//// (Gleam trusts the annotation without checking) and the BEAM term would then
//// be misused — one declaration, one truth.
////
//// Both offset lookups read the same IANA/TZif data Temporal reads and report
//// LOCAL MINUS UTC minutes. They differ in what their argument *is*: an
//// instant (`_utc_ms`) or a wall clock (`_local_ms`). Passing one where the
//// other belongs is what a single `tz_offset_minutes(epoch_ms)` would allow.
////
//// The one place in the runtime that flips this sign convention is
//// `Date.prototype.getTimezoneOffset`, which reports UTC-minus-local; it does
//// the negation itself, at that boundary.

/// Milliseconds since the Unix epoch (`erlang:system_time(millisecond)`).
/// Returns an Erlang integer — convert with `int.to_float` where a Float is
/// needed.
@external(erlang, "arc_host_time_ffi", "now_ms")
pub fn now_ms() -> Int

/// A resolved time zone: a loaded IANA zone (its parsed TZif transitions), a
/// bare POSIX TZ rule, or UTC. A plain value: whoever resolves it keeps it
/// (the shared runtime on `HostHooks.time_zone`) and every offset query is a
/// pure function of it.
pub type TimeZone

/// The host's zone: `TZ` if set, else the /etc/localtime chain, else UTC.
/// Reads the environment and the zoneinfo tree on every call.
@external(erlang, "arc_tz_ffi", "host_zone")
pub fn host_time_zone() -> TimeZone

/// A named IANA zone ("America/New_York", case-insensitive), loaded from the
/// host's zoneinfo database. `Error(Nil)` for an unknown name or a host with
/// no database.
@external(erlang, "arc_tz_ffi", "zone_named")
pub fn time_zone_named(name: String) -> Result(TimeZone, Nil)

/// The zone whose local time is UTC.
@external(erlang, "arc_tz_ffi", "utc_zone")
pub fn utc_time_zone() -> TimeZone

/// The IANA identifier of a zone, when it has one (a POSIX rule and UTC do
/// not).
@external(erlang, "arc_tz_ffi", "zone_id")
pub fn time_zone_id(zone: TimeZone) -> Result(String, Nil)

/// Offset in minutes (local − UTC) of `zone` at the UTC instant `epoch_ms`.
@external(erlang, "arc_tz_ffi", "zone_offset_at_utc_ms")
pub fn zone_offset_at_utc_ms(zone: TimeZone, epoch_ms: Int) -> Int

/// Offset in minutes (local − UTC) of `zone` for the *local wall clock*
/// `local_ms` — ES2024 §21.4.1.25 LocalTZA with isUTC = false, so a wall
/// clock a transition skips or repeats is read with the offset in effect
/// before that transition. Not interchangeable with `zone_offset_at_utc_ms`.
@external(erlang, "arc_tz_ffi", "zone_offset_at_local_ms")
pub fn zone_offset_at_local_ms(zone: TimeZone, local_ms: Int) -> Int
