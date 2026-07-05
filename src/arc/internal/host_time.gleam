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

/// Local time zone offset in minutes (local − UTC) at the UTC instant
/// `epoch_ms`.
@external(erlang, "arc_tz_ffi", "offset_at_utc_ms")
pub fn offset_at_utc_ms(epoch_ms: Int) -> Int

/// Local time zone offset in minutes (local − UTC) for the *local wall clock*
/// `local_ms` — ES2024 §21.4.1.25 LocalTZA with isUTC = false, so a wall clock
/// a transition skips or repeats is read with the offset in effect before that
/// transition. Not interchangeable with `offset_at_utc_ms`.
@external(erlang, "arc_tz_ffi", "offset_at_local_ms")
pub fn offset_at_local_ms(local_ms: Int) -> Int
