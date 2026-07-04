//// IANA time zone support for Temporal, backed by the system tzdata
//// (TZif files, RFC 8536) via `arc_tz_ffi`.
////
//// Identifiers are matched case-insensitively and reported in their
//// properly-cased spelling without canonicalizing links (per ECMA-402:
//// "Asia/Calcutta" stays "Asia/Calcutta"). `canonical` resolves links for
//// TimeZoneEquals.
////
//// A `Zone` is a *validated* zone id: it can only be built by `lookup`, so
//// no query function can be handed an id that was never checked against the
//// zone-name table.

import arc/internal/gregorian.{floor_div}
import gleam/option.{type Option, None, Some}
import gleam/result

/// A time zone identifier that has been checked against the tzdata name
/// table, in its properly-cased spelling. Only `lookup` produces one.
pub opaque type Zone {
  Zone(id: String)
}

/// The properly-cased IANA identifier of a validated zone.
pub fn zone_id(zone: Zone) -> String {
  zone.id
}

@external(erlang, "arc_tz_ffi", "lookup")
fn ffi_lookup(id: String) -> Result(String, Nil)

/// Case-insensitive zone id lookup — the only way to obtain a `Zone`.
pub fn lookup(id: String) -> Result(Zone, Nil) {
  use proper <- result.map(ffi_lookup(id))
  Zone(proper)
}

@external(erlang, "arc_tz_ffi", "canonical_id")
fn ffi_canonical(id: String) -> String

/// Resolve tzdata links ("Asia/Calcutta" -> "Asia/Kolkata"). Identity for
/// already-canonical zones. Per ECMA-402, the primary identifier of Etc/UTC,
/// Etc/GMT, and GMT (and zones linking to them) is "UTC".
pub fn canonical(zone: Zone) -> String {
  case ffi_canonical(zone.id) {
    "Etc/UTC" | "Etc/GMT" | "GMT" -> "UTC"
    c -> c
  }
}

@external(erlang, "arc_tz_ffi", "offset_at")
fn ffi_offset_at(id: String, epoch_seconds: Int) -> Result(Int, Nil)

/// The three distinct answers the transition FFI can give. On the Erlang side
/// these are `{found, Sec}`, `no_transition`, and `unloadable`.
type FfiTransition {
  Found(Int)
  NoTransition
  Unloadable
}

@external(erlang, "arc_tz_ffi", "next_transition")
fn ffi_next_transition(id: String, epoch_seconds: Int) -> FfiTransition

@external(erlang, "arc_tz_ffi", "previous_transition")
fn ffi_previous_transition(id: String, epoch_seconds: Int) -> FfiTransition

const ns_per_second = 1_000_000_000

/// UTC offset of a named zone, in nanoseconds, at the given epoch instant.
/// `Error(Nil)` when the zone's TZif data cannot be read/parsed.
pub fn offset_ns_at(zone: Zone, epoch_ns: Int) -> Result(Int, Nil) {
  use offset_s <- result.map(ffi_offset_at(
    zone.id,
    floor_div(epoch_ns, ns_per_second),
  ))
  offset_s * ns_per_second
}

/// `Ok(None)` (no further transition) and `Error(Nil)` (zone data unloadable)
/// are different answers: the first is a `null` result for JS, the second is
/// a broken zoneinfo install and must be reported as an error.
fn transition_ns(t: FfiTransition) -> Result(Option(Int), Nil) {
  case t {
    Found(sec) -> Ok(Some(sec * ns_per_second))
    NoTransition -> Ok(None)
    Unloadable -> Error(Nil)
  }
}

/// Earliest offset transition strictly after `epoch_ns`, in epoch ns.
pub fn next_transition_ns(
  zone: Zone,
  epoch_ns: Int,
) -> Result(Option(Int), Nil) {
  // Transitions are integral seconds; T*1e9 > epoch_ns iff T > floor(ns/1e9).
  transition_ns(ffi_next_transition(zone.id, floor_div(epoch_ns, ns_per_second)))
}

/// Latest offset transition strictly before `epoch_ns`, in epoch ns.
pub fn prev_transition_ns(
  zone: Zone,
  epoch_ns: Int,
) -> Result(Option(Int), Nil) {
  let sec = floor_div(epoch_ns, ns_per_second)
  // T*1e9 < epoch_ns iff T < sec (exact second) or T <= sec (mid-second).
  let arg = case epoch_ns % ns_per_second == 0 {
    True -> sec
    False -> sec + 1
  }
  transition_ns(ffi_previous_transition(zone.id, arg))
}
