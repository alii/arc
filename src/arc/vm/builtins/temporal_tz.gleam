//// IANA time zone support for Temporal, backed by the system tzdata
//// (TZif files, RFC 8536) via `arc_tz_ffi`.
////
//// Identifiers are matched case-insensitively and reported in their
//// properly-cased spelling without canonicalizing links (per ECMA-402:
//// "Asia/Calcutta" stays "Asia/Calcutta"). `canonical` resolves links for
//// TimeZoneEquals.

import gleam/result

/// Case-insensitive zone id lookup; returns the properly-cased identifier.
@external(erlang, "arc_tz_ffi", "lookup")
pub fn lookup(id: String) -> Result(String, Nil)

@external(erlang, "arc_tz_ffi", "canonical_id")
fn ffi_canonical(id: String) -> String

/// Resolve tzdata links ("Asia/Calcutta" -> "Asia/Kolkata"). Identity for
/// already-canonical or unknown ids. Per ECMA-402, the primary identifier
/// of Etc/UTC, Etc/GMT, and GMT (and zones linking to them) is "UTC".
pub fn canonical(id: String) -> String {
  case ffi_canonical(id) {
    "Etc/UTC" | "Etc/GMT" | "GMT" -> "UTC"
    c -> c
  }
}

@external(erlang, "arc_tz_ffi", "offset_at")
fn ffi_offset_at(id: String, epoch_seconds: Int) -> Result(Int, Nil)

@external(erlang, "arc_tz_ffi", "next_transition")
fn ffi_next_transition(id: String, epoch_seconds: Int) -> Result(Int, Nil)

@external(erlang, "arc_tz_ffi", "previous_transition")
fn ffi_previous_transition(id: String, epoch_seconds: Int) -> Result(Int, Nil)

const ns_per_second = 1_000_000_000

fn floor_div(a: Int, b: Int) -> Int {
  let q = a / b
  case a % b != 0 && { a < 0 } != { b < 0 } {
    True -> q - 1
    False -> q
  }
}

/// UTC offset of a named zone, in nanoseconds, at the given epoch instant.
/// `Error(Nil)` when the zone id is unknown or its TZif data cannot be
/// read/parsed.
pub fn offset_ns_at(id: String, epoch_ns: Int) -> Result(Int, Nil) {
  use offset_s <- result.map(ffi_offset_at(
    id,
    floor_div(epoch_ns, ns_per_second),
  ))
  offset_s * ns_per_second
}

/// Earliest offset transition strictly after `epoch_ns`, in epoch ns.
pub fn next_transition_ns(id: String, epoch_ns: Int) -> Result(Int, Nil) {
  // Transitions are integral seconds; T*1e9 > epoch_ns iff T > floor(ns/1e9).
  case ffi_next_transition(id, floor_div(epoch_ns, ns_per_second)) {
    Ok(sec) -> Ok(sec * ns_per_second)
    Error(Nil) -> Error(Nil)
  }
}

/// Latest offset transition strictly before `epoch_ns`, in epoch ns.
pub fn prev_transition_ns(id: String, epoch_ns: Int) -> Result(Int, Nil) {
  let sec = floor_div(epoch_ns, ns_per_second)
  // T*1e9 < epoch_ns iff T < sec (exact second) or T <= sec (mid-second).
  let arg = case epoch_ns % ns_per_second == 0 {
    True -> sec
    False -> sec + 1
  }
  case ffi_previous_transition(id, arg) {
    Ok(s) -> Ok(s * ns_per_second)
    Error(Nil) -> Error(Nil)
  }
}
