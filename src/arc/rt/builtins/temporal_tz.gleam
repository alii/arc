import arc/internal/int_math.{floor_div}
import gleam/option.{type Option, None, Some}
import gleam/result

pub opaque type Zone {
  Zone(id: String)
}

pub fn zone_id(zone: Zone) -> String {
  zone.id
}

@external(erlang, "arc_tz_ffi", "lookup")
fn ffi_lookup(id: String) -> Result(String, Nil)

pub fn lookup(id: String) -> Result(Zone, Nil) {
  use proper <- result.map(ffi_lookup(id))
  Zone(proper)
}

@external(erlang, "arc_tz_ffi", "canonical_id")
fn ffi_canonical(id: String) -> String

pub fn canonical(zone: Zone) -> String {
  case ffi_canonical(zone.id) {
    "Etc/UTC" | "Etc/GMT" | "GMT" -> "UTC"
    c -> c
  }
}

pub type TzError {
  NoZoneinfo
  Unreadable(detail: String)
  Unparseable(detail: String)
}

pub fn describe(error: TzError) -> String {
  case error {
    NoZoneinfo -> "no time zone database on this host"
    Unreadable(detail:) -> "unreadable time zone data (" <> detail <> ")"
    Unparseable(detail:) -> "corrupt time zone data (" <> detail <> ")"
  }
}

@external(erlang, "arc_tz_ffi", "offset_at")
fn ffi_offset_at(id: String, epoch_seconds: Int) -> Result(Int, TzError)

type FfiTransition {
  Found(Int)
  NoTransition
  LoadFailed(TzError)
}

@external(erlang, "arc_tz_ffi", "next_transition")
fn ffi_next_transition(id: String, epoch_seconds: Int) -> FfiTransition

@external(erlang, "arc_tz_ffi", "previous_transition")
fn ffi_previous_transition(id: String, epoch_seconds: Int) -> FfiTransition

const ns_per_second = 1_000_000_000

pub fn offset_ns_at(zone: Zone, epoch_ns: Int) -> Result(Int, TzError) {
  use offset_s <- result.map(ffi_offset_at(
    zone.id,
    floor_div(epoch_ns, ns_per_second),
  ))
  offset_s * ns_per_second
}

fn transition_ns(t: FfiTransition) -> Result(Option(Int), TzError) {
  case t {
    Found(sec) -> Ok(Some(sec * ns_per_second))
    NoTransition -> Ok(None)
    LoadFailed(error) -> Error(error)
  }
}

pub fn next_transition_ns(
  zone: Zone,
  epoch_ns: Int,
) -> Result(Option(Int), TzError) {
  // transitions are whole seconds
  transition_ns(ffi_next_transition(zone.id, floor_div(epoch_ns, ns_per_second)))
}

pub fn prev_transition_ns(
  zone: Zone,
  epoch_ns: Int,
) -> Result(Option(Int), TzError) {
  let sec = floor_div(epoch_ns, ns_per_second)
  // mid-second: the transition at sec itself is before us
  let arg = case epoch_ns % ns_per_second == 0 {
    True -> sec
    False -> sec + 1
  }
  transition_ns(ffi_previous_transition(zone.id, arg))
}
