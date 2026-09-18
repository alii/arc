import arc/internal/int_math.{floor_div}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/string

// parsed transition data for one zone, host supplied
pub type Rules

pub opaque type Zone {
  Zone(id: String, rules: Rules)
}

pub fn zone_id(zone: Zone) -> String {
  zone.id
}

@external(erlang, "arc_tz_ffi", "lookup")
fn lookup(id: String) -> Result(String, Nil)

pub fn known_identifier(id: String) -> Option(String) {
  lookup(id) |> option.from_result
}

@external(erlang, "arc_tz_ffi", "canonical_id")
fn link_target(id: String) -> String

pub fn primary_identifier(identifier: String) -> String {
  case link_target(identifier) {
    "Etc/UTC" | "Etc/GMT" | "GMT" -> "UTC"
    c -> c
  }
}

pub fn primary_identifier_of(zone: Zone) -> String {
  primary_identifier(zone.id)
}

// primary ids out of what the host has data for, sorted, utc always there
pub fn available_ids(host_ids: List(String)) -> List(String) {
  let zones =
    list.filter(host_ids, fn(z) {
      z != "Etc/UTC" && z != "Etc/GMT" && z != "UTC"
    })
  list.sort(["UTC", ..zones], string.compare)
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

pub type ResolveError {
  UnknownZone
  LoadFailed(id: String, error: TzError)
}

// a known name becomes a zone with its rules, loading each identifier once
pub fn resolve(
  name: String,
  zones: Dict(String, Zone),
  load: fn(String) -> Result(Rules, TzError),
) -> Result(#(Zone, Dict(String, Zone)), ResolveError) {
  use identifier <- result.try(
    known_identifier(name) |> option.to_result(UnknownZone),
  )
  case dict.get(zones, identifier) {
    Ok(zone) -> Ok(#(zone, zones))
    Error(Nil) -> {
      use rules <- result.map(
        load(link_target(identifier))
        |> result.map_error(LoadFailed(identifier, _)),
      )
      let zone = Zone(id: identifier, rules:)
      #(zone, dict.insert(zones, identifier, zone))
    }
  }
}

@external(erlang, "arc_tz_ffi", "rules_offset_at")
fn ffi_offset_at(rules: Rules, epoch_seconds: Int) -> Int

@external(erlang, "arc_tz_ffi", "rules_next_transition")
fn ffi_next_transition(rules: Rules, epoch_seconds: Int) -> Option(Int)

@external(erlang, "arc_tz_ffi", "rules_previous_transition")
fn ffi_previous_transition(rules: Rules, epoch_seconds: Int) -> Option(Int)

const ns_per_second = 1_000_000_000

pub fn offset_ns_at(zone: Zone, epoch_ns: Int) -> Int {
  ffi_offset_at(zone.rules, floor_div(epoch_ns, ns_per_second)) * ns_per_second
}

// transitions are whole seconds
pub fn next_transition_ns(zone: Zone, epoch_ns: Int) -> Option(Int) {
  ffi_next_transition(zone.rules, floor_div(epoch_ns, ns_per_second))
  |> option.map(fn(sec) { sec * ns_per_second })
}

pub fn prev_transition_ns(zone: Zone, epoch_ns: Int) -> Option(Int) {
  let sec = floor_div(epoch_ns, ns_per_second)
  // mid-second: the transition at sec itself is before us
  let arg = case epoch_ns % ns_per_second == 0 {
    True -> sec
    False -> sec + 1
  }
  ffi_previous_transition(zone.rules, arg)
  |> option.map(fn(s) { s * ns_per_second })
}
