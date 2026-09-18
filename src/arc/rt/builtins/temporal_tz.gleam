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
pub fn lookup_name(id: String) -> Result(String, Nil)

@external(erlang, "arc_tz_ffi", "canonical_id")
fn ffi_canonical(id: String) -> String

pub fn canonical_id(proper: String) -> String {
  case ffi_canonical(proper) {
    "Etc/UTC" | "Etc/GMT" | "GMT" -> "UTC"
    c -> c
  }
}

pub fn canonical(zone: Zone) -> String {
  canonical_id(zone.id)
}

@external(erlang, "arc_tz_ffi", "available_zones")
fn ffi_available_zones() -> List(String)

// primary ids the host has data for, sorted
pub fn available_ids() -> List(String) {
  let zones =
    list.filter(ffi_available_zones(), fn(z) {
      z != "Etc/UTC" && z != "Etc/GMT"
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

// zoneinfo on disk, keyed by canonical id
@external(erlang, "arc_tz_ffi", "load")
pub fn host_loader(canonical: String) -> Result(Rules, TzError)

// a known name becomes a zone with its rules, loading each proper id once
pub fn resolve(
  name: String,
  zones: Dict(String, Zone),
  load: fn(String) -> Result(Rules, TzError),
) -> Result(#(Zone, Dict(String, Zone)), ResolveError) {
  use proper <- result.try(
    lookup_name(name) |> result.replace_error(UnknownZone),
  )
  case dict.get(zones, proper) {
    Ok(zone) -> Ok(#(zone, zones))
    Error(Nil) -> {
      use rules <- result.map(
        load(ffi_canonical(proper)) |> result.map_error(LoadFailed(proper, _)),
      )
      let zone = Zone(id: proper, rules:)
      #(zone, dict.insert(zones, proper, zone))
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
