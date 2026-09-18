import arc/internal/host_time
import arc/rt/builtins/intl_format
import arc/rt/builtins/temporal_common
import arc/rt/builtins/temporal_tz
import arc/rt/intl_data.{
  type DtfTimeZone, type TimeZoneNameWidth, FixedZone, HostZone, NamedZone,
  TzLong, TzLongGeneric, TzLongOffset, TzShort, TzShortGeneric, TzShortOffset,
}
import arc/rt/types.{type Agent}
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/string

pub fn resolve(st: Agent, s: String) -> #(Option(DtfTimeZone), Agent) {
  case parse_utc_offset_minutes(s) {
    Some(minutes) -> #(Some(FixedZone(offset_zone_name(minutes), minutes)), st)
    None ->
      case etc_gmt_zone(string.lowercase(s)) {
        Some(zone) -> #(Some(zone), st)
        None -> named_zone(st, s)
      }
  }
}

fn is_utc(name: String) -> Bool {
  case temporal_tz.known_identifier(name) {
    Some(identifier) -> temporal_tz.primary_identifier(identifier) == "UTC"
    None -> False
  }
}

// a zone the host has no data for is not offered
fn named_zone(st: Agent, s: String) -> #(Option(DtfTimeZone), Agent) {
  case temporal_tz.known_identifier(s) {
    None -> #(None, st)
    Some(identifier) ->
      case temporal_tz.primary_identifier(identifier) {
        "UTC" -> #(Some(FixedZone(identifier, 0)), st)
        _ ->
          case temporal_common.resolve_zone(st, identifier) {
            #(Ok(zone), st) -> #(Some(NamedZone(zone:)), st)
            #(Error(_host_lacks_data), st) -> #(None, st)
          }
      }
  }
}

pub fn offset_at(tz: DtfTimeZone, instant_ms: Int) -> Int {
  case tz {
    HostZone(zone:) -> host_time.zone_offset_at_utc_ms(zone, instant_ms)
    FixedZone(offset_minutes:, ..) -> offset_minutes
    NamedZone(zone:) ->
      temporal_tz.offset_ns_at(zone, instant_ms * 1_000_000) / 60_000_000_000
  }
}

// etc/gmt+n is utc-n and etc/gmt-n is utc+n
fn etc_gmt_zone(lower: String) -> Option(DtfTimeZone) {
  case lower {
    "etc/gmt+" <> digits ->
      etc_gmt_fixed("Etc/GMT+", digits, sign: -1, max_hours: 12)
    "etc/gmt-" <> digits ->
      etc_gmt_fixed("Etc/GMT-", digits, sign: 1, max_hours: 14)
    _ -> None
  }
}

fn etc_gmt_fixed(
  prefix: String,
  digits: String,
  sign sign: Int,
  max_hours max_hours: Int,
) -> Option(DtfTimeZone) {
  case int.parse(digits) {
    Ok(hours) if hours >= 1 && hours <= max_hours ->
      Some(FixedZone(prefix <> int.to_string(hours), sign * hours * 60))
    _ -> None
  }
}

fn parse_utc_offset_minutes(s: String) -> Option(Int) {
  use #(sign, rest) <- option.then(case string.pop_grapheme(s) {
    Ok(#("+", rest)) -> Some(#(1, rest))
    Ok(#("-", rest)) -> Some(#(-1, rest))
    _ -> None
  })
  let minutes = case string.split(rest, ":") {
    [hh, mm] ->
      case string.length(hh), int.parse(hh), string.length(mm), int.parse(mm) {
        2, Ok(h), 2, Ok(m) if h >= 0 && h <= 23 && m >= 0 && m <= 59 ->
          Some(h * 60 + m)
        _, _, _, _ -> None
      }
    [hhmm] ->
      case string.length(hhmm), int.parse(hhmm) {
        2, Ok(h) if h >= 0 && h <= 23 -> Some(h * 60)
        4, Ok(v) -> {
          let h = v / 100
          let m = v % 100
          case h <= 23 && m <= 59 {
            True -> Some(h * 60 + m)
            False -> None
          }
        }
        _, _ -> None
      }
    _ -> None
  }
  option.map(minutes, fn(m) { sign * m })
}

fn offset_sign(minutes: Int) -> String {
  case minutes < 0 {
    True -> "-"
    False -> "+"
  }
}

fn offset_zone_name(minutes: Int) -> String {
  let m = int.absolute_value(minutes)
  offset_sign(minutes)
  <> intl_format.pad2(m / 60)
  <> ":"
  <> intl_format.pad2(m % 60)
}

pub fn display_name(
  name: String,
  width: TimeZoneNameWidth,
  offset_minutes: Int,
) -> String {
  case is_utc(name), width {
    True, TzShort | True, TzShortGeneric -> "UTC"
    True, TzLong | True, TzLongGeneric -> "Coordinated Universal Time"
    _, TzLong | _, TzLongOffset | _, TzLongGeneric ->
      gmt_offset_label(offset_minutes, long: True)
    _, TzShort | _, TzShortOffset | _, TzShortGeneric ->
      gmt_offset_label(offset_minutes, long: False)
  }
}

fn gmt_offset_label(offset_minutes: Int, long long: Bool) -> String {
  let m = int.absolute_value(offset_minutes)
  let sign = offset_sign(offset_minutes)
  let hours = m / 60
  let minutes = m % 60
  case offset_minutes, long, minutes {
    0, _, _ -> "GMT"
    _, True, _ ->
      "GMT"
      <> sign
      <> intl_format.pad2(hours)
      <> ":"
      <> intl_format.pad2(minutes)
    _, False, 0 -> "GMT" <> sign <> int.to_string(hours)
    _, False, _ ->
      "GMT" <> sign <> int.to_string(hours) <> ":" <> intl_format.pad2(minutes)
  }
}
