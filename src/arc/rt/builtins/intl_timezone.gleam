import arc/internal/host_time
import arc/rt/builtins/intl_format as fmt
import arc/rt/builtins/temporal_tz
import arc/rt/intl_data.{
  type DtfTimeZone, type TimeZoneNameWidth, FixedZone, HostZone, NamedZone,
  TzLong, TzLongGeneric, TzLongOffset, TzShort, TzShortGeneric, TzShortOffset,
}
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/string

pub fn canonical(s: String) -> Option(DtfTimeZone) {
  case parse_offset_zone(s) {
    Some(minutes) -> Some(FixedZone(format_offset_zone(minutes), minutes))
    None ->
      option.lazy_or(etc_gmt_zone(string.lowercase(s)), fn() { named_zone(s) })
  }
}

fn is_utc(name: String) -> Bool {
  case temporal_tz.lookup(name) {
    Ok(zone) -> temporal_tz.canonical(zone) == "UTC"
    Error(Nil) -> False
  }
}

fn named_zone(s: String) -> Option(DtfTimeZone) {
  use zone <- option.then(option.from_result(temporal_tz.lookup(s)))
  case temporal_tz.canonical(zone) {
    "UTC" -> Some(FixedZone(temporal_tz.zone_id(zone), 0))
    _ ->
      case temporal_tz.offset_ns_at(zone, 0) {
        Ok(_) -> Some(NamedZone(zone:))
        Error(_host_lacks_data) -> None
      }
  }
}

pub fn offset_at(tz: DtfTimeZone, instant_ms: Int) -> Int {
  case tz {
    HostZone(zone:) -> host_time.zone_offset_at_utc_ms(zone, instant_ms)
    FixedZone(offset_minutes:, ..) -> offset_minutes
    NamedZone(zone:) -> {
      let assert Ok(offset_ns) =
        temporal_tz.offset_ns_at(zone, instant_ms * 1_000_000)
        as "intl: tzdata offset lookup failed for a zone lookup accepted"
      offset_ns / 60_000_000_000
    }
  }
}

// etc/gmt+n is utc-n and etc/gmt-n is utc+n
fn etc_gmt_zone(lower: String) -> Option(DtfTimeZone) {
  case string.split_once(lower, "etc/gmt") {
    Ok(#("", rest)) -> {
      let #(sign, num) = case string.pop_grapheme(rest) {
        Ok(#("+", n)) -> #(-1, n)
        Ok(#("-", n)) -> #(1, n)
        _ -> #(0, "")
      }
      case sign != 0, int.parse(num) {
        True, Ok(n) if n >= 1 && n <= 14 -> {
          let name = case sign < 0 {
            True -> "Etc/GMT+" <> int.to_string(n)
            False -> "Etc/GMT-" <> int.to_string(n)
          }
          case sign < 0 && n > 12 {
            True -> None
            False -> Some(FixedZone(name, sign * n * 60))
          }
        }
        _, _ -> None
      }
    }
    _ -> None
  }
}

fn parse_offset_zone(s: String) -> Option(Int) {
  let #(sign, rest) = case string.pop_grapheme(s) {
    Ok(#("+", rest)) -> #(1, rest)
    Ok(#("-", rest)) -> #(-1, rest)
    _ -> #(0, s)
  }
  case sign {
    0 -> None
    _ ->
      case string.split(rest, ":") {
        [hh, mm] -> {
          let hh_len = string.length(hh)
          let mm_len = string.length(mm)
          case int.parse(hh), int.parse(mm) {
            Ok(h), Ok(m)
              if h >= 0
              && h <= 23
              && m >= 0
              && m <= 59
              && hh_len == 2
              && mm_len == 2
            -> Some(sign * { h * 60 + m })
            _, _ -> None
          }
        }
        [hhmm] ->
          case string.length(hhmm), int.parse(hhmm) {
            2, Ok(h) if h >= 0 && h <= 23 -> Some(sign * h * 60)
            4, Ok(v) -> {
              let h = v / 100
              let m = v % 100
              case h <= 23 && m <= 59 {
                True -> Some(sign * { h * 60 + m })
                False -> None
              }
            }
            _, _ -> None
          }
        _ -> None
      }
  }
}

fn format_offset_zone(minutes: Int) -> String {
  let sign = case minutes < 0 {
    True -> "-"
    False -> "+"
  }
  let m = int.absolute_value(minutes)
  sign <> fmt.pad2(m / 60) <> ":" <> fmt.pad2(m % 60)
}

pub fn display(name: String, width: TimeZoneNameWidth, offset: Int) -> String {
  case is_utc(name), width {
    True, TzShort | True, TzShortGeneric -> "UTC"
    True, TzLong | True, TzLongGeneric -> "Coordinated Universal Time"
    _, TzLong | _, TzLongOffset | _, TzLongGeneric -> gmt_offset(offset, True)
    _, TzShort | _, TzShortOffset | _, TzShortGeneric ->
      gmt_offset(offset, False)
  }
}

fn gmt_offset(offset: Int, long: Bool) -> String {
  case offset {
    0 -> "GMT"
    _ -> {
      let sign = case offset < 0 {
        True -> "-"
        False -> "+"
      }
      let m = int.absolute_value(offset)
      let h = m / 60
      let mm = m % 60
      case long {
        True -> "GMT" <> sign <> fmt.pad2(h) <> ":" <> fmt.pad2(mm)
        False ->
          case mm {
            0 -> "GMT" <> sign <> int.to_string(h)
            _ -> "GMT" <> sign <> int.to_string(h) <> ":" <> fmt.pad2(mm)
          }
      }
    }
  }
}
