//// Intl.DateTimeFormat time-zone resolution and rendering.
////
//// Pure `String`/`Int`/`DtfTimeZone` — no `State`. `canonical` is
//// IsValidTimeZoneName + case normalization; `offset_at` is the one place a
//// DateTimeFormat's UTC offset ever comes from; `display` renders that offset
//// under a `timeZoneName` width.

import arc/internal/digits
import arc/internal/host_time
import arc/vm/builtins/intl_format as fmt
import arc/vm/builtins/intl_locale as tags
import arc/vm/builtins/temporal_tz
import arc/vm/value.{
  type DtfTimeZone, type TimeZoneNameWidth, FixedZone, HostZone, NamedZone,
  TzLong, TzLongGeneric, TzLongOffset, TzShort, TzShortGeneric, TzShortOffset,
}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// IsValidTimeZoneName + identifier case normalization. Named zones are
/// resolved against the system tzdata (Temporal's TZif backend), which is
/// also what supplies their per-instant offset later; the remaining forms
/// (UTC/GMT aliases, ±HH:MM, Etc/GMT±N) have an offset that never varies.
pub fn canonical(s: String) -> Option(DtfTimeZone) {
  let lower = string.lowercase(s)
  case lower {
    "utc"
    | "etc/universal"
    | "etc/zulu"
    | "universal"
    | "zulu"
    | "etc/utc"
    | "etc/uct"
    | "uct" ->
      Some(FixedZone(
        case lower {
          "etc/utc" -> "Etc/UTC"
          "etc/uct" -> "Etc/UCT"
          "uct" -> "UCT"
          _ -> "UTC"
        },
        0,
      ))
    "gmt" | "etc/greenwich" | "greenwich" -> Some(FixedZone("GMT", 0))
    "etc/gmt" | "etc/gmt0" | "etc/gmt+0" | "etc/gmt-0" ->
      Some(FixedZone("Etc/GMT", 0))
    _ ->
      case parse_offset_zone(s) {
        Some(minutes) -> Some(FixedZone(format_offset_zone(minutes), minutes))
        None ->
          case etc_gmt_zone(lower) {
            Some(res) -> Some(res)
            None -> iana_zone(lower)
          }
      }
  }
}

/// The UTC offset (minutes) a formatter's zone has at a given instant. The
/// only place a DateTimeFormat offset ever comes from.
pub fn offset_at(tz: DtfTimeZone, instant_ms: Int) -> Int {
  case tz {
    HostZone -> host_time.offset_at_utc_ms(instant_ms)
    FixedZone(offset_minutes:, ..) -> offset_minutes
    NamedZone(zone:) -> {
      // `temporal_tz.lookup` already loaded this zone's TZif to mint the
      // handle, so a failure here means the zoneinfo install vanished
      // underneath us. Falling back to UTC would silently render every
      // timestamp in the wrong zone — fail loudly instead.
      let assert Ok(offset_ns) =
        temporal_tz.offset_ns_at(zone, instant_ms * 1_000_000)
        as "intl: tzdata offset lookup failed for a zone lookup accepted"
      offset_ns / 60_000_000_000
    }
  }
}

/// Etc/GMT+N (UTC-N) and Etc/GMT-N (UTC+N), N in 1..14.
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

/// Named IANA zones: the abbreviation zones plus structurally-valid
/// Area/Location identifiers. ECMA-402 §6.5: an implementation supporting
/// named zones must accept every IANA Zone/Link name, so resolution goes
/// through the system tzdata (Temporal's TZif backend) rather than a
/// hardcoded table; names absent from tzdata get the caller's
/// "Invalid time zone specified" RangeError instead of silently rendering as
/// UTC (offset 0).
fn iana_zone(lower: String) -> Option(DtfTimeZone) {
  case lower {
    "est"
    | "cst6cdt"
    | "est5edt"
    | "mst7mdt"
    | "pst8pdt"
    | "mst"
    | "hst"
    | "cet"
    | "eet"
    | "met"
    | "wet" -> tzdata_zone(lower)
    _ ->
      case string.split(lower, "/") {
        [area, ..rest] if rest != [] -> {
          let known_area =
            list.contains(
              [
                "africa", "america", "antarctica", "arctic", "asia", "atlantic",
                "australia", "europe", "indian", "pacific",
              ],
              area,
            )
          let parts_ok = list.all(rest, fn(p) { p != "" && is_zone_word(p) })
          case known_area && parts_ok {
            True -> tzdata_zone(lower)
            False -> None
          }
        }
        _ -> None
      }
  }
}

/// Resolve a zone id against the system tzdata. The `Zone` handle it returns
/// is what supplies the offset at each formatted instant — no offset is
/// snapshotted here. `None` when the name is not in tzdata.
fn tzdata_zone(lower: String) -> Option(DtfTimeZone) {
  temporal_tz.lookup(lower)
  |> result.map(NamedZone)
  |> option.from_result
}

fn is_zone_word(p: String) -> Bool {
  tags.all_codepoints(p, fn(c) {
    digits.is_ascii_alnum_code(c) || c == 0x5f || c == 0x2b || c == 0x2d
  })
}

/// ±HH:MM offset time zones.
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
          // Guards can't contain function calls — bind the lengths first.
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

/// Render a formatter's [[TimeZone]] under a timeZoneName width.
pub fn display(name: String, width: TimeZoneNameWidth, offset: Int) -> String {
  case name, width {
    "UTC", TzShort | "UTC", TzShortGeneric -> "UTC"
    "UTC", TzLong | "UTC", TzLongGeneric -> "Coordinated Universal Time"
    "UTC", TzShortOffset -> "GMT"
    "UTC", TzLongOffset -> "GMT"
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
