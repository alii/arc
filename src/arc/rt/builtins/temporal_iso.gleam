//// Pure ISO-8601 core for Temporal — no `State`, no `Heap`, no `JsValue`.
////
//// Everything the Temporal builtins need to describe, parse, format, and do
//// arithmetic on ISO calendar values lives here so it can be unit-tested
//// without spinning up a VM heap. `arc/vm/builtins/temporal` imports this
//// unqualified (matching how it already pulls in `gregorian` and `int_math`)
//// and layers the property-bag / options-object / brand-check machinery on
//// top.

import arc/internal/digits.{take_digits}
import arc/internal/gregorian.{
  civil_from_days, days_from_year, days_in_month,
  days_in_year as days_in_iso_year,
}
import arc/internal/int_math.{floor_div}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// ============================================================================
// Constants
// ============================================================================

pub const ns_per_day = 86_400_000_000_000

pub const ns_per_hour = 3_600_000_000_000

pub const ns_per_minute = 60_000_000_000

pub const ns_per_second = 1_000_000_000

pub const ns_per_ms = 1_000_000

pub const ns_per_us = 1000

/// nsMaxInstant = 8.64e21 (±100,000,000 days from epoch).
pub const ns_max_instant = 8_640_000_000_000_000_000_000

/// Maximum time duration in ns: 2^53 seconds − 1 ns.
pub const max_time_duration_ns = 9_007_199_254_740_991_999_999_999

/// ISODateWithinLimits bounds expressed in epoch days.
pub const min_epoch_days = -100_000_001

pub const max_epoch_days = 100_000_000

// ============================================================================
// Internal records
// ============================================================================

pub type IsoDate {
  IsoDate(year: Int, month: Int, day: Int)
}

pub type TimeRec {
  TimeRec(hour: Int, minute: Int, second: Int, ms: Int, us: Int, ns: Int)
}

pub type DurRec {
  DurRec(
    years: Int,
    months: Int,
    weeks: Int,
    days: Int,
    hours: Int,
    minutes: Int,
    seconds: Int,
    ms: Int,
    us: Int,
    ns: Int,
  )
}

pub const midnight = TimeRec(0, 0, 0, 0, 0, 0)

pub const zero_dur = DurRec(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

/// Internal error kind for pure helpers — converted to JS errors at the
/// dispatch boundary.
pub type TErr {
  RangeE(String)
  TypeE(String)
}

/// overflow option: how out-of-range calendar/time fields are handled.
pub type Overflow {
  Constrain
  Reject
}

// ============================================================================
// Pure calendar math (ISO 8601 proleptic Gregorian)
// ============================================================================

fn days_before_month(y: Int, m: Int) -> Int {
  sum_months(y, 1, m, 0)
}

fn sum_months(y: Int, i: Int, until: Int, acc: Int) -> Int {
  case i >= until {
    True -> acc
    False -> sum_months(y, i + 1, until, acc + days_in_month(y, i))
  }
}

pub fn epoch_days(d: IsoDate) -> Int {
  days_from_year(d.year) + days_before_month(d.year, d.month) + d.day - 1
}

pub fn iso_date_from_epoch_days(days: Int) -> IsoDate {
  let #(year, month, day) = civil_from_days(days)
  IsoDate(year:, month:, day:)
}

/// ISO day of week: Monday = 1 .. Sunday = 7. Epoch day 0 = Thursday.
pub fn day_of_week(d: IsoDate) -> Int {
  gregorian.iso_weekday_from_days(epoch_days(d))
}

pub fn day_of_year(d: IsoDate) -> Int {
  days_before_month(d.year, d.month) + d.day
}

/// ISO 8601 week number + week-calendar year.
pub fn week_of_year(d: IsoDate) -> #(Int, Int) {
  let doy = day_of_year(d)
  let dow = day_of_week(d)
  let week = { doy - dow + 10 } / 7
  case week < 1 {
    True -> {
      // Belongs to the last week of the previous year.
      let py = d.year - 1
      let pdoy = doy + days_in_iso_year(py)
      #({ pdoy - dow + 10 } / 7, py)
    }
    False -> {
      let weeks_in_year = case { days_in_iso_year(d.year) - doy < 4 - dow } {
        True -> {
          // Might belong to week 1 of next year.
          let nyd = doy - days_in_iso_year(d.year)
          let nweek = { nyd - dow + 10 } / 7
          case nweek >= 1 {
            True -> -1
            False -> week
          }
        }
        False -> week
      }
      case weeks_in_year == -1 {
        True -> #(1, d.year + 1)
        False -> #(week, d.year)
      }
    }
  }
}

pub fn is_valid_iso_date(y: Int, m: Int, d: Int) -> Bool {
  m >= 1 && m <= 12 && d >= 1 && d <= days_in_month(y, m)
}

pub fn iso_date_within_limits(d: IsoDate) -> Bool {
  let ed = epoch_days(d)
  ed >= min_epoch_days && ed <= max_epoch_days
}

pub fn iso_datetime_within_limits(d: IsoDate, t: TimeRec) -> Bool {
  let ns = epoch_days(d) * ns_per_day + time_to_ns(t)
  ns > { 0 - ns_max_instant } - ns_per_day && ns < ns_max_instant + ns_per_day
}

pub fn iso_year_month_within_limits(y: Int, m: Int) -> Bool {
  case y {
    -271_821 -> m >= 4
    275_760 -> m <= 9
    _ -> y > -271_821 && y < 275_760
  }
}

pub fn time_to_ns(t: TimeRec) -> Int {
  t.hour
  * ns_per_hour
  + t.minute
  * ns_per_minute
  + t.second
  * ns_per_second
  + t.ms
  * ns_per_ms
  + t.us
  * ns_per_us
  + t.ns
}

/// Decompose non-negative day-local nanoseconds into a TimeRec.
pub fn ns_to_time(total: Int) -> TimeRec {
  let hour = total / ns_per_hour
  let rem = total - hour * ns_per_hour
  let minute = rem / ns_per_minute
  let rem = rem - minute * ns_per_minute
  let second = rem / ns_per_second
  let rem = rem - second * ns_per_second
  let ms = rem / ns_per_ms
  let rem = rem - ms * ns_per_ms
  let us = rem / ns_per_us
  let ns = rem - us * ns_per_us
  TimeRec(hour:, minute:, second:, ms:, us:, ns:)
}

pub fn is_valid_time(t: TimeRec) -> Bool {
  t.hour >= 0
  && t.hour <= 23
  && t.minute >= 0
  && t.minute <= 59
  && t.second >= 0
  && t.second <= 59
  && t.ms >= 0
  && t.ms <= 999
  && t.us >= 0
  && t.us <= 999
  && t.ns >= 0
  && t.ns <= 999
}

/// RegulateISODate. Month must already be ≥ 1 when called from property
/// bags (ToPositiveIntegerWithTruncation).
pub fn regulate_iso_date(
  y: Int,
  m: Int,
  d: Int,
  overflow: Overflow,
) -> Result(IsoDate, TErr) {
  case overflow {
    Reject ->
      case is_valid_iso_date(y, m, d) {
        True -> Ok(IsoDate(y, m, d))
        False -> Error(RangeE("invalid ISO date"))
      }
    Constrain -> {
      let m = int.clamp(m, 1, 12)
      let d = int.clamp(d, 1, days_in_month(y, m))
      Ok(IsoDate(y, m, d))
    }
  }
}

pub fn check_date_limits(d: IsoDate) -> Result(IsoDate, TErr) {
  case iso_date_within_limits(d) {
    True -> Ok(d)
    False -> Error(RangeE("date outside of supported range"))
  }
}

/// Epoch nanoseconds for an ISO date+time interpreted as UTC.
pub fn utc_epoch_ns(d: IsoDate, t: TimeRec) -> Int {
  epoch_days(d) * ns_per_day + time_to_ns(t)
}

/// Split epoch nanoseconds (+ offset) into date and time.
pub fn epoch_ns_to_iso(epoch_ns: Int, offset_ns: Int) -> #(IsoDate, TimeRec) {
  let local = epoch_ns + offset_ns
  let days = floor_div(local, ns_per_day)
  let rem = local - days * ns_per_day
  #(iso_date_from_epoch_days(days), ns_to_time(rem))
}

// ============================================================================
// Formatting
// ============================================================================

pub fn pad2(n: Int) -> String {
  int.to_string(int.absolute_value(n)) |> string.pad_start(2, "0")
}

pub fn format_iso_year(y: Int) -> String {
  case y >= 0 && y <= 9999 {
    True -> int.to_string(y) |> string.pad_start(4, "0")
    False -> {
      let sign = case y < 0 {
        True -> "-"
        False -> "+"
      }
      sign
      <> { int.to_string(int.absolute_value(y)) |> string.pad_start(6, "0") }
    }
  }
}

pub fn format_iso_date(d: IsoDate) -> String {
  format_iso_year(d.year) <> "-" <> pad2(d.month) <> "-" <> pad2(d.day)
}

pub type Precision {
  AutoPrec
  FixedPrec(Int)
  MinutePrec
}

/// Format sub-second part per `precision`: "auto" trims trailing zeros,
/// an Int 0..9 forces that many digits. Returns "" or ".ddd...".
pub fn format_fraction(sub_ns: Int, precision: Precision) -> String {
  let digits9 = int.to_string(sub_ns) |> string.pad_start(9, "0")
  case precision {
    AutoPrec ->
      case sub_ns == 0 {
        True -> ""
        False -> "." <> trim_trailing_zeros(digits9)
      }
    FixedPrec(0) -> ""
    FixedPrec(n) -> "." <> string.slice(digits9, 0, n)
    MinutePrec -> ""
  }
}

pub fn trim_trailing_zeros(s: String) -> String {
  case string.ends_with(s, "0") {
    True -> trim_trailing_zeros(string.drop_end(s, 1))
    False -> s
  }
}

pub fn format_iso_time(t: TimeRec, precision: Precision) -> String {
  let sub = t.ms * ns_per_ms + t.us * ns_per_us + t.ns
  let base = pad2(t.hour) <> ":" <> pad2(t.minute)
  case precision {
    MinutePrec -> base
    _ -> base <> ":" <> pad2(t.second) <> format_fraction(sub, precision)
  }
}

/// Format a UTC offset from nanoseconds, minute precision ("+05:30").
pub fn format_offset_minutes(offset_ns: Int) -> String {
  let sign = case offset_ns < 0 {
    True -> "-"
    False -> "+"
  }
  let a = int.absolute_value(offset_ns)
  let total_minutes = a / ns_per_minute
  sign <> pad2(total_minutes / 60) <> ":" <> pad2(total_minutes % 60)
}

// ============================================================================
// ISO 8601 string parsing
// ============================================================================

/// The UTC-offset portion of a parsed ISO string. Makes the impossible
/// combinations (Z with a numeric value, sub-minute without a value)
/// unconstructable.
pub type ParsedOffset {
  /// no offset syntax was present
  NoOffset
  /// the Z designator (offset 0)
  Zulu
  /// an explicit ±HH:MM[:SS[.fff]] value; `sub_minute` is True when a seconds
  /// component was spelled out (even ":00"), which disqualifies it from use as
  /// a time zone identifier
  NumericOffset(ns: Int, sub_minute: Bool)
}

pub type ParsedIso {
  ParsedIso(
    date: IsoDate,
    time: Option(TimeRec),
    /// parsed UTC offset (Z / numeric / none)
    offset: ParsedOffset,
    /// time zone annotation [Etc/UTC] / [+01:00]
    tz: Option(String),
    /// calendar annotation value
    calendar: Option(String),
  )
}

/// Take up to `max` digits (at least 1), returning value, count, rest.
pub fn take_some_digits(s: String, max: Int) -> Option(#(Int, Int, String)) {
  take_some_digits_loop(s, max, 0, 0)
}

fn take_some_digits_loop(
  s: String,
  max: Int,
  acc: Int,
  count: Int,
) -> Option(#(Int, Int, String)) {
  case max == 0 {
    True -> Some(#(acc, count, s))
    False ->
      case string.pop_grapheme(s) {
        Ok(#(c, rest)) ->
          case digits.digit_value(c) {
            Some(d) ->
              take_some_digits_loop(rest, max - 1, acc * 10 + d, count + 1)
            None ->
              case count > 0 {
                True -> Some(#(acc, count, s))
                False -> None
              }
          }
        Error(Nil) ->
          case count > 0 {
            True -> Some(#(acc, count, s))
            False -> None
          }
      }
  }
}

/// Parse the date part: ±YYYYYY-MM-DD / ±YYYYYYMMDD / YYYY-MM-DD / YYYYMMDD.
/// Returns y/m/d unvalidated (range-checked by caller) + rest.
pub fn parse_date_part(s: String) -> Option(#(Int, Int, Int, String)) {
  use #(year, rest) <- option.then(parse_year_part(s))
  case rest {
    "-" <> r1 -> {
      use #(m, r2) <- option.then(take_digits(r1, 2))
      case r2 {
        "-" <> r3 -> {
          use #(d, r4) <- option.then(take_digits(r3, 2))
          Some(#(year, m, d, r4))
        }
        _ -> None
      }
    }
    _ -> {
      use #(m, r2) <- option.then(take_digits(rest, 2))
      use #(d, r3) <- option.then(take_digits(r2, 2))
      Some(#(year, m, d, r3))
    }
  }
}

/// Year: 4 digits, or sign + 6 digits. "-000000" is rejected.
pub fn parse_year_part(s: String) -> Option(#(Int, String)) {
  case s {
    "+" <> rest -> take_digits(rest, 6) |> option.map(fn(p) { #(p.0, p.1) })
    "-" <> rest ->
      case take_digits(rest, 6) {
        Some(#(0, _)) -> None
        Some(#(y, r)) -> Some(#(0 - y, r))
        None -> None
      }
    // U+2212 MINUS SIGN is NOT valid in ISO 8601 strings (only ASCII hyphen).
    _ -> take_digits(s, 4)
  }
}

/// Time: HH[:MM[:SS[.fffffffff]]] or HH[MM[SS[.fffffffff]]].
pub fn parse_time_part(s: String) -> Option(#(TimeRec, String)) {
  use #(h, rest) <- option.then(take_digits(s, 2))
  let #(mi, sec, frac_ns, rest) = case rest {
    ":" <> r1 ->
      case take_digits(r1, 2) {
        Some(#(mi, r2)) ->
          case r2 {
            ":" <> r3 ->
              case take_digits(r3, 2) {
                Some(#(sec, r4)) -> {
                  let #(f, r5) = parse_fraction(r4)
                  #(mi, sec, f, r5)
                }
                None -> #(mi, 0, 0, r2)
              }
            _ -> #(mi, 0, 0, r2)
          }
        None -> #(0, 0, 0, rest)
      }
    _ ->
      case take_digits(rest, 2) {
        Some(#(mi, r2)) ->
          case take_digits(r2, 2) {
            Some(#(sec, r3)) -> {
              let #(f, r4) = parse_fraction(r3)
              #(mi, sec, f, r4)
            }
            None -> #(mi, 0, 0, r2)
          }
        None -> #(0, 0, 0, rest)
      }
  }
  // Leap second: 60 → 59 per spec. Anything above 60 is a syntax error, so
  // the range check below MUST see the raw parsed value — clamping first
  // would silently accept "T00:00:61".
  let t =
    TimeRec(
      hour: h,
      minute: mi,
      second: int.min(sec, 59),
      ms: frac_ns / ns_per_ms,
      us: { frac_ns % ns_per_ms } / ns_per_us,
      ns: frac_ns % ns_per_us,
    )
  case h <= 23 && mi <= 59 && sec <= 60 {
    True -> Some(#(t, rest))
    False -> None
  }
}

/// Fraction after '.' or ',' — 1..9 digits → nanoseconds.
pub fn parse_fraction(s: String) -> #(Int, String) {
  case s {
    "." <> r | "," <> r ->
      case take_some_digits(r, 9) {
        Some(#(v, count, rest)) -> #(v * pow10(9 - count), rest)
        None -> #(0, s)
      }
    _ -> #(0, s)
  }
}

pub fn pow10(n: Int) -> Int {
  case n {
    0 -> 1
    _ -> 10 * pow10(n - 1)
  }
}

/// UTC offset: Z / z / ±HH[:MM[:SS[.fff]]] / ±HH[MM[SS]].
/// Returns None when no offset syntax is present at all.
pub fn parse_offset_part(s: String) -> Option(#(ParsedOffset, String)) {
  case s {
    "Z" <> rest | "z" <> rest -> Some(#(Zulu, rest))
    "+" <> rest -> parse_offset_value(rest, 1)
    "-" <> rest -> parse_offset_value(rest, -1)
    _ -> None
  }
}

fn parse_offset_value(s: String, sign: Int) -> Option(#(ParsedOffset, String)) {
  use #(h, rest) <- option.then(take_digits(s, 2))
  let #(mi, sec, frac, sub_minute, rest) = case rest {
    ":" <> r1 ->
      case take_digits(r1, 2) {
        Some(#(mi, r2)) ->
          case r2 {
            ":" <> r3 ->
              case take_digits(r3, 2) {
                Some(#(sec, r4)) -> {
                  let #(f, r5) = parse_fraction(r4)
                  #(mi, sec, f, True, r5)
                }
                None -> #(mi, 0, 0, False, r2)
              }
            _ -> #(mi, 0, 0, False, r2)
          }
        None -> #(0, 0, 0, False, rest)
      }
    _ ->
      case take_digits(rest, 2) {
        Some(#(mi, r2)) ->
          case take_digits(r2, 2) {
            Some(#(sec, r3)) -> {
              let #(f, r4) = parse_fraction(r3)
              #(mi, sec, f, True, r4)
            }
            None -> #(mi, 0, 0, False, r2)
          }
        None -> #(0, 0, 0, False, rest)
      }
  }
  case h <= 23 && mi <= 59 && sec <= 59 {
    True -> {
      let ns =
        { h * ns_per_hour + mi * ns_per_minute + sec * ns_per_second + frac }
        * sign
      Some(#(NumericOffset(ns, sub_minute), rest))
    }
    False -> None
  }
}

/// Annotations: time zone bracket first (no '='), then key=value pairs.
/// Returns #(tz, calendar, rest) or None on syntax/critical errors.
pub fn parse_annotations(
  s: String,
  tz: Option(String),
  cal: Option(String),
  cal_critical: Bool,
) -> Option(#(Option(String), Option(String), String)) {
  case s {
    "[" <> r -> {
      let #(critical, r) = case r {
        "!" <> rr -> #(True, rr)
        _ -> #(False, r)
      }
      use #(body, rest) <- option.then(split_bracket(r, ""))
      case string.split_once(body, "=") {
        Ok(#(key, val)) ->
          case is_annotation_key(key) && val != "" {
            False -> None
            True ->
              case key {
                "u-ca" ->
                  case cal {
                    // Duplicate calendar annotations are a RangeError when
                    // ANY of them is critical; otherwise first one wins.
                    Some(_) ->
                      case critical || cal_critical {
                        True -> None
                        False -> parse_annotations(rest, tz, cal, cal_critical)
                      }
                    None -> parse_annotations(rest, tz, Some(val), critical)
                  }
                _ ->
                  case critical {
                    True -> None
                    False -> parse_annotations(rest, tz, cal, cal_critical)
                  }
              }
          }
        Error(Nil) ->
          // Time-zone annotation — only valid as the first bracket.
          case tz, cal, is_tz_annotation(body) {
            None, None, True ->
              parse_annotations(rest, Some(body), cal, cal_critical)
            _, _, _ -> None
          }
      }
    }
    _ -> Some(#(tz, cal, s))
  }
}

fn split_bracket(s: String, acc: String) -> Option(#(String, String)) {
  case string.pop_grapheme(s) {
    Ok(#("]", rest)) ->
      case acc {
        "" -> None
        _ -> Some(#(acc, rest))
      }
    Ok(#(c, rest)) -> split_bracket(rest, acc <> c)
    Error(Nil) -> None
  }
}

fn is_annotation_key(s: String) -> Bool {
  s != ""
  && list.all(string.to_graphemes(s), fn(c) {
    is_lower_alpha(c) || c == "-" || c == "_" || digits.digit_value(c) != None
  })
}

fn is_lower_alpha(c: String) -> Bool {
  string.contains("abcdefghijklmnopqrstuvwxyz", c) && c != ""
}

pub fn is_tz_annotation(s: String) -> Bool {
  case s {
    "+" <> _ | "-" <> _ -> True
    _ ->
      s != ""
      && list.all(string.split(s, "/"), fn(part) {
        // TimeZoneIANANameComponent: TZLeadingChar TZChar* — a leading
        // digit is not a name (so date-time strings fall through to the
        // ISO-string time zone extraction).
        case string.pop_grapheme(part) {
          Error(Nil) -> False
          Ok(#(first, rest)) ->
            is_tz_leading_char(first)
            && list.all(string.to_graphemes(rest), is_tz_char)
        }
      })
  }
}

fn is_tz_leading_char(c: String) -> Bool {
  is_lower_alpha(c)
  || string.contains("ABCDEFGHIJKLMNOPQRSTUVWXYZ", c)
  || c == "."
  || c == "_"
}

fn is_tz_char(c: String) -> Bool {
  is_tz_leading_char(c) || digits.digit_value(c) != None || c == "-" || c == "+"
}

/// Parse a full Temporal date-time string:
///   date [T time [offset]] [annotations]
pub fn parse_iso_datetime_string(s: String) -> Option(ParsedIso) {
  use #(y, m, d, rest) <- option.then(parse_date_part(s))
  case is_valid_iso_date(y, m, d) {
    False -> None
    True -> {
      let date = IsoDate(y, m, d)
      let #(time, offset, rest) = case rest {
        "T" <> tr | "t" <> tr | " " <> tr ->
          case parse_time_part(tr) {
            Some(#(t, r2)) ->
              case parse_offset_part(r2) {
                Some(#(off, r3)) -> #(Some(t), off, r3)
                None -> #(Some(t), NoOffset, r2)
              }
            None -> #(None, NoOffset, rest)
          }
        _ -> #(None, NoOffset, rest)
      }
      // If a "T" was present but the time failed to parse, reject.
      case time == None && is_time_prefix(rest) {
        True -> None
        False -> {
          use #(tz, cal, rest2) <- option.then(parse_annotations(
            rest,
            None,
            None,
            False,
          ))
          case rest2 {
            "" -> Some(ParsedIso(date:, time:, offset:, tz:, calendar: cal))
            _ -> None
          }
        }
      }
    }
  }
}

fn is_time_prefix(s: String) -> Bool {
  case s {
    "T" <> _ | "t" <> _ | " " <> _ -> True
    _ -> False
  }
}

// ============================================================================
// Float64 rounding of arbitrary-precision integers/rationals
// ============================================================================

pub const two52 = 4_503_599_627_370_496

pub const two53 = 9_007_199_254_740_992

pub fn int_sign(n: Int) -> Int {
  case n > 0 {
    True -> 1
    False ->
      case n < 0 {
        True -> -1
        False -> 0
      }
  }
}

/// Each Duration field is a float64 Number in the spec (CreateTemporalDuration
/// stores ℝ(𝔽(v))), so huge components lose precision on construction.
/// Rounded in integer space via scale_ratio (ties-to-even) because erlang's
/// float/1 mis-rounds integers wider than 53 bits.
pub fn f64_int(n: Int) -> Int {
  case int.absolute_value(n) < two53 {
    True -> n
    False -> {
      let #(m, s) = scale_ratio(int.absolute_value(n), 1, 0)
      let v = int.bitwise_shift_left(m, s)
      case n < 0 {
        True -> 0 - v
        False -> v
      }
    }
  }
}

/// Correctly-rounded integer ratio → double (round-to-nearest, ties-to-even).
/// Computing `q + r/b` in floats double-rounds near representability
/// boundaries; this scales the exact rational into [2^52, 2^53) and rounds
/// once (DivideTimeDuration operates on exact mathematical values).
pub fn ns_div_float(a: Int, b: Int) -> Float {
  case b < 0 {
    True -> ns_div_float(0 - a, 0 - b)
    False ->
      case a == 0 {
        True -> 0.0
        False -> {
          let neg = a < 0
          let #(q, s) = scale_ratio(int.absolute_value(a), b, 0)
          let f = case s >= 0 {
            True -> int.to_float(int.bitwise_shift_left(q, s))
            False ->
              int.to_float(q) /. int.to_float(int.bitwise_shift_left(1, 0 - s))
          }
          case neg {
            True -> 0.0 -. f
            False -> f
          }
        }
      }
  }
}

/// Scale a/b (both positive) by a power of two so the rounded quotient lands
/// in [2^52, 2^53), then round to nearest (ties to even).
/// Returns #(mantissa, exponent) with a/b ≈ mantissa × 2^exponent.
fn scale_ratio(a: Int, b: Int, s: Int) -> #(Int, Int) {
  case a >= b * two53 {
    True -> scale_ratio(a, b * 2, s + 1)
    False ->
      case a < b * two52 {
        True -> scale_ratio(a * 2, b, s - 1)
        False -> {
          let q0 = a / b
          let r = a - q0 * b
          let round_up = case int_sign(2 * r - b) {
            1 -> True
            -1 -> False
            _ -> q0 % 2 == 1
          }
          let q = case round_up {
            True -> q0 + 1
            False -> q0
          }
          case q == two53 {
            True -> #(two52, s + 1)
            False -> #(q, s)
          }
        }
      }
  }
}
