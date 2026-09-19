import arc/bytecode/error_kind.{type JsError, JsError, RangeError}
import arc/internal/digits
import arc/internal/gregorian.{
  civil_from_days, days_from_year, days_in_month,
  days_in_year as days_in_iso_year,
}
import arc/internal/int_math.{floor_div, pow10}
import arc/internal/temporal_calendar
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub const ns_per_day = 86_400_000_000_000

pub const ns_per_hour = 3_600_000_000_000

pub const ns_per_minute = 60_000_000_000

pub const ns_per_second = 1_000_000_000

pub const ns_per_ms = 1_000_000

pub const ns_per_us = 1000

pub const ns_max_instant = 8_640_000_000_000_000_000_000

// 2^53 seconds minus 1 ns
pub const max_time_duration_ns = 9_007_199_254_740_991_999_999_999

pub const min_epoch_days = -100_000_001

pub const max_epoch_days = 100_000_000

// spec checkisodaysrange
pub const iso_days_range = 100_000_000

pub type IsoDate {
  IsoDate(year: Int, month: Int, day: Int)
}

// [[isodate]] and [[calendar]] slots of plaindate, plainyearmonth, plainmonthday
// calendar part of a duration before balancing into Duration
pub type DateDuration {
  DateDuration(years: Int, months: Int, weeks: Int, days: Int)
}

pub type IsoDateSlots {
  IsoDateSlots(iso_date: IsoDate, calendar: temporal_calendar.Calendar)
}

pub type IsoTime {
  IsoTime(
    hour: Int,
    minute: Int,
    second: Int,
    millisecond: Int,
    microsecond: Int,
    nanosecond: Int,
  )
}

pub type Duration {
  Duration(
    years: Int,
    months: Int,
    weeks: Int,
    days: Int,
    hours: Int,
    minutes: Int,
    seconds: Int,
    milliseconds: Int,
    microseconds: Int,
    nanoseconds: Int,
  )
}

pub const midnight = IsoTime(0, 0, 0, 0, 0, 0)

pub const zero_duration = Duration(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

pub type Overflow {
  Constrain
  Reject
}

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
  let gregorian.CivilDate(year, month, day) = civil_from_days(days)
  IsoDate(year:, month:, day:)
}

pub fn add_days(d: IsoDate, days: Int) -> IsoDate {
  iso_date_from_epoch_days(epoch_days(d) + days)
}

pub fn day_of_week(d: IsoDate) -> Int {
  gregorian.iso_weekday_from_days(epoch_days(d))
}

pub fn day_of_year(d: IsoDate) -> Int {
  days_before_month(d.year, d.month) + d.day
}

pub fn week_of_year(d: IsoDate) -> #(Int, Int) {
  let doy = day_of_year(d)
  let dow = day_of_week(d)
  let week = { doy - dow + 10 } / 7
  let days_left = days_in_iso_year(d.year) - doy
  case week < 1, days_left < 4 - dow {
    True, _ -> {
      let prev_year = d.year - 1
      let day_of_prev_year = doy + days_in_iso_year(prev_year)
      #({ day_of_prev_year - dow + 10 } / 7, prev_year)
    }
    False, True -> #(1, d.year + 1)
    False, False -> #(week, d.year)
  }
}

pub fn is_valid_iso_date(y: Int, m: Int, d: Int) -> Bool {
  m >= 1 && m <= 12 && d >= 1 && d <= days_in_month(y, m)
}

pub fn iso_date_within_limits(d: IsoDate) -> Bool {
  let ed = epoch_days(d)
  ed >= min_epoch_days && ed <= max_epoch_days
}

pub fn iso_datetime_within_limits(d: IsoDate, t: IsoTime) -> Bool {
  let ns = utc_epoch_ns(d, t)
  ns > { 0 - ns_max_instant } - ns_per_day && ns < ns_max_instant + ns_per_day
}

pub fn iso_year_month_within_limits(y: Int, m: Int) -> Bool {
  case y {
    -271_821 -> m >= 4
    275_760 -> m <= 9
    _ -> y > -271_821 && y < 275_760
  }
}

pub fn time_to_ns(t: IsoTime) -> Int {
  t.hour
  * ns_per_hour
  + t.minute
  * ns_per_minute
  + t.second
  * ns_per_second
  + t.millisecond
  * ns_per_ms
  + t.microsecond
  * ns_per_us
  + t.nanosecond
}

pub fn ns_to_time(total: Int) -> IsoTime {
  let hour = total / ns_per_hour
  let rem = total - hour * ns_per_hour
  let minute = rem / ns_per_minute
  let rem = rem - minute * ns_per_minute
  let second = rem / ns_per_second
  let rem = rem - second * ns_per_second
  let millisecond = rem / ns_per_ms
  let rem = rem - millisecond * ns_per_ms
  let microsecond = rem / ns_per_us
  let nanosecond = rem - microsecond * ns_per_us
  IsoTime(hour:, minute:, second:, millisecond:, microsecond:, nanosecond:)
}

pub fn is_valid_time(t: IsoTime) -> Bool {
  t.hour >= 0
  && t.hour <= 23
  && t.minute >= 0
  && t.minute <= 59
  && t.second >= 0
  && t.second <= 59
  && t.millisecond >= 0
  && t.millisecond <= 999
  && t.microsecond >= 0
  && t.microsecond <= 999
  && t.nanosecond >= 0
  && t.nanosecond <= 999
}

pub fn regulate_iso_date(
  y: Int,
  m: Int,
  d: Int,
  overflow: Overflow,
) -> Result(IsoDate, JsError) {
  case overflow {
    Reject ->
      case is_valid_iso_date(y, m, d) {
        True -> Ok(IsoDate(y, m, d))
        False -> Error(JsError(RangeError, "invalid ISO date"))
      }
    Constrain -> {
      let m = int.clamp(m, 1, 12)
      let d = int.clamp(d, 1, days_in_month(y, m))
      Ok(IsoDate(y, m, d))
    }
  }
}

pub fn check_date_limits(d: IsoDate) -> Result(IsoDate, JsError) {
  case iso_date_within_limits(d) {
    True -> Ok(d)
    False -> Error(JsError(RangeError, "date outside of supported range"))
  }
}

pub fn check_date_time_limits(
  d: IsoDate,
  t: IsoTime,
) -> Result(#(IsoDate, IsoTime), JsError) {
  case iso_datetime_within_limits(d, t) {
    True -> Ok(#(d, t))
    False -> Error(JsError(RangeError, "date-time outside supported range"))
  }
}

pub fn utc_epoch_ns(d: IsoDate, t: IsoTime) -> Int {
  epoch_days(d) * ns_per_day + time_to_ns(t)
}

pub fn epoch_ns_to_iso(epoch_ns: Int, offset_ns: Int) -> #(IsoDate, IsoTime) {
  let local = epoch_ns + offset_ns
  let days = floor_div(local, ns_per_day)
  let rem = local - days * ns_per_day
  #(iso_date_from_epoch_days(days), ns_to_time(rem))
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
  format_iso_year(d.year)
  <> "-"
  <> digits.pad2(d.month)
  <> "-"
  <> digits.pad2(d.day)
}

pub type SecondsPrecision {
  AutoPrecision
  SubsecondDigits(Int)
  MinutePrecision
}

pub fn format_fraction(sub_ns: Int, precision: SecondsPrecision) -> String {
  let digits9 = int.to_string(sub_ns) |> string.pad_start(9, "0")
  case precision {
    AutoPrecision ->
      case sub_ns == 0 {
        True -> ""
        False -> "." <> trim_trailing_zeros(digits9)
      }
    SubsecondDigits(0) -> ""
    SubsecondDigits(n) -> "." <> string.slice(digits9, 0, n)
    MinutePrecision -> ""
  }
}

pub fn trim_trailing_zeros(s: String) -> String {
  case string.ends_with(s, "0") {
    True -> trim_trailing_zeros(string.drop_end(s, 1))
    False -> s
  }
}

pub fn format_iso_time(t: IsoTime, precision: SecondsPrecision) -> String {
  let sub = t.millisecond * ns_per_ms + t.microsecond * ns_per_us + t.nanosecond
  let base = digits.pad2(t.hour) <> ":" <> digits.pad2(t.minute)
  case precision {
    MinutePrecision -> base
    _ -> base <> ":" <> digits.pad2(t.second) <> format_fraction(sub, precision)
  }
}

pub fn format_offset_minutes(offset_ns: Int) -> String {
  let sign = case offset_ns < 0 {
    True -> "-"
    False -> "+"
  }
  let a = int.absolute_value(offset_ns)
  let total_minutes = a / ns_per_minute
  sign
  <> digits.pad2(total_minutes / 60)
  <> ":"
  <> digits.pad2(total_minutes % 60)
}

pub type ParsedOffset {
  NoOffset
  Zulu
  NumericOffset(ns: Int, sub_minute: Bool)
}

pub type ParsedIso {
  ParsedIso(
    date: IsoDate,
    time: Option(IsoTime),
    offset: ParsedOffset,
    tz: Option(String),
    calendar: Option(String),
  )
}

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

pub fn parse_date_part(s: String) -> Option(#(IsoDate, String)) {
  use #(year, rest) <- option.then(parse_year_part(s))
  case rest {
    "-" <> r1 -> {
      use #(m, r2) <- option.then(digits.take(r1, 2))
      case r2 {
        "-" <> r3 -> {
          use #(d, r4) <- option.then(digits.take(r3, 2))
          Some(#(IsoDate(year, m, d), r4))
        }
        _ -> None
      }
    }
    _ -> {
      use #(m, r2) <- option.then(digits.take(rest, 2))
      use #(d, r3) <- option.then(digits.take(r2, 2))
      Some(#(IsoDate(year, m, d), r3))
    }
  }
}

pub fn parse_year_part(s: String) -> Option(#(Int, String)) {
  case s {
    "+" <> rest -> digits.take(rest, 6) |> option.map(fn(p) { #(p.0, p.1) })
    "-" <> rest ->
      case digits.take(rest, 6) {
        Some(#(0, _)) -> None
        Some(#(y, r)) -> Some(#(0 - y, r))
        None -> None
      }
    _ -> digits.take(s, 4)
  }
}

pub fn parse_time_part(s: String) -> Option(#(IsoTime, String)) {
  use #(h, rest) <- option.then(digits.take(s, 2))
  let #(MinutesSeconds(mi, sec, frac_ns, ..), rest) =
    parse_minutes_seconds(rest)
  let t =
    IsoTime(
      hour: h,
      minute: mi,
      // clamp leap second only after the range check
      second: int.min(sec, 59),
      millisecond: frac_ns / ns_per_ms,
      microsecond: { frac_ns % ns_per_ms } / ns_per_us,
      nanosecond: frac_ns % ns_per_us,
    )
  case h <= 23 && mi <= 59 && sec <= 60 {
    True -> Some(#(t, rest))
    False -> None
  }
}

type MinutesSeconds {
  MinutesSeconds(minute: Int, second: Int, subsecond_ns: Int, has_seconds: Bool)
}

// minutes then seconds after an hour, extended or basic format
fn parse_minutes_seconds(s: String) -> #(MinutesSeconds, String) {
  let #(extended, after_sep) = case s {
    ":" <> r -> #(True, r)
    _ -> #(False, s)
  }
  case digits.take(after_sep, 2) {
    None -> #(MinutesSeconds(0, 0, 0, has_seconds: False), s)
    Some(#(mi, rest)) -> {
      let seconds_start = case extended, rest {
        True, ":" <> r -> Some(r)
        True, _ -> None
        False, _ -> Some(rest)
      }
      case option.then(seconds_start, digits.take(_, 2)) {
        Some(#(sec, rest)) -> {
          let #(frac, rest) = parse_fraction(rest)
          #(MinutesSeconds(mi, sec, frac, has_seconds: True), rest)
        }
        None -> #(MinutesSeconds(mi, 0, 0, has_seconds: False), rest)
      }
    }
  }
}

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

pub fn parse_offset_part(s: String) -> Option(#(ParsedOffset, String)) {
  case s {
    "Z" <> rest | "z" <> rest -> Some(#(Zulu, rest))
    "+" <> rest -> parse_offset_value(rest, 1)
    "-" <> rest -> parse_offset_value(rest, -1)
    _ -> None
  }
}

fn parse_offset_value(s: String, sign: Int) -> Option(#(ParsedOffset, String)) {
  use #(h, rest) <- option.then(digits.take(s, 2))
  let #(MinutesSeconds(mi, sec, frac, sub_minute), rest) =
    parse_minutes_seconds(rest)
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

pub fn parse_annotations(
  s: String,
  tz: Option(String),
  cal: Option(String),
  cal_critical cal_critical: Bool,
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

pub fn parse_iso_datetime_string(s: String) -> Option(ParsedIso) {
  use #(date, rest) <- option.then(parse_date_part(s))
  case is_valid_iso_date(date.year, date.month, date.day) {
    False -> None
    True -> {
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
      case time == None && is_time_prefix(rest) {
        True -> None
        False -> {
          use #(tz, cal, rest2) <- option.then(parse_annotations(
            rest,
            None,
            None,
            cal_critical: False,
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

pub const pow2_32 = 4_294_967_296

pub const pow2_52 = 4_503_599_627_370_496

pub const pow2_53 = 9_007_199_254_740_992

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

// integer-space rounding, erlang float/1 misrounds past 53 bits
pub fn round_to_float_precision(n: Int) -> Int {
  case int.absolute_value(n) < pow2_53 {
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

// scale into [2^52, 2^53) and round once, no double rounding
pub fn divide_as_float(a: Int, b: Int) -> Float {
  case b < 0 {
    True -> divide_as_float(0 - a, 0 - b)
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

fn scale_ratio(a: Int, b: Int, s: Int) -> #(Int, Int) {
  case a >= b * pow2_53 {
    True -> scale_ratio(a, b * 2, s + 1)
    False ->
      case a < b * pow2_52 {
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
          case q == pow2_53 {
            True -> #(pow2_52, s + 1)
            False -> #(q, s)
          }
        }
      }
  }
}
