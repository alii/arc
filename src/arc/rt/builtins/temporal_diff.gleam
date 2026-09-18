import arc/bytecode/error_kind.{type JsError}
import arc/internal/gregorian.{days_in_month}
import arc/internal/int_math.{trunc_div, trunc_mod}
import arc/internal/temporal_calendar as tcal
import arc/rt/builtins/temporal_common.{
  type RoundingMode, type Unit, Compatible, Day, Hour, Microsecond, Millisecond,
  Minute, Month, Nanosecond, Second, Week, Year, balance_time_ns,
  epoch_ns_to_iso_in, max_unit, require_time_unit, round_to_increment,
  time_unit_ns, unit_rank,
}
import arc/rt/builtins/temporal_fields.{
  balance_year_month, calendar_date_add, calendar_years_months_until,
  compare_iso_date, compare_triple, iso_date_add, round_between,
}
import arc/rt/builtins/temporal_iso.{
  type Duration, type IsoDate, type IsoTime, Constrain, Duration, IsoDate,
  add_days, epoch_days, int_sign, midnight, ns_per_day, time_to_ns, utc_epoch_ns,
  zero_duration,
}
import arc/rt/builtins/temporal_zoned_ops.{
  check_iso_days_range, get_epoch_ns_for,
}
import arc/rt/types.{type TimeZone}
import gleam/int
import gleam/list
import gleam/result

pub fn compare_iso_date_time(
  a: #(IsoDate, IsoTime),
  b: #(IsoDate, IsoTime),
) -> Int {
  int_sign(utc_epoch_ns(a.0, a.1) - utc_epoch_ns(b.0, b.1))
}

pub fn calendar_date_until(
  cal: tcal.Calendar,
  d1: IsoDate,
  d2: IsoDate,
  largest: Unit,
) -> #(Int, Int, Int, Int) {
  case cal {
    tcal.Iso8601 -> iso_date_until(d1, d2, largest)
    _ ->
      case largest {
        Year | Month -> {
          let #(y, m, rem_days) =
            calendar_years_months_until(
              cal,
              d1,
              d2,
              whole_years: largest == Year,
            )
          #(y, m, 0, rem_days)
        }
        _ -> iso_date_until(d1, d2, largest)
      }
  }
}

pub fn difference_calendar_date(
  cal: tcal.Calendar,
  d1: IsoDate,
  d2: IsoDate,
  largest: Unit,
  smallest: Unit,
  inc: Int,
  mode: RoundingMode,
) -> Result(Duration, JsError) {
  let sign = compare_iso_date(d2, d1)
  case sign == 0 {
    True -> Ok(zero_duration)
    False -> {
      let #(years, months, weeks, days) =
        calendar_date_until(cal, d1, d2, largest)
      case smallest == Day && inc == 1 {
        True -> Ok(Duration(..zero_duration, years:, months:, weeks:, days:))
        False ->
          round_relative_date_duration(
            #(years, months, weeks, days),
            #(d1, midnight),
            epoch_days(d2) * ns_per_day,
            largest,
            smallest,
            inc,
            mode,
            zoned: False,
          )
      }
    }
  }
}

pub fn iso_date_until(
  d1: IsoDate,
  d2: IsoDate,
  largest: Unit,
) -> #(Int, Int, Int, Int) {
  case largest {
    Year | Month -> {
      let sign = compare_iso_date(d2, d1)
      let total_months = count_months_between(d1, d2, sign)
      let #(years, months) = case largest {
        Year -> #(trunc_div(total_months, 12), trunc_mod(total_months, 12))
        _ -> #(0, total_months)
      }
      let intermediate = add_months_constrained(d1, years * 12 + months)
      let days = epoch_days(d2) - epoch_days(intermediate)
      #(years, months, 0, days)
    }
    Week -> {
      let days = epoch_days(d2) - epoch_days(d1)
      #(0, 0, trunc_div(days, 7), trunc_mod(days, 7))
    }
    _ -> #(0, 0, 0, epoch_days(d2) - epoch_days(d1))
  }
}

fn count_months_between(d1: IsoDate, d2: IsoDate, sign: Int) -> Int {
  let approx = { d2.year - d1.year } * 12 + d2.month - d1.month
  adjust_months(d1, d2, approx, sign)
}

fn adjust_months(d1: IsoDate, d2: IsoDate, candidate: Int, sign: Int) -> Int {
  // compares with unconstrained day, jan 29 + 1 month is "feb 29"
  let #(y, m) = balance_year_month(d1.year, d1.month + candidate)
  let cmp = compare_triple(#(y, m, d1.day), #(d2.year, d2.month, d2.day))
  case cmp * sign > 0 {
    True -> adjust_months(d1, d2, candidate - sign, sign)
    False -> candidate
  }
}

pub fn add_months_constrained(d: IsoDate, months: Int) -> IsoDate {
  let #(y, m) = balance_year_month(d.year, d.month + months)
  let day = int.min(d.day, days_in_month(y, m))
  IsoDate(y, m, day)
}

pub fn add_calendar_units(d: IsoDate, unit: Unit, n: Int) -> IsoDate {
  case unit {
    Year -> add_months_constrained(d, n * 12)
    Month -> add_months_constrained(d, n)
    _ -> add_days(d, n * 7)
  }
}

// step the end date toward the start when the time remainder has the other sign
pub fn adjust_date_for_time_sign(
  sign: Int,
  date: IsoDate,
  time_ns: Int,
) -> #(IsoDate, Int) {
  case sign > 0 && time_ns < 0, sign < 0 && time_ns > 0 {
    True, _ -> #(add_days(date, -1), time_ns + ns_per_day)
    _, True -> #(add_days(date, 1), time_ns - ns_per_day)
    _, _ -> #(date, time_ns)
  }
}

fn within(sign: Int, start: Int, end: Int, x: Int) -> Bool {
  case sign > 0 {
    True -> start <= x && x <= end
    False -> end <= x && x <= start
  }
}

pub type Window {
  Window(count: Int, start_ns: Int, end_ns: Int, shifted: Bool)
}

// the count whose [count, count + step] bounds enclose dest, sliding once if not
pub fn find_enclosing_window(
  sign: Int,
  count: Int,
  step: Int,
  dest_ns: Int,
  bound_ns: fn(Int) -> Result(Int, JsError),
) -> Result(Window, JsError) {
  use start_ns <- result.try(bound_ns(count))
  let next = count + step * sign
  use end_ns <- result.try(bound_ns(next))
  case within(sign, start_ns, end_ns, dest_ns) {
    True -> Ok(Window(count:, start_ns:, end_ns:, shifted: False))
    False -> {
      use beyond_ns <- result.map(bound_ns(next + step * sign))
      Window(count: next, start_ns: end_ns, end_ns: beyond_ns, shifted: True)
    }
  }
}

fn nudge_calendar_unit(
  sign: Int,
  ymwd: #(Int, Int, Int, Int),
  origin: #(IsoDate, IsoTime),
  dest_ns: Int,
  unit: Unit,
  inc: Int,
  mode: RoundingMode,
  zoned zoned: Bool,
) -> Result(#(Duration, Bool, Int), JsError) {
  let #(years, months, weeks, days) = ymwd
  let #(whole, with_count) = case unit {
    Year -> #(years, fn(r) { Duration(..zero_duration, years: r) })
    Month -> #(months, fn(r) { Duration(..zero_duration, years:, months: r) })
    Week -> #(weeks + trunc_div(days, 7), fn(r) {
      Duration(..zero_duration, years:, months:, weeks: r)
    })
    _ -> #(days, fn(r) {
      Duration(..zero_duration, years:, months:, weeks:, days: r)
    })
  }
  let bound_ns = fn(r) {
    use date <- result.try(iso_date_add(origin.0, with_count(r), Constrain))
    use Nil <- result.map(case zoned {
      True -> check_iso_days_range(date)
      False -> Ok(Nil)
    })
    utc_epoch_ns(date, origin.1)
  }
  let base = trunc_div(whole, inc) * inc
  use w <- result.map(find_enclosing_window(sign, base, inc, dest_ns, bound_ns))
  let r1 = w.count
  let r2 = r1 + inc * sign
  let num = dest_ns - w.start_ns
  let den = w.end_ns - w.start_ns
  let abs_r2 = int.absolute_value(r2)
  let rounded_abs =
    round_between(int.absolute_value(r1), abs_r2, num, den, inc, mode, sign)
  // nudged must match chosen or bubbling overshoots
  case rounded_abs == abs_r2 {
    True -> #(with_count(r2), True, w.end_ns)
    False -> #(with_count(r1), w.shifted, w.start_ns)
  }
}

pub fn bubble_date_duration(
  sign: Int,
  dur: Duration,
  nudged_ns: Int,
  origin: #(IsoDate, IsoTime),
  largest: Unit,
  start_unit: Unit,
) -> Duration {
  let candidates =
    case start_unit {
      Day -> [Week, Month, Year]
      Week -> [Month, Year]
      Month -> [Year]
      _ -> []
    }
    |> list.filter(fn(u) {
      unit_rank(u) <= unit_rank(largest) && { u != Week || largest == Week }
    })
  bubble_date_duration_loop(sign, dur, nudged_ns, origin, candidates)
}

fn bubble_date_duration_loop(
  sign: Int,
  dur: Duration,
  nudged_ns: Int,
  origin: #(IsoDate, IsoTime),
  candidates: List(Unit),
) -> Duration {
  case candidates {
    [] -> dur
    [u, ..rest] -> {
      let end_dur = case u {
        Year -> Duration(..zero_duration, years: dur.years + sign)
        Month ->
          Duration(..zero_duration, years: dur.years, months: dur.months + sign)
        _ ->
          Duration(
            ..zero_duration,
            years: dur.years,
            months: dur.months,
            weeks: dur.weeks + sign,
          )
      }
      let end_date =
        add_months_constrained(origin.0, end_dur.years * 12 + end_dur.months)
        |> add_days(end_dur.weeks * 7)
      let end_ns = utc_epoch_ns(end_date, origin.1)
      case int_sign(nudged_ns - end_ns) != 0 - sign {
        True ->
          bubble_date_duration_loop(sign, end_dur, nudged_ns, origin, rest)
        False -> dur
      }
    }
  }
}

pub fn round_relative_date_duration(
  ymwd: #(Int, Int, Int, Int),
  origin: #(IsoDate, IsoTime),
  dest_ns: Int,
  largest: Unit,
  smallest: Unit,
  inc: Int,
  mode: RoundingMode,
  zoned zoned: Bool,
) -> Result(Duration, JsError) {
  let sign = case int_sign(dest_ns - utc_epoch_ns(origin.0, origin.1)) {
    -1 -> -1
    _ -> 1
  }
  use #(dur, did_expand, nudged) <- result.map(nudge_calendar_unit(
    sign,
    ymwd,
    origin,
    dest_ns,
    smallest,
    inc,
    mode,
    zoned:,
  ))
  case did_expand && smallest != Week {
    True ->
      bubble_date_duration(
        sign,
        dur,
        nudged,
        origin,
        largest,
        max_unit(smallest, Day),
      )
    False -> dur
  }
}

pub fn diff_date_time_core(
  cal: tcal.Calendar,
  a: #(IsoDate, IsoTime),
  b: #(IsoDate, IsoTime),
  largest: Unit,
  smallest: Unit,
  inc: Int,
  mode: RoundingMode,
  zoned zoned: Bool,
) -> Result(Duration, JsError) {
  let date_sign = compare_iso_date(b.0, a.0)
  let #(b_date, time_diff) =
    adjust_date_for_time_sign(date_sign, b.0, time_to_ns(b.1) - time_to_ns(a.1))
  case unit_rank(largest) >= unit_rank(Day) {
    True -> {
      let #(years, months, weeks, days) =
        calendar_date_until(cal, a.0, b_date, largest)
      case
        unit_rank(smallest) > unit_rank(Day) || { zoned && smallest == Day }
      {
        True ->
          round_relative_date_duration(
            #(years, months, weeks, days),
            a,
            utc_epoch_ns(b.0, b.1),
            largest,
            smallest,
            inc,
            mode,
            zoned:,
          )
        False -> {
          use smallest_time_unit <- result.try(require_time_unit(smallest))
          let time_total = days * ns_per_day + time_diff
          let rounded = case smallest == Nanosecond && inc == 1 {
            True -> time_total
            False ->
              round_to_increment(
                time_total,
                inc * time_unit_ns(smallest_time_unit),
                mode,
              )
          }
          let whole_days = trunc_div(time_total, ns_per_day)
          let rounded_whole = trunc_div(rounded, ns_per_day)
          let rem_ns = rounded - rounded_whole * ns_per_day
          let time_part = balance_time_ns(rem_ns, Hour)
          let base =
            Duration(..time_part, years:, months:, weeks:, days: rounded_whole)
          let did_expand =
            int_sign(rounded_whole - whole_days) == int_sign(time_total)
          case did_expand {
            False -> Ok(base)
            True -> {
              let dest_ns = utc_epoch_ns(b.0, b.1)
              let nudged = dest_ns + rounded - time_total
              let dsign = case int_sign(dest_ns - utc_epoch_ns(a.0, a.1)) {
                -1 -> -1
                _ -> 1
              }
              Ok(bubble_date_duration(dsign, base, nudged, a, largest, Day))
            }
          }
        }
      }
    }
    False -> {
      use smallest_time_unit <- result.try(require_time_unit(smallest))
      let total = utc_epoch_ns(b.0, b.1) - utc_epoch_ns(a.0, a.1)
      let rounded =
        round_to_increment(total, inc * time_unit_ns(smallest_time_unit), mode)
      Ok(balance_time_ns(rounded, largest))
    }
  }
}

pub fn zoned_diff_round_time(
  cal: tcal.Calendar,
  tz: TimeZone,
  a_ns: Int,
  b_ns: Int,
  largest: Unit,
  smallest: Unit,
  inc: Int,
  mode: RoundingMode,
) -> Result(Duration, JsError) {
  let #(a_d, a_t) = epoch_ns_to_iso_in(tz, a_ns)
  let #(b_d, b_t) = epoch_ns_to_iso_in(tz, b_ns)
  let sign = case b_ns < a_ns {
    True -> -1
    False -> 1
  }
  let #(b_date, _) =
    adjust_date_for_time_sign(sign, b_d, time_to_ns(b_t) - time_to_ns(a_t))
  let #(years, months, weeks, days) =
    calendar_date_until(cal, a_d, b_date, largest)
  let date_dur = Duration(..zero_duration, years:, months:, weeks:, days:)
  use start_date <- result.try(calendar_date_add(cal, a_d, date_dur, Constrain))
  use start_ns <- result.try(get_epoch_ns_for(tz, start_date, a_t, Compatible))
  let time_rem = b_ns - start_ns
  case smallest == Nanosecond && inc == 1 {
    // skip next-day bound, it can be out of range
    True -> {
      let time_part = balance_time_ns(time_rem, Hour)
      Ok(Duration(..time_part, years:, months:, weeks:, days:))
    }
    False ->
      zoned_nudge_time(
        tz,
        #(a_d, a_t),
        start_date,
        start_ns,
        time_rem,
        #(years, months, weeks, days),
        sign,
        largest,
        smallest,
        inc,
        mode,
      )
  }
}

fn zoned_nudge_time(
  tz: TimeZone,
  a_dt: #(IsoDate, IsoTime),
  start_date: IsoDate,
  start_ns: Int,
  time_rem: Int,
  ymwd: #(Int, Int, Int, Int),
  sign: Int,
  largest: Unit,
  smallest: Unit,
  inc: Int,
  mode: RoundingMode,
) -> Result(Duration, JsError) {
  let #(a_d, a_t) = a_dt
  let #(years, months, weeks, days) = ymwd
  use smallest_time_unit <- result.try(require_time_unit(smallest))
  let end_date = add_days(start_date, sign)
  use end_ns <- result.try(get_epoch_ns_for(tz, end_date, a_t, Compatible))
  let day_span = end_ns - start_ns
  let smallest_ns = inc * time_unit_ns(smallest_time_unit)
  let rounded_t = round_to_increment(time_rem, smallest_ns, mode)
  let beyond = rounded_t - day_span
  case int_sign(beyond) != 0 - sign {
    True -> {
      let rounded_t2 =
        round_to_increment(time_rem - day_span, smallest_ns, mode)
      let time_part = balance_time_ns(rounded_t2, Hour)
      let base =
        Duration(..time_part, years:, months:, weeks:, days: days + sign)
      let #(n_d, n_t) = epoch_ns_to_iso_in(tz, end_ns + rounded_t2)
      let nudged_ns = utc_epoch_ns(n_d, n_t)
      Ok(bubble_date_duration(sign, base, nudged_ns, #(a_d, a_t), largest, Day))
    }
    False -> {
      let time_part = balance_time_ns(rounded_t, Hour)
      Ok(Duration(..time_part, years:, months:, weeks:, days:))
    }
  }
}

pub fn larger_time_unit(a: Duration, b: Duration) -> Unit {
  max_unit(time_unit_of(a), time_unit_of(b))
}

fn time_unit_of(d: Duration) -> Unit {
  case d.days != 0, d.hours != 0, d.minutes != 0, d.seconds != 0 {
    True, _, _, _ -> Day
    _, True, _, _ -> Hour
    _, _, True, _ -> Minute
    _, _, _, True -> Second
    _, _, _, _ ->
      case d.milliseconds != 0, d.microseconds != 0 {
        True, _ -> Millisecond
        _, True -> Microsecond
        _, _ -> Nanosecond
      }
  }
}

pub fn default_largest_unit(d: Duration) -> Unit {
  case d.years != 0, d.months != 0, d.weeks != 0 {
    True, _, _ -> Year
    _, True, _ -> Month
    _, _, True -> Week
    _, _, _ -> time_unit_of(d)
  }
}
