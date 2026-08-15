//// The difference/rounding cores shared by the Temporal types:
//// DifferenceISODateTime and DifferenceZonedDateTime with NudgeToCalendarUnit,
//// NudgeToDayOrTime, NudgeToZonedTime and BubbleRelativeDuration, plus the
//// ISO month stepping they use. PlainDate/PlainDateTime/ZonedDateTime
//// until/since and Duration.round/total all go through here.
////
//// Options, units and duration records are temporal_common.gleam; calendar
//// fields and CalendarDateAdd/Until are temporal_fields.gleam; exact-time
//// resolution and relativeTo are temporal_zoned_ops.gleam.

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
  balance_year_month, calendar_date_add, calendar_date_until, compare_iso_date,
  compare_triple, round_between,
}
import arc/rt/builtins/temporal_iso.{
  type DurRec, type IsoDate, type TErr, type TimeRec, Constrain, DurRec, IsoDate,
  RangeE, epoch_days, int_sign, iso_date_from_epoch_days, iso_date_within_limits,
  midnight, ns_per_day, time_to_ns, utc_epoch_ns, zero_dur,
}
import arc/rt/builtins/temporal_zoned_ops.{
  check_iso_days_range, get_epoch_ns_for,
}
import arc/rt/types.{type TimeZone}
import gleam/int
import gleam/list
import gleam/result

// ============================================================================
// Date arithmetic helpers
// ============================================================================

pub fn compare_iso_date_time(
  a: #(IsoDate, TimeRec),
  b: #(IsoDate, TimeRec),
) -> Int {
  int_sign(utc_epoch_ns(a.0, a.1) - utc_epoch_ns(b.0, b.1))
}

/// Local timeline nanoseconds for a date-time (days since epoch * 86400e9 +
/// time of day). Differences in this space equal epoch-ns differences for any
/// fixed-offset time zone.
pub fn local_ns(d: IsoDate, t: TimeRec) -> Int {
  epoch_days(d) * ns_per_day + time_to_ns(t)
}

/// The exact date difference decomposed per `largest`, in `cal`.
pub fn date_parts_in(
  cal: tcal.Calendar,
  d1: IsoDate,
  d2: IsoDate,
  largest: Unit,
) -> #(Int, Int, Int, Int) {
  case cal {
    tcal.Iso8601 -> diff_date_parts(d1, d2, largest)
    _ ->
      case largest {
        Year | Month -> {
          let #(y, m, rem_days) =
            calendar_date_until(cal, d1, d2, whole_years: largest == Year)
          #(y, m, 0, rem_days)
        }
        _ -> diff_date_parts(d1, d2, largest)
      }
  }
}

// ============================================================================
// Difference and rounding cores
// ============================================================================

/// DifferenceDate in a specific calendar + date-unit rounding.
pub fn difference_calendar_date(
  cal: tcal.Calendar,
  d1: IsoDate,
  d2: IsoDate,
  largest: Unit,
  smallest: Unit,
  inc: Int,
  mode: RoundingMode,
) -> Result(DurRec, TErr) {
  let sign = compare_iso_date(d2, d1)
  case sign == 0 {
    True -> Ok(zero_dur)
    False -> {
      // Compute exact difference at `largest` granularity.
      let #(years, months, weeks, days) = date_parts_in(cal, d1, d2, largest)
      // Round to smallest/increment if needed.
      case smallest == Day && inc == 1 {
        True -> Ok(DurRec(..zero_dur, years:, months:, weeks:, days:))
        False ->
          round_relative_date_duration(
            #(years, months, weeks, days),
            #(d1, midnight),
            epoch_days(d2) * ns_per_day,
            largest,
            smallest,
            inc,
            mode,
            False,
          )
      }
    }
  }
}

/// Exact ISO calendar difference decomposed per largestUnit.
pub fn diff_date_parts(
  d1: IsoDate,
  d2: IsoDate,
  largest: Unit,
) -> #(Int, Int, Int, Int) {
  case largest {
    Year | Month -> {
      let sign = compare_iso_date(d2, d1)
      // months difference counting whole months.
      let total_months = count_months_between(d1, d2, sign)
      let #(years, months) = case largest {
        Year -> #(trunc_div(total_months, 12), trunc_mod(total_months, 12))
        _ -> #(0, total_months)
      }
      // Remaining days after adding years+months to d1.
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

/// Count whole months from d1 toward d2 (sign = direction).
fn count_months_between(d1: IsoDate, d2: IsoDate, sign: Int) -> Int {
  let approx = { d2.year - d1.year } * 12 + d2.month - d1.month
  // Adjust: stepping by whole months must not surpass d2.
  adjust_months(d1, d2, approx, sign)
}

fn adjust_months(d1: IsoDate, d2: IsoDate, candidate: Int, sign: Int) -> Int {
  // ISODateSurpasses: the stepped date keeps the original day-of-month
  // (unconstrained) — Jan 29th + 1 month counts as "Feb 29th" for the
  // comparison, so until(Jan 29, Feb 28) is 30 days, not one month.
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

/// Add `n` of a calendar `unit` (year/month/week) to an ISO date, constraining
/// the day-of-month.
pub fn add_calendar_units(d: IsoDate, unit: Unit, n: Int) -> IsoDate {
  case unit {
    Year -> add_months_constrained(d, n * 12)
    Month -> add_months_constrained(d, n)
    _ -> iso_date_from_epoch_days(epoch_days(d) + n * 7)
  }
}

/// ISO CalendarDateAdd: years+months with day constrained, then weeks/days.
/// RangeError when the result is outside the ISO date limits.
fn cal_date_add_checked(d: IsoDate, dur: DurRec) -> Result(IsoDate, TErr) {
  let md = add_months_constrained(d, dur.years * 12 + dur.months)
  let r = iso_date_from_epoch_days(epoch_days(md) + dur.weeks * 7 + dur.days)
  case iso_date_within_limits(r) {
    True -> Ok(r)
    False -> Error(RangeE("date outside of supported range"))
  }
}

/// ComputeNudgeWindow: bounding durations/instants for rounding `unit`.
/// Returns #(r1, r2, start_dur, end_dur, start_ns, end_ns).
fn nudge_window(
  sign: Int,
  ymwd: #(Int, Int, Int, Int),
  origin: #(IsoDate, TimeRec),
  unit: Unit,
  inc: Int,
  shift: Bool,
  zoned: Bool,
) -> Result(#(Int, Int, DurRec, DurRec, Int, Int), TErr) {
  let #(years, months, weeks, days) = ymwd
  let #(whole, mk) = case unit {
    Year -> #(years, fn(r) { DurRec(..zero_dur, years: r) })
    Month -> #(months, fn(r) { DurRec(..zero_dur, years:, months: r) })
    Week -> #(weeks + trunc_div(days, 7), fn(r) {
      DurRec(..zero_dur, years:, months:, weeks: r)
    })
    _ -> #(days, fn(r) { DurRec(..zero_dur, years:, months:, weeks:, days: r) })
  }
  let base = trunc_div(whole, inc) * inc
  let r1 = case shift {
    True -> base + inc * sign
    False -> base
  }
  let r2 = r1 + inc * sign
  let start_dur = mk(r1)
  let end_dur = mk(r2)
  use start_date <- result.try(cal_date_add_checked(origin.0, start_dur))
  use end_date <- result.try(cal_date_add_checked(origin.0, end_dur))
  // Zoned bounds go through GetEpochNanosecondsFor, whose CheckISODaysRange
  // is stricter (plain/exact ±1e8 days) than the noon-based date limits.
  use Nil <- result.try(case zoned {
    True -> {
      use Nil <- result.try(check_iso_days_range(start_date))
      check_iso_days_range(end_date)
    }
    False -> Ok(Nil)
  })
  let start_ns = case start_dur == zero_dur {
    True -> local_ns(origin.0, origin.1)
    False -> local_ns(start_date, origin.1)
  }
  let end_ns = local_ns(end_date, origin.1)
  Ok(#(r1, r2, start_dur, end_dur, start_ns, end_ns))
}

/// NudgeToCalendarUnit: round the calendar `unit` of a date duration by
/// bounding it between whole-unit instants on the local timeline.
/// Returns #(rounded date duration, did_expand, nudged_ns).
fn nudge_calendar_unit(
  sign: Int,
  ymwd: #(Int, Int, Int, Int),
  origin: #(IsoDate, TimeRec),
  dest_ns: Int,
  unit: Unit,
  inc: Int,
  mode: RoundingMode,
  zoned: Bool,
) -> Result(#(DurRec, Bool, Int), TErr) {
  use w0 <- result.try(nudge_window(sign, ymwd, origin, unit, inc, False, zoned))
  let in_bounds = fn(w: #(Int, Int, DurRec, DurRec, Int, Int)) {
    case sign > 0 {
      True -> w.4 <= dest_ns && dest_ns <= w.5
      False -> w.5 <= dest_ns && dest_ns <= w.4
    }
  }
  // Retry one increment further when end-of-month clamping made the first
  // window not contain the destination.
  use #(w, pre_expanded) <- result.try(case in_bounds(w0) {
    True -> Ok(#(w0, False))
    False -> {
      use w1 <- result.map(nudge_window(
        sign,
        ymwd,
        origin,
        unit,
        inc,
        True,
        zoned,
      ))
      #(w1, True)
    }
  })
  let #(r1, r2, start_dur, end_dur, start_ns, end_ns) = w
  let num = dest_ns - start_ns
  let den = end_ns - start_ns
  let abs_r1 = int.absolute_value(r1)
  let abs_r2 = int.absolute_value(r2)
  let rounded_abs = round_between(abs_r1, abs_r2, num, den, inc, mode, sign)
  let expanded_here = rounded_abs == abs_r2
  let did_expand = pre_expanded || expanded_here
  let chosen = case expanded_here {
    True -> end_dur
    False -> start_dur
  }
  // nudged must correspond to `chosen`: bubble_date_duration compares it
  // against the larger-unit boundary, so passing the ns of a duration one
  // increment beyond the chosen one could wrongly bubble (e.g. 1 year
  // instead of 12 months) when the window was pre-shifted by end-of-month
  // clamping. did_expand (pre_expanded || expanded_here) still triggers
  // the bubble check.
  let nudged = case expanded_here {
    True -> end_ns
    False -> start_ns
  }
  Ok(#(chosen, did_expand, nudged))
}

/// BubbleRelativeDuration: carry a nudged duration into larger units while
/// the nudged instant sits exactly on (or beyond) the larger-unit boundary.
pub fn bubble_date_duration(
  sign: Int,
  dur: DurRec,
  nudged_ns: Int,
  origin: #(IsoDate, TimeRec),
  largest: Unit,
  start_unit: Unit,
) -> DurRec {
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
  bubble_loop(sign, dur, nudged_ns, origin, candidates)
}

fn bubble_loop(
  sign: Int,
  dur: DurRec,
  nudged_ns: Int,
  origin: #(IsoDate, TimeRec),
  candidates: List(Unit),
) -> DurRec {
  case candidates {
    [] -> dur
    [u, ..rest] -> {
      let end_dur = case u {
        Year -> DurRec(..zero_dur, years: dur.years + sign)
        Month -> DurRec(..zero_dur, years: dur.years, months: dur.months + sign)
        _ ->
          DurRec(
            ..zero_dur,
            years: dur.years,
            months: dur.months,
            weeks: dur.weeks + sign,
          )
      }
      let end_date =
        add_months_constrained(origin.0, end_dur.years * 12 + end_dur.months)
      let end_date =
        iso_date_from_epoch_days(epoch_days(end_date) + end_dur.weeks * 7)
      let end_ns = local_ns(end_date, origin.1)
      case int_sign(nudged_ns - end_ns) != 0 - sign {
        True -> bubble_loop(sign, end_dur, nudged_ns, origin, rest)
        False -> dur
      }
    }
  }
}

/// RoundRelativeDuration for a date-unit smallestUnit: nudge then bubble.
pub fn round_relative_date_duration(
  ymwd: #(Int, Int, Int, Int),
  origin: #(IsoDate, TimeRec),
  dest_ns: Int,
  largest: Unit,
  smallest: Unit,
  inc: Int,
  mode: RoundingMode,
  zoned: Bool,
) -> Result(DurRec, TErr) {
  let sign = case int_sign(dest_ns - local_ns(origin.0, origin.1)) {
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
    zoned,
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

/// Difference between two ISO date-times decomposed per largest/smallest
/// units with rounding. Shared by PlainDateTime and ZonedDateTime until/since
/// and by Duration.round/total with a relativeTo.
pub fn diff_date_time_core(
  cal: tcal.Calendar,
  a: #(IsoDate, TimeRec),
  b: #(IsoDate, TimeRec),
  largest: Unit,
  smallest: Unit,
  inc: Int,
  mode2: RoundingMode,
  zoned: Bool,
) -> Result(DurRec, TErr) {
  // Time difference first; borrow a day if signs conflict.
  let date_sign = compare_iso_date(b.0, a.0)
  let time_diff = time_to_ns(b.1) - time_to_ns(a.1)
  let #(b_date, time_diff) = case
    date_sign > 0 && time_diff < 0,
    date_sign < 0 && time_diff > 0
  {
    True, _ -> #(
      iso_date_from_epoch_days(epoch_days(b.0) - 1),
      time_diff + ns_per_day,
    )
    _, True -> #(
      iso_date_from_epoch_days(epoch_days(b.0) + 1),
      time_diff - ns_per_day,
    )
    _, _ -> #(b.0, time_diff)
  }
  case unit_rank(largest) >= unit_rank(Day) {
    True -> {
      let #(years, months, weeks, days) =
        date_parts_in(cal, a.0, b_date, largest)
      case
        unit_rank(smallest) > unit_rank(Day) || { zoned && smallest == Day }
      {
        // Calendar-unit smallestUnit (or day with a time zone, whose length
        // varies): epoch-ns bounding (NudgeToCalendarUnit). A plain `day` is
        // uniform 24 hours and is rounded numerically below.
        True ->
          round_relative_date_duration(
            #(years, months, weeks, days),
            a,
            local_ns(b.0, b.1),
            largest,
            smallest,
            inc,
            mode2,
            zoned,
          )
        False -> {
          // Time-unit smallestUnit: round days+time in ns (NudgeToDayOrTime).
          use su <- result.try(require_time_unit(smallest))
          let time_total = days * ns_per_day + time_diff
          let rounded = case smallest == Nanosecond && inc == 1 {
            True -> time_total
            False ->
              round_to_increment(time_total, inc * time_unit_ns(su), mode2)
          }
          let whole_days = trunc_div(time_total, ns_per_day)
          let rounded_whole = trunc_div(rounded, ns_per_day)
          let rem_ns = rounded - rounded_whole * ns_per_day
          let time_part = balance_time_ns(rem_ns, Hour)
          let base =
            DurRec(..time_part, years:, months:, weeks:, days: rounded_whole)
          let did_expand =
            int_sign(rounded_whole - whole_days) == int_sign(time_total)
          case did_expand {
            False -> Ok(base)
            True -> {
              let dest_ns = local_ns(b.0, b.1)
              let nudged = dest_ns + rounded - time_total
              let dsign = case int_sign(dest_ns - local_ns(a.0, a.1)) {
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
      // Pure time-based difference.
      use su <- result.try(require_time_unit(smallest))
      let total =
        { epoch_days(b.0) - epoch_days(a.0) }
        * ns_per_day
        + { time_to_ns(b.1) - time_to_ns(a.1) }
      let rounded = round_to_increment(total, inc * time_unit_ns(su), mode2)
      Ok(balance_time_ns(rounded, largest))
    }
  }
}

/// DifferenceZonedDateTime + NudgeToZonedTime: difference between two zoned
/// instants, rounded at a time unit. Days are measured between wall-clock
/// instants (variable length); the time remainder is rounded within the
/// final day and carries into it when it overflows.
pub fn zoned_diff_round_time(
  cal: tcal.Calendar,
  tz: TimeZone,
  a_ns: Int,
  b_ns: Int,
  largest: Unit,
  smallest: Unit,
  inc: Int,
  mode: RoundingMode,
) -> Result(DurRec, TErr) {
  // Note: a zero difference still computes the next-day boundary, which can
  // throw when the anchor sits at the edge of the representable range
  // (NudgeToZonedTime always materialises both day bounds).
  use #(a_d, a_t) <- result.try(epoch_ns_to_iso_in(tz, a_ns))
  use #(b_d, b_t) <- result.try(epoch_ns_to_iso_in(tz, b_ns))
  let sign = case b_ns < a_ns {
    True -> -1
    False -> 1
  }
  // Wall-clock date difference with a time borrow.
  let tb = time_to_ns(b_t) - time_to_ns(a_t)
  let b_date = case sign > 0 && tb < 0, sign < 0 && tb > 0 {
    True, _ -> iso_date_from_epoch_days(epoch_days(b_d) - 1)
    _, True -> iso_date_from_epoch_days(epoch_days(b_d) + 1)
    _, _ -> b_d
  }
  let #(years, months, weeks, days) = date_parts_in(cal, a_d, b_date, largest)
  let date_dur = DurRec(..zero_dur, years:, months:, weeks:, days:)
  use start_date <- result.try(calendar_date_add(cal, a_d, date_dur, Constrain))
  use start_ns <- result.try(get_epoch_ns_for(tz, start_date, a_t, Compatible))
  let time_rem = b_ns - start_ns
  case smallest == Nanosecond && inc == 1 {
    // Rounding is a noop: balance only, without materialising the
    // next-day boundary (which can be out of range at the edges).
    True -> {
      let time_part = balance_time_ns(time_rem, Hour)
      Ok(DurRec(..time_part, years:, months:, weeks:, days:))
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

/// NudgeToZonedTime: round the time remainder within the (variable-length)
/// final day, carrying into it on overflow and bubbling into larger units.
fn zoned_nudge_time(
  tz: TimeZone,
  a_dt: #(IsoDate, TimeRec),
  start_date: IsoDate,
  start_ns: Int,
  time_rem: Int,
  ymwd: #(Int, Int, Int, Int),
  sign: Int,
  largest: Unit,
  smallest: Unit,
  inc: Int,
  mode: RoundingMode,
) -> Result(DurRec, TErr) {
  let #(a_d, a_t) = a_dt
  let #(years, months, weeks, days) = ymwd
  use su <- result.try(require_time_unit(smallest))
  let end_date = iso_date_from_epoch_days(epoch_days(start_date) + sign)
  use end_ns <- result.try(get_epoch_ns_for(tz, end_date, a_t, Compatible))
  let day_span = end_ns - start_ns
  let smallest_ns = inc * time_unit_ns(su)
  let rounded_t = round_to_increment(time_rem, smallest_ns, mode)
  let beyond = rounded_t - day_span
  case int_sign(beyond) != 0 - sign {
    // Rounded time fills (or exceeds) the whole day: carry one day and
    // round the remainder beyond it, then bubble into larger units.
    True -> {
      let rounded_t2 =
        round_to_increment(time_rem - day_span, smallest_ns, mode)
      let time_part = balance_time_ns(rounded_t2, Hour)
      let base = DurRec(..time_part, years:, months:, weeks:, days: days + sign)
      let nudged_inst = end_ns + rounded_t2
      use #(n_d, n_t) <- result.map(epoch_ns_to_iso_in(tz, nudged_inst))
      bubble_date_duration(
        sign,
        base,
        local_ns(n_d, n_t),
        #(a_d, a_t),
        largest,
        Day,
      )
    }
    False -> {
      let time_part = balance_time_ns(rounded_t, Hour)
      Ok(DurRec(..time_part, years:, months:, weeks:, days:))
    }
  }
}

/// The largest fixed-length unit with a nonzero field across `a` and `b`
/// (the balance target of Duration add/subtract).
pub fn larger_time_unit(a: DurRec, b: DurRec) -> Unit {
  max_unit(time_unit_of(a), time_unit_of(b))
}

fn time_unit_of(d: DurRec) -> Unit {
  case d.days != 0, d.hours != 0, d.minutes != 0, d.seconds != 0 {
    True, _, _, _ -> Day
    _, True, _, _ -> Hour
    _, _, True, _ -> Minute
    _, _, _, True -> Second
    _, _, _, _ ->
      case d.ms != 0, d.us != 0 {
        True, _ -> Millisecond
        _, True -> Microsecond
        _, _ -> Nanosecond
      }
  }
}

/// The largest unit with a nonzero field (DefaultTemporalLargestUnit).
pub fn default_largest_unit(d: DurRec) -> Unit {
  case d.years != 0, d.months != 0, d.weeks != 0 {
    True, _, _ -> Year
    _, True, _ -> Month
    _, _, True -> Week
    _, _, _ -> time_unit_of(d)
  }
}
