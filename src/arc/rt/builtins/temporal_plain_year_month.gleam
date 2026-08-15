//// Temporal.PlainYearMonth (proposal-temporal §9): constructor, from /
//// compare, the prototype getters and methods, and ToTemporalYearMonth.
////
//// The slot holds the ISO date of the calendar month's reference day (day 1
//// of the calendar month; day 1 for iso8601) plus the calendar.

import arc/internal/gregorian.{
  days_in_month, days_in_year as days_in_iso_year, is_leap_year,
}
import arc/internal/int_math.{trunc_div, trunc_mod}
import arc/internal/temporal_calendar as tcal
import arc/rt/builtins/helpers
import arc/rt/builtins/temporal_common.{
  type CalendarNameMode, type RoundingMode, CalAlways, CalAuto, CalCritical,
  CalNever, Month, Year, apply_since_dur, apply_since_mode, arg_trunc_int,
  arg_trunc_int_or, calendar_suffix, get_calendar_name_option,
  get_difference_settings, make_date_cal, make_duration, make_year_month,
  make_year_month_cal, max_unit, read_pos_int_field, require_largest_ge_smallest,
  require_temporal, round_to_increment, terr, unit_rank, year_month_slot_of,
}
import arc/rt/builtins/temporal_fields.{
  type DateFields, DateFields, add_sub_args, balance_year_month,
  calendar_date_add, calendar_date_until, check_ym_limits, compare_iso_date,
  era_field, era_year_field, int_val, merge_year_month_code, month_code_str,
  parse_year_month_string, read_bag_calendar, read_year_month_fields,
  regulate_calendar_day, require_nonempty_fields, require_partial_bag,
  resolve_calendar_month, resolve_calendar_year, resolve_iso_month,
  round_between, to_calendar_arg, validated_overflow,
}
import arc/rt/builtins/temporal_iso.{
  type IsoDate, type Overflow, type TErr, Constrain, DurRec, IsoDate, RangeE,
  Reject, TypeE, check_date_limits, epoch_days, format_iso_date, format_iso_year,
  is_valid_iso_date, iso_date_from_epoch_days, iso_year_month_within_limits,
  pad2, regulate_iso_date, zero_dur,
}
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type JsVal, type NativeToken, type PlainYearMonthMethod,
  type TemporalProtos, type TemporalStaticName, type TemporalYearMonthGetter,
  KHandle, KStr, PymAdd, PymEquals, PymSince, PymSubtract, PymToJson,
  PymToLocaleString, PymToPlainDate, PymToString, PymUntil, PymValueOf, PymWith,
  SObject, TemporalN, TemporalPlainYearMonthCtor, TemporalPlainYearMonthGetter,
  TemporalPlainYearMonthMethod, TemporalPlainYearMonthStatic, TsCompare, TsFrom,
  YmCalendarId, YmDaysInMonth, YmDaysInYear, YmEra, YmEraYear, YmInLeapYear,
  YmMonth, YmMonthCode, YmMonthsInYear, YmYear, classify, mk_bool, mk_string,
  mk_undefined,
}
import arc/rt/val as rt_val
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result

// ============================================================================
// Registration tables — the only place the JS-facing names are written down
// ============================================================================

const all_getters = [
  YmCalendarId,
  YmEra,
  YmEraYear,
  YmYear,
  YmMonth,
  YmMonthCode,
  YmDaysInYear,
  YmDaysInMonth,
  YmMonthsInYear,
  YmInLeapYear,
]

const all_methods = [
  #(PymWith, 1),
  #(PymAdd, 1),
  #(PymSubtract, 1),
  #(PymUntil, 1),
  #(PymSince, 1),
  #(PymEquals, 1),
  #(PymToString, 0),
  #(PymToLocaleString, 0),
  #(PymToJson, 0),
  #(PymValueOf, 0),
  #(PymToPlainDate, 1),
]

/// Registration specs for `temporal.init_temporal_type`: the constructor
/// token, `from`/`compare`, the getters and the prototype methods, in
/// prototype-registration order.
pub fn ctor_token(protos: TemporalProtos) -> NativeToken {
  TemporalN(TemporalPlainYearMonthCtor(protos:))
}

pub fn statics(protos: TemporalProtos) -> List(#(String, NativeToken, Int)) {
  list.map([#(TsFrom, 1), #(TsCompare, 2)], fn(s) {
    #(
      static_name(s.0),
      TemporalN(TemporalPlainYearMonthStatic(s.0, protos)),
      s.1,
    )
  })
}

pub fn getters() -> List(#(String, NativeToken)) {
  list.map(all_getters, fn(g) {
    #(getter_name(g), TemporalN(TemporalPlainYearMonthGetter(g)))
  })
}

pub fn methods(protos: TemporalProtos) -> List(#(String, NativeToken, Int)) {
  list.map(all_methods, fn(m) {
    #(
      method_name(m.0),
      TemporalN(TemporalPlainYearMonthMethod(m.0, protos)),
      m.1,
    )
  })
}

fn static_name(s: TemporalStaticName) -> String {
  case s {
    TsFrom -> "from"
    TsCompare -> "compare"
  }
}

pub fn getter_name(g: TemporalYearMonthGetter) -> String {
  case g {
    YmCalendarId -> "calendarId"
    YmEra -> "era"
    YmEraYear -> "eraYear"
    YmYear -> "year"
    YmMonth -> "month"
    YmMonthCode -> "monthCode"
    YmDaysInYear -> "daysInYear"
    YmDaysInMonth -> "daysInMonth"
    YmMonthsInYear -> "monthsInYear"
    YmInLeapYear -> "inLeapYear"
  }
}

pub fn method_name(m: PlainYearMonthMethod) -> String {
  case m {
    PymWith -> "with"
    PymAdd -> "add"
    PymSubtract -> "subtract"
    PymUntil -> "until"
    PymSince -> "since"
    PymEquals -> "equals"
    PymToString -> "toString"
    PymToLocaleString -> "toLocaleString"
    PymToJson -> "toJSON"
    PymValueOf -> "valueOf"
    PymToPlainDate -> "toPlainDate"
  }
}

// ============================================================================
// Constructor and statics
// ============================================================================

/// new Temporal.PlainYearMonth(year, month [, calendar [, referenceISODay]])
/// — allocated on the intrinsic prototype; the caller applies NewTarget's.
pub fn ctor(
  st: Agent,
  protos: TemporalProtos,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(y, st) = arg_trunc_int(st, args, 0)
  let #(m, st) = arg_trunc_int(st, args, 1)
  let cal = terr(st, to_calendar_arg(helpers.arg_at(args, 2)))
  let #(d, st) = arg_trunc_int_or(st, args, 3, 1)
  case is_valid_iso_date(y, m, d) {
    False -> rt_val.t_throw_range_error(st, "invalid ISO year-month")
    True ->
      case iso_year_month_within_limits(y, m) {
        False ->
          rt_val.t_throw_range_error(
            st,
            "year-month outside of supported range",
          )
        True -> make_year_month_cal(st, protos, y, m, d, cal)
      }
  }
}

/// Temporal.PlainYearMonth.from / compare
pub fn static(
  st: Agent,
  name: TemporalStaticName,
  protos: TemporalProtos,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case name {
    TsFrom -> {
      let #(#(y, m, rd, cal), st) =
        to_temporal_year_month(
          st,
          helpers.arg_at(args, 0),
          helpers.arg_at(args, 1),
        )
      make_year_month_cal(st, protos, y, m, rd, cal)
    }
    TsCompare -> {
      let #(a, st) =
        to_temporal_year_month(st, helpers.arg_at(args, 0), mk_undefined())
      let #(b, st) =
        to_temporal_year_month(st, helpers.arg_at(args, 1), mk_undefined())
      // CompareISODate including the reference day.
      let n = compare_iso_date(IsoDate(a.0, a.1, a.2), IsoDate(b.0, b.1, b.2))
      #(int_val(n), st)
    }
  }
}

// ============================================================================
// ToTemporalYearMonth
// ============================================================================

/// ToTemporalYearMonth(item [, options]) → #(iso_year, iso_month,
/// reference_day, calendar).
pub fn to_temporal_year_month(
  st: Agent,
  item: JsVal,
  options: JsVal,
) -> #(#(Int, Int, Int, tcal.Calendar), Agent) {
  case classify(item) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind:, ..) ->
          case year_month_slot_of(kind) {
            Some(ym) -> {
              let #(_o, st) = validated_overflow(st, options)
              #(ym, st)
            }
            None -> year_month_from_bag(st, h, options)
          }
        _ -> year_month_from_bag(st, h, options)
      }
    KStr(s) -> {
      let ym = terr(st, parse_year_month_string(s))
      let #(_o, st) = validated_overflow(st, options)
      #(ym, st)
    }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "cannot convert to a Temporal.PlainYearMonth",
      )
  }
}

/// Property bag → year-month. Fields: calendar, then era, eraYear, month,
/// monthCode, year. Returns the ISO date of the calendar month's first day.
fn year_month_from_bag(
  st: Agent,
  h: types.Handle,
  options: JsVal,
) -> #(#(Int, Int, Int, tcal.Calendar), Agent) {
  let #(cal, st) = read_bag_calendar(st, h)
  let #(fields, st) = read_year_month_fields(st, h, cal)
  let #(overflow, st) = validated_overflow(st, options)
  #(terr(st, resolve_calendar_year_month(cal, fields, overflow)), st)
}

/// Resolve year-month fields to the ISO date of the calendar month's first
/// day (day 1 for iso8601).
pub fn resolve_calendar_year_month(
  cal: tcal.Calendar,
  f: DateFields,
  overflow: Overflow,
) -> Result(#(Int, Int, Int, tcal.Calendar), TErr) {
  use Nil <- result.try(case f.year, f.era, f.era_year {
    None, None, None -> Error(TypeE("year is required"))
    _, _, _ -> Ok(Nil)
  })
  use Nil <- result.try(case f.month, f.month_code {
    None, None -> Error(TypeE("month or monthCode is required"))
    _, _ -> Ok(Nil)
  })
  use y <- result.try(resolve_calendar_year(cal, f))
  case cal {
    tcal.Iso8601 -> {
      use m <- result.try(resolve_iso_month(f))
      use m <- result.try(case m >= 1 && m <= 12 {
        True -> Ok(m)
        False ->
          case overflow {
            Reject -> Error(RangeE("invalid month"))
            Constrain -> Ok(int.clamp(m, 1, 12))
          }
      })
      check_ym_limits(y, m, 1, cal)
    }
    _ -> {
      use m <- result.try(resolve_calendar_month(cal, y, f, overflow))
      let first =
        iso_date_from_epoch_days(tcal.date_to_epoch_days(cal, y, m, 1))
      check_ym_limits(first.year, first.month, first.day, cal)
    }
  }
}

// ============================================================================
// Getters
// ============================================================================

pub fn getter(
  st: Agent,
  g: TemporalYearMonthGetter,
  this: JsVal,
) -> #(JsVal, Agent) {
  let #(y, m, rd, cal) =
    require_temporal(
      st,
      this,
      "PlainYearMonth",
      getter_name(g),
      year_month_slot_of,
    )
  #(year_month_field_cal(cal, y, m, rd, g), st)
}

fn year_month_field(y: Int, m: Int, g: TemporalYearMonthGetter) -> JsVal {
  case g {
    YmCalendarId -> mk_string("iso8601")
    YmEra -> mk_undefined()
    YmEraYear -> mk_undefined()
    YmYear -> int_val(y)
    YmMonth -> int_val(m)
    YmMonthCode -> mk_string(month_code_str(m))
    YmDaysInYear -> int_val(days_in_iso_year(y))
    YmDaysInMonth -> int_val(days_in_month(y, m))
    YmMonthsInYear -> int_val(12)
    YmInLeapYear -> mk_bool(is_leap_year(y))
  }
}

/// Calendar-aware year-month field getter. y/m/rd are the slot's ISO date.
fn year_month_field_cal(
  cal: tcal.Calendar,
  y: Int,
  m: Int,
  rd: Int,
  g: TemporalYearMonthGetter,
) -> JsVal {
  case cal {
    tcal.Iso8601 -> year_month_field(y, m, g)
    _ -> {
      let cd = tcal.date_from_epoch_days(cal, epoch_days(IsoDate(y, m, rd)))
      case g {
        YmCalendarId -> mk_string(tcal.identifier(cal))
        YmEra -> era_field(cal, cd)
        YmEraYear -> era_year_field(cal, cd)
        YmYear -> int_val(cd.year)
        YmMonth -> int_val(cd.month)
        YmMonthCode -> mk_string(tcal.month_code(cal, cd.year, cd.month))
        YmDaysInYear -> int_val(tcal.days_in_year(cal, cd.year))
        YmDaysInMonth -> int_val(tcal.days_in_month(cal, cd.year, cd.month))
        YmMonthsInYear -> int_val(tcal.months_in_year(cal, cd.year))
        YmInLeapYear -> mk_bool(tcal.in_leap_year(cal, cd.year))
      }
    }
  }
}

// ============================================================================
// Methods
// ============================================================================

pub fn method(
  st: Agent,
  meth: PlainYearMonthMethod,
  protos: TemporalProtos,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(y, m, rd, cal) =
    require_temporal(
      st,
      this,
      "PlainYearMonth",
      method_name(meth),
      year_month_slot_of,
    )
  case meth {
    PymToJson | PymToLocaleString -> #(
      mk_string(format_ym_cal(y, m, rd, cal, CalAuto)),
      st,
    )
    PymToString -> {
      let #(#(cal_name, _), st) =
        get_calendar_name_option(st, helpers.arg_at(args, 0))
      #(mk_string(format_ym_cal(y, m, rd, cal, cal_name)), st)
    }
    PymValueOf ->
      rt_val.t_throw_type_error(
        st,
        "Temporal.PlainYearMonth cannot be converted with valueOf",
      )
    PymEquals -> {
      let #(other, st) =
        to_temporal_year_month(st, helpers.arg_at(args, 0), mk_undefined())
      #(mk_bool(#(y, m, rd, cal) == other), st)
    }
    PymAdd | PymSubtract -> add_subtract(st, protos, y, m, rd, cal, args, meth)
    PymWith -> with(st, protos, y, m, rd, cal, args)
    PymToPlainDate -> to_plain_date(st, protos, y, m, rd, cal, args)
    PymUntil | PymSince -> {
      let #(other, st) =
        to_temporal_year_month(st, helpers.arg_at(args, 0), mk_undefined())
      case other.3 == cal {
        False ->
          rt_val.t_throw_range_error(
            st,
            "cannot compute difference between dates of different calendars",
          )
        True ->
          year_month_until_since(
            st,
            protos,
            cal,
            #(y, m, rd),
            #(other.0, other.1, other.2),
            args,
            meth == PymSince,
          )
      }
    }
  }
}

/// Temporal.PlainYearMonth.prototype.add / subtract
fn add_subtract(
  st: Agent,
  protos: TemporalProtos,
  y: Int,
  m: Int,
  rd: Int,
  cal: tcal.Calendar,
  args: List(JsVal),
  meth: PlainYearMonthMethod,
) -> #(JsVal, Agent) {
  let #(dur, overflow, st) = add_sub_args(st, args, meth == PymSubtract)
  // AddDurationToYearMonth: only years and months are allowed
  // (weeks/days/time throw RangeError); the calculation always
  // starts from day 1 of the calendar month, so day overflow never
  // occurs — `overflow` only affects month-code resolution (e.g.
  // hebrew M05L in a non-leap year).
  let has_lower_units =
    dur.weeks != 0
    || dur.days != 0
    || dur.hours != 0
    || dur.minutes != 0
    || dur.seconds != 0
    || dur.ms != 0
    || dur.us != 0
    || dur.ns != 0
  case has_lower_units {
    True ->
      rt_val.t_throw_range_error(
        st,
        "only years and months can be added to Temporal.PlainYearMonth",
      )
    False -> Nil
  }
  case cal {
    tcal.Iso8601 -> {
      // AddDurationToYearMonth steps 8-9: the intermediate date is
      // day 1 of the receiver's month and goes through
      // CalendarDateFromFields, which throws when it is outside the
      // ISO date limits (e.g. -271821-04-01, before the minimum
      // date -271821-04-19) — even for a zero duration.
      let _day1 = terr(st, check_date_limits(IsoDate(y, m, 1)))
      let #(y2, m2) = balance_year_month(y + dur.years, m + dur.months)
      case iso_year_month_within_limits(y2, m2) {
        False ->
          rt_val.t_throw_range_error(st, "year-month outside supported range")
        True -> make_year_month(st, protos, y2, m2, 1)
      }
    }
    _ -> {
      let cd = tcal.date_from_epoch_days(cal, epoch_days(IsoDate(y, m, rd)))
      let start =
        iso_date_from_epoch_days(tcal.date_to_epoch_days(
          cal,
          cd.year,
          cd.month,
          1,
        ))
      // CalendarDateFromFields on the day-1 intermediate date also
      // enforces the ISO date limits for non-ISO calendars.
      let start = terr(st, check_date_limits(start))
      let d2 = terr(st, calendar_date_add(cal, start, dur, overflow))
      let cd2 = tcal.date_from_epoch_days(cal, epoch_days(d2))
      let first =
        iso_date_from_epoch_days(tcal.date_to_epoch_days(
          cal,
          cd2.year,
          cd2.month,
          1,
        ))
      case iso_year_month_within_limits(first.year, first.month) {
        False ->
          rt_val.t_throw_range_error(st, "year-month outside supported range")
        True ->
          make_year_month_cal(
            st,
            protos,
            first.year,
            first.month,
            first.day,
            cal,
          )
      }
    }
  }
}

/// Temporal.PlainYearMonth.prototype.with ( temporalYearMonthLike [, options] )
fn with(
  st: Agent,
  protos: TemporalProtos,
  y: Int,
  m: Int,
  rd: Int,
  cal: tcal.Calendar,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(bag, st) = require_partial_bag(st, helpers.arg_at(args, 0))
  let #(fields, st) = read_year_month_fields(st, bag, cal)
  let DateFields(era:, era_year:, month:, month_code:, year:, ..) = fields
  require_nonempty_fields(
    st,
    month == None
      && month_code == None
      && year == None
      && era == None
      && era_year == None,
  )
  let #(overflow, st) = validated_overflow(st, helpers.arg_at(args, 1))
  // Merge with existing calendar year/monthCode.
  let cd = tcal.date_from_epoch_days(cal, epoch_days(IsoDate(y, m, rd)))
  let f = merge_year_month_code(cal, cd, fields)
  let #(y2, m2, rd2, _) =
    terr(st, resolve_calendar_year_month(cal, f, overflow))
  make_year_month_cal(st, protos, y2, m2, rd2, cal)
}

/// Temporal.PlainYearMonth.prototype.toPlainDate ( item )
fn to_plain_date(
  st: Agent,
  protos: TemporalProtos,
  y: Int,
  m: Int,
  rd: Int,
  cal: tcal.Calendar,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case classify(helpers.arg_at(args, 0)) {
    KHandle(h) -> {
      let #(day, st) = read_pos_int_field(st, h, "day")
      case day {
        Some(dd) -> {
          let date = case cal {
            tcal.Iso8601 -> terr(st, regulate_iso_date(y, m, dd, Constrain))
            _ -> {
              let cd =
                tcal.date_from_epoch_days(cal, epoch_days(IsoDate(y, m, rd)))
              let d2 =
                terr(
                  st,
                  regulate_calendar_day(cal, cd.year, cd.month, dd, Constrain),
                )
              iso_date_from_epoch_days(tcal.date_to_epoch_days(
                cal,
                cd.year,
                cd.month,
                d2,
              ))
            }
          }
          let date = terr(st, check_date_limits(date))
          make_date_cal(st, protos, date, cal)
        }
        None -> rt_val.t_throw_type_error(st, "day is required")
      }
    }
    _ -> rt_val.t_throw_type_error(st, "argument must be an object")
  }
}

/// Format a PlainYearMonth: non-ISO calendars always include the reference
/// day and the calendar annotation.
fn format_ym_cal(
  y: Int,
  m: Int,
  rd: Int,
  cal: tcal.Calendar,
  mode: CalendarNameMode,
) -> String {
  case cal {
    tcal.Iso8601 ->
      case mode {
        CalAlways | CalCritical ->
          format_iso_date(IsoDate(y, m, rd)) <> calendar_suffix(mode, cal)
        CalAuto | CalNever -> format_iso_year(y) <> "-" <> pad2(m)
      }
    _ ->
      case mode {
        CalNever -> format_iso_date(IsoDate(y, m, rd))
        CalAuto | CalAlways | CalCritical ->
          format_iso_date(IsoDate(y, m, rd)) <> calendar_suffix(mode, cal)
      }
  }
}

// ============================================================================
// until / since
// ============================================================================

fn year_month_until_since(
  st: Agent,
  protos: TemporalProtos,
  cal: tcal.Calendar,
  a: #(Int, Int, Int),
  b: #(Int, Int, Int),
  args: List(JsVal),
  is_since: Bool,
) -> #(JsVal, Agent) {
  let #(#(largest, smallest, inc, mode), st) = get_difference_settings(st, args)
  let smallest = option.unwrap(smallest, Month)
  let largest = option.unwrap(largest, max_unit(smallest, Year))
  case unit_rank(smallest) < unit_rank(Month) {
    True -> rt_val.t_throw_range_error(st, "smallestUnit must be year or month")
    False -> Nil
  }
  require_largest_ge_smallest(st, largest, smallest)
  let mode2 = apply_since_mode(mode, is_since)
  let ia = IsoDate(a.0, a.1, a.2)
  let ib = IsoDate(b.0, b.1, b.2)
  let total_months = case cal {
    tcal.Iso8601 -> { b.0 - a.0 } * 12 + b.1 - a.1
    _ -> {
      // Count whole calendar months between the two month-firsts.
      let #(_, months, _) = calendar_date_until(cal, ia, ib, whole_years: False)
      months
    }
  }
  let rounded = case smallest {
    Year -> round_to_increment(total_months, inc * 12, mode2) / 12
    _ -> round_to_increment(total_months, inc, mode2)
  }
  let dur = case cal {
    tcal.Iso8601 ->
      case smallest, largest {
        Year, _ -> DurRec(..zero_dur, years: rounded)
        _, Year ->
          DurRec(
            ..zero_dur,
            years: trunc_div(rounded, 12),
            months: trunc_mod(rounded, 12),
          )
        _, _ -> DurRec(..zero_dur, months: rounded)
      }
    _ ->
      // Calendar-space years/months decomposition. RoundRelativeDuration
      // is calendar-agnostic, so roundingMode/roundingIncrement apply
      // here too: years are nudged against real calendar-year
      // boundaries, and a rounded month total is re-decomposed by
      // stepping calendar years (not recomputed unrounded).
      case smallest, largest {
        Year, _ -> {
          let yrs = terr(st, round_calendar_year_total(cal, ia, ib, inc, mode2))
          DurRec(..zero_dur, years: yrs)
        }
        _, Year -> {
          let mid =
            terr(
              st,
              calendar_date_add(
                cal,
                ia,
                DurRec(..zero_dur, months: rounded),
                Constrain,
              ),
            )
          let #(yrs, mos, _) =
            calendar_date_until(cal, ia, mid, whole_years: True)
          DurRec(..zero_dur, years: yrs, months: mos)
        }
        _, _ -> DurRec(..zero_dur, months: rounded)
      }
  }
  let dur = apply_since_dur(dur, is_since)
  make_duration(st, protos, dur)
}

/// NudgeToCalendarUnit for a PlainYearMonth difference in a non-ISO
/// calendar: round the whole-year count of ib − ia (both ISO dates of
/// calendar day 1) to `inc`-year multiples per `mode`. The fractional year
/// is measured as day progress between the bounding calendar-year marks
/// (start = ia + r1 years, end = ia + r2 years), like the spec's
/// epoch-nanosecond progress.
fn round_calendar_year_total(
  cal: tcal.Calendar,
  ia: IsoDate,
  ib: IsoDate,
  inc: Int,
  mode: RoundingMode,
) -> Result(Int, TErr) {
  let dest = epoch_days(ib)
  let sign = case dest < epoch_days(ia) {
    True -> -1
    False -> 1
  }
  let #(yrs, _, _) = calendar_date_until(cal, ia, ib, whole_years: True)
  let r1 = trunc_div(yrs, inc) * inc
  let r2 = r1 + inc * sign
  use start <- result.try(calendar_date_add(
    cal,
    ia,
    DurRec(..zero_dur, years: r1),
    Constrain,
  ))
  use end_date <- result.map(calendar_date_add(
    cal,
    ia,
    DurRec(..zero_dur, years: r2),
    Constrain,
  ))
  let num = dest - epoch_days(start)
  let den = epoch_days(end_date) - epoch_days(start)
  round_between(
    int.absolute_value(r1),
    int.absolute_value(r2),
    num,
    den,
    inc,
    mode,
    sign,
  )
  * sign
}
