import arc/bytecode/error_kind.{type JsError, JsError, RangeError, TypeError}
import arc/internal/gregorian.{
  days_in_month, days_in_year as days_in_iso_year, is_leap_year,
}
import arc/internal/int_math.{trunc_div, trunc_mod}
import arc/internal/temporal_calendar
import arc/rt/builtins/helpers
import arc/rt/builtins/options.{get_options_object}
import arc/rt/builtins/temporal_common.{
  make_date_cal, make_duration, make_year_month, make_year_month_cal,
  read_pos_int_field, require_temporal, static_name, truncated_int_arg,
  truncated_int_arg_or, year_month_slot_of,
}
import arc/rt/builtins/temporal_fields.{
  type DateFields, DateFields, add_sub_args, balance_year_month,
  calendar_date_add, calendar_years_months_until, check_ym_limits,
  compare_iso_date, era_field, era_year_field, merge_year_month_code,
  month_code_text, parse_year_month_string, read_bag_calendar,
  read_year_month_fields, regulate_calendar_day, require_nonempty_fields,
  require_partial_bag, resolve_calendar_month, resolve_calendar_year,
  resolve_iso_month, round_between, to_calendar_arg,
}
import arc/rt/builtins/temporal_iso.{
  type IsoDate, type IsoDateSlots, type Overflow, Constrain, Duration, IsoDate,
  IsoDateSlots, Reject, check_date_limits, epoch_days, format_iso_year,
  is_valid_iso_date, iso_date_from_epoch_days, iso_year_month_within_limits,
  pad2, regulate_iso_date, zero_duration,
}
import arc/rt/builtins/temporal_options.{
  type CalendarNameMode, CalendarNameAuto, format_with_reference,
  get_calendar_name_option, get_overflow_option_from_value,
}
import arc/rt/builtins/temporal_rounding.{
  type RoundingMode, Month, Year, apply_since_duration, apply_since_mode,
  get_difference_settings, max_unit, require_largest_ge_smallest,
  round_to_increment, unit_rank,
}
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type JsVal, type NativeToken, type PlainYearMonthMethod,
  type TemporalProtos, type TemporalStaticName, type TemporalYearMonthGetter,
  CompareStatic, FromStatic, KHandle, KStr, PlainYearMonthAdd,
  PlainYearMonthEquals, PlainYearMonthSince, PlainYearMonthSubtract,
  PlainYearMonthToJson, PlainYearMonthToLocaleString, PlainYearMonthToPlainDate,
  PlainYearMonthToString, PlainYearMonthUntil, PlainYearMonthValueOf,
  PlainYearMonthWith, SObject, TemporalN, TemporalPlainYearMonthCtor,
  TemporalPlainYearMonthGetter, TemporalPlainYearMonthMethod,
  TemporalPlainYearMonthStatic, YearMonthCalendarId, YearMonthDaysInMonth,
  YearMonthDaysInYear, YearMonthEra, YearMonthEraYear, YearMonthInLeapYear,
  YearMonthMonth, YearMonthMonthCode, YearMonthMonthsInYear, YearMonthYear,
  classify, mk_bool, mk_int, mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result

const all_getters = [
  YearMonthCalendarId,
  YearMonthEra,
  YearMonthEraYear,
  YearMonthYear,
  YearMonthMonth,
  YearMonthMonthCode,
  YearMonthDaysInYear,
  YearMonthDaysInMonth,
  YearMonthMonthsInYear,
  YearMonthInLeapYear,
]

const all_methods = [
  #(PlainYearMonthWith, 1),
  #(PlainYearMonthAdd, 1),
  #(PlainYearMonthSubtract, 1),
  #(PlainYearMonthUntil, 1),
  #(PlainYearMonthSince, 1),
  #(PlainYearMonthEquals, 1),
  #(PlainYearMonthToString, 0),
  #(PlainYearMonthToLocaleString, 0),
  #(PlainYearMonthToJson, 0),
  #(PlainYearMonthValueOf, 0),
  #(PlainYearMonthToPlainDate, 1),
]

pub fn ctor_token(protos: TemporalProtos) -> NativeToken {
  TemporalN(TemporalPlainYearMonthCtor(protos:))
}

pub fn statics(protos: TemporalProtos) -> List(#(String, NativeToken, Int)) {
  list.map([#(FromStatic, 1), #(CompareStatic, 2)], fn(s) {
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

pub fn getter_name(g: TemporalYearMonthGetter) -> String {
  case g {
    YearMonthCalendarId -> "calendarId"
    YearMonthEra -> "era"
    YearMonthEraYear -> "eraYear"
    YearMonthYear -> "year"
    YearMonthMonth -> "month"
    YearMonthMonthCode -> "monthCode"
    YearMonthDaysInYear -> "daysInYear"
    YearMonthDaysInMonth -> "daysInMonth"
    YearMonthMonthsInYear -> "monthsInYear"
    YearMonthInLeapYear -> "inLeapYear"
  }
}

pub fn method_name(m: PlainYearMonthMethod) -> String {
  case m {
    PlainYearMonthWith -> "with"
    PlainYearMonthAdd -> "add"
    PlainYearMonthSubtract -> "subtract"
    PlainYearMonthUntil -> "until"
    PlainYearMonthSince -> "since"
    PlainYearMonthEquals -> "equals"
    PlainYearMonthToString -> "toString"
    PlainYearMonthToLocaleString -> "toLocaleString"
    PlainYearMonthToJson -> "toJSON"
    PlainYearMonthValueOf -> "valueOf"
    PlainYearMonthToPlainDate -> "toPlainDate"
  }
}

pub fn ctor(
  st: Agent,
  protos: TemporalProtos,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(y, st) = truncated_int_arg(st, args, 0)
  let #(m, st) = truncated_int_arg(st, args, 1)
  let cal = rt_val.or_throw(st, to_calendar_arg(helpers.arg_at(args, 2)))
  let #(d, st) = truncated_int_arg_or(st, args, 3, 1)
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

pub fn static(
  st: Agent,
  name: TemporalStaticName,
  protos: TemporalProtos,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case name {
    FromStatic -> {
      let #(IsoDateSlots(IsoDate(y, m, rd), cal), st) =
        to_temporal_year_month(
          st,
          helpers.arg_at(args, 0),
          helpers.arg_at(args, 1),
        )
      make_year_month_cal(st, protos, y, m, rd, cal)
    }
    CompareStatic -> {
      let #(a, st) =
        to_temporal_year_month(st, helpers.arg_at(args, 0), mk_undefined())
      let #(b, st) =
        to_temporal_year_month(st, helpers.arg_at(args, 1), mk_undefined())
      let n = compare_iso_date(a.iso_date, b.iso_date)
      #(mk_int(n), st)
    }
  }
}

pub fn to_temporal_year_month(
  st: Agent,
  item: JsVal,
  options: JsVal,
) -> #(IsoDateSlots, Agent) {
  case classify(item) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind:, ..) ->
          case year_month_slot_of(kind) {
            Some(ym) -> {
              let #(_o, st) = get_overflow_option_from_value(st, options)
              #(ym, st)
            }
            None -> year_month_from_bag(st, h, options)
          }
        _ -> year_month_from_bag(st, h, options)
      }
    KStr(s) -> {
      let ym = rt_val.or_throw(st, parse_year_month_string(s))
      let #(_o, st) = get_overflow_option_from_value(st, options)
      #(ym, st)
    }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "cannot convert to a Temporal.PlainYearMonth",
      )
  }
}

fn year_month_from_bag(
  st: Agent,
  h: types.Handle,
  options: JsVal,
) -> #(IsoDateSlots, Agent) {
  let #(cal, st) = read_bag_calendar(st, h)
  let #(fields, st) = read_year_month_fields(st, h, cal)
  let #(overflow, st) = get_overflow_option_from_value(st, options)
  #(rt_val.or_throw(st, resolve_calendar_year_month(cal, fields, overflow)), st)
}

pub fn resolve_calendar_year_month(
  cal: temporal_calendar.Calendar,
  f: DateFields,
  overflow: Overflow,
) -> Result(IsoDateSlots, JsError) {
  use Nil <- result.try(case f.year, f.era, f.era_year {
    None, None, None -> Error(JsError(TypeError, "year is required"))
    _, _, _ -> Ok(Nil)
  })
  use Nil <- result.try(case f.month, f.month_code {
    None, None -> Error(JsError(TypeError, "month or monthCode is required"))
    _, _ -> Ok(Nil)
  })
  use y <- result.try(resolve_calendar_year(cal, f))
  case cal {
    temporal_calendar.Iso8601 -> {
      use m <- result.try(resolve_iso_month(f))
      use m <- result.try(case m >= 1 && m <= 12 {
        True -> Ok(m)
        False ->
          case overflow {
            Reject -> Error(JsError(RangeError, "invalid month"))
            Constrain -> Ok(int.clamp(m, 1, 12))
          }
      })
      check_ym_limits(y, m, 1, cal)
    }
    _ -> {
      use m <- result.try(resolve_calendar_month(cal, y, f, overflow))
      let first =
        iso_date_from_epoch_days(temporal_calendar.date_to_epoch_days(
          cal,
          y,
          m,
          1,
        ))
      check_ym_limits(first.year, first.month, first.day, cal)
    }
  }
}

pub fn getter(
  st: Agent,
  g: TemporalYearMonthGetter,
  this: JsVal,
) -> #(JsVal, Agent) {
  let IsoDateSlots(IsoDate(y, m, rd), cal) =
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
    YearMonthCalendarId -> mk_string("iso8601")
    YearMonthEra -> mk_undefined()
    YearMonthEraYear -> mk_undefined()
    YearMonthYear -> mk_int(y)
    YearMonthMonth -> mk_int(m)
    YearMonthMonthCode -> mk_string(month_code_text(m))
    YearMonthDaysInYear -> mk_int(days_in_iso_year(y))
    YearMonthDaysInMonth -> mk_int(days_in_month(y, m))
    YearMonthMonthsInYear -> mk_int(12)
    YearMonthInLeapYear -> mk_bool(is_leap_year(y))
  }
}

fn year_month_field_cal(
  cal: temporal_calendar.Calendar,
  y: Int,
  m: Int,
  rd: Int,
  g: TemporalYearMonthGetter,
) -> JsVal {
  case cal {
    temporal_calendar.Iso8601 -> year_month_field(y, m, g)
    _ -> {
      let cd =
        temporal_calendar.date_from_epoch_days(
          cal,
          epoch_days(IsoDate(y, m, rd)),
        )
      case g {
        YearMonthCalendarId -> mk_string(temporal_calendar.identifier(cal))
        YearMonthEra -> era_field(cal, cd)
        YearMonthEraYear -> era_year_field(cal, cd)
        YearMonthYear -> mk_int(cd.year)
        YearMonthMonth -> mk_int(cd.month)
        YearMonthMonthCode ->
          mk_string(temporal_calendar.month_code(cal, cd.year, cd.month))
        YearMonthDaysInYear ->
          mk_int(temporal_calendar.days_in_year(cal, cd.year))
        YearMonthDaysInMonth ->
          mk_int(temporal_calendar.days_in_month(cal, cd.year, cd.month))
        YearMonthMonthsInYear ->
          mk_int(temporal_calendar.months_in_year(cal, cd.year))
        YearMonthInLeapYear ->
          mk_bool(temporal_calendar.in_leap_year(cal, cd.year))
      }
    }
  }
}

pub fn method(
  st: Agent,
  meth: PlainYearMonthMethod,
  protos: TemporalProtos,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let IsoDateSlots(IsoDate(y, m, rd), cal) =
    require_temporal(
      st,
      this,
      "PlainYearMonth",
      method_name(meth),
      year_month_slot_of,
    )
  case meth {
    PlainYearMonthToJson | PlainYearMonthToLocaleString -> #(
      mk_string(format_ym_cal(y, m, rd, cal, CalendarNameAuto)),
      st,
    )
    PlainYearMonthToString -> {
      let #(opts, st) = get_options_object(st, helpers.arg_at(args, 0))
      let #(cal_name, st) = get_calendar_name_option(st, opts)
      #(mk_string(format_ym_cal(y, m, rd, cal, cal_name)), st)
    }
    PlainYearMonthValueOf ->
      rt_val.t_throw_type_error(
        st,
        "Temporal.PlainYearMonth cannot be converted with valueOf",
      )
    PlainYearMonthEquals -> {
      let #(other, st) =
        to_temporal_year_month(st, helpers.arg_at(args, 0), mk_undefined())
      #(mk_bool(IsoDateSlots(IsoDate(y, m, rd), cal) == other), st)
    }
    PlainYearMonthAdd | PlainYearMonthSubtract ->
      add_subtract(st, protos, y, m, rd, cal, args, meth)
    PlainYearMonthWith -> with(st, protos, y, m, rd, cal, args)
    PlainYearMonthToPlainDate -> to_plain_date(st, protos, y, m, rd, cal, args)
    PlainYearMonthUntil | PlainYearMonthSince -> {
      let #(other, st) =
        to_temporal_year_month(st, helpers.arg_at(args, 0), mk_undefined())
      case other.calendar == cal {
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
            IsoDate(y, m, rd),
            other.iso_date,
            args,
            meth == PlainYearMonthSince,
          )
      }
    }
  }
}

fn add_subtract(
  st: Agent,
  protos: TemporalProtos,
  y: Int,
  m: Int,
  rd: Int,
  cal: temporal_calendar.Calendar,
  args: List(JsVal),
  meth: PlainYearMonthMethod,
) -> #(JsVal, Agent) {
  let #(dur, overflow, st) =
    add_sub_args(st, args, meth == PlainYearMonthSubtract)
  let has_lower_units =
    dur.weeks != 0
    || dur.days != 0
    || dur.hours != 0
    || dur.minutes != 0
    || dur.seconds != 0
    || dur.milliseconds != 0
    || dur.microseconds != 0
    || dur.nanoseconds != 0
  let Nil = case has_lower_units {
    True ->
      rt_val.t_throw_range_error(
        st,
        "only years and months can be added to Temporal.PlainYearMonth",
      )
    False -> Nil
  }
  case cal {
    temporal_calendar.Iso8601 -> {
      // day-1 intermediate must be within iso limits, even for zero duration
      let _day1 = rt_val.or_throw(st, check_date_limits(IsoDate(y, m, 1)))
      let #(y2, m2) = balance_year_month(y + dur.years, m + dur.months)
      case iso_year_month_within_limits(y2, m2) {
        False ->
          rt_val.t_throw_range_error(st, "year-month outside supported range")
        True -> make_year_month(st, protos, y2, m2, 1)
      }
    }
    _ -> {
      let cd =
        temporal_calendar.date_from_epoch_days(
          cal,
          epoch_days(IsoDate(y, m, rd)),
        )
      let start =
        iso_date_from_epoch_days(temporal_calendar.date_to_epoch_days(
          cal,
          cd.year,
          cd.month,
          1,
        ))
      let start = rt_val.or_throw(st, check_date_limits(start))
      let d2 = rt_val.or_throw(st, calendar_date_add(cal, start, dur, overflow))
      let cd2 = temporal_calendar.date_from_epoch_days(cal, epoch_days(d2))
      let first =
        iso_date_from_epoch_days(temporal_calendar.date_to_epoch_days(
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

fn with(
  st: Agent,
  protos: TemporalProtos,
  y: Int,
  m: Int,
  rd: Int,
  cal: temporal_calendar.Calendar,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(bag, st) = require_partial_bag(st, helpers.arg_at(args, 0))
  let #(fields, st) = read_year_month_fields(st, bag, cal)
  let DateFields(era:, era_year:, month:, month_code:, year:, ..) = fields
  let Nil =
    require_nonempty_fields(
      st,
      month == None
        && month_code == None
        && year == None
        && era == None
        && era_year == None,
    )
  let #(overflow, st) =
    get_overflow_option_from_value(st, helpers.arg_at(args, 1))
  let cd =
    temporal_calendar.date_from_epoch_days(cal, epoch_days(IsoDate(y, m, rd)))
  let f = merge_year_month_code(cal, cd, fields)
  let IsoDateSlots(IsoDate(y2, m2, rd2), _) =
    rt_val.or_throw(st, resolve_calendar_year_month(cal, f, overflow))
  make_year_month_cal(st, protos, y2, m2, rd2, cal)
}

fn to_plain_date(
  st: Agent,
  protos: TemporalProtos,
  y: Int,
  m: Int,
  rd: Int,
  cal: temporal_calendar.Calendar,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case classify(helpers.arg_at(args, 0)) {
    KHandle(h) -> {
      let #(day, st) = read_pos_int_field(st, h, "day")
      case day {
        Some(dd) -> {
          let date = case cal {
            temporal_calendar.Iso8601 ->
              rt_val.or_throw(st, regulate_iso_date(y, m, dd, Constrain))
            _ -> {
              let cd =
                temporal_calendar.date_from_epoch_days(
                  cal,
                  epoch_days(IsoDate(y, m, rd)),
                )
              let d2 =
                rt_val.or_throw(
                  st,
                  regulate_calendar_day(cal, cd.year, cd.month, dd, Constrain),
                )
              iso_date_from_epoch_days(temporal_calendar.date_to_epoch_days(
                cal,
                cd.year,
                cd.month,
                d2,
              ))
            }
          }
          let date = rt_val.or_throw(st, check_date_limits(date))
          make_date_cal(st, protos, date, cal)
        }
        None -> rt_val.t_throw_type_error(st, "day is required")
      }
    }
    _ -> rt_val.t_throw_type_error(st, "argument must be an object")
  }
}

fn format_ym_cal(
  y: Int,
  m: Int,
  rd: Int,
  cal: temporal_calendar.Calendar,
  mode: CalendarNameMode,
) -> String {
  format_with_reference(
    IsoDate(y, m, rd),
    cal,
    mode,
    short: format_iso_year(y) <> "-" <> pad2(m),
  )
}

fn year_month_until_since(
  st: Agent,
  protos: TemporalProtos,
  cal: temporal_calendar.Calendar,
  ia: IsoDate,
  ib: IsoDate,
  args: List(JsVal),
  is_since is_since: Bool,
) -> #(JsVal, Agent) {
  let #(#(largest, smallest, inc, mode), st) = get_difference_settings(st, args)
  let smallest = option.unwrap(smallest, Month)
  let largest = option.unwrap(largest, max_unit(smallest, Year))
  let Nil = case unit_rank(smallest) < unit_rank(Month) {
    True -> rt_val.t_throw_range_error(st, "smallestUnit must be year or month")
    False -> Nil
  }
  let Nil = require_largest_ge_smallest(st, largest, smallest)
  let mode = apply_since_mode(mode, is_since)
  let total_months = case cal {
    temporal_calendar.Iso8601 ->
      { ib.year - ia.year } * 12 + ib.month - ia.month
    _ -> {
      let #(_, months, _) =
        calendar_years_months_until(cal, ia, ib, whole_years: False)
      months
    }
  }
  let rounded = case smallest {
    Year -> round_to_increment(total_months, inc * 12, mode) / 12
    _ -> round_to_increment(total_months, inc, mode)
  }
  let dur = case cal {
    temporal_calendar.Iso8601 ->
      case smallest, largest {
        Year, _ -> Duration(..zero_duration, years: rounded)
        _, Year ->
          Duration(
            ..zero_duration,
            years: trunc_div(rounded, 12),
            months: trunc_mod(rounded, 12),
          )
        _, _ -> Duration(..zero_duration, months: rounded)
      }
    _ ->
      case smallest, largest {
        Year, _ -> {
          let yrs =
            rt_val.or_throw(
              st,
              round_calendar_year_total(cal, ia, ib, inc, mode),
            )
          Duration(..zero_duration, years: yrs)
        }
        _, Year -> {
          let mid =
            rt_val.or_throw(
              st,
              calendar_date_add(
                cal,
                ia,
                Duration(..zero_duration, months: rounded),
                Constrain,
              ),
            )
          let #(yrs, mos, _) =
            calendar_years_months_until(cal, ia, mid, whole_years: True)
          Duration(..zero_duration, years: yrs, months: mos)
        }
        _, _ -> Duration(..zero_duration, months: rounded)
      }
  }
  let dur = apply_since_duration(dur, is_since)
  make_duration(st, protos, dur)
}

// round year count of ib - ia by day progress between year marks
fn round_calendar_year_total(
  cal: temporal_calendar.Calendar,
  ia: IsoDate,
  ib: IsoDate,
  inc: Int,
  mode: RoundingMode,
) -> Result(Int, JsError) {
  let dest = epoch_days(ib)
  let sign = case dest < epoch_days(ia) {
    True -> -1
    False -> 1
  }
  let #(yrs, _, _) = calendar_years_months_until(cal, ia, ib, whole_years: True)
  let r1 = trunc_div(yrs, inc) * inc
  let r2 = r1 + inc * sign
  use start <- result.try(calendar_date_add(
    cal,
    ia,
    Duration(..zero_duration, years: r1),
    Constrain,
  ))
  use end_date <- result.map(calendar_date_add(
    cal,
    ia,
    Duration(..zero_duration, years: r2),
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
