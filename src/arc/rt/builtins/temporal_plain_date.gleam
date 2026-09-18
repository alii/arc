import arc/internal/gregorian.{
  days_in_month, days_in_year as days_in_iso_year, is_leap_year,
}
import arc/internal/temporal_calendar
import arc/rt/builtins/helpers
import arc/rt/builtins/options.{get_options_object}
import arc/rt/builtins/temporal_common.{
  date_slot_of, make_date_cal, make_date_time_cal, make_duration,
  make_month_day_cal, make_year_month_cal, make_zoned_cal, require_temporal,
  static_name, temporal_data_of, truncated_int_arg,
}
import arc/rt/builtins/temporal_diff.{difference_calendar_date}
import arc/rt/builtins/temporal_fields.{
  add_sub_args, calendar_date_add, calendar_with_fields, compare_iso_date,
  era_field, era_year_field, get_named, month_code_text, month_day_reference_iso,
  no_date_fields, parse_plain_datetime_string, parsed_calendar_id,
  read_bag_calendar, read_date_fields, require_nonempty_fields,
  require_partial_bag, resolve_calendar_date, to_calendar_arg,
  to_temporal_calendar_identifier,
}
import arc/rt/builtins/temporal_iso.{
  type IsoDate, type IsoTime, Constrain, IsoDate, check_date_limits, day_of_week,
  day_of_year, epoch_days, format_iso_date, is_valid_iso_date,
  iso_date_from_epoch_days, midnight, week_of_year,
}
import arc/rt/builtins/temporal_options.{
  CalendarNameAuto, Compatible, calendar_suffix, get_calendar_name_option,
  get_overflow_option_from_value,
}
import arc/rt/builtins/temporal_plain_time.{to_temporal_time}
import arc/rt/builtins/temporal_rounding.{
  Day, apply_since_duration, apply_since_mode, get_difference_settings, max_unit,
  require_largest_ge_smallest, unit_rank,
}
import arc/rt/builtins/temporal_time_zone.{
  epoch_ns_to_iso_in, time_zone_from_string,
}
import arc/rt/builtins/temporal_zoned_ops.{get_epoch_ns_for, start_of_day_ns}
import arc/rt/temporal_data.{
  TemporalDate, TemporalDateTime, TemporalZonedDateTime,
}
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type NativeToken, type PlainDateMethod,
  type TemporalDateGetter, type TemporalProtos, type TemporalStaticName,
  CompareStatic, DateCalendarId, DateDay, DateDayOfWeek, DateDayOfYear,
  DateDaysInMonth, DateDaysInWeek, DateDaysInYear, DateEra, DateEraYear,
  DateInLeapYear, DateMonth, DateMonthCode, DateMonthsInYear, DateWeekOfYear,
  DateYear, DateYearOfWeek, FromStatic, KHandle, KStr, KUndef, PlainDateAdd,
  PlainDateEquals, PlainDateSince, PlainDateSubtract, PlainDateToJson,
  PlainDateToLocaleString, PlainDateToPlainDateTime, PlainDateToPlainMonthDay,
  PlainDateToPlainYearMonth, PlainDateToString, PlainDateToZonedDateTime,
  PlainDateUntil, PlainDateValueOf, PlainDateWith, PlainDateWithCalendar,
  TemporalN, TemporalPlainDateCtor, TemporalPlainDateGetter,
  TemporalPlainDateMethod, TemporalPlainDateStatic, classify, mk_bool, mk_int,
  mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/list
import gleam/option

pub const all_date_getters = [
  DateCalendarId,
  DateEra,
  DateEraYear,
  DateYear,
  DateMonth,
  DateMonthCode,
  DateDay,
  DateDayOfWeek,
  DateDayOfYear,
  DateWeekOfYear,
  DateYearOfWeek,
  DateDaysInWeek,
  DateDaysInMonth,
  DateDaysInYear,
  DateMonthsInYear,
  DateInLeapYear,
]

pub fn ctor_token(protos: TemporalProtos) -> NativeToken {
  TemporalN(TemporalPlainDateCtor(protos:))
}

pub fn statics(protos: TemporalProtos) -> List(#(String, NativeToken, Int)) {
  list.map([#(FromStatic, 1), #(CompareStatic, 2)], fn(s) {
    #(static_name(s.0), TemporalN(TemporalPlainDateStatic(s.0, protos)), s.1)
  })
}

pub fn getters() -> List(#(String, NativeToken)) {
  list.map(all_date_getters, fn(g) {
    #(date_getter_name(g), TemporalN(TemporalPlainDateGetter(g)))
  })
}

pub fn methods(protos: TemporalProtos) -> List(#(String, NativeToken, Int)) {
  list.map(
    [
      #(PlainDateToPlainYearMonth, 0),
      #(PlainDateToPlainMonthDay, 0),
      #(PlainDateToPlainDateTime, 0),
      #(PlainDateToZonedDateTime, 1),
      #(PlainDateAdd, 1),
      #(PlainDateSubtract, 1),
      #(PlainDateWith, 1),
      #(PlainDateWithCalendar, 1),
      #(PlainDateUntil, 1),
      #(PlainDateSince, 1),
      #(PlainDateEquals, 1),
      #(PlainDateToString, 0),
      #(PlainDateToLocaleString, 0),
      #(PlainDateToJson, 0),
      #(PlainDateValueOf, 0),
    ],
    fn(m) {
      #(
        plain_date_method_name(m.0),
        TemporalN(TemporalPlainDateMethod(m.0, protos)),
        m.1,
      )
    },
  )
}

pub fn date_getter_name(g: TemporalDateGetter) -> String {
  case g {
    DateCalendarId -> "calendarId"
    DateEra -> "era"
    DateEraYear -> "eraYear"
    DateYear -> "year"
    DateMonth -> "month"
    DateMonthCode -> "monthCode"
    DateDay -> "day"
    DateDayOfWeek -> "dayOfWeek"
    DateDayOfYear -> "dayOfYear"
    DateWeekOfYear -> "weekOfYear"
    DateYearOfWeek -> "yearOfWeek"
    DateDaysInWeek -> "daysInWeek"
    DateDaysInMonth -> "daysInMonth"
    DateDaysInYear -> "daysInYear"
    DateMonthsInYear -> "monthsInYear"
    DateInLeapYear -> "inLeapYear"
  }
}

pub fn plain_date_method_name(m: PlainDateMethod) -> String {
  case m {
    PlainDateToPlainYearMonth -> "toPlainYearMonth"
    PlainDateToPlainMonthDay -> "toPlainMonthDay"
    PlainDateToPlainDateTime -> "toPlainDateTime"
    PlainDateToZonedDateTime -> "toZonedDateTime"
    PlainDateAdd -> "add"
    PlainDateSubtract -> "subtract"
    PlainDateWith -> "with"
    PlainDateWithCalendar -> "withCalendar"
    PlainDateUntil -> "until"
    PlainDateSince -> "since"
    PlainDateEquals -> "equals"
    PlainDateToString -> "toString"
    PlainDateToLocaleString -> "toLocaleString"
    PlainDateToJson -> "toJSON"
    PlainDateValueOf -> "valueOf"
  }
}

pub fn ctor(
  st: Agent,
  protos: TemporalProtos,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(y, st) = truncated_int_arg(st, args, 0)
  let #(m, st) = truncated_int_arg(st, args, 1)
  let #(d, st) = truncated_int_arg(st, args, 2)
  let cal = rt_val.or_throw(st, to_calendar_arg(helpers.arg_at(args, 3)))
  case is_valid_iso_date(y, m, d) {
    False -> rt_val.t_throw_range_error(st, "invalid ISO date")
    True -> {
      let date = rt_val.or_throw(st, check_date_limits(IsoDate(y, m, d)))
      make_date_cal(st, protos, date, cal)
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
      let #(#(d, cal), st) =
        to_temporal_date(st, helpers.arg_at(args, 0), helpers.arg_at(args, 1))
      make_date_cal(st, protos, d, cal)
    }
    CompareStatic -> {
      let #(#(a, _), st) =
        to_temporal_date(st, helpers.arg_at(args, 0), mk_undefined())
      let #(#(b, _), st) =
        to_temporal_date(st, helpers.arg_at(args, 1), mk_undefined())
      #(mk_int(compare_iso_date(a, b)), st)
    }
  }
}

pub fn to_temporal_date(
  st: Agent,
  item: JsVal,
  options: JsVal,
) -> #(#(IsoDate, temporal_calendar.Calendar), Agent) {
  case classify(item) {
    KHandle(h) ->
      case temporal_data_of(st, item) {
        option.Some(TemporalDate(year:, month:, day:, calendar:))
        | option.Some(TemporalDateTime(year:, month:, day:, calendar:, ..)) -> {
          let #(_opts, st) = get_overflow_option_from_value(st, options)
          #(#(IsoDate(year, month, day), calendar), st)
        }
        option.Some(TemporalZonedDateTime(epoch_ns:, time_zone:, calendar:)) -> {
          let #(_opts, st) = get_overflow_option_from_value(st, options)
          let #(d, _) = epoch_ns_to_iso_in(time_zone, epoch_ns)
          #(#(d, calendar), st)
        }
        _ -> date_from_bag(st, h, options)
      }
    KStr(s) -> {
      let p = rt_val.or_throw(st, parse_plain_datetime_string(s))
      let cal = rt_val.or_throw(st, parsed_calendar_id(p))
      let #(_opts, st) = get_overflow_option_from_value(st, options)
      #(#(rt_val.or_throw(st, check_date_limits(p.date)), cal), st)
    }
    _ -> rt_val.t_throw_type_error(st, "cannot convert to a Temporal.PlainDate")
  }
}

pub fn date_from_bag(
  st: Agent,
  h: Handle,
  options: JsVal,
) -> #(#(IsoDate, temporal_calendar.Calendar), Agent) {
  let #(cal, st) = read_bag_calendar(st, h)
  let #(fields, st) = read_date_fields(st, h, cal)
  let #(overflow, st) = get_overflow_option_from_value(st, options)
  let date = rt_val.or_throw(st, resolve_calendar_date(cal, fields, overflow))
  #(#(rt_val.or_throw(st, check_date_limits(date)), cal), st)
}

pub fn getter(
  st: Agent,
  g: TemporalDateGetter,
  this: JsVal,
) -> #(JsVal, Agent) {
  let #(d, cal) =
    require_temporal(st, this, "PlainDate", date_getter_name(g), date_slot_of)
  #(date_field_cal(cal, d, g), st)
}

pub fn date_field(d: IsoDate, g: TemporalDateGetter) -> JsVal {
  case g {
    DateCalendarId -> mk_string("iso8601")
    DateEra -> mk_undefined()
    DateEraYear -> mk_undefined()
    DateYear -> mk_int(d.year)
    DateMonth -> mk_int(d.month)
    DateMonthCode -> mk_string(month_code_text(d.month))
    DateDay -> mk_int(d.day)
    DateDayOfWeek -> mk_int(day_of_week(d))
    DateDayOfYear -> mk_int(day_of_year(d))
    DateWeekOfYear -> mk_int(week_of_year(d).0)
    DateYearOfWeek -> mk_int(week_of_year(d).1)
    DateDaysInWeek -> mk_int(7)
    DateDaysInMonth -> mk_int(days_in_month(d.year, d.month))
    DateDaysInYear -> mk_int(days_in_iso_year(d.year))
    DateMonthsInYear -> mk_int(12)
    DateInLeapYear -> mk_bool(is_leap_year(d.year))
  }
}

pub fn date_field_cal(
  cal: temporal_calendar.Calendar,
  d: IsoDate,
  g: TemporalDateGetter,
) -> JsVal {
  case cal {
    temporal_calendar.Iso8601 -> date_field(d, g)
    _ -> {
      let cd = temporal_calendar.date_from_epoch_days(cal, epoch_days(d))
      case g {
        DateCalendarId -> mk_string(temporal_calendar.identifier(cal))
        DateEra -> era_field(cal, cd)
        DateEraYear -> era_year_field(cal, cd)
        DateYear -> mk_int(cd.year)
        DateMonth -> mk_int(cd.month)
        DateMonthCode ->
          mk_string(temporal_calendar.month_code(cal, cd.year, cd.month))
        DateDay -> mk_int(cd.day)
        DateDayOfWeek -> mk_int(day_of_week(d))
        DateDayOfYear ->
          mk_int(temporal_calendar.day_of_year(cal, cd.year, cd.month, cd.day))
        DateWeekOfYear -> mk_undefined()
        DateYearOfWeek -> mk_undefined()
        DateDaysInWeek -> mk_int(7)
        DateDaysInMonth ->
          mk_int(temporal_calendar.days_in_month(cal, cd.year, cd.month))
        DateDaysInYear -> mk_int(temporal_calendar.days_in_year(cal, cd.year))
        DateMonthsInYear ->
          mk_int(temporal_calendar.months_in_year(cal, cd.year))
        DateInLeapYear -> mk_bool(temporal_calendar.in_leap_year(cal, cd.year))
      }
    }
  }
}

pub fn method(
  st: Agent,
  m: PlainDateMethod,
  protos: TemporalProtos,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(d, cal) =
    require_temporal(
      st,
      this,
      "PlainDate",
      plain_date_method_name(m),
      date_slot_of,
    )
  case m {
    PlainDateToJson | PlainDateToLocaleString -> #(
      mk_string(format_iso_date(d) <> calendar_suffix(CalendarNameAuto, cal)),
      st,
    )
    PlainDateToString -> {
      let #(opts, st) = get_options_object(st, helpers.arg_at(args, 0))
      let #(cal_name, st) = get_calendar_name_option(st, opts)
      #(mk_string(format_iso_date(d) <> calendar_suffix(cal_name, cal)), st)
    }
    PlainDateValueOf ->
      rt_val.t_throw_type_error(
        st,
        "Temporal.PlainDate cannot be converted with valueOf; use compare() or equals()",
      )
    PlainDateEquals -> {
      let #(#(other, other_cal), st) =
        to_temporal_date(st, helpers.arg_at(args, 0), mk_undefined())
      #(mk_bool(d == other && cal == other_cal), st)
    }
    PlainDateAdd | PlainDateSubtract -> {
      let #(dur, overflow, st) = add_sub_args(st, args, m == PlainDateSubtract)
      let d2 = rt_val.or_throw(st, calendar_date_add(cal, d, dur, overflow))
      make_date_cal(st, protos, d2, cal)
    }
    PlainDateWith -> {
      let #(bag, st) = require_partial_bag(st, helpers.arg_at(args, 0))
      let #(fields, st) = read_date_fields(st, bag, cal)
      let Nil = require_nonempty_fields(st, fields == no_date_fields)
      let #(overflow, st) =
        get_overflow_option_from_value(st, helpers.arg_at(args, 1))
      let date =
        rt_val.or_throw(st, calendar_with_fields(cal, d, fields, overflow))
      let date = rt_val.or_throw(st, check_date_limits(date))
      make_date_cal(st, protos, date, cal)
    }
    PlainDateWithCalendar -> {
      let #(new_cal, st) =
        to_temporal_calendar_identifier(st, helpers.arg_at(args, 0))
      make_date_cal(st, protos, d, new_cal)
    }
    PlainDateToPlainDateTime -> {
      let #(t, st) = optional_time_arg(st, helpers.arg_at(args, 0))
      make_date_time_cal(st, protos, d, t, cal)
    }
    PlainDateToPlainYearMonth -> {
      let first = case cal {
        temporal_calendar.Iso8601 -> IsoDate(..d, day: 1)
        _ -> {
          let cd = temporal_calendar.date_from_epoch_days(cal, epoch_days(d))
          iso_date_from_epoch_days(temporal_calendar.date_to_epoch_days(
            cal,
            cd.year,
            cd.month,
            1,
          ))
        }
      }
      make_year_month_cal(st, protos, first.year, first.month, first.day, cal)
    }
    PlainDateToPlainMonthDay ->
      case cal {
        temporal_calendar.Iso8601 ->
          make_month_day_cal(st, protos, d.month, d.day, 1972, cal)
        _ -> {
          let cd = temporal_calendar.date_from_epoch_days(cal, epoch_days(d))
          let mc = temporal_calendar.month_code_of(cal, cd.year, cd.month)
          let iso =
            rt_val.or_throw(
              st,
              month_day_reference_iso(cal, mc, cd.day, Constrain),
            )
          make_month_day_cal(st, protos, iso.month, iso.day, iso.year, cal)
        }
      }
    PlainDateToZonedDateTime -> {
      let arg = helpers.arg_at(args, 0)
      let #(tz, plain_time, st) = case classify(arg) {
        KStr(tz_text) -> {
          let #(tz, st) = time_zone_from_string(st, tz_text)
          #(tz, mk_undefined(), st)
        }
        KHandle(oh) -> {
          let #(tz_val, st) = get_named(st, oh, "timeZone")
          let #(tz, st) = case classify(tz_val) {
            KUndef -> rt_val.t_throw_type_error(st, "time zone is required")
            KStr(tz_text) -> time_zone_from_string(st, tz_text)
            _ -> rt_val.t_throw_type_error(st, "time zone must be a string")
          }
          let #(plain_time, st) = get_named(st, oh, "plainTime")
          #(tz, plain_time, st)
        }
        _ -> rt_val.t_throw_type_error(st, "time zone must be a string")
      }
      let #(ns, st) = case classify(plain_time) {
        KUndef -> #(rt_val.or_throw(st, start_of_day_ns(tz, d)), st)
        _ -> {
          let #(t, st) = to_temporal_time(st, plain_time, mk_undefined())
          #(rt_val.or_throw(st, get_epoch_ns_for(tz, d, t, Compatible)), st)
        }
      }
      make_zoned_cal(st, protos, ns, tz, cal)
    }
    PlainDateUntil | PlainDateSince -> {
      let #(#(other, other_cal), st) =
        to_temporal_date(st, helpers.arg_at(args, 0), mk_undefined())
      case other_cal == cal {
        False ->
          rt_val.t_throw_range_error(
            st,
            "cannot compute difference between dates of different calendars",
          )
        True ->
          date_until_since(st, protos, cal, d, other, args, m == PlainDateSince)
      }
    }
  }
}

fn optional_time_arg(st: Agent, v: JsVal) -> #(IsoTime, Agent) {
  case classify(v) {
    KUndef -> #(midnight, st)
    _ -> to_temporal_time(st, v, mk_undefined())
  }
}

fn date_until_since(
  st: Agent,
  protos: TemporalProtos,
  cal: temporal_calendar.Calendar,
  d1: IsoDate,
  d2: IsoDate,
  args: List(JsVal),
  is_since is_since: Bool,
) -> #(JsVal, Agent) {
  let #(#(largest, smallest, inc, mode), st) = get_difference_settings(st, args)
  let smallest = option.unwrap(smallest, Day)
  let largest = option.unwrap(largest, max_unit(smallest, Day))
  case unit_rank(smallest) < unit_rank(Day) {
    True ->
      rt_val.t_throw_range_error(
        st,
        "smallestUnit must be a date unit for PlainDate",
      )
    False -> {
      let Nil = require_largest_ge_smallest(st, largest, smallest)
      let mode = apply_since_mode(mode, is_since)
      let dur =
        rt_val.or_throw(
          st,
          difference_calendar_date(cal, d1, d2, largest, smallest, inc, mode),
        )
      let dur = apply_since_duration(dur, is_since)
      make_duration(st, protos, dur)
    }
  }
}
