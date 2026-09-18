import arc/internal/temporal_calendar
import arc/rt/builtins/helpers
import arc/rt/builtins/options.{get_options_object}
import arc/rt/builtins/temporal_common.{
  date_part, date_time_slot_of, make_date_cal, make_date_time_cal, make_duration,
  make_time, make_zoned_cal, require_temporal, static_name, temporal_data_of,
  time_part_ns, truncated_int_arg, truncated_int_arg_or,
}
import arc/rt/builtins/temporal_diff.{compare_iso_date_time, diff_date_time_core}
import arc/rt/builtins/temporal_fields.{
  add_sub_args, calendar_date_add, calendar_with_fields,
  parse_plain_datetime_string, parsed_calendar_id, read_bag_calendar,
  require_nonempty_fields, require_partial_bag, resolve_calendar_date,
  to_calendar_arg, to_temporal_calendar_identifier,
}
import arc/rt/builtins/temporal_iso.{
  type IsoDate, type IsoTime, AutoPrecision, Duration, IsoDate, IsoTime,
  check_date_time_limits, epoch_ns_to_iso, format_iso_date, format_iso_time,
  is_valid_iso_date, is_valid_time, iso_datetime_within_limits, midnight,
  ns_per_day, utc_epoch_ns,
}
import arc/rt/builtins/temporal_options.{
  CalendarNameAuto, calendar_suffix, get_calendar_name_option,
  get_disambiguation_option, get_overflow_option_from_value,
}
import arc/rt/builtins/temporal_plain_date.{date_field_cal, date_getter_name}
import arc/rt/builtins/temporal_plain_time.{
  add_time, regulate_time, time_field, time_fields_apply, time_getter_name,
  to_temporal_time,
}
import arc/rt/builtins/temporal_rounding.{
  Day, DayUnit, Nanosecond, apply_since_duration, apply_since_mode,
  check_diff_setup, get_difference_settings, max_unit, round_options,
  round_to_increment, time_unit_ns, to_string_time_options,
  valid_rounding_increment,
}
import arc/rt/builtins/temporal_time_zone.{
  epoch_ns_to_iso_in, time_zone_from_string, validate_epoch_ns,
}
import arc/rt/builtins/temporal_zoned_ops.{
  date_time_fields_all_none, get_epoch_ns_for, read_date_time_fields,
}
import arc/rt/temporal_data.{
  TemporalDate, TemporalDateTime, TemporalZonedDateTime,
}
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type NativeToken,
  type PlainDateTimeMethod, type TemporalDateTimeGetter, type TemporalProtos,
  type TemporalStaticName, CompareStatic, DateTimeDate, DateTimeTime, FromStatic,
  KHandle, KStr, KUndef, PlainDateTimeAdd, PlainDateTimeEquals,
  PlainDateTimeRound, PlainDateTimeSince, PlainDateTimeSubtract,
  PlainDateTimeToJson, PlainDateTimeToLocaleString, PlainDateTimeToPlainDate,
  PlainDateTimeToPlainTime, PlainDateTimeToString, PlainDateTimeToZonedDateTime,
  PlainDateTimeUntil, PlainDateTimeValueOf, PlainDateTimeWith,
  PlainDateTimeWithCalendar, PlainDateTimeWithPlainTime, TemporalN,
  TemporalPlainDateTimeCtor, TemporalPlainDateTimeGetter,
  TemporalPlainDateTimeMethod, TemporalPlainDateTimeStatic, classify, mk_bool,
  mk_int, mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/list
import gleam/option.{None, Some}

const all_getters = [
  DateTimeDate(types.DateCalendarId),
  DateTimeDate(types.DateEra),
  DateTimeDate(types.DateEraYear),
  DateTimeDate(types.DateYear),
  DateTimeDate(types.DateMonth),
  DateTimeDate(types.DateMonthCode),
  DateTimeDate(types.DateDay),
  DateTimeTime(types.TimeHour),
  DateTimeTime(types.TimeMinute),
  DateTimeTime(types.TimeSecond),
  DateTimeTime(types.TimeMillisecond),
  DateTimeTime(types.TimeMicrosecond),
  DateTimeTime(types.TimeNanosecond),
  DateTimeDate(types.DateDayOfWeek),
  DateTimeDate(types.DateDayOfYear),
  DateTimeDate(types.DateWeekOfYear),
  DateTimeDate(types.DateYearOfWeek),
  DateTimeDate(types.DateDaysInWeek),
  DateTimeDate(types.DateDaysInMonth),
  DateTimeDate(types.DateDaysInYear),
  DateTimeDate(types.DateMonthsInYear),
  DateTimeDate(types.DateInLeapYear),
]

pub fn ctor_token(protos: TemporalProtos) -> NativeToken {
  TemporalN(TemporalPlainDateTimeCtor(protos:))
}

pub fn statics(protos: TemporalProtos) -> List(#(String, NativeToken, Int)) {
  list.map([#(FromStatic, 1), #(CompareStatic, 2)], fn(s) {
    #(
      static_name(s.0),
      TemporalN(TemporalPlainDateTimeStatic(s.0, protos)),
      s.1,
    )
  })
}

pub fn getters() -> List(#(String, NativeToken)) {
  list.map(all_getters, fn(g) {
    #(date_time_getter_name(g), TemporalN(TemporalPlainDateTimeGetter(g)))
  })
}

pub fn methods(protos: TemporalProtos) -> List(#(String, NativeToken, Int)) {
  list.map(
    [
      #(PlainDateTimeWith, 1),
      #(PlainDateTimeWithPlainTime, 0),
      #(PlainDateTimeWithCalendar, 1),
      #(PlainDateTimeAdd, 1),
      #(PlainDateTimeSubtract, 1),
      #(PlainDateTimeUntil, 1),
      #(PlainDateTimeSince, 1),
      #(PlainDateTimeRound, 1),
      #(PlainDateTimeEquals, 1),
      #(PlainDateTimeToString, 0),
      #(PlainDateTimeToLocaleString, 0),
      #(PlainDateTimeToJson, 0),
      #(PlainDateTimeValueOf, 0),
      #(PlainDateTimeToPlainDate, 0),
      #(PlainDateTimeToPlainTime, 0),
      #(PlainDateTimeToZonedDateTime, 1),
    ],
    fn(m) {
      #(
        plain_date_time_method_name(m.0),
        TemporalN(TemporalPlainDateTimeMethod(m.0, protos)),
        m.1,
      )
    },
  )
}

pub fn date_time_getter_name(g: TemporalDateTimeGetter) -> String {
  case g {
    DateTimeDate(g) -> date_getter_name(g)
    DateTimeTime(g) -> time_getter_name(g)
  }
}

pub fn plain_date_time_method_name(m: PlainDateTimeMethod) -> String {
  case m {
    PlainDateTimeWith -> "with"
    PlainDateTimeWithPlainTime -> "withPlainTime"
    PlainDateTimeWithCalendar -> "withCalendar"
    PlainDateTimeAdd -> "add"
    PlainDateTimeSubtract -> "subtract"
    PlainDateTimeUntil -> "until"
    PlainDateTimeSince -> "since"
    PlainDateTimeRound -> "round"
    PlainDateTimeEquals -> "equals"
    PlainDateTimeToString -> "toString"
    PlainDateTimeToLocaleString -> "toLocaleString"
    PlainDateTimeToJson -> "toJSON"
    PlainDateTimeValueOf -> "valueOf"
    PlainDateTimeToPlainDate -> "toPlainDate"
    PlainDateTimeToPlainTime -> "toPlainTime"
    PlainDateTimeToZonedDateTime -> "toZonedDateTime"
  }
}

pub fn ctor(
  st: Agent,
  protos: TemporalProtos,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(year, st) = truncated_int_arg(st, args, 0)
  let #(month, st) = truncated_int_arg(st, args, 1)
  let #(day, st) = truncated_int_arg(st, args, 2)
  let #(hour, st) = truncated_int_arg_or(st, args, 3, 0)
  let #(minute, st) = truncated_int_arg_or(st, args, 4, 0)
  let #(second, st) = truncated_int_arg_or(st, args, 5, 0)
  let #(millisecond, st) = truncated_int_arg_or(st, args, 6, 0)
  let #(microsecond, st) = truncated_int_arg_or(st, args, 7, 0)
  let #(nanosecond, st) = truncated_int_arg_or(st, args, 8, 0)
  let cal = rt_val.or_throw(st, to_calendar_arg(helpers.arg_at(args, 9)))
  let t =
    IsoTime(hour:, minute:, second:, millisecond:, microsecond:, nanosecond:)
  case is_valid_iso_date(year, month, day) && is_valid_time(t) {
    False -> rt_val.t_throw_range_error(st, "invalid ISO date-time")
    True -> {
      let date = IsoDate(year:, month:, day:)
      case iso_datetime_within_limits(date, t) {
        False ->
          rt_val.t_throw_range_error(st, "date-time outside of supported range")
        True -> make_date_time_cal(st, protos, date, t, cal)
      }
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
      let #(#(d, t, cal), st) =
        to_temporal_date_time(
          st,
          helpers.arg_at(args, 0),
          helpers.arg_at(args, 1),
        )
      make_date_time_cal(st, protos, d, t, cal)
    }
    CompareStatic -> {
      let #(#(ad, at, _), st) =
        to_temporal_date_time(st, helpers.arg_at(args, 0), mk_undefined())
      let #(#(bd, bt, _), st) =
        to_temporal_date_time(st, helpers.arg_at(args, 1), mk_undefined())
      #(mk_int(compare_iso_date_time(#(ad, at), #(bd, bt))), st)
    }
  }
}

pub fn to_temporal_date_time(
  st: Agent,
  item: JsVal,
  options: JsVal,
) -> #(#(IsoDate, IsoTime, temporal_calendar.Calendar), Agent) {
  case classify(item) {
    KHandle(h) ->
      case temporal_data_of(st, item) {
        Some(TemporalDateTime(
          year:,
          month:,
          day:,
          hour:,
          minute:,
          second:,
          millisecond:,
          microsecond:,
          nanosecond:,
          calendar:,
        )) -> {
          let #(_o, st) = get_overflow_option_from_value(st, options)
          let t =
            IsoTime(hour, minute, second, millisecond, microsecond, nanosecond)
          #(#(IsoDate(year, month, day), t, calendar), st)
        }
        Some(TemporalDate(year:, month:, day:, calendar:)) -> {
          let #(_o, st) = get_overflow_option_from_value(st, options)
          #(#(IsoDate(year, month, day), midnight, calendar), st)
        }
        Some(TemporalZonedDateTime(epoch_ns:, time_zone:, calendar:)) -> {
          let #(_o, st) = get_overflow_option_from_value(st, options)
          let #(d, t) = epoch_ns_to_iso_in(time_zone, epoch_ns)
          #(#(d, t, calendar), st)
        }
        Some(_) | None -> date_time_from_bag(st, h, options)
      }
    KStr(s) -> {
      let p = rt_val.or_throw(st, parse_plain_datetime_string(s))
      let t = option.unwrap(p.time, midnight)
      let cal = rt_val.or_throw(st, parsed_calendar_id(p))
      let #(_o, st) = get_overflow_option_from_value(st, options)
      let #(d, t) = rt_val.or_throw(st, check_date_time_limits(p.date, t))
      #(#(d, t, cal), st)
    }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "cannot convert to a Temporal.PlainDateTime",
      )
  }
}

pub fn date_time_from_bag(
  st: Agent,
  h: Handle,
  options: JsVal,
) -> #(#(IsoDate, IsoTime, temporal_calendar.Calendar), Agent) {
  let #(cal, st) = read_bag_calendar(st, h)
  let #(f, st) =
    read_date_time_fields(st, h, cal, read_offset: False, read_tz: False)
  let #(overflow, st) = get_overflow_option_from_value(st, options)
  let date = rt_val.or_throw(st, resolve_calendar_date(cal, f.date, overflow))
  let t0 = time_fields_apply(f.time, midnight)
  let t = rt_val.or_throw(st, regulate_time(t0, overflow))
  let #(date, t) = rt_val.or_throw(st, check_date_time_limits(date, t))
  #(#(date, t, cal), st)
}

pub fn getter(
  st: Agent,
  g: TemporalDateTimeGetter,
  this: JsVal,
) -> #(JsVal, Agent) {
  let #(d, t, cal) =
    require_temporal(
      st,
      this,
      "PlainDateTime",
      date_time_getter_name(g),
      date_time_slot_of,
    )
  case g {
    DateTimeTime(tg) -> #(time_field(t, tg), st)
    DateTimeDate(dg) -> #(date_field_cal(cal, d, dg), st)
  }
}

pub fn method(
  st: Agent,
  m: PlainDateTimeMethod,
  protos: TemporalProtos,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(d, t, cal) =
    require_temporal(
      st,
      this,
      "PlainDateTime",
      plain_date_time_method_name(m),
      date_time_slot_of,
    )
  case m {
    PlainDateTimeToJson -> #(
      mk_string(
        format_iso_date(d)
        <> "T"
        <> format_iso_time(t, AutoPrecision)
        <> calendar_suffix(CalendarNameAuto, cal),
      ),
      st,
    )
    PlainDateTimeToLocaleString -> #(
      mk_string(format_iso_date(d) <> " " <> format_iso_time(t, AutoPrecision)),
      st,
    )
    PlainDateTimeToString -> {
      let #(opts, st) = get_options_object(st, helpers.arg_at(args, 0))
      let #(cal_name, st) = get_calendar_name_option(st, opts)
      let #(#(precision, smallest_time_unit, inc, mode), st) =
        to_string_time_options(st, opts)
      let #(d2, t2) = case smallest_time_unit {
        None -> #(d, t)
        Some(u) -> {
          let rounded =
            round_to_increment(utc_epoch_ns(d, t), inc * time_unit_ns(u), mode)
          epoch_ns_to_iso(rounded, 0)
        }
      }
      let s =
        format_iso_date(d2)
        <> "T"
        <> format_iso_time(t2, precision)
        <> calendar_suffix(cal_name, cal)
      #(mk_string(s), st)
    }
    PlainDateTimeValueOf ->
      rt_val.t_throw_type_error(
        st,
        "Temporal.PlainDateTime cannot be converted with valueOf",
      )
    PlainDateTimeEquals -> {
      let #(#(od, ot, ocal), st) =
        to_temporal_date_time(st, helpers.arg_at(args, 0), mk_undefined())
      #(mk_bool(#(d, t) == #(od, ot) && cal == ocal), st)
    }
    PlainDateTimeAdd | PlainDateTimeSubtract -> {
      let #(dur, overflow, st) =
        add_sub_args(st, args, m == PlainDateTimeSubtract)
      let #(carry, t2) = add_time(t, time_part_ns(dur))
      let date_dur = Duration(..date_part(dur), days: dur.days + carry)
      let d2 =
        rt_val.or_throw(st, calendar_date_add(cal, d, date_dur, overflow))
      let #(d2, t2) = rt_val.or_throw(st, check_date_time_limits(d2, t2))
      make_date_time_cal(st, protos, d2, t2, cal)
    }
    PlainDateTimeWithPlainTime -> {
      let arg = helpers.arg_at(args, 0)
      let #(t2, st) = case classify(arg) {
        KUndef -> #(midnight, st)
        _ -> to_temporal_time(st, arg, mk_undefined())
      }
      make_date_time_cal(st, protos, d, t2, cal)
    }
    PlainDateTimeWithCalendar -> {
      let #(new_cal, st) =
        to_temporal_calendar_identifier(st, helpers.arg_at(args, 0))
      make_date_time_cal(st, protos, d, t, new_cal)
    }
    PlainDateTimeWith -> {
      let #(bag, st) = require_partial_bag(st, helpers.arg_at(args, 0))
      let #(f, st) =
        read_date_time_fields(st, bag, cal, read_offset: False, read_tz: False)
      let Nil = require_nonempty_fields(st, date_time_fields_all_none(f))
      let #(overflow, st) =
        get_overflow_option_from_value(st, helpers.arg_at(args, 1))
      let date =
        rt_val.or_throw(st, calendar_with_fields(cal, d, f.date, overflow))
      let t0 = time_fields_apply(f.time, t)
      let t2 = rt_val.or_throw(st, regulate_time(t0, overflow))
      let #(date, t2) = rt_val.or_throw(st, check_date_time_limits(date, t2))
      make_date_time_cal(st, protos, date, t2, cal)
    }
    PlainDateTimeRound -> {
      let #(#(smallest_time_unit, inc, mode), st) =
        round_options(st, helpers.arg_at(args, 0), allow_day: True)
      let unit_ns = time_unit_ns(smallest_time_unit)
      let max = case smallest_time_unit {
        DayUnit -> 1
        _ -> ns_per_day / unit_ns
      }
      case valid_rounding_increment(inc, max, inclusive: False) {
        False -> rt_val.t_throw_range_error(st, "invalid roundingIncrement")
        True -> {
          let rounded =
            round_to_increment(utc_epoch_ns(d, t), inc * unit_ns, mode)
          let #(d2, t2) = epoch_ns_to_iso(rounded, 0)
          let #(d2, t2) = rt_val.or_throw(st, check_date_time_limits(d2, t2))
          make_date_time_cal(st, protos, d2, t2, cal)
        }
      }
    }
    PlainDateTimeToPlainDate -> make_date_cal(st, protos, d, cal)
    PlainDateTimeToPlainTime -> make_time(st, protos, t)
    PlainDateTimeToZonedDateTime -> {
      let arg = helpers.arg_at(args, 0)
      case classify(arg) {
        KStr(tz_text) -> {
          let #(tz, st) = time_zone_from_string(st, tz_text)
          let #(opts, st) = get_options_object(st, helpers.arg_at(args, 1))
          let #(dis, st) = get_disambiguation_option(st, opts)
          let ns = rt_val.or_throw(st, get_epoch_ns_for(tz, d, t, dis))
          let ns = rt_val.or_throw(st, validate_epoch_ns(ns))
          make_zoned_cal(st, protos, ns, tz, cal)
        }
        KUndef -> rt_val.t_throw_type_error(st, "time zone is required")
        _ -> rt_val.t_throw_type_error(st, "time zone must be a string")
      }
    }
    PlainDateTimeUntil | PlainDateTimeSince -> {
      let #(#(od, ot, ocal), st) =
        to_temporal_date_time(st, helpers.arg_at(args, 0), mk_undefined())
      case ocal == cal {
        False ->
          rt_val.t_throw_range_error(
            st,
            "cannot compute difference between dates of different calendars",
          )
        True ->
          date_time_until_since(
            st,
            protos,
            cal,
            #(d, t),
            #(od, ot),
            args,
            m == PlainDateTimeSince,
          )
      }
    }
  }
}

fn date_time_until_since(
  st: Agent,
  protos: TemporalProtos,
  cal: temporal_calendar.Calendar,
  a: #(IsoDate, IsoTime),
  b: #(IsoDate, IsoTime),
  args: List(JsVal),
  is_since is_since: Bool,
) -> #(JsVal, Agent) {
  let #(#(largest, smallest, inc, mode), st) = get_difference_settings(st, args)
  let smallest = option.unwrap(smallest, Nanosecond)
  let largest = option.unwrap(largest, max_unit(smallest, Day))
  let Nil = check_diff_setup(st, largest, smallest, inc)
  let mode = apply_since_mode(mode, is_since)
  let final =
    rt_val.or_throw(
      st,
      diff_date_time_core(cal, a, b, largest, smallest, inc, mode, zoned: False),
    )
  let final = apply_since_duration(final, is_since)
  make_duration(st, protos, final)
}
