import arc/internal/temporal_calendar as tcal
import arc/rt/builtins/helpers
import arc/rt/builtins/temporal_common.{
  CalAuto, Day, Nanosecond, UDay, apply_since_dur, apply_since_mode,
  calendar_suffix, check_diff_setup, date_time_slot_of, epoch_ns_to_iso_in,
  get_calendar_name_option, get_difference_settings, get_disambiguation_option,
  get_options_object, make_date_cal, make_date_time_cal, make_duration,
  make_time, make_zoned_cal, max_unit, parse_time_zone_id, require_temporal,
  round_options, round_to_increment, temporal_data_of, terr, time_only_ns,
  time_unit_ns, to_string_time_options, valid_time_increment,
} as tc
import arc/rt/builtins/temporal_diff.{compare_iso_date_time, diff_date_time_core}
import arc/rt/builtins/temporal_fields.{
  add_sub_args, calendar_date_add, calendar_with_fields, int_val,
  parse_plain_datetime_string, parsed_calendar_id, read_bag_calendar,
  require_nonempty_fields, require_partial_bag, resolve_calendar_date,
  to_calendar_arg, to_temporal_calendar_identifier, validated_overflow,
}
import arc/rt/builtins/temporal_iso.{
  type IsoDate, type TimeRec, AutoPrec, DurRec, IsoDate, TimeRec, epoch_days,
  epoch_ns_to_iso, format_iso_date, format_iso_time, is_valid_iso_date,
  is_valid_time, iso_datetime_within_limits, midnight, ns_max_instant,
  ns_per_day, time_to_ns, zero_dur,
}
import arc/rt/builtins/temporal_plain_date.{date_field_cal, date_getter_name}
import arc/rt/builtins/temporal_plain_time.{
  add_time, regulate_time, time_field, time_fields_apply, time_getter_name,
  to_temporal_time,
}
import arc/rt/builtins/temporal_zoned_ops.{
  date_time_fields_all_none, get_epoch_ns_for, read_date_time_fields,
}
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type NativeToken,
  type PlainDateTimeMethod, type TemporalDateTimeGetter, type TemporalProtos,
  type TemporalStaticName, DtDate, DtTime, KHandle, KStr, KUndef, PdtAdd,
  PdtEquals, PdtRound, PdtSince, PdtSubtract, PdtToJson, PdtToLocaleString,
  PdtToPlainDate, PdtToPlainTime, PdtToString, PdtToZonedDateTime, PdtUntil,
  PdtValueOf, PdtWith, PdtWithCalendar, PdtWithPlainTime, TemporalDate,
  TemporalDateTime, TemporalN, TemporalPlainDateTimeCtor,
  TemporalPlainDateTimeGetter, TemporalPlainDateTimeMethod,
  TemporalPlainDateTimeStatic, TemporalZonedDateTime, TsCompare, TsFrom,
  classify, mk_bool, mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/int
import gleam/list
import gleam/option.{None, Some}

const all_getters = [
  DtDate(types.DgCalendarId),
  DtDate(types.DgEra),
  DtDate(types.DgEraYear),
  DtDate(types.DgYear),
  DtDate(types.DgMonth),
  DtDate(types.DgMonthCode),
  DtDate(types.DgDay),
  DtTime(types.TgHour),
  DtTime(types.TgMinute),
  DtTime(types.TgSecond),
  DtTime(types.TgMillisecond),
  DtTime(types.TgMicrosecond),
  DtTime(types.TgNanosecond),
  DtDate(types.DgDayOfWeek),
  DtDate(types.DgDayOfYear),
  DtDate(types.DgWeekOfYear),
  DtDate(types.DgYearOfWeek),
  DtDate(types.DgDaysInWeek),
  DtDate(types.DgDaysInMonth),
  DtDate(types.DgDaysInYear),
  DtDate(types.DgMonthsInYear),
  DtDate(types.DgInLeapYear),
]

pub fn ctor_token(protos: TemporalProtos) -> NativeToken {
  TemporalN(TemporalPlainDateTimeCtor(protos:))
}

pub fn statics(protos: TemporalProtos) -> List(#(String, NativeToken, Int)) {
  list.map([#(TsFrom, 1), #(TsCompare, 2)], fn(s) {
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
      #(PdtWith, 1),
      #(PdtWithPlainTime, 0),
      #(PdtWithCalendar, 1),
      #(PdtAdd, 1),
      #(PdtSubtract, 1),
      #(PdtUntil, 1),
      #(PdtSince, 1),
      #(PdtRound, 1),
      #(PdtEquals, 1),
      #(PdtToString, 0),
      #(PdtToLocaleString, 0),
      #(PdtToJson, 0),
      #(PdtValueOf, 0),
      #(PdtToPlainDate, 0),
      #(PdtToPlainTime, 0),
      #(PdtToZonedDateTime, 1),
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

fn static_name(s: TemporalStaticName) -> String {
  case s {
    TsFrom -> "from"
    TsCompare -> "compare"
  }
}

pub fn date_time_getter_name(g: TemporalDateTimeGetter) -> String {
  case g {
    DtDate(g) -> date_getter_name(g)
    DtTime(g) -> time_getter_name(g)
  }
}

pub fn plain_date_time_method_name(m: PlainDateTimeMethod) -> String {
  case m {
    PdtWith -> "with"
    PdtWithPlainTime -> "withPlainTime"
    PdtWithCalendar -> "withCalendar"
    PdtAdd -> "add"
    PdtSubtract -> "subtract"
    PdtUntil -> "until"
    PdtSince -> "since"
    PdtRound -> "round"
    PdtEquals -> "equals"
    PdtToString -> "toString"
    PdtToLocaleString -> "toLocaleString"
    PdtToJson -> "toJSON"
    PdtValueOf -> "valueOf"
    PdtToPlainDate -> "toPlainDate"
    PdtToPlainTime -> "toPlainTime"
    PdtToZonedDateTime -> "toZonedDateTime"
  }
}

pub fn ctor(
  st: Agent,
  protos: TemporalProtos,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(y, st) = tc.arg_trunc_int(st, args, 0)
  let #(mo, st) = tc.arg_trunc_int(st, args, 1)
  let #(d, st) = tc.arg_trunc_int(st, args, 2)
  let #(h, st) = tc.opt_int_arg(st, args, 3)
  let #(mi, st) = tc.opt_int_arg(st, args, 4)
  let #(s, st) = tc.opt_int_arg(st, args, 5)
  let #(ms, st) = tc.opt_int_arg(st, args, 6)
  let #(us, st) = tc.opt_int_arg(st, args, 7)
  let #(ns, st) = tc.opt_int_arg(st, args, 8)
  let cal = terr(st, to_calendar_arg(helpers.arg_at(args, 9)))
  let t = TimeRec(h, mi, s, ms, us, ns)
  case is_valid_iso_date(y, mo, d) && is_valid_time(t) {
    False -> rt_val.t_throw_range_error(st, "invalid ISO date-time")
    True -> {
      let date = IsoDate(y, mo, d)
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
    TsFrom -> {
      let #(#(d, t, cal), st) =
        to_temporal_date_time(
          st,
          helpers.arg_at(args, 0),
          helpers.arg_at(args, 1),
        )
      make_date_time_cal(st, protos, d, t, cal)
    }
    TsCompare -> {
      let #(#(ad, at, _), st) =
        to_temporal_date_time(st, helpers.arg_at(args, 0), mk_undefined())
      let #(#(bd, bt, _), st) =
        to_temporal_date_time(st, helpers.arg_at(args, 1), mk_undefined())
      #(int_val(compare_iso_date_time(#(ad, at), #(bd, bt))), st)
    }
  }
}

pub fn to_temporal_date_time(
  st: Agent,
  item: JsVal,
  options: JsVal,
) -> #(#(IsoDate, TimeRec, tcal.Calendar), Agent) {
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
          let #(_o, st) = validated_overflow(st, options)
          let t =
            TimeRec(hour, minute, second, millisecond, microsecond, nanosecond)
          #(#(IsoDate(year, month, day), t, calendar), st)
        }
        Some(TemporalDate(year:, month:, day:, calendar:)) -> {
          let #(_o, st) = validated_overflow(st, options)
          #(#(IsoDate(year, month, day), midnight, calendar), st)
        }
        Some(TemporalZonedDateTime(epoch_ns:, time_zone:, calendar:)) -> {
          let #(_o, st) = validated_overflow(st, options)
          let #(d, t) = terr(st, epoch_ns_to_iso_in(time_zone, epoch_ns))
          #(#(d, t, calendar), st)
        }
        Some(_) | None -> date_time_from_bag(st, h, options)
      }
    KStr(s) -> {
      let p = terr(st, parse_plain_datetime_string(s))
      let t = option.unwrap(p.time, midnight)
      let cal = terr(st, parsed_calendar_id(p))
      let #(_o, st) = validated_overflow(st, options)
      case iso_datetime_within_limits(p.date, t) {
        True -> #(#(p.date, t, cal), st)
        False ->
          rt_val.t_throw_range_error(st, "date-time outside supported range")
      }
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
) -> #(#(IsoDate, TimeRec, tcal.Calendar), Agent) {
  let #(cal, st) = read_bag_calendar(st, h)
  let #(f, st) =
    read_date_time_fields(st, h, cal, read_offset: False, read_tz: False)
  let #(overflow, st) = validated_overflow(st, options)
  let date = terr(st, resolve_calendar_date(cal, f.date, overflow))
  let t0 = time_fields_apply(f.time, midnight)
  let t = terr(st, regulate_time(t0, overflow))
  case iso_datetime_within_limits(date, t) {
    True -> #(#(date, t, cal), st)
    False -> rt_val.t_throw_range_error(st, "date-time outside supported range")
  }
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
    DtTime(tg) -> #(time_field(t, tg), st)
    DtDate(dg) -> #(date_field_cal(cal, d, dg), st)
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
    PdtToJson -> #(
      mk_string(
        format_iso_date(d)
        <> "T"
        <> format_iso_time(t, AutoPrec)
        <> calendar_suffix(CalAuto, cal),
      ),
      st,
    )
    PdtToLocaleString -> #(
      mk_string(format_iso_date(d) <> " " <> format_iso_time(t, AutoPrec)),
      st,
    )
    PdtToString -> {
      let #(#(cal_name, opts), st) =
        get_calendar_name_option(st, helpers.arg_at(args, 0))
      let #(#(prec, su, sinc, mode), st) = to_string_time_options(st, opts)
      let #(d2, t2) = case su {
        None -> #(d, t)
        Some(u) -> {
          let total = epoch_days(d) * ns_per_day + time_to_ns(t)
          let rounded = round_to_increment(total, sinc * time_unit_ns(u), mode)
          epoch_ns_to_iso(rounded, 0)
        }
      }
      let s =
        format_iso_date(d2)
        <> "T"
        <> format_iso_time(t2, prec)
        <> calendar_suffix(cal_name, cal)
      #(mk_string(s), st)
    }
    PdtValueOf ->
      rt_val.t_throw_type_error(
        st,
        "Temporal.PlainDateTime cannot be converted with valueOf",
      )
    PdtEquals -> {
      let #(#(od, ot, ocal), st) =
        to_temporal_date_time(st, helpers.arg_at(args, 0), mk_undefined())
      #(mk_bool(#(d, t) == #(od, ot) && cal == ocal), st)
    }
    PdtAdd | PdtSubtract -> {
      let #(dur, overflow, st) = add_sub_args(st, args, m == PdtSubtract)
      let #(carry, t2) = add_time(t, time_only_ns(dur))
      let date_dur =
        DurRec(
          ..zero_dur,
          years: dur.years,
          months: dur.months,
          weeks: dur.weeks,
          days: dur.days + carry,
        )
      let d2 = terr(st, calendar_date_add(cal, d, date_dur, overflow))
      case iso_datetime_within_limits(d2, t2) {
        False ->
          rt_val.t_throw_range_error(st, "date-time outside supported range")
        True -> make_date_time_cal(st, protos, d2, t2, cal)
      }
    }
    PdtWithPlainTime -> {
      let arg = helpers.arg_at(args, 0)
      let #(t2, st) = case classify(arg) {
        KUndef -> #(midnight, st)
        _ -> to_temporal_time(st, arg, mk_undefined())
      }
      make_date_time_cal(st, protos, d, t2, cal)
    }
    PdtWithCalendar -> {
      let #(new_cal, st) =
        to_temporal_calendar_identifier(st, helpers.arg_at(args, 0))
      make_date_time_cal(st, protos, d, t, new_cal)
    }
    PdtWith -> {
      let #(bag, st) = require_partial_bag(st, helpers.arg_at(args, 0))
      let #(f, st) =
        read_date_time_fields(st, bag, cal, read_offset: False, read_tz: False)
      let Nil = require_nonempty_fields(st, date_time_fields_all_none(f))
      let #(overflow, st) = validated_overflow(st, helpers.arg_at(args, 1))
      let date = terr(st, calendar_with_fields(cal, d, f.date, overflow))
      let t0 = time_fields_apply(f.time, t)
      let t2 = terr(st, regulate_time(t0, overflow))
      case iso_datetime_within_limits(date, t2) {
        False ->
          rt_val.t_throw_range_error(st, "date-time outside supported range")
        True -> make_date_time_cal(st, protos, date, t2, cal)
      }
    }
    PdtRound -> {
      let #(#(su, inc, mode), st) =
        round_options(st, helpers.arg_at(args, 0), allow_day: True)
      let u_ns = time_unit_ns(su)
      let max = case su {
        UDay -> 1
        _ -> ns_per_day / u_ns
      }
      case valid_time_increment(inc, max) {
        False -> rt_val.t_throw_range_error(st, "invalid roundingIncrement")
        True -> {
          let total = epoch_days(d) * ns_per_day + time_to_ns(t)
          let rounded = round_to_increment(total, inc * u_ns, mode)
          let #(d2, t2) = epoch_ns_to_iso(rounded, 0)
          case iso_datetime_within_limits(d2, t2) {
            False ->
              rt_val.t_throw_range_error(
                st,
                "date-time outside supported range",
              )
            True -> make_date_time_cal(st, protos, d2, t2, cal)
          }
        }
      }
    }
    PdtToPlainDate -> make_date_cal(st, protos, d, cal)
    PdtToPlainTime -> make_time(st, protos, t)
    PdtToZonedDateTime -> {
      let arg = helpers.arg_at(args, 0)
      case classify(arg) {
        KStr(tz_str) -> {
          let tz = terr(st, parse_time_zone_id(tz_str))
          let #(opts, st) = get_options_object(st, helpers.arg_at(args, 1))
          let #(dis, st) = get_disambiguation_option(st, opts)
          let ns = terr(st, get_epoch_ns_for(tz, d, t, dis))
          case int.absolute_value(ns) <= ns_max_instant {
            False ->
              rt_val.t_throw_range_error(st, "instant outside valid range")
            True -> make_zoned_cal(st, protos, ns, tz, cal)
          }
        }
        KUndef -> rt_val.t_throw_type_error(st, "time zone is required")
        _ -> rt_val.t_throw_type_error(st, "time zone must be a string")
      }
    }
    PdtUntil | PdtSince -> {
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
            m == PdtSince,
          )
      }
    }
  }
}

fn date_time_until_since(
  st: Agent,
  protos: TemporalProtos,
  cal: tcal.Calendar,
  a: #(IsoDate, TimeRec),
  b: #(IsoDate, TimeRec),
  args: List(JsVal),
  is_since: Bool,
) -> #(JsVal, Agent) {
  let #(#(largest, smallest, inc, mode), st) = get_difference_settings(st, args)
  let smallest = option.unwrap(smallest, Nanosecond)
  let largest = option.unwrap(largest, max_unit(smallest, Day))
  let Nil = check_diff_setup(st, largest, smallest, inc)
  let mode2 = apply_since_mode(mode, is_since)
  let final =
    terr(
      st,
      diff_date_time_core(cal, a, b, largest, smallest, inc, mode2, False),
    )
  let final = apply_since_dur(final, is_since)
  make_duration(st, protos, final)
}
