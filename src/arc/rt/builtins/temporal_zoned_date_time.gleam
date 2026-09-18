import arc/bytecode/error_kind.{JsError, RangeError}
import arc/internal/int_math.{floor_div}
import arc/internal/temporal_calendar
import arc/rt/builtins/helpers
import arc/rt/builtins/temporal_common.{
  Compatible, DayUnit, Hour, InvalidIdentifier, Nanosecond, OffsetShowAuto,
  OffsetShowNever, PreferOffset, Trunc, UnknownIdentifier, ZoneNameAuto,
  ZoneNameCritical, ZoneNameNever, apply_since_duration, apply_since_mode,
  apply_since_ns, as_if_positive_mode, balance_time_ns, calendar_suffix,
  check_diff_setup, date_part, epoch_ns_to_iso_in, format_offset_full,
  format_offset_rounded, get_calendar_name_option, get_difference_settings,
  get_disambiguation_option, get_enum_option, get_fractional_digits,
  get_offset_option, get_options_object, get_overflow_option,
  get_rounding_mode_option, get_show_offset_option, get_time_zone_name_option,
  get_unit_option, has_date_units, is_valid_epoch_ns, make_date_cal,
  make_date_time_cal, make_duration, make_instant, make_time, make_zoned_cal,
  max_rounding_increment, max_unit, parse_time_zone_identifier, require_temporal,
  require_time_unit, round_options, round_to_increment, seconds_string_precision,
  static_name, time_part_ns, time_unit_ns, time_zone_equals, time_zone_id,
  to_temporal_time_zone, tz_offset_ns_at, unit_rank, valid_rounding_increment,
  validate_epoch_ns, zoned_slot_of,
}
import arc/rt/builtins/temporal_diff.{diff_date_time_core}
import arc/rt/builtins/temporal_fields.{
  add_sub_args, calendar_date_add, calendar_with_fields, require_nonempty_fields,
  require_partial_bag, to_calendar_arg, to_temporal_calendar_identifier,
}
import arc/rt/builtins/temporal_iso.{
  type SecondsPrecision, AutoPrecision, add_days, divide_as_float,
  epoch_ns_to_iso, format_iso_date, format_iso_time, int_sign,
  iso_date_from_epoch_days, ns_per_day, ns_per_hour, ns_per_ms,
}
import arc/rt/builtins/temporal_plain_date.{date_field_cal, date_getter_name}
import arc/rt/builtins/temporal_plain_time.{
  regulate_time, time_field, time_fields_apply, time_getter_name,
  to_temporal_time,
}
import arc/rt/builtins/temporal_tz
import arc/rt/builtins/temporal_zoned_ops.{
  OptionOffset, date_time_fields_all_none, get_epoch_ns_for, interpret_offset,
  read_date_time_fields, start_of_day_ns, to_temporal_zoned,
}
import arc/rt/types.{
  type Agent, type JsVal, type NativeToken, type TemporalProtos,
  type TemporalStaticName, type TemporalZonedGetter, type TimeZone,
  type ZonedDateTimeMethod, CompareStatic, DateCalendarId, DateDay,
  DateDayOfWeek, DateDayOfYear, DateDaysInMonth, DateDaysInWeek, DateDaysInYear,
  DateEra, DateEraYear, DateInLeapYear, DateMonth, DateMonthCode,
  DateMonthsInYear, DateWeekOfYear, DateYear, DateYearOfWeek, FromStatic,
  IanaZone, JFloat, KHandle, KStr, KUndef, OffsetZone, TemporalN,
  TemporalZonedDateTimeCtor, TemporalZonedDateTimeGetter,
  TemporalZonedDateTimeMethod, TemporalZonedDateTimeStatic, TimeHour,
  TimeMicrosecond, TimeMillisecond, TimeMinute, TimeNanosecond, TimeSecond,
  UtcZone, ZonedDate, ZonedDateTimeAdd, ZonedDateTimeEquals,
  ZonedDateTimeGetTimeZoneTransition, ZonedDateTimeRound, ZonedDateTimeSince,
  ZonedDateTimeStartOfDay, ZonedDateTimeSubtract, ZonedDateTimeToInstant,
  ZonedDateTimeToJson, ZonedDateTimeToLocaleString, ZonedDateTimeToPlainDate,
  ZonedDateTimeToPlainDateTime, ZonedDateTimeToPlainTime, ZonedDateTimeToString,
  ZonedDateTimeUntil, ZonedDateTimeValueOf, ZonedDateTimeWith,
  ZonedDateTimeWithCalendar, ZonedDateTimeWithPlainTime,
  ZonedDateTimeWithTimeZone, ZonedEpochMilliseconds, ZonedEpochNanoseconds,
  ZonedHoursInDay, ZonedOffset, ZonedOffsetNanoseconds, ZonedTime,
  ZonedTimeZoneId, classify, mk_bigint, mk_bool, mk_int, mk_null, mk_number,
  mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/list
import gleam/option.{None, Some}
import gleam/result

const all_getters = [
  ZonedDate(DateCalendarId),
  ZonedTimeZoneId,
  ZonedDate(DateEra),
  ZonedDate(DateEraYear),
  ZonedDate(DateYear),
  ZonedDate(DateMonth),
  ZonedDate(DateMonthCode),
  ZonedDate(DateDay),
  ZonedTime(TimeHour),
  ZonedTime(TimeMinute),
  ZonedTime(TimeSecond),
  ZonedTime(TimeMillisecond),
  ZonedTime(TimeMicrosecond),
  ZonedTime(TimeNanosecond),
  ZonedEpochMilliseconds,
  ZonedEpochNanoseconds,
  ZonedDate(DateDayOfWeek),
  ZonedDate(DateDayOfYear),
  ZonedDate(DateWeekOfYear),
  ZonedDate(DateYearOfWeek),
  ZonedHoursInDay,
  ZonedDate(DateDaysInWeek),
  ZonedDate(DateDaysInMonth),
  ZonedDate(DateDaysInYear),
  ZonedDate(DateMonthsInYear),
  ZonedDate(DateInLeapYear),
  ZonedOffsetNanoseconds,
  ZonedOffset,
]

pub fn ctor_token(protos: TemporalProtos) -> NativeToken {
  TemporalN(TemporalZonedDateTimeCtor(protos:))
}

pub fn statics(protos: TemporalProtos) -> List(#(String, NativeToken, Int)) {
  list.map([#(FromStatic, 1), #(CompareStatic, 2)], fn(s) {
    #(
      static_name(s.0),
      TemporalN(TemporalZonedDateTimeStatic(s.0, protos)),
      s.1,
    )
  })
}

pub fn getters() -> List(#(String, NativeToken)) {
  list.map(all_getters, fn(g) {
    #(zoned_getter_name(g), TemporalN(TemporalZonedDateTimeGetter(g)))
  })
}

pub fn methods(protos: TemporalProtos) -> List(#(String, NativeToken, Int)) {
  list.map(
    [
      #(ZonedDateTimeWithTimeZone, 1),
      #(ZonedDateTimeWithCalendar, 1),
      #(ZonedDateTimeWithPlainTime, 0),
      #(ZonedDateTimeWith, 1),
      #(ZonedDateTimeAdd, 1),
      #(ZonedDateTimeSubtract, 1),
      #(ZonedDateTimeUntil, 1),
      #(ZonedDateTimeSince, 1),
      #(ZonedDateTimeRound, 1),
      #(ZonedDateTimeEquals, 1),
      #(ZonedDateTimeToString, 0),
      #(ZonedDateTimeToLocaleString, 0),
      #(ZonedDateTimeToJson, 0),
      #(ZonedDateTimeValueOf, 0),
      #(ZonedDateTimeStartOfDay, 0),
      #(ZonedDateTimeGetTimeZoneTransition, 1),
      #(ZonedDateTimeToInstant, 0),
      #(ZonedDateTimeToPlainDate, 0),
      #(ZonedDateTimeToPlainTime, 0),
      #(ZonedDateTimeToPlainDateTime, 0),
    ],
    fn(m) {
      #(
        method_name(m.0),
        TemporalN(TemporalZonedDateTimeMethod(m.0, protos)),
        m.1,
      )
    },
  )
}

pub fn zoned_getter_name(g: TemporalZonedGetter) -> String {
  case g {
    ZonedTimeZoneId -> "timeZoneId"
    ZonedEpochMilliseconds -> "epochMilliseconds"
    ZonedEpochNanoseconds -> "epochNanoseconds"
    ZonedOffsetNanoseconds -> "offsetNanoseconds"
    ZonedOffset -> "offset"
    ZonedHoursInDay -> "hoursInDay"
    ZonedDate(dg) -> date_getter_name(dg)
    ZonedTime(tg) -> time_getter_name(tg)
  }
}

pub fn method_name(m: ZonedDateTimeMethod) -> String {
  case m {
    ZonedDateTimeWithTimeZone -> "withTimeZone"
    ZonedDateTimeWithCalendar -> "withCalendar"
    ZonedDateTimeWithPlainTime -> "withPlainTime"
    ZonedDateTimeWith -> "with"
    ZonedDateTimeAdd -> "add"
    ZonedDateTimeSubtract -> "subtract"
    ZonedDateTimeUntil -> "until"
    ZonedDateTimeSince -> "since"
    ZonedDateTimeRound -> "round"
    ZonedDateTimeEquals -> "equals"
    ZonedDateTimeToString -> "toString"
    ZonedDateTimeToLocaleString -> "toLocaleString"
    ZonedDateTimeToJson -> "toJSON"
    ZonedDateTimeValueOf -> "valueOf"
    ZonedDateTimeStartOfDay -> "startOfDay"
    ZonedDateTimeGetTimeZoneTransition -> "getTimeZoneTransition"
    ZonedDateTimeToInstant -> "toInstant"
    ZonedDateTimeToPlainDate -> "toPlainDate"
    ZonedDateTimeToPlainTime -> "toPlainTime"
    ZonedDateTimeToPlainDateTime -> "toPlainDateTime"
  }
}

pub fn ctor(
  st: Agent,
  protos: TemporalProtos,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(ns, st) = rt_val.t_to_bigint(st, helpers.arg_at(args, 0))
  case classify(helpers.arg_at(args, 1)) {
    KStr(tz_text) -> {
      // only bare identifiers, not iso date-time strings
      let #(parsed, st) = parse_time_zone_identifier(st, tz_text)
      let tz =
        rt_val.or_throw(st, case parsed {
          Ok(tz) -> Ok(tz)
          Error(UnknownIdentifier) ->
            Error(JsError(
              RangeError,
              "invalid time zone identifier: " <> tz_text,
            ))
          Error(InvalidIdentifier(e)) -> Error(e)
        })
      let cal = rt_val.or_throw(st, to_calendar_arg(helpers.arg_at(args, 2)))
      case is_valid_epoch_ns(ns) {
        False ->
          rt_val.t_throw_range_error(st, "epoch nanoseconds out of range")
        True -> make_zoned_cal(st, protos, ns, tz, cal)
      }
    }
    _ -> rt_val.t_throw_type_error(st, "time zone must be a string")
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
      let #(#(ns, tz, cal), st) =
        to_temporal_zoned(st, helpers.arg_at(args, 0), helpers.arg_at(args, 1))
      make_zoned_cal(st, protos, ns, tz, cal)
    }
    CompareStatic -> {
      let #(#(a, _, _), st) =
        to_temporal_zoned(st, helpers.arg_at(args, 0), mk_undefined())
      let #(#(b, _, _), st) =
        to_temporal_zoned(st, helpers.arg_at(args, 1), mk_undefined())
      #(mk_int(int_sign(a - b)), st)
    }
  }
}

fn require_zoned(
  st: Agent,
  this: JsVal,
  name: String,
) -> #(Int, TimeZone, temporal_calendar.Calendar) {
  require_temporal(st, this, "ZonedDateTime", name, zoned_slot_of)
}

pub fn getter(
  st: Agent,
  g: TemporalZonedGetter,
  this: JsVal,
) -> #(JsVal, Agent) {
  let #(ns, tz, zcal) = require_zoned(st, this, zoned_getter_name(g))
  let offset = tz_offset_ns_at(tz, ns)
  let #(d, t) = epoch_ns_to_iso(ns, offset)
  case g {
    ZonedTimeZoneId -> #(mk_string(time_zone_id(tz)), st)
    ZonedEpochMilliseconds -> #(mk_int(floor_div(ns, ns_per_ms)), st)
    ZonedEpochNanoseconds -> #(mk_bigint(ns), st)
    ZonedOffsetNanoseconds -> #(mk_int(offset), st)
    ZonedOffset -> #(mk_string(format_offset_full(offset)), st)
    ZonedHoursInDay -> {
      let s1 = rt_val.or_throw(st, start_of_day_ns(tz, d))
      let s2 = rt_val.or_throw(st, start_of_day_ns(tz, add_days(d, 1)))
      #(mk_number(JFloat(divide_as_float(s2 - s1, ns_per_hour))), st)
    }
    ZonedTime(tg) -> #(time_field(t, tg), st)
    ZonedDate(dg) -> #(date_field_cal(zcal, d, dg), st)
  }
}

type TransitionDirection {
  Next
  Previous
}

pub fn method(
  st: Agent,
  m: ZonedDateTimeMethod,
  protos: TemporalProtos,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(ns, tz, zcal) = require_zoned(st, this, method_name(m))
  let off = tz_offset_ns_at(tz, ns)
  let #(d, t) = epoch_ns_to_iso(ns, off)
  case m {
    ZonedDateTimeToJson | ZonedDateTimeToLocaleString -> #(
      mk_string(format_zoned(ns, tz, AutoPrecision)),
      st,
    )
    ZonedDateTimeToString -> {
      let #(opts, st) = get_options_object(st, helpers.arg_at(args, 0))
      let #(cal_name, st) = get_calendar_name_option(st, opts)
      let #(digits, st) = get_fractional_digits(st, opts)
      let #(offset_mode, st) = get_show_offset_option(st, opts)
      let #(mode, st) = get_rounding_mode_option(st, opts, Trunc)
      let #(smallest, st) =
        get_unit_option(st, opts, "smallestUnit", allow_auto: False)
      let #(tz_mode, st) = get_time_zone_name_option(st, opts)
      let #(precision, smallest_time_unit, inc) =
        rt_val.or_throw(st, seconds_string_precision(digits, smallest))
      let rounded = case smallest_time_unit {
        None -> ns
        Some(u) ->
          round_to_increment(
            ns,
            inc * time_unit_ns(u),
            as_if_positive_mode(mode),
          )
      }
      let off2 = tz_offset_ns_at(tz, rounded)
      let #(d2, t2) = epoch_ns_to_iso(rounded, off2)
      let base = format_iso_date(d2) <> "T" <> format_iso_time(t2, precision)
      let with_offset = case offset_mode {
        OffsetShowNever -> base
        OffsetShowAuto -> base <> format_offset_rounded(off2)
      }
      let with_tz = case tz_mode {
        ZoneNameNever -> with_offset
        ZoneNameCritical -> with_offset <> "[!" <> time_zone_id(tz) <> "]"
        ZoneNameAuto -> with_offset <> "[" <> time_zone_id(tz) <> "]"
      }
      #(mk_string(with_tz <> calendar_suffix(cal_name, zcal)), st)
    }
    ZonedDateTimeValueOf ->
      rt_val.t_throw_type_error(
        st,
        "Temporal.ZonedDateTime cannot be converted with valueOf",
      )
    ZonedDateTimeEquals -> {
      let #(#(ons, otz, ocal), st) =
        to_temporal_zoned(st, helpers.arg_at(args, 0), mk_undefined())
      #(mk_bool(ns == ons && time_zone_equals(tz, otz) && zcal == ocal), st)
    }
    ZonedDateTimeAdd | ZonedDateTimeSubtract -> {
      let #(dur, overflow, st) =
        add_sub_args(st, args, m == ZonedDateTimeSubtract)
      let base_ns =
        rt_val.or_throw(st, case has_date_units(dur) {
          False -> Ok(ns)
          True -> {
            use d2 <- result.try(calendar_date_add(
              zcal,
              d,
              date_part(dur),
              overflow,
            ))
            get_epoch_ns_for(tz, d2, t, Compatible)
          }
        })
      let ns2 =
        rt_val.or_throw(st, validate_epoch_ns(base_ns + time_part_ns(dur)))
      make_zoned_cal(st, protos, ns2, tz, zcal)
    }
    ZonedDateTimeWithTimeZone -> {
      let #(tz2, st) = to_temporal_time_zone(st, helpers.arg_at(args, 0))
      make_zoned_cal(st, protos, ns, tz2, zcal)
    }
    ZonedDateTimeUntil | ZonedDateTimeSince -> {
      let #(#(ons, otz, ocal), st) =
        to_temporal_zoned(st, helpers.arg_at(args, 0), mk_undefined())
      case ocal == zcal {
        False ->
          rt_val.t_throw_range_error(
            st,
            "cannot compute difference between dates of different calendars",
          )
        True ->
          zoned_until_since(
            st,
            protos,
            zcal,
            ns,
            tz,
            ons,
            otz,
            args,
            m == ZonedDateTimeSince,
          )
      }
    }
    ZonedDateTimeRound -> {
      let #(#(smallest_time_unit, inc, mode), st) =
        round_options(st, helpers.arg_at(args, 0), allow_day: True)
      let unit_ns = time_unit_ns(smallest_time_unit)
      let max = max_rounding_increment(smallest_time_unit) |> option.unwrap(1)
      case valid_rounding_increment(inc, max, inclusive: False) {
        False -> rt_val.t_throw_range_error(st, "invalid roundingIncrement")
        True -> {
          let local = ns + off
          let day_part = floor_div(local, ns_per_day)
          let local_date = iso_date_from_epoch_days(day_part)
          case smallest_time_unit == DayUnit {
            True -> {
              let day_start =
                rt_val.or_throw(st, start_of_day_ns(tz, local_date))
              let day_end =
                rt_val.or_throw(
                  st,
                  start_of_day_ns(tz, iso_date_from_epoch_days(day_part + 1)),
                )
              let ns2 =
                day_start
                + round_to_increment(ns - day_start, day_end - day_start, mode)
              make_zoned_cal(st, protos, ns2, tz, zcal)
            }
            False -> {
              let tod = local - day_part * ns_per_day
              let rounded_tod = round_to_increment(tod, inc * unit_ns, mode)
              let #(rd, rt) =
                epoch_ns_to_iso(day_part * ns_per_day + rounded_tod, 0)
              let ns2 =
                rt_val.or_throw(
                  st,
                  interpret_offset(
                    rd,
                    rt,
                    OptionOffset(off),
                    tz,
                    Compatible,
                    PreferOffset,
                    match_minutes: False,
                  ),
                )
              let ns2 = rt_val.or_throw(st, validate_epoch_ns(ns2))
              make_zoned_cal(st, protos, ns2, tz, zcal)
            }
          }
        }
      }
    }
    ZonedDateTimeWith -> {
      let #(bag, st) = require_partial_bag(st, helpers.arg_at(args, 0))
      let #(f, st) =
        read_date_time_fields(st, bag, zcal, read_offset: True, read_tz: False)
      let Nil = require_nonempty_fields(st, date_time_fields_all_none(f))
      let #(opts, st) = get_options_object(st, helpers.arg_at(args, 1))
      let #(dis_opt, st) = get_disambiguation_option(st, opts)
      let #(off_opt, st) = get_offset_option(st, opts, PreferOffset)
      let #(overflow, st) = get_overflow_option(st, opts)
      let date =
        rt_val.or_throw(st, calendar_with_fields(zcal, d, f.date, overflow))
      let t0 = time_fields_apply(f.time, t)
      let t2 = rt_val.or_throw(st, regulate_time(t0, overflow))
      let ns2 =
        rt_val.or_throw(
          st,
          interpret_offset(
            date,
            t2,
            OptionOffset(option.unwrap(f.offset, off)),
            tz,
            dis_opt,
            off_opt,
            match_minutes: False,
          ),
        )
      make_zoned_cal(st, protos, ns2, tz, zcal)
    }
    ZonedDateTimeWithCalendar -> {
      let #(new_cal, st) =
        to_temporal_calendar_identifier(st, helpers.arg_at(args, 0))
      make_zoned_cal(st, protos, ns, tz, new_cal)
    }
    ZonedDateTimeWithPlainTime -> {
      // explicit midnight differs from start of day when midnight is skipped
      let arg = helpers.arg_at(args, 0)
      case classify(arg) {
        KUndef -> {
          let ns2 = rt_val.or_throw(st, start_of_day_ns(tz, d))
          make_zoned_cal(st, protos, ns2, tz, zcal)
        }
        _ -> {
          let #(t2, st) = to_temporal_time(st, arg, mk_undefined())
          let ns2 = rt_val.or_throw(st, get_epoch_ns_for(tz, d, t2, Compatible))
          make_zoned_cal(st, protos, ns2, tz, zcal)
        }
      }
    }
    ZonedDateTimeStartOfDay -> {
      let ns2 = rt_val.or_throw(st, start_of_day_ns(tz, d))
      make_zoned_cal(st, protos, ns2, tz, zcal)
    }
    ZonedDateTimeGetTimeZoneTransition -> {
      let arg = helpers.arg_at(args, 0)
      let #(dir, st) = case classify(arg) {
        KUndef ->
          rt_val.t_throw_type_error(st, "direction parameter is required")
        KStr("next") -> #(Next, st)
        KStr("previous") -> #(Previous, st)
        KStr(_) ->
          rt_val.t_throw_range_error(st, "direction must be next or previous")
        KHandle(oh) -> {
          let #(dir, st) =
            get_enum_option(
              st,
              Some(oh),
              "direction",
              [#("next", Some(Next)), #("previous", Some(Previous))],
              None,
            )
          case dir {
            Some(d2) -> #(d2, st)
            None -> rt_val.t_throw_range_error(st, "direction is required")
          }
        }
        _ -> rt_val.t_throw_type_error(st, "invalid direction")
      }
      case tz {
        UtcZone | OffsetZone(_) -> #(mk_null(), st)
        IanaZone(zone:) -> {
          let found = case dir {
            Next -> temporal_tz.next_transition_ns(zone, ns)
            Previous -> temporal_tz.previous_transition_ns(zone, ns)
          }
          let in_range = option.map(found, is_valid_epoch_ns) == Some(True)
          case found {
            Some(t_ns) if in_range -> make_zoned_cal(st, protos, t_ns, tz, zcal)
            _ -> #(mk_null(), st)
          }
        }
      }
    }
    ZonedDateTimeToInstant -> make_instant(st, protos, ns)
    ZonedDateTimeToPlainDate -> make_date_cal(st, protos, d, zcal)
    ZonedDateTimeToPlainTime -> make_time(st, protos, t)
    ZonedDateTimeToPlainDateTime -> make_date_time_cal(st, protos, d, t, zcal)
  }
}

fn zoned_until_since(
  st: Agent,
  protos: TemporalProtos,
  cal: temporal_calendar.Calendar,
  a_ns: Int,
  a_tz: TimeZone,
  b_ns: Int,
  b_tz: TimeZone,
  args: List(JsVal),
  is_since is_since: Bool,
) -> #(JsVal, Agent) {
  let #(#(largest, smallest, inc, mode), st) = get_difference_settings(st, args)
  let smallest = option.unwrap(smallest, Nanosecond)
  let largest = option.unwrap(largest, max_unit(smallest, Hour))
  let Nil = check_diff_setup(st, largest, smallest, inc)
  let mode = apply_since_mode(mode, is_since)
  case unit_rank(largest) <= unit_rank(Hour) {
    True -> {
      let smallest_time_unit = rt_val.or_throw(st, require_time_unit(smallest))
      let diff = b_ns - a_ns
      let rounded =
        round_to_increment(diff, inc * time_unit_ns(smallest_time_unit), mode)
      let rounded = apply_since_ns(rounded, is_since)
      make_duration(st, protos, balance_time_ns(rounded, largest))
    }
    False ->
      case time_zone_equals(a_tz, b_tz) {
        False ->
          rt_val.t_throw_range_error(
            st,
            "time zones must be equal for calendar-unit differences",
          )
        True -> {
          let final =
            rt_val.or_throw(
              st,
              diff_date_time_core(
                cal,
                epoch_ns_to_iso_in(a_tz, a_ns),
                epoch_ns_to_iso_in(a_tz, b_ns),
                largest,
                smallest,
                inc,
                mode,
                zoned: True,
              ),
            )
          make_duration(st, protos, apply_since_duration(final, is_since))
        }
      }
  }
}

fn format_zoned(ns: Int, tz: TimeZone, precision: SecondsPrecision) -> String {
  let off = tz_offset_ns_at(tz, ns)
  let #(d, t) = epoch_ns_to_iso(ns, off)
  format_iso_date(d)
  <> "T"
  <> format_iso_time(t, precision)
  <> format_offset_rounded(off)
  <> "["
  <> time_zone_id(tz)
  <> "]"
}
