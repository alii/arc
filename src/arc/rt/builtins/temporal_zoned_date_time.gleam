//// Temporal.ZonedDateTime (proposal-temporal §6): an exact time paired with
//// a resolved time zone and a calendar. Every wall-clock view (getters,
//// toString, with, round) is derived from [[EpochNanoseconds]] through the
//// zone's offset at that instant.
////
//// The zone-aware abstract operations it shares with the other types
//// (ToTemporalZonedDateTime, InterpretISODateTimeOffset, GetStartOfDay, ...)
//// are temporal_zoned_ops.gleam; the difference core is temporal_diff.gleam.

import arc/internal/int_math.{floor_div}
import arc/rt/builtins/helpers
import arc/rt/builtins/temporal_common.{
  Compatible, Hour, Nanosecond, OffsetShowAuto, OffsetShowNever, PreferOffset,
  Trunc, TzAuto, TzCritical, TzNever, UDay, UHour, UMicrosecond, UMillisecond,
  UMinute, UNanosecond, USecond, apply_since_dur, apply_since_mode,
  apply_since_ns, as_if_positive_mode, balance_time_ns, calendar_suffix,
  check_diff_setup, epoch_ns_to_iso_in, format_offset_full,
  format_offset_rounded, get_calendar_name_option, get_difference_settings,
  get_disambiguation_option, get_enum_option, get_fractional_digits,
  get_offset_option, get_options_object, get_overflow_option,
  get_show_offset_option, get_time_zone_name_option, get_unit_option,
  make_date_cal, make_date_time_cal, make_duration, make_instant, make_time,
  make_zoned_cal, max_unit, parse_time_zone_id_strict, require_temporal,
  require_time_unit, round_options, round_to_increment, seconds_string_precision,
  terr, throw_terr, time_only_ns, time_unit_ns, time_zone_equals, time_zone_id,
  to_temporal_time_zone, tz_offset_ns_at, unit_rank, valid_time_increment,
  zoned_slot_of,
}
import arc/rt/builtins/temporal_diff.{diff_date_time_core}
import arc/rt/builtins/temporal_fields.{
  add_sub_args, calendar_date_add, calendar_with_fields, require_nonempty_fields,
  require_partial_bag, to_calendar_arg, to_temporal_calendar_identifier,
}
import arc/rt/builtins/temporal_iso.{
  type Precision, type TErr, AutoPrec, DurRec, RangeE, epoch_days,
  epoch_ns_to_iso, format_iso_date, format_iso_time, int_sign,
  iso_date_from_epoch_days, ns_div_float, ns_max_instant, ns_per_day,
  ns_per_hour, ns_per_ms, zero_dur,
}
import arc/rt/builtins/temporal_plain_date.{date_field_cal, date_getter_name}
import arc/rt/builtins/temporal_plain_time.{
  regulate_time, time_field, time_fields_apply, time_getter_name,
  to_temporal_time,
}
import arc/rt/builtins/temporal_tz
import arc/rt/builtins/temporal_zoned_ops.{
  OptionOffset, date_time_fields_all_none, get_epoch_ns_for, interpret_offset,
  read_date_time_fields, start_of_day_ns, to_temporal_zoned, unloadable_tz,
}
import arc/rt/types.{
  type Agent, type JsVal, type NativeToken, type TemporalProtos,
  type TemporalStaticName, type TemporalZonedGetter, type TimeZone,
  type ZonedDateTimeMethod, DgCalendarId, DgDay, DgDayOfWeek, DgDayOfYear,
  DgDaysInMonth, DgDaysInWeek, DgDaysInYear, DgEra, DgEraYear, DgInLeapYear,
  DgMonth, DgMonthCode, DgMonthsInYear, DgWeekOfYear, DgYear, DgYearOfWeek,
  JFloat, JInt, KHandle, KStr, KUndef, TemporalN, TemporalZonedDateTimeCtor,
  TemporalZonedDateTimeGetter, TemporalZonedDateTimeMethod,
  TemporalZonedDateTimeStatic, TgHour, TgMicrosecond, TgMillisecond, TgMinute,
  TgNanosecond, TgSecond, TsCompare, TsFrom, TzNamed, TzOffset, TzUtc, ZgDate,
  ZgEpochMilliseconds, ZgEpochNanoseconds, ZgHoursInDay, ZgOffset,
  ZgOffsetNanoseconds, ZgTime, ZgTimeZoneId, ZmAdd, ZmEquals,
  ZmGetTimeZoneTransition, ZmRound, ZmSince, ZmStartOfDay, ZmSubtract,
  ZmToInstant, ZmToJson, ZmToLocaleString, ZmToPlainDate, ZmToPlainDateTime,
  ZmToPlainTime, ZmToString, ZmUntil, ZmValueOf, ZmWith, ZmWithCalendar,
  ZmWithPlainTime, ZmWithTimeZone, classify, mk_bigint, mk_bool, mk_null,
  mk_number, mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import arc/vm/internal/temporal_calendar as tcal
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result

// ============================================================================
// Init — Temporal.ZonedDateTime constructor + prototype
// ============================================================================

/// The getters, in prototype-registration order.
const all_getters = [
  ZgDate(DgCalendarId),
  ZgTimeZoneId,
  ZgDate(DgEra),
  ZgDate(DgEraYear),
  ZgDate(DgYear),
  ZgDate(DgMonth),
  ZgDate(DgMonthCode),
  ZgDate(DgDay),
  ZgTime(TgHour),
  ZgTime(TgMinute),
  ZgTime(TgSecond),
  ZgTime(TgMillisecond),
  ZgTime(TgMicrosecond),
  ZgTime(TgNanosecond),
  ZgEpochMilliseconds,
  ZgEpochNanoseconds,
  ZgDate(DgDayOfWeek),
  ZgDate(DgDayOfYear),
  ZgDate(DgWeekOfYear),
  ZgDate(DgYearOfWeek),
  ZgHoursInDay,
  ZgDate(DgDaysInWeek),
  ZgDate(DgDaysInMonth),
  ZgDate(DgDaysInYear),
  ZgDate(DgMonthsInYear),
  ZgDate(DgInLeapYear),
  ZgOffsetNanoseconds,
  ZgOffset,
]

/// The pieces `temporal.init_temporal_type` builds Temporal.ZonedDateTime
/// from: the constructor token, `from`/`compare`, the getters and the
/// prototype methods (in registration order).
pub fn ctor_token(protos: TemporalProtos) -> NativeToken {
  TemporalN(TemporalZonedDateTimeCtor(protos:))
}

pub fn statics(protos: TemporalProtos) -> List(#(String, NativeToken, Int)) {
  list.map([#(TsFrom, 1), #(TsCompare, 2)], fn(s) {
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
      #(ZmWithTimeZone, 1),
      #(ZmWithCalendar, 1),
      #(ZmWithPlainTime, 0),
      #(ZmWith, 1),
      #(ZmAdd, 1),
      #(ZmSubtract, 1),
      #(ZmUntil, 1),
      #(ZmSince, 1),
      #(ZmRound, 1),
      #(ZmEquals, 1),
      #(ZmToString, 0),
      #(ZmToLocaleString, 0),
      #(ZmToJson, 0),
      #(ZmValueOf, 0),
      #(ZmStartOfDay, 0),
      #(ZmGetTimeZoneTransition, 1),
      #(ZmToInstant, 0),
      #(ZmToPlainDate, 0),
      #(ZmToPlainTime, 0),
      #(ZmToPlainDateTime, 0),
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

fn static_name(s: TemporalStaticName) -> String {
  case s {
    TsFrom -> "from"
    TsCompare -> "compare"
  }
}

pub fn zoned_getter_name(g: TemporalZonedGetter) -> String {
  case g {
    ZgTimeZoneId -> "timeZoneId"
    ZgEpochMilliseconds -> "epochMilliseconds"
    ZgEpochNanoseconds -> "epochNanoseconds"
    ZgOffsetNanoseconds -> "offsetNanoseconds"
    ZgOffset -> "offset"
    ZgHoursInDay -> "hoursInDay"
    ZgDate(dg) -> date_getter_name(dg)
    ZgTime(tg) -> time_getter_name(tg)
  }
}

pub fn method_name(m: ZonedDateTimeMethod) -> String {
  case m {
    ZmWithTimeZone -> "withTimeZone"
    ZmWithCalendar -> "withCalendar"
    ZmWithPlainTime -> "withPlainTime"
    ZmWith -> "with"
    ZmAdd -> "add"
    ZmSubtract -> "subtract"
    ZmUntil -> "until"
    ZmSince -> "since"
    ZmRound -> "round"
    ZmEquals -> "equals"
    ZmToString -> "toString"
    ZmToLocaleString -> "toLocaleString"
    ZmToJson -> "toJSON"
    ZmValueOf -> "valueOf"
    ZmStartOfDay -> "startOfDay"
    ZmGetTimeZoneTransition -> "getTimeZoneTransition"
    ZmToInstant -> "toInstant"
    ZmToPlainDate -> "toPlainDate"
    ZmToPlainTime -> "toPlainTime"
    ZmToPlainDateTime -> "toPlainDateTime"
  }
}

// ============================================================================
// Constructor and statics
// ============================================================================

/// new Temporal.ZonedDateTime(epochNanoseconds: BigInt, timeZone [, calendar])
/// — the value before OrdinaryCreateFromConstructor re-points its prototype.
pub fn ctor(
  st: Agent,
  protos: TemporalProtos,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(ns, st) = rt_val.t_to_bigint(st, helpers.arg_at(args, 0))
  case classify(helpers.arg_at(args, 1)) {
    KStr(tz_str) -> {
      // Only bare identifiers: an ISO date-time string is not a zone here.
      let tz =
        terr(st, case parse_time_zone_id_strict(tz_str) {
          Ok(tz) -> Ok(tz)
          Error(temporal_common.StrictUnknown) ->
            Error(RangeE("invalid time zone identifier: " <> tz_str))
          Error(temporal_common.StrictInvalid(e)) -> Error(e)
        })
      let cal = terr(st, to_calendar_arg(helpers.arg_at(args, 2)))
      case int.absolute_value(ns) <= ns_max_instant {
        False ->
          rt_val.t_throw_range_error(st, "epoch nanoseconds out of range")
        True -> make_zoned_cal(st, protos, ns, tz, cal)
      }
    }
    _ -> rt_val.t_throw_type_error(st, "time zone must be a string")
  }
}

/// Temporal.ZonedDateTime.from(item [, options]) / .compare(one, two).
pub fn static(
  st: Agent,
  name: TemporalStaticName,
  protos: TemporalProtos,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case name {
    TsFrom -> {
      let #(#(ns, tz, cal), st) =
        to_temporal_zoned(st, helpers.arg_at(args, 0), helpers.arg_at(args, 1))
      make_zoned_cal(st, protos, ns, tz, cal)
    }
    TsCompare -> {
      let #(#(a, _, _), st) =
        to_temporal_zoned(st, helpers.arg_at(args, 0), mk_undefined())
      let #(#(b, _, _), st) =
        to_temporal_zoned(st, helpers.arg_at(args, 1), mk_undefined())
      #(mk_number(JInt(int_sign(a - b))), st)
    }
  }
}

// ============================================================================
// Getters
// ============================================================================

fn require_zoned(
  st: Agent,
  this: JsVal,
  name: String,
) -> #(Int, TimeZone, tcal.Calendar) {
  require_temporal(st, this, "ZonedDateTime", name, zoned_slot_of)
}

pub fn getter(
  st: Agent,
  g: TemporalZonedGetter,
  this: JsVal,
) -> #(JsVal, Agent) {
  let #(ns, tz, zcal) = require_zoned(st, this, zoned_getter_name(g))
  let offset = terr(st, tz_offset_ns_at(tz, ns))
  let #(d, t) = epoch_ns_to_iso(ns, offset)
  case g {
    ZgTimeZoneId -> #(mk_string(time_zone_id(tz)), st)
    ZgEpochMilliseconds -> #(mk_number(JInt(floor_div(ns, ns_per_ms))), st)
    ZgEpochNanoseconds -> #(mk_bigint(ns), st)
    ZgOffsetNanoseconds -> #(mk_number(JInt(offset)), st)
    ZgOffset -> #(mk_string(format_offset_full(offset)), st)
    ZgHoursInDay -> {
      let tomorrow = iso_date_from_epoch_days(epoch_days(d) + 1)
      let s1 = terr(st, start_of_day_ns(tz, d))
      let s2 = terr(st, start_of_day_ns(tz, tomorrow))
      #(mk_number(JFloat(ns_div_float(s2 - s1, ns_per_hour))), st)
    }
    ZgTime(tg) -> #(time_field(t, tg), st)
    ZgDate(dg) -> #(date_field_cal(zcal, d, dg), st)
  }
}

// ============================================================================
// Methods
// ============================================================================

/// direction argument of ZonedDateTime.prototype.getTimeZoneTransition.
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
  let off = terr(st, tz_offset_ns_at(tz, ns))
  let #(d, t) = epoch_ns_to_iso(ns, off)
  case m {
    ZmToJson | ZmToLocaleString -> #(
      mk_string(terr(st, format_zoned(ns, tz, AutoPrec))),
      st,
    )
    ZmToString -> {
      // Read order: calendarName, fractionalSecondDigits, offset,
      // roundingMode, smallestUnit, timeZoneName; validate after.
      let #(#(cal_name, opts), st) =
        get_calendar_name_option(st, helpers.arg_at(args, 0))
      let #(digits, st) = get_fractional_digits(st, opts)
      let #(offset_mode, st) = get_show_offset_option(st, opts)
      let #(mode, st) =
        temporal_common.get_rounding_mode_option(st, opts, Trunc)
      let #(su_opt, st) =
        get_unit_option(st, opts, "smallestUnit", allow_auto: False)
      let #(tz_mode, st) = get_time_zone_name_option(st, opts)
      let #(prec, su, sinc, mode) =
        terr(st, seconds_string_precision(digits, su_opt, mode))
      let rounded = case su {
        None -> ns
        Some(u) ->
          round_to_increment(
            ns,
            sinc * time_unit_ns(u),
            as_if_positive_mode(mode),
          )
      }
      let off2 = terr(st, tz_offset_ns_at(tz, rounded))
      let #(d2, t2) = epoch_ns_to_iso(rounded, off2)
      let base = format_iso_date(d2) <> "T" <> format_iso_time(t2, prec)
      let with_offset = case offset_mode {
        OffsetShowNever -> base
        OffsetShowAuto -> base <> format_offset_rounded(off2)
      }
      let with_tz = case tz_mode {
        TzNever -> with_offset
        TzCritical -> with_offset <> "[!" <> time_zone_id(tz) <> "]"
        TzAuto -> with_offset <> "[" <> time_zone_id(tz) <> "]"
      }
      #(mk_string(with_tz <> calendar_suffix(cal_name, zcal)), st)
    }
    ZmValueOf ->
      rt_val.t_throw_type_error(
        st,
        "Temporal.ZonedDateTime cannot be converted with valueOf",
      )
    ZmEquals -> {
      let #(#(ons, otz, ocal), st) =
        to_temporal_zoned(st, helpers.arg_at(args, 0), mk_undefined())
      #(mk_bool(ns == ons && time_zone_equals(tz, otz) && zcal == ocal), st)
    }
    ZmAdd | ZmSubtract -> {
      let #(dur, overflow, st) = add_sub_args(st, args, m == ZmSubtract)
      // Add date part in local wall-clock space, then exact time. Pure
      // time-unit durations add directly to the epoch (AddZonedDateTime).
      let date_dur =
        DurRec(
          ..zero_dur,
          years: dur.years,
          months: dur.months,
          weeks: dur.weeks,
          days: dur.days,
        )
      let base_ns =
        terr(
          st,
          case
            dur.years == 0 && dur.months == 0 && dur.weeks == 0 && dur.days == 0
          {
            True -> Ok(ns)
            False -> {
              use d2 <- result.try(calendar_date_add(
                zcal,
                d,
                date_dur,
                overflow,
              ))
              get_epoch_ns_for(tz, d2, t, Compatible)
            }
          },
        )
      let ns2 = base_ns + time_only_ns(dur)
      case int.absolute_value(ns2) <= ns_max_instant {
        False -> rt_val.t_throw_range_error(st, "instant outside valid range")
        True -> make_zoned_cal(st, protos, ns2, tz, zcal)
      }
    }
    ZmWithTimeZone -> {
      let #(tz2, st) = to_temporal_time_zone(st, helpers.arg_at(args, 0))
      make_zoned_cal(st, protos, ns, tz2, zcal)
    }
    ZmUntil | ZmSince -> {
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
            m == ZmSince,
          )
      }
    }
    ZmRound -> {
      let #(#(su, inc, mode), st) =
        round_options(st, helpers.arg_at(args, 0), allow_day: True)
      let u_ns = time_unit_ns(su)
      let max = case su {
        UDay -> 1
        UHour -> 24
        UMinute | USecond -> 60
        UMillisecond | UMicrosecond | UNanosecond -> 1000
      }
      case valid_time_increment(inc, max) {
        False -> rt_val.t_throw_range_error(st, "invalid roundingIncrement")
        True -> {
          let local = ns + off
          let day_part = floor_div(local, ns_per_day)
          let local_date = iso_date_from_epoch_days(day_part)
          case su == UDay {
            // Round within the day bounded by start-of-day instants;
            // both bounds must be representable.
            True -> {
              let day_start = terr(st, start_of_day_ns(tz, local_date))
              let day_end =
                terr(
                  st,
                  start_of_day_ns(tz, iso_date_from_epoch_days(day_part + 1)),
                )
              let ns2 =
                day_start
                + round_to_increment(ns - day_start, day_end - day_start, mode)
              make_zoned_cal(st, protos, ns2, tz, zcal)
            }
            False -> {
              // Round the wall-clock time of day (RoundISODateTime),
              // then reinterpret preferring the current offset.
              let tod = local - day_part * ns_per_day
              let rounded_tod = round_to_increment(tod, inc * u_ns, mode)
              let #(rd, rt) =
                epoch_ns_to_iso(day_part * ns_per_day + rounded_tod, 0)
              let ns2 =
                terr(
                  st,
                  interpret_offset(
                    rd,
                    rt,
                    OptionOffset(off),
                    tz,
                    Compatible,
                    PreferOffset,
                    False,
                  ),
                )
              case int.absolute_value(ns2) <= ns_max_instant {
                False ->
                  rt_val.t_throw_range_error(st, "instant outside valid range")
                True -> make_zoned_cal(st, protos, ns2, tz, zcal)
              }
            }
          }
        }
      }
    }
    ZmWith -> {
      let #(bag, st) = require_partial_bag(st, helpers.arg_at(args, 0))
      let #(f, st) =
        read_date_time_fields(st, bag, zcal, read_offset: True, read_tz: False)
      let Nil = require_nonempty_fields(st, date_time_fields_all_none(f))
      let #(opts, st) = get_options_object(st, helpers.arg_at(args, 1))
      let #(dis_opt, st) = get_disambiguation_option(st, opts)
      let #(off_opt, st) = get_offset_option(st, opts, PreferOffset)
      let #(overflow, st) = get_overflow_option(st, opts)
      let date = terr(st, calendar_with_fields(zcal, d, f.date, overflow))
      let t0 = time_fields_apply(f.time, t)
      let t2 = terr(st, regulate_time(t0, overflow))
      let ns2 =
        terr(
          st,
          interpret_offset(
            date,
            t2,
            OptionOffset(option.unwrap(f.offset, off)),
            tz,
            dis_opt,
            off_opt,
            False,
          ),
        )
      make_zoned_cal(st, protos, ns2, tz, zcal)
    }
    ZmWithCalendar -> {
      let #(new_cal, st) =
        to_temporal_calendar_identifier(st, helpers.arg_at(args, 0))
      make_zoned_cal(st, protos, ns, tz, new_cal)
    }
    ZmWithPlainTime -> {
      // Undefined → GetStartOfDay; an explicit time (even midnight) uses
      // compatible disambiguation. These differ when midnight is skipped.
      let arg = helpers.arg_at(args, 0)
      case classify(arg) {
        KUndef -> {
          let ns2 = terr(st, start_of_day_ns(tz, d))
          make_zoned_cal(st, protos, ns2, tz, zcal)
        }
        _ -> {
          let #(t2, st) = to_temporal_time(st, arg, mk_undefined())
          let ns2 = terr(st, get_epoch_ns_for(tz, d, t2, Compatible))
          make_zoned_cal(st, protos, ns2, tz, zcal)
        }
      }
    }
    ZmStartOfDay -> {
      let ns2 = terr(st, start_of_day_ns(tz, d))
      make_zoned_cal(st, protos, ns2, tz, zcal)
    }
    ZmGetTimeZoneTransition -> {
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
      // UTC and offset zones have no transitions.
      case tz {
        TzUtc | TzOffset(_) -> #(mk_null(), st)
        TzNamed(zone:) -> {
          let found = case dir {
            Next -> temporal_tz.next_transition_ns(zone, ns)
            Previous -> temporal_tz.prev_transition_ns(zone, ns)
          }
          case found {
            // No further transition, or one outside the instant range.
            Ok(None) -> #(mk_null(), st)
            Ok(Some(t_ns)) ->
              case int.absolute_value(t_ns) <= ns_max_instant {
                True -> make_zoned_cal(st, protos, t_ns, tz, zcal)
                False -> #(mk_null(), st)
              }
            // Broken zoneinfo is not "no transition" — report it.
            Error(err) -> throw_terr(st, unloadable_tz(tz, err))
          }
        }
      }
    }
    ZmToInstant -> make_instant(st, protos, ns)
    ZmToPlainDate -> make_date_cal(st, protos, d, zcal)
    ZmToPlainTime -> make_time(st, protos, t)
    ZmToPlainDateTime -> make_date_time_cal(st, protos, d, t, zcal)
  }
}

/// ZonedDateTime.prototype.until/since (DifferenceTemporalZonedDateTime).
fn zoned_until_since(
  st: Agent,
  protos: TemporalProtos,
  cal: tcal.Calendar,
  a_ns: Int,
  a_tz: TimeZone,
  b_ns: Int,
  b_tz: TimeZone,
  args: List(JsVal),
  is_since: Bool,
) -> #(JsVal, Agent) {
  let #(#(largest, smallest, inc, mode), st) = get_difference_settings(st, args)
  let smallest = option.unwrap(smallest, Nanosecond)
  let largest = option.unwrap(largest, max_unit(smallest, Hour))
  let Nil = check_diff_setup(st, largest, smallest, inc)
  let mode2 = apply_since_mode(mode, is_since)
  case unit_rank(largest) <= unit_rank(Hour) {
    True -> {
      // Exact-time difference, like Instant.
      let su = terr(st, require_time_unit(smallest))
      let diff = b_ns - a_ns
      let rounded = round_to_increment(diff, inc * time_unit_ns(su), mode2)
      let rounded = apply_since_ns(rounded, is_since)
      make_duration(st, protos, balance_time_ns(rounded, largest))
    }
    False ->
      // Calendar-unit difference requires equal time zones (§ spec:
      // TimeZoneEquals, RangeError otherwise).
      case time_zone_equals(a_tz, b_tz) {
        False ->
          rt_val.t_throw_range_error(
            st,
            "time zones must be equal for calendar-unit differences",
          )
        True -> {
          let a_dt = terr(st, epoch_ns_to_iso_in(a_tz, a_ns))
          let b_dt = terr(st, epoch_ns_to_iso_in(a_tz, b_ns))
          let final =
            terr(
              st,
              diff_date_time_core(
                cal,
                a_dt,
                b_dt,
                largest,
                smallest,
                inc,
                mode2,
                True,
              ),
            )
          make_duration(st, protos, apply_since_dur(final, is_since))
        }
      }
  }
}

/// TemporalZonedDateTimeToString with the default options: rounded offset,
/// bracketed zone, no calendar suffix.
fn format_zoned(
  ns: Int,
  tz: TimeZone,
  prec: Precision,
) -> Result(String, TErr) {
  use off <- result.map(tz_offset_ns_at(tz, ns))
  let #(d, t) = epoch_ns_to_iso(ns, off)
  format_iso_date(d)
  <> "T"
  <> format_iso_time(t, prec)
  <> format_offset_rounded(off)
  <> "["
  <> time_zone_id(tz)
  <> "]"
}
