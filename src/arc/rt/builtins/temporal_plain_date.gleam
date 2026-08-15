//// Temporal.PlainDate (proposal-temporal §3): a calendar date with no time
//// or time zone, held as its ISO 8601 date plus the calendar it is expressed
//// in.
////
//// Also owns the date abstract operations the other calendared types share:
//// ToTemporalDate, the date property bag, and the calendar-aware date field
//// getters (PlainDateTime and ZonedDateTime register the same sixteen).

import arc/internal/gregorian.{
  days_in_month, days_in_year as days_in_iso_year, is_leap_year,
}
import arc/rt/builtins/helpers
import arc/rt/builtins/temporal_common.{
  CalAuto, Compatible, Day, apply_since_dur, apply_since_mode, calendar_suffix,
  date_slot_of, epoch_ns_to_iso_in, get_calendar_name_option,
  get_difference_settings, make_date_cal, make_date_time_cal, make_duration,
  make_month_day_cal, make_year_month_cal, make_zoned_cal, max_unit,
  parse_time_zone_id, require_largest_ge_smallest, require_temporal,
  temporal_data_of, terr, unit_rank,
} as tc
import arc/rt/builtins/temporal_diff.{difference_calendar_date}
import arc/rt/builtins/temporal_fields.{
  add_sub_args, calendar_date_add, calendar_with_fields, compare_iso_date,
  era_field, era_year_field, get_named, int_val, month_code_str,
  month_day_reference_iso, no_date_fields, parse_plain_datetime_string,
  parsed_calendar_id, read_bag_calendar, read_date_fields,
  require_nonempty_fields, require_partial_bag, resolve_calendar_date,
  to_calendar_arg, to_temporal_calendar_identifier, validated_overflow,
}
import arc/rt/builtins/temporal_iso.{
  type IsoDate, Constrain, IsoDate, day_of_week, day_of_year, epoch_days,
  format_iso_date, is_valid_iso_date, iso_date_from_epoch_days,
  iso_date_within_limits, midnight, week_of_year,
}
import arc/rt/builtins/temporal_plain_time.{to_temporal_time}
import arc/rt/builtins/temporal_zoned_ops.{get_epoch_ns_for, start_of_day_ns}
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type NativeToken, type PlainDateMethod,
  type TemporalDateGetter, type TemporalProtos, type TemporalStaticName,
  DgCalendarId, DgDay, DgDayOfWeek, DgDayOfYear, DgDaysInMonth, DgDaysInWeek,
  DgDaysInYear, DgEra, DgEraYear, DgInLeapYear, DgMonth, DgMonthCode,
  DgMonthsInYear, DgWeekOfYear, DgYear, DgYearOfWeek, KHandle, KStr, KUndef,
  PdAdd, PdEquals, PdSince, PdSubtract, PdToJson, PdToLocaleString,
  PdToPlainDateTime, PdToPlainMonthDay, PdToPlainYearMonth, PdToString,
  PdToZonedDateTime, PdUntil, PdValueOf, PdWith, PdWithCalendar, TemporalDate,
  TemporalDateTime, TemporalN, TemporalPlainDateCtor, TemporalPlainDateGetter,
  TemporalPlainDateMethod, TemporalPlainDateStatic, TemporalZonedDateTime,
  TsCompare, TsFrom, classify, mk_bool, mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import arc/vm/internal/temporal_calendar as tcal
import gleam/list
import gleam/option

// ============================================================================
// Init — Temporal.PlainDate constructor + prototype
// ============================================================================

/// The date getters, in prototype-registration order. Kept as values (never
/// strings) so `date_getter_name` is the only place a JS-facing name is
/// written down. PlainDateTime and ZonedDateTime register the same set.
pub const all_date_getters = [
  DgCalendarId,
  DgEra,
  DgEraYear,
  DgYear,
  DgMonth,
  DgMonthCode,
  DgDay,
  DgDayOfWeek,
  DgDayOfYear,
  DgWeekOfYear,
  DgYearOfWeek,
  DgDaysInWeek,
  DgDaysInMonth,
  DgDaysInYear,
  DgMonthsInYear,
  DgInLeapYear,
]

/// Registration specs for `temporal.init_temporal_type`: the constructor
/// token, `from`/`compare`, the sixteen getters and the prototype methods, in
/// prototype-registration order.
pub fn ctor_token(protos: TemporalProtos) -> NativeToken {
  TemporalN(TemporalPlainDateCtor(protos:))
}

pub fn statics(protos: TemporalProtos) -> List(#(String, NativeToken, Int)) {
  list.map([#(TsFrom, 1), #(TsCompare, 2)], fn(s) {
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
      #(PdToPlainYearMonth, 0),
      #(PdToPlainMonthDay, 0),
      #(PdToPlainDateTime, 0),
      #(PdToZonedDateTime, 1),
      #(PdAdd, 1),
      #(PdSubtract, 1),
      #(PdWith, 1),
      #(PdWithCalendar, 1),
      #(PdUntil, 1),
      #(PdSince, 1),
      #(PdEquals, 1),
      #(PdToString, 0),
      #(PdToLocaleString, 0),
      #(PdToJson, 0),
      #(PdValueOf, 0),
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

fn static_name(s: TemporalStaticName) -> String {
  case s {
    TsFrom -> "from"
    TsCompare -> "compare"
  }
}

/// JS-facing name of a date getter — shared by PlainDateTime and
/// ZonedDateTime, which register the same sixteen.
pub fn date_getter_name(g: TemporalDateGetter) -> String {
  case g {
    DgCalendarId -> "calendarId"
    DgEra -> "era"
    DgEraYear -> "eraYear"
    DgYear -> "year"
    DgMonth -> "month"
    DgMonthCode -> "monthCode"
    DgDay -> "day"
    DgDayOfWeek -> "dayOfWeek"
    DgDayOfYear -> "dayOfYear"
    DgWeekOfYear -> "weekOfYear"
    DgYearOfWeek -> "yearOfWeek"
    DgDaysInWeek -> "daysInWeek"
    DgDaysInMonth -> "daysInMonth"
    DgDaysInYear -> "daysInYear"
    DgMonthsInYear -> "monthsInYear"
    DgInLeapYear -> "inLeapYear"
  }
}

pub fn plain_date_method_name(m: PlainDateMethod) -> String {
  case m {
    PdToPlainYearMonth -> "toPlainYearMonth"
    PdToPlainMonthDay -> "toPlainMonthDay"
    PdToPlainDateTime -> "toPlainDateTime"
    PdToZonedDateTime -> "toZonedDateTime"
    PdAdd -> "add"
    PdSubtract -> "subtract"
    PdWith -> "with"
    PdWithCalendar -> "withCalendar"
    PdUntil -> "until"
    PdSince -> "since"
    PdEquals -> "equals"
    PdToString -> "toString"
    PdToLocaleString -> "toLocaleString"
    PdToJson -> "toJSON"
    PdValueOf -> "valueOf"
  }
}

// ============================================================================
// Constructor and statics
// ============================================================================

/// new Temporal.PlainDate(year, month, day [, calendar]).
/// The caller applies NewTarget's prototype (OrdinaryCreateFromConstructor).
pub fn ctor(
  st: Agent,
  protos: TemporalProtos,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(y, st) = tc.arg_trunc_int(st, args, 0)
  let #(m, st) = tc.arg_trunc_int(st, args, 1)
  let #(d, st) = tc.arg_trunc_int(st, args, 2)
  let cal = terr(st, to_calendar_arg(helpers.arg_at(args, 3)))
  case is_valid_iso_date(y, m, d) {
    False -> rt_val.t_throw_range_error(st, "invalid ISO date")
    True -> {
      let date = IsoDate(y, m, d)
      case iso_date_within_limits(date) {
        False ->
          rt_val.t_throw_range_error(st, "date outside of supported range")
        True -> make_date_cal(st, protos, date, cal)
      }
    }
  }
}

/// Temporal.PlainDate.from / compare.
pub fn static(
  st: Agent,
  name: TemporalStaticName,
  protos: TemporalProtos,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case name {
    TsFrom -> {
      let #(#(d, cal), st) =
        to_temporal_date(st, helpers.arg_at(args, 0), helpers.arg_at(args, 1))
      make_date_cal(st, protos, d, cal)
    }
    TsCompare -> {
      let #(#(a, _), st) =
        to_temporal_date(st, helpers.arg_at(args, 0), mk_undefined())
      let #(#(b, _), st) =
        to_temporal_date(st, helpers.arg_at(args, 1), mk_undefined())
      #(int_val(compare_iso_date(a, b)), st)
    }
  }
}

// ============================================================================
// ToTemporalDate
// ============================================================================

/// ToTemporalDate(item [, options]) — returns the ISO date + calendar.
/// Reads + validates options AFTER item conversion, per spec order.
pub fn to_temporal_date(
  st: Agent,
  item: JsVal,
  options: JsVal,
) -> #(#(IsoDate, tcal.Calendar), Agent) {
  case classify(item) {
    KHandle(h) ->
      case temporal_data_of(st, item) {
        option.Some(TemporalDate(year:, month:, day:, calendar:))
        | option.Some(TemporalDateTime(year:, month:, day:, calendar:, ..)) -> {
          let #(_opts, st) = validated_overflow(st, options)
          #(#(IsoDate(year, month, day), calendar), st)
        }
        option.Some(TemporalZonedDateTime(epoch_ns:, time_zone:, calendar:)) -> {
          let #(_opts, st) = validated_overflow(st, options)
          let #(d, _) = terr(st, epoch_ns_to_iso_in(time_zone, epoch_ns))
          #(#(d, calendar), st)
        }
        _ -> date_from_bag(st, h, options)
      }
    KStr(s) -> {
      let p = terr(st, parse_plain_datetime_string(s))
      let cal = terr(st, parsed_calendar_id(p))
      let #(_opts, st) = validated_overflow(st, options)
      case iso_date_within_limits(p.date) {
        True -> #(#(p.date, cal), st)
        False ->
          rt_val.t_throw_range_error(st, "date outside of supported range")
      }
    }
    _ -> rt_val.t_throw_type_error(st, "cannot convert to a Temporal.PlainDate")
  }
}

/// Property-bag → ISO date + calendar. Field read order: calendar, then
/// alphabetical (day, era, eraYear, month, monthCode, year).
pub fn date_from_bag(
  st: Agent,
  h: Handle,
  options: JsVal,
) -> #(#(IsoDate, tcal.Calendar), Agent) {
  let #(cal, st) = read_bag_calendar(st, h)
  let #(fields, st) = read_date_fields(st, h, cal)
  let #(overflow, st) = validated_overflow(st, options)
  let date = terr(st, resolve_calendar_date(cal, fields, overflow))
  case iso_date_within_limits(date) {
    True -> #(#(date, cal), st)
    False -> rt_val.t_throw_range_error(st, "date outside of supported range")
  }
}

// ============================================================================
// Getters
// ============================================================================

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
    DgCalendarId -> mk_string("iso8601")
    DgEra -> mk_undefined()
    DgEraYear -> mk_undefined()
    DgYear -> int_val(d.year)
    DgMonth -> int_val(d.month)
    DgMonthCode -> mk_string(month_code_str(d.month))
    DgDay -> int_val(d.day)
    DgDayOfWeek -> int_val(day_of_week(d))
    DgDayOfYear -> int_val(day_of_year(d))
    DgWeekOfYear -> int_val(week_of_year(d).0)
    DgYearOfWeek -> int_val(week_of_year(d).1)
    DgDaysInWeek -> int_val(7)
    DgDaysInMonth -> int_val(days_in_month(d.year, d.month))
    DgDaysInYear -> int_val(days_in_iso_year(d.year))
    DgMonthsInYear -> int_val(12)
    DgInLeapYear -> mk_bool(is_leap_year(d.year))
  }
}

/// Calendar-aware date field getter (ISO dates fall through to date_field).
pub fn date_field_cal(
  cal: tcal.Calendar,
  d: IsoDate,
  g: TemporalDateGetter,
) -> JsVal {
  case cal {
    tcal.Iso8601 -> date_field(d, g)
    _ -> {
      let cd = tcal.date_from_epoch_days(cal, epoch_days(d))
      case g {
        DgCalendarId -> mk_string(tcal.identifier(cal))
        DgEra -> era_field(cal, cd)
        DgEraYear -> era_year_field(cal, cd)
        DgYear -> int_val(cd.year)
        DgMonth -> int_val(cd.month)
        DgMonthCode -> mk_string(tcal.month_code(cal, cd.year, cd.month))
        DgDay -> int_val(cd.day)
        DgDayOfWeek -> int_val(day_of_week(d))
        DgDayOfYear -> int_val(tcal.day_of_year(cal, cd.year, cd.month, cd.day))
        // weekOfYear/yearOfWeek are undefined for non-ISO calendars.
        DgWeekOfYear -> mk_undefined()
        DgYearOfWeek -> mk_undefined()
        DgDaysInWeek -> int_val(7)
        DgDaysInMonth -> int_val(tcal.days_in_month(cal, cd.year, cd.month))
        DgDaysInYear -> int_val(tcal.days_in_year(cal, cd.year))
        DgMonthsInYear -> int_val(tcal.months_in_year(cal, cd.year))
        DgInLeapYear -> mk_bool(tcal.in_leap_year(cal, cd.year))
      }
    }
  }
}

// ============================================================================
// Methods
// ============================================================================

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
    PdToJson | PdToLocaleString -> #(
      mk_string(format_iso_date(d) <> calendar_suffix(CalAuto, cal)),
      st,
    )
    PdToString -> {
      let #(#(cal_name, _), st) =
        get_calendar_name_option(st, helpers.arg_at(args, 0))
      #(mk_string(format_iso_date(d) <> calendar_suffix(cal_name, cal)), st)
    }
    PdValueOf ->
      rt_val.t_throw_type_error(
        st,
        "Temporal.PlainDate cannot be converted with valueOf; use compare() or equals()",
      )
    PdEquals -> {
      let #(#(other, other_cal), st) =
        to_temporal_date(st, helpers.arg_at(args, 0), mk_undefined())
      #(mk_bool(d == other && cal == other_cal), st)
    }
    PdAdd | PdSubtract -> {
      let #(dur, overflow, st) = add_sub_args(st, args, m == PdSubtract)
      let d2 = terr(st, calendar_date_add(cal, d, dur, overflow))
      make_date_cal(st, protos, d2, cal)
    }
    PdWith -> {
      let #(bag, st) = require_partial_bag(st, helpers.arg_at(args, 0))
      let #(fields, st) = read_date_fields(st, bag, cal)
      let Nil = require_nonempty_fields(st, fields == no_date_fields)
      let #(overflow, st) = validated_overflow(st, helpers.arg_at(args, 1))
      let date = terr(st, calendar_with_fields(cal, d, fields, overflow))
      let date = terr(st, temporal_iso.check_date_limits(date))
      make_date_cal(st, protos, date, cal)
    }
    PdWithCalendar -> {
      let #(new_cal, st) =
        to_temporal_calendar_identifier(st, helpers.arg_at(args, 0))
      make_date_cal(st, protos, d, new_cal)
    }
    PdToPlainDateTime -> {
      let #(t, st) = optional_time_arg(st, helpers.arg_at(args, 0))
      make_date_time_cal(st, protos, d, t, cal)
    }
    PdToPlainYearMonth -> {
      let #(ymy, ymm, ymd) = case cal {
        tcal.Iso8601 -> #(d.year, d.month, 1)
        _ -> {
          let cd = tcal.date_from_epoch_days(cal, epoch_days(d))
          let first =
            iso_date_from_epoch_days(tcal.date_to_epoch_days(
              cal,
              cd.year,
              cd.month,
              1,
            ))
          #(first.year, first.month, first.day)
        }
      }
      make_year_month_cal(st, protos, ymy, ymm, ymd, cal)
    }
    PdToPlainMonthDay ->
      case cal {
        tcal.Iso8601 ->
          make_month_day_cal(st, protos, d.month, d.day, 1972, cal)
        _ -> {
          let cd = tcal.date_from_epoch_days(cal, epoch_days(d))
          let mc = tcal.month_code_of(cal, cd.year, cd.month)
          let iso =
            terr(st, month_day_reference_iso(cal, mc, cd.day, Constrain))
          make_month_day_cal(st, protos, iso.month, iso.day, iso.year, cal)
        }
      }
    PdToZonedDateTime -> {
      // Argument: a time zone string, or an object with a timeZone
      // property (plus optional plainTime).
      let arg = helpers.arg_at(args, 0)
      case classify(arg) {
        KStr(tz_str) -> {
          let tz = terr(st, parse_time_zone_id(tz_str))
          let ns = terr(st, start_of_day_ns(tz, d))
          make_zoned_cal(st, protos, ns, tz, cal)
        }
        KHandle(oh) -> {
          let #(tz_val, st) = get_named(st, oh, "timeZone")
          case classify(tz_val) {
            KUndef -> rt_val.t_throw_type_error(st, "time zone is required")
            KStr(tz_str) -> {
              let tz = terr(st, parse_time_zone_id(tz_str))
              let #(pt_val, st) = get_named(st, oh, "plainTime")
              case classify(pt_val) {
                // No plainTime: the day starts at GetStartOfDay, which is
                // not necessarily midnight (DST gaps at 00:00).
                KUndef -> {
                  let ns = terr(st, start_of_day_ns(tz, d))
                  make_zoned_cal(st, protos, ns, tz, cal)
                }
                _ -> {
                  let #(t, st) = to_temporal_time(st, pt_val, mk_undefined())
                  let ns = terr(st, get_epoch_ns_for(tz, d, t, Compatible))
                  make_zoned_cal(st, protos, ns, tz, cal)
                }
              }
            }
            _ -> rt_val.t_throw_type_error(st, "time zone must be a string")
          }
        }
        _ -> rt_val.t_throw_type_error(st, "time zone must be a string")
      }
    }
    PdUntil | PdSince -> {
      let #(#(other, other_cal), st) =
        to_temporal_date(st, helpers.arg_at(args, 0), mk_undefined())
      case other_cal == cal {
        False ->
          rt_val.t_throw_range_error(
            st,
            "cannot compute difference between dates of different calendars",
          )
        True -> date_until_since(st, protos, cal, d, other, args, m == PdSince)
      }
    }
  }
}

/// toPlainDateTime's optional time argument: undefined → midnight, else
/// ToTemporalTime.
fn optional_time_arg(st: Agent, v: JsVal) -> #(temporal_iso.TimeRec, Agent) {
  case classify(v) {
    KUndef -> #(midnight, st)
    _ -> to_temporal_time(st, v, mk_undefined())
  }
}

/// PlainDate.prototype.until/since.
fn date_until_since(
  st: Agent,
  protos: TemporalProtos,
  cal: tcal.Calendar,
  d1: IsoDate,
  d2: IsoDate,
  args: List(JsVal),
  is_since: Bool,
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
        terr(
          st,
          difference_calendar_date(cal, d1, d2, largest, smallest, inc, mode),
        )
      let dur = apply_since_dur(dur, is_since)
      make_duration(st, protos, dur)
    }
  }
}
