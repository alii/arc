import arc/bytecode/error_kind.{type JsError, JsError, RangeError}
import arc/internal/int_math.{floor_div, trunc_div}
import arc/rt/builtins/helpers
import arc/rt/builtins/options.{get_options_object, opt_get}
import arc/rt/builtins/temporal_common.{
  apply_duration_fields, check_time_duration_range, date_part, days_and_time_ns,
  duration_sign, duration_slot_of, finish_duration, has_calendar_units,
  has_date_units, integral_int_arg_or, is_valid_duration, make_duration,
  negate_duration, read_duration_fields, require_temporal, static_name,
  time_part_ns, to_temporal_duration,
}
import arc/rt/builtins/temporal_diff.{
  add_calendar_units, adjust_date_for_time_sign, default_largest_unit,
  diff_date_time_core, find_enclosing_window, iso_date_until, larger_time_unit,
  zoned_diff_round_time,
}
import arc/rt/builtins/temporal_fields.{
  get_named, iso_date_add, require_nonempty_fields,
}
import arc/rt/builtins/temporal_iso.{
  type Duration, type IsoDate, type IsoTime, type SecondsPrecision,
  AutoPrecision, Constrain, Duration, MinutePrecision, SubsecondDigits, add_days,
  check_date_limits, divide_as_float, epoch_days, format_fraction, int_sign,
  iso_date_from_epoch_days, iso_datetime_within_limits, midnight, ns_per_day,
  ns_per_ms, ns_per_second, ns_per_us, ns_to_time, pow10, time_to_ns,
  utc_epoch_ns,
}
import arc/rt/builtins/temporal_options.{Compatible}
import arc/rt/builtins/temporal_rounding.{
  type FractionalDigits, type RoundingMode, type TimeUnit, type Unit, Day,
  DigitsAuto, DigitsFixed, HalfExpand, Hour, Microsecond, MicrosecondUnit,
  Millisecond, MillisecondUnit, Month, Nanosecond, NanosecondUnit, Second,
  SecondUnit, Trunc, UnitAbsent, UnitAuto, UnitValue, Week, Year,
  balance_time_ns, get_fractional_digits, get_rounding_increment_option,
  get_rounding_mode_option, get_unit_option, largest_smaller_msg,
  largest_smaller_than_smallest, max_unit, read_unit_option, require_time_unit,
  round_to_increment, singular_unit, time_unit_ns, unit_rank, unit_to_string,
  valid_increment_for_unit,
}
import arc/rt/builtins/temporal_time_zone.{epoch_ns_to_iso_in}
import arc/rt/builtins/temporal_zoned_ops.{
  type RelativeTo, NoRelativeTo, RelativeDate, RelativeZoned, add_zoned_ns,
  convert_relative_to, date_duration_days, get_epoch_ns_for,
}
import arc/rt/temporal_data
import arc/rt/types.{
  type Agent, type DurationMethod, type JsVal, type NativeToken,
  type TemporalDurationGetter, type TemporalProtos, type TemporalStaticName,
  CompareStatic, DurationAbs, DurationAdd, DurationBlank, DurationDays,
  DurationHours, DurationMicroseconds, DurationMilliseconds, DurationMinutes,
  DurationMonths, DurationNanoseconds, DurationNegated, DurationRound,
  DurationSeconds, DurationSign, DurationSubtract, DurationToJson,
  DurationToLocaleString, DurationToString, DurationTotal, DurationValueOf,
  DurationWeeks, DurationWith, DurationYears, FromStatic, JFloat, KHandle, KStr,
  KUndef, TemporalDurationCtor, TemporalDurationGetter, TemporalDurationMethod,
  TemporalDurationStatic, TemporalN, classify, mk_bool, mk_int, mk_number,
  mk_string,
}
import arc/rt/val as rt_val
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

pub const all_duration_getters = [
  DurationYears,
  DurationMonths,
  DurationWeeks,
  DurationDays,
  DurationHours,
  DurationMinutes,
  DurationSeconds,
  DurationMilliseconds,
  DurationMicroseconds,
  DurationNanoseconds,
  DurationSign,
  DurationBlank,
]

pub fn ctor_token(protos: TemporalProtos) -> NativeToken {
  TemporalN(TemporalDurationCtor(protos:))
}

pub fn statics(protos: TemporalProtos) -> List(#(String, NativeToken, Int)) {
  list.map([#(FromStatic, 1), #(CompareStatic, 2)], fn(s) {
    #(static_name(s.0), TemporalN(TemporalDurationStatic(s.0, protos)), s.1)
  })
}

pub fn getters() -> List(#(String, NativeToken)) {
  list.map(all_duration_getters, fn(g) {
    #(duration_getter_name(g), TemporalN(TemporalDurationGetter(g)))
  })
}

pub fn methods(protos: TemporalProtos) -> List(#(String, NativeToken, Int)) {
  list.map(
    [
      #(DurationWith, 1),
      #(DurationNegated, 0),
      #(DurationAbs, 0),
      #(DurationAdd, 1),
      #(DurationSubtract, 1),
      #(DurationRound, 1),
      #(DurationTotal, 1),
      #(DurationToString, 0),
      #(DurationToJson, 0),
      #(DurationToLocaleString, 0),
      #(DurationValueOf, 0),
    ],
    fn(m) {
      #(
        duration_method_name(m.0),
        TemporalN(TemporalDurationMethod(m.0, protos)),
        m.1,
      )
    },
  )
}

pub fn duration_getter_name(g: TemporalDurationGetter) -> String {
  case g {
    DurationYears -> "years"
    DurationMonths -> "months"
    DurationWeeks -> "weeks"
    DurationDays -> "days"
    DurationHours -> "hours"
    DurationMinutes -> "minutes"
    DurationSeconds -> "seconds"
    DurationMilliseconds -> "milliseconds"
    DurationMicroseconds -> "microseconds"
    DurationNanoseconds -> "nanoseconds"
    DurationSign -> "sign"
    DurationBlank -> "blank"
  }
}

pub fn duration_method_name(m: DurationMethod) -> String {
  case m {
    DurationWith -> "with"
    DurationNegated -> "negated"
    DurationAbs -> "abs"
    DurationAdd -> "add"
    DurationSubtract -> "subtract"
    DurationRound -> "round"
    DurationTotal -> "total"
    DurationToString -> "toString"
    DurationToJson -> "toJSON"
    DurationToLocaleString -> "toLocaleString"
    DurationValueOf -> "valueOf"
  }
}

pub fn ctor(
  st: Agent,
  protos: TemporalProtos,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(years, st) = integral_int_arg_or(st, args, 0, 0)
  let #(months, st) = integral_int_arg_or(st, args, 1, 0)
  let #(weeks, st) = integral_int_arg_or(st, args, 2, 0)
  let #(days, st) = integral_int_arg_or(st, args, 3, 0)
  let #(hours, st) = integral_int_arg_or(st, args, 4, 0)
  let #(minutes, st) = integral_int_arg_or(st, args, 5, 0)
  let #(seconds, st) = integral_int_arg_or(st, args, 6, 0)
  let #(milliseconds, st) = integral_int_arg_or(st, args, 7, 0)
  let #(microseconds, st) = integral_int_arg_or(st, args, 8, 0)
  let #(nanoseconds, st) = integral_int_arg_or(st, args, 9, 0)
  let dur =
    Duration(
      years:,
      months:,
      weeks:,
      days:,
      hours:,
      minutes:,
      seconds:,
      milliseconds:,
      microseconds:,
      nanoseconds:,
    )
  finish_duration(st, protos, dur)
}

pub fn static(
  st: Agent,
  name: TemporalStaticName,
  protos: TemporalProtos,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case name {
    FromStatic -> {
      let #(d, st) = to_temporal_duration(st, helpers.arg_at(args, 0))
      make_duration(st, protos, d)
    }
    CompareStatic -> duration_compare(st, args)
  }
}

fn duration_compare(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(a, st) = to_temporal_duration(st, helpers.arg_at(args, 0))
  let #(b, st) = to_temporal_duration(st, helpers.arg_at(args, 1))
  let #(opts, st) = get_options_object(st, helpers.arg_at(args, 2))
  let #(relative_to_value, st) = opt_get(st, opts, "relativeTo")
  let #(relative_to, st) = convert_relative_to(st, relative_to_value)
  let has_cal_units = has_calendar_units(a) || has_calendar_units(b)
  let time_compare = fn(st) {
    #(mk_int(int_sign(days_and_time_ns(a) - days_and_time_ns(b))), st)
  }
  case a == b {
    True -> #(mk_int(0), st)
    False ->
      case relative_to {
        RelativeZoned(ns, tz, cal) ->
          case has_date_units(a) || has_date_units(b) {
            True -> {
              let na = rt_val.or_throw(st, add_zoned_ns(ns, tz, cal, a))
              let nb = rt_val.or_throw(st, add_zoned_ns(ns, tz, cal, b))
              #(mk_int(int_sign(na - nb)), st)
            }
            False -> time_compare(st)
          }
        RelativeDate(date, cal) ->
          case has_cal_units {
            True -> {
              let da = rt_val.or_throw(st, date_duration_days(a, date, cal))
              let na = da * ns_per_day + time_part_ns(a)
              let Nil = rt_val.or_throw(st, check_time_duration_range(na))
              let db = rt_val.or_throw(st, date_duration_days(b, date, cal))
              let nb = db * ns_per_day + time_part_ns(b)
              let Nil = rt_val.or_throw(st, check_time_duration_range(nb))
              #(mk_int(int_sign(na - nb)), st)
            }
            False -> time_compare(st)
          }
        NoRelativeTo ->
          case has_cal_units {
            True ->
              rt_val.t_throw_range_error(
                st,
                "relativeTo is required for duration comparison with calendar units",
              )
            False -> time_compare(st)
          }
      }
  }
}

fn require_duration(st: Agent, this: JsVal, name: String) -> Duration {
  require_temporal(st, this, "Duration", name, duration_slot_of)
}

pub fn getter(
  st: Agent,
  g: TemporalDurationGetter,
  this: JsVal,
) -> #(JsVal, Agent) {
  let d = require_duration(st, this, duration_getter_name(g))
  #(duration_field(d, g), st)
}

pub fn duration_field(d: Duration, g: TemporalDurationGetter) -> JsVal {
  case g {
    DurationYears -> mk_int(d.years)
    DurationMonths -> mk_int(d.months)
    DurationWeeks -> mk_int(d.weeks)
    DurationDays -> mk_int(d.days)
    DurationHours -> mk_int(d.hours)
    DurationMinutes -> mk_int(d.minutes)
    DurationSeconds -> mk_int(d.seconds)
    DurationMilliseconds -> mk_int(d.milliseconds)
    DurationMicroseconds -> mk_int(d.microseconds)
    DurationNanoseconds -> mk_int(d.nanoseconds)
    DurationSign -> mk_int(duration_sign(d))
    DurationBlank -> mk_bool(duration_sign(d) == 0)
  }
}

pub fn method(
  st: Agent,
  m: DurationMethod,
  protos: TemporalProtos,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let d = require_duration(st, this, duration_method_name(m))
  case m {
    DurationToJson | DurationToLocaleString -> #(
      mk_string(format_duration(d, AutoPrecision)),
      st,
    )
    DurationToString -> {
      let #(opts, st) = get_options_object(st, helpers.arg_at(args, 0))
      let #(digits, st) = get_fractional_digits(st, opts)
      let #(mode, st) = get_rounding_mode_option(st, opts, Trunc)
      let #(smallest, st) =
        get_unit_option(st, opts, "smallestUnit", allow_auto: False)
      let #(precision, unit, inc) =
        rt_val.or_throw(st, duration_string_precision(digits, smallest))
      let d2 =
        rt_val.or_throw(st, case unit == NanosecondUnit && inc == 1 {
          True -> Ok(d)
          False -> round_duration_for_string(d, inc, unit, mode)
        })
      #(mk_string(format_duration(d2, precision)), st)
    }
    DurationValueOf ->
      rt_val.t_throw_type_error(
        st,
        "Temporal.Duration cannot be converted with valueOf",
      )
    DurationNegated -> make_duration(st, protos, negate_duration(d))
    DurationAbs -> {
      let abs_d = case duration_sign(d) < 0 {
        True -> negate_duration(d)
        False -> d
      }
      make_duration(st, protos, abs_d)
    }
    DurationWith ->
      case classify(helpers.arg_at(args, 0)) {
        KHandle(bag) -> {
          let #(fields, st) = read_duration_fields(st, bag)
          let Nil =
            require_nonempty_fields(st, list.all(fields, option.is_none))
          finish_duration(st, protos, apply_duration_fields(d, fields))
        }
        _ -> rt_val.t_throw_type_error(st, "argument must be an object")
      }
    DurationAdd | DurationSubtract -> {
      let #(other, st) = to_temporal_duration(st, helpers.arg_at(args, 0))
      let other = case m {
        DurationSubtract -> negate_duration(other)
        _ -> other
      }
      case has_calendar_units(d) || has_calendar_units(other) {
        True ->
          rt_val.t_throw_range_error(
            st,
            "duration add/subtract requires non-calendar durations",
          )
        False -> {
          let total = days_and_time_ns(d) + days_and_time_ns(other)
          let largest = larger_time_unit(d, other)
          let sum = balance_time_ns(total, largest)
          finish_duration(st, protos, sum)
        }
      }
    }
    DurationRound -> duration_round(st, protos, d, args)
    DurationTotal -> duration_total(st, d, args)
  }
}

fn duration_round(
  st: Agent,
  protos: TemporalProtos,
  d: Duration,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let arg = helpers.arg_at(args, 0)
  case classify(arg) {
    KUndef -> rt_val.t_throw_type_error(st, "options parameter is required")
    KStr(unit_name) ->
      case singular_unit(unit_name) {
        Some(smallest) ->
          duration_round_with(
            st,
            protos,
            d,
            None,
            smallest,
            1,
            HalfExpand,
            NoRelativeTo,
          )
        None -> rt_val.t_throw_range_error(st, "invalid smallestUnit")
      }
    KHandle(oh) -> {
      let opts = Some(oh)
      let #(largest, st) =
        read_unit_option(st, opts, "largestUnit", allow_auto: True)
      let #(relative_to_value, st) = get_named(st, oh, "relativeTo")
      let #(relative_to, st) = convert_relative_to(st, relative_to_value)
      let #(inc, st) = get_rounding_increment_option(st, opts)
      let #(mode, st) = get_rounding_mode_option(st, opts, HalfExpand)
      let #(smallest, st) =
        get_unit_option(st, opts, "smallestUnit", allow_auto: False)
      case smallest == None && largest == UnitAbsent {
        True ->
          rt_val.t_throw_range_error(
            st,
            "at least one of smallestUnit or largestUnit is required",
          )
        False -> {
          let smallest = option.unwrap(smallest, Nanosecond)
          let largest = case largest {
            UnitValue(u) -> Some(u)
            UnitAuto | UnitAbsent -> None
          }
          duration_round_with(
            st,
            protos,
            d,
            largest,
            smallest,
            inc,
            mode,
            relative_to,
          )
        }
      }
    }
    _ -> rt_val.t_throw_type_error(st, "invalid options")
  }
}

// largest None means the duration's default largest unit
fn duration_round_with(
  st: Agent,
  protos: TemporalProtos,
  d: Duration,
  largest: Option(Unit),
  smallest: Unit,
  inc: Int,
  mode: RoundingMode,
  relative_to: RelativeTo,
) -> #(JsVal, Agent) {
  let largest = case largest {
    None -> max_unit(default_largest_unit(d), smallest)
    Some(u) -> u
  }
  let date_inc_invalid =
    inc > 1 && unit_rank(smallest) >= unit_rank(Day) && largest != smallest
  case
    largest_smaller_than_smallest(largest, smallest),
    !valid_increment_for_unit(inc, smallest) || date_inc_invalid
  {
    True, _ -> rt_val.t_throw_range_error(st, largest_smaller_msg)
    _, True -> rt_val.t_throw_range_error(st, "invalid roundingIncrement")
    False, False ->
      case relative_to {
        NoRelativeTo -> {
          let needs_rel =
            has_calendar_units(d)
            || unit_rank(largest) > unit_rank(Day)
            || unit_rank(smallest) > unit_rank(Day)
          case needs_rel {
            True ->
              rt_val.t_throw_range_error(
                st,
                "relativeTo is required for calendar-unit rounding",
              )
            False -> {
              let smallest_time_unit =
                rt_val.or_throw(st, require_time_unit(smallest))
              let total = days_and_time_ns(d)
              let rounded =
                round_to_increment(
                  total,
                  inc * time_unit_ns(smallest_time_unit),
                  mode,
                )
              let result = balance_time_ns(rounded, largest)
              finish_duration(st, protos, result)
            }
          }
        }
        RelativeZoned(relative_ns, tz, cal) -> {
          let target_ns =
            rt_val.or_throw(st, add_zoned_ns(relative_ns, tz, cal, d))
          case unit_rank(largest) <= unit_rank(Hour) {
            True -> {
              let smallest_time_unit =
                rt_val.or_throw(st, require_time_unit(smallest))
              let diff = target_ns - relative_ns
              let rounded =
                round_to_increment(
                  diff,
                  inc * time_unit_ns(smallest_time_unit),
                  mode,
                )
              let result = balance_time_ns(rounded, largest)
              finish_duration(st, protos, result)
            }
            False -> {
              let result =
                rt_val.or_throw(st, case unit_rank(smallest) >= unit_rank(Day) {
                  True ->
                    diff_date_time_core(
                      cal,
                      epoch_ns_to_iso_in(tz, relative_ns),
                      epoch_ns_to_iso_in(tz, target_ns),
                      largest,
                      smallest,
                      inc,
                      mode,
                      zoned: True,
                    )
                  False ->
                    zoned_diff_round_time(
                      cal,
                      tz,
                      relative_ns,
                      target_ns,
                      largest,
                      smallest,
                      inc,
                      mode,
                    )
                })
              finish_duration(st, protos, result)
            }
          }
        }
        RelativeDate(relative_date, cal) -> {
          let Nil =
            rt_val.or_throw(st, check_plain_relative_to_range(d, relative_date))
          let target =
            rt_val.or_throw(st, duration_target_datetime(relative_date, d))
          let result =
            rt_val.or_throw(
              st,
              diff_date_time_core(
                cal,
                #(relative_date, midnight),
                target,
                largest,
                smallest,
                inc,
                mode,
                zoned: False,
              ),
            )
          finish_duration(st, protos, result)
        }
      }
  }
}

// a zero duration returns before relativeto is range-checked
fn check_plain_relative_to_range(
  d: Duration,
  relative_date: IsoDate,
) -> Result(Nil, JsError) {
  case
    duration_sign(d) != 0
    && !iso_datetime_within_limits(relative_date, midnight)
  {
    True ->
      Error(JsError(
        RangeError,
        "relativeTo is outside the representable range after conversion to DateTime",
      ))
    False -> Ok(Nil)
  }
}

fn duration_target_datetime(
  rel: IsoDate,
  d: Duration,
) -> Result(#(IsoDate, IsoTime), JsError) {
  use base <- result.try(iso_date_add(rel, date_part(d), Constrain))
  let time_ns = time_part_ns(d)
  let extra_days = floor_div(time_ns, ns_per_day)
  let rem = time_ns - extra_days * ns_per_day
  use final <- result.map(check_date_limits(add_days(base, extra_days)))
  #(final, ns_to_time(rem))
}

fn duration_total(
  st: Agent,
  d: Duration,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let arg = helpers.arg_at(args, 0)
  case classify(arg) {
    KUndef -> rt_val.t_throw_type_error(st, "totalOf is required")
    KStr(unit_name) ->
      case singular_unit(unit_name) {
        Some(u) -> duration_total_with(st, d, u, NoRelativeTo)
        None -> rt_val.t_throw_range_error(st, "invalid unit")
      }
    KHandle(oh) -> {
      let #(relative_to_value, st) = get_named(st, oh, "relativeTo")
      let #(relative_to, st) = convert_relative_to(st, relative_to_value)
      let #(unit, st) = get_unit_option(st, Some(oh), "unit", allow_auto: False)
      case unit {
        None -> rt_val.t_throw_range_error(st, "unit is required")
        Some(u) -> duration_total_with(st, d, u, relative_to)
      }
    }
    _ -> rt_val.t_throw_type_error(st, "invalid totalOf")
  }
}

fn duration_total_with(
  st: Agent,
  d: Duration,
  unit: Unit,
  relative_to: RelativeTo,
) -> #(JsVal, Agent) {
  case relative_to {
    NoRelativeTo -> {
      case has_calendar_units(d) || unit_rank(unit) > unit_rank(Day) {
        True ->
          rt_val.t_throw_range_error(
            st,
            "relativeTo is required to total calendar units",
          )
        False -> {
          let time_unit = rt_val.or_throw(st, require_time_unit(unit))
          let total = days_and_time_ns(d)
          #(
            mk_number(JFloat(divide_as_float(total, time_unit_ns(time_unit)))),
            st,
          )
        }
      }
    }
    RelativeZoned(anchor_ns, tz, cal) -> {
      let target_ns = rt_val.or_throw(st, add_zoned_ns(anchor_ns, tz, cal, d))
      case unit_rank(unit) <= unit_rank(Hour) {
        True -> {
          let time_unit = rt_val.or_throw(st, require_time_unit(unit))
          let diff = target_ns - anchor_ns
          #(
            mk_number(JFloat(divide_as_float(diff, time_unit_ns(time_unit)))),
            st,
          )
        }
        False -> zoned_calendar_total(st, tz, anchor_ns, target_ns, unit)
      }
    }
    RelativeDate(relative_date, _cal) -> {
      let Nil =
        rt_val.or_throw(st, check_plain_relative_to_range(d, relative_date))
      let target =
        rt_val.or_throw(st, duration_target_datetime(relative_date, d))
      let relative_ns = utc_epoch_ns(relative_date, midnight)
      let target_ns = utc_epoch_ns(target.0, target.1)
      case unit_rank(unit) <= unit_rank(Day) {
        True -> {
          let time_unit = rt_val.or_throw(st, require_time_unit(unit))
          let diff = target_ns - relative_ns
          #(
            mk_number(JFloat(divide_as_float(diff, time_unit_ns(time_unit)))),
            st,
          )
        }
        False ->
          plain_calendar_total(st, relative_date, relative_ns, target_ns, unit)
      }
    }
  }
}

fn zoned_calendar_total(
  st: Agent,
  tz: temporal_data.TemporalZone,
  anchor_ns: Int,
  target_ns: Int,
  unit: Unit,
) -> #(JsVal, Agent) {
  let #(a_d, a_t) = epoch_ns_to_iso_in(tz, anchor_ns)
  let #(b_d, b_t) = epoch_ns_to_iso_in(tz, target_ns)
  let sign = case target_ns < anchor_ns {
    True -> -1
    False -> 1
  }
  let #(b_date, _) =
    adjust_date_for_time_sign(sign, b_d, time_to_ns(b_t) - time_to_ns(a_t))
  let whole = case unit {
    Year -> iso_date_until(a_d, b_date, Year).0
    Month -> iso_date_until(a_d, b_date, Month).1
    Week -> trunc_div(epoch_days(b_date) - epoch_days(a_d), 7)
    _ -> epoch_days(b_date) - epoch_days(a_d)
  }
  let bound_ns = fn(w: Int) {
    let date = case unit {
      Day -> add_days(a_d, w)
      _ -> add_calendar_units(a_d, unit, w)
    }
    get_epoch_ns_for(tz, date, a_t, Compatible)
  }
  let window =
    rt_val.or_throw(
      st,
      find_enclosing_window(sign, whole, 1, target_ns, bound_ns),
    )
  #(mk_number(JFloat(fractional_total(window, sign, target_ns))), st)
}

// window shifts one unit when month-end clamping undercounts
fn plain_calendar_total(
  st: Agent,
  relative_date: IsoDate,
  relative_ns: Int,
  target_ns: Int,
  unit: Unit,
) -> #(JsVal, Agent) {
  let sign = case target_ns < relative_ns {
    True -> -1
    False -> 1
  }
  let target_floor_days = floor_div(target_ns, ns_per_day)
  let target_date = iso_date_from_epoch_days(target_floor_days)
  let whole = case unit {
    Year -> iso_date_until(relative_date, target_date, Year).0
    Month -> iso_date_until(relative_date, target_date, Month).1
    _ -> trunc_div(epoch_days(target_date) - epoch_days(relative_date), 7)
  }
  let bound_ns = fn(w: Int) {
    use d2 <- result.map(
      check_date_limits(add_calendar_units(relative_date, unit, w)),
    )
    utc_epoch_ns(d2, midnight)
  }
  let window =
    rt_val.or_throw(
      st,
      find_enclosing_window(sign, whole, 1, target_ns, bound_ns),
    )
  #(mk_number(JFloat(fractional_total(window, sign, target_ns))), st)
}

fn fractional_total(
  window: temporal_diff.Window,
  sign: Int,
  target_ns: Int,
) -> Float {
  let num = target_ns - window.start_ns
  let den = window.end_ns - window.start_ns
  case den == 0 {
    True -> int.to_float(window.count)
    False -> divide_as_float(window.count * den + sign * num, den)
  }
}

fn duration_string_precision(
  digits: FractionalDigits,
  smallest: Option(Unit),
) -> Result(#(SecondsPrecision, TimeUnit, Int), JsError) {
  case smallest {
    Some(Second) -> Ok(#(SubsecondDigits(0), SecondUnit, 1))
    Some(Millisecond) -> Ok(#(SubsecondDigits(3), MillisecondUnit, 1))
    Some(Microsecond) -> Ok(#(SubsecondDigits(6), MicrosecondUnit, 1))
    Some(Nanosecond) -> Ok(#(SubsecondDigits(9), NanosecondUnit, 1))
    Some(u) ->
      Error(JsError(
        RangeError,
        unit_to_string(u)
          <> " is not a valid smallestUnit for Duration.toString",
      ))
    None ->
      case digits {
        DigitsAuto -> Ok(#(AutoPrecision, NanosecondUnit, 1))
        DigitsFixed(0) -> Ok(#(SubsecondDigits(0), SecondUnit, 1))
        DigitsFixed(n) ->
          Ok(#(SubsecondDigits(n), NanosecondUnit, pow10(9 - n)))
      }
  }
}

fn round_duration_for_string(
  d: Duration,
  inc: Int,
  unit: TimeUnit,
  mode: RoundingMode,
) -> Result(Duration, JsError) {
  let time_ns = time_part_ns(d)
  let rounded = round_to_increment(time_ns, inc * time_unit_ns(unit), mode)
  let largest = max_unit(default_largest_unit(d), Second)
  let result = case unit_rank(largest) >= unit_rank(Day) {
    True -> {
      let extra_days = trunc_div(rounded, ns_per_day)
      let rem = rounded - extra_days * ns_per_day
      let t = balance_time_ns(rem, Hour)
      Duration(
        ..t,
        years: d.years,
        months: d.months,
        weeks: d.weeks,
        days: d.days + extra_days,
      )
    }
    False -> balance_time_ns(rounded, largest)
  }
  case is_valid_duration(result) {
    True -> Ok(result)
    False -> Error(JsError(RangeError, "rounded duration is out of range"))
  }
}

pub fn format_duration(d: Duration, precision: SecondsPrecision) -> String {
  let sign = duration_sign(d)
  let prefix = case sign < 0 {
    True -> "-"
    False -> ""
  }
  let abs_part = fn(n: Int) { int.absolute_value(n) }
  let date_text =
    join_unit(abs_part(d.years), "Y")
    <> join_unit(abs_part(d.months), "M")
    <> join_unit(abs_part(d.weeks), "W")
    <> join_unit(abs_part(d.days), "D")
  // sub-second parts may exceed their unit; carry into seconds
  let sub_total =
    abs_part(d.milliseconds)
    * ns_per_ms
    + abs_part(d.microseconds)
    * ns_per_us
    + abs_part(d.nanoseconds)
  let extra_seconds = sub_total / ns_per_second
  let sub = sub_total % ns_per_second
  let seconds_text = case
    d.seconds != 0
    || sub_total != 0
    || { date_text == "" && d.hours == 0 && d.minutes == 0 }
    || show_fixed_seconds(precision)
  {
    True ->
      int.to_string(abs_part(d.seconds) + extra_seconds)
      <> format_fraction(sub, precision)
      <> "S"
    False -> ""
  }
  let time_text =
    join_unit(abs_part(d.hours), "H")
    <> join_unit(abs_part(d.minutes), "M")
    <> seconds_text
  let t = case time_text {
    "" -> ""
    _ -> "T" <> time_text
  }
  prefix <> "P" <> date_text <> t
}

fn show_fixed_seconds(p: SecondsPrecision) -> Bool {
  case p {
    SubsecondDigits(_) -> True
    AutoPrecision | MinutePrecision -> False
  }
}

fn join_unit(n: Int, designator: String) -> String {
  case n == 0 {
    True -> ""
    False -> int.to_string(n) <> designator
  }
}
