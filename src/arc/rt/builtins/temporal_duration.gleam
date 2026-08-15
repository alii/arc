//// Temporal.Duration (proposal-temporal §7): ten integral fields sharing one
//// sign. The date part (years/months/weeks) is only ever measured against a
//// relativeTo anchor; the day+time part is exact nanoseconds.
////
//// The duration record ops (ToTemporalDuration, IsValidDuration, balancing)
//// are temporal_common.gleam; relativeTo resolution is temporal_zoned_ops;
//// the round/total cores are temporal_diff.gleam.

import arc/internal/int_math.{floor_div, trunc_div}
import arc/rt/builtins/helpers
import arc/rt/builtins/temporal_common.{
  type FractionalDigits, type RoundingMode, type TimeUnit, type Unit, Compatible,
  Day, DigitsAuto, DigitsFixed, HalfExpand, Hour, Microsecond, Millisecond,
  Minute, Month, Nanosecond, Second, Trunc, UMicrosecond, UMillisecond,
  UNanosecond, USecond, UnitAbsent, UnitAuto, UnitValue, Week, Year,
  apply_duration_fields, balance_time_ns, check_time_duration_range,
  duration_sign, duration_slot_of, epoch_ns_to_iso_in, finish_duration,
  get_fractional_digits, get_options_object, get_rounding_increment_option,
  get_rounding_mode_option, get_unit_option, get_unit_option_keep,
  is_valid_duration, largest_smaller_msg, largest_smaller_than_smallest,
  make_duration, max_unit, negate_dur, opt_integral_arg, read_duration_fields,
  require_temporal, require_time_unit, round_to_increment, singular_unit, terr,
  time_duration_ns, time_only_ns, time_unit_ns, to_temporal_duration, unit_rank,
  unit_to_string, valid_time_increment,
}
import arc/rt/builtins/temporal_diff.{
  add_calendar_units, default_largest_unit, diff_date_parts, diff_date_time_core,
  larger_time_unit, zoned_diff_round_time,
}
import arc/rt/builtins/temporal_fields.{
  add_duration_to_date, get_named, require_nonempty_fields,
}
import arc/rt/builtins/temporal_iso.{
  type DurRec, type IsoDate, type Precision, type TErr, type TimeRec, AutoPrec,
  Constrain, DurRec, FixedPrec, MinutePrec, RangeE, check_date_limits,
  epoch_days, format_fraction, int_sign, iso_date_from_epoch_days,
  iso_date_within_limits, iso_datetime_within_limits, midnight, ns_div_float,
  ns_per_day, ns_per_ms, ns_per_second, ns_per_us, ns_to_time, pow10, time_to_ns,
  zero_dur,
}
import arc/rt/builtins/temporal_zoned_ops.{
  type RelTo, RelNone, RelPlain, RelZoned, add_zoned_ns, convert_relative_to,
  date_duration_days, get_epoch_ns_for,
}
import arc/rt/types.{
  type Agent, type DurationMethod, type JsVal, type NativeToken,
  type TemporalDurationGetter, type TemporalProtos, type TemporalStaticName,
  DmAbs, DmAdd, DmNegated, DmRound, DmSubtract, DmToJson, DmToLocaleString,
  DmToString, DmTotal, DmValueOf, DmWith, DrBlank, DrDays, DrHours,
  DrMicroseconds, DrMilliseconds, DrMinutes, DrMonths, DrNanoseconds, DrSeconds,
  DrSign, DrWeeks, DrYears, JFloat, JInt, KHandle, KStr, KUndef,
  TemporalDurationCtor, TemporalDurationGetter, TemporalDurationMethod,
  TemporalDurationStatic, TemporalN, TsCompare, TsFrom, classify, mk_bool,
  mk_number, mk_string,
}
import arc/rt/val as rt_val
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

// ============================================================================
// Init — Temporal.Duration constructor + prototype
// ============================================================================

/// The getters, in prototype-registration order.
pub const all_duration_getters = [
  DrYears,
  DrMonths,
  DrWeeks,
  DrDays,
  DrHours,
  DrMinutes,
  DrSeconds,
  DrMilliseconds,
  DrMicroseconds,
  DrNanoseconds,
  DrSign,
  DrBlank,
]

/// Registration specs for `temporal.init_temporal_type`: the constructor
/// token, `from`/`compare`, the twelve getters and the prototype methods, in
/// prototype-registration order.
pub fn ctor_token(protos: TemporalProtos) -> NativeToken {
  TemporalN(TemporalDurationCtor(protos:))
}

pub fn statics(protos: TemporalProtos) -> List(#(String, NativeToken, Int)) {
  list.map([#(TsFrom, 1), #(TsCompare, 2)], fn(s) {
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
      #(DmWith, 1),
      #(DmNegated, 0),
      #(DmAbs, 0),
      #(DmAdd, 1),
      #(DmSubtract, 1),
      #(DmRound, 1),
      #(DmTotal, 1),
      #(DmToString, 0),
      #(DmToJson, 0),
      #(DmToLocaleString, 0),
      #(DmValueOf, 0),
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

fn static_name(s: TemporalStaticName) -> String {
  case s {
    TsFrom -> "from"
    TsCompare -> "compare"
  }
}

pub fn duration_getter_name(g: TemporalDurationGetter) -> String {
  case g {
    DrYears -> "years"
    DrMonths -> "months"
    DrWeeks -> "weeks"
    DrDays -> "days"
    DrHours -> "hours"
    DrMinutes -> "minutes"
    DrSeconds -> "seconds"
    DrMilliseconds -> "milliseconds"
    DrMicroseconds -> "microseconds"
    DrNanoseconds -> "nanoseconds"
    DrSign -> "sign"
    DrBlank -> "blank"
  }
}

pub fn duration_method_name(m: DurationMethod) -> String {
  case m {
    DmWith -> "with"
    DmNegated -> "negated"
    DmAbs -> "abs"
    DmAdd -> "add"
    DmSubtract -> "subtract"
    DmRound -> "round"
    DmTotal -> "total"
    DmToString -> "toString"
    DmToJson -> "toJSON"
    DmToLocaleString -> "toLocaleString"
    DmValueOf -> "valueOf"
  }
}

fn from_int(n: Int) -> JsVal {
  mk_number(JInt(n))
}

// ============================================================================
// Constructor and statics
// ============================================================================

/// new Temporal.Duration(y, mo, w, d, h, mi, s, ms, us, ns) — all optional.
pub fn ctor(
  st: Agent,
  protos: TemporalProtos,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(y, st) = opt_integral_arg(st, args, 0)
  let #(mo, st) = opt_integral_arg(st, args, 1)
  let #(w, st) = opt_integral_arg(st, args, 2)
  let #(d, st) = opt_integral_arg(st, args, 3)
  let #(h, st) = opt_integral_arg(st, args, 4)
  let #(mi, st) = opt_integral_arg(st, args, 5)
  let #(s, st) = opt_integral_arg(st, args, 6)
  let #(ms, st) = opt_integral_arg(st, args, 7)
  let #(us, st) = opt_integral_arg(st, args, 8)
  let #(ns, st) = opt_integral_arg(st, args, 9)
  let dur = DurRec(y, mo, w, d, h, mi, s, ms, us, ns)
  finish_duration(st, protos, dur)
}

/// Temporal.Duration.from(item) / Temporal.Duration.compare(one, two, options).
pub fn static(
  st: Agent,
  name: TemporalStaticName,
  protos: TemporalProtos,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case name {
    TsFrom -> {
      let #(d, st) = to_temporal_duration(st, helpers.arg_at(args, 0))
      make_duration(st, protos, d)
    }
    TsCompare -> duration_compare(st, args)
  }
}

/// Temporal.Duration.compare(one, two [, options]).
fn duration_compare(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(a, st) = to_temporal_duration(st, helpers.arg_at(args, 0))
  let #(b, st) = to_temporal_duration(st, helpers.arg_at(args, 1))
  let #(opts, st) = get_options_object(st, helpers.arg_at(args, 2))
  let #(relative_to, st) = case opts {
    None -> #(types.mk_undefined(), st)
    Some(oh) -> get_named(st, oh, "relativeTo")
  }
  let #(rel, st) = convert_relative_to(st, relative_to)
  let has_cal_units =
    a.years != 0
    || a.months != 0
    || a.weeks != 0
    || b.years != 0
    || b.months != 0
    || b.weeks != 0
  let time_compare = fn(st) {
    #(from_int(int_sign(time_duration_ns(a) - time_duration_ns(b))), st)
  }
  case a == b {
    // Identical field values compare equal without consulting relativeTo.
    True -> #(from_int(0), st)
    False ->
      case rel {
        RelZoned(ns, tz, cal) ->
          case has_cal_units || a.days != 0 || b.days != 0 {
            True -> {
              let na = terr(st, add_zoned_ns(ns, tz, cal, a))
              let nb = terr(st, add_zoned_ns(ns, tz, cal, b))
              #(from_int(int_sign(na - nb)), st)
            }
            False -> time_compare(st)
          }
        RelPlain(rel_date, rel_cal) ->
          case has_cal_units {
            True -> {
              let da = terr(st, date_duration_days(a, rel_date, rel_cal))
              let na = da * ns_per_day + time_only_ns(a)
              let Nil = terr(st, check_time_duration_range(na))
              let db = terr(st, date_duration_days(b, rel_date, rel_cal))
              let nb = db * ns_per_day + time_only_ns(b)
              let Nil = terr(st, check_time_duration_range(nb))
              #(from_int(int_sign(na - nb)), st)
            }
            False -> time_compare(st)
          }
        RelNone ->
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

// ============================================================================
// Getters
// ============================================================================

/// RequireInternalSlot(this, [[InitializedTemporalDuration]]).
fn require_duration(st: Agent, this: JsVal, name: String) -> DurRec {
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

pub fn duration_field(d: DurRec, g: TemporalDurationGetter) -> JsVal {
  case g {
    DrYears -> from_int(d.years)
    DrMonths -> from_int(d.months)
    DrWeeks -> from_int(d.weeks)
    DrDays -> from_int(d.days)
    DrHours -> from_int(d.hours)
    DrMinutes -> from_int(d.minutes)
    DrSeconds -> from_int(d.seconds)
    DrMilliseconds -> from_int(d.ms)
    DrMicroseconds -> from_int(d.us)
    DrNanoseconds -> from_int(d.ns)
    DrSign -> from_int(duration_sign(d))
    DrBlank -> mk_bool(duration_sign(d) == 0)
  }
}

// ============================================================================
// Methods
// ============================================================================

pub fn method(
  st: Agent,
  m: DurationMethod,
  protos: TemporalProtos,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let d = require_duration(st, this, duration_method_name(m))
  case m {
    DmToJson | DmToLocaleString -> #(
      mk_string(format_duration(d, AutoPrec)),
      st,
    )
    DmToString -> {
      let #(opts, st) = get_options_object(st, helpers.arg_at(args, 0))
      let #(digits, st) = get_fractional_digits(st, opts)
      let #(mode, st) = get_rounding_mode_option(st, opts, Trunc)
      let #(su, st) =
        get_unit_option(st, opts, "smallestUnit", allow_auto: False)
      let #(prec, runit, rinc) = terr(st, duration_string_precision(digits, su))
      let d2 =
        terr(st, case runit == UNanosecond && rinc == 1 {
          True -> Ok(d)
          False -> round_duration_for_string(d, rinc, runit, mode)
        })
      #(mk_string(format_duration(d2, prec)), st)
    }
    DmValueOf ->
      rt_val.t_throw_type_error(
        st,
        "Temporal.Duration cannot be converted with valueOf",
      )
    DmNegated -> make_duration(st, protos, negate_dur(d))
    DmAbs -> {
      let abs_d = case duration_sign(d) < 0 {
        True -> negate_dur(d)
        False -> d
      }
      make_duration(st, protos, abs_d)
    }
    DmWith ->
      case classify(helpers.arg_at(args, 0)) {
        KHandle(bag) -> {
          let #(fields, st) = read_duration_fields(st, bag)
          let Nil =
            require_nonempty_fields(st, list.all(fields, option.is_none))
          finish_duration(st, protos, apply_duration_fields(d, fields))
        }
        _ -> rt_val.t_throw_type_error(st, "argument must be an object")
      }
    DmAdd | DmSubtract -> {
      let #(other, st) = to_temporal_duration(st, helpers.arg_at(args, 0))
      let other = case m {
        DmSubtract -> negate_dur(other)
        _ -> other
      }
      let has_cal =
        d.years != 0
        || d.months != 0
        || d.weeks != 0
        || other.years != 0
        || other.months != 0
        || other.weeks != 0
      case has_cal {
        True ->
          rt_val.t_throw_range_error(
            st,
            "duration add/subtract requires non-calendar durations",
          )
        False -> {
          let total = time_duration_ns(d) + time_duration_ns(other)
          let largest = larger_time_unit(d, other)
          let sum = balance_time_ns(total, largest)
          finish_duration(st, protos, sum)
        }
      }
    }
    DmRound -> duration_round(st, protos, d, args)
    DmTotal -> duration_total(st, d, args)
  }
}

// ----------------------------------------------------------------------------
// round
// ----------------------------------------------------------------------------

/// Temporal.Duration.prototype.round ( roundTo )
fn duration_round(
  st: Agent,
  protos: TemporalProtos,
  d: DurRec,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let arg = helpers.arg_at(args, 0)
  case classify(arg) {
    KUndef -> rt_val.t_throw_type_error(st, "options parameter is required")
    KStr(su_str) ->
      case singular_unit(su_str) {
        Some(su) ->
          duration_round_with(st, protos, d, None, su, 1, HalfExpand, RelNone)
        None -> rt_val.t_throw_range_error(st, "invalid smallestUnit")
      }
    KHandle(oh) -> {
      // Options are read in alphabetical order: largestUnit, relativeTo,
      // roundingIncrement, roundingMode, smallestUnit.
      let opts = Some(oh)
      let #(largest, st) = get_unit_option_keep(st, opts, "largestUnit")
      let #(rel_v, st) = get_named(st, oh, "relativeTo")
      let #(rel, st) = convert_relative_to(st, rel_v)
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
          let su = option.unwrap(smallest, Nanosecond)
          let lu = case largest {
            UnitValue(u) -> Some(u)
            UnitAuto | UnitAbsent -> None
          }
          duration_round_with(st, protos, d, lu, su, inc, mode, rel)
        }
      }
    }
    _ -> rt_val.t_throw_type_error(st, "invalid options")
  }
}

/// `largest` None means "auto": the duration's own default largest unit.
fn duration_round_with(
  st: Agent,
  protos: TemporalProtos,
  d: DurRec,
  largest: Option(Unit),
  smallest: Unit,
  inc: Int,
  mode: RoundingMode,
  rel: RelTo,
) -> #(JsVal, Agent) {
  let largest = case largest {
    None -> max_unit(default_largest_unit(d), smallest)
    Some(u) -> u
  }
  let max_inc = case smallest {
    Hour -> Some(24)
    Minute | Second -> Some(60)
    Millisecond | Microsecond | Nanosecond -> Some(1000)
    Year | Month | Week | Day -> None
  }
  let inc_invalid = case max_inc {
    Some(max) -> !valid_time_increment(inc, max)
    None -> False
  }
  let date_inc_invalid =
    inc > 1 && unit_rank(smallest) >= unit_rank(Day) && largest != smallest
  case
    largest_smaller_than_smallest(largest, smallest),
    inc_invalid || date_inc_invalid
  {
    True, _ -> rt_val.t_throw_range_error(st, largest_smaller_msg)
    _, True -> rt_val.t_throw_range_error(st, "invalid roundingIncrement")
    False, False ->
      case rel {
        RelNone -> {
          let needs_rel =
            d.years != 0
            || d.months != 0
            || d.weeks != 0
            || unit_rank(largest) > unit_rank(Day)
            || unit_rank(smallest) > unit_rank(Day)
          case needs_rel {
            True ->
              rt_val.t_throw_range_error(
                st,
                "relativeTo is required for calendar-unit rounding",
              )
            False -> {
              let su = terr(st, require_time_unit(smallest))
              let total = time_duration_ns(d)
              let rounded =
                round_to_increment(total, inc * time_unit_ns(su), mode)
              let result = balance_time_ns(rounded, largest)
              finish_duration(st, protos, result)
            }
          }
        }
        RelZoned(rel_ns, tz, cal) -> {
          // DifferenceZonedDateTimeWithRounding between the anchor and
          // anchor + duration.
          let target_ns = terr(st, add_zoned_ns(rel_ns, tz, cal, d))
          case unit_rank(largest) <= unit_rank(Hour) {
            True -> {
              let su = terr(st, require_time_unit(smallest))
              let diff = target_ns - rel_ns
              let rounded =
                round_to_increment(diff, inc * time_unit_ns(su), mode)
              let result = balance_time_ns(rounded, largest)
              finish_duration(st, protos, result)
            }
            False -> {
              let result =
                terr(st, case unit_rank(smallest) >= unit_rank(Day) {
                  // Calendar-unit (or zoned day) smallestUnit: wall-clock
                  // diff with calendar nudging.
                  True -> {
                    use a_dt <- result.try(epoch_ns_to_iso_in(tz, rel_ns))
                    use b_dt <- result.try(epoch_ns_to_iso_in(tz, target_ns))
                    diff_date_time_core(
                      cal,
                      a_dt,
                      b_dt,
                      largest,
                      smallest,
                      inc,
                      mode,
                      True,
                    )
                  }
                  // Time-unit smallestUnit: days are bounded by real instants
                  // and the time part is rounded within the day
                  // (NudgeToZonedTime).
                  False ->
                    zoned_diff_round_time(
                      cal,
                      tz,
                      rel_ns,
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
        RelPlain(rel_date, rel_cal) -> {
          // A zero duration rounds to zero before the relativeTo date-time
          // is range-checked.
          let out_of_range =
            duration_sign(d) != 0
            && !iso_datetime_within_limits(rel_date, midnight)
          case out_of_range {
            True ->
              rt_val.t_throw_range_error(
                st,
                "relativeTo is outside the representable range after conversion to DateTime",
              )
            False -> {
              let target = terr(st, duration_target_datetime(rel_date, d))
              let result =
                terr(
                  st,
                  diff_date_time_core(
                    rel_cal,
                    #(rel_date, midnight),
                    target,
                    largest,
                    smallest,
                    inc,
                    mode,
                    False,
                  ),
                )
              finish_duration(st, protos, result)
            }
          }
        }
      }
  }
}

/// rel + duration as an exact (date, time) pair.
fn duration_target_datetime(
  rel: IsoDate,
  d: DurRec,
) -> Result(#(IsoDate, TimeRec), TErr) {
  let date_only =
    DurRec(
      ..zero_dur,
      years: d.years,
      months: d.months,
      weeks: d.weeks,
      days: d.days,
    )
  use base <- result.try(add_duration_to_date(rel, date_only, Constrain))
  let time_ns = time_only_ns(d)
  let extra_days = floor_div(time_ns, ns_per_day)
  let rem = time_ns - extra_days * ns_per_day
  let final = iso_date_from_epoch_days(epoch_days(base) + extra_days)
  case iso_date_within_limits(final) {
    True -> Ok(#(final, ns_to_time(rem)))
    False -> Error(RangeE("date outside of supported range"))
  }
}

// ----------------------------------------------------------------------------
// total
// ----------------------------------------------------------------------------

/// Temporal.Duration.prototype.total ( totalOf )
fn duration_total(st: Agent, d: DurRec, args: List(JsVal)) -> #(JsVal, Agent) {
  let arg = helpers.arg_at(args, 0)
  case classify(arg) {
    KUndef -> rt_val.t_throw_type_error(st, "totalOf is required")
    KStr(u_str) ->
      case singular_unit(u_str) {
        Some(u) -> duration_total_with(st, d, u, RelNone)
        None -> rt_val.t_throw_range_error(st, "invalid unit")
      }
    KHandle(oh) -> {
      let #(rel_v, st) = get_named(st, oh, "relativeTo")
      let #(rel, st) = convert_relative_to(st, rel_v)
      let #(unit_o, st) =
        get_unit_option(st, Some(oh), "unit", allow_auto: False)
      case unit_o {
        None -> rt_val.t_throw_range_error(st, "unit is required")
        Some(u) -> duration_total_with(st, d, u, rel)
      }
    }
    _ -> rt_val.t_throw_type_error(st, "invalid totalOf")
  }
}

fn total_number(f: Float) -> JsVal {
  mk_number(JFloat(f))
}

fn duration_total_with(
  st: Agent,
  d: DurRec,
  unit: Unit,
  rel: RelTo,
) -> #(JsVal, Agent) {
  case rel {
    RelNone -> {
      let needs_rel =
        d.years != 0
        || d.months != 0
        || d.weeks != 0
        || unit_rank(unit) > unit_rank(Day)
      case needs_rel {
        True ->
          rt_val.t_throw_range_error(
            st,
            "relativeTo is required to total calendar units",
          )
        False -> {
          let tu = terr(st, require_time_unit(unit))
          let total = time_duration_ns(d)
          #(total_number(ns_div_float(total, time_unit_ns(tu))), st)
        }
      }
    }
    RelZoned(anchor_ns, tz, cal) -> {
      let target_ns = terr(st, add_zoned_ns(anchor_ns, tz, cal, d))
      case unit_rank(unit) <= unit_rank(Hour) {
        True -> {
          let tu = terr(st, require_time_unit(unit))
          let diff = target_ns - anchor_ns
          #(total_number(ns_div_float(diff, time_unit_ns(tu))), st)
        }
        False -> zoned_calendar_total(st, tz, anchor_ns, target_ns, unit)
      }
    }
    RelPlain(rel_date, _rel_cal) -> {
      // A zero duration totals to zero before the relativeTo date-time is
      // range-checked.
      let Nil =
        terr(
          st,
          case
            duration_sign(d) != 0
            && !iso_datetime_within_limits(rel_date, midnight)
          {
            True ->
              Error(RangeE(
                "relativeTo is outside the representable range after conversion to DateTime",
              ))
            False -> Ok(Nil)
          },
        )
      let target = terr(st, duration_target_datetime(rel_date, d))
      let rel_ns = epoch_days(rel_date) * ns_per_day
      let target_ns = epoch_days(target.0) * ns_per_day + time_to_ns(target.1)
      case unit_rank(unit) <= unit_rank(Day) {
        True -> {
          let tu = terr(st, require_time_unit(unit))
          let diff = target_ns - rel_ns
          #(total_number(ns_div_float(diff, time_unit_ns(tu))), st)
        }
        False -> plain_calendar_total(st, rel_date, rel_ns, target_ns, unit)
      }
    }
  }
}

/// Whole calendar units in wall-clock space + fractional progress between the
/// bounding instants (NudgeToCalendarUnit, zoned).
fn zoned_calendar_total(
  st: Agent,
  tz: types.TimeZone,
  anchor_ns: Int,
  target_ns: Int,
  unit: Unit,
) -> #(JsVal, Agent) {
  let #(a_d, a_t) = terr(st, epoch_ns_to_iso_in(tz, anchor_ns))
  let #(b_d, b_t) = terr(st, epoch_ns_to_iso_in(tz, target_ns))
  let sign = case target_ns < anchor_ns {
    True -> -1
    False -> 1
  }
  let tb = time_to_ns(b_t) - time_to_ns(a_t)
  let b_date = case sign > 0 && tb < 0, sign < 0 && tb > 0 {
    True, _ -> iso_date_from_epoch_days(epoch_days(b_d) - 1)
    _, True -> iso_date_from_epoch_days(epoch_days(b_d) + 1)
    _, _ -> b_d
  }
  let whole0 = case unit {
    Year -> diff_date_parts(a_d, b_date, Year).0
    Month -> diff_date_parts(a_d, b_date, Month).1
    Week -> trunc_div(epoch_days(b_date) - epoch_days(a_d), 7)
    _ -> epoch_days(b_date) - epoch_days(a_d)
  }
  let bound = fn(w: Int) {
    let date = case unit {
      Day -> iso_date_from_epoch_days(epoch_days(a_d) + w)
      _ -> add_calendar_units(a_d, unit, w)
    }
    get_epoch_ns_for(tz, date, a_t, Compatible)
  }
  let start0_ns = terr(st, bound(whole0))
  let end0_ns = terr(st, bound(whole0 + sign))
  let in_window = case sign > 0 {
    True -> start0_ns <= target_ns && target_ns <= end0_ns
    False -> end0_ns <= target_ns && target_ns <= start0_ns
  }
  let #(whole, start_ns, end_ns) = case in_window {
    True -> #(whole0, start0_ns, end0_ns)
    False -> {
      let e2 = terr(st, bound(whole0 + 2 * sign))
      #(whole0 + sign, end0_ns, e2)
    }
  }
  #(
    total_number(fractional_total(whole, sign, target_ns, start_ns, end_ns)),
    st,
  )
}

/// Whole calendar units + fractional progress between bounds
/// (NudgeToCalendarUnit). Per ComputeNudgeWindow, when the target falls
/// outside the first window the window is recomputed shifted by one unit.
/// That happens when day-of-month clamping makes the date diff undercount
/// (e.g. 2020-01-31 + 1 month lands on 2020-02-29, which diffs back as 0
/// months).
fn plain_calendar_total(
  st: Agent,
  rel_date: IsoDate,
  rel_ns: Int,
  target_ns: Int,
  unit: Unit,
) -> #(JsVal, Agent) {
  let sign = case target_ns < rel_ns {
    True -> -1
    False -> 1
  }
  let target_floor_days = floor_div(target_ns, ns_per_day)
  let target_date = iso_date_from_epoch_days(target_floor_days)
  let whole0 = case unit {
    Year -> diff_date_parts(rel_date, target_date, Year).0
    Month -> diff_date_parts(rel_date, target_date, Month).1
    _ -> trunc_div(epoch_days(target_date) - epoch_days(rel_date), 7)
  }
  // Window bounds come from CalendarDateAdd, which range-checks its result
  // (NudgeToCalendarUnit).
  let bound_ns = fn(w: Int) {
    use d2 <- result.map(
      check_date_limits(add_calendar_units(rel_date, unit, w)),
    )
    epoch_days(d2) * ns_per_day
  }
  let start0_ns = terr(st, bound_ns(whole0))
  let end0_ns = terr(st, bound_ns(whole0 + sign))
  let in_window = case sign > 0 {
    True -> start0_ns <= target_ns && target_ns <= end0_ns
    False -> end0_ns <= target_ns && target_ns <= start0_ns
  }
  let #(whole, start_ns, end_ns) =
    terr(st, case in_window {
      True -> Ok(#(whole0, start0_ns, end0_ns))
      False -> {
        use e2 <- result.map(bound_ns(whole0 + 2 * sign))
        #(whole0 + sign, end0_ns, e2)
      }
    })
  #(
    total_number(fractional_total(whole, sign, target_ns, start_ns, end_ns)),
    st,
  )
}

/// Single correctly-rounded division of the exact rational
/// whole + sign·num/den (NudgeToCalendarUnit's `total`).
fn fractional_total(
  whole: Int,
  sign: Int,
  target_ns: Int,
  start_ns: Int,
  end_ns: Int,
) -> Float {
  let num = target_ns - start_ns
  let den = end_ns - start_ns
  case den == 0 {
    True -> int.to_float(whole)
    False -> ns_div_float(whole * den + sign * num, den)
  }
}

// ----------------------------------------------------------------------------
// toString
// ----------------------------------------------------------------------------

/// ToSecondsStringPrecisionRecord for Duration.toString: only sub-minute
/// smallestUnit values are allowed. Returns #(precision, unit, increment).
fn duration_string_precision(
  digits: FractionalDigits,
  su: Option(Unit),
) -> Result(#(Precision, TimeUnit, Int), TErr) {
  case su {
    Some(Second) -> Ok(#(FixedPrec(0), USecond, 1))
    Some(Millisecond) -> Ok(#(FixedPrec(3), UMillisecond, 1))
    Some(Microsecond) -> Ok(#(FixedPrec(6), UMicrosecond, 1))
    Some(Nanosecond) -> Ok(#(FixedPrec(9), UNanosecond, 1))
    Some(u) ->
      Error(RangeE(
        unit_to_string(u)
        <> " is not a valid smallestUnit for Duration.toString",
      ))
    None ->
      case digits {
        DigitsAuto -> Ok(#(AutoPrec, UNanosecond, 1))
        DigitsFixed(0) -> Ok(#(FixedPrec(0), USecond, 1))
        DigitsFixed(n) -> Ok(#(FixedPrec(n), UNanosecond, pow10(9 - n)))
      }
  }
}

/// RoundTimeDuration + TemporalDurationFromInternal for Duration.toString:
/// round the time portion (hours and below) and rebalance, carrying into
/// days only when the duration's default largest unit is a date unit.
fn round_duration_for_string(
  d: DurRec,
  inc: Int,
  unit: TimeUnit,
  mode: RoundingMode,
) -> Result(DurRec, TErr) {
  let time_ns = time_only_ns(d)
  let rounded = round_to_increment(time_ns, inc * time_unit_ns(unit), mode)
  let largest = max_unit(default_largest_unit(d), Second)
  let result = case unit_rank(largest) >= unit_rank(Day) {
    True -> {
      let extra_days = trunc_div(rounded, ns_per_day)
      let rem = rounded - extra_days * ns_per_day
      let t = balance_time_ns(rem, Hour)
      DurRec(
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
    False -> Error(RangeE("rounded duration is out of range"))
  }
}

/// ISO 8601 duration serialization (TemporalDurationToString).
pub fn format_duration(d: DurRec, prec: Precision) -> String {
  let sign = duration_sign(d)
  let prefix = case sign < 0 {
    True -> "-"
    False -> ""
  }
  let abs_part = fn(n: Int) { int.absolute_value(n) }
  let date_part =
    join_unit(abs_part(d.years), "Y")
    <> join_unit(abs_part(d.months), "M")
    <> join_unit(abs_part(d.weeks), "W")
    <> join_unit(abs_part(d.days), "D")
  // Sub-second components may exceed their unit (e.g. 1.8e16 microseconds);
  // carry whole seconds out of the combined sub-second total.
  let sub_total =
    abs_part(d.ms) * ns_per_ms + abs_part(d.us) * ns_per_us + abs_part(d.ns)
  let extra_seconds = sub_total / ns_per_second
  let sub = sub_total % ns_per_second
  let seconds_str = case
    d.seconds != 0
    || sub_total != 0
    || { date_part == "" && d.hours == 0 && d.minutes == 0 }
    || show_fixed_seconds(prec)
  {
    True -> {
      let frac = case prec {
        AutoPrec -> format_fraction(sub, AutoPrec)
        FixedPrec(0) -> ""
        FixedPrec(n) -> {
          let digits9 = int.to_string(sub) |> string.pad_start(9, "0")
          "." <> string.slice(digits9, 0, n)
        }
        MinutePrec -> ""
      }
      int.to_string(abs_part(d.seconds) + extra_seconds) <> frac <> "S"
    }
    False -> ""
  }
  let time_part =
    join_unit(abs_part(d.hours), "H")
    <> join_unit(abs_part(d.minutes), "M")
    <> seconds_str
  let t = case time_part {
    "" -> ""
    _ -> "T" <> time_part
  }
  prefix <> "P" <> date_part <> t
}

fn show_fixed_seconds(p: Precision) -> Bool {
  case p {
    FixedPrec(_) -> True
    AutoPrec | MinutePrec -> False
  }
}

fn join_unit(n: Int, designator: String) -> String {
  case n == 0 {
    True -> ""
    False -> int.to_string(n) <> designator
  }
}
