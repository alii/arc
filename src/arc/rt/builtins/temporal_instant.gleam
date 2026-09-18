import arc/internal/int_math.{floor_div}
import arc/rt/builtins/helpers
import arc/rt/builtins/options.{get_options_object, opt_get}
import arc/rt/builtins/temporal_common.{
  has_date_units, instant_slot_of, make_duration, make_instant, make_zoned,
  require_temporal, time_part_ns, to_temporal_duration,
}
import arc/rt/builtins/temporal_iso.{
  type SecondsPrecision, AutoPrecision, NoOffset, NumericOffset, Zulu,
  epoch_ns_to_iso, format_iso_date, format_iso_time, ns_per_day, ns_per_ms,
  parse_iso_datetime_string, utc_epoch_ns,
}
import arc/rt/builtins/temporal_rounding.{
  Hour, Nanosecond, Second, Trunc, apply_since_mode, apply_since_ns,
  as_if_positive_mode, balance_time_ns, check_diff_setup,
  get_difference_settings, get_fractional_digits, get_rounding_mode_option,
  get_unit_option, max_unit, require_time_unit, round_options,
  round_to_increment, seconds_string_precision, time_unit_ns, unit_rank,
  valid_rounding_increment,
}
import arc/rt/builtins/temporal_time_zone.{
  format_offset_rounded, is_valid_epoch_ns, parse_offset_tz_id,
  to_temporal_time_zone, tz_offset_ns_at, validate_epoch_ns,
}
import arc/rt/store as rt_store
import arc/rt/temporal_data.{TemporalInstant, TemporalZonedDateTime}
import arc/rt/types.{
  type Agent, type InstantGetterName, type InstantMethodName,
  type InstantStaticName, type JsVal, type TemporalProtos, HintString,
  InstantAdd, InstantCompare, InstantEpochMilliseconds, InstantEpochNanoseconds,
  InstantEquals, InstantFrom, InstantFromEpochMilliseconds,
  InstantFromEpochNanoseconds, InstantRound, InstantSince, InstantSubtract,
  InstantToJson, InstantToLocaleString, InstantToString,
  InstantToZonedDateTimeIso, InstantUntil, InstantValueOf, JFloat, JInt, JNan,
  JNegInf, JPosInf, KHandle, KStr, KUndef, SObject, TemporalObj, classify,
  mk_bigint, mk_bool, mk_int, mk_string,
}
import arc/rt/val as rt_val
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/order

pub fn to_temporal_instant(st: Agent, item: JsVal) -> #(Int, Agent) {
  case classify(item) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: TemporalObj(data: TemporalInstant(epoch_ns:)), ..) -> #(
          epoch_ns,
          st,
        )
        SObject(
          kind: TemporalObj(data: TemporalZonedDateTime(epoch_ns:, ..)),
          ..,
        ) -> #(epoch_ns, st)
        _ -> {
          let #(prim, st) = rt_val.t_to_primitive(st, item, HintString)
          case classify(prim) {
            KStr(s) -> parse_instant_to_ns(st, s)
            _ ->
              rt_val.t_throw_type_error(
                st,
                "cannot convert to a Temporal.Instant",
              )
          }
        }
      }
    KStr(s) -> parse_instant_to_ns(st, s)
    _ -> rt_val.t_throw_type_error(st, "cannot convert to a Temporal.Instant")
  }
}

fn parse_instant_to_ns(st: Agent, s: String) -> #(Int, Agent) {
  // calendar annotation value is ignored for instant
  case parse_iso_datetime_string(s) {
    None -> rt_val.t_throw_range_error(st, "invalid instant string: " <> s)
    Some(p) ->
      case p.time, valid_tz_annotation(p.tz) {
        _, False ->
          rt_val.t_throw_range_error(
            st,
            "invalid time zone annotation in instant string: " <> s,
          )
        Some(t), True ->
          case p.offset {
            NoOffset ->
              rt_val.t_throw_range_error(
                st,
                "instant string requires a UTC offset",
              )
            Zulu | NumericOffset(_, _) -> {
              let off = case p.offset {
                NumericOffset(o, _) -> o
                Zulu | NoOffset -> 0
              }
              let ns = utc_epoch_ns(p.date, t) - off
              #(rt_val.or_throw(st, validate_epoch_ns(ns)), st)
            }
          }
        None, True ->
          rt_val.t_throw_range_error(st, "instant string requires a time")
      }
  }
}

fn valid_tz_annotation(tz: Option(String)) -> Bool {
  case tz {
    Some("+" <> _ as ann) | Some("-" <> _ as ann) ->
      option.is_some(parse_offset_tz_id(ann))
    Some(_) | None -> True
  }
}

pub fn instant_getter_name(g: InstantGetterName) -> String {
  case g {
    InstantEpochMilliseconds -> "epochMilliseconds"
    InstantEpochNanoseconds -> "epochNanoseconds"
  }
}

pub fn instant_method_name(m: InstantMethodName) -> String {
  case m {
    InstantAdd -> "add"
    InstantSubtract -> "subtract"
    InstantUntil -> "until"
    InstantSince -> "since"
    InstantRound -> "round"
    InstantEquals -> "equals"
    InstantToString -> "toString"
    InstantToLocaleString -> "toLocaleString"
    InstantToJson -> "toJSON"
    InstantValueOf -> "valueOf"
    InstantToZonedDateTimeIso -> "toZonedDateTimeISO"
  }
}

pub fn instant_static_name(s: InstantStaticName) -> String {
  case s {
    InstantFrom -> "from"
    InstantFromEpochMilliseconds -> "fromEpochMilliseconds"
    InstantFromEpochNanoseconds -> "fromEpochNanoseconds"
    InstantCompare -> "compare"
  }
}

fn require_instant(st: Agent, this: JsVal, name: String) -> Int {
  require_temporal(st, this, "Instant", name, instant_slot_of)
}

pub fn instant_from_epoch_ns(
  st: Agent,
  protos: TemporalProtos,
  arg: JsVal,
) -> #(JsVal, Agent) {
  let #(ns, st) = rt_val.t_to_bigint(st, arg)
  case is_valid_epoch_ns(ns) {
    False -> rt_val.t_throw_range_error(st, "epoch nanoseconds out of range")
    True -> make_instant(st, protos, ns)
  }
}

pub fn instant_static(
  st: Agent,
  name: InstantStaticName,
  protos: TemporalProtos,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case name {
    InstantFrom -> {
      let #(ns, st) = to_temporal_instant(st, helpers.arg_at(args, 0))
      make_instant(st, protos, ns)
    }
    InstantCompare -> {
      let #(a, st) = to_temporal_instant(st, helpers.arg_at(args, 0))
      let #(b, st) = to_temporal_instant(st, helpers.arg_at(args, 1))
      #(mk_int(int.compare(a, b) |> order_to_int), st)
    }
    InstantFromEpochMilliseconds -> {
      let #(n, st) = rt_val.t_to_number(st, helpers.arg_at(args, 0))
      // -0 is integral, so use the ±0-safe check
      let i = case n {
        JInt(i) -> Some(i)
        JFloat(f) -> rt_val.integral_int(f)
        JNan | JPosInf | JNegInf ->
          rt_val.t_throw_range_error(st, "not a finite number")
      }
      case i {
        None -> rt_val.t_throw_range_error(st, "not an integral number")
        Some(i) -> {
          let ns = i * ns_per_ms
          case is_valid_epoch_ns(ns) {
            False ->
              rt_val.t_throw_range_error(st, "epoch milliseconds out of range")
            True -> make_instant(st, protos, ns)
          }
        }
      }
    }
    InstantFromEpochNanoseconds ->
      instant_from_epoch_ns(st, protos, helpers.arg_at(args, 0))
  }
}

fn order_to_int(o: order.Order) -> Int {
  case o {
    order.Lt -> -1
    order.Eq -> 0
    order.Gt -> 1
  }
}

pub fn instant_getter(
  st: Agent,
  g: InstantGetterName,
  this: JsVal,
) -> #(JsVal, Agent) {
  let ns = require_instant(st, this, instant_getter_name(g))
  case g {
    InstantEpochMilliseconds -> #(mk_int(floor_div(ns, ns_per_ms)), st)
    InstantEpochNanoseconds -> #(mk_bigint(ns), st)
  }
}

pub fn instant_method(
  st: Agent,
  m: InstantMethodName,
  protos: TemporalProtos,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let ns = require_instant(st, this, instant_method_name(m))
  case m {
    InstantToJson | InstantToLocaleString -> #(
      mk_string(format_instant(ns, AutoPrecision)),
      st,
    )
    InstantToString -> {
      let #(opts, st) = get_options_object(st, helpers.arg_at(args, 0))
      let #(digits, st) = get_fractional_digits(st, opts)
      let #(mode, st) = get_rounding_mode_option(st, opts, Trunc)
      let #(smallest, st) =
        get_unit_option(st, opts, "smallestUnit", allow_auto: False)
      let #(tz_opt, st) = opt_get(st, opts, "timeZone")
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
      case classify(tz_opt) {
        KUndef -> #(mk_string(format_instant(rounded, precision)), st)
        _ -> {
          let #(tz, st) = to_temporal_time_zone(st, tz_opt)
          let off = tz_offset_ns_at(tz, rounded)
          let #(d, t) = epoch_ns_to_iso(rounded, off)
          let s =
            format_iso_date(d)
            <> "T"
            <> format_iso_time(t, precision)
            <> format_offset_rounded(off)
          #(mk_string(s), st)
        }
      }
    }
    InstantValueOf ->
      rt_val.t_throw_type_error(
        st,
        "Temporal.Instant cannot be converted with valueOf",
      )
    InstantEquals -> {
      let #(other, st) = to_temporal_instant(st, helpers.arg_at(args, 0))
      #(mk_bool(ns == other), st)
    }
    InstantAdd | InstantSubtract -> {
      let #(dur, st) = to_temporal_duration(st, helpers.arg_at(args, 0))
      case has_date_units(dur) {
        True ->
          rt_val.t_throw_range_error(
            st,
            "Instant arithmetic does not support date units",
          )
        False -> {
          let delta = case m {
            InstantSubtract -> 0 - time_part_ns(dur)
            _ -> time_part_ns(dur)
          }
          make_instant(
            st,
            protos,
            rt_val.or_throw(st, validate_epoch_ns(ns + delta)),
          )
        }
      }
    }
    InstantRound -> {
      let #(#(smallest_time_unit, inc, mode), st) =
        round_options(st, helpers.arg_at(args, 0), allow_day: False)
      let unit_ns = time_unit_ns(smallest_time_unit)
      let max = ns_per_day / unit_ns
      case valid_rounding_increment(inc, max, inclusive: True) {
        False -> rt_val.t_throw_range_error(st, "invalid roundingIncrement")
        True -> {
          // rounds as if positive: down is toward the big bang
          let rounded =
            round_to_increment(ns, inc * unit_ns, as_if_positive_mode(mode))
          make_instant(
            st,
            protos,
            rt_val.or_throw(st, validate_epoch_ns(rounded)),
          )
        }
      }
    }
    InstantUntil | InstantSince -> {
      let #(other, st) = to_temporal_instant(st, helpers.arg_at(args, 0))
      instant_until_since(st, protos, ns, other, args, m == InstantSince)
    }
    InstantToZonedDateTimeIso -> {
      let #(tz, st) = to_temporal_time_zone(st, helpers.arg_at(args, 0))
      make_zoned(st, protos, ns, tz)
    }
  }
}

fn format_instant(ns: Int, precision: SecondsPrecision) -> String {
  let #(d, t) = epoch_ns_to_iso(ns, 0)
  format_iso_date(d) <> "T" <> format_iso_time(t, precision) <> "Z"
}

fn instant_until_since(
  st: Agent,
  protos: TemporalProtos,
  a: Int,
  b: Int,
  args: List(JsVal),
  is_since is_since: Bool,
) -> #(JsVal, Agent) {
  let #(#(largest, smallest, inc, mode), st) = get_difference_settings(st, args)
  let smallest = option.unwrap(smallest, Nanosecond)
  let largest = option.unwrap(largest, max_unit(smallest, Second))
  case
    unit_rank(smallest) > unit_rank(Hour)
    || unit_rank(largest) > unit_rank(Hour)
  {
    True ->
      rt_val.t_throw_range_error(st, "units must be time units for Instant")
    False -> {
      let Nil = check_diff_setup(st, largest, smallest, inc)
      let smallest_time_unit = rt_val.or_throw(st, require_time_unit(smallest))
      let mode = apply_since_mode(mode, is_since)
      let diff = b - a
      let rounded =
        round_to_increment(diff, inc * time_unit_ns(smallest_time_unit), mode)
      let rounded = apply_since_ns(rounded, is_since)
      let dur = balance_time_ns(rounded, largest)
      make_duration(st, protos, dur)
    }
  }
}
