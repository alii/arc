import arc/bytecode/error_kind.{type JsError, JsError, RangeError}
import arc/internal/int_math.{floor_div, floor_mod}
import arc/rt/builtins/helpers
import arc/rt/builtins/options.{get_options_object}
import arc/rt/builtins/temporal_common.{
  make_duration, make_time, negate_duration, read_int_field, require_temporal,
  static_name, time_part_ns, time_slot_of, to_temporal_duration,
  truncated_int_arg_or,
}
import arc/rt/builtins/temporal_fields.{
  check_parsed_calendar, is_month_day_like, is_year_month_like,
  require_nonempty_fields, require_partial_bag,
}
import arc/rt/builtins/temporal_iso.{
  type IsoTime, type Overflow, AutoPrecision, Constrain, IsoTime, NoOffset,
  NumericOffset, Reject, Zulu, format_iso_time, int_sign, is_valid_time,
  midnight, ns_per_day, ns_to_time, parse_annotations, parse_iso_datetime_string,
  parse_offset_part, parse_time_part, time_to_ns,
}
import arc/rt/builtins/temporal_options.{get_overflow_option_from_value}
import arc/rt/builtins/temporal_rounding.{
  Hour, Nanosecond, apply_since_mode, apply_since_ns, balance_time_ns,
  get_difference_settings, max_unit, require_largest_ge_smallest,
  require_time_unit, round_options, round_to_increment, time_unit_ns,
  to_string_time_options, unit_rank, valid_rounding_increment,
}
import arc/rt/builtins/temporal_time_zone.{epoch_ns_to_iso_in}
import arc/rt/store as rt_store
import arc/rt/temporal_data.{
  TemporalDateTime, TemporalTime, TemporalZonedDateTime,
}
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type NativeToken, type PlainTimeMethod,
  type TemporalProtos, type TemporalStaticName, type TemporalTimeGetter,
  CompareStatic, FromStatic, KHandle, KStr, PlainTimeAdd, PlainTimeEquals,
  PlainTimeRound, PlainTimeSince, PlainTimeSubtract, PlainTimeToJson,
  PlainTimeToLocaleString, PlainTimeToString, PlainTimeUntil, PlainTimeValueOf,
  PlainTimeWith, SObject, TemporalN, TemporalObj, TemporalPlainTimeCtor,
  TemporalPlainTimeGetter, TemporalPlainTimeMethod, TemporalPlainTimeStatic,
  TimeHour, TimeMicrosecond, TimeMillisecond, TimeMinute, TimeNanosecond,
  TimeSecond, classify, mk_bool, mk_int, mk_string,
}
import arc/rt/val as rt_val
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub const all_time_getters = [
  TimeHour,
  TimeMinute,
  TimeSecond,
  TimeMillisecond,
  TimeMicrosecond,
  TimeNanosecond,
]

pub fn ctor_token(protos: TemporalProtos) -> NativeToken {
  TemporalN(TemporalPlainTimeCtor(protos:))
}

pub fn statics(protos: TemporalProtos) -> List(#(String, NativeToken, Int)) {
  list.map([#(FromStatic, 1), #(CompareStatic, 2)], fn(s) {
    #(static_name(s.0), TemporalN(TemporalPlainTimeStatic(s.0, protos)), s.1)
  })
}

pub fn getters() -> List(#(String, NativeToken)) {
  list.map(all_time_getters, fn(g) {
    #(time_getter_name(g), TemporalN(TemporalPlainTimeGetter(g)))
  })
}

pub fn methods(protos: TemporalProtos) -> List(#(String, NativeToken, Int)) {
  list.map(
    [
      #(PlainTimeAdd, 1),
      #(PlainTimeSubtract, 1),
      #(PlainTimeWith, 1),
      #(PlainTimeUntil, 1),
      #(PlainTimeSince, 1),
      #(PlainTimeRound, 1),
      #(PlainTimeEquals, 1),
      #(PlainTimeToString, 0),
      #(PlainTimeToLocaleString, 0),
      #(PlainTimeToJson, 0),
      #(PlainTimeValueOf, 0),
    ],
    fn(m) {
      #(
        plain_time_method_name(m.0),
        TemporalN(TemporalPlainTimeMethod(m.0, protos)),
        m.1,
      )
    },
  )
}

pub fn time_getter_name(g: TemporalTimeGetter) -> String {
  case g {
    TimeHour -> "hour"
    TimeMinute -> "minute"
    TimeSecond -> "second"
    TimeMillisecond -> "millisecond"
    TimeMicrosecond -> "microsecond"
    TimeNanosecond -> "nanosecond"
  }
}

pub fn plain_time_method_name(m: PlainTimeMethod) -> String {
  case m {
    PlainTimeAdd -> "add"
    PlainTimeSubtract -> "subtract"
    PlainTimeWith -> "with"
    PlainTimeUntil -> "until"
    PlainTimeSince -> "since"
    PlainTimeRound -> "round"
    PlainTimeEquals -> "equals"
    PlainTimeToString -> "toString"
    PlainTimeToLocaleString -> "toLocaleString"
    PlainTimeToJson -> "toJSON"
    PlainTimeValueOf -> "valueOf"
  }
}

pub fn ctor(
  st: Agent,
  protos: TemporalProtos,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(hour, st) = truncated_int_arg_or(st, args, 0, 0)
  let #(minute, st) = truncated_int_arg_or(st, args, 1, 0)
  let #(second, st) = truncated_int_arg_or(st, args, 2, 0)
  let #(millisecond, st) = truncated_int_arg_or(st, args, 3, 0)
  let #(microsecond, st) = truncated_int_arg_or(st, args, 4, 0)
  let #(nanosecond, st) = truncated_int_arg_or(st, args, 5, 0)
  let t =
    IsoTime(hour:, minute:, second:, millisecond:, microsecond:, nanosecond:)
  case is_valid_time(t) {
    False -> rt_val.t_throw_range_error(st, "invalid time")
    True -> make_time(st, protos, t)
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
      let #(t, st) =
        to_temporal_time(st, helpers.arg_at(args, 0), helpers.arg_at(args, 1))
      make_time(st, protos, t)
    }
    CompareStatic -> {
      let #(a, st) =
        to_temporal_time(st, helpers.arg_at(args, 0), types.mk_undefined())
      let #(b, st) =
        to_temporal_time(st, helpers.arg_at(args, 1), types.mk_undefined())
      #(mk_int(int_sign(time_to_ns(a) - time_to_ns(b))), st)
    }
  }
}

pub fn getter(
  st: Agent,
  g: TemporalTimeGetter,
  this: JsVal,
) -> #(JsVal, Agent) {
  let t =
    require_temporal(st, this, "PlainTime", time_getter_name(g), time_slot_of)
  #(time_field(t, g), st)
}

pub fn time_field(t: IsoTime, g: TemporalTimeGetter) -> JsVal {
  let n = case g {
    TimeHour -> t.hour
    TimeMinute -> t.minute
    TimeSecond -> t.second
    TimeMillisecond -> t.millisecond
    TimeMicrosecond -> t.microsecond
    TimeNanosecond -> t.nanosecond
  }
  mk_int(n)
}

pub fn method(
  st: Agent,
  m: PlainTimeMethod,
  protos: TemporalProtos,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let t =
    require_temporal(
      st,
      this,
      "PlainTime",
      plain_time_method_name(m),
      time_slot_of,
    )
  case m {
    PlainTimeToJson | PlainTimeToLocaleString -> #(
      mk_string(format_iso_time(t, AutoPrecision)),
      st,
    )
    PlainTimeToString -> {
      let #(opts, st) = get_options_object(st, helpers.arg_at(args, 0))
      let #(#(precision, smallest_time_unit, inc, mode), st) =
        to_string_time_options(st, opts)
      let t2 = case smallest_time_unit {
        None -> t
        Some(u) -> {
          let rounded =
            round_to_increment(time_to_ns(t), inc * time_unit_ns(u), mode)
          ns_to_time(floor_mod(rounded, ns_per_day))
        }
      }
      #(mk_string(format_iso_time(t2, precision)), st)
    }
    PlainTimeValueOf ->
      rt_val.t_throw_type_error(
        st,
        "Temporal.PlainTime cannot be converted with valueOf",
      )
    PlainTimeEquals -> {
      let #(other, st) =
        to_temporal_time(st, helpers.arg_at(args, 0), types.mk_undefined())
      #(mk_bool(t == other), st)
    }
    PlainTimeAdd | PlainTimeSubtract -> {
      let #(dur, st) = to_temporal_duration(st, helpers.arg_at(args, 0))
      let dur = case m {
        PlainTimeSubtract -> negate_duration(dur)
        _ -> dur
      }
      let #(_, t2) = add_time(t, time_part_ns(dur))
      make_time(st, protos, t2)
    }
    PlainTimeWith -> {
      let #(bag, st) = require_partial_bag(st, helpers.arg_at(args, 0))
      let #(f, st) = read_time_fields(st, bag)
      let Nil = require_nonempty_fields(st, f == no_time_fields)
      let #(overflow, st) =
        get_overflow_option_from_value(st, helpers.arg_at(args, 1))
      let t2 = time_fields_apply(f, t)
      let t3 = rt_val.or_throw(st, regulate_time(t2, overflow))
      make_time(st, protos, t3)
    }
    PlainTimeRound -> {
      let #(#(smallest_time_unit, inc, mode), st) =
        round_options(st, helpers.arg_at(args, 0), allow_day: False)
      let unit_ns = time_unit_ns(smallest_time_unit)
      let max = ns_per_day / unit_ns
      case valid_rounding_increment(inc, max, inclusive: False) {
        False -> rt_val.t_throw_range_error(st, "invalid roundingIncrement")
        True -> {
          let rounded = round_to_increment(time_to_ns(t), inc * unit_ns, mode)
          let t2 = ns_to_time(floor_mod(rounded, ns_per_day))
          make_time(st, protos, t2)
        }
      }
    }
    PlainTimeUntil | PlainTimeSince -> {
      let #(other, st) =
        to_temporal_time(st, helpers.arg_at(args, 0), types.mk_undefined())
      time_until_since(st, protos, t, other, args, m == PlainTimeSince)
    }
  }
}

fn time_until_since(
  st: Agent,
  protos: TemporalProtos,
  t1: IsoTime,
  t2: IsoTime,
  args: List(JsVal),
  is_since is_since: Bool,
) -> #(JsVal, Agent) {
  let #(#(largest, smallest, inc, mode), st) = get_difference_settings(st, args)
  let smallest = option.unwrap(smallest, Nanosecond)
  let largest = option.unwrap(largest, max_unit(smallest, Hour))
  case
    unit_rank(smallest) > unit_rank(Hour)
    || unit_rank(largest) > unit_rank(Hour)
  {
    True ->
      rt_val.t_throw_range_error(st, "units must be time units for PlainTime")
    False -> {
      let Nil = require_largest_ge_smallest(st, largest, smallest)
      let smallest_time_unit = rt_val.or_throw(st, require_time_unit(smallest))
      let mode = apply_since_mode(mode, is_since)
      let diff = time_to_ns(t2) - time_to_ns(t1)
      let rounded =
        round_to_increment(diff, inc * time_unit_ns(smallest_time_unit), mode)
      let rounded = apply_since_ns(rounded, is_since)
      let dur = balance_time_ns(rounded, largest)
      make_duration(st, protos, dur)
    }
  }
}

pub type TimeFields {
  TimeFields(
    hour: Option(Int),
    minute: Option(Int),
    second: Option(Int),
    millisecond: Option(Int),
    microsecond: Option(Int),
    nanosecond: Option(Int),
  )
}

pub const no_time_fields = TimeFields(None, None, None, None, None, None)

pub fn time_fields_apply(f: TimeFields, base: IsoTime) -> IsoTime {
  IsoTime(
    hour: option.unwrap(f.hour, base.hour),
    minute: option.unwrap(f.minute, base.minute),
    second: option.unwrap(f.second, base.second),
    millisecond: option.unwrap(f.millisecond, base.millisecond),
    microsecond: option.unwrap(f.microsecond, base.microsecond),
    nanosecond: option.unwrap(f.nanosecond, base.nanosecond),
  )
}

pub fn read_time_fields(st: Agent, bag: Handle) -> #(TimeFields, Agent) {
  let #(hour, st) = read_int_field(st, bag, "hour")
  let #(microsecond, st) = read_int_field(st, bag, "microsecond")
  let #(millisecond, st) = read_int_field(st, bag, "millisecond")
  let #(minute, st) = read_int_field(st, bag, "minute")
  let #(nanosecond, st) = read_int_field(st, bag, "nanosecond")
  let #(second, st) = read_int_field(st, bag, "second")
  #(
    TimeFields(hour:, minute:, second:, millisecond:, microsecond:, nanosecond:),
    st,
  )
}

pub fn regulate_time(
  t: IsoTime,
  overflow: Overflow,
) -> Result(IsoTime, JsError) {
  case overflow {
    Reject ->
      case is_valid_time(t) {
        True -> Ok(t)
        False -> Error(JsError(RangeError, "time out of range"))
      }
    Constrain ->
      Ok(IsoTime(
        hour: int.clamp(t.hour, 0, 23),
        minute: int.clamp(t.minute, 0, 59),
        second: int.clamp(t.second, 0, 59),
        millisecond: int.clamp(t.millisecond, 0, 999),
        microsecond: int.clamp(t.microsecond, 0, 999),
        nanosecond: int.clamp(t.nanosecond, 0, 999),
      ))
  }
}

pub fn to_temporal_time(
  st: Agent,
  item: JsVal,
  options: JsVal,
) -> #(IsoTime, Agent) {
  case classify(item) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(
          kind: TemporalObj(data: TemporalTime(
            hour:,
            minute:,
            second:,
            millisecond:,
            microsecond:,
            nanosecond:,
          )),
          ..,
        )
        | SObject(
            kind: TemporalObj(data: TemporalDateTime(
              hour:,
              minute:,
              second:,
              millisecond:,
              microsecond:,
              nanosecond:,
              ..,
            )),
            ..,
          ) -> {
          let #(_o, st) = get_overflow_option_from_value(st, options)
          #(
            IsoTime(hour, minute, second, millisecond, microsecond, nanosecond),
            st,
          )
        }
        SObject(
          kind: TemporalObj(data: TemporalZonedDateTime(
            epoch_ns:,
            time_zone:,
            calendar: _,
          )),
          ..,
        ) -> {
          let #(_o, st) = get_overflow_option_from_value(st, options)
          let #(_, t) = epoch_ns_to_iso_in(time_zone, epoch_ns)
          #(t, st)
        }
        _ -> time_from_bag(st, h, options)
      }
    KStr(s) -> {
      let t = rt_val.or_throw(st, parse_time_string(s))
      let #(_o, st) = get_overflow_option_from_value(st, options)
      #(t, st)
    }
    _ -> rt_val.t_throw_type_error(st, "cannot convert to a Temporal.PlainTime")
  }
}

pub fn parse_time_string(s: String) -> Result(IsoTime, JsError) {
  case parse_iso_datetime_string(s) {
    Some(p) ->
      case p.offset {
        Zulu ->
          Error(JsError(RangeError, "Z designator not valid for PlainTime"))
        NoOffset | NumericOffset(_, _) ->
          case p.time {
            Some(t) -> {
              use Nil <- result.map(check_parsed_calendar(p))
              t
            }
            None -> Error(JsError(RangeError, "no time in string"))
          }
      }
    None -> {
      let #(body, explicit_t) = case s {
        "T" <> r | "t" <> r -> #(r, True)
        _ -> #(s, False)
      }
      case parse_time_with_annotations(body) {
        Some(t) ->
          case !explicit_t && time_string_is_ambiguous(body) {
            True -> Error(JsError(RangeError, "ambiguous time string"))
            False -> Ok(t)
          }
        None -> Error(JsError(RangeError, "invalid time string: " <> s))
      }
    }
  }
}

fn parse_time_with_annotations(s: String) -> Option(IsoTime) {
  use #(t, rest) <- option.then(parse_time_part(s))
  use rest <- option.then(case parse_offset_part(rest) {
    Some(#(Zulu, _)) -> None
    Some(#(_, r)) -> Some(r)
    None -> Some(rest)
  })
  use #(_, _cal, rest2) <- option.then(parse_annotations(
    rest,
    None,
    None,
    cal_critical: False,
  ))
  case rest2 {
    "" -> Some(t)
    _ -> None
  }
}

// time string that also matches date syntax is ambiguous, reject
fn time_string_is_ambiguous(s: String) -> Bool {
  let base = case string.split_once(s, "[") {
    Ok(#(b, _)) -> b
    Error(Nil) -> s
  }
  is_year_month_like(base) || is_month_day_like(base)
}

pub fn time_from_bag(
  st: Agent,
  bag: Handle,
  options: JsVal,
) -> #(IsoTime, Agent) {
  let #(f, st) = read_time_fields(st, bag)
  case f == no_time_fields {
    True ->
      rt_val.t_throw_type_error(
        st,
        "invalid property bag for Temporal.PlainTime",
      )
    False -> {
      let #(overflow, st) = get_overflow_option_from_value(st, options)
      let t0 = time_fields_apply(f, midnight)
      #(rt_val.or_throw(st, regulate_time(t0, overflow)), st)
    }
  }
}

pub fn add_time(t: IsoTime, add_ns: Int) -> #(Int, IsoTime) {
  let total = time_to_ns(t) + add_ns
  let days = floor_div(total, ns_per_day)
  let rem = total - days * ns_per_day
  #(days, ns_to_time(rem))
}
