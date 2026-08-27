import arc/internal/int_math.{floor_div, floor_mod as math_mod}
import arc/rt/builtins/helpers
import arc/rt/builtins/temporal_common.{
  Hour, Nanosecond, apply_since_mode, apply_since_ns, balance_time_ns,
  epoch_ns_to_iso_in, get_difference_settings, get_options_object, make_duration,
  make_time, max_unit, negate_dur, read_int_field, require_largest_ge_smallest,
  require_temporal, require_time_unit, round_options, round_to_increment, terr,
  time_only_ns, time_slot_of, time_unit_ns, to_string_time_options,
  to_temporal_duration, unit_rank, valid_time_increment,
} as tc
import arc/rt/builtins/temporal_fields.{
  check_parsed_calendar, is_month_day_like, is_year_month_like,
  require_nonempty_fields, require_partial_bag, validated_overflow,
}
import arc/rt/builtins/temporal_iso.{
  type Overflow, type TErr, type TimeRec, AutoPrec, Constrain, NoOffset,
  NumericOffset, RangeE, Reject, TimeRec, Zulu, format_iso_time, int_sign,
  is_valid_time, midnight, ns_per_day, ns_to_time, parse_annotations,
  parse_iso_datetime_string, parse_offset_part, parse_time_part, time_to_ns,
}
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type NativeToken, type PlainTimeMethod,
  type TemporalProtos, type TemporalStaticName, type TemporalTimeGetter, JInt,
  KHandle, KStr, PtAdd, PtEquals, PtRound, PtSince, PtSubtract, PtToJson,
  PtToLocaleString, PtToString, PtUntil, PtValueOf, PtWith, SObject,
  TemporalDateTime, TemporalN, TemporalObj, TemporalPlainTimeCtor,
  TemporalPlainTimeGetter, TemporalPlainTimeMethod, TemporalPlainTimeStatic,
  TemporalTime, TemporalZonedDateTime, TgHour, TgMicrosecond, TgMillisecond,
  TgMinute, TgNanosecond, TgSecond, TsCompare, TsFrom, classify, mk_bool,
  mk_number, mk_string,
}
import arc/rt/val as rt_val
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub const all_time_getters = [
  TgHour,
  TgMinute,
  TgSecond,
  TgMillisecond,
  TgMicrosecond,
  TgNanosecond,
]

pub fn ctor_token(protos: TemporalProtos) -> NativeToken {
  TemporalN(TemporalPlainTimeCtor(protos:))
}

pub fn statics(protos: TemporalProtos) -> List(#(String, NativeToken, Int)) {
  list.map([#(TsFrom, 1), #(TsCompare, 2)], fn(s) {
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
      #(PtAdd, 1),
      #(PtSubtract, 1),
      #(PtWith, 1),
      #(PtUntil, 1),
      #(PtSince, 1),
      #(PtRound, 1),
      #(PtEquals, 1),
      #(PtToString, 0),
      #(PtToLocaleString, 0),
      #(PtToJson, 0),
      #(PtValueOf, 0),
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

fn static_name(s: TemporalStaticName) -> String {
  case s {
    TsFrom -> "from"
    TsCompare -> "compare"
  }
}

pub fn time_getter_name(g: TemporalTimeGetter) -> String {
  case g {
    TgHour -> "hour"
    TgMinute -> "minute"
    TgSecond -> "second"
    TgMillisecond -> "millisecond"
    TgMicrosecond -> "microsecond"
    TgNanosecond -> "nanosecond"
  }
}

pub fn plain_time_method_name(m: PlainTimeMethod) -> String {
  case m {
    PtAdd -> "add"
    PtSubtract -> "subtract"
    PtWith -> "with"
    PtUntil -> "until"
    PtSince -> "since"
    PtRound -> "round"
    PtEquals -> "equals"
    PtToString -> "toString"
    PtToLocaleString -> "toLocaleString"
    PtToJson -> "toJSON"
    PtValueOf -> "valueOf"
  }
}

pub fn ctor(
  st: Agent,
  protos: TemporalProtos,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(h, st) = tc.opt_int_arg(st, args, 0)
  let #(mi, st) = tc.opt_int_arg(st, args, 1)
  let #(s, st) = tc.opt_int_arg(st, args, 2)
  let #(ms, st) = tc.opt_int_arg(st, args, 3)
  let #(us, st) = tc.opt_int_arg(st, args, 4)
  let #(ns, st) = tc.opt_int_arg(st, args, 5)
  let t = TimeRec(h, mi, s, ms, us, ns)
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
    TsFrom -> {
      let #(t, st) =
        to_temporal_time(st, helpers.arg_at(args, 0), helpers.arg_at(args, 1))
      make_time(st, protos, t)
    }
    TsCompare -> {
      let #(a, st) =
        to_temporal_time(st, helpers.arg_at(args, 0), types.mk_undefined())
      let #(b, st) =
        to_temporal_time(st, helpers.arg_at(args, 1), types.mk_undefined())
      #(mk_number(JInt(int_sign(time_to_ns(a) - time_to_ns(b)))), st)
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

pub fn time_field(t: TimeRec, g: TemporalTimeGetter) -> JsVal {
  let n = case g {
    TgHour -> t.hour
    TgMinute -> t.minute
    TgSecond -> t.second
    TgMillisecond -> t.ms
    TgMicrosecond -> t.us
    TgNanosecond -> t.ns
  }
  mk_number(JInt(n))
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
    PtToJson | PtToLocaleString -> #(
      mk_string(format_iso_time(t, AutoPrec)),
      st,
    )
    PtToString -> {
      let #(opts, st) = get_options_object(st, helpers.arg_at(args, 0))
      let #(#(prec, su, sinc, mode), st) = to_string_time_options(st, opts)
      let t2 = case su {
        None -> t
        Some(u) -> {
          let rounded =
            round_to_increment(time_to_ns(t), sinc * time_unit_ns(u), mode)
          ns_to_time(math_mod(rounded, ns_per_day))
        }
      }
      #(mk_string(format_iso_time(t2, prec)), st)
    }
    PtValueOf ->
      rt_val.t_throw_type_error(
        st,
        "Temporal.PlainTime cannot be converted with valueOf",
      )
    PtEquals -> {
      let #(other, st) =
        to_temporal_time(st, helpers.arg_at(args, 0), types.mk_undefined())
      #(mk_bool(t == other), st)
    }
    PtAdd | PtSubtract -> {
      let #(dur, st) = to_temporal_duration(st, helpers.arg_at(args, 0))
      let dur = case m {
        PtSubtract -> negate_dur(dur)
        _ -> dur
      }
      let #(_, t2) = add_time(t, time_only_ns(dur))
      make_time(st, protos, t2)
    }
    PtWith -> {
      let #(bag, st) = require_partial_bag(st, helpers.arg_at(args, 0))
      let #(f, st) = read_time_fields(st, bag)
      let Nil = require_nonempty_fields(st, f == no_time_fields)
      let #(overflow, st) = validated_overflow(st, helpers.arg_at(args, 1))
      let t2 = time_fields_apply(f, t)
      let t3 = terr(st, regulate_time(t2, overflow))
      make_time(st, protos, t3)
    }
    PtRound -> {
      let #(#(su, inc, mode), st) =
        round_options(st, helpers.arg_at(args, 0), allow_day: False)
      let u_ns = time_unit_ns(su)
      let max = ns_per_day / u_ns
      case valid_time_increment(inc, max) {
        False -> rt_val.t_throw_range_error(st, "invalid roundingIncrement")
        True -> {
          let rounded = round_to_increment(time_to_ns(t), inc * u_ns, mode)
          let t2 = ns_to_time(math_mod(rounded, ns_per_day))
          make_time(st, protos, t2)
        }
      }
    }
    PtUntil | PtSince -> {
      let #(other, st) =
        to_temporal_time(st, helpers.arg_at(args, 0), types.mk_undefined())
      time_until_since(st, protos, t, other, args, m == PtSince)
    }
  }
}

fn time_until_since(
  st: Agent,
  protos: TemporalProtos,
  t1: TimeRec,
  t2: TimeRec,
  args: List(JsVal),
  is_since: Bool,
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
      let su = terr(st, require_time_unit(smallest))
      let mode2 = apply_since_mode(mode, is_since)
      let diff = time_to_ns(t2) - time_to_ns(t1)
      let rounded = round_to_increment(diff, inc * time_unit_ns(su), mode2)
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
    ms: Option(Int),
    us: Option(Int),
    ns: Option(Int),
  )
}

pub const no_time_fields = TimeFields(None, None, None, None, None, None)

pub fn time_fields_apply(f: TimeFields, base: TimeRec) -> TimeRec {
  TimeRec(
    hour: option.unwrap(f.hour, base.hour),
    minute: option.unwrap(f.minute, base.minute),
    second: option.unwrap(f.second, base.second),
    ms: option.unwrap(f.ms, base.ms),
    us: option.unwrap(f.us, base.us),
    ns: option.unwrap(f.ns, base.ns),
  )
}

pub fn read_time_fields(st: Agent, bag: Handle) -> #(TimeFields, Agent) {
  let #(hour, st) = read_int_field(st, bag, "hour")
  let #(us, st) = read_int_field(st, bag, "microsecond")
  let #(ms, st) = read_int_field(st, bag, "millisecond")
  let #(minute, st) = read_int_field(st, bag, "minute")
  let #(ns, st) = read_int_field(st, bag, "nanosecond")
  let #(second, st) = read_int_field(st, bag, "second")
  #(TimeFields(hour:, minute:, second:, ms:, us:, ns:), st)
}

pub fn regulate_time(t: TimeRec, overflow: Overflow) -> Result(TimeRec, TErr) {
  case overflow {
    Reject ->
      case is_valid_time(t) {
        True -> Ok(t)
        False -> Error(RangeE("time out of range"))
      }
    Constrain ->
      Ok(TimeRec(
        hour: int.clamp(t.hour, 0, 23),
        minute: int.clamp(t.minute, 0, 59),
        second: int.clamp(t.second, 0, 59),
        ms: int.clamp(t.ms, 0, 999),
        us: int.clamp(t.us, 0, 999),
        ns: int.clamp(t.ns, 0, 999),
      ))
  }
}

pub fn to_temporal_time(
  st: Agent,
  item: JsVal,
  options: JsVal,
) -> #(TimeRec, Agent) {
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
          let #(_o, st) = validated_overflow(st, options)
          #(
            TimeRec(hour, minute, second, millisecond, microsecond, nanosecond),
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
          let #(_o, st) = validated_overflow(st, options)
          let #(_, t) = terr(st, epoch_ns_to_iso_in(time_zone, epoch_ns))
          #(t, st)
        }
        _ -> time_from_bag(st, h, options)
      }
    KStr(s) -> {
      let t = terr(st, parse_time_string(s))
      let #(_o, st) = validated_overflow(st, options)
      #(t, st)
    }
    _ -> rt_val.t_throw_type_error(st, "cannot convert to a Temporal.PlainTime")
  }
}

pub fn parse_time_string(s: String) -> Result(TimeRec, TErr) {
  case parse_iso_datetime_string(s) {
    Some(p) ->
      case p.offset {
        Zulu -> Error(RangeE("Z designator not valid for PlainTime"))
        NoOffset | NumericOffset(_, _) ->
          case p.time {
            Some(t) -> {
              use Nil <- result.map(check_parsed_calendar(p))
              t
            }
            None -> Error(RangeE("no time in string"))
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
            True -> Error(RangeE("ambiguous time string"))
            False -> Ok(t)
          }
        None -> Error(RangeE("invalid time string: " <> s))
      }
    }
  }
}

fn parse_time_with_annotations(s: String) -> Option(TimeRec) {
  use #(t, rest) <- option.then(parse_time_part(s))
  let rest = case parse_offset_part(rest) {
    Some(#(Zulu, _)) -> "###invalid###"
    Some(#(_, r)) -> r
    None -> rest
  }
  use #(_, _cal, rest2) <- option.then(parse_annotations(
    rest,
    None,
    None,
    False,
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
) -> #(TimeRec, Agent) {
  let #(f, st) = read_time_fields(st, bag)
  case f == no_time_fields {
    True ->
      rt_val.t_throw_type_error(
        st,
        "invalid property bag for Temporal.PlainTime",
      )
    False -> {
      let #(overflow, st) = validated_overflow(st, options)
      let t0 = time_fields_apply(f, midnight)
      #(terr(st, regulate_time(t0, overflow)), st)
    }
  }
}

pub fn add_time(t: TimeRec, add_ns: Int) -> #(Int, TimeRec) {
  let total = time_to_ns(t) + add_ns
  let days = floor_div(total, ns_per_day)
  let rem = total - days * ns_per_day
  #(days, ns_to_time(rem))
}
