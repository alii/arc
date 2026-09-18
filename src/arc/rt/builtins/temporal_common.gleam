import arc/bytecode/error_kind.{type JsError, JsError, RangeError}
import arc/bytecode/key.{Named}
import arc/internal/temporal_calendar
import arc/rt/builtins/helpers
import arc/rt/builtins/realm_ops
import arc/rt/builtins/temporal_iso.{
  type Duration, type IsoDate, type IsoDateSlots, type IsoTime, Duration,
  IsoDate, IsoDateSlots, IsoTime, int_sign, max_time_duration_ns, ns_per_day,
  ns_per_hour, ns_per_minute, ns_per_ms, ns_per_second, ns_per_us, pow10,
  pow2_32, round_to_float_precision, take_some_digits, zero_duration,
}
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/temporal_data.{
  type TemporalZone, TemporalDate, TemporalDateTime, TemporalDuration,
  TemporalInstant, TemporalMonthDay, TemporalTime, TemporalYearMonth,
  TemporalZonedDateTime,
}
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type ObjKind, type TemporalProtos,
  type TemporalStaticName, CompareStatic, FromStatic, JFloat, JInt, JNan,
  JNegInf, JPosInf, KHandle, KStr, KUndef, SObject, StringKey, TemporalObj,
  classify, mk_object,
}
import arc/rt/val as rt_val
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub fn require_temporal(
  st: Agent,
  this: JsVal,
  type_name: String,
  name: String,
  extract: fn(ObjKind) -> Option(a),
) -> a {
  case helpers.brand_of(st, this, extract) {
    Some(#(v, _h)) -> v
    None ->
      rt_val.t_throw_type_error(
        st,
        "Temporal."
          <> type_name
          <> ".prototype."
          <> name
          <> " called on incompatible receiver",
      )
  }
}

pub fn static_name(s: TemporalStaticName) -> String {
  case s {
    FromStatic -> "from"
    CompareStatic -> "compare"
  }
}

pub fn temporal_data_of(
  st: Agent,
  v: JsVal,
) -> Option(temporal_data.TemporalData) {
  case classify(v) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: TemporalObj(data:), ..) -> Some(data)
        _ -> None
      }
    _ -> None
  }
}

pub fn date_slot_of(
  kind: ObjKind,
) -> Option(#(IsoDate, temporal_calendar.Calendar)) {
  case kind {
    TemporalObj(data: TemporalDate(year:, month:, day:, calendar:)) ->
      Some(#(IsoDate(year:, month:, day:), calendar))
    _ -> None
  }
}

pub fn time_slot_of(kind: ObjKind) -> Option(IsoTime) {
  case kind {
    TemporalObj(data: TemporalTime(
      hour:,
      minute:,
      second:,
      millisecond:,
      microsecond:,
      nanosecond:,
    )) ->
      Some(IsoTime(hour, minute, second, millisecond, microsecond, nanosecond))
    _ -> None
  }
}

pub fn date_time_slot_of(
  kind: ObjKind,
) -> Option(#(IsoDate, IsoTime, temporal_calendar.Calendar)) {
  case kind {
    TemporalObj(data: TemporalDateTime(
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
    )) ->
      Some(#(
        IsoDate(year, month, day),
        IsoTime(hour, minute, second, millisecond, microsecond, nanosecond),
        calendar,
      ))
    _ -> None
  }
}

pub fn year_month_slot_of(kind: ObjKind) -> Option(IsoDateSlots) {
  case kind {
    TemporalObj(data: TemporalYearMonth(year:, month:, day:, calendar:)) ->
      Some(IsoDateSlots(IsoDate(year:, month:, day:), calendar))
    _ -> None
  }
}

pub fn month_day_slot_of(kind: ObjKind) -> Option(IsoDateSlots) {
  case kind {
    TemporalObj(data: TemporalMonthDay(month:, day:, ref_year:, calendar:)) ->
      Some(IsoDateSlots(IsoDate(year: ref_year, month:, day:), calendar))
    _ -> None
  }
}

pub fn duration_slot_of(kind: ObjKind) -> Option(Duration) {
  case kind {
    TemporalObj(data: TemporalDuration(
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
    )) ->
      Some(Duration(
        years,
        months,
        weeks,
        days,
        hours,
        minutes,
        seconds,
        milliseconds,
        microseconds,
        nanoseconds,
      ))
    _ -> None
  }
}

pub fn instant_slot_of(kind: ObjKind) -> Option(Int) {
  case kind {
    TemporalObj(data: TemporalInstant(epoch_ns:)) -> Some(epoch_ns)
    _ -> None
  }
}

pub fn zoned_slot_of(
  kind: ObjKind,
) -> Option(#(Int, TemporalZone, temporal_calendar.Calendar)) {
  case kind {
    TemporalObj(data: TemporalZonedDateTime(epoch_ns:, time_zone:, calendar:)) ->
      Some(#(epoch_ns, time_zone, calendar))
    _ -> None
  }
}

fn alloc_value(
  st: Agent,
  data: temporal_data.TemporalData,
  proto: Handle,
) -> #(JsVal, Agent) {
  let #(h, st) = realm_ops.alloc_object(st, TemporalObj(data), proto)
  #(mk_object(h), st)
}

pub fn make_date(
  st: Agent,
  protos: TemporalProtos,
  d: IsoDate,
) -> #(JsVal, Agent) {
  make_date_cal(st, protos, d, temporal_calendar.Iso8601)
}

pub fn make_date_cal(
  st: Agent,
  protos: TemporalProtos,
  d: IsoDate,
  cal: temporal_calendar.Calendar,
) -> #(JsVal, Agent) {
  alloc_value(
    st,
    TemporalDate(year: d.year, month: d.month, day: d.day, calendar: cal),
    protos.plain_date,
  )
}

pub fn make_time(
  st: Agent,
  protos: TemporalProtos,
  t: IsoTime,
) -> #(JsVal, Agent) {
  alloc_value(
    st,
    TemporalTime(
      hour: t.hour,
      minute: t.minute,
      second: t.second,
      millisecond: t.millisecond,
      microsecond: t.microsecond,
      nanosecond: t.nanosecond,
    ),
    protos.plain_time,
  )
}

pub fn make_date_time(
  st: Agent,
  protos: TemporalProtos,
  d: IsoDate,
  t: IsoTime,
) -> #(JsVal, Agent) {
  make_date_time_cal(st, protos, d, t, temporal_calendar.Iso8601)
}

pub fn make_date_time_cal(
  st: Agent,
  protos: TemporalProtos,
  d: IsoDate,
  t: IsoTime,
  cal: temporal_calendar.Calendar,
) -> #(JsVal, Agent) {
  alloc_value(
    st,
    TemporalDateTime(
      year: d.year,
      month: d.month,
      day: d.day,
      hour: t.hour,
      minute: t.minute,
      second: t.second,
      millisecond: t.millisecond,
      microsecond: t.microsecond,
      nanosecond: t.nanosecond,
      calendar: cal,
    ),
    protos.plain_date_time,
  )
}

pub fn make_year_month(
  st: Agent,
  protos: TemporalProtos,
  y: Int,
  m: Int,
  ref_day: Int,
) -> #(JsVal, Agent) {
  make_year_month_cal(st, protos, y, m, ref_day, temporal_calendar.Iso8601)
}

pub fn make_year_month_cal(
  st: Agent,
  protos: TemporalProtos,
  y: Int,
  m: Int,
  ref_day: Int,
  cal: temporal_calendar.Calendar,
) -> #(JsVal, Agent) {
  alloc_value(
    st,
    TemporalYearMonth(year: y, month: m, day: ref_day, calendar: cal),
    protos.plain_year_month,
  )
}

pub fn make_month_day_cal(
  st: Agent,
  protos: TemporalProtos,
  m: Int,
  d: Int,
  ref_year: Int,
  cal: temporal_calendar.Calendar,
) -> #(JsVal, Agent) {
  alloc_value(
    st,
    TemporalMonthDay(month: m, day: d, ref_year:, calendar: cal),
    protos.plain_month_day,
  )
}

pub fn make_duration(
  st: Agent,
  protos: TemporalProtos,
  dur: Duration,
) -> #(JsVal, Agent) {
  let d = map_duration_fields(dur, round_to_float_precision)
  alloc_value(
    st,
    TemporalDuration(
      years: d.years,
      months: d.months,
      weeks: d.weeks,
      days: d.days,
      hours: d.hours,
      minutes: d.minutes,
      seconds: d.seconds,
      milliseconds: d.milliseconds,
      microseconds: d.microseconds,
      nanoseconds: d.nanoseconds,
    ),
    protos.duration,
  )
}

pub fn finish_duration(
  st: Agent,
  protos: TemporalProtos,
  dur: Duration,
) -> #(JsVal, Agent) {
  case is_valid_duration(dur) {
    False -> rt_val.t_throw_range_error(st, "invalid duration")
    True -> make_duration(st, protos, dur)
  }
}

pub fn make_instant(
  st: Agent,
  protos: TemporalProtos,
  ns: Int,
) -> #(JsVal, Agent) {
  alloc_value(st, TemporalInstant(epoch_ns: ns), protos.instant)
}

pub fn make_zoned(
  st: Agent,
  protos: TemporalProtos,
  ns: Int,
  tz: TemporalZone,
) -> #(JsVal, Agent) {
  make_zoned_cal(st, protos, ns, tz, temporal_calendar.Iso8601)
}

pub fn make_zoned_cal(
  st: Agent,
  protos: TemporalProtos,
  ns: Int,
  tz: TemporalZone,
  cal: temporal_calendar.Calendar,
) -> #(JsVal, Agent) {
  alloc_value(
    st,
    TemporalZonedDateTime(epoch_ns: ns, time_zone: tz, calendar: cal),
    protos.zoned_date_time,
  )
}

pub fn apply_new_target_proto(
  st: Agent,
  new_target: JsVal,
  v: JsVal,
) -> #(Handle, Agent) {
  let assert KHandle(obj) = classify(v)
    as "Temporal constructor produced no object"
  let assert SObject(proto: Some(intrinsic), ..) = rt_store.t_cell_get(st, obj)
    as "Temporal constructor produced an object with no prototype"
  let #(proto, st) =
    rt_call.get_prototype_from_constructor(st, new_target, fn(_realm) {
      intrinsic
    })
  let st =
    rt_store.t_cell_update(st, obj, fn(cell) {
      case cell {
        SObject(..) -> SObject(..cell, proto: Some(proto))
        other -> other
      }
    })
  #(obj, st)
}

pub fn to_integer_with_truncation(st: Agent, v: JsVal) -> #(Int, Agent) {
  let #(n, st) = rt_val.t_to_number(st, v)
  case n {
    JInt(i) -> #(i, st)
    JFloat(f) -> #(rt_val.float_to_int(f), st)
    JNan | JPosInf | JNegInf ->
      rt_val.t_throw_range_error(st, "not a finite number")
  }
}

pub fn to_positive_integer_with_truncation(
  st: Agent,
  v: JsVal,
) -> #(Int, Agent) {
  let #(n, st) = to_integer_with_truncation(st, v)
  case n > 0 {
    True -> #(n, st)
    False -> rt_val.t_throw_range_error(st, "expected a positive integer")
  }
}

pub fn to_integer_if_integral(st: Agent, v: JsVal) -> #(Int, Agent) {
  let #(n, st) = rt_val.t_to_number(st, v)
  case n {
    JInt(i) -> #(i, st)
    JFloat(f) -> {
      let i = rt_val.float_to_int(f)
      let fi = int.to_float(i)
      // arithmetic compare: term == treats -0.0 != 0.0
      case f >=. fi && f <=. fi {
        True -> #(i, st)
        False -> rt_val.t_throw_range_error(st, "expected an integral number")
      }
    }
    JNan | JPosInf | JNegInf ->
      rt_val.t_throw_range_error(st, "expected an integral number")
  }
}

pub fn integral_int_arg_or(
  st: Agent,
  args: List(JsVal),
  idx: Int,
  default: Int,
) -> #(Int, Agent) {
  let v = helpers.arg_at(args, idx)
  case classify(v) {
    KUndef -> #(default, st)
    _ -> to_integer_if_integral(st, v)
  }
}

pub fn truncated_int_arg(
  st: Agent,
  args: List(JsVal),
  idx: Int,
) -> #(Int, Agent) {
  to_integer_with_truncation(st, helpers.arg_at(args, idx))
}

pub fn truncated_int_arg_or(
  st: Agent,
  args: List(JsVal),
  idx: Int,
  default: Int,
) -> #(Int, Agent) {
  let v = helpers.arg_at(args, idx)
  case classify(v) {
    KUndef -> #(default, st)
    _ -> to_integer_with_truncation(st, v)
  }
}

fn duration_field_list(d: Duration) -> List(Int) {
  [
    d.years,
    d.months,
    d.weeks,
    d.days,
    d.hours,
    d.minutes,
    d.seconds,
    d.milliseconds,
    d.microseconds,
    d.nanoseconds,
  ]
}

fn map_duration_fields(d: Duration, f: fn(Int) -> Int) -> Duration {
  Duration(
    years: f(d.years),
    months: f(d.months),
    weeks: f(d.weeks),
    days: f(d.days),
    hours: f(d.hours),
    minutes: f(d.minutes),
    seconds: f(d.seconds),
    milliseconds: f(d.milliseconds),
    microseconds: f(d.microseconds),
    nanoseconds: f(d.nanoseconds),
  )
}

pub fn has_calendar_units(d: Duration) -> Bool {
  d.years != 0 || d.months != 0 || d.weeks != 0
}

pub fn has_date_units(d: Duration) -> Bool {
  has_calendar_units(d) || d.days != 0
}

pub fn date_part(d: Duration) -> Duration {
  Duration(
    ..zero_duration,
    years: d.years,
    months: d.months,
    weeks: d.weeks,
    days: d.days,
  )
}

pub fn duration_sign(d: Duration) -> Int {
  list.fold(duration_field_list(d), 0, fn(acc, f) {
    case acc != 0 {
      True -> acc
      False -> int_sign(f)
    }
  })
}

pub fn is_valid_duration(d: Duration) -> Bool {
  let sign = duration_sign(d)
  // validity is checked on float-rounded components per spec
  let d = map_duration_fields(d, round_to_float_precision)
  let consistent =
    list.all(duration_field_list(d), fn(f) {
      { f >= 0 || sign <= 0 } && { f <= 0 || sign >= 0 }
    })
  let cal_ok =
    int.absolute_value(d.years) < pow2_32
    && int.absolute_value(d.months) < pow2_32
    && int.absolute_value(d.weeks) < pow2_32
  let total = days_and_time_ns(d)
  consistent && cal_ok && int.absolute_value(total) <= max_time_duration_ns
}

pub fn days_and_time_ns(d: Duration) -> Int {
  d.days * ns_per_day + time_part_ns(d)
}

pub fn time_part_ns(d: Duration) -> Int {
  d.hours
  * ns_per_hour
  + d.minutes
  * ns_per_minute
  + d.seconds
  * ns_per_second
  + d.milliseconds
  * ns_per_ms
  + d.microseconds
  * ns_per_us
  + d.nanoseconds
}

pub fn check_time_duration_range(ns: Int) -> Result(Nil, JsError) {
  case int.absolute_value(ns) > max_time_duration_ns {
    True -> Error(JsError(RangeError, "duration time units out of range"))
    False -> Ok(Nil)
  }
}

pub fn apply_duration_sign(d: Duration, sign: Int) -> Duration {
  case sign < 0 {
    False -> d
    True -> map_duration_fields(d, int.negate)
  }
}

pub fn negate_duration(d: Duration) -> Duration {
  apply_duration_sign(d, -1)
}

pub fn read_bag_int_field(
  st: Agent,
  bag: Handle,
  key: String,
  conv: fn(Agent, JsVal) -> #(Int, Agent),
) -> #(Option(Int), Agent) {
  let #(v, st) = rt_obj.t_get_prop(st, mk_object(bag), StringKey(Named(key)))
  case classify(v) {
    KUndef -> #(None, st)
    _ -> {
      let #(n, st) = conv(st, v)
      #(Some(n), st)
    }
  }
}

pub fn read_int_field(
  st: Agent,
  bag: Handle,
  key: String,
) -> #(Option(Int), Agent) {
  read_bag_int_field(st, bag, key, to_integer_with_truncation)
}

pub fn read_pos_int_field(
  st: Agent,
  bag: Handle,
  key: String,
) -> #(Option(Int), Agent) {
  read_bag_int_field(st, bag, key, to_positive_integer_with_truncation)
}

pub fn read_integral_int_field(
  st: Agent,
  bag: Handle,
  key: String,
) -> #(Option(Int), Agent) {
  read_bag_int_field(st, bag, key, to_integer_if_integral)
}

pub fn to_temporal_duration(st: Agent, item: JsVal) -> #(Duration, Agent) {
  case classify(item) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind:, ..) ->
          case duration_slot_of(kind) {
            Some(d) -> #(d, st)
            None -> duration_from_bag(st, h)
          }
        _ -> duration_from_bag(st, h)
      }
    KStr(s) ->
      case parse_duration_string(s) {
        Some(d) ->
          case is_valid_duration(d) {
            True -> #(d, st)
            False -> rt_val.t_throw_range_error(st, "invalid duration")
          }
        None -> rt_val.t_throw_range_error(st, "invalid duration string: " <> s)
      }
    _ -> rt_val.t_throw_type_error(st, "cannot convert to a Temporal.Duration")
  }
}

pub fn read_duration_fields(
  st: Agent,
  bag: Handle,
) -> #(List(Option(Int)), Agent) {
  let #(days, st) = read_integral_int_field(st, bag, "days")
  let #(hours, st) = read_integral_int_field(st, bag, "hours")
  let #(microseconds, st) = read_integral_int_field(st, bag, "microseconds")
  let #(milliseconds, st) = read_integral_int_field(st, bag, "milliseconds")
  let #(minutes, st) = read_integral_int_field(st, bag, "minutes")
  let #(months, st) = read_integral_int_field(st, bag, "months")
  let #(nanoseconds, st) = read_integral_int_field(st, bag, "nanoseconds")
  let #(seconds, st) = read_integral_int_field(st, bag, "seconds")
  let #(weeks, st) = read_integral_int_field(st, bag, "weeks")
  let #(years, st) = read_integral_int_field(st, bag, "years")
  #(
    [
      years,
      months,
      weeks,
      days,
      hours,
      minutes,
      seconds,
      milliseconds,
      microseconds,
      nanoseconds,
    ],
    st,
  )
}

pub fn apply_duration_fields(
  base: Duration,
  fields: List(Option(Int)),
) -> Duration {
  case fields {
    [
      years,
      months,
      weeks,
      days,
      hours,
      minutes,
      seconds,
      milliseconds,
      microseconds,
      nanoseconds,
    ] ->
      Duration(
        years: option.unwrap(years, base.years),
        months: option.unwrap(months, base.months),
        weeks: option.unwrap(weeks, base.weeks),
        days: option.unwrap(days, base.days),
        hours: option.unwrap(hours, base.hours),
        minutes: option.unwrap(minutes, base.minutes),
        seconds: option.unwrap(seconds, base.seconds),
        milliseconds: option.unwrap(milliseconds, base.milliseconds),
        microseconds: option.unwrap(microseconds, base.microseconds),
        nanoseconds: option.unwrap(nanoseconds, base.nanoseconds),
      )
    _ -> base
  }
}

pub fn duration_from_bag(st: Agent, bag: Handle) -> #(Duration, Agent) {
  let #(fields, st) = read_duration_fields(st, bag)
  case list.all(fields, option.is_none) {
    True ->
      rt_val.t_throw_type_error(
        st,
        "invalid property bag for Temporal.Duration",
      )
    False -> {
      let d = apply_duration_fields(zero_duration, fields)
      case is_valid_duration(d) {
        True -> #(d, st)
        False -> rt_val.t_throw_range_error(st, "invalid duration")
      }
    }
  }
}

pub fn parse_duration_string(s: String) -> Option(Duration) {
  let #(sign, rest) = case s {
    "+" <> r -> #(1, r)
    "-" <> r -> #(-1, r)
    _ -> #(1, s)
  }
  case rest {
    "P" <> r | "p" <> r -> parse_duration_date_units(r, sign)
    _ -> None
  }
}

fn parse_duration_date_units(s: String, sign: Int) -> Option(Duration) {
  let #(years, s) = parse_duration_unit(s, ["Y", "y"])
  let #(months, s) = parse_duration_unit(s, ["M", "m"])
  let #(weeks, s) = parse_duration_unit(s, ["W", "w"])
  let #(days, s) = parse_duration_unit(s, ["D", "d"])
  case s {
    "" ->
      case years == None && months == None && weeks == None && days == None {
        True -> None
        False ->
          Some(apply_duration_sign(
            Duration(
              years: option.unwrap(years, 0),
              months: option.unwrap(months, 0),
              weeks: option.unwrap(weeks, 0),
              days: option.unwrap(days, 0),
              hours: 0,
              minutes: 0,
              seconds: 0,
              milliseconds: 0,
              microseconds: 0,
              nanoseconds: 0,
            ),
            sign,
          ))
      }
    "T" <> r | "t" <> r -> {
      use #(h, mi, sec, sub_ns) <- option.then(parse_duration_time_units(r))
      Some(apply_duration_sign(
        Duration(
          years: option.unwrap(years, 0),
          months: option.unwrap(months, 0),
          weeks: option.unwrap(weeks, 0),
          days: option.unwrap(days, 0),
          hours: h,
          minutes: mi,
          seconds: sec,
          milliseconds: sub_ns / ns_per_ms,
          microseconds: { sub_ns % ns_per_ms } / ns_per_us,
          nanoseconds: sub_ns % ns_per_us,
        ),
        sign,
      ))
    }
    _ -> None
  }
}

fn parse_duration_unit(
  s: String,
  designators: List(String),
) -> #(Option(Int), String) {
  case take_some_digits(s, 16) {
    Some(#(v, _, rest)) ->
      case list.find(designators, fn(d) { string.starts_with(rest, d) }) {
        Ok(d) -> #(Some(v), string.drop_start(rest, string.length(d)))
        Error(Nil) -> #(None, s)
      }
    None -> #(None, s)
  }
}

fn parse_duration_time_units(s: String) -> Option(#(Int, Int, Int, Int)) {
  use #(h, h_frac, s1) <- option.then(parse_duration_time_unit(s, ["H", "h"]))
  case h_frac {
    Some(f) ->
      case s1 {
        "" -> {
          let total_ns = f * 3600
          let mi = total_ns / ns_per_minute
          let rem = total_ns - mi * ns_per_minute
          let sec = rem / ns_per_second
          Some(#(option.unwrap(h, 0), mi, sec, rem - sec * ns_per_second))
        }
        _ -> None
      }
    None -> {
      use #(mi, mi_frac, s2) <- option.then(
        parse_duration_time_unit(s1, ["M", "m"]),
      )
      case mi_frac {
        Some(f) ->
          case s2 {
            "" -> {
              let total_ns = f * 60
              let sec = total_ns / ns_per_second
              Some(#(
                option.unwrap(h, 0),
                option.unwrap(mi, 0),
                sec,
                total_ns - sec * ns_per_second,
              ))
            }
            _ -> None
          }
        None -> {
          use #(sec, s_frac, s3) <- option.then(
            parse_duration_time_unit(s2, ["S", "s"]),
          )
          case s3 {
            "" ->
              case h == None && mi == None && sec == None {
                True -> None
                False ->
                  Some(#(
                    option.unwrap(h, 0),
                    option.unwrap(mi, 0),
                    option.unwrap(sec, 0),
                    option.unwrap(s_frac, 0),
                  ))
              }
            _ -> None
          }
        }
      }
    }
  }
}

fn parse_duration_time_unit(
  s: String,
  designators: List(String),
) -> Option(#(Option(Int), Option(Int), String)) {
  case take_some_digits(s, 16) {
    None -> Some(#(None, None, s))
    Some(#(v, _, rest)) -> {
      let #(frac_ns, rest2, had_frac) = case rest {
        "." <> r | "," <> r ->
          case take_some_digits(r, 9) {
            Some(#(f, count, rr)) -> #(f * pow10(9 - count), rr, True)
            None -> #(0, rest, False)
          }
        _ -> #(0, rest, False)
      }
      case list.find(designators, fn(d) { string.starts_with(rest2, d) }) {
        Ok(d) ->
          case had_frac {
            True ->
              Some(#(
                Some(v),
                Some(frac_ns),
                string.drop_start(rest2, string.length(d)),
              ))
            False ->
              Some(#(Some(v), None, string.drop_start(rest2, string.length(d))))
          }
        Error(Nil) ->
          // designator mismatch: backtrack so caller tries next unit
          Some(#(None, None, s))
      }
    }
  }
}
