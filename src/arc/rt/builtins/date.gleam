import arc/internal/digits.{take_digits}
import arc/internal/gregorian.{civil_from_days, days_from_year}
import arc/internal/host_time.{
  type TimeZone, zone_offset_at_local_ms, zone_offset_at_utc_ms,
}
import arc/internal/int_math.{floor_div, floor_mod as math_mod}
import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/builtins/realm_ops
import arc/rt/call as rt_call
import arc/rt/name_keys as nk
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type DateNative, type Handle, type JsNum,
  type JsVal, DateConstructor, DateN, DateNow, DateObj, DateParse,
  DatePrototypeGetDate, DatePrototypeGetDay, DatePrototypeGetFullYear,
  DatePrototypeGetHours, DatePrototypeGetMilliseconds, DatePrototypeGetMinutes,
  DatePrototypeGetMonth, DatePrototypeGetSeconds, DatePrototypeGetTime,
  DatePrototypeGetTimezoneOffset, DatePrototypeGetUTCDate,
  DatePrototypeGetUTCDay, DatePrototypeGetUTCFullYear, DatePrototypeGetUTCHours,
  DatePrototypeGetUTCMilliseconds, DatePrototypeGetUTCMinutes,
  DatePrototypeGetUTCMonth, DatePrototypeGetUTCSeconds, DatePrototypeGetYear,
  DatePrototypeSetDate, DatePrototypeSetFullYear, DatePrototypeSetHours,
  DatePrototypeSetMilliseconds, DatePrototypeSetMinutes, DatePrototypeSetMonth,
  DatePrototypeSetSeconds, DatePrototypeSetTime, DatePrototypeSetUTCDate,
  DatePrototypeSetUTCFullYear, DatePrototypeSetUTCHours,
  DatePrototypeSetUTCMilliseconds, DatePrototypeSetUTCMinutes,
  DatePrototypeSetUTCMonth, DatePrototypeSetUTCSeconds, DatePrototypeSetYear,
  DatePrototypeSymbolToPrimitive, DatePrototypeToDateString,
  DatePrototypeToISOString, DatePrototypeToJSON, DatePrototypeToLocaleDateString,
  DatePrototypeToLocaleString, DatePrototypeToLocaleTimeString,
  DatePrototypeToString, DatePrototypeToTimeString, DatePrototypeToUTCString,
  DatePrototypeValueOf, DateUTC, HintDefault, HintNumber, HintString, JFloat,
  JInt, JNan, JNegInf, JPosInf, KHandle, KNum, KStr, StringKey, classify,
  mk_null, mk_number, mk_object, mk_string,
} as rt_types
import arc/rt/val as rt_val
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// the one sign flip: js wants utc minus local
fn js_get_timezone_offset_minutes(zone: TimeZone, epoch_ms: Int) -> Int {
  0 - zone_offset_at_utc_ms(zone, epoch_ms)
}

pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
) -> #(BuiltinPair, Agent) {
  let #(statics, st) =
    common.alloc_methods(st, fn_proto, [
      #("now", DateN(DateNow), 0),
      #("parse", DateN(DateParse), 1),
      #("UTC", DateN(DateUTC), 7),
    ])

  let #(proto_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("valueOf", DateN(DatePrototypeValueOf), 0),
      #("getTime", DateN(DatePrototypeGetTime), 0),
      #("getTimezoneOffset", DateN(DatePrototypeGetTimezoneOffset), 0),
      #("getFullYear", DateN(DatePrototypeGetFullYear), 0),
      #("getUTCFullYear", DateN(DatePrototypeGetUTCFullYear), 0),
      #("getMonth", DateN(DatePrototypeGetMonth), 0),
      #("getUTCMonth", DateN(DatePrototypeGetUTCMonth), 0),
      #("getDate", DateN(DatePrototypeGetDate), 0),
      #("getUTCDate", DateN(DatePrototypeGetUTCDate), 0),
      #("getDay", DateN(DatePrototypeGetDay), 0),
      #("getUTCDay", DateN(DatePrototypeGetUTCDay), 0),
      #("getHours", DateN(DatePrototypeGetHours), 0),
      #("getUTCHours", DateN(DatePrototypeGetUTCHours), 0),
      #("getMinutes", DateN(DatePrototypeGetMinutes), 0),
      #("getUTCMinutes", DateN(DatePrototypeGetUTCMinutes), 0),
      #("getSeconds", DateN(DatePrototypeGetSeconds), 0),
      #("getUTCSeconds", DateN(DatePrototypeGetUTCSeconds), 0),
      #("getMilliseconds", DateN(DatePrototypeGetMilliseconds), 0),
      #("getUTCMilliseconds", DateN(DatePrototypeGetUTCMilliseconds), 0),
      #("setTime", DateN(DatePrototypeSetTime), 1),
      #("setMilliseconds", DateN(DatePrototypeSetMilliseconds), 1),
      #("setUTCMilliseconds", DateN(DatePrototypeSetUTCMilliseconds), 1),
      #("setSeconds", DateN(DatePrototypeSetSeconds), 2),
      #("setUTCSeconds", DateN(DatePrototypeSetUTCSeconds), 2),
      #("setMinutes", DateN(DatePrototypeSetMinutes), 3),
      #("setUTCMinutes", DateN(DatePrototypeSetUTCMinutes), 3),
      #("setHours", DateN(DatePrototypeSetHours), 4),
      #("setUTCHours", DateN(DatePrototypeSetUTCHours), 4),
      #("setDate", DateN(DatePrototypeSetDate), 1),
      #("setUTCDate", DateN(DatePrototypeSetUTCDate), 1),
      #("setMonth", DateN(DatePrototypeSetMonth), 2),
      #("setUTCMonth", DateN(DatePrototypeSetUTCMonth), 2),
      #("setFullYear", DateN(DatePrototypeSetFullYear), 3),
      #("setUTCFullYear", DateN(DatePrototypeSetUTCFullYear), 3),
      #("getYear", DateN(DatePrototypeGetYear), 0),
      #("setYear", DateN(DatePrototypeSetYear), 1),
      #("toString", DateN(DatePrototypeToString), 0),
      #("toDateString", DateN(DatePrototypeToDateString), 0),
      #("toTimeString", DateN(DatePrototypeToTimeString), 0),
      #("toISOString", DateN(DatePrototypeToISOString), 0),
      #("toUTCString", DateN(DatePrototypeToUTCString), 0),
      #("toGMTString", DateN(DatePrototypeToUTCString), 0),
      #("toLocaleString", DateN(DatePrototypeToLocaleString), 0),
      #("toLocaleDateString", DateN(DatePrototypeToLocaleDateString), 0),
      #("toLocaleTimeString", DateN(DatePrototypeToLocaleTimeString), 0),
      #("toJSON", DateN(DatePrototypeToJSON), 1),
    ])

  let #(bt, st) =
    common.init_type(
      st,
      object_proto,
      fn_proto,
      proto_methods,
      fn(proto) { DateN(DateConstructor(proto:)) },
      "Date",
      7,
      statics,
    )

  let #(to_prim_h, st) =
    common.alloc_rooted_native_fn(
      st,
      fn_proto,
      DateN(DatePrototypeSymbolToPrimitive),
      "[Symbol.toPrimitive]",
      1,
    )
  let #(prop, st) = common.data_prop(st, mk_object(to_prim_h))
  let st =
    common.add_symbol_property(
      st,
      bt.prototype,
      rt_types.symbol_to_primitive,
      common.configurable(prop),
    )

  #(bt, st)
}

pub fn dispatch(
  st: Agent,
  native: DateNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let name = method_name(native)
  let local = LocalTime(st.hooks.time_zone)
  case native {
    DateConstructor(..) -> {
      let fields = get_date_fields(now_ms(st), local)
      #(mk_string(format_date(FmtLocal(DateAndTime), fields)), st)
    }
    DateNow -> #(mk_number(JInt(now_ms(st))), st)
    DateParse -> date_parse(st, args, local)
    DateUTC -> date_utc(st, args)
    DatePrototypeValueOf | DatePrototypeGetTime -> date_get_time(st, this, name)
    DatePrototypeGetTimezoneOffset -> date_get_tz_offset(st, this, name, local)
    DatePrototypeGetFullYear -> date_get_field(st, this, name, FieldYear, local)
    DatePrototypeGetUTCFullYear ->
      date_get_field(st, this, name, FieldYear, UtcTime)
    DatePrototypeGetMonth -> date_get_field(st, this, name, FieldMonth, local)
    DatePrototypeGetUTCMonth ->
      date_get_field(st, this, name, FieldMonth, UtcTime)
    DatePrototypeGetDate -> date_get_field(st, this, name, FieldDate, local)
    DatePrototypeGetUTCDate ->
      date_get_field(st, this, name, FieldDate, UtcTime)
    DatePrototypeGetDay -> date_get_field(st, this, name, FieldWeekday, local)
    DatePrototypeGetUTCDay ->
      date_get_field(st, this, name, FieldWeekday, UtcTime)
    DatePrototypeGetHours -> date_get_field(st, this, name, FieldHours, local)
    DatePrototypeGetUTCHours ->
      date_get_field(st, this, name, FieldHours, UtcTime)
    DatePrototypeGetMinutes ->
      date_get_field(st, this, name, FieldMinutes, local)
    DatePrototypeGetUTCMinutes ->
      date_get_field(st, this, name, FieldMinutes, UtcTime)
    DatePrototypeGetSeconds ->
      date_get_field(st, this, name, FieldSeconds, local)
    DatePrototypeGetUTCSeconds ->
      date_get_field(st, this, name, FieldSeconds, UtcTime)
    DatePrototypeGetMilliseconds ->
      date_get_field(st, this, name, FieldMs, local)
    DatePrototypeGetUTCMilliseconds ->
      date_get_field(st, this, name, FieldMs, UtcTime)
    DatePrototypeSetTime -> date_set_time(st, this, args, name)
    DatePrototypeSetMilliseconds ->
      date_set_field(st, this, args, name, SetMs, local)
    DatePrototypeSetUTCMilliseconds ->
      date_set_field(st, this, args, name, SetMs, UtcTime)
    DatePrototypeSetSeconds ->
      date_set_field(st, this, args, name, SetSeconds, local)
    DatePrototypeSetUTCSeconds ->
      date_set_field(st, this, args, name, SetSeconds, UtcTime)
    DatePrototypeSetMinutes ->
      date_set_field(st, this, args, name, SetMinutes, local)
    DatePrototypeSetUTCMinutes ->
      date_set_field(st, this, args, name, SetMinutes, UtcTime)
    DatePrototypeSetHours ->
      date_set_field(st, this, args, name, SetHours, local)
    DatePrototypeSetUTCHours ->
      date_set_field(st, this, args, name, SetHours, UtcTime)
    DatePrototypeSetDate -> date_set_field(st, this, args, name, SetDate, local)
    DatePrototypeSetUTCDate ->
      date_set_field(st, this, args, name, SetDate, UtcTime)
    DatePrototypeSetMonth ->
      date_set_field(st, this, args, name, SetMonth, local)
    DatePrototypeSetUTCMonth ->
      date_set_field(st, this, args, name, SetMonth, UtcTime)
    DatePrototypeSetFullYear ->
      date_set_field(st, this, args, name, SetYear, local)
    DatePrototypeSetUTCFullYear ->
      date_set_field(st, this, args, name, SetYear, UtcTime)
    DatePrototypeGetYear -> date_get_year(st, this, name, local)
    DatePrototypeSetYear -> date_set_year(st, this, args, name, local)
    DatePrototypeToString ->
      date_to_string(st, this, name, FmtLocal(DateAndTime), local)
    DatePrototypeToDateString ->
      date_to_string(st, this, name, FmtLocal(DateOnly), local)
    DatePrototypeToTimeString ->
      date_to_string(st, this, name, FmtLocal(TimeOnly), local)
    DatePrototypeToISOString -> date_to_string(st, this, name, FmtIso, UtcTime)
    DatePrototypeToUTCString -> date_to_string(st, this, name, FmtUtc, UtcTime)
    DatePrototypeToLocaleString ->
      date_to_string(st, this, name, FmtLocale(DateAndTime), local)
    DatePrototypeToLocaleDateString ->
      date_to_string(st, this, name, FmtLocale(DateOnly), local)
    DatePrototypeToLocaleTimeString ->
      date_to_string(st, this, name, FmtLocale(TimeOnly), local)
    DatePrototypeToJSON -> date_to_json(st, this)
    DatePrototypeSymbolToPrimitive -> date_to_primitive(st, this, args)
  }
}

pub fn dispatch_construct(
  st: Agent,
  native: DateNative,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case native {
    DateConstructor(..) -> date_constructor(st, args, new_target)
    _ -> rt_val.t_throw_type_error(st, "not a constructor")
  }
}

fn method_name(native: DateNative) -> String {
  case native {
    DateConstructor(..) -> "constructor"
    DateNow -> "now"
    DateParse -> "parse"
    DateUTC -> "UTC"
    DatePrototypeValueOf -> "valueOf"
    DatePrototypeGetTime -> "getTime"
    DatePrototypeGetTimezoneOffset -> "getTimezoneOffset"
    DatePrototypeGetFullYear -> "getFullYear"
    DatePrototypeGetUTCFullYear -> "getUTCFullYear"
    DatePrototypeGetMonth -> "getMonth"
    DatePrototypeGetUTCMonth -> "getUTCMonth"
    DatePrototypeGetDate -> "getDate"
    DatePrototypeGetUTCDate -> "getUTCDate"
    DatePrototypeGetDay -> "getDay"
    DatePrototypeGetUTCDay -> "getUTCDay"
    DatePrototypeGetHours -> "getHours"
    DatePrototypeGetUTCHours -> "getUTCHours"
    DatePrototypeGetMinutes -> "getMinutes"
    DatePrototypeGetUTCMinutes -> "getUTCMinutes"
    DatePrototypeGetSeconds -> "getSeconds"
    DatePrototypeGetUTCSeconds -> "getUTCSeconds"
    DatePrototypeGetMilliseconds -> "getMilliseconds"
    DatePrototypeGetUTCMilliseconds -> "getUTCMilliseconds"
    DatePrototypeSetTime -> "setTime"
    DatePrototypeSetMilliseconds -> "setMilliseconds"
    DatePrototypeSetUTCMilliseconds -> "setUTCMilliseconds"
    DatePrototypeSetSeconds -> "setSeconds"
    DatePrototypeSetUTCSeconds -> "setUTCSeconds"
    DatePrototypeSetMinutes -> "setMinutes"
    DatePrototypeSetUTCMinutes -> "setUTCMinutes"
    DatePrototypeSetHours -> "setHours"
    DatePrototypeSetUTCHours -> "setUTCHours"
    DatePrototypeSetDate -> "setDate"
    DatePrototypeSetUTCDate -> "setUTCDate"
    DatePrototypeSetMonth -> "setMonth"
    DatePrototypeSetUTCMonth -> "setUTCMonth"
    DatePrototypeSetFullYear -> "setFullYear"
    DatePrototypeSetUTCFullYear -> "setUTCFullYear"
    DatePrototypeGetYear -> "getYear"
    DatePrototypeSetYear -> "setYear"
    DatePrototypeToString -> "toString"
    DatePrototypeToDateString -> "toDateString"
    DatePrototypeToTimeString -> "toTimeString"
    DatePrototypeToISOString -> "toISOString"
    DatePrototypeToUTCString -> "toUTCString"
    DatePrototypeToLocaleString -> "toLocaleString"
    DatePrototypeToLocaleDateString -> "toLocaleDateString"
    DatePrototypeToLocaleTimeString -> "toLocaleTimeString"
    DatePrototypeToJSON -> "toJSON"
    DatePrototypeSymbolToPrimitive -> "[Symbol.toPrimitive]"
  }
}

const ms_per_day = 86_400_000

const max_time_value = 8_640_000_000_000_000

fn days_in_month(y: Int, m: Int) -> Int {
  gregorian.days_in_month(y, m + 1)
}

// §21.4.1.31 timeclip
fn time_clip(t: JsNum) -> JsNum {
  case t {
    JInt(i) ->
      case i >= -max_time_value && i <= max_time_value {
        True -> JInt(i)
        False -> JNan
      }
    JFloat(f) ->
      case f >=. -8.64e15 && f <=. 8.64e15 {
        True -> JInt(rt_val.float_to_int(f))
        False -> JNan
      }
    JNan | JPosInf | JNegInf -> JNan
  }
}

fn finite_ms(tv: JsNum) -> Option(Int) {
  case tv {
    JInt(i) -> Some(i)
    JFloat(f) -> Some(rt_val.float_to_int(f))
    JNan | JPosInf | JNegInf -> None
  }
}

type TimeRef {
  LocalTime(TimeZone)
  UtcTime
}

type DateFields {
  DateFields(
    year: Int,
    month: Int,
    date: Int,
    hours: Int,
    minutes: Int,
    seconds: Int,
    ms: Int,
    weekday: Int,
    tz: Int,
  )
}

type DateField {
  FieldYear
  FieldMonth
  FieldDate
  FieldHours
  FieldMinutes
  FieldSeconds
  FieldMs
  FieldWeekday
}

fn field_at(f: DateFields, field: DateField) -> Int {
  case field {
    FieldYear -> f.year
    FieldMonth -> f.month
    FieldDate -> f.date
    FieldHours -> f.hours
    FieldMinutes -> f.minutes
    FieldSeconds -> f.seconds
    FieldMs -> f.ms
    FieldWeekday -> f.weekday
  }
}

type SettableField {
  SetYear
  SetMonth
  SetDate
  SetHours
  SetMinutes
  SetSeconds
  SetMs
}

fn settable_max_args(f: SettableField) -> Int {
  case f {
    SetMs -> 1
    SetSeconds -> 2
    SetMinutes -> 3
    SetHours -> 4
    SetDate -> 1
    SetMonth -> 2
    SetYear -> 3
  }
}

fn settable_index(f: SettableField) -> Int {
  case f {
    SetYear -> 0
    SetMonth -> 1
    SetDate -> 2
    SetHours -> 3
    SetMinutes -> 4
    SetSeconds -> 5
    SetMs -> 6
  }
}

fn get_date_fields(tv: Int, time_ref: TimeRef) -> DateFields {
  let tz = case time_ref {
    LocalTime(zone) -> zone_offset_at_utc_ms(zone, tv)
    UtcTime -> 0
  }
  let d = tv + tz * 60_000
  let h = math_mod(d, ms_per_day)
  let days = { d - h } / ms_per_day
  let ms = math_mod(h, 1000)
  let h = { h - ms } / 1000
  let seconds = math_mod(h, 60)
  let h = { h - seconds } / 60
  let minutes = math_mod(h, 60)
  let hours = { h - minutes } / 60
  let weekday = gregorian.weekday_from_days(days)
  let #(year, month1, date) = civil_from_days(days)
  DateFields(
    year:,
    month: month1 - 1,
    date:,
    hours:,
    minutes:,
    seconds:,
    ms:,
    weekday:,
    tz:,
  )
}

// §21.4.1.28-30 makeday, maketime, makedate in int math
fn make_date(
  y: Int,
  mon: Int,
  date: Int,
  hours: Int,
  minutes: Int,
  seconds: Int,
  ms: Int,
  time_ref: TimeRef,
) -> JsNum {
  let ym = y + floor_div(mon, 12)
  let mn = math_mod(mon, 12)
  // years outside this can never be in range
  case ym < -285_426 || ym > 285_426 {
    True -> JNan
    False -> {
      let day = days_from_year(ym) + sum_month_days(ym, mn, 0, 0) + date - 1
      let time = hours * 3_600_000 + minutes * 60_000 + seconds * 1000 + ms
      let tv = day * ms_per_day + time
      let tv = case time_ref {
        LocalTime(zone) -> tv - zone_offset_at_local_ms(zone, tv) * 60_000
        UtcTime -> tv
      }
      time_clip(JInt(tv))
    }
  }
}

fn sum_month_days(y: Int, until: Int, i: Int, acc: Int) -> Int {
  case i >= until {
    True -> acc
    False -> sum_month_days(y, until, i + 1, acc + days_in_month(y, i))
  }
}

type DateComponents {
  DateComponents(
    year: JsNum,
    month: JsNum,
    date: JsNum,
    hours: JsNum,
    minutes: JsNum,
    seconds: JsNum,
    ms: JsNum,
  )
}

fn components_to_ints(
  c: DateComponents,
) -> Option(#(Int, Int, Int, Int, Int, Int, Int)) {
  use y <- option.then(num_to_int(c.year))
  use mon <- option.then(num_to_int(c.month))
  use dt <- option.then(num_to_int(c.date))
  use h <- option.then(num_to_int(c.hours))
  use mi <- option.then(num_to_int(c.minutes))
  use s <- option.then(num_to_int(c.seconds))
  use ms <- option.map(num_to_int(c.ms))
  #(y, mon, dt, h, mi, s, ms)
}

fn num_to_int(n: JsNum) -> Option(Int) {
  case n {
    JInt(i) -> Some(i)
    JFloat(f) -> Some(rt_val.float_to_int(f))
    JNan | JPosInf | JNegInf -> None
  }
}

fn int_num(i: Int) -> JsNum {
  JInt(i)
}

fn make_date_checked(c: DateComponents, time_ref: TimeRef) -> JsNum {
  case components_to_ints(c) {
    None -> JNan
    Some(#(y, mon, dt, h, mi, s, ms)) -> {
      let y = case y >= 0 && y <= 99 {
        True -> y + 1900
        False -> y
      }
      make_date(y, mon, dt, h, mi, s, ms, time_ref)
    }
  }
}

fn this_time_value(st: Agent, this: JsVal) -> Option(#(Handle, JsNum)) {
  case classify(this) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        rt_types.SObject(kind: DateObj(ms:), ..) -> Some(#(h, ms))
        _ -> None
      }
    _ -> None
  }
}

fn require_time_value(
  st: Agent,
  this: JsVal,
  name: String,
  k: fn(Handle, JsNum) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  case this_time_value(st, this) {
    Some(#(h, tv)) -> k(h, tv)
    None ->
      rt_val.t_throw_type_error(
        st,
        "Date.prototype." <> name <> " called on incompatible receiver",
      )
  }
}

fn set_this_time_value(st: Agent, h: Handle, tv: JsNum) -> Agent {
  rt_store.t_cell_update(st, h, fn(slot) {
    let assert rt_types.SObject(kind: DateObj(_), ..) as obj = slot
      as "date: slot is not a Date object"
    rt_types.SObject(..obj, kind: DateObj(ms: tv))
  })
}

fn date_constructor(
  st: Agent,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  let local = LocalTime(st.hooks.time_zone)
  let #(tv, st) = case args {
    [] -> #(JInt(now_ms(st)), st)
    [single] -> single_arg_time_value(st, single, local)
    many -> args_to_time_value(st, many, local)
  }
  let #(proto, st) =
    rt_call.get_prototype_from_constructor(st, new_target, fn(r) {
      r.date.prototype
    })
  realm_ops.alloc_wrapper(st, DateObj(ms: tv), proto)
}

fn single_arg_time_value(
  st: Agent,
  arg: JsVal,
  local: TimeRef,
) -> #(JsNum, Agent) {
  case this_time_value(st, arg) {
    Some(#(_, tv)) -> #(time_clip(tv), st)
    None -> {
      let #(prim, st) = rt_val.t_to_primitive(st, arg, HintDefault)
      case classify(prim) {
        KStr(s) -> #(parse_date_string(s, local), st)
        _ -> {
          let #(n, st) = rt_val.t_to_number(st, prim)
          #(time_clip(n), st)
        }
      }
    }
  }
}

fn args_to_time_value(
  st: Agent,
  args: List(JsVal),
  time_ref: TimeRef,
) -> #(JsNum, Agent) {
  let #(nums, st) = args_to_nums(st, list.take(args, 7))
  #(make_date_checked(pad_fields(nums), time_ref), st)
}

fn pad_fields(nums: List(JsNum)) -> DateComponents {
  let zero = JInt(0)
  let one = JInt(1)
  case nums {
    [] -> DateComponents(JNan, zero, one, zero, zero, zero, zero)
    [y] -> DateComponents(y, zero, one, zero, zero, zero, zero)
    [y, mon] -> DateComponents(y, mon, one, zero, zero, zero, zero)
    [y, mon, d] -> DateComponents(y, mon, d, zero, zero, zero, zero)
    [y, mon, d, h] -> DateComponents(y, mon, d, h, zero, zero, zero)
    [y, mon, d, h, mi] -> DateComponents(y, mon, d, h, mi, zero, zero)
    [y, mon, d, h, mi, s] -> DateComponents(y, mon, d, h, mi, s, zero)
    [y, mon, d, h, mi, s, ms, ..] -> DateComponents(y, mon, d, h, mi, s, ms)
  }
}

fn date_parse(st: Agent, args: List(JsVal), local: TimeRef) -> #(JsVal, Agent) {
  let arg = helpers.first_arg_or_undefined(args)
  let #(s, st) = rt_val.t_to_string(st, arg)
  #(mk_number(parse_date_string(s, local)), st)
}

fn date_utc(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  case args {
    [] -> #(mk_number(JNan), st)
    many -> {
      let #(tv, st) = args_to_time_value(st, many, UtcTime)
      #(mk_number(tv), st)
    }
  }
}

fn date_get_time(st: Agent, this: JsVal, name: String) -> #(JsVal, Agent) {
  use _, tv <- require_time_value(st, this, name)
  #(mk_number(tv), st)
}

fn date_get_tz_offset(
  st: Agent,
  this: JsVal,
  name: String,
  local: TimeRef,
) -> #(JsVal, Agent) {
  use _, tv <- require_time_value(st, this, name)
  case finite_ms(tv), local {
    Some(ms), LocalTime(zone) -> #(
      mk_number(JInt(js_get_timezone_offset_minutes(zone, ms))),
      st,
    )
    Some(_), UtcTime -> #(mk_number(JInt(0)), st)
    None, _ -> #(mk_number(JNan), st)
  }
}

fn date_get_field(
  st: Agent,
  this: JsVal,
  name: String,
  field: DateField,
  time_ref: TimeRef,
) -> #(JsVal, Agent) {
  use _, tv <- require_time_value(st, this, name)
  case finite_ms(tv) {
    Some(ms) -> {
      let fields = get_date_fields(ms, time_ref)
      #(mk_number(JInt(field_at(fields, field))), st)
    }
    None -> #(mk_number(JNan), st)
  }
}

fn date_set_time(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  name: String,
) -> #(JsVal, Agent) {
  use h, _ <- require_time_value(st, this, name)
  let arg = helpers.first_arg_or_undefined(args)
  let #(n, st) = rt_val.t_to_number(st, arg)
  let tv = time_clip(n)
  let st = set_this_time_value(st, h, tv)
  #(mk_number(tv), st)
}

fn date_set_field(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  name: String,
  first: SettableField,
  time_ref: TimeRef,
) -> #(JsVal, Agent) {
  use h, tv <- require_time_value(st, this, name)
  let supplied = list.take(args, settable_max_args(first))
  let #(new_nums, st) = args_to_nums(st, supplied)
  // none is a nan base: return nan without writing back
  case compute_set_field(tv, first, new_nums, time_ref) {
    None -> #(mk_number(JNan), st)
    Some(result) -> {
      let result = case args {
        [] -> JNan
        _ -> result
      }
      let st = set_this_time_value(st, h, result)
      #(mk_number(result), st)
    }
  }
}

fn compute_set_field(
  tv: JsNum,
  first: SettableField,
  new_nums: List(JsNum),
  time_ref: TimeRef,
) -> Option(JsNum) {
  case finite_ms(tv) {
    Some(ms) -> {
      let base = get_date_fields(ms, time_ref)
      let merged = overwrite_fields(fields_to_components(base), first, new_nums)
      Some(make_date_from_components(merged, time_ref))
    }
    None ->
      case first {
        // §21.4.4.21 step 5: nan base becomes +0
        SetYear -> {
          let zero = JInt(0)
          let epoch =
            DateComponents(
              year: JInt(1970),
              month: zero,
              date: JInt(1),
              hours: zero,
              minutes: zero,
              seconds: zero,
              ms: zero,
            )
          let merged = overwrite_fields(epoch, first, new_nums)
          Some(make_date_from_components(merged, time_ref))
        }
        _ -> None
      }
  }
}

fn fields_to_components(f: DateFields) -> DateComponents {
  DateComponents(
    year: int_num(f.year),
    month: int_num(f.month),
    date: int_num(f.date),
    hours: int_num(f.hours),
    minutes: int_num(f.minutes),
    seconds: int_num(f.seconds),
    ms: int_num(f.ms),
  )
}

fn overwrite_fields(
  base: DateComponents,
  first: SettableField,
  new_nums: List(JsNum),
) -> DateComponents {
  let lo = settable_index(first)
  DateComponents(
    year: merge_field(base.year, 0, lo, new_nums),
    month: merge_field(base.month, 1, lo, new_nums),
    date: merge_field(base.date, 2, lo, new_nums),
    hours: merge_field(base.hours, 3, lo, new_nums),
    minutes: merge_field(base.minutes, 4, lo, new_nums),
    seconds: merge_field(base.seconds, 5, lo, new_nums),
    ms: merge_field(base.ms, 6, lo, new_nums),
  )
}

fn merge_field(base: JsNum, i: Int, lo: Int, new_nums: List(JsNum)) -> JsNum {
  case i >= lo {
    True -> helpers.list_at(new_nums, i - lo) |> option.unwrap(base)
    False -> base
  }
}

fn make_date_from_components(c: DateComponents, time_ref: TimeRef) -> JsNum {
  case components_to_ints(c) {
    None -> JNan
    Some(#(y, mon, dt, h, mi, s, ms)) ->
      make_date(y, mon, dt, h, mi, s, ms, time_ref)
  }
}

type DateFmt {
  FmtLocal(DatePart)
  FmtUtc
  FmtIso
  FmtLocale(DatePart)
}

type DatePart {
  DateOnly
  TimeOnly
  DateAndTime
}

const day_names = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]

const month_names = [
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov",
  "Dec",
]

fn name_at(names: List(String), i: Int) -> String {
  helpers.list_at(names, i) |> option.unwrap("")
}

fn pad2(n: Int) -> String {
  int.to_string(int.absolute_value(n)) |> string.pad_start(2, "0")
}

fn pad3(n: Int) -> String {
  int.to_string(int.absolute_value(n)) |> string.pad_start(3, "0")
}

fn date_to_string(
  st: Agent,
  this: JsVal,
  name: String,
  fmt: DateFmt,
  time_ref: TimeRef,
) -> #(JsVal, Agent) {
  use _, tv <- require_time_value(st, this, name)
  case finite_ms(tv) {
    Some(ms) -> {
      let fields = get_date_fields(ms, time_ref)
      #(mk_string(format_date(fmt, fields)), st)
    }
    None ->
      case fmt {
        FmtIso -> rt_val.t_throw_range_error(st, "Invalid time value")
        _ -> #(mk_string("Invalid Date"), st)
      }
  }
}

fn format_date(fmt: DateFmt, f: DateFields) -> String {
  case fmt {
    FmtIso -> format_iso(f)
    FmtUtc -> format_utc(f)
    FmtLocal(part) -> format_local(part, f)
    FmtLocale(part) -> format_locale(part, f)
  }
}

fn format_iso(f: DateFields) -> String {
  let year = case f.year >= 0 && f.year <= 9999 {
    True -> string.pad_start(int.to_string(f.year), 4, "0")
    False -> {
      let sign = case f.year < 0 {
        True -> "-"
        False -> "+"
      }
      sign
      <> string.pad_start(int.to_string(int.absolute_value(f.year)), 6, "0")
    }
  }
  year
  <> "-"
  <> pad2(f.month + 1)
  <> "-"
  <> pad2(f.date)
  <> "T"
  <> pad2(f.hours)
  <> ":"
  <> pad2(f.minutes)
  <> ":"
  <> pad2(f.seconds)
  <> "."
  <> pad3(f.ms)
  <> "Z"
}

fn format_utc(f: DateFields) -> String {
  name_at(day_names, f.weekday)
  <> ", "
  <> pad2(f.date)
  <> " "
  <> name_at(month_names, f.month)
  <> " "
  <> format_year_signed(f.year)
  <> " "
  <> pad2(f.hours)
  <> ":"
  <> pad2(f.minutes)
  <> ":"
  <> pad2(f.seconds)
  <> " GMT"
}

fn format_local(part: DatePart, f: DateFields) -> String {
  let date_part =
    name_at(day_names, f.weekday)
    <> " "
    <> name_at(month_names, f.month)
    <> " "
    <> pad2(f.date)
    <> " "
    <> format_year_signed(f.year)
  let time_part =
    pad2(f.hours)
    <> ":"
    <> pad2(f.minutes)
    <> ":"
    <> pad2(f.seconds)
    <> " GMT"
    <> format_tz(f.tz)
  case part {
    DateOnly -> date_part
    TimeOnly -> time_part
    DateAndTime -> date_part <> " " <> time_part
  }
}

fn format_locale(part: DatePart, f: DateFields) -> String {
  let date_part =
    int.to_string(f.month + 1)
    <> "/"
    <> int.to_string(f.date)
    <> "/"
    <> int.to_string(f.year)
  let h12 = case f.hours % 12 {
    0 -> 12
    other -> other
  }
  let ampm = case f.hours < 12 {
    True -> "AM"
    False -> "PM"
  }
  let time_part =
    int.to_string(h12)
    <> ":"
    <> pad2(f.minutes)
    <> ":"
    <> pad2(f.seconds)
    <> " "
    <> ampm
  case part {
    DateOnly -> date_part
    TimeOnly -> time_part
    DateAndTime -> date_part <> ", " <> time_part
  }
}

fn format_year_signed(y: Int) -> String {
  case y < 0 {
    True -> "-" <> string.pad_start(int.to_string(0 - y), 4, "0")
    False -> string.pad_start(int.to_string(y), 4, "0")
  }
}

fn format_tz(tz: Int) -> String {
  let sign = case tz < 0 {
    True -> "-"
    False -> "+"
  }
  let a = int.absolute_value(tz)
  sign <> pad2(a / 60) <> pad2(a % 60)
}

fn parse_date_string(s: String, local: TimeRef) -> JsNum {
  let s = string.trim(s)
  parse_iso(s, local) |> option.unwrap(JNan)
}

type IsoTime {
  IsoTime(hours: Int, minutes: Int, seconds: Int, ms: Int)
}

fn parse_iso(s: String, local: TimeRef) -> Option(JsNum) {
  use #(year, rest) <- option.then(parse_year(s))
  let #(mon, rest) = parse_dash_int(rest, 2) |> option.unwrap(#(1, rest))
  let #(day, rest) = parse_dash_int(rest, 2) |> option.unwrap(#(1, rest))
  use #(time, rest) <- option.then(case rest {
    "T" <> t -> parse_time(t) |> option.map(fn(p) { #(Some(p.0), p.1) })
    _ -> Some(#(None, rest))
  })
  let IsoTime(h, mi, sec, ms) = option.unwrap(time, IsoTime(0, 0, 0, 0))
  use #(zone, rest) <- option.then(parse_zone(rest, option.is_some(time)))
  use Nil <- option.then(validate_iso(year, mon, day, h, mi, sec, ms))
  case rest {
    "" ->
      Some(case zone {
        LocalZone -> make_date(year, mon - 1, day, h, mi, sec, ms, local)
        FixedOffset(minutes) ->
          make_date(year, mon - 1, day, h, mi, sec, ms, UtcTime)
          |> jsnum_add_minutes(minutes)
      })
    _ -> None
  }
}

// parse rejects out-of-range parts, never rolls over
fn validate_iso(
  year: Int,
  mon: Int,
  day: Int,
  h: Int,
  mi: Int,
  sec: Int,
  ms: Int,
) -> Option(Nil) {
  let hours_ok = case h {
    24 -> mi == 0 && sec == 0 && ms == 0
    _ -> h <= 23
  }
  case
    mon >= 1
    && mon <= 12
    && day >= 1
    && day <= days_in_month(year, mon - 1)
    && hours_ok
    && mi <= 59
    && sec <= 59
  {
    True -> Some(Nil)
    False -> None
  }
}

fn parse_year(s: String) -> Option(#(Int, String)) {
  case s {
    "+" <> rest -> take_digits(rest, 6)
    // -000000 is invalid, year zero is positive
    "-" <> rest ->
      take_digits(rest, 6)
      |> option.then(fn(p) {
        case p.0 {
          0 -> None
          y -> Some(#(0 - y, p.1))
        }
      })
    _ -> take_digits(s, 4)
  }
}

fn parse_dash_int(s: String, n: Int) -> Option(#(Int, String)) {
  case s {
    "-" <> rest -> take_digits(rest, n)
    _ -> None
  }
}

fn parse_time(s: String) -> Option(#(IsoTime, String)) {
  use #(h, rest) <- option.then(take_digits(s, 2))
  use #(mi, rest) <- option.then(case rest {
    ":" <> r -> take_digits(r, 2)
    _ -> None
  })
  use #(sec, rest) <- option.then(case rest {
    ":" <> r -> take_digits(r, 2)
    _ -> Some(#(0, rest))
  })
  use #(ms, rest) <- option.then(case rest {
    "." <> r -> take_digits(r, 3)
    _ -> Some(#(0, rest))
  })
  Some(#(IsoTime(h, mi, sec, ms), rest))
}

type Zone {
  // utc minus local, added after reading fields as utc
  FixedOffset(minutes: Int)
  LocalZone
}

// no designator: date-only is utc, date-time is local
fn parse_zone(s: String, has_time: Bool) -> Option(#(Zone, String)) {
  case s {
    "Z" <> rest -> Some(#(FixedOffset(0), rest))
    "+" <> rest ->
      parse_hhmm(rest) |> option.map(fn(p) { #(FixedOffset(0 - p.0), p.1) })
    "-" <> rest ->
      parse_hhmm(rest) |> option.map(fn(p) { #(FixedOffset(p.0), p.1) })
    "" ->
      Some(#(
        case has_time {
          True -> LocalZone
          False -> FixedOffset(0)
        },
        "",
      ))
    _ -> None
  }
}

fn parse_hhmm(s: String) -> Option(#(Int, String)) {
  use #(h, rest) <- option.then(take_digits(s, 2))
  use #(m, rest) <- option.then(case rest {
    ":" <> r -> take_digits(r, 2)
    _ -> take_digits(rest, 2)
  })
  case h <= 23 && m <= 59 {
    True -> Some(#(h * 60 + m, rest))
    False -> None
  }
}

fn jsnum_add_minutes(n: JsNum, minutes: Int) -> JsNum {
  case finite_ms(n) {
    Some(ms) -> time_clip(JInt(ms + minutes * 60_000))
    None -> n
  }
}

fn args_to_nums(st: Agent, args: List(JsVal)) -> #(List(JsNum), Agent) {
  let #(rev, st) =
    list.fold(args, #([], st), fn(acc, arg) {
      let #(nums, st) = acc
      let #(n, st) = rt_val.t_to_number(st, arg)
      #([n, ..nums], st)
    })
  #(list.reverse(rev), st)
}

fn date_get_year(
  st: Agent,
  this: JsVal,
  name: String,
  local: TimeRef,
) -> #(JsVal, Agent) {
  use _, tv <- require_time_value(st, this, name)
  case finite_ms(tv) {
    Some(ms) -> {
      let fields = get_date_fields(ms, local)
      #(mk_number(JInt(fields.year - 1900)), st)
    }
    None -> #(mk_number(JNan), st)
  }
}

fn date_set_year(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  name: String,
  local: TimeRef,
) -> #(JsVal, Agent) {
  use h, tv <- require_time_value(st, this, name)
  let arg = helpers.first_arg_or_undefined(args)
  let #(n, st) = rt_val.t_to_number(st, arg)
  case num_to_int(n) {
    Some(yi) -> {
      let yi = case yi >= 0 && yi <= 99 {
        True -> yi + 1900
        False -> yi
      }
      let new_tv = case finite_ms(tv) {
        Some(ms) -> {
          let b = get_date_fields(ms, local)
          make_date(
            yi,
            b.month,
            b.date,
            b.hours,
            b.minutes,
            b.seconds,
            b.ms,
            local,
          )
        }
        None -> make_date(yi, 0, 1, 0, 0, 0, 0, local)
      }
      let st = set_this_time_value(st, h, new_tv)
      #(mk_number(new_tv), st)
    }
    None -> {
      let st = set_this_time_value(st, h, JNan)
      #(mk_number(JNan), st)
    }
  }
}

fn date_to_primitive(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case classify(this) {
    KHandle(h) -> {
      let hint_arg = helpers.first_arg_or_undefined(args)
      case classify(hint_arg) {
        KStr("string") | KStr("default") ->
          rt_val.t_ordinary_to_primitive(st, h, HintString)
        KStr("number") -> rt_val.t_ordinary_to_primitive(st, h, HintNumber)
        _ -> rt_val.t_throw_type_error(st, "Invalid hint")
      }
    }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "Date.prototype[Symbol.toPrimitive] called on non-object",
      )
  }
}

fn date_to_json(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  case rt_val.is_nullish(this) {
    True ->
      rt_val.t_throw_type_error(
        st,
        "Date.prototype.toJSON called on null or undefined",
      )
    False -> {
      let #(o_h, st) = rt_val.t_to_object(st, this)
      let obj = mk_object(o_h)
      let #(prim, st) = rt_val.t_to_primitive(st, obj, HintNumber)
      case classify(prim) {
        KNum(JNan) | KNum(JPosInf) | KNum(JNegInf) -> #(mk_null(), st)
        _ -> invoke_to_iso_string(st, obj)
      }
    }
  }
}

fn invoke_to_iso_string(st: Agent, obj: JsVal) -> #(JsVal, Agent) {
  let #(method, st) = rt_obj.t_get_prop(st, obj, StringKey(nk.to_isostring))
  let #(callable, st) = rt_val.t_is_callable(st, method)
  case callable {
    True -> rt_call.t_call_checked(st, method, obj, [])
    False -> rt_val.t_throw_type_error(st, "toISOString is not a function")
  }
}

fn now_ms(st: Agent) -> Int {
  st.hooks.wall_clock_ms()
}
