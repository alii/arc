//// ES2024 §21.4 Date Objects
////
//// A Date object encapsulates a single time value, an integral Number
//// representing milliseconds since 1970-01-01T00:00:00Z (the epoch), or NaN
//// for an invalid date. The range is exactly -8.64e15 .. 8.64e15 ms (±100M
//// days from the epoch — roughly 271821 BCE to 275760 CE).
////
//// Internal storage: `DateObj(ms: JsNum)` exotic kind. After TimeClip the
//// value is always either `JInt` (an integer in range) or `JNan`. §21.4.4:
//// the prototype is an ORDINARY object (not a Date instance — no
//// [[DateValue]] internal slot).
////
//// Date math (year/month/day/weekday/hour/minute/second/ms breakdown) is
//// pure Gleam Int arithmetic ported from the QuickJS algorithms. The wall
//// clock is `st.hooks.wall_clock_ms`; the local zone is `st.hooks.time_zone`,
//// whose offsets come from `arc/internal/host_time`.

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
  JInt, JNan, JNegInf, JPosInf, KHandle, KNum, KStr, Named, StringKey, classify,
  mk_null, mk_number, mk_object, mk_string,
} as rt_types
import arc/rt/val as rt_val
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// The `Date.prototype.getTimezoneOffset` sign convention: minutes UTC is
/// *ahead of* local (US Pacific Standard is +480). This negation is the only
/// place the runtime flips the sign — everything else, arc_tz_ffi and Temporal
/// included, speaks local − UTC. Deliberately private: a caller that wants an
/// offset wants `host_time.zone_offset_at_utc_ms`, not this getter's inverted
/// convention.
fn js_get_timezone_offset_minutes(zone: TimeZone, epoch_ms: Int) -> Int {
  0 - zone_offset_at_utc_ms(zone, epoch_ms)
}

// ============================================================================
// Init — Date constructor + Date.prototype
// ============================================================================

/// Set up Date constructor + Date.prototype.
///
/// ES2024 §21.4.2: "The Date constructor is %Date%. It is the initial value of
/// the Date property of the global object." Date.length is 7.
///
/// ES2024 §21.4.4: "The Date prototype object is itself an ordinary object. It
/// is not a Date instance and does not have a [[DateValue]] internal slot." —
/// so unlike Boolean/Number we leave the prototype as an ordinary object.
pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
) -> #(BuiltinPair, Agent) {
  // Static methods on Date constructor
  let #(statics, st) =
    common.alloc_methods(st, fn_proto, [
      #("now", DateN(DateNow), 0),
      #("parse", DateN(DateParse), 1),
      #("UTC", DateN(DateUTC), 7),
    ])

  // Date.prototype methods
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

  // §21.4.4.45 Date.prototype [ @@toPrimitive ] ( hint )
  // Property attributes: { writable: false, enumerable: false, configurable: true }
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

// ============================================================================
// Dispatch
// ============================================================================

/// Per-module [[Call]] dispatch for Date native functions.
pub fn dispatch(
  st: Agent,
  native: DateNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  // The name every "called on incompatible receiver" TypeError reports. It has
  // to come from `native` — the shared getter/setter helpers below serve a
  // dozen methods each and cannot know which one they are.
  let name = method_name(native)
  let local = LocalTime(st.hooks.time_zone)
  case native {
    // §21.4.2.1 step 2: when NewTarget is undefined (`Date(...)` called as a
    // function) every argument is ignored and the result is the current time
    // formatted as by `new Date().toString()` — no Date object is allocated.
    // [[Construct]] arrives through `dispatch_construct` instead.
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

/// Per-module [[Construct]] dispatch — §21.4.2.1 steps 3-5.
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

/// The JS-visible name of a Date native, as it appears in error messages.
/// Exhaustive by construction: adding a `DateNative` variant without naming
/// it here is a compile error, so no method can inherit a stale placeholder.
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

// ============================================================================
// Core date math (ported from QuickJS, all Int arithmetic)
// ============================================================================

const ms_per_day = 86_400_000

const max_time_value = 8_640_000_000_000_000

/// Days in month `m` (0-based, as ES exposes it) for year `y`.
fn days_in_month(y: Int, m: Int) -> Int {
  gregorian.days_in_month(y, m + 1)
}

/// ES2024 §21.4.1.31 TimeClip(time). NaN/±Infinity → NaN; finite out-of-range
/// → NaN; otherwise truncate toward zero (a float -0 truncates to Int 0, which
/// is the spec's "add +0" canonicalisation).
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

/// The integral epoch-ms of a stored [[DateValue]], `None` for an Invalid
/// Date. Every value written back went through `time_clip`, so it is `JInt`
/// or `JNan`; a `JFloat` is tolerated for a cell some other route filled.
fn finite_ms(tv: JsNum) -> Option(Int) {
  case tv {
    JInt(i) -> Some(i)
    JFloat(f) -> Some(rt_val.float_to_int(f))
    JNan | JPosInf | JNegInf -> None
  }
}

/// Which coordinate system a time value is interpreted in: local wall-clock
/// time in the host's zone (apply its offset) or UTC.
type TimeRef {
  LocalTime(TimeZone)
  UtcTime
}

/// Broken-down date components (all Int). `tz` is the timezone-offset minutes
/// at the moment in question (local − UTC; 0 for UTC fields).
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

/// One calendar field of a broken-down Date, as named by the get* accessors.
/// Setters use the narrower `SettableField` (weekday is read-only).
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

/// Project a field out of the broken-down record.
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

/// The first field a setX method writes. Unlike `DateField` this excludes
/// weekday (there is no `setDay`), and each variant fixes both WHERE the
/// consecutive-component overwrite starts and HOW MANY arguments the spec
/// admits — so `date_set_field(.., SetHours, ..)` cannot be paired with a
/// wrong `max_args`, and a weekday setter is unrepresentable.
type SettableField {
  SetYear
  SetMonth
  SetDate
  SetHours
  SetMinutes
  SetSeconds
  SetMs
}

/// How many consecutive components a setter may accept, per §21.4.4.18-.30:
/// setHours(h,m,s,ms) → 4, setFullYear(y,m,d) → 3, setMilliseconds(ms) → 1.
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

/// Position of a settable field in the year..ms component order (0..6).
/// The setters overwrite a consecutive run of components starting here.
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

/// Decompose an integral epoch-ms time value into calendar fields. For
/// `LocalTime` the zone's offset for that instant is applied first.
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

/// ES2024 §21.4.1.28 / §21.4.1.29 MakeDay+MakeDate+MakeTime combined.
/// Input is a 7-tuple of already-integerised fields. Works in BEAM Int (no
/// IEEE overflow), with an explicit year-range guard before the big multiply
/// (matches QuickJS) so the product always fits TimeClip's domain check.
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
  // Guard before multiply: years outside this range can never produce a
  // value inside ±8.64e15 ms even with extreme date/time components.
  case ym < -285_426 || ym > 285_426 {
    True -> JNan
    False -> {
      let day = days_from_year(ym) + sum_month_days(ym, mn, 0, 0) + date - 1
      let time = hours * 3_600_000 + minutes * 60_000 + seconds * 1000 + ms
      let tv = day * ms_per_day + time
      // `tv` here is a *local wall clock* in ms, not an instant: resolve it
      // through LocalTZA-for-a-wall-clock, which pins skipped and repeated
      // times to the offset before their transition.
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

/// The seven MakeDay/MakeTime inputs of §21.4.1.28, always all present.
/// Built exclusively by `pad_fields` / `overwrite_fields`, so a wrong number
/// of components is unrepresentable.
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

/// Truncate all seven components toward zero. None if any is non-finite.
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

/// Truncate a finite JsNum toward zero; NaN/±Infinity → None.
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

/// Convert the constructor/Date.UTC components to a time value. Any
/// NaN/Infinity → NaN. All values truncated toward zero. Year in [0,100) is
/// mapped to 1900+year per spec §21.4.2.1 step 5.k.
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

// ============================================================================
// thisTimeValue helper / mutation helper
// ============================================================================

/// ES2024 §21.4.4 thisTimeValue: extract [[DateValue]] from a Date object,
/// or None if `this` is not a Date.
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

/// Guard that `this` is a Date; on failure raises a TypeError, otherwise
/// continues into `k` with the handle + time value.
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

/// Write a new [[DateValue]] into the Date object at `h`.
fn set_this_time_value(st: Agent, h: Handle, tv: JsNum) -> Agent {
  rt_store.t_cell_update(st, h, fn(slot) {
    // Only reachable via `require_time_value`, which proved this handle holds
    // a Date. Silently keeping the old slot would drop the store while every
    // caller still reports the new time value back to JS.
    let assert rt_types.SObject(kind: DateObj(_), ..) as obj = slot
      as "date: slot is not a Date object"
    rt_types.SObject(..obj, kind: DateObj(ms: tv))
  })
}

// ============================================================================
// Constructor / static methods
// ============================================================================

/// ES2024 §21.4.2.1 Date ( ...values ) via [[Construct]]
///
/// 0 args → now; 1 arg → time value or parsed string; 2..7 args → component
/// fields interpreted as local time. Steps 6-7: the new object's prototype
/// comes from NewTarget (OrdinaryCreateFromConstructor).
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

/// Single-argument constructor path: clone a Date, parse a string, or
/// ToNumber+TimeClip.
fn single_arg_time_value(
  st: Agent,
  arg: JsVal,
  local: TimeRef,
) -> #(JsNum, Agent) {
  // §21.4.2.1 step 4.b: if value is a Date object, copy its [[DateValue]].
  case this_time_value(st, arg) {
    Some(#(_, tv)) -> #(time_clip(tv), st)
    None -> {
      // ToPrimitive(value) → string? parse : ToNumber+TimeClip
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

/// Coerce an N-arg list (1..7) to a time value with full ToNumber re-entry.
/// Missing fields default to month=0, date=1, h/m/s/ms=0. Extra args ignored.
fn args_to_time_value(
  st: Agent,
  args: List(JsVal),
  time_ref: TimeRef,
) -> #(JsNum, Agent) {
  let #(nums, st) = args_to_nums(st, list.take(args, 7))
  #(make_date_checked(pad_fields(nums), time_ref), st)
}

/// Turn the 1..7 supplied constructor/Date.UTC arguments into the full
/// component record, filling the unsupplied tail with the spec defaults
/// (month 0, date 1, time 0). This is the ONE place that handles arity; the
/// zero-argument arm is unreachable (both callers special-case it) and yields
/// a NaN year so the result would be NaN.
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

/// ES2024 §21.4.3.1 Date.parse ( string )
fn date_parse(st: Agent, args: List(JsVal), local: TimeRef) -> #(JsVal, Agent) {
  let arg = helpers.first_arg_or_undefined(args)
  let #(s, st) = rt_val.t_to_string(st, arg)
  #(mk_number(parse_date_string(s, local)), st)
}

/// ES2024 §21.4.3.4 Date.UTC ( year [, month [, date [, hours ...]]] )
/// 0 args → NaN; 1+ args → fields interpreted as UTC, year-mapping applied.
fn date_utc(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  case args {
    [] -> #(mk_number(JNan), st)
    many -> {
      let #(tv, st) = args_to_time_value(st, many, UtcTime)
      #(mk_number(tv), st)
    }
  }
}

// ============================================================================
// Prototype getters
// ============================================================================

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

/// Shared getter: read [[DateValue]], decompose, return one field.
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

// ============================================================================
// Prototype setters
// ============================================================================

/// ES2024 §21.4.4.27 Date.prototype.setTime ( time )
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

/// Shared setter. `first` names the first field being written; the number of
/// consecutive fields the caller may supply is derived from it. Ported from
/// QuickJS `set_date_field`.
///
/// When `first` is `SetYear` (setFullYear/setUTCFullYear) and the current
/// value is NaN, the spec uses +0 as the base time (§21.4.4.21 step 5). For
/// all other setters, NaN base → result stays NaN.
fn date_set_field(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  name: String,
  first: SettableField,
  time_ref: TimeRef,
) -> #(JsVal, Agent) {
  use h, tv <- require_time_value(st, this, name)
  // Coerce supplied args (capped at the spec arity for `first`) to JsNum —
  // full ToNumber so valueOf side effects and abrupt completions are observed
  // in order.
  let supplied = list.take(args, settable_max_args(first))
  let #(new_nums, st) = args_to_nums(st, supplied)
  case compute_set_field(tv, first, new_nums, time_ref) {
    // Original [[DateValue]] was NaN (and this isn't setFullYear): per
    // spec step "If t is NaN, return NaN" — early return WITHOUT
    // writing back, so any side-effect setTime in valueOf is preserved.
    None -> #(mk_number(JNan), st)
    Some(result) -> {
      // Per spec, if no argument was supplied the result is NaN.
      let result = case args {
        [] -> JNan
        _ -> result
      }
      let st = set_this_time_value(st, h, result)
      #(mk_number(result), st)
    }
  }
}

/// Compute new time value for a setter. Returns None for the "original t was
/// NaN" early-out (caller must NOT write back), Some(tv) otherwise.
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
        // setFullYear on Invalid Date: per §21.4.4.21 step 5, t becomes +0
        // (NOT LocalTime(+0)) → Year 1970, Month 0, Date 1, all-zero time.
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

/// The year..ms components of a broken-down date as (finite) JsNums.
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

/// Replace `len(new_nums)` consecutive components starting at `first` with
/// the supplied values; every other component keeps its base value.
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

/// Component `i` of the merge: at/after `lo` take the supplied value at that
/// offset if one was given, otherwise keep the base value.
fn merge_field(base: JsNum, i: Int, lo: Int, new_nums: List(JsNum)) -> JsNum {
  case i >= lo {
    True -> helpers.list_at(new_nums, i - lo) |> option.unwrap(base)
    False -> base
  }
}

/// Variant of make_date_checked for the setters: the merged components are
/// already integral fields, so no [0,100) year mapping is applied —
/// setFullYear sets the year literally. Any non-finite component → NaN.
fn make_date_from_components(c: DateComponents, time_ref: TimeRef) -> JsNum {
  case components_to_ints(c) {
    None -> JNan
    Some(#(y, mon, dt, h, mi, s, ms)) ->
      make_date(y, mon, dt, h, mi, s, ms, time_ref)
  }
}

// ============================================================================
// String formatting
// ============================================================================

/// A toString-family output format. FmtIso and FmtUtc always render the full
/// date-and-time string; only FmtLocal/FmtLocale have Date-only / Time-only
/// halves, so the `DatePart` selector lives on those variants and a nonsense
/// combination like "ISO, time-only" is unrepresentable.
type DateFmt {
  FmtLocal(DatePart)
  FmtUtc
  FmtIso
  FmtLocale(DatePart)
}

/// Which half of a formatted date string a toString-family method returns:
/// to*DateString → DateOnly, to*TimeString → TimeOnly, toString → both.
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

/// ES2024 §21.4.4.41-.43 + .35-.39 toString family.
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

/// "YYYY-MM-DDTHH:mm:ss.sssZ" — extended-year form for years outside 0..9999.
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

/// "Sat, 02 Jan 2021 03:04:05 GMT"
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

/// Full toString / toDateString / toTimeString.
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

/// Minimal locale formatting — "M/D/YYYY, HH:mm:ss AM/PM" enough to satisfy
/// type/shape tests; spec leaves the exact format implementation-defined.
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

/// "+HHMM" / "-HHMM" from local-minus-UTC minutes (`DateFields.tz`).
fn format_tz(tz: Int) -> String {
  let sign = case tz < 0 {
    True -> "-"
    False -> "+"
  }
  let a = int.absolute_value(tz)
  sign <> pad2(a / 60) <> pad2(a % 60)
}

// ============================================================================
// Date.parse — minimal ISO-8601 + Date.prototype.toString round-trip
// ============================================================================

/// ES2024 §21.4.1.32 Date Time String Format. Handles the spec-required
/// `YYYY[-MM[-DD]][THH:mm[:ss[.sss]]][Z|±HH:mm]` form plus the extended-year
/// `±YYYYYY` prefix. Anything else → NaN.
fn parse_date_string(s: String, local: TimeRef) -> JsNum {
  let s = string.trim(s)
  parse_iso(s, local) |> option.unwrap(JNan)
}

/// The parsed time-of-day of a date-time form. Its absence (`None` at the call
/// site) means the string was date-only — never "the time was there but some
/// required field of it wasn't", which is what the old all-optional tuple with
/// its always-`True` `has_time` flag could not distinguish.
type IsoTime {
  IsoTime(hours: Int, minutes: Int, seconds: Int, ms: Int)
}

fn parse_iso(s: String, local: TimeRef) -> Option(JsNum) {
  // Year: "+YYYYYY" / "-YYYYYY" / "YYYY"
  use #(year, rest) <- option.then(parse_year(s))
  // Month + day (optional)
  let #(mon, rest) = parse_dash_int(rest, 2) |> option.unwrap(#(1, rest))
  let #(day, rest) = parse_dash_int(rest, 2) |> option.unwrap(#(1, rest))
  // Time (optional, after "T"). Once "T" is seen, "HH:mm" is mandatory.
  use #(time, rest) <- option.then(case rest {
    "T" <> t -> parse_time(t) |> option.map(fn(p) { #(Some(p.0), p.1) })
    _ -> Some(#(None, rest))
  })
  let IsoTime(h, mi, sec, ms) = option.unwrap(time, IsoTime(0, 0, 0, 0))
  // Zone (optional). Date-only forms are UTC; date-time forms with no zone
  // are local time per spec.
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

/// Component range gate for §21.4.1.32. Out-of-range components make the whole
/// string invalid — they must NOT reach `make_date`, whose MakeDay/MakeTime
/// arithmetic happily rolls "2021-13-01" over into 2022 and "2021-01-01T25:00"
/// into the next day. Parsing rejects; only construction rolls over.
/// The zone offset is gated separately, in `parse_hhmm`.
fn validate_iso(
  year: Int,
  mon: Int,
  day: Int,
  h: Int,
  mi: Int,
  sec: Int,
  ms: Int,
) -> Option(Nil) {
  // HH may be 24 only as the "end of day" designator, i.e. exactly 24:00:00.000.
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
    // "-000000" is not a valid extended year: the year 0 is positive and must
    // be written "+000000" (test262 built-ins/Date/parse/year-zero.js).
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

/// `HH:mm[:ss[.sss]]`. `HH:mm` is required; every optional part that claims a
/// separator must then supply all of its digits.
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

/// The time zone designation (or lack of one) at the end of an ISO string.
/// `FixedOffset(minutes)` follows the getTimezoneOffset sign convention
/// (UTC − local): it is the correction to ADD to the wall-clock components
/// interpreted as UTC. `LocalZone` means "no designator" — the caller derives
/// the host offset at the parsed instant instead.
type Zone {
  FixedOffset(minutes: Int)
  LocalZone
}

/// Parse the optional trailing zone designator, returning the zone and the
/// unconsumed remainder. `has_time` selects the no-designator default:
/// date-only → UTC, date-time → local. That is the whole rule — the extended
/// year prefix does not enter into it (test262
/// built-ins/Date/parse/without-utc-offset.js).
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

/// The `HH:mm` / `HHmm` body of a `±` offset. Minutes are not optional — a
/// bare `+05` is not a Date Time String Format offset. The TimeZoneUTCOffset
/// production also bounds the digits (HH 00-23, mm 00-59), so `+99:99` and
/// `-24:00` are parse failures, not offsets that roll over.
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

/// Coerce a list of args to JsNum, threading state; a throwing valueOf raises
/// at its position in the list. Used by the constructor multi-arg path,
/// Date.UTC and the setters.
fn args_to_nums(st: Agent, args: List(JsVal)) -> #(List(JsNum), Agent) {
  let #(rev, st) =
    list.fold(args, #([], st), fn(acc, arg) {
      let #(nums, st) = acc
      let #(n, st) = rt_val.t_to_number(st, arg)
      #([n, ..nums], st)
    })
  #(list.reverse(rev), st)
}

// ============================================================================
// Annex B: getYear / setYear
// ============================================================================

/// Annex B §B.2.3.1 Date.prototype.getYear ( ) — returns FullYear - 1900.
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

/// Annex B §B.2.3.2 Date.prototype.setYear ( year )
/// Year in [0,99] maps to 1900+year; otherwise sets the full year literally.
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
      // Base on local-time fields of current value; if NaN, t=+0 →
      // Month 0, Date 1, all-zero time (NOT LocalTime(+0)).
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

// ============================================================================
// @@toPrimitive / toJSON
// ============================================================================

/// ES2024 §21.4.4.45 Date.prototype [ @@toPrimitive ] ( hint )
///
///   1. Let O be the this value.
///   2. If O is not an Object, throw a TypeError.
///   3. If hint is "string" or "default", let tryFirst be string.
///   4. Else if hint is "number", let tryFirst be number.
///   5. Else throw a TypeError.
///   6. Return ? OrdinaryToPrimitive(O, tryFirst).
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

/// ES2024 §21.4.4.37 Date.prototype.toJSON ( key )
///
///   1. Let O be ? ToObject(this value).
///   2. Let tv be ? ToPrimitive(O, number).
///   3. If tv is a Number and tv is not finite, return null.
///   4. Return ? Invoke(O, "toISOString").
fn date_to_json(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  // Step 1: ToObject(this value).
  case rt_val.is_nullish(this) {
    True ->
      rt_val.t_throw_type_error(
        st,
        "Date.prototype.toJSON called on null or undefined",
      )
    False -> {
      let #(o_h, st) = rt_val.t_to_object(st, this)
      let obj = mk_object(o_h)
      // Step 2: ToPrimitive(O, number).
      let #(prim, st) = rt_val.t_to_primitive(st, obj, HintNumber)
      case classify(prim) {
        // Step 3: non-finite Number → return null.
        KNum(JNan) | KNum(JPosInf) | KNum(JNegInf) -> #(mk_null(), st)
        // Step 4: Invoke(O, "toISOString").
        _ -> invoke_to_iso_string(st, obj)
      }
    }
  }
}

/// Generic Invoke(O, "toISOString") — looks up via prototype chain and calls.
fn invoke_to_iso_string(st: Agent, obj: JsVal) -> #(JsVal, Agent) {
  let #(method, st) =
    rt_obj.t_get_prop(st, obj, StringKey(Named("toISOString")))
  let #(callable, st) = rt_val.t_is_callable(st, method)
  case callable {
    True -> rt_call.t_call_checked(st, method, obj, [])
    False -> rt_val.t_throw_type_error(st, "toISOString is not a function")
  }
}

// ============================================================================
// low-level helpers
// ============================================================================

fn now_ms(st: Agent) -> Int {
  st.hooks.wall_clock_ms()
}
