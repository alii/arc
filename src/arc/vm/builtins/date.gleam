/// ES2024 §21.4 Date Objects
///
/// A Date object encapsulates a single time value, an integral Number
/// representing milliseconds since 1970-01-01T00:00:00Z (the epoch), or NaN
/// for an invalid date. The range is exactly -8.64e15 .. 8.64e15 ms (±100M
/// days from the epoch — roughly 271821 BCE to 275760 CE).
///
/// Internal storage: `DateObject(time_value: JsNum)` exotic kind. After
/// TimeClip the value is always either `Finite(Float)` (an integer in range)
/// or `NaN`.
///
/// Date math (year/month/day/weekday/hour/minute/second/ms breakdown) is done
/// in pure Gleam Int arithmetic ported from the QuickJS algorithms; only the
/// wall clock and the local-zone offset lookups go through FFI, and those live
/// in `arc/internal/host_time`.
import arc/internal/digits.{take_digits}
import arc/internal/gregorian.{
  civil_from_days, days_from_year, floor_div, floor_mod as math_mod,
}
import arc/internal/host_time.{now_ms, offset_at_local_ms, offset_at_utc_ms}
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers
import arc/vm/heap
import arc/vm/key.{Named}
import arc/vm/ops/coerce
import arc/vm/ops/object as ops_object
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type DateNativeFn, type JsNum, type JsValue, type Ref, DateConstructor,
  DateNative, DateNow, DateObject, DateParse, DatePrototypeGetDate,
  DatePrototypeGetDay, DatePrototypeGetFullYear, DatePrototypeGetHours,
  DatePrototypeGetMilliseconds, DatePrototypeGetMinutes, DatePrototypeGetMonth,
  DatePrototypeGetSeconds, DatePrototypeGetTime, DatePrototypeGetTimezoneOffset,
  DatePrototypeGetUTCDate, DatePrototypeGetUTCDay, DatePrototypeGetUTCFullYear,
  DatePrototypeGetUTCHours, DatePrototypeGetUTCMilliseconds,
  DatePrototypeGetUTCMinutes, DatePrototypeGetUTCMonth,
  DatePrototypeGetUTCSeconds, DatePrototypeGetYear, DatePrototypeSetDate,
  DatePrototypeSetFullYear, DatePrototypeSetHours, DatePrototypeSetMilliseconds,
  DatePrototypeSetMinutes, DatePrototypeSetMonth, DatePrototypeSetSeconds,
  DatePrototypeSetTime, DatePrototypeSetUTCDate, DatePrototypeSetUTCFullYear,
  DatePrototypeSetUTCHours, DatePrototypeSetUTCMilliseconds,
  DatePrototypeSetUTCMinutes, DatePrototypeSetUTCMonth,
  DatePrototypeSetUTCSeconds, DatePrototypeSetYear,
  DatePrototypeSymbolToPrimitive, DatePrototypeToDateString,
  DatePrototypeToISOString, DatePrototypeToJSON, DatePrototypeToLocaleDateString,
  DatePrototypeToLocaleString, DatePrototypeToLocaleTimeString,
  DatePrototypeToString, DatePrototypeToTimeString, DatePrototypeToUTCString,
  DatePrototypeValueOf, DateUTC, Dispatch, Finite, JsNull, JsNumber, JsObject,
  JsString, JsUndefined, NaN, ObjectSlot,
}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// The `Date.prototype.getTimezoneOffset` sign convention: minutes UTC is
/// *ahead of* local (US Pacific Standard is +480). This negation is the only
/// place the runtime flips the sign — everything else, arc_tz_ffi and Temporal
/// included, speaks local − UTC. Deliberately private: a caller that wants an
/// offset wants `host_time.offset_at_utc_ms`, not this getter's inverted
/// convention.
fn js_get_timezone_offset_minutes(epoch_ms: Int) -> Int {
  0 - offset_at_utc_ms(epoch_ms)
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
/// so unlike Boolean/Number we leave the prototype as OrdinaryObject.
pub fn init(
  h: Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(host), BuiltinType) {
  // Static methods on Date constructor
  let #(h, static_methods) =
    common.alloc_methods(h, function_proto, [
      #("now", DateNative(DateNow), 0),
      #("parse", DateNative(DateParse), 1),
      #("UTC", DateNative(DateUTC), 7),
    ])

  // Date.prototype methods
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("valueOf", DateNative(DatePrototypeValueOf), 0),
      #("getTime", DateNative(DatePrototypeGetTime), 0),
      #("getTimezoneOffset", DateNative(DatePrototypeGetTimezoneOffset), 0),
      #("getFullYear", DateNative(DatePrototypeGetFullYear), 0),
      #("getUTCFullYear", DateNative(DatePrototypeGetUTCFullYear), 0),
      #("getMonth", DateNative(DatePrototypeGetMonth), 0),
      #("getUTCMonth", DateNative(DatePrototypeGetUTCMonth), 0),
      #("getDate", DateNative(DatePrototypeGetDate), 0),
      #("getUTCDate", DateNative(DatePrototypeGetUTCDate), 0),
      #("getDay", DateNative(DatePrototypeGetDay), 0),
      #("getUTCDay", DateNative(DatePrototypeGetUTCDay), 0),
      #("getHours", DateNative(DatePrototypeGetHours), 0),
      #("getUTCHours", DateNative(DatePrototypeGetUTCHours), 0),
      #("getMinutes", DateNative(DatePrototypeGetMinutes), 0),
      #("getUTCMinutes", DateNative(DatePrototypeGetUTCMinutes), 0),
      #("getSeconds", DateNative(DatePrototypeGetSeconds), 0),
      #("getUTCSeconds", DateNative(DatePrototypeGetUTCSeconds), 0),
      #("getMilliseconds", DateNative(DatePrototypeGetMilliseconds), 0),
      #("getUTCMilliseconds", DateNative(DatePrototypeGetUTCMilliseconds), 0),
      #("setTime", DateNative(DatePrototypeSetTime), 1),
      #("setMilliseconds", DateNative(DatePrototypeSetMilliseconds), 1),
      #("setUTCMilliseconds", DateNative(DatePrototypeSetUTCMilliseconds), 1),
      #("setSeconds", DateNative(DatePrototypeSetSeconds), 2),
      #("setUTCSeconds", DateNative(DatePrototypeSetUTCSeconds), 2),
      #("setMinutes", DateNative(DatePrototypeSetMinutes), 3),
      #("setUTCMinutes", DateNative(DatePrototypeSetUTCMinutes), 3),
      #("setHours", DateNative(DatePrototypeSetHours), 4),
      #("setUTCHours", DateNative(DatePrototypeSetUTCHours), 4),
      #("setDate", DateNative(DatePrototypeSetDate), 1),
      #("setUTCDate", DateNative(DatePrototypeSetUTCDate), 1),
      #("setMonth", DateNative(DatePrototypeSetMonth), 2),
      #("setUTCMonth", DateNative(DatePrototypeSetUTCMonth), 2),
      #("setFullYear", DateNative(DatePrototypeSetFullYear), 3),
      #("setUTCFullYear", DateNative(DatePrototypeSetUTCFullYear), 3),
      #("getYear", DateNative(DatePrototypeGetYear), 0),
      #("setYear", DateNative(DatePrototypeSetYear), 1),
      #("toString", DateNative(DatePrototypeToString), 0),
      #("toDateString", DateNative(DatePrototypeToDateString), 0),
      #("toTimeString", DateNative(DatePrototypeToTimeString), 0),
      #("toISOString", DateNative(DatePrototypeToISOString), 0),
      #("toUTCString", DateNative(DatePrototypeToUTCString), 0),
      #("toGMTString", DateNative(DatePrototypeToUTCString), 0),
      #("toLocaleString", DateNative(DatePrototypeToLocaleString), 0),
      #("toLocaleDateString", DateNative(DatePrototypeToLocaleDateString), 0),
      #("toLocaleTimeString", DateNative(DatePrototypeToLocaleTimeString), 0),
      #("toJSON", DateNative(DatePrototypeToJSON), 1),
    ])

  let #(h, bt) =
    common.init_type(
      h,
      object_proto,
      function_proto,
      proto_methods,
      fn(proto) { Dispatch(DateNative(DateConstructor(proto:))) },
      "Date",
      7,
      static_methods,
    )

  // §21.4.4.45 Date.prototype [ @@toPrimitive ] ( hint )
  // Property attributes: { writable: false, enumerable: false, configurable: true }
  let #(h, to_prim_ref) =
    common.alloc_native_fn(
      h,
      function_proto,
      DateNative(DatePrototypeSymbolToPrimitive),
      "[Symbol.toPrimitive]",
      1,
    )
  let h =
    common.add_symbol_property(
      h,
      bt.prototype,
      value.symbol_to_primitive,
      value.data(JsObject(to_prim_ref)) |> value.configurable(),
    )

  #(h, bt)
}

// ============================================================================
// Dispatch
// ============================================================================

/// Per-module dispatch for Date native functions.
pub fn dispatch(
  native: DateNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // The name every "called on incompatible receiver" TypeError reports. It has
  // to come from `native` — the shared getter/setter helpers below serve a
  // dozen methods each and cannot know which one they are.
  let name = method_name(native)
  case native {
    DateConstructor(proto:) -> date_constructor(proto, args, state)
    DateNow -> #(state, Ok(value.from_int(now_ms())))
    DateParse -> date_parse(args, state)
    DateUTC -> date_utc(args, state)
    DatePrototypeValueOf | DatePrototypeGetTime ->
      date_get_time(this, state, name)
    DatePrototypeGetTimezoneOffset -> date_get_tz_offset(this, state, name)
    DatePrototypeGetFullYear ->
      date_get_field(this, state, name, FieldYear, LocalTime)
    DatePrototypeGetUTCFullYear ->
      date_get_field(this, state, name, FieldYear, UtcTime)
    DatePrototypeGetMonth ->
      date_get_field(this, state, name, FieldMonth, LocalTime)
    DatePrototypeGetUTCMonth ->
      date_get_field(this, state, name, FieldMonth, UtcTime)
    DatePrototypeGetDate ->
      date_get_field(this, state, name, FieldDate, LocalTime)
    DatePrototypeGetUTCDate ->
      date_get_field(this, state, name, FieldDate, UtcTime)
    DatePrototypeGetDay ->
      date_get_field(this, state, name, FieldWeekday, LocalTime)
    DatePrototypeGetUTCDay ->
      date_get_field(this, state, name, FieldWeekday, UtcTime)
    DatePrototypeGetHours ->
      date_get_field(this, state, name, FieldHours, LocalTime)
    DatePrototypeGetUTCHours ->
      date_get_field(this, state, name, FieldHours, UtcTime)
    DatePrototypeGetMinutes ->
      date_get_field(this, state, name, FieldMinutes, LocalTime)
    DatePrototypeGetUTCMinutes ->
      date_get_field(this, state, name, FieldMinutes, UtcTime)
    DatePrototypeGetSeconds ->
      date_get_field(this, state, name, FieldSeconds, LocalTime)
    DatePrototypeGetUTCSeconds ->
      date_get_field(this, state, name, FieldSeconds, UtcTime)
    DatePrototypeGetMilliseconds ->
      date_get_field(this, state, name, FieldMs, LocalTime)
    DatePrototypeGetUTCMilliseconds ->
      date_get_field(this, state, name, FieldMs, UtcTime)
    DatePrototypeSetTime -> date_set_time(this, args, state, name)
    DatePrototypeSetMilliseconds ->
      date_set_field(this, args, state, name, FieldMs, 1, LocalTime)
    DatePrototypeSetUTCMilliseconds ->
      date_set_field(this, args, state, name, FieldMs, 1, UtcTime)
    DatePrototypeSetSeconds ->
      date_set_field(this, args, state, name, FieldSeconds, 2, LocalTime)
    DatePrototypeSetUTCSeconds ->
      date_set_field(this, args, state, name, FieldSeconds, 2, UtcTime)
    DatePrototypeSetMinutes ->
      date_set_field(this, args, state, name, FieldMinutes, 3, LocalTime)
    DatePrototypeSetUTCMinutes ->
      date_set_field(this, args, state, name, FieldMinutes, 3, UtcTime)
    DatePrototypeSetHours ->
      date_set_field(this, args, state, name, FieldHours, 4, LocalTime)
    DatePrototypeSetUTCHours ->
      date_set_field(this, args, state, name, FieldHours, 4, UtcTime)
    DatePrototypeSetDate ->
      date_set_field(this, args, state, name, FieldDate, 1, LocalTime)
    DatePrototypeSetUTCDate ->
      date_set_field(this, args, state, name, FieldDate, 1, UtcTime)
    DatePrototypeSetMonth ->
      date_set_field(this, args, state, name, FieldMonth, 2, LocalTime)
    DatePrototypeSetUTCMonth ->
      date_set_field(this, args, state, name, FieldMonth, 2, UtcTime)
    DatePrototypeSetFullYear ->
      date_set_field(this, args, state, name, FieldYear, 3, LocalTime)
    DatePrototypeSetUTCFullYear ->
      date_set_field(this, args, state, name, FieldYear, 3, UtcTime)
    DatePrototypeGetYear -> date_get_year(this, state, name)
    DatePrototypeSetYear -> date_set_year(this, args, state, name)
    DatePrototypeToString ->
      date_to_string(this, state, name, FmtLocal, DateAndTime)
    DatePrototypeToDateString ->
      date_to_string(this, state, name, FmtLocal, DateOnly)
    DatePrototypeToTimeString ->
      date_to_string(this, state, name, FmtLocal, TimeOnly)
    DatePrototypeToISOString ->
      date_to_string(this, state, name, FmtIso, DateAndTime)
    DatePrototypeToUTCString ->
      date_to_string(this, state, name, FmtUtc, DateAndTime)
    DatePrototypeToLocaleString ->
      date_to_string(this, state, name, FmtLocale, DateAndTime)
    DatePrototypeToLocaleDateString ->
      date_to_string(this, state, name, FmtLocale, DateOnly)
    DatePrototypeToLocaleTimeString ->
      date_to_string(this, state, name, FmtLocale, TimeOnly)
    DatePrototypeToJSON -> date_to_json(this, state)
    DatePrototypeSymbolToPrimitive -> date_to_primitive(this, args, state)
  }
}

/// The JS-visible name of a Date native, as it appears in error messages.
/// Exhaustive by construction: adding a `DateNativeFn` variant without naming
/// it here is a compile error, so no method can inherit a stale placeholder.
fn method_name(native: DateNativeFn) -> String {
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

const max_time_value = 8.64e15

/// Days in month `m` (0-based, as ES exposes it) for year `y`.
fn days_in_month(y: Int, m: Int) -> Int {
  gregorian.days_in_month(y, m + 1)
}

/// ES2024 §21.4.1.31 TimeClip(time). NaN/±Infinity → NaN; finite out-of-range
/// → NaN; otherwise truncate toward zero and add +0 to canonicalize -0.
fn time_clip(t: JsNum) -> JsNum {
  case t {
    Finite(f) -> {
      let neg_max = float.negate(max_time_value)
      case f >=. neg_max && f <=. max_time_value {
        True -> Finite(int.to_float(value.float_to_int(f)) +. 0.0)
        False -> NaN
      }
    }
    _ -> NaN
  }
}

/// Which coordinate system a time value is interpreted in: local wall-clock
/// time (apply the FFI timezone offset) or UTC.
type TimeRef {
  LocalTime
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

/// One calendar field of a broken-down Date, as named by the get*/set*
/// accessor pairs. The first seven (year..ms) are also the setters' write
/// targets, in `setHours(h, m, s, ms)` argument order — see `field_index`.
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

/// Position of a field in the year..ms component order (0..6, weekday last).
/// The setters overwrite a consecutive run of components starting here.
fn field_index(field: DateField) -> Int {
  case field {
    FieldYear -> 0
    FieldMonth -> 1
    FieldDate -> 2
    FieldHours -> 3
    FieldMinutes -> 4
    FieldSeconds -> 5
    FieldMs -> 6
    FieldWeekday -> 7
  }
}

/// Decompose an integral epoch-ms time value into calendar fields. For
/// `LocalTime` the FFI timezone offset for that instant is applied first.
fn get_date_fields(tv: Int, time_ref: TimeRef) -> DateFields {
  let tz = case time_ref {
    LocalTime -> offset_at_utc_ms(tv)
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
/// (matches QuickJS) so we never overflow Float when converting back.
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
    True -> NaN
    False -> {
      let day = days_from_year(ym) + sum_month_days(ym, mn, 0, 0) + date - 1
      let time = hours * 3_600_000 + minutes * 60_000 + seconds * 1000 + ms
      let tv = day * ms_per_day + time
      // `tv` here is a *local wall clock* in ms, not an instant: resolve it
      // through LocalTZA-for-a-wall-clock, which pins skipped and repeated
      // times to the offset before their transition.
      let tv = case time_ref {
        LocalTime -> tv - offset_at_local_ms(tv) * 60_000
        UtcTime -> tv
      }
      time_clip(Finite(int.to_float(tv)))
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
    Finite(f) -> Some(value.float_to_int(f))
    _ -> None
  }
}

/// Convert the constructor/Date.UTC components to a time value. Any
/// NaN/Infinity → NaN. All values truncated toward zero. Year in [0,100) is
/// mapped to 1900+year per spec §21.4.2.1 step 5.k.
fn make_date_checked(c: DateComponents, time_ref: TimeRef) -> JsNum {
  case components_to_ints(c) {
    None -> NaN
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
fn this_time_value(state: State(host), this: JsValue) -> Option(#(Ref, JsNum)) {
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: DateObject(time_value:), ..)) ->
          Some(#(ref, time_value))
        _ -> None
      }
    _ -> None
  }
}

/// Guard that `this` is a Date; on failure produces a TypeError, otherwise
/// continues into `k` with the ref + time value.
fn require_time_value(
  state: State(host),
  this: JsValue,
  name: String,
  k: fn(Ref, JsNum) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this_time_value(state, this) {
    Some(#(ref, tv)) -> k(ref, tv)
    None ->
      state.type_error(
        state,
        "Date.prototype." <> name <> " called on incompatible receiver",
      )
  }
}

/// Write a new [[DateValue]] into the Date object at `ref`.
fn set_this_time_value(state: State(host), ref: Ref, tv: JsNum) -> State(host) {
  let heap =
    heap.update(state.heap, ref, fn(slot) {
      // Only reachable via `require_time_value`, which proved this ref holds a
      // Date. Silently keeping the old slot would drop the store while every
      // caller still reports the new time value back to JS.
      let assert ObjectSlot(kind: DateObject(_), ..) as obj = slot
        as "date: slot is not a Date object"
      ObjectSlot(..obj, kind: DateObject(time_value: tv))
    })
  State(..state, heap:)
}

// ============================================================================
// Constructor / static methods
// ============================================================================

/// ES2024 §21.4.2.1 Date ( ...values )
///
/// 0 args → now; 1 arg → time value or parsed string; 2..7 args → component
/// fields interpreted as local time.
///
/// §21.4.2.4: when NewTarget is undefined (`Date(...)` called as a function)
/// every argument is ignored and the result is the current time formatted as
/// by `new Date().toString()` — no Date object is allocated. `do_construct`
/// sets `state.new_target` before native dispatch; a plain call leaves it
/// `JsUndefined`.
fn date_constructor(
  proto: Ref,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case state.new_target {
    JsUndefined -> {
      let fields = get_date_fields(now_ms(), LocalTime)
      #(state, Ok(JsString(format_date(FmtLocal, DateAndTime, fields))))
    }
    _ -> {
      let #(state, tv_result) = case args {
        [] -> #(state, Ok(Finite(int.to_float(now_ms()))))
        [single] -> single_arg_time_value(state, single)
        many -> args_to_time_value(state, many, LocalTime)
      }
      case tv_result {
        Error(e) -> #(state, Error(e))
        Ok(tv) -> {
          let #(heap, ref) =
            common.alloc_wrapper(state.heap, DateObject(time_value: tv), proto)
          #(State(..state, heap:), Ok(JsObject(ref)))
        }
      }
    }
  }
}

/// Single-argument constructor path: clone a Date, parse a string, or
/// ToNumber+TimeClip.
fn single_arg_time_value(
  state: State(host),
  arg: JsValue,
) -> #(State(host), Result(JsNum, JsValue)) {
  // §21.4.2.1 step 4.b: if value is a Date object, copy its [[DateValue]].
  case this_time_value(state, arg) {
    Some(#(_, tv)) -> #(state, Ok(time_clip(tv)))
    None ->
      // ToPrimitive(value) → string? parse : ToNumber+TimeClip
      case coerce.to_primitive(state, arg, coerce.DefaultHint) {
        Error(#(e, st)) -> #(st, Error(e))
        Ok(#(JsString(s), st)) -> #(st, Ok(parse_date_string(s)))
        Ok(#(prim, st)) ->
          case coerce.js_to_number(st, prim) {
            Error(#(e, st)) -> #(st, Error(e))
            Ok(#(n, st)) -> #(st, Ok(time_clip(n)))
          }
      }
  }
}

/// Coerce an N-arg list (1..7) to a time value with full ToNumber re-entry.
/// Missing fields default to month=0, date=1, h/m/s/ms=0. Extra args ignored.
fn args_to_time_value(
  state: State(host),
  args: List(JsValue),
  time_ref: TimeRef,
) -> #(State(host), Result(JsNum, JsValue)) {
  use nums, st <- state.try_op(args_to_nums(state, list.take(args, 7)))
  #(st, Ok(make_date_checked(pad_fields(nums), time_ref)))
}

/// Turn the 1..7 supplied constructor/Date.UTC arguments into the full
/// component record, filling the unsupplied tail with the spec defaults
/// (month 0, date 1, time 0). This is the ONE place that handles arity; the
/// zero-argument arm is unreachable (both callers special-case it) and yields
/// a NaN year so the result would be NaN.
fn pad_fields(nums: List(JsNum)) -> DateComponents {
  let zero = Finite(0.0)
  let one = Finite(1.0)
  case nums {
    [] -> DateComponents(NaN, zero, one, zero, zero, zero, zero)
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
fn date_parse(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let arg = helpers.first_arg_or_undefined(args)
  use s, st <- state.try_op(coerce.js_to_string(state, arg))
  #(st, Ok(JsNumber(parse_date_string(s))))
}

/// ES2024 §21.4.3.4 Date.UTC ( year [, month [, date [, hours ...]]] )
/// 0 args → NaN; 1+ args → fields interpreted as UTC, year-mapping applied.
fn date_utc(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case args {
    [] -> #(state, Ok(JsNumber(NaN)))
    many -> {
      let #(st, r) = args_to_time_value(state, many, UtcTime)
      #(st, result.map(r, JsNumber))
    }
  }
}

// ============================================================================
// Prototype getters
// ============================================================================

fn date_get_time(
  this: JsValue,
  state: State(host),
  name: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  use _, tv <- require_time_value(state, this, name)
  #(state, Ok(JsNumber(tv)))
}

fn date_get_tz_offset(
  this: JsValue,
  state: State(host),
  name: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  use _, tv <- require_time_value(state, this, name)
  case tv {
    Finite(f) -> #(
      state,
      Ok(
        JsNumber(
          Finite(
            int.to_float(js_get_timezone_offset_minutes(value.float_to_int(f))),
          ),
        ),
      ),
    )
    _ -> #(state, Ok(JsNumber(NaN)))
  }
}

/// Shared getter: read [[DateValue]], decompose, return one field.
fn date_get_field(
  this: JsValue,
  state: State(host),
  name: String,
  field: DateField,
  time_ref: TimeRef,
) -> #(State(host), Result(JsValue, JsValue)) {
  use _, tv <- require_time_value(state, this, name)
  case tv {
    Finite(f) -> {
      let fields = get_date_fields(value.float_to_int(f), time_ref)
      #(state, Ok(value.from_int(field_at(fields, field))))
    }
    _ -> #(state, Ok(JsNumber(NaN)))
  }
}

// ============================================================================
// Prototype setters
// ============================================================================

/// ES2024 §21.4.4.27 Date.prototype.setTime ( time )
fn date_set_time(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
  name: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, _ <- require_time_value(state, this, name)
  let arg = helpers.first_arg_or_undefined(args)
  use n, st <- state.try_op(coerce.js_to_number(state, arg))
  let tv = time_clip(n)
  let st = set_this_time_value(st, ref, tv)
  #(st, Ok(JsNumber(tv)))
}

/// Shared setter. `first` is the first field being written (year .. ms),
/// `max_args` is how many consecutive fields from `first` may be supplied.
/// Ported from QuickJS `set_date_field`.
///
/// When `first` is `FieldYear` (setFullYear/setUTCFullYear) and the current
/// value is NaN, the spec uses +0 as the base time (§21.4.4.21 step 5). For
/// all other setters, NaN base → result stays NaN.
fn date_set_field(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
  name: String,
  first: DateField,
  max_args: Int,
  time_ref: TimeRef,
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, tv <- require_time_value(state, this, name)
  // Coerce supplied args (capped at max_args) to JsNum — full ToNumber so
  // valueOf side effects and abrupt completions are observed in order.
  let supplied = list.take(args, max_args)
  use new_nums, state <- state.try_op(args_to_nums(state, supplied))
  case compute_set_field(tv, first, new_nums, time_ref) {
    // Original [[DateValue]] was NaN (and this isn't setFullYear): per
    // spec step "If t is NaN, return NaN" — early return WITHOUT
    // writing back, so any side-effect setTime in valueOf is preserved.
    None -> #(state, Ok(JsNumber(NaN)))
    Some(result) -> {
      // Per spec, if no argument was supplied the result is NaN.
      let result = case args {
        [] -> NaN
        _ -> result
      }
      let state = set_this_time_value(state, ref, result)
      #(state, Ok(JsNumber(result)))
    }
  }
}

/// Compute new time value for a setter. Returns None for the "original t was
/// NaN" early-out (caller must NOT write back), Some(tv) otherwise.
fn compute_set_field(
  tv: JsNum,
  first: DateField,
  new_nums: List(JsNum),
  time_ref: TimeRef,
) -> Option(JsNum) {
  case tv {
    Finite(f) -> {
      let base = get_date_fields(value.float_to_int(f), time_ref)
      let merged = overwrite_fields(fields_to_components(base), first, new_nums)
      Some(make_date_from_components(merged, time_ref))
    }
    _ ->
      case first {
        // setFullYear on Invalid Date: per §21.4.4.21 step 5, t becomes +0
        // (NOT LocalTime(+0)) → Year 1970, Month 0, Date 1, all-zero time.
        FieldYear -> {
          let zero = Finite(0.0)
          let epoch =
            DateComponents(
              year: Finite(1970.0),
              month: zero,
              date: Finite(1.0),
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

fn int_num(i: Int) -> JsNum {
  Finite(int.to_float(i))
}

/// Replace `len(new_nums)` consecutive components starting at `first` with
/// the supplied values; every other component keeps its base value.
fn overwrite_fields(
  base: DateComponents,
  first: DateField,
  new_nums: List(JsNum),
) -> DateComponents {
  let lo = field_index(first)
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
    None -> NaN
    Some(#(y, mon, dt, h, mi, s, ms)) ->
      make_date(y, mon, dt, h, mi, s, ms, time_ref)
  }
}

// ============================================================================
// String formatting
// ============================================================================

type DateFmt {
  FmtLocal
  FmtUtc
  FmtIso
  FmtLocale
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
  this: JsValue,
  state: State(host),
  name: String,
  fmt: DateFmt,
  part: DatePart,
) -> #(State(host), Result(JsValue, JsValue)) {
  use _, tv <- require_time_value(state, this, name)
  case tv {
    Finite(f) -> {
      let time_ref = case fmt {
        FmtLocal | FmtLocale -> LocalTime
        FmtUtc | FmtIso -> UtcTime
      }
      let fields = get_date_fields(value.float_to_int(f), time_ref)
      let s = format_date(fmt, part, fields)
      #(state, Ok(JsString(s)))
    }
    _ ->
      case fmt {
        FmtIso -> state.range_error(state, "Invalid time value")
        _ -> #(state, Ok(JsString("Invalid Date")))
      }
  }
}

fn format_date(fmt: DateFmt, part: DatePart, f: DateFields) -> String {
  case fmt {
    FmtIso -> format_iso(f)
    FmtUtc -> format_utc(f)
    FmtLocal -> format_local(part, f)
    FmtLocale -> format_locale(part, f)
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
fn parse_date_string(s: String) -> JsNum {
  let s = string.trim(s)
  parse_iso(s) |> option.unwrap(NaN)
}

/// The parsed time-of-day of a date-time form. Its absence (`None` at the call
/// site) means the string was date-only — never "the time was there but some
/// required field of it wasn't", which is what the old all-optional tuple with
/// its always-`True` `has_time` flag could not distinguish.
type IsoTime {
  IsoTime(hours: Int, minutes: Int, seconds: Int, ms: Int)
}

fn parse_iso(s: String) -> Option(JsNum) {
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
        LocalZone -> make_date(year, mon - 1, day, h, mi, sec, ms, LocalTime)
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
  case n {
    Finite(f) -> time_clip(Finite(f +. int.to_float(minutes * 60_000)))
    other -> other
  }
}

/// Coerce a list of args to JsNum, threading state and propagating throws.
/// Used by the constructor multi-arg path, Date.UTC and the setters.
fn args_to_nums(
  state: State(host),
  args: List(JsValue),
) -> Result(#(List(JsNum), State(host)), #(JsValue, State(host))) {
  list.fold(args, Ok(#([], state)), fn(acc, arg) {
    use #(nums, st) <- result.try(acc)
    use #(n, st) <- result.map(coerce.js_to_number(st, arg))
    #([n, ..nums], st)
  })
  |> result.map(fn(p) { #(list.reverse(p.0), p.1) })
}

// ============================================================================
// Annex B: getYear / setYear
// ============================================================================

/// Annex B §B.2.3.1 Date.prototype.getYear ( ) — returns FullYear - 1900.
fn date_get_year(
  this: JsValue,
  state: State(host),
  name: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  use _, tv <- require_time_value(state, this, name)
  case tv {
    Finite(f) -> {
      let fields = get_date_fields(value.float_to_int(f), LocalTime)
      #(state, Ok(value.from_int(fields.year - 1900)))
    }
    _ -> #(state, Ok(JsNumber(NaN)))
  }
}

/// Annex B §B.2.3.2 Date.prototype.setYear ( year )
/// Year in [0,99] maps to 1900+year; otherwise sets the full year literally.
fn date_set_year(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
  name: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, tv <- require_time_value(state, this, name)
  let arg = helpers.first_arg_or_undefined(args)
  use n, st <- state.try_op(coerce.js_to_number(state, arg))
  case n {
    Finite(yf) -> {
      let yi = value.float_to_int(yf)
      let yi = case yi >= 0 && yi <= 99 {
        True -> yi + 1900
        False -> yi
      }
      // Base on local-time fields of current value; if NaN, t=+0 →
      // Month 0, Date 1, all-zero time (NOT LocalTime(+0)).
      let new_tv = case tv {
        Finite(f) -> {
          let b = get_date_fields(value.float_to_int(f), LocalTime)
          make_date(
            yi,
            b.month,
            b.date,
            b.hours,
            b.minutes,
            b.seconds,
            b.ms,
            LocalTime,
          )
        }
        _ -> make_date(yi, 0, 1, 0, 0, 0, 0, LocalTime)
      }
      let st = set_this_time_value(st, ref, new_tv)
      #(st, Ok(JsNumber(new_tv)))
    }
    _ -> {
      let st = set_this_time_value(st, ref, NaN)
      #(st, Ok(JsNumber(NaN)))
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
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) -> {
      let hint_arg = helpers.first_arg_or_undefined(args)
      case hint_arg {
        JsString("string") | JsString("default") ->
          run_ordinary_to_primitive(state, this, ref, coerce.StringHint)
        JsString("number") ->
          run_ordinary_to_primitive(state, this, ref, coerce.NumberHint)
        _ -> state.type_error(state, "Invalid hint")
      }
    }
    _ ->
      state.type_error(
        state,
        "Date.prototype[Symbol.toPrimitive] called on non-object",
      )
  }
}

fn run_ordinary_to_primitive(
  state: State(host),
  val: JsValue,
  ref: Ref,
  hint: coerce.ToPrimitiveHint,
) -> #(State(host), Result(JsValue, JsValue)) {
  use v, st <- state.try_op(coerce.ordinary_to_primitive(state, val, ref, hint))
  #(st, Ok(v))
}

/// ES2024 §21.4.4.37 Date.prototype.toJSON ( key )
///
///   1. Let O be ? ToObject(this value).
///   2. Let tv be ? ToPrimitive(O, number).
///   3. If tv is a Number and tv is not finite, return null.
///   4. Return ? Invoke(O, "toISOString").
fn date_to_json(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 1: ToObject(this value).
  case common.to_object(state.heap, state.builtins, this) {
    None ->
      state.type_error(
        state,
        "Date.prototype.toJSON called on null or undefined",
      )
    Some(#(heap, ref)) -> {
      let state = State(..state, heap:)
      let obj = JsObject(ref)
      // Step 2: ToPrimitive(O, number).
      let prim_r = coerce.to_primitive(state, obj, coerce.NumberHint)
      use prim, st <- state.try_op(prim_r)
      case prim {
        // Step 3: non-finite Number → return null.
        JsNumber(NaN)
        | JsNumber(value.Infinity)
        | JsNumber(value.NegInfinity) -> #(st, Ok(JsNull))
        // Step 4: Invoke(O, "toISOString").
        _ -> invoke_to_iso_string(st, obj, ref)
      }
    }
  }
}

/// Generic Invoke(O, "toISOString") — looks up via prototype chain and calls.
fn invoke_to_iso_string(
  state: State(host),
  obj: JsValue,
  ref: Ref,
) -> #(State(host), Result(JsValue, JsValue)) {
  let lookup = ops_object.get_value(state, ref, Named("toISOString"), obj)
  use method, st <- state.try_op(lookup)
  case helpers.is_callable(st.heap, method) {
    True -> {
      use v, st <- state.try_call(st, method, obj, [])
      #(st, Ok(v))
    }
    False -> state.type_error(st, "toISOString is not a function")
  }
}
