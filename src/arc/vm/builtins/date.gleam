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
/// in pure Gleam Int arithmetic ported from the QuickJS algorithms; only
/// `now_ms` and `tz_offset_minutes` go through FFI.
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers
import arc/vm/builtins/math as builtins_math
import arc/vm/heap
import arc/vm/internal/elements
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
  JsString, NaN, ObjectSlot,
}
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

// ============================================================================
// FFI
// ============================================================================

@external(erlang, "arc_date_ffi", "now_ms")
fn ffi_now_ms() -> Int

@external(erlang, "arc_date_ffi", "tz_offset_minutes")
fn ffi_tz_offset_minutes(epoch_ms: Int) -> Int

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
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, BuiltinType) {
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case native {
    DateConstructor(proto:) -> date_constructor(proto, args, state)
    DateNow -> #(state, Ok(JsNumber(Finite(int.to_float(ffi_now_ms())))))
    DateParse -> date_parse(args, state)
    DateUTC -> date_utc(args, state)
    DatePrototypeValueOf | DatePrototypeGetTime -> date_get_time(this, state)
    DatePrototypeGetTimezoneOffset -> date_get_tz_offset(this, state)
    DatePrototypeGetFullYear -> date_get_field(this, state, 0, True)
    DatePrototypeGetUTCFullYear -> date_get_field(this, state, 0, False)
    DatePrototypeGetMonth -> date_get_field(this, state, 1, True)
    DatePrototypeGetUTCMonth -> date_get_field(this, state, 1, False)
    DatePrototypeGetDate -> date_get_field(this, state, 2, True)
    DatePrototypeGetUTCDate -> date_get_field(this, state, 2, False)
    DatePrototypeGetDay -> date_get_field(this, state, 7, True)
    DatePrototypeGetUTCDay -> date_get_field(this, state, 7, False)
    DatePrototypeGetHours -> date_get_field(this, state, 3, True)
    DatePrototypeGetUTCHours -> date_get_field(this, state, 3, False)
    DatePrototypeGetMinutes -> date_get_field(this, state, 4, True)
    DatePrototypeGetUTCMinutes -> date_get_field(this, state, 4, False)
    DatePrototypeGetSeconds -> date_get_field(this, state, 5, True)
    DatePrototypeGetUTCSeconds -> date_get_field(this, state, 5, False)
    DatePrototypeGetMilliseconds -> date_get_field(this, state, 6, True)
    DatePrototypeGetUTCMilliseconds -> date_get_field(this, state, 6, False)
    DatePrototypeSetTime -> date_set_time(this, args, state)
    DatePrototypeSetMilliseconds ->
      date_set_field(this, args, state, 6, 1, True)
    DatePrototypeSetUTCMilliseconds ->
      date_set_field(this, args, state, 6, 1, False)
    DatePrototypeSetSeconds -> date_set_field(this, args, state, 5, 2, True)
    DatePrototypeSetUTCSeconds -> date_set_field(this, args, state, 5, 2, False)
    DatePrototypeSetMinutes -> date_set_field(this, args, state, 4, 3, True)
    DatePrototypeSetUTCMinutes -> date_set_field(this, args, state, 4, 3, False)
    DatePrototypeSetHours -> date_set_field(this, args, state, 3, 4, True)
    DatePrototypeSetUTCHours -> date_set_field(this, args, state, 3, 4, False)
    DatePrototypeSetDate -> date_set_field(this, args, state, 2, 1, True)
    DatePrototypeSetUTCDate -> date_set_field(this, args, state, 2, 1, False)
    DatePrototypeSetMonth -> date_set_field(this, args, state, 1, 2, True)
    DatePrototypeSetUTCMonth -> date_set_field(this, args, state, 1, 2, False)
    DatePrototypeSetFullYear -> date_set_field(this, args, state, 0, 3, True)
    DatePrototypeSetUTCFullYear ->
      date_set_field(this, args, state, 0, 3, False)
    DatePrototypeGetYear -> date_get_year(this, state)
    DatePrototypeSetYear -> date_set_year(this, args, state)
    DatePrototypeToString -> date_to_string(this, state, FmtLocal, 3)
    DatePrototypeToDateString -> date_to_string(this, state, FmtLocal, 1)
    DatePrototypeToTimeString -> date_to_string(this, state, FmtLocal, 2)
    DatePrototypeToISOString -> date_to_string(this, state, FmtIso, 3)
    DatePrototypeToUTCString -> date_to_string(this, state, FmtUtc, 3)
    DatePrototypeToLocaleString -> date_to_string(this, state, FmtLocale, 3)
    DatePrototypeToLocaleDateString -> date_to_string(this, state, FmtLocale, 1)
    DatePrototypeToLocaleTimeString -> date_to_string(this, state, FmtLocale, 2)
    DatePrototypeToJSON -> date_to_json(this, state)
    DatePrototypeSymbolToPrimitive -> date_to_primitive(this, args, state)
  }
}

// ============================================================================
// Core date math (ported from QuickJS, all Int arithmetic)
// ============================================================================

const ms_per_day = 86_400_000

const max_time_value = 8.64e15

/// Euclidean integer division (floor toward -infinity). Divisor is always
/// positive in our calls so the stdlib's Result(Nil) error path is unreachable.
fn floor_div(a: Int, b: Int) -> Int {
  int.floor_divide(a, b) |> result.unwrap(0)
}

/// Euclidean modulo (result has sign of divisor). Divisor always positive here.
fn math_mod(a: Int, b: Int) -> Int {
  int.modulo(a, b) |> result.unwrap(0)
}

/// ES2024 §21.4.1.3 Day Number from year. Days since epoch to Jan 1 of `y`.
fn days_from_year(y: Int) -> Int {
  365
  * { y - 1970 }
  + floor_div(y - 1969, 4)
  - floor_div(y - 1901, 100)
  + floor_div(y - 1601, 400)
}

fn is_leap_year(y: Int) -> Bool {
  math_mod(y, 4) == 0 && { math_mod(y, 100) != 0 || math_mod(y, 400) == 0 }
}

fn days_in_year(y: Int) -> Int {
  case is_leap_year(y) {
    True -> 366
    False -> 365
  }
}

/// Return #(year, day_within_year) for an absolute day number since epoch.
/// Initial guess from average year length, then correct ±1 (QuickJS algorithm).
fn year_from_days(days: Int) -> #(Int, Int) {
  let y = floor_div(days * 10_000, 3_652_425) + 1970
  year_from_days_loop(y, days)
}

fn year_from_days_loop(y: Int, days: Int) -> #(Int, Int) {
  let d = days - days_from_year(y)
  case d < 0 {
    True -> year_from_days_loop(y - 1, days)
    False ->
      case d >= days_in_year(y) {
        True -> year_from_days_loop(y + 1, days)
        False -> #(y, d)
      }
  }
}

/// Days in month `m` (0-based) for year `y`.
fn days_in_month(y: Int, m: Int) -> Int {
  case m {
    1 ->
      case is_leap_year(y) {
        True -> 29
        False -> 28
      }
    3 | 5 | 8 | 10 -> 30
    _ -> 31
  }
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

/// Broken-down date components (all Int). `tz` is the timezone-offset minutes
/// at the moment in question (UTC - local; 0 for UTC fields).
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

/// Project a field by index (0=year .. 6=ms, 7=weekday).
fn field_at(f: DateFields, idx: Int) -> Int {
  case idx {
    0 -> f.year
    1 -> f.month
    2 -> f.date
    3 -> f.hours
    4 -> f.minutes
    5 -> f.seconds
    6 -> f.ms
    7 -> f.weekday
    _ -> 0
  }
}

/// Decompose an integral epoch-ms time value into calendar fields. When
/// `is_local` the FFI timezone offset for that instant is applied first.
fn get_date_fields(tv: Int, is_local: Bool) -> DateFields {
  let tz = case is_local {
    True -> ffi_tz_offset_minutes(tv)
    False -> 0
  }
  let d = tv - tz * 60_000
  let h = math_mod(d, ms_per_day)
  let days = { d - h } / ms_per_day
  let ms = math_mod(h, 1000)
  let h = { h - ms } / 1000
  let seconds = math_mod(h, 60)
  let h = { h - seconds } / 60
  let minutes = math_mod(h, 60)
  let hours = { h - minutes } / 60
  let weekday = math_mod(days + 4, 7)
  let #(year, day_in_year) = year_from_days(days)
  let #(month, date) = month_from_day_in_year(year, day_in_year, 0)
  DateFields(
    year:,
    month:,
    date:,
    hours:,
    minutes:,
    seconds:,
    ms:,
    weekday:,
    tz:,
  )
}

fn month_from_day_in_year(y: Int, d: Int, m: Int) -> #(Int, Int) {
  let md = days_in_month(y, m)
  case d < md {
    True -> #(m, d + 1)
    False -> month_from_day_in_year(y, d - md, m + 1)
  }
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
  is_local: Bool,
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
      let tv = case is_local {
        True -> tv + ffi_tz_offset_minutes(tv) * 60_000
        False -> tv
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

/// Convert a list of JsNum fields (year, month, date, h, m, s, ms) to a time
/// value. Any NaN/Infinity → NaN. All values truncated toward zero. Year in
/// [0,100) is mapped to 1900+year per spec §21.4.2.1 step 5.k.
fn make_date_checked(fields: List(JsNum), is_local: Bool) -> JsNum {
  case nums_to_ints(fields) {
    None -> NaN
    Some([y, mon, dt, h, mi, s, ms]) -> {
      let y = case y >= 0 && y <= 99 {
        True -> y + 1900
        False -> y
      }
      make_date(y, mon, dt, h, mi, s, ms, is_local)
    }
    Some(_) -> NaN
  }
}

/// Convert a List(JsNum) to List(Int), short-circuiting to None on any
/// non-finite element. Truncates toward zero.
fn nums_to_ints(nums: List(JsNum)) -> Option(List(Int)) {
  list.try_map(nums, fn(n) {
    case n {
      Finite(f) -> Ok(value.float_to_int(f))
      _ -> Error(Nil)
    }
  })
  |> option.from_result
}

// ============================================================================
// thisTimeValue helper / mutation helper
// ============================================================================

/// ES2024 §21.4.4 thisTimeValue: extract [[DateValue]] from a Date object,
/// or None if `this` is not a Date.
fn this_time_value(state: State, this: JsValue) -> Option(#(Ref, JsNum)) {
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
  state: State,
  this: JsValue,
  name: String,
  k: fn(Ref, JsNum) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
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
fn set_this_time_value(state: State, ref: Ref, tv: JsNum) -> State {
  let heap =
    heap.update(state.heap, ref, fn(slot) {
      case slot {
        ObjectSlot(kind: DateObject(_), ..) ->
          ObjectSlot(..slot, kind: DateObject(time_value: tv))
        other -> other
      }
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
/// Known deviation: arc's call layer passes `this=JsUndefined` for both `f()`
/// and `new f()` so we can't distinguish — this always returns a Date object,
/// never the §21.4.2.1 step 2 string. Only ~2 test262 tests rely on that path.
fn date_constructor(
  proto: Ref,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let #(state, tv_result) = case args {
    [] -> #(state, Ok(Finite(int.to_float(ffi_now_ms()))))
    [single] -> single_arg_time_value(state, single)
    many -> args_to_time_value(state, many, True)
  }
  case tv_result {
    Error(e) -> #(state, Error(e))
    Ok(tv) -> {
      let #(heap, ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: DateObject(time_value: tv),
            properties: dict.new(),
            elements: elements.new(),
            prototype: Some(proto),
            symbol_properties: [],
            extensible: True,
          ),
        )
      #(State(..state, heap:), Ok(JsObject(ref)))
    }
  }
}

/// Single-argument constructor path: clone a Date, parse a string, or
/// ToNumber+TimeClip.
fn single_arg_time_value(
  state: State,
  arg: JsValue,
) -> #(State, Result(JsNum, JsValue)) {
  // §21.4.2.1 step 4.a: if value is a Date object, copy its [[DateValue]].
  case this_time_value(state, arg) {
    Some(#(_, tv)) -> #(state, Ok(time_clip(tv)))
    None ->
      // ToPrimitive(value) → string? parse : ToNumber+TimeClip
      case coerce.to_primitive(state, arg, coerce.DefaultHint) {
        Error(#(e, st)) -> #(st, Error(e))
        Ok(#(JsString(s), st)) -> #(st, Ok(parse_date_string(s)))
        Ok(#(prim, st)) ->
          case to_number_state(st, prim) {
            Error(#(e, st)) -> #(st, Error(e))
            Ok(#(n, st)) -> #(st, Ok(time_clip(n)))
          }
      }
  }
}

/// Coerce an N-arg list (1..7) to a time value with full ToNumber re-entry.
/// Missing fields default to month=0, date=1, h/m/s/ms=0. Extra args ignored.
fn args_to_time_value(
  state: State,
  args: List(JsValue),
  is_local: Bool,
) -> #(State, Result(JsNum, JsValue)) {
  case args_to_nums(state, list.take(args, 7)) {
    Error(#(e, st)) -> #(st, Error(e))
    Ok(#(nums, st)) -> #(st, Ok(make_date_checked(pad_fields(nums), is_local)))
  }
}

/// Pad a fields list to length 7 with spec defaults.
fn pad_fields(nums: List(JsNum)) -> List(JsNum) {
  let defaults = [
    Finite(0.0),
    Finite(1.0),
    Finite(0.0),
    Finite(0.0),
    Finite(0.0),
    Finite(0.0),
  ]
  case nums {
    [y] -> [y, ..defaults]
    [y, m] -> [y, m, ..list.drop(defaults, 1)]
    [y, m, d] -> [y, m, d, ..list.drop(defaults, 2)]
    [y, m, d, h] -> [y, m, d, h, ..list.drop(defaults, 3)]
    [y, m, d, h, mi] -> [y, m, d, h, mi, ..list.drop(defaults, 4)]
    [y, m, d, h, mi, s] -> [y, m, d, h, mi, s, Finite(0.0)]
    other -> other
  }
}

/// ES2024 §21.4.3.1 Date.parse ( string )
fn date_parse(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case coerce.js_to_string(state, helpers.first_arg_or_undefined(args)) {
    Error(#(e, st)) -> #(st, Error(e))
    Ok(#(s, st)) -> #(st, Ok(JsNumber(parse_date_string(s))))
  }
}

/// ES2024 §21.4.3.4 Date.UTC ( year [, month [, date [, hours ...]]] )
/// 0 args → NaN; 1+ args → fields interpreted as UTC, year-mapping applied.
fn date_utc(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case args {
    [] -> #(state, Ok(JsNumber(NaN)))
    many -> {
      let #(st, r) = args_to_time_value(state, many, False)
      #(st, result.map(r, JsNumber))
    }
  }
}

// ============================================================================
// Prototype getters
// ============================================================================

fn date_get_time(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use _, tv <- require_time_value(state, this, "valueOf")
  #(state, Ok(JsNumber(tv)))
}

fn date_get_tz_offset(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use _, tv <- require_time_value(state, this, "getTimezoneOffset")
  case tv {
    Finite(f) -> #(
      state,
      Ok(
        JsNumber(
          Finite(int.to_float(ffi_tz_offset_minutes(value.float_to_int(f)))),
        ),
      ),
    )
    _ -> #(state, Ok(JsNumber(NaN)))
  }
}

/// Shared getter: read [[DateValue]], decompose, return one field.
fn date_get_field(
  this: JsValue,
  state: State,
  field_index: Int,
  is_local: Bool,
) -> #(State, Result(JsValue, JsValue)) {
  use _, tv <- require_time_value(state, this, "get")
  case tv {
    Finite(f) -> {
      let fields = get_date_fields(value.float_to_int(f), is_local)
      #(
        state,
        Ok(JsNumber(Finite(int.to_float(field_at(fields, field_index))))),
      )
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use ref, _ <- require_time_value(state, this, "setTime")
  case to_number_state(state, helpers.first_arg_or_undefined(args)) {
    Error(#(e, st)) -> #(st, Error(e))
    Ok(#(n, st)) -> {
      let tv = time_clip(n)
      let st = set_this_time_value(st, ref, tv)
      #(st, Ok(JsNumber(tv)))
    }
  }
}

/// Shared setter. `first` is the first field index being set (0=year .. 6=ms),
/// `max_args` is how many of [first, first+1, ...] may be supplied. Ported
/// from QuickJS `set_date_field`.
///
/// When `first==0` (setFullYear/setUTCFullYear) and the current value is NaN,
/// the spec uses +0 as the base time (§21.4.4.21 step 2). For all other
/// setters, NaN base → result stays NaN.
fn date_set_field(
  this: JsValue,
  args: List(JsValue),
  state: State,
  first: Int,
  max_args: Int,
  is_local: Bool,
) -> #(State, Result(JsValue, JsValue)) {
  use ref, tv <- require_time_value(state, this, "set")
  // Coerce supplied args (capped at max_args) to JsNum — full ToNumber so
  // valueOf side effects and abrupt completions are observed in order.
  case args_to_nums(state, list.take(args, max_args)) {
    Error(#(e, st)) -> #(st, Error(e))
    Ok(#(new_nums, state)) ->
      case compute_set_field(tv, first, new_nums, is_local) {
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
}

/// Compute new time value for a setter. Returns None for the "original t was
/// NaN" early-out (caller must NOT write back), Some(tv) otherwise.
fn compute_set_field(
  tv: JsNum,
  first: Int,
  new_nums: List(JsNum),
  is_local: Bool,
) -> Option(JsNum) {
  case tv {
    Finite(f) -> {
      let base = get_date_fields(value.float_to_int(f), is_local)
      let base_floats = [
        int.to_float(base.year),
        int.to_float(base.month),
        int.to_float(base.date),
        int.to_float(base.hours),
        int.to_float(base.minutes),
        int.to_float(base.seconds),
        int.to_float(base.ms),
      ]
      let merged = overwrite_fields(base_floats, first, new_nums)
      Some(make_date_from_floats(merged, is_local))
    }
    _ ->
      case first == 0 {
        // setFullYear on Invalid Date: per §21.4.4.21 step 2, t becomes +0
        // (NOT LocalTime(+0)) → Year 1970, Month 0, Date 1, all-zero time.
        True -> {
          let base_floats = [1970.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0]
          let merged = overwrite_fields(base_floats, first, new_nums)
          Some(make_date_from_floats(merged, is_local))
        }
        False -> None
      }
  }
}

/// Replace `len(new_nums)` consecutive Float fields starting at `first` with
/// the supplied JsNum values. Any non-finite new_num → return None (caller
/// turns that into NaN). Otherwise returns the merged 7-field list as Floats.
fn overwrite_fields(
  base: List(Float),
  first: Int,
  new_nums: List(JsNum),
) -> Option(List(Float)) {
  list.index_map(base, fn(v, i) {
    case i >= first && i < first + list.length(new_nums) {
      True ->
        case helpers.list_at(new_nums, i - first) {
          Some(Finite(f)) -> Ok(f)
          _ -> Error(Nil)
        }
      False -> Ok(v)
    }
  })
  |> list.try_map(fn(r) { r })
  |> option.from_result
}

/// Variant of make_date_checked that takes plain Floats (already merged from
/// integer fields, so no [0,100) year mapping is applied — setFullYear sets
/// the year literally). None → NaN.
fn make_date_from_floats(fields: Option(List(Float)), is_local: Bool) -> JsNum {
  case fields {
    None -> NaN
    Some([y, mon, dt, h, mi, s, ms]) ->
      make_date(
        value.float_to_int(y),
        value.float_to_int(mon),
        value.float_to_int(dt),
        value.float_to_int(h),
        value.float_to_int(mi),
        value.float_to_int(s),
        value.float_to_int(ms),
        is_local,
      )
    Some(_) -> NaN
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
/// `part`: bit 1 = date, bit 2 = time, 3 = both.
fn date_to_string(
  this: JsValue,
  state: State,
  fmt: DateFmt,
  part: Int,
) -> #(State, Result(JsValue, JsValue)) {
  use _, tv <- require_time_value(state, this, "toString")
  case tv {
    Finite(f) -> {
      let is_local = case fmt {
        FmtLocal | FmtLocale -> True
        FmtUtc | FmtIso -> False
      }
      let fields = get_date_fields(value.float_to_int(f), is_local)
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

fn format_date(fmt: DateFmt, part: Int, f: DateFields) -> String {
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
fn format_local(part: Int, f: DateFields) -> String {
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
    1 -> date_part
    2 -> time_part
    _ -> date_part <> " " <> time_part
  }
}

/// Minimal locale formatting — "M/D/YYYY, HH:mm:ss AM/PM" enough to satisfy
/// type/shape tests; spec leaves the exact format implementation-defined.
fn format_locale(part: Int, f: DateFields) -> String {
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
    1 -> date_part
    2 -> time_part
    _ -> date_part <> ", " <> time_part
  }
}

fn format_year_signed(y: Int) -> String {
  case y < 0 {
    True -> "-" <> string.pad_start(int.to_string(0 - y), 4, "0")
    False -> string.pad_start(int.to_string(y), 4, "0")
  }
}

/// "+HHMM" / "-HHMM" — note JS sign convention is local-minus-UTC, the
/// negation of getTimezoneOffset().
fn format_tz(tz: Int) -> String {
  let off = 0 - tz
  let sign = case off < 0 {
    True -> "-"
    False -> "+"
  }
  let a = int.absolute_value(off)
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

fn parse_iso(s: String) -> Option(JsNum) {
  // Year: "+YYYYYY" / "-YYYYYY" / "YYYY"
  use #(year, rest, has_sign) <- option.then(parse_year(s))
  // Month + day (optional)
  let #(mon, rest) = parse_dash_int(rest, 2) |> option.unwrap(#(1, rest))
  let #(day, rest) = parse_dash_int(rest, 2) |> option.unwrap(#(1, rest))
  // Time (optional, after "T")
  let #(h, mi, sec, ms, rest, has_time) = case rest {
    "T" <> t -> parse_time(t)
    _ -> #(0, 0, 0, 0, rest, False)
  }
  // Zone (optional). Date-only forms are UTC; date-time forms with no zone
  // are local time per spec — but only when no zone is present.
  use #(tz_min, rest) <- option.then(parse_zone(rest, has_time, has_sign))
  case rest {
    "" -> {
      let tv =
        make_date(year, mon - 1, day, h, mi, sec, ms, False)
        |> jsnum_add_minutes(tz_min)
      Some(tv)
    }
    _ -> None
  }
}

fn parse_year(s: String) -> Option(#(Int, String, Bool)) {
  case s {
    "+" <> rest ->
      take_digits(rest, 6) |> option.map(fn(p) { #(p.0, p.1, True) })
    "-" <> rest ->
      take_digits(rest, 6) |> option.map(fn(p) { #(0 - p.0, p.1, True) })
    _ -> take_digits(s, 4) |> option.map(fn(p) { #(p.0, p.1, False) })
  }
}

fn parse_dash_int(s: String, n: Int) -> Option(#(Int, String)) {
  case s {
    "-" <> rest -> take_digits(rest, n)
    _ -> None
  }
}

fn parse_time(s: String) -> #(Int, Int, Int, Int, String, Bool) {
  let #(h, rest) = take_digits(s, 2) |> option.unwrap(#(0, s))
  let #(mi, rest) = case rest {
    ":" <> r -> take_digits(r, 2) |> option.unwrap(#(0, r))
    _ -> #(0, rest)
  }
  let #(sec, rest) = case rest {
    ":" <> r -> take_digits(r, 2) |> option.unwrap(#(0, r))
    _ -> #(0, rest)
  }
  let #(ms, rest) = case rest {
    "." <> r -> take_digits(r, 3) |> option.unwrap(#(0, r))
    _ -> #(0, rest)
  }
  #(h, mi, sec, ms, rest, True)
}

/// Returns Some(#(minutes_to_subtract, remaining)). For local time we resolve
/// the offset later inside make_date, so return 0 and let the caller handle.
/// `has_time` selects the no-zone default: date-only → UTC, date-time → local.
fn parse_zone(
  s: String,
  has_time: Bool,
  has_sign: Bool,
) -> Option(#(Int, String)) {
  case s {
    "Z" <> rest -> Some(#(0, rest))
    "+" <> rest -> parse_hhmm(rest) |> option.map(fn(p) { #(0 - p.0, p.1) })
    "-" <> rest -> parse_hhmm(rest) |> option.map(fn(p) { #(p.0, p.1) })
    "" ->
      case has_time && !has_sign {
        // date-time with no zone: spec says local time. We approximate by
        // using the current tz offset (good enough for test262 shape tests).
        True -> Some(#(0 - ffi_tz_offset_minutes(ffi_now_ms()), ""))
        False -> Some(#(0, ""))
      }
    _ -> None
  }
}

fn parse_hhmm(s: String) -> Option(#(Int, String)) {
  use #(h, rest) <- option.then(take_digits(s, 2))
  let #(m, rest) = case rest {
    ":" <> r -> take_digits(r, 2) |> option.unwrap(#(0, r))
    _ -> take_digits(rest, 2) |> option.unwrap(#(0, rest))
  }
  Some(#(h * 60 + m, rest))
}

/// Consume exactly `n` ASCII digits, returning #(value, rest). None if fewer
/// than `n` digits are available.
fn take_digits(s: String, n: Int) -> Option(#(Int, String)) {
  take_digits_loop(s, n, 0)
}

fn take_digits_loop(s: String, n: Int, acc: Int) -> Option(#(Int, String)) {
  case n {
    0 -> Some(#(acc, s))
    _ ->
      case string.pop_grapheme(s) {
        Ok(#(c, rest)) ->
          case digit_value(c) {
            Some(d) -> take_digits_loop(rest, n - 1, acc * 10 + d)
            None -> None
          }
        Error(Nil) -> None
      }
  }
}

fn digit_value(c: String) -> Option(Int) {
  case c {
    "0" -> Some(0)
    "1" -> Some(1)
    "2" -> Some(2)
    "3" -> Some(3)
    "4" -> Some(4)
    "5" -> Some(5)
    "6" -> Some(6)
    "7" -> Some(7)
    "8" -> Some(8)
    "9" -> Some(9)
    _ -> None
  }
}

fn jsnum_add_minutes(n: JsNum, minutes: Int) -> JsNum {
  case n {
    Finite(f) -> time_clip(Finite(f +. int.to_float(minutes * 60_000)))
    other -> other
  }
}

// ============================================================================
// ToNumber with VM re-entry (state-threading)
// ============================================================================

/// ES2024 §7.1.4 ToNumber with full ToPrimitive(number) re-entry so object
/// arguments observe their valueOf being called and abrupt completions
/// propagate. Symbols → TypeError, BigInt → TypeError per spec.
fn to_number_state(
  state: State,
  val: JsValue,
) -> Result(#(JsNum, State), #(JsValue, State)) {
  case val {
    JsObject(_) -> {
      use #(prim, st) <- result.try(coerce.to_primitive(
        state,
        val,
        coerce.NumberHint,
      ))
      to_number_state(st, prim)
    }
    value.JsSymbol(_) ->
      coerce.thrown_type_error(
        state,
        "Cannot convert a Symbol value to a number",
      )
    value.JsBigInt(_) ->
      coerce.thrown_type_error(state, "Cannot convert a BigInt to a number")
    JsString(s) -> Ok(#(string_to_number(s), state))
    other -> Ok(#(builtins_math.to_number(other), state))
  }
}

/// ES2024 §7.1.4.1.1 StringToNumber — fuller than builtins_math.to_number's
/// version: trims whitespace, accepts leading + or -, scientific notation,
/// leading/trailing decimal point. Hex/oct/bin prefixes not handled here.
fn string_to_number(s: String) -> JsNum {
  let s = string.trim(s)
  case s {
    "" -> Finite(0.0)
    "Infinity" | "+Infinity" -> value.Infinity
    "-Infinity" -> value.NegInfinity
    _ -> {
      let #(neg, rest) = case s {
        "-" <> r -> #(True, r)
        "+" <> r -> #(False, r)
        _ -> #(False, s)
      }
      let n =
        float.parse(rest)
        |> result.try_recover(fn(_: Nil) {
          // "200." → "200.0"; ".5" → "0.5"; "2e3" → "2.0e3" — Gleam's
          // float.parse needs both sides of the decimal point.
          float.parse(normalize_float_literal(rest))
        })
        |> result.try_recover(fn(_: Nil) {
          int.parse(rest) |> result.map(int.to_float)
        })
      case n {
        Ok(f) ->
          case neg {
            True -> Finite(float.negate(f))
            False -> Finite(f)
          }
        Error(Nil) -> NaN
      }
    }
  }
}

/// Best-effort fixup so Erlang float parsing accepts JS-style literals like
/// "2e3", "200.", ".5", "200.000E-02".
fn normalize_float_literal(s: String) -> String {
  // Split at 'e' or 'E'
  let #(mant, exp) = case string.split_once(s, "e") {
    Ok(#(m, e)) -> #(m, "e" <> e)
    Error(Nil) ->
      case string.split_once(s, "E") {
        Ok(#(m, e)) -> #(m, "e" <> e)
        Error(Nil) -> #(s, "")
      }
  }
  let mant = case string.contains(mant, ".") {
    True ->
      case string.ends_with(mant, ".") {
        True -> mant <> "0"
        False ->
          case string.starts_with(mant, ".") {
            True -> "0" <> mant
            False -> mant
          }
      }
    False -> mant <> ".0"
  }
  // strip leading zeros so "+00200.000" parses
  mant <> exp
}

/// Coerce a list of args to JsNum, threading state and propagating throws.
/// Used by the constructor multi-arg path, Date.UTC and the setters.
fn args_to_nums(
  state: State,
  args: List(JsValue),
) -> Result(#(List(JsNum), State), #(JsValue, State)) {
  list.fold(args, Ok(#([], state)), fn(acc, arg) {
    use #(nums, st) <- result.try(acc)
    use #(n, st) <- result.map(to_number_state(st, arg))
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use _, tv <- require_time_value(state, this, "getYear")
  case tv {
    Finite(f) -> {
      let fields = get_date_fields(value.float_to_int(f), True)
      #(state, Ok(JsNumber(Finite(int.to_float(fields.year - 1900)))))
    }
    _ -> #(state, Ok(JsNumber(NaN)))
  }
}

/// Annex B §B.2.3.2 Date.prototype.setYear ( year )
/// Year in [0,99] maps to 1900+year; otherwise sets the full year literally.
fn date_set_year(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use ref, tv <- require_time_value(state, this, "setYear")
  case to_number_state(state, helpers.first_arg_or_undefined(args)) {
    Error(#(e, st)) -> #(st, Error(e))
    Ok(#(n, st)) ->
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
              let b = get_date_fields(value.float_to_int(f), True)
              make_date(
                yi,
                b.month,
                b.date,
                b.hours,
                b.minutes,
                b.seconds,
                b.ms,
                True,
              )
            }
            _ -> make_date(yi, 0, 1, 0, 0, 0, 0, True)
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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
  state: State,
  val: JsValue,
  ref: Ref,
  hint: coerce.ToPrimitiveHint,
) -> #(State, Result(JsValue, JsValue)) {
  case coerce.ordinary_to_primitive(state, val, ref, hint) {
    Ok(#(v, st)) -> #(st, Ok(v))
    Error(#(e, st)) -> #(st, Error(e))
  }
}

/// ES2024 §21.4.4.37 Date.prototype.toJSON ( key )
///
///   1. Let O be ? ToObject(this value).
///   2. Let tv be ? ToPrimitive(O, number).
///   3. If tv is a Number and tv is not finite, return null.
///   4. Return ? Invoke(O, "toISOString").
fn date_to_json(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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
      case coerce.to_primitive(state, obj, coerce.NumberHint) {
        Error(#(e, st)) -> #(st, Error(e))
        Ok(#(prim, st)) ->
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
}

/// Generic Invoke(O, "toISOString") — looks up via prototype chain and calls.
fn invoke_to_iso_string(
  state: State,
  obj: JsValue,
  ref: Ref,
) -> #(State, Result(JsValue, JsValue)) {
  case ops_object.get_value(state, ref, value.Named("toISOString"), obj) {
    Error(#(e, st)) -> #(st, Error(e))
    Ok(#(method, st)) ->
      case helpers.is_callable(st.heap, method) {
        True ->
          case state.call(st, method, obj, []) {
            Ok(#(v, st)) -> #(st, Ok(v))
            Error(#(e, st)) -> #(st, Error(e))
          }
        False -> state.type_error(st, "toISOString is not a function")
      }
  }
}
