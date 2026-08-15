//// ES2024 §21.4 Date Objects
////
//// A Date object encapsulates a single time value: an integral Number of
//// milliseconds since 1970-01-01T00:00:00Z (the epoch), or NaN for an invalid
//// date. Internal storage: `DateObj(ms: JsNum)` exotic kind. §21.4.4: the
//// prototype is an ORDINARY object (NOT a Date instance — no [[DateValue]]).
//// Port of arc `builtins/date.gleam:79-173` init/dispatch re-expressed under
//// D7/R1. Full field-breakdown/format arithmetic ports with the §10 tz FFI
//// (`arc_rt_tz_ffi.erl`); until then getters/setters route through the
//// UTC-only pure-Int helpers below.

import arc/rt/builtins/common
import arc/rt/builtins/helpers
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
  DatePrototypeValueOf, DateUTC, HintNumber, JInt, JNan, KHandle, KStr, Named,
  NoElements, SObject, StringKey, classify, mk_number, mk_object, mk_string,
} as rt_types
import arc/rt/val as rt_val
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

// ═══════════════════════════════════════════════════════════════════════════
// Init — Date constructor + Date.prototype
// ═══════════════════════════════════════════════════════════════════════════

/// Set up Date constructor + Date.prototype. §21.4.2: Date.length is 7.
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
  // §21.4.4.45 Date.prototype[@@toPrimitive] — {W:F, E:F, C:T}.
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

// ═══════════════════════════════════════════════════════════════════════════
// Dispatch
// ═══════════════════════════════════════════════════════════════════════════

/// Per-module [[Call]] dispatch for Date native functions.
pub fn dispatch(
  st: Agent,
  native: DateNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let name = method_name(native)
  case native {
    // §21.4.2.1 step 3.a: called as a function → ToDateString(now).
    DateConstructor(..) -> #(mk_string(to_utc_string(now_ms(st))), st)
    DateNow -> #(mk_number(JInt(now_ms(st))), st)
    DateParse -> date_parse(st, args)
    DateUTC -> date_utc(st, args)
    DatePrototypeValueOf | DatePrototypeGetTime -> get_time(st, this, name)
    DatePrototypeGetTimezoneOffset ->
      // §21.4.4.22 step 4: NaN date → NaN. No local-tz FFI yet — UTC offset 0.
      case require_date(st, this, name) {
        JInt(_) -> #(mk_number(JInt(0)), st)
        _ -> #(mk_number(JNan), st)
      }
    DatePrototypeGetFullYear | DatePrototypeGetUTCFullYear ->
      get_field(st, this, name, FYear)
    DatePrototypeGetMonth | DatePrototypeGetUTCMonth ->
      get_field(st, this, name, FMonth)
    DatePrototypeGetDate | DatePrototypeGetUTCDate ->
      get_field(st, this, name, FDate)
    DatePrototypeGetDay | DatePrototypeGetUTCDay ->
      get_field(st, this, name, FDay)
    DatePrototypeGetHours | DatePrototypeGetUTCHours ->
      get_field(st, this, name, FHours)
    DatePrototypeGetMinutes | DatePrototypeGetUTCMinutes ->
      get_field(st, this, name, FMinutes)
    DatePrototypeGetSeconds | DatePrototypeGetUTCSeconds ->
      get_field(st, this, name, FSeconds)
    DatePrototypeGetMilliseconds | DatePrototypeGetUTCMilliseconds ->
      get_field(st, this, name, FMillis)
    DatePrototypeGetYear -> {
      let #(v, st) = get_field(st, this, name, FYear)
      case classify(v) {
        rt_types.KNum(JInt(y)) -> #(mk_number(JInt(y - 1900)), st)
        _ -> #(v, st)
      }
    }
    DatePrototypeSetTime -> set_time(st, this, args, name)
    DatePrototypeSetMilliseconds | DatePrototypeSetUTCMilliseconds ->
      date_set_field(st, this, args, name, SetMs)
    DatePrototypeSetSeconds | DatePrototypeSetUTCSeconds ->
      date_set_field(st, this, args, name, SetSeconds)
    DatePrototypeSetMinutes | DatePrototypeSetUTCMinutes ->
      date_set_field(st, this, args, name, SetMinutes)
    DatePrototypeSetHours | DatePrototypeSetUTCHours ->
      date_set_field(st, this, args, name, SetHours)
    DatePrototypeSetDate | DatePrototypeSetUTCDate ->
      date_set_field(st, this, args, name, SetDate)
    DatePrototypeSetMonth | DatePrototypeSetUTCMonth ->
      date_set_field(st, this, args, name, SetMonth)
    DatePrototypeSetFullYear | DatePrototypeSetUTCFullYear ->
      date_set_field(st, this, args, name, SetYear)
    DatePrototypeSetYear -> date_set_year(st, this, args, name)
    DatePrototypeToISOString -> to_iso(st, this, name)
    DatePrototypeToUTCString
    | DatePrototypeToString
    | DatePrototypeToDateString
    | DatePrototypeToTimeString
    | DatePrototypeToLocaleString
    | DatePrototypeToLocaleDateString
    | DatePrototypeToLocaleTimeString -> to_string_like(st, this, name)
    DatePrototypeToJSON -> to_json(st, this)
    DatePrototypeSymbolToPrimitive -> symbol_to_primitive(st, this, args)
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
    DateConstructor(proto:) -> constructor(st, proto, args, new_target)
    _ -> rt_val.t_throw_type_error(st, "not a constructor")
  }
}

// ── §21.4.2.1 Date ( ...values ) ────────────────────────────────────────────

fn constructor(
  st: Agent,
  fallback_proto: Handle,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  let #(tv, st) = case args {
    // Step 3.a: no args → now.
    [] -> #(JInt(now_ms(st)), st)
    // Step 3.b: single arg — Date instance / string / number.
    [only] ->
      case classify(only) {
        KHandle(h) ->
          case rt_store.t_cell_get(st, h) {
            SObject(kind: DateObj(ms:), ..) -> #(ms, st)
            _ -> {
              let #(prim, st) =
                rt_val.t_to_primitive(st, only, rt_types.HintDefault)
              case classify(prim) {
                KStr(s) -> #(parse_iso(s), st)
                _ -> {
                  let #(n, st) = rt_val.t_to_number(st, prim)
                  #(time_clip(n), st)
                }
              }
            }
          }
        KStr(s) -> #(parse_iso(s), st)
        _ -> {
          let #(n, st) = rt_val.t_to_number(st, only)
          #(time_clip(n), st)
        }
      }
    // Step 3.c: 2+ args — MakeDate(MakeDay(y,m,d), MakeTime(h,mi,s,ms)).
    _ -> {
      let #(ms, st) = make_from_components(st, args)
      #(ms, st)
    }
  }
  let #(proto, st) = proto_from_new_target(st, new_target, fallback_proto)
  rt_store.t_cell_new(
    st,
    SObject(
      kind: DateObj(ms: tv),
      proto: Some(proto),
      props: dict.new(),
      symbol_props: [],
      elements: NoElements,
      extensible: True,
    ),
  )
}

// ── static methods ──────────────────────────────────────────────────────────

fn date_parse(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(s, st) = rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
  #(mk_number(parse_iso(s)), st)
}

fn date_utc(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(ms, st) = make_from_components(st, args)
  #(mk_number(ms), st)
}

// ── prototype methods ───────────────────────────────────────────────────────

fn get_time(st: Agent, this: JsVal, name: String) -> #(JsVal, Agent) {
  #(mk_number(require_date(st, this, name)), st)
}

fn set_time(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  name: String,
) -> #(JsVal, Agent) {
  let h = require_date_handle(st, this, name)
  let #(n, st) = rt_val.t_to_number(st, helpers.first_arg_or_undefined(args))
  let clipped = time_clip(n)
  let st = write_date(st, h, clipped)
  #(mk_number(clipped), st)
}

/// The first field a setX method writes; excludes weekday. arc `SettableField`.
type SettableField {
  SetYear
  SetMonth
  SetDate
  SetHours
  SetMinutes
  SetSeconds
  SetMs
}

/// §21.4.4.18-.30: how many consecutive components a setter accepts.
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

/// Position in the year..ms component order (0..6).
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

/// Shared setter — arc `date_set_field`. Coerce supplied args (capped at spec
/// arity), decompose current [[DateValue]], overwrite the run starting at
/// `first`, MakeDate. UTC only (LocalTime≡UtcTime pending §10 tz FFI).
fn date_set_field(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  name: String,
  first: SettableField,
) -> #(JsVal, Agent) {
  let h = require_date_handle(st, this, name)
  let tv = require_date(st, this, name)
  let supplied = list.take(args, settable_max_args(first))
  let #(new_nums, st) = args_to_nums(st, supplied)
  case compute_set_field(tv, first, new_nums) {
    // Original NaN and not setFullYear: §21.4.4 "if t is NaN, return NaN" —
    // do NOT write back (a valueOf side effect that setTime'd survives).
    None -> #(mk_number(JNan), st)
    Some(result) -> {
      let result = case args {
        [] -> JNan
        _ -> result
      }
      let st = write_date(st, h, result)
      #(mk_number(result), st)
    }
  }
}

fn compute_set_field(
  tv: JsNum,
  first: SettableField,
  new_nums: List(JsNum),
) -> Option(JsNum) {
  case tv {
    JInt(ms) -> {
      let base = fields_to_components(breakdown(ms))
      Some(make_date_from_components(overwrite_fields(base, first, new_nums)))
    }
    rt_types.JFloat(f) ->
      compute_set_field(JInt(rt_val.float_to_int(f)), first, new_nums)
    _ ->
      case first {
        // §21.4.4.21 step 5: setFullYear on Invalid Date → t becomes +0.
        SetYear -> {
          let z = JInt(0)
          let epoch = DateComponents(JInt(1970), z, JInt(1), z, z, z, z)
          Some(
            make_date_from_components(overwrite_fields(epoch, first, new_nums)),
          )
        }
        _ -> None
      }
  }
}

fn fields_to_components(
  b: #(Int, Int, Int, Int, Int, Int, Int, Int),
) -> DateComponents {
  let #(y, mo, d, _wd, h, mi, s, ms) = b
  DateComponents(
    JInt(y),
    JInt(mo),
    JInt(d),
    JInt(h),
    JInt(mi),
    JInt(s),
    JInt(ms),
  )
}

/// Replace `len(new_nums)` consecutive components starting at `first`.
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

/// Annex B §B.2.3.2 Date.prototype.setYear — [0,99] maps to 1900+year.
fn date_set_year(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  name: String,
) -> #(JsVal, Agent) {
  let h = require_date_handle(st, this, name)
  let tv = require_date(st, this, name)
  let #(n, st) = rt_val.t_to_number(st, helpers.first_arg_or_undefined(args))
  case num_to_int(n) {
    None -> {
      let st = write_date(st, h, JNan)
      #(mk_number(JNan), st)
    }
    Some(y) -> {
      let y = case y >= 0 && y <= 99 {
        True -> 1900 + y
        False -> y
      }
      let base = case tv {
        JInt(ms) -> fields_to_components(breakdown(ms))
        _ -> {
          let z = JInt(0)
          DateComponents(JInt(1970), z, JInt(1), z, z, z, z)
        }
      }
      let result =
        make_date_from_components(DateComponents(..base, year: JInt(y)))
      let st = write_date(st, h, result)
      #(mk_number(result), st)
    }
  }
}

fn to_iso(st: Agent, this: JsVal, name: String) -> #(JsVal, Agent) {
  case require_date(st, this, name) {
    JInt(ms) -> #(mk_string(format_iso(ms)), st)
    _ -> rt_val.t_throw_range_error(st, "Invalid time value")
  }
}

fn to_string_like(st: Agent, this: JsVal, name: String) -> #(JsVal, Agent) {
  case require_date(st, this, name) {
    JInt(ms) -> #(mk_string(to_utc_string(ms)), st)
    _ -> #(mk_string("Invalid Date"), st)
  }
}

/// §21.4.4.37 Date.prototype.toJSON — ToPrimitive(Number) then toISOString().
fn to_json(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let #(o_h, st) = rt_val.t_to_object(st, this)
  let #(tv, st) = rt_val.t_to_primitive(st, mk_object(o_h), HintNumber)
  case classify(tv) {
    rt_types.KNum(JNan)
    | rt_types.KNum(rt_types.JPosInf)
    | rt_types.KNum(rt_types.JNegInf) -> #(rt_types.mk_null(), st)
    _ -> {
      let #(iso_fn, st) =
        rt_obj.t_get_prop(st, mk_object(o_h), StringKey(Named("toISOString")))
      let js = st.store
      js.ops.call(st, iso_fn, mk_object(o_h), [])
    }
  }
}

/// §21.4.4.45 Date.prototype[@@toPrimitive] ( hint ).
fn symbol_to_primitive(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case classify(this) {
    KHandle(_) -> Nil
    _ ->
      rt_val.t_throw_type_error(
        st,
        "Date.prototype[Symbol.toPrimitive] called on non-object",
      )
  }
  let names = case classify(helpers.first_arg_or_undefined(args)) {
    KStr("string") | KStr("default") -> ["toString", "valueOf"]
    KStr("number") -> ["valueOf", "toString"]
    _ ->
      rt_val.t_throw_type_error(
        st,
        "Invalid hint: must be \"string\", \"number\", or \"default\"",
      )
  }
  // §7.1.1.1 OrdinaryToPrimitive — inlined (rt_val's is private).
  let js = st.store
  ordinary_to_primitive(st, js.ops, this, names)
}

fn ordinary_to_primitive(
  st: Agent,
  ops: rt_types.JsOps(Agent),
  o: JsVal,
  names: List(String),
) -> #(JsVal, Agent) {
  case names {
    [] ->
      rt_val.t_throw_type_error(st, "Cannot convert object to primitive value")
    [name, ..rest] -> {
      let #(method, st) = ops.get_prop(st, o, StringKey(Named(name)))
      let #(is_call, st) = rt_val.t_is_callable(st, method)
      case is_call {
        False -> ordinary_to_primitive(st, ops, o, rest)
        True -> {
          let #(result, st) = ops.call(st, method, o, [])
          case classify(result) {
            KHandle(_) -> ordinary_to_primitive(st, ops, o, rest)
            _ -> #(result, st)
          }
        }
      }
    }
  }
}

// ── date-field arithmetic (§21.4.1 pure Int; UTC only pending tz FFI) ──────

type Field {
  FYear
  FMonth
  FDate
  FDay
  FHours
  FMinutes
  FSeconds
  FMillis
}

fn get_field(
  st: Agent,
  this: JsVal,
  name: String,
  which: Field,
) -> #(JsVal, Agent) {
  case require_date(st, this, name) {
    JInt(ms) -> {
      let #(y, mo, d, wd, h, mi, s, milli) = breakdown(ms)
      let n = case which {
        FYear -> y
        FMonth -> mo
        FDate -> d
        FDay -> wd
        FHours -> h
        FMinutes -> mi
        FSeconds -> s
        FMillis -> milli
      }
      #(mk_number(JInt(n)), st)
    }
    _ -> #(mk_number(JNan), st)
  }
}

/// §21.4.1.3-12 breakdown of an epoch-ms into y/mo/d/weekday/h/mi/s/ms.
fn breakdown(ms: Int) -> #(Int, Int, Int, Int, Int, Int, Int, Int) {
  let day = floor_div(ms, 86_400_000)
  let tod = floor_mod(ms, 86_400_000)
  let #(y, mo, d) = civil_from_days(day)
  let wd = floor_mod(day + 4, 7)
  let h = floor_div(tod, 3_600_000)
  let mi = floor_mod(floor_div(tod, 60_000), 60)
  let s = floor_mod(floor_div(tod, 1000), 60)
  let milli = floor_mod(tod, 1000)
  #(y, mo, d, wd, h, mi, s, milli)
}

/// §21.4.2.1 step 3.c / §21.4.3.4: coerce EVERY arg via ToNumber (in order —
/// valueOf side effects observable), pad to the full 7-tuple, then MakeDate.
/// arc `args_to_time_value` + `pad_fields` + `make_date_checked`.
fn make_from_components(st: Agent, args: List(JsVal)) -> #(JsNum, Agent) {
  let #(nums, st) = args_to_nums(st, list.take(args, 7))
  #(make_date_checked(pad_fields(nums)), st)
}

/// The seven MakeDay/MakeTime inputs, always all present.
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

/// arc `pad_fields`: fill unsupplied tail with spec defaults (month 0, date 1,
/// time 0). The `[y]` arm makes `Date.UTC(y)` valid (§21.4.3.4 defaults).
fn pad_fields(nums: List(JsNum)) -> DateComponents {
  let z = JInt(0)
  let one = JInt(1)
  case nums {
    [] -> DateComponents(JNan, z, one, z, z, z, z)
    [y] -> DateComponents(y, z, one, z, z, z, z)
    [y, mo] -> DateComponents(y, mo, one, z, z, z, z)
    [y, mo, d] -> DateComponents(y, mo, d, z, z, z, z)
    [y, mo, d, h] -> DateComponents(y, mo, d, h, z, z, z)
    [y, mo, d, h, mi] -> DateComponents(y, mo, d, h, mi, z, z)
    [y, mo, d, h, mi, s] -> DateComponents(y, mo, d, h, mi, s, z)
    [y, mo, d, h, mi, s, ms, ..] -> DateComponents(y, mo, d, h, mi, s, ms)
  }
}

/// arc `make_date_checked`: any non-finite → NaN; else truncate, apply the
/// §21.4.2.1 step 5.k [0,100) year mapping, and MakeDay+MakeTime+TimeClip.
fn make_date_checked(c: DateComponents) -> JsNum {
  case components_to_ints(c) {
    None -> JNan
    Some(#(y, mo, d, h, mi, s, ms)) -> {
      let y = case y >= 0 && y <= 99 {
        True -> 1900 + y
        False -> y
      }
      make_date(y, mo, d, h, mi, s, ms)
    }
  }
}

/// arc `make_date_from_components` — setter variant: NO year mapping.
fn make_date_from_components(c: DateComponents) -> JsNum {
  case components_to_ints(c) {
    None -> JNan
    Some(#(y, mo, d, h, mi, s, ms)) -> make_date(y, mo, d, h, mi, s, ms)
  }
}

fn components_to_ints(
  c: DateComponents,
) -> Option(#(Int, Int, Int, Int, Int, Int, Int)) {
  use y <- option.then(num_to_int(c.year))
  use mo <- option.then(num_to_int(c.month))
  use d <- option.then(num_to_int(c.date))
  use h <- option.then(num_to_int(c.hours))
  use mi <- option.then(num_to_int(c.minutes))
  use s <- option.then(num_to_int(c.seconds))
  use ms <- option.map(num_to_int(c.ms))
  #(y, mo, d, h, mi, s, ms)
}

fn num_to_int(n: JsNum) -> Option(Int) {
  case n {
    JInt(i) -> Some(i)
    rt_types.JFloat(f) -> Some(rt_val.float_to_int(f))
    JNan | rt_types.JPosInf | rt_types.JNegInf -> None
  }
}

/// §21.4.1.28 MakeDay + §21.4.1.29 MakeTime + §21.4.1.30 MakeDate + TimeClip.
/// UTC only (LocalTime≡UtcTime pending §10 tz FFI). arc `make_date`.
fn make_date(
  y: Int,
  mo: Int,
  d: Int,
  h: Int,
  mi: Int,
  s: Int,
  ms: Int,
) -> JsNum {
  let ym = y + floor_div(mo, 12)
  let mn = floor_mod(mo, 12)
  // Guard before multiply (arc/QuickJS): out-of-range year → NaN.
  case ym < -285_426 || ym > 285_426 {
    True -> JNan
    False -> {
      let day = days_from_civil(ym, mn, d)
      let time = h * 3_600_000 + mi * 60_000 + s * 1000 + ms
      time_clip(JInt(day * 86_400_000 + time))
    }
  }
}

/// Coerce each arg via ToNumber, threading state. Unlike the old
/// `coerce_all_numbers`, does NOT early-exit on NaN — every arg's valueOf must
/// run in order (§21.4.2.1 step 3.c, §21.4.3.4). arc `args_to_nums`.
fn args_to_nums(st: Agent, args: List(JsVal)) -> #(List(JsNum), Agent) {
  let #(rev, st) =
    list.fold(args, #([], st), fn(acc, arg) {
      let #(nums, st) = acc
      let #(n, st) = rt_val.t_to_number(st, arg)
      #([n, ..nums], st)
    })
  #(list.reverse(rev), st)
}

/// §21.4.1.31 TimeClip.
fn time_clip(n: JsNum) -> JsNum {
  case n {
    JInt(i) ->
      case i > 8_640_000_000_000_000 || i < -8_640_000_000_000_000 {
        True -> JNan
        False -> JInt(i)
      }
    rt_types.JFloat(f) -> time_clip(JInt(rt_val.float_to_int(f)))
    _ -> JNan
  }
}

// ── Gregorian civil-date <-> epoch-day (Howard Hinnant algorithm; arc port) ─

fn civil_from_days(z: Int) -> #(Int, Int, Int) {
  let z = z + 719_468
  let era = floor_div(z, 146_097)
  let doe = z - era * 146_097
  let yoe =
    floor_div(
      doe
        - floor_div(doe, 1460)
        + floor_div(doe, 36_524)
        - floor_div(doe, 146_096),
      365,
    )
  let y = yoe + era * 400
  let doy = doe - { 365 * yoe + floor_div(yoe, 4) - floor_div(yoe, 100) }
  let mp = floor_div(5 * doy + 2, 153)
  let d = doy - floor_div(153 * mp + 2, 5) + 1
  let m = case mp < 10 {
    True -> mp + 2
    False -> mp - 10
  }
  let y = case m < 2 {
    True -> y + 1
    False -> y
  }
  #(y, m, d)
}

fn days_from_civil(y: Int, m: Int, d: Int) -> Int {
  let y = case m < 2 {
    True -> y - 1
    False -> y
  }
  let era = floor_div(y, 400)
  let yoe = y - era * 400
  let mp = case m > 1 {
    True -> m - 2
    False -> m + 10
  }
  let doy = floor_div(153 * mp + 2, 5) + d - 1
  let doe = yoe * 365 + floor_div(yoe, 4) - floor_div(yoe, 100) + doy
  era * 146_097 + doe - 719_468
}

// ── formatting ──────────────────────────────────────────────────────────────

fn format_iso(ms: Int) -> String {
  let #(y, mo, d, _, h, mi, s, milli) = breakdown(ms)
  pad(y, 4)
  <> "-"
  <> pad(mo + 1, 2)
  <> "-"
  <> pad(d, 2)
  <> "T"
  <> pad(h, 2)
  <> ":"
  <> pad(mi, 2)
  <> ":"
  <> pad(s, 2)
  <> "."
  <> pad(milli, 3)
  <> "Z"
}

fn to_utc_string(ms: Int) -> String {
  let #(y, mo, d, wd, h, mi, s, _) = breakdown(ms)
  weekday_name(wd)
  <> ", "
  <> pad(d, 2)
  <> " "
  <> month_name(mo)
  <> " "
  <> pad(y, 4)
  <> " "
  <> pad(h, 2)
  <> ":"
  <> pad(mi, 2)
  <> ":"
  <> pad(s, 2)
  <> " GMT"
}

/// §21.4.3.2: parse the §21.4.1.32 Date Time String Format subset arc supports
/// under the harness. Recognises `YYYY-MM-DDTHH:mm:ss(.sss)?Z?`; anything else
/// is NaN.
fn parse_iso(s: String) -> JsNum {
  case <<s:utf8>> {
    <<
      y3,
      y2,
      y1,
      y0,
      "-":utf8,
      m1,
      m0,
      "-":utf8,
      d1,
      d0,
      "T":utf8,
      h1,
      h0,
      ":":utf8,
      mi1,
      mi0,
      ":":utf8,
      s1,
      s0,
      rest:bytes,
    >> ->
      case
        digs4(y3, y2, y1, y0),
        digs2(m1, m0),
        digs2(d1, d0),
        digs2(h1, h0),
        digs2(mi1, mi0),
        digs2(s1, s0)
      {
        Ok(y), Ok(mo), Ok(d), Ok(h), Ok(mi), Ok(sec) -> {
          let milli = parse_frac(rest)
          let day = days_from_civil(y, mo - 1, d)
          time_clip(JInt(
            day * 86_400_000 + h * 3_600_000 + mi * 60_000 + sec * 1000 + milli,
          ))
        }
        _, _, _, _, _, _ -> JNan
      }
    _ -> JNan
  }
}

fn parse_frac(rest: BitArray) -> Int {
  case rest {
    <<".":utf8, a, b, c, _:bytes>> ->
      case digs2(a, b), dig(c) {
        Ok(ab), Ok(c) -> ab * 10 + c
        _, _ -> 0
      }
    _ -> 0
  }
}

// ── low-level helpers ───────────────────────────────────────────────────────

fn now_ms(st: Agent) -> Int {
  let js = st.store
  js.host_hooks.monotonic_now()
}

fn require_date(st: Agent, v: JsVal, name: String) -> JsNum {
  case classify(v) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: DateObj(ms:), ..) -> ms
        _ -> throw_receiver(st, name)
      }
    _ -> throw_receiver(st, name)
  }
}

fn require_date_handle(st: Agent, v: JsVal, name: String) -> Handle {
  case classify(v) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: DateObj(..), ..) -> h
        _ -> throw_receiver(st, name)
      }
    _ -> throw_receiver(st, name)
  }
}

fn write_date(st: Agent, h: Handle, ms: JsNum) -> Agent {
  rt_store.t_cell_update(st, h, fn(slot) {
    case slot {
      SObject(kind: DateObj(..), ..) -> SObject(..slot, kind: DateObj(ms:))
      _ -> slot
    }
  })
}

fn throw_receiver(st: Agent, name: String) -> a {
  rt_val.t_throw_type_error(
    st,
    "Method Date.prototype." <> name <> " called on incompatible receiver",
  )
}

fn proto_from_new_target(
  st: Agent,
  new_target: JsVal,
  fallback: Handle,
) -> #(Handle, Agent) {
  let #(proto, st) =
    rt_obj.t_get_prop(st, new_target, StringKey(Named("prototype")))
  case classify(proto) {
    KHandle(h) -> #(h, st)
    _ -> #(fallback, st)
  }
}

fn floor_div(a: Int, b: Int) -> Int {
  case { a < 0 } != { b < 0 } && a % b != 0 {
    True -> a / b - 1
    False -> a / b
  }
}

fn floor_mod(a: Int, b: Int) -> Int {
  a - floor_div(a, b) * b
}

fn pad(n: Int, width: Int) -> String {
  let s = int.to_string(int.absolute_value(n))
  let sign = case n < 0 {
    True -> "-"
    False -> ""
  }
  sign <> pad_zeros(s, width - byte_size(s))
}

fn pad_zeros(s: String, need: Int) -> String {
  case need > 0 {
    True -> pad_zeros("0" <> s, need - 1)
    False -> s
  }
}

fn dig(b: Int) -> Result(Int, Nil) {
  case b >= 0x30 && b <= 0x39 {
    True -> Ok(b - 0x30)
    False -> Error(Nil)
  }
}

fn digs2(a: Int, b: Int) -> Result(Int, Nil) {
  case dig(a), dig(b) {
    Ok(a), Ok(b) -> Ok(a * 10 + b)
    _, _ -> Error(Nil)
  }
}

fn digs4(a: Int, b: Int, c: Int, d: Int) -> Result(Int, Nil) {
  case digs2(a, b), digs2(c, d) {
    Ok(ab), Ok(cd) -> Ok(ab * 100 + cd)
    _, _ -> Error(Nil)
  }
}

fn weekday_name(wd: Int) -> String {
  case wd {
    0 -> "Sun"
    1 -> "Mon"
    2 -> "Tue"
    3 -> "Wed"
    4 -> "Thu"
    5 -> "Fri"
    _ -> "Sat"
  }
}

fn month_name(m: Int) -> String {
  case m {
    0 -> "Jan"
    1 -> "Feb"
    2 -> "Mar"
    3 -> "Apr"
    4 -> "May"
    5 -> "Jun"
    6 -> "Jul"
    7 -> "Aug"
    8 -> "Sep"
    9 -> "Oct"
    10 -> "Nov"
    _ -> "Dec"
  }
}

fn method_name(n: DateNative) -> String {
  case n {
    DateConstructor(..) -> "Date"
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

@external(erlang, "erlang", "byte_size")
fn byte_size(s: String) -> Int
