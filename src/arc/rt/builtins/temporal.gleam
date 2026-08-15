//// The Temporal namespace (proposal-temporal, test262 feature "Temporal"):
//// reserves the eight type prototypes, registers each type's constructor,
//// statics, getters and methods, Temporal.Now, and fans dispatch out to the
//// per-type modules. Temporal.Instant and Temporal.Now are implemented here.
////
//// Shared abstract operations live in temporal_common.gleam; ISO 8601
//// parsing/formatting in temporal_iso.gleam; named IANA zones resolve through
//// temporal_tz.gleam (system tzdata).

import arc/internal/int_math.{floor_div}
import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/builtins/temporal_common.{
  Hour, Nanosecond, Second, Trunc, apply_new_target_proto, apply_since_mode,
  apply_since_ns, as_if_positive_mode, balance_time_ns, check_diff_setup,
  epoch_ns_to_iso_in, get_difference_settings, get_fractional_digits,
  get_options_object, get_rounding_mode_option, get_unit_option,
  instant_slot_of, make_date, make_date_time, make_duration, make_instant,
  make_time, make_zoned, max_unit, parse_time_zone_id, require_temporal,
  require_time_unit, round_options, round_to_increment,
  seconds_string_precision, system_time_zone, terr, time_only_ns, time_unit_ns,
  time_zone_id, to_temporal_duration, to_temporal_instant,
  to_temporal_time_zone, tz_offset_ns_at, unit_rank,
}
import arc/rt/builtins/temporal_iso.{
  type Precision, AutoPrec, epoch_ns_to_iso, format_iso_date, format_iso_time,
  ns_max_instant, ns_per_day, ns_per_ms,
}
import arc/rt/builtins/temporal_duration
import arc/rt/builtins/temporal_plain_date
import arc/rt/builtins/temporal_plain_date_time
import arc/rt/builtins/temporal_plain_month_day
import arc/rt/builtins/temporal_plain_time
import arc/rt/builtins/temporal_plain_year_month
import arc/rt/builtins/temporal_zoned_date_time
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type Handle, type InstantGetterName, type InstantMethodName,
  type InstantStaticName, type JsVal, type NativeToken, type TemporalNative,
  type TemporalNowName, type TemporalProtos, type TimeZone, InstantAdd,
  InstantCompare, InstantEpochMilliseconds, InstantEpochNanoseconds,
  InstantEquals, InstantFrom, InstantFromEpochMilliseconds,
  InstantFromEpochNanoseconds, InstantRound, InstantSince, InstantSubtract,
  InstantToJson, InstantToLocaleString, InstantToString,
  InstantToZonedDateTimeIso, InstantUntil, InstantValueOf, JFloat, JInt, JNan,
  JNegInf, JPosInf, KStr, KUndef, Named, NowInstant, NowPlainDateISO,
  NowPlainDateTimeISO, NowPlainTimeISO, NowTimeZoneId, NowZonedDateTimeISO,
  StringKey, TemporalDurationCtor, TemporalDurationGetter,
  TemporalDurationMethod, TemporalDurationStatic, TemporalInstantCtor,
  TemporalInstantGetter,
  TemporalInstantMethod, TemporalInstantStatic, TemporalN, TemporalNowFn,
  TemporalPlainDateCtor, TemporalPlainDateGetter, TemporalPlainDateMethod,
  TemporalPlainDateStatic, TemporalPlainDateTimeCtor,
  TemporalPlainDateTimeGetter, TemporalPlainDateTimeMethod,
  TemporalPlainDateTimeStatic, TemporalPlainMonthDayCtor,
  TemporalPlainMonthDayGetter, TemporalPlainMonthDayMethod,
  TemporalPlainMonthDayStatic, TemporalPlainTimeCtor, TemporalPlainTimeGetter,
  TemporalPlainTimeMethod, TemporalPlainTimeStatic, TemporalPlainYearMonthCtor,
  TemporalPlainYearMonthGetter, TemporalPlainYearMonthMethod,
  TemporalPlainYearMonthStatic,
  TemporalProtos, TemporalZonedDateTimeCtor, TemporalZonedDateTimeGetter,
  TemporalZonedDateTimeMethod, TemporalZonedDateTimeStatic, classify,
  mk_bigint, mk_bool, mk_number, mk_object, mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/order

// ============================================================================
// Init — Temporal namespace, the type constructors, Temporal.Now
// ============================================================================

/// Build the Temporal global. Returns the namespace object.
pub fn init(
  st: Agent,
  object_proto: Handle,
  function_proto: Handle,
) -> #(Handle, Agent) {
  // Reserve all eight prototypes up front so each native token can carry the
  // handles of every sibling prototype.
  let #(pd_proto, st) = common.alloc_proto(st, Some(object_proto), dict.new())
  let #(pt_proto, st) = common.alloc_proto(st, Some(object_proto), dict.new())
  let #(pdt_proto, st) = common.alloc_proto(st, Some(object_proto), dict.new())
  let #(pym_proto, st) = common.alloc_proto(st, Some(object_proto), dict.new())
  let #(pmd_proto, st) = common.alloc_proto(st, Some(object_proto), dict.new())
  let #(dur_proto, st) = common.alloc_proto(st, Some(object_proto), dict.new())
  let #(ins_proto, st) = common.alloc_proto(st, Some(object_proto), dict.new())
  let #(zdt_proto, st) = common.alloc_proto(st, Some(object_proto), dict.new())
  let protos =
    TemporalProtos(
      plain_date: pd_proto,
      plain_time: pt_proto,
      plain_date_time: pdt_proto,
      plain_year_month: pym_proto,
      plain_month_day: pmd_proto,
      duration: dur_proto,
      instant: ins_proto,
      zoned_date_time: zdt_proto,
    )

  let #(pd_ctor, st) =
    init_temporal_type(
      st,
      function_proto,
      "PlainDate",
      3,
      pd_proto,
      temporal_plain_date.ctor_token(protos),
      temporal_plain_date.statics(protos),
      temporal_plain_date.getters(),
      temporal_plain_date.methods(protos),
    )

  let #(pt_ctor, st) =
    init_temporal_type(
      st,
      function_proto,
      "PlainTime",
      0,
      pt_proto,
      temporal_plain_time.ctor_token(protos),
      temporal_plain_time.statics(protos),
      temporal_plain_time.getters(),
      temporal_plain_time.methods(protos),
    )

  let #(pdt_ctor, st) =
    init_temporal_type(
      st,
      function_proto,
      "PlainDateTime",
      3,
      pdt_proto,
      temporal_plain_date_time.ctor_token(protos),
      temporal_plain_date_time.statics(protos),
      temporal_plain_date_time.getters(),
      temporal_plain_date_time.methods(protos),
    )

  let #(pym_ctor, st) =
    init_temporal_type(
      st,
      function_proto,
      "PlainYearMonth",
      2,
      pym_proto,
      temporal_plain_year_month.ctor_token(protos),
      temporal_plain_year_month.statics(protos),
      temporal_plain_year_month.getters(),
      temporal_plain_year_month.methods(protos),
    )

  let #(pmd_ctor, st) =
    init_temporal_type(
      st,
      function_proto,
      "PlainMonthDay",
      2,
      pmd_proto,
      temporal_plain_month_day.ctor_token(protos),
      temporal_plain_month_day.statics(protos),
      temporal_plain_month_day.getters(),
      temporal_plain_month_day.methods(protos),
    )

  let #(dur_ctor, st) =
    init_temporal_type(
      st,
      function_proto,
      "Duration",
      0,
      dur_proto,
      temporal_duration.ctor_token(protos),
      temporal_duration.statics(protos),
      temporal_duration.getters(),
      temporal_duration.methods(protos),
    )

  let #(ins_ctor, st) =
    init_temporal_type(
      st,
      function_proto,
      "Instant",
      1,
      ins_proto,
      TemporalN(TemporalInstantCtor(protos)),
      list.map(
        [
          #(InstantFrom, 1),
          #(InstantFromEpochMilliseconds, 1),
          #(InstantFromEpochNanoseconds, 1),
          #(InstantCompare, 2),
        ],
        fn(s) {
          #(
            instant_static_name(s.0),
            TemporalN(TemporalInstantStatic(s.0, protos)),
            s.1,
          )
        },
      ),
      list.map([InstantEpochMilliseconds, InstantEpochNanoseconds], fn(g) {
        #(instant_getter_name(g), TemporalN(TemporalInstantGetter(g)))
      }),
      list.map(
        [
          #(InstantAdd, 1),
          #(InstantSubtract, 1),
          #(InstantUntil, 1),
          #(InstantSince, 1),
          #(InstantRound, 1),
          #(InstantEquals, 1),
          #(InstantToString, 0),
          #(InstantToLocaleString, 0),
          #(InstantToJson, 0),
          #(InstantValueOf, 0),
          #(InstantToZonedDateTimeIso, 1),
        ],
        fn(m) {
          #(
            instant_method_name(m.0),
            TemporalN(TemporalInstantMethod(m.0, protos)),
            m.1,
          )
        },
      ),
    )

  let #(zdt_ctor, st) =
    init_temporal_type(
      st,
      function_proto,
      "ZonedDateTime",
      2,
      zdt_proto,
      temporal_zoned_date_time.ctor_token(protos),
      temporal_zoned_date_time.statics(protos),
      temporal_zoned_date_time.getters(),
      temporal_zoned_date_time.methods(protos),
    )

  // Temporal.Now namespace
  let #(now_props, st) =
    common.alloc_methods(
      st,
      function_proto,
      list.map(
        [
          NowInstant,
          NowTimeZoneId,
          NowPlainDateISO,
          NowPlainDateTimeISO,
          NowPlainTimeISO,
          NowZonedDateTimeISO,
        ],
        fn(n) { #(now_name(n), TemporalN(TemporalNowFn(n, protos)), 0) },
      ),
    )
  let #(now_h, st) =
    common.init_namespace(st, object_proto, "Temporal.Now", now_props)

  // Temporal namespace itself, in spec order: PlainDate, PlainTime,
  // PlainDateTime, PlainYearMonth, PlainMonthDay, Duration, Instant,
  // ZonedDateTime, Now.
  let #(pd_prop, st) = common.builtin_property(st, mk_object(pd_ctor))
  let #(pt_prop, st) = common.builtin_property(st, mk_object(pt_ctor))
  let #(pdt_prop, st) = common.builtin_property(st, mk_object(pdt_ctor))
  let #(pym_prop, st) = common.builtin_property(st, mk_object(pym_ctor))
  let #(pmd_prop, st) = common.builtin_property(st, mk_object(pmd_ctor))
  let #(dur_prop, st) = common.builtin_property(st, mk_object(dur_ctor))
  let #(ins_prop, st) = common.builtin_property(st, mk_object(ins_ctor))
  let #(zdt_prop, st) = common.builtin_property(st, mk_object(zdt_ctor))
  let #(now_prop, st) = common.builtin_property(st, mk_object(now_h))
  common.init_namespace(st, object_proto, "Temporal", [
    #("PlainDate", pd_prop),
    #("PlainTime", pt_prop),
    #("PlainDateTime", pdt_prop),
    #("PlainYearMonth", pym_prop),
    #("PlainMonthDay", pmd_prop),
    #("Duration", dur_prop),
    #("Instant", ins_prop),
    #("ZonedDateTime", zdt_prop),
    #("Now", now_prop),
  ])
}

/// Build one Temporal type on its reserved prototype: constructor (with
/// statics) + filled prototype (getters, methods, @@toStringTag, constructor
/// backlink). Returns the constructor handle.
pub fn init_temporal_type(
  st: Agent,
  function_proto: Handle,
  name: String,
  arity: Int,
  proto: Handle,
  ctor_tag: NativeToken,
  statics: List(#(String, NativeToken, Int)),
  getters: List(#(String, NativeToken)),
  methods: List(#(String, NativeToken, Int)),
) -> #(Handle, Agent) {
  let #(static_props, st) = common.alloc_methods(st, function_proto, statics)
  let #(getter_props, st) = common.alloc_getters(st, function_proto, getters)
  let #(method_props, st) = common.alloc_methods(st, function_proto, methods)
  let #(bt, st) =
    common.init_type_on(
      st,
      proto,
      function_proto,
      list.append(getter_props, method_props),
      fn(_proto) { ctor_tag },
      name,
      arity,
      static_props,
      True,
    )
  let st = common.add_to_string_tag(st, proto, "Temporal." <> name)
  #(bt.constructor, st)
}

fn instant_getter_name(g: InstantGetterName) -> String {
  case g {
    InstantEpochMilliseconds -> "epochMilliseconds"
    InstantEpochNanoseconds -> "epochNanoseconds"
  }
}

fn instant_method_name(m: InstantMethodName) -> String {
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

fn instant_static_name(s: InstantStaticName) -> String {
  case s {
    InstantFrom -> "from"
    InstantFromEpochMilliseconds -> "fromEpochMilliseconds"
    InstantFromEpochNanoseconds -> "fromEpochNanoseconds"
    InstantCompare -> "compare"
  }
}

fn now_name(n: TemporalNowName) -> String {
  case n {
    NowInstant -> "instant"
    NowTimeZoneId -> "timeZoneId"
    NowPlainDateISO -> "plainDateISO"
    NowPlainDateTimeISO -> "plainDateTimeISO"
    NowPlainTimeISO -> "plainTimeISO"
    NowZonedDateTimeISO -> "zonedDateTimeISO"
  }
}

// ============================================================================
// Dispatch
// ============================================================================

pub fn dispatch(
  st: Agent,
  native: TemporalNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    // All Temporal constructors throw TypeError when called without `new`.
    TemporalInstantCtor(..) | TemporalPlainTimeCtor(..) | TemporalPlainDateCtor(..) ->
      rt_val.t_throw_type_error(st, "Temporal constructor requires new")
    TemporalPlainDateStatic(name:, protos:) ->
      temporal_plain_date.static(st, name, protos, args)
    TemporalPlainDateGetter(getter:) ->
      temporal_plain_date.getter(st, getter, this)
    TemporalPlainDateMethod(method:, protos:) ->
      temporal_plain_date.method(st, method, protos, this, args)
    TemporalPlainTimeStatic(name:, protos:) ->
      temporal_plain_time.static(st, name, protos, args)
    TemporalPlainTimeGetter(getter:) ->
      temporal_plain_time.getter(st, getter, this)
    TemporalPlainTimeMethod(method:, protos:) ->
      temporal_plain_time.method(st, method, protos, this, args)
    TemporalPlainDateTimeCtor(..) ->
      rt_val.t_throw_type_error(st, "Temporal constructor requires new")
    TemporalPlainDateTimeStatic(name:, protos:) ->
      temporal_plain_date_time.static(st, name, protos, args)
    TemporalPlainDateTimeGetter(getter:) ->
      temporal_plain_date_time.getter(st, getter, this)
    TemporalPlainDateTimeMethod(method:, protos:) ->
      temporal_plain_date_time.method(st, method, protos, this, args)
    TemporalPlainYearMonthCtor(..) | TemporalPlainMonthDayCtor(..) ->
      rt_val.t_throw_type_error(st, "Temporal constructor requires new")
    TemporalPlainYearMonthStatic(name:, protos:) ->
      temporal_plain_year_month.static(st, name, protos, args)
    TemporalPlainYearMonthGetter(getter:) ->
      temporal_plain_year_month.getter(st, getter, this)
    TemporalPlainYearMonthMethod(method:, protos:) ->
      temporal_plain_year_month.method(st, method, protos, this, args)
    TemporalPlainMonthDayStatic(name:, protos:) ->
      temporal_plain_month_day.static(st, name, protos, args)
    TemporalPlainMonthDayGetter(getter:) ->
      temporal_plain_month_day.getter(st, getter, this)
    TemporalPlainMonthDayMethod(method:, protos:) ->
      temporal_plain_month_day.method(st, method, protos, this, args)
    TemporalDurationCtor(..) ->
      rt_val.t_throw_type_error(st, "Temporal constructor requires new")
    TemporalDurationStatic(name:, protos:) ->
      temporal_duration.static(st, name, protos, args)
    TemporalDurationGetter(getter:) ->
      temporal_duration.getter(st, getter, this)
    TemporalDurationMethod(method:, protos:) ->
      temporal_duration.method(st, method, protos, this, args)
    TemporalInstantStatic(name:, protos:) ->
      instant_static(st, name, protos, args)
    TemporalInstantGetter(getter:) -> instant_getter(st, getter, this)
    TemporalInstantMethod(method:, protos:) ->
      instant_method(st, method, protos, this, args)
    TemporalNowFn(name:, protos:) -> now_dispatch(st, name, protos, args)
    TemporalZonedDateTimeCtor(..) ->
      rt_val.t_throw_type_error(st, "Temporal constructor requires new")
    TemporalZonedDateTimeStatic(name:, protos:) ->
      temporal_zoned_date_time.static(st, name, protos, args)
    TemporalZonedDateTimeGetter(getter:) ->
      temporal_zoned_date_time.getter(st, getter, this)
    TemporalZonedDateTimeMethod(method:, protos:) ->
      temporal_zoned_date_time.method(st, method, protos, this, args)
  }
}

pub fn dispatch_construct(
  st: Agent,
  native: TemporalNative,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case native {
    TemporalInstantCtor(protos:) -> {
      let #(v, st) = instant_ctor(st, protos, args)
      apply_new_target_proto(st, new_target, v)
    }
    TemporalPlainTimeCtor(protos:) -> {
      let #(v, st) = temporal_plain_time.ctor(st, protos, args)
      apply_new_target_proto(st, new_target, v)
    }
    TemporalPlainDateTimeCtor(protos:) -> {
      let #(v, st) = temporal_plain_date_time.ctor(st, protos, args)
      apply_new_target_proto(st, new_target, v)
    }
    TemporalPlainDateCtor(protos:) -> {
      let #(v, st) = temporal_plain_date.ctor(st, protos, args)
      apply_new_target_proto(st, new_target, v)
    }
    TemporalPlainYearMonthCtor(protos:) -> {
      let #(v, st) = temporal_plain_year_month.ctor(st, protos, args)
      apply_new_target_proto(st, new_target, v)
    }
    TemporalPlainMonthDayCtor(protos:) -> {
      let #(v, st) = temporal_plain_month_day.ctor(st, protos, args)
      apply_new_target_proto(st, new_target, v)
    }
    TemporalDurationCtor(protos:) -> {
      let #(v, st) = temporal_duration.ctor(st, protos, args)
      apply_new_target_proto(st, new_target, v)
    }
    TemporalZonedDateTimeCtor(protos:) -> {
      let #(v, st) = temporal_zoned_date_time.ctor(st, protos, args)
      apply_new_target_proto(st, new_target, v)
    }
    _ -> rt_val.t_throw_type_error(st, "not a constructor")
  }
}

// ============================================================================
// Temporal.Instant — constructor and statics
// ============================================================================

/// RequireInternalSlot(this, [[InitializedTemporalInstant]]).
fn require_instant(st: Agent, this: JsVal, name: String) -> Int {
  require_temporal(st, this, "Instant", name, instant_slot_of)
}

/// new Temporal.Instant(epochNanoseconds: BigInt)
fn instant_ctor(
  st: Agent,
  protos: TemporalProtos,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(ns, st) = rt_val.t_to_bigint(st, helpers.arg_at(args, 0))
  case int.absolute_value(ns) <= ns_max_instant {
    False -> rt_val.t_throw_range_error(st, "epoch nanoseconds out of range")
    True -> make_instant(st, protos, ns)
  }
}

fn instant_static(
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
      #(mk_number(JInt(int.compare(a, b) |> order_to_int)), st)
    }
    InstantFromEpochMilliseconds -> {
      let #(n, st) = rt_val.t_to_number(st, helpers.arg_at(args, 0))
      // -0 IS an integral Number, so this needs the ±0-safe predicate.
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
          case int.absolute_value(ns) <= ns_max_instant {
            False ->
              rt_val.t_throw_range_error(st, "epoch milliseconds out of range")
            True -> make_instant(st, protos, ns)
          }
        }
      }
    }
    InstantFromEpochNanoseconds -> {
      let #(ns, st) = rt_val.t_to_bigint(st, helpers.arg_at(args, 0))
      case int.absolute_value(ns) <= ns_max_instant {
        False ->
          rt_val.t_throw_range_error(st, "epoch nanoseconds out of range")
        True -> make_instant(st, protos, ns)
      }
    }
  }
}

fn order_to_int(o: order.Order) -> Int {
  case o {
    order.Lt -> -1
    order.Eq -> 0
    order.Gt -> 1
  }
}

// ============================================================================
// Temporal.Instant — getters
// ============================================================================

fn instant_getter(
  st: Agent,
  g: InstantGetterName,
  this: JsVal,
) -> #(JsVal, Agent) {
  let ns = require_instant(st, this, instant_getter_name(g))
  case g {
    InstantEpochMilliseconds -> #(mk_number(JInt(floor_div(ns, ns_per_ms))), st)
    InstantEpochNanoseconds -> #(mk_bigint(ns), st)
  }
}

// ============================================================================
// Temporal.Instant — methods
// ============================================================================

fn instant_method(
  st: Agent,
  m: InstantMethodName,
  protos: TemporalProtos,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let ns = require_instant(st, this, instant_method_name(m))
  case m {
    InstantToJson | InstantToLocaleString -> #(
      mk_string(format_instant(ns, AutoPrec)),
      st,
    )
    InstantToString -> {
      let #(opts, st) = get_options_object(st, helpers.arg_at(args, 0))
      // Read every option before any algorithmic validation:
      // fractionalSecondDigits, roundingMode, smallestUnit, timeZone
      // (alphabetical); only then resolve the precision (which may throw).
      let #(digits, st) = get_fractional_digits(st, opts)
      let #(mode, st) = get_rounding_mode_option(st, opts, Trunc)
      let #(su_opt, st) =
        get_unit_option(st, opts, "smallestUnit", allow_auto: False)
      let #(tz_opt, st) = case opts {
        None -> #(mk_undefined(), st)
        Some(h) ->
          rt_obj.t_get_prop(st, mk_object(h), StringKey(Named("timeZone")))
      }
      let #(prec, su, sinc, mode) =
        terr(st, seconds_string_precision(digits, su_opt, mode))
      let rounded = case su {
        None -> ns
        Some(u) ->
          round_to_increment(
            ns,
            sinc * time_unit_ns(u),
            as_if_positive_mode(mode),
          )
      }
      case classify(tz_opt) {
        KUndef -> #(mk_string(format_instant(rounded, prec)), st)
        KStr(tz_str) -> {
          let tz = terr(st, parse_time_zone_id(tz_str))
          let off = terr(st, tz_offset_ns_at(tz, rounded))
          let #(d, t) = epoch_ns_to_iso(rounded, off)
          let s =
            format_iso_date(d)
            <> "T"
            <> format_iso_time(t, prec)
            <> temporal_common.format_offset_rounded(off)
          #(mk_string(s), st)
        }
        _ -> rt_val.t_throw_type_error(st, "timeZone must be a string")
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
      case
        dur.years != 0 || dur.months != 0 || dur.weeks != 0 || dur.days != 0
      {
        True ->
          rt_val.t_throw_range_error(
            st,
            "Instant arithmetic does not support date units",
          )
        False -> {
          let delta = case m {
            InstantSubtract -> 0 - time_only_ns(dur)
            _ -> time_only_ns(dur)
          }
          let ns2 = ns + delta
          case int.absolute_value(ns2) <= ns_max_instant {
            False ->
              rt_val.t_throw_range_error(st, "instant outside valid range")
            True -> make_instant(st, protos, ns2)
          }
        }
      }
    }
    InstantRound -> {
      let #(#(su, inc, mode), st) =
        round_options(st, helpers.arg_at(args, 0), allow_day: False)
      let u_ns = time_unit_ns(su)
      // For Instant: increment*unit must divide 24h.
      let max = ns_per_day / u_ns
      case inc >= 1 && inc <= max && max % inc == 0 {
        False -> rt_val.t_throw_range_error(st, "invalid roundingIncrement")
        True -> {
          // RoundTemporalInstant rounds as if positive: "down" is towards
          // the Big Bang, not towards the epoch.
          let rounded =
            round_to_increment(ns, inc * u_ns, as_if_positive_mode(mode))
          case int.absolute_value(rounded) <= ns_max_instant {
            False ->
              rt_val.t_throw_range_error(st, "instant outside valid range")
            True -> make_instant(st, protos, rounded)
          }
        }
      }
    }
    InstantUntil | InstantSince -> {
      let #(other, st) = to_temporal_instant(st, helpers.arg_at(args, 0))
      instant_until_since(st, protos, ns, other, args, m == InstantSince)
    }
    InstantToZonedDateTimeIso -> {
      let arg = helpers.arg_at(args, 0)
      case classify(arg) {
        KStr(tz_str) -> {
          let tz = terr(st, parse_time_zone_id(tz_str))
          make_zoned(st, protos, ns, tz)
        }
        KUndef -> rt_val.t_throw_type_error(st, "time zone is required")
        _ -> rt_val.t_throw_type_error(st, "time zone must be a string")
      }
    }
  }
}

fn format_instant(ns: Int, prec: Precision) -> String {
  let #(d, t) = epoch_ns_to_iso(ns, 0)
  format_iso_date(d) <> "T" <> format_iso_time(t, prec) <> "Z"
}

/// DifferenceTemporalInstant: until/since with time-unit rounding, balanced
/// up to largestUnit (default "second", never above "hour").
fn instant_until_since(
  st: Agent,
  protos: TemporalProtos,
  a: Int,
  b: Int,
  args: List(JsVal),
  is_since: Bool,
) -> #(JsVal, Agent) {
  let #(#(largest, smallest, inc, mode), st) = get_difference_settings(st, args)
  let smallest = option.unwrap(smallest, Nanosecond)
  let largest = option.unwrap(largest, max_unit(smallest, Second))
  case
    unit_rank(smallest) > unit_rank(Hour) || unit_rank(largest) > unit_rank(Hour)
  {
    True ->
      rt_val.t_throw_range_error(st, "units must be time units for Instant")
    False -> {
      let Nil = check_diff_setup(st, largest, smallest, inc)
      let su = terr(st, require_time_unit(smallest))
      let mode2 = apply_since_mode(mode, is_since)
      let diff = b - a
      let rounded = round_to_increment(diff, inc * time_unit_ns(su), mode2)
      let rounded = apply_since_ns(rounded, is_since)
      let dur = balance_time_ns(rounded, largest)
      make_duration(st, protos, dur)
    }
  }
}

// ============================================================================
// Temporal.Now
// ============================================================================

/// SystemUTCEpochNanoseconds, from the host's wall clock.
fn now_epoch_ns(st: Agent) -> Int {
  st.hooks.wall_clock_ms() * ns_per_ms
}

fn now_dispatch(
  st: Agent,
  name: TemporalNowName,
  protos: TemporalProtos,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case name {
    NowInstant -> make_instant(st, protos, now_epoch_ns(st))
    NowTimeZoneId -> #(mk_string(time_zone_id(system_time_zone(st))), st)
    NowPlainDateISO -> {
      let #(tz, st) = now_tz_arg(st, args)
      let #(d, _) = terr(st, epoch_ns_to_iso_in(tz, now_epoch_ns(st)))
      make_date(st, protos, d)
    }
    NowPlainDateTimeISO -> {
      let #(tz, st) = now_tz_arg(st, args)
      let #(d, t) = terr(st, epoch_ns_to_iso_in(tz, now_epoch_ns(st)))
      make_date_time(st, protos, d, t)
    }
    NowPlainTimeISO -> {
      let #(tz, st) = now_tz_arg(st, args)
      let #(_, t) = terr(st, epoch_ns_to_iso_in(tz, now_epoch_ns(st)))
      make_time(st, protos, t)
    }
    NowZonedDateTimeISO -> {
      let #(tz, st) = now_tz_arg(st, args)
      make_zoned(st, protos, now_epoch_ns(st), tz)
    }
  }
}

/// The optional temporalTimeZoneLike argument of the Now functions: the
/// system zone when undefined, else ToTemporalTimeZoneIdentifier.
fn now_tz_arg(st: Agent, args: List(JsVal)) -> #(TimeZone, Agent) {
  let arg = helpers.arg_at(args, 0)
  case classify(arg) {
    KUndef -> #(system_time_zone(st), st)
    _ -> to_temporal_time_zone(st, arg)
  }
}
