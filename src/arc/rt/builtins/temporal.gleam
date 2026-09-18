import arc/internal/int_math.{floor_div}
import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/builtins/temporal_common.{
  Hour, Nanosecond, Second, Trunc, apply_new_target_proto, apply_since_mode,
  apply_since_ns, as_if_positive_mode, balance_time_ns, check_diff_setup,
  epoch_ns_to_iso_in, format_offset_rounded, get_difference_settings,
  get_fractional_digits, get_options_object, get_rounding_mode_option,
  get_unit_option, has_date_units, instant_slot_of, is_valid_epoch_ns, make_date,
  make_date_time, make_duration, make_instant, make_time, make_zoned, max_unit,
  opt_get, require_temporal, require_time_unit, round_options,
  round_to_increment, seconds_string_precision, system_time_zone, terr,
  time_part_ns, time_unit_ns, time_zone_id, to_temporal_duration,
  to_temporal_instant, to_temporal_time_zone, tz_offset_ns_at, unit_rank,
  valid_rounding_increment, validate_epoch_ns,
}
import arc/rt/builtins/temporal_duration
import arc/rt/builtins/temporal_iso.{
  type SecondsPrecision, AutoPrecision, epoch_ns_to_iso, format_iso_date,
  format_iso_time, ns_per_day, ns_per_ms,
}
import arc/rt/builtins/temporal_plain_date
import arc/rt/builtins/temporal_plain_date_time
import arc/rt/builtins/temporal_plain_month_day
import arc/rt/builtins/temporal_plain_time
import arc/rt/builtins/temporal_plain_year_month
import arc/rt/builtins/temporal_zoned_date_time
import arc/rt/types.{
  type Agent, type Handle, type InstantGetterName, type InstantMethodName,
  type InstantStaticName, type JsVal, type NativeToken, type TemporalNative,
  type TemporalNowName, type TemporalProtos, type TimeZone, InstantAdd,
  InstantCompare, InstantEpochMilliseconds, InstantEpochNanoseconds,
  InstantEquals, InstantFrom, InstantFromEpochMilliseconds,
  InstantFromEpochNanoseconds, InstantRound, InstantSince, InstantSubtract,
  InstantToJson, InstantToLocaleString, InstantToString,
  InstantToZonedDateTimeIso, InstantUntil, InstantValueOf, JFloat, JInt, JNan,
  JNegInf, JPosInf, KUndef, NowInstant, NowPlainDateISO, NowPlainDateTimeISO,
  NowPlainTimeISO, NowTimeZoneId, NowZonedDateTimeISO, TemporalDurationCtor,
  TemporalDurationGetter, TemporalDurationMethod, TemporalDurationStatic,
  TemporalInstantCtor, TemporalInstantGetter, TemporalInstantMethod,
  TemporalInstantStatic, TemporalN, TemporalNowFn, TemporalPlainDateCtor,
  TemporalPlainDateGetter, TemporalPlainDateMethod, TemporalPlainDateStatic,
  TemporalPlainDateTimeCtor, TemporalPlainDateTimeGetter,
  TemporalPlainDateTimeMethod, TemporalPlainDateTimeStatic,
  TemporalPlainMonthDayCtor, TemporalPlainMonthDayGetter,
  TemporalPlainMonthDayMethod, TemporalPlainMonthDayStatic,
  TemporalPlainTimeCtor, TemporalPlainTimeGetter, TemporalPlainTimeMethod,
  TemporalPlainTimeStatic, TemporalPlainYearMonthCtor,
  TemporalPlainYearMonthGetter, TemporalPlainYearMonthMethod,
  TemporalPlainYearMonthStatic, TemporalProtos, TemporalZonedDateTimeCtor,
  TemporalZonedDateTimeGetter, TemporalZonedDateTimeMethod,
  TemporalZonedDateTimeStatic, classify, mk_bigint, mk_bool, mk_number,
  mk_object, mk_string,
}
import arc/rt/val as rt_val
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/order

pub fn init(
  st: Agent,
  object_proto: Handle,
  function_proto: Handle,
) -> #(Handle, Agent) {
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
      constructible: True,
    )
  let st = common.add_string_tag(st, proto, "Temporal." <> name)
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

pub fn dispatch(
  st: Agent,
  native: TemporalNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    TemporalPlainDateCtor(..)
    | TemporalPlainTimeCtor(..)
    | TemporalPlainDateTimeCtor(..)
    | TemporalPlainYearMonthCtor(..)
    | TemporalPlainMonthDayCtor(..)
    | TemporalDurationCtor(..)
    | TemporalInstantCtor(..)
    | TemporalZonedDateTimeCtor(..) ->
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
    TemporalPlainDateTimeStatic(name:, protos:) ->
      temporal_plain_date_time.static(st, name, protos, args)
    TemporalPlainDateTimeGetter(getter:) ->
      temporal_plain_date_time.getter(st, getter, this)
    TemporalPlainDateTimeMethod(method:, protos:) ->
      temporal_plain_date_time.method(st, method, protos, this, args)
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
    TemporalZonedDateTimeStatic(name:, protos:) ->
      temporal_zoned_date_time.static(st, name, protos, args)
    TemporalZonedDateTimeGetter(getter:) ->
      temporal_zoned_date_time.getter(st, getter, this)
    TemporalZonedDateTimeMethod(method:, protos:) ->
      temporal_zoned_date_time.method(st, method, protos, this, args)
    TemporalNowFn(name:, protos:) -> now_dispatch(st, name, protos, args)
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
      let #(v, st) = instant_from_epoch_ns(st, protos, helpers.arg_at(args, 0))
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

fn require_instant(st: Agent, this: JsVal, name: String) -> Int {
  require_temporal(st, this, "Instant", name, instant_slot_of)
}

fn instant_from_epoch_ns(
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
        terr(st, seconds_string_precision(digits, smallest))
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
          make_instant(st, protos, terr(st, validate_epoch_ns(ns + delta)))
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
          make_instant(st, protos, terr(st, validate_epoch_ns(rounded)))
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
  is_since: Bool,
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
      let smallest_time_unit = terr(st, require_time_unit(smallest))
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
    NowTimeZoneId -> {
      let #(tz, st) = system_time_zone(st)
      #(mk_string(time_zone_id(tz)), st)
    }
    NowPlainDateISO -> {
      let #(tz, st) = now_tz_arg(st, args)
      let #(d, _) = epoch_ns_to_iso_in(tz, now_epoch_ns(st))
      make_date(st, protos, d)
    }
    NowPlainDateTimeISO -> {
      let #(tz, st) = now_tz_arg(st, args)
      let #(d, t) = epoch_ns_to_iso_in(tz, now_epoch_ns(st))
      make_date_time(st, protos, d, t)
    }
    NowPlainTimeISO -> {
      let #(tz, st) = now_tz_arg(st, args)
      let #(_, t) = epoch_ns_to_iso_in(tz, now_epoch_ns(st))
      make_time(st, protos, t)
    }
    NowZonedDateTimeISO -> {
      let #(tz, st) = now_tz_arg(st, args)
      make_zoned(st, protos, now_epoch_ns(st), tz)
    }
  }
}

fn now_tz_arg(st: Agent, args: List(JsVal)) -> #(TimeZone, Agent) {
  let arg = helpers.arg_at(args, 0)
  case classify(arg) {
    KUndef -> system_time_zone(st)
    _ -> to_temporal_time_zone(st, arg)
  }
}
