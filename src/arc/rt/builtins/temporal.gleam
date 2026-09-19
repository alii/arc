import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/builtins/temporal_common.{
  apply_new_target_proto, make_date, make_date_time, make_instant, make_time,
  make_zoned,
}
import arc/rt/builtins/temporal_duration
import arc/rt/builtins/temporal_instant
import arc/rt/builtins/temporal_iso.{ns_per_ms}
import arc/rt/builtins/temporal_plain_date
import arc/rt/builtins/temporal_plain_date_time
import arc/rt/builtins/temporal_plain_month_day
import arc/rt/builtins/temporal_plain_time
import arc/rt/builtins/temporal_plain_year_month
import arc/rt/builtins/temporal_time_zone.{
  epoch_ns_to_iso_in, system_time_zone, time_zone_id, to_temporal_time_zone,
}
import arc/rt/builtins/temporal_zoned_date_time
import arc/rt/store as rt_store
import arc/rt/temporal_data.{type TemporalZone}
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type NativeToken, type TemporalNative,
  type TemporalNowName, type TemporalProtos, InstantAdd, InstantCompare,
  InstantEpochMilliseconds, InstantEpochNanoseconds, InstantEquals, InstantFrom,
  InstantFromEpochMilliseconds, InstantFromEpochNanoseconds, InstantRound,
  InstantSince, InstantSubtract, InstantToJson, InstantToLocaleString,
  InstantToString, InstantToZonedDateTimeIso, InstantUntil, InstantValueOf,
  KUndef, NowInstant, NowPlainDateISO, NowPlainDateTimeISO, NowPlainTimeISO,
  NowTimeZoneId, NowZonedDateTimeISO, TemporalDurationCtor,
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
  TemporalZonedDateTimeStatic, classify, mk_object, mk_string,
}
import arc/rt/val as rt_val
import gleam/dict
import gleam/list
import gleam/option.{Some}

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
    init_type(
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
    init_type(
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
    init_type(
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
    init_type(
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
    init_type(
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
    init_type(
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
    init_type(
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
            temporal_instant.instant_static_name(s.0),
            TemporalN(TemporalInstantStatic(s.0, protos)),
            s.1,
          )
        },
      ),
      list.map([InstantEpochMilliseconds, InstantEpochNanoseconds], fn(g) {
        #(
          temporal_instant.instant_getter_name(g),
          TemporalN(TemporalInstantGetter(g)),
        )
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
            temporal_instant.instant_method_name(m.0),
            TemporalN(TemporalInstantMethod(m.0, protos)),
            m.1,
          )
        },
      ),
    )

  let #(zdt_ctor, st) =
    init_type(
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

  let #(pd_prop, st) = rt_store.builtin_property(st, mk_object(pd_ctor))
  let #(pt_prop, st) = rt_store.builtin_property(st, mk_object(pt_ctor))
  let #(pdt_prop, st) = rt_store.builtin_property(st, mk_object(pdt_ctor))
  let #(pym_prop, st) = rt_store.builtin_property(st, mk_object(pym_ctor))
  let #(pmd_prop, st) = rt_store.builtin_property(st, mk_object(pmd_ctor))
  let #(dur_prop, st) = rt_store.builtin_property(st, mk_object(dur_ctor))
  let #(ins_prop, st) = rt_store.builtin_property(st, mk_object(ins_ctor))
  let #(zdt_prop, st) = rt_store.builtin_property(st, mk_object(zdt_ctor))
  let #(now_prop, st) = rt_store.builtin_property(st, mk_object(now_h))
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

fn init_type(
  st: Agent,
  function_proto: Handle,
  name: String,
  arity: Int,
  proto: Handle,
  ctor_token: NativeToken,
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
      fn(_proto) { ctor_token },
      name,
      arity,
      static_props,
      constructible: True,
    )
  let st = common.add_string_tag(st, proto, "Temporal." <> name)
  #(bt.constructor, st)
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
      rt_val.throw_type_error(st, "Temporal constructor requires new")
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
    TemporalPlainMonthDayStatic(protos:) ->
      temporal_plain_month_day.static(st, protos, args)
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
      temporal_instant.instant_static(st, name, protos, args)
    TemporalInstantGetter(getter:) ->
      temporal_instant.instant_getter(st, getter, this)
    TemporalInstantMethod(method:, protos:) ->
      temporal_instant.instant_method(st, method, protos, this, args)
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
      let #(v, st) =
        temporal_instant.instant_from_epoch_ns(
          st,
          protos,
          helpers.arg_at(args, 0),
        )
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
    _ -> rt_val.throw_type_error(st, "not a constructor")
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

fn now_tz_arg(st: Agent, args: List(JsVal)) -> #(TemporalZone, Agent) {
  let arg = helpers.arg_at(args, 0)
  case classify(arg) {
    KUndef -> system_time_zone(st)
    _ -> to_temporal_time_zone(st, arg)
  }
}
