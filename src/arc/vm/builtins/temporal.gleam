/// Temporal (ECMAScript Temporal proposal, ES2026 draft §21.x / test262
/// feature "Temporal").
///
/// Design notes:
/// - Calendars: the full CLDR set implemented by temporal_calendar (iso8601,
///   gregory, buddhist, japanese, roc, coptic, ethiopic, ethioaa, hebrew,
///   the islamic-* tabular variants, persian, indian, chinese, dangi).
///   Unrecognised identifiers throw RangeError.
/// - Time zones: named IANA zones resolved through the system tzdata (TZif,
///   via arc_tz_ffi in temporal_tz), plus "UTC" and fixed-offset identifiers
///   ("±HH:MM"). Named zones observe real DST transitions; only offset zones
///   are transition-free.
/// - All numeric internals are BEAM Ints (arbitrary precision), so epoch
///   nanoseconds (±8.64e21) and duration totals (≤2^53·1e9) are exact.
///
/// Prior art: tc39/proposal-temporal spec text + reference polyfill;
/// QuickJS has no Temporal, engine262's implementation was consulted for
/// abstract-operation semantics (ToIntegerWithTruncation, RegulateISODate,
/// ISODateTimeWithinLimits, IsValidDuration, ParseISODateTime).
import arc/internal/digits.{take_digits}
import arc/internal/gregorian.{
  days_in_month, days_in_year as days_in_iso_year, floor_div,
  floor_mod as math_mod, is_leap_year,
}
import arc/vm/builtins/common
import arc/vm/builtins/date
import arc/vm/builtins/helpers
import arc/vm/builtins/temporal_tz
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/internal/temporal_calendar as tcal
import arc/vm/key.{Named}
import arc/vm/ops/coerce
import arc/vm/ops/object as ops_object
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type DurationMethod, type InstantMethod, type JsValue, type PlainDateMethod,
  type PlainDateTimeMethod, type PlainMonthDayMethod, type PlainTimeMethod,
  type PlainYearMonthMethod, type Ref, type TemporalDateGetter,
  type TemporalDateTimeGetter, type TemporalDurationGetter, type TemporalGetter,
  type TemporalInstantGetter, type TemporalKind, type TemporalMethodName,
  type TemporalMonthDayGetter, type TemporalNativeFn, type TemporalProtos,
  type TemporalTimeGetter, type TemporalYearMonthGetter,
  type TemporalZonedGetter, type ZonedDateTimeMethod, DgCalendarId, DgDay,
  DgDayOfWeek, DgDayOfYear, DgDaysInMonth, DgDaysInWeek, DgDaysInYear, DgEra,
  DgEraYear, DgInLeapYear, DgMonth, DgMonthCode, DgMonthsInYear, DgWeekOfYear,
  DgYear, DgYearOfWeek, Dispatch, DmAbs, DmAdd, DmNegated, DmRound, DmSubtract,
  DmToJson, DmToLocaleString, DmToString, DmTotal, DmValueOf, DmWith, DrBlank,
  DrDays, DrHours, DrMicroseconds, DrMilliseconds, DrMinutes, DrMonths,
  DrNanoseconds, DrSeconds, DrSign, DrWeeks, DrYears, DtDate, DtTime,
  DurationGetter, DurationMethodName, Finite, ImAdd, ImEquals, ImRound, ImSince,
  ImSubtract, ImToJson, ImToLocaleString, ImToString, ImToZonedDateTimeIso,
  ImUntil, ImValueOf, InEpochMilliseconds, InEpochNanoseconds, InstantGetter,
  InstantMethodName, JsBigInt, JsBool, JsNull, JsNumber, JsObject, JsString,
  JsUndefined, MdCalendarId, MdDay, MdMonthCode, NativeFunction, ObjectSlot,
  OrdinaryObject, PdAdd, PdEquals, PdSince, PdSubtract, PdToJson,
  PdToLocaleString, PdToPlainDateTime, PdToPlainMonthDay, PdToPlainYearMonth,
  PdToString, PdToZonedDateTime, PdUntil, PdValueOf, PdWith, PdWithCalendar,
  PdtAdd, PdtEquals, PdtRound, PdtSince, PdtSubtract, PdtToJson,
  PdtToLocaleString, PdtToPlainDate, PdtToPlainTime, PdtToString,
  PdtToZonedDateTime, PdtUntil, PdtValueOf, PdtWith, PdtWithCalendar,
  PdtWithPlainTime, PlainDateGetter, PlainDateMethodName, PlainDateTimeGetter,
  PlainDateTimeMethodName, PlainMonthDayGetter, PlainMonthDayMethodName,
  PlainTimeGetter, PlainTimeMethodName, PlainYearMonthGetter,
  PlainYearMonthMethodName, PmdEquals, PmdToJson, PmdToLocaleString,
  PmdToPlainDate, PmdToString, PmdValueOf, PmdWith, PtAdd, PtEquals, PtRound,
  PtSince, PtSubtract, PtToJson, PtToLocaleString, PtToString, PtUntil,
  PtValueOf, PtWith, PymAdd, PymEquals, PymSince, PymSubtract, PymToJson,
  PymToLocaleString, PymToPlainDate, PymToString, PymUntil, PymValueOf, PymWith,
  TemporalCtor, TemporalDateSlot, TemporalDateTimeSlot, TemporalDurationKind,
  TemporalDurationSlot, TemporalGetterFn, TemporalInstantKind,
  TemporalInstantSlot, TemporalMethod, TemporalMonthDaySlot, TemporalNative,
  TemporalNowFn, TemporalPlainDateKind, TemporalPlainDateTimeKind,
  TemporalPlainMonthDayKind, TemporalPlainTimeKind, TemporalPlainYearMonthKind,
  TemporalProtos, TemporalStatic, TemporalTimeSlot, TemporalYearMonthSlot,
  TemporalZonedDateTimeKind, TemporalZonedDateTimeSlot, TgHour, TgMicrosecond,
  TgMillisecond, TgMinute, TgNanosecond, TgSecond, YmCalendarId, YmDaysInMonth,
  YmDaysInYear, YmEra, YmEraYear, YmInLeapYear, YmMonth, YmMonthCode,
  YmMonthsInYear, YmYear, ZgDate, ZgEpochMilliseconds, ZgEpochNanoseconds,
  ZgHoursInDay, ZgOffset, ZgOffsetNanoseconds, ZgTime, ZgTimeZoneId, ZmAdd,
  ZmEquals, ZmGetTimeZoneTransition, ZmRound, ZmSince, ZmStartOfDay, ZmSubtract,
  ZmToInstant, ZmToJson, ZmToLocaleString, ZmToPlainDate, ZmToPlainDateTime,
  ZmToPlainTime, ZmToString, ZmUntil, ZmValueOf, ZmWith, ZmWithCalendar,
  ZmWithPlainTime, ZmWithTimeZone, ZonedDateTimeGetter, ZonedDateTimeMethodName,
}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

// ============================================================================
// Constants
// ============================================================================

const ns_per_day = 86_400_000_000_000

const ns_per_hour = 3_600_000_000_000

const ns_per_minute = 60_000_000_000

const ns_per_second = 1_000_000_000

const ns_per_ms = 1_000_000

const ns_per_us = 1000

/// nsMaxInstant = 8.64e21 (±100,000,000 days from epoch).
const ns_max_instant = 8_640_000_000_000_000_000_000

/// Maximum time duration in ns: 2^53 seconds − 1 ns.
const max_time_duration_ns = 9_007_199_254_740_991_999_999_999

/// ISODateWithinLimits bounds expressed in epoch days.
const min_epoch_days = -100_000_001

const max_epoch_days = 100_000_000

// ============================================================================
// Internal records
// ============================================================================

type IsoDate {
  IsoDate(year: Int, month: Int, day: Int)
}

type TimeRec {
  TimeRec(hour: Int, minute: Int, second: Int, ms: Int, us: Int, ns: Int)
}

type DurRec {
  DurRec(
    years: Int,
    months: Int,
    weeks: Int,
    days: Int,
    hours: Int,
    minutes: Int,
    seconds: Int,
    ms: Int,
    us: Int,
    ns: Int,
  )
}

const midnight = TimeRec(0, 0, 0, 0, 0, 0)

const zero_dur = DurRec(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

/// Internal error kind for pure helpers — converted to JS errors at the
/// dispatch boundary.
type TErr {
  RangeE(String)
  TypeE(String)
}

// ============================================================================
// Init — Temporal namespace, 8 types, Temporal.Now
// ============================================================================

/// Build the whole Temporal global. Returns the namespace object ref.
pub fn init(
  h: Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(host), Ref) {
  // Reserve all eight prototypes up front so each native-fn token can carry
  // refs to every sibling prototype.
  let #(h, pd_proto) = reserve_rooted(h)
  let #(h, pt_proto) = reserve_rooted(h)
  let #(h, pdt_proto) = reserve_rooted(h)
  let #(h, pym_proto) = reserve_rooted(h)
  let #(h, pmd_proto) = reserve_rooted(h)
  let #(h, dur_proto) = reserve_rooted(h)
  let #(h, ins_proto) = reserve_rooted(h)
  let #(h, zdt_proto) = reserve_rooted(h)
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

  let #(h, pd_ctor) =
    init_temporal_type(
      h,
      TemporalPlainDateKind,
      protos,
      "PlainDate",
      3,
      pd_proto,
      [#("from", 1), #("compare", 2)],
      list.map(all_date_getters, PlainDateGetter),
      wrap_methods(
        [
          #(PdToPlainYearMonth, 0),
          #(PdToPlainMonthDay, 0),
          #(PdToPlainDateTime, 0),
          #(PdToZonedDateTime, 1),
          #(PdAdd, 1),
          #(PdSubtract, 1),
          #(PdWith, 1),
          #(PdWithCalendar, 1),
          #(PdUntil, 1),
          #(PdSince, 1),
          #(PdEquals, 1),
          #(PdToString, 0),
          #(PdToLocaleString, 0),
          #(PdToJson, 0),
          #(PdValueOf, 0),
        ],
        PlainDateMethodName,
      ),
      object_proto,
      function_proto,
    )

  let #(h, pt_ctor) =
    init_temporal_type(
      h,
      TemporalPlainTimeKind,
      protos,
      "PlainTime",
      0,
      pt_proto,
      [#("from", 1), #("compare", 2)],
      list.map(all_time_getters, PlainTimeGetter),
      wrap_methods(
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
        PlainTimeMethodName,
      ),
      object_proto,
      function_proto,
    )

  let #(h, pdt_ctor) =
    init_temporal_type(
      h,
      TemporalPlainDateTimeKind,
      protos,
      "PlainDateTime",
      3,
      pdt_proto,
      [#("from", 1), #("compare", 2)],
      list.map(
        [
          DtDate(DgCalendarId),
          DtDate(DgEra),
          DtDate(DgEraYear),
          DtDate(DgYear),
          DtDate(DgMonth),
          DtDate(DgMonthCode),
          DtDate(DgDay),
          DtTime(TgHour),
          DtTime(TgMinute),
          DtTime(TgSecond),
          DtTime(TgMillisecond),
          DtTime(TgMicrosecond),
          DtTime(TgNanosecond),
          DtDate(DgDayOfWeek),
          DtDate(DgDayOfYear),
          DtDate(DgWeekOfYear),
          DtDate(DgYearOfWeek),
          DtDate(DgDaysInWeek),
          DtDate(DgDaysInMonth),
          DtDate(DgDaysInYear),
          DtDate(DgMonthsInYear),
          DtDate(DgInLeapYear),
        ],
        PlainDateTimeGetter,
      ),
      wrap_methods(
        [
          #(PdtWith, 1),
          #(PdtWithPlainTime, 0),
          #(PdtWithCalendar, 1),
          #(PdtAdd, 1),
          #(PdtSubtract, 1),
          #(PdtUntil, 1),
          #(PdtSince, 1),
          #(PdtRound, 1),
          #(PdtEquals, 1),
          #(PdtToString, 0),
          #(PdtToLocaleString, 0),
          #(PdtToJson, 0),
          #(PdtValueOf, 0),
          #(PdtToPlainDate, 0),
          #(PdtToPlainTime, 0),
          #(PdtToZonedDateTime, 1),
        ],
        PlainDateTimeMethodName,
      ),
      object_proto,
      function_proto,
    )

  let #(h, pym_ctor) =
    init_temporal_type(
      h,
      TemporalPlainYearMonthKind,
      protos,
      "PlainYearMonth",
      2,
      pym_proto,
      [#("from", 1), #("compare", 2)],
      list.map(
        [
          YmCalendarId,
          YmEra,
          YmEraYear,
          YmYear,
          YmMonth,
          YmMonthCode,
          YmDaysInYear,
          YmDaysInMonth,
          YmMonthsInYear,
          YmInLeapYear,
        ],
        PlainYearMonthGetter,
      ),
      wrap_methods(
        [
          #(PymWith, 1),
          #(PymAdd, 1),
          #(PymSubtract, 1),
          #(PymUntil, 1),
          #(PymSince, 1),
          #(PymEquals, 1),
          #(PymToString, 0),
          #(PymToLocaleString, 0),
          #(PymToJson, 0),
          #(PymValueOf, 0),
          #(PymToPlainDate, 1),
        ],
        PlainYearMonthMethodName,
      ),
      object_proto,
      function_proto,
    )

  let #(h, pmd_ctor) =
    init_temporal_type(
      h,
      TemporalPlainMonthDayKind,
      protos,
      "PlainMonthDay",
      2,
      pmd_proto,
      [#("from", 1)],
      list.map([MdCalendarId, MdMonthCode, MdDay], PlainMonthDayGetter),
      wrap_methods(
        [
          #(PmdWith, 1),
          #(PmdEquals, 1),
          #(PmdToString, 0),
          #(PmdToLocaleString, 0),
          #(PmdToJson, 0),
          #(PmdValueOf, 0),
          #(PmdToPlainDate, 1),
        ],
        PlainMonthDayMethodName,
      ),
      object_proto,
      function_proto,
    )

  let #(h, dur_ctor) =
    init_temporal_type(
      h,
      TemporalDurationKind,
      protos,
      "Duration",
      0,
      dur_proto,
      [#("from", 1), #("compare", 2)],
      list.map(
        [
          DrYears,
          DrMonths,
          DrWeeks,
          DrDays,
          DrHours,
          DrMinutes,
          DrSeconds,
          DrMilliseconds,
          DrMicroseconds,
          DrNanoseconds,
          DrSign,
          DrBlank,
        ],
        DurationGetter,
      ),
      wrap_methods(
        [
          #(DmWith, 1),
          #(DmNegated, 0),
          #(DmAbs, 0),
          #(DmAdd, 1),
          #(DmSubtract, 1),
          #(DmRound, 1),
          #(DmTotal, 1),
          #(DmToString, 0),
          #(DmToJson, 0),
          #(DmToLocaleString, 0),
          #(DmValueOf, 0),
        ],
        DurationMethodName,
      ),
      object_proto,
      function_proto,
    )

  let #(h, ins_ctor) =
    init_temporal_type(
      h,
      TemporalInstantKind,
      protos,
      "Instant",
      1,
      ins_proto,
      [
        #("from", 1),
        #("fromEpochMilliseconds", 1),
        #("fromEpochNanoseconds", 1),
        #("compare", 2),
      ],
      list.map([InEpochMilliseconds, InEpochNanoseconds], InstantGetter),
      wrap_methods(
        [
          #(ImAdd, 1),
          #(ImSubtract, 1),
          #(ImUntil, 1),
          #(ImSince, 1),
          #(ImRound, 1),
          #(ImEquals, 1),
          #(ImToString, 0),
          #(ImToLocaleString, 0),
          #(ImToJson, 0),
          #(ImValueOf, 0),
          #(ImToZonedDateTimeIso, 1),
        ],
        InstantMethodName,
      ),
      object_proto,
      function_proto,
    )

  let #(h, zdt_ctor) =
    init_temporal_type(
      h,
      TemporalZonedDateTimeKind,
      protos,
      "ZonedDateTime",
      2,
      zdt_proto,
      [#("from", 1), #("compare", 2)],
      list.map(
        [
          ZgDate(DgCalendarId),
          ZgTimeZoneId,
          ZgDate(DgEra),
          ZgDate(DgEraYear),
          ZgDate(DgYear),
          ZgDate(DgMonth),
          ZgDate(DgMonthCode),
          ZgDate(DgDay),
          ZgTime(TgHour),
          ZgTime(TgMinute),
          ZgTime(TgSecond),
          ZgTime(TgMillisecond),
          ZgTime(TgMicrosecond),
          ZgTime(TgNanosecond),
          ZgEpochMilliseconds,
          ZgEpochNanoseconds,
          ZgDate(DgDayOfWeek),
          ZgDate(DgDayOfYear),
          ZgDate(DgWeekOfYear),
          ZgDate(DgYearOfWeek),
          ZgHoursInDay,
          ZgDate(DgDaysInWeek),
          ZgDate(DgDaysInMonth),
          ZgDate(DgDaysInYear),
          ZgDate(DgMonthsInYear),
          ZgDate(DgInLeapYear),
          ZgOffsetNanoseconds,
          ZgOffset,
        ],
        ZonedDateTimeGetter,
      ),
      wrap_methods(
        [
          #(ZmWithTimeZone, 1),
          #(ZmWithCalendar, 1),
          #(ZmWithPlainTime, 0),
          #(ZmWith, 1),
          #(ZmAdd, 1),
          #(ZmSubtract, 1),
          #(ZmUntil, 1),
          #(ZmSince, 1),
          #(ZmRound, 1),
          #(ZmEquals, 1),
          #(ZmToString, 0),
          #(ZmToLocaleString, 0),
          #(ZmToJson, 0),
          #(ZmValueOf, 0),
          #(ZmStartOfDay, 0),
          #(ZmGetTimeZoneTransition, 1),
          #(ZmToInstant, 0),
          #(ZmToPlainDate, 0),
          #(ZmToPlainTime, 0),
          #(ZmToPlainDateTime, 0),
        ],
        ZonedDateTimeMethodName,
      ),
      object_proto,
      function_proto,
    )

  // Temporal.Now namespace
  let #(h, now_props) =
    common.alloc_methods(
      h,
      function_proto,
      list.map(
        [
          #("instant", 0),
          #("timeZoneId", 0),
          #("plainDateISO", 0),
          #("plainDateTimeISO", 0),
          #("plainTimeISO", 0),
          #("zonedDateTimeISO", 0),
        ],
        fn(spec) {
          #(spec.0, TemporalNative(TemporalNowFn(spec.0, protos)), spec.1)
        },
      ),
    )
  let #(h, now_ref) =
    common.init_namespace(h, object_proto, "Temporal.Now", now_props)

  // Temporal namespace itself
  let #(h, ns_ref) =
    common.init_namespace(h, object_proto, "Temporal", [
      #("PlainDate", value.builtin_property(JsObject(pd_ctor))),
      #("PlainTime", value.builtin_property(JsObject(pt_ctor))),
      #("PlainDateTime", value.builtin_property(JsObject(pdt_ctor))),
      #("PlainYearMonth", value.builtin_property(JsObject(pym_ctor))),
      #("PlainMonthDay", value.builtin_property(JsObject(pmd_ctor))),
      #("Duration", value.builtin_property(JsObject(dur_ctor))),
      #("Instant", value.builtin_property(JsObject(ins_ctor))),
      #("ZonedDateTime", value.builtin_property(JsObject(zdt_ctor))),
      #("Now", value.builtin_property(JsObject(now_ref))),
    ])
  #(h, ns_ref)
}

fn reserve_rooted(h: Heap(host)) -> #(Heap(host), Ref) {
  let #(h, r) = heap.reserve(h)
  #(heap.root(h, r), r)
}

/// The full date/time getter sets, in prototype-registration order. Kept as
/// values (never strings) so `getter_name` is the only place a JS-facing name
/// is written down.
const all_date_getters = [
  DgCalendarId,
  DgEra,
  DgEraYear,
  DgYear,
  DgMonth,
  DgMonthCode,
  DgDay,
  DgDayOfWeek,
  DgDayOfYear,
  DgWeekOfYear,
  DgYearOfWeek,
  DgDaysInWeek,
  DgDaysInMonth,
  DgDaysInYear,
  DgMonthsInYear,
  DgInLeapYear,
]

const all_time_getters = [
  TgHour,
  TgMinute,
  TgSecond,
  TgMillisecond,
  TgMicrosecond,
  TgNanosecond,
]

/// Tag one Temporal type's `#(method, arity)` pairs with the type they belong to.
fn wrap_methods(
  methods: List(#(a, Int)),
  wrap: fn(a) -> TemporalMethodName,
) -> List(#(TemporalMethodName, Int)) {
  list.map(methods, fn(m) { #(wrap(m.0), m.1) })
}

/// Build one Temporal type: constructor (with statics) + filled prototype
/// (getters, methods, @@toStringTag, constructor backlink).
fn init_temporal_type(
  h: Heap(host),
  kind: TemporalKind,
  protos: TemporalProtos,
  name: String,
  arity: Int,
  proto_ref: Ref,
  statics: List(#(String, Int)),
  getters: List(TemporalGetter),
  methods: List(#(TemporalMethodName, Int)),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(host), Ref) {
  let #(h, static_props) =
    common.alloc_methods(
      h,
      function_proto,
      list.map(statics, fn(s) {
        #(s.0, TemporalNative(TemporalStatic(kind, s.0, protos)), s.1)
      }),
    )
  let #(h, getter_props) =
    common.alloc_getters(
      h,
      function_proto,
      list.map(getters, fn(g) {
        #(getter_name(g), TemporalNative(TemporalGetterFn(g, protos)))
      }),
    )
  let #(h, method_props) =
    common.alloc_methods(
      h,
      function_proto,
      list.map(methods, fn(m) {
        #(method_name(m.0), TemporalNative(TemporalMethod(m.0, protos)), m.1)
      }),
    )
  let #(h, ctor_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(
          Dispatch(TemporalNative(TemporalCtor(kind, protos))),
          constructible: True,
        ),
        properties: common.named_props([
          // §x: constructor "prototype" is non-writable, non-configurable.
          // Constant seq 2 — the statics above already took counter seqs, and
          // "prototype" must enumerate before them.
          #("prototype", common.fn_prototype_property(proto_ref)),
          #("length", common.fn_length_property(arity)),
          #("name", common.fn_name_property(name)),
          ..static_props
        ]),
        elements: elements.new(),
        prototype: Some(function_proto),
        symbol_properties: [],
        extensible: True,
      ),
    )
  let h = heap.root(h, ctor_ref)
  let h =
    heap.write(
      h,
      proto_ref,
      ObjectSlot(
        kind: OrdinaryObject,
        properties: common.named_props([
          #("constructor", value.builtin_property(JsObject(ctor_ref))),
          ..list.append(getter_props, method_props)
        ]),
        elements: elements.new(),
        prototype: Some(object_proto),
        symbol_properties: [common.to_string_tag("Temporal." <> name)],
        extensible: True,
      ),
    )
  #(h, ctor_ref)
}

// ============================================================================
// Dispatch
// ============================================================================

pub fn dispatch(
  native: TemporalNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case native {
    TemporalCtor(kind:, protos:) -> ctor_dispatch(kind, protos, args, state)
    TemporalStatic(kind:, name:, protos:) ->
      static_dispatch(kind, name, protos, args, state)
    TemporalGetterFn(getter:, protos:) ->
      getter_dispatch(getter, protos, this, state)
    TemporalMethod(method:, protos:) ->
      method_dispatch(method, protos, this, args, state)
    TemporalNowFn(name:, protos:) -> now_dispatch(name, protos, args, state)
  }
}

fn ctor_dispatch(
  kind: TemporalKind,
  protos: TemporalProtos,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // All Temporal constructors throw TypeError when called without `new`
  // (NewTarget undefined). do_construct sets state.new_target for natives.
  case state.new_target {
    JsUndefined -> state.type_error(state, "Temporal constructor requires new")
    nt -> {
      let #(state, res) = case kind {
        TemporalPlainDateKind -> plain_date_ctor(protos, args, state)
        TemporalPlainTimeKind -> plain_time_ctor(protos, args, state)
        TemporalPlainDateTimeKind -> plain_date_time_ctor(protos, args, state)
        TemporalPlainYearMonthKind -> plain_year_month_ctor(protos, args, state)
        TemporalPlainMonthDayKind -> plain_month_day_ctor(protos, args, state)
        TemporalDurationKind -> duration_ctor(protos, args, state)
        TemporalInstantKind -> instant_ctor(protos, args, state)
        TemporalZonedDateTimeKind -> zoned_date_time_ctor(protos, args, state)
      }
      case res {
        Error(e) -> #(state, Error(e))
        Ok(v) -> apply_new_target_proto(state, nt, v)
      }
    }
  }
}

/// GetPrototypeFromConstructor (via OrdinaryCreateFromConstructor): read
/// newTarget.prototype — an observable Get that may throw — and re-point the
/// created object at it when it is an object. The intrinsic default proto
/// stays when newTarget.prototype is not an object.
fn apply_new_target_proto(
  state: State(host),
  nt: JsValue,
  v: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  case nt, v {
    JsObject(nt_ref), JsObject(obj_ref) ->
      case ops_object.get_value(state, nt_ref, Named("prototype"), nt) {
        Error(#(thrown, state)) -> #(state, Error(thrown))
        Ok(#(proto_v, state)) -> {
          let state = case proto_v {
            JsObject(proto_ref) ->
              case heap.read(state.heap, obj_ref) {
                Some(ObjectSlot(..) as slot) ->
                  State(
                    ..state,
                    heap: heap.write(
                      state.heap,
                      obj_ref,
                      ObjectSlot(..slot, prototype: Some(proto_ref)),
                    ),
                  )
                _ -> state
              }
            _ -> state
          }
          #(state, Ok(v))
        }
      }
    _, _ -> #(state, Ok(v))
  }
}

// ============================================================================
// Pure calendar math (ISO 8601 proleptic Gregorian)
// ============================================================================

/// Days since epoch to Jan 1 of year y.
fn days_from_year(y: Int) -> Int {
  365
  * { y - 1970 }
  + floor_div(y - 1969, 4)
  - floor_div(y - 1901, 100)
  + floor_div(y - 1601, 400)
}

fn days_before_month(y: Int, m: Int) -> Int {
  sum_months(y, 1, m, 0)
}

fn sum_months(y: Int, i: Int, until: Int, acc: Int) -> Int {
  case i >= until {
    True -> acc
    False -> sum_months(y, i + 1, until, acc + days_in_month(y, i))
  }
}

fn epoch_days(d: IsoDate) -> Int {
  days_from_year(d.year) + days_before_month(d.year, d.month) + d.day - 1
}

fn year_from_days(days: Int) -> #(Int, Int) {
  let y = floor_div(days * 10_000, 3_652_425) + 1970
  year_from_days_loop(y, days)
}

fn year_from_days_loop(y: Int, days: Int) -> #(Int, Int) {
  let d = days - days_from_year(y)
  case d < 0 {
    True -> year_from_days_loop(y - 1, days)
    False ->
      case d >= days_in_iso_year(y) {
        True -> year_from_days_loop(y + 1, days)
        False -> #(y, d)
      }
  }
}

fn iso_date_from_epoch_days(days: Int) -> IsoDate {
  let #(year, day_in_year) = year_from_days(days)
  let #(month, day) = month_day_from_doy(year, day_in_year, 1)
  IsoDate(year:, month:, day:)
}

fn month_day_from_doy(y: Int, d: Int, m: Int) -> #(Int, Int) {
  let md = days_in_month(y, m)
  case d < md {
    True -> #(m, d + 1)
    False -> month_day_from_doy(y, d - md, m + 1)
  }
}

/// ISO day of week: Monday = 1 .. Sunday = 7. Epoch day 0 = Thursday.
fn day_of_week(d: IsoDate) -> Int {
  gregorian.iso_weekday_from_days(epoch_days(d))
}

fn day_of_year(d: IsoDate) -> Int {
  days_before_month(d.year, d.month) + d.day
}

/// ISO 8601 week number + week-calendar year.
fn week_of_year(d: IsoDate) -> #(Int, Int) {
  let doy = day_of_year(d)
  let dow = day_of_week(d)
  let week = { doy - dow + 10 } / 7
  case week < 1 {
    True -> {
      // Belongs to the last week of the previous year.
      let py = d.year - 1
      let pdoy = doy + days_in_iso_year(py)
      #({ pdoy - dow + 10 } / 7, py)
    }
    False -> {
      let weeks_in_year = case { days_in_iso_year(d.year) - doy < 4 - dow } {
        True -> {
          // Might belong to week 1 of next year.
          let nyd = doy - days_in_iso_year(d.year)
          let nweek = { nyd - dow + 10 } / 7
          case nweek >= 1 {
            True -> -1
            False -> week
          }
        }
        False -> week
      }
      case weeks_in_year == -1 {
        True -> #(1, d.year + 1)
        False -> #(week, d.year)
      }
    }
  }
}

fn is_valid_iso_date(y: Int, m: Int, d: Int) -> Bool {
  m >= 1 && m <= 12 && d >= 1 && d <= days_in_month(y, m)
}

fn iso_date_within_limits(d: IsoDate) -> Bool {
  let ed = epoch_days(d)
  ed >= min_epoch_days && ed <= max_epoch_days
}

fn iso_datetime_within_limits(d: IsoDate, t: TimeRec) -> Bool {
  let ns = epoch_days(d) * ns_per_day + time_to_ns(t)
  ns > { 0 - ns_max_instant } - ns_per_day && ns < ns_max_instant + ns_per_day
}

fn iso_year_month_within_limits(y: Int, m: Int) -> Bool {
  case y {
    -271_821 -> m >= 4
    275_760 -> m <= 9
    _ -> y > -271_821 && y < 275_760
  }
}

fn time_to_ns(t: TimeRec) -> Int {
  t.hour
  * ns_per_hour
  + t.minute
  * ns_per_minute
  + t.second
  * ns_per_second
  + t.ms
  * ns_per_ms
  + t.us
  * ns_per_us
  + t.ns
}

/// Decompose non-negative day-local nanoseconds into a TimeRec.
fn ns_to_time(total: Int) -> TimeRec {
  let hour = total / ns_per_hour
  let rem = total - hour * ns_per_hour
  let minute = rem / ns_per_minute
  let rem = rem - minute * ns_per_minute
  let second = rem / ns_per_second
  let rem = rem - second * ns_per_second
  let ms = rem / ns_per_ms
  let rem = rem - ms * ns_per_ms
  let us = rem / ns_per_us
  let ns = rem - us * ns_per_us
  TimeRec(hour:, minute:, second:, ms:, us:, ns:)
}

fn is_valid_time(t: TimeRec) -> Bool {
  t.hour >= 0
  && t.hour <= 23
  && t.minute >= 0
  && t.minute <= 59
  && t.second >= 0
  && t.second <= 59
  && t.ms >= 0
  && t.ms <= 999
  && t.us >= 0
  && t.us <= 999
  && t.ns >= 0
  && t.ns <= 999
}

/// RegulateISODate. Month must already be ≥ 1 when called from property
/// bags (ToPositiveIntegerWithTruncation).
fn regulate_iso_date(
  y: Int,
  m: Int,
  d: Int,
  overflow: Overflow,
) -> Result(IsoDate, TErr) {
  case overflow {
    Reject ->
      case is_valid_iso_date(y, m, d) {
        True -> Ok(IsoDate(y, m, d))
        False -> Error(RangeE("invalid ISO date"))
      }
    Constrain -> {
      let m = int.clamp(m, 1, 12)
      let d = int.clamp(d, 1, days_in_month(y, m))
      Ok(IsoDate(y, m, d))
    }
  }
}

fn check_date_limits(d: IsoDate) -> Result(IsoDate, TErr) {
  case iso_date_within_limits(d) {
    True -> Ok(d)
    False -> Error(RangeE("date outside of supported range"))
  }
}

/// Epoch nanoseconds for an ISO date+time interpreted as UTC.
fn utc_epoch_ns(d: IsoDate, t: TimeRec) -> Int {
  epoch_days(d) * ns_per_day + time_to_ns(t)
}

/// Split epoch nanoseconds (+ offset) into date and time.
fn epoch_ns_to_iso(epoch_ns: Int, offset_ns: Int) -> #(IsoDate, TimeRec) {
  let local = epoch_ns + offset_ns
  let days = floor_div(local, ns_per_day)
  let rem = local - days * ns_per_day
  #(iso_date_from_epoch_days(days), ns_to_time(rem))
}

// ============================================================================
// Formatting
// ============================================================================

fn pad2(n: Int) -> String {
  int.to_string(int.absolute_value(n)) |> string.pad_start(2, "0")
}

fn format_iso_year(y: Int) -> String {
  case y >= 0 && y <= 9999 {
    True -> int.to_string(y) |> string.pad_start(4, "0")
    False -> {
      let sign = case y < 0 {
        True -> "-"
        False -> "+"
      }
      sign
      <> { int.to_string(int.absolute_value(y)) |> string.pad_start(6, "0") }
    }
  }
}

fn format_iso_date(d: IsoDate) -> String {
  format_iso_year(d.year) <> "-" <> pad2(d.month) <> "-" <> pad2(d.day)
}

/// Format sub-second part per `precision`: "auto" trims trailing zeros,
/// an Int 0..9 forces that many digits. Returns "" or ".ddd...".
fn format_fraction(sub_ns: Int, precision: Precision) -> String {
  let digits9 = int.to_string(sub_ns) |> string.pad_start(9, "0")
  case precision {
    AutoPrec ->
      case sub_ns == 0 {
        True -> ""
        False -> "." <> trim_trailing_zeros(digits9)
      }
    FixedPrec(0) -> ""
    FixedPrec(n) -> "." <> string.slice(digits9, 0, n)
    MinutePrec -> ""
  }
}

fn trim_trailing_zeros(s: String) -> String {
  case string.ends_with(s, "0") {
    True -> trim_trailing_zeros(string.drop_end(s, 1))
    False -> s
  }
}

type Precision {
  AutoPrec
  FixedPrec(Int)
  MinutePrec
}

fn format_iso_time(t: TimeRec, precision: Precision) -> String {
  let sub = t.ms * ns_per_ms + t.us * ns_per_us + t.ns
  let base = pad2(t.hour) <> ":" <> pad2(t.minute)
  case precision {
    MinutePrec -> base
    _ -> base <> ":" <> pad2(t.second) <> format_fraction(sub, precision)
  }
}

/// Format a UTC offset from nanoseconds, minute precision ("+05:30").
fn format_offset_minutes(offset_ns: Int) -> String {
  let sign = case offset_ns < 0 {
    True -> "-"
    False -> "+"
  }
  let a = int.absolute_value(offset_ns)
  let total_minutes = a / ns_per_minute
  sign <> pad2(total_minutes / 60) <> ":" <> pad2(total_minutes % 60)
}

// ============================================================================
// Branding helpers — extract internal slots from `this`
// ============================================================================

fn read_kind(
  state: State(host),
  this: JsValue,
) -> Option(value.ExoticKind(State(host), host)) {
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind:, ..)) -> Some(kind)
        _ -> None
      }
    _ -> None
  }
}

/// Every calendared receiver hands its calendar back with its date (as
/// `this_zoned` already did), so a caller that has the date always has the
/// calendar the date is expressed in — there is no separate lookup that could
/// miss and fall back to iso8601.
fn this_date(
  state: State(host),
  this: JsValue,
) -> Option(#(IsoDate, tcal.Calendar)) {
  case read_kind(state, this) {
    Some(TemporalDateSlot(year:, month:, day:, calendar:)) ->
      Some(#(IsoDate(year:, month:, day:), calendar))
    _ -> None
  }
}

fn this_time(state: State(host), this: JsValue) -> Option(TimeRec) {
  case read_kind(state, this) {
    Some(TemporalTimeSlot(
      hour:,
      minute:,
      second:,
      millisecond:,
      microsecond:,
      nanosecond:,
    )) ->
      Some(TimeRec(hour, minute, second, millisecond, microsecond, nanosecond))
    _ -> None
  }
}

fn this_date_time(
  state: State(host),
  this: JsValue,
) -> Option(#(IsoDate, TimeRec, tcal.Calendar)) {
  case read_kind(state, this) {
    Some(TemporalDateTimeSlot(
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
        TimeRec(hour, minute, second, millisecond, microsecond, nanosecond),
        calendar,
      ))
    _ -> None
  }
}

fn this_year_month(
  state: State(host),
  this: JsValue,
) -> Option(#(Int, Int, Int, tcal.Calendar)) {
  case read_kind(state, this) {
    Some(TemporalYearMonthSlot(year:, month:, day:, calendar:)) ->
      Some(#(year, month, day, calendar))
    _ -> None
  }
}

fn this_month_day(
  state: State(host),
  this: JsValue,
) -> Option(#(Int, Int, Int, tcal.Calendar)) {
  case read_kind(state, this) {
    Some(TemporalMonthDaySlot(month:, day:, ref_year:, calendar:)) ->
      Some(#(month, day, ref_year, calendar))
    _ -> None
  }
}

fn this_duration(state: State(host), this: JsValue) -> Option(DurRec) {
  case read_kind(state, this) {
    Some(TemporalDurationSlot(
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
      Some(DurRec(
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

fn this_instant(state: State(host), this: JsValue) -> Option(Int) {
  case read_kind(state, this) {
    Some(TemporalInstantSlot(epoch_ns:)) -> Some(epoch_ns)
    _ -> None
  }
}

fn this_zoned(
  state: State(host),
  this: JsValue,
) -> Option(#(Int, String, tcal.Calendar)) {
  case read_kind(state, this) {
    Some(TemporalZonedDateTimeSlot(epoch_ns:, time_zone:, calendar:)) ->
      Some(#(epoch_ns, time_zone, calendar))
    _ -> None
  }
}

fn brand_error(
  state: State(host),
  type_name: String,
  name: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  state.type_error(
    state,
    "Temporal."
      <> type_name
      <> ".prototype."
      <> name
      <> " called on incompatible receiver",
  )
}

// ============================================================================
// Allocation helpers
// ============================================================================

fn alloc_value(
  state: State(host),
  kind: value.ExoticKind(State(host), host),
  proto: Ref,
) -> #(State(host), JsValue) {
  let #(h, ref) = common.alloc_wrapper(state.heap, kind, proto)
  #(State(..state, heap: h), JsObject(ref))
}

fn make_date(
  state: State(host),
  protos: TemporalProtos,
  d: IsoDate,
) -> #(State(host), JsValue) {
  make_date_cal(state, protos, d, tcal.Iso8601)
}

fn make_date_cal(
  state: State(host),
  protos: TemporalProtos,
  d: IsoDate,
  cal: tcal.Calendar,
) -> #(State(host), JsValue) {
  alloc_value(
    state,
    TemporalDateSlot(year: d.year, month: d.month, day: d.day, calendar: cal),
    protos.plain_date,
  )
}

fn make_time(
  state: State(host),
  protos: TemporalProtos,
  t: TimeRec,
) -> #(State(host), JsValue) {
  alloc_value(
    state,
    TemporalTimeSlot(
      hour: t.hour,
      minute: t.minute,
      second: t.second,
      millisecond: t.ms,
      microsecond: t.us,
      nanosecond: t.ns,
    ),
    protos.plain_time,
  )
}

fn make_date_time(
  state: State(host),
  protos: TemporalProtos,
  d: IsoDate,
  t: TimeRec,
) -> #(State(host), JsValue) {
  make_date_time_cal(state, protos, d, t, tcal.Iso8601)
}

fn make_date_time_cal(
  state: State(host),
  protos: TemporalProtos,
  d: IsoDate,
  t: TimeRec,
  cal: tcal.Calendar,
) -> #(State(host), JsValue) {
  alloc_value(
    state,
    TemporalDateTimeSlot(
      year: d.year,
      month: d.month,
      day: d.day,
      hour: t.hour,
      minute: t.minute,
      second: t.second,
      millisecond: t.ms,
      microsecond: t.us,
      nanosecond: t.ns,
      calendar: cal,
    ),
    protos.plain_date_time,
  )
}

fn make_year_month(
  state: State(host),
  protos: TemporalProtos,
  y: Int,
  m: Int,
  ref_day: Int,
) -> #(State(host), JsValue) {
  make_year_month_cal(state, protos, y, m, ref_day, tcal.Iso8601)
}

fn make_year_month_cal(
  state: State(host),
  protos: TemporalProtos,
  y: Int,
  m: Int,
  ref_day: Int,
  cal: tcal.Calendar,
) -> #(State(host), JsValue) {
  alloc_value(
    state,
    TemporalYearMonthSlot(year: y, month: m, day: ref_day, calendar: cal),
    protos.plain_year_month,
  )
}

fn make_month_day_cal(
  state: State(host),
  protos: TemporalProtos,
  m: Int,
  d: Int,
  ref_year: Int,
  cal: tcal.Calendar,
) -> #(State(host), JsValue) {
  alloc_value(
    state,
    TemporalMonthDaySlot(month: m, day: d, ref_year: ref_year, calendar: cal),
    protos.plain_month_day,
  )
}

/// Each Duration field is a float64 Number in the spec (CreateTemporalDuration
/// stores ℝ(𝔽(v))), so huge components lose precision on construction.
/// Rounded in integer space via scale_ratio (ties-to-even) because erlang's
/// float/1 mis-rounds integers wider than 53 bits.
fn f64_int(n: Int) -> Int {
  case int.absolute_value(n) < two53 {
    True -> n
    False -> {
      let #(m, s) = scale_ratio(int.absolute_value(n), 1, 0)
      let v = int.bitwise_shift_left(m, s)
      case n < 0 {
        True -> 0 - v
        False -> v
      }
    }
  }
}

fn make_duration(
  state: State(host),
  protos: TemporalProtos,
  dur: DurRec,
) -> #(State(host), JsValue) {
  alloc_value(
    state,
    TemporalDurationSlot(
      years: f64_int(dur.years),
      months: f64_int(dur.months),
      weeks: f64_int(dur.weeks),
      days: f64_int(dur.days),
      hours: f64_int(dur.hours),
      minutes: f64_int(dur.minutes),
      seconds: f64_int(dur.seconds),
      milliseconds: f64_int(dur.ms),
      microseconds: f64_int(dur.us),
      nanoseconds: f64_int(dur.ns),
    ),
    protos.duration,
  )
}

/// Validate then allocate a Temporal.Duration, or throw a RangeError.
fn finish_duration(
  state: State(host),
  protos: TemporalProtos,
  dur: DurRec,
) -> #(State(host), Result(JsValue, JsValue)) {
  case is_valid_duration(dur) {
    False -> state.range_error(state, "invalid duration")
    True -> {
      let #(state, v) = make_duration(state, protos, dur)
      #(state, Ok(v))
    }
  }
}

fn make_instant(
  state: State(host),
  protos: TemporalProtos,
  ns: Int,
) -> #(State(host), JsValue) {
  alloc_value(state, TemporalInstantSlot(epoch_ns: ns), protos.instant)
}

fn make_zoned(
  state: State(host),
  protos: TemporalProtos,
  ns: Int,
  tz: String,
) -> #(State(host), JsValue) {
  make_zoned_cal(state, protos, ns, tz, tcal.Iso8601)
}

fn make_zoned_cal(
  state: State(host),
  protos: TemporalProtos,
  ns: Int,
  tz: String,
  cal: tcal.Calendar,
) -> #(State(host), JsValue) {
  alloc_value(
    state,
    TemporalZonedDateTimeSlot(epoch_ns: ns, time_zone: tz, calendar: cal),
    protos.zoned_date_time,
  )
}

// ============================================================================
// Coercion helpers
// ============================================================================

fn throw_terr(
  state: State(host),
  e: TErr,
) -> #(State(host), Result(JsValue, JsValue)) {
  case e {
    RangeE(m) -> state.range_error(state, m)
    TypeE(m) -> state.type_error(state, m)
  }
}

/// CPS adapter: run a pure Result(a, TErr) op, throwing on Error.
fn terr(
  state: State(host),
  r: Result(a, TErr),
  k: fn(a) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case r {
    Ok(v) -> k(v)
    Error(e) -> throw_terr(state, e)
  }
}

/// ToIntegerWithTruncation: ToNumber, RangeError on NaN/±∞, truncate.
fn to_integer_with_truncation(
  state: State(host),
  v: JsValue,
) -> Result(#(Int, State(host)), #(JsValue, State(host))) {
  use num <- result.try(coerce.js_to_number(state, v))
  case num {
    #(Finite(f), st) -> Ok(#(value.float_to_int(f), st))
    #(_, st) -> range_error_result(st, "not a finite number")
  }
}

/// ToPositiveIntegerWithTruncation: like above but must be > 0.
fn to_positive_integer_with_truncation(
  state: State(host),
  v: JsValue,
) -> Result(#(Int, State(host)), #(JsValue, State(host))) {
  use #(n, st) <- result.try(to_integer_with_truncation(state, v))
  case n > 0 {
    True -> Ok(#(n, st))
    False -> range_error_result(st, "expected a positive integer")
  }
}

/// ToIntegerIfIntegral: ToNumber, RangeError unless an integral Number.
fn to_integer_if_integral(
  state: State(host),
  v: JsValue,
) -> Result(#(Int, State(host)), #(JsValue, State(host))) {
  use num <- result.try(coerce.js_to_number(state, v))
  case num {
    #(Finite(f), st) -> {
      let i = value.float_to_int(f)
      let fi = int.to_float(i)
      // Arithmetic comparison: `==` is term equality, where -0.0 ≠ 0.0.
      case f >=. fi && f <=. fi {
        True -> Ok(#(i, st))
        False -> range_error_result(st, "expected an integral number")
      }
    }
    #(_, st) -> range_error_result(st, "expected an integral number")
  }
}

/// A RangeError as an ops-level `Error(#(thrown, state))`. `range_error_value`
/// always yields the thrown value, so there is no "no error was allocated"
/// arm to invent behaviour for.
fn range_error_result(
  st: State(host),
  msg: String,
) -> Result(#(a, State(host)), #(JsValue, State(host))) {
  Error(state.range_error_value(st, msg))
}

/// A TypeError as an ops-level `Error(#(thrown, state))`.
fn type_error_result(
  st: State(host),
  msg: String,
) -> Result(#(a, State(host)), #(JsValue, State(host))) {
  Error(state.type_error_value(st, msg))
}

/// The one place a `TErr` becomes an ops-level thrown JS error, mirroring
/// `throw_terr` for the `#(State, Result(..))` boundary. A new `TErr` variant
/// breaks here rather than at every op that can raise one.
fn throw_terr_op(
  state: State(host),
  e: TErr,
) -> Result(#(a, State(host)), #(JsValue, State(host))) {
  case e {
    RangeE(m) -> range_error_result(state, m)
    TypeE(m) -> type_error_result(state, m)
  }
}

/// Adapter: a pure `Result(a, TErr)` op as an ops-level result, throwing on
/// Error. Use `terr_r` instead when the success value feeds more work.
fn terr_op(
  state: State(host),
  r: Result(a, TErr),
) -> Result(#(a, State(host)), #(JsValue, State(host))) {
  case r {
    Ok(v) -> Ok(#(v, state))
    Error(e) -> throw_terr_op(state, e)
  }
}

// ============================================================================
// Options handling
// ============================================================================

/// GetOptionsObject: undefined → None, object → Some(ref), else TypeError.
fn get_options_object(
  state: State(host),
  v: JsValue,
) -> Result(#(Option(Ref), State(host)), #(JsValue, State(host))) {
  case v {
    JsUndefined -> Ok(#(None, state))
    JsObject(ref) -> Ok(#(Some(ref), state))
    _ -> type_error_result(state, "options must be an object or undefined")
  }
}

/// GetOption for an enum-valued option: `allowed` maps each accepted string
/// to its variant, so the allow-list and the parse are the same table and no
/// consumer ever sees the raw string. Anything else is a RangeError here.
fn get_enum_option(
  state: State(host),
  opts: Option(Ref),
  key: String,
  allowed: List(#(String, a)),
  default: a,
) -> Result(#(a, State(host)), #(JsValue, State(host))) {
  case opts {
    None -> Ok(#(default, state))
    Some(ref) -> {
      use got <- result.try(ops_object.get_value(
        state,
        ref,
        Named(key),
        JsObject(ref),
      ))
      case got {
        #(JsUndefined, st) -> Ok(#(default, st))
        #(v, st) -> {
          use #(s, st) <- result.try(coerce.js_to_string(st, v))
          case list.key_find(allowed, s) {
            Ok(parsed) -> Ok(#(parsed, st))
            Error(Nil) ->
              range_error_result(
                st,
                s <> " is not a valid value for option " <> key,
              )
          }
        }
      }
    }
  }
}

/// overflow option: how out-of-range calendar/time fields are handled.
pub type Overflow {
  Constrain
  Reject
}

/// disambiguation option: which instant an ambiguous or skipped wall-clock
/// time resolves to.
pub type Disambiguation {
  Compatible
  Earlier
  Later
  RejectDisambiguation
}

/// offset option: how a ZonedDateTime's offset field is reconciled with its
/// time zone.
pub type OffsetOption {
  PreferOffset
  UseOffset
  IgnoreOffset
  RejectOffset
}

/// GetTemporalOverflowOption: "constrain" (default) or "reject".
fn get_overflow_option(
  state: State(host),
  opts: Option(Ref),
) -> Result(#(Overflow, State(host)), #(JsValue, State(host))) {
  get_enum_option(
    state,
    opts,
    "overflow",
    [#("constrain", Constrain), #("reject", Reject)],
    Constrain,
  )
}

/// GetTemporalDisambiguationOption: "compatible" (default) | "earlier" |
/// "later" | "reject".
fn get_disambiguation_option(
  state: State(host),
  opts: Option(Ref),
) -> Result(#(Disambiguation, State(host)), #(JsValue, State(host))) {
  get_enum_option(
    state,
    opts,
    "disambiguation",
    [
      #("compatible", Compatible),
      #("earlier", Earlier),
      #("later", Later),
      #("reject", RejectDisambiguation),
    ],
    Compatible,
  )
}

/// GetTemporalOffsetOption: "prefer" | "use" | "ignore" | "reject", with a
/// per-caller default.
fn get_offset_option(
  state: State(host),
  opts: Option(Ref),
  default: OffsetOption,
) -> Result(#(OffsetOption, State(host)), #(JsValue, State(host))) {
  get_enum_option(
    state,
    opts,
    "offset",
    [
      #("prefer", PreferOffset),
      #("use", UseOffset),
      #("ignore", IgnoreOffset),
      #("reject", RejectOffset),
    ],
    default,
  )
}

/// calendarName option: whether/how the [u-ca=] annotation is emitted.
pub type CalendarNameMode {
  CalAuto
  CalAlways
  CalNever
  CalCritical
}

/// offset option of ZonedDateTime.toString: whether the numeric UTC offset is
/// emitted. (Distinct from `OffsetOption`, which reconciles a *parsed* offset
/// against the zone.)
pub type ShowOffset {
  OffsetShowAuto
  OffsetShowNever
}

/// timeZoneName option: whether/how the [tz] annotation is emitted.
pub type TimeZoneNameMode {
  TzAuto
  TzNever
  TzCritical
}

/// direction argument of ZonedDateTime.getTimeZoneTransition.
pub type TransitionDirection {
  Next
  Previous
}

/// GetOptionsObject + GetTemporalShowCalendarNameOption: reads the options
/// argument and its "calendarName" option (`CalAuto` default). Also returns
/// the options object for callers that read further options from it.
fn get_calendar_name_option(
  state: State(host),
  options_arg: JsValue,
) -> Result(
  #(#(CalendarNameMode, Option(Ref)), State(host)),
  #(JsValue, State(host)),
) {
  use #(opts, state) <- result.try(get_options_object(state, options_arg))
  use #(cal_name, state) <- result.map(get_enum_option(
    state,
    opts,
    "calendarName",
    [
      #("auto", CalAuto),
      #("always", CalAlways),
      #("never", CalNever),
      #("critical", CalCritical),
    ],
    CalAuto,
  ))
  #(#(cal_name, opts), state)
}

/// GetTemporalShowOffsetOption: "auto" (default) or "never".
fn get_show_offset_option(
  state: State(host),
  opts: Option(Ref),
) -> Result(#(ShowOffset, State(host)), #(JsValue, State(host))) {
  get_enum_option(
    state,
    opts,
    "offset",
    [#("auto", OffsetShowAuto), #("never", OffsetShowNever)],
    OffsetShowAuto,
  )
}

/// GetTemporalShowTimeZoneNameOption: "auto" (default) | "never" | "critical".
fn get_time_zone_name_option(
  state: State(host),
  opts: Option(Ref),
) -> Result(#(TimeZoneNameMode, State(host)), #(JsValue, State(host))) {
  get_enum_option(
    state,
    opts,
    "timeZoneName",
    [#("auto", TzAuto), #("never", TzNever), #("critical", TzCritical)],
    TzAuto,
  )
}

// ============================================================================
// Calendar handling (delegates to temporal_calendar's CLDR calendar set)
// ============================================================================

/// ToTemporalCalendarIdentifier / CanonicalizeCalendar for a bare identifier:
/// case-insensitive lookup + alias resolution via tcal.canonicalize.
/// RangeError for identifiers outside temporal_calendar's supported set.
fn canonicalize_calendar(id: String) -> Result(tcal.Calendar, TErr) {
  case tcal.canonicalize(id) {
    Ok(c) -> Ok(c)
    Error(Nil) -> Error(RangeE("calendar " <> id <> " is not supported"))
  }
}

/// ToTemporalCalendarIdentifier for string inputs: either a bare calendar id,
/// or an ISO date/date-time/year-month/month-day/time string whose [u-ca=]
/// annotation (default iso8601) supplies the calendar.
fn calendar_from_string(s: String) -> Result(tcal.Calendar, TErr) {
  case canonicalize_calendar(s) {
    Ok(c) -> Ok(c)
    Error(e) ->
      case extract_calendar_annotation(s) {
        Some(cal) -> canonicalize_calendar(cal)
        None -> Error(e)
      }
  }
}

/// If `s` parses as some ISO 8601 Temporal string, return its calendar
/// annotation value (or "iso8601" when absent). None when not parseable.
fn extract_calendar_annotation(s: String) -> Option(String) {
  case parse_iso_datetime_string(s) {
    Some(p) -> Some(option.unwrap(p.calendar, "iso8601"))
    None -> {
      // Try time-only / year-month / month-day forms.
      let body = case s {
        "T" <> r | "t" <> r -> r
        _ -> s
      }
      case parse_time_part(body) {
        Some(#(_, rest)) -> {
          let rest = case parse_offset_part(rest) {
            Some(#(False, _, _, r)) -> r
            _ -> rest
          }
          case parse_annotations(rest, None, None, False) {
            Some(#(_, cal, "")) -> Some(option.unwrap(cal, "iso8601"))
            _ -> try_ym_md_calendar(s)
          }
        }
        None -> try_ym_md_calendar(s)
      }
    }
  }
}

fn try_ym_md_calendar(s: String) -> Option(String) {
  case
    result.is_ok(parse_year_month_string(s))
    || result.is_ok(parse_month_day_string(s))
  {
    True -> Some("iso8601")
    False -> None
  }
}

/// Calendar argument of constructors: undefined → iso8601; string → must be
/// supported; anything else → TypeError.
fn to_calendar_arg(v: JsValue) -> Result(tcal.Calendar, TErr) {
  case v {
    JsUndefined -> Ok(tcal.Iso8601)
    JsString(s) -> canonicalize_calendar(s)
    _ -> Error(TypeE("calendar must be a string"))
  }
}

// ============================================================================
// ISO 8601 string parsing
// ============================================================================

type ParsedIso {
  ParsedIso(
    date: Option(IsoDate),
    time: Option(TimeRec),
    /// UTC designator present (Z)
    z: Bool,
    /// explicit numeric offset, in nanoseconds
    offset_ns: Option(Int),
    /// the explicit offset spelled a seconds component (sub-minute syntax)
    offset_sub_minute: Bool,
    /// time zone annotation [Etc/UTC] / [+01:00]
    tz: Option(String),
    /// calendar annotation value
    calendar: Option(String),
  )
}

/// Take up to `max` digits (at least 1), returning value, count, rest.
fn take_some_digits(s: String, max: Int) -> Option(#(Int, Int, String)) {
  take_some_digits_loop(s, max, 0, 0)
}

fn take_some_digits_loop(
  s: String,
  max: Int,
  acc: Int,
  count: Int,
) -> Option(#(Int, Int, String)) {
  case max == 0 {
    True -> Some(#(acc, count, s))
    False ->
      case string.pop_grapheme(s) {
        Ok(#(c, rest)) ->
          case digits.digit_value(c) {
            Some(d) ->
              take_some_digits_loop(rest, max - 1, acc * 10 + d, count + 1)
            None ->
              case count > 0 {
                True -> Some(#(acc, count, s))
                False -> None
              }
          }
        Error(Nil) ->
          case count > 0 {
            True -> Some(#(acc, count, s))
            False -> None
          }
      }
  }
}

/// Parse the date part: ±YYYYYY-MM-DD / ±YYYYYYMMDD / YYYY-MM-DD / YYYYMMDD.
/// Returns y/m/d unvalidated (range-checked by caller) + rest.
fn parse_date_part(s: String) -> Option(#(Int, Int, Int, String)) {
  use #(year, rest) <- option.then(parse_year_part(s))
  case rest {
    "-" <> r1 -> {
      use #(m, r2) <- option.then(take_digits(r1, 2))
      case r2 {
        "-" <> r3 -> {
          use #(d, r4) <- option.then(take_digits(r3, 2))
          Some(#(year, m, d, r4))
        }
        _ -> None
      }
    }
    _ -> {
      use #(m, r2) <- option.then(take_digits(rest, 2))
      use #(d, r3) <- option.then(take_digits(r2, 2))
      Some(#(year, m, d, r3))
    }
  }
}

/// Year: 4 digits, or sign + 6 digits. "-000000" is rejected.
fn parse_year_part(s: String) -> Option(#(Int, String)) {
  case s {
    "+" <> rest -> take_digits(rest, 6) |> option.map(fn(p) { #(p.0, p.1) })
    "-" <> rest ->
      case take_digits(rest, 6) {
        Some(#(0, _)) -> None
        Some(#(y, r)) -> Some(#(0 - y, r))
        None -> None
      }
    // U+2212 MINUS SIGN is NOT valid in ISO 8601 strings (only ASCII hyphen).
    _ -> take_digits(s, 4)
  }
}

/// Time: HH[:MM[:SS[.fffffffff]]] or HH[MM[SS[.fffffffff]]].
fn parse_time_part(s: String) -> Option(#(TimeRec, String)) {
  use #(h, rest) <- option.then(take_digits(s, 2))
  let #(mi, sec, frac_ns, rest) = case rest {
    ":" <> r1 ->
      case take_digits(r1, 2) {
        Some(#(mi, r2)) ->
          case r2 {
            ":" <> r3 ->
              case take_digits(r3, 2) {
                Some(#(sec, r4)) -> {
                  let #(f, r5) = parse_fraction(r4)
                  #(mi, sec, f, r5)
                }
                None -> #(mi, 0, 0, r2)
              }
            _ -> #(mi, 0, 0, r2)
          }
        None -> #(0, 0, 0, rest)
      }
    _ ->
      case take_digits(rest, 2) {
        Some(#(mi, r2)) ->
          case take_digits(r2, 2) {
            Some(#(sec, r3)) -> {
              let #(f, r4) = parse_fraction(r3)
              #(mi, sec, f, r4)
            }
            None -> #(mi, 0, 0, r2)
          }
        None -> #(0, 0, 0, rest)
      }
  }
  // Leap second: 60 → 59 per spec. Anything above 60 is a syntax error, so
  // the range check below MUST see the raw parsed value — clamping first
  // would silently accept "T00:00:61".
  let t =
    TimeRec(
      hour: h,
      minute: mi,
      second: int.min(sec, 59),
      ms: frac_ns / ns_per_ms,
      us: { frac_ns % ns_per_ms } / ns_per_us,
      ns: frac_ns % ns_per_us,
    )
  case h <= 23 && mi <= 59 && sec <= 60 {
    True -> Some(#(t, rest))
    False -> None
  }
}

/// Fraction after '.' or ',' — 1..9 digits → nanoseconds.
fn parse_fraction(s: String) -> #(Int, String) {
  case s {
    "." <> r | "," <> r ->
      case take_some_digits(r, 9) {
        Some(#(v, count, rest)) -> #(v * pow10(9 - count), rest)
        None -> #(0, s)
      }
    _ -> #(0, s)
  }
}

fn pow10(n: Int) -> Int {
  case n {
    0 -> 1
    _ -> 10 * pow10(n - 1)
  }
}

/// UTC offset: Z / z / ±HH[:MM[:SS[.fff]]] / ±HH[MM[SS]].
/// Returns #(is_z, offset_ns, rest).
/// Returns #(is_z, offset_ns, sub_minute_syntax, rest). `sub_minute_syntax`
/// is True when the offset spelled out a seconds component (even ":00"),
/// which disqualifies it from use as a time zone identifier.
fn parse_offset_part(s: String) -> Option(#(Bool, Option(Int), Bool, String)) {
  case s {
    "Z" <> rest | "z" <> rest -> Some(#(True, None, False, rest))
    "+" <> rest -> parse_offset_value(rest, 1)
    "-" <> rest -> parse_offset_value(rest, -1)
    _ -> None
  }
}

fn parse_offset_value(
  s: String,
  sign: Int,
) -> Option(#(Bool, Option(Int), Bool, String)) {
  use #(h, rest) <- option.then(take_digits(s, 2))
  let #(mi, sec, frac, sub_minute, rest) = case rest {
    ":" <> r1 ->
      case take_digits(r1, 2) {
        Some(#(mi, r2)) ->
          case r2 {
            ":" <> r3 ->
              case take_digits(r3, 2) {
                Some(#(sec, r4)) -> {
                  let #(f, r5) = parse_fraction(r4)
                  #(mi, sec, f, True, r5)
                }
                None -> #(mi, 0, 0, False, r2)
              }
            _ -> #(mi, 0, 0, False, r2)
          }
        None -> #(0, 0, 0, False, rest)
      }
    _ ->
      case take_digits(rest, 2) {
        Some(#(mi, r2)) ->
          case take_digits(r2, 2) {
            Some(#(sec, r3)) -> {
              let #(f, r4) = parse_fraction(r3)
              #(mi, sec, f, True, r4)
            }
            None -> #(mi, 0, 0, False, r2)
          }
        None -> #(0, 0, 0, False, rest)
      }
  }
  case h <= 23 && mi <= 59 && sec <= 59 {
    True -> {
      let ns =
        { h * ns_per_hour + mi * ns_per_minute + sec * ns_per_second + frac }
        * sign
      Some(#(False, Some(ns), sub_minute, rest))
    }
    False -> None
  }
}

/// Annotations: time zone bracket first (no '='), then key=value pairs.
/// Returns #(tz, calendar, rest) or None on syntax/critical errors.
fn parse_annotations(
  s: String,
  tz: Option(String),
  cal: Option(String),
  cal_critical: Bool,
) -> Option(#(Option(String), Option(String), String)) {
  case s {
    "[" <> r -> {
      let #(critical, r) = case r {
        "!" <> rr -> #(True, rr)
        _ -> #(False, r)
      }
      use #(body, rest) <- option.then(split_bracket(r, ""))
      case string.split_once(body, "=") {
        Ok(#(key, val)) ->
          case is_annotation_key(key) && val != "" {
            False -> None
            True ->
              case key {
                "u-ca" ->
                  case cal {
                    // Duplicate calendar annotations are a RangeError when
                    // ANY of them is critical; otherwise first one wins.
                    Some(_) ->
                      case critical || cal_critical {
                        True -> None
                        False -> parse_annotations(rest, tz, cal, cal_critical)
                      }
                    None -> parse_annotations(rest, tz, Some(val), critical)
                  }
                _ ->
                  case critical {
                    True -> None
                    False -> parse_annotations(rest, tz, cal, cal_critical)
                  }
              }
          }
        Error(Nil) ->
          // Time-zone annotation — only valid as the first bracket.
          case tz, cal, is_tz_annotation(body) {
            None, None, True ->
              parse_annotations(rest, Some(body), cal, cal_critical)
            _, _, _ -> None
          }
      }
    }
    _ -> Some(#(tz, cal, s))
  }
}

fn split_bracket(s: String, acc: String) -> Option(#(String, String)) {
  case string.pop_grapheme(s) {
    Ok(#("]", rest)) ->
      case acc {
        "" -> None
        _ -> Some(#(acc, rest))
      }
    Ok(#(c, rest)) -> split_bracket(rest, acc <> c)
    Error(Nil) -> None
  }
}

fn is_annotation_key(s: String) -> Bool {
  s != ""
  && list.all(string.to_graphemes(s), fn(c) {
    is_lower_alpha(c) || c == "-" || c == "_" || digits.digit_value(c) != None
  })
}

fn is_lower_alpha(c: String) -> Bool {
  string.contains("abcdefghijklmnopqrstuvwxyz", c) && c != ""
}

fn is_tz_annotation(s: String) -> Bool {
  case s {
    "+" <> _ | "-" <> _ -> True
    _ ->
      s != ""
      && list.all(string.split(s, "/"), fn(part) {
        // TimeZoneIANANameComponent: TZLeadingChar TZChar* — a leading
        // digit is not a name (so date-time strings fall through to the
        // ISO-string time zone extraction).
        case string.pop_grapheme(part) {
          Error(Nil) -> False
          Ok(#(first, rest)) ->
            is_tz_leading_char(first)
            && list.all(string.to_graphemes(rest), is_tz_char)
        }
      })
  }
}

fn is_tz_leading_char(c: String) -> Bool {
  is_lower_alpha(c)
  || string.contains("ABCDEFGHIJKLMNOPQRSTUVWXYZ", c)
  || c == "."
  || c == "_"
}

fn is_tz_char(c: String) -> Bool {
  is_tz_leading_char(c) || digits.digit_value(c) != None || c == "-" || c == "+"
}

/// Parse a full Temporal date-time string:
///   date [T time [offset]] [annotations]
fn parse_iso_datetime_string(s: String) -> Option(ParsedIso) {
  use #(y, m, d, rest) <- option.then(parse_date_part(s))
  case is_valid_iso_date(y, m, d) {
    False -> None
    True -> {
      let date = IsoDate(y, m, d)
      let #(time, z, offset_ns, offset_sub_minute, rest) = case rest {
        "T" <> tr | "t" <> tr | " " <> tr ->
          case parse_time_part(tr) {
            Some(#(t, r2)) ->
              case parse_offset_part(r2) {
                Some(#(z, off, sub, r3)) -> #(Some(t), z, off, sub, r3)
                None -> #(Some(t), False, None, False, r2)
              }
            None -> #(None, False, None, False, rest)
          }
        _ -> #(None, False, None, False, rest)
      }
      // If a "T" was present but the time failed to parse, reject.
      case time == None && is_time_prefix(rest) {
        True -> None
        False -> {
          use #(tz, cal, rest2) <- option.then(parse_annotations(
            rest,
            None,
            None,
            False,
          ))
          case rest2 {
            "" ->
              Some(ParsedIso(
                date: Some(date),
                time:,
                z:,
                offset_ns:,
                offset_sub_minute:,
                tz:,
                calendar: cal,
              ))
            _ -> None
          }
        }
      }
    }
  }
}

fn is_time_prefix(s: String) -> Bool {
  case s {
    "T" <> _ | "t" <> _ | " " <> _ -> True
    _ -> False
  }
}

/// Validate that the string's [u-ca=] calendar annotation (if any) names a
/// supported calendar. RangeError otherwise. No annotation is always valid.
fn check_parsed_calendar(p: ParsedIso) -> Result(Nil, TErr) {
  case p.calendar {
    None -> Ok(Nil)
    Some(c) -> {
      use _ <- result.map(canonicalize_calendar(c))
      Nil
    }
  }
}

/// Parse a string for ToTemporalDate/DateTime — Z designator is rejected.
fn parse_plain_datetime_string(s: String) -> Result(ParsedIso, TErr) {
  case parse_iso_datetime_string(s) {
    None -> Error(RangeE("invalid ISO 8601 string: " <> s))
    Some(p) ->
      case p.z {
        True ->
          Error(RangeE("Z designator not supported for plain Temporal types"))
        False -> {
          use Nil <- result.map(check_parsed_calendar(p))
          p
        }
      }
  }
}

// ============================================================================
// Time zone handling — named IANA zones (system tzdata via temporal_tz),
// "UTC", and fixed numeric offsets
// ============================================================================

/// Parse + canonicalize a time zone identifier. Returns the canonical id.
/// Accepts bare identifiers ("UTC", "+05:30") and ISO date-time strings that
/// carry a [TimeZone] annotation, a Z designator, or a numeric offset
/// (ParseTemporalTimeZoneString).
fn parse_time_zone_id(id: String) -> Result(String, TErr) {
  case parse_time_zone_id_strict(id) {
    Ok(tz) -> Ok(tz)
    Error(StrictUnknown) -> tz_from_datetime_string(id)
    Error(StrictInvalid(e)) -> Error(e)
  }
}

type StrictTzError {
  /// Not a bare identifier; an ISO string fallback may still apply.
  StrictUnknown
  StrictInvalid(TErr)
}

/// ParseTimeZoneIdentifier: bare identifiers only (UTC, offsets, IANA names);
/// ISO date-time strings are not identifiers (the constructor rejects them).
fn parse_time_zone_id_strict(id: String) -> Result(String, StrictTzError) {
  case string.uppercase(id) == "UTC" {
    True -> Ok("UTC")
    False ->
      case parse_offset_tz_id(id) {
        Some(canonical) -> Ok(canonical)
        None ->
          case temporal_tz.lookup(id) {
            Ok(zone) -> Ok(temporal_tz.zone_id(zone))
            Error(Nil) ->
              case is_tz_annotation(id) {
                True -> Error(StrictInvalid(unsupported_tz(id)))
                False -> Error(StrictUnknown)
              }
          }
      }
  }
}

/// Extract a time zone from an ISO date-time string: annotation wins, then
/// the Z designator (-> "UTC"), then a minute-precision numeric offset.
fn tz_from_datetime_string(s: String) -> Result(String, TErr) {
  case parse_iso_datetime_string(s) {
    None -> Error(RangeE("invalid time zone: " <> s))
    Some(p) ->
      case p.tz {
        Some(tz_str) ->
          case string.uppercase(tz_str) == "UTC" {
            True -> Ok("UTC")
            False ->
              case parse_offset_tz_id(tz_str) {
                Some(canonical) -> Ok(canonical)
                None ->
                  case temporal_tz.lookup(tz_str) {
                    Ok(zone) -> Ok(temporal_tz.zone_id(zone))
                    Error(Nil) -> Error(unsupported_tz(tz_str))
                  }
              }
          }
        None ->
          case p.z {
            True -> Ok("UTC")
            False ->
              case p.offset_ns {
                Some(off) ->
                  // The offset must be syntactically minute-precision: a
                  // seconds component (even ":00") is not a valid zone.
                  case !p.offset_sub_minute && off % ns_per_minute == 0 {
                    True -> Ok(format_offset_minutes(off))
                    False ->
                      Error(RangeE("sub-minute offset not valid as a time zone"))
                  }
                None -> Error(RangeE("no time zone found in string: " <> s))
              }
          }
      }
  }
}

/// Offset time zone identifier: ±HH[:MM] (minute precision only).
/// Canonical form is ±HH:MM.
fn parse_offset_tz_id(id: String) -> Option(String) {
  let sign = case id {
    "+" <> _ -> 1
    "-" <> _ -> -1
    _ -> 0
  }
  case sign == 0 {
    True -> None
    False ->
      case parse_offset_part(id) {
        // A seconds component (sub-minute syntax) is not allowed in an
        // offset time zone identifier, even when it is ":00".
        Some(#(False, Some(ns), False, "")) ->
          case ns % ns_per_minute == 0 && int.absolute_value(ns) < ns_per_day {
            True -> Some(format_offset_minutes(ns))
            False -> None
          }
        _ -> None
      }
  }
}

/// Zone id classification: "UTC", fixed offset, or named IANA zone. A
/// `NamedZone` carries the *validated* `temporal_tz.Zone`, so classification
/// is the single place a raw id is checked against the tzdata name table.
type TzKind {
  UtcZone
  OffsetZone(Int)
  NamedZone(temporal_tz.Zone)
  /// Not "UTC", not a numeric offset, and not in the tzdata name table.
  UnknownZone
}

fn tz_kind(tz: String) -> TzKind {
  case tz {
    "UTC" -> UtcZone
    "+" <> _ | "-" <> _ ->
      case parse_offset_part(tz) {
        Some(#(False, Some(ns), _, "")) -> OffsetZone(ns)
        _ -> named_or_unknown(tz)
      }
    _ -> named_or_unknown(tz)
  }
}

fn named_or_unknown(tz: String) -> TzKind {
  case temporal_tz.lookup(tz) {
    Ok(zone) -> NamedZone(zone)
    Error(Nil) -> UnknownZone
  }
}

fn unsupported_tz(tz: String) -> TErr {
  RangeE("time zone " <> tz <> " is not supported")
}

/// A zone whose name we accepted but whose tzdata will not load is a broken
/// install, not an unknown identifier: same RangeError, but the reason says so.
fn unloadable_tz(tz: String, error: temporal_tz.TzError) -> TErr {
  RangeE(
    "time zone " <> tz <> " cannot be loaded: " <> temporal_tz.describe(error),
  )
}

/// GetOffsetNanosecondsFor — UTC offset of `tz` at an exact instant.
/// RangeError when a named zone's TZif data cannot be loaded (same error as
/// an unsupported time zone identifier).
fn tz_offset_ns_at(tz: String, epoch_ns: Int) -> Result(Int, TErr) {
  case tz_kind(tz) {
    UtcZone -> Ok(0)
    OffsetZone(off) -> Ok(off)
    UnknownZone -> Error(unsupported_tz(tz))
    NamedZone(zone) ->
      temporal_tz.offset_ns_at(zone, epoch_ns)
      |> result.map_error(unloadable_tz(tz, _))
  }
}

/// Wall-clock date/time of `epoch_ns` in `tz`.
/// RangeError when a named zone's TZif data cannot be loaded.
fn epoch_ns_to_iso_in(
  tz: String,
  epoch_ns: Int,
) -> Result(#(IsoDate, TimeRec), TErr) {
  use off <- result.map(tz_offset_ns_at(tz, epoch_ns))
  epoch_ns_to_iso(epoch_ns, off)
}

fn validate_epoch_ns(ns: Int) -> Result(Int, TErr) {
  case int.absolute_value(ns) <= ns_max_instant {
    True -> Ok(ns)
    False -> Error(RangeE("instant outside valid range"))
  }
}

/// CheckISODaysRange — the wall-clock date must be within ±10^8 days.
fn check_iso_days_range(d: IsoDate) -> Result(Nil, TErr) {
  case int.absolute_value(epoch_days(d)) <= 100_000_000 {
    True -> Ok(Nil)
    False -> Error(RangeE("date outside of supported range"))
  }
}

/// GetPossibleEpochNanoseconds — ascending epoch instants whose local time
/// in `tz` reads as the given wall-clock date-time. Empty for times skipped
/// by an offset transition; two entries for repeated times.
/// RangeError when the (offset-shifted) date is outside the supported range.
fn get_possible_epoch_ns(
  tz: String,
  d: IsoDate,
  t: TimeRec,
) -> Result(List(Int), TErr) {
  let utc = utc_epoch_ns(d, t)
  case tz_kind(tz) {
    UtcZone -> {
      use Nil <- result.try(check_iso_days_range(d))
      use ns <- result.map(validate_epoch_ns(utc))
      [ns]
    }
    OffsetZone(off) -> {
      let shifted_day = floor_div(utc - off, ns_per_day)
      use Nil <- result.try(
        check_iso_days_range(iso_date_from_epoch_days(shifted_day)),
      )
      use ns <- result.map(validate_epoch_ns(utc - off))
      [ns]
    }
    UnknownZone -> Error(unsupported_tz(tz))
    NamedZone(zone) -> {
      use Nil <- result.try(check_iso_days_range(d))
      use before <- result.try(tz_offset_ns_at(tz, utc - ns_per_day))
      use after <- result.try(tz_offset_ns_at(tz, utc + ns_per_day))
      let candidates = case before == after {
        True -> [before]
        False -> [before, after]
      }
      Ok(
        list.filter_map(candidates, fn(off) {
          let ens = utc - off
          case temporal_tz.offset_ns_at(zone, ens) == Ok(off) {
            True -> Ok(ens)
            False -> Error(Nil)
          }
        }),
      )
    }
  }
}

/// DisambiguatePossibleEpochNanoseconds.
fn disambiguate_epoch_ns(
  possible: List(Int),
  tz: String,
  d: IsoDate,
  t: TimeRec,
  dis: Disambiguation,
) -> Result(Int, TErr) {
  case possible {
    [one] -> validate_epoch_ns(one)
    [first, ..rest] ->
      case dis {
        Compatible | Earlier -> validate_epoch_ns(first)
        Later ->
          case list.last(rest) {
            Ok(l) -> validate_epoch_ns(l)
            Error(Nil) -> validate_epoch_ns(first)
          }
        RejectDisambiguation -> Error(RangeE("ambiguous wall-clock time"))
      }
    [] ->
      case dis {
        RejectDisambiguation -> Error(RangeE("no such wall-clock time"))
        Compatible | Earlier | Later -> {
          // Skipped (gap) time: shift by the size of the gap and retry.
          let utc = utc_epoch_ns(d, t)
          use before <- result.try(tz_offset_ns_at(tz, utc - ns_per_day))
          use after <- result.try(tz_offset_ns_at(tz, utc + ns_per_day))
          let gap = after - before
          let shifted = case dis {
            Earlier -> utc - gap
            _ -> utc + gap
          }
          let #(d2, t2) = epoch_ns_to_iso(shifted, 0)
          use possible2 <- result.try(get_possible_epoch_ns(tz, d2, t2))
          case dis, possible2 {
            _, [] -> Error(RangeE("no such wall-clock time"))
            Earlier, [f, ..] -> validate_epoch_ns(f)
            _, [f, ..rest2] ->
              case list.last(rest2) {
                Ok(la) -> validate_epoch_ns(la)
                Error(Nil) -> validate_epoch_ns(f)
              }
          }
        }
      }
  }
}

/// GetEpochNanosecondsFor.
fn get_epoch_ns_for(
  tz: String,
  d: IsoDate,
  t: TimeRec,
  dis: Disambiguation,
) -> Result(Int, TErr) {
  use possible <- result.try(get_possible_epoch_ns(tz, d, t))
  disambiguate_epoch_ns(possible, tz, d, t, dis)
}

/// GetStartOfDay.
fn start_of_day_ns(tz: String, d: IsoDate) -> Result(Int, TErr) {
  use possible <- result.try(get_possible_epoch_ns(tz, d, midnight))
  case possible {
    [first, ..] -> validate_epoch_ns(first)
    [] -> {
      // Midnight lies in a DST gap; only named zones can reach here. The day
      // starts at the instant the gap ends: the next transition after a
      // point guaranteed to be before it (one day earlier).
      use day_before <- result.try(validate_epoch_ns(
        utc_epoch_ns(d, midnight) - ns_per_day,
      ))
      use zone <- result.try(case tz_kind(tz) {
        NamedZone(zone) -> Ok(zone)
        UtcZone | OffsetZone(_) | UnknownZone -> Error(unsupported_tz(tz))
      })
      case temporal_tz.next_transition_ns(zone, day_before) {
        Ok(Some(transition)) -> validate_epoch_ns(transition)
        Ok(None) -> Error(RangeE("no start of day for skipped midnight"))
        Error(err) -> Error(unloadable_tz(tz, err))
      }
    }
  }
}

/// Whether the source of a wall-clock date-time also supplied a UTC offset.
/// The offset lives inside `OptionOffset`, so it cannot be read — nor
/// defaulted to a meaningless 0 — when the source had none.
type OffsetBehaviour {
  WallOffset
  OptionOffset(offset_ns: Int)
}

/// InterpretISODateTimeOffset. `match_minutes` allows minute-truncated
/// offsets (ISO strings).
fn interpret_offset(
  d: IsoDate,
  t: TimeRec,
  behaviour: OffsetBehaviour,
  tz: String,
  dis: Disambiguation,
  offset_opt: OffsetOption,
  match_minutes: Bool,
) -> Result(Int, TErr) {
  case behaviour {
    WallOffset -> get_epoch_ns_for(tz, d, t, dis)
    OptionOffset(_) if offset_opt == IgnoreOffset ->
      get_epoch_ns_for(tz, d, t, dis)
    OptionOffset(offset_ns) if offset_opt == UseOffset -> {
      let ns = utc_epoch_ns(d, t) - offset_ns
      use Nil <- result.try(
        check_iso_days_range(
          iso_date_from_epoch_days(floor_div(ns, ns_per_day)),
        ),
      )
      validate_epoch_ns(ns)
    }
    OptionOffset(offset_ns) -> {
      let utc = utc_epoch_ns(d, t)
      use Nil <- result.try(check_iso_days_range(d))
      use possible <- result.try(get_possible_epoch_ns(tz, d, t))
      let matched =
        list.find(possible, fn(candidate) {
          let cand_off = utc - candidate
          let rounded = round_to_increment(cand_off, ns_per_minute, HalfExpand)
          cand_off == offset_ns || { match_minutes && rounded == offset_ns }
        })
      case matched {
        Ok(c) -> validate_epoch_ns(c)
        Error(Nil) ->
          case offset_opt == RejectOffset {
            True -> Error(RangeE("offset does not match time zone"))
            False -> disambiguate_epoch_ns(possible, tz, d, t, dis)
          }
      }
    }
  }
}

/// Full-precision offset string: ±HH:MM[:SS] (for the `offset` getter;
/// some zones historically had sub-minute offsets).
fn format_offset_full(offset_ns: Int) -> String {
  let sign = case offset_ns < 0 {
    True -> "-"
    False -> "+"
  }
  let total_sec = int.absolute_value(offset_ns) / ns_per_second
  let base =
    sign <> pad2(total_sec / 3600) <> ":" <> pad2({ total_sec / 60 } % 60)
  case total_sec % 60 {
    0 -> base
    s -> base <> ":" <> pad2(s)
  }
}

/// Offset rounded to the nearest minute, for ISO string display
/// (FormatDateTimeUTCOffsetRounded).
fn format_offset_rounded(offset_ns: Int) -> String {
  format_offset_minutes(round_to_increment(offset_ns, ns_per_minute, HalfExpand))
}

/// TimeZoneEquals — identical ids, or named ids resolving to the same
/// canonical zone (links like Asia/Calcutta -> Asia/Kolkata). Etc/UTC and its
/// links canonicalize to "UTC", so a named zone can equal the UTC zone.
fn time_zone_equals(a: String, b: String) -> Bool {
  a == b
  || case tz_kind(a), tz_kind(b) {
    OffsetZone(_), _ | _, OffsetZone(_) -> False
    UnknownZone, _ | _, UnknownZone -> False
    UtcZone, UtcZone -> True
    UtcZone, NamedZone(z) | NamedZone(z), UtcZone ->
      temporal_tz.canonical(z) == "UTC"
    NamedZone(za), NamedZone(zb) ->
      temporal_tz.canonical(za) == temporal_tz.canonical(zb)
  }
}

// ============================================================================
// Constructors
// ============================================================================

/// new Temporal.PlainDate(year, month, day [, calendar])
fn plain_date_ctor(
  protos: TemporalProtos,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use y, state <- state.try_op(arg_trunc_int(state, args, 0))
  use m, state <- state.try_op(arg_trunc_int(state, args, 1))
  use d, state <- state.try_op(arg_trunc_int(state, args, 2))
  use cal <- terr(state, to_calendar_arg(helpers.arg_at(args, 3)))
  case is_valid_iso_date(y, m, d) {
    False -> state.range_error(state, "invalid ISO date")
    True -> {
      let date = IsoDate(y, m, d)
      case iso_date_within_limits(date) {
        False -> state.range_error(state, "date outside of supported range")
        True -> {
          let #(state, v) = make_date_cal(state, protos, date, cal)
          #(state, Ok(v))
        }
      }
    }
  }
}

/// new Temporal.PlainTime(h, mi, s, ms, us, ns) — all optional, default 0.
fn plain_time_ctor(
  protos: TemporalProtos,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use h, state <- state.try_op(opt_int_arg(state, args, 0))
  use mi, state <- state.try_op(opt_int_arg(state, args, 1))
  use s, state <- state.try_op(opt_int_arg(state, args, 2))
  use ms, state <- state.try_op(opt_int_arg(state, args, 3))
  use us, state <- state.try_op(opt_int_arg(state, args, 4))
  use ns, state <- state.try_op(opt_int_arg(state, args, 5))
  let t = TimeRec(h, mi, s, ms, us, ns)
  case is_valid_time(t) {
    False -> state.range_error(state, "invalid time")
    True -> {
      let #(state, v) = make_time(state, protos, t)
      #(state, Ok(v))
    }
  }
}

/// ToIntegerWithTruncation on the argument at `idx`.
fn arg_trunc_int(
  state: State(host),
  args: List(JsValue),
  idx: Int,
) -> Result(#(Int, State(host)), #(JsValue, State(host))) {
  to_integer_with_truncation(state, helpers.arg_at(args, idx))
}

/// ToIntegerWithTruncation on the argument at `idx`, undefined → `default`.
fn arg_trunc_int_or(
  state: State(host),
  args: List(JsValue),
  idx: Int,
  default: Int,
) -> Result(#(Int, State(host)), #(JsValue, State(host))) {
  case helpers.arg_at(args, idx) {
    JsUndefined -> Ok(#(default, state))
    v -> to_integer_with_truncation(state, v)
  }
}

/// Optional integer argument: undefined → 0, else ToIntegerWithTruncation.
fn opt_int_arg(
  state: State(host),
  args: List(JsValue),
  idx: Int,
) -> Result(#(Int, State(host)), #(JsValue, State(host))) {
  arg_trunc_int_or(state, args, idx, 0)
}

/// new Temporal.PlainDateTime(y, mo, d, h, mi, s, ms, us, ns [, calendar])
fn plain_date_time_ctor(
  protos: TemporalProtos,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use y, state <- state.try_op(arg_trunc_int(state, args, 0))
  use mo, state <- state.try_op(arg_trunc_int(state, args, 1))
  use d, state <- state.try_op(arg_trunc_int(state, args, 2))
  use h, state <- state.try_op(opt_int_arg(state, args, 3))
  use mi, state <- state.try_op(opt_int_arg(state, args, 4))
  use s, state <- state.try_op(opt_int_arg(state, args, 5))
  use ms, state <- state.try_op(opt_int_arg(state, args, 6))
  use us, state <- state.try_op(opt_int_arg(state, args, 7))
  use ns, state <- state.try_op(opt_int_arg(state, args, 8))
  use cal <- terr(state, to_calendar_arg(helpers.arg_at(args, 9)))
  let t = TimeRec(h, mi, s, ms, us, ns)
  case is_valid_iso_date(y, mo, d) && is_valid_time(t) {
    False -> state.range_error(state, "invalid ISO date-time")
    True -> {
      let date = IsoDate(y, mo, d)
      case iso_datetime_within_limits(date, t) {
        False ->
          state.range_error(state, "date-time outside of supported range")
        True -> {
          let #(state, v) = make_date_time_cal(state, protos, date, t, cal)
          #(state, Ok(v))
        }
      }
    }
  }
}

/// new Temporal.PlainYearMonth(year, month [, calendar [, referenceISODay]])
fn plain_year_month_ctor(
  protos: TemporalProtos,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use y, state <- state.try_op(arg_trunc_int(state, args, 0))
  use m, state <- state.try_op(arg_trunc_int(state, args, 1))
  use cal <- terr(state, to_calendar_arg(helpers.arg_at(args, 2)))
  use d, state <- state.try_op(arg_trunc_int_or(state, args, 3, 1))
  case is_valid_iso_date(y, m, d) {
    False -> state.range_error(state, "invalid ISO year-month")
    True ->
      case iso_year_month_within_limits(y, m) {
        False ->
          state.range_error(state, "year-month outside of supported range")
        True -> {
          let #(state, v) = make_year_month_cal(state, protos, y, m, d, cal)
          #(state, Ok(v))
        }
      }
  }
}

/// new Temporal.PlainMonthDay(month, day [, calendar [, referenceISOYear]])
fn plain_month_day_ctor(
  protos: TemporalProtos,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use m, state <- state.try_op(arg_trunc_int(state, args, 0))
  use d, state <- state.try_op(arg_trunc_int(state, args, 1))
  use cal <- terr(state, to_calendar_arg(helpers.arg_at(args, 2)))
  use y, state <- state.try_op(arg_trunc_int_or(state, args, 3, 1972))
  case is_valid_iso_date(y, m, d) {
    False -> state.range_error(state, "invalid ISO month-day")
    True -> {
      let #(state, v) = make_month_day_cal(state, protos, m, d, y, cal)
      #(state, Ok(v))
    }
  }
}

/// new Temporal.Duration(y, mo, w, d, h, mi, s, ms, us, ns) — all optional.
fn duration_ctor(
  protos: TemporalProtos,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use y, state <- state.try_op(opt_integral_arg(state, args, 0))
  use mo, state <- state.try_op(opt_integral_arg(state, args, 1))
  use w, state <- state.try_op(opt_integral_arg(state, args, 2))
  use d, state <- state.try_op(opt_integral_arg(state, args, 3))
  use h, state <- state.try_op(opt_integral_arg(state, args, 4))
  use mi, state <- state.try_op(opt_integral_arg(state, args, 5))
  use s, state <- state.try_op(opt_integral_arg(state, args, 6))
  use ms, state <- state.try_op(opt_integral_arg(state, args, 7))
  use us, state <- state.try_op(opt_integral_arg(state, args, 8))
  use ns, state <- state.try_op(opt_integral_arg(state, args, 9))
  let dur = DurRec(y, mo, w, d, h, mi, s, ms, us, ns)
  finish_duration(state, protos, dur)
}

fn opt_integral_arg(
  state: State(host),
  args: List(JsValue),
  idx: Int,
) -> Result(#(Int, State(host)), #(JsValue, State(host))) {
  case helpers.arg_at(args, idx) {
    JsUndefined -> Ok(#(0, state))
    v -> to_integer_if_integral(state, v)
  }
}

fn duration_sign(d: DurRec) -> Int {
  let fields = [
    d.years, d.months, d.weeks, d.days, d.hours, d.minutes, d.seconds, d.ms,
    d.us, d.ns,
  ]
  list.fold(fields, 0, fn(acc, f) {
    case acc != 0 {
      True -> acc
      False ->
        case f > 0 {
          True -> 1
          False ->
            case f < 0 {
              True -> -1
              False -> 0
            }
        }
    }
  })
}

fn is_valid_duration(d: DurRec) -> Bool {
  let sign = duration_sign(d)
  // Components are observed as Numbers, so validity is determined on the
  // float-rounded values ℝ(𝔽(x)) — rounding a large component up can push
  // the total over the limit (CreateTemporalDuration → IsValidDuration).
  let fr = f64_int
  let d =
    DurRec(
      years: fr(d.years),
      months: fr(d.months),
      weeks: fr(d.weeks),
      days: fr(d.days),
      hours: fr(d.hours),
      minutes: fr(d.minutes),
      seconds: fr(d.seconds),
      ms: fr(d.ms),
      us: fr(d.us),
      ns: fr(d.ns),
    )
  let fields = [
    d.years, d.months, d.weeks, d.days, d.hours, d.minutes, d.seconds, d.ms,
    d.us, d.ns,
  ]
  let consistent =
    list.all(fields, fn(f) {
      { f >= 0 || sign <= 0 } && { f <= 0 || sign >= 0 }
    })
  let two32 = 4_294_967_296
  let cal_ok =
    int.absolute_value(d.years) < two32
    && int.absolute_value(d.months) < two32
    && int.absolute_value(d.weeks) < two32
  let total = time_duration_ns(d)
  consistent && cal_ok && int.absolute_value(total) <= max_time_duration_ns
}

/// Total nanoseconds of the day+time portion (days..nanoseconds).
fn time_duration_ns(d: DurRec) -> Int {
  d.days
  * ns_per_day
  + d.hours
  * ns_per_hour
  + d.minutes
  * ns_per_minute
  + d.seconds
  * ns_per_second
  + d.ms
  * ns_per_ms
  + d.us
  * ns_per_us
  + d.ns
}

/// new Temporal.Instant(epochNanoseconds: BigInt)
fn instant_ctor(
  protos: TemporalProtos,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ns, state <- state.try_op(coerce.to_bigint(state, helpers.arg_at(args, 0)))
  case int.absolute_value(ns) <= ns_max_instant {
    False -> state.range_error(state, "epoch nanoseconds out of range")
    True -> {
      let #(state, v) = make_instant(state, protos, ns)
      #(state, Ok(v))
    }
  }
}

/// new Temporal.ZonedDateTime(epochNanoseconds: BigInt, timeZone [, calendar])
fn zoned_date_time_ctor(
  protos: TemporalProtos,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ns, state <- state.try_op(coerce.to_bigint(state, helpers.arg_at(args, 0)))
  case helpers.arg_at(args, 1) {
    JsString(tz_str) -> {
      use tz <- terr(state, case parse_time_zone_id_strict(tz_str) {
        Ok(tz) -> Ok(tz)
        Error(StrictUnknown) ->
          Error(RangeE("invalid time zone identifier: " <> tz_str))
        Error(StrictInvalid(e)) -> Error(e)
      })
      use cal <- terr(state, to_calendar_arg(helpers.arg_at(args, 2)))
      case int.absolute_value(ns) <= ns_max_instant {
        False -> state.range_error(state, "epoch nanoseconds out of range")
        True -> {
          let #(state, v) = make_zoned_cal(state, protos, ns, tz, cal)
          #(state, Ok(v))
        }
      }
    }
    _ -> state.type_error(state, "time zone must be a string")
  }
}

// ============================================================================
// ToTemporalX conversions
// ============================================================================

/// Read a field from a property bag, converting with `conv`. Missing fields
/// yield None.
fn read_bag_int_field(
  state: State(host),
  ref: Ref,
  key: String,
  conv: fn(State(host), JsValue) ->
    Result(#(Int, State(host)), #(JsValue, State(host))),
) -> Result(#(Option(Int), State(host)), #(JsValue, State(host))) {
  use got <- result.try(ops_object.get_value(
    state,
    ref,
    Named(key),
    JsObject(ref),
  ))
  case got {
    #(JsUndefined, st) -> Ok(#(None, st))
    #(v, st) -> {
      use #(n, st) <- result.map(conv(st, v))
      #(Some(n), st)
    }
  }
}

/// `read_bag_int_field` with ToIntegerWithTruncation.
fn read_int_field(
  state: State(host),
  ref: Ref,
  key: String,
) -> Result(#(Option(Int), State(host)), #(JsValue, State(host))) {
  read_bag_int_field(state, ref, key, to_integer_with_truncation)
}

/// `read_bag_int_field` with ToPositiveIntegerWithTruncation.
fn read_pos_int_field(
  state: State(host),
  ref: Ref,
  key: String,
) -> Result(#(Option(Int), State(host)), #(JsValue, State(host))) {
  read_bag_int_field(state, ref, key, to_positive_integer_with_truncation)
}

/// `read_bag_int_field` with ToIntegerIfIntegral.
fn read_integral_int_field(
  state: State(host),
  ref: Ref,
  key: String,
) -> Result(#(Option(Int), State(host)), #(JsValue, State(host))) {
  read_bag_int_field(state, ref, key, to_integer_if_integral)
}

/// Read the "monthCode" field: must be a String primitive "M01".."M13"
/// optionally with an "L" suffix (leap month).
fn read_month_code(
  state: State(host),
  ref: Ref,
) -> Result(#(Option(tcal.MonthCode), State(host)), #(JsValue, State(host))) {
  use got <- result.try(ops_object.get_value(
    state,
    ref,
    Named("monthCode"),
    JsObject(ref),
  ))
  case got {
    #(JsUndefined, st) -> Ok(#(None, st))
    #(v, st) -> {
      // ToPrimitive(string) then require a String type.
      use prim <- result.try(coerce.to_primitive(st, v, coerce.StringHint))
      case prim {
        #(JsString(s), st) ->
          // Only the ToMonthCode GRAMMAR is checked at read time; whether
          // the code suits the calendar is validated in ResolveFields,
          // after the required-field TypeError checks.
          case parse_month_code_grammar(s) {
            Ok(mc) -> Ok(#(Some(mc), st))
            Error(Nil) -> range_error_result(st, "invalid monthCode: " <> s)
          }
        #(_, st) -> type_error_result(st, "monthCode must be a string")
      }
    }
  }
}

/// ToMonthCode grammar: "M" + two digits + optional "L"; "M00" needs "L".
/// Whether the code suits the calendar is `tcal.month_for_code`'s answer;
/// this is the only String -> `tcal.MonthCode` site.
fn parse_month_code_grammar(s: String) -> Result(tcal.MonthCode, Nil) {
  case s {
    "M" <> rest -> {
      let #(digits, leap) = case string.ends_with(rest, "L") {
        True -> #(string.drop_end(rest, 1), True)
        False -> #(rest, False)
      }
      case two_decimal_digits(digits) {
        // "M00" is only meaningful as the leap month "M00L".
        Ok(n) if n >= 1 || leap -> Ok(tcal.MonthCode(number: n, leap:))
        Ok(_) | Error(Nil) -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

/// Exactly two ASCII decimal digits, so signed forms ("M-1L", "M+1") are
/// rejected before `int.parse` — which would happily accept them.
fn two_decimal_digits(s: String) -> Result(Int, Nil) {
  case string.to_graphemes(s) {
    [a, b] ->
      case is_ascii_digit(a) && is_ascii_digit(b) {
        True -> int.parse(s)
        False -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

fn is_ascii_digit(g: String) -> Bool {
  case g {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

fn month_code_str(m: Int) -> String {
  "M" <> pad2(m)
}

/// Read the "era" field: must be a String when present.
fn read_bag_era(
  state: State(host),
  ref: Ref,
) -> Result(#(Option(String), State(host)), #(JsValue, State(host))) {
  use got <- result.try(ops_object.get_value(
    state,
    ref,
    Named("era"),
    JsObject(ref),
  ))
  case got {
    #(JsUndefined, st) -> Ok(#(None, st))
    #(v, st) -> {
      use prim <- result.try(coerce.to_primitive(st, v, coerce.StringHint))
      case prim {
        #(JsString(s), st) -> Ok(#(Some(s), st))
        #(_, st) -> type_error_result(st, "era must be a string")
      }
    }
  }
}

/// Read a property bag's "calendar" field; returns the calendar (iso8601
/// when absent).
fn read_bag_calendar(
  state: State(host),
  ref: Ref,
) -> Result(#(tcal.Calendar, State(host)), #(JsValue, State(host))) {
  use got <- result.try(ops_object.get_value(
    state,
    ref,
    Named("calendar"),
    JsObject(ref),
  ))
  case got {
    #(JsUndefined, st) -> Ok(#(tcal.Iso8601, st))
    #(JsString(s), st) -> terr_op(st, calendar_from_string(s))
    #(JsObject(oref), st) ->
      // A Temporal object with a calendar slot acts as its calendar.
      case heap.read(st.heap, oref) {
        Some(ObjectSlot(kind: TemporalDateSlot(calendar:, ..), ..))
        | Some(ObjectSlot(kind: TemporalDateTimeSlot(calendar:, ..), ..))
        | Some(ObjectSlot(kind: TemporalYearMonthSlot(calendar:, ..), ..))
        | Some(ObjectSlot(kind: TemporalMonthDaySlot(calendar:, ..), ..))
        | Some(ObjectSlot(kind: TemporalZonedDateTimeSlot(calendar:, ..), ..)) ->
          Ok(#(calendar, st))
        _ -> type_error_result(st, "invalid calendar")
      }
    #(_, st) -> type_error_result(st, "invalid calendar")
  }
}

/// ToTemporalCalendarIdentifier(calendarLike) — string ids, ISO strings with
/// annotations, and Temporal objects carrying a calendar slot.
fn to_temporal_calendar_identifier(
  state: State(host),
  v: JsValue,
) -> Result(#(tcal.Calendar, State(host)), #(JsValue, State(host))) {
  case v {
    JsString(s) -> terr_op(state, calendar_from_string(s))
    JsObject(oref) ->
      case heap.read(state.heap, oref) {
        Some(ObjectSlot(kind: TemporalDateSlot(calendar:, ..), ..))
        | Some(ObjectSlot(kind: TemporalDateTimeSlot(calendar:, ..), ..))
        | Some(ObjectSlot(kind: TemporalYearMonthSlot(calendar:, ..), ..))
        | Some(ObjectSlot(kind: TemporalMonthDaySlot(calendar:, ..), ..))
        | Some(ObjectSlot(kind: TemporalZonedDateTimeSlot(calendar:, ..), ..)) ->
          Ok(#(calendar, state))
        _ -> type_error_result(state, "not a valid calendar")
      }
    _ -> type_error_result(state, "not a valid calendar")
  }
}

// ============================================================================
// Calendar-aware field resolution (CalendarResolveFields + CalendarDateToISO)
// ============================================================================

/// Calendar date fields read from a property bag (all optional here;
/// requiredness is checked in resolve_calendar_date).
type DateFields {
  DateFields(
    day: Option(Int),
    era: Option(String),
    era_year: Option(Int),
    month: Option(Int),
    month_code: Option(tcal.MonthCode),
    year: Option(Int),
  )
}

/// Read date fields from a bag in spec (alphabetical) order:
/// day, era, eraYear, month, monthCode, year. era/eraYear are only read for
/// calendars with eras.
fn read_date_fields(
  state: State(host),
  ref: Ref,
  cal: tcal.Calendar,
) -> Result(#(DateFields, State(host)), #(JsValue, State(host))) {
  use #(day, state) <- result.try(read_pos_int_field(state, ref, "day"))
  use #(era, state) <- result.try(case tcal.has_eras(cal) {
    True -> read_bag_era(state, ref)
    False -> Ok(#(None, state))
  })
  use #(era_year, state) <- result.try(case tcal.has_eras(cal) {
    True -> read_int_field(state, ref, "eraYear")
    False -> Ok(#(None, state))
  })
  use #(month, state) <- result.try(read_pos_int_field(state, ref, "month"))
  use #(month_code, state) <- result.try(read_month_code(state, ref))
  use #(year, state) <- result.try(read_int_field(state, ref, "year"))
  Ok(#(DateFields(day:, era:, era_year:, month:, month_code:, year:), state))
}

/// Time-of-day fields read from a property bag (all optional).
type TimeFields {
  TimeFields(
    hour: Option(Int),
    minute: Option(Int),
    second: Option(Int),
    ms: Option(Int),
    us: Option(Int),
    ns: Option(Int),
  )
}

const no_time_fields = TimeFields(None, None, None, None, None, None)

const no_date_fields = DateFields(None, None, None, None, None, None)

/// Fill a TimeRec from optional fields, defaulting each component to `base`.
fn time_fields_apply(f: TimeFields, base: TimeRec) -> TimeRec {
  TimeRec(
    hour: option.unwrap(f.hour, base.hour),
    minute: option.unwrap(f.minute, base.minute),
    second: option.unwrap(f.second, base.second),
    ms: option.unwrap(f.ms, base.ms),
    us: option.unwrap(f.us, base.us),
    ns: option.unwrap(f.ns, base.ns),
  )
}

/// Read time fields from a bag in spec (alphabetical) order: hour,
/// microsecond, millisecond, minute, nanosecond, second.
fn read_time_fields(
  state: State(host),
  ref: Ref,
) -> Result(#(TimeFields, State(host)), #(JsValue, State(host))) {
  use #(hour, state) <- result.try(read_int_field(state, ref, "hour"))
  use #(us, state) <- result.try(read_int_field(state, ref, "microsecond"))
  use #(ms, state) <- result.try(read_int_field(state, ref, "millisecond"))
  use #(minute, state) <- result.try(read_int_field(state, ref, "minute"))
  use #(ns, state) <- result.try(read_int_field(state, ref, "nanosecond"))
  use #(second, state) <- result.map(read_int_field(state, ref, "second"))
  #(TimeFields(hour:, minute:, second:, ms:, us:, ns:), state)
}

/// Date-time fields read from a property bag (all optional). `tz` is the raw
/// `timeZone` value (JsUndefined when absent or not requested).
type DateTimeFields {
  DateTimeFields(
    date: DateFields,
    time: TimeFields,
    offset: Option(Int),
    tz: JsValue,
  )
}

fn date_time_fields_all_none(f: DateTimeFields) -> Bool {
  f.date == no_date_fields && f.time == no_time_fields && f.offset == None
}

/// Read date-time fields from a bag in spec (alphabetical) order: day, era,
/// eraYear, hour, microsecond, millisecond, minute, month, monthCode,
/// nanosecond, [offset], second, [timeZone], year. era/eraYear are read only
/// for calendars with eras; offset and timeZone only when requested.
fn read_date_time_fields(
  state: State(host),
  ref: Ref,
  cal: tcal.Calendar,
  read_offset read_offset: Bool,
  read_tz read_tz: Bool,
) -> Result(#(DateTimeFields, State(host)), #(JsValue, State(host))) {
  use #(day, state) <- result.try(read_pos_int_field(state, ref, "day"))
  use #(era, state) <- result.try(case tcal.has_eras(cal) {
    True -> read_bag_era(state, ref)
    False -> Ok(#(None, state))
  })
  use #(era_year, state) <- result.try(case tcal.has_eras(cal) {
    True -> read_int_field(state, ref, "eraYear")
    False -> Ok(#(None, state))
  })
  use #(hour, state) <- result.try(read_int_field(state, ref, "hour"))
  use #(us, state) <- result.try(read_int_field(state, ref, "microsecond"))
  use #(ms, state) <- result.try(read_int_field(state, ref, "millisecond"))
  use #(minute, state) <- result.try(read_int_field(state, ref, "minute"))
  use #(month, state) <- result.try(read_pos_int_field(state, ref, "month"))
  use #(month_code, state) <- result.try(read_month_code(state, ref))
  use #(ns, state) <- result.try(read_int_field(state, ref, "nanosecond"))
  use #(offset, state) <- result.try(case read_offset {
    True -> read_bag_offset(state, ref)
    False -> Ok(#(None, state))
  })
  use #(second, state) <- result.try(read_int_field(state, ref, "second"))
  use #(tz_raw, state) <- result.try(case read_tz {
    True -> ops_object.get_value(state, ref, Named("timeZone"), JsObject(ref))
    False -> Ok(#(JsUndefined, state))
  })
  use #(year, state) <- result.map(read_int_field(state, ref, "year"))
  // ToTemporalTimeZoneIdentifier: a ZonedDateTime contributes its time zone.
  let tz = case tz_raw {
    JsObject(tz_ref) ->
      case heap.read(state.heap, tz_ref) {
        Some(ObjectSlot(kind: TemporalZonedDateTimeSlot(time_zone:, ..), ..)) ->
          JsString(time_zone)
        _ -> tz_raw
      }
    _ -> tz_raw
  }
  #(
    DateTimeFields(
      date: DateFields(day:, era:, era_year:, month:, month_code:, year:),
      time: TimeFields(hour:, minute:, second:, ms:, us:, ns:),
      offset:,
      tz:,
    ),
    state,
  )
}

/// Resolve the arithmetic year from year/era/eraYear fields. The fields must
/// contain a year (checked by the caller for TypeError ordering).
fn resolve_calendar_year(
  cal: tcal.Calendar,
  f: DateFields,
) -> Result(Int, TErr) {
  // era and eraYear must come as a pair.
  use Nil <- result.try(case f.era, f.era_year {
    Some(_), None | None, Some(_) ->
      Error(TypeE("era and eraYear must both be provided"))
    _, _ -> Ok(Nil)
  })
  case f.year, f.era, f.era_year {
    _, Some(era), Some(ey) ->
      // The era code is free-form user input; `parse_era_code` closes it, and
      // `year_for_era` says whether this calendar uses it. Both failures are
      // the same RangeError, raised here rather than at read time so the
      // era/eraYear TypeError above still wins.
      case
        tcal.parse_era_code(era)
        |> result.try(tcal.year_for_era(cal, _, ey))
      {
        Error(Nil) ->
          Error(RangeE(
            era <> " is not a valid era for calendar " <> tcal.identifier(cal),
          ))
        Ok(y2) ->
          case f.year {
            Some(y) if y != y2 ->
              Error(RangeE("era/eraYear inconsistent with year"))
            _ -> Ok(y2)
          }
      }
    Some(y), _, _ -> Ok(y)
    None, _, _ -> Error(TypeE("year is required"))
  }
}

/// Resolve the ordinal month within `year` from month/monthCode fields.
fn resolve_calendar_month(
  cal: tcal.Calendar,
  year: Int,
  f: DateFields,
  overflow: Overflow,
) -> Result(Int, TErr) {
  case f.month_code {
    Some(mc) -> {
      use ordinal <- result.try(case tcal.month_for_code(cal, year, mc) {
        Ok(o) -> Ok(o)
        Error(tcal.NeverValid) ->
          Error(RangeE(
            "monthCode is not valid for calendar " <> tcal.identifier(cal),
          ))
        Error(tcal.NotInThisYear(skip_to)) ->
          case overflow {
            Reject -> Error(RangeE("monthCode not present in year"))
            Constrain -> Ok(skip_to)
          }
      })
      case f.month {
        Some(m) if m != ordinal ->
          Error(RangeE("month and monthCode must agree"))
        _ -> Ok(ordinal)
      }
    }
    None ->
      case f.month {
        None -> Error(TypeE("month or monthCode is required"))
        Some(m) -> {
          let max = tcal.months_in_year(cal, year)
          case m > max {
            True ->
              case overflow {
                Reject -> Error(RangeE("month out of range"))
                Constrain -> Ok(max)
              }
            False -> Ok(m)
          }
        }
      }
  }
}

/// Full date resolution: fields -> ISO date (CalendarDateToISO).
fn resolve_calendar_date(
  cal: tcal.Calendar,
  f: DateFields,
  overflow: Overflow,
) -> Result(IsoDate, TErr) {
  // Required-field (TypeError) checks come before all RangeError checks.
  use Nil <- result.try(case f.year, f.era, f.era_year {
    None, None, None -> Error(TypeE("year is required"))
    _, _, _ -> Ok(Nil)
  })
  use Nil <- result.try(case f.month, f.month_code {
    None, None -> Error(TypeE("month or monthCode is required"))
    _, _ -> Ok(Nil)
  })
  use day <- result.try(case f.day {
    None -> Error(TypeE("day is required"))
    Some(d) -> Ok(d)
  })
  use y <- result.try(resolve_calendar_year(cal, f))
  case cal {
    tcal.Iso8601 -> {
      use m <- result.try(resolve_iso_month(f))
      regulate_iso_date(y, m, day, overflow)
    }
    _ -> {
      use m <- result.try(resolve_calendar_month(cal, y, f, overflow))
      use d <- result.try(regulate_calendar_day(cal, y, m, day, overflow))
      Ok(iso_date_from_epoch_days(tcal.date_to_epoch_days(cal, y, m, d)))
    }
  }
}

/// month/monthCode resolution for iso8601 (codes are plain ordinals).
fn resolve_iso_month(f: DateFields) -> Result(Int, TErr) {
  case f.month_code {
    Some(tcal.MonthCode(number: num, leap:)) ->
      case leap || num > 12 {
        True -> Error(RangeE("monthCode is not valid for calendar iso8601"))
        False ->
          case f.month {
            Some(m) if m != num ->
              Error(RangeE("month and monthCode must agree"))
            _ -> Ok(num)
          }
      }
    None ->
      case f.month {
        None -> Error(TypeE("month or monthCode is required"))
        Some(m) -> Ok(m)
      }
  }
}

fn regulate_calendar_day(
  cal: tcal.Calendar,
  year: Int,
  month: Int,
  day: Int,
  overflow: Overflow,
) -> Result(Int, TErr) {
  let max = tcal.days_in_month(cal, year, month)
  case day >= 1 && day <= max {
    True -> Ok(day)
    False ->
      case overflow {
        Reject -> Error(RangeE("day out of range"))
        Constrain -> Ok(int.clamp(day, 1, max))
      }
  }
}

/// CalendarDateAdd: add a duration's years/months/weeks/days to an ISO date
/// interpreted in `cal`.
fn calendar_date_add(
  cal: tcal.Calendar,
  d: IsoDate,
  dur: DurRec,
  overflow: Overflow,
) -> Result(IsoDate, TErr) {
  case cal {
    tcal.Iso8601 -> add_duration_to_date(d, dur, overflow)
    _ -> {
      let cd = tcal.date_from_epoch_days(cal, epoch_days(d))
      // Add years keeping the month code (leap months constrain forward).
      let y1 = cd.year + dur.years
      use m1 <- result.try(case dur.years == 0 {
        True -> Ok(cd.month)
        False -> {
          let mc = tcal.month_code_of(cal, cd.year, cd.month)
          case tcal.carry_month_code(cal, y1, mc) {
            Ok(o) -> Ok(o)
            Error(skip_to) ->
              case overflow {
                Reject -> Error(RangeE("month not present in year"))
                Constrain -> Ok(skip_to)
              }
          }
        }
      })
      // Add months ordinally, balancing across variable-length years.
      let #(y2, m2) = balance_calendar_month(cal, y1, m1 + dur.months)
      // Regulate the day, convert back to ISO, then add weeks/days.
      use d2 <- result.try(regulate_calendar_day(cal, y2, m2, cd.day, overflow))
      let days = tcal.date_to_epoch_days(cal, y2, m2, d2)
      let extra =
        dur.weeks * 7 + dur.days + truncate_div(time_only_ns(dur), ns_per_day)
      let final = iso_date_from_epoch_days(days + extra)
      check_date_limits(final)
    }
  }
}

fn balance_calendar_month(
  cal: tcal.Calendar,
  year: Int,
  month: Int,
) -> #(Int, Int) {
  case month < 1 {
    True ->
      balance_calendar_month(
        cal,
        year - 1,
        month + tcal.months_in_year(cal, year - 1),
      )
    False -> {
      let max = tcal.months_in_year(cal, year)
      case month > max {
        True -> balance_calendar_month(cal, year + 1, month - max)
        False -> #(year, month)
      }
    }
  }
}

/// CalendarDateUntil for years/months in calendar space. Returns
/// #(years, months, day_remainder) — weeks/days handled by the caller from
/// the day remainder.
fn calendar_date_until(
  cal: tcal.Calendar,
  from: IsoDate,
  to: IsoDate,
  largest_unit: Unit,
) -> #(Int, Int, Int) {
  let from_days = epoch_days(from)
  let to_days = epoch_days(to)
  let sign = case to_days >= from_days {
    True -> 1
    False -> -1
  }
  let cd1 = tcal.date_from_epoch_days(cal, from_days)
  let cd2 = tcal.date_from_epoch_days(cal, to_days)
  // Count whole years (only when largestUnit is years).
  let years = case largest_unit {
    Year -> count_calendar_years(cal, cd1, cd2, cd2.year - cd1.year, sign)
    _ -> 0
  }
  let after_years = add_calendar_years_constrain(cal, cd1, years)
  // Count whole months. Comparisons use the receiver's original (possibly
  // unconstrained) day — a month only counts once the same day-of-month is
  // reached, per DifferenceISODate / CalendarDateUntil.
  let months = count_calendar_months(cal, after_years, cd1.day, cd2, sign, 0)
  let #(ym, mm) =
    balance_calendar_month(cal, after_years.year, after_years.month + months)
  let dmax = tcal.days_in_month(cal, ym, mm)
  // Constrain the receiver's ORIGINAL day into the final month — the day
  // constrain happens once from the original fields, not cascaded through
  // the intermediate year step (30th Esfand minus 60 years lands on the
  // 30th of the target month, even when the stepped year's Esfand has 29).
  let dd = int.min(cd1.day, dmax)
  let intermediate = tcal.date_to_epoch_days(cal, ym, mm, dd)
  #(years, months, to_days - intermediate)
}

/// Lexicographic comparison of two (year-ish, month-ish, day) triples.
fn compare_triple(a: #(Int, Int, Int), b: #(Int, Int, Int)) -> Int {
  let #(a1, a2, a3) = a
  let #(b1, b2, b3) = b
  case a1 == b1, a2 == b2 {
    False, _ -> int_sign(a1 - b1)
    True, False -> int_sign(a2 - b2)
    True, True -> int_sign(a3 - b3)
  }
}

/// Sort position of a month within a year that is comparable across years
/// of the same calendar: a leap month sorts between its base month and the
/// next one (M05 < M05L < M06).
fn month_code_pos(cal: tcal.Calendar, year: Int, month: Int) -> Int {
  let tcal.MonthCode(number: num, leap:) = tcal.month_code_of(cal, year, month)
  case leap {
    True -> num * 2 + 1
    False -> num * 2
  }
}

/// Add years to a calendar date keeping month code (constrain semantics).
fn add_calendar_years_constrain(
  cal: tcal.Calendar,
  cd: tcal.CalDate,
  years: Int,
) -> tcal.CalDate {
  let y = cd.year + years
  let mc = tcal.month_code_of(cal, cd.year, cd.month)
  let m = case tcal.carry_month_code(cal, y, mc) {
    Ok(o) -> o
    Error(skip_to) -> skip_to
  }
  let d = int.min(cd.day, tcal.days_in_month(cal, y, m))
  tcal.CalDate(y, m, d)
}

/// Count whole years from cd1 toward cd2. A year only counts when the
/// stepped (year, monthCode position, day) triple does not surpass the
/// target — the day is compared WITHOUT constraining into the stepped year
/// (e.g. Iyyar 30 of an islamic leap year to Iyyar 29 a year later is
/// 11 months and days, not one year), while a leap month code missing from
/// the stepped year constrains in the direction of travel.
fn count_calendar_years(
  cal: tcal.Calendar,
  cd1: tcal.CalDate,
  cd2: tcal.CalDate,
  candidate: Int,
  sign: Int,
) -> Int {
  case candidate * sign < 0 {
    True -> 0
    False -> {
      let pos1 = stepped_month_pos(cal, cd1, cd1.year + candidate, sign)
      let pos2 = month_code_pos(cal, cd2.year, cd2.month)
      let cmp =
        compare_triple(#(cd1.year + candidate, pos1, cd1.day), #(
          cd2.year,
          pos2,
          cd2.day,
        ))
      case cmp * sign > 0 {
        True -> count_calendar_years(cal, cd1, cd2, candidate - sign, sign)
        False -> candidate
      }
    }
  }
}

/// Month position of cd1's month code carried into `target_year`. When a
/// leap month (e.g. hebrew M05L) does not exist in the stepped year, it
/// constrains in the direction of travel: forward to the following month
/// (skip-forward), backward to the preceding base month.
fn stepped_month_pos(
  cal: tcal.Calendar,
  cd1: tcal.CalDate,
  target_year: Int,
  sign: Int,
) -> Int {
  let mc = tcal.month_code_of(cal, cd1.year, cd1.month)
  let tcal.MonthCode(number: num, leap:) = mc
  case leap {
    False -> num * 2
    True ->
      case tcal.carry_month_code(cal, target_year, mc) {
        Ok(_) -> num * 2 + 1
        Error(_) ->
          case sign > 0 {
            True -> num * 2 + 2
            False -> num * 2
          }
      }
  }
}

/// Count whole months from cd toward cd2; `day_cmp` is the original
/// (unconstrained) day-of-month used for the surpass comparison.
/// Carries the current (year, month) position through the walk so each
/// step is O(1) — re-balancing `acc` months from cd on every iteration
/// made large spans accidentally quadratic.
fn count_calendar_months(
  cal: tcal.Calendar,
  cd: tcal.CalDate,
  day_cmp: Int,
  cd2: tcal.CalDate,
  sign: Int,
  acc: Int,
) -> Int {
  let #(y, m) = balance_calendar_month(cal, cd.year, cd.month + acc)
  count_calendar_months_loop(cal, y, m, day_cmp, cd2, sign, acc)
}

fn count_calendar_months_loop(
  cal: tcal.Calendar,
  y: Int,
  m: Int,
  day_cmp: Int,
  cd2: tcal.CalDate,
  sign: Int,
  acc: Int,
) -> Int {
  let #(ny, nm) = step_calendar_month(cal, y, m, sign)
  let cmp = compare_triple(#(ny, nm, day_cmp), #(cd2.year, cd2.month, cd2.day))
  case cmp * sign <= 0 {
    True ->
      count_calendar_months_loop(cal, ny, nm, day_cmp, cd2, sign, acc + sign)
    False -> acc
  }
}

/// Step a valid (year, month) pair by exactly one month in `sign` direction,
/// wrapping across variable-length years.
fn step_calendar_month(
  cal: tcal.Calendar,
  y: Int,
  m: Int,
  sign: Int,
) -> #(Int, Int) {
  case sign > 0 {
    True ->
      case m >= tcal.months_in_year(cal, y) {
        True -> #(y + 1, 1)
        False -> #(y, m + 1)
      }
    False ->
      case m <= 1 {
        True -> #(y - 1, tcal.months_in_year(cal, y - 1))
        False -> #(y, m - 1)
      }
  }
}

/// ToTemporalDate(item [, options]) — returns the ISO date + calendar.
/// Reads + validates options AFTER item conversion, per spec order.
fn to_temporal_date(
  state: State(host),
  item: JsValue,
  options: JsValue,
) -> Result(#(#(IsoDate, tcal.Calendar), State(host)), #(JsValue, State(host))) {
  case item {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(
          kind: TemporalDateSlot(year:, month:, day:, calendar:),
          ..,
        )) -> {
          use #(_opts, st) <- result.try(validated_overflow(state, options))
          Ok(#(#(IsoDate(year, month, day), calendar), st))
        }
        Some(ObjectSlot(
          kind: TemporalDateTimeSlot(year:, month:, day:, calendar:, ..),
          ..,
        )) -> {
          use #(_opts, st) <- result.try(validated_overflow(state, options))
          Ok(#(#(IsoDate(year, month, day), calendar), st))
        }
        Some(ObjectSlot(
          kind: TemporalZonedDateTimeSlot(epoch_ns:, time_zone:, calendar:),
          ..,
        )) -> {
          use #(_opts, st) <- result.try(validated_overflow(state, options))
          use #(d, _) <- terr_r(st, epoch_ns_to_iso_in(time_zone, epoch_ns))
          Ok(#(#(d, calendar), st))
        }
        _ -> date_from_bag(state, ref, options)
      }
    JsString(s) -> {
      use p <- terr_r(state, parse_plain_datetime_string(s))
      case p.date {
        Some(d) -> {
          use cal <- terr_r(state, parsed_calendar_id(p))
          use #(_opts, st) <- result.try(validated_overflow(state, options))
          case iso_date_within_limits(d) {
            True -> Ok(#(#(d, cal), st))
            False -> range_error_result(st, "date outside of supported range")
          }
        }
        None -> range_error_result(state, "invalid date string")
      }
    }
    _ -> type_error_result(state, "cannot convert to a Temporal.PlainDate")
  }
}

/// Canonical calendar id from a parsed ISO string's annotation.
fn parsed_calendar_id(p: ParsedIso) -> Result(tcal.Calendar, TErr) {
  case p.calendar {
    None -> Ok(tcal.Iso8601)
    Some(c) -> canonicalize_calendar(c)
  }
}

/// `use`-style adapter: Result(a, TErr) -> Result(#(b, State), ...).
fn terr_r(
  state: State(host),
  r: Result(a, TErr),
  k: fn(a) -> Result(#(b, State(host)), #(JsValue, State(host))),
) -> Result(#(b, State(host)), #(JsValue, State(host))) {
  case r {
    Ok(v) -> k(v)
    Error(e) -> throw_terr_op(state, e)
  }
}

/// Helper: validate options object + read overflow (result often unused for
/// instance copies, but the validation is observable).
fn validated_overflow(
  state: State(host),
  options: JsValue,
) -> Result(#(Overflow, State(host)), #(JsValue, State(host))) {
  use #(opts, st) <- result.try(get_options_object(state, options))
  get_overflow_option(st, opts)
}

/// Shared preamble for the `add`/`subtract` instance methods: read the duration
/// argument, validate the options bag's `overflow`, and negate the duration
/// when the method is `subtract`.
fn add_sub_args(
  state: State(host),
  args: List(JsValue),
  is_subtract: Bool,
  k: fn(DurRec, Overflow, State(host)) -> #(State(host), Result(b, JsValue)),
) -> #(State(host), Result(b, JsValue)) {
  use dur, state <- state.try_op(to_temporal_duration(
    state,
    helpers.arg_at(args, 0),
  ))
  use overflow, state <- state.try_op(validated_overflow(
    state,
    helpers.arg_at(args, 1),
  ))
  let dur = case is_subtract {
    True -> negate_dur(dur)
    False -> dur
  }
  k(dur, overflow, state)
}

/// Property-bag → ISO date + calendar. Field read order: calendar, then
/// alphabetical (day, era, eraYear, month, monthCode, year).
fn date_from_bag(
  state: State(host),
  ref: Ref,
  options: JsValue,
) -> Result(#(#(IsoDate, tcal.Calendar), State(host)), #(JsValue, State(host))) {
  use #(cal, state) <- result.try(read_bag_calendar(state, ref))
  use #(fields, state) <- result.try(read_date_fields(state, ref, cal))
  use #(overflow, state) <- result.try(validated_overflow(state, options))
  use date <- terr_r(state, resolve_calendar_date(cal, fields, overflow))
  case iso_date_within_limits(date) {
    True -> Ok(#(#(date, cal), state))
    False -> range_error_result(state, "date outside of supported range")
  }
}

/// ToTemporalTime(item [, options]).
fn to_temporal_time(
  state: State(host),
  item: JsValue,
  options: JsValue,
) -> Result(#(TimeRec, State(host)), #(JsValue, State(host))) {
  case item {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(
          kind: TemporalTimeSlot(
            hour:,
            minute:,
            second:,
            millisecond:,
            microsecond:,
            nanosecond:,
          ),
          ..,
        )) -> {
          use #(_o, st) <- result.try(validated_overflow(state, options))
          Ok(#(
            TimeRec(hour, minute, second, millisecond, microsecond, nanosecond),
            st,
          ))
        }
        Some(ObjectSlot(
          kind: TemporalDateTimeSlot(
            hour:,
            minute:,
            second:,
            millisecond:,
            microsecond:,
            nanosecond:,
            ..,
          ),
          ..,
        )) -> {
          use #(_o, st) <- result.try(validated_overflow(state, options))
          Ok(#(
            TimeRec(hour, minute, second, millisecond, microsecond, nanosecond),
            st,
          ))
        }
        Some(ObjectSlot(
          kind: TemporalZonedDateTimeSlot(epoch_ns:, time_zone:, calendar: _),
          ..,
        )) -> {
          use #(_o, st) <- result.try(validated_overflow(state, options))
          use #(_, t) <- terr_r(st, epoch_ns_to_iso_in(time_zone, epoch_ns))
          Ok(#(t, st))
        }
        _ -> time_from_bag(state, ref, options)
      }
    JsString(s) -> {
      use t <- terr_r(state, parse_time_string(s))
      use #(_o, st) <- result.try(validated_overflow(state, options))
      Ok(#(t, st))
    }
    _ -> type_error_result(state, "cannot convert to a Temporal.PlainTime")
  }
}

/// TemporalTimeString — time-only (optional T prefix) or full date-time.
fn parse_time_string(s: String) -> Result(TimeRec, TErr) {
  // Try a full date-time string first.
  case parse_iso_datetime_string(s) {
    Some(p) ->
      case p.z {
        True -> Error(RangeE("Z designator not valid for PlainTime"))
        False ->
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
  // Optional offset (not Z).
  let rest = case parse_offset_part(rest) {
    Some(#(True, _, _, _)) -> "###invalid###"
    Some(#(False, _, _, r)) -> r
    None -> rest
  }
  use #(_, _cal, rest2) <- option.then(parse_annotations(
    rest,
    None,
    None,
    False,
  ))
  // The calendar annotation value is not validated for time-only strings
  // (ToTemporalTime ignores it entirely).
  case rest2 {
    "" -> Some(t)
    _ -> None
  }
}

/// A time-only string that also matches YYYY-MM / MMDD / MM-DD date syntax is
/// ambiguous and must be rejected (spec: ParseISODateTime ambiguity rules).
fn time_string_is_ambiguous(s: String) -> Bool {
  // Strip annotations for the check.
  let base = case string.split_once(s, "[") {
    Ok(#(b, _)) -> b
    Error(Nil) -> s
  }
  is_year_month_like(base) || is_month_day_like(base)
}

fn is_year_month_like(s: String) -> Bool {
  case take_digits(s, 4) {
    Some(#(_, "")) -> False
    Some(#(_, rest)) ->
      case rest {
        "-" <> r ->
          case take_digits(r, 2) {
            Some(#(m, "")) -> m >= 1 && m <= 12
            _ -> False
          }
        _ ->
          case take_digits(rest, 2) {
            Some(#(m, "")) -> m >= 1 && m <= 12
            _ -> False
          }
      }
    None -> False
  }
}

fn is_month_day_like(s: String) -> Bool {
  let s = case s {
    "--" <> r -> r
    _ -> s
  }
  case take_digits(s, 2) {
    Some(#(m, rest)) -> {
      let rest = case rest {
        "-" <> r -> r
        _ -> rest
      }
      case take_digits(rest, 2) {
        // Reference year 1972 (leap) — "0229" is a possible month-day but
        // "0230" is not, so the latter is unambiguously a time.
        Some(#(d, "")) ->
          m >= 1 && m <= 12 && d >= 1 && d <= days_in_month(1972, m)
        _ -> False
      }
    }
    None -> False
  }
}

/// Property bag → TimeRec. Alphabetical: hour, microsecond, millisecond,
/// minute, nanosecond, second. At least one required.
fn time_from_bag(
  state: State(host),
  ref: Ref,
  options: JsValue,
) -> Result(#(TimeRec, State(host)), #(JsValue, State(host))) {
  use #(f, state) <- result.try(read_time_fields(state, ref))
  case f == no_time_fields {
    True ->
      type_error_result(state, "invalid property bag for Temporal.PlainTime")
    False -> {
      use #(overflow, state) <- result.try(validated_overflow(state, options))
      let t0 = time_fields_apply(f, midnight)
      use t <- terr_r(state, regulate_time(t0, overflow))
      Ok(#(t, state))
    }
  }
}

/// ToTemporalDuration(item).
fn to_temporal_duration(
  state: State(host),
  item: JsValue,
) -> Result(#(DurRec, State(host)), #(JsValue, State(host))) {
  case item {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(
          kind: TemporalDurationSlot(
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
          ),
          ..,
        )) ->
          Ok(#(
            DurRec(
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
            ),
            state,
          ))
        _ -> duration_from_bag(state, ref)
      }
    JsString(s) ->
      case parse_duration_string(s) {
        Some(d) ->
          case is_valid_duration(d) {
            True -> Ok(#(d, state))
            False -> range_error_result(state, "invalid duration")
          }
        None -> range_error_result(state, "invalid duration string: " <> s)
      }
    _ -> type_error_result(state, "cannot convert to a Temporal.Duration")
  }
}

/// Duration property bag — alphabetical field order; at least one required.
fn duration_from_bag(
  state: State(host),
  ref: Ref,
) -> Result(#(DurRec, State(host)), #(JsValue, State(host))) {
  use #(days, state) <- result.try(read_integral_int_field(state, ref, "days"))
  use #(hours, state) <- result.try(read_integral_int_field(state, ref, "hours"))
  use #(us, state) <- result.try(read_integral_int_field(
    state,
    ref,
    "microseconds",
  ))
  use #(ms, state) <- result.try(read_integral_int_field(
    state,
    ref,
    "milliseconds",
  ))
  use #(minutes, state) <- result.try(read_integral_int_field(
    state,
    ref,
    "minutes",
  ))
  use #(months, state) <- result.try(read_integral_int_field(
    state,
    ref,
    "months",
  ))
  use #(ns, state) <- result.try(read_integral_int_field(
    state,
    ref,
    "nanoseconds",
  ))
  use #(seconds, state) <- result.try(read_integral_int_field(
    state,
    ref,
    "seconds",
  ))
  use #(weeks, state) <- result.try(read_integral_int_field(state, ref, "weeks"))
  use #(years, state) <- result.try(read_integral_int_field(state, ref, "years"))
  let all = [days, hours, us, ms, minutes, months, ns, seconds, weeks, years]
  case list.all(all, fn(f) { f == None }) {
    True ->
      type_error_result(state, "invalid property bag for Temporal.Duration")
    False -> {
      let d =
        DurRec(
          years: option.unwrap(years, 0),
          months: option.unwrap(months, 0),
          weeks: option.unwrap(weeks, 0),
          days: option.unwrap(days, 0),
          hours: option.unwrap(hours, 0),
          minutes: option.unwrap(minutes, 0),
          seconds: option.unwrap(seconds, 0),
          ms: option.unwrap(ms, 0),
          us: option.unwrap(us, 0),
          ns: option.unwrap(ns, 0),
        )
      case is_valid_duration(d) {
        True -> Ok(#(d, state))
        False -> range_error_result(state, "invalid duration")
      }
    }
  }
}

/// ISO 8601 duration string: [+-]P[nY][nM][nW][nD][T[nH][nM][nS]] with an
/// optional fraction on the smallest present time unit.
fn parse_duration_string(s: String) -> Option(DurRec) {
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

fn parse_duration_date_units(s: String, sign: Int) -> Option(DurRec) {
  let #(years, s) = parse_dur_unit(s, ["Y", "y"])
  let #(months, s) = parse_dur_unit(s, ["M", "m"])
  let #(weeks, s) = parse_dur_unit(s, ["W", "w"])
  let #(days, s) = parse_dur_unit(s, ["D", "d"])
  case s {
    "" ->
      case years == None && months == None && weeks == None && days == None {
        True -> None
        False ->
          Some(apply_dur_sign(
            DurRec(
              years: option.unwrap(years, 0),
              months: option.unwrap(months, 0),
              weeks: option.unwrap(weeks, 0),
              days: option.unwrap(days, 0),
              hours: 0,
              minutes: 0,
              seconds: 0,
              ms: 0,
              us: 0,
              ns: 0,
            ),
            sign,
          ))
      }
    "T" <> r | "t" <> r -> {
      use #(h, mi, sec, sub_ns) <- option.then(parse_duration_time_units(r))
      Some(apply_dur_sign(
        DurRec(
          years: option.unwrap(years, 0),
          months: option.unwrap(months, 0),
          weeks: option.unwrap(weeks, 0),
          days: option.unwrap(days, 0),
          hours: h,
          minutes: mi,
          seconds: sec,
          ms: sub_ns / ns_per_ms,
          us: { sub_ns % ns_per_ms } / ns_per_us,
          ns: sub_ns % ns_per_us,
        ),
        sign,
      ))
    }
    _ -> None
  }
}

/// Integer (no fraction) date unit: returns the value if `s` starts with
/// digits followed by one of `designators`.
fn parse_dur_unit(
  s: String,
  designators: List(String),
) -> #(Option(Int), String) {
  case take_some_digits(s, 16) {
    Some(#(v, _, rest)) -> {
      case list.find(designators, fn(d) { string.starts_with(rest, d) }) {
        Ok(d) -> #(Some(v), string.drop_start(rest, string.length(d)))
        Error(Nil) -> #(None, s)
      }
    }
    None -> #(None, s)
  }
}

/// Time units: hours/minutes/seconds; fraction allowed only on the last unit
/// present. Returns total #(hours, minutes, seconds, sub_second_ns).
fn parse_duration_time_units(s: String) -> Option(#(Int, Int, Int, Int)) {
  use #(h, h_frac, s1) <- option.then(parse_dur_time_unit(s, ["H", "h"]))
  case h_frac {
    Some(f) ->
      // Fractional hours: nothing may follow; convert to mi/s/ns exactly.
      case s1 {
        "" -> {
          let total_ns = f * 3600 / 1
          let mi = total_ns / ns_per_minute
          let rem = total_ns - mi * ns_per_minute
          let sec = rem / ns_per_second
          Some(#(option.unwrap(h, 0), mi, sec, rem - sec * ns_per_second))
        }
        _ -> None
      }
    None -> {
      use #(mi, mi_frac, s2) <- option.then(parse_dur_time_unit(s1, ["M", "m"]))
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
            parse_dur_time_unit(s2, ["S", "s"]),
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

/// One time unit with optional fraction → #(value, fraction_ns, rest).
fn parse_dur_time_unit(
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
          // Designator mismatch → backtrack so the caller can try the next
          // unit (e.g. "0.5S" probed by the hours parser). The "fraction must
          // be on the last unit" rule is enforced by the caller: any
          // non-empty remainder after a fractional unit is rejected.
          Some(#(None, None, s))
      }
    }
  }
}

fn apply_dur_sign(d: DurRec, sign: Int) -> DurRec {
  case sign < 0 {
    False -> d
    True ->
      DurRec(
        years: 0 - d.years,
        months: 0 - d.months,
        weeks: 0 - d.weeks,
        days: 0 - d.days,
        hours: 0 - d.hours,
        minutes: 0 - d.minutes,
        seconds: 0 - d.seconds,
        ms: 0 - d.ms,
        us: 0 - d.us,
        ns: 0 - d.ns,
      )
  }
}

// ============================================================================
// Static methods
// ============================================================================

fn static_dispatch(
  kind: TemporalKind,
  name: String,
  protos: TemporalProtos,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case kind, name {
    TemporalPlainDateKind, "from" -> {
      use #(d, cal), state <- state.try_op(to_temporal_date(
        state,
        helpers.arg_at(args, 0),
        helpers.arg_at(args, 1),
      ))
      let #(state, v) = make_date_cal(state, protos, d, cal)
      #(state, Ok(v))
    }
    TemporalPlainDateKind, "compare" -> {
      use #(a, _), state <- state.try_op(to_temporal_date(
        state,
        helpers.arg_at(args, 0),
        JsUndefined,
      ))
      use #(b, _), state <- state.try_op(to_temporal_date(
        state,
        helpers.arg_at(args, 1),
        JsUndefined,
      ))
      #(state, Ok(value.from_int(compare_iso_date(a, b))))
    }
    TemporalPlainTimeKind, "from" -> {
      use t, state <- state.try_op(to_temporal_time(
        state,
        helpers.arg_at(args, 0),
        helpers.arg_at(args, 1),
      ))
      let #(state, v) = make_time(state, protos, t)
      #(state, Ok(v))
    }
    TemporalPlainTimeKind, "compare" -> {
      use a, state <- state.try_op(to_temporal_time(
        state,
        helpers.arg_at(args, 0),
        JsUndefined,
      ))
      use b, state <- state.try_op(to_temporal_time(
        state,
        helpers.arg_at(args, 1),
        JsUndefined,
      ))
      #(state, Ok(value.from_int(int_sign(time_to_ns(a) - time_to_ns(b)))))
    }
    TemporalPlainDateTimeKind, "from" -> {
      use #(d, t, cal), state <- state.try_op(to_temporal_date_time(
        state,
        helpers.arg_at(args, 0),
        helpers.arg_at(args, 1),
      ))
      let #(state, v) = make_date_time_cal(state, protos, d, t, cal)
      #(state, Ok(v))
    }
    TemporalPlainDateTimeKind, "compare" -> {
      use #(ad, at, _), state <- state.try_op(to_temporal_date_time(
        state,
        helpers.arg_at(args, 0),
        JsUndefined,
      ))
      use #(bd, bt, _), state <- state.try_op(to_temporal_date_time(
        state,
        helpers.arg_at(args, 1),
        JsUndefined,
      ))
      #(state, Ok(value.from_int(compare_iso_date_time(#(ad, at), #(bd, bt)))))
    }
    TemporalPlainYearMonthKind, "from" -> {
      use #(y, m, rd, cal), state <- state.try_op(to_temporal_year_month(
        state,
        helpers.arg_at(args, 0),
        helpers.arg_at(args, 1),
      ))
      let #(state, v) = make_year_month_cal(state, protos, y, m, rd, cal)
      #(state, Ok(v))
    }
    TemporalPlainYearMonthKind, "compare" -> {
      use a, state <- state.try_op(to_temporal_year_month(
        state,
        helpers.arg_at(args, 0),
        JsUndefined,
      ))
      use b, state <- state.try_op(to_temporal_year_month(
        state,
        helpers.arg_at(args, 1),
        JsUndefined,
      ))
      // CompareISODate including the reference day.
      let n = compare_iso_date(IsoDate(a.0, a.1, a.2), IsoDate(b.0, b.1, b.2))
      #(state, Ok(value.from_int(n)))
    }
    TemporalPlainMonthDayKind, "from" -> {
      use #(m, d, ry, cal), state <- state.try_op(to_temporal_month_day(
        state,
        helpers.arg_at(args, 0),
        helpers.arg_at(args, 1),
      ))
      let #(state, v) = make_month_day_cal(state, protos, m, d, ry, cal)
      #(state, Ok(v))
    }
    TemporalDurationKind, "from" -> {
      use d, state <- state.try_op(to_temporal_duration(
        state,
        helpers.arg_at(args, 0),
      ))
      let #(state, v) = make_duration(state, protos, d)
      #(state, Ok(v))
    }
    TemporalDurationKind, "compare" -> duration_compare(args, state)
    TemporalInstantKind, "from" -> {
      use ns, state <- state.try_op(to_temporal_instant(
        state,
        helpers.arg_at(args, 0),
      ))
      let #(state, v) = make_instant(state, protos, ns)
      #(state, Ok(v))
    }
    TemporalInstantKind, "fromEpochMilliseconds" -> {
      use n, state <- state.try_op(coerce.js_to_number(
        state,
        helpers.arg_at(args, 0),
      ))
      case n {
        Finite(f) -> {
          // -0 IS an integral Number, so this needs the ±0-safe predicate.
          case value.integral_int(f) {
            None -> state.range_error(state, "not an integral number")
            Some(i) -> {
              let ns = i * ns_per_ms
              case int.absolute_value(ns) <= ns_max_instant {
                False ->
                  state.range_error(state, "epoch milliseconds out of range")
                True -> {
                  let #(state, v) = make_instant(state, protos, ns)
                  #(state, Ok(v))
                }
              }
            }
          }
        }
        _ -> state.range_error(state, "not a finite number")
      }
    }
    TemporalInstantKind, "fromEpochNanoseconds" -> {
      use ns, state <- state.try_op(coerce.to_bigint(
        state,
        helpers.arg_at(args, 0),
      ))
      case int.absolute_value(ns) <= ns_max_instant {
        False -> state.range_error(state, "epoch nanoseconds out of range")
        True -> {
          let #(state, v) = make_instant(state, protos, ns)
          #(state, Ok(v))
        }
      }
    }
    TemporalInstantKind, "compare" -> {
      use a, state <- state.try_op(to_temporal_instant(
        state,
        helpers.arg_at(args, 0),
      ))
      use b, state <- state.try_op(to_temporal_instant(
        state,
        helpers.arg_at(args, 1),
      ))
      #(state, Ok(value.from_int(int_sign(a - b))))
    }
    TemporalZonedDateTimeKind, "from" -> {
      use #(ns, tz, cal), state <- state.try_op(to_temporal_zoned(
        state,
        helpers.arg_at(args, 0),
        helpers.arg_at(args, 1),
      ))
      let #(state, v) = make_zoned_cal(state, protos, ns, tz, cal)
      #(state, Ok(v))
    }
    TemporalZonedDateTimeKind, "compare" -> {
      use #(a, _, _), state <- state.try_op(to_temporal_zoned(
        state,
        helpers.arg_at(args, 0),
        JsUndefined,
      ))
      use #(b, _, _), state <- state.try_op(to_temporal_zoned(
        state,
        helpers.arg_at(args, 1),
        JsUndefined,
      ))
      #(state, Ok(value.from_int(int_sign(a - b))))
    }
    _, _ -> state.type_error(state, "unknown Temporal static method")
  }
}

fn int_sign(n: Int) -> Int {
  case n > 0 {
    True -> 1
    False ->
      case n < 0 {
        True -> -1
        False -> 0
      }
  }
}

fn compare_iso_date(a: IsoDate, b: IsoDate) -> Int {
  int_sign(epoch_days(a) - epoch_days(b))
}

fn compare_iso_date_time(
  a: #(IsoDate, TimeRec),
  b: #(IsoDate, TimeRec),
) -> Int {
  int_sign(utc_epoch_ns(a.0, a.1) - utc_epoch_ns(b.0, b.1))
}

/// ToTemporalDateTime(item [, options]).
fn to_temporal_date_time(
  state: State(host),
  item: JsValue,
  options: JsValue,
) -> Result(
  #(#(IsoDate, TimeRec, tcal.Calendar), State(host)),
  #(JsValue, State(host)),
) {
  case item {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(
          kind: TemporalDateTimeSlot(
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
          ),
          ..,
        )) -> {
          use #(_o, st) <- result.try(validated_overflow(state, options))
          let t =
            TimeRec(hour, minute, second, millisecond, microsecond, nanosecond)
          Ok(#(#(IsoDate(year, month, day), t, calendar), st))
        }
        Some(ObjectSlot(
          kind: TemporalDateSlot(year:, month:, day:, calendar:),
          ..,
        )) -> {
          use #(_o, st) <- result.try(validated_overflow(state, options))
          Ok(#(#(IsoDate(year, month, day), midnight, calendar), st))
        }
        Some(ObjectSlot(
          kind: TemporalZonedDateTimeSlot(epoch_ns:, time_zone:, calendar:),
          ..,
        )) -> {
          use #(_o, st) <- result.try(validated_overflow(state, options))
          use #(d, t) <- terr_r(st, epoch_ns_to_iso_in(time_zone, epoch_ns))
          Ok(#(#(d, t, calendar), st))
        }
        _ -> date_time_from_bag(state, ref, options)
      }
    JsString(s) -> {
      use p <- terr_r(state, parse_plain_datetime_string(s))
      case p.date {
        Some(d) -> {
          let t = option.unwrap(p.time, midnight)
          use cal <- terr_r(state, parsed_calendar_id(p))
          use #(_o, st) <- result.try(validated_overflow(state, options))
          case iso_datetime_within_limits(d, t) {
            True -> Ok(#(#(d, t, cal), st))
            False -> range_error_result(st, "date-time outside supported range")
          }
        }
        None -> range_error_result(state, "invalid date-time string")
      }
    }
    _ -> type_error_result(state, "cannot convert to a Temporal.PlainDateTime")
  }
}

/// Property bag → date-time. Fields: calendar, then alphabetical (day, era,
/// eraYear, hour, microsecond, millisecond, minute, month, monthCode,
/// nanosecond, second, year).
fn date_time_from_bag(
  state: State(host),
  ref: Ref,
  options: JsValue,
) -> Result(
  #(#(IsoDate, TimeRec, tcal.Calendar), State(host)),
  #(JsValue, State(host)),
) {
  use #(cal, state) <- result.try(read_bag_calendar(state, ref))
  use #(f, state) <- result.try(read_date_time_fields(
    state,
    ref,
    cal,
    read_offset: False,
    read_tz: False,
  ))
  use #(overflow, state) <- result.try(validated_overflow(state, options))
  use date <- terr_r(state, resolve_calendar_date(cal, f.date, overflow))
  let t0 = time_fields_apply(f.time, midnight)
  use t <- terr_r(state, regulate_time(t0, overflow))
  case iso_datetime_within_limits(date, t) {
    True -> Ok(#(#(date, t, cal), state))
    False -> range_error_result(state, "date-time outside supported range")
  }
}

/// ToTemporalYearMonth(item [, options]).
fn to_temporal_year_month(
  state: State(host),
  item: JsValue,
  options: JsValue,
) -> Result(
  #(#(Int, Int, Int, tcal.Calendar), State(host)),
  #(JsValue, State(host)),
) {
  case item {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(
          kind: TemporalYearMonthSlot(year:, month:, day:, calendar:),
          ..,
        )) -> {
          use #(_o, st) <- result.try(validated_overflow(state, options))
          Ok(#(#(year, month, day, calendar), st))
        }
        _ -> year_month_from_bag(state, ref, options)
      }
    JsString(s) -> {
      use #(y, m, rd, cal) <- terr_r(state, parse_year_month_string(s))
      use #(_o, st) <- result.try(validated_overflow(state, options))
      Ok(#(#(y, m, rd, cal), st))
    }
    _ -> type_error_result(state, "cannot convert to a Temporal.PlainYearMonth")
  }
}

fn parse_year_month_string(
  s: String,
) -> Result(#(Int, Int, Int, tcal.Calendar), TErr) {
  // YYYY-MM or YYYYMM (+ annotations), or any full date-time string.
  let ym = case parse_year_part(s) {
    Some(#(y, rest)) -> {
      let mm = case rest {
        "-" <> r -> take_digits(r, 2)
        _ -> take_digits(rest, 2)
      }
      case mm {
        Some(#(m, rest2)) ->
          case parse_annotations(rest2, None, None, False) {
            Some(#(_, cal, "")) -> Some(#(y, m, cal))
            _ -> None
          }
        None -> None
      }
    }
    None -> None
  }
  case ym {
    Some(#(y, m, cal)) ->
      case m >= 1 && m <= 12 {
        False -> Error(RangeE("invalid year-month string"))
        True ->
          case cal {
            None -> check_ym_limits(y, m, 1, tcal.Iso8601)
            Some(c) -> {
              use canon <- result.try(canonicalize_calendar(c))
              // Year-month-only strings are only valid for iso8601.
              case canon {
                tcal.Iso8601 -> check_ym_limits(y, m, 1, tcal.Iso8601)
                _ ->
                  Error(RangeE(
                    "year-month string requires a day for non-ISO calendars",
                  ))
              }
            }
          }
      }
    None -> {
      use p <- result.try(parse_plain_datetime_string(s))
      case p.date {
        Some(d) -> {
          use cal_id <- result.try(parsed_calendar_id(p))
          case cal_id {
            tcal.Iso8601 ->
              check_ym_limits(d.year, d.month, d.day, tcal.Iso8601)
            cal -> {
              // Reference day: first day of the calendar month
              // containing the parsed date.
              let cd = tcal.date_from_epoch_days(cal, epoch_days(d))
              let first =
                iso_date_from_epoch_days(tcal.date_to_epoch_days(
                  cal,
                  cd.year,
                  cd.month,
                  1,
                ))
              check_ym_limits(first.year, first.month, first.day, cal)
            }
          }
        }
        None -> Error(RangeE("invalid year-month string"))
      }
    }
  }
}

fn check_ym_limits(
  y: Int,
  m: Int,
  rd: Int,
  cal: tcal.Calendar,
) -> Result(#(Int, Int, Int, tcal.Calendar), TErr) {
  case iso_year_month_within_limits(y, m) {
    True -> Ok(#(y, m, rd, cal))
    False -> Error(RangeE("year-month outside of supported range"))
  }
}

/// Property bag → year-month. Fields: calendar, then era, eraYear, month,
/// monthCode, year. Returns the ISO date of the calendar month's first day.
fn year_month_from_bag(
  state: State(host),
  ref: Ref,
  options: JsValue,
) -> Result(
  #(#(Int, Int, Int, tcal.Calendar), State(host)),
  #(JsValue, State(host)),
) {
  use #(cal, state) <- result.try(read_bag_calendar(state, ref))
  use #(era, state) <- result.try(case tcal.has_eras(cal) {
    True -> read_bag_era(state, ref)
    False -> Ok(#(None, state))
  })
  use #(era_year, state) <- result.try(case tcal.has_eras(cal) {
    True -> read_int_field(state, ref, "eraYear")
    False -> Ok(#(None, state))
  })
  use #(month, state) <- result.try(read_pos_int_field(state, ref, "month"))
  use #(month_code, state) <- result.try(read_month_code(state, ref))
  use #(year, state) <- result.try(read_int_field(state, ref, "year"))
  use #(overflow, state) <- result.try(validated_overflow(state, options))
  let fields =
    DateFields(day: None, era:, era_year:, month:, month_code:, year:)
  use ym <- terr_r(state, resolve_calendar_year_month(cal, fields, overflow))
  Ok(#(ym, state))
}

/// Resolve year-month fields to the ISO date of the calendar month's first
/// day (day 1 for iso8601).
fn resolve_calendar_year_month(
  cal: tcal.Calendar,
  f: DateFields,
  overflow: Overflow,
) -> Result(#(Int, Int, Int, tcal.Calendar), TErr) {
  use Nil <- result.try(case f.year, f.era, f.era_year {
    None, None, None -> Error(TypeE("year is required"))
    _, _, _ -> Ok(Nil)
  })
  use Nil <- result.try(case f.month, f.month_code {
    None, None -> Error(TypeE("month or monthCode is required"))
    _, _ -> Ok(Nil)
  })
  use y <- result.try(resolve_calendar_year(cal, f))
  case cal {
    tcal.Iso8601 -> {
      use m <- result.try(resolve_iso_month(f))
      use m <- result.try(case m >= 1 && m <= 12 {
        True -> Ok(m)
        False ->
          case overflow {
            Reject -> Error(RangeE("invalid month"))
            Constrain -> Ok(int.clamp(m, 1, 12))
          }
      })
      check_ym_limits(y, m, 1, cal)
    }
    _ -> {
      use m <- result.try(resolve_calendar_month(cal, y, f, overflow))
      let first =
        iso_date_from_epoch_days(tcal.date_to_epoch_days(cal, y, m, 1))
      check_ym_limits(first.year, first.month, first.day, cal)
    }
  }
}

/// ToTemporalMonthDay(item [, options]).
fn to_temporal_month_day(
  state: State(host),
  item: JsValue,
  options: JsValue,
) -> Result(
  #(#(Int, Int, Int, tcal.Calendar), State(host)),
  #(JsValue, State(host)),
) {
  case item {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(
          kind: TemporalMonthDaySlot(month:, day:, ref_year:, calendar:),
          ..,
        )) -> {
          use #(_o, st) <- result.try(validated_overflow(state, options))
          Ok(#(#(month, day, ref_year, calendar), st))
        }
        _ -> month_day_from_bag(state, ref, options)
      }
    JsString(s) -> {
      use #(m, d, ry, cal) <- terr_r(state, parse_month_day_string(s))
      use #(_o, st) <- result.try(validated_overflow(state, options))
      Ok(#(#(m, d, ry, cal), st))
    }
    _ -> type_error_result(state, "cannot convert to a Temporal.PlainMonthDay")
  }
}

fn parse_month_day_string(
  s: String,
) -> Result(#(Int, Int, Int, tcal.Calendar), TErr) {
  // --MM-DD / --MMDD / MM-DD / MMDD (+ annotations), or full date-time.
  let body = case s {
    "--" <> r -> Some(r)
    _ -> Some(s)
  }
  let md = case body {
    Some(b) ->
      case take_digits(b, 2) {
        Some(#(m, rest)) -> {
          let dd = case rest {
            "-" <> r -> take_digits(r, 2)
            _ -> take_digits(rest, 2)
          }
          case dd {
            Some(#(d, rest2)) ->
              case parse_annotations(rest2, None, None, False) {
                Some(#(_, cal, "")) -> Some(#(m, d, cal))
                _ -> None
              }
            None -> None
          }
        }
        None -> None
      }
    None -> None
  }
  case md {
    Some(#(m, d, cal)) ->
      // Use a leap reference year so Feb 29 is valid.
      case is_valid_iso_date(1972, m, d) {
        False -> try_month_day_as_datetime(s)
        True ->
          case cal {
            None -> Ok(#(m, d, 1972, tcal.Iso8601))
            Some(c) -> {
              use canon <- result.try(canonicalize_calendar(c))
              // Month-day-only strings are only valid for iso8601.
              case canon {
                tcal.Iso8601 -> Ok(#(m, d, 1972, tcal.Iso8601))
                _ ->
                  Error(RangeE(
                    "month-day string requires a year for non-ISO calendars",
                  ))
              }
            }
          }
      }
    None -> try_month_day_as_datetime(s)
  }
}

fn try_month_day_as_datetime(
  s: String,
) -> Result(#(Int, Int, Int, tcal.Calendar), TErr) {
  use p <- result.try(parse_plain_datetime_string(s))
  case p.date {
    Some(d) -> {
      use cal_id <- result.try(parsed_calendar_id(p))
      case cal_id {
        tcal.Iso8601 -> Ok(#(d.month, d.day, 1972, tcal.Iso8601))
        cal -> {
          // ISODateWithinLimits before converting to calendar space:
          // e.g. -999999-01-01[u-ca=gregory] must throw RangeError.
          use Nil <- result.try(case iso_date_within_limits(d) {
            False -> Error(RangeE("date outside of supported range"))
            True -> Ok(Nil)
          })
          let cd = tcal.date_from_epoch_days(cal, epoch_days(d))
          let mc = tcal.month_code_of(cal, cd.year, cd.month)
          use iso <- result.try(month_day_reference_iso(
            cal,
            mc,
            cd.day,
            Constrain,
          ))
          Ok(#(iso.month, iso.day, iso.year, cal))
        }
      }
    }
    None -> Error(RangeE("invalid month-day string"))
  }
}

/// ISO epoch days of 1972-12-31 — the month-day reference-year boundary.
const md_reference_boundary = 1095

/// Find the ISO date of the latest calendar month-day on or before
/// 1972-12-31 with the given month code and day.
fn month_day_reference_iso(
  cal: tcal.Calendar,
  mc: tcal.MonthCode,
  day: Int,
  overflow: Overflow,
) -> Result(IsoDate, TErr) {
  let boundary_cd = tcal.date_from_epoch_days(cal, md_reference_boundary)
  case md_search(cal, mc, day, boundary_cd.year, 300) {
    Ok(iso) -> Ok(iso)
    Error(Nil) -> {
      // No year in the window has this exact day. Constrain clamps to the
      // largest day the month ever has; reject throws.
      case overflow {
        Reject -> Error(RangeE("day out of range for month"))
        Constrain -> {
          let dmax = md_max_day(cal, mc, boundary_cd.year, 300, 0)
          case dmax > 0 {
            True ->
              case md_search(cal, mc, dmax, boundary_cd.year, 300) {
                Ok(iso) -> Ok(iso)
                Error(Nil) -> Error(RangeE("invalid month-day"))
              }
            False -> Error(RangeE("invalid month-day"))
          }
        }
      }
    }
  }
}

fn md_search(
  cal: tcal.Calendar,
  mc: tcal.MonthCode,
  day: Int,
  year: Int,
  tries: Int,
) -> Result(IsoDate, Nil) {
  case tries <= 0 {
    True -> Error(Nil)
    False ->
      case tcal.month_for_code(cal, year, mc) {
        // NeverValid depends only on the code, not the year: no earlier year
        // can produce this month either, so stop rather than spin.
        Error(tcal.NeverValid) -> Error(Nil)
        Error(tcal.NotInThisYear(_)) ->
          md_search(cal, mc, day, year - 1, tries - 1)
        Ok(m) ->
          case day <= tcal.days_in_month(cal, year, m) {
            False -> md_search(cal, mc, day, year - 1, tries - 1)
            True -> {
              let days = tcal.date_to_epoch_days(cal, year, m, day)
              case days <= md_reference_boundary {
                True -> Ok(iso_date_from_epoch_days(days))
                False -> md_search(cal, mc, day, year - 1, tries - 1)
              }
            }
          }
      }
  }
}

/// Largest day the month with this code reaches in the search window.
fn md_max_day(
  cal: tcal.Calendar,
  mc: tcal.MonthCode,
  year: Int,
  tries: Int,
  best: Int,
) -> Int {
  case tries <= 0 {
    True -> best
    False ->
      case tcal.month_for_code(cal, year, mc) {
        // As in `md_search`: NeverValid is year-independent, so `best` is
        // already final.
        Error(tcal.NeverValid) -> best
        Error(tcal.NotInThisYear(_)) ->
          md_max_day(cal, mc, year - 1, tries - 1, best)
        Ok(m) ->
          md_max_day(
            cal,
            mc,
            year - 1,
            tries - 1,
            int.max(best, tcal.days_in_month(cal, year, m)),
          )
      }
  }
}

/// Property bag → month-day. Fields: calendar, then day, era, eraYear,
/// month, monthCode, year.
fn month_day_from_bag(
  state: State(host),
  ref: Ref,
  options: JsValue,
) -> Result(
  #(#(Int, Int, Int, tcal.Calendar), State(host)),
  #(JsValue, State(host)),
) {
  use #(cal, state) <- result.try(read_bag_calendar(state, ref))
  use #(fields, state) <- result.try(read_date_fields(state, ref, cal))
  use #(overflow, state) <- result.try(validated_overflow(state, options))
  use md <- terr_r(state, resolve_calendar_month_day(cal, fields, overflow))
  Ok(#(md, state))
}

/// Resolve month-day fields to #(iso_month, iso_day, iso_ref_year, calendar).
fn resolve_calendar_month_day(
  cal: tcal.Calendar,
  f: DateFields,
  overflow: Overflow,
) -> Result(#(Int, Int, Int, tcal.Calendar), TErr) {
  // Required fields (TypeError) first.
  use day <- result.try(case f.day {
    None -> Error(TypeE("day is required"))
    Some(d) -> Ok(d)
  })
  use Nil <- result.try(case f.month, f.month_code {
    None, None -> Error(TypeE("month or monthCode is required"))
    _, _ -> Ok(Nil)
  })
  let has_year = f.year != None || { f.era != None && f.era_year != None }
  case cal {
    tcal.Iso8601 -> {
      use m <- result.try(resolve_iso_month(f))
      let ref_year = case f.month_code {
        Some(_) -> 1972
        None -> option.unwrap(f.year, 1972)
      }
      use date <- result.try(regulate_iso_date(ref_year, m, day, overflow))
      // Clamp day to the leap reference year's month length.
      let d2 = int.min(date.day, days_in_month(1972, date.month))
      Ok(#(date.month, d2, 1972, cal))
    }
    _ -> {
      // What pins down the reference-year search. Absent a year, only a bare
      // month code can: producing it here is what makes it available below,
      // rather than re-deriving it from `f.month_code` and asserting.
      // (For iso8601 the month maps straight to a code, so this is non-ISO
      // only.)
      use anchor <- result.try(case has_year, f.month_code {
        True, _ -> Ok(AnchorFromYear)
        False, Some(mc) -> Ok(AnchorFromCode(mc))
        False, None ->
          Error(TypeE("either year or monthCode required with month"))
      })
      // Determine the month code (and day) to anchor the reference search.
      use #(mc, day) <- result.try(case anchor {
        AnchorFromYear -> {
          use y <- result.try(resolve_calendar_year(cal, f))
          // Bail out before any month-info computation when no date in the
          // calendar year is within the representable ISO range.
          let year_first = tcal.date_to_epoch_days(cal, y, 1, 1)
          let year_last = tcal.date_to_epoch_days(cal, y + 1, 1, 1) - 1
          use Nil <- result.try(
            case year_first > max_epoch_days || year_last < min_epoch_days {
              True -> Error(RangeE("year outside of supported range"))
              False -> Ok(Nil)
            },
          )
          use m <- result.try(resolve_calendar_month(cal, y, f, overflow))
          use d <- result.try(regulate_calendar_day(cal, y, m, day, overflow))
          Ok(#(tcal.month_code_of(cal, y, m), d))
        }
        AnchorFromCode(mc) -> {
          // Validate the code can ever occur in this calendar.
          use Nil <- result.try(
            case tcal.month_for_code(cal, md_probe_year(cal, mc.leap), mc) {
              Error(tcal.NeverValid) ->
                Error(RangeE(
                  "monthCode is not valid for calendar " <> tcal.identifier(cal),
                ))
              _ -> Ok(Nil)
            },
          )
          case f.month {
            Some(_) -> Error(TypeE("year is required when month is present"))
            None -> Ok(#(mc, day))
          }
        }
      })
      // chinese/dangi leap month-day pairs with no ISO reference year in the
      // spec's reference-year table throw under reject; constrain falls back
      // to the non-leap month (keeping the day).
      use mc <- result.try(
        case
          { cal == tcal.Chinese || cal == tcal.Dangi }
          && mc.leap
          && chinese_ref_year_missing(mc.number, day)
        {
          True ->
            case overflow {
              Reject -> Error(RangeE("no reference year for monthCode and day"))
              Constrain -> Ok(tcal.MonthCode(number: mc.number, leap: False))
            }
          False -> Ok(mc)
        },
      )
      use iso <- result.try(month_day_reference_iso(cal, mc, day, overflow))
      Ok(#(iso.month, iso.day, iso.year, cal))
    }
  }
}

/// What pins the month-day reference-year search to a specific year: an
/// explicit calendar year in the fields, or — when there is none — the bare
/// month code, which is then the only thing that can identify the month.
type MdAnchor {
  AnchorFromYear
  AnchorFromCode(tcal.MonthCode)
}

/// chinese/dangi leap-month + day combinations that have no ISO reference
/// year (the "—" cells of the spec's chinese/dangi reference-year table):
/// such dates are not known to occur between ISO years 1900 and 2035.
fn chinese_ref_year_missing(num: Int, day: Int) -> Bool {
  case num {
    1 | 12 -> True
    2 | 8 | 9 | 10 | 11 -> day == 30
    _ -> False
  }
}

/// A year in which a leap/normal month code can plausibly occur, used only
/// for NeverValid validation of bare month codes.
fn md_probe_year(cal: tcal.Calendar, leap: Bool) -> Int {
  case cal == tcal.Hebrew && leap {
    True -> 5779
    False -> {
      let cd = tcal.date_from_epoch_days(cal, md_reference_boundary)
      cd.year
    }
  }
}

/// ToTemporalInstant(item) → epoch ns.
fn to_temporal_instant(
  state: State(host),
  item: JsValue,
) -> Result(#(Int, State(host)), #(JsValue, State(host))) {
  case item {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: TemporalInstantSlot(epoch_ns:), ..)) ->
          Ok(#(epoch_ns, state))
        Some(ObjectSlot(kind: TemporalZonedDateTimeSlot(epoch_ns:, ..), ..)) ->
          Ok(#(epoch_ns, state))
        _ -> {
          use prim <- result.try(coerce.to_primitive(
            state,
            item,
            coerce.StringHint,
          ))
          case prim {
            #(JsString(s), st) -> parse_instant_to_ns(st, s)
            #(_, st) ->
              type_error_result(st, "cannot convert to a Temporal.Instant")
          }
        }
      }
    JsString(s) -> parse_instant_to_ns(state, s)
    _ -> type_error_result(state, "cannot convert to a Temporal.Instant")
  }
}

fn parse_instant_to_ns(
  state: State(host),
  s: String,
) -> Result(#(Int, State(host)), #(JsValue, State(host))) {
  // Per ParseTemporalInstantString, a [u-ca=...] annotation is only
  // syntax-checked (done by parse_iso_datetime_string); its value is
  // IGNORED for Instant, so unknown calendars must not throw here.
  case parse_iso_datetime_string(s) {
    None -> range_error_result(state, "invalid instant string: " <> s)
    Some(p) ->
      case p.date, p.time {
        Some(d), Some(t) -> {
          let offset = case p.z, p.offset_ns {
            True, _ -> Some(0)
            False, Some(o) -> Some(o)
            False, None -> None
          }
          case offset {
            None ->
              range_error_result(state, "instant string requires a UTC offset")
            Some(off) -> {
              let ns = utc_epoch_ns(d, t) - off
              case int.absolute_value(ns) <= ns_max_instant {
                True -> Ok(#(ns, state))
                False ->
                  range_error_result(state, "instant outside valid range")
              }
            }
          }
        }
        _, _ ->
          range_error_result(state, "instant string requires date and time")
      }
  }
}

/// ToTemporalZonedDateTime(item [, options]) → #(epoch_ns, tz, calendar).
fn to_temporal_zoned(
  state: State(host),
  item: JsValue,
  options: JsValue,
) -> Result(
  #(#(Int, String, tcal.Calendar), State(host)),
  #(JsValue, State(host)),
) {
  case item {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(
          kind: TemporalZonedDateTimeSlot(epoch_ns:, time_zone:, calendar:),
          ..,
        )) -> {
          use #(_o, st) <- result.try(validated_zdt_options(state, options))
          Ok(#(#(epoch_ns, time_zone, calendar), st))
        }
        _ -> zoned_from_bag(state, ref, options)
      }
    JsString(s) -> {
      use #(d, t_opt, z, off_opt, off_sub, tz) <- terr_r(
        state,
        parse_zoned_string(s),
      )
      use cal <- terr_r(state, case extract_calendar_annotation(s) {
        Some(c) -> canonicalize_calendar(c)
        None -> Ok(tcal.Iso8601)
      })
      use #(#(dis, offset_opt, _ov), st) <- result.try(validated_zdt_options(
        state,
        options,
      ))
      use ns <- terr_r(
        st,
        zoned_string_epoch_ns(
          d,
          t_opt,
          z,
          off_opt,
          off_sub,
          tz,
          dis,
          offset_opt,
        ),
      )
      Ok(#(#(ns, tz, cal), st))
    }
    _ -> type_error_result(state, "cannot convert to a Temporal.ZonedDateTime")
  }
}

/// ZonedDateTime options: disambiguation, offset, overflow (alphabetical).
fn validated_zdt_options(
  state: State(host),
  options: JsValue,
) -> Result(
  #(#(Disambiguation, OffsetOption, Overflow), State(host)),
  #(JsValue, State(host)),
) {
  use #(opts, state) <- result.try(get_options_object(state, options))
  use #(d, state) <- result.try(get_disambiguation_option(state, opts))
  use #(of, state) <- result.try(get_offset_option(state, opts, RejectOffset))
  use #(ov, state) <- result.try(get_overflow_option(state, opts))
  Ok(#(#(d, of, ov), state))
}

fn parse_zoned_string(
  s: String,
) -> Result(#(IsoDate, Option(TimeRec), Bool, Option(Int), Bool, String), TErr) {
  case parse_iso_datetime_string(s) {
    None -> Error(RangeE("invalid ZonedDateTime string: " <> s))
    Some(p) -> {
      use Nil <- result.try(check_parsed_calendar(p))
      case p.tz {
        None -> Error(RangeE("ZonedDateTime string requires a [TimeZone]"))
        Some(tz_str) -> {
          use tz <- result.try(parse_time_zone_id(tz_str))
          case p.date {
            None -> Error(RangeE("missing date"))
            Some(d) ->
              Ok(#(d, p.time, p.z, p.offset_ns, p.offset_sub_minute, tz))
          }
        }
      }
    }
  }
}

/// Epoch ns for a parsed ZonedDateTime string: Z → exact instant; explicit
/// offset → interpreted per the offset option (match-minutes); no time →
/// start of day; otherwise wall-clock with disambiguation.
fn zoned_string_epoch_ns(
  d: IsoDate,
  t_opt: Option(TimeRec),
  z: Bool,
  off_opt: Option(Int),
  off_sub_minute: Bool,
  tz: String,
  dis: Disambiguation,
  offset_opt: OffsetOption,
) -> Result(Int, TErr) {
  case t_opt, z, off_opt {
    None, False, None -> start_of_day_ns(tz, d)
    _, _, _ -> {
      let t = option.unwrap(t_opt, midnight)
      // Match-minutes only when the offset lacks a seconds component.
      let match_minutes = !off_sub_minute
      case z, off_opt {
        True, _ -> validate_epoch_ns(utc_epoch_ns(d, t))
        False, Some(off) ->
          interpret_offset(
            d,
            t,
            OptionOffset(off),
            tz,
            dis,
            offset_opt,
            match_minutes,
          )
        False, None ->
          interpret_offset(d, t, WallOffset, tz, dis, offset_opt, True)
      }
    }
  }
}

/// RegulateTime: constrain clamps each component; reject errors.
fn regulate_time(t: TimeRec, overflow: Overflow) -> Result(TimeRec, TErr) {
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

/// ZonedDateTime property bag: calendar, day, era, eraYear, hour,
/// microsecond, millisecond, minute, month, monthCode, nanosecond, offset,
/// second, timeZone, year.
fn zoned_from_bag(
  state: State(host),
  ref: Ref,
  options: JsValue,
) -> Result(
  #(#(Int, String, tcal.Calendar), State(host)),
  #(JsValue, State(host)),
) {
  use #(cal, state) <- result.try(read_bag_calendar(state, ref))
  use #(f, state) <- result.try(read_date_time_fields(
    state,
    ref,
    cal,
    read_offset: True,
    read_tz: True,
  ))
  // timeZone is required.
  case f.tz {
    JsUndefined -> type_error_result(state, "timeZone is required")
    JsString(tz_str) -> {
      use tz <- terr_r(state, parse_time_zone_id(tz_str))
      use #(#(dis, offset_opt, ov), state) <- result.try(validated_zdt_options(
        state,
        options,
      ))
      use date <- terr_r(state, resolve_calendar_date(cal, f.date, ov))
      let t0 = time_fields_apply(f.time, midnight)
      use t <- terr_r(state, regulate_time(t0, ov))
      let behaviour = case f.offset {
        Some(o) -> OptionOffset(o)
        None -> WallOffset
      }
      use ens <- terr_r(
        state,
        interpret_offset(date, t, behaviour, tz, dis, offset_opt, False),
      )
      Ok(#(#(ens, tz, cal), state))
    }
    _ -> type_error_result(state, "timeZone must be a string")
  }
}

/// Read + validate an `offset` field from a property bag (ToOffsetString):
/// ToPrimitive with string hint, require a String, then parse.
fn read_bag_offset(
  state: State(host),
  ref: Ref,
) -> Result(#(Option(Int), State(host)), #(JsValue, State(host))) {
  use #(v, state) <- result.try(
    ops_object.get_value(state, ref, Named("offset"), JsObject(ref))
    |> result.map(fn(p) { #(p.0, p.1) }),
  )
  case v {
    JsUndefined -> Ok(#(None, state))
    _ -> {
      use #(prim, state) <- result.try(coerce.to_primitive(
        state,
        v,
        coerce.StringHint,
      ))
      case prim {
        JsString(s) ->
          case parse_offset_part(s) {
            Some(#(False, Some(off), _, "")) -> Ok(#(Some(off), state))
            _ -> range_error_result(state, "invalid offset string: " <> s)
          }
        _ -> type_error_result(state, "offset must be a string")
      }
    }
  }
}

/// GetTemporalRelativeToOption's result: nothing, a plain date, or an exact
/// zoned instant.
type RelTo {
  RelNone
  RelPlain(date: IsoDate, cal: tcal.Calendar)
  RelZoned(epoch_ns: Int, tz: String, cal: tcal.Calendar)
}

/// GetTemporalRelativeToOption, after the `relativeTo` value itself has been
/// read from the options bag.
fn convert_relative_to(
  state: State(host),
  v: JsValue,
) -> Result(#(RelTo, State(host)), #(JsValue, State(host))) {
  case v {
    JsUndefined -> Ok(#(RelNone, state))
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(
          kind: TemporalZonedDateTimeSlot(epoch_ns:, time_zone:, calendar:),
          ..,
        )) -> Ok(#(RelZoned(epoch_ns, time_zone, calendar), state))
        Some(ObjectSlot(
          kind: TemporalDateSlot(year:, month:, day:, calendar:),
          ..,
        )) -> Ok(#(RelPlain(IsoDate(year, month, day), calendar), state))
        Some(ObjectSlot(
          kind: TemporalDateTimeSlot(year:, month:, day:, calendar:, ..),
          ..,
        )) -> Ok(#(RelPlain(IsoDate(year, month, day), calendar), state))
        _ -> relative_from_bag(state, ref)
      }
    JsString(s) ->
      case parse_iso_datetime_string(s) {
        None -> range_error_result(state, "invalid ISO 8601 string: " <> s)
        Some(p) -> {
          use Nil <- terr_r(state, check_parsed_calendar(p))
          use cal <- terr_r(state, parsed_calendar_id(p))
          case p.date {
            None ->
              range_error_result(state, "relativeTo string requires a date")
            Some(d) ->
              case p.tz {
                Some(tz_str) -> {
                  use tz <- terr_r(state, parse_time_zone_id(tz_str))
                  use ens <- terr_r(
                    state,
                    zoned_string_epoch_ns(
                      d,
                      p.time,
                      p.z,
                      p.offset_ns,
                      p.offset_sub_minute,
                      tz,
                      Compatible,
                      RejectOffset,
                    ),
                  )
                  Ok(#(RelZoned(ens, tz, cal), state))
                }
                None ->
                  case p.z {
                    True ->
                      range_error_result(
                        state,
                        "Z designator requires a bracketed time zone in relativeTo",
                      )
                    False ->
                      case iso_date_within_limits(d) {
                        True -> Ok(#(RelPlain(d, cal), state))
                        False ->
                          range_error_result(
                            state,
                            "date outside of supported range",
                          )
                      }
                  }
              }
          }
        }
      }
    _ -> type_error_result(state, "relativeTo must be a string or an object")
  }
}

/// ToRelativeTemporalObject's property-bag path: like zoned_from_bag but
/// timeZone is optional (absent → plain date) and the options are fixed
/// (overflow constrain, disambiguation compatible, offset reject).
fn relative_from_bag(
  state: State(host),
  ref: Ref,
) -> Result(#(RelTo, State(host)), #(JsValue, State(host))) {
  use #(cal, state) <- result.try(read_bag_calendar(state, ref))
  use #(f, state) <- result.try(read_date_time_fields(
    state,
    ref,
    cal,
    read_offset: True,
    read_tz: True,
  ))
  use date <- terr_r(state, resolve_calendar_date(cal, f.date, Constrain))
  let t0 = time_fields_apply(f.time, midnight)
  use t <- terr_r(state, regulate_time(t0, Constrain))
  case f.tz {
    JsUndefined ->
      case iso_date_within_limits(date) {
        True -> Ok(#(RelPlain(date, cal), state))
        False -> range_error_result(state, "date outside of supported range")
      }
    JsString(tz_str) -> {
      use tz <- terr_r(state, parse_time_zone_id(tz_str))
      let behaviour = case f.offset {
        Some(o) -> OptionOffset(o)
        None -> WallOffset
      }
      use ens <- terr_r(
        state,
        interpret_offset(
          date,
          t,
          behaviour,
          tz,
          Compatible,
          RejectOffset,
          False,
        ),
      )
      Ok(#(RelZoned(ens, tz, cal), state))
    }
    _ -> type_error_result(state, "timeZone must be a string")
  }
}

/// AddZonedDateTime: calendar part added in wall-clock space, time part
/// added exactly to the epoch instant.
fn add_zoned_ns(
  ns: Int,
  tz: String,
  cal: tcal.Calendar,
  dur: DurRec,
) -> Result(Int, TErr) {
  use base <- result.try(
    case dur.years == 0 && dur.months == 0 && dur.weeks == 0 && dur.days == 0 {
      True -> Ok(ns)
      False -> {
        use #(d0, t0) <- result.try(epoch_ns_to_iso_in(tz, ns))
        let date_dur =
          DurRec(
            ..zero_dur,
            years: dur.years,
            months: dur.months,
            weeks: dur.weeks,
            days: dur.days,
          )
        use d2 <- result.try(calendar_date_add(cal, d0, date_dur, Constrain))
        get_epoch_ns_for(tz, d2, t0, Compatible)
      }
    },
  )
  validate_epoch_ns(base + time_only_ns(dur))
}

/// Add24HourDaysToTimeDuration's range check: a time duration must not
/// exceed maxTimeDuration.
fn check_time_duration_range(ns: Int) -> Result(Nil, TErr) {
  case int.absolute_value(ns) <= max_time_duration_ns {
    True -> Ok(Nil)
    False -> Error(RangeE("time duration out of range"))
  }
}

/// DateDurationDays: a date duration's length in days anchored at a plain
/// date (years/months/weeks resolved through the calendar).
fn date_duration_days(
  dur: DurRec,
  rel: IsoDate,
  cal: tcal.Calendar,
) -> Result(Int, TErr) {
  case dur.years == 0 && dur.months == 0 && dur.weeks == 0 {
    True -> Ok(dur.days)
    False -> {
      let ymw =
        DurRec(
          ..zero_dur,
          years: dur.years,
          months: dur.months,
          weeks: dur.weeks,
        )
      use later <- result.map(calendar_date_add(cal, rel, ymw, Constrain))
      epoch_days(later) - epoch_days(rel) + dur.days
    }
  }
}

/// Temporal.Duration.compare(one, two [, options]).
fn duration_compare(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use a, state <- state.try_op(to_temporal_duration(
    state,
    helpers.arg_at(args, 0),
  ))
  use b, state <- state.try_op(to_temporal_duration(
    state,
    helpers.arg_at(args, 1),
  ))
  use opts, state <- state.try_op(get_options_object(
    state,
    helpers.arg_at(args, 2),
  ))
  use relative_to, state <- state.try_op(case opts {
    None -> Ok(#(JsUndefined, state))
    Some(oref) ->
      ops_object.get_value(state, oref, Named("relativeTo"), JsObject(oref))
  })
  use rel, state <- state.try_op(convert_relative_to(state, relative_to))
  let has_cal_units =
    a.years != 0
    || a.months != 0
    || a.weeks != 0
    || b.years != 0
    || b.months != 0
    || b.weeks != 0
  let time_compare = fn(state) {
    #(
      state,
      Ok(value.from_int(int_sign(time_duration_ns(a) - time_duration_ns(b)))),
    )
  }
  case a == b {
    // Identical field values compare equal without consulting relativeTo.
    True -> #(state, Ok(value.from_int(0)))
    False ->
      case rel {
        RelZoned(ns, tz, cal) ->
          case has_cal_units || a.days != 0 || b.days != 0 {
            True -> {
              use na <- terr(state, add_zoned_ns(ns, tz, cal, a))
              use nb <- terr(state, add_zoned_ns(ns, tz, cal, b))
              #(state, Ok(value.from_int(int_sign(na - nb))))
            }
            False -> time_compare(state)
          }
        RelPlain(rel_date, rel_cal) ->
          case has_cal_units {
            True -> {
              use da <- terr(state, date_duration_days(a, rel_date, rel_cal))
              let na = da * ns_per_day + time_only_ns(a)
              use Nil <- terr(state, check_time_duration_range(na))
              use db <- terr(state, date_duration_days(b, rel_date, rel_cal))
              let nb = db * ns_per_day + time_only_ns(b)
              use Nil <- terr(state, check_time_duration_range(nb))
              #(state, Ok(value.from_int(int_sign(na - nb))))
            }
            False -> time_compare(state)
          }
        RelNone ->
          case has_cal_units {
            True ->
              state.range_error(
                state,
                "relativeTo is required for duration comparison with calendar units",
              )
            False -> time_compare(state)
          }
      }
  }
}

/// Nanoseconds of the pure time portion (hours and below).
fn time_only_ns(d: DurRec) -> Int {
  d.hours
  * ns_per_hour
  + d.minutes
  * ns_per_minute
  + d.seconds
  * ns_per_second
  + d.ms
  * ns_per_ms
  + d.us
  * ns_per_us
  + d.ns
}

// ============================================================================
// Date arithmetic
// ============================================================================

/// Balance year-month after adding months (1-based months).
fn balance_year_month(y: Int, m: Int) -> #(Int, Int) {
  let total = y * 12 + m - 1
  #(floor_div(total, 12), math_mod(total, 12) + 1)
}

/// ISODateAdd: add a duration to an ISO date (calendar part y/m regulated
/// by overflow, then weeks/days/time-as-days exact).
fn add_duration_to_date(
  d: IsoDate,
  dur: DurRec,
  overflow: Overflow,
) -> Result(IsoDate, TErr) {
  let #(y2, m2) = balance_year_month(d.year + dur.years, d.month + dur.months)
  use intermediate <- result.try(regulate_iso_date(y2, m2, d.day, overflow))
  let extra_days =
    dur.weeks * 7 + dur.days + truncate_div(time_only_ns(dur), ns_per_day)
  let final_days = epoch_days(intermediate) + extra_days
  let final = iso_date_from_epoch_days(final_days)
  check_date_limits(final)
}

/// Integer division truncating toward zero.
fn truncate_div(a: Int, b: Int) -> Int {
  a / b
}

/// Add a duration's time portion to a wall-clock time. Returns
/// #(day_carry, new_time).
fn add_time(t: TimeRec, add_ns: Int) -> #(Int, TimeRec) {
  let total = time_to_ns(t) + add_ns
  let days = floor_div(total, ns_per_day)
  let rem = total - days * ns_per_day
  #(days, ns_to_time(rem))
}

fn negate_dur(d: DurRec) -> DurRec {
  apply_dur_sign(d, -1)
}

// ============================================================================
// Getter dispatch
// ============================================================================

fn getter_dispatch(
  getter: TemporalGetter,
  protos: TemporalProtos,
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let _ = protos
  case getter {
    PlainDateGetter(g) ->
      case this_date(state, this) {
        Some(#(d, cal)) -> #(state, Ok(date_field_cal(cal, d, g)))
        None -> brand_error(state, "PlainDate", date_getter_name(g))
      }
    PlainTimeGetter(g) ->
      case this_time(state, this) {
        Some(t) -> #(state, Ok(time_field(t, g)))
        None -> brand_error(state, "PlainTime", time_getter_name(g))
      }
    PlainDateTimeGetter(g) ->
      case this_date_time(state, this) {
        Some(#(d, t, cal)) ->
          case g {
            DtTime(tg) -> #(state, Ok(time_field(t, tg)))
            DtDate(dg) -> #(state, Ok(date_field_cal(cal, d, dg)))
          }
        None -> brand_error(state, "PlainDateTime", date_time_getter_name(g))
      }
    PlainYearMonthGetter(g) ->
      case this_year_month(state, this) {
        Some(#(y, m, rd, cal)) -> #(
          state,
          Ok(year_month_field_cal(cal, y, m, rd, g)),
        )
        None -> brand_error(state, "PlainYearMonth", year_month_getter_name(g))
      }
    PlainMonthDayGetter(g) ->
      case this_month_day(state, this) {
        Some(#(m, d, ry, cal)) -> #(
          state,
          Ok(month_day_field_cal(cal, m, d, ry, g)),
        )
        None -> brand_error(state, "PlainMonthDay", month_day_getter_name(g))
      }
    DurationGetter(g) ->
      case this_duration(state, this) {
        Some(d) -> #(state, Ok(duration_field(d, g)))
        None -> brand_error(state, "Duration", duration_getter_name(g))
      }
    InstantGetter(g) ->
      case this_instant(state, this) {
        Some(ns) -> #(state, Ok(instant_field(ns, g)))
        None -> brand_error(state, "Instant", instant_getter_name(g))
      }
    ZonedDateTimeGetter(g) ->
      case this_zoned(state, this) {
        Some(#(ns, tz, zcal)) -> zoned_field(state, ns, tz, zcal, g)
        None -> brand_error(state, "ZonedDateTime", zoned_getter_name(g))
      }
  }
}

// ----------------------------------------------------------------------------
// Getter names — the single source of truth for what each prototype registers.
// ----------------------------------------------------------------------------

fn getter_name(g: TemporalGetter) -> String {
  case g {
    PlainDateGetter(g) -> date_getter_name(g)
    PlainTimeGetter(g) -> time_getter_name(g)
    PlainDateTimeGetter(g) -> date_time_getter_name(g)
    PlainYearMonthGetter(g) -> year_month_getter_name(g)
    PlainMonthDayGetter(g) -> month_day_getter_name(g)
    DurationGetter(g) -> duration_getter_name(g)
    InstantGetter(g) -> instant_getter_name(g)
    ZonedDateTimeGetter(g) -> zoned_getter_name(g)
  }
}

fn date_getter_name(g: TemporalDateGetter) -> String {
  case g {
    DgCalendarId -> "calendarId"
    DgEra -> "era"
    DgEraYear -> "eraYear"
    DgYear -> "year"
    DgMonth -> "month"
    DgMonthCode -> "monthCode"
    DgDay -> "day"
    DgDayOfWeek -> "dayOfWeek"
    DgDayOfYear -> "dayOfYear"
    DgWeekOfYear -> "weekOfYear"
    DgYearOfWeek -> "yearOfWeek"
    DgDaysInWeek -> "daysInWeek"
    DgDaysInMonth -> "daysInMonth"
    DgDaysInYear -> "daysInYear"
    DgMonthsInYear -> "monthsInYear"
    DgInLeapYear -> "inLeapYear"
  }
}

fn time_getter_name(g: TemporalTimeGetter) -> String {
  case g {
    TgHour -> "hour"
    TgMinute -> "minute"
    TgSecond -> "second"
    TgMillisecond -> "millisecond"
    TgMicrosecond -> "microsecond"
    TgNanosecond -> "nanosecond"
  }
}

fn date_time_getter_name(g: TemporalDateTimeGetter) -> String {
  case g {
    DtDate(g) -> date_getter_name(g)
    DtTime(g) -> time_getter_name(g)
  }
}

fn year_month_getter_name(g: TemporalYearMonthGetter) -> String {
  case g {
    YmCalendarId -> "calendarId"
    YmEra -> "era"
    YmEraYear -> "eraYear"
    YmYear -> "year"
    YmMonth -> "month"
    YmMonthCode -> "monthCode"
    YmDaysInYear -> "daysInYear"
    YmDaysInMonth -> "daysInMonth"
    YmMonthsInYear -> "monthsInYear"
    YmInLeapYear -> "inLeapYear"
  }
}

fn month_day_getter_name(g: TemporalMonthDayGetter) -> String {
  case g {
    MdCalendarId -> "calendarId"
    MdMonthCode -> "monthCode"
    MdDay -> "day"
  }
}

fn duration_getter_name(g: TemporalDurationGetter) -> String {
  case g {
    DrYears -> "years"
    DrMonths -> "months"
    DrWeeks -> "weeks"
    DrDays -> "days"
    DrHours -> "hours"
    DrMinutes -> "minutes"
    DrSeconds -> "seconds"
    DrMilliseconds -> "milliseconds"
    DrMicroseconds -> "microseconds"
    DrNanoseconds -> "nanoseconds"
    DrSign -> "sign"
    DrBlank -> "blank"
  }
}

fn instant_getter_name(g: TemporalInstantGetter) -> String {
  case g {
    InEpochMilliseconds -> "epochMilliseconds"
    InEpochNanoseconds -> "epochNanoseconds"
  }
}

fn zoned_getter_name(g: TemporalZonedGetter) -> String {
  case g {
    ZgTimeZoneId -> "timeZoneId"
    ZgEpochMilliseconds -> "epochMilliseconds"
    ZgEpochNanoseconds -> "epochNanoseconds"
    ZgOffsetNanoseconds -> "offsetNanoseconds"
    ZgOffset -> "offset"
    ZgHoursInDay -> "hoursInDay"
    ZgDate(g) -> date_getter_name(g)
    ZgTime(g) -> time_getter_name(g)
  }
}

// ----------------------------------------------------------------------------
// Getter implementations
// ----------------------------------------------------------------------------

fn date_field(d: IsoDate, g: TemporalDateGetter) -> JsValue {
  case g {
    DgCalendarId -> JsString("iso8601")
    DgEra -> JsUndefined
    DgEraYear -> JsUndefined
    DgYear -> value.from_int(d.year)
    DgMonth -> value.from_int(d.month)
    DgMonthCode -> JsString(month_code_str(d.month))
    DgDay -> value.from_int(d.day)
    DgDayOfWeek -> value.from_int(day_of_week(d))
    DgDayOfYear -> value.from_int(day_of_year(d))
    DgWeekOfYear -> value.from_int(week_of_year(d).0)
    DgYearOfWeek -> value.from_int(week_of_year(d).1)
    DgDaysInWeek -> value.from_int(7)
    DgDaysInMonth -> value.from_int(days_in_month(d.year, d.month))
    DgDaysInYear -> value.from_int(days_in_iso_year(d.year))
    DgMonthsInYear -> value.from_int(12)
    DgInLeapYear -> JsBool(is_leap_year(d.year))
  }
}

/// Calendar-aware date field getter (ISO dates fall through to date_field).
fn date_field_cal(
  cal: tcal.Calendar,
  d: IsoDate,
  g: TemporalDateGetter,
) -> JsValue {
  case cal {
    tcal.Iso8601 -> date_field(d, g)
    _ -> {
      let cd = tcal.date_from_epoch_days(cal, epoch_days(d))
      case g {
        DgCalendarId -> JsString(tcal.identifier(cal))
        DgEra -> era_field(cal, cd)
        DgEraYear -> era_year_field(cal, cd)
        DgYear -> value.from_int(cd.year)
        DgMonth -> value.from_int(cd.month)
        DgMonthCode -> JsString(tcal.month_code(cal, cd.year, cd.month))
        DgDay -> value.from_int(cd.day)
        DgDayOfWeek -> value.from_int(day_of_week(d))
        DgDayOfYear ->
          value.from_int(tcal.day_of_year(cal, cd.year, cd.month, cd.day))
        // weekOfYear/yearOfWeek are undefined for non-ISO calendars.
        DgWeekOfYear -> JsUndefined
        DgYearOfWeek -> JsUndefined
        DgDaysInWeek -> value.from_int(7)
        DgDaysInMonth ->
          value.from_int(tcal.days_in_month(cal, cd.year, cd.month))
        DgDaysInYear -> value.from_int(tcal.days_in_year(cal, cd.year))
        DgMonthsInYear -> value.from_int(tcal.months_in_year(cal, cd.year))
        DgInLeapYear -> JsBool(tcal.in_leap_year(cal, cd.year))
      }
    }
  }
}

fn era_field(cal: tcal.Calendar, cd: tcal.CalDate) -> JsValue {
  tcal.era_for(cal, cd.year, cd.month, cd.day)
  |> option.map(fn(e: tcal.Era) { JsString(tcal.era_code_string(e.code)) })
  |> option.unwrap(JsUndefined)
}

fn era_year_field(cal: tcal.Calendar, cd: tcal.CalDate) -> JsValue {
  tcal.era_for(cal, cd.year, cd.month, cd.day)
  |> option.map(fn(e: tcal.Era) { value.from_int(e.year) })
  |> option.unwrap(JsUndefined)
}

fn time_field(t: TimeRec, g: TemporalTimeGetter) -> JsValue {
  case g {
    TgHour -> value.from_int(t.hour)
    TgMinute -> value.from_int(t.minute)
    TgSecond -> value.from_int(t.second)
    TgMillisecond -> value.from_int(t.ms)
    TgMicrosecond -> value.from_int(t.us)
    TgNanosecond -> value.from_int(t.ns)
  }
}

fn year_month_field(y: Int, m: Int, g: TemporalYearMonthGetter) -> JsValue {
  case g {
    YmCalendarId -> JsString("iso8601")
    YmEra -> JsUndefined
    YmEraYear -> JsUndefined
    YmYear -> value.from_int(y)
    YmMonth -> value.from_int(m)
    YmMonthCode -> JsString(month_code_str(m))
    YmDaysInYear -> value.from_int(days_in_iso_year(y))
    YmDaysInMonth -> value.from_int(days_in_month(y, m))
    YmMonthsInYear -> value.from_int(12)
    YmInLeapYear -> JsBool(is_leap_year(y))
  }
}

/// Calendar-aware year-month field getter. y/m/rd are the slot's ISO date.
fn year_month_field_cal(
  cal: tcal.Calendar,
  y: Int,
  m: Int,
  rd: Int,
  g: TemporalYearMonthGetter,
) -> JsValue {
  case cal {
    tcal.Iso8601 -> year_month_field(y, m, g)
    _ -> {
      let cd = tcal.date_from_epoch_days(cal, epoch_days(IsoDate(y, m, rd)))
      case g {
        YmCalendarId -> JsString(tcal.identifier(cal))
        YmEra -> era_field(cal, cd)
        YmEraYear -> era_year_field(cal, cd)
        YmYear -> value.from_int(cd.year)
        YmMonth -> value.from_int(cd.month)
        YmMonthCode -> JsString(tcal.month_code(cal, cd.year, cd.month))
        YmDaysInYear -> value.from_int(tcal.days_in_year(cal, cd.year))
        YmDaysInMonth ->
          value.from_int(tcal.days_in_month(cal, cd.year, cd.month))
        YmMonthsInYear -> value.from_int(tcal.months_in_year(cal, cd.year))
        YmInLeapYear -> JsBool(tcal.in_leap_year(cal, cd.year))
      }
    }
  }
}

/// Calendar-aware month-day field getter. m/d/ry are the slot's ISO date.
fn month_day_field_cal(
  cal: tcal.Calendar,
  m: Int,
  d: Int,
  ry: Int,
  g: TemporalMonthDayGetter,
) -> JsValue {
  case g {
    MdCalendarId -> JsString(tcal.identifier(cal))
    MdMonthCode ->
      case cal {
        tcal.Iso8601 -> JsString(month_code_str(m))
        _ -> {
          let cd = tcal.date_from_epoch_days(cal, epoch_days(IsoDate(ry, m, d)))
          JsString(tcal.month_code(cal, cd.year, cd.month))
        }
      }
    MdDay ->
      case cal {
        tcal.Iso8601 -> value.from_int(d)
        _ -> {
          let cd = tcal.date_from_epoch_days(cal, epoch_days(IsoDate(ry, m, d)))
          value.from_int(cd.day)
        }
      }
  }
}

fn duration_field(d: DurRec, g: TemporalDurationGetter) -> JsValue {
  case g {
    DrYears -> value.from_int(d.years)
    DrMonths -> value.from_int(d.months)
    DrWeeks -> value.from_int(d.weeks)
    DrDays -> value.from_int(d.days)
    DrHours -> value.from_int(d.hours)
    DrMinutes -> value.from_int(d.minutes)
    DrSeconds -> value.from_int(d.seconds)
    DrMilliseconds -> value.from_int(d.ms)
    DrMicroseconds -> value.from_int(d.us)
    DrNanoseconds -> value.from_int(d.ns)
    DrSign -> value.from_int(duration_sign(d))
    DrBlank -> JsBool(duration_sign(d) == 0)
  }
}

fn instant_field(ns: Int, g: TemporalInstantGetter) -> JsValue {
  case g {
    InEpochMilliseconds -> value.from_int(floor_div(ns, ns_per_ms))
    InEpochNanoseconds -> JsBigInt(value.BigInt(ns))
  }
}

fn zoned_field(
  state: State(host),
  ns: Int,
  tz: String,
  zcal: tcal.Calendar,
  g: TemporalZonedGetter,
) -> #(State(host), Result(JsValue, JsValue)) {
  use offset <- terr(state, tz_offset_ns_at(tz, ns))
  let #(d, t) = epoch_ns_to_iso(ns, offset)
  case g {
    ZgTimeZoneId -> #(state, Ok(JsString(tz)))
    ZgEpochMilliseconds -> #(
      state,
      Ok(value.from_int(floor_div(ns, ns_per_ms))),
    )
    ZgEpochNanoseconds -> #(state, Ok(JsBigInt(value.BigInt(ns))))
    ZgOffsetNanoseconds -> #(state, Ok(value.from_int(offset)))
    ZgOffset -> #(state, Ok(JsString(format_offset_full(offset))))
    ZgHoursInDay -> {
      let tomorrow = iso_date_from_epoch_days(epoch_days(d) + 1)
      use s1 <- terr(state, start_of_day_ns(tz, d))
      use s2 <- terr(state, start_of_day_ns(tz, tomorrow))
      #(state, Ok(JsNumber(Finite(ns_div_float(s2 - s1, ns_per_hour)))))
    }
    ZgTime(tg) -> #(state, Ok(time_field(t, tg)))
    ZgDate(dg) -> #(state, Ok(date_field_cal(zcal, d, dg)))
  }
}

// ============================================================================
// Temporal.Now
// ============================================================================

fn now_epoch_ns() -> Int {
  date.now_ms() * ns_per_ms
}

fn now_dispatch(
  name: String,
  protos: TemporalProtos,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case name {
    "instant" -> {
      let #(state, v) = make_instant(state, protos, now_epoch_ns())
      #(state, Ok(v))
    }
    "timeZoneId" -> #(state, Ok(JsString("UTC")))
    "plainDateISO" -> {
      use tz, state <- state.try_op(now_tz_arg(state, args))
      let nns = now_epoch_ns()
      use #(d, _) <- terr(state, epoch_ns_to_iso_in(tz, nns))
      let #(state, v) = make_date(state, protos, d)
      #(state, Ok(v))
    }
    "plainDateTimeISO" -> {
      use tz, state <- state.try_op(now_tz_arg(state, args))
      let nns = now_epoch_ns()
      use #(d, t) <- terr(state, epoch_ns_to_iso_in(tz, nns))
      let #(state, v) = make_date_time(state, protos, d, t)
      #(state, Ok(v))
    }
    "plainTimeISO" -> {
      use tz, state <- state.try_op(now_tz_arg(state, args))
      let nns = now_epoch_ns()
      use #(_, t) <- terr(state, epoch_ns_to_iso_in(tz, nns))
      let #(state, v) = make_time(state, protos, t)
      #(state, Ok(v))
    }
    "zonedDateTimeISO" -> {
      use tz, state <- state.try_op(now_tz_arg(state, args))
      let #(state, v) = make_zoned(state, protos, now_epoch_ns(), tz)
      #(state, Ok(v))
    }
    _ -> state.type_error(state, "unknown Temporal.Now function")
  }
}

fn now_tz_arg(
  state: State(host),
  args: List(JsValue),
) -> Result(#(String, State(host)), #(JsValue, State(host))) {
  case helpers.arg_at(args, 0) {
    JsUndefined -> Ok(#("UTC", state))
    JsString(s) -> terr_op(state, parse_time_zone_id(s))
    _ -> type_error_result(state, "time zone must be a string")
  }
}

// ============================================================================
// Method dispatch
// ============================================================================

fn method_dispatch(
  method: TemporalMethodName,
  protos: TemporalProtos,
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case method {
    PlainDateMethodName(m) -> plain_date_method(m, protos, this, args, state)
    PlainTimeMethodName(m) -> plain_time_method(m, protos, this, args, state)
    PlainDateTimeMethodName(m) ->
      plain_date_time_method(m, protos, this, args, state)
    PlainYearMonthMethodName(m) ->
      plain_year_month_method(m, protos, this, args, state)
    PlainMonthDayMethodName(m) ->
      plain_month_day_method(m, protos, this, args, state)
    DurationMethodName(m) -> duration_method(m, protos, this, args, state)
    InstantMethodName(m) -> instant_method(m, protos, this, args, state)
    ZonedDateTimeMethodName(m) ->
      zoned_date_time_method(m, protos, this, args, state)
  }
}

/// JS-facing name of a prototype method — the only place these strings live.
fn method_name(m: TemporalMethodName) -> String {
  case m {
    PlainDateMethodName(m) -> plain_date_method_name(m)
    PlainTimeMethodName(m) -> plain_time_method_name(m)
    PlainDateTimeMethodName(m) -> plain_date_time_method_name(m)
    PlainYearMonthMethodName(m) -> plain_year_month_method_name(m)
    PlainMonthDayMethodName(m) -> plain_month_day_method_name(m)
    DurationMethodName(m) -> duration_method_name(m)
    InstantMethodName(m) -> instant_method_name(m)
    ZonedDateTimeMethodName(m) -> zoned_date_time_method_name(m)
  }
}

fn plain_date_method_name(m: PlainDateMethod) -> String {
  case m {
    PdToPlainYearMonth -> "toPlainYearMonth"
    PdToPlainMonthDay -> "toPlainMonthDay"
    PdToPlainDateTime -> "toPlainDateTime"
    PdToZonedDateTime -> "toZonedDateTime"
    PdAdd -> "add"
    PdSubtract -> "subtract"
    PdWith -> "with"
    PdWithCalendar -> "withCalendar"
    PdUntil -> "until"
    PdSince -> "since"
    PdEquals -> "equals"
    PdToString -> "toString"
    PdToLocaleString -> "toLocaleString"
    PdToJson -> "toJSON"
    PdValueOf -> "valueOf"
  }
}

fn plain_time_method_name(m: PlainTimeMethod) -> String {
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

fn plain_date_time_method_name(m: PlainDateTimeMethod) -> String {
  case m {
    PdtWith -> "with"
    PdtWithPlainTime -> "withPlainTime"
    PdtWithCalendar -> "withCalendar"
    PdtAdd -> "add"
    PdtSubtract -> "subtract"
    PdtUntil -> "until"
    PdtSince -> "since"
    PdtRound -> "round"
    PdtEquals -> "equals"
    PdtToString -> "toString"
    PdtToLocaleString -> "toLocaleString"
    PdtToJson -> "toJSON"
    PdtValueOf -> "valueOf"
    PdtToPlainDate -> "toPlainDate"
    PdtToPlainTime -> "toPlainTime"
    PdtToZonedDateTime -> "toZonedDateTime"
  }
}

fn plain_year_month_method_name(m: PlainYearMonthMethod) -> String {
  case m {
    PymWith -> "with"
    PymAdd -> "add"
    PymSubtract -> "subtract"
    PymUntil -> "until"
    PymSince -> "since"
    PymEquals -> "equals"
    PymToString -> "toString"
    PymToLocaleString -> "toLocaleString"
    PymToJson -> "toJSON"
    PymValueOf -> "valueOf"
    PymToPlainDate -> "toPlainDate"
  }
}

fn plain_month_day_method_name(m: PlainMonthDayMethod) -> String {
  case m {
    PmdWith -> "with"
    PmdEquals -> "equals"
    PmdToString -> "toString"
    PmdToLocaleString -> "toLocaleString"
    PmdToJson -> "toJSON"
    PmdValueOf -> "valueOf"
    PmdToPlainDate -> "toPlainDate"
  }
}

fn duration_method_name(m: DurationMethod) -> String {
  case m {
    DmWith -> "with"
    DmNegated -> "negated"
    DmAbs -> "abs"
    DmAdd -> "add"
    DmSubtract -> "subtract"
    DmRound -> "round"
    DmTotal -> "total"
    DmToString -> "toString"
    DmToJson -> "toJSON"
    DmToLocaleString -> "toLocaleString"
    DmValueOf -> "valueOf"
  }
}

fn instant_method_name(m: InstantMethod) -> String {
  case m {
    ImAdd -> "add"
    ImSubtract -> "subtract"
    ImUntil -> "until"
    ImSince -> "since"
    ImRound -> "round"
    ImEquals -> "equals"
    ImToString -> "toString"
    ImToLocaleString -> "toLocaleString"
    ImToJson -> "toJSON"
    ImValueOf -> "valueOf"
    ImToZonedDateTimeIso -> "toZonedDateTimeISO"
  }
}

fn zoned_date_time_method_name(m: ZonedDateTimeMethod) -> String {
  case m {
    ZmWithTimeZone -> "withTimeZone"
    ZmWithCalendar -> "withCalendar"
    ZmWithPlainTime -> "withPlainTime"
    ZmWith -> "with"
    ZmAdd -> "add"
    ZmSubtract -> "subtract"
    ZmUntil -> "until"
    ZmSince -> "since"
    ZmRound -> "round"
    ZmEquals -> "equals"
    ZmToString -> "toString"
    ZmToLocaleString -> "toLocaleString"
    ZmToJson -> "toJSON"
    ZmValueOf -> "valueOf"
    ZmStartOfDay -> "startOfDay"
    ZmGetTimeZoneTransition -> "getTimeZoneTransition"
    ZmToInstant -> "toInstant"
    ZmToPlainDate -> "toPlainDate"
    ZmToPlainTime -> "toPlainTime"
    ZmToPlainDateTime -> "toPlainDateTime"
  }
}

// ----------------------------------------------------------------------------
// PlainDate methods
// ----------------------------------------------------------------------------

fn plain_date_method(
  m: PlainDateMethod,
  protos: TemporalProtos,
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this_date(state, this) {
    None -> brand_error(state, "PlainDate", plain_date_method_name(m))
    Some(#(d, cal)) -> {
      case m {
        PdToJson | PdToLocaleString -> #(
          state,
          Ok(JsString(format_iso_date(d) <> calendar_suffix(CalAuto, cal))),
        )
        PdToString -> {
          use #(cal_name, _), state <- state.try_op(get_calendar_name_option(
            state,
            helpers.arg_at(args, 0),
          ))
          let s = format_iso_date(d) <> calendar_suffix(cal_name, cal)
          #(state, Ok(JsString(s)))
        }
        PdValueOf ->
          state.type_error(
            state,
            "Temporal.PlainDate cannot be converted with valueOf; use compare() or equals()",
          )
        PdEquals -> {
          use #(other, other_cal), state <- state.try_op(to_temporal_date(
            state,
            helpers.arg_at(args, 0),
            JsUndefined,
          ))
          #(state, Ok(JsBool(d == other && cal == other_cal)))
        }
        PdAdd | PdSubtract -> {
          use dur, overflow, state <- add_sub_args(state, args, m == PdSubtract)
          use d2 <- terr(state, calendar_date_add(cal, d, dur, overflow))
          let #(state, v) = make_date_cal(state, protos, d2, cal)
          #(state, Ok(v))
        }
        PdWith -> {
          use bag, state <- with_partial_bag(state, args)
          use fields, state <- state.try_op(read_date_fields(state, bag, cal))
          use <- require_nonempty_fields(
            state,
            fields == DateFields(None, None, None, None, None, None),
          )
          use overflow, state <- state.try_op(validated_overflow(
            state,
            helpers.arg_at(args, 1),
          ))
          use date <- terr(
            state,
            calendar_with_fields(cal, d, fields, overflow),
          )
          use date <- terr(state, check_date_limits(date))
          let #(state, v) = make_date_cal(state, protos, date, cal)
          #(state, Ok(v))
        }
        PdWithCalendar -> {
          use new_cal, state <- state.try_op(to_temporal_calendar_identifier(
            state,
            helpers.arg_at(args, 0),
          ))
          let #(state, v) = make_date_cal(state, protos, d, new_cal)
          #(state, Ok(v))
        }
        PdToPlainDateTime -> {
          use t, state <- state.try_op(case helpers.arg_at(args, 0) {
            JsUndefined -> Ok(#(midnight, state))
            v -> to_temporal_time(state, v, JsUndefined)
          })
          let #(state, v) = make_date_time_cal(state, protos, d, t, cal)
          #(state, Ok(v))
        }
        PdToPlainYearMonth -> {
          let #(ymy, ymm, ymd) = case cal {
            tcal.Iso8601 -> #(d.year, d.month, 1)
            _ -> {
              let cd = tcal.date_from_epoch_days(cal, epoch_days(d))
              let first =
                iso_date_from_epoch_days(tcal.date_to_epoch_days(
                  cal,
                  cd.year,
                  cd.month,
                  1,
                ))
              #(first.year, first.month, first.day)
            }
          }
          let #(state, v) =
            make_year_month_cal(state, protos, ymy, ymm, ymd, cal)
          #(state, Ok(v))
        }
        PdToPlainMonthDay -> {
          case cal {
            tcal.Iso8601 -> {
              let #(state, v) =
                make_month_day_cal(state, protos, d.month, d.day, 1972, cal)
              #(state, Ok(v))
            }
            _ -> {
              let cd = tcal.date_from_epoch_days(cal, epoch_days(d))
              let mc = tcal.month_code_of(cal, cd.year, cd.month)
              use iso <- terr(
                state,
                month_day_reference_iso(cal, mc, cd.day, Constrain),
              )
              let #(state, v) =
                make_month_day_cal(
                  state,
                  protos,
                  iso.month,
                  iso.day,
                  iso.year,
                  cal,
                )
              #(state, Ok(v))
            }
          }
        }
        PdToZonedDateTime -> {
          // Argument: a time zone string, or an object with a timeZone
          // property (plus optional plainTime).
          case helpers.arg_at(args, 0) {
            JsString(tz_str) -> {
              use tz <- terr(state, parse_time_zone_id(tz_str))
              use ns <- terr(state, start_of_day_ns(tz, d))
              let #(state, v) = make_zoned_cal(state, protos, ns, tz, cal)
              #(state, Ok(v))
            }
            JsObject(oref) -> {
              use tz_val, state <- state.try_op(ops_object.get_value(
                state,
                oref,
                Named("timeZone"),
                JsObject(oref),
              ))
              case tz_val {
                JsUndefined -> state.type_error(state, "time zone is required")
                JsString(tz_str) -> {
                  use tz <- terr(state, parse_time_zone_id(tz_str))
                  use pt_val, state <- state.try_op(ops_object.get_value(
                    state,
                    oref,
                    Named("plainTime"),
                    JsObject(oref),
                  ))
                  use t, state <- state.try_op(case pt_val {
                    JsUndefined -> Ok(#(midnight, state))
                    v -> to_temporal_time(state, v, JsUndefined)
                  })
                  use ns <- terr(state, get_epoch_ns_for(tz, d, t, Compatible))
                  let #(state, v) = make_zoned_cal(state, protos, ns, tz, cal)
                  #(state, Ok(v))
                }
                _ -> state.type_error(state, "time zone must be a string")
              }
            }
            _ -> state.type_error(state, "time zone must be a string")
          }
        }
        PdUntil | PdSince -> {
          use #(other, other_cal), state <- state.try_op(to_temporal_date(
            state,
            helpers.arg_at(args, 0),
            JsUndefined,
          ))
          case other_cal == cal {
            False ->
              state.range_error(
                state,
                "cannot compute difference between dates of different calendars",
              )
            True ->
              date_until_since(state, protos, cal, d, other, args, m == PdSince)
          }
        }
      }
    }
  }
}

fn calendar_suffix(mode: CalendarNameMode, cal: tcal.Calendar) -> String {
  let id = tcal.identifier(cal)
  case mode {
    CalNever -> ""
    CalAlways -> "[u-ca=" <> id <> "]"
    CalCritical -> "[!u-ca=" <> id <> "]"
    CalAuto ->
      case cal {
        tcal.Iso8601 -> ""
        _ -> "[u-ca=" <> id <> "]"
      }
  }
}

/// Merge `with()` fields onto an existing calendar date and resolve.
/// Existing date contributes year, monthCode (not ordinal month), day —
/// matching CalendarMergeFields/ISODateToFields.
fn calendar_with_fields(
  cal: tcal.Calendar,
  d: IsoDate,
  f: DateFields,
  overflow: Overflow,
) -> Result(IsoDate, TErr) {
  let cd = tcal.date_from_epoch_days(cal, epoch_days(d))
  let has_year = f.year != None || f.era != None || f.era_year != None
  let f = case has_year {
    True -> f
    False -> DateFields(..f, year: Some(cd.year))
  }
  let f = case f.month != None || f.month_code != None {
    True -> f
    False -> {
      DateFields(
        ..f,
        month_code: Some(tcal.month_code_of(cal, cd.year, cd.month)),
      )
    }
  }
  let f = case f.day {
    Some(_) -> f
    None -> DateFields(..f, day: Some(cd.day))
  }
  resolve_calendar_date(cal, f, overflow)
}

/// CPS wrapper: read `with()`'s first argument as a partial property bag.
fn with_partial_bag(
  state: State(host),
  args: List(JsValue),
  cont: fn(Ref, State(host)) -> #(State(host), Result(a, JsValue)),
) -> #(State(host), Result(a, JsValue)) {
  state.try_op(require_partial_bag(state, helpers.arg_at(args, 0)), cont)
}

/// `with()` throws when the bag carries none of the recognized fields.
fn require_nonempty_fields(
  state: State(host),
  is_empty: Bool,
  cont: fn() -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case is_empty {
    True -> state.type_error(state, "with() requires at least one field")
    False -> cont()
  }
}

/// `with()` argument: must be an object with no calendar/timeZone properties
/// and not a Temporal instance.
fn require_partial_bag(
  state: State(host),
  v: JsValue,
) -> Result(#(Ref, State(host)), #(JsValue, State(host))) {
  case v {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: TemporalDateSlot(..), ..))
        | Some(ObjectSlot(kind: TemporalTimeSlot(..), ..))
        | Some(ObjectSlot(kind: TemporalDateTimeSlot(..), ..))
        | Some(ObjectSlot(kind: TemporalYearMonthSlot(..), ..))
        | Some(ObjectSlot(kind: TemporalMonthDaySlot(..), ..))
        | Some(ObjectSlot(kind: TemporalDurationSlot(..), ..))
        | Some(ObjectSlot(kind: TemporalInstantSlot(..), ..))
        | Some(ObjectSlot(kind: TemporalZonedDateTimeSlot(..), ..)) ->
          type_error_result(
            state,
            "with() argument must be a plain object, not a Temporal instance",
          )
        _ -> {
          use #(cal, state) <- result.try(ops_object.get_value(
            state,
            ref,
            Named("calendar"),
            v,
          ))
          case cal {
            JsUndefined -> {
              use #(tz, state) <- result.try(ops_object.get_value(
                state,
                ref,
                Named("timeZone"),
                v,
              ))
              case tz {
                JsUndefined -> Ok(#(ref, state))
                _ ->
                  type_error_result(
                    state,
                    "with() argument must not have a timeZone property",
                  )
              }
            }
            _ ->
              type_error_result(
                state,
                "with() argument must not have a calendar property",
              )
          }
        }
      }
    _ -> type_error_result(state, "with() argument must be an object")
  }
}

// ----------------------------------------------------------------------------
// until/since — units & rounding
// ----------------------------------------------------------------------------

/// A Temporal unit, largest to smallest. JS strings become this type exactly
/// once, in `singular_unit` (reached only via the unit option getters);
/// everything downstream matches on the enum, so a misspelled, plural, or
/// otherwise unnormalized string can never reach a computation.
pub type Unit {
  Year
  Month
  Week
  Day
  Hour
  Minute
  Second
  Millisecond
  Microsecond
  Nanosecond
}

/// roundingMode option values. Parsed once, in `get_rounding_mode_option`.
pub type RoundingMode {
  Ceil
  Floor
  Expand
  Trunc
  HalfCeil
  HalfFloor
  HalfExpand
  HalfTrunc
  HalfEven
}

/// GetUnsignedRoundingMode result: a rounding mode collapsed for a value of
/// known sign (spec "zero"/"infinity"/"half-zero"/"half-infinity"/"half-even").
type UnsignedRoundingMode {
  RZero
  RInfinity
  RHalfZero
  RHalfInfinity
  RHalfEven
}

/// A largestUnit/smallestUnit option value as read from an options bag:
/// absent, an explicit "auto", or a concrete unit.
type UnitOption {
  UnitAbsent
  UnitAuto
  UnitValue(Unit)
}

/// JS-facing singular name of a unit (error messages / option echoes).
pub fn unit_to_string(u: Unit) -> String {
  case u {
    Year -> "year"
    Month -> "month"
    Week -> "week"
    Day -> "day"
    Hour -> "hour"
    Minute -> "minute"
    Second -> "second"
    Millisecond -> "millisecond"
    Microsecond -> "microsecond"
    Nanosecond -> "nanosecond"
  }
}

/// GetTemporalUnitValuedOption's name table: the singular and plural forms
/// map to the unit, anything else is rejected. The ONLY String → Unit
/// conversion.
fn singular_unit(u: String) -> Option(Unit) {
  case u {
    "year" | "years" -> Some(Year)
    "month" | "months" -> Some(Month)
    "week" | "weeks" -> Some(Week)
    "day" | "days" -> Some(Day)
    "hour" | "hours" -> Some(Hour)
    "minute" | "minutes" -> Some(Minute)
    "second" | "seconds" -> Some(Second)
    "millisecond" | "milliseconds" -> Some(Millisecond)
    "microsecond" | "microseconds" -> Some(Microsecond)
    "nanosecond" | "nanoseconds" -> Some(Nanosecond)
    _ -> None
  }
}

fn unit_rank(u: Unit) -> Int {
  case u {
    Year -> 9
    Month -> 8
    Week -> 7
    Day -> 6
    Hour -> 5
    Minute -> 4
    Second -> 3
    Millisecond -> 2
    Microsecond -> 1
    Nanosecond -> 0
  }
}

/// The units that have a fixed nanosecond length: day and below. Year, month
/// and week are deliberately absent — a calendar unit's length depends on the
/// date it is measured from, so it cannot be turned into a nanosecond count.
/// Anything measured in nanoseconds takes a `TimeUnit`, never a `Unit`.
pub type TimeUnit {
  UDay
  UHour
  UMinute
  USecond
  UMillisecond
  UMicrosecond
  UNanosecond
}

/// The fixed-length view of a unit, or None for a calendar unit.
fn as_time_unit(u: Unit) -> Option(TimeUnit) {
  case u {
    Year | Month | Week -> None
    Day -> Some(UDay)
    Hour -> Some(UHour)
    Minute -> Some(UMinute)
    Second -> Some(USecond)
    Millisecond -> Some(UMillisecond)
    Microsecond -> Some(UMicrosecond)
    Nanosecond -> Some(UNanosecond)
  }
}

/// `as_time_unit` where the surrounding spec step has already rejected
/// calendar units; the RangeError restates that guard for the type checker.
fn require_time_unit(u: Unit) -> Result(TimeUnit, TErr) {
  case as_time_unit(u) {
    Some(t) -> Ok(t)
    None ->
      Error(RangeE(
        unit_to_string(u) <> " has no fixed length; expected a time unit",
      ))
  }
}

/// Length of a fixed-length unit in nanoseconds. Total by construction.
fn time_unit_ns(u: TimeUnit) -> Int {
  case u {
    UDay -> ns_per_day
    UHour -> ns_per_hour
    UMinute -> ns_per_minute
    USecond -> ns_per_second
    UMillisecond -> ns_per_ms
    UMicrosecond -> ns_per_us
    UNanosecond -> 1
  }
}

/// Read a unit-valued option ("largestUnit"/"smallestUnit"/"unit"). Both an
/// absent option and (when `allow_auto`) an explicit "auto" become None.
fn get_unit_option(
  state: State(host),
  opts: Option(Ref),
  key: String,
  allow_auto allow_auto: Bool,
) -> Result(#(Option(Unit), State(host)), #(JsValue, State(host))) {
  use #(u, st) <- result.map(get_unit_option_impl(state, opts, key, allow_auto))
  case u {
    UnitValue(v) -> #(Some(v), st)
    UnitAuto | UnitAbsent -> #(None, st)
  }
}

/// Like get_unit_option, but reports an explicit "auto" as UnitAuto instead
/// of collapsing it to UnitAbsent, so callers can distinguish the two.
fn get_unit_option_keep(
  state: State(host),
  opts: Option(Ref),
  key: String,
) -> Result(#(UnitOption, State(host)), #(JsValue, State(host))) {
  get_unit_option_impl(state, opts, key, True)
}

fn get_unit_option_impl(
  state: State(host),
  opts: Option(Ref),
  key: String,
  allow_auto: Bool,
) -> Result(#(UnitOption, State(host)), #(JsValue, State(host))) {
  case opts {
    None -> Ok(#(UnitAbsent, state))
    Some(ref) -> {
      use got <- result.try(ops_object.get_value(
        state,
        ref,
        Named(key),
        JsObject(ref),
      ))
      case got {
        #(JsUndefined, st) -> Ok(#(UnitAbsent, st))
        #(v, st) -> {
          use #(s, st) <- result.try(coerce.js_to_string(st, v))
          case allow_auto && s == "auto", singular_unit(s) {
            True, _ -> Ok(#(UnitAuto, st))
            False, Some(u) -> Ok(#(UnitValue(u), st))
            False, None ->
              range_error_result(st, s <> " is not a valid value for " <> key)
          }
        }
      }
    }
  }
}

fn get_rounding_mode_option(
  state: State(host),
  opts: Option(Ref),
  default: RoundingMode,
) -> Result(#(RoundingMode, State(host)), #(JsValue, State(host))) {
  get_enum_option(
    state,
    opts,
    "roundingMode",
    [
      #("ceil", Ceil),
      #("floor", Floor),
      #("expand", Expand),
      #("trunc", Trunc),
      #("halfCeil", HalfCeil),
      #("halfFloor", HalfFloor),
      #("halfExpand", HalfExpand),
      #("halfTrunc", HalfTrunc),
      #("halfEven", HalfEven),
    ],
    default,
  )
}

fn get_rounding_increment_option(
  state: State(host),
  opts: Option(Ref),
) -> Result(#(Int, State(host)), #(JsValue, State(host))) {
  case opts {
    None -> Ok(#(1, state))
    Some(ref) -> {
      use got <- result.try(ops_object.get_value(
        state,
        ref,
        Named("roundingIncrement"),
        JsObject(ref),
      ))
      case got {
        #(JsUndefined, st) -> Ok(#(1, st))
        #(v, st) -> {
          use num <- result.try(coerce.js_to_number(st, v))
          case num {
            #(Finite(f), st) -> {
              // ToIntegerWithTruncation: truncate, then bounds-check 1..1e9.
              let i = value.float_to_int(f)
              case i >= 1 && i <= 1_000_000_000 {
                True -> Ok(#(i, st))
                False -> range_error_result(st, "invalid roundingIncrement")
              }
            }
            #(_, st) -> range_error_result(st, "invalid roundingIncrement")
          }
        }
      }
    }
  }
}

/// RoundNumberToIncrementAsIfPositive — rounding modes act as if the value
/// were positive (floor-family on the number line). Used for instants.
fn as_if_positive_mode(mode: RoundingMode) -> RoundingMode {
  case mode {
    Trunc -> Floor
    Expand -> Ceil
    HalfTrunc -> HalfFloor
    HalfExpand -> HalfCeil
    Ceil | Floor | HalfCeil | HalfFloor | HalfEven -> mode
  }
}

/// RoundNumberToIncrement on integers: round `x` to a multiple of `inc`.
fn round_to_increment(x: Int, inc: Int, mode: RoundingMode) -> Int {
  let q = floor_div(x, inc)
  let r = x - q * inc
  case r == 0 {
    True -> x
    False -> {
      let lower = q * inc
      let upper = lower + inc
      let twice = 2 * r
      let pick_upper = case mode {
        Ceil -> True
        Floor -> False
        Expand -> x > 0
        Trunc -> x < 0
        HalfCeil -> twice >= inc
        HalfFloor -> twice > inc
        HalfExpand ->
          case x > 0 {
            True -> twice >= inc
            False -> twice > inc
          }
        HalfTrunc ->
          case x > 0 {
            True -> twice > inc
            False -> twice >= inc
          }
        HalfEven ->
          case twice == inc {
            True -> math_mod(q, 2) != 0
            False -> twice > inc
          }
      }
      case pick_upper {
        True -> upper
        False -> lower
      }
    }
  }
}

/// Shared until/since options prologue: largestUnit, roundingIncrement,
/// roundingMode, smallestUnit (read alphabetically — observable order).
/// Continues with #(largest_opt, smallest_opt, inc, mode); callers apply
/// their own per-type defaults and validation.
fn get_difference_settings(
  state: State(host),
  args: List(JsValue),
  cont: fn(Option(Unit), Option(Unit), Int, RoundingMode, State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  use opts, state <- state.try_op(get_options_object(
    state,
    helpers.arg_at(args, 1),
  ))
  use largest, state <- state.try_op(get_unit_option(
    state,
    opts,
    "largestUnit",
    allow_auto: True,
  ))
  use inc, state <- state.try_op(get_rounding_increment_option(state, opts))
  use mode, state <- state.try_op(get_rounding_mode_option(state, opts, Trunc))
  use smallest, state <- state.try_op(get_unit_option(
    state,
    opts,
    "smallestUnit",
    allow_auto: False,
  ))
  cont(largest, smallest, inc, mode, state)
}

const largest_smaller_msg = "largestUnit must not be smaller than smallestUnit"

/// The GetDifferenceSettings ordering check itself: True when largestUnit is
/// finer than smallestUnit, i.e. the RangeError case. Callers differ only in
/// how they raise it (tuple-returning vs Result-returning).
fn largest_smaller_than_smallest(largest: Unit, smallest: Unit) -> Bool {
  unit_rank(largest) < unit_rank(smallest)
}

/// GetDifferenceSettings tail shared by the four tuple-returning until/since
/// paths: largestUnit must not be smaller than smallestUnit, else RangeError.
fn require_largest_ge_smallest(
  state: State(host),
  largest: Unit,
  smallest: Unit,
  cont: fn() -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case largest_smaller_than_smallest(largest, smallest) {
    True -> state.range_error(state, largest_smaller_msg)
    False -> cont()
  }
}

/// since: negate the rounding mode per spec (difference computed two→one).
fn apply_since_mode(mode: RoundingMode, is_since: Bool) -> RoundingMode {
  case is_since {
    True -> negate_rounding_mode(mode)
    False -> mode
  }
}

/// since: negate the resulting duration per spec.
fn apply_since_dur(dur: DurRec, is_since: Bool) -> DurRec {
  case is_since {
    True -> negate_dur(dur)
    False -> dur
  }
}

/// since: negate a rounded ns total per spec.
fn apply_since_ns(ns: Int, is_since: Bool) -> Int {
  case is_since {
    True -> 0 - ns
    False -> ns
  }
}

/// PlainDate.prototype.until/since.
fn date_until_since(
  state: State(host),
  protos: TemporalProtos,
  cal: tcal.Calendar,
  d1: IsoDate,
  d2: IsoDate,
  args: List(JsValue),
  is_since: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  use largest, smallest, inc, mode, state <- get_difference_settings(
    state,
    args,
  )
  let smallest = option.unwrap(smallest, Day)
  let largest = option.unwrap(largest, max_unit(smallest, Day))
  case unit_rank(smallest) < unit_rank(Day) {
    True ->
      state.range_error(state, "smallestUnit must be a date unit for PlainDate")
    False -> {
      use <- require_largest_ge_smallest(state, largest, smallest)
      let mode = apply_since_mode(mode, is_since)
      use dur <- terr(
        state,
        difference_calendar_date(cal, d1, d2, largest, smallest, inc, mode),
      )
      let dur = apply_since_dur(dur, is_since)
      let #(state, v) = make_duration(state, protos, dur)
      #(state, Ok(v))
    }
  }
}

fn max_unit(a: Unit, b: Unit) -> Unit {
  case unit_rank(a) >= unit_rank(b) {
    True -> a
    False -> b
  }
}

fn negate_rounding_mode(mode: RoundingMode) -> RoundingMode {
  case mode {
    Ceil -> Floor
    Floor -> Ceil
    HalfCeil -> HalfFloor
    HalfFloor -> HalfCeil
    Expand | Trunc | HalfExpand | HalfTrunc | HalfEven -> mode
  }
}

/// DifferenceDate in a specific calendar + date-unit rounding.
fn difference_calendar_date(
  cal: tcal.Calendar,
  d1: IsoDate,
  d2: IsoDate,
  largest: Unit,
  smallest: Unit,
  inc: Int,
  mode: RoundingMode,
) -> Result(DurRec, TErr) {
  let sign = compare_iso_date(d2, d1)
  case sign == 0 {
    True -> Ok(zero_dur)
    False -> {
      // Compute exact difference at `largest` granularity.
      let #(years, months, weeks, days) = case cal {
        tcal.Iso8601 -> diff_date_parts(d1, d2, largest)
        _ ->
          case largest {
            Year | Month -> {
              let #(y, m, rem_days) = calendar_date_until(cal, d1, d2, largest)
              #(y, m, 0, rem_days)
            }
            _ -> diff_date_parts(d1, d2, largest)
          }
      }
      // Round to smallest/increment if needed.
      case smallest == Day && inc == 1 {
        True -> Ok(DurRec(..zero_dur, years:, months:, weeks:, days:))
        False ->
          round_relative_date_duration(
            #(years, months, weeks, days),
            #(d1, midnight),
            epoch_days(d2) * ns_per_day,
            largest,
            smallest,
            inc,
            mode,
            False,
          )
      }
    }
  }
}

/// Exact calendar difference decomposed per largestUnit.
fn diff_date_parts(
  d1: IsoDate,
  d2: IsoDate,
  largest: Unit,
) -> #(Int, Int, Int, Int) {
  case largest {
    Year | Month -> {
      let sign = compare_iso_date(d2, d1)
      // months difference counting whole months.
      let total_months = count_months_between(d1, d2, sign)
      let #(years, months) = case largest {
        Year -> #(
          truncate_div(total_months, 12),
          math_mod_signed(total_months, 12),
        )
        _ -> #(0, total_months)
      }
      // Remaining days after adding years+months to d1.
      let intermediate = add_months_constrained(d1, years * 12 + months)
      let days = epoch_days(d2) - epoch_days(intermediate)
      #(years, months, 0, days)
    }
    Week -> {
      let days = epoch_days(d2) - epoch_days(d1)
      #(0, 0, truncate_div(days, 7), math_mod_signed(days, 7))
    }
    _ -> #(0, 0, 0, epoch_days(d2) - epoch_days(d1))
  }
}

/// Modulo with the sign of the dividend (truncated division remainder).
fn math_mod_signed(a: Int, b: Int) -> Int {
  a - truncate_div(a, b) * b
}

/// Count whole months from d1 toward d2 (sign = direction).
fn count_months_between(d1: IsoDate, d2: IsoDate, sign: Int) -> Int {
  let approx = { d2.year - d1.year } * 12 + d2.month - d1.month
  // Adjust: stepping by whole months must not surpass d2.
  adjust_months(d1, d2, approx, sign)
}

fn adjust_months(d1: IsoDate, d2: IsoDate, candidate: Int, sign: Int) -> Int {
  // ISODateSurpasses: the stepped date keeps the original day-of-month
  // (unconstrained) — Jan 29th + 1 month counts as "Feb 29th" for the
  // comparison, so until(Jan 29, Feb 28) is 30 days, not one month.
  let #(y, m) = balance_year_month(d1.year, d1.month + candidate)
  let cmp = compare_triple(#(y, m, d1.day), #(d2.year, d2.month, d2.day))
  case cmp * sign > 0 {
    True -> adjust_months(d1, d2, candidate - sign, sign)
    False -> candidate
  }
}

fn add_months_constrained(d: IsoDate, months: Int) -> IsoDate {
  let #(y, m) = balance_year_month(d.year, d.month + months)
  let day = int.min(d.day, days_in_month(y, m))
  IsoDate(y, m, day)
}

/// Local timeline nanoseconds for a date-time (days since epoch * 86400e9 +
/// time of day). Differences in this space equal epoch-ns differences for any
/// fixed-offset time zone.
fn local_ns(d: IsoDate, t: TimeRec) -> Int {
  epoch_days(d) * ns_per_day + time_to_ns(t)
}

/// ISO CalendarDateAdd: years+months with day constrained, then weeks/days.
/// RangeError when the result is outside the ISO date limits.
fn cal_date_add_checked(d: IsoDate, dur: DurRec) -> Result(IsoDate, TErr) {
  let md = add_months_constrained(d, dur.years * 12 + dur.months)
  let r = iso_date_from_epoch_days(epoch_days(md) + dur.weeks * 7 + dur.days)
  case iso_date_within_limits(r) {
    True -> Ok(r)
    False -> Error(RangeE("date outside of supported range"))
  }
}

/// GetUnsignedRoundingMode — collapse a signed rounding mode for a value of
/// known sign into an UnsignedRoundingMode.
fn unsigned_rounding_mode(
  mode: RoundingMode,
  positive: Bool,
) -> UnsignedRoundingMode {
  case mode {
    Ceil ->
      case positive {
        True -> RInfinity
        False -> RZero
      }
    Floor ->
      case positive {
        True -> RZero
        False -> RInfinity
      }
    Expand -> RInfinity
    Trunc -> RZero
    HalfCeil ->
      case positive {
        True -> RHalfInfinity
        False -> RHalfZero
      }
    HalfFloor ->
      case positive {
        True -> RHalfZero
        False -> RHalfInfinity
      }
    HalfExpand -> RHalfInfinity
    HalfTrunc -> RHalfZero
    HalfEven -> RHalfEven
  }
}

/// ApplyUnsignedRoundingMode for a value strictly between r1 and r2.
/// `cmp` is the sign of (2*|numerator| - |denominator|).
fn apply_unsigned_rounding(
  r1: Int,
  r2: Int,
  cmp: Int,
  even: Bool,
  umode: UnsignedRoundingMode,
) -> Int {
  case umode {
    RZero -> r1
    RInfinity -> r2
    RHalfZero | RHalfInfinity | RHalfEven ->
      case int_sign(cmp) {
        -1 -> r1
        1 -> r2
        _ ->
          case umode {
            RHalfInfinity -> r2
            RHalfEven ->
              case even {
                True -> r1
                False -> r2
              }
            // RZero/RInfinity are handled above; a half-zero tie truncates.
            RZero | RInfinity | RHalfZero -> r1
          }
      }
  }
}

/// Round a value that lies at `num`/`den` progress between the bounding
/// unsigned candidates `abs_r1` and `abs_r2` (multiples of `inc`), per `mode`
/// applied to a value of the given `sign`.
fn round_between(
  abs_r1: Int,
  abs_r2: Int,
  num: Int,
  den: Int,
  inc: Int,
  mode: RoundingMode,
  sign: Int,
) -> Int {
  case num == 0, num == den {
    True, _ -> abs_r1
    _, True -> abs_r2
    _, _ ->
      apply_unsigned_rounding(
        abs_r1,
        abs_r2,
        int_sign(2 * int.absolute_value(num) - int.absolute_value(den)),
        math_mod(abs_r1 / inc, 2) == 0,
        unsigned_rounding_mode(mode, sign > 0),
      )
  }
}

/// ComputeNudgeWindow: bounding durations/instants for rounding `unit`.
/// Returns #(r1, r2, start_dur, end_dur, start_ns, end_ns).
fn nudge_window(
  sign: Int,
  ymwd: #(Int, Int, Int, Int),
  origin: #(IsoDate, TimeRec),
  unit: Unit,
  inc: Int,
  shift: Bool,
  zoned: Bool,
) -> Result(#(Int, Int, DurRec, DurRec, Int, Int), TErr) {
  let #(years, months, weeks, days) = ymwd
  let #(whole, mk) = case unit {
    Year -> #(years, fn(r) { DurRec(..zero_dur, years: r) })
    Month -> #(months, fn(r) { DurRec(..zero_dur, years:, months: r) })
    Week -> #(weeks + truncate_div(days, 7), fn(r) {
      DurRec(..zero_dur, years:, months:, weeks: r)
    })
    _ -> #(days, fn(r) { DurRec(..zero_dur, years:, months:, weeks:, days: r) })
  }
  let base = truncate_div(whole, inc) * inc
  let r1 = case shift {
    True -> base + inc * sign
    False -> base
  }
  let r2 = r1 + inc * sign
  let start_dur = mk(r1)
  let end_dur = mk(r2)
  use start_date <- result.try(cal_date_add_checked(origin.0, start_dur))
  use end_date <- result.try(cal_date_add_checked(origin.0, end_dur))
  // Zoned bounds go through GetEpochNanosecondsFor, whose CheckISODaysRange
  // is stricter (plain/exact ±1e8 days) than the noon-based date limits.
  use Nil <- result.try(case zoned {
    True -> {
      use Nil <- result.try(check_iso_days_range(start_date))
      check_iso_days_range(end_date)
    }
    False -> Ok(Nil)
  })
  let start_ns = case start_dur == zero_dur {
    True -> local_ns(origin.0, origin.1)
    False -> local_ns(start_date, origin.1)
  }
  let end_ns = local_ns(end_date, origin.1)
  Ok(#(r1, r2, start_dur, end_dur, start_ns, end_ns))
}

/// NudgeToCalendarUnit: round the calendar `unit` of a date duration by
/// bounding it between whole-unit instants on the local timeline.
/// Returns #(rounded date duration, did_expand, nudged_ns).
fn nudge_calendar_unit(
  sign: Int,
  ymwd: #(Int, Int, Int, Int),
  origin: #(IsoDate, TimeRec),
  dest_ns: Int,
  unit: Unit,
  inc: Int,
  mode: RoundingMode,
  zoned: Bool,
) -> Result(#(DurRec, Bool, Int), TErr) {
  use w0 <- result.try(nudge_window(sign, ymwd, origin, unit, inc, False, zoned))
  let in_bounds = fn(w: #(Int, Int, DurRec, DurRec, Int, Int)) {
    case sign > 0 {
      True -> w.4 <= dest_ns && dest_ns <= w.5
      False -> w.5 <= dest_ns && dest_ns <= w.4
    }
  }
  // Retry one increment further when end-of-month clamping made the first
  // window not contain the destination.
  use #(w, pre_expanded) <- result.try(case in_bounds(w0) {
    True -> Ok(#(w0, False))
    False -> {
      use w1 <- result.map(nudge_window(
        sign,
        ymwd,
        origin,
        unit,
        inc,
        True,
        zoned,
      ))
      #(w1, True)
    }
  })
  let #(r1, r2, start_dur, end_dur, start_ns, end_ns) = w
  let num = dest_ns - start_ns
  let den = end_ns - start_ns
  let abs_r1 = int.absolute_value(r1)
  let abs_r2 = int.absolute_value(r2)
  let rounded_abs = round_between(abs_r1, abs_r2, num, den, inc, mode, sign)
  let expanded_here = rounded_abs == abs_r2
  let did_expand = pre_expanded || expanded_here
  let chosen = case expanded_here {
    True -> end_dur
    False -> start_dur
  }
  // nudged must correspond to `chosen`: bubble_date_duration compares it
  // against the larger-unit boundary, so passing the ns of a duration one
  // increment beyond the chosen one could wrongly bubble (e.g. 1 year
  // instead of 12 months) when the window was pre-shifted by end-of-month
  // clamping. did_expand (pre_expanded || expanded_here) still triggers
  // the bubble check.
  let nudged = case expanded_here {
    True -> end_ns
    False -> start_ns
  }
  Ok(#(chosen, did_expand, nudged))
}

/// BubbleRelativeDuration: carry a nudged duration into larger units while
/// the nudged instant sits exactly on (or beyond) the larger-unit boundary.
fn bubble_date_duration(
  sign: Int,
  dur: DurRec,
  nudged_ns: Int,
  origin: #(IsoDate, TimeRec),
  largest: Unit,
  start_unit: Unit,
) -> DurRec {
  let candidates =
    case start_unit {
      Day -> [Week, Month, Year]
      Week -> [Month, Year]
      Month -> [Year]
      _ -> []
    }
    |> list.filter(fn(u) {
      unit_rank(u) <= unit_rank(largest) && { u != Week || largest == Week }
    })
  bubble_loop(sign, dur, nudged_ns, origin, candidates)
}

fn bubble_loop(
  sign: Int,
  dur: DurRec,
  nudged_ns: Int,
  origin: #(IsoDate, TimeRec),
  candidates: List(Unit),
) -> DurRec {
  case candidates {
    [] -> dur
    [u, ..rest] -> {
      let end_dur = case u {
        Year -> DurRec(..zero_dur, years: dur.years + sign)
        Month -> DurRec(..zero_dur, years: dur.years, months: dur.months + sign)
        _ ->
          DurRec(
            ..zero_dur,
            years: dur.years,
            months: dur.months,
            weeks: dur.weeks + sign,
          )
      }
      let end_date =
        add_months_constrained(origin.0, end_dur.years * 12 + end_dur.months)
      let end_date =
        iso_date_from_epoch_days(epoch_days(end_date) + end_dur.weeks * 7)
      let end_ns = local_ns(end_date, origin.1)
      case int_sign(nudged_ns - end_ns) != 0 - sign {
        True -> bubble_loop(sign, end_dur, nudged_ns, origin, rest)
        False -> dur
      }
    }
  }
}

/// RoundRelativeDuration for a date-unit smallestUnit: nudge then bubble.
fn round_relative_date_duration(
  ymwd: #(Int, Int, Int, Int),
  origin: #(IsoDate, TimeRec),
  dest_ns: Int,
  largest: Unit,
  smallest: Unit,
  inc: Int,
  mode: RoundingMode,
  zoned: Bool,
) -> Result(DurRec, TErr) {
  let sign = case int_sign(dest_ns - local_ns(origin.0, origin.1)) {
    -1 -> -1
    _ -> 1
  }
  use #(dur, did_expand, nudged) <- result.map(nudge_calendar_unit(
    sign,
    ymwd,
    origin,
    dest_ns,
    smallest,
    inc,
    mode,
    zoned,
  ))
  case did_expand && smallest != Week {
    True ->
      bubble_date_duration(
        sign,
        dur,
        nudged,
        origin,
        largest,
        max_unit(smallest, Day),
      )
    False -> dur
  }
}

// ----------------------------------------------------------------------------
// PlainTime methods
// ----------------------------------------------------------------------------

fn plain_time_method(
  m: PlainTimeMethod,
  protos: TemporalProtos,
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this_time(state, this) {
    None -> brand_error(state, "PlainTime", plain_time_method_name(m))
    Some(t) ->
      case m {
        PtToJson | PtToLocaleString -> #(
          state,
          Ok(JsString(format_iso_time(t, AutoPrec))),
        )
        PtToString -> {
          use opts, state <- state.try_op(get_options_object(
            state,
            helpers.arg_at(args, 0),
          ))
          use #(prec, su, sinc, mode), state <- state.try_op(
            to_string_time_options(state, opts),
          )
          let t2 = case su {
            None -> t
            Some(u) -> {
              let rounded =
                round_to_increment(time_to_ns(t), sinc * time_unit_ns(u), mode)
              ns_to_time(math_mod(rounded, ns_per_day))
            }
          }
          #(state, Ok(JsString(format_iso_time(t2, prec))))
        }
        PtValueOf ->
          state.type_error(
            state,
            "Temporal.PlainTime cannot be converted with valueOf",
          )
        PtEquals -> {
          use other, state <- state.try_op(to_temporal_time(
            state,
            helpers.arg_at(args, 0),
            JsUndefined,
          ))
          #(state, Ok(JsBool(t == other)))
        }
        PtAdd | PtSubtract -> {
          use dur, state <- state.try_op(to_temporal_duration(
            state,
            helpers.arg_at(args, 0),
          ))
          let dur = case m {
            PtSubtract -> negate_dur(dur)
            _ -> dur
          }
          let #(_, t2) = add_time(t, time_only_ns(dur))
          let #(state, v) = make_time(state, protos, t2)
          #(state, Ok(v))
        }
        PtWith -> {
          use bag, state <- with_partial_bag(state, args)
          use f, state <- state.try_op(read_time_fields(state, bag))
          use <- require_nonempty_fields(state, f == no_time_fields)
          use overflow, state <- state.try_op(validated_overflow(
            state,
            helpers.arg_at(args, 1),
          ))
          let t2 = time_fields_apply(f, t)
          use t3 <- terr(state, regulate_time(t2, overflow))
          let #(state, v) = make_time(state, protos, t3)
          #(state, Ok(v))
        }
        PtRound -> {
          use #(su, inc, mode), state <- state.try_op(round_options(
            state,
            helpers.arg_at(args, 0),
            allow_day: False,
          ))
          let u_ns = time_unit_ns(su)
          let max = ns_per_day / u_ns
          case valid_time_increment(inc, max) {
            False -> state.range_error(state, "invalid roundingIncrement")
            True -> {
              let rounded = round_to_increment(time_to_ns(t), inc * u_ns, mode)
              let t2 = ns_to_time(math_mod(rounded, ns_per_day))
              let #(state, v) = make_time(state, protos, t2)
              #(state, Ok(v))
            }
          }
        }
        PtUntil | PtSince -> {
          use other, state <- state.try_op(to_temporal_time(
            state,
            helpers.arg_at(args, 0),
            JsUndefined,
          ))
          time_until_since(state, protos, t, other, args, m == PtSince)
        }
      }
  }
}

/// PlainTime/Instant toString options: fractionalSecondDigits,
/// roundingMode, smallestUnit (alphabetical).
/// Returns #(precision, rounding unit, rounding increment, mode) — the
/// increment scales the unit (ToSecondsStringPrecisionRecord).
fn to_string_time_options(
  state: State(host),
  opts: Option(Ref),
) -> Result(
  #(#(Precision, Option(TimeUnit), Int, RoundingMode), State(host)),
  #(JsValue, State(host)),
) {
  use #(digits, state) <- result.try(get_fractional_digits(state, opts))
  use #(mode, state) <- result.try(get_rounding_mode_option(state, opts, Trunc))
  use #(su, state) <- result.try(get_unit_option(
    state,
    opts,
    "smallestUnit",
    allow_auto: False,
  ))
  terr_op(state, seconds_string_precision(digits, su, mode))
}

/// The fractionalSecondDigits option: "auto" or 0..9. Distinct from
/// `Precision`, the *output* precision of a formatter, which additionally has
/// a minute-truncated form that this option can never name.
type FractionalDigits {
  DigitsAuto
  DigitsFixed(Int)
}

/// ToSecondsStringPrecisionRecord (pure part).
fn seconds_string_precision(
  digits: FractionalDigits,
  su: Option(Unit),
  mode: RoundingMode,
) -> Result(#(Precision, Option(TimeUnit), Int, RoundingMode), TErr) {
  case su {
    Some(Year) | Some(Month) | Some(Week) | Some(Day) | Some(Hour) ->
      Error(RangeE("smallestUnit must be a time unit"))
    Some(Minute) -> Ok(#(MinutePrec, Some(UMinute), 1, mode))
    Some(Second) -> Ok(#(FixedPrec(0), Some(USecond), 1, mode))
    Some(Millisecond) -> Ok(#(FixedPrec(3), Some(UMillisecond), 1, mode))
    Some(Microsecond) -> Ok(#(FixedPrec(6), Some(UMicrosecond), 1, mode))
    Some(Nanosecond) -> Ok(#(FixedPrec(9), Some(UNanosecond), 1, mode))
    None ->
      case digits {
        DigitsAuto -> Ok(#(AutoPrec, None, 1, mode))
        DigitsFixed(0) -> Ok(#(FixedPrec(0), Some(USecond), 1, mode))
        DigitsFixed(n) ->
          Ok(#(FixedPrec(n), Some(UNanosecond), pow10(9 - n), mode))
      }
  }
}

fn get_fractional_digits(
  state: State(host),
  opts: Option(Ref),
) -> Result(#(FractionalDigits, State(host)), #(JsValue, State(host))) {
  case opts {
    None -> Ok(#(DigitsAuto, state))
    Some(ref) -> {
      use got <- result.try(ops_object.get_value(
        state,
        ref,
        Named("fractionalSecondDigits"),
        JsObject(ref),
      ))
      case got {
        #(JsUndefined, st) -> Ok(#(DigitsAuto, st))
        #(JsNumber(Finite(f)), st) -> {
          // floor, then 0..9 bounds (GetTemporalFractionalSecondDigitsOption).
          let i = value.float_to_int(float.floor(f))
          case i >= 0 && i <= 9 {
            True -> Ok(#(DigitsFixed(i), st))
            False -> range_error_result(st, "invalid fractionalSecondDigits")
          }
        }
        #(JsNumber(_), st) ->
          range_error_result(st, "invalid fractionalSecondDigits")
        // Non-number: ToString it; only "auto" is accepted. Symbols raise
        // TypeError from the string coercion.
        #(v, st) -> {
          use str <- result.try(coerce.js_to_string(st, v))
          case str {
            #("auto", st) -> Ok(#(DigitsAuto, st))
            #(_, st) -> range_error_result(st, "invalid fractionalSecondDigits")
          }
        }
      }
    }
  }
}

/// round() options: positional string shorthand or object with smallestUnit
/// (required), roundingIncrement, roundingMode.
fn round_options(
  state: State(host),
  arg: JsValue,
  allow_day allow_day: Bool,
) -> Result(
  #(#(TimeUnit, Int, RoundingMode), State(host)),
  #(JsValue, State(host)),
) {
  case arg {
    JsUndefined -> type_error_result(state, "options parameter is required")
    JsString(s) ->
      case singular_unit(s) |> option.then(round_unit(_, allow_day)) {
        Some(u) -> Ok(#(#(u, 1, HalfExpand), state))
        None -> range_error_result(state, "invalid smallestUnit")
      }
    JsObject(ref) -> {
      use #(inc, state) <- result.try(get_rounding_increment_option(
        state,
        Some(ref),
      ))
      use #(mode, state) <- result.try(get_rounding_mode_option(
        state,
        Some(ref),
        HalfExpand,
      ))
      use #(su, state) <- result.try(get_unit_option(
        state,
        Some(ref),
        "smallestUnit",
        allow_auto: False,
      ))
      case su {
        None -> range_error_result(state, "smallestUnit is required")
        Some(u) ->
          case round_unit(u, allow_day) {
            Some(tu) -> Ok(#(#(tu, inc, mode), state))
            None -> range_error_result(state, "invalid smallestUnit")
          }
      }
    }
    _ -> type_error_result(state, "invalid options")
  }
}

/// A round() smallestUnit: a fixed-length unit, with `day` accepted only where
/// the receiver has days (so a calendar unit can never reach the rounding).
fn round_unit(u: Unit, allow_day: Bool) -> Option(TimeUnit) {
  case as_time_unit(u) {
    Some(UDay) if !allow_day -> None
    other -> other
  }
}

/// Increment must evenly divide the unit's span (and be < span).
fn valid_time_increment(inc: Int, max: Int) -> Bool {
  inc >= 1
  && inc <= max
  && { inc == max || max % inc == 0 }
  && inc != max
  || inc == 1
}

/// GetDifferenceSettings tail: largest must not be smaller than smallest, and
/// for a time-unit smallest the increment must divide the unit's maximum.
fn check_diff_setup(
  state: State(host),
  largest: Unit,
  smallest: Unit,
  inc: Int,
) -> Result(#(Nil, State(host)), #(JsValue, State(host))) {
  case largest_smaller_than_smallest(largest, smallest) {
    True -> range_error_result(state, largest_smaller_msg)
    False -> {
      let ok = case smallest {
        Hour -> valid_time_increment(inc, 24)
        Minute | Second -> valid_time_increment(inc, 60)
        Millisecond | Microsecond | Nanosecond ->
          valid_time_increment(inc, 1000)
        Year | Month | Week | Day -> True
      }
      case ok {
        True -> Ok(#(Nil, state))
        False -> range_error_result(state, "invalid roundingIncrement")
      }
    }
  }
}

fn time_until_since(
  state: State(host),
  protos: TemporalProtos,
  t1: TimeRec,
  t2: TimeRec,
  args: List(JsValue),
  is_since: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  use largest, smallest, inc, mode, state <- get_difference_settings(
    state,
    args,
  )
  let smallest = option.unwrap(smallest, Nanosecond)
  let largest = option.unwrap(largest, max_unit(smallest, Hour))
  case
    unit_rank(smallest) > unit_rank(Hour)
    || unit_rank(largest) > unit_rank(Hour)
  {
    True -> state.range_error(state, "units must be time units for PlainTime")
    False -> {
      use <- require_largest_ge_smallest(state, largest, smallest)
      use su <- terr(state, require_time_unit(smallest))
      let mode2 = apply_since_mode(mode, is_since)
      let diff = time_to_ns(t2) - time_to_ns(t1)
      let rounded = round_to_increment(diff, inc * time_unit_ns(su), mode2)
      let rounded = apply_since_ns(rounded, is_since)
      let dur = balance_time_ns(rounded, largest)
      let #(state, v) = make_duration(state, protos, dur)
      #(state, Ok(v))
    }
  }
}

/// Balance a signed ns total into a Duration up to `largest` (time unit).
fn balance_time_ns(total: Int, largest: Unit) -> DurRec {
  let sign = int_sign(total)
  let a = int.absolute_value(total)
  let lr = unit_rank(largest)
  let #(days, a) = case lr >= unit_rank(Day) {
    True -> #(a / ns_per_day, a % ns_per_day)
    False -> #(0, a)
  }
  let #(hours, a) = case lr >= unit_rank(Hour) {
    True -> #(a / ns_per_hour, a % ns_per_hour)
    False -> #(0, a)
  }
  let #(minutes, a) = case lr >= unit_rank(Minute) {
    True -> #(a / ns_per_minute, a % ns_per_minute)
    False -> #(0, a)
  }
  let #(seconds, a) = case lr >= unit_rank(Second) {
    True -> #(a / ns_per_second, a % ns_per_second)
    False -> #(0, a)
  }
  let #(ms, a) = case lr >= unit_rank(Millisecond) {
    True -> #(a / ns_per_ms, a % ns_per_ms)
    False -> #(0, a)
  }
  let #(us, a) = case lr >= unit_rank(Microsecond) {
    True -> #(a / ns_per_us, a % ns_per_us)
    False -> #(0, a)
  }
  apply_dur_sign(
    DurRec(
      years: 0,
      months: 0,
      weeks: 0,
      days:,
      hours:,
      minutes:,
      seconds:,
      ms:,
      us:,
      ns: a,
    ),
    sign,
  )
}

// ----------------------------------------------------------------------------
// PlainDateTime methods
// ----------------------------------------------------------------------------

fn plain_date_time_method(
  m: PlainDateTimeMethod,
  protos: TemporalProtos,
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this_date_time(state, this) {
    None -> brand_error(state, "PlainDateTime", plain_date_time_method_name(m))
    Some(#(d, t, cal)) -> {
      case m {
        PdtToJson -> #(
          state,
          Ok(JsString(
            format_iso_date(d)
            <> "T"
            <> format_iso_time(t, AutoPrec)
            <> calendar_suffix(CalAuto, cal),
          )),
        )
        PdtToLocaleString -> #(
          state,
          Ok(JsString(format_iso_date(d) <> " " <> format_iso_time(t, AutoPrec))),
        )
        PdtToString -> {
          use #(cal_name, opts), state <- state.try_op(get_calendar_name_option(
            state,
            helpers.arg_at(args, 0),
          ))
          use #(prec, su, sinc, mode), state <- state.try_op(
            to_string_time_options(state, opts),
          )
          let #(d2, t2) = case su {
            None -> #(d, t)
            Some(u) -> {
              let total = epoch_days(d) * ns_per_day + time_to_ns(t)
              let rounded =
                round_to_increment(total, sinc * time_unit_ns(u), mode)
              epoch_ns_to_iso(rounded, 0)
            }
          }
          let s =
            format_iso_date(d2)
            <> "T"
            <> format_iso_time(t2, prec)
            <> calendar_suffix(cal_name, cal)
          #(state, Ok(JsString(s)))
        }
        PdtValueOf ->
          state.type_error(
            state,
            "Temporal.PlainDateTime cannot be converted with valueOf",
          )
        PdtEquals -> {
          use #(od, ot, ocal), state <- state.try_op(to_temporal_date_time(
            state,
            helpers.arg_at(args, 0),
            JsUndefined,
          ))
          #(state, Ok(JsBool(#(d, t) == #(od, ot) && cal == ocal)))
        }
        PdtAdd | PdtSubtract -> {
          use dur, overflow, state <- add_sub_args(
            state,
            args,
            m == PdtSubtract,
          )
          // Time first, carry days into the date addition.
          let #(carry, t2) = add_time(t, time_only_ns(dur))
          let date_dur =
            DurRec(
              ..zero_dur,
              years: dur.years,
              months: dur.months,
              weeks: dur.weeks,
              days: dur.days + carry,
            )
          use d2 <- terr(state, calendar_date_add(cal, d, date_dur, overflow))
          case iso_datetime_within_limits(d2, t2) {
            False ->
              state.range_error(state, "date-time outside supported range")
            True -> {
              let #(state, v) = make_date_time_cal(state, protos, d2, t2, cal)
              #(state, Ok(v))
            }
          }
        }
        PdtWithPlainTime -> {
          use t2, state <- state.try_op(case helpers.arg_at(args, 0) {
            JsUndefined -> Ok(#(midnight, state))
            v -> to_temporal_time(state, v, JsUndefined)
          })
          let #(state, v) = make_date_time_cal(state, protos, d, t2, cal)
          #(state, Ok(v))
        }
        PdtWithCalendar -> {
          use new_cal, state <- state.try_op(to_temporal_calendar_identifier(
            state,
            helpers.arg_at(args, 0),
          ))
          let #(state, v) = make_date_time_cal(state, protos, d, t, new_cal)
          #(state, Ok(v))
        }
        PdtWith -> {
          use bag, state <- with_partial_bag(state, args)
          use f, state <- state.try_op(read_date_time_fields(
            state,
            bag,
            cal,
            read_offset: False,
            read_tz: False,
          ))
          use <- require_nonempty_fields(state, date_time_fields_all_none(f))
          use overflow, state <- state.try_op(validated_overflow(
            state,
            helpers.arg_at(args, 1),
          ))
          use date <- terr(
            state,
            calendar_with_fields(cal, d, f.date, overflow),
          )
          let t0 = time_fields_apply(f.time, t)
          use t2 <- terr(state, regulate_time(t0, overflow))
          case iso_datetime_within_limits(date, t2) {
            False ->
              state.range_error(state, "date-time outside supported range")
            True -> {
              let #(state, v) = make_date_time_cal(state, protos, date, t2, cal)
              #(state, Ok(v))
            }
          }
        }
        PdtRound -> {
          use #(su, inc, mode), state <- state.try_op(round_options(
            state,
            helpers.arg_at(args, 0),
            allow_day: True,
          ))
          let u_ns = time_unit_ns(su)
          let max = case su {
            UDay -> 1
            _ -> ns_per_day / u_ns
          }
          case valid_time_increment(inc, max) {
            False -> state.range_error(state, "invalid roundingIncrement")
            True -> {
              let total = epoch_days(d) * ns_per_day + time_to_ns(t)
              let rounded = round_to_increment(total, inc * u_ns, mode)
              let #(d2, t2) = epoch_ns_to_iso(rounded, 0)
              case iso_datetime_within_limits(d2, t2) {
                False ->
                  state.range_error(state, "date-time outside supported range")
                True -> {
                  let #(state, v) =
                    make_date_time_cal(state, protos, d2, t2, cal)
                  #(state, Ok(v))
                }
              }
            }
          }
        }
        PdtToPlainDate -> {
          let #(state, v) = make_date_cal(state, protos, d, cal)
          #(state, Ok(v))
        }
        PdtToPlainTime -> {
          let #(state, v) = make_time(state, protos, t)
          #(state, Ok(v))
        }
        PdtToZonedDateTime -> {
          case helpers.arg_at(args, 0) {
            JsString(tz_str) -> {
              use tz <- terr(state, parse_time_zone_id(tz_str))
              use opts, state <- state.try_op(get_options_object(
                state,
                helpers.arg_at(args, 1),
              ))
              use dis2, state <- state.try_op(get_disambiguation_option(
                state,
                opts,
              ))
              use ns <- terr(state, get_epoch_ns_for(tz, d, t, dis2))
              case int.absolute_value(ns) <= ns_max_instant {
                False -> state.range_error(state, "instant outside valid range")
                True -> {
                  let #(state, v) = make_zoned_cal(state, protos, ns, tz, cal)
                  #(state, Ok(v))
                }
              }
            }
            JsUndefined -> state.type_error(state, "time zone is required")
            _ -> state.type_error(state, "time zone must be a string")
          }
        }
        PdtUntil | PdtSince -> {
          use #(od, ot, ocal), state <- state.try_op(to_temporal_date_time(
            state,
            helpers.arg_at(args, 0),
            JsUndefined,
          ))
          case ocal == cal {
            False ->
              state.range_error(
                state,
                "cannot compute difference between dates of different calendars",
              )
            True ->
              date_time_until_since(
                state,
                protos,
                cal,
                #(d, t),
                #(od, ot),
                args,
                m == PdtSince,
              )
          }
        }
      }
    }
  }
}

fn date_time_until_since(
  state: State(host),
  protos: TemporalProtos,
  cal: tcal.Calendar,
  a: #(IsoDate, TimeRec),
  b: #(IsoDate, TimeRec),
  args: List(JsValue),
  is_since: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  use largest, smallest, inc, mode, state <- get_difference_settings(
    state,
    args,
  )
  let smallest = option.unwrap(smallest, Nanosecond)
  let largest = option.unwrap(largest, max_unit(smallest, Day))
  use Nil, state <- state.try_op(check_diff_setup(state, largest, smallest, inc))
  let mode2 = apply_since_mode(mode, is_since)
  use final <- terr(
    state,
    diff_date_time_core(cal, a, b, largest, smallest, inc, mode2, False),
  )
  let final = apply_since_dur(final, is_since)
  let #(state, v) = make_duration(state, protos, final)
  #(state, Ok(v))
}

/// Difference between two ISO date-times decomposed per largest/smallest
/// units with rounding. Shared by PlainDateTime and ZonedDateTime until/since.
fn diff_date_time_core(
  cal: tcal.Calendar,
  a: #(IsoDate, TimeRec),
  b: #(IsoDate, TimeRec),
  largest: Unit,
  smallest: Unit,
  inc: Int,
  mode2: RoundingMode,
  zoned: Bool,
) -> Result(DurRec, TErr) {
  // Time difference first; borrow a day if signs conflict.
  let date_sign = compare_iso_date(b.0, a.0)
  let time_diff = time_to_ns(b.1) - time_to_ns(a.1)
  let #(b_date, time_diff) = case
    date_sign > 0 && time_diff < 0,
    date_sign < 0 && time_diff > 0
  {
    True, _ -> #(
      iso_date_from_epoch_days(epoch_days(b.0) - 1),
      time_diff + ns_per_day,
    )
    _, True -> #(
      iso_date_from_epoch_days(epoch_days(b.0) + 1),
      time_diff - ns_per_day,
    )
    _, _ -> #(b.0, time_diff)
  }
  case unit_rank(largest) >= unit_rank(Day) {
    True -> {
      let #(years, months, weeks, days) = case cal {
        tcal.Iso8601 -> diff_date_parts(a.0, b_date, largest)
        _ ->
          case largest {
            Year | Month -> {
              let #(y, m, rem_days) =
                calendar_date_until(cal, a.0, b_date, largest)
              #(y, m, 0, rem_days)
            }
            _ -> diff_date_parts(a.0, b_date, largest)
          }
      }
      case
        unit_rank(smallest) > unit_rank(Day) || { zoned && smallest == Day }
      {
        // Calendar-unit smallestUnit (or day with a time zone, whose length
        // varies): epoch-ns bounding (NudgeToCalendarUnit). A plain `day` is
        // uniform 24 hours and is rounded numerically below.
        True ->
          round_relative_date_duration(
            #(years, months, weeks, days),
            a,
            local_ns(b.0, b.1),
            largest,
            smallest,
            inc,
            mode2,
            zoned,
          )
        False -> {
          // Time-unit smallestUnit: round days+time in ns (NudgeToDayOrTime).
          use su <- result.try(require_time_unit(smallest))
          let time_total = days * ns_per_day + time_diff
          let rounded = case smallest == Nanosecond && inc == 1 {
            True -> time_total
            False ->
              round_to_increment(time_total, inc * time_unit_ns(su), mode2)
          }
          let whole_days = truncate_div(time_total, ns_per_day)
          let rounded_whole = truncate_div(rounded, ns_per_day)
          let rem_ns = rounded - rounded_whole * ns_per_day
          let time_part = balance_time_ns(rem_ns, Hour)
          let base =
            DurRec(..time_part, years:, months:, weeks:, days: rounded_whole)
          let did_expand =
            int_sign(rounded_whole - whole_days) == int_sign(time_total)
          case did_expand {
            False -> Ok(base)
            True -> {
              let dest_ns = local_ns(b.0, b.1)
              let nudged = dest_ns + rounded - time_total
              let dsign = case int_sign(dest_ns - local_ns(a.0, a.1)) {
                -1 -> -1
                _ -> 1
              }
              Ok(bubble_date_duration(dsign, base, nudged, a, largest, Day))
            }
          }
        }
      }
    }
    False -> {
      // Pure time-based difference.
      use su <- result.try(require_time_unit(smallest))
      let total =
        { epoch_days(b.0) - epoch_days(a.0) }
        * ns_per_day
        + { time_to_ns(b.1) - time_to_ns(a.1) }
      let rounded = round_to_increment(total, inc * time_unit_ns(su), mode2)
      Ok(balance_time_ns(rounded, largest))
    }
  }
}

// ----------------------------------------------------------------------------
// PlainYearMonth methods
// ----------------------------------------------------------------------------

fn plain_year_month_method(
  meth: PlainYearMonthMethod,
  protos: TemporalProtos,
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this_year_month(state, this) {
    None ->
      brand_error(state, "PlainYearMonth", plain_year_month_method_name(meth))
    Some(#(y, m, rd, cal)) -> {
      case meth {
        PymToJson | PymToLocaleString -> #(
          state,
          Ok(JsString(format_ym_cal(y, m, rd, cal, CalAuto))),
        )
        PymToString -> {
          use #(cal_name, _), state <- state.try_op(get_calendar_name_option(
            state,
            helpers.arg_at(args, 0),
          ))
          #(state, Ok(JsString(format_ym_cal(y, m, rd, cal, cal_name))))
        }
        PymValueOf ->
          state.type_error(
            state,
            "Temporal.PlainYearMonth cannot be converted with valueOf",
          )
        PymEquals -> {
          use other, state <- state.try_op(to_temporal_year_month(
            state,
            helpers.arg_at(args, 0),
            JsUndefined,
          ))
          #(state, Ok(JsBool(#(y, m, rd, cal) == other)))
        }
        PymAdd | PymSubtract -> {
          use dur, overflow, state <- add_sub_args(
            state,
            args,
            meth == PymSubtract,
          )
          // AddDurationToYearMonth: only years and months are allowed
          // (weeks/days/time throw RangeError); the calculation always
          // starts from day 1 of the calendar month, so day overflow never
          // occurs — `overflow` only affects month-code resolution (e.g.
          // hebrew M05L in a non-leap year).
          let has_lower_units =
            dur.weeks != 0
            || dur.days != 0
            || dur.hours != 0
            || dur.minutes != 0
            || dur.seconds != 0
            || dur.ms != 0
            || dur.us != 0
            || dur.ns != 0
          use Nil <- terr(state, case has_lower_units {
            True ->
              Error(RangeE(
                "only years and months can be added to Temporal.PlainYearMonth",
              ))
            False -> Ok(Nil)
          })
          case cal {
            tcal.Iso8601 -> {
              // AddDurationToYearMonth steps 8-9: the intermediate date is
              // day 1 of the receiver's month and goes through
              // CalendarDateFromFields, which throws when it is outside the
              // ISO date limits (e.g. -271821-04-01, before the minimum
              // date -271821-04-19) — even for a zero duration.
              use _day1 <- terr(state, check_date_limits(IsoDate(y, m, 1)))
              let #(y2, m2) = balance_year_month(y + dur.years, m + dur.months)
              case iso_year_month_within_limits(y2, m2) {
                False ->
                  state.range_error(state, "year-month outside supported range")
                True -> {
                  let #(state, v) = make_year_month(state, protos, y2, m2, 1)
                  #(state, Ok(v))
                }
              }
            }
            _ -> {
              let cd =
                tcal.date_from_epoch_days(cal, epoch_days(IsoDate(y, m, rd)))
              let start =
                iso_date_from_epoch_days(tcal.date_to_epoch_days(
                  cal,
                  cd.year,
                  cd.month,
                  1,
                ))
              // CalendarDateFromFields on the day-1 intermediate date also
              // enforces the ISO date limits for non-ISO calendars.
              use start <- terr(state, check_date_limits(start))
              use d2 <- terr(
                state,
                calendar_date_add(cal, start, dur, overflow),
              )
              let cd2 = tcal.date_from_epoch_days(cal, epoch_days(d2))
              let first =
                iso_date_from_epoch_days(tcal.date_to_epoch_days(
                  cal,
                  cd2.year,
                  cd2.month,
                  1,
                ))
              case iso_year_month_within_limits(first.year, first.month) {
                False ->
                  state.range_error(state, "year-month outside supported range")
                True -> {
                  let #(state, v) =
                    make_year_month_cal(
                      state,
                      protos,
                      first.year,
                      first.month,
                      first.day,
                      cal,
                    )
                  #(state, Ok(v))
                }
              }
            }
          }
        }
        PymWith -> {
          use bag, state <- with_partial_bag(state, args)
          use era, state <- state.try_op(case tcal.has_eras(cal) {
            True -> read_bag_era(state, bag)
            False -> Ok(#(None, state))
          })
          use era_year, state <- state.try_op(case tcal.has_eras(cal) {
            True -> read_int_field(state, bag, "eraYear")
            False -> Ok(#(None, state))
          })
          use month, state <- state.try_op(read_pos_int_field(
            state,
            bag,
            "month",
          ))
          use month_code, state <- state.try_op(read_month_code(state, bag))
          use year, state <- state.try_op(read_int_field(state, bag, "year"))
          use <- require_nonempty_fields(
            state,
            month == None
              && month_code == None
              && year == None
              && era == None
              && era_year == None,
          )
          use overflow, state <- state.try_op(validated_overflow(
            state,
            helpers.arg_at(args, 1),
          ))
          // Merge with existing calendar year/monthCode.
          let cd = tcal.date_from_epoch_days(cal, epoch_days(IsoDate(y, m, rd)))
          let f =
            DateFields(day: None, era:, era_year:, month:, month_code:, year:)
          let has_year = year != None || era != None || era_year != None
          let f = case has_year {
            True -> f
            False -> DateFields(..f, year: Some(cd.year))
          }
          let f = case month != None || month_code != None {
            True -> f
            False -> {
              DateFields(
                ..f,
                month_code: Some(tcal.month_code_of(cal, cd.year, cd.month)),
              )
            }
          }
          use #(y2, m2, rd2, _) <- terr(
            state,
            resolve_calendar_year_month(cal, f, overflow),
          )
          let #(state, v) = make_year_month_cal(state, protos, y2, m2, rd2, cal)
          #(state, Ok(v))
        }
        PymToPlainDate -> {
          case helpers.arg_at(args, 0) {
            JsObject(ref) -> {
              use day, state <- state.try_op(read_pos_int_field(
                state,
                ref,
                "day",
              ))
              case day {
                Some(dd) -> {
                  case cal {
                    tcal.Iso8601 -> {
                      use date <- terr(
                        state,
                        regulate_iso_date(y, m, dd, Constrain),
                      )
                      use date <- terr(state, check_date_limits(date))
                      let #(state, v) = make_date_cal(state, protos, date, cal)
                      #(state, Ok(v))
                    }
                    _ -> {
                      let cd =
                        tcal.date_from_epoch_days(
                          cal,
                          epoch_days(IsoDate(y, m, rd)),
                        )
                      use d2 <- terr(
                        state,
                        regulate_calendar_day(
                          cal,
                          cd.year,
                          cd.month,
                          dd,
                          Constrain,
                        ),
                      )
                      let date =
                        iso_date_from_epoch_days(tcal.date_to_epoch_days(
                          cal,
                          cd.year,
                          cd.month,
                          d2,
                        ))
                      use date <- terr(state, check_date_limits(date))
                      let #(state, v) = make_date_cal(state, protos, date, cal)
                      #(state, Ok(v))
                    }
                  }
                }
                None -> state.type_error(state, "day is required")
              }
            }
            _ -> state.type_error(state, "argument must be an object")
          }
        }
        PymUntil | PymSince -> {
          use other, state <- state.try_op(to_temporal_year_month(
            state,
            helpers.arg_at(args, 0),
            JsUndefined,
          ))
          case other.3 == cal {
            False ->
              state.range_error(
                state,
                "cannot compute difference between dates of different calendars",
              )
            True ->
              year_month_until_since(
                state,
                protos,
                cal,
                #(y, m, rd),
                #(other.0, other.1, other.2),
                args,
                meth == PymSince,
              )
          }
        }
      }
    }
  }
}

/// Format a PlainYearMonth: non-ISO calendars always include the reference
/// day and the calendar annotation.
fn format_ym_cal(
  y: Int,
  m: Int,
  rd: Int,
  cal: tcal.Calendar,
  mode: CalendarNameMode,
) -> String {
  case cal {
    tcal.Iso8601 ->
      case mode {
        CalAlways | CalCritical ->
          format_iso_date(IsoDate(y, m, rd)) <> calendar_suffix(mode, cal)
        CalAuto | CalNever -> format_ym(y, m)
      }
    _ ->
      case mode {
        CalNever -> format_iso_date(IsoDate(y, m, rd))
        CalAuto | CalAlways | CalCritical ->
          format_iso_date(IsoDate(y, m, rd)) <> calendar_suffix(mode, cal)
      }
  }
}

fn format_ym(y: Int, m: Int) -> String {
  format_iso_year(y) <> "-" <> pad2(m)
}

fn year_month_until_since(
  state: State(host),
  protos: TemporalProtos,
  cal: tcal.Calendar,
  a: #(Int, Int, Int),
  b: #(Int, Int, Int),
  args: List(JsValue),
  is_since: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  use largest, smallest, inc, mode, state <- get_difference_settings(
    state,
    args,
  )
  let smallest = option.unwrap(smallest, Month)
  let largest = option.unwrap(largest, max_unit(smallest, Year))
  case unit_rank(smallest) < unit_rank(Month) {
    True -> state.range_error(state, "smallestUnit must be year or month")
    False -> {
      use <- require_largest_ge_smallest(state, largest, smallest)
      let mode2 = apply_since_mode(mode, is_since)
      let total_months = case cal {
        tcal.Iso8601 -> { b.0 - a.0 } * 12 + b.1 - a.1
        _ -> {
          // Count whole calendar months between the two month-firsts.
          let ia = IsoDate(a.0, a.1, a.2)
          let ib = IsoDate(b.0, b.1, b.2)
          let #(_, months, _) = calendar_date_until(cal, ia, ib, Month)
          months
        }
      }
      let rounded = case smallest {
        Year -> round_to_increment(total_months, inc * 12, mode2) / 12
        _ -> round_to_increment(total_months, inc, mode2)
      }
      use dur <- terr(state, case cal {
        tcal.Iso8601 ->
          Ok(case smallest, largest {
            Year, _ -> DurRec(..zero_dur, years: rounded)
            _, Year ->
              DurRec(
                ..zero_dur,
                years: truncate_div(rounded, 12),
                months: math_mod_signed(rounded, 12),
              )
            _, _ -> DurRec(..zero_dur, months: rounded)
          })
        _ -> {
          // Calendar-space years/months decomposition. RoundRelativeDuration
          // is calendar-agnostic, so roundingMode/roundingIncrement apply
          // here too: years are nudged against real calendar-year
          // boundaries, and a rounded month total is re-decomposed by
          // stepping calendar years (not recomputed unrounded).
          let ia = IsoDate(a.0, a.1, a.2)
          let ib = IsoDate(b.0, b.1, b.2)
          case smallest, largest {
            Year, _ -> {
              use yrs <- result.map(round_calendar_year_total(
                cal,
                ia,
                ib,
                inc,
                mode2,
              ))
              DurRec(..zero_dur, years: yrs)
            }
            _, Year -> {
              use mid <- result.map(calendar_date_add(
                cal,
                ia,
                DurRec(..zero_dur, months: rounded),
                Constrain,
              ))
              let #(yrs, mos, _) = calendar_date_until(cal, ia, mid, Year)
              DurRec(..zero_dur, years: yrs, months: mos)
            }
            _, _ -> Ok(DurRec(..zero_dur, months: rounded))
          }
        }
      })
      let dur = apply_since_dur(dur, is_since)
      let #(state, v) = make_duration(state, protos, dur)
      #(state, Ok(v))
    }
  }
}

/// NudgeToCalendarUnit for a PlainYearMonth difference in a non-ISO
/// calendar: round the whole-year count of ib − ia (both ISO dates of
/// calendar day 1) to `inc`-year multiples per `mode`. The fractional year
/// is measured as day progress between the bounding calendar-year marks
/// (start = ia + r1 years, end = ia + r2 years), like the spec's
/// epoch-nanosecond progress.
fn round_calendar_year_total(
  cal: tcal.Calendar,
  ia: IsoDate,
  ib: IsoDate,
  inc: Int,
  mode: RoundingMode,
) -> Result(Int, TErr) {
  let dest = epoch_days(ib)
  let sign = case dest < epoch_days(ia) {
    True -> -1
    False -> 1
  }
  let #(yrs, _, _) = calendar_date_until(cal, ia, ib, Year)
  let r1 = truncate_div(yrs, inc) * inc
  let r2 = r1 + inc * sign
  use start <- result.try(calendar_date_add(
    cal,
    ia,
    DurRec(..zero_dur, years: r1),
    Constrain,
  ))
  use end_date <- result.map(calendar_date_add(
    cal,
    ia,
    DurRec(..zero_dur, years: r2),
    Constrain,
  ))
  let num = dest - epoch_days(start)
  let den = epoch_days(end_date) - epoch_days(start)
  round_between(
    int.absolute_value(r1),
    int.absolute_value(r2),
    num,
    den,
    inc,
    mode,
    sign,
  )
  * sign
}

// ----------------------------------------------------------------------------
// PlainMonthDay methods
// ----------------------------------------------------------------------------

fn plain_month_day_method(
  meth: PlainMonthDayMethod,
  protos: TemporalProtos,
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this_month_day(state, this) {
    None ->
      brand_error(state, "PlainMonthDay", plain_month_day_method_name(meth))
    Some(#(m, d, ry, cal)) -> {
      case meth {
        PmdToJson | PmdToLocaleString -> #(
          state,
          Ok(JsString(format_md_cal(m, d, ry, cal, CalAuto))),
        )
        PmdToString -> {
          use #(cal_name, _), state <- state.try_op(get_calendar_name_option(
            state,
            helpers.arg_at(args, 0),
          ))
          #(state, Ok(JsString(format_md_cal(m, d, ry, cal, cal_name))))
        }
        PmdValueOf ->
          state.type_error(
            state,
            "Temporal.PlainMonthDay cannot be converted with valueOf",
          )
        PmdEquals -> {
          use other, state <- state.try_op(to_temporal_month_day(
            state,
            helpers.arg_at(args, 0),
            JsUndefined,
          ))
          #(state, Ok(JsBool(#(m, d, ry, cal) == other)))
        }
        PmdWith -> {
          use bag, state <- with_partial_bag(state, args)
          use fields, state <- state.try_op(read_date_fields(state, bag, cal))
          use <- require_nonempty_fields(
            state,
            fields == DateFields(None, None, None, None, None, None),
          )
          use overflow, state <- state.try_op(validated_overflow(
            state,
            helpers.arg_at(args, 1),
          ))
          // Merge with the existing month-day's calendar fields.
          let cd = tcal.date_from_epoch_days(cal, epoch_days(IsoDate(ry, m, d)))
          let f = fields
          let f = case f.month != None || f.month_code != None {
            True -> f
            False -> {
              DateFields(
                ..f,
                month_code: Some(tcal.month_code_of(cal, cd.year, cd.month)),
              )
            }
          }
          let f = case f.day {
            Some(_) -> f
            None -> DateFields(..f, day: Some(cd.day))
          }
          use md <- terr(state, resolve_calendar_month_day(cal, f, overflow))
          let #(state, v) =
            make_month_day_cal(state, protos, md.0, md.1, md.2, md.3)
          #(state, Ok(v))
        }
        PmdToPlainDate -> {
          case helpers.arg_at(args, 0) {
            JsObject(ref) -> {
              use era, state <- state.try_op(case tcal.has_eras(cal) {
                True -> read_bag_era(state, ref)
                False -> Ok(#(None, state))
              })
              use era_year, state <- state.try_op(case tcal.has_eras(cal) {
                True -> read_int_field(state, ref, "eraYear")
                False -> Ok(#(None, state))
              })
              use year, state <- state.try_op(read_int_field(state, ref, "year"))
              // iso8601 has no eras, so an explicit `year` is the only thing
              // that can satisfy the "year is required" rule for it — matching
              // on the pair produces the year rather than asserting it later.
              case cal, year {
                tcal.Iso8601, Some(y) -> {
                  use date <- terr(state, regulate_iso_date(y, m, d, Constrain))
                  use date <- terr(state, check_date_limits(date))
                  let #(state, v) = make_date_cal(state, protos, date, cal)
                  #(state, Ok(v))
                }
                tcal.Iso8601, None ->
                  state.type_error(state, "year is required")
                _, _ ->
                  case year != None || { era != None && era_year != None } {
                    True -> {
                      let cd =
                        tcal.date_from_epoch_days(
                          cal,
                          epoch_days(IsoDate(ry, m, d)),
                        )
                      let mc = tcal.month_code_of(cal, cd.year, cd.month)
                      let f =
                        DateFields(
                          day: Some(cd.day),
                          era:,
                          era_year:,
                          month: None,
                          month_code: Some(mc),
                          year:,
                        )
                      use date <- terr(
                        state,
                        resolve_calendar_date(cal, f, Constrain),
                      )
                      use date <- terr(state, check_date_limits(date))
                      let #(state, v) = make_date_cal(state, protos, date, cal)
                      #(state, Ok(v))
                    }
                    False -> state.type_error(state, "year is required")
                  }
              }
            }
            _ -> state.type_error(state, "argument must be an object")
          }
        }
      }
    }
  }
}

/// Format a PlainMonthDay: non-ISO calendars always include the reference
/// year and the calendar annotation.
fn format_md_cal(
  m: Int,
  d: Int,
  ry: Int,
  cal: tcal.Calendar,
  mode: CalendarNameMode,
) -> String {
  case cal {
    tcal.Iso8601 ->
      case mode {
        CalAlways | CalCritical ->
          format_iso_date(IsoDate(ry, m, d)) <> calendar_suffix(mode, cal)
        CalAuto | CalNever -> pad2(m) <> "-" <> pad2(d)
      }
    _ ->
      case mode {
        CalNever -> format_iso_date(IsoDate(ry, m, d))
        CalAuto | CalAlways | CalCritical ->
          format_iso_date(IsoDate(ry, m, d)) <> calendar_suffix(mode, cal)
      }
  }
}

// ----------------------------------------------------------------------------
// Duration methods
// ----------------------------------------------------------------------------

fn duration_method(
  m: DurationMethod,
  protos: TemporalProtos,
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this_duration(state, this) {
    None -> brand_error(state, "Duration", duration_method_name(m))
    Some(d) ->
      case m {
        DmToJson | DmToLocaleString -> #(
          state,
          Ok(JsString(format_duration(d, AutoPrec))),
        )
        DmToString -> {
          use opts, state <- state.try_op(get_options_object(
            state,
            helpers.arg_at(args, 0),
          ))
          use digits, state <- state.try_op(get_fractional_digits(state, opts))
          use mode, state <- state.try_op(get_rounding_mode_option(
            state,
            opts,
            Trunc,
          ))
          use su, state <- state.try_op(get_unit_option(
            state,
            opts,
            "smallestUnit",
            allow_auto: False,
          ))
          use #(prec, runit, rinc) <- terr(
            state,
            duration_string_precision(digits, su),
          )
          use d2 <- terr(state, case runit == UNanosecond && rinc == 1 {
            True -> Ok(d)
            False -> round_duration_for_string(d, rinc, runit, mode)
          })
          #(state, Ok(JsString(format_duration(d2, prec))))
        }
        DmValueOf ->
          state.type_error(
            state,
            "Temporal.Duration cannot be converted with valueOf",
          )
        DmNegated -> {
          let #(state, v) = make_duration(state, protos, negate_dur(d))
          #(state, Ok(v))
        }
        DmAbs -> {
          let abs_d = case duration_sign(d) < 0 {
            True -> negate_dur(d)
            False -> d
          }
          let #(state, v) = make_duration(state, protos, abs_d)
          #(state, Ok(v))
        }
        DmWith -> {
          case helpers.arg_at(args, 0) {
            JsObject(ref) -> {
              use days, state <- state.try_op(read_integral_int_field(
                state,
                ref,
                "days",
              ))
              use hours, state <- state.try_op(read_integral_int_field(
                state,
                ref,
                "hours",
              ))
              use us, state <- state.try_op(read_integral_int_field(
                state,
                ref,
                "microseconds",
              ))
              use ms, state <- state.try_op(read_integral_int_field(
                state,
                ref,
                "milliseconds",
              ))
              use minutes, state <- state.try_op(read_integral_int_field(
                state,
                ref,
                "minutes",
              ))
              use months, state <- state.try_op(read_integral_int_field(
                state,
                ref,
                "months",
              ))
              use ns, state <- state.try_op(read_integral_int_field(
                state,
                ref,
                "nanoseconds",
              ))
              use seconds, state <- state.try_op(read_integral_int_field(
                state,
                ref,
                "seconds",
              ))
              use weeks, state <- state.try_op(read_integral_int_field(
                state,
                ref,
                "weeks",
              ))
              use years, state <- state.try_op(read_integral_int_field(
                state,
                ref,
                "years",
              ))
              let all = [
                days, hours, us, ms, minutes, months, ns, seconds, weeks, years,
              ]
              use <- require_nonempty_fields(
                state,
                list.all(all, fn(f) { f == None }),
              )
              let d2 =
                DurRec(
                  years: option.unwrap(years, d.years),
                  months: option.unwrap(months, d.months),
                  weeks: option.unwrap(weeks, d.weeks),
                  days: option.unwrap(days, d.days),
                  hours: option.unwrap(hours, d.hours),
                  minutes: option.unwrap(minutes, d.minutes),
                  seconds: option.unwrap(seconds, d.seconds),
                  ms: option.unwrap(ms, d.ms),
                  us: option.unwrap(us, d.us),
                  ns: option.unwrap(ns, d.ns),
                )
              finish_duration(state, protos, d2)
            }
            _ -> state.type_error(state, "argument must be an object")
          }
        }
        DmAdd | DmSubtract -> {
          use other, state <- state.try_op(to_temporal_duration(
            state,
            helpers.arg_at(args, 0),
          ))
          let other = case m {
            DmSubtract -> negate_dur(other)
            _ -> other
          }
          let has_cal =
            d.years != 0
            || d.months != 0
            || d.weeks != 0
            || other.years != 0
            || other.months != 0
            || other.weeks != 0
          case has_cal {
            True ->
              state.range_error(
                state,
                "duration add/subtract requires non-calendar durations",
              )
            False -> {
              let total = time_duration_ns(d) + time_duration_ns(other)
              let largest = larger_time_unit(d, other)
              let sum = balance_time_ns(total, largest)
              finish_duration(state, protos, sum)
            }
          }
        }
        DmRound -> duration_round(state, protos, d, args)
        DmTotal -> duration_total(state, d, args)
      }
  }
}

fn larger_time_unit(a: DurRec, b: DurRec) -> Unit {
  let unit_of = fn(d: DurRec) {
    case d.days != 0 {
      True -> Day
      False ->
        case d.hours != 0 {
          True -> Hour
          False ->
            case d.minutes != 0 {
              True -> Minute
              False ->
                case d.seconds != 0 {
                  True -> Second
                  False ->
                    case d.ms != 0 {
                      True -> Millisecond
                      False ->
                        case d.us != 0 {
                          True -> Microsecond
                          False -> Nanosecond
                        }
                    }
                }
            }
        }
    }
  }
  max_unit(unit_of(a), unit_of(b))
}

/// The largest unit with a nonzero field (DefaultTemporalLargestUnit).
fn default_largest_unit(d: DurRec) -> Unit {
  case d.years != 0 {
    True -> Year
    False ->
      case d.months != 0 {
        True -> Month
        False ->
          case d.weeks != 0 {
            True -> Week
            False -> larger_time_unit(d, zero_dur)
          }
      }
  }
}

/// Temporal.Duration.prototype.round ( roundTo )
fn duration_round(
  state: State(host),
  protos: TemporalProtos,
  d: DurRec,
  args: List(JsValue),
) -> #(State(host), Result(JsValue, JsValue)) {
  case helpers.arg_at(args, 0) {
    JsUndefined -> state.type_error(state, "options parameter is required")
    JsString(su_str) ->
      case singular_unit(su_str) {
        Some(su) ->
          duration_round_with(
            state,
            protos,
            d,
            None,
            su,
            1,
            HalfExpand,
            RelNone,
          )
        None -> state.range_error(state, "invalid smallestUnit")
      }
    JsObject(oref) -> {
      use largest, state <- state.try_op(get_unit_option_keep(
        state,
        Some(oref),
        "largestUnit",
      ))
      use rel_v, state <- state.try_op(ops_object.get_value(
        state,
        oref,
        Named("relativeTo"),
        JsObject(oref),
      ))
      use rel, state <- state.try_op(convert_relative_to(state, rel_v))
      use inc, state <- state.try_op(get_rounding_increment_option(
        state,
        Some(oref),
      ))
      use mode, state <- state.try_op(get_rounding_mode_option(
        state,
        Some(oref),
        HalfExpand,
      ))
      use smallest, state <- state.try_op(get_unit_option(
        state,
        Some(oref),
        "smallestUnit",
        allow_auto: False,
      ))
      case smallest == None && largest == UnitAbsent {
        True ->
          state.range_error(
            state,
            "at least one of smallestUnit or largestUnit is required",
          )
        False -> {
          let su = option.unwrap(smallest, Nanosecond)
          let lu = case largest {
            UnitValue(u) -> Some(u)
            UnitAuto | UnitAbsent -> None
          }
          duration_round_with(state, protos, d, lu, su, inc, mode, rel)
        }
      }
    }
    _ -> state.type_error(state, "invalid options")
  }
}

/// `largest` None means "auto": the duration's own default largest unit.
fn duration_round_with(
  state: State(host),
  protos: TemporalProtos,
  d: DurRec,
  largest: Option(Unit),
  smallest: Unit,
  inc: Int,
  mode: RoundingMode,
  rel: RelTo,
) -> #(State(host), Result(JsValue, JsValue)) {
  let largest = case largest {
    None -> max_unit(default_largest_unit(d), smallest)
    Some(u) -> u
  }
  let max_inc = case smallest {
    Hour -> Some(24)
    Minute | Second -> Some(60)
    Millisecond | Microsecond | Nanosecond -> Some(1000)
    _ -> None
  }
  let inc_invalid = case max_inc {
    Some(max) -> !valid_time_increment(inc, max)
    None -> False
  }
  let date_inc_invalid =
    inc > 1 && unit_rank(smallest) >= unit_rank(Day) && largest != smallest
  case
    largest_smaller_than_smallest(largest, smallest),
    inc_invalid || date_inc_invalid
  {
    True, _ -> state.range_error(state, largest_smaller_msg)
    _, True -> state.range_error(state, "invalid roundingIncrement")
    False, False ->
      case rel {
        RelNone -> {
          let needs_rel =
            d.years != 0
            || d.months != 0
            || d.weeks != 0
            || unit_rank(largest) > unit_rank(Day)
            || unit_rank(smallest) > unit_rank(Day)
          case needs_rel {
            True ->
              state.range_error(
                state,
                "relativeTo is required for calendar-unit rounding",
              )
            False -> {
              use su <- terr(state, require_time_unit(smallest))
              let total = time_duration_ns(d)
              let rounded =
                round_to_increment(total, inc * time_unit_ns(su), mode)
              let result = balance_time_ns(rounded, largest)
              finish_duration(state, protos, result)
            }
          }
        }
        RelZoned(rel_ns, tz, cal) -> {
          // DifferenceZonedDateTimeWithRounding between the anchor and
          // anchor + duration.
          use target_ns <- terr(state, add_zoned_ns(rel_ns, tz, cal, d))
          case unit_rank(largest) <= unit_rank(Hour) {
            True -> {
              use su <- terr(state, require_time_unit(smallest))
              let diff = target_ns - rel_ns
              let rounded =
                round_to_increment(diff, inc * time_unit_ns(su), mode)
              let result = balance_time_ns(rounded, largest)
              finish_duration(state, protos, result)
            }
            False -> {
              use result <- terr(
                state,
                case unit_rank(smallest) >= unit_rank(Day) {
                  // Calendar-unit (or zoned day) smallestUnit: wall-clock
                  // diff with calendar nudging.
                  True -> {
                    use a_dt <- result.try(epoch_ns_to_iso_in(tz, rel_ns))
                    use b_dt <- result.try(epoch_ns_to_iso_in(tz, target_ns))
                    diff_date_time_core(
                      cal,
                      a_dt,
                      b_dt,
                      largest,
                      smallest,
                      inc,
                      mode,
                      True,
                    )
                  }
                  // Time-unit smallestUnit: days are bounded by real instants
                  // and the time part is rounded within the day
                  // (NudgeToZonedTime).
                  False ->
                    zoned_diff_round_time(
                      cal,
                      tz,
                      rel_ns,
                      target_ns,
                      largest,
                      smallest,
                      inc,
                      mode,
                    )
                },
              )
              finish_duration(state, protos, result)
            }
          }
        }
        RelPlain(rel_date, rel_cal) -> {
          // A zero duration rounds to zero before the relativeTo date-time
          // is range-checked.
          let out_of_range =
            duration_sign(d) != 0
            && !iso_datetime_within_limits(rel_date, midnight)
          case out_of_range {
            True ->
              state.range_error(
                state,
                "relativeTo is outside the representable range after conversion to DateTime",
              )
            False -> {
              use target <- terr(state, duration_target_datetime(rel_date, d))
              use result <- terr(
                state,
                diff_date_time_core(
                  rel_cal,
                  #(rel_date, midnight),
                  target,
                  largest,
                  smallest,
                  inc,
                  mode,
                  False,
                ),
              )
              finish_duration(state, protos, result)
            }
          }
        }
      }
  }
}

/// DifferenceZonedDateTime + NudgeToZonedTime: difference between two zoned
/// instants, rounded at a time unit. Days are measured between wall-clock
/// instants (variable length); the time remainder is rounded within the
/// final day and carries into it when it overflows.
fn zoned_diff_round_time(
  cal: tcal.Calendar,
  tz: String,
  a_ns: Int,
  b_ns: Int,
  largest: Unit,
  smallest: Unit,
  inc: Int,
  mode: RoundingMode,
) -> Result(DurRec, TErr) {
  // Note: a zero difference still computes the next-day boundary, which can
  // throw when the anchor sits at the edge of the representable range
  // (NudgeToZonedTime always materialises both day bounds).
  {
    {
      use #(a_d, a_t) <- result.try(epoch_ns_to_iso_in(tz, a_ns))
      use #(b_d, b_t) <- result.try(epoch_ns_to_iso_in(tz, b_ns))
      let sign = case b_ns < a_ns {
        True -> -1
        False -> 1
      }
      // Wall-clock date difference with a time borrow.
      let tb = time_to_ns(b_t) - time_to_ns(a_t)
      let b_date = case sign > 0 && tb < 0, sign < 0 && tb > 0 {
        True, _ -> iso_date_from_epoch_days(epoch_days(b_d) - 1)
        _, True -> iso_date_from_epoch_days(epoch_days(b_d) + 1)
        _, _ -> b_d
      }
      let #(years, months, weeks, days) = case cal {
        tcal.Iso8601 -> diff_date_parts(a_d, b_date, largest)
        _ ->
          case largest {
            Year | Month -> {
              let #(y, m, rem_days) =
                calendar_date_until(cal, a_d, b_date, largest)
              #(y, m, 0, rem_days)
            }
            _ -> diff_date_parts(a_d, b_date, largest)
          }
      }
      let date_dur = DurRec(..zero_dur, years:, months:, weeks:, days:)
      use start_date <- result.try(calendar_date_add(
        cal,
        a_d,
        date_dur,
        Constrain,
      ))
      use start_ns <- result.try(get_epoch_ns_for(
        tz,
        start_date,
        a_t,
        Compatible,
      ))
      let time_rem = b_ns - start_ns
      case smallest == Nanosecond && inc == 1 {
        // Rounding is a noop: balance only, without materialising the
        // next-day boundary (which can be out of range at the edges).
        True -> {
          let time_part = balance_time_ns(time_rem, Hour)
          Ok(DurRec(..time_part, years:, months:, weeks:, days:))
        }
        False ->
          zoned_nudge_time(
            tz,
            #(a_d, a_t),
            start_date,
            start_ns,
            time_rem,
            #(years, months, weeks, days),
            sign,
            largest,
            smallest,
            inc,
            mode,
          )
      }
    }
  }
}

/// NudgeToZonedTime: round the time remainder within the (variable-length)
/// final day, carrying into it on overflow and bubbling into larger units.
fn zoned_nudge_time(
  tz: String,
  a_dt: #(IsoDate, TimeRec),
  start_date: IsoDate,
  start_ns: Int,
  time_rem: Int,
  ymwd: #(Int, Int, Int, Int),
  sign: Int,
  largest: Unit,
  smallest: Unit,
  inc: Int,
  mode: RoundingMode,
) -> Result(DurRec, TErr) {
  let #(a_d, a_t) = a_dt
  let #(years, months, weeks, days) = ymwd
  use su <- result.try(require_time_unit(smallest))
  let end_date = iso_date_from_epoch_days(epoch_days(start_date) + sign)
  use end_ns <- result.try(get_epoch_ns_for(tz, end_date, a_t, Compatible))
  let day_span = end_ns - start_ns
  let smallest_ns = inc * time_unit_ns(su)
  let rounded_t = round_to_increment(time_rem, smallest_ns, mode)
  let beyond = rounded_t - day_span
  case int_sign(beyond) != 0 - sign {
    // Rounded time fills (or exceeds) the whole day: carry one day and
    // round the remainder beyond it, then bubble into larger units.
    True -> {
      let rounded_t2 =
        round_to_increment(time_rem - day_span, smallest_ns, mode)
      let time_part = balance_time_ns(rounded_t2, Hour)
      let base = DurRec(..time_part, years:, months:, weeks:, days: days + sign)
      let nudged_inst = end_ns + rounded_t2
      use #(n_d, n_t) <- result.map(epoch_ns_to_iso_in(tz, nudged_inst))
      bubble_date_duration(
        sign,
        base,
        local_ns(n_d, n_t),
        #(a_d, a_t),
        largest,
        Day,
      )
    }
    False -> {
      let time_part = balance_time_ns(rounded_t, Hour)
      Ok(DurRec(..time_part, years:, months:, weeks:, days:))
    }
  }
}

/// rel + duration as an exact (date, time) pair.
fn duration_target_datetime(
  rel: IsoDate,
  d: DurRec,
) -> Result(#(IsoDate, TimeRec), TErr) {
  let date_only =
    DurRec(
      ..zero_dur,
      years: d.years,
      months: d.months,
      weeks: d.weeks,
      days: d.days,
    )
  use base <- result.try(add_duration_to_date(rel, date_only, Constrain))
  let time_ns = time_only_ns(d)
  let extra_days = floor_div(time_ns, ns_per_day)
  let rem = time_ns - extra_days * ns_per_day
  let final = iso_date_from_epoch_days(epoch_days(base) + extra_days)
  case iso_date_within_limits(final) {
    True -> Ok(#(final, ns_to_time(rem)))
    False -> Error(RangeE("date outside of supported range"))
  }
}

/// Temporal.Duration.prototype.total ( totalOf )
fn duration_total(
  state: State(host),
  d: DurRec,
  args: List(JsValue),
) -> #(State(host), Result(JsValue, JsValue)) {
  case helpers.arg_at(args, 0) {
    JsUndefined -> state.type_error(state, "totalOf is required")
    JsString(u_str) ->
      case singular_unit(u_str) {
        Some(u) -> duration_total_with(state, d, u, RelNone)
        None -> state.range_error(state, "invalid unit")
      }
    JsObject(oref) -> {
      use rel_v, state <- state.try_op(ops_object.get_value(
        state,
        oref,
        Named("relativeTo"),
        JsObject(oref),
      ))
      use rel, state <- state.try_op(convert_relative_to(state, rel_v))
      use unit_o, state <- state.try_op(get_unit_option(
        state,
        Some(oref),
        "unit",
        allow_auto: False,
      ))
      case unit_o {
        None -> state.range_error(state, "unit is required")
        Some(u) -> duration_total_with(state, d, u, rel)
      }
    }
    _ -> state.type_error(state, "invalid totalOf")
  }
}

fn duration_total_with(
  state: State(host),
  d: DurRec,
  unit: Unit,
  rel: RelTo,
) -> #(State(host), Result(JsValue, JsValue)) {
  case rel {
    RelNone -> {
      let needs_rel =
        d.years != 0
        || d.months != 0
        || d.weeks != 0
        || unit_rank(unit) > unit_rank(Day)
      case needs_rel {
        True ->
          state.range_error(
            state,
            "relativeTo is required to total calendar units",
          )
        False -> {
          use tu <- terr(state, require_time_unit(unit))
          let total = time_duration_ns(d)
          #(state, Ok(JsNumber(Finite(ns_div_float(total, time_unit_ns(tu))))))
        }
      }
    }
    RelZoned(anchor_ns, tz, cal) -> {
      use target_ns <- terr(state, add_zoned_ns(anchor_ns, tz, cal, d))
      case unit_rank(unit) <= unit_rank(Hour) {
        True -> {
          use tu <- terr(state, require_time_unit(unit))
          let diff = target_ns - anchor_ns
          #(state, Ok(JsNumber(Finite(ns_div_float(diff, time_unit_ns(tu))))))
        }
        False -> {
          // Whole calendar units in wall-clock space + fractional progress
          // between the bounding instants (NudgeToCalendarUnit, zoned).
          use #(a_d, a_t) <- terr(state, epoch_ns_to_iso_in(tz, anchor_ns))
          use #(b_d, b_t) <- terr(state, epoch_ns_to_iso_in(tz, target_ns))
          let sign = case target_ns < anchor_ns {
            True -> -1
            False -> 1
          }
          let tb = time_to_ns(b_t) - time_to_ns(a_t)
          let b_date = case sign > 0 && tb < 0, sign < 0 && tb > 0 {
            True, _ -> iso_date_from_epoch_days(epoch_days(b_d) - 1)
            _, True -> iso_date_from_epoch_days(epoch_days(b_d) + 1)
            _, _ -> b_d
          }
          let whole0 = case unit {
            Year -> diff_date_parts(a_d, b_date, Year).0
            Month -> diff_date_parts(a_d, b_date, Month).1
            Week -> truncate_div(epoch_days(b_date) - epoch_days(a_d), 7)
            _ -> epoch_days(b_date) - epoch_days(a_d)
          }
          let bound = fn(w: Int) {
            let date = case unit {
              Day -> iso_date_from_epoch_days(epoch_days(a_d) + w)
              _ -> add_calendar_units(a_d, unit, w)
            }
            get_epoch_ns_for(tz, date, a_t, Compatible)
          }
          use start0_ns <- terr(state, bound(whole0))
          use end0_ns <- terr(state, bound(whole0 + sign))
          let in_window = case sign > 0 {
            True -> start0_ns <= target_ns && target_ns <= end0_ns
            False -> end0_ns <= target_ns && target_ns <= start0_ns
          }
          let finish = fn(
            state: State(host),
            whole: Int,
            start_ns: Int,
            end_ns: Int,
          ) {
            let num = target_ns - start_ns
            let den = end_ns - start_ns
            // Single correctly-rounded division of the exact rational
            // whole + sign·num/den (NudgeToCalendarUnit's total).
            let total = case den == 0 {
              True -> int.to_float(whole)
              False -> ns_div_float(whole * den + sign * num, den)
            }
            #(state, Ok(JsNumber(Finite(total))))
          }
          case in_window {
            True -> finish(state, whole0, start0_ns, end0_ns)
            False -> {
              use e2 <- terr(state, bound(whole0 + 2 * sign))
              finish(state, whole0 + sign, end0_ns, e2)
            }
          }
        }
      }
    }
    RelPlain(rel_date, _rel_cal) -> {
      // A zero duration totals to zero before the relativeTo date-time is
      // range-checked.
      use Nil <- terr(
        state,
        case
          duration_sign(d) != 0
          && !iso_datetime_within_limits(rel_date, midnight)
        {
          True ->
            Error(RangeE(
              "relativeTo is outside the representable range after conversion to DateTime",
            ))
          False -> Ok(Nil)
        },
      )
      use target <- terr(state, duration_target_datetime(rel_date, d))
      let rel_ns = epoch_days(rel_date) * ns_per_day
      let target_ns = epoch_days(target.0) * ns_per_day + time_to_ns(target.1)
      case unit_rank(unit) <= unit_rank(Day) {
        True -> {
          use tu <- terr(state, require_time_unit(unit))
          let diff = target_ns - rel_ns
          #(state, Ok(JsNumber(Finite(ns_div_float(diff, time_unit_ns(tu))))))
        }
        False -> {
          // Whole calendar units + fractional progress between bounds
          // (NudgeToCalendarUnit). Per ComputeNudgeWindow, when the target
          // falls outside the first window the window is recomputed shifted
          // by one unit. That happens when day-of-month clamping makes the
          // date diff undercount (e.g. 2020-01-31 + 1 month lands on
          // 2020-02-29, which diffs back as 0 months).
          let sign = case target_ns < rel_ns {
            True -> -1
            False -> 1
          }
          let target_floor_days = floor_div(target_ns, ns_per_day)
          let target_date = iso_date_from_epoch_days(target_floor_days)
          let whole0 = case unit {
            Year -> diff_date_parts(rel_date, target_date, Year).0
            Month -> diff_date_parts(rel_date, target_date, Month).1
            _ -> truncate_div(epoch_days(target_date) - epoch_days(rel_date), 7)
          }
          // Window bounds come from CalendarDateAdd, which range-checks its
          // result (NudgeToCalendarUnit).
          let bound_ns = fn(w: Int) {
            use d2 <- result.map(
              check_date_limits(add_calendar_units(rel_date, unit, w)),
            )
            epoch_days(d2) * ns_per_day
          }
          use start0_ns <- terr(state, bound_ns(whole0))
          use end0_ns <- terr(state, bound_ns(whole0 + sign))
          let in_window = case sign > 0 {
            True -> start0_ns <= target_ns && target_ns <= end0_ns
            False -> end0_ns <= target_ns && target_ns <= start0_ns
          }
          use #(whole, start_ns, end_ns) <- terr(state, case in_window {
            True -> Ok(#(whole0, start0_ns, end0_ns))
            False -> {
              use e2 <- result.map(bound_ns(whole0 + 2 * sign))
              #(whole0 + sign, end0_ns, e2)
            }
          })
          let num = target_ns - start_ns
          let den = end_ns - start_ns
          // Single correctly-rounded division of the exact rational
          // whole + sign·num/den (NudgeToCalendarUnit's total).
          let total = case den == 0 {
            True -> int.to_float(whole)
            False -> ns_div_float(whole * den + sign * num, den)
          }
          #(state, Ok(JsNumber(Finite(total))))
        }
      }
    }
  }
}

fn add_calendar_units(d: IsoDate, unit: Unit, n: Int) -> IsoDate {
  case unit {
    Year -> add_months_constrained(d, n * 12)
    Month -> add_months_constrained(d, n)
    _ -> iso_date_from_epoch_days(epoch_days(d) + n * 7)
  }
}

/// Correctly-rounded integer ratio → double (round-to-nearest, ties-to-even).
/// Computing `q + r/b` in floats double-rounds near representability
/// boundaries; this scales the exact rational into [2^52, 2^53) and rounds
/// once (DivideTimeDuration operates on exact mathematical values).
fn ns_div_float(a: Int, b: Int) -> Float {
  case b < 0 {
    True -> ns_div_float(0 - a, 0 - b)
    False ->
      case a == 0 {
        True -> 0.0
        False -> {
          let neg = a < 0
          let #(q, s) = scale_ratio(int.absolute_value(a), b, 0)
          let f = case s >= 0 {
            True -> int.to_float(int.bitwise_shift_left(q, s))
            False ->
              int.to_float(q) /. int.to_float(int.bitwise_shift_left(1, 0 - s))
          }
          case neg {
            True -> 0.0 -. f
            False -> f
          }
        }
      }
  }
}

const two52 = 4_503_599_627_370_496

const two53 = 9_007_199_254_740_992

/// Scale a/b (both positive) by a power of two so the rounded quotient lands
/// in [2^52, 2^53), then round to nearest (ties to even).
/// Returns #(mantissa, exponent) with a/b ≈ mantissa × 2^exponent.
fn scale_ratio(a: Int, b: Int, s: Int) -> #(Int, Int) {
  case a >= b * two53 {
    True -> scale_ratio(a, b * 2, s + 1)
    False ->
      case a < b * two52 {
        True -> scale_ratio(a * 2, b, s - 1)
        False -> {
          let q0 = a / b
          let r = a - q0 * b
          let round_up = case int_sign(2 * r - b) {
            1 -> True
            -1 -> False
            _ -> q0 % 2 == 1
          }
          let q = case round_up {
            True -> q0 + 1
            False -> q0
          }
          case q == two53 {
            True -> #(two52, s + 1)
            False -> #(q, s)
          }
        }
      }
  }
}

/// ToSecondsStringPrecisionRecord for Duration.toString: only sub-minute
/// smallestUnit values are allowed. Returns #(precision, unit, increment).
fn duration_string_precision(
  digits: FractionalDigits,
  su: Option(Unit),
) -> Result(#(Precision, TimeUnit, Int), TErr) {
  case su {
    Some(Second) -> Ok(#(FixedPrec(0), USecond, 1))
    Some(Millisecond) -> Ok(#(FixedPrec(3), UMillisecond, 1))
    Some(Microsecond) -> Ok(#(FixedPrec(6), UMicrosecond, 1))
    Some(Nanosecond) -> Ok(#(FixedPrec(9), UNanosecond, 1))
    Some(u) ->
      Error(RangeE(
        unit_to_string(u)
        <> " is not a valid smallestUnit for Duration.toString",
      ))
    None ->
      case digits {
        DigitsAuto -> Ok(#(AutoPrec, UNanosecond, 1))
        DigitsFixed(0) -> Ok(#(FixedPrec(0), USecond, 1))
        DigitsFixed(n) -> Ok(#(FixedPrec(n), UNanosecond, pow10(9 - n)))
      }
  }
}

/// RoundTimeDuration + TemporalDurationFromInternal for Duration.toString:
/// round the time portion (hours and below) and rebalance, carrying into
/// days only when the duration's default largest unit is a date unit.
fn round_duration_for_string(
  d: DurRec,
  inc: Int,
  unit: TimeUnit,
  mode: RoundingMode,
) -> Result(DurRec, TErr) {
  let time_ns = time_only_ns(d)
  let rounded = round_to_increment(time_ns, inc * time_unit_ns(unit), mode)
  let largest = max_unit(default_largest_unit(d), Second)
  let result = case unit_rank(largest) >= unit_rank(Day) {
    True -> {
      let extra_days = truncate_div(rounded, ns_per_day)
      let rem = rounded - extra_days * ns_per_day
      let t = balance_time_ns(rem, Hour)
      DurRec(
        ..t,
        years: d.years,
        months: d.months,
        weeks: d.weeks,
        days: d.days + extra_days,
      )
    }
    False -> balance_time_ns(rounded, largest)
  }
  case is_valid_duration(result) {
    True -> Ok(result)
    False -> Error(RangeE("rounded duration is out of range"))
  }
}

/// ISO 8601 duration serialization.
fn format_duration(d: DurRec, prec: Precision) -> String {
  let sign = duration_sign(d)
  let prefix = case sign < 0 {
    True -> "-"
    False -> ""
  }
  let abs_part = fn(n: Int) { int.absolute_value(n) }
  let date_part =
    join_unit(abs_part(d.years), "Y")
    <> join_unit(abs_part(d.months), "M")
    <> join_unit(abs_part(d.weeks), "W")
    <> join_unit(abs_part(d.days), "D")
  // Sub-second components may exceed their unit (e.g. 1.8e16 microseconds);
  // carry whole seconds out of the combined sub-second total.
  let sub_total =
    abs_part(d.ms) * ns_per_ms + abs_part(d.us) * ns_per_us + abs_part(d.ns)
  let extra_seconds = sub_total / ns_per_second
  let sub = sub_total % ns_per_second
  let seconds_str = case
    d.seconds != 0
    || sub_total != 0
    || { date_part == "" && d.hours == 0 && d.minutes == 0 }
    || show_fixed_seconds(prec)
  {
    True -> {
      let frac = case prec {
        AutoPrec -> format_fraction(sub, AutoPrec)
        FixedPrec(0) -> ""
        FixedPrec(n) -> {
          let digits9 = int.to_string(sub) |> string.pad_start(9, "0")
          "." <> string.slice(digits9, 0, n)
        }
        MinutePrec -> ""
      }
      int.to_string(abs_part(d.seconds) + extra_seconds) <> frac <> "S"
    }
    False -> ""
  }
  let time_part =
    join_unit(abs_part(d.hours), "H")
    <> join_unit(abs_part(d.minutes), "M")
    <> seconds_str
  let t = case time_part {
    "" -> ""
    _ -> "T" <> time_part
  }
  prefix <> "P" <> date_part <> t
}

fn show_fixed_seconds(p: Precision) -> Bool {
  case p {
    FixedPrec(_) -> True
    _ -> False
  }
}

fn join_unit(n: Int, designator: String) -> String {
  case n == 0 {
    True -> ""
    False -> int.to_string(n) <> designator
  }
}

// ----------------------------------------------------------------------------
// Instant methods
// ----------------------------------------------------------------------------

fn instant_method(
  m: InstantMethod,
  protos: TemporalProtos,
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this_instant(state, this) {
    None -> brand_error(state, "Instant", instant_method_name(m))
    Some(ns) ->
      case m {
        ImToJson | ImToLocaleString -> #(
          state,
          Ok(JsString(format_instant(ns, AutoPrec))),
        )
        ImToString -> {
          use opts, state <- state.try_op(get_options_object(
            state,
            helpers.arg_at(args, 0),
          ))
          // Read options: fractionalSecondDigits, roundingMode, smallestUnit,
          // timeZone (alphabetical).
          use #(prec, su, sinc, mode), state <- state.try_op(
            to_string_time_options(state, opts),
          )
          use tz_opt, state <- state.try_op(case opts {
            None -> Ok(#(JsUndefined, state))
            Some(oref) ->
              ops_object.get_value(
                state,
                oref,
                Named("timeZone"),
                JsObject(oref),
              )
          })
          let rounded = case su {
            None -> ns
            Some(u) ->
              round_to_increment(
                ns,
                sinc * time_unit_ns(u),
                as_if_positive_mode(mode),
              )
          }
          case tz_opt {
            JsUndefined -> #(state, Ok(JsString(format_instant(rounded, prec))))
            JsString(tz_str) -> {
              use tz <- terr(state, parse_time_zone_id(tz_str))
              use off <- terr(state, tz_offset_ns_at(tz, rounded))
              let #(d, t) = epoch_ns_to_iso(rounded, off)
              let s =
                format_iso_date(d)
                <> "T"
                <> format_iso_time(t, prec)
                <> format_offset_rounded(off)
              #(state, Ok(JsString(s)))
            }
            _ -> state.type_error(state, "timeZone must be a string")
          }
        }
        ImValueOf ->
          state.type_error(
            state,
            "Temporal.Instant cannot be converted with valueOf",
          )
        ImEquals -> {
          use other, state <- state.try_op(to_temporal_instant(
            state,
            helpers.arg_at(args, 0),
          ))
          #(state, Ok(JsBool(ns == other)))
        }
        ImAdd | ImSubtract -> {
          use dur, state <- state.try_op(to_temporal_duration(
            state,
            helpers.arg_at(args, 0),
          ))
          case
            dur.years != 0 || dur.months != 0 || dur.weeks != 0 || dur.days != 0
          {
            True ->
              state.range_error(
                state,
                "Instant arithmetic does not support date units",
              )
            False -> {
              let delta = case m {
                ImSubtract -> 0 - time_only_ns(dur)
                _ -> time_only_ns(dur)
              }
              let ns2 = ns + delta
              case int.absolute_value(ns2) <= ns_max_instant {
                False -> state.range_error(state, "instant outside valid range")
                True -> {
                  let #(state, v) = make_instant(state, protos, ns2)
                  #(state, Ok(v))
                }
              }
            }
          }
        }
        ImRound -> {
          use #(su, inc, mode), state <- state.try_op(round_options(
            state,
            helpers.arg_at(args, 0),
            allow_day: False,
          ))
          let u_ns = time_unit_ns(su)
          // For Instant: increment*unit must divide 24h.
          let max = ns_per_day / u_ns
          case inc >= 1 && inc <= max && max % inc == 0 {
            False -> state.range_error(state, "invalid roundingIncrement")
            True -> {
              let rounded = round_to_increment(ns, inc * u_ns, mode)
              case int.absolute_value(rounded) <= ns_max_instant {
                False -> state.range_error(state, "instant outside valid range")
                True -> {
                  let #(state, v) = make_instant(state, protos, rounded)
                  #(state, Ok(v))
                }
              }
            }
          }
        }
        ImUntil | ImSince -> {
          use other, state <- state.try_op(to_temporal_instant(
            state,
            helpers.arg_at(args, 0),
          ))
          instant_until_since(state, protos, ns, other, args, m == ImSince)
        }
        ImToZonedDateTimeIso -> {
          case helpers.arg_at(args, 0) {
            JsString(tz_str) -> {
              use tz <- terr(state, parse_time_zone_id(tz_str))
              let #(state, v) = make_zoned(state, protos, ns, tz)
              #(state, Ok(v))
            }
            JsUndefined -> state.type_error(state, "time zone is required")
            _ -> state.type_error(state, "time zone must be a string")
          }
        }
      }
  }
}

fn format_instant(ns: Int, prec: Precision) -> String {
  let #(d, t) = epoch_ns_to_iso(ns, 0)
  format_iso_date(d) <> "T" <> format_iso_time(t, prec) <> "Z"
}

fn instant_until_since(
  state: State(host),
  protos: TemporalProtos,
  a: Int,
  b: Int,
  args: List(JsValue),
  is_since: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  use largest, smallest, inc, mode, state <- get_difference_settings(
    state,
    args,
  )
  let smallest = option.unwrap(smallest, Nanosecond)
  let largest = option.unwrap(largest, max_unit(smallest, Second))
  case
    unit_rank(smallest) > unit_rank(Hour)
    || unit_rank(largest) > unit_rank(Hour)
  {
    True -> state.range_error(state, "units must be time units for Instant")
    False -> {
      use <- require_largest_ge_smallest(state, largest, smallest)
      use su <- terr(state, require_time_unit(smallest))
      let mode2 = apply_since_mode(mode, is_since)
      let diff = b - a
      let rounded = round_to_increment(diff, inc * time_unit_ns(su), mode2)
      let rounded = apply_since_ns(rounded, is_since)
      let dur = balance_time_ns(rounded, largest)
      let #(state, v) = make_duration(state, protos, dur)
      #(state, Ok(v))
    }
  }
}

// ----------------------------------------------------------------------------
// ZonedDateTime methods
// ----------------------------------------------------------------------------

fn zoned_date_time_method(
  m: ZonedDateTimeMethod,
  protos: TemporalProtos,
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this_zoned(state, this) {
    None -> brand_error(state, "ZonedDateTime", zoned_date_time_method_name(m))
    Some(#(ns, tz, zcal)) -> {
      let _ = zcal
      use off <- terr(state, tz_offset_ns_at(tz, ns))
      let #(d, t) = epoch_ns_to_iso(ns, off)
      case m {
        ZmToJson | ZmToLocaleString -> {
          use s <- terr(state, format_zoned(ns, tz, AutoPrec))
          #(state, Ok(JsString(s)))
        }
        ZmToString -> {
          // Read order: calendarName, fractionalSecondDigits, offset,
          // roundingMode, smallestUnit, timeZoneName; validate after.
          use #(cal_name, opts), state <- state.try_op(get_calendar_name_option(
            state,
            helpers.arg_at(args, 0),
          ))
          use digits, state <- state.try_op(get_fractional_digits(state, opts))
          use offset_mode, state <- state.try_op(get_show_offset_option(
            state,
            opts,
          ))
          use mode, state <- state.try_op(get_rounding_mode_option(
            state,
            opts,
            Trunc,
          ))
          use su_opt, state <- state.try_op(get_unit_option(
            state,
            opts,
            "smallestUnit",
            allow_auto: False,
          ))
          use tz_mode, state <- state.try_op(get_time_zone_name_option(
            state,
            opts,
          ))
          use #(prec, su, sinc, mode) <- terr(
            state,
            seconds_string_precision(digits, su_opt, mode),
          )
          let rounded = case su {
            None -> ns
            Some(u) ->
              round_to_increment(
                ns,
                sinc * time_unit_ns(u),
                as_if_positive_mode(mode),
              )
          }
          use off2 <- terr(state, tz_offset_ns_at(tz, rounded))
          let #(d2, t2) = epoch_ns_to_iso(rounded, off2)
          let base = format_iso_date(d2) <> "T" <> format_iso_time(t2, prec)
          let with_offset = case offset_mode {
            OffsetShowNever -> base
            OffsetShowAuto -> base <> format_offset_rounded(off2)
          }
          let with_tz = case tz_mode {
            TzNever -> with_offset
            TzCritical -> with_offset <> "[!" <> tz <> "]"
            TzAuto -> with_offset <> "[" <> tz <> "]"
          }
          let s = with_tz <> calendar_suffix(cal_name, zcal)
          #(state, Ok(JsString(s)))
        }
        ZmValueOf ->
          state.type_error(
            state,
            "Temporal.ZonedDateTime cannot be converted with valueOf",
          )
        ZmEquals -> {
          use #(ons, otz, ocal), state <- state.try_op(to_temporal_zoned(
            state,
            helpers.arg_at(args, 0),
            JsUndefined,
          ))
          #(
            state,
            Ok(JsBool(ns == ons && time_zone_equals(tz, otz) && zcal == ocal)),
          )
        }
        ZmAdd | ZmSubtract -> {
          use dur, overflow, state <- add_sub_args(state, args, m == ZmSubtract)
          // Add date part in local wall-clock space, then exact time. Pure
          // time-unit durations add directly to the epoch (AddZonedDateTime).
          let date_dur =
            DurRec(
              ..zero_dur,
              years: dur.years,
              months: dur.months,
              weeks: dur.weeks,
              days: dur.days,
            )
          use base_ns <- terr(
            state,
            case
              dur.years == 0
              && dur.months == 0
              && dur.weeks == 0
              && dur.days == 0
            {
              True -> Ok(ns)
              False -> {
                use d2 <- result.try(calendar_date_add(
                  zcal,
                  d,
                  date_dur,
                  overflow,
                ))
                get_epoch_ns_for(tz, d2, t, Compatible)
              }
            },
          )
          let ns2 = base_ns + time_only_ns(dur)
          case int.absolute_value(ns2) <= ns_max_instant {
            False -> state.range_error(state, "instant outside valid range")
            True -> {
              let #(state, v) = make_zoned_cal(state, protos, ns2, tz, zcal)
              #(state, Ok(v))
            }
          }
        }
        ZmWithTimeZone -> {
          case helpers.arg_at(args, 0) {
            JsString(tz_str) -> {
              use tz2 <- terr(state, parse_time_zone_id(tz_str))
              let #(state, v) = make_zoned_cal(state, protos, ns, tz2, zcal)
              #(state, Ok(v))
            }
            _ -> state.type_error(state, "time zone must be a string")
          }
        }
        ZmUntil | ZmSince -> {
          use #(ons, otz, ocal), state <- state.try_op(to_temporal_zoned(
            state,
            helpers.arg_at(args, 0),
            JsUndefined,
          ))
          case ocal == zcal {
            False ->
              state.range_error(
                state,
                "cannot compute difference between dates of different calendars",
              )
            True ->
              zoned_until_since(
                state,
                protos,
                zcal,
                ns,
                tz,
                ons,
                otz,
                args,
                m == ZmSince,
              )
          }
        }
        ZmRound -> {
          use #(su, inc, mode), state <- state.try_op(round_options(
            state,
            helpers.arg_at(args, 0),
            allow_day: True,
          ))
          let u_ns = time_unit_ns(su)
          let max = case su {
            UDay -> 1
            UHour -> 24
            UMinute | USecond -> 60
            _ -> 1000
          }
          case valid_time_increment(inc, max) {
            False -> state.range_error(state, "invalid roundingIncrement")
            True -> {
              let local = ns + off
              let day_part = floor_div(local, ns_per_day)
              let local_date = iso_date_from_epoch_days(day_part)
              case su == UDay {
                // Round within the day bounded by start-of-day instants;
                // both bounds must be representable.
                True -> {
                  use day_start <- terr(state, start_of_day_ns(tz, local_date))
                  use day_end <- terr(
                    state,
                    start_of_day_ns(tz, iso_date_from_epoch_days(day_part + 1)),
                  )
                  let ns2 =
                    day_start
                    + round_to_increment(
                      ns - day_start,
                      day_end - day_start,
                      mode,
                    )
                  let #(state, v) = make_zoned_cal(state, protos, ns2, tz, zcal)
                  #(state, Ok(v))
                }
                False -> {
                  // Round the wall-clock time of day (RoundISODateTime),
                  // then reinterpret preferring the current offset.
                  let tod = local - day_part * ns_per_day
                  let rounded_tod = round_to_increment(tod, inc * u_ns, mode)
                  let #(rd, rt) =
                    epoch_ns_to_iso(day_part * ns_per_day + rounded_tod, 0)
                  use ns2 <- terr(
                    state,
                    interpret_offset(
                      rd,
                      rt,
                      OptionOffset(off),
                      tz,
                      Compatible,
                      PreferOffset,
                      False,
                    ),
                  )
                  case int.absolute_value(ns2) <= ns_max_instant {
                    False ->
                      state.range_error(state, "instant outside valid range")
                    True -> {
                      let #(state, v) =
                        make_zoned_cal(state, protos, ns2, tz, zcal)
                      #(state, Ok(v))
                    }
                  }
                }
              }
            }
          }
        }
        ZmWith -> {
          use bag, state <- with_partial_bag(state, args)
          use f, state <- state.try_op(read_date_time_fields(
            state,
            bag,
            zcal,
            read_offset: True,
            read_tz: False,
          ))
          use <- require_nonempty_fields(state, date_time_fields_all_none(f))
          use opts, state <- state.try_op(get_options_object(
            state,
            helpers.arg_at(args, 1),
          ))
          use dis_opt, state <- state.try_op(get_disambiguation_option(
            state,
            opts,
          ))
          use off_opt, state <- state.try_op(get_offset_option(
            state,
            opts,
            PreferOffset,
          ))
          use overflow, state <- state.try_op(get_overflow_option(state, opts))
          use date <- terr(
            state,
            calendar_with_fields(zcal, d, f.date, overflow),
          )
          let t0 = time_fields_apply(f.time, t)
          use t2 <- terr(state, regulate_time(t0, overflow))
          use ns2 <- terr(
            state,
            interpret_offset(
              date,
              t2,
              OptionOffset(option.unwrap(f.offset, off)),
              tz,
              dis_opt,
              off_opt,
              False,
            ),
          )
          let #(state, v) = make_zoned_cal(state, protos, ns2, tz, zcal)
          #(state, Ok(v))
        }
        ZmWithCalendar -> {
          use new_cal, state <- state.try_op(to_temporal_calendar_identifier(
            state,
            helpers.arg_at(args, 0),
          ))
          let #(state, v) = make_zoned_cal(state, protos, ns, tz, new_cal)
          #(state, Ok(v))
        }
        ZmWithPlainTime -> {
          // Undefined → GetStartOfDay; an explicit time (even midnight) uses
          // compatible disambiguation. These differ when midnight is skipped.
          case helpers.arg_at(args, 0) {
            JsUndefined -> {
              use ns2 <- terr(state, start_of_day_ns(tz, d))
              let #(state, v) = make_zoned_cal(state, protos, ns2, tz, zcal)
              #(state, Ok(v))
            }
            arg -> {
              use t2, state <- state.try_op(to_temporal_time(
                state,
                arg,
                JsUndefined,
              ))
              use ns2 <- terr(state, get_epoch_ns_for(tz, d, t2, Compatible))
              let #(state, v) = make_zoned_cal(state, protos, ns2, tz, zcal)
              #(state, Ok(v))
            }
          }
        }
        ZmStartOfDay -> {
          use ns2 <- terr(state, start_of_day_ns(tz, d))
          let #(state, v) = make_zoned_cal(state, protos, ns2, tz, zcal)
          #(state, Ok(v))
        }
        ZmGetTimeZoneTransition -> {
          use dir, state <- state.try_op(case helpers.arg_at(args, 0) {
            JsUndefined ->
              type_error_result(state, "direction parameter is required")
            JsString("next") -> Ok(#(Next, state))
            JsString("previous") -> Ok(#(Previous, state))
            JsString(_) ->
              range_error_result(state, "direction must be next or previous")
            JsObject(oref) -> {
              use #(dir, st) <- result.try(get_enum_option(
                state,
                Some(oref),
                "direction",
                [#("next", Some(Next)), #("previous", Some(Previous))],
                None,
              ))
              case dir {
                Some(d2) -> Ok(#(d2, st))
                None -> range_error_result(st, "direction is required")
              }
            }
            _ -> type_error_result(state, "invalid direction")
          })
          // UTC and offset zones have no transitions.
          case tz_kind(tz) {
            NamedZone(zone) -> {
              let found = case dir {
                Next -> temporal_tz.next_transition_ns(zone, ns)
                Previous -> temporal_tz.prev_transition_ns(zone, ns)
              }
              case found {
                // No further transition, or one outside the instant range.
                Ok(None) -> #(state, Ok(JsNull))
                Ok(Some(t_ns)) ->
                  case int.absolute_value(t_ns) <= ns_max_instant {
                    True -> {
                      let #(state, v) =
                        make_zoned_cal(state, protos, t_ns, tz, zcal)
                      #(state, Ok(v))
                    }
                    False -> #(state, Ok(JsNull))
                  }
                // Broken zoneinfo is not "no transition" — report it.
                Error(err) -> throw_terr(state, unloadable_tz(tz, err))
              }
            }
            UtcZone | OffsetZone(_) | UnknownZone -> #(state, Ok(JsNull))
          }
        }
        ZmToInstant -> {
          let #(state, v) = make_instant(state, protos, ns)
          #(state, Ok(v))
        }
        ZmToPlainDate -> {
          let #(state, v) = make_date_cal(state, protos, d, zcal)
          #(state, Ok(v))
        }
        ZmToPlainTime -> {
          let #(state, v) = make_time(state, protos, t)
          #(state, Ok(v))
        }
        ZmToPlainDateTime -> {
          let #(state, v) = make_date_time_cal(state, protos, d, t, zcal)
          #(state, Ok(v))
        }
      }
    }
  }
}

fn zoned_until_since(
  state: State(host),
  protos: TemporalProtos,
  cal: tcal.Calendar,
  a_ns: Int,
  a_tz: String,
  b_ns: Int,
  b_tz: String,
  args: List(JsValue),
  is_since: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  use largest, smallest, inc, mode, state <- get_difference_settings(
    state,
    args,
  )
  let smallest = option.unwrap(smallest, Nanosecond)
  let largest = option.unwrap(largest, max_unit(smallest, Hour))
  use Nil, state <- state.try_op(check_diff_setup(state, largest, smallest, inc))
  let mode2 = apply_since_mode(mode, is_since)
  case unit_rank(largest) <= unit_rank(Hour) {
    True -> {
      // Exact-time difference, like Instant.
      use su <- terr(state, require_time_unit(smallest))
      let diff = b_ns - a_ns
      let rounded = round_to_increment(diff, inc * time_unit_ns(su), mode2)
      let rounded = apply_since_ns(rounded, is_since)
      let dur = balance_time_ns(rounded, largest)
      let #(state, v) = make_duration(state, protos, dur)
      #(state, Ok(v))
    }
    False ->
      // Calendar-unit difference requires equal time zones (§ spec:
      // TimeZoneEquals, RangeError otherwise).
      case time_zone_equals(a_tz, b_tz) {
        False ->
          state.range_error(
            state,
            "time zones must be equal for calendar-unit differences",
          )
        True -> {
          use a_dt <- terr(state, epoch_ns_to_iso_in(a_tz, a_ns))
          use b_dt <- terr(state, epoch_ns_to_iso_in(a_tz, b_ns))
          use final <- terr(
            state,
            diff_date_time_core(
              cal,
              a_dt,
              b_dt,
              largest,
              smallest,
              inc,
              mode2,
              True,
            ),
          )
          let final = apply_since_dur(final, is_since)
          let #(state, v) = make_duration(state, protos, final)
          #(state, Ok(v))
        }
      }
  }
}

fn format_zoned(ns: Int, tz: String, prec: Precision) -> Result(String, TErr) {
  use off <- result.map(tz_offset_ns_at(tz, ns))
  let #(d, t) = epoch_ns_to_iso(ns, off)
  format_iso_date(d)
  <> "T"
  <> format_iso_time(t, prec)
  <> format_offset_rounded(off)
  <> "["
  <> tz
  <> "]"
}
