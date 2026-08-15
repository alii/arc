//// The Intl namespace (ECMA-402): Intl.getCanonicalLocales,
//// Intl.supportedValuesOf, and the service constructors
//// (Locale, Collator, NumberFormat, DateTimeFormat, PluralRules, ListFormat,
//// RelativeTimeFormat, Segmenter, DisplayNames, DurationFormat), plus the
//// §17-19 locale-sensitive overrides on the Number / BigInt / String / Date
//// prototypes.
////
//// Locale data is root/English with per-language separators: formatters
//// implement CLDR patterns in intl_format.gleam; tag parsing/
//// canonicalization is in intl_locale.gleam.

import arc/internal/gregorian.{days_from_civil}
import arc/internal/int_math.{floor_div}
import arc/internal/temporal_calendar as tcal
import arc/rt/builtins/common
import arc/rt/builtins/helpers.{first_arg_or_undefined}
import arc/rt/builtins/intl_collate.{collator_compare}
import arc/rt/builtins/intl_format.{
  PDay, PDayPeriod, PElement, PEra, PFractionalSecond, PHour, PLiteral, PMinute,
  PMonth, PSecond, PTimeZoneName, PWeekday, PYear,
} as fmt
import arc/rt/builtins/intl_locale as tags
import arc/rt/builtins/intl_segment as seg
import arc/rt/builtins/intl_timezone as tz
import arc/rt/builtins/realm_ops
import arc/rt/builtins/string as b_string
import arc/rt/call as rt_call
import arc/rt/intl_data.{
  type BoundGetterService, type CaseFirst, type CollatorSensitivity,
  type CollatorState, type CollatorUsage, type CompactDisplay,
  type ConstructibleService, type CurrencyDisplay, type CurrencySign,
  type DateStyle, type DateTimeFormatState, type DisplayNamesFallback,
  type DisplayNamesState, type DisplayNamesType, type DtfComponent,
  type DtfComponents, type DurationBaseStyle, type DurationDisplay,
  type DurationFormatState, type DurationUnitOptions, type DurationUnitStyle,
  type Granularity, type HourCycle, type IntlData, type IntlDigitOptions,
  type IntlService, type IntlUseGrouping, type LanguageDisplay,
  type ListFormatState, type ListFormatStyle, type ListFormatType,
  type LocaleState, type MonthWidth, type NameWidth, type Notation,
  type NumStyle, type NumberFormatState, type NumericWidth,
  type PluralRulesState, type PluralType, type RelativeTimeFormatState,
  type RoundingMode, type RoundingPriority, type RtfNumeric, type RtfStyle,
  type Segment, type SegmentIteratorState, type SegmenterState,
  type SegmentsState, type SignDisplay, type TimeStyle, type TimeZoneNameWidth,
  type TrailingZeroDisplay, type UnitDisplay, BgCollator, BgDateTimeFormat,
  BgNumberFormat, BsDigital, BsLong, BsNarrow, BsShort, Cardinal, CaseFirstFalse,
  CaseFirstLower, CaseFirstUpper, CollatorData, CollatorState, CompactLong,
  CompactShort, Conjunction, CsCollator, CsDateTimeFormat, CsDisplayNames,
  CsDurationFormat, CsListFormat, CsLocale, CsNumberFormat, CsPluralRules,
  CsRelativeTimeFormat, CsSegmenter, CurAccounting, CurCode, CurName,
  CurNarrowSymbol, CurStandard, CurSymbol, DateTimeFormatData,
  DateTimeFormatState, Disjunction, DisplayAlways, DisplayAuto, DisplayNamesData,
  DisplayNamesState, DnCalendar, DnCurrency, DnDateTimeField, DnLanguage,
  DnRegion, DnScript, DsFull, DsLong, DsMedium, DsShort, DtfComponents, DtfDay,
  DtfDayPeriod, DtfEra, DtfFractionalSecondDigits, DtfHour, DtfMinute, DtfMonth,
  DtfSecond, DtfTimeZoneName, DtfWeekday, DtfYear, DurFractional, DurLong,
  DurNarrow, DurNumeric, DurShort, DurTwoDigit, DurationFormatData,
  DurationFormatState, DurationUnitOptions, FbCode, FbNone, GGrapheme, GSentence,
  GWord, GroupingAlways, GroupingAuto, GroupingMin2, GroupingNever, H11, H12,
  H23, H24, HostZone, IntlDateTimeFormat, IntlDigitOptions, IntlDisplayNames,
  IntlDurationFormat, IntlListFormat, IntlNumberFormat, IntlPluralRules,
  IntlRelativeTimeFormat, IntlSegmentIterator, IntlSegments, LLong, LNarrow,
  LShort, LdDialect, LdStandard, ListFormatData, ListFormatState, LocaleData,
  LocaleState, MonthName, MonthNum, NotationCompact, NotationEngineering,
  NotationScientific, NotationStandard, NumberFormatData, NumberFormatState,
  Ordinal, PluralRulesData, PluralRulesState, PriorityAuto,
  PriorityLessPrecision, PriorityMorePrecision, RelativeTimeFormatData,
  RelativeTimeFormatState, RoundCeil, RoundExpand, RoundFloor, RoundHalfCeil,
  RoundHalfEven, RoundHalfExpand, RoundHalfFloor, RoundHalfTrunc, RoundTrunc,
  RtfAlways, RtfAuto, RtfLong, RtfNarrow, RtfShort, SegmentIteratorData,
  SegmentIteratorState, SegmenterData, SegmenterState, SegmentsData,
  SegmentsState, SensAccent, SensBase, SensCase, SensVariant, SignAlways,
  SignAuto, SignExceptZero, SignNegative, SignNever, StyleCurrency, StyleDecimal,
  StylePercent, StyleUnit, TsFull, TsLong, TsMedium, TsShort, TzLong,
  TzLongGeneric, TzLongOffset, TzShort, TzShortGeneric, TzShortOffset, TzdAuto,
  TzdStripIfInteger, UnitList, UnitLong, UnitNarrow, UnitShort, UsageSearch,
  UsageSort, WLong, WNarrow, WNumeric, WShort, WTwoDigit,
}
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type IntlHostOverrideName, type IntlMethodName,
  type IntlNative, type JsNum, type JsVal, type LocaleGetterName,
  type LocaleMethodName, type TemporalData, BigIntObj, BigIntToLocaleString,
  DateObj, DateToLocaleDateString, DateToLocaleString, DateToLocaleTimeString,
  Index, IntlBoundGetter, IntlBoundMethod, IntlConstructor, IntlFormat,
  IntlFormatRange, IntlFormatRangeToParts, IntlFormatToParts,
  IntlGetCanonicalLocales, IntlHostOverride, IntlLocaleGetter, IntlLocaleMethod,
  IntlMethod, IntlN, IntlObj, IntlOf, IntlResolvedOptions,
  IntlSegmentIteratorNext, IntlSegmenterSegment, IntlSegmentsContaining,
  IntlSegmentsIterator, IntlSelect, IntlSelectRange, IntlSupportedLocalesOf,
  IntlSupportedValuesOf, JFloat, JInt, JNan, JNegInf, JPosInf, KBig, KBool,
  KHandle, KNum, KStr, KUndef, LocaleBaseName, LocaleCalendar, LocaleCaseFirst,
  LocaleCollation, LocaleFirstDayOfWeek, LocaleGetCalendars, LocaleGetCollations,
  LocaleGetHourCycles, LocaleGetNumberingSystems, LocaleGetTextInfo,
  LocaleGetTimeZones, LocaleGetWeekInfo, LocaleHourCycle, LocaleLanguage,
  LocaleMaximize, LocaleMinimize, LocaleNumberingSystem, LocaleNumeric,
  LocaleRegion, LocaleScript, LocaleToString, LocaleVariants, Named, NumberObj,
  NumberToLocaleString, SObject, StringKey, StringLocaleCompare,
  StringToLocaleLowerCase, StringToLocaleUpperCase, SymbolKey, TemporalDate,
  TemporalDateTime, TemporalDuration, TemporalInstant, TemporalMonthDay,
  TemporalObj, TemporalTime, TemporalYearMonth, TemporalZonedDateTime, classify,
  mk_bool, mk_number, mk_object, mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

// ============================================================================
// Initialization
// ============================================================================

/// Build the Intl namespace and install the §17-19 overrides on the
/// Number / BigInt / String / Date prototypes. Returns the namespace object.
pub fn init(
  st: Agent,
  object_proto: Handle,
  function_proto: Handle,
  number_proto: Handle,
  bigint_proto: Handle,
  string_proto: Handle,
  date_proto: Handle,
) -> #(Handle, Agent) {
  // --- Intl.Locale ---
  let #(locale_getters, st) =
    common.alloc_getters(
      st,
      function_proto,
      list.map(
        [
          LocaleBaseName, LocaleCalendar, LocaleCaseFirst, LocaleCollation,
          LocaleFirstDayOfWeek, LocaleHourCycle, LocaleNumeric,
          LocaleNumberingSystem, LocaleLanguage, LocaleScript, LocaleRegion,
          LocaleVariants,
        ],
        fn(getter) {
          #(locale_getter_js_name(getter), IntlN(IntlLocaleGetter(getter)))
        },
      ),
    )
  let #(locale, st) =
    common.init_type(
      st,
      object_proto,
      function_proto,
      locale_getters,
      fn(proto) { IntlN(IntlConstructor(CsLocale, proto)) },
      "Locale",
      1,
      [],
    )
  let st = common.add_to_string_tag(st, locale.prototype, "Intl.Locale")
  // Locale methods need the prototype handle for maximize/minimize results.
  let #(locale_methods, st) =
    common.alloc_methods(
      st,
      function_proto,
      list.map(
        [
          LocaleToString, LocaleMaximize, LocaleMinimize, LocaleGetCalendars,
          LocaleGetCollations, LocaleGetHourCycles, LocaleGetNumberingSystems,
          LocaleGetTimeZones, LocaleGetTextInfo, LocaleGetWeekInfo,
        ],
        fn(method) {
          #(
            locale_method_js_name(method),
            IntlN(IntlLocaleMethod(method, locale.prototype)),
            0,
          )
        },
      ),
    )
  let st = add_named_properties(st, locale.prototype, locale_methods)

  // --- Simple formatter services ---
  let #(collator, st) =
    init_service(st, object_proto, function_proto, CsCollator, [], [
      #("compare", IntlN(IntlBoundGetter(BgCollator))),
    ])
  let #(number_format, st) =
    init_service(
      st,
      object_proto,
      function_proto,
      CsNumberFormat,
      [
        service_method(IntlNumberFormat, IntlFormatToParts, 1),
        service_method(IntlNumberFormat, IntlFormatRange, 2),
        service_method(IntlNumberFormat, IntlFormatRangeToParts, 2),
      ],
      [#("format", IntlN(IntlBoundGetter(BgNumberFormat)))],
    )
  let #(date_time_format, st) =
    init_service(
      st,
      object_proto,
      function_proto,
      CsDateTimeFormat,
      [
        service_method(IntlDateTimeFormat, IntlFormatToParts, 1),
        service_method(IntlDateTimeFormat, IntlFormatRange, 2),
        service_method(IntlDateTimeFormat, IntlFormatRangeToParts, 2),
      ],
      [#("format", IntlN(IntlBoundGetter(BgDateTimeFormat)))],
    )
  let #(plural_rules, st) =
    init_service(
      st,
      object_proto,
      function_proto,
      CsPluralRules,
      [
        service_method(IntlPluralRules, IntlSelect, 1),
        service_method(IntlPluralRules, IntlSelectRange, 2),
      ],
      [],
    )
  let #(list_format, st) =
    init_service(
      st,
      object_proto,
      function_proto,
      CsListFormat,
      [
        service_method(IntlListFormat, IntlFormat, 1),
        service_method(IntlListFormat, IntlFormatToParts, 1),
      ],
      [],
    )
  let #(relative_time_format, st) =
    init_service(
      st,
      object_proto,
      function_proto,
      CsRelativeTimeFormat,
      [
        service_method(IntlRelativeTimeFormat, IntlFormat, 2),
        service_method(IntlRelativeTimeFormat, IntlFormatToParts, 2),
      ],
      [],
    )
  let #(display_names, st) =
    init_service(
      st,
      object_proto,
      function_proto,
      CsDisplayNames,
      [service_method(IntlDisplayNames, IntlOf, 1)],
      [],
    )
  let #(duration_format, st) =
    init_service(
      st,
      object_proto,
      function_proto,
      CsDurationFormat,
      [
        service_method(IntlDurationFormat, IntlFormat, 1),
        service_method(IntlDurationFormat, IntlFormatToParts, 1),
      ],
      [],
    )

  // --- Segmenter (needs %SegmentIteratorPrototype% / %SegmentsPrototype%) ---
  let #(seg_iter_next, st) =
    common.alloc_methods(st, function_proto, [
      service_method(IntlSegmentIterator, IntlSegmentIteratorNext, 0),
    ])
  let #(seg_iter_proto, st) =
    common.init_namespace(
      st,
      object_proto,
      "Segmenter String Iterator",
      seg_iter_next,
    )
  let #(seg_containing, st) =
    common.alloc_methods(st, function_proto, [
      service_method(IntlSegments, IntlSegmentsContaining, 1),
    ])
  let #(segments_proto, st) =
    common.alloc_proto(st, Some(object_proto), dict.new())
  let st = add_named_properties(st, segments_proto, seg_containing)
  let #(seg_iter_fn, st) =
    common.alloc_rooted_native_fn(
      st,
      function_proto,
      IntlN(IntlSegmentsIterator(seg_iter_proto)),
      "[Symbol.iterator]",
      0,
    )
  let #(seg_iter_prop, st) = common.builtin_property(st, mk_object(seg_iter_fn))
  let st =
    common.add_symbol_property(
      st,
      segments_proto,
      types.symbol_iterator,
      seg_iter_prop,
    )
  let #(segment_method, st) =
    common.alloc_methods(st, function_proto, [
      #("segment", IntlN(IntlSegmenterSegment(segments_proto)), 1),
    ])
  let #(segmenter, st) =
    init_service(st, object_proto, function_proto, CsSegmenter, [], [])
  let st = add_named_properties(st, segmenter.prototype, segment_method)

  // --- Namespace object ---
  let #(ns_methods, st) =
    common.alloc_methods(st, function_proto, [
      #("getCanonicalLocales", IntlN(IntlGetCanonicalLocales), 1),
      #("supportedValuesOf", IntlN(IntlSupportedValuesOf), 1),
    ])
  let #(ctor_props, st) =
    list.fold(
      [
        #("Locale", locale),
        #("Collator", collator),
        #("NumberFormat", number_format),
        #("DateTimeFormat", date_time_format),
        #("PluralRules", plural_rules),
        #("ListFormat", list_format),
        #("RelativeTimeFormat", relative_time_format),
        #("Segmenter", segmenter),
        #("DisplayNames", display_names),
        #("DurationFormat", duration_format),
      ],
      #([], st),
      fn(acc, entry: #(String, types.BuiltinPair)) {
        let #(props, st) = acc
        let #(name, bt) = entry
        let #(prop, st) = common.builtin_property(st, mk_object(bt.constructor))
        #([#(name, prop), ..props], st)
      },
    )
  let #(namespace, st) =
    common.init_namespace(
      st,
      object_proto,
      "Intl",
      list.append(ns_methods, list.reverse(ctor_props)),
    )

  // ECMA-402 §17-19: locale-sensitive overrides on Number/BigInt/String/Date.
  let #(number_methods, st) =
    common.alloc_methods(st, function_proto, [
      #("toLocaleString", IntlN(IntlHostOverride(NumberToLocaleString)), 0),
    ])
  let st = add_named_properties(st, number_proto, number_methods)
  let #(bigint_methods, st) =
    common.alloc_methods(st, function_proto, [
      #("toLocaleString", IntlN(IntlHostOverride(BigIntToLocaleString)), 0),
    ])
  let st = add_named_properties(st, bigint_proto, bigint_methods)
  let #(string_methods, st) =
    common.alloc_methods(st, function_proto, [
      #("localeCompare", IntlN(IntlHostOverride(StringLocaleCompare)), 1),
      #(
        "toLocaleLowerCase",
        IntlN(IntlHostOverride(StringToLocaleLowerCase)),
        0,
      ),
      #(
        "toLocaleUpperCase",
        IntlN(IntlHostOverride(StringToLocaleUpperCase)),
        0,
      ),
    ])
  let st = add_named_properties(st, string_proto, string_methods)
  let #(date_methods, st) =
    common.alloc_methods(st, function_proto, [
      #("toLocaleString", IntlN(IntlHostOverride(DateToLocaleString)), 0),
      #(
        "toLocaleDateString",
        IntlN(IntlHostOverride(DateToLocaleDateString)),
        0,
      ),
      #(
        "toLocaleTimeString",
        IntlN(IntlHostOverride(DateToLocaleTimeString)),
        0,
      ),
    ])
  let st = add_named_properties(st, date_proto, date_methods)

  #(namespace, st)
}

/// Build one formatter service: prototype methods + accessor getters +
/// resolvedOptions + supportedLocalesOf static + @@toStringTag.
fn init_service(
  st: Agent,
  object_proto: Handle,
  function_proto: Handle,
  service: ConstructibleService,
  methods: List(#(String, types.NativeToken, Int)),
  accessors: List(#(String, types.NativeToken)),
) -> #(types.BuiltinPair, Agent) {
  let arity = case service {
    CsDisplayNames -> 2
    _ -> 0
  }
  // The brand instances of this service carry: `resolvedOptions` and
  // `supportedLocalesOf` are shared by every service, constructible or not.
  let brand = intl_data.constructible_service(service)
  let name = service_name(brand)
  let #(proto_methods, st) =
    common.alloc_methods(st, function_proto, [
      #("resolvedOptions", IntlN(IntlResolvedOptions(brand)), 0),
      ..methods
    ])
  let #(proto_accessors, st) =
    common.alloc_getters(st, function_proto, accessors)
  let #(slo, st) =
    common.alloc_methods(st, function_proto, [
      #("supportedLocalesOf", IntlN(IntlSupportedLocalesOf(brand)), 1),
    ])
  let #(bt, st) =
    common.init_type(
      st,
      object_proto,
      function_proto,
      list.append(proto_accessors, proto_methods),
      fn(proto) { IntlN(IntlConstructor(service:, proto:)) },
      name,
      arity,
      slo,
    )
  let st = common.add_to_string_tag(st, bt.prototype, "Intl." <> name)
  #(bt, st)
}

/// A prototype-method registration triple: the JS property name is derived
/// from the `IntlMethodName` variant so the two can never disagree.
fn service_method(
  service: IntlService,
  method: IntlMethodName,
  arity: Int,
) -> #(String, types.NativeToken, Int) {
  #(intl_method_js_name(method), IntlN(IntlMethod(service:, method:)), arity)
}

/// The JS property name an `IntlMethodName` is installed under.
fn intl_method_js_name(method: IntlMethodName) -> String {
  case method {
    IntlFormat -> "format"
    IntlFormatToParts -> "formatToParts"
    IntlFormatRange -> "formatRange"
    IntlFormatRangeToParts -> "formatRangeToParts"
    IntlSelect -> "select"
    IntlSelectRange -> "selectRange"
    IntlOf -> "of"
    IntlSegmentIteratorNext -> "next"
    IntlSegmentsContaining -> "containing"
  }
}

/// The JS property name an Intl.Locale.prototype getter is installed under.
fn locale_getter_js_name(getter: LocaleGetterName) -> String {
  case getter {
    LocaleBaseName -> "baseName"
    LocaleCalendar -> "calendar"
    LocaleCaseFirst -> "caseFirst"
    LocaleCollation -> "collation"
    LocaleFirstDayOfWeek -> "firstDayOfWeek"
    LocaleHourCycle -> "hourCycle"
    LocaleNumeric -> "numeric"
    LocaleNumberingSystem -> "numberingSystem"
    LocaleLanguage -> "language"
    LocaleScript -> "script"
    LocaleRegion -> "region"
    LocaleVariants -> "variants"
  }
}

/// The JS property name an Intl.Locale.prototype method is installed under.
fn locale_method_js_name(method: LocaleMethodName) -> String {
  case method {
    LocaleToString -> "toString"
    LocaleMaximize -> "maximize"
    LocaleMinimize -> "minimize"
    LocaleGetCalendars -> "getCalendars"
    LocaleGetCollations -> "getCollations"
    LocaleGetHourCycles -> "getHourCycles"
    LocaleGetNumberingSystems -> "getNumberingSystems"
    LocaleGetTimeZones -> "getTimeZones"
    LocaleGetTextInfo -> "getTextInfo"
    LocaleGetWeekInfo -> "getWeekInfo"
  }
}

/// Insert named builtin properties into an existing object.
fn add_named_properties(
  st: Agent,
  h: Handle,
  props: List(#(String, types.Property)),
) -> Agent {
  list.fold(props, st, fn(st, p) { common.add_named_property(st, h, p.0, p.1) })
}

// ============================================================================
// Dispatch
// ============================================================================

/// Per-module [[Call]] dispatch. A service constructor reached here was
/// called without `new` (NewTarget undefined).
pub fn dispatch(
  st: Agent,
  native: IntlNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    IntlGetCanonicalLocales -> get_canonical_locales(st, args)
    IntlSupportedValuesOf -> supported_values_of(st, args)
    IntlConstructor(service:, proto:) ->
      construct_service(st, service, proto, args, mk_undefined())
    IntlSupportedLocalesOf(_service) -> supported_locales_of(st, args)
    IntlResolvedOptions(service:) -> resolved_options(st, service, this)
    IntlBoundGetter(service:) -> bound_getter(st, service, this)
    IntlBoundMethod(service:, target:) ->
      bound_method(st, service, target, args)
    IntlMethod(service:, method:) -> run_method(st, service, method, this, args)
    IntlHostOverride(which:) -> run_host_override(st, which, this, args)
    IntlSegmenterSegment(segments_proto:) ->
      segmenter_segment(st, segments_proto, this, args)
    IntlSegmentsIterator(iter_proto:) -> segments_iterator(st, iter_proto, this)
    IntlLocaleGetter(name:) -> locale_getter(st, name, this)
    IntlLocaleMethod(method:, proto:) -> locale_method(st, method, proto, this)
  }
}

/// Per-module [[Construct]] dispatch — only the service constructors are
/// constructible.
pub fn dispatch_construct(
  st: Agent,
  native: IntlNative,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case native {
    IntlConstructor(service:, proto:) -> {
      let #(v, st) = construct_service(st, service, proto, args, new_target)
      case classify(v) {
        KHandle(h) -> #(h, st)
        _ -> panic as "Intl constructor returned a non-object"
      }
    }
    _ -> rt_val.t_throw_type_error(st, "not a constructor")
  }
}

// ============================================================================
// Shared plumbing
// ============================================================================

/// Read the IntlObj state for `this`, throwing TypeError on brand mismatch.
/// Yields the receiver handle, its state, and the bound-method cache.
fn branded(
  st: Agent,
  this: JsVal,
  service: IntlService,
  method: String,
) -> #(Handle, IntlData, Option(Handle)) {
  use data <- branded_of(st, this, method)
  case intl_data.intl_service(data) == service {
    True -> Some(data)
    False -> None
  }
}

// --- `branded` narrowed to one service ------------------------------------
//
// Each extractor pattern-matches the receiver's `IntlData` variant directly,
// so callers get the concrete state record instead of the sum plus a
// hand-written "cannot happen" arm.

/// The narrowing helper behind the service-specific `branded_*` extractors.
fn branded_of(
  st: Agent,
  this: JsVal,
  method: String,
  extract: fn(IntlData) -> Option(a),
) -> #(Handle, a, Option(Handle)) {
  let found =
    helpers.brand_of(st, this, fn(kind) {
      case kind {
        IntlObj(data:, bound:) ->
          option.map(extract(data), fn(v) { #(v, bound) })
        _ -> None
      }
    })
  case found {
    Some(#(#(v, bound), h)) -> #(h, v, bound)
    None ->
      rt_val.t_throw_type_error(
        st,
        method <> " called on incompatible receiver",
      )
  }
}

fn branded_locale(
  st: Agent,
  this: JsVal,
  method: String,
) -> #(Handle, LocaleState, Option(Handle)) {
  use data <- branded_of(st, this, method)
  case data {
    LocaleData(l) -> Some(l)
    _other -> None
  }
}

fn branded_segmenter(
  st: Agent,
  this: JsVal,
  method: String,
) -> #(Handle, SegmenterState, Option(Handle)) {
  use data <- branded_of(st, this, method)
  case data {
    SegmenterData(s) -> Some(s)
    _other -> None
  }
}

fn branded_segments(
  st: Agent,
  this: JsVal,
  method: String,
) -> #(Handle, SegmentsState, Option(Handle)) {
  use data <- branded_of(st, this, method)
  case data {
    SegmentsData(s) -> Some(s)
    _other -> None
  }
}

fn branded_collator(
  st: Agent,
  this: JsVal,
  method: String,
) -> #(Handle, CollatorState, Option(Handle)) {
  use data <- branded_of(st, this, method)
  case data {
    CollatorData(c) -> Some(c)
    _other -> None
  }
}

fn branded_number_format(
  st: Agent,
  this: JsVal,
  method: String,
) -> #(Handle, NumberFormatState, Option(Handle)) {
  use data <- branded_of(st, this, method)
  case data {
    NumberFormatData(nf) -> Some(nf)
    _other -> None
  }
}

fn branded_date_time_format(
  st: Agent,
  this: JsVal,
  method: String,
) -> #(Handle, DateTimeFormatState, Option(Handle)) {
  use data <- branded_of(st, this, method)
  case data {
    DateTimeFormatData(d) -> Some(d)
    _other -> None
  }
}

/// Replace an Intl instance's per-service state in its cell, keeping every
/// other object attribute (including the bound-method cache).
fn write_intl_data(st: Agent, h: Handle, data: IntlData) -> Agent {
  rt_store.t_cell_update(st, h, fn(slot) {
    case slot {
      SObject(kind: IntlObj(bound:, ..), ..) ->
        SObject(..slot, kind: IntlObj(data:, bound:))
      other -> other
    }
  })
}

/// Whether a DateTimeFormat formatting component is part of the format.
fn has_component(c: DtfComponents, which: DtfComponent) -> Bool {
  case which {
    DtfWeekday -> option.is_some(c.weekday)
    DtfEra -> option.is_some(c.era)
    DtfYear -> option.is_some(c.year)
    DtfMonth -> option.is_some(c.month)
    DtfDay -> option.is_some(c.day)
    DtfDayPeriod -> option.is_some(c.day_period)
    DtfHour -> option.is_some(c.hour)
    DtfMinute -> option.is_some(c.minute)
    DtfSecond -> option.is_some(c.second)
    DtfFractionalSecondDigits -> option.is_some(c.fractional_second_digits)
    DtfTimeZoneName -> option.is_some(c.time_zone_name)
  }
}

/// Keep a component's width only when the component is in `keep`.
fn kept_width(
  keep: List(DtfComponent),
  which: DtfComponent,
  v: Option(a),
) -> Option(a) {
  case list.contains(keep, which) {
    True -> v
    False -> None
  }
}

/// Drop every component not in `keep`, keeping the widths of the rest.
fn keep_components(
  c: DtfComponents,
  keep: List(DtfComponent),
) -> DtfComponents {
  DtfComponents(
    weekday: kept_width(keep, DtfWeekday, c.weekday),
    era: kept_width(keep, DtfEra, c.era),
    year: kept_width(keep, DtfYear, c.year),
    month: kept_width(keep, DtfMonth, c.month),
    day: kept_width(keep, DtfDay, c.day),
    day_period: kept_width(keep, DtfDayPeriod, c.day_period),
    hour: kept_width(keep, DtfHour, c.hour),
    minute: kept_width(keep, DtfMinute, c.minute),
    second: kept_width(keep, DtfSecond, c.second),
    fractional_second_digits: kept_width(
      keep,
      DtfFractionalSecondDigits,
      c.fractional_second_digits,
    ),
    time_zone_name: kept_width(keep, DtfTimeZoneName, c.time_zone_name),
  )
}

/// Field-wise overlay: `base`'s component wins where present, else `fallback`.
fn merge_components(
  base: DtfComponents,
  fallback: DtfComponents,
) -> DtfComponents {
  DtfComponents(
    weekday: option.or(base.weekday, fallback.weekday),
    era: option.or(base.era, fallback.era),
    year: option.or(base.year, fallback.year),
    month: option.or(base.month, fallback.month),
    day: option.or(base.day, fallback.day),
    day_period: option.or(base.day_period, fallback.day_period),
    hour: option.or(base.hour, fallback.hour),
    minute: option.or(base.minute, fallback.minute),
    second: option.or(base.second, fallback.second),
    fractional_second_digits: option.or(
      base.fractional_second_digits,
      fallback.fractional_second_digits,
    ),
    time_zone_name: option.or(base.time_zone_name, fallback.time_zone_name),
  )
}

/// Overlay a formatter's resolved digit options onto a `fmt.NumOpts` base.
fn with_digits(o: fmt.NumOpts, dg: IntlDigitOptions) -> fmt.NumOpts {
  let precision = fn(p: #(Int, Int)) { fmt.Precision(min: p.0, max: p.1) }
  fmt.NumOpts(
    ..o,
    min_int: dg.minimum_integer_digits,
    frac: option.map(dg.fraction_digits, precision),
    sig: option.map(dg.significant_digits, precision),
    rounding_increment: dg.rounding_increment,
    rounding_mode: dg.rounding_mode,
    rounding_priority: dg.rounding_priority,
    trailing_zero_display: dg.trailing_zero_display,
  )
}

fn alloc_array(st: Agent, values: List(JsVal)) -> #(JsVal, Agent) {
  let #(h, st) = realm_ops.alloc_array(st, values)
  #(mk_object(h), st)
}

fn alloc_pojo(st: Agent, props: List(#(String, JsVal))) -> #(JsVal, Agent) {
  let #(h, st) = common.alloc_pojo(st, st.realm.object.prototype, props)
  #(mk_object(h), st)
}

/// Parts → JS array of `{ type, value }` objects.
fn parts_to_js(st: Agent, parts: List(fmt.Part)) -> #(JsVal, Agent) {
  let #(objs, st) =
    list.fold(parts, #([], st), fn(acc, part) {
      let #(objs, st) = acc
      let #(t, v) = part
      let #(obj, st) =
        alloc_pojo(st, [
          #("type", mk_string(fmt.part_type_to_js_string(t))),
          #("value", mk_string(v)),
        ])
      #([obj, ..objs], st)
    })
  alloc_array(st, list.reverse(objs))
}

/// The formatted string a range's parts spell out (formatRange).
fn range_parts_to_string(parts: List(fmt.RangePart)) -> String {
  parts |> list.map(fn(p: fmt.RangePart) { p.value }) |> string.join("")
}

/// Parts → JS array of `{ type, value, source }` objects (formatRangeToParts).
fn parts_to_js_sourced(
  st: Agent,
  parts: List(fmt.RangePart),
) -> #(JsVal, Agent) {
  let #(objs, st) =
    list.fold(parts, #([], st), fn(acc, part: fmt.RangePart) {
      let #(objs, st) = acc
      let #(obj, st) =
        alloc_pojo(st, [
          #("type", mk_string(fmt.part_type_to_js_string(part.type_))),
          #("value", mk_string(part.value)),
          #("source", mk_string(fmt.part_source_to_js_string(part.source))),
        ])
      #([obj, ..objs], st)
    })
  alloc_array(st, list.reverse(objs))
}

/// Parts → JS array of `{ type, value, unit? }` objects (DurationFormat
/// formatToParts). `unit: None` means no unit property.
fn parts_to_js_with_unit(
  st: Agent,
  parts: List(fmt.UnitPart),
) -> #(JsVal, Agent) {
  let #(objs, st) =
    list.fold(parts, #([], st), fn(acc, part: fmt.UnitPart) {
      let #(objs, st) = acc
      let base = [
        #("type", mk_string(fmt.part_type_to_js_string(part.type_))),
        #("value", mk_string(part.value)),
      ]
      let props = case part.unit {
        None -> base
        Some(unit) -> list.append(base, [#("unit", mk_string(unit))])
      }
      let #(obj, st) = alloc_pojo(st, props)
      #([obj, ..objs], st)
    })
  alloc_array(st, list.reverse(objs))
}

// ============================================================================
// Options helpers (ECMA-402 §9.2.10–9.2.17)
// ============================================================================

/// CoerceOptionsToObject: undefined → no options; else ToObject.
fn coerce_options(st: Agent, v: JsVal) -> #(Option(Handle), Agent) {
  case classify(v) {
    KUndef -> #(None, st)
    _ -> {
      let #(h, st) = rt_val.t_to_object(st, v)
      #(Some(h), st)
    }
  }
}

/// GetOptionsObject: undefined → none; Object → it; else TypeError.
fn strict_options(st: Agent, v: JsVal) -> #(Option(Handle), Agent) {
  case classify(v) {
    KUndef -> #(None, st)
    KHandle(h) -> #(Some(h), st)
    _ -> rt_val.t_throw_type_error(st, "options must be an object or undefined")
  }
}

fn opt_get(st: Agent, opts: Option(Handle), name: String) -> #(JsVal, Agent) {
  case opts {
    None -> #(mk_undefined(), st)
    Some(h) -> rt_obj.t_get_prop(st, mk_object(h), StringKey(Named(name)))
  }
}

/// GetOption with type string. Empty `allowed` list = any string allowed.
fn get_str_opt(
  st: Agent,
  opts: Option(Handle),
  name: String,
  allowed: List(String),
  default: Option(String),
) -> #(Option(String), Agent) {
  let #(v, st) = opt_get(st, opts, name)
  case classify(v) {
    KUndef -> #(default, st)
    _ -> {
      let #(s, st) = rt_val.t_to_string(st, v)
      case allowed == [] || list.contains(allowed, s) {
        True -> #(Some(s), st)
        False ->
          rt_val.t_throw_range_error(
            st,
            "Value " <> s <> " out of range for options property " <> name,
          )
      }
    }
  }
}

/// GetOption for a closed string-enum option. `variants` is the full option
/// set as #(spec spelling, variant) pairs: it is both the validation list and
/// the (single) place the spelling is turned into its typed variant, so an
/// out-of-set spelling always throws instead of silently defaulting.
fn get_enum_opt(
  st: Agent,
  opts: Option(Handle),
  name: String,
  variants: List(#(String, a)),
  default: a,
) -> #(a, Agent) {
  let #(v, st) = opt_get(st, opts, name)
  case classify(v) {
    KUndef -> #(default, st)
    _ -> {
      let #(s, st) = rt_val.t_to_string(st, v)
      case list.key_find(variants, s) {
        Ok(variant) -> #(variant, st)
        Error(Nil) ->
          rt_val.t_throw_range_error(
            st,
            "Value " <> s <> " out of range for options property " <> name,
          )
      }
    }
  }
}

/// GetOption with type boolean.
fn get_bool_opt(
  st: Agent,
  opts: Option(Handle),
  name: String,
  default: Option(Bool),
) -> #(Option(Bool), Agent) {
  let #(v, st) = opt_get(st, opts, name)
  case classify(v) {
    KUndef -> #(default, st)
    _ -> #(Some(rt_val.to_boolean(v)), st)
  }
}

/// GetNumberOption/DefaultNumberOption (§9.2.16/9.2.17).
fn get_num_opt(
  st: Agent,
  opts: Option(Handle),
  name: String,
  min: Int,
  max: Int,
  default: Option(Int),
) -> #(Option(Int), Agent) {
  let #(v, st) = opt_get(st, opts, name)
  default_number_option(st, v, min, max, default, name)
}

fn default_number_option(
  st: Agent,
  v: JsVal,
  min: Int,
  max: Int,
  default: Option(Int),
  name: String,
) -> #(Option(Int), Agent) {
  case classify(v) {
    KUndef -> #(default, st)
    _ -> {
      let #(n, st) = rt_val.t_to_number(st, v)
      let f = case n {
        JInt(i) -> Some(int.to_float(i))
        JFloat(f) -> Some(f)
        JNan | JPosInf | JNegInf -> None
      }
      case f {
        Some(f) ->
          // Range check happens on the unrounded value (§9.2.17).
          case f >=. int.to_float(min) && f <=. int.to_float(max) {
            True -> #(Some(float.truncate(float.floor(f))), st)
            False ->
              rt_val.t_throw_range_error(
                st,
                name <> " value is out of range: " <> float.to_string(f),
              )
          }
        None -> rt_val.t_throw_range_error(st, name <> " value is out of range")
      }
    }
  }
}

/// "type" nonterminal check: (3*8alphanum) ("-" (3*8alphanum))*
fn is_type_sequence(s: String) -> Bool {
  let parts = string.split(s, "-")
  parts != []
  && list.all(parts, fn(p) {
    let n = string.length(p)
    n >= 3 && n <= 8 && tags.is_alnum(p)
  })
}

// ============================================================================
// CanonicalizeLocaleList (§9.2.1)
// ============================================================================

fn canonicalize_locale_list(
  st: Agent,
  locales: JsVal,
) -> #(List(String), Agent) {
  case classify(locales) {
    KUndef -> #([], st)
    KStr(s) -> {
      let #(tag, st) = canonical_tag_or_throw(st, s)
      #([tag], st)
    }
    // Step 4: an Intl.Locale object passes its [[Locale]] straight through.
    KHandle(h) ->
      case locale_of_handle(st, h) {
        Some(l) -> #([l.locale], st)
        None -> locale_list_from_object(st, h)
      }
    _ -> {
      let #(h, st) = rt_val.t_to_object(st, locales)
      locale_list_from_object(st, h)
    }
  }
}

/// The [[Locale]] state of `h` when it is an Intl.Locale instance.
fn locale_of_handle(st: Agent, h: Handle) -> Option(LocaleState) {
  case rt_store.t_cell_get(st, h) {
    SObject(kind: IntlObj(data: LocaleData(l), ..), ..) -> Some(l)
    _ -> None
  }
}

fn canonical_tag_or_throw(st: Agent, s: String) -> #(String, Agent) {
  case tags.canonicalize_tag(s) {
    Ok(tag) -> #(tag, st)
    Error(Nil) ->
      rt_val.t_throw_range_error(
        st,
        "Incorrect locale information provided: " <> s,
      )
  }
}

fn locale_list_from_object(st: Agent, h: Handle) -> #(List(String), Agent) {
  let o = mk_object(h)
  let #(len_v, st) = rt_obj.t_get_prop(st, o, StringKey(Named("length")))
  let #(len_n, st) = rt_val.t_to_number(st, len_v)
  let len = rt_val.jsnum_to_length(len_n)
  locale_list_loop(st, o, 0, len, [])
}

fn locale_list_loop(
  st: Agent,
  o: JsVal,
  k: Int,
  len: Int,
  seen: List(String),
) -> #(List(String), Agent) {
  case k >= len {
    True -> #(list.reverse(seen), st)
    False -> {
      let key = StringKey(Index(k))
      // §9.2.1 step 7.b: HasProperty(O, Pk) — the list may be a proxy, whose
      // `has` trap is user code and can throw.
      let #(has, st) = rt_obj.t_has_prop(st, o, key)
      case has {
        False -> locale_list_loop(st, o, k + 1, len, seen)
        True -> {
          let #(k_value, st) = rt_obj.t_get_prop(st, o, key)
          // Step 7.c.ii-iii: String or Object only; a Locale object
          // contributes its [[Locale]].
          let #(tag_str, st) = case classify(k_value) {
            KStr(s) -> #(s, st)
            KHandle(o) ->
              case locale_of_handle(st, o) {
                Some(l) -> #(l.locale, st)
                None -> rt_val.t_to_string(st, k_value)
              }
            _ ->
              rt_val.t_throw_type_error(
                st,
                "Locales item must be a string or object",
              )
          }
          let #(tag, st) = canonical_tag_or_throw(st, tag_str)
          let seen = case list.contains(seen, tag) {
            True -> seen
            False -> [tag, ..seen]
          }
          locale_list_loop(st, o, k + 1, len, seen)
        }
      }
    }
  }
}

// ============================================================================
// Intl.getCanonicalLocales
// ============================================================================

fn get_canonical_locales(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(tag_list, st) =
    canonicalize_locale_list(st, first_arg_or_undefined(args))
  alloc_array(st, list.map(tag_list, mk_string))
}

fn supported_values_of(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(key, st) = rt_val.t_to_string(st, first_arg_or_undefined(args))
  let values = case key {
    "calendar" -> Some(supported_calendars())
    "collation" -> Some(supported_collations())
    "currency" ->
      Some([
        "AUD", "BRL", "CAD", "CHF", "CNY", "EUR", "GBP", "INR", "JPY", "KRW",
        "MXN", "RUB", "SEK", "USD",
      ])
    "numberingSystem" -> Some(fmt.numbering_systems())
    "timeZone" -> Some(supported_time_zones())
    "unit" -> Some(fmt.sanctioned_units())
    _ -> None
  }
  case values {
    Some(vs) -> alloc_array(st, list.map(vs, mk_string))
    None -> rt_val.t_throw_range_error(st, "Invalid key : " <> key)
  }
}

/// Collation types we accept (sorted; excludes "standard"/"search").
fn supported_collations() -> List(String) {
  [
    "big5han", "compat", "dict", "direct", "ducet", "emoji", "eor", "gb2312",
    "phonebk", "phonetic", "pinyin", "reformed", "searchjl", "stroke", "trad",
    "unihan", "zhuyin",
  ]
}

/// Calendars required by ECMA-402 era/monthCode support (sorted).
fn supported_calendars() -> List(String) {
  [
    "buddhist", "chinese", "coptic", "dangi", "ethioaa", "ethiopic", "gregory",
    "hebrew", "indian", "islamic-civil", "islamic-tbla", "islamic-umalqura",
    "iso8601", "japanese", "persian", "roc",
  ]
}

/// Calendar identifiers DateTimeFormat accepts (resolved; formatting data is
/// Gregorian only).
fn valid_dtf_calendar(v: String) -> Bool {
  list.contains(supported_calendars(), v)
}

/// Primary time zone identifiers we recognise (sorted).
fn supported_time_zones() -> List(String) {
  list.sort(
    list.flatten([
      [
        "UTC", "Africa/Cairo", "Africa/Johannesburg", "America/Chicago",
        "America/Denver", "America/Los_Angeles", "America/New_York",
        "America/Sao_Paulo", "Asia/Dubai", "Asia/Hong_Kong", "Asia/Kolkata",
        "Asia/Shanghai", "Asia/Singapore", "Asia/Tokyo", "Asia/Seoul",
        "Australia/Sydney", "Europe/Berlin", "Europe/London", "Europe/Madrid",
        "Europe/Moscow", "Europe/Paris", "Europe/Rome", "Pacific/Auckland",
      ],
      int.range(12, 0, [], fn(acc, n) {
        ["Etc/GMT+" <> int.to_string(n), ..acc]
      }),
      int.range(14, 0, [], fn(acc, n) {
        ["Etc/GMT-" <> int.to_string(n), ..acc]
      }),
    ]),
    string.compare,
  )
}

// ============================================================================
// supportedLocalesOf (§9.2.8 LookupSupportedLocales)
// ============================================================================

fn supported_locales_of(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let locales = first_arg_or_undefined(args)
  let options_v = helpers.arg_at(args, 1)
  let #(requested, st) = canonicalize_locale_list(st, locales)
  let #(opts, st) = coerce_options(st, options_v)
  let #(_matcher, st) =
    get_str_opt(
      st,
      opts,
      "localeMatcher",
      ["lookup", "best fit"],
      Some("best fit"),
    )
  let supported =
    list.filter(requested, fn(tag) {
      tags.best_available_locale(tags.strip_extensions(tag)) != None
    })
  alloc_array(st, list.map(supported, mk_string))
}

// ============================================================================
// ResolveLocale (§9.2.7, lookup matcher against our available set)
// ============================================================================

/// Returns #(data_locale, extension_keywords). `extension_keywords` are the
/// u-extension key/values from the matched requested tag (valid values only
/// — callers overlay option values).
fn resolve_locale(
  requested: List(String),
) -> #(String, List(#(String, String))) {
  case requested {
    [] -> #(tags.default_locale(), [])
    [tag, ..rest] ->
      case tags.best_available_locale(tags.strip_extensions(tag)) {
        Some(available) -> #(available, u_keywords_of(tag))
        None -> resolve_locale(rest)
      }
  }
}

fn u_keywords_of(tag: String) -> List(#(String, String)) {
  tags.parse(tag)
  |> result.map(lid_u_keywords)
  |> result.unwrap([])
}

fn lid_u_keywords(lid: tags.LocaleId) -> List(#(String, String)) {
  lid.extensions
  |> list.filter_map(fn(ext) {
    case ext {
      tags.UExt(keywords:, ..) -> Ok(keywords)
      _ -> Error(Nil)
    }
  })
  |> list.flatten
}

/// Build the resolved [[Locale]] string: data locale + the u-keywords whose
/// value actually came from the locale extension (the `Bool` in each entry).
fn build_resolved_locale(
  data_locale: String,
  candidates: List(#(String, Bool, String)),
) -> String {
  let keywords =
    list.filter_map(candidates, fn(t) {
      case t {
        #(k, True, v) -> Ok(#(k, v))
        #(_, False, _) -> Error(Nil)
      }
    })
  case keywords {
    [] -> data_locale
    _ -> {
      let sorted = list.sort(keywords, fn(a, b) { string.compare(a.0, b.0) })
      let kw_str =
        sorted
        |> list.map(fn(kv) {
          case kv {
            #(k, "") | #(k, "true") -> k
            #(k, v) -> k <> "-" <> v
          }
        })
        |> string.join("-")
      data_locale <> "-u-" <> kw_str
    }
  }
}

// ============================================================================
// Constructors
// ============================================================================

fn service_name(service: IntlService) -> String {
  intl_data.service_name(service)
}

fn construct_service(
  st: Agent,
  service: ConstructibleService,
  proto: Handle,
  args: List(JsVal),
  new_target: JsVal,
) -> #(JsVal, Agent) {
  let callable_without_new = case service {
    CsCollator | CsNumberFormat | CsDateTimeFormat -> True
    _ -> False
  }
  case !callable_without_new && rt_val.is_undef(new_target) {
    True ->
      rt_val.t_throw_type_error(
        st,
        "Constructor Intl."
          <> service_name(intl_data.constructible_service(service))
          <> " requires 'new'",
      )
    False -> {
      // §10.1.13 OrdinaryCreateFromConstructor: resolve the prototype from
      // NewTarget so `class Sub extends Intl.X` instances get Sub.prototype.
      // The realm record has no Intl slots, so the intrinsic default (also
      // taken when called without `new`) is the constructor's own `proto`.
      let #(proto, st) =
        rt_call.get_prototype_from_constructor(st, new_target, fn(_realm) {
          proto
        })
      let arg0 = first_arg_or_undefined(args)
      let arg1 = helpers.arg_at(args, 1)
      let #(data, st) = case service {
        CsLocale -> {
          let #(s, st) = locale_state(st, arg0, arg1)
          #(LocaleData(s), st)
        }
        CsCollator -> {
          let #(s, st) = collator_state(st, arg0, arg1)
          #(CollatorData(s), st)
        }
        CsNumberFormat -> {
          let #(s, st) = number_format_state(st, arg0, arg1)
          #(NumberFormatData(s), st)
        }
        CsDateTimeFormat -> {
          let #(s, st) = date_time_format_state(st, arg0, arg1)
          #(DateTimeFormatData(s), st)
        }
        CsPluralRules -> {
          let #(s, st) = plural_rules_state(st, arg0, arg1)
          #(PluralRulesData(s), st)
        }
        CsListFormat -> {
          let #(s, st) = list_format_state(st, arg0, arg1)
          #(ListFormatData(s), st)
        }
        CsRelativeTimeFormat -> {
          let #(s, st) = rtf_state(st, arg0, arg1)
          #(RelativeTimeFormatData(s), st)
        }
        CsSegmenter -> {
          let #(s, st) = segmenter_state(st, arg0, arg1)
          #(SegmenterData(s), st)
        }
        CsDisplayNames -> {
          let #(s, st) = display_names_state(st, arg0, arg1)
          #(DisplayNamesData(s), st)
        }
        CsDurationFormat -> {
          let #(s, st) = duration_format_state(st, arg0, arg1)
          #(DurationFormatData(s), st)
        }
      }
      let #(h, st) =
        realm_ops.alloc_wrapper(st, IntlObj(data:, bound: None), proto)
      #(mk_object(h), st)
    }
  }
}

// --- Intl.Locale ---

fn locale_state(
  st: Agent,
  tag_v: JsVal,
  options_v: JsVal,
) -> #(LocaleState, Agent) {
  // Step 7-9: tag must be String or Object; Locale objects pass [[Locale]].
  let #(tag_str, st) = case classify(tag_v) {
    KStr(s) -> #(s, st)
    KHandle(h) ->
      case locale_of_handle(st, h) {
        Some(l) -> #(l.locale, st)
        None -> rt_val.t_to_string(st, tag_v)
      }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "Intl.Locale tag must be a string or object",
      )
  }
  let #(opts, st) = coerce_options(st, options_v)
  // ApplyOptionsToTag: structural validity first.
  let lid = case tags.parse(tag_str) {
    Ok(lid) -> lid
    Error(Nil) ->
      rt_val.t_throw_range_error(
        st,
        "Incorrect locale information provided: " <> tag_str,
      )
  }
  let #(language, st) = get_str_opt(st, opts, "language", [], None)
  let st = case language {
    Some(l) ->
      case tags.is_language(l) {
        True -> st
        False -> rt_val.t_throw_range_error(st, "Invalid language: " <> l)
      }
    None -> st
  }
  let #(script, st) = get_str_opt(st, opts, "script", [], None)
  let st = case script {
    Some(s) ->
      case tags.is_script(s) {
        True -> st
        False -> rt_val.t_throw_range_error(st, "Invalid script: " <> s)
      }
    None -> st
  }
  let #(region, st) = get_str_opt(st, opts, "region", [], None)
  let st = case region {
    Some(r) ->
      case tags.is_region(r) {
        True -> st
        False -> rt_val.t_throw_range_error(st, "Invalid region: " <> r)
      }
    None -> st
  }
  let #(variants_opt, st) = get_str_opt(st, opts, "variants", [], None)
  let variants_opt = case variants_opt {
    None -> None
    Some(v) -> {
      let lower = string.lowercase(v)
      let parts = string.split(lower, "-")
      let valid =
        parts != []
        && list.all(parts, tags.is_variant)
        && list.length(list.unique(parts)) == list.length(parts)
      case valid {
        True -> Some(parts)
        False -> rt_val.t_throw_range_error(st, "Invalid variants: " <> v)
      }
    }
  }
  let lid =
    tags.LocaleId(
      ..lid,
      language: option.map(language, string.lowercase)
        |> option.unwrap(lid.language),
      script: case script {
        Some(s) -> Some(string.lowercase(s))
        None -> lid.script
      },
      region: case region {
        Some(r) -> Some(string.lowercase(r))
        None -> lid.region
      },
      variants: option.unwrap(variants_opt, lid.variants),
    )
  // Keyword options (§14.1.2 steps 14-30).
  let #(calendar, st) = get_str_opt(st, opts, "calendar", [], None)
  let st = require_type_seq(st, calendar, "calendar")
  let #(collation, st) = get_str_opt(st, opts, "collation", [], None)
  let st = require_type_seq(st, collation, "collation")
  let #(hour_cycle, st) =
    get_str_opt(st, opts, "hourCycle", ["h11", "h12", "h23", "h24"], None)
  let #(case_first, st) =
    get_str_opt(st, opts, "caseFirst", ["upper", "lower", "false"], None)
  let #(numeric, st) = get_bool_opt(st, opts, "numeric", None)
  let #(first_day, st) = get_str_opt(st, opts, "firstDayOfWeek", [], None)
  let first_day = case first_day {
    None -> None
    Some(fd) ->
      case weekday_string(fd) {
        Some(v) -> Some(v)
        None -> rt_val.t_throw_range_error(st, "Invalid firstDayOfWeek: " <> fd)
      }
  }
  let #(numbering, st) = get_str_opt(st, opts, "numberingSystem", [], None)
  let st = require_type_seq(st, numbering, "numberingSystem")
  let new_kws =
    list.filter_map(
      [
        #("ca", calendar),
        #("co", collation),
        #("hc", hour_cycle),
        #("kf", case_first),
        #(
          "kn",
          option.map(numeric, fn(b) {
            case b {
              True -> "true"
              False -> "false"
            }
          }),
        ),
        #("nu", numbering),
        #("fw", first_day),
      ],
      fn(kv) {
        case kv {
          #(k, Some(v)) -> Ok(#(k, string.lowercase(v)))
          #(_, None) -> Error(Nil)
        }
      },
    )
  let lid = set_u_keywords(lid, new_kws)
  let canonical = tags.to_string(tags.canonicalize(lid))
  #(LocaleState(locale: canonical), st)
}

/// WeekdayToString (Intl.Locale firstDayOfWeek): "0"-"7" map to day codes;
/// any other well-formed value passes through (validated as a uvalue).
fn weekday_string(fd: String) -> Option(String) {
  case fd {
    "0" | "sun" -> Some("sun")
    "1" | "mon" -> Some("mon")
    "2" | "tue" -> Some("tue")
    "3" | "wed" -> Some("wed")
    "4" | "thu" -> Some("thu")
    "5" | "fri" -> Some("fri")
    "6" | "sat" -> Some("sat")
    "7" -> Some("sun")
    other ->
      case is_type_sequence(string.lowercase(other)) {
        True -> Some(string.lowercase(other))
        False -> None
      }
  }
}

fn require_type_seq(st: Agent, v: Option(String), name: String) -> Agent {
  case v {
    Some(s) ->
      case is_type_sequence(s) {
        True -> st
        False -> rt_val.t_throw_range_error(st, "Invalid " <> name <> ": " <> s)
      }
    None -> st
  }
}

/// Override/insert u-extension keywords on a parsed locale id.
fn set_u_keywords(
  lid: tags.LocaleId,
  new_kws: List(#(String, String)),
) -> tags.LocaleId {
  case new_kws {
    [] -> lid
    _ -> {
      let #(u_exts, others) =
        list.partition(lid.extensions, fn(e) {
          case e {
            tags.UExt(..) -> True
            _ -> False
          }
        })
      let #(attributes, existing) = case u_exts {
        [tags.UExt(attributes:, keywords:), ..] -> #(attributes, keywords)
        _ -> #([], [])
      }
      let merged =
        list.fold(new_kws, existing, fn(acc, kv) {
          list.key_set(acc, kv.0, kv.1)
        })
      tags.LocaleId(..lid, extensions: [
        tags.UExt(attributes:, keywords: merged),
        ..others
      ])
    }
  }
}

/// Extension keyword or option value for a relevant key. `parse` both
/// validates the (string) extension keyword and turns it into the option's own
/// type. Returns the resolved value plus whether the (valid) value came from
/// the locale's u-extension (those flow into the resolved locale string).
fn resolve_typed_keyword(
  ext_kws: List(#(String, String)),
  key: String,
  option_value: Option(a),
  parse: fn(String) -> Option(a),
  default: a,
) -> #(a, Bool) {
  // Bare keywords ("-u-kn") mean "true" (UTS 35).
  let from_ext = case list.key_find(ext_kws, key) {
    Ok("") -> parse("true")
    Ok(v) -> parse(v)
    Error(Nil) -> None
  }
  case option_value {
    // §9.2.7 ResolveLocale step 9.h.ii.2: an options value that matches
    // the requested extension keyword keeps the keyword in [[Locale]].
    Some(v) -> #(v, from_ext == Some(v))
    None ->
      case from_ext {
        Some(v) -> #(v, True)
        None -> #(default, False)
      }
  }
}

/// `resolve_typed_keyword` for the string-valued keys (nu): an option value
/// the host does not support falls back like an absent one.
fn resolve_keyword(
  ext_kws: List(#(String, String)),
  key: String,
  option_value: Option(String),
  valid: fn(String) -> Bool,
  default: String,
) -> #(String, Bool) {
  let parse = fn(v) {
    case valid(v) {
      True -> Some(v)
      False -> None
    }
  }
  resolve_typed_keyword(
    ext_kws,
    key,
    // An unsupported options value leaves the extension value in place.
    option.then(option_value, parse),
    parse,
    default,
  )
}

/// GetOption "localeMatcher" — validated then discarded (we only implement
/// one matcher), but the read is observable so it must happen in spec order.
fn read_locale_matcher(st: Agent, opts: Option(Handle)) -> Agent {
  let #(_matcher, st) =
    get_str_opt(
      st,
      opts,
      "localeMatcher",
      ["lookup", "best fit"],
      Some("best fit"),
    )
  st
}

/// Shared *_state constructor prologue: CanonicalizeLocaleList, options
/// coercion (GetOptionsObject when `strict`, else CoerceOptionsToObject),
/// then the localeMatcher read.
fn constructor_prologue(
  st: Agent,
  locales_v: JsVal,
  options_v: JsVal,
  strict strict: Bool,
) -> #(List(String), Option(Handle), Agent) {
  let #(requested, st) = canonicalize_locale_list(st, locales_v)
  let #(opts, st) = case strict {
    True -> strict_options(st, options_v)
    False -> coerce_options(st, options_v)
  }
  let st = read_locale_matcher(st, opts)
  #(requested, opts, st)
}

/// numberingSystem option read + ResolveLocale with only the "nu" relevant
/// extension keyword. Returns #(numbering_system, resolved_locale, st).
fn resolve_nu_locale(
  st: Agent,
  opts: Option(Handle),
  requested: List(String),
) -> #(String, String, Agent) {
  let #(nu_opt, st) = get_str_opt(st, opts, "numberingSystem", [], None)
  let st = require_type_seq(st, nu_opt, "numberingSystem")
  let #(data_locale, ext_kws) = resolve_locale(requested)
  let #(nu, nu_from_ext) =
    resolve_keyword(ext_kws, "nu", nu_opt, fmt.is_numbering_system, "latn")
  let locale = build_resolved_locale(data_locale, [#("nu", nu_from_ext, nu)])
  #(nu, locale, st)
}

// --- Intl.Collator ---

/// §10.1.2 InitializeCollator.
fn collator_state(
  st: Agent,
  locales_v: JsVal,
  options_v: JsVal,
) -> #(CollatorState, Agent) {
  let #(requested, st) = canonicalize_locale_list(st, locales_v)
  let #(opts, st) = coerce_options(st, options_v)
  let #(usage, st) =
    get_enum_opt(
      st,
      opts,
      "usage",
      [#("sort", UsageSort), #("search", UsageSearch)],
      UsageSort,
    )
  let st = read_locale_matcher(st, opts)
  let #(collation_opt, st) = get_str_opt(st, opts, "collation", [], None)
  let st = require_type_seq(st, collation_opt, "collation")
  let #(numeric_opt, st) = get_bool_opt(st, opts, "numeric", None)
  let #(case_first_opt, st) =
    get_enum_opt(
      st,
      opts,
      "caseFirst",
      [
        #("upper", Some(CaseFirstUpper)),
        #("lower", Some(CaseFirstLower)),
        #("false", Some(CaseFirstFalse)),
      ],
      None,
    )
  let #(data_locale, ext_kws) = resolve_locale(requested)
  let #(collation, co_from_ext) =
    resolve_keyword(
      ext_kws,
      "co",
      collation_opt,
      fn(v) { list.contains(supported_collations(), v) },
      "default",
    )
  let #(numeric_str, kn_from_ext) =
    resolve_keyword(
      ext_kws,
      "kn",
      option.map(numeric_opt, fn(b) {
        case b {
          True -> "true"
          False -> "false"
        }
      }),
      fn(v) { v == "true" || v == "false" },
      "false",
    )
  let numeric = numeric_str == "true"
  let #(case_first, kf_from_ext) =
    resolve_typed_keyword(
      ext_kws,
      "kf",
      case_first_opt,
      case_first_from_js_string,
      CaseFirstFalse,
    )
  let #(sensitivity, st) =
    get_enum_opt(
      st,
      opts,
      "sensitivity",
      [
        #("base", SensBase),
        #("accent", SensAccent),
        #("case", SensCase),
        #("variant", SensVariant),
      ],
      SensVariant,
    )
  let ignore_punct_default = string.starts_with(data_locale, "th")
  let #(ignore_punct, st) =
    get_bool_opt(st, opts, "ignorePunctuation", Some(ignore_punct_default))
  let locale =
    build_resolved_locale(data_locale, [
      #("co", co_from_ext, collation),
      #("kn", kn_from_ext, case numeric {
        True -> "true"
        False -> "false"
      }),
      #("kf", kf_from_ext, case_first_to_js_string(case_first)),
    ])
  #(
    CollatorState(
      locale:,
      usage:,
      sensitivity:,
      ignore_punctuation: option.unwrap(ignore_punct, False),
      collation:,
      numeric:,
      case_first:,
    ),
    st,
  )
}

// --- Intl.NumberFormat ---

/// §15.1.2 InitializeNumberFormat.
fn number_format_state(
  st: Agent,
  locales_v: JsVal,
  options_v: JsVal,
) -> #(NumberFormatState, Agent) {
  let #(requested, opts, st) =
    constructor_prologue(st, locales_v, options_v, strict: False)
  let #(nu, locale, st) = resolve_nu_locale(st, opts, requested)
  // SetNumberFormatUnitOptions (§15.1.3)
  let #(style, st) = read_unit_options(st, opts)
  let #(mnfd_default, mxfd_default) = case style {
    StyleCurrency(currency:, ..) -> {
      let d = fmt.currency_digits(currency)
      #(d, d)
    }
    StylePercent -> #(0, 0)
    StyleDecimal | StyleUnit(..) -> #(0, 3)
  }
  let #(notation_kind, st) =
    get_enum_opt(st, opts, "notation", notation_variants(), NkStandard)
  let #(digits, st) =
    digit_options(st, opts, mnfd_default, mxfd_default, notation_kind)
  let #(notation, st) = read_notation(st, opts, notation_kind)
  // useGrouping: boolean or "min2"/"auto"/"always" (§15.1.6
  // GetBooleanOrStringNumberFormatOption)
  let #(grouping_v, st) = opt_get(st, opts, "useGrouping")
  let #(use_grouping, st) = case classify(grouping_v) {
    KUndef -> #(
      case notation {
        NotationCompact(..) -> GroupingMin2
        NotationStandard | NotationScientific | NotationEngineering ->
          GroupingAuto
      },
      st,
    )
    KBool(False) -> #(GroupingNever, st)
    KBool(True) -> #(GroupingAlways, st)
    _ -> {
      let #(s, st) = rt_val.t_to_string(st, grouping_v)
      case s {
        "min2" -> #(GroupingMin2, st)
        "auto" -> #(GroupingAuto, st)
        "always" -> #(GroupingAlways, st)
        // Spec: any other string (including "true"/"false") is out of range.
        _ ->
          rt_val.t_throw_range_error(
            st,
            "Value " <> s <> " out of range for options property useGrouping",
          )
      }
    }
  }
  let #(sign_display, st) =
    get_enum_opt(
      st,
      opts,
      "signDisplay",
      [
        #("auto", SignAuto),
        #("never", SignNever),
        #("always", SignAlways),
        #("exceptZero", SignExceptZero),
        #("negative", SignNegative),
      ],
      SignAuto,
    )
  #(
    NumberFormatState(
      locale:,
      numbering_system: nu,
      style:,
      digits:,
      use_grouping:,
      notation:,
      sign_display:,
    ),
    st,
  )
}

/// The `style` option's tag, before its style-conditional slots are read.
type NumStyleKind {
  KDecimal
  KPercent
  KCurrency
  KUnit
}

/// `[[Style]]` after §15.1.3 steps 2-6: the currency style now carries its
/// validated, uppercased code, so it can never lose it further down.
type StyleWithCurrency {
  ScDecimal
  ScPercent
  ScCurrency(currency: String)
  ScUnit
}

/// SetNumberFormatUnitOptions (§15.1.3): reads `style` and its five
/// style-conditional options in spec order, and returns the one `NumStyle`
/// variant they select — a currency style always with its code, a unit style
/// always with its identifier.
fn read_unit_options(st: Agent, opts: Option(Handle)) -> #(NumStyle, Agent) {
  let #(kind, st) =
    get_enum_opt(
      st,
      opts,
      "style",
      [
        #("decimal", KDecimal),
        #("percent", KPercent),
        #("currency", KCurrency),
        #("unit", KUnit),
      ],
      KDecimal,
    )
  let #(currency, st) = get_str_opt(st, opts, "currency", [], None)
  let st = case currency {
    Some(c) ->
      case tags.is_alpha(c) && string.length(c) == 3 {
        True -> st
        False -> rt_val.t_throw_range_error(st, "Invalid currency code: " <> c)
      }
    None -> st
  }
  let sc = case kind, currency {
    KCurrency, Some(c) -> ScCurrency(currency: string.uppercase(c))
    KCurrency, None ->
      rt_val.t_throw_type_error(
        st,
        "Currency code is required with currency style",
      )
    KDecimal, _ -> ScDecimal
    KPercent, _ -> ScPercent
    KUnit, _ -> ScUnit
  }
  let #(currency_display, st) =
    get_enum_opt(
      st,
      opts,
      "currencyDisplay",
      [
        #("code", CurCode),
        #("symbol", CurSymbol),
        #("narrowSymbol", CurNarrowSymbol),
        #("name", CurName),
      ],
      CurSymbol,
    )
  let #(currency_sign, st) =
    get_enum_opt(
      st,
      opts,
      "currencySign",
      [#("standard", CurStandard), #("accounting", CurAccounting)],
      CurStandard,
    )
  let #(unit, st) = get_str_opt(st, opts, "unit", [], None)
  let st = case unit {
    Some(u) ->
      case fmt.is_well_formed_unit(u) {
        True -> st
        False ->
          rt_val.t_throw_range_error(
            st,
            "Invalid unit argument for option unit: " <> u,
          )
      }
    None -> st
  }
  // Everything but the unit style's `unitDisplay` (read last) is now known.
  let build = case sc, unit {
    ScDecimal, _ -> fn(_ud) { StyleDecimal }
    ScPercent, _ -> fn(_ud) { StylePercent }
    ScCurrency(currency:), _ -> fn(_ud) {
      StyleCurrency(currency:, display: currency_display, sign: currency_sign)
    }
    ScUnit, Some(u) -> fn(ud) { StyleUnit(unit: u, display: ud) }
    ScUnit, None ->
      rt_val.t_throw_type_error(st, "Unit is required with unit style")
  }
  let #(unit_display, st) =
    get_enum_opt(
      st,
      opts,
      "unitDisplay",
      [#("short", UnitShort), #("narrow", UnitNarrow), #("long", UnitLong)],
      UnitShort,
    )
  #(build(unit_display), st)
}

/// The `notation` option's tag, before `compactDisplay` completes it.
type NotationKind {
  NkStandard
  NkScientific
  NkEngineering
  NkCompact
}

/// The `notation` option's spellings.
fn notation_variants() -> List(#(String, NotationKind)) {
  [
    #("standard", NkStandard),
    #("scientific", NkScientific),
    #("engineering", NkEngineering),
    #("compact", NkCompact),
  ]
}

/// GetOption "compactDisplay", folded into the compact `Notation` variant.
fn read_notation(
  st: Agent,
  opts: Option(Handle),
  kind: NotationKind,
) -> #(Notation, Agent) {
  let #(compact_display, st) =
    get_enum_opt(
      st,
      opts,
      "compactDisplay",
      [#("short", CompactShort), #("long", CompactLong)],
      CompactShort,
    )
  #(
    case kind {
      NkStandard -> NotationStandard
      NkScientific -> NotationScientific
      NkEngineering -> NotationEngineering
      NkCompact -> NotationCompact(display: compact_display)
    },
    st,
  )
}

/// SetNumberFormatDigitOptions (§15.1.6) — resolves the digit-related slots.
fn digit_options(
  st: Agent,
  opts: Option(Handle),
  mnfd_default: Int,
  mxfd_default: Int,
  notation: NotationKind,
) -> #(IntlDigitOptions, Agent) {
  let #(mnid, st) =
    get_num_opt(st, opts, "minimumIntegerDigits", 1, 21, Some(1))
  let #(mnfd_v, st) = opt_get(st, opts, "minimumFractionDigits")
  let #(mxfd_v, st) = opt_get(st, opts, "maximumFractionDigits")
  let #(mnsd_v, st) = opt_get(st, opts, "minimumSignificantDigits")
  let #(mxsd_v, st) = opt_get(st, opts, "maximumSignificantDigits")
  let #(rounding_increment, st) =
    get_num_opt(st, opts, "roundingIncrement", 1, 5000, Some(1))
  let rounding_increment = option.unwrap(rounding_increment, 1)
  let st = case
    list.contains(
      [1, 2, 5, 10, 20, 25, 50, 100, 200, 250, 500, 1000, 2000, 2500, 5000],
      rounding_increment,
    )
  {
    True -> st
    False ->
      rt_val.t_throw_range_error(
        st,
        "roundingIncrement value is out of range: "
          <> int.to_string(rounding_increment),
      )
  }
  let #(rounding_mode, st) =
    get_enum_opt(
      st,
      opts,
      "roundingMode",
      [
        #("ceil", RoundCeil),
        #("floor", RoundFloor),
        #("expand", RoundExpand),
        #("trunc", RoundTrunc),
        #("halfCeil", RoundHalfCeil),
        #("halfFloor", RoundHalfFloor),
        #("halfExpand", RoundHalfExpand),
        #("halfTrunc", RoundHalfTrunc),
        #("halfEven", RoundHalfEven),
      ],
      RoundHalfExpand,
    )
  let #(rounding_priority, st) =
    get_enum_opt(
      st,
      opts,
      "roundingPriority",
      [
        #("auto", PriorityAuto),
        #("morePrecision", PriorityMorePrecision),
        #("lessPrecision", PriorityLessPrecision),
      ],
      PriorityAuto,
    )
  let #(trailing_zero, st) =
    get_enum_opt(
      st,
      opts,
      "trailingZeroDisplay",
      [#("auto", TzdAuto), #("stripIfInteger", TzdStripIfInteger)],
      TzdAuto,
    )
  let is_undef = rt_val.is_undef
  let has_sd = !is_undef(mnsd_v) || !is_undef(mxsd_v)
  let has_fd = !is_undef(mnfd_v) || !is_undef(mxfd_v)
  let need_sd = case rounding_priority {
    PriorityAuto -> has_sd
    PriorityMorePrecision | PriorityLessPrecision -> True
  }
  let need_fd = case rounding_priority {
    PriorityAuto -> !{ has_sd || { !has_fd && notation == NkCompact } }
    PriorityMorePrecision | PriorityLessPrecision -> True
  }
  // sig / fd are #(min, max) when that rounding kind is in effect.
  let #(sig, st) = case need_sd {
    False -> #(None, st)
    True ->
      case has_sd {
        True -> {
          let #(mnsd, st) =
            default_number_option(
              st,
              mnsd_v,
              1,
              21,
              Some(1),
              "minimumSignificantDigits",
            )
          let mnsd = option.unwrap(mnsd, 1)
          let #(mxsd, st) =
            default_number_option(
              st,
              mxsd_v,
              mnsd,
              21,
              Some(21),
              "maximumSignificantDigits",
            )
          let mxsd = option.unwrap(mxsd, 21)
          #(Some(#(mnsd, mxsd)), st)
        }
        False -> #(Some(#(1, 21)), st)
      }
  }
  let #(fd, st) = case need_fd {
    False -> #(None, st)
    True ->
      case has_fd {
        True -> {
          let #(mnfd, st) =
            default_number_option(
              st,
              mnfd_v,
              0,
              100,
              None,
              "minimumFractionDigits",
            )
          let #(mxfd, st) =
            default_number_option(
              st,
              mxfd_v,
              0,
              100,
              None,
              "maximumFractionDigits",
            )
          let #(mnfd, mxfd) = case mnfd, mxfd {
            Some(mn), Some(mx) ->
              case mn > mx {
                True ->
                  rt_val.t_throw_range_error(
                    st,
                    "minimumFractionDigits is greater than maximumFractionDigits",
                  )
                False -> #(mn, mx)
              }
            Some(mn), None -> #(mn, int.max(mxfd_default, mn))
            None, Some(mx) -> #(int.min(mnfd_default, mx), mx)
            None, None -> #(mnfd_default, mxfd_default)
          }
          #(Some(#(mnfd, mxfd)), st)
        }
        False -> #(
          Some(#(mnfd_default, int.max(mxfd_default, mnfd_default))),
          st,
        )
      }
  }
  // Neither kind requested (compact notation default): more-precision
  // rounding with mnfd/mxfd = 0 and mnsd/mxsd = 1..2 (§15.1.6 step 16).
  let #(sig, fd, rounding_priority) = case sig, fd {
    None, None -> #(Some(#(1, 2)), Some(#(0, 0)), PriorityMorePrecision)
    _, _ -> #(sig, fd, rounding_priority)
  }
  // roundingIncrement constraints (§15.1.6 steps 24-26).
  let st = case rounding_increment != 1 {
    False -> st
    True ->
      case need_sd || !need_fd {
        True ->
          rt_val.t_throw_type_error(
            st,
            "roundingIncrement requires fractionDigits rounding type",
          )
        False ->
          case fd {
            None -> st
            Some(#(mn, mx)) if mn == mx -> st
            Some(_) ->
              rt_val.t_throw_range_error(
                st,
                "roundingIncrement requires minimumFractionDigits equal to maximumFractionDigits",
              )
          }
      }
  }
  #(
    IntlDigitOptions(
      minimum_integer_digits: option.unwrap(mnid, 1),
      fraction_digits: fd,
      significant_digits: sig,
      rounding_increment:,
      rounding_mode:,
      rounding_priority:,
      trailing_zero_display: trailing_zero,
    ),
    st,
  )
}

// --- Intl.DateTimeFormat ---

fn date_time_format_state(
  st: Agent,
  locales_v: JsVal,
  options_v: JsVal,
) -> #(DateTimeFormatState, Agent) {
  dtf_state_required(st, locales_v, options_v, date_defaults(), DateAndTime)
}

/// Which group of date-time components a DateTimeFormat is required to
/// produce (ToDateTimeOptions' `required` argument, §17):
/// Date.prototype.toLocaleDateString needs date fields,
/// toLocaleTimeString needs time fields, everything else accepts either.
type DtfRequired {
  DateOnly
  TimeOnly
  DateAndTime
}

/// The date-group locale defaults (ToDateTimeOptions "date" defaults).
fn date_defaults() -> DtfComponents {
  DtfComponents(
    ..intl_data.empty_dtf_components,
    year: Some(WNumeric),
    month: Some(MonthNum(WNumeric)),
    day: Some(WNumeric),
  )
}

/// The time-group locale defaults (ToDateTimeOptions "time" defaults).
fn time_defaults() -> DtfComponents {
  DtfComponents(
    ..intl_data.empty_dtf_components,
    hour: Some(WNumeric),
    minute: Some(WNumeric),
    second: Some(WNumeric),
  )
}

/// resolvedOptions' view of a component option: what the user asked for, or —
/// with no dateStyle/timeStyle and no explicit component of the required
/// group — the locale default. With a style set, only the styles are visible.
fn public_component(
  user: Option(a),
  default: Option(a),
  styled: Bool,
  required_group_present: Bool,
) -> Option(a) {
  case user, styled || required_group_present {
    Some(_), _ -> user
    None, False -> default
    None, True -> None
  }
}

/// The name widths (weekday, era, dayPeriod) as GetOption variants.
fn name_width_variants() -> List(#(String, NameWidth)) {
  [#("narrow", WNarrow), #("short", WShort), #("long", WLong)]
}

/// The numeric widths (year, day, hour, minute, second) as GetOption variants.
fn numeric_width_variants() -> List(#(String, NumericWidth)) {
  [#("2-digit", WTwoDigit), #("numeric", WNumeric)]
}

/// CreateDateTimeFormat (§11.1.2). `defaults` are the locale default
/// components applied when the user requested no component of the `required`
/// group.
fn dtf_state_required(
  st: Agent,
  locales_v: JsVal,
  options_v: JsVal,
  defaults: DtfComponents,
  required: DtfRequired,
) -> #(DateTimeFormatState, Agent) {
  let #(requested, opts, st) =
    constructor_prologue(st, locales_v, options_v, strict: False)
  let #(calendar_opt, st) = get_str_opt(st, opts, "calendar", [], None)
  let st = require_type_seq(st, calendar_opt, "calendar")
  let #(nu_opt, st) = get_str_opt(st, opts, "numberingSystem", [], None)
  let st = require_type_seq(st, nu_opt, "numberingSystem")
  let #(hour12, st) = get_bool_opt(st, opts, "hour12", None)
  let #(hour_cycle_opt, st) =
    get_enum_opt(st, opts, "hourCycle", hour_cycle_variants(), None)
  // hour12 overrides hourCycle (§11.1.2 step 6).
  let hour_cycle_opt = case hour12 {
    Some(_) -> None
    None -> hour_cycle_opt
  }
  let #(data_locale, ext_kws) = resolve_locale(requested)
  let #(calendar, ca_from_ext) =
    resolve_keyword(
      ext_kws,
      "ca",
      option.map(calendar_opt, fn(v) {
        tags.canonical_u_value("ca", string.lowercase(v))
      }),
      valid_dtf_calendar,
      "gregory",
    )
  let #(nu, nu_from_ext) =
    resolve_keyword(ext_kws, "nu", nu_opt, fmt.is_numbering_system, "latn")
  let lang = case tags.parse(data_locale) {
    Ok(lid) -> lid.language
    Error(Nil) -> "en"
  }
  let hc_locale_default = case lang {
    "ja" -> H11
    _ -> H12
  }
  let #(hc, hc_from_ext) =
    resolve_typed_keyword(
      ext_kws,
      "hc",
      hour_cycle_opt,
      parse_hour_cycle,
      hc_locale_default,
    )
  // hour12 option resolution (hourCycle12 / hourCycle24 of the locale).
  let hc = case hour12 {
    Some(True) -> hc_locale_default
    Some(False) -> H23
    None -> hc
  }
  let locale =
    build_resolved_locale(data_locale, [
      #("ca", ca_from_ext, calendar),
      #("nu", nu_from_ext, nu),
      #("hc", hc_from_ext && hour12 == None, hour_cycle_to_js_string(hc)),
    ])
  // timeZone
  let #(tz_v, st) = opt_get(st, opts, "timeZone")
  let #(time_zone, st) = case classify(tz_v) {
    // DefaultTimeZone: the host environment zone. Its offset (like every
    // zone's) is resolved per formatted instant, not snapshotted here.
    KUndef -> #(HostZone(st.hooks.time_zone), st)
    _ -> {
      let #(s, st) = rt_val.t_to_string(st, tz_v)
      case tz.canonical(s) {
        Some(tz) -> #(tz, st)
        None ->
          rt_val.t_throw_range_error(st, "Invalid time zone specified: " <> s)
      }
    }
  }
  // Component options (table order).
  let #(weekday, st) =
    get_enum_opt(
      st,
      opts,
      "weekday",
      optional_variants(name_width_variants()),
      None,
    )
  let #(era, st) =
    get_enum_opt(
      st,
      opts,
      "era",
      optional_variants(name_width_variants()),
      None,
    )
  let #(year, st) =
    get_enum_opt(
      st,
      opts,
      "year",
      optional_variants(numeric_width_variants()),
      None,
    )
  let #(month, st) =
    get_enum_opt(
      st,
      opts,
      "month",
      optional_variants(month_width_variants()),
      None,
    )
  let #(day, st) =
    get_enum_opt(
      st,
      opts,
      "day",
      optional_variants(numeric_width_variants()),
      None,
    )
  let #(day_period, st) =
    get_enum_opt(
      st,
      opts,
      "dayPeriod",
      optional_variants(name_width_variants()),
      None,
    )
  let #(hour, st) =
    get_enum_opt(
      st,
      opts,
      "hour",
      optional_variants(numeric_width_variants()),
      None,
    )
  let #(minute, st) =
    get_enum_opt(
      st,
      opts,
      "minute",
      optional_variants(numeric_width_variants()),
      None,
    )
  let #(second, st) =
    get_enum_opt(
      st,
      opts,
      "second",
      optional_variants(numeric_width_variants()),
      None,
    )
  let #(fractional, st) =
    get_num_opt(st, opts, "fractionalSecondDigits", 1, 3, None)
  let #(tz_name_opt, st) =
    get_enum_opt(
      st,
      opts,
      "timeZoneName",
      optional_variants(tz_name_width_variants()),
      None,
    )
  let #(_format_matcher, st) =
    get_str_opt(
      st,
      opts,
      "formatMatcher",
      ["basic", "best fit"],
      Some("best fit"),
    )
  let #(date_style, st) =
    get_enum_opt(
      st,
      opts,
      "dateStyle",
      optional_variants([
        #("full", DsFull),
        #("long", DsLong),
        #("medium", DsMedium),
        #("short", DsShort),
      ]),
      None,
    )
  let #(time_style, st) =
    get_enum_opt(
      st,
      opts,
      "timeStyle",
      optional_variants([
        #("full", TsFull),
        #("long", TsLong),
        #("medium", TsMedium),
        #("short", TsShort),
      ]),
      None,
    )
  // The user's component options as a component table.
  let user =
    DtfComponents(
      weekday:,
      era:,
      year:,
      month:,
      day:,
      day_period:,
      hour:,
      minute:,
      second:,
      fractional_second_digits: fractional,
      time_zone_name: tz_name_opt,
    )
  // ECMA-402 §11.1.2 InitializeDateTimeFormat's `hasExplicitFormatComponents`:
  // ANY of the 11 Table-7 components (era and timeZoneName included). Used
  // ONLY for the dateStyle/timeStyle-with-components TypeError below — never
  // for defaulting.
  let explicit = list.any(dtf_component_order, fn(c) { has_component(user, c) })
  // ToDateTimeOptions' `needDefaults`: cleared only by a component of the
  // REQUIRED group — a narrower set than `explicit`, since era and
  // timeZoneName clear nothing. `new Intl.DateTimeFormat("en", {timeZoneName:
  // "short"})` must still default to numeric year/month/day.
  let date_group = [DtfWeekday, DtfYear, DtfMonth, DtfDay]
  let time_group = [
    DtfDayPeriod, DtfHour, DtfMinute, DtfSecond, DtfFractionalSecondDigits,
  ]
  let required_group_present = case required {
    DateOnly -> list.any(date_group, has_component(user, _))
    TimeOnly -> list.any(time_group, has_component(user, _))
    DateAndTime ->
      list.any(list.append(date_group, time_group), has_component(user, _))
  }
  let st = case required {
    DateOnly ->
      case time_style {
        Some(_) ->
          rt_val.t_throw_type_error(
            st,
            "timeStyle cannot be used with toLocaleDateString",
          )
        None -> st
      }
    TimeOnly ->
      case date_style {
        Some(_) ->
          rt_val.t_throw_type_error(
            st,
            "dateStyle cannot be used with toLocaleTimeString",
          )
        None -> st
      }
    DateAndTime -> st
  }
  let styled = option.is_some(date_style) || option.is_some(time_style)
  let st = case styled && explicit {
    True ->
      rt_val.t_throw_type_error(
        st,
        "Invalid option: dateStyle/timeStyle cannot be used with other date/time options",
      )
    False -> st
  }
  // Expand styles / apply defaults into the effective formatting components.
  let components = case styled, required_group_present {
    True, _ ->
      merge_components(
        date_style_components(date_style),
        time_style_components(time_style),
      )
    False, True -> user
    False, False -> merge_components(user, defaults)
  }
  let has_hour = option.is_some(components.hour)
  // The component options that were explicitly provided — needed at format
  // time to compute per-Temporal-type formats (GetDateTimeFormat with
  // inherit = ~relevant~).
  let explicit_names =
    list.filter(
      [
        DtfWeekday, DtfYear, DtfMonth, DtfDay, DtfDayPeriod, DtfHour, DtfMinute,
        DtfSecond, DtfFractionalSecondDigits,
      ],
      has_component(user, _),
    )
  #(
    DateTimeFormatState(
      locale:,
      calendar:,
      numbering_system: nu,
      time_zone:,
      hour_cycle: case has_hour {
        True -> Some(hc)
        False -> None
      },
      weekday: public_component(
        weekday,
        defaults.weekday,
        styled,
        required_group_present,
      ),
      era: public_component(era, defaults.era, styled, required_group_present),
      year: public_component(
        year,
        defaults.year,
        styled,
        required_group_present,
      ),
      month: public_component(
        month,
        defaults.month,
        styled,
        required_group_present,
      ),
      day: public_component(day, defaults.day, styled, required_group_present),
      day_period: public_component(
        day_period,
        defaults.day_period,
        styled,
        required_group_present,
      ),
      hour: public_component(
        hour,
        defaults.hour,
        styled,
        required_group_present,
      ),
      minute: public_component(
        minute,
        defaults.minute,
        styled,
        required_group_present,
      ),
      second: public_component(
        second,
        defaults.second,
        styled,
        required_group_present,
      ),
      fractional_second_digits: fractional,
      time_zone_name: public_component(
        tz_name_opt,
        defaults.time_zone_name,
        styled,
        required_group_present,
      ),
      date_style:,
      time_style:,
      explicit: explicit_names,
      components:,
    ),
    st,
  )
}

/// The `hourCycle` option's spellings.
fn hour_cycle_variants() -> List(#(String, Option(HourCycle))) {
  optional_variants([#("h11", H11), #("h12", H12), #("h23", H23), #("h24", H24)])
}

fn parse_hour_cycle(s: String) -> Option(HourCycle) {
  list.key_find(hour_cycle_variants(), s)
  |> option.from_result
  |> option.flatten
}

/// The `month` option's spellings.
fn month_width_variants() -> List(#(String, MonthWidth)) {
  [
    #("2-digit", MonthNum(WTwoDigit)),
    #("numeric", MonthNum(WNumeric)),
    #("narrow", MonthName(WNarrow)),
    #("short", MonthName(WShort)),
    #("long", MonthName(WLong)),
  ]
}

/// The `timeZoneName` option's spellings.
fn tz_name_width_variants() -> List(#(String, TimeZoneNameWidth)) {
  [
    #("short", TzShort),
    #("long", TzLong),
    #("shortOffset", TzShortOffset),
    #("longOffset", TzLongOffset),
    #("shortGeneric", TzShortGeneric),
    #("longGeneric", TzLongGeneric),
  ]
}

/// Every DateTimeFormat component, in §11.1.2 table order.
const dtf_component_order = [
  DtfWeekday,
  DtfEra,
  DtfYear,
  DtfMonth,
  DtfDay,
  DtfDayPeriod,
  DtfHour,
  DtfMinute,
  DtfSecond,
  DtfFractionalSecondDigits,
  DtfTimeZoneName,
]

fn date_style_components(style: Option(DateStyle)) -> DtfComponents {
  let base = intl_data.empty_dtf_components
  case style {
    Some(DsFull) ->
      DtfComponents(
        ..base,
        weekday: Some(WLong),
        year: Some(WNumeric),
        month: Some(MonthName(WLong)),
        day: Some(WNumeric),
      )
    Some(DsLong) ->
      DtfComponents(
        ..base,
        year: Some(WNumeric),
        month: Some(MonthName(WLong)),
        day: Some(WNumeric),
      )
    Some(DsMedium) ->
      DtfComponents(
        ..base,
        year: Some(WNumeric),
        month: Some(MonthName(WShort)),
        day: Some(WNumeric),
      )
    Some(DsShort) ->
      DtfComponents(
        ..base,
        year: Some(WTwoDigit),
        month: Some(MonthNum(WNumeric)),
        day: Some(WNumeric),
      )
    None -> base
  }
}

fn time_style_components(style: Option(TimeStyle)) -> DtfComponents {
  let base = intl_data.empty_dtf_components
  case style {
    Some(TsFull) ->
      DtfComponents(
        ..base,
        hour: Some(WNumeric),
        minute: Some(WTwoDigit),
        second: Some(WTwoDigit),
        time_zone_name: Some(TzLong),
      )
    Some(TsLong) ->
      DtfComponents(
        ..base,
        hour: Some(WNumeric),
        minute: Some(WTwoDigit),
        second: Some(WTwoDigit),
        time_zone_name: Some(TzShort),
      )
    Some(TsMedium) ->
      DtfComponents(
        ..base,
        hour: Some(WNumeric),
        minute: Some(WTwoDigit),
        second: Some(WTwoDigit),
      )
    Some(TsShort) ->
      DtfComponents(..base, hour: Some(WNumeric), minute: Some(WTwoDigit))
    None -> base
  }
}

// --- Intl.PluralRules ---

fn plural_rules_state(
  st: Agent,
  locales_v: JsVal,
  options_v: JsVal,
) -> #(PluralRulesState, Agent) {
  let #(requested, opts, st) =
    constructor_prologue(st, locales_v, options_v, strict: False)
  let #(type_, st) =
    get_enum_opt(
      st,
      opts,
      "type",
      [#("cardinal", Cardinal), #("ordinal", Ordinal)],
      Cardinal,
    )
  let #(notation_kind, st) =
    get_enum_opt(st, opts, "notation", notation_variants(), NkStandard)
  let #(notation, st) = read_notation(st, opts, notation_kind)
  let #(data_locale, _ext) = resolve_locale(requested)
  let #(digits, st) = digit_options(st, opts, 0, 3, notation_kind)
  #(
    PluralRulesState(
      locale: data_locale,
      plural_type: type_,
      notation:,
      digits:,
    ),
    st,
  )
}

// --- Intl.ListFormat ---

fn list_format_state(
  st: Agent,
  locales_v: JsVal,
  options_v: JsVal,
) -> #(ListFormatState, Agent) {
  let #(requested, opts, st) =
    constructor_prologue(st, locales_v, options_v, strict: True)
  let #(type_, st) =
    get_enum_opt(
      st,
      opts,
      "type",
      [
        #("conjunction", Conjunction),
        #("disjunction", Disjunction),
        #("unit", UnitList),
      ],
      Conjunction,
    )
  let #(style, st) =
    get_enum_opt(st, opts, "style", list_format_style_variants(), LLong)
  let #(data_locale, _ext) = resolve_locale(requested)
  #(ListFormatState(locale: data_locale, list_type: type_, style:), st)
}

fn list_format_style_variants() -> List(#(String, ListFormatStyle)) {
  [#("long", LLong), #("short", LShort), #("narrow", LNarrow)]
}

// --- Intl.RelativeTimeFormat ---

fn rtf_state(
  st: Agent,
  locales_v: JsVal,
  options_v: JsVal,
) -> #(RelativeTimeFormatState, Agent) {
  let #(requested, opts, st) =
    constructor_prologue(st, locales_v, options_v, strict: False)
  let #(nu, locale, st) = resolve_nu_locale(st, opts, requested)
  let #(style, st) =
    get_enum_opt(
      st,
      opts,
      "style",
      [#("long", RtfLong), #("short", RtfShort), #("narrow", RtfNarrow)],
      RtfLong,
    )
  let #(numeric, st) =
    get_enum_opt(
      st,
      opts,
      "numeric",
      [#("always", RtfAlways), #("auto", RtfAuto)],
      RtfAlways,
    )
  #(
    RelativeTimeFormatState(locale:, style:, numeric:, numbering_system: nu),
    st,
  )
}

// --- Intl.Segmenter ---

fn segmenter_state(
  st: Agent,
  locales_v: JsVal,
  options_v: JsVal,
) -> #(SegmenterState, Agent) {
  let #(requested, opts, st) =
    constructor_prologue(st, locales_v, options_v, strict: True)
  let #(granularity, st) =
    get_enum_opt(
      st,
      opts,
      "granularity",
      [#("grapheme", GGrapheme), #("word", GWord), #("sentence", GSentence)],
      GGrapheme,
    )
  let #(data_locale, _ext) = resolve_locale(requested)
  #(SegmenterState(locale: data_locale, granularity:), st)
}

// --- Intl.DisplayNames ---

fn display_names_state(
  st: Agent,
  locales_v: JsVal,
  options_v: JsVal,
) -> #(DisplayNamesState, Agent) {
  let #(requested, opts, st) =
    constructor_prologue(st, locales_v, options_v, strict: True)
  let #(style, st) =
    get_enum_opt(
      st,
      opts,
      "style",
      [#("narrow", WNarrow), #("short", WShort), #("long", WLong)],
      WLong,
    )
  let #(type_, st) =
    get_enum_opt(
      st,
      opts,
      "type",
      [
        #("language", Some(DnLanguage)),
        #("region", Some(DnRegion)),
        #("script", Some(DnScript)),
        #("currency", Some(DnCurrency)),
        #("calendar", Some(DnCalendar)),
        #("dateTimeField", Some(DnDateTimeField)),
      ],
      None,
    )
  let type_ = case type_ {
    Some(t) -> t
    None ->
      rt_val.t_throw_type_error(
        st,
        "Intl.DisplayNames constructor requires type option",
      )
  }
  let #(fallback, st) =
    get_enum_opt(
      st,
      opts,
      "fallback",
      [#("code", FbCode), #("none", FbNone)],
      FbCode,
    )
  let #(language_display, st) =
    get_enum_opt(
      st,
      opts,
      "languageDisplay",
      [#("dialect", LdDialect), #("standard", LdStandard)],
      LdDialect,
    )
  let #(data_locale, _ext) = resolve_locale(requested)
  #(
    DisplayNamesState(
      locale: data_locale,
      style:,
      display_type: type_,
      fallback:,
      language_display: case type_ {
        DnLanguage -> Some(language_display)
        DnRegion | DnScript | DnCurrency | DnCalendar | DnDateTimeField -> None
      },
    ),
    st,
  )
}

// --- Intl.DurationFormat ---

/// A duration unit. The one place the ten unit names are written down: option
/// key, DurationRecord field, and singular unit tag all derive from it.
type DurationUnit {
  DuYears
  DuMonths
  DuWeeks
  DuDays
  DuHours
  DuMinutes
  DuSeconds
  DuMilliseconds
  DuMicroseconds
  DuNanoseconds
}

/// The duration units in canonical (largest-first) spec order.
const duration_units = [
  DuYears,
  DuMonths,
  DuWeeks,
  DuDays,
  DuHours,
  DuMinutes,
  DuSeconds,
  DuMilliseconds,
  DuMicroseconds,
  DuNanoseconds,
]

/// The unit's JS property name (`Duration` field, DurationFormat option key).
fn duration_unit_js_name(u: DurationUnit) -> String {
  case u {
    DuYears -> "years"
    DuMonths -> "months"
    DuWeeks -> "weeks"
    DuDays -> "days"
    DuHours -> "hours"
    DuMinutes -> "minutes"
    DuSeconds -> "seconds"
    DuMilliseconds -> "milliseconds"
    DuMicroseconds -> "microseconds"
    DuNanoseconds -> "nanoseconds"
  }
}

/// The unit's singular NumberFormat unit identifier / part `unit` tag.
fn duration_unit_singular(u: DurationUnit) -> String {
  case u {
    DuYears -> "year"
    DuMonths -> "month"
    DuWeeks -> "week"
    DuDays -> "day"
    DuHours -> "hour"
    DuMinutes -> "minute"
    DuSeconds -> "second"
    DuMilliseconds -> "millisecond"
    DuMicroseconds -> "microsecond"
    DuNanoseconds -> "nanosecond"
  }
}

/// A duration's ten fields (ToDurationRecord / ParseTemporalDurationString).
type DurationRecord {
  DurationRecord(
    years: Float,
    months: Float,
    weeks: Float,
    days: Float,
    hours: Float,
    minutes: Float,
    seconds: Float,
    milliseconds: Float,
    microseconds: Float,
    nanoseconds: Float,
  )
}

const zero_duration = DurationRecord(
  years: 0.0,
  months: 0.0,
  weeks: 0.0,
  days: 0.0,
  hours: 0.0,
  minutes: 0.0,
  seconds: 0.0,
  milliseconds: 0.0,
  microseconds: 0.0,
  nanoseconds: 0.0,
)

fn duration_field(d: DurationRecord, u: DurationUnit) -> Float {
  case u {
    DuYears -> d.years
    DuMonths -> d.months
    DuWeeks -> d.weeks
    DuDays -> d.days
    DuHours -> d.hours
    DuMinutes -> d.minutes
    DuSeconds -> d.seconds
    DuMilliseconds -> d.milliseconds
    DuMicroseconds -> d.microseconds
    DuNanoseconds -> d.nanoseconds
  }
}

fn set_duration_field(
  d: DurationRecord,
  u: DurationUnit,
  v: Float,
) -> DurationRecord {
  case u {
    DuYears -> DurationRecord(..d, years: v)
    DuMonths -> DurationRecord(..d, months: v)
    DuWeeks -> DurationRecord(..d, weeks: v)
    DuDays -> DurationRecord(..d, days: v)
    DuHours -> DurationRecord(..d, hours: v)
    DuMinutes -> DurationRecord(..d, minutes: v)
    DuSeconds -> DurationRecord(..d, seconds: v)
    DuMilliseconds -> DurationRecord(..d, milliseconds: v)
    DuMicroseconds -> DurationRecord(..d, microseconds: v)
    DuNanoseconds -> DurationRecord(..d, nanoseconds: v)
  }
}

fn duration_values(d: DurationRecord) -> List(Float) {
  list.map(duration_units, duration_field(d, _))
}

/// Intl.DurationFormat §1.2.1 InitializeDurationFormat.
fn duration_format_state(
  st: Agent,
  locales_v: JsVal,
  options_v: JsVal,
) -> #(DurationFormatState, Agent) {
  let #(requested, opts, st) =
    constructor_prologue(st, locales_v, options_v, strict: True)
  let #(nu, locale, st) = resolve_nu_locale(st, opts, requested)
  let #(base_style, st) =
    get_enum_opt(
      st,
      opts,
      "style",
      [
        #("long", BsLong),
        #("short", BsShort),
        #("narrow", BsNarrow),
        #("digital", BsDigital),
      ],
      BsShort,
    )
  // GetDurationUnitOptions for each unit in spec order, threading the
  // previous unit's INTERNAL style (`DurFractional` possible) — numeric
  // chaining and sub-second fraction folding depend on it.
  let unit = fn(st, name, prev) {
    duration_unit_options(st, opts, base_style, name, prev)
  }
  let #(years, prev, st) = unit(st, DuYears, None)
  let #(months, prev, st) = unit(st, DuMonths, Some(prev))
  let #(weeks, prev, st) = unit(st, DuWeeks, Some(prev))
  let #(days, prev, st) = unit(st, DuDays, Some(prev))
  let #(hours, prev, st) = unit(st, DuHours, Some(prev))
  let #(minutes, prev, st) = unit(st, DuMinutes, Some(prev))
  let #(seconds, prev, st) = unit(st, DuSeconds, Some(prev))
  let #(milliseconds, prev, st) = unit(st, DuMilliseconds, Some(prev))
  let #(microseconds, prev, st) = unit(st, DuMicroseconds, Some(prev))
  let #(nanoseconds, _prev, st) = unit(st, DuNanoseconds, Some(prev))
  let #(fractional, st) = get_num_opt(st, opts, "fractionalDigits", 0, 9, None)
  #(
    DurationFormatState(
      locale:,
      numbering_system: nu,
      style: base_style,
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
      fractional_digits: fractional,
    ),
    st,
  )
}

/// The `[[<Unit>Style]]` values a unit's option accepts (its stylesList).
fn duration_style_variants(
  u: DurationUnit,
) -> List(#(String, DurationUnitStyle)) {
  let base = [#("long", DurLong), #("short", DurShort), #("narrow", DurNarrow)]
  case u {
    DuHours | DuMinutes | DuSeconds ->
      list.append(base, [#("numeric", DurNumeric), #("2-digit", DurTwoDigit)])
    DuMilliseconds | DuMicroseconds | DuNanoseconds ->
      list.append(base, [#("numeric", DurNumeric)])
    DuYears | DuMonths | DuWeeks | DuDays -> base
  }
}

/// Whether a style makes the *next* unit chain onto it numerically.
fn is_numeric_style(s: DurationUnitStyle) -> Bool {
  case s {
    DurNumeric | DurTwoDigit | DurFractional -> True
    DurLong | DurShort | DurNarrow -> False
  }
}

fn is_sub_second(u: DurationUnit) -> Bool {
  case u {
    DuMilliseconds | DuMicroseconds | DuNanoseconds -> True
    DuYears | DuMonths | DuWeeks | DuDays | DuHours | DuMinutes | DuSeconds ->
      False
  }
}

fn optional_variants(
  variants: List(#(String, a)),
) -> List(#(String, Option(a))) {
  list.map(variants, fn(kv) { #(kv.0, Some(kv.1)) })
}

/// GetDurationUnitOptions (Intl.DurationFormat §1.1.6): one unit's resolved
/// style/display, plus the internal style to thread into the next unit.
fn duration_unit_options(
  st: Agent,
  opts: Option(Handle),
  base_style: DurationBaseStyle,
  unit: DurationUnit,
  prev_style: Option(DurationUnitStyle),
) -> #(DurationUnitOptions, DurationUnitStyle, Agent) {
  let name = duration_unit_js_name(unit)
  let #(style_opt, st) =
    get_enum_opt(
      st,
      opts,
      name,
      optional_variants(duration_style_variants(unit)),
      None,
    )
  let sub_second = is_sub_second(unit)
  let prev_numeric = case prev_style {
    Some(s) -> is_numeric_style(s)
    None -> False
  }
  let two_digit_unit = case unit {
    DuMinutes | DuSeconds -> True
    _ -> False
  }
  // Steps 2-3: default the style from baseStyle / the previous unit's style.
  let #(style, display_default) = case style_opt {
    Some(chosen) -> #(chosen, DisplayAlways)
    None ->
      case base_style {
        // digitalBase: "short" for the calendar units, "numeric" for the rest.
        BsDigital ->
          case unit {
            DuYears | DuMonths | DuWeeks | DuDays -> #(DurShort, DisplayAuto)
            _ -> #(DurNumeric, DisplayAlways)
          }
        BsLong | BsShort | BsNarrow ->
          case prev_numeric {
            True ->
              case two_digit_unit {
                True -> #(DurNumeric, DisplayAlways)
                False -> #(DurNumeric, DisplayAuto)
              }
            False -> #(duration_base_unit_style(base_style), DisplayAuto)
          }
      }
  }
  // Step 4: a numeric sub-second unit always folds into a fraction.
  let #(style, display_default) = case style == DurNumeric && sub_second {
    True -> #(DurFractional, DisplayAuto)
    False -> #(style, display_default)
  }
  let #(display, st) =
    get_enum_opt(
      st,
      opts,
      name <> "Display",
      [#("auto", DisplayAuto), #("always", DisplayAlways)],
      display_default,
    )
  // Step 7.
  let st = case display == DisplayAlways && style == DurFractional {
    True ->
      rt_val.t_throw_range_error(
        st,
        name <> "Display cannot be 'always' for fractional units",
      )
    False -> st
  }
  // Steps 8-9.
  let style = case prev_style {
    Some(DurFractional) ->
      case style {
        DurFractional -> style
        _ ->
          rt_val.t_throw_range_error(
            st,
            name <> " style must be fractional after a fractional unit",
          )
      }
    Some(DurNumeric) | Some(DurTwoDigit) ->
      case style {
        DurFractional | DurNumeric | DurTwoDigit ->
          // Step 9.b: minutes/seconds after a numeric unit are zero-padded.
          case two_digit_unit {
            True -> DurTwoDigit
            False -> style
          }
        _ ->
          rt_val.t_throw_range_error(
            st,
            name <> " style cannot be mixed with numeric styles",
          )
      }
    Some(DurLong) | Some(DurShort) | Some(DurNarrow) | None -> style
  }
  #(DurationUnitOptions(style:, display:), style, st)
}

/// The base style as a per-unit style — `digital` has none of its own (each
/// unit takes its digitalBase instead), so it is not accepted here.
fn duration_base_unit_style(base: DurationBaseStyle) -> DurationUnitStyle {
  case base {
    BsLong -> DurLong
    BsShort | BsDigital -> DurShort
    BsNarrow -> DurNarrow
  }
}

/// The ListFormat style the assembled duration groups are joined with —
/// `digital` has no list style of its own and joins like `short`.
fn duration_list_style(base: DurationBaseStyle) -> intl_data.ListFormatStyle {
  case base {
    BsDigital | BsShort -> LShort
    BsLong -> LLong
    BsNarrow -> LNarrow
  }
}

/// The DurationFormat per-unit options paired with their unit, in canonical
/// spec order.
fn duration_unit_list(
  d: DurationFormatState,
) -> List(#(DurationUnit, DurationUnitOptions)) {
  [
    #(DuYears, d.years),
    #(DuMonths, d.months),
    #(DuWeeks, d.weeks),
    #(DuDays, d.days),
    #(DuHours, d.hours),
    #(DuMinutes, d.minutes),
    #(DuSeconds, d.seconds),
    #(DuMilliseconds, d.milliseconds),
    #(DuMicroseconds, d.microseconds),
    #(DuNanoseconds, d.nanoseconds),
  ]
}

// ============================================================================
// resolvedOptions
// ============================================================================

// The `*_to_js_string` renderers below produce the ECMA-402 spec spelling of
// each closed option enum for `resolvedOptions()`. The enums themselves live
// in `rt/intl_data`; the spellings live here alongside their sole consumer.

fn collator_usage_to_js_string(v: CollatorUsage) -> String {
  case v {
    UsageSort -> "sort"
    UsageSearch -> "search"
  }
}

fn collator_sensitivity_to_js_string(v: CollatorSensitivity) -> String {
  case v {
    SensBase -> "base"
    SensAccent -> "accent"
    SensCase -> "case"
    SensVariant -> "variant"
  }
}

fn case_first_to_js_string(v: CaseFirst) -> String {
  case v {
    CaseFirstUpper -> "upper"
    CaseFirstLower -> "lower"
    CaseFirstFalse -> "false"
  }
}

/// The `kf` u-extension / `caseFirst` option spellings.
fn case_first_from_js_string(s: String) -> Option(CaseFirst) {
  case s {
    "upper" -> Some(CaseFirstUpper)
    "lower" -> Some(CaseFirstLower)
    "false" -> Some(CaseFirstFalse)
    _ -> None
  }
}

fn num_style_to_js_string(v: NumStyle) -> String {
  case v {
    StyleDecimal -> "decimal"
    StylePercent -> "percent"
    StyleCurrency(..) -> "currency"
    StyleUnit(..) -> "unit"
  }
}

fn notation_to_js_string(v: Notation) -> String {
  case v {
    NotationStandard -> "standard"
    NotationScientific -> "scientific"
    NotationEngineering -> "engineering"
    NotationCompact(..) -> "compact"
  }
}

fn compact_display_to_js_string(v: CompactDisplay) -> String {
  case v {
    CompactShort -> "short"
    CompactLong -> "long"
  }
}

fn sign_display_to_js_string(v: SignDisplay) -> String {
  case v {
    SignAuto -> "auto"
    SignNever -> "never"
    SignAlways -> "always"
    SignExceptZero -> "exceptZero"
    SignNegative -> "negative"
  }
}

fn currency_display_to_js_string(v: CurrencyDisplay) -> String {
  case v {
    CurCode -> "code"
    CurSymbol -> "symbol"
    CurNarrowSymbol -> "narrowSymbol"
    CurName -> "name"
  }
}

fn currency_sign_to_js_string(v: CurrencySign) -> String {
  case v {
    CurStandard -> "standard"
    CurAccounting -> "accounting"
  }
}

fn unit_display_to_js_string(v: UnitDisplay) -> String {
  case v {
    UnitShort -> "short"
    UnitNarrow -> "narrow"
    UnitLong -> "long"
  }
}

fn rounding_mode_to_js_string(v: RoundingMode) -> String {
  case v {
    RoundCeil -> "ceil"
    RoundFloor -> "floor"
    RoundExpand -> "expand"
    RoundTrunc -> "trunc"
    RoundHalfCeil -> "halfCeil"
    RoundHalfFloor -> "halfFloor"
    RoundHalfExpand -> "halfExpand"
    RoundHalfTrunc -> "halfTrunc"
    RoundHalfEven -> "halfEven"
  }
}

fn rounding_priority_to_js_string(v: RoundingPriority) -> String {
  case v {
    PriorityAuto -> "auto"
    PriorityMorePrecision -> "morePrecision"
    PriorityLessPrecision -> "lessPrecision"
  }
}

fn trailing_zero_display_to_js_string(v: TrailingZeroDisplay) -> String {
  case v {
    TzdAuto -> "auto"
    TzdStripIfInteger -> "stripIfInteger"
  }
}

fn numeric_width_to_js_string(v: NumericWidth) -> String {
  case v {
    WNumeric -> "numeric"
    WTwoDigit -> "2-digit"
  }
}

fn name_width_to_js_string(v: NameWidth) -> String {
  case v {
    WLong -> "long"
    WShort -> "short"
    WNarrow -> "narrow"
  }
}

fn month_width_to_js_string(v: MonthWidth) -> String {
  case v {
    MonthNum(w) -> numeric_width_to_js_string(w)
    MonthName(w) -> name_width_to_js_string(w)
  }
}

fn time_zone_name_width_to_js_string(v: TimeZoneNameWidth) -> String {
  case v {
    TzShort -> "short"
    TzLong -> "long"
    TzShortOffset -> "shortOffset"
    TzLongOffset -> "longOffset"
    TzShortGeneric -> "shortGeneric"
    TzLongGeneric -> "longGeneric"
  }
}

fn hour_cycle_to_js_string(v: HourCycle) -> String {
  case v {
    H11 -> "h11"
    H12 -> "h12"
    H23 -> "h23"
    H24 -> "h24"
  }
}

fn date_style_to_js_string(v: DateStyle) -> String {
  case v {
    DsFull -> "full"
    DsLong -> "long"
    DsMedium -> "medium"
    DsShort -> "short"
  }
}

fn time_style_to_js_string(v: TimeStyle) -> String {
  case v {
    TsFull -> "full"
    TsLong -> "long"
    TsMedium -> "medium"
    TsShort -> "short"
  }
}

fn plural_type_to_js_string(v: PluralType) -> String {
  case v {
    Cardinal -> "cardinal"
    Ordinal -> "ordinal"
  }
}

fn list_format_type_to_js_string(v: ListFormatType) -> String {
  case v {
    Conjunction -> "conjunction"
    Disjunction -> "disjunction"
    UnitList -> "unit"
  }
}

fn list_format_style_to_js_string(v: ListFormatStyle) -> String {
  case v {
    LLong -> "long"
    LShort -> "short"
    LNarrow -> "narrow"
  }
}

fn rtf_style_to_js_string(v: RtfStyle) -> String {
  case v {
    RtfLong -> "long"
    RtfShort -> "short"
    RtfNarrow -> "narrow"
  }
}

fn rtf_numeric_to_js_string(v: RtfNumeric) -> String {
  case v {
    RtfAlways -> "always"
    RtfAuto -> "auto"
  }
}

fn granularity_to_js_string(v: Granularity) -> String {
  case v {
    GGrapheme -> "grapheme"
    GWord -> "word"
    GSentence -> "sentence"
  }
}

fn display_names_type_to_js_string(v: DisplayNamesType) -> String {
  case v {
    DnLanguage -> "language"
    DnRegion -> "region"
    DnScript -> "script"
    DnCurrency -> "currency"
    DnCalendar -> "calendar"
    DnDateTimeField -> "dateTimeField"
  }
}

fn display_names_fallback_to_js_string(v: DisplayNamesFallback) -> String {
  case v {
    FbCode -> "code"
    FbNone -> "none"
  }
}

fn language_display_to_js_string(v: LanguageDisplay) -> String {
  case v {
    LdDialect -> "dialect"
    LdStandard -> "standard"
  }
}

/// The resolvedOptions spelling of a `[[<Unit>Style]]`. `DurFractional` is
/// internal-only and surfaces as "numeric".
fn duration_unit_style_to_js_string(v: DurationUnitStyle) -> String {
  case v {
    DurLong -> "long"
    DurShort -> "short"
    DurNarrow -> "narrow"
    DurNumeric -> "numeric"
    DurTwoDigit -> "2-digit"
    DurFractional -> "numeric"
  }
}

fn duration_display_to_js_string(v: DurationDisplay) -> String {
  case v {
    DisplayAuto -> "auto"
    DisplayAlways -> "always"
  }
}

fn duration_base_style_to_js_string(v: DurationBaseStyle) -> String {
  case v {
    BsLong -> "long"
    BsShort -> "short"
    BsNarrow -> "narrow"
    BsDigital -> "digital"
  }
}

/// `#(name, Some(value))` pairs, dropping the absent ones (order preserved).
fn present_pairs(pairs: List(#(k, Option(a)))) -> List(#(k, a)) {
  list.filter_map(pairs, fn(p) {
    case p.1 {
      Some(v) -> Ok(#(p.0, v))
      None -> Error(Nil)
    }
  })
}

fn resolved_options(
  st: Agent,
  service: IntlService,
  this: JsVal,
) -> #(JsVal, Agent) {
  let #(_h, data, _bound) =
    branded(
      st,
      this,
      service,
      "Intl." <> service_name(service) <> ".prototype.resolvedOptions",
    )
  // Property order and presence are observable: each arm enumerates its
  // state's fields in the spec's resolvedOptions order, skipping absent
  // (None) internal slots.
  let #(props, st) = case data {
    LocaleData(l) -> #([#("locale", mk_string(l.locale))], st)
    CollatorData(c) -> #(
      [
        #("locale", mk_string(c.locale)),
        #("usage", mk_string(collator_usage_to_js_string(c.usage))),
        #(
          "sensitivity",
          mk_string(collator_sensitivity_to_js_string(c.sensitivity)),
        ),
        #("ignorePunctuation", mk_bool(c.ignore_punctuation)),
        #("collation", mk_string(c.collation)),
        #("numeric", mk_bool(c.numeric)),
        #("caseFirst", mk_string(case_first_to_js_string(c.case_first))),
      ],
      st,
    )
    NumberFormatData(nf) -> {
      let dg = nf.digits
      // The style-conditional slots exist exactly for the style that
      // selects them, so resolvedOptions omits the rest.
      let #(currency, currency_display, currency_sign) = case nf.style {
        StyleCurrency(currency:, display:, sign:) -> #(
          Some(mk_string(currency)),
          Some(mk_string(currency_display_to_js_string(display))),
          Some(mk_string(currency_sign_to_js_string(sign))),
        )
        StyleDecimal | StylePercent | StyleUnit(..) -> #(None, None, None)
      }
      let #(unit, unit_display) = case nf.style {
        StyleUnit(unit:, display:) -> #(
          Some(mk_string(unit)),
          Some(mk_string(unit_display_to_js_string(display))),
        )
        StyleDecimal | StylePercent | StyleCurrency(..) -> #(None, None)
      }
      #(
        present_pairs([
          #("locale", Some(mk_string(nf.locale))),
          #("numberingSystem", Some(mk_string(nf.numbering_system))),
          #("style", Some(mk_string(num_style_to_js_string(nf.style)))),
          #("currency", currency),
          #("currencyDisplay", currency_display),
          #("currencySign", currency_sign),
          #("unit", unit),
          #("unitDisplay", unit_display),
          ..digit_option_pairs(dg, [
            #("useGrouping", Some(use_grouping_js(nf.use_grouping))),
            #("notation", Some(mk_string(notation_to_js_string(nf.notation)))),
            #("compactDisplay", compact_display_of(nf.notation)),
            #(
              "signDisplay",
              Some(mk_string(sign_display_to_js_string(nf.sign_display))),
            ),
            ..digit_rounding_pairs(dg)
          ])
        ]),
        st,
      )
    }
    DateTimeFormatData(d) -> #(
      present_pairs([
        #("locale", Some(mk_string(d.locale))),
        #("calendar", Some(mk_string(d.calendar))),
        #("numberingSystem", Some(mk_string(d.numbering_system))),
        #("timeZone", Some(mk_string(intl_data.dtf_time_zone_id(d.time_zone)))),
        #(
          "hourCycle",
          option.map(d.hour_cycle, fn(hc) {
            mk_string(hour_cycle_to_js_string(hc))
          }),
        ),
        #(
          "hour12",
          option.map(d.hour_cycle, fn(hc) {
            mk_bool(case hc {
              H11 | H12 -> True
              H23 | H24 -> False
            })
          }),
        ),
        #("weekday", option.map(d.weekday, name_width_js)),
        #("era", option.map(d.era, name_width_js)),
        #("year", option.map(d.year, numeric_width_js)),
        #("month", option.map(d.month, month_width_js)),
        #("day", option.map(d.day, numeric_width_js)),
        #("dayPeriod", option.map(d.day_period, name_width_js)),
        #("hour", option.map(d.hour, numeric_width_js)),
        #("minute", option.map(d.minute, numeric_width_js)),
        #("second", option.map(d.second, numeric_width_js)),
        #(
          "fractionalSecondDigits",
          option.map(d.fractional_second_digits, fn(n) { mk_number(JInt(n)) }),
        ),
        #("timeZoneName", option.map(d.time_zone_name, tz_name_width_js)),
        #(
          "dateStyle",
          option.map(d.date_style, fn(s) {
            mk_string(date_style_to_js_string(s))
          }),
        ),
        #(
          "timeStyle",
          option.map(d.time_style, fn(s) {
            mk_string(time_style_to_js_string(s))
          }),
        ),
      ]),
      st,
    )
    PluralRulesData(p) -> {
      let dg = p.digits
      let #(cats, st) =
        alloc_array(
          st,
          fmt.plural_categories_en(p.plural_type)
            |> list.map(fmt.plural_category_to_js_string)
            |> list.map(mk_string),
        )
      #(
        present_pairs([
          #("locale", Some(mk_string(p.locale))),
          #("type", Some(mk_string(plural_type_to_js_string(p.plural_type)))),
          #("notation", Some(mk_string(notation_to_js_string(p.notation)))),
          #("compactDisplay", compact_display_of(p.notation)),
          ..digit_option_pairs(dg, [
            #("pluralCategories", Some(cats)),
            ..digit_rounding_pairs(dg)
          ])
        ]),
        st,
      )
    }
    ListFormatData(l) -> #(
      [
        #("locale", mk_string(l.locale)),
        #("type", mk_string(list_format_type_to_js_string(l.list_type))),
        #("style", mk_string(list_format_style_to_js_string(l.style))),
      ],
      st,
    )
    RelativeTimeFormatData(r) -> #(
      [
        #("locale", mk_string(r.locale)),
        #("style", mk_string(rtf_style_to_js_string(r.style))),
        #("numeric", mk_string(rtf_numeric_to_js_string(r.numeric))),
        #("numberingSystem", mk_string(r.numbering_system)),
      ],
      st,
    )
    SegmenterData(sg) -> #(
      [
        #("locale", mk_string(sg.locale)),
        #("granularity", mk_string(granularity_to_js_string(sg.granularity))),
      ],
      st,
    )
    DisplayNamesData(d) -> #(
      present_pairs([
        #("locale", Some(mk_string(d.locale))),
        #("style", Some(mk_string(name_width_to_js_string(d.style)))),
        #(
          "type",
          Some(mk_string(display_names_type_to_js_string(d.display_type))),
        ),
        #(
          "fallback",
          Some(mk_string(display_names_fallback_to_js_string(d.fallback))),
        ),
        #(
          "languageDisplay",
          option.map(d.language_display, fn(ld) {
            mk_string(language_display_to_js_string(ld))
          }),
        ),
      ]),
      st,
    )
    DurationFormatData(df) -> #(
      list.flatten([
        [
          #("locale", mk_string(df.locale)),
          #("numberingSystem", mk_string(df.numbering_system)),
          #("style", mk_string(duration_base_style_to_js_string(df.style))),
        ],
        list.flat_map(duration_unit_list(df), fn(u) {
          let #(unit, o) = u
          let name = duration_unit_js_name(unit)
          [
            #(name, mk_string(duration_unit_style_to_js_string(o.style))),
            #(
              name <> "Display",
              mk_string(duration_display_to_js_string(o.display)),
            ),
          ]
        }),
        case df.fractional_digits {
          Some(f) -> [#("fractionalDigits", mk_number(JInt(f)))]
          None -> []
        },
      ]),
      st,
    )
    // %Segments%/%SegmentIterator% never expose resolvedOptions.
    SegmentsData(_) | SegmentIteratorData(_) -> #([], st)
  }
  alloc_pojo(st, props)
}

/// `[[UseGrouping]]` as its JS resolvedOptions value: never is the boolean
/// `false`, everything else its string spelling.
fn use_grouping_js(g: IntlUseGrouping) -> JsVal {
  case g {
    GroupingNever -> mk_bool(False)
    GroupingAuto -> mk_string("auto")
    GroupingAlways -> mk_string("always")
    GroupingMin2 -> mk_string("min2")
  }
}

/// `[[CompactDisplay]]` as its resolvedOptions value — present only when the
/// notation is compact.
fn compact_display_of(n: Notation) -> Option(JsVal) {
  case n {
    NotationCompact(display:) ->
      Some(mk_string(compact_display_to_js_string(display)))
    NotationStandard | NotationScientific | NotationEngineering -> None
  }
}

fn name_width_js(v: NameWidth) -> JsVal {
  mk_string(name_width_to_js_string(v))
}

fn numeric_width_js(v: NumericWidth) -> JsVal {
  mk_string(numeric_width_to_js_string(v))
}

fn month_width_js(v: MonthWidth) -> JsVal {
  mk_string(month_width_to_js_string(v))
}

fn tz_name_width_js(v: TimeZoneNameWidth) -> JsVal {
  mk_string(time_zone_name_width_to_js_string(v))
}

/// The integer/fraction/significant digit resolvedOptions pairs shared by
/// NumberFormat and PluralRules, prepended to `rest`.
fn digit_option_pairs(
  dg: IntlDigitOptions,
  rest: List(#(String, Option(JsVal))),
) -> List(#(String, Option(JsVal))) {
  let num = fn(i) { mk_number(JInt(i)) }
  [
    #("minimumIntegerDigits", Some(num(dg.minimum_integer_digits))),
    #(
      "minimumFractionDigits",
      option.map(dg.fraction_digits, fn(p) { num(p.0) }),
    ),
    #(
      "maximumFractionDigits",
      option.map(dg.fraction_digits, fn(p) { num(p.1) }),
    ),
    #(
      "minimumSignificantDigits",
      option.map(dg.significant_digits, fn(p) { num(p.0) }),
    ),
    #(
      "maximumSignificantDigits",
      option.map(dg.significant_digits, fn(p) { num(p.1) }),
    ),
    ..rest
  ]
}

/// The roundingIncrement/roundingMode/roundingPriority/trailingZeroDisplay
/// resolvedOptions tail shared by NumberFormat and PluralRules.
fn digit_rounding_pairs(
  dg: IntlDigitOptions,
) -> List(#(String, Option(JsVal))) {
  [
    #("roundingIncrement", Some(mk_number(JInt(dg.rounding_increment)))),
    #(
      "roundingMode",
      Some(mk_string(rounding_mode_to_js_string(dg.rounding_mode))),
    ),
    #(
      "roundingPriority",
      Some(mk_string(rounding_priority_to_js_string(dg.rounding_priority))),
    ),
    #(
      "trailingZeroDisplay",
      Some(
        mk_string(trailing_zero_display_to_js_string(dg.trailing_zero_display)),
      ),
    ),
  ]
}

// ============================================================================
// Bound method getters (format / compare)
// ============================================================================

/// The `format` / `compare` accessor getters (§10.3.3, §15.3.3, §11.3.3): the
/// bound function is created once and cached on the receiver, so the getter
/// is idempotent. `BoundGetterService` has exactly the three services with
/// such an accessor — no "service without a getter" arm.
fn bound_getter(
  st: Agent,
  service: BoundGetterService,
  this: JsVal,
) -> #(JsVal, Agent) {
  let method =
    "Intl."
    <> service_name(intl_data.bound_getter_service(service))
    <> " bound method getter"
  let #(target, cached, arity) = case service {
    BgCollator -> {
      let #(h, _c, cached) = branded_collator(st, this, method)
      #(h, cached, 2)
    }
    BgNumberFormat -> {
      let #(h, _nf, cached) = branded_number_format(st, this, method)
      #(h, cached, 1)
    }
    BgDateTimeFormat -> {
      let #(h, _d, cached) = branded_date_time_format(st, this, method)
      #(h, cached, 1)
    }
  }
  case cached {
    Some(fn_h) -> #(mk_object(fn_h), st)
    None -> {
      // Not rooted: the receiver's `bound` slot keeps it alive.
      let #(fn_h, st) =
        rt_call.t_native_new(
          st,
          Some(st.realm.function.prototype),
          IntlN(IntlBoundMethod(service:, target:)),
          "",
          arity,
          False,
        )
      let st =
        rt_store.t_cell_update(st, target, fn(slot) {
          case slot {
            SObject(kind: IntlObj(data:, ..), ..) ->
              SObject(..slot, kind: IntlObj(data:, bound: Some(fn_h)))
            other -> other
          }
        })
      #(mk_object(fn_h), st)
    }
  }
}

/// The bound `format` / `compare` function itself: `target` is the receiver
/// captured by `bound_getter`, and its brand is re-checked (the instance's
/// state can only have been swapped by another Intl object of the same shape).
fn bound_method(
  st: Agent,
  service: BoundGetterService,
  target: Handle,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let this = mk_object(target)
  let method = "bound Intl method"
  case service {
    BgNumberFormat -> {
      let #(_h, nf, _bound) = branded_number_format(st, this, method)
      let #(parts, st) = nf_format_parts(st, nf, first_arg_or_undefined(args))
      #(mk_string(fmt.parts_to_string(parts)), st)
    }
    BgDateTimeFormat -> {
      let #(_h, d, _bound) = branded_date_time_format(st, this, method)
      let #(parts, st) = dtf_format_parts(st, d, first_arg_or_undefined(args))
      #(mk_string(fmt.parts_to_string(parts)), st)
    }
    BgCollator -> {
      let #(_h, c, _bound) = branded_collator(st, this, method)
      let #(a, st) = rt_val.t_to_string(st, first_arg_or_undefined(args))
      let #(b, st) = rt_val.t_to_string(st, helpers.arg_at(args, 1))
      #(mk_number(JInt(collator_compare(c, a, b))), st)
    }
  }
}

// ============================================================================
// NumberFormat formatting glue
// ============================================================================

/// The `fmt.NumOpts` a NumberFormat instance formats with.
fn num_opts_from_nf(nf: NumberFormatState) -> fmt.NumOpts {
  let d = fmt.default_num_opts()
  with_digits(
    fmt.NumOpts(
      ..d,
      locale: fmt.locale_key(nf.locale),
      style: nf.style,
      use_grouping: nf.use_grouping,
      notation: nf.notation,
      sign_display: nf.sign_display,
    ),
    nf.digits,
  )
}

/// The `fmt.NumOpts` a PluralRules instance derives its plural operands
/// with — its own locale / notation / digit options over decimal defaults.
fn num_opts_from_plural(p: PluralRulesState) -> fmt.NumOpts {
  let d = fmt.default_num_opts()
  with_digits(
    fmt.NumOpts(..d, locale: fmt.locale_key(p.locale), notation: p.notation),
    p.digits,
  )
}

/// ToIntlMathematicalValue, approximated with ToNumber (BigInt allowed).
fn to_intl_number(st: Agent, v: JsVal) -> #(JsNum, Agent) {
  case classify(v) {
    // num_from_int saturates out-of-range BigInts to ±Infinity; a bare
    // int.to_float would badarg (and kill the VM) on e.g. 10n ** 400n.
    KBig(n) -> #(rt_val.num_from_int(n), st)
    _ -> rt_val.t_to_number(st, v)
  }
}

fn nf_format_parts(
  st: Agent,
  nf: NumberFormatState,
  x: JsVal,
) -> #(List(fmt.Part), Agent) {
  let opts = num_opts_from_nf(nf)
  let nu = nf.numbering_system
  // ToIntlMathematicalValue keeps decimal strings exact.
  case classify(x) {
    KStr(str) ->
      case is_plain_decimal(string.trim(str)) {
        True -> #(
          fmt.apply_numbering_system(
            fmt.format_decimal_string_parts(opts, string.trim(str)),
            nu,
            fmt.is_number_digit,
          ),
          st,
        )
        False -> nf_format_number(st, x, opts, nu)
      }
    _ -> nf_format_number(st, x, opts, nu)
  }
}

fn is_plain_decimal(s: String) -> Bool {
  let s = case string.pop_grapheme(s) {
    Ok(#("-", rest)) | Ok(#("+", rest)) -> rest
    _ -> s
  }
  s != ""
  && s != "."
  && !string.starts_with(string.lowercase(s), "infinity")
  && !string.starts_with(string.lowercase(s), "0x")
  && !string.starts_with(string.lowercase(s), "0o")
  && !string.starts_with(string.lowercase(s), "0b")
  && string.to_graphemes(s)
  |> list.all(fn(c) {
    c == "."
    || c == "e"
    || c == "E"
    || c == "+"
    || c == "-"
    || case int.parse(c) {
      Ok(_) -> True
      Error(Nil) -> False
    }
  })
}

fn nf_format_number(
  st: Agent,
  x: JsVal,
  opts: fmt.NumOpts,
  nu: String,
) -> #(List(fmt.Part), Agent) {
  let #(n, st) = to_intl_number(st, x)
  let parts = case n {
    JNan -> fmt.format_nan_parts(opts)
    JPosInf -> fmt.format_infinity_parts(opts, False)
    JNegInf -> fmt.format_infinity_parts(opts, True)
    JFloat(f) -> fmt.format_number_parts(opts, f)
    JInt(i) -> fmt.format_number_parts(opts, int.to_float(i))
  }
  #(fmt.apply_numbering_system(parts, nu, fmt.is_number_digit), st)
}

fn nf_range_parts(
  st: Agent,
  nf: NumberFormatState,
  x_v: JsVal,
  y_v: JsVal,
) -> #(List(fmt.RangePart), Agent) {
  let st = case rt_val.is_undef(x_v) || rt_val.is_undef(y_v) {
    True -> rt_val.t_throw_type_error(st, "Invalid range arguments")
    False -> st
  }
  let #(x, st) = to_intl_number(st, x_v)
  let #(y, st) = to_intl_number(st, y_v)
  let st = case x, y {
    JNan, _ | _, JNan ->
      rt_val.t_throw_range_error(st, "Invalid range argument: NaN")
    _, _ -> st
  }
  // Format the original values: decimal strings stay exact (they can exceed
  // float precision), everything else uses the coerced number.
  let x_fmt = case classify(x_v) {
    KStr(_) -> x_v
    _ -> mk_number(x)
  }
  let y_fmt = case classify(y_v) {
    KStr(_) -> y_v
    _ -> mk_number(y)
  }
  let #(x_parts, st) = nf_format_parts(st, nf, x_fmt)
  let #(y_parts, st) = nf_format_parts(st, nf, y_fmt)
  #(fmt.format_range_combine(fmt.locale_key(nf.locale), x_parts, y_parts), st)
}

// ============================================================================
// DateTimeFormat formatting glue
// ============================================================================

/// A Temporal object as seen by DateTimeFormat (ECMA-402 HandleDateTimeValue).
type TemporalFormattable {
  /// A wall-clock Temporal type: its fields ARE the fields to format.
  TfPlain(PlainTemporal)
  /// Temporal.Instant — an exact time, rendered through the formatter's zone.
  TfInstant(epoch_ns: Int)
  /// Temporal.ZonedDateTime — always a TypeError (`toLocaleString` instead).
  TfZoned
}

/// The Temporal types with no instant behind them, split out of
/// `TemporalFormattable` so `plain_temporal_fields` / `plain_component_rules`
/// are total: neither can be reached with an Instant or a ZonedDateTime, so
/// neither needs a "cannot happen" fallback that would silently format the
/// epoch.
type PlainTemporal {
  PDate(year: Int, month: Int, day: Int, calendar: String)
  PYearMonth(year: Int, month: Int, day: Int, calendar: String)
  PMonthDay(month: Int, day: Int, ref_year: Int, calendar: String)
  PTime(hour: Int, minute: Int, second: Int, millisecond: Int)
  PDateTime(
    year: Int,
    month: Int,
    day: Int,
    hour: Int,
    minute: Int,
    second: Int,
    millisecond: Int,
    calendar: String,
  )
}

/// A Temporal value that DateTimeFormat accepted: ZonedDateTime has already
/// thrown, so field extraction sees only these two shapes.
type AcceptedTemporal {
  AtInstant(epoch_ns: Int)
  AtPlain(PlainTemporal)
}

/// HandleDateTimeValue's ZonedDateTime rejection.
fn throw_zoned(st: Agent) -> a {
  rt_val.t_throw_type_error(
    st,
    "Temporal.ZonedDateTime cannot be formatted with Intl.DateTimeFormat; use Temporal.ZonedDateTime.prototype.toLocaleString instead",
  )
}

fn accept_temporal(st: Agent, t: TemporalFormattable) -> AcceptedTemporal {
  case t {
    TfInstant(epoch_ns:) -> AtInstant(epoch_ns:)
    TfPlain(p) -> AtPlain(p)
    TfZoned -> throw_zoned(st)
  }
}

/// IsTemporalObject — recognize Temporal values handed to format methods.
fn dtf_temporal_value(st: Agent, v: JsVal) -> Option(TemporalFormattable) {
  case classify(v) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: TemporalObj(data:), ..) -> temporal_formattable(data)
        _ -> None
      }
    _ -> None
  }
}

/// The DateTimeFormat view of a Temporal object's slots. Duration is not a
/// date-time value (it goes through ToNumber like any other object).
fn temporal_formattable(data: TemporalData) -> Option(TemporalFormattable) {
  case data {
    TemporalInstant(epoch_ns:) -> Some(TfInstant(epoch_ns:))
    TemporalDate(year:, month:, day:, calendar:) ->
      Some(
        TfPlain(PDate(year:, month:, day:, calendar: tcal.identifier(calendar))),
      )
    TemporalYearMonth(year:, month:, day:, calendar:) ->
      Some(
        TfPlain(PYearMonth(
          year:,
          month:,
          day:,
          calendar: tcal.identifier(calendar),
        )),
      )
    TemporalMonthDay(month:, day:, ref_year:, calendar:) ->
      Some(
        TfPlain(PMonthDay(
          month:,
          day:,
          ref_year:,
          calendar: tcal.identifier(calendar),
        )),
      )
    TemporalTime(hour:, minute:, second:, millisecond:, ..) ->
      Some(TfPlain(PTime(hour:, minute:, second:, millisecond:)))
    TemporalDateTime(
      year:,
      month:,
      day:,
      hour:,
      minute:,
      second:,
      millisecond:,
      calendar:,
      ..,
    ) ->
      Some(
        TfPlain(PDateTime(
          year:,
          month:,
          day:,
          hour:,
          minute:,
          second:,
          millisecond:,
          calendar: tcal.identifier(calendar),
        )),
      )
    TemporalZonedDateTime(..) -> Some(TfZoned)
    TemporalDuration(..) -> None
  }
}

/// SameTemporalType — both values are the same Temporal type.
fn same_temporal_kind(a: TemporalFormattable, b: TemporalFormattable) -> Bool {
  case a, b {
    TfPlain(a), TfPlain(b) -> same_plain_kind(a, b)
    TfInstant(..), TfInstant(..) -> True
    TfZoned, TfZoned -> True
    _, _ -> False
  }
}

fn same_plain_kind(a: PlainTemporal, b: PlainTemporal) -> Bool {
  case a, b {
    PDate(..), PDate(..) -> True
    PYearMonth(..), PYearMonth(..) -> True
    PMonthDay(..), PMonthDay(..) -> True
    PTime(..), PTime(..) -> True
    PDateTime(..), PDateTime(..) -> True
    _, _ -> False
  }
}

/// Allowed / required / default components per Temporal type, plus
/// whether `era` / hour-cycle options carry over (GetDateTimeFormat's
/// ~relevant~ inheritance).
fn plain_component_rules(
  t: PlainTemporal,
) -> #(List(DtfComponent), List(DtfComponent), DtfComponents, Bool) {
  case t {
    PDate(..) -> #(
      [DtfWeekday, DtfEra, DtfYear, DtfMonth, DtfDay],
      [DtfWeekday, DtfYear, DtfMonth, DtfDay],
      date_defaults(),
      True,
    )
    PYearMonth(..) -> #(
      [DtfEra, DtfYear, DtfMonth],
      [DtfYear, DtfMonth],
      DtfComponents(
        ..intl_data.empty_dtf_components,
        year: Some(WNumeric),
        month: Some(MonthNum(WNumeric)),
      ),
      True,
    )
    PMonthDay(..) -> #(
      [DtfMonth, DtfDay],
      [DtfMonth, DtfDay],
      DtfComponents(
        ..intl_data.empty_dtf_components,
        month: Some(MonthNum(WNumeric)),
        day: Some(WNumeric),
      ),
      False,
    )
    PTime(..) -> #(
      [DtfDayPeriod, DtfHour, DtfMinute, DtfSecond, DtfFractionalSecondDigits],
      [DtfDayPeriod, DtfHour, DtfMinute, DtfSecond, DtfFractionalSecondDigits],
      time_defaults(),
      False,
    )
    PDateTime(..) -> #(
      [
        DtfWeekday, DtfEra, DtfYear, DtfMonth, DtfDay, DtfDayPeriod, DtfHour,
        DtfMinute, DtfSecond, DtfFractionalSecondDigits,
      ],
      [
        DtfWeekday, DtfYear, DtfMonth, DtfDay, DtfDayPeriod, DtfHour, DtfMinute,
        DtfSecond, DtfFractionalSecondDigits,
      ],
      merge_components(date_defaults(), time_defaults()),
      True,
    )
  }
}

/// HandleDateTimeValue: validate the Temporal value against the formatter
/// (calendar compatibility, suitable format availability) and return the
/// state with its formatting components adjusted to the per-type format.
fn dtf_temporal_state(
  st: Agent,
  d: DateTimeFormatState,
  t: TemporalFormattable,
) -> DateTimeFormatState {
  case t {
    TfZoned -> throw_zoned(st)
    // [[TemporalInstantFormat]] is GetDateTimeFormat(..., required = ~any~,
    // defaults = ~all~): with no style and no explicit date/time component,
    // an Instant defaults to date AND time — the constructor only defaulted
    // the date half (defaults = ~date~). Repro:
    //   new Intl.DateTimeFormat("en", { era: "narrow" })
    //     .format(new Temporal.Instant(0n))
    //     === new Date(0).toLocaleString("en", { era: "narrow" })
    TfInstant(..) ->
      case
        d.explicit != []
        || option.is_some(d.date_style)
        || option.is_some(d.time_style)
      {
        True -> d
        False ->
          with_components(d, merge_components(d.components, time_defaults()))
      }
    TfPlain(p) -> {
      let cal_ok = case p {
        PDate(calendar:, ..) | PDateTime(calendar:, ..) ->
          calendar == "iso8601" || calendar == d.calendar
        PYearMonth(calendar:, ..) | PMonthDay(calendar:, ..) ->
          calendar == d.calendar
        PTime(..) -> True
      }
      use Nil <- helpers.guard(cal_ok, fn() {
        rt_val.t_throw_range_error(
          st,
          "Temporal object calendar does not match DateTimeFormat calendar",
        )
      })
      let #(allowed, required, defaults, copy_era) = plain_component_rules(p)
      let has_styles =
        option.is_some(d.date_style) || option.is_some(d.time_style)
      case has_styles {
        True -> {
          // AdjustDateTimeStyleFormat: per-type formats exist only when the
          // matching style was given; keep only the allowed components.
          let style_ok = case p {
            PDate(..) | PYearMonth(..) | PMonthDay(..) ->
              option.is_some(d.date_style)
            PTime(..) -> option.is_some(d.time_style)
            PDateTime(..) -> True
          }
          case style_ok {
            True -> with_components(d, keep_components(d.components, allowed))
            False ->
              rt_val.t_throw_type_error(
                st,
                "DateTimeFormat has no suitable format for this Temporal type",
              )
          }
        }
        False -> {
          // GetDateTimeFormat with inherit = ~relevant~ over the explicitly
          // provided component options.
          let in_required =
            list.filter(d.explicit, fn(name) { list.contains(required, name) })
          let era = case copy_era {
            True -> d.components.era
            False -> None
          }
          case in_required {
            [] ->
              case d.explicit {
                [] -> with_components(d, DtfComponents(..defaults, era:))
                _ ->
                  rt_val.t_throw_type_error(
                    st,
                    "DateTimeFormat options have no overlap with this Temporal type",
                  )
              }
            _ -> {
              let kept =
                keep_components(
                  d.components,
                  list.filter(required, fn(name) {
                    list.contains(d.explicit, name)
                  }),
                )
              with_components(d, DtfComponents(..kept, era:))
            }
          }
        }
      }
    }
  }
}

/// Replace the formatter's component table.
fn with_components(
  d: DateTimeFormatState,
  components: DtfComponents,
) -> DateTimeFormatState {
  DateTimeFormatState(..d, components:)
}

fn civil_week_day(year: Int, month: Int, day: Int) -> Int {
  gregorian.weekday_from_days(days_from_civil(year, month, day))
}

/// Wall-clock fields for a Temporal value, and the zone offset that produced
/// them (which a requested `timeZoneName` renders). Plain types format their
/// fields directly (the formatter's time zone is ignored); Instant converts
/// through the formatter's zone like a Number time value. `now_ms` is the
/// host wall clock, read only for the plain types' zone-name offset.
fn dtf_temporal_fields(
  d: DateTimeFormatState,
  t: AcceptedTemporal,
  now_ms: fn() -> Int,
) -> #(fmt.DateFields, Int) {
  case t {
    AtInstant(epoch_ns:) -> {
      let ms = floor_div(epoch_ns, 1_000_000)
      let offset = tz.offset_at(d.time_zone, ms)
      #(fmt.fields_from_epoch_ms(int.to_float(ms), offset), offset)
    }
    // Plain types carry no instant, so a requested timeZoneName can only show
    // the zone's offset now.
    AtPlain(p) -> #(
      plain_temporal_fields(p),
      tz.offset_at(d.time_zone, now_ms()),
    )
  }
}

fn plain_temporal_fields(t: PlainTemporal) -> fmt.DateFields {
  case t {
    PDate(year:, month:, day:, ..) | PYearMonth(year:, month:, day:, ..) ->
      fmt.DateFields(
        year:,
        month:,
        day:,
        hour: 12,
        minute: 0,
        second: 0,
        millisecond: 0,
        week_day: civil_week_day(year, month, day),
      )
    PMonthDay(month:, day:, ref_year:, ..) ->
      fmt.DateFields(
        year: ref_year,
        month:,
        day:,
        hour: 12,
        minute: 0,
        second: 0,
        millisecond: 0,
        week_day: civil_week_day(ref_year, month, day),
      )
    PTime(hour:, minute:, second:, millisecond:) ->
      fmt.DateFields(
        year: 1970,
        month: 1,
        day: 1,
        hour:,
        minute:,
        second:,
        millisecond:,
        week_day: 4,
      )
    PDateTime(year:, month:, day:, hour:, minute:, second:, millisecond:, ..) ->
      fmt.DateFields(
        year:,
        month:,
        day:,
        hour:,
        minute:,
        second:,
        millisecond:,
        week_day: civil_week_day(year, month, day),
      )
  }
}

fn dtf_format_parts(
  st: Agent,
  d: DateTimeFormatState,
  date_v: JsVal,
) -> #(List(fmt.Part), Agent) {
  case dtf_temporal_value(st, date_v) {
    Some(t) -> {
      let d = dtf_temporal_state(st, d, t)
      let accepted = accept_temporal(st, t)
      let #(fields, offset) =
        dtf_temporal_fields(d, accepted, st.hooks.wall_clock_ms)
      let parts = build_dtf_parts(d, fields, offset)
      #(
        fmt.apply_numbering_system(
          parts,
          d.numbering_system,
          fmt.is_date_numeric,
        ),
        st,
      )
    }
    None -> dtf_format_parts_number(st, d, date_v)
  }
}

fn dtf_format_parts_number(
  st: Agent,
  d: DateTimeFormatState,
  date_v: JsVal,
) -> #(List(fmt.Part), Agent) {
  let #(fields, offset, st) = dtf_fields_number(st, d, date_v)
  let parts = build_dtf_parts(d, fields, offset)
  #(
    fmt.apply_numbering_system(parts, d.numbering_system, fmt.is_date_numeric),
    st,
  )
}

fn build_dtf_parts(
  d: DateTimeFormatState,
  fields: fmt.DateFields,
  offset: Int,
) -> List(fmt.Part) {
  let DtfComponents(
    weekday:,
    era:,
    year:,
    month:,
    day:,
    day_period:,
    hour:,
    minute:,
    second:,
    fractional_second_digits: fractional,
    time_zone_name: tz_name,
  ) = d.components
  let hc = option.unwrap(d.hour_cycle, H12)

  let display_year = case fields.year <= 0 {
    True -> 1 - fields.year
    False -> fields.year
  }
  let year_str = fn(width) { numeric_width_str(width, display_year) }
  let weekday_parts = case weekday {
    Some(w) -> [#(PWeekday, fmt.weekday_name(fields.week_day, w))]
    None -> []
  }
  // Date portion.
  let date_parts = case month {
    Some(MonthName(mw)) -> {
      let m_part = [#(PMonth, fmt.month_name(fields.month, mw))]
      let d_part = case day {
        Some(dw) -> [
          #(PLiteral, " "),
          #(PDay, numeric_width_str(dw, fields.day)),
        ]
        None -> []
      }
      let y_part = case year {
        Some(yw) ->
          case day {
            Some(_) -> [#(PLiteral, ", "), #(PYear, year_str(yw))]
            None -> [#(PLiteral, " "), #(PYear, year_str(yw))]
          }
        None -> []
      }
      list.flatten([m_part, d_part, y_part])
    }
    // Numeric month / partial combos. Most locales we ship use M/D/Y with
    // "/"; German-style locales order D.M.Y with ".".
    Some(MonthNum(_)) | None -> {
      let month_num = case month {
        Some(MonthNum(mw)) -> Some(numeric_width_str(mw, fields.month))
        Some(MonthName(_)) | None -> None
      }
      let lang = case tags.parse(d.locale) {
        Ok(lid) -> lid.language
        Error(Nil) -> "en"
      }
      let dotted =
        list.contains(
          ["de", "fi", "ru", "cs", "tr", "nb", "pl", "uk", "bg", "sr", "lv"],
          lang,
        )
      let m_pair = #(PMonth, month_num)
      let d_pair = #(
        PDay,
        option.map(day, fn(dw) { numeric_width_str(dw, fields.day) }),
      )
      let y_pair = #(PYear, option.map(year, year_str))
      let raw = case dotted {
        True -> [d_pair, m_pair, y_pair]
        False -> [m_pair, d_pair, y_pair]
      }
      let pieces = present_pairs(raw)
      case dotted {
        True -> join_parts(pieces, ".")
        False -> join_parts(pieces, "/")
      }
    }
  }
  let date_parts = case era, date_parts {
    Some(e), [_, ..] ->
      list.append(date_parts, [
        #(PLiteral, " "),
        #(PEra, fmt.era_name(fields.year, e)),
      ])
    _, _ -> date_parts
  }
  // Time portion.
  let #(display_hour, dp) = case hc {
    H11 -> #(fields.hour % 12, am_pm(fields.hour))
    H12 -> {
      let h = fields.hour % 12
      #(
        case h {
          0 -> 12
          _ -> h
        },
        am_pm(fields.hour),
      )
    }
    H24 -> #(
      case fields.hour {
        0 -> 24
        h -> h
      },
      "",
    )
    H23 -> #(fields.hour, "")
  }
  let hour_parts = case hour {
    Some(WTwoDigit) -> [#(PHour, fmt.pad2(display_hour))]
    Some(WNumeric) -> [#(PHour, int.to_string(display_hour))]
    None -> []
  }
  let minute_parts = case minute {
    Some(width) -> {
      let v = case hour, second {
        Some(_), _ -> fmt.pad2(fields.minute)
        None, Some(_) -> fmt.pad2(fields.minute)
        None, None ->
          case width {
            WTwoDigit -> fmt.pad2(fields.minute)
            WNumeric -> int.to_string(fields.minute)
          }
      }
      case hour_parts {
        [] -> [#(PMinute, v)]
        _ -> [#(PLiteral, ":"), #(PMinute, v)]
      }
    }
    None -> []
  }
  let second_parts = case second {
    Some(width) -> {
      let v = case minute {
        Some(_) -> fmt.pad2(fields.second)
        None ->
          case width {
            WTwoDigit -> fmt.pad2(fields.second)
            WNumeric -> int.to_string(fields.second)
          }
      }
      case minute_parts {
        [] -> [#(PSecond, v)]
        _ -> [#(PLiteral, ":"), #(PSecond, v)]
      }
    }
    None -> []
  }
  let fractional_parts = case fractional {
    Some(digits) -> {
      let ms3 = string.pad_start(int.to_string(fields.millisecond), 3, "0")
      let v = string.slice(ms3, 0, digits)
      case second_parts {
        [] -> [#(PFractionalSecond, v)]
        _ -> [#(PLiteral, "."), #(PFractionalSecond, v)]
      }
    }
    None -> []
  }
  let day_period_parts = case day_period, hour {
    Some(dpw), _ -> [
      #(PLiteral, " "),
      #(PDayPeriod, fmt.day_period_name(fields.hour, fields.minute, dpw)),
    ]
    None, Some(_) ->
      case dp {
        "" -> []
        _ -> [#(PLiteral, " "), #(PDayPeriod, dp)]
      }
    None, None -> []
  }
  // Standalone dayPeriod (no hour): no leading space.
  let day_period_parts = case hour, day_period {
    None, Some(dpw) -> [
      #(PDayPeriod, fmt.day_period_name(fields.hour, fields.minute, dpw)),
    ]
    _, _ -> day_period_parts
  }
  let tz_parts = case tz_name {
    Some(width) -> {
      let name =
        tz.display(intl_data.dtf_time_zone_id(d.time_zone), width, offset)
      [#(PLiteral, " "), #(PTimeZoneName, name)]
    }
    None -> []
  }
  let time_parts =
    list.flatten([
      hour_parts,
      minute_parts,
      second_parts,
      fractional_parts,
      day_period_parts,
    ])
  let time_parts = case time_parts, tz_parts {
    [], [#(PLiteral, _), ..rest] -> rest
    _, _ -> list.append(time_parts, tz_parts)
  }
  let all = case weekday_parts, date_parts, time_parts {
    [], [], t -> t
    w, [], [] -> w
    [], d, [] -> d
    w, d, [] -> list.flatten([w, [#(PLiteral, ", ")], d])
    [], d, t -> list.flatten([d, [#(PLiteral, ", ")], t])
    w, [], t -> list.flatten([w, [#(PLiteral, " ")], t])
    w, d, t -> list.flatten([w, [#(PLiteral, ", ")], d, [#(PLiteral, ", ")], t])
  }
  all
}

fn am_pm(hour: Int) -> String {
  case hour < 12 {
    True -> "AM"
    False -> "PM"
  }
}

/// Render an integer under a `numeric`/`2-digit` width. `2-digit` truncates
/// to the low two digits (per §11.5.8 for year/month/day).
fn numeric_width_str(width: NumericWidth, n: Int) -> String {
  case width {
    WTwoDigit -> fmt.pad2(n % 100)
    WNumeric -> int.to_string(n)
  }
}

fn join_parts(pieces: List(fmt.Part), sep: String) -> List(fmt.Part) {
  case pieces {
    [] -> []
    [first, ..rest] ->
      list.fold(rest, [first], fn(acc, p) { [p, #(PLiteral, sep), ..acc] })
      |> list.reverse
  }
}

fn dtf_range_parts(
  st: Agent,
  d: DateTimeFormatState,
  x_v: JsVal,
  y_v: JsVal,
) -> #(List(fmt.RangePart), Agent) {
  let defined = !{ rt_val.is_undef(x_v) || rt_val.is_undef(y_v) }
  use Nil <- helpers.guard(defined, fn() {
    rt_val.t_throw_type_error(st, "Invalid range arguments")
  })
  // ToDateTimeFormattable runs on both arguments (in order) before the
  // SameTemporalType check: Temporal objects pass through, everything else
  // goes through ToNumber.
  let tx = dtf_temporal_value(st, x_v)
  let ty = dtf_temporal_value(st, y_v)
  let #(x_v, st) = case tx {
    Some(_) -> #(x_v, st)
    None -> {
      let #(n, st) = rt_val.t_to_number(st, x_v)
      #(mk_number(n), st)
    }
  }
  let #(y_v, st) = case ty {
    Some(_) -> #(y_v, st)
    None -> {
      let #(n, st) = rt_val.t_to_number(st, y_v)
      #(mk_number(n), st)
    }
  }
  let same_type_error = fn() {
    rt_val.t_throw_type_error(
      st,
      "Intl.DateTimeFormat range arguments must be of the same type",
    )
  }
  let d = case tx, ty {
    None, None -> d
    Some(a), Some(b) ->
      case same_temporal_kind(a, b) {
        False -> same_type_error()
        True -> {
          // Validates x (calendar / suitable format) and yields the
          // per-type adjusted components; y is validated separately.
          let adjusted = dtf_temporal_state(st, d, a)
          let _d_y = dtf_temporal_state(st, d, b)
          adjusted
        }
      }
    _, _ -> same_type_error()
  }
  let #(collapsed, st) = dtf_collapsed_range(st, d, x_v, y_v)
  case collapsed {
    Some(parts) -> #(parts, st)
    None -> {
      let #(x_parts, st) = dtf_format_parts(st, d, x_v)
      let #(y_parts, st) = dtf_format_parts(st, d, y_v)
      let sourced = fn(p: fmt.Part, source) { fmt.RangePart(p.0, p.1, source) }
      case fmt.parts_to_string(x_parts) == fmt.parts_to_string(y_parts) {
        True -> #(list.map(x_parts, sourced(_, fmt.SourceShared)), st)
        False -> #(
          list.flatten([
            list.map(x_parts, sourced(_, fmt.SourceStart)),
            [sourced(#(PLiteral, " – "), fmt.SourceShared)],
            list.map(y_parts, sourced(_, fmt.SourceEnd)),
          ]),
          st,
        )
      }
    }
  }
}

/// "Jan 3 – 5, 2019": collapse a named-month date-only range that differs
/// only in the day.
fn dtf_collapsed_range(
  st: Agent,
  d: DateTimeFormatState,
  x_v: JsVal,
  y_v: JsVal,
) -> #(Option(List(fmt.RangePart)), Agent) {
  let c = d.components
  let date_only =
    c.hour == None && c.minute == None && c.second == None && c.weekday == None
  // A named month with a year and a day is the only shape we collapse.
  case c.month, c.year, c.day, date_only {
    Some(MonthName(month_width)), Some(year_width), Some(day_width), True -> {
      let #(xf, st) = dtf_fields(st, d, x_v)
      let #(yf, st) = dtf_fields(st, d, y_v)
      let day_style = day_width
      let display_year = case xf.year <= 0 {
        True -> 1 - xf.year
        False -> xf.year
      }
      let year_str = case year_width {
        WTwoDigit -> fmt.pad2(display_year % 100)
        WNumeric -> int.to_string(display_year)
      }
      let mname = fn(m) { fmt.month_name(m, month_width) }
      case xf.year == yf.year {
        False -> #(None, st)
        True ->
          case xf.month == yf.month, xf.day != yf.day {
            True, True -> {
              let parts = [
                fmt.RangePart(PMonth, mname(xf.month), fmt.SourceShared),
                fmt.RangePart(PLiteral, " ", fmt.SourceShared),
                fmt.RangePart(
                  PDay,
                  numeric_width_str(day_style, xf.day),
                  fmt.SourceStart,
                ),
                fmt.RangePart(PLiteral, " – ", fmt.SourceShared),
                fmt.RangePart(
                  PDay,
                  numeric_width_str(day_style, yf.day),
                  fmt.SourceEnd,
                ),
                fmt.RangePart(PLiteral, ", ", fmt.SourceShared),
                fmt.RangePart(PYear, year_str, fmt.SourceShared),
              ]
              #(Some(parts), st)
            }
            False, _ -> {
              let parts = [
                fmt.RangePart(PMonth, mname(xf.month), fmt.SourceStart),
                fmt.RangePart(PLiteral, " ", fmt.SourceStart),
                fmt.RangePart(
                  PDay,
                  numeric_width_str(day_style, xf.day),
                  fmt.SourceStart,
                ),
                fmt.RangePart(PLiteral, " – ", fmt.SourceShared),
                fmt.RangePart(PMonth, mname(yf.month), fmt.SourceEnd),
                fmt.RangePart(PLiteral, " ", fmt.SourceEnd),
                fmt.RangePart(
                  PDay,
                  numeric_width_str(day_style, yf.day),
                  fmt.SourceEnd,
                ),
                fmt.RangePart(PLiteral, ", ", fmt.SourceShared),
                fmt.RangePart(PYear, year_str, fmt.SourceShared),
              ]
              #(Some(parts), st)
            }
            True, False -> #(None, st)
          }
      }
    }
    _, _, _, _ -> #(None, st)
  }
}

/// Compute the civil fields a DTF instance would use for a value.
fn dtf_fields(
  st: Agent,
  d: DateTimeFormatState,
  date_v: JsVal,
) -> #(fmt.DateFields, Agent) {
  case dtf_temporal_value(st, date_v) {
    Some(t) -> {
      let accepted = accept_temporal(st, t)
      let #(fields, _offset) =
        dtf_temporal_fields(d, accepted, st.hooks.wall_clock_ms)
      #(fields, st)
    }
    None -> {
      let #(fields, _offset, st) = dtf_fields_number(st, d, date_v)
      #(fields, st)
    }
  }
}

/// Civil fields for a Number time value, plus the zone offset used.
fn dtf_fields_number(
  st: Agent,
  d: DateTimeFormatState,
  date_v: JsVal,
) -> #(fmt.DateFields, Int, Agent) {
  let #(tv, st) = case classify(date_v) {
    KUndef -> #(JInt(st.hooks.wall_clock_ms()), st)
    _ -> rt_val.t_to_number(st, date_v)
  }
  let tv_f = case tv {
    JInt(i) -> time_clip(st, int.to_float(i))
    // TimeClip truncates toward zero before the range check.
    JFloat(f) -> time_clip(st, int.to_float(float.truncate(f)))
    JNan | JPosInf | JNegInf ->
      rt_val.t_throw_range_error(st, "Invalid time value")
  }
  let offset = tz.offset_at(d.time_zone, float.truncate(tv_f))
  #(fmt.fields_from_epoch_ms(tv_f, offset), offset, st)
}

/// TimeClip's range check on an already-integral time value.
fn time_clip(st: Agent, f: Float) -> Float {
  case float.absolute_value(f) <=. 8.64e15 {
    True -> f
    False -> rt_val.t_throw_range_error(st, "Invalid time value")
  }
}

// ============================================================================
// Prototype methods (IntlMethod dispatch)
// ============================================================================

fn run_method(
  st: Agent,
  service: IntlService,
  method: IntlMethodName,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let arg0 = first_arg_or_undefined(args)
  let arg1 = helpers.arg_at(args, 1)
  let js_name =
    "Intl."
    <> service_name(service)
    <> ".prototype."
    <> intl_method_js_name(method)
  let #(h, data, _bound) = branded(st, this, service, js_name)
  case method, data {
    IntlFormatToParts, NumberFormatData(nf) -> {
      let #(parts, st) = nf_format_parts(st, nf, arg0)
      parts_to_js(st, parts)
    }
    IntlFormatRange, NumberFormatData(nf) -> {
      let #(parts, st) = nf_range_parts(st, nf, arg0, arg1)
      #(mk_string(range_parts_to_string(parts)), st)
    }
    IntlFormatRangeToParts, NumberFormatData(nf) -> {
      let #(parts, st) = nf_range_parts(st, nf, arg0, arg1)
      parts_to_js_sourced(st, parts)
    }
    IntlFormatToParts, DateTimeFormatData(d) -> {
      let #(parts, st) = dtf_format_parts(st, d, arg0)
      parts_to_js(st, parts)
    }
    IntlFormatRange, DateTimeFormatData(d) -> {
      let #(parts, st) = dtf_range_parts(st, d, arg0, arg1)
      #(mk_string(range_parts_to_string(parts)), st)
    }
    IntlFormatRangeToParts, DateTimeFormatData(d) -> {
      let #(parts, st) = dtf_range_parts(st, d, arg0, arg1)
      parts_to_js_sourced(st, parts)
    }
    IntlSelect, PluralRulesData(p) -> {
      let #(n, st) = rt_val.t_to_number(st, arg0)
      #(mk_string(fmt.plural_category_to_js_string(plural_select(p, n))), st)
    }
    IntlSelectRange, PluralRulesData(_) -> {
      let defined = !{ rt_val.is_undef(arg0) || rt_val.is_undef(arg1) }
      use Nil <- helpers.guard(defined, fn() {
        rt_val.t_throw_type_error(st, "Invalid selectRange arguments")
      })
      let #(x, st) = rt_val.t_to_number(st, arg0)
      let #(y, st) = rt_val.t_to_number(st, arg1)
      case x, y {
        JNan, _ | _, JNan ->
          rt_val.t_throw_range_error(st, "Invalid selectRange argument: NaN")
        // CLDR en plural ranges resolve to "other" for all combinations.
        _, _ -> #(mk_string(fmt.plural_category_to_js_string(fmt.PcOther)), st)
      }
    }
    IntlFormat, ListFormatData(l) -> {
      let #(items, st) = string_list_from_iterable(st, arg0)
      let parts = fmt.list_format_parts(l.list_type, l.style, items)
      #(mk_string(fmt.parts_to_string(parts)), st)
    }
    IntlFormatToParts, ListFormatData(l) -> {
      let #(items, st) = string_list_from_iterable(st, arg0)
      let parts = fmt.list_format_parts(l.list_type, l.style, items)
      parts_to_js(st, parts)
    }
    IntlFormat, RelativeTimeFormatData(r) -> {
      let #(parts, st) = rtf_method_parts(st, r, arg0, arg1)
      #(mk_string(fmt.unit_parts_to_string(parts)), st)
    }
    IntlFormatToParts, RelativeTimeFormatData(r) -> {
      let #(parts, st) = rtf_method_parts(st, r, arg0, arg1)
      parts_to_js_with_unit(st, parts)
    }
    IntlOf, DisplayNamesData(dn) -> display_names_of(st, dn, arg0)
    IntlFormat, DurationFormatData(df) -> {
      let #(parts, st) = duration_parts(st, df, arg0)
      #(mk_string(fmt.unit_parts_to_string(parts)), st)
    }
    IntlFormatToParts, DurationFormatData(df) -> {
      let #(parts, st) = duration_parts(st, df, arg0)
      parts_to_js_with_unit(st, parts)
    }
    IntlSegmentsContaining, SegmentsData(sg) ->
      segments_containing(st, sg, arg0)
    IntlSegmentIteratorNext, SegmentIteratorData(it) ->
      segment_iterator_next(st, h, it)
    // `branded` guarantees data matches `service`, so these pairings are
    // methods that were never registered on the receiver's prototype.
    IntlFormat, _
    | IntlFormatToParts, _
    | IntlFormatRange, _
    | IntlFormatRangeToParts, _
    | IntlSelect, _
    | IntlSelectRange, _
    | IntlOf, _
    | IntlSegmentIteratorNext, _
    | IntlSegmentsContaining, _
    ->
      rt_val.t_throw_type_error(
        st,
        js_name <> " called on incompatible receiver",
      )
  }
}

/// The Number/BigInt/String/Date prototype locale-sensitive overrides
/// (ECMA-402 §17-19) — installed by `init`, no Intl brand check.
fn run_host_override(
  st: Agent,
  which: IntlHostOverrideName,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let arg0 = first_arg_or_undefined(args)
  let arg1 = helpers.arg_at(args, 1)
  case which {
    NumberToLocaleString -> host_number_to_locale_string(st, this, arg0, arg1)
    BigIntToLocaleString -> host_bigint_to_locale_string(st, this, arg0, arg1)
    StringLocaleCompare -> {
      let arg2 = helpers.arg_at(args, 2)
      host_locale_compare(st, this, arg0, arg1, arg2)
    }
    StringToLocaleLowerCase -> host_locale_case(st, this, arg0, False)
    StringToLocaleUpperCase -> host_locale_case(st, this, arg0, True)
    DateToLocaleString -> host_date_to_locale(st, this, arg0, arg1, DateAndTime)
    DateToLocaleDateString ->
      host_date_to_locale(st, this, arg0, arg1, DateOnly)
    DateToLocaleTimeString ->
      host_date_to_locale(st, this, arg0, arg1, TimeOnly)
  }
}

/// Number.prototype.toLocaleString (ECMA-402 §18.2.1).
fn host_number_to_locale_string(
  st: Agent,
  this: JsVal,
  locales: JsVal,
  options: JsVal,
) -> #(JsVal, Agent) {
  let not_number = fn() {
    rt_val.t_throw_type_error(
      st,
      "Number.prototype.toLocaleString requires that 'this' be a Number",
    )
  }
  let n = case classify(this) {
    KNum(n) -> n
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: NumberObj(value: n), ..) -> n
        _ -> not_number()
      }
    _ -> not_number()
  }
  let #(nf, st) = number_format_state(st, locales, options)
  let #(parts, st) = nf_format_parts(st, nf, mk_number(n))
  #(mk_string(fmt.parts_to_string(parts)), st)
}

/// BigInt.prototype.toLocaleString (ECMA-402 §18.3.1) — same NumberFormat path
/// as Number.prototype.toLocaleString, but the value is handed over as its
/// exact decimal string so arbitrarily large BigInts keep every digit.
fn host_bigint_to_locale_string(
  st: Agent,
  this: JsVal,
  locales: JsVal,
  options: JsVal,
) -> #(JsVal, Agent) {
  let not_bigint = fn() {
    rt_val.t_throw_type_error(
      st,
      "BigInt.prototype.toLocaleString requires that 'this' be a BigInt",
    )
  }
  let n = case classify(this) {
    KBig(n) -> n
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: BigIntObj(value: n), ..) -> n
        _ -> not_bigint()
      }
    _ -> not_bigint()
  }
  let #(nf, st) = number_format_state(st, locales, options)
  let #(parts, st) = nf_format_parts(st, nf, mk_string(int.to_string(n)))
  #(mk_string(fmt.parts_to_string(parts)), st)
}

/// String.prototype.localeCompare (ECMA-402 §19.1.1).
fn host_locale_compare(
  st: Agent,
  this: JsVal,
  that_v: JsVal,
  locales: JsVal,
  options: JsVal,
) -> #(JsVal, Agent) {
  use Nil <- helpers.guard(!rt_val.is_nullish(this), fn() {
    rt_val.t_throw_type_error(
      st,
      "String.prototype.localeCompare called on null or undefined",
    )
  })
  let #(s, st) = rt_val.t_to_string(st, this)
  let #(that, st) = rt_val.t_to_string(st, that_v)
  let #(c, st) = collator_state(st, locales, options)
  #(mk_number(JInt(collator_compare(c, s, that))), st)
}

/// String.prototype.toLocale{Lower,Upper}Case — locale list is validated,
/// casing uses the default (root) algorithm.
fn host_locale_case(
  st: Agent,
  this: JsVal,
  locales: JsVal,
  upper: Bool,
) -> #(JsVal, Agent) {
  use Nil <- helpers.guard(!rt_val.is_nullish(this), fn() {
    rt_val.t_throw_type_error(st, "method called on null or undefined")
  })
  let #(s, st) = rt_val.t_to_string(st, this)
  let #(tag_list, st) = canonicalize_locale_list(st, locales)
  let lang = case tag_list {
    [first, ..] ->
      case tags.parse(first) {
        Ok(lid) -> string.lowercase(lid.language)
        Error(Nil) -> "en"
      }
    [] -> "en"
  }
  // Apply locale special casing first, then run the same Unicode Default Case
  // Conversion `String.prototype.toLowerCase` runs (final sigma etc. live in
  // `builtins/string`). Deliberately NOT a [[Get]] + [[Call]] of
  // `String.prototype.toLowerCase`: reassigning that property must not change
  // what `toLocaleLowerCase` returns.
  let pre = case lang {
    "tr" | "az" -> turkic_case(s, upper)
    "lt" -> lithuanian_case(s, upper)
    _ -> s
  }
  let cased = case upper {
    True -> string.uppercase(pre)
    False -> b_string.to_lower_case(pre)
  }
  #(mk_string(cased), st)
}

/// Turkish/Azeri dotted and dotless I special casing (pre-transform only —
/// the generic case conversion runs afterwards).
fn turkic_case(s: String, upper: Bool) -> String {
  case upper {
    True ->
      // i → İ (U+0130); the rest is handled by the default algorithm.
      string.to_graphemes(s)
      |> list.map(fn(g) {
        case g {
          "i" -> "İ"
          _ -> g
        }
      })
      |> string.join("")
    False -> {
      // İ → i; I → ı (U+0131); I + U+0307 → i.
      let cps =
        string.to_utf_codepoints(s) |> list.map(string.utf_codepoint_to_int)
      lower_turkic_cps(cps, [])
    }
  }
}

/// A single codepoint as a string; empty for a surrogate scalar value.
fn codepoint_str(c: Int) -> String {
  case string.utf_codepoint(c) {
    Ok(cp) -> string.from_utf_codepoints([cp])
    Error(Nil) -> ""
  }
}

fn lower_turkic_cps(cps: List(Int), acc: List(String)) -> String {
  case cps {
    [] -> string.join(list.reverse(acc), "")
    [0x130, ..rest] -> lower_turkic_cps(rest, ["i", ..acc])
    [0x49, 0x307, ..rest] -> lower_turkic_cps(rest, ["i", ..acc])
    [0x49, ..rest] -> lower_turkic_cps(rest, ["ı", ..acc])
    [c, ..rest] -> lower_turkic_cps(rest, [codepoint_str(c), ..acc])
  }
}

/// Lithuanian dot-above special casing.
fn lithuanian_case(s: String, upper: Bool) -> String {
  let cps = string.to_utf_codepoints(s) |> list.map(string.utf_codepoint_to_int)
  case upper {
    // Uppercasing removes U+0307 after i/j.
    True -> upper_lt_cps(cps, [])
    False -> lower_lt_cps(cps, [])
  }
}

fn upper_lt_cps(cps: List(Int), acc: List(String)) -> String {
  case cps {
    [] -> string.join(list.reverse(acc), "")
    [0x69, 0x307, ..rest] -> upper_lt_cps(rest, ["I", ..acc])
    [0x6a, 0x307, ..rest] -> upper_lt_cps(rest, ["J", ..acc])
    [0x12f, 0x307, ..rest] -> upper_lt_cps(rest, ["Į", ..acc])
    [c, ..rest] -> upper_lt_cps(rest, [codepoint_str(c), ..acc])
  }
}

fn lower_lt_cps(cps: List(Int), acc: List(String)) -> String {
  let is_mark = fn(c) { c >= 0x300 && c <= 0x36f && c != 0x307 }
  case cps {
    [] -> string.join(list.reverse(acc), "")
    // I/J followed by a combining mark keep an explicit dot above.
    [0x49, m, ..rest] ->
      case is_mark(m) {
        True -> lower_lt_cps(rest, [codepoint_str(m), "i\u{0307}", ..acc])
        False -> lower_lt_cps([m, ..rest], ["i", ..acc])
      }
    [0x4a, m, ..rest] ->
      case is_mark(m) {
        True -> lower_lt_cps(rest, [codepoint_str(m), "j\u{0307}", ..acc])
        False -> lower_lt_cps([m, ..rest], ["j", ..acc])
      }
    [0xcc, ..rest] -> lower_lt_cps(rest, ["i\u{0307}\u{0300}", ..acc])
    [0xcd, ..rest] -> lower_lt_cps(rest, ["i\u{0307}\u{0301}", ..acc])
    [0x128, ..rest] -> lower_lt_cps(rest, ["i\u{0307}\u{0303}", ..acc])
    [c, ..rest] -> lower_lt_cps(rest, [codepoint_str(c), ..acc])
  }
}

/// Date.prototype.toLocale{,Date,Time}String (ECMA-402 §17).
fn host_date_to_locale(
  st: Agent,
  this: JsVal,
  locales: JsVal,
  options: JsVal,
  required: DtfRequired,
) -> #(JsVal, Agent) {
  let not_date = fn() {
    rt_val.t_throw_type_error(st, "this is not a Date object")
  }
  let tv = case classify(this) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: DateObj(ms: tv), ..) -> tv
        _ -> not_date()
      }
    _ -> not_date()
  }
  let defaults = case required {
    DateOnly -> date_defaults()
    TimeOnly -> time_defaults()
    DateAndTime -> merge_components(date_defaults(), time_defaults())
  }
  let #(d, st) = dtf_state_required(st, locales, options, defaults, required)
  case tv {
    JInt(_) | JFloat(_) -> {
      let #(parts, st) = dtf_format_parts(st, d, mk_number(tv))
      #(mk_string(fmt.parts_to_string(parts)), st)
    }
    JNan | JPosInf | JNegInf -> #(mk_string("Invalid Date"), st)
  }
}

/// PluralRules select: operands come from the formatted digit strings.
fn plural_select(p: PluralRulesState, n: JsNum) -> fmt.PluralCategory {
  let finite = fn(f) {
    let opts =
      fmt.NumOpts(
        ..num_opts_from_plural(p),
        style: StyleDecimal,
        use_grouping: GroupingNever,
        sign_display: SignNever,
      )
    let #(int_digits, frac_digits) =
      fmt.plural_operands(fmt.format_number_parts(opts, f))
    fmt.plural_select_en(p.plural_type, int_digits, frac_digits)
  }
  case n {
    JInt(i) -> finite(int.to_float(i))
    JFloat(f) -> finite(f)
    // NaN/Infinity have no operands.
    JNan | JPosInf | JNegInf -> fmt.PcOther
  }
}

/// RelativeTimeFormat format/formatToParts core.
fn rtf_method_parts(
  st: Agent,
  r: RelativeTimeFormatState,
  value_v: JsVal,
  unit_v: JsVal,
) -> #(List(fmt.UnitPart), Agent) {
  let #(n, st) = rt_val.t_to_number(st, value_v)
  let f = case n {
    JInt(i) -> int.to_float(i)
    JFloat(f) -> f
    JNan | JPosInf | JNegInf ->
      rt_val.t_throw_range_error(st, "Value need to be finite number")
  }
  let #(unit_str, st) = rt_val.t_to_string(st, unit_v)
  let unit = case singular_unit(unit_str) {
    Some(u) -> u
    None ->
      rt_val.t_throw_range_error(st, "Invalid unit argument: " <> unit_str)
  }
  let abs_opts = fmt.NumOpts(..fmt.default_num_opts(), sign_display: SignNever)
  let value_parts = fmt.format_number_parts(abs_opts, float.absolute_value(f))
  let value_parts =
    fmt.apply_numbering_system(
      value_parts,
      r.numbering_system,
      fmt.is_number_digit,
    )
  #(fmt.rtf_parts_en(r.style, r.numeric, f, unit, value_parts), st)
}

fn singular_unit(unit: String) -> Option(String) {
  let u = case string.ends_with(unit, "s") {
    True -> string.slice(unit, 0, string.length(unit) - 1)
    False -> unit
  }
  case
    list.contains(
      ["year", "quarter", "month", "week", "day", "hour", "minute", "second"],
      u,
    )
  {
    True -> Some(u)
    False -> None
  }
}

/// StringListFromIterable (§13.5.1) — undefined → empty list.
fn string_list_from_iterable(
  st: Agent,
  iterable: JsVal,
) -> #(List(String), Agent) {
  case classify(iterable) {
    KUndef -> #([], st)
    // Strings iterate by code points (String.prototype[Symbol.iterator]).
    KStr(str) -> {
      let items =
        string.to_utf_codepoints(str)
        |> list.map(fn(cp) { string.from_utf_codepoints([cp]) })
      #(items, st)
    }
    _ -> {
      let #(method, st) =
        rt_obj.t_get_prop(st, iterable, SymbolKey(types.symbol_iterator))
      use Nil <- helpers.guard(rt_call.is_callable(st, method), fn() {
        rt_val.t_throw_type_error(st, "object is not iterable")
      })
      let #(iter, st) = rt_call.t_call_checked(st, method, iterable, [])
      use Nil <- helpers.guard(rt_val.is_object(iter), fn() {
        rt_val.t_throw_type_error(st, "iterator result is not an object")
      })
      let #(next_fn, st) = rt_obj.t_get_prop(st, iter, StringKey(Named("next")))
      iterate_strings(st, iter, next_fn, [])
    }
  }
}

fn iterate_strings(
  st: Agent,
  iter: JsVal,
  next_fn: JsVal,
  acc: List(String),
) -> #(List(String), Agent) {
  let #(step, st) = rt_call.t_call_checked(st, next_fn, iter, [])
  use Nil <- helpers.guard(rt_val.is_object(step), fn() {
    rt_val.t_throw_type_error(st, "iterator result is not an object")
  })
  let #(done, st) = rt_obj.t_get_prop(st, step, StringKey(Named("done")))
  case rt_val.to_boolean(done) {
    True -> #(list.reverse(acc), st)
    False -> {
      let #(v, st) = rt_obj.t_get_prop(st, step, StringKey(Named("value")))
      case classify(v) {
        KStr(s) -> iterate_strings(st, iter, next_fn, [s, ..acc])
        _ ->
          rt_val.t_throw_type_error(
            st,
            "Iterable yielded a value that is not a string",
          )
      }
    }
  }
}

/// Intl.DisplayNames.prototype.of(code)
fn display_names_of(
  st: Agent,
  dn: DisplayNamesState,
  code_v: JsVal,
) -> #(JsVal, Agent) {
  let #(code, st) = rt_val.t_to_string(st, code_v)
  let type_ = dn.display_type
  let fallback = dn.fallback
  let #(canonical, name) = case type_ {
    DnLanguage ->
      case tags.parse(code) {
        Ok(lid) ->
          // Must match unicode_language_id: no extensions/private use.
          case lid.extensions, lid.private_use {
            [], [] -> {
              let tag = tags.to_string(tags.canonicalize(lid))
              #(tag, fmt.language_display_name(tag))
            }
            _, _ ->
              rt_val.t_throw_range_error(st, "invalid language code: " <> code)
          }
        Error(Nil) ->
          rt_val.t_throw_range_error(st, "invalid language code: " <> code)
      }
    DnRegion ->
      case tags.is_region(code) {
        True -> {
          let r = string.uppercase(code)
          #(r, fmt.region_display_name(r))
        }
        False -> rt_val.t_throw_range_error(st, "invalid region code: " <> code)
      }
    DnScript ->
      case tags.is_script(code) {
        True -> {
          let s = tags.titlecase(code)
          #(s, fmt.script_display_name(s))
        }
        False -> rt_val.t_throw_range_error(st, "invalid script code: " <> code)
      }
    DnCurrency ->
      case tags.is_alpha(code) && string.length(code) == 3 {
        True -> {
          let c = string.uppercase(code)
          #(c, fmt.currency_display_name(c))
        }
        False ->
          rt_val.t_throw_range_error(st, "invalid currency code: " <> code)
      }
    DnCalendar ->
      case is_type_sequence(string.lowercase(code)) {
        True -> {
          let c = string.lowercase(code)
          let name = case c {
            "gregory" -> Some("Gregorian Calendar")
            "iso8601" -> Some("ISO-8601 Calendar")
            _ -> None
          }
          #(c, name)
        }
        False ->
          rt_val.t_throw_range_error(st, "invalid calendar code: " <> code)
      }
    DnDateTimeField ->
      case
        list.contains(
          [
            "era", "year", "quarter", "month", "weekOfYear", "weekday", "day",
            "dayPeriod", "hour", "minute", "second", "timeZoneName",
          ],
          code,
        )
      {
        True -> {
          let name = case code {
            "weekOfYear" -> "week"
            "weekday" -> "day of the week"
            "dayPeriod" -> "AM/PM"
            "timeZoneName" -> "time zone"
            other -> other
          }
          #(code, Some(name))
        }
        False ->
          rt_val.t_throw_range_error(st, "invalid dateTimeField code: " <> code)
      }
  }
  case name, fallback {
    Some(n), _ -> #(mk_string(n), st)
    None, FbCode -> #(mk_string(canonical), st)
    None, FbNone -> #(mk_undefined(), st)
  }
}

// ============================================================================
// DurationFormat formatting
// ============================================================================

fn duration_parts(
  st: Agent,
  df: DurationFormatState,
  duration_v: JsVal,
) -> #(List(fmt.UnitPart), Agent) {
  let #(fields, st) = to_duration_record(st, duration_v)
  // DurationSign consistency + IsValidDuration ranges.
  let values = duration_values(fields)
  let has_neg = list.any(values, fn(v) { v <. 0.0 })
  let has_pos = list.any(values, fn(v) { v >. 0.0 })
  let st = case has_neg && has_pos {
    True ->
      rt_val.t_throw_range_error(
        st,
        "Duration fields must have consistent sign",
      )
    False -> st
  }
  let st = case is_valid_duration(fields) {
    True -> st
    False ->
      rt_val.t_throw_range_error(st, "Duration field value is out of range")
  }
  #(build_duration_parts(df, fields), st)
}

/// ToDurationRecord (object) / Temporal duration string parsing.
fn to_duration_record(
  st: Agent,
  duration_v: JsVal,
) -> #(DurationRecord, Agent) {
  case classify(duration_v) {
    KStr(str) ->
      case parse_iso_duration(str) {
        Ok(fields) -> #(fields, st)
        Error(Nil) ->
          rt_val.t_throw_range_error(st, "Invalid duration string: " <> str)
      }
    KHandle(_) -> {
      let #(fields, st, any_defined) =
        list.fold(duration_units, #(zero_duration, st, False), fn(acc, unit) {
          let #(fields, st, any) = acc
          let name = duration_unit_js_name(unit)
          let #(v, st) =
            rt_obj.t_get_prop(st, duration_v, StringKey(Named(name)))
          case classify(v) {
            KUndef -> #(fields, st, any)
            _ -> {
              let #(n, st) = rt_val.t_to_number(st, v)
              case n {
                JInt(i) -> #(
                  set_duration_field(fields, unit, int.to_float(i)),
                  st,
                  True,
                )
                JFloat(f) ->
                  case f == float.floor(f) {
                    True -> #(set_duration_field(fields, unit, f), st, True)
                    False ->
                      rt_val.t_throw_range_error(
                        st,
                        name <> " must be an integral number",
                      )
                  }
                JNan | JPosInf | JNegInf ->
                  rt_val.t_throw_range_error(
                    st,
                    name <> " must be a finite number",
                  )
              }
            }
          }
        })
      case any_defined {
        True -> #(fields, st)
        False -> rt_val.t_throw_range_error(st, "Invalid duration object")
      }
    }
    _ -> rt_val.t_throw_type_error(st, "Duration must be an object or string")
  }
}

/// IsValidDuration: calendar units < 2^32; total time < 2^53 seconds.
fn is_valid_duration(d: DurationRecord) -> Bool {
  let cal_ok =
    list.all([d.years, d.months, d.weeks], fn(v) {
      float.absolute_value(v) <. 4_294_967_296.0
    })
  let total_seconds =
    d.days
    *. 86_400.0
    +. d.hours
    *. 3600.0
    +. d.minutes
    *. 60.0
    +. d.seconds
    +. d.milliseconds
    /. 1000.0
    +. d.microseconds
    /. 1_000_000.0
    +. d.nanoseconds
    /. 1_000_000_000.0
  cal_ok && float.absolute_value(total_seconds) <. 9_007_199_254_740_992.0
}

/// Parse a Temporal ISO 8601 duration string: [+-]PnYnMnWnDTnHnMnS.
fn parse_iso_duration(str: String) -> Result(DurationRecord, Nil) {
  let trimmed = string.trim(str)
  let #(sign, rest) = case string.pop_grapheme(trimmed) {
    Ok(#("-", r)) -> #(-1.0, r)
    Ok(#("\u{2212}", r)) -> #(-1.0, r)
    Ok(#("+", r)) -> #(1.0, r)
    _ -> #(1.0, trimmed)
  }
  use rest <- result.try(case string.pop_grapheme(rest) {
    Ok(#("P", r)) | Ok(#("p", r)) -> Ok(r)
    _ -> Error(Nil)
  })
  let #(date_part, time_part) = case string.split_once(rest, "T") {
    Ok(#(d, t)) -> #(d, Some(t))
    Error(Nil) ->
      case string.split_once(rest, "t") {
        Ok(#(d, t)) -> #(d, Some(t))
        Error(Nil) -> #(rest, None)
      }
  }
  use date_fields <- result.try(parse_duration_section(
    date_part,
    [#("Y", DuYears), #("M", DuMonths), #("W", DuWeeks), #("D", DuDays)],
    False,
  ))
  use time_fields <- result.try(case time_part {
    None -> Ok([])
    Some("") -> Error(Nil)
    Some(t) ->
      parse_duration_section(
        t,
        [#("H", DuHours), #("M", DuMinutes), #("S", DuSeconds)],
        True,
      )
  })
  let all = list.append(date_fields, time_fields)
  case all {
    [] -> Error(Nil)
    _ -> {
      let parsed =
        list.fold(all, zero_duration, fn(acc, kv) {
          set_duration_field(acc, kv.0, kv.1)
        })
      // Split fractional seconds into ms/us/ns.
      let whole = float.truncate(parsed.seconds) |> int.to_float
      let frac = parsed.seconds -. whole
      let ns_total = float.round(frac *. 1_000_000_000.0)
      let ms = ns_total / 1_000_000
      let us = { ns_total % 1_000_000 } / 1000
      let ns = ns_total % 1000
      let signed = fn(v: Float) { sign *. v }
      Ok(DurationRecord(
        years: signed(parsed.years),
        months: signed(parsed.months),
        weeks: signed(parsed.weeks),
        days: signed(parsed.days),
        hours: signed(parsed.hours),
        minutes: signed(parsed.minutes),
        seconds: signed(whole),
        milliseconds: signed(int.to_float(ms)),
        microseconds: signed(int.to_float(us)),
        nanoseconds: signed(int.to_float(ns)),
      ))
    }
  }
}

/// Parse "3Y2M..." style segments in designator order.
fn parse_duration_section(
  part: String,
  designators: List(#(String, DurationUnit)),
  allow_fraction: Bool,
) -> Result(List(#(DurationUnit, Float)), Nil) {
  case part {
    "" -> Ok([])
    _ ->
      parse_duration_loop(
        string.to_graphemes(part),
        designators,
        allow_fraction,
        "",
        [],
      )
  }
}

fn parse_duration_loop(
  gs: List(String),
  designators: List(#(String, DurationUnit)),
  allow_fraction: Bool,
  num_acc: String,
  out: List(#(DurationUnit, Float)),
) -> Result(List(#(DurationUnit, Float)), Nil) {
  case gs {
    [] ->
      case num_acc {
        "" -> Ok(list.reverse(out))
        _ -> Error(Nil)
      }
    [g, ..rest] -> {
      let is_num = case g {
        "." | "," -> True
        _ ->
          case int.parse(g) {
            Ok(_) -> True
            Error(Nil) -> False
          }
      }
      case is_num {
        True ->
          parse_duration_loop(
            rest,
            designators,
            allow_fraction,
            num_acc <> g,
            out,
          )
        False -> {
          let upper = string.uppercase(g)
          use #(field, remaining) <- result.try(take_designator(
            designators,
            upper,
          ))
          let normalized = string.replace(num_acc, ",", ".")
          let has_fraction = string.contains(normalized, ".")
          case num_acc == "" || has_fraction && !allow_fraction {
            True -> Error(Nil)
            False -> {
              use v <- result.try(parse_duration_number(normalized))
              parse_duration_loop(rest, remaining, allow_fraction, "", [
                #(field, v),
                ..out
              ])
            }
          }
        }
      }
    }
  }
}

/// Designators must appear in order; consuming one drops the earlier ones.
fn take_designator(
  designators: List(#(String, DurationUnit)),
  d: String,
) -> Result(#(DurationUnit, List(#(String, DurationUnit))), Nil) {
  case designators {
    [] -> Error(Nil)
    [#(key, field), ..rest] ->
      case key == d {
        True -> Ok(#(field, rest))
        False -> take_designator(rest, d)
      }
  }
}

fn parse_duration_number(s: String) -> Result(Float, Nil) {
  // The integer fallback must go through num_from_int: a bare int.to_float
  // on an arbitrary-precision int (e.g. a 400-digit duration component)
  // raises an uncatchable erlang:float/1 badarg. Out-of-range values
  // saturate to ±Infinity, which is not a valid duration field, so treat
  // them as a parse failure (the caller surfaces a RangeError).
  float.parse(s)
  |> result.lazy_or(fn() {
    int.parse(s)
    |> result.try(fn(n) {
      case rt_val.num_from_int(n) {
        JFloat(f) -> Ok(f)
        JInt(i) -> Ok(int.to_float(i))
        JNan | JPosInf | JNegInf -> Error(Nil)
      }
    })
  })
}

/// PartitionDurationFormatPattern — mirrors ECMA-402 Intl.DurationFormat §1.1.7.
fn build_duration_parts(
  df: DurationFormatState,
  fields: DurationRecord,
) -> List(fmt.UnitPart) {
  let nu = df.numbering_system
  let base_style = df.style
  let frac_digits = df.fractional_digits
  let overall_negative = list.any(duration_values(fields), fn(v) { v <. 0.0 })
  // The style of the next-smaller sub-second unit (seconds → ms → us → ns);
  // `None` for the units that have no such successor.
  let next_style_of = fn(unit) {
    case unit {
      DuSeconds -> Some(df.milliseconds.style)
      DuMilliseconds -> Some(df.microseconds.style)
      DuMicroseconds -> Some(df.nanoseconds.style)
      _other -> None
    }
  }
  // Iterate units building groups; numeric units join via ":" separators.
  let init = #([], False, True, False)
  let #(groups_rev, _need_sep, _display_neg, _done) =
    list.fold(duration_unit_list(df), init, fn(acc, entry) {
      let #(unit, unit_opts) = entry
      let #(groups, need_sep, display_neg, done) = acc
      case done {
        True -> acc
        False -> {
          let style = unit_opts.style
          let display = unit_opts.display
          let raw_value = duration_field(fields, unit) +. 0.0
          // Combine sub-second units when the next unit is numeric — only
          // seconds/milliseconds/microseconds have such a next unit at all.
          let combine = case next_style_of(unit) {
            Some(next_style) -> folds_into_fraction(next_style)
            None -> False
          }
          let #(value_repr, is_zero, this_done, frac_precision, trunc_mode) = case
            combine
          {
            True -> {
              let #(repr, zero) = duration_fractional_value(fields, unit)
              #(
                repr,
                zero,
                True,
                fmt.Precision(
                  min: option.unwrap(frac_digits, 0),
                  max: option.unwrap(frac_digits, 9),
                ),
                True,
              )
            }
            // Not folded into a fraction: an integral count of this unit.
            False -> #(
              FloatValue(raw_value),
              raw_value == 0.0,
              False,
              fmt.Precision(min: 0, max: 0),
              False,
            )
          }
          // Display zero numeric minutes when seconds follow.
          let display_required = case unit == DuMinutes && need_sep {
            True ->
              df.seconds.display == DisplayAlways
              || duration_field(fields, DuSeconds) != 0.0
              || duration_field(fields, DuMilliseconds) != 0.0
              || duration_field(fields, DuMicroseconds) != 0.0
              || duration_field(fields, DuNanoseconds) != 0.0
            False -> False
          }
          let show = !is_zero || display == DisplayAlways || display_required
          case show {
            False -> #(groups, need_sep, display_neg, this_done)
            True -> {
              // Only the first displayed value carries the sign.
              let #(sign_display, value_repr, display_neg) = case display_neg {
                True -> {
                  let value_repr = case is_zero && overall_negative {
                    True -> FloatValue(-1.0 *. 0.0)
                    False -> value_repr
                  }
                  #(SignAuto, value_repr, False)
                }
                False -> #(SignNever, value_repr, False)
              }
              let numeric_style = is_numeric_style(style)
              let opts =
                fmt.NumOpts(
                  ..fmt.default_num_opts(),
                  sign_display: sign_display,
                  min_int: case style {
                    DurTwoDigit -> 2
                    DurLong
                    | DurShort
                    | DurNarrow
                    | DurNumeric
                    | DurFractional -> 1
                  },
                  use_grouping: case numeric_style {
                    True -> GroupingNever
                    False -> GroupingAuto
                  },
                  frac: Some(frac_precision),
                  rounding_mode: case trunc_mode {
                    True -> RoundTrunc
                    False -> RoundHalfExpand
                  },
                  style: case numeric_style {
                    True -> StyleDecimal
                    // DurationFormat unit styles are long/short/narrow here
                    // (the numeric styles took the branch above).
                    False ->
                      StyleUnit(
                        unit: duration_unit_singular(unit),
                        display: unit_display_from_duration_style(style),
                      )
                  },
                )
              let parts = case value_repr {
                FloatValue(f) -> fmt.format_number_parts(opts, f)
                DecValue(str) -> fmt.format_decimal_string_parts(opts, str)
              }
              let unit_tag = duration_unit_singular(unit)
              let parts =
                fmt.apply_numbering_system(parts, nu, fmt.is_number_digit)
                |> list.map(fn(part: fmt.Part) {
                  case part.0 {
                    PLiteral -> fmt.UnitPart(part.0, part.1, None)
                    _ -> fmt.UnitPart(part.0, part.1, Some(unit_tag))
                  }
                })
              case need_sep {
                True ->
                  // Join onto the previous numeric group with ":".
                  case groups {
                    [last, ..earlier] -> #(
                      [
                        list.flatten([
                          last,
                          [fmt.UnitPart(PLiteral, ":", None)],
                          parts,
                        ]),
                        ..earlier
                      ],
                      need_sep,
                      display_neg,
                      this_done,
                    )
                    [] -> #([parts], need_sep, display_neg, this_done)
                  }
                False -> #(
                  [parts, ..groups],
                  numeric_style,
                  display_neg,
                  this_done,
                )
              }
            }
          }
        }
      }
    })
  let groups = list.reverse(groups_rev)
  let strings = list.map(groups, fmt.unit_parts_to_string)
  // Re-expand element parts so formatToParts keeps the numeric structure.
  let lf_parts =
    fmt.list_format_parts(UnitList, duration_list_style(base_style), strings)
  expand_list_elements(lf_parts, groups, [])
}

type DurationValue {
  FloatValue(Float)
  DecValue(String)
}

/// Whether the *next* sub-second unit's style makes this unit fold its value
/// into a fraction. `DurFractional` is the internal style GetDurationUnitOptions
/// folds a numeric sub-second unit into; both spell "numeric" publicly.
fn folds_into_fraction(style: DurationUnitStyle) -> Bool {
  case style {
    DurNumeric | DurFractional -> True
    DurLong | DurShort | DurNarrow | DurTwoDigit -> False
  }
}

/// A DurationFormat per-unit non-numeric style (long/short/narrow) as the
/// NumberFormat unitDisplay it renders with.
fn unit_display_from_duration_style(style: DurationUnitStyle) -> UnitDisplay {
  case style {
    DurLong -> UnitLong
    DurNarrow -> UnitNarrow
    DurShort | DurNumeric | DurTwoDigit | DurFractional -> UnitShort
  }
}

/// durationToFractional: exact decimal string for combined sub-second units.
fn duration_fractional_value(
  fields: DurationRecord,
  unit: DurationUnit,
) -> #(DurationValue, Bool) {
  let get = fn(u) { duration_field(fields, u) |> float.truncate }
  let #(exponent, components) = case unit {
    DuSeconds -> #(9, [
      #(get(DuSeconds), 1_000_000_000),
      #(get(DuMilliseconds), 1_000_000),
      #(get(DuMicroseconds), 1000),
      #(get(DuNanoseconds), 1),
    ])
    DuMilliseconds -> #(6, [
      #(get(DuMilliseconds), 1_000_000),
      #(get(DuMicroseconds), 1000),
      #(get(DuNanoseconds), 1),
    ])
    _other -> #(3, [#(get(DuMicroseconds), 1000), #(get(DuNanoseconds), 1)])
  }
  let total = list.fold(components, 0, fn(acc, c) { acc + c.0 * c.1 })
  let e = pow10_i(exponent)
  let q = total / e
  let r = int.absolute_value(total % e)
  let zero = total == 0
  case r == 0 {
    True -> #(FloatValue(int.to_float(q)), zero)
    False -> {
      let sign = case total < 0 {
        True -> "-"
        False -> ""
      }
      let r_str = string.pad_start(int.to_string(r), exponent, "0")
      #(
        DecValue(sign <> int.to_string(int.absolute_value(q)) <> "." <> r_str),
        zero,
      )
    }
  }
}

fn pow10_i(e: Int) -> Int {
  case e <= 0 {
    True -> 1
    False -> 10 * pow10_i(e - 1)
  }
}

/// Substitute "element" parts from ListFormat with the group's real parts.
fn expand_list_elements(
  lf_parts: List(fmt.Part),
  groups: List(List(fmt.UnitPart)),
  acc: List(List(fmt.UnitPart)),
) -> List(fmt.UnitPart) {
  case lf_parts {
    [] -> list.flatten(list.reverse(acc))
    [#(PElement, _), ..rest] ->
      case groups {
        [g, ..gs] -> expand_list_elements(rest, gs, [g, ..acc])
        [] -> expand_list_elements(rest, [], acc)
      }
    [#(t, v), ..rest] ->
      expand_list_elements(rest, groups, [[fmt.UnitPart(t, v, None)], ..acc])
  }
}

// ============================================================================
// Segmenter methods
// ============================================================================

fn segmenter_segment(
  st: Agent,
  segments_proto: Handle,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let name = "Intl.Segmenter.prototype.segment"
  let #(_h, sg, _bound) = branded_segmenter(st, this, name)
  let #(s, st) = rt_val.t_to_string(st, first_arg_or_undefined(args))
  let data = SegmentsData(SegmentsState(string: s, granularity: sg.granularity))
  let #(h, st) =
    realm_ops.alloc_wrapper(st, IntlObj(data:, bound: None), segments_proto)
  #(mk_object(h), st)
}

fn segments_iterator(
  st: Agent,
  iter_proto: Handle,
  this: JsVal,
) -> #(JsVal, Agent) {
  let name = "%Segments.prototype%[Symbol.iterator]"
  let #(_h, sg, _bound) = branded_segments(st, this, name)
  let data =
    SegmentIteratorData(SegmentIteratorState(
      string: sg.string,
      granularity: sg.granularity,
      remaining: seg.segment_string(sg.string, sg.granularity),
    ))
  let #(h, st) =
    realm_ops.alloc_wrapper(st, IntlObj(data:, bound: None), iter_proto)
  #(mk_object(h), st)
}

fn make_segment_data(
  st: Agent,
  input: String,
  granularity: Granularity,
  seg: Segment,
) -> #(JsVal, Agent) {
  let base = [
    #("segment", mk_string(seg.text)),
    #("index", mk_number(JInt(seg.index))),
    #("input", mk_string(input)),
  ]
  let props = case granularity {
    GWord -> list.append(base, [#("isWordLike", mk_bool(seg.word_like))])
    GGrapheme | GSentence -> base
  }
  alloc_pojo(st, props)
}

fn segments_containing(
  st: Agent,
  sg: SegmentsState,
  index_v: JsVal,
) -> #(JsVal, Agent) {
  let input = sg.string
  let granularity = sg.granularity
  let #(n, st) = rt_val.t_to_number(st, index_v)
  let segments = seg.segment_string(input, granularity)
  let total = seg.utf16_len(input)
  // ToIntegerOrInfinity: NaN is 0; ±Infinity fall outside [0, total).
  let idx = case n {
    JInt(i) -> i
    JFloat(f) -> float.truncate(f)
    JNan -> 0
    JPosInf -> total
    JNegInf -> -1
  }
  case idx < 0 || idx >= total {
    True -> #(mk_undefined(), st)
    False -> {
      let found =
        list.fold(segments, None, fn(acc, seg: Segment) {
          case seg.index <= idx {
            True -> Some(seg)
            False -> acc
          }
        })
      case found {
        Some(seg) -> make_segment_data(st, input, granularity, seg)
        None -> #(mk_undefined(), st)
      }
    }
  }
}

fn segment_iterator_next(
  st: Agent,
  h: Handle,
  it: SegmentIteratorState,
) -> #(JsVal, Agent) {
  case it.remaining {
    [] -> realm_ops.alloc_iter_result(st, mk_undefined(), True)
    [seg, ..rest] -> {
      let st =
        write_intl_data(
          st,
          h,
          SegmentIteratorData(SegmentIteratorState(..it, remaining: rest)),
        )
      let #(data, st) = make_segment_data(st, it.string, it.granularity, seg)
      realm_ops.alloc_iter_result(st, data, False)
    }
  }
}

// ============================================================================
// Intl.Locale getters & methods
// ============================================================================

/// Parse a Locale instance's canonical tag into its LocaleId, if well formed.
fn locale_lid(l: LocaleState) -> Option(tags.LocaleId) {
  case tags.parse(l.locale) {
    Ok(lid) -> Some(lid)
    Error(Nil) -> None
  }
}

fn locale_u_kw(l: LocaleState, key: String) -> Option(String) {
  use lid <- option.then(locale_lid(l))
  lid_u_keywords(lid)
  |> list.key_find(key)
  |> option.from_result
}

fn locale_getter(
  st: Agent,
  name: LocaleGetterName,
  this: JsVal,
) -> #(JsVal, Agent) {
  let #(_h, l, _bound) =
    branded_locale(
      st,
      this,
      "Intl.Locale.prototype." <> locale_getter_js_name(name),
    )
  let lid = locale_lid(l)
  let kw = fn(key) {
    locale_u_kw(l, key)
    |> option.map(mk_string)
    |> option.unwrap(mk_undefined())
  }
  let v = case name {
    LocaleBaseName ->
      case lid {
        Some(l) -> mk_string(tags.base_name(l))
        None -> mk_undefined()
      }
    LocaleLanguage ->
      case lid {
        Some(l) -> mk_string(string.lowercase(l.language))
        None -> mk_undefined()
      }
    LocaleScript ->
      case lid {
        Some(tags.LocaleId(script: Some(s), ..)) -> mk_string(tags.titlecase(s))
        _ -> mk_undefined()
      }
    LocaleRegion ->
      case lid {
        Some(tags.LocaleId(region: Some(r), ..)) ->
          mk_string(string.uppercase(r))
        _ -> mk_undefined()
      }
    LocaleCalendar -> kw("ca")
    LocaleCollation -> kw("co")
    LocaleHourCycle -> kw("hc")
    LocaleNumberingSystem -> kw("nu")
    LocaleCaseFirst -> kw("kf")
    LocaleNumeric ->
      case locale_u_kw(l, "kn") {
        Some("") | Some("true") -> mk_bool(True)
        Some(_) -> mk_bool(False)
        None -> mk_bool(False)
      }
    LocaleFirstDayOfWeek -> kw("fw")
    LocaleVariants ->
      case lid {
        Some(tags.LocaleId(variants: [_, ..] as vs, ..)) ->
          mk_string(string.join(vs, "-"))
        _ -> mk_undefined()
      }
  }
  #(v, st)
}

fn locale_method(
  st: Agent,
  method: LocaleMethodName,
  proto: Handle,
  this: JsVal,
) -> #(JsVal, Agent) {
  let #(_h, l, _bound) =
    branded_locale(
      st,
      this,
      "Intl.Locale.prototype." <> locale_method_js_name(method),
    )
  let tag = l.locale
  let strings = fn(vals: List(String)) {
    alloc_array(st, list.map(vals, mk_string))
  }
  case method {
    LocaleToString -> #(mk_string(tag), st)
    LocaleMaximize | LocaleMinimize -> {
      let new_tag = case tags.parse(tag) {
        Ok(lid) ->
          case method {
            LocaleMaximize -> tags.to_string(tags.maximize(lid))
            _ -> tags.to_string(tags.minimize(lid))
          }
        Error(Nil) -> tag
      }
      let data = LocaleData(LocaleState(locale: new_tag))
      let #(h, st) =
        realm_ops.alloc_wrapper(st, IntlObj(data:, bound: None), proto)
      #(mk_object(h), st)
    }
    LocaleGetCalendars ->
      strings(case locale_u_kw(l, "ca") {
        Some(ca) -> [ca]
        None -> ["gregory"]
      })
    LocaleGetCollations ->
      strings(case locale_u_kw(l, "co") {
        Some(co) -> [co]
        None -> ["emoji", "eor"]
      })
    LocaleGetHourCycles ->
      strings(case locale_u_kw(l, "hc") {
        Some(hc) -> [hc]
        None -> ["h12"]
      })
    LocaleGetNumberingSystems ->
      strings(case locale_u_kw(l, "nu") {
        Some(nu) -> [nu]
        None -> ["latn"]
      })
    LocaleGetTimeZones ->
      case locale_lid(l) {
        Some(tags.LocaleId(region: Some(r), ..)) ->
          strings(case string.uppercase(r) {
            "US" -> ["America/New_York"]
            "GB" -> ["Europe/London"]
            "DE" -> ["Europe/Berlin"]
            "FR" -> ["Europe/Paris"]
            "JP" -> ["Asia/Tokyo"]
            "CN" -> ["Asia/Shanghai"]
            _ -> []
          })
        _ -> #(mk_undefined(), st)
      }
    LocaleGetTextInfo -> {
      let lang = case locale_lid(l) {
        Some(l) -> string.lowercase(l.language)
        None -> "en"
      }
      let dir = case list.contains(["ar", "he", "fa", "ur", "ps", "yi"], lang) {
        True -> "rtl"
        False -> "ltr"
      }
      alloc_pojo(st, [#("direction", mk_string(dir))])
    }
    LocaleGetWeekInfo -> {
      let #(weekend, st) =
        alloc_array(st, [mk_number(JInt(6)), mk_number(JInt(7))])
      let first_day = case locale_u_kw(l, "fw") {
        Some("mon") -> 1
        Some("tue") -> 2
        Some("wed") -> 3
        Some("thu") -> 4
        Some("fri") -> 5
        Some("sat") -> 6
        Some("sun") -> 7
        _ -> 7
      }
      alloc_pojo(st, [
        #("firstDay", mk_number(JInt(first_day))),
        #("weekend", weekend),
      ])
    }
  }
}
