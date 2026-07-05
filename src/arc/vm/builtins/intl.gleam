//// The Intl namespace (ECMA-402): Intl.getCanonicalLocales,
//// Intl.supportedValuesOf, and the service constructors
//// (Locale, Collator, NumberFormat, DateTimeFormat, PluralRules, ListFormat,
//// RelativeTimeFormat, Segmenter, DisplayNames, DurationFormat).
////
//// Locale data is root/English: formatters implement en/en-US CLDR patterns
//// (see intl_format.gleam); tag parsing/canonicalization is in
//// intl_locale.gleam.

import arc/internal/digits
import arc/internal/gregorian.{days_from_civil, floor_div}
import arc/internal/host_time
import arc/vm/builtins/common
import arc/vm/builtins/helpers.{first_arg_or_undefined}
import arc/vm/builtins/intl_collate.{collator_compare}
import arc/vm/builtins/intl_format.{
  PDay, PDayPeriod, PElement, PEra, PFractionalSecond, PHour, PLiteral, PMinute,
  PMonth, PSecond, PTimeZoneName, PWeekday, PYear,
} as fmt
import arc/vm/builtins/intl_locale as tags
import arc/vm/builtins/temporal_tz
import arc/vm/heap
import arc/vm/internal/temporal_calendar as tcal
import arc/vm/key.{Index, Named}
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State, State}
import arc/vm/unicode_case
import arc/vm/value.{
  type BoundGetterService, type CollatorState, type ConstructibleService,
  type DateStyle, type DateTimeFormatState, type DisplayNamesState,
  type DtfComponent, type DtfComponents, type DtfTimeZone,
  type DurationBaseStyle, type DurationFormatState, type DurationUnitOptions,
  type DurationUnitStyle, type Granularity, type HostOverride, type HourCycle,
  type IntlData, type IntlDigitOptions, type IntlMethodName, type IntlNativeFn,
  type IntlService, type JsValue, type ListFormatState, type ListFormatStyle,
  type LocaleGetterName, type LocaleMethodName, type LocaleState,
  type MonthWidth, type NameWidth, type Notation, type NumberFormatState,
  type NumericWidth, type PluralRulesState, type Ref,
  type RelativeTimeFormatState, type SegmentIteratorState, type SegmenterState,
  type SegmentsState, type TimeStyle, type TimeZoneNameWidth, BgCollator,
  BgDateTimeFormat, BgNumberFormat, BigIntToLocaleString, BsDigital, BsLong,
  BsNarrow, BsShort, Cardinal, CaseFirstFalse, CaseFirstLower, CaseFirstUpper,
  CollatorData, CollatorState, CompactLong, CompactShort, Conjunction,
  CsCollator, CsDateTimeFormat, CsDisplayNames, CsDurationFormat, CsListFormat,
  CsLocale, CsNumberFormat, CsPluralRules, CsRelativeTimeFormat, CsSegmenter,
  CurAccounting, CurCode, CurName, CurNarrowSymbol, CurStandard, CurSymbol,
  DateTimeFormatData, DateTimeFormatState, DateToLocaleDateString,
  DateToLocaleString, DateToLocaleTimeString, Disjunction, Dispatch,
  DisplayAlways, DisplayAuto, DisplayNamesData, DisplayNamesState, DnCalendar,
  DnCurrency, DnDateTimeField, DnLanguage, DnRegion, DnScript, DsFull, DsLong,
  DsMedium, DsShort, DtfComponents, DtfDay, DtfDayPeriod, DtfEra,
  DtfFractionalSecondDigits, DtfHour, DtfMinute, DtfMonth, DtfSecond,
  DtfTimeZoneName, DtfWeekday, DtfYear, DurFractional, DurLong, DurNarrow,
  DurNumeric, DurShort, DurTwoDigit, DurationFormatData, DurationFormatState,
  DurationUnitOptions, FbCode, FbNone, FixedZone, GGrapheme, GSentence, GWord,
  GroupingAlways, GroupingAuto, GroupingMin2, GroupingNever, H11, H12, H23, H24,
  HostZone, IntlBoundGetter, IntlBoundMethod, IntlCollator, IntlConstructor,
  IntlDateTimeFormat, IntlDigitOptions, IntlDisplayNames, IntlDurationFormat,
  IntlFormat, IntlFormatRange, IntlFormatRangeToParts, IntlFormatToParts,
  IntlGetCanonicalLocales, IntlHostOverride, IntlListFormat, IntlLocale,
  IntlLocaleGetter, IntlLocaleMethod, IntlMethod, IntlNative, IntlNumberFormat,
  IntlObject, IntlOf, IntlPluralRules, IntlRelativeTimeFormat,
  IntlResolvedOptions, IntlSegmentIterator, IntlSegmentIteratorNext,
  IntlSegmenter, IntlSegmenterSegment, IntlSegments, IntlSegmentsContaining,
  IntlSegmentsIterator, IntlSelect, IntlSelectRange, IntlSupportedLocalesOf,
  IntlSupportedValuesOf, JsBool, JsNumber, JsObject, JsString, JsUndefined,
  LLong, LNarrow, LShort, LdDialect, LdStandard, ListFormatData, ListFormatState,
  LocaleBaseName, LocaleCalendar, LocaleCaseFirst, LocaleCollation, LocaleData,
  LocaleFirstDayOfWeek, LocaleGetCalendars, LocaleGetCollations,
  LocaleGetHourCycles, LocaleGetNumberingSystems, LocaleGetTextInfo,
  LocaleGetTimeZones, LocaleGetWeekInfo, LocaleHourCycle, LocaleLanguage,
  LocaleMaximize, LocaleMinimize, LocaleNumberingSystem, LocaleNumeric,
  LocaleRegion, LocaleScript, LocaleState, LocaleToString, LocaleVariants,
  MonthName, MonthNum, NamedZone, NotationCompact, NotationEngineering,
  NotationScientific, NotationStandard, NumberFormatData, NumberFormatState,
  NumberToLocaleString, ObjectSlot, Ordinal, PluralRulesData, PluralRulesState,
  PriorityAuto, PriorityLessPrecision, PriorityMorePrecision,
  RelativeTimeFormatData, RelativeTimeFormatState, RoundCeil, RoundExpand,
  RoundFloor, RoundHalfCeil, RoundHalfEven, RoundHalfExpand, RoundHalfFloor,
  RoundHalfTrunc, RoundTrunc, RtfAlways, RtfAuto, RtfLong, RtfNarrow, RtfShort,
  SegmentIteratorData, SegmentIteratorState, SegmenterData, SegmenterState,
  SegmentsData, SegmentsState, SensAccent, SensBase, SensCase, SensVariant,
  SignAlways, SignAuto, SignExceptZero, SignNegative, SignNever,
  StringLocaleCompare, StringToLocaleLowerCase, StringToLocaleUpperCase,
  StyleCurrency, StyleDecimal, StylePercent, StyleUnit, TemporalDateSlot,
  TemporalDateTimeSlot, TemporalInstantSlot, TemporalMonthDaySlot,
  TemporalTimeSlot, TemporalYearMonthSlot, TemporalZonedDateTimeSlot, TsFull,
  TsLong, TsMedium, TsShort, TzLong, TzLongGeneric, TzLongOffset, TzShort,
  TzShortGeneric, TzShortOffset, TzdAuto, TzdStripIfInteger, UnitList, UnitLong,
  UnitNarrow, UnitShort, UsageSearch, UsageSort, WLong, WNarrow, WNumeric,
  WShort, WTwoDigit,
}

//
// Closed option-value sum types (§10.1.2, §12.1.2, §13.1.2, §16.1.2,
// §17.1.2, §18.1.2). Parsed once here, at the JS boundary; the formatting
// engines only ever see the variants.
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

type Thrown(host) =
  #(JsValue, State(host))

// ============================================================================
// Initialization
// ============================================================================

/// All the rooted refs other parts of the engine may need.
pub type IntlBuiltin {
  IntlBuiltin(namespace: Ref)
}

pub fn init(
  h: Heap(host),
  object_proto: Ref,
  function_proto: Ref,
  number_proto: Ref,
  bigint_proto: Ref,
  string_proto: Ref,
  date_proto: Ref,
) -> #(Heap(host), IntlBuiltin) {
  // --- Intl.Locale ---
  let #(h, locale_getters) =
    common.alloc_getters(
      h,
      function_proto,
      list.map(
        [
          LocaleBaseName, LocaleCalendar, LocaleCaseFirst, LocaleCollation,
          LocaleFirstDayOfWeek, LocaleHourCycle, LocaleNumeric,
          LocaleNumberingSystem, LocaleLanguage, LocaleScript, LocaleRegion,
          LocaleVariants,
        ],
        fn(getter) {
          #(locale_getter_js_name(getter), IntlNative(IntlLocaleGetter(getter)))
        },
      ),
    )
  let #(h, locale) =
    common.init_type(
      h,
      object_proto,
      function_proto,
      locale_getters,
      fn(proto) { Dispatch(IntlNative(IntlConstructor(CsLocale, proto))) },
      "Locale",
      1,
      [],
    )
  let h = common.add_to_string_tag(h, locale.prototype, "Intl.Locale")
  // Locale methods need the prototype ref for maximize/minimize results.
  let #(h, locale_methods) =
    common.alloc_methods(
      h,
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
            IntlNative(IntlLocaleMethod(method, locale.prototype)),
            0,
          )
        },
      ),
    )
  let h = add_named_properties(h, locale.prototype, locale_methods)

  // --- Simple formatter services ---
  let #(h, collator) =
    init_service(h, object_proto, function_proto, CsCollator, "Collator", [], [
      #("compare", IntlNative(IntlBoundGetter(BgCollator))),
    ])
  let #(h, number_format) =
    init_service(
      h,
      object_proto,
      function_proto,
      CsNumberFormat,
      "NumberFormat",
      [
        service_method(IntlNumberFormat, IntlFormatToParts, 1),
        service_method(IntlNumberFormat, IntlFormatRange, 2),
        service_method(IntlNumberFormat, IntlFormatRangeToParts, 2),
      ],
      [#("format", IntlNative(IntlBoundGetter(BgNumberFormat)))],
    )
  let #(h, date_time_format) =
    init_service(
      h,
      object_proto,
      function_proto,
      CsDateTimeFormat,
      "DateTimeFormat",
      [
        service_method(IntlDateTimeFormat, IntlFormatToParts, 1),
        service_method(IntlDateTimeFormat, IntlFormatRange, 2),
        service_method(IntlDateTimeFormat, IntlFormatRangeToParts, 2),
      ],
      [#("format", IntlNative(IntlBoundGetter(BgDateTimeFormat)))],
    )
  let #(h, plural_rules) =
    init_service(
      h,
      object_proto,
      function_proto,
      CsPluralRules,
      "PluralRules",
      [
        service_method(IntlPluralRules, IntlSelect, 1),
        service_method(IntlPluralRules, IntlSelectRange, 2),
      ],
      [],
    )
  let #(h, list_format) =
    init_service(
      h,
      object_proto,
      function_proto,
      CsListFormat,
      "ListFormat",
      [
        service_method(IntlListFormat, IntlFormat, 1),
        service_method(IntlListFormat, IntlFormatToParts, 1),
      ],
      [],
    )
  let #(h, relative_time_format) =
    init_service(
      h,
      object_proto,
      function_proto,
      CsRelativeTimeFormat,
      "RelativeTimeFormat",
      [
        service_method(IntlRelativeTimeFormat, IntlFormat, 2),
        service_method(IntlRelativeTimeFormat, IntlFormatToParts, 2),
      ],
      [],
    )
  let #(h, display_names) =
    init_service(
      h,
      object_proto,
      function_proto,
      CsDisplayNames,
      "DisplayNames",
      [service_method(IntlDisplayNames, IntlOf, 1)],
      [],
    )
  let #(h, duration_format) =
    init_service(
      h,
      object_proto,
      function_proto,
      CsDurationFormat,
      "DurationFormat",
      [
        service_method(IntlDurationFormat, IntlFormat, 1),
        service_method(IntlDurationFormat, IntlFormatToParts, 1),
      ],
      [],
    )

  // --- Segmenter (needs %SegmentIteratorPrototype% / %SegmentsPrototype%) ---
  let #(h, seg_iter_next) =
    common.alloc_methods(h, function_proto, [
      service_method(IntlSegmentIterator, IntlSegmentIteratorNext, 0),
    ])
  let #(h, seg_iter_proto) =
    common.init_namespace(
      h,
      object_proto,
      "Segmenter String Iterator",
      seg_iter_next,
    )
  let #(h, seg_containing) =
    common.alloc_methods(h, function_proto, [
      service_method(IntlSegments, IntlSegmentsContaining, 1),
    ])
  let #(h, segments_proto) =
    common.alloc_proto(h, Some(object_proto), dict.new())
  let h = add_named_properties(h, segments_proto, seg_containing)
  let #(h, seg_iter_fn) =
    common.alloc_native_fn(
      h,
      function_proto,
      IntlNative(IntlSegmentsIterator(seg_iter_proto)),
      "[Symbol.iterator]",
      0,
    )
  let h =
    common.add_symbol_property(
      h,
      segments_proto,
      value.symbol_iterator,
      value.builtin_property(JsObject(seg_iter_fn)),
    )
  let #(h, segment_method) =
    common.alloc_methods(h, function_proto, [
      #("segment", IntlNative(IntlSegmenterSegment(segments_proto)), 1),
    ])
  let #(h, segmenter) =
    init_service(
      h,
      object_proto,
      function_proto,
      CsSegmenter,
      "Segmenter",
      [],
      [],
    )
  let h = add_named_properties(h, segmenter.prototype, segment_method)

  // --- Namespace object ---
  let #(h, get_canonical_locales) =
    common.alloc_native_fn(
      h,
      function_proto,
      IntlNative(IntlGetCanonicalLocales),
      "getCanonicalLocales",
      1,
    )
  let #(h, supported_values_of) =
    common.alloc_native_fn(
      h,
      function_proto,
      IntlNative(IntlSupportedValuesOf),
      "supportedValuesOf",
      1,
    )
  let ns_props = [
    #(
      "getCanonicalLocales",
      value.builtin_property(JsObject(get_canonical_locales)),
    ),
    #(
      "supportedValuesOf",
      value.builtin_property(JsObject(supported_values_of)),
    ),
    #("Locale", value.builtin_property(JsObject(locale.constructor))),
    #("Collator", value.builtin_property(JsObject(collator.constructor))),
    #(
      "NumberFormat",
      value.builtin_property(JsObject(number_format.constructor)),
    ),
    #(
      "DateTimeFormat",
      value.builtin_property(JsObject(date_time_format.constructor)),
    ),
    #("PluralRules", value.builtin_property(JsObject(plural_rules.constructor))),
    #("ListFormat", value.builtin_property(JsObject(list_format.constructor))),
    #(
      "RelativeTimeFormat",
      value.builtin_property(JsObject(relative_time_format.constructor)),
    ),
    #("Segmenter", value.builtin_property(JsObject(segmenter.constructor))),
    #(
      "DisplayNames",
      value.builtin_property(JsObject(display_names.constructor)),
    ),
    #(
      "DurationFormat",
      value.builtin_property(JsObject(duration_format.constructor)),
    ),
  ]
  let #(h, namespace) = common.init_namespace(h, object_proto, "Intl", ns_props)

  // ECMA-402 §17-19: locale-sensitive overrides on Number/BigInt/String/Date.
  let #(h, number_methods) =
    common.alloc_methods(h, function_proto, [
      #("toLocaleString", IntlNative(IntlHostOverride(NumberToLocaleString)), 0),
    ])
  let h = add_named_properties(h, number_proto, number_methods)
  let #(h, bigint_methods) =
    common.alloc_methods(h, function_proto, [
      #("toLocaleString", IntlNative(IntlHostOverride(BigIntToLocaleString)), 0),
    ])
  let h = add_named_properties(h, bigint_proto, bigint_methods)
  let #(h, string_methods) =
    common.alloc_methods(h, function_proto, [
      #("localeCompare", IntlNative(IntlHostOverride(StringLocaleCompare)), 1),
      #(
        "toLocaleLowerCase",
        IntlNative(IntlHostOverride(StringToLocaleLowerCase)),
        0,
      ),
      #(
        "toLocaleUpperCase",
        IntlNative(IntlHostOverride(StringToLocaleUpperCase)),
        0,
      ),
    ])
  let h = add_named_properties(h, string_proto, string_methods)
  let #(h, date_methods) =
    common.alloc_methods(h, function_proto, [
      #("toLocaleString", IntlNative(IntlHostOverride(DateToLocaleString)), 0),
      #(
        "toLocaleDateString",
        IntlNative(IntlHostOverride(DateToLocaleDateString)),
        0,
      ),
      #(
        "toLocaleTimeString",
        IntlNative(IntlHostOverride(DateToLocaleTimeString)),
        0,
      ),
    ])
  let h = add_named_properties(h, date_proto, date_methods)

  #(h, IntlBuiltin(namespace:))
}

/// Build one formatter service: prototype methods + accessor getters +
/// resolvedOptions + supportedLocalesOf static + @@toStringTag.
fn init_service(
  h: Heap(host),
  object_proto: Ref,
  function_proto: Ref,
  service: ConstructibleService,
  name: String,
  methods: List(#(String, value.NativeFn, Int)),
  accessors: List(#(String, value.NativeFn)),
) -> #(Heap(host), common.BuiltinType) {
  let arity = case service {
    CsDisplayNames -> 2
    _ -> 0
  }
  // The brand instances of this service carry: `resolvedOptions` and
  // `supportedLocalesOf` are shared by every service, constructible or not.
  let brand = value.constructible_service(service)
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("resolvedOptions", IntlNative(IntlResolvedOptions(brand)), 0),
      ..methods
    ])
  let #(h, proto_accessors) = common.alloc_getters(h, function_proto, accessors)
  let #(h, slo) =
    common.alloc_methods(h, function_proto, [
      #("supportedLocalesOf", IntlNative(IntlSupportedLocalesOf(brand)), 1),
    ])
  let #(h, bt) =
    common.init_type(
      h,
      object_proto,
      function_proto,
      list.append(proto_accessors, proto_methods),
      fn(proto) { Dispatch(IntlNative(IntlConstructor(service, proto))) },
      name,
      arity,
      slo,
    )
  let h = common.add_to_string_tag(h, bt.prototype, "Intl." <> name)
  #(h, bt)
}

/// A prototype-method registration triple for `init_service` /
/// `common.alloc_methods` — the JS property name is derived from the
/// `IntlMethodName` variant so the two can never disagree.
fn service_method(
  service: IntlService,
  method: IntlMethodName,
  arity: Int,
) -> #(String, value.NativeFn, Int) {
  #(intl_method_js_name(method), IntlNative(IntlMethod(service, method)), arity)
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
  h: Heap(host),
  ref: Ref,
  props: List(#(String, value.Property)),
) -> Heap(host) {
  heap.update(h, ref, fn(slot) {
    case slot {
      ObjectSlot(properties:, ..) ->
        ObjectSlot(
          ..slot,
          properties: list.fold(props, properties, fn(acc, p) {
            dict.insert(acc, Named(p.0), p.1)
          }),
        )
      other -> other
    }
  })
}

// ============================================================================
// Dispatch
// ============================================================================

pub fn dispatch(
  native: IntlNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case native {
    IntlGetCanonicalLocales -> get_canonical_locales(args, state)
    IntlSupportedValuesOf -> supported_values_of(args, state)
    IntlConstructor(service:, proto:) ->
      construct_service(service, proto, args, state)
    IntlSupportedLocalesOf(_service) -> supported_locales_of(args, state)
    IntlResolvedOptions(service:) -> resolved_options(service, this, state)
    IntlBoundGetter(service:) -> bound_getter(service, this, state)
    IntlBoundMethod(service:, target:) ->
      bound_method(service, target, args, state)
    IntlMethod(service:, method:) ->
      run_method(service, method, args, this, state)
    IntlHostOverride(which:) -> run_host_override(which, args, this, state)
    IntlSegmenterSegment(segments_proto:) ->
      segmenter_segment(segments_proto, args, this, state)
    IntlSegmentsIterator(iter_proto:) ->
      segments_iterator(iter_proto, this, state)
    IntlLocaleGetter(name:) -> locale_getter(name, this, state)
    IntlLocaleMethod(method:, proto:) ->
      locale_method(method, proto, args, this, state)
  }
}

// ============================================================================
// Shared plumbing
// ============================================================================

fn run(
  r: Result(#(JsValue, State(host)), Thrown(host)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case r {
    Ok(#(v, state)) -> #(state, Ok(v))
    Error(#(t, state)) -> #(state, Error(t))
  }
}

/// Raise a RangeError as an abrupt completion. `range_error_value` hands back
/// the bare `#(thrown, state)` pair, so there is no success value in scope that
/// could accidentally be thrown.
fn throw_range(state: State(host), msg: String) -> Result(a, Thrown(host)) {
  Error(state.range_error_value(state, msg))
}

/// Raise a TypeError as an abrupt completion. See `throw_range`.
fn throw_type(state: State(host), msg: String) -> Result(a, Thrown(host)) {
  Error(state.type_error_value(state, msg))
}

/// Read the IntlObject state for `this`, throwing TypeError on brand mismatch.
fn branded(
  state: State(host),
  this: JsValue,
  service: IntlService,
  method: String,
) -> Result(#(Ref, IntlData), Thrown(host)) {
  use data <- branded_of(state, this, method)
  case value.intl_service(data) == service {
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
  state: State(host),
  this: JsValue,
  method: String,
  extract: fn(IntlData) -> Option(a),
) -> Result(#(Ref, a), Thrown(host)) {
  let found =
    helpers.brand_of(state.heap, this, fn(kind) {
      case kind {
        IntlObject(data:) -> extract(data)
        _ -> None
      }
    })
  case found {
    Some(#(v, ref)) -> Ok(#(ref, v))
    None -> throw_type(state, method <> " called on incompatible receiver")
  }
}

fn branded_locale(
  state: State(host),
  this: JsValue,
  method: String,
) -> Result(#(Ref, LocaleState), Thrown(host)) {
  use data <- branded_of(state, this, method)
  case data {
    LocaleData(l) -> Some(l)
    _other -> None
  }
}

fn branded_segmenter(
  state: State(host),
  this: JsValue,
  method: String,
) -> Result(#(Ref, SegmenterState), Thrown(host)) {
  use data <- branded_of(state, this, method)
  case data {
    SegmenterData(s) -> Some(s)
    _other -> None
  }
}

fn branded_segments(
  state: State(host),
  this: JsValue,
  method: String,
) -> Result(#(Ref, SegmentsState), Thrown(host)) {
  use data <- branded_of(state, this, method)
  case data {
    SegmentsData(s) -> Some(s)
    _other -> None
  }
}

fn branded_collator(
  state: State(host),
  this: JsValue,
  method: String,
) -> Result(#(Ref, CollatorState), Thrown(host)) {
  use data <- branded_of(state, this, method)
  case data {
    CollatorData(c) -> Some(c)
    _other -> None
  }
}

fn branded_number_format(
  state: State(host),
  this: JsValue,
  method: String,
) -> Result(#(Ref, NumberFormatState), Thrown(host)) {
  use data <- branded_of(state, this, method)
  case data {
    NumberFormatData(nf) -> Some(nf)
    _other -> None
  }
}

fn branded_date_time_format(
  state: State(host),
  this: JsValue,
  method: String,
) -> Result(#(Ref, DateTimeFormatState), Thrown(host)) {
  use data <- branded_of(state, this, method)
  case data {
    DateTimeFormatData(d) -> Some(d)
    _other -> None
  }
}

/// Replace an Intl instance's per-service state in the heap, keeping every
/// other object attribute.
fn write_intl_data(h: Heap(host), ref: Ref, data: IntlData) -> Heap(host) {
  heap.update(h, ref, fn(slot) {
    case slot {
      ObjectSlot(kind: IntlObject(..), ..) ->
        ObjectSlot(..slot, kind: IntlObject(data:))
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

fn alloc_array(
  state: State(host),
  values: List(JsValue),
) -> #(State(host), JsValue) {
  let #(heap, ref) =
    common.alloc_array(state.heap, values, state.builtins.array.prototype)
  #(State(..state, heap:), JsObject(ref))
}

fn alloc_pojo(
  state: State(host),
  props: List(#(String, JsValue)),
) -> #(State(host), JsValue) {
  let #(heap, ref) =
    common.alloc_pojo(
      state.heap,
      state.builtins.object.prototype,
      list.map(props, fn(p) { #(p.0, value.data_property(p.1)) }),
    )
  #(State(..state, heap:), JsObject(ref))
}

/// Parts → JS array of `{ type, value }` objects.
fn parts_to_js(
  state: State(host),
  parts: List(fmt.Part),
) -> #(State(host), JsValue) {
  let #(state, objs) =
    list.fold(parts, #(state, []), fn(acc, part) {
      let #(state, objs) = acc
      let #(t, v) = part
      let #(state, obj) =
        alloc_pojo(state, [
          #("type", JsString(fmt.part_type_to_js_string(t))),
          #("value", JsString(v)),
        ])
      #(state, [obj, ..objs])
    })
  alloc_array(state, list.reverse(objs))
}

/// The formatted string a range's parts spell out (formatRange).
fn range_parts_to_string(parts: List(fmt.RangePart)) -> String {
  parts |> list.map(fn(p: fmt.RangePart) { p.value }) |> string.join("")
}

/// Parts → JS array of `{ type, value, source }` objects (formatRangeToParts).
fn parts_to_js_sourced(
  state: State(host),
  parts: List(fmt.RangePart),
) -> #(State(host), JsValue) {
  let #(state, objs) =
    list.fold(parts, #(state, []), fn(acc, part: fmt.RangePart) {
      let #(state, objs) = acc
      let #(state, obj) =
        alloc_pojo(state, [
          #("type", JsString(fmt.part_type_to_js_string(part.type_))),
          #("value", JsString(part.value)),
          #("source", JsString(fmt.part_source_to_js_string(part.source))),
        ])
      #(state, [obj, ..objs])
    })
  alloc_array(state, list.reverse(objs))
}

/// Parts → JS array of `{ type, value, unit? }` objects (RelativeTimeFormat /
/// DurationFormat formatToParts). `unit: None` means no unit property.
fn parts_to_js_with_unit(
  state: State(host),
  parts: List(fmt.UnitPart),
) -> #(State(host), JsValue) {
  let #(state, objs) =
    list.fold(parts, #(state, []), fn(acc, part: fmt.UnitPart) {
      let #(state, objs) = acc
      let base = [
        #("type", JsString(fmt.part_type_to_js_string(part.type_))),
        #("value", JsString(part.value)),
      ]
      let props = case part.unit {
        None -> base
        Some(unit) -> list.append(base, [#("unit", JsString(unit))])
      }
      let #(state, obj) = alloc_pojo(state, props)
      #(state, [obj, ..objs])
    })
  alloc_array(state, list.reverse(objs))
}

// ============================================================================
// Options helpers (ECMA-402 §9.2.10–9.2.17)
// ============================================================================

/// CoerceOptionsToObject: undefined → no options; else ToObject.
fn coerce_options(
  state: State(host),
  v: JsValue,
) -> Result(#(Option(Ref), State(host)), Thrown(host)) {
  case v {
    JsUndefined -> Ok(#(None, state))
    _ ->
      case common.to_object(state.heap, state.builtins, v) {
        Some(#(heap, ref)) -> Ok(#(Some(ref), State(..state, heap:)))
        None -> throw_type(state, "Cannot convert options to object")
      }
  }
}

/// GetOptionsObject: undefined → none; Object → it; else TypeError.
fn strict_options(
  state: State(host),
  v: JsValue,
) -> Result(#(Option(Ref), State(host)), Thrown(host)) {
  case v {
    JsUndefined -> Ok(#(None, state))
    JsObject(ref) -> Ok(#(Some(ref), state))
    _ -> throw_type(state, "options must be an object or undefined")
  }
}

fn opt_get(
  state: State(host),
  opts: Option(Ref),
  name: String,
) -> Result(#(JsValue, State(host)), Thrown(host)) {
  case opts {
    None -> Ok(#(JsUndefined, state))
    Some(ref) -> object.get_value(state, ref, Named(name), JsObject(ref))
  }
}

/// GetOption with type string. Empty `allowed` list = any string allowed.
fn get_str_opt(
  state: State(host),
  opts: Option(Ref),
  name: String,
  allowed: List(String),
  default: Option(String),
) -> Result(#(Option(String), State(host)), Thrown(host)) {
  use #(v, state) <- result.try(opt_get(state, opts, name))
  case v {
    JsUndefined -> Ok(#(default, state))
    _ -> {
      use #(s, state) <- result.try(coerce.js_to_string(state, v))
      case allowed == [] || list.contains(allowed, s) {
        True -> Ok(#(Some(s), state))
        False ->
          throw_range(
            state,
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
  state: State(host),
  opts: Option(Ref),
  name: String,
  variants: List(#(String, a)),
  default: a,
) -> Result(#(a, State(host)), Thrown(host)) {
  use #(v, state) <- result.try(opt_get(state, opts, name))
  case v {
    JsUndefined -> Ok(#(default, state))
    _ -> {
      use #(s, state) <- result.try(coerce.js_to_string(state, v))
      case list.key_find(variants, s) {
        Ok(variant) -> Ok(#(variant, state))
        Error(Nil) ->
          throw_range(
            state,
            "Value " <> s <> " out of range for options property " <> name,
          )
      }
    }
  }
}

fn get_bool_opt(
  state: State(host),
  opts: Option(Ref),
  name: String,
  default: Option(Bool),
) -> Result(#(Option(Bool), State(host)), Thrown(host)) {
  use #(v, state) <- result.try(opt_get(state, opts, name))
  case v {
    JsUndefined -> Ok(#(default, state))
    _ -> Ok(#(Some(value.is_truthy(v)), state))
  }
}

/// GetNumberOption/DefaultNumberOption (§9.2.16/9.2.17).
fn get_num_opt(
  state: State(host),
  opts: Option(Ref),
  name: String,
  min: Int,
  max: Int,
  default: Option(Int),
) -> Result(#(Option(Int), State(host)), Thrown(host)) {
  use #(v, state) <- result.try(opt_get(state, opts, name))
  default_number_option(state, v, min, max, default, name)
}

fn default_number_option(
  state: State(host),
  v: JsValue,
  min: Int,
  max: Int,
  default: Option(Int),
  name: String,
) -> Result(#(Option(Int), State(host)), Thrown(host)) {
  case v {
    JsUndefined -> Ok(#(default, state))
    _ -> {
      use #(n, state) <- result.try(coerce.js_to_number(state, v))
      case n {
        value.Finite(f) ->
          // Range check happens on the unrounded value (§9.2.17).
          case f >=. int.to_float(min) && f <=. int.to_float(max) {
            True -> Ok(#(Some(float.truncate(float.floor(f))), state))
            False ->
              throw_range(
                state,
                name <> " value is out of range: " <> float.to_string(f),
              )
          }
        _ -> throw_range(state, name <> " value is out of range")
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
    n >= 3 && n <= 8 && is_alnum(p)
  })
}

fn all_codepoints(s: String, pred: fn(Int) -> Bool) -> Bool {
  string.to_utf_codepoints(s)
  |> list.all(fn(cp) { pred(string.utf_codepoint_to_int(cp)) })
}

fn is_alnum(s: String) -> Bool {
  s != "" && all_codepoints(s, digits.is_ascii_alnum_code)
}

// ============================================================================
// CanonicalizeLocaleList (§9.2.1)
// ============================================================================

fn canonicalize_locale_list(
  state: State(host),
  locales: JsValue,
) -> Result(#(List(String), State(host)), Thrown(host)) {
  case locales {
    JsUndefined -> Ok(#([], state))
    JsString(s) -> {
      use #(tag, state) <- result.try(canonical_tag_or_throw(state, s))
      Ok(#([tag], state))
    }
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: IntlObject(data: LocaleData(l)), ..)) ->
          Ok(#([l.locale], state))
        _ -> locale_list_from_object(state, ref)
      }
    _ ->
      case common.to_object(state.heap, state.builtins, locales) {
        Some(#(heap, ref)) ->
          locale_list_from_object(State(..state, heap:), ref)
        None -> throw_type(state, "Cannot convert locales to object")
      }
  }
}

fn canonical_tag_or_throw(
  state: State(host),
  s: String,
) -> Result(#(String, State(host)), Thrown(host)) {
  case tags.canonicalize_tag(s) {
    Ok(tag) -> Ok(#(tag, state))
    Error(Nil) ->
      throw_range(state, "Incorrect locale information provided: " <> s)
  }
}

fn locale_list_from_object(
  state: State(host),
  ref: Ref,
) -> Result(#(List(String), State(host)), Thrown(host)) {
  use #(len_v, state) <- result.try(object.get_value(
    state,
    ref,
    Named("length"),
    JsObject(ref),
  ))
  use #(len_n, state) <- result.try(coerce.js_to_number(state, len_v))
  let len = to_length(len_n)
  locale_list_loop(state, ref, 0, len, [])
}

fn to_length(n: value.JsNum) -> Int {
  case n {
    value.Finite(f) ->
      case f <. 0.0 {
        True -> 0
        False -> int.min(float.truncate(f), 9_007_199_254_740_991)
      }
    value.Infinity -> 9_007_199_254_740_991
    _ -> 0
  }
}

fn locale_list_loop(
  state: State(host),
  ref: Ref,
  k: Int,
  len: Int,
  seen: List(String),
) -> Result(#(List(String), State(host)), Thrown(host)) {
  case k >= len {
    True -> Ok(#(list.reverse(seen), state))
    False -> {
      let key = Index(k)
      // §9.2.1 step 6.c: HasProperty(O, Pk) — the list may be a proxy, whose
      // `has` trap is user code and can throw.
      use #(has, state) <- result.try(object.has_property_stateful(
        state,
        ref,
        value.string_object_key(key),
      ))
      case has {
        False -> locale_list_loop(state, ref, k + 1, len, seen)
        True -> {
          use #(k_value, state) <- result.try(object.get_value(
            state,
            ref,
            key,
            JsObject(ref),
          ))
          use #(tag_str, state) <- result.try(case k_value {
            JsString(s) -> Ok(#(s, state))
            JsObject(o_ref) ->
              case heap.read(state.heap, o_ref) {
                Some(ObjectSlot(kind: IntlObject(data: LocaleData(l)), ..)) ->
                  Ok(#(l.locale, state))
                _ -> coerce.js_to_string(state, k_value)
              }
            _ -> throw_type(state, "Locales item must be a string or object")
          })
          use #(tag, state) <- result.try(canonical_tag_or_throw(state, tag_str))
          let seen = case list.contains(seen, tag) {
            True -> seen
            False -> [tag, ..seen]
          }
          locale_list_loop(state, ref, k + 1, len, seen)
        }
      }
    }
  }
}

// ============================================================================
// Intl.getCanonicalLocales / Intl.supportedValuesOf
// ============================================================================

fn get_canonical_locales(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let locales = first_arg_or_undefined(args)
  run({
    use #(tag_list, state) <- result.try(canonicalize_locale_list(
      state,
      locales,
    ))
    let #(state, arr) = alloc_array(state, list.map(tag_list, JsString))
    Ok(#(arr, state))
  })
}

fn supported_values_of(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  run({
    use #(key, state) <- result.try(coerce.js_to_string(
      state,
      first_arg_or_undefined(args),
    ))
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
      Some(vs) -> {
        let #(state, arr) = alloc_array(state, list.map(vs, JsString))
        Ok(#(arr, state))
      }
      None -> throw_range(state, "Invalid key : " <> key)
    }
  })
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

fn supported_locales_of(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let locales = first_arg_or_undefined(args)
  let options_v = helpers.arg_at(args, 1)
  run({
    use #(requested, state) <- result.try(canonicalize_locale_list(
      state,
      locales,
    ))
    use #(opts, state) <- result.try(coerce_options(state, options_v))
    use #(_matcher, state) <- result.try(get_str_opt(
      state,
      opts,
      "localeMatcher",
      ["lookup", "best fit"],
      Some("best fit"),
    ))
    let supported =
      list.filter(requested, fn(tag) {
        tags.best_available_locale(tags.strip_extensions(tag)) != None
      })
    let #(state, arr) = alloc_array(state, list.map(supported, JsString))
    Ok(#(arr, state))
  })
}

// ============================================================================
// ResolveLocale (§9.2.7, lookup matcher against our available set)
// ============================================================================

/// Returns #(resolved_locale_string, data_locale, extension_keywords).
/// `extension_keywords` are the u-extension key/values from the matched
/// requested tag (valid values only — callers overlay option values).
fn resolve_locale(
  requested: List(String),
) -> #(String, String, List(#(String, String))) {
  case requested {
    [] -> #(tags.default_locale(), tags.default_locale(), [])
    [tag, ..rest] ->
      case tags.best_available_locale(tags.strip_extensions(tag)) {
        Some(available) -> {
          let keywords = u_keywords_of(tag)
          #(available, available, keywords)
        }
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
  case service {
    IntlLocale -> "Locale"
    IntlCollator -> "Collator"
    IntlNumberFormat -> "NumberFormat"
    IntlDateTimeFormat -> "DateTimeFormat"
    IntlPluralRules -> "PluralRules"
    IntlListFormat -> "ListFormat"
    IntlRelativeTimeFormat -> "RelativeTimeFormat"
    IntlSegmenter -> "Segmenter"
    IntlDisplayNames -> "DisplayNames"
    IntlDurationFormat -> "DurationFormat"
    IntlSegments -> "Segments"
    IntlSegmentIterator -> "Segment Iterator"
  }
}

fn construct_service(
  service: ConstructibleService,
  proto: Ref,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let callable_without_new = case service {
    CsCollator | CsNumberFormat | CsDateTimeFormat -> True
    _ -> False
  }
  case !callable_without_new && state.new_target == JsUndefined {
    True ->
      state.type_error(
        state,
        "Constructor Intl."
          <> service_name(value.constructible_service(service))
          <> " requires 'new'",
      )
    False -> {
      // §10.1.13 OrdinaryCreateFromConstructor: resolve the prototype from
      // NewTarget so `class Sub extends Intl.X` instances get Sub.prototype.
      // Falls back to the intrinsic `proto` when called without `new`.
      use proto, state <- object.proto_from_new_target(
        state,
        state.new_target,
        proto,
      )
      let arg0 = first_arg_or_undefined(args)
      let arg1 = helpers.arg_at(args, 1)
      run({
        use #(data, state) <- result.try(case service {
          CsLocale -> {
            use #(s, state) <- result.map(locale_state(state, arg0, arg1))
            #(LocaleData(s), state)
          }
          CsCollator -> {
            use #(s, state) <- result.map(collator_state(state, arg0, arg1))
            #(CollatorData(s), state)
          }
          CsNumberFormat -> {
            use #(s, state) <- result.map(number_format_state(state, arg0, arg1))
            #(NumberFormatData(s), state)
          }
          CsDateTimeFormat -> {
            use #(s, state) <- result.map(date_time_format_state(
              state,
              arg0,
              arg1,
            ))
            #(DateTimeFormatData(s), state)
          }
          CsPluralRules -> {
            use #(s, state) <- result.map(plural_rules_state(state, arg0, arg1))
            #(PluralRulesData(s), state)
          }
          CsListFormat -> {
            use #(s, state) <- result.map(list_format_state(state, arg0, arg1))
            #(ListFormatData(s), state)
          }
          CsRelativeTimeFormat -> {
            use #(s, state) <- result.map(rtf_state(state, arg0, arg1))
            #(RelativeTimeFormatData(s), state)
          }
          CsSegmenter -> {
            use #(s, state) <- result.map(segmenter_state(state, arg0, arg1))
            #(SegmenterData(s), state)
          }
          CsDisplayNames -> {
            use #(s, state) <- result.map(display_names_state(state, arg0, arg1))
            #(DisplayNamesData(s), state)
          }
          CsDurationFormat -> {
            use #(s, state) <- result.map(duration_format_state(
              state,
              arg0,
              arg1,
            ))
            #(DurationFormatData(s), state)
          }
        })
        let #(heap, ref) =
          common.alloc_wrapper(state.heap, IntlObject(data:), proto)
        Ok(#(JsObject(ref), State(..state, heap:)))
      })
    }
  }
}

// --- Intl.Locale ---

fn locale_state(
  state: State(host),
  tag_v: JsValue,
  options_v: JsValue,
) -> Result(#(LocaleState, State(host)), Thrown(host)) {
  // Step 7-9: tag must be String or Object; Locale objects pass [[Locale]].
  use #(tag_str, state) <- result.try(case tag_v {
    JsString(s) -> Ok(#(s, state))
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: IntlObject(data: LocaleData(l)), ..)) ->
          Ok(#(l.locale, state))
        _ -> coerce.js_to_string(state, tag_v)
      }
    _ -> throw_type(state, "Intl.Locale tag must be a string or object")
  })
  use #(opts, state) <- result.try(coerce_options(state, options_v))
  // ApplyOptionsToTag: structural validity first.
  use lid <- result.try(case tags.parse(tag_str) {
    Ok(lid) -> Ok(lid)
    Error(Nil) ->
      throw_range(state, "Incorrect locale information provided: " <> tag_str)
  })
  use #(language, state) <- result.try(get_str_opt(
    state,
    opts,
    "language",
    [],
    None,
  ))
  use Nil <- result.try(case language {
    Some(l) ->
      case is_language_subtag(l) {
        True -> Ok(Nil)
        False -> throw_range(state, "Invalid language: " <> l)
      }
    None -> Ok(Nil)
  })
  use #(script, state) <- result.try(get_str_opt(
    state,
    opts,
    "script",
    [],
    None,
  ))
  use Nil <- result.try(case script {
    Some(s) ->
      case is_script_subtag(s) {
        True -> Ok(Nil)
        False -> throw_range(state, "Invalid script: " <> s)
      }
    None -> Ok(Nil)
  })
  use #(region, state) <- result.try(get_str_opt(
    state,
    opts,
    "region",
    [],
    None,
  ))
  use Nil <- result.try(case region {
    Some(r) ->
      case is_region_subtag(r) {
        True -> Ok(Nil)
        False -> throw_range(state, "Invalid region: " <> r)
      }
    None -> Ok(Nil)
  })
  use #(variants_opt, state) <- result.try(get_str_opt(
    state,
    opts,
    "variants",
    [],
    None,
  ))
  use variants_opt <- result.try(case variants_opt {
    None -> Ok(None)
    Some(v) -> {
      let lower = string.lowercase(v)
      let parts = string.split(lower, "-")
      let valid =
        parts != []
        && list.all(parts, is_variant_subtag)
        && list.length(list.unique(parts)) == list.length(parts)
      case valid {
        True -> Ok(Some(parts))
        False -> throw_range(state, "Invalid variants: " <> v)
      }
    }
  })
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
  use #(calendar, state) <- result.try(get_str_opt(
    state,
    opts,
    "calendar",
    [],
    None,
  ))
  use Nil <- result.try(require_type_seq(state, calendar, "calendar"))
  use #(collation, state) <- result.try(get_str_opt(
    state,
    opts,
    "collation",
    [],
    None,
  ))
  use Nil <- result.try(require_type_seq(state, collation, "collation"))
  use #(hour_cycle, state) <- result.try(get_str_opt(
    state,
    opts,
    "hourCycle",
    ["h11", "h12", "h23", "h24"],
    None,
  ))
  use #(case_first, state) <- result.try(get_str_opt(
    state,
    opts,
    "caseFirst",
    ["upper", "lower", "false"],
    None,
  ))
  use #(numeric, state) <- result.try(get_bool_opt(state, opts, "numeric", None))
  use #(first_day, state) <- result.try(get_str_opt(
    state,
    opts,
    "firstDayOfWeek",
    [],
    None,
  ))
  use first_day <- result.try(case first_day {
    None -> Ok(None)
    Some(fd) ->
      case weekday_string(fd) {
        Some(v) -> Ok(Some(v))
        None -> throw_range(state, "Invalid firstDayOfWeek: " <> fd)
      }
  })
  use #(numbering, state) <- result.try(get_str_opt(
    state,
    opts,
    "numberingSystem",
    [],
    None,
  ))
  use Nil <- result.try(require_type_seq(state, numbering, "numberingSystem"))
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
  Ok(#(LocaleState(locale: canonical), state))
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

fn is_variant_subtag(s: String) -> Bool {
  let n = string.length(s)
  case n {
    4 ->
      case string.first(s) {
        Ok(c) -> is_digit_str(c) && is_alnum(s)
        Error(Nil) -> False
      }
    _ -> n >= 5 && n <= 8 && is_alnum(s)
  }
}

fn is_language_subtag(s: String) -> Bool {
  let n = string.length(s)
  is_alpha_str(s) && { n == 2 || n == 3 || { n >= 5 && n <= 8 } }
}

fn is_script_subtag(s: String) -> Bool {
  is_alpha_str(s) && string.length(s) == 4
}

fn is_region_subtag(s: String) -> Bool {
  let n = string.length(s)
  { is_alpha_str(s) && n == 2 } || { is_digit_str(s) && n == 3 }
}

fn is_alpha_str(s: String) -> Bool {
  s != "" && all_codepoints(s, digits.is_ascii_alpha_code)
}

fn is_digit_str(s: String) -> Bool {
  s != "" && all_codepoints(s, digits.is_decimal_code)
}

fn require_type_seq(
  state: State(host),
  v: Option(String),
  name: String,
) -> Result(Nil, Thrown(host)) {
  case v {
    Some(s) ->
      case is_type_sequence(s) {
        True -> Ok(Nil)
        False -> throw_range(state, "Invalid " <> name <> ": " <> s)
      }
    None -> Ok(Nil)
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

/// `resolve_typed_keyword` for the string-valued keys (ca / nu / co / kf / kn):
/// an option value the host does not support falls back like an absent one.
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
fn read_locale_matcher(
  state: State(host),
  opts: Option(Ref),
) -> Result(State(host), Thrown(host)) {
  use #(_matcher, state) <- result.map(get_str_opt(
    state,
    opts,
    "localeMatcher",
    ["lookup", "best fit"],
    Some("best fit"),
  ))
  state
}

/// Shared *_state constructor prologue: CanonicalizeLocaleList, options
/// coercion (GetOptionsObject when `strict`, else CoerceOptionsToObject),
/// then the localeMatcher read.
fn constructor_prologue(
  state: State(host),
  locales_v: JsValue,
  options_v: JsValue,
  strict strict: Bool,
) -> Result(#(List(String), Option(Ref), State(host)), Thrown(host)) {
  use #(requested, state) <- result.try(canonicalize_locale_list(
    state,
    locales_v,
  ))
  use #(opts, state) <- result.try(case strict {
    True -> strict_options(state, options_v)
    False -> coerce_options(state, options_v)
  })
  use state <- result.map(read_locale_matcher(state, opts))
  #(requested, opts, state)
}

/// numberingSystem option read + ResolveLocale with only the "nu" relevant
/// extension keyword. Returns #(numbering_system, resolved_locale, state).
fn resolve_nu_locale(
  state: State(host),
  opts: Option(Ref),
  requested: List(String),
) -> Result(#(String, String, State(host)), Thrown(host)) {
  use #(nu_opt, state) <- result.try(get_str_opt(
    state,
    opts,
    "numberingSystem",
    [],
    None,
  ))
  use Nil <- result.map(require_type_seq(state, nu_opt, "numberingSystem"))
  let #(_locale, data_locale, ext_kws) = resolve_locale(requested)
  let #(nu, nu_from_ext) =
    resolve_keyword(ext_kws, "nu", nu_opt, fmt.is_numbering_system, "latn")
  let locale = build_resolved_locale(data_locale, [#("nu", nu_from_ext, nu)])
  #(nu, locale, state)
}

// --- Intl.Collator ---

fn collator_state(
  state: State(host),
  locales_v: JsValue,
  options_v: JsValue,
) -> Result(#(CollatorState, State(host)), Thrown(host)) {
  use #(requested, state) <- result.try(canonicalize_locale_list(
    state,
    locales_v,
  ))
  use #(opts, state) <- result.try(coerce_options(state, options_v))
  use #(usage, state) <- result.try(get_enum_opt(
    state,
    opts,
    "usage",
    [#("sort", UsageSort), #("search", UsageSearch)],
    UsageSort,
  ))
  use state <- result.try(read_locale_matcher(state, opts))
  use #(collation_opt, state) <- result.try(get_str_opt(
    state,
    opts,
    "collation",
    [],
    None,
  ))
  use Nil <- result.try(require_type_seq(state, collation_opt, "collation"))
  use #(numeric_opt, state) <- result.try(get_bool_opt(
    state,
    opts,
    "numeric",
    None,
  ))
  use #(case_first_opt, state) <- result.try(get_enum_opt(
    state,
    opts,
    "caseFirst",
    [
      #("upper", Some(CaseFirstUpper)),
      #("lower", Some(CaseFirstLower)),
      #("false", Some(CaseFirstFalse)),
    ],
    None,
  ))
  let #(_locale, data_locale, ext_kws) = resolve_locale(requested)
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
      value.case_first_from_js_string,
      CaseFirstFalse,
    )
  use #(sensitivity, state) <- result.try(get_enum_opt(
    state,
    opts,
    "sensitivity",
    [
      #("base", SensBase),
      #("accent", SensAccent),
      #("case", SensCase),
      #("variant", SensVariant),
    ],
    SensVariant,
  ))
  let ignore_punct_default = string.starts_with(data_locale, "th")
  use #(ignore_punct, state) <- result.try(get_bool_opt(
    state,
    opts,
    "ignorePunctuation",
    Some(ignore_punct_default),
  ))
  let locale =
    build_resolved_locale(data_locale, [
      #("co", co_from_ext, collation),
      #("kn", kn_from_ext, case numeric {
        True -> "true"
        False -> "false"
      }),
      #("kf", kf_from_ext, value.case_first_to_js_string(case_first)),
    ])
  Ok(#(
    CollatorState(
      locale:,
      usage:,
      sensitivity:,
      ignore_punctuation: option.unwrap(ignore_punct, False),
      collation:,
      numeric:,
      case_first:,
      bound_compare: None,
    ),
    state,
  ))
}

// --- Intl.NumberFormat ---

fn number_format_state(
  state: State(host),
  locales_v: JsValue,
  options_v: JsValue,
) -> Result(#(NumberFormatState, State(host)), Thrown(host)) {
  use #(requested, opts, state) <- result.try(constructor_prologue(
    state,
    locales_v,
    options_v,
    strict: False,
  ))
  use #(nu, locale, state) <- result.try(resolve_nu_locale(
    state,
    opts,
    requested,
  ))
  // SetNumberFormatUnitOptions (§15.1.3)
  use #(style, state) <- result.try(read_unit_options(state, opts))
  let #(mnfd_default, mxfd_default) = case style {
    StyleCurrency(currency:, ..) -> {
      let d = fmt.currency_digits(currency)
      #(d, d)
    }
    StylePercent -> #(0, 0)
    StyleDecimal | StyleUnit(..) -> #(0, 3)
  }
  use #(notation_kind, state) <- result.try(get_enum_opt(
    state,
    opts,
    "notation",
    notation_variants(),
    NkStandard,
  ))
  use #(digits, state) <- result.try(digit_options(
    state,
    opts,
    mnfd_default,
    mxfd_default,
    notation_kind,
  ))
  use #(notation, state) <- result.try(read_notation(state, opts, notation_kind))
  // useGrouping: boolean or "min2"/"auto"/"always" (§15.1.6 GetBooleanOrStringNumberFormatOption)
  use #(grouping_v, state) <- result.try(opt_get(state, opts, "useGrouping"))
  use #(use_grouping, state) <- result.try(case grouping_v {
    JsUndefined ->
      Ok(#(
        case notation {
          NotationCompact(..) -> GroupingMin2
          NotationStandard | NotationScientific | NotationEngineering ->
            GroupingAuto
        },
        state,
      ))
    JsBool(False) -> Ok(#(GroupingNever, state))
    JsBool(True) -> Ok(#(GroupingAlways, state))
    _ -> {
      use #(s, state) <- result.try(coerce.js_to_string(state, grouping_v))
      case s {
        "min2" -> Ok(#(GroupingMin2, state))
        "auto" -> Ok(#(GroupingAuto, state))
        "always" -> Ok(#(GroupingAlways, state))
        // Spec: any other string (including "true"/"false") is out of range.
        _ ->
          throw_range(
            state,
            "Value " <> s <> " out of range for options property useGrouping",
          )
      }
    }
  })
  use #(sign_display, state) <- result.try(get_enum_opt(
    state,
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
  ))
  Ok(#(
    NumberFormatState(
      locale:,
      numbering_system: nu,
      style:,
      digits:,
      use_grouping:,
      notation:,
      sign_display:,
      bound_format: None,
    ),
    state,
  ))
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
fn read_unit_options(
  state: State(host),
  opts: Option(Ref),
) -> Result(#(value.NumStyle, State(host)), Thrown(host)) {
  use #(kind, state) <- result.try(get_enum_opt(
    state,
    opts,
    "style",
    [
      #("decimal", KDecimal),
      #("percent", KPercent),
      #("currency", KCurrency),
      #("unit", KUnit),
    ],
    KDecimal,
  ))
  use #(currency, state) <- result.try(get_str_opt(
    state,
    opts,
    "currency",
    [],
    None,
  ))
  use Nil <- result.try(case currency {
    Some(c) ->
      case is_alpha_str(c) && string.length(c) == 3 {
        True -> Ok(Nil)
        False -> throw_range(state, "Invalid currency code: " <> c)
      }
    None -> Ok(Nil)
  })
  use sc <- result.try(case kind, currency {
    KCurrency, Some(c) -> Ok(ScCurrency(currency: string.uppercase(c)))
    KCurrency, None ->
      throw_type(state, "Currency code is required with currency style")
    KDecimal, _ -> Ok(ScDecimal)
    KPercent, _ -> Ok(ScPercent)
    KUnit, _ -> Ok(ScUnit)
  })
  use #(currency_display, state) <- result.try(get_enum_opt(
    state,
    opts,
    "currencyDisplay",
    [
      #("code", CurCode),
      #("symbol", CurSymbol),
      #("narrowSymbol", CurNarrowSymbol),
      #("name", CurName),
    ],
    CurSymbol,
  ))
  use #(currency_sign, state) <- result.try(get_enum_opt(
    state,
    opts,
    "currencySign",
    [#("standard", CurStandard), #("accounting", CurAccounting)],
    CurStandard,
  ))
  use #(unit, state) <- result.try(get_str_opt(state, opts, "unit", [], None))
  use Nil <- result.try(case unit {
    Some(u) ->
      case fmt.is_well_formed_unit(u) {
        True -> Ok(Nil)
        False ->
          throw_range(state, "Invalid unit argument for option unit: " <> u)
      }
    None -> Ok(Nil)
  })
  // Everything but the unit style's `unitDisplay` (read last) is now known.
  use build <- result.try(case sc, unit {
    ScDecimal, _ -> Ok(fn(_ud) { StyleDecimal })
    ScPercent, _ -> Ok(fn(_ud) { StylePercent })
    ScCurrency(currency:), _ ->
      Ok(fn(_ud) {
        StyleCurrency(currency:, display: currency_display, sign: currency_sign)
      })
    ScUnit, Some(u) -> Ok(fn(ud) { StyleUnit(unit: u, display: ud) })
    ScUnit, None -> throw_type(state, "Unit is required with unit style")
  })
  use #(unit_display, state) <- result.map(get_enum_opt(
    state,
    opts,
    "unitDisplay",
    [#("short", UnitShort), #("narrow", UnitNarrow), #("long", UnitLong)],
    UnitShort,
  ))
  #(build(unit_display), state)
}

/// The `notation` option's tag, before `compactDisplay` completes it.
type NotationKind {
  NkStandard
  NkScientific
  NkEngineering
  NkCompact
}

/// The `notation` option's spellings — shared by NumberFormat and PluralRules.
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
  state: State(host),
  opts: Option(Ref),
  kind: NotationKind,
) -> Result(#(Notation, State(host)), Thrown(host)) {
  use #(compact_display, state) <- result.map(get_enum_opt(
    state,
    opts,
    "compactDisplay",
    [#("short", CompactShort), #("long", CompactLong)],
    CompactShort,
  ))
  #(
    case kind {
      NkStandard -> NotationStandard
      NkScientific -> NotationScientific
      NkEngineering -> NotationEngineering
      NkCompact -> NotationCompact(display: compact_display)
    },
    state,
  )
}

/// SetNumberFormatDigitOptions (§15.1.6) — resolves the digit-related slots.
fn digit_options(
  state: State(host),
  opts: Option(Ref),
  mnfd_default: Int,
  mxfd_default: Int,
  notation: NotationKind,
) -> Result(#(IntlDigitOptions, State(host)), Thrown(host)) {
  use #(mnid, state) <- result.try(get_num_opt(
    state,
    opts,
    "minimumIntegerDigits",
    1,
    21,
    Some(1),
  ))
  use #(mnfd_v, state) <- result.try(opt_get(
    state,
    opts,
    "minimumFractionDigits",
  ))
  use #(mxfd_v, state) <- result.try(opt_get(
    state,
    opts,
    "maximumFractionDigits",
  ))
  use #(mnsd_v, state) <- result.try(opt_get(
    state,
    opts,
    "minimumSignificantDigits",
  ))
  use #(mxsd_v, state) <- result.try(opt_get(
    state,
    opts,
    "maximumSignificantDigits",
  ))
  use #(rounding_increment, state) <- result.try(get_num_opt(
    state,
    opts,
    "roundingIncrement",
    1,
    5000,
    Some(1),
  ))
  let rounding_increment = option.unwrap(rounding_increment, 1)
  use Nil <- result.try(
    case
      list.contains(
        [1, 2, 5, 10, 20, 25, 50, 100, 200, 250, 500, 1000, 2000, 2500, 5000],
        rounding_increment,
      )
    {
      True -> Ok(Nil)
      False ->
        throw_range(
          state,
          "roundingIncrement value is out of range: "
            <> int.to_string(rounding_increment),
        )
    },
  )
  use #(rounding_mode, state) <- result.try(get_enum_opt(
    state,
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
  ))
  use #(rounding_priority, state) <- result.try(get_enum_opt(
    state,
    opts,
    "roundingPriority",
    [
      #("auto", PriorityAuto),
      #("morePrecision", PriorityMorePrecision),
      #("lessPrecision", PriorityLessPrecision),
    ],
    PriorityAuto,
  ))
  use #(trailing_zero, state) <- result.try(get_enum_opt(
    state,
    opts,
    "trailingZeroDisplay",
    [#("auto", TzdAuto), #("stripIfInteger", TzdStripIfInteger)],
    TzdAuto,
  ))
  let has_sd = mnsd_v != JsUndefined || mxsd_v != JsUndefined
  let has_fd = mnfd_v != JsUndefined || mxfd_v != JsUndefined
  let need_sd = case rounding_priority {
    PriorityAuto -> has_sd
    PriorityMorePrecision | PriorityLessPrecision -> True
  }
  let need_fd = case rounding_priority {
    PriorityAuto -> !{ has_sd || { !has_fd && notation == NkCompact } }
    PriorityMorePrecision | PriorityLessPrecision -> True
  }
  // sig / fd are #(min, max) when that rounding kind is in effect.
  use #(sig, state) <- result.try(case need_sd {
    False -> Ok(#(None, state))
    True ->
      case has_sd {
        True -> {
          use #(mnsd, state) <- result.try(default_number_option(
            state,
            mnsd_v,
            1,
            21,
            Some(1),
            "minimumSignificantDigits",
          ))
          let mnsd = option.unwrap(mnsd, 1)
          use #(mxsd, state) <- result.try(default_number_option(
            state,
            mxsd_v,
            mnsd,
            21,
            Some(21),
            "maximumSignificantDigits",
          ))
          let mxsd = option.unwrap(mxsd, 21)
          Ok(#(Some(#(mnsd, mxsd)), state))
        }
        False -> Ok(#(Some(#(1, 21)), state))
      }
  })
  use #(fd, state) <- result.try(case need_fd {
    False -> Ok(#(None, state))
    True ->
      case has_fd {
        True -> {
          use #(mnfd, state) <- result.try(default_number_option(
            state,
            mnfd_v,
            0,
            100,
            None,
            "minimumFractionDigits",
          ))
          use #(mxfd, state) <- result.try(default_number_option(
            state,
            mxfd_v,
            0,
            100,
            None,
            "maximumFractionDigits",
          ))
          use #(mnfd, mxfd) <- result.try(case mnfd, mxfd {
            Some(mn), Some(mx) ->
              case mn > mx {
                True ->
                  throw_range(
                    state,
                    "minimumFractionDigits is greater than maximumFractionDigits",
                  )
                False -> Ok(#(mn, mx))
              }
            Some(mn), None -> Ok(#(mn, int.max(mxfd_default, mn)))
            None, Some(mx) -> Ok(#(int.min(mnfd_default, mx), mx))
            None, None -> Ok(#(mnfd_default, mxfd_default))
          })
          Ok(#(Some(#(mnfd, mxfd)), state))
        }
        False ->
          Ok(#(
            Some(#(mnfd_default, int.max(mxfd_default, mnfd_default))),
            state,
          ))
      }
  })
  // Neither kind requested (compact notation default): more-precision
  // rounding with mnfd/mxfd = 0 and mnsd/mxsd = 1..2 (§15.1.6 step 16).
  let #(sig, fd, rounding_priority) = case sig, fd {
    None, None -> #(Some(#(1, 2)), Some(#(0, 0)), PriorityMorePrecision)
    _, _ -> #(sig, fd, rounding_priority)
  }
  // roundingIncrement constraints (§15.1.6 steps 24-26).
  use Nil <- result.try(case rounding_increment != 1 {
    False -> Ok(Nil)
    True ->
      case need_sd || !need_fd {
        True ->
          throw_type(
            state,
            "roundingIncrement requires fractionDigits rounding type",
          )
        False ->
          case fd {
            None -> Ok(Nil)
            Some(#(mn, mx)) if mn == mx -> Ok(Nil)
            Some(_) ->
              throw_range(
                state,
                "roundingIncrement requires minimumFractionDigits equal to maximumFractionDigits",
              )
          }
      }
  })
  Ok(#(
    IntlDigitOptions(
      minimum_integer_digits: option.unwrap(mnid, 1),
      fraction_digits: fd,
      significant_digits: sig,
      rounding_increment:,
      rounding_mode:,
      rounding_priority:,
      trailing_zero_display: trailing_zero,
    ),
    state,
  ))
}

// --- Intl.DateTimeFormat ---

fn date_time_format_state(
  state: State(host),
  locales_v: JsValue,
  options_v: JsValue,
) -> Result(#(DateTimeFormatState, State(host)), Thrown(host)) {
  dtf_state_required(state, locales_v, options_v, date_defaults(), DateAndTime)
}

/// Which group of date-time components a DateTimeFormat is required to
/// produce (ToDateTimeOptions' `required` argument, §17):
/// Date.prototype.toLocaleDateString needs date fields,
/// toLocaleTimeString needs time fields, everything else accepts either.
pub type DtfRequired {
  DateOnly
  TimeOnly
  DateAndTime
}

/// Keep only the pairs whose value is present.
fn present_pairs(pairs: List(#(k, Option(a)))) -> List(#(k, a)) {
  list.filter_map(pairs, fn(kv) {
    case kv {
      #(k, Some(v)) -> Ok(#(k, v))
      #(_, None) -> Error(Nil)
    }
  })
}

/// The date-group locale defaults (ToDateTimeOptions "date" defaults).
fn date_defaults() -> DtfComponents {
  DtfComponents(
    ..value.empty_dtf_components,
    year: Some(WNumeric),
    month: Some(MonthNum(WNumeric)),
    day: Some(WNumeric),
  )
}

/// The time-group locale defaults (ToDateTimeOptions "time" defaults).
fn time_defaults() -> DtfComponents {
  DtfComponents(
    ..value.empty_dtf_components,
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
  state: State(host),
  locales_v: JsValue,
  options_v: JsValue,
  defaults: DtfComponents,
  required: DtfRequired,
) -> Result(#(DateTimeFormatState, State(host)), Thrown(host)) {
  use #(requested, opts, state) <- result.try(constructor_prologue(
    state,
    locales_v,
    options_v,
    strict: False,
  ))
  use #(calendar_opt, state) <- result.try(get_str_opt(
    state,
    opts,
    "calendar",
    [],
    None,
  ))
  use Nil <- result.try(require_type_seq(state, calendar_opt, "calendar"))
  use #(nu_opt, state) <- result.try(get_str_opt(
    state,
    opts,
    "numberingSystem",
    [],
    None,
  ))
  use Nil <- result.try(require_type_seq(state, nu_opt, "numberingSystem"))
  use #(hour12, state) <- result.try(get_bool_opt(state, opts, "hour12", None))
  use #(hour_cycle_opt, state) <- result.try(get_enum_opt(
    state,
    opts,
    "hourCycle",
    hour_cycle_variants(),
    None,
  ))
  // hour12 overrides hourCycle (§11.1.2 step 6).
  let hour_cycle_opt = case hour12 {
    Some(_) -> None
    None -> hour_cycle_opt
  }
  let #(_locale, data_locale, ext_kws) = resolve_locale(requested)
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
      #("hc", hc_from_ext && hour12 == None, value.hour_cycle_to_js_string(hc)),
    ])
  // timeZone
  use #(tz_v, state) <- result.try(opt_get(state, opts, "timeZone"))
  use #(time_zone, state) <- result.try(case tz_v {
    // DefaultTimeZone: the host environment zone. Its offset (like every
    // zone's) is resolved per formatted instant, not snapshotted here.
    JsUndefined -> Ok(#(HostZone, state))
    _ -> {
      use #(s, state) <- result.try(coerce.js_to_string(state, tz_v))
      case canonical_time_zone(s) {
        Some(tz) -> Ok(#(tz, state))
        None -> throw_range(state, "Invalid time zone specified: " <> s)
      }
    }
  })
  // Component options (table order).
  use #(weekday, state) <- result.try(get_enum_opt(
    state,
    opts,
    "weekday",
    optional_variants(name_width_variants()),
    None,
  ))
  use #(era, state) <- result.try(get_enum_opt(
    state,
    opts,
    "era",
    optional_variants(name_width_variants()),
    None,
  ))
  use #(year, state) <- result.try(get_enum_opt(
    state,
    opts,
    "year",
    optional_variants(numeric_width_variants()),
    None,
  ))
  use #(month, state) <- result.try(get_enum_opt(
    state,
    opts,
    "month",
    optional_variants(month_width_variants()),
    None,
  ))
  use #(day, state) <- result.try(get_enum_opt(
    state,
    opts,
    "day",
    optional_variants(numeric_width_variants()),
    None,
  ))
  use #(day_period, state) <- result.try(get_enum_opt(
    state,
    opts,
    "dayPeriod",
    optional_variants(name_width_variants()),
    None,
  ))
  use #(hour, state) <- result.try(get_enum_opt(
    state,
    opts,
    "hour",
    optional_variants(numeric_width_variants()),
    None,
  ))
  use #(minute, state) <- result.try(get_enum_opt(
    state,
    opts,
    "minute",
    optional_variants(numeric_width_variants()),
    None,
  ))
  use #(second, state) <- result.try(get_enum_opt(
    state,
    opts,
    "second",
    optional_variants(numeric_width_variants()),
    None,
  ))
  use #(fractional, state) <- result.try(get_num_opt(
    state,
    opts,
    "fractionalSecondDigits",
    1,
    3,
    None,
  ))
  use #(tz_name_opt, state) <- result.try(get_enum_opt(
    state,
    opts,
    "timeZoneName",
    optional_variants(tz_name_width_variants()),
    None,
  ))
  use #(_format_matcher, state) <- result.try(get_str_opt(
    state,
    opts,
    "formatMatcher",
    ["basic", "best fit"],
    Some("best fit"),
  ))
  use #(date_style, state) <- result.try(get_enum_opt(
    state,
    opts,
    "dateStyle",
    optional_variants([
      #("full", DsFull),
      #("long", DsLong),
      #("medium", DsMedium),
      #("short", DsShort),
    ]),
    None,
  ))
  use #(time_style, state) <- result.try(get_enum_opt(
    state,
    opts,
    "timeStyle",
    optional_variants([
      #("full", TsFull),
      #("long", TsLong),
      #("medium", TsMedium),
      #("short", TsShort),
    ]),
    None,
  ))
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
  use Nil <- result.try(case required {
    DateOnly ->
      case time_style {
        Some(_) ->
          throw_type(state, "timeStyle cannot be used with toLocaleDateString")
        None -> Ok(Nil)
      }
    TimeOnly ->
      case date_style {
        Some(_) ->
          throw_type(state, "dateStyle cannot be used with toLocaleTimeString")
        None -> Ok(Nil)
      }
    DateAndTime -> Ok(Nil)
  })
  let styled = option.is_some(date_style) || option.is_some(time_style)
  use Nil <- result.try(case styled && explicit {
    True ->
      throw_type(
        state,
        "Invalid option: dateStyle/timeStyle cannot be used with other date/time options",
      )
    False -> Ok(Nil)
  })
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
  Ok(#(
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
      bound_format: None,
    ),
    state,
  ))
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

/// A GetOption variant table for an option whose default is "absent".
fn optional_variants(
  variants: List(#(String, a)),
) -> List(#(String, Option(a))) {
  list.map(variants, fn(kv) { #(kv.0, Some(kv.1)) })
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
  let base = value.empty_dtf_components
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
  let base = value.empty_dtf_components
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

/// IsValidTimeZoneName + identifier case normalization. Named zones are
/// resolved against the system tzdata (Temporal's TZif backend), which is
/// also what supplies their per-instant offset later; the remaining forms
/// (UTC/GMT aliases, ±HH:MM, Etc/GMT±N) have an offset that never varies.
fn canonical_time_zone(s: String) -> Option(DtfTimeZone) {
  let lower = string.lowercase(s)
  case lower {
    "utc"
    | "etc/universal"
    | "etc/zulu"
    | "universal"
    | "zulu"
    | "etc/utc"
    | "etc/uct"
    | "uct" ->
      Some(FixedZone(
        case lower {
          "etc/utc" -> "Etc/UTC"
          "etc/uct" -> "Etc/UCT"
          "uct" -> "UCT"
          _ -> "UTC"
        },
        0,
      ))
    "gmt" | "etc/greenwich" | "greenwich" -> Some(FixedZone("GMT", 0))
    "etc/gmt" | "etc/gmt0" | "etc/gmt+0" | "etc/gmt-0" ->
      Some(FixedZone("Etc/GMT", 0))
    _ ->
      case parse_offset_zone(s) {
        Some(minutes) -> Some(FixedZone(format_offset_zone(minutes), minutes))
        None ->
          case etc_gmt_zone(lower) {
            Some(res) -> Some(res)
            None -> iana_zone(lower)
          }
      }
  }
}

/// The UTC offset (minutes) a formatter's zone has at a given instant. The
/// only place a DateTimeFormat offset ever comes from.
fn zone_offset_at(tz: DtfTimeZone, instant_ms: Int) -> Int {
  case tz {
    HostZone -> host_time.offset_at_utc_ms(instant_ms)
    FixedZone(offset_minutes:, ..) -> offset_minutes
    NamedZone(zone:) -> {
      // `temporal_tz.lookup` already loaded this zone's TZif to mint the
      // handle, so a failure here means the zoneinfo install vanished
      // underneath us. Falling back to UTC would silently render every
      // timestamp in the wrong zone — fail loudly instead.
      let assert Ok(offset_ns) =
        temporal_tz.offset_ns_at(zone, instant_ms * 1_000_000)
        as "intl: tzdata offset lookup failed for a zone lookup accepted"
      offset_ns / 60_000_000_000
    }
  }
}

/// Etc/GMT+N (UTC-N) and Etc/GMT-N (UTC+N), N in 1..14.
fn etc_gmt_zone(lower: String) -> Option(DtfTimeZone) {
  case string.split_once(lower, "etc/gmt") {
    Ok(#("", rest)) -> {
      let #(sign, num) = case string.pop_grapheme(rest) {
        Ok(#("+", n)) -> #(-1, n)
        Ok(#("-", n)) -> #(1, n)
        _ -> #(0, "")
      }
      case sign != 0, int.parse(num) {
        True, Ok(n) if n >= 1 && n <= 14 -> {
          let name = case sign < 0 {
            True -> "Etc/GMT+" <> int.to_string(n)
            False -> "Etc/GMT-" <> int.to_string(n)
          }
          case sign < 0 && n > 12 {
            True -> None
            False -> Some(FixedZone(name, sign * n * 60))
          }
        }
        _, _ -> None
      }
    }
    _ -> None
  }
}

/// Named IANA zones: the abbreviation zones plus structurally-valid
/// Area/Location identifiers. ECMA-402 §6.5: an implementation supporting
/// named zones must accept every IANA Zone/Link name, so resolution goes
/// through the system tzdata (Temporal's TZif backend) rather than a
/// hardcoded table; names absent from tzdata get the caller's
/// "Invalid time zone specified" RangeError instead of silently rendering as
/// UTC (offset 0).
fn iana_zone(lower: String) -> Option(DtfTimeZone) {
  case lower {
    "est"
    | "cst6cdt"
    | "est5edt"
    | "mst7mdt"
    | "pst8pdt"
    | "mst"
    | "hst"
    | "cet"
    | "eet"
    | "met"
    | "wet" -> tzdata_zone(lower)
    _ ->
      case string.split(lower, "/") {
        [area, ..rest] if rest != [] -> {
          let known_area =
            list.contains(
              [
                "africa", "america", "antarctica", "arctic", "asia", "atlantic",
                "australia", "europe", "indian", "pacific",
              ],
              area,
            )
          let parts_ok = list.all(rest, fn(p) { p != "" && is_zone_word(p) })
          case known_area && parts_ok {
            True -> tzdata_zone(lower)
            False -> None
          }
        }
        _ -> None
      }
  }
}

/// Resolve a zone id against the system tzdata. The `Zone` handle it returns
/// is what supplies the offset at each formatted instant — no offset is
/// snapshotted here. `None` when the name is not in tzdata.
fn tzdata_zone(lower: String) -> Option(DtfTimeZone) {
  temporal_tz.lookup(lower)
  |> result.map(NamedZone)
  |> option.from_result
}

fn is_zone_word(p: String) -> Bool {
  all_codepoints(p, fn(c) {
    digits.is_ascii_alnum_code(c) || c == 0x5f || c == 0x2b || c == 0x2d
  })
}

/// ±HH:MM offset time zones.
fn parse_offset_zone(s: String) -> Option(Int) {
  let #(sign, rest) = case string.pop_grapheme(s) {
    Ok(#("+", rest)) -> #(1, rest)
    Ok(#("-", rest)) -> #(-1, rest)
    _ -> #(0, s)
  }
  case sign {
    0 -> None
    _ ->
      case string.split(rest, ":") {
        [hh, mm] -> {
          // Guards can't contain function calls — bind the lengths first.
          let hh_len = string.length(hh)
          let mm_len = string.length(mm)
          case int.parse(hh), int.parse(mm) {
            Ok(h), Ok(m)
              if h >= 0
              && h <= 23
              && m >= 0
              && m <= 59
              && hh_len == 2
              && mm_len == 2
            -> Some(sign * { h * 60 + m })
            _, _ -> None
          }
        }
        [hhmm] ->
          case string.length(hhmm), int.parse(hhmm) {
            2, Ok(h) if h >= 0 && h <= 23 -> Some(sign * h * 60)
            4, Ok(v) -> {
              let h = v / 100
              let m = v % 100
              case h <= 23 && m <= 59 {
                True -> Some(sign * { h * 60 + m })
                False -> None
              }
            }
            _, _ -> None
          }
        _ -> None
      }
  }
}

fn format_offset_zone(minutes: Int) -> String {
  let sign = case minutes < 0 {
    True -> "-"
    False -> "+"
  }
  let m = int.absolute_value(minutes)
  sign <> fmt.pad2(m / 60) <> ":" <> fmt.pad2(m % 60)
}

// --- Intl.PluralRules ---

fn plural_rules_state(
  state: State(host),
  locales_v: JsValue,
  options_v: JsValue,
) -> Result(#(PluralRulesState, State(host)), Thrown(host)) {
  use #(requested, opts, state) <- result.try(constructor_prologue(
    state,
    locales_v,
    options_v,
    strict: False,
  ))
  use #(type_, state) <- result.try(get_enum_opt(
    state,
    opts,
    "type",
    [#("cardinal", Cardinal), #("ordinal", Ordinal)],
    Cardinal,
  ))
  use #(notation_kind, state) <- result.try(get_enum_opt(
    state,
    opts,
    "notation",
    notation_variants(),
    NkStandard,
  ))
  use #(notation, state) <- result.try(read_notation(state, opts, notation_kind))
  let #(_locale, data_locale, _ext) = resolve_locale(requested)
  use #(digits, state) <- result.try(digit_options(
    state,
    opts,
    0,
    3,
    notation_kind,
  ))
  Ok(#(
    PluralRulesState(
      locale: data_locale,
      plural_type: type_,
      notation:,
      digits:,
    ),
    state,
  ))
}

// --- Intl.ListFormat ---

fn list_format_state(
  state: State(host),
  locales_v: JsValue,
  options_v: JsValue,
) -> Result(#(ListFormatState, State(host)), Thrown(host)) {
  use #(requested, opts, state) <- result.try(constructor_prologue(
    state,
    locales_v,
    options_v,
    strict: True,
  ))
  use #(type_, state) <- result.try(get_enum_opt(
    state,
    opts,
    "type",
    [
      #("conjunction", Conjunction),
      #("disjunction", Disjunction),
      #("unit", UnitList),
    ],
    Conjunction,
  ))
  use #(style, state) <- result.try(get_enum_opt(
    state,
    opts,
    "style",
    list_format_style_variants(),
    LLong,
  ))
  let #(_locale, data_locale, _ext) = resolve_locale(requested)
  Ok(#(ListFormatState(locale: data_locale, list_type: type_, style:), state))
}

fn list_format_style_variants() -> List(#(String, ListFormatStyle)) {
  [#("long", LLong), #("short", LShort), #("narrow", LNarrow)]
}

// --- Intl.RelativeTimeFormat ---

fn rtf_state(
  state: State(host),
  locales_v: JsValue,
  options_v: JsValue,
) -> Result(#(RelativeTimeFormatState, State(host)), Thrown(host)) {
  use #(requested, opts, state) <- result.try(constructor_prologue(
    state,
    locales_v,
    options_v,
    strict: False,
  ))
  use #(nu, locale, state) <- result.try(resolve_nu_locale(
    state,
    opts,
    requested,
  ))
  use #(style, state) <- result.try(get_enum_opt(
    state,
    opts,
    "style",
    [#("long", RtfLong), #("short", RtfShort), #("narrow", RtfNarrow)],
    RtfLong,
  ))
  use #(numeric, state) <- result.try(get_enum_opt(
    state,
    opts,
    "numeric",
    [#("always", RtfAlways), #("auto", RtfAuto)],
    RtfAlways,
  ))
  Ok(#(
    RelativeTimeFormatState(locale:, style:, numeric:, numbering_system: nu),
    state,
  ))
}

// --- Intl.Segmenter ---

fn segmenter_state(
  state: State(host),
  locales_v: JsValue,
  options_v: JsValue,
) -> Result(#(SegmenterState, State(host)), Thrown(host)) {
  use #(requested, opts, state) <- result.try(constructor_prologue(
    state,
    locales_v,
    options_v,
    strict: True,
  ))
  use #(granularity, state) <- result.try(get_enum_opt(
    state,
    opts,
    "granularity",
    [
      #("grapheme", GGrapheme),
      #("word", GWord),
      #("sentence", GSentence),
    ],
    GGrapheme,
  ))
  let #(_locale, data_locale, _ext) = resolve_locale(requested)
  Ok(#(SegmenterState(locale: data_locale, granularity:), state))
}

// --- Intl.DisplayNames ---

fn display_names_state(
  state: State(host),
  locales_v: JsValue,
  options_v: JsValue,
) -> Result(#(DisplayNamesState, State(host)), Thrown(host)) {
  use #(requested, opts, state) <- result.try(constructor_prologue(
    state,
    locales_v,
    options_v,
    strict: True,
  ))
  use #(style, state) <- result.try(get_enum_opt(
    state,
    opts,
    "style",
    [#("narrow", WNarrow), #("short", WShort), #("long", WLong)],
    WLong,
  ))
  use #(type_, state) <- result.try(get_enum_opt(
    state,
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
  ))
  use type_ <- result.try(case type_ {
    Some(t) -> Ok(t)
    None ->
      throw_type(state, "Intl.DisplayNames constructor requires type option")
  })
  use #(fallback, state) <- result.try(get_enum_opt(
    state,
    opts,
    "fallback",
    [#("code", FbCode), #("none", FbNone)],
    FbCode,
  ))
  use #(language_display, state) <- result.try(get_enum_opt(
    state,
    opts,
    "languageDisplay",
    [#("dialect", LdDialect), #("standard", LdStandard)],
    LdDialect,
  ))
  let #(_locale, data_locale, _ext) = resolve_locale(requested)
  Ok(#(
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
    state,
  ))
}

// --- Intl.DurationFormat ---

/// A duration unit. The one place the ten unit names are written down: option
/// key, DurationRecord field, and singular unit tag all derive from it.
pub type DurationUnit {
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
/// Field access replaces the string lookups with a silent zero default.
pub type DurationRecord {
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

pub const zero_duration = DurationRecord(
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

fn duration_format_state(
  state: State(host),
  locales_v: JsValue,
  options_v: JsValue,
) -> Result(#(DurationFormatState, State(host)), Thrown(host)) {
  use #(requested, opts, state) <- result.try(constructor_prologue(
    state,
    locales_v,
    options_v,
    strict: True,
  ))
  use #(nu, locale, state) <- result.try(resolve_nu_locale(
    state,
    opts,
    requested,
  ))
  use #(base_style, state) <- result.try(get_enum_opt(
    state,
    opts,
    "style",
    [
      #("long", BsLong),
      #("short", BsShort),
      #("narrow", BsNarrow),
      #("digital", BsDigital),
    ],
    BsShort,
  ))
  // GetDurationUnitOptions for each unit in spec order, threading the
  // previous unit's INTERNAL style (`DurFractional` possible) — numeric
  // chaining and sub-second fraction folding depend on it.
  let unit = fn(state, name, prev) {
    duration_unit_options(state, opts, base_style, name, prev)
  }
  use #(years, prev, state) <- result.try(unit(state, DuYears, None))
  use #(months, prev, state) <- result.try(unit(state, DuMonths, Some(prev)))
  use #(weeks, prev, state) <- result.try(unit(state, DuWeeks, Some(prev)))
  use #(days, prev, state) <- result.try(unit(state, DuDays, Some(prev)))
  use #(hours, prev, state) <- result.try(unit(state, DuHours, Some(prev)))
  use #(minutes, prev, state) <- result.try(unit(state, DuMinutes, Some(prev)))
  use #(seconds, prev, state) <- result.try(unit(state, DuSeconds, Some(prev)))
  use #(milliseconds, prev, state) <- result.try(unit(
    state,
    DuMilliseconds,
    Some(prev),
  ))
  use #(microseconds, prev, state) <- result.try(unit(
    state,
    DuMicroseconds,
    Some(prev),
  ))
  use #(nanoseconds, _prev, state) <- result.try(unit(
    state,
    DuNanoseconds,
    Some(prev),
  ))
  use #(fractional, state) <- result.try(get_num_opt(
    state,
    opts,
    "fractionalDigits",
    0,
    9,
    None,
  ))
  Ok(#(
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
    state,
  ))
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

/// GetDurationUnitOptions (Intl.DurationFormat §1.1.6): one unit's resolved
/// style/display, plus the internal style to thread into the next unit.
fn duration_unit_options(
  state: State(host),
  opts: Option(Ref),
  base_style: DurationBaseStyle,
  unit: DurationUnit,
  prev_style: Option(DurationUnitStyle),
) -> Result(
  #(DurationUnitOptions, DurationUnitStyle, State(host)),
  Thrown(host),
) {
  let name = duration_unit_js_name(unit)
  use #(style_opt, state) <- result.try(get_enum_opt(
    state,
    opts,
    name,
    optional_variants(duration_style_variants(unit)),
    None,
  ))
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
    Some(st) -> #(st, DisplayAlways)
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
  use #(display, state) <- result.try(get_enum_opt(
    state,
    opts,
    name <> "Display",
    [#("auto", DisplayAuto), #("always", DisplayAlways)],
    display_default,
  ))
  // Step 7.
  use Nil <- result.try(
    case display == DisplayAlways && style == DurFractional {
      True ->
        throw_range(
          state,
          name <> "Display cannot be 'always' for fractional units",
        )
      False -> Ok(Nil)
    },
  )
  // Steps 8-9.
  use style <- result.try(case prev_style {
    Some(DurFractional) ->
      case style {
        DurFractional -> Ok(style)
        _ ->
          throw_range(
            state,
            name <> " style must be fractional after a fractional unit",
          )
      }
    Some(DurNumeric) | Some(DurTwoDigit) ->
      case style {
        DurFractional | DurNumeric | DurTwoDigit ->
          // Step 9.b: minutes/seconds after a numeric unit are zero-padded.
          Ok(case two_digit_unit {
            True -> DurTwoDigit
            False -> style
          })
        _ ->
          throw_range(
            state,
            name <> " style cannot be mixed with numeric styles",
          )
      }
    Some(DurLong) | Some(DurShort) | Some(DurNarrow) | None -> Ok(style)
  })
  Ok(#(DurationUnitOptions(style:, display:), style, state))
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
fn duration_list_style(base: DurationBaseStyle) -> ListFormatStyle {
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

fn resolved_options(
  service: IntlService,
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  run({
    use #(_ref, data) <- result.try(branded(
      state,
      this,
      service,
      "Intl." <> service_name(service) <> ".prototype.resolvedOptions",
    ))
    // Property order and presence are observable: each arm enumerates its
    // state's fields in the spec's resolvedOptions order, skipping absent
    // (None) internal slots.
    let #(state, props) = case data {
      LocaleData(l) -> #(state, [#("locale", JsString(l.locale))])
      CollatorData(c) -> #(state, [
        #("locale", JsString(c.locale)),
        #("usage", JsString(value.collator_usage_to_js_string(c.usage))),
        #(
          "sensitivity",
          JsString(value.collator_sensitivity_to_js_string(c.sensitivity)),
        ),
        #("ignorePunctuation", JsBool(c.ignore_punctuation)),
        #("collation", JsString(c.collation)),
        #("numeric", JsBool(c.numeric)),
        #("caseFirst", JsString(value.case_first_to_js_string(c.case_first))),
      ])
      NumberFormatData(nf) -> {
        let dg = nf.digits
        // The style-conditional slots exist exactly for the style that
        // selects them, so resolvedOptions omits the rest.
        let #(currency, currency_display, currency_sign) = case nf.style {
          StyleCurrency(currency:, display:, sign:) -> #(
            Some(JsString(currency)),
            Some(currency_display_js(display)),
            Some(currency_sign_js(sign)),
          )
          StyleDecimal | StylePercent | StyleUnit(..) -> #(None, None, None)
        }
        let #(unit, unit_display) = case nf.style {
          StyleUnit(unit:, display:) -> #(
            Some(JsString(unit)),
            Some(unit_display_js(display)),
          )
          StyleDecimal | StylePercent | StyleCurrency(..) -> #(None, None)
        }
        #(
          state,
          present_pairs([
            #("locale", Some(JsString(nf.locale))),
            #("numberingSystem", Some(JsString(nf.numbering_system))),
            #("style", Some(JsString(value.num_style_to_js_string(nf.style)))),
            #("currency", currency),
            #("currencyDisplay", currency_display),
            #("currencySign", currency_sign),
            #("unit", unit),
            #("unitDisplay", unit_display),
            ..digit_option_pairs(dg, [
              #("useGrouping", Some(use_grouping_js(nf.use_grouping))),
              #(
                "notation",
                Some(JsString(value.notation_to_js_string(nf.notation))),
              ),
              #("compactDisplay", compact_display_of(nf.notation)),
              #(
                "signDisplay",
                Some(JsString(value.sign_display_to_js_string(nf.sign_display))),
              ),
              ..digit_rounding_pairs(dg)
            ])
          ]),
        )
      }
      DateTimeFormatData(d) -> #(
        state,
        present_pairs([
          #("locale", Some(JsString(d.locale))),
          #("calendar", Some(JsString(d.calendar))),
          #("numberingSystem", Some(JsString(d.numbering_system))),
          #("timeZone", Some(JsString(value.dtf_time_zone_id(d.time_zone)))),
          #(
            "hourCycle",
            option.map(d.hour_cycle, fn(hc) {
              JsString(value.hour_cycle_to_js_string(hc))
            }),
          ),
          #(
            "hour12",
            option.map(d.hour_cycle, fn(hc) {
              JsBool(case hc {
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
            option.map(d.fractional_second_digits, value.from_int),
          ),
          #("timeZoneName", option.map(d.time_zone_name, tz_name_width_js)),
          #(
            "dateStyle",
            option.map(d.date_style, fn(s) {
              JsString(value.date_style_to_js_string(s))
            }),
          ),
          #(
            "timeStyle",
            option.map(d.time_style, fn(s) {
              JsString(value.time_style_to_js_string(s))
            }),
          ),
        ]),
      )
      PluralRulesData(p) -> {
        let dg = p.digits
        let #(state, cats) =
          alloc_array(
            state,
            fmt.plural_categories_en(p.plural_type)
              |> list.map(fmt.plural_category_to_js_string)
              |> list.map(JsString),
          )
        #(
          state,
          present_pairs([
            #("locale", Some(JsString(p.locale))),
            #(
              "type",
              Some(JsString(value.plural_type_to_js_string(p.plural_type))),
            ),
            #(
              "notation",
              Some(JsString(value.notation_to_js_string(p.notation))),
            ),
            #("compactDisplay", compact_display_of(p.notation)),
            ..digit_option_pairs(dg, [
              #("pluralCategories", Some(cats)),
              ..digit_rounding_pairs(dg)
            ])
          ]),
        )
      }
      ListFormatData(l) -> #(state, [
        #("locale", JsString(l.locale)),
        #("type", JsString(value.list_format_type_to_js_string(l.list_type))),
        #("style", JsString(value.list_format_style_to_js_string(l.style))),
      ])
      RelativeTimeFormatData(r) -> #(state, [
        #("locale", JsString(r.locale)),
        #("style", JsString(value.rtf_style_to_js_string(r.style))),
        #("numeric", JsString(value.rtf_numeric_to_js_string(r.numeric))),
        #("numberingSystem", JsString(r.numbering_system)),
      ])
      SegmenterData(s) -> #(state, [
        #("locale", JsString(s.locale)),
        #(
          "granularity",
          JsString(value.granularity_to_js_string(s.granularity)),
        ),
      ])
      DisplayNamesData(d) -> #(
        state,
        present_pairs([
          #("locale", Some(JsString(d.locale))),
          #("style", Some(JsString(value.name_width_to_js_string(d.style)))),
          #(
            "type",
            Some(
              JsString(value.display_names_type_to_js_string(d.display_type)),
            ),
          ),
          #(
            "fallback",
            Some(
              JsString(value.display_names_fallback_to_js_string(d.fallback)),
            ),
          ),
          #(
            "languageDisplay",
            option.map(d.language_display, fn(ld) {
              JsString(value.language_display_to_js_string(ld))
            }),
          ),
        ]),
      )
      DurationFormatData(df) -> #(
        state,
        list.flatten([
          [
            #("locale", JsString(df.locale)),
            #("numberingSystem", JsString(df.numbering_system)),
            #(
              "style",
              JsString(value.duration_base_style_to_js_string(df.style)),
            ),
          ],
          list.flat_map(duration_unit_list(df), fn(u) {
            let #(unit, o) = u
            let name = duration_unit_js_name(unit)
            [
              #(name, JsString(value.duration_unit_style_to_js_string(o.style))),
              #(
                name <> "Display",
                JsString(value.duration_display_to_js_string(o.display)),
              ),
            ]
          }),
          case df.fractional_digits {
            Some(f) -> [#("fractionalDigits", value.from_int(f))]
            None -> []
          },
        ]),
      )
      // %Segments%/%SegmentIterator% never expose resolvedOptions.
      SegmentsData(_) | SegmentIteratorData(_) -> #(state, [])
    }
    let #(state, obj) = alloc_pojo(state, props)
    Ok(#(obj, state))
  })
}

/// `[[UseGrouping]]` as its JS resolvedOptions value: never is the boolean
/// `false`, everything else its string spelling.
fn use_grouping_js(g: value.IntlUseGrouping) -> JsValue {
  case g {
    GroupingNever -> JsBool(False)
    GroupingAuto -> JsString("auto")
    GroupingAlways -> JsString("always")
    GroupingMin2 -> JsString("min2")
  }
}

fn currency_display_js(v: value.CurrencyDisplay) -> JsValue {
  JsString(value.currency_display_to_js_string(v))
}

fn currency_sign_js(v: value.CurrencySign) -> JsValue {
  JsString(value.currency_sign_to_js_string(v))
}

fn unit_display_js(v: value.UnitDisplay) -> JsValue {
  JsString(value.unit_display_to_js_string(v))
}

/// `[[CompactDisplay]]` as its resolvedOptions value — present only when the
/// notation is compact.
fn compact_display_of(n: Notation) -> Option(JsValue) {
  case n {
    NotationCompact(display:) ->
      Some(JsString(value.compact_display_to_js_string(display)))
    NotationStandard | NotationScientific | NotationEngineering -> None
  }
}

fn name_width_js(v: NameWidth) -> JsValue {
  JsString(value.name_width_to_js_string(v))
}

fn numeric_width_js(v: NumericWidth) -> JsValue {
  JsString(value.numeric_width_to_js_string(v))
}

fn month_width_js(v: MonthWidth) -> JsValue {
  JsString(value.month_width_to_js_string(v))
}

fn tz_name_width_js(v: TimeZoneNameWidth) -> JsValue {
  JsString(value.time_zone_name_width_to_js_string(v))
}

/// The integer/fraction/significant digit resolvedOptions pairs shared by
/// NumberFormat and PluralRules, prepended to `rest`.
fn digit_option_pairs(
  dg: IntlDigitOptions,
  rest: List(#(String, Option(JsValue))),
) -> List(#(String, Option(JsValue))) {
  [
    #("minimumIntegerDigits", Some(value.from_int(dg.minimum_integer_digits))),
    #(
      "minimumFractionDigits",
      option.map(dg.fraction_digits, fn(p) { value.from_int(p.0) }),
    ),
    #(
      "maximumFractionDigits",
      option.map(dg.fraction_digits, fn(p) { value.from_int(p.1) }),
    ),
    #(
      "minimumSignificantDigits",
      option.map(dg.significant_digits, fn(p) { value.from_int(p.0) }),
    ),
    #(
      "maximumSignificantDigits",
      option.map(dg.significant_digits, fn(p) { value.from_int(p.1) }),
    ),
    ..rest
  ]
}

/// The roundingIncrement/roundingMode/roundingPriority/trailingZeroDisplay
/// resolvedOptions tail shared by NumberFormat and PluralRules.
fn digit_rounding_pairs(
  dg: IntlDigitOptions,
) -> List(#(String, Option(JsValue))) {
  [
    #("roundingIncrement", Some(value.from_int(dg.rounding_increment))),
    #(
      "roundingMode",
      Some(JsString(value.rounding_mode_to_js_string(dg.rounding_mode))),
    ),
    #(
      "roundingPriority",
      Some(JsString(value.rounding_priority_to_js_string(dg.rounding_priority))),
    ),
    #(
      "trailingZeroDisplay",
      Some(
        JsString(value.trailing_zero_display_to_js_string(
          dg.trailing_zero_display,
        )),
      ),
    ),
  ]
}

// ============================================================================
// Bound method getters (format / compare)
// ============================================================================

/// The `format` / `compare` accessor getters (§10.3.3, §15.3.3, §11.3.3): the
/// bound function is created once and cached in the receiver's own state, so
/// the getter is idempotent. `BoundGetterService` has exactly the three
/// services with such a cache slot — no "service without a slot" arm.
fn bound_getter(
  service: BoundGetterService,
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let method =
    "Intl."
    <> service_name(value.bound_getter_service(service))
    <> " bound method getter"
  run(case service {
    BgCollator -> {
      use #(ref, c) <- result.try(branded_collator(state, this, method))
      cached_bound_fn(state, service, ref, c.bound_compare, 2, fn(heap, fn_ref) {
        write_intl_data(
          heap,
          ref,
          CollatorData(CollatorState(..c, bound_compare: Some(fn_ref))),
        )
      })
    }
    BgNumberFormat -> {
      use #(ref, nf) <- result.try(branded_number_format(state, this, method))
      cached_bound_fn(state, service, ref, nf.bound_format, 1, fn(heap, fn_ref) {
        write_intl_data(
          heap,
          ref,
          NumberFormatData(NumberFormatState(..nf, bound_format: Some(fn_ref))),
        )
      })
    }
    BgDateTimeFormat -> {
      use #(ref, d) <- result.try(branded_date_time_format(state, this, method))
      cached_bound_fn(state, service, ref, d.bound_format, 1, fn(heap, fn_ref) {
        write_intl_data(
          heap,
          ref,
          DateTimeFormatData(
            DateTimeFormatState(..d, bound_format: Some(fn_ref)),
          ),
        )
      })
    }
  })
}

/// Return the already-cached bound function, or allocate one and hand it to
/// `store` (which writes it into the receiver's cache slot).
fn cached_bound_fn(
  state: State(host),
  service: BoundGetterService,
  target: Ref,
  cached: Option(Ref),
  arity: Int,
  store: fn(Heap(host), Ref) -> Heap(host),
) -> Result(#(JsValue, State(host)), Thrown(host)) {
  case cached {
    Some(fn_ref) -> Ok(#(JsObject(fn_ref), state))
    None -> {
      let #(heap, fn_ref) =
        common.alloc_native_fn(
          state.heap,
          state.builtins.function.prototype,
          IntlNative(IntlBoundMethod(service:, target:)),
          "",
          arity,
        )
      Ok(#(JsObject(fn_ref), State(..state, heap: store(heap, fn_ref))))
    }
  }
}

/// The bound `format` / `compare` function itself: `target` is the receiver
/// captured by `bound_getter`, and its brand is re-checked (the instance's
/// state can only have been swapped by another Intl object of the same shape).
fn bound_method(
  service: BoundGetterService,
  target: Ref,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let this = JsObject(target)
  let method = "bound Intl method"
  run(case service {
    BgNumberFormat -> {
      use #(_ref, nf) <- result.try(branded_number_format(state, this, method))
      use #(parts, state) <- result.map(nf_format_parts(
        state,
        nf,
        first_arg_or_undefined(args),
      ))
      #(JsString(fmt.parts_to_string(parts)), state)
    }
    BgDateTimeFormat -> {
      use #(_ref, d) <- result.try(branded_date_time_format(state, this, method))
      use #(parts, state) <- result.map(dtf_format_parts(
        state,
        d,
        first_arg_or_undefined(args),
      ))
      #(JsString(fmt.parts_to_string(parts)), state)
    }
    BgCollator -> {
      use #(_ref, c) <- result.try(branded_collator(state, this, method))
      use #(a, state) <- result.try(coerce.js_to_string(
        state,
        first_arg_or_undefined(args),
      ))
      use #(b, state) <- result.map(coerce.js_to_string(
        state,
        helpers.arg_at(args, 1),
      ))
      #(value.from_int(collator_compare(c, a, b)), state)
    }
  })
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
fn to_intl_number(
  state: State(host),
  v: JsValue,
) -> Result(#(value.JsNum, State(host)), Thrown(host)) {
  case v {
    // num_from_int saturates out-of-range BigInts to ±Infinity; a bare
    // int.to_float would badarg (and kill the VM) on e.g. 10n ** 400n.
    value.JsBigInt(value.BigInt(n)) -> Ok(#(value.num_from_int(n), state))
    _ -> coerce.js_to_number(state, v)
  }
}

fn nf_format_parts(
  state: State(host),
  nf: NumberFormatState,
  x: JsValue,
) -> Result(#(List(fmt.Part), State(host)), Thrown(host)) {
  let opts = num_opts_from_nf(nf)
  let nu = nf.numbering_system
  // ToIntlMathematicalValue keeps decimal strings exact.
  case x {
    JsString(str) ->
      case is_plain_decimal(string.trim(str)) {
        True ->
          Ok(#(
            fmt.apply_numbering_system(
              fmt.format_decimal_string_parts(opts, string.trim(str)),
              nu,
              fmt.is_number_digit,
            ),
            state,
          ))
        False -> nf_format_number(state, x, opts, nu)
      }
    _ -> nf_format_number(state, x, opts, nu)
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
  state: State(host),
  x: JsValue,
  opts: fmt.NumOpts,
  nu: String,
) -> Result(#(List(fmt.Part), State(host)), Thrown(host)) {
  use #(n, state) <- result.try(to_intl_number(state, x))
  let parts = case n {
    value.NaN -> fmt.format_nan_parts(opts)
    value.Infinity -> fmt.format_infinity_parts(opts, False)
    value.NegInfinity -> fmt.format_infinity_parts(opts, True)
    value.Finite(f) -> fmt.format_number_parts(opts, f)
  }
  Ok(#(fmt.apply_numbering_system(parts, nu, fmt.is_number_digit), state))
}

fn nf_range_parts(
  state: State(host),
  nf: NumberFormatState,
  x_v: JsValue,
  y_v: JsValue,
) -> Result(#(List(fmt.RangePart), State(host)), Thrown(host)) {
  use Nil <- result.try(case x_v, y_v {
    JsUndefined, _ | _, JsUndefined ->
      throw_type(state, "Invalid range arguments")
    _, _ -> Ok(Nil)
  })
  use #(x, state) <- result.try(to_intl_number(state, x_v))
  use #(y, state) <- result.try(to_intl_number(state, y_v))
  use Nil <- result.try(case x, y {
    value.NaN, _ | _, value.NaN ->
      throw_range(state, "Invalid range argument: NaN")
    _, _ -> Ok(Nil)
  })
  // Format the original values: decimal strings stay exact (they can exceed
  // float precision), everything else uses the coerced number.
  let x_fmt = case x_v {
    JsString(_) -> x_v
    _ -> JsNumber(x)
  }
  let y_fmt = case y_v {
    JsString(_) -> y_v
    _ -> JsNumber(y)
  }
  use #(x_parts, state) <- result.try(nf_format_parts(state, nf, x_fmt))
  use #(y_parts, state) <- result.try(nf_format_parts(state, nf, y_fmt))
  Ok(#(
    fmt.format_range_combine(fmt.locale_key(nf.locale), x_parts, y_parts),
    state,
  ))
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
fn throw_zoned(state: State(host)) -> Result(a, Thrown(host)) {
  throw_type(
    state,
    "Temporal.ZonedDateTime cannot be formatted with Intl.DateTimeFormat; use Temporal.ZonedDateTime.prototype.toLocaleString instead",
  )
}

fn accept_temporal(
  state: State(host),
  t: TemporalFormattable,
) -> Result(AcceptedTemporal, Thrown(host)) {
  case t {
    TfInstant(epoch_ns:) -> Ok(AtInstant(epoch_ns:))
    TfPlain(p) -> Ok(AtPlain(p))
    TfZoned -> throw_zoned(state)
  }
}

/// IsTemporalObject — recognize Temporal values handed to format methods.
fn dtf_temporal_value(
  state: State(host),
  v: JsValue,
) -> Option(TemporalFormattable) {
  case v {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(
          kind: TemporalDateSlot(year:, month:, day:, calendar:),
          ..,
        )) ->
          Some(
            TfPlain(PDate(
              year:,
              month:,
              day:,
              calendar: tcal.identifier(calendar),
            )),
          )
        Some(ObjectSlot(
          kind: TemporalYearMonthSlot(year:, month:, day:, calendar:),
          ..,
        )) ->
          Some(
            TfPlain(PYearMonth(
              year:,
              month:,
              day:,
              calendar: tcal.identifier(calendar),
            )),
          )
        Some(ObjectSlot(
          kind: TemporalMonthDaySlot(month:, day:, ref_year:, calendar:),
          ..,
        )) ->
          Some(
            TfPlain(PMonthDay(
              month:,
              day:,
              ref_year:,
              calendar: tcal.identifier(calendar),
            )),
          )
        Some(ObjectSlot(
          kind: TemporalTimeSlot(
            hour:,
            minute:,
            second:,
            millisecond:,
            microsecond: _,
            nanosecond: _,
          ),
          ..,
        )) -> Some(TfPlain(PTime(hour:, minute:, second:, millisecond:)))
        Some(ObjectSlot(
          kind: TemporalDateTimeSlot(
            year:,
            month:,
            day:,
            hour:,
            minute:,
            second:,
            millisecond:,
            microsecond: _,
            nanosecond: _,
            calendar:,
          ),
          ..,
        )) ->
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
        Some(ObjectSlot(kind: TemporalInstantSlot(epoch_ns:), ..)) ->
          Some(TfInstant(epoch_ns:))
        Some(ObjectSlot(kind: TemporalZonedDateTimeSlot(..), ..)) ->
          Some(TfZoned)
        _ -> None
      }
    _ -> None
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
        ..value.empty_dtf_components,
        year: Some(WNumeric),
        month: Some(MonthNum(WNumeric)),
      ),
      True,
    )
    PMonthDay(..) -> #(
      [DtfMonth, DtfDay],
      [DtfMonth, DtfDay],
      DtfComponents(
        ..value.empty_dtf_components,
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
  state: State(host),
  d: DateTimeFormatState,
  t: TemporalFormattable,
) -> Result(#(DateTimeFormatState, State(host)), Thrown(host)) {
  case t {
    TfZoned -> throw_zoned(state)
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
        True -> Ok(#(d, state))
        False ->
          Ok(#(
            with_components(d, merge_components(d.components, time_defaults())),
            state,
          ))
      }
    TfPlain(p) -> {
      let cal_ok = case p {
        PDate(calendar:, ..) | PDateTime(calendar:, ..) ->
          calendar == "iso8601" || calendar == d.calendar
        PYearMonth(calendar:, ..) | PMonthDay(calendar:, ..) ->
          calendar == d.calendar
        PTime(..) -> True
      }
      use Nil <- result.try(case cal_ok {
        True -> Ok(Nil)
        False ->
          throw_range(
            state,
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
          use Nil <- result.try(case style_ok {
            True -> Ok(Nil)
            False ->
              throw_type(
                state,
                "DateTimeFormat has no suitable format for this Temporal type",
              )
          })
          Ok(#(
            with_components(d, keep_components(d.components, allowed)),
            state,
          ))
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
                [] ->
                  Ok(#(
                    with_components(d, DtfComponents(..defaults, era:)),
                    state,
                  ))
                _ ->
                  throw_type(
                    state,
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
              Ok(#(with_components(d, DtfComponents(..kept, era:)), state))
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
/// through the formatter's zone like a Number time value.
fn dtf_temporal_fields(
  d: DateTimeFormatState,
  t: AcceptedTemporal,
) -> #(fmt.DateFields, Int) {
  case t {
    AtInstant(epoch_ns:) -> {
      let ms = floor_div(epoch_ns, 1_000_000)
      let offset = zone_offset_at(d.time_zone, ms)
      #(fmt.fields_from_epoch_ms(int.to_float(ms), offset), offset)
    }
    // Plain types carry no instant, so a requested timeZoneName can only show
    // the zone's offset now.
    AtPlain(p) -> #(
      plain_temporal_fields(p),
      zone_offset_at(d.time_zone, host_time.now_ms()),
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
  state: State(host),
  d: DateTimeFormatState,
  date_v: JsValue,
) -> Result(#(List(fmt.Part), State(host)), Thrown(host)) {
  case dtf_temporal_value(state, date_v) {
    Some(t) -> {
      use #(d, state) <- result.try(dtf_temporal_state(state, d, t))
      use accepted <- result.try(accept_temporal(state, t))
      let #(fields, offset) = dtf_temporal_fields(d, accepted)
      let parts = build_dtf_parts(d, fields, offset)
      Ok(#(
        fmt.apply_numbering_system(
          parts,
          d.numbering_system,
          fmt.is_date_numeric,
        ),
        state,
      ))
    }
    None -> dtf_format_parts_number(state, d, date_v)
  }
}

fn dtf_format_parts_number(
  state: State(host),
  d: DateTimeFormatState,
  date_v: JsValue,
) -> Result(#(List(fmt.Part), State(host)), Thrown(host)) {
  use #(fields, offset, state) <- result.try(dtf_fields_number(state, d, date_v))
  let parts = build_dtf_parts(d, fields, offset)
  Ok(#(
    fmt.apply_numbering_system(parts, d.numbering_system, fmt.is_date_numeric),
    state,
  ))
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
  let year_str = fn(width) {
    case width {
      WTwoDigit -> fmt.pad2(display_year % 100)
      WNumeric -> int.to_string(display_year)
    }
  }
  let weekday_parts = case weekday {
    Some(w) -> [#(PWeekday, fmt.weekday_name(fields.week_day, w))]
    None -> []
  }
  // Date portion.
  let date_parts = case month {
    Some(MonthName(mw)) -> {
      let m_part = [#(PMonth, fmt.month_name(fields.month, mw))]
      let d_part = case day {
        Some(dw) -> [#(PLiteral, " "), #(PDay, day_str(dw, fields.day))]
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
        Some(MonthNum(mw)) -> Some(month_num_str(mw, fields.month))
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
      let d_pair = #(PDay, option.map(day, fn(dw) { day_str(dw, fields.day) }))
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
      let name = tz_display(value.dtf_time_zone_id(d.time_zone), width, offset)
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

fn day_str(width: NumericWidth, day: Int) -> String {
  case width {
    WTwoDigit -> fmt.pad2(day)
    WNumeric -> int.to_string(day)
  }
}

fn month_num_str(width: NumericWidth, month: Int) -> String {
  case width {
    WTwoDigit -> fmt.pad2(month)
    WNumeric -> int.to_string(month)
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

/// Render a formatter's [[TimeZone]] under a timeZoneName width.
fn tz_display(name: String, width: TimeZoneNameWidth, offset: Int) -> String {
  case name, width {
    "UTC", TzShort | "UTC", TzShortGeneric -> "UTC"
    "UTC", TzLong | "UTC", TzLongGeneric -> "Coordinated Universal Time"
    "UTC", TzShortOffset -> "GMT"
    "UTC", TzLongOffset -> "GMT"
    _, TzLong | _, TzLongOffset | _, TzLongGeneric -> gmt_offset(offset, True)
    _, TzShort | _, TzShortOffset | _, TzShortGeneric ->
      gmt_offset(offset, False)
  }
}

fn gmt_offset(offset: Int, long: Bool) -> String {
  case offset {
    0 -> "GMT"
    _ -> {
      let sign = case offset < 0 {
        True -> "-"
        False -> "+"
      }
      let m = int.absolute_value(offset)
      let h = m / 60
      let mm = m % 60
      case long {
        True -> "GMT" <> sign <> fmt.pad2(h) <> ":" <> fmt.pad2(mm)
        False ->
          case mm {
            0 -> "GMT" <> sign <> int.to_string(h)
            _ -> "GMT" <> sign <> int.to_string(h) <> ":" <> fmt.pad2(mm)
          }
      }
    }
  }
}

fn dtf_range_parts(
  state: State(host),
  d: DateTimeFormatState,
  x_v: JsValue,
  y_v: JsValue,
) -> Result(#(List(fmt.RangePart), State(host)), Thrown(host)) {
  use Nil <- result.try(case x_v, y_v {
    JsUndefined, _ | _, JsUndefined ->
      throw_type(state, "Invalid range arguments")
    _, _ -> Ok(Nil)
  })
  // ToDateTimeFormattable runs on both arguments (in order) before the
  // SameTemporalType check: Temporal objects pass through, everything else
  // goes through ToNumber.
  let tx = dtf_temporal_value(state, x_v)
  let ty = dtf_temporal_value(state, y_v)
  use #(x_v, state) <- result.try(case tx {
    Some(_) -> Ok(#(x_v, state))
    None -> {
      use #(n, state) <- result.map(coerce.js_to_number(state, x_v))
      #(JsNumber(n), state)
    }
  })
  use #(y_v, state) <- result.try(case ty {
    Some(_) -> Ok(#(y_v, state))
    None -> {
      use #(n, state) <- result.map(coerce.js_to_number(state, y_v))
      #(JsNumber(n), state)
    }
  })
  use #(d, state) <- result.try(case tx, ty {
    None, None -> Ok(#(d, state))
    Some(a), Some(b) ->
      case same_temporal_kind(a, b) {
        False ->
          throw_type(
            state,
            "Intl.DateTimeFormat range arguments must be of the same type",
          )
        True -> {
          // Validates x (calendar / suitable format) and yields the
          // per-type adjusted components; y is validated separately.
          use #(adjusted, state) <- result.try(dtf_temporal_state(state, d, a))
          use #(_d_y, state) <- result.try(dtf_temporal_state(state, d, b))
          Ok(#(adjusted, state))
        }
      }
    _, _ ->
      throw_type(
        state,
        "Intl.DateTimeFormat range arguments must be of the same type",
      )
  })
  use collapsed <- result.try(dtf_collapsed_range(state, d, x_v, y_v))
  case collapsed {
    Some(#(parts, state)) -> Ok(#(parts, state))
    None -> {
      use #(x_parts, state) <- result.try(dtf_format_parts(state, d, x_v))
      use #(y_parts, state) <- result.try(dtf_format_parts(state, d, y_v))
      let sourced = fn(p: fmt.Part, source) { fmt.RangePart(p.0, p.1, source) }
      case fmt.parts_to_string(x_parts) == fmt.parts_to_string(y_parts) {
        True -> Ok(#(list.map(x_parts, sourced(_, fmt.SourceShared)), state))
        False ->
          Ok(#(
            list.flatten([
              list.map(x_parts, sourced(_, fmt.SourceStart)),
              [sourced(#(PLiteral, " – "), fmt.SourceShared)],
              list.map(y_parts, sourced(_, fmt.SourceEnd)),
            ]),
            state,
          ))
      }
    }
  }
}

/// "Jan 3 – 5, 2019": collapse a named-month date-only range that differs
/// only in the day.
fn dtf_collapsed_range(
  state: State(host),
  d: DateTimeFormatState,
  x_v: JsValue,
  y_v: JsValue,
) -> Result(Option(#(List(fmt.RangePart), State(host))), Thrown(host)) {
  let c = d.components
  let date_only =
    c.hour == None && c.minute == None && c.second == None && c.weekday == None
  // A named month with a year and a day is the only shape we collapse.
  case c.month, c.year, c.day, date_only {
    Some(MonthName(month_width)), Some(year_width), Some(day_width), True -> {
      use #(xf, state) <- result.try(dtf_fields(state, d, x_v))
      use #(yf, state) <- result.try(dtf_fields(state, d, y_v))
      {
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
          False -> Ok(None)
          True ->
            case xf.month == yf.month, xf.day != yf.day {
              True, True -> {
                let parts = [
                  fmt.RangePart(PMonth, mname(xf.month), fmt.SourceShared),
                  fmt.RangePart(PLiteral, " ", fmt.SourceShared),
                  fmt.RangePart(
                    PDay,
                    day_str(day_style, xf.day),
                    fmt.SourceStart,
                  ),
                  fmt.RangePart(PLiteral, " – ", fmt.SourceShared),
                  fmt.RangePart(PDay, day_str(day_style, yf.day), fmt.SourceEnd),
                  fmt.RangePart(PLiteral, ", ", fmt.SourceShared),
                  fmt.RangePart(PYear, year_str, fmt.SourceShared),
                ]
                Ok(Some(#(parts, state)))
              }
              False, _ -> {
                let parts = [
                  fmt.RangePart(PMonth, mname(xf.month), fmt.SourceStart),
                  fmt.RangePart(PLiteral, " ", fmt.SourceStart),
                  fmt.RangePart(
                    PDay,
                    day_str(day_style, xf.day),
                    fmt.SourceStart,
                  ),
                  fmt.RangePart(PLiteral, " – ", fmt.SourceShared),
                  fmt.RangePart(PMonth, mname(yf.month), fmt.SourceEnd),
                  fmt.RangePart(PLiteral, " ", fmt.SourceEnd),
                  fmt.RangePart(PDay, day_str(day_style, yf.day), fmt.SourceEnd),
                  fmt.RangePart(PLiteral, ", ", fmt.SourceShared),
                  fmt.RangePart(PYear, year_str, fmt.SourceShared),
                ]
                Ok(Some(#(parts, state)))
              }
              True, False -> Ok(None)
            }
        }
      }
    }
    _, _, _, _ -> Ok(None)
  }
}

/// Compute the civil fields a DTF instance would use for a value.
fn dtf_fields(
  state: State(host),
  d: DateTimeFormatState,
  date_v: JsValue,
) -> Result(#(fmt.DateFields, State(host)), Thrown(host)) {
  case dtf_temporal_value(state, date_v) {
    Some(t) -> {
      use accepted <- result.map(accept_temporal(state, t))
      let #(fields, _offset) = dtf_temporal_fields(d, accepted)
      #(fields, state)
    }
    None -> {
      use #(fields, _offset, state) <- result.map(dtf_fields_number(
        state,
        d,
        date_v,
      ))
      #(fields, state)
    }
  }
}

/// Civil fields for a Number time value, plus the zone offset used.
fn dtf_fields_number(
  state: State(host),
  d: DateTimeFormatState,
  date_v: JsValue,
) -> Result(#(fmt.DateFields, Int, State(host)), Thrown(host)) {
  use #(tv, state) <- result.try(case date_v {
    JsUndefined -> Ok(#(value.Finite(int.to_float(host_time.now_ms())), state))
    _ -> coerce.js_to_number(state, date_v)
  })
  use tv_f <- result.try(case tv {
    value.Finite(f) -> {
      // TimeClip truncates toward zero before the range check.
      let f = int.to_float(float.truncate(f))
      case float.absolute_value(f) <=. 8.64e15 {
        True -> Ok(f)
        False -> throw_range(state, "Invalid time value")
      }
    }
    _ -> throw_range(state, "Invalid time value")
  })
  let offset = zone_offset_at(d.time_zone, float.truncate(tv_f))
  Ok(#(fmt.fields_from_epoch_ms(tv_f, offset), offset, state))
}

// ============================================================================
// Prototype methods (IntlMethod dispatch)
// ============================================================================

fn run_method(
  service: IntlService,
  method: IntlMethodName,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let arg0 = first_arg_or_undefined(args)
  let arg1 = helpers.arg_at(args, 1)
  let js_name =
    "Intl."
    <> service_name(service)
    <> ".prototype."
    <> intl_method_js_name(method)
  run({
    use #(ref, data) <- result.try(branded(state, this, service, js_name))
    case method, data {
      IntlFormatToParts, NumberFormatData(nf) -> {
        use #(parts, state) <- result.try(nf_format_parts(state, nf, arg0))
        let #(state, arr) = parts_to_js(state, parts)
        Ok(#(arr, state))
      }
      IntlFormatRange, NumberFormatData(nf) -> {
        use #(parts, state) <- result.try(nf_range_parts(state, nf, arg0, arg1))
        Ok(#(JsString(range_parts_to_string(parts)), state))
      }
      IntlFormatRangeToParts, NumberFormatData(nf) -> {
        use #(parts, state) <- result.try(nf_range_parts(state, nf, arg0, arg1))
        let #(state, arr) = parts_to_js_sourced(state, parts)
        Ok(#(arr, state))
      }
      IntlFormatToParts, DateTimeFormatData(d) -> {
        use #(parts, state) <- result.try(dtf_format_parts(state, d, arg0))
        let #(state, arr) = parts_to_js(state, parts)
        Ok(#(arr, state))
      }
      IntlFormatRange, DateTimeFormatData(d) -> {
        use #(parts, state) <- result.try(dtf_range_parts(state, d, arg0, arg1))
        Ok(#(JsString(range_parts_to_string(parts)), state))
      }
      IntlFormatRangeToParts, DateTimeFormatData(d) -> {
        use #(parts, state) <- result.try(dtf_range_parts(state, d, arg0, arg1))
        let #(state, arr) = parts_to_js_sourced(state, parts)
        Ok(#(arr, state))
      }
      IntlSelect, PluralRulesData(p) -> {
        use #(n, state) <- result.try(coerce.js_to_number(state, arg0))
        Ok(#(
          JsString(fmt.plural_category_to_js_string(plural_select(p, n))),
          state,
        ))
      }
      IntlSelectRange, PluralRulesData(_) -> {
        use Nil <- result.try(case arg0, arg1 {
          JsUndefined, _ | _, JsUndefined ->
            throw_type(state, "Invalid selectRange arguments")
          _, _ -> Ok(Nil)
        })
        use #(x, state) <- result.try(coerce.js_to_number(state, arg0))
        use #(y, state) <- result.try(coerce.js_to_number(state, arg1))
        use Nil <- result.try(case x, y {
          value.NaN, _ | _, value.NaN ->
            throw_range(state, "Invalid selectRange argument: NaN")
          _, _ -> Ok(Nil)
        })
        // CLDR en plural ranges resolve to "other" for all combinations.
        Ok(#(JsString(fmt.plural_category_to_js_string(fmt.PcOther)), state))
      }
      IntlFormat, ListFormatData(l) -> {
        use #(items, state) <- result.try(string_list_from_iterable(state, arg0))
        let parts = fmt.list_format_parts(l.list_type, l.style, items)
        Ok(#(JsString(fmt.parts_to_string(parts)), state))
      }
      IntlFormatToParts, ListFormatData(l) -> {
        use #(items, state) <- result.try(string_list_from_iterable(state, arg0))
        let parts = fmt.list_format_parts(l.list_type, l.style, items)
        let #(state, arr) = parts_to_js(state, parts)
        Ok(#(arr, state))
      }
      IntlFormat, RelativeTimeFormatData(r) -> {
        use #(parts, state) <- result.try(rtf_method_parts(state, r, arg0, arg1))
        Ok(#(JsString(fmt.unit_parts_to_string(parts)), state))
      }
      IntlFormatToParts, RelativeTimeFormatData(r) -> {
        use #(parts, state) <- result.try(rtf_method_parts(state, r, arg0, arg1))
        let #(state, arr) = parts_to_js_with_unit(state, parts)
        Ok(#(arr, state))
      }
      IntlOf, DisplayNamesData(dn) -> display_names_of(state, dn, arg0)
      IntlFormat, DurationFormatData(df) -> {
        use #(parts, state) <- result.try(duration_parts(state, df, arg0))
        Ok(#(JsString(fmt.unit_parts_to_string(parts)), state))
      }
      IntlFormatToParts, DurationFormatData(df) -> {
        use #(parts, state) <- result.try(duration_parts(state, df, arg0))
        let #(state, arr) = parts_to_js_with_unit(state, parts)
        Ok(#(arr, state))
      }
      IntlSegmentsContaining, SegmentsData(sg) ->
        segments_containing(state, sg, arg0)
      IntlSegmentIteratorNext, SegmentIteratorData(it) ->
        segment_iterator_next(state, ref, it)
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
      -> throw_type(state, js_name <> " called on incompatible receiver")
    }
  })
}

/// The Number/BigInt/String/Date prototype locale-sensitive overrides
/// (ECMA-402 §17-19) — installed by `init`, no Intl brand check.
fn run_host_override(
  which: HostOverride,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let arg0 = first_arg_or_undefined(args)
  let arg1 = helpers.arg_at(args, 1)
  run(case which {
    NumberToLocaleString ->
      host_number_to_locale_string(state, this, arg0, arg1)
    BigIntToLocaleString ->
      host_bigint_to_locale_string(state, this, arg0, arg1)
    StringLocaleCompare -> {
      let arg2 = helpers.arg_at(args, 2)
      host_locale_compare(state, this, arg0, arg1, arg2)
    }
    StringToLocaleLowerCase -> host_locale_case(state, this, arg0, False)
    StringToLocaleUpperCase -> host_locale_case(state, this, arg0, True)
    DateToLocaleString ->
      host_date_to_locale(state, this, arg0, arg1, DateAndTime)
    DateToLocaleDateString ->
      host_date_to_locale(state, this, arg0, arg1, DateOnly)
    DateToLocaleTimeString ->
      host_date_to_locale(state, this, arg0, arg1, TimeOnly)
  })
}

/// Number.prototype.toLocaleString (ECMA-402 §18.2.1).
fn host_number_to_locale_string(
  state: State(host),
  this: JsValue,
  locales: JsValue,
  options: JsValue,
) -> Result(#(JsValue, State(host)), Thrown(host)) {
  use n <- result.try(case this {
    JsNumber(n) -> Ok(n)
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: value.NumberObject(value: n), ..)) -> Ok(n)
        _ ->
          throw_type(
            state,
            "Number.prototype.toLocaleString requires that 'this' be a Number",
          )
      }
    _ ->
      throw_type(
        state,
        "Number.prototype.toLocaleString requires that 'this' be a Number",
      )
  })
  use #(nf, state) <- result.try(number_format_state(state, locales, options))
  use #(parts, state) <- result.try(nf_format_parts(state, nf, JsNumber(n)))
  Ok(#(JsString(fmt.parts_to_string(parts)), state))
}

/// BigInt.prototype.toLocaleString (ECMA-402 §18.3.1) — same NumberFormat path
/// as Number.prototype.toLocaleString, but the value is handed over as its
/// exact decimal string so arbitrarily large BigInts keep every digit.
fn host_bigint_to_locale_string(
  state: State(host),
  this: JsValue,
  locales: JsValue,
  options: JsValue,
) -> Result(#(JsValue, State(host)), Thrown(host)) {
  use n <- result.try(case this {
    value.JsBigInt(value.BigInt(n)) -> Ok(n)
    JsObject(ref) ->
      case heap.read_bigint_object(state.heap, ref) {
        Some(value.BigInt(n)) -> Ok(n)
        None -> throw_bigint_receiver(state)
      }
    _ -> throw_bigint_receiver(state)
  })
  use #(nf, state) <- result.try(number_format_state(state, locales, options))
  use #(parts, state) <- result.try(nf_format_parts(
    state,
    nf,
    JsString(int.to_string(n)),
  ))
  Ok(#(JsString(fmt.parts_to_string(parts)), state))
}

fn throw_bigint_receiver(state: State(host)) -> Result(a, Thrown(host)) {
  throw_type(
    state,
    "BigInt.prototype.toLocaleString requires that 'this' be a BigInt",
  )
}

/// String.prototype.localeCompare (ECMA-402 §19.1.1).
fn host_locale_compare(
  state: State(host),
  this: JsValue,
  that_v: JsValue,
  locales: JsValue,
  options: JsValue,
) -> Result(#(JsValue, State(host)), Thrown(host)) {
  use Nil <- result.try(case this {
    JsUndefined | value.JsNull ->
      throw_type(
        state,
        "String.prototype.localeCompare called on null or undefined",
      )
    _ -> Ok(Nil)
  })
  use #(s, state) <- result.try(coerce.js_to_string(state, this))
  use #(that, state) <- result.try(coerce.js_to_string(state, that_v))
  use #(c, state) <- result.try(collator_state(state, locales, options))
  Ok(#(value.from_int(collator_compare(c, s, that)), state))
}

/// String.prototype.toLocale{Lower,Upper}Case — locale list is validated,
/// casing uses the default (root) algorithm.
fn host_locale_case(
  state: State(host),
  this: JsValue,
  locales: JsValue,
  upper: Bool,
) -> Result(#(JsValue, State(host)), Thrown(host)) {
  use Nil <- result.try(case this {
    JsUndefined | value.JsNull ->
      throw_type(state, "method called on null or undefined")
    _ -> Ok(Nil)
  })
  use #(s, state) <- result.try(coerce.js_to_string(state, this))
  use #(tag_list, state) <- result.try(canonicalize_locale_list(state, locales))
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
  // `unicode_case`). Deliberately NOT a [[Get]] + [[Call]] of
  // `String.prototype.toLowerCase`: reassigning that property must not change
  // what `toLocaleLowerCase` returns.
  let pre = case lang {
    "tr" | "az" -> turkic_case(s, upper)
    "lt" -> lithuanian_case(s, upper)
    _ -> s
  }
  let cased = case upper {
    True -> unicode_case.to_upper_case(pre)
    False -> unicode_case.to_lower_case(pre)
  }
  Ok(#(JsString(cased), state))
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

fn lower_turkic_cps(cps: List(Int), acc: List(String)) -> String {
  case cps {
    [] -> string.join(list.reverse(acc), "")
    [0x130, ..rest] -> lower_turkic_cps(rest, ["i", ..acc])
    [0x49, 0x307, ..rest] -> lower_turkic_cps(rest, ["i", ..acc])
    [0x49, ..rest] -> lower_turkic_cps(rest, ["ı", ..acc])
    [c, ..rest] -> {
      let g = case string.utf_codepoint(c) {
        Ok(cp) -> string.from_utf_codepoints([cp])
        Error(Nil) -> ""
      }
      lower_turkic_cps(rest, [g, ..acc])
    }
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
    [c, ..rest] -> {
      let g = case string.utf_codepoint(c) {
        Ok(cp) -> string.from_utf_codepoints([cp])
        Error(Nil) -> ""
      }
      upper_lt_cps(rest, [g, ..acc])
    }
  }
}

fn lower_lt_cps(cps: List(Int), acc: List(String)) -> String {
  let is_mark = fn(c) { c >= 0x300 && c <= 0x36f && c != 0x307 }
  case cps {
    [] -> string.join(list.reverse(acc), "")
    // I/J followed by a combining mark keep an explicit dot above.
    [0x49, m, ..rest] ->
      case is_mark(m) {
        True -> lower_lt_cps(rest, [mark_str(m), "i\u{0307}", ..acc])
        False -> lower_lt_cps([m, ..rest], ["i", ..acc])
      }
    [0x4a, m, ..rest] ->
      case is_mark(m) {
        True -> lower_lt_cps(rest, [mark_str(m), "j\u{0307}", ..acc])
        False -> lower_lt_cps([m, ..rest], ["j", ..acc])
      }
    [0xcc, ..rest] -> lower_lt_cps(rest, ["i\u{0307}\u{0300}", ..acc])
    [0xcd, ..rest] -> lower_lt_cps(rest, ["i\u{0307}\u{0301}", ..acc])
    [0x128, ..rest] -> lower_lt_cps(rest, ["i\u{0307}\u{0303}", ..acc])
    [c, ..rest] -> {
      let g = case string.utf_codepoint(c) {
        Ok(cp) -> string.from_utf_codepoints([cp])
        Error(Nil) -> ""
      }
      lower_lt_cps(rest, [g, ..acc])
    }
  }
}

fn mark_str(c: Int) -> String {
  case string.utf_codepoint(c) {
    Ok(cp) -> string.from_utf_codepoints([cp])
    Error(Nil) -> ""
  }
}

/// Date.prototype.toLocale{,Date,Time}String (ECMA-402 §17).
fn host_date_to_locale(
  state: State(host),
  this: JsValue,
  locales: JsValue,
  options: JsValue,
  required: DtfRequired,
) -> Result(#(JsValue, State(host)), Thrown(host)) {
  use tv <- result.try(case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: value.DateObject(time_value: tv), ..)) -> Ok(tv)
        _ -> throw_type(state, "this is not a Date object")
      }
    _ -> throw_type(state, "this is not a Date object")
  })
  let defaults = case required {
    DateOnly -> date_defaults()
    TimeOnly -> time_defaults()
    DateAndTime -> merge_components(date_defaults(), time_defaults())
  }
  use #(d, state) <- result.try(dtf_state_required(
    state,
    locales,
    options,
    defaults,
    required,
  ))
  case tv {
    value.Finite(_) -> {
      use #(parts, state) <- result.try(dtf_format_parts(state, d, JsNumber(tv)))
      Ok(#(JsString(fmt.parts_to_string(parts)), state))
    }
    _ -> Ok(#(JsString("Invalid Date"), state))
  }
}

/// PluralRules select: operands come from the formatted digit strings.
fn plural_select(p: PluralRulesState, n: value.JsNum) -> fmt.PluralCategory {
  case n {
    value.Finite(f) -> {
      let opts =
        fmt.NumOpts(
          ..num_opts_from_plural(p),
          style: StyleDecimal,
          use_grouping: GroupingNever,
          sign_display: SignNever,
        )
      let #(int_digits, frac_digits) =
        fmt.plural_operands(fmt.format_number_parts(opts, f))
      fmt.plural_select_en(p.plural_type, int_digits, frac_digits, f <. 0.0)
    }
    // NaN/Infinity have no operands.
    _ -> fmt.PcOther
  }
}

/// RelativeTimeFormat format/formatToParts core.
fn rtf_method_parts(
  state: State(host),
  r: RelativeTimeFormatState,
  value_v: JsValue,
  unit_v: JsValue,
) -> Result(#(List(fmt.UnitPart), State(host)), Thrown(host)) {
  use #(n, state) <- result.try(coerce.js_to_number(state, value_v))
  use f <- result.try(case n {
    value.Finite(f) -> Ok(f)
    _ -> throw_range(state, "Value need to be finite number")
  })
  use #(unit_str, state) <- result.try(coerce.js_to_string(state, unit_v))
  use unit <- result.try(case singular_unit(unit_str) {
    Some(u) -> Ok(u)
    None -> throw_range(state, "Invalid unit argument: " <> unit_str)
  })
  let abs_opts = fmt.NumOpts(..fmt.default_num_opts(), sign_display: SignNever)
  let value_parts = fmt.format_number_parts(abs_opts, float.absolute_value(f))
  let value_parts =
    fmt.apply_numbering_system(
      value_parts,
      r.numbering_system,
      fmt.is_number_digit,
    )
  Ok(#(fmt.rtf_parts_en(r.style, r.numeric, f, unit, value_parts), state))
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
  state: State(host),
  iterable: JsValue,
) -> Result(#(List(String), State(host)), Thrown(host)) {
  case iterable {
    JsUndefined -> Ok(#([], state))
    // Strings iterate by code points (String.prototype[Symbol.iterator]).
    JsString(str) -> {
      let items =
        string.to_utf_codepoints(str)
        |> list.map(fn(cp) { string.from_utf_codepoints([cp]) })
      Ok(#(items, state))
    }
    _ -> {
      use #(method, state) <- result.try(object.get_symbol_value_of(
        state,
        iterable,
        value.symbol_iterator,
      ))
      use Nil <- result.try(case helpers.is_callable(state.heap, method) {
        True -> Ok(Nil)
        False -> throw_type(state, "object is not iterable")
      })
      use #(iter, state) <- result.try(state.call(state, method, iterable, []))
      use iter_ref <- result.try(case iter {
        JsObject(r) -> Ok(r)
        _ -> throw_type(state, "iterator result is not an object")
      })
      use #(next_fn, state) <- result.try(object.get_value(
        state,
        iter_ref,
        Named("next"),
        iter,
      ))
      iterate_strings(state, iter, next_fn, [])
    }
  }
}

fn iterate_strings(
  state: State(host),
  iter: JsValue,
  next_fn: JsValue,
  acc: List(String),
) -> Result(#(List(String), State(host)), Thrown(host)) {
  use #(step, state) <- result.try(state.call(state, next_fn, iter, []))
  use step_ref <- result.try(case step {
    JsObject(r) -> Ok(r)
    _ -> throw_type(state, "iterator result is not an object")
  })
  use #(done, state) <- result.try(object.get_value(
    state,
    step_ref,
    Named("done"),
    step,
  ))
  case value.is_truthy(done) {
    True -> Ok(#(list.reverse(acc), state))
    False -> {
      use #(v, state) <- result.try(object.get_value(
        state,
        step_ref,
        Named("value"),
        step,
      ))
      case v {
        JsString(s) -> iterate_strings(state, iter, next_fn, [s, ..acc])
        _ -> throw_type(state, "Iterable yielded a value that is not a string")
      }
    }
  }
}

/// Intl.DisplayNames.prototype.of(code)
fn display_names_of(
  state: State(host),
  dn: DisplayNamesState,
  code_v: JsValue,
) -> Result(#(JsValue, State(host)), Thrown(host)) {
  use #(code, state) <- result.try(coerce.js_to_string(state, code_v))
  let type_ = dn.display_type
  let fallback = dn.fallback
  use #(canonical, name) <- result.try(case type_ {
    DnLanguage ->
      case tags.parse(code) {
        Ok(lid) ->
          // Must match unicode_language_id: no extensions/private use.
          case lid.extensions, lid.private_use {
            [], [] -> {
              let tag = tags.to_string(tags.canonicalize(lid))
              Ok(#(tag, fmt.language_display_name(tag)))
            }
            _, _ -> throw_range(state, "invalid language code: " <> code)
          }
        Error(Nil) -> throw_range(state, "invalid language code: " <> code)
      }
    DnRegion ->
      case is_region_subtag(code) {
        True -> {
          let r = string.uppercase(code)
          Ok(#(r, fmt.region_display_name(r)))
        }
        False -> throw_range(state, "invalid region code: " <> code)
      }
    DnScript ->
      case is_script_subtag(code) {
        True -> {
          let s = titlecase_ascii(code)
          Ok(#(s, fmt.script_display_name(s)))
        }
        False -> throw_range(state, "invalid script code: " <> code)
      }
    DnCurrency ->
      case is_alpha_str(code) && string.length(code) == 3 {
        True -> {
          let c = string.uppercase(code)
          Ok(#(c, fmt.currency_display_name(c)))
        }
        False -> throw_range(state, "invalid currency code: " <> code)
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
          Ok(#(c, name))
        }
        False -> throw_range(state, "invalid calendar code: " <> code)
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
          Ok(#(code, Some(name)))
        }
        False -> throw_range(state, "invalid dateTimeField code: " <> code)
      }
  })
  case name, fallback {
    Some(n), _ -> Ok(#(JsString(n), state))
    None, FbCode -> Ok(#(JsString(canonical), state))
    None, FbNone -> Ok(#(JsUndefined, state))
  }
}

fn titlecase_ascii(s: String) -> String {
  case string.pop_grapheme(s) {
    Ok(#(first, rest)) -> string.uppercase(first) <> string.lowercase(rest)
    Error(Nil) -> s
  }
}

// ============================================================================
// DurationFormat formatting
// ============================================================================

fn duration_parts(
  state: State(host),
  df: DurationFormatState,
  duration_v: JsValue,
) -> Result(#(List(fmt.UnitPart), State(host)), Thrown(host)) {
  use #(fields, state) <- result.try(to_duration_record(state, duration_v))
  // DurationSign consistency + IsValidDuration ranges.
  let values = duration_values(fields)
  let has_neg = list.any(values, fn(v) { v <. 0.0 })
  let has_pos = list.any(values, fn(v) { v >. 0.0 })
  use Nil <- result.try(case has_neg && has_pos {
    True -> throw_range(state, "Duration fields must have consistent sign")
    False -> Ok(Nil)
  })
  use Nil <- result.try(case is_valid_duration(fields) {
    True -> Ok(Nil)
    False -> throw_range(state, "Duration field value is out of range")
  })
  Ok(#(build_duration_parts(df, fields), state))
}

/// ToDurationRecord (object) / Temporal duration string parsing.
fn to_duration_record(
  state: State(host),
  duration_v: JsValue,
) -> Result(#(DurationRecord, State(host)), Thrown(host)) {
  case duration_v {
    JsString(str) ->
      case parse_iso_duration(str) {
        Ok(fields) -> Ok(#(fields, state))
        Error(Nil) -> throw_range(state, "Invalid duration string: " <> str)
      }
    JsObject(dur_ref) -> {
      use acc <- result.try(
        list.try_fold(
          duration_units,
          #(zero_duration, state, False),
          fn(acc, unit) {
            let #(fields, state, any) = acc
            let name = duration_unit_js_name(unit)
            use #(v, state) <- result.try(object.get_value(
              state,
              dur_ref,
              Named(name),
              duration_v,
            ))
            case v {
              JsUndefined -> Ok(#(fields, state, any))
              _ -> {
                use #(n, state) <- result.try(coerce.js_to_number(state, v))
                case n {
                  value.Finite(f) ->
                    case f == float.floor(f) {
                      True ->
                        Ok(#(set_duration_field(fields, unit, f), state, True))
                      False ->
                        throw_range(
                          state,
                          name <> " must be an integral number",
                        )
                    }
                  _ -> throw_range(state, name <> " must be a finite number")
                }
              }
            }
          },
        ),
      )
      let #(fields, state, any_defined) = acc
      use Nil <- result.try(case any_defined {
        True -> Ok(Nil)
        False -> throw_range(state, "Invalid duration object")
      })
      Ok(#(fields, state))
    }
    _ -> throw_type(state, "Duration must be an object or string")
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
          case take_designator(designators, upper) {
            Error(Nil) -> Error(Nil)
            Ok(#(field, remaining)) -> {
              let normalized = string.replace(num_acc, ",", ".")
              let has_fraction = string.contains(normalized, ".")
              case num_acc == "" || has_fraction && !allow_fraction {
                True -> Error(Nil)
                False ->
                  case parse_duration_number(normalized) {
                    Error(Nil) -> Error(Nil)
                    Ok(v) ->
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
  // The integer fallback must go through value.num_from_int: a bare
  // int.to_float on an arbitrary-precision int (e.g. a 400-digit duration
  // component) raises an uncatchable erlang:float/1 badarg. Out-of-range
  // values saturate to ±Infinity, which is not a valid duration field, so
  // treat them as a parse failure (the caller surfaces a RangeError).
  float.parse(s)
  |> result.lazy_or(fn() {
    int.parse(s)
    |> result.try(fn(n) {
      case value.num_from_int(n) {
        value.Finite(f) -> Ok(f)
        _ -> Error(Nil)
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
fn unit_display_from_duration_style(
  style: DurationUnitStyle,
) -> value.UnitDisplay {
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
  segments_proto: Ref,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let name = "Intl.Segmenter.prototype.segment"
  run({
    use #(_ref, sg) <- result.try(branded_segmenter(state, this, name))
    use #(s, state) <- result.try(coerce.js_to_string(
      state,
      first_arg_or_undefined(args),
    ))
    let seg =
      SegmentsData(SegmentsState(string: s, granularity: sg.granularity))
    let #(heap, ref) =
      common.alloc_wrapper(state.heap, IntlObject(data: seg), segments_proto)
    Ok(#(JsObject(ref), State(..state, heap:)))
  })
}

fn segments_iterator(
  iter_proto: Ref,
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let name = "%Segments.prototype%[Symbol.iterator]"
  run({
    use #(_ref, sg) <- result.try(branded_segments(state, this, name))
    let iter =
      SegmentIteratorData(SegmentIteratorState(
        string: sg.string,
        granularity: sg.granularity,
        position: 0,
      ))
    let #(heap, ref) =
      common.alloc_wrapper(state.heap, IntlObject(data: iter), iter_proto)
    Ok(#(JsObject(ref), State(..state, heap:)))
  })
}

fn make_segment_data(
  state: State(host),
  input: String,
  granularity: Granularity,
  seg: fmt.Segment,
) -> #(State(host), JsValue) {
  let base = [
    #("segment", JsString(seg.text)),
    #("index", value.from_int(seg.index)),
    #("input", JsString(input)),
  ]
  let props = case granularity {
    GWord -> list.append(base, [#("isWordLike", JsBool(seg.word_like))])
    GGrapheme | GSentence -> base
  }
  alloc_pojo(state, props)
}

fn segments_containing(
  state: State(host),
  sg: SegmentsState,
  index_v: JsValue,
) -> Result(#(JsValue, State(host)), Thrown(host)) {
  let input = sg.string
  let granularity = sg.granularity
  use #(n, state) <- result.try(coerce.js_to_number(state, index_v))
  let idx = case n {
    value.Finite(f) -> float.truncate(f)
    _ -> 0
  }
  let segments = fmt.segment_string(input, granularity)
  let total = fmt.utf16_len(input)
  case idx < 0 || idx >= total {
    True -> Ok(#(JsUndefined, state))
    False -> {
      let found =
        list.fold(segments, None, fn(acc, seg: fmt.Segment) {
          case seg.index <= idx {
            True -> Some(seg)
            False -> acc
          }
        })
      case found {
        Some(seg) -> {
          let #(state, obj) = make_segment_data(state, input, granularity, seg)
          Ok(#(obj, state))
        }
        None -> Ok(#(JsUndefined, state))
      }
    }
  }
}

fn segment_iterator_next(
  state: State(host),
  ref: Ref,
  it: SegmentIteratorState,
) -> Result(#(JsValue, State(host)), Thrown(host)) {
  let input = it.string
  let granularity = it.granularity
  let position = it.position
  let segments = fmt.segment_string(input, granularity)
  let next_seg =
    list.find(segments, fn(seg: fmt.Segment) { seg.index >= position })
  case next_seg {
    Error(Nil) -> {
      let #(state, res) = iter_result(state, JsUndefined, True)
      Ok(#(res, state))
    }
    Ok(seg) -> {
      let new_pos = seg.index + fmt.utf16_len(seg.text)
      let heap =
        write_intl_data(
          state.heap,
          ref,
          SegmentIteratorData(SegmentIteratorState(..it, position: new_pos)),
        )
      let state = State(..state, heap:)
      let #(state, data) = make_segment_data(state, input, granularity, seg)
      let #(state, res) = iter_result(state, data, False)
      Ok(#(res, state))
    }
  }
}

fn iter_result(
  state: State(host),
  v: JsValue,
  done: Bool,
) -> #(State(host), JsValue) {
  alloc_pojo(state, [#("value", v), #("done", JsBool(done))])
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
  name: LocaleGetterName,
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  run({
    use #(_ref, l) <- result.try(branded_locale(
      state,
      this,
      "Intl.Locale.prototype." <> locale_getter_js_name(name),
    ))
    let lid = locale_lid(l)
    let v = case name {
      LocaleBaseName ->
        case lid {
          Some(l) -> JsString(tags.base_name(l))
          None -> JsUndefined
        }
      LocaleLanguage ->
        case lid {
          Some(l) -> JsString(string.lowercase(l.language))
          None -> JsUndefined
        }
      LocaleScript ->
        case lid {
          Some(tags.LocaleId(script: Some(s), ..)) ->
            JsString(titlecase_ascii(s))
          _ -> JsUndefined
        }
      LocaleRegion ->
        case lid {
          Some(tags.LocaleId(region: Some(r), ..)) ->
            JsString(string.uppercase(r))
          _ -> JsUndefined
        }
      LocaleCalendar ->
        locale_u_kw(l, "ca")
        |> option.map(JsString)
        |> option.unwrap(JsUndefined)
      LocaleCollation ->
        locale_u_kw(l, "co")
        |> option.map(JsString)
        |> option.unwrap(JsUndefined)
      LocaleHourCycle ->
        locale_u_kw(l, "hc")
        |> option.map(JsString)
        |> option.unwrap(JsUndefined)
      LocaleNumberingSystem ->
        locale_u_kw(l, "nu")
        |> option.map(JsString)
        |> option.unwrap(JsUndefined)
      LocaleCaseFirst ->
        locale_u_kw(l, "kf")
        |> option.map(JsString)
        |> option.unwrap(JsUndefined)
      LocaleNumeric ->
        case locale_u_kw(l, "kn") {
          Some("") | Some("true") -> JsBool(True)
          Some(_) -> JsBool(False)
          None -> JsBool(False)
        }
      LocaleFirstDayOfWeek ->
        locale_u_kw(l, "fw")
        |> option.map(JsString)
        |> option.unwrap(JsUndefined)
      LocaleVariants ->
        case lid {
          Some(tags.LocaleId(variants: [_, ..] as vs, ..)) ->
            JsString(string.join(vs, "-"))
          _ -> JsUndefined
        }
    }
    Ok(#(v, state))
  })
}

fn locale_method(
  method: LocaleMethodName,
  proto: Ref,
  _args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  run({
    use #(_ref, l) <- result.try(branded_locale(
      state,
      this,
      "Intl.Locale.prototype." <> locale_method_js_name(method),
    ))
    let tag = l.locale
    case method {
      LocaleToString -> Ok(#(JsString(tag), state))
      LocaleMaximize | LocaleMinimize -> {
        let new_tag = case tags.parse(tag) {
          Ok(lid) ->
            case method {
              LocaleMaximize -> tags.to_string(tags.maximize(lid))
              _ -> tags.to_string(tags.minimize(lid))
            }
          Error(Nil) -> tag
        }
        let #(heap, ref) =
          common.alloc_wrapper(
            state.heap,
            IntlObject(data: LocaleData(LocaleState(locale: new_tag))),
            proto,
          )
        Ok(#(JsObject(ref), State(..state, heap:)))
      }
      LocaleGetCalendars -> {
        let vals = case locale_u_kw(l, "ca") {
          Some(ca) -> [ca]
          None -> ["gregory"]
        }
        let #(state, arr) = alloc_array(state, list.map(vals, JsString))
        Ok(#(arr, state))
      }
      LocaleGetCollations -> {
        let vals = case locale_u_kw(l, "co") {
          Some(co) -> [co]
          None -> ["emoji", "eor"]
        }
        let #(state, arr) = alloc_array(state, list.map(vals, JsString))
        Ok(#(arr, state))
      }
      LocaleGetHourCycles -> {
        let vals = case locale_u_kw(l, "hc") {
          Some(hc) -> [hc]
          None -> ["h12"]
        }
        let #(state, arr) = alloc_array(state, list.map(vals, JsString))
        Ok(#(arr, state))
      }
      LocaleGetNumberingSystems -> {
        let vals = case locale_u_kw(l, "nu") {
          Some(nu) -> [nu]
          None -> ["latn"]
        }
        let #(state, arr) = alloc_array(state, list.map(vals, JsString))
        Ok(#(arr, state))
      }
      LocaleGetTimeZones ->
        case locale_lid(l) {
          Some(tags.LocaleId(region: Some(r), ..)) -> {
            let zones = case string.uppercase(r) {
              "US" -> ["America/New_York"]
              "GB" -> ["Europe/London"]
              "DE" -> ["Europe/Berlin"]
              "FR" -> ["Europe/Paris"]
              "JP" -> ["Asia/Tokyo"]
              "CN" -> ["Asia/Shanghai"]
              _ -> []
            }
            let #(state, arr) = alloc_array(state, list.map(zones, JsString))
            Ok(#(arr, state))
          }
          _ -> Ok(#(JsUndefined, state))
        }
      LocaleGetTextInfo -> {
        let lang = case locale_lid(l) {
          Some(l) -> string.lowercase(l.language)
          None -> "en"
        }
        let dir = case
          list.contains(["ar", "he", "fa", "ur", "ps", "yi"], lang)
        {
          True -> "rtl"
          False -> "ltr"
        }
        let #(state, obj) = alloc_pojo(state, [#("direction", JsString(dir))])
        Ok(#(obj, state))
      }
      LocaleGetWeekInfo -> {
        let #(state, weekend) =
          alloc_array(state, [value.from_int(6), value.from_int(7)])
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
        let #(state, obj) =
          alloc_pojo(state, [
            #("firstDay", value.from_int(first_day)),
            #("weekend", weekend),
          ])
        Ok(#(obj, state))
      }
    }
  })
}
