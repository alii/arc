import arc/bytecode/key.{Index, Named}
import arc/internal/digits
import arc/internal/gregorian.{days_from_civil}
import arc/internal/int_math.{floor_div, pow10}
import arc/internal/temporal_calendar
import arc/rt/builtins/common
import arc/rt/builtins/helpers.{first_arg_or_undefined}
import arc/rt/builtins/intl_casing
import arc/rt/builtins/intl_collate.{collator_compare}
import arc/rt/builtins/intl_format.{
  PartDay, PartDayPeriod, PartElement, PartEra, PartFractionalSecond, PartHour,
  PartLiteral, PartMinute, PartMonth, PartSecond, PartTimeZoneName, PartWeekday,
  PartYear,
}
import arc/rt/builtins/intl_locale
import arc/rt/builtins/intl_segment
import arc/rt/builtins/intl_timezone
import arc/rt/builtins/options.{
  coerce_options_to_object, default_number_option, get_bool_opt, get_enum_opt,
  get_num_opt, get_option, get_options_object, get_text_opt,
}
import arc/rt/builtins/realm_ops
import arc/rt/call as rt_call
import arc/rt/intl_data.{
  type BoundGetterService, type CollatorState, type ConstructibleService,
  type DateStyle, type DateTimeComponent, type DateTimeComponents,
  type DateTimeFormatState, type DigitRange, type DisplayNamesState,
  type DurationBaseStyle, type DurationFormatState, type DurationUnitOptions,
  type DurationUnitStyle, type Granularity, type HourCycle, type IntlData,
  type IntlDigitOptions, type IntlService, type IntlUseGrouping,
  type ListFormatState, type ListFormatStyle, type LocaleState, type MonthWidth,
  type NameWidth, type Notation, type NumberFormatState, type NumberStyle,
  type NumericWidth, type PluralRulesState, type RelativeTimeFormatState,
  type Segment, type SegmentIteratorState, type SegmenterState,
  type SegmentsState, type TimeStyle, type TimeZoneNameWidth, type UnitDisplay,
  AccountingSign, BaseDigital, BaseLong, BaseNarrow, BaseShort, BoundCollator,
  BoundDateTimeFormat, BoundNumberFormat, CalendarNames, Cardinal,
  CaseFirstFalse, CaseFirstLower, CaseFirstUpper, CodeFallback, CollatorData,
  CollatorService, CollatorState, CompactLong, CompactShort, Conjunction,
  CurrencyCode, CurrencyName, CurrencyNames, CurrencyNarrowSymbol,
  CurrencySymbol, DateFull, DateLong, DateMedium, DateShort, DateTimeComponents,
  DateTimeFieldNames, DateTimeFormatData, DateTimeFormatService,
  DateTimeFormatState, DayComponent, DayPeriodComponent, DialectNames,
  DigitRange, Disjunction, DisplayAlways, DisplayAuto, DisplayNamesData,
  DisplayNamesService, DisplayNamesState, DurationFormatData,
  DurationFormatService, DurationFormatState, DurationUnitOptions, EraComponent,
  FractionalSecondDigitsComponent, GraphemeGranularity, GroupingAlways,
  GroupingAuto, GroupingMin2, GroupingNever, H11, H12, H23, H24, HostZone,
  HourComponent, IntlDateTimeFormat, IntlDigitOptions, IntlDisplayNames,
  IntlDurationFormat, IntlListFormat, IntlNumberFormat, IntlPluralRules,
  IntlRelativeTimeFormat, IntlSegmentIterator, IntlSegments, LanguageNames,
  ListFormatData, ListFormatService, ListFormatState, ListLong, ListNarrow,
  ListShort, LocaleData, LocaleService, LocaleState, MinuteComponent,
  MonthComponent, MonthName, MonthNum, NoFallback, NotationCompact,
  NotationEngineering, NotationScientific, NotationStandard, NumberFormatData,
  NumberFormatService, NumberFormatState, Numeric, NumericAlways, NumericAuto,
  Ordinal, PluralRulesData, PluralRulesService, PluralRulesState, PriorityAuto,
  PriorityLessPrecision, PriorityMorePrecision, RegionNames, RelativeLong,
  RelativeNarrow, RelativeShort, RelativeTimeFormatData,
  RelativeTimeFormatService, RelativeTimeFormatState, RoundCeil, RoundExpand,
  RoundFloor, RoundHalfCeil, RoundHalfEven, RoundHalfExpand, RoundHalfFloor,
  RoundHalfTrunc, RoundTrunc, ScriptNames, SecondComponent, SegmentIteratorData,
  SegmentIteratorState, SegmenterData, SegmenterService, SegmenterState,
  SegmentsData, SegmentsState, SensAccent, SensBase, SensCase, SensVariant,
  SentenceGranularity, SignAlways, SignAuto, SignExceptZero, SignNegative,
  SignNever, StandardNames, StandardSign, StyleCurrency, StyleDecimal,
  StylePercent, StyleUnit, TimeFull, TimeLong, TimeMedium, TimeShort,
  TimeZoneNameComponent, TrailingZeroAuto, TrailingZeroStripIfInteger, TwoDigit,
  UnitList, UnitLong, UnitNarrow, UnitShort, UnitStyleFractional, UnitStyleLong,
  UnitStyleNarrow, UnitStyleNumeric, UnitStyleShort, UnitStyleTwoDigit,
  UsageSearch, UsageSort, WeekdayComponent, WidthLong, WidthNarrow, WidthShort,
  WordGranularity, YearComponent, ZoneLong, ZoneLongGeneric, ZoneLongOffset,
  ZoneShort, ZoneShortGeneric, ZoneShortOffset,
}
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/temporal_data.{
  type TemporalData, TemporalDate, TemporalDateTime, TemporalDuration,
  TemporalInstant, TemporalMonthDay, TemporalTime, TemporalYearMonth,
  TemporalZonedDateTime,
}
import arc/rt/types.{
  type Agent, type Handle, type IntlHostOverrideName, type IntlMethodName,
  type IntlNative, type JsNum, type JsVal, type LocaleGetterName,
  type LocaleMethodName, BigIntObj, BigIntToLocaleString, DateObj,
  DateToLocaleDateString, DateToLocaleString, DateToLocaleTimeString,
  IntlBoundGetter, IntlBoundMethod, IntlConstructor, IntlFormat, IntlFormatRange,
  IntlFormatRangeToParts, IntlFormatToParts, IntlGetCanonicalLocales,
  IntlHostOverride, IntlLocaleGetter, IntlLocaleMethod, IntlMethod, IntlN,
  IntlObj, IntlOf, IntlResolvedOptions, IntlSegmentIteratorNext,
  IntlSegmenterSegment, IntlSegmentsContaining, IntlSegmentsIterator, IntlSelect,
  IntlSelectRange, IntlSupportedLocalesOf, IntlSupportedValuesOf, JFloat, JInt,
  JNan, JNegInf, JPosInf, KBig, KBool, KHandle, KNum, KStr, KUndef,
  LocaleBaseName, LocaleCalendar, LocaleCaseFirst, LocaleCollation,
  LocaleFirstDayOfWeek, LocaleGetCalendars, LocaleGetCollations,
  LocaleGetHourCycles, LocaleGetNumberingSystems, LocaleGetTextInfo,
  LocaleGetTimeZones, LocaleGetWeekInfo, LocaleHourCycle, LocaleLanguage,
  LocaleMaximize, LocaleMinimize, LocaleNumberingSystem, LocaleNumeric,
  LocaleRegion, LocaleScript, LocaleToString, LocaleVariants, NumberObj,
  NumberToLocaleString, SObject, StringKey, StringLocaleCompare,
  StringToLocaleLowerCase, StringToLocaleUpperCase, SymbolKey, TemporalObj,
  classify, mk_bool, mk_int, mk_number, mk_object, mk_string, mk_undefined,
}
import arc/rt/unicode_case
import arc/rt/val as rt_val
import arc/time_zone
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub fn init(
  st: Agent,
  object_proto: Handle,
  function_proto: Handle,
  number_proto: Handle,
  bigint_proto: Handle,
  string_proto: Handle,
  date_proto: Handle,
) -> #(Handle, Agent) {
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
      fn(proto) { IntlN(IntlConstructor(LocaleService, proto)) },
      "Locale",
      1,
      [],
    )
  let st = common.add_string_tag(st, locale.prototype, "Intl.Locale")
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
  let st = common.add_named_properties(st, locale.prototype, locale_methods)

  let #(collator, st) =
    init_service(st, object_proto, function_proto, CollatorService, [], [
      #("compare", IntlN(IntlBoundGetter(BoundCollator))),
    ])
  let #(number_format, st) =
    init_service(
      st,
      object_proto,
      function_proto,
      NumberFormatService,
      [
        service_method(IntlNumberFormat, IntlFormatToParts, 1),
        service_method(IntlNumberFormat, IntlFormatRange, 2),
        service_method(IntlNumberFormat, IntlFormatRangeToParts, 2),
      ],
      [#("format", IntlN(IntlBoundGetter(BoundNumberFormat)))],
    )
  let #(date_time_format, st) =
    init_service(
      st,
      object_proto,
      function_proto,
      DateTimeFormatService,
      [
        service_method(IntlDateTimeFormat, IntlFormatToParts, 1),
        service_method(IntlDateTimeFormat, IntlFormatRange, 2),
        service_method(IntlDateTimeFormat, IntlFormatRangeToParts, 2),
      ],
      [#("format", IntlN(IntlBoundGetter(BoundDateTimeFormat)))],
    )
  let #(plural_rules, st) =
    init_service(
      st,
      object_proto,
      function_proto,
      PluralRulesService,
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
      ListFormatService,
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
      RelativeTimeFormatService,
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
      DisplayNamesService,
      [service_method(IntlDisplayNames, IntlOf, 1)],
      [],
    )
  let #(duration_format, st) =
    init_service(
      st,
      object_proto,
      function_proto,
      DurationFormatService,
      [
        service_method(IntlDurationFormat, IntlFormat, 1),
        service_method(IntlDurationFormat, IntlFormatToParts, 1),
      ],
      [],
    )

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
  let st = common.add_named_properties(st, segments_proto, seg_containing)
  let #(seg_iter_fn, st) =
    common.alloc_rooted_native_fn(
      st,
      function_proto,
      IntlN(IntlSegmentsIterator(seg_iter_proto)),
      "[Symbol.iterator]",
      0,
    )
  let #(seg_iter_prop, st) =
    rt_store.builtin_property(st, mk_object(seg_iter_fn))
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
    init_service(st, object_proto, function_proto, SegmenterService, [], [])
  let st = common.add_named_properties(st, segmenter.prototype, segment_method)

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
        let #(prop, st) =
          rt_store.builtin_property(st, mk_object(bt.constructor))
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

  let #(number_methods, st) =
    common.alloc_methods(st, function_proto, [
      #("toLocaleString", IntlN(IntlHostOverride(NumberToLocaleString)), 0),
    ])
  let st = common.add_named_properties(st, number_proto, number_methods)
  let #(bigint_methods, st) =
    common.alloc_methods(st, function_proto, [
      #("toLocaleString", IntlN(IntlHostOverride(BigIntToLocaleString)), 0),
    ])
  let st = common.add_named_properties(st, bigint_proto, bigint_methods)
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
  let st = common.add_named_properties(st, string_proto, string_methods)
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
  let st = common.add_named_properties(st, date_proto, date_methods)

  #(namespace, st)
}

fn init_service(
  st: Agent,
  object_proto: Handle,
  function_proto: Handle,
  service: ConstructibleService,
  methods: List(#(String, types.NativeToken, Int)),
  accessors: List(#(String, types.NativeToken)),
) -> #(types.BuiltinPair, Agent) {
  let arity = case service {
    DisplayNamesService -> 2
    _ -> 0
  }
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
      #("supportedLocalesOf", IntlN(IntlSupportedLocalesOf), 1),
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
  let st = common.add_string_tag(st, bt.prototype, "Intl." <> name)
  #(bt, st)
}

fn service_method(
  service: IntlService,
  method: IntlMethodName,
  arity: Int,
) -> #(String, types.NativeToken, Int) {
  #(intl_method_js_name(method), IntlN(IntlMethod(service:, method:)), arity)
}

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
    IntlSupportedLocalesOf -> supported_locales_of(st, args)
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
    _ -> rt_val.throw_type_error(st, "not a constructor")
  }
}

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
      rt_val.throw_type_error(st, method <> " called on incompatible receiver")
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

fn write_intl_data(st: Agent, h: Handle, data: IntlData) -> Agent {
  rt_store.cell_update(st, h, fn(cell) {
    case cell {
      SObject(kind: IntlObj(bound:, ..), ..) ->
        SObject(..cell, kind: IntlObj(data:, bound:))
      other -> other
    }
  })
}

fn has_component(c: DateTimeComponents, which: DateTimeComponent) -> Bool {
  case which {
    WeekdayComponent -> option.is_some(c.weekday)
    EraComponent -> option.is_some(c.era)
    YearComponent -> option.is_some(c.year)
    MonthComponent -> option.is_some(c.month)
    DayComponent -> option.is_some(c.day)
    DayPeriodComponent -> option.is_some(c.day_period)
    HourComponent -> option.is_some(c.hour)
    MinuteComponent -> option.is_some(c.minute)
    SecondComponent -> option.is_some(c.second)
    FractionalSecondDigitsComponent ->
      option.is_some(c.fractional_second_digits)
    TimeZoneNameComponent -> option.is_some(c.time_zone_name)
  }
}

fn kept_width(
  keep: List(DateTimeComponent),
  which: DateTimeComponent,
  v: Option(a),
) -> Option(a) {
  case list.contains(keep, which) {
    True -> v
    False -> None
  }
}

fn keep_components(
  c: DateTimeComponents,
  keep: List(DateTimeComponent),
) -> DateTimeComponents {
  DateTimeComponents(
    weekday: kept_width(keep, WeekdayComponent, c.weekday),
    era: kept_width(keep, EraComponent, c.era),
    year: kept_width(keep, YearComponent, c.year),
    month: kept_width(keep, MonthComponent, c.month),
    day: kept_width(keep, DayComponent, c.day),
    day_period: kept_width(keep, DayPeriodComponent, c.day_period),
    hour: kept_width(keep, HourComponent, c.hour),
    minute: kept_width(keep, MinuteComponent, c.minute),
    second: kept_width(keep, SecondComponent, c.second),
    fractional_second_digits: kept_width(
      keep,
      FractionalSecondDigitsComponent,
      c.fractional_second_digits,
    ),
    time_zone_name: kept_width(keep, TimeZoneNameComponent, c.time_zone_name),
  )
}

fn merge_components(
  base: DateTimeComponents,
  fallback: DateTimeComponents,
) -> DateTimeComponents {
  DateTimeComponents(
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

fn with_digits(
  o: intl_format.NumberFormatOptions,
  dg: IntlDigitOptions,
) -> intl_format.NumberFormatOptions {
  let precision = fn(p: DigitRange) {
    intl_format.Precision(min: p.min, max: p.max)
  }
  intl_format.NumberFormatOptions(
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
  let #(h, st) = common.alloc_plain_object(st, st.realm.object.prototype, props)
  #(mk_object(h), st)
}

fn parts_to_js(st: Agent, parts: List(intl_format.Part)) -> #(JsVal, Agent) {
  let #(objs, st) =
    list.fold(parts, #([], st), fn(acc, part) {
      let #(objs, st) = acc
      let intl_format.Part(t, v) = part
      let #(obj, st) =
        alloc_pojo(st, [
          #("type", mk_string(intl_format.part_type_text(t))),
          #("value", mk_string(v)),
        ])
      #([obj, ..objs], st)
    })
  alloc_array(st, list.reverse(objs))
}

fn range_parts_to_string(parts: List(intl_format.RangePart)) -> String {
  parts |> list.map(fn(p: intl_format.RangePart) { p.value }) |> string.join("")
}

fn parts_to_js_sourced(
  st: Agent,
  parts: List(intl_format.RangePart),
) -> #(JsVal, Agent) {
  let #(objs, st) =
    list.fold(parts, #([], st), fn(acc, part: intl_format.RangePart) {
      let #(objs, st) = acc
      let #(obj, st) =
        alloc_pojo(st, [
          #("type", mk_string(intl_format.part_type_text(part.type_))),
          #("value", mk_string(part.value)),
          #("source", mk_string(intl_format.part_source_text(part.source))),
        ])
      #([obj, ..objs], st)
    })
  alloc_array(st, list.reverse(objs))
}

fn parts_to_js_with_unit(
  st: Agent,
  parts: List(intl_format.UnitPart),
) -> #(JsVal, Agent) {
  let #(objs, st) =
    list.fold(parts, #([], st), fn(acc, part: intl_format.UnitPart) {
      let #(objs, st) = acc
      let base = [
        #("type", mk_string(intl_format.part_type_text(part.type_))),
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
    KHandle(h) ->
      case locale_of_handle(st, h) {
        Some(l) -> #([l.locale], st)
        None -> locale_list_from_object(st, h)
      }
    _ -> {
      let #(h, st) = rt_val.to_object(st, locales)
      locale_list_from_object(st, h)
    }
  }
}

fn locale_of_handle(st: Agent, h: Handle) -> Option(LocaleState) {
  case rt_store.cell_get(st, h) {
    SObject(kind: IntlObj(data: LocaleData(l), ..), ..) -> Some(l)
    _ -> None
  }
}

fn canonical_tag_or_throw(st: Agent, s: String) -> #(String, Agent) {
  case intl_locale.canonicalize_tag(s) {
    Ok(tag) -> #(tag, st)
    Error(Nil) ->
      rt_val.throw_range_error(
        st,
        "Incorrect locale information provided: " <> s,
      )
  }
}

fn locale_list_from_object(st: Agent, h: Handle) -> #(List(String), Agent) {
  let o = mk_object(h)
  let #(len_v, st) = rt_obj.get_prop(st, o, StringKey(Named("length")))
  let #(len_n, st) = rt_val.to_number(st, len_v)
  let len = rt_val.jsnum_to_length(len_n)
  locale_list_from_object_loop(st, o, 0, len, [])
}

fn locale_list_from_object_loop(
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
      let #(has, st) = rt_obj.has_prop(st, o, key)
      case has {
        False -> locale_list_from_object_loop(st, o, k + 1, len, seen)
        True -> {
          let #(k_value, st) = rt_obj.get_prop(st, o, key)
          let #(tag_text, st) = case classify(k_value) {
            KStr(s) -> #(s, st)
            KHandle(o) ->
              case locale_of_handle(st, o) {
                Some(l) -> #(l.locale, st)
                None -> rt_val.to_string(st, k_value)
              }
            _ ->
              rt_val.throw_type_error(
                st,
                "Locales item must be a string or object",
              )
          }
          let #(tag, st) = canonical_tag_or_throw(st, tag_text)
          let seen = case list.contains(seen, tag) {
            True -> seen
            False -> [tag, ..seen]
          }
          locale_list_from_object_loop(st, o, k + 1, len, seen)
        }
      }
    }
  }
}

fn get_canonical_locales(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(tag_list, st) =
    canonicalize_locale_list(st, first_arg_or_undefined(args))
  alloc_array(st, list.map(tag_list, mk_string))
}

fn supported_values_of(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(key, st) = rt_val.to_string(st, first_arg_or_undefined(args))
  let values = case key {
    "calendar" -> Some(supported_calendars())
    "collation" -> Some(supported_collations())
    "currency" ->
      Some([
        "AUD", "BRL", "CAD", "CHF", "CNY", "EUR", "GBP", "INR", "JPY", "KRW",
        "MXN", "RUB", "SEK", "USD",
      ])
    "numberingSystem" -> Some(intl_format.numbering_systems())
    "timeZone" -> Some(time_zone.available_ids(st.hooks.time_zone_ids()))
    "unit" -> Some(intl_format.sanctioned_units())
    _ -> None
  }
  case values {
    Some(vs) -> alloc_array(st, list.map(vs, mk_string))
    None -> rt_val.throw_range_error(st, "Invalid key : " <> key)
  }
}

// keep sorted
fn supported_collations() -> List(String) {
  [
    "big5han", "compat", "dict", "direct", "ducet", "emoji", "eor", "gb2312",
    "phonebk", "phonetic", "pinyin", "reformed", "searchjl", "stroke", "trad",
    "unihan", "zhuyin",
  ]
}

// keep sorted
fn supported_calendars() -> List(String) {
  [
    "buddhist", "chinese", "coptic", "dangi", "ethioaa", "ethiopic", "gregory",
    "hebrew", "indian", "islamic-civil", "islamic-tbla", "islamic-umalqura",
    "iso8601", "japanese", "persian", "roc",
  ]
}

fn valid_date_time_calendar(v: String) -> Bool {
  list.contains(supported_calendars(), v)
}

fn supported_locales_of(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let locales = first_arg_or_undefined(args)
  let options_v = helpers.arg_at(args, 1)
  let #(requested, st) = canonicalize_locale_list(st, locales)
  let #(opts, st) = coerce_options_to_object(st, options_v)
  let #(_matcher, st) =
    get_text_opt(
      st,
      opts,
      "localeMatcher",
      ["lookup", "best fit"],
      Some("best fit"),
    )
  let supported =
    list.filter(requested, fn(tag) {
      intl_locale.best_available_locale(intl_locale.strip_extensions(tag))
      != None
    })
  alloc_array(st, list.map(supported, mk_string))
}

fn resolve_locale(
  requested: List(String),
) -> #(String, List(#(String, String))) {
  case requested {
    [] -> #(intl_locale.default_locale(), [])
    [tag, ..rest] ->
      case
        intl_locale.best_available_locale(intl_locale.strip_extensions(tag))
      {
        Some(available) -> #(available, u_keywords_of(tag))
        None -> resolve_locale(rest)
      }
  }
}

fn u_keywords_of(tag: String) -> List(#(String, String)) {
  intl_locale.parse(tag)
  |> result.map(lid_u_keywords)
  |> result.unwrap([])
}

fn lid_u_keywords(lid: intl_locale.LocaleId) -> List(#(String, String)) {
  lid.extensions
  |> list.filter_map(fn(ext) {
    case ext {
      intl_locale.UnicodeExtension(keywords:, ..) -> Ok(keywords)
      _ -> Error(Nil)
    }
  })
  |> list.flatten
}

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
      let kw_text =
        sorted
        |> list.map(fn(kv) {
          case kv {
            #(k, "") | #(k, "true") -> k
            #(k, v) -> k <> "-" <> v
          }
        })
        |> string.join("-")
      data_locale <> "-u-" <> kw_text
    }
  }
}

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
    CollatorService | NumberFormatService | DateTimeFormatService -> True
    _ -> False
  }
  case !callable_without_new && rt_val.is_undef(new_target) {
    True ->
      rt_val.throw_type_error(
        st,
        "Constructor Intl."
          <> service_name(intl_data.constructible_service(service))
          <> " requires 'new'",
      )
    False -> {
      // §10.1.13 prototype from newtarget so subclasses work
      let #(proto, st) =
        rt_call.get_prototype_from_constructor(st, new_target, fn(_realm) {
          proto
        })
      let arg0 = first_arg_or_undefined(args)
      let arg1 = helpers.arg_at(args, 1)
      let #(data, st) = case service {
        LocaleService -> {
          let #(s, st) = locale_state(st, arg0, arg1)
          #(LocaleData(s), st)
        }
        CollatorService -> {
          let #(s, st) = collator_state(st, arg0, arg1)
          #(CollatorData(s), st)
        }
        NumberFormatService -> {
          let #(s, st) = number_format_state(st, arg0, arg1)
          #(NumberFormatData(s), st)
        }
        DateTimeFormatService -> {
          let #(s, st) = date_time_format_state(st, arg0, arg1)
          #(DateTimeFormatData(s), st)
        }
        PluralRulesService -> {
          let #(s, st) = plural_rules_state(st, arg0, arg1)
          #(PluralRulesData(s), st)
        }
        ListFormatService -> {
          let #(s, st) = list_format_state(st, arg0, arg1)
          #(ListFormatData(s), st)
        }
        RelativeTimeFormatService -> {
          let #(s, st) = relative_time_format_state(st, arg0, arg1)
          #(RelativeTimeFormatData(s), st)
        }
        SegmenterService -> {
          let #(s, st) = segmenter_state(st, arg0, arg1)
          #(SegmenterData(s), st)
        }
        DisplayNamesService -> {
          let #(s, st) = display_names_state(st, arg0, arg1)
          #(DisplayNamesData(s), st)
        }
        DurationFormatService -> {
          let #(s, st) = duration_format_state(st, arg0, arg1)
          #(DurationFormatData(s), st)
        }
      }
      let #(h, st) =
        realm_ops.alloc_object(st, IntlObj(data:, bound: None), proto)
      #(mk_object(h), st)
    }
  }
}

fn locale_state(
  st: Agent,
  tag_v: JsVal,
  options_v: JsVal,
) -> #(LocaleState, Agent) {
  let #(tag_text, st) = case classify(tag_v) {
    KStr(s) -> #(s, st)
    KHandle(h) ->
      case locale_of_handle(st, h) {
        Some(l) -> #(l.locale, st)
        None -> rt_val.to_string(st, tag_v)
      }
    _ ->
      rt_val.throw_type_error(st, "Intl.Locale tag must be a string or object")
  }
  let #(opts, st) = coerce_options_to_object(st, options_v)
  let lid = case intl_locale.parse(tag_text) {
    Ok(lid) -> lid
    Error(Nil) ->
      rt_val.throw_range_error(
        st,
        "Incorrect locale information provided: " <> tag_text,
      )
  }
  let #(language, st) = get_text_opt(st, opts, "language", [], None)
  let st = case language {
    Some(l) ->
      case intl_locale.is_language(l) {
        True -> st
        False -> rt_val.throw_range_error(st, "Invalid language: " <> l)
      }
    None -> st
  }
  let #(script, st) = get_text_opt(st, opts, "script", [], None)
  let st = case script {
    Some(s) ->
      case intl_locale.is_script(s) {
        True -> st
        False -> rt_val.throw_range_error(st, "Invalid script: " <> s)
      }
    None -> st
  }
  let #(region, st) = get_text_opt(st, opts, "region", [], None)
  let st = case region {
    Some(r) ->
      case intl_locale.is_region(r) {
        True -> st
        False -> rt_val.throw_range_error(st, "Invalid region: " <> r)
      }
    None -> st
  }
  let #(variants_opt, st) = get_text_opt(st, opts, "variants", [], None)
  let variants_opt = case variants_opt {
    None -> None
    Some(v) -> {
      let lower = string.lowercase(v)
      let parts = string.split(lower, "-")
      let valid =
        parts != []
        && list.all(parts, intl_locale.is_variant)
        && list.length(list.unique(parts)) == list.length(parts)
      case valid {
        True -> Some(parts)
        False -> rt_val.throw_range_error(st, "Invalid variants: " <> v)
      }
    }
  }
  let lid =
    intl_locale.LocaleId(
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
  let #(calendar, st) = get_text_opt(st, opts, "calendar", [], None)
  let st = require_type_seq(st, calendar, "calendar")
  let #(collation, st) = get_text_opt(st, opts, "collation", [], None)
  let st = require_type_seq(st, collation, "collation")
  let #(hour_cycle, st) =
    get_text_opt(st, opts, "hourCycle", ["h11", "h12", "h23", "h24"], None)
  let #(case_first, st) =
    get_text_opt(st, opts, "caseFirst", ["upper", "lower", "false"], None)
  let #(numeric, st) = get_bool_opt(st, opts, "numeric", None)
  let #(first_day, st) = get_text_opt(st, opts, "firstDayOfWeek", [], None)
  let first_day = case first_day {
    None -> None
    Some(fd) ->
      case weekday_string(fd) {
        Some(v) -> Some(v)
        None -> rt_val.throw_range_error(st, "Invalid firstDayOfWeek: " <> fd)
      }
  }
  let #(numbering, st) = get_text_opt(st, opts, "numberingSystem", [], None)
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
  let canonical = intl_locale.to_string(intl_locale.canonicalize(lid))
  #(LocaleState(locale: canonical), st)
}

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
      case intl_locale.is_type_sequence(string.lowercase(other)) {
        True -> Some(string.lowercase(other))
        False -> None
      }
  }
}

fn require_type_seq(st: Agent, v: Option(String), name: String) -> Agent {
  case v {
    Some(s) ->
      case intl_locale.is_type_sequence(s) {
        True -> st
        False -> rt_val.throw_range_error(st, "Invalid " <> name <> ": " <> s)
      }
    None -> st
  }
}

fn set_u_keywords(
  lid: intl_locale.LocaleId,
  new_kws: List(#(String, String)),
) -> intl_locale.LocaleId {
  case new_kws {
    [] -> lid
    _ -> {
      let #(u_exts, others) =
        list.partition(lid.extensions, fn(e) {
          case e {
            intl_locale.UnicodeExtension(..) -> True
            _ -> False
          }
        })
      let #(attributes, existing) = case u_exts {
        [intl_locale.UnicodeExtension(attributes:, keywords:), ..] -> #(
          attributes,
          keywords,
        )
        _ -> #([], [])
      }
      let merged =
        list.fold(new_kws, existing, fn(acc, kv) {
          list.key_set(acc, kv.0, kv.1)
        })
      intl_locale.LocaleId(..lid, extensions: [
        intl_locale.UnicodeExtension(attributes:, keywords: merged),
        ..others
      ])
    }
  }
}

fn resolve_typed_keyword(
  ext_kws: List(#(String, String)),
  key: String,
  option_value: Option(a),
  parse: fn(String) -> Option(a),
  default: a,
) -> #(a, Bool) {
  // bare keyword ("-u-kn") means "true"
  let from_ext = case list.key_find(ext_kws, key) {
    Ok("") -> parse("true")
    Ok(v) -> parse(v)
    Error(Nil) -> None
  }
  case option_value {
    Some(v) -> #(v, from_ext == Some(v))
    None ->
      case from_ext {
        Some(v) -> #(v, True)
        None -> #(default, False)
      }
  }
}

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
    option.then(option_value, parse),
    parse,
    default,
  )
}

// value unused but the read is observable
fn read_locale_matcher(st: Agent, opts: Option(Handle)) -> Agent {
  let #(_matcher, st) =
    get_text_opt(
      st,
      opts,
      "localeMatcher",
      ["lookup", "best fit"],
      Some("best fit"),
    )
  st
}

fn constructor_prologue(
  st: Agent,
  locales_v: JsVal,
  options_v: JsVal,
  strict strict: Bool,
) -> #(List(String), Option(Handle), Agent) {
  let #(requested, st) = canonicalize_locale_list(st, locales_v)
  let #(opts, st) = case strict {
    True -> #(get_options_object(st, options_v), st)
    False -> coerce_options_to_object(st, options_v)
  }
  let st = read_locale_matcher(st, opts)
  #(requested, opts, st)
}

fn resolve_nu_locale(
  st: Agent,
  opts: Option(Handle),
  requested: List(String),
) -> #(String, String, Agent) {
  let #(nu_opt, st) = get_text_opt(st, opts, "numberingSystem", [], None)
  let st = require_type_seq(st, nu_opt, "numberingSystem")
  let #(data_locale, ext_kws) = resolve_locale(requested)
  let #(nu, nu_from_ext) =
    resolve_keyword(
      ext_kws,
      "nu",
      nu_opt,
      intl_format.is_numbering_system,
      "latn",
    )
  let locale = build_resolved_locale(data_locale, [#("nu", nu_from_ext, nu)])
  #(nu, locale, st)
}

fn collator_state(
  st: Agent,
  locales_v: JsVal,
  options_v: JsVal,
) -> #(CollatorState, Agent) {
  let #(requested, st) = canonicalize_locale_list(st, locales_v)
  let #(opts, st) = coerce_options_to_object(st, options_v)
  let #(usage, st) =
    get_enum_opt(
      st,
      opts,
      "usage",
      [#("sort", UsageSort), #("search", UsageSearch)],
      UsageSort,
    )
  let st = read_locale_matcher(st, opts)
  let #(collation_opt, st) = get_text_opt(st, opts, "collation", [], None)
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
  let #(numeric_text, kn_from_ext) =
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
  let numeric = numeric_text == "true"
  let #(case_first, kf_from_ext) =
    resolve_typed_keyword(
      ext_kws,
      "kf",
      case_first_opt,
      intl_data.case_first_from_text,
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
      #("kf", kf_from_ext, intl_data.case_first_text(case_first)),
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

fn number_format_state(
  st: Agent,
  locales_v: JsVal,
  options_v: JsVal,
) -> #(NumberFormatState, Agent) {
  let #(requested, opts, st) =
    constructor_prologue(st, locales_v, options_v, strict: False)
  let #(nu, locale, st) = resolve_nu_locale(st, opts, requested)
  let #(style, st) = read_unit_options(st, opts)
  let #(mnfd_default, mxfd_default) = case style {
    StyleCurrency(currency:, ..) -> {
      let d = intl_format.currency_digits(currency)
      #(d, d)
    }
    StylePercent -> #(0, 0)
    StyleDecimal | StyleUnit(..) -> #(0, 3)
  }
  let #(notation_kind, st) =
    get_enum_opt(st, opts, "notation", notation_variants(), RequestedStandard)
  let #(digits, st) =
    digit_options(st, opts, mnfd_default, mxfd_default, notation_kind)
  let #(notation, st) = read_notation(st, opts, notation_kind)
  let #(grouping_v, st) = get_option(st, opts, "useGrouping")
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
      let #(s, st) = rt_val.to_string(st, grouping_v)
      case s {
        "min2" -> #(GroupingMin2, st)
        "auto" -> #(GroupingAuto, st)
        "always" -> #(GroupingAlways, st)
        _ ->
          rt_val.throw_range_error(
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

type RequestedStyle {
  RequestedDecimal
  RequestedPercent
  RequestedCurrency
  RequestedUnit
}

type StyleWithCurrency {
  StyledDecimal
  StyledPercent
  StyledCurrency(currency: String)
  StyledUnit
}

fn read_unit_options(st: Agent, opts: Option(Handle)) -> #(NumberStyle, Agent) {
  let #(kind, st) =
    get_enum_opt(
      st,
      opts,
      "style",
      [
        #("decimal", RequestedDecimal),
        #("percent", RequestedPercent),
        #("currency", RequestedCurrency),
        #("unit", RequestedUnit),
      ],
      RequestedDecimal,
    )
  let #(currency, st) = get_text_opt(st, opts, "currency", [], None)
  let st = case currency {
    Some(c) ->
      case intl_locale.is_alpha(c) && string.length(c) == 3 {
        True -> st
        False -> rt_val.throw_range_error(st, "Invalid currency code: " <> c)
      }
    None -> st
  }
  let sc = case kind, currency {
    RequestedCurrency, Some(c) -> StyledCurrency(currency: string.uppercase(c))
    RequestedCurrency, None ->
      rt_val.throw_type_error(
        st,
        "Currency code is required with currency style",
      )
    RequestedDecimal, _ -> StyledDecimal
    RequestedPercent, _ -> StyledPercent
    RequestedUnit, _ -> StyledUnit
  }
  let #(currency_display, st) =
    get_enum_opt(
      st,
      opts,
      "currencyDisplay",
      [
        #("code", CurrencyCode),
        #("symbol", CurrencySymbol),
        #("narrowSymbol", CurrencyNarrowSymbol),
        #("name", CurrencyName),
      ],
      CurrencySymbol,
    )
  let #(currency_sign, st) =
    get_enum_opt(
      st,
      opts,
      "currencySign",
      [#("standard", StandardSign), #("accounting", AccountingSign)],
      StandardSign,
    )
  let #(unit, st) = get_text_opt(st, opts, "unit", [], None)
  let st = case unit {
    Some(u) ->
      case intl_format.is_well_formed_unit(u) {
        True -> st
        False ->
          rt_val.throw_range_error(
            st,
            "Invalid unit argument for option unit: " <> u,
          )
      }
    None -> st
  }
  let build = case sc, unit {
    StyledDecimal, _ -> fn(_ud) { StyleDecimal }
    StyledPercent, _ -> fn(_ud) { StylePercent }
    StyledCurrency(currency:), _ -> fn(_ud) {
      StyleCurrency(currency:, display: currency_display, sign: currency_sign)
    }
    StyledUnit, Some(u) -> fn(ud) { StyleUnit(unit: u, display: ud) }
    StyledUnit, None ->
      rt_val.throw_type_error(st, "Unit is required with unit style")
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

type RequestedNotation {
  RequestedStandard
  RequestedScientific
  RequestedEngineering
  RequestedCompact
}

fn notation_variants() -> List(#(String, RequestedNotation)) {
  [
    #("standard", RequestedStandard),
    #("scientific", RequestedScientific),
    #("engineering", RequestedEngineering),
    #("compact", RequestedCompact),
  ]
}

fn read_notation(
  st: Agent,
  opts: Option(Handle),
  kind: RequestedNotation,
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
      RequestedStandard -> NotationStandard
      RequestedScientific -> NotationScientific
      RequestedEngineering -> NotationEngineering
      RequestedCompact -> NotationCompact(display: compact_display)
    },
    st,
  )
}

fn digit_options(
  st: Agent,
  opts: Option(Handle),
  mnfd_default: Int,
  mxfd_default: Int,
  notation: RequestedNotation,
) -> #(IntlDigitOptions, Agent) {
  let #(mnid, st) =
    get_num_opt(st, opts, "minimumIntegerDigits", 1, 21, Some(1))
  let #(mnfd_v, st) = get_option(st, opts, "minimumFractionDigits")
  let #(mxfd_v, st) = get_option(st, opts, "maximumFractionDigits")
  let #(mnsd_v, st) = get_option(st, opts, "minimumSignificantDigits")
  let #(mxsd_v, st) = get_option(st, opts, "maximumSignificantDigits")
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
      rt_val.throw_range_error(
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
      [
        #("auto", TrailingZeroAuto),
        #("stripIfInteger", TrailingZeroStripIfInteger),
      ],
      TrailingZeroAuto,
    )
  let is_undef = rt_val.is_undef
  let has_sd = !is_undef(mnsd_v) || !is_undef(mxsd_v)
  let has_fd = !is_undef(mnfd_v) || !is_undef(mxfd_v)
  let need_sd = case rounding_priority {
    PriorityAuto -> has_sd
    PriorityMorePrecision | PriorityLessPrecision -> True
  }
  let need_fd = case rounding_priority {
    PriorityAuto -> !{ has_sd || { !has_fd && notation == RequestedCompact } }
    PriorityMorePrecision | PriorityLessPrecision -> True
  }
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
          #(Some(DigitRange(mnsd, mxsd)), st)
        }
        False -> #(Some(DigitRange(1, 21)), st)
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
                  rt_val.throw_range_error(
                    st,
                    "minimumFractionDigits is greater than maximumFractionDigits",
                  )
                False -> #(mn, mx)
              }
            Some(mn), None -> #(mn, int.max(mxfd_default, mn))
            None, Some(mx) -> #(int.min(mnfd_default, mx), mx)
            None, None -> #(mnfd_default, mxfd_default)
          }
          #(Some(DigitRange(mnfd, mxfd)), st)
        }
        False -> #(
          Some(DigitRange(mnfd_default, int.max(mxfd_default, mnfd_default))),
          st,
        )
      }
  }
  let #(sig, fd, rounding_priority) = case sig, fd {
    None, None -> #(
      Some(DigitRange(1, 2)),
      Some(DigitRange(0, 0)),
      PriorityMorePrecision,
    )
    _, _ -> #(sig, fd, rounding_priority)
  }
  let st = case rounding_increment != 1 {
    False -> st
    True ->
      case need_sd || !need_fd {
        True ->
          rt_val.throw_type_error(
            st,
            "roundingIncrement requires fractionDigits rounding type",
          )
        False ->
          case fd {
            None -> st
            Some(DigitRange(mn, mx)) if mn == mx -> st
            Some(_) ->
              rt_val.throw_range_error(
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

fn date_time_format_state(
  st: Agent,
  locales_v: JsVal,
  options_v: JsVal,
) -> #(DateTimeFormatState, Agent) {
  date_time_format_state_required(
    st,
    locales_v,
    options_v,
    date_defaults(),
    DateAndTime,
  )
}

type RequiredComponents {
  DateOnly
  TimeOnly
  DateAndTime
}

fn date_defaults() -> DateTimeComponents {
  DateTimeComponents(
    ..intl_data.empty_date_time_components,
    year: Some(Numeric),
    month: Some(MonthNum(Numeric)),
    day: Some(Numeric),
  )
}

fn time_defaults() -> DateTimeComponents {
  DateTimeComponents(
    ..intl_data.empty_date_time_components,
    hour: Some(Numeric),
    minute: Some(Numeric),
    second: Some(Numeric),
  )
}

fn public_component(
  user: Option(a),
  default: Option(a),
  styled styled: Bool,
  required_group_present required_group_present: Bool,
) -> Option(a) {
  case user, styled || required_group_present {
    Some(_), _ -> user
    None, False -> default
    None, True -> None
  }
}

fn name_width_variants() -> List(#(String, NameWidth)) {
  [#("narrow", WidthNarrow), #("short", WidthShort), #("long", WidthLong)]
}

fn numeric_width_variants() -> List(#(String, NumericWidth)) {
  [#("2-digit", TwoDigit), #("numeric", Numeric)]
}

fn date_time_format_state_required(
  st: Agent,
  locales_v: JsVal,
  options_v: JsVal,
  defaults: DateTimeComponents,
  required: RequiredComponents,
) -> #(DateTimeFormatState, Agent) {
  let #(requested, opts, st) =
    constructor_prologue(st, locales_v, options_v, strict: False)
  let #(calendar_opt, st) = get_text_opt(st, opts, "calendar", [], None)
  let st = require_type_seq(st, calendar_opt, "calendar")
  let #(nu_opt, st) = get_text_opt(st, opts, "numberingSystem", [], None)
  let st = require_type_seq(st, nu_opt, "numberingSystem")
  let #(hour12, st) = get_bool_opt(st, opts, "hour12", None)
  let #(hour_cycle_opt, st) =
    get_enum_opt(st, opts, "hourCycle", hour_cycle_variants(), None)
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
        intl_locale.canonical_keyword_value("ca", string.lowercase(v))
      }),
      valid_date_time_calendar,
      "gregory",
    )
  let #(nu, nu_from_ext) =
    resolve_keyword(
      ext_kws,
      "nu",
      nu_opt,
      intl_format.is_numbering_system,
      "latn",
    )
  let lang = intl_locale.language_of(data_locale)
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
  let hc = case hour12 {
    Some(True) -> hc_locale_default
    Some(False) -> H23
    None -> hc
  }
  let locale =
    build_resolved_locale(data_locale, [
      #("ca", ca_from_ext, calendar),
      #("nu", nu_from_ext, nu),
      #("hc", hc_from_ext && hour12 == None, intl_data.hour_cycle_text(hc)),
    ])
  let #(tz_v, st) = get_option(st, opts, "timeZone")
  let #(time_zone, st) = case classify(tz_v) {
    KUndef -> #(HostZone(st.hooks.time_zone), st)
    _ -> {
      let #(s, st) = rt_val.to_string(st, tz_v)
      case intl_timezone.lookup(st, s) {
        #(Some(tz), st) -> #(tz, st)
        #(None, st) ->
          rt_val.throw_range_error(st, "Invalid time zone specified: " <> s)
      }
    }
  }
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
    get_text_opt(
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
        #("full", DateFull),
        #("long", DateLong),
        #("medium", DateMedium),
        #("short", DateShort),
      ]),
      None,
    )
  let #(time_style, st) =
    get_enum_opt(
      st,
      opts,
      "timeStyle",
      optional_variants([
        #("full", TimeFull),
        #("long", TimeLong),
        #("medium", TimeMedium),
        #("short", TimeShort),
      ]),
      None,
    )
  let user =
    DateTimeComponents(
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
  let explicit =
    list.any(date_time_component_order, fn(c) { has_component(user, c) })
  // era and timeZoneName do not clear need_defaults
  let date_group = [
    WeekdayComponent,
    YearComponent,
    MonthComponent,
    DayComponent,
  ]
  let time_group = [
    DayPeriodComponent, HourComponent, MinuteComponent, SecondComponent,
    FractionalSecondDigitsComponent,
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
          rt_val.throw_type_error(
            st,
            "timeStyle cannot be used with toLocaleDateString",
          )
        None -> st
      }
    TimeOnly ->
      case date_style {
        Some(_) ->
          rt_val.throw_type_error(
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
      rt_val.throw_type_error(
        st,
        "Invalid option: dateStyle/timeStyle cannot be used with other date/time options",
      )
    False -> st
  }
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
  let explicit_names =
    list.filter(
      [
        WeekdayComponent, YearComponent, MonthComponent, DayComponent,
        DayPeriodComponent, HourComponent, MinuteComponent, SecondComponent,
        FractionalSecondDigitsComponent,
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

fn hour_cycle_variants() -> List(#(String, Option(HourCycle))) {
  optional_variants([#("h11", H11), #("h12", H12), #("h23", H23), #("h24", H24)])
}

fn parse_hour_cycle(s: String) -> Option(HourCycle) {
  list.key_find(hour_cycle_variants(), s)
  |> option.from_result
  |> option.flatten
}

fn month_width_variants() -> List(#(String, MonthWidth)) {
  [
    #("2-digit", MonthNum(TwoDigit)),
    #("numeric", MonthNum(Numeric)),
    #("narrow", MonthName(WidthNarrow)),
    #("short", MonthName(WidthShort)),
    #("long", MonthName(WidthLong)),
  ]
}

fn tz_name_width_variants() -> List(#(String, TimeZoneNameWidth)) {
  [
    #("short", ZoneShort),
    #("long", ZoneLong),
    #("shortOffset", ZoneShortOffset),
    #("longOffset", ZoneLongOffset),
    #("shortGeneric", ZoneShortGeneric),
    #("longGeneric", ZoneLongGeneric),
  ]
}

const date_time_component_order = [
  WeekdayComponent,
  EraComponent,
  YearComponent,
  MonthComponent,
  DayComponent,
  DayPeriodComponent,
  HourComponent,
  MinuteComponent,
  SecondComponent,
  FractionalSecondDigitsComponent,
  TimeZoneNameComponent,
]

fn date_style_components(style: Option(DateStyle)) -> DateTimeComponents {
  let base = intl_data.empty_date_time_components
  case style {
    Some(DateFull) ->
      DateTimeComponents(
        ..base,
        weekday: Some(WidthLong),
        year: Some(Numeric),
        month: Some(MonthName(WidthLong)),
        day: Some(Numeric),
      )
    Some(DateLong) ->
      DateTimeComponents(
        ..base,
        year: Some(Numeric),
        month: Some(MonthName(WidthLong)),
        day: Some(Numeric),
      )
    Some(DateMedium) ->
      DateTimeComponents(
        ..base,
        year: Some(Numeric),
        month: Some(MonthName(WidthShort)),
        day: Some(Numeric),
      )
    Some(DateShort) ->
      DateTimeComponents(
        ..base,
        year: Some(TwoDigit),
        month: Some(MonthNum(Numeric)),
        day: Some(Numeric),
      )
    None -> base
  }
}

fn time_style_components(style: Option(TimeStyle)) -> DateTimeComponents {
  let base = intl_data.empty_date_time_components
  case style {
    Some(TimeFull) ->
      DateTimeComponents(
        ..base,
        hour: Some(Numeric),
        minute: Some(TwoDigit),
        second: Some(TwoDigit),
        time_zone_name: Some(ZoneLong),
      )
    Some(TimeLong) ->
      DateTimeComponents(
        ..base,
        hour: Some(Numeric),
        minute: Some(TwoDigit),
        second: Some(TwoDigit),
        time_zone_name: Some(ZoneShort),
      )
    Some(TimeMedium) ->
      DateTimeComponents(
        ..base,
        hour: Some(Numeric),
        minute: Some(TwoDigit),
        second: Some(TwoDigit),
      )
    Some(TimeShort) ->
      DateTimeComponents(..base, hour: Some(Numeric), minute: Some(TwoDigit))
    None -> base
  }
}

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
    get_enum_opt(st, opts, "notation", notation_variants(), RequestedStandard)
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
    get_enum_opt(st, opts, "style", list_format_style_variants(), ListLong)
  let #(data_locale, _ext) = resolve_locale(requested)
  #(ListFormatState(locale: data_locale, list_type: type_, style:), st)
}

fn list_format_style_variants() -> List(#(String, ListFormatStyle)) {
  [#("long", ListLong), #("short", ListShort), #("narrow", ListNarrow)]
}

fn relative_time_format_state(
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
      [
        #("long", RelativeLong),
        #("short", RelativeShort),
        #("narrow", RelativeNarrow),
      ],
      RelativeLong,
    )
  let #(numeric, st) =
    get_enum_opt(
      st,
      opts,
      "numeric",
      [#("always", NumericAlways), #("auto", NumericAuto)],
      NumericAlways,
    )
  #(
    RelativeTimeFormatState(locale:, style:, numeric:, numbering_system: nu),
    st,
  )
}

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
      [
        #("grapheme", GraphemeGranularity),
        #("word", WordGranularity),
        #("sentence", SentenceGranularity),
      ],
      GraphemeGranularity,
    )
  let #(data_locale, _ext) = resolve_locale(requested)
  #(SegmenterState(locale: data_locale, granularity:), st)
}

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
      [#("narrow", WidthNarrow), #("short", WidthShort), #("long", WidthLong)],
      WidthLong,
    )
  let #(type_, st) =
    get_enum_opt(
      st,
      opts,
      "type",
      [
        #("language", Some(LanguageNames)),
        #("region", Some(RegionNames)),
        #("script", Some(ScriptNames)),
        #("currency", Some(CurrencyNames)),
        #("calendar", Some(CalendarNames)),
        #("dateTimeField", Some(DateTimeFieldNames)),
      ],
      None,
    )
  let type_ = case type_ {
    Some(t) -> t
    None ->
      rt_val.throw_type_error(
        st,
        "Intl.DisplayNames constructor requires type option",
      )
  }
  let #(fallback, st) =
    get_enum_opt(
      st,
      opts,
      "fallback",
      [#("code", CodeFallback), #("none", NoFallback)],
      CodeFallback,
    )
  let #(language_display, st) =
    get_enum_opt(
      st,
      opts,
      "languageDisplay",
      [#("dialect", DialectNames), #("standard", StandardNames)],
      DialectNames,
    )
  let #(data_locale, _ext) = resolve_locale(requested)
  #(
    DisplayNamesState(
      locale: data_locale,
      style:,
      display_type: type_,
      fallback:,
      language_display: case type_ {
        LanguageNames -> Some(language_display)
        RegionNames
        | ScriptNames
        | CurrencyNames
        | CalendarNames
        | DateTimeFieldNames -> None
      },
    ),
    st,
  )
}

type DurationUnit {
  YearsUnit
  MonthsUnit
  WeeksUnit
  DaysUnit
  HoursUnit
  MinutesUnit
  SecondsUnit
  MillisecondsUnit
  MicrosecondsUnit
  NanosecondsUnit
}

const duration_units = [
  YearsUnit,
  MonthsUnit,
  WeeksUnit,
  DaysUnit,
  HoursUnit,
  MinutesUnit,
  SecondsUnit,
  MillisecondsUnit,
  MicrosecondsUnit,
  NanosecondsUnit,
]

fn duration_unit_js_name(u: DurationUnit) -> String {
  case u {
    YearsUnit -> "years"
    MonthsUnit -> "months"
    WeeksUnit -> "weeks"
    DaysUnit -> "days"
    HoursUnit -> "hours"
    MinutesUnit -> "minutes"
    SecondsUnit -> "seconds"
    MillisecondsUnit -> "milliseconds"
    MicrosecondsUnit -> "microseconds"
    NanosecondsUnit -> "nanoseconds"
  }
}

fn duration_unit_singular(u: DurationUnit) -> String {
  case u {
    YearsUnit -> "year"
    MonthsUnit -> "month"
    WeeksUnit -> "week"
    DaysUnit -> "day"
    HoursUnit -> "hour"
    MinutesUnit -> "minute"
    SecondsUnit -> "second"
    MillisecondsUnit -> "millisecond"
    MicrosecondsUnit -> "microsecond"
    NanosecondsUnit -> "nanosecond"
  }
}

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
    YearsUnit -> d.years
    MonthsUnit -> d.months
    WeeksUnit -> d.weeks
    DaysUnit -> d.days
    HoursUnit -> d.hours
    MinutesUnit -> d.minutes
    SecondsUnit -> d.seconds
    MillisecondsUnit -> d.milliseconds
    MicrosecondsUnit -> d.microseconds
    NanosecondsUnit -> d.nanoseconds
  }
}

fn set_duration_field(
  d: DurationRecord,
  u: DurationUnit,
  v: Float,
) -> DurationRecord {
  case u {
    YearsUnit -> DurationRecord(..d, years: v)
    MonthsUnit -> DurationRecord(..d, months: v)
    WeeksUnit -> DurationRecord(..d, weeks: v)
    DaysUnit -> DurationRecord(..d, days: v)
    HoursUnit -> DurationRecord(..d, hours: v)
    MinutesUnit -> DurationRecord(..d, minutes: v)
    SecondsUnit -> DurationRecord(..d, seconds: v)
    MillisecondsUnit -> DurationRecord(..d, milliseconds: v)
    MicrosecondsUnit -> DurationRecord(..d, microseconds: v)
    NanosecondsUnit -> DurationRecord(..d, nanoseconds: v)
  }
}

fn duration_values(d: DurationRecord) -> List(Float) {
  list.map(duration_units, duration_field(d, _))
}

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
        #("long", BaseLong),
        #("short", BaseShort),
        #("narrow", BaseNarrow),
        #("digital", BaseDigital),
      ],
      BaseShort,
    )
  let unit = fn(st, name, prev) {
    duration_unit_options(st, opts, base_style, name, prev)
  }
  let #(years, prev, st) = unit(st, YearsUnit, None)
  let #(months, prev, st) = unit(st, MonthsUnit, Some(prev))
  let #(weeks, prev, st) = unit(st, WeeksUnit, Some(prev))
  let #(days, prev, st) = unit(st, DaysUnit, Some(prev))
  let #(hours, prev, st) = unit(st, HoursUnit, Some(prev))
  let #(minutes, prev, st) = unit(st, MinutesUnit, Some(prev))
  let #(seconds, prev, st) = unit(st, SecondsUnit, Some(prev))
  let #(milliseconds, prev, st) = unit(st, MillisecondsUnit, Some(prev))
  let #(microseconds, prev, st) = unit(st, MicrosecondsUnit, Some(prev))
  let #(nanoseconds, _prev, st) = unit(st, NanosecondsUnit, Some(prev))
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

fn duration_style_variants(
  u: DurationUnit,
) -> List(#(String, DurationUnitStyle)) {
  let base = [
    #("long", UnitStyleLong),
    #("short", UnitStyleShort),
    #("narrow", UnitStyleNarrow),
  ]
  case u {
    HoursUnit | MinutesUnit | SecondsUnit ->
      list.append(base, [
        #("numeric", UnitStyleNumeric),
        #("2-digit", UnitStyleTwoDigit),
      ])
    MillisecondsUnit | MicrosecondsUnit | NanosecondsUnit ->
      list.append(base, [#("numeric", UnitStyleNumeric)])
    YearsUnit | MonthsUnit | WeeksUnit | DaysUnit -> base
  }
}

fn is_numeric_style(s: DurationUnitStyle) -> Bool {
  case s {
    UnitStyleNumeric | UnitStyleTwoDigit | UnitStyleFractional -> True
    UnitStyleLong | UnitStyleShort | UnitStyleNarrow -> False
  }
}

fn is_sub_second(u: DurationUnit) -> Bool {
  case u {
    MillisecondsUnit | MicrosecondsUnit | NanosecondsUnit -> True
    YearsUnit
    | MonthsUnit
    | WeeksUnit
    | DaysUnit
    | HoursUnit
    | MinutesUnit
    | SecondsUnit -> False
  }
}

fn optional_variants(
  variants: List(#(String, a)),
) -> List(#(String, Option(a))) {
  list.map(variants, fn(kv) { #(kv.0, Some(kv.1)) })
}

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
    MinutesUnit | SecondsUnit -> True
    _ -> False
  }
  let #(style, display_default) = case style_opt {
    Some(chosen) -> #(chosen, DisplayAlways)
    None ->
      case base_style {
        BaseDigital ->
          case unit {
            YearsUnit | MonthsUnit | WeeksUnit | DaysUnit -> #(
              UnitStyleShort,
              DisplayAuto,
            )
            _ -> #(UnitStyleNumeric, DisplayAlways)
          }
        BaseLong | BaseShort | BaseNarrow ->
          case prev_numeric {
            True ->
              case two_digit_unit {
                True -> #(UnitStyleNumeric, DisplayAlways)
                False -> #(UnitStyleNumeric, DisplayAuto)
              }
            False -> #(duration_base_unit_style(base_style), DisplayAuto)
          }
      }
  }
  let #(style, display_default) = case style == UnitStyleNumeric && sub_second {
    True -> #(UnitStyleFractional, DisplayAuto)
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
  let st = case display == DisplayAlways && style == UnitStyleFractional {
    True ->
      rt_val.throw_range_error(
        st,
        name <> "Display cannot be 'always' for fractional units",
      )
    False -> st
  }
  let style = case prev_style {
    Some(UnitStyleFractional) ->
      case style {
        UnitStyleFractional -> style
        _ ->
          rt_val.throw_range_error(
            st,
            name <> " style must be fractional after a fractional unit",
          )
      }
    Some(UnitStyleNumeric) | Some(UnitStyleTwoDigit) ->
      case style {
        UnitStyleFractional | UnitStyleNumeric | UnitStyleTwoDigit ->
          case two_digit_unit {
            True -> UnitStyleTwoDigit
            False -> style
          }
        _ ->
          rt_val.throw_range_error(
            st,
            name <> " style cannot be mixed with numeric styles",
          )
      }
    Some(UnitStyleLong) | Some(UnitStyleShort) | Some(UnitStyleNarrow) | None ->
      style
  }
  #(DurationUnitOptions(style:, display:), style, st)
}

fn duration_base_unit_style(base: DurationBaseStyle) -> DurationUnitStyle {
  case base {
    BaseLong -> UnitStyleLong
    BaseShort | BaseDigital -> UnitStyleShort
    BaseNarrow -> UnitStyleNarrow
  }
}

fn duration_list_style(base: DurationBaseStyle) -> intl_data.ListFormatStyle {
  case base {
    BaseDigital | BaseShort -> ListShort
    BaseLong -> ListLong
    BaseNarrow -> ListNarrow
  }
}

fn duration_unit_list(
  d: DurationFormatState,
) -> List(#(DurationUnit, DurationUnitOptions)) {
  [
    #(YearsUnit, d.years),
    #(MonthsUnit, d.months),
    #(WeeksUnit, d.weeks),
    #(DaysUnit, d.days),
    #(HoursUnit, d.hours),
    #(MinutesUnit, d.minutes),
    #(SecondsUnit, d.seconds),
    #(MillisecondsUnit, d.milliseconds),
    #(MicrosecondsUnit, d.microseconds),
    #(NanosecondsUnit, d.nanoseconds),
  ]
}

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
  let #(props, st) = case data {
    LocaleData(l) -> #([#("locale", mk_string(l.locale))], st)
    CollatorData(c) -> #(
      [
        #("locale", mk_string(c.locale)),
        #("usage", mk_string(intl_data.collator_usage_text(c.usage))),
        #(
          "sensitivity",
          mk_string(intl_data.collator_sensitivity_text(c.sensitivity)),
        ),
        #("ignorePunctuation", mk_bool(c.ignore_punctuation)),
        #("collation", mk_string(c.collation)),
        #("numeric", mk_bool(c.numeric)),
        #("caseFirst", mk_string(intl_data.case_first_text(c.case_first))),
      ],
      st,
    )
    NumberFormatData(nf) -> {
      let dg = nf.digits
      let #(currency, currency_display, currency_sign) = case nf.style {
        StyleCurrency(currency:, display:, sign:) -> #(
          Some(mk_string(currency)),
          Some(mk_string(intl_data.currency_display_text(display))),
          Some(mk_string(intl_data.currency_sign_text(sign))),
        )
        StyleDecimal | StylePercent | StyleUnit(..) -> #(None, None, None)
      }
      let #(unit, unit_display) = case nf.style {
        StyleUnit(unit:, display:) -> #(
          Some(mk_string(unit)),
          Some(mk_string(intl_data.unit_display_text(display))),
        )
        StyleDecimal | StylePercent | StyleCurrency(..) -> #(None, None)
      }
      #(
        present_pairs([
          #("locale", Some(mk_string(nf.locale))),
          #("numberingSystem", Some(mk_string(nf.numbering_system))),
          #("style", Some(mk_string(intl_data.num_style_text(nf.style)))),
          #("currency", currency),
          #("currencyDisplay", currency_display),
          #("currencySign", currency_sign),
          #("unit", unit),
          #("unitDisplay", unit_display),
          ..digit_option_pairs(dg, [
            #("useGrouping", Some(use_grouping_js(nf.use_grouping))),
            #("notation", Some(mk_string(intl_data.notation_text(nf.notation)))),
            #("compactDisplay", compact_display_of(nf.notation)),
            #(
              "signDisplay",
              Some(mk_string(intl_data.sign_display_text(nf.sign_display))),
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
        #(
          "timeZone",
          Some(mk_string(intl_data.format_time_zone_id(d.time_zone))),
        ),
        #(
          "hourCycle",
          option.map(d.hour_cycle, fn(hc) {
            mk_string(intl_data.hour_cycle_text(hc))
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
          option.map(d.fractional_second_digits, fn(n) { mk_int(n) }),
        ),
        #("timeZoneName", option.map(d.time_zone_name, tz_name_width_js)),
        #(
          "dateStyle",
          option.map(d.date_style, fn(s) {
            mk_string(intl_data.date_style_text(s))
          }),
        ),
        #(
          "timeStyle",
          option.map(d.time_style, fn(s) {
            mk_string(intl_data.time_style_text(s))
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
          intl_format.plural_categories_en(p.plural_type)
            |> list.map(intl_format.plural_category_text)
            |> list.map(mk_string),
        )
      #(
        present_pairs([
          #("locale", Some(mk_string(p.locale))),
          #("type", Some(mk_string(intl_data.plural_type_text(p.plural_type)))),
          #("notation", Some(mk_string(intl_data.notation_text(p.notation)))),
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
        #("type", mk_string(intl_data.list_format_type_text(l.list_type))),
        #("style", mk_string(intl_data.list_format_style_text(l.style))),
      ],
      st,
    )
    RelativeTimeFormatData(r) -> #(
      [
        #("locale", mk_string(r.locale)),
        #("style", mk_string(intl_data.relative_time_style_text(r.style))),
        #("numeric", mk_string(intl_data.relative_time_numeric_text(r.numeric))),
        #("numberingSystem", mk_string(r.numbering_system)),
      ],
      st,
    )
    SegmenterData(sg) -> #(
      [
        #("locale", mk_string(sg.locale)),
        #("granularity", mk_string(intl_data.granularity_text(sg.granularity))),
      ],
      st,
    )
    DisplayNamesData(d) -> #(
      present_pairs([
        #("locale", Some(mk_string(d.locale))),
        #("style", Some(mk_string(intl_data.name_width_text(d.style)))),
        #(
          "type",
          Some(mk_string(intl_data.display_names_type_text(d.display_type))),
        ),
        #(
          "fallback",
          Some(mk_string(intl_data.display_names_fallback_text(d.fallback))),
        ),
        #(
          "languageDisplay",
          option.map(d.language_display, fn(ld) {
            mk_string(intl_data.language_display_text(ld))
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
          #("style", mk_string(intl_data.duration_base_style_text(df.style))),
        ],
        list.flat_map(duration_unit_list(df), fn(u) {
          let #(unit, o) = u
          let name = duration_unit_js_name(unit)
          [
            #(name, mk_string(intl_data.duration_unit_style_text(o.style))),
            #(
              name <> "Display",
              mk_string(intl_data.duration_display_text(o.display)),
            ),
          ]
        }),
        case df.fractional_digits {
          Some(f) -> [#("fractionalDigits", mk_int(f))]
          None -> []
        },
      ]),
      st,
    )
    SegmentsData(_) | SegmentIteratorData(_) -> #([], st)
  }
  alloc_pojo(st, props)
}

fn use_grouping_js(g: IntlUseGrouping) -> JsVal {
  case g {
    GroupingNever -> mk_bool(False)
    GroupingAuto -> mk_string("auto")
    GroupingAlways -> mk_string("always")
    GroupingMin2 -> mk_string("min2")
  }
}

fn compact_display_of(n: Notation) -> Option(JsVal) {
  case n {
    NotationCompact(display:) ->
      Some(mk_string(intl_data.compact_display_text(display)))
    NotationStandard | NotationScientific | NotationEngineering -> None
  }
}

fn name_width_js(v: NameWidth) -> JsVal {
  mk_string(intl_data.name_width_text(v))
}

fn numeric_width_js(v: NumericWidth) -> JsVal {
  mk_string(intl_data.numeric_width_text(v))
}

fn month_width_js(v: MonthWidth) -> JsVal {
  mk_string(intl_data.month_width_text(v))
}

fn tz_name_width_js(v: TimeZoneNameWidth) -> JsVal {
  mk_string(intl_data.time_zone_name_width_text(v))
}

fn digit_option_pairs(
  dg: IntlDigitOptions,
  rest: List(#(String, Option(JsVal))),
) -> List(#(String, Option(JsVal))) {
  let num = fn(i) { mk_int(i) }
  [
    #("minimumIntegerDigits", Some(num(dg.minimum_integer_digits))),
    #(
      "minimumFractionDigits",
      option.map(dg.fraction_digits, fn(p) { num(p.min) }),
    ),
    #(
      "maximumFractionDigits",
      option.map(dg.fraction_digits, fn(p) { num(p.max) }),
    ),
    #(
      "minimumSignificantDigits",
      option.map(dg.significant_digits, fn(p) { num(p.min) }),
    ),
    #(
      "maximumSignificantDigits",
      option.map(dg.significant_digits, fn(p) { num(p.max) }),
    ),
    ..rest
  ]
}

fn digit_rounding_pairs(
  dg: IntlDigitOptions,
) -> List(#(String, Option(JsVal))) {
  [
    #("roundingIncrement", Some(mk_int(dg.rounding_increment))),
    #(
      "roundingMode",
      Some(mk_string(intl_data.rounding_mode_text(dg.rounding_mode))),
    ),
    #(
      "roundingPriority",
      Some(mk_string(intl_data.rounding_priority_text(dg.rounding_priority))),
    ),
    #(
      "trailingZeroDisplay",
      Some(
        mk_string(intl_data.trailing_zero_display_text(dg.trailing_zero_display)),
      ),
    ),
  ]
}

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
    BoundCollator -> {
      let #(h, _c, cached) = branded_collator(st, this, method)
      #(h, cached, 2)
    }
    BoundNumberFormat -> {
      let #(h, _nf, cached) = branded_number_format(st, this, method)
      #(h, cached, 1)
    }
    BoundDateTimeFormat -> {
      let #(h, _d, cached) = branded_date_time_format(st, this, method)
      #(h, cached, 1)
    }
  }
  case cached {
    Some(fn_h) -> #(mk_object(fn_h), st)
    None -> {
      // not rooted: receiver's bound field keeps it alive
      let #(fn_h, st) =
        rt_call.native_new(
          st,
          Some(st.realm.function.prototype),
          IntlN(IntlBoundMethod(service:, target:)),
          "",
          arity,
          constructible: False,
        )
      let st =
        rt_store.cell_update(st, target, fn(cell) {
          case cell {
            SObject(kind: IntlObj(data:, ..), ..) ->
              SObject(..cell, kind: IntlObj(data:, bound: Some(fn_h)))
            other -> other
          }
        })
      #(mk_object(fn_h), st)
    }
  }
}

fn bound_method(
  st: Agent,
  service: BoundGetterService,
  target: Handle,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let this = mk_object(target)
  let method = "bound Intl method"
  case service {
    BoundNumberFormat -> {
      let #(_h, nf, _bound) = branded_number_format(st, this, method)
      let #(parts, st) =
        number_format_parts(st, nf, first_arg_or_undefined(args))
      #(mk_string(intl_format.parts_to_string(parts)), st)
    }
    BoundDateTimeFormat -> {
      let #(_h, d, _bound) = branded_date_time_format(st, this, method)
      let #(parts, st) =
        date_time_format_parts(st, d, first_arg_or_undefined(args))
      #(mk_string(intl_format.parts_to_string(parts)), st)
    }
    BoundCollator -> {
      let #(_h, c, _bound) = branded_collator(st, this, method)
      let #(a, st) = rt_val.to_string(st, first_arg_or_undefined(args))
      let #(b, st) = rt_val.to_string(st, helpers.arg_at(args, 1))
      #(mk_int(collator_compare(c, a, b)), st)
    }
  }
}

fn number_format_options(
  nf: NumberFormatState,
) -> intl_format.NumberFormatOptions {
  let d = intl_format.default_number_format_options()
  with_digits(
    intl_format.NumberFormatOptions(
      ..d,
      locale: intl_format.locale_key(nf.locale),
      style: nf.style,
      use_grouping: nf.use_grouping,
      notation: nf.notation,
      sign_display: nf.sign_display,
    ),
    nf.digits,
  )
}

fn num_opts_from_plural(
  p: PluralRulesState,
) -> intl_format.NumberFormatOptions {
  let d = intl_format.default_number_format_options()
  with_digits(
    intl_format.NumberFormatOptions(
      ..d,
      locale: intl_format.locale_key(p.locale),
      notation: p.notation,
    ),
    p.digits,
  )
}

fn to_intl_number(st: Agent, v: JsVal) -> #(JsNum, Agent) {
  case classify(v) {
    // int.to_float would badarg on huge bigints
    KBig(n) -> #(rt_val.num_from_int(n), st)
    _ -> rt_val.to_number(st, v)
  }
}

fn number_format_parts(
  st: Agent,
  nf: NumberFormatState,
  x: JsVal,
) -> #(List(intl_format.Part), Agent) {
  let opts = number_format_options(nf)
  let nu = nf.numbering_system
  case classify(x) {
    KStr(text) ->
      case is_plain_decimal(string.trim(text)) {
        True -> #(
          intl_format.apply_numbering_system(
            intl_format.format_decimal_string_parts(opts, string.trim(text)),
            nu,
            intl_format.is_number_digit,
          ),
          st,
        )
        False -> number_format_number(st, x, opts, nu)
      }
    _ -> number_format_number(st, x, opts, nu)
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

fn number_format_number(
  st: Agent,
  x: JsVal,
  opts: intl_format.NumberFormatOptions,
  nu: String,
) -> #(List(intl_format.Part), Agent) {
  let #(n, st) = to_intl_number(st, x)
  let parts = case n {
    JNan -> intl_format.format_nan_parts(opts)
    JPosInf -> intl_format.format_infinity_parts(opts, negative: False)
    JNegInf -> intl_format.format_infinity_parts(opts, negative: True)
    JFloat(f) -> intl_format.format_number_parts(opts, f)
    JInt(i) -> intl_format.format_number_parts(opts, int.to_float(i))
  }
  #(
    intl_format.apply_numbering_system(parts, nu, intl_format.is_number_digit),
    st,
  )
}

fn number_format_range_parts(
  st: Agent,
  nf: NumberFormatState,
  x_v: JsVal,
  y_v: JsVal,
) -> #(List(intl_format.RangePart), Agent) {
  let st = case rt_val.is_undef(x_v) || rt_val.is_undef(y_v) {
    True -> rt_val.throw_type_error(st, "Invalid range arguments")
    False -> st
  }
  let #(x, st) = to_intl_number(st, x_v)
  let #(y, st) = to_intl_number(st, y_v)
  let st = case x, y {
    JNan, _ | _, JNan ->
      rt_val.throw_range_error(st, "Invalid range argument: NaN")
    _, _ -> st
  }
  // decimal strings stay exact beyond float precision
  let x_fmt = case classify(x_v) {
    KStr(_) -> x_v
    _ -> mk_number(x)
  }
  let y_fmt = case classify(y_v) {
    KStr(_) -> y_v
    _ -> mk_number(y)
  }
  let #(x_parts, st) = number_format_parts(st, nf, x_fmt)
  let #(y_parts, st) = number_format_parts(st, nf, y_fmt)
  #(
    intl_format.format_range_combine(
      intl_format.locale_key(nf.locale),
      x_parts,
      y_parts,
    ),
    st,
  )
}

type TemporalFormattable {
  FormattablePlain(PlainTemporal)
  FormattableInstant(epoch_ns: Int)
  FormattableZoned
}

type PlainTemporal {
  PlainDateFields(year: Int, month: Int, day: Int, calendar: String)
  PlainYearMonthFields(year: Int, month: Int, day: Int, calendar: String)
  PlainMonthDayFields(month: Int, day: Int, ref_year: Int, calendar: String)
  PlainTimeFields(hour: Int, minute: Int, second: Int, millisecond: Int)
  PlainDateTimeFields(
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

type AcceptedTemporal {
  AcceptedInstant(epoch_ns: Int)
  AcceptedPlain(PlainTemporal)
}

fn throw_zoned(st: Agent) -> a {
  rt_val.throw_type_error(
    st,
    "Temporal.ZonedDateTime cannot be formatted with Intl.DateTimeFormat; use Temporal.ZonedDateTime.prototype.toLocaleString instead",
  )
}

fn accept_temporal(st: Agent, t: TemporalFormattable) -> AcceptedTemporal {
  case t {
    FormattableInstant(epoch_ns:) -> AcceptedInstant(epoch_ns:)
    FormattablePlain(p) -> AcceptedPlain(p)
    FormattableZoned -> throw_zoned(st)
  }
}

fn date_time_format_temporal_value(
  st: Agent,
  v: JsVal,
) -> Option(TemporalFormattable) {
  case classify(v) {
    KHandle(h) ->
      case rt_store.cell_get(st, h) {
        SObject(kind: TemporalObj(data:), ..) -> temporal_formattable(data)
        _ -> None
      }
    _ -> None
  }
}

fn temporal_formattable(data: TemporalData) -> Option(TemporalFormattable) {
  case data {
    TemporalInstant(epoch_ns:) -> Some(FormattableInstant(epoch_ns:))
    TemporalDate(year:, month:, day:, calendar:) ->
      Some(
        FormattablePlain(PlainDateFields(
          year:,
          month:,
          day:,
          calendar: temporal_calendar.identifier(calendar),
        )),
      )
    TemporalYearMonth(year:, month:, day:, calendar:) ->
      Some(
        FormattablePlain(PlainYearMonthFields(
          year:,
          month:,
          day:,
          calendar: temporal_calendar.identifier(calendar),
        )),
      )
    TemporalMonthDay(month:, day:, ref_year:, calendar:) ->
      Some(
        FormattablePlain(PlainMonthDayFields(
          month:,
          day:,
          ref_year:,
          calendar: temporal_calendar.identifier(calendar),
        )),
      )
    TemporalTime(hour:, minute:, second:, millisecond:, ..) ->
      Some(
        FormattablePlain(PlainTimeFields(hour:, minute:, second:, millisecond:)),
      )
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
        FormattablePlain(PlainDateTimeFields(
          year:,
          month:,
          day:,
          hour:,
          minute:,
          second:,
          millisecond:,
          calendar: temporal_calendar.identifier(calendar),
        )),
      )
    TemporalZonedDateTime(..) -> Some(FormattableZoned)
    TemporalDuration(..) -> None
  }
}

fn same_temporal_kind(a: TemporalFormattable, b: TemporalFormattable) -> Bool {
  case a, b {
    FormattablePlain(a), FormattablePlain(b) -> same_plain_kind(a, b)
    FormattableInstant(..), FormattableInstant(..) -> True
    FormattableZoned, FormattableZoned -> True
    _, _ -> False
  }
}

fn same_plain_kind(a: PlainTemporal, b: PlainTemporal) -> Bool {
  case a, b {
    PlainDateFields(..), PlainDateFields(..) -> True
    PlainYearMonthFields(..), PlainYearMonthFields(..) -> True
    PlainMonthDayFields(..), PlainMonthDayFields(..) -> True
    PlainTimeFields(..), PlainTimeFields(..) -> True
    PlainDateTimeFields(..), PlainDateTimeFields(..) -> True
    _, _ -> False
  }
}

fn plain_component_rules(
  t: PlainTemporal,
) -> #(
  List(DateTimeComponent),
  List(DateTimeComponent),
  DateTimeComponents,
  Bool,
) {
  case t {
    PlainDateFields(..) -> #(
      [
        WeekdayComponent,
        EraComponent,
        YearComponent,
        MonthComponent,
        DayComponent,
      ],
      [WeekdayComponent, YearComponent, MonthComponent, DayComponent],
      date_defaults(),
      True,
    )
    PlainYearMonthFields(..) -> #(
      [EraComponent, YearComponent, MonthComponent],
      [YearComponent, MonthComponent],
      DateTimeComponents(
        ..intl_data.empty_date_time_components,
        year: Some(Numeric),
        month: Some(MonthNum(Numeric)),
      ),
      True,
    )
    PlainMonthDayFields(..) -> #(
      [MonthComponent, DayComponent],
      [MonthComponent, DayComponent],
      DateTimeComponents(
        ..intl_data.empty_date_time_components,
        month: Some(MonthNum(Numeric)),
        day: Some(Numeric),
      ),
      False,
    )
    PlainTimeFields(..) -> #(
      [
        DayPeriodComponent,
        HourComponent,
        MinuteComponent,
        SecondComponent,
        FractionalSecondDigitsComponent,
      ],
      [
        DayPeriodComponent,
        HourComponent,
        MinuteComponent,
        SecondComponent,
        FractionalSecondDigitsComponent,
      ],
      time_defaults(),
      False,
    )
    PlainDateTimeFields(..) -> #(
      [
        WeekdayComponent, EraComponent, YearComponent, MonthComponent,
        DayComponent, DayPeriodComponent, HourComponent, MinuteComponent,
        SecondComponent, FractionalSecondDigitsComponent,
      ],
      [
        WeekdayComponent, YearComponent, MonthComponent, DayComponent,
        DayPeriodComponent, HourComponent, MinuteComponent, SecondComponent,
        FractionalSecondDigitsComponent,
      ],
      merge_components(date_defaults(), time_defaults()),
      True,
    )
  }
}

fn date_time_format_temporal_state(
  st: Agent,
  d: DateTimeFormatState,
  t: TemporalFormattable,
) -> DateTimeFormatState {
  case t {
    FormattableZoned -> throw_zoned(st)
    // instant defaults to date and time, ctor only date
    FormattableInstant(..) ->
      case
        d.explicit != []
        || option.is_some(d.date_style)
        || option.is_some(d.time_style)
      {
        True -> d
        False ->
          with_components(d, merge_components(d.components, time_defaults()))
      }
    FormattablePlain(p) -> {
      let cal_ok = case p {
        PlainDateFields(calendar:, ..) | PlainDateTimeFields(calendar:, ..) ->
          calendar == "iso8601" || calendar == d.calendar
        PlainYearMonthFields(calendar:, ..)
        | PlainMonthDayFields(calendar:, ..) -> calendar == d.calendar
        PlainTimeFields(..) -> True
      }
      use Nil <- helpers.guard(cal_ok, fn() {
        rt_val.throw_range_error(
          st,
          "Temporal object calendar does not match DateTimeFormat calendar",
        )
      })
      let #(allowed, required, defaults, copy_era) = plain_component_rules(p)
      let has_styles =
        option.is_some(d.date_style) || option.is_some(d.time_style)
      case has_styles {
        True -> {
          let style_ok = case p {
            PlainDateFields(..)
            | PlainYearMonthFields(..)
            | PlainMonthDayFields(..) -> option.is_some(d.date_style)
            PlainTimeFields(..) -> option.is_some(d.time_style)
            PlainDateTimeFields(..) -> True
          }
          case style_ok {
            True -> with_components(d, keep_components(d.components, allowed))
            False ->
              rt_val.throw_type_error(
                st,
                "DateTimeFormat has no suitable format for this Temporal type",
              )
          }
        }
        False -> {
          let in_required =
            list.filter(d.explicit, fn(name) { list.contains(required, name) })
          let era = case copy_era {
            True -> d.components.era
            False -> None
          }
          case in_required {
            [] ->
              case d.explicit {
                [] -> with_components(d, DateTimeComponents(..defaults, era:))
                _ ->
                  rt_val.throw_type_error(
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
              with_components(d, DateTimeComponents(..kept, era:))
            }
          }
        }
      }
    }
  }
}

fn with_components(
  d: DateTimeFormatState,
  components: DateTimeComponents,
) -> DateTimeFormatState {
  DateTimeFormatState(..d, components:)
}

fn civil_week_day(year: Int, month: Int, day: Int) -> Int {
  gregorian.weekday_from_days(days_from_civil(year, month, day))
}

fn date_time_format_temporal_fields(
  d: DateTimeFormatState,
  t: AcceptedTemporal,
  now_ms: fn() -> Int,
) -> #(intl_format.DateFields, Int) {
  case t {
    AcceptedInstant(epoch_ns:) -> {
      let ms = floor_div(epoch_ns, 1_000_000)
      let offset = intl_timezone.offset_at(d.time_zone, ms)
      #(intl_format.fields_from_epoch_ms(int.to_float(ms), offset), offset)
    }
    AcceptedPlain(p) -> #(
      plain_temporal_fields(p),
      intl_timezone.offset_at(d.time_zone, now_ms()),
    )
  }
}

fn plain_temporal_fields(t: PlainTemporal) -> intl_format.DateFields {
  case t {
    PlainDateFields(year:, month:, day:, ..)
    | PlainYearMonthFields(year:, month:, day:, ..) ->
      intl_format.DateFields(
        year:,
        month:,
        day:,
        hour: 12,
        minute: 0,
        second: 0,
        millisecond: 0,
        week_day: civil_week_day(year, month, day),
      )
    PlainMonthDayFields(month:, day:, ref_year:, ..) ->
      intl_format.DateFields(
        year: ref_year,
        month:,
        day:,
        hour: 12,
        minute: 0,
        second: 0,
        millisecond: 0,
        week_day: civil_week_day(ref_year, month, day),
      )
    PlainTimeFields(hour:, minute:, second:, millisecond:) ->
      intl_format.DateFields(
        year: 1970,
        month: 1,
        day: 1,
        hour:,
        minute:,
        second:,
        millisecond:,
        week_day: 4,
      )
    PlainDateTimeFields(
      year:,
      month:,
      day:,
      hour:,
      minute:,
      second:,
      millisecond:,
      ..,
    ) ->
      intl_format.DateFields(
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

fn date_time_format_parts(
  st: Agent,
  d: DateTimeFormatState,
  date_v: JsVal,
) -> #(List(intl_format.Part), Agent) {
  case date_time_format_temporal_value(st, date_v) {
    Some(t) -> {
      let d = date_time_format_temporal_state(st, d, t)
      let accepted = accept_temporal(st, t)
      let #(fields, offset) =
        date_time_format_temporal_fields(d, accepted, st.hooks.wall_clock_ms)
      let parts = build_date_time_parts(d, fields, offset)
      #(
        intl_format.apply_numbering_system(
          parts,
          d.numbering_system,
          intl_format.is_date_numeric,
        ),
        st,
      )
    }
    None -> date_time_format_parts_number(st, d, date_v)
  }
}

fn date_time_format_parts_number(
  st: Agent,
  d: DateTimeFormatState,
  date_v: JsVal,
) -> #(List(intl_format.Part), Agent) {
  let #(fields, offset, st) = date_time_format_fields_number(st, d, date_v)
  let parts = build_date_time_parts(d, fields, offset)
  #(
    intl_format.apply_numbering_system(
      parts,
      d.numbering_system,
      intl_format.is_date_numeric,
    ),
    st,
  )
}

fn build_date_time_parts(
  d: DateTimeFormatState,
  fields: intl_format.DateFields,
  offset: Int,
) -> List(intl_format.Part) {
  let DateTimeComponents(
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
  let year_text = fn(width) { format_numeric_width(width, display_year) }
  let weekday_parts = case weekday {
    Some(w) -> [
      intl_format.Part(
        PartWeekday,
        intl_format.weekday_name(fields.week_day, w),
      ),
    ]
    None -> []
  }
  let date_parts = case month {
    Some(MonthName(mw)) -> {
      let m_part = [
        intl_format.Part(PartMonth, intl_format.month_name(fields.month, mw)),
      ]
      let d_part = case day {
        Some(dw) -> [
          intl_format.Part(PartLiteral, " "),
          intl_format.Part(PartDay, format_numeric_width(dw, fields.day)),
        ]
        None -> []
      }
      let y_part = case year {
        Some(yw) ->
          case day {
            Some(_) -> [
              intl_format.Part(PartLiteral, ", "),
              intl_format.Part(PartYear, year_text(yw)),
            ]
            None -> [
              intl_format.Part(PartLiteral, " "),
              intl_format.Part(PartYear, year_text(yw)),
            ]
          }
        None -> []
      }
      list.flatten([m_part, d_part, y_part])
    }
    Some(MonthNum(_)) | None -> {
      let month_num = case month {
        Some(MonthNum(mw)) -> Some(format_numeric_width(mw, fields.month))
        Some(MonthName(_)) | None -> None
      }
      let lang = intl_locale.language_of(d.locale)
      let dotted =
        list.contains(
          ["de", "fi", "ru", "cs", "tr", "nb", "pl", "uk", "bg", "sr", "lv"],
          lang,
        )
      let m_pair = #(PartMonth, month_num)
      let d_pair = #(
        PartDay,
        option.map(day, fn(dw) { format_numeric_width(dw, fields.day) }),
      )
      let y_pair = #(PartYear, option.map(year, year_text))
      let raw = case dotted {
        True -> [d_pair, m_pair, y_pair]
        False -> [m_pair, d_pair, y_pair]
      }
      let pieces =
        present_pairs(raw)
        |> list.map(fn(p) {
          let #(t, v) = p
          intl_format.Part(t, v)
        })
      case dotted {
        True -> join_parts(pieces, ".")
        False -> join_parts(pieces, "/")
      }
    }
  }
  let date_parts = case era, date_parts {
    Some(e), [_, ..] ->
      list.append(date_parts, [
        intl_format.Part(PartLiteral, " "),
        intl_format.Part(PartEra, intl_format.era_name(fields.year, e)),
      ])
    _, _ -> date_parts
  }
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
    Some(TwoDigit) -> [intl_format.Part(PartHour, digits.pad2(display_hour))]
    Some(Numeric) -> [intl_format.Part(PartHour, int.to_string(display_hour))]
    None -> []
  }
  let minute_parts = case minute {
    Some(width) -> {
      let v = case hour, second {
        Some(_), _ -> digits.pad2(fields.minute)
        None, Some(_) -> digits.pad2(fields.minute)
        None, None ->
          case width {
            TwoDigit -> digits.pad2(fields.minute)
            Numeric -> int.to_string(fields.minute)
          }
      }
      case hour_parts {
        [] -> [intl_format.Part(PartMinute, v)]
        _ -> [
          intl_format.Part(PartLiteral, ":"),
          intl_format.Part(PartMinute, v),
        ]
      }
    }
    None -> []
  }
  let second_parts = case second {
    Some(width) -> {
      let v = case minute {
        Some(_) -> digits.pad2(fields.second)
        None ->
          case width {
            TwoDigit -> digits.pad2(fields.second)
            Numeric -> int.to_string(fields.second)
          }
      }
      case minute_parts {
        [] -> [intl_format.Part(PartSecond, v)]
        _ -> [
          intl_format.Part(PartLiteral, ":"),
          intl_format.Part(PartSecond, v),
        ]
      }
    }
    None -> []
  }
  let fractional_parts = case fractional {
    Some(digits) -> {
      let ms3 = string.pad_start(int.to_string(fields.millisecond), 3, "0")
      let v = string.slice(ms3, 0, digits)
      case second_parts {
        [] -> [intl_format.Part(PartFractionalSecond, v)]
        _ -> [
          intl_format.Part(PartLiteral, "."),
          intl_format.Part(PartFractionalSecond, v),
        ]
      }
    }
    None -> []
  }
  let day_period_parts = case day_period, hour {
    Some(dpw), _ -> [
      intl_format.Part(PartLiteral, " "),
      intl_format.Part(
        PartDayPeriod,
        intl_format.day_period_name(fields.hour, fields.minute, dpw),
      ),
    ]
    None, Some(_) ->
      case dp {
        "" -> []
        _ -> [
          intl_format.Part(PartLiteral, " "),
          intl_format.Part(PartDayPeriod, dp),
        ]
      }
    None, None -> []
  }
  let day_period_parts = case hour, day_period {
    None, Some(dpw) -> [
      intl_format.Part(
        PartDayPeriod,
        intl_format.day_period_name(fields.hour, fields.minute, dpw),
      ),
    ]
    _, _ -> day_period_parts
  }
  let tz_parts = case tz_name {
    Some(width) -> {
      let name =
        intl_timezone.display_name(
          intl_data.format_time_zone_id(d.time_zone),
          width,
          offset,
        )
      [
        intl_format.Part(PartLiteral, " "),
        intl_format.Part(PartTimeZoneName, name),
      ]
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
    [], [intl_format.Part(PartLiteral, _), ..rest] -> rest
    _, _ -> list.append(time_parts, tz_parts)
  }
  let all = case weekday_parts, date_parts, time_parts {
    [], [], t -> t
    w, [], [] -> w
    [], d, [] -> d
    w, d, [] -> list.flatten([w, [intl_format.Part(PartLiteral, ", ")], d])
    [], d, t -> list.flatten([d, [intl_format.Part(PartLiteral, ", ")], t])
    w, [], t -> list.flatten([w, [intl_format.Part(PartLiteral, " ")], t])
    w, d, t ->
      list.flatten([
        w,
        [intl_format.Part(PartLiteral, ", ")],
        d,
        [intl_format.Part(PartLiteral, ", ")],
        t,
      ])
  }
  all
}

fn am_pm(hour: Int) -> String {
  case hour < 12 {
    True -> "AM"
    False -> "PM"
  }
}

// 2-digit keeps the low two digits
fn format_numeric_width(width: NumericWidth, n: Int) -> String {
  case width {
    TwoDigit -> digits.pad2(n % 100)
    Numeric -> int.to_string(n)
  }
}

fn join_parts(
  pieces: List(intl_format.Part),
  sep: String,
) -> List(intl_format.Part) {
  case pieces {
    [] -> []
    [first, ..rest] ->
      list.fold(rest, [first], fn(acc, p) {
        [p, intl_format.Part(PartLiteral, sep), ..acc]
      })
      |> list.reverse
  }
}

fn date_time_format_range_parts(
  st: Agent,
  d: DateTimeFormatState,
  x_v: JsVal,
  y_v: JsVal,
) -> #(List(intl_format.RangePart), Agent) {
  let defined = !{ rt_val.is_undef(x_v) || rt_val.is_undef(y_v) }
  use Nil <- helpers.guard(defined, fn() {
    rt_val.throw_type_error(st, "Invalid range arguments")
  })
  let tx = date_time_format_temporal_value(st, x_v)
  let ty = date_time_format_temporal_value(st, y_v)
  let #(x_v, st) = case tx {
    Some(_) -> #(x_v, st)
    None -> {
      let #(n, st) = rt_val.to_number(st, x_v)
      #(mk_number(n), st)
    }
  }
  let #(y_v, st) = case ty {
    Some(_) -> #(y_v, st)
    None -> {
      let #(n, st) = rt_val.to_number(st, y_v)
      #(mk_number(n), st)
    }
  }
  let same_type_error = fn() {
    rt_val.throw_type_error(
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
          let adjusted = date_time_format_temporal_state(st, d, a)
          let _d_y = date_time_format_temporal_state(st, d, b)
          adjusted
        }
      }
    _, _ -> same_type_error()
  }
  let #(collapsed, st) = date_time_format_collapsed_range(st, d, x_v, y_v)
  case collapsed {
    Some(parts) -> #(parts, st)
    None -> {
      let #(x_parts, st) = date_time_format_parts(st, d, x_v)
      let #(y_parts, st) = date_time_format_parts(st, d, y_v)
      let sourced = fn(p: intl_format.Part, source) {
        intl_format.RangePart(p.type_, p.value, source)
      }
      case
        intl_format.parts_to_string(x_parts)
        == intl_format.parts_to_string(y_parts)
      {
        True -> #(list.map(x_parts, sourced(_, intl_format.SourceShared)), st)
        False -> #(
          list.flatten([
            list.map(x_parts, sourced(_, intl_format.SourceStart)),
            [
              sourced(
                intl_format.Part(PartLiteral, " – "),
                intl_format.SourceShared,
              ),
            ],
            list.map(y_parts, sourced(_, intl_format.SourceEnd)),
          ]),
          st,
        )
      }
    }
  }
}

fn date_time_format_collapsed_range(
  st: Agent,
  d: DateTimeFormatState,
  x_v: JsVal,
  y_v: JsVal,
) -> #(Option(List(intl_format.RangePart)), Agent) {
  let c = d.components
  let date_only =
    c.hour == None && c.minute == None && c.second == None && c.weekday == None
  case c.month, c.year, c.day, date_only {
    Some(MonthName(month_width)), Some(year_width), Some(day_width), True -> {
      let #(xf, st) = date_time_format_fields(st, d, x_v)
      let #(yf, st) = date_time_format_fields(st, d, y_v)
      let day_style = day_width
      let display_year = case xf.year <= 0 {
        True -> 1 - xf.year
        False -> xf.year
      }
      let year_text = case year_width {
        TwoDigit -> digits.pad2(display_year % 100)
        Numeric -> int.to_string(display_year)
      }
      let mname = fn(m) { intl_format.month_name(m, month_width) }
      case xf.year == yf.year {
        False -> #(None, st)
        True ->
          case xf.month == yf.month, xf.day != yf.day {
            True, True -> {
              let parts = [
                intl_format.RangePart(
                  PartMonth,
                  mname(xf.month),
                  intl_format.SourceShared,
                ),
                intl_format.RangePart(
                  PartLiteral,
                  " ",
                  intl_format.SourceShared,
                ),
                intl_format.RangePart(
                  PartDay,
                  format_numeric_width(day_style, xf.day),
                  intl_format.SourceStart,
                ),
                intl_format.RangePart(
                  PartLiteral,
                  " – ",
                  intl_format.SourceShared,
                ),
                intl_format.RangePart(
                  PartDay,
                  format_numeric_width(day_style, yf.day),
                  intl_format.SourceEnd,
                ),
                intl_format.RangePart(
                  PartLiteral,
                  ", ",
                  intl_format.SourceShared,
                ),
                intl_format.RangePart(
                  PartYear,
                  year_text,
                  intl_format.SourceShared,
                ),
              ]
              #(Some(parts), st)
            }
            False, _ -> {
              let parts = [
                intl_format.RangePart(
                  PartMonth,
                  mname(xf.month),
                  intl_format.SourceStart,
                ),
                intl_format.RangePart(PartLiteral, " ", intl_format.SourceStart),
                intl_format.RangePart(
                  PartDay,
                  format_numeric_width(day_style, xf.day),
                  intl_format.SourceStart,
                ),
                intl_format.RangePart(
                  PartLiteral,
                  " – ",
                  intl_format.SourceShared,
                ),
                intl_format.RangePart(
                  PartMonth,
                  mname(yf.month),
                  intl_format.SourceEnd,
                ),
                intl_format.RangePart(PartLiteral, " ", intl_format.SourceEnd),
                intl_format.RangePart(
                  PartDay,
                  format_numeric_width(day_style, yf.day),
                  intl_format.SourceEnd,
                ),
                intl_format.RangePart(
                  PartLiteral,
                  ", ",
                  intl_format.SourceShared,
                ),
                intl_format.RangePart(
                  PartYear,
                  year_text,
                  intl_format.SourceShared,
                ),
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

fn date_time_format_fields(
  st: Agent,
  d: DateTimeFormatState,
  date_v: JsVal,
) -> #(intl_format.DateFields, Agent) {
  case date_time_format_temporal_value(st, date_v) {
    Some(t) -> {
      let accepted = accept_temporal(st, t)
      let #(fields, _offset) =
        date_time_format_temporal_fields(d, accepted, st.hooks.wall_clock_ms)
      #(fields, st)
    }
    None -> {
      let #(fields, _offset, st) = date_time_format_fields_number(st, d, date_v)
      #(fields, st)
    }
  }
}

fn date_time_format_fields_number(
  st: Agent,
  d: DateTimeFormatState,
  date_v: JsVal,
) -> #(intl_format.DateFields, Int, Agent) {
  let #(tv, st) = case classify(date_v) {
    KUndef -> #(JInt(st.hooks.wall_clock_ms()), st)
    _ -> rt_val.to_number(st, date_v)
  }
  let tv_f = case tv {
    JInt(i) -> time_clip(st, int.to_float(i))
    JFloat(f) -> time_clip(st, int.to_float(float.truncate(f)))
    JNan | JPosInf | JNegInf ->
      rt_val.throw_range_error(st, "Invalid time value")
  }
  let offset = intl_timezone.offset_at(d.time_zone, float.truncate(tv_f))
  #(intl_format.fields_from_epoch_ms(tv_f, offset), offset, st)
}

fn time_clip(st: Agent, f: Float) -> Float {
  case float.absolute_value(f) <=. 8.64e15 {
    True -> f
    False -> rt_val.throw_range_error(st, "Invalid time value")
  }
}

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
      let #(parts, st) = number_format_parts(st, nf, arg0)
      parts_to_js(st, parts)
    }
    IntlFormatRange, NumberFormatData(nf) -> {
      let #(parts, st) = number_format_range_parts(st, nf, arg0, arg1)
      #(mk_string(range_parts_to_string(parts)), st)
    }
    IntlFormatRangeToParts, NumberFormatData(nf) -> {
      let #(parts, st) = number_format_range_parts(st, nf, arg0, arg1)
      parts_to_js_sourced(st, parts)
    }
    IntlFormatToParts, DateTimeFormatData(d) -> {
      let #(parts, st) = date_time_format_parts(st, d, arg0)
      parts_to_js(st, parts)
    }
    IntlFormatRange, DateTimeFormatData(d) -> {
      let #(parts, st) = date_time_format_range_parts(st, d, arg0, arg1)
      #(mk_string(range_parts_to_string(parts)), st)
    }
    IntlFormatRangeToParts, DateTimeFormatData(d) -> {
      let #(parts, st) = date_time_format_range_parts(st, d, arg0, arg1)
      parts_to_js_sourced(st, parts)
    }
    IntlSelect, PluralRulesData(p) -> {
      let #(n, st) = rt_val.to_number(st, arg0)
      #(mk_string(intl_format.plural_category_text(plural_select(p, n))), st)
    }
    IntlSelectRange, PluralRulesData(_) -> {
      let defined = !{ rt_val.is_undef(arg0) || rt_val.is_undef(arg1) }
      use Nil <- helpers.guard(defined, fn() {
        rt_val.throw_type_error(st, "Invalid selectRange arguments")
      })
      let #(x, st) = rt_val.to_number(st, arg0)
      let #(y, st) = rt_val.to_number(st, arg1)
      case x, y {
        JNan, _ | _, JNan ->
          rt_val.throw_range_error(st, "Invalid selectRange argument: NaN")
        _, _ -> #(
          mk_string(intl_format.plural_category_text(intl_format.PluralOther)),
          st,
        )
      }
    }
    IntlFormat, ListFormatData(l) -> {
      let #(items, st) = string_list_from_iterable(st, arg0)
      let parts = intl_format.list_format_parts(l.list_type, l.style, items)
      #(mk_string(intl_format.parts_to_string(parts)), st)
    }
    IntlFormatToParts, ListFormatData(l) -> {
      let #(items, st) = string_list_from_iterable(st, arg0)
      let parts = intl_format.list_format_parts(l.list_type, l.style, items)
      parts_to_js(st, parts)
    }
    IntlFormat, RelativeTimeFormatData(r) -> {
      let #(parts, st) = relative_time_format_method_parts(st, r, arg0, arg1)
      #(mk_string(intl_format.unit_parts_to_string(parts)), st)
    }
    IntlFormatToParts, RelativeTimeFormatData(r) -> {
      let #(parts, st) = relative_time_format_method_parts(st, r, arg0, arg1)
      parts_to_js_with_unit(st, parts)
    }
    IntlOf, DisplayNamesData(dn) -> display_names_of(st, dn, arg0)
    IntlFormat, DurationFormatData(df) -> {
      let #(parts, st) = duration_parts(st, df, arg0)
      #(mk_string(intl_format.unit_parts_to_string(parts)), st)
    }
    IntlFormatToParts, DurationFormatData(df) -> {
      let #(parts, st) = duration_parts(st, df, arg0)
      parts_to_js_with_unit(st, parts)
    }
    IntlSegmentsContaining, SegmentsData(sg) ->
      segments_containing(st, sg, arg0)
    IntlSegmentIteratorNext, SegmentIteratorData(it) ->
      segment_iterator_next(st, h, it)
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
      rt_val.throw_type_error(st, js_name <> " called on incompatible receiver")
  }
}

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
    StringToLocaleLowerCase -> host_locale_case(st, this, arg0, upper: False)
    StringToLocaleUpperCase -> host_locale_case(st, this, arg0, upper: True)
    DateToLocaleString -> host_date_to_locale(st, this, arg0, arg1, DateAndTime)
    DateToLocaleDateString ->
      host_date_to_locale(st, this, arg0, arg1, DateOnly)
    DateToLocaleTimeString ->
      host_date_to_locale(st, this, arg0, arg1, TimeOnly)
  }
}

fn host_number_to_locale_string(
  st: Agent,
  this: JsVal,
  locales: JsVal,
  options: JsVal,
) -> #(JsVal, Agent) {
  let not_number = fn() {
    rt_val.throw_type_error(
      st,
      "Number.prototype.toLocaleString requires that 'this' be a Number",
    )
  }
  let n = case classify(this) {
    KNum(n) -> n
    KHandle(h) ->
      case rt_store.cell_get(st, h) {
        SObject(kind: NumberObj(value: n), ..) -> n
        _ -> not_number()
      }
    _ -> not_number()
  }
  let #(nf, st) = number_format_state(st, locales, options)
  let #(parts, st) = number_format_parts(st, nf, mk_number(n))
  #(mk_string(intl_format.parts_to_string(parts)), st)
}

fn host_bigint_to_locale_string(
  st: Agent,
  this: JsVal,
  locales: JsVal,
  options: JsVal,
) -> #(JsVal, Agent) {
  let not_bigint = fn() {
    rt_val.throw_type_error(
      st,
      "BigInt.prototype.toLocaleString requires that 'this' be a BigInt",
    )
  }
  let n = case classify(this) {
    KBig(n) -> n
    KHandle(h) ->
      case rt_store.cell_get(st, h) {
        SObject(kind: BigIntObj(value: n), ..) -> n
        _ -> not_bigint()
      }
    _ -> not_bigint()
  }
  let #(nf, st) = number_format_state(st, locales, options)
  let #(parts, st) = number_format_parts(st, nf, mk_string(int.to_string(n)))
  #(mk_string(intl_format.parts_to_string(parts)), st)
}

fn host_locale_compare(
  st: Agent,
  this: JsVal,
  that_v: JsVal,
  locales: JsVal,
  options: JsVal,
) -> #(JsVal, Agent) {
  use Nil <- helpers.guard(!rt_val.is_nullish(this), fn() {
    rt_val.throw_type_error(
      st,
      "String.prototype.localeCompare called on null or undefined",
    )
  })
  let #(s, st) = rt_val.to_string(st, this)
  let #(that, st) = rt_val.to_string(st, that_v)
  let #(c, st) = collator_state(st, locales, options)
  #(mk_int(collator_compare(c, s, that)), st)
}

fn host_locale_case(
  st: Agent,
  this: JsVal,
  locales: JsVal,
  upper upper: Bool,
) -> #(JsVal, Agent) {
  use Nil <- helpers.guard(!rt_val.is_nullish(this), fn() {
    rt_val.throw_type_error(st, "method called on null or undefined")
  })
  let #(s, st) = rt_val.to_string(st, this)
  let #(tag_list, st) = canonicalize_locale_list(st, locales)
  let lang = case tag_list {
    [first, ..] -> intl_locale.language_of(first)
    [] -> "en"
  }
  // deliberately not a lookup of String.prototype.toLowerCase
  let pre = case lang {
    "tr" | "az" -> intl_casing.turkic_case(s, upper)
    "lt" -> intl_casing.lithuanian_case(s, upper)
    _ -> s
  }
  let cased = case upper {
    True -> string.uppercase(pre)
    False -> unicode_case.to_lower_case(pre)
  }
  #(mk_string(cased), st)
}

fn host_date_to_locale(
  st: Agent,
  this: JsVal,
  locales: JsVal,
  options: JsVal,
  required: RequiredComponents,
) -> #(JsVal, Agent) {
  let not_date = fn() {
    rt_val.throw_type_error(st, "this is not a Date object")
  }
  let tv = case classify(this) {
    KHandle(h) ->
      case rt_store.cell_get(st, h) {
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
  let #(d, st) =
    date_time_format_state_required(st, locales, options, defaults, required)
  case tv {
    JInt(_) | JFloat(_) -> {
      let #(parts, st) = date_time_format_parts(st, d, mk_number(tv))
      #(mk_string(intl_format.parts_to_string(parts)), st)
    }
    JNan | JPosInf | JNegInf -> #(mk_string("Invalid Date"), st)
  }
}

fn plural_select(p: PluralRulesState, n: JsNum) -> intl_format.PluralCategory {
  let finite = fn(f) {
    let opts =
      intl_format.NumberFormatOptions(
        ..num_opts_from_plural(p),
        style: StyleDecimal,
        use_grouping: GroupingNever,
        sign_display: SignNever,
      )
    let #(int_digits, frac_digits) =
      intl_format.plural_operands(intl_format.format_number_parts(opts, f))
    intl_format.plural_select_en(p.plural_type, int_digits, frac_digits)
  }
  case n {
    JInt(i) -> finite(int.to_float(i))
    JFloat(f) -> finite(f)
    JNan | JPosInf | JNegInf -> intl_format.PluralOther
  }
}

fn relative_time_format_method_parts(
  st: Agent,
  r: RelativeTimeFormatState,
  value_v: JsVal,
  unit_v: JsVal,
) -> #(List(intl_format.UnitPart), Agent) {
  let #(n, st) = rt_val.to_number(st, value_v)
  let f = case n {
    JInt(i) -> int.to_float(i)
    JFloat(f) -> f
    JNan | JPosInf | JNegInf ->
      rt_val.throw_range_error(st, "Value need to be finite number")
  }
  let #(unit_text, st) = rt_val.to_string(st, unit_v)
  let unit = case singular_unit(unit_text) {
    Some(u) -> u
    None -> rt_val.throw_range_error(st, "Invalid unit argument: " <> unit_text)
  }
  let abs_opts =
    intl_format.NumberFormatOptions(
      ..intl_format.default_number_format_options(),
      sign_display: SignNever,
    )
  let value_parts =
    intl_format.format_number_parts(abs_opts, float.absolute_value(f))
  let value_parts =
    intl_format.apply_numbering_system(
      value_parts,
      r.numbering_system,
      intl_format.is_number_digit,
    )
  #(
    intl_format.relative_time_parts_en(r.style, r.numeric, f, unit, value_parts),
    st,
  )
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

fn string_list_from_iterable(
  st: Agent,
  iterable: JsVal,
) -> #(List(String), Agent) {
  case classify(iterable) {
    KUndef -> #([], st)
    KStr(text) -> {
      let items =
        string.to_utf_codepoints(text)
        |> list.map(fn(cp) { string.from_utf_codepoints([cp]) })
      #(items, st)
    }
    _ -> {
      let #(method, st) =
        rt_obj.get_prop(st, iterable, SymbolKey(types.symbol_iterator))
      use Nil <- helpers.guard(rt_val.is_callable(st, method), fn() {
        rt_val.throw_type_error(st, "object is not iterable")
      })
      let #(iter, st) = rt_call.call(st, method, iterable, [])
      use Nil <- helpers.guard(rt_val.is_object(iter), fn() {
        rt_val.throw_type_error(st, "iterator result is not an object")
      })
      let #(next_fn, st) = rt_obj.get_prop(st, iter, StringKey(Named("next")))
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
  let #(step, st) = rt_call.call(st, next_fn, iter, [])
  use Nil <- helpers.guard(rt_val.is_object(step), fn() {
    rt_val.throw_type_error(st, "iterator result is not an object")
  })
  let #(done, st) = rt_obj.get_prop(st, step, StringKey(Named("done")))
  case rt_val.to_boolean(done) {
    True -> #(list.reverse(acc), st)
    False -> {
      let #(v, st) = rt_obj.get_prop(st, step, StringKey(Named("value")))
      case classify(v) {
        KStr(s) -> iterate_strings(st, iter, next_fn, [s, ..acc])
        _ ->
          rt_val.throw_type_error(
            st,
            "Iterable yielded a value that is not a string",
          )
      }
    }
  }
}

fn display_names_of(
  st: Agent,
  dn: DisplayNamesState,
  code_v: JsVal,
) -> #(JsVal, Agent) {
  let #(code, st) = rt_val.to_string(st, code_v)
  let type_ = dn.display_type
  let fallback = dn.fallback
  let #(canonical, name) = case type_ {
    LanguageNames ->
      case intl_locale.parse(code) {
        Ok(lid) ->
          case lid.extensions, lid.private_use {
            [], [] -> {
              let tag = intl_locale.to_string(intl_locale.canonicalize(lid))
              #(tag, intl_format.language_display_name(tag))
            }
            _, _ ->
              rt_val.throw_range_error(st, "invalid language code: " <> code)
          }
        Error(Nil) ->
          rt_val.throw_range_error(st, "invalid language code: " <> code)
      }
    RegionNames ->
      case intl_locale.is_region(code) {
        True -> {
          let r = string.uppercase(code)
          #(r, intl_format.region_display_name(r))
        }
        False -> rt_val.throw_range_error(st, "invalid region code: " <> code)
      }
    ScriptNames ->
      case intl_locale.is_script(code) {
        True -> {
          let s = intl_locale.titlecase(code)
          #(s, intl_format.script_display_name(s))
        }
        False -> rt_val.throw_range_error(st, "invalid script code: " <> code)
      }
    CurrencyNames ->
      case intl_locale.is_alpha(code) && string.length(code) == 3 {
        True -> {
          let c = string.uppercase(code)
          #(c, intl_format.currency_display_name(c))
        }
        False -> rt_val.throw_range_error(st, "invalid currency code: " <> code)
      }
    CalendarNames ->
      case intl_locale.is_type_sequence(string.lowercase(code)) {
        True -> {
          let c = string.lowercase(code)
          let name = case c {
            "gregory" -> Some("Gregorian Calendar")
            "iso8601" -> Some("ISO-8601 Calendar")
            _ -> None
          }
          #(c, name)
        }
        False -> rt_val.throw_range_error(st, "invalid calendar code: " <> code)
      }
    DateTimeFieldNames ->
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
          rt_val.throw_range_error(st, "invalid dateTimeField code: " <> code)
      }
  }
  case name, fallback {
    Some(n), _ -> #(mk_string(n), st)
    None, CodeFallback -> #(mk_string(canonical), st)
    None, NoFallback -> #(mk_undefined(), st)
  }
}

fn duration_parts(
  st: Agent,
  df: DurationFormatState,
  duration_v: JsVal,
) -> #(List(intl_format.UnitPart), Agent) {
  let #(fields, st) = to_duration_record(st, duration_v)
  let values = duration_values(fields)
  let has_neg = list.any(values, fn(v) { v <. 0.0 })
  let has_pos = list.any(values, fn(v) { v >. 0.0 })
  let st = case has_neg && has_pos {
    True ->
      rt_val.throw_range_error(st, "Duration fields must have consistent sign")
    False -> st
  }
  let st = case is_valid_duration(fields) {
    True -> st
    False ->
      rt_val.throw_range_error(st, "Duration field value is out of range")
  }
  #(build_duration_parts(df, fields), st)
}

fn to_duration_record(
  st: Agent,
  duration_v: JsVal,
) -> #(DurationRecord, Agent) {
  case classify(duration_v) {
    KStr(text) ->
      case parse_iso_duration(text) {
        Ok(fields) -> #(fields, st)
        Error(Nil) ->
          rt_val.throw_range_error(st, "Invalid duration string: " <> text)
      }
    KHandle(_) -> {
      let #(fields, st, any_defined) =
        list.fold(duration_units, #(zero_duration, st, False), fn(acc, unit) {
          let #(fields, st, any) = acc
          let name = duration_unit_js_name(unit)
          let #(v, st) = rt_obj.get_prop(st, duration_v, StringKey(Named(name)))
          case classify(v) {
            KUndef -> #(fields, st, any)
            _ -> {
              let #(n, st) = rt_val.to_number(st, v)
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
                      rt_val.throw_range_error(
                        st,
                        name <> " must be an integral number",
                      )
                  }
                JNan | JPosInf | JNegInf ->
                  rt_val.throw_range_error(
                    st,
                    name <> " must be a finite number",
                  )
              }
            }
          }
        })
      case any_defined {
        True -> #(fields, st)
        False -> rt_val.throw_range_error(st, "Invalid duration object")
      }
    }
    _ -> rt_val.throw_type_error(st, "Duration must be an object or string")
  }
}

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

// [+-]PnYnMnWnDTnHnMnS
fn parse_iso_duration(text: String) -> Result(DurationRecord, Nil) {
  let trimmed = string.trim(text)
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
    [#("Y", YearsUnit), #("M", MonthsUnit), #("W", WeeksUnit), #("D", DaysUnit)],
    allow_fraction: False,
  ))
  use time_fields <- result.try(case time_part {
    None -> Ok([])
    Some("") -> Error(Nil)
    Some(t) ->
      parse_duration_section(
        t,
        [#("H", HoursUnit), #("M", MinutesUnit), #("S", SecondsUnit)],
        allow_fraction: True,
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

fn parse_duration_section(
  part: String,
  designators: List(#(String, DurationUnit)),
  allow_fraction allow_fraction: Bool,
) -> Result(List(#(DurationUnit, Float)), Nil) {
  case part {
    "" -> Ok([])
    _ ->
      parse_duration_section_loop(
        string.to_graphemes(part),
        designators,
        allow_fraction,
        "",
        [],
      )
  }
}

fn parse_duration_section_loop(
  gs: List(String),
  designators: List(#(String, DurationUnit)),
  allow_fraction allow_fraction: Bool,
  num_acc num_acc: String,
  out out: List(#(DurationUnit, Float)),
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
          parse_duration_section_loop(
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
              parse_duration_section_loop(rest, remaining, allow_fraction, "", [
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
  // int.to_float would badarg on huge ints
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

fn build_duration_parts(
  df: DurationFormatState,
  fields: DurationRecord,
) -> List(intl_format.UnitPart) {
  let nu = df.numbering_system
  let base_style = df.style
  let frac_digits = df.fractional_digits
  let overall_negative = list.any(duration_values(fields), fn(v) { v <. 0.0 })
  let next_style_of = fn(unit) {
    case unit {
      SecondsUnit -> Some(df.milliseconds.style)
      MillisecondsUnit -> Some(df.microseconds.style)
      MicrosecondsUnit -> Some(df.nanoseconds.style)
      _other -> None
    }
  }
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
                intl_format.Precision(
                  min: option.unwrap(frac_digits, 0),
                  max: option.unwrap(frac_digits, 9),
                ),
                True,
              )
            }
            False -> #(
              FloatValue(raw_value),
              raw_value == 0.0,
              False,
              intl_format.Precision(min: 0, max: 0),
              False,
            )
          }
          let display_required = case unit == MinutesUnit && need_sep {
            True ->
              df.seconds.display == DisplayAlways
              || duration_field(fields, SecondsUnit) != 0.0
              || duration_field(fields, MillisecondsUnit) != 0.0
              || duration_field(fields, MicrosecondsUnit) != 0.0
              || duration_field(fields, NanosecondsUnit) != 0.0
            False -> False
          }
          let show = !is_zero || display == DisplayAlways || display_required
          case show {
            False -> #(groups, need_sep, display_neg, this_done)
            True -> {
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
                intl_format.NumberFormatOptions(
                  ..intl_format.default_number_format_options(),
                  sign_display:,
                  min_int: case style {
                    UnitStyleTwoDigit -> 2
                    UnitStyleLong
                    | UnitStyleShort
                    | UnitStyleNarrow
                    | UnitStyleNumeric
                    | UnitStyleFractional -> 1
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
                    False ->
                      StyleUnit(
                        unit: duration_unit_singular(unit),
                        display: unit_display_from_duration_style(style),
                      )
                  },
                )
              let parts = case value_repr {
                FloatValue(f) -> intl_format.format_number_parts(opts, f)
                DecimalValue(text) ->
                  intl_format.format_decimal_string_parts(opts, text)
              }
              let unit_tag = duration_unit_singular(unit)
              let parts =
                intl_format.apply_numbering_system(
                  parts,
                  nu,
                  intl_format.is_number_digit,
                )
                |> list.map(fn(part: intl_format.Part) {
                  case part.type_ {
                    PartLiteral ->
                      intl_format.UnitPart(part.type_, part.value, None)
                    _ ->
                      intl_format.UnitPart(
                        part.type_,
                        part.value,
                        Some(unit_tag),
                      )
                  }
                })
              case need_sep {
                True ->
                  case groups {
                    [last, ..earlier] -> #(
                      [
                        list.flatten([
                          last,
                          [intl_format.UnitPart(PartLiteral, ":", None)],
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
  let strings = list.map(groups, intl_format.unit_parts_to_string)
  let lf_parts =
    intl_format.list_format_parts(
      UnitList,
      duration_list_style(base_style),
      strings,
    )
  expand_list_elements(lf_parts, groups, [])
}

type DurationValue {
  FloatValue(Float)
  DecimalValue(String)
}

fn folds_into_fraction(style: DurationUnitStyle) -> Bool {
  case style {
    UnitStyleNumeric | UnitStyleFractional -> True
    UnitStyleLong | UnitStyleShort | UnitStyleNarrow | UnitStyleTwoDigit ->
      False
  }
}

fn unit_display_from_duration_style(style: DurationUnitStyle) -> UnitDisplay {
  case style {
    UnitStyleLong -> UnitLong
    UnitStyleNarrow -> UnitNarrow
    UnitStyleShort
    | UnitStyleNumeric
    | UnitStyleTwoDigit
    | UnitStyleFractional -> UnitShort
  }
}

fn duration_fractional_value(
  fields: DurationRecord,
  unit: DurationUnit,
) -> #(DurationValue, Bool) {
  let get = fn(u) { duration_field(fields, u) |> float.truncate }
  let #(exponent, components) = case unit {
    SecondsUnit -> #(9, [
      #(get(SecondsUnit), 1_000_000_000),
      #(get(MillisecondsUnit), 1_000_000),
      #(get(MicrosecondsUnit), 1000),
      #(get(NanosecondsUnit), 1),
    ])
    MillisecondsUnit -> #(6, [
      #(get(MillisecondsUnit), 1_000_000),
      #(get(MicrosecondsUnit), 1000),
      #(get(NanosecondsUnit), 1),
    ])
    _other -> #(3, [#(get(MicrosecondsUnit), 1000), #(get(NanosecondsUnit), 1)])
  }
  let total = list.fold(components, 0, fn(acc, c) { acc + c.0 * c.1 })
  let e = pow10(exponent)
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
      let r_text = string.pad_start(int.to_string(r), exponent, "0")
      #(
        DecimalValue(
          sign <> int.to_string(int.absolute_value(q)) <> "." <> r_text,
        ),
        zero,
      )
    }
  }
}

fn expand_list_elements(
  lf_parts: List(intl_format.Part),
  groups: List(List(intl_format.UnitPart)),
  acc: List(List(intl_format.UnitPart)),
) -> List(intl_format.UnitPart) {
  case lf_parts {
    [] -> list.flatten(list.reverse(acc))
    [intl_format.Part(PartElement, _), ..rest] ->
      case groups {
        [g, ..gs] -> expand_list_elements(rest, gs, [g, ..acc])
        [] -> expand_list_elements(rest, [], acc)
      }
    [intl_format.Part(t, v), ..rest] ->
      expand_list_elements(rest, groups, [
        [intl_format.UnitPart(t, v, None)],
        ..acc
      ])
  }
}

fn segmenter_segment(
  st: Agent,
  segments_proto: Handle,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let name = "Intl.Segmenter.prototype.segment"
  let #(_h, sg, _bound) = branded_segmenter(st, this, name)
  let #(s, st) = rt_val.to_string(st, first_arg_or_undefined(args))
  let data = SegmentsData(SegmentsState(string: s, granularity: sg.granularity))
  let #(h, st) =
    realm_ops.alloc_object(st, IntlObj(data:, bound: None), segments_proto)
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
      remaining: intl_segment.segment_string(sg.string, sg.granularity),
    ))
  let #(h, st) =
    realm_ops.alloc_object(st, IntlObj(data:, bound: None), iter_proto)
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
    #("index", mk_int(seg.index)),
    #("input", mk_string(input)),
  ]
  let props = case granularity {
    WordGranularity ->
      list.append(base, [#("isWordLike", mk_bool(seg.word_like))])
    GraphemeGranularity | SentenceGranularity -> base
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
  let #(n, st) = rt_val.to_number(st, index_v)
  let segments = intl_segment.segment_string(input, granularity)
  let total = intl_segment.utf16_len(input)
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
    [] -> realm_ops.alloc_iter_result(st, mk_undefined(), done: True)
    [seg, ..rest] -> {
      let st =
        write_intl_data(
          st,
          h,
          SegmentIteratorData(SegmentIteratorState(..it, remaining: rest)),
        )
      let #(data, st) = make_segment_data(st, it.string, it.granularity, seg)
      realm_ops.alloc_iter_result(st, data, done: False)
    }
  }
}

fn locale_lid(l: LocaleState) -> Option(intl_locale.LocaleId) {
  case intl_locale.parse(l.locale) {
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
        Some(l) -> mk_string(intl_locale.base_name(l))
        None -> mk_undefined()
      }
    LocaleLanguage ->
      case lid {
        Some(l) -> mk_string(string.lowercase(l.language))
        None -> mk_undefined()
      }
    LocaleScript ->
      case lid {
        Some(intl_locale.LocaleId(script: Some(s), ..)) ->
          mk_string(intl_locale.titlecase(s))
        _ -> mk_undefined()
      }
    LocaleRegion ->
      case lid {
        Some(intl_locale.LocaleId(region: Some(r), ..)) ->
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
        Some(intl_locale.LocaleId(variants: [_, ..] as vs, ..)) ->
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
      let new_tag = case intl_locale.parse(tag) {
        Ok(lid) ->
          case method {
            LocaleMaximize -> intl_locale.to_string(intl_locale.maximize(lid))
            _ -> intl_locale.to_string(intl_locale.minimize(lid))
          }
        Error(Nil) -> tag
      }
      let data = LocaleData(LocaleState(locale: new_tag))
      let #(h, st) =
        realm_ops.alloc_object(st, IntlObj(data:, bound: None), proto)
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
        Some(intl_locale.LocaleId(region: Some(r), ..)) ->
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
      let lang = intl_locale.language_of(l.locale)
      let dir = case list.contains(["ar", "he", "fa", "ur", "ps", "yi"], lang) {
        True -> "rtl"
        False -> "ltr"
      }
      alloc_pojo(st, [#("direction", mk_string(dir))])
    }
    LocaleGetWeekInfo -> {
      let #(weekend, st) = alloc_array(st, [mk_int(6), mk_int(7)])
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
        #("firstDay", mk_int(first_day)),
        #("weekend", weekend),
      ])
    }
  }
}
