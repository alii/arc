//// The Intl namespace (ECMA-402): Intl.getCanonicalLocales,
//// Intl.supportedValuesOf, and the service constructors
//// (Locale, Collator, NumberFormat, DateTimeFormat, PluralRules, ListFormat,
//// RelativeTimeFormat, Segmenter, DisplayNames, DurationFormat).
////
//// Locale data is root/English: formatters implement en/en-US CLDR patterns
//// (see intl_format.gleam); tag parsing/canonicalization is in
//// intl_locale.gleam.

import arc/vm/builtins/common
import arc/vm/builtins/date
import arc/vm/builtins/helpers.{first_arg_or_undefined}
import arc/vm/builtins/intl_format as fmt
import arc/vm/builtins/intl_locale as tags
import arc/vm/heap
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type CollatorState, type DateTimeFormatState, type DisplayNamesState,
  type DtfComponent, type DtfComponents, type DurationFormatState,
  type DurationUnitOptions, type IntlData, type IntlDigitOptions,
  type IntlNativeFn, type IntlService, type JsValue, type ListFormatState,
  type LocaleState, type NumberFormatState, type PluralRulesState, type Ref,
  type RelativeTimeFormatState, type SegmentIteratorState, type SegmenterState,
  type SegmentsState, CollatorData, CollatorState, DateTimeFormatData,
  DateTimeFormatState, Dispatch, DisplayNamesData, DisplayNamesState,
  DtfComponents, DtfDay, DtfDayPeriod, DtfEra, DtfFractionalSecondDigits,
  DtfHour, DtfMinute, DtfMonth, DtfSecond, DtfTimeZoneName, DtfWeekday, DtfYear,
  DurationFormatData, DurationFormatState, DurationUnitOptions, GroupingMode,
  GroupingNever, IntlBoundGetter, IntlBoundMethod, IntlCollator, IntlConstructor,
  IntlDateTimeFormat, IntlDigitOptions, IntlDisplayNames, IntlDurationFormat,
  IntlGetCanonicalLocales, IntlListFormat, IntlLocale, IntlLocaleGetter,
  IntlLocaleMethod, IntlMethod, IntlNative, IntlNumberFormat, IntlObject,
  IntlPluralRules, IntlRelativeTimeFormat, IntlResolvedOptions,
  IntlSegmentIterator, IntlSegmenter, IntlSegmenterSegment, IntlSegments,
  IntlSegmentsIterator, IntlSupportedLocalesOf, IntlSupportedValuesOf, JsBool,
  JsNumber, JsObject, JsString, JsUndefined, ListFormatData, ListFormatState,
  LocaleData, LocaleState, Named, NumberFormatData, NumberFormatState,
  ObjectSlot, PluralRulesData, PluralRulesState, RelativeTimeFormatData,
  RelativeTimeFormatState, SegmentIteratorData, SegmentIteratorState,
  SegmenterData, SegmenterState, SegmentsData, SegmentsState, TemporalDateSlot,
  TemporalDateTimeSlot, TemporalInstantSlot, TemporalMonthDaySlot,
  TemporalTimeSlot, TemporalYearMonthSlot, TemporalZonedDateTimeSlot,
}
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
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
          "baseName", "calendar", "caseFirst", "collation", "firstDayOfWeek",
          "hourCycle", "numeric", "numberingSystem", "language", "script",
          "region", "variants",
        ],
        fn(name) { #(name, IntlNative(IntlLocaleGetter(name))) },
      ),
    )
  let #(h, locale) =
    common.init_type(
      h,
      object_proto,
      function_proto,
      locale_getters,
      fn(proto) { Dispatch(IntlNative(IntlConstructor(IntlLocale, proto))) },
      "Locale",
      1,
      [],
    )
  let h = common.add_to_string_tag(h, locale.prototype, "Intl.Locale")
  let h = freeze_prototype_prop(h, locale.constructor, locale.prototype)
  // Locale methods need the prototype ref for maximize/minimize results.
  let #(h, locale_methods) =
    common.alloc_methods(h, function_proto, [
      #(
        "toString",
        IntlNative(IntlLocaleMethod("toString", locale.prototype)),
        0,
      ),
      #(
        "maximize",
        IntlNative(IntlLocaleMethod("maximize", locale.prototype)),
        0,
      ),
      #(
        "minimize",
        IntlNative(IntlLocaleMethod("minimize", locale.prototype)),
        0,
      ),
      #(
        "getCalendars",
        IntlNative(IntlLocaleMethod("getCalendars", locale.prototype)),
        0,
      ),
      #(
        "getCollations",
        IntlNative(IntlLocaleMethod("getCollations", locale.prototype)),
        0,
      ),
      #(
        "getHourCycles",
        IntlNative(IntlLocaleMethod("getHourCycles", locale.prototype)),
        0,
      ),
      #(
        "getNumberingSystems",
        IntlNative(IntlLocaleMethod("getNumberingSystems", locale.prototype)),
        0,
      ),
      #(
        "getTimeZones",
        IntlNative(IntlLocaleMethod("getTimeZones", locale.prototype)),
        0,
      ),
      #(
        "getTextInfo",
        IntlNative(IntlLocaleMethod("getTextInfo", locale.prototype)),
        0,
      ),
      #(
        "getWeekInfo",
        IntlNative(IntlLocaleMethod("getWeekInfo", locale.prototype)),
        0,
      ),
    ])
  let h = add_named_properties(h, locale.prototype, locale_methods)

  // --- Simple formatter services ---
  let #(h, collator) =
    init_service(h, object_proto, function_proto, IntlCollator, "Collator", [], [
      #("compare", IntlNative(IntlBoundGetter(IntlCollator))),
    ])
  let #(h, number_format) =
    init_service(
      h,
      object_proto,
      function_proto,
      IntlNumberFormat,
      "NumberFormat",
      [
        #(
          "formatToParts",
          IntlNative(IntlMethod(IntlNumberFormat, "formatToParts")),
          1,
        ),
        #(
          "formatRange",
          IntlNative(IntlMethod(IntlNumberFormat, "formatRange")),
          2,
        ),
        #(
          "formatRangeToParts",
          IntlNative(IntlMethod(IntlNumberFormat, "formatRangeToParts")),
          2,
        ),
      ],
      [#("format", IntlNative(IntlBoundGetter(IntlNumberFormat)))],
    )
  let #(h, date_time_format) =
    init_service(
      h,
      object_proto,
      function_proto,
      IntlDateTimeFormat,
      "DateTimeFormat",
      [
        #(
          "formatToParts",
          IntlNative(IntlMethod(IntlDateTimeFormat, "formatToParts")),
          1,
        ),
        #(
          "formatRange",
          IntlNative(IntlMethod(IntlDateTimeFormat, "formatRange")),
          2,
        ),
        #(
          "formatRangeToParts",
          IntlNative(IntlMethod(IntlDateTimeFormat, "formatRangeToParts")),
          2,
        ),
      ],
      [#("format", IntlNative(IntlBoundGetter(IntlDateTimeFormat)))],
    )
  let #(h, plural_rules) =
    init_service(
      h,
      object_proto,
      function_proto,
      IntlPluralRules,
      "PluralRules",
      [
        #("select", IntlNative(IntlMethod(IntlPluralRules, "select")), 1),
        #(
          "selectRange",
          IntlNative(IntlMethod(IntlPluralRules, "selectRange")),
          2,
        ),
      ],
      [],
    )
  let #(h, list_format) =
    init_service(
      h,
      object_proto,
      function_proto,
      IntlListFormat,
      "ListFormat",
      [
        #("format", IntlNative(IntlMethod(IntlListFormat, "format")), 1),
        #(
          "formatToParts",
          IntlNative(IntlMethod(IntlListFormat, "formatToParts")),
          1,
        ),
      ],
      [],
    )
  let #(h, relative_time_format) =
    init_service(
      h,
      object_proto,
      function_proto,
      IntlRelativeTimeFormat,
      "RelativeTimeFormat",
      [
        #("format", IntlNative(IntlMethod(IntlRelativeTimeFormat, "format")), 2),
        #(
          "formatToParts",
          IntlNative(IntlMethod(IntlRelativeTimeFormat, "formatToParts")),
          2,
        ),
      ],
      [],
    )
  let #(h, display_names) =
    init_service(
      h,
      object_proto,
      function_proto,
      IntlDisplayNames,
      "DisplayNames",
      [#("of", IntlNative(IntlMethod(IntlDisplayNames, "of")), 1)],
      [],
    )
  let #(h, duration_format) =
    init_service(
      h,
      object_proto,
      function_proto,
      IntlDurationFormat,
      "DurationFormat",
      [
        #("format", IntlNative(IntlMethod(IntlDurationFormat, "format")), 1),
        #(
          "formatToParts",
          IntlNative(IntlMethod(IntlDurationFormat, "formatToParts")),
          1,
        ),
      ],
      [],
    )

  // --- Segmenter (needs %SegmentIteratorPrototype% / %SegmentsPrototype%) ---
  let #(h, seg_iter_next) =
    common.alloc_methods(h, function_proto, [
      #("next", IntlNative(IntlMethod(IntlSegmentIterator, "next")), 0),
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
      #("containing", IntlNative(IntlMethod(IntlSegments, "containing")), 1),
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
      IntlSegmenter,
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

  // ECMA-402 §17-19: locale-sensitive overrides on Number/String/Date.
  let #(h, number_methods) =
    common.alloc_methods(h, function_proto, [
      #(
        "toLocaleString",
        IntlNative(IntlMethod(IntlNumberFormat, "Number#toLocaleString")),
        0,
      ),
    ])
  let h = add_named_properties(h, number_proto, number_methods)
  let #(h, string_methods) =
    common.alloc_methods(h, function_proto, [
      #(
        "localeCompare",
        IntlNative(IntlMethod(IntlCollator, "String#localeCompare")),
        1,
      ),
      #(
        "toLocaleLowerCase",
        IntlNative(IntlMethod(IntlCollator, "String#toLocaleLowerCase")),
        0,
      ),
      #(
        "toLocaleUpperCase",
        IntlNative(IntlMethod(IntlCollator, "String#toLocaleUpperCase")),
        0,
      ),
    ])
  let h = add_named_properties(h, string_proto, string_methods)
  let #(h, date_methods) =
    common.alloc_methods(h, function_proto, [
      #(
        "toLocaleString",
        IntlNative(IntlMethod(IntlDateTimeFormat, "Date#toLocaleString")),
        0,
      ),
      #(
        "toLocaleDateString",
        IntlNative(IntlMethod(IntlDateTimeFormat, "Date#toLocaleDateString")),
        0,
      ),
      #(
        "toLocaleTimeString",
        IntlNative(IntlMethod(IntlDateTimeFormat, "Date#toLocaleTimeString")),
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
  service: IntlService,
  name: String,
  methods: List(#(String, value.NativeFn, Int)),
  accessors: List(#(String, value.NativeFn)),
) -> #(Heap(host), common.BuiltinType) {
  let arity = case service {
    IntlDisplayNames -> 2
    _ -> 0
  }
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("resolvedOptions", IntlNative(IntlResolvedOptions(service)), 0),
      ..methods
    ])
  let #(h, proto_accessors) = common.alloc_getters(h, function_proto, accessors)
  let #(h, slo) =
    common.alloc_methods(h, function_proto, [
      #("supportedLocalesOf", IntlNative(IntlSupportedLocalesOf(service)), 1),
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
  let h = freeze_prototype_prop(h, bt.constructor, bt.prototype)
  #(h, bt)
}

/// Intl constructors have non-writable, non-configurable .prototype.
fn freeze_prototype_prop(h: Heap(host), ctor: Ref, proto: Ref) -> Heap(host) {
  heap.update(h, ctor, fn(slot) {
    case slot {
      ObjectSlot(properties:, ..) ->
        ObjectSlot(
          ..slot,
          properties: dict.insert(
            properties,
            Named("prototype"),
            value.data(JsObject(proto)),
          ),
        )
      other -> other
    }
  })
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

fn throw_range(state: State(host), msg: String) -> Result(a, Thrown(host)) {
  let #(state, res) = state.range_error(state, msg)
  case res {
    Error(e) -> Error(#(e, state))
    Ok(v) -> Error(#(v, state))
  }
}

fn throw_type(state: State(host), msg: String) -> Result(a, Thrown(host)) {
  let #(state, res) = state.type_error(state, msg)
  case res {
    Error(e) -> Error(#(e, state))
    Ok(v) -> Error(#(v, state))
  }
}

/// Read the IntlObject state for `this`, throwing TypeError on brand mismatch.
fn branded(
  state: State(host),
  this: JsValue,
  service: IntlService,
  method: String,
) -> Result(#(Ref, IntlData), Thrown(host)) {
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: IntlObject(service: s, data:), ..))
          if s == service
        -> Ok(#(ref, data))
        _ -> throw_type(state, method <> " called on incompatible receiver")
      }
    _ -> throw_type(state, method <> " called on incompatible receiver")
  }
}

/// `branded` narrowed to Intl.Locale instances.
fn branded_locale(
  state: State(host),
  this: JsValue,
  method: String,
) -> Result(#(Ref, LocaleState), Thrown(host)) {
  use #(ref, data) <- result.try(branded(state, this, IntlLocale, method))
  case data {
    LocaleData(l) -> Ok(#(ref, l))
    _ -> throw_type(state, method <> " called on incompatible receiver")
  }
}

/// Replace an Intl instance's per-service state in the heap, keeping every
/// other object attribute. The service tag is re-derived from the new data
/// so the two can never disagree.
fn write_intl_data(h: Heap(host), ref: Ref, data: IntlData) -> Heap(host) {
  heap.update(h, ref, fn(slot) {
    case slot {
      ObjectSlot(kind: IntlObject(..), ..) ->
        ObjectSlot(
          ..slot,
          kind: IntlObject(service: value.intl_service(data), data:),
        )
      other -> other
    }
  })
}

/// Look up a DateTimeFormat formatting component.
fn component(c: DtfComponents, which: DtfComponent) -> Option(String) {
  case which {
    DtfWeekday -> c.weekday
    DtfEra -> c.era
    DtfYear -> c.year
    DtfMonth -> c.month
    DtfDay -> c.day
    DtfDayPeriod -> c.day_period
    DtfHour -> c.hour
    DtfMinute -> c.minute
    DtfSecond -> c.second
    DtfFractionalSecondDigits -> c.fractional_second_digits
    DtfTimeZoneName -> c.time_zone_name
  }
}

/// Set a DateTimeFormat formatting component's width.
fn set_component(
  c: DtfComponents,
  which: DtfComponent,
  v: String,
) -> DtfComponents {
  case which {
    DtfWeekday -> DtfComponents(..c, weekday: Some(v))
    DtfEra -> DtfComponents(..c, era: Some(v))
    DtfYear -> DtfComponents(..c, year: Some(v))
    DtfMonth -> DtfComponents(..c, month: Some(v))
    DtfDay -> DtfComponents(..c, day: Some(v))
    DtfDayPeriod -> DtfComponents(..c, day_period: Some(v))
    DtfHour -> DtfComponents(..c, hour: Some(v))
    DtfMinute -> DtfComponents(..c, minute: Some(v))
    DtfSecond -> DtfComponents(..c, second: Some(v))
    DtfFractionalSecondDigits ->
      DtfComponents(..c, fractional_second_digits: Some(v))
    DtfTimeZoneName -> DtfComponents(..c, time_zone_name: Some(v))
  }
}

/// Build a component table from #(component, width) pairs; later pairs win.
fn components_from_pairs(
  pairs: List(#(DtfComponent, String)),
) -> DtfComponents {
  list.fold(pairs, value.empty_dtf_components, fn(c, kv) {
    set_component(c, kv.0, kv.1)
  })
}

/// Overlay a formatter's resolved digit options onto a `fmt.NumOpts` base.
fn with_digits(o: fmt.NumOpts, dg: IntlDigitOptions) -> fmt.NumOpts {
  fmt.NumOpts(
    ..o,
    min_int: dg.minimum_integer_digits,
    min_frac: dg.minimum_fraction_digits,
    max_frac: dg.maximum_fraction_digits,
    min_sig: dg.minimum_significant_digits,
    max_sig: dg.maximum_significant_digits,
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
        alloc_pojo(state, [#("type", JsString(t)), #("value", JsString(v))])
      #(state, [obj, ..objs])
    })
  alloc_array(state, list.reverse(objs))
}

/// Parts → JS array of `{ type, value, source }` objects (formatRangeToParts).
fn parts_to_js_sourced(
  state: State(host),
  parts: List(#(String, String, String)),
) -> #(State(host), JsValue) {
  let #(state, objs) =
    list.fold(parts, #(state, []), fn(acc, part) {
      let #(state, objs) = acc
      let #(t, v, src) = part
      let #(state, obj) =
        alloc_pojo(state, [
          #("type", JsString(t)),
          #("value", JsString(v)),
          #("source", JsString(src)),
        ])
      #(state, [obj, ..objs])
    })
  alloc_array(state, list.reverse(objs))
}

/// Parts → JS array of `{ type, value, unit? }` objects (RelativeTimeFormat /
/// DurationFormat formatToParts). Empty unit string means no unit property.
fn parts_to_js_with_unit(
  state: State(host),
  parts: List(#(String, String, String)),
) -> #(State(host), JsValue) {
  let #(state, objs) =
    list.fold(parts, #(state, []), fn(acc, part) {
      let #(state, objs) = acc
      let #(t, v, unit) = part
      let props = case unit {
        "" -> [#("type", JsString(t)), #("value", JsString(v))]
        _ -> [
          #("type", JsString(t)),
          #("value", JsString(v)),
          #("unit", JsString(unit)),
        ]
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

fn is_alnum(s: String) -> Bool {
  s != ""
  && string.to_utf_codepoints(s)
  |> list.all(fn(cp) {
    let c = string.utf_codepoint_to_int(cp)
    { c >= 0x30 && c <= 0x39 }
    || { c >= 0x41 && c <= 0x5a }
    || { c >= 0x61 && c <= 0x7a }
  })
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
        Some(ObjectSlot(kind: IntlObject(data: LocaleData(l), ..), ..)) ->
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
      let key = value.Index(k)
      case object.has_property(state.heap, ref, key) {
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
                Some(ObjectSlot(kind: IntlObject(data: LocaleData(l), ..), ..)) ->
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
      "numberingSystem" -> Some(numbering_systems())
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

/// Numbering systems we can render digits for (sorted).
fn numbering_systems() -> List(String) {
  [
    "adlm", "ahom", "arab", "arabext", "bali", "beng", "bhks", "brah", "cakm",
    "cham", "deva", "diak", "fullwide", "gara", "gong", "gonm", "gujr", "gukh",
    "guru", "hanidec", "hmng", "hmnp", "java", "kali", "kawi", "khmr", "knda",
    "krai", "lana", "lanatham", "laoo", "latn", "lepc", "limb", "mathbold",
    "mathdbl", "mathmono", "mathsanb", "mathsans", "mlym", "modi", "mong",
    "mroo", "mtei", "mymr", "mymrepka", "mymrpao", "mymrshan", "mymrtlng",
    "nagm", "newa", "nkoo", "olck", "onao", "orya", "osma", "outlined", "rohg",
    "saur", "segment", "shrd", "sind", "sinh", "sora", "sund", "sunu", "takr",
    "talu", "tamldec", "telu", "thai", "tibt", "tirh", "tnsa", "tols", "vaii",
    "wara", "wcho",
  ]
}

fn is_numbering_system(s: String) -> Bool {
  list.contains(numbering_systems(), s)
}

/// Transliterate latn digits in numeric parts to the numbering system.
fn apply_numbering_system(parts: List(fmt.Part), nu: String) -> List(fmt.Part) {
  case nu {
    "latn" -> parts
    _ ->
      list.map(parts, fn(part) {
        let #(t, v) = part
        case t {
          "integer" | "fraction" | "exponentInteger" -> #(
            t,
            translit_digits(v, nu),
          )
          _ -> part
        }
      })
  }
}

fn translit_digits(s: String, nu: String) -> String {
  case nu {
    "hanidec" ->
      string.to_graphemes(s)
      |> list.map(fn(c) {
        case c {
          "0" -> "〇"
          "1" -> "一"
          "2" -> "二"
          "3" -> "三"
          "4" -> "四"
          "5" -> "五"
          "6" -> "六"
          "7" -> "七"
          "8" -> "八"
          "9" -> "九"
          _ -> c
        }
      })
      |> string.join("")
    _ ->
      case numbering_base(nu) {
        None -> s
        Some(base) ->
          string.to_graphemes(s)
          |> list.map(fn(c) {
            case int.parse(c) {
              Ok(d) ->
                case string.utf_codepoint(base + d) {
                  Ok(cp) -> string.from_utf_codepoints([cp])
                  Error(Nil) -> c
                }
              Error(Nil) -> c
            }
          })
          |> string.join("")
      }
  }
}

fn numbering_base(nu: String) -> Option(Int) {
  case nu {
    "adlm" -> Some(0x1e950)
    "ahom" -> Some(0x11730)
    "bhks" -> Some(0x11c50)
    "brah" -> Some(0x11066)
    "cakm" -> Some(0x11136)
    "cham" -> Some(0xaa50)
    "diak" -> Some(0x11950)
    "gara" -> Some(0x10d40)
    "gukh" -> Some(0x16130)
    "krai" -> Some(0x16d70)
    "onao" -> Some(0x1e5f1)
    "tols" -> Some(0x11de0)
    "sunu" -> Some(0x11bf0)
    "mymrepka" -> Some(0x116da)
    "mymrpao" -> Some(0x116d0)
    "outlined" -> Some(0x1ccf0)
    "gong" -> Some(0x11da0)
    "gonm" -> Some(0x11d50)
    "hmng" -> Some(0x16b50)
    "hmnp" -> Some(0x1e140)
    "java" -> Some(0xa9d0)
    "kali" -> Some(0xa900)
    "kawi" -> Some(0x11f50)
    "lana" -> Some(0x1a80)
    "lanatham" -> Some(0x1a90)
    "lepc" -> Some(0x1c40)
    "mathbold" -> Some(0x1d7ce)
    "mathdbl" -> Some(0x1d7d8)
    "mathmono" -> Some(0x1d7f6)
    "mathsanb" -> Some(0x1d7ec)
    "mathsans" -> Some(0x1d7e2)
    "modi" -> Some(0x11650)
    "mroo" -> Some(0x16a60)
    "mtei" -> Some(0xabf0)
    "mymrshan" -> Some(0x1090)
    "mymrtlng" -> Some(0xa9f0)
    "nagm" -> Some(0x1e4f0)
    "newa" -> Some(0x11450)
    "nkoo" -> Some(0x07c0)
    "olck" -> Some(0x1c50)
    "osma" -> Some(0x104a0)
    "rohg" -> Some(0x10d30)
    "saur" -> Some(0xa8d0)
    "segment" -> Some(0x1fbf0)
    "shrd" -> Some(0x111d0)
    "sind" -> Some(0x112f0)
    "sinh" -> Some(0x0de6)
    "sora" -> Some(0x110f0)
    "sund" -> Some(0x1bb0)
    "takr" -> Some(0x116c0)
    "talu" -> Some(0x19d0)
    "tirh" -> Some(0x114d0)
    "tnsa" -> Some(0x16ac0)
    "vaii" -> Some(0xa620)
    "wara" -> Some(0x118e0)
    "wcho" -> Some(0x1e2f0)
    "arab" -> Some(0x0660)
    "arabext" -> Some(0x06f0)
    "bali" -> Some(0x1b50)
    "beng" -> Some(0x09e6)
    "deva" -> Some(0x0966)
    "fullwide" -> Some(0xff10)
    "gujr" -> Some(0x0ae6)
    "guru" -> Some(0x0a66)
    "khmr" -> Some(0x17e0)
    "knda" -> Some(0x0ce6)
    "laoo" -> Some(0x0ed0)
    "limb" -> Some(0x1946)
    "mlym" -> Some(0x0d66)
    "mong" -> Some(0x1810)
    "mymr" -> Some(0x1040)
    "orya" -> Some(0x0b66)
    "tamldec" -> Some(0x0be6)
    "telu" -> Some(0x0c66)
    "thai" -> Some(0x0e50)
    "tibt" -> Some(0x0f20)
    _ -> None
  }
}

// ============================================================================
// supportedLocalesOf (§9.2.8 LookupSupportedLocales)
// ============================================================================

fn supported_locales_of(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let locales = first_arg_or_undefined(args)
  let options_v = helpers.list_at(args, 1) |> option.unwrap(JsUndefined)
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

/// Build the resolved [[Locale]] string: data locale + supported u-keywords.
fn build_resolved_locale(
  data_locale: String,
  keywords: List(#(String, String)),
) -> String {
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
  service: IntlService,
  proto: Ref,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let callable_without_new = case service {
    IntlCollator | IntlNumberFormat | IntlDateTimeFormat -> True
    _ -> False
  }
  case !callable_without_new && state.new_target == JsUndefined {
    True ->
      state.type_error(
        state,
        "Constructor Intl." <> service_name(service) <> " requires 'new'",
      )
    False -> {
      let arg0 = first_arg_or_undefined(args)
      let arg1 = helpers.list_at(args, 1) |> option.unwrap(JsUndefined)
      run({
        use #(data, state) <- result.try(case service {
          IntlLocale -> {
            use #(s, state) <- result.map(locale_state(state, arg0, arg1))
            #(LocaleData(s), state)
          }
          IntlCollator -> {
            use #(s, state) <- result.map(collator_state(state, arg0, arg1))
            #(CollatorData(s), state)
          }
          IntlNumberFormat -> {
            use #(s, state) <- result.map(number_format_state(state, arg0, arg1))
            #(NumberFormatData(s), state)
          }
          IntlDateTimeFormat -> {
            use #(s, state) <- result.map(date_time_format_state(
              state,
              arg0,
              arg1,
            ))
            #(DateTimeFormatData(s), state)
          }
          IntlPluralRules -> {
            use #(s, state) <- result.map(plural_rules_state(state, arg0, arg1))
            #(PluralRulesData(s), state)
          }
          IntlListFormat -> {
            use #(s, state) <- result.map(list_format_state(state, arg0, arg1))
            #(ListFormatData(s), state)
          }
          IntlRelativeTimeFormat -> {
            use #(s, state) <- result.map(rtf_state(state, arg0, arg1))
            #(RelativeTimeFormatData(s), state)
          }
          IntlSegmenter -> {
            use #(s, state) <- result.map(segmenter_state(state, arg0, arg1))
            #(SegmenterData(s), state)
          }
          IntlDisplayNames -> {
            use #(s, state) <- result.map(display_names_state(state, arg0, arg1))
            #(DisplayNamesData(s), state)
          }
          IntlDurationFormat -> {
            use #(s, state) <- result.map(duration_format_state(
              state,
              arg0,
              arg1,
            ))
            #(DurationFormatData(s), state)
          }
          IntlSegments | IntlSegmentIterator ->
            throw_type(state, "Illegal constructor")
        })
        let #(heap, ref) =
          common.alloc_wrapper(
            state.heap,
            IntlObject(service: value.intl_service(data), data:),
            proto,
          )
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
        Some(ObjectSlot(kind: IntlObject(data: LocaleData(l), ..), ..)) ->
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
  s != ""
  && string.to_utf_codepoints(s)
  |> list.all(fn(cp) {
    let c = string.utf_codepoint_to_int(cp)
    { c >= 0x41 && c <= 0x5a } || { c >= 0x61 && c <= 0x7a }
  })
}

fn is_digit_str(s: String) -> Bool {
  s != ""
  && string.to_utf_codepoints(s)
  |> list.all(fn(cp) {
    let c = string.utf_codepoint_to_int(cp)
    c >= 0x30 && c <= 0x39
  })
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

/// Extension keyword or option value for a relevant key, with validation.
/// Returns the resolved value plus whether the (valid) value came from the
/// locale's u-extension (those flow into the resolved locale string).
fn resolve_keyword(
  ext_kws: List(#(String, String)),
  key: String,
  option_value: Option(String),
  valid: fn(String) -> Bool,
  default: String,
) -> #(String, Bool) {
  // Bare keywords ("-u-kn") mean "true" (UTS 35).
  let ext = case list.key_find(ext_kws, key) {
    Ok("") -> Some("true")
    Ok(v) -> Some(v)
    Error(Nil) -> None
  }
  let from_ext = case ext {
    Some(v) ->
      case valid(v) {
        True -> Some(v)
        False -> None
      }
    None -> None
  }
  case option_value {
    Some(v) ->
      case valid(v) {
        // §9.2.7 ResolveLocale step 9.h.ii.2: an options value that matches
        // the requested extension keyword keeps the keyword in [[Locale]];
        // an unsupported options value leaves the extension value in place.
        True -> #(v, from_ext == Some(v))
        False ->
          case from_ext {
            Some(e) -> #(e, True)
            None -> #(default, False)
          }
      }
    None ->
      case from_ext {
        Some(v) -> #(v, True)
        None -> #(default, False)
      }
  }
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
    resolve_keyword(ext_kws, "nu", nu_opt, is_numbering_system, "latn")
  let locale =
    build_resolved_locale(data_locale, case nu_from_ext {
      True -> [#("nu", nu)]
      False -> []
    })
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
  use #(usage, state) <- result.try(get_str_opt(
    state,
    opts,
    "usage",
    ["sort", "search"],
    Some("sort"),
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
  use #(case_first_opt, state) <- result.try(get_str_opt(
    state,
    opts,
    "caseFirst",
    ["upper", "lower", "false"],
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
    resolve_keyword(
      ext_kws,
      "kf",
      case_first_opt,
      fn(v) { v == "upper" || v == "lower" || v == "false" },
      "false",
    )
  use #(sensitivity, state) <- result.try(get_str_opt(
    state,
    opts,
    "sensitivity",
    ["base", "accent", "case", "variant"],
    None,
  ))
  let sensitivity = option.unwrap(sensitivity, "variant")
  let ignore_punct_default = string.starts_with(data_locale, "th")
  use #(ignore_punct, state) <- result.try(get_bool_opt(
    state,
    opts,
    "ignorePunctuation",
    Some(ignore_punct_default),
  ))
  let ext_used =
    list.filter_map(
      [
        #("co", co_from_ext, collation),
        #("kn", kn_from_ext, case numeric {
          True -> "true"
          False -> "false"
        }),
        #("kf", kf_from_ext, case_first),
      ],
      fn(t) {
        case t {
          #(k, True, v) -> Ok(#(k, v))
          _ -> Error(Nil)
        }
      },
    )
  let locale = build_resolved_locale(data_locale, ext_used)
  Ok(#(
    CollatorState(
      locale:,
      usage: option.unwrap(usage, "sort"),
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
  use #(style, state) <- result.try(get_str_opt(
    state,
    opts,
    "style",
    ["decimal", "percent", "currency", "unit"],
    Some("decimal"),
  ))
  let style = option.unwrap(style, "decimal")
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
    None ->
      case style == "currency" {
        True ->
          throw_type(state, "Currency code is required with currency style")
        False -> Ok(Nil)
      }
  })
  use #(currency_display, state) <- result.try(get_str_opt(
    state,
    opts,
    "currencyDisplay",
    ["code", "symbol", "narrowSymbol", "name"],
    Some("symbol"),
  ))
  use #(currency_sign, state) <- result.try(get_str_opt(
    state,
    opts,
    "currencySign",
    ["standard", "accounting"],
    Some("standard"),
  ))
  use #(unit, state) <- result.try(get_str_opt(state, opts, "unit", [], None))
  use Nil <- result.try(case unit {
    Some(u) ->
      case fmt.is_well_formed_unit(u) {
        True -> Ok(Nil)
        False ->
          throw_range(state, "Invalid unit argument for option unit: " <> u)
      }
    None ->
      case style == "unit" {
        True -> throw_type(state, "Unit is required with unit style")
        False -> Ok(Nil)
      }
  })
  use #(unit_display, state) <- result.try(get_str_opt(
    state,
    opts,
    "unitDisplay",
    ["short", "narrow", "long"],
    Some("short"),
  ))
  let currency = option.map(currency, string.uppercase)
  let #(mnfd_default, mxfd_default) = case style {
    "currency" -> {
      let d = fmt.currency_digits(option.unwrap(currency, "USD"))
      #(d, d)
    }
    "percent" -> #(0, 0)
    _ -> #(0, 3)
  }
  use #(notation, state) <- result.try(get_str_opt(
    state,
    opts,
    "notation",
    ["standard", "scientific", "engineering", "compact"],
    Some("standard"),
  ))
  let notation = option.unwrap(notation, "standard")
  use #(digits, state) <- result.try(digit_options(
    state,
    opts,
    mnfd_default,
    mxfd_default,
    notation,
  ))
  use #(compact_display, state) <- result.try(get_str_opt(
    state,
    opts,
    "compactDisplay",
    ["short", "long"],
    Some("short"),
  ))
  // useGrouping: boolean or "min2"/"auto"/"always" (§15.1.6 GetBooleanOrStringNumberFormatOption)
  use #(grouping_v, state) <- result.try(opt_get(state, opts, "useGrouping"))
  use #(use_grouping, state) <- result.try(case grouping_v {
    JsUndefined ->
      Ok(#(
        GroupingMode(case notation {
          "compact" -> "min2"
          _ -> "auto"
        }),
        state,
      ))
    JsBool(False) -> Ok(#(GroupingNever, state))
    JsBool(True) -> Ok(#(GroupingMode("always"), state))
    _ -> {
      use #(s, state) <- result.try(coerce.js_to_string(state, grouping_v))
      case s {
        "min2" | "auto" | "always" -> Ok(#(GroupingMode(s), state))
        "true" | "false" ->
          // Legacy: any other truthy value behaves per ToBoolean? Spec:
          // strings must be in the allowed set.
          throw_range(
            state,
            "Value " <> s <> " out of range for options property useGrouping",
          )
        _ ->
          throw_range(
            state,
            "Value " <> s <> " out of range for options property useGrouping",
          )
      }
    }
  })
  use #(sign_display, state) <- result.try(get_str_opt(
    state,
    opts,
    "signDisplay",
    ["auto", "never", "always", "exceptZero", "negative"],
    Some("auto"),
  ))
  Ok(#(
    NumberFormatState(
      locale:,
      numbering_system: nu,
      style:,
      currency:,
      currency_display: case style {
        "currency" -> currency_display
        _ -> None
      },
      currency_sign: case style {
        "currency" -> currency_sign
        _ -> None
      },
      unit:,
      unit_display: case style {
        "unit" -> unit_display
        _ -> None
      },
      digits:,
      use_grouping:,
      notation:,
      compact_display: case notation {
        "compact" -> compact_display
        _ -> None
      },
      sign_display: option.unwrap(sign_display, "auto"),
      bound_format: None,
    ),
    state,
  ))
}

/// SetNumberFormatDigitOptions (§15.1.6) — resolves the digit-related slots.
fn digit_options(
  state: State(host),
  opts: Option(Ref),
  mnfd_default: Int,
  mxfd_default: Int,
  notation: String,
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
  use #(rounding_mode, state) <- result.try(get_str_opt(
    state,
    opts,
    "roundingMode",
    [
      "ceil", "floor", "expand", "trunc", "halfCeil", "halfFloor", "halfExpand",
      "halfTrunc", "halfEven",
    ],
    Some("halfExpand"),
  ))
  use #(rounding_priority, state) <- result.try(get_str_opt(
    state,
    opts,
    "roundingPriority",
    ["auto", "morePrecision", "lessPrecision"],
    Some("auto"),
  ))
  let rounding_priority = option.unwrap(rounding_priority, "auto")
  use #(trailing_zero, state) <- result.try(get_str_opt(
    state,
    opts,
    "trailingZeroDisplay",
    ["auto", "stripIfInteger"],
    Some("auto"),
  ))
  let has_sd = mnsd_v != JsUndefined || mxsd_v != JsUndefined
  let has_fd = mnfd_v != JsUndefined || mxfd_v != JsUndefined
  let need_sd = case rounding_priority {
    "auto" -> has_sd
    _ -> True
  }
  let need_fd = case rounding_priority {
    "auto" -> !{ has_sd || { !has_fd && notation == "compact" } }
    _ -> True
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
    None, None -> #(Some(#(1, 2)), Some(#(0, 0)), "morePrecision")
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
      minimum_fraction_digits: option.map(fd, fn(p) { p.0 }),
      maximum_fraction_digits: option.map(fd, fn(p) { p.1 }),
      minimum_significant_digits: option.map(sig, fn(p) { p.0 }),
      maximum_significant_digits: option.map(sig, fn(p) { p.1 }),
      rounding_increment:,
      rounding_mode: option.unwrap(rounding_mode, "halfExpand"),
      rounding_priority:,
      trailing_zero_display: option.unwrap(trailing_zero, "auto"),
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
  dtf_state_required(
    state,
    locales_v,
    options_v,
    [#(DtfYear, "numeric"), #(DtfMonth, "numeric"), #(DtfDay, "numeric")],
    "any",
  )
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

/// CreateDateTimeFormat (§11.1.2). `default_components` are the
/// #(component, width) locale defaults applied when the user requested no
/// component of the `required` group ("date" / "time" / "any").
fn dtf_state_required(
  state: State(host),
  locales_v: JsValue,
  options_v: JsValue,
  default_components: List(#(DtfComponent, String)),
  required: String,
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
  use #(hour_cycle_opt, state) <- result.try(get_str_opt(
    state,
    opts,
    "hourCycle",
    ["h11", "h12", "h23", "h24"],
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
    resolve_keyword(ext_kws, "nu", nu_opt, is_numbering_system, "latn")
  let lang = case tags.parse(data_locale) {
    Ok(lid) -> lid.language
    Error(Nil) -> "en"
  }
  let hc_locale_default = case lang {
    "ja" -> "h11"
    _ -> "h12"
  }
  let #(hc, hc_from_ext) =
    resolve_keyword(
      ext_kws,
      "hc",
      hour_cycle_opt,
      fn(v) { v == "h11" || v == "h12" || v == "h23" || v == "h24" },
      hc_locale_default,
    )
  // hour12 option resolution (hourCycle12 / hourCycle24 of the locale).
  let hc = case hour12 {
    Some(True) -> hc_locale_default
    Some(False) -> "h23"
    None -> hc
  }
  let ext_used =
    list.filter_map(
      [
        #("ca", ca_from_ext, calendar),
        #("nu", nu_from_ext, nu),
        #("hc", hc_from_ext && hour12 == None, hc),
      ],
      fn(t) {
        case t {
          #(k, True, v) -> Ok(#(k, v))
          _ -> Error(Nil)
        }
      },
    )
  let locale = build_resolved_locale(data_locale, ext_used)
  // timeZone
  use #(tz_v, state) <- result.try(opt_get(state, opts, "timeZone"))
  use #(tz_name, tz_offset, tz_system, state) <- result.try(case tz_v {
    // DefaultTimeZone: the host environment zone (offset resolved per-date).
    JsUndefined -> {
      let offset = 0 - date.tz_offset_minutes(date.now_ms())
      // The host zone name is unknown; expose "UTC" while formatting with
      // the live host offset (tzSystem).
      Ok(#("UTC", offset, True, state))
    }
    _ -> {
      use #(s, state) <- result.try(coerce.js_to_string(state, tz_v))
      case canonical_time_zone(s) {
        Some(#(name, offset)) -> Ok(#(name, offset, False, state))
        None -> throw_range(state, "Invalid time zone specified: " <> s)
      }
    }
  })
  // Component options (table order).
  use #(weekday, state) <- result.try(get_str_opt(
    state,
    opts,
    "weekday",
    ["narrow", "short", "long"],
    None,
  ))
  use #(era, state) <- result.try(get_str_opt(
    state,
    opts,
    "era",
    ["narrow", "short", "long"],
    None,
  ))
  use #(year, state) <- result.try(get_str_opt(
    state,
    opts,
    "year",
    ["2-digit", "numeric"],
    None,
  ))
  use #(month, state) <- result.try(get_str_opt(
    state,
    opts,
    "month",
    ["2-digit", "numeric", "narrow", "short", "long"],
    None,
  ))
  use #(day, state) <- result.try(get_str_opt(
    state,
    opts,
    "day",
    ["2-digit", "numeric"],
    None,
  ))
  use #(day_period, state) <- result.try(get_str_opt(
    state,
    opts,
    "dayPeriod",
    ["narrow", "short", "long"],
    None,
  ))
  use #(hour, state) <- result.try(get_str_opt(
    state,
    opts,
    "hour",
    ["2-digit", "numeric"],
    None,
  ))
  use #(minute, state) <- result.try(get_str_opt(
    state,
    opts,
    "minute",
    ["2-digit", "numeric"],
    None,
  ))
  use #(second, state) <- result.try(get_str_opt(
    state,
    opts,
    "second",
    ["2-digit", "numeric"],
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
  use #(tz_name_opt, state) <- result.try(get_str_opt(
    state,
    opts,
    "timeZoneName",
    [
      "short", "long", "shortOffset", "longOffset", "shortGeneric",
      "longGeneric",
    ],
    None,
  ))
  use #(_format_matcher, state) <- result.try(get_str_opt(
    state,
    opts,
    "formatMatcher",
    ["basic", "best fit"],
    Some("best fit"),
  ))
  use #(date_style, state) <- result.try(get_str_opt(
    state,
    opts,
    "dateStyle",
    ["full", "long", "medium", "short"],
    None,
  ))
  use #(time_style, state) <- result.try(get_str_opt(
    state,
    opts,
    "timeStyle",
    ["full", "long", "medium", "short"],
    None,
  ))
  let explicit =
    [
      weekday, era, year, month, day, day_period, hour, minute, second,
      tz_name_opt,
    ]
    |> list.any(option.is_some)
    || option.is_some(fractional)
  // ToDateTimeOptions: defaults apply when no component of the required
  // group was requested.
  let required_present = case required {
    "date" -> list.any([weekday, year, month, day], option.is_some)
    "time" ->
      list.any([day_period, hour, minute, second], option.is_some)
      || option.is_some(fractional)
    _ -> explicit
  }
  use Nil <- result.try(case required {
    "date" ->
      case time_style {
        Some(_) ->
          throw_type(state, "timeStyle cannot be used with toLocaleDateString")
        None -> Ok(Nil)
      }
    "time" ->
      case date_style {
        Some(_) ->
          throw_type(state, "dateStyle cannot be used with toLocaleTimeString")
        None -> Ok(Nil)
      }
    _ -> Ok(Nil)
  })
  use Nil <- result.try(
    case
      { option.is_some(date_style) || option.is_some(time_style) } && explicit
    {
      True ->
        throw_type(
          state,
          "Invalid option: dateStyle/timeStyle cannot be used with other date/time options",
        )
      False -> Ok(Nil)
    },
  )
  // Expand styles / apply defaults into the effective formatting components.
  let user_date =
    present_pairs([
      #(DtfWeekday, weekday),
      #(DtfEra, era),
      #(DtfYear, year),
      #(DtfMonth, month),
      #(DtfDay, day),
    ])
  let user_time =
    present_pairs([
      #(DtfDayPeriod, day_period),
      #(DtfHour, hour),
      #(DtfMinute, minute),
      #(DtfSecond, second),
      #(DtfFractionalSecondDigits, option.map(fractional, int.to_string)),
      #(DtfTimeZoneName, tz_name_opt),
    ])
  let #(c_date, c_time) = case date_style, time_style, required_present {
    None, None, False -> {
      let merge = fn(
        user: List(#(DtfComponent, String)),
        defaults: List(#(DtfComponent, String)),
      ) {
        list.append(
          user,
          list.filter(defaults, fn(d) { !list.any(user, fn(u) { u.0 == d.0 }) }),
        )
      }
      #(
        merge(
          user_date,
          list.filter(default_components, fn(kv) {
            list.contains([DtfWeekday, DtfEra, DtfYear, DtfMonth, DtfDay], kv.0)
          }),
        ),
        merge(
          user_time,
          list.filter(default_components, fn(kv) {
            list.contains([DtfHour, DtfMinute, DtfSecond], kv.0)
          }),
        ),
      )
    }
    None, None, True -> #(user_date, user_time)
    ds, ts, _ -> #(date_style_components(ds), time_style_components(ts))
  }
  let components = components_from_pairs(list.append(c_date, c_time))
  let has_hour = option.is_some(components.hour)
  // resolvedOptions component view: the option the user requested, or —
  // with no dateStyle/timeStyle and no explicit option of the required
  // group — the locale default for that component. With styles set, only
  // the styles themselves are visible.
  let public = fn(user: Option(String), which: DtfComponent) -> Option(String) {
    case user, date_style, time_style, required_present {
      Some(_), _, _, _ -> user
      None, None, None, False ->
        list.key_find(default_components, which) |> option.from_result
      None, _, _, _ -> None
    }
  }
  // The nine component options that were explicitly provided — needed at
  // format time to compute per-Temporal-type formats
  // (GetDateTimeFormat with inherit = ~relevant~).
  let explicit_names =
    list.filter_map(
      [
        #(DtfWeekday, weekday),
        #(DtfYear, year),
        #(DtfMonth, month),
        #(DtfDay, day),
        #(DtfDayPeriod, day_period),
        #(DtfHour, hour),
        #(DtfMinute, minute),
        #(DtfSecond, second),
        #(DtfFractionalSecondDigits, option.map(fractional, int.to_string)),
      ],
      fn(kv) {
        case kv {
          #(k, Some(_)) -> Ok(k)
          #(_, None) -> Error(Nil)
        }
      },
    )
  Ok(#(
    DateTimeFormatState(
      locale:,
      calendar:,
      numbering_system: nu,
      time_zone: tz_name,
      tz_offset_minutes: tz_offset,
      tz_system:,
      hour_cycle: case has_hour {
        True -> Some(hc)
        False -> None
      },
      weekday: public(weekday, DtfWeekday),
      era: public(era, DtfEra),
      year: public(year, DtfYear),
      month: public(month, DtfMonth),
      day: public(day, DtfDay),
      day_period: public(day_period, DtfDayPeriod),
      hour: public(hour, DtfHour),
      minute: public(minute, DtfMinute),
      second: public(second, DtfSecond),
      fractional_second_digits: fractional,
      time_zone_name: public(tz_name_opt, DtfTimeZoneName),
      date_style:,
      time_style:,
      explicit: explicit_names,
      components:,
      bound_format: None,
    ),
    state,
  ))
}

fn date_style_components(
  style: Option(String),
) -> List(#(DtfComponent, String)) {
  case style {
    Some("full") -> [
      #(DtfWeekday, "long"),
      #(DtfYear, "numeric"),
      #(DtfMonth, "long"),
      #(DtfDay, "numeric"),
    ]
    Some("long") -> [
      #(DtfYear, "numeric"),
      #(DtfMonth, "long"),
      #(DtfDay, "numeric"),
    ]
    Some("medium") -> [
      #(DtfYear, "numeric"),
      #(DtfMonth, "short"),
      #(DtfDay, "numeric"),
    ]
    Some("short") -> [
      #(DtfYear, "2-digit"),
      #(DtfMonth, "numeric"),
      #(DtfDay, "numeric"),
    ]
    _ -> []
  }
}

fn time_style_components(
  style: Option(String),
) -> List(#(DtfComponent, String)) {
  case style {
    Some("full") -> [
      #(DtfHour, "numeric"),
      #(DtfMinute, "2-digit"),
      #(DtfSecond, "2-digit"),
      #(DtfTimeZoneName, "long"),
    ]
    Some("long") -> [
      #(DtfHour, "numeric"),
      #(DtfMinute, "2-digit"),
      #(DtfSecond, "2-digit"),
      #(DtfTimeZoneName, "short"),
    ]
    Some("medium") -> [
      #(DtfHour, "numeric"),
      #(DtfMinute, "2-digit"),
      #(DtfSecond, "2-digit"),
    ]
    Some("short") -> [#(DtfHour, "numeric"), #(DtfMinute, "2-digit")]
    _ -> []
  }
}

/// IsValidTimeZoneName + identifier case normalization. Offsets come from a
/// fixed standard-time table (no DST database). Returns #(name, offset_min).
fn canonical_time_zone(s: String) -> Option(#(String, Int)) {
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
      Some(#(
        case lower {
          "etc/utc" -> "Etc/UTC"
          "etc/uct" -> "Etc/UCT"
          "uct" -> "UCT"
          _ -> "UTC"
        },
        0,
      ))
    "gmt" | "etc/greenwich" | "greenwich" -> Some(#("GMT", 0))
    "etc/gmt" | "etc/gmt0" | "etc/gmt+0" | "etc/gmt-0" -> Some(#("Etc/GMT", 0))
    _ ->
      case parse_offset_zone(s) {
        Some(minutes) -> Some(#(format_offset_zone(minutes), minutes))
        None ->
          case etc_gmt_zone(lower) {
            Some(res) -> Some(res)
            None -> iana_zone(lower)
          }
      }
  }
}

/// Etc/GMT+N (UTC-N) and Etc/GMT-N (UTC+N), N in 1..14.
fn etc_gmt_zone(lower: String) -> Option(#(String, Int)) {
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
            False -> Some(#(name, sign * n * 60))
          }
        }
        _, _ -> None
      }
    }
    _ -> None
  }
}

/// IANA Area/Location identifiers: structural validation, canonical casing,
/// and a fixed standard-time offset table.
fn iana_zone(lower: String) -> Option(#(String, Int)) {
  case lower {
    "est" -> Some(#("EST", -300))
    "cst6cdt" -> Some(#("CST6CDT", -360))
    "est5edt" -> Some(#("EST5EDT", -300))
    "mst7mdt" -> Some(#("MST7MDT", -420))
    "pst8pdt" -> Some(#("PST8PDT", -480))
    "mst" -> Some(#("MST", -420))
    "hst" -> Some(#("HST", -600))
    "cet" -> Some(#("CET", 60))
    "eet" -> Some(#("EET", 120))
    "met" -> Some(#("MET", 60))
    "wet" -> Some(#("WET", 0))
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
            True -> {
              let name = normalize_zone_case(lower)
              Some(#(name, iana_zone_offset(name) |> option.unwrap(0)))
            }
            False -> None
          }
        }
        _ -> None
      }
  }
}

fn is_zone_word(p: String) -> Bool {
  string.to_utf_codepoints(p)
  |> list.all(fn(cp) {
    let c = string.utf_codepoint_to_int(cp)
    { c >= 0x61 && c <= 0x7a }
    || { c >= 0x41 && c <= 0x5a }
    || { c >= 0x30 && c <= 0x39 }
    || c == 0x5f
    || c == 0x2b
    || c == 0x2d
  })
}

/// Canonical IANA casing: capitalize the first letter of each word
/// (split on "/", "_", "-").
fn normalize_zone_case(lower: String) -> String {
  string.split(lower, "/")
  |> list.map(fn(seg) {
    string.split(seg, "_")
    |> list.map(fn(word) {
      string.split(word, "-")
      |> list.index_map(fn(piece, i) {
        // Minor words inside hyphenated names stay lowercase
        // ("Port-au-Prince"); leading pieces are capitalized.
        case
          i > 0
          && list.contains(["au", "of", "es", "de", "du", "da", "la"], piece)
        {
          True -> piece
          False -> titlecase_ascii(piece)
        }
      })
      |> string.join("-")
    })
    |> string.join("_")
  })
  |> string.join("/")
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

/// Standard (non-DST) offsets for common IANA zones.
fn iana_zone_offset(s: String) -> Option(Int) {
  case s {
    "America/New_York" | "America/Toronto" -> Some(-300)
    "America/Chicago" | "America/Mexico_City" -> Some(-360)
    "America/Denver" | "America/Phoenix" -> Some(-420)
    "America/Los_Angeles" | "America/Vancouver" -> Some(-480)
    "America/Anchorage" -> Some(-540)
    "America/Sao_Paulo" | "America/Argentina/Buenos_Aires" -> Some(-180)
    "Pacific/Honolulu" -> Some(-600)
    "Europe/London" | "Europe/Lisbon" | "Atlantic/Reykjavik" -> Some(0)
    "Europe/Paris"
    | "Europe/Berlin"
    | "Europe/Madrid"
    | "Europe/Rome"
    | "Europe/Amsterdam"
    | "Europe/Brussels"
    | "Europe/Vienna"
    | "Europe/Warsaw"
    | "Europe/Stockholm"
    | "Europe/Zurich"
    | "Europe/Oslo"
    | "Europe/Copenhagen"
    | "Europe/Prague"
    | "Europe/Budapest" -> Some(60)
    "Europe/Athens"
    | "Europe/Helsinki"
    | "Europe/Kiev"
    | "Europe/Bucharest"
    | "Africa/Cairo"
    | "Africa/Johannesburg"
    | "Asia/Jerusalem" -> Some(120)
    "Europe/Moscow" | "Europe/Istanbul" | "Asia/Riyadh" -> Some(180)
    "Asia/Dubai" -> Some(240)
    "Asia/Karachi" -> Some(300)
    "Asia/Kolkata" | "Asia/Calcutta" -> Some(330)
    "Asia/Kathmandu" | "Asia/Katmandu" -> Some(345)
    "Asia/Dhaka" -> Some(360)
    "Asia/Bangkok" | "Asia/Jakarta" -> Some(420)
    "Asia/Shanghai"
    | "Asia/Hong_Kong"
    | "Asia/Singapore"
    | "Asia/Taipei"
    | "Australia/Perth"
    | "Asia/Macau" -> Some(480)
    "Asia/Tokyo" | "Asia/Seoul" -> Some(540)
    "Australia/Sydney" | "Australia/Melbourne" | "Australia/Brisbane" ->
      Some(600)
    "Pacific/Auckland" -> Some(720)
    "Pacific/Apia" -> Some(780)
    _ -> None
  }
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
  use #(type_, state) <- result.try(get_str_opt(
    state,
    opts,
    "type",
    ["cardinal", "ordinal"],
    Some("cardinal"),
  ))
  use #(notation, state) <- result.try(get_str_opt(
    state,
    opts,
    "notation",
    ["standard", "scientific", "engineering", "compact"],
    Some("standard"),
  ))
  let notation = option.unwrap(notation, "standard")
  use #(compact_display, state) <- result.try(get_str_opt(
    state,
    opts,
    "compactDisplay",
    ["short", "long"],
    Some("short"),
  ))
  let #(_locale, data_locale, _ext) = resolve_locale(requested)
  use #(digits, state) <- result.try(digit_options(state, opts, 0, 3, notation))
  Ok(#(
    PluralRulesState(
      locale: data_locale,
      plural_type: option.unwrap(type_, "cardinal"),
      notation:,
      compact_display: case notation {
        "compact" -> Some(option.unwrap(compact_display, "short"))
        _ -> None
      },
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
  use #(type_, state) <- result.try(get_str_opt(
    state,
    opts,
    "type",
    ["conjunction", "disjunction", "unit"],
    Some("conjunction"),
  ))
  use #(style, state) <- result.try(get_str_opt(
    state,
    opts,
    "style",
    ["long", "short", "narrow"],
    Some("long"),
  ))
  let #(_locale, data_locale, _ext) = resolve_locale(requested)
  Ok(#(
    ListFormatState(
      locale: data_locale,
      list_type: option.unwrap(type_, "conjunction"),
      style: option.unwrap(style, "long"),
    ),
    state,
  ))
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
  use #(style, state) <- result.try(get_str_opt(
    state,
    opts,
    "style",
    ["long", "short", "narrow"],
    Some("long"),
  ))
  use #(numeric, state) <- result.try(get_str_opt(
    state,
    opts,
    "numeric",
    ["always", "auto"],
    Some("always"),
  ))
  Ok(#(
    RelativeTimeFormatState(
      locale:,
      style: option.unwrap(style, "long"),
      numeric: option.unwrap(numeric, "always"),
      numbering_system: nu,
    ),
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
  use #(granularity, state) <- result.try(get_str_opt(
    state,
    opts,
    "granularity",
    ["grapheme", "word", "sentence"],
    Some("grapheme"),
  ))
  let #(_locale, data_locale, _ext) = resolve_locale(requested)
  Ok(#(
    SegmenterState(
      locale: data_locale,
      granularity: option.unwrap(granularity, "grapheme"),
    ),
    state,
  ))
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
  use #(style, state) <- result.try(get_str_opt(
    state,
    opts,
    "style",
    ["narrow", "short", "long"],
    Some("long"),
  ))
  use #(type_, state) <- result.try(get_str_opt(
    state,
    opts,
    "type",
    ["language", "region", "script", "currency", "calendar", "dateTimeField"],
    None,
  ))
  use type_ <- result.try(case type_ {
    Some(t) -> Ok(t)
    None ->
      throw_type(state, "Intl.DisplayNames constructor requires type option")
  })
  use #(fallback, state) <- result.try(get_str_opt(
    state,
    opts,
    "fallback",
    ["code", "none"],
    Some("code"),
  ))
  use #(language_display, state) <- result.try(get_str_opt(
    state,
    opts,
    "languageDisplay",
    ["dialect", "standard"],
    Some("dialect"),
  ))
  let #(_locale, data_locale, _ext) = resolve_locale(requested)
  Ok(#(
    DisplayNamesState(
      locale: data_locale,
      style: option.unwrap(style, "long"),
      display_type: type_,
      fallback: option.unwrap(fallback, "code"),
      language_display: case type_ {
        "language" -> Some(option.unwrap(language_display, "dialect"))
        _ -> None
      },
    ),
    state,
  ))
}

// --- Intl.DurationFormat ---

const duration_units = [
  "years", "months", "weeks", "days", "hours", "minutes", "seconds",
  "milliseconds", "microseconds", "nanoseconds",
]

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
  use #(base_style, state) <- result.try(get_str_opt(
    state,
    opts,
    "style",
    ["long", "short", "narrow", "digital"],
    Some("short"),
  ))
  let base_style = option.unwrap(base_style, "short")
  // GetDurationUnitOptions for each unit in spec order, threading the
  // previous unit's INTERNAL style ("fractional" possible) — numeric
  // chaining and sub-second fraction folding depend on it.
  let unit = fn(state, name, prev) {
    duration_unit_options(state, opts, base_style, name, prev)
  }
  use #(years, prev, state) <- result.try(unit(state, "years", "none"))
  use #(months, prev, state) <- result.try(unit(state, "months", prev))
  use #(weeks, prev, state) <- result.try(unit(state, "weeks", prev))
  use #(days, prev, state) <- result.try(unit(state, "days", prev))
  use #(hours, prev, state) <- result.try(unit(state, "hours", prev))
  use #(minutes, prev, state) <- result.try(unit(state, "minutes", prev))
  use #(seconds, prev, state) <- result.try(unit(state, "seconds", prev))
  use #(milliseconds, prev, state) <- result.try(unit(
    state,
    "milliseconds",
    prev,
  ))
  use #(microseconds, prev, state) <- result.try(unit(
    state,
    "microseconds",
    prev,
  ))
  use #(nanoseconds, _prev, state) <- result.try(unit(
    state,
    "nanoseconds",
    prev,
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

/// GetDurationUnitOptions (Intl.DurationFormat §1.1.6): one unit's resolved
/// style/display. Returns the unit options (with the public style — the
/// internal "fractional" style is exposed as "numeric") plus the internal
/// style to thread into the next unit.
fn duration_unit_options(
  state: State(host),
  opts: Option(Ref),
  base_style: String,
  unit: String,
  prev_style: String,
) -> Result(#(DurationUnitOptions, String, State(host)), Thrown(host)) {
  let styles = case unit {
    "hours" | "minutes" | "seconds" -> [
      "long", "short", "narrow", "numeric", "2-digit",
    ]
    "milliseconds" | "microseconds" | "nanoseconds" -> [
      "long", "short", "narrow", "numeric",
    ]
    _ -> ["long", "short", "narrow"]
  }
  use #(style_opt, state) <- result.try(get_str_opt(
    state,
    opts,
    unit,
    styles,
    None,
  ))
  let sub_second =
    unit == "milliseconds" || unit == "microseconds" || unit == "nanoseconds"
  let prev_numeric =
    prev_style == "numeric"
    || prev_style == "2-digit"
    || prev_style == "fractional"
  let #(style, display_default) = case style_opt {
    Some(st) -> #(st, "always")
    None ->
      case base_style {
        "digital" ->
          case unit {
            "years" | "months" | "weeks" | "days" -> #("short", "auto")
            "minutes" | "seconds" ->
              case prev_numeric {
                True -> #("2-digit", "always")
                False -> #("numeric", "always")
              }
            _ -> #("numeric", "always")
          }
        _ ->
          case prev_numeric {
            True ->
              case unit {
                "minutes" | "seconds" -> #("2-digit", "always")
                _ ->
                  case sub_second {
                    True -> #("numeric", "always")
                    False -> #(base_style, "auto")
                  }
              }
            False -> #(base_style, "auto")
          }
      }
  }
  // Sub-second "numeric" after a numeric unit folds into a fraction.
  let #(style, display_default) = case
    style == "numeric" && sub_second && prev_numeric
  {
    True -> #("fractional", "auto")
    False -> #(style, display_default)
  }
  use #(display, state) <- result.try(get_str_opt(
    state,
    opts,
    unit <> "Display",
    ["auto", "always"],
    Some(display_default),
  ))
  let display = option.unwrap(display, display_default)
  use Nil <- result.try(case display == "always" && style == "fractional" {
    True ->
      throw_range(
        state,
        unit <> "Display cannot be 'always' for fractional units",
      )
    False -> Ok(Nil)
  })
  use Nil <- result.try(case prev_style {
    "fractional" ->
      case style {
        "fractional" -> Ok(Nil)
        _ ->
          throw_range(
            state,
            unit <> " style must be fractional after a fractional unit",
          )
      }
    "numeric" | "2-digit" ->
      case style {
        "fractional" | "numeric" | "2-digit" -> Ok(Nil)
        _ ->
          throw_range(
            state,
            unit <> " style cannot be mixed with numeric styles",
          )
      }
    _ -> Ok(Nil)
  })
  // [[<unit>Style]] "fractional" is exposed as "numeric" (resolvedOptions).
  let public_style = case style {
    "fractional" -> "numeric"
    st -> st
  }
  Ok(#(DurationUnitOptions(style: public_style, display:), style, state))
}

/// The DurationFormat per-unit options paired with their unit name, in
/// canonical spec order.
fn duration_unit_list(
  d: DurationFormatState,
) -> List(#(String, DurationUnitOptions)) {
  [
    #("years", d.years),
    #("months", d.months),
    #("weeks", d.weeks),
    #("days", d.days),
    #("hours", d.hours),
    #("minutes", d.minutes),
    #("seconds", d.seconds),
    #("milliseconds", d.milliseconds),
    #("microseconds", d.microseconds),
    #("nanoseconds", d.nanoseconds),
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
        #("usage", JsString(c.usage)),
        #("sensitivity", JsString(c.sensitivity)),
        #("ignorePunctuation", JsBool(c.ignore_punctuation)),
        #("collation", JsString(c.collation)),
        #("numeric", JsBool(c.numeric)),
        #("caseFirst", JsString(c.case_first)),
      ])
      NumberFormatData(nf) -> {
        let dg = nf.digits
        #(
          state,
          present_pairs([
            #("locale", Some(JsString(nf.locale))),
            #("numberingSystem", Some(JsString(nf.numbering_system))),
            #("style", Some(JsString(nf.style))),
            #("currency", option.map(nf.currency, JsString)),
            #("currencyDisplay", option.map(nf.currency_display, JsString)),
            #("currencySign", option.map(nf.currency_sign, JsString)),
            #("unit", option.map(nf.unit, JsString)),
            #("unitDisplay", option.map(nf.unit_display, JsString)),
            #(
              "minimumIntegerDigits",
              Some(value.from_int(dg.minimum_integer_digits)),
            ),
            #(
              "minimumFractionDigits",
              option.map(dg.minimum_fraction_digits, value.from_int),
            ),
            #(
              "maximumFractionDigits",
              option.map(dg.maximum_fraction_digits, value.from_int),
            ),
            #(
              "minimumSignificantDigits",
              option.map(dg.minimum_significant_digits, value.from_int),
            ),
            #(
              "maximumSignificantDigits",
              option.map(dg.maximum_significant_digits, value.from_int),
            ),
            #("useGrouping", Some(use_grouping_js(nf.use_grouping))),
            #("notation", Some(JsString(nf.notation))),
            #("compactDisplay", option.map(nf.compact_display, JsString)),
            #("signDisplay", Some(JsString(nf.sign_display))),
            #("roundingIncrement", Some(value.from_int(dg.rounding_increment))),
            #("roundingMode", Some(JsString(dg.rounding_mode))),
            #("roundingPriority", Some(JsString(dg.rounding_priority))),
            #("trailingZeroDisplay", Some(JsString(dg.trailing_zero_display))),
          ]),
        )
      }
      DateTimeFormatData(d) -> #(
        state,
        present_pairs([
          #("locale", Some(JsString(d.locale))),
          #("calendar", Some(JsString(d.calendar))),
          #("numberingSystem", Some(JsString(d.numbering_system))),
          #("timeZone", Some(JsString(d.time_zone))),
          #("hourCycle", option.map(d.hour_cycle, JsString)),
          #(
            "hour12",
            option.map(d.hour_cycle, fn(hc) {
              JsBool(hc == "h11" || hc == "h12")
            }),
          ),
          #("weekday", option.map(d.weekday, JsString)),
          #("era", option.map(d.era, JsString)),
          #("year", option.map(d.year, JsString)),
          #("month", option.map(d.month, JsString)),
          #("day", option.map(d.day, JsString)),
          #("dayPeriod", option.map(d.day_period, JsString)),
          #("hour", option.map(d.hour, JsString)),
          #("minute", option.map(d.minute, JsString)),
          #("second", option.map(d.second, JsString)),
          #(
            "fractionalSecondDigits",
            option.map(d.fractional_second_digits, value.from_int),
          ),
          #("timeZoneName", option.map(d.time_zone_name, JsString)),
          #("dateStyle", option.map(d.date_style, JsString)),
          #("timeStyle", option.map(d.time_style, JsString)),
        ]),
      )
      PluralRulesData(p) -> {
        let dg = p.digits
        let #(state, cats) =
          alloc_array(
            state,
            fmt.plural_categories_en(p.plural_type) |> list.map(JsString),
          )
        #(
          state,
          present_pairs([
            #("locale", Some(JsString(p.locale))),
            #("type", Some(JsString(p.plural_type))),
            #("notation", Some(JsString(p.notation))),
            #("compactDisplay", option.map(p.compact_display, JsString)),
            #(
              "minimumIntegerDigits",
              Some(value.from_int(dg.minimum_integer_digits)),
            ),
            #(
              "minimumFractionDigits",
              option.map(dg.minimum_fraction_digits, value.from_int),
            ),
            #(
              "maximumFractionDigits",
              option.map(dg.maximum_fraction_digits, value.from_int),
            ),
            #(
              "minimumSignificantDigits",
              option.map(dg.minimum_significant_digits, value.from_int),
            ),
            #(
              "maximumSignificantDigits",
              option.map(dg.maximum_significant_digits, value.from_int),
            ),
            #("pluralCategories", Some(cats)),
            #("roundingIncrement", Some(value.from_int(dg.rounding_increment))),
            #("roundingMode", Some(JsString(dg.rounding_mode))),
            #("roundingPriority", Some(JsString(dg.rounding_priority))),
            #("trailingZeroDisplay", Some(JsString(dg.trailing_zero_display))),
          ]),
        )
      }
      ListFormatData(l) -> #(state, [
        #("locale", JsString(l.locale)),
        #("type", JsString(l.list_type)),
        #("style", JsString(l.style)),
      ])
      RelativeTimeFormatData(r) -> #(state, [
        #("locale", JsString(r.locale)),
        #("style", JsString(r.style)),
        #("numeric", JsString(r.numeric)),
        #("numberingSystem", JsString(r.numbering_system)),
      ])
      SegmenterData(s) -> #(state, [
        #("locale", JsString(s.locale)),
        #("granularity", JsString(s.granularity)),
      ])
      DisplayNamesData(d) -> #(
        state,
        present_pairs([
          #("locale", Some(JsString(d.locale))),
          #("style", Some(JsString(d.style))),
          #("type", Some(JsString(d.display_type))),
          #("fallback", Some(JsString(d.fallback))),
          #("languageDisplay", option.map(d.language_display, JsString)),
        ]),
      )
      DurationFormatData(df) -> #(
        state,
        list.flatten([
          [
            #("locale", JsString(df.locale)),
            #("numberingSystem", JsString(df.numbering_system)),
            #("style", JsString(df.style)),
          ],
          list.flat_map(duration_unit_list(df), fn(u) {
            let #(name, o) = u
            [
              #(name, JsString(o.style)),
              #(name <> "Display", JsString(o.display)),
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

/// `[[UseGrouping]]` as its JS resolvedOptions value.
fn use_grouping_js(g: value.IntlUseGrouping) -> JsValue {
  case g {
    GroupingNever -> JsBool(False)
    GroupingMode(s) -> JsString(s)
  }
}

// ============================================================================
// Bound method getters (format / compare)
// ============================================================================

fn bound_getter(
  service: IntlService,
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  run({
    use #(ref, data) <- result.try(branded(
      state,
      this,
      service,
      "Intl." <> service_name(service) <> " bound method getter",
    ))
    let #(cached, arity) = case data {
      CollatorData(c) -> #(c.bound_compare, 2)
      NumberFormatData(nf) -> #(nf.bound_format, 1)
      DateTimeFormatData(d) -> #(d.bound_format, 1)
      _ -> #(None, 1)
    }
    case cached {
      Some(fn_ref) -> Ok(#(JsObject(fn_ref), state))
      None -> {
        let #(heap, fn_ref) =
          common.alloc_native_fn(
            state.heap,
            state.builtins.function.prototype,
            IntlNative(IntlBoundMethod(service:, target: ref)),
            "",
            arity,
          )
        let heap = case data {
          CollatorData(c) ->
            write_intl_data(
              heap,
              ref,
              CollatorData(CollatorState(..c, bound_compare: Some(fn_ref))),
            )
          NumberFormatData(nf) ->
            write_intl_data(
              heap,
              ref,
              NumberFormatData(
                NumberFormatState(..nf, bound_format: Some(fn_ref)),
              ),
            )
          DateTimeFormatData(d) ->
            write_intl_data(
              heap,
              ref,
              DateTimeFormatData(
                DateTimeFormatState(..d, bound_format: Some(fn_ref)),
              ),
            )
          _ -> heap
        }
        Ok(#(JsObject(fn_ref), State(..state, heap:)))
      }
    }
  })
}

fn bound_method(
  service: IntlService,
  target: Ref,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  run({
    use #(_ref, data) <- result.try(branded(
      state,
      JsObject(target),
      service,
      "bound Intl method",
    ))
    case data {
      NumberFormatData(nf) -> {
        use #(parts, state) <- result.try(nf_format_parts(
          state,
          nf,
          first_arg_or_undefined(args),
        ))
        Ok(#(JsString(fmt.parts_to_string(parts)), state))
      }
      DateTimeFormatData(d) -> {
        use #(parts, state) <- result.try(dtf_format_parts(
          state,
          d,
          first_arg_or_undefined(args),
        ))
        Ok(#(JsString(fmt.parts_to_string(parts)), state))
      }
      CollatorData(c) -> {
        use #(a, state) <- result.try(coerce.js_to_string(
          state,
          first_arg_or_undefined(args),
        ))
        use #(b, state) <- result.try(coerce.js_to_string(
          state,
          helpers.list_at(args, 1) |> option.unwrap(JsUndefined),
        ))
        Ok(#(value.from_int(collator_compare(c, a, b)), state))
      }
      _ -> throw_type(state, "Unsupported bound method")
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
      locale: nf.locale,
      style: nf.style,
      currency: nf.currency,
      currency_display: option.unwrap(nf.currency_display, d.currency_display),
      currency_sign: option.unwrap(nf.currency_sign, d.currency_sign),
      unit: nf.unit,
      unit_display: option.unwrap(nf.unit_display, d.unit_display),
      use_grouping: case nf.use_grouping {
        GroupingNever -> "never"
        GroupingMode(s) -> s
      },
      notation: nf.notation,
      compact_display: option.unwrap(nf.compact_display, d.compact_display),
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
    fmt.NumOpts(
      ..d,
      locale: p.locale,
      notation: p.notation,
      compact_display: option.unwrap(p.compact_display, d.compact_display),
    ),
    p.digits,
  )
}

/// ToIntlMathematicalValue, approximated with ToNumber (BigInt allowed).
fn to_intl_number(
  state: State(host),
  v: JsValue,
) -> Result(#(value.JsNum, State(host)), Thrown(host)) {
  case v {
    value.JsBigInt(value.BigInt(n)) ->
      Ok(#(value.Finite(int.to_float(n)), state))
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
            apply_numbering_system(
              fmt.format_decimal_string_parts(opts, string.trim(str)),
              nu,
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
  Ok(#(apply_numbering_system(parts, nu), state))
}

fn nf_range_parts(
  state: State(host),
  nf: NumberFormatState,
  x_v: JsValue,
  y_v: JsValue,
) -> Result(#(List(#(String, String, String)), State(host)), Thrown(host)) {
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
  Ok(#(fmt.format_range_combine(nf.locale, x_parts, y_parts), state))
}

// ============================================================================
// DateTimeFormat formatting glue
// ============================================================================

/// A Temporal object as seen by DateTimeFormat (ECMA-402 HandleDateTimeValue).
type TemporalFormattable {
  TfDate(year: Int, month: Int, day: Int, calendar: String)
  TfYearMonth(year: Int, month: Int, day: Int, calendar: String)
  TfMonthDay(month: Int, day: Int, ref_year: Int, calendar: String)
  TfTime(hour: Int, minute: Int, second: Int, millisecond: Int)
  TfDateTime(
    year: Int,
    month: Int,
    day: Int,
    hour: Int,
    minute: Int,
    second: Int,
    millisecond: Int,
    calendar: String,
  )
  TfInstant(epoch_ns: Int)
  TfZoned
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
        )) -> Some(TfDate(year:, month:, day:, calendar:))
        Some(ObjectSlot(
          kind: TemporalYearMonthSlot(year:, month:, day:, calendar:),
          ..,
        )) -> Some(TfYearMonth(year:, month:, day:, calendar:))
        Some(ObjectSlot(
          kind: TemporalMonthDaySlot(month:, day:, ref_year:, calendar:),
          ..,
        )) -> Some(TfMonthDay(month:, day:, ref_year:, calendar:))
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
        )) -> Some(TfTime(hour:, minute:, second:, millisecond:))
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
          Some(TfDateTime(
            year:,
            month:,
            day:,
            hour:,
            minute:,
            second:,
            millisecond:,
            calendar:,
          ))
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
    TfDate(..), TfDate(..) -> True
    TfYearMonth(..), TfYearMonth(..) -> True
    TfMonthDay(..), TfMonthDay(..) -> True
    TfTime(..), TfTime(..) -> True
    TfDateTime(..), TfDateTime(..) -> True
    TfInstant(..), TfInstant(..) -> True
    TfZoned, TfZoned -> True
    _, _ -> False
  }
}

/// Allowed / required / default components per Temporal type, plus
/// whether `era` / hour-cycle options carry over (GetDateTimeFormat's
/// ~relevant~ inheritance).
fn tf_component_rules(
  t: TemporalFormattable,
) -> #(List(DtfComponent), List(DtfComponent), List(DtfComponent), Bool) {
  case t {
    TfDate(..) -> #(
      [DtfWeekday, DtfEra, DtfYear, DtfMonth, DtfDay],
      [DtfWeekday, DtfYear, DtfMonth, DtfDay],
      [DtfYear, DtfMonth, DtfDay],
      True,
    )
    TfYearMonth(..) -> #(
      [DtfEra, DtfYear, DtfMonth],
      [DtfYear, DtfMonth],
      [DtfYear, DtfMonth],
      True,
    )
    TfMonthDay(..) -> #(
      [DtfMonth, DtfDay],
      [DtfMonth, DtfDay],
      [DtfMonth, DtfDay],
      False,
    )
    TfTime(..) -> #(
      [DtfDayPeriod, DtfHour, DtfMinute, DtfSecond, DtfFractionalSecondDigits],
      [DtfDayPeriod, DtfHour, DtfMinute, DtfSecond, DtfFractionalSecondDigits],
      [DtfHour, DtfMinute, DtfSecond],
      False,
    )
    TfDateTime(..) -> #(
      [
        DtfWeekday, DtfEra, DtfYear, DtfMonth, DtfDay, DtfDayPeriod, DtfHour,
        DtfMinute, DtfSecond, DtfFractionalSecondDigits,
      ],
      [
        DtfWeekday, DtfYear, DtfMonth, DtfDay, DtfDayPeriod, DtfHour, DtfMinute,
        DtfSecond, DtfFractionalSecondDigits,
      ],
      [DtfYear, DtfMonth, DtfDay, DtfHour, DtfMinute, DtfSecond],
      True,
    )
    TfInstant(..) | TfZoned -> #([], [], [], False)
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
    TfZoned ->
      throw_type(
        state,
        "Temporal.ZonedDateTime cannot be formatted with Intl.DateTimeFormat; use Temporal.ZonedDateTime.prototype.toLocaleString instead",
      )
    TfInstant(..) -> Ok(#(d, state))
    _ -> {
      let cal_ok = case t {
        TfDate(calendar:, ..) | TfDateTime(calendar:, ..) ->
          calendar == "iso8601" || calendar == d.calendar
        TfYearMonth(calendar:, ..) | TfMonthDay(calendar:, ..) ->
          calendar == d.calendar
        _ -> True
      }
      use Nil <- result.try(case cal_ok {
        True -> Ok(Nil)
        False ->
          throw_range(
            state,
            "Temporal object calendar does not match DateTimeFormat calendar",
          )
      })
      let #(allowed, required, defaults, copy_era) = tf_component_rules(t)
      let has_styles =
        option.is_some(d.date_style) || option.is_some(d.time_style)
      case has_styles {
        True -> {
          // AdjustDateTimeStyleFormat: per-type formats exist only when the
          // matching style was given; keep only the allowed components.
          let style_ok = case t {
            TfDate(..) | TfYearMonth(..) | TfMonthDay(..) ->
              option.is_some(d.date_style)
            TfTime(..) -> option.is_some(d.time_style)
            _ -> True
          }
          use Nil <- result.try(case style_ok {
            True -> Ok(Nil)
            False ->
              throw_type(
                state,
                "DateTimeFormat has no suitable format for this Temporal type",
              )
          })
          let kept =
            list.filter_map(allowed, fn(name) {
              case component(d.components, name) {
                Some(v) -> Ok(#(name, v))
                None -> Error(Nil)
              }
            })
          Ok(#(with_components(d, kept), state))
        }
        False -> {
          // GetDateTimeFormat with inherit = ~relevant~ over the explicitly
          // provided component options.
          let in_required =
            list.filter(d.explicit, fn(name) { list.contains(required, name) })
          let era_comps = case copy_era, d.era {
            True, Some(e) -> [#(DtfEra, e)]
            _, _ -> []
          }
          case in_required {
            [] ->
              case d.explicit {
                [] -> {
                  let comps =
                    list.map(defaults, fn(name) { #(name, "numeric") })
                  Ok(#(with_components(d, list.append(era_comps, comps)), state))
                }
                _ ->
                  throw_type(
                    state,
                    "DateTimeFormat options have no overlap with this Temporal type",
                  )
              }
            _ -> {
              let comps =
                list.filter_map(required, fn(name) {
                  case list.contains(d.explicit, name) {
                    False -> Error(Nil)
                    True ->
                      case name {
                        DtfFractionalSecondDigits ->
                          case d.fractional_second_digits {
                            Some(n) -> Ok(#(name, int.to_string(n)))
                            None -> Error(Nil)
                          }
                        _ ->
                          case component(d.components, name) {
                            Some(v) -> Ok(#(name, v))
                            None -> Error(Nil)
                          }
                      }
                  }
                })
              Ok(#(with_components(d, list.append(era_comps, comps)), state))
            }
          }
        }
      }
    }
  }
}

/// Replace the formatter's component table with `comps`.
fn with_components(
  d: DateTimeFormatState,
  comps: List(#(DtfComponent, String)),
) -> DateTimeFormatState {
  DateTimeFormatState(..d, components: components_from_pairs(comps))
}

/// Howard Hinnant's days_from_civil: civil date -> days since 1970-01-01.
fn days_from_civil(year: Int, month: Int, day: Int) -> Int {
  let y = case month <= 2 {
    True -> year - 1
    False -> year
  }
  let era = case y >= 0 {
    True -> y / 400
    False -> { y - 399 } / 400
  }
  let yoe = y - era * 400
  let mp = case month > 2 {
    True -> month - 3
    False -> month + 9
  }
  let doy = { 153 * mp + 2 } / 5 + day - 1
  let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy
  era * 146_097 + doe - 719_468
}

fn civil_week_day(year: Int, month: Int, day: Int) -> Int {
  let days = days_from_civil(year, month, day)
  { { days % 7 } + 11 } % 7
}

/// Wall-clock fields for a Temporal value. Plain types format their fields
/// directly (the formatter's time zone is ignored); Instant converts through
/// the formatter's zone like a Number time value.
fn dtf_temporal_fields(
  d: DateTimeFormatState,
  t: TemporalFormattable,
) -> fmt.DateFields {
  case t {
    TfDate(year:, month:, day:, ..) | TfYearMonth(year:, month:, day:, ..) ->
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
    TfMonthDay(month:, day:, ref_year:, ..) ->
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
    TfTime(hour:, minute:, second:, millisecond:) ->
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
    TfDateTime(year:, month:, day:, hour:, minute:, second:, millisecond:, ..) ->
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
    TfInstant(epoch_ns:) -> {
      let ms = floor_div(epoch_ns, 1_000_000)
      let offset = case d.tz_system {
        True -> 0 - date.tz_offset_minutes(ms)
        False -> d.tz_offset_minutes
      }
      fmt.fields_from_epoch_ms(int.to_float(ms), offset)
    }
    TfZoned ->
      // Unreachable: dtf_temporal_state throws for ZonedDateTime first.
      fmt.fields_from_epoch_ms(0.0, 0)
  }
}

fn floor_div(a: Int, b: Int) -> Int {
  case a < 0 && a % b != 0 {
    True -> a / b - 1
    False -> a / b
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
      let parts = build_dtf_parts(d, dtf_temporal_fields(d, t))
      Ok(#(apply_numbering_system_dtf(parts, d.numbering_system), state))
    }
    None -> dtf_format_parts_number(state, d, date_v)
  }
}

fn dtf_format_parts_number(
  state: State(host),
  d: DateTimeFormatState,
  date_v: JsValue,
) -> Result(#(List(fmt.Part), State(host)), Thrown(host)) {
  use #(fields, state) <- result.try(dtf_fields_number(state, d, date_v))
  let parts = build_dtf_parts(d, fields)
  Ok(#(apply_numbering_system_dtf(parts, d.numbering_system), state))
}

fn apply_numbering_system_dtf(
  parts: List(fmt.Part),
  nu: String,
) -> List(fmt.Part) {
  case nu {
    "latn" -> parts
    _ ->
      list.map(parts, fn(part) {
        let #(t, v) = part
        case t {
          "year"
          | "month"
          | "day"
          | "hour"
          | "minute"
          | "second"
          | "fractionalSecond" -> #(t, translit_dtf(v, nu))
          _ -> part
        }
      })
  }
}

fn translit_dtf(s: String, nu: String) -> String {
  // Reuse digit transliteration; non-digits pass through.
  translit_digits(s, nu)
}

fn build_dtf_parts(
  d: DateTimeFormatState,
  fields: fmt.DateFields,
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
  let hc = option.unwrap(d.hour_cycle, "h12")

  let display_year = case fields.year <= 0 {
    True -> 1 - fields.year
    False -> fields.year
  }
  let year_str = fn(style) {
    case style {
      "2-digit" -> fmt.pad2(display_year % 100)
      _ -> int.to_string(display_year)
    }
  }
  let weekday_parts = case weekday {
    Some(w) -> [#("weekday", fmt.weekday_name(fields.week_day, w))]
    None -> []
  }
  let named_month = case month {
    Some("long") | Some("short") | Some("narrow") -> True
    _ -> False
  }
  // Date portion.
  let date_parts = case named_month {
    True -> {
      let m_part = [
        #("month", fmt.month_name(fields.month, option.unwrap(month, "long"))),
      ]
      let d_part = case day {
        Some(ds) -> [
          #("literal", " "),
          #("day", day_str(ds, fields.day)),
        ]
        None -> []
      }
      let y_part = case year {
        Some(ys) ->
          case day {
            Some(_) -> [#("literal", ", "), #("year", year_str(ys))]
            None -> [#("literal", " "), #("year", year_str(ys))]
          }
        None -> []
      }
      list.flatten([m_part, d_part, y_part])
    }
    False -> {
      // Numeric month / partial combos. Most locales we ship use M/D/Y with
      // "/"; German-style locales order D.M.Y with ".".
      let lang = case tags.parse(d.locale) {
        Ok(lid) -> lid.language
        Error(Nil) -> "en"
      }
      let dotted =
        list.contains(
          ["de", "fi", "ru", "cs", "tr", "nb", "pl", "uk", "bg", "sr", "lv"],
          lang,
        )
      let raw = [
        #(
          "month",
          option.map(month, fn(ms) { month_num_str(ms, fields.month) }),
        ),
        #("day", option.map(day, fn(ds) { day_str(ds, fields.day) })),
        #("year", option.map(year, year_str)),
      ]
      let raw = case dotted {
        True -> [
          #("day", option.map(day, fn(ds) { day_str(ds, fields.day) })),
          #(
            "month",
            option.map(month, fn(ms) { month_num_str(ms, fields.month) }),
          ),
          #("year", option.map(year, year_str)),
        ]
        False -> raw
      }
      let pieces =
        list.filter_map(raw, fn(kv) {
          case kv {
            #(t, Some(v)) -> Ok(#(t, v))
            #(_, None) -> Error(Nil)
          }
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
        #("literal", " "),
        #("era", fmt.era_name(fields.year, e)),
      ])
    _, _ -> date_parts
  }
  // Time portion.
  let #(display_hour, dp) = case hc {
    "h11" -> #(fields.hour % 12, am_pm(fields.hour))
    "h12" -> {
      let h = fields.hour % 12
      #(
        case h {
          0 -> 12
          _ -> h
        },
        am_pm(fields.hour),
      )
    }
    "h24" -> #(
      case fields.hour {
        0 -> 24
        h -> h
      },
      "",
    )
    _ -> #(fields.hour, "")
  }
  let hour_parts = case hour {
    Some("2-digit") -> [#("hour", fmt.pad2(display_hour))]
    Some(_) -> [#("hour", int.to_string(display_hour))]
    None -> []
  }
  let minute_parts = case minute {
    Some(style) -> {
      let v = case hour, second {
        Some(_), _ -> fmt.pad2(fields.minute)
        None, Some(_) -> fmt.pad2(fields.minute)
        None, None ->
          case style {
            "2-digit" -> fmt.pad2(fields.minute)
            _ -> int.to_string(fields.minute)
          }
      }
      case hour_parts {
        [] -> [#("minute", v)]
        _ -> [#("literal", ":"), #("minute", v)]
      }
    }
    None -> []
  }
  let second_parts = case second {
    Some(style) -> {
      let v = case minute {
        Some(_) -> fmt.pad2(fields.second)
        None ->
          case style {
            "2-digit" -> fmt.pad2(fields.second)
            _ -> int.to_string(fields.second)
          }
      }
      case minute_parts {
        [] -> [#("second", v)]
        _ -> [#("literal", ":"), #("second", v)]
      }
    }
    None -> []
  }
  let fractional_parts = case fractional {
    Some(fs) ->
      case int.parse(fs) {
        Ok(digits) -> {
          let ms3 = string.pad_start(int.to_string(fields.millisecond), 3, "0")
          let v = string.slice(ms3, 0, digits)
          case second_parts {
            [] -> [#("fractionalSecond", v)]
            _ -> [#("literal", "."), #("fractionalSecond", v)]
          }
        }
        Error(Nil) -> []
      }
    None -> []
  }
  let day_period_parts = case day_period, hour {
    Some(dps), _ -> [
      #("literal", " "),
      #("dayPeriod", fmt.day_period_name(fields.hour, fields.minute, dps)),
    ]
    None, Some(_) ->
      case dp {
        "" -> []
        _ -> [#("literal", " "), #("dayPeriod", dp)]
      }
    None, None -> []
  }
  // Standalone dayPeriod (no hour): no leading space.
  let day_period_parts = case hour, day_period {
    None, Some(dps) -> [
      #("dayPeriod", fmt.day_period_name(fields.hour, fields.minute, dps)),
    ]
    _, _ -> day_period_parts
  }
  let tz_parts = case tz_name {
    Some(style) -> {
      let name = tz_display(d.time_zone, style, d.tz_offset_minutes)
      [#("literal", " "), #("timeZoneName", name)]
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
    [], [#("literal", _), ..rest] -> rest
    _, _ -> list.append(time_parts, tz_parts)
  }
  let all = case weekday_parts, date_parts, time_parts {
    [], [], t -> t
    w, [], [] -> w
    [], d, [] -> d
    w, d, [] -> list.flatten([w, [#("literal", ", ")], d])
    [], d, t -> list.flatten([d, [#("literal", ", ")], t])
    w, [], t -> list.flatten([w, [#("literal", " ")], t])
    w, d, t ->
      list.flatten([w, [#("literal", ", ")], d, [#("literal", ", ")], t])
  }
  all
}

fn am_pm(hour: Int) -> String {
  case hour < 12 {
    True -> "AM"
    False -> "PM"
  }
}

fn day_str(style: String, day: Int) -> String {
  case style {
    "2-digit" -> fmt.pad2(day)
    _ -> int.to_string(day)
  }
}

fn month_num_str(style: String, month: Int) -> String {
  case style {
    "2-digit" -> fmt.pad2(month)
    _ -> int.to_string(month)
  }
}

fn join_parts(pieces: List(fmt.Part), sep: String) -> List(fmt.Part) {
  case pieces {
    [] -> []
    [first, ..rest] ->
      list.fold(rest, [first], fn(acc, p) { [p, #("literal", sep), ..acc] })
      |> list.reverse
  }
}

/// Render a formatter's [[TimeZone]] under a timeZoneName style.
fn tz_display(name: String, style: String, offset: Int) -> String {
  case name, style {
    "UTC", "short" | "UTC", "shortGeneric" -> "UTC"
    "UTC", "long" | "UTC", "longGeneric" -> "Coordinated Universal Time"
    "UTC", "shortOffset" -> "GMT"
    "UTC", "longOffset" -> "GMT"
    _, "long" | _, "longOffset" | _, "longGeneric" -> gmt_offset(offset, True)
    _, _ -> gmt_offset(offset, False)
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
) -> Result(#(List(#(String, String, String)), State(host)), Thrown(host)) {
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
      case fmt.parts_to_string(x_parts) == fmt.parts_to_string(y_parts) {
        True -> Ok(#(list.map(x_parts, fn(p) { #(p.0, p.1, "shared") }), state))
        False ->
          Ok(#(
            list.flatten([
              list.map(x_parts, fn(p) { #(p.0, p.1, "startRange") }),
              [#("literal", " – ", "shared")],
              list.map(y_parts, fn(p) { #(p.0, p.1, "endRange") }),
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
) -> Result(
  Option(#(List(#(String, String, String)), State(host))),
  Thrown(host),
) {
  let c = d.components
  let month_style = c.month
  let named_month = case month_style {
    Some("long") | Some("short") | Some("narrow") -> True
    _ -> False
  }
  let date_only =
    c.hour == None && c.minute == None && c.second == None && c.weekday == None
  let has_ymd = c.year != None && c.month != None && c.day != None
  case named_month && date_only && has_ymd {
    False -> Ok(None)
    True -> {
      use #(xf, state) <- result.try(dtf_fields(state, d, x_v))
      use #(yf, state) <- result.try(dtf_fields(state, d, y_v))
      {
        let day_style = option.unwrap(c.day, "numeric")
        let year_style = option.unwrap(c.year, "numeric")
        let display_year = case xf.year <= 0 {
          True -> 1 - xf.year
          False -> xf.year
        }
        let year_str = case year_style {
          "2-digit" -> fmt.pad2(display_year % 100)
          _ -> int.to_string(display_year)
        }
        let mname = fn(m) {
          fmt.month_name(m, option.unwrap(month_style, "short"))
        }
        case xf.year == yf.year {
          False -> Ok(None)
          True ->
            case xf.month == yf.month, xf.day != yf.day {
              True, True -> {
                let parts = [
                  #("month", mname(xf.month), "shared"),
                  #("literal", " ", "shared"),
                  #("day", day_str(day_style, xf.day), "startRange"),
                  #("literal", " – ", "shared"),
                  #("day", day_str(day_style, yf.day), "endRange"),
                  #("literal", ", ", "shared"),
                  #("year", year_str, "shared"),
                ]
                Ok(Some(#(parts, state)))
              }
              False, _ -> {
                let parts = [
                  #("month", mname(xf.month), "startRange"),
                  #("literal", " ", "startRange"),
                  #("day", day_str(day_style, xf.day), "startRange"),
                  #("literal", " – ", "shared"),
                  #("month", mname(yf.month), "endRange"),
                  #("literal", " ", "endRange"),
                  #("day", day_str(day_style, yf.day), "endRange"),
                  #("literal", ", ", "shared"),
                  #("year", year_str, "shared"),
                ]
                Ok(Some(#(parts, state)))
              }
              True, False -> Ok(None)
            }
        }
      }
    }
  }
}

/// Compute the civil fields a DTF instance would use for a value.
fn dtf_fields(
  state: State(host),
  d: DateTimeFormatState,
  date_v: JsValue,
) -> Result(#(fmt.DateFields, State(host)), Thrown(host)) {
  case dtf_temporal_value(state, date_v) {
    Some(t) -> Ok(#(dtf_temporal_fields(d, t), state))
    None -> dtf_fields_number(state, d, date_v)
  }
}

fn dtf_fields_number(
  state: State(host),
  d: DateTimeFormatState,
  date_v: JsValue,
) -> Result(#(fmt.DateFields, State(host)), Thrown(host)) {
  use #(tv, state) <- result.try(case date_v {
    JsUndefined -> Ok(#(value.Finite(int.to_float(date.now_ms())), state))
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
  let offset = case d.tz_system {
    True -> 0 - date.tz_offset_minutes(float.truncate(tv_f))
    False -> d.tz_offset_minutes
  }
  Ok(#(fmt.fields_from_epoch_ms(tv_f, offset), state))
}

// ============================================================================
// Collator compare
// ============================================================================

fn collator_compare(c: CollatorState, a: String, b: String) -> Int {
  let sensitivity = c.sensitivity
  let numeric = c.numeric
  let ignore_punct = c.ignore_punctuation
  let prep = fn(s: String) {
    case ignore_punct {
      True -> strip_punctuation(s)
      False -> s
    }
  }
  let a = prep(a)
  let b = prep(b)
  // Primary: base letters (case- and accent-folded).
  let pa = collation_primary(a)
  let pb = collation_primary(b)
  case numeric {
    True ->
      case numeric_compare(string.to_graphemes(pa), string.to_graphemes(pb)) {
        0 -> collator_levels(sensitivity, a, b)
        n -> n
      }
    False ->
      case simple_compare(pa, pb) {
        0 -> collator_levels(sensitivity, a, b)
        n -> n
      }
  }
}

/// Secondary (accents) and tertiary (case) comparisons per sensitivity.
fn collator_levels(sensitivity: String, a: String, b: String) -> Int {
  let secondary = fn() {
    simple_compare(
      string.lowercase(fold_combining(a)),
      string.lowercase(fold_combining(b)),
    )
  }
  let tertiary = fn() {
    // Lowercase sorts before uppercase in the en default (caseFirst false).
    simple_compare(swap_case(fold_combining(a)), swap_case(fold_combining(b)))
  }
  case sensitivity {
    "base" -> 0
    "accent" -> secondary()
    "case" ->
      case tertiary() {
        0 -> 0
        n -> n
      }
    // variant
    _ ->
      case secondary() {
        0 -> tertiary()
        n -> n
      }
  }
}

fn strip_punctuation(s: String) -> String {
  string.to_utf_codepoints(s)
  |> list.filter(fn(cp) {
    let c = string.utf_codepoint_to_int(cp)
    !{
      c == 0x20
      || { c >= 0x21 && c <= 0x2f }
      || { c >= 0x3a && c <= 0x40 }
      || { c >= 0x5b && c <= 0x60 }
      || { c >= 0x7b && c <= 0x7e }
    }
  })
  |> string.from_utf_codepoints
}

/// Case-fold + strip accents to the primary collation key.
fn collation_primary(s: String) -> String {
  string.lowercase(s)
  |> string.to_graphemes
  |> list.map(deaccent)
  |> string.join("")
}

/// Normalize combining sequences to precomposed forms (rough NFC for the
/// Latin-1 accents the tests exercise), keeping accents.
fn fold_combining(s: String) -> String {
  string.to_graphemes(s)
  |> list.map(compose_grapheme)
  |> string.join("")
}

fn compose_grapheme(g: String) -> String {
  case string.to_utf_codepoints(g) {
    [base, mark] -> {
      let b = string.utf_codepoint_to_int(base)
      let m = string.utf_codepoint_to_int(mark)
      case precomposed(b, m) {
        Some(ch) -> ch
        None -> g
      }
    }
    _ -> g
  }
}

/// base codepoint + combining mark → precomposed character (Latin-1 subset).
fn precomposed(base: Int, mark: Int) -> Option(String) {
  let b = case string.utf_codepoint(base) {
    Ok(cp) -> string.from_utf_codepoints([cp])
    Error(Nil) -> ""
  }
  case b, mark {
    "a", 0x300 -> Some("à")
    "a", 0x301 -> Some("á")
    "a", 0x302 -> Some("â")
    "a", 0x303 -> Some("ã")
    "a", 0x308 -> Some("ä")
    "a", 0x30a -> Some("å")
    "e", 0x300 -> Some("è")
    "e", 0x301 -> Some("é")
    "e", 0x302 -> Some("ê")
    "e", 0x308 -> Some("ë")
    "i", 0x300 -> Some("ì")
    "i", 0x301 -> Some("í")
    "i", 0x302 -> Some("î")
    "i", 0x308 -> Some("ï")
    "o", 0x300 -> Some("ò")
    "o", 0x301 -> Some("ó")
    "o", 0x302 -> Some("ô")
    "o", 0x303 -> Some("õ")
    "o", 0x308 -> Some("ö")
    "u", 0x300 -> Some("ù")
    "u", 0x301 -> Some("ú")
    "u", 0x302 -> Some("û")
    "u", 0x308 -> Some("ü")
    "n", 0x303 -> Some("ñ")
    "c", 0x327 -> Some("ç")
    "y", 0x301 -> Some("ý")
    "y", 0x308 -> Some("ÿ")
    "A", 0x300 -> Some("À")
    "A", 0x301 -> Some("Á")
    "A", 0x302 -> Some("Â")
    "A", 0x303 -> Some("Ã")
    "A", 0x308 -> Some("Ä")
    "A", 0x30a -> Some("Å")
    "E", 0x301 -> Some("É")
    "I", 0x301 -> Some("Í")
    "O", 0x301 -> Some("Ó")
    "O", 0x303 -> Some("Õ")
    "O", 0x308 -> Some("Ö")
    "U", 0x301 -> Some("Ú")
    "U", 0x308 -> Some("Ü")
    "N", 0x303 -> Some("Ñ")
    "C", 0x327 -> Some("Ç")
    _, _ -> None
  }
}

fn deaccent(g: String) -> String {
  // Graphemes may be base+combining; drop combining marks (U+0300-036F).
  let cps =
    string.to_utf_codepoints(g)
    |> list.filter(fn(cp) {
      let c = string.utf_codepoint_to_int(cp)
      !{ c >= 0x300 && c <= 0x36f }
    })
  let base = string.from_utf_codepoints(cps)
  case base {
    "à" | "á" | "â" | "ã" | "ä" | "å" | "ā" -> "a"
    "è" | "é" | "ê" | "ë" | "ē" -> "e"
    "ì" | "í" | "î" | "ï" -> "i"
    "ò" | "ó" | "ô" | "õ" | "ö" | "ø" -> "o"
    "ù" | "ú" | "û" | "ü" -> "u"
    "ý" | "ÿ" -> "y"
    "ñ" -> "n"
    "ç" -> "c"
    "ß" -> "ss"
    "æ" -> "ae"
    "œ" -> "oe"
    _ -> base
  }
}

/// Swap case so that lowercase sorts before uppercase under byte compare.
fn swap_case(s: String) -> String {
  string.to_utf_codepoints(s)
  |> list.map(fn(cp) {
    let c = string.utf_codepoint_to_int(cp)
    let swapped = case c {
      _ if c >= 0x41 && c <= 0x5a -> c + 32
      _ if c >= 0x61 && c <= 0x7a -> c - 32
      _ -> c
    }
    case string.utf_codepoint(swapped) {
      Ok(v) -> v
      Error(Nil) -> cp
    }
  })
  |> string.from_utf_codepoints
}

fn simple_compare(a: String, b: String) -> Int {
  case string.compare(a, b) {
    order.Lt -> -1
    order.Eq -> 0
    order.Gt -> 1
  }
}

fn numeric_compare(a: List(String), b: List(String)) -> Int {
  case a, b {
    [], [] -> 0
    [], _ -> -1
    _, [] -> 1
    [ca, ..], [cb, ..] -> {
      let da = is_digit_str(ca)
      let db = is_digit_str(cb)
      case da, db {
        True, True -> {
          let #(na, rest_a) = take_digits(a, "")
          let #(nb, rest_b) = take_digits(b, "")
          let ia = int.parse(na) |> result.unwrap(0)
          let ib = int.parse(nb) |> result.unwrap(0)
          case int.compare(ia, ib) {
            order.Eq -> numeric_compare(rest_a, rest_b)
            order.Lt -> -1
            order.Gt -> 1
          }
        }
        _, _ ->
          case
            string.compare(
              option.unwrap(list.first(a) |> option.from_result, ""),
              option.unwrap(list.first(b) |> option.from_result, ""),
            )
          {
            order.Eq -> numeric_compare(list.drop(a, 1), list.drop(b, 1))
            order.Lt -> -1
            order.Gt -> 1
          }
      }
    }
  }
}

fn take_digits(gs: List(String), acc: String) -> #(String, List(String)) {
  case gs {
    [g, ..rest] ->
      case is_digit_str(g) {
        True -> take_digits(rest, acc <> g)
        False -> #(acc, gs)
      }
    [] -> #(acc, [])
  }
}

// ============================================================================
// Prototype methods (IntlMethod dispatch)
// ============================================================================

fn run_method(
  service: IntlService,
  method: String,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let arg0 = first_arg_or_undefined(args)
  let arg1 = helpers.list_at(args, 1) |> option.unwrap(JsUndefined)
  let arg2 = helpers.list_at(args, 2) |> option.unwrap(JsUndefined)
  use <- host_method_guard(service, method, args, this, state, arg0, arg1, arg2)
  run({
    use #(ref, data) <- result.try(branded(
      state,
      this,
      service,
      "Intl." <> service_name(service) <> ".prototype." <> method,
    ))
    case data, method {
      NumberFormatData(nf), "formatToParts" -> {
        use #(parts, state) <- result.try(nf_format_parts(state, nf, arg0))
        let #(state, arr) = parts_to_js(state, parts)
        Ok(#(arr, state))
      }
      NumberFormatData(nf), "formatRange" -> {
        use #(parts, state) <- result.try(nf_range_parts(state, nf, arg0, arg1))
        let s = parts |> list.map(fn(p) { p.1 }) |> string.join("")
        Ok(#(JsString(s), state))
      }
      NumberFormatData(nf), "formatRangeToParts" -> {
        use #(parts, state) <- result.try(nf_range_parts(state, nf, arg0, arg1))
        let #(state, arr) = parts_to_js_sourced(state, parts)
        Ok(#(arr, state))
      }
      DateTimeFormatData(d), "formatToParts" -> {
        use #(parts, state) <- result.try(dtf_format_parts(state, d, arg0))
        let #(state, arr) = parts_to_js(state, parts)
        Ok(#(arr, state))
      }
      DateTimeFormatData(d), "formatRange" -> {
        use #(parts, state) <- result.try(dtf_range_parts(state, d, arg0, arg1))
        let s = parts |> list.map(fn(p) { p.1 }) |> string.join("")
        Ok(#(JsString(s), state))
      }
      DateTimeFormatData(d), "formatRangeToParts" -> {
        use #(parts, state) <- result.try(dtf_range_parts(state, d, arg0, arg1))
        let #(state, arr) = parts_to_js_sourced(state, parts)
        Ok(#(arr, state))
      }
      PluralRulesData(p), "select" -> {
        use #(n, state) <- result.try(coerce.js_to_number(state, arg0))
        Ok(#(JsString(plural_select(p, n)), state))
      }
      PluralRulesData(_), "selectRange" -> {
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
        Ok(#(JsString("other"), state))
      }
      ListFormatData(l), "format" -> {
        use #(items, state) <- result.try(string_list_from_iterable(state, arg0))
        let parts = fmt.list_format_parts(l.list_type, l.style, items)
        Ok(#(JsString(fmt.parts_to_string(parts)), state))
      }
      ListFormatData(l), "formatToParts" -> {
        use #(items, state) <- result.try(string_list_from_iterable(state, arg0))
        let parts = fmt.list_format_parts(l.list_type, l.style, items)
        let #(state, arr) = parts_to_js(state, parts)
        Ok(#(arr, state))
      }
      RelativeTimeFormatData(r), "format" -> {
        use #(parts, state) <- result.try(rtf_method_parts(state, r, arg0, arg1))
        let str = parts |> list.map(fn(part) { part.1 }) |> string.join("")
        Ok(#(JsString(str), state))
      }
      RelativeTimeFormatData(r), "formatToParts" -> {
        use #(parts, state) <- result.try(rtf_method_parts(state, r, arg0, arg1))
        let #(state, arr) = parts_to_js_with_unit(state, parts)
        Ok(#(arr, state))
      }
      DisplayNamesData(dn), "of" -> display_names_of(state, dn, arg0)
      DurationFormatData(df), "format" -> {
        use #(parts, state) <- result.try(duration_parts(state, df, arg0))
        let str = parts |> list.map(fn(part) { part.1 }) |> string.join("")
        Ok(#(JsString(str), state))
      }
      DurationFormatData(df), "formatToParts" -> {
        use #(parts, state) <- result.try(duration_parts(state, df, arg0))
        let #(state, arr) = parts_to_js_with_unit(state, parts)
        Ok(#(arr, state))
      }
      SegmentsData(sg), "containing" -> segments_containing(state, sg, arg0)
      SegmentIteratorData(it), "next" -> segment_iterator_next(state, ref, it)
      _, _ -> throw_type(state, "Unknown Intl method: " <> method)
    }
  })
}

/// Routes the Number/String/Date prototype overrides; falls through to the
/// branded Intl prototype methods otherwise.
fn host_method_guard(
  service: IntlService,
  method: String,
  _args: List(JsValue),
  this: JsValue,
  state: State(host),
  arg0: JsValue,
  arg1: JsValue,
  arg2: JsValue,
  next: fn() -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case service, method {
    IntlNumberFormat, "Number#toLocaleString" ->
      run(host_number_to_locale_string(state, this, arg0, arg1))
    IntlCollator, "String#localeCompare" ->
      run(host_locale_compare(state, this, arg0, arg1, arg2))
    IntlCollator, "String#toLocaleLowerCase" ->
      run(host_locale_case(state, this, arg0, False))
    IntlCollator, "String#toLocaleUpperCase" ->
      run(host_locale_case(state, this, arg0, True))
    IntlDateTimeFormat, "Date#toLocaleString" ->
      run(host_date_to_locale(state, this, arg0, arg1, 3))
    IntlDateTimeFormat, "Date#toLocaleDateString" ->
      run(host_date_to_locale(state, this, arg0, arg1, 1))
    IntlDateTimeFormat, "Date#toLocaleTimeString" ->
      run(host_date_to_locale(state, this, arg0, arg1, 2))
    _, _ -> next()
  }
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
  // Apply locale special casing first, then delegate to the engine's own
  // toLowerCase/toUpperCase (final sigma etc. live there).
  let pre = case lang {
    "tr" | "az" -> turkic_case(s, upper)
    "lt" -> lithuanian_case(s, upper)
    _ -> s
  }
  let method = case upper {
    True -> "toUpperCase"
    False -> "toLowerCase"
  }
  use #(case_fn, state) <- result.try(object.get_value(
    state,
    state.builtins.string.prototype,
    Named(method),
    JsObject(state.builtins.string.prototype),
  ))
  case helpers.is_callable(state.heap, case_fn) {
    True -> state.call(state, case_fn, JsString(pre), [])
    False ->
      Ok(#(
        JsString(case upper {
          True -> string.uppercase(pre)
          False -> string.lowercase(pre)
        }),
        state,
      ))
  }
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
/// `which`: 1 = date, 2 = time, 3 = both.
fn host_date_to_locale(
  state: State(host),
  this: JsValue,
  locales: JsValue,
  options: JsValue,
  which: Int,
) -> Result(#(JsValue, State(host)), Thrown(host)) {
  use tv <- result.try(case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: value.DateObject(time_value: tv), ..)) -> Ok(tv)
        _ -> throw_type(state, "this is not a Date object")
      }
    _ -> throw_type(state, "this is not a Date object")
  })
  let date_defaults = [
    #(DtfYear, "numeric"),
    #(DtfMonth, "numeric"),
    #(DtfDay, "numeric"),
  ]
  let time_defaults = [
    #(DtfHour, "numeric"),
    #(DtfMinute, "numeric"),
    #(DtfSecond, "numeric"),
  ]
  let #(defaults, required) = case which {
    1 -> #(date_defaults, "date")
    2 -> #(time_defaults, "time")
    _ -> #(list.append(date_defaults, time_defaults), "any")
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
fn plural_select(p: PluralRulesState, n: value.JsNum) -> String {
  case n {
    value.Finite(f) -> {
      let opts =
        fmt.NumOpts(
          ..num_opts_from_plural(p),
          style: "decimal",
          use_grouping: "never",
          sign_display: "never",
        )
      let parts = fmt.format_number_parts(opts, f)
      let int_digits =
        parts
        |> list.filter_map(fn(p) {
          case p {
            #("integer", v) -> Ok(v)
            _ -> Error(Nil)
          }
        })
        |> string.join("")
      let frac_digits =
        parts
        |> list.filter_map(fn(p) {
          case p {
            #("fraction", v) -> Ok(v)
            _ -> Error(Nil)
          }
        })
        |> string.join("")
      fmt.plural_select_en(p.plural_type, int_digits, frac_digits, f <. 0.0)
    }
    _ -> "other"
  }
}

/// RelativeTimeFormat format/formatToParts core.
fn rtf_method_parts(
  state: State(host),
  r: RelativeTimeFormatState,
  value_v: JsValue,
  unit_v: JsValue,
) -> Result(#(List(#(String, String, String)), State(host)), Thrown(host)) {
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
  let abs_opts = fmt.NumOpts(..fmt.default_num_opts(), sign_display: "never")
  let value_parts = fmt.format_number_parts(abs_opts, float.absolute_value(f))
  let value_parts = apply_numbering_system(value_parts, r.numbering_system)
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
    "language" ->
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
    "region" ->
      case is_region_subtag(code) {
        True -> {
          let r = string.uppercase(code)
          Ok(#(r, fmt.region_display_name(r)))
        }
        False -> throw_range(state, "invalid region code: " <> code)
      }
    "script" ->
      case is_script_subtag(code) {
        True -> {
          let s = titlecase_ascii(code)
          Ok(#(s, fmt.script_display_name(s)))
        }
        False -> throw_range(state, "invalid script code: " <> code)
      }
    "currency" ->
      case is_alpha_str(code) && string.length(code) == 3 {
        True -> {
          let c = string.uppercase(code)
          Ok(#(c, fmt.currency_display_name(c)))
        }
        False -> throw_range(state, "invalid currency code: " <> code)
      }
    "calendar" ->
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
    // dateTimeField
    _ ->
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
    None, "code" -> Ok(#(JsString(canonical), state))
    None, _ -> Ok(#(JsUndefined, state))
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
) -> Result(#(List(#(String, String, String)), State(host)), Thrown(host)) {
  use #(fields, state) <- result.try(to_duration_record(state, duration_v))
  // DurationSign consistency + IsValidDuration ranges.
  let has_neg = list.any(fields, fn(f) { f.1 <. 0.0 })
  let has_pos = list.any(fields, fn(f) { f.1 >. 0.0 })
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
) -> Result(#(List(#(String, Float)), State(host)), Thrown(host)) {
  case duration_v {
    JsString(str) ->
      case parse_iso_duration(str) {
        Ok(fields) -> Ok(#(fields, state))
        Error(Nil) -> throw_range(state, "Invalid duration string: " <> str)
      }
    JsObject(dur_ref) -> {
      use acc <- result.try(
        list.try_fold(duration_units, #([], state, False), fn(acc, unit) {
          let #(fields, state, any) = acc
          use #(v, state) <- result.try(object.get_value(
            state,
            dur_ref,
            Named(unit),
            duration_v,
          ))
          case v {
            JsUndefined -> Ok(#([#(unit, 0.0), ..fields], state, any))
            _ -> {
              use #(n, state) <- result.try(coerce.js_to_number(state, v))
              case n {
                value.Finite(f) ->
                  case f == float.floor(f) {
                    True -> Ok(#([#(unit, f), ..fields], state, True))
                    False ->
                      throw_range(state, unit <> " must be an integral number")
                  }
                _ -> throw_range(state, unit <> " must be a finite number")
              }
            }
          }
        }),
      )
      let #(fields_rev, state, any_defined) = acc
      use Nil <- result.try(case any_defined {
        True -> Ok(Nil)
        False -> throw_range(state, "Invalid duration object")
      })
      Ok(#(list.reverse(fields_rev), state))
    }
    _ -> throw_type(state, "Duration must be an object or string")
  }
}

/// IsValidDuration: calendar units < 2^32; total time < 2^53 seconds.
fn is_valid_duration(fields: List(#(String, Float))) -> Bool {
  let get = fn(name) { list.key_find(fields, name) |> result.unwrap(0.0) }
  let cal_ok =
    list.all(["years", "months", "weeks"], fn(u) {
      float.absolute_value(get(u)) <. 4_294_967_296.0
    })
  let total_seconds =
    get("days")
    *. 86_400.0
    +. get("hours")
    *. 3600.0
    +. get("minutes")
    *. 60.0
    +. get("seconds")
    +. get("milliseconds")
    /. 1000.0
    +. get("microseconds")
    /. 1_000_000.0
    +. get("nanoseconds")
    /. 1_000_000_000.0
  cal_ok && float.absolute_value(total_seconds) <. 9_007_199_254_740_992.0
}

/// Parse a Temporal ISO 8601 duration string: [+-]PnYnMnWnDTnHnMnS.
fn parse_iso_duration(str: String) -> Result(List(#(String, Float)), Nil) {
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
    [#("Y", "years"), #("M", "months"), #("W", "weeks"), #("D", "days")],
    False,
  ))
  use time_fields <- result.try(case time_part {
    None -> Ok([])
    Some("") -> Error(Nil)
    Some(t) ->
      parse_duration_section(
        t,
        [#("H", "hours"), #("M", "minutes"), #("S", "seconds")],
        True,
      )
  })
  let all = list.append(date_fields, time_fields)
  case all {
    [] -> Error(Nil)
    _ -> {
      let lookup = fn(name) { list.key_find(all, name) |> result.unwrap(0.0) }
      // Split fractional seconds into ms/us/ns.
      let secs = lookup("seconds")
      let whole = float.truncate(secs) |> int.to_float
      let frac = secs -. whole
      let ns_total = float.round(frac *. 1_000_000_000.0)
      let ms = ns_total / 1_000_000
      let us = { ns_total % 1_000_000 } / 1000
      let ns = ns_total % 1000
      Ok(
        list.map(
          [
            #("years", lookup("years")),
            #("months", lookup("months")),
            #("weeks", lookup("weeks")),
            #("days", lookup("days")),
            #("hours", lookup("hours")),
            #("minutes", lookup("minutes")),
            #("seconds", whole),
            #("milliseconds", int.to_float(ms)),
            #("microseconds", int.to_float(us)),
            #("nanoseconds", int.to_float(ns)),
          ],
          fn(kv) { #(kv.0, sign *. kv.1) },
        ),
      )
    }
  }
}

/// Parse "3Y2M..." style segments in designator order.
fn parse_duration_section(
  part: String,
  designators: List(#(String, String)),
  allow_fraction: Bool,
) -> Result(List(#(String, Float)), Nil) {
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
  designators: List(#(String, String)),
  allow_fraction: Bool,
  num_acc: String,
  out: List(#(String, Float)),
) -> Result(List(#(String, Float)), Nil) {
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
  designators: List(#(String, String)),
  d: String,
) -> Result(#(String, List(#(String, String))), Nil) {
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
  float.parse(s)
  |> result.lazy_or(fn() { int.parse(s) |> result.map(int.to_float) })
}

/// PartitionDurationFormatPattern — mirrors ECMA-402 Intl.DurationFormat §1.1.7.
fn build_duration_parts(
  df: DurationFormatState,
  fields: List(#(String, Float)),
) -> List(#(String, String, String)) {
  let nu = df.numbering_system
  let base_style = df.style
  let frac_digits = df.fractional_digits
  let get = fn(name) { list.key_find(fields, name) |> result.unwrap(0.0) }
  let overall_negative = list.any(fields, fn(f) { f.1 <. 0.0 })
  let fields = list.map(fields, fn(f) { #(f.0, f.1 +. 0.0) })
  // The style of the next-smaller sub-second unit (seconds → ms → us → ns).
  let next_style_of = fn(unit) {
    case unit {
      "seconds" -> df.milliseconds.style
      "milliseconds" -> df.microseconds.style
      "microseconds" -> df.nanoseconds.style
      _ -> ""
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
          let raw_value = get(unit)
          // Combine sub-second units when the next unit is numeric.
          let next_style = next_style_of(unit)
          let raw_value = raw_value +. 0.0
          let combine =
            {
              unit == "seconds"
              || unit == "milliseconds"
              || unit == "microseconds"
            }
            && next_style == "numeric"
          let #(value_repr, is_zero, this_done, max_frac, min_frac, trunc_mode) = case
            combine
          {
            True -> {
              let #(repr, zero) = duration_fractional_value(fields, unit)
              #(
                repr,
                zero,
                True,
                Some(option.unwrap(frac_digits, 9)),
                Some(option.unwrap(frac_digits, 0)),
                True,
              )
            }
            False -> #(
              FloatValue(raw_value),
              raw_value == 0.0,
              False,
              None,
              None,
              False,
            )
          }
          // Display zero numeric minutes when seconds follow.
          let display_required = case unit == "minutes" && need_sep {
            True ->
              df.seconds.display == "always"
              || get("seconds") != 0.0
              || get("milliseconds") != 0.0
              || get("microseconds") != 0.0
              || get("nanoseconds") != 0.0
            False -> False
          }
          let show = !is_zero || display != "auto" || display_required
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
                  #("auto", value_repr, False)
                }
                False -> #("never", value_repr, False)
              }
              let numeric_style = style == "numeric" || style == "2-digit"
              let opts =
                fmt.NumOpts(
                  ..fmt.default_num_opts(),
                  sign_display: sign_display,
                  min_int: case style == "2-digit" {
                    True -> 2
                    False -> 1
                  },
                  use_grouping: case numeric_style {
                    True -> "never"
                    False -> "auto"
                  },
                  min_frac: case min_frac {
                    Some(_) -> min_frac
                    None -> Some(0)
                  },
                  max_frac: case max_frac {
                    Some(_) -> max_frac
                    None -> Some(0)
                  },
                  rounding_mode: case trunc_mode {
                    True -> "trunc"
                    False -> "halfExpand"
                  },
                  style: case numeric_style {
                    True -> "decimal"
                    False -> "unit"
                  },
                  unit: case numeric_style {
                    True -> None
                    False -> Some(singular_duration_unit(unit))
                  },
                  unit_display: case numeric_style {
                    True -> "short"
                    False -> style
                  },
                )
              let parts = case value_repr {
                FloatValue(f) -> fmt.format_number_parts(opts, f)
                DecValue(str) -> fmt.format_decimal_string_parts(opts, str)
              }
              let unit_tag = singular_duration_unit(unit)
              let parts =
                apply_numbering_system(parts, nu)
                |> list.map(fn(part) {
                  let #(t, v) = part
                  case t {
                    "literal" -> #(t, v, "")
                    _ -> #(t, v, unit_tag)
                  }
                })
              case need_sep {
                True ->
                  // Join onto the previous numeric group with ":".
                  case groups {
                    [last, ..earlier] -> #(
                      [
                        list.flatten([last, [#("literal", ":", "")], parts]),
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
  let strings =
    list.map(groups, fn(g) {
      g |> list.map(fn(part) { part.1 }) |> string.join("")
    })
  let list_style = case base_style {
    "digital" -> "short"
    other -> other
  }
  // Re-expand element parts so formatToParts keeps the numeric structure.
  let lf_parts = fmt.list_format_parts("unit", list_style, strings)
  expand_list_elements(lf_parts, groups, [])
}

type DurationValue {
  FloatValue(Float)
  DecValue(String)
}

fn singular_duration_unit(unit: String) -> String {
  string.slice(unit, 0, string.length(unit) - 1)
}

/// durationToFractional: exact decimal string for combined sub-second units.
fn duration_fractional_value(
  fields: List(#(String, Float)),
  unit: String,
) -> #(DurationValue, Bool) {
  let get = fn(name) {
    list.key_find(fields, name)
    |> result.unwrap(0.0)
    |> float.truncate
  }
  let #(exponent, components) = case unit {
    "seconds" -> #(9, [
      #(get("seconds"), 1_000_000_000),
      #(get("milliseconds"), 1_000_000),
      #(get("microseconds"), 1000),
      #(get("nanoseconds"), 1),
    ])
    "milliseconds" -> #(6, [
      #(get("milliseconds"), 1_000_000),
      #(get("microseconds"), 1000),
      #(get("nanoseconds"), 1),
    ])
    _ -> #(3, [#(get("microseconds"), 1000), #(get("nanoseconds"), 1)])
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
  groups: List(List(#(String, String, String))),
  acc: List(List(#(String, String, String))),
) -> List(#(String, String, String)) {
  case lf_parts {
    [] -> list.flatten(list.reverse(acc))
    [#("element", _), ..rest] ->
      case groups {
        [g, ..gs] -> expand_list_elements(rest, gs, [g, ..acc])
        [] -> expand_list_elements(rest, [], acc)
      }
    [#(t, v), ..rest] ->
      expand_list_elements(rest, groups, [[#(t, v, "")], ..acc])
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
    use #(_ref, data) <- result.try(branded(state, this, IntlSegmenter, name))
    use sg <- result.try(case data {
      SegmenterData(sg) -> Ok(sg)
      _ -> throw_type(state, name <> " called on incompatible receiver")
    })
    use #(s, state) <- result.try(coerce.js_to_string(
      state,
      first_arg_or_undefined(args),
    ))
    let seg =
      SegmentsData(SegmentsState(string: s, granularity: sg.granularity))
    let #(heap, ref) =
      common.alloc_wrapper(
        state.heap,
        IntlObject(service: IntlSegments, data: seg),
        segments_proto,
      )
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
    use #(_ref, data) <- result.try(branded(state, this, IntlSegments, name))
    use sg <- result.try(case data {
      SegmentsData(sg) -> Ok(sg)
      _ -> throw_type(state, name <> " called on incompatible receiver")
    })
    let iter =
      SegmentIteratorData(SegmentIteratorState(
        string: sg.string,
        granularity: sg.granularity,
        position: 0,
      ))
    let #(heap, ref) =
      common.alloc_wrapper(
        state.heap,
        IntlObject(service: IntlSegmentIterator, data: iter),
        iter_proto,
      )
    Ok(#(JsObject(ref), State(..state, heap:)))
  })
}

fn make_segment_data(
  state: State(host),
  input: String,
  granularity: String,
  segment: String,
  index: Int,
  word_like: Bool,
) -> #(State(host), JsValue) {
  let base = [
    #("segment", JsString(segment)),
    #("index", value.from_int(index)),
    #("input", JsString(input)),
  ]
  let props = case granularity {
    "word" -> list.append(base, [#("isWordLike", JsBool(word_like))])
    _ -> base
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
        list.fold(segments, None, fn(acc, seg) {
          let #(_s, start, _wl) = seg
          case start <= idx {
            True -> Some(seg)
            False -> acc
          }
        })
      case found {
        Some(#(s, start, wl)) -> {
          let #(state, obj) =
            make_segment_data(state, input, granularity, s, start, wl)
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
    list.find(segments, fn(seg) {
      let #(_s, start, _wl) = seg
      start >= position
    })
  case next_seg {
    Error(Nil) -> {
      let #(state, res) = iter_result(state, JsUndefined, True)
      Ok(#(res, state))
    }
    Ok(#(s, start, wl)) -> {
      let new_pos = start + fmt.utf16_len(s)
      let heap =
        write_intl_data(
          state.heap,
          ref,
          SegmentIteratorData(SegmentIteratorState(..it, position: new_pos)),
        )
      let state = State(..state, heap:)
      let #(state, data) =
        make_segment_data(state, input, granularity, s, start, wl)
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
  name: String,
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  run({
    use #(_ref, l) <- result.try(branded_locale(
      state,
      this,
      "Intl.Locale.prototype." <> name,
    ))
    let lid = locale_lid(l)
    let v = case name {
      "baseName" ->
        case lid {
          Some(l) -> JsString(tags.base_name(l))
          None -> JsUndefined
        }
      "language" ->
        case lid {
          Some(l) -> JsString(string.lowercase(l.language))
          None -> JsUndefined
        }
      "script" ->
        case lid {
          Some(tags.LocaleId(script: Some(s), ..)) ->
            JsString(titlecase_ascii(s))
          _ -> JsUndefined
        }
      "region" ->
        case lid {
          Some(tags.LocaleId(region: Some(r), ..)) ->
            JsString(string.uppercase(r))
          _ -> JsUndefined
        }
      "calendar" ->
        locale_u_kw(l, "ca")
        |> option.map(JsString)
        |> option.unwrap(JsUndefined)
      "collation" ->
        locale_u_kw(l, "co")
        |> option.map(JsString)
        |> option.unwrap(JsUndefined)
      "hourCycle" ->
        locale_u_kw(l, "hc")
        |> option.map(JsString)
        |> option.unwrap(JsUndefined)
      "numberingSystem" ->
        locale_u_kw(l, "nu")
        |> option.map(JsString)
        |> option.unwrap(JsUndefined)
      "caseFirst" ->
        locale_u_kw(l, "kf")
        |> option.map(JsString)
        |> option.unwrap(JsUndefined)
      "numeric" ->
        case locale_u_kw(l, "kn") {
          Some("") | Some("true") -> JsBool(True)
          Some(_) -> JsBool(False)
          None -> JsBool(False)
        }
      "firstDayOfWeek" ->
        locale_u_kw(l, "fw")
        |> option.map(JsString)
        |> option.unwrap(JsUndefined)
      "variants" ->
        case lid {
          Some(tags.LocaleId(variants: [_, ..] as vs, ..)) ->
            JsString(string.join(vs, "-"))
          _ -> JsUndefined
        }
      _ -> JsUndefined
    }
    Ok(#(v, state))
  })
}

fn locale_method(
  method: String,
  proto: Ref,
  _args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  run({
    use #(_ref, l) <- result.try(branded_locale(
      state,
      this,
      "Intl.Locale.prototype." <> method,
    ))
    let tag = l.locale
    case method {
      "toString" -> Ok(#(JsString(tag), state))
      "maximize" | "minimize" -> {
        let new_tag = case tags.parse(tag) {
          Ok(lid) ->
            case method {
              "maximize" -> tags.to_string(tags.maximize(lid))
              _ -> tags.to_string(tags.minimize(lid))
            }
          Error(Nil) -> tag
        }
        let #(heap, ref) =
          common.alloc_wrapper(
            state.heap,
            IntlObject(
              service: IntlLocale,
              data: LocaleData(LocaleState(locale: new_tag)),
            ),
            proto,
          )
        Ok(#(JsObject(ref), State(..state, heap:)))
      }
      "getCalendars" -> {
        let vals = case locale_u_kw(l, "ca") {
          Some(ca) -> [ca]
          None -> ["gregory"]
        }
        let #(state, arr) = alloc_array(state, list.map(vals, JsString))
        Ok(#(arr, state))
      }
      "getCollations" -> {
        let vals = case locale_u_kw(l, "co") {
          Some(co) -> [co]
          None -> ["emoji", "eor"]
        }
        let #(state, arr) = alloc_array(state, list.map(vals, JsString))
        Ok(#(arr, state))
      }
      "getHourCycles" -> {
        let vals = case locale_u_kw(l, "hc") {
          Some(hc) -> [hc]
          None -> ["h12"]
        }
        let #(state, arr) = alloc_array(state, list.map(vals, JsString))
        Ok(#(arr, state))
      }
      "getNumberingSystems" -> {
        let vals = case locale_u_kw(l, "nu") {
          Some(nu) -> [nu]
          None -> ["latn"]
        }
        let #(state, arr) = alloc_array(state, list.map(vals, JsString))
        Ok(#(arr, state))
      }
      "getTimeZones" ->
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
      "getTextInfo" -> {
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
      "getWeekInfo" -> {
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
      _ -> throw_type(state, "Unknown Intl.Locale method: " <> method)
    }
  })
}
