//// The Intl namespace (ECMA-402): Intl.getCanonicalLocales,
//// Intl.supportedValuesOf, and the service constructors
//// (Locale, Collator, NumberFormat, DateTimeFormat, PluralRules, ListFormat,
//// RelativeTimeFormat, Segmenter, DisplayNames, DurationFormat).
////
//// Locale data is root/English: formatters implement en/en-US CLDR patterns
//// (see intl_format.gleam); tag parsing/canonicalization is in
//// intl_locale.gleam.

import arc/vm/builtins/common
import arc/vm/builtins/helpers.{first_arg_or_undefined}
import arc/vm/builtins/intl_format as fmt
import arc/vm/builtins/intl_locale as tags
import arc/vm/heap
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type IntlNativeFn, type IntlService, type JsValue, type Ref, Dispatch,
  IntlBoundGetter, IntlBoundMethod, IntlCollator, IntlConstructor,
  IntlDateTimeFormat, IntlDisplayNames, IntlDurationFormat,
  IntlGetCanonicalLocales, IntlListFormat, IntlLocale, IntlLocaleGetter,
  IntlLocaleMethod, IntlMethod, IntlNative, IntlNumberFormat, IntlObject,
  IntlPluralRules, IntlRelativeTimeFormat, IntlResolvedOptions,
  IntlSegmentIterator, IntlSegmenter, IntlSegmenterSegment, IntlSegments,
  IntlSegmentsIterator, IntlSupportedLocalesOf, IntlSupportedValuesOf, JsBool,
  JsNumber, JsObject, JsString, JsUndefined, Named, ObjectSlot, TemporalDateSlot,
  TemporalDateTimeSlot, TemporalInstantSlot, TemporalMonthDaySlot,
  TemporalTimeSlot, TemporalYearMonthSlot, TemporalZonedDateTimeSlot,
}
import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/string

type Thrown =
  #(JsValue, State)

@external(erlang, "arc_date_ffi", "now_ms")
fn now_ms() -> Float

@external(erlang, "arc_date_ffi", "tz_offset_minutes")
fn ffi_tz_offset_minutes(epoch_ms: Int) -> Int

// ============================================================================
// Initialization
// ============================================================================

/// All the rooted refs other parts of the engine may need.
pub type IntlBuiltin {
  IntlBuiltin(namespace: Ref)
}

pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
  number_proto: Ref,
  string_proto: Ref,
  date_proto: Ref,
) -> #(Heap, IntlBuiltin) {
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
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
  service: IntlService,
  name: String,
  methods: List(#(String, value.NativeFn, Int)),
  accessors: List(#(String, value.NativeFn)),
) -> #(Heap, common.BuiltinType) {
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
fn freeze_prototype_prop(h: Heap, ctor: Ref, proto: Ref) -> Heap {
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
  h: Heap,
  ref: Ref,
  props: List(#(String, value.Property)),
) -> Heap {
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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
  r: Result(#(JsValue, State), Thrown),
) -> #(State, Result(JsValue, JsValue)) {
  case r {
    Ok(#(v, state)) -> #(state, Ok(v))
    Error(#(t, state)) -> #(state, Error(t))
  }
}

fn throw_range(state: State, msg: String) -> Result(a, Thrown) {
  let #(state, res) = state.range_error(state, msg)
  case res {
    Error(e) -> Error(#(e, state))
    Ok(v) -> Error(#(v, state))
  }
}

fn throw_type(state: State, msg: String) -> Result(a, Thrown) {
  let #(state, res) = state.type_error(state, msg)
  case res {
    Error(e) -> Error(#(e, state))
    Ok(v) -> Error(#(v, state))
  }
}

/// Read the IntlObject slots for `this`, throwing TypeError on brand mismatch.
fn branded(
  state: State,
  this: JsValue,
  service: IntlService,
  method: String,
) -> Result(#(Ref, Dict(String, JsValue)), Thrown) {
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: IntlObject(service: s, slots:), ..))
          if s == service
        -> Ok(#(ref, slots))
        _ -> throw_type(state, method <> " called on incompatible receiver")
      }
    _ -> throw_type(state, method <> " called on incompatible receiver")
  }
}

fn slot_str(slots: Dict(String, JsValue), key: String) -> Option(String) {
  case dict.get(slots, key) {
    Ok(JsString(s)) -> Some(s)
    Ok(_) -> None
    Error(Nil) -> None
  }
}

fn slot_int(slots: Dict(String, JsValue), key: String) -> Option(Int) {
  case dict.get(slots, key) {
    Ok(JsNumber(value.Finite(f))) -> Some(float.truncate(f))
    _ -> None
  }
}

fn slot_bool(slots: Dict(String, JsValue), key: String) -> Option(Bool) {
  case dict.get(slots, key) {
    Ok(JsBool(b)) -> Some(b)
    _ -> None
  }
}

fn alloc_array(state: State, values: List(JsValue)) -> #(State, JsValue) {
  let #(heap, ref) =
    common.alloc_array(state.heap, values, state.builtins.array.prototype)
  #(State(..state, heap:), JsObject(ref))
}

fn alloc_pojo(
  state: State,
  props: List(#(String, JsValue)),
) -> #(State, JsValue) {
  let #(heap, ref) =
    common.alloc_pojo(
      state.heap,
      state.builtins.object.prototype,
      list.map(props, fn(p) { #(p.0, value.data_property(p.1)) }),
    )
  #(State(..state, heap:), JsObject(ref))
}

/// Parts → JS array of `{ type, value }` objects.
fn parts_to_js(state: State, parts: List(fmt.Part)) -> #(State, JsValue) {
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
  state: State,
  parts: List(#(String, String, String)),
) -> #(State, JsValue) {
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
  state: State,
  parts: List(#(String, String, String)),
) -> #(State, JsValue) {
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
  state: State,
  v: JsValue,
) -> Result(#(Option(Ref), State), Thrown) {
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
  state: State,
  v: JsValue,
) -> Result(#(Option(Ref), State), Thrown) {
  case v {
    JsUndefined -> Ok(#(None, state))
    JsObject(ref) -> Ok(#(Some(ref), state))
    _ -> throw_type(state, "options must be an object or undefined")
  }
}

fn opt_get(
  state: State,
  opts: Option(Ref),
  name: String,
) -> Result(#(JsValue, State), Thrown) {
  case opts {
    None -> Ok(#(JsUndefined, state))
    Some(ref) -> object.get_value(state, ref, Named(name), JsObject(ref))
  }
}

/// GetOption with type string. Empty `allowed` list = any string allowed.
fn get_str_opt(
  state: State,
  opts: Option(Ref),
  name: String,
  allowed: List(String),
  default: Option(String),
) -> Result(#(Option(String), State), Thrown) {
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
  state: State,
  opts: Option(Ref),
  name: String,
  default: Option(Bool),
) -> Result(#(Option(Bool), State), Thrown) {
  use #(v, state) <- result.try(opt_get(state, opts, name))
  case v {
    JsUndefined -> Ok(#(default, state))
    _ -> Ok(#(Some(value.is_truthy(v)), state))
  }
}

/// GetNumberOption/DefaultNumberOption (§9.2.16/9.2.17).
fn get_num_opt(
  state: State,
  opts: Option(Ref),
  name: String,
  min: Int,
  max: Int,
  default: Option(Int),
) -> Result(#(Option(Int), State), Thrown) {
  use #(v, state) <- result.try(opt_get(state, opts, name))
  default_number_option(state, v, min, max, default, name)
}

fn default_number_option(
  state: State,
  v: JsValue,
  min: Int,
  max: Int,
  default: Option(Int),
  name: String,
) -> Result(#(Option(Int), State), Thrown) {
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
  state: State,
  locales: JsValue,
) -> Result(#(List(String), State), Thrown) {
  case locales {
    JsUndefined -> Ok(#([], state))
    JsString(s) -> {
      use #(tag, state) <- result.try(canonical_tag_or_throw(state, s))
      Ok(#([tag], state))
    }
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: IntlObject(service: IntlLocale, slots:), ..)) ->
          case slot_str(slots, "locale") {
            Some(tag) -> Ok(#([tag], state))
            None -> Ok(#([], state))
          }
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
  state: State,
  s: String,
) -> Result(#(String, State), Thrown) {
  case tags.canonicalize_tag(s) {
    Ok(tag) -> Ok(#(tag, state))
    Error(Nil) ->
      throw_range(state, "Incorrect locale information provided: " <> s)
  }
}

fn locale_list_from_object(
  state: State,
  ref: Ref,
) -> Result(#(List(String), State), Thrown) {
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
  state: State,
  ref: Ref,
  k: Int,
  len: Int,
  seen: List(String),
) -> Result(#(List(String), State), Thrown) {
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
                Some(ObjectSlot(
                  kind: IntlObject(service: IntlLocale, slots:),
                  ..,
                )) -> Ok(#(option.unwrap(slot_str(slots, "locale"), ""), state))
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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
  case tags.parse(tag) {
    Ok(lid) ->
      lid.extensions
      |> list.filter_map(fn(ext) {
        case ext {
          tags.UExt(keywords:, ..) -> Ok(keywords)
          _ -> Error(Nil)
        }
      })
      |> list.flatten
    Error(Nil) -> []
  }
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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
        use #(slots, state) <- result.try(case service {
          IntlLocale -> locale_slots(state, arg0, arg1)
          IntlCollator -> collator_slots(state, arg0, arg1)
          IntlNumberFormat -> number_format_slots(state, arg0, arg1)
          IntlDateTimeFormat -> date_time_format_slots(state, arg0, arg1)
          IntlPluralRules -> plural_rules_slots(state, arg0, arg1)
          IntlListFormat -> list_format_slots(state, arg0, arg1)
          IntlRelativeTimeFormat -> rtf_slots(state, arg0, arg1)
          IntlSegmenter -> segmenter_slots(state, arg0, arg1)
          IntlDisplayNames -> display_names_slots(state, arg0, arg1)
          IntlDurationFormat -> duration_format_slots(state, arg0, arg1)
          IntlSegments | IntlSegmentIterator ->
            throw_type(state, "Illegal constructor")
        })
        let #(heap, ref) =
          common.alloc_wrapper(state.heap, IntlObject(service:, slots:), proto)
        Ok(#(JsObject(ref), State(..state, heap:)))
      })
    }
  }
}

// --- Intl.Locale ---

fn locale_slots(
  state: State,
  tag_v: JsValue,
  options_v: JsValue,
) -> Result(#(Dict(String, JsValue), State), Thrown) {
  // Step 7-9: tag must be String or Object; Locale objects pass [[Locale]].
  use #(tag_str, state) <- result.try(case tag_v {
    JsString(s) -> Ok(#(s, state))
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: IntlObject(service: IntlLocale, slots:), ..)) ->
          Ok(#(option.unwrap(slot_str(slots, "locale"), "und"), state))
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
  Ok(#(dict.from_list([#("locale", JsString(canonical))]), state))
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
  state: State,
  v: Option(String),
  name: String,
) -> Result(Nil, Thrown) {
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
  state: State,
  opts: Option(Ref),
) -> Result(State, Thrown) {
  use #(_matcher, state) <- result.map(get_str_opt(
    state,
    opts,
    "localeMatcher",
    ["lookup", "best fit"],
    Some("best fit"),
  ))
  state
}

/// Shared *_slots constructor prologue: CanonicalizeLocaleList, options
/// coercion (GetOptionsObject when `strict`, else CoerceOptionsToObject),
/// then the localeMatcher read.
fn slots_prologue(
  state: State,
  locales_v: JsValue,
  options_v: JsValue,
  strict strict: Bool,
) -> Result(#(List(String), Option(Ref), State), Thrown) {
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
  state: State,
  opts: Option(Ref),
  requested: List(String),
) -> Result(#(String, String, State), Thrown) {
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

fn collator_slots(
  state: State,
  locales_v: JsValue,
  options_v: JsValue,
) -> Result(#(Dict(String, JsValue), State), Thrown) {
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
  let slots =
    dict.from_list([
      #("locale", JsString(locale)),
      #("usage", JsString(option.unwrap(usage, "sort"))),
      #("sensitivity", JsString(sensitivity)),
      #("ignorePunctuation", JsBool(option.unwrap(ignore_punct, False))),
      #("collation", JsString(collation)),
      #("numeric", JsBool(numeric)),
      #("caseFirst", JsString(case_first)),
    ])
  Ok(#(slots, state))
}

// --- Intl.NumberFormat ---

fn number_format_slots(
  state: State,
  locales_v: JsValue,
  options_v: JsValue,
) -> Result(#(Dict(String, JsValue), State), Thrown) {
  use #(requested, opts, state) <- result.try(slots_prologue(
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
  use #(digit_slots, state) <- result.try(digit_options(
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
        JsString(case notation {
          "compact" -> "min2"
          _ -> "auto"
        }),
        state,
      ))
    JsBool(False) -> Ok(#(JsBool(False), state))
    JsBool(True) -> Ok(#(JsString("always"), state))
    _ -> {
      use #(s, state) <- result.try(coerce.js_to_string(state, grouping_v))
      case s {
        "min2" | "auto" | "always" -> Ok(#(JsString(s), state))
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
  let base = [
    #("locale", JsString(locale)),
    #("numberingSystem", JsString(nu)),
    #("style", JsString(style)),
    #("useGrouping", use_grouping),
    #("notation", JsString(notation)),
    #("signDisplay", JsString(option.unwrap(sign_display, "auto"))),
  ]
  let extras =
    list.filter_map(
      [
        #("currency", option.map(currency, JsString)),
        #("currencyDisplay", case style {
          "currency" -> option.map(currency_display, JsString)
          _ -> None
        }),
        #("currencySign", case style {
          "currency" -> option.map(currency_sign, JsString)
          _ -> None
        }),
        #("unit", option.map(unit, JsString)),
        #("unitDisplay", case style {
          "unit" -> option.map(unit_display, JsString)
          _ -> None
        }),
        #("compactDisplay", case notation {
          "compact" -> option.map(compact_display, JsString)
          _ -> None
        }),
      ],
      fn(kv) {
        case kv {
          #(k, Some(v)) -> Ok(#(k, v))
          #(_, None) -> Error(Nil)
        }
      },
    )
  let slots =
    dict.from_list(list.flatten([base, extras, dict.to_list(digit_slots)]))
  Ok(#(slots, state))
}

/// SetNumberFormatDigitOptions (§15.1.6) — returns the digit-related slots.
fn digit_options(
  state: State,
  opts: Option(Ref),
  mnfd_default: Int,
  mxfd_default: Int,
  notation: String,
) -> Result(#(Dict(String, JsValue), State), Thrown) {
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
  use #(sig_slots, state) <- result.try(case need_sd {
    False -> Ok(#([], state))
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
          Ok(#(
            [
              #("minimumSignificantDigits", value.from_int(mnsd)),
              #("maximumSignificantDigits", value.from_int(mxsd)),
            ],
            state,
          ))
        }
        False ->
          Ok(#(
            [
              #("minimumSignificantDigits", value.from_int(1)),
              #("maximumSignificantDigits", value.from_int(21)),
            ],
            state,
          ))
      }
  })
  use #(fd_slots, state) <- result.try(case need_fd {
    False -> Ok(#([], state))
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
          Ok(#(
            [
              #("minimumFractionDigits", value.from_int(mnfd)),
              #("maximumFractionDigits", value.from_int(mxfd)),
            ],
            state,
          ))
        }
        False ->
          Ok(#(
            [
              #("minimumFractionDigits", value.from_int(mnfd_default)),
              #(
                "maximumFractionDigits",
                value.from_int(int.max(mxfd_default, mnfd_default)),
              ),
            ],
            state,
          ))
      }
  })
  // Neither kind requested (compact notation default): more-precision
  // rounding with mnfd/mxfd = 0 and mnsd/mxsd = 1..2 (§15.1.6 step 16).
  let #(sig_slots, fd_slots, rounding_priority) = case sig_slots, fd_slots {
    [], [] -> #(
      [
        #("minimumSignificantDigits", value.from_int(1)),
        #("maximumSignificantDigits", value.from_int(2)),
      ],
      [
        #("minimumFractionDigits", value.from_int(0)),
        #("maximumFractionDigits", value.from_int(0)),
      ],
      "morePrecision",
    )
    _, _ -> #(sig_slots, fd_slots, rounding_priority)
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
          case
            list.key_find(fd_slots, "minimumFractionDigits")
            == list.key_find(fd_slots, "maximumFractionDigits")
          {
            True -> Ok(Nil)
            False ->
              throw_range(
                state,
                "roundingIncrement requires minimumFractionDigits equal to maximumFractionDigits",
              )
          }
      }
  })
  let slots =
    dict.from_list(
      list.flatten([
        [
          #("minimumIntegerDigits", value.from_int(option.unwrap(mnid, 1))),
          #("roundingIncrement", value.from_int(rounding_increment)),
          #(
            "roundingMode",
            JsString(option.unwrap(rounding_mode, "halfExpand")),
          ),
          #("roundingPriority", JsString(rounding_priority)),
          #(
            "trailingZeroDisplay",
            JsString(option.unwrap(trailing_zero, "auto")),
          ),
        ],
        sig_slots,
        fd_slots,
      ]),
    )
  Ok(#(slots, state))
}

// --- Intl.DateTimeFormat ---

fn date_time_format_slots(
  state: State,
  locales_v: JsValue,
  options_v: JsValue,
) -> Result(#(Dict(String, JsValue), State), Thrown) {
  dtf_slots_with_defaults(state, locales_v, options_v, [
    #("c:year", "numeric"),
    #("c:month", "numeric"),
    #("c:day", "numeric"),
  ])
}

fn dtf_slots_with_defaults(
  state: State,
  locales_v: JsValue,
  options_v: JsValue,
  default_components: List(#(String, String)),
) -> Result(#(Dict(String, JsValue), State), Thrown) {
  dtf_slots_required(state, locales_v, options_v, default_components, "any")
}

fn dtf_slots_required(
  state: State,
  locales_v: JsValue,
  options_v: JsValue,
  default_components: List(#(String, String)),
  required: String,
) -> Result(#(Dict(String, JsValue), State), Thrown) {
  use #(requested, opts, state) <- result.try(slots_prologue(
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
      let offset = 0 - ffi_tz_offset_minutes(float.truncate(now_ms()))
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
  // Expand styles / apply defaults into the formatting components ("c:" keys).
  let #(c_date, c_time) = case date_style, time_style, required_present {
    None, None, False -> {
      let user_date =
        list.filter_map(
          [
            #("c:weekday", weekday),
            #("c:era", era),
            #("c:year", year),
            #("c:month", month),
            #("c:day", day),
          ],
          fn(kv) {
            case kv {
              #(k, Some(v)) -> Ok(#(k, v))
              #(_, None) -> Error(Nil)
            }
          },
        )
      let user_time =
        list.filter_map(
          [
            #("c:dayPeriod", day_period),
            #("c:hour", hour),
            #("c:minute", minute),
            #("c:second", second),
            #("c:fractionalSecondDigits", option.map(fractional, int.to_string)),
            #("c:timeZoneName", tz_name_opt),
          ],
          fn(kv) {
            case kv {
              #(k, Some(v)) -> Ok(#(k, v))
              #(_, None) -> Error(Nil)
            }
          },
        )
      let merge = fn(
        user: List(#(String, String)),
        defaults: List(#(String, String)),
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
            kv.0 == "c:weekday"
            || kv.0 == "c:era"
            || kv.0 == "c:year"
            || kv.0 == "c:month"
            || kv.0 == "c:day"
          }),
        ),
        merge(
          user_time,
          list.filter(default_components, fn(kv) {
            kv.0 == "c:hour" || kv.0 == "c:minute" || kv.0 == "c:second"
          }),
        ),
      )
    }
    None, None, True -> #(
      list.filter_map(
        [
          #("c:weekday", weekday),
          #("c:era", era),
          #("c:year", year),
          #("c:month", month),
          #("c:day", day),
        ],
        fn(kv) {
          case kv {
            #(k, Some(v)) -> Ok(#(k, v))
            #(_, None) -> Error(Nil)
          }
        },
      ),
      list.filter_map(
        [
          #("c:dayPeriod", day_period),
          #("c:hour", hour),
          #("c:minute", minute),
          #("c:second", second),
          #("c:fractionalSecondDigits", option.map(fractional, int.to_string)),
          #("c:timeZoneName", tz_name_opt),
        ],
        fn(kv) {
          case kv {
            #(k, Some(v)) -> Ok(#(k, v))
            #(_, None) -> Error(Nil)
          }
        },
      ),
    )
    ds, ts, _ -> #(date_style_components(ds), time_style_components(ts))
  }
  let has_hour = list.any(c_time, fn(kv) { kv.0 == "c:hour" })
  let public =
    list.filter_map(
      [
        #("weekday", weekday),
        #("era", era),
        #("year", year),
        #("month", month),
        #("day", day),
        #("dayPeriod", day_period),
        #("hour", hour),
        #("minute", minute),
        #("second", second),
        #("timeZoneName", tz_name_opt),
        #("dateStyle", date_style),
        #("timeStyle", time_style),
      ],
      fn(kv) {
        case kv {
          #(k, Some(v)) -> Ok(#(k, JsString(v)))
          #(_, None) -> Error(Nil)
        }
      },
    )
  // Style-derived components are also visible in resolvedOptions when no
  // dateStyle/timeStyle is set; with styles set, only the styles show.
  let public = case date_style, time_style {
    None, None ->
      case required_present {
        True -> public
        False ->
          list.append(
            public,
            default_components
              |> list.filter(fn(kv) {
                !list.any(public, fn(pv) { pv.0 == string.drop_start(kv.0, 2) })
              })
              |> list.map(fn(kv) {
                #(string.drop_start(kv.0, 2), JsString(kv.1))
              }),
          )
      }
    _, _ -> public
  }
  let fractional_slot = case fractional {
    Some(f) -> [#("fractionalSecondDigits", value.from_int(f))]
    None -> []
  }
  // Names of the nine component options that were explicitly provided —
  // needed at format time to compute per-Temporal-type formats
  // (GetDateTimeFormat with inherit = ~relevant~).
  let explicit_names =
    list.filter_map(
      [
        #("weekday", weekday),
        #("year", year),
        #("month", month),
        #("day", day),
        #("dayPeriod", day_period),
        #("hour", hour),
        #("minute", minute),
        #("second", second),
        #("fractionalSecondDigits", option.map(fractional, int.to_string)),
      ],
      fn(kv) {
        case kv {
          #(k, Some(_)) -> Ok(k)
          #(_, None) -> Error(Nil)
        }
      },
    )
  let hc_slots = case has_hour {
    True -> [#("hourCycle", JsString(hc))]
    False -> []
  }
  let slots =
    dict.from_list(
      list.flatten([
        [
          #("locale", JsString(locale)),
          #("calendar", JsString(calendar)),
          #("numberingSystem", JsString(nu)),
          #("timeZone", JsString(tz_name)),
          #("tzOffsetMinutes", value.from_int(tz_offset)),
          #("tzSystem", JsBool(tz_system)),
          #("explicit", JsString(string.join(explicit_names, ","))),
        ],
        hc_slots,
        public,
        fractional_slot,
        list.map(c_date, fn(kv) { #(kv.0, JsString(kv.1)) }),
        list.map(c_time, fn(kv) { #(kv.0, JsString(kv.1)) }),
      ]),
    )
  Ok(#(slots, state))
}

fn date_style_components(style: Option(String)) -> List(#(String, String)) {
  case style {
    Some("full") -> [
      #("c:weekday", "long"),
      #("c:year", "numeric"),
      #("c:month", "long"),
      #("c:day", "numeric"),
    ]
    Some("long") -> [
      #("c:year", "numeric"),
      #("c:month", "long"),
      #("c:day", "numeric"),
    ]
    Some("medium") -> [
      #("c:year", "numeric"),
      #("c:month", "short"),
      #("c:day", "numeric"),
    ]
    Some("short") -> [
      #("c:year", "2-digit"),
      #("c:month", "numeric"),
      #("c:day", "numeric"),
    ]
    _ -> []
  }
}

fn time_style_components(style: Option(String)) -> List(#(String, String)) {
  case style {
    Some("full") -> [
      #("c:hour", "numeric"),
      #("c:minute", "2-digit"),
      #("c:second", "2-digit"),
      #("c:timeZoneName", "long"),
    ]
    Some("long") -> [
      #("c:hour", "numeric"),
      #("c:minute", "2-digit"),
      #("c:second", "2-digit"),
      #("c:timeZoneName", "short"),
    ]
    Some("medium") -> [
      #("c:hour", "numeric"),
      #("c:minute", "2-digit"),
      #("c:second", "2-digit"),
    ]
    Some("short") -> [#("c:hour", "numeric"), #("c:minute", "2-digit")]
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

fn plural_rules_slots(
  state: State,
  locales_v: JsValue,
  options_v: JsValue,
) -> Result(#(Dict(String, JsValue), State), Thrown) {
  use #(requested, opts, state) <- result.try(slots_prologue(
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
  use #(digit_slots, state) <- result.try(digit_options(
    state,
    opts,
    0,
    3,
    notation,
  ))
  let compact_slot = case notation {
    "compact" -> [
      #("compactDisplay", JsString(option.unwrap(compact_display, "short"))),
    ]
    _ -> []
  }
  let slots =
    dict.from_list(
      list.flatten([
        [
          #("locale", JsString(data_locale)),
          #("type", JsString(option.unwrap(type_, "cardinal"))),
          #("notation", JsString(notation)),
        ],
        compact_slot,
        dict.to_list(digit_slots),
      ]),
    )
  Ok(#(slots, state))
}

// --- Intl.ListFormat ---

fn list_format_slots(
  state: State,
  locales_v: JsValue,
  options_v: JsValue,
) -> Result(#(Dict(String, JsValue), State), Thrown) {
  use #(requested, opts, state) <- result.try(slots_prologue(
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
  let slots =
    dict.from_list([
      #("locale", JsString(data_locale)),
      #("type", JsString(option.unwrap(type_, "conjunction"))),
      #("style", JsString(option.unwrap(style, "long"))),
    ])
  Ok(#(slots, state))
}

// --- Intl.RelativeTimeFormat ---

fn rtf_slots(
  state: State,
  locales_v: JsValue,
  options_v: JsValue,
) -> Result(#(Dict(String, JsValue), State), Thrown) {
  use #(requested, opts, state) <- result.try(slots_prologue(
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
  let slots =
    dict.from_list([
      #("locale", JsString(locale)),
      #("style", JsString(option.unwrap(style, "long"))),
      #("numeric", JsString(option.unwrap(numeric, "always"))),
      #("numberingSystem", JsString(nu)),
    ])
  Ok(#(slots, state))
}

// --- Intl.Segmenter ---

fn segmenter_slots(
  state: State,
  locales_v: JsValue,
  options_v: JsValue,
) -> Result(#(Dict(String, JsValue), State), Thrown) {
  use #(requested, opts, state) <- result.try(slots_prologue(
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
  let slots =
    dict.from_list([
      #("locale", JsString(data_locale)),
      #("granularity", JsString(option.unwrap(granularity, "grapheme"))),
    ])
  Ok(#(slots, state))
}

// --- Intl.DisplayNames ---

fn display_names_slots(
  state: State,
  locales_v: JsValue,
  options_v: JsValue,
) -> Result(#(Dict(String, JsValue), State), Thrown) {
  use #(requested, opts, state) <- result.try(slots_prologue(
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
  let base = [
    #("locale", JsString(data_locale)),
    #("style", JsString(option.unwrap(style, "long"))),
    #("type", JsString(type_)),
    #("fallback", JsString(option.unwrap(fallback, "code"))),
  ]
  let slots = case type_ {
    "language" -> [
      #("languageDisplay", JsString(option.unwrap(language_display, "dialect"))),
      ..base
    ]
    _ -> base
  }
  Ok(#(dict.from_list(slots), state))
}

// --- Intl.DurationFormat ---

const duration_units = [
  "years", "months", "weeks", "days", "hours", "minutes", "seconds",
  "milliseconds", "microseconds", "nanoseconds",
]

fn duration_format_slots(
  state: State,
  locales_v: JsValue,
  options_v: JsValue,
) -> Result(#(Dict(String, JsValue), State), Thrown) {
  use #(requested, opts, state) <- result.try(slots_prologue(
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
  use #(unit_slots, state, _prev) <- result.try(
    list.try_fold(duration_units, #([], state, "none"), fn(acc, unit) {
      let #(slots_acc, state, prev_style) = acc
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
        unit == "milliseconds"
        || unit == "microseconds"
        || unit == "nanoseconds"
      let prev_numeric =
        prev_style == "numeric"
        || prev_style == "2-digit"
        || prev_style == "fractional"
      // GetDurationUnitOptions defaulting (Intl.DurationFormat §1.1.6).
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
      let slots_acc = [
        #(unit, JsString(public_style)),
        #(unit <> "Display", JsString(display)),
        ..slots_acc
      ]
      Ok(#(slots_acc, state, style))
    }),
  )
  use #(fractional, state) <- result.try(get_num_opt(
    state,
    opts,
    "fractionalDigits",
    0,
    9,
    None,
  ))
  let fractional_slot = case fractional {
    Some(f) -> [#("fractionalDigits", value.from_int(f))]
    None -> []
  }
  let slots =
    dict.from_list(
      list.flatten([
        [
          #("locale", JsString(locale)),
          #("numberingSystem", JsString(nu)),
          #("style", JsString(base_style)),
        ],
        unit_slots,
        fractional_slot,
      ]),
    )
  Ok(#(slots, state))
}

// ============================================================================
// resolvedOptions
// ============================================================================

fn resolved_options(
  service: IntlService,
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  run({
    use #(_ref, slots) <- result.try(branded(
      state,
      this,
      service,
      "Intl." <> service_name(service) <> ".prototype.resolvedOptions",
    ))
    let keys = case service {
      IntlCollator -> [
        "locale", "usage", "sensitivity", "ignorePunctuation", "collation",
        "numeric", "caseFirst",
      ]
      IntlNumberFormat -> [
        "locale", "numberingSystem", "style", "currency", "currencyDisplay",
        "currencySign", "unit", "unitDisplay", "minimumIntegerDigits",
        "minimumFractionDigits", "maximumFractionDigits",
        "minimumSignificantDigits", "maximumSignificantDigits", "useGrouping",
        "notation", "compactDisplay", "signDisplay", "roundingIncrement",
        "roundingMode", "roundingPriority", "trailingZeroDisplay",
      ]
      IntlDateTimeFormat -> [
        "locale", "calendar", "numberingSystem", "timeZone", "hourCycle",
        "hour12", "weekday", "era", "year", "month", "day", "dayPeriod", "hour",
        "minute", "second", "fractionalSecondDigits", "timeZoneName",
        "dateStyle", "timeStyle",
      ]
      IntlPluralRules -> [
        "locale", "type", "notation", "compactDisplay", "minimumIntegerDigits",
        "minimumFractionDigits", "maximumFractionDigits",
        "minimumSignificantDigits", "maximumSignificantDigits",
        "pluralCategories", "roundingIncrement", "roundingMode",
        "roundingPriority", "trailingZeroDisplay",
      ]
      IntlListFormat -> ["locale", "type", "style"]
      IntlRelativeTimeFormat -> [
        "locale",
        "style",
        "numeric",
        "numberingSystem",
      ]
      IntlSegmenter -> ["locale", "granularity"]
      IntlDisplayNames -> [
        "locale", "style", "type", "fallback", "languageDisplay",
      ]
      IntlDurationFormat ->
        list.flatten([
          ["locale", "numberingSystem", "style"],
          list.flat_map(duration_units, fn(u) { [u, u <> "Display"] }),
          ["fractionalDigits"],
        ])
      _ -> ["locale"]
    }
    let #(state, props) =
      list.fold(keys, #(state, []), fn(acc, key) {
        let #(state, props) = acc
        case key {
          "hour12" ->
            case slot_str(slots, "hourCycle") {
              Some(hc) -> #(state, [
                #("hour12", JsBool(hc == "h11" || hc == "h12")),
                ..props
              ])
              None -> #(state, props)
            }
          "pluralCategories" -> {
            let type_ = slot_str(slots, "type") |> option.unwrap("cardinal")
            let #(state, arr) =
              alloc_array(
                state,
                fmt.plural_categories_en(type_) |> list.map(JsString),
              )
            #(state, [#("pluralCategories", arr), ..props])
          }
          _ ->
            case dict.get(slots, key) {
              Ok(v) -> #(state, [#(key, v), ..props])
              Error(Nil) -> #(state, props)
            }
        }
      })
    let #(state, obj) = alloc_pojo(state, list.reverse(props))
    Ok(#(obj, state))
  })
}

// ============================================================================
// Bound method getters (format / compare)
// ============================================================================

fn bound_getter(
  service: IntlService,
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let #(slot_key, arity) = case service {
    IntlCollator -> #("boundCompare", 2)
    _ -> #("boundFormat", 1)
  }
  run({
    use #(ref, slots) <- result.try(branded(
      state,
      this,
      service,
      "Intl." <> service_name(service) <> " bound method getter",
    ))
    case dict.get(slots, slot_key) {
      Ok(cached) -> Ok(#(cached, state))
      Error(Nil) -> {
        let #(heap, fn_ref) =
          common.alloc_native_fn(
            state.heap,
            state.builtins.function.prototype,
            IntlNative(IntlBoundMethod(service:, target: ref)),
            "",
            arity,
          )
        let bound = JsObject(fn_ref)
        let heap =
          heap.update(heap, ref, fn(slot) {
            case slot {
              ObjectSlot(kind: IntlObject(service: s, slots: sl), ..) ->
                ObjectSlot(
                  ..slot,
                  kind: IntlObject(
                    service: s,
                    slots: dict.insert(sl, slot_key, bound),
                  ),
                )
              other -> other
            }
          })
        Ok(#(bound, State(..state, heap:)))
      }
    }
  })
}

fn bound_method(
  service: IntlService,
  target: Ref,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  run({
    use #(_ref, slots) <- result.try(branded(
      state,
      JsObject(target),
      service,
      "bound Intl method",
    ))
    case service {
      IntlNumberFormat -> {
        use #(parts, state) <- result.try(nf_format_parts(
          state,
          slots,
          first_arg_or_undefined(args),
        ))
        Ok(#(JsString(fmt.parts_to_string(parts)), state))
      }
      IntlDateTimeFormat -> {
        use #(parts, state) <- result.try(dtf_format_parts(
          state,
          slots,
          first_arg_or_undefined(args),
        ))
        Ok(#(JsString(fmt.parts_to_string(parts)), state))
      }
      IntlCollator -> {
        use #(a, state) <- result.try(coerce.js_to_string(
          state,
          first_arg_or_undefined(args),
        ))
        use #(b, state) <- result.try(coerce.js_to_string(
          state,
          helpers.list_at(args, 1) |> option.unwrap(JsUndefined),
        ))
        Ok(#(value.from_int(collator_compare(slots, a, b)), state))
      }
      _ -> throw_type(state, "Unsupported bound method")
    }
  })
}

// ============================================================================
// NumberFormat formatting glue
// ============================================================================

fn num_opts_from_slots(slots: Dict(String, JsValue)) -> fmt.NumOpts {
  let d = fmt.default_num_opts()
  fmt.NumOpts(
    locale: slot_str(slots, "locale") |> option.unwrap(d.locale),
    style: slot_str(slots, "style") |> option.unwrap(d.style),
    currency: slot_str(slots, "currency"),
    currency_display: slot_str(slots, "currencyDisplay")
      |> option.unwrap(d.currency_display),
    currency_sign: slot_str(slots, "currencySign")
      |> option.unwrap(d.currency_sign),
    unit: slot_str(slots, "unit"),
    unit_display: slot_str(slots, "unitDisplay")
      |> option.unwrap(d.unit_display),
    min_int: slot_int(slots, "minimumIntegerDigits") |> option.unwrap(1),
    min_frac: slot_int(slots, "minimumFractionDigits"),
    max_frac: slot_int(slots, "maximumFractionDigits"),
    min_sig: slot_int(slots, "minimumSignificantDigits"),
    max_sig: slot_int(slots, "maximumSignificantDigits"),
    use_grouping: case dict.get(slots, "useGrouping") {
      Ok(JsBool(False)) -> "never"
      Ok(JsString(s)) -> s
      _ -> "auto"
    },
    notation: slot_str(slots, "notation") |> option.unwrap(d.notation),
    compact_display: slot_str(slots, "compactDisplay")
      |> option.unwrap(d.compact_display),
    sign_display: slot_str(slots, "signDisplay")
      |> option.unwrap(d.sign_display),
    rounding_increment: slot_int(slots, "roundingIncrement") |> option.unwrap(1),
    rounding_mode: slot_str(slots, "roundingMode")
      |> option.unwrap(d.rounding_mode),
    rounding_priority: slot_str(slots, "roundingPriority")
      |> option.unwrap(d.rounding_priority),
    trailing_zero_display: slot_str(slots, "trailingZeroDisplay")
      |> option.unwrap(d.trailing_zero_display),
  )
}

/// ToIntlMathematicalValue, approximated with ToNumber (BigInt allowed).
fn to_intl_number(
  state: State,
  v: JsValue,
) -> Result(#(value.JsNum, State), Thrown) {
  case v {
    value.JsBigInt(value.BigInt(n)) ->
      Ok(#(value.Finite(int.to_float(n)), state))
    _ -> coerce.js_to_number(state, v)
  }
}

fn nf_format_parts(
  state: State,
  slots: Dict(String, JsValue),
  x: JsValue,
) -> Result(#(List(fmt.Part), State), Thrown) {
  let opts = num_opts_from_slots(slots)
  let nu = slot_str(slots, "numberingSystem") |> option.unwrap("latn")
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
        False -> nf_format_number(state, slots, x, opts, nu)
      }
    _ -> nf_format_number(state, slots, x, opts, nu)
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
  state: State,
  _slots: Dict(String, JsValue),
  x: JsValue,
  opts: fmt.NumOpts,
  nu: String,
) -> Result(#(List(fmt.Part), State), Thrown) {
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
  state: State,
  slots: Dict(String, JsValue),
  x_v: JsValue,
  y_v: JsValue,
) -> Result(#(List(#(String, String, String)), State), Thrown) {
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
  use #(x_parts, state) <- result.try(nf_format_parts(state, slots, x_fmt))
  use #(y_parts, state) <- result.try(nf_format_parts(state, slots, y_fmt))
  let locale = slot_str(slots, "locale") |> option.unwrap("en")
  Ok(#(fmt.format_range_combine(locale, x_parts, y_parts), state))
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
fn dtf_temporal_value(state: State, v: JsValue) -> Option(TemporalFormattable) {
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

/// Allowed / required / default component names per Temporal type, plus
/// whether `era` / hour-cycle options carry over (GetDateTimeFormat's
/// ~relevant~ inheritance).
fn tf_component_rules(
  t: TemporalFormattable,
) -> #(List(String), List(String), List(String), Bool) {
  case t {
    TfDate(..) -> #(
      ["weekday", "era", "year", "month", "day"],
      ["weekday", "year", "month", "day"],
      ["year", "month", "day"],
      True,
    )
    TfYearMonth(..) -> #(
      ["era", "year", "month"],
      ["year", "month"],
      ["year", "month"],
      True,
    )
    TfMonthDay(..) -> #(
      ["month", "day"],
      ["month", "day"],
      ["month", "day"],
      False,
    )
    TfTime(..) -> #(
      ["dayPeriod", "hour", "minute", "second", "fractionalSecondDigits"],
      ["dayPeriod", "hour", "minute", "second", "fractionalSecondDigits"],
      ["hour", "minute", "second"],
      False,
    )
    TfDateTime(..) -> #(
      [
        "weekday", "era", "year", "month", "day", "dayPeriod", "hour", "minute",
        "second", "fractionalSecondDigits",
      ],
      [
        "weekday", "year", "month", "day", "dayPeriod", "hour", "minute",
        "second", "fractionalSecondDigits",
      ],
      ["year", "month", "day", "hour", "minute", "second"],
      True,
    )
    TfInstant(..) | TfZoned -> #([], [], [], False)
  }
}

/// HandleDateTimeValue: validate the Temporal value against the formatter
/// (calendar compatibility, suitable format availability) and return slots
/// whose "c:" component keys are adjusted to the per-type format.
fn dtf_temporal_slots(
  state: State,
  slots: Dict(String, JsValue),
  t: TemporalFormattable,
) -> Result(#(Dict(String, JsValue), State), Thrown) {
  case t {
    TfZoned ->
      throw_type(
        state,
        "Temporal.ZonedDateTime cannot be formatted with Intl.DateTimeFormat; use Temporal.ZonedDateTime.prototype.toLocaleString instead",
      )
    TfInstant(..) -> Ok(#(slots, state))
    _ -> {
      let dtf_cal = slot_str(slots, "calendar") |> option.unwrap("gregory")
      let cal_ok = case t {
        TfDate(calendar:, ..) | TfDateTime(calendar:, ..) ->
          calendar == "iso8601" || calendar == dtf_cal
        TfYearMonth(calendar:, ..) | TfMonthDay(calendar:, ..) ->
          calendar == dtf_cal
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
        slot_str(slots, "dateStyle") != None
        || slot_str(slots, "timeStyle") != None
      case has_styles {
        True -> {
          // AdjustDateTimeStyleFormat: per-type formats exist only when the
          // matching style was given; keep only the allowed components.
          let style_ok = case t {
            TfDate(..) | TfYearMonth(..) | TfMonthDay(..) ->
              slot_str(slots, "dateStyle") != None
            TfTime(..) -> slot_str(slots, "timeStyle") != None
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
              case c_slot(slots, name) {
                Some(v) -> Ok(#("c:" <> name, v))
                None -> Error(Nil)
              }
            })
          Ok(#(replace_components(slots, kept), state))
        }
        False -> {
          // GetDateTimeFormat with inherit = ~relevant~ over the explicitly
          // provided component options.
          let explicit = case slot_str(slots, "explicit") {
            Some("") | None -> []
            Some(s) -> string.split(s, ",")
          }
          let in_required =
            list.filter(explicit, fn(name) { list.contains(required, name) })
          let era_comps = case copy_era, slot_str(slots, "era") {
            True, Some(e) -> [#("c:era", e)]
            _, _ -> []
          }
          case in_required {
            [] ->
              case explicit {
                [] -> {
                  let comps =
                    list.map(defaults, fn(name) { #("c:" <> name, "numeric") })
                  Ok(#(
                    replace_components(slots, list.append(era_comps, comps)),
                    state,
                  ))
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
                  case list.contains(explicit, name) {
                    False -> Error(Nil)
                    True ->
                      case name {
                        "fractionalSecondDigits" ->
                          case slot_int(slots, "fractionalSecondDigits") {
                            Some(d) -> Ok(#("c:" <> name, int.to_string(d)))
                            None -> Error(Nil)
                          }
                        _ ->
                          case c_slot(slots, name) {
                            Some(v) -> Ok(#("c:" <> name, v))
                            None -> Error(Nil)
                          }
                      }
                  }
                })
              Ok(#(
                replace_components(slots, list.append(era_comps, comps)),
                state,
              ))
            }
          }
        }
      }
    }
  }
}

/// Replace every "c:" component key in `slots` with `comps`.
fn replace_components(
  slots: Dict(String, JsValue),
  comps: List(#(String, String)),
) -> Dict(String, JsValue) {
  let cleaned = dict.filter(slots, fn(k, _) { !string.starts_with(k, "c:") })
  list.fold(comps, cleaned, fn(acc, kv) {
    dict.insert(acc, kv.0, JsString(kv.1))
  })
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
  slots: Dict(String, JsValue),
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
      let offset = case slot_bool(slots, "tzSystem") {
        Some(True) -> 0 - ffi_tz_offset_minutes(ms)
        _ -> slot_int(slots, "tzOffsetMinutes") |> option.unwrap(0)
      }
      fmt.fields_from_epoch_ms(int.to_float(ms), offset)
    }
    TfZoned ->
      // Unreachable: dtf_temporal_slots throws for ZonedDateTime first.
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
  state: State,
  slots: Dict(String, JsValue),
  date_v: JsValue,
) -> Result(#(List(fmt.Part), State), Thrown) {
  case dtf_temporal_value(state, date_v) {
    Some(t) -> {
      use #(slots, state) <- result.try(dtf_temporal_slots(state, slots, t))
      let fields = dtf_temporal_fields(slots, t)
      let parts = build_dtf_parts(slots, fields)
      let nu = slot_str(slots, "numberingSystem") |> option.unwrap("latn")
      Ok(#(apply_numbering_system_dtf(parts, nu), state))
    }
    None -> dtf_format_parts_number(state, slots, date_v)
  }
}

fn dtf_format_parts_number(
  state: State,
  slots: Dict(String, JsValue),
  date_v: JsValue,
) -> Result(#(List(fmt.Part), State), Thrown) {
  use #(tv, state) <- result.try(case date_v {
    JsUndefined -> Ok(#(value.Finite(now_ms()), state))
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
  let offset = case slot_bool(slots, "tzSystem") {
    Some(True) -> 0 - ffi_tz_offset_minutes(float.truncate(tv_f))
    _ -> slot_int(slots, "tzOffsetMinutes") |> option.unwrap(0)
  }
  let fields = fmt.fields_from_epoch_ms(tv_f, offset)
  let parts = build_dtf_parts(slots, fields)
  let nu = slot_str(slots, "numberingSystem") |> option.unwrap("latn")
  Ok(#(apply_numbering_system_dtf(parts, nu), state))
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

fn c_slot(slots: Dict(String, JsValue), name: String) -> Option(String) {
  slot_str(slots, "c:" <> name)
}

fn build_dtf_parts(
  slots: Dict(String, JsValue),
  fields: fmt.DateFields,
) -> List(fmt.Part) {
  let weekday = c_slot(slots, "weekday")
  let era = c_slot(slots, "era")
  let year = c_slot(slots, "year")
  let month = c_slot(slots, "month")
  let day = c_slot(slots, "day")
  let day_period = c_slot(slots, "dayPeriod")
  let hour = c_slot(slots, "hour")
  let minute = c_slot(slots, "minute")
  let second = c_slot(slots, "second")
  let fractional = c_slot(slots, "fractionalSecondDigits")
  let tz_name = c_slot(slots, "timeZoneName")
  let hc = slot_str(slots, "hourCycle") |> option.unwrap("h12")

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
      let lang = case
        tags.parse(slot_str(slots, "locale") |> option.unwrap("en"))
      {
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
      let offset = slot_int(slots, "tzOffsetMinutes") |> option.unwrap(0)
      let name = tz_display(slots, style, offset)
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

fn tz_display(
  slots: Dict(String, JsValue),
  style: String,
  offset: Int,
) -> String {
  let name = slot_str(slots, "timeZone") |> option.unwrap("UTC")
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
  state: State,
  slots: Dict(String, JsValue),
  x_v: JsValue,
  y_v: JsValue,
) -> Result(#(List(#(String, String, String)), State), Thrown) {
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
  use #(slots, state) <- result.try(case tx, ty {
    None, None -> Ok(#(slots, state))
    Some(a), Some(b) ->
      case same_temporal_kind(a, b) {
        False ->
          throw_type(
            state,
            "Intl.DateTimeFormat range arguments must be of the same type",
          )
        True -> {
          // Validates x (calendar / suitable format) and yields the
          // per-type adjusted component slots; y is validated separately.
          use #(adjusted, state) <- result.try(dtf_temporal_slots(
            state,
            slots,
            a,
          ))
          use #(_slots_y, state) <- result.try(dtf_temporal_slots(
            state,
            slots,
            b,
          ))
          Ok(#(adjusted, state))
        }
      }
    _, _ ->
      throw_type(
        state,
        "Intl.DateTimeFormat range arguments must be of the same type",
      )
  })
  case dtf_collapsed_range(state, slots, x_v, y_v) {
    Ok(Some(#(parts, state))) -> Ok(#(parts, state))
    Ok(None) -> {
      use #(x_parts, state) <- result.try(dtf_format_parts(state, slots, x_v))
      use #(y_parts, state) <- result.try(dtf_format_parts(state, slots, y_v))
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
    Error(e) -> Error(e)
  }
}

/// "Jan 3 – 5, 2019": collapse a named-month date-only range that differs
/// only in the day.
fn dtf_collapsed_range(
  state: State,
  slots: Dict(String, JsValue),
  x_v: JsValue,
  y_v: JsValue,
) -> Result(Option(#(List(#(String, String, String)), State)), Thrown) {
  let month_style = c_slot(slots, "month")
  let named_month = case month_style {
    Some("long") | Some("short") | Some("narrow") -> True
    _ -> False
  }
  let date_only =
    c_slot(slots, "hour") == None
    && c_slot(slots, "minute") == None
    && c_slot(slots, "second") == None
    && c_slot(slots, "weekday") == None
  let has_ymd =
    c_slot(slots, "year") != None
    && c_slot(slots, "month") != None
    && c_slot(slots, "day") != None
  case named_month && date_only && has_ymd {
    False -> Ok(None)
    True -> {
      use #(xf, state) <- result.try(dtf_fields(state, slots, x_v))
      use #(yf, state) <- result.try(dtf_fields(state, slots, y_v))
      {
        let day_style = c_slot(slots, "day") |> option.unwrap("numeric")
        let year_style = c_slot(slots, "year") |> option.unwrap("numeric")
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
  state: State,
  slots: Dict(String, JsValue),
  date_v: JsValue,
) -> Result(#(fmt.DateFields, State), Thrown) {
  case dtf_temporal_value(state, date_v) {
    Some(t) -> Ok(#(dtf_temporal_fields(slots, t), state))
    None -> dtf_fields_number(state, slots, date_v)
  }
}

fn dtf_fields_number(
  state: State,
  slots: Dict(String, JsValue),
  date_v: JsValue,
) -> Result(#(fmt.DateFields, State), Thrown) {
  use #(tv, state) <- result.try(case date_v {
    JsUndefined -> Ok(#(value.Finite(now_ms()), state))
    _ -> coerce.js_to_number(state, date_v)
  })
  use tv_f <- result.try(case tv {
    value.Finite(f) -> {
      let f = int.to_float(float.truncate(f))
      case float.absolute_value(f) <=. 8.64e15 {
        True -> Ok(f)
        False -> throw_range(state, "Invalid time value")
      }
    }
    _ -> throw_range(state, "Invalid time value")
  })
  let offset = case slot_bool(slots, "tzSystem") {
    Some(True) -> 0 - ffi_tz_offset_minutes(float.truncate(tv_f))
    _ -> slot_int(slots, "tzOffsetMinutes") |> option.unwrap(0)
  }
  Ok(#(fmt.fields_from_epoch_ms(tv_f, offset), state))
}

// ============================================================================
// Collator compare
// ============================================================================

fn collator_compare(slots: Dict(String, JsValue), a: String, b: String) -> Int {
  let sensitivity = slot_str(slots, "sensitivity") |> option.unwrap("variant")
  let numeric = slot_bool(slots, "numeric") |> option.unwrap(False)
  let ignore_punct =
    slot_bool(slots, "ignorePunctuation") |> option.unwrap(False)
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let arg0 = first_arg_or_undefined(args)
  let arg1 = helpers.list_at(args, 1) |> option.unwrap(JsUndefined)
  let arg2 = helpers.list_at(args, 2) |> option.unwrap(JsUndefined)
  use <- host_method_guard(service, method, args, this, state, arg0, arg1, arg2)
  run({
    use #(ref, slots) <- result.try(branded(
      state,
      this,
      service,
      "Intl." <> service_name(service) <> ".prototype." <> method,
    ))
    case service, method {
      IntlNumberFormat, "formatToParts" -> {
        use #(parts, state) <- result.try(nf_format_parts(state, slots, arg0))
        let #(state, arr) = parts_to_js(state, parts)
        Ok(#(arr, state))
      }
      IntlNumberFormat, "formatRange" -> {
        use #(parts, state) <- result.try(nf_range_parts(
          state,
          slots,
          arg0,
          arg1,
        ))
        let s = parts |> list.map(fn(p) { p.1 }) |> string.join("")
        Ok(#(JsString(s), state))
      }
      IntlNumberFormat, "formatRangeToParts" -> {
        use #(parts, state) <- result.try(nf_range_parts(
          state,
          slots,
          arg0,
          arg1,
        ))
        let #(state, arr) = parts_to_js_sourced(state, parts)
        Ok(#(arr, state))
      }
      IntlDateTimeFormat, "formatToParts" -> {
        use #(parts, state) <- result.try(dtf_format_parts(state, slots, arg0))
        let #(state, arr) = parts_to_js(state, parts)
        Ok(#(arr, state))
      }
      IntlDateTimeFormat, "formatRange" -> {
        use #(parts, state) <- result.try(dtf_range_parts(
          state,
          slots,
          arg0,
          arg1,
        ))
        let s = parts |> list.map(fn(p) { p.1 }) |> string.join("")
        Ok(#(JsString(s), state))
      }
      IntlDateTimeFormat, "formatRangeToParts" -> {
        use #(parts, state) <- result.try(dtf_range_parts(
          state,
          slots,
          arg0,
          arg1,
        ))
        let #(state, arr) = parts_to_js_sourced(state, parts)
        Ok(#(arr, state))
      }
      IntlPluralRules, "select" -> {
        use #(n, state) <- result.try(coerce.js_to_number(state, arg0))
        Ok(#(JsString(plural_select(slots, n)), state))
      }
      IntlPluralRules, "selectRange" -> {
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
      IntlListFormat, "format" -> {
        use #(items, state) <- result.try(string_list_from_iterable(state, arg0))
        let type_ = slot_str(slots, "type") |> option.unwrap("conjunction")
        let style = slot_str(slots, "style") |> option.unwrap("long")
        let parts = fmt.list_format_parts(type_, style, items)
        Ok(#(JsString(fmt.parts_to_string(parts)), state))
      }
      IntlListFormat, "formatToParts" -> {
        use #(items, state) <- result.try(string_list_from_iterable(state, arg0))
        let type_ = slot_str(slots, "type") |> option.unwrap("conjunction")
        let style = slot_str(slots, "style") |> option.unwrap("long")
        let parts = fmt.list_format_parts(type_, style, items)
        let #(state, arr) = parts_to_js(state, parts)
        Ok(#(arr, state))
      }
      IntlRelativeTimeFormat, "format" -> {
        use #(parts, state) <- result.try(rtf_method_parts(
          state,
          slots,
          arg0,
          arg1,
        ))
        let str = parts |> list.map(fn(part) { part.1 }) |> string.join("")
        Ok(#(JsString(str), state))
      }
      IntlRelativeTimeFormat, "formatToParts" -> {
        use #(parts, state) <- result.try(rtf_method_parts(
          state,
          slots,
          arg0,
          arg1,
        ))
        let #(state, arr) = parts_to_js_with_unit(state, parts)
        Ok(#(arr, state))
      }
      IntlDisplayNames, "of" -> display_names_of(state, slots, arg0)
      IntlDurationFormat, "format" -> {
        use #(parts, state) <- result.try(duration_parts(state, slots, arg0))
        let str = parts |> list.map(fn(part) { part.1 }) |> string.join("")
        Ok(#(JsString(str), state))
      }
      IntlDurationFormat, "formatToParts" -> {
        use #(parts, state) <- result.try(duration_parts(state, slots, arg0))
        let #(state, arr) = parts_to_js_with_unit(state, parts)
        Ok(#(arr, state))
      }
      IntlSegments, "containing" -> segments_containing(state, slots, arg0)
      IntlSegmentIterator, "next" -> segment_iterator_next(state, ref, slots)
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
  state: State,
  arg0: JsValue,
  arg1: JsValue,
  arg2: JsValue,
  next: fn() -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
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
  state: State,
  this: JsValue,
  locales: JsValue,
  options: JsValue,
) -> Result(#(JsValue, State), Thrown) {
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
  use #(slots, state) <- result.try(number_format_slots(state, locales, options))
  use #(parts, state) <- result.try(nf_format_parts(state, slots, JsNumber(n)))
  Ok(#(JsString(fmt.parts_to_string(parts)), state))
}

/// String.prototype.localeCompare (ECMA-402 §19.1.1).
fn host_locale_compare(
  state: State,
  this: JsValue,
  that_v: JsValue,
  locales: JsValue,
  options: JsValue,
) -> Result(#(JsValue, State), Thrown) {
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
  use #(slots, state) <- result.try(collator_slots(state, locales, options))
  Ok(#(value.from_int(collator_compare(slots, s, that)), state))
}

/// String.prototype.toLocale{Lower,Upper}Case — locale list is validated,
/// casing uses the default (root) algorithm.
fn host_locale_case(
  state: State,
  this: JsValue,
  locales: JsValue,
  upper: Bool,
) -> Result(#(JsValue, State), Thrown) {
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
    True ->
      case state.call(state, case_fn, JsString(pre), []) {
        Ok(#(v, state)) -> Ok(#(v, state))
        Error(#(thrown, state)) -> Error(#(thrown, state))
      }
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
  state: State,
  this: JsValue,
  locales: JsValue,
  options: JsValue,
  which: Int,
) -> Result(#(JsValue, State), Thrown) {
  use tv <- result.try(case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: value.DateObject(time_value: tv), ..)) -> Ok(tv)
        _ -> throw_type(state, "this is not a Date object")
      }
    _ -> throw_type(state, "this is not a Date object")
  })
  let date_defaults = [
    #("c:year", "numeric"),
    #("c:month", "numeric"),
    #("c:day", "numeric"),
  ]
  let time_defaults = [
    #("c:hour", "numeric"),
    #("c:minute", "numeric"),
    #("c:second", "numeric"),
  ]
  let #(defaults, required) = case which {
    1 -> #(date_defaults, "date")
    2 -> #(time_defaults, "time")
    _ -> #(list.append(date_defaults, time_defaults), "any")
  }
  use #(slots, state) <- result.try(dtf_slots_required(
    state,
    locales,
    options,
    defaults,
    required,
  ))
  case tv {
    value.Finite(_) -> {
      use #(parts, state) <- result.try(dtf_format_parts(
        state,
        slots,
        JsNumber(tv),
      ))
      Ok(#(JsString(fmt.parts_to_string(parts)), state))
    }
    _ -> Ok(#(JsString("Invalid Date"), state))
  }
}

/// PluralRules select: operands come from the formatted digit strings.
fn plural_select(slots: Dict(String, JsValue), n: value.JsNum) -> String {
  case n {
    value.Finite(f) -> {
      let opts =
        fmt.NumOpts(
          ..num_opts_from_slots(slots),
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
      let type_ = slot_str(slots, "type") |> option.unwrap("cardinal")
      fmt.plural_select_en(type_, int_digits, frac_digits, f <. 0.0)
    }
    _ -> "other"
  }
}

/// RelativeTimeFormat format/formatToParts core.
fn rtf_method_parts(
  state: State,
  slots: Dict(String, JsValue),
  value_v: JsValue,
  unit_v: JsValue,
) -> Result(#(List(#(String, String, String)), State), Thrown) {
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
  let style = slot_str(slots, "style") |> option.unwrap("long")
  let numeric = slot_str(slots, "numeric") |> option.unwrap("always")
  let abs_opts = fmt.NumOpts(..fmt.default_num_opts(), sign_display: "never")
  let value_parts = fmt.format_number_parts(abs_opts, float.absolute_value(f))
  let nu = slot_str(slots, "numberingSystem") |> option.unwrap("latn")
  let value_parts = apply_numbering_system(value_parts, nu)
  Ok(#(fmt.rtf_parts_en(style, numeric, f, unit, value_parts), state))
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
  state: State,
  iterable: JsValue,
) -> Result(#(List(String), State), Thrown) {
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
  state: State,
  iter: JsValue,
  next_fn: JsValue,
  acc: List(String),
) -> Result(#(List(String), State), Thrown) {
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
  state: State,
  slots: Dict(String, JsValue),
  code_v: JsValue,
) -> Result(#(JsValue, State), Thrown) {
  use #(code, state) <- result.try(coerce.js_to_string(state, code_v))
  let type_ = slot_str(slots, "type") |> option.unwrap("language")
  let fallback = slot_str(slots, "fallback") |> option.unwrap("code")
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
  state: State,
  slots: Dict(String, JsValue),
  duration_v: JsValue,
) -> Result(#(List(#(String, String, String)), State), Thrown) {
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
  Ok(#(build_duration_parts(slots, fields), state))
}

/// ToDurationRecord (object) / Temporal duration string parsing.
fn to_duration_record(
  state: State,
  duration_v: JsValue,
) -> Result(#(List(#(String, Float)), State), Thrown) {
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
  case float.parse(s) {
    Ok(v) -> Ok(v)
    Error(Nil) ->
      case int.parse(s) {
        Ok(v) -> Ok(int.to_float(v))
        Error(Nil) -> Error(Nil)
      }
  }
}

/// PartitionDurationFormatPattern — mirrors ECMA-402 Intl.DurationFormat §1.1.7.
fn build_duration_parts(
  slots: Dict(String, JsValue),
  fields: List(#(String, Float)),
) -> List(#(String, String, String)) {
  let nu = slot_str(slots, "numberingSystem") |> option.unwrap("latn")
  let base_style = slot_str(slots, "style") |> option.unwrap("short")
  let frac_digits = slot_int(slots, "fractionalDigits")
  let get = fn(name) { list.key_find(fields, name) |> result.unwrap(0.0) }
  let overall_negative = list.any(fields, fn(f) { f.1 <. 0.0 })
  let fields = list.map(fields, fn(f) { #(f.0, f.1 +. 0.0) })
  let style_of = fn(unit) { slot_str(slots, unit) |> option.unwrap("short") }
  let next_unit_of = fn(unit) {
    case unit {
      "seconds" -> "milliseconds"
      "milliseconds" -> "microseconds"
      "microseconds" -> "nanoseconds"
      _ -> ""
    }
  }
  // Iterate units building groups; numeric units join via ":" separators.
  let init = #([], False, True, False)
  let #(groups_rev, _need_sep, _display_neg, _done) =
    list.fold(duration_units, init, fn(acc, unit) {
      let #(groups, need_sep, display_neg, done) = acc
      case done {
        True -> acc
        False -> {
          let style = style_of(unit)
          let display =
            slot_str(slots, unit <> "Display") |> option.unwrap("auto")
          let raw_value = get(unit)
          // Combine sub-second units when the next unit is numeric.
          let next_style = case next_unit_of(unit) {
            "" -> ""
            nx -> style_of(nx)
          }
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
              slot_str(slots, "secondsDisplay") == Some("always")
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  run({
    use #(_ref, slots) <- result.try(branded(
      state,
      this,
      IntlSegmenter,
      "Intl.Segmenter.prototype.segment",
    ))
    use #(s, state) <- result.try(coerce.js_to_string(
      state,
      first_arg_or_undefined(args),
    ))
    let granularity =
      slot_str(slots, "granularity") |> option.unwrap("grapheme")
    let seg_slots =
      dict.from_list([
        #("string", JsString(s)),
        #("granularity", JsString(granularity)),
      ])
    let #(heap, ref) =
      common.alloc_wrapper(
        state.heap,
        IntlObject(service: IntlSegments, slots: seg_slots),
        segments_proto,
      )
    Ok(#(JsObject(ref), State(..state, heap:)))
  })
}

fn segments_iterator(
  iter_proto: Ref,
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  run({
    use #(_ref, slots) <- result.try(branded(
      state,
      this,
      IntlSegments,
      "%Segments.prototype%[Symbol.iterator]",
    ))
    let iter_slots = dict.insert(slots, "position", value.from_int(0))
    let #(heap, ref) =
      common.alloc_wrapper(
        state.heap,
        IntlObject(service: IntlSegmentIterator, slots: iter_slots),
        iter_proto,
      )
    Ok(#(JsObject(ref), State(..state, heap:)))
  })
}

fn make_segment_data(
  state: State,
  input: String,
  granularity: String,
  segment: String,
  index: Int,
  word_like: Bool,
) -> #(State, JsValue) {
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
  state: State,
  slots: Dict(String, JsValue),
  index_v: JsValue,
) -> Result(#(JsValue, State), Thrown) {
  let input = slot_str(slots, "string") |> option.unwrap("")
  let granularity = slot_str(slots, "granularity") |> option.unwrap("grapheme")
  use #(n, state) <- result.try(coerce.js_to_number(state, index_v))
  let idx = case n {
    value.Finite(f) -> float.truncate(f)
    _ -> 0
  }
  let segments = fmt.segment_string(input, granularity)
  let total = string_utf16_length(input)
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

fn string_utf16_length(s: String) -> Int {
  string.to_utf_codepoints(s)
  |> list.fold(0, fn(n, cp) {
    case string.utf_codepoint_to_int(cp) > 0xffff {
      True -> n + 2
      False -> n + 1
    }
  })
}

fn segment_iterator_next(
  state: State,
  ref: Ref,
  slots: Dict(String, JsValue),
) -> Result(#(JsValue, State), Thrown) {
  let input = slot_str(slots, "string") |> option.unwrap("")
  let granularity = slot_str(slots, "granularity") |> option.unwrap("grapheme")
  let position = slot_int(slots, "position") |> option.unwrap(0)
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
      let new_pos = start + string_utf16_length(s)
      let heap =
        heap.update(state.heap, ref, fn(slot) {
          case slot {
            ObjectSlot(kind: IntlObject(service:, slots: sl), ..) ->
              ObjectSlot(
                ..slot,
                kind: IntlObject(
                  service:,
                  slots: dict.insert(sl, "position", value.from_int(new_pos)),
                ),
              )
            other -> other
          }
        })
      let state = State(..state, heap:)
      let #(state, data) =
        make_segment_data(state, input, granularity, s, start, wl)
      let #(state, res) = iter_result(state, data, False)
      Ok(#(res, state))
    }
  }
}

fn iter_result(state: State, v: JsValue, done: Bool) -> #(State, JsValue) {
  alloc_pojo(state, [#("value", v), #("done", JsBool(done))])
}

// ============================================================================
// Intl.Locale getters & methods
// ============================================================================

fn locale_lid(slots: Dict(String, JsValue)) -> Option(tags.LocaleId) {
  case slot_str(slots, "locale") {
    Some(tag) ->
      case tags.parse(tag) {
        Ok(lid) -> Some(lid)
        Error(Nil) -> None
      }
    None -> None
  }
}

fn locale_u_kw(slots: Dict(String, JsValue), key: String) -> Option(String) {
  case locale_lid(slots) {
    Some(lid) ->
      lid.extensions
      |> list.filter_map(fn(ext) {
        case ext {
          tags.UExt(keywords:, ..) -> Ok(keywords)
          _ -> Error(Nil)
        }
      })
      |> list.flatten
      |> list.key_find(key)
      |> option.from_result
    None -> None
  }
}

fn locale_getter(
  name: String,
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  run({
    use #(_ref, slots) <- result.try(branded(
      state,
      this,
      IntlLocale,
      "Intl.Locale.prototype." <> name,
    ))
    let lid = locale_lid(slots)
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
        locale_u_kw(slots, "ca")
        |> option.map(JsString)
        |> option.unwrap(JsUndefined)
      "collation" ->
        locale_u_kw(slots, "co")
        |> option.map(JsString)
        |> option.unwrap(JsUndefined)
      "hourCycle" ->
        locale_u_kw(slots, "hc")
        |> option.map(JsString)
        |> option.unwrap(JsUndefined)
      "numberingSystem" ->
        locale_u_kw(slots, "nu")
        |> option.map(JsString)
        |> option.unwrap(JsUndefined)
      "caseFirst" ->
        locale_u_kw(slots, "kf")
        |> option.map(JsString)
        |> option.unwrap(JsUndefined)
      "numeric" ->
        case locale_u_kw(slots, "kn") {
          Some("") | Some("true") -> JsBool(True)
          Some(_) -> JsBool(False)
          None -> JsBool(False)
        }
      "firstDayOfWeek" ->
        locale_u_kw(slots, "fw")
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  run({
    use #(_ref, slots) <- result.try(branded(
      state,
      this,
      IntlLocale,
      "Intl.Locale.prototype." <> method,
    ))
    let tag = slot_str(slots, "locale") |> option.unwrap("und")
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
        let new_slots = dict.from_list([#("locale", JsString(new_tag))])
        let #(heap, ref) =
          common.alloc_wrapper(
            state.heap,
            IntlObject(service: IntlLocale, slots: new_slots),
            proto,
          )
        Ok(#(JsObject(ref), State(..state, heap:)))
      }
      "getCalendars" -> {
        let vals = case locale_u_kw(slots, "ca") {
          Some(ca) -> [ca]
          None -> ["gregory"]
        }
        let #(state, arr) = alloc_array(state, list.map(vals, JsString))
        Ok(#(arr, state))
      }
      "getCollations" -> {
        let vals = case locale_u_kw(slots, "co") {
          Some(co) -> [co]
          None -> ["emoji", "eor"]
        }
        let #(state, arr) = alloc_array(state, list.map(vals, JsString))
        Ok(#(arr, state))
      }
      "getHourCycles" -> {
        let vals = case locale_u_kw(slots, "hc") {
          Some(hc) -> [hc]
          None -> ["h12"]
        }
        let #(state, arr) = alloc_array(state, list.map(vals, JsString))
        Ok(#(arr, state))
      }
      "getNumberingSystems" -> {
        let vals = case locale_u_kw(slots, "nu") {
          Some(nu) -> [nu]
          None -> ["latn"]
        }
        let #(state, arr) = alloc_array(state, list.map(vals, JsString))
        Ok(#(arr, state))
      }
      "getTimeZones" ->
        case locale_lid(slots) {
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
        let lang = case locale_lid(slots) {
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
        let first_day = case locale_u_kw(slots, "fw") {
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
