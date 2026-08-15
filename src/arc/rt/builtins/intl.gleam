//// The Intl namespace (ECMA-402): Intl.getCanonicalLocales and the service
//// constructors NumberFormat and DurationFormat, plus the §18 locale-
//// sensitive overrides on Number.prototype / BigInt.prototype.
////
//// Locale data is root/English with per-language separators: formatters
//// implement CLDR patterns in intl_format.gleam; tag parsing/
//// canonicalization is in intl_locale.gleam.

import arc/rt/builtins/common
import arc/rt/builtins/helpers.{first_arg_or_undefined}
import arc/rt/builtins/intl_format.{PElement, PLiteral} as fmt
import arc/rt/builtins/intl_locale as tags
import arc/rt/builtins/realm_ops
import arc/rt/call as rt_call
import arc/rt/intl_data.{
  type CompactDisplay, type CurrencyDisplay, type CurrencySign,
  type DurationBaseStyle, type DurationDisplay, type DurationFormatState,
  type DurationUnitOptions, type DurationUnitStyle, type IntlData,
  type IntlDigitOptions, type IntlService, type IntlUseGrouping, type Notation,
  type NumStyle, type NumberFormatState, type RoundingMode,
  type RoundingPriority, type SignDisplay, type TrailingZeroDisplay,
  type UnitDisplay, BsDigital, BsLong, BsNarrow, BsShort, CompactLong,
  CompactShort, CurAccounting, CurCode, CurName, CurNarrowSymbol, CurStandard,
  CurSymbol, DisplayAlways, DisplayAuto, DurFractional, DurLong, DurNarrow,
  DurNumeric, DurShort, DurTwoDigit, DurationFormatData, DurationFormatState,
  DurationUnitOptions, GroupingAlways, GroupingAuto, GroupingMin2, GroupingNever,
  IntlDigitOptions, IntlDurationFormat, IntlNumberFormat, LLong, LNarrow, LShort,
  NotationCompact, NotationEngineering, NotationScientific, NotationStandard,
  NumberFormatData, NumberFormatState, PriorityAuto, PriorityLessPrecision,
  PriorityMorePrecision, RoundCeil, RoundExpand, RoundFloor, RoundHalfCeil,
  RoundHalfEven, RoundHalfExpand, RoundHalfFloor, RoundHalfTrunc, RoundTrunc,
  SignAlways, SignAuto, SignExceptZero, SignNegative, SignNever, StyleCurrency,
  StyleDecimal, StylePercent, StyleUnit, TzdAuto, TzdStripIfInteger, UnitList,
  UnitLong, UnitNarrow, UnitShort,
}
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type IntlHostOverrideName, type IntlMethodName,
  type IntlNative, type JsNum, type JsVal, BigIntObj, BigIntToLocaleString,
  Index, IntlBoundGetter, IntlBoundMethod, IntlConstructor, IntlFormat,
  IntlFormatRange, IntlFormatRangeToParts, IntlFormatToParts,
  IntlGetCanonicalLocales, IntlHostOverride, IntlMethod, IntlN, IntlObj,
  IntlResolvedOptions, IntlSupportedLocalesOf, JFloat, JInt, JNan, JNegInf,
  JPosInf, KBig, KBool, KHandle, KNum, KStr, KUndef, Named, NumberObj,
  NumberToLocaleString, SObject, StringKey, classify, mk_bool, mk_number,
  mk_object, mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

// ============================================================================
// Initialization
// ============================================================================

/// Build the Intl namespace and install the §18 overrides on
/// Number.prototype / BigInt.prototype. Returns the namespace object.
pub fn init(
  st: Agent,
  object_proto: Handle,
  function_proto: Handle,
  number_proto: Handle,
  bigint_proto: Handle,
) -> #(Handle, Agent) {
  let #(number_format, st) =
    init_service(
      st,
      object_proto,
      function_proto,
      IntlNumberFormat,
      [
        service_method(IntlNumberFormat, IntlFormatToParts, 1),
        service_method(IntlNumberFormat, IntlFormatRange, 2),
        service_method(IntlNumberFormat, IntlFormatRangeToParts, 2),
      ],
      [#("format", IntlN(IntlBoundGetter(IntlNumberFormat)))],
    )
  let #(duration_format, st) =
    init_service(
      st,
      object_proto,
      function_proto,
      IntlDurationFormat,
      [
        service_method(IntlDurationFormat, IntlFormat, 1),
        service_method(IntlDurationFormat, IntlFormatToParts, 1),
      ],
      [],
    )

  // --- Namespace object ---
  let #(ns_methods, st) =
    common.alloc_methods(st, function_proto, [
      #("getCanonicalLocales", IntlN(IntlGetCanonicalLocales), 1),
    ])
  let #(nf_prop, st) =
    common.builtin_property(st, mk_object(number_format.constructor))
  let #(df_prop, st) =
    common.builtin_property(st, mk_object(duration_format.constructor))
  let #(namespace, st) =
    common.init_namespace(
      st,
      object_proto,
      "Intl",
      list.append(ns_methods, [
        #("NumberFormat", nf_prop),
        #("DurationFormat", df_prop),
      ]),
    )

  // ECMA-402 §18: locale-sensitive overrides on Number/BigInt.
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

  #(namespace, st)
}

/// Build one formatter service: prototype methods + accessor getters +
/// resolvedOptions + supportedLocalesOf static + @@toStringTag.
fn init_service(
  st: Agent,
  object_proto: Handle,
  function_proto: Handle,
  service: IntlService,
  methods: List(#(String, types.NativeToken, Int)),
  accessors: List(#(String, types.NativeToken)),
) -> #(types.BuiltinPair, Agent) {
  let name = service_name(service)
  let #(proto_methods, st) =
    common.alloc_methods(st, function_proto, [
      #("resolvedOptions", IntlN(IntlResolvedOptions(service)), 0),
      ..methods
    ])
  let #(proto_accessors, st) =
    common.alloc_getters(st, function_proto, accessors)
  let #(slo, st) =
    common.alloc_methods(st, function_proto, [
      #("supportedLocalesOf", IntlN(IntlSupportedLocalesOf(service)), 1),
    ])
  let #(bt, st) =
    common.init_type(
      st,
      object_proto,
      function_proto,
      list.append(proto_accessors, proto_methods),
      fn(proto) { IntlN(IntlConstructor(service:, proto:)) },
      name,
      0,
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
    IntlConstructor(service:, proto:) ->
      construct_service(st, service, proto, args, mk_undefined())
    IntlSupportedLocalesOf(_service) -> supported_locales_of(st, args)
    IntlResolvedOptions(service:) -> resolved_options(st, service, this)
    IntlBoundGetter(service:) -> bound_getter(st, service, this)
    IntlBoundMethod(service:, target:) ->
      bound_method(st, service, target, args)
    IntlMethod(service:, method:) -> run_method(st, service, method, this, args)
    IntlHostOverride(which:) -> run_host_override(st, which, this, args)
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
  let found =
    helpers.brand_of(st, this, fn(kind) {
      case kind {
        IntlObj(data:, bound:) ->
          case intl_data.intl_service(data) == service {
            True -> Some(#(data, bound))
            False -> None
          }
        _ -> None
      }
    })
  case found {
    Some(#(#(data, bound), h)) -> #(h, data, bound)
    None ->
      rt_val.t_throw_type_error(
        st,
        method <> " called on incompatible receiver",
      )
  }
}

/// `branded` narrowed to NumberFormat: the concrete state record instead of
/// the sum plus a hand-written "cannot happen" arm at every caller.
fn branded_number_format(
  st: Agent,
  this: JsVal,
  method: String,
) -> #(Handle, NumberFormatState, Option(Handle)) {
  let #(h, data, bound) = branded(st, this, IntlNumberFormat, method)
  case data {
    NumberFormatData(nf) -> #(h, nf, bound)
    DurationFormatData(_) ->
      rt_val.t_throw_type_error(
        st,
        method <> " called on incompatible receiver",
      )
  }
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
    // Step 4: an Intl.Locale object would pass its [[Locale]] straight
    // through; there is no Locale service, so every object is a list.
    KHandle(h) -> locale_list_from_object(st, h)
    _ -> {
      let #(h, st) = rt_val.t_to_object(st, locales)
      locale_list_from_object(st, h)
    }
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
          // Step 7.c.ii: String or Object only.
          let #(tag_str, st) = case classify(k_value) {
            KStr(s) -> #(s, st)
            KHandle(_) -> rt_val.t_to_string(st, k_value)
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
  case service {
    IntlNumberFormat -> "NumberFormat"
    IntlDurationFormat -> "DurationFormat"
  }
}

fn construct_service(
  st: Agent,
  service: IntlService,
  proto: Handle,
  args: List(JsVal),
  new_target: JsVal,
) -> #(JsVal, Agent) {
  let callable_without_new = case service {
    IntlNumberFormat -> True
    IntlDurationFormat -> False
  }
  case !callable_without_new && rt_val.is_undef(new_target) {
    True ->
      rt_val.t_throw_type_error(
        st,
        "Constructor Intl." <> service_name(service) <> " requires 'new'",
      )
    False -> {
      // §10.1.13 OrdinaryCreateFromConstructor: resolve the prototype from
      // NewTarget so `class Sub extends Intl.X` instances get Sub.prototype.
      // Falls back to the intrinsic `proto` when called without `new`.
      let #(proto, st) = proto_from_new_target(st, new_target, proto)
      let arg0 = first_arg_or_undefined(args)
      let arg1 = helpers.arg_at(args, 1)
      let #(data, st) = case service {
        IntlNumberFormat -> {
          let #(s, st) = number_format_state(st, arg0, arg1)
          #(NumberFormatData(s), st)
        }
        IntlDurationFormat -> {
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

/// §10.1.13.2 GetPrototypeFromConstructor with the intrinsic fallback; an
/// undefined NewTarget (plain call) takes the fallback without a lookup.
fn proto_from_new_target(
  st: Agent,
  new_target: JsVal,
  fallback: Handle,
) -> #(Handle, Agent) {
  case classify(new_target) {
    KHandle(_) -> {
      let #(proto, st) =
        rt_obj.t_get_prop(st, new_target, StringKey(Named("prototype")))
      case classify(proto) {
        KHandle(h) -> #(h, st)
        _ -> #(fallback, st)
      }
    }
    _ -> #(fallback, st)
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
  let props = case data {
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
      ])
    }
    DurationFormatData(df) ->
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
      ])
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

/// The integer/fraction/significant digit resolvedOptions pairs, prepended to
/// `rest`.
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
/// resolvedOptions tail.
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
// Bound method getter (format)
// ============================================================================

/// The `format` accessor getter (§15.3.3): the bound function is created once
/// and cached on the receiver, so the getter is idempotent.
fn bound_getter(
  st: Agent,
  service: IntlService,
  this: JsVal,
) -> #(JsVal, Agent) {
  let method = "Intl." <> service_name(service) <> " bound method getter"
  let #(target, _nf, cached) = branded_number_format(st, this, method)
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
          1,
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

/// The bound `format` function itself: `target` is the receiver captured by
/// `bound_getter`, and its brand is re-checked.
fn bound_method(
  st: Agent,
  service: IntlService,
  target: Handle,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let this = mk_object(target)
  let method = "bound Intl." <> service_name(service) <> " method"
  let #(_h, nf, _bound) = branded_number_format(st, this, method)
  let #(parts, st) = nf_format_parts(st, nf, first_arg_or_undefined(args))
  #(mk_string(fmt.parts_to_string(parts)), st)
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
// Prototype methods
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
  let #(_h, data, _bound) = branded(st, this, service, js_name)
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
    IntlFormat, DurationFormatData(df) -> {
      let #(parts, st) = duration_parts(st, df, arg0)
      #(mk_string(fmt.unit_parts_to_string(parts)), st)
    }
    IntlFormatToParts, DurationFormatData(df) -> {
      let #(parts, st) = duration_parts(st, df, arg0)
      parts_to_js_with_unit(st, parts)
    }
    // `branded` guarantees data matches `service`, so these pairings are
    // methods that were never registered on the receiver's prototype.
    IntlFormat, _ | IntlFormatRange, _ | IntlFormatRangeToParts, _ ->
      rt_val.t_throw_type_error(
        st,
        js_name <> " called on incompatible receiver",
      )
  }
}

/// The Number/BigInt prototype locale-sensitive overrides (ECMA-402 §18) —
/// installed by `init`, no Intl brand check.
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
