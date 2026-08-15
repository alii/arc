//// The Temporal namespace (proposal-temporal, test262 feature "Temporal"):
//// Temporal.Instant — an exact point on the UTC timeline, held as integer
//// nanoseconds since the epoch (|ns| <= 8.64e21).
////
//// ISO 8601 parsing/formatting is in temporal_iso.gleam; named IANA zones
//// resolve through temporal_tz.gleam (system tzdata).

import arc/internal/int_math.{floor_div, floor_mod as math_mod}
import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/builtins/realm_ops
import arc/rt/builtins/temporal_iso.{
  type Precision, type TErr, AutoPrec, FixedPrec, MinutePrec, NoOffset,
  NumericOffset, RangeE, TypeE, Zulu, epoch_ns_to_iso, format_iso_date,
  format_iso_time, format_offset_minutes, is_tz_annotation, ns_max_instant,
  ns_per_day, ns_per_hour, ns_per_minute, ns_per_ms, ns_per_second, ns_per_us,
  parse_iso_datetime_string, parse_offset_part, pow10, utc_epoch_ns,
}
import arc/rt/builtins/temporal_tz
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type InstantGetterName, type InstantMethodName,
  type InstantStaticName, type JsVal, type TemporalNative, HintString,
  InstantCompare, InstantEpochMilliseconds, InstantEpochNanoseconds,
  InstantEquals, InstantFrom, InstantFromEpochMilliseconds,
  InstantFromEpochNanoseconds, InstantRound, InstantToJson,
  InstantToLocaleString, InstantToString, InstantValueOf, JFloat, JInt, JNan,
  JNegInf, JPosInf, KHandle, KNum, KStr, KUndef, Named, SObject, StringKey,
  TemporalInstant, TemporalInstantCtor, TemporalInstantGetter,
  TemporalInstantMethod, TemporalInstantStatic, TemporalN, TemporalObj, classify,
  mk_bigint, mk_bool, mk_number, mk_object, mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/string

// ============================================================================
// Init — Temporal namespace, Temporal.Instant
// ============================================================================

/// Build the Temporal global. Returns the namespace object.
pub fn init(
  st: Agent,
  object_proto: Handle,
  function_proto: Handle,
) -> #(Handle, Agent) {
  let getters =
    list.map([InstantEpochMilliseconds, InstantEpochNanoseconds], fn(g) {
      #(instant_getter_name(g), TemporalN(TemporalInstantGetter(g)))
    })
  let #(getter_props, st) = common.alloc_getters(st, function_proto, getters)
  // The methods and statics allocate Instants, so they need the prototype
  // handle: reserve it via init_type first, then install them.
  let #(bt, st) =
    common.init_type(
      st,
      object_proto,
      function_proto,
      getter_props,
      fn(proto) { TemporalN(TemporalInstantCtor(proto:)) },
      "Instant",
      1,
      [],
    )
  let proto = bt.prototype
  let #(method_props, st) =
    common.alloc_methods(
      st,
      function_proto,
      list.map(
        [
          #(InstantRound, 1),
          #(InstantEquals, 1),
          #(InstantToString, 0),
          #(InstantToLocaleString, 0),
          #(InstantToJson, 0),
          #(InstantValueOf, 0),
        ],
        fn(m) {
          #(
            instant_method_name(m.0),
            TemporalN(TemporalInstantMethod(m.0, proto)),
            m.1,
          )
        },
      ),
    )
  let st = add_named_properties(st, proto, method_props)
  let st = common.add_to_string_tag(st, proto, "Temporal.Instant")
  let #(static_props, st) =
    common.alloc_methods(
      st,
      function_proto,
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
            TemporalN(TemporalInstantStatic(s.0, proto)),
            s.1,
          )
        },
      ),
    )
  let st = add_named_properties(st, bt.constructor, static_props)

  // Temporal namespace itself
  let #(ctor_prop, st) = common.builtin_property(st, mk_object(bt.constructor))
  common.init_namespace(st, object_proto, "Temporal", [#("Instant", ctor_prop)])
}

fn add_named_properties(
  st: Agent,
  h: Handle,
  props: List(#(String, types.Property)),
) -> Agent {
  list.fold(props, st, fn(st, p) { common.add_named_property(st, h, p.0, p.1) })
}

fn instant_getter_name(g: InstantGetterName) -> String {
  case g {
    InstantEpochMilliseconds -> "epochMilliseconds"
    InstantEpochNanoseconds -> "epochNanoseconds"
  }
}

fn instant_method_name(m: InstantMethodName) -> String {
  case m {
    InstantRound -> "round"
    InstantEquals -> "equals"
    InstantToString -> "toString"
    InstantToLocaleString -> "toLocaleString"
    InstantToJson -> "toJSON"
    InstantValueOf -> "valueOf"
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
    TemporalInstantCtor(..) ->
      rt_val.t_throw_type_error(st, "Temporal constructor requires new")
    TemporalInstantStatic(name:, proto:) ->
      instant_static(st, name, proto, args)
    TemporalInstantGetter(getter:) -> instant_getter(st, getter, this)
    TemporalInstantMethod(method:, proto:) ->
      instant_method(st, method, proto, this, args)
  }
}

pub fn dispatch_construct(
  st: Agent,
  native: TemporalNative,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case native {
    TemporalInstantCtor(proto:) -> instant_ctor(st, proto, args, new_target)
    _ -> rt_val.t_throw_type_error(st, "not a constructor")
  }
}

// ============================================================================
// Errors and branding
// ============================================================================

/// Raise a pure helper's `TErr` as the matching JS error.
fn throw_terr(st: Agent, e: TErr) -> a {
  case e {
    RangeE(msg) -> rt_val.t_throw_range_error(st, msg)
    TypeE(msg) -> rt_val.t_throw_type_error(st, msg)
  }
}

/// Unwrap a pure helper's Result, raising its `TErr`.
fn terr(st: Agent, r: Result(a, TErr)) -> a {
  case r {
    Ok(v) -> v
    Error(e) -> throw_terr(st, e)
  }
}

/// RequireInternalSlot(this, [[InitializedTemporalInstant]]).
fn require_instant(st: Agent, this: JsVal, name: String) -> Int {
  let found =
    helpers.brand_of(st, this, fn(kind) {
      case kind {
        TemporalObj(data: TemporalInstant(epoch_ns:)) -> Some(epoch_ns)
        _ -> None
      }
    })
  case found {
    Some(#(ns, _h)) -> ns
    None ->
      rt_val.t_throw_type_error(
        st,
        "Temporal.Instant.prototype."
          <> name
          <> " called on incompatible receiver",
      )
  }
}

fn make_instant(st: Agent, proto: Handle, ns: Int) -> #(JsVal, Agent) {
  let #(h, st) =
    realm_ops.alloc_wrapper(
      st,
      TemporalObj(TemporalInstant(epoch_ns: ns)),
      proto,
    )
  #(mk_object(h), st)
}

fn validate_epoch_ns(ns: Int) -> Result(Int, TErr) {
  case int.absolute_value(ns) <= ns_max_instant {
    True -> Ok(ns)
    False -> Error(RangeE("instant outside valid range"))
  }
}

// ============================================================================
// Constructor and statics
// ============================================================================

/// new Temporal.Instant(epochNanoseconds: BigInt)
fn instant_ctor(
  st: Agent,
  fallback_proto: Handle,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  let #(ns, st) = rt_val.t_to_bigint(st, helpers.arg_at(args, 0))
  case int.absolute_value(ns) <= ns_max_instant {
    False -> rt_val.t_throw_range_error(st, "epoch nanoseconds out of range")
    True -> {
      // GetPrototypeFromConstructor (via OrdinaryCreateFromConstructor):
      // an observable Get that happens after argument validation.
      let #(proto, st) = proto_from_new_target(st, new_target, fallback_proto)
      let #(h, st) =
        realm_ops.alloc_wrapper(
          st,
          TemporalObj(TemporalInstant(epoch_ns: ns)),
          proto,
        )
      #(h, st)
    }
  }
}

/// §10.1.13.2 GetPrototypeFromConstructor with the intrinsic fallback.
fn proto_from_new_target(
  st: Agent,
  new_target: JsVal,
  fallback: Handle,
) -> #(Handle, Agent) {
  let #(proto, st) =
    rt_obj.t_get_prop(st, new_target, StringKey(Named("prototype")))
  case classify(proto) {
    KHandle(h) -> #(h, st)
    _ -> #(fallback, st)
  }
}

fn instant_static(
  st: Agent,
  name: InstantStaticName,
  proto: Handle,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case name {
    InstantFrom -> {
      let #(ns, st) = to_temporal_instant(st, helpers.arg_at(args, 0))
      make_instant(st, proto, ns)
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
            True -> make_instant(st, proto, ns)
          }
        }
      }
    }
    InstantFromEpochNanoseconds -> {
      let #(ns, st) = rt_val.t_to_bigint(st, helpers.arg_at(args, 0))
      case int.absolute_value(ns) <= ns_max_instant {
        False ->
          rt_val.t_throw_range_error(st, "epoch nanoseconds out of range")
        True -> make_instant(st, proto, ns)
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

/// ToTemporalInstant(item) → epoch ns.
fn to_temporal_instant(st: Agent, item: JsVal) -> #(Int, Agent) {
  case classify(item) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: TemporalObj(data: TemporalInstant(epoch_ns:)), ..) -> #(
          epoch_ns,
          st,
        )
        _ -> {
          let #(prim, st) = rt_val.t_to_primitive(st, item, HintString)
          case classify(prim) {
            KStr(s) -> parse_instant_to_ns(st, s)
            _ ->
              rt_val.t_throw_type_error(
                st,
                "cannot convert to a Temporal.Instant",
              )
          }
        }
      }
    KStr(s) -> parse_instant_to_ns(st, s)
    _ -> rt_val.t_throw_type_error(st, "cannot convert to a Temporal.Instant")
  }
}

fn parse_instant_to_ns(st: Agent, s: String) -> #(Int, Agent) {
  // Per ParseTemporalInstantString, a [u-ca=...] annotation is only
  // syntax-checked (done by parse_iso_datetime_string); its value is
  // IGNORED for Instant, so unknown calendars must not throw here.
  case parse_iso_datetime_string(s) {
    None -> rt_val.t_throw_range_error(st, "invalid instant string: " <> s)
    Some(p) ->
      case p.time {
        Some(t) ->
          case p.offset {
            NoOffset ->
              rt_val.t_throw_range_error(
                st,
                "instant string requires a UTC offset",
              )
            Zulu | NumericOffset(_, _) -> {
              let off = case p.offset {
                NumericOffset(o, _) -> o
                Zulu | NoOffset -> 0
              }
              let ns = utc_epoch_ns(p.date, t) - off
              #(terr(st, validate_epoch_ns(ns)), st)
            }
          }
        None -> rt_val.t_throw_range_error(st, "instant string requires a time")
      }
  }
}

// ============================================================================
// Getters
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
// Methods
// ============================================================================

fn instant_method(
  st: Agent,
  m: InstantMethodName,
  proto: Handle,
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
      // Read options: fractionalSecondDigits, roundingMode, smallestUnit,
      // timeZone (alphabetical).
      let #(#(prec, su, sinc, mode), st) = to_string_time_options(st, opts)
      let #(tz_opt, st) = case opts {
        None -> #(mk_undefined(), st)
        Some(h) ->
          rt_obj.t_get_prop(st, mk_object(h), StringKey(Named("timeZone")))
      }
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
            <> format_offset_rounded(off)
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
    InstantRound -> {
      let #(#(su, inc, mode), st) = round_options(st, helpers.arg_at(args, 0))
      let u_ns = time_unit_ns(su)
      // For Instant: increment*unit must divide 24h.
      let max = ns_per_day / u_ns
      case inc >= 1 && inc <= max && max % inc == 0 {
        False -> rt_val.t_throw_range_error(st, "invalid roundingIncrement")
        True -> {
          let rounded = round_to_increment(ns, inc * u_ns, mode)
          case int.absolute_value(rounded) <= ns_max_instant {
            False ->
              rt_val.t_throw_range_error(st, "instant outside valid range")
            True -> make_instant(st, proto, rounded)
          }
        }
      }
    }
  }
}

fn format_instant(ns: Int, prec: Precision) -> String {
  let #(d, t) = epoch_ns_to_iso(ns, 0)
  format_iso_date(d) <> "T" <> format_iso_time(t, prec) <> "Z"
}

// ============================================================================
// Options handling
// ============================================================================

/// GetOptionsObject: undefined → None, object → Some(handle), else TypeError.
fn get_options_object(st: Agent, v: JsVal) -> #(Option(Handle), Agent) {
  case classify(v) {
    KUndef -> #(None, st)
    KHandle(h) -> #(Some(h), st)
    _ -> rt_val.t_throw_type_error(st, "options must be an object or undefined")
  }
}

fn opt_get(st: Agent, opts: Option(Handle), key: String) -> #(JsVal, Agent) {
  case opts {
    None -> #(mk_undefined(), st)
    Some(h) -> rt_obj.t_get_prop(st, mk_object(h), StringKey(Named(key)))
  }
}

/// GetOption for an enum-valued option: `allowed` maps each accepted string
/// to its variant, so the allow-list and the parse are the same table and no
/// consumer ever sees the raw string. Anything else is a RangeError here.
fn get_enum_option(
  st: Agent,
  opts: Option(Handle),
  key: String,
  allowed: List(#(String, a)),
  default: a,
) -> #(a, Agent) {
  let #(v, st) = opt_get(st, opts, key)
  case classify(v) {
    KUndef -> #(default, st)
    _ -> {
      let #(s, st) = rt_val.t_to_string(st, v)
      case list.key_find(allowed, s) {
        Ok(parsed) -> #(parsed, st)
        Error(Nil) ->
          rt_val.t_throw_range_error(
            st,
            s <> " is not a valid value for option " <> key,
          )
      }
    }
  }
}

// ============================================================================
// Units and rounding
// ============================================================================

type Unit {
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
type RoundingMode {
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

/// The units that have a fixed nanosecond length: day and below. Year, month
/// and week are deliberately absent — a calendar unit's length depends on the
/// date it is measured from, so it cannot be turned into a nanosecond count.
type TimeUnit {
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

/// Read a unit-valued option ("smallestUnit"). An absent option is None.
fn get_unit_option(
  st: Agent,
  opts: Option(Handle),
  key: String,
) -> #(Option(Unit), Agent) {
  let #(v, st) = opt_get(st, opts, key)
  case classify(v) {
    KUndef -> #(None, st)
    _ -> {
      let #(s, st) = rt_val.t_to_string(st, v)
      case singular_unit(s) {
        Some(u) -> #(Some(u), st)
        None ->
          rt_val.t_throw_range_error(
            st,
            s <> " is not a valid value for " <> key,
          )
      }
    }
  }
}

fn get_rounding_mode_option(
  st: Agent,
  opts: Option(Handle),
  default: RoundingMode,
) -> #(RoundingMode, Agent) {
  get_enum_option(
    st,
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
  st: Agent,
  opts: Option(Handle),
) -> #(Int, Agent) {
  let #(v, st) = opt_get(st, opts, "roundingIncrement")
  case classify(v) {
    KUndef -> #(1, st)
    _ -> {
      let #(n, st) = rt_val.t_to_number(st, v)
      // ToIntegerWithTruncation: truncate, then bounds-check 1..1e9.
      let i = case n {
        JInt(i) -> Some(i)
        JFloat(f) -> Some(rt_val.float_to_int(f))
        JNan | JPosInf | JNegInf -> None
      }
      case i {
        Some(i) if i >= 1 && i <= 1_000_000_000 -> #(i, st)
        _ -> rt_val.t_throw_range_error(st, "invalid roundingIncrement")
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

/// Instant toString options: fractionalSecondDigits, roundingMode,
/// smallestUnit (alphabetical). Returns the output precision, the rounding
/// unit (None = no rounding), the increment and mode.
fn to_string_time_options(
  st: Agent,
  opts: Option(Handle),
) -> #(#(Precision, Option(TimeUnit), Int, RoundingMode), Agent) {
  let #(digits, st) = get_fractional_digits(st, opts)
  let #(mode, st) = get_rounding_mode_option(st, opts, Trunc)
  let #(su, st) = get_unit_option(st, opts, "smallestUnit")
  #(terr(st, seconds_string_precision(digits, su, mode)), st)
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
  st: Agent,
  opts: Option(Handle),
) -> #(FractionalDigits, Agent) {
  let #(v, st) = opt_get(st, opts, "fractionalSecondDigits")
  case classify(v) {
    KUndef -> #(DigitsAuto, st)
    KNum(JInt(i)) ->
      case i >= 0 && i <= 9 {
        True -> #(DigitsFixed(i), st)
        False ->
          rt_val.t_throw_range_error(st, "invalid fractionalSecondDigits")
      }
    KNum(JFloat(f)) -> {
      // floor, then 0..9 bounds (GetTemporalFractionalSecondDigitsOption).
      let i = rt_val.float_to_int(float.floor(f))
      case i >= 0 && i <= 9 {
        True -> #(DigitsFixed(i), st)
        False ->
          rt_val.t_throw_range_error(st, "invalid fractionalSecondDigits")
      }
    }
    KNum(_) -> rt_val.t_throw_range_error(st, "invalid fractionalSecondDigits")
    // Non-number: ToString it; only "auto" is accepted. Symbols raise
    // TypeError from the string coercion.
    _ -> {
      let #(s, st) = rt_val.t_to_string(st, v)
      case s {
        "auto" -> #(DigitsAuto, st)
        _ -> rt_val.t_throw_range_error(st, "invalid fractionalSecondDigits")
      }
    }
  }
}

/// round() options: positional string shorthand or object with smallestUnit
/// (required), roundingIncrement, roundingMode. Instant has no days, so a
/// calendar unit or `day` can never reach the rounding.
fn round_options(
  st: Agent,
  arg: JsVal,
) -> #(#(TimeUnit, Int, RoundingMode), Agent) {
  case classify(arg) {
    KUndef -> rt_val.t_throw_type_error(st, "options parameter is required")
    KStr(s) ->
      case singular_unit(s) |> option.then(round_unit) {
        Some(u) -> #(#(u, 1, HalfExpand), st)
        None -> rt_val.t_throw_range_error(st, "invalid smallestUnit")
      }
    KHandle(h) -> {
      let opts = Some(h)
      let #(inc, st) = get_rounding_increment_option(st, opts)
      let #(mode, st) = get_rounding_mode_option(st, opts, HalfExpand)
      let #(su, st) = get_unit_option(st, opts, "smallestUnit")
      case su {
        None -> rt_val.t_throw_range_error(st, "smallestUnit is required")
        Some(u) ->
          case round_unit(u) {
            Some(tu) -> #(#(tu, inc, mode), st)
            None -> rt_val.t_throw_range_error(st, "invalid smallestUnit")
          }
      }
    }
    _ -> rt_val.t_throw_type_error(st, "invalid options")
  }
}

/// A round() smallestUnit for Instant: a fixed-length unit below `day`.
fn round_unit(u: Unit) -> Option(TimeUnit) {
  case as_time_unit(u) {
    Some(UDay) -> None
    other -> other
  }
}

// ============================================================================
// Time zone handling — named IANA zones (system tzdata via temporal_tz),
// "UTC", and fixed numeric offsets
// ============================================================================

/// A resolved time zone. There is no "unknown" variant: an unrecognised
/// identifier is a RangeError at parse time.
type TimeZone {
  /// The distinguished "UTC" zone (offset 0, no transitions).
  TzUtc
  /// A fixed numeric offset ("+05:30"), stored in nanoseconds. No transitions.
  TzOffset(ns: Int)
  /// A named IANA zone, validated against the system tzdata.
  TzNamed(zone: temporal_tz.Zone)
}

/// Parse + validate a time zone identifier into a resolved `TimeZone`.
/// Accepts bare identifiers ("UTC", "+05:30") and ISO date-time strings that
/// carry a [TimeZone] annotation, a Z designator, or a numeric offset
/// (ParseTemporalTimeZoneString).
fn parse_time_zone_id(id: String) -> Result(TimeZone, TErr) {
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

/// ParseTimeZoneIdentifier: bare identifiers only (UTC, offsets, IANA names).
fn parse_time_zone_id_strict(id: String) -> Result(TimeZone, StrictTzError) {
  case string.uppercase(id) == "UTC" {
    True -> Ok(TzUtc)
    False ->
      case parse_offset_tz_id(id) {
        Some(ns) -> Ok(TzOffset(ns:))
        None ->
          case temporal_tz.lookup(id) {
            Ok(zone) -> Ok(TzNamed(zone:))
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
fn tz_from_datetime_string(s: String) -> Result(TimeZone, TErr) {
  case parse_iso_datetime_string(s) {
    None -> Error(RangeE("invalid time zone: " <> s))
    Some(p) ->
      case p.tz {
        Some(tz_str) ->
          case string.uppercase(tz_str) == "UTC" {
            True -> Ok(TzUtc)
            False ->
              case parse_offset_tz_id(tz_str) {
                Some(ns) -> Ok(TzOffset(ns:))
                None ->
                  temporal_tz.lookup(tz_str)
                  |> result.map(fn(zone) { TzNamed(zone:) })
                  |> result.replace_error(unsupported_tz(tz_str))
              }
          }
        None ->
          case p.offset {
            Zulu -> Ok(TzUtc)
            NumericOffset(off, sub_minute) ->
              // The offset must be syntactically minute-precision: a
              // seconds component (even ":00") is not a valid zone.
              case !sub_minute && off % ns_per_minute == 0 {
                True -> Ok(TzOffset(ns: off))
                False ->
                  Error(RangeE("sub-minute offset not valid as a time zone"))
              }
            NoOffset -> Error(RangeE("no time zone found in string: " <> s))
          }
      }
  }
}

/// Offset time zone identifier: ±HH[:MM] (minute precision only).
/// Returns the offset in nanoseconds.
fn parse_offset_tz_id(id: String) -> Option(Int) {
  let signed = case id {
    "+" <> _ | "-" <> _ -> True
    _ -> False
  }
  case signed {
    False -> None
    True ->
      case parse_offset_part(id) {
        // A seconds component (sub-minute syntax) is not allowed in an
        // offset time zone identifier, even when it is ":00".
        Some(#(NumericOffset(ns, False), "")) ->
          case ns % ns_per_minute == 0 && int.absolute_value(ns) < ns_per_day {
            True -> Some(ns)
            False -> None
          }
        _ -> None
      }
  }
}

/// The identifier for a resolved time zone.
fn time_zone_id(tz: TimeZone) -> String {
  case tz {
    TzUtc -> "UTC"
    TzOffset(ns:) -> format_offset_minutes(ns)
    TzNamed(zone:) -> temporal_tz.zone_id(zone)
  }
}

fn unsupported_tz(tz: String) -> TErr {
  RangeE("time zone " <> tz <> " is not supported")
}

/// A zone whose name we accepted but whose tzdata will not load is a broken
/// install, not an unknown identifier: same RangeError, but the reason says so.
fn unloadable_tz(tz: TimeZone, error: temporal_tz.TzError) -> TErr {
  RangeE(
    "time zone "
    <> time_zone_id(tz)
    <> " cannot be loaded: "
    <> temporal_tz.describe(error),
  )
}

/// GetOffsetNanosecondsFor — UTC offset of `tz` at an exact instant.
/// RangeError when a named zone's TZif data cannot be loaded.
fn tz_offset_ns_at(tz: TimeZone, epoch_ns: Int) -> Result(Int, TErr) {
  case tz {
    TzUtc -> Ok(0)
    TzOffset(ns:) -> Ok(ns)
    TzNamed(zone:) ->
      temporal_tz.offset_ns_at(zone, epoch_ns)
      |> result.map_error(unloadable_tz(tz, _))
  }
}

/// Offset rounded to the nearest minute, for ISO string display
/// (FormatDateTimeUTCOffsetRounded).
fn format_offset_rounded(offset_ns: Int) -> String {
  format_offset_minutes(round_to_increment(offset_ns, ns_per_minute, HalfExpand))
}
