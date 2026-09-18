import arc/bytecode/error_kind.{type JsError, JsError, RangeError}
import arc/bytecode/key.{Named}
import arc/internal/host_time
import arc/internal/int_math.{floor_div, floor_mod}
import arc/internal/temporal_calendar
import arc/rt/builtins/helpers
import arc/rt/builtins/realm_ops
import arc/rt/builtins/temporal_iso.{
  type Duration, type IsoDate, type IsoDateSlots, type IsoTime, type Overflow,
  type SecondsPrecision, AutoPrecision, Constrain, Duration, IsoDate,
  IsoDateSlots, IsoTime, MinutePrecision, NoOffset, NumericOffset, Reject,
  SubsecondDigits, Zulu, epoch_ns_to_iso, format_iso_date, format_offset_minutes,
  int_sign, is_tz_annotation, max_time_duration_ns, ns_max_instant, ns_per_day,
  ns_per_hour, ns_per_minute, ns_per_ms, ns_per_second, ns_per_us, pad2,
  parse_iso_datetime_string, parse_offset_part, pow10, pow2_32,
  round_to_float_precision, take_some_digits, utc_epoch_ns, zero_duration,
}
import arc/rt/builtins/temporal_tz
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type ObjKind, type TemporalProtos,
  type TemporalStaticName, type TemporalZone, Agent, CompareStatic, FromStatic,
  HintString, IanaZone, JFloat, JInt, JNan, JNegInf, JPosInf, KHandle, KNum,
  KStr, KUndef, OffsetZone, SObject, StringKey, TemporalDate, TemporalDateTime,
  TemporalDuration, TemporalInstant, TemporalMonthDay, TemporalObj, TemporalTime,
  TemporalYearMonth, TemporalZonedDateTime, UtcZone, classify, mk_object,
  mk_undefined,
}
import arc/rt/val as rt_val
import arc/time_zone
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/string

pub fn require_temporal(
  st: Agent,
  this: JsVal,
  type_name: String,
  name: String,
  extract: fn(ObjKind) -> Option(a),
) -> a {
  case helpers.brand_of(st, this, extract) {
    Some(#(v, _h)) -> v
    None ->
      rt_val.t_throw_type_error(
        st,
        "Temporal."
          <> type_name
          <> ".prototype."
          <> name
          <> " called on incompatible receiver",
      )
  }
}

pub fn static_name(s: TemporalStaticName) -> String {
  case s {
    FromStatic -> "from"
    CompareStatic -> "compare"
  }
}

pub fn temporal_data_of(st: Agent, v: JsVal) -> Option(types.TemporalData) {
  case classify(v) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: TemporalObj(data:), ..) -> Some(data)
        _ -> None
      }
    _ -> None
  }
}

pub fn date_slot_of(
  kind: ObjKind,
) -> Option(#(IsoDate, temporal_calendar.Calendar)) {
  case kind {
    TemporalObj(data: TemporalDate(year:, month:, day:, calendar:)) ->
      Some(#(IsoDate(year:, month:, day:), calendar))
    _ -> None
  }
}

pub fn time_slot_of(kind: ObjKind) -> Option(IsoTime) {
  case kind {
    TemporalObj(data: TemporalTime(
      hour:,
      minute:,
      second:,
      millisecond:,
      microsecond:,
      nanosecond:,
    )) ->
      Some(IsoTime(hour, minute, second, millisecond, microsecond, nanosecond))
    _ -> None
  }
}

pub fn date_time_slot_of(
  kind: ObjKind,
) -> Option(#(IsoDate, IsoTime, temporal_calendar.Calendar)) {
  case kind {
    TemporalObj(data: TemporalDateTime(
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
        IsoTime(hour, minute, second, millisecond, microsecond, nanosecond),
        calendar,
      ))
    _ -> None
  }
}

pub fn year_month_slot_of(kind: ObjKind) -> Option(IsoDateSlots) {
  case kind {
    TemporalObj(data: TemporalYearMonth(year:, month:, day:, calendar:)) ->
      Some(IsoDateSlots(IsoDate(year:, month:, day:), calendar))
    _ -> None
  }
}

pub fn month_day_slot_of(kind: ObjKind) -> Option(IsoDateSlots) {
  case kind {
    TemporalObj(data: TemporalMonthDay(month:, day:, ref_year:, calendar:)) ->
      Some(IsoDateSlots(IsoDate(year: ref_year, month:, day:), calendar))
    _ -> None
  }
}

pub fn duration_slot_of(kind: ObjKind) -> Option(Duration) {
  case kind {
    TemporalObj(data: TemporalDuration(
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
      Some(Duration(
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

pub fn instant_slot_of(kind: ObjKind) -> Option(Int) {
  case kind {
    TemporalObj(data: TemporalInstant(epoch_ns:)) -> Some(epoch_ns)
    _ -> None
  }
}

pub fn zoned_slot_of(
  kind: ObjKind,
) -> Option(#(Int, TemporalZone, temporal_calendar.Calendar)) {
  case kind {
    TemporalObj(data: TemporalZonedDateTime(epoch_ns:, time_zone:, calendar:)) ->
      Some(#(epoch_ns, time_zone, calendar))
    _ -> None
  }
}

fn alloc_value(
  st: Agent,
  data: types.TemporalData,
  proto: Handle,
) -> #(JsVal, Agent) {
  let #(h, st) = realm_ops.alloc_object(st, TemporalObj(data), proto)
  #(mk_object(h), st)
}

pub fn make_date(
  st: Agent,
  protos: TemporalProtos,
  d: IsoDate,
) -> #(JsVal, Agent) {
  make_date_cal(st, protos, d, temporal_calendar.Iso8601)
}

pub fn make_date_cal(
  st: Agent,
  protos: TemporalProtos,
  d: IsoDate,
  cal: temporal_calendar.Calendar,
) -> #(JsVal, Agent) {
  alloc_value(
    st,
    TemporalDate(year: d.year, month: d.month, day: d.day, calendar: cal),
    protos.plain_date,
  )
}

pub fn make_time(
  st: Agent,
  protos: TemporalProtos,
  t: IsoTime,
) -> #(JsVal, Agent) {
  alloc_value(
    st,
    TemporalTime(
      hour: t.hour,
      minute: t.minute,
      second: t.second,
      millisecond: t.millisecond,
      microsecond: t.microsecond,
      nanosecond: t.nanosecond,
    ),
    protos.plain_time,
  )
}

pub fn make_date_time(
  st: Agent,
  protos: TemporalProtos,
  d: IsoDate,
  t: IsoTime,
) -> #(JsVal, Agent) {
  make_date_time_cal(st, protos, d, t, temporal_calendar.Iso8601)
}

pub fn make_date_time_cal(
  st: Agent,
  protos: TemporalProtos,
  d: IsoDate,
  t: IsoTime,
  cal: temporal_calendar.Calendar,
) -> #(JsVal, Agent) {
  alloc_value(
    st,
    TemporalDateTime(
      year: d.year,
      month: d.month,
      day: d.day,
      hour: t.hour,
      minute: t.minute,
      second: t.second,
      millisecond: t.millisecond,
      microsecond: t.microsecond,
      nanosecond: t.nanosecond,
      calendar: cal,
    ),
    protos.plain_date_time,
  )
}

pub fn make_year_month(
  st: Agent,
  protos: TemporalProtos,
  y: Int,
  m: Int,
  ref_day: Int,
) -> #(JsVal, Agent) {
  make_year_month_cal(st, protos, y, m, ref_day, temporal_calendar.Iso8601)
}

pub fn make_year_month_cal(
  st: Agent,
  protos: TemporalProtos,
  y: Int,
  m: Int,
  ref_day: Int,
  cal: temporal_calendar.Calendar,
) -> #(JsVal, Agent) {
  alloc_value(
    st,
    TemporalYearMonth(year: y, month: m, day: ref_day, calendar: cal),
    protos.plain_year_month,
  )
}

pub fn make_month_day_cal(
  st: Agent,
  protos: TemporalProtos,
  m: Int,
  d: Int,
  ref_year: Int,
  cal: temporal_calendar.Calendar,
) -> #(JsVal, Agent) {
  alloc_value(
    st,
    TemporalMonthDay(month: m, day: d, ref_year:, calendar: cal),
    protos.plain_month_day,
  )
}

pub fn make_duration(
  st: Agent,
  protos: TemporalProtos,
  dur: Duration,
) -> #(JsVal, Agent) {
  let d = map_duration_fields(dur, round_to_float_precision)
  alloc_value(
    st,
    TemporalDuration(
      years: d.years,
      months: d.months,
      weeks: d.weeks,
      days: d.days,
      hours: d.hours,
      minutes: d.minutes,
      seconds: d.seconds,
      milliseconds: d.milliseconds,
      microseconds: d.microseconds,
      nanoseconds: d.nanoseconds,
    ),
    protos.duration,
  )
}

pub fn finish_duration(
  st: Agent,
  protos: TemporalProtos,
  dur: Duration,
) -> #(JsVal, Agent) {
  case is_valid_duration(dur) {
    False -> rt_val.t_throw_range_error(st, "invalid duration")
    True -> make_duration(st, protos, dur)
  }
}

pub fn make_instant(
  st: Agent,
  protos: TemporalProtos,
  ns: Int,
) -> #(JsVal, Agent) {
  alloc_value(st, TemporalInstant(epoch_ns: ns), protos.instant)
}

pub fn make_zoned(
  st: Agent,
  protos: TemporalProtos,
  ns: Int,
  tz: TemporalZone,
) -> #(JsVal, Agent) {
  make_zoned_cal(st, protos, ns, tz, temporal_calendar.Iso8601)
}

pub fn make_zoned_cal(
  st: Agent,
  protos: TemporalProtos,
  ns: Int,
  tz: TemporalZone,
  cal: temporal_calendar.Calendar,
) -> #(JsVal, Agent) {
  alloc_value(
    st,
    TemporalZonedDateTime(epoch_ns: ns, time_zone: tz, calendar: cal),
    protos.zoned_date_time,
  )
}

pub fn apply_new_target_proto(
  st: Agent,
  new_target: JsVal,
  v: JsVal,
) -> #(Handle, Agent) {
  let assert KHandle(obj) = classify(v)
    as "Temporal constructor produced no object"
  let assert SObject(proto: Some(intrinsic), ..) = rt_store.t_cell_get(st, obj)
    as "Temporal constructor produced an object with no prototype"
  let #(proto, st) =
    rt_call.get_prototype_from_constructor(st, new_target, fn(_realm) {
      intrinsic
    })
  let st =
    rt_store.t_cell_update(st, obj, fn(cell) {
      case cell {
        SObject(..) -> SObject(..cell, proto: Some(proto))
        other -> other
      }
    })
  #(obj, st)
}

pub fn to_integer_with_truncation(st: Agent, v: JsVal) -> #(Int, Agent) {
  let #(n, st) = rt_val.t_to_number(st, v)
  case n {
    JInt(i) -> #(i, st)
    JFloat(f) -> #(rt_val.float_to_int(f), st)
    JNan | JPosInf | JNegInf ->
      rt_val.t_throw_range_error(st, "not a finite number")
  }
}

pub fn to_positive_integer_with_truncation(
  st: Agent,
  v: JsVal,
) -> #(Int, Agent) {
  let #(n, st) = to_integer_with_truncation(st, v)
  case n > 0 {
    True -> #(n, st)
    False -> rt_val.t_throw_range_error(st, "expected a positive integer")
  }
}

pub fn to_integer_if_integral(st: Agent, v: JsVal) -> #(Int, Agent) {
  let #(n, st) = rt_val.t_to_number(st, v)
  case n {
    JInt(i) -> #(i, st)
    JFloat(f) -> {
      let i = rt_val.float_to_int(f)
      let fi = int.to_float(i)
      // arithmetic compare: term == treats -0.0 != 0.0
      case f >=. fi && f <=. fi {
        True -> #(i, st)
        False -> rt_val.t_throw_range_error(st, "expected an integral number")
      }
    }
    JNan | JPosInf | JNegInf ->
      rt_val.t_throw_range_error(st, "expected an integral number")
  }
}

pub fn integral_int_arg_or(
  st: Agent,
  args: List(JsVal),
  idx: Int,
  default: Int,
) -> #(Int, Agent) {
  let v = helpers.arg_at(args, idx)
  case classify(v) {
    KUndef -> #(default, st)
    _ -> to_integer_if_integral(st, v)
  }
}

pub fn truncated_int_arg(
  st: Agent,
  args: List(JsVal),
  idx: Int,
) -> #(Int, Agent) {
  to_integer_with_truncation(st, helpers.arg_at(args, idx))
}

pub fn truncated_int_arg_or(
  st: Agent,
  args: List(JsVal),
  idx: Int,
  default: Int,
) -> #(Int, Agent) {
  let v = helpers.arg_at(args, idx)
  case classify(v) {
    KUndef -> #(default, st)
    _ -> to_integer_with_truncation(st, v)
  }
}

pub fn get_options_object(st: Agent, v: JsVal) -> #(Option(Handle), Agent) {
  case classify(v) {
    KUndef -> #(None, st)
    KHandle(h) -> #(Some(h), st)
    _ -> rt_val.t_throw_type_error(st, "options must be an object or undefined")
  }
}

pub fn opt_get(
  st: Agent,
  opts: Option(Handle),
  key: String,
) -> #(JsVal, Agent) {
  case opts {
    None -> #(mk_undefined(), st)
    Some(h) -> rt_obj.t_get_prop(st, mk_object(h), StringKey(Named(key)))
  }
}

pub fn get_enum_option(
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

pub type Disambiguation {
  Compatible
  Earlier
  Later
  RejectDisambiguation
}

pub type OffsetOption {
  PreferOffset
  UseOffset
  IgnoreOffset
  RejectOffset
}

pub fn get_overflow_option(
  st: Agent,
  opts: Option(Handle),
) -> #(Overflow, Agent) {
  get_enum_option(
    st,
    opts,
    "overflow",
    [#("constrain", Constrain), #("reject", Reject)],
    Constrain,
  )
}

pub fn get_overflow_option_from_value(
  st: Agent,
  options: JsVal,
) -> #(Overflow, Agent) {
  let #(opts, st) = get_options_object(st, options)
  get_overflow_option(st, opts)
}

pub fn get_disambiguation_option(
  st: Agent,
  opts: Option(Handle),
) -> #(Disambiguation, Agent) {
  get_enum_option(
    st,
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

pub fn get_offset_option(
  st: Agent,
  opts: Option(Handle),
  default: OffsetOption,
) -> #(OffsetOption, Agent) {
  get_enum_option(
    st,
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

pub type CalendarNameMode {
  CalendarNameAuto
  CalendarNameAlways
  CalendarNameNever
  CalendarNameCritical
}

pub type ShowOffset {
  OffsetShowAuto
  OffsetShowNever
}

pub type TimeZoneNameMode {
  ZoneNameAuto
  ZoneNameNever
  ZoneNameCritical
}

pub fn get_calendar_name_option(
  st: Agent,
  opts: Option(Handle),
) -> #(CalendarNameMode, Agent) {
  get_enum_option(
    st,
    opts,
    "calendarName",
    [
      #("auto", CalendarNameAuto),
      #("always", CalendarNameAlways),
      #("never", CalendarNameNever),
      #("critical", CalendarNameCritical),
    ],
    CalendarNameAuto,
  )
}

pub fn get_show_offset_option(
  st: Agent,
  opts: Option(Handle),
) -> #(ShowOffset, Agent) {
  get_enum_option(
    st,
    opts,
    "offset",
    [#("auto", OffsetShowAuto), #("never", OffsetShowNever)],
    OffsetShowAuto,
  )
}

pub fn get_time_zone_name_option(
  st: Agent,
  opts: Option(Handle),
) -> #(TimeZoneNameMode, Agent) {
  get_enum_option(
    st,
    opts,
    "timeZoneName",
    [
      #("auto", ZoneNameAuto),
      #("never", ZoneNameNever),
      #("critical", ZoneNameCritical),
    ],
    ZoneNameAuto,
  )
}

pub fn calendar_suffix(
  mode: CalendarNameMode,
  cal: temporal_calendar.Calendar,
) -> String {
  let id = temporal_calendar.identifier(cal)
  case mode {
    CalendarNameNever -> ""
    CalendarNameAuto ->
      case cal {
        temporal_calendar.Iso8601 -> ""
        _ -> "[u-ca=" <> id <> "]"
      }
    CalendarNameAlways -> "[u-ca=" <> id <> "]"
    CalendarNameCritical -> "[!u-ca=" <> id <> "]"
  }
}

// year-month and month-day print the reference date unless iso omits it
pub fn format_with_reference(
  iso: IsoDate,
  cal: temporal_calendar.Calendar,
  mode: CalendarNameMode,
  short short: String,
) -> String {
  case cal, mode {
    temporal_calendar.Iso8601, CalendarNameAuto
    | temporal_calendar.Iso8601, CalendarNameNever
    -> short
    _, _ -> format_iso_date(iso) <> calendar_suffix(mode, cal)
  }
}

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

pub type UnsignedRoundingMode {
  UnsignedZero
  UnsignedInfinity
  UnsignedHalfZero
  UnsignedHalfInfinity
  UnsignedHalfEven
}

pub type UnitOption {
  UnitAbsent
  UnitAuto
  UnitValue(Unit)
}

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

pub fn singular_unit(u: String) -> Option(Unit) {
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

pub fn unit_rank(u: Unit) -> Int {
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

pub type TimeUnit {
  DayUnit
  HourUnit
  MinuteUnit
  SecondUnit
  MillisecondUnit
  MicrosecondUnit
  NanosecondUnit
}

pub fn as_time_unit(u: Unit) -> Option(TimeUnit) {
  case u {
    Year | Month | Week -> None
    Day -> Some(DayUnit)
    Hour -> Some(HourUnit)
    Minute -> Some(MinuteUnit)
    Second -> Some(SecondUnit)
    Millisecond -> Some(MillisecondUnit)
    Microsecond -> Some(MicrosecondUnit)
    Nanosecond -> Some(NanosecondUnit)
  }
}

pub fn require_time_unit(u: Unit) -> Result(TimeUnit, JsError) {
  case as_time_unit(u) {
    Some(t) -> Ok(t)
    None ->
      Error(JsError(
        RangeError,
        unit_to_string(u) <> " has no fixed length; expected a time unit",
      ))
  }
}

pub fn time_unit_ns(u: TimeUnit) -> Int {
  case u {
    DayUnit -> ns_per_day
    HourUnit -> ns_per_hour
    MinuteUnit -> ns_per_minute
    SecondUnit -> ns_per_second
    MillisecondUnit -> ns_per_ms
    MicrosecondUnit -> ns_per_us
    NanosecondUnit -> 1
  }
}

// spec maximumtemporaldurationroundingincrement
pub fn max_rounding_increment(u: TimeUnit) -> Option(Int) {
  case u {
    DayUnit -> None
    HourUnit -> Some(24)
    MinuteUnit | SecondUnit -> Some(60)
    MillisecondUnit | MicrosecondUnit | NanosecondUnit -> Some(1000)
  }
}

pub fn get_unit_option(
  st: Agent,
  opts: Option(Handle),
  key: String,
  allow_auto allow_auto: Bool,
) -> #(Option(Unit), Agent) {
  let #(u, st) = read_unit_option(st, opts, key, allow_auto:)
  case u {
    UnitValue(v) -> #(Some(v), st)
    UnitAuto | UnitAbsent -> #(None, st)
  }
}

pub fn read_unit_option(
  st: Agent,
  opts: Option(Handle),
  key: String,
  allow_auto allow_auto: Bool,
) -> #(UnitOption, Agent) {
  let #(v, st) = opt_get(st, opts, key)
  case classify(v) {
    KUndef -> #(UnitAbsent, st)
    _ -> {
      let #(s, st) = rt_val.t_to_string(st, v)
      case allow_auto && s == "auto", singular_unit(s) {
        True, _ -> #(UnitAuto, st)
        False, Some(u) -> #(UnitValue(u), st)
        False, None ->
          rt_val.t_throw_range_error(
            st,
            s <> " is not a valid value for " <> key,
          )
      }
    }
  }
}

pub fn get_rounding_mode_option(
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

pub fn get_rounding_increment_option(
  st: Agent,
  opts: Option(Handle),
) -> #(Int, Agent) {
  let #(v, st) = opt_get(st, opts, "roundingIncrement")
  case classify(v) {
    KUndef -> #(1, st)
    _ -> {
      let #(n, st) = rt_val.t_to_number(st, v)
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

pub fn as_if_positive_mode(mode: RoundingMode) -> RoundingMode {
  case mode {
    Trunc -> Floor
    Expand -> Ceil
    HalfTrunc -> HalfFloor
    HalfExpand -> HalfCeil
    Ceil | Floor | HalfCeil | HalfFloor | HalfEven -> mode
  }
}

pub fn round_to_increment(x: Int, inc: Int, mode: RoundingMode) -> Int {
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
            True -> floor_mod(q, 2) != 0
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

pub fn unsigned_rounding_mode(
  mode: RoundingMode,
  negative negative: Bool,
) -> UnsignedRoundingMode {
  case mode, negative {
    Ceil, False -> UnsignedInfinity
    Ceil, True -> UnsignedZero
    Floor, False -> UnsignedZero
    Floor, True -> UnsignedInfinity
    Expand, _ -> UnsignedInfinity
    Trunc, _ -> UnsignedZero
    HalfCeil, False -> UnsignedHalfInfinity
    HalfCeil, True -> UnsignedHalfZero
    HalfFloor, False -> UnsignedHalfZero
    HalfFloor, True -> UnsignedHalfInfinity
    HalfExpand, _ -> UnsignedHalfInfinity
    HalfTrunc, _ -> UnsignedHalfZero
    HalfEven, _ -> UnsignedHalfEven
  }
}

// num/den in [0,1] is x between r1 and r2; true picks r2
pub fn apply_unsigned_rounding(
  num: Int,
  den: Int,
  r1_even r1_even: Bool,
  mode mode: UnsignedRoundingMode,
) -> Bool {
  case num == 0 {
    True -> False
    False ->
      case mode {
        UnsignedZero -> False
        UnsignedInfinity -> True
        UnsignedHalfZero | UnsignedHalfInfinity | UnsignedHalfEven -> {
          let twice = 2 * num
          case int.compare(twice, den) {
            order.Lt -> False
            order.Gt -> True
            order.Eq ->
              case mode {
                UnsignedHalfZero -> False
                UnsignedHalfInfinity -> True
                UnsignedHalfEven | UnsignedZero | UnsignedInfinity -> !r1_even
              }
          }
        }
      }
  }
}

// options read alphabetically, order is observable
pub fn get_difference_settings(
  st: Agent,
  args: List(JsVal),
) -> #(#(Option(Unit), Option(Unit), Int, RoundingMode), Agent) {
  let #(opts, st) = get_options_object(st, helpers.arg_at(args, 1))
  let #(largest, st) =
    get_unit_option(st, opts, "largestUnit", allow_auto: True)
  let #(inc, st) = get_rounding_increment_option(st, opts)
  let #(mode, st) = get_rounding_mode_option(st, opts, Trunc)
  let #(smallest, st) =
    get_unit_option(st, opts, "smallestUnit", allow_auto: False)
  #(#(largest, smallest, inc, mode), st)
}

pub const largest_smaller_msg = "largestUnit must not be smaller than smallestUnit"

pub fn largest_smaller_than_smallest(largest: Unit, smallest: Unit) -> Bool {
  unit_rank(largest) < unit_rank(smallest)
}

pub fn require_largest_ge_smallest(
  st: Agent,
  largest: Unit,
  smallest: Unit,
) -> Nil {
  case largest_smaller_than_smallest(largest, smallest) {
    True -> rt_val.t_throw_range_error(st, largest_smaller_msg)
    False -> Nil
  }
}

pub fn apply_since_mode(
  mode: RoundingMode,
  is_since is_since: Bool,
) -> RoundingMode {
  case is_since {
    True -> negate_rounding_mode(mode)
    False -> mode
  }
}

pub fn apply_since_duration(
  dur: Duration,
  is_since is_since: Bool,
) -> Duration {
  case is_since {
    True -> negate_duration(dur)
    False -> dur
  }
}

pub fn apply_since_ns(ns: Int, is_since is_since: Bool) -> Int {
  case is_since {
    True -> 0 - ns
    False -> ns
  }
}

pub fn max_unit(a: Unit, b: Unit) -> Unit {
  case unit_rank(a) >= unit_rank(b) {
    True -> a
    False -> b
  }
}

pub fn negate_rounding_mode(mode: RoundingMode) -> RoundingMode {
  case mode {
    Ceil -> Floor
    Floor -> Ceil
    HalfCeil -> HalfFloor
    HalfFloor -> HalfCeil
    Expand | Trunc | HalfExpand | HalfTrunc | HalfEven -> mode
  }
}

pub fn round_options(
  st: Agent,
  arg: JsVal,
  allow_day allow_day: Bool,
) -> #(#(TimeUnit, Int, RoundingMode), Agent) {
  case classify(arg) {
    KUndef -> rt_val.t_throw_type_error(st, "options parameter is required")
    KStr(s) ->
      case singular_unit(s) |> option.then(round_unit(_, allow_day)) {
        Some(u) -> #(#(u, 1, HalfExpand), st)
        None -> rt_val.t_throw_range_error(st, "invalid smallestUnit")
      }
    KHandle(h) -> {
      let opts = Some(h)
      let #(inc, st) = get_rounding_increment_option(st, opts)
      let #(mode, st) = get_rounding_mode_option(st, opts, HalfExpand)
      let #(smallest, st) =
        get_unit_option(st, opts, "smallestUnit", allow_auto: False)
      case smallest {
        None -> rt_val.t_throw_range_error(st, "smallestUnit is required")
        Some(u) ->
          case round_unit(u, allow_day) {
            Some(unit) -> #(#(unit, inc, mode), st)
            None -> rt_val.t_throw_range_error(st, "invalid smallestUnit")
          }
      }
    }
    _ -> rt_val.t_throw_type_error(st, "invalid options")
  }
}

pub fn round_unit(u: Unit, allow_day allow_day: Bool) -> Option(TimeUnit) {
  case as_time_unit(u) {
    Some(DayUnit) if !allow_day -> None
    other -> other
  }
}

// spec validatetemporalroundingincrement
pub fn valid_rounding_increment(
  inc: Int,
  dividend: Int,
  inclusive inclusive: Bool,
) -> Bool {
  case inclusive {
    True -> inc >= 1 && inc <= dividend && dividend % inc == 0
    False -> inc == 1 || { inc > 1 && inc < dividend && dividend % inc == 0 }
  }
}

pub fn valid_increment_for_unit(inc: Int, smallest: Unit) -> Bool {
  case option.then(as_time_unit(smallest), max_rounding_increment) {
    Some(max) -> valid_rounding_increment(inc, max, inclusive: False)
    None -> True
  }
}

pub fn check_diff_setup(
  st: Agent,
  largest: Unit,
  smallest: Unit,
  inc: Int,
) -> Nil {
  case
    largest_smaller_than_smallest(largest, smallest),
    valid_increment_for_unit(inc, smallest)
  {
    True, _ -> rt_val.t_throw_range_error(st, largest_smaller_msg)
    False, False -> rt_val.t_throw_range_error(st, "invalid roundingIncrement")
    False, True -> Nil
  }
}

pub fn balance_time_ns(total: Int, largest: Unit) -> Duration {
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
  let #(milliseconds, a) = case lr >= unit_rank(Millisecond) {
    True -> #(a / ns_per_ms, a % ns_per_ms)
    False -> #(0, a)
  }
  let #(microseconds, a) = case lr >= unit_rank(Microsecond) {
    True -> #(a / ns_per_us, a % ns_per_us)
    False -> #(0, a)
  }
  apply_duration_sign(
    Duration(
      years: 0,
      months: 0,
      weeks: 0,
      days:,
      hours:,
      minutes:,
      seconds:,
      milliseconds:,
      microseconds:,
      nanoseconds: a,
    ),
    sign,
  )
}

pub fn to_string_time_options(
  st: Agent,
  opts: Option(Handle),
) -> #(#(SecondsPrecision, Option(TimeUnit), Int, RoundingMode), Agent) {
  let #(digits, st) = get_fractional_digits(st, opts)
  let #(mode, st) = get_rounding_mode_option(st, opts, Trunc)
  let #(smallest, st) =
    get_unit_option(st, opts, "smallestUnit", allow_auto: False)
  let #(precision, unit, inc) =
    rt_val.or_throw(st, seconds_string_precision(digits, smallest))
  #(#(precision, unit, inc, mode), st)
}

pub type FractionalDigits {
  DigitsAuto
  DigitsFixed(Int)
}

pub fn seconds_string_precision(
  digits: FractionalDigits,
  smallest: Option(Unit),
) -> Result(#(SecondsPrecision, Option(TimeUnit), Int), JsError) {
  case smallest {
    Some(Year) | Some(Month) | Some(Week) | Some(Day) | Some(Hour) ->
      Error(JsError(RangeError, "smallestUnit must be a time unit"))
    Some(Minute) -> Ok(#(MinutePrecision, Some(MinuteUnit), 1))
    Some(Second) -> Ok(#(SubsecondDigits(0), Some(SecondUnit), 1))
    Some(Millisecond) -> Ok(#(SubsecondDigits(3), Some(MillisecondUnit), 1))
    Some(Microsecond) -> Ok(#(SubsecondDigits(6), Some(MicrosecondUnit), 1))
    Some(Nanosecond) -> Ok(#(SubsecondDigits(9), Some(NanosecondUnit), 1))
    None ->
      case digits {
        DigitsAuto -> Ok(#(AutoPrecision, None, 1))
        DigitsFixed(0) -> Ok(#(SubsecondDigits(0), Some(SecondUnit), 1))
        DigitsFixed(n) ->
          Ok(#(SubsecondDigits(n), Some(NanosecondUnit), pow10(9 - n)))
      }
  }
}

pub fn get_fractional_digits(
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
      let i = rt_val.float_to_int(float.floor(f))
      case i >= 0 && i <= 9 {
        True -> #(DigitsFixed(i), st)
        False ->
          rt_val.t_throw_range_error(st, "invalid fractionalSecondDigits")
      }
    }
    KNum(_) -> rt_val.t_throw_range_error(st, "invalid fractionalSecondDigits")
    _ -> {
      let #(s, st) = rt_val.t_to_string(st, v)
      case s {
        "auto" -> #(DigitsAuto, st)
        _ -> rt_val.t_throw_range_error(st, "invalid fractionalSecondDigits")
      }
    }
  }
}

fn duration_field_list(d: Duration) -> List(Int) {
  [
    d.years,
    d.months,
    d.weeks,
    d.days,
    d.hours,
    d.minutes,
    d.seconds,
    d.milliseconds,
    d.microseconds,
    d.nanoseconds,
  ]
}

fn map_duration_fields(d: Duration, f: fn(Int) -> Int) -> Duration {
  Duration(
    years: f(d.years),
    months: f(d.months),
    weeks: f(d.weeks),
    days: f(d.days),
    hours: f(d.hours),
    minutes: f(d.minutes),
    seconds: f(d.seconds),
    milliseconds: f(d.milliseconds),
    microseconds: f(d.microseconds),
    nanoseconds: f(d.nanoseconds),
  )
}

pub fn has_calendar_units(d: Duration) -> Bool {
  d.years != 0 || d.months != 0 || d.weeks != 0
}

pub fn has_date_units(d: Duration) -> Bool {
  has_calendar_units(d) || d.days != 0
}

pub fn date_part(d: Duration) -> Duration {
  Duration(
    ..zero_duration,
    years: d.years,
    months: d.months,
    weeks: d.weeks,
    days: d.days,
  )
}

pub fn duration_sign(d: Duration) -> Int {
  list.fold(duration_field_list(d), 0, fn(acc, f) {
    case acc != 0 {
      True -> acc
      False -> int_sign(f)
    }
  })
}

pub fn is_valid_duration(d: Duration) -> Bool {
  let sign = duration_sign(d)
  // validity is checked on float-rounded components per spec
  let d = map_duration_fields(d, round_to_float_precision)
  let consistent =
    list.all(duration_field_list(d), fn(f) {
      { f >= 0 || sign <= 0 } && { f <= 0 || sign >= 0 }
    })
  let cal_ok =
    int.absolute_value(d.years) < pow2_32
    && int.absolute_value(d.months) < pow2_32
    && int.absolute_value(d.weeks) < pow2_32
  let total = days_and_time_ns(d)
  consistent && cal_ok && int.absolute_value(total) <= max_time_duration_ns
}

pub fn days_and_time_ns(d: Duration) -> Int {
  d.days * ns_per_day + time_part_ns(d)
}

pub fn time_part_ns(d: Duration) -> Int {
  d.hours
  * ns_per_hour
  + d.minutes
  * ns_per_minute
  + d.seconds
  * ns_per_second
  + d.milliseconds
  * ns_per_ms
  + d.microseconds
  * ns_per_us
  + d.nanoseconds
}

pub fn check_time_duration_range(ns: Int) -> Result(Nil, JsError) {
  case int.absolute_value(ns) > max_time_duration_ns {
    True -> Error(JsError(RangeError, "duration time units out of range"))
    False -> Ok(Nil)
  }
}

pub fn apply_duration_sign(d: Duration, sign: Int) -> Duration {
  case sign < 0 {
    False -> d
    True -> map_duration_fields(d, int.negate)
  }
}

pub fn negate_duration(d: Duration) -> Duration {
  apply_duration_sign(d, -1)
}

pub fn read_bag_int_field(
  st: Agent,
  bag: Handle,
  key: String,
  conv: fn(Agent, JsVal) -> #(Int, Agent),
) -> #(Option(Int), Agent) {
  let #(v, st) = rt_obj.t_get_prop(st, mk_object(bag), StringKey(Named(key)))
  case classify(v) {
    KUndef -> #(None, st)
    _ -> {
      let #(n, st) = conv(st, v)
      #(Some(n), st)
    }
  }
}

pub fn read_int_field(
  st: Agent,
  bag: Handle,
  key: String,
) -> #(Option(Int), Agent) {
  read_bag_int_field(st, bag, key, to_integer_with_truncation)
}

pub fn read_pos_int_field(
  st: Agent,
  bag: Handle,
  key: String,
) -> #(Option(Int), Agent) {
  read_bag_int_field(st, bag, key, to_positive_integer_with_truncation)
}

pub fn read_integral_int_field(
  st: Agent,
  bag: Handle,
  key: String,
) -> #(Option(Int), Agent) {
  read_bag_int_field(st, bag, key, to_integer_if_integral)
}

pub fn to_temporal_duration(st: Agent, item: JsVal) -> #(Duration, Agent) {
  case classify(item) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind:, ..) ->
          case duration_slot_of(kind) {
            Some(d) -> #(d, st)
            None -> duration_from_bag(st, h)
          }
        _ -> duration_from_bag(st, h)
      }
    KStr(s) ->
      case parse_duration_string(s) {
        Some(d) ->
          case is_valid_duration(d) {
            True -> #(d, st)
            False -> rt_val.t_throw_range_error(st, "invalid duration")
          }
        None -> rt_val.t_throw_range_error(st, "invalid duration string: " <> s)
      }
    _ -> rt_val.t_throw_type_error(st, "cannot convert to a Temporal.Duration")
  }
}

pub fn read_duration_fields(
  st: Agent,
  bag: Handle,
) -> #(List(Option(Int)), Agent) {
  let #(days, st) = read_integral_int_field(st, bag, "days")
  let #(hours, st) = read_integral_int_field(st, bag, "hours")
  let #(microseconds, st) = read_integral_int_field(st, bag, "microseconds")
  let #(milliseconds, st) = read_integral_int_field(st, bag, "milliseconds")
  let #(minutes, st) = read_integral_int_field(st, bag, "minutes")
  let #(months, st) = read_integral_int_field(st, bag, "months")
  let #(nanoseconds, st) = read_integral_int_field(st, bag, "nanoseconds")
  let #(seconds, st) = read_integral_int_field(st, bag, "seconds")
  let #(weeks, st) = read_integral_int_field(st, bag, "weeks")
  let #(years, st) = read_integral_int_field(st, bag, "years")
  #(
    [
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
    ],
    st,
  )
}

pub fn apply_duration_fields(
  base: Duration,
  fields: List(Option(Int)),
) -> Duration {
  case fields {
    [
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
    ] ->
      Duration(
        years: option.unwrap(years, base.years),
        months: option.unwrap(months, base.months),
        weeks: option.unwrap(weeks, base.weeks),
        days: option.unwrap(days, base.days),
        hours: option.unwrap(hours, base.hours),
        minutes: option.unwrap(minutes, base.minutes),
        seconds: option.unwrap(seconds, base.seconds),
        milliseconds: option.unwrap(milliseconds, base.milliseconds),
        microseconds: option.unwrap(microseconds, base.microseconds),
        nanoseconds: option.unwrap(nanoseconds, base.nanoseconds),
      )
    _ -> base
  }
}

pub fn duration_from_bag(st: Agent, bag: Handle) -> #(Duration, Agent) {
  let #(fields, st) = read_duration_fields(st, bag)
  case list.all(fields, option.is_none) {
    True ->
      rt_val.t_throw_type_error(
        st,
        "invalid property bag for Temporal.Duration",
      )
    False -> {
      let d = apply_duration_fields(zero_duration, fields)
      case is_valid_duration(d) {
        True -> #(d, st)
        False -> rt_val.t_throw_range_error(st, "invalid duration")
      }
    }
  }
}

pub fn parse_duration_string(s: String) -> Option(Duration) {
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

fn parse_duration_date_units(s: String, sign: Int) -> Option(Duration) {
  let #(years, s) = parse_duration_unit(s, ["Y", "y"])
  let #(months, s) = parse_duration_unit(s, ["M", "m"])
  let #(weeks, s) = parse_duration_unit(s, ["W", "w"])
  let #(days, s) = parse_duration_unit(s, ["D", "d"])
  case s {
    "" ->
      case years == None && months == None && weeks == None && days == None {
        True -> None
        False ->
          Some(apply_duration_sign(
            Duration(
              years: option.unwrap(years, 0),
              months: option.unwrap(months, 0),
              weeks: option.unwrap(weeks, 0),
              days: option.unwrap(days, 0),
              hours: 0,
              minutes: 0,
              seconds: 0,
              milliseconds: 0,
              microseconds: 0,
              nanoseconds: 0,
            ),
            sign,
          ))
      }
    "T" <> r | "t" <> r -> {
      use #(h, mi, sec, sub_ns) <- option.then(parse_duration_time_units(r))
      Some(apply_duration_sign(
        Duration(
          years: option.unwrap(years, 0),
          months: option.unwrap(months, 0),
          weeks: option.unwrap(weeks, 0),
          days: option.unwrap(days, 0),
          hours: h,
          minutes: mi,
          seconds: sec,
          milliseconds: sub_ns / ns_per_ms,
          microseconds: { sub_ns % ns_per_ms } / ns_per_us,
          nanoseconds: sub_ns % ns_per_us,
        ),
        sign,
      ))
    }
    _ -> None
  }
}

fn parse_duration_unit(
  s: String,
  designators: List(String),
) -> #(Option(Int), String) {
  case take_some_digits(s, 16) {
    Some(#(v, _, rest)) ->
      case list.find(designators, fn(d) { string.starts_with(rest, d) }) {
        Ok(d) -> #(Some(v), string.drop_start(rest, string.length(d)))
        Error(Nil) -> #(None, s)
      }
    None -> #(None, s)
  }
}

fn parse_duration_time_units(s: String) -> Option(#(Int, Int, Int, Int)) {
  use #(h, h_frac, s1) <- option.then(parse_duration_time_unit(s, ["H", "h"]))
  case h_frac {
    Some(f) ->
      case s1 {
        "" -> {
          let total_ns = f * 3600
          let mi = total_ns / ns_per_minute
          let rem = total_ns - mi * ns_per_minute
          let sec = rem / ns_per_second
          Some(#(option.unwrap(h, 0), mi, sec, rem - sec * ns_per_second))
        }
        _ -> None
      }
    None -> {
      use #(mi, mi_frac, s2) <- option.then(
        parse_duration_time_unit(s1, ["M", "m"]),
      )
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
            parse_duration_time_unit(s2, ["S", "s"]),
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

fn parse_duration_time_unit(
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
          // designator mismatch: backtrack so caller tries next unit
          Some(#(None, None, s))
      }
    }
  }
}

pub fn time_zone_from_string(st: Agent, id: String) -> #(TemporalZone, Agent) {
  case parse_time_zone_identifier(st, id) {
    #(Ok(tz), st) -> #(tz, st)
    #(Error(UnknownIdentifier), st) -> {
      let #(tz, st) = tz_from_datetime_string(st, id)
      #(rt_val.or_throw(st, tz), st)
    }
    #(Error(InvalidIdentifier(e)), st) -> rt_val.t_throw(st, e)
  }
}

pub type TimeZoneIdError {
  UnknownIdentifier
  InvalidIdentifier(JsError)
}

pub fn parse_time_zone_identifier(
  st: Agent,
  id: String,
) -> #(Result(TemporalZone, TimeZoneIdError), Agent) {
  case string.uppercase(id) == "UTC" {
    True -> #(Ok(UtcZone), st)
    False ->
      case parse_offset_tz_id(id) {
        Some(ns) -> #(Ok(OffsetZone(ns:)), st)
        None ->
          case lookup_zone(st, id) {
            #(Ok(zone), st) -> #(Ok(IanaZone(zone:)), st)
            #(Error(temporal_tz.LoadFailed(id:, error:)), st) -> #(
              Error(InvalidIdentifier(unloadable_tz(id, error))),
              st,
            )
            #(Error(temporal_tz.UnknownZone), st) ->
              case is_tz_annotation(id) {
                True -> #(Error(InvalidIdentifier(unsupported_tz(id))), st)
                False -> #(Error(UnknownIdentifier), st)
              }
          }
      }
  }
}

// the agent keeps each zone the host loaded, see HostHooks.load_time_zone
pub fn lookup_zone(
  st: Agent,
  name: String,
) -> #(Result(temporal_tz.Zone, temporal_tz.ZoneLookupError), Agent) {
  case temporal_tz.lookup(name, st.tz_zones, st.hooks.load_time_zone) {
    Ok(#(zone, tz_zones)) -> #(Ok(zone), Agent(..st, tz_zones:))
    Error(err) -> #(Error(err), st)
  }
}

fn tz_from_datetime_string(
  st: Agent,
  s: String,
) -> #(Result(TemporalZone, JsError), Agent) {
  case parse_iso_datetime_string(s) {
    None -> #(Error(JsError(RangeError, "invalid time zone: " <> s)), st)
    Some(p) ->
      case p.tz {
        Some(tz_text) ->
          case parse_time_zone_identifier(st, tz_text) {
            #(Ok(tz), st) -> #(Ok(tz), st)
            #(Error(InvalidIdentifier(e)), st) -> #(Error(e), st)
            #(Error(UnknownIdentifier), st) -> #(
              Error(unsupported_tz(tz_text)),
              st,
            )
          }
        None ->
          case p.offset {
            Zulu -> #(Ok(UtcZone), st)
            NumericOffset(off, sub_minute) ->
              // seconds component not allowed, even ":00"
              case !sub_minute && off % ns_per_minute == 0 {
                True -> #(Ok(OffsetZone(ns: off)), st)
                False -> #(
                  Error(JsError(
                    RangeError,
                    "sub-minute offset not valid as a time zone",
                  )),
                  st,
                )
              }
            NoOffset -> #(
              Error(JsError(RangeError, "no time zone found in string: " <> s)),
              st,
            )
          }
      }
  }
}

pub fn parse_offset_tz_id(id: String) -> Option(Int) {
  let signed = case id {
    "+" <> _ | "-" <> _ -> True
    _ -> False
  }
  case signed {
    False -> None
    True ->
      case parse_offset_part(id) {
        // seconds component not allowed, even ":00"
        Some(#(NumericOffset(ns, False), "")) ->
          case ns % ns_per_minute == 0 && int.absolute_value(ns) < ns_per_day {
            True -> Some(ns)
            False -> None
          }
        _ -> None
      }
  }
}

pub fn time_zone_id(tz: TemporalZone) -> String {
  case tz {
    UtcZone -> "UTC"
    OffsetZone(ns:) -> format_offset_minutes(ns)
    IanaZone(zone:) -> temporal_tz.zone_id(zone)
  }
}

pub fn unsupported_tz(tz: String) -> JsError {
  JsError(RangeError, "time zone " <> tz <> " is not supported")
}

pub fn unloadable_tz(id: String, error: time_zone.TzError) -> JsError {
  JsError(
    RangeError,
    "time zone " <> id <> " cannot be loaded: " <> time_zone.describe(error),
  )
}

pub fn tz_offset_ns_at(tz: TemporalZone, epoch_ns: Int) -> Int {
  case tz {
    UtcZone -> 0
    OffsetZone(ns:) -> ns
    IanaZone(zone:) -> temporal_tz.offset_ns_at(zone, epoch_ns)
  }
}

pub fn epoch_ns_to_iso_in(
  tz: TemporalZone,
  epoch_ns: Int,
) -> #(IsoDate, IsoTime) {
  epoch_ns_to_iso(epoch_ns, tz_offset_ns_at(tz, epoch_ns))
}

pub fn is_valid_epoch_ns(ns: Int) -> Bool {
  int.absolute_value(ns) <= ns_max_instant
}

pub fn validate_epoch_ns(ns: Int) -> Result(Int, JsError) {
  case is_valid_epoch_ns(ns) {
    True -> Ok(ns)
    False -> Error(JsError(RangeError, "instant outside valid range"))
  }
}

pub fn format_offset_full(offset_ns: Int) -> String {
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

pub fn format_offset_rounded(offset_ns: Int) -> String {
  format_offset_minutes(round_to_increment(offset_ns, ns_per_minute, HalfExpand))
}

pub fn time_zone_equals(a: TemporalZone, b: TemporalZone) -> Bool {
  a == b
  || case a, b {
    OffsetZone(_), _ | _, OffsetZone(_) -> False
    UtcZone, UtcZone -> True
    UtcZone, IanaZone(z) | IanaZone(z), UtcZone ->
      temporal_tz.primary_identifier_of(z) == "UTC"
    IanaZone(za), IanaZone(zb) ->
      temporal_tz.primary_identifier_of(za)
      == temporal_tz.primary_identifier_of(zb)
  }
}

pub fn to_temporal_time_zone(st: Agent, v: JsVal) -> #(TemporalZone, Agent) {
  case classify(v) {
    KStr(s) -> time_zone_from_string(st, s)
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(
          kind: TemporalObj(data: TemporalZonedDateTime(time_zone:, ..)),
          ..,
        ) -> #(time_zone, st)
        _ -> rt_val.t_throw_type_error(st, "timeZone must be a string")
      }
    _ -> rt_val.t_throw_type_error(st, "timeZone must be a string")
  }
}

pub fn system_time_zone(st: Agent) -> #(TemporalZone, Agent) {
  case host_time.time_zone_id(st.hooks.time_zone) {
    Some(id) ->
      case parse_time_zone_identifier(st, id) {
        #(Ok(tz), st) -> #(tz, st)
        #(Error(UnknownIdentifier), st) | #(Error(InvalidIdentifier(_)), st) -> #(
          UtcZone,
          st,
        )
      }
    None -> #(UtcZone, st)
  }
}

pub fn to_temporal_instant(st: Agent, item: JsVal) -> #(Int, Agent) {
  case classify(item) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: TemporalObj(data: TemporalInstant(epoch_ns:)), ..) -> #(
          epoch_ns,
          st,
        )
        SObject(
          kind: TemporalObj(data: TemporalZonedDateTime(epoch_ns:, ..)),
          ..,
        ) -> #(epoch_ns, st)
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
  // calendar annotation value is ignored for instant
  case parse_iso_datetime_string(s) {
    None -> rt_val.t_throw_range_error(st, "invalid instant string: " <> s)
    Some(p) ->
      case p.time, valid_tz_annotation(p.tz) {
        _, False ->
          rt_val.t_throw_range_error(
            st,
            "invalid time zone annotation in instant string: " <> s,
          )
        Some(t), True ->
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
              #(rt_val.or_throw(st, validate_epoch_ns(ns)), st)
            }
          }
        None, True ->
          rt_val.t_throw_range_error(st, "instant string requires a time")
      }
  }
}

fn valid_tz_annotation(tz: Option(String)) -> Bool {
  case tz {
    Some("+" <> _ as ann) | Some("-" <> _ as ann) ->
      option.is_some(parse_offset_tz_id(ann))
    Some(_) | None -> True
  }
}
