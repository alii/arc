import arc/bytecode/error_kind.{type JsError, JsError, RangeError}
import arc/internal/int_math.{floor_div, floor_mod}
import arc/rt/builtins/helpers
import arc/rt/builtins/options.{get_options_object, opt_get}
import arc/rt/builtins/temporal_iso.{
  type Duration, type SecondsPrecision, AutoPrecision, Duration, MinutePrecision,
  SubsecondDigits, int_sign, ns_per_day, ns_per_hour, ns_per_minute, ns_per_ms,
  ns_per_second, ns_per_us, pow10,
}
import arc/rt/types.{
  type Agent, type Handle, type JsVal, JFloat, JInt, JNan, JNegInf, JPosInf,
  KHandle, KNum, KStr, KUndef, classify,
}
import arc/rt/val as rt_val
import gleam/float
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/order

import arc/rt/builtins/temporal_common.{apply_duration_sign, negate_duration}
import arc/rt/builtins/temporal_options.{get_enum_option}

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
      let #(s, st) = rt_val.to_string(st, v)
      case allow_auto && s == "auto", singular_unit(s) {
        True, _ -> #(UnitAuto, st)
        False, Some(u) -> #(UnitValue(u), st)
        False, None ->
          rt_val.throw_range_error(st, s <> " is not a valid value for " <> key)
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
      let #(n, st) = rt_val.to_number(st, v)
      let i = case n {
        JInt(i) -> Some(i)
        JFloat(f) -> Some(rt_val.float_to_int(f))
        JNan | JPosInf | JNegInf -> None
      }
      case i {
        Some(i) if i >= 1 && i <= 1_000_000_000 -> #(i, st)
        _ -> rt_val.throw_range_error(st, "invalid roundingIncrement")
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
    True -> rt_val.throw_range_error(st, largest_smaller_msg)
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
    KUndef -> rt_val.throw_type_error(st, "options parameter is required")
    KStr(s) ->
      case singular_unit(s) |> option.then(round_unit(_, allow_day)) {
        Some(u) -> #(#(u, 1, HalfExpand), st)
        None -> rt_val.throw_range_error(st, "invalid smallestUnit")
      }
    KHandle(h) -> {
      let opts = Some(h)
      let #(inc, st) = get_rounding_increment_option(st, opts)
      let #(mode, st) = get_rounding_mode_option(st, opts, HalfExpand)
      let #(smallest, st) =
        get_unit_option(st, opts, "smallestUnit", allow_auto: False)
      case smallest {
        None -> rt_val.throw_range_error(st, "smallestUnit is required")
        Some(u) ->
          case round_unit(u, allow_day) {
            Some(unit) -> #(#(unit, inc, mode), st)
            None -> rt_val.throw_range_error(st, "invalid smallestUnit")
          }
      }
    }
    _ -> rt_val.throw_type_error(st, "invalid options")
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
    True, _ -> rt_val.throw_range_error(st, largest_smaller_msg)
    False, False -> rt_val.throw_range_error(st, "invalid roundingIncrement")
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
        False -> rt_val.throw_range_error(st, "invalid fractionalSecondDigits")
      }
    KNum(JFloat(f)) -> {
      let i = rt_val.float_to_int(float.floor(f))
      case i >= 0 && i <= 9 {
        True -> #(DigitsFixed(i), st)
        False -> rt_val.throw_range_error(st, "invalid fractionalSecondDigits")
      }
    }
    KNum(_) -> rt_val.throw_range_error(st, "invalid fractionalSecondDigits")
    _ -> {
      let #(s, st) = rt_val.to_string(st, v)
      case s {
        "auto" -> #(DigitsAuto, st)
        _ -> rt_val.throw_range_error(st, "invalid fractionalSecondDigits")
      }
    }
  }
}
