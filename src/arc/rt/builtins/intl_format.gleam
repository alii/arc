import arc/internal/gregorian.{civil_from_days}
import arc/internal/int_math.{floor_div}
import arc/rt/intl_data.{
  type CompactDisplay, type CurrencyDisplay, type IntlUseGrouping,
  type ListFormatStyle, type ListFormatType, type NameWidth, type Notation,
  type NumberStyle, type PluralType, type RelativeTimeNumeric,
  type RelativeTimeStyle, type RoundingMode, type RoundingPriority,
  type SignDisplay, type TrailingZeroDisplay, type UnitDisplay, AccountingSign,
  Cardinal, CompactLong, CompactShort, Conjunction, CurrencyCode, CurrencyName,
  CurrencyNarrowSymbol, CurrencySymbol, Disjunction, GroupingAlways,
  GroupingAuto, GroupingMin2, GroupingNever, ListLong, ListNarrow, ListShort,
  NotationCompact, NotationEngineering, NotationScientific, NotationStandard,
  NumericAlways, NumericAuto, Ordinal, PriorityAuto, PriorityLessPrecision,
  PriorityMorePrecision, RelativeLong, RelativeNarrow, RelativeShort, RoundCeil,
  RoundExpand, RoundFloor, RoundHalfCeil, RoundHalfEven, RoundHalfExpand,
  RoundHalfFloor, RoundHalfTrunc, RoundTrunc, SignAlways, SignAuto,
  SignExceptZero, SignNegative, SignNever, StandardSign, StyleCurrency,
  StyleDecimal, StylePercent, StyleUnit, TrailingZeroAuto,
  TrailingZeroStripIfInteger, UnitList, UnitLong, UnitNarrow, UnitShort,
  WidthLong, WidthNarrow, WidthShort,
}
import arc/rt/val as rt_val
import gleam/bool
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order}
import gleam/result
import gleam/string

pub type PartType {
  PartInteger
  PartGroup
  PartDecimal
  PartFraction
  PartCurrency
  PartPercentSign
  PartPlusSign
  PartMinusSign
  PartUnit
  PartCompact
  PartExponentSeparator
  PartExponentMinusSign
  PartExponentInteger
  PartNan
  PartInfinity
  PartApproximatelySign
  PartWeekday
  PartEra
  PartYear
  PartMonth
  PartDay
  PartHour
  PartMinute
  PartSecond
  PartFractionalSecond
  PartDayPeriod
  PartTimeZoneName
  PartElement
  PartLiteral
}

pub fn part_type_text(t: PartType) -> String {
  case t {
    PartInteger -> "integer"
    PartGroup -> "group"
    PartDecimal -> "decimal"
    PartFraction -> "fraction"
    PartCurrency -> "currency"
    PartPercentSign -> "percentSign"
    PartPlusSign -> "plusSign"
    PartMinusSign -> "minusSign"
    PartUnit -> "unit"
    PartCompact -> "compact"
    PartExponentSeparator -> "exponentSeparator"
    PartExponentMinusSign -> "exponentMinusSign"
    PartExponentInteger -> "exponentInteger"
    PartNan -> "nan"
    PartInfinity -> "infinity"
    PartApproximatelySign -> "approximatelySign"
    PartWeekday -> "weekday"
    PartEra -> "era"
    PartYear -> "year"
    PartMonth -> "month"
    PartDay -> "day"
    PartHour -> "hour"
    PartMinute -> "minute"
    PartSecond -> "second"
    PartFractionalSecond -> "fractionalSecond"
    PartDayPeriod -> "dayPeriod"
    PartTimeZoneName -> "timeZoneName"
    PartElement -> "element"
    PartLiteral -> "literal"
  }
}

type PartClass {
  NumberDigit
  NumberExponentDigit
  NumberCore
  NumberAffix
  DateNumeric
  DateText
  OtherPart
}

fn part_class(t: PartType) -> PartClass {
  case t {
    PartInteger | PartFraction -> NumberDigit
    PartExponentInteger -> NumberExponentDigit
    PartGroup
    | PartDecimal
    | PartCompact
    | PartNan
    | PartInfinity
    | PartExponentSeparator
    | PartExponentMinusSign -> NumberCore
    PartCurrency
    | PartPercentSign
    | PartPlusSign
    | PartMinusSign
    | PartUnit
    | PartApproximatelySign -> NumberAffix
    PartYear
    | PartMonth
    | PartDay
    | PartHour
    | PartMinute
    | PartSecond
    | PartFractionalSecond -> DateNumeric
    PartWeekday | PartEra | PartDayPeriod | PartTimeZoneName -> DateText
    PartLiteral | PartElement -> OtherPart
  }
}

pub fn is_number_digit(t: PartType) -> Bool {
  case part_class(t) {
    NumberDigit | NumberExponentDigit -> True
    NumberCore | NumberAffix | DateNumeric | DateText | OtherPart -> False
  }
}

pub fn is_date_numeric(t: PartType) -> Bool {
  case part_class(t) {
    DateNumeric -> True
    NumberDigit
    | NumberExponentDigit
    | NumberCore
    | NumberAffix
    | DateText
    | OtherPart -> False
  }
}

pub type Part {
  Part(type_: PartType, value: String)
}

pub type PartSource {
  SourceStart
  SourceEnd
  SourceShared
}

pub fn part_source_text(s: PartSource) -> String {
  case s {
    SourceStart -> "startRange"
    SourceEnd -> "endRange"
    SourceShared -> "shared"
  }
}

pub type RangePart {
  RangePart(type_: PartType, value: String, source: PartSource)
}

pub type UnitPart {
  UnitPart(type_: PartType, value: String, unit: Option(String))
}

pub fn parts_to_string(parts: List(Part)) -> String {
  parts |> list.map(fn(p) { p.value }) |> string.join("")
}

pub fn unit_parts_to_string(parts: List(UnitPart)) -> String {
  parts |> list.map(fn(p) { p.value }) |> string.join("")
}

// icu: shared affixes collapse unless exactly one code point
pub fn format_range_combine(
  key: LocaleKey,
  x_parts: List(Part),
  y_parts: List(Part),
) -> List(RangePart) {
  let as_start = fn(p: Part) { RangePart(p.type_, p.value, SourceStart) }
  let as_end = fn(p: Part) { RangePart(p.type_, p.value, SourceEnd) }
  let shared = fn(p: Part) { RangePart(p.type_, p.value, SourceShared) }
  let separator = fn(spaced) {
    RangePart(PartLiteral, range_sep(key, spaced), SourceShared)
  }
  case parts_to_string(x_parts) == parts_to_string(y_parts) {
    True -> [
      RangePart(PartApproximatelySign, "~", SourceShared),
      ..list.map(x_parts, shared)
    ]
    False -> {
      let #(x_pre, x_core, x_suf) = split_range_affixes(x_parts)
      let #(y_pre, y_core, y_suf) = split_range_affixes(y_parts)
      let affix_cp =
        list.fold(list.append(x_pre, x_suf), 0, fn(acc, p: Part) {
          acc + string.length(p.value)
        })
      case x_pre == y_pre && x_suf == y_suf && affix_cp != 1 {
        True ->
          list.flatten([
            list.map(x_pre, shared),
            list.map(x_core, as_start),
            [separator(False)],
            list.map(y_core, as_end),
            list.map(x_suf, shared),
          ])
        False -> {
          let spaced = x_suf != [] || y_pre != []
          list.flatten([
            list.map(x_parts, as_start),
            [separator(spaced)],
            list.map(y_parts, as_end),
          ])
        }
      }
    }
  }
}

fn split_range_affixes(
  parts: List(Part),
) -> #(List(Part), List(Part), List(Part)) {
  let is_core = fn(p: Part) {
    case part_class(p.type_) {
      NumberDigit | NumberExponentDigit | NumberCore -> True
      NumberAffix | DateNumeric | DateText | OtherPart -> False
    }
  }
  let #(pre, rest) = list.split_while(parts, fn(p) { !is_core(p) })
  let #(rev_suf, rev_core) =
    list.split_while(list.reverse(rest), fn(p) { !is_core(p) })
  #(pre, list.reverse(rev_core), list.reverse(rev_suf))
}

fn range_sep(key: LocaleKey, spaced spaced: Bool) -> String {
  case base_language(key) {
    "pt" -> " - "
    _ ->
      case spaced {
        True -> " – "
        False -> "–"
      }
  }
}

pub type Precision {
  Precision(min: Int, max: Int)
}

pub type NumberFormatOptions {
  NumberFormatOptions(
    locale: LocaleKey,
    style: NumberStyle,
    min_int: Int,
    frac: Option(Precision),
    sig: Option(Precision),
    use_grouping: IntlUseGrouping,
    notation: Notation,
    sign_display: SignDisplay,
    rounding_increment: Int,
    rounding_mode: RoundingMode,
    rounding_priority: RoundingPriority,
    trailing_zero_display: TrailingZeroDisplay,
  )
}

pub fn default_number_format_options() -> NumberFormatOptions {
  NumberFormatOptions(
    locale: locale_key("en"),
    style: StyleDecimal,
    min_int: 1,
    frac: Some(Precision(min: 0, max: 3)),
    sig: None,
    use_grouping: GroupingAuto,
    notation: NotationStandard,
    sign_display: SignAuto,
    rounding_increment: 1,
    rounding_mode: RoundHalfExpand,
    rounding_priority: PriorityAuto,
    trailing_zero_display: TrailingZeroAuto,
  )
}

pub opaque type LocaleKey {
  LocaleKey(base_tag: String)
}

pub fn locale_key(tag: String) -> LocaleKey {
  case string.split_once(tag, "-u-") {
    Ok(#(base, _)) -> LocaleKey(base)
    Error(Nil) ->
      case string.split_once(tag, "-x-") {
        Ok(#(base, _)) -> LocaleKey(base)
        Error(Nil) -> LocaleKey(tag)
      }
  }
}

fn base_language(key: LocaleKey) -> String {
  case string.split_once(key.base_tag, "-") {
    Ok(#(language, _)) -> language
    Error(Nil) -> key.base_tag
  }
}

fn decimal_sep(key: LocaleKey) -> String {
  case base_language(key) {
    "de" | "pt" | "it" | "nl" -> ","
    _ -> "."
  }
}

fn group_sep(key: LocaleKey) -> String {
  case key.base_tag {
    "pt-PT" -> "\u{00A0}"
    _ ->
      case base_language(key) {
        "de" | "pt" | "it" | "nl" -> "."
        _ -> ","
      }
  }
}

fn nan_text(key: LocaleKey) -> String {
  case key.base_tag {
    "zh-TW" | "zh-Hant" -> "非數值"
    _ -> "NaN"
  }
}

fn indian_grouping(key: LocaleKey) -> Bool {
  case key.base_tag {
    "en-IN" -> True
    _ -> base_language(key) == "hi"
  }
}

fn currency_suffixed(key: LocaleKey) -> Bool {
  case base_language(key) {
    "de" | "pt" -> True
    _ -> False
  }
}

fn accounting_parens(key: LocaleKey) -> Bool {
  base_language(key) != "de"
}

pub fn format_number_parts(opts: NumberFormatOptions, x: Float) -> List(Part) {
  let negative = is_negative_float(x)
  let dec = decimal_of_float(float.absolute_value(x))
  format_decimal_parts(opts, negative, dec)
}

pub fn format_decimal_string_parts(
  opts: NumberFormatOptions,
  s: String,
) -> List(Part) {
  let #(negative, rest) = case string.pop_grapheme(s) {
    Ok(#("-", rest)) -> #(True, rest)
    _ -> #(False, s)
  }
  format_decimal_parts(opts, negative, parse_decimal(rest))
}

fn format_decimal_parts(
  opts: NumberFormatOptions,
  negative negative: Bool,
  dec dec: Decimal,
) -> List(Part) {
  let opts = case opts.notation, opts.use_grouping {
    NotationCompact(..), GroupingAuto ->
      NumberFormatOptions(..opts, use_grouping: GroupingMin2)
    _, _ -> opts
  }
  let dec = case opts.style {
    StylePercent -> Decimal(..dec, exponent: dec.exponent + 2)
    StyleDecimal | StyleCurrency(..) | StyleUnit(..) -> dec
  }
  let dec = normalize(dec)
  let key = opts.locale
  let #(mantissa, exponent, suffix) = case opts.notation, dec.digits {
    _, "" | NotationStandard, _ -> #(dec, 0, no_compact_suffix)
    NotationScientific, _ -> #(
      Decimal(..dec, exponent: 1),
      dec.exponent - 1,
      no_compact_suffix,
    )
    NotationEngineering, _ -> {
      let e = 3 * floor_div(dec.exponent - 1, 3)
      #(Decimal(..dec, exponent: dec.exponent - e), e, no_compact_suffix)
    }
    NotationCompact(display:), _ -> {
      let suffix = compact_suffix(key, display, dec.exponent - 1)
      #(Decimal(..dec, exponent: dec.exponent - suffix.shift), 0, suffix)
    }
  }
  let digit_parts = format_digits(opts, mantissa, negative)
  let digit_parts = case opts.notation {
    NotationScientific | NotationEngineering -> {
      let exp_parts = case exponent < 0 {
        True -> [
          Part(PartExponentSeparator, "E"),
          Part(PartExponentMinusSign, "-"),
          Part(PartExponentInteger, int.to_string(-exponent)),
        ]
        False -> [
          Part(PartExponentSeparator, "E"),
          Part(PartExponentInteger, int.to_string(exponent)),
        ]
      }
      list.append(digit_parts, exp_parts)
    }
    NotationCompact(..) ->
      case suffix.plural {
        [] -> digit_parts
        _ ->
          case is_one_parts(digit_parts) {
            True -> list.append(digit_parts, suffix.singular)
            False -> list.append(digit_parts, suffix.plural)
          }
      }
    NotationStandard -> digit_parts
  }
  wrap_affixes(opts, digit_parts, negative, is_nan: False)
}

type CompactSuffix {
  CompactSuffix(shift: Int, singular: List(Part), plural: List(Part))
}

const no_compact_suffix = CompactSuffix(shift: 0, singular: [], plural: [])

fn same_suffix(shift: Int, parts: List(Part)) -> CompactSuffix {
  CompactSuffix(shift:, singular: parts, plural: parts)
}

// e is floor(log10(x))
fn compact_suffix(
  key: LocaleKey,
  display: CompactDisplay,
  e: Int,
) -> CompactSuffix {
  use <- bool.lazy_guard(key.base_tag == "en-IN", fn() {
    indian_compact(e, display)
  })
  case base_language(key) {
    "ja" ->
      cjk_compact(
        e,
        ten_thousand: "万",
        hundred_million: "億",
        trillion: "兆",
        thousand: None,
      )
    "ko" ->
      cjk_compact(
        e,
        ten_thousand: "만",
        hundred_million: "억",
        trillion: "조",
        thousand: Some("천"),
      )
    "zh" ->
      case key.base_tag {
        "zh-TW" | "zh-Hant" ->
          cjk_compact(
            e,
            ten_thousand: "萬",
            hundred_million: "億",
            trillion: "兆",
            thousand: None,
          )
        _ -> en_compact(e, display)
      }
    "de" -> de_compact(e, display)
    _ -> en_compact(e, display)
  }
}

fn en_compact(e: Int, display: CompactDisplay) -> CompactSuffix {
  use <- bool.guard(e < 3, no_compact_suffix)
  let k = int.min(4, e / 3)
  let suffix = case k, display {
    1, CompactShort -> [Part(PartCompact, "K")]
    2, CompactShort -> [Part(PartCompact, "M")]
    3, CompactShort -> [Part(PartCompact, "B")]
    _, CompactShort -> [Part(PartCompact, "T")]
    1, CompactLong -> [Part(PartLiteral, " "), Part(PartCompact, "thousand")]
    2, CompactLong -> [Part(PartLiteral, " "), Part(PartCompact, "million")]
    3, CompactLong -> [Part(PartLiteral, " "), Part(PartCompact, "billion")]
    _, CompactLong -> [Part(PartLiteral, " "), Part(PartCompact, "trillion")]
  }
  same_suffix(3 * k, suffix)
}

fn indian_compact(e: Int, display: CompactDisplay) -> CompactSuffix {
  let entry = fn(shift: Int, short: String, long: String) {
    case display {
      CompactShort -> same_suffix(shift, [Part(PartCompact, short)])
      CompactLong ->
        same_suffix(shift, [Part(PartLiteral, " "), Part(PartCompact, long)])
    }
  }
  case e {
    _ if e >= 3 && e <= 4 -> entry(3, "K", "thousand")
    _ if e >= 5 && e <= 6 -> entry(5, "L", "lakh")
    _ if e >= 7 -> entry(7, "Cr", "crore")
    _ -> no_compact_suffix
  }
}

fn cjk_compact(
  e: Int,
  ten_thousand ten_thousand: String,
  hundred_million hundred_million: String,
  trillion trillion: String,
  thousand thousand: Option(String),
) -> CompactSuffix {
  let unit = fn(shift: Int, name: String) {
    same_suffix(shift, [Part(PartCompact, name)])
  }
  case e {
    3 -> option.map(thousand, unit(3, _)) |> option.unwrap(no_compact_suffix)
    _ if e >= 4 && e <= 7 -> unit(4, ten_thousand)
    _ if e >= 8 && e <= 11 -> unit(8, hundred_million)
    _ if e >= 12 -> unit(12, trillion)
    _ -> no_compact_suffix
  }
}

fn de_compact(e: Int, display: CompactDisplay) -> CompactSuffix {
  let short = fn(s: String) {
    [Part(PartLiteral, "\u{00A0}"), Part(PartCompact, s)]
  }
  let long = fn(s: String) { [Part(PartLiteral, " "), Part(PartCompact, s)] }
  case display {
    CompactShort ->
      case e {
        _ if e >= 6 && e <= 8 -> same_suffix(6, short("Mio."))
        _ if e >= 9 && e <= 11 -> same_suffix(9, short("Mrd."))
        _ if e >= 12 -> same_suffix(12, short("Bio."))
        _ -> no_compact_suffix
      }
    CompactLong ->
      case e {
        _ if e >= 3 && e <= 5 -> same_suffix(3, long("Tausend"))
        _ if e >= 6 && e <= 8 ->
          CompactSuffix(6, long("Million"), long("Millionen"))
        _ if e >= 9 && e <= 11 ->
          CompactSuffix(9, long("Milliarde"), long("Milliarden"))
        _ if e >= 12 -> CompactSuffix(12, long("Billion"), long("Billionen"))
        _ -> no_compact_suffix
      }
  }
}

pub fn format_nan_parts(opts: NumberFormatOptions) -> List(Part) {
  wrap_affixes(
    opts,
    [Part(PartNan, nan_text(opts.locale))],
    negative: False,
    is_nan: True,
  )
}

pub fn format_infinity_parts(
  opts: NumberFormatOptions,
  negative negative: Bool,
) -> List(Part) {
  wrap_affixes(opts, [Part(PartInfinity, "∞")], negative, is_nan: False)
}

fn wrap_affixes(
  opts: NumberFormatOptions,
  core: List(Part),
  negative negative: Bool,
  is_nan is_nan: Bool,
) -> List(Part) {
  let key = opts.locale
  let zero = !is_nan && !negative && is_zero_parts(core)
  let neg_zero = negative && is_zero_parts(core)
  let show_minus = case opts.sign_display {
    SignNever -> False
    SignAlways -> negative
    SignExceptZero -> negative && !neg_zero
    SignNegative -> negative && !neg_zero
    SignAuto -> negative
  }
  let show_plus = case opts.sign_display, is_nan {
    SignAlways, _ -> !negative
    SignExceptZero, False -> !negative && !zero
    SignExceptZero, True -> False
    SignAuto, _ | SignNever, _ | SignNegative, _ -> False
  }
  let accounting = case opts.style {
    StyleCurrency(sign: AccountingSign, ..) ->
      show_minus && accounting_parens(key)
    StyleCurrency(sign: StandardSign, ..)
    | StyleDecimal
    | StylePercent
    | StyleUnit(..) -> False
  }
  let sign_parts = case accounting {
    True -> []
    False ->
      case show_minus, show_plus {
        True, _ -> [Part(PartMinusSign, "-")]
        _, True -> [Part(PartPlusSign, "+")]
        False, False -> []
      }
  }
  case opts.style {
    StylePercent ->
      list.flatten([sign_parts, core, [Part(PartPercentSign, "%")]])
    StyleCurrency(currency: code, display: cur_display, ..) -> {
      let #(text, spaced) = currency_text(key, code, cur_display)
      let with_cur = case cur_display {
        CurrencyName ->
          list.append(core, [Part(PartLiteral, " "), Part(PartCurrency, text)])
        CurrencyCode | CurrencySymbol | CurrencyNarrowSymbol ->
          case currency_suffixed(key) {
            True ->
              list.append(core, [
                Part(PartLiteral, "\u{00A0}"),
                Part(PartCurrency, text),
              ])
            False ->
              case spaced {
                True ->
                  list.flatten([
                    [Part(PartCurrency, text), Part(PartLiteral, " ")],
                    core,
                  ])
                False -> [Part(PartCurrency, text), ..core]
              }
          }
      }
      case accounting {
        True ->
          list.flatten([
            [Part(PartLiteral, "(")],
            with_cur,
            [Part(PartLiteral, ")")],
          ])
        False -> list.append(sign_parts, with_cur)
      }
    }
    StyleUnit(unit: u, display: u_display) -> {
      let #(u_pre, u_suf) = unit_affixes(key, u, u_display, is_one_parts(core))
      list.flatten([u_pre, sign_parts, core, u_suf])
    }
    StyleDecimal -> list.append(sign_parts, core)
  }
}

fn unit_affixes(
  key: LocaleKey,
  unit: String,
  display: UnitDisplay,
  one one: Bool,
) -> #(List(Part), List(Part)) {
  let lang = base_language(key)
  let hant = key.base_tag == "zh-TW" || key.base_tag == "zh-Hant"
  case unit, lang {
    "kilometer-per-hour", "de" ->
      case display {
        UnitLong -> #([], [
          Part(PartLiteral, " "),
          Part(PartUnit, "Kilometer pro Stunde"),
        ])
        UnitShort | UnitNarrow -> #([], [
          Part(PartLiteral, " "),
          Part(PartUnit, "km/h"),
        ])
      }
    "kilometer-per-hour", "ja" ->
      case display {
        UnitLong -> #([Part(PartUnit, "時速"), Part(PartLiteral, " ")], [
          Part(PartLiteral, " "),
          Part(PartUnit, "キロメートル"),
        ])
        UnitNarrow -> #([], [Part(PartUnit, "km/h")])
        UnitShort -> #([], [Part(PartLiteral, " "), Part(PartUnit, "km/h")])
      }
    "kilometer-per-hour", "ko" ->
      case display {
        UnitLong -> #([Part(PartUnit, "시속"), Part(PartLiteral, " ")], [
          Part(PartUnit, "킬로미터"),
        ])
        UnitShort | UnitNarrow -> #([], [Part(PartUnit, "km/h")])
      }
    "kilometer-per-hour", "zh" if hant ->
      case display {
        UnitLong -> #([Part(PartUnit, "每小時"), Part(PartLiteral, " ")], [
          Part(PartLiteral, " "),
          Part(PartUnit, "公里"),
        ])
        UnitNarrow -> #([], [Part(PartUnit, "公里/小時")])
        UnitShort -> #([], [Part(PartLiteral, " "), Part(PartUnit, "公里/小時")])
      }
    "percent", _ ->
      case display {
        UnitLong -> #([], [Part(PartLiteral, " "), Part(PartUnit, "percent")])
        UnitShort | UnitNarrow -> #([], [Part(PartUnit, "%")])
      }
    _, _ ->
      case display {
        UnitLong -> #([], [
          Part(PartLiteral, " "),
          Part(PartUnit, unit_name_long(unit, one)),
        ])
        UnitNarrow -> #([], [
          Part(PartUnit, unit_name(unit, narrow: True, one:)),
        ])
        UnitShort -> #([], [
          Part(PartLiteral, " "),
          Part(PartUnit, unit_name(unit, narrow: False, one:)),
        ])
      }
  }
}

pub fn plural_operands(parts: List(Part)) -> #(String, String) {
  let digits_of = fn(want: PartType) {
    parts
    |> list.filter_map(fn(p: Part) {
      case p.type_ == want {
        True -> Ok(p.value)
        False -> Error(Nil)
      }
    })
    |> string.join("")
  }
  #(digits_of(PartInteger), digits_of(PartFraction))
}

fn is_one_parts(parts: List(Part)) -> Bool {
  let #(int_digits, frac_digits) = plural_operands(parts)
  int_digits == "1" && frac_digits == ""
}

fn is_zero_parts(parts: List(Part)) -> Bool {
  let #(int_digits, frac_digits) = plural_operands(parts)
  let digits = int_digits <> frac_digits
  digits != "" && string.to_graphemes(digits) |> list.all(fn(c) { c == "0" })
}

fn currency_text(
  key: LocaleKey,
  code: String,
  display: CurrencyDisplay,
) -> #(String, Bool) {
  let usd_prefixed = case key.base_tag {
    "ko" | "ko-KR" | "zh-TW" | "zh-Hant" -> True
    _ -> False
  }
  case display {
    CurrencySymbol | CurrencyNarrowSymbol -> {
      let narrow = display == CurrencyNarrowSymbol
      case code {
        "USD" ->
          case !narrow && usd_prefixed {
            True -> #("US$", False)
            False -> #("$", False)
          }
        "EUR" -> #("€", False)
        "GBP" -> #("£", False)
        "JPY" -> #("¥", False)
        "CNY" -> #("CN¥", False)
        "KRW" -> #("₩", False)
        "INR" -> #("₹", False)
        "CAD" ->
          case narrow {
            True -> #("$", False)
            False -> #("CA$", False)
          }
        "AUD" ->
          case narrow {
            True -> #("$", False)
            False -> #("A$", False)
          }
        _ -> #(code, True)
      }
    }
    CurrencyName -> #(currency_name(code), False)
    CurrencyCode -> #(code, True)
  }
}

fn currency_name(code: String) -> String {
  case code {
    "USD" -> "US dollars"
    "EUR" -> "euros"
    "GBP" -> "British pounds"
    "JPY" -> "Japanese yen"
    _ -> code
  }
}

pub fn currency_digits(code: String) -> Int {
  case code {
    "BHD" | "IQD" | "JOD" | "KWD" | "LYD" | "OMR" | "TND" -> 3
    "BIF"
    | "CLP"
    | "DJF"
    | "GNF"
    | "ISK"
    | "JPY"
    | "KMF"
    | "KRW"
    | "PYG"
    | "RWF"
    | "UGX"
    | "UYI"
    | "VND"
    | "VUV"
    | "XAF"
    | "XOF"
    | "XPF" -> 0
    _ -> 2
  }
}

fn unit_name_long(unit: String, one one: Bool) -> String {
  let singular = fn(u: String) -> String {
    case u {
      "celsius" -> "degree Celsius"
      "fahrenheit" -> "degree Fahrenheit"
      "fluid-ounce" -> "fluid ounce"
      other -> other
    }
  }
  let plural = fn(u: String) -> String {
    case u {
      "celsius" -> "degrees Celsius"
      "fahrenheit" -> "degrees Fahrenheit"
      "fluid-ounce" -> "fluid ounces"
      "foot" -> "feet"
      "inch" -> "inches"
      "percent" -> "percent"
      other -> other <> "s"
    }
  }
  case string.split_once(unit, "-per-") {
    Ok(#(num, den)) ->
      case one {
        True -> singular(num) <> " per " <> singular(den)
        False -> plural(num) <> " per " <> singular(den)
      }
    Error(Nil) ->
      case one {
        True -> singular(unit)
        False -> plural(unit)
      }
  }
}

fn unit_name(unit: String, narrow narrow: Bool, one one: Bool) -> String {
  let simple = fn(u: String) -> String {
    case u, narrow {
      "acre", _ -> "ac"
      "bit", _ -> "bit"
      "byte", _ -> "byte"
      "celsius", _ -> "°C"
      "centimeter", _ -> "cm"
      "day", True -> "d"
      "day", False -> "day"
      "degree", True -> "°"
      "degree", False -> "deg"
      "fahrenheit", _ -> "°F"
      "fluid-ounce", _ -> "fl oz"
      "foot", True -> "′"
      "foot", False -> "ft"
      "gallon", _ -> "gal"
      "gigabit", _ -> "Gb"
      "gigabyte", _ -> "GB"
      "gram", _ -> "g"
      "hectare", _ -> "ha"
      "hour", True -> "h"
      "hour", False -> "hr"
      "inch", True -> "″"
      "inch", False -> "in"
      "kilobit", _ -> "kb"
      "kilobyte", _ -> "kB"
      "kilogram", _ -> "kg"
      "kilometer", _ -> "km"
      "liter", _ -> "L"
      "megabit", _ -> "Mb"
      "megabyte", _ -> "MB"
      "meter", _ -> "m"
      "microsecond", _ -> "μs"
      "mile", _ -> "mi"
      "mile-scandinavian", _ -> "smi"
      "milliliter", _ -> "mL"
      "millimeter", _ -> "mm"
      "millisecond", _ -> "ms"
      "minute", True -> "m"
      "minute", False -> "min"
      "month", True -> "m"
      "month", False -> "mth"
      "nanosecond", _ -> "ns"
      "ounce", _ -> "oz"
      "percent", _ -> "%"
      "petabyte", _ -> "PB"
      "pound", True -> "#"
      "pound", False -> "lb"
      "second", True -> "s"
      "second", False -> "sec"
      "stone", _ -> "st"
      "terabit", _ -> "Tb"
      "terabyte", _ -> "TB"
      "week", True -> "w"
      "week", False -> "wk"
      "yard", _ -> "yd"
      "year", True -> "y"
      "year", False -> "yr"
      _, _ -> u
    }
  }
  let counted = fn(u: String) -> String {
    case u, narrow || one {
      "day", False -> "days"
      "month", False -> "mths"
      "week", False -> "wks"
      "year", False -> "yrs"
      _, _ -> simple(u)
    }
  }
  case string.split_once(unit, "-per-") {
    Ok(#(num, den)) -> {
      let den_text = case den {
        "hour" -> "h"
        "second" -> "s"
        _ -> simple(den)
      }
      counted(num) <> "/" <> den_text
    }
    Error(Nil) -> counted(unit)
  }
}

pub fn sanctioned_units() -> List(String) {
  [
    "acre", "bit", "byte", "celsius", "centimeter", "day", "degree",
    "fahrenheit", "fluid-ounce", "foot", "gallon", "gigabit", "gigabyte", "gram",
    "hectare", "hour", "inch", "kilobit", "kilobyte", "kilogram", "kilometer",
    "liter", "megabit", "megabyte", "meter", "microsecond", "mile",
    "mile-scandinavian", "milliliter", "millimeter", "millisecond", "minute",
    "month", "nanosecond", "ounce", "percent", "petabyte", "pound", "second",
    "stone", "terabit", "terabyte", "week", "yard", "year",
  ]
}

pub fn is_well_formed_unit(unit: String) -> Bool {
  let sanctioned = sanctioned_units()
  case list.contains(sanctioned, unit) {
    True -> True
    False ->
      case string.split_once(unit, "-per-") {
        Ok(#(num, den)) ->
          list.contains(sanctioned, num) && list.contains(sanctioned, den)
        Error(Nil) -> False
      }
  }
}

// value = 0.digits * 10^exponent, digits "" is zero
type Decimal {
  Decimal(digits: String, exponent: Int)
}

const zero_decimal = Decimal(digits: "", exponent: 0)

fn decimal_of_float(x: Float) -> Decimal {
  parse_decimal(rt_val.js_format_float(x))
}

fn parse_decimal(s: String) -> Decimal {
  let #(base, e) = case string.split_once(s, "e") {
    Ok(#(b, ex)) -> #(b, parse_exp(ex))
    Error(Nil) ->
      case string.split_once(s, "E") {
        Ok(#(b, ex)) -> #(b, parse_exp(ex))
        Error(Nil) -> #(s, 0)
      }
  }
  let #(int_part, frac_part) = case string.split_once(base, ".") {
    Ok(#(i, f)) -> #(i, f)
    Error(Nil) -> #(base, "")
  }
  let digits = int_part <> frac_part
  let exponent = string.length(int_part) + e
  normalize(Decimal(digits:, exponent:))
}

fn parse_exp(s: String) -> Int {
  case string.pop_grapheme(s) {
    Ok(#("+", rest)) -> parse_int_or_zero(rest)
    _ -> parse_int_or_zero(s)
  }
}

fn normalize(dec: Decimal) -> Decimal {
  let #(digits, exponent) = strip_leading(dec.digits, dec.exponent)
  let digits = strip_trailing(digits)
  case digits {
    "" -> zero_decimal
    _ -> Decimal(digits:, exponent:)
  }
}

fn strip_leading(digits: String, exp: Int) -> #(String, Int) {
  case string.pop_grapheme(digits) {
    Ok(#("0", rest)) -> strip_leading(rest, exp - 1)
    _ -> #(digits, exp)
  }
}

fn strip_trailing(digits: String) -> String {
  case string.ends_with(digits, "0") {
    True -> strip_trailing(string.slice(digits, 0, string.length(digits) - 1))
    False -> digits
  }
}

fn round_to_leading_digits(
  dec: Decimal,
  keep keep: Int,
  mode mode: RoundingMode,
  negative negative: Bool,
) -> Decimal {
  let n_digits = string.length(dec.digits)
  use <- bool.guard(dec.digits == "" || keep >= n_digits, dec)
  let #(kept, dropped) = case keep <= 0 {
    True -> #("", dec.digits)
    False -> #(
      string.slice(dec.digits, 0, keep),
      string.slice(dec.digits, keep, n_digits - keep),
    )
  }
  let n = parse_int_or_zero(kept)
  let lead_zeros = int.max(-keep, 0)
  let rounded = case has_nonzero_digit(dropped) {
    False -> n
    True -> {
      let vs_half = compare_remainder_to_half(dropped, lead_zeros)
      case rounds_up(mode, negative, vs_half, odd: n % 2 == 1) {
        True -> n + 1
        False -> n
      }
    }
  }
  rebuild_rounded(dec, keep, rounded)
}

// odd only matters for halfeven
fn rounds_up(
  mode: RoundingMode,
  negative negative: Bool,
  vs_half vs_half: Order,
  odd odd: Bool,
) -> Bool {
  case vs_half, mode {
    _, RoundCeil -> !negative
    _, RoundFloor -> negative
    _, RoundExpand -> True
    _, RoundTrunc -> False
    order.Gt, _ -> True
    order.Lt, _ -> False
    order.Eq, RoundHalfCeil -> !negative
    order.Eq, RoundHalfFloor -> negative
    order.Eq, RoundHalfTrunc -> False
    order.Eq, RoundHalfExpand -> True
    order.Eq, RoundHalfEven -> odd
  }
}

fn has_nonzero_digit(digits: String) -> Bool {
  string.to_graphemes(digits) |> list.any(fn(c) { c != "0" })
}

// compares 0.{lead_zeros zeros}{dropped} against one half
fn compare_remainder_to_half(dropped: String, lead_zeros: Int) -> Order {
  use <- bool.guard(lead_zeros > 0 || !has_nonzero_digit(dropped), order.Lt)
  case string.pop_grapheme(dropped) {
    Error(Nil) -> order.Lt
    Ok(#(first, rest)) ->
      int.compare(parse_int_or_zero(first), 5)
      |> order.lazy_break_tie(fn() {
        case has_nonzero_digit(rest) {
          True -> order.Gt
          False -> order.Eq
        }
      })
  }
}

fn rebuild_rounded(dec: Decimal, keep: Int, rounded: Int) -> Decimal {
  case rounded == 0 {
    True -> zero_decimal
    False -> {
      let s = int.to_string(rounded)
      let kept_len = int.max(keep, 0)
      let exponent = dec.exponent - kept_len + string.length(s)
      normalize(Decimal(digits: s, exponent:))
    }
  }
}

fn round_fraction(
  dec: Decimal,
  fraction_digits: Int,
  inc: Int,
  mode: RoundingMode,
  negative negative: Bool,
) -> Decimal {
  let keep = dec.exponent + fraction_digits
  use <- bool.lazy_guard(inc == 1, fn() {
    round_to_leading_digits(dec, keep:, mode:, negative:)
  })
  let n_digits = string.length(dec.digits)
  let dropped = dropped_digits(dec, keep)
  let #(n, remainder_nonzero) = case keep >= n_digits, keep <= 0 {
    True, _ -> #(
      parse_int_or_zero(dec.digits) * int_math.pow10(keep - n_digits),
      False,
    )
    False, True -> #(0, dec.digits != "")
    False, False -> #(
      parse_int_or_zero(string.slice(dec.digits, 0, keep)),
      has_nonzero_digit(dropped),
    )
  }
  let r = n % inc
  let rounded = case r == 0 && !remainder_nonzero {
    True -> n
    False -> {
      let vs_half =
        remainder_vs_half_increment(r, inc, remainder_nonzero:, dropped:)
      case rounds_up(mode, negative, vs_half, odd: { n / inc } % 2 == 1) {
        True -> n - r + inc
        False -> n - r
      }
    }
  }
  case rounded == 0 {
    True -> zero_decimal
    False -> {
      let s = int.to_string(rounded)
      normalize(Decimal(digits: s, exponent: string.length(s) - fraction_digits))
    }
  }
}

fn remainder_vs_half_increment(
  r: Int,
  inc: Int,
  remainder_nonzero remainder_nonzero: Bool,
  dropped dropped: String,
) -> Order {
  case int.compare(2 * r, inc) {
    order.Gt -> order.Gt
    order.Eq ->
      case remainder_nonzero {
        True -> order.Gt
        False -> order.Eq
      }
    order.Lt ->
      case 2 * r + 1 == inc {
        True -> compare_remainder_to_half(dropped, 0)
        False -> order.Lt
      }
  }
}

fn dropped_digits(dec: Decimal, keep: Int) -> String {
  let n = string.length(dec.digits)
  case keep >= n, keep <= 0 {
    True, _ -> ""
    False, True -> dec.digits
    False, False -> string.slice(dec.digits, keep, n - keep)
  }
}

fn parse_int_or_zero(s: String) -> Int {
  int.parse(s) |> result.unwrap(0)
}

fn split_integer_fraction(dec: Decimal, frac_len: Int) -> #(String, String) {
  let n = string.length(dec.digits)
  let exponent = dec.exponent
  let #(int_text, frac_text) = case dec.digits, exponent <= 0, n <= exponent {
    "", _, _ -> #("0", "")
    _, True, _ -> #("0", string.repeat("0", -exponent) <> dec.digits)
    _, False, True -> #(dec.digits <> string.repeat("0", exponent - n), "")
    _, False, False -> #(
      string.slice(dec.digits, 0, exponent),
      string.slice(dec.digits, exponent, n - exponent),
    )
  }
  let flen = string.length(frac_text)
  let frac_text = case flen < frac_len {
    True -> frac_text <> string.repeat("0", frac_len - flen)
    False -> frac_text
  }
  #(int_text, frac_text)
}

fn format_digits(
  opts: NumberFormatOptions,
  dec: Decimal,
  negative negative: Bool,
) -> List(Part) {
  let mode = opts.rounding_mode
  let by_sig = fn(sig: Precision) { render_sig(dec, sig, mode, negative) }
  let by_frac = fn(frac: Precision) {
    render_frac(dec, frac, opts.rounding_increment, mode, negative)
  }
  let #(int_text, frac_text) = case opts.sig, opts.frac {
    Some(sig), None -> by_sig(sig)
    Some(sig), Some(frac) ->
      case prefer_sig(opts.rounding_priority, dec, sig, frac) {
        True -> by_sig(sig)
        False -> by_frac(frac)
      }
    None, Some(frac) -> by_frac(frac)
    None, None -> by_frac(Precision(min: 0, max: 3))
  }
  let frac_text = case opts.trailing_zero_display {
    TrailingZeroStripIfInteger ->
      case string.to_graphemes(frac_text) |> list.all(fn(c) { c == "0" }) {
        True -> ""
        False -> frac_text
      }
    TrailingZeroAuto -> frac_text
  }
  let key = opts.locale
  let int_text = string.pad_start(int_text, opts.min_int, "0")
  let int_parts = group_integer(opts, int_text)
  case frac_text {
    "" -> int_parts
    _ ->
      list.append(int_parts, [
        Part(PartDecimal, decimal_sep(key)),
        Part(PartFraction, frac_text),
      ])
  }
}

fn prefer_sig(
  priority: RoundingPriority,
  dec: Decimal,
  sig: Precision,
  frac: Precision,
) -> Bool {
  let sig_magnitude = case dec.digits {
    "" -> -sig.max
    _ -> dec.exponent - sig.max
  }
  let frac_magnitude = -frac.max
  case priority {
    PriorityAuto -> True
    PriorityMorePrecision -> sig_magnitude <= frac_magnitude
    PriorityLessPrecision -> sig_magnitude >= frac_magnitude
  }
}

// §15.1.3 torawprecision
fn render_sig(
  dec: Decimal,
  sig: Precision,
  mode: RoundingMode,
  negative negative: Bool,
) -> #(String, String) {
  case dec.digits {
    "" -> #("0", string.repeat("0", sig.min - 1))
    _ -> {
      let rounded =
        round_to_leading_digits(dec, keep: sig.max, mode:, negative:)
      let frac_len =
        int.max(0, string.length(rounded.digits) - rounded.exponent)
      let #(i, f) = split_integer_fraction(rounded, frac_len)
      let count = count_sig(i, f)
      let f = case count < sig.min {
        True -> f <> string.repeat("0", sig.min - count)
        False -> f
      }
      #(i, f)
    }
  }
}

// §15.1.4 torawfixed
fn render_frac(
  dec: Decimal,
  frac: Precision,
  rounding_increment: Int,
  mode: RoundingMode,
  negative negative: Bool,
) -> #(String, String) {
  let rounded =
    round_fraction(dec, frac.max, rounding_increment, mode, negative)
  let #(i, f) = split_integer_fraction(rounded, frac.max)
  #(i, strip_frac_to_min(f, frac.min))
}

fn count_sig(int_text: String, frac_text: String) -> Int {
  let all = int_text <> frac_text
  let #(stripped, _) = strip_leading(all, 0)
  string.length(stripped)
}

fn strip_frac_to_min(frac: String, min: Int) -> String {
  case string.length(frac) > min && string.ends_with(frac, "0") {
    True ->
      strip_frac_to_min(string.slice(frac, 0, string.length(frac) - 1), min)
    False -> string.pad_end(frac, min, "0")
  }
}

fn group_integer(opts: NumberFormatOptions, int_text: String) -> List(Part) {
  let key = opts.locale
  let n = string.length(int_text)
  let grouped = case opts.use_grouping {
    GroupingNever -> False
    GroupingAlways -> n > 3
    GroupingMin2 -> n > 4
    GroupingAuto -> n > 3
  }
  case grouped {
    False -> [Part(PartInteger, int_text)]
    True -> {
      let groups = case indian_grouping(key) {
        True -> split_groups_indian(int_text)
        False -> split_groups(int_text)
      }
      let sep = group_sep(key)
      groups
      |> list.map(fn(g) { [Part(PartGroup, sep), Part(PartInteger, g)] })
      |> list.flatten
      |> list.drop(1)
    }
  }
}

fn split_groups(s: String) -> List(String) {
  let n = string.length(s)
  let first = { n - 1 } % 3 + 1
  let head = string.slice(s, 0, first)
  split_groups_loop(string.slice(s, first, n - first), [head])
}

fn split_groups_loop(s: String, acc: List(String)) -> List(String) {
  case s {
    "" -> list.reverse(acc)
    _ ->
      split_groups_loop(string.slice(s, 3, string.length(s) - 3), [
        string.slice(s, 0, 3),
        ..acc
      ])
  }
}

fn split_groups_indian(s: String) -> List(String) {
  let n = string.length(s)
  case n <= 3 {
    True -> [s]
    False -> {
      let head = string.slice(s, 0, n - 3)
      let tail = string.slice(s, n - 3, 3)
      list.append(split_pairs(head), [tail])
    }
  }
}

fn split_pairs(s: String) -> List(String) {
  let n = string.length(s)
  case n <= 2 {
    True -> [s]
    False -> {
      let first = { n - 1 } % 2 + 1
      let head = string.slice(s, 0, first)
      split_pairs_loop(string.slice(s, first, n - first), [head])
    }
  }
}

fn split_pairs_loop(s: String, acc: List(String)) -> List(String) {
  case s {
    "" -> list.reverse(acc)
    _ ->
      split_pairs_loop(string.slice(s, 2, string.length(s) - 2), [
        string.slice(s, 0, 2),
        ..acc
      ])
  }
}

pub type PluralCategory {
  PluralOne
  PluralTwo
  PluralFew
  PluralOther
}

pub fn plural_category_text(c: PluralCategory) -> String {
  case c {
    PluralOne -> "one"
    PluralTwo -> "two"
    PluralFew -> "few"
    PluralOther -> "other"
  }
}

pub fn plural_select_en(
  type_: PluralType,
  int_digits: String,
  frac_digits: String,
) -> PluralCategory {
  case type_ {
    Ordinal -> {
      let n = parse_int_or_zero(int_digits)
      let n = int.absolute_value(n)
      let r10 = n % 10
      let r100 = n % 100
      case r10, r100 {
        1, _ if r100 != 11 -> PluralOne
        2, _ if r100 != 12 -> PluralTwo
        3, _ if r100 != 13 -> PluralFew
        _, _ -> PluralOther
      }
    }
    Cardinal ->
      case int_digits == "1" && frac_digits == "" {
        True -> PluralOne
        False -> PluralOther
      }
  }
}

pub fn plural_categories_en(type_: PluralType) -> List(PluralCategory) {
  case type_ {
    Ordinal -> [PluralFew, PluralOne, PluralOther, PluralTwo]
    Cardinal -> [PluralOne, PluralOther]
  }
}

pub fn list_format_parts(
  type_: ListFormatType,
  style: ListFormatStyle,
  items: List(String),
) -> List(Part) {
  case items {
    [] -> []
    [only] -> [Part(PartElement, only)]
    [a, b] -> {
      let sep = two_separator(type_, style)
      [Part(PartElement, a), Part(PartLiteral, sep), Part(PartElement, b)]
    }
    [first, ..rest] -> {
      let mid = case type_, style {
        UnitList, ListNarrow -> " "
        UnitList, ListLong | UnitList, ListShort -> ", "
        Conjunction, _ | Disjunction, _ -> ", "
      }
      let last_sep = end_separator(type_, style)
      build_list_parts(rest, [Part(PartElement, first)], mid, last_sep)
    }
  }
}

fn two_separator(type_: ListFormatType, style: ListFormatStyle) -> String {
  case type_, style {
    Conjunction, ListNarrow -> ", "
    Conjunction, ListShort -> " & "
    Conjunction, ListLong -> " and "
    Disjunction, _ -> " or "
    UnitList, ListNarrow -> " "
    UnitList, ListLong | UnitList, ListShort -> ", "
  }
}

fn end_separator(type_: ListFormatType, style: ListFormatStyle) -> String {
  case type_, style {
    Conjunction, ListLong -> ", and "
    Conjunction, ListShort -> ", & "
    Conjunction, ListNarrow -> ", "
    Disjunction, _ -> ", or "
    UnitList, ListNarrow -> " "
    UnitList, ListLong | UnitList, ListShort -> ", "
  }
}

fn build_list_parts(
  remaining: List(String),
  acc: List(Part),
  mid: String,
  last_sep: String,
) -> List(Part) {
  case remaining {
    [] -> list.reverse(acc)
    [last] ->
      list.reverse([Part(PartElement, last), Part(PartLiteral, last_sep), ..acc])
    [next, ..rest] ->
      build_list_parts(
        rest,
        [Part(PartElement, next), Part(PartLiteral, mid), ..acc],
        mid,
        last_sep,
      )
  }
}

pub fn relative_time_parts_en(
  style: RelativeTimeStyle,
  numeric: RelativeTimeNumeric,
  value: Float,
  unit: String,
  value_parts: List(Part),
) -> List(UnitPart) {
  let is_auto = case numeric {
    NumericAuto -> True
    NumericAlways -> False
  }
  let with_unit = fn(p: Part) { UnitPart(p.type_, p.value, Some(unit)) }
  let literal = fn(text) { UnitPart(PartLiteral, text, None) }
  // normalize -0.0 so the 0.0 patterns match
  let v = case rt_val.is_neg_zero(value) {
    True -> 0.0
    False -> value
  }
  case is_auto, relative_time_auto_name(unit, v) {
    True, Some(name) -> [literal(name)]
    _, _ -> {
      let plural = case float.absolute_value(value) {
        1.0 -> PluralOne
        _ -> PluralOther
      }
      let unit_text = relative_time_unit_en(style, unit, plural)
      let past = is_negative_float(value)
      let tagged =
        list.map(value_parts, fn(p: Part) {
          case p.type_ {
            PartLiteral -> literal(p.value)
            _ -> with_unit(p)
          }
        })
      case past {
        True -> list.append(tagged, [literal(" " <> unit_text <> " ago")])
        False ->
          list.flatten([[literal("in ")], tagged, [literal(" " <> unit_text)]])
      }
    }
  }
}

fn relative_time_auto_name(unit: String, value: Float) -> Option(String) {
  case unit, value {
    "second", 0.0 -> Some("now")
    "day", -1.0 -> Some("yesterday")
    "day", 0.0 -> Some("today")
    "day", 1.0 -> Some("tomorrow")
    "week", -1.0 -> Some("last week")
    "week", 0.0 -> Some("this week")
    "week", 1.0 -> Some("next week")
    "month", -1.0 -> Some("last month")
    "month", 0.0 -> Some("this month")
    "month", 1.0 -> Some("next month")
    "quarter", -1.0 -> Some("last quarter")
    "quarter", 0.0 -> Some("this quarter")
    "quarter", 1.0 -> Some("next quarter")
    "year", -1.0 -> Some("last year")
    "year", 0.0 -> Some("this year")
    "year", 1.0 -> Some("next year")
    _, _ -> None
  }
}

fn relative_time_unit_en(
  style: RelativeTimeStyle,
  unit: String,
  plural: PluralCategory,
) -> String {
  let single = case plural {
    PluralOne -> True
    PluralTwo | PluralFew | PluralOther -> False
  }
  case style {
    RelativeLong ->
      case single {
        True -> unit
        False -> unit <> "s"
      }
    RelativeShort | RelativeNarrow ->
      case unit, single {
        "second", _ -> "sec."
        "minute", _ -> "min."
        "hour", _ -> "hr."
        "day", True -> "day"
        "day", False -> "days"
        "week", _ -> "wk."
        "month", _ -> "mo."
        "quarter", True -> "qtr."
        "quarter", False -> "qtrs."
        "year", _ -> "yr."
        _, _ -> unit
      }
  }
}

pub type DateFields {
  DateFields(
    year: Int,
    month: Int,
    day: Int,
    hour: Int,
    minute: Int,
    second: Int,
    millisecond: Int,
    week_day: Int,
  )
}

pub fn fields_from_epoch_ms(ms: Float, offset_minutes: Int) -> DateFields {
  let total_ms = float.truncate(ms) + offset_minutes * 60_000
  let days = floor_div(total_ms, 86_400_000)
  let ms_in_day = total_ms - days * 86_400_000
  let millisecond = ms_in_day % 1000
  let total_seconds = ms_in_day / 1000
  let second = total_seconds % 60
  let minute = { total_seconds / 60 } % 60
  let hour = total_seconds / 3600
  let week_day = gregorian.weekday_from_days(days)
  let gregorian.CivilDate(year, month, day) = civil_from_days(days)
  DateFields(
    year:,
    month:,
    day:,
    hour:,
    minute:,
    second:,
    millisecond:,
    week_day:,
  )
}

pub fn month_name(m: Int, width: NameWidth) -> String {
  let long = case m {
    1 -> "January"
    2 -> "February"
    3 -> "March"
    4 -> "April"
    5 -> "May"
    6 -> "June"
    7 -> "July"
    8 -> "August"
    9 -> "September"
    10 -> "October"
    11 -> "November"
    12 -> "December"
    _ -> ""
  }
  case width {
    WidthLong -> long
    WidthShort -> string.slice(long, 0, 3)
    WidthNarrow -> string.slice(long, 0, 1)
  }
}

pub fn weekday_name(wd: Int, width: NameWidth) -> String {
  let long = case wd {
    0 -> "Sunday"
    1 -> "Monday"
    2 -> "Tuesday"
    3 -> "Wednesday"
    4 -> "Thursday"
    5 -> "Friday"
    6 -> "Saturday"
    _ -> ""
  }
  case width {
    WidthLong -> long
    WidthShort -> string.slice(long, 0, 3)
    WidthNarrow -> string.slice(long, 0, 1)
  }
}

pub fn era_name(year: Int, width: NameWidth) -> String {
  let bc = year <= 0
  case width, bc {
    WidthLong, True -> "Before Christ"
    WidthLong, False -> "Anno Domini"
    WidthNarrow, True -> "B"
    WidthNarrow, False -> "A"
    WidthShort, True -> "BC"
    WidthShort, False -> "AD"
  }
}

pub fn day_period_name(hour: Int, minute: Int, width: NameWidth) -> String {
  case hour * 60 + minute == 720, width {
    True, WidthNarrow -> "n"
    True, WidthLong | True, WidthShort -> "noon"
    False, _ ->
      case hour {
        h if h < 6 -> "at night"
        h if h < 12 -> "in the morning"
        h if h < 18 -> "in the afternoon"
        h if h < 21 -> "in the evening"
        _ -> "at night"
      }
  }
}

pub fn language_display_name(code: String) -> Option(String) {
  case code {
    "en" -> Some("English")
    "en-US" -> Some("American English")
    "en-GB" -> Some("British English")
    "de" -> Some("German")
    "fr" -> Some("French")
    "es" -> Some("Spanish")
    "it" -> Some("Italian")
    "pt" -> Some("Portuguese")
    "ru" -> Some("Russian")
    "zh" -> Some("Chinese")
    "ja" -> Some("Japanese")
    "ko" -> Some("Korean")
    "ar" -> Some("Arabic")
    "hi" -> Some("Hindi")
    "nl" -> Some("Dutch")
    "sv" -> Some("Swedish")
    "pl" -> Some("Polish")
    "tr" -> Some("Turkish")
    "he" -> Some("Hebrew")
    "th" -> Some("Thai")
    _ -> None
  }
}

pub fn region_display_name(code: String) -> Option(String) {
  case code {
    "US" -> Some("United States")
    "GB" -> Some("United Kingdom")
    "DE" -> Some("Germany")
    "FR" -> Some("France")
    "ES" -> Some("Spain")
    "IT" -> Some("Italy")
    "JP" -> Some("Japan")
    "CN" -> Some("China")
    "KR" -> Some("South Korea")
    "RU" -> Some("Russia")
    "CA" -> Some("Canada")
    "AU" -> Some("Australia")
    "BR" -> Some("Brazil")
    "IN" -> Some("India")
    "MX" -> Some("Mexico")
    "NL" -> Some("Netherlands")
    "419" -> Some("Latin America")
    _ -> None
  }
}

pub fn script_display_name(code: String) -> Option(String) {
  case code {
    "Latn" -> Some("Latin")
    "Cyrl" -> Some("Cyrillic")
    "Arab" -> Some("Arabic")
    "Hans" -> Some("Simplified Han")
    "Hant" -> Some("Traditional Han")
    "Grek" -> Some("Greek")
    "Hebr" -> Some("Hebrew")
    "Jpan" -> Some("Japanese")
    "Kore" -> Some("Korean")
    "Deva" -> Some("Devanagari")
    _ -> None
  }
}

pub fn currency_display_name(code: String) -> Option(String) {
  case code {
    "USD" -> Some("US Dollar")
    "EUR" -> Some("Euro")
    "GBP" -> Some("British Pound")
    "JPY" -> Some("Japanese Yen")
    "CNY" -> Some("Chinese Yuan")
    "CHF" -> Some("Swiss Franc")
    "CAD" -> Some("Canadian Dollar")
    "AUD" -> Some("Australian Dollar")
    _ -> None
  }
}

pub fn numbering_systems() -> List(String) {
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

pub fn is_numbering_system(s: String) -> Bool {
  list.contains(numbering_systems(), s)
}

pub fn apply_numbering_system(
  parts: List(Part),
  nu: String,
  translits: fn(PartType) -> Bool,
) -> List(Part) {
  case nu {
    "latn" -> parts
    _ ->
      list.map(parts, fn(part: Part) {
        case translits(part.type_) {
          True -> Part(part.type_, translit_digits(part.value, nu))
          False -> part
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

fn is_negative_float(x: Float) -> Bool {
  x <. 0.0 || rt_val.is_neg_zero(x)
}
