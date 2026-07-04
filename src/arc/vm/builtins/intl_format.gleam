//// Pure formatting engines for the Intl builtins (root/English locale data).
////
//// No heap/state dependencies: every function maps plain Gleam values to
//// lists of `#(type, value)` parts, mirroring the partitioning operations of
//// ECMA-402 (PartitionNumberPattern, PartitionDateTimePattern, …). The
//// builtins layer turns parts into strings or {type, value} part objects.

import arc/internal/gregorian.{civil_from_days, floor_div}
import arc/vm/ops/operators
import arc/vm/value.{
  type CompactDisplay, type CurrencyDisplay, type Granularity,
  type IntlUseGrouping, type ListFormatStyle, type ListFormatType,
  type NameWidth, type Notation, type NumStyle, type PluralType,
  type RoundingMode, type RoundingPriority, type RtfNumeric, type RtfStyle,
  type SignDisplay, type TrailingZeroDisplay, type UnitDisplay, Cardinal,
  CompactLong, CompactShort, Conjunction, CurAccounting, CurCode, CurName,
  CurNarrowSymbol, CurStandard, CurSymbol, Disjunction, GGrapheme, GSentence,
  GWord, GroupingAlways, GroupingAuto, GroupingMin2, GroupingNever, LLong,
  LNarrow, LShort, NotationCompact, NotationEngineering, NotationScientific,
  NotationStandard, Ordinal, PriorityAuto, PriorityLessPrecision,
  PriorityMorePrecision, RoundCeil, RoundExpand, RoundFloor, RoundHalfCeil,
  RoundHalfEven, RoundHalfExpand, RoundHalfFloor, RoundHalfTrunc, RoundTrunc,
  RtfAlways, RtfAuto, RtfLong, RtfNarrow, RtfShort, SignAlways, SignAuto,
  SignExceptZero, SignNegative, SignNever, StyleCurrency, StyleDecimal,
  StylePercent, StyleUnit, TzdAuto, TzdStripIfInteger, UnitList, UnitLong,
  UnitNarrow, UnitShort, WLong, WNarrow, WShort,
}
import gleam/bool
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// The kind of a formatted part. Covers every part type ECMA-402 lets the
/// number, date-time and list formatters emit; the spec-visible strings live
/// in `part_type_to_js_string` and nowhere else.
pub type PartType {
  // Number parts (§15.5.x)
  PInteger
  PGroup
  PDecimal
  PFraction
  PCurrency
  PPercentSign
  PPlusSign
  PMinusSign
  PUnit
  PCompact
  PExponentSeparator
  PExponentMinusSign
  PExponentInteger
  PNaN
  PInfinity
  /// The "~" emitted by formatRange when the two endpoints collapse (§15.5.6).
  PApproximatelySign
  // Date-time parts (§11.5.x)
  PWeekday
  PEra
  PYear
  PMonth
  PDay
  PHour
  PMinute
  PSecond
  PFractionalSecond
  PDayPeriod
  PTimeZoneName
  // List parts (§13.5.x)
  PElement
  // Shared
  PLiteral
}

/// The spec-visible `type` string of a part. Only the JS boundary
/// (`formatToParts` and friends) should call this.
pub fn part_type_to_js_string(t: PartType) -> String {
  case t {
    PInteger -> "integer"
    PGroup -> "group"
    PDecimal -> "decimal"
    PFraction -> "fraction"
    PCurrency -> "currency"
    PPercentSign -> "percentSign"
    PPlusSign -> "plusSign"
    PMinusSign -> "minusSign"
    PUnit -> "unit"
    PCompact -> "compact"
    PExponentSeparator -> "exponentSeparator"
    PExponentMinusSign -> "exponentMinusSign"
    PExponentInteger -> "exponentInteger"
    PNaN -> "nan"
    PInfinity -> "infinity"
    PApproximatelySign -> "approximatelySign"
    PWeekday -> "weekday"
    PEra -> "era"
    PYear -> "year"
    PMonth -> "month"
    PDay -> "day"
    PHour -> "hour"
    PMinute -> "minute"
    PSecond -> "second"
    PFractionalSecond -> "fractionalSecond"
    PDayPeriod -> "dayPeriod"
    PTimeZoneName -> "timeZoneName"
    PElement -> "element"
    PLiteral -> "literal"
  }
}

/// The behavioural class of a part type: the single place the 27 variants are
/// bucketed. Every predicate over `PartType` is expressed over this class, so
/// exhaustiveness is checked exactly once, in `part_class`.
pub type PartClass {
  /// Digits of the value itself (integer/fraction).
  NumberDigit
  /// Digits of a scientific exponent — transliterated, but not value digits.
  NumberExponentDigit
  /// Non-digit pieces of the numeric core (separators, NaN/Infinity, compact).
  NumberCore
  /// Signs, currency and unit decoration around the numeric core.
  NumberAffix
  /// Date-time fields written in digits.
  DateNumeric
  /// Date-time fields written as words/symbols.
  DateText
  /// Literals and list elements.
  OtherPart
}

pub fn part_class(t: PartType) -> PartClass {
  case t {
    PInteger | PFraction -> NumberDigit
    PExponentInteger -> NumberExponentDigit
    PGroup
    | PDecimal
    | PCompact
    | PNaN
    | PInfinity
    | PExponentSeparator
    | PExponentMinusSign -> NumberCore
    PCurrency
    | PPercentSign
    | PPlusSign
    | PMinusSign
    | PUnit
    | PApproximatelySign -> NumberAffix
    PYear | PMonth | PDay | PHour | PMinute | PSecond | PFractionalSecond ->
      DateNumeric
    PWeekday | PEra | PDayPeriod | PTimeZoneName -> DateText
    PLiteral | PElement -> OtherPart
  }
}

/// Whether the numbering system's digits apply to this part of a formatted
/// number.
pub fn is_number_digit(t: PartType) -> Bool {
  case part_class(t) {
    NumberDigit | NumberExponentDigit -> True
    NumberCore | NumberAffix | DateNumeric | DateText | OtherPart -> False
  }
}

/// Whether the numbering system's digits apply to this part of a formatted
/// date-time.
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

/// A formatted part: #(type, value), e.g. #(PInteger, "1"), #(PGroup, ",").
pub type Part =
  #(PartType, String)

/// Which endpoint of a formatted range a part came from — the `source`
/// property of `formatRangeToParts` (§15.5.6 / §11.5.7).
pub type PartSource {
  SourceStart
  SourceEnd
  SourceShared
}

pub fn part_source_to_js_string(s: PartSource) -> String {
  case s {
    SourceStart -> "startRange"
    SourceEnd -> "endRange"
    SourceShared -> "shared"
  }
}

/// A part of a formatted *range*: `{ type, value, source }`.
pub type RangePart {
  RangePart(type_: PartType, value: String, source: PartSource)
}

/// A part of a formatted relative time / duration: `{ type, value, unit? }`.
/// An empty `unit` means the part carries no `unit` property.
pub type UnitPart {
  UnitPart(type_: PartType, value: String, unit: String)
}

pub fn parts_to_string(parts: List(Part)) -> String {
  parts |> list.map(fn(p) { p.1 }) |> string.join("")
}

pub fn unit_parts_to_string(parts: List(UnitPart)) -> String {
  parts |> list.map(fn(p) { p.value }) |> string.join("")
}

/// Combine two formatted endpoints into range parts. Implements the ICU
/// collapse heuristic: identical affixes are emitted once when they amount to
/// more than one code point ("+$2.90–3.10"), and the range separator is padded
/// with spaces when an uncollapsed affix sits next to it ("$3 – $5" vs
/// "987–988").
pub fn format_range_combine(
  key: LocaleKey,
  x_parts: List(Part),
  y_parts: List(Part),
) -> List(RangePart) {
  let js = fn(p: Part, source) { RangePart(p.0, p.1, source) }
  let start3 = fn(p: Part) { js(p, SourceStart) }
  let end3 = fn(p: Part) { js(p, SourceEnd) }
  let shared3 = fn(p: Part) { js(p, SourceShared) }
  let sep3 = fn(spaced) {
    js(#(PLiteral, range_sep(key, spaced)), SourceShared)
  }
  case parts_to_string(x_parts) == parts_to_string(y_parts) {
    True -> [
      RangePart(PApproximatelySign, "~", SourceShared),
      ..list.map(x_parts, shared3)
    ]
    False -> {
      let #(x_pre, x_core, x_suf) = split_range_affixes(x_parts)
      let #(y_pre, y_core, y_suf) = split_range_affixes(y_parts)
      let affix_cp =
        list.fold(list.append(x_pre, x_suf), 0, fn(acc, p: Part) {
          acc + string.length(p.1)
        })
      case x_pre == y_pre && x_suf == y_suf && affix_cp != 1 {
        True ->
          list.flatten([
            list.map(x_pre, shared3),
            list.map(x_core, start3),
            [sep3(False)],
            list.map(y_core, end3),
            list.map(x_suf, shared3),
          ])
        False -> {
          let spaced = x_suf != [] || y_pre != []
          list.flatten([
            list.map(x_parts, start3),
            [sep3(spaced)],
            list.map(y_parts, end3),
          ])
        }
      }
    }
  }
}

/// Split formatted parts into (prefix affix, numeric core, suffix affix).
/// The core spans from the first to the last digit-bearing part; signs and
/// currency symbols outside that span are affixes for collapse purposes.
fn split_range_affixes(
  parts: List(Part),
) -> #(List(Part), List(Part), List(Part)) {
  let is_core = fn(p: Part) {
    case part_class(p.0) {
      NumberDigit | NumberExponentDigit | NumberCore -> True
      NumberAffix | DateNumeric | DateText | OtherPart -> False
    }
  }
  let #(pre, rest) = list.split_while(parts, fn(p) { !is_core(p) })
  let #(rev_suf, rev_core) =
    list.split_while(list.reverse(rest), fn(p) { !is_core(p) })
  #(pre, list.reverse(rev_core), list.reverse(rev_suf))
}

/// Locale range separator. `spaced` requests the space-padded variant used
/// when an affix is adjacent to the separator.
fn range_sep(key: LocaleKey, spaced: Bool) -> String {
  case loc_lang(key) {
    "pt" -> " - "
    _ ->
      case spaced {
        True -> " – "
        False -> "–"
      }
  }
}

// ============================================================================
// Number formatting — ECMA-402 §15.5 (root/en patterns)
// ============================================================================

pub type NumOpts {
  NumOpts(
    locale: LocaleKey,
    style: NumStyle,
    min_int: Int,
    min_frac: Option(Int),
    max_frac: Option(Int),
    min_sig: Option(Int),
    max_sig: Option(Int),
    use_grouping: IntlUseGrouping,
    notation: Notation,
    sign_display: SignDisplay,
    rounding_increment: Int,
    rounding_mode: RoundingMode,
    rounding_priority: RoundingPriority,
    trailing_zero_display: TrailingZeroDisplay,
  )
}

pub fn default_num_opts() -> NumOpts {
  NumOpts(
    locale: locale_key("en"),
    style: StyleDecimal,
    min_int: 1,
    min_frac: Some(0),
    max_frac: Some(3),
    min_sig: None,
    max_sig: None,
    use_grouping: GroupingAuto,
    notation: NotationStandard,
    sign_display: SignAuto,
    rounding_increment: 1,
    rounding_mode: RoundHalfExpand,
    rounding_priority: PriorityAuto,
    trailing_zero_display: TzdAuto,
  )
}

// ============================================================================
// Locale data (CLDR subset). The key is the resolved locale tag with any
// "-u-…" extension stripped, e.g. "de", "zh-TW", "en-IN". Unknown locales
// fall back to the en/root patterns, preserving the historical behavior.
// ============================================================================

/// A resolved locale tag with its "-u-…"/"-x-…" extension stripped — the shape
/// every locale-data table below is keyed by. Opaque and constructible only
/// through `locale_key`, so an unstripped tag ("de-u-nu-latn", which matches
/// no table row) cannot reach a lookup.
pub opaque type LocaleKey {
  LocaleKey(key: String)
}

/// Strip a "-u-…" (or any singleton) extension from a resolved locale tag.
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

/// Primary language subtag ("de-AT" → "de").
fn loc_lang(key: LocaleKey) -> String {
  case string.split_once(key.key, "-") {
    Ok(#(lang, _)) -> lang
    Error(Nil) -> key.key
  }
}

fn decimal_sep(key: LocaleKey) -> String {
  case loc_lang(key) {
    "de" | "pt" | "it" | "nl" -> ","
    _ -> "."
  }
}

fn group_sep(key: LocaleKey) -> String {
  case key.key {
    "pt-PT" -> "\u{00A0}"
    _ ->
      case loc_lang(key) {
        "de" | "pt" | "it" | "nl" -> "."
        _ -> ","
      }
  }
}

fn nan_str(key: LocaleKey) -> String {
  case key.key {
    "zh-TW" | "zh-Hant" -> "非數值"
    _ -> "NaN"
  }
}

/// Indian digit grouping (last 3, then 2s): 1,00,000.
fn indian_grouping(key: LocaleKey) -> Bool {
  case key.key {
    "en-IN" -> True
    _ -> loc_lang(key) == "hi"
  }
}

/// Currency placement: True → symbol suffixed after a NBSP ("987,00 $"),
/// False → symbol prefixed ("$987.00").
fn currency_suffixed(key: LocaleKey) -> Bool {
  case loc_lang(key) {
    "de" | "pt" -> True
    _ -> False
  }
}

/// Whether currencySign:"accounting" wraps negatives in parentheses.
/// CLDR de accounting pattern equals the standard pattern (minus sign).
fn accounting_parens(key: LocaleKey) -> Bool {
  loc_lang(key) != "de"
}

/// Format a finite float per the options. `is_nan`/`is_inf` are handled by
/// the caller. Returns the full part list including sign/affixes.
pub fn format_number_parts(opts: NumOpts, x: Float) -> List(Part) {
  let negative = operators.is_negative_float(x)
  let dec = decompose(float.absolute_value(x))
  format_dec_parts(opts, negative, dec)
}

/// Format an exact decimal string (e.g. "12.345", "-0.5") — used by
/// DurationFormat for arbitrary-precision combined values.
pub fn format_decimal_string_parts(opts: NumOpts, s: String) -> List(Part) {
  let #(negative, rest) = case string.pop_grapheme(s) {
    Ok(#("-", rest)) -> #(True, rest)
    _ -> #(False, s)
  }
  format_dec_parts(opts, negative, parse_decimal(rest))
}

fn format_dec_parts(opts: NumOpts, negative: Bool, dec: Dec) -> List(Part) {
  // Compact notation defaults useGrouping "auto" to the min2 behavior
  // (ECMA-402 §15.5.3 / CLDR compact patterns).
  let opts = case opts.notation, opts.use_grouping {
    NotationCompact(..), GroupingAuto ->
      NumOpts(..opts, use_grouping: GroupingMin2)
    _, _ -> opts
  }
  // Percent scaling happens before rounding (ECMA-402 §15.5.1).
  let dec = case opts.style {
    StylePercent -> Dec(..dec, exp: dec.exp + 2)
    StyleDecimal | StyleCurrency(..) | StyleUnit(..) -> dec
  }
  let dec = normalize(dec)
  let key = opts.locale
  let #(mantissa, exponent, compact_one, compact_other) = case opts.notation {
    NotationScientific ->
      case dec.digits {
        "" -> #(dec, 0, [], [])
        _ -> #(Dec(..dec, exp: 1), dec.exp - 1, [], [])
      }
    NotationEngineering ->
      case dec.digits {
        "" -> #(dec, 0, [], [])
        _ -> {
          let e = 3 * floor_div(dec.exp - 1, 3)
          #(Dec(..dec, exp: dec.exp - e), e, [], [])
        }
      }
    NotationCompact(display:) ->
      case dec.digits {
        "" -> #(dec, 0, [], [])
        _ -> {
          let #(div_exp, one_p, other_p) =
            compact_entry(key, display, dec.exp - 1)
          #(Dec(..dec, exp: dec.exp - div_exp), 0, one_p, other_p)
        }
      }
    NotationStandard -> #(dec, 0, [], [])
  }
  let digit_parts = format_digits(opts, mantissa, negative)
  let digit_parts = case opts.notation {
    NotationScientific | NotationEngineering -> {
      let exp_parts = case exponent < 0 {
        True -> [
          #(PExponentSeparator, "E"),
          #(PExponentMinusSign, "-"),
          #(PExponentInteger, int.to_string(-exponent)),
        ]
        False -> [
          #(PExponentSeparator, "E"),
          #(PExponentInteger, int.to_string(exponent)),
        ]
      }
      list.append(digit_parts, exp_parts)
    }
    NotationCompact(..) ->
      case compact_other {
        [] -> digit_parts
        _ ->
          case is_one_parts(digit_parts) {
            True -> list.append(digit_parts, compact_one)
            False -> list.append(digit_parts, compact_other)
          }
      }
    NotationStandard -> digit_parts
  }
  wrap_affixes(opts, digit_parts, negative, False)
}

/// Compact notation data (CLDR compact decimal patterns). `e` is the decimal
/// exponent of the value (floor(log10 |x|)). Returns
/// #(divisor_exponent, suffix parts for plural "one", suffix parts otherwise).
fn compact_entry(
  key: LocaleKey,
  display: CompactDisplay,
  e: Int,
) -> #(Int, List(Part), List(Part)) {
  use <- bool.lazy_guard(key.key == "en-IN", fn() { in_compact(e, display) })
  case loc_lang(key) {
    "ja" -> cjk_compact(e, "万", "億", "兆", None)
    "ko" -> cjk_compact(e, "만", "억", "조", Some("천"))
    "zh" ->
      case key.key {
        "zh-TW" | "zh-Hant" -> cjk_compact(e, "萬", "億", "兆", None)
        _ -> en_compact(e, display)
      }
    "de" -> de_compact(e, display)
    _ -> en_compact(e, display)
  }
}

fn en_compact(
  e: Int,
  display: CompactDisplay,
) -> #(Int, List(Part), List(Part)) {
  use <- bool.guard(e < 3, #(0, [], []))
  let k = int.min(4, e / 3)
  let suffix = case k, display {
    1, CompactShort -> [#(PCompact, "K")]
    2, CompactShort -> [#(PCompact, "M")]
    3, CompactShort -> [#(PCompact, "B")]
    _, CompactShort -> [#(PCompact, "T")]
    1, CompactLong -> [#(PLiteral, " "), #(PCompact, "thousand")]
    2, CompactLong -> [#(PLiteral, " "), #(PCompact, "million")]
    3, CompactLong -> [#(PLiteral, " "), #(PCompact, "billion")]
    _, CompactLong -> [#(PLiteral, " "), #(PCompact, "trillion")]
  }
  #(3 * k, suffix, suffix)
}

/// en-IN compact data: thousand (K), lakh (10^5, L), crore (10^7, Cr).
fn in_compact(
  e: Int,
  display: CompactDisplay,
) -> #(Int, List(Part), List(Part)) {
  let entry = fn(div: Int, short: String, long: String) {
    let suffix = case display {
      CompactShort -> [#(PCompact, short)]
      CompactLong -> [#(PLiteral, " "), #(PCompact, long)]
    }
    #(div, suffix, suffix)
  }
  case e {
    _ if e >= 3 && e <= 4 -> entry(3, "K", "thousand")
    _ if e >= 5 && e <= 6 -> entry(5, "L", "lakh")
    _ if e >= 7 -> entry(7, "Cr", "crore")
    _ -> #(0, [], [])
  }
}

/// CJK myriad-based compact data: 10^4, 10^8, 10^12 (plus an optional 10^3
/// form, used by Korean "천"). Suffixes attach without spacing.
fn cjk_compact(
  e: Int,
  m4: String,
  m8: String,
  m12: String,
  m3: Option(String),
) -> #(Int, List(Part), List(Part)) {
  case e {
    3 ->
      case m3 {
        Some(s) -> #(3, [#(PCompact, s)], [#(PCompact, s)])
        None -> #(0, [], [])
      }
    _ if e >= 4 && e <= 7 -> #(4, [#(PCompact, m4)], [#(PCompact, m4)])
    _ if e >= 8 && e <= 11 -> #(8, [#(PCompact, m8)], [#(PCompact, m8)])
    _ if e >= 12 -> #(12, [#(PCompact, m12)], [#(PCompact, m12)])
    _ -> #(0, [], [])
  }
}

fn de_compact(
  e: Int,
  display: CompactDisplay,
) -> #(Int, List(Part), List(Part)) {
  let short = fn(s: String) { [#(PLiteral, "\u{00A0}"), #(PCompact, s)] }
  let long = fn(s: String) { [#(PLiteral, " "), #(PCompact, s)] }
  case display {
    CompactShort ->
      // CLDR de short compact has no abbreviation below one million.
      case e {
        _ if e >= 6 && e <= 8 -> #(6, short("Mio."), short("Mio."))
        _ if e >= 9 && e <= 11 -> #(9, short("Mrd."), short("Mrd."))
        _ if e >= 12 -> #(12, short("Bio."), short("Bio."))
        _ -> #(0, [], [])
      }
    CompactLong ->
      case e {
        _ if e >= 3 && e <= 5 -> #(3, long("Tausend"), long("Tausend"))
        _ if e >= 6 && e <= 8 -> #(6, long("Million"), long("Millionen"))
        _ if e >= 9 && e <= 11 -> #(9, long("Milliarde"), long("Milliarden"))
        _ if e >= 12 -> #(12, long("Billion"), long("Billionen"))
        _ -> #(0, [], [])
      }
  }
}

pub fn format_nan_parts(opts: NumOpts) -> List(Part) {
  wrap_affixes(opts, [#(PNaN, nan_str(opts.locale))], False, True)
}

pub fn format_infinity_parts(opts: NumOpts, negative: Bool) -> List(Part) {
  wrap_affixes(opts, [#(PInfinity, "∞")], negative, False)
}

/// Add sign, currency/percent/unit affixes around the core digit parts.
fn wrap_affixes(
  opts: NumOpts,
  core: List(Part),
  negative: Bool,
  is_nan: Bool,
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
  // Accounting parentheses replace the minus sign when it would be shown —
  // in locales whose accounting pattern uses parentheses at all (not de).
  let accounting = case opts.style {
    StyleCurrency(sign: CurAccounting, ..) ->
      show_minus && accounting_parens(key)
    StyleCurrency(sign: CurStandard, ..)
    | StyleDecimal
    | StylePercent
    | StyleUnit(..) -> False
  }
  let sign_parts = case accounting {
    True -> []
    False ->
      case show_minus, show_plus {
        True, _ -> [#(PMinusSign, "-")]
        _, True -> [#(PPlusSign, "+")]
        False, False -> []
      }
  }
  case opts.style {
    StylePercent -> list.flatten([sign_parts, core, [#(PPercentSign, "%")]])
    StyleCurrency(currency: code, display: cur_display, ..) -> {
      let #(text, spaced) = currency_text(key, code, cur_display)
      let with_cur = case cur_display {
        CurName -> list.append(core, [#(PLiteral, " "), #(PCurrency, text)])
        CurCode | CurSymbol | CurNarrowSymbol ->
          case currency_suffixed(key) {
            True ->
              list.append(core, [
                #(PLiteral, "\u{00A0}"),
                #(PCurrency, text),
              ])
            False ->
              case spaced {
                True ->
                  list.flatten([
                    [#(PCurrency, text), #(PLiteral, " ")],
                    core,
                  ])
                False -> [#(PCurrency, text), ..core]
              }
          }
      }
      case accounting {
        True -> list.flatten([[#(PLiteral, "(")], with_cur, [#(PLiteral, ")")]])
        False -> list.append(sign_parts, with_cur)
      }
    }
    StyleUnit(unit: u, display: u_display) -> {
      let #(u_pre, u_suf) = unit_affixes(key, u, u_display, is_one_parts(core))
      // The sign sits between a unit prefix and the number ("時速 -987 …").
      list.flatten([u_pre, sign_parts, core, u_suf])
    }
    StyleDecimal -> list.append(sign_parts, core)
  }
}

/// Unit pattern as prefix/suffix part lists around the (signed) number.
fn unit_affixes(
  key: LocaleKey,
  unit: String,
  display: UnitDisplay,
  one: Bool,
) -> #(List(Part), List(Part)) {
  let lang = loc_lang(key)
  let hant = key.key == "zh-TW" || key.key == "zh-Hant"
  case unit, lang {
    // CLDR kilometer-per-hour patterns for the locales we carry data for.
    "kilometer-per-hour", "de" ->
      case display {
        UnitLong -> #([], [#(PLiteral, " "), #(PUnit, "Kilometer pro Stunde")])
        UnitShort | UnitNarrow -> #([], [#(PLiteral, " "), #(PUnit, "km/h")])
      }
    "kilometer-per-hour", "ja" ->
      case display {
        UnitLong -> #([#(PUnit, "時速"), #(PLiteral, " ")], [
          #(PLiteral, " "),
          #(PUnit, "キロメートル"),
        ])
        UnitNarrow -> #([], [#(PUnit, "km/h")])
        UnitShort -> #([], [#(PLiteral, " "), #(PUnit, "km/h")])
      }
    "kilometer-per-hour", "ko" ->
      case display {
        UnitLong -> #([#(PUnit, "시속"), #(PLiteral, " ")], [
          #(PUnit, "킬로미터"),
        ])
        UnitShort | UnitNarrow -> #([], [#(PUnit, "km/h")])
      }
    "kilometer-per-hour", "zh" if hant ->
      case display {
        UnitLong -> #([#(PUnit, "每小時"), #(PLiteral, " ")], [
          #(PLiteral, " "),
          #(PUnit, "公里"),
        ])
        UnitNarrow -> #([], [#(PUnit, "公里/小時")])
        UnitShort -> #([], [#(PLiteral, " "), #(PUnit, "公里/小時")])
      }
    // en/root fallback.
    "percent", _ ->
      case display {
        UnitLong -> #([], [#(PLiteral, " "), #(PUnit, "percent")])
        UnitShort | UnitNarrow -> #([], [#(PUnit, "%")])
      }
    _, _ ->
      case display {
        UnitLong -> #([], [
          #(PLiteral, " "),
          #(PUnit, unit_name_long(unit, one)),
        ])
        UnitNarrow -> #([], [#(PUnit, unit_name(unit, narrow: True))])
        UnitShort -> #([], [
          #(PLiteral, " "),
          #(PUnit, unit_name(unit, narrow: False)),
        ])
      }
  }
}

/// Whether a part carries the value's decimal digits (as opposed to affixes,
/// separators or date/list decoration).
fn is_digit_part(t: PartType) -> Bool {
  case part_class(t) {
    NumberDigit -> True
    NumberExponentDigit
    | NumberCore
    | NumberAffix
    | DateNumeric
    | DateText
    | OtherPart -> False
  }
}

/// Plural-relevant: formatted value is exactly "1" (i = 1, v = 0).
fn is_one_parts(parts: List(Part)) -> Bool {
  let ints =
    parts
    |> list.filter_map(fn(p: Part) {
      case p.0 {
        PInteger -> Ok(p.1)
        _ -> Error(Nil)
      }
    })
    |> string.join("")
  let has_frac = list.any(parts, fn(p: Part) { p.0 == PFraction })
  ints == "1" && !has_frac
}

fn is_zero_parts(parts: List(Part)) -> Bool {
  let has_digits = list.any(parts, fn(p: Part) { is_digit_part(p.0) })
  has_digits
  && list.all(parts, fn(p: Part) {
    case is_digit_part(p.0) {
      True -> string.to_graphemes(p.1) |> list.all(fn(c) { c == "0" })
      False -> True
    }
  })
}

/// Currency display text. Mostly the en symbols; a handful of locales use a
/// disambiguated USD symbol ("US$").
fn currency_text(
  key: LocaleKey,
  code: String,
  display: CurrencyDisplay,
) -> #(String, Bool) {
  let usd_prefixed = case key.key {
    "ko" | "ko-KR" | "zh-TW" | "zh-Hant" -> True
    _ -> False
  }
  case display {
    CurSymbol | CurNarrowSymbol -> {
      let narrow = display == CurNarrowSymbol
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
    CurName -> #(currency_name(code), False)
    CurCode -> #(code, True)
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

/// Minor-unit count for a currency (ISO 4217). Used for fraction defaults.
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

/// en-US long unit names: "{n} kilometers per hour".
fn unit_name_long(unit: String, one: Bool) -> String {
  let singular = fn(u: String) -> String {
    case u {
      "celsius" -> "degree Celsius"
      "fahrenheit" -> "degree Fahrenheit"
      "fluid-ounce" -> "fluid ounce"
      "foot" -> "foot"
      "inch" -> "inch"
      "mile-scandinavian" -> "mile-scandinavian"
      "percent" -> "percent"
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

/// en-US short/narrow unit display names (CLDR subset; falls back to the
/// identifier). The long width is handled by `unit_name_long`.
fn unit_name(unit: String, narrow narrow: Bool) -> String {
  let simple = fn(u: String) -> String {
    case u, narrow {
      "acre", True -> "ac"
      "acre", False -> "ac"
      "bit", _ -> "bit"
      "byte", _ -> "byte"
      "celsius", True -> "°C"
      "celsius", False -> "°C"
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
      "gram", True -> "g"
      "gram", False -> "g"
      "hectare", _ -> "ha"
      "hour", True -> "h"
      "hour", False -> "hr"
      "inch", True -> "″"
      "inch", False -> "in"
      "kilobit", _ -> "kb"
      "kilobyte", _ -> "kB"
      "kilogram", True -> "kg"
      "kilogram", False -> "kg"
      "kilometer", _ -> "km"
      "liter", True -> "L"
      "liter", False -> "L"
      "megabit", _ -> "Mb"
      "megabyte", _ -> "MB"
      "meter", _ -> "m"
      "microsecond", _ -> "μs"
      "mile", _ -> "mi"
      "mile-scandinavian", _ -> "smi"
      "milliliter", _ -> "mL"
      "millimeter", _ -> "mm"
      "millisecond", True -> "ms"
      "millisecond", False -> "ms"
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
  case string.split_once(unit, "-per-") {
    Ok(#(num, den)) -> {
      // CLDR per-unit forms differ from the standalone short names:
      // "{0}/h" (not "/hr"), "{0}/s" (not "/sec").
      let den_text = case den {
        "hour" -> "h"
        "second" -> "s"
        _ -> simple(den)
      }
      simple(num) <> "/" <> den_text
    }
    Error(Nil) -> simple(unit)
  }
}

/// The ECMA-402 sanctioned simple unit identifiers (§6.5.2, sorted).
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

/// IsWellFormedUnitIdentifier (§6.5.1): sanctioned, or sanctioned-per-sanctioned.
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

// ============================================================================
// Exact decimal core — digits are kept as strings, rounding is performed on
// the shortest decimal representation of the float (matching engines).
// ============================================================================

/// value = 0.digits × 10^exp. digits == "" represents zero.
pub type Dec {
  Dec(digits: String, exp: Int)
}

/// Parse the shortest JS decimal string of a non-negative float.
fn decompose(x: Float) -> Dec {
  parse_decimal(value.js_format_number(x))
}

fn parse_decimal(s: String) -> Dec {
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
  let exp = string.length(int_part) + e
  normalize(Dec(digits:, exp:))
}

fn parse_exp(s: String) -> Int {
  case string.pop_grapheme(s) {
    Ok(#("+", rest)) -> int.parse(rest) |> result.unwrap(0)
    _ -> int.parse(s) |> result.unwrap(0)
  }
}

/// Strip leading zeros (adjusting exp) and trailing zeros.
fn normalize(dec: Dec) -> Dec {
  let #(digits, exp) = strip_leading(dec.digits, dec.exp)
  let digits = strip_trailing(digits)
  case digits {
    "" -> Dec(digits: "", exp: 0)
    _ -> Dec(digits:, exp:)
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

/// Round `dec` keeping `keep` leading digits (rest become remainder).
fn round_dec(dec: Dec, keep: Int, mode: RoundingMode, negative: Bool) -> Dec {
  let n_digits = string.length(dec.digits)
  case dec.digits == "" || keep >= n_digits {
    True -> dec
    False -> {
      let #(kept, rem) = case keep <= 0 {
        True -> #("", dec.digits)
        False -> #(
          string.slice(dec.digits, 0, keep),
          string.slice(dec.digits, keep, n_digits - keep),
        )
      }
      let n = parse_int_or_zero(kept)
      let lead_zeros = case keep < 0 {
        True -> -keep
        False -> 0
      }
      let cmp = half_cmp(rem, lead_zeros)
      let rem_nonzero = string.to_graphemes(rem) |> list.any(fn(c) { c != "0" })
      let up = case rem_nonzero {
        False -> False
        True -> round_up_cmp(mode, negative, cmp, odd: n % 2 == 1)
      }
      let n2 = case up {
        True -> n + 1
        False -> n
      }
      rebuild_rounded(dec, keep, n2)
    }
  }
}

/// Whether the truncated result must be bumped away from zero. `cmp` is the
/// remainder vs half (-1 below, 0 tie, 1 above) and the remainder is known to
/// be nonzero when this is called; `odd` is the parity of the truncated
/// result, which only halfEven consults.
fn round_up_cmp(
  mode: RoundingMode,
  negative: Bool,
  cmp: Int,
  odd odd: Bool,
) -> Bool {
  case mode {
    RoundCeil -> !negative
    RoundFloor -> negative
    RoundExpand -> True
    RoundTrunc -> False
    RoundHalfCeil ->
      case cmp {
        1 -> True
        0 -> !negative
        _ -> False
      }
    RoundHalfFloor ->
      case cmp {
        1 -> True
        0 -> negative
        _ -> False
      }
    RoundHalfTrunc -> cmp == 1
    RoundHalfExpand -> cmp >= 0
    RoundHalfEven ->
      case cmp {
        1 -> True
        0 -> odd
        _ -> False
      }
  }
}

fn half_cmp(rem: String, lead_zeros: Int) -> Int {
  let rem_nonzero = string.to_graphemes(rem) |> list.any(fn(c) { c != "0" })
  case rem_nonzero {
    False -> -1
    True ->
      case lead_zeros > 0 {
        True -> -1
        False ->
          case string.pop_grapheme(rem) {
            Ok(#(first, rest)) -> {
              let d = parse_int_or_zero(first)
              case d < 5 {
                True -> -1
                False ->
                  case d > 5 {
                    True -> 1
                    False ->
                      case
                        string.to_graphemes(rest)
                        |> list.any(fn(c) { c != "0" })
                      {
                        True -> 1
                        False -> 0
                      }
                  }
              }
            }
            Error(Nil) -> -1
          }
      }
  }
}

/// Rebuild a Dec from the rounded integer of the kept digits.
fn rebuild_rounded(dec: Dec, keep: Int, n2: Int) -> Dec {
  case n2 == 0 {
    True -> Dec(digits: "", exp: 0)
    False -> {
      let s = int.to_string(n2)
      let kept_len = int.max(keep, 0)
      // Last kept digit sits at exponent dec.exp - kept_len; overflow
      // (99 → 100) grows the digit count and bumps the exponent.
      let exp = dec.exp - kept_len + string.length(s)
      normalize(Dec(digits: s, exp:))
    }
  }
}

/// Round at fraction position f (value' = N × 10^-f) honoring the rounding
/// increment.
fn round_fraction(
  dec: Dec,
  f: Int,
  inc: Int,
  mode: RoundingMode,
  negative: Bool,
) -> Dec {
  let keep = dec.exp + f
  case inc {
    1 -> round_dec(dec, keep, mode, negative)
    _ -> {
      let n_digits = string.length(dec.digits)
      let #(n, rem_nonzero) = case keep >= n_digits {
        True -> #(
          parse_int_or_zero(dec.digits) * pow10_int(keep - n_digits),
          False,
        )
        False ->
          case keep <= 0 {
            True -> #(0, dec.digits != "")
            False -> #(
              parse_int_or_zero(string.slice(dec.digits, 0, keep)),
              string.slice(dec.digits, keep, n_digits - keep)
                |> string.to_graphemes
                |> list.any(fn(c) { c != "0" }),
            )
          }
      }
      let r = n % inc
      let n2 = case r == 0 && !rem_nonzero {
        True -> n
        False -> {
          // Position within the increment is r + frac where 0 <= frac < 1
          // comes from the leftover digits. Compare 2(r + frac) with inc.
          let doubled = 2 * r
          let cmp = case doubled > inc {
            True -> 1
            False ->
              case doubled == inc {
                True ->
                  case rem_nonzero {
                    True -> 1
                    False -> 0
                  }
                False ->
                  // doubled < inc: 2r + 2*frac vs inc — only the boundary
                  // 2r + 1 == inc can flip on the fractional part.
                  case doubled + 1 == inc {
                    True ->
                      // frac vs 0.5 decides.
                      half_cmp(rem_digits_of(dec, keep), 0)
                    False ->
                      case doubled + 2 <= inc {
                        True -> -1
                        False ->
                          case rem_nonzero {
                            True -> 1
                            False -> -1
                          }
                      }
                  }
              }
          }
          let up = round_up_cmp(mode, negative, cmp, odd: { n / inc } % 2 == 1)
          case up {
            True -> n - r + inc
            False -> n - r
          }
        }
      }
      case n2 == 0 {
        True -> Dec(digits: "", exp: 0)
        False -> {
          let s = int.to_string(n2)
          normalize(Dec(digits: s, exp: string.length(s) - f))
        }
      }
    }
  }
}

/// Digits beyond the keep position (used for increment tie-breaking).
fn rem_digits_of(dec: Dec, keep: Int) -> String {
  let n = string.length(dec.digits)
  case keep >= n {
    True -> ""
    False ->
      case keep <= 0 {
        True -> dec.digits
        False -> string.slice(dec.digits, keep, n - keep)
      }
  }
}

fn parse_int_or_zero(s: String) -> Int {
  case int.parse(s) {
    Ok(v) -> v
    Error(Nil) -> 0
  }
}

fn pow10_int(e: Int) -> Int {
  case e <= 0 {
    True -> 1
    False -> 10 * pow10_int(e - 1)
  }
}

/// Render a rounded Dec to #(int_str, frac_str) padding the fraction to
/// `frac_len` digits.
fn render_dec(dec: Dec, frac_len: Int) -> #(String, String) {
  let n = string.length(dec.digits)
  let #(int_str, frac_str) = case dec.digits {
    "" -> #("0", "")
    _ ->
      case dec.exp <= 0 {
        True -> #("0", string.repeat("0", -dec.exp) <> dec.digits)
        False ->
          case n <= dec.exp {
            True -> #(dec.digits <> string.repeat("0", dec.exp - n), "")
            False -> #(
              string.slice(dec.digits, 0, dec.exp),
              string.slice(dec.digits, dec.exp, n - dec.exp),
            )
          }
      }
  }
  let flen = string.length(frac_str)
  let frac_str = case flen < frac_len {
    True -> frac_str <> string.repeat("0", frac_len - flen)
    False -> frac_str
  }
  #(int_str, frac_str)
}

/// Round + render the absolute value into integer/group/decimal/fraction parts.
fn format_digits(opts: NumOpts, dec: Dec, negative: Bool) -> List(Part) {
  let mode = opts.rounding_mode
  let has_sig = opts.min_sig != None && opts.max_sig != None
  let has_frac = opts.min_frac != None && opts.max_frac != None
  let use_sig = case has_sig, has_frac {
    True, False -> True
    False, _ -> False
    True, True ->
      case opts.rounding_priority {
        PriorityAuto -> True
        PriorityMorePrecision | PriorityLessPrecision -> {
          let max_sig = option.unwrap(opts.max_sig, 21)
          let max_frac = option.unwrap(opts.max_frac, 3)
          // Rounding magnitudes: lower = more precise.
          let m_s = case dec.digits {
            "" -> 0 - max_sig
            _ -> dec.exp - max_sig
          }
          let m_f = 0 - max_frac
          case opts.rounding_priority == PriorityMorePrecision {
            True -> m_s <= m_f
            False -> m_s >= m_f
          }
        }
      }
  }
  let #(int_str, frac_str) = case use_sig {
    True -> {
      let min_sig = option.unwrap(opts.min_sig, 1)
      let max_sig = option.unwrap(opts.max_sig, 21)
      case dec.digits {
        // Zero: "0" plus min_sig-1 fraction zeros (ToRawPrecision step 5).
        "" -> #("0", string.repeat("0", min_sig - 1))
        _ -> {
          let rounded = round_dec(dec, max_sig, mode, negative)
          let frac_len = int.max(0, string.length(rounded.digits) - rounded.exp)
          let #(i, f) = render_dec(rounded, frac_len)
          // Pad with zeros until min_sig significant digits.
          let sig = count_sig(i, f)
          let f = case sig < min_sig {
            True -> f <> string.repeat("0", min_sig - sig)
            False -> f
          }
          #(i, f)
        }
      }
    }
    False -> {
      let min_frac = option.unwrap(opts.min_frac, 0)
      let max_frac = option.unwrap(opts.max_frac, 3)
      let rounded =
        round_fraction(dec, max_frac, opts.rounding_increment, mode, negative)
      let #(i, f) = render_dec(rounded, max_frac)
      // Strip trailing zeros beyond min_frac.
      #(i, strip_frac_to_min(f, min_frac))
    }
  }
  // stripIfInteger: drop fraction when it is all zeros.
  let frac_str = case opts.trailing_zero_display {
    TzdStripIfInteger ->
      case string.to_graphemes(frac_str) |> list.all(fn(c) { c == "0" }) {
        True -> ""
        False -> frac_str
      }
    TzdAuto -> frac_str
  }
  // Pad to minimumIntegerDigits.
  let key = opts.locale
  let int_str = string.pad_start(int_str, opts.min_int, "0")
  let int_parts = group_integer(opts, int_str)
  case frac_str {
    "" -> int_parts
    _ ->
      list.append(int_parts, [
        #(PDecimal, decimal_sep(key)),
        #(PFraction, frac_str),
      ])
  }
}

fn count_sig(int_str: String, frac_str: String) -> Int {
  let all = int_str <> frac_str
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

/// Insert group separators (en: every 3 digits, ","; de: "."; en-IN: 3-then-2).
fn group_integer(opts: NumOpts, int_str: String) -> List(Part) {
  let key = opts.locale
  let n = string.length(int_str)
  let grouped = case opts.use_grouping {
    GroupingNever -> False
    GroupingAlways -> n > 3
    GroupingMin2 -> n > 4
    // auto — groups at 4+ digits
    GroupingAuto -> n > 3
  }
  case grouped {
    False -> [#(PInteger, int_str)]
    True -> {
      let groups = case indian_grouping(key) {
        True -> split_groups_indian(int_str)
        False -> split_groups(int_str)
      }
      let sep = group_sep(key)
      groups
      |> list.map(fn(g) { [#(PGroup, sep), #(PInteger, g)] })
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

/// Indian grouping: rightmost group of 3, then groups of 2 ("1,00,000").
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

// ============================================================================
// Plural rules — CLDR en
// ============================================================================

/// Select the plural category for English. `int_digits`/`frac_digits` are the
/// formatted digit strings (so digit options affect the operands, per spec).
pub fn plural_select_en(
  type_: PluralType,
  int_digits: String,
  frac_digits: String,
  negative: Bool,
) -> String {
  let _ = negative
  case type_ {
    Ordinal -> {
      let n = int.parse(int_digits) |> option.from_result |> option.unwrap(0)
      let n = int.absolute_value(n)
      let r10 = n % 10
      let r100 = n % 100
      case r10, r100 {
        1, _ if r100 != 11 -> "one"
        2, _ if r100 != 12 -> "two"
        3, _ if r100 != 13 -> "few"
        _, _ -> "other"
      }
    }
    // cardinal: one iff i = 1 and v = 0
    Cardinal ->
      case int_digits == "1" && frac_digits == "" {
        True -> "one"
        False -> "other"
      }
  }
}

pub fn plural_categories_en(type_: PluralType) -> List(String) {
  case type_ {
    Ordinal -> ["few", "one", "other", "two"]
    Cardinal -> ["one", "other"]
  }
}

// ============================================================================
// List formatting — CLDR en
// ============================================================================

/// CreatePartsFromList (§13.5.2) with en patterns.
pub fn list_format_parts(
  type_: ListFormatType,
  style: ListFormatStyle,
  items: List(String),
) -> List(Part) {
  case items {
    [] -> []
    [only] -> [#(PElement, only)]
    [a, b] -> {
      let sep = two_separator(type_, style)
      [#(PElement, a), #(PLiteral, sep), #(PElement, b)]
    }
    [first, ..rest] -> {
      let mid = case type_, style {
        UnitList, LNarrow -> " "
        UnitList, LLong | UnitList, LShort -> ", "
        Conjunction, _ | Disjunction, _ -> ", "
      }
      let last_sep = end_separator(type_, style)
      build_list_parts(rest, [#(PElement, first)], mid, last_sep)
    }
  }
}

fn two_separator(type_: ListFormatType, style: ListFormatStyle) -> String {
  case type_, style {
    Conjunction, LNarrow -> ", "
    Conjunction, LShort -> " & "
    Conjunction, LLong -> " and "
    Disjunction, _ -> " or "
    UnitList, LNarrow -> " "
    UnitList, LLong | UnitList, LShort -> ", "
  }
}

fn end_separator(type_: ListFormatType, style: ListFormatStyle) -> String {
  case type_, style {
    Conjunction, LLong -> ", and "
    Conjunction, LShort -> ", & "
    Conjunction, LNarrow -> ", "
    Disjunction, _ -> ", or "
    UnitList, LNarrow -> " "
    UnitList, LLong | UnitList, LShort -> ", "
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
    [last] -> list.reverse([#(PElement, last), #(PLiteral, last_sep), ..acc])
    [next, ..rest] ->
      build_list_parts(
        rest,
        [#(PElement, next), #(PLiteral, mid), ..acc],
        mid,
        last_sep,
      )
  }
}

// ============================================================================
// Relative time formatting — CLDR en
// ============================================================================

/// en relative-time formatting. `value_parts` is the pre-formatted absolute
/// value (so digit/numbering options apply); `value` selects plural/special.
pub fn rtf_parts_en(
  style: RtfStyle,
  numeric: RtfNumeric,
  value: Float,
  unit: String,
  value_parts: List(Part),
) -> List(UnitPart) {
  let is_auto = case numeric {
    RtfAuto -> True
    RtfAlways -> False
  }
  // The unit property is only attached to numeric parts (never literals).
  let js3 = fn(p: Part, unit) { UnitPart(p.0, p.1, unit) }
  let literal3 = fn(text) { js3(#(PLiteral, text), "") }
  case is_auto, rtf_auto_name(unit, value +. 0.0) {
    True, Some(name) -> [literal3(name)]
    _, _ -> {
      let plural = case float.absolute_value(value) {
        1.0 -> "one"
        _ -> "other"
      }
      let unit_text = rtf_unit_en(style, unit, plural)
      let past = operators.is_negative_float(value)
      // Numeric parts carry the unit for formatToParts.
      let tagged =
        list.map(value_parts, fn(p: Part) {
          case p.0 {
            PLiteral -> literal3(p.1)
            _ -> js3(p, unit)
          }
        })
      case past {
        True -> list.flatten([tagged, [literal3(" " <> unit_text <> " ago")]])
        False ->
          list.flatten([
            [literal3("in ")],
            tagged,
            [literal3(" " <> unit_text)],
          ])
      }
    }
  }
}

fn rtf_auto_name(unit: String, value: Float) -> Option(String) {
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

fn rtf_unit_en(style: RtfStyle, unit: String, plural: String) -> String {
  let single = plural == "one"
  case style {
    RtfLong ->
      case single {
        True -> unit
        False -> unit <> "s"
      }
    RtfShort | RtfNarrow ->
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

// ============================================================================
// Date/time formatting — proleptic Gregorian, en-US patterns
// ============================================================================

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

/// Convert epoch milliseconds (+ offset minutes) to civil fields.
pub fn fields_from_epoch_ms(ms: Float, offset_minutes: Int) -> DateFields {
  let total_ms = float_to_int_trunc(ms) + offset_minutes * 60_000
  let #(days, ms_in_day) = divmod(total_ms, 86_400_000)
  let millisecond = ms_in_day % 1000
  let total_seconds = ms_in_day / 1000
  let second = total_seconds % 60
  let minute = { total_seconds / 60 } % 60
  let hour = total_seconds / 3600
  let week_day = { { days % 7 } + 11 } % 7
  // days → civil date (proleptic Gregorian), epoch = 1970-01-01.
  let #(year, month, day) = civil_from_days(days)
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

fn float_to_int_trunc(x: Float) -> Int {
  float.truncate(x)
}

fn divmod(a: Int, b: Int) -> #(Int, Int) {
  let q = case a < 0 && a % b != 0 {
    True -> a / b - 1
    False -> a / b
  }
  #(q, a - q * b)
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
    WLong -> long
    WShort ->
      case m {
        9 -> "Sep"
        _ -> string.slice(long, 0, 3)
      }
    WNarrow -> string.slice(long, 0, 1)
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
    WLong -> long
    WShort -> string.slice(long, 0, 3)
    WNarrow -> string.slice(long, 0, 1)
  }
}

pub fn era_name(year: Int, width: NameWidth) -> String {
  let bc = year <= 0
  case width, bc {
    WLong, True -> "Before Christ"
    WLong, False -> "Anno Domini"
    WNarrow, True -> "B"
    WNarrow, False -> "A"
    WShort, True -> "BC"
    WShort, False -> "AD"
  }
}

/// en flexible day periods (CLDR): used for the dayPeriod option.
pub fn day_period_name(hour: Int, minute: Int, width: NameWidth) -> String {
  let mins = hour * 60 + minute
  let noon = case width {
    WNarrow -> "n"
    WLong | WShort -> "noon"
  }
  case mins == 720 {
    True -> noon
    False ->
      case hour {
        h if h < 6 -> "at night"
        h if h < 12 -> "in the morning"
        h if h < 18 -> "in the afternoon"
        h if h < 21 -> "in the evening"
        _ -> "at night"
      }
  }
}

pub fn pad2(n: Int) -> String {
  string.pad_start(int.to_string(n), 2, "0")
}

// ============================================================================
// Segmentation (root rules, approximate)
// ============================================================================

/// Returns (segment, startIndex, isWordLike) triples covering the string.
pub fn segment_string(
  s: String,
  granularity: Granularity,
) -> List(#(String, Int, Bool)) {
  case granularity {
    GWord -> segment_words(s)
    GSentence -> segment_sentences(s)
    GGrapheme -> segment_graphemes(s)
  }
}

fn segment_graphemes(s: String) -> List(#(String, Int, Bool)) {
  string.to_graphemes(s)
  |> list.fold(#([], 0), fn(acc, g) {
    let #(parts, idx) = acc
    #([#(g, idx, False), ..parts], idx + utf16_len(g))
  })
  |> fn(acc) { list.reverse(acc.0) }
}

pub fn utf16_len(s: String) -> Int {
  string.to_utf_codepoints(s)
  |> list.fold(0, fn(n, cp) {
    case string.utf_codepoint_to_int(cp) > 0xffff {
      True -> n + 2
      False -> n + 1
    }
  })
}

fn is_word_char(g: String) -> Bool {
  case string.to_utf_codepoints(g) {
    [cp, ..] -> {
      let c = string.utf_codepoint_to_int(cp)
      { c >= 0x30 && c <= 0x39 }
      || { c >= 0x41 && c <= 0x5a }
      || { c >= 0x61 && c <= 0x7a }
      || c == 0x27
      || c > 0x7f
    }
    [] -> False
  }
}

fn segment_words(s: String) -> List(#(String, Int, Bool)) {
  let graphemes = string.to_graphemes(s)
  segment_words_loop(graphemes, 0, [], "", 0, None)
}

fn segment_words_loop(
  rest: List(String),
  idx: Int,
  acc: List(#(String, Int, Bool)),
  current: String,
  current_start: Int,
  current_kind: Option(Bool),
) -> List(#(String, Int, Bool)) {
  case rest {
    [] ->
      case current {
        "" -> list.reverse(acc)
        _ ->
          list.reverse([
            #(current, current_start, option.unwrap(current_kind, False)),
            ..acc
          ])
      }
    [g, ..gs] -> {
      let kind = is_word_char(g)
      case current_kind {
        Some(k) if k == kind ->
          segment_words_loop(
            gs,
            idx + utf16_len(g),
            acc,
            current <> g,
            current_start,
            current_kind,
          )
        Some(k) ->
          segment_words_loop(
            gs,
            idx + utf16_len(g),
            [#(current, current_start, k), ..acc],
            g,
            idx,
            Some(kind),
          )
        None ->
          segment_words_loop(gs, idx + utf16_len(g), acc, g, idx, Some(kind))
      }
    }
  }
}

fn segment_sentences(s: String) -> List(#(String, Int, Bool)) {
  case s {
    "" -> []
    _ -> segment_sentences_loop(string.to_graphemes(s), 0, [], "", 0, False)
  }
}

fn segment_sentences_loop(
  rest: List(String),
  idx: Int,
  acc: List(#(String, Int, Bool)),
  current: String,
  current_start: Int,
  after_terminator: Bool,
) -> List(#(String, Int, Bool)) {
  case rest {
    [] ->
      case current {
        "" -> list.reverse(acc)
        _ -> list.reverse([#(current, current_start, False), ..acc])
      }
    [g, ..gs] -> {
      let next_idx = idx + utf16_len(g)
      let is_term = g == "." || g == "!" || g == "?"
      case after_terminator && !is_term && g != " " && g != "\n" {
        True ->
          // Start a new sentence at this grapheme.
          segment_sentences_loop(
            gs,
            next_idx,
            [#(current, current_start, False), ..acc],
            g,
            idx,
            False,
          )
        False ->
          segment_sentences_loop(
            gs,
            next_idx,
            acc,
            current <> g,
            current_start,
            after_terminator || is_term,
          )
      }
    }
  }
}

// ============================================================================
// Display names — en subset
// ============================================================================

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
