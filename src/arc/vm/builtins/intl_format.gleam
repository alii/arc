//// Pure formatting engines for the Intl builtins (root/English locale data).
////
//// No heap/state dependencies: every function maps plain Gleam values to
//// lists of `#(type, value)` parts, mirroring the partitioning operations of
//// ECMA-402 (PartitionNumberPattern, PartitionDateTimePattern, …). The
//// builtins layer turns parts into strings or {type, value} part objects.

import arc/vm/value
import gleam/bool
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// A formatted part: #(type, value), e.g. #("integer", "1"), #("group", ",").
pub type Part =
  #(String, String)

pub fn parts_to_string(parts: List(Part)) -> String {
  parts |> list.map(fn(p) { p.1 }) |> string.join("")
}

/// Combine two formatted endpoints into range parts (#(type, value, source)).
/// Implements the ICU collapse heuristic: identical affixes are emitted once
/// when they amount to more than one code point ("+$2.90–3.10"), and the
/// range separator is padded with spaces when an uncollapsed affix sits next
/// to it ("$3 – $5" vs "987–988").
pub fn format_range_combine(
  locale: String,
  x_parts: List(Part),
  y_parts: List(Part),
) -> List(#(String, String, String)) {
  let key = loc_key(locale)
  let start3 = fn(p: Part) { #(p.0, p.1, "startRange") }
  let end3 = fn(p: Part) { #(p.0, p.1, "endRange") }
  let shared3 = fn(p: Part) { #(p.0, p.1, "shared") }
  case parts_to_string(x_parts) == parts_to_string(y_parts) {
    True -> [
      #("approximatelySign", "~", "shared"),
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
            [#("literal", range_sep(key, False), "shared")],
            list.map(y_core, end3),
            list.map(x_suf, shared3),
          ])
        False -> {
          let spaced = x_suf != [] || y_pre != []
          list.flatten([
            list.map(x_parts, start3),
            [#("literal", range_sep(key, spaced), "shared")],
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
    case p.0 {
      "integer"
      | "group"
      | "decimal"
      | "fraction"
      | "nan"
      | "infinity"
      | "exponentSeparator"
      | "exponentMinusSign"
      | "exponentInteger"
      | "compact" -> True
      _ -> False
    }
  }
  let #(pre, rest) = list.split_while(parts, fn(p) { !is_core(p) })
  let #(rev_suf, rev_core) =
    list.split_while(list.reverse(rest), fn(p) { !is_core(p) })
  #(pre, list.reverse(rev_core), list.reverse(rev_suf))
}

/// Locale range separator. `spaced` requests the space-padded variant used
/// when an affix is adjacent to the separator.
fn range_sep(key: String, spaced: Bool) -> String {
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
    locale: String,
    style: String,
    currency: Option(String),
    currency_display: String,
    currency_sign: String,
    unit: Option(String),
    unit_display: String,
    min_int: Int,
    min_frac: Option(Int),
    max_frac: Option(Int),
    min_sig: Option(Int),
    max_sig: Option(Int),
    use_grouping: String,
    notation: String,
    compact_display: String,
    sign_display: String,
    rounding_increment: Int,
    rounding_mode: String,
    rounding_priority: String,
    trailing_zero_display: String,
  )
}

pub fn default_num_opts() -> NumOpts {
  NumOpts(
    locale: "en",
    style: "decimal",
    currency: None,
    currency_display: "symbol",
    currency_sign: "standard",
    unit: None,
    unit_display: "short",
    min_int: 1,
    min_frac: Some(0),
    max_frac: Some(3),
    min_sig: None,
    max_sig: None,
    use_grouping: "auto",
    notation: "standard",
    compact_display: "short",
    sign_display: "auto",
    rounding_increment: 1,
    rounding_mode: "halfExpand",
    rounding_priority: "auto",
    trailing_zero_display: "auto",
  )
}

// ============================================================================
// Locale data (CLDR subset). The key is the resolved locale tag with any
// "-u-…" extension stripped, e.g. "de", "zh-TW", "en-IN". Unknown locales
// fall back to the en/root patterns, preserving the historical behavior.
// ============================================================================

/// Strip a "-u-…" (or any singleton) extension from a resolved locale tag.
fn loc_key(tag: String) -> String {
  case string.split_once(tag, "-u-") {
    Ok(#(base, _)) -> base
    Error(Nil) ->
      case string.split_once(tag, "-x-") {
        Ok(#(base, _)) -> base
        Error(Nil) -> tag
      }
  }
}

/// Primary language subtag ("de-AT" → "de").
fn loc_lang(key: String) -> String {
  case string.split_once(key, "-") {
    Ok(#(lang, _)) -> lang
    Error(Nil) -> key
  }
}

fn decimal_sep(key: String) -> String {
  case loc_lang(key) {
    "de" | "pt" | "it" | "nl" -> ","
    _ -> "."
  }
}

fn group_sep(key: String) -> String {
  case key {
    "pt-PT" -> "\u{00A0}"
    _ ->
      case loc_lang(key) {
        "de" | "pt" | "it" | "nl" -> "."
        _ -> ","
      }
  }
}

fn nan_str(key: String) -> String {
  case key {
    "zh-TW" | "zh-Hant" -> "非數值"
    _ -> "NaN"
  }
}

/// Indian digit grouping (last 3, then 2s): 1,00,000.
fn indian_grouping(key: String) -> Bool {
  case key {
    "en-IN" -> True
    _ -> loc_lang(key) == "hi"
  }
}

/// Currency placement: True → symbol suffixed after a NBSP ("987,00 $"),
/// False → symbol prefixed ("$987.00").
fn currency_suffixed(key: String) -> Bool {
  case loc_lang(key) {
    "de" | "pt" -> True
    _ -> False
  }
}

/// Whether currencySign:"accounting" wraps negatives in parentheses.
/// CLDR de accounting pattern equals the standard pattern (minus sign).
fn accounting_parens(key: String) -> Bool {
  loc_lang(key) != "de"
}

@external(erlang, "arc_math_ffi", "is_neg_zero")
fn is_neg_zero(x: Float) -> Bool

/// Format a finite float per the options. `is_nan`/`is_inf` are handled by
/// the caller. Returns the full part list including sign/affixes.
pub fn format_number_parts(opts: NumOpts, x: Float) -> List(Part) {
  let negative = x <. 0.0 || is_neg_zero(x)
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
  let opts = case opts.notation == "compact" && opts.use_grouping == "auto" {
    True -> NumOpts(..opts, use_grouping: "min2")
    False -> opts
  }
  // Percent scaling happens before rounding (ECMA-402 §15.5.1).
  let dec = case opts.style {
    "percent" -> Dec(..dec, exp: dec.exp + 2)
    _ -> dec
  }
  let dec = normalize(dec)
  let key = loc_key(opts.locale)
  let #(mantissa, exponent, compact_one, compact_other) = case opts.notation {
    "scientific" ->
      case dec.digits {
        "" -> #(dec, 0, [], [])
        _ -> #(Dec(..dec, exp: 1), dec.exp - 1, [], [])
      }
    "engineering" ->
      case dec.digits {
        "" -> #(dec, 0, [], [])
        _ -> {
          let e = 3 * floor_div(dec.exp - 1, 3)
          #(Dec(..dec, exp: dec.exp - e), e, [], [])
        }
      }
    "compact" ->
      case dec.digits {
        "" -> #(dec, 0, [], [])
        _ -> {
          let #(div_exp, one_p, other_p) =
            compact_entry(key, opts.compact_display, dec.exp - 1)
          #(Dec(..dec, exp: dec.exp - div_exp), 0, one_p, other_p)
        }
      }
    _ -> #(dec, 0, [], [])
  }
  let digit_parts = format_digits(opts, mantissa, negative)
  let digit_parts = case opts.notation {
    "scientific" | "engineering" -> {
      let exp_parts = case exponent < 0 {
        True -> [
          #("exponentSeparator", "E"),
          #("exponentMinusSign", "-"),
          #("exponentInteger", int.to_string(-exponent)),
        ]
        False -> [
          #("exponentSeparator", "E"),
          #("exponentInteger", int.to_string(exponent)),
        ]
      }
      list.append(digit_parts, exp_parts)
    }
    "compact" ->
      case compact_other {
        [] -> digit_parts
        _ ->
          case is_one_parts(digit_parts) {
            True -> list.append(digit_parts, compact_one)
            False -> list.append(digit_parts, compact_other)
          }
      }
    _ -> digit_parts
  }
  wrap_affixes(opts, digit_parts, negative, False)
}

/// Compact notation data (CLDR compact decimal patterns). `e` is the decimal
/// exponent of the value (floor(log10 |x|)). Returns
/// #(divisor_exponent, suffix parts for plural "one", suffix parts otherwise).
fn compact_entry(
  key: String,
  display: String,
  e: Int,
) -> #(Int, List(Part), List(Part)) {
  use <- bool.lazy_guard(key == "en-IN", fn() { in_compact(e, display) })
  case loc_lang(key) {
    "ja" -> cjk_compact(e, "万", "億", "兆", None)
    "ko" -> cjk_compact(e, "만", "억", "조", Some("천"))
    "zh" ->
      case key {
        "zh-TW" | "zh-Hant" -> cjk_compact(e, "萬", "億", "兆", None)
        _ -> en_compact(e, display)
      }
    "de" -> de_compact(e, display)
    _ -> en_compact(e, display)
  }
}

fn en_compact(e: Int, display: String) -> #(Int, List(Part), List(Part)) {
  use <- bool.guard(e < 3, #(0, [], []))
  let k = int.min(4, e / 3)
  let suffix = case k, display == "long" {
    1, False -> [#("compact", "K")]
    2, False -> [#("compact", "M")]
    3, False -> [#("compact", "B")]
    _, False -> [#("compact", "T")]
    1, True -> [#("literal", " "), #("compact", "thousand")]
    2, True -> [#("literal", " "), #("compact", "million")]
    3, True -> [#("literal", " "), #("compact", "billion")]
    _, True -> [#("literal", " "), #("compact", "trillion")]
  }
  #(3 * k, suffix, suffix)
}

/// en-IN compact data: thousand (K), lakh (10^5, L), crore (10^7, Cr).
fn in_compact(e: Int, display: String) -> #(Int, List(Part), List(Part)) {
  let entry = fn(div: Int, short: String, long: String) {
    let suffix = case display == "long" {
      False -> [#("compact", short)]
      True -> [#("literal", " "), #("compact", long)]
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
        Some(s) -> #(3, [#("compact", s)], [#("compact", s)])
        None -> #(0, [], [])
      }
    _ if e >= 4 && e <= 7 -> #(4, [#("compact", m4)], [#("compact", m4)])
    _ if e >= 8 && e <= 11 -> #(8, [#("compact", m8)], [#("compact", m8)])
    _ if e >= 12 -> #(12, [#("compact", m12)], [#("compact", m12)])
    _ -> #(0, [], [])
  }
}

fn de_compact(e: Int, display: String) -> #(Int, List(Part), List(Part)) {
  let short = fn(s: String) { [#("literal", "\u{00A0}"), #("compact", s)] }
  let long = fn(s: String) { [#("literal", " "), #("compact", s)] }
  case display == "long" {
    False ->
      // CLDR de short compact has no abbreviation below one million.
      case e {
        _ if e >= 6 && e <= 8 -> #(6, short("Mio."), short("Mio."))
        _ if e >= 9 && e <= 11 -> #(9, short("Mrd."), short("Mrd."))
        _ if e >= 12 -> #(12, short("Bio."), short("Bio."))
        _ -> #(0, [], [])
      }
    True ->
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
  wrap_affixes(opts, [#("nan", nan_str(loc_key(opts.locale)))], False, True)
}

pub fn format_infinity_parts(opts: NumOpts, negative: Bool) -> List(Part) {
  wrap_affixes(opts, [#("infinity", "∞")], negative, False)
}

/// Add sign, currency/percent/unit affixes around the core digit parts.
fn wrap_affixes(
  opts: NumOpts,
  core: List(Part),
  negative: Bool,
  is_nan: Bool,
) -> List(Part) {
  let key = loc_key(opts.locale)
  let zero = !is_nan && !negative && is_zero_parts(core)
  let neg_zero = negative && is_zero_parts(core)
  let show_minus = case opts.sign_display {
    "never" -> False
    "always" -> negative
    "exceptZero" -> negative && !neg_zero
    "negative" -> negative && !neg_zero
    _ -> negative
  }
  let show_plus = case opts.sign_display, is_nan {
    "always", _ -> !negative
    "exceptZero", False -> !negative && !zero
    _, _ -> False
  }
  // Accounting parentheses replace the minus sign when it would be shown —
  // in locales whose accounting pattern uses parentheses at all (not de).
  let accounting =
    opts.style == "currency"
    && opts.currency_sign == "accounting"
    && show_minus
    && accounting_parens(key)
  let sign_parts = case accounting {
    True -> []
    False ->
      case show_minus, show_plus {
        True, _ -> [#("minusSign", "-")]
        _, True -> [#("plusSign", "+")]
        False, False -> []
      }
  }
  case opts.style {
    "percent" -> list.flatten([sign_parts, core, [#("percentSign", "%")]])
    "currency" -> {
      let code = option.unwrap(opts.currency, "USD")
      let #(text, spaced) = currency_text(key, code, opts.currency_display)
      let with_cur = case opts.currency_display {
        "name" -> list.append(core, [#("literal", " "), #("currency", text)])
        _ ->
          case currency_suffixed(key) {
            True ->
              list.append(core, [
                #("literal", "\u{00A0}"),
                #("currency", text),
              ])
            False ->
              case spaced {
                True ->
                  list.flatten([
                    [#("currency", text), #("literal", " ")],
                    core,
                  ])
                False -> [#("currency", text), ..core]
              }
          }
      }
      case accounting {
        True ->
          list.flatten([[#("literal", "(")], with_cur, [#("literal", ")")]])
        False -> list.append(sign_parts, with_cur)
      }
    }
    "unit" -> {
      let u = option.unwrap(opts.unit, "")
      let #(u_pre, u_suf) =
        unit_affixes(key, u, opts.unit_display, is_one_parts(core))
      // The sign sits between a unit prefix and the number ("時速 -987 …").
      list.flatten([u_pre, sign_parts, core, u_suf])
    }
    _ -> list.append(sign_parts, core)
  }
}

/// Unit pattern as prefix/suffix part lists around the (signed) number.
fn unit_affixes(
  key: String,
  unit: String,
  display: String,
  one: Bool,
) -> #(List(Part), List(Part)) {
  let lang = loc_lang(key)
  let hant = key == "zh-TW" || key == "zh-Hant"
  case unit, lang {
    // CLDR kilometer-per-hour patterns for the locales we carry data for.
    "kilometer-per-hour", "de" ->
      case display {
        "long" -> #([], [#("literal", " "), #("unit", "Kilometer pro Stunde")])
        _ -> #([], [#("literal", " "), #("unit", "km/h")])
      }
    "kilometer-per-hour", "ja" ->
      case display {
        "long" -> #([#("unit", "時速"), #("literal", " ")], [
          #("literal", " "),
          #("unit", "キロメートル"),
        ])
        "narrow" -> #([], [#("unit", "km/h")])
        _ -> #([], [#("literal", " "), #("unit", "km/h")])
      }
    "kilometer-per-hour", "ko" ->
      case display {
        "long" -> #([#("unit", "시속"), #("literal", " ")], [
          #("unit", "킬로미터"),
        ])
        _ -> #([], [#("unit", "km/h")])
      }
    "kilometer-per-hour", "zh" if hant ->
      case display {
        "long" -> #([#("unit", "每小時"), #("literal", " ")], [
          #("literal", " "),
          #("unit", "公里"),
        ])
        "narrow" -> #([], [#("unit", "公里/小時")])
        _ -> #([], [#("literal", " "), #("unit", "公里/小時")])
      }
    // en/root fallback.
    "percent", _ ->
      case display {
        "long" -> #([], [#("literal", " "), #("unit", "percent")])
        _ -> #([], [#("unit", "%")])
      }
    _, _ ->
      case display {
        "long" -> #([], [
          #("literal", " "),
          #("unit", unit_name_long(unit, one)),
        ])
        "narrow" -> #([], [#("unit", unit_name(unit, "narrow"))])
        _ -> #([], [#("literal", " "), #("unit", unit_name(unit, display))])
      }
  }
}

/// Plural-relevant: formatted value is exactly "1" (i = 1, v = 0).
fn is_one_parts(parts: List(Part)) -> Bool {
  let ints =
    parts
    |> list.filter_map(fn(p) {
      case p {
        #("integer", v) -> Ok(v)
        _ -> Error(Nil)
      }
    })
    |> string.join("")
  let has_frac =
    list.any(parts, fn(p) {
      case p {
        #("fraction", _) -> True
        _ -> False
      }
    })
  ints == "1" && !has_frac
}

fn is_zero_parts(parts: List(Part)) -> Bool {
  let has_digits =
    list.any(parts, fn(p) {
      case p {
        #("integer", _) | #("fraction", _) -> True
        _ -> False
      }
    })
  has_digits
  && list.all(parts, fn(p) {
    case p {
      #("integer", v) | #("fraction", v) ->
        string.to_graphemes(v) |> list.all(fn(c) { c == "0" })
      _ -> True
    }
  })
}

/// Currency display text. Mostly the en symbols; a handful of locales use a
/// disambiguated USD symbol ("US$").
fn currency_text(
  key: String,
  code: String,
  display: String,
) -> #(String, Bool) {
  let usd_prefixed = case key {
    "ko" | "ko-KR" | "zh-TW" | "zh-Hant" -> True
    _ -> False
  }
  case display {
    "symbol" | "narrowSymbol" ->
      case code {
        "USD" ->
          case display == "symbol" && usd_prefixed {
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
          case display {
            "narrowSymbol" -> #("$", False)
            _ -> #("CA$", False)
          }
        "AUD" ->
          case display {
            "narrowSymbol" -> #("$", False)
            _ -> #("A$", False)
          }
        _ -> #(code, True)
      }
    "name" -> #(currency_name(code), False)
    // "code"
    _ -> #(code, True)
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

/// en-US unit display names (CLDR subset; falls back to the identifier).
fn unit_name(unit: String, display: String) -> String {
  let simple = fn(u: String) -> String {
    case u, display {
      "acre", "narrow" -> "ac"
      "acre", _ -> "ac"
      "bit", _ -> "bit"
      "byte", _ -> "byte"
      "celsius", "narrow" -> "°C"
      "celsius", _ -> "°C"
      "centimeter", _ -> "cm"
      "day", "narrow" -> "d"
      "day", _ -> "day"
      "degree", "narrow" -> "°"
      "degree", _ -> "deg"
      "fahrenheit", _ -> "°F"
      "fluid-ounce", _ -> "fl oz"
      "foot", "narrow" -> "′"
      "foot", _ -> "ft"
      "gallon", _ -> "gal"
      "gigabit", _ -> "Gb"
      "gigabyte", _ -> "GB"
      "gram", "narrow" -> "g"
      "gram", _ -> "g"
      "hectare", _ -> "ha"
      "hour", "narrow" -> "h"
      "hour", _ -> "hr"
      "inch", "narrow" -> "″"
      "inch", _ -> "in"
      "kilobit", _ -> "kb"
      "kilobyte", _ -> "kB"
      "kilogram", "narrow" -> "kg"
      "kilogram", _ -> "kg"
      "kilometer", _ -> "km"
      "liter", "narrow" -> "L"
      "liter", _ -> "L"
      "megabit", _ -> "Mb"
      "megabyte", _ -> "MB"
      "meter", _ -> "m"
      "microsecond", _ -> "μs"
      "mile", _ -> "mi"
      "mile-scandinavian", _ -> "smi"
      "milliliter", _ -> "mL"
      "millimeter", _ -> "mm"
      "millisecond", "narrow" -> "ms"
      "millisecond", _ -> "ms"
      "minute", "narrow" -> "m"
      "minute", _ -> "min"
      "month", "narrow" -> "m"
      "month", _ -> "mth"
      "nanosecond", _ -> "ns"
      "ounce", _ -> "oz"
      "percent", _ -> "%"
      "petabyte", _ -> "PB"
      "pound", "narrow" -> "#"
      "pound", _ -> "lb"
      "second", "narrow" -> "s"
      "second", _ -> "sec"
      "stone", _ -> "st"
      "terabit", _ -> "Tb"
      "terabyte", _ -> "TB"
      "week", "narrow" -> "w"
      "week", _ -> "wk"
      "yard", _ -> "yd"
      "year", "narrow" -> "y"
      "year", _ -> "yr"
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

fn floor_div(a: Int, b: Int) -> Int {
  let q = a / b
  case a % b < 0 {
    True -> q - 1
    False -> q
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
fn round_dec(dec: Dec, keep: Int, mode: String, negative: Bool) -> Dec {
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
        True ->
          case mode {
            "halfEven" ->
              case cmp {
                1 -> True
                0 -> n % 2 == 1
                _ -> False
              }
            _ -> round_up_cmp(mode, negative, cmp)
          }
      }
      let n2 = case up {
        True -> n + 1
        False -> n
      }
      rebuild_rounded(dec, keep, n2)
    }
  }
}

/// `cmp` is remainder vs half (-1 below, 0 tie, 1 above). Remainder is known
/// to be nonzero when this is called.
fn round_up_cmp(mode: String, negative: Bool, cmp: Int) -> Bool {
  case mode {
    "ceil" -> !negative
    "floor" -> negative
    "expand" -> True
    "trunc" -> False
    "halfCeil" ->
      case cmp {
        1 -> True
        0 -> !negative
        _ -> False
      }
    "halfFloor" ->
      case cmp {
        1 -> True
        0 -> negative
        _ -> False
      }
    "halfTrunc" -> cmp == 1
    // halfExpand (default)
    _ -> cmp >= 0
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
  mode: String,
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
          let up = case mode {
            "halfEven" ->
              case cmp {
                1 -> True
                0 -> { n / inc } % 2 == 1
                _ -> False
              }
            _ -> round_up_cmp(mode, negative, cmp)
          }
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
        "morePrecision" | "lessPrecision" -> {
          let max_sig = option.unwrap(opts.max_sig, 21)
          let max_frac = option.unwrap(opts.max_frac, 3)
          // Rounding magnitudes: lower = more precise.
          let m_s = case dec.digits {
            "" -> 0 - max_sig
            _ -> dec.exp - max_sig
          }
          let m_f = 0 - max_frac
          case opts.rounding_priority {
            "morePrecision" -> m_s <= m_f
            _ -> m_s >= m_f
          }
        }
        _ -> True
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
    "stripIfInteger" ->
      case string.to_graphemes(frac_str) |> list.all(fn(c) { c == "0" }) {
        True -> ""
        False -> frac_str
      }
    _ -> frac_str
  }
  // Pad to minimumIntegerDigits.
  let key = loc_key(opts.locale)
  let int_str = string.pad_start(int_str, opts.min_int, "0")
  let int_parts = group_integer(opts, int_str)
  case frac_str {
    "" -> int_parts
    _ ->
      list.append(int_parts, [
        #("decimal", decimal_sep(key)),
        #("fraction", frac_str),
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
  let key = loc_key(opts.locale)
  let n = string.length(int_str)
  let grouped = case opts.use_grouping {
    "never" -> False
    "always" -> n > 3
    "min2" -> n > 4
    // "auto" — groups at 4+ digits
    _ -> n > 3
  }
  case grouped {
    False -> [#("integer", int_str)]
    True -> {
      let groups = case indian_grouping(key) {
        True -> split_groups_indian(int_str)
        False -> split_groups(int_str)
      }
      let sep = group_sep(key)
      groups
      |> list.map(fn(g) { [#("group", sep), #("integer", g)] })
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
  type_: String,
  int_digits: String,
  frac_digits: String,
  negative: Bool,
) -> String {
  let _ = negative
  case type_ {
    "ordinal" -> {
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
    _ ->
      case int_digits == "1" && frac_digits == "" {
        True -> "one"
        False -> "other"
      }
  }
}

pub fn plural_categories_en(type_: String) -> List(String) {
  case type_ {
    "ordinal" -> ["few", "one", "other", "two"]
    _ -> ["one", "other"]
  }
}

// ============================================================================
// List formatting — CLDR en
// ============================================================================

/// CreatePartsFromList (§13.5.2) with en patterns.
pub fn list_format_parts(
  type_: String,
  style: String,
  items: List(String),
) -> List(Part) {
  case items {
    [] -> []
    [only] -> [#("element", only)]
    [a, b] -> {
      let sep = two_separator(type_, style)
      [#("element", a), #("literal", sep), #("element", b)]
    }
    [first, ..rest] -> {
      let mid = case type_, style {
        "unit", "narrow" -> " "
        _, _ -> ", "
      }
      let last_sep = end_separator(type_, style)
      build_list_parts(rest, [#("element", first)], mid, last_sep)
    }
  }
}

fn two_separator(type_: String, style: String) -> String {
  case type_, style {
    "conjunction", "narrow" -> ", "
    "conjunction", "short" -> " & "
    "conjunction", _ -> " and "
    "disjunction", _ -> " or "
    "unit", "narrow" -> " "
    "unit", _ -> ", "
    _, _ -> " and "
  }
}

fn end_separator(type_: String, style: String) -> String {
  case type_, style {
    "conjunction", "long" -> ", and "
    "conjunction", "short" -> ", & "
    "conjunction", "narrow" -> ", "
    "disjunction", _ -> ", or "
    "unit", "narrow" -> " "
    "unit", _ -> ", "
    _, _ -> ", and "
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
    [last] -> list.reverse([#("element", last), #("literal", last_sep), ..acc])
    [next, ..rest] ->
      build_list_parts(
        rest,
        [#("element", next), #("literal", mid), ..acc],
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
  style: String,
  numeric: String,
  value: Float,
  unit: String,
  value_parts: List(Part),
) -> List(#(String, String, String)) {
  let is_auto = numeric == "auto"
  case is_auto, rtf_auto_name(unit, value +. 0.0) {
    True, Some(name) -> [#("literal", name, "")]
    _, _ -> {
      let plural = case float.absolute_value(value) {
        1.0 -> "one"
        _ -> "other"
      }
      let unit_text = rtf_unit_en(style, unit, plural)
      let past = value <. 0.0 || is_neg_zero(value)
      // Numeric parts carry the unit for formatToParts.
      let tagged =
        list.map(value_parts, fn(p) {
          let #(t, v) = p
          case t {
            "literal" -> #(t, v, "")
            _ -> #(t, v, unit)
          }
        })
      case past {
        True ->
          list.flatten([
            tagged,
            [#("literal", " " <> unit_text <> " ago", "")],
          ])
        False ->
          list.flatten([
            [#("literal", "in ", "")],
            tagged,
            [#("literal", " " <> unit_text, "")],
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

fn rtf_unit_en(style: String, unit: String, plural: String) -> String {
  let single = plural == "one"
  case style {
    "long" ->
      case single {
        True -> unit
        False -> unit <> "s"
      }
    "short" | "narrow" ->
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
    _ -> unit
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

/// Howard Hinnant's civil_from_days algorithm.
fn civil_from_days(z: Int) -> #(Int, Int, Int) {
  let z = z + 719_468
  let era = case z >= 0 {
    True -> z / 146_097
    False -> { z - 146_096 } / 146_097
  }
  let doe = z - era * 146_097
  let yoe = { doe - doe / 1460 + doe / 36_524 - doe / 146_096 } / 365
  let y = yoe + era * 400
  let doy = doe - { 365 * yoe + yoe / 4 - yoe / 100 }
  let mp = { 5 * doy + 2 } / 153
  let d = doy - { 153 * mp + 2 } / 5 + 1
  let m = case mp < 10 {
    True -> mp + 3
    False -> mp - 9
  }
  let y = case m <= 2 {
    True -> y + 1
    False -> y
  }
  #(y, m, d)
}

pub fn month_name(m: Int, width: String) -> String {
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
    "long" -> long
    "short" ->
      case m {
        9 -> "Sep"
        _ -> string.slice(long, 0, 3)
      }
    "narrow" -> string.slice(long, 0, 1)
    _ -> long
  }
}

pub fn weekday_name(wd: Int, width: String) -> String {
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
    "long" -> long
    "short" -> string.slice(long, 0, 3)
    "narrow" ->
      case wd {
        2 | 4 -> string.slice(long, 0, 1)
        _ -> string.slice(long, 0, 1)
      }
    _ -> long
  }
}

pub fn era_name(year: Int, width: String) -> String {
  let bc = year <= 0
  case width, bc {
    "long", True -> "Before Christ"
    "long", False -> "Anno Domini"
    "narrow", True -> "B"
    "narrow", False -> "A"
    _, True -> "BC"
    _, False -> "AD"
  }
}

/// en flexible day periods (CLDR): used for the dayPeriod option.
pub fn day_period_name(hour: Int, minute: Int, width: String) -> String {
  let mins = hour * 60 + minute
  let noon = case width {
    "narrow" -> "n"
    _ -> "noon"
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
  granularity: String,
) -> List(#(String, Int, Bool)) {
  case granularity {
    "word" -> segment_words(s)
    "sentence" -> segment_sentences(s)
    _ -> segment_graphemes(s)
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

fn utf16_len(s: String) -> Int {
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
