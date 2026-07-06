//// The Intl.Collator comparison algorithm (ECMA-402 §10.3.3 CompareStrings).
////
//// A pure `String -> String -> Int` function: no `State`, no heap, no JS
//// values — `intl.gleam` reads the resolved options off a `CollatorState` and
//// calls `collator_compare`, which is the only entry point.
////
//// The comparison is a simplified root-locale UCA: a *primary* level over base
//// letters (accents and case folded away), then a *secondary* level over
//// accents and a *tertiary* level over case, each enabled by the collator's
//// `sensitivity` and, for the case level, ordered by `caseFirst`.

import arc/vm/value.{
  type CaseFirst, type CollatorSensitivity, type CollatorState, CaseFirstFalse,
  CaseFirstLower, CaseFirstUpper, SensAccent, SensBase, SensCase, SensVariant,
}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/string

pub fn collator_compare(c: CollatorState, a: String, b: String) -> Int {
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
  let levels = fn() { collator_levels(sensitivity, c.case_first, a, b) }
  case numeric {
    True ->
      case numeric_compare(string.to_graphemes(pa), string.to_graphemes(pb)) {
        0 -> levels()
        n -> n
      }
    False ->
      case simple_compare(pa, pb) {
        0 -> levels()
        n -> n
      }
  }
}

/// Secondary (accents) and tertiary (case) comparisons per sensitivity.
/// The tertiary (case) level honours `[[CaseFirst]]` (§10.1.2 `kf`).
fn collator_levels(
  sensitivity: CollatorSensitivity,
  case_first: CaseFirst,
  a: String,
  b: String,
) -> Int {
  let secondary = fn() {
    simple_compare(
      string.lowercase(fold_combining(a)),
      string.lowercase(fold_combining(b)),
    )
  }
  let tertiary = fn() {
    case case_first {
      // Uppercase first: compare as-is, so "A" (0x41) precedes "a" (0x61).
      CaseFirstUpper -> simple_compare(fold_combining(a), fold_combining(b))
      // Lowercase first — also the `false` (en locale default) order.
      CaseFirstLower | CaseFirstFalse ->
        simple_compare(
          swap_case(fold_combining(a)),
          swap_case(fold_combining(b)),
        )
    }
  }
  case sensitivity {
    SensBase -> 0
    SensAccent -> secondary()
    SensCase -> tertiary()
    SensVariant ->
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
    [ca, ..ta], [cb, ..tb] -> {
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
          case string.compare(ca, cb) {
            order.Eq -> numeric_compare(ta, tb)
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

/// ASCII digits only — the numeric collation level compares runs of `0`-`9`.
fn is_digit_str(s: String) -> Bool {
  s != ""
  && string.to_utf_codepoints(s)
  |> list.all(fn(cp) {
    let c = string.utf_codepoint_to_int(cp)
    c >= 0x30 && c <= 0x39
  })
}
