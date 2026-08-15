//// Unicode Default Case Conversion — the engine behind
//// `String.prototype.toLowerCase`/`toUpperCase` (§22.1.3.27/.28) *and*
//// `Intl`'s `toLocaleLowerCase`/`toLocaleUpperCase` (ECMA-402 §19.1.2/.3).
////
//// This lives outside `builtins/` on purpose: Intl must apply the same
//// casing the plain methods do, and it must do so WITHOUT re-entering JS.
//// Reading `String.prototype.toLowerCase` off the heap and `[[Call]]`ing it
//// would let user code that reassigns `String.prototype.toLowerCase` change
//// what `toLocaleLowerCase` returns.

import gleam/list
import gleam/option.{None, Some}
import gleam/string

/// Unicode Default Case Conversion toLowercase, including the SpecialCasing
/// Final_Sigma rule: GREEK CAPITAL LETTER SIGMA (U+03A3) lowercases to FINAL
/// SIGMA (U+03C2) when preceded by a cased character (skipping
/// case-ignorable characters) and not followed by one.
pub fn to_lower_case(s: String) -> String {
  // Codepoint-based scan: Gleam's string functions are grapheme-aware, so a
  // Σ followed by a combining mark would be invisible to string.split.
  let cps = string.to_utf_codepoints(s) |> list.map(string.utf_codepoint_to_int)
  case list.contains(cps, 0x03A3) {
    False -> string.lowercase(s)
    True -> sigma_assemble(split_cps_on_sigma(cps, [], []), True)
  }
}

/// Split a codepoint list on U+03A3 boundaries.
fn split_cps_on_sigma(
  cps: List(Int),
  cur: List(Int),
  acc: List(List(Int)),
) -> List(List(Int)) {
  case cps {
    [] -> list.reverse([list.reverse(cur), ..acc])
    [0x03A3, ..rest] -> split_cps_on_sigma(rest, [], [list.reverse(cur), ..acc])
    [cp, ..rest] -> split_cps_on_sigma(rest, [cp, ..cur], acc)
  }
}

/// Lowercase the parts of a Σ-split codepoint list, joining with σ or ς per
/// the Final_Sigma context rule. `is_first` is True for the leading part (no
/// Σ before it).
fn sigma_assemble(parts: List(List(Int)), is_first: Bool) -> String {
  case parts {
    [] -> ""
    [last] -> lowercase_cps(last)
    [part, ..rest] -> {
      // Before-context: nearest non-case-ignorable char at the end of this
      // part; if the part is entirely ignorable, the preceding boundary char
      // (another Σ, which is cased) decides.
      let preceded = case first_non_ignorable_cased(list.reverse(part)) {
        Some(cased) -> cased
        None -> !is_first
      }
      // After-context: nearest non-case-ignorable char at the start of the
      // next part; if that part is entirely ignorable, a following boundary
      // (another Σ) counts as cased, end-of-string does not.
      let followed = case rest {
        [next, ..more] ->
          case first_non_ignorable_cased(next) {
            Some(cased) -> cased
            None -> more != []
          }
        [] -> False
      }
      let sigma = case preceded && !followed {
        True -> "\u{03C2}"
        False -> "\u{03C3}"
      }
      lowercase_cps(part) <> sigma <> sigma_assemble(rest, False)
    }
  }
}

/// Codepoint ints back to a lowercased string. The ints all came from
/// string.to_utf_codepoints, so utf_codepoint cannot fail here.
fn lowercase_cps(cps: List(Int)) -> String {
  cps
  |> list.filter_map(string.utf_codepoint)
  |> string.from_utf_codepoints
  |> string.lowercase
}

fn first_non_ignorable_cased(cps: List(Int)) -> option.Option(Bool) {
  case cps {
    [] -> None
    [cp, ..rest] ->
      case is_case_ignorable_cp(cp) {
        True -> first_non_ignorable_cased(rest)
        False -> Some(is_cased_cp(cp))
      }
  }
}

/// Approximation of the Unicode Cased property (Lu/Ll/Lt plus cased Nl/So
/// ranges). Holes inside ranges are unassigned codepoints, so over-matching
/// them is harmless.
fn is_cased_cp(cp: Int) -> Bool {
  case cp {
    _ if cp >= 0x41 && cp <= 0x5A -> True
    _ if cp >= 0x61 && cp <= 0x7A -> True
    0xAA | 0xB5 | 0xBA -> True
    _ if cp >= 0xC0 && cp <= 0xD6 -> True
    _ if cp >= 0xD8 && cp <= 0xF6 -> True
    _ if cp >= 0xF8 && cp <= 0x2AF -> True
    _ if cp >= 0x370 && cp <= 0x373 -> True
    0x376 | 0x377 | 0x37F | 0x386 -> True
    _ if cp >= 0x37B && cp <= 0x37D -> True
    _ if cp >= 0x388 && cp <= 0x481 -> True
    _ if cp >= 0x48A && cp <= 0x52F -> True
    _ if cp >= 0x531 && cp <= 0x556 -> True
    _ if cp >= 0x560 && cp <= 0x588 -> True
    _ if cp >= 0x10A0 && cp <= 0x10CD -> True
    _ if cp >= 0x13A0 && cp <= 0x13FD -> True
    _ if cp >= 0x1C80 && cp <= 0x1C88 -> True
    _ if cp >= 0x1C90 && cp <= 0x1CBF -> True
    _ if cp >= 0x1E00 && cp <= 0x1FFC -> True
    _ if cp >= 0x2126 && cp <= 0x212B -> True
    _ if cp >= 0x2160 && cp <= 0x217F -> True
    0x2183 | 0x2184 -> True
    _ if cp >= 0x24B6 && cp <= 0x24E9 -> True
    _ if cp >= 0x2C00 && cp <= 0x2D2D -> True
    _ if cp >= 0xA640 && cp <= 0xA66D -> True
    _ if cp >= 0xA680 && cp <= 0xA69B -> True
    _ if cp >= 0xA722 && cp <= 0xA787 -> True
    _ if cp >= 0xA78B && cp <= 0xA7CA -> True
    _ if cp >= 0xAB70 && cp <= 0xABBF -> True
    _ if cp >= 0xFB00 && cp <= 0xFB17 -> True
    _ if cp >= 0xFF21 && cp <= 0xFF3A -> True
    _ if cp >= 0xFF41 && cp <= 0xFF5A -> True
    _ if cp >= 0x10400 && cp <= 0x104FB -> True
    _ if cp >= 0x10C80 && cp <= 0x10CFF -> True
    _ if cp >= 0x118A0 && cp <= 0x118DF -> True
    _ if cp >= 0x16E40 && cp <= 0x16E7F -> True
    _ if cp >= 0x1D400 && cp <= 0x1D7CB -> True
    _ if cp >= 0x1E900 && cp <= 0x1E943 -> True
    _ -> False
  }
}

/// Approximation of the Unicode Case_Ignorable property: Mn/Me/Cf/Lm/Sk
/// blocks plus the Word_Break MidLetter/MidNumLet/Single_Quote punctuation.
fn is_case_ignorable_cp(cp: Int) -> Bool {
  case cp {
    0x27
    | 0x2E
    | 0x3A
    | 0x5E
    | 0x60
    | 0xA8
    | 0xAD
    | 0xAF
    | 0xB4
    | 0xB7
    | 0xB8 -> True
    _ if cp >= 0x2B0 && cp <= 0x36F -> True
    0x374 | 0x375 | 0x37A | 0x384 | 0x385 | 0x387 -> True
    _ if cp >= 0x483 && cp <= 0x489 -> True
    _ if cp >= 0x559 && cp <= 0x55F -> True
    _ if cp >= 0x591 && cp <= 0x5C7 -> True
    0x5F3 | 0x5F4 -> True
    _ if cp >= 0x600 && cp <= 0x605 -> True
    _ if cp >= 0x610 && cp <= 0x61A -> True
    0x61C | 0x640 | 0x670 | 0x6DD | 0x70F | 0x711 -> True
    _ if cp >= 0x64B && cp <= 0x65F -> True
    _ if cp >= 0x6D6 && cp <= 0x6DC -> True
    _ if cp >= 0x6DF && cp <= 0x6E8 -> True
    _ if cp >= 0x6EA && cp <= 0x6ED -> True
    _ if cp >= 0x730 && cp <= 0x74A -> True
    _ if cp >= 0x7A6 && cp <= 0x7B0 -> True
    _ if cp >= 0x7EB && cp <= 0x7F5 -> True
    _ if cp >= 0x816 && cp <= 0x82D -> True
    _ if cp >= 0x180B && cp <= 0x180E -> True
    _ if cp >= 0x1AB0 && cp <= 0x1AFF -> True
    _ if cp >= 0x1C78 && cp <= 0x1C7D -> True
    _ if cp >= 0x1DC0 && cp <= 0x1DFF -> True
    0x1FBD -> True
    _ if cp >= 0x1FBF && cp <= 0x1FC1 -> True
    _ if cp >= 0x1FCD && cp <= 0x1FCF -> True
    _ if cp >= 0x1FDD && cp <= 0x1FDF -> True
    _ if cp >= 0x1FED && cp <= 0x1FEF -> True
    0x1FFD | 0x1FFE -> True
    0x2018 | 0x2019 | 0x2024 | 0x2027 -> True
    _ if cp >= 0x200B && cp <= 0x200F -> True
    _ if cp >= 0x202A && cp <= 0x202E -> True
    _ if cp >= 0x2060 && cp <= 0x2064 -> True
    _ if cp >= 0x2066 && cp <= 0x206F -> True
    0x2071 | 0x207F -> True
    _ if cp >= 0x2090 && cp <= 0x209C -> True
    _ if cp >= 0x20D0 && cp <= 0x20F0 -> True
    0x2C7C | 0x2C7D | 0x2D6F | 0x2D7F | 0x2E2F -> True
    _ if cp >= 0x2DE0 && cp <= 0x2DFF -> True
    0x3005 | 0x303B | 0x309B | 0x309C | 0xFB1E -> True
    _ if cp >= 0xA66F && cp <= 0xA672 -> True
    _ if cp >= 0xA674 && cp <= 0xA67D -> True
    0xA67F | 0xA69C | 0xA69D | 0xA69E | 0xA69F -> True
    _ if cp >= 0xA700 && cp <= 0xA721 -> True
    0xA770 | 0xA788 | 0xA789 | 0xA78A | 0xA7F8 | 0xA7F9 -> True
    _ if cp >= 0xFE00 && cp <= 0xFE0F -> True
    _ if cp >= 0xFE20 && cp <= 0xFE2F -> True
    0xFE13 | 0xFE52 | 0xFE55 | 0xFEFF | 0xFF07 | 0xFF0E | 0xFF1A -> True
    0xFF3E | 0xFF40 | 0xFF70 | 0xFF9E | 0xFF9F | 0xFFE3 -> True
    _ if cp >= 0x1D165 && cp <= 0x1D244 -> True
    _ if cp >= 0xE0001 && cp <= 0xE01EF -> True
    _ -> False
  }
}

/// Unicode Default Case Conversion toUppercase. No context-sensitive rule
/// applies at the root locale, so this is the plain per-codepoint mapping.
pub fn to_upper_case(s: String) -> String {
  string.uppercase(s)
}
