// language-sensitive case mappings specialcasing.txt tailors

import gleam/list
import gleam/string

pub fn turkic_case(s: String, upper upper: Bool) -> String {
  case upper {
    True ->
      // i → İ (U+0130)
      string.to_graphemes(s)
      |> list.map(fn(g) {
        case g {
          "i" -> "İ"
          _ -> g
        }
      })
      |> string.join("")
    False -> {
      // İ → i, I → ı (U+0131), I + U+0307 → i
      let cps =
        string.to_utf_codepoints(s) |> list.map(string.utf_codepoint_to_int)
      lower_turkic_cps(cps, [])
    }
  }
}

fn codepoint_text(c: Int) -> String {
  case string.utf_codepoint(c) {
    Ok(cp) -> string.from_utf_codepoints([cp])
    Error(Nil) -> ""
  }
}

fn lower_turkic_cps(cps: List(Int), acc: List(String)) -> String {
  case cps {
    [] -> string.join(list.reverse(acc), "")
    [0x130, ..rest] -> lower_turkic_cps(rest, ["i", ..acc])
    [0x49, 0x307, ..rest] -> lower_turkic_cps(rest, ["i", ..acc])
    [0x49, ..rest] -> lower_turkic_cps(rest, ["ı", ..acc])
    [c, ..rest] -> lower_turkic_cps(rest, [codepoint_text(c), ..acc])
  }
}

pub fn lithuanian_case(s: String, upper upper: Bool) -> String {
  let cps = string.to_utf_codepoints(s) |> list.map(string.utf_codepoint_to_int)
  case upper {
    // uppercasing drops U+0307 after i/j
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
    [c, ..rest] -> upper_lt_cps(rest, [codepoint_text(c), ..acc])
  }
}

fn lower_lt_cps(cps: List(Int), acc: List(String)) -> String {
  let is_mark = fn(c) { c >= 0x300 && c <= 0x36f && c != 0x307 }
  case cps {
    [] -> string.join(list.reverse(acc), "")
    // I/J before a combining mark keep a dot above
    [0x49, m, ..rest] ->
      case is_mark(m) {
        True -> lower_lt_cps(rest, [codepoint_text(m), "i\u{0307}", ..acc])
        False -> lower_lt_cps([m, ..rest], ["i", ..acc])
      }
    [0x4a, m, ..rest] ->
      case is_mark(m) {
        True -> lower_lt_cps(rest, [codepoint_text(m), "j\u{0307}", ..acc])
        False -> lower_lt_cps([m, ..rest], ["j", ..acc])
      }
    [0xcc, ..rest] -> lower_lt_cps(rest, ["i\u{0307}\u{0300}", ..acc])
    [0xcd, ..rest] -> lower_lt_cps(rest, ["i\u{0307}\u{0301}", ..acc])
    [0x128, ..rest] -> lower_lt_cps(rest, ["i\u{0307}\u{0303}", ..acc])
    [c, ..rest] -> lower_lt_cps(rest, [codepoint_text(c), ..acc])
  }
}
