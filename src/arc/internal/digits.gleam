/// ASCII digit helpers shared by the parsers and the date/number builtins.
///
/// These are pure lookups over single-character strings; keeping one copy
/// avoids the five near-identical tables that used to live in the parser and
/// builtin modules.
import gleam/option.{type Option, None, Some}
import gleam/string

/// Value of an ASCII hex digit (0-9, a-f, A-F).
pub fn hex_value(ch: String) -> Option(Int) {
  case ch {
    "0" -> Some(0)
    "1" -> Some(1)
    "2" -> Some(2)
    "3" -> Some(3)
    "4" -> Some(4)
    "5" -> Some(5)
    "6" -> Some(6)
    "7" -> Some(7)
    "8" -> Some(8)
    "9" -> Some(9)
    "a" | "A" -> Some(10)
    "b" | "B" -> Some(11)
    "c" | "C" -> Some(12)
    "d" | "D" -> Some(13)
    "e" | "E" -> Some(14)
    "f" | "F" -> Some(15)
    _ -> None
  }
}

/// Value of an ASCII decimal digit.
pub fn digit_value(ch: String) -> Option(Int) {
  case ch {
    "0" -> Some(0)
    "1" -> Some(1)
    "2" -> Some(2)
    "3" -> Some(3)
    "4" -> Some(4)
    "5" -> Some(5)
    "6" -> Some(6)
    "7" -> Some(7)
    "8" -> Some(8)
    "9" -> Some(9)
    _ -> None
  }
}

/// Consume exactly `n` ASCII digits, returning #(value, rest). None if fewer
/// than `n` digits are available.
pub fn take_digits(s: String, n: Int) -> Option(#(Int, String)) {
  take_digits_loop(s, n, 0)
}

fn take_digits_loop(s: String, n: Int, acc: Int) -> Option(#(Int, String)) {
  case n {
    0 -> Some(#(acc, s))
    _ ->
      case string.pop_grapheme(s) {
        Ok(#(c, rest)) ->
          case digit_value(c) {
            Some(d) -> take_digits_loop(rest, n - 1, acc * 10 + d)
            None -> None
          }
        Error(Nil) -> None
      }
  }
}
