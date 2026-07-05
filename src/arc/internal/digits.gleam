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

/// Value of an ASCII hex digit given as a code point (`0-9`, `A-F`, `a-f`).
///
/// The code-point form of `hex_value`, for callers scanning bytes or code
/// units. Never reach for `int.base_parse` in its place: that is Erlang's
/// `binary_to_integer/2`, which also accepts a leading sign, so `"%+4"`
/// would decode as U+0004 instead of being rejected as a bad escape.
pub fn hex_value_code(code: Int) -> Option(Int) {
  case code {
    // 0-9
    _ if code >= 0x30 && code <= 0x39 -> Some(code - 0x30)
    // A-F
    _ if code >= 0x41 && code <= 0x46 -> Some(code - 0x41 + 10)
    // a-f
    _ if code >= 0x61 && code <= 0x66 -> Some(code - 0x61 + 10)
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

/// Value of an ASCII alphanumeric digit in radix 36: `0-9` → 0..9 and
/// `a-z`/`A-Z` → 10..35, covering every radix from 2 to 36. Callers pick a
/// radix by rejecting values `>= radix`.
pub fn alnum_value(ch: String) -> Option(Int) {
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
    "g" | "G" -> Some(16)
    "h" | "H" -> Some(17)
    "i" | "I" -> Some(18)
    "j" | "J" -> Some(19)
    "k" | "K" -> Some(20)
    "l" | "L" -> Some(21)
    "m" | "M" -> Some(22)
    "n" | "N" -> Some(23)
    "o" | "O" -> Some(24)
    "p" | "P" -> Some(25)
    "q" | "Q" -> Some(26)
    "r" | "R" -> Some(27)
    "s" | "S" -> Some(28)
    "t" | "T" -> Some(29)
    "u" | "U" -> Some(30)
    "v" | "V" -> Some(31)
    "w" | "W" -> Some(32)
    "x" | "X" -> Some(33)
    "y" | "Y" -> Some(34)
    "z" | "Z" -> Some(35)
    _ -> None
  }
}

/// `0-9`, `a-f`, `A-F`.
pub fn is_hex(ch: String) -> Bool {
  hex_value(ch) != None
}

/// `0-9`.
pub fn is_decimal(ch: String) -> Bool {
  digit_value(ch) != None
}

/// `0-7`.
pub fn is_octal(ch: String) -> Bool {
  digit_value(ch) |> option.map(fn(d) { d < 8 }) |> option.unwrap(False)
}

/// `0` or `1`.
pub fn is_binary(ch: String) -> Bool {
  digit_value(ch) |> option.map(fn(d) { d < 2 }) |> option.unwrap(False)
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
