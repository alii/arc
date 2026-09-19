import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/string

// not int.base_parse, that accepts a leading sign
pub fn hex_value_code(code: Int) -> Option(Int) {
  case code {
    _ if code >= 0x30 && code <= 0x39 -> Some(code - 0x30)
    _ if code >= 0x41 && code <= 0x46 -> Some(code - 0x41 + 10)
    _ if code >= 0x61 && code <= 0x66 -> Some(code - 0x61 + 10)
    _ -> None
  }
}

pub fn is_decimal_code(c: Int) -> Bool {
  c >= 0x30 && c <= 0x39
}

pub fn is_ascii_alpha_code(c: Int) -> Bool {
  { c >= 0x41 && c <= 0x5A } || { c >= 0x61 && c <= 0x7A }
}

pub fn is_ascii_alnum_code(c: Int) -> Bool {
  is_decimal_code(c) || is_ascii_alpha_code(c)
}

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

// exactly n digits, none if fewer
pub fn take(s: String, n: Int) -> Option(#(Int, String)) {
  take_loop(s, n, 0)
}

fn take_loop(s: String, n: Int, acc: Int) -> Option(#(Int, String)) {
  case n {
    0 -> Some(#(acc, s))
    _ ->
      case string.pop_grapheme(s) {
        Ok(#(c, rest)) ->
          case digit_value(c) {
            Some(d) -> take_loop(rest, n - 1, acc * 10 + d)
            None -> None
          }
        Error(Nil) -> None
      }
  }
}

pub fn pad2(n: Int) -> String {
  int.to_string(int.absolute_value(n)) |> string.pad_start(2, "0")
}

pub fn pad3(n: Int) -> String {
  int.to_string(int.absolute_value(n)) |> string.pad_start(3, "0")
}
