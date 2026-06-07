/// JavaScript number literal parsing.
/// Pure functions for converting number literal strings to Float values.
/// Handles hex/octal/binary prefixes, numeric separators, and float formats.
import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub fn parse_js_number(raw: String) -> Float {
  // Simple number parsing — handles basic cases
  case raw {
    "0" -> 0.0
    _ ->
      case string.starts_with(raw, "0x") || string.starts_with(raw, "0X") {
        True ->
          string.drop_start(raw, 2)
          |> string.replace("_", "")
          |> parse_int_radix(16)
          |> result.map(int_to_float)
          |> result.unwrap(0.0)
        False ->
          case string.starts_with(raw, "0o") || string.starts_with(raw, "0O") {
            True ->
              string.drop_start(raw, 2)
              |> string.replace("_", "")
              |> parse_int_radix(8)
              |> result.map(int_to_float)
              |> result.unwrap(0.0)
            False ->
              case
                string.starts_with(raw, "0b") || string.starts_with(raw, "0B")
              {
                True ->
                  string.drop_start(raw, 2)
                  |> string.replace("_", "")
                  |> parse_int_radix(2)
                  |> result.map(int_to_float)
                  |> result.unwrap(0.0)
                False -> {
                  // Remove numeric separators
                  let clean = string.replace(raw, "_", "")
                  // Try float parse first, then int
                  case
                    string.contains(clean, ".")
                    || string.contains(clean, "e")
                    || string.contains(clean, "E")
                  {
                    True ->
                      case gleam_float_parse(clean) {
                        Ok(f) -> f
                        Error(Nil) -> 0.0
                      }
                    False ->
                      case gleam_int_parse(clean) {
                        Ok(i) -> int_to_float(i)
                        Error(Nil) -> 0.0
                      }
                  }
                }
              }
          }
      }
  }
}

const two52 = 4_503_599_627_370_496

const two53 = 9_007_199_254_740_992

/// Integer → Float with correct rounding (round-to-nearest, ties-to-even).
/// Erlang's float/1 mis-rounds integers wider than 53 bits, so reduce to a
/// 53-bit mantissa ourselves and convert the (exactly representable) result.
fn int_to_float(i: Int) -> Float {
  let a = int.absolute_value(i)
  case a < two53 {
    True -> int.to_float(i)
    False -> {
      let s = bit_length(a, 0) - 53
      let q0 = int.bitwise_shift_right(a, s)
      let r = a - int.bitwise_shift_left(q0, s)
      let half = int.bitwise_shift_left(1, s - 1)
      let q = case r > half || { r == half && q0 % 2 == 1 } {
        True -> q0 + 1
        False -> q0
      }
      let #(q, s) = case q == two53 {
        True -> #(two52, s + 1)
        False -> #(q, s)
      }
      case 53 + s > 1024 {
        // Beyond the double range; BEAM floats cannot express infinity, so
        // clamp (erlang float conversion would crash outright).
        True ->
          case i < 0 {
            True -> -1.7976931348623157e308
            False -> 1.7976931348623157e308
          }
        False -> {
          let f = int.to_float(int.bitwise_shift_left(q, s))
          case i < 0 {
            True -> 0.0 -. f
            False -> f
          }
        }
      }
    }
  }
}

fn bit_length(n: Int, acc: Int) -> Int {
  case n == 0 {
    True -> acc
    False -> bit_length(int.bitwise_shift_right(n, 1), acc + 1)
  }
}

fn gleam_float_parse(s: String) -> Result(Float, Nil) {
  case string.contains(s, ".") {
    True -> {
      // Normalize trailing dot (e.g. "1." -> "1.0") and leading dot (e.g. ".5" -> "0.5")
      // for Erlang's binary_to_float which requires digits on both sides
      let normalized = case string.ends_with(s, ".") {
        True -> s <> "0"
        False ->
          case string.starts_with(s, ".") {
            True -> "0" <> s
            False -> s
          }
      }
      case catch_float_parse(normalized) {
        Ok(f) -> Ok(f)
        Error(_) -> Error(Nil)
      }
    }
    False ->
      case string.contains(s, "e") || string.contains(s, "E") {
        True ->
          case catch_float_parse(s) {
            Ok(f) -> Ok(f)
            Error(_) -> Error(Nil)
          }
        False -> Error(Nil)
      }
  }
}

@external(erlang, "arc_parser_ffi", "parse_float")
fn catch_float_parse(s: String) -> Result(Float, Nil)

fn parse_int_radix(s: String, radix: Int) -> Result(Int, Nil) {
  case string.to_graphemes(s) {
    [] -> Error(Nil)
    graphemes ->
      list.try_fold(graphemes, 0, fn(acc, ch) {
        let digit = case ch {
          "0" -> Ok(0)
          "1" -> Ok(1)
          "2" -> Ok(2)
          "3" -> Ok(3)
          "4" -> Ok(4)
          "5" -> Ok(5)
          "6" -> Ok(6)
          "7" -> Ok(7)
          "8" -> Ok(8)
          "9" -> Ok(9)
          "a" | "A" -> Ok(10)
          "b" | "B" -> Ok(11)
          "c" | "C" -> Ok(12)
          "d" | "D" -> Ok(13)
          "e" | "E" -> Ok(14)
          "f" | "F" -> Ok(15)
          _ -> Error(Nil)
        }
        use d <- result.try(digit)
        case d < radix {
          True -> Ok(acc * radix + d)
          False -> Error(Nil)
        }
      })
  }
}

fn gleam_int_parse(s: String) -> Result(Int, Nil) {
  case string.to_graphemes(s) {
    [] -> Error(Nil)
    _ -> {
      let result =
        list.try_fold(string.to_graphemes(s), 0, fn(acc, ch) {
          case ch {
            "0" -> Ok(acc * 10)
            "1" -> Ok(acc * 10 + 1)
            "2" -> Ok(acc * 10 + 2)
            "3" -> Ok(acc * 10 + 3)
            "4" -> Ok(acc * 10 + 4)
            "5" -> Ok(acc * 10 + 5)
            "6" -> Ok(acc * 10 + 6)
            "7" -> Ok(acc * 10 + 7)
            "8" -> Ok(acc * 10 + 8)
            "9" -> Ok(acc * 10 + 9)
            _ -> Error(Nil)
          }
        })
      result
    }
  }
}

/// Parse a BigInt literal's raw text (INCLUDING the trailing "n") to its
/// exact integer value. Handles 0x/0o/0b prefixes and numeric separators.
/// The lexer guarantees well-formedness, so parse failures map to 0.
pub fn parse_js_bigint(raw: String) -> Int {
  let digits =
    raw
    |> string.drop_end(1)
    |> string.replace("_", "")
  let parsed = case digits {
    "0x" <> hex -> parse_int_radix(hex, 16)
    "0X" <> hex -> parse_int_radix(hex, 16)
    "0o" <> oct -> parse_int_radix(oct, 8)
    "0O" <> oct -> parse_int_radix(oct, 8)
    "0b" <> bin -> parse_int_radix(bin, 2)
    "0B" <> bin -> parse_int_radix(bin, 2)
    _ -> gleam_int_parse(digits)
  }
  result.unwrap(parsed, 0)
}
