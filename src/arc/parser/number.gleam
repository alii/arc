/// JavaScript number literal parsing.
/// Pure functions for converting number literal strings to Float values.
/// Handles hex/octal/binary prefixes, numeric separators, and float formats.
import gleam/int
import gleam/list
import gleam/result
import gleam/string

/// Why converting a decimal float literal to a Float can fail.
pub type FloatParseError {
  /// The text is valid float syntax but its magnitude does not fit in an
  /// IEEE double (e.g. "1e400", mathematically Infinity). Erlang's
  /// `binary_to_float` raises for overflow; underflow rounds to 0.0.
  OutOfRange
  /// The text is not something Erlang can parse as a float at all.
  /// Unreachable for the literals the lexer emits.
  Invalid
}

/// Parse the raw source text of a numeric literal to its Float value.
/// The lexer guarantees well-formedness, so the "impossible" failure arms
/// below fall back to 0.0 rather than crashing the parser.
pub fn parse_js_number(raw: String) -> Float {
  case raw {
    "0x" <> hex | "0X" <> hex -> prefixed_literal_to_float(hex, 16)
    "0o" <> oct | "0O" <> oct -> prefixed_literal_to_float(oct, 8)
    "0b" <> bin | "0B" <> bin -> prefixed_literal_to_float(bin, 2)
    _ -> {
      // Remove numeric separators
      let clean = string.replace(raw, "_", "")
      // A dot or exponent means a float literal; otherwise a decimal integer.
      case
        string.contains(clean, ".")
        || string.contains(clean, "e")
        || string.contains(clean, "E")
      {
        True ->
          case parse_float(normalize_dot(clean)) {
            Ok(f) -> f
            // A literal never carries a sign (unary minus is a separate
            // operator), so an out-of-range float literal always overflows
            // towards +Infinity.
            Error(OutOfRange) -> overflow_clamp(negative: False)
            // Unreachable: the lexer only emits well-formed decimal
            // literals, which normalize_dot turns into valid float syntax.
            Error(Invalid) -> 0.0
          }
        False ->
          case gleam_int_parse(clean) {
            Ok(i) -> int_to_float(i)
            // Unreachable: the lexer only emits decimal digits here.
            Error(Nil) -> 0.0
          }
      }
    }
  }
}

/// A radix-prefixed (0x/0o/0b) literal's digits to its Float value.
fn prefixed_literal_to_float(digits: String, radix: Int) -> Float {
  parse_prefixed_int(digits, radix)
  |> result.map(int_to_float)
  // Unreachable: the lexer rejects a radix prefix without valid digits.
  |> result.unwrap(0.0)
}

/// The digits after a 0x/0o/0b radix prefix (numeric separators allowed) to
/// their exact integer value. Shared by parse_js_number and parse_js_bigint.
fn parse_prefixed_int(digits: String, radix: Int) -> Result(Int, Nil) {
  digits
  |> string.replace("_", "")
  |> parse_int_radix(radix)
}

const two52 = 4_503_599_627_370_496

const two53 = 9_007_199_254_740_992

/// The value a numeric literal takes when its magnitude exceeds the IEEE
/// double range. Shared by the decimal-integer path (int_to_float) and the
/// float/exponent path (parse_js_number).
///
/// KNOWN SPEC DEVIATION. Per ES2024 the mathematical value is ±Infinity, and
/// the VM can represent it (value.JsNum has Infinity/NegInfinity;
/// value.num_from_int already returns Infinity for the same overflow). The
/// only reason we clamp to the largest finite double instead is that
/// ast.NumberLiteral stores a plain Float and BEAM's Float type has no
/// infinity, so this function's return type cannot express the right answer.
/// So `1e400 === Number.MAX_VALUE` is true here where it should be false.
/// Fixing it means widening NumberLiteral to carry a value.JsNum (or a
/// Finite|Infinity sum) through parser.gleam and emit.gleam's push_const /
/// js_format_number sites — a cross-module change, not a local one.
/// The clamp is still strictly better than the pre-existing behaviour of
/// cooking an overflowing literal to 0.0.
fn overflow_clamp(negative negative: Bool) -> Float {
  case negative {
    True -> -1.7976931348623157e308
    False -> 1.7976931348623157e308
  }
}

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
        // Beyond the double range (erlang float conversion would crash).
        True -> overflow_clamp(negative: i < 0)
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

/// Erlang's binary_to_float requires digits on both sides of the '.', but a
/// JS literal may omit either: ".5", "1.", "1.e3", "2.E5". Insert the
/// missing "0". The exponent (if any) is split off first so a mantissa's
/// trailing dot is still seen as trailing in "1.e3".
fn normalize_dot(s: String) -> String {
  let #(mantissa, exponent) = split_exponent(s)
  let mantissa = case string.starts_with(mantissa, ".") {
    True -> "0" <> mantissa
    False -> mantissa
  }
  let mantissa = case string.ends_with(mantissa, ".") {
    True -> mantissa <> "0"
    False -> mantissa
  }
  mantissa <> exponent
}

/// Split a decimal literal into mantissa and exponent ("1.e3" -> #("1.",
/// "e3")). The exponent keeps its leading e/E and is empty when absent.
fn split_exponent(s: String) -> #(String, String) {
  case string.split_once(s, "e") {
    Ok(#(mantissa, exp)) -> #(mantissa, "e" <> exp)
    Error(Nil) ->
      case string.split_once(s, "E") {
        Ok(#(mantissa, exp)) -> #(mantissa, "E" <> exp)
        Error(Nil) -> #(s, "")
      }
  }
}

/// Erlang binary_to_float with a typed failure: OutOfRange when the text is
/// valid float syntax whose magnitude overflows a double, Invalid otherwise.
@external(erlang, "arc_parser_ffi", "parse_float")
fn parse_float(s: String) -> Result(Float, FloatParseError)

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
    "0x" <> hex | "0X" <> hex -> parse_prefixed_int(hex, 16)
    "0o" <> oct | "0O" <> oct -> parse_prefixed_int(oct, 8)
    "0b" <> bin | "0B" <> bin -> parse_prefixed_int(bin, 2)
    _ -> gleam_int_parse(digits)
  }
  result.unwrap(parsed, 0)
}
