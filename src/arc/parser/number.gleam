/// JavaScript numeric-literal parsing.
///
/// ONE classifier — `parse_numeric_literal` — turns the raw source text of a
/// NumericLiteral token into the value it denotes. Everything a caller could
/// otherwise re-derive from the string (is this hex? is this a legacy octal?
/// is this a BigInt?) is decided exactly once, in `classify`.
import arc/internal/digits
import arc/parser/ast.{type LiteralNumber, FiniteNumber, InfiniteNumber}
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/string

/// The value a NumericLiteral token denotes: a Number or a BigInt. Which one
/// is decided by the trailing `n`, inside the classifier — callers no longer
/// sniff the suffix themselves.
pub type NumericLiteral {
  NumberValue(value: LiteralNumber)
  BigIntValue(value: Int)
}

/// Why raw text handed to `parse_numeric_literal` is not a numeric literal.
/// The lexer should never emit such a token; the parser turns this into a
/// SyntaxError rather than silently cooking the literal to 0.
pub type NumberParseError {
  /// A digit the radix does not admit, or text that is not numeric-literal
  /// syntax at all. Carries the offending digits.
  NotANumericLiteral(text: String)
  /// A radix prefix (0x/0o/0b) or a BigInt suffix with no digits behind it.
  EmptyDigits
}

pub fn parse_error_message(err: NumberParseError) -> String {
  case err {
    NotANumericLiteral(text) -> "Malformed numeric literal: " <> text
    EmptyDigits -> "Numeric literal has no digits"
  }
}

/// Why converting a decimal float literal to a Float can fail.
pub type FloatParseError {
  /// The text is valid float syntax but its magnitude does not fit in an
  /// IEEE double (e.g. "1e400", mathematically Infinity). Erlang's
  /// `binary_to_float` raises for overflow; underflow rounds to 0.0.
  OutOfRange
  /// The text is not something Erlang can parse as a float at all.
  Invalid
}

/// The shapes a NumericLiteral token can take, with numeric separators
/// already stripped. Classified once, up front, so no downstream code has to
/// re-scan the string to learn which shape it is looking at.
type LiteralForm {
  /// `0xFF` / `0o17` / `0b101` — an integer in the prefixed radix.
  Radix(digits: String, radix: Int)
  /// Annex B §B.1.1 LegacyOctalIntegerLiteral: `010` is 8, NOT 10. Sloppy
  /// mode only — the parser rejects it under "use strict".
  LegacyOctal(digits: String)
  /// Annex B §B.1.1 NonOctalDecimalIntegerLiteral: `08`, `09` — a leading
  /// zero that cannot be octal, so it is base 10. Sloppy mode only.
  NonOctalDecimal(digits: String)
  /// An ordinary DecimalLiteral, with an optional fraction and/or exponent.
  Decimal(text: String)
  /// A BigInt literal, `n` suffix already stripped.
  BigInt(digits: String, radix: Int)
}

/// Parse the raw source text of a numeric literal token to its value.
pub fn parse_numeric_literal(
  raw: String,
) -> Result(NumericLiteral, NumberParseError) {
  case classify(raw) {
    Radix(digits:, radix:) -> integer_number(digits, radix)
    // The whole point of the classifier: `010` is base 8.
    LegacyOctal(digits) -> integer_number(digits, 8)
    NonOctalDecimal(digits) -> integer_number(digits, 10)
    Decimal(text) -> {
      use n <- result.map(parse_decimal(text))
      NumberValue(n)
    }
    BigInt(digits:, radix:) -> {
      use i <- result.map(parse_digits(digits, radix))
      BigIntValue(i)
    }
  }
}

fn integer_number(
  digits: String,
  radix: Int,
) -> Result(NumericLiteral, NumberParseError) {
  use i <- result.map(parse_digits(digits, radix))
  NumberValue(nonneg_int_to_number(i))
}

fn classify(raw: String) -> LiteralForm {
  // Numeric separators are only ever legal between digits, so a blanket
  // strip is safe and keeps every arm below separator-free.
  let clean = string.replace(raw, "_", "")
  case string.ends_with(clean, "n") {
    True -> {
      let #(digits, radix) = split_radix(string.drop_end(clean, 1))
      BigInt(digits:, radix:)
    }
    False ->
      case clean {
        "0x" <> hex | "0X" <> hex -> Radix(hex, 16)
        "0o" <> oct | "0O" <> oct -> Radix(oct, 8)
        "0b" <> bin | "0B" <> bin -> Radix(bin, 2)
        "0" <> rest -> classify_leading_zero(rest)
        _ -> Decimal(clean)
      }
  }
}

/// A `0`-prefixed literal that is not 0x/0o/0b. All-octal digits behind the
/// zero make it a LegacyOctalIntegerLiteral; an 8 or a 9 makes it a
/// NonOctalDecimalIntegerLiteral; anything else (`0`, `0.5`, `0e3`) is an
/// ordinary DecimalLiteral that just happens to start with a zero.
fn classify_leading_zero(rest: String) -> LiteralForm {
  case string.to_graphemes(rest) {
    [] -> Decimal("0")
    graphemes ->
      case list.all(graphemes, is_octal_digit) {
        True -> LegacyOctal(rest)
        False ->
          case list.all(graphemes, is_decimal_digit) {
            True -> NonOctalDecimal(rest)
            False -> Decimal("0" <> rest)
          }
      }
  }
}

fn split_radix(digits: String) -> #(String, Int) {
  case digits {
    "0x" <> hex | "0X" <> hex -> #(hex, 16)
    "0o" <> oct | "0O" <> oct -> #(oct, 8)
    "0b" <> bin | "0B" <> bin -> #(bin, 2)
    _ -> #(digits, 10)
  }
}

fn parse_decimal(text: String) -> Result(LiteralNumber, NumberParseError) {
  // A dot or an exponent means a float literal; otherwise a decimal integer,
  // which we convert exactly (see nonneg_int_to_number).
  case
    string.contains(text, ".")
    || string.contains(text, "e")
    || string.contains(text, "E")
  {
    True ->
      case parse_float(normalize_dot(text)) {
        Ok(f) -> Ok(FiniteNumber(f))
        // A literal never carries a sign (unary minus is a separate
        // operator), so an out-of-range float literal is always +Infinity.
        Error(OutOfRange) -> Ok(InfiniteNumber)
        Error(Invalid) -> Error(NotANumericLiteral(text))
      }
    False -> {
      use i <- result.map(parse_digits(text, 10))
      nonneg_int_to_number(i)
    }
  }
}

const two52 = 4_503_599_627_370_496

const two53 = 9_007_199_254_740_992

/// A non-negative Int → the Number it denotes, with correct rounding
/// (round-to-nearest, ties-to-even). Erlang's float/1 mis-rounds integers
/// wider than 53 bits, so reduce to a 53-bit mantissa ourselves and convert
/// the (exactly representable) result. Past the double range the value is
/// Infinity, per ES2024 §12.9.3 — a numeric literal is never negative, so
/// there is no -Infinity to consider.
fn nonneg_int_to_number(a: Int) -> LiteralNumber {
  case a < two53 {
    True -> FiniteNumber(int.to_float(a))
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
        True -> InfiniteNumber
        False -> FiniteNumber(int.to_float(int.bitwise_shift_left(q, s)))
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

/// The exact integer value of a run of digits in `radix`.
fn parse_digits(s: String, radix: Int) -> Result(Int, NumberParseError) {
  case string.to_graphemes(s) {
    [] -> Error(EmptyDigits)
    graphemes ->
      list.try_fold(graphemes, 0, fn(acc, ch) {
        use d <- result.try(
          digits.hex_value(ch) |> option.to_result(NotANumericLiteral(s)),
        )
        case d < radix {
          True -> Ok(acc * radix + d)
          False -> Error(NotANumericLiteral(s))
        }
      })
  }
}

fn is_octal_digit(ch: String) -> Bool {
  case ch {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" -> True
    _ -> False
  }
}

fn is_decimal_digit(ch: String) -> Bool {
  case ch {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}
