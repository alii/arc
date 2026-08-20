/// JavaScript numeric-literal parsing.
///
/// ONE classifier — `parse_numeric_literal` — turns the raw source text of a
/// NumericLiteral token into the value it denotes. Everything a caller could
/// otherwise re-derive from the string (is this hex? is this a legacy octal?
/// is this a BigInt?) is decided exactly once, in `classify`.
import arc/internal/digits
import arc/parser/ast.{type LiteralNumber, FiniteNumber, InfiniteNumber}
import gleam/bit_array
import gleam/int
import gleam/option.{Some}
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
  /// An ordinary DecimalLiteral; `is_float` when it has a fraction and/or
  /// an exponent.
  Decimal(text: String, is_float: Bool)
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
    Decimal(text:, is_float:) -> {
      use n <- result.map(parse_decimal(text, is_float))
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

/// What one pass over the literal's bytes finds: a numeric separator
/// anywhere, a `.`/`e`/`E` anywhere (only meaningful for a decimal form),
/// and a trailing `n`.
type Shape {
  Shape(has_separator: Bool, is_float: Bool, is_bigint: Bool)
}

fn shape(bytes: BitArray, sep: Bool, float: Bool) -> Shape {
  case bytes {
    <<0x5F, rest:bytes>> -> shape(rest, True, float)
    <<0x2E, rest:bytes>> | <<0x65, rest:bytes>> | <<0x45, rest:bytes>> ->
      shape(rest, sep, True)
    <<0x6E>> -> Shape(sep, float, True)
    <<_, rest:bytes>> -> shape(rest, sep, float)
    _ -> Shape(sep, float, False)
  }
}

fn classify(raw: String) -> LiteralForm {
  let Shape(has_separator:, is_float:, is_bigint:) =
    shape(bit_array.from_string(raw), False, False)
  // Numeric separators are only ever legal between digits, so a blanket
  // strip is safe and keeps every arm below separator-free.
  let clean = case has_separator {
    True -> string.replace(raw, "_", "")
    False -> raw
  }
  case is_bigint {
    True -> {
      let #(digits, radix) = split_radix(string.drop_end(clean, 1))
      BigInt(digits:, radix:)
    }
    False ->
      case clean {
        "0x" <> hex | "0X" <> hex -> Radix(hex, 16)
        "0o" <> oct | "0O" <> oct -> Radix(oct, 8)
        "0b" <> bin | "0B" <> bin -> Radix(bin, 2)
        "0" -> Decimal("0", False)
        "0" <> rest ->
          case is_float {
            True -> Decimal(clean, True)
            False -> classify_leading_zero(rest)
          }
        _ -> Decimal(clean, is_float)
      }
  }
}

/// A `0`-prefixed integer literal that is not 0x/0o/0b. All-octal digits
/// behind the zero make it a LegacyOctalIntegerLiteral; an 8 or a 9 makes it
/// a NonOctalDecimalIntegerLiteral.
fn classify_leading_zero(rest: String) -> LiteralForm {
  case all_octal(bit_array.from_string(rest)) {
    True -> LegacyOctal(rest)
    False -> NonOctalDecimal(rest)
  }
}

fn all_octal(bytes: BitArray) -> Bool {
  case bytes {
    <<c, tail:bytes>> if c >= 0x30 && c <= 0x37 -> all_octal(tail)
    <<>> -> True
    _ -> False
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

fn parse_decimal(
  text: String,
  is_float: Bool,
) -> Result(LiteralNumber, NumberParseError) {
  // A dot or an exponent means a float literal; otherwise a decimal integer,
  // which we convert exactly (see nonneg_int_to_number).
  case is_float {
    True ->
      case parse_float(text) {
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

/// A JS decimal literal → Float, with a typed failure: OutOfRange when the text
/// is valid float syntax whose magnitude overflows a double, Invalid otherwise.
/// The literal is fed in verbatim: padding it into the shape Erlang's
/// binary_to_float accepts (".5" → "0.5", "1.e3" → "1.0e3") happens inside the
/// FFI, next to the syntax classifier that must see the very same text.
///
/// The engine's ONE decimal-literal → double conversion: the parser reads
/// NumericLiterals through it and the runtime reads StringNumericLiterals
/// (`Number("1e999")`) through it, so both agree that an overflowing magnitude
/// is Infinity rather than NaN.
@external(erlang, "arc_float_ffi", "parse_float")
pub fn parse_float(s: String) -> Result(Float, FloatParseError)

/// The exact integer value of a run of digits in `radix`.
fn parse_digits(s: String, radix: Int) -> Result(Int, NumberParseError) {
  case bit_array.from_string(s) {
    <<>> -> Error(EmptyDigits)
    bytes -> digits_value(bytes, radix, 0, s)
  }
}

fn digits_value(
  bytes: BitArray,
  radix: Int,
  acc: Int,
  s: String,
) -> Result(Int, NumberParseError) {
  case bytes {
    <<>> -> Ok(acc)
    <<c, rest:bytes>> ->
      case digits.hex_value_code(c) {
        Some(d) if d < radix -> digits_value(rest, radix, acc * radix + d, s)
        _ -> Error(NotANumericLiteral(s))
      }
    _ -> Error(NotANumericLiteral(s))
  }
}
