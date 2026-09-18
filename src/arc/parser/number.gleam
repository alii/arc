import arc/internal/digits
import arc/parser/ast.{type LiteralNumber, FiniteNumber, InfiniteNumber}
import gleam/bit_array
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub type ParsedNumeric {
  ParsedNumber(value: LiteralNumber)
  ParsedBigInt(value: Int)
}

pub type NumberParseError {
  NotANumericLiteral(text: String)
  EmptyDigits
}

pub fn parse_error_message(err: NumberParseError) -> String {
  case err {
    NotANumericLiteral(text) -> "Malformed numeric literal: " <> text
    EmptyDigits -> "Numeric literal has no digits"
  }
}

type FloatParseError {
  OutOfRange
  Invalid
}

type LiteralForm {
  Radix(digits: String, radix: Int)
  // annex b: 010 is 8, 08 is decimal, sloppy only
  LegacyOctal(digits: String)
  NonOctalDecimal(digits: String)
  Decimal(text: String, is_float: Bool)
  BigInt(digits: String, radix: Int)
}

pub fn parse_numeric_literal(
  raw: String,
) -> Result(ParsedNumeric, NumberParseError) {
  case classify(raw) {
    Radix(digits:, radix:) -> integer_number(digits, radix)
    LegacyOctal(digits) -> integer_number(digits, 8)
    NonOctalDecimal(digits) -> integer_number(digits, 10)
    Decimal(text:, is_float:) -> {
      use n <- result.map(parse_decimal(text, is_float))
      ParsedNumber(n)
    }
    BigInt(digits:, radix:) -> {
      use i <- result.map(parse_digits(digits, radix))
      ParsedBigInt(i)
    }
  }
}

fn integer_number(
  digits: String,
  radix: Int,
) -> Result(ParsedNumeric, NumberParseError) {
  use i <- result.map(parse_digits(digits, radix))
  ParsedNumber(rounded_int_to_number(i))
}

type Shape {
  Shape(has_separator: Bool, is_float: Bool, is_bigint: Bool)
}

fn shape(
  bytes: BitArray,
  has_separator has_separator: Bool,
  is_float is_float: Bool,
) -> Shape {
  case bytes {
    <<"_", rest:bytes>> -> shape(rest, has_separator: True, is_float:)
    <<".", rest:bytes>> | <<"e", rest:bytes>> | <<"E", rest:bytes>> ->
      shape(rest, has_separator:, is_float: True)
    <<"n">> -> Shape(has_separator:, is_float:, is_bigint: True)
    <<_, rest:bytes>> -> shape(rest, has_separator:, is_float:)
    _ -> Shape(has_separator:, is_float:, is_bigint: False)
  }
}

fn classify(raw: String) -> LiteralForm {
  let Shape(has_separator:, is_float:, is_bigint:) =
    shape(bit_array.from_string(raw), has_separator: False, is_float: False)
  let clean = case has_separator {
    True -> string.replace(raw, "_", "")
    False -> raw
  }
  let body = case is_bigint {
    True -> string.drop_end(clean, 1)
    False -> clean
  }
  case radix_prefix(body), is_bigint {
    Some(#(digits, radix)), True -> BigInt(digits:, radix:)
    Some(#(digits, radix)), False -> Radix(digits:, radix:)
    None, True -> BigInt(digits: body, radix: 10)
    None, False ->
      case body {
        "0" -> Decimal("0", False)
        "0" <> rest ->
          case is_float {
            True -> Decimal(body, True)
            False -> classify_leading_zero(rest)
          }
        _ -> Decimal(body, is_float)
      }
  }
}

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

fn radix_prefix(text: String) -> Option(#(String, Int)) {
  case text {
    "0x" <> hex | "0X" <> hex -> Some(#(hex, 16))
    "0o" <> oct | "0O" <> oct -> Some(#(oct, 8))
    "0b" <> bin | "0B" <> bin -> Some(#(bin, 2))
    _ -> None
  }
}

fn parse_decimal(
  text: String,
  is_float: Bool,
) -> Result(LiteralNumber, NumberParseError) {
  case is_float {
    True ->
      case parse_float(text) {
        Ok(f) -> Ok(FiniteNumber(f))
        // a literal is never negative so overflow is +infinity
        Error(OutOfRange) -> Ok(InfiniteNumber)
        Error(Invalid) -> Error(NotANumericLiteral(text))
      }
    False -> {
      use i <- result.map(parse_digits(text, 10))
      rounded_int_to_number(i)
    }
  }
}

const two52 = 4_503_599_627_370_496

const two53 = 9_007_199_254_740_992

// float/1 mis-rounds past 53 bits, so round to nearest even ourselves
fn rounded_int_to_number(a: Int) -> LiteralNumber {
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

@external(erlang, "arc_float_ffi", "parse_float")
fn parse_float(s: String) -> Result(Float, FloatParseError)

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
