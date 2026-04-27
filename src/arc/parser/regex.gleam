/// Regex literal scanning and /u flag validation.
/// Pure bit-array scanning functions split from parser.gleam.
/// The parser re-lexes regex literals from source bytes since the lexer
/// can't always distinguish / (divide) from / (regex start).
import arc/parser/error.{type ParseError, LexerError}
import gleam/bit_array
import gleam/int
import gleam/list
import gleam/result
import gleam/string

// ---- Source byte access ----

/// O(1) character access into the source bytes (ASCII only, for regex scanning).
pub fn char_at_source(bytes: BitArray, pos: Int) -> String {
  case bit_array.slice(bytes, pos, 1) {
    Error(_) -> ""
    Ok(<<byte>>) if byte < 0x80 -> {
      case bit_array.to_string(<<byte>>) {
        Ok(s) -> s
        Error(_) -> ""
      }
    }
    _ -> ""
  }
}

/// O(1) byte slice from the source bytes.
pub fn byte_slice_source(bytes: BitArray, start: Int, len: Int) -> String {
  case bit_array.slice(bytes, start, len) {
    Ok(s) ->
      case bit_array.to_string(s) {
        Ok(str) -> str
        Error(_) -> ""
      }
    Error(_) -> ""
  }
}

/// Check if a single character is a hex digit (0-9, a-f, A-F).
fn is_hex_char(ch: String) -> Bool {
  case ch {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    "a" | "b" | "c" | "d" | "e" | "f" -> True
    "A" | "B" | "C" | "D" | "E" | "F" -> True
    _ -> False
  }
}

/// Skip consecutive hex digits in the source bytes starting at pos,
/// returning the position after the last hex digit.
fn skip_regex_hex_run(bytes: BitArray, pos: Int, end: Int) -> Int {
  case pos >= end {
    True -> pos
    False ->
      case is_hex_char(char_at_source(bytes, pos)) {
        True -> skip_regex_hex_run(bytes, pos + 1, end)
        False -> pos
      }
  }
}

// ---- Regex body scanning ----

/// Scan regex source from just after the opening /, returning the position
/// just past the closing /. Handles escapes and character classes.
pub fn scan_regex_source(
  bytes: BitArray,
  pos: Int,
  in_class: Bool,
) -> Result(Int, String) {
  let ch = char_at_source(bytes, pos)
  case ch {
    "" -> Error("Unterminated regular expression")
    "\n" | "\r" -> Error("Unterminated regular expression")
    "\\" -> {
      // Escaped character — skip next char
      let next = char_at_source(bytes, pos + 1)
      case next {
        "" | "\n" | "\r" -> Error("Unterminated regular expression")
        _ -> scan_regex_source(bytes, pos + 2, in_class)
      }
    }
    "[" -> scan_regex_source(bytes, pos + 1, True)
    "]" ->
      case in_class {
        True -> scan_regex_source(bytes, pos + 1, False)
        False -> scan_regex_source(bytes, pos + 1, in_class)
      }
    "/" ->
      case in_class {
        True -> scan_regex_source(bytes, pos + 1, in_class)
        False -> Ok(pos + 1)
      }
    _ -> scan_regex_source(bytes, pos + 1, in_class)
  }
}

/// Scan regex flags after the closing /, returning end position and flag list.
/// Rejects duplicate flags.
pub fn skip_regex_flags(
  bytes: BitArray,
  pos: Int,
) -> Result(#(Int, List(String)), ParseError) {
  scan_regex_flags(bytes, pos, [])
}

fn scan_regex_flags(
  bytes: BitArray,
  pos: Int,
  seen: List(String),
) -> Result(#(Int, List(String)), ParseError) {
  let ch = char_at_source(bytes, pos)
  case ch {
    "g" | "i" | "m" | "s" | "u" | "v" | "y" | "d" ->
      case list.contains(seen, ch) {
        True ->
          Error(LexerError(
            "Duplicate regular expression flag '" <> ch <> "'",
            pos,
          ))
        False -> scan_regex_flags(bytes, pos + 1, [ch, ..seen])
      }
    _ -> Ok(#(pos, seen))
  }
}

/// Split a pre-tokenized regex value like "/pattern/flags" into (pattern, flags).
pub fn split_regex_value(raw: String) -> #(String, String) {
  // Strip leading /
  let body = string.drop_start(raw, 1)
  // Split on "/" and rejoin all but the last segment as the pattern
  let parts = string.split(body, "/")
  case parts {
    [single] -> #(single, "")
    _ -> {
      let assert Ok(flags) = list.last(parts)
      let pattern =
        parts
        |> list.take(list.length(parts) - 1)
        |> string.join("/")
      #(pattern, flags)
    }
  }
}

// ---- Unicode mode (/u flag) validation ----

/// Validate regex body with the /u flag.
/// In Unicode mode:
/// 1. `{` must start a valid quantifier ({n}, {n,}, {n,m}) and `}` must close one
/// 2. Quantifiers (*, +, ?, {n,m}) cannot follow assertion groups (?=, ?!, ?<=, ?<!)
/// Braces inside character classes [...] are allowed.
pub fn validate_regex_unicode_body(
  bytes: BitArray,
  pos: Int,
  end: Int,
) -> Result(Nil, String) {
  validate_regex_unicode_loop(bytes, pos, end, False, False)
}

fn validate_regex_unicode_loop(
  bytes: BitArray,
  pos: Int,
  end: Int,
  in_class: Bool,
  after_assertion: Bool,
) -> Result(Nil, String) {
  case pos >= end {
    True -> Ok(Nil)
    False -> {
      let ch = char_at_source(bytes, pos)
      case ch {
        "\\" -> {
          // Skip escaped character — not a quantifier, not an assertion.
          // For \u escapes, skip the full escape sequence length,
          // not just 2 chars, otherwise \u{XXXX} leaves the { exposed
          // and the validator incorrectly rejects it as a lone brace.
          let escape_result = case char_at_source(bytes, pos + 1) {
            "u" ->
              case char_at_source(bytes, pos + 2) {
                "{" -> {
                  // \u{...} — find the closing }, validate codepoint value
                  let after = skip_regex_hex_run(bytes, pos + 3, end)
                  case char_at_source(bytes, after) {
                    "}" -> {
                      // Validate the hex value is <= 0x10FFFF
                      let hex_str =
                        byte_slice_source(bytes, pos + 3, after - { pos + 3 })
                      case parse_hex_value(hex_str) {
                        Ok(val) ->
                          case val > 0x10FFFF {
                            True ->
                              Error(
                                "Invalid regular expression: Unicode escape value > 0x10FFFF",
                              )
                            False -> Ok(after + 1 - pos)
                          }
                        Error(_) -> Ok(2)
                      }
                    }
                    _ -> Ok(2)
                  }
                }
                _ -> {
                  // \uXXXX — check for 4 hex digits
                  case
                    pos + 6 <= end
                    && is_hex_char(char_at_source(bytes, pos + 2))
                    && is_hex_char(char_at_source(bytes, pos + 3))
                    && is_hex_char(char_at_source(bytes, pos + 4))
                    && is_hex_char(char_at_source(bytes, pos + 5))
                  {
                    True -> Ok(6)
                    False -> Ok(2)
                  }
                }
              }
            "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
              // Backreferences (\1-\9) are not allowed in Unicode mode
              Error(
                "Invalid regular expression: decimal escape in Unicode mode",
              )
            _ -> Ok(2)
          }
          use escape_len <- result.try(escape_result)
          validate_regex_unicode_loop(
            bytes,
            pos + escape_len,
            end,
            in_class,
            False,
          )
        }
        "[" -> validate_regex_unicode_loop(bytes, pos + 1, end, True, False)
        "]" ->
          case in_class {
            True ->
              validate_regex_unicode_loop(bytes, pos + 1, end, False, False)
            False ->
              validate_regex_unicode_loop(bytes, pos + 1, end, in_class, False)
          }
        "(" ->
          case in_class {
            True ->
              validate_regex_unicode_loop(bytes, pos + 1, end, in_class, False)
            False -> {
              // Check if this is an assertion group: (?=, (?!, (?<=, (?<!
              let is_assertion = is_assertion_group(bytes, pos + 1, end)
              case is_assertion {
                True -> {
                  // Find matching closing paren, then mark after_assertion
                  case find_matching_paren(bytes, pos + 1, end) {
                    Ok(close_pos) ->
                      validate_regex_unicode_loop(
                        bytes,
                        close_pos + 1,
                        end,
                        in_class,
                        True,
                      )
                    // If we can't find matching paren, just continue normally
                    // (the error will be caught elsewhere)
                    Error(_) ->
                      validate_regex_unicode_loop(
                        bytes,
                        pos + 1,
                        end,
                        in_class,
                        False,
                      )
                  }
                }
                False -> {
                  // Regular group — find matching close and mark NOT assertion
                  case find_matching_paren(bytes, pos + 1, end) {
                    Ok(close_pos) ->
                      validate_regex_unicode_loop(
                        bytes,
                        close_pos + 1,
                        end,
                        in_class,
                        False,
                      )
                    Error(_) ->
                      validate_regex_unicode_loop(
                        bytes,
                        pos + 1,
                        end,
                        in_class,
                        False,
                      )
                  }
                }
              }
            }
          }
        // Quantifiers: *, +, ?
        "*" | "+" ->
          case in_class {
            True ->
              validate_regex_unicode_loop(bytes, pos + 1, end, in_class, False)
            False ->
              case after_assertion {
                True ->
                  Error(
                    "Invalid regular expression: quantifier on assertion in Unicode mode",
                  )
                False -> {
                  // Skip optional ? for lazy quantifier
                  let next_pos = case char_at_source(bytes, pos + 1) {
                    "?" -> pos + 2
                    _ -> pos + 1
                  }
                  validate_regex_unicode_loop(
                    bytes,
                    next_pos,
                    end,
                    in_class,
                    False,
                  )
                }
              }
          }
        "?" ->
          case in_class {
            True ->
              validate_regex_unicode_loop(bytes, pos + 1, end, in_class, False)
            False ->
              case after_assertion {
                True ->
                  Error(
                    "Invalid regular expression: quantifier on assertion in Unicode mode",
                  )
                False -> {
                  // ? as quantifier — skip optional ? for lazy
                  let next_pos = case char_at_source(bytes, pos + 1) {
                    "?" -> pos + 2
                    _ -> pos + 1
                  }
                  validate_regex_unicode_loop(
                    bytes,
                    next_pos,
                    end,
                    in_class,
                    False,
                  )
                }
              }
          }
        "{" ->
          case in_class {
            True ->
              validate_regex_unicode_loop(bytes, pos + 1, end, in_class, False)
            False ->
              // Try to parse as valid quantifier: {digits}, {digits,}, {digits,digits}
              case try_parse_quantifier_brace(bytes, pos + 1, end) {
                Ok(after_brace) ->
                  case after_assertion {
                    True ->
                      Error(
                        "Invalid regular expression: quantifier on assertion in Unicode mode",
                      )
                    False -> {
                      // Skip optional ? for lazy quantifier
                      let next_pos = case char_at_source(bytes, after_brace) {
                        "?" -> after_brace + 1
                        _ -> after_brace
                      }
                      validate_regex_unicode_loop(
                        bytes,
                        next_pos,
                        end,
                        in_class,
                        False,
                      )
                    }
                  }
                Error(_) ->
                  Error("Invalid regular expression: lone '{' in Unicode mode")
              }
          }
        "}" ->
          case in_class {
            True ->
              validate_regex_unicode_loop(bytes, pos + 1, end, in_class, False)
            False ->
              Error("Invalid regular expression: lone '}' in Unicode mode")
          }
        // Any other character resets after_assertion
        _ -> validate_regex_unicode_loop(bytes, pos + 1, end, in_class, False)
      }
    }
  }
}

/// Parse a hex string into an integer value.
fn parse_hex_value(hex_str: String) -> Result(Int, Nil) {
  int.base_parse(hex_str, 16)
}

/// Check if position starts an assertion group: ?=, ?!, ?<=, ?<!
fn is_assertion_group(bytes: BitArray, pos: Int, end: Int) -> Bool {
  case pos >= end {
    True -> False
    False -> {
      let ch = char_at_source(bytes, pos)
      case ch {
        "?" -> {
          let ch2 = char_at_source(bytes, pos + 1)
          case ch2 {
            "=" | "!" -> True
            "<" -> {
              let ch3 = char_at_source(bytes, pos + 2)
              case ch3 {
                "=" | "!" -> True
                _ -> False
              }
            }
            _ -> False
          }
        }
        _ -> False
      }
    }
  }
}

/// Find the matching closing paren for a group, accounting for nesting,
/// character classes, and escapes. pos starts inside the group (after opening `(`).
fn find_matching_paren(
  bytes: BitArray,
  pos: Int,
  end: Int,
) -> Result(Int, Nil) {
  find_matching_paren_loop(bytes, pos, end, 1, False)
}

fn find_matching_paren_loop(
  bytes: BitArray,
  pos: Int,
  end: Int,
  depth: Int,
  in_class: Bool,
) -> Result(Int, Nil) {
  case pos >= end {
    True -> Error(Nil)
    False -> {
      let ch = char_at_source(bytes, pos)
      case ch {
        "\\" ->
          // Skip escaped char
          find_matching_paren_loop(bytes, pos + 2, end, depth, in_class)
        "[" -> find_matching_paren_loop(bytes, pos + 1, end, depth, True)
        "]" ->
          case in_class {
            True -> find_matching_paren_loop(bytes, pos + 1, end, depth, False)
            False ->
              find_matching_paren_loop(bytes, pos + 1, end, depth, in_class)
          }
        "(" ->
          case in_class {
            True ->
              find_matching_paren_loop(bytes, pos + 1, end, depth, in_class)
            False ->
              find_matching_paren_loop(bytes, pos + 1, end, depth + 1, in_class)
          }
        ")" ->
          case in_class {
            True ->
              find_matching_paren_loop(bytes, pos + 1, end, depth, in_class)
            False ->
              case depth <= 1 {
                True -> Ok(pos)
                False ->
                  find_matching_paren_loop(
                    bytes,
                    pos + 1,
                    end,
                    depth - 1,
                    in_class,
                  )
              }
          }
        _ -> find_matching_paren_loop(bytes, pos + 1, end, depth, in_class)
      }
    }
  }
}

/// Try to parse a valid quantifier starting after the opening `{`.
/// Valid forms: {n}, {n,}, {n,m} where n and m are decimal digits.
/// Returns the position after the closing `}` on success.
fn try_parse_quantifier_brace(
  bytes: BitArray,
  pos: Int,
  end: Int,
) -> Result(Int, Nil) {
  // Must start with at least one digit
  case skip_digits(bytes, pos, end) {
    Ok(after_digits) ->
      case after_digits == pos {
        // No digits found — not a valid quantifier
        True -> Error(Nil)
        False -> {
          let ch = char_at_source(bytes, after_digits)
          case ch {
            "}" ->
              // {n} form
              Ok(after_digits + 1)
            "," -> {
              // Could be {n,} or {n,m}
              let after_comma = after_digits + 1
              case skip_digits(bytes, after_comma, end) {
                Ok(after_digits2) -> {
                  let ch2 = char_at_source(bytes, after_digits2)
                  case ch2 {
                    "}" ->
                      // {n,} or {n,m} form
                      Ok(after_digits2 + 1)
                    _ -> Error(Nil)
                  }
                }
                Error(_) -> Error(Nil)
              }
            }
            _ -> Error(Nil)
          }
        }
      }
    Error(_) -> Error(Nil)
  }
}

/// Skip decimal digits from pos, returning the position after the last digit.
/// Always succeeds (returns pos unchanged if no digits found).
fn skip_digits(bytes: BitArray, pos: Int, end: Int) -> Result(Int, Nil) {
  case pos >= end {
    True -> Ok(pos)
    False -> {
      let ch = char_at_source(bytes, pos)
      case ch {
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
          skip_digits(bytes, pos + 1, end)
        _ -> Ok(pos)
      }
    }
  }
}
