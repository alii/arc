/// Annex B legacy-octal detection for strict-mode early errors.
///
/// `0123`-style NumericLiterals (ES2024 §B.1.1) and `\07`-style octal
/// escapes in string literals (§B.1.2) are legacy forms that are early
/// SyntaxErrors in strict mode (§12.9.3.1 / §12.9.4.1). The parser calls
/// these predicates on the RAW token text of number and string literals
/// (and directive prologues) when the surrounding code is strict.
import gleam/string

/// Check if a number literal value is a legacy octal (e.g. 0123, 09)
pub fn is_legacy_octal_number(value: String) -> Bool {
  case string.first(value) {
    Ok("0") ->
      case string.slice(value, 1, 1) {
        // 0x, 0o, 0b, 0X, 0O, 0B are modern prefixed forms
        "x" | "o" | "b" | "X" | "O" | "B" -> False
        // 0. or 0e/0E are decimal floats
        "." | "e" | "E" -> False
        // 0n is BigInt 0
        "n" -> False
        // Just "0" is fine
        "" -> False
        // 0 followed by digit is legacy octal (01, 07, 08, 09 etc.)
        _ -> string.length(value) > 1
      }
    _ -> False
  }
}

/// Check if a string literal value contains legacy octal escapes (\0-\7 followed by more)
pub fn has_legacy_octal_escape(value: String) -> Bool {
  check_string_escapes(value, 0, string.length(value))
}

fn check_string_escapes(s: String, pos: Int, end: Int) -> Bool {
  case pos >= end {
    True -> False
    False ->
      case string.slice(s, pos, 1) {
        "\\" ->
          case string.slice(s, pos + 1, 1) {
            "0" ->
              // \0 followed by a digit is legacy octal
              case string.slice(s, pos + 2, 1) {
                "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
                  True
                _ -> check_string_escapes(s, pos + 2, end)
              }
            "1" | "2" | "3" | "4" | "5" | "6" | "7" -> True
            "x" -> check_string_escapes(s, pos + 4, end)
            "u" ->
              case string.slice(s, pos + 2, 1) {
                "{" -> {
                  // Skip to closing brace
                  skip_to_brace(s, pos + 3, end)
                }
                _ -> check_string_escapes(s, pos + 6, end)
              }
            _ -> check_string_escapes(s, pos + 2, end)
          }
        _ -> check_string_escapes(s, pos + 1, end)
      }
  }
}

fn skip_to_brace(s: String, pos: Int, end: Int) -> Bool {
  case pos >= end {
    True -> False
    False ->
      case string.slice(s, pos, 1) {
        "}" -> check_string_escapes(s, pos + 1, end)
        _ -> skip_to_brace(s, pos + 1, end)
      }
  }
}
