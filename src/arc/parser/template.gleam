/// Template literal string splitting and escape processing.
/// Pure string functions split from parser.gleam. The parser still owns
/// parse_template_raw since that calls back into parse_expression.
import gleam/list
import gleam/string

/// Split template inner text (without backticks) into quasi strings and
/// expression source strings. Tracks brace depth for nested `{}`.
pub fn split_template_parts(inner: String) -> #(List(String), List(String)) {
  let graphemes = string.to_graphemes(inner)
  do_split_template(graphemes, "", [], [], 0, False)
}

fn do_split_template(
  chars: List(String),
  current_quasi: String,
  quasis: List(String),
  expr_sources: List(String),
  brace_depth: Int,
  in_expr: Bool,
) -> #(List(String), List(String)) {
  case chars, in_expr {
    // End of input
    [], False -> #(
      list.reverse([current_quasi, ..quasis]),
      list.reverse(expr_sources),
    )
    [], True ->
      // Unterminated expression (shouldn't happen if lexer is correct)
      #(list.reverse([current_quasi, ..quasis]), list.reverse(expr_sources))
    // In quasi mode: look for ${ to start an expression
    ["\\", next, ..rest], False ->
      // Escaped character in quasi — include both chars, process escape
      do_split_template(
        rest,
        current_quasi <> process_template_escape(next),
        quasis,
        expr_sources,
        brace_depth,
        False,
      )
    ["$", "{", ..rest], False ->
      // Start of expression: save current quasi, begin expression collection
      do_split_template(
        rest,
        "",
        [current_quasi, ..quasis],
        expr_sources,
        0,
        True,
      )
    [ch, ..rest], False ->
      do_split_template(
        rest,
        current_quasi <> ch,
        quasis,
        expr_sources,
        brace_depth,
        False,
      )
    // In expression mode: track brace depth, look for closing }
    ["}", ..rest], True ->
      case brace_depth {
        0 ->
          // End of expression
          do_split_template(
            rest,
            "",
            quasis,
            [current_quasi, ..expr_sources],
            0,
            False,
          )
        _ ->
          do_split_template(
            rest,
            current_quasi <> "}",
            quasis,
            expr_sources,
            brace_depth - 1,
            True,
          )
      }
    ["{", ..rest], True ->
      do_split_template(
        rest,
        current_quasi <> "{",
        quasis,
        expr_sources,
        brace_depth + 1,
        True,
      )
    // Handle string literals inside expressions (to avoid counting braces in strings)
    ["\"", ..rest], True -> {
      let #(str_content, remaining) = collect_string_in_expr(rest, "\"", "\"")
      do_split_template(
        remaining,
        current_quasi <> str_content,
        quasis,
        expr_sources,
        brace_depth,
        True,
      )
    }
    ["'", ..rest], True -> {
      let #(str_content, remaining) = collect_string_in_expr(rest, "'", "'")
      do_split_template(
        remaining,
        current_quasi <> str_content,
        quasis,
        expr_sources,
        brace_depth,
        True,
      )
    }
    [ch, ..rest], True ->
      do_split_template(
        rest,
        current_quasi <> ch,
        quasis,
        expr_sources,
        brace_depth,
        True,
      )
  }
}

/// Collect characters inside a string literal in a template expression,
/// preserving escapes and tracking the closing quote.
fn collect_string_in_expr(
  chars: List(String),
  quote: String,
  acc: String,
) -> #(String, List(String)) {
  case chars {
    [] -> #(acc, [])
    ["\\", next, ..rest] ->
      collect_string_in_expr(rest, quote, acc <> "\\" <> next)
    [ch, ..rest] if ch == quote -> #(acc <> ch, rest)
    [ch, ..rest] -> collect_string_in_expr(rest, quote, acc <> ch)
  }
}

/// Process a single escape sequence in a template quasi.
fn process_template_escape(ch: String) -> String {
  case ch {
    "n" -> "\n"
    "t" -> "\t"
    "r" -> "\r"
    "\\" -> "\\"
    "`" -> "`"
    "$" -> "$"
    "0" -> "\u{0000}"
    _ -> ch
  }
}

// ---- Legacy octal detection (strict mode checks) ----

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
