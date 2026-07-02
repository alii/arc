/// Template literal string splitting.
/// Pure string functions split from parser.gleam. The parser still owns
/// parse_template_raw since that calls back into parse_expression.
import gleam/list
import gleam/string

/// Why a template's inner text could not be split into quasis and
/// substitution sources.
pub type TemplateSplitError {
  /// A `${` substitution never reached its closing `}` — e.g. the text
  /// inside it ends in an unterminated string or nested template literal.
  /// The parser reports this as a SyntaxError at the template's position.
  UnterminatedSubstitution
}

/// Split template inner text (without backticks) into RAW quasi strings and
/// expression source strings. Tracks brace depth for nested `{}`.
///
/// Quasis are returned verbatim (escape sequences NOT decoded) so the caller
/// can build both the raw strings (tagged templates' `.raw`) and the cooked
/// values (via the parser's cook_template_string FFI). Line endings are
/// normalized to LF per the spec's TRV definition (§12.9.6: <CR><LF> and
/// <CR> → <LF>).
pub fn split_template_parts(
  inner: String,
) -> Result(#(List(String), List(String)), TemplateSplitError) {
  let graphemes = string.to_graphemes(inner)
  do_split_template(graphemes, "", [], [], 0, False)
}

/// Normalize a single grapheme's line ending per TRV: CRLF / CR → LF.
/// (CRLF is a single grapheme cluster, so it arrives as one list element.)
fn normalize_line_ending(ch: String) -> String {
  case ch {
    "\r\n" | "\r" -> "\n"
    _ -> ch
  }
}

fn do_split_template(
  chars: List(String),
  current_quasi: String,
  quasis: List(String),
  expr_sources: List(String),
  brace_depth: Int,
  in_expr: Bool,
) -> Result(#(List(String), List(String)), TemplateSplitError) {
  case chars, in_expr {
    // End of input
    [], False ->
      Ok(#(list.reverse([current_quasi, ..quasis]), list.reverse(expr_sources)))
    // A `${` whose `}` was never found (e.g. it swallowed the rest of the
    // template via an unterminated string or nested template literal).
    [], True -> Error(UnterminatedSubstitution)
    // In quasi mode: look for ${ to start an expression
    ["\\", next, ..rest], False ->
      // Escaped character in quasi — keep verbatim (raw), so `\${` and
      // `\\` don't confuse the `${` scan. Line continuations keep the
      // backslash; the cook step drops them.
      do_split_template(
        rest,
        current_quasi <> "\\" <> normalize_line_ending(next),
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
        current_quasi <> normalize_line_ending(ch),
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
    // A nested template literal inside the expression: collect it verbatim
    // so its own `}` / `${` / quoted text never affect the outer brace
    // depth (`` `${`}`}` `` must not end the substitution at the inner `}`).
    ["`", ..rest], True -> {
      let #(tpl, remaining) = collect_template_in_expr(rest, "`", 0, False)
      do_split_template(
        remaining,
        current_quasi <> tpl,
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

/// Collect an entire nested template literal — the opening backtick is
/// already in `acc` — verbatim into the enclosing expression source,
/// tracking its own escape / `${…}` / backtick state (symmetric to
/// collect_string_in_expr). `in_sub` is True while inside one of the nested
/// template's own `${…}` substitutions, whose brace depth is tracked
/// independently of the caller's; templates nested inside such a
/// substitution recurse. If the nested template is unterminated the whole
/// remaining input is consumed and the caller's expression scan reports
/// UnterminatedSubstitution.
fn collect_template_in_expr(
  chars: List(String),
  acc: String,
  brace_depth: Int,
  in_sub: Bool,
) -> #(String, List(String)) {
  case chars, in_sub {
    [], _ -> #(acc, [])
    // Quasi text of the nested template
    ["\\", next, ..rest], False ->
      collect_template_in_expr(rest, acc <> "\\" <> next, brace_depth, False)
    ["`", ..rest], False -> #(acc <> "`", rest)
    ["$", "{", ..rest], False ->
      collect_template_in_expr(rest, acc <> "${", 0, True)
    [ch, ..rest], False ->
      collect_template_in_expr(rest, acc <> ch, brace_depth, False)
    // Inside one of the nested template's `${…}` substitutions
    ["}", ..rest], True ->
      case brace_depth {
        0 -> collect_template_in_expr(rest, acc <> "}", 0, False)
        _ -> collect_template_in_expr(rest, acc <> "}", brace_depth - 1, True)
      }
    ["{", ..rest], True ->
      collect_template_in_expr(rest, acc <> "{", brace_depth + 1, True)
    ["\"", ..rest], True -> {
      let #(str_content, remaining) = collect_string_in_expr(rest, "\"", "\"")
      collect_template_in_expr(remaining, acc <> str_content, brace_depth, True)
    }
    ["'", ..rest], True -> {
      let #(str_content, remaining) = collect_string_in_expr(rest, "'", "'")
      collect_template_in_expr(remaining, acc <> str_content, brace_depth, True)
    }
    ["`", ..rest], True -> {
      let #(nested, remaining) = collect_template_in_expr(rest, "`", 0, False)
      collect_template_in_expr(remaining, acc <> nested, brace_depth, True)
    }
    [ch, ..rest], True ->
      collect_template_in_expr(rest, acc <> ch, brace_depth, True)
  }
}
