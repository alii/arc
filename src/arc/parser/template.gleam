/// Template literal string splitting.
/// Pure string functions split from parser.gleam. The parser still owns
/// parse_template_raw since that calls back into parse_expression.
import gleam/list
import gleam/option.{type Option, None, Some}
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
    // `//` and `/* */` comments inside the expression: consume them
    // verbatim so quotes / braces / backticks in the comment text never
    // affect the scan. Checked before the regex / division split — `//`
    // is a comment in any position.
    ["/", "/", ..rest], True -> {
      let #(comment, remaining) = collect_line_comment(rest, "//")
      do_split_template(
        remaining,
        current_quasi <> comment,
        quasis,
        expr_sources,
        brace_depth,
        True,
      )
    }
    ["/", "*", ..rest], True -> {
      let #(comment, remaining) = collect_block_comment(rest, "/*")
      do_split_template(
        remaining,
        current_quasi <> comment,
        quasis,
        expr_sources,
        brace_depth,
        True,
      )
    }
    // A regex literal inside the expression: collect it verbatim so quotes,
    // braces, and backticks inside the pattern (`/'/g`, `/{/`) never open a
    // phantom string or change the brace depth. `/` only heads a regex when
    // the last significant character scanned leaves us in operator position
    // (`(`, `,`, `=`, a keyword, …); after an operand it is division and
    // needs no special handling.
    ["/", ..rest], True ->
      case regex_at_slash(current_quasi, rest) {
        Some(#(regex, remaining)) ->
          do_split_template(
            remaining,
            current_quasi <> regex,
            quasis,
            expr_sources,
            brace_depth,
            True,
          )
        // Division (or no closing `/` on this line): a plain character.
        None ->
          do_split_template(
            rest,
            current_quasi <> "/",
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

/// The regex literal starting at a `/` in a substitution's expression source
/// (`scanned` is the source already consumed, `rest` what follows the `/`),
/// or None when the `/` is a division operator or no regex terminates on
/// this line.
fn regex_at_slash(
  scanned: String,
  rest: List(String),
) -> Option(#(String, List(String))) {
  case regex_can_follow(scanned) {
    True -> collect_regex_in_expr(rest, "/", False)
    False -> None
  }
}

/// True when a `/` at this point of a substitution's source starts a regex
/// literal rather than a division operator. Standard last-significant-token
/// heuristic: at the start of the expression, after a punctuator that cannot
/// end an operand, or after an operator keyword (`typeof`, `return`, …) a
/// regex can start; after an operand (identifier, number, `)`, `]`, `}`,
/// string, template) `/` is division.
fn regex_can_follow(scanned: String) -> Bool {
  case last_significant(string.to_graphemes(scanned) |> list.reverse) {
    None -> True
    Some(#(ch, before)) ->
      case ch {
        // `x++ / y` — after a POSTFIX increment/decrement, `/` is division.
        // A prefix ++/-- can only be followed by an operand (never a regex
        // literal), so a trailing ++/-- always means operand position.
        "+" | "-" ->
          case before {
            [prev, ..] if prev == ch -> False
            _ -> True
          }
        "("
        | ","
        | "="
        | ":"
        | "["
        | "!"
        | "&"
        | "|"
        | "?"
        | "{"
        | ";"
        | "*"
        | "%"
        | "^"
        | "~"
        | "<"
        | ">" -> True
        _ ->
          case is_ident_grapheme(ch) {
            // A trailing identifier is an operand unless it is a keyword
            // that expects an operand after it (`typeof /re/.test(s)`) —
            // and even a keyword is an operand when it is a PROPERTY NAME
            // (`x.of / 2`).
            True -> {
              let #(word, before_word) = trailing_word(before, ch)
              case is_regex_head_keyword(word) {
                False -> False
                True ->
                  case last_significant(before_word) {
                    Some(#(".", _)) -> False
                    _ -> True
                  }
              }
            }
            False -> False
          }
      }
  }
}

/// Last non-whitespace grapheme of the (already reversed) scanned text,
/// together with the reversed text that precedes it. A trailing `/* … */`
/// block comment is skipped like whitespace, so a comment between an
/// operator and a `/` stays transparent (`` `${a = /*sep*/ /re/}` ``).
fn last_significant(reversed: List(String)) -> Option(#(String, List(String))) {
  case reversed {
    [] -> None
    ["/", "*", ..rest] -> last_significant(skip_reversed_block_comment(rest))
    [ch, ..rest] ->
      case is_ws_grapheme(ch) {
        True -> last_significant(rest)
        False -> Some(#(ch, rest))
      }
  }
}

/// Drop reversed graphemes up to and including the `/*` that opened the
/// block comment whose closing `*/` the caller just consumed.
fn skip_reversed_block_comment(reversed: List(String)) -> List(String) {
  case reversed {
    [] -> []
    ["*", "/", ..rest] -> rest
    [_, ..rest] -> skip_reversed_block_comment(rest)
  }
}

/// ES WhiteSpace (§12.2) + LineTerminator, as graphemes (CRLF arrives as
/// one grapheme cluster). Mirrors the lexer's whitespace set.
fn is_ws_grapheme(ch: String) -> Bool {
  case ch {
    " "
    | "\t"
    | "\n"
    | "\r"
    | "\r\n"
    | "\u{000B}"
    | "\u{000C}"
    | "\u{00A0}"
    | "\u{1680}"
    | "\u{2000}"
    | "\u{2001}"
    | "\u{2002}"
    | "\u{2003}"
    | "\u{2004}"
    | "\u{2005}"
    | "\u{2006}"
    | "\u{2007}"
    | "\u{2008}"
    | "\u{2009}"
    | "\u{200A}"
    | "\u{2028}"
    | "\u{2029}"
    | "\u{202F}"
    | "\u{205F}"
    | "\u{3000}"
    | "\u{FEFF}" -> True
    _ -> False
  }
}

/// The identifier ending at `last` (whose preceding source, reversed, is
/// `reversed_before`), e.g. scanning `a + typeof` back from `f` gives
/// #("typeof", reversed text before the word).
fn trailing_word(
  reversed_before: List(String),
  last: String,
) -> #(String, List(String)) {
  case reversed_before {
    [ch, ..rest] ->
      case is_ident_grapheme(ch) {
        True -> trailing_word(rest, ch <> last)
        False -> #(last, reversed_before)
      }
    [] -> #(last, [])
  }
}

/// RESERVED words after which a `/` starts a regex literal, not a division.
/// Contextual keywords that are also plain identifiers (`of`, `async`, …)
/// are deliberately absent: `of / 2` is division.
fn is_regex_head_keyword(word: String) -> Bool {
  case word {
    "return"
    | "typeof"
    | "instanceof"
    | "in"
    | "new"
    | "delete"
    | "void"
    | "case"
    | "do"
    | "else"
    | "yield"
    | "await"
    | "throw" -> True
    _ -> False
  }
}

fn is_ident_grapheme(ch: String) -> Bool {
  string.contains(
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_$",
    ch,
  )
}

/// Collect a regex literal (leading `/` already in `acc`) verbatim: body up
/// to the unescaped closing `/` outside a `[...]` character class, then its
/// flags. Returns None — the `/` was not a regex head after all — if a line
/// terminator or the end of the substitution text arrives first, since a
/// regex literal cannot span lines.
fn collect_regex_in_expr(
  chars: List(String),
  acc: String,
  in_class: Bool,
) -> Option(#(String, List(String))) {
  case chars {
    [] -> None
    ["\n", ..]
    | ["\r", ..]
    | ["\r\n", ..]
    | ["\u{2028}", ..]
    | ["\u{2029}", ..] -> None
    // A line terminator is not allowed in a regex body even escaped —
    // agrees with regex.scan_regex_source.
    ["\\", "\n", ..] | ["\\", "\r", ..] | ["\\", "\r\n", ..] -> None
    ["\\", "\u{2028}", ..] | ["\\", "\u{2029}", ..] -> None
    ["\\", next, ..rest] ->
      collect_regex_in_expr(rest, acc <> "\\" <> next, in_class)
    ["[", ..rest] -> collect_regex_in_expr(rest, acc <> "[", True)
    ["]", ..rest] if in_class -> collect_regex_in_expr(rest, acc <> "]", False)
    ["/", ..rest] if !in_class -> Some(collect_regex_flags(rest, acc <> "/"))
    [ch, ..rest] -> collect_regex_in_expr(rest, acc <> ch, in_class)
  }
}

/// Collect a `//` line comment verbatim (opener already in `acc`) up to,
/// but not including, the terminating line terminator.
fn collect_line_comment(
  chars: List(String),
  acc: String,
) -> #(String, List(String)) {
  case chars {
    [] -> #(acc, [])
    ["\n", ..]
    | ["\r", ..]
    | ["\r\n", ..]
    | ["\u{2028}", ..]
    | ["\u{2029}", ..] -> #(acc, chars)
    [ch, ..rest] -> collect_line_comment(rest, acc <> ch)
  }
}

/// Collect a `/* … */` block comment verbatim (opener already in `acc`),
/// through its closing `*/`. An unterminated comment consumes the rest of
/// the template text, so the enclosing substitution reports
/// UnterminatedSubstitution.
fn collect_block_comment(
  chars: List(String),
  acc: String,
) -> #(String, List(String)) {
  case chars {
    [] -> #(acc, [])
    ["*", "/", ..rest] -> #(acc <> "*/", rest)
    [ch, ..rest] -> collect_block_comment(rest, acc <> ch)
  }
}

fn collect_regex_flags(
  chars: List(String),
  acc: String,
) -> #(String, List(String)) {
  case chars {
    [ch, ..rest] -> {
      case is_ident_grapheme(ch) {
        True -> collect_regex_flags(rest, acc <> ch)
        False -> #(acc, chars)
      }
    }
    [] -> #(acc, [])
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
    // Comments and regex literals inside the nested template's
    // substitution — same reasoning as in do_split_template.
    ["/", "/", ..rest], True -> {
      let #(comment, remaining) = collect_line_comment(rest, "//")
      collect_template_in_expr(remaining, acc <> comment, brace_depth, True)
    }
    ["/", "*", ..rest], True -> {
      let #(comment, remaining) = collect_block_comment(rest, "/*")
      collect_template_in_expr(remaining, acc <> comment, brace_depth, True)
    }
    ["/", ..rest], True ->
      case regex_at_slash(acc, rest) {
        Some(#(regex, remaining)) ->
          collect_template_in_expr(remaining, acc <> regex, brace_depth, True)
        None -> collect_template_in_expr(rest, acc <> "/", brace_depth, True)
      }
    [ch, ..rest], True ->
      collect_template_in_expr(rest, acc <> ch, brace_depth, True)
  }
}
