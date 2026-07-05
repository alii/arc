/// Regex literal scanning and /u flag validation.
/// Pure bit-array scanning functions split from parser.gleam.
/// The parser re-lexes regex literals from source bytes since the lexer
/// can't always distinguish / (divide) from / (regex start).
import arc/internal/digits
import arc/internal/utf16
import arc/parser/lexer
import arc/parser/source_bytes.{ascii_at}
import gleam/bit_array
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

// ---- Pattern errors ----

/// Every syntax error the regex scanner / pattern validator can report.
/// One variant per distinct diagnostic; `pattern_error_message` is the single
/// place each is rendered to its user-facing string, and `pattern_error_pos`
/// gives the byte offset of the offending construct in the scanned source
/// (absolute for a regex literal, pattern-relative for `new RegExp(src)`).
///
/// NOTE: this module must not import `arc/parser/error` — `error.gleam`
/// wraps `PatternError` in its `RegExpSyntaxError` variant, so an import
/// here would be a cycle.
pub type PatternError {
  /// Source ended, or a line terminator appeared, before the closing `/`.
  UnterminatedRegex(pos: Int)
  /// A flag letter appeared twice after the closing `/`.
  DuplicateFlag(pos: Int, flag: String)
  /// A character outside the flag alphabet in a `new RegExp(_, flags)` string.
  InvalidFlag(pos: Int, flag: String)
  /// The `u` and `v` flags were both present.
  ExclusiveUnicodeFlags(pos: Int)
  /// A `)` with no matching `(` ended the top-level Disjunction early.
  UnmatchedParen(pos: Int)
  /// A quantifier with no quantifiable atom before it.
  NothingToRepeat(pos: Int)
  /// The same group name declared twice in terms that can both participate
  /// in a match.
  DuplicateGroupName(pos: Int, name: String)
  /// A lone `{` / `}` outside a braced quantifier (Unicode modes only).
  LoneQuantifierBrackets(pos: Int)
  /// A lone `]` outside a character class (Unicode modes only).
  LoneClassBracket(pos: Int)
  /// A group was opened but never closed.
  MissingClosingParen(pos: Int)
  /// `{n,m}` with `m < n`.
  OutOfOrderQuantifier(pos: Int)
  /// An escape that is not an IdentityEscape in Unicode modes.
  InvalidEscape(pos: Int)
  /// `\N` where `N` exceeds the number of capturing groups (strict modes).
  BackReferenceOutOfRange(pos: Int, n: Int, captures: Int)
  /// `\0` followed by a digit in Unicode modes.
  InvalidDecimalEscape(pos: Int)
  /// `\xHH` with fewer than two hex digits in Unicode modes.
  InvalidHexEscape(pos: Int)
  /// `\k` not followed by `<name>`, or naming a group that doesn't exist.
  InvalidNamedReference(pos: Int)
  /// The pattern ends with a bare backslash.
  BackslashAtEnd(pos: Int)
  /// `\uHHHH` / `\u{...}` that is not a well-formed Unicode escape.
  InvalidUnicodeEscape(pos: Int)
  /// `\u{...}` whose code point exceeds U+10FFFF.
  InvalidUnicodeEscapeValue(pos: Int)
  /// A `[` character class with no closing `]`.
  UnterminatedClass(pos: Int)
  /// A range endpoint that is a class escape (`\d`, `\p{..}`, ...).
  InvalidClassRange(pos: Int)
  /// A literal range `a-b` with `a > b`.
  OutOfOrderClassRange(pos: Int)
  /// An escape that is not a ClassEscape inside a character class.
  InvalidClassEscape(pos: Int)
  /// `&&` / `--` in a v-mode class with a missing operand.
  InvalidClassSetOperation(pos: Int)
  /// An unescaped ClassSetSyntaxCharacter, or misplaced `-`, in a v-mode
  /// class.
  InvalidClassCharacter(pos: Int)
  /// A reserved double punctuator (`!!`, `##`, ...) in a v-mode class.
  ReservedDoublePunctuator(pos: Int)
  /// A `<name>` group name with no closing `>`.
  UnterminatedGroupName(pos: Int)
  /// `(?<>...)` — a group name must have at least one character.
  EmptyGroupName(pos: Int)
  /// A group name that is not a valid identifier (or a malformed escape
  /// within one).
  InvalidGroupName(pos: Int)
  /// Inline modifiers (`(?ims-ims:`) that are malformed.
  InvalidModifierFlags(pos: Int)
  /// `(?-:` — the add and remove modifier sets are both empty.
  EmptyModifiers(pos: Int)
  /// A modifier flag repeated within, or across, the add/remove sets.
  RepeatedModifierFlag(pos: Int)
  /// `\p{...}` naming an unknown Unicode property (or a `\p` with no `{`).
  InvalidPropertyName(pos: Int)
  /// A property of strings (e.g. `RGI_Emoji`) outside a non-negated v-mode
  /// `\p`.
  PropertyOfStringsRequiresVFlag(pos: Int)
}

/// Render a `PatternError` to its user-facing message. This is the ONLY
/// place the regex diagnostic strings live — callers that surface the error
/// as a `SyntaxError` message must go through here.
pub fn pattern_error_message(e: PatternError) -> String {
  case e {
    UnterminatedRegex(_) -> "Unterminated regular expression"
    DuplicateFlag(_, flag) ->
      "Duplicate regular expression flag '" <> flag <> "'"
    InvalidFlag(_, flag) -> "Invalid regular expression flag '" <> flag <> "'"
    ExclusiveUnicodeFlags(_) ->
      "Invalid regular expression flags: u and v are exclusive"
    UnmatchedParen(_) -> "Invalid regular expression: unmatched ')'"
    NothingToRepeat(_) -> "Invalid regular expression: nothing to repeat"
    DuplicateGroupName(..) -> "Invalid regular expression: duplicate group name"
    LoneQuantifierBrackets(_) ->
      "Invalid regular expression: lone quantifier brackets"
    LoneClassBracket(_) ->
      "Invalid regular expression: lone character class bracket"
    MissingClosingParen(_) ->
      "Invalid regular expression: missing closing parenthesis"
    OutOfOrderQuantifier(_) ->
      "Invalid regular expression: numbers out of order in {} quantifier"
    InvalidEscape(_) -> "Invalid regular expression: invalid escape"
    BackReferenceOutOfRange(..) ->
      "Invalid regular expression: back reference out of range"
    InvalidDecimalEscape(_) ->
      "Invalid regular expression: invalid decimal escape"
    InvalidHexEscape(_) ->
      "Invalid regular expression: invalid hexadecimal escape"
    InvalidNamedReference(_) ->
      "Invalid regular expression: invalid named reference"
    BackslashAtEnd(_) -> "Invalid regular expression: \\ at end of pattern"
    InvalidUnicodeEscape(_) ->
      "Invalid regular expression: invalid Unicode escape"
    InvalidUnicodeEscapeValue(_) ->
      "Invalid regular expression: invalid Unicode escape value"
    UnterminatedClass(_) ->
      "Invalid regular expression: unterminated character class"
    InvalidClassRange(_) ->
      "Invalid regular expression: invalid character class range"
    OutOfOrderClassRange(_) ->
      "Invalid regular expression: range out of order in character class"
    InvalidClassEscape(_) -> "Invalid regular expression: invalid class escape"
    InvalidClassSetOperation(_) ->
      "Invalid regular expression: invalid set operation in character class"
    InvalidClassCharacter(_) ->
      "Invalid regular expression: invalid character in character class"
    ReservedDoublePunctuator(_) ->
      "Invalid regular expression: reserved double punctuator in character class"
    UnterminatedGroupName(_) ->
      "Invalid regular expression: unterminated group name"
    EmptyGroupName(_) -> "Invalid regular expression: empty group name"
    InvalidGroupName(_) -> "Invalid regular expression: invalid group name"
    InvalidModifierFlags(_) ->
      "Invalid regular expression: invalid modifier flags"
    EmptyModifiers(_) ->
      "Invalid regular expression: add and remove modifiers must not both be empty"
    RepeatedModifierFlag(_) ->
      "Invalid regular expression: repeated modifier flag"
    InvalidPropertyName(_) ->
      "Invalid regular expression: invalid property name in \\p{}"
    PropertyOfStringsRequiresVFlag(_) ->
      "Invalid regular expression: properties of strings require the v flag and a non-negated \\p"
  }
}

/// Byte offset of the offending construct. `pos` is the first field of every
/// variant, so this is a plain accessor.
pub fn pattern_error_pos(e: PatternError) -> Int {
  e.pos
}

// ---- Source byte access ----

/// The code point at `pos` and its byte width, or `Eof` past the end of the
/// bytes. A byte that isn't valid UTF-8 decodes as its own value with width 1
/// (enough for the scanners to make progress); only genuine end-of-input is
/// `Eof`, so an "absent" code point can never leak into a value position.
type CodePoint {
  Cp(value: Int, width: Int)
  Eof
}

/// Decode the UTF-8 code point at `pos`.
fn codepoint_at(bytes: BitArray, pos: Int) -> CodePoint {
  case bit_array.slice(bytes, pos, 1) {
    Ok(<<b>>) if b < 0x80 -> Cp(b, 1)
    Ok(<<b>>) -> {
      let w = utf8_byte_width(b)
      case bit_array.slice(bytes, pos, w) {
        Ok(chunk) ->
          case bit_array.to_string(chunk) {
            Ok(s) ->
              case string.to_utf_codepoints(s) {
                [c, ..] -> Cp(string.utf_codepoint_to_int(c), w)
                [] -> Cp(b, 1)
              }
            Error(Nil) -> Cp(b, 1)
          }
        Error(Nil) -> Cp(b, 1)
      }
    }
    Ok(_) | Error(Nil) -> Eof
  }
}

/// Bytes to advance past `pos`; 1 at end of input (so scanners terminate).
fn advance_width(bytes: BitArray, pos: Int) -> Int {
  case codepoint_at(bytes, pos) {
    Cp(_, w) -> w
    Eof -> 1
  }
}

/// What sits at `pos` within a pattern body that ends at `end`. Exhaustive:
/// the three cases the old `""` sentinel conflated are now separate variants.
type At {
  /// A lone ASCII character.
  Ascii(ch: String)
  /// A code point of `width` bytes that is not a lone ASCII character.
  NonAscii(width: Int)
  /// `pos` is at or past `end` — nothing here.
  AtEnd
}

fn at(bytes: BitArray, pos: Int, end: Int) -> At {
  case pos >= end {
    True -> AtEnd
    False ->
      case ascii_at(bytes, pos) {
        Some(ch) -> Ascii(ch)
        None ->
          case codepoint_at(bytes, pos) {
            Cp(_, w) -> NonAscii(w)
            Eof -> AtEnd
          }
      }
  }
}

/// The property-escape name (or value) text at `[start, start + len)`.
/// `skip_property_chars` only ever advances over ASCII `[A-Za-z0-9_]`, so the
/// range always slices to text; the empty fallback is unreachable, and would
/// classify as an invalid property name anyway.
fn source_slice(bytes: BitArray, start: Int, len: Int) -> String {
  source_bytes.slice(bytes, start, len)
  |> option.unwrap("")
}

/// The character at `pos`, or `None` at/past `end`. The bounded read the
/// pattern validator uses everywhere: raw `ascii_at` is for the literal
/// scanner, which is the thing that discovers where the body ends.
fn ascii_in(bytes: BitArray, pos: Int, end: Int) -> Option(String) {
  case pos < end {
    True -> ascii_at(bytes, pos)
    False -> None
  }
}

/// Value of the hex digit at `pos`, if there is one before `end`.
fn hex_in(bytes: BitArray, pos: Int, end: Int) -> Option(Int) {
  case ascii_in(bytes, pos, end) {
    Some(ch) -> digits.hex_value(ch)
    None -> None
  }
}

/// Value of the decimal digit at `pos`, if there is one before `end`.
fn digit_in(bytes: BitArray, pos: Int, end: Int) -> Option(Int) {
  case ascii_in(bytes, pos, end) {
    Some(ch) -> digits.digit_value(ch)
    None -> None
  }
}

/// Scan a run of decimal digits in `[pos, end)`, returning the position after
/// the run and its value — `Some` iff the run was non-empty. The value is
/// accumulated as we walk, so there is no re-slice + re-parse step that could
/// fail and silently yield 0.
fn decimal_run(bytes: BitArray, pos: Int, end: Int) -> #(Int, Option(Int)) {
  digit_run_loop(bytes, pos, end, 10, digit_in, None)
}

/// Same, for a run of hex digits.
fn hex_run(bytes: BitArray, pos: Int, end: Int) -> #(Int, Option(Int)) {
  digit_run_loop(bytes, pos, end, 16, hex_in, None)
}

fn digit_run_loop(
  bytes: BitArray,
  pos: Int,
  end: Int,
  base: Int,
  digit: fn(BitArray, Int, Int) -> Option(Int),
  acc: Option(Int),
) -> #(Int, Option(Int)) {
  case pos < end, digit(bytes, pos, end) {
    True, Some(d) ->
      digit_run_loop(
        bytes,
        pos + 1,
        end,
        base,
        digit,
        Some(option.unwrap(acc, 0) * base + d),
      )
    _, _ -> #(pos, acc)
  }
}

// ---- Regex body scanning ----

/// Scan regex source from just after the opening /, returning the position
/// just past the closing /. Handles escapes and character classes.
pub fn scan_regex_source(
  bytes: BitArray,
  pos: Int,
) -> Result(Int, PatternError) {
  scan_regex_loop(bytes, pos, False)
}

/// The recursion of `scan_regex_source`. `in_class` is loop state (are we
/// inside a `[...]`, where `/` is not the terminator?), never a caller's
/// choice.
fn scan_regex_loop(
  bytes: BitArray,
  pos: Int,
  in_class: Bool,
) -> Result(Int, PatternError) {
  case bit_array.slice(bytes, pos, 1) {
    // End of source before the closing /.
    Error(_) -> Error(UnterminatedRegex(pos))
    // Non-ASCII byte: part of the regex body (any SourceCharacter is allowed,
    // e.g. the Cf format-control U+180E). ascii_at is ASCII-only, so we
    // handle these here. U+2028/U+2029 are line terminators and end the regex.
    Ok(<<b>>) if b >= 0x80 ->
      case is_unicode_line_terminator(bytes, pos) {
        True -> Error(UnterminatedRegex(pos))
        False -> scan_regex_loop(bytes, pos + utf8_byte_width(b), in_class)
      }
    Ok(_) -> {
      case ascii_at(bytes, pos) {
        Some("\n") | Some("\r") -> Error(UnterminatedRegex(pos))
        Some("\\") -> {
          // Escaped character — skip the backslash and the next character
          // (which may itself be multi-byte, e.g. an escaped non-ASCII char).
          // A line terminator after `\` is not allowed (RegularExpressionChar
          // excludes LineTerminator), including U+2028/U+2029.
          case bit_array.slice(bytes, pos + 1, 1) {
            Error(_) -> Error(UnterminatedRegex(pos + 1))
            Ok(<<0x0A>>) | Ok(<<0x0D>>) -> Error(UnterminatedRegex(pos + 1))
            Ok(<<nb>>) if nb >= 0x80 ->
              case is_unicode_line_terminator(bytes, pos + 1) {
                True -> Error(UnterminatedRegex(pos + 1))
                False ->
                  scan_regex_loop(
                    bytes,
                    pos + 1 + utf8_byte_width(nb),
                    in_class,
                  )
              }
            Ok(<<nb>>) ->
              scan_regex_loop(bytes, pos + 1 + utf8_byte_width(nb), in_class)
            Ok(_) -> scan_regex_loop(bytes, pos + 2, in_class)
          }
        }
        Some("[") -> scan_regex_loop(bytes, pos + 1, True)
        Some("]") ->
          case in_class {
            True -> scan_regex_loop(bytes, pos + 1, False)
            False -> scan_regex_loop(bytes, pos + 1, in_class)
          }
        Some("/") ->
          case in_class {
            True -> scan_regex_loop(bytes, pos + 1, in_class)
            False -> Ok(pos + 1)
          }
        _ -> scan_regex_loop(bytes, pos + 1, in_class)
      }
    }
  }
}

/// UTF-8 byte width from a leading byte.
fn utf8_byte_width(lead: Int) -> Int {
  case lead {
    b if b >= 0xF0 -> 4
    b if b >= 0xE0 -> 3
    b if b >= 0xC0 -> 2
    _ -> 1
  }
}

/// True if the bytes at `pos` are U+2028 (E2 80 A8) or U+2029 (E2 80 A9),
/// the Unicode line/paragraph separators (line terminators in ES).
fn is_unicode_line_terminator(bytes: BitArray, pos: Int) -> Bool {
  case bit_array.slice(bytes, pos, 3) {
    Ok(<<0xE2, 0x80, 0xA8>>) | Ok(<<0xE2, 0x80, 0xA9>>) -> True
    _ -> False
  }
}

// ---- Pattern validation (parse-time early errors) ----

/// Regex compile mode selected by the flags. Derived once, in `regex_flags`,
/// and carried in `RegexFlags` from there on.
pub type RegexMode {
  /// No u or v flag — Annex B extended (web-compat) grammar.
  Legacy
  /// u flag — strict Unicode-mode grammar.
  Unicode
  /// v flag — unicodeSets mode (strict grammar, ClassSetExpression classes).
  UnicodeSets
}

type Ctx {
  Ctx(
    bytes: BitArray,
    end: Int,
    mode: RegexMode,
    captures: Int,
    names: List(String),
    has_named: Bool,
  )
}

/// The character at `pos`, or `None` at/past the end of the pattern body.
/// Every read the validator makes goes through `ctx_*` / `*_in`: the pattern
/// body of a regex literal is followed by `/flags…` and then the rest of the
/// file, so an unbounded read would validate against text that is not part of
/// the pattern.
fn ctx_ascii(ctx: Ctx, pos: Int) -> Option(String) {
  ascii_in(ctx.bytes, pos, ctx.end)
}

/// Value of the hex digit at `pos`, if there is one before `ctx.end`.
fn ctx_hex(ctx: Ctx, pos: Int) -> Option(Int) {
  hex_in(ctx.bytes, pos, ctx.end)
}

/// Value of the decimal digit at `pos`, if there is one before `ctx.end`.
fn ctx_digit(ctx: Ctx, pos: Int) -> Option(Int) {
  digit_in(ctx.bytes, pos, ctx.end)
}

/// The four hex digits at `pos`, if all four are hex digits before `ctx.end`.
fn ctx_hex4(ctx: Ctx, pos: Int) -> Option(Int) {
  case
    ctx_hex(ctx, pos),
    ctx_hex(ctx, pos + 1),
    ctx_hex(ctx, pos + 2),
    ctx_hex(ctx, pos + 3)
  {
    Some(a), Some(b), Some(c), Some(d) ->
      Some({ { { a * 16 + b } * 16 + c } * 16 } + d)
    _, _, _, _ -> None
  }
}

/// True when the character at `pos` is an ASCII letter, before `ctx.end`.
fn ctx_letter(ctx: Ctx, pos: Int) -> Bool {
  case ctx_ascii(ctx, pos) {
    Some(ch) -> is_ascii_letter(ch)
    None -> False
  }
}

/// Kind of the term just parsed — determines whether a quantifier may follow.
type TermKind {
  /// A quantifiable atom.
  KAtom
  /// ^ $ \b \B and lookbehind — never quantifiable.
  KAssertion
  /// (?= and (?! — quantifiable in legacy (Annex B) mode only.
  KLookahead
}

/// Validate a regex pattern body [start, end) against the ECMAScript Pattern
/// grammar: the Annex B extended grammar without the u/v flag, the strict
/// grammar with it. Reports the early errors test262 expects at parse time:
/// nothing-to-repeat, invalid braced quantifiers, quantified assertions,
/// named-group rules (duplicates, dangling/incomplete \k references), inline
/// modifier rules, and Unicode-mode escape restrictions.
/// The mode is taken from `flags`, which `validate_flags` / `skip_regex_flags`
/// already derived (and which already rejected `u` + `v` together).
pub fn validate_pattern(
  bytes: BitArray,
  start: Int,
  end: Int,
  flags: RegexFlags,
) -> Result(Nil, PatternError) {
  let mode = flags.mode
  let #(captures, names, has_named) =
    scan_groups(bytes, start, end, mode, 0, 0, [], False)
  let ctx = Ctx(bytes:, end:, mode:, captures:, names:, has_named:)
  use #(stop, _names) <- result.try(p_disjunction(ctx, start))
  use Nil <- result.try(case stop >= end {
    True -> Ok(Nil)
    False -> Error(UnmatchedParen(stop))
  })
  // v mode: walk nested classes for property-escape validity (the main
  // parser skips over v-mode class internals).
  case mode {
    UnicodeSets -> validate_regex_vmode_body(bytes, start, end)
    Legacy | Unicode -> Ok(Nil)
  }
}

/// First pass over the body: count capturing groups, collect the decoded
/// names of all named groups (for resolving \k<...> forward references), and
/// detect whether any GroupSpecifier is present (which switches \k from an
/// Annex B identity escape to a strict group reference in legacy mode).
fn scan_groups(
  bytes: BitArray,
  pos: Int,
  end: Int,
  mode: RegexMode,
  class_depth: Int,
  captures: Int,
  names: List(String),
  has_named: Bool,
) -> #(Int, List(String), Bool) {
  case pos >= end {
    True -> #(captures, names, has_named)
    False ->
      case ascii_in(bytes, pos, end), class_depth {
        Some("\\"), _ -> {
          let w = advance_width(bytes, pos + 1)
          scan_groups(
            bytes,
            pos + 1 + w,
            end,
            mode,
            class_depth,
            captures,
            names,
            has_named,
          )
        }
        Some("["), _ -> {
          // Legacy/u classes don't nest — a `[` inside one is a literal.
          let depth = case mode {
            UnicodeSets -> class_depth + 1
            Legacy | Unicode -> 1
          }
          scan_groups(
            bytes,
            pos + 1,
            end,
            mode,
            depth,
            captures,
            names,
            has_named,
          )
        }
        Some("]"), _ -> {
          let depth = case mode {
            UnicodeSets -> int.max(class_depth - 1, 0)
            Legacy | Unicode -> 0
          }
          scan_groups(
            bytes,
            pos + 1,
            end,
            mode,
            depth,
            captures,
            names,
            has_named,
          )
        }
        Some("("), 0 ->
          case ascii_in(bytes, pos + 1, end) {
            Some("?") ->
              case ascii_in(bytes, pos + 2, end) {
                Some("<") ->
                  case ascii_in(bytes, pos + 3, end) {
                    Some("=") | Some("!") ->
                      scan_groups(
                        bytes,
                        pos + 4,
                        end,
                        mode,
                        0,
                        captures,
                        names,
                        has_named,
                      )
                    _ -> {
                      // Named capturing group. A malformed name is reported
                      // by the second pass — here it just flips the
                      // has_named switch.
                      let names2 = case parse_group_name(bytes, pos + 3, end) {
                        Ok(#(name, _)) -> [name, ..names]
                        Error(_malformed_name) -> names
                      }
                      scan_groups(
                        bytes,
                        pos + 3,
                        end,
                        mode,
                        0,
                        captures + 1,
                        names2,
                        True,
                      )
                    }
                  }
                _ ->
                  scan_groups(
                    bytes,
                    pos + 2,
                    end,
                    mode,
                    0,
                    captures,
                    names,
                    has_named,
                  )
              }
            _ ->
              scan_groups(
                bytes,
                pos + 1,
                end,
                mode,
                0,
                captures + 1,
                names,
                has_named,
              )
          }
        _, _ -> {
          let w = advance_width(bytes, pos)
          scan_groups(
            bytes,
            pos + w,
            end,
            mode,
            class_depth,
            captures,
            names,
            has_named,
          )
        }
      }
  }
}

fn p_disjunction(
  ctx: Ctx,
  pos: Int,
) -> Result(#(Int, List(String)), PatternError) {
  use #(pos2, names) <- result.try(p_alternative(ctx, pos, []))
  case pos2 < ctx.end && ctx_ascii(ctx, pos2) == Some("|") {
    True -> {
      use #(pos3, names2) <- result.map(p_disjunction(ctx, pos2 + 1))
      // Names in different alternatives may legally repeat (they can never
      // both participate in a match), so union without a duplicate check.
      #(pos3, list.append(names, names2))
    }
    False -> Ok(#(pos2, names))
  }
}

fn p_alternative(
  ctx: Ctx,
  pos: Int,
  acc: List(String),
) -> Result(#(Int, List(String)), PatternError) {
  case pos >= ctx.end {
    True -> Ok(#(pos, acc))
    False ->
      case ctx_ascii(ctx, pos) {
        Some("|") | Some(")") -> Ok(#(pos, acc))
        _ -> {
          use #(pos2, tnames, kind) <- result.try(p_term(ctx, pos))
          use Nil <- result.try(check_no_duplicate(tnames, acc, pos))
          use #(pos3, quantified) <- result.try(p_quantifier_opt(ctx, pos2))
          use Nil <- result.try(case quantified, kind, ctx.mode {
            False, _, _ -> Ok(Nil)
            True, KAtom, _ -> Ok(Nil)
            True, KLookahead, Legacy -> Ok(Nil)
            True, _, _ -> Error(NothingToRepeat(pos2))
          })
          p_alternative(ctx, pos3, list.append(tnames, acc))
        }
      }
  }
}

/// Group names declared in terms of the same alternative (or nested within
/// one another) can both participate in a match — duplicates are an error.
/// `pos` is the position of the term/group that introduced `new_names`.
fn check_no_duplicate(
  new_names: List(String),
  seen: List(String),
  pos: Int,
) -> Result(Nil, PatternError) {
  case list.find(new_names, list.contains(seen, _)) {
    Ok(name) -> Error(DuplicateGroupName(pos, name))
    Error(Nil) -> Ok(Nil)
  }
}

fn p_term(
  ctx: Ctx,
  pos: Int,
) -> Result(#(Int, List(String), TermKind), PatternError) {
  case ctx_ascii(ctx, pos) {
    Some("^") | Some("$") -> Ok(#(pos + 1, [], KAssertion))
    Some("\\") ->
      case ctx_ascii(ctx, pos + 1) {
        Some("b") | Some("B") -> Ok(#(pos + 2, [], KAssertion))
        _ -> {
          use pos2 <- result.map(p_atom_escape(ctx, pos))
          #(pos2, [], KAtom)
        }
      }
    Some("(") -> p_group(ctx, pos)
    Some("*") | Some("+") | Some("?") -> Error(NothingToRepeat(pos))
    Some("{") ->
      case ctx.mode {
        // Annex B: a braced-quantifier-shaped `{...}` with nothing to
        // quantify is the InvalidBracedQuantifier early error; any other
        // `{` is an ExtendedPatternCharacter.
        Legacy ->
          case braced_quantifier(ctx.bytes, pos, ctx.end) {
            Some(_) -> Error(NothingToRepeat(pos))
            None -> Ok(#(pos + 1, [], KAtom))
          }
        Unicode | UnicodeSets -> Error(LoneQuantifierBrackets(pos))
      }
    Some("}") ->
      case ctx.mode {
        Legacy -> Ok(#(pos + 1, [], KAtom))
        Unicode | UnicodeSets -> Error(LoneQuantifierBrackets(pos))
      }
    Some("]") ->
      case ctx.mode {
        Legacy -> Ok(#(pos + 1, [], KAtom))
        Unicode | UnicodeSets -> Error(LoneClassBracket(pos))
      }
    Some("[") -> {
      use pos2 <- result.map(p_class(ctx, pos + 1))
      #(pos2, [], KAtom)
    }
    _ -> Ok(#(pos + advance_width(ctx.bytes, pos), [], KAtom))
  }
}

fn p_group(
  ctx: Ctx,
  pos: Int,
) -> Result(#(Int, List(String), TermKind), PatternError) {
  case ctx_ascii(ctx, pos + 1) {
    Some("?") ->
      case ctx_ascii(ctx, pos + 2) {
        Some("=") | Some("!") -> {
          use #(pos2, names) <- result.map(p_group_body(ctx, pos + 3))
          #(pos2, names, KLookahead)
        }
        Some(":") -> {
          use #(pos2, names) <- result.map(p_group_body(ctx, pos + 3))
          #(pos2, names, KAtom)
        }
        Some("<") ->
          case ctx_ascii(ctx, pos + 3) {
            Some("=") | Some("!") -> {
              use #(pos2, names) <- result.map(p_group_body(ctx, pos + 4))
              #(pos2, names, KAssertion)
            }
            _ -> {
              use #(name, after_gt) <- result.try(parse_group_name(
                ctx.bytes,
                pos + 3,
                ctx.end,
              ))
              use #(pos2, inner) <- result.try(p_group_body(ctx, after_gt))
              use Nil <- result.try(check_no_duplicate([name], inner, pos))
              Ok(#(pos2, [name, ..inner], KAtom))
            }
          }
        _ -> {
          use after_colon <- result.try(p_modifiers(ctx.bytes, pos + 2, ctx.end))
          use #(pos2, names) <- result.map(p_group_body(ctx, after_colon))
          #(pos2, names, KAtom)
        }
      }
    _ -> {
      use #(pos2, names) <- result.map(p_group_body(ctx, pos + 1))
      #(pos2, names, KAtom)
    }
  }
}

fn p_group_body(
  ctx: Ctx,
  pos: Int,
) -> Result(#(Int, List(String)), PatternError) {
  use #(pos2, names) <- result.try(p_disjunction(ctx, pos))
  case pos2 < ctx.end && ctx_ascii(ctx, pos2) == Some(")") {
    True -> Ok(#(pos2 + 1, names))
    False -> Error(MissingClosingParen(pos2))
  }
}

/// Optional quantifier after a term: `*` `+` `?` or a braced quantifier,
/// each with an optional lazy `?` suffix. In legacy mode a `{` that is not a
/// valid braced quantifier is left for the caller (ExtendedPatternCharacter);
/// in Unicode modes it is an error.
fn p_quantifier_opt(ctx: Ctx, pos: Int) -> Result(#(Int, Bool), PatternError) {
  case pos >= ctx.end {
    True -> Ok(#(pos, False))
    False ->
      case ctx_ascii(ctx, pos) {
        Some("*") | Some("+") | Some("?") ->
          Ok(#(skip_lazy(ctx, pos + 1), True))
        Some("{") ->
          case braced_quantifier(ctx.bytes, pos, ctx.end) {
            Some(BracedQuantifier(after:, min:, max:)) ->
              case max {
                Some(m) if m < min -> Error(OutOfOrderQuantifier(pos))
                _ -> Ok(#(skip_lazy(ctx, after), True))
              }
            None ->
              case ctx.mode {
                Legacy -> Ok(#(pos, False))
                Unicode | UnicodeSets -> Error(LoneQuantifierBrackets(pos))
              }
          }
        _ -> Ok(#(pos, False))
      }
  }
}

fn skip_lazy(ctx: Ctx, pos: Int) -> Int {
  case ctx_ascii(ctx, pos) {
    Some("?") -> pos + 1
    _ -> pos
  }
}

/// A well-formed `{n}` / `{n,}` / `{n,m}`: the position after the `}` and the
/// decoded bounds (`max` is `None` for the open-ended forms).
type BracedQuantifier {
  BracedQuantifier(after: Int, min: Int, max: Option(Int))
}

/// Parse a braced quantifier starting at the `{`. `None` means "this `{` is
/// not a quantifier" — an absence, not a failure: the caller decides whether
/// that is legal (Annex B) or an error (Unicode modes).
fn braced_quantifier(
  bytes: BitArray,
  pos: Int,
  end: Int,
) -> Option(BracedQuantifier) {
  case decimal_run(bytes, pos + 1, end) {
    #(_, None) -> None
    #(lo_end, Some(min)) ->
      case ascii_in(bytes, lo_end, end) {
        Some("}") -> Some(BracedQuantifier(after: lo_end + 1, min:, max: None))
        Some(",") -> {
          let #(hi_end, max) = decimal_run(bytes, lo_end + 1, end)
          case ascii_in(bytes, hi_end, end) {
            Some("}") -> Some(BracedQuantifier(after: hi_end + 1, min:, max:))
            _ -> None
          }
        }
        _ -> None
      }
  }
}

/// AtomEscape at `pos` (the backslash), outside a character class.
/// \b and \B are handled by the caller (they are assertions).
fn p_atom_escape(ctx: Ctx, pos: Int) -> Result(Int, PatternError) {
  let strict = ctx.mode != Legacy
  let invalid = InvalidEscape(pos)
  case at(ctx.bytes, pos + 1, ctx.end) {
    // The scanner prevents a lone trailing `\` in a literal; `new RegExp("\\")`
    // reaches here.
    AtEnd -> Error(BackslashAtEnd(pos))
    // An escaped non-ASCII character: an IdentityEscape only in Annex B.
    NonAscii(w) ->
      case strict {
        True -> Error(invalid)
        False -> Ok(pos + 1 + w)
      }
    Ascii("1")
    | Ascii("2")
    | Ascii("3")
    | Ascii("4")
    | Ascii("5")
    | Ascii("6")
    | Ascii("7")
    | Ascii("8")
    | Ascii("9") -> {
      let #(after, n) = decimal_run(ctx.bytes, pos + 1, ctx.end)
      case strict, n {
        // Annex B: a too-large backreference falls back to a legacy octal
        // escape or identity, so anything goes.
        False, _ -> Ok(after)
        True, Some(n) if n > ctx.captures ->
          Error(BackReferenceOutOfRange(pos, n, ctx.captures))
        True, _ -> Ok(after)
      }
    }
    Ascii("0") ->
      case strict && option.is_some(ctx_digit(ctx, pos + 2)) {
        True -> Error(InvalidDecimalEscape(pos))
        False -> Ok(pos + 2)
      }
    Ascii("f") | Ascii("n") | Ascii("r") | Ascii("t") | Ascii("v") ->
      Ok(pos + 2)
    Ascii("d")
    | Ascii("D")
    | Ascii("s")
    | Ascii("S")
    | Ascii("w")
    | Ascii("W") -> Ok(pos + 2)
    Ascii("p") | Ascii("P") ->
      case ctx.mode {
        Legacy -> Ok(pos + 2)
        Unicode -> {
          use len <- result.map(property_escape_length(
            ctx.bytes,
            pos,
            ctx.end,
            allow_strings: False,
          ))
          pos + len
        }
        // v mode: measure the escape only. Whether a property of strings is
        // legal at this position depends on the enclosing negated classes,
        // and `validate_regex_vmode_body` — the single owner of that rule —
        // walks the whole body afterwards and reports it there.
        UnicodeSets -> {
          use len <- result.map(property_escape_length(
            ctx.bytes,
            pos,
            ctx.end,
            allow_strings: True,
          ))
          pos + len
        }
      }
    Ascii("c") ->
      case ctx_letter(ctx, pos + 2) {
        True -> Ok(pos + 3)
        False ->
          case strict {
            True -> Error(invalid)
            // Annex B: the backslash alone is an atom matching `\`, and the
            // `c` is re-parsed as a normal pattern character.
            False -> Ok(pos + 1)
          }
      }
    Ascii("x") ->
      case
        option.is_some(ctx_hex(ctx, pos + 2))
        && option.is_some(ctx_hex(ctx, pos + 3))
      {
        True -> Ok(pos + 4)
        False ->
          case strict {
            True -> Error(InvalidHexEscape(pos))
            False -> Ok(pos + 2)
          }
      }
    Ascii("u") -> {
      use #(after, _value) <- result.map(p_unicode_escape(ctx, pos))
      after
    }
    Ascii("k") ->
      case strict || ctx.has_named {
        True ->
          case ctx_ascii(ctx, pos + 2) {
            Some("<") -> {
              use #(name, after) <- result.try(parse_group_name(
                ctx.bytes,
                pos + 3,
                ctx.end,
              ))
              case list.contains(ctx.names, name) {
                True -> Ok(after)
                False -> Error(InvalidNamedReference(pos))
              }
            }
            _ -> Error(InvalidNamedReference(pos))
          }
        False -> Ok(pos + 2)
      }
    Ascii(other) ->
      case strict {
        // Unicode-mode IdentityEscape: SyntaxCharacter or `/` only.
        True ->
          case is_syntax_char(other) || other == "/" {
            True -> Ok(pos + 2)
            False -> Error(invalid)
          }
        False -> Ok(pos + 2)
      }
  }
}

/// \u escape at `pos` (the backslash). In Unicode modes: Hex4Digits or
/// {CodePoint}; in legacy mode an incomplete escape is the identity `u`
/// (so `/\u{2}/` is `u` with a braced quantifier).
///
/// Returns the position after the escape AND its CharacterValue (§22.2.1) —
/// the two are decided together, so a caller can never accept an escape and
/// then guess at what character it denotes.
fn p_unicode_escape(ctx: Ctx, pos: Int) -> Result(#(Int, Int), PatternError) {
  case ctx_hex4(ctx, pos + 2) {
    Some(lead) -> {
      // In Unicode mode a lead surrogate followed by `\u` + a trail surrogate
      // is ONE RegExpUnicodeEscapeSequence denoting the combined code point.
      let lone = Ok(#(pos + 6, lead))
      case utf16.is_high(lead), ctx.mode {
        False, _ | True, Legacy -> lone
        True, _ ->
          case ctx_ascii(ctx, pos + 6), ctx_ascii(ctx, pos + 7) {
            Some("\\"), Some("u") if pos + 12 <= ctx.end ->
              case ctx_hex4(ctx, pos + 8) {
                Some(trail) ->
                  case utf16.is_low(trail) {
                    True -> Ok(#(pos + 12, utf16.combine(lead, trail)))
                    False -> lone
                  }
                None -> lone
              }
            _, _ -> lone
          }
      }
    }
    None ->
      case ctx.mode {
        // Identity escape: the `u` itself.
        Legacy -> Ok(#(pos + 2, 0x75))
        Unicode | UnicodeSets ->
          case ctx_ascii(ctx, pos + 2) {
            Some("{") -> {
              let #(after, value) = hex_run(ctx.bytes, pos + 3, ctx.end)
              case value, ctx_ascii(ctx, after) {
                Some(v), Some("}") if v > 0x10FFFF ->
                  Error(InvalidUnicodeEscapeValue(pos))
                Some(v), Some("}") -> Ok(#(after + 1, v))
                // No digits, or no closing brace.
                _, _ -> Error(InvalidUnicodeEscape(pos))
              }
            }
            _ -> Error(InvalidUnicodeEscape(pos))
          }
      }
  }
}

/// Which grammar the ClassRanges walk below is running under. Produced only
/// by `p_class`'s non-v branch, so `strict = mode == UnicodeClass` in every
/// function it is threaded through — the v-mode class never gets here.
type ClassMode {
  LegacyClass
  UnicodeClass
}

/// Character class starting just after the `[`. v-mode classes have their
/// own (nested) grammar — skip over them bracket-aware; their property
/// escapes are validated by the separate v-mode walk.
fn p_class(ctx: Ctx, pos: Int) -> Result(Int, PatternError) {
  case ctx.mode {
    UnicodeSets -> skip_v_class(ctx.bytes, pos, ctx.end)
    Legacy -> p_class_ranges(ctx, LegacyClass, skip_class_negation(ctx, pos))
    Unicode -> p_class_ranges(ctx, UnicodeClass, skip_class_negation(ctx, pos))
  }
}

/// Past the leading `^` of a negated class, if there is one.
fn skip_class_negation(ctx: Ctx, pos: Int) -> Int {
  case ctx_ascii(ctx, pos) {
    Some("^") -> pos + 1
    _ -> pos
  }
}

fn p_class_ranges(
  ctx: Ctx,
  mode: ClassMode,
  pos: Int,
) -> Result(Int, PatternError) {
  case pos >= ctx.end {
    True -> Error(UnterminatedClass(pos))
    False ->
      case ctx_ascii(ctx, pos) {
        Some("]") -> Ok(pos + 1)
        _ -> {
          use a <- result.try(p_class_atom(ctx, mode, pos))
          let after_a = class_atom_after(a)
          let is_range =
            ctx_ascii(ctx, after_a) == Some("-")
            && after_a + 1 < ctx.end
            && ctx_ascii(ctx, after_a + 1) != Some("]")
          case is_range {
            True -> {
              use b <- result.try(p_class_atom(ctx, mode, after_a + 1))
              use Nil <- result.try(check_range(mode, a, b, pos))
              p_class_ranges(ctx, mode, class_atom_after(b))
            }
            False -> p_class_ranges(ctx, mode, after_a)
          }
        }
      }
  }
}

/// A `x-y` range: both endpoints must be single characters (u-mode only —
/// Annex B treats a `-` next to a `\d`-style set as a literal), and their
/// CharacterValues must be in order.
///
/// The one exception is a legacy-mode non-BMP endpoint: legacy patterns match
/// on UTF-16 code units, so such an endpoint is really a surrogate PAIR and
/// the range's ordering is not decided by its code point.
fn check_range(
  mode: ClassMode,
  a: ClassAtom,
  b: ClassAtom,
  pos: Int,
) -> Result(Nil, PatternError) {
  case a, b {
    ClassCharacter(value: av, ..), ClassCharacter(value: bv, ..) -> {
      let unknowable = mode == LegacyClass && { av > 0xFFFF || bv > 0xFFFF }
      case !unknowable && av > bv {
        True -> Error(OutOfOrderClassRange(pos))
        False -> Ok(Nil)
      }
    }
    // \d \s \w \p{..} etc. are not valid range endpoints in u mode.
    _, _ ->
      case mode {
        UnicodeClass -> Error(InvalidClassRange(pos))
        LegacyClass -> Ok(Nil)
      }
  }
}

/// One class atom, and everything the ClassRanges walk needs to know about
/// it. A single character always carries its CharacterValue (§22.2.1) — the
/// value the range-order early error compares — so an atom whose value we
/// forgot to record is a compile error, not a silently skipped check.
type ClassAtom {
  ClassCharacter(after: Int, value: Int)
  /// A CharacterClassEscape (`\d`, `\p{...}`, …): a SET of characters, with
  /// no single value, and never a valid range endpoint in u mode.
  ClassSet(after: Int)
}

fn class_atom_after(atom: ClassAtom) -> Int {
  case atom {
    ClassCharacter(after:, ..) -> after
    ClassSet(after:) -> after
  }
}

fn p_class_atom(
  ctx: Ctx,
  mode: ClassMode,
  pos: Int,
) -> Result(ClassAtom, PatternError) {
  case pos >= ctx.end {
    True -> Error(UnterminatedClass(pos))
    False ->
      case ctx_ascii(ctx, pos) {
        Some("\\") -> p_class_escape(ctx, mode, pos)
        _ ->
          case codepoint_at(ctx.bytes, pos) {
            Cp(cp, w) -> Ok(ClassCharacter(after: pos + w, value: cp))
            Eof -> Error(UnterminatedClass(pos))
          }
      }
  }
}

/// The code point of a lone ASCII character (as produced by `ascii_at`).
/// A non-ASCII or empty string cannot occur; 0 keeps the function total.
fn ascii_code(ch: String) -> Int {
  case string.to_utf_codepoints(ch) {
    [cp, ..] -> string.utf_codepoint_to_int(cp)
    [] -> 0
  }
}

/// ClassEscape at `pos` (the backslash) inside a legacy or u-mode class.
fn p_class_escape(
  ctx: Ctx,
  mode: ClassMode,
  pos: Int,
) -> Result(ClassAtom, PatternError) {
  let strict = mode == UnicodeClass
  let invalid = InvalidClassEscape(pos)
  case at(ctx.bytes, pos + 1, ctx.end) {
    AtEnd -> Error(UnterminatedClass(pos))
    // An escaped non-ASCII character: an IdentityEscape only in Annex B.
    NonAscii(w) ->
      case strict, codepoint_at(ctx.bytes, pos + 1) {
        True, _ -> Error(invalid)
        False, Cp(cp, _) -> Ok(ClassCharacter(after: pos + 1 + w, value: cp))
        False, Eof -> Error(UnterminatedClass(pos))
      }
    // \b in a class is the backspace character, \- the literal `-`.
    Ascii("b") -> Ok(ClassCharacter(after: pos + 2, value: 0x08))
    Ascii("-") -> Ok(ClassCharacter(after: pos + 2, value: 0x2D))
    Ascii("d")
    | Ascii("D")
    | Ascii("s")
    | Ascii("S")
    | Ascii("w")
    | Ascii("W") -> Ok(ClassSet(after: pos + 2))
    Ascii("p" as ch) | Ascii("P" as ch) ->
      case mode {
        // Annex B: `\p` is the identity escape for `p`.
        LegacyClass -> Ok(ClassCharacter(after: pos + 2, value: ascii_code(ch)))
        UnicodeClass -> {
          use len <- result.map(property_escape_length(
            ctx.bytes,
            pos,
            ctx.end,
            allow_strings: False,
          ))
          ClassSet(after: pos + len)
        }
      }
    Ascii("f") -> Ok(ClassCharacter(after: pos + 2, value: 0x0C))
    Ascii("n") -> Ok(ClassCharacter(after: pos + 2, value: 0x0A))
    Ascii("r") -> Ok(ClassCharacter(after: pos + 2, value: 0x0D))
    Ascii("t") -> Ok(ClassCharacter(after: pos + 2, value: 0x09))
    Ascii("v") -> Ok(ClassCharacter(after: pos + 2, value: 0x0B))
    Ascii("c") ->
      case control_letter_at(ctx, pos + 2, strict) {
        Some(value) -> Ok(ClassCharacter(after: pos + 3, value:))
        None ->
          case strict {
            True -> Error(invalid)
            // Annex B: the backslash alone is a class atom matching `\`.
            False -> Ok(ClassCharacter(after: pos + 1, value: 0x5C))
          }
      }
    Ascii("x") ->
      case ctx_hex(ctx, pos + 2), ctx_hex(ctx, pos + 3) {
        Some(h1), Some(h2) ->
          Ok(ClassCharacter(after: pos + 4, value: h1 * 16 + h2))
        _, _ ->
          case strict {
            True -> Error(InvalidHexEscape(pos))
            // Annex B: the identity escape for `x`.
            False -> Ok(ClassCharacter(after: pos + 2, value: 0x78))
          }
      }
    Ascii("u") -> {
      use #(after, value) <- result.map(p_unicode_escape(ctx, pos))
      ClassCharacter(after:, value:)
    }
    Ascii("0")
    | Ascii("1")
    | Ascii("2")
    | Ascii("3")
    | Ascii("4")
    | Ascii("5")
    | Ascii("6")
    | Ascii("7")
    | Ascii("8")
    | Ascii("9") ->
      case strict {
        // Annex B: legacy octal escapes and `\8` `\9` identity.
        False -> Ok(legacy_numeric_escape(ctx.bytes, pos, ctx.end))
        // Strict: only `\0` not followed by a decimal digit (the NUL escape).
        True ->
          case
            ctx_ascii(ctx, pos + 1) == Some("0")
            && !option.is_some(ctx_digit(ctx, pos + 2))
          {
            True -> Ok(ClassCharacter(after: pos + 2, value: 0x00))
            False -> Error(invalid)
          }
      }
    Ascii("k") ->
      // IdentityEscape excludes `k` whenever the pattern has named groups
      // (and \k is never a ClassEscape group reference).
      case strict || ctx.has_named {
        True -> Error(invalid)
        False -> Ok(ClassCharacter(after: pos + 2, value: 0x6B))
      }
    Ascii(other) ->
      case strict {
        True ->
          case is_syntax_char(other) || other == "/" {
            True -> Ok(ClassCharacter(after: pos + 2, value: ascii_code(other)))
            False -> Error(invalid)
          }
        False -> Ok(ClassCharacter(after: pos + 2, value: ascii_code(other)))
      }
  }
}

/// The value of a `\c` control escape's letter at `pos`: the ControlLetter's
/// code point modulo 32. In Annex B (`strict` False) `ClassControlLetter` also
/// admits decimal digits and `_`. `None` when there is no control letter here.
fn control_letter_at(ctx: Ctx, pos: Int, strict: Bool) -> Option(Int) {
  case ctx_ascii(ctx, pos) {
    Some(ch) ->
      case is_ascii_letter(ch) {
        True -> Some(ascii_code(ch) % 32)
        False ->
          case !strict && { option.is_some(ctx_digit(ctx, pos)) || ch == "_" } {
            True -> Some(ascii_code(ch) % 32)
            False -> None
          }
      }
    None -> None
  }
}

/// An Annex B `\` + digit escape inside a class: a LegacyOctalEscapeSequence
/// (up to three octal digits, at most two when the first is 4-7), or the
/// identity escape for `\8` / `\9`.
fn legacy_numeric_escape(bytes: BitArray, pos: Int, end: Int) -> ClassAtom {
  case octal_at(bytes, pos + 1, end) {
    None -> {
      // `\8` / `\9`: identity.
      let value =
        ascii_in(bytes, pos + 1, end)
        |> option.map(ascii_code)
        |> option.unwrap(0)
      ClassCharacter(after: pos + 2, value:)
    }
    Some(d1) -> {
      let max_more = case d1 <= 3 {
        True -> 2
        False -> 1
      }
      let #(after, value) = octal_run(bytes, pos + 2, end, max_more, d1)
      ClassCharacter(after:, value:)
    }
  }
}

/// The value of the octal digit at `pos`, if the character there is one.
fn octal_at(bytes: BitArray, pos: Int, end: Int) -> Option(Int) {
  case digit_in(bytes, pos, end) {
    Some(d) if d < 8 -> Some(d)
    Some(_) | None -> None
  }
}

fn octal_run(
  bytes: BitArray,
  pos: Int,
  end: Int,
  remaining: Int,
  acc: Int,
) -> #(Int, Int) {
  case remaining > 0, octal_at(bytes, pos, end) {
    True, Some(d) -> octal_run(bytes, pos + 1, end, remaining - 1, acc * 8 + d)
    _, _ -> #(pos, acc)
  }
}

/// Walk a v-mode (unicodeSets) class from just after the opening `[`,
/// returning the position past the matching `]`. Classes may nest
/// (`[a[bc]]`). Enforces the ClassSetExpression surface rules:
/// - raw ( ) { } / | are ClassSetSyntaxCharacters and must be escaped
/// - reserved double punctuators (!! ## $$ %% ** ++ ,, .. :: ;; << == >>
///   ?? @@ ^^ `` ~~) are SyntaxErrors
/// - `&&` (intersection) and `--` (difference) need operands on both sides
/// - a single `-` is only valid between two range operands
/// Property/string escapes (\p{...}, \q{...}) are skipped brace-aware; their
/// contents are validated by the separate v-mode walk.
fn skip_v_class(
  bytes: BitArray,
  pos: Int,
  end: Int,
) -> Result(Int, PatternError) {
  // Skip the optional leading negation `^` (a lone `^` here is the negation
  // marker, not an atom — `[^^^]` is negation + the reserved `^^`).
  let pos2 = case ascii_in(bytes, pos, end) {
    Some("^") -> pos + 1
    _ -> pos
  }
  v_class_loop(bytes, pos2, end, False)
}

fn v_class_loop(
  bytes: BitArray,
  pos: Int,
  end: Int,
  prev_atom: Bool,
) -> Result(Int, PatternError) {
  let nxt = ascii_in(bytes, pos + 1, end)
  case pos >= end, ascii_in(bytes, pos, end) {
    True, _ -> Error(UnterminatedClass(pos))
    _, Some("]") -> Ok(pos + 1)
    _, Some("[") -> {
      use after <- result.try(skip_v_class(bytes, pos + 1, end))
      v_class_loop(bytes, after, end, True)
    }
    _, Some("\\") ->
      case nxt {
        // \p{...} \P{...} \q{...} \u{...} — skip the whole braced payload so
        // its contents aren't misread as class syntax.
        Some("p") | Some("P") | Some("q") | Some("u") ->
          case ascii_in(bytes, pos + 2, end) {
            Some("{") -> {
              use after <- result.try(skip_to_close_brace(bytes, pos + 3, end))
              v_class_loop(bytes, after, end, True)
            }
            _ -> v_class_loop(bytes, pos + 2, end, True)
          }
        _ ->
          v_class_loop(
            bytes,
            pos + 1 + advance_width(bytes, pos + 1),
            end,
            True,
          )
      }
    _, Some("&") ->
      case nxt {
        Some("&") -> {
          let after2 = ascii_in(bytes, pos + 2, end)
          case
            prev_atom
            && after2 != Some("]")
            && after2 != Some("&")
            && pos + 2 < end
          {
            True -> v_class_loop(bytes, pos + 2, end, False)
            False -> Error(InvalidClassSetOperation(pos))
          }
        }
        _ -> v_class_loop(bytes, pos + 1, end, True)
      }
    _, Some("-") ->
      case nxt {
        Some("-") -> {
          let after2 = ascii_in(bytes, pos + 2, end)
          case prev_atom && after2 != Some("]") && pos + 2 < end {
            True -> v_class_loop(bytes, pos + 2, end, False)
            False -> Error(InvalidClassSetOperation(pos))
          }
        }
        _ ->
          // Single dash: only valid as a range separator between operands.
          case prev_atom && nxt != Some("]") && pos + 1 < end {
            True -> v_class_loop(bytes, pos + 1, end, False)
            False -> Error(InvalidClassCharacter(pos))
          }
      }
    _, Some("(")
    | _, Some(")")
    | _, Some("{")
    | _, Some("}")
    | _, Some("/")
    | _, Some("|")
    -> Error(InvalidClassCharacter(pos))
    _, Some("!" as ch)
    | _, Some("#" as ch)
    | _, Some("$" as ch)
    | _, Some("%" as ch)
    | _, Some("*" as ch)
    | _, Some("+" as ch)
    | _, Some("," as ch)
    | _, Some("." as ch)
    | _, Some(":" as ch)
    | _, Some(";" as ch)
    | _, Some("<" as ch)
    | _, Some("=" as ch)
    | _, Some(">" as ch)
    | _, Some("?" as ch)
    | _, Some("@" as ch)
    | _, Some("`" as ch)
    | _, Some("~" as ch)
    | _, Some("^" as ch)
    ->
      case nxt == Some(ch) {
        True -> Error(ReservedDoublePunctuator(pos))
        False -> v_class_loop(bytes, pos + 1, end, True)
      }
    _, _ -> v_class_loop(bytes, pos + advance_width(bytes, pos), end, True)
  }
}

/// Skip to just past the next `}` (for \p{...} / \q{...} payloads).
fn skip_to_close_brace(
  bytes: BitArray,
  pos: Int,
  end: Int,
) -> Result(Int, PatternError) {
  case pos >= end {
    True -> Error(UnterminatedClass(pos))
    False ->
      case ascii_in(bytes, pos, end) {
        Some("}") -> Ok(pos + 1)
        _ -> skip_to_close_brace(bytes, pos + 1, end)
      }
  }
}

/// Parse and validate a group name starting at `pos` (just after `<`),
/// returning the decoded name and the position after the closing `>`.
/// First code point must be ID_Start, the rest ID_Continue, with \uXXXX and
/// \u{...} escapes (including surrogate pairs) decoded.
fn parse_group_name(
  bytes: BitArray,
  pos: Int,
  end: Int,
) -> Result(#(String, Int), PatternError) {
  group_name_loop(bytes, pos, end, True, [])
}

/// The accepted code point of a group-name character, as an encodable
/// `UtfCodepoint`. Every code point that gets this far already passed
/// `validate_identifier_codepoint` (which rejects surrogates and NUL) and the
/// U+10FFFF bound, so the encoding cannot fail — but the type says so instead
/// of a `filter_map` silently dropping the character from the name.
fn name_codepoint(cp: Int, pos: Int) -> Result(UtfCodepoint, PatternError) {
  string.utf_codepoint(cp)
  |> result.replace_error(InvalidGroupName(pos))
}

fn group_name_loop(
  bytes: BitArray,
  pos: Int,
  end: Int,
  is_first: Bool,
  acc: List(UtfCodepoint),
) -> Result(#(String, Int), PatternError) {
  case pos >= end, ascii_in(bytes, pos, end) {
    True, _ -> Error(UnterminatedGroupName(pos))
    _, Some(">") ->
      case is_first {
        True -> Error(EmptyGroupName(pos))
        False -> Ok(#(string.from_utf_codepoints(list.reverse(acc)), pos + 1))
      }
    _, Some("\\") -> {
      use #(cp0, next0) <- result.try(decode_name_escape(bytes, pos, end))
      // Combine a surrogate pair written as two \uXXXX escapes.
      let #(cp, next) = case utf16.is_high(cp0) {
        False -> #(cp0, next0)
        True ->
          case decode_name_escape(bytes, next0, end) {
            Ok(#(trail, next1)) ->
              case utf16.is_low(trail) {
                True -> #(utf16.combine(cp0, trail), next1)
                False -> #(cp0, next0)
              }
            Error(_not_escape) -> #(cp0, next0)
          }
      }
      case lexer.validate_identifier_codepoint(cp, is_first) {
        True -> {
          use encoded <- result.try(name_codepoint(cp, pos))
          group_name_loop(bytes, next, end, False, [encoded, ..acc])
        }
        False -> Error(InvalidGroupName(pos))
      }
    }
    _, _ ->
      case codepoint_at(bytes, pos) {
        Cp(cp, width) ->
          case lexer.validate_identifier_codepoint(cp, is_first) {
            True -> {
              use encoded <- result.try(name_codepoint(cp, pos))
              group_name_loop(bytes, pos + width, end, False, [encoded, ..acc])
            }
            False -> Error(InvalidGroupName(pos))
          }
        Eof -> Error(UnterminatedGroupName(pos))
      }
  }
}

/// A `\uXXXX` / `\u{...}` escape inside a group name at `pos` (the
/// backslash), decoded to its code point. `end` bounds the pattern body, so a
/// `\u{` run can never scan past it.
fn decode_name_escape(
  bytes: BitArray,
  pos: Int,
  end: Int,
) -> Result(#(Int, Int), PatternError) {
  case ascii_in(bytes, pos + 1, end) {
    Some("u") ->
      case ascii_in(bytes, pos + 2, end) {
        Some("{") -> {
          let #(after, value) = hex_run(bytes, pos + 3, end)
          case value, ascii_in(bytes, after, end) {
            // Reject code points above U+10FFFF here — downstream identifier
            // checks assume a valid Unicode scalar range.
            Some(cp), Some("}") if cp <= 0x10FFFF -> Ok(#(cp, after + 1))
            _, _ -> Error(InvalidGroupName(pos))
          }
        }
        _ -> {
          // Exactly four hex digits: `hex_run` bounded to `pos + 6` (and to
          // the pattern body) cannot consume more, and `after == pos + 6`
          // rejects any shorter run.
          let #(after, value) = hex_run(bytes, pos + 2, int.min(pos + 6, end))
          case value, after == pos + 6 {
            Some(cp), True -> Ok(#(cp, pos + 6))
            _, _ -> Error(InvalidGroupName(pos))
          }
        }
      }
    _ -> Error(InvalidGroupName(pos))
  }
}

/// Validate inline modifier flags (after `(?`), e.g. `(?ims:`, `(?ims-ims:`,
/// `(?-i:`. Only i/m/s are allowed; no flag may repeat (including across the
/// add/remove sets), and the two sets must not both be empty. Returns the
/// position just after the `:`.
///
/// PRECONDITION: the character at `pos` is not `:` — `p_group` handles `(?:`
/// itself and only falls through to here for other `(?x` heads. So a `:` at
/// `pos2` implies at least one add flag was consumed, and `(?:` can never be
/// mistaken for an empty modifier set.
fn p_modifiers(
  bytes: BitArray,
  pos: Int,
  end: Int,
) -> Result(Int, PatternError) {
  use #(pos2, add) <- result.try(p_mod_flags(bytes, pos, end, []))
  case ascii_in(bytes, pos2, end) {
    Some(":") -> Ok(pos2 + 1)
    Some("-") -> {
      use #(pos3, remove) <- result.try(p_mod_flags(bytes, pos2 + 1, end, []))
      use Nil <- result.try(case ascii_in(bytes, pos3, end) {
        Some(":") -> Ok(Nil)
        _ -> Error(InvalidModifierFlags(pos3))
      })
      use Nil <- result.try(case add, remove {
        [], [] -> Error(EmptyModifiers(pos))
        _, _ -> Ok(Nil)
      })
      case list.any(add, list.contains(remove, _)) {
        True -> Error(RepeatedModifierFlag(pos))
        False -> Ok(pos3 + 1)
      }
    }
    _ -> Error(InvalidModifierFlags(pos2))
  }
}

fn p_mod_flags(
  bytes: BitArray,
  pos: Int,
  end: Int,
  seen: List(String),
) -> Result(#(Int, List(String)), PatternError) {
  case pos >= end {
    True -> Ok(#(pos, seen))
    False ->
      case ascii_in(bytes, pos, end) {
        Some("i" as f) | Some("m" as f) | Some("s" as f) ->
          case list.contains(seen, f) {
            True -> Error(RepeatedModifierFlag(pos))
            False -> p_mod_flags(bytes, pos + 1, end, [f, ..seen])
          }
        _ -> Ok(#(pos, seen))
      }
  }
}

/// True for a-z A-Z.
fn is_ascii_letter(ch: String) -> Bool {
  case string.to_utf_codepoints(ch) {
    [cp] -> digits.is_ascii_alpha_code(string.utf_codepoint_to_int(cp))
    _ -> False
  }
}

/// SyntaxCharacter: ^ $ \ . * + ? ( ) [ ] { } |
fn is_syntax_char(ch: String) -> Bool {
  case ch {
    "^"
    | "$"
    | "\\"
    | "."
    | "*"
    | "+"
    | "?"
    | "("
    | ")"
    | "["
    | "]"
    | "{"
    | "}"
    | "|" -> True
    _ -> False
  }
}

/// The decoded flags of a regular expression, in source order. Produced only
/// by `validate_flags` / `skip_regex_flags`, so holding one proves the flags
/// string contained no unknown, duplicate or mutually exclusive flag — and
/// that `mode` is the grammar those flags select. `validate_pattern` takes
/// this rather than a raw list, so "forgot to derive the mode from the flags"
/// cannot be written.
pub type RegexFlags {
  RegexFlags(mode: RegexMode, flags: List(String))
}

/// The single place `RegexFlags` is built: reverse the accumulated flags back
/// into source order, reject `u` + `v` together, and pin the mode they select.
/// `pos` is the byte offset of the flags for the exclusivity error.
fn regex_flags(
  seen: List(String),
  pos: Int,
) -> Result(RegexFlags, PatternError) {
  let flags = list.reverse(seen)
  case list.contains(flags, "u"), list.contains(flags, "v") {
    True, True -> Error(ExclusiveUnicodeFlags(pos))
    True, False -> Ok(RegexFlags(mode: Unicode, flags:))
    False, True -> Ok(RegexFlags(mode: UnicodeSets, flags:))
    False, False -> Ok(RegexFlags(mode: Legacy, flags:))
  }
}

/// Validate a whole flags string (e.g. the second argument of
/// `new RegExp(pattern, flags)`): every character must belong to the flag
/// alphabet and none may repeat. Positions in the returned error are byte
/// offsets into `flags`. This shares `scan_regex_flags` with the literal
/// scanner, so the flag alphabet and duplicate detection live in one place.
pub fn validate_flags(flags: String) -> Result(RegexFlags, PatternError) {
  let bytes = <<flags:utf8>>
  use #(end, seen) <- result.try(scan_regex_flags(bytes, 0, []))
  use Nil <- result.try(case end >= bit_array.byte_size(bytes) {
    True -> Ok(Nil)
    False -> Error(InvalidFlag(end, grapheme_at(bytes, end)))
  })
  regex_flags(seen, 0)
}

/// The full (possibly non-ASCII) grapheme starting at byte `pos`, for
/// reporting the offending character of an `InvalidFlag`.
fn grapheme_at(bytes: BitArray, pos: Int) -> String {
  bit_array.slice(bytes, pos, bit_array.byte_size(bytes) - pos)
  |> result.try(bit_array.to_string)
  |> result.try(string.first)
  |> result.unwrap("")
}

/// Scan regex flags after the closing /, returning the position past them and
/// the validated flags. Rejects duplicate and mutually exclusive flags. The
/// literal scanner's entry point into the same alphabet + duplicate detection
/// that `validate_flags` uses.
pub fn skip_regex_flags(
  bytes: BitArray,
  pos: Int,
) -> Result(#(Int, RegexFlags), PatternError) {
  use #(end, seen) <- result.try(scan_regex_flags(bytes, pos, []))
  use flags <- result.map(regex_flags(seen, pos))
  #(end, flags)
}

/// The single owner of the flag alphabet: consume flag characters from `pos`,
/// rejecting duplicates, and stop at the first character that is not a flag.
/// The accumulated list is in reverse source order — `regex_flags` is the only
/// consumer, and it un-reverses.
fn scan_regex_flags(
  bytes: BitArray,
  pos: Int,
  seen: List(String),
) -> Result(#(Int, List(String)), PatternError) {
  case ascii_at(bytes, pos) {
    Some("g" as ch)
    | Some("i" as ch)
    | Some("m" as ch)
    | Some("s" as ch)
    | Some("u" as ch)
    | Some("v" as ch)
    | Some("y" as ch)
    | Some("d" as ch) ->
      case list.contains(seen, ch) {
        True -> Error(DuplicateFlag(pos, ch))
        False -> scan_regex_flags(bytes, pos + 1, [ch, ..seen])
      }
    _ -> Ok(#(pos, seen))
  }
}

// ---- Unicode property escapes (\p{...} / \P{...}) ----

/// Result of looking up a property name against the ECMA-262 tables
/// (general categories, scripts, binary properties, string properties).
type PropertyEscapeKind {
  PropValid
  PropString
  PropInvalid
}

/// Strict (exact-case, no loose matching) lookup of a lone property name:
/// a General_Category value, binary property, or binary property of strings.
@external(erlang, "arc_regex_props_ffi", "classify_lone")
fn classify_lone_property(name: String) -> PropertyEscapeKind

/// Strict lookup of a name=value pair: only gc/sc/scx (and long forms)
/// accept values.
@external(erlang, "arc_regex_props_ffi", "classify_pair")
fn classify_pair_property(name: String, value: String) -> PropertyEscapeKind

/// True for UnicodePropertyValueCharacter: a-z, A-Z, 0-9, _.
fn is_property_char(ch: String) -> Bool {
  case ch {
    "_" -> True
    _ ->
      case string.to_utf_codepoints(ch) {
        [cp] -> digits.is_ascii_alnum_code(string.utf_codepoint_to_int(cp))
        _ -> False
      }
  }
}

/// Skip a run of property name/value characters.
fn skip_property_chars(bytes: BitArray, pos: Int, end: Int) -> Int {
  let is_property_at = case ascii_in(bytes, pos, end) {
    Some(ch) -> is_property_char(ch)
    None -> False
  }
  case pos < end && is_property_at {
    True -> skip_property_chars(bytes, pos + 1, end)
    False -> pos
  }
}

/// Validate a property escape at `pos` (the backslash of \p or \P), returning
/// its total byte length. `allow_strings` is True only for a non-negated \p
/// outside negated classes in v mode — the only place a property of strings
/// (e.g. RGI_Emoji) is legal.
fn property_escape_length(
  bytes: BitArray,
  pos: Int,
  end: Int,
  allow_strings allow_strings: Bool,
) -> Result(Int, PatternError) {
  let invalid = InvalidPropertyName(pos)
  case ascii_in(bytes, pos + 2, end) {
    Some("{") -> {
      let name_end = skip_property_chars(bytes, pos + 3, end)
      let name = source_slice(bytes, pos + 3, name_end - pos - 3)
      case ascii_in(bytes, name_end, end) {
        Some("}") ->
          case classify_lone_property(name) {
            PropValid -> Ok(name_end + 1 - pos)
            PropString ->
              case allow_strings {
                True -> Ok(name_end + 1 - pos)
                False -> Error(PropertyOfStringsRequiresVFlag(pos))
              }
            PropInvalid -> Error(invalid)
          }
        Some("=") -> {
          let value_end = skip_property_chars(bytes, name_end + 1, end)
          let value =
            source_slice(bytes, name_end + 1, value_end - name_end - 1)
          case ascii_in(bytes, value_end, end) {
            Some("}") ->
              case classify_pair_property(name, value) {
                PropValid -> Ok(value_end + 1 - pos)
                PropString | PropInvalid -> Error(invalid)
              }
            _ -> Error(invalid)
          }
        }
        _ -> Error(invalid)
      }
    }
    // In Unicode mode \p must be followed by {…} — \p alone or \pL is a
    // SyntaxError (IdentityEscape excludes p/P with the u or v flag).
    _ -> Error(invalid)
  }
}

// ---- v flag (unicodeSets) property escape validation ----

/// Validate property escapes in a /v-flagged pattern body. Deliberately
/// narrow: walks escapes and class nesting only, so valid v-mode syntax
/// (nested classes, &&, --) is never falsely rejected. Enforces:
/// - \p{...}/\P{...} names must be valid (same tables as u mode)
/// - properties of strings are only legal in a non-negated \p outside
///   negated character classes
fn validate_regex_vmode_body(
  bytes: BitArray,
  pos: Int,
  end: Int,
) -> Result(Nil, PatternError) {
  validate_vmode_loop(bytes, pos, end, [])
}

fn validate_vmode_loop(
  bytes: BitArray,
  pos: Int,
  end: Int,
  class_negations: List(Bool),
) -> Result(Nil, PatternError) {
  case pos >= end {
    True -> Ok(Nil)
    False ->
      case ascii_in(bytes, pos, end) {
        Some("\\") ->
          case ascii_in(bytes, pos + 1, end) {
            Some("p") | Some("P") -> {
              let in_negated_class = list.contains(class_negations, True)
              let allow_strings =
                ascii_in(bytes, pos + 1, end) == Some("p") && !in_negated_class
              use len <- result.try(property_escape_length(
                bytes,
                pos,
                end,
                allow_strings:,
              ))
              validate_vmode_loop(bytes, pos + len, end, class_negations)
            }
            _ -> validate_vmode_loop(bytes, pos + 2, end, class_negations)
          }
        Some("[") ->
          case ascii_in(bytes, pos + 1, end) {
            Some("^") ->
              validate_vmode_loop(bytes, pos + 2, end, [True, ..class_negations])
            _ ->
              validate_vmode_loop(bytes, pos + 1, end, [
                False,
                ..class_negations
              ])
          }
        Some("]") ->
          case class_negations {
            [_, ..rest] -> validate_vmode_loop(bytes, pos + 1, end, rest)
            [] -> validate_vmode_loop(bytes, pos + 1, end, [])
          }
        _ -> validate_vmode_loop(bytes, pos + 1, end, class_negations)
      }
  }
}
