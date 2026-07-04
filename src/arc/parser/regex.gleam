/// Regex literal scanning and /u flag validation.
/// Pure bit-array scanning functions split from parser.gleam.
/// The parser re-lexes regex literals from source bytes since the lexer
/// can't always distinguish / (divide) from / (regex start).
import arc/parser/lexer
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

/// The lone ASCII character at byte `pos`, or `None` when there isn't one —
/// `pos` is out of bounds, or the byte there belongs to a multi-byte code
/// point. `None` is the *only* way absence is signalled: there is no empty
/// character, so a caller can never confuse "nothing here" with a real char.
pub fn ascii_at(bytes: BitArray, pos: Int) -> Option(String) {
  case bit_array.slice(bytes, pos, 1) {
    Ok(<<byte>>) if byte < 0x80 ->
      option.from_result(bit_array.to_string(<<byte>>))
    Ok(_) | Error(Nil) -> None
  }
}

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

/// O(1) byte slice from the source bytes.
pub fn byte_slice_source(bytes: BitArray, start: Int, len: Int) -> String {
  case bit_array.slice(bytes, start, len) {
    Ok(s) ->
      case bit_array.to_string(s) {
        Ok(str) -> str
        Error(Nil) -> ""
      }
    Error(Nil) -> ""
  }
}

/// Value of an ASCII hex digit (0-9, a-f, A-F).
fn hex_value(ch: String) -> Option(Int) {
  case ch {
    "0" -> Some(0)
    "1" -> Some(1)
    "2" -> Some(2)
    "3" -> Some(3)
    "4" -> Some(4)
    "5" -> Some(5)
    "6" -> Some(6)
    "7" -> Some(7)
    "8" -> Some(8)
    "9" -> Some(9)
    "a" | "A" -> Some(10)
    "b" | "B" -> Some(11)
    "c" | "C" -> Some(12)
    "d" | "D" -> Some(13)
    "e" | "E" -> Some(14)
    "f" | "F" -> Some(15)
    _ -> None
  }
}

/// Value of an ASCII decimal digit.
fn digit_value(ch: String) -> Option(Int) {
  case ch {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> hex_value(ch)
    _ -> None
  }
}

/// Value of the hex digit at `pos`, if the byte there is one.
fn hex_at(bytes: BitArray, pos: Int) -> Option(Int) {
  case ascii_at(bytes, pos) {
    Some(ch) -> hex_value(ch)
    None -> None
  }
}

fn is_hex_at(bytes: BitArray, pos: Int) -> Bool {
  option.is_some(hex_at(bytes, pos))
}

/// Value of the decimal digit at `pos`, if the byte there is one.
fn digit_at(bytes: BitArray, pos: Int) -> Option(Int) {
  case ascii_at(bytes, pos) {
    Some(ch) -> digit_value(ch)
    None -> None
  }
}

fn is_digit_at(bytes: BitArray, pos: Int) -> Bool {
  option.is_some(digit_at(bytes, pos))
}

/// Scan a run of decimal digits in `[pos, end)`, returning the position after
/// the run and its value — `Some` iff the run was non-empty. The value is
/// accumulated as we walk, so there is no re-slice + re-parse step that could
/// fail and silently yield 0.
fn decimal_run(bytes: BitArray, pos: Int, end: Int) -> #(Int, Option(Int)) {
  digit_run_loop(bytes, pos, end, 10, digit_at, None)
}

/// Same, for a run of hex digits.
fn hex_run(bytes: BitArray, pos: Int, end: Int) -> #(Int, Option(Int)) {
  digit_run_loop(bytes, pos, end, 16, hex_at, None)
}

fn digit_run_loop(
  bytes: BitArray,
  pos: Int,
  end: Int,
  base: Int,
  digit: fn(BitArray, Int) -> Option(Int),
  acc: Option(Int),
) -> #(Int, Option(Int)) {
  case pos < end, digit(bytes, pos) {
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
        False -> scan_regex_source(bytes, pos + utf8_byte_width(b), in_class)
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
                  scan_regex_source(
                    bytes,
                    pos + 1 + utf8_byte_width(nb),
                    in_class,
                  )
              }
            Ok(<<nb>>) ->
              scan_regex_source(bytes, pos + 1 + utf8_byte_width(nb), in_class)
            Ok(_) -> scan_regex_source(bytes, pos + 2, in_class)
          }
        }
        Some("[") -> scan_regex_source(bytes, pos + 1, True)
        Some("]") ->
          case in_class {
            True -> scan_regex_source(bytes, pos + 1, False)
            False -> scan_regex_source(bytes, pos + 1, in_class)
          }
        Some("/") ->
          case in_class {
            True -> scan_regex_source(bytes, pos + 1, in_class)
            False -> Ok(pos + 1)
          }
        _ -> scan_regex_source(bytes, pos + 1, in_class)
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

/// Regex compile mode derived from the flags.
type RegexMode {
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
pub fn validate_pattern(
  bytes: BitArray,
  start: Int,
  end: Int,
  flags: List(String),
) -> Result(Nil, PatternError) {
  let has_u = list.contains(flags, "u")
  let has_v = list.contains(flags, "v")
  use Nil <- result.try(case has_u && has_v {
    True -> Error(ExclusiveUnicodeFlags(start))
    False -> Ok(Nil)
  })
  let mode = case has_v, has_u {
    True, _ -> UnicodeSets
    False, True -> Unicode
    False, False -> Legacy
  }
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
      case ascii_at(bytes, pos), class_depth {
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
          case ascii_at(bytes, pos + 1) {
            Some("?") ->
              case ascii_at(bytes, pos + 2) {
                Some("<") ->
                  case ascii_at(bytes, pos + 3) {
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
  case pos2 < ctx.end && ascii_at(ctx.bytes, pos2) == Some("|") {
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
      case ascii_at(ctx.bytes, pos) {
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
  case ascii_at(ctx.bytes, pos) {
    Some("^") | Some("$") -> Ok(#(pos + 1, [], KAssertion))
    Some("\\") ->
      case ascii_at(ctx.bytes, pos + 1) {
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
  case ascii_at(ctx.bytes, pos + 1) {
    Some("?") ->
      case ascii_at(ctx.bytes, pos + 2) {
        Some("=") | Some("!") -> {
          use #(pos2, names) <- result.map(p_group_body(ctx, pos + 3))
          #(pos2, names, KLookahead)
        }
        Some(":") -> {
          use #(pos2, names) <- result.map(p_group_body(ctx, pos + 3))
          #(pos2, names, KAtom)
        }
        Some("<") ->
          case ascii_at(ctx.bytes, pos + 3) {
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
  case pos2 < ctx.end && ascii_at(ctx.bytes, pos2) == Some(")") {
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
      case ascii_at(ctx.bytes, pos) {
        Some("*") | Some("+") | Some("?") ->
          Ok(#(skip_lazy(ctx.bytes, pos + 1), True))
        Some("{") ->
          case braced_quantifier(ctx.bytes, pos, ctx.end) {
            Some(BracedQuantifier(after:, min:, max:)) ->
              case max {
                Some(m) if m < min -> Error(OutOfOrderQuantifier(pos))
                _ -> Ok(#(skip_lazy(ctx.bytes, after), True))
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

fn skip_lazy(bytes: BitArray, pos: Int) -> Int {
  case ascii_at(bytes, pos) {
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
      case ascii_at(bytes, lo_end) {
        Some("}") -> Some(BracedQuantifier(after: lo_end + 1, min:, max: None))
        Some(",") -> {
          let #(hi_end, max) = decimal_run(bytes, lo_end + 1, end)
          case ascii_at(bytes, hi_end) {
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
      case strict && is_digit_at(ctx.bytes, pos + 2) {
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
        UnicodeSets -> {
          let allow = ascii_at(ctx.bytes, pos + 1) == Some("p")
          use len <- result.map(property_escape_length(
            ctx.bytes,
            pos,
            ctx.end,
            allow_strings: allow,
          ))
          pos + len
        }
      }
    Ascii("c") ->
      case is_letter_at(ctx.bytes, pos + 2) {
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
      case is_hex_at(ctx.bytes, pos + 2) && is_hex_at(ctx.bytes, pos + 3) {
        True -> Ok(pos + 4)
        False ->
          case strict {
            True -> Error(InvalidHexEscape(pos))
            False -> Ok(pos + 2)
          }
      }
    Ascii("u") -> p_unicode_escape(ctx, pos)
    Ascii("k") ->
      case strict || ctx.has_named {
        True ->
          case ascii_at(ctx.bytes, pos + 2) {
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
fn p_unicode_escape(ctx: Ctx, pos: Int) -> Result(Int, PatternError) {
  let bytes = ctx.bytes
  let hex4 =
    is_hex_at(bytes, pos + 2)
    && is_hex_at(bytes, pos + 3)
    && is_hex_at(bytes, pos + 4)
    && is_hex_at(bytes, pos + 5)
  case hex4 {
    True -> Ok(pos + 6)
    False ->
      case ctx.mode {
        Legacy -> Ok(pos + 2)
        Unicode | UnicodeSets ->
          case ascii_at(bytes, pos + 2) {
            Some("{") -> {
              let #(after, value) = hex_run(bytes, pos + 3, ctx.end)
              case value, ascii_at(bytes, after) {
                Some(v), Some("}") if v > 0x10FFFF ->
                  Error(InvalidUnicodeEscapeValue(pos))
                Some(_), Some("}") -> Ok(after + 1)
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
    Legacy | Unicode -> {
      let mode = case ctx.mode {
        Unicode -> UnicodeClass
        Legacy | UnicodeSets -> LegacyClass
      }
      let pos2 = case ascii_at(ctx.bytes, pos) {
        Some("^") -> pos + 1
        _ -> pos
      }
      p_class_ranges(ctx, mode, pos2)
    }
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
      case ascii_at(ctx.bytes, pos) {
        Some("]") -> Ok(pos + 1)
        _ -> {
          use #(after_a, a_is_class, a_val) <- result.try(p_class_atom(
            ctx,
            mode,
            pos,
          ))
          let is_range =
            ascii_at(ctx.bytes, after_a) == Some("-")
            && after_a + 1 < ctx.end
            && ascii_at(ctx.bytes, after_a + 1) != Some("]")
          case is_range {
            True -> {
              use #(after_b, b_is_class, b_val) <- result.try(p_class_atom(
                ctx,
                mode,
                after_a + 1,
              ))
              // Unicode mode: \d \s \w \p{..} etc. are not valid range
              // endpoints. Annex B treats such a `-` as a literal.
              let bad = mode == UnicodeClass && { a_is_class || b_is_class }
              use Nil <- result.try(case bad {
                True -> Error(InvalidClassRange(pos))
                False -> Ok(Nil)
              })
              use Nil <- result.try(check_range_order(mode, a_val, b_val, pos))
              p_class_ranges(ctx, mode, after_b)
            }
            False -> p_class_ranges(ctx, mode, after_a)
          }
        }
      }
  }
}

/// Range endpoints that are plain literal characters must be in order.
/// Escaped endpoints are skipped (their values aren't decoded here), as are
/// non-BMP literals in legacy mode (which split into surrogate halves).
fn check_range_order(
  mode: ClassMode,
  a_val: Option(Int),
  b_val: Option(Int),
  pos: Int,
) -> Result(Nil, PatternError) {
  case a_val, b_val {
    Some(av), Some(bv) -> {
      let unknowable = mode == LegacyClass && { av > 0xFFFF || bv > 0xFFFF }
      case !unknowable && av > bv {
        True -> Error(OutOfOrderClassRange(pos))
        False -> Ok(Nil)
      }
    }
    _, _ -> Ok(Nil)
  }
}

/// One class atom: #(position_after, is_character_class_escape,
/// literal_codepoint). The codepoint is Some only for plain (non-escape)
/// characters — used for the range-order check.
fn p_class_atom(
  ctx: Ctx,
  mode: ClassMode,
  pos: Int,
) -> Result(#(Int, Bool, Option(Int)), PatternError) {
  case pos >= ctx.end {
    True -> Error(UnterminatedClass(pos))
    False ->
      case ascii_at(ctx.bytes, pos) {
        Some("\\") -> {
          use #(after, is_class) <- result.map(p_class_escape(ctx, mode, pos))
          #(after, is_class, None)
        }
        _ ->
          case codepoint_at(ctx.bytes, pos) {
            Cp(cp, w) -> Ok(#(pos + w, False, Some(cp)))
            Eof -> Error(UnterminatedClass(pos))
          }
      }
  }
}

/// ClassEscape at `pos` (the backslash) inside a legacy or u-mode class.
fn p_class_escape(
  ctx: Ctx,
  mode: ClassMode,
  pos: Int,
) -> Result(#(Int, Bool), PatternError) {
  let strict = mode == UnicodeClass
  let invalid = InvalidClassEscape(pos)
  case at(ctx.bytes, pos + 1, ctx.end) {
    AtEnd -> Error(UnterminatedClass(pos))
    // An escaped non-ASCII character: an IdentityEscape only in Annex B.
    NonAscii(w) ->
      case strict {
        True -> Error(invalid)
        False -> Ok(#(pos + 1 + w, False))
      }
    Ascii("b") | Ascii("-") -> Ok(#(pos + 2, False))
    Ascii("d")
    | Ascii("D")
    | Ascii("s")
    | Ascii("S")
    | Ascii("w")
    | Ascii("W") -> Ok(#(pos + 2, True))
    Ascii("p") | Ascii("P") ->
      case mode {
        LegacyClass -> Ok(#(pos + 2, False))
        UnicodeClass -> {
          use len <- result.map(property_escape_length(
            ctx.bytes,
            pos,
            ctx.end,
            allow_strings: False,
          ))
          #(pos + len, True)
        }
      }
    Ascii("f") | Ascii("n") | Ascii("r") | Ascii("t") | Ascii("v") ->
      Ok(#(pos + 2, False))
    Ascii("c") ->
      case is_letter_at(ctx.bytes, pos + 2) {
        True -> Ok(#(pos + 3, False))
        False ->
          case strict {
            True -> Error(invalid)
            False ->
              // Annex B ClassControlLetter also allows digits and `_`;
              // otherwise the backslash alone is a class atom matching `\`.
              case
                is_digit_at(ctx.bytes, pos + 2)
                || ascii_at(ctx.bytes, pos + 2) == Some("_")
              {
                True -> Ok(#(pos + 3, False))
                False -> Ok(#(pos + 1, False))
              }
          }
      }
    Ascii("x") ->
      case is_hex_at(ctx.bytes, pos + 2) && is_hex_at(ctx.bytes, pos + 3) {
        True -> Ok(#(pos + 4, False))
        False ->
          case strict {
            True -> Error(InvalidHexEscape(pos))
            False -> Ok(#(pos + 2, False))
          }
      }
    Ascii("u") -> {
      use after <- result.map(p_unicode_escape(ctx, pos))
      #(after, False)
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
        // Annex B: legacy octal escapes and \8 \9 identity.
        False -> Ok(#(pos + 2, False))
        True ->
          case
            ascii_at(ctx.bytes, pos + 1) == Some("0")
            && !is_digit_at(ctx.bytes, pos + 2)
          {
            True -> Ok(#(pos + 2, False))
            False -> Error(invalid)
          }
      }
    Ascii("k") ->
      // IdentityEscape excludes `k` whenever the pattern has named groups
      // (and \k is never a ClassEscape group reference).
      case strict || ctx.has_named {
        True -> Error(invalid)
        False -> Ok(#(pos + 2, False))
      }
    Ascii(other) ->
      case strict {
        True ->
          case is_syntax_char(other) || other == "/" {
            True -> Ok(#(pos + 2, False))
            False -> Error(invalid)
          }
        False -> Ok(#(pos + 2, False))
      }
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
  let pos2 = case ascii_at(bytes, pos) {
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
  let nxt = ascii_at(bytes, pos + 1)
  case pos >= end, ascii_at(bytes, pos) {
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
          case ascii_at(bytes, pos + 2) {
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
          let after2 = ascii_at(bytes, pos + 2)
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
          let after2 = ascii_at(bytes, pos + 2)
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
      case ascii_at(bytes, pos) {
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

fn group_name_loop(
  bytes: BitArray,
  pos: Int,
  end: Int,
  is_first: Bool,
  acc: List(Int),
) -> Result(#(String, Int), PatternError) {
  case pos >= end, ascii_at(bytes, pos) {
    True, _ -> Error(UnterminatedGroupName(pos))
    _, Some(">") ->
      case is_first {
        True -> Error(EmptyGroupName(pos))
        False -> Ok(#(codepoints_to_string(list.reverse(acc)), pos + 1))
      }
    _, Some("\\") -> {
      use #(cp0, next0) <- result.try(decode_name_escape(bytes, pos))
      // Combine a surrogate pair written as two \uXXXX escapes.
      let #(cp, next) = case cp0 >= 0xD800 && cp0 <= 0xDBFF {
        True ->
          case decode_name_escape(bytes, next0) {
            Ok(#(trail, next1)) if trail >= 0xDC00 && trail <= 0xDFFF -> #(
              0x10000 + { cp0 - 0xD800 } * 1024 + trail - 0xDC00,
              next1,
            )
            _ -> #(cp0, next0)
          }
        False -> #(cp0, next0)
      }
      case lexer.validate_identifier_codepoint(cp, is_first) {
        True -> group_name_loop(bytes, next, end, False, [cp, ..acc])
        False -> Error(InvalidGroupName(pos))
      }
    }
    _, _ ->
      case codepoint_at(bytes, pos) {
        Cp(cp, width) ->
          case lexer.validate_identifier_codepoint(cp, is_first) {
            True -> group_name_loop(bytes, pos + width, end, False, [cp, ..acc])
            False -> Error(InvalidGroupName(pos))
          }
        Eof -> Error(UnterminatedGroupName(pos))
      }
  }
}

fn codepoints_to_string(cps: List(Int)) -> String {
  cps
  |> list.filter_map(string.utf_codepoint)
  |> string.from_utf_codepoints
}

fn decode_name_escape(
  bytes: BitArray,
  pos: Int,
) -> Result(#(Int, Int), PatternError) {
  case ascii_at(bytes, pos + 1) {
    Some("u") ->
      case ascii_at(bytes, pos + 2) {
        Some("{") -> {
          let #(after, value) = hex_run(bytes, pos + 3, pos + 100)
          case value, ascii_at(bytes, after) {
            // Reject code points above U+10FFFF here — downstream identifier
            // checks assume a valid Unicode scalar range.
            Some(cp), Some("}") if cp <= 0x10FFFF -> Ok(#(cp, after + 1))
            _, _ -> Error(InvalidGroupName(pos))
          }
        }
        _ -> {
          // Exactly four hex digits: `hex_run` bounded to `pos + 6` cannot
          // consume more, and `after == pos + 6` rejects any shorter run.
          let #(after, value) = hex_run(bytes, pos + 2, pos + 6)
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
  case ascii_at(bytes, pos2) {
    Some(":") -> Ok(pos2 + 1)
    Some("-") -> {
      use #(pos3, remove) <- result.try(p_mod_flags(bytes, pos2 + 1, end, []))
      use Nil <- result.try(case ascii_at(bytes, pos3) {
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
      case ascii_at(bytes, pos) {
        Some("i" as f) | Some("m" as f) | Some("s" as f) ->
          case list.contains(seen, f) {
            True -> Error(RepeatedModifierFlag(pos))
            False -> p_mod_flags(bytes, pos + 1, end, [f, ..seen])
          }
        _ -> Ok(#(pos, seen))
      }
  }
}

/// True when the byte at `pos` is an ASCII letter (a-z A-Z).
fn is_letter_at(bytes: BitArray, pos: Int) -> Bool {
  case ascii_at(bytes, pos) {
    Some(ch) -> is_ascii_letter(ch)
    None -> False
  }
}

/// True for a-z A-Z.
fn is_ascii_letter(ch: String) -> Bool {
  case string.to_utf_codepoints(ch) {
    [cp] -> {
      let c = string.utf_codepoint_to_int(cp)
      { c >= 0x41 && c <= 0x5A } || { c >= 0x61 && c <= 0x7A }
    }
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

/// The decoded flags of a regular expression. Produced only by
/// `validate_flags`, so holding one proves the flags string contained no
/// unknown or duplicate flag.
pub type RegexFlags {
  RegexFlags(has_u: Bool, has_v: Bool, flags: List(String))
}

/// Validate a whole flags string (e.g. the second argument of
/// `new RegExp(pattern, flags)`): every character must belong to the flag
/// alphabet and none may repeat. Positions in the returned error are byte
/// offsets into `flags`. This shares `scan_regex_flags` with the literal
/// scanner, so the flag alphabet and duplicate detection live in one place.
pub fn validate_flags(flags: String) -> Result(RegexFlags, PatternError) {
  let bytes = <<flags:utf8>>
  use #(end, seen) <- result.try(scan_regex_flags(bytes, 0, []))
  case end >= bit_array.byte_size(bytes) {
    True ->
      Ok(RegexFlags(
        has_u: list.contains(seen, "u"),
        has_v: list.contains(seen, "v"),
        flags: seen,
      ))
    False -> Error(InvalidFlag(end, grapheme_at(bytes, end)))
  }
}

/// The full (possibly non-ASCII) grapheme starting at byte `pos`, for
/// reporting the offending character of an `InvalidFlag`.
fn grapheme_at(bytes: BitArray, pos: Int) -> String {
  bit_array.slice(bytes, pos, bit_array.byte_size(bytes) - pos)
  |> result.try(bit_array.to_string)
  |> result.try(string.first)
  |> result.unwrap("")
}

/// Scan regex flags after the closing /, returning end position and flag list.
/// Rejects duplicate flags. The literal scanner's entry point into the same
/// alphabet + duplicate detection that `validate_flags` uses.
pub fn skip_regex_flags(
  bytes: BitArray,
  pos: Int,
) -> Result(#(Int, List(String)), PatternError) {
  scan_regex_flags(bytes, pos, [])
}

/// The single owner of the flag alphabet: consume flag characters from `pos`,
/// rejecting duplicates, and stop at the first character that is not a flag.
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
pub type PropertyEscapeKind {
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
        [cp] -> {
          let c = string.utf_codepoint_to_int(cp)
          { c >= 0x41 && c <= 0x5A }
          || { c >= 0x61 && c <= 0x7A }
          || { c >= 0x30 && c <= 0x39 }
        }
        _ -> False
      }
  }
}

/// Skip a run of property name/value characters.
fn skip_property_chars(bytes: BitArray, pos: Int, end: Int) -> Int {
  let is_property_at = case ascii_at(bytes, pos) {
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
  case ascii_at(bytes, pos + 2) {
    Some("{") -> {
      let name_end = skip_property_chars(bytes, pos + 3, end)
      let name = byte_slice_source(bytes, pos + 3, name_end - pos - 3)
      case ascii_at(bytes, name_end) {
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
            byte_slice_source(bytes, name_end + 1, value_end - name_end - 1)
          case ascii_at(bytes, value_end) {
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
pub fn validate_regex_vmode_body(
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
      case ascii_at(bytes, pos) {
        Some("\\") ->
          case ascii_at(bytes, pos + 1) {
            Some("p") | Some("P") -> {
              let in_negated_class = list.contains(class_negations, True)
              let allow_strings =
                ascii_at(bytes, pos + 1) == Some("p") && !in_negated_class
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
          case ascii_at(bytes, pos + 1) {
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
