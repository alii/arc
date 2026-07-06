/// The typed error every regex scanner / pattern validator can report, and
/// its rendering.
///
/// This lives in its own module (rather than in `arc/parser/regex`) so that
/// `arc/parser/error` — which wraps `PatternError` in its `RegExpSyntaxError`
/// variant — can import it without pulling in the whole scanner.
/// Every syntax error the regex scanner / pattern validator can report.
/// One variant per distinct diagnostic; `pattern_error_message` is the single
/// place each is rendered to its user-facing string, and `pattern_error_pos`
/// gives the byte offset of the offending construct in the scanned source
/// (absolute for a regex literal, pattern-relative for `new RegExp(src)`).
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
