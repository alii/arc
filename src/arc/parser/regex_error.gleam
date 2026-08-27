pub type PatternError {
  UnterminatedRegex(pos: Int)
  DuplicateFlag(pos: Int, flag: String)
  InvalidFlag(pos: Int, flag: String)
  ExclusiveUnicodeFlags(pos: Int)
  UnmatchedParen(pos: Int)
  NothingToRepeat(pos: Int)
  DuplicateGroupName(pos: Int, name: String)
  LoneQuantifierBrackets(pos: Int)
  LoneClassBracket(pos: Int)
  MissingClosingParen(pos: Int)
  OutOfOrderQuantifier(pos: Int)
  InvalidEscape(pos: Int)
  BackReferenceOutOfRange(pos: Int, n: Int, captures: Int)
  InvalidDecimalEscape(pos: Int)
  InvalidHexEscape(pos: Int)
  InvalidNamedReference(pos: Int)
  BackslashAtEnd(pos: Int)
  InvalidUnicodeEscape(pos: Int)
  InvalidUnicodeEscapeValue(pos: Int)
  UnterminatedClass(pos: Int)
  InvalidClassRange(pos: Int)
  OutOfOrderClassRange(pos: Int)
  InvalidClassEscape(pos: Int)
  InvalidClassSetOperation(pos: Int)
  InvalidClassCharacter(pos: Int)
  ReservedDoublePunctuator(pos: Int)
  UnterminatedGroupName(pos: Int)
  EmptyGroupName(pos: Int)
  InvalidGroupName(pos: Int)
  InvalidModifierFlags(pos: Int)
  EmptyModifiers(pos: Int)
  RepeatedModifierFlag(pos: Int)
  InvalidPropertyName(pos: Int)
  PropertyOfStringsRequiresVFlag(pos: Int)
}

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

pub fn pattern_error_pos(e: PatternError) -> Int {
  e.pos
}
