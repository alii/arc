import arc/internal/bytes as source_bytes
import arc/internal/digits
import arc/internal/utf16
import arc/parser/lexer
import arc/parser/regex_error.{
  type PatternError, BackReferenceOutOfRange, BackslashAtEnd, DuplicateFlag,
  DuplicateGroupName, EmptyGroupName, EmptyModifiers, ExclusiveUnicodeFlags,
  InvalidClassCharacter, InvalidClassEscape, InvalidClassRange,
  InvalidClassSetOperation, InvalidDecimalEscape, InvalidEscape, InvalidFlag,
  InvalidGroupName, InvalidHexEscape, InvalidModifierFlags,
  InvalidNamedReference, InvalidPropertyName, InvalidUnicodeEscape,
  InvalidUnicodeEscapeValue, LoneClassBracket, LoneQuantifierBrackets,
  MissingClosingParen, NothingToRepeat, OutOfOrderClassRange,
  OutOfOrderQuantifier, PropertyOfStringsRequiresVFlag, RepeatedModifierFlag,
  ReservedDoublePunctuator, UnmatchedParen, UnterminatedClass,
  UnterminatedGroupName, UnterminatedRegex,
}
import gleam/bit_array
import gleam/bool
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub type RegexMode {
  Legacy
  Unicode
  UnicodeSets
}

pub type RegexFlags {
  RegexFlags(mode: RegexMode, text: String)
}

// returns the position just past the closing /
pub fn skip_regex_body(bytes: BitArray, pos: Int) -> Result(Int, PatternError) {
  case bytes {
    <<_:bytes-size(pos), rest:bytes>> -> skip_regex_body_loop(rest, pos, False)
    _ -> Error(UnterminatedRegex(pos))
  }
}

// line terminators end it, even escaped; trail bytes fall through
fn skip_regex_body_loop(
  rest: BitArray,
  pos: Int,
  in_class: Bool,
) -> Result(Int, PatternError) {
  case rest {
    <<0x5c, 0x0a, _:bytes>> | <<0x5c, 0x0d, _:bytes>> ->
      Error(UnterminatedRegex(pos + 1))
    <<0x5c, 0xe2, 0x80, 0xa8, _:bytes>> | <<0x5c, 0xe2, 0x80, 0xa9, _:bytes>> ->
      Error(UnterminatedRegex(pos + 1))
    <<0x5c, _, rest:bytes>> -> skip_regex_body_loop(rest, pos + 2, in_class)
    <<0x5c, _:bytes>> -> Error(UnterminatedRegex(pos + 1))
    <<0x5b, rest:bytes>> -> skip_regex_body_loop(rest, pos + 1, True)
    <<0x5d, rest:bytes>> -> skip_regex_body_loop(rest, pos + 1, False)
    <<0x2f, _:bytes>> if !in_class -> Ok(pos + 1)
    <<0x0a, _:bytes>> | <<0x0d, _:bytes>> -> Error(UnterminatedRegex(pos))
    <<0xe2, 0x80, 0xa8, _:bytes>> | <<0xe2, 0x80, 0xa9, _:bytes>> ->
      Error(UnterminatedRegex(pos))
    <<_, rest:bytes>> -> skip_regex_body_loop(rest, pos + 1, in_class)
    _ -> Error(UnterminatedRegex(pos))
  }
}

pub fn skip_regex_flags(
  bytes: BitArray,
  pos: Int,
) -> Result(#(Int, RegexFlags), PatternError) {
  use #(end, seen) <- result.try(scan_regex_flags(bytes, pos, []))
  use mode <- result.map(mode_of_flags(seen, pos))
  #(
    end,
    RegexFlags(mode:, text: source_bytes.unsafe_slice(bytes, pos, end - pos)),
  )
}

pub fn validate_flags(flags: String) -> Result(RegexFlags, PatternError) {
  let bytes = <<flags:utf8>>
  use #(end, seen) <- result.try(scan_regex_flags(bytes, 0, []))
  use Nil <- result.try(case end >= bit_array.byte_size(bytes) {
    True -> Ok(Nil)
    False -> Error(InvalidFlag(end, grapheme_at(bytes, end)))
  })
  use mode <- result.map(mode_of_flags(seen, 0))
  RegexFlags(mode:, text: flags)
}

fn scan_regex_flags(
  bytes: BitArray,
  pos: Int,
  seen: List(String),
) -> Result(#(Int, List(String)), PatternError) {
  case source_bytes.ascii_at(bytes, pos) {
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

fn mode_of_flags(
  seen: List(String),
  pos: Int,
) -> Result(RegexMode, PatternError) {
  case list.contains(seen, "u"), list.contains(seen, "v") {
    True, True -> Error(ExclusiveUnicodeFlags(pos))
    True, False -> Ok(Unicode)
    False, True -> Ok(UnicodeSets)
    False, False -> Ok(Legacy)
  }
}

fn grapheme_at(bytes: BitArray, pos: Int) -> String {
  bit_array.slice(bytes, pos, bit_array.byte_size(bytes) - pos)
  |> result.try(bit_array.to_string)
  |> result.try(string.first)
  |> result.unwrap("")
}

type PatternContext {
  PatternContext(
    bytes: BitArray,
    end: Int,
    mode: RegexMode,
    capture_count: Int,
    group_names: List(String),
    has_named_groups: Bool,
  )
}

// §22.2.1.1 pattern early errors, annex b grammar without u/v
pub fn validate_pattern(
  bytes: BitArray,
  start: Int,
  end: Int,
  flags: RegexFlags,
) -> Result(Nil, PatternError) {
  let ctx =
    PatternContext(
      bytes:,
      end:,
      mode: flags.mode,
      capture_count: 0,
      group_names: [],
      has_named_groups: False,
    )
    |> scan_groups(byte_range(bytes, start, end), start, 0)
  use #(stop, _names) <- result.try(parse_disjunction(ctx, start))
  use <- bool.guard(stop < end, Error(UnmatchedParen(stop)))
  case ctx.mode {
    UnicodeSets -> check_v_mode_property_escapes(ctx, start, [])
    Legacy | Unicode -> Ok(Nil)
  }
}

fn byte_range(bytes: BitArray, start: Int, end: Int) -> BitArray {
  case bytes {
    <<_:bytes-size(start), range:bytes-size(end - start), _:bytes>> -> range
    _ -> <<>>
  }
}

fn rest_of_pattern(ctx: PatternContext, pos: Int) -> BitArray {
  byte_range(ctx.bytes, pos, ctx.end)
}

fn scan_groups(
  ctx: PatternContext,
  rest: BitArray,
  pos: Int,
  class_depth: Int,
) -> PatternContext {
  let classes_nest = ctx.mode == UnicodeSets
  case rest, class_depth {
    <<"\\", _, rest:bytes>>, _ -> scan_groups(ctx, rest, pos + 2, class_depth)
    <<"[", rest:bytes>>, _ if classes_nest ->
      scan_groups(ctx, rest, pos + 1, class_depth + 1)
    <<"[", rest:bytes>>, _ -> scan_groups(ctx, rest, pos + 1, 1)
    <<"]", rest:bytes>>, _ if classes_nest ->
      scan_groups(ctx, rest, pos + 1, int.max(class_depth - 1, 0))
    <<"]", rest:bytes>>, _ -> scan_groups(ctx, rest, pos + 1, 0)
    <<"(?<=", rest:bytes>>, 0 | <<"(?<!", rest:bytes>>, 0 ->
      scan_groups(ctx, rest, pos + 4, 0)
    <<"(?<", rest:bytes>>, 0 -> {
      let group_names = case parse_group_name(ctx, pos + 3) {
        Ok(#(name, _)) -> [name, ..ctx.group_names]
        Error(_malformed_name) -> ctx.group_names
      }
      let ctx =
        PatternContext(
          ..ctx,
          capture_count: ctx.capture_count + 1,
          group_names:,
          has_named_groups: True,
        )
      scan_groups(ctx, rest, pos + 3, 0)
    }
    <<"(?", rest:bytes>>, 0 -> scan_groups(ctx, rest, pos + 2, 0)
    <<"(", rest:bytes>>, 0 -> {
      let ctx = PatternContext(..ctx, capture_count: ctx.capture_count + 1)
      scan_groups(ctx, rest, pos + 1, 0)
    }
    <<_, rest:bytes>>, _ -> scan_groups(ctx, rest, pos + 1, class_depth)
    _, _ -> ctx
  }
}

type CodePoint {
  CodePoint(value: Int, width: Int)
}

fn codepoint_at(ctx: PatternContext, pos: Int) -> Option(CodePoint) {
  case rest_of_pattern(ctx, pos) {
    <<b, _:bytes>> if b < 0x80 -> Some(CodePoint(b, 1))
    <<c:utf8_codepoint, _:bytes>> -> {
      let value = string.utf_codepoint_to_int(c)
      Some(CodePoint(value, codepoint_width(value)))
    }
    <<b, _:bytes>> -> Some(CodePoint(b, 1))
    _ -> None
  }
}

fn codepoint_width(cp: Int) -> Int {
  case cp {
    c if c >= 0x10000 -> 4
    c if c >= 0x800 -> 3
    c if c >= 0x80 -> 2
    _ -> 1
  }
}

fn char_width_at(ctx: PatternContext, pos: Int) -> Int {
  case codepoint_at(ctx, pos) {
    Some(CodePoint(width:, ..)) -> width
    None -> 1
  }
}

// the char after a backslash, split the way the escape tables need it
type EscapedChar {
  Digit(value: Int)
  Ascii(char: String)
  NonAscii(value: Int, width: Int)
  EndOfPattern
}

fn escaped_char_at(ctx: PatternContext, pos: Int) -> EscapedChar {
  case codepoint_at(ctx, pos) {
    None -> EndOfPattern
    Some(CodePoint(value:, width:)) if value >= 0x80 -> NonAscii(value:, width:)
    Some(CodePoint(value:, ..)) if value >= 0x30 && value <= 0x39 ->
      Digit(value - 0x30)
    Some(_) -> Ascii(source_bytes.unsafe_slice(ctx.bytes, pos, 1))
  }
}

fn ascii_at(ctx: PatternContext, pos: Int) -> Option(String) {
  case pos < ctx.end {
    True -> source_bytes.ascii_at(ctx.bytes, pos)
    False -> None
  }
}

fn byte_at(ctx: PatternContext, pos: Int) -> Int {
  case pos < ctx.end {
    True -> source_bytes.byte_at(ctx.bytes, pos)
    False -> -1
  }
}

fn hex_at(ctx: PatternContext, pos: Int) -> Option(Int) {
  digits.hex_value_code(byte_at(ctx, pos))
}

fn hex4_at(ctx: PatternContext, pos: Int) -> Option(Int) {
  case
    hex_at(ctx, pos),
    hex_at(ctx, pos + 1),
    hex_at(ctx, pos + 2),
    hex_at(ctx, pos + 3)
  {
    Some(a), Some(b), Some(c), Some(d) ->
      Some({ { { a * 16 + b } * 16 + c } * 16 } + d)
    _, _, _, _ -> None
  }
}

fn digit_at(ctx: PatternContext, pos: Int) -> Option(Int) {
  case byte_at(ctx, pos) {
    b if b >= 0x30 && b <= 0x39 -> Some(b - 0x30)
    _ -> None
  }
}

fn octal_at(ctx: PatternContext, pos: Int) -> Option(Int) {
  case byte_at(ctx, pos) {
    b if b >= 0x30 && b <= 0x37 -> Some(b - 0x30)
    _ -> None
  }
}

fn ascii_code(ch: String) -> Int {
  case string.to_utf_codepoints(ch) {
    [cp, ..] -> string.utf_codepoint_to_int(cp)
    [] -> 0
  }
}

fn is_ascii_letter(ch: String) -> Bool {
  digits.is_ascii_alpha_code(ascii_code(ch))
}

fn decimal_run(ctx: PatternContext, pos: Int) -> #(Int, Option(Int)) {
  case rest_of_pattern(ctx, pos) {
    <<b, rest:bytes>> if b >= 0x30 && b <= 0x39 ->
      decimal_run_loop(rest, pos + 1, b - 0x30)
    _ -> #(pos, None)
  }
}

fn decimal_run_loop(rest: BitArray, pos: Int, acc: Int) -> #(Int, Option(Int)) {
  case rest {
    <<b, rest:bytes>> if b >= 0x30 && b <= 0x39 ->
      decimal_run_loop(rest, pos + 1, acc * 10 + b - 0x30)
    _ -> #(pos, Some(acc))
  }
}

fn hex_run(ctx: PatternContext, pos: Int) -> #(Int, Option(Int)) {
  case digits.hex_value_code(byte_at(ctx, pos)) {
    Some(d) -> hex_run_loop(rest_of_pattern(ctx, pos + 1), pos + 1, d)
    None -> #(pos, None)
  }
}

// value saturates past 0x10ffff, callers only range check it
fn hex_run_loop(rest: BitArray, pos: Int, acc: Int) -> #(Int, Option(Int)) {
  case rest {
    <<b, rest:bytes>> ->
      case digits.hex_value_code(b) {
        Some(_) if acc > 0x10ffff -> hex_run_loop(rest, pos + 1, acc)
        Some(d) -> hex_run_loop(rest, pos + 1, acc * 16 + d)
        None -> #(pos, Some(acc))
      }
    _ -> #(pos, Some(acc))
  }
}

type TermKind {
  AtomTerm
  AssertionTerm
  LookaheadTerm
}

fn parse_disjunction(
  ctx: PatternContext,
  pos: Int,
) -> Result(#(Int, List(String)), PatternError) {
  use #(pos2, names) <- result.try(parse_alternative(ctx, pos, []))
  case ascii_at(ctx, pos2) {
    Some("|") -> {
      use #(pos3, names2) <- result.map(parse_disjunction(ctx, pos2 + 1))
      #(pos3, list.append(names, names2))
    }
    _ -> Ok(#(pos2, names))
  }
}

fn parse_alternative(
  ctx: PatternContext,
  pos: Int,
  acc: List(String),
) -> Result(#(Int, List(String)), PatternError) {
  use <- bool.guard(pos >= ctx.end, Ok(#(pos, acc)))
  case ascii_at(ctx, pos) {
    Some("|") | Some(")") -> Ok(#(pos, acc))
    _ -> {
      use #(pos2, term_names, kind) <- result.try(parse_term(ctx, pos))
      use Nil <- result.try(check_no_duplicate(term_names, acc, pos))
      use #(pos3, quantified) <- result.try(parse_quantifier(ctx, pos2))
      use Nil <- result.try(case quantified, kind, ctx.mode {
        False, _, _ -> Ok(Nil)
        True, AtomTerm, _ -> Ok(Nil)
        True, LookaheadTerm, Legacy -> Ok(Nil)
        True, _, _ -> Error(NothingToRepeat(pos2))
      })
      parse_alternative(ctx, pos3, list.append(term_names, acc))
    }
  }
}

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

fn parse_term(
  ctx: PatternContext,
  pos: Int,
) -> Result(#(Int, List(String), TermKind), PatternError) {
  let strict = ctx.mode != Legacy
  case ascii_at(ctx, pos) {
    Some("^") | Some("$") -> Ok(#(pos + 1, [], AssertionTerm))
    Some("\\") ->
      case ascii_at(ctx, pos + 1) {
        Some("b") | Some("B") -> Ok(#(pos + 2, [], AssertionTerm))
        _ -> {
          use pos2 <- result.map(parse_atom_escape(ctx, pos))
          #(pos2, [], AtomTerm)
        }
      }
    Some("(") -> parse_group(ctx, pos)
    Some("[") -> {
      use pos2 <- result.map(parse_class(ctx, pos + 1))
      #(pos2, [], AtomTerm)
    }
    Some("*") | Some("+") | Some("?") -> Error(NothingToRepeat(pos))
    Some("{") if strict -> Error(LoneQuantifierBrackets(pos))
    Some("{") ->
      case braced_quantifier(ctx, pos) {
        Some(_) -> Error(NothingToRepeat(pos))
        None -> Ok(#(pos + 1, [], AtomTerm))
      }
    Some("}") if strict -> Error(LoneQuantifierBrackets(pos))
    Some("]") if strict -> Error(LoneClassBracket(pos))
    _ -> Ok(#(pos + char_width_at(ctx, pos), [], AtomTerm))
  }
}

fn parse_group(
  ctx: PatternContext,
  pos: Int,
) -> Result(#(Int, List(String), TermKind), PatternError) {
  case ascii_at(ctx, pos + 1), ascii_at(ctx, pos + 2), ascii_at(ctx, pos + 3) {
    Some("?"), Some("="), _ | Some("?"), Some("!"), _ ->
      group_rest(ctx, pos + 3, LookaheadTerm)
    Some("?"), Some(":"), _ -> group_rest(ctx, pos + 3, AtomTerm)
    Some("?"), Some("<"), Some("=") | Some("?"), Some("<"), Some("!") ->
      group_rest(ctx, pos + 4, AssertionTerm)
    Some("?"), Some("<"), _ -> {
      use #(name, after_name) <- result.try(parse_group_name(ctx, pos + 3))
      use #(pos2, inner) <- result.try(parse_group_body(ctx, after_name))
      use Nil <- result.try(check_no_duplicate([name], inner, pos))
      Ok(#(pos2, [name, ..inner], AtomTerm))
    }
    Some("?"), _, _ -> {
      use after_colon <- result.try(parse_modifiers(ctx, pos + 2))
      group_rest(ctx, after_colon, AtomTerm)
    }
    _, _, _ -> group_rest(ctx, pos + 1, AtomTerm)
  }
}

fn group_rest(
  ctx: PatternContext,
  pos: Int,
  kind: TermKind,
) -> Result(#(Int, List(String), TermKind), PatternError) {
  use #(pos2, names) <- result.map(parse_group_body(ctx, pos))
  #(pos2, names, kind)
}

fn parse_group_body(
  ctx: PatternContext,
  pos: Int,
) -> Result(#(Int, List(String)), PatternError) {
  use #(pos2, names) <- result.try(parse_disjunction(ctx, pos))
  case ascii_at(ctx, pos2) {
    Some(")") -> Ok(#(pos2 + 1, names))
    _ -> Error(MissingClosingParen(pos2))
  }
}

// pos is never ":" here, parse_group handles (?: itself
fn parse_modifiers(ctx: PatternContext, pos: Int) -> Result(Int, PatternError) {
  use #(pos2, add) <- result.try(parse_modifier_flags(ctx, pos, []))
  case ascii_at(ctx, pos2) {
    Some(":") -> Ok(pos2 + 1)
    Some("-") -> {
      use #(pos3, remove) <- result.try(parse_modifier_flags(ctx, pos2 + 1, []))
      use Nil <- result.try(case ascii_at(ctx, pos3) {
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

fn parse_modifier_flags(
  ctx: PatternContext,
  pos: Int,
  seen: List(String),
) -> Result(#(Int, List(String)), PatternError) {
  case ascii_at(ctx, pos) {
    Some("i" as f) | Some("m" as f) | Some("s" as f) ->
      case list.contains(seen, f) {
        True -> Error(RepeatedModifierFlag(pos))
        False -> parse_modifier_flags(ctx, pos + 1, [f, ..seen])
      }
    _ -> Ok(#(pos, seen))
  }
}

fn parse_quantifier(
  ctx: PatternContext,
  pos: Int,
) -> Result(#(Int, Bool), PatternError) {
  case ascii_at(ctx, pos) {
    Some("*") | Some("+") | Some("?") -> Ok(#(skip_lazy(ctx, pos + 1), True))
    Some("{") ->
      case braced_quantifier(ctx, pos), ctx.mode {
        Some(BracedQuantifier(min:, max: Some(max), ..)), _ if max < min ->
          Error(OutOfOrderQuantifier(pos))
        Some(BracedQuantifier(after:, ..)), _ ->
          Ok(#(skip_lazy(ctx, after), True))
        None, Legacy -> Ok(#(pos, False))
        None, Unicode | None, UnicodeSets -> Error(LoneQuantifierBrackets(pos))
      }
    _ -> Ok(#(pos, False))
  }
}

fn skip_lazy(ctx: PatternContext, pos: Int) -> Int {
  case ascii_at(ctx, pos) {
    Some("?") -> pos + 1
    _ -> pos
  }
}

type BracedQuantifier {
  BracedQuantifier(after: Int, min: Int, max: Option(Int))
}

fn braced_quantifier(
  ctx: PatternContext,
  pos: Int,
) -> Option(BracedQuantifier) {
  case decimal_run(ctx, pos + 1) {
    #(_, None) -> None
    #(min_end, Some(min)) ->
      case ascii_at(ctx, min_end) {
        Some("}") -> Some(BracedQuantifier(after: min_end + 1, min:, max: None))
        Some(",") -> {
          let #(max_end, max) = decimal_run(ctx, min_end + 1)
          case ascii_at(ctx, max_end) {
            Some("}") -> Some(BracedQuantifier(after: max_end + 1, min:, max:))
            _ -> None
          }
        }
        _ -> None
      }
  }
}

fn parse_atom_escape(
  ctx: PatternContext,
  pos: Int,
) -> Result(Int, PatternError) {
  let strict = ctx.mode != Legacy
  case escaped_char_at(ctx, pos + 1) {
    Digit(0) ->
      case strict && option.is_some(digit_at(ctx, pos + 2)) {
        True -> Error(InvalidDecimalEscape(pos))
        False -> Ok(pos + 2)
      }
    // back reference, annex b lets it run past the group count
    Digit(_) -> {
      let #(after, n) = decimal_run(ctx, pos + 1)
      case strict, n {
        True, Some(n) if n > ctx.capture_count ->
          Error(BackReferenceOutOfRange(pos, n, ctx.capture_count))
        _, _ -> Ok(after)
      }
    }
    Ascii("k") ->
      case strict || ctx.has_named_groups, ascii_at(ctx, pos + 2) {
        False, _ -> Ok(pos + 2)
        True, Some("<") -> {
          use #(name, after) <- result.try(parse_group_name(ctx, pos + 3))
          case list.contains(ctx.group_names, name) {
            True -> Ok(after)
            False -> Error(InvalidNamedReference(pos))
          }
        }
        True, _ -> Error(InvalidNamedReference(pos))
      }
    _ -> {
      use atom <- result.map(parse_character_escape(ctx, pos, in_class: False))
      atom.after
    }
  }
}

type ClassAtom {
  ClassCharacter(after: Int, value: Int)
  CharacterClassEscape(after: Int)
}

// escapes read the same in and out of a class, as in quickjs get_class_atom
fn parse_character_escape(
  ctx: PatternContext,
  pos: Int,
  in_class in_class: Bool,
) -> Result(ClassAtom, PatternError) {
  let strict = ctx.mode != Legacy
  let invalid = case in_class {
    True -> InvalidClassEscape(pos)
    False -> InvalidEscape(pos)
  }
  let char = fn(after, value) { Ok(ClassCharacter(after:, value:)) }
  case escaped_char_at(ctx, pos + 1) {
    EndOfPattern if in_class -> Error(UnterminatedClass(pos))
    EndOfPattern -> Error(BackslashAtEnd(pos))
    NonAscii(..) if strict -> Error(invalid)
    NonAscii(value:, width:) -> char(pos + 1 + width, value)
    Ascii("d")
    | Ascii("D")
    | Ascii("s")
    | Ascii("S")
    | Ascii("w")
    | Ascii("W") -> Ok(CharacterClassEscape(after: pos + 2))
    Ascii("p" as ch) | Ascii("P" as ch) ->
      case ctx.mode {
        Legacy -> char(pos + 2, ascii_code(ch))
        Unicode | UnicodeSets -> {
          let allow_strings = ctx.mode == UnicodeSets
          use len <- result.map(property_escape_length(ctx, pos, allow_strings:))
          CharacterClassEscape(after: pos + len)
        }
      }
    Ascii("f") -> char(pos + 2, 0x0C)
    Ascii("n") -> char(pos + 2, 0x0A)
    Ascii("r") -> char(pos + 2, 0x0D)
    Ascii("t") -> char(pos + 2, 0x09)
    Ascii("v") -> char(pos + 2, 0x0B)
    // annex b lets \c take a digit or _ inside a class
    Ascii("c") -> {
      let annex_b_class = in_class && !strict
      case control_letter_at(ctx, pos + 2, annex_b_class), strict {
        Some(value), _ -> char(pos + 3, value)
        None, True -> Error(invalid)
        None, False -> char(pos + 1, 0x5C)
      }
    }
    Ascii("x") ->
      case hex_at(ctx, pos + 2), hex_at(ctx, pos + 3), strict {
        Some(h1), Some(h2), _ -> char(pos + 4, h1 * 16 + h2)
        _, _, True -> Error(InvalidHexEscape(pos))
        _, _, False -> char(pos + 2, 0x78)
      }
    Ascii("u") -> {
      use #(after, value) <- result.map(parse_unicode_escape(ctx, pos))
      ClassCharacter(after:, value:)
    }
    // u/v allow only \0, annex b reads legacy octal
    Digit(d) ->
      case strict, d, digit_at(ctx, pos + 2) {
        False, _, _ -> Ok(legacy_octal_escape(ctx, pos, d))
        True, 0, None -> char(pos + 2, 0x00)
        True, _, _ -> Error(invalid)
      }
    Ascii("k") ->
      case strict || ctx.has_named_groups {
        True -> Error(invalid)
        False -> char(pos + 2, 0x6B)
      }
    // identity escape, u/v only allow syntax chars and /
    Ascii(other) ->
      case strict && !is_syntax_char(other) && other != "/" {
        True -> Error(invalid)
        False -> char(pos + 2, ascii_code(other))
      }
  }
}

fn control_letter_at(
  ctx: PatternContext,
  pos: Int,
  annex_b_class: Bool,
) -> Option(Int) {
  use ch <- option.then(ascii_at(ctx, pos))
  let accepted =
    is_ascii_letter(ch)
    || { annex_b_class && { option.is_some(digit_at(ctx, pos)) || ch == "_" } }
  case accepted {
    True -> Some(ascii_code(ch) % 32)
    False -> None
  }
}

fn legacy_octal_escape(
  ctx: PatternContext,
  pos: Int,
  first_digit: Int,
) -> ClassAtom {
  case first_digit {
    8 | 9 -> ClassCharacter(after: pos + 2, value: 0x30 + first_digit)
    _ -> {
      let max_more = case first_digit <= 3 {
        True -> 2
        False -> 1
      }
      let #(after, value) = octal_run(ctx, pos + 2, max_more, first_digit)
      ClassCharacter(after:, value:)
    }
  }
}

fn octal_run(
  ctx: PatternContext,
  pos: Int,
  remaining: Int,
  acc: Int,
) -> #(Int, Int) {
  case remaining > 0, octal_at(ctx, pos) {
    True, Some(d) -> octal_run(ctx, pos + 1, remaining - 1, acc * 8 + d)
    _, _ -> #(pos, acc)
  }
}

// pattern \u, surrogate pairs only join under u or v
fn parse_unicode_escape(
  ctx: PatternContext,
  pos: Int,
) -> Result(#(Int, Int), PatternError) {
  case hex4_at(ctx, pos + 2), ctx.mode {
    Some(code), Legacy -> Ok(#(pos + 6, code))
    Some(code), Unicode | Some(code), UnicodeSets ->
      Ok(join_trailing_surrogate(#(pos + 6, code), hex4_unicode_escape(ctx, _)))
    None, Legacy -> Ok(#(pos + 2, 0x75))
    None, Unicode | None, UnicodeSets ->
      case braced_unicode_escape(ctx, pos) {
        Ok(escape) -> Ok(escape)
        Error(CodePointOutOfRange) -> Error(InvalidUnicodeEscapeValue(pos))
        Error(MalformedUnicodeEscape) -> Error(InvalidUnicodeEscape(pos))
      }
  }
}

type UnicodeEscapeError {
  MalformedUnicodeEscape
  CodePointOutOfRange
}

// \u{X..} with pos at the backslash
fn braced_unicode_escape(
  ctx: PatternContext,
  pos: Int,
) -> Result(#(Int, Int), UnicodeEscapeError) {
  use <- bool.guard(
    ascii_at(ctx, pos + 2) != Some("{"),
    Error(MalformedUnicodeEscape),
  )
  let #(digits_end, value) = hex_run(ctx, pos + 3)
  case value, ascii_at(ctx, digits_end) {
    Some(v), Some("}") if v > 0x10FFFF -> Error(CodePointOutOfRange)
    Some(v), Some("}") -> Ok(#(digits_end + 1, v))
    _, _ -> Error(MalformedUnicodeEscape)
  }
}

// \uXXXX with pos at the backslash
fn hex4_unicode_escape(ctx: PatternContext, pos: Int) -> Option(#(Int, Int)) {
  case ascii_at(ctx, pos), ascii_at(ctx, pos + 1), hex4_at(ctx, pos + 2) {
    Some("\\"), Some("u"), Some(code) -> Some(#(pos + 6, code))
    _, _, _ -> None
  }
}

fn join_trailing_surrogate(
  lead_escape: #(Int, Int),
  parse_trail: fn(Int) -> Option(#(Int, Int)),
) -> #(Int, Int) {
  let #(after_lead, lead) = lead_escape
  use <- bool.guard(!utf16.is_high(lead), lead_escape)
  case parse_trail(after_lead) {
    Some(#(after_trail, trail)) ->
      case utf16.is_low(trail) {
        True -> #(after_trail, utf16.combine(lead, trail))
        False -> lead_escape
      }
    None -> lead_escape
  }
}

fn parse_class(ctx: PatternContext, pos: Int) -> Result(Int, PatternError) {
  let body_start = case ascii_at(ctx, pos) {
    Some("^") -> pos + 1
    _ -> pos
  }
  case ctx.mode {
    UnicodeSets -> skip_class_set(ctx, body_start, prev_atom: False)
    Legacy | Unicode -> parse_class_ranges(ctx, body_start)
  }
}

fn parse_class_ranges(
  ctx: PatternContext,
  pos: Int,
) -> Result(Int, PatternError) {
  use <- bool.guard(pos >= ctx.end, Error(UnterminatedClass(pos)))
  use <- bool.guard(ascii_at(ctx, pos) == Some("]"), Ok(pos + 1))
  use low <- result.try(parse_class_atom(ctx, pos))
  let is_range =
    ascii_at(ctx, low.after) == Some("-")
    && low.after + 1 < ctx.end
    && ascii_at(ctx, low.after + 1) != Some("]")
  case is_range {
    True -> {
      use high <- result.try(parse_class_atom(ctx, low.after + 1))
      use Nil <- result.try(check_range(ctx, low, high, pos))
      parse_class_ranges(ctx, high.after)
    }
    False -> parse_class_ranges(ctx, low.after)
  }
}

fn check_range(
  ctx: PatternContext,
  low: ClassAtom,
  high: ClassAtom,
  pos: Int,
) -> Result(Nil, PatternError) {
  case low, high, ctx.mode {
    // without u a non-bmp endpoint is a surrogate pair, order unknowable
    ClassCharacter(value: low_value, ..),
      ClassCharacter(value: high_value, ..),
      Legacy
      if low_value > 0xFFFF || high_value > 0xFFFF
    -> Ok(Nil)
    ClassCharacter(value: low_value, ..),
      ClassCharacter(value: high_value, ..),
      _
      if low_value > high_value
    -> Error(OutOfOrderClassRange(pos))
    ClassCharacter(..), ClassCharacter(..), _ -> Ok(Nil)
    _, _, Legacy -> Ok(Nil)
    _, _, Unicode | _, _, UnicodeSets -> Error(InvalidClassRange(pos))
  }
}

fn parse_class_atom(
  ctx: PatternContext,
  pos: Int,
) -> Result(ClassAtom, PatternError) {
  case codepoint_at(ctx, pos) {
    None -> Error(UnterminatedClass(pos))
    Some(CodePoint(0x5C, _)) -> parse_class_escape(ctx, pos)
    Some(CodePoint(value:, width:)) ->
      Ok(ClassCharacter(after: pos + width, value:))
  }
}

fn parse_class_escape(
  ctx: PatternContext,
  pos: Int,
) -> Result(ClassAtom, PatternError) {
  case ascii_at(ctx, pos + 1) {
    Some("b") -> Ok(ClassCharacter(after: pos + 2, value: 0x08))
    Some("-") -> Ok(ClassCharacter(after: pos + 2, value: 0x2D))
    _ -> parse_character_escape(ctx, pos, in_class: True)
  }
}

// a v flag class is only skipped, matching validates it later
fn skip_class_set(
  ctx: PatternContext,
  pos: Int,
  prev_atom prev_atom: Bool,
) -> Result(Int, PatternError) {
  use <- bool.guard(pos >= ctx.end, Error(UnterminatedClass(pos)))
  let continue = fn(next_pos) { skip_class_set(ctx, next_pos, prev_atom: True) }
  let operator = fn(allowed) {
    case allowed {
      True -> skip_class_set(ctx, pos + 2, prev_atom: False)
      False -> Error(InvalidClassSetOperation(pos))
    }
  }
  let next = ascii_at(ctx, pos + 1)
  let after_next = ascii_at(ctx, pos + 2)
  case ascii_at(ctx, pos) {
    Some("]") -> Ok(pos + 1)
    Some("[") -> {
      let nested_start = case next {
        Some("^") -> pos + 2
        _ -> pos + 1
      }
      use after <- result.try(skip_class_set(
        ctx,
        nested_start,
        prev_atom: False,
      ))
      continue(after)
    }
    Some("\\") ->
      case next, after_next {
        Some("p"), Some("{")
        | Some("P"), Some("{")
        | Some("q"), Some("{")
        | Some("u"), Some("{")
        -> result.try(skip_to_close_brace(ctx, pos + 3), continue)
        Some("p"), _ | Some("P"), _ | Some("q"), _ | Some("u"), _ ->
          continue(pos + 2)
        _, _ -> continue(pos + 1 + char_width_at(ctx, pos + 1))
      }
    Some("&") if next == Some("&") ->
      operator(
        prev_atom
        && after_next != Some("]")
        && after_next != Some("&")
        && pos + 2 < ctx.end,
      )
    Some("-") if next == Some("-") ->
      operator(prev_atom && after_next != Some("]") && pos + 2 < ctx.end)
    Some("-") ->
      case prev_atom && next != Some("]") && pos + 1 < ctx.end {
        True -> skip_class_set(ctx, pos + 1, prev_atom: False)
        False -> Error(InvalidClassCharacter(pos))
      }
    Some(ch) ->
      case is_class_set_syntax_char(ch) {
        True -> Error(InvalidClassCharacter(pos))
        False ->
          case is_class_set_reserved_punctuator(ch) && next == Some(ch) {
            True -> Error(ReservedDoublePunctuator(pos))
            False -> continue(pos + 1)
          }
      }
    None -> continue(pos + char_width_at(ctx, pos))
  }
}

fn skip_to_close_brace(
  ctx: PatternContext,
  pos: Int,
) -> Result(Int, PatternError) {
  use <- bool.guard(pos >= ctx.end, Error(UnterminatedClass(pos)))
  case ascii_at(ctx, pos) {
    Some("}") -> Ok(pos + 1)
    _ -> skip_to_close_brace(ctx, pos + 1)
  }
}

// §22.2.1 SyntaxCharacter
fn is_syntax_char(ch: String) -> Bool {
  case ch {
    "^" | "$" | "\\" | "." | "*" | "+" | "?" -> True
    "(" | ")" | "[" | "]" | "{" | "}" | "|" -> True
    _ -> False
  }
}

// §22.2.1 ClassSetSyntaxCharacter
fn is_class_set_syntax_char(ch: String) -> Bool {
  case ch {
    "(" | ")" | "[" | "]" | "{" | "}" | "/" | "-" | "\\" | "|" -> True
    _ -> False
  }
}

// §22.2.1 ClassSetReservedDoublePunctuator, when doubled
fn is_class_set_reserved_punctuator(ch: String) -> Bool {
  case ch {
    "&" | "!" | "#" | "$" | "%" | "*" | "+" | "," | "." -> True
    ":" | ";" | "<" | "=" | ">" | "?" | "@" | "^" | "`" | "~" -> True
    _ -> False
  }
}

fn parse_group_name(
  ctx: PatternContext,
  pos: Int,
) -> Result(#(String, Int), PatternError) {
  group_name_loop(ctx, pos, True, [])
}

fn group_name_loop(
  ctx: PatternContext,
  pos: Int,
  is_first: Bool,
  acc: List(UtfCodepoint),
) -> Result(#(String, Int), PatternError) {
  case codepoint_at(ctx, pos), is_first {
    None, _ -> Error(UnterminatedGroupName(pos))
    Some(CodePoint(0x3E, _)), True -> Error(EmptyGroupName(pos))
    Some(CodePoint(0x3E, _)), False ->
      Ok(#(string.from_utf_codepoints(list.reverse(acc)), pos + 1))
    Some(CodePoint(0x5C, _)), _ -> {
      use lead <- result.try(group_name_escape(ctx, pos))
      let parse_trail = fn(at) {
        option.from_result(group_name_escape(ctx, at))
      }
      let #(next, code) = join_trailing_surrogate(lead, parse_trail)
      group_name_char(ctx, pos, code, next, is_first, acc)
    }
    Some(CodePoint(value:, width:)), _ ->
      group_name_char(ctx, pos, value, pos + width, is_first, acc)
  }
}

fn group_name_char(
  ctx: PatternContext,
  pos: Int,
  code: Int,
  next: Int,
  is_first: Bool,
  acc: List(UtfCodepoint),
) -> Result(#(String, Int), PatternError) {
  let invalid = InvalidGroupName(pos)
  use <- bool.guard(
    !lexer.validate_identifier_codepoint(code, is_first),
    Error(invalid),
  )
  use encoded <- result.try(
    string.utf_codepoint(code) |> result.replace_error(invalid),
  )
  group_name_loop(ctx, next, False, [encoded, ..acc])
}

// group names always take the u flag escape forms
fn group_name_escape(
  ctx: PatternContext,
  pos: Int,
) -> Result(#(Int, Int), PatternError) {
  let invalid = InvalidGroupName(pos)
  use <- bool.guard(ascii_at(ctx, pos + 1) != Some("u"), Error(invalid))
  case hex4_at(ctx, pos + 2) {
    Some(code) -> Ok(#(pos + 6, code))
    None -> braced_unicode_escape(ctx, pos) |> result.replace_error(invalid)
  }
}

type PropertyEscapeKind {
  PropValid
  PropString
  PropInvalid
}

@external(erlang, "arc_regex_props_ffi", "classify_lone")
fn classify_lone(name: String) -> PropertyEscapeKind

@external(erlang, "arc_regex_props_ffi", "classify_pair")
fn classify_pair(name: String, value: String) -> PropertyEscapeKind

fn skip_property_chars(ctx: PatternContext, pos: Int) -> Int {
  let continues = case ascii_at(ctx, pos) {
    Some("_") -> True
    Some(ch) -> digits.is_ascii_alnum_code(ascii_code(ch))
    None -> False
  }
  case continues {
    True -> skip_property_chars(ctx, pos + 1)
    False -> pos
  }
}

fn property_escape_length(
  ctx: PatternContext,
  pos: Int,
  allow_strings allow_strings: Bool,
) -> Result(Int, PatternError) {
  let invalid = Error(InvalidPropertyName(pos))
  use <- bool.guard(ascii_at(ctx, pos + 2) != Some("{"), invalid)
  let name_start = pos + 3
  let name_end = skip_property_chars(ctx, name_start)
  let name =
    source_bytes.unsafe_slice(ctx.bytes, name_start, name_end - name_start)
  case ascii_at(ctx, name_end) {
    Some("}") ->
      case classify_lone(name), allow_strings {
        PropValid, _ | PropString, True -> Ok(name_end + 1 - pos)
        PropString, False -> Error(PropertyOfStringsRequiresVFlag(pos))
        PropInvalid, _ -> invalid
      }
    Some("=") -> {
      let value_start = name_end + 1
      let value_end = skip_property_chars(ctx, value_start)
      let value =
        source_bytes.unsafe_slice(
          ctx.bytes,
          value_start,
          value_end - value_start,
        )
      use <- bool.guard(ascii_at(ctx, value_end) != Some("}"), invalid)
      case classify_pair(name, value) {
        PropValid -> Ok(value_end + 1 - pos)
        PropString | PropInvalid -> invalid
      }
    }
    _ -> invalid
  }
}

// \p of strings is only allowed outside negated classes
fn check_v_mode_property_escapes(
  ctx: PatternContext,
  pos: Int,
  class_negations: List(Bool),
) -> Result(Nil, PatternError) {
  use <- bool.guard(pos >= ctx.end, Ok(Nil))
  let continue = fn(next_pos, class_negations) {
    check_v_mode_property_escapes(ctx, next_pos, class_negations)
  }
  case ascii_at(ctx, pos), ascii_at(ctx, pos + 1) {
    Some("\\"), Some("p" as ch) | Some("\\"), Some("P" as ch) -> {
      let in_negated_class = list.contains(class_negations, True)
      let allow_strings = ch == "p" && !in_negated_class
      use len <- result.try(property_escape_length(ctx, pos, allow_strings:))
      continue(pos + len, class_negations)
    }
    Some("\\"), _ -> continue(pos + 2, class_negations)
    Some("["), Some("^") -> continue(pos + 2, [True, ..class_negations])
    Some("["), _ -> continue(pos + 1, [False, ..class_negations])
    Some("]"), _ -> continue(pos + 1, list.drop(class_negations, 1))
    _, _ -> continue(pos + 1, class_negations)
  }
}
