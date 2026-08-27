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
import arc/parser/source_bytes.{ascii_at}
import gleam/bit_array
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

type CodePoint {
  Cp(value: Int, width: Int)
  Eof
}

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

fn advance_width(bytes: BitArray, pos: Int) -> Int {
  case codepoint_at(bytes, pos) {
    Cp(_, w) -> w
    Eof -> 1
  }
}

type At {
  Ascii(ch: String)
  NonAscii(width: Int)
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

fn source_slice(bytes: BitArray, start: Int, len: Int) -> String {
  source_bytes.slice(bytes, start, len)
  |> option.unwrap("")
}

fn ascii_in(bytes: BitArray, pos: Int, end: Int) -> Option(String) {
  case pos < end {
    True -> ascii_at(bytes, pos)
    False -> None
  }
}

fn hex_in(bytes: BitArray, pos: Int, end: Int) -> Option(Int) {
  case ascii_in(bytes, pos, end) {
    Some(ch) -> digits.hex_value(ch)
    None -> None
  }
}

fn digit_in(bytes: BitArray, pos: Int, end: Int) -> Option(Int) {
  case ascii_in(bytes, pos, end) {
    Some(ch) -> digits.digit_value(ch)
    None -> None
  }
}

fn decimal_run(bytes: BitArray, pos: Int, end: Int) -> #(Int, Option(Int)) {
  digit_run_loop(bytes, pos, end, 10, digit_in, None)
}

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

pub fn scan_regex_source(
  bytes: BitArray,
  pos: Int,
) -> Result(Int, PatternError) {
  scan_regex_loop(bytes, pos, False)
}

fn scan_regex_loop(
  bytes: BitArray,
  pos: Int,
  in_class: Bool,
) -> Result(Int, PatternError) {
  case bit_array.slice(bytes, pos, 1) {
    Error(_) -> Error(UnterminatedRegex(pos))
    Ok(<<b>>) if b >= 0x80 ->
      case is_unicode_line_terminator(bytes, pos) {
        True -> Error(UnterminatedRegex(pos))
        False -> scan_regex_loop(bytes, pos + utf8_byte_width(b), in_class)
      }
    Ok(_) -> {
      case ascii_at(bytes, pos) {
        Some("\n") | Some("\r") -> Error(UnterminatedRegex(pos))
        Some("\\") -> {
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

fn utf8_byte_width(lead: Int) -> Int {
  case lead {
    b if b >= 0xF0 -> 4
    b if b >= 0xE0 -> 3
    b if b >= 0xC0 -> 2
    _ -> 1
  }
}

fn is_unicode_line_terminator(bytes: BitArray, pos: Int) -> Bool {
  case bit_array.slice(bytes, pos, 3) {
    Ok(<<0xE2, 0x80, 0xA8>>) | Ok(<<0xE2, 0x80, 0xA9>>) -> True
    _ -> False
  }
}

pub type RegexMode {
  Legacy
  Unicode
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

fn ctx_ascii(ctx: Ctx, pos: Int) -> Option(String) {
  ascii_in(ctx.bytes, pos, ctx.end)
}

fn ctx_hex(ctx: Ctx, pos: Int) -> Option(Int) {
  hex_in(ctx.bytes, pos, ctx.end)
}

fn ctx_digit(ctx: Ctx, pos: Int) -> Option(Int) {
  digit_in(ctx.bytes, pos, ctx.end)
}

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

fn ctx_letter(ctx: Ctx, pos: Int) -> Bool {
  case ctx_ascii(ctx, pos) {
    Some(ch) -> is_ascii_letter(ch)
    None -> False
  }
}

type TermKind {
  KAtom
  KAssertion
  KLookahead
}

// §22.2.1.1 pattern early errors, annex b grammar without u/v
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
  case mode {
    UnicodeSets -> validate_regex_vmode_body(bytes, start, end)
    Legacy | Unicode -> Ok(Nil)
  }
}

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

type BracedQuantifier {
  BracedQuantifier(after: Int, min: Int, max: Option(Int))
}

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

fn p_atom_escape(ctx: Ctx, pos: Int) -> Result(Int, PatternError) {
  let strict = ctx.mode != Legacy
  let invalid = InvalidEscape(pos)
  case at(ctx.bytes, pos + 1, ctx.end) {
    AtEnd -> Error(BackslashAtEnd(pos))
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
        True ->
          case is_syntax_char(other) || other == "/" {
            True -> Ok(pos + 2)
            False -> Error(invalid)
          }
        False -> Ok(pos + 2)
      }
  }
}

fn p_unicode_escape(ctx: Ctx, pos: Int) -> Result(#(Int, Int), PatternError) {
  case ctx_hex4(ctx, pos + 2) {
    Some(lead) -> {
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
        Legacy -> Ok(#(pos + 2, 0x75))
        Unicode | UnicodeSets ->
          case ctx_ascii(ctx, pos + 2) {
            Some("{") -> {
              let #(after, value) = hex_run(ctx.bytes, pos + 3, ctx.end)
              case value, ctx_ascii(ctx, after) {
                Some(v), Some("}") if v > 0x10FFFF ->
                  Error(InvalidUnicodeEscapeValue(pos))
                Some(v), Some("}") -> Ok(#(after + 1, v))
                _, _ -> Error(InvalidUnicodeEscape(pos))
              }
            }
            _ -> Error(InvalidUnicodeEscape(pos))
          }
      }
  }
}

type ClassMode {
  LegacyClass
  UnicodeClass
}

fn p_class(ctx: Ctx, pos: Int) -> Result(Int, PatternError) {
  case ctx.mode {
    UnicodeSets -> skip_v_class(ctx.bytes, pos, ctx.end)
    Legacy -> p_class_ranges(ctx, LegacyClass, skip_class_negation(ctx, pos))
    Unicode -> p_class_ranges(ctx, UnicodeClass, skip_class_negation(ctx, pos))
  }
}

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

fn check_range(
  mode: ClassMode,
  a: ClassAtom,
  b: ClassAtom,
  pos: Int,
) -> Result(Nil, PatternError) {
  case a, b {
    ClassCharacter(value: av, ..), ClassCharacter(value: bv, ..) -> {
      // legacy non-bmp endpoint is a surrogate pair, order unknowable
      let unknowable = mode == LegacyClass && { av > 0xFFFF || bv > 0xFFFF }
      case !unknowable && av > bv {
        True -> Error(OutOfOrderClassRange(pos))
        False -> Ok(Nil)
      }
    }
    _, _ ->
      case mode {
        UnicodeClass -> Error(InvalidClassRange(pos))
        LegacyClass -> Ok(Nil)
      }
  }
}

type ClassAtom {
  ClassCharacter(after: Int, value: Int)
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

fn ascii_code(ch: String) -> Int {
  case string.to_utf_codepoints(ch) {
    [cp, ..] -> string.utf_codepoint_to_int(cp)
    [] -> 0
  }
}

fn p_class_escape(
  ctx: Ctx,
  mode: ClassMode,
  pos: Int,
) -> Result(ClassAtom, PatternError) {
  let strict = mode == UnicodeClass
  let invalid = InvalidClassEscape(pos)
  case at(ctx.bytes, pos + 1, ctx.end) {
    AtEnd -> Error(UnterminatedClass(pos))
    NonAscii(w) ->
      case strict, codepoint_at(ctx.bytes, pos + 1) {
        True, _ -> Error(invalid)
        False, Cp(cp, _) -> Ok(ClassCharacter(after: pos + 1 + w, value: cp))
        False, Eof -> Error(UnterminatedClass(pos))
      }
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
        False -> Ok(legacy_numeric_escape(ctx.bytes, pos, ctx.end))
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

fn legacy_numeric_escape(bytes: BitArray, pos: Int, end: Int) -> ClassAtom {
  case octal_at(bytes, pos + 1, end) {
    None -> {
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

fn skip_v_class(
  bytes: BitArray,
  pos: Int,
  end: Int,
) -> Result(Int, PatternError) {
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

fn parse_group_name(
  bytes: BitArray,
  pos: Int,
  end: Int,
) -> Result(#(String, Int), PatternError) {
  group_name_loop(bytes, pos, end, True, [])
}

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
            Some(cp), Some("}") if cp <= 0x10FFFF -> Ok(#(cp, after + 1))
            _, _ -> Error(InvalidGroupName(pos))
          }
        }
        _ -> {
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

// pos is never ":" here, p_group handles (?: itself
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

fn is_ascii_letter(ch: String) -> Bool {
  case string.to_utf_codepoints(ch) {
    [cp] -> digits.is_ascii_alpha_code(string.utf_codepoint_to_int(cp))
    _ -> False
  }
}

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

pub type RegexFlags {
  RegexFlags(mode: RegexMode, flags: List(String))
}

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

pub fn validate_flags(flags: String) -> Result(RegexFlags, PatternError) {
  let bytes = <<flags:utf8>>
  use #(end, seen) <- result.try(scan_regex_flags(bytes, 0, []))
  use Nil <- result.try(case end >= bit_array.byte_size(bytes) {
    True -> Ok(Nil)
    False -> Error(InvalidFlag(end, grapheme_at(bytes, end)))
  })
  regex_flags(seen, 0)
}

fn grapheme_at(bytes: BitArray, pos: Int) -> String {
  bit_array.slice(bytes, pos, bit_array.byte_size(bytes) - pos)
  |> result.try(bit_array.to_string)
  |> result.try(string.first)
  |> result.unwrap("")
}

pub fn skip_regex_flags(
  bytes: BitArray,
  pos: Int,
) -> Result(#(Int, RegexFlags), PatternError) {
  use #(end, seen) <- result.try(scan_regex_flags(bytes, pos, []))
  use flags <- result.map(regex_flags(seen, pos))
  #(end, flags)
}

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

type PropertyEscapeKind {
  PropValid
  PropString
  PropInvalid
}

@external(erlang, "arc_regex_props_ffi", "classify_lone")
fn classify_lone_property(name: String) -> PropertyEscapeKind

@external(erlang, "arc_regex_props_ffi", "classify_pair")
fn classify_pair_property(name: String, value: String) -> PropertyEscapeKind

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
    _ -> Error(invalid)
  }
}

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
