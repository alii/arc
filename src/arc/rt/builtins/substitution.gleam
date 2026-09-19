import arc/rt/utf8
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// §22.1.3.19.1 getsubstitution, tokenized once per replace call
pub type PlainSegment {
  LiteralSegment(text: String)
  MatchedSegment
  BeforeSegment
  AfterSegment
  CaptureSegment(idx: Int)
  TwoDigitSegment(two_idx: Int, one_idx: Int, suffix: String)
  ZeroDigitSegment(two_idx: Int, literal: String)
}

pub type NamedSegment {
  Plain(segment: PlainSegment)
  NamedGroupSegment(name: String)
}

pub type MatchContext {
  MatchContext(
    matched: String,
    before: fn() -> String,
    after: fn() -> String,
    // 1-based, only called with 1 <= n <= capture_count
    capture: fn(Int) -> String,
    capture_count: Int,
  )
}

pub type Expanded {
  Text(text: String)
  NamedGroup(name: String)
}

pub fn expand_plain(segment: PlainSegment, ctx: MatchContext) -> String {
  case segment {
    LiteralSegment(text) -> text
    MatchedSegment -> ctx.matched
    BeforeSegment -> ctx.before()
    AfterSegment -> ctx.after()
    CaptureSegment(idx) ->
      case idx <= ctx.capture_count {
        True -> ctx.capture(idx)
        False -> "$" <> int.to_string(idx)
      }
    TwoDigitSegment(two_idx, one_idx, suffix) ->
      case two_idx <= ctx.capture_count, one_idx <= ctx.capture_count {
        True, _ -> ctx.capture(two_idx)
        False, True -> ctx.capture(one_idx) <> suffix
        False, False -> "$" <> int.to_string(one_idx) <> suffix
      }
    ZeroDigitSegment(two_idx, literal) ->
      case two_idx <= ctx.capture_count && two_idx >= 1 {
        True -> ctx.capture(two_idx)
        False -> literal
      }
  }
}

pub fn expand(segment: NamedSegment, ctx: MatchContext) -> Expanded {
  case segment {
    Plain(p) -> Text(expand_plain(p, ctx))
    NamedGroupSegment(name) -> NamedGroup(name)
  }
}

pub fn expand_without_named(
  segments: List(PlainSegment),
  ctx: MatchContext,
) -> String {
  segments
  |> expand_plain_parts(ctx)
  |> string.concat
}

pub fn expand_plain_parts(
  segments: List(PlainSegment),
  ctx: MatchContext,
) -> List(String) {
  list.map(segments, expand_plain(_, ctx))
}

type Emit(segment) {
  Emit(plain: fn(PlainSegment) -> segment, named: Option(fn(String) -> segment))
}

pub fn tokenize_plain(template: String) -> List(PlainSegment) {
  tokenize(template, Emit(plain: fn(p) { p }, named: None))
}

pub fn tokenize_named(template: String) -> List(NamedSegment) {
  tokenize(template, Emit(plain: Plain, named: Some(NamedGroupSegment)))
}

fn tokenize(template: String, emit: Emit(segment)) -> List(segment) {
  case utf8.has_byte(template, 0x24) {
    False -> [emit.plain(LiteralSegment(template))]
    True -> tokenize_loop(to_code_points(template), emit, "", [])
  }
}

// code points not graphemes, so "$&" + combining mark still splits
fn to_code_points(s: String) -> List(String) {
  s
  |> string.to_utf_codepoints
  |> list.map(fn(cp) { string.from_utf_codepoints([cp]) })
}

fn flush_literal(
  lit: String,
  emit: Emit(segment),
  segs: List(segment),
) -> List(segment) {
  case lit {
    "" -> segs
    _ -> [emit.plain(LiteralSegment(lit)), ..segs]
  }
}

fn tokenize_loop(
  chars: List(String),
  emit: Emit(segment),
  lit: String,
  segs: List(segment),
) -> List(segment) {
  case chars {
    [] -> list.reverse(flush_literal(lit, emit, segs))
    ["$", "$", ..rest] -> tokenize_loop(rest, emit, lit <> "$", segs)
    ["$", "&", ..rest] ->
      tokenize_loop(rest, emit, "", [
        emit.plain(MatchedSegment),
        ..flush_literal(lit, emit, segs)
      ])
    ["$", "`", ..rest] ->
      tokenize_loop(rest, emit, "", [
        emit.plain(BeforeSegment),
        ..flush_literal(lit, emit, segs)
      ])
    ["$", "'", ..rest] ->
      tokenize_loop(rest, emit, "", [
        emit.plain(AfterSegment),
        ..flush_literal(lit, emit, segs)
      ])
    ["$", "<", ..rest] ->
      case emit.named {
        Some(mk_named) ->
          case take_group_name(rest, "") {
            Some(#(name, rest2)) ->
              tokenize_loop(rest2, emit, "", [
                mk_named(name),
                ..flush_literal(lit, emit, segs)
              ])
            None -> tokenize_loop(rest, emit, lit <> "$<", segs)
          }
        None -> tokenize_loop(rest, emit, lit <> "$<", segs)
      }
    ["$", d1, d2, ..rest] ->
      case is_digit(d1), is_digit(d2) {
        True, True -> tokenize_two_digit(d1, d2, rest, emit, lit, segs)
        True, False -> tokenize_one_digit(d1, [d2, ..rest], emit, lit, segs)
        False, _ -> tokenize_loop([d1, d2, ..rest], emit, lit <> "$", segs)
      }
    ["$", d1] ->
      case is_digit(d1) {
        True -> tokenize_one_digit(d1, [], emit, lit, segs)
        False -> tokenize_loop([d1], emit, lit <> "$", segs)
      }
    [ch, ..rest] -> tokenize_loop(rest, emit, lit <> ch, segs)
  }
}

fn take_group_name(
  chars: List(String),
  acc: String,
) -> Option(#(String, List(String))) {
  case chars {
    [] -> None
    [">", ..rest] -> Some(#(acc, rest))
    [ch, ..rest] -> take_group_name(rest, acc <> ch)
  }
}

fn tokenize_one_digit(
  d1: String,
  rest: List(String),
  emit: Emit(segment),
  lit: String,
  segs: List(segment),
) -> List(segment) {
  case digit_value(d1) {
    0 -> tokenize_loop(rest, emit, lit <> "$0", segs)
    idx ->
      tokenize_loop(rest, emit, "", [
        emit.plain(CaptureSegment(idx)),
        ..flush_literal(lit, emit, segs)
      ])
  }
}

fn tokenize_two_digit(
  d1: String,
  d2: String,
  rest: List(String),
  emit: Emit(segment),
  lit: String,
  segs: List(segment),
) -> List(segment) {
  let two_idx = digit_value(d1) * 10 + digit_value(d2)
  case digit_value(d1), two_idx {
    0, 0 -> tokenize_loop(rest, emit, lit <> "$00", segs)
    0, _ ->
      tokenize_loop(rest, emit, "", [
        emit.plain(ZeroDigitSegment(two_idx, "$0" <> d2)),
        ..flush_literal(lit, emit, segs)
      ])
    one_idx, _ ->
      tokenize_loop(rest, emit, "", [
        emit.plain(TwoDigitSegment(two_idx, one_idx, d2)),
        ..flush_literal(lit, emit, segs)
      ])
  }
}

fn digit_value(ch: String) -> Int {
  case ch {
    "1" -> 1
    "2" -> 2
    "3" -> 3
    "4" -> 4
    "5" -> 5
    "6" -> 6
    "7" -> 7
    "8" -> 8
    "9" -> 9
    _ -> 0
  }
}

fn is_digit(ch: String) -> Bool {
  case ch {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}
