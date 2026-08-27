import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// §22.1.3.19.1 getsubstitution, tokenized once per replace call
pub type PlainSegment {
  LiteralSeg(text: String)
  MatchedSeg
  BeforeSeg
  AfterSeg
  CaptureSeg(idx: Int)
  TwoDigitSeg(two_idx: Int, one_idx: Int, suffix: String)
  ZeroDigitSeg(two_idx: Int, literal: String)
}

pub type NamedSegment {
  Plain(seg: PlainSegment)
  NamedSeg(name: String)
}

pub type Ctx {
  Ctx(
    matched: String,
    before: fn() -> String,
    after: fn() -> String,
    // 1-based, only called with 1 <= n <= m
    capture: fn(Int) -> String,
    m: Int,
  )
}

pub type Resolved {
  Text(text: String)
  NamedRef(name: String)
}

pub fn resolve_plain(seg: PlainSegment, ctx: Ctx) -> String {
  case seg {
    LiteralSeg(text) -> text
    MatchedSeg -> ctx.matched
    BeforeSeg -> ctx.before()
    AfterSeg -> ctx.after()
    CaptureSeg(idx) ->
      case idx <= ctx.m {
        True -> ctx.capture(idx)
        False -> "$" <> int.to_string(idx)
      }
    TwoDigitSeg(two_idx, one_idx, suffix) ->
      case two_idx <= ctx.m, one_idx <= ctx.m {
        True, _ -> ctx.capture(two_idx)
        False, True -> ctx.capture(one_idx) <> suffix
        False, False -> "$" <> int.to_string(one_idx) <> suffix
      }
    ZeroDigitSeg(two_idx, literal) ->
      case two_idx <= ctx.m && two_idx >= 1 {
        True -> ctx.capture(two_idx)
        False -> literal
      }
  }
}

pub fn resolve(seg: NamedSegment, ctx: Ctx) -> Resolved {
  case seg {
    Plain(p) -> Text(resolve_plain(p, ctx))
    NamedSeg(name) -> NamedRef(name)
  }
}

pub fn resolve_without_named(segments: List(PlainSegment), ctx: Ctx) -> String {
  segments
  |> resolve_plain_parts(ctx)
  |> string.concat
}

pub fn resolve_plain_parts(
  segments: List(PlainSegment),
  ctx: Ctx,
) -> List(String) {
  list.map(segments, resolve_plain(_, ctx))
}

type Emit(seg) {
  Emit(plain: fn(PlainSegment) -> seg, named: Option(fn(String) -> seg))
}

pub fn tokenize_plain(template: String) -> List(PlainSegment) {
  tokenize(template, Emit(plain: fn(p) { p }, named: None))
}

pub fn tokenize_named(template: String) -> List(NamedSegment) {
  tokenize(template, Emit(plain: Plain, named: Some(NamedSeg)))
}

fn tokenize(template: String, emit: Emit(seg)) -> List(seg) {
  case string.contains(template, "$") {
    False -> [emit.plain(LiteralSeg(template))]
    True -> tokenize_loop(to_code_points(template), emit, "", [])
  }
}

// code points not graphemes, so "$&" + combining mark still splits
fn to_code_points(s: String) -> List(String) {
  s
  |> string.to_utf_codepoints
  |> list.map(fn(cp) { string.from_utf_codepoints([cp]) })
}

fn flush_literal(lit: String, emit: Emit(seg), segs: List(seg)) -> List(seg) {
  case lit {
    "" -> segs
    _ -> [emit.plain(LiteralSeg(lit)), ..segs]
  }
}

fn tokenize_loop(
  chars: List(String),
  emit: Emit(seg),
  lit: String,
  segs: List(seg),
) -> List(seg) {
  case chars {
    [] -> list.reverse(flush_literal(lit, emit, segs))
    ["$", "$", ..rest] -> tokenize_loop(rest, emit, lit <> "$", segs)
    ["$", "&", ..rest] ->
      tokenize_loop(rest, emit, "", [
        emit.plain(MatchedSeg),
        ..flush_literal(lit, emit, segs)
      ])
    ["$", "`", ..rest] ->
      tokenize_loop(rest, emit, "", [
        emit.plain(BeforeSeg),
        ..flush_literal(lit, emit, segs)
      ])
    ["$", "'", ..rest] ->
      tokenize_loop(rest, emit, "", [
        emit.plain(AfterSeg),
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
  emit: Emit(seg),
  lit: String,
  segs: List(seg),
) -> List(seg) {
  case digit_value(d1) {
    0 -> tokenize_loop(rest, emit, lit <> "$0", segs)
    idx ->
      tokenize_loop(rest, emit, "", [
        emit.plain(CaptureSeg(idx)),
        ..flush_literal(lit, emit, segs)
      ])
  }
}

fn tokenize_two_digit(
  d1: String,
  d2: String,
  rest: List(String),
  emit: Emit(seg),
  lit: String,
  segs: List(seg),
) -> List(seg) {
  let two_idx = digit_value(d1) * 10 + digit_value(d2)
  case digit_value(d1), two_idx {
    0, 0 -> tokenize_loop(rest, emit, lit <> "$00", segs)
    0, _ ->
      tokenize_loop(rest, emit, "", [
        emit.plain(ZeroDigitSeg(two_idx, "$0" <> d2)),
        ..flush_literal(lit, emit, segs)
      ])
    one_idx, _ ->
      tokenize_loop(rest, emit, "", [
        emit.plain(TwoDigitSeg(two_idx, one_idx, d2)),
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
