//// ES2024 §22.1.3.19.1 GetSubstitution — the replacement-template language
//// (`$$`, `$&`, `` $` ``, `$'`, `$1`..`$99`, `$<name>`) shared by
//// `String.prototype.replace`/`replaceAll` and `RegExp.prototype[@@replace]`.
////
//// The template is tokenized ONCE per replace call (`tokenize_template`) and
//// each match then only resolves the segments (`resolve`). Resolution is
//// pure: the single observable step in the whole operation is the
//// `Get(namedCaptures, name)` behind `$<name>`, so `resolve` hands that back
//// as `NamedRef` for the caller to perform. `String.prototype` never sees a
//// `NamedRef` — a string search has no captures, so it tokenizes with
//// `named_mode: False` and can use `resolve_without_named`.

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// One pre-tokenized piece of a replacement template. The common no-dollar
/// template is a single `LiteralSeg`.
pub type ReplaceSegment {
  /// Literal text — also covers "$$" → "$".
  LiteralSeg(text: String)
  /// "$&" — the matched substring.
  MatchedSeg
  /// "$`" — the portion of the string before the match.
  BeforeSeg
  /// "$'" — the portion of the string after the match.
  AfterSeg
  /// "$N" (N in 1-9) — capture group N if N ≤ m, else the literal "$N".
  CaptureSeg(idx: Int)
  /// "$NN" (first digit 1-9): group NN if NN ≤ m; else group N (first digit)
  /// followed by the literal second digit if N ≤ m; else literal.
  TwoDigitSeg(two_idx: Int, one_idx: Int, suffix: String)
  /// "$0d" (d in 1-9): group d if d ≤ m, else the literal "$0d".
  ZeroDigitSeg(two_idx: Int, literal: String)
  /// "$<name>" — named capture: Get(namedCaptures, name) then ToString
  /// (undefined → ""). When namedCaptures is undefined the whole "$<name>"
  /// is literal text.
  NamedSeg(name: String)
}

/// Everything a segment can refer to besides `namedCaptures`. `before`/`after`
/// are thunks because the overwhelmingly common template mentions neither, and
/// materialising them costs a slice of the whole subject per match.
pub type Ctx {
  Ctx(
    /// The matched substring ("$&").
    matched: String,
    /// The subject before the match ("$`").
    before: fn() -> String,
    /// The subject after the match ("$'").
    after: fn() -> String,
    /// 1-based capture group `n` as a string; "" for an undefined capture.
    /// Only ever called with `1 <= n <= m`.
    capture: fn(Int) -> String,
    /// The number of capture groups (spec's `m`).
    m: Int,
  )
}

/// The outcome of resolving one segment: either final text, or the one case
/// that needs an observable `Get` the caller must perform.
pub type Resolved {
  Text(text: String)
  NamedRef(name: String)
}

/// GetSubstitution for a single segment. Total and pure.
pub fn resolve(seg: ReplaceSegment, ctx: Ctx) -> Resolved {
  case seg {
    LiteralSeg(text) -> Text(text)
    MatchedSeg -> Text(ctx.matched)
    BeforeSeg -> Text(ctx.before())
    AfterSeg -> Text(ctx.after())
    CaptureSeg(idx) ->
      case idx <= ctx.m {
        True -> Text(ctx.capture(idx))
        False -> Text("$" <> int.to_string(idx))
      }
    TwoDigitSeg(two_idx, one_idx, suffix) ->
      case two_idx <= ctx.m, one_idx <= ctx.m {
        True, _ -> Text(ctx.capture(two_idx))
        False, True -> Text(ctx.capture(one_idx) <> suffix)
        False, False -> Text("$" <> int.to_string(one_idx) <> suffix)
      }
    ZeroDigitSeg(two_idx, literal) ->
      case two_idx <= ctx.m && two_idx >= 1 {
        True -> Text(ctx.capture(two_idx))
        False -> Text(literal)
      }
    NamedSeg(name) -> NamedRef(name)
  }
}

/// GetSubstitution when there is no `namedCaptures` object, i.e. every caller
/// that tokenized with `named_mode: False`. `NamedRef` is unreachable there —
/// `tokenize_template(_, False)` never emits `NamedSeg` — but the spec's own
/// answer for a `$<name>` with undefined namedCaptures is "keep it literal",
/// so that is what the branch does.
pub fn resolve_without_named(
  segments: List(ReplaceSegment),
  ctx: Ctx,
) -> String {
  segments
  |> list.map(fn(seg) {
    case resolve(seg, ctx) {
      Text(t) -> t
      NamedRef(name) -> "$<" <> name <> ">"
    }
  })
  |> string.concat
}

/// Tokenize a replacement template into segments. Called once per replace
/// call; templates without "$" skip grapheme segmentation entirely.
///
/// `named_mode` selects the "$<" interpretation: with a `groups` object on the
/// match result "$<" opens a named reference scanned to ">"; without one it is
/// the 2-char literal "$<" and scanning resumes immediately after it. Callers
/// that can see either shape tokenize twice and pick per match result.
pub fn tokenize_template(
  template: String,
  named_mode: Bool,
) -> List(ReplaceSegment) {
  case string.contains(template, "$") {
    False -> [LiteralSeg(template)]
    True -> tokenize_loop(string.to_graphemes(template), named_mode, "", [])
  }
}

fn flush_literal(
  lit: String,
  segs: List(ReplaceSegment),
) -> List(ReplaceSegment) {
  case lit {
    "" -> segs
    _ -> [LiteralSeg(lit), ..segs]
  }
}

fn tokenize_loop(
  chars: List(String),
  named_mode: Bool,
  lit: String,
  segs: List(ReplaceSegment),
) -> List(ReplaceSegment) {
  case chars {
    [] -> list.reverse(flush_literal(lit, segs))
    ["$", "$", ..rest] -> tokenize_loop(rest, named_mode, lit <> "$", segs)
    ["$", "&", ..rest] ->
      tokenize_loop(rest, named_mode, "", [
        MatchedSeg,
        ..flush_literal(lit, segs)
      ])
    ["$", "`", ..rest] ->
      tokenize_loop(rest, named_mode, "", [
        BeforeSeg,
        ..flush_literal(lit, segs)
      ])
    ["$", "'", ..rest] ->
      tokenize_loop(rest, named_mode, "", [AfterSeg, ..flush_literal(lit, segs)])
    // "$<": named reference in named mode (scanned to ">"); otherwise the
    // 2-char literal "$<" with scanning resumed right after it.
    ["$", "<", ..rest] ->
      case named_mode {
        True ->
          case take_group_name(rest, "") {
            Some(#(name, rest2)) ->
              tokenize_loop(rest2, named_mode, "", [
                NamedSeg(name),
                ..flush_literal(lit, segs)
              ])
            None -> tokenize_loop(rest, named_mode, lit <> "$<", segs)
          }
        False -> tokenize_loop(rest, named_mode, lit <> "$<", segs)
      }
    ["$", d1, d2, ..rest] ->
      case is_digit(d1), is_digit(d2) {
        True, True -> tokenize_two_digit(d1, d2, rest, named_mode, lit, segs)
        // "$N" followed by a non-digit — d2 is rescanned (it may start "$&").
        True, False ->
          tokenize_one_digit(d1, [d2, ..rest], named_mode, lit, segs)
        // Not a reference — "$" is literal, rescan from d1.
        False, _ ->
          tokenize_loop([d1, d2, ..rest], named_mode, lit <> "$", segs)
      }
    ["$", d1] ->
      case is_digit(d1) {
        True -> tokenize_one_digit(d1, [], named_mode, lit, segs)
        False -> tokenize_loop([d1], named_mode, lit <> "$", segs)
      }
    [ch, ..rest] -> tokenize_loop(rest, named_mode, lit <> ch, segs)
  }
}

/// Scan a "$<name>" group name up to the closing ">". None if unterminated.
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

/// "$N": a CaptureSeg for $1-$9; "$0" stays literal.
fn tokenize_one_digit(
  d1: String,
  rest: List(String),
  named_mode: Bool,
  lit: String,
  segs: List(ReplaceSegment),
) -> List(ReplaceSegment) {
  case digit_value(d1) {
    0 -> tokenize_loop(rest, named_mode, lit <> "$0", segs)
    idx ->
      tokenize_loop(rest, named_mode, "", [
        CaptureSeg(idx),
        ..flush_literal(lit, segs)
      ])
  }
}

/// "$NN": prefer the two-digit group, falling back to the single-digit group
/// + literal second digit, or the literal "$0d" when the first digit is 0.
fn tokenize_two_digit(
  d1: String,
  d2: String,
  rest: List(String),
  named_mode: Bool,
  lit: String,
  segs: List(ReplaceSegment),
) -> List(ReplaceSegment) {
  let two_idx = digit_value(d1) * 10 + digit_value(d2)
  case digit_value(d1), two_idx {
    // "$00" can never resolve to a group — always literal.
    0, 0 -> tokenize_loop(rest, named_mode, lit <> "$00", segs)
    0, _ ->
      tokenize_loop(rest, named_mode, "", [
        ZeroDigitSeg(two_idx, "$0" <> d2),
        ..flush_literal(lit, segs)
      ])
    one_idx, _ ->
      tokenize_loop(rest, named_mode, "", [
        TwoDigitSeg(two_idx, one_idx, d2),
        ..flush_literal(lit, segs)
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
