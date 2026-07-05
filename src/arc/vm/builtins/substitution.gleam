//// ES2024 §22.1.3.19.1 GetSubstitution — the replacement-template language
//// (`$$`, `$&`, `` $` ``, `$'`, `$1`..`$99`, `$<name>`) shared by
//// `String.prototype.replace`/`replaceAll` and `RegExp.prototype[@@replace]`.
////
//// The template is tokenized ONCE per replace call and each match then only
//// resolves the segments. Resolution is pure apart from `$<name>`, whose
//// `Get(namedCaptures, name)` is observable, so the named resolver hands that
//// back as `NamedRef` for the caller to perform.
////
//// Two segment types keep the two worlds apart at compile time:
//// `tokenize_plain` yields `PlainSegment`s, which contain no named reference
//// and resolve to a plain `String`; `tokenize_named` yields `NamedSegment`s,
//// which may. `String.prototype` (a string search has no captures) only ever
//// tokenizes plain, so it cannot be handed a `NamedSeg` at all.

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// One pre-tokenized piece of a replacement template that resolves purely.
/// The common no-dollar template is a single `LiteralSeg`.
pub type PlainSegment {
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
}

/// A segment of a template tokenized against a match result that has a
/// `groups` object: everything a `PlainSegment` can be, plus the named
/// reference that only exists in that mode.
pub type NamedSegment {
  Plain(seg: PlainSegment)
  /// "$<name>" — named capture: Get(namedCaptures, name) then ToString
  /// (undefined → "").
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

/// The outcome of resolving one `NamedSegment`: either final text, or the one
/// case that needs an observable `Get` the caller must perform.
pub type Resolved {
  Text(text: String)
  NamedRef(name: String)
}

/// GetSubstitution for a single plain segment. Total and pure.
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

/// GetSubstitution for a single segment of a named-mode template. Pure: the
/// `Get` behind a `NamedSeg` is handed back to the caller as `NamedRef`.
pub fn resolve(seg: NamedSegment, ctx: Ctx) -> Resolved {
  case seg {
    Plain(p) -> Text(resolve_plain(p, ctx))
    NamedSeg(name) -> NamedRef(name)
  }
}

/// GetSubstitution over a whole plain-mode template. Nothing here is
/// observable, so it is a plain function.
pub fn resolve_without_named(segments: List(PlainSegment), ctx: Ctx) -> String {
  segments
  |> resolve_plain_parts(ctx)
  |> string.concat
}

/// The resolved pieces of a plain-mode template, unconcatenated — for callers
/// that must size-check the result before materialising it.
pub fn resolve_plain_parts(
  segments: List(PlainSegment),
  ctx: Ctx,
) -> List(String) {
  list.map(segments, resolve_plain(_, ctx))
}

/// How the tokenizer builds a segment of the caller's chosen segment type:
/// every mode wraps a `PlainSegment`, and only named mode can build a
/// `NamedSeg` — so `named: None` is what makes "$<" a 2-char literal.
type Emit(seg) {
  Emit(plain: fn(PlainSegment) -> seg, named: Option(fn(String) -> seg))
}

/// Tokenize a replacement template for a match with no `groups` object: "$<"
/// is the 2-char literal "$<" and scanning resumes immediately after it.
pub fn tokenize_plain(template: String) -> List(PlainSegment) {
  tokenize(template, Emit(plain: fn(p) { p }, named: None))
}

/// Tokenize a replacement template for a match with a `groups` object: "$<"
/// opens a named reference scanned to ">". Callers that can see either shape
/// tokenize both ways and pick per match result.
pub fn tokenize_named(template: String) -> List(NamedSegment) {
  tokenize(template, Emit(plain: Plain, named: Some(NamedSeg)))
}

/// Called once per replace call; templates without "$" skip segmentation.
fn tokenize(template: String, emit: Emit(seg)) -> List(seg) {
  case string.contains(template, "$") {
    False -> [emit.plain(LiteralSeg(template))]
    True -> tokenize_loop(to_code_points(template), emit, "", [])
  }
}

/// GetSubstitution is defined over code units, NOT grapheme clusters: in
/// `"$&" <> combining_acute` the escape is `$&` followed by a lone combining
/// mark, even though those three code points form ONE grapheme. Splitting on
/// graphemes would fuse `&` with the mark and leave the `$&` unrecognised, so
/// the tokenizer scans one code point per element.
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
    // "$<": named reference in named mode (scanned to ">"); otherwise the
    // 2-char literal "$<" with scanning resumed right after it.
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
        // "$N" followed by a non-digit — d2 is rescanned (it may start "$&").
        True, False -> tokenize_one_digit(d1, [d2, ..rest], emit, lit, segs)
        // Not a reference — "$" is literal, rescan from d1.
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

/// "$NN": prefer the two-digit group, falling back to the single-digit group
/// + literal second digit, or the literal "$0d" when the first digit is 0.
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
    // "$00" can never resolve to a group — always literal.
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
