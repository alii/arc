import arc/vm/builtins/substitution.{
  type PlainSegment, CaptureSeg, Ctx, LiteralSeg, MatchedSeg, NamedSeg, Plain,
}

const combining_acute = "\u{0301}"

fn tokenize(template: String) -> List(PlainSegment) {
  substitution.tokenize_plain(template)
}

fn ctx() -> substitution.Ctx {
  Ctx(
    matched: "X",
    before: fn() { "a" },
    after: fn() { "b" },
    capture: fn(_) { "X" },
    m: 1,
  )
}

// ----------------------------------------------------------------------------
// GetSubstitution is defined over code units, not grapheme clusters. A `$`
// escape immediately followed by a combining mark forms ONE grapheme with it,
// so a grapheme-based scan swallows the escape and emits it as literal text.
// ----------------------------------------------------------------------------

pub fn dollar_escape_before_a_combining_mark_still_matches_test() {
  assert tokenize("$&" <> combining_acute)
    == [MatchedSeg, LiteralSeg(combining_acute)]
}

pub fn capture_escape_before_a_combining_mark_still_matches_test() {
  assert tokenize("$1" <> combining_acute)
    == [CaptureSeg(1), LiteralSeg(combining_acute)]
}

pub fn dollar_dollar_before_a_combining_mark_still_matches_test() {
  assert tokenize("$$" <> combining_acute)
    == [LiteralSeg("$" <> combining_acute)]
}

/// `"aXb".replaceAll("X", "$&" <> acute)` must be `"aX́b"`, not `"a$&́b"`.
pub fn resolving_a_matched_seg_before_a_combining_mark_test() {
  assert substitution.resolve_without_named(
      tokenize("$&" <> combining_acute),
      ctx(),
    )
    == "X" <> combining_acute
}

pub fn a_template_without_dollar_is_one_literal_test() {
  assert tokenize("plain") == [LiteralSeg("plain")]
}

// ----------------------------------------------------------------------------
// "$<" only opens a named reference in named mode; plain mode cannot even
// represent one (`PlainSegment` has no `NamedSeg`).
// ----------------------------------------------------------------------------

pub fn plain_mode_keeps_dollar_angle_literal_test() {
  assert tokenize("$<a>") == [LiteralSeg("$<a>")]
}

pub fn named_mode_scans_a_group_name_test() {
  assert substitution.tokenize_named("x$<a>y")
    == [Plain(LiteralSeg("x")), NamedSeg("a"), Plain(LiteralSeg("y"))]
}

pub fn named_mode_keeps_an_unterminated_group_name_literal_test() {
  assert substitution.tokenize_named("$<a") == [Plain(LiteralSeg("$<a"))]
}
