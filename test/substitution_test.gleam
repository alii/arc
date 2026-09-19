import arc/rt/builtins/substitution.{
  type PlainSegment, CaptureSegment, LiteralSegment, MatchContext,
  MatchedSegment, NamedGroupSegment, Plain,
}

const combining_acute = "\u{0301}"

fn tokenize(template: String) -> List(PlainSegment) {
  substitution.tokenize_plain(template)
}

fn ctx() -> substitution.MatchContext {
  MatchContext(
    matched: "X",
    before: fn() { "a" },
    after: fn() { "b" },
    capture: fn(_) { "X" },
    capture_count: 1,
  )
}

// getsubstitution works on code units, not graphemes

pub fn dollar_escape_before_a_combining_mark_still_matches_test() {
  assert tokenize("$&" <> combining_acute)
    == [MatchedSegment, LiteralSegment(combining_acute)]
}

pub fn capture_escape_before_a_combining_mark_still_matches_test() {
  assert tokenize("$1" <> combining_acute)
    == [CaptureSegment(1), LiteralSegment(combining_acute)]
}

pub fn dollar_dollar_before_a_combining_mark_still_matches_test() {
  assert tokenize("$$" <> combining_acute)
    == [LiteralSegment("$" <> combining_acute)]
}

pub fn resolving_a_matched_seg_before_a_combining_mark_test() {
  assert substitution.expand_without_named(
      tokenize("$&" <> combining_acute),
      ctx(),
    )
    == "X" <> combining_acute
}

pub fn a_template_without_dollar_is_one_literal_test() {
  assert tokenize("plain") == [LiteralSegment("plain")]
}

pub fn plain_mode_keeps_dollar_angle_literal_test() {
  assert tokenize("$<a>") == [LiteralSegment("$<a>")]
}

pub fn named_mode_scans_a_group_name_test() {
  assert substitution.tokenize_named("x$<a>y")
    == [
      Plain(LiteralSegment("x")),
      NamedGroupSegment("a"),
      Plain(LiteralSegment("y")),
    ]
}

pub fn named_mode_keeps_an_unterminated_group_name_literal_test() {
  assert substitution.tokenize_named("$<a") == [Plain(LiteralSegment("$<a"))]
}
