import arc/module/specifier

pub fn normalize_dotdot_does_not_consume_leading_dotdot_test() {
  assert specifier.normalize("../lib/../../x") == "../../x"
}

pub fn normalize_absolute_path_cannot_escape_root_test() {
  assert specifier.normalize("/a/../../x") == "/x"
}

pub fn normalize_relative_path_overshoot_keeps_dotdot_test() {
  assert specifier.normalize("a/../../x") == "../x"
}

pub fn normalize_plain_dotdot_pops_a_segment_test() {
  assert specifier.normalize("a/b/../c") == "a/c"
}

pub fn normalize_dot_segments_are_dropped_test() {
  assert specifier.normalize("./a/./b") == "a/b"
}

pub fn normalize_dot_is_current_directory_test() {
  assert specifier.normalize(".") == "."
}

pub fn normalize_segment_popped_by_dotdot_is_current_directory_test() {
  assert specifier.normalize("a/..") == "."
}

pub fn normalize_root_stays_root_test() {
  assert specifier.normalize("/") == "/"
}

pub fn normalize_dotdot_at_root_stays_root_test() {
  assert specifier.normalize("/..") == "/"
}

pub fn normalize_relative_trailing_slash_is_current_directory_test() {
  assert specifier.normalize("a/../") == "."
}

pub fn normalize_dot_slash_is_current_directory_test() {
  assert specifier.normalize("./") == "."
}

pub fn normalize_empty_path_is_current_directory_test() {
  assert specifier.normalize("") == "."
}

fn resolve(raw: String, parent: String) -> specifier.Specifier {
  specifier.resolve_path(specifier.raw(raw), specifier.resolved(parent))
}

fn a_path(identity: String) -> specifier.Specifier {
  specifier.PathSpecifier(specifier.resolved(identity))
}

pub fn resolve_specifier_relative_is_a_path_test() {
  assert resolve("./b.js", "dir/a.js") == a_path("dir/b.js")
}

pub fn resolve_specifier_parent_relative_is_a_path_test() {
  assert resolve("../b.js", "dir/sub/a.js") == a_path("dir/b.js")
}

pub fn resolve_specifier_absolute_is_a_normalized_path_test() {
  assert resolve("/x/../b.js", "a.js") == a_path("/b.js")
}

pub fn resolve_specifier_relative_directory_is_current_directory_test() {
  assert resolve("./dir/../", "a.js") == a_path(".")
}

pub fn resolve_specifier_bare_is_not_a_path_test() {
  assert resolve("fs", "dir/a.js")
    == specifier.BareSpecifier(specifier.raw("fs"))
}

pub fn resolve_specifier_url_is_bare_test() {
  assert resolve("https://x/y.js", "a.js")
    == specifier.BareSpecifier(specifier.raw("https://x/y.js"))
}
