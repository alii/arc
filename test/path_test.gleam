import arc/esm
import arc/internal/path

// ----------------------------------------------------------------------------
// path.normalize — `.` / `..` resolution.
//
// The `..` arm must only pop a REAL directory segment. It must not consume a
// preceding `..` (a leading run of `..`s escapes the base directory and has to
// be preserved), and it must not climb above the root of an absolute path.
// ----------------------------------------------------------------------------

pub fn normalize_dotdot_does_not_consume_leading_dotdot_test() {
  assert path.normalize("../lib/../../x") == "../../x"
}

pub fn normalize_absolute_path_cannot_escape_root_test() {
  assert path.normalize("/a/../../x") == "/x"
}

pub fn normalize_relative_path_overshoot_keeps_dotdot_test() {
  assert path.normalize("a/../../x") == "../x"
}

pub fn normalize_plain_dotdot_pops_a_segment_test() {
  assert path.normalize("a/b/../c") == "a/c"
}

pub fn normalize_dot_segments_are_dropped_test() {
  assert path.normalize("./a/./b") == "a/b"
}

// ----------------------------------------------------------------------------
// path.normalize — a path that cancels down to no segments still denotes a
// DIRECTORY, never the empty string (an empty module identity naming nothing).
// ----------------------------------------------------------------------------

pub fn normalize_dot_is_current_directory_test() {
  assert path.normalize(".") == "."
}

pub fn normalize_segment_popped_by_dotdot_is_current_directory_test() {
  assert path.normalize("a/..") == "."
}

pub fn normalize_root_stays_root_test() {
  assert path.normalize("/") == "/"
}

pub fn normalize_dotdot_at_root_stays_root_test() {
  assert path.normalize("/..") == "/"
}

// A trailing slash also leaves an empty segment behind, so a RELATIVE path that
// cancels to nothing must not be mistaken for the filesystem root.
pub fn normalize_relative_trailing_slash_is_current_directory_test() {
  assert path.normalize("a/../") == "."
}

pub fn normalize_dot_slash_is_current_directory_test() {
  assert path.normalize("./") == "."
}

pub fn normalize_empty_path_is_current_directory_test() {
  assert path.normalize("") == "."
}

// ----------------------------------------------------------------------------
// path.resolve_specifier — path-shaped vs bare specifiers are different kinds
// of thing, and the type says so.
// ----------------------------------------------------------------------------

/// A resolved specifier is a module identity, a raw one is source text.
fn resolve(raw: String, parent: String) -> path.Specifier {
  path.resolve_specifier(esm.raw(raw), esm.resolved_unchecked(parent))
}

fn a_path(identity: String) -> path.Specifier {
  path.PathSpecifier(esm.resolved_unchecked(identity))
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
  assert resolve("fs", "dir/a.js") == path.BareSpecifier(esm.raw("fs"))
}

pub fn resolve_specifier_url_is_bare_test() {
  assert resolve("https://x/y.js", "a.js")
    == path.BareSpecifier(esm.raw("https://x/y.js"))
}
