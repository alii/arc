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
