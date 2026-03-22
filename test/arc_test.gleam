@external(erlang, "arc_test_ffi", "main")
pub fn main() -> Nil {
  panic as "Test suite is BEAM-only. Run `gleam test` without --target=javascript."
}
