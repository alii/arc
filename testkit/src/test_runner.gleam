@external(erlang, "test_runner_ffi", "get_env")
pub fn get_env(_name: String) -> Result(String, Nil) {
  Error(Nil)
}

pub fn get_env_is_truthy(name: String) {
  case get_env(name) {
    Ok("1") | Ok("true") -> True
    _ -> False
  }
}

@external(erlang, "test_runner_ffi", "run_with_timeout")
pub fn run_with_timeout(
  _fun: fn() -> a,
  _timeout_ms: Int,
) -> Result(a, String) {
  panic as "test_runner is BEAM-only"
}

@external(erlang, "test_runner_ffi", "list_files")
pub fn list_files(_dir: String) -> Result(List(String), String) {
  panic as "test_runner is BEAM-only"
}

@external(erlang, "test_runner_ffi", "run_parallel")
pub fn run_parallel(
  _items: List(a),
  _test_fn: fn(a) -> Result(Nil, String),
) -> List(#(a, String)) {
  panic as "test_runner is BEAM-only"
}
