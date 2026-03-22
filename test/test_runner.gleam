/// Test utilities — env vars, timeouts, parallel runner.
/// No EUnit dependency.
/// Read an environment variable. Returns Ok(value) or Error(Nil).
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

/// Run a zero-arg function with a timeout in milliseconds.
/// Returns Ok(result) or Error("timeout").
@external(erlang, "test_runner_ffi", "run_with_timeout")
pub fn run_with_timeout(
  _fun: fn() -> a,
  _timeout_ms: Int,
) -> Result(a, String) {
  panic as "test_runner is BEAM-only"
}

/// List all .js files in a directory, returning filenames (not full paths).
@external(erlang, "test_runner_ffi", "list_files")
pub fn list_files(_dir: String) -> Result(List(String), String) {
  panic as "test_runner is BEAM-only"
}

/// Run test_fn over all items in parallel (one BEAM process per item).
/// Returns list of (item, reason) for failures only.
@external(erlang, "test_runner_ffi", "run_parallel")
pub fn run_parallel(
  _items: List(a),
  _test_fn: fn(a) -> Result(Nil, String),
) -> List(#(a, String)) {
  panic as "test_runner is BEAM-only"
}
