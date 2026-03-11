/// Test utilities — env vars, timeouts, parallel runner.
/// No EUnit dependency.
/// Read an environment variable. Returns Ok(value) or Error(Nil).
@external(erlang, "test_runner_ffi", "get_env")
pub fn get_env(name: String) -> Result(String, Nil)

pub fn get_env_is_truthy(name: String) {
  case get_env(name) {
    Ok("1") | Ok("true") -> True
    _ -> False
  }
}

/// Run a zero-arg function with a timeout in milliseconds.
/// Returns Ok(result) or Error("timeout").
@external(erlang, "test_runner_ffi", "run_with_timeout")
pub fn run_with_timeout(fun: fn() -> a, timeout_ms: Int) -> Result(a, String)

/// List all .js files in a directory, returning filenames (not full paths).
@external(erlang, "test_runner_ffi", "list_files")
pub fn list_files(dir: String) -> Result(List(String), String)

/// Run test_fn over all items in parallel (one BEAM process per item).
/// Returns list of (item, reason) for failures only.
@external(erlang, "test_runner_ffi", "run_parallel")
pub fn run_parallel(
  items: List(a),
  test_fn: fn(a) -> Result(Nil, String),
) -> List(#(a, String))
