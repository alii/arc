/// Generic file-based test runner for EUnit.
/// Reads all .js files from a directory and runs a callback on each one,
/// generating individual EUnit test descriptors.
/// Opaque type representing a list of EUnit test descriptors.
pub type EunitTests

/// Generate EUnit test descriptors for all .js files in a directory.
///
/// `test_fn` receives `(filename, source)` and should return
/// `Ok(Nil)` on pass or `Error(reason)` on failure.
pub fn generate_file_tests(
  dir: String,
  test_fn: fn(String, String) -> Result(Nil, String),
) -> EunitTests {
  case generate_eunit_tests_ffi(dir, test_fn) {
    Ok(tests) -> tests
    Error(err) ->
      panic as { "Failed to generate tests for " <> dir <> ": " <> err }
  }
}

@external(erlang, "test_runner_ffi", "generate_eunit_tests")
@external(javascript, "../test/test_runner_ffi.mjs", "generate_eunit_tests")
fn generate_eunit_tests_ffi(
  dir: String,
  test_fn: fn(String, String) -> Result(Nil, String),
) -> Result(EunitTests, String)

/// Generate EUnit test descriptors for all .js files recursively in a directory.
/// `test_fn` receives `(relative_path, source)` where relative_path is relative to dir.
pub fn generate_recursive_file_tests(
  dir: String,
  test_fn: fn(String, String) -> Result(Nil, String),
) -> EunitTests {
  case generate_recursive_eunit_tests_ffi(dir, test_fn) {
    Ok(tests) -> tests
    Error(err) ->
      panic as { "Failed to generate tests for " <> dir <> ": " <> err }
  }
}

@external(erlang, "test_runner_ffi", "generate_recursive_eunit_tests")
fn generate_recursive_eunit_tests_ffi(
  dir: String,
  test_fn: fn(String, String) -> Result(Nil, String),
) -> Result(EunitTests, String)

/// Read an environment variable. Returns Ok(value) or Error(Nil).
@external(erlang, "test_runner_ffi", "get_env")
pub fn get_env(name: String) -> Result(String, Nil)

pub fn get_env_is_truthy(name: String) {
  case get_env(name) {
    Ok("1") | Ok("true") -> True
    _ -> False
  }
}

/// Return an empty EUnit test descriptor (no tests to run).
@external(erlang, "test_runner_ffi", "empty_tests")
pub fn empty_tests() -> EunitTests

/// Create a single EUnit test with a custom timeout (in seconds).
@external(erlang, "test_runner_ffi", "timeout_test")
pub fn timeout_test(timeout_sec: Int, fun: fn() -> Nil) -> EunitTests

/// Run a zero-arg function with a timeout in milliseconds.
/// Returns Ok(result) or Error("timeout").
@external(erlang, "test_runner_ffi", "run_with_timeout")
pub fn run_with_timeout(fun: fn() -> a, timeout_ms: Int) -> Result(a, String)
