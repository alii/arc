/// Full test262 conformance tests with snapshot-based failure tracking.
///
/// Bypasses EUnit's per-test reporting to avoid 53K dots.
/// Instead, shows a progress counter and prints only failures.
///
/// Usage:
///   TEST262=1 gleam test          — run test262 with snapshot
///   GENERATE_SNAPSHOT=1 gleam test — regenerate snapshot file
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{Some}
import gleam/set
import gleam/string
import lumen/parser
import simplifile
import test262_metadata.{type TestMetadata, Parse}
import test262_snapshot
import test_runner

const test_dir = "vendor/test262/test"

const snapshot_path = "test/test262_snapshot.txt"

/// EUnit test generator — returns empty. Actual work done in test262_run_test().
pub fn test262_test_() -> test_runner.EunitTests {
  test_runner.empty_tests()
}

/// Single EUnit test that runs the entire test262 suite with custom output.
pub fn test262_run_test() {
  let generate = test_runner.get_env_is_truthy("GENERATE_SNAPSHOT")
  let run = test_runner.get_env_is_truthy("TEST262")

  case generate || run {
    False -> Nil
    True -> {
      let snapshot = case generate {
        True -> set.new()
        False -> test262_snapshot.load_snapshot(snapshot_path)
      }

      let assert Ok(all_files) = simplifile.get_files(in: test_dir)
      let js_files =
        all_files
        |> list.filter(fn(path) {
          string.ends_with(path, ".js") && !string.contains(path, "_FIXTURE")
        })
        |> list.sort(string.compare)

      let total = list.length(js_files)
      let prefix = test_dir <> "/"

      io.println("\ntest262: running " <> int.to_string(total) <> " tests...")

      let result =
        list.index_fold(js_files, #(0, 0, []), fn(acc, full_path, idx) {
          let #(pass_count, fail_count, failures) = acc

          // Progress counter every 500 tests
          case idx % 500 {
            0 -> print_progress(idx, total, pass_count, fail_count)
            _ -> Nil
          }

          let relative = case string.starts_with(full_path, prefix) {
            True -> string.drop_start(full_path, string.length(prefix))
            False -> full_path
          }

          case simplifile.read(full_path) {
            Error(_) -> #(pass_count, fail_count + 1, [relative, ..failures])
            Ok(source) -> {
              let metadata = test262_metadata.parse_metadata(source)
              let test_result = run_parse_test(metadata, source)
              let in_snapshot =
                test262_snapshot.is_expected_failure(snapshot, relative)

              case generate {
                True ->
                  case test_result {
                    Ok(_) -> #(pass_count + 1, fail_count, failures)
                    Error(_) -> #(pass_count, fail_count + 1, [
                      relative,
                      ..failures
                    ])
                  }
                False ->
                  case in_snapshot, test_result {
                    // Expected failure
                    True, Error(_) -> #(pass_count + 1, fail_count, failures)
                    // Unexpected pass
                    True, Ok(_) -> {
                      io.println(
                        "\n  UNEXPECTED PASS: "
                        <> relative
                        <> " (remove from snapshot)",
                      )
                      #(pass_count, fail_count + 1, failures)
                    }
                    // Expected pass
                    False, Ok(_) -> #(pass_count + 1, fail_count, failures)
                    // Unexpected failure
                    False, Error(reason) -> {
                      io.println("\n  FAIL: " <> relative <> " — " <> reason)
                      #(pass_count, fail_count + 1, failures)
                    }
                  }
              }
            }
          }
        })

      let #(pass_count, fail_count, failures) = result

      // Clear progress line and print final summary
      clear_line()

      case generate {
        True -> {
          let assert Ok(_) =
            test262_snapshot.write_snapshot(snapshot_path, failures)
          io.println(
            "test262: "
            <> int.to_string(pass_count)
            <> " pass, "
            <> int.to_string(fail_count)
            <> " fail — wrote snapshot to "
            <> snapshot_path,
          )
        }
        False -> {
          let snapshot_count = set.size(snapshot)
          io.println(
            "test262: "
            <> int.to_string(pass_count)
            <> " pass ("
            <> int.to_string(snapshot_count)
            <> " expected failures in snapshot)",
          )
          case fail_count > 0 {
            True ->
              panic as {
                "test262: "
                <> int.to_string(fail_count)
                <> " unexpected failures"
              }
            False -> Nil
          }
        }
      }
    }
  }
}

@external(erlang, "io", "format")
fn erl_io_format(fmt: String, args: List(String)) -> Nil

fn clear_line() -> Nil {
  // \r moves to start of line, ESC[K clears to end of line
  let assert Ok(esc) = string.utf_codepoint(0x1b)
  io.print("\r" <> string.from_utf_codepoints([esc]) <> "[K")
}

fn print_progress(current: Int, total: Int, passes: Int, fails: Int) -> Nil {
  erl_io_format("  \r  [~s/~s] ~s pass, ~s fail", [
    int.to_string(current),
    int.to_string(total),
    int.to_string(passes),
    int.to_string(fails),
  ])
}

// --- Parse test logic ---

fn run_parse_test(metadata: TestMetadata, source: String) -> Result(Nil, String) {
  let mode = case list.contains(metadata.flags, "module") {
    True -> parser.Module
    False -> parser.Script
  }

  let source_to_parse = case list.contains(metadata.flags, "onlyStrict") {
    True -> "\"use strict\";\n" <> source
    False -> source
  }

  let expect_parse_error = case metadata.negative_phase {
    Some(Parse) -> True
    _ -> False
  }

  let parse_result = parser.parse(source_to_parse, mode)

  case parse_result, expect_parse_error {
    Ok(_), False -> Ok(Nil)
    Error(_), True -> Ok(Nil)
    Ok(_), True ->
      Error(
        "Expected parse error (negative.phase: parse) but parsed successfully",
      )
    Error(err), False ->
      Error("Parse error: " <> parser.parse_error_to_string(err))
  }
}
