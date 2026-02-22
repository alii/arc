/// Full test262 conformance tests with snapshot-based failure tracking.
///
/// Normal mode: runs all tests in parallel, checks against snapshot.
/// Generate mode (GENERATE_SNAPSHOT=1): runs all tests sequentially, writes snapshot file.
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

/// EUnit test generator — entry point.
/// Only runs when TEST262=1 (or GENERATE_SNAPSHOT=1).
/// Usage: TEST262=1 gleam test
pub fn test262_test_() -> test_runner.EunitTests {
  case test_runner.get_env("GENERATE_SNAPSHOT") {
    Ok(_) -> test_runner.empty_tests()
    _ ->
      case test_runner.get_env("TEST262") {
        Ok(_) -> {
          let snapshot = test262_snapshot.load_snapshot(snapshot_path)
          test_runner.generate_recursive_file_tests(
            test_dir,
            fn(path, source) {
              run_test_with_snapshot(snapshot, path, source)
            },
          )
        }
        Error(_) -> test_runner.empty_tests()
      }
  }
}

fn run_test_with_snapshot(
  snapshot: set.Set(String),
  path: String,
  source: String,
) -> Result(Nil, String) {
  let metadata = test262_metadata.parse_metadata(source)
  let test_result = run_parse_test(metadata, source)
  let in_snapshot = test262_snapshot.is_expected_failure(snapshot, path)

  case in_snapshot, test_result {
    True, Error(_) -> Ok(Nil)
    True, Ok(_) ->
      Error(
        "UNEXPECTED PASS: " <> path <> " now passes; remove from snapshot",
      )
    False, Ok(_) -> Ok(Nil)
    False, Error(reason) -> Error(reason)
  }
}

/// Snapshot generation — only active when GENERATE_SNAPSHOT=1.
/// Runs all files sequentially, collects failures, writes snapshot.
pub fn generate_snapshot_test() {
  case test_runner.get_env("GENERATE_SNAPSHOT") {
    Error(_) -> Nil
    Ok(_) -> {
      io.println("\nGenerating test262 snapshot...")
      let assert Ok(all_files) = simplifile.get_files(in: test_dir)
      let js_files =
        all_files
        |> list.filter(fn(path) {
          string.ends_with(path, ".js")
          && !string.contains(path, "_FIXTURE")
        })
        |> list.sort(string.compare)

      io.println(
        "Found " <> int.to_string(list.length(js_files)) <> " test files",
      )

      let prefix = test_dir <> "/"
      let failures =
        list.filter_map(js_files, fn(full_path) {
          let relative = case string.starts_with(full_path, prefix) {
            True -> string.drop_start(full_path, string.length(prefix))
            False -> full_path
          }
          case simplifile.read(full_path) {
            Error(_) -> Ok(relative)
            Ok(source) -> {
              let metadata = test262_metadata.parse_metadata(source)
              case run_parse_test(metadata, source) {
                Ok(_) -> Error(Nil)
                Error(_) -> Ok(relative)
              }
            }
          }
        })

      let total = list.length(js_files)
      let fail_count = list.length(failures)
      let pass_count = total - fail_count

      io.println(
        "\ntest262 snapshot results: "
        <> int.to_string(pass_count)
        <> " pass, "
        <> int.to_string(fail_count)
        <> " fail out of "
        <> int.to_string(total),
      )

      let assert Ok(_) =
        test262_snapshot.write_snapshot(snapshot_path, failures)
      io.println("Wrote snapshot to " <> snapshot_path)
    }
  }
}

// --- Parse test logic ---

fn run_parse_test(
  metadata: TestMetadata,
  source: String,
) -> Result(Nil, String) {
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
