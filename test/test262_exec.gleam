/// test262 execution conformance runner.
///
/// Compiles and runs test262 tests through the full pipeline:
///   Parse → Compile → Execute
///
/// Usage:
///   TEST262_EXEC=1 gleam test         — run execution tests, output results
///   TEST262_EXEC=1 RESULTS_FILE=path gleam test — also write JSON results
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import lumen/compiler
import lumen/parser
import lumen/vm/builtins
import lumen/vm/heap
import lumen/vm/value
import lumen/vm/vm
import simplifile
import test262_metadata.{type TestMetadata, Parse, Resolution, Runtime}
import test_runner

const test_dir = "vendor/test262/test"

const harness_dir = "vendor/test262/harness"

/// EUnit test generator — returns a test with 1-hour timeout when enabled,
/// or empty when disabled (to avoid blocking normal test runs).
pub fn test262_exec_test_() -> test_runner.EunitTests {
  case test_runner.get_env_is_truthy("TEST262_EXEC") {
    False -> test_runner.empty_tests()
    True -> test_runner.timeout_test(3600, run_execution_suite)
  }
}

/// Outcome of running a single test.
type TestOutcome {
  Pass
  Fail(reason: String)
  Skip(reason: String)
}

fn run_execution_suite() {
  let assert Ok(all_files) = simplifile.get_files(in: test_dir)
  let js_files =
    all_files
    |> list.filter(fn(path) {
      string.ends_with(path, ".js") && !string.contains(path, "_FIXTURE")
    })
    |> list.sort(string.compare)

  let total = list.length(js_files)
  let prefix = test_dir <> "/"

  io.println("\ntest262 exec: running " <> int.to_string(total) <> " tests...")

  let result =
    list.index_fold(js_files, #(0, 0, 0), fn(acc, full_path, idx) {
      let #(pass_count, fail_count, skip_count) = acc

      // Progress counter every 1000 tests
      case idx % 1000 {
        0 -> print_progress(idx, total, pass_count, fail_count, skip_count)
        _ -> Nil
      }

      let relative = case string.starts_with(full_path, prefix) {
        True -> string.drop_start(full_path, string.length(prefix))
        False -> full_path
      }

      case simplifile.read(full_path) {
        Error(_) -> #(pass_count, fail_count + 1, skip_count)
        Ok(source) -> {
          let metadata = test262_metadata.parse_metadata(source)
          let outcome = run_single_test(metadata, source, relative)
          case outcome {
            Pass -> #(pass_count + 1, fail_count, skip_count)
            Fail(_) -> #(pass_count, fail_count + 1, skip_count)
            Skip(_) -> #(pass_count, fail_count, skip_count + 1)
          }
        }
      }
    })

  let #(pass_count, fail_count, skip_count) = result
  clear_line()

  let tested = pass_count + fail_count
  let pct = case tested > 0 {
    True -> {
      let pct_x100 = { pass_count * 10_000 } / tested
      let whole = pct_x100 / 100
      let frac = pct_x100 % 100
      int.to_string(whole)
      <> "."
      <> case frac < 10 {
        True -> "0" <> int.to_string(frac)
        False -> int.to_string(frac)
      }
    }
    False -> "0.00"
  }

  io.println(
    "test262 exec: "
    <> int.to_string(pass_count)
    <> " pass, "
    <> int.to_string(fail_count)
    <> " fail, "
    <> int.to_string(skip_count)
    <> " skip ("
    <> pct
    <> "% of "
    <> int.to_string(tested)
    <> " tested)",
  )

  // Write results to JSON file if RESULTS_FILE is set
  case test_runner.get_env("RESULTS_FILE") {
    Ok(path) -> {
      let json =
        "{\"pass\":"
        <> int.to_string(pass_count)
        <> ",\"fail\":"
        <> int.to_string(fail_count)
        <> ",\"skip\":"
        <> int.to_string(skip_count)
        <> ",\"total\":"
        <> int.to_string(total)
        <> ",\"tested\":"
        <> int.to_string(tested)
        <> ",\"percent\":"
        <> pct
        <> "}"
      case simplifile.write(path, json) {
        Ok(_) -> io.println("Results written to " <> path)
        Error(err) ->
          io.println(
            "Warning: could not write results: " <> string.inspect(err),
          )
      }
    }
    Error(_) -> Nil
  }
}

/// Run a single test262 test through the full pipeline.
fn run_single_test(
  metadata: TestMetadata,
  source: String,
  _relative_path: String,
) -> TestOutcome {
  // Skip module tests — not supported
  case list.contains(metadata.flags, "module") {
    True -> Skip("module")
    False -> {
      // Skip async tests — not supported
      case list.contains(metadata.flags, "async") {
        True -> Skip("async")
        False -> run_test_by_phase(metadata, source)
      }
    }
  }
}

/// Route test based on its negative phase.
fn run_test_by_phase(metadata: TestMetadata, source: String) -> TestOutcome {
  // Handle onlyStrict flag
  let source = case list.contains(metadata.flags, "onlyStrict") {
    True -> "\"use strict\";\n" <> source
    False -> source
  }

  case metadata.negative_phase {
    // Parse-phase negative test: should fail to parse
    Some(Parse) -> run_parse_negative_test(source)
    // Resolution-phase: skip (module resolution)
    Some(Resolution) -> Skip("resolution")
    // Runtime-phase negative test: should throw at runtime
    Some(Runtime) -> run_runtime_negative_test(metadata, source)
    // No negative phase: should parse, compile, and run without throwing
    None -> run_positive_test(metadata, source)
  }
}

/// Test that should fail to parse.
fn run_parse_negative_test(source: String) -> TestOutcome {
  case parser.parse(source, parser.Script) {
    Error(_) -> Pass
    Ok(_) -> Fail("expected parse error but parsed successfully")
  }
}

/// Test that should throw at runtime.
fn run_runtime_negative_test(
  metadata: TestMetadata,
  source: String,
) -> TestOutcome {
  let full_source = prepend_harness(metadata, source)
  case parse_compile_run(full_source) {
    // Threw — that's what we want
    Ok(vm.ThrowCompletion(_, _)) -> Pass
    // Normal completion — bad, should have thrown
    Ok(vm.NormalCompletion(_, _)) ->
      Fail("expected runtime throw but completed normally")
    // Parse/compile error — also counts as "didn't throw at runtime"
    Error(reason) -> Fail("expected runtime throw but got: " <> reason)
  }
}

/// Test that should complete normally (no throw).
fn run_positive_test(metadata: TestMetadata, source: String) -> TestOutcome {
  let full_source = prepend_harness(metadata, source)
  case parse_compile_run(full_source) {
    Ok(vm.NormalCompletion(_, _)) -> Pass
    Ok(vm.ThrowCompletion(thrown, _)) ->
      Fail("unexpected throw: " <> inspect_js_value(thrown))
    Error(reason) -> Fail(reason)
  }
}

/// Per-test timeout in milliseconds (5 seconds).
const test_timeout_ms = 5000

/// Parse, compile, and execute JS source (with timeout).
fn parse_compile_run(source: String) -> Result(vm.Completion, String) {
  case
    test_runner.run_with_timeout(
      fn() { do_parse_compile_run(source) },
      test_timeout_ms,
    )
  {
    Ok(result) -> result
    Error(_) -> Error("timeout")
  }
}

/// Parse, compile, and execute JS source (inner, no timeout).
fn do_parse_compile_run(source: String) -> Result(vm.Completion, String) {
  case parser.parse(source, parser.Script) {
    Error(err) -> Error("parse: " <> parser.parse_error_to_string(err))
    Ok(program) ->
      case compiler.compile(program) {
        Error(compiler.Unsupported(desc)) ->
          Error("compile: unsupported " <> desc)
        Error(compiler.BreakOutsideLoop) -> Error("compile: break outside loop")
        Error(compiler.ContinueOutsideLoop) ->
          Error("compile: continue outside loop")
        Ok(template) -> {
          let h = heap.new()
          let #(h, b) = builtins.init(h)
          let globals = make_test262_globals(b)
          case vm.run_with_globals(template, h, b, globals) {
            Ok(completion) -> Ok(completion)
            Error(vm_err) -> Error("vm: " <> string.inspect(vm_err))
          }
        }
      }
  }
}

/// Pre-populated globals for test262 execution.
fn make_test262_globals(
  b: builtins.Builtins,
) -> dict.Dict(String, value.JsValue) {
  dict.from_list([
    #("NaN", value.JsNumber(value.NaN)),
    #("Infinity", value.JsNumber(value.Infinity)),
    #("undefined", value.JsUndefined),
    #("Object", value.JsObject(b.object.constructor)),
    #("Function", value.JsObject(b.function.constructor)),
    #("Array", value.JsObject(b.array.constructor)),
    #("Error", value.JsObject(b.error.constructor)),
    #("TypeError", value.JsObject(b.type_error.constructor)),
    #("ReferenceError", value.JsObject(b.reference_error.constructor)),
    #("RangeError", value.JsObject(b.range_error.constructor)),
    #("SyntaxError", value.JsObject(b.syntax_error.constructor)),
    #("Math", value.JsObject(b.math)),
  ])
}

/// Prepend harness files to test source.
/// Per test262 spec: assert.js and sta.js are ALWAYS included (unless raw flag).
/// Additional includes come from the test's metadata.
fn prepend_harness(metadata: TestMetadata, source: String) -> String {
  // Both assert.js and sta.js are always included unless raw flag
  let is_raw = list.contains(metadata.flags, "raw")

  case is_raw {
    True -> source
    False -> {
      // Build list: assert.js + sta.js first, then metadata includes
      // (dedup any that are already in the default set)
      let default_harness = ["assert.js", "sta.js"]
      let extra_includes =
        metadata.includes
        |> list.filter(fn(f) { !list.contains(default_harness, f) })
      let harness_files = list.append(default_harness, extra_includes)

      // Read and prepend each harness file
      let harness_source =
        list.filter_map(harness_files, fn(filename) {
          let path = harness_dir <> "/" <> filename
          case simplifile.read(path) {
            Ok(content) -> Ok(content)
            Error(_) -> Error(Nil)
          }
        })
        |> string.join("\n")

      case harness_source {
        "" -> source
        _ -> harness_source <> "\n" <> source
      }
    }
  }
}

/// Simple string representation of a JsValue for error messages.
fn inspect_js_value(val: value.JsValue) -> String {
  case val {
    value.JsUndefined -> "undefined"
    value.JsNull -> "null"
    value.JsBool(True) -> "true"
    value.JsBool(False) -> "false"
    value.JsNumber(value.Finite(n)) -> string.inspect(n)
    value.JsNumber(value.NaN) -> "NaN"
    value.JsNumber(value.Infinity) -> "Infinity"
    value.JsNumber(value.NegInfinity) -> "-Infinity"
    value.JsString(s) -> "\"" <> s <> "\""
    value.JsObject(_) -> "[object]"
    value.JsSymbol(_) -> "Symbol()"
    value.JsBigInt(value.BigInt(n)) -> int.to_string(n) <> "n"
    value.JsUninitialized -> "<uninitialized>"
  }
}

@external(erlang, "io", "format")
fn erl_io_format(fmt: String, args: List(String)) -> Nil

fn clear_line() -> Nil {
  let assert Ok(esc) = string.utf_codepoint(0x1b)
  io.print("\r" <> string.from_utf_codepoints([esc]) <> "[K")
}

fn print_progress(
  current: Int,
  total: Int,
  passes: Int,
  fails: Int,
  skips: Int,
) -> Nil {
  erl_io_format("  \r  [~s/~s] ~s pass, ~s fail, ~s skip", [
    int.to_string(current),
    int.to_string(total),
    int.to_string(passes),
    int.to_string(fails),
    int.to_string(skips),
  ])
}
