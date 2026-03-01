/// test262 execution conformance runner (snapshot mode).
///
/// Each test262 file is an individual EUnit test case.
/// Results are compared against a snapshot of expected outcomes.
///
/// Usage:
///   TEST262_EXEC=1 gleam test                  — run and compare against snapshot
///   TEST262_EXEC=1 UPDATE_SNAPSHOT=1 gleam test — run and update the snapshot
///   TEST262_EXEC=1 FAIL_LOG=path gleam test     — also write per-test failure reasons
///   TEST262_EXEC=1 RESULTS_FILE=path gleam test — also write JSON results
///
/// Snapshot semantics:
///   - Regressions (was pass, now fail) → eunit FAILURE
///   - New passes  (was fail, now pass) → eunit FAILURE (update snapshot)
///   - Expected outcomes               → eunit pass (silent)
///   - No snapshot file                → all outcomes accepted (no comparison)
import arc/compiler
import arc/module
import arc/parser
import arc/vm/builtins
import arc/vm/builtins/common
import arc/vm/heap.{type Heap}
import arc/vm/object
import arc/vm/value
import arc/vm/vm
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string
import simplifile
import test262_metadata.{type TestMetadata, Parse, Resolution, Runtime}
import test_runner

const test_dir: String = "vendor/test262/test"

const harness_dir: String = "vendor/test262/harness"

const snapshot_path: String = ".github/test262/pass.txt"

pub fn test262_exec_test_() -> test_runner.EunitTests {
  case test_runner.get_env_is_truthy("TEST262_EXEC") {
    False -> test_runner.empty_tests()
    True -> generate_test_suite()
  }
}

type TestOutcome {
  Pass
  Fail(reason: String)
  Skip(reason: String)
}

fn generate_test_suite() -> test_runner.EunitTests {
  let fail_log = test_runner.get_env("FAIL_LOG") |> option.from_result
  let update_mode = test_runner.get_env_is_truthy("UPDATE_SNAPSHOT")
  let snapshot = load_snapshot(snapshot_path)
  let has_snapshot = set.size(snapshot) > 0

  // Clear fail log if set
  case fail_log {
    Some(path) ->
      case simplifile.write(to: path, contents: "") {
        Ok(Nil) -> Nil
        Error(err) ->
          io.println(
            "Warning: could not clear fail log: " <> string.inspect(err),
          )
      }
    None -> Nil
  }

  init_stats()

  // Single test function — receives relative path, does all I/O at test time
  let test_fn = fn(relative: String) -> Result(Nil, String) {
    let full_path = test_dir <> "/" <> relative
    case simplifile.read(full_path) {
      Error(err) -> {
        record_fail()
        Error("could not read file: " <> string.inspect(err))
      }
      Ok(source) -> {
        let metadata = test262_metadata.parse_metadata(source)

        let should_skip =
          list.contains(metadata.flags, "async")
          || metadata.negative_phase == Some(Resolution)
          || list.contains(metadata.features, "top-level-await")

        case should_skip {
          True -> {
            record_skip()
            Ok(Nil)
          }
          False -> {
            let outcome = run_test_by_phase(metadata, source, full_path)
            let expected_pass = set.contains(snapshot, relative)

            case outcome {
              Pass -> {
                record_pass()
                record_pass_path(relative)
                case update_mode || !has_snapshot || expected_pass {
                  True -> Ok(Nil)
                  False ->
                    Error(
                      "NEW PASS — run with UPDATE_SNAPSHOT=1 to update snapshot",
                    )
                }
              }
              Skip(_) -> {
                record_skip()
                Ok(Nil)
              }
              Fail(reason) -> {
                record_fail()
                case fail_log {
                  Some(path) ->
                    case
                      simplifile.append(
                        to: path,
                        contents: relative <> "\t" <> reason <> "\n",
                      )
                    {
                      Ok(Nil) -> Nil
                      Error(err) ->
                        io.println(
                          "Warning: fail log append error: "
                          <> string.inspect(err),
                        )
                    }
                  None -> Nil
                }
                case update_mode || !has_snapshot || !expected_pass {
                  True -> Ok(Nil)
                  False -> Error("REGRESSION: " <> reason)
                }
              }
            }
          }
        }
      }
    }
  }

  // List all files upfront (fast — just filenames, no reading) and run in parallel
  let tests =
    test_runner.make_test_suite(
      list_test_files(test_dir)
        |> list.map(fn(relative) { #(relative, fn() { test_fn(relative) }) }),
      10,
    )

  let summary_test = #("__summary__", fn() {
    let #(pass_count, fail_count, skip_count) = get_stats()
    let tested = pass_count + fail_count
    let pct = format_percent(pass_count, tested)

    io.println(
      "\ntest262 exec: "
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

    case fail_log {
      Some(path) -> io.println("Failures written to " <> path)
      None -> Nil
    }

    // Write snapshot if UPDATE_SNAPSHOT=1
    case update_mode {
      True -> {
        let paths = get_pass_paths()
        let content = string.join(paths, "\n") <> "\n"
        case simplifile.write(to: snapshot_path, contents: content) {
          Ok(Nil) ->
            io.println(
              "Snapshot updated: "
              <> snapshot_path
              <> " ("
              <> int.to_string(list.length(paths))
              <> " passing tests)",
            )
          Error(err) ->
            io.println(
              "Warning: could not write snapshot: " <> string.inspect(err),
            )
        }
      }
      False -> Nil
    }

    // Write RESULTS_FILE if set
    case test_runner.get_env("RESULTS_FILE") {
      Ok(path) -> {
        let total = pass_count + fail_count + skip_count
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
        case simplifile.write(to: path, contents: json) {
          Ok(Nil) -> io.println("Results written to " <> path)
          Error(err) ->
            io.println(
              "Warning: could not write results: " <> string.inspect(err),
            )
        }
      }
      Error(Nil) -> Nil
    }

    Ok(Nil)
  })

  test_runner.with_cleanup(tests, summary_test, 60)
}

@external(erlang, "test262_exec_ffi", "list_test_files")
fn list_test_files(dir: String) -> List(String)

fn load_snapshot(path: String) -> Set(String) {
  case simplifile.read(path) {
    Ok(content) ->
      content
      |> string.split("\n")
      |> list.filter(fn(line) { line != "" })
      |> set.from_list
    Error(_) -> set.new()
  }
}

fn format_percent(pass: Int, tested: Int) -> String {
  case tested > 0 {
    True -> {
      let pct_x100 = { pass * 10_000 } / tested
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
}

// --- Test execution ---

fn run_test_by_phase(
  metadata: TestMetadata,
  source: String,
  path: String,
) -> TestOutcome {
  let source = case list.contains(metadata.flags, "onlyStrict") {
    True -> "\"use strict\";\n" <> source
    False -> source
  }

  let is_module = list.contains(metadata.flags, "module")
  case metadata.negative_phase {
    Some(Parse) -> run_parse_negative_test(metadata, source)
    Some(Resolution) -> Skip("resolution")
    Some(Runtime) ->
      run_runtime_negative_test(metadata, source, is_module, path)
    None -> run_positive_test(metadata, source, is_module, path)
  }
}

fn run_parse_negative_test(
  metadata: TestMetadata,
  source: String,
) -> TestOutcome {
  let mode = case list.contains(metadata.flags, "module") {
    True -> parser.Module
    False -> parser.Script
  }
  case parser.parse(source, mode) {
    Error(_) -> Pass
    Ok(_) -> Fail("expected parse error but parsed successfully")
  }
}

fn run_runtime_negative_test(
  metadata: TestMetadata,
  source: String,
  is_module: Bool,
  path: String,
) -> TestOutcome {
  let result = case
    test_runner.run_with_timeout(
      fn() {
        case is_module {
          True -> do_run_module(metadata, source, path)
          False -> do_run_script(prepend_harness(metadata, source))
        }
      },
      test_timeout_ms,
    )
  {
    Ok(r) -> r
    Error(reason) -> Error(reason)
  }
  case result {
    Ok(vm.ThrowCompletion(_, _)) -> Pass
    Ok(vm.NormalCompletion(_, _)) ->
      Fail("expected runtime throw but completed normally")
    Ok(vm.YieldCompletion(_, _)) -> Fail("unexpected YieldCompletion")
    Error(reason) -> Fail("expected runtime throw but got: " <> reason)
  }
}

fn run_positive_test(
  metadata: TestMetadata,
  source: String,
  is_module: Bool,
  path: String,
) -> TestOutcome {
  let result = case
    test_runner.run_with_timeout(
      fn() {
        case is_module {
          True -> do_run_module(metadata, source, path)
          False -> do_run_script(prepend_harness(metadata, source))
        }
      },
      test_timeout_ms,
    )
  {
    Ok(r) -> r
    Error(_) -> Error("timeout")
  }
  case result {
    Ok(vm.NormalCompletion(_, _)) -> Pass
    Ok(vm.ThrowCompletion(thrown, heap)) ->
      Fail("unexpected throw: " <> inspect_thrown(thrown, heap))
    Ok(vm.YieldCompletion(_, _)) -> Fail("unexpected YieldCompletion")
    Error(reason) -> Fail(reason)
  }
}

const test_timeout_ms: Int = 30_000

fn do_run_module(
  metadata: TestMetadata,
  source: String,
  path: String,
) -> Result(vm.Completion, String) {
  let h = heap.new()
  let #(h, b) = builtins.init(h)
  let #(h, globals) = builtins.globals(b, h)

  // Evaluate harness files as REPL scripts to populate globals
  use #(h, env) <- result.try(eval_harness(metadata, h, b, globals))
  let globals = env.globals

  case module.compile_bundle(path, source, test262_resolve_and_load) {
    Error(err) -> Error("module: " <> string.inspect(err))
    Ok(bundle) ->
      case module.evaluate_bundle(bundle, h, b, globals) {
        Ok(#(val, new_heap)) -> Ok(vm.NormalCompletion(val, new_heap))
        Error(module.EvaluationError(val)) -> Ok(vm.ThrowCompletion(val, h))
        Error(err) -> Error("module: " <> string.inspect(err))
      }
  }
}

/// Resolve and load a dependency module for test262 tests.
/// Resolves relative paths against the parent module's directory.
fn test262_resolve_and_load(
  raw_specifier: String,
  parent_specifier: String,
) -> Result(#(String, String), String) {
  let resolved = resolve_test262_specifier(raw_specifier, parent_specifier)
  case simplifile.read(resolved) {
    Ok(source) -> Ok(#(resolved, source))
    Error(err) ->
      Error(
        "file not found: " <> resolved <> " (" <> string.inspect(err) <> ")",
      )
  }
}

/// Resolve a module specifier relative to the parent module's path.
fn resolve_test262_specifier(raw: String, parent: String) -> String {
  case string.starts_with(raw, "./"), string.starts_with(raw, "../") {
    True, _ | _, True -> {
      let parent_dir = test262_dirname(parent)
      normalize_test262_path(parent_dir <> "/" <> raw)
    }
    _, _ -> raw
  }
}

fn test262_dirname(path: String) -> String {
  let parts = string.split(path, "/")
  case list.reverse(parts) {
    [_, ..rest] ->
      case list.reverse(rest) {
        [] -> "."
        dir_parts -> string.join(dir_parts, "/")
      }
    [] -> "."
  }
}

fn normalize_test262_path(path: String) -> String {
  let parts = string.split(path, "/")
  let resolved =
    list.fold(parts, [], fn(acc, part) {
      case part {
        "." -> acc
        ".." ->
          case acc {
            [_, ..rest] -> rest
            [] -> [".."]
          }
        "" ->
          case acc {
            [] -> [""]
            _ -> acc
          }
        _ -> [part, ..acc]
      }
    })
  list.reverse(resolved) |> string.join("/")
}

fn do_run_script(source: String) -> Result(vm.Completion, String) {
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
          let #(h, globals) = builtins.globals(b, h)
          case vm.run_and_drain(template, h, b, globals) {
            Ok(completion) -> Ok(completion)
            Error(vm_err) -> Error("vm: " <> string.inspect(vm_err))
          }
        }
      }
  }
}

/// Evaluate harness files as REPL scripts to populate globals.
/// This is the spec-correct approach: harness is evaluated in the realm
/// before the test module runs, making harness functions (assert, etc.)
/// available as globals.
fn eval_harness(
  metadata: TestMetadata,
  h: Heap,
  b: common.Builtins,
  globals: dict.Dict(String, value.JsValue),
) -> Result(#(Heap, vm.ReplEnv), String) {
  let is_raw = list.contains(metadata.flags, "raw")
  case is_raw {
    True -> {
      let env =
        vm.ReplEnv(
          globals:,
          const_globals: set.new(),
          symbol_descriptions: dict.new(),
        )
      Ok(#(h, env))
    }
    False -> {
      let default_harness = ["assert.js", "sta.js"]
      let extra_includes =
        metadata.includes
        |> list.filter(fn(f) { !list.contains(default_harness, f) })
      let harness_files = list.append(default_harness, extra_includes)

      let env =
        vm.ReplEnv(
          globals:,
          const_globals: set.new(),
          symbol_descriptions: dict.new(),
        )

      list.try_fold(harness_files, #(h, env), fn(acc, filename) {
        let #(heap, env) = acc
        let path = harness_dir <> "/" <> filename
        case simplifile.read(path) {
          Error(err) -> Error("harness read: " <> string.inspect(err))
          Ok(source) -> eval_harness_script(source, heap, b, env)
        }
      })
    }
  }
}

/// Evaluate a single harness file as a REPL script.
fn eval_harness_script(
  source: String,
  h: Heap,
  b: common.Builtins,
  env: vm.ReplEnv,
) -> Result(#(Heap, vm.ReplEnv), String) {
  case parser.parse(source, parser.Script) {
    Error(err) -> Error("harness parse: " <> parser.parse_error_to_string(err))
    Ok(program) ->
      case compiler.compile_repl(program) {
        Error(err) -> Error("harness compile: " <> string.inspect(err))
        Ok(template) ->
          case vm.run_and_drain_repl(template, h, b, env) {
            Error(vm_err) -> Error("harness vm: " <> string.inspect(vm_err))
            Ok(#(completion, new_env)) ->
              case completion {
                vm.NormalCompletion(_, new_heap) -> Ok(#(new_heap, new_env))
                vm.ThrowCompletion(thrown, new_heap) ->
                  Error("harness threw: " <> inspect_thrown(thrown, new_heap))
                vm.YieldCompletion(_, _) -> Error("harness yielded")
              }
          }
      }
  }
}

fn prepend_harness(metadata: TestMetadata, source: String) -> String {
  let is_raw = list.contains(metadata.flags, "raw")

  case is_raw {
    True -> source
    False -> {
      let default_harness = ["assert.js", "sta.js"]
      let extra_includes =
        metadata.includes
        |> list.filter(fn(f) { !list.contains(default_harness, f) })
      let harness_files = list.append(default_harness, extra_includes)

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

fn get_data(h: Heap, ref: value.Ref, key: String) -> Result(value.JsValue, Nil) {
  case object.get_own_property(h, ref, key) {
    Some(value.DataProperty(value: val, ..)) -> Ok(val)
    Some(_) -> Error(Nil)
    None ->
      case heap.read(h, ref) {
        Some(value.ObjectSlot(prototype: Some(proto_ref), ..)) ->
          get_data(h, proto_ref, key)
        _ -> Error(Nil)
      }
  }
}

fn inspect_thrown(val: value.JsValue, heap: Heap) -> String {
  case val {
    value.JsObject(ref) -> {
      case get_data(heap, ref, "message") {
        Ok(value.JsString(msg)) -> {
          let name = case get_data(heap, ref, "name") {
            Ok(value.JsString(n)) -> n
            _ -> "Error"
          }
          name <> ": " <> msg
        }
        _ -> object.inspect(val, heap)
      }
    }
    _ -> object.inspect(val, heap)
  }
}

// -- FFI --

@external(erlang, "test262_exec_ffi", "init_stats")
fn init_stats() -> Nil

@external(erlang, "test262_exec_ffi", "record_pass")
fn record_pass() -> Nil

@external(erlang, "test262_exec_ffi", "record_fail")
fn record_fail() -> Nil

@external(erlang, "test262_exec_ffi", "record_skip")
fn record_skip() -> Nil

@external(erlang, "test262_exec_ffi", "get_stats")
fn get_stats() -> #(Int, Int, Int)

@external(erlang, "test262_exec_ffi", "record_pass_path")
fn record_pass_path(path: String) -> Nil

@external(erlang, "test262_exec_ffi", "get_pass_paths")
fn get_pass_paths() -> List(String)
