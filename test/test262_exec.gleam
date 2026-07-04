/// test262 execution conformance runner (snapshot mode).
///
/// Tests are registered with the main harness as individual entries.
/// The harness calls init() before spawning, run_file() per test,
/// and finish() after all complete.
///
/// Usage:
///   TEST262_EXEC=1 gleam test                  — run and compare against snapshot
///   TEST262_EXEC=1 UPDATE_SNAPSHOT=1 gleam test — run and update the snapshot
///   TEST262_EXEC=1 FAIL_LOG=path gleam test     — also write per-test failure reasons
///   TEST262_EXEC=1 RESULTS_FILE=path gleam test — also write JSON results
///   TEST262_FILTER=path/prefix                  — only run matching test files
///   TEST262_SHARD=k/n                           — only run bucket k of an n-way
///                                                 hash partition (CI parallelism)
///
/// With TEST262_FILTER or TEST262_SHARD set, UPDATE_SNAPSHOT rewrites pass.txt
/// to just that subset — fine for CI shards (the merge job reassembles them),
/// wrong to commit from a local partial run.
import arc/compiler
import arc/host
import arc/internal/path
import arc/module
import arc/module/load_error
import arc/module_host
import arc/parser
import arc/vm/builtins
import arc/vm/builtins/atomics as builtins_atomics
import arc/vm/builtins/common
import arc/vm/completion.{ThrowCompletion}
import arc/vm/exec/entry
import arc/vm/exec/event_loop
import arc/vm/exec/interpreter
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/key.{Named}
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/realm
import arc/vm/state.{type Heap, type State, RealmCtx, State}
import arc/vm/value
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set
import gleam/string
import simplifile
import test262_metadata.{type TestMetadata, Parse, Resolution, Runtime}
import test_runner

/// A settled top-level run: `Ok(value)` for a normal completion,
/// `Error(thrown)` for an uncaught throw, paired with the drained heap.
type Settled(host) =
  #(Result(value.JsValue, value.JsValue), Heap(host))

const test_dir: String = "vendor/test262/test"

/// JS preamble: defines `print` which captures output for async test protocol.
/// $262 is installed natively via entry.build_262 instead.
const print_preamble: String = "var __print_output__; function print(x) { __print_output__ = '' + x; }"

const harness_dir: String = "vendor/test262/harness"

const snapshot_path: String = ".github/test262/pass.txt"

/// Initialize ETS tables and config. Called once before tests start.
pub fn init() -> Nil {
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
  init_config(update_mode, has_snapshot, fail_log)
  init_snapshot_set(snapshot |> set.to_list)
  warm_caches()
}

// --- Cross-test caches ---
//
// The booted base realm (heap + builtins + global object) and compiled
// harness templates are immutable Gleam data, so they can be computed once
// and shared across all per-test worker processes via persistent_term
// (zero-copy reads). Each test starts from the shared snapshot and forks its
// own heap on mutation, so tests stay fully isolated.

const realm_cache_key = "base_realm"

/// Warm the realm and common-harness caches from the main process before
/// workers spawn, so parallel first uses don't race on cache_put.
fn warm_caches() -> Nil {
  let _ = boot_base_realm()
  list.each(["assert.js", "sta.js", "doneprintHandle.js"], fn(filename) {
    case harness_template(filename, fn() { read_harness_file(filename) }) {
      Ok(_) -> Nil
      Error(err) -> io.println("Warning: harness cache warm failed: " <> err)
    }
  })
}

/// Boot (or fetch the cached) base realm: heap + builtins + global object.
fn boot_base_realm() -> #(Heap(host), common.Builtins, value.Ref) {
  case realm_cache_get(realm_cache_key) {
    Some(snapshot) -> snapshot
    None -> {
      let h = heap.new()
      let #(h, b) = builtins.init(h)
      let #(h, global_object) = builtins.globals(b, h)
      let snapshot = #(h, b, global_object)
      realm_cache_put(realm_cache_key, snapshot)
      snapshot
    }
  }
}

/// Fetch the cached compiled template for a harness script, parsing and
/// compiling (then caching) on first use. `read_source` is only called on a
/// cache miss. Failed parses/compiles are not cached; every test that needs
/// the file reports the same error.
fn harness_template(
  key: String,
  read_source: fn() -> Result(String, String),
) -> Result(value.FuncTemplate, String) {
  case template_cache_get(key) {
    Some(template) -> Ok(template)
    None -> {
      use source <- result.try(read_source())
      use template <- result.map(compile_harness_source(source))
      template_cache_put(key, template)
      template
    }
  }
}

fn compile_harness_source(
  source: String,
) -> Result(value.FuncTemplate, String) {
  use #(body, sb) <- result.try(
    parser.parse_script(source)
    |> result.map_error(fn(err) {
      "harness parse: " <> parser.parse_error_to_string(err)
    }),
  )
  compiler.compile_repl(body, sb)
  |> result.map_error(fn(err) { "harness compile: " <> string.inspect(err) })
}

fn read_harness_file(filename: String) -> Result(String, String) {
  simplifile.read(harness_dir <> "/" <> filename)
  |> result.map_error(fn(err) {
    "harness read " <> filename <> ": " <> string.inspect(err)
  })
}

/// List all test262 .js files (relative paths).
pub fn list_files() -> List(String) {
  list_test_files(test_dir)
}

/// Run a single test262 file. Called per-test by the harness.
/// Returns Ok(Nil) for expected outcomes, Error for regressions/new passes.
pub fn run_file(relative: String) -> Result(Nil, String) {
  let update_mode = get_update_mode()
  let has_snapshot = get_has_snapshot()
  let fail_log = get_fail_log()
  let full_path = test_dir <> "/" <> relative
  case simplifile.read(full_path) {
    Error(err) -> {
      record_fail()
      Error("could not read file: " <> string.inspect(err))
    }
    Ok(source) -> {
      let metadata = test262_metadata.parse_metadata(source)
      let outcome = run_test_by_phase(metadata, source, full_path)
      let expected_pass = snapshot_contains(relative)

      case outcome {
        Pass -> {
          record_pass()
          record_pass_path(relative)
          case update_mode || !has_snapshot || expected_pass {
            True -> Ok(Nil)
            False ->
              Error("NEW PASS — run with UPDATE_SNAPSHOT=1 to update snapshot")
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
                    "Warning: fail log append error: " <> string.inspect(err),
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

/// Print summary and write snapshot. Called once after all tests complete.
/// Returns Error if there are regressions.
pub fn finish(errors: List(#(String, String))) -> Result(Nil, String) {
  let update_mode = get_update_mode()
  let fail_log = get_fail_log()

  // Print summary
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
        Ok(Nil) -> {
          io.println(
            "Snapshot updated: "
            <> snapshot_path
            <> " ("
            <> int.to_string(list.length(paths))
            <> " passing tests)",
          )
          case partial_run_env() {
            Some(name) ->
              io.println(
                "Warning: "
                <> name
                <> " is set, so "
                <> snapshot_path
                <> " now covers only that subset — do not commit it",
              )
            None -> Nil
          }
        }
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

  // Report regressions as test failure
  case errors {
    [] -> Ok(Nil)
    _ -> {
      let count = list.length(errors)
      Error(
        int.to_string(count)
        <> " regression(s) — run with UPDATE_SNAPSHOT=1 to update",
      )
    }
  }
}

type TestOutcome {
  Pass
  Fail(reason: String)
  Skip(reason: String)
}

type StrictnessVariant {
  NonStrict
  Strict
}

fn variants_for_test(metadata: TestMetadata) -> List(StrictnessVariant) {
  let is_module = list.contains(metadata.flags, "module")
  let is_raw = list.contains(metadata.flags, "raw")
  let is_only_strict = list.contains(metadata.flags, "onlyStrict")
  let is_no_strict = list.contains(metadata.flags, "noStrict")
  case is_only_strict {
    True -> [Strict]
    False ->
      case is_no_strict || is_raw || is_module {
        True -> [NonStrict]
        False -> [NonStrict, Strict]
      }
  }
}

@external(erlang, "test262_exec_ffi", "list_test_files")
fn list_test_files(_dir: String) -> List(String) {
  panic as "test262 suite is BEAM-only"
}

fn load_snapshot(path: String) -> set.Set(String) {
  case simplifile.read(path) {
    Ok(content) ->
      content
      |> string.split("\n")
      |> list.filter(fn(line) { line != "" })
      |> set.from_list
    Error(_) -> set.new()
  }
}

/// The env var, if any, that restricts this run to a subset of test262 — a
/// snapshot written from such a run must not be committed as the full baseline.
fn partial_run_env() -> Option(String) {
  ["TEST262_SHARD", "TEST262_FILTER"]
  |> list.find(fn(name) {
    case test_runner.get_env(name) {
      Ok("") -> False
      Ok(_) -> True
      Error(Nil) -> False
    }
  })
  |> option.from_result
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
  let variants = variants_for_test(metadata)
  let is_module = list.contains(metadata.flags, "module")
  let is_async = list.contains(metadata.flags, "async")

  // Run all variants; a test passes only if ALL variants pass
  list.fold_until(variants, Pass, fn(_acc, variant) {
    let outcome = case metadata.negative_phase {
      Some(Parse) -> run_parse_negative_test(metadata, source, variant)
      Some(Resolution) ->
        run_runtime_negative_test(
          metadata,
          source,
          is_module,
          path,
          variant,
          is_async,
        )
      Some(Runtime) ->
        run_runtime_negative_test(
          metadata,
          source,
          is_module,
          path,
          variant,
          is_async,
        )
      None ->
        run_positive_test(metadata, source, is_module, path, variant, is_async)
    }
    case outcome {
      Pass -> list.Continue(Pass)
      Skip(reason) -> list.Stop(Skip(reason))
      Fail(reason) -> {
        let variant_label = case variant {
          Strict -> " (strict)"
          NonStrict -> " (non-strict)"
        }
        list.Stop(Fail(reason <> variant_label))
      }
    }
  })
}

fn run_parse_negative_test(
  metadata: TestMetadata,
  source: String,
  variant: StrictnessVariant,
) -> TestOutcome {
  let mode = case list.contains(metadata.flags, "module") {
    True -> parser.Module
    False -> parser.Script
  }
  let test_source = case variant {
    Strict -> "\"use strict\";\n" <> source
    NonStrict -> source
  }
  case parser.parse(test_source, mode) {
    Error(_) -> Pass
    Ok(_) -> Fail("expected parse error but parsed successfully")
  }
}

/// Shared scaffold for running a test to completion: handles the
/// module/script branch, timeout, and async dispatch. Callers supply how to
/// map run errors, completions, and async completions to outcomes.
fn run_test_completion(
  metadata: TestMetadata,
  source: String,
  is_module: Bool,
  path: String,
  variant: StrictnessVariant,
  is_async: Bool,
  on_error: fn(String) -> TestOutcome,
  completion_outcome: fn(Settled(host)) -> TestOutcome,
  async_outcome: fn(Settled(host), value.Ref) -> TestOutcome,
) -> TestOutcome {
  // test262 CanBlockIsFalse flag: the test must run in an agent whose
  // [[CanBlock]] is false (sync Atomics.wait throws a TypeError). The flag
  // is process-local and read at realm boot, so it must be set inside the
  // worker closure (run_with_timeout executes it in a spawned process).
  // Set unconditionally: True for every other test.
  // install_agent_hook is process-local for the same reason: it registers
  // the $262 extension that puts the harness's host-side `agent` object on
  // every $262 the worker's realms build (initial + createRealm children).
  let can_block = !list.contains(metadata.flags, "CanBlockIsFalse")
  case is_module {
    True ->
      case
        test_runner.run_with_timeout(
          fn() {
            let Nil = builtins_atomics.set_can_block(can_block)
            let Nil = install_agent_hook()
            do_run_module(metadata, source, path, is_async)
          },
          test_timeout_ms,
        )
        |> result.flatten
      {
        Ok(#(completion, global_ref)) ->
          case is_async {
            False -> completion_outcome(completion)
            True -> async_outcome(completion, global_ref)
          }
        Error(reason) -> on_error(reason)
      }
    False ->
      case
        test_runner.run_with_timeout(
          fn() {
            let Nil = builtins_atomics.set_can_block(can_block)
            let Nil = install_agent_hook()
            do_run_script_with_harness(
              metadata,
              source,
              path,
              variant,
              is_async,
            )
          },
          test_timeout_ms,
        )
        |> result.flatten
      {
        Error(reason) -> on_error(reason)
        Ok(#(completion, global_ref)) ->
          case is_async {
            False -> completion_outcome(completion)
            True -> async_outcome(completion, global_ref)
          }
      }
  }
}

fn run_runtime_negative_test(
  metadata: TestMetadata,
  source: String,
  is_module: Bool,
  path: String,
  variant: StrictnessVariant,
  is_async: Bool,
) -> TestOutcome {
  run_test_completion(
    metadata,
    source,
    is_module,
    path,
    variant,
    is_async,
    fn(reason) { Fail("expected runtime throw but got: " <> reason) },
    negative_completion_outcome(metadata, _),
    fn(completion, global_ref) {
      // For async negative tests, $DONE reports via print
      case check_async_completion(completion, global_ref) {
        Ok(Nil) ->
          // Test completed successfully — but we expected a throw
          Fail("expected runtime throw but async test completed")
        Error(msg) ->
          // Async test reported failure — check if it's the right error
          case
            string.contains(msg, metadata.negative_type |> option.unwrap(""))
          {
            True -> Pass
            False -> Fail("wrong async error: " <> msg)
          }
      }
    },
  )
}

/// Map a settled run to the outcome for a runtime-negative test:
/// only a throw (of the expected error type) passes.
fn negative_completion_outcome(
  metadata: TestMetadata,
  settled: Settled(host),
) -> TestOutcome {
  case settled {
    #(Error(thrown), heap) -> verify_negative_type(metadata, thrown, heap)
    #(Ok(_), _) -> Fail("expected runtime throw but completed normally")
  }
}

/// Map a settled run to the outcome for a positive test:
/// only a normal completion passes.
fn positive_completion_outcome(settled: Settled(host)) -> TestOutcome {
  case settled {
    #(Ok(_), _) -> Pass
    #(Error(thrown), heap) ->
      Fail("unexpected throw: " <> inspect_thrown(thrown, heap))
  }
}

fn run_positive_test(
  metadata: TestMetadata,
  source: String,
  is_module: Bool,
  path: String,
  variant: StrictnessVariant,
  is_async: Bool,
) -> TestOutcome {
  run_test_completion(
    metadata,
    source,
    is_module,
    path,
    variant,
    is_async,
    Fail,
    positive_completion_outcome,
    check_async_positive,
  )
}

/// Check async test completion for positive tests.
/// Reads __print_output__ from the global object to determine pass/fail.
fn check_async_positive(
  settled: Settled(host),
  global_ref: value.Ref,
) -> TestOutcome {
  case check_async_completion(settled, global_ref) {
    Ok(Nil) -> Pass
    Error(reason) -> Fail(reason)
  }
}

/// Core async completion check. Returns Ok(Nil) for "Test262:AsyncTestComplete",
/// Error with reason for everything else.
fn check_async_completion(
  settled: Settled(host),
  global_ref: value.Ref,
) -> Result(Nil, String) {
  case settled {
    #(Error(thrown), heap) ->
      Error("unexpected throw: " <> inspect_thrown(thrown, heap))
    #(Ok(_), heap) -> {
      case get_data(heap, global_ref, "__print_output__") {
        Ok(value.JsString(output)) ->
          case output {
            "Test262:AsyncTestComplete" -> Ok(Nil)
            _ ->
              case string.starts_with(output, "Test262:AsyncTestFailure:") {
                True -> {
                  let msg =
                    string.drop_start(
                      output,
                      string.length("Test262:AsyncTestFailure:"),
                    )
                  Error("async failure: " <> msg)
                }
                False -> Error("unexpected print output: " <> output)
              }
          }
        Ok(value.JsUndefined) -> Error("async test did not call $DONE")
        Ok(other) ->
          Error("unexpected __print_output__: " <> string.inspect(other))
        Error(Nil) ->
          Error("async test did not call $DONE (no __print_output__)")
      }
    }
  }
}

fn verify_negative_type(
  metadata: TestMetadata,
  thrown: value.JsValue,
  heap: Heap(host),
) -> TestOutcome {
  case metadata.negative_type {
    None -> Pass
    Some(expected_type) -> {
      let actual_name = case thrown {
        value.JsObject(ref) ->
          case get_data(heap, ref, "name") {
            Ok(value.JsString(n)) -> Ok(n)
            _ -> Error(Nil)
          }
        _ -> Error(Nil)
      }
      case actual_name {
        Ok(name) if name == expected_type -> Pass
        Ok(name) ->
          Fail(
            "expected "
            <> expected_type
            <> " but got "
            <> name
            <> ": "
            <> inspect_thrown(thrown, heap),
          )
        Error(Nil) -> Pass
      }
    }
  }
}

const test_timeout_ms: Int = 120_000

fn do_run_module(
  metadata: TestMetadata,
  source: String,
  path: String,
  is_async: Bool,
) -> Result(#(Settled(host), value.Ref), String) {
  let #(h, b, global_object) = boot_base_realm()

  // Evaluate harness files as REPL scripts to populate globals. Async module
  // tests use the same $DONE/print protocol as scripts (doneprintHandle.js).
  use #(h, env, test_hooks) <- result.try(eval_harness(
    metadata,
    h,
    b,
    global_object,
    path,
    is_async,
  ))
  let global_object = env.global_object

  case module.compile_bundle(path, source, test262_resolve, test262_load) {
    Error(err) -> Error("module: " <> string.inspect(err))
    Ok(bundle) -> {
      // Evaluate through the realm-wide module registry so a dynamic
      // import() of any module in this static graph (including the test file
      // itself) resolves to the same module record instead of re-evaluating
      // it (§16.2.1.8).
      // Top-level driver is the notify-consuming embedder loop
      // (settle_pending_wakes — drains microtasks, then consumes
      // cross-process arc_notify wakes bounded by the earliest pending
      // deadline), so leftover jobs are always empty here.
      let #(new_heap, _jobs, result) =
        module_host.evaluate_bundle_with_registry(
          h,
          b,
          global_object,
          bundle,
          test_hooks,
          settle_pending_wakes,
        )
      case result {
        Ok(module.EvaluatedBundle(value: val, ..)) ->
          Ok(#(#(Ok(val), new_heap), global_object))
        Error(module.EvaluationError(value: val, heap: _)) ->
          Ok(#(#(Error(val), new_heap), global_object))
        // Entry module still parked on top-level await after a full drain:
        // an awaited promise can never settle. Same outcome as the
        // pre-EvaluationPending behavior (a host-level throw).
        Error(module.EvaluationPending(promise_data_ref: _, heap: _)) ->
          Ok(#(
            #(
              Error(value.JsString(
                "module evaluation never completed: top-level await promise never settled",
              )),
              new_heap,
            ),
            global_object,
          ))
        Error(err) -> Error("module: " <> string.inspect(err))
      }
    }
  }
}

/// Resolve a test262 dependency specifier relative to its parent's directory.
/// The runner is a filesystem loader: a bare specifier is not a path.
fn test262_resolve(
  raw_specifier: String,
  parent_specifier: String,
) -> Result(String, module_host.ModuleLoadError) {
  case path.resolve_specifier(raw_specifier, parent_specifier) {
    path.PathSpecifier(resolved) -> Ok(resolved)
    path.BareSpecifier(bare) -> Error(load_error.UnsupportedBareSpecifier(bare))
  }
}

/// Read a resolved test262 module from disk.
fn test262_load(
  resolved: String,
) -> Result(String, module_host.ModuleLoadError) {
  case simplifile.read(resolved) {
    Ok(source) -> Ok(source)
    Error(simplifile.Enoent) -> Error(load_error.NotFound(resolved))
    Error(err) ->
      Error(load_error.ReadFailed(resolved, simplifile.describe_error(err)))
  }
}

fn do_run_script_with_harness(
  metadata: TestMetadata,
  source: String,
  path: String,
  variant: StrictnessVariant,
  is_async: Bool,
) -> Result(#(Settled(host), value.Ref), String) {
  let #(h, b, global_object) = boot_base_realm()

  // Evaluate harness files as REPL scripts to populate globals
  use #(h, env, test_hooks) <- result.try(eval_harness(
    metadata,
    h,
    b,
    global_object,
    path,
    is_async,
  ))

  // Prepend "use strict" to test source only (not harness) when strict
  let test_source = case variant {
    Strict -> "\"use strict\";\n" <> source
    NonStrict -> source
  }

  case parser.parse_script(test_source) {
    Error(err) -> Error("parse: " <> parser.parse_error_to_string(err))
    Ok(#(body, sb)) ->
      case compiler.compile_repl(body, sb) {
        Error(err) -> Error("compile: " <> string.inspect(err))
        Ok(template) ->
          // The test source runs with the harness's host capabilities
          // supplied at State construction (sync Atomics.wait blocking /
          // notify wake delivery — contract in arc/host.gleam) and the
          // notify-consuming embedder loop as its post-script driver, so
          // cross-agent waitAsync wakes landing in this worker's mailbox
          // settle before their deadline.
          case
            entry.run_and_drain_repl_with(
              template,
              h,
              b,
              env,
              test_hooks,
              settle_pending_wakes,
            )
          {
            Error(vm_err) -> Error("vm: " <> string.inspect(vm_err))
            Ok(#(settled, final_heap, final_env)) ->
              Ok(#(#(settled, final_heap), final_env.global_object))
          }
      }
  }
}

/// Evaluate harness files as REPL scripts to populate globals.
/// This is the spec-correct approach: harness is evaluated in the realm
/// before the test module runs, making harness functions (assert, etc.)
/// available as globals.
///
/// Also hands back the `HostHooks` the TEST source must be booted with:
/// the harness's Atomics capabilities plus the dynamic-import host hook —
/// engine state, not a globalThis property, so it has to be threaded into
/// the State that runs the test rather than installed on the realm's global.
fn eval_harness(
  metadata: TestMetadata,
  h: Heap(host),
  b: common.Builtins,
  global_object: value.Ref,
  path: String,
  is_async: Bool,
) -> Result(#(Heap(host), entry.ReplEnv, host.HostHooks), String) {
  let is_raw = list.contains(metadata.flags, "raw")
  case is_raw {
    True -> {
      // Raw tests get no harness and no import hook — import() rejects.
      Ok(#(h, entry.new_repl_env(global_object), harness_host_hooks()))
    }
    False -> {
      // Build the dynamic-import host hook: import() resolves specifiers
      // relative to the test file and loads fixtures from disk. It rides on
      // `HostHooks.import_hook`, never on globalThis.
      let #(h, import_hook) =
        module_host.install_import_hook(
          h,
          b,
          path,
          test262_resolve,
          test262_load,
        )
      let test_hooks =
        state.HostHooks(..harness_host_hooks(), import_hook: Some(import_hook))
      // Install native $262 object on the global
      let #(h, realm_ref) =
        heap.alloc(
          h,
          value.RealmSlot(
            global_object:,
            lexical_globals: dict.new(),
            symbol_registry: dict.new(),
          ),
        )
      let h = heap.root(h, realm_ref)
      let #(h, dollar_262_ref) = entry.build_262(h, b, global_object, realm_ref)
      let #(h, _) =
        object.set_property(
          h,
          global_object,
          Named("$262"),
          value.JsObject(dollar_262_ref),
        )

      let realms = dict.from_list([#(realm_ref, b)])

      // Harness file order: print preamble → assert.js → sta.js →
      // doneprintHandle.js (if async) → extra includes
      let default_harness = ["assert.js", "sta.js"]
      let async_harness = case is_async {
        True -> ["doneprintHandle.js"]
        False -> []
      }
      let extra_includes =
        metadata.includes
        |> list.filter(fn(f) {
          !list.contains(default_harness, f) && !list.contains(async_harness, f)
        })
      let harness_files =
        list.flatten([default_harness, async_harness, extra_includes])

      let env =
        entry.ReplEnv(
          global_object:,
          lexical_globals: dict.new(),
          symbol_registry: dict.new(),
          realms:,
        )

      // Evaluate print preamble first (defines print + __print_output__)
      use preamble <- result.try(
        harness_template("__print_preamble__", fn() { Ok(print_preamble) }),
      )
      use #(h, env) <- result.try(eval_harness_template(preamble, h, b, env))

      use #(h, env) <- result.map(
        list.try_fold(harness_files, #(h, env), fn(acc, filename) {
          let #(heap, env) = acc
          use template <- result.try(
            harness_template(filename, fn() { read_harness_file(filename) }),
          )
          eval_harness_template(template, heap, b, env)
        }),
      )
      #(h, env, test_hooks)
    }
  }
}

/// Evaluate a compiled harness template as a REPL script.
fn eval_harness_template(
  template: value.FuncTemplate,
  h: Heap(host),
  b: common.Builtins,
  env: entry.ReplEnv,
) -> Result(#(Heap(host), entry.ReplEnv), String) {
  case entry.run_and_drain_repl(template, h, b, env) {
    Error(vm_err) -> Error("harness vm: " <> string.inspect(vm_err))
    Ok(#(Ok(_), new_heap, new_env)) -> Ok(#(new_heap, new_env))
    Ok(#(Error(thrown), new_heap, _)) ->
      Error("harness threw: " <> inspect_thrown(thrown, new_heap))
  }
}

fn get_data(
  h: Heap(host),
  ref: value.Ref,
  key: String,
) -> Result(value.JsValue, Nil) {
  case object.get_own_property(h, ref, Named(key)) {
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

fn inspect_thrown(val: value.JsValue, heap: Heap(host)) -> String {
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

// ============================================================================
// $262.agent — real BEAM-process test262 agent cluster (harness host layer)
//
// $262.agent.* is test262 HOST machinery (INTERPRETING.md), so it lives in
// the harness — the embedder — not in VM core: agent processes block on
// their BEAM mailboxes for broadcasts, acks, reports and Atomics wake
// messages, and mailbox receives are embedder territory (the same boundary
// as the Atomics host capabilities; see the contract in arc/host.gleam).
// The harness injects the `agent` object onto every $262 via the
// realm.set_extend_262 hook, registered per-test worker process and
// re-registered inside each spawned agent child.
//
// `$262.agent.start(script)` spawns a REAL BEAM child process
// (test262_exec_ffi.erl) that boots a completely fresh realm — its own
// heap, builtins, globals, and $262 — compiles the (NOT IIFE-wrapped: the
// child owns its realm globals) agent source, executes it, drains its
// event loop, and then parks in a broadcast loop.
//
// `broadcast(sab)` serializes the SharedArrayBuffer's backing storage and
// sends it to every child, blocking until each child acknowledges receipt
// (the ack is sent BEFORE the child invokes its receiveBroadcast callbacks,
// so a callback blocking in a sync Atomics.wait cannot deadlock broadcast).
// Because shared buffers are backed by an Erlang `atomics` array
// (value.BufShared — see arc_sab_ffi.erl) and atomics refs cross process
// boundaries by reference, the SAB the child reconstructs aliases the very
// same mutable cells as the parent's: Atomics writes in an agent are
// genuinely visible to the main agent and vice versa, and a child blocked
// in Atomics.wait can really be woken by the main agent's Atomics.notify.
//
// `report(str)` in a child posts the string to the parent's mailbox;
// `getReport()` in the parent drains that mailbox non-blockingly. The
// hidden __reports__/__agents__ queues remain: __agents__ holds the
// receiveBroadcast callbacks a child's script registered (consumed by the
// child's own broadcast loop), __reports__ backs report/getReport for the
// degenerate same-process case (the main agent reporting to itself).
// ============================================================================

/// Register the harness's $262 extension hook in the CURRENT process.
/// Process-local (like the CanBlock flag): must run in every per-test
/// worker before realms boot, and again inside each agent child body.
fn install_agent_hook() -> Nil {
  realm.set_extend_262(extend_262_with_agent)
}

/// The hook itself: build the agent object and hang it off the fresh $262.
fn extend_262_with_agent(
  h: Heap(host),
  b: common.Builtins,
  dollar_262: value.Ref,
) -> Heap(host) {
  let #(h, agent_ref) = build_agent(h, b)
  // builtin_property attributes (enumerable:False) — matches how the rest of
  // the $262 surface is defined and keeps "agent" out of Object.keys($262).
  object.define_method_property(
    h,
    dollar_262,
    Named("agent"),
    value.JsObject(agent_ref),
  )
}

/// Allocate the $262.agent object: host-closure methods plus two hidden
/// array-backed queues — __reports__ (strings posted by $262.agent.report,
/// consumed by getReport) and __agents__ (callbacks registered by
/// receiveBroadcast, invoked by the child's broadcast loop).
fn build_agent(h: Heap(host), b: common.Builtins) -> #(Heap(host), value.Ref) {
  let func_proto = b.function.prototype
  let #(h, reports_ref) = common.alloc_array(h, [], b.array.prototype)
  let #(h, agents_ref) = common.alloc_array(h, [], b.array.prototype)
  let methods = [
    #("start", agent_start_native, 1),
    #("broadcast", agent_broadcast_native, 2),
    #("getReport", agent_get_report_native, 0),
    #("sleep", agent_sleep_native, 1),
    #("monotonicNow", agent_monotonic_now_native, 0),
    #("report", agent_report_native, 1),
    #("leaving", agent_leaving_native, 0),
    #("receiveBroadcast", agent_receive_broadcast_native, 1),
  ]
  let #(h, method_props) =
    list.fold(methods, #(h, []), fn(acc, method) {
      let #(h, props) = acc
      let #(name, impl, arity) = method
      let #(h, fn_ref) = common.alloc_host_fn(h, func_proto, impl, name, arity)
      #(h, [
        #(Named(name), value.builtin_property(value.JsObject(fn_ref))),
        ..props
      ])
    })
  let hidden = [
    #(
      Named("__reports__"),
      value.data(value.JsObject(reports_ref)) |> value.configurable(),
    ),
    #(
      Named("__agents__"),
      value.data(value.JsObject(agents_ref)) |> value.configurable(),
    ),
  ]
  let #(h, ref) =
    heap.alloc(
      h,
      value.ObjectSlot(
        kind: value.OrdinaryObject,
        properties: dict.from_list(list.append(method_props, hidden)),
        symbol_properties: [],
        elements: elements.new(),
        prototype: Some(b.object.prototype),
        extensible: True,
      ),
    )
  #(h, ref)
}

/// A BEAM agent child process pid (opaque — see test262_exec_ffi.erl).
type AgentPid

/// The term `broadcast` ships to each child process. SharedArrayBuffers
/// travel as their raw `BufferData` storage: for `BufShared` the atomics
/// ref is shared by reference (true shared memory); a `BufBytes` payload
/// would arrive as a copy (non-shared buffers have no cross-agent identity
/// to preserve). Non-object primitives pass through as-is — they are
/// heap-independent.
type AgentPayload {
  /// Shared-ness is derived from `data` (`value.buffer_is_shared`), not a
  /// separate flag — `BufShared` is shared, `BufBytes` is not.
  AgentSabPayload(
    data: value.BufferData,
    max_byte_length: option.Option(Int),
    immutable: Bool,
  )
  AgentValuePayload(value: value.JsValue)
}

/// What woke an idle agent child process (see test262_exec_ffi.erl):
/// a parent broadcast, a cross-process Atomics.notify for one of this
/// agent's pending waitAsync waiters, or the parent process dying.
type AgentWake {
  AgentWakeBroadcast(payload: AgentPayload)
  AgentWakeNotify(key: state.WaiterKey, byte_index: Int)
  AgentWakeParentDown
}

/// $262.agent.start(script) — spawn a REAL BEAM child process that boots a
/// fresh realm and runs the agent script there. The source is NOT
/// IIFE-wrapped: the child has its own realm, so its top-level declarations
/// are its own realm globals (several tests start N agents with identical
/// scripts — separate realms keep them from colliding).
fn agent_start_native(
  args: List(value.JsValue),
  _this: value.JsValue,
  st: State(host),
) -> #(State(host), Result(value.JsValue, value.JsValue)) {
  let source = case args {
    [s, ..] -> s
    [] -> value.JsUndefined
  }
  use source_str, st <- coerce.try_to_string(st, source)
  let Nil = ffi_spawn_agent(fn() { run_agent_child(source_str) })
  #(st, Ok(value.JsUndefined))
}

/// Child-process body: boot a fresh realm (own heap/builtins/globals/$262),
/// compile + execute the agent script, drain the event loop, then park in
/// the broadcast loop until the parent broadcasts or goes away. Runs INSIDE
/// the spawned BEAM process — errors are reported to stderr, never thrown
/// back (there is no JS frame to throw into).
fn run_agent_child(source: String) -> Nil {
  // Fresh process: re-register the process-local $262 hook so this child's
  // realm (and any realm it creates) also gets the agent object. CanBlock
  // needs no re-set — fresh processes default to True, which is correct
  // for spawned agents (§9.7).
  let Nil = install_agent_hook()
  let h = heap.new()
  let #(h, b) = builtins.init(h)
  let #(h, global_ref) = builtins.globals(b, h)
  let #(h, realm_ref) =
    heap.alloc(
      h,
      value.RealmSlot(
        global_object: global_ref,
        lexical_globals: dict.new(),
        symbol_registry: dict.new(),
      ),
    )
  let h = heap.root(h, realm_ref)
  let #(h, dollar_262_ref) = entry.build_262(h, b, global_ref, realm_ref)
  let #(h, _) =
    object.set_property(
      h,
      global_ref,
      Named("$262"),
      value.JsObject(dollar_262_ref),
    )
  // The child's $262.agent object — its __agents__ queue collects the
  // receiveBroadcast callbacks the script registers; the broadcast loop
  // below invokes them.
  let agent_this = case
    object.get_own_property(h, dollar_262_ref, Named("agent"))
  {
    Some(value.DataProperty(value: v, ..)) -> v
    _ -> value.JsUndefined
  }
  let compiled =
    ffi_run_compile_task(string.byte_size(source), fn() {
      case parser.parse_script(source) {
        Error(err) -> Error(parser.parse_error_to_string(err))
        Ok(#(body, sb)) ->
          case compiler.compile_eval(body, sb) {
            Error(err) -> Error(string.inspect(err))
            Ok(template) -> Ok(template)
          }
      }
    })
  case compiled {
    Error(msg) ->
      io.println_error(
        "$262.agent.start: agent script did not compile: " <> msg,
      )
    Ok(template) -> {
      let locals =
        interpreter.init_top_level_locals(template, value.JsObject(global_ref))
      // The agent child is an embedder-driven State of its own: it blocks
      // in sync Atomics.wait and delivers notify wakes from THIS process,
      // so its realm is constructed with the harness host capabilities.
      let st =
        interpreter.new_state(
          template,
          locals,
          h,
          b,
          global_ref,
          dict.new(),
          dict.new(),
          harness_host_hooks(),
        )
      let st =
        State(
          ..st,
          ctx: RealmCtx(
            ..st.ctx,
            realms: dict.insert(st.ctx.realms, realm_ref, b),
          ),
        )
      // Agent scripts are plain top-level scripts (non-coroutine frames):
      // the narrowed loop reports a leaked suspension as a VmError.
      case interpreter.execute_to_completion(st, "test262 agent script") {
        Error(vm_err) ->
          io.println_error(
            "$262.agent: agent VM error: " <> string.inspect(vm_err),
          )
        Ok(#(completion, st)) -> {
          let Nil = case completion {
            ThrowCompletion(thrown) ->
              io.println_error(
                "$262.agent: agent script threw: "
                <> object.format_error(thrown, st.heap),
              )
            _ -> Nil
          }
          let st = settle_pending_wakes(st)
          agent_child_loop(st, agent_this)
        }
      }
    }
  }
}

/// Child broadcast loop: block until the parent broadcasts (the receipt ack
/// is sent by await_broadcast_or_notify BEFORE we run any JS), materialize
/// the payload in the child heap, invoke every registered receiveBroadcast
/// callback, drive the embedder loop (`settle_pending_wakes` — drain, then
/// consume `arc_notify` wakes bounded by the earliest pending deadline, so a
/// finite-timeout waitAsync settles "ok" instead of sleeping blindly to its
/// deadline), repeat. A cross-process Atomics.notify can also wake the loop:
/// an infinite-timeout waitAsync waiter has no deadline, so the drive
/// returns with it still pending and the notify message must be consumed
/// HERE — inject the wake and re-drive so its reaction jobs (e.g.
/// $262.agent.report) run. Ends — and the child process exits — when the
/// parent process goes away.
fn agent_child_loop(st: State(host), agent_this: value.JsValue) -> Nil {
  case ffi_await_broadcast_or_notify() {
    AgentWakeParentDown -> Nil
    AgentWakeNotify(key, byte_index) -> {
      let st = event_loop.inject_notify(st, key, byte_index)
      let st = settle_pending_wakes(st)
      agent_child_loop(st, agent_this)
    }
    AgentWakeBroadcast(payload) -> {
      let #(st, msg) = payload_to_value(st, payload)
      let st = case agent_queue(st, agent_this, "__agents__") {
        Ok(#(_arr_ref, callbacks)) ->
          list.fold(callbacks, st, fn(st, cb) {
            case state.call(st, cb, value.JsUndefined, [msg]) {
              Ok(#(_, st)) -> st
              Error(#(thrown, st)) -> {
                io.println_error(
                  "$262.agent: broadcast callback threw: "
                  <> object.format_error(thrown, st.heap),
                )
                st
              }
            }
          })
        Error(Nil) -> st
      }
      let st = settle_pending_wakes(st)
      agent_child_loop(st, agent_this)
    }
  }
}

/// Rebuild a broadcast payload as a JsValue in the child's heap. A
/// `BufShared` payload aliases the parent's atomics cells — this IS the
/// shared memory, not a copy.
fn payload_to_value(
  st: State(host),
  payload: AgentPayload,
) -> #(State(host), value.JsValue) {
  case payload {
    AgentValuePayload(v) -> #(st, v)
    AgentSabPayload(data:, max_byte_length:, immutable:) -> {
      let proto = case value.buffer_is_shared(data) {
        True -> st.builtins.shared_array_buffer.prototype
        False -> st.builtins.array_buffer.prototype
      }
      let #(heap, ref) =
        common.alloc_wrapper(
          st.heap,
          value.ArrayBufferObject(
            data: Some(data),
            max_byte_length:,
            immutable:,
          ),
          proto,
        )
      #(State(..st, heap:), value.JsObject(ref))
    }
  }
}

/// $262.agent.receiveBroadcast(callback) — register for the next broadcast.
fn agent_receive_broadcast_native(
  args: List(value.JsValue),
  this: value.JsValue,
  st: State(host),
) -> #(State(host), Result(value.JsValue, value.JsValue)) {
  let cb = case args {
    [f, ..] -> f
    [] -> value.JsUndefined
  }
  case agent_queue(st, this, "__agents__") {
    Ok(#(arr_ref, callbacks)) -> {
      let st = agent_queue_write(st, arr_ref, list.append(callbacks, [cb]))
      #(st, Ok(value.JsUndefined))
    }
    Error(Nil) ->
      state.type_error(st, "receiveBroadcast: $262.agent state missing")
  }
}

/// $262.agent.broadcast(sab) — ship the buffer to every child agent process
/// and block until all of them have RECEIVED it (test262 INTERPRETING.md).
/// Children ack on receipt, before invoking their receiveBroadcast
/// callbacks, so a callback that immediately blocks in a sync Atomics.wait
/// cannot deadlock the broadcaster.
fn agent_broadcast_native(
  args: List(value.JsValue),
  _this: value.JsValue,
  st: State(host),
) -> #(State(host), Result(value.JsValue, value.JsValue)) {
  let sab = case args {
    [v, ..] -> v
    [] -> value.JsUndefined
  }
  case make_broadcast_payload(st, sab) {
    Error(Nil) ->
      state.type_error(
        st,
        "$262.agent.broadcast: argument must be a (Shared)ArrayBuffer or a primitive",
      )
    Ok(payload) -> {
      let children = ffi_agent_children()
      let Nil = list.each(children, ffi_send_broadcast(_, payload))
      let Nil = ffi_await_acks(children)
      #(st, Ok(value.JsUndefined))
    }
  }
}

/// Serialize a broadcast argument. (Shared)ArrayBuffers travel as their
/// backing storage (atomics ref for shared — aliased, not copied);
/// primitives travel as-is; any other object has no cross-heap meaning.
fn make_broadcast_payload(
  st: State(host),
  v: value.JsValue,
) -> Result(AgentPayload, Nil) {
  case v {
    value.JsObject(ref) ->
      case heap.read(st.heap, ref) {
        // A detached buffer (`data: None`) has no storage to ship.
        Some(value.ObjectSlot(
          kind: value.ArrayBufferObject(
            data: Some(data),
            max_byte_length:,
            immutable:,
          ),
          ..,
        )) -> Ok(AgentSabPayload(data:, max_byte_length:, immutable:))
        _ -> Error(Nil)
      }
    other -> Ok(AgentValuePayload(other))
  }
}

/// $262.agent.report(value) — in a child agent process, post ToString(value)
/// to the parent's mailbox; in the main agent, push onto the local
/// __reports__ queue (degenerate self-report).
fn agent_report_native(
  args: List(value.JsValue),
  this: value.JsValue,
  st: State(host),
) -> #(State(host), Result(value.JsValue, value.JsValue)) {
  let val = case args {
    [v, ..] -> v
    [] -> value.JsUndefined
  }
  use str, st <- coerce.try_to_string(st, val)
  case ffi_agent_parent() {
    Ok(parent) -> {
      let Nil = ffi_send_report(parent, str)
      #(st, Ok(value.JsUndefined))
    }
    Error(Nil) ->
      case agent_queue(st, this, "__reports__") {
        Ok(#(arr_ref, reports)) -> {
          let st =
            agent_queue_write(
              st,
              arr_ref,
              list.append(reports, [value.JsString(str)]),
            )
          #(st, Ok(value.JsUndefined))
        }
        Error(Nil) -> state.type_error(st, "report: $262.agent state missing")
      }
  }
}

/// $262.agent.getReport() — dequeue the oldest report, or null when none is
/// pending. Local (same-process) reports first, then the mailbox of reports
/// posted by child agent processes.
fn agent_get_report_native(
  _args: List(value.JsValue),
  this: value.JsValue,
  st: State(host),
) -> #(State(host), Result(value.JsValue, value.JsValue)) {
  case agent_queue(st, this, "__reports__") {
    Ok(#(arr_ref, reports)) ->
      case reports {
        [] ->
          case ffi_take_report() {
            Ok(report) -> #(st, Ok(value.JsString(report)))
            Error(Nil) -> #(st, Ok(value.JsNull))
          }
        [head, ..rest] -> {
          let st = agent_queue_write(st, arr_ref, rest)
          #(st, Ok(head))
        }
      }
    Error(Nil) -> state.type_error(st, "getReport: $262.agent state missing")
  }
}

/// $262.agent.sleep(ms) — block the (single) BEAM scheduler thread running
/// this VM for ms milliseconds.
fn agent_sleep_native(
  args: List(value.JsValue),
  _this: value.JsValue,
  st: State(host),
) -> #(State(host), Result(value.JsValue, value.JsValue)) {
  let val = case args {
    [v, ..] -> v
    [] -> value.JsUndefined
  }
  case coerce.js_to_number(st, val) {
    Error(#(thrown, st)) -> #(st, Error(thrown))
    Ok(#(num, st)) -> {
      let ms = case num {
        value.Finite(f) -> value.float_to_int(f)
        _ -> 0
      }
      let Nil = builtins_atomics.sleep_ms(ms)
      #(st, Ok(value.JsUndefined))
    }
  }
}

/// $262.agent.monotonicNow() — monotonic milliseconds (same clock as the
/// waitAsync deadline bookkeeping).
fn agent_monotonic_now_native(
  _args: List(value.JsValue),
  _this: value.JsValue,
  st: State(host),
) -> #(State(host), Result(value.JsValue, value.JsValue)) {
  #(st, Ok(value.from_int(builtins_atomics.monotonic_now())))
}

/// $262.agent.leaving() — agent termination hint. The child process exits
/// when its parent goes away (parent monitor), so this is a no-op.
fn agent_leaving_native(
  _args: List(value.JsValue),
  _this: value.JsValue,
  st: State(host),
) -> #(State(host), Result(value.JsValue, value.JsValue)) {
  #(st, Ok(value.JsUndefined))
}

/// Read a hidden JsObject-valued own property off the agent object.
fn agent_hidden_ref(
  st: State(host),
  this: value.JsValue,
  name: String,
) -> Result(value.Ref, Nil) {
  case this {
    value.JsObject(this_ref) ->
      case object.get_own_property(st.heap, this_ref, Named(name)) {
        Some(value.DataProperty(value: value.JsObject(ref), ..)) -> Ok(ref)
        _ -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

/// Read an agent queue array as #(ref, values). Error(Nil) if missing.
fn agent_queue(
  st: State(host),
  this: value.JsValue,
  name: String,
) -> Result(#(value.Ref, List(value.JsValue)), Nil) {
  use arr_ref <- result.try(agent_hidden_ref(st, this, name))
  case heap.read(st.heap, arr_ref) {
    Some(value.ObjectSlot(kind: value.ArrayObject(length), elements: els, ..)) ->
      Ok(#(arr_ref, elements.to_list_padded(els, length)))
    _ -> Error(Nil)
  }
}

/// Overwrite an agent queue array's contents in place.
fn agent_queue_write(
  st: State(host),
  arr_ref: value.Ref,
  values: List(value.JsValue),
) -> State(host) {
  let heap = case heap.read(st.heap, arr_ref) {
    Some(value.ObjectSlot(kind: value.ArrayObject(_), ..) as slot) ->
      heap.write(
        st.heap,
        arr_ref,
        value.ObjectSlot(
          ..slot,
          kind: value.ArrayObject(list.length(values)),
          elements: elements.from_list(values),
        ),
      )
    _ -> st.heap
  }
  State(..st, heap:)
}

// -- Agent FFI (test262_exec_ffi.erl) --

@external(erlang, "test262_exec_ffi", "spawn_agent")
fn ffi_spawn_agent(_body: fn() -> Nil) -> Nil {
  panic as beam_only_test
}

@external(erlang, "test262_exec_ffi", "agent_children")
fn ffi_agent_children() -> List(AgentPid) {
  panic as beam_only_test
}

@external(erlang, "test262_exec_ffi", "agent_parent")
fn ffi_agent_parent() -> Result(AgentPid, Nil) {
  panic as beam_only_test
}

@external(erlang, "test262_exec_ffi", "send_broadcast")
fn ffi_send_broadcast(_pid: AgentPid, _payload: AgentPayload) -> Nil {
  panic as beam_only_test
}

@external(erlang, "test262_exec_ffi", "await_acks")
fn ffi_await_acks(_pids: List(AgentPid)) -> Nil {
  panic as beam_only_test
}

@external(erlang, "test262_exec_ffi", "await_broadcast_or_notify")
fn ffi_await_broadcast_or_notify() -> AgentWake {
  panic as beam_only_test
}

@external(erlang, "test262_exec_ffi", "send_report")
fn ffi_send_report(_parent: AgentPid, _report: String) -> Nil {
  panic as beam_only_test
}

@external(erlang, "test262_exec_ffi", "take_report")
fn ffi_take_report() -> Result(String, Nil) {
  panic as beam_only_test
}

// -- Atomics host capabilities (harness as embedder) --
//
// The embedder side of the host capability contract in arc/host.gleam:
// clause 1 (blocking sync wait) and clause 2 (wake delivery) supplied as the
// HostHooks value every State the harness boots is constructed with,
// clauses 3-4 (the bounded mailbox receive
// that feeds event_loop.inject_notify) as the post-script driver. Core
// registers waiterlist entries and claims waiters as pure ETS data
// (arc_waiter_ffi); every receive of — and every send into — the
// `{arc_notify, Ref, Key, ByteIndex}` wake protocol happens HERE, via
// test262_exec_ffi.erl. This is test262 HOST machinery (INTERPRETING.md):
// agents and blocking waits are the host's job, the same engine/platform
// split as V8 vs d8.

/// Clause 1's blocking receive: selective receive for the entry's wake,
/// with the notify-vs-timeout race resolved by ets:take of our own entry
/// (negative timeout = infinity). Returns the JS "ok" / "timed-out".
@external(erlang, "test262_exec_ffi", "await_notify")
fn ffi_await_notify(_handle: state.WaiterHandle, _timeout_ms: Int) -> String {
  panic as beam_only_test
}

/// Clause 2's wake delivery: send `{arc_notify, Ref, Key, ByteIndex}` to
/// each remote waiter claimed by Atomics.notify's waiterlist take.
@external(erlang, "test262_exec_ffi", "deliver_wakes")
fn ffi_deliver_wakes(_claimed: List(state.ClaimedWaiter)) -> Nil {
  panic as beam_only_test
}

/// Clause 4's bounded dry-queue receive for arc_notify messages.
@external(erlang, "test262_exec_ffi", "wait_for_notify")
fn ffi_wait_for_notify(_ms: Int) -> Option(#(state.WaiterKey, Int)) {
  panic as beam_only_test
}

/// Blocking-wait capability (`AtomicsCapabilities.sync_wait` on
/// `HostHooks.atomics`, contract clause 1):
/// suspend this worker process in a selective receive until the registered
/// waiterlist entry is woken or the timeout elapses. `timeout_ms: None` =
/// wait forever (the FFI clamps to the BEAM receive ceiling). A `Some(0)`
/// timeout doubles as the cancel flush — see await_notify's doc.
fn atomics_sync_wait(req: state.WaitRequest) -> state.WaitOutcome {
  let state.WaitRequest(handle:, timeout_ms:, key: _, byte_index: _) = req
  case ffi_await_notify(handle, option.unwrap(timeout_ms, -1)) {
    "timed-out" -> state.WaitTimedOut
    _ -> state.WaitOk
  }
}

/// The harness's host capabilities — both Atomics capabilities together,
/// per contract clause 5 (a host that can block but not deliver wakes, or
/// vice versa, deadlocks its peers). Supplied ONCE wherever the harness
/// boots a realm (entry/module/agent State construction); every derived
/// State — eval/Function realms, $262.createRealm children, $262.agent
/// children, module bodies including dynamic import() — inherits them.
/// Leaves `State.can_block` untouched — that is per-agent spec policy (the
/// CanBlockIsFalse flag), not capability presence.
fn harness_host_hooks() -> host.HostHooks {
  host.atomics_capabilities(
    sync_wait: atomics_sync_wait,
    deliver_wake: ffi_deliver_wakes,
  )
}

/// One bounded wait-settle-drain step (contract clause 3): block at most
/// `timeout_ms` for a cross-process `arc_notify` message; if one arrives,
/// settle this agent's first matching waitAsync waiter with "ok" and
/// re-drain microtasks. `False` = the timeout elapsed, i.e. a deadline is
/// due — the caller's next drain fires it.
fn wait_settle_step(s: State(host), timeout_ms: Int) -> #(State(host), Bool) {
  case ffi_wait_for_notify(timeout_ms) {
    Some(#(key, byte_index)) -> {
      let s = event_loop.inject_notify(s, key, byte_index)
      #(event_loop.drain_jobs_yielding(s), True)
    }
    None -> #(s, False)
  }
}

/// The harness's post-script driver: drain, then loop `wait_settle_step`
/// bounded by the earliest pending deadline until no settleable deadline
/// remains. Mirrors event_loop.drain_jobs' dry-queue semantics: when only
/// deadline-free (infinite) waiters remain and no wake arrives, the loop
/// returns — a mailbox loop cannot distinguish a never-notified infinite
/// waiter from quiescence, and parking forever here would hang the worker.
fn settle_pending_wakes(s: State(host)) -> State(host) {
  let s = event_loop.drain_jobs_yielding(s)
  case s.atomics_waiters {
    [] -> s
    _ ->
      case event_loop.next_deadline_timeout(s) {
        None -> s
        Some(timeout) -> {
          let #(s, _woke) = wait_settle_step(s, timeout)
          settle_pending_wakes(s)
        }
      }
  }
}

/// See arc_compile_task_ffi:run_compile_task/2 — runs the compile in a
/// short-lived, generously sized-heap process (sync spawn-compute-join),
/// keeping the agent child's own heap small.
@external(erlang, "arc_compile_task_ffi", "run_compile_task")
fn ffi_run_compile_task(_source_bytes: Int, _task: fn() -> a) -> a {
  panic as beam_only_test
}

// -- FFI (BEAM-only; JS target gets panic bodies) --

const beam_only_test = "test262 suite is BEAM-only"

@external(erlang, "test262_exec_ffi", "init_stats")
fn init_stats() -> Nil {
  panic as beam_only_test
}

// The realm/template caches share one persistent_term-backed store keyed by
// string; the two typed views below must use disjoint keys (realm_cache_key
// vs harness filenames) since FFI bypasses the type checker.

@external(erlang, "test262_exec_ffi", "cache_get")
fn realm_cache_get(
  _key: String,
) -> option.Option(#(Heap(host), common.Builtins, value.Ref)) {
  panic as beam_only_test
}

@external(erlang, "test262_exec_ffi", "cache_put")
fn realm_cache_put(
  _key: String,
  _snapshot: #(Heap(host), common.Builtins, value.Ref),
) -> Nil {
  panic as beam_only_test
}

@external(erlang, "test262_exec_ffi", "cache_get")
fn template_cache_get(_key: String) -> option.Option(value.FuncTemplate) {
  panic as beam_only_test
}

@external(erlang, "test262_exec_ffi", "cache_put")
fn template_cache_put(_key: String, _template: value.FuncTemplate) -> Nil {
  panic as beam_only_test
}

@external(erlang, "test262_exec_ffi", "init_config")
fn init_config(
  _update_mode: Bool,
  _has_snapshot: Bool,
  _fail_log: option.Option(String),
) -> Nil {
  panic as beam_only_test
}

@external(erlang, "test262_exec_ffi", "init_snapshot_set")
fn init_snapshot_set(_paths: List(String)) -> Nil {
  panic as beam_only_test
}

@external(erlang, "test262_exec_ffi", "get_update_mode")
fn get_update_mode() -> Bool {
  panic as beam_only_test
}

@external(erlang, "test262_exec_ffi", "get_has_snapshot")
fn get_has_snapshot() -> Bool {
  panic as beam_only_test
}

@external(erlang, "test262_exec_ffi", "get_fail_log")
fn get_fail_log() -> option.Option(String) {
  panic as beam_only_test
}

@external(erlang, "test262_exec_ffi", "snapshot_contains")
fn snapshot_contains(_path: String) -> Bool {
  panic as beam_only_test
}

@external(erlang, "test262_exec_ffi", "record_pass")
fn record_pass() -> Nil {
  panic as beam_only_test
}

@external(erlang, "test262_exec_ffi", "record_fail")
fn record_fail() -> Nil {
  panic as beam_only_test
}

@external(erlang, "test262_exec_ffi", "record_skip")
fn record_skip() -> Nil {
  panic as beam_only_test
}

@external(erlang, "test262_exec_ffi", "get_stats")
fn get_stats() -> #(Int, Int, Int) {
  panic as beam_only_test
}

@external(erlang, "test262_exec_ffi", "record_pass_path")
fn record_pass_path(_path: String) -> Nil {
  panic as beam_only_test
}

@external(erlang, "test262_exec_ffi", "get_pass_paths")
fn get_pass_paths() -> List(String) {
  panic as beam_only_test
}
