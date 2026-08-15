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
import arc/esm
import arc/host
import arc/host_hooks.{HostHooks}
import arc/internal/path
import arc/interp/entry
import arc/interp/safepoint
import arc/module
import arc/module/load_error
import arc/module_host
import arc/parser
import arc/rt/async as rt_async
import arc/rt/buffer
import arc/rt/builtins as rt_builtins
import arc/rt/builtins/common
import arc/rt/builtins/realm_ops
import arc/rt/bytecode.{type FuncTemplate}
import arc/rt/call.{NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/elements
import arc/rt/inspect as rt_inspect
import arc/rt/obj as rt_obj
import arc/rt/realm as rt_realm
import arc/rt/sab
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BufferStorage, type Handle, type JsVal, type WaiterRef, Agent,
  ArrayBufferObj, ArrayObj, DataProperty, Detached, JFloat, JInt, KHandle, KStr,
  KUndef, Named, NoElements, Ordinary, ProxyObj, SObject, SShapedObject, Shared,
  StringKey, classify, mk_null, mk_number, mk_object, mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set
import gleam/string
import simplifile
import test262_metadata.{type TestMetadata, Parse, Resolution, Runtime}
import test262_suite.{type StrictnessVariant}
import test_runner

/// A settled top-level run: `Ok(value)` for a normal completion,
/// `Error(thrown)` for an uncaught throw, paired with the drained agent.
type Settled =
  #(Result(JsVal, JsVal), Agent)

/// The state the harness's host functions thread. The one typed host
/// payload the harness keeps is the pid of a `$262.agent` child process
/// (the `__children__` list on the agent object), so that is the payload
/// type.
type HostState =
  host.State(AgentPid)

const test_dir: String = "vendor/test262/test"

/// Global the `print` host function writes: the async test protocol
/// (doneprintHandle.js) reports through `print`, and the runner reads the
/// last printed line back from here.
const print_output: String = "__print_output__"

const harness_dir: String = "vendor/test262/harness"

const snapshot_path: String = ".github/test262/pass.txt"

/// Initialize ETS tables and config. Called once before tests start.
pub fn init() -> Nil {
  let fail_log = test_runner.get_env("FAIL_LOG") |> option.from_result
  let update_mode = test_runner.get_env_is_truthy("UPDATE_SNAPSHOT")
  let snapshot = test262_suite.load_pass_list(snapshot_path)
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
// The booted base agent (store + realm 0 intrinsics + global object, with
// the interpreter linked) and the compiled harness templates are immutable
// Gleam data, so they are computed once and shared across all per-test
// worker processes via persistent_term (zero-copy reads). Each test starts
// from the shared agent and forks its own store on mutation, so tests stay
// fully isolated.

const agent_cache_key = "base_agent"

/// Warm the agent and common-harness caches from the main process before
/// workers spawn, so parallel first uses don't race on cache_put.
fn warm_caches() -> Nil {
  let _ = boot_base_agent()
  list.each(["assert.js", "sta.js", "doneprintHandle.js"], fn(filename) {
    case harness_template(filename, fn() { read_harness_file(filename) }) {
      Ok(_) -> Nil
      Error(err) -> io.println("Warning: harness cache warm failed: " <> err)
    }
  })
}

/// The agent a test runs on: the base agent, with the test's [[CanBlock]]
/// policy applied. test262's CanBlockIsFalse flag asks for an agent whose
/// AgentCanSuspend() is false (sync Atomics.wait throws a TypeError); every
/// other test — and every `$262.agent` child — may block (CanBlockIsTrue is
/// the harness default, §9.7).
fn boot_agent(metadata: TestMetadata) -> Agent {
  let agent = boot_base_agent()
  case list.contains(metadata.flags, "CanBlockIsFalse") {
    True -> Agent(..agent, hooks: HostHooks(..agent.hooks, can_block: False))
    False -> agent
  }
}

/// Boot (or fetch the cached) base agent: a fresh realm on the harness's
/// host hooks with the bytecode interpreter linked into its `JsOps`.
fn boot_base_agent() -> Agent {
  case agent_cache_get(agent_cache_key) {
    Some(agent) -> agent
    None -> {
      let agent = rt_builtins.new_agent(harness_host_hooks()) |> entry.link
      agent_cache_put(agent_cache_key, agent)
      agent
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
) -> Result(FuncTemplate, String) {
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

fn compile_harness_source(source: String) -> Result(FuncTemplate, String) {
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

/// The test262 files this run covers (relative paths): everything under
/// the test dir, narrowed by TEST262_FILTER / TEST262_SHARD.
pub fn list_files() -> List(String) {
  test262_suite.list_test_files(test_dir) |> test262_suite.select_files
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
  io.println(
    "\n"
    <> test262_suite.summary_line(
      "test262 exec",
      pass_count,
      fail_count,
      skip_count,
    ),
  )

  case fail_log {
    Some(path) -> io.println("Failures written to " <> path)
    None -> Nil
  }

  // Write snapshot if UPDATE_SNAPSHOT=1
  case update_mode {
    True -> {
      let paths = get_pass_paths()
      case test262_suite.write_pass_list(snapshot_path, paths) {
        Ok(Nil) -> {
          io.println(
            "Snapshot updated: "
            <> snapshot_path
            <> " ("
            <> int.to_string(list.length(paths))
            <> " passing tests)",
          )
          case test262_suite.partial_run_env() {
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
      let json = test262_suite.results_json(pass_count, fail_count, skip_count)
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

// --- Test execution ---

fn run_test_by_phase(
  metadata: TestMetadata,
  source: String,
  path: String,
) -> TestOutcome {
  let variants = test262_suite.variants_for_test(metadata)
  let is_module = list.contains(metadata.flags, "module")
  let is_async = list.contains(metadata.flags, "async")

  // Run all variants; a test passes only if ALL variants pass
  list.fold_until(variants, Pass, fn(_acc, variant) {
    let outcome = case metadata.negative_phase {
      Some(Parse) -> run_parse_negative_test(metadata, source, variant)
      Some(Resolution) ->
        run_resolution_negative_test(
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
      Fail(reason) ->
        list.Stop(Fail(reason <> test262_suite.variant_label(variant)))
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
  case parser.parse(test262_suite.variant_source(source, variant), mode) {
    Error(_) -> Pass
    Ok(_) -> Fail("expected parse error but parsed successfully")
  }
}

/// Shared scaffold for running a test to completion: handles the
/// module/script branch, timeout, and async dispatch. Callers supply how to
/// map run errors, completions, and async completions to outcomes. The
/// outcome is judged INSIDE the timed run: `run_with_timeout` runs it in a
/// process of its own, and only the small verdict travels back — never the
/// settled agent, whose store would be copied whole (sharing flattened)
/// into the heap-capped worker.
fn run_test_completion(
  metadata: TestMetadata,
  source: String,
  is_module: Bool,
  path: String,
  variant: StrictnessVariant,
  is_async: Bool,
  on_error: fn(String) -> TestOutcome,
  completion_outcome: fn(Settled) -> TestOutcome,
  async_outcome: fn(Settled) -> TestOutcome,
) -> TestOutcome {
  let run = case is_module {
    True -> fn() { do_run_module(metadata, source, path, is_async) }
    False -> fn() {
      do_run_script_with_harness(metadata, source, path, variant, is_async)
    }
  }
  let judged = fn() {
    case run() {
      Error(reason) -> on_error(reason)
      Ok(settled) ->
        case is_async {
          False -> completion_outcome(settled)
          True -> async_outcome(settled)
        }
    }
  }
  case test_runner.run_with_timeout(judged, test_timeout_ms) {
    Error(reason) -> on_error(reason)
    Ok(outcome) -> outcome
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
    fn(settled) {
      // For async negative tests, $DONE reports via print
      case check_async_completion(settled) {
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

/// A `phase: resolution` negative test succeeds when the error is raised
/// while building or linking the module graph (§16.2 INTERPRETING.md), before
/// any code executes: a dependency's parse error, a resolver rejection, or a
/// link failure. Those surface here as a Gleam-level `module.compile_bundle`
/// error (stringified via `do_run_module`), not a JS throw. A JS throw is
/// still accepted — link errors reach evaluation as a realm-built
/// SyntaxError.
fn run_resolution_negative_test(
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
    resolution_error_outcome(metadata, _),
    negative_completion_outcome(metadata, _),
    fn(_settled) {
      Fail("expected resolution-phase error but async test ran to $DONE")
    },
  )
}

/// Match a `module.compile_bundle`/link error against the test's expected
/// negative type. A `GraphError(ParseFailed(..))` — a dependency's early
/// error — is exactly the SyntaxError a `phase: resolution` test asks for;
/// resolver and load rejections likewise satisfy a `phase: resolution`
/// TypeError. Anything else is a genuine harness/engine failure and stays
/// FAIL with the raw reason.
fn resolution_error_outcome(
  metadata: TestMetadata,
  reason: String,
) -> TestOutcome {
  let expected = option.unwrap(metadata.negative_type, "")
  let is_match = case expected {
    "SyntaxError" ->
      string.contains(reason, "ParseFailed(")
      || string.contains(reason, "LinkError(")
    "TypeError" ->
      string.contains(reason, "ResolveFailed(")
      || string.contains(reason, "LoadFailed(")
    _ -> False
  }
  case is_match {
    True -> Pass
    False ->
      Fail("expected resolution-phase " <> expected <> " but got: " <> reason)
  }
}

/// Map a settled run to the outcome for a runtime-negative test:
/// only a throw (of the expected error type) passes.
fn negative_completion_outcome(
  metadata: TestMetadata,
  settled: Settled,
) -> TestOutcome {
  case settled {
    #(Error(thrown), st) -> verify_negative_type(metadata, thrown, st)
    #(Ok(_), _) -> Fail("expected runtime throw but completed normally")
  }
}

/// Map a settled run to the outcome for a positive test:
/// only a normal completion passes.
fn positive_completion_outcome(settled: Settled) -> TestOutcome {
  case settled {
    #(Ok(_), _) -> Pass
    #(Error(thrown), st) ->
      Fail("unexpected throw: " <> inspect_thrown(thrown, st))
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
fn check_async_positive(settled: Settled) -> TestOutcome {
  case check_async_completion(settled) {
    Ok(Nil) -> Pass
    Error(reason) -> Fail(reason)
  }
}

/// Core async completion check. Returns Ok(Nil) for "Test262:AsyncTestComplete",
/// Error with reason for everything else.
fn check_async_completion(settled: Settled) -> Result(Nil, String) {
  case settled {
    #(Error(thrown), st) ->
      Error("unexpected throw: " <> inspect_thrown(thrown, st))
    #(Ok(_), st) ->
      case get_data(st, st.realm.global_object, print_output) {
        None -> Error("async test did not call $DONE (no __print_output__)")
        Some(output) ->
          case classify(output) {
            KStr("Test262:AsyncTestComplete") -> Ok(Nil)
            KStr("Test262:AsyncTestFailure:" <> msg) ->
              Error("async failure: " <> msg)
            KStr(other) -> Error("unexpected print output: " <> other)
            KUndef -> Error("async test did not call $DONE")
            _ ->
              Error(
                "unexpected __print_output__: "
                <> rt_inspect.inspect(st, output),
              )
          }
      }
  }
}

fn verify_negative_type(
  metadata: TestMetadata,
  thrown: JsVal,
  st: Agent,
) -> TestOutcome {
  case metadata.negative_type {
    None -> Pass
    Some(expected_type) -> {
      let actual_name = {
        use h <- option.then(as_handle(thrown))
        use name <- option.then(get_data(st, h, "name"))
        case classify(name) {
          KStr(n) -> Some(n)
          _ -> None
        }
      }
      case actual_name {
        Some(name) if name == expected_type -> Pass
        Some(name) ->
          Fail(
            "expected "
            <> expected_type
            <> " but got "
            <> name
            <> ": "
            <> inspect_thrown(thrown, st),
          )
        None -> Pass
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
) -> Result(Settled, String) {
  // Evaluate harness files as REPL scripts to populate globals. Async module
  // tests use the same $DONE/print protocol as scripts (doneprintHandle.js).
  use st <- result.try(eval_harness(
    metadata,
    boot_agent(metadata),
    path,
    is_async,
  ))

  case module.compile_bundle(path, source, test262_resolve, test262_load) {
    Error(err) -> Error("module: " <> string.inspect(err))
    Ok(bundle) -> {
      // Evaluate through the realm-wide module registry so a dynamic
      // import() of any module in this static graph (including the test file
      // itself) resolves to the same module record instead of re-evaluating
      // it (§16.2.1.8). The post-body driver drains microtasks, so leftover
      // jobs are always empty here.
      let #(st, res) =
        module_host.evaluate_bundle_with_registry(
          st,
          bundle,
          settle_pending_wakes,
        )
      case res {
        Ok(module.EvaluatedBundle(value: val, ..)) -> Ok(#(Ok(val), st))
        Error(module.EvaluationError(value: val)) -> Ok(#(Error(val), st))
        // Entry module still parked on top-level await after a full drain:
        // an awaited promise can never settle. Reported as a host-level
        // throw.
        Error(module.EvaluationPending(promise: _)) ->
          Ok(#(
            Error(mk_string(
              "module evaluation never completed: top-level await promise never settled",
            )),
            st,
          ))
        Error(err) -> Error("module: " <> string.inspect(err))
      }
    }
  }
}

/// Resolve a test262 dependency specifier relative to its parent's directory.
/// The runner is a filesystem loader: a bare specifier is not a path.
///
/// `module_host.ResolveFn` is a stringly host boundary; the Raw/Resolved
/// distinction is put back on at this edge and taken off again on the way out.
fn test262_resolve(
  raw_specifier: String,
  parent_specifier: String,
) -> Result(String, module_host.ResolveError) {
  let raw = esm.raw(raw_specifier)
  let parent = esm.resolved_unchecked(parent_specifier)
  case path.resolve_specifier(raw, parent) {
    path.PathSpecifier(resolved) -> Ok(esm.resolved_text(resolved))
    path.BareSpecifier(_bare) -> Error(load_error.UnsupportedBareSpecifier)
  }
}

/// Read a resolved test262 module from disk.
fn test262_load(resolved: String) -> Result(String, module_host.LoadError) {
  case simplifile.read(resolved) {
    Ok(source) -> Ok(source)
    Error(simplifile.Enoent) -> Error(load_error.LoadNotFound)
    Error(err) -> Error(load_error.ReadFailed(simplifile.describe_error(err)))
  }
}

fn do_run_script_with_harness(
  metadata: TestMetadata,
  source: String,
  path: String,
  variant: StrictnessVariant,
  is_async: Bool,
) -> Result(Settled, String) {
  // Evaluate harness files as REPL scripts to populate globals
  use st <- result.try(eval_harness(
    metadata,
    boot_agent(metadata),
    path,
    is_async,
  ))

  // Prepend "use strict" to test source only (not harness) when strict
  let test_source = test262_suite.variant_source(source, variant)

  case parser.parse_script(test_source) {
    Error(err) -> Error("parse: " <> parser.parse_error_to_string(err))
    Ok(#(body, sb)) ->
      case compiler.compile_repl(body, sb) {
        Error(err) -> Error("compile: " <> string.inspect(err))
        // §16.1.6 ScriptEvaluation in the harness's realm, then the
        // post-script driver (microtask checkpoint) so promise reactions,
        // and with them the async $DONE protocol, settle before the
        // outcome is read.
        Ok(template) -> Ok(run_settled(st, template))
      }
  }
}

/// Run one compiled script in `st`'s current realm and drive its turn to
/// quiescence. The completion value stays rooted across the turn-end
/// collect and the drain (which collects between jobs), since the outcome
/// is read off it afterwards.
fn run_settled(st: Agent, template: FuncTemplate) -> Settled {
  let #(completion, st) = entry.run_script(st, template)
  let held = case completion {
    NormalCompletion(v) -> v
    ThrowCompletion(e) -> e
  }
  let st = safepoint.finish_turn(st, [held], settle_pending_wakes)
  case completion {
    NormalCompletion(v) -> #(Ok(v), st)
    ThrowCompletion(e) -> #(Error(e), st)
  }
}

/// Evaluate harness files as REPL scripts to populate globals.
/// This is the spec-correct approach: harness is evaluated in the realm
/// before the test runs, making harness functions (assert, etc.) available
/// as globals; top-level `let`/`const` persist as the realm's lexical
/// globals across scripts.
///
/// Also installs the engine-side state the TEST needs on the agent: the
/// dynamic-import host hook (relative to the test file), `$262` (with the
/// harness's `agent` API) and the `print` protocol.
fn eval_harness(
  metadata: TestMetadata,
  st: Agent,
  path: String,
  is_async: Bool,
) -> Result(Agent, String) {
  let is_raw = list.contains(metadata.flags, "raw")
  case is_raw {
    // Raw tests get no harness and no import hook — import() rejects.
    True -> Ok(st)
    False -> {
      // The dynamic-import host hook: import() resolves specifiers relative
      // to the test file and loads fixtures from disk. It is engine state on
      // the agent, never a globalThis property.
      let st =
        module_host.install_import_hook(st, path, test262_resolve, test262_load)
      // Native $262 (global/evalScript/createRealm/gc/detachArrayBuffer) on
      // the global, extended with the harness's `agent` API and `print`.
      // This is the main agent: it has no parent to report to.
      let st = install_host_api(st, None)

      let harness_files = test262_suite.harness_files(metadata, is_async)
      list.try_fold(harness_files, st, fn(st, filename) {
        use template <- result.try(
          harness_template(filename, fn() { read_harness_file(filename) }),
        )
        eval_harness_template(template, st)
      })
    }
  }
}

/// `$262` plus the harness host functions (`$262.agent.*`, `print`) on the
/// current realm of `st`. `parent` is the pid of the agent process that
/// started this one — `None` in the main agent.
fn install_host_api(st: Agent, parent: Option(AgentPid)) -> Agent {
  let #(dollar_262, st) = rt_realm.install_262(st, st.realm)
  let s = host.from_agent(st, host.new_key())
  let s = extend_262_with_agent(s, dollar_262, parent)
  let s = install_print(s)
  s.agent
}

/// `print(x)` stores ToString(x) in the `__print_output__` global (initially
/// `undefined`): the capture side of the async $DONE protocol.
fn install_print(s: HostState) -> HostState {
  let s = host.define_global(s, print_output, mk_undefined())
  host.define_fn(s, "print", 1, print_native)
}

fn print_native(
  args: List(JsVal),
  _this: JsVal,
  s: HostState,
) -> #(HostState, Result(JsVal, JsVal)) {
  let #(str, st) = rt_val.t_to_string(s.agent, host.first_arg(args))
  let global = mk_object(st.realm.global_object)
  let #(_ok, st) =
    rt_obj.t_set_prop(
      st,
      global,
      StringKey(Named(print_output)),
      mk_string(str),
    )
  done(s, st)
}

/// Evaluate a compiled harness template as a REPL script.
fn eval_harness_template(
  template: FuncTemplate,
  st: Agent,
) -> Result(Agent, String) {
  case run_settled(st, template) {
    #(Ok(_), st) -> Ok(st)
    #(Error(thrown), st) ->
      Error("harness threw: " <> inspect_thrown(thrown, st))
  }
}

fn as_handle(v: JsVal) -> Option(Handle) {
  case classify(v) {
    KHandle(h) -> Some(h)
    _ -> None
  }
}

/// `[[Prototype]]` of an ordinary object without running user code: a Proxy
/// (whose trap could throw) or a non-object cell has none here.
fn ordinary_proto(st: Agent, h: Handle) -> Option(Handle) {
  case rt_store.t_cell_get(st, h) {
    SObject(kind: ProxyObj(..), ..) -> None
    SObject(proto:, ..) | SShapedObject(proto:, ..) -> proto
    _ -> None
  }
}

/// The value of the DATA property `key` on `h` or its prototype chain,
/// without getters or traps.
fn get_data(st: Agent, h: Handle, key: String) -> Option(JsVal) {
  case rt_obj.t_ordinary_own_property(st, h, StringKey(Named(key))) {
    Some(DataProperty(value: val, ..)) -> Some(val)
    Some(_) -> None
    None -> option.then(ordinary_proto(st, h), get_data(st, _, key))
  }
}

fn inspect_thrown(val: JsVal, st: Agent) -> String {
  let described = {
    use h <- option.then(as_handle(val))
    use message <- option.then(get_data(st, h, "message"))
    case classify(message) {
      KStr(msg) -> {
        let name = case option.map(get_data(st, h, "name"), classify) {
          Some(KStr(n)) -> n
          _ -> "Error"
        }
        Some(name <> ": " <> msg)
      }
      _ -> None
    }
  }
  option.lazy_unwrap(described, fn() { rt_inspect.inspect(st, val) })
}

// ============================================================================
// $262.agent — real BEAM-process test262 agent cluster (harness host layer)
//
// $262.agent.* is test262 HOST machinery (INTERPRETING.md), so it lives in
// the harness — the embedder — not in the runtime: agent processes block on
// their BEAM mailboxes for broadcasts, acks and reports, and mailbox
// receives are embedder territory (see the [[CanBlock]] contract in
// arc/host_hooks.gleam). The harness hangs
// the `agent` object off every `$262` it installs; `$262.createRealm()`
// carries it over to child realms, and every spawned agent child installs
// its own.
//
// `$262.agent.start(script)` spawns a REAL BEAM child process
// (test262_exec_ffi.erl) that boots a completely fresh agent — its own
// store, intrinsics, globals, and $262 — compiles the (NOT IIFE-wrapped: the
// child owns its realm globals) agent source, executes it, drains its
// microtasks, and then parks in a broadcast loop.
//
// `broadcast(sab)` hands the SharedArrayBuffer's block to an owner process
// (`arc/rt/sab.share`) and ships its storage — the owner pid — to every
// child, blocking until each child acknowledges receipt (the ack is sent
// BEFORE the child invokes its receiveBroadcast callbacks, so a callback
// blocking in a sync Atomics.wait cannot deadlock broadcast). The child's
// reconstructed SAB aliases the very same block, so Atomics writes, waits
// and notifies cross agents. A plain ArrayBuffer travels as a byte copy.
//
// Wakes for an agent's `Atomics.waitAsync` waiters land in that agent's own
// mailbox and are taken by its microtask drain (`arc/rt/async.drain`), which
// also runs the timeout jobs. A child idling between broadcasts with a
// deadline-free waiter still pending receives the wake in its idle loop and
// hands it to the runtime (`rt_async.t_wake_waiter`) before draining.
//
// `report(str)` in a child posts the string to the parent's mailbox;
// `getReport()` in the parent drains that mailbox non-blockingly.
//
// All agent bookkeeping is agent state, held in hidden queues on the
// `$262.agent` object: __children__ holds the pids (host objects) of the
// agent processes `start` spawned, which `broadcast` hands to the FFI;
// __agents__ holds the receiveBroadcast callbacks a child's script
// registered (consumed by the child's own broadcast loop); __reports__
// backs report/getReport for the degenerate same-process case (the main
// agent reporting to itself). A child's parent pid is the argument its
// process body is spawned with, captured by the child's `report`.
// ============================================================================

/// Build the agent object and hang it off `$262` with builtin_property
/// attributes (enumerable:False), which matches the rest of the $262 surface
/// and keeps "agent" out of Object.keys($262).
fn extend_262_with_agent(
  s: HostState,
  dollar_262: Handle,
  parent: Option(AgentPid),
) -> HostState {
  let #(s, agent) = build_agent(s, parent)
  let #(prop, st) = common.builtin_property(s.agent, agent)
  let st = common.add_named_property(st, dollar_262, "agent", prop)
  host.State(..s, agent: st)
}

/// Allocate the $262.agent object: host-function methods plus three hidden
/// array-backed queues — __children__ (pids of the agent processes `start`
/// spawned, as host objects), __reports__ (strings posted by
/// $262.agent.report, consumed by getReport) and __agents__ (callbacks
/// registered by receiveBroadcast, invoked by the child's broadcast loop).
/// `report` captures `parent`, the pid a child posts its reports to.
fn build_agent(s: HostState, parent: Option(AgentPid)) -> #(HostState, JsVal) {
  let report = fn(args, this, s) { agent_report_native(args, this, s, parent) }
  let methods = [
    #("start", agent_start_native, 1),
    #("broadcast", agent_broadcast_native, 2),
    #("getReport", agent_get_report_native, 0),
    #("sleep", agent_sleep_native, 1),
    #("monotonicNow", agent_monotonic_now_native, 0),
    #("report", report, 1),
    #("leaving", agent_leaving_native, 0),
    #("receiveBroadcast", agent_receive_broadcast_native, 1),
  ]
  let #(s, method_props) =
    list.fold(methods, #(s, []), fn(acc, method) {
      let #(s, props) = acc
      let #(name, impl, arity) = method
      let #(s, f) = host.function(s, name, arity, impl)
      let #(prop, st) = common.builtin_property(s.agent, f)
      #(host.State(..s, agent: st), [#(name, prop), ..props])
    })
  let st = s.agent
  let array_proto = st.realm.array.prototype
  let #(children, st) = common.alloc_array(st, [], array_proto)
  let #(reports, st) = common.alloc_array(st, [], array_proto)
  let #(agents, st) = common.alloc_array(st, [], array_proto)
  let #(children_prop, st) = common.data_prop(st, mk_object(children))
  let #(reports_prop, st) = common.data_prop(st, mk_object(reports))
  let #(agents_prop, st) = common.data_prop(st, mk_object(agents))
  let hidden = [
    #("__children__", common.configurable(children_prop)),
    #("__reports__", common.configurable(reports_prop)),
    #("__agents__", common.configurable(agents_prop)),
  ]
  let #(h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: Ordinary,
        proto: Some(st.realm.object.prototype),
        props: common.named_props(list.append(method_props, hidden)),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  #(host.State(..s, agent: st), mk_object(h))
}

/// A BEAM agent child process pid (opaque — see test262_exec_ffi.erl).
type AgentPid

/// The term `broadcast` ships to each child process. A SharedArrayBuffer
/// travels as its `BufferStorage` AFTER `sab.share`, i.e. as the owner
/// process handle, so every receiver sees the same bytes; a plain
/// ArrayBuffer's storage is its byte image (a copy); primitives pass
/// through as-is — they are store-independent.
type AgentPayload {
  /// Shared-ness is derived from the storage variant (`buffer_is_shared`),
  /// not a separate flag.
  AgentSabPayload(storage: BufferStorage)
  AgentValuePayload(value: JsVal)
}

/// What woke an idle agent child process (see test262_exec_ffi.erl): a
/// parent broadcast, the owner wake of one of this agent's deadline-free
/// `Atomics.waitAsync` waiters, or the parent process dying.
type AgentWake {
  AgentWakeBroadcast(payload: AgentPayload)
  AgentWakeSab(ref: WaiterRef)
  AgentWakeParentDown
}

/// The `undefined` normal completion of a host function that only mutates
/// the agent.
fn done(s: HostState, st: Agent) -> #(HostState, Result(JsVal, JsVal)) {
  #(host.State(..s, agent: st), Ok(mk_undefined()))
}

/// $262.agent.start(script) — spawn a REAL BEAM child process that boots a
/// fresh agent and runs the agent script there. The source is NOT
/// IIFE-wrapped: the child has its own realm, so its top-level declarations
/// are its own realm globals (several tests start N agents with identical
/// scripts — separate realms keep them from colliding). The child's pid
/// joins this agent's __children__ list, which `broadcast` reads.
fn agent_start_native(
  args: List(JsVal),
  this: JsVal,
  s: HostState,
) -> #(HostState, Result(JsVal, JsVal)) {
  let #(source, st) = rt_val.t_to_string(s.agent, host.first_arg(args))
  let s = host.State(..s, agent: st)
  case agent_queue(s.agent, this, "__children__") {
    None -> host.type_error(s, "start: $262.agent state missing")
    Some(#(arr, children)) -> {
      let pid = ffi_spawn_agent(fn(parent) { run_agent_child(source, parent) })
      let #(s, child) = host.alloc_host_object(s, pid, None)
      done(s, agent_queue_write(s.agent, arr, list.append(children, [child])))
    }
  }
}

/// Child-process body: boot a fresh agent (own store/intrinsics/globals/
/// $262), compile + execute the agent script, drain, then park in the
/// broadcast loop until the parent broadcasts or goes away. Runs INSIDE the
/// spawned BEAM process — errors are reported to stderr, never thrown back
/// (there is no JS frame to throw into). `parent` is the pid of the agent
/// process that started this one: reports are posted to it, and its death
/// ends the loop.
fn run_agent_child(source: String, parent: AgentPid) -> Nil {
  let st = install_host_api(boot_base_agent(), Some(parent))
  // The child's $262.agent object — its __agents__ queue collects the
  // receiveBroadcast callbacks the script registers; the broadcast loop
  // below invokes them.
  let agent_this =
    {
      use dollar <- option.then(get_data(st, st.realm.global_object, "$262"))
      use dollar <- option.then(as_handle(dollar))
      get_data(st, dollar, "agent")
    }
    |> option.unwrap(mk_undefined())
  let compiled =
    ffi_run_compile_task(string.byte_size(source), fn() {
      case parser.parse_script(source) {
        Error(err) -> Error(parser.parse_error_to_string(err))
        Ok(#(body, sb)) ->
          compiler.compile_eval(body, sb) |> result.map_error(string.inspect)
      }
    })
  case compiled {
    Error(msg) ->
      io.println_error(
        "$262.agent.start: agent script did not compile: " <> msg,
      )
    Ok(template) -> {
      let #(completion, st) = entry.run_script(st, template)
      let Nil = case completion {
        ThrowCompletion(thrown) ->
          io.println_error(
            "$262.agent: agent script threw: "
            <> rt_inspect.format_error(st, thrown),
          )
        NormalCompletion(_) -> Nil
      }
      let st = settle_pending_wakes(st)
      agent_child_loop(st, agent_this, parent)
    }
  }
}

/// Child idle loop: block until the parent broadcasts (the receipt ack is
/// sent by await_broadcast_or_wake BEFORE we run any JS) or the wake of a
/// deadline-free waitAsync waiter arrives (waiters with a deadline never
/// reach here pending: the drain runs until they settle). A broadcast is
/// materialized in the child's store and handed to every registered
/// receiveBroadcast callback; a wake queues its waiter's resolve job; either
/// way the child then drains. Ends — and the child process exits — when
/// `parent`, the process that started this agent, goes away.
fn agent_child_loop(st: Agent, agent_this: JsVal, parent: AgentPid) -> Nil {
  case ffi_await_broadcast_or_wake(parent) {
    AgentWakeParentDown -> Nil
    AgentWakeSab(ref) ->
      agent_child_loop(
        settle_pending_wakes(rt_async.t_wake_waiter(st, ref)),
        agent_this,
        parent,
      )
    AgentWakeBroadcast(payload) -> {
      let #(st, msg) = payload_to_value(st, payload)
      let st = case agent_queue(st, agent_this, "__agents__") {
        Some(#(_arr, callbacks)) ->
          list.fold(callbacks, st, fn(st, cb) {
            case rt_call.t_call(st, cb, mk_undefined(), [msg]) {
              #(NormalCompletion(_), st) -> st
              #(ThrowCompletion(thrown), st) -> {
                io.println_error(
                  "$262.agent: broadcast callback threw: "
                  <> rt_inspect.format_error(st, thrown),
                )
                st
              }
            }
          })
        None -> st
      }
      agent_child_loop(settle_pending_wakes(st), agent_this, parent)
    }
  }
}

/// Rebuild a broadcast payload as a value in the child's store.
fn payload_to_value(st: Agent, payload: AgentPayload) -> #(Agent, JsVal) {
  case payload {
    AgentValuePayload(v) -> #(st, v)
    AgentSabPayload(storage:) -> {
      let proto = case types.buffer_is_shared(storage) {
        True -> st.realm.shared_array_buffer.prototype
        False -> st.realm.array_buffer.prototype
      }
      let #(h, st) =
        realm_ops.alloc_wrapper(st, ArrayBufferObj(storage:), proto)
      #(st, mk_object(h))
    }
  }
}

/// $262.agent.receiveBroadcast(callback) — register for the next broadcast.
fn agent_receive_broadcast_native(
  args: List(JsVal),
  this: JsVal,
  s: HostState,
) -> #(HostState, Result(JsVal, JsVal)) {
  let cb = host.first_arg(args)
  case agent_queue(s.agent, this, "__agents__") {
    Some(#(arr, callbacks)) ->
      done(s, agent_queue_write(s.agent, arr, list.append(callbacks, [cb])))
    None -> host.type_error(s, "receiveBroadcast: $262.agent state missing")
  }
}

/// $262.agent.broadcast(sab) — ship the buffer to every child agent process
/// in this agent's __children__ list and block until all of them have
/// RECEIVED it (test262 INTERPRETING.md). Children ack on receipt, before
/// invoking their receiveBroadcast callbacks, so a callback that immediately
/// blocks cannot deadlock the broadcaster.
fn agent_broadcast_native(
  args: List(JsVal),
  this: JsVal,
  s: HostState,
) -> #(HostState, Result(JsVal, JsVal)) {
  case agent_queue(s.agent, this, "__children__") {
    None -> host.type_error(s, "broadcast: $262.agent state missing")
    Some(#(_arr, children)) -> {
      let pids =
        list.filter_map(children, fn(child) {
          host.read_host(s, child) |> option.to_result(Nil)
        })
      case make_broadcast_payload(s.agent, host.first_arg(args)) {
        #(None, st) ->
          host.type_error(
            host.State(..s, agent: st),
            "$262.agent.broadcast: argument must be a (Shared)ArrayBuffer or a primitive",
          )
        #(Some(payload), st) -> {
          let Nil = ffi_broadcast(pids, payload)
          done(s, st)
        }
      }
    }
  }
}

/// Serialize a broadcast argument. A SharedArrayBuffer is first handed to
/// an owner process (`sab.share`), so the storage that travels is the owner
/// pid and every receiver aliases one block; a plain ArrayBuffer travels as
/// its bytes (a copy); primitives travel as-is; any other object has no
/// cross-store meaning, and a detached buffer has no storage to ship.
fn make_broadcast_payload(
  st: Agent,
  v: JsVal,
) -> #(Option(AgentPayload), Agent) {
  case classify(v) {
    KHandle(h) ->
      case buffer.buffer_storage(st, h) {
        Some(Detached(..)) | None -> #(None, st)
        Some(Shared(..)) -> {
          let #(_owner, st) = sab.share(st, h)
          #(option.map(buffer.buffer_storage(st, h), AgentSabPayload), st)
        }
        Some(storage) -> #(Some(AgentSabPayload(storage:)), st)
      }
    _ -> #(Some(AgentValuePayload(v)), st)
  }
}

/// $262.agent.report(value) — in a child agent process, post ToString(value)
/// to the mailbox of `parent` (the process that started it); in the main
/// agent (`parent` is None), push onto the local __reports__ queue
/// (degenerate self-report).
fn agent_report_native(
  args: List(JsVal),
  this: JsVal,
  s: HostState,
  parent: Option(AgentPid),
) -> #(HostState, Result(JsVal, JsVal)) {
  let #(str, st) = rt_val.t_to_string(s.agent, host.first_arg(args))
  case parent {
    Some(parent) -> {
      let Nil = ffi_send_report(parent, str)
      done(s, st)
    }
    None ->
      case agent_queue(st, this, "__reports__") {
        Some(#(arr, reports)) ->
          done(
            s,
            agent_queue_write(st, arr, list.append(reports, [mk_string(str)])),
          )
        None ->
          host.type_error(
            host.State(..s, agent: st),
            "report: $262.agent state missing",
          )
      }
  }
}

/// $262.agent.getReport() — dequeue the oldest report, or null when none is
/// pending. Local (same-process) reports first, then the mailbox of reports
/// posted by child agent processes.
fn agent_get_report_native(
  _args: List(JsVal),
  this: JsVal,
  s: HostState,
) -> #(HostState, Result(JsVal, JsVal)) {
  case agent_queue(s.agent, this, "__reports__") {
    Some(#(arr, reports)) ->
      case reports {
        [] ->
          case ffi_take_report() {
            Ok(report) -> #(s, Ok(mk_string(report)))
            Error(Nil) -> #(s, Ok(mk_null()))
          }
        [head, ..rest] -> #(
          host.State(..s, agent: agent_queue_write(s.agent, arr, rest)),
          Ok(head),
        )
      }
    None -> host.type_error(s, "getReport: $262.agent state missing")
  }
}

/// $262.agent.sleep(ms) — block the (single) BEAM scheduler thread running
/// this agent for ms milliseconds, through the host's `sleep_ms` hook.
fn agent_sleep_native(
  args: List(JsVal),
  _this: JsVal,
  s: HostState,
) -> #(HostState, Result(JsVal, JsVal)) {
  let #(num, st) = rt_val.t_to_number(s.agent, host.first_arg(args))
  let ms = case num {
    JInt(i) -> i
    JFloat(f) -> float.truncate(f)
    _ -> 0
  }
  let Nil = st.hooks.sleep_ms(ms)
  done(s, st)
}

/// $262.agent.monotonicNow() — monotonic milliseconds from the host's clock
/// hook (the same clock the runtime's own timing reads).
fn agent_monotonic_now_native(
  _args: List(JsVal),
  _this: JsVal,
  s: HostState,
) -> #(HostState, Result(JsVal, JsVal)) {
  #(s, Ok(mk_number(JInt(s.agent.hooks.monotonic_now()))))
}

/// $262.agent.leaving() — agent termination hint. The child process exits
/// when its parent goes away (parent monitor), so this is a no-op.
fn agent_leaving_native(
  _args: List(JsVal),
  _this: JsVal,
  s: HostState,
) -> #(HostState, Result(JsVal, JsVal)) {
  #(s, Ok(mk_undefined()))
}

/// Read a hidden object-valued own data property off the agent object.
fn agent_hidden_ref(st: Agent, this: JsVal, name: String) -> Option(Handle) {
  use this_h <- option.then(as_handle(this))
  case rt_obj.t_ordinary_own_property(st, this_h, StringKey(Named(name))) {
    Some(DataProperty(value:, ..)) -> as_handle(value)
    _ -> None
  }
}

/// Read an agent queue array as #(handle, values). None if missing.
fn agent_queue(
  st: Agent,
  this: JsVal,
  name: String,
) -> Option(#(Handle, List(JsVal))) {
  use arr <- option.then(agent_hidden_ref(st, this, name))
  case rt_store.t_cell_get(st, arr) {
    SObject(kind: ArrayObj(length:), elements: els, ..) -> {
      let values =
        int.range(from: length - 1, to: -1, with: [], run: fn(acc, i) {
          [elements.get(els, i), ..acc]
        })
      Some(#(arr, values))
    }
    _ -> None
  }
}

/// Overwrite an agent queue array's contents in place.
fn agent_queue_write(st: Agent, arr: Handle, values: List(JsVal)) -> Agent {
  case rt_store.t_cell_get(st, arr) {
    SObject(kind: ArrayObj(_), ..) as slot ->
      rt_store.t_cell_set(
        st,
        arr,
        SObject(
          ..slot,
          kind: ArrayObj(length: list.length(values)),
          elements: elements.from_list(values),
        ),
      )
    _ -> st
  }
}

// -- Agent FFI (test262_exec_ffi.erl) --

/// Spawn a child agent process running `body`, which is handed the pid of
/// this (the parent) process. Returns the child's pid.
@external(erlang, "test262_exec_ffi", "spawn_agent")
fn ffi_spawn_agent(_body: fn(AgentPid) -> Nil) -> AgentPid {
  panic as beam_only_test
}

/// Hand `payload` to every child in `pids` and block until each has acked
/// receipt (a child that died counts as having received it).
@external(erlang, "test262_exec_ffi", "broadcast")
fn ffi_broadcast(_pids: List(AgentPid), _payload: AgentPayload) -> Nil {
  panic as beam_only_test
}

/// Child side: block for the next broadcast, waitAsync wake, or the death
/// of `parent`.
@external(erlang, "test262_exec_ffi", "await_broadcast_or_wake")
fn ffi_await_broadcast_or_wake(_parent: AgentPid) -> AgentWake {
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

// -- Atomics: [[CanBlock]] and the waitAsync driver (harness as embedder) --

/// The harness's host hooks: [[CanBlock]] = true (§9.7), so its agents — the
/// main test agent and every `$262.agent` worker, each its own process — may
/// park in a sync `Atomics.wait`. A CanBlockIsFalse test strips it again in
/// `boot_agent`.
fn harness_host_hooks() -> host.HostHooks {
  HostHooks(..host.default_host_hooks(), can_block: True)
}

/// The harness's post-script driver: one microtask checkpoint through the
/// shared drain, so promise reactions (and the async $DONE protocol) settle
/// before an outcome is read. The drain is also what wakes and times out
/// `Atomics.waitAsync` waiters; it returns once the queue is dry and no
/// waiter with a deadline remains.
fn settle_pending_wakes(st: Agent) -> Agent {
  rt_async.drain(st)
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

// The agent/template caches share one persistent_term-backed store keyed by
// string; the two typed views below must use disjoint keys (agent_cache_key
// vs harness filenames) since FFI bypasses the type checker.

@external(erlang, "test262_exec_ffi", "cache_get")
fn agent_cache_get(_key: String) -> Option(Agent) {
  panic as beam_only_test
}

@external(erlang, "test262_exec_ffi", "cache_put")
fn agent_cache_put(_key: String, _agent: Agent) -> Nil {
  panic as beam_only_test
}

@external(erlang, "test262_exec_ffi", "cache_get")
fn template_cache_get(_key: String) -> Option(FuncTemplate) {
  panic as beam_only_test
}

@external(erlang, "test262_exec_ffi", "cache_put")
fn template_cache_put(_key: String, _template: FuncTemplate) -> Nil {
  panic as beam_only_test
}

@external(erlang, "test262_exec_ffi", "init_config")
fn init_config(
  _update_mode: Bool,
  _has_snapshot: Bool,
  _fail_log: Option(String),
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
fn get_fail_log() -> Option(String) {
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
