//// M20 differential-test harness: run the same JS source through the arc
//// interpreter and through emit_2core→ir_to_core→BEAM, and compare console
//// output byte-for-byte. `run_compiled` / `run_interpreted` (added by sibling
//// units) both return `DiffRun` so the test file can `assert c.stdout ==
//// i.stdout` without caring which path produced it.

import arc/compiler/emit_2core
import arc/engine
import arc/vm/host_hooks
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/string
import twocore/pipeline.{type DiffRun, DiffRun}
import twocore/runtime/profiles
import twocore/runtime/rt_js_types

// ----------------------------------------------------------------------------
// Interpreter oracle
// ----------------------------------------------------------------------------

/// Run `source` through arc's tree-walking interpreter and capture stdout.
///
/// arc's `console.log` writes straight to `io.println` (see
/// `arc/vm/builtins/console.gleam`) — there is no `print` host hook — so
/// stdout is captured by temporarily installing an Erlang group_leader
/// collector around the eval (`emit_2core_harness_ffi:capture_stdout/1`).
/// `engine.Outcome` maps onto `DiffRun.result`: `Returned(v)` → `Ok(v as
/// Dynamic)`, `Threw(e)` → `Error(format_error(e))`; a parse/compile
/// failure is also `Error`.
pub fn run_interpreted(source: String) -> DiffRun {
  let #(stdout, eval_result) =
    capture_stdout(fn() {
      let eng: engine.Engine(Nil) =
        engine.new() |> engine.with_host_hooks(arc_test_hooks())
      engine.eval(eng, source)
    })
  let result = case eval_result {
    Ok(#(engine.Returned(value:), _eng)) -> Ok(to_dynamic(value))
    Ok(#(engine.Threw(error:), eng)) -> Error(engine.format_error(eng, error))
    Error(err) -> Error(engine.eval_error_message(err))
  }
  DiffRun(stdout:, result:)
}

@external(erlang, "emit_2core_harness_ffi", "capture_stdout")
fn capture_stdout(thunk: fn() -> a) -> #(BitArray, a)

@external(erlang, "emit_2core_harness_ffi", "to_dynamic")
fn to_dynamic(a: a) -> Dynamic

// ----------------------------------------------------------------------------
// Deterministic host hooks (SPEC §20)
// ----------------------------------------------------------------------------

/// The fixed monotonic-clock reading both runtimes report for `Date.now` /
/// `performance.now`. sum(n)/makeAdder never read the clock, but SPEC§20
/// mandates the constructors exist so time-sensitive fixtures added later
/// diff cleanly.
pub const fixed_now_ms = 1_700_000_000_000

/// Deterministic `arc/vm/host_hooks.HostHooks` for the interpreter oracle.
/// arc's `HostHooks` has no `random`/`print` fields (arc's `console.log`
/// is `io.println`, arc's `Math.random` is not hook-driven), so only the
/// clock, sleep and uncaught-report sinks are virtualised here.
pub fn arc_test_hooks() -> host_hooks.HostHooks {
  host_hooks.HostHooks(
    ..host_hooks.default_host_hooks(),
    monotonic_now: fn() { fixed_now_ms },
    sleep_ms: fn(_) { Nil },
    report_uncaught: buf_push,
  )
}

/// Deterministic `twocore/runtime/rt_js_types.HostHooks` for the compiled
/// path. `random` is a seeded xorshift64* stepped in the process
/// dictionary; `print` appends to the same process-local buffer, newline-
/// terminated to match arc's `io.println` bytes.
pub fn twocore_test_hooks() -> rt_js_types.HostHooks {
  rt_js_types.HostHooks(
    monotonic_now: fn() { fixed_now_ms },
    random: next_random,
    sleep_ms: fn(_) { Nil },
    print: buf_push,
  )
}

/// Re-seed the deterministic PRNG behind `twocore_test_hooks().random`.
/// Call at the top of a fixture that needs a known `Math.random` sequence.
pub fn seed_random(seed: Int) -> Nil {
  do_seed_random(seed)
}

/// Clear the process-local print buffer. `run_compiled` calls this before
/// each fixture so consecutive tests do not see each other's console lines.
pub fn buf_reset() -> Nil {
  do_buf_reset()
}

/// Read the process-local print buffer as one contiguous BitArray in
/// emission order — the compiled path's `DiffRun.stdout`.
pub fn buf_read() -> BitArray {
  do_buf_read()
}

@external(erlang, "emit_2core_harness_ffi", "next_random")
fn next_random() -> Float

@external(erlang, "emit_2core_harness_ffi", "seed_random")
fn do_seed_random(seed: Int) -> Nil

@external(erlang, "emit_2core_harness_ffi", "buf_push")
fn buf_push(line: String) -> Nil

@external(erlang, "emit_2core_harness_ffi", "buf_reset")
fn do_buf_reset() -> Nil

@external(erlang, "emit_2core_harness_ffi", "buf_read")
fn do_buf_read() -> BitArray

// ----------------------------------------------------------------------------
// Compiled path (emit_2core → 2core IR → Core Erlang → BEAM)
// ----------------------------------------------------------------------------

/// Run `source` through emit_2core → `pipeline.compile_ir(js_direct())` →
/// BEAM and return the captured console bytes + completion result. Any
/// compile-stage failure (parse/emit/lower/build) is folded into
/// `Error(string.inspect(e))` with empty `stdout` so the differential test
/// still gets two structurally comparable `DiffRun`s. Each call mints a
/// fresh module atom so the BEAM code server never sees a re-register.
pub fn run_compiled(source: String) -> DiffRun {
  buf_reset()
  let mod_name =
    "arc_emit2c_test_" <> int.to_string(unique_integer([Positive]))
  let opts =
    emit_2core.CompileOpts(
      module_name: mod_name,
      source_kind: emit_2core.AsScript,
      entry_name: "js_main",
    )
  case emit_2core.compile_source(source, opts) {
    Error(e) -> DiffRun(stdout: <<>>, result: Error(string.inspect(e)))
    Ok(unit) ->
      case pipeline.compile_ir(unit.module, profiles.js_direct()) {
        Error(e) -> DiffRun(stdout: <<>>, result: Error(string.inspect(e)))
        Ok(beam) -> {
          let #(_st, run) =
            pipeline.run_js_beam(beam, mod_name, twocore_test_hooks())
          run
        }
      }
  }
}

type UniqueOpt {
  Positive
}

@external(erlang, "erlang", "unique_integer")
fn unique_integer(opts: List(UniqueOpt)) -> Int
