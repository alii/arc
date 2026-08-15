//// M20 differential-test harness: run the same JS source through the arc
//// interpreter and through emit_2core→ir_to_core→BEAM, and compare console
//// output byte-for-byte. `run_compiled` / `run_interpreted` both return
//// `DiffRun` so the test file can `assert c.stdout == i.stdout` without
//// caring which path produced it.

import arc/engine
import arc/host_hooks.{type ConsoleLevel, DebugLevel, InfoLevel, LogLevel}
import arc/internal/host_time
import arc/rt/types.{type Agent}
import arc_aot/emit as emit_2core
import arc_aot/run.{type RunResult}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/int
import gleam/string
import twocore/pipeline

/// Console stdout bytes in emission order plus how the top level completed.
pub type DiffRun {
  DiffRun(stdout: BitArray, result: RunResult)
}

// ----------------------------------------------------------------------------
// Interpreter oracle
// ----------------------------------------------------------------------------

/// Run `source` through arc's interpreter on `test_hooks()`, so its
/// `console` lines land in the same process-local buffers the compiled path
/// fills, and read the stdout buffer back. `engine.Outcome` maps onto
/// `DiffRun.result`: `Returned(v)` → `Ok(v as Dynamic)`, `Threw(e)` →
/// `Error(format_error(e))`; a parse/compile failure is also `Error`.
pub fn run_interpreted(source: String) -> DiffRun {
  buf_reset()
  let eng: engine.Engine(Nil) =
    engine.new() |> engine.with_host_hooks(test_hooks())
  let result = case engine.eval(eng, source) {
    Ok(#(engine.Returned(value:), _eng)) -> Ok(to_dynamic(value))
    Ok(#(engine.Threw(error:), eng)) -> Error(engine.format_error(eng, error))
    Error(err) -> Error(engine.eval_error_message(err))
  }
  DiffRun(stdout: buf_read(), result:)
}

@external(erlang, "emit_2core_harness_ffi", "to_dynamic")
fn to_dynamic(a: a) -> Dynamic

// ----------------------------------------------------------------------------
// Deterministic host hooks (SPEC §20)
// ----------------------------------------------------------------------------

/// The fixed clock reading both runtimes report for `Date.now` /
/// `performance.now`.
pub const fixed_now_ms = 1_700_000_000_000

/// Deterministic hooks for both paths: fixed clocks in UTC, no sleep, a seeded
/// xorshift64* PRNG, console lines into the process-local stdout buffer
/// (log/info/debug, newline-terminated to match `io.println` bytes) or the
/// stderr buffer (warn/error), and uncaught-job reports into the stderr
/// buffer.
pub fn test_hooks() -> host_hooks.HostHooks {
  host_hooks.HostHooks(
    ..host_hooks.default_host_hooks(),
    monotonic_now: fn() { fixed_now_ms },
    wall_clock_ms: fn() { fixed_now_ms },
    time_zone: host_time.utc_time_zone(),
    sleep_ms: fn(_) { Nil },
    random: next_random,
    print: buf_print,
    report_uncaught: err_push,
  )
}

fn buf_print(level: ConsoleLevel, line: String) -> Nil {
  case level {
    LogLevel | InfoLevel | DebugLevel -> buf_push(line)
    _ -> err_push(line)
  }
}

/// Re-seed the deterministic PRNG behind `test_hooks().random`.
/// Call at the top of a fixture that needs a known `Math.random` sequence.
pub fn seed_random(seed: Int) -> Nil {
  do_seed_random(seed)
}

/// Clear both process-local buffers. `run_compiled` calls this before each
/// fixture so consecutive tests do not see each other's console lines.
pub fn buf_reset() -> Nil {
  do_buf_reset()
}

/// Read the stdout buffer as one contiguous BitArray in emission order —
/// the compiled path's `DiffRun.stdout`.
pub fn buf_read() -> BitArray {
  do_buf_read()
}

/// Read the stderr buffer (console.warn/error lines and uncaught-job
/// reports) in emission order.
pub fn err_read() -> BitArray {
  do_err_read()
}

@external(erlang, "emit_2core_harness_ffi", "next_random")
fn next_random() -> Float

@external(erlang, "emit_2core_harness_ffi", "seed_random")
fn do_seed_random(seed: Int) -> Nil

@external(erlang, "emit_2core_harness_ffi", "buf_push")
fn buf_push(line: String) -> Nil

@external(erlang, "emit_2core_harness_ffi", "err_push")
fn err_push(line: String) -> Nil

@external(erlang, "emit_2core_harness_ffi", "buf_reset")
fn do_buf_reset() -> Nil

@external(erlang, "emit_2core_harness_ffi", "buf_read")
fn do_buf_read() -> BitArray

@external(erlang, "emit_2core_harness_ffi", "err_read")
fn do_err_read() -> BitArray

@external(erlang, "emit_2core_harness_ffi", "env_is_truthy")
pub fn env_is_truthy(name: String) -> Bool

// ----------------------------------------------------------------------------
// Compiled path (emit_2core → 2core IR → Core Erlang → BEAM)
// ----------------------------------------------------------------------------

/// A seeded agent on `test_hooks`.
pub fn seed() -> Agent {
  run.seed(test_hooks())
}

/// Apply a loaded module's `js_main` from `st` with fresh buffers and
/// package the captured stdout as a `DiffRun`.
pub fn run_loaded(module: Atom, st: Agent) -> #(Agent, DiffRun) {
  buf_reset()
  let #(st, result) = run.run_loaded(module, st)
  #(st, DiffRun(stdout: buf_read(), result:))
}

/// Run `source` through emit_2core → `pipeline.compile_ir(emit.binding())`
/// → BEAM and return the captured console bytes + completion result. Any
/// compile-stage failure (parse/emit/lower/build) is folded into
/// `Error(string.inspect(e))` with empty `stdout` so the differential test
/// still gets two structurally comparable `DiffRun`s. Each call mints a
/// fresh module atom so the BEAM code server never sees a re-register.
pub fn run_compiled(source: String) -> DiffRun {
  let mod_name = "arc_emit2c_test_" <> int.to_string(unique_integer([Positive]))
  let opts =
    emit_2core.CompileOpts(
      module_name: mod_name,
      source_kind: emit_2core.AsScript,
      entry_name: "js_main",
    )
  case emit_2core.compile_source(source, opts) {
    Error(e) -> DiffRun(stdout: <<>>, result: Error(string.inspect(e)))
    Ok(unit) ->
      case pipeline.compile_ir(unit.module, emit_2core.binding()) {
        Error(e) -> DiffRun(stdout: <<>>, result: Error(string.inspect(e)))
        Ok(beam) ->
          case run.load(beam, mod_name) {
            Error(reason) ->
              DiffRun(stdout: <<>>, result: Error("load failed: " <> reason))
            Ok(module) -> run_loaded(module, seed()).1
          }
      }
  }
}

type UniqueOpt {
  Positive
}

@external(erlang, "erlang", "unique_integer")
fn unique_integer(opts: List(UniqueOpt)) -> Int
