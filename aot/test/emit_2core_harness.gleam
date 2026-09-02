import arc/engine
import arc/host_hooks.{type ConsoleLevel, DebugLevel, InfoLevel, LogLevel}
import arc/internal/host_time
import arc/rt/types.{type Agent}
import arc_aot/emit as emit_2core
import arc_aot/run.{type RunResult}
import carder/pipeline
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/int
import gleam/list
import gleam/string

pub type DiffRun {
  DiffRun(stdout: BitArray, result: RunResult)
}

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

// the kinds of cache entry filled at sites `before` did not have
pub fn ic_kinds(st: Agent, before: Agent) -> List(String) {
  dict.to_list(st.store.ics)
  |> list.filter(fn(p) { !dict.has_key(before.store.ics, p.0) })
  |> list.map(fn(p) {
    case p.1 {
      types.IcRead(..) -> "read"
      types.IcCall(..) -> "call"
      types.IcInit(..) -> "init"
      types.IcGlobal(..) -> "global"
      types.IcOff -> "off"
    }
  })
}

pub const fixed_now_ms = 1_700_000_000_000

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

pub fn seed_random(seed: Int) -> Nil {
  do_seed_random(seed)
}

pub fn buf_reset() -> Nil {
  do_buf_reset()
}

pub fn buf_read() -> BitArray {
  do_buf_read()
}

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

pub fn seed() -> Agent {
  run.seed(test_hooks())
}

pub fn run_loaded(module: Atom, st: Agent) -> #(Agent, DiffRun) {
  buf_reset()
  let #(st, result) = run.run_loaded(module, st)
  #(st, DiffRun(stdout: buf_read(), result:))
}

pub fn load_compiled(source: String) -> Result(Atom, String) {
  let mod_name = "arc_emit2c_test_" <> int.to_string(unique_integer([Positive]))
  let opts =
    emit_2core.CompileOpts(
      module_name: mod_name,
      source_kind: emit_2core.AsScript,
      entry_name: "js_main",
    )
  case emit_2core.compile_source(source, opts) {
    Error(e) -> Error(string.inspect(e))
    Ok(unit) ->
      case pipeline.compile_ir(unit.module, emit_2core.binding()) {
        Error(e) -> Error(string.inspect(e))
        Ok(beam) ->
          case run.load(beam, mod_name) {
            Error(reason) -> Error("load failed: " <> reason)
            Ok(module) -> Ok(module)
          }
      }
  }
}

pub fn run_compiled(source: String) -> DiffRun {
  case load_compiled(source) {
    Error(reason) -> DiffRun(stdout: <<>>, result: Error(reason))
    Ok(module) -> run_loaded(module, seed()).1
  }
}

type UniqueOpt {
  Positive
}

@external(erlang, "erlang", "unique_integer")
fn unique_integer(opts: List(UniqueOpt)) -> Int
