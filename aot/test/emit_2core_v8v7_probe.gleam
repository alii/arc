import arc/rt/types.{type Agent}
import arc_aot/emit as emit_2core
import arc_aot/run
import carder/pipeline
import emit_2core_harness as harness
import gleam/erlang/atom.{type Atom}
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import simplifile

type TimeUnit {
  Microsecond
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(unit: TimeUnit) -> Int

fn time_us(f: fn() -> a) -> #(Int, a) {
  let t0 = monotonic_time(Microsecond)
  let r = f()
  #(monotonic_time(Microsecond) - t0, r)
}

const budget_us = 5_000_000

fn reps_for(warm: Int) -> Int {
  case warm {
    0 -> 200
    _ -> int.min(200, int.max(3, budget_us / warm))
  }
}

type Outcome {
  CompileFailed(stage: String, err: String)
  RunFailed(err: String, stdout: String)
  Measured(per_us: Int, reps: Int)
}

@external(erlang, "emit_2core_probe_ffi", "env_int")
fn env_int(name: String, default: Int) -> Int

@external(erlang, "emit_2core_probe_ffi", "load_average")
fn load_average() -> String

fn batches() -> Int {
  env_int("PROBE_N", 3)
}

fn min_of_n(reps: Int, f: fn() -> a) -> Int {
  list.repeat(Nil, batches())
  |> list.map(fn(_) { time_us(fn() { repeat(reps, f) }).0 / reps })
  |> list.fold(-1, fn(best, us) {
    case best {
      -1 -> us
      _ -> int.min(best, us)
    }
  })
}

fn repeat(n: Int, f: fn() -> a) -> Nil {
  case n {
    0 -> Nil
    _ -> {
      let _ = f()
      repeat(n - 1, f)
    }
  }
}

type Loaded {
  Loaded(mod: Atom, seed: Agent)
}

fn seed_realm() -> Agent {
  harness.seed()
}

fn run_once(loaded: Loaded) -> harness.DiffRun {
  harness.run_loaded(loaded.mod, loaded.seed).1
}

fn bench_compiled(name: String, source: String) -> Outcome {
  let mod_name = "arc_v8v7_" <> name
  let opts =
    emit_2core.CompileOpts(
      module_name: mod_name,
      source_kind: emit_2core.AsScript,
      entry_name: "js_main",
    )
  let #(emit_us, emit_r) =
    time_us(fn() { emit_2core.compile_source(source, opts) })
  case emit_r {
    Error(e) -> CompileFailed("emit_2core", string.inspect(e))
    Ok(unit) -> {
      let #(lower_us, lower_r) =
        time_us(fn() { pipeline.compile_ir(unit.module, emit_2core.binding()) })
      io.println(
        "  compile: emit_2core="
        <> int.to_string(emit_us)
        <> "µs ir_to_beam="
        <> int.to_string(lower_us)
        <> "µs → "
        <> case lower_r {
          Ok(_) -> "OK (BEAM produced)"
          Error(e) -> "FAILED: " <> string.slice(string.inspect(e), 0, 200)
        },
      )
      case lower_r {
        Error(e) -> CompileFailed("ir_to_beam", string.inspect(e))
        Ok(beam) ->
          case run.load(beam, mod_name) {
            Error(e) -> CompileFailed("beam_load", e)
            Ok(mod) -> {
              let #(realm_us, seed) = time_us(seed_realm)
              io.println(
                "  realm-init: " <> int.to_string(realm_us) <> "µs (once)",
              )
              let loaded = Loaded(mod:, seed:)
              let #(warm_us, first) = time_us(fn() { run_once(loaded) })
              case first {
                harness.DiffRun(result: Error(e), stdout:) ->
                  RunFailed(e, string.inspect(stdout))
                harness.DiffRun(result: Ok(_), stdout:) ->
                  case stdout {
                    <<"ok\n":utf8>> -> {
                      let reps = reps_for(warm_us)
                      Measured(min_of_n(reps, fn() { run_once(loaded) }), reps)
                    }
                    _ -> RunFailed("stdout mismatch", string.inspect(stdout))
                  }
              }
            }
          }
      }
    }
  }
}

fn bench_interp(source: String) -> Outcome {
  let #(warm_us, first) = time_us(fn() { harness.run_interpreted(source) })
  case first {
    harness.DiffRun(result: Error(e), stdout:) ->
      RunFailed(e, string.inspect(stdout))
    harness.DiffRun(result: Ok(_), stdout:) ->
      case stdout {
        <<"ok\n":utf8>> -> {
          let reps = reps_for(warm_us)
          Measured(
            min_of_n(reps, fn() { harness.run_interpreted(source) }),
            reps,
          )
        }
        _ -> RunFailed("stdout mismatch", string.inspect(stdout))
      }
  }
}

fn show(o: Outcome) -> String {
  case o {
    Measured(us, r) -> int.to_string(us) <> " µs (×" <> int.to_string(r) <> ")"
    CompileFailed(stage, e) -> "COMPILE FAIL [" <> stage <> "]: " <> e
    RunFailed(e, out) -> "RUN FAIL: " <> e <> " | stdout=" <> out
  }
}

fn per_us(o: Outcome) -> Int {
  case o {
    Measured(us, _) -> us
    _ -> -1
  }
}

fn ext_refs(name: String) -> #(Int, Int) {
  // #(qjs, bun-llint)
  case name {
    "richards" -> #(1917, 1744)
    "deltablue" -> #(3825, 3183)
    "crypto" -> #(51_323, 34_054)
    "raytrace" -> #(19_477, 14_543)
    _ -> #(-1, -1)
  }
}

fn one(name: String) {
  let path = "../bench/v8-v7/" <> name <> "_run.js"
  io.println("")
  io.println("═══ " <> name <> " (" <> path <> ") ═══")
  case simplifile.read(path) {
    Error(e) -> io.println("!! read failed: " <> string.inspect(e))
    Ok(source) -> {
      io.println(
        "  source: " <> int.to_string(string.length(source)) <> " chars",
      )
      let compiled = bench_compiled(name, source)
      io.println("  emit_2core : " <> show(compiled))
      let interp = case harness.env_is_truthy("PROBE_SKIP_INTERP") {
        True -> Measured(0, 0)
        False -> bench_interp(source)
      }
      io.println("  arc-interp : " <> show(interp))
      let #(qjs, llint) = ext_refs(name)
      let load = load_average()
      io.println("  ref qjs    : " <> int.to_string(qjs) <> " µs")
      io.println("  ref llint  : " <> int.to_string(llint) <> " µs")
      io.println("  load avg   : " <> load)
      io.println(
        "  ROW "
        <> name
        <> "\t"
        <> int.to_string(per_us(compiled))
        <> "\t"
        <> int.to_string(per_us(interp))
        <> "\t"
        <> int.to_string(qjs)
        <> "\t"
        <> int.to_string(llint)
        <> "\t"
        <> load,
      )
    }
  }
}

pub fn main() {
  io.println(
    "emit_2core V8-v7 probe — one call = one full bench iteration, min-of-"
    <> int.to_string(batches())
    <> " batches (PROBE_N)",
  )
  io.println("  ROW bench\temit_2core\tarc_interp\tqjs\tllint\tload_avg")
  one("richards")
  one("deltablue")
  one("crypto")
  one("raytrace")
}
