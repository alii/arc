//// emit_2core end-to-end benchmark: JS source → emit_2core → 2core IR →
//// Core Erlang → BEAM, timed against the arc tree-walking interpreter and
//// the Milestone-0 hand-written-IR baselines. Unlike `emit_2core_harness.
//// run_compiled`, this SEPARATES compile+load+realm-init (once) from the
//// hot invoke loop (repeated), so the reported per-call number is the pure
//// generated-code cost — directly comparable to `milestone0_test`'s
//// `exec_beam` figure and to native Gleam.
////
//// Run standalone (NOT via `gleam test` — the arc runner is parallel and
//// has a 10s per-test cap):
////
////     cd arc && gleam run -m emit_2core_bench

import arc/compiler/emit_2core
import arc/engine
import emit_2core_harness as harness
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/io
import gleam/string
import twocore/backend/build_beam
import twocore/pipeline
import twocore/runtime/profiles
import twocore/runtime/rt_js_builtins
import twocore/runtime/rt_js_store
import twocore/runtime/rt_state.{type InstanceState}

// arc has gleam_erlang only transitively (via twocore); the import
// warning is expected and harmless for a bench-only module.
import gleam/erlang/atom.{type Atom}

type TimeUnit {
  Microsecond
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(unit: TimeUnit) -> Int

@external(erlang, "emit_2core_harness_ffi", "to_dynamic")
fn to_dynamic(a: a) -> Dynamic

// ───────────────────────────── configuration ─────────────────────────────

const n = 1_000_000

/// Per-bench wall-clock budget for the timed repeat loop. The repeat count
/// is derived from a warm-up call so a slow kernel (makeAdder ≈1.2s) does
/// not blow the run out to minutes.
const budget_us = 3_000_000

// ───────────────────────────── JS sources ─────────────────────────────
// n is baked in — the compiled entry is a zero-arg Script whose completion
// value is the final expression. The same text feeds qjs/bun/LLInt below.

pub const sum_js = "let s=0;for(let i=0;i<1000000;i++)s+=i;s"

pub const adder_js = "function makeAdder(x){return function(y){return x+y}}let add5=makeAdder(5);let s=0;for(let i=0;i<1000000;i++)s+=add5(i);s"

pub const obj_js = "let o={x:0};for(let i=0;i<1000000;i++)o.x=o.x+i;let s=o.x;s"

// ───────────────────────────── compiled path ─────────────────────────────

/// A compiled+loaded JS script plus the seeded realm state each apply
/// starts from. `InstanceState` is a pure threaded record (no process-
/// dictionary state — see rt_state.fresh_full), so the SAME `seed` is
/// passed to every `apply_js_main` and each run observes an identical
/// fresh realm.
type Loaded {
  Loaded(mod: Atom, seed: InstanceState)
}

fn compile_load(source: String, name: String) -> #(Int, Int, Loaded) {
  let opts =
    emit_2core.CompileOpts(
      module_name: name,
      source_kind: emit_2core.AsScript,
      entry_name: "js_main",
    )
  let #(compile_us, beam) =
    time_us(fn() {
      let assert Ok(unit) = emit_2core.compile_source(source, opts)
      let assert Ok(beam) =
        pipeline.compile_ir(unit.module, profiles.js_direct())
      beam
    })
  let assert Ok(mod) = build_beam.load_module(atom.create(name), name, beam)
  let #(realm_us, seed) =
    time_us(fn() {
      let st =
        rt_state.fresh_full(
          rt_state.FullDecl(mems: [], globals: [], tables: [], ref_globals: []),
        )
      let st =
        rt_state.t_with_js_store(
          st,
          rt_js_store.t_store_new(harness.twocore_test_hooks()),
        )
      let #(_realm, st) = rt_js_builtins.init_realm(st)
      st
    })
  #(compile_us, realm_us, Loaded(mod:, seed:))
}

/// Re-declared FFI (private in `pipeline`): apply the loaded module's
/// `js_main(st, frame, [])` under a protected try. Returns
/// `#(JsExecOutcome, st')`; only the completion value is inspected here.
@external(erlang, "twocore_rt_js_exec_ffi", "apply_js_main")
fn ffi_apply_js_main(mod: Atom, st: InstanceState) -> #(Dynamic, InstanceState)

fn run_once(loaded: Loaded) -> Dynamic {
  let #(outcome, _st) = ffi_apply_js_main(loaded.mod, loaded.seed)
  outcome
}

// ───────────────────────────── interpreter path ─────────────────────────────
// Full engine.eval each call (parse + tree-walk). Parse is negligible next
// to a 1M-iteration walk, so this is a fair "interpreter run" number.

fn interp_once(source: String) -> Dynamic {
  let eng: engine.Engine(Nil) = engine.new()
  let assert Ok(#(engine.Returned(value:), _)) = engine.eval(eng, source)
  to_dynamic(value)
}

// ───────────────────────────── native Gleam baselines ─────────────────────────────

fn native_sum(i: Int, s: Int, lim: Int) -> Int {
  case i < lim {
    True -> native_sum(i + 1, s + i, lim)
    False -> s
  }
}

fn native_adder(i: Int, s: Int, lim: Int, f: fn(Int) -> Int) -> Int {
  case i < lim {
    True -> native_adder(i + 1, s + f(i), lim, f)
    False -> s
  }
}

// ───────────────────────────── timing ─────────────────────────────

fn time_us(f: fn() -> a) -> #(Int, a) {
  let t0 = monotonic_time(Microsecond)
  let r = f()
  #(monotonic_time(Microsecond) - t0, r)
}

fn repeat_n(times: Int, f: fn() -> a, last: a) -> a {
  case times {
    0 -> last
    _ -> repeat_n(times - 1, f, f())
  }
}

fn per(us: Int, reps: Int) -> String {
  int.to_string(us / reps) <> " µs"
}

/// Pick a repeat count that keeps `f`'s total under `budget_us`, given a
/// single warm-up timing. Minimum 3, maximum 200.
fn reps_for(warm_us: Int) -> Int {
  let r = case warm_us {
    0 -> 200
    _ -> budget_us / warm_us
  }
  int.min(200, int.max(3, r))
}

// ───────────────────────────── one bench row ─────────────────────────────

type Row {
  Row(
    name: String,
    compile_us: Int,
    realm_us: Int,
    compiled_per_us: Int,
    compiled_reps: Int,
    interp_per_us: Int,
    interp_reps: Int,
    native_per_us: Int,
    correctness: String,
  )
}

fn bench(
  name: String,
  source: String,
  native: fn() -> Int,
  expected: Int,
) -> Row {
  let mod = "arc_emit2c_bench_" <> name
  let #(compile_us, realm_us, loaded) = compile_load(source, mod)

  // correctness via console.log — the compiled Script's completion value is
  // Undefined (emit_2core does not thread ExpressionStatement completions),
  // so verify by stdout using the harness once, then time the print-free source.
  let logged = source <> ";console.log(s)"
  let want = <<{ int.to_string(expected) <> "\n" }:utf8>>
  let c = harness.run_compiled(logged)
  let i = harness.run_interpreted(logged)
  let correctness = case c.stdout == want, i.stdout == want, c.result {
    True, True, _ -> "ok"
    _, _, Error(e) -> "FAIL compiled: " <> e
    cok, iok, _ ->
      "FAIL compiled="
      <> string.inspect(c.stdout)
      <> " interp="
      <> string.inspect(i.stdout)
      <> " (c="
      <> string.inspect(cok)
      <> " i="
      <> string.inspect(iok)
      <> ")"
  }

  // adaptive repeat, warm-up first
  let #(cw_us, _) = time_us(fn() { run_once(loaded) })
  let compiled_reps = reps_for(cw_us)
  let #(compiled_us, _) =
    time_us(fn() {
      repeat_n(compiled_reps, fn() { run_once(loaded) }, dynamic.nil())
    })

  let #(iw_us, _) = time_us(fn() { interp_once(source) })
  let interp_reps = reps_for(iw_us)
  let #(interp_us, _) =
    time_us(fn() {
      repeat_n(interp_reps, fn() { interp_once(source) }, dynamic.nil())
    })

  let #(native_us, _) = time_us(fn() { repeat_n(200, native, 0) })

  Row(
    name:,
    compile_us:,
    realm_us:,
    compiled_per_us: compiled_us / compiled_reps,
    compiled_reps:,
    interp_per_us: interp_us / interp_reps,
    interp_reps:,
    native_per_us: native_us / 200,
    correctness:,
  )
}

// ───────────────────────────── report ─────────────────────────────

fn print_row(r: Row) {
  io.println("")
  io.println("[bench] " <> r.name <> "  (n=" <> int.to_string(n) <> ")")
  io.println("  correctness         : " <> r.correctness)
  io.println(
    "  compile (JS→beam)   : " <> int.to_string(r.compile_us) <> " µs (once)",
  )
  io.println(
    "  realm init          : " <> int.to_string(r.realm_us) <> " µs (once)",
  )
  io.println(
    "  emit_2core compiled : "
    <> per(r.compiled_per_us, 1)
    <> "/call  ×"
    <> int.to_string(r.compiled_reps),
  )
  io.println(
    "  arc interpreter     : "
    <> per(r.interp_per_us, 1)
    <> "/call  ×"
    <> int.to_string(r.interp_reps),
  )
  io.println(
    "  native gleam        : " <> per(r.native_per_us, 1) <> "/call  ×200",
  )
  io.println(
    "  compiled/native     : "
    <> ratio(r.compiled_per_us, r.native_per_us)
    <> "x",
  )
  io.println(
    "  interp/compiled     : "
    <> ratio(r.interp_per_us, r.compiled_per_us)
    <> "x",
  )
}

fn ratio(a: Int, b: Int) -> String {
  case b <= 0 {
    True -> "?"
    False -> {
      let t = a * 10 / b
      int.to_string(t / 10) <> "." <> int.to_string(t % 10)
    }
  }
}

pub fn main() {
  io.println(
    "emit_2core benchmark — compiled path vs arc interpreter vs native",
  )
  io.println(
    "compiled = compile ONCE, seed realm ONCE, apply js_main × adaptive",
  )

  let sum = bench("sum", sum_js, fn() { native_sum(0, 0, n) }, 499_999_500_000)
  let adder =
    bench(
      "makeAdder",
      adder_js,
      fn() { native_adder(0, 0, n, fn(y) { 5 + y }) },
      500_004_500_000,
    )
  let obj =
    bench("obj_prop", obj_js, fn() { native_sum(0, 0, n) }, 499_999_500_000)

  print_row(sum)
  print_row(adder)
  print_row(obj)

  // Milestone-0 hand-IR figures (this machine, 2core `gleam test` — see
  // twocore/milestone0_test.gleam). Re-measure there if the host changes.
  let m0_sum_us = 994
  let m0_adder_us = 2405

  io.println("")
  io.println("── summary (µs per 1M-iteration call) ──")
  io.println(
    "bench      emit_2core   arc-interp   m0-hand-IR   native-gleam   "
    <> "compiled/m0",
  )
  summary("sum      ", sum, m0_sum_us)
  summary("makeAdder", adder, m0_adder_us)
  summary("obj_prop ", obj, -1)
  io.println("")
  io.println("m0-hand-IR = twocore/milestone0_test.gleam on this machine.")
  io.println(
    "External refs (bench_driver.js): "
    <> "qjs sum=8639 adder=18764 obj=11543; "
    <> "bun-jit sum=254 adder=524 obj=651; "
    <> "bun-llint sum=3787 adder=20245 obj=6827",
  )
}

fn summary(label: String, r: Row, m0: Int) {
  let m0_s = case m0 {
    -1 -> "     n/a"
    _ -> pad(m0)
  }
  let vs_m0 = case m0 {
    -1 -> "n/a"
    _ -> ratio(r.compiled_per_us, m0) <> "x"
  }
  let compiled_s = case r.correctness {
    "ok" -> pad(r.compiled_per_us)
    _ -> "   CRASH"
  }
  io.println(
    label
    <> "   "
    <> compiled_s
    <> "     "
    <> pad(r.interp_per_us)
    <> "     "
    <> m0_s
    <> "       "
    <> pad(r.native_per_us)
    <> "       "
    <> vs_m0,
  )
}

fn pad(n: Int) -> String {
  let s = int.to_string(n)
  string.pad_start(s, 8, " ")
}
