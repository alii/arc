// profiling harness, not a test

import arc/rt/store as rt_store
import arc/rt/types.{type Agent}
import arc_aot/emit as emit_2core
import arc_aot/run
import carder/pipeline
import emit_2core_bench.{adder_js, obj_js, sum_js}
import emit_2core_harness as harness
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import simplifile

@external(erlang, "emit_2core_profile_ffi", "trace_on")
fn trace_on(bench_mod: Atom) -> Nil

@external(erlang, "emit_2core_profile_ffi", "trace_off")
fn trace_off() -> Nil

@external(erlang, "emit_2core_profile_ffi", "reset")
fn trace_reset() -> Nil

@external(erlang, "emit_2core_profile_ffi", "count_of")
fn count_of(m: Atom, f: Atom, a: Int) -> Int

@external(erlang, "emit_2core_profile_ffi", "top_n")
fn top_n(n: Int) -> List(#(String, String, Int, Int, Int))

@external(erlang, "emit_2core_profile_ffi", "module_total")
fn module_total(m: Atom) -> Int

@external(erlang, "emit_2core_profile_ffi", "all_mods")
fn all_mods() -> List(Atom)

@external(erlang, "arc_aot_exec_ffi", "apply_js_main")
fn ffi_apply_js_main(mod: Atom, st: Agent) -> #(Dynamic, Agent)

type TimeUnit {
  Microsecond
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(unit: TimeUnit) -> Int

fn compile_and_seed(source: String, name: String) -> #(Atom, Agent) {
  let opts =
    emit_2core.CompileOpts(
      module_name: name,
      source_kind: emit_2core.AsScript,
      entry_name: "js_main",
    )
  let assert Ok(unit) = emit_2core.compile_source(source, opts)
  let assert Ok(beam) = pipeline.compile_ir(unit.module, emit_2core.binding())
  let assert Ok(mod) = run.load(beam, name)
  #(mod, harness.seed())
}

fn repeat(times: Int, f: fn() -> a) -> Nil {
  case times {
    0 -> Nil
    _ -> {
      f()
      repeat(times - 1, f)
    }
  }
}

fn profile(label: String, source: String, runs: Int, iters: Int) -> Nil {
  let name = "arc_prof_" <> label
  trace_reset()
  let #(mod, seed) = compile_and_seed(source, name)

  ffi_apply_js_main(mod, seed)

  let js_before = seed.store
  let #(_v, st_after) = ffi_apply_js_main(mod, seed)
  let js_after = st_after.store
  let cells = js_after.alloc_since_gc - js_before.alloc_since_gc

  let t0 = monotonic_time(Microsecond)
  repeat(runs, fn() { ffi_apply_js_main(mod, seed) })
  let untraced_us = monotonic_time(Microsecond) - t0

  trace_on(mod)
  let t1 = monotonic_time(Microsecond)
  repeat(runs, fn() { ffi_apply_js_main(mod, seed) })
  let traced_us = monotonic_time(Microsecond) - t1
  trace_off()

  io.println("")
  io.println(
    "══════ "
    <> label
    <> "  (runs="
    <> int.to_string(runs)
    <> " × 1M iter, cells/run="
    <> int.to_string(cells)
    <> ") ══════",
  )
  io.println(
    "  wall untraced: "
    <> int.to_string(untraced_us)
    <> " µs total, "
    <> int.to_string(untraced_us / runs)
    <> " µs/run",
  )
  io.println(
    "  wall traced:   "
    <> int.to_string(traced_us)
    <> " µs total ("
    <> int.to_string(traced_us * 100 / int.max(1, untraced_us))
    <> "% of untraced)",
  )

  io.println("  ── per-module µs (traced) ──")
  let mods = [mod, ..all_mods()]
  list.each(
    list.sort(
      list.filter_map(mods, fn(m) {
        case module_total(m) {
          0 -> Error(Nil)
          us -> Ok(#(atom.to_string(m), us))
        }
      }),
      fn(a, b) { int.compare(b.1, a.1) },
    ),
    fn(row) {
      io.println(
        "    "
        <> string.pad_end(row.0, 42, " ")
        <> " "
        <> string.pad_start(int.to_string(row.1), 10, " ")
        <> " µs  "
        <> string.pad_start(int.to_string(row.1 * 100 / traced_us), 3, " ")
        <> "%",
      )
    },
  )

  let fast =
    count_of(atom.create("arc_rt_call_ffi"), atom.create("t_kfn_code"), 3)
  let slow =
    count_of(atom.create("arc@rt@call"), atom.create("t_call_checked"), 4)
  case fast + slow {
    0 -> Nil
    _ ->
      io.println(
        "  fast-path: kfn_code="
        <> int.to_string(fast)
        <> " ("
        <> int.to_string(fast / runs)
        <> "/run)  t_call_checked(slow)="
        <> int.to_string(slow)
        <> " → fast-path "
        <> case slow {
          0 -> "TAKEN"
          _ -> "MISSED " <> int.to_string(slow) <> "×"
        },
      )
  }

  io.println("  ── top functions by µs (call_time) ──")
  let rows = top_n(25)
  io.println(
    "    "
    <> string.pad_end("module:function/arity", 55, " ")
    <> string.pad_start("count", 12, " ")
    <> string.pad_start("µs", 12, " ")
    <> string.pad_start("ns/call", 9, " ")
    <> string.pad_start("µs/run", 9, " ")
    <> string.pad_start("ns/iter", 9, " "),
  )
  list.each(rows, fn(row) {
    let #(m, f, a, count, us) = row
    let ns_call = case count {
      0 -> 0
      _ -> us * 1000 / count
    }
    io.println(
      "    "
      <> string.pad_end(
        short(m) <> ":" <> f <> "/" <> int.to_string(a),
        55,
        " ",
      )
      <> string.pad_start(int.to_string(count), 12, " ")
      <> string.pad_start(int.to_string(us), 12, " ")
      <> string.pad_start(int.to_string(ns_call), 9, " ")
      <> string.pad_start(int.to_string(us / runs), 9, " ")
      <> string.pad_start(int.to_string(us * 1000 / { runs * iters }), 9, " "),
    )
  })
}

fn short(m: String) -> String {
  case string.split(m, "@") {
    [_, _, tail] -> tail
    _ -> m
  }
}

pub fn profile_file(label: String, path: String, runs: Int) -> Nil {
  let assert Ok(source) = simplifile.read(path)
  let name = "arc_prof_" <> label
  trace_reset()
  let #(mod, seed) = compile_and_seed(source, name)

  ffi_apply_js_main(mod, seed)

  let js_before = seed.store
  let #(_v, st_after) = ffi_apply_js_main(mod, seed)
  let js_after = st_after.store
  let cells = js_after.alloc_since_gc - js_before.alloc_since_gc

  let t0 = monotonic_time(Microsecond)
  repeat(runs, fn() { ffi_apply_js_main(mod, seed) })
  let untraced_us = monotonic_time(Microsecond) - t0

  trace_on(mod)
  let t1 = monotonic_time(Microsecond)
  repeat(runs, fn() { ffi_apply_js_main(mod, seed) })
  let traced_us = monotonic_time(Microsecond) - t1
  trace_off()

  io.println("")
  io.println(
    "══════ "
    <> label
    <> "  ("
    <> path
    <> ", runs="
    <> int.to_string(runs)
    <> ", cells/run="
    <> int.to_string(cells)
    <> ") ══════",
  )
  io.println(
    "  wall untraced: "
    <> int.to_string(untraced_us)
    <> " µs total, "
    <> int.to_string(untraced_us / runs)
    <> " µs/run",
  )
  io.println(
    "  wall traced:   "
    <> int.to_string(traced_us)
    <> " µs total ("
    <> int.to_string(traced_us * 100 / int.max(1, untraced_us))
    <> "% of untraced)",
  )

  io.println("  ── per-module µs (traced) ──")
  let mods = [mod, ..all_mods()]
  list.each(
    list.sort(
      list.filter_map(mods, fn(m) {
        case module_total(m) {
          0 -> Error(Nil)
          us -> Ok(#(atom.to_string(m), us))
        }
      }),
      fn(a, b) { int.compare(b.1, a.1) },
    ),
    fn(row) {
      io.println(
        "    "
        <> string.pad_end(row.0, 42, " ")
        <> " "
        <> string.pad_start(int.to_string(row.1), 10, " ")
        <> " µs  "
        <> string.pad_start(
          int.to_string(row.1 * 100 / int.max(1, traced_us)),
          3,
          " ",
        )
        <> "%",
      )
    },
  )

  io.println("  ── top functions by µs (call_time) ──")
  let rows = top_n(25)
  io.println(
    "    "
    <> string.pad_end("module:function/arity", 55, " ")
    <> string.pad_start("count", 12, " ")
    <> string.pad_start("µs", 12, " ")
    <> string.pad_start("ns/call", 9, " ")
    <> string.pad_start("µs/run", 9, " "),
  )
  list.each(rows, fn(row) {
    let #(m, f, a, count, us) = row
    let ns_call = case count {
      0 -> 0
      _ -> us * 1000 / count
    }
    io.println(
      "    "
      <> string.pad_end(
        short(m) <> ":" <> f <> "/" <> int.to_string(a),
        55,
        " ",
      )
      <> string.pad_start(int.to_string(count), 12, " ")
      <> string.pad_start(int.to_string(us), 12, " ")
      <> string.pad_start(int.to_string(ns_call), 9, " ")
      <> string.pad_start(int.to_string(us / runs), 9, " "),
    )
  })

  io.println("  ── targeted call counts (per run) ──")
  let rt = fn(m: String) { atom.create("arc@rt@" <> m) }
  let ffi = fn(m: String) { atom.create("arc_" <> m) }
  let targets = [
    #(rt("obj"), "t_get_prop_any", 3),
    #(rt("obj"), "t_set_prop_any", 4),
    #(rt("call"), "t_call_checked", 4),
    #(rt("call"), "t_kfn_code", 3),
    #(rt("call"), "t_construct", 4),
    #(rt("ops"), "t_instance_of", 3),
    #(ffi("rt_obj_ffi"), "t_get_prop_own_data", 3),
    #(rt("obj"), "t_global_get", 2),
    #(ffi("rt_obj_ffi"), "t_global_get_fast", 2),
    #(ffi("rt_obj_ffi"), "t_get_elem_fast", 3),
    #(ffi("rt_obj_ffi"), "elem_read", 2),
    #(ffi("rt_obj_ffi"), "t_set_elem_fast", 4),
    #(ffi("rt_obj_ffi"), "elem_write", 3),
    #(rt("val"), "t_to_property_key", 2),
    #(ffi("rt_obj_ffi"), "t_set_prop_own_data", 4),
    #(rt("store"), "t_cell_get", 2),
    #(ffi("rt_store_ffi"), "t_cell_get", 2),
    #(ffi("rt_call_ffi"), "t_call_method_ic", 5),
    #(ffi("rt_call_ffi"), "t_new_simple", 3),
    #(ffi("rt_obj_ffi"), "t_ic_get", 4),
    #(ffi("rt_obj_ffi"), "t_ic_set", 5),
    #(rt("obj"), "t_new_arguments", 4),
    #(ffi("rt_call_ffi"), "new_simple_apply", 7),
    #(ffi("rt_call_ffi"), "t_method_ic_warm", 2),
  ]
  list.each(targets, fn(t) {
    let #(m, f, a) = t
    let n = count_of(m, atom.create(f), a)
    io.println(
      "    "
      <> string.pad_end(
        short(atom.to_string(m)) <> ":" <> f <> "/" <> int.to_string(a),
        55,
        " ",
      )
      <> string.pad_start(int.to_string(n), 12, " ")
      <> " total  "
      <> string.pad_start(int.to_string(n / runs), 10, " ")
      <> " /run",
    )
  })
}

@external(erlang, "emit_2core_profile_ffi", "bench_op")
fn bench_op(which: Atom, st: Agent, arg: Dynamic, n: Int) -> Int

@external(erlang, "emit_2core_harness_ffi", "to_dynamic")
fn to_dynamic(a: a) -> Dynamic

fn micro(label: String, which: String, st: Agent, arg: Dynamic, n: Int) {
  let a = atom.create(which)
  bench_op(a, st, arg, n)
  let us = bench_op(a, st, arg, n)
  let nop = bench_op(atom.create("nop"), st, arg, n)
  io.println(
    "  [micro] "
    <> string.pad_end(label, 30, " ")
    <> string.pad_start(int.to_string(us), 8, " ")
    <> " µs/1M  = "
    <> string.pad_start(int.to_string({ us - nop } * 1000 / n), 4, " ")
    <> " ns/call (nop-corrected; nop="
    <> int.to_string(nop)
    <> "µs)",
  )
}

fn microbench() {
  io.println("")
  io.println("══════ isolated untraced microbench (1M calls each) ══════")
  trace_reset()
  let #(mod, seed) = compile_and_seed(adder_js, "arc_prof_micro_adder")
  let #(_v, st_adder) = ffi_apply_js_main(mod, seed)
  let js = st_adder.store
  // inner fn is last cell, captured x is next-3
  let add5_h = to_dynamic(#(atom.create("js_cell"), js.next - 1))
  let x_h = to_dynamic(#(atom.create("js_cell"), js.next - 3))
  micro("kfn_code (via Gleam wrapper)", "kfn_code", st_adder, add5_h, 1_000_000)
  micro("kfn_code (FFI direct)", "kfn_code_ffi", st_adder, add5_h, 1_000_000)
  micro("cell_get (via Gleam wrapper)", "cell_get", st_adder, x_h, 1_000_000)
  micro("cell_get (FFI direct)", "cell_get_ffi", st_adder, x_h, 1_000_000)

  let #(mod2, seed2) = compile_and_seed(obj_js, "arc_prof_micro_obj")
  let #(_v2, st_obj) = ffi_apply_js_main(mod2, seed2)
  let js2 = st_obj.store
  let o_h = to_dynamic(#(atom.create("js_cell"), js2.next - 1))
  let kx = rt_store.t_key(st_obj, "x").0
  let key = to_dynamic(#(atom.create("string_key"), kx))
  micro(
    "t_get_prop_any (o.x)",
    "get_prop",
    st_obj,
    to_dynamic(#(o_h, key)),
    1_000_000,
  )
  micro(
    "t_set_prop_any (o.x = v)",
    "set_prop",
    st_obj,
    to_dynamic(#(o_h, key)),
    1_000_000,
  )
  let kb = to_dynamic(kx)
  micro(
    "t_get_prop_own_data (FFI)",
    "get_prop_own_data",
    st_obj,
    to_dynamic(#(o_h, kb)),
    1_000_000,
  )
  micro(
    "t_set_prop_own_data (FFI)",
    "set_prop_own_data",
    st_obj,
    to_dynamic(#(o_h, kb)),
    1_000_000,
  )
}

const richards_us_target = 2200

const obj_prop_us_target = 11_800

const richards_baseline = [
  #("arc_rt_obj_ffi", "t_global_get_fast", 2, 65),
  #("arc@rt@obj", "t_global_get", 2, 0),
  #("arc_rt_obj_ffi", "t_get_prop_own_data", 3, 106),
  #("arc_rt_obj_ffi", "t_set_prop_own_data", 4, 143),
  #("arc_rt_obj_ffi", "t_ic_get", 4, 0),
  #("arc_rt_obj_ffi", "t_ic_set", 5, 0),
  #("arc_rt_call_ffi", "t_new_simple", 3, 32),
  #("arc_rt_call_ffi", "t_call_method_ic", 5, 40_466),
  #("arc_rt_store_ffi", "t_cell_get", 2, 1320),
  #("arc_rt_call_ffi", "t_kfn_code", 3, 1),
]

fn correctness_gate(label: String, path: String) -> Bool {
  let assert Ok(source) = simplifile.read(path)
  case harness.run_compiled(source) {
    harness.DiffRun(result: Ok(_), stdout: <<"ok\n":utf8>>) -> {
      io.println("  ✓ " <> label <> " prints ok")
      True
    }
    harness.DiffRun(result: Ok(_), stdout:) -> {
      io.println(
        "  ✗ "
        <> label
        <> " completed but stdout="
        <> string.inspect(stdout)
        <> " (expected \"ok\\n\")",
      )
      False
    }
    harness.DiffRun(result: Error(e), stdout:) -> {
      io.println(
        "  ✗ "
        <> label
        <> " FAILED: "
        <> string.slice(e, 0, 300)
        <> " | stdout="
        <> string.inspect(stdout),
      )
      False
    }
  }
}

pub fn bench_verify() -> Bool {
  io.println("")
  io.println("══════ final-bench integration gate ══════")

  let richards_ok =
    correctness_gate("richards_run.js", "../bench/v8-v7/richards_run.js")
  let deltablue_ok =
    correctness_gate("deltablue_run.js", "../bench/v8-v7/deltablue_run.js")

  trace_reset()
  let assert Ok(src) = simplifile.read("../bench/v8-v7/richards_run.js")
  let #(mod, seed) = compile_and_seed(src, "arc_prof_gate_richards")
  ffi_apply_js_main(mod, seed)
  let runs = 5
  let best =
    list.fold(list.repeat(Nil, runs), 1_000_000_000, fn(acc, _) {
      let t0 = monotonic_time(Microsecond)
      ffi_apply_js_main(mod, seed)
      let dt = monotonic_time(Microsecond) - t0
      int.min(acc, dt)
    })
  let perf_ok = best <= richards_us_target
  io.println(
    "  "
    <> case perf_ok {
      True -> "✓"
      False -> "✗"
    }
    <> " richards "
    <> int.to_string(best)
    <> " µs/run (best of "
    <> int.to_string(runs)
    <> "; target ≤"
    <> int.to_string(richards_us_target)
    <> ")",
  )

  trace_reset()
  let #(obj_mod, obj_seed) = compile_and_seed(obj_js, "arc_prof_gate_obj")
  ffi_apply_js_main(obj_mod, obj_seed)
  let obj_best =
    list.fold(list.repeat(Nil, runs), 1_000_000_000, fn(acc, _) {
      let t0 = monotonic_time(Microsecond)
      ffi_apply_js_main(obj_mod, obj_seed)
      let dt = monotonic_time(Microsecond) - t0
      int.min(acc, dt)
    })
  let obj_ok = obj_best <= obj_prop_us_target
  io.println(
    "  "
    <> case obj_ok {
      True -> "✓"
      False -> "✗"
    }
    <> " obj_prop "
    <> int.to_string(obj_best)
    <> " µs/run (best of "
    <> int.to_string(runs)
    <> "; target ≤"
    <> int.to_string(obj_prop_us_target)
    <> ")",
  )

  trace_on(mod)
  ffi_apply_js_main(mod, seed)
  trace_off()
  io.println("  ── targeted counts: before (a2881bb) → after ──")
  io.println(
    "    "
    <> string.pad_end("{M,F,A}", 48, " ")
    <> string.pad_start("before", 10, " ")
    <> string.pad_start("after", 10, " ")
    <> string.pad_start("Δ", 12, " "),
  )
  list.each(richards_baseline, fn(row) {
    let #(m, f, a, before) = row
    let after = count_of(atom.create(m), atom.create(f), a)
    let delta = after - before
    io.println(
      "    "
      <> string.pad_end(
        short(m) <> ":" <> f <> "/" <> int.to_string(a),
        48,
        " ",
      )
      <> string.pad_start(int.to_string(before), 10, " ")
      <> string.pad_start(int.to_string(after), 10, " ")
      <> string.pad_start(
        case delta >= 0 {
          True -> "+" <> int.to_string(delta)
          False -> int.to_string(delta)
        },
        12,
        " ",
      ),
    )
  })

  case perf_ok {
    True -> Nil
    False -> {
      io.println("  ── attribution (target missed) ──")
      let n = fn(m, f, a) { count_of(atom.create(m), atom.create(f), a) }
      let g_after = n("arc_rt_obj_ffi", "t_global_get_fast", 2)
      let i_after = n("arc_rt_obj_ffi", "t_ic_get", 4)
      let h_own = n("arc_rt_obj_ffi", "t_get_prop_own_data", 3)
      io.println(
        "    G slotted-globals: t_global_get_fast "
        <> int.to_string(g_after)
        <> "/run — "
        <> case g_after < 10 {
          True -> "FIRED (NB baseline 65 setup-only, perf-negligible)"
          False -> "NOT FIRED (baseline 65 → expect ~0)"
        },
      )
      io.println(
        "    I prop-IC:         t_ic_get "
        <> int.to_string(i_after)
        <> "/run — "
        <> case i_after > 0 {
          True -> "FIRED"
          False -> "NOT FIRED (expect >0; reads still via own_data)"
        },
      )
      io.println(
        "    H shaped-objects:  t_get_prop_own_data "
        <> int.to_string(h_own)
        <> "/run — "
        <> case h_own < 50 {
          True -> "reads shifted (H/I)"
          False -> "still map-backed (baseline 106; H not firing)"
        },
      )
    }
  }

  let all = richards_ok && deltablue_ok && perf_ok && obj_ok
  io.println("")
  io.println(case all {
    True -> "  ══ PASS ══"
    False -> "  ══ FAIL ══"
  })
  all
}

pub fn raytrace_apply_verify() -> Bool {
  io.println("")
  io.println("══════ perf8 CC: raytrace-apply-verify ══════")
  let assert Ok(src) = simplifile.read("../bench/v8-v7/raytrace_run.js")
  trace_reset()
  let #(mod, seed) = compile_and_seed(src, "arc_prof_rt_cc")
  ffi_apply_js_main(mod, seed)
  trace_on(mod)
  let t0 = monotonic_time(Microsecond)
  ffi_apply_js_main(mod, seed)
  let traced_us = monotonic_time(Microsecond) - t0
  trace_off()

  io.println("  wall traced: " <> int.to_string(traced_us) <> " µs (1 run)")
  io.println("  ── top-10 by µs (call_time) ──")
  io.println(
    "    "
    <> string.pad_end("module:function/arity", 55, " ")
    <> string.pad_start("count", 12, " ")
    <> string.pad_start("µs", 12, " ")
    <> string.pad_start("ns/call", 9, " "),
  )
  list.each(top_n(10), fn(row) {
    let #(m, f, a, count, us) = row
    let ns_call = case count {
      0 -> 0
      _ -> us * 1000 / count
    }
    io.println(
      "    "
      <> string.pad_end(
        short(m) <> ":" <> f <> "/" <> int.to_string(a),
        55,
        " ",
      )
      <> string.pad_start(int.to_string(count), 12, " ")
      <> string.pad_start(int.to_string(us), 12, " ")
      <> string.pad_start(int.to_string(ns_call), 9, " "),
    )
  })

  let rt = fn(m: String) { atom.create("arc@rt@" <> m) }
  let ffi = fn(m: String) { atom.create("arc_" <> m) }
  let n_new_args = count_of(rt("obj"), atom.create("t_new_arguments"), 4)
  let n_call_chk = count_of(rt("call"), atom.create("t_call_checked"), 4)
  let n_new_simple =
    count_of(ffi("rt_call_ffi"), atom.create("t_new_simple"), 3)
  let n_ns_apply =
    count_of(ffi("rt_call_ffi"), atom.create("new_simple_apply"), 7)
  let n_method_ic =
    count_of(ffi("rt_call_ffi"), atom.create("t_call_method_ic"), 5)
  let n_construct = count_of(rt("call"), atom.create("t_construct"), 4)
  io.println("  ── targeted counts (per run) ──")
  let row = fn(name: String, n: Int) {
    io.println(
      "    "
      <> string.pad_end(name, 40, " ")
      <> string.pad_start(int.to_string(n), 10, " "),
    )
  }
  row("t_new_arguments/4", n_new_args)
  row("t_call_checked/4", n_call_chk)
  row("t_new_simple/3", n_new_simple)
  row("new_simple_apply/7", n_ns_apply)
  row("t_call_method_ic/5", n_method_ic)
  row("t_construct/4 (new_simple miss)", n_construct)

  let args_ok = n_new_args < 100
  let chk_ok = n_call_chk < 100
  let reaches = n_method_ic >= n_ns_apply - n_construct - 10
  io.println("  ── verdict ──")
  io.println(
    "    (1) t_new_arguments ≈0:       "
    <> case args_ok {
      True ->
        "✓ FIRES (perf7_args_elide elided; "
        <> int.to_string(n_new_args)
        <> "/run)"
      False ->
        "✗ REGRESSED ("
        <> int.to_string(n_new_args)
        <> "/run — needs_args_object carve-out not firing)"
    },
  )
  io.println(
    "    (2) t_call_checked ≈0:        "
    <> case chk_ok {
      True ->
        "✓ FIRES (.apply → call_method_ic; "
        <> int.to_string(n_call_chk)
        <> "/run)"
      False ->
        "✗ REGRESSED ("
        <> int.to_string(n_call_chk)
        <> "/run — emit_apply_arguments miss)"
    },
  )
  io.println(
    "    (3) new_simple → emit_apply_arguments: "
    <> case reaches {
      True ->
        "✓ REACHED (via compiled ctor body — "
        <> int.to_string(n_ns_apply)
        <> " new_simple_apply, "
        <> int.to_string(n_method_ic)
        <> " method_ic incl. initialize)"
      False ->
        "✗ NOT REACHED (method_ic "
        <> int.to_string(n_method_ic)
        <> " < new_simple_apply "
        <> int.to_string(n_ns_apply)
        <> " — ctor bodies falling to slow path)"
    },
  )
  args_ok && chk_ok && reaches
}

const am3_bench_js = "
function BI() { this.array = new Array(); }
BI.prototype.am3 = function(i,x,w,j,c,n) {
  var this_array = this.array;
  var w_array    = w.array;
  var xl = x&0x3fff, xh = x>>14;
  while(--n >= 0) {
    var l = this_array[i]&0x3fff;
    var h = this_array[i++]>>14;
    var m = xh*l+h*xl;
    l = xl*l+((m&0x3fff)<<14)+w_array[j]+c;
    c = (l>>28)+(m>>14)+xh*h;
    w_array[j++] = l&0xfffffff;
  }
  return c;
};
var a = new BI(); var w = new BI();
for (var k = 0; k < 40; k++) { a.array[k] = 0x7654321; w.array[k] = 0x1234567; }
var c = 0;
for (var r = 0; r < 100; r++) c = a.am3(0, 0x89abcde, w, 0, c, 40);
c;
"

pub fn crypto_am3_op_map() -> Nil {
  io.println("")
  io.println("══════ perf8 BB: crypto am3 op-map (isolated am3 harness) ══════")
  trace_reset()
  let #(mod, seed) = compile_and_seed(am3_bench_js, "arc_prof_am3")
  ffi_apply_js_main(mod, seed)
  trace_on(mod)
  ffi_apply_js_main(mod, seed)
  trace_off()

  io.println(
    "  (4000 am3 inner iters; per-iter: 2 reads_c, 1 read_p,"
    <> " 1 write_p, 4 >>C, 1 <<C, 4 &C, 4 *, 6 +)",
  )
  io.println("  ── top-10 by µs (call_time) ──")
  io.println(
    "    "
    <> string.pad_end("module:function/arity", 55, " ")
    <> string.pad_start("count", 12, " ")
    <> string.pad_start("µs", 12, " ")
    <> string.pad_start("ns/call", 9, " "),
  )
  list.each(top_n(10), fn(row) {
    let #(m, f, a, count, us) = row
    let ns_call = case count {
      0 -> 0
      _ -> us * 1000 / count
    }
    io.println(
      "    "
      <> string.pad_end(
        short(m) <> ":" <> f <> "/" <> int.to_string(a),
        55,
        " ",
      )
      <> string.pad_start(int.to_string(count), 12, " ")
      <> string.pad_start(int.to_string(us), 12, " ")
      <> string.pad_start(int.to_string(ns_call), 9, " "),
    )
  })

  io.println("  ── per-op runtime counts (JPure verdict) ──")
  let ffi = fn(m: String) { atom.create("arc_" <> m) }
  let rt = fn(m: String) { atom.create("arc@rt@" <> m) }
  let per_op = [
    #(ffi("rt_ops_ffi"), "t_shr_fast", 2, ">>14/>>28 fallback", 0),
    #(ffi("rt_ops_ffi"), "t_shl_fast", 2, "<<14 fallback", 0),
    #(ffi("rt_ops_ffi"), "t_bitand_fast", 2, "& 0x3fff/0xfffffff fallback", 0),
    #(ffi("rt_ops_ffi"), "t_ushr_fast", 2, ">>> (am3 has none)", 0),
    #(rt("ops"), "t_mul", 3, "* fallback (JMut)", 0),
    #(rt("ops"), "t_add", 3, "+ fallback (JMut)", 0),
    #(ffi("rt_obj_ffi"), "t_get_elem_fast_c", 4, "this_array[i] hoisted", 8000),
    #(ffi("rt_obj_ffi"), "t_get_elem_fast_p", 3, "w_array[j] read", 4000),
    #(ffi("rt_obj_ffi"), "t_set_elem_fast_p", 4, "w_array[j++]= write", 4000),
    #(ffi("rt_obj_ffi"), "t_arr_c_load", 1, "arr_c hoist (1/am3 call)", 100),
    #(rt("obj"), "t_get_prop_any", 3, "elem-miss slow path", 0),
    #(rt("val"), "t_to_property_key", 2, "elem-miss key coerce", 0),
  ]
  io.println(
    "    "
    <> string.pad_end("{M,F,A}", 42, " ")
    <> string.pad_start("count", 8, " ")
    <> string.pad_start("expect", 8, " ")
    <> "  op",
  )
  list.each(per_op, fn(t) {
    let #(m, f, a, op, expect) = t
    let n = count_of(m, atom.create(f), a)
    io.println(
      "    "
      <> string.pad_end(
        short(atom.to_string(m)) <> ":" <> f <> "/" <> int.to_string(a),
        42,
        " ",
      )
      <> string.pad_start(int.to_string(n), 8, " ")
      <> string.pad_start(int.to_string(expect), 8, " ")
      <> "  "
      <> op,
    )
  })
}

fn dump_am3_core() -> Nil {
  let opts =
    emit_2core.CompileOpts(
      module_name: "arc_prof_am3",
      source_kind: emit_2core.AsScript,
      entry_name: "js_main",
    )
  case emit_2core.compile_source(am3_bench_js, opts) {
    Error(e) ->
      io.println("!! am3 compile_source FAILED: " <> string.inspect(e))
    Ok(unit) ->
      case pipeline.ir_to_core(unit.module, emit_2core.binding()) {
        Error(e) ->
          io.println("!! am3 ir_to_core FAILED: " <> string.inspect(e))
        Ok(core) -> io.println(core)
      }
  }
}

pub fn main() {
  io.println("emit_2core call_time profile — traced=local, per-{M,F,A}")
  dump_am3_core()
}
