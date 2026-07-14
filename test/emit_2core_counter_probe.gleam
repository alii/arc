//// Focused microbench: does `alloc_since_gc + 1` in t_cell_new cost anything?
//// Run: cd arc && gleam run -m emit_2core_counter_probe

import arc/compiler/emit_2core
import emit_2core_bench.{adder_js, obj_js, sum_js}
import emit_2core_harness as harness
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/int
import gleam/io
import gleam/option.{Some}
import twocore/backend/build_beam
import twocore/pipeline
import twocore/runtime/profiles
import twocore/runtime/rt_js_builtins
import twocore/runtime/rt_js_store
import twocore/runtime/rt_js_types.{JsStore}
import twocore/runtime/rt_state.{type InstanceState}

type TimeUnit {
  Nanosecond
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(unit: TimeUnit) -> Int

@external(erlang, "twocore_rt_js_exec_ffi", "apply_js_main")
fn ffi_apply_js_main(mod: Atom, st: InstanceState) -> #(Dynamic, InstanceState)

fn compile_and_seed(source: String, name: String) -> #(Atom, InstanceState) {
  let opts =
    emit_2core.CompileOpts(
      module_name: name,
      source_kind: emit_2core.AsScript,
      entry_name: "js_main",
    )
  let assert Ok(unit) = emit_2core.compile_source(source, opts)
  let assert Ok(beam) = pipeline.compile_ir(unit.module, profiles.js_direct())
  let assert Ok(mod) = build_beam.load_module(atom.create(name), name, beam)
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
  #(mod, st)
}

fn probe_allocs(label: String, source: String, name: String) {
  let #(mod, seed) = compile_and_seed(source, name)
  // realm-init baseline
  let assert Some(js0) = seed.js_store
  let #(_out, st1) = ffi_apply_js_main(mod, seed)
  let assert Some(js1) = st1.js_store
  io.println(
    "[alloc] "
    <> label
    <> ": realm_next="
    <> int.to_string(js0.next)
    <> " realm_alloc="
    <> int.to_string(js0.alloc_since_gc)
    <> " after_next="
    <> int.to_string(js1.next)
    <> " after_alloc="
    <> int.to_string(js1.alloc_since_gc)
    <> " user_cells="
    <> int.to_string(js1.alloc_since_gc - js0.alloc_since_gc),
  )
}

fn repeat_ignore(times: Int, f: fn() -> a) -> Nil {
  case times {
    0 -> Nil
    _ -> {
      f()
      repeat_ignore(times - 1, f)
    }
  }
}

fn time_reps(
  label: String,
  source: String,
  name: String,
  reps: Int,
  cells_per_run: Int,
) {
  let #(mod, seed) = compile_and_seed(source, name)
  // warm
  ffi_apply_js_main(mod, seed)
  ffi_apply_js_main(mod, seed)
  let t0 = monotonic_time(Nanosecond)
  repeat_ignore(reps, fn() { ffi_apply_js_main(mod, seed) })
  let dt = monotonic_time(Nanosecond) - t0
  let per_run_ns = dt / reps
  let per_cell = case cells_per_run {
    0 -> "n/a"
    _ -> int.to_string(per_run_ns / cells_per_run) <> " ns/cell"
  }
  io.println(
    "[time]  "
    <> label
    <> ": "
    <> int.to_string(reps)
    <> " reps, total="
    <> int.to_string(dt / 1000)
    <> " µs, per_run="
    <> int.to_string(per_run_ns)
    <> " ns ("
    <> int.to_string(per_run_ns / 1000)
    <> " µs), "
    <> per_cell,
  )
  Nil
}

// ── direct t_cell_new microbench: isolate the allocation cost ──

fn cell_new_loop(st: InstanceState, n: Int) -> InstanceState {
  case n {
    0 -> st
    _ -> {
      let #(_h, st) =
        rt_js_store.t_cell_new(st, rt_js_types.SBox(rt_js_types.mk_undefined()))
      cell_new_loop(st, n - 1)
    }
  }
}

fn direct_cell_bench(iters: Int, reps: Int) {
  // seed one realm-free store per rep (fresh, so free-list is empty)
  let mk_seed = fn() {
    let st =
      rt_state.fresh_full(
        rt_state.FullDecl(mems: [], globals: [], tables: [], ref_globals: []),
      )
    rt_state.t_with_js_store(
      st,
      rt_js_store.t_store_new(harness.twocore_test_hooks()),
    )
  }
  // warm
  cell_new_loop(mk_seed(), iters)
  let t0 = monotonic_time(Nanosecond)
  repeat_ignore(reps, fn() { cell_new_loop(mk_seed(), iters) })
  let dt = monotonic_time(Nanosecond) - t0
  let per_call = dt / { reps * iters }
  io.println(
    "[direct] t_cell_new × "
    <> int.to_string(iters)
    <> " × "
    <> int.to_string(reps)
    <> " reps: total="
    <> int.to_string(dt / 1000)
    <> " µs, per_call="
    <> int.to_string(per_call)
    <> " ns",
  )
}

pub fn main() {
  // suppress unused warnings for the e2e helpers we may skip
  let _ = probe_allocs
  let _ = time_reps
  let _ = #(sum_js, adder_js, obj_js)
  io.println("── counter-overhead probe ──")
  io.println("direct t_cell_new microbench (isolates alloc cost)")
  direct_cell_bench(1_000_000, 10)
  direct_cell_bench(1_000_000, 10)
  direct_cell_bench(1_000_000, 10)
  direct_cell_bench(1_000_000, 10)
  direct_cell_bench(1_000_000, 10)
}
