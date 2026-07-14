//// One-shot obj_prop probe: best-of-5 untraced timing + call counts for
//// the ic_warm/ic tiers so obj-prop-goal-unverified can see whether the
//// warm-only probe actually hits. Run:
////   cd arc && gleam run -m emit_2core_objprop_probe

import emit_2core_bench.{obj_js}
import emit_2core_harness as harness
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/int
import gleam/io
import gleam/list
import twocore/backend/build_beam
import twocore/pipeline
import twocore/runtime/profiles
import twocore/runtime/rt_js_builtins
import twocore/runtime/rt_js_store
import twocore/runtime/rt_state.{type InstanceState}
import arc/compiler/emit_2core

type TimeUnit {
  Microsecond
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(unit: TimeUnit) -> Int

@external(erlang, "twocore_rt_js_exec_ffi", "apply_js_main")
fn ffi_apply_js_main(mod: Atom, st: InstanceState) -> #(Dynamic, InstanceState)

@external(erlang, "emit_2core_profile_ffi", "trace_on")
fn trace_on(bench_mod: Atom) -> Nil

@external(erlang, "emit_2core_profile_ffi", "trace_off")
fn trace_off() -> Nil

@external(erlang, "emit_2core_profile_ffi", "reset")
fn trace_reset() -> Nil

@external(erlang, "emit_2core_profile_ffi", "count_of")
fn count_of(m: Atom, f: Atom, a: Int) -> Int

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

fn c(m: String, f: String, a: Int) -> Int {
  count_of(atom.create(m), atom.create(f), a)
}

pub fn main() {
  trace_reset()
  let #(mod, seed) = compile_and_seed(obj_js, "arc_objprop_probe")
  // warm
  ffi_apply_js_main(mod, seed)
  // best-of-5 untraced
  let best =
    list.fold(list.repeat(Nil, 5), 1_000_000_000, fn(acc, _) {
      let t0 = monotonic_time(Microsecond)
      ffi_apply_js_main(mod, seed)
      let dt = monotonic_time(Microsecond) - t0
      int.min(acc, dt)
    })
  io.println("obj_prop best-of-5: " <> int.to_string(best) <> " µs (target ≤11800)")
  // one traced run for call counts
  trace_on(mod)
  ffi_apply_js_main(mod, seed)
  trace_off()
  let ffi = "twocore_rt_js_obj_ffi"
  let obj = "twocore@runtime@rt_js_obj"
  io.println("  t_ic_warm_get/2       = " <> int.to_string(c(ffi, "t_ic_warm_get", 2)))
  io.println("  t_ic_warm_set/3       = " <> int.to_string(c(ffi, "t_ic_warm_set", 3)))
  io.println("  t_ic_get/4            = " <> int.to_string(c(ffi, "t_ic_get", 4)))
  io.println("  t_ic_set/5            = " <> int.to_string(c(ffi, "t_ic_set", 5)))
  io.println("  t_get_prop_own_data/3 = " <> int.to_string(c(ffi, "t_get_prop_own_data", 3)))
  io.println("  t_set_prop_own_data/4 = " <> int.to_string(c(ffi, "t_set_prop_own_data", 4)))
  io.println("  t_get_prop_any/3      = " <> int.to_string(c(obj, "t_get_prop_any", 3)))
  io.println("  t_set_prop_any/4      = " <> int.to_string(c(obj, "t_set_prop_any", 4)))
  io.println("  t_new_object_shaped/4 = " <> int.to_string(c(ffi, "t_new_object_shaped", 4)))
}
