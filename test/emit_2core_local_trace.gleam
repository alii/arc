//// LOCAL call_time trace of the compiled arc_prof_* module — the ~486k
//// letrec/jsf_N BEAM calls that profile_file's global-only trace can't see
//// (Core-Erlang-emitted modules lack module_info). Recreates the technique
//// referenced at emit_2core_profile.gleam:691.
////
////     cd arc && gleam run -m emit_2core_local_trace

import arc/compiler/emit_2core
import emit_2core_harness as harness
import gleam/erlang/atom.{type Atom}
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import simplifile
import twocore/backend/build_beam
import twocore/pipeline
import twocore/runtime/profiles
import twocore/runtime/rt_js_builtins
import twocore/runtime/rt_js_store
import twocore/runtime/rt_state.{type InstanceState}

@external(erlang, "twocore_rt_js_exec_ffi", "apply_js_main")
fn ffi_apply_js_main(mod: Atom, st: InstanceState) -> #(a, InstanceState)

@external(erlang, "emit_2core_profile_ffi", "eprof_run")
fn eprof_run(mod: Atom, st: InstanceState) -> Nil

@external(erlang, "local_probe", "run")
fn local_probe_run(mod: Atom, st: InstanceState) -> Nil

@external(erlang, "emit_2core_profile_ffi", "probe_jsf")
fn probe_jsf(
  mod: Atom,
  st: InstanceState,
) -> #(Int, Int, Int, Int, List(#(String, Int, Int, Int)))

@external(erlang, "emit_2core_profile_ffi", "reset")
fn trace_reset() -> Nil

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

@external(erlang, "local_probe", "raytrace_as_sobject")
fn raytrace_as_sobject(mod: Atom, st: InstanceState) -> Nil

@external(erlang, "local_probe", "count_new_object_shaped")
fn count_new_object_shaped(mod: Atom, st: InstanceState) -> Nil

pub fn main() {
  io.println("═══ raytrace as_sobject shape-size probe ═══")
  trace_reset()
  let assert Ok(rsrc) = simplifile.read("bench/v8-v7/raytrace_run.js")
  let #(rmod, rseed) = compile_and_seed(rsrc, "arc_prof_raytrace")
  ffi_apply_js_main(rmod, rseed)
  raytrace_as_sobject(rmod, rseed)
  io.println("─── shape-transition + new_object_shaped counts ───")
  trace_reset()
  count_new_object_shaped(rmod, rseed)

  io.println("═══ LOCAL call_time: arc_prof_richards ═══")
  trace_reset()
  let assert Ok(src) = simplifile.read("bench/v8-v7/richards_run.js")
  let #(mod, seed) = compile_and_seed(src, "arc_prof_richards")
  // warm
  ffi_apply_js_main(mod, seed)
  // eprof_run: BIF counts + jN letrec total + per-jsf_N call_time (1 run)
  eprof_run(mod, seed)

  io.println("")
  io.println("═══ local_probe (get_module_info + LOCAL call_time) ═══")
  trace_reset()
  ffi_apply_js_main(mod, seed)
  local_probe_run(mod, seed)

  io.println("")
  io.println("═══ probe_jsf (pdict get/put/element/setelement + jsf_N) ═══")
  trace_reset()
  ffi_apply_js_main(mod, seed)
  // warm again (eprof_run clears patterns)
  let #(gc, pc, ec, sc, rows) = probe_jsf(mod, seed)
  io.println(
    "  erlang:get/1        " <> string.pad_start(int.to_string(gc), 10, " "),
  )
  io.println(
    "  erlang:put/2        " <> string.pad_start(int.to_string(pc), 10, " "),
  )
  io.println(
    "  erlang:element/2    " <> string.pad_start(int.to_string(ec), 10, " "),
  )
  io.println(
    "  erlang:setelement/3 " <> string.pad_start(int.to_string(sc), 10, " "),
  )
  io.println("  ── jsf_N by µs ──")
  list.each(rows, fn(row) {
    let #(f, a, count, us) = row
    io.println(
      "    "
      <> string.pad_end(f <> "/" <> int.to_string(a), 20, " ")
      <> string.pad_start(int.to_string(count), 10, " ")
      <> string.pad_start(int.to_string(us), 10, " ")
      <> " µs  "
      <> string.pad_start(int.to_string(us * 1000 / int.max(1, count)), 6, " ")
      <> " ns/call",
    )
  })
}
