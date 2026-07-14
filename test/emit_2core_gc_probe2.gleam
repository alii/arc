//// probe-store-growth micro-probe: isolate what allocates 1 cell per call.

import arc/compiler/emit_2core
import emit_2core_bench.{adder_js, obj_js, sum_js}
import emit_2core_harness as harness
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/int
import gleam/io
import twocore/backend/build_beam
import twocore/pipeline
import twocore/runtime/profiles
import twocore/runtime/rt_js_builtins
import twocore/runtime/rt_js_gc
import twocore/runtime/rt_js_store
import twocore/runtime/rt_state.{type InstanceState}

@external(erlang, "twocore_rt_js_exec_ffi", "apply_js_main")
fn ffi_apply_js_main(mod: Atom, st: InstanceState) -> #(Dynamic, InstanceState)

fn seed() -> InstanceState {
  let st =
    rt_state.fresh_full(
      rt_state.FullDecl(mems: [], globals: [], tables: [], ref_globals: []),
    )
  let st =
    rt_state.t_with_js_store(
      st,
      rt_js_store.t_store_new(harness.twocore_test_hooks()),
    )
  let #(_, st) = rt_js_builtins.init_realm(st)
  st
}

fn delta(source: String, name: String) -> Int {
  let opts =
    emit_2core.CompileOpts(
      module_name: name,
      source_kind: emit_2core.AsScript,
      entry_name: "js_main",
    )
  let assert Ok(unit) = emit_2core.compile_source(source, opts)
  let assert Ok(beam) = pipeline.compile_ir(unit.module, profiles.js_direct())
  let assert Ok(mod) = build_beam.load_module(atom.create(name), name, beam)
  let s = seed()
  let base = rt_js_gc.stats(s)
  let #(_, st) = ffi_apply_js_main(mod, s)
  rt_js_gc.stats(st).since_gc - base.since_gc
}

fn p(label: String, source: String, name: String) {
  io.println(label <> " = +" <> int.to_string(delta(source, name)))
}

pub fn main() {
  let s = seed()
  let b = rt_js_gc.stats(s)
  io.println(
    "realm baseline: next="
    <> int.to_string(b.next)
    <> " since_gc="
    <> int.to_string(b.since_gc)
    <> " live="
    <> int.to_string(b.live),
  )
  io.println("── 1M benches ──")
  p("sum(1M)      ", sum_js, "gcp2_sum")
  p("makeAdder(1M)", adder_js, "gcp2_adder")
  io.println("obj_prop(1M)  = CRASH (element(6,0) — obj-literal path bug)")
  let _ = obj_js
  io.println("── isolate per-call cell ──")
  p("empty        ", "0", "gcp2_e")
  p("fn decl only ", "function f(y){return y+1}", "gcp2_fd")
  p("fn + 1 call  ", "function f(y){return y+1}f(5)", "gcp2_f1")
  p("fn + 3 calls ", "function f(y){return y+1}f(1);f(2);f(3)", "gcp2_f3")
  p("arrow + 3    ", "let f=(y)=>y+1;f(1);f(2);f(3)", "gcp2_a3")
  p("makeAdder n=0", "function m(x){return function(y){return x+y}}let a=m(5)", "gcp2_m0")
  p("makeAdder n=3", "function m(x){return function(y){return x+y}}let a=m(5);a(1);a(2);a(3)", "gcp2_m3")
  p("call m 3x    ", "function m(x){return function(y){return x+y}}m(1);m(2);m(3)", "gcp2_mm3")
}
