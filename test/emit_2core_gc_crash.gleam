//// P5 crash investigation: t_collect fires between microtasks and program
//// crashes. Get full stack + verify with simpler chain.

import arc/compiler/emit_2core
import emit_2core_harness as harness
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/int
import gleam/io
import gleam/string
import twocore/backend/build_beam
import twocore/pipeline
import twocore/runtime/profiles
import twocore/runtime/rt_js_builtins
import twocore/runtime/rt_js_gc
import twocore/runtime/rt_js_store
import twocore/runtime/rt_js_types.{JsStore}
import twocore/runtime/rt_state.{type InstanceState}

@external(erlang, "twocore_rt_js_exec_ffi", "apply_js_main")
fn ffi_apply_js_main(mod: Atom, st: InstanceState) -> #(Dynamic, InstanceState)

fn seed_with(threshold: Int) -> InstanceState {
  let st =
    rt_state.fresh_full(
      rt_state.FullDecl(mems: [], globals: [], tables: [], ref_globals: []),
    )
  let store = rt_js_store.t_store_new(harness.twocore_test_hooks())
  let store = JsStore(..store, gc_threshold: threshold)
  let st = rt_state.t_with_js_store(st, store)
  let #(_realm, st) = rt_js_builtins.init_realm(st)
  st
}

fn compile_load(source: String, name: String) -> Atom {
  let opts =
    emit_2core.CompileOpts(
      module_name: name,
      source_kind: emit_2core.AsScript,
      entry_name: "js_main",
    )
  let assert Ok(unit) = emit_2core.compile_source(source, opts)
  let assert Ok(beam) = pipeline.compile_ir(unit.module, profiles.js_direct())
  let assert Ok(mod) = build_beam.load_module(atom.create(name), name, beam)
  mod
}

fn run_at(label: String, mod: Atom, thr: Int) {
  let seed = seed_with(thr)
  let #(outcome, st) = ffi_apply_js_main(mod, seed)
  let s = rt_js_gc.stats(st)
  io.println("")
  io.println("── " <> label <> " thr=" <> int.to_string(thr) <> " ──")
  io.println("outcome: " <> string.inspect(outcome))
  io.println("stdout : " <> string.inspect(rt_js_store.t_console_bytes(st)))
  io.println(
    "stats  : since_gc="
    <> int.to_string(s.since_gc)
    <> " live="
    <> int.to_string(s.live)
    <> " free="
    <> int.to_string(s.free),
  )
}

pub fn main() {
  // P5: recursive step via microtask; step is a top-level fn decl.
  let p5 = "function step(n){if(n==0){console.log('done');return}function m(x){return function(y){return x+y}}let a=m(5);let s=0;for(let i=0;i<1000;i++)s+=a(i);Promise.resolve().then(function(){step(n-1)})}step(100)"
  let m5 = compile_load(p5, "arc_gccrash_p5")
  run_at("P5 (100 steps × 1K allocs)", m5, 1_000_000_000)
  run_at("P5 (100 steps × 1K allocs)", m5, 65_536)
  run_at("P5 (100 steps × 1K allocs)", m5, 1000)

  // P5b: minimal — 2 chained microtasks with a small alloc burst between.
  // If GC fires after job1, does job2 crash?
  let p5b = "Promise.resolve().then(function(){function m(x){return function(y){return x+y}}let a=m(5);for(let i=0;i<2000;i++)a(i)}).then(function(){console.log('done')})"
  let m5b = compile_load(p5b, "arc_gccrash_p5b")
  run_at("P5b (2 chained .then, 2K allocs)", m5b, 1_000_000_000)
  run_at("P5b (2 chained .then, 2K allocs)", m5b, 1000)

  // P5c: minimal — just one .then that reads a global fn AFTER collect.
  let p5c = "function g(){return 42}function m(x){return function(y){return x+y}}let a=m(5);for(let i=0;i<2000;i++)a(i);Promise.resolve().then(function(){console.log(g())})"
  let m5c = compile_load(p5c, "arc_gccrash_p5c")
  run_at("P5c (top-level fn call in .then)", m5c, 1_000_000_000)
  run_at("P5c (top-level fn call in .then)", m5c, 1000)

  // P5d: minimal repro of P5 crash — closure captures outer PARAM `n`.
  let p5d = "function step(n){function m(x){return function(y){return x+y}}let a=m(5);for(let i=0;i<2000;i++)a(i);Promise.resolve().then(function(){console.log(n)})}step(7)"
  let m5d = compile_load(p5d, "arc_gccrash_p5d")
  run_at("P5d (capture param n=7)", m5d, 1_000_000_000)
  run_at("P5d (capture param n=7)", m5d, 1000)

  // P5e: closure captures a top-level fn (should be pinned root).
  let p5e = "function step(n){function m(x){return function(y){return x+y}}let a=m(5);for(let i=0;i<2000;i++)a(i);Promise.resolve().then(function(){step})}step(7)"
  let m5e = compile_load(p5e, "arc_gccrash_p5e")
  run_at("P5e (capture step fn)", m5e, 1_000_000_000)
  run_at("P5e (capture step fn)", m5e, 1000)

  // P5f: P5 with n forced through explicit local — check if it's param-specific.
  let p5f = "function step(n){let nn=n;function m(x){return function(y){return x+y}}let a=m(5);for(let i=0;i<2000;i++)a(i);Promise.resolve().then(function(){console.log(nn)})}step(7)"
  let m5f = compile_load(p5f, "arc_gccrash_p5f")
  run_at("P5f (capture local nn=7)", m5f, 1_000_000_000)
  run_at("P5f (capture local nn=7)", m5f, 1000)
}
