//// GC cost isolation: P2 + P3 only, 100 reps × 3 interleaved rounds.

import arc/compiler/emit_2core
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
import twocore/runtime/rt_js_types.{JsStore}
import twocore/runtime/rt_state.{type InstanceState}

type TimeUnit {
  Microsecond
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(unit: TimeUnit) -> Int

@external(erlang, "twocore_rt_js_exec_ffi", "apply_js_main")
fn ffi_apply_js_main(mod: Atom, st: InstanceState) -> #(Dynamic, InstanceState)

const p2 = "function m(x){return function(y){return x+y}}let a=m(5);let s=0;for(let i=0;i<100000;i++)s+=a(i);Promise.resolve().then(function(){console.log('cb')});console.log('done')"

const p3 = "Promise.resolve().then(function(){function m(x){return function(y){return x+y}}let a=m(5);let s=0;for(let i=0;i<100000;i++)s+=a(i)}).then(function(){console.log('done')})"

fn seed_with(threshold: Int) -> InstanceState {
  let st =
    rt_state.fresh_full(
      rt_state.FullDecl(mems: [], globals: [], tables: [], ref_globals: []),
    )
  let store = rt_js_store.t_store_new(harness.twocore_test_hooks())
  let store = JsStore(..store, gc_threshold: threshold)
  let st = rt_state.t_with_js_store(st, store)
  let #(_, st) = rt_js_builtins.init_realm(st)
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

fn repeat_n(times: Int, f: fn() -> a) -> Nil {
  case times {
    0 -> Nil
    _ -> {
      let _ = f()
      repeat_n(times - 1, f)
    }
  }
}

fn time_reps(mod: Atom, seed: InstanceState, reps: Int) -> Int {
  let t0 = monotonic_time(Microsecond)
  repeat_n(reps, fn() { ffi_apply_js_main(mod, seed) })
  let dt = monotonic_time(Microsecond) - t0
  dt / reps
}

fn round(
  label: String,
  mod: Atom,
  seed_on: InstanceState,
  seed_off: InstanceState,
  reps: Int,
) {
  let on = time_reps(mod, seed_on, reps)
  let off = time_reps(mod, seed_off, reps)
  let on2 = time_reps(mod, seed_on, reps)
  let off2 = time_reps(mod, seed_off, reps)
  let on3 = time_reps(mod, seed_on, reps)
  let off3 = time_reps(mod, seed_off, reps)
  let on_avg = { on + on2 + on3 } / 3
  let off_avg = { off + off2 + off3 } / 3
  io.println(
    label
    <> "  gc-on(3×"
    <> int.to_string(reps)
    <> "): ["
    <> int.to_string(on)
    <> ","
    <> int.to_string(on2)
    <> ","
    <> int.to_string(on3)
    <> "] avg="
    <> int.to_string(on_avg)
    <> "µs   gc-off: ["
    <> int.to_string(off)
    <> ","
    <> int.to_string(off2)
    <> ","
    <> int.to_string(off3)
    <> "] avg="
    <> int.to_string(off_avg)
    <> "µs   Δ(on-off)="
    <> int.to_string(on_avg - off_avg)
    <> "µs ("
    <> int.to_string({ on_avg - off_avg } * 100 / off_avg)
    <> "%)",
  )
}

pub fn main() {
  let m2 = compile_load(p2, "arc_gccost_p2")
  let m3 = compile_load(p3, "arc_gccost_p3")
  let seed_on = seed_with(65_536)
  let seed_off = seed_with(1_000_000_000)
  // warm
  let _ = ffi_apply_js_main(m2, seed_on)
  let _ = ffi_apply_js_main(m3, seed_on)
  // sanity: verify fired?
  let s2on = rt_js_gc.stats(ffi_apply_js_main(m2, seed_on).1)
  let s2off = rt_js_gc.stats(ffi_apply_js_main(m2, seed_off).1)
  io.println(
    "sanity P2: gc-on since_gc="
    <> int.to_string(s2on.since_gc)
    <> " free="
    <> int.to_string(s2on.free)
    <> "   gc-off since_gc="
    <> int.to_string(s2off.since_gc)
    <> " free="
    <> int.to_string(s2off.free),
  )
  round("P2", m2, seed_on, seed_off, 50)
  round("P3", m3, seed_on, seed_off, 50)
}
