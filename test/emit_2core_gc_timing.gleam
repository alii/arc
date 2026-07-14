//// GC force-threshold-timing probe (rdr pass 1a).
//// Matrix: {P1 sync, P2 trailing-microtask, P3 loop-inside-then} ×
//// {gc_threshold=65536 (stock), 10^9 (=gc-off), 1000}. Prints wall_us,
//// final since_gc, fired?. NO 2core src edits — gc-off is achieved via
//// gc_threshold=10^9 (t_maybe_collect's `alloc_since_gc >= threshold`
//// short-circuits).
////
////     cd arc && gleam run -m emit_2core_gc_timing

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

type TimeUnit {
  Microsecond
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(unit: TimeUnit) -> Int

@external(erlang, "twocore_rt_js_exec_ffi", "apply_js_main")
fn ffi_apply_js_main(mod: Atom, st: InstanceState) -> #(Dynamic, InstanceState)

// ── programs ───────────────────────────────────────────────────────────────
// makeAdder closure-call → 1 cell/iter (verified). Object-literal path
// tried separately below via try_compile.

/// P1: 100K allocs, NO microtask. `t_drain_microtasks` sees empty queue →
/// returns at `None -> st` → `t_maybe_collect` NEVER called.
const p1 = "function m(x){return function(y){return x+y}}let a=m(5);let s=0;for(let i=0;i<100000;i++)s+=a(i);console.log('done')"

/// P2: 100K allocs + trailing microtask. Drain: pop → execute → `t_maybe_collect`.
const p2 = "function m(x){return function(y){return x+y}}let a=m(5);let s=0;for(let i=0;i<100000;i++)s+=a(i);Promise.resolve().then(function(){console.log('cb')});console.log('done')"

/// P3: 100K allocs INSIDE .then callback, then a second .then. Drain: pop
/// job1 (100K allocs) → t_maybe_collect → pop job2 → t_maybe_collect. All
/// defs INSIDE the callback (emit_2core closure-capture of outer `let` is
/// currently broken — see P4 note).
const p3 = "Promise.resolve().then(function(){function m(x){return function(y){return x+y}}let a=m(5);let s=0;for(let i=0;i<100000;i++)s+=a(i)}).then(function(){console.log('done')})"

/// P4 correctness: allocate 100K, force collect, then USE the top-level
/// `a` fn post-GC. `a` is a FunctionDeclaration → hoisted onto global obj
/// → pinned root, so it must survive `t_collect`.
const p4 = "function m(x){return function(y){return x+y}}function a(y){return 5+y}let s=0;for(let i=0;i<100000;i++)s+=m(5)(i);Promise.resolve().then(function(){console.log('cb')});console.log(a(10))"

/// P5: 100 microtasks × 1K allocs each. thr=1000 fires per-job; thr=65536
/// fires once (after ~66th job); thr=10^9 never.
const p5 = "function step(n){if(n==0){console.log('done');return}function m(x){return function(y){return x+y}}let a=m(5);let s=0;for(let i=0;i<1000;i++)s+=a(i);Promise.resolve().then(function(){step(n-1)})}step(100)"

/// task-literal (a): SYNC obj-literal loop.
const obj_sync = "for(let i=0;i<100000;i++){let o={x:i}};console.log('done')"

/// task-literal (b): ASYNC obj-literal loop inside .then.
const obj_async = "Promise.resolve().then(function(){for(let i=0;i<100000;i++){let o={x:i}}}).then(function(){console.log('done')})"

// ── seeding ────────────────────────────────────────────────────────────────

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

fn compile_load(source: String, name: String) -> Result(Atom, String) {
  let opts =
    emit_2core.CompileOpts(
      module_name: name,
      source_kind: emit_2core.AsScript,
      entry_name: "js_main",
    )
  case emit_2core.compile_source(source, opts) {
    Error(e) -> Error("emit: " <> string.inspect(e))
    Ok(unit) ->
      case pipeline.compile_ir(unit.module, profiles.js_direct()) {
        Error(e) -> Error("lower: " <> string.inspect(e))
        Ok(beam) ->
          case build_beam.load_module(atom.create(name), name, beam) {
            Error(e) -> Error("load: " <> e)
            Ok(mod) -> Ok(mod)
          }
      }
  }
}

fn repeat_n(times: Int, f: fn() -> a) -> a {
  case times {
    1 -> f()
    _ -> {
      let _ = f()
      repeat_n(times - 1, f)
    }
  }
}

/// One matrix row. `variant` names the gc mode; `threshold` sets it.
fn row(prog: String, variant: String, mod: Atom, threshold: Int, reps: Int) {
  let seed = seed_with(threshold)
  // warm
  let #(outcome0, st0) = ffi_apply_js_main(mod, seed)
  let s_after0 = rt_js_gc.stats(st0)
  let stdout0 = rt_js_store.t_console_bytes(st0)
  // timed reps (each starts from the SAME immutable seed)
  let t0 = monotonic_time(Microsecond)
  let #(_, _st) = repeat_n(reps, fn() { ffi_apply_js_main(mod, seed) })
  let dt = monotonic_time(Microsecond) - t0
  let s_seed = rt_js_gc.stats(seed)
  let fired = s_after0.since_gc < s_seed.since_gc || s_after0.free > s_seed.free
  let fired_s = case fired {
    True -> "Y"
    False -> "N"
  }
  let ok = case string.slice(string.inspect(outcome0), 0, 4) {
    "JsRe" -> "ok"
    _ -> "!! " <> string.slice(string.inspect(outcome0), 0, 60)
  }
  io.println(
    pad(prog, 18)
    <> pad(variant, 15)
    <> pad(int.to_string(dt / reps), 10)
    <> pad(int.to_string(s_after0.since_gc), 12)
    <> pad(int.to_string(s_after0.live), 10)
    <> pad(int.to_string(s_after0.free), 10)
    <> pad(fired_s, 6)
    <> ok
    <> "  out="
    <> string.slice(string.inspect(stdout0), 0, 60),
  )
}

fn pad(s: String, w: Int) -> String {
  let n = string.length(s)
  case n < w {
    True -> s <> string.repeat(" ", w - n)
    False -> s
  }
}

fn matrix(prog: String, mod: Atom, reps: Int) {
  row(prog, "gc-on(65536)", mod, 65_536, reps)
  row(prog, "gc-off(10^9)", mod, 1_000_000_000, reps)
  row(prog, "thr=1000", mod, 1000, reps)
}

fn try_run_once(label: String, source: String, name: String) {
  case compile_load(source, name) {
    Error(e) -> io.println("SKIP " <> label <> "  — " <> e)
    Ok(mod) -> {
      let seed = seed_with(65_536)
      let #(outcome, st) = ffi_apply_js_main(mod, seed)
      let s = rt_js_gc.stats(st)
      io.println(
        "TRY  "
        <> pad(label, 24)
        <> "outcome="
        <> string.slice(string.inspect(outcome), 0, 80)
        <> "  since_gc="
        <> int.to_string(s.since_gc)
        <> "  live="
        <> int.to_string(s.live),
      )
    }
  }
}

pub fn main() {
  io.println("emit_2core GC force-threshold-timing probe")
  io.println("")
  io.println("── does the task's obj-literal loop compile+run? ──")
  try_run_once("obj-sync", obj_sync, "arc_gcpt_objs")
  try_run_once("obj-async", obj_async, "arc_gcpt_obja")
  io.println("")

  io.println("── seed baseline ──")
  let s0 = rt_js_gc.stats(seed_with(65_536))
  io.println(
    "seed: since_gc="
    <> int.to_string(s0.since_gc)
    <> " live="
    <> int.to_string(s0.live)
    <> " next="
    <> int.to_string(s0.next),
  )
  io.println("")

  io.println(
    pad("program", 18)
    <> pad("variant", 15)
    <> pad("wall_us", 10)
    <> pad("since_gc", 12)
    <> pad("live", 10)
    <> pad("free", 10)
    <> pad("fired", 6)
    <> "outcome / stdout",
  )
  io.println(string.repeat("-", 120))

  case compile_load(p1, "arc_gcpt_p1") {
    Ok(m) -> matrix("P1-sync", m, 30)
    Error(e) -> io.println("P1 compile FAIL: " <> e)
  }
  case compile_load(p2, "arc_gcpt_p2") {
    Ok(m) -> matrix("P2-trail-then", m, 50)
    Error(e) -> io.println("P2 compile FAIL: " <> e)
  }
  case compile_load(p3, "arc_gcpt_p3") {
    Ok(m) -> matrix("P3-loop-in-then", m, 50)
    Error(e) -> io.println("P3 compile FAIL: " <> e)
  }
  case compile_load(p5, "arc_gcpt_p5") {
    Ok(m) -> matrix("P5-100utask", m, 50)
    Error(e) -> io.println("P5 compile FAIL: " <> e)
  }
  let _ = p4
}
