//// GC-probe (rdr force-threshold): does t_maybe_collect fire when >65K
//// cells are allocated in the compiled path? P1 (no microtask, drain sees
//// empty queue → t_maybe_collect never called) vs P2 (one microtask →
//// drain executes one job → t_maybe_collect called → threshold crossed →
//// t_collect fires). Timing: P2 with stock gc_threshold=65536 vs
//// gc_threshold=10^9 (neuters t_maybe_collect without editing rt_js_gc).
////
////     cd arc && gleam run -m emit_2core_gc_probe

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
import twocore/runtime/rt_js_gc.{type GcStats, GcStats}
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
// NOTE: object literal `({x:i})` currently crashes the compiled path
// (`error:badarg element(6,0)`) — see sibling probe run. Use the makeAdder
// closure-call pattern instead (verified: 1 cell/iter).

const alloc_body = "function m(x){return function(y){return x+y}}let a=m(5);let s=0;for(let i=0;i<100000;i++)s+=a(i);"

/// P1: 100K allocs, NO microtask.
const p1 = "function m(x){return function(y){return x+y}}let a=m(5);let s=0;for(let i=0;i<100000;i++)s+=a(i);console.log('done')"

/// P2: 100K allocs + one Promise.resolve().then microtask.
const p2 = "function m(x){return function(y){return x+y}}let a=m(5);let s=0;for(let i=0;i<100000;i++)s+=a(i);Promise.resolve().then(function(){console.log('cb')});console.log('done')"

/// P3: correctness — allocate 100K, force microtask, then read a value that
/// SHOULD survive collection (a is in a top-level let binding → global var
/// → pinned root via global object).
const p3 = "function m(x){return function(y){return x+y}}let a=m(5);let s=0;for(let i=0;i<100000;i++)s+=a(i);Promise.resolve().then(function(){console.log(a(10))});console.log('done')"

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

fn print_stats(label: String, s: GcStats) {
  let GcStats(live:, free:, next:, since_gc:) = s
  io.println(
    "  "
    <> label
    <> "next="
    <> int.to_string(next)
    <> " since_gc="
    <> int.to_string(since_gc)
    <> " live="
    <> int.to_string(live)
    <> " free="
    <> int.to_string(free),
  )
}

fn probe(label: String, mod: Atom, threshold: Int) {
  harness.buf_reset()
  let seed = seed_with(threshold)
  let s0 = rt_js_gc.stats(seed)
  let t0 = monotonic_time(Microsecond)
  let #(outcome, st) = ffi_apply_js_main(mod, seed)
  let dt = monotonic_time(Microsecond) - t0
  let s1 = rt_js_gc.stats(st)
  let stdout = rt_js_store.t_console_bytes(st)
  io.println("")
  io.println(
    "── " <> label <> " (gc_threshold=" <> int.to_string(threshold) <> ") ──",
  )
  io.println("  outcome  : " <> string.slice(string.inspect(outcome), 0, 200))
  io.println("  stdout   : " <> string.inspect(stdout))
  print_stats("pre  : ", s0)
  print_stats("post : ", s1)
  let fired = case s1.since_gc < s0.since_gc || s1.free > 0 {
    True -> "YES (since_gc reset and/or free>0)"
    False -> "no"
  }
  io.println("  t_collect fired? " <> fired)
  io.println("  time     : " <> int.to_string(dt) <> " µs")
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

fn time_reps(label: String, mod: Atom, seed: InstanceState, reps: Int) {
  let _ = ffi_apply_js_main(mod, seed)
  let t0 = monotonic_time(Microsecond)
  let #(_, st) = repeat_n(reps, fn() { ffi_apply_js_main(mod, seed) })
  let dt = monotonic_time(Microsecond) - t0
  let s = rt_js_gc.stats(st)
  io.println(
    label
    <> ": "
    <> int.to_string(dt)
    <> " µs / "
    <> int.to_string(reps)
    <> " = "
    <> int.to_string(dt / reps)
    <> " µs/run  (last since_gc="
    <> int.to_string(s.since_gc)
    <> " live="
    <> int.to_string(s.live)
    <> " free="
    <> int.to_string(s.free)
    <> ")",
  )
}

fn seed_plain() -> InstanceState {
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
}

fn try_compile(label: String, source: String) {
  let opts =
    emit_2core.CompileOpts(
      module_name: "arc_gcprobe_try",
      source_kind: emit_2core.AsScript,
      entry_name: "js_main",
    )
  case emit_2core.compile_source(source, opts) {
    Error(e) -> io.println(label <> ": emit FAIL " <> string.inspect(e))
    Ok(unit) ->
      case pipeline.compile_ir(unit.module, profiles.js_direct()) {
        Error(e) -> io.println(label <> ": lower FAIL " <> string.inspect(e))
        Ok(_) -> io.println(label <> ": ok")
      }
  }
}

pub fn main() {
  io.println("emit_2core GC probe — force-threshold")
  io.println("")
  io.println("── isolate what compiles ──")
  try_compile("promise-resolve  ", "Promise.resolve()")
  try_compile("promise-then-nul ", "Promise.resolve().then()")
  try_compile("let-p-then       ", "let p=Promise.resolve();p.then()")
  try_compile(
    "let-p-then-fn    ",
    "let p=Promise.resolve();p.then(function(){})",
  )
  try_compile(
    "let-p-then-log   ",
    "let p=Promise.resolve();p.then(function(){console.log('cb')})",
  )
  try_compile(
    "alloc+let-p-then ",
    alloc_body
      <> "let p=Promise.resolve();p.then(function(){});console.log('done')",
  )
  try_compile("p2-original      ", p2)
  try_compile("p3-original      ", p3)
  io.println("")

  io.println(
    "── seed sanity: does JsStore(..store, gc_threshold:) break alloc_since_gc? ──",
  )
  let sp = seed_plain()
  let sw = seed_with(65_536)
  print_stats("plain     : ", rt_js_gc.stats(sp))
  print_stats("with(65k) : ", rt_js_gc.stats(sw))
  let sw2 = seed_with(1_000_000_000)
  print_stats("with(10^9): ", rt_js_gc.stats(sw2))
  io.println("")

  let m1 = compile_load(p1, "arc_gcprobe_p1")
  let m2 = compile_load(p2, "arc_gcprobe_p2")

  io.println("── P1 (no microtask) with PLAIN seed ──")
  harness.buf_reset()
  let #(out1, st1) = ffi_apply_js_main(m1, sp)
  print_stats("post P1 plain: ", rt_js_gc.stats(st1))
  io.println("  stdout: " <> string.inspect(rt_js_store.t_console_bytes(st1)))
  io.println("  outcome: " <> string.slice(string.inspect(out1), 0, 100))

  io.println("")
  io.println("── P2 (one microtask) with PLAIN seed ──")
  harness.buf_reset()
  let t0 = monotonic_time(Microsecond)
  let #(out2, st2) = ffi_apply_js_main(m2, sp)
  let dt2 = monotonic_time(Microsecond) - t0
  print_stats("post P2 plain: ", rt_js_gc.stats(st2))
  io.println("  stdout: " <> string.inspect(rt_js_store.t_console_bytes(st2)))
  io.println("  outcome: " <> string.slice(string.inspect(out2), 0, 100))
  io.println("  time: " <> int.to_string(dt2) <> " µs")
}
