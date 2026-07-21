//// arc-oracle (rdr unit): does arc's `maybe_collect_at_toplevel` fire on
//// >65K-alloc programs, and what does it cost in bytes/µs?
////
//// Instrumentation lives in `arc_gc_oracle_ffi.erl` (process-dict counters)
//// and two @external hooks patched into `interpreter.gleam:maybe_collect_at_toplevel`.
//// Revert both after the run.
////
////     cd arc && gleam run -m arc_gc_oracle_probe

import arc/engine
import arc/vm/heap
import gleam/int
import gleam/io
import gleam/list
import gleam/string

@external(erlang, "arc_gc_oracle_ffi", "reset")
fn reset() -> Nil

@external(erlang, "arc_gc_oracle_ffi", "stats")
fn stats() -> #(Int, Int, Int)

@external(erlang, "arc_gc_oracle_ffi", "self_mem")
fn self_mem() -> Int

@external(erlang, "arc_gc_oracle_ffi", "now_us")
fn now_us() -> Int

@external(erlang, "arc_gc_oracle_ffi", "keep")
fn keep(a: a) -> a

type Prog {
  Prog(name: String, src: String)
}

/// Same shape as sibling `emit_2core_gc_mem_probe.sync_src`: N calls to a
/// no-alloc closure. Tests whether *calling* alone drives arc GC.
fn make_adder(n: Int) -> String {
  "function m(x){return function(y){return x+y}}let a=m(5);let s=0;for(let i=0;i<"
  <> int.to_string(n)
  <> ";i++)s+=a(i);s"
}

/// N object literals allocated inside a called function that returns to
/// toplevel each iteration — the shape most likely to trip the 65536 gate
/// AND reach the maybe_collect call site (fn return).
fn obj_via_call(n: Int) -> String {
  "function f(i){return {x:i}} let r; for(let i=0;i<"
  <> int.to_string(n)
  <> ";i++){r=f(i)} r.x"
}

/// N object literals with NO function call — maybe_collect_at_toplevel is
/// only reached on fn return (interpreter.gleam:3079), so this should show
/// checks=0 fires=0 regardless of N.
fn obj_no_call(n: Int) -> String {
  "let r; for(let i=0;i<" <> int.to_string(n) <> ";i++){r={x:i}} r.x"
}

/// N objects, all kept live in an array. GC should fire but reclaim ~nothing;
/// final read proves mark-phase kept element 0 across a mid-run collect.
fn obj_keep_all(n: Int) -> String {
  "function f(i){return {x:i}} let a=[]; for(let i=0;i<"
  <> int.to_string(n)
  <> ";i++){a.push(f(i))} a[0].x + a["
  <> int.to_string(n - 1)
  <> "].x"
}

fn programs() -> List(Prog) {
  [
    Prog("adder_10k", make_adder(10_000)),
    Prog("adder_100k", make_adder(100_000)),
    Prog("obj_nocall_100k", obj_no_call(100_000)),
    Prog("obj_call_100k", obj_via_call(100_000)),
    Prog("obj_call_200k", obj_via_call(200_000)),
    Prog("obj_keep_100k", obj_keep_all(100_000)),
    Prog("adder_500k", make_adder(500_000)),
  ]
}

fn pad(s: String, w: Int) -> String {
  s <> string.repeat(" ", int.max(0, w - string.length(s)))
}

fn run_one(p: Prog) -> Nil {
  reset()
  let m0 = self_mem()
  let t0 = now_us()
  let eng: engine.Engine(Nil) = engine.new()
  let r = engine.eval(eng, p.src)
  let t1 = now_us()
  let m1 = self_mem()
  // Keep the returned engine live across self_mem so its heap is counted.
  let _ = keep(r)
  let #(checks, fires, max_grown) = stats()
  let #(result, hs) = case r {
    Ok(#(engine.Returned(v), eng2)) -> #(
      string.inspect(v),
      heap.stats(engine.heap(eng2)),
    )
    Ok(#(engine.Threw(e), eng2)) -> #(
      "THREW " <> engine.format_error(eng2, e),
      heap.stats(engine.heap(eng2)),
    )
    Error(e) -> #(
      "ERR " <> engine.eval_error_message(e),
      heap.HeapStats(0, 0, 0, 0),
    )
  }
  io.println(
    pad(p.name, 18)
    <> " checks="
    <> pad(int.to_string(checks), 8)
    <> " fires="
    <> pad(int.to_string(fires), 4)
    <> " max_grown="
    <> pad(int.to_string(max_grown), 8)
    <> " next="
    <> pad(int.to_string(hs.next), 8)
    <> " live="
    <> pad(int.to_string(hs.live), 8)
    <> " wall_us="
    <> pad(int.to_string(t1 - t0), 10)
    <> " mem_delta="
    <> pad(int.to_string(m1 - m0), 12)
    <> " result="
    <> result,
  )
}

pub fn main() {
  io.println(
    "arc interpreter GC oracle — maybe_collect_at_toplevel instrumented",
  )
  io.println(
    "threshold=65536  call-site=interpreter.gleam:3079 (fn return only)",
  )
  io.println("")
  // Warmup: engine.new + eval once so first-row wall_us isn't JIT noise.
  let _ = engine.eval(engine.new(), "1+1")
  list.each(programs(), run_one)
}
