//// GC memory-bytes probe (rdr unit `memory-bytes`).
////
//// For each N ∈ {0, 10k, 50k, 100k, 200k}: compile makeAdder(N) (1 store
//// cell/call — obj-literal `{x:i}` currently crashes emit_2core, see gc_probe
//// obj_prop badarg), run in a FRESH spawned process, and measure BEAM
//// process memory (erlang:garbage_collect + process_info(memory)) while the
//// child holds (a) the seeded-realm state, (b) the raw post-js_main state,
//// (c) the state after a forced `t_collect`, (d) nothing. Delta (b)−(c) is
//// bytes reclaimed by the JS-store GC; slope of (b)−(a) vs live cells is
//// bytes per live JsStore cell.
////
////     cd arc && gleam run -m emit_2core_gc_membytes

import arc/compiler/emit_2core
import emit_2core_harness as harness
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import twocore/backend/build_beam
import twocore/pipeline
import twocore/runtime/profiles
import twocore/runtime/rt_js_builtins
import twocore/runtime/rt_js_gc.{type GcStats}
import twocore/runtime/rt_js_store
import twocore/runtime/rt_state.{type InstanceState}

@external(erlang, "twocore_rt_js_exec_ffi", "apply_js_main")
fn ffi_apply_js_main(mod: Atom, st: InstanceState) -> #(Dynamic, InstanceState)

@external(erlang, "twocore_conformance_ffi", "gc_and_memory")
fn gc_and_memory(pid: Pid) -> Int

fn adder_src(n: Int) -> String {
  "function makeAdder(x){return function(y){return x+y}}let add5=makeAdder(5);let s=0;for(let i=0;i<"
  <> int.to_string(n)
  <> ";i++)s+=add5(i);s"
}

/// Same as adder_src but ends with a resolved-promise .then so the
/// generated js_main's terminal `drain_microtasks` iterates at least once,
/// which is the ONLY place `t_maybe_collect` is called (rt_js_async:287).
/// If stock GC fires, live_pre after js_main should already be ~572.
fn adder_src_p2(n: Int) -> String {
  adder_src(n) <> ";Promise.resolve().then(function(){})"
}

fn compile_mod(source: String, name: String) -> Atom {
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
  let #(_realm, st) = rt_js_builtins.init_realm(st)
  st
}

pub type Row {
  Row(
    n: Int,
    live_before: Int,
    live_after: Int,
    free_after: Int,
    since_gc_before: Int,
    mem_seed: Int,
    mem_no_gc: Int,
    mem_with_gc: Int,
    mem_drop: Int,
  )
}

type Msg {
  ChildReady(process.Subject(Cmd))
  Checkpoint(GcStats)
  KeepAlive(Int)
}

type Cmd {
  Go
}

/// Spawn a fresh process; child pauses at 4 checkpoints, parent measures
/// its memory at each. `st` is FORCED live across each measured `receive`
/// by reading it AFTER the receive (KeepAlive echo) so BEAM's live-range
/// analysis cannot free it early.
fn measure(n: Int) -> Row {
  measure_src(n, adder_src(n), "arc_gcmemb_" <> int.to_string(n))
}

fn measure_p2(n: Int) -> Row {
  measure_src(n, adder_src_p2(n), "arc_gcmemb_p2_" <> int.to_string(n))
}

fn measure_src(n: Int, src: String, name: String) -> Row {
  let mod = compile_mod(src, name)
  let reply = process.new_subject()

  let child =
    process.spawn(fn() {
      let inbox = process.new_subject()
      process.send(reply, ChildReady(inbox))

      // (a) seeded realm
      let st = seed()
      process.send(reply, Checkpoint(rt_js_gc.stats(st)))
      let assert Ok(Go) = process.receive(inbox, 60_000)
      process.send(reply, KeepAlive(rt_js_gc.stats(st).live))

      // (b) after js_main (no JS-store GC)
      let #(_outcome, st) = ffi_apply_js_main(mod, st)
      process.send(reply, Checkpoint(rt_js_gc.stats(st)))
      let assert Ok(Go) = process.receive(inbox, 60_000)
      process.send(reply, KeepAlive(rt_js_gc.stats(st).live))

      // (c) after forced t_collect
      let st = rt_js_gc.t_collect(st, [])
      process.send(reply, Checkpoint(rt_js_gc.stats(st)))
      let assert Ok(Go) = process.receive(inbox, 60_000)
      process.send(reply, KeepAlive(rt_js_gc.stats(st).live))

      // (d) st dropped — process baseline
      process.send(reply, Checkpoint(rt_js_gc.GcStats(0, 0, 0, 0)))
      let assert Ok(Go) = process.receive(inbox, 60_000)
    })

  let assert Ok(ChildReady(inbox)) = process.receive(reply, 60_000)

  let cp = fn(has_keepalive: Bool) {
    let assert Ok(Checkpoint(s)) = process.receive(reply, 60_000)
    let m = gc_and_memory(child)
    process.send(inbox, Go)
    case has_keepalive {
      True -> {
        let assert Ok(KeepAlive(_)) = process.receive(reply, 60_000)
        Nil
      }
      False -> Nil
    }
    #(s, m)
  }

  let #(_, mem_seed) = cp(True)
  let #(before, mem_no_gc) = cp(True)
  let #(after, mem_with_gc) = cp(True)
  let #(_, mem_drop) = cp(False)

  Row(
    n:,
    live_before: before.live,
    live_after: after.live,
    free_after: after.free,
    since_gc_before: before.since_gc,
    mem_seed:,
    mem_no_gc:,
    mem_with_gc:,
    mem_drop:,
  )
}

fn pad(n: Int, w: Int) -> String {
  string.pad_start(int.to_string(n), w, " ")
}

fn pad_s(s: String, w: Int) -> String {
  string.pad_start(s, w, " ")
}

fn print_row(r: Row) {
  let delta = r.mem_no_gc - r.mem_with_gc
  let fired = case r.since_gc_before < r.live_before {
    True -> " Y"
    False -> " n"
  }
  io.println(
    pad(r.n, 8)
    <> pad(r.live_before, 10)
    <> pad(r.since_gc_before, 10)
    <> fired
    <> pad(r.live_after, 10)
    <> pad(r.free_after, 10)
    <> pad(r.mem_seed, 12)
    <> pad(r.mem_no_gc, 14)
    <> pad(r.mem_with_gc, 14)
    <> pad(r.mem_drop, 12)
    <> pad(delta, 14),
  )
}

pub fn main() {
  io.println("emit_2core GC memory-bytes probe (fresh process per N)")
  io.println(
    "makeAdder(N): 1 store cell/call + 4 setup + 571 realm; obj-literal path crashes today",
  )
  io.println("")
  io.println(
    pad_s("N", 8)
    <> pad_s("live_pre", 10)
    <> pad_s("since_gc", 10)
    <> " F"
    <> pad_s("live_post", 10)
    <> pad_s("free_post", 10)
    <> pad_s("mem_seed", 12)
    <> pad_s("mem_no_gc", 14)
    <> pad_s("mem_gc", 14)
    <> pad_s("mem_drop", 12)
    <> pad_s("gc_freed_B", 14),
  )
  let ns = [0, 10_000, 50_000, 100_000, 200_000]
  let rows = list.map(ns, measure)
  list.each(rows, print_row)

  io.println("")
  io.println(
    "── P2: with trailing microtask (drain_microtasks fires t_maybe_collect) ──",
  )
  let rows_p2 = list.map(ns, measure_p2)
  list.each(rows_p2, print_row)

  io.println("")
  io.println("── slopes ──")
  case rows {
    [r0, ..] ->
      list.each(rows, fn(r) {
        case r.n {
          0 -> Nil
          _ -> {
            let dcells = r.live_before - r0.live_before
            let dnogc = r.mem_no_gc - r0.mem_no_gc
            let dgc = r.mem_with_gc - r0.mem_with_gc
            io.println(
              "  N="
              <> pad(r.n, 7)
              <> "  Δcells="
              <> pad(dcells, 8)
              <> "  Δmem_no_gc="
              <> pad(dnogc, 12)
              <> " ("
              <> pad(safe_div(dnogc, dcells), 4)
              <> " B/cell)"
              <> "  Δmem_gc="
              <> pad(dgc, 12)
              <> " ("
              <> pad(safe_div(dgc, dcells), 4)
              <> " B/cell)",
            )
          }
        }
      })
    _ -> Nil
  }
}

fn safe_div(a: Int, b: Int) -> Int {
  case b {
    0 -> 0
    _ -> a / b
  }
}
