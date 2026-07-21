//// GC memory-bytes probe (rdr unit `force-threshold-memory`).
////
//// For n ∈ {0,10K,100K,500K}: compile a JS program that allocates ~n cells
//// via the makeAdder pattern (1 cell/call, verified by sibling gc_probe:
//// adder_n0=+4, adder_n10=+14). Run in a FRESH child process; parent
//// measures child memory (`erlang:garbage_collect + process_info(memory)`)
//// at four checkpoints: (0) after init_realm, (1) after js_main returns
//// with GC neutered (threshold=10^9), (2) after a manual t_collect(st,[]),
//// (3) after js_main returns with as-shipped threshold=65536 + one
//// microtask (so drain fires t_maybe_collect). Column (1)−(0) / n → bytes
//// per JsStore cell; (1)−(2) → bytes freed by t_collect.
////
////     cd arc && gleam run -m emit_2core_gc_mem_probe

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
import twocore/runtime/rt_js_types.{JsStore}
import twocore/runtime/rt_state.{type InstanceState}

@external(erlang, "twocore_rt_js_exec_ffi", "apply_js_main")
fn ffi_apply_js_main(mod: Atom, st: InstanceState) -> #(Dynamic, InstanceState)

@external(erlang, "twocore_conformance_ffi", "gc_and_memory")
fn gc_and_memory(pid: Pid) -> Int

// ── programs ────────────────────────────────────────────────────────────────
// makeAdder: N calls × 1 cell/call + 4 setup cells. Object literals crash
// the compiled path today (badarg element/2), so closures stand in.

fn sync_src(n: Int) -> String {
  "function m(x){return function(y){return x+y}}let a=m(5);let s=0;for(let i=0;i<"
  <> int.to_string(n)
  <> ";i++)s+=a(i);s"
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

fn seed(threshold: Int) -> InstanceState {
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

// ── child process protocol ──────────────────────────────────────────────────
// Child parks at each checkpoint holding ONLY the current `st` live, sends
// stats to parent, and waits for `Go` before proceeding — so `gc_and_memory`
// (called by parent between the stats send and the Go) sees exactly that
// `st`'s reachable set.

type ToParent {
  Ready(process.Subject(Cmd))
  Parked(GcStats)
}

type Cmd {
  Go
}

fn park(
  reply: process.Subject(ToParent),
  inbox: process.Subject(Cmd),
  st: InstanceState,
) -> InstanceState {
  process.send(reply, Parked(rt_js_gc.stats(st)))
  let assert Ok(Go) = process.receive(inbox, 120_000)
  // Return `st` — forces it live across the receive (so the parent's
  // gc_and_memory reading includes it) and lets the caller rebind.
  st
}

fn child(mod: Atom, reply: process.Subject(ToParent)) -> Nil {
  let inbox = process.new_subject()
  process.send(reply, Ready(inbox))

  // cp0: realm only, GC neutered so alloc_since_gc is a raw counter.
  let st = seed(1_000_000_000)
  let st = park(reply, inbox, st)

  // cp1: after js_main, GC neutered → full store held.
  let #(_, st) = ffi_apply_js_main(mod, st)
  let st = park(reply, inbox, st)

  // cp2: after manual t_collect(st, []) → dead cells freed.
  let st = rt_js_gc.t_collect(st, [])
  let _ = park(reply, inbox, st)
  Nil
}

fn measure(mod: Atom, n: Int) -> Row {
  let reply = process.new_subject()
  let pid = process.spawn(fn() { child(mod, reply) })
  let assert Ok(Ready(inbox)) = process.receive(reply, 120_000)

  let cp = fn() {
    let assert Ok(Parked(s)) = process.receive(reply, 300_000)
    let m = gc_and_memory(pid)
    process.send(inbox, Go)
    #(s, m)
  }

  let #(s0, m0) = cp()
  let #(s1, m1) = cp()
  let #(s2, m2) = cp()

  Row(
    n:,
    realm_live: s0.live,
    realm_mem: m0,
    nogc_live: s1.live,
    nogc_mem: m1,
    coll_live: s2.live,
    coll_free: s2.free,
    coll_mem: m2,
  )
}

pub type Row {
  Row(
    n: Int,
    realm_live: Int,
    realm_mem: Int,
    nogc_live: Int,
    nogc_mem: Int,
    coll_live: Int,
    coll_free: Int,
    coll_mem: Int,
  )
}

// ── report ──────────────────────────────────────────────────────────────────

fn pad(x: Int, w: Int) -> String {
  string.pad_start(int.to_string(x), w, " ")
}

fn hdr(s: String, w: Int) -> String {
  string.pad_start(s, w, " ")
}

fn print_row(r: Row) -> Nil {
  let store_bytes = r.nogc_mem - r.realm_mem
  let cells_added = r.nogc_live - r.realm_live
  let per_cell = case cells_added {
    0 -> 0
    c -> store_bytes / c
  }
  let freed_bytes = r.nogc_mem - r.coll_mem
  let freed_cells = r.nogc_live - r.coll_live
  let per_freed = case freed_cells {
    0 -> 0
    c -> freed_bytes / c
  }
  io.println(
    pad(r.n, 8)
    <> pad(r.realm_live, 8)
    <> pad(r.realm_mem, 12)
    <> pad(r.nogc_live, 10)
    <> pad(r.nogc_mem, 14)
    <> pad(store_bytes, 12)
    <> pad(per_cell, 8)
    <> pad(r.coll_live, 10)
    <> pad(r.coll_free, 10)
    <> pad(r.coll_mem, 14)
    <> pad(freed_bytes, 12)
    <> pad(per_freed, 8),
  )
}

pub fn main() -> Nil {
  io.println("emit_2core GC memory probe (bytes/cell)")
  io.println("makeAdder(N): 1 store cell per call + ~4 setup")
  io.println(
    "cp0=after init_realm  cp1=after js_main (gc neutered)  cp2=after t_collect(st,[])",
  )
  io.println("")
  io.println(
    hdr("N", 8)
    <> hdr("rlm_liv", 8)
    <> hdr("rlm_mem", 12)
    <> hdr("run_liv", 10)
    <> hdr("run_mem", 14)
    <> hdr("Δmem", 12)
    <> hdr("B/cell", 8)
    <> hdr("gc_liv", 10)
    <> hdr("gc_free", 10)
    <> hdr("gc_mem", 14)
    <> hdr("freed_B", 12)
    <> hdr("B/freed", 8),
  )
  let ns = [0, 10_000, 100_000, 500_000]
  let rows =
    list.map(ns, fn(n) {
      let mod = compile_mod(sync_src(n), "arc_gcmem_" <> int.to_string(n))
      measure(mod, n)
    })
  list.each(rows, print_row)

  // Linear-fit bytes/cell from (n_max − n_min) — cancels realm overhead.
  io.println("")
  case rows {
    [_, r1, ..] -> {
      let assert Ok(rmax) = list.last(rows)
      let dcells = rmax.nogc_live - r1.nogc_live
      let dbytes = rmax.nogc_mem - r1.nogc_mem
      let slope = case dcells {
        0 -> 0
        d -> dbytes / d
      }
      io.println(
        "slope (nogc, "
        <> int.to_string(rmax.n)
        <> "−"
        <> int.to_string(r1.n)
        <> "): "
        <> int.to_string(slope)
        <> " bytes / live JsStore cell (SBox closure)",
      )
      let dbytes2 = rmax.coll_mem - r1.coll_mem
      let dfree = rmax.coll_free - r1.coll_free
      let slope2 = case dfree {
        0 -> 0
        d -> dbytes2 / d
      }
      io.println(
        "slope (post-collect free-list): "
        <> int.to_string(slope2)
        <> " bytes / freed id (Int on List)",
      )
      io.println("")
      io.println(
        "at 65536 cells (default gc_threshold): ~"
        <> int.to_string(slope * 65_536 / 1024)
        <> " KiB store; freeing recovers ~"
        <> int.to_string({ slope - slope2 } * 65_536 / 1024)
        <> " KiB (cells → free-list ints)",
      )
    }
    _ -> Nil
  }
}
