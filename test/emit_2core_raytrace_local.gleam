//// One-shot LOCAL call_time profile of the compiled raytrace module.
//// profile_file's top_n can't see arc_prof_* internals (no module_info/1);
//// this uses eprof_run which enumerates jsf_N/{s,t} + jN letrecs by name.
////
////     cd arc && gleam run -m emit_2core_raytrace_local

// ── z-raytrace-profile (2026-07-13, HEAD ≈ perf6 AFTER + block_let_case) ─────
// raytrace_run.js: 196,387 µs/run untraced  ·  133,379 cells/run
//
//   jsf_N by µs (LOCAL call_time; top-10)      calls        µs   ns/call
//   ────────────────────────────────────────────────────────────────────────────
//   jsf_6/3   (Class.create inner ctor)       66,596    65,741       987   22%
//   jsf_30/3                                  13,058    32,981     2,525   11%
//   jsf_63/3                                   3,328    26,862     8,071    9%
//   jsf_65/3  (rayTrace? 764 pixels)             764    25,941    33,954    9%
//   jsf_32/3                                   3,182    19,932     6,263    7%
//   jsf_14/3                                   6,326    18,647     2,947    6%
//   jsf_53/3                                  11,912    15,971     1,340    5%
//   jsf_48/3  (Sphere.intersect)               5,768    15,887     2,754    5%
//   jsf_25/3                                   6,493    13,878     2,137    5%
//   jsf_9/3                                   25,042    11,356       453    4%
//   ────────────────────────────────────────────────────────────────────────────
//   TOTAL jsf µs: 298,342   letrec-apply TOTAL: 287,259 (430 fns)
//   BIF: erlang:put/2 373,947  ·  erlang:setelement/3 570,065
//   letrec top: -jsf_5/3-anonymous-0-/3 (66,596 = code_t wrapper for jsf_6)
//               -js_main/3-anonymous-186- (26,298) …-158- (25,042) …-196- (19,596)
//
//   runtime top-10 by µs (profile_file, runs=1)       calls        µs   ns/call
//   ────────────────────────────────────────────────────────────────────────────
//   obj_ffi:-shaped_unflat/1-lc$^0/1-0-/2           931,111   111,252       119
//   rt_js_obj:t_new_arguments/3                      66,596    91,799     1,378
//   call_ffi:new_simple_apply/7                      66,598    77,254     1,160
//   obj_ffi:flush_ids/2                              66,694    36,383       545
//   gleam@dict:insert/3                              67,623    30,657       453
//   call_ffi:mono_apply/5                           148,352    21,469       144
//   call_ffi:ic_shaped_cold/9                       116,573    20,093       172
//   rt_js_store:t_cell_new/2                         66,779    19,374       290
//   rt_js_store:t_next_prop_seq/1                    67,077    13,789       205
//   call_ffi:t_call_method_ic/5                     148,357    13,762        92
//
//   targeted (per run):  t_new_arguments 66,596 · t_call_checked 1 ·
//     t_new_simple 66,600 · t_call_method_ic 148,357 · t_ic_get 51,994 ·
//     t_ic_set 137 · t_get_prop_any 47,372 · t_global_get_fast 98,436 ·
//     t_get_elem_fast 11,532 · t_cell_get 49,229
//
// VERDICT — #1 sink is (a) initialize.apply chain, but the specific leak is
//   **t_new_arguments still 66,596/run** (91,799 µs traced ≈ 9%): U's
//   `.apply(_, arguments)` fast-path FIRES (t_call_checked 66,597→1) and
//   forwards raw `_args` verbatim — but func.gleam:1255 `uses_args =
//   refs_args_stmts(body)` still returns True for the ctor body (per
//   func.gleam:738 note: refs_args_expr does NOT elide `X.apply(Y,arguments)`
//   because the same walker gates is_simple_abi_eligible), so init_arguments
//   (:1170) still emits `host("new_arguments")`. The Arguments object is
//   allocated and NEVER READ — pure dead-alloc. → z-apply-fires owns this;
//   fix = separate `uses_args_needs_object` walker that DOES carve out the
//   `.apply(_, arguments)`-only shape (or a post-check "if the only body
//   stmt is that call, skip init_arguments").
//
// #2 sink is (d) something else — **jsv_flush O(cells) teardown** (~148k µs
//   traced ≈ 15%): apply_js_main exit calls jsv_flush → flush_ids recurses
//   66,694 IDs → shaped_unflat 66,606× → LC 931,111 iter (avg 14 slots/obj).
//   obj_ffi.erl:754 `[element(I,C) || I <- lists:seq(4,N)]` — the list-comp
//   dominates. Not hot-loop; scales with cells/run. Lever: replace LC with
//   `erlang:delete_element/2`×3 or a hand-rolled tuple-tail copy; OR skip
//   flush entirely for cells whose pdict entry was never overlaid-written
//   (track a dirty bit — most 66k Vectors are read-only after ctor).
//
// #3 (a) new_simple machinery ~130k µs — 66,600 allocs × (new_simple_apply
//   + new_simple_warm + t_cell_new + t_next_prop_seq + jsv_install +
//   dict:insert). Irreducible per-`new` floor unless Vector/Color escape-
//   analyse to stack tuples.
//
// #4 (b) Vector/Color arithmetic — t_to_primitive 97,427 + t_to_numeric
//   51,922 + t_div 21,569 + to_numeric_operands 22,752 ≈ 40k µs. `.x/.y/.z`
//   reads ARE inline (t_ic_set=137 low; t_ic_get 51,994 is Flog.*-chain, not
//   Vector), but the ops themselves (`v.x*w.x` etc.) go through slow t_div/
//   t_to_numeric — likely `float × int` mixed-type coercion (raytrace uses
//   `2*t`, `1 - w` mixing int literals with float slots).
//
// #5 (c) Flog.RayTracer.* namespace — t_global_get_fast 98,436 +
//   t_get_prop_any 47,372 (proto-walk on plain-{} Flog/RayTracer). Every
//   `new Flog.RayTracer.Vector(...)` = 1 global + 2 own-data reads.
//   ic_shaped_cold 116,573 = mono-IC learning on the 148k method calls.
//
// raytrace at 177k already ≤287k gate — the two cheap wins (t_new_arguments
// carve-out + shaped_unflat LC → hand loop) alone are worth ~25% traced.
// ─────────────────────────────────────────────────────────────────────────────

import emit_2core_harness as harness
import emit_2core_profile
import gleam/erlang/atom.{type Atom}
import gleam/io
import simplifile
import twocore/backend/build_beam
import twocore/pipeline
import twocore/runtime/profiles
import twocore/runtime/rt_js_builtins
import twocore/runtime/rt_js_store
import twocore/runtime/rt_state.{type InstanceState}
import arc/compiler/emit_2core

@external(erlang, "emit_2core_profile_ffi", "eprof_run")
fn eprof_run(mod: Atom, st: InstanceState) -> Nil

@external(erlang, "emit_2core_profile_ffi", "reset")
fn trace_reset() -> Nil

@external(erlang, "twocore_rt_js_exec_ffi", "apply_js_main")
fn ffi_apply_js_main(mod: Atom, st: InstanceState) -> #(a, InstanceState)

pub fn main() {
  trace_reset()
  let assert Ok(src) = simplifile.read("bench/v8-v7/raytrace_run.js")
  let opts =
    emit_2core.CompileOpts(
      module_name: "arc_prof_raytrace",
      source_kind: emit_2core.AsScript,
      entry_name: "js_main",
    )
  let assert Ok(unit) = emit_2core.compile_source(src, opts)
  let assert Ok(beam) = pipeline.compile_ir(unit.module, profiles.js_direct())
  let assert Ok(mod) =
    build_beam.load_module(
      atom.create("arc_prof_raytrace"),
      "arc_prof_raytrace",
      beam,
    )
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
  // warm run untraced
  ffi_apply_js_main(mod, st)
  io.println("══ raytrace LOCAL call_time (arc_prof_raytrace) ══")
  eprof_run(mod, st)
  // runtime-module top-N (t_* FFI + rt_js_* rows) — categorises the jsf cost
  emit_2core_profile.profile_file("raytrace", "bench/v8-v7/raytrace_run.js", 1)
}
