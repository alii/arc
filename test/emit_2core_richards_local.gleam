//// One-shot LOCAL call_time profile of the compiled richards module.
//// profile_file's top_n can't see arc_prof_* internals (no module_info/1);
//// this uses eprof_run which enumerates jsf_N/{s,t} + jN letrecs by name.
////
////     cd arc && gleam run -m emit_2core_richards_local
////
//// ── perf7 w-letrec-attrib: bucket → applies/run ────────────────────────────
//// Measured at perf6 HEAD (a7ebc74: block_let_case=T letrec_float=T; every
//// perf7_*=F, cfunref=F). Total 56,018 applies / 254 fns — matches
//// profile.gleam:804's 57,942/256 (drift = other in-tree perf7 host reclass).
////
////   bucket                                       applies/run   fns  emit-site
////   ─────────────────────────────────────────────────────────────────────────
////   (a) code_t MakeClosure eta-wrap                40,489      34  emit_core
////       `-js_main/3-anonymous-N-`; MakeClosure(f,[],_) →      MakeClosure
////       fun(…)->apply f/N when cfunref_zero_capture=False.    :1584 else
////   (b) Loop head (Scheduler.schedule `while`)      10,672       1  emit_core
////       `-jsf_18_t/2-jN/1-`; ir.Loop → recursive letrec.      emit_loop
////   (c) stmt.emit_if joins, Return-in-arm            4,627       5  emit_core
////       `-jsf_{36,38,40}_t/3-jN/{2,3}-` (Worker/Handler/       emit_if:5656
////       Device .run's `if(packet==null){…return…}`) —          `all_breaks_
////       arm contains ir.Return → all_breaks_local=False →      local`=False
////       block_can_let_case gate fails → materialize.
////   (d) l_miss cold-path (anf.share inner Block)      ~230     214  anf.share
////       `-jsf_N/3-jN/2-` at 1-2 calls each; richards is        :398 l_miss
////       monomorphic so the cold tier almost never fires.
////   ─────────────────────────────────────────────────────────────────────────
////   TOTAL                                           56,018     254
////
//// Per-lever elimination (isolated flip atop perf6 HEAD, applies/run):
////   perf5_cfunref_zero_capture=True   56,018 → 15,529   kills (a) entirely
////   perf7_cold_outline=True (x)       15,529 → 15,299   kills (d): 214 fns
////                                                       gone, only 230 applies
////   perf7_share_dup=True (w2)         15,529 → 15,529   NO-OP: get_prop_fast
////                                                       has ~4-7 miss refs,
////                                                       n_miss≤2 never true
////   perf7_nested_let_case (w1)        —      → —        UNSOUND (l_miss body
////                                                       Break(l_join) bypasses
////                                                       cold; flag OFF, see
////                                                       emit_core.gleam:5919)
////
//// SPEC EXPECTATION vs MEASURED: brief predicted (d) l_miss as "expected
//// majority" — it IS 214/254 fns (84%) but only 230/56,018 applies (0.4%).
//// Actual majority by APPLIES is (a) code_t wrappers (72%).
////
//// ONE IR shape whose elimination is highest-leverage: `MakeClosure(f,[],_)`
//// under cfunref=False — flipping `perf5_cfunref_zero_capture=True` (already
//// re-enabled in-tree with the codegen_ffi no_type_opt-retry guard) alone
//// takes 56,018 → 15,529. After that the 15,529 residual is 10,672 Loop +
//// 4,627 emit_if-with-Return + 230 l_miss — none of w1/w2/x touches (b)/(c),
//// so the perf7 W/X levers cap at ~15,299 (x) / ~15,529 (w2 no-op). Getting
//// below ~15k needs either (b) Loop → beam local-call (already is) or (c) an
//// emit_if let-case that treats an arm-terminal ir.Return as a diverging
//// leaf (it IS — Return never reaches cont; the `all_breaks_local` Return
//// veto at emit_core.gleam:5985 is over-strict for KValues cont).

import emit_2core_harness as harness
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
  let assert Ok(src) = simplifile.read("bench/v8-v7/richards_run.js")
  let opts =
    emit_2core.CompileOpts(
      module_name: "arc_prof_richards",
      source_kind: emit_2core.AsScript,
      entry_name: "js_main",
    )
  let assert Ok(unit) = emit_2core.compile_source(src, opts)
  let assert Ok(beam) = pipeline.compile_ir(unit.module, profiles.js_direct())
  let assert Ok(mod) =
    build_beam.load_module(
      atom.create("arc_prof_richards"),
      "arc_prof_richards",
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
  io.println("══ richards LOCAL call_time (arc_prof_richards) ══")
  eprof_run(mod, st)
}
