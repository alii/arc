//// call_time profile of the emit_2core-compiled hot loop. For each bench
//// (sum/adder/obj_prop): compile+load once, seed a realm once, install
//// erlang:trace_pattern([call_time,local]) on every rt_js_* module + FFI
//// shim + the compiled module, run js_main × `runs`, then dump the top-N
//// {M,F,A} by wall time. Also reports per-run wall clock and cell-alloc
//// count (js_store.alloc_since_gc delta).
////
////     cd arc && gleam run -m emit_2core_profile
////
//// Not a test — profiling harness only.

import arc/compiler/emit_2core
import emit_2core_bench.{adder_js, obj_js, sum_js}
import emit_2core_harness as harness
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{Some}
import gleam/string
import simplifile
import twocore/backend/build_beam
import twocore/pipeline
import twocore/runtime/profiles
import twocore/runtime/rt_js_builtins
import twocore/runtime/rt_js_store
import twocore/runtime/rt_state.{type InstanceState}

// ───────────────────────────── FFI ─────────────────────────────

@external(erlang, "emit_2core_profile_ffi", "trace_on")
fn trace_on(bench_mod: Atom) -> Nil

@external(erlang, "emit_2core_profile_ffi", "trace_off")
fn trace_off() -> Nil

@external(erlang, "emit_2core_profile_ffi", "reset")
fn trace_reset() -> Nil

@external(erlang, "emit_2core_profile_ffi", "count_of")
fn count_of(m: Atom, f: Atom, a: Int) -> Int

@external(erlang, "emit_2core_profile_ffi", "top_n")
fn top_n(n: Int) -> List(#(String, String, Int, Int, Int))

@external(erlang, "emit_2core_profile_ffi", "module_total")
fn module_total(m: Atom) -> Int

@external(erlang, "emit_2core_profile_ffi", "all_mods")
fn all_mods() -> List(Atom)

@external(erlang, "twocore_rt_js_exec_ffi", "apply_js_main")
fn ffi_apply_js_main(mod: Atom, st: InstanceState) -> #(Dynamic, InstanceState)

type TimeUnit {
  Microsecond
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(unit: TimeUnit) -> Int

// ───────────────────────────── setup ─────────────────────────────

fn compile_and_seed(source: String, name: String) -> #(Atom, InstanceState) {
  let opts =
    emit_2core.CompileOpts(
      module_name: name,
      source_kind: emit_2core.AsScript,
      entry_name: "js_main",
    )
  let assert Ok(unit) = emit_2core.compile_source(source, opts)
  let assert Ok(beam) = pipeline.compile_ir(unit.module, profiles.js_direct())
  let assert Ok(mod) = build_beam.load_module(atom.create(name), name, beam)
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
  #(mod, st)
}

fn repeat(times: Int, f: fn() -> a) -> Nil {
  case times {
    0 -> Nil
    _ -> {
      f()
      repeat(times - 1, f)
    }
  }
}

// ───────────────────────────── one profile ─────────────────────────────

/// Profile `runs` invocations of the compiled js_main. Returns nothing —
/// prints directly.
fn profile(label: String, source: String, runs: Int, iters: Int) -> Nil {
  let name = "arc_prof_" <> label
  // drop any prior trace patterns so untraced timing is clean
  trace_reset()
  let #(mod, seed) = compile_and_seed(source, name)

  // 1 warm run untraced (JIT + fast-path warm)
  ffi_apply_js_main(mod, seed)

  // cell-alloc probe: one untraced run, delta the store counter
  let assert Some(js_before) = seed.js_store
  let #(_v, st_after) = ffi_apply_js_main(mod, seed)
  let assert Some(js_after) = st_after.js_store
  let cells = js_after.alloc_since_gc - js_before.alloc_since_gc

  // untraced wall-clock (baseline — trace overhead is ~2-3×)
  let t0 = monotonic_time(Microsecond)
  repeat(runs, fn() { ffi_apply_js_main(mod, seed) })
  let untraced_us = monotonic_time(Microsecond) - t0

  // traced run
  trace_on(mod)
  let t1 = monotonic_time(Microsecond)
  repeat(runs, fn() { ffi_apply_js_main(mod, seed) })
  let traced_us = monotonic_time(Microsecond) - t1
  trace_off()

  io.println("")
  io.println(
    "══════ "
    <> label
    <> "  (runs="
    <> int.to_string(runs)
    <> " × 1M iter, cells/run="
    <> int.to_string(cells)
    <> ") ══════",
  )
  io.println(
    "  wall untraced: "
    <> int.to_string(untraced_us)
    <> " µs total, "
    <> int.to_string(untraced_us / runs)
    <> " µs/run",
  )
  io.println(
    "  wall traced:   "
    <> int.to_string(traced_us)
    <> " µs total ("
    <> int.to_string(traced_us * 100 / int.max(1, untraced_us))
    <> "% of untraced)",
  )

  // per-module rollup
  io.println("  ── per-module µs (traced) ──")
  let mods = [mod, ..all_mods()]
  list.each(
    list.sort(
      list.filter_map(mods, fn(m) {
        case module_total(m) {
          0 -> Error(Nil)
          us -> Ok(#(atom.to_string(m), us))
        }
      }),
      fn(a, b) { int.compare(b.1, a.1) },
    ),
    fn(row) {
      io.println(
        "    "
        <> string.pad_end(row.0, 42, " ")
        <> " "
        <> string.pad_start(int.to_string(row.1), 10, " ")
        <> " µs  "
        <> string.pad_start(int.to_string(row.1 * 100 / traced_us), 3, " ")
        <> "%",
      )
    },
  )

  // fast-path probe: is CallClosure fast-path actually taken?
  let fast =
    count_of(atom.create("twocore_rt_js_call_ffi"), atom.create("t_kfn_code"), 3)
  let slow =
    count_of(
      atom.create("twocore@runtime@rt_js_call"),
      atom.create("t_call_checked"),
      4,
    )
  case fast + slow {
    0 -> Nil
    _ ->
      io.println(
        "  fast-path: kfn_code="
        <> int.to_string(fast)
        <> " ("
        <> int.to_string(fast / runs)
        <> "/run)  t_call_checked(slow)="
        <> int.to_string(slow)
        <> " → fast-path "
        <> case slow {
          0 -> "TAKEN"
          _ -> "MISSED " <> int.to_string(slow) <> "×"
        },
      )
  }

  // top-N functions
  io.println("  ── top functions by µs (call_time) ──")
  let rows = top_n(25)
  io.println(
    "    "
    <> string.pad_end("module:function/arity", 55, " ")
    <> string.pad_start("count", 12, " ")
    <> string.pad_start("µs", 12, " ")
    <> string.pad_start("ns/call", 9, " ")
    <> string.pad_start("µs/run", 9, " ")
    <> string.pad_start("ns/iter", 9, " "),
  )
  list.each(rows, fn(row) {
    let #(m, f, a, count, us) = row
    let ns_call = case count {
      0 -> 0
      _ -> us * 1000 / count
    }
    io.println(
      "    "
      <> string.pad_end(short(m) <> ":" <> f <> "/" <> int.to_string(a), 55, " ")
      <> string.pad_start(int.to_string(count), 12, " ")
      <> string.pad_start(int.to_string(us), 12, " ")
      <> string.pad_start(int.to_string(ns_call), 9, " ")
      <> string.pad_start(int.to_string(us / runs), 9, " ")
      <> string.pad_start(int.to_string(us * 1000 / { runs * iters }), 9, " "),
    )
  })
}

fn short(m: String) -> String {
  // "twocore@runtime@rt_js_obj" → "rt_js_obj"
  case string.split(m, "@") {
    [_, _, tail] -> tail
    _ -> m
  }
}

// ───────────────────────────── file-based profile ─────────────────────────────

// ── BASELINE (a2881bb, pre-G/H/I — captured 2026-07-12, runs=3) ──────────────
// richards_run.js:  10,336 µs/run   118 cells/run
//
//   top-25 by µs (traced, call_time)           count       µs  ns/call  µs/run
//   ────────────────────────────────────────────────────────────────────────────
//   t_call_method_ic/5                       121,398   14,200      116   4,733
//   t_get_elem_fast/3                         12,756    2,129      166     709
//   slot_of/2                                 13,560      652       48     217
//   t_set_elem_fast/4                          2,826      635      224     211
//   elem_read/2                               12,756      619       48     206
//   rt_js_val:primitive_to_prop_key/2          2,826      572      202     190
//   t_bitor_fast/2                            14,196      501       35     167
//   rt_js_val:t_to_property_key/2              2,826      461      163     153
//   t_cell_get/2 (store_ffi)                   3,960      377       95     125
//   rt_js_obj:set_from/5                       1,380      337      244     112
//   rt_js_obj:own_property_of/4                1,782      289      162      96
//   rt_js_obj:read_object/2                    2,331      266      114      88
//   classify/1 (val_ffi)                       7,083      256       36      85
//   overlay_prop/3                             2,034      238      117      79
//   jsv_overlay_slot/2                           399      224      561      74
//   rt_js_store:t_cell_set/3                     774      204      263      68
//   rt_js_store:t_next_prop_seq/1                984      204      207      68
//   rt_js_store:t_cell_update/3                  774      183      236      61
//   gleam_stdlib:map_get/2                     2,346      177       75      59
//   elem_write/3                               2,826      175       61      58
//   gleam@dict:insert/3                        1,791      168       93      56
//   rt_js_obj:set_own_string/8                   522      166      318      55
//   rt_js_obj:set_on_receiver/4                  522      165      316      55
//   t_shr_fast/2                               2,997      120       40      40
//   t_bitand_fast/2                            2,997      114       38      38
//
//   targeted counts (per run)          — diff target for G/H/I/K —
//   ────────────────────────────────────────────────────────────────────────────
//   t_get_prop_any/3                    41    t_set_prop_any/4            142
//   t_call_checked/4                     0    t_kfn_code/3                  1
//   t_construct/4                        9    t_instance_of/3               0
//   t_get_prop_own_data/3              106    t_set_prop_own_data/4       143
//   t_global_get/2 (slow)                0    t_global_get_fast/2          65
//   t_get_elem_fast/3                4,252    elem_read/2               4,252
//   t_set_elem_fast/4                  942    elem_write/3                942
//   t_to_property_key/2                942    t_cell_get/2 (ffi)        1,320
//   t_call_method_ic/5              40,466    t_new_simple/3               32
//   t_ic_get/4                           0    t_ic_set/5                    0
//
//   VERIFY note: spec assumed t_global_get(_fast) ≈ 41k/run — STALE. That
//   was the pre-a2881bb count for STATE_*/ID_*/KIND_* int-const reads, which
//   analyze_const_globals (expr.gleam:533) already inlines to 0 call_ext.
//   Residual 65/run = non-const top-level fn refs (`new Scheduler`, `Packet`,
//   …) during setup only. The 40k-class hot row is t_call_method_ic/5
//   (4,733 µs/run ≈ 46% of wall) — H+I target it via receiver shape, not G.
//   Next-3 hottest after method_ic: t_get_elem_fast, slot_of, t_set_elem_fast.
//
//   micro (profile_fresh.txt, 1M calls, nop-corrected ns/call):
//     cell_get 5 · kfn_code 10 · get_prop_own_data 15 · get_prop_any 30
//     set_prop_own_data 35 · set_prop_any 138
// ─────────────────────────────────────────────────────────────────────────────

// ── AFTER (a2881bb+wt, post-G/H/I — captured 2026-07-12, runs=3) ─────────────
// richards_run.js:  14,648 µs/run   144 cells/run  (gate best-of-5: 16,034 µs)
//
//   top-25 by µs (traced, call_time)           count       µs  ns/call  µs/run
//   ────────────────────────────────────────────────────────────────────────────
//   t_ic_get/4                               484,308   65,262      134  21,754
//   slot_of/2                                484,578   24,475       50   8,158
//   t_call_method_ic/5                       121,398   21,984      181   7,328
//   ic_write/9                               151,977   20,888      137   6,962
//   t_ic_set/5                               152,151   18,079      118   6,026
//   rt_js_store:t_cell_get/2                 124,008    9,453       76   3,151
//   t_cell_get/2 (store_ffi)                 126,690    7,936       62   2,645
//   mono_shaped_own/4                        121,395    7,226       59   2,408
//   t_eq_fast/2                               54,267    1,836       33     612
//   t_bitand_fast/2                           35,010    1,171       33     390
//   t_get_elem_fast/3                         12,756      864       67     288
//   t_set_elem_fast/4                          2,826      590      208     196
//   t_bitor_fast/2                            14,196      483       34     161
//   elem_write/3                               2,826      153       54      51
//   rt_js_obj:own_property_of/4                  927      146      157      48
//   rt_js_store:t_cell_set/3                     546      144      263      48
//   t_shr_fast/2                               2,997      137       45      45
//   gleam@dict:insert/3                        1,335      128       95      42
//   rt_js_obj:set_from/5                         558      126      225      42
//   rt_js_store:t_next_prop_seq/1                678      125      184      41
//   rt_js_obj:read_object/2                    1,170      121      103      40
//   -t_define_own_prop/4-anonymous-3-/7          252      108      428      36
//   rt_js_store:t_cell_update/3                  468      105      224      35
//   jsv_evict/1                                  573      102      178      34
//   rt_js_store:t_cell_new/2                     363      100      275      33
//
//   targeted counts (per run)            before → after       Δ
//   ────────────────────────────────────────────────────────────────────────────
//   t_global_get_fast/2                    65 →      10        -55  (G partial)
//   t_global_get/2 (slow)                   0 →       0         +0
//   t_get_prop_own_data/3                 106 →      30        -76  (H/I fired)
//   t_set_prop_own_data/4                 143 →      58        -85  (H/I fired)
//   t_ic_get/4                              0 → 161,436   +161,436  (I fired)
//   t_ic_set/5                              0 →  50,717    +50,717  (I fired)
//   t_new_simple/3                         32 →      32         +0
//   t_call_method_ic/5                 40,466 →  40,466         +0
//   t_cell_get/2 (ffi)                  1,320 →  42,230    +40,910  (G slots)
//   t_kfn_code/3                            1 →       1         +0
//   t_get_elem_fast/3                   4,252 →   4,252         +0
//   t_set_elem_fast/4                     942 →     942         +0
//
//   ══ final-bench gate: FAIL ══
//     ✓ richards_run.js prints ok    ✓ deltablue_run.js prints ok
//     ✗ richards 4,324 µs/run (best of 5; target ≤3,000) — 1.44× over,
//       2.4× FASTER than the 10,336 µs baseline (was 1.55× slower).
//   Applied: (1) SShapedObject.slots → plain tuple (ShapeSlots) so warm hit
//   is `element(Off1, Slots)` BIF not array:get. (2) ic_set JMut→JRead —
//   writes land in pdict overlay, no St rebuild. (3) INLINE the warm hit in
//   emitted IR: `.x` = pdict[id] + pdict[SiteKey] + element, zero call_ext
//   (t_ic_get 161k→118/run, t_ic_set 50k→125/run). (4) known_handle: `this`/
//   `new` receivers skip is_tuple∧tag=:=js_cell guard. (5) t_call_method_ic
//   pdict-first for shaped receivers — skip Store.data lookup on warm hit.
//   Remaining gap ≈ 40k method-dispatch × ~25ns + 161k inline reads × ~11ns
//   (near BEAM floor: 2 pdict gets + Sid compare + element). ≤3000 needs
//   per-method-entry `this` slot hoist (thread pdict[this_id] as ANF local
//   across writes) or compile-time offset resolution.
// ─────────────────────────────────────────────────────────────────────────────

// ── K: get/set_elem_fast verify (a2881bb baseline, 2026-07-12) ───────────────
// EMIT-side (expr.gleam:1362/1375 helpers, called at ~1398/2058/2077):
//   get_elem_fast / set_elem_fast is emitted UNCONDITIONALLY for every
//   ast.Bracket read/write — static_dot_key returns None on Bracket, so the
//   only compile-time non-emit is a static `.name` (get_prop_fast instead).
//   No AST shape defeats emission; the gate is purely runtime (FFI).
//
// RUNTIME gate (twocore_rt_js_obj_ffi.erl:306/347):
//   is_integer(Idx)∧Idx≥0 ∧ slot=s_object{array_obj,Len} ∧ Idx<Len ∧
//   no {index,Idx} own-prop ∧ dense/sparse has value (holes miss).
//
// richards (baseline above): FIRES. get 4,252/run · set 942/run; elem_read
//   == t_get_elem_fast and elem_write == t_set_elem_fast → 100% pass the
//   outer s_object/array_obj gate. t_get_prop_any=41 t_set_prop_any=142 are
//   dominated by non-elem callers, so slow-path fallthrough is negligible.
//   NB t_to_property_key=942 is NOT miss — emit_lvalue coerces the bracket
//   key eagerly (§6.2.5 share-once for `o[e]+=v`) before the fast-path
//   probe, so every lvalue `o[e]=v` pays one to_property_key regardless of
//   hit. That is 190 µs/run ≈ 1.8% of wall — a candidate cut (defer coercion
//   into the miss arm for plain `=`; only RMW `+=`/`++` needs the shared k).
//
// deltablue arr[i] sites (all OrderedCollection, elms = new Array()+push):
//   :70  this.elms[index]  read   idx=int param       → HIT (hot: Plan.at)
//   :84  this.elms[i]      read   idx=for-int i++     → HIT
//   :86  this.elms[index]= write  idx=local int++     → HIT
//
// raytrace arr[i] sites:
//   :49  destination[property]=source[property]  idx=for-in STRING key on
//        plain Object → MISS (is_integer fails). Cold: Object.extend runs
//        once per Class.create at setup, ≈50 calls total, not hot-loop.
//   :691 scene.shapes[i]  read  idx=for-int, recv=new Array()+push → HIT
//   :721 scene.lights[i]  read  idx=for-int, recv=new Array()+push → HIT
//   :898 pixelSize[0]/[1] read  idx=int-literal, recv=String.split → HIT
//
// CONCLUSION: fast path fires on every hot-loop `arr[i]` in all three
//   benches. The single miss shape is for-in string-keyed Object.extend
//   (raytrace setup only). No emit-side gate widening needed. H-shape adds
//   an s_shaped_object receiver → miss clause but arrays remain s_object so
//   K is unaffected. Remaining K-adjacent cost is the eager to_property_key
//   in emit_lvalue (see NB above), not the probe.
// ─────────────────────────────────────────────────────────────────────────────

// ── L: state-tuple audit (3b198d8, 2026-07-12) ───────────────────────────────
// ALREADY DONE. emit_core.gleam:3706-3715 — the JRead arm emits a bare-V
//   `call_ext` with `sc` UNCHANGED (no `#(V,St')` unpack, no rebind). Every
//   hot read op is JRead in resolve_js: ic_get/ic_set:3914-3915,
//   get_prop_own_data:3893, set_prop_own_data:3908, get_elem_fast:3923,
//   kfn_code:3989. So the per-read state-tuple alloc L targets is 0/run.
//
// JMut→JRead demotion audit: set_elem_fast rebuilds St on hit
//   (obj_ffi.erl:413 `setelement(9, St, …)` — no pdict overlay for indexed
//   elements); call_method_ic tail-calls user `Code(St, …)` → `{V,St'}`
//   (call_ffi.erl:167). Both genuinely mutate — stay JMut. to_property_key
//   on primitives (int/str/sym — the richards `o[i]` shape) does NOT touch
//   St (rt_js_val.gleam:632-659); only KHandle → ToPrimitive → user code
//   does. SPLIT: `to_property_key_fast` (JPure, val_ffi.erl:86) covers the
//   primitive shapes; expr.to_property_key wraps it with an IsAtom→miss
//   fallback to the JMut op.
//
// Remaining per-call tuple allocs on the richards hot path (40,466/run each):
//   (a) call_method_ic `{V,St'}` return — NECESSARY (user Code mutates St);
//   (b) `{Recv,FnH,undef,undef}` frame tuple (call_ffi.erl:167) — → unit O
//       this-abi eliminates it via `CodeT(St, [Recv | pos])`;
//   (c) `cons_list(pos)` args list — → units O this-abi + M/N
//       inline-method-ic-warm eliminate it (positional params, no cons).
// ─────────────────────────────────────────────────────────────────────────────

// ── perf5 BASELINE (3b198d8, 2026-07-12) ─────────────────────────────────────
// Fresh capture at HEAD — the AFTER block above was mid-WIP; this pins where
// the 4.6k µs actually goes so L/M/N/O/P/Q can be ranked.
//
// richards_run.js:  4,683 µs/run   118 cells/run   (gate best-of-5: 4,951 µs)
// obj_prop micro:  20,647 µs/run   ← REGRESSED (perf4 target ≤11,800)
//
//   richards top-25 by µs (traced, call_time)    count       µs  ns/call  µs/run
//   ────────────────────────────────────────────────────────────────────────────
//   t_call_method_ic/5                         121,398    8,996       74   2,998  ← 64%
//   t_get_elem_fast/3                           12,756      817       64     272
//   t_set_elem_fast/4                            2,826      631      223     210
//   t_bitor_fast/2                              14,196      476       33     158
//   own_property_of/4                            1,119      222      198      74
//   t_cell_get/2 (store_ffi)                     3,258      188       57      62
//   set_from/5                                     750      187      249      62
//   elem_write/3                                 2,826      173       61      57
//   read_object/2                                1,458      164      112      54
//   t_cell_update/3                                564      157      278      52
//   t_next_prop_seq/1                              774      154      198      51
//   dict:insert/3                                1,371      153      111      51
//   t_cell_set/3                                   564      143      253      47
//   set_on_receiver/4                              312      126      403      42
//   map_get/2                                    1,473      123       83      41
//   ic_proto_walk/11                               114      121    1,061      40
//   -t_define_own_prop/4-anonymous-3-/7            252      114      452      38
//   t_bitand_fast/2                              2,997      109       36      36
//   t_define_own_prop/4                            252      108      428      36
//   t_shr_fast/2                                 2,997      104       34      34
//   t_with_js_store/2                            1,626      103       63      34
//   overlay_prop/3                                 705      103      146      34
//   set_own_string/8                               312      100      320      33
//   jsv_evict/1                                    591       90      152      30
//   t_cell_new/2                                   285       85      298      28
//
//   richards targeted counts (per run)
//   ────────────────────────────────────────────────────────────────────────────
//   t_call_method_ic/5         40,466    t_ic_get/4              118  (inline hit)
//   t_ic_set/5                    125    t_cell_get/2 (ffi)    1,086
//   t_get_elem_fast/3           4,252    t_set_elem_fast/4       942
//   t_get_prop_own_data/3          30    t_set_prop_own_data/4    58
//   t_to_property_key/2            14    t_global_get_fast/2      65
//   t_new_simple/3                 32    t_kfn_code/3              1
//   slot_of/2                     n/a    mono_shaped_own/4       n/a  (below top-25)
//
//   L/M/N/O ranking by hottest row attacked:
//     M+N inline-method-ic-warm  → t_call_method_ic 40,466×74ns = 2,998µs  #1
//     O   this-abi               → same 40k row (frame tuple + args cons)  #1
//     L   JRead reclass          → DONE (see L block above); residual ≈ 0
//   → richards ≤2,200 needs O + M/N together on the single 2,998µs row.
//
// deltablue: 19,650 µs/run  2,468 cells/run
//   t_call_method_ic 115,440/run · t_ic_get 20,099 · ic_proto_walk 4,716
//   t_get_prop_own_data 19,955 · t_global_get_fast 37,317 — inline shaped
//   path MISSING a lot (t_ic_get≈t_get_prop_own_data → cold-tier every time).
//
// raytrace: 415,854 µs/run  133,379 cells/run
//   t_ic_get 137,147/run · t_get_prop_own_data 132,594 · t_new_simple 66,600
//   → Q verdict: t_new_simple FIRES (66k Vector allocs) but inline shaped
//   path does NOT — t_ic_get high, own_data high. Vector ctor-body-assigned
//   fields (`this.x=x`) are not shape-learned → receivers stay s_object.
//
// crypto: 252,745 µs/run  3,930 cells/run  (P — first capture)
//   crypto top-10 by µs                          count       µs  ns/call
//   ────────────────────────────────────────────────────────────────────────────
//   t_set_elem_fast/4                          868,243  186,558      214
//   t_get_elem_fast/3                        2,432,497  167,956       69
//   t_shr_fast/2                             2,324,601   81,038       34
//   elem_write/3                               792,619   47,763       60
//   set_on_receiver/4                           76,697   33,337      434
//   own_property_of/4                          124,336   32,147      258
//   t_shl_fast/2                               771,995   28,096       36
//   read_object/2                              201,124   24,072      119
//   set_own_string/8                            76,697   24,057      313
//   t_cell_get/2 (store_ffi)                   435,843   23,937       54
//   targeted: t_set_prop_any 75,751/run · t_to_property_key 107,737/run ·
//   t_ic_get 4,443 · t_call_method_ic 80,354 · t_global_get_fast 85,923.
//   → P verdict: `this.array` reads DO hit inline shaped path (t_ic_get low
//   vs 2.4M elem reads); leak is 76k array WRITES falling to t_set_prop_any
//   (set_elem_fast 868k − elem_write 792k = 76k miss → slow set_own_string).
//   Candidate: index-grows-past-Len on `w_array[j++]=` — needs elem_fast
//   auto-extend, not a new fast path.
// ─────────────────────────────────────────────────────────────────────────────

// ── Q: raytrace shaped-path verify (3b198d8, 2026-07-12) ─────────────────────
// raytrace_run.js: 345,887 µs/run  133,379 cells/run
//
//   top-10 by µs (traced, call_time)             count       µs  ns/call
//   ────────────────────────────────────────────────────────────────────────────
//   new_simple_apply/7                          66,598  167,261    2,511  ← 48%
//   dict:insert/3                              314,582  115,096      365
//   rt_js_obj:get_from/4                       416,928  106,254      254
//   rt_js_obj:own_property_of/4                462,099   93,221      201
//   -as_sobject/2-anonymous-0-/4               192,663   71,472      370
//   ic_proto_walk/11                            66,642   68,939    1,034
//   rt_js_obj:t_new_arguments/3                 66,596   56,873      854
//   rt_js_obj:t_get_prop/3                     345,812   58,173      168
//   t_cell_get/2 (store_ffi)                   619,290   42,160       68
//   t_ic_get/4                                 137,147   31,470      229
//
//   targeted (per run): t_ic_get 137,147 · t_ic_set 24,444 ·
//     t_call_method_ic 148,357 · t_new_simple 66,600 · t_call_checked 66,597
//     t_new_arguments 66,596 · t_global_get_fast 98,436 · t_get_prop_any
//     118,484 · t_get_elem_fast 11,532 (elem_read 0 = inlined, not miss).
//
// VERDICT: t_ic_get ≈ 0? — NO (137k/run). Inline shaped path does NOT
//   dominate. t_call_method_ic (148k) is the top driver as expected, but
//   the `.x/.y/.z` inline hit that fires for richards does NOT here.
//
// SHAPE-LEARNING: FIRES. t_new_simple gate passes (66,598 new_simple_apply
//   ≈ 66,600 t_new_simple) and props_shapeable admits ctor-body-assigned
//   fields — cold path inspects post-ctor own props (call_ffi.erl:526
//   jsv_overlay_slot → :530 props_shapeable), so `this.x=v` inside
//   `initialize` counts. Vectors ARE SShapedObject on the warm path. The
//   perf5-BASELINE line above ("receivers stay s_object") is wrong.
//
// WHY t_ic_get stays high — three raytrace-specific leaks:
//   (1) Class.create tax. Every ctor is `function(){this.initialize.apply
//       (this,arguments)}` (raytrace.js:35). Per `new`: `this.initialize`
//       is a proto-walk (never own → t_ic_get miss → t_get_prop_any),
//       `arguments` allocs (t_new_arguments 66,596), `.apply` is
//       Function.prototype.apply → t_call_checked 66,597 → call_kfunction.
//       new_simple_apply's 167kµs is ~all of this. 66,600 × the lot.
//   (2) Fresh-instance pdict-cold. new_simple_warm (call_ffi.erl:505)
//       writes the SShapedObject to Store.data ONLY — no jsv_install — so
//       pdict[NewId]=undefined until the first t_ic_get/set on that cell
//       pulls slot_of and installs it. Every one of 66,600 fresh cells
//       pays ≥1 inline-miss → t_ic_get before the shaped tuple lands in
//       pdict. richards allocates 32 objects total so never notices.
//   (3) Proto-default fields. IntersectionInfo/Color/Material declare
//       defaults on the prototype literal (`isHit:false, position:null,
//       …`) and initialize() assigns a SUBSET, so the learned shape has
//       fewer slots than the read-set → `info.isHit` etc. are proto-walks
//       until first write, and each write is a shape TRANSITION → sid
//       mismatch at every monomorphic inline site → t_ic_get.
//
// Math.* → JsMathFfi direct-dispatch: WIRED (expr.gleam:1200-1204 →
//   emit_core.gleam:4024-4029 JPure). No twocore_rt_js_math_ffi row in
//   top-25 → either firing (JPure = single BIF, sub-top-25 cost) or
//   drowned by (1)-(3); not the bottleneck either way.
//
// FIX (in perf5 scope, ranked):
//   Q-a  new_simple_apply: `put(NewId, NewSlot)` right after the
//        Data#{NewId=>NewSlot} write (call_ffi.erl:563) so fresh
//        SShapedObject cells are pdict-warm on birth. One line; kills
//        leak (2) — the only Q leak addressable without source-pattern
//        knowledge. richards unaffected (32 allocs).
//   Q-b  (out of perf5) shape-learn from the prototype LITERAL's own data
//        keys ∪ ctor-body keys, so IntersectionInfo warm-allocs the full
//        7-slot shape with proto defaults pre-filled → kills leak (3).
//   Q-c  (out of perf5) recognise `this.M.apply(this,arguments)` ctor
//        bodies at emit and lower `new F(a,b,c)` straight to
//        t_call_method_ic(newThis,"M",[a,b,c]) — kills leak (1)'s
//        arguments alloc + t_call_checked + `this.initialize` proto-walk.
// ─────────────────────────────────────────────────────────────────────────────

// ── P: crypto this.array/elem_fast verify (3b198d8, 2026-07-12) ──────────────
// crypto_run.js: 269,900 µs/run  3,930 cells/run  (prints ok ✓)
// hot kernel = am3 (crypto.js:107, 28-bit digits): 66,393 calls, 744,269 inner
// iters (matches node ±0.01% — random pkcs1 pad only). Per iter: 3× arr[i]
// reads + 1× arr[j++]=v write + 5× >>/<< + 3× &.
//
//   top-10 by µs (traced, call_time)             count       µs  ns/call
//   ────────────────────────────────────────────────────────────────────────────
//   t_set_elem_fast/4                          868,243  195,323      224
//   t_get_elem_fast/3                        2,432,497  174,739       71
//   t_shr_fast/2                             2,324,601   84,279       36
//   elem_write/3                               792,619   50,444       63
//   rt_js_obj:set_on_receiver/4                 76,697   34,911      455
//   rt_js_obj:own_property_of/4                124,336   33,600      270
//   t_shl_fast/2                               771,995   29,413       38
//   rt_js_obj:set_own_string/8                  76,697   25,524      332
//   t_cell_get/2 (store_ffi)                   435,843   25,118       57
//   rt_js_obj:read_object/2                    201,124   24,969      124
//
//   targeted counts (per run)            hit-rate inference
//   ────────────────────────────────────────────────────────────────────────────
//   t_get_elem_fast/3       2,432,497    t_get_prop_any/3        36,359
//   t_set_elem_fast/4         868,243    t_set_prop_any/4        75,751
//   elem_write/3              792,619    t_to_property_key/2    107,737
//   t_ic_get/4                  4,443    t_ic_set/5                 264
//   t_call_method_ic/5         80,354    t_new_simple/3             184
//   t_global_get_fast/2        85,923    t_cell_get/2           435,843
//
//   VERDICT — `this.array` (BigInteger ctor: `this.array=new Array()`):
//     FIRES. am3 alone reads `.array` 2×66,393 ≈ 133k; whole bench >>200k
//     `.x` reads on `new BigInteger()` receivers. t_ic_get FFI = 4,443/run
//     → ~2% cold-install, ~98% hit the inline pdict[id]+element path (zero
//     call_ext). t_new_simple=184 shape-learns BigInteger/RSAKey ctors so
//     `this` is s_shaped_object with `.array/.t/.s` slots.
//
//   VERDICT — `this_array[i]` / `w_array[j]`:
//     FIRES. `new Array()` → s_object/array_obj slot; every bracket read
//     goes through t_get_elem_fast (2.43M) with ~98.5% hit (36k fall to
//     get_prop_any = holes/oob). Writes: 792,619/868,243 = 91% hit; the
//     75,624 misses are `w_array[j++]=v` where j==length (append past
//     bound → t_set_elem_fast's `Idx<Length` gate rejects → full
//     set_on_receiver/set_own_string slow chain, ~1,100 ns/write, 34% of
//     rows 5-8 above).
//
//   NEXT — no new fast-path needed for `this.array` or elem READ; both at
//   BEAM floor. Remaining crypto-specific wins ranked:
//     P-a  elem_fast write-APPEND: extend t_set_elem_fast to accept
//          Idx==Length (bump length + dense array:set) — kills 75k×1,100ns
//          ≈ 83ms of the 270ms wall (rows 5,8,12-14,17,19,20,24,25).
//     P-b  85,923 t_global_get_fast/run = BI_DB/BI_DM/BI_DV/BI_F1/BI_F2
//          reads in inner loops; assigned once in setupEngine so
//          analyze_const_globals can't inline. Per-method-entry global
//          hoist (or defer-lvalue-propkey-style: cache in ANF local at
//          first read per fn body) → ~85k×88ns ≈ 7.5ms.
//     P-c  t_set_elem_fast at 224ns/call is 3× t_get_elem_fast (71ns) —
//          the setelement(9,St,{some,setelement(2,Store,Data#{…})}) St
//          rebuild dominates. Pdict-overlay array elements (like
//          i-prop-ic-warm-inline did for shaped slots) → ~868k×150ns
//          ≈ 130ms. Biggest lever but heavy.
// ─────────────────────────────────────────────────────────────────────────────

// ── perf5 AFTER (3b198d8+wt, 2026-07-12) ─────────────────────────────────────
// Captured with L/M/N/O + fix-objprop + atom-SiteKey + this-c-hoist +
// known-handle applied. gleam test: 1573/0. bisect 60/60.
// bench_verify: obj_prop 11,020 ≤ 11,800 ✓; richards 3,283 > 2,200 ✗ (FAIL).
//
// richards_run.js:  3,283 µs/run  118 cells/run  (prints ok ✓)
// obj_prop micro:  11,020 µs/run  (5-sample min; mono `{kb,V}` + no recv-guard)
// deltablue:           ok ✓
//
//   targeted counts (per run)            perf5-BASE → AFTER       Δ  attribution
//   ────────────────────────────────────────────────────────────────────────────
//   t_call_method_ic/5                 40,466 →      41     -40,425  M/N FIRED
//   t_get_prop_own_data/3                  30 →       0         -30  fix(a) merged tier
//   t_set_prop_own_data/4                  58 →       0         -58  fix(a) merged tier
//   t_ic_get/4                            118 →     118           0
//   t_ic_set/5                            125 →     125           0
//   t_cell_get/2 (ffi)                  1,086 →   1,086           0
//   t_get_elem_fast/3                   4,252 →   4,252           0
//
//   perf5 net wins landed vs perf5-BASE:
//     M/N inline-method-ic-warm — 40k call_ext → inline.
//     O   this-abi              — CodeT([recv|pos]), zero frame/cons on hit.
//     ic_site_key (atom)        — pdict get(atom)≈2.8ns vs get(binary)≈9.0ns.
//     this-c-hoist              — `_this_c = pdict[_this_id]` threaded via
//                                 slot -1; `this.x` reuses it (0 pdict-get on
//                                 id); writes rebind to nc; user-JS calls
//                                 refresh. this_bench 8→4ns/read.
//     known-handle              — `let o={…}` marks o; `.x` skips receiver
//                                 guard. obj_prop 12k→11.0k.
//
//     • Rejected: anf.share naive Block/Break (obj_prop 20.6k→24k without
//       to_break sink); t_ic_warm_get single-call_ext (obj_prop→42.7k,
//       richards→6.4k — call_ext≈13ns×161k); cont_inline_weight (hand-BEAM
//       4-fn-split ≈ single-fn, letrec joins ~free); skip csid==sid for
//       simple_this (unsound — polymorphic `this` via inheritance).
// ─────────────────────────────────────────────────────────────────────────────

// ── perf5 residual attribution (2026-07-13) ──────────────────────────────────
// The 3,283→2,200 gap is now attributed. profile_file's stock top_n cannot
// see arc_prof_* rows (no `module_info/1`); a LOCAL call_time trace via
// `erlang:get_module_info/2` (test/emit_2core_richards_only.gleam) shows the
// compiled richards module runs ~486k BEAM function calls/run:
//
//   arc_prof_richards call breakdown (per run, LOCAL call_time)
//   ───────────────────────────────────────────────────────────
//   letrec-in-jsf_*_t (bind_if/share join points)    ~404k
//   jsf-toplevel (jsf_N_t method bodies)              40,489
//   js_main-anonymous (MakeClosure code_t wrappers)   40,650
//   ─────────────────────────────────────────────────────────
//   Σ ≈ 486k calls/run × ~3ns each ≈ 1,450 µs of the 3,200
//
//   erlang:put/2         51,154/run  × ~8ns  ≈  400 µs
//   erlang:setelement/3  55k/run     × ~15ns ≈  825 µs (this.x=v tuple copy)
//
// LANDED (581k→486k letrecs, richards 3,283→~3,200):
//   • emit_core.materialize `cont_inline_weight` — a KBind cont whose body is
//     a short linear spine (Values/Return/≤6-Let-chain, no If) with a trivial
//     `next` is passed through unmaterialised; each arm re-emits it (bounded).
//   • emit_core.emit_if `is_diverging` — one arm Break/Return/Continue → cont
//     is reached from ONE arm only, so pass through unmaterialised (emitted
//     once). Gated on `spine_tail_arity_ok` so the surviving arm's own nested
//     Loop/Block (arc's while shape) doesn't re-materialize with wrong arity.
//   • core_erlang `CFunRef` for zero-capture MakeClosure — emitted but
//     DISABLED: `'f'/N` fun-ref values inside a large js_main crash OTP-29
//     beam_ssa_opt (`ssa_opt_type_start` badmatch on the fun-type lattice).
//
// BLOCKED — the two mechanisms that would close the gap:
//   • OTP `inline` compile pass — cerl_inline+beam_ssa_opt crash on the
//     compiled richards Core Erlang (cerl_inline→beam_ssa_opt badmatch,
//     with `no_type_opt` → beam_ssa_pre_codegen badkey). Catchable only via
//     `no_spawn_compiler_process` + old-style `catch`; the fallback compile
//     without `inline` costs the letrec-collapse win.
//   • emit_block `count_breaks ≤ 1` inline — kills share()'s l_join letrec
//     but DUPLICATES the entire post-share tail per `.x`; obj_prop's
//     3-share loop body ~8×'d (11k→13k). Bounded by cont-size only saves
//     the same conts materialize's inline already covers.
//
// FLOOR — the ~1,000 µs gap is architectural: 55k setelement (immutable
// FLAT-tuple copy per shaped write) + 51k pdict put + ~400k residual letrec
// applies. Hitting 2,200 needs ONE OF: (a) OTP-29 beam_ssa fixes so `inline`
// / `CFunRef` land; (b) a mutable-array `_this_c` (atomics/counters instead
// of a pdict tuple) so `this.x=v` is 1 BIF not setelement+put; (c) hoisting
// `_this_sid` at CodeT entry so each `this.x` read is 4 BIFs not 10.
// ─────────────────────────────────────────────────────────────────────────────

// ── perf6 AFTER (04c63b6+wt, 2026-07-13) — integrate-gate-all ────────────────
// Working tree carries: S t_method_ic_warm/2 FFI (call_ffi.erl:182) replacing
// the ~130-line inline warm ladder; T new_simple pdict-seed; U `.apply(this,
// arguments)` raw-args forward + refs_args_expr carve-out; V proto-default-
// preshape (proto_data_kvs + {Sid,Arity,Defaults} cache); (c) `_this_sid`/
// `_this_proto` hoist at CodeT entry (func.gleam slots -2/-3); perf5_* gate
// consts (this_c_hoist / code_t) for bisect-raytrace; anf.share(frame_path)
// so mono/poly dispatch_entry Break to one join.
//
// gleam test:                 1573 passed, 0 failed ✓
// emit_2core_v8v7_bisect:     62/62 ✓  (60 + apply_args + t_while_this_after)
//
//   bench     perf5 (04c63b6)   perf6 AFTER (best-of-8)   HARD GATE   verdict
//   ────────────────────────────────────────────────────────────────────────────
//   richards           ~3,461                     3,570     ≤ 2,000       ✗
//   deltablue         ~19,650                    15,657     ≤ perf5       ✓
//   crypto           ~252,745                   226,657     ≤ perf5       ✓
//   raytrace         ~683,000                   157,351   ≤ 287,000       ✓
//
// 8-run min: richards 3,570 · deltablue 15,657 · crypto 226,657 ·
// raytrace 157,351 (perf6_block_let_case + perf6_letrec_float). raytrace
// RESTORED (683k→157k). deltablue −20%, crypto −10%, raytrace −8% vs
// pre-let-case perf6. richards ≈unchanged across BOTH let-case and
// letrec-float — the anf.share join cost is not the bottleneck the ~4ns/
// tuple estimate implied; ≤2,000 gate NOT MET (see attribution below).
//
// richards-floor lever attempted (sid-hoist REVERTED — slot -2 dropped,
// per-site derivation): its refresh_this_c If broke emit_core
// cont_inline_weight (any post-call bind_if cont with the If → letrec).
// Linear 1-Let refresh + per-site sid derive: letrec-apply 510k→445k,
// richards 3,535→3,386 (−149). Tried: overlay_sid FFI (81k call_ext ≈
// +1.2ms — worse); linear-cold + post-share miss recheck (letrecs UP
// 445k→657k — worse). RESIDUAL 445k letrec applies × ~4ns ≈ 1,780µs is
// the floor; verified `[inline, no_type_opt]` still crashes cerl_inline
// on richards' .core (emit_2core_cfunref_spike matrix, OTP-29).
//
// perf6 richards-gate levers TRIED+MEASURED (all reverted; gate NOT met):
//   • t_shaped_get/set FFI for `this.x` (bypass share+ladder → 1 call_ext):
//     richards 3,656→4,838 (+1,182). ~150k this-accesses × ~15ns call_ext
//     >> the ~11-BIF inline. Confirms t_ic_warm_get rejection at 161k/run.
//   • cerl_inline unblock (wildcard `_→match_fail` on JMut-destructure
//     single-clause `case` — the `undefined_var` fix): `[inline,size=24]`
//     COMPILES, letrecs 394k→375k, richards ~flat. size=100000+effort=500
//     → 323k, still ~3,700. cerl_inline won't collapse the l_join chain
//     (each l_joinK applied 3× in Core: hit + 2 cold-tiers; body=REST_K).
//   • emit_block Break-inline (register `label→cont_RAW` when
//     count_breaks_to≤1 so anf.share's single hit-leaf `Break l_join(v)`
//     inlines the continuation): letrec-DEFS 33→143 (schedule _t, O(N²)),
//     richards 3,722→3,360. FAILURE MODE: pre-loop share1 (`this.list`)
//     cold-misses on schedule()'s only call → applies l_fn1 whose body is
//     the compact chain → all 10,671 iters run there. Fixing needs l_fn's
//     body to also Break-inline → 2^N IR. The l_join chain IS the floor:
//     each `.x`/method-call/if-stmt = 1 join reachable from hit+cold; hit
//     applies it once. ≤2,000 needs a lowering that avoids the join
//     (case-as-tuple-return + destructure) — a larger emit_core change.
//
// perf6_block_let_case (emit_core.gleam:5665, LANDED — the case-as-tuple-
// return + destructure lowering). `Block`/`If` with a `KBind` cont whose body
// has no outer-label Break lowers to `let <t> = <case…{st,v}> in case t of
// {st_out,r..} -> cont` instead of `letrec l_join = fun(st,r)→cont in <case…
// apply l_join>`. `KValues` Cont variant yields the `{st,v..}` tuple at each
// case-leaf; nested l_miss (whose body_tree Breaks to l_join = an outer
// label from l_miss's view) still materializes as a letrec — its fun body
// ends in a KValues tuple (single-value ✓), and `apply l_miss_fn` returns it.
//   • perf6_mut_this_c=True re-measured WITHOUT let-case: letrec-apply
//     434,957/run — the "~445k gone" claim (expr.gleam:63) was WRONG. share
//     carried=[] narrows joins to arity-1 but does NOT remove the l_join
//     letrec; the applies persist.
//   • WITH let-case (Block+If): letrec-apply 434,957→57,942 (−87%; 256 fns
//     down from 657); residual = code_t MakeClosure wrappers (~40k) +
//     l_miss cold-path applies + Loop letrecs. richards best-5 3,526 —
//     ≈UNCHANGED vs 3,421 pre-let-case. `{st,v}` tuple-alloc + one-clause
//     `case` destructure ≈ letrec-apply cost on BEAM (~4ns each; 377k
//     eliminated applies × 4ns saved ≈ 377k added tuples × ~4ns).
//   • deltablue 15,773→15,372, crypto 245,447→229,704, raytrace
//     170,121→162,115 — the let-case DOES win where beam_ssa can sink the
//     immediately-destructured tuple (fewer/larger joins); richards' ~200k
//     dense per-`.x` joins are the shape it can't.
//   • Zero-cost `<st,v>` values-list (KValues → CValues, `let <st,r> =
//     case…<st,v> in cont`) LANDED as perf6_letrec_float (emit_core.gleam:
//     5789). Core Erlang `letrec…in E` is single-value, so the l_miss cont
//     letrec is HOISTED above the multi-value `let` (float_or_wrap pushes
//     the def to a per-let-case sink; free-var-safe by float_safe_body's
//     structural gate on anf.share's `Let(_, Block(l_miss,_,_), cold)` head-
//     shape — cold's free vars are ⊂ scope-at-l_join-entry). Hit-leaf =
//     `<st,v>`; miss-leaf = `let<t>=apply l_miss(st,0) in case t of {s,v}→
//     <s,v>` (cold-path only). Force-inline (cold duplicated) had regressed
//     raytrace/crypto via IR bloat; hoisting keeps l_miss defined once.
//   • perf6_letrec_float MEASURED — richards 3,570 (best-8), ≈UNCHANGED vs
//     let-case's 3,526. Dumped Core (jsf_1_t) confirms the transform fires
//     (451 multi-value lets in richards); a 2-join microbench (mb_deep.S)
//     confirms `let<S,V>=case…<s,v>` compiles to bare `swap`/`move` (0
//     alloc) vs the tuple path's `put_tuple2`+`get_tuple_element` — but on
//     richards' jsf bodies beam_ssa emits equivalent code either way (the
//     ~200k/run join cost was NOT the ~1,400µs bottleneck the ~4ns/tuple
//     estimate implied; beam_ssa was already sinking most, or the residual
//     is jsf-body BIF density). crypto 229k→226k / raytrace 162k→157k DID
//     improve. deltablue 15,657 (noise vs 15,372). No regressions; kept.
//   • ≤2,000 gate UNREACHABLE within emit_core scope — CONFIRMED by the
//     letrec-float measurement. Even at 0 join cost the floor is 55k
//     setelement (825µs) + 51k put (408µs) + ~40k code_t wrapper applies +
//     ~40k jsf `{V,St}` return tuples + jsf body BIFs ≈ 2,000-2,200.
//     Hitting it needs the InstanceState thread eliminated (all richards-
//     hot ops are pdict-backed, so `st` is dead freight — a
//     `js_direct_nostate` profile with JMut→JPure-via-pdict, jsf/N return
//     bare V not `{V,St}`, CallClosure no `{V,St'}` destructure). That is
//     an ABI-wide change (every jsf signature + every t_* FFI + KReturn/
//     CallClosure/emit_function threading), not an emit_core lowering.
//
// PRIOR-BLOCKER ROOT (was: ir_to_beam CompileFailed all four): NOT
// share(frame_path) as first diagnosed. Actual: stmt.gleam
// with_this_c_slot carried only slot -1 through loop LoopParams; the
// this-sid-hoist added slot -2 (rebound with -1 by every refresh_this_c).
// A `_t` body with a loop touching `this.*` leaked slot -2's inner
// share-join name past the Loop → subsequent `this.x` read = unbound
// `_t<N>`. Fixed: with_this_c_slot prepends -2 when live (now moot —
// sid-hoist reverted). Repro added to bisect (t_while_this_after).
// ─────────────────────────────────────────────────────────────────────────────

/// Profile a whole-program bench read from disk (v8-v7 richards/deltablue).
/// Unlike `profile`, there is no fixed inner iteration count so the ns/iter
/// column is dropped; instead we print explicit call counts for the {M,F,A}s
/// the emit_2core fast-path work targets, so a before/after diff shows which
/// hot path each optimisation actually removed.
pub fn profile_file(label: String, path: String, runs: Int) -> Nil {
  let assert Ok(source) = simplifile.read(path)
  let name = "arc_prof_" <> label
  trace_reset()
  let #(mod, seed) = compile_and_seed(source, name)

  // 1 warm run untraced (JIT + fast-path warm)
  ffi_apply_js_main(mod, seed)

  // cell-alloc probe: one untraced run, delta the store counter
  let assert Some(js_before) = seed.js_store
  let #(_v, st_after) = ffi_apply_js_main(mod, seed)
  let assert Some(js_after) = st_after.js_store
  let cells = js_after.alloc_since_gc - js_before.alloc_since_gc

  // untraced wall-clock (baseline — trace overhead is ~2-3×)
  let t0 = monotonic_time(Microsecond)
  repeat(runs, fn() { ffi_apply_js_main(mod, seed) })
  let untraced_us = monotonic_time(Microsecond) - t0

  // traced run
  trace_on(mod)
  let t1 = monotonic_time(Microsecond)
  repeat(runs, fn() { ffi_apply_js_main(mod, seed) })
  let traced_us = monotonic_time(Microsecond) - t1
  trace_off()

  io.println("")
  io.println(
    "══════ "
    <> label
    <> "  ("
    <> path
    <> ", runs="
    <> int.to_string(runs)
    <> ", cells/run="
    <> int.to_string(cells)
    <> ") ══════",
  )
  io.println(
    "  wall untraced: "
    <> int.to_string(untraced_us)
    <> " µs total, "
    <> int.to_string(untraced_us / runs)
    <> " µs/run",
  )
  io.println(
    "  wall traced:   "
    <> int.to_string(traced_us)
    <> " µs total ("
    <> int.to_string(traced_us * 100 / int.max(1, untraced_us))
    <> "% of untraced)",
  )

  // per-module rollup
  io.println("  ── per-module µs (traced) ──")
  let mods = [mod, ..all_mods()]
  list.each(
    list.sort(
      list.filter_map(mods, fn(m) {
        case module_total(m) {
          0 -> Error(Nil)
          us -> Ok(#(atom.to_string(m), us))
        }
      }),
      fn(a, b) { int.compare(b.1, a.1) },
    ),
    fn(row) {
      io.println(
        "    "
        <> string.pad_end(row.0, 42, " ")
        <> " "
        <> string.pad_start(int.to_string(row.1), 10, " ")
        <> " µs  "
        <> string.pad_start(
          int.to_string(row.1 * 100 / int.max(1, traced_us)),
          3,
          " ",
        )
        <> "%",
      )
    },
  )

  // top-N functions
  io.println("  ── top functions by µs (call_time) ──")
  let rows = top_n(25)
  io.println(
    "    "
    <> string.pad_end("module:function/arity", 55, " ")
    <> string.pad_start("count", 12, " ")
    <> string.pad_start("µs", 12, " ")
    <> string.pad_start("ns/call", 9, " ")
    <> string.pad_start("µs/run", 9, " "),
  )
  list.each(rows, fn(row) {
    let #(m, f, a, count, us) = row
    let ns_call = case count {
      0 -> 0
      _ -> us * 1000 / count
    }
    io.println(
      "    "
      <> string.pad_end(short(m) <> ":" <> f <> "/" <> int.to_string(a), 55, " ")
      <> string.pad_start(int.to_string(count), 12, " ")
      <> string.pad_start(int.to_string(us), 12, " ")
      <> string.pad_start(int.to_string(ns_call), 9, " ")
      <> string.pad_start(int.to_string(us / runs), 9, " "),
    )
  })

  // explicit counts for the {M,F,A}s the fast-path work targets — these are
  // the rows we expect to move when A/B/C/E land.
  io.println("  ── targeted call counts (per run) ──")
  let rt = fn(m: String) { atom.create("twocore@runtime@" <> m) }
  let ffi = fn(m: String) { atom.create("twocore_" <> m) }
  let targets = [
    #(rt("rt_js_obj"), "t_get_prop_any", 3),
    #(rt("rt_js_obj"), "t_set_prop_any", 4),
    #(rt("rt_js_call"), "t_call_checked", 4),
    #(rt("rt_js_call"), "t_kfn_code", 3),
    #(rt("rt_js_call"), "t_construct", 4),
    #(rt("rt_js_ops"), "t_instance_of", 3),
    #(ffi("rt_js_obj_ffi"), "t_get_prop_own_data", 3),
    // profile-baseline-and-k: G/H/I/K drivers
    #(rt("rt_js_obj"), "t_global_get", 2),
    #(ffi("rt_js_obj_ffi"), "t_global_get_fast", 2),
    #(ffi("rt_js_obj_ffi"), "t_get_elem_fast", 3),
    #(ffi("rt_js_obj_ffi"), "elem_read", 2),
    #(ffi("rt_js_obj_ffi"), "t_set_elem_fast", 4),
    #(ffi("rt_js_obj_ffi"), "elem_write", 3),
    #(rt("rt_js_val"), "t_to_property_key", 2),
    #(ffi("rt_js_obj_ffi"), "t_set_prop_own_data", 4),
    #(rt("rt_js_store"), "t_cell_get", 2),
    #(ffi("rt_js_store_ffi"), "t_cell_get", 2),
    #(ffi("rt_js_call_ffi"), "t_call_method_ic", 5),
    #(ffi("rt_js_call_ffi"), "t_new_simple", 3),
    // H/I placeholders — read 0 pre-impl
    #(ffi("rt_js_obj_ffi"), "t_ic_get", 4),
    #(ffi("rt_js_obj_ffi"), "t_ic_set", 5),
    // perf8 CC: raytrace `.apply(this,arguments)` chain
    #(rt("rt_js_obj"), "t_new_arguments", 3),
    #(ffi("rt_js_call_ffi"), "new_simple_apply", 7),
    #(ffi("rt_js_call_ffi"), "t_method_ic_warm", 2),
  ]
  list.each(targets, fn(t) {
    let #(m, f, a) = t
    let n = count_of(m, atom.create(f), a)
    io.println(
      "    "
      <> string.pad_end(
        short(atom.to_string(m)) <> ":" <> f <> "/" <> int.to_string(a),
        55,
        " ",
      )
      <> string.pad_start(int.to_string(n), 12, " ")
      <> " total  "
      <> string.pad_start(int.to_string(n / runs), 10, " ")
      <> " /run",
    )
  })
}

// ───────────────────────────── isolated microbench ─────────────────────────────

@external(erlang, "emit_2core_profile_ffi", "bench_op")
fn bench_op(which: Atom, st: InstanceState, arg: Dynamic, n: Int) -> Int

@external(erlang, "emit_2core_harness_ffi", "to_dynamic")
fn to_dynamic(a: a) -> Dynamic

fn micro(label: String, which: String, st: InstanceState, arg: Dynamic, n: Int) {
  let a = atom.create(which)
  bench_op(a, st, arg, n)
  // warm
  let us = bench_op(a, st, arg, n)
  let nop = bench_op(atom.create("nop"), st, arg, n)
  io.println(
    "  [micro] "
    <> string.pad_end(label, 30, " ")
    <> string.pad_start(int.to_string(us), 8, " ")
    <> " µs/1M  = "
    <> string.pad_start(int.to_string({ us - nop } * 1000 / n), 4, " ")
    <> " ns/call (nop-corrected; nop="
    <> int.to_string(nop)
    <> "µs)",
  )
}

/// Run isolated untraced microbenchmarks against a real seeded realm so
/// per-op cost can be attributed without call_time trace overhead.
fn microbench() {
  io.println("")
  io.println("══════ isolated untraced microbench (1M calls each) ══════")
  // seed a realm and grab a live fn Handle + object Handle
  trace_reset()
  let #(mod, seed) = compile_and_seed(adder_js, "arc_prof_micro_adder")
  // run once so `add5` and its `x` cell exist; capture the resulting state
  let #(_v, st_adder) = ffi_apply_js_main(mod, seed)
  let assert Some(js) = st_adder.js_store
  // adder's inner fn is the last cell allocated (next-1); its captured `x`
  // is next-3 (makeAdder cell, x cell, inner cell → 3 allocs after realm)
  let add5_h = to_dynamic(#(atom.create("js_cell"), js.next - 1))
  let x_h = to_dynamic(#(atom.create("js_cell"), js.next - 3))
  micro("kfn_code (via Gleam wrapper)", "kfn_code", st_adder, add5_h, 1_000_000)
  micro("kfn_code (FFI direct)", "kfn_code_ffi", st_adder, add5_h, 1_000_000)
  micro("cell_get (via Gleam wrapper)", "cell_get", st_adder, x_h, 1_000_000)
  micro("cell_get (FFI direct)", "cell_get_ffi", st_adder, x_h, 1_000_000)

  // obj_prop: make an object with x, then microbench get/set
  let #(mod2, seed2) = compile_and_seed(obj_js, "arc_prof_micro_obj")
  let #(_v2, st_obj) = ffi_apply_js_main(mod2, seed2)
  let assert Some(js2) = st_obj.js_store
  let o_h = to_dynamic(#(atom.create("js_cell"), js2.next - 1))
  let key =
    to_dynamic(#(
      atom.create("string_key"),
      #(atom.create("named"), <<"x":utf8>>),
    ))
  micro(
    "t_get_prop_any (o.x)",
    "get_prop",
    st_obj,
    to_dynamic(#(o_h, key)),
    1_000_000,
  )
  micro(
    "t_set_prop_any (o.x = v)",
    "set_prop",
    st_obj,
    to_dynamic(#(o_h, key)),
    1_000_000,
  )
  let kb = to_dynamic(<<"x":utf8>>)
  micro(
    "t_get_prop_own_data (FFI)",
    "get_prop_own_data",
    st_obj,
    to_dynamic(#(o_h, kb)),
    1_000_000,
  )
  micro(
    "t_set_prop_own_data (FFI)",
    "set_prop_own_data",
    st_obj,
    to_dynamic(#(o_h, kb)),
    1_000_000,
  )
}

// ─────────────────── deltablue.js static shape survey ───────────────────
// Grep-derived facts that gate the fast-path candidates. Re-verify with
// `grep -c instanceof bench/v8-v7/deltablue.js` etc. before acting on them.
//
// instanceof:  0 occurrences (richards.js also 0; every v8-v7 bench 0) →
//   candidate E (t_instanceof_fast) has NO driver in v8-v7; drop E.
//
// Inheritance mechanism (deltablue.js:49-57): `Object.prototype.inheritsFrom`
//   sets Child.prototype = new Inheriter() where Inheriter.prototype ===
//   Parent.prototype, i.e. Child.prototype.[[Prototype]] = Parent.prototype.
//   Constructors chain via `Child.superConstructor.call(this, …)` — plain
//   .call, no `new`, so candidate C (t_new_simple) sees only the OUTER
//   `new Leaf(...)`; the super chain is ordinary calls.
//
// Class hierarchy (leaf → root, .inheritsFrom edges):
//   StayConstraint, EditConstraint       → UnaryConstraint  → Constraint
//   ScaleConstraint, EqualityConstraint  → BinaryConstraint → Constraint
//   OrderedCollection, Strength, Variable, Planner, Plan     → (flat)
// Only leaf constraints are instantiated; Unary/Binary are abstract.
//
// Method-dispatch proto-hop depth from a LEAF instance:
//   1 hop  — leaf-own (execute; EditConstraint.isInput; every flat-class
//            method incl. Plan/OrderedCollection/Planner/Variable)
//   2 hops — Unary/Binary methods (chooseMethod isSatisfied markInputs
//            output recalculate markUnsatisfied inputsKnown addToGraph
//            removeFromGraph input) — the Constraint.satisfy body at :175
//            calls FOUR of these per invocation
//   3 hops — Constraint methods (addConstraint satisfy destroyConstraint
//            isInput) — every ctor calls this.addConstraint()
// ⇒ candidate A's single-proto-hop t_call_method_mono covers Plan.execute's
//   inner loop (all 1-hop) but MISSES the satisfy/addConstraint churn.
//   deltablue therefore JUSTIFIES either a ≥2-hop walk in t_call_method_mono
//   or candidate D (proto-lookup-cache keyed on {ProtoId,KeyBin}).
//
// Own-data field counts (all instances, set in ctors incl. super chain):
//   Variable=7  ScaleConstraint=6  Binary/Equality=4  Unary/Stay/Edit=3
//   Strength=2  OrderedCollection/Planner/Plan=1
// ⇒ multi-field objects — the map-based poly pdict overlay
//   (twocore_rt_js_obj_ffi.erl t_get_prop_own_data) now covers these;
//   candidate B is landed. Verify emitted inline warm-probe matches.
//
// Array-index hotness: OrderedCollection.at (:67) = `this.elms[index]` and
//   .remove (:79) = `this.elms[i]` — numeric-key reads on a plain Array.
//   Plan.execute (:776) is the innermost hot loop: for i<size():
//   constraintAt(i)→v.at(i)→elms[i] then c.execute(). chainTest runs it
//   100× over an n-length plan; change() 10× ×4 per projectionTest.
// ⇒ an integer-key element read fast-path (Array-kind s_object, dense
//   elements) would remove a t_get_prop_any per inner iteration here;
//   no such candidate is spec'd yet — record as array-index-fast-path.

// ───────────────────────── final-bench integration gate ─────────────────────────
// Regression gate for the G/H/I optimisation set. Separate from profile_file
// so it can be run standalone (`gleam run -m emit_2core_profile`) and returns
// a hard PASS/FAIL rather than a wall of profile output.
//
// Baseline (pre-G/H/I, commit a2881bb) richards targeted counts /run — the
// "before" column of the diff table below. Captured via profile_file at
// runs=3 on this machine; per-run counts are exact (deterministic bench).
const richards_us_target = 2200

// obj_prop micro-bench (1M-iter `let o={x:0}; for… o.x=o.x+i`). Spec target
// = d24d583's mono `{kb,V}` cache (≤11.8k µs).
const obj_prop_us_target = 11_800

const richards_baseline = [
  #("twocore_rt_js_obj_ffi", "t_global_get_fast", 2, 65),
  #("twocore@runtime@rt_js_obj", "t_global_get", 2, 0),
  #("twocore_rt_js_obj_ffi", "t_get_prop_own_data", 3, 106),
  #("twocore_rt_js_obj_ffi", "t_set_prop_own_data", 4, 143),
  #("twocore_rt_js_obj_ffi", "t_ic_get", 4, 0),
  #("twocore_rt_js_obj_ffi", "t_ic_set", 5, 0),
  #("twocore_rt_js_call_ffi", "t_new_simple", 3, 32),
  #("twocore_rt_js_call_ffi", "t_call_method_ic", 5, 40_466),
  #("twocore_rt_js_store_ffi", "t_cell_get", 2, 1320),
  #("twocore_rt_js_call_ffi", "t_kfn_code", 3, 1),
]

fn correctness_gate(label: String, path: String) -> Bool {
  let assert Ok(source) = simplifile.read(path)
  case harness.run_compiled(source) {
    pipeline.DiffRun(result: Ok(_), stdout: <<"ok\n":utf8>>) -> {
      io.println("  ✓ " <> label <> " prints ok")
      True
    }
    pipeline.DiffRun(result: Ok(_), stdout:) -> {
      io.println(
        "  ✗ " <> label <> " completed but stdout=" <> string.inspect(stdout)
        <> " (expected \"ok\\n\")",
      )
      False
    }
    pipeline.DiffRun(result: Error(e), stdout:) -> {
      io.println(
        "  ✗ " <> label <> " FAILED: " <> string.slice(e, 0, 300)
        <> " | stdout=" <> string.inspect(stdout),
      )
      False
    }
  }
}

/// Final-bench: correctness (richards+deltablue print "ok") + perf (richards
/// / obj_prop ≤ the *_us_target constants above, untraced best-of-5) +
/// before/after targeted-count diff. Returns True iff every gate passed.
/// Prints a per-opt attribution table when the target is missed so the failing
/// optimisation can be identified from the counts.
pub fn bench_verify() -> Bool {
  io.println("")
  io.println("══════ final-bench integration gate ══════")

  // (3) richards_run.js prints "ok" — the bench self-checks queueCount/
  //     holdCount and throws if wrong, so "ok" == semantic correctness.
  let richards_ok =
    correctness_gate("richards_run.js", "bench/v8-v7/richards_run.js")
  // (4) deltablue_run.js completes without error.
  let deltablue_ok =
    correctness_gate("deltablue_run.js", "bench/v8-v7/deltablue_run.js")

  // (2) richards µs/run ≤ richards_us_target — untraced wall-clock, best of
  //     the runs so a one-off GC pause doesn't fail the gate.
  trace_reset()
  let assert Ok(src) = simplifile.read("bench/v8-v7/richards_run.js")
  let #(mod, seed) = compile_and_seed(src, "arc_prof_gate_richards")
  ffi_apply_js_main(mod, seed)
  // warm
  let runs = 5
  let best =
    list.fold(list.repeat(Nil, runs), 1_000_000_000, fn(acc, _) {
      let t0 = monotonic_time(Microsecond)
      ffi_apply_js_main(mod, seed)
      let dt = monotonic_time(Microsecond) - t0
      int.min(acc, dt)
    })
  let perf_ok = best <= richards_us_target
  io.println(
    "  "
    <> case perf_ok {
      True -> "✓"
      False -> "✗"
    }
    <> " richards "
    <> int.to_string(best)
    <> " µs/run (best of "
    <> int.to_string(runs)
    <> "; target ≤"
    <> int.to_string(richards_us_target)
    <> ")",
  )

  // (2b) obj_prop µs/run ≤ obj_prop_us_target — regression guard for the
  //      shaped-write hot path. Same best-of-5 untraced metric as richards.
  trace_reset()
  let #(obj_mod, obj_seed) = compile_and_seed(obj_js, "arc_prof_gate_obj")
  ffi_apply_js_main(obj_mod, obj_seed)
  // warm
  let obj_best =
    list.fold(list.repeat(Nil, runs), 1_000_000_000, fn(acc, _) {
      let t0 = monotonic_time(Microsecond)
      ffi_apply_js_main(obj_mod, obj_seed)
      let dt = monotonic_time(Microsecond) - t0
      int.min(acc, dt)
    })
  let obj_ok = obj_best <= obj_prop_us_target
  io.println(
    "  "
    <> case obj_ok {
      True -> "✓"
      False -> "✗"
    }
    <> " obj_prop "
    <> int.to_string(obj_best)
    <> " µs/run (best of "
    <> int.to_string(runs)
    <> "; target ≤"
    <> int.to_string(obj_prop_us_target)
    <> ")",
  )

  // (5) targeted-count before/after diff — one traced run.
  trace_on(mod)
  ffi_apply_js_main(mod, seed)
  trace_off()
  io.println("  ── targeted counts: before (a2881bb) → after ──")
  io.println(
    "    "
    <> string.pad_end("{M,F,A}", 48, " ")
    <> string.pad_start("before", 10, " ")
    <> string.pad_start("after", 10, " ")
    <> string.pad_start("Δ", 12, " "),
  )
  list.each(richards_baseline, fn(row) {
    let #(m, f, a, before) = row
    let after = count_of(atom.create(m), atom.create(f), a)
    let delta = after - before
    io.println(
      "    "
      <> string.pad_end(short(m) <> ":" <> f <> "/" <> int.to_string(a), 48, " ")
      <> string.pad_start(int.to_string(before), 10, " ")
      <> string.pad_start(int.to_string(after), 10, " ")
      <> string.pad_start(
        case delta >= 0 {
          True -> "+" <> int.to_string(delta)
          False -> int.to_string(delta)
        },
        12,
        " ",
      ),
    )
  })

  // Attribution when the µs target is missed: which of G/H/I didn't fire.
  case perf_ok {
    True -> Nil
    False -> {
      io.println("  ── attribution (target missed) ──")
      let n = fn(m, f, a) { count_of(atom.create(m), atom.create(f), a) }
      let g_after = n("twocore_rt_js_obj_ffi", "t_global_get_fast", 2)
      let i_after = n("twocore_rt_js_obj_ffi", "t_ic_get", 4)
      let h_own = n("twocore_rt_js_obj_ffi", "t_get_prop_own_data", 3)
      io.println(
        "    G slotted-globals: t_global_get_fast "
        <> int.to_string(g_after)
        <> "/run — "
        <> case g_after < 10 {
          True -> "FIRED (NB baseline 65 setup-only, perf-negligible)"
          False -> "NOT FIRED (baseline 65 → expect ~0)"
        },
      )
      io.println(
        "    I prop-IC:         t_ic_get "
        <> int.to_string(i_after)
        <> "/run — "
        <> case i_after > 0 {
          True -> "FIRED"
          False -> "NOT FIRED (expect >0; reads still via own_data)"
        },
      )
      io.println(
        "    H shaped-objects:  t_get_prop_own_data "
        <> int.to_string(h_own)
        <> "/run — "
        <> case h_own < 50 {
          True -> "reads shifted (H/I)"
          False -> "still map-backed (baseline 106; H not firing)"
        },
      )
    }
  }

  let all = richards_ok && deltablue_ok && perf_ok && obj_ok
  io.println("")
  io.println(case all {
    True -> "  ══ PASS ══"
    False -> "  ══ FAIL ══"
  })
  all
}

// ── perf8 CC: raytrace-apply-verify (f289f0c+wt, 2026-07-13) ─────────────────
// Class.create (raytrace.js:35) returns `function(){this.initialize.apply(
// this,arguments)}`. Every raytrace class ctor is that shape; every hot
// `new Flog.RayTracer.Vector(x,y,z)` etc. runs it via new_simple. The Q
// block above (:551-588) measured the perf5 leak triple; U (perf6) killed
// t_call_checked; z (perf7 args_elide + needs_args_object) killed
// t_new_arguments. This block VERIFIES those two counts stay ≈0 at perf8
// and traces whether emit_apply_arguments (expr.gleam:2688) is reached from
// the `new Klass(…)` → new_simple path.
//
// FLOW: `new Klass(a,b,c)` at emit → `host("new_simple",[c,args_l])`
//   (expr.gleam:325). At runtime new_simple_apply (call_ffi.erl:590) calls
//   `Code(St,Frame,Args)` where Code = the compiled body of Klass. Klass's
//   body — `this.initialize.apply(this,arguments)` — was itself compiled
//   through emit_plain_call → emit_apply_arguments (expr.gleam:2688) since
//   raw_args_var=Some("_args") for non-arrows (func.gleam:1507). So the
//   compiled body already contains `call_method_ic(this,"initialize",_args,
//   @ic)` — the `new` site does NOT reach emit_apply_arguments (it emits
//   new_simple), but the ctor BODY does at its own compile time.
//   ⇒ new_simple's ctor-apply reaches emit_apply_arguments TRANSITIVELY:
//   the raw Args cons-list from the `new` site becomes `_args` inside Code
//   and flows straight into call_method_ic — zero arguments-object, zero
//   Function.prototype.apply reflection.
//
// GATES (per run; 66,600 = total `new` in raytrace hot-loop):
//   t_new_arguments   ≈0      ← perf7_args_elide + needs_args_object carve-out
//   t_call_checked    ≈0-1    ← .apply routed to call_method_ic not reflection
//   new_simple_apply  ~66,598 ← every `new` (2 miss to t_construct)
//   t_call_method_ic  ≥66,596 ← includes the 66,596 initialize dispatches
//                               (proof emit_apply_arguments fired in Code)
//
// ANY of the first two >100/run → the corresponding perf7 lever REGRESSED
// and raytrace-new-simple-lever's 60k target is unreachable. Run standalone:
//     gleam run -m emit_2core_profile          (calls this via main)
// ─────────────────────────────────────────────────────────────────────────────

/// perf8 CC: verify t_new_arguments≈0 ∧ t_call_checked≈0 for raytrace and
/// print the top-10 sinks. Returns True iff both apply-chain gates hold —
/// the input for raytrace-new-simple-lever's `new_simple` optimisation.
pub fn raytrace_apply_verify() -> Bool {
  io.println("")
  io.println("══════ perf8 CC: raytrace-apply-verify ══════")
  let assert Ok(src) = simplifile.read("bench/v8-v7/raytrace_run.js")
  trace_reset()
  let #(mod, seed) = compile_and_seed(src, "arc_prof_rt_cc")
  // 1 warm run untraced (JIT + shape-learn + IC install)
  ffi_apply_js_main(mod, seed)
  // one traced run — counts are deterministic
  trace_on(mod)
  let t0 = monotonic_time(Microsecond)
  ffi_apply_js_main(mod, seed)
  let traced_us = monotonic_time(Microsecond) - t0
  trace_off()

  io.println("  wall traced: " <> int.to_string(traced_us) <> " µs (1 run)")
  io.println("  ── top-10 by µs (call_time) ──")
  io.println(
    "    "
    <> string.pad_end("module:function/arity", 55, " ")
    <> string.pad_start("count", 12, " ")
    <> string.pad_start("µs", 12, " ")
    <> string.pad_start("ns/call", 9, " "),
  )
  list.each(top_n(10), fn(row) {
    let #(m, f, a, count, us) = row
    let ns_call = case count {
      0 -> 0
      _ -> us * 1000 / count
    }
    io.println(
      "    "
      <> string.pad_end(short(m) <> ":" <> f <> "/" <> int.to_string(a), 55, " ")
      <> string.pad_start(int.to_string(count), 12, " ")
      <> string.pad_start(int.to_string(us), 12, " ")
      <> string.pad_start(int.to_string(ns_call), 9, " "),
    )
  })

  // Targeted counts — the four rows that prove/disprove the .apply chain.
  let rt = fn(m: String) { atom.create("twocore@runtime@" <> m) }
  let ffi = fn(m: String) { atom.create("twocore_" <> m) }
  let n_new_args = count_of(rt("rt_js_obj"), atom.create("t_new_arguments"), 3)
  let n_call_chk =
    count_of(rt("rt_js_call"), atom.create("t_call_checked"), 4)
  let n_new_simple =
    count_of(ffi("rt_js_call_ffi"), atom.create("t_new_simple"), 3)
  let n_ns_apply =
    count_of(ffi("rt_js_call_ffi"), atom.create("new_simple_apply"), 7)
  let n_method_ic =
    count_of(ffi("rt_js_call_ffi"), atom.create("t_call_method_ic"), 5)
  let n_construct = count_of(rt("rt_js_call"), atom.create("t_construct"), 4)
  io.println("  ── targeted counts (per run) ──")
  let row = fn(name: String, n: Int) {
    io.println(
      "    " <> string.pad_end(name, 40, " ")
      <> string.pad_start(int.to_string(n), 10, " "),
    )
  }
  row("t_new_arguments/3", n_new_args)
  row("t_call_checked/4", n_call_chk)
  row("t_new_simple/3", n_new_simple)
  row("new_simple_apply/7", n_ns_apply)
  row("t_call_method_ic/5", n_method_ic)
  row("t_construct/4 (new_simple miss)", n_construct)

  // Gate (1): perf7_args_elide + needs_args_object elided the object.
  let args_ok = n_new_args < 100
  // Gate (2): emit_apply_arguments routed .apply through call_method_ic.
  let chk_ok = n_call_chk < 100
  // Evidence emit_apply_arguments reached FROM new_simple's ctor-apply:
  // ~every new_simple_apply's Code body dispatches initialize via method_ic.
  // Allow slack for the ≤4 non-Class.create ctors + t_construct fall-throughs.
  let reaches = n_method_ic >= n_ns_apply - n_construct - 10
  io.println("  ── verdict ──")
  io.println(
    "    (1) t_new_arguments ≈0:       "
    <> case args_ok {
      True -> "✓ FIRES (perf7_args_elide elided; " <> int.to_string(n_new_args)
        <> "/run)"
      False -> "✗ REGRESSED (" <> int.to_string(n_new_args)
        <> "/run — needs_args_object carve-out not firing)"
    },
  )
  io.println(
    "    (2) t_call_checked ≈0:        "
    <> case chk_ok {
      True -> "✓ FIRES (.apply → call_method_ic; " <> int.to_string(n_call_chk)
        <> "/run)"
      False -> "✗ REGRESSED (" <> int.to_string(n_call_chk)
        <> "/run — emit_apply_arguments miss)"
    },
  )
  io.println(
    "    (3) new_simple → emit_apply_arguments: "
    <> case reaches {
      True -> "✓ REACHED (via compiled ctor body — "
        <> int.to_string(n_ns_apply) <> " new_simple_apply, "
        <> int.to_string(n_method_ic)
        <> " method_ic incl. initialize)"
      False -> "✗ NOT REACHED (method_ic " <> int.to_string(n_method_ic)
        <> " < new_simple_apply " <> int.to_string(n_ns_apply)
        <> " — ctor bodies falling to slow path)"
    },
  )
  args_ok && chk_ok && reaches
}

/// perf8 BB: standalone am3 harness — the crypto.js:108-122 inner loop with
/// 40-elem `this.array`/`w.array` seeded to 28-bit values, run for 100
/// outer × 40 inner iterations under call_time trace. Prints the top-10 by
/// µs and per-op runtime counts so each of `>>14`/`>>28`/`<<14`/`& 0x3fff`/
/// `& 0xfffffff`/`w[j++]`/`this_array[i++]` can be verified against its
/// resolve_js class WITHOUT compiling the full 1700-line crypto.js.
const am3_bench_js = "
function BI() { this.array = new Array(); }
BI.prototype.am3 = function(i,x,w,j,c,n) {
  var this_array = this.array;
  var w_array    = w.array;
  var xl = x&0x3fff, xh = x>>14;
  while(--n >= 0) {
    var l = this_array[i]&0x3fff;
    var h = this_array[i++]>>14;
    var m = xh*l+h*xl;
    l = xl*l+((m&0x3fff)<<14)+w_array[j]+c;
    c = (l>>28)+(m>>14)+xh*h;
    w_array[j++] = l&0xfffffff;
  }
  return c;
};
var a = new BI(); var w = new BI();
for (var k = 0; k < 40; k++) { a.array[k] = 0x7654321; w.array[k] = 0x1234567; }
var c = 0;
for (var r = 0; r < 100; r++) c = a.am3(0, 0x89abcde, w, 0, c, 40);
c;
"

/// perf8 BB: run am3_bench_js under call_time trace and print top-10 + the
/// per-op runtime counts that decide the JPure ✓/✗ verdict per op.
pub fn crypto_am3_op_map() -> Nil {
  io.println("")
  io.println("══════ perf8 BB: crypto am3 op-map (isolated am3 harness) ══════")
  trace_reset()
  let #(mod, seed) = compile_and_seed(am3_bench_js, "arc_prof_am3")
  // 1 warm run untraced (JIT + tc_arr install + IC install)
  ffi_apply_js_main(mod, seed)
  trace_on(mod)
  ffi_apply_js_main(mod, seed)
  trace_off()

  // 100 outer × 40 inner = 4000 am3 iters. Per iter: 2× this_array[i] read,
  // 1× w_array[j] read, 1× w_array[j++]= write, 4× >>C, 1× <<C, 4× &C, 4× *,
  // 6× +, 1× --n. Any *_fast row >0 below means the erl_* inline arm was
  // REJECTED at runtime for that many operands.
  io.println("  (4000 am3 inner iters; per-iter: 2 reads_c, 1 read_p,"
    <> " 1 write_p, 4 >>C, 1 <<C, 4 &C, 4 *, 6 +)")
  io.println("  ── top-10 by µs (call_time) ──")
  io.println(
    "    "
    <> string.pad_end("module:function/arity", 55, " ")
    <> string.pad_start("count", 12, " ")
    <> string.pad_start("µs", 12, " ")
    <> string.pad_start("ns/call", 9, " "),
  )
  list.each(top_n(10), fn(row) {
    let #(m, f, a, count, us) = row
    let ns_call = case count {
      0 -> 0
      _ -> us * 1000 / count
    }
    io.println(
      "    "
      <> string.pad_end(short(m) <> ":" <> f <> "/" <> int.to_string(a), 55, " ")
      <> string.pad_start(int.to_string(count), 12, " ")
      <> string.pad_start(int.to_string(us), 12, " ")
      <> string.pad_start(int.to_string(ns_call), 9, " "),
    )
  })

  io.println("  ── per-op runtime counts (JPure verdict) ──")
  let ffi = fn(m: String) { atom.create("twocore_" <> m) }
  let rt = fn(m: String) { atom.create("twocore@runtime@" <> m) }
  let per_op = [
    // (host, arity, "op it backs", expected/4000-iter, verdict_if_zero)
    #(ffi("rt_js_ops_ffi"), "t_shr_fast", 2, ">>14/>>28 fallback", 0),
    #(ffi("rt_js_ops_ffi"), "t_shl_fast", 2, "<<14 fallback", 0),
    #(ffi("rt_js_ops_ffi"), "t_bitand_fast", 2, "& 0x3fff/0xfffffff fallback", 0),
    #(ffi("rt_js_ops_ffi"), "t_ushr_fast", 2, ">>> (am3 has none)", 0),
    #(rt("rt_js_ops"), "t_mul", 3, "* fallback (JMut)", 0),
    #(rt("rt_js_ops"), "t_add", 3, "+ fallback (JMut)", 0),
    #(ffi("rt_js_obj_ffi"), "t_get_elem_fast_c", 4, "this_array[i] hoisted", 8000),
    #(ffi("rt_js_obj_ffi"), "t_get_elem_fast_p", 3, "w_array[j] read", 4000),
    #(ffi("rt_js_obj_ffi"), "t_set_elem_fast_p", 4, "w_array[j++]= write", 4000),
    #(ffi("rt_js_obj_ffi"), "t_arr_c_load", 1, "arr_c hoist (1/am3 call)", 100),
    #(rt("rt_js_obj"), "t_get_prop_any", 3, "elem-miss slow path", 0),
    #(rt("rt_js_val"), "t_to_property_key", 2, "elem-miss key coerce", 0),
  ]
  io.println(
    "    "
    <> string.pad_end("{M,F,A}", 42, " ")
    <> string.pad_start("count", 8, " ")
    <> string.pad_start("expect", 8, " ")
    <> "  op",
  )
  list.each(per_op, fn(t) {
    let #(m, f, a, op, expect) = t
    let n = count_of(m, atom.create(f), a)
    io.println(
      "    "
      <> string.pad_end(short(atom.to_string(m)) <> ":" <> f <> "/"
        <> int.to_string(a), 42, " ")
      <> string.pad_start(int.to_string(n), 8, " ")
      <> string.pad_start(int.to_string(expect), 8, " ")
      <> "  " <> op,
    )
  })
}

fn dump_am3_core() -> Nil {
  let opts =
    emit_2core.CompileOpts(
      module_name: "arc_prof_am3",
      source_kind: emit_2core.AsScript,
      entry_name: "js_main",
    )
  case emit_2core.compile_source(am3_bench_js, opts) {
    Error(e) ->
      io.println("!! am3 compile_source FAILED: " <> string.inspect(e))
    Ok(unit) ->
      case pipeline.ir_to_core(unit.module, profiles.js_direct()) {
        Error(e) ->
          io.println("!! am3 ir_to_core FAILED: " <> string.inspect(e))
        Ok(core) -> io.println(core)
      }
  }
}

pub fn main() {
  io.println("emit_2core call_time profile — traced=local, per-{M,F,A}")
  dump_am3_core()
}
