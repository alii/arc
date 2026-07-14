//// V8-v7 (Octane) suite through emit_2core: compile → IR → BEAM → run,
//// timed against arc's tree-walk interpreter. Sources are the standalone
//// *_run.js written by bench/v8-v7/make_standalone.sh (BenchmarkSuite
//// wrapper stripped, `alert` shimmed to throw, entry called once, prints
//// "ok"). Not a `_test` module — run standalone:
////
////     cd arc && gleam run -m emit_2core_v8v7_probe
////
//// A compile/run failure is REPORTED, not fatal: this is measurement.

// ── measure-iterate: fast-path before/after (µs/iter, this machine) ──
// Captured via `gleam run -m emit_2core_v8v7_probe` with `gleam test` green
// (1573/1573) and `emit_2core_v8v7_bisect` 60/60 after each row. Per-unit
// attribution from `gleam run -m emit_2core_profile` targeted-count deltas
// (units landed together in one wave; individual rows are profile-derived,
// not separately timed). Spec baseline was 54,259 — re-measured here.
//
//                          richards   deltablue   vs-qjs(rich)
//   baseline (6d0fd5f)       48,450      50,194        25.3×
//   after-A  method_mono     ~28,000       n/m       (t_call_checked→0/run t_kfn_code→1/run)
//   after-B  own-data-poly   ~23,000       n/m       (t_get_prop_any→41/run own_data→106/run)
//   after-C  new_simple      ~21,000       n/m       (t_construct→9/run)
//   after-E  instanceof_fast ~21,000       n/m       (t_instance_of→0/run; richards has none)
//   A+B+C+E measured          20,241      51,162        10.6×
//   after-F  global_get_fast   (t_global_get→0; classify 481k→7k/run)
//   after-G  eq_fast/nul_eq    (t_eq→0; `x==null` → single nul_eq probe)
//   after-H  truthy→val_ffi    (to_boolean_i32 rerouted to guard-dispatch FFI)
//   after-I  bit*_fast         (int32_binop→0; ToPrimitive/ToNumeric→~0)
//   after-J  method_mono flat  (mono_proto/mono_own_value/tc_mc_get inlined)
//   F..J measured             ~14,500      27,000         7.6×
//   after-K  emit_cond_i32     (stmt: `if(==/!=/rel)` skip truthy; expr:
//                               nul_eq inline → nul_eq 33k→0, truthy 70k→21k)
//   after-L  const_globals     (top-level `var X = <int>` never-assigned →
//                               inline literal; global_get_fast 41k→~0)
//   after-M  call_method_ic    (per-CALLSITE ≤4-poly IC caching {PId,Code,
//                               FnH}; mega→mono fallback; 169→118 ns/call)
//   after-N  int_const_eq/band (loose_eq int×ConstI32 inline; `x & C≥0` →
//                               erlang:band BIF; eq_fast 31k→~0)
//   K..N measured             ~10,200      24,100         5.3×
//   after-O  truthy_i32 inline  (anf: `true`/`false`/is_int → i32
//                               inline; stmt.cond_i32 fallthrough
//                               routed same → to_boolean_i32 21k→~0)
//   after-P  method_ic map+NKey (SiteKey list→{NKey,#{PId=>{Code,FnH}}}
//                               — precomputed {named,K} zero-alloc
//                               own-shadow, O(1) poly; 117→113 ns/call)
//   O..P measured              ~9,800      24,000         5.1×
//
//                          richards   deltablue     crypto    raytrace
//   perf4 (3b198d8)           4,683      19,650    269,900     287,000
//     G slotted-globals + H hidden-class/SShapedObject + I per-site
//     IC + J Math.*-direct + K array; profile.gleam P/Q captures
//     (raytrace 287k is the 683k/2.4× pre-regression target from the
//     04c63b6 commit msg — profile.gleam Q measured 345,887 at 3b198d8).
//   perf5 (04c63b6)           3,461     ~19,650   ~252,745     683,000
//     L JRead-reclass + M/N inline-method-IC-warm + O this-ABI/CodeT
//     + shape-obj-literals + this-c-hoist + atom-SiteKey + known-handle.
//     richards: profile.gleam:656 3,283 min / 3,461 median-of-5.
//     raytrace: REGRESSED 2.4× (commit msg) — bisect-raytrace-perf5 owns
//     the culprit. deltablue/crypto: 3b198d8 proxies (profile.gleam:485/
//     496) — perf5 not clean-captured; interim gate floor matching :94.
//   ref qjs                    1,917       3,825     51,323      19,477
//
// GATE richards ≤10,000µs: MET (min 9,642 / median ~9,800 across 12
// cool runs; thermal drift ±5%). O eliminated the residual truthy sink
// (to_boolean_i32 21k→~0 out of top-10); P shaved the ic warm-path
// alloc. Spec-suggested "cache CodeS (simple-ABI)" was DEAD: every
// richards proto method reads `this`, so KFunction.simple is `none`
// (arc func.gleam is_simple_abi_eligible rejects refs_frame_body).
// Residual profile (emit_2core_profile top-4):
//   call_ffi:t_call_method_ic/5  40,466/run  ~4,590µs — floor is now
//     element(9+2)+map_get RSlot + get(SiteKey) + map_get PId +
//     is_map_key(NKey) own-shadow + Frame tuple + apply. Next win =
//     `this`-ABI variant (positional args + this, no Frame) — needs
//     func.gleam third-body emit.
//   obj_ffi:t_get_elem_fast/3     4,252/run    ~720µs — array:get/size
//     /default all cross into OTP `array` module per read.
//   obj_ffi:t_set_elem_fast/4       942/run    ~220µs
//   ops_ffi:t_bitor_fast/2        4,732/run    ~160µs
//
// D (proto-lookup-cache) DECISION: NOT the next unit. richards mono_proto
// is 40,465/run at ~8k traced-µs — single-level chain, marginal. deltablue
// mono_proto is 112,722/run over 2-level inheritsFrom chains, so D helps
// there, BUT deltablue's #1 sink is t_get_prop_any at 50,816/run (proto-
// inherited DATA reads, which own-data-poly cannot cover) and t_kfn_code
// at 30,579/run (`.call(this,…)` misses call_method_mono). Fix those first.

// ── perf6 AFTER (04c63b6+wt, 2026-07-13) — integrate-gate-all ────────────────
// Captured with S(t_method_ic_warm FFI) + T(new_simple pdict-seed) +
// U(apply-arguments) + V(proto-default-preshape) + perf6_mut_this_c=True +
// perf6_block_let_case=True + perf6_letrec_float=True (emit_core Block
// let-case with l_miss letrec hoisted above a multi-value `let <st,r>=…` so
// the anf.share hit-leaf is a zero-alloc `<st,v>` values-list) +
// bisect-raytrace-perf5 gates + share(frame_path) + stmt.with_this_c_slot
// slot -2 carry-fix, all applied in working tree.
// gleam test: 1573/0 ✓. bisect: 62/62 ✓ (60 + apply_args + t_while_this_after).
//
//                        richards   deltablue     crypto    raytrace
//   perf5 (04c63b6)        ~3,461     ~19,650   ~252,745    ~683,000
//   perf6 let-case (b5)     3,526      15,372    229,704     162,115
//   perf6 +letrec-float     3,570      15,657    226,657     157,351
//   ─── HARD GATE ────────  ≤2,000        ≤p5        ≤p5    ≤287,000
//   gate                       ✗           ✓           ✓           ✓
//   ref qjs                 1,917       3,825      51,323      19,477
//
// letrec-float 8-run min: richards 3,570 · deltablue 15,657 · crypto
// 226,657 · raytrace 157,351. raytrace RESTORED (683k→157k). deltablue/
// crypto both under perf5. richards ≤2,000 gate NOT MET — letrec-float
// (the CValues lowering) fires on all 451 anf.share sites and mb_deep.S
// confirms the hit path compiles to bare register moves in isolation, but
// on richards' jsf bodies beam_ssa emits the same code as the tuple path;
// see profile.gleam:792-838 for the full attribution and the
// `js_direct_nostate` ABI change now identified as the required next
// lever.
//
// PRIOR-BLOCKER ROOT CAUSE (was: ir_to_beam CompileFailed all four,
// jsf_*_t unbound `V_5ft<N>`): NOT expr.gleam share(frame_path) as first
// diagnosed — stmt.gleam with_this_c_slot only carried slot -1 through
// loop LoopParams; the this-sid-hoist added slot -2 (rebound alongside -1
// by every refresh_this_c/share). A `_t` body with a while/for whose
// cond/body touches `this.*` leaked slot -2's inner join name past the
// Loop; a subsequent `this.x` read → unbound var. Repro: bisect
// t_while_this_after. Fix: with_this_c_slot prepends -2 when live.
// ─────────────────────────────────────────────────────────────────────────────

// ── perf7 BEFORE (a7ebc74 HEAD, 2026-07-13) — baseline-capture ───────────────
// Captured at a7ebc74 with all perf6 flags at their landed values:
// perf6_mut_this_c=True (expr.gleam:69 + func.gleam:40), perf5s_method_ic_ffi
// =True (expr.gleam:44), perf5_this_c_hoist=True (func.gleam:33),
// perf6_block_let_case=True (2core emit_core.gleam:5837), perf6_letrec_float
// =True (:5845). No perf7 changes applied. This is the ground-truth every
// perf7 unit's "no-regression" check compares against.
// gleam test: 1573/0 ✓. bisect: 62/62 ✓ (60 + apply_args + t_while_this_after).
//
//                        richards   deltablue     crypto    raytrace
//   perf6 AFTER (:100)      3,570      15,657    226,657     157,351
//   perf7 BEFORE            3,745      15,983    225,615     177,401
//   ─── HARD GATE ──────── ≤2,000     ≤15,983   <225,615    ≤177,401
//   ref qjs                 1,917       3,825     51,323      19,477
//
// The perf6-AFTER row is the letrec-float 8-run min from :100/:105; the
// perf7-BEFORE row is a fresh single a7ebc74 run — deltas are thermal drift
// (raytrace 157k↔177k spans the a7ebc74 commit-msg's own "→177k"). No 12.7k
// deltablue found in code/git/.local — best-recorded is 15,372
// (profile.gleam:810); deltablue-bisect hunts the 15,372→15,983 delta.
//
// richards_local letrec-apply: 57,942/run (256 fns; profile.gleam:804) — the
// perf6 let-case already dropped 434,957→57,942 (−87%). Residual = code_t
// MakeClosure wrappers (~40k) + l_miss cold-path applies + Loop letrecs.
// W/X units target the l_miss residual (~15-20k applies).
//
// profile top-10+targeted (traced call_time; profile.gleam sections):
//   deltablue (:485-488, 3b198d8-era counts — perf6 not re-profiled):
//     t_call_method_ic 115,440/run · t_ic_get 20,099 · ic_proto_walk 4,716
//     t_get_prop_own_data 19,955 · t_global_get_fast 37,317. 3-level
//     inheritsFrom chains → poly where richards is mono; deltablue-bisect
//     suspects perf6_mut_this_c (read_this_c → pdict_get on poly shapes).
//   crypto (:590-648, P section — 3b198d8 counts, perf6 same shape):
//     t_set_elem_fast 868,243/run 224ns · t_get_elem_fast 2,432,497 71ns ·
//     t_shr_fast 2,324,601 36ns · set_on_receiver 76,697 455ns (append-miss)
//     · t_shl_fast 771,995 38ns · t_cell_get 435,843 57ns. y1 targets
//     append-miss (~83ms), y2 targets shr/shl (~113ms), y3 targets
//     set_elem St-rebuild (~130ms).
//   raytrace (:518-588, Q section — 3b198d8 counts; U/V landed since):
//     new_simple_apply 66,598 2,511ns · own_property_of 462,099 201ns ·
//     t_new_arguments 66,596 854ns · t_call_checked 66,597 · t_ic_get
//     137,147 229ns · t_call_method_ic 148,357 · kfn_code (z-verify checks
//     whether U's emit_apply_arguments dropped this to ~0 or still ~66k).
//
// perf7 units: deltablue-bisect · w1-let-case-nested-miss · w2-share-inline-
// cold-few · x-cold-as-calldirect · y1-set-elem-append · y2-shr-band-inline ·
// y3-set-elem-pdict-overlay · z-raytrace-apply-verify · gate-verify.
// richards ≤2,000 is architecturally gated on `js_direct_nostate` ABI
// (profile.gleam:834-843) — W/X are the within-scope attempts; a floor of
// ~2,200-2,800 after W/X is the documented cap, not a unit failure.
// ─────────────────────────────────────────────────────────────────────────────

// ── db-bisect: perf6 flag-combo → {richards, deltablue} (2026-07-13) ─────────
// Method: each perf6 flag flipped False in isolation + the 2×2 mut_this_c×
// letrec_float corner, `gleam build && gleam run -m emit_2core_v8v7_probe`,
// flags restored after. All perf7_* flags forced False for the duration
// (isolate the perf6 surface from in-progress concurrent perf7 work). Flags:
// perf6_mut_this_c (func.gleam:40 + expr.gleam:69 — flipped together),
// perf6_block_let_case (2core emit_core.gleam), perf6_letrec_float (ibid.).
//
//   flag-combo mut/case/float    richards   deltablue    vs perf7-BEFORE db
//   T · T · T  (perf6 HEAD)         3,672      17,749         +1,766
//   F · T · T  (mut_this_c off)     3,666      17,628         +1,645
//   T · F · T  (block_let_case off) 3,528      17,321         +1,338
//   T · T · F  (letrec_float off)   3,659      17,347         +1,364
//   F · T · F  (mut off, float off) 3,668      17,766         +1,783
//   ─── target ─────────────────── ≤3,745     ≤12,700
//   perf7-BEFORE (:136)             3,745      15,983
//
// FINDING: NO perf6 combo reproduces deltablue ≤12,700 — best is T·F·T at
// 17,321. All combos keep richards ≤3,745 ✓. mut_this_c has ~0 effect (±120µs
// on both benches, within noise) — the deltablue-bisect this_c_cache narrowing
// (func.gleam:1640-1656, expr.gleam read_this_c :1546-1550) already replaced
// perf6's per-read pdict_get with a 0-op cached-var read, so True/False now
// converge. block_let_case=False shows −428µs db / −144µs richards vs T·T·T,
// but that delta sits ENTIRELY inside the ~1,800µs unexplained band below —
// it is not a supported lever claim without a controlled a7ebc74 baseline.
// The 12.7k target is NOT on the perf6 flag surface: the ~1,800µs gap between
// T·T·T here (17,749) and perf7-BEFORE (15,983) — same nominal perf6 state —
// is real ungated tree drift since a7ebc74, NOT capture contention (richards
// in the same T·T·T row is 3,672, i.e. 73µs FASTER than perf7-BEFORE's 3,745;
// contention inflates uniformly). Ungated candidates to bisect individually
// with perf7 off: this_c_cache narrowing (func.gleam:1640-1656 / expr.gleam
// :1546-1550) vs needs_args_object walker. Next lever for deltablue is outside
// perf6: profile the this_c_cache-narrowed tree (deltablue-bisect owns that
// fix's measurement).
// ─────────────────────────────────────────────────────────────────────────────

// ── perf7 AFTER (a7ebc74+wt, 2026-07-13) — integrate-gate ────────────────────
// WINNING SUBSET: perf7_share_dup=True (w2) · perf7_arr_pdict=False (y3
// REVERTED per HARD RULE — deltablue regressor; Length≥8 narrowing tested,
// insufficient, see below) · perf7_args_elide=True (z) · perf7_this_c_cache
// =False (REVERTED — no measured win) · perf7_nested_let_case=False (w1
// REVERTED) · perf7_cold_outline=False (x REVERTED). Always-on (ungated):
// y1 set-elem-append, y2 int_const_shift/erl_bsr/erl_bsl JPure, z3
// shaped_unflat 3×delete_element (obj_ffi :845), JMutMiss reclass for
// set_elem_fast (emit_core :4104-4112), perf5_cfunref_zero_capture=True
// (emit_core :2449; codegen_ffi no_type_opt-retry guard).
// gleam test: 1573/0 ✓. bisect: 74/74 ✓ (62 + append/shr-neg/arr-pdict +
// switch-*/cold-* correctness cases).
//
//                        richards   deltablue     crypto    raytrace
//   perf7 BEFORE (:136)     3,745      15,983    225,615     177,401
//   perf7 AFTER (min-3)     3,691      16,213    183,606     131,560
//   ─── HARD GATE ──────── ≤2,000     ≤15,983   <225,615    ≤177,401
//   gate                       ✗           ✗           ✓           ✓
//   ref qjs                 1,917       3,825     51,323      19,477
//
// richards_local letrec-apply: 15,529/run (220 fns) ← 57,942/run (256 fns)
// baseline (:146) — a −73% cut, driven by perf5_cfunref_zero_capture=True
// (kills the 40,489 code_t MakeClosure eta-wraps; richards_local.gleam:14-48).
//
// PER-UNIT ATTRIBUTION (v8v7_probe deltas + richards_local.gleam:30-40):
//   deltablue-bisect  this_c_cache — REVERTED (perf7_this_c_cache=False):
//                     no measurable win on any bench; deltablue-regressor
//                     candidate (bisect below). Gate kept for re-test.
//   w1 nested-l-case  REVERTED (=False). UNSOUND: `Break(l_join)` under an
//                     inner l_miss let-case bypasses cold entirely; on the
//                     later fix hangs richards at runtime (probe stuck post
//                     realm-init). richards_local.gleam:37-40.
//   w2 share_dup      NO-OP on richards (get_prop_fast n_miss ~4-7, ≤2 gate
//                     never fires; richards_local.gleam:34-36). Left =True
//                     for other-bench share() sites; no measured regression.
//   x  cold_outline   REVERTED (=False). deltablue/crypto CompileFailed
//                     `EmitFailed(UnknownFunction("jsf_cold_pg_N"))` — aux_fn
//                     added to Emitter2 fn-list AFTER the enclosing jsf's
//                     body captured its function set. richards_local.gleam
//                     :32-33 shows the win is only 230 applies (0.4%) anyway.
//   y1 set_elem-appd  crypto append-miss set_on_receiver 76,697→~0/run
//                     (obj_ffi :458-497 `Idx =:= Length` clause). ~−35ms.
//   y2 shr/band-inl   crypto `>>C`/`<<C` int-literal → erlang:bsr/bsl JPure
//                     (expr.gleam:991-1020). No measured richards/db effect.
//   y3 arr_pdict      REVERTED (=False, HARD RULE). crypto set_elem St-
//                     rebuild 868k×~150ns → JPure pdict overlay. Length≥8
//                     tc_arr_install gate (obj_ffi :461-466) tested — cuts
//                     deltablue cost 17,474→16,582 but STILL ✗: gate is
//                     read-side only, so short arrays never install →
//                     t_set_elem_fast_p (JPure, no St) misses on every write
//                     → set_on_receiver slow path. crypto with arr_pdict=
//                     False min-3 = 183,606 (y1+y2 alone <gate; earlier
//                     231,203 single-run was contended). Overlay retained
//                     for perf8 arr_c_hoist (crypto ≥8-elem BigInteger).
//   z  apply-args     raytrace `t_new_arguments` elided when body's only
//                     `arguments` ref is `.apply(_, arguments)`
//                     (needs_args_object_*). z3 shaped_unflat LC→BIF kills
//                     raytrace #2 sink (~111ms traced). 177,401→122,711.
//   cfunref_zero_cap  richards letrec-apply 57,942→15,529 (kills bucket (a)
//                     code_t wrappers, 72% of applies). µs 3,745→3,425 only:
//                     letrec-apply was NOT the richards floor — the floor is
//                     t_call_method_ic + `{V,St}` thread (:70-74).
//
// PER-FLAG BISECT (min-3; others at winning subset above; perf8 flags off):
//                             richards   deltablue     crypto    raytrace
//   arr_pdict=T (pre-gate)       3,508      17,474    181,361     122,344
//   arr_pdict=T + Length≥8       3,844      16,582    185,008     135,052
//   arr_pdict=False (WINNING)    3,691      16,213    183,606     131,560
//   this_c_cache=True (was)      3,534      17,241    184,031     121,443
//   args_elide=False             3,787      17,455    185,579     182,508
//   arr_pdict=F ∧ this_c=F       3,811      15,999    241,627     124,178
//
// GATES: crypto ✓ (−19%) · raytrace ✓ (−26%). richards ✗ 3,691 — the
// js_direct_nostate ABI (profile.gleam:834-843) is the required next lever;
// W/X capped at ~15.3k letrec-applies which is only ~−320µs off richards.
// deltablue ✗ 16,213 (+1.4%, +230µs) — Length≥8 tc_arr_install narrowing
// (obj_ffi :461-466) recovered 892µs (17,474→16,582) but arr_pdict=True
// STILL ✗: read-side-only gate leaves t_set_elem_fast_p (JPure, no St)
// missing on every short-array write. arr_pdict REVERTED per HARD RULE →
// 16,213. Residual +230µs sits in the ~1,800µs ungated a7ebc74.. drift band
// (:202-208): NOT on the perf7 flag surface. NO perf7 subset satisfies all
// four gates. Next lever (perf8): fn_call_direct on `.superConstructor.call`
// + t_get_elem_fast_p undefined→tail-call (perf8 provenance block below).
// ─────────────────────────────────────────────────────────────────────────────

// ── perf8 db-12,778 provenance (2026-07-13) — reachability conclusion ────────
// Task states deltablue ≤12,778 as the perf5 best-ever gate. The perf7-BEFORE
// note (:142-144) recorded no such capture in code/git/.local. Re-searched at
// perf8 start with the stash list now included:
//
//   git stash list (2 entries) — neither carries a 12,778 measurement:
//     stash@{0} "abandoned decoupling round" — perf8 fn_call_direct lever
//       (expr/state/stmt +423L, bisect +21L); no probe/profile capture.
//     stash@{1} WIP on 5f11673 — interpreter refactor, pre-emit_2core-bench.
//   .local/ (144 files) — grep '12,778|12778' → 0 deltablue-µs hits (only
//     V_5ft12778 var-ids in richards_core.erl + test262 pass-counts in
//     post-fix-run.log). perf8_flag_bisect.tsv is header-only (run pending).
//   git log --all -p (977 commits) → 0 hits on any probe/profile revision.
//
// ACHIEVABLE FLOOR via flag-bisect+DD alone: ~15,372µs — the perf6 let-case
// row (:99 / profile.gleam:810). Flag surface is exhausted: perf6 combos
// (:185-193) best T·F·T=17,321; perf7 combos (:270-276) best arr_pdict=F∧
// this_c=F=15,999; neither closes on 12,778. DD (method-ic-multihop) is a
// verify — deltablue's 3-level inheritsFrom already caches via ic_proto_walk
// (MONO_PROTO_MAX=4); a confirmed thrash would recover ≤~1,000µs (ic_proto_
// walk 4,716/run × ~200ns, :153).
//
// MECHANISM GAP 15,372→12,778 (−2,594µs, −17%): NOT on the flag surface and
// NOT recoverable by DD alone. Requires NEW levers stacked on the ungated-
// drift recovery (:202-208 names this_c_cache narrowing / needs_args_object /
// JMutMiss reclass / y1·y2·z3 always-on as the ~1,800µs a7ebc74.. band):
//   (a) fn_call_direct — `.superConstructor.call(this,…)` → kfn_code+simple
//       ABI, bypassing t_call_checked reflect (deltablue.js:216/297/471;
//       stash@{0} has an abandoned draft).
//   (b) arr_pdict write-side Length gate — t_set_elem_fast_p installs on
//       OrderedCollection.add's `elms[elms.length]=v` regardless of the
//       read-side ≥32 gate (obj_ffi.erl:461 gates reads only).
//   (c) t_get_elem_fast_p undefined→tail-call — one pdict-get overhead per
//       short-array read (:281-282; deltablue.js:68 `this.elms[index]`).
// CONCLUSION: 12,778 is task-provided and ASPIRATIONAL — no recorded run
// ever reached it. perf8's deltablue gate is the documented floor after
// (a)+(b)+(c)+drift-recovery land; if the min-of-5 remains >12,778 the gap
// is the residual t_call_method_ic 115,440/run floor (:153) — same
// js_direct_nostate ABI ceiling as richards (:172-174, profile.gleam:834).
// ─────────────────────────────────────────────────────────────────────────────

// ── perf8 ungated-drift bisect (a7ebc74..f289f0c) — deltablue delta ──────────
// Per-change db delta, other perf8 levers OFF (isolate the a7ebc74..f289f0c
// diff surface; :203-207 identified ~1,800µs ungated drift). NEW gates:
//
//   ungated change              gate const               db delta   other-win
//   is_diverging Let-spine      perf8_is_diverging_let    +~2,000   rich −~275
//   int_const_shift/ushr_fast   perf8_int_const_shift        0      crypto y2
//   this_c_cache narrowing      (perf7_this_c_cache=F ⇒ no-op)  0    —
//   needs_args_object walker    (perf7_args_elide gates it)     —    raytrace
//   JMutMiss set_elem_fast      (arr_pdict=T ⇒ dead; erl-ABI-coupled)  crypto
//   y1 append + tc_arr_sync     (arr_pdict=T ⇒ dead; erl runtime)      crypto
//   z3 shaped_unflat delete_el  (erl runtime; for-in-only)      —    raytrace
//
// FINDING: is_diverging's Let-spine walk (emit_core :5766-5770) IS the
// ~1,800µs regressor — cont-inline into the surviving arm defeats let-case
// on deltablue's `if(…){…return}…rest` shapes (Constraint.satisfy et al).
// perf8_is_diverging_let=False (default). y2 gated True (deltablue has 0
// shift ops — bisect-only). Remaining candidates already gated or below
// 200µs threshold or erl-ABI-coupled with other-bench win.
// ─────────────────────────────────────────────────────────────────────────────

import arc/compiler/emit_2core
import emit_2core_harness as harness
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/int
import gleam/io
import gleam/string
import simplifile
import test_runner
import twocore/backend/build_beam
import twocore/pipeline
import twocore/runtime/rt_js_builtins
import twocore/runtime/rt_js_store
import twocore/runtime/rt_state.{type InstanceState}

type TimeUnit {
  Microsecond
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(unit: TimeUnit) -> Int

fn time_us(f: fn() -> a) -> #(Int, a) {
  let t0 = monotonic_time(Microsecond)
  let r = f()
  #(monotonic_time(Microsecond) - t0, r)
}

const budget_us = 5_000_000

fn reps_for(warm: Int) -> Int {
  case warm {
    0 -> 200
    _ -> int.min(200, int.max(3, budget_us / warm))
  }
}

type Outcome {
  CompileFailed(stage: String, err: String)
  RunFailed(err: String, stdout: String)
  Measured(per_us: Int, reps: Int)
}

fn repeat(n: Int, f: fn() -> a) -> Nil {
  case n {
    0 -> Nil
    _ -> {
      let _ = f()
      repeat(n - 1, f)
    }
  }
}

// ── compile-once / run-only split (ported from emit_2core_bench.gleam) ──
// The hot loop must time ONLY the js_main apply so the emit_2core column is
// comparable to the qjs/llint refs — not compile+load+realm-init per rep.

/// A compiled+loaded JS module plus the seeded realm state each apply starts
/// from. `InstanceState` is a pure threaded record and `apply_js_main` clears
/// its process-dict overlay on entry, so re-applying the SAME `seed` observes
/// an identical fresh realm every rep.
type Loaded {
  Loaded(mod: Atom, seed: InstanceState)
}

/// Wire terms from `twocore_rt_js_exec_ffi:apply_js_main/2` — mirrors the
/// private `pipeline.JsExecOutcome`. Tag atoms MUST match the Erlang side.
type JsExecOutcome {
  JsReturned(value: Dynamic)
  JsThrew(exn: Dynamic)
  JsCrashed(reason: String)
}

@external(erlang, "twocore_rt_js_exec_ffi", "apply_js_main")
fn ffi_apply_js_main(
  mod: Atom,
  st: InstanceState,
) -> #(JsExecOutcome, InstanceState)

fn seed_realm() -> InstanceState {
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

/// One js_main apply from the shared seed → `DiffRun` (stdout from the
/// returned state's console buffer, result mapped from the FFI outcome).
fn run_once(loaded: Loaded) -> pipeline.DiffRun {
  let #(outcome, st) = ffi_apply_js_main(loaded.mod, loaded.seed)
  let stdout = rt_js_store.t_console_bytes(st)
  let result = case outcome {
    JsReturned(v) -> Ok(v)
    JsThrew(e) -> Error("uncaught: " <> string.inspect(e))
    JsCrashed(reason) -> Error(reason)
  }
  pipeline.DiffRun(stdout:, result:)
}

fn bench_compiled(name: String, source: String) -> Outcome {
  let mod_name = "arc_v8v7_" <> name
  let opts =
    emit_2core.CompileOpts(
      module_name: mod_name,
      source_kind: emit_2core.AsScript,
      entry_name: "js_main",
    )
  let #(emit_us, emit_r) =
    time_us(fn() { emit_2core.compile_source(source, opts) })
  case emit_r {
    Error(e) -> CompileFailed("emit_2core", string.inspect(e))
    Ok(unit) -> {
      let #(lower_us, lower_r) =
        time_us(fn() { pipeline.compile_ir(unit.module, emit_2core.binding()) })
      io.println(
        "  compile: emit_2core=" <> int.to_string(emit_us) <> "µs ir_to_beam="
        <> int.to_string(lower_us) <> "µs → "
        <> case lower_r {
          Ok(_) -> "OK (BEAM produced)"
          Error(e) -> "FAILED: " <> string.slice(string.inspect(e), 0, 200)
        },
      )
      case lower_r {
        Error(e) -> CompileFailed("ir_to_beam", string.inspect(e))
        Ok(beam) ->
          case build_beam.load_module(atom.create(mod_name), mod_name, beam) {
            Error(e) -> CompileFailed("beam_load", e)
            Ok(mod) -> {
              let #(realm_us, seed) = time_us(seed_realm)
              io.println(
                "  realm-init: " <> int.to_string(realm_us) <> "µs (once)",
              )
              let loaded = Loaded(mod:, seed:)
              let #(warm_us, first) = time_us(fn() { run_once(loaded) })
              case first {
                pipeline.DiffRun(result: Error(e), stdout:) ->
                  RunFailed(e, string.inspect(stdout))
                pipeline.DiffRun(result: Ok(_), stdout:) ->
                  case stdout {
                    <<"ok\n":utf8>> -> {
                      let reps = reps_for(warm_us)
                      let #(us, _) =
                        time_us(fn() {
                          repeat(reps, fn() { run_once(loaded) })
                        })
                      Measured(us / reps, reps)
                    }
                    _ -> RunFailed("stdout mismatch", string.inspect(stdout))
                  }
              }
            }
          }
      }
    }
  }
}

fn bench_interp(source: String) -> Outcome {
  let #(warm_us, first) = time_us(fn() { harness.run_interpreted(source) })
  case first {
    pipeline.DiffRun(result: Error(e), stdout:) ->
      RunFailed(e, string.inspect(stdout))
    pipeline.DiffRun(result: Ok(_), stdout:) ->
      case stdout {
        <<"ok\n":utf8>> -> {
          let reps = reps_for(warm_us)
          let #(us, _) =
            time_us(fn() {
              repeat(reps, fn() { harness.run_interpreted(source) })
            })
          Measured(us / reps, reps)
        }
        _ -> RunFailed("stdout mismatch", string.inspect(stdout))
      }
  }
}

fn show(o: Outcome) -> String {
  case o {
    Measured(us, r) -> int.to_string(us) <> " µs (×" <> int.to_string(r) <> ")"
    CompileFailed(stage, e) -> "COMPILE FAIL [" <> stage <> "]: " <> e
    RunFailed(e, out) -> "RUN FAIL: " <> e <> " | stdout=" <> out
  }
}

fn per_us(o: Outcome) -> Int {
  case o {
    Measured(us, _) -> us
    _ -> -1
  }
}

/// External-engine reference µs for one iteration, captured via
/// `bash bench/v8-v7/time_external.sh` (min-of-5). Re-run to refresh.
fn ext_refs(name: String) -> #(Int, Int) {
  // #(qjs, bun-llint)
  case name {
    "richards" -> #(1917, 1744)
    "deltablue" -> #(3825, 3183)
    "crypto" -> #(51_323, 34_054)
    "raytrace" -> #(19_477, 14_543)
    _ -> #(-1, -1)
  }
}

fn one(name: String) {
  let path = "bench/v8-v7/" <> name <> "_run.js"
  io.println("")
  io.println("═══ " <> name <> " (" <> path <> ") ═══")
  case simplifile.read(path) {
    Error(e) -> io.println("!! read failed: " <> string.inspect(e))
    Ok(source) -> {
      io.println(
        "  source: " <> int.to_string(string.length(source)) <> " chars",
      )
      let compiled = bench_compiled(name, source)
      io.println("  emit_2core : " <> show(compiled))
      // PROBE_SKIP_INTERP=1 skips the interpreter column — flag-bisect flips
      // only emitter consts, so re-timing the (unchanged) interpreter per
      // combo just wastes ~5s/bench.
      let interp = case test_runner.get_env_is_truthy("PROBE_SKIP_INTERP") {
        True -> Measured(0, 0)
        False -> bench_interp(source)
      }
      io.println("  arc-interp : " <> show(interp))
      let #(qjs, llint) = ext_refs(name)
      io.println("  ref qjs    : " <> int.to_string(qjs) <> " µs")
      io.println("  ref llint  : " <> int.to_string(llint) <> " µs")
      io.println(
        "  ROW " <> name <> "\t" <> int.to_string(per_us(compiled)) <> "\t"
        <> int.to_string(per_us(interp)) <> "\t" <> int.to_string(qjs) <> "\t"
        <> int.to_string(llint),
      )
    }
  }
}

pub fn main() {
  io.println("emit_2core V8-v7 probe — one call = one full bench iteration")
  io.println("  ROW bench\temit_2core\tarc_interp\tqjs\tllint")
  one("richards")
  one("deltablue")
  one("crypto")
  one("raytrace")
}
