# Cross-Cut Naming/Shape Rulings

One-line canonical picks for names/shapes that multiple M-drafts answered differently. Every KEEP file (M2, M5, M6, M7, M9, M10-M11, M12, M17, u-call-abi, u-ffi-shared) and every SPEC.md §7.M*/§8 edit MUST match these exactly. Where a draft disagrees, the draft is STALE — this file wins.

| R# | Ruling | Applies to |
|---|---|---|
| **R1** | Return-tuple order is `#(V, St')` — value FIRST, state SECOND (ground truth `emit_core.gleam:1559,1936`); any `#(St, V)` / `#(InstanceState, a)` / `{St', V}` in a draft is wrong. | M5 M6 M7 SPEC§7 |
| **R2** | Exception tag string is `"js_exn"` (NOT `js_throw`, NOT `js_exc`); `pub const js_exn_tag = "js_exn"`; wire payload order `{wasm_exn, 0, [St, V]}` — St FIRST. | M9 M17 u-call-abi SPEC§4 |
| **R3** | Binding flag is `js_profile: Bool` (NOT `uniform_state_threading`); §9.10/§9.11 gate on `ctx.binding.js_profile`, WASM path byte-identical. | M9 SPEC§1.D3/D18 SPEC§3 |
| **R4** | Slot type name is `JsSlot` (NOT `JsCell`); the Handle WIRE constructor stays `{js_cell, N}` for on-wire compat — type rename only. | M2 SPEC§2.4 |
| **R5** | Object-kind type name is `ObjKind` (NOT `ExoticKind`); canonical constructors `KFunction`/`KNative`/`KBound`/`SObject`. | M6 SPEC§2.4 SPEC§7.M1a |
| **R6** | Cell-handle type name is `Handle` (NOT `CellHandle`). | M3 u-call-abi SPEC§2 |
| **R7** | `TupleGet` indices are 0-based; `_frame` fields: `this=0`, `active_func=1`, `home_object=2`, `new_target=3`. | M14 M16 u-call-abi SPEC§3.5 |
| **R8** | `strict_eq` / `strict_ne` are classified `JPure` (no St param, body is pure); the separate `strict_eq_prim` op/row is DELETED. | M5 M12 SPEC§8 |
| **R9** | `JRead` (4th `JsOpKind`: takes St, returns bare V, `sc` unchanged) classifies exactly: `cell_get`, `private_in`, `fn_home_object`, `is_constructor`. | M7 M9 SPEC§8 |
| **R10** | `KNative` dispatch key is `NativeToken` sum-type (NOT `Int`); `KNative(tag: NativeToken)`. | M6 SPEC§2.4 SPEC§8 |
| **R11** | `MethodInstallKind` is a sum-type (NOT `Int`/`String`). | M7 SPEC§7.M7 |
| **R12** | Compile-time rejects return `Error(EarlySyntaxError(..))` (frontend) / `Error(UnknownJsOp(name))` (`resolve_js` None-arm) — NEVER `panic`. | M9 M12 SPEC§7 |
| **R13** | `EmitDispatch` is a **7-field** record (`emit_expr`, `emit_stmts`, `emit_pattern`, `emit_function`, `emit_class`, `emit_async_body`, `emit_destructure`), defined in **M10 `emit_2core/state.gleam`** (leaf, alongside `Emitter2`), **NOT `opaque`**. `Build(a)` monad is defined in **M11 `emit_2core/anf.gleam`** with signature `pub type Build(a) = fn(Emitter2, fn(Emitter2, a) -> ir.Expr) -> ir.Expr` (M12.md:32) — the `#(Emitter2, ir.Expr)`-returning variant and direct-CPS `Emit` alias are REJECTED. | M10-M11 M12 SPEC§1.D13 SPEC§7.M10-M11 SPEC§7.M19 |
| **R14** | `EmitDispatch.emit_function` return type is `Result(#(ir.Expr, Emitter2), EmitError)` (NOT the SPEC§7.M10-frozen `#(Emitter2, ir.Value)`). SPEC§7.M14:1388's closure-site `bind(MakeClosure(…), λfun. host("fn_new",…))` is a Let-chain `ir.Expr`; `ir.Value` (2core/ir.gleam:471-496) has no Expr-carrying variant, so the frozen sig was unsatisfiable. Matches R1 `#(V, St')` order and 5/6 sibling dispatch fields. `state.gleam:228` one-line amendment sanctioned. | M10 M14 SPEC§7.M10 SPEC§7.M14 |
| **R15** | JS `continue` lowers to `ir.Break(frame.ir_continue, carried)` — **NOT** `ir.Continue`. `Frame2.Loop2.ir_continue` names an INNER `ir.Block` wrapping just the loop body (NOT the `ir.Loop` head). Rationale: `ir.Continue` re-enters the Loop from the top, which for `do-while`/`for(;;)` skips the update expression and post-test — unimplementable per SPEC§7.M13:1339. Uniform shape for every iteration statement (while/do-while/for/for-in/for-of): `Block(Lbrk,…, Loop(Lloop, params,…, …Block(Lcont, [TTerm×n], body)…; <update/test>; ir.Continue(Lloop, carried)))`; `push_loop(e, Lbrk, Lcont, carried, …)`. `stmt.emit_continue` emits `ir.Break` unconditionally; the loop emitter owns the sole `ir.Continue(Lloop,…)` after the inner Block. Overrides SPEC§7.M13 `Continue` row and the `While` row's `Continue(Lcont,…)` inside body. | M10 M13 SPEC§7.M13 |

Provenance: spec-review.md PART 5 (corrected-spec diff) + disposition table. See SPEC.md §1 D-table for rationale (D1/D6/D13/D16-D18).
