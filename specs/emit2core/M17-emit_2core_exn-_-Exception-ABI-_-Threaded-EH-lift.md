# M17 `emit_2core/exn` + Exception ABI + Threaded-EH lift — FINAL SPEC

## 0. CRITICAL CORRECTION to the brief

The brief asserts: *"each handler emitted with sc=Threading(cur) (SAME cur as at Try entry — mutations inside body-before-throw are already in cur because every rt_js op returned updated st and rebound cur before the Throw)"*.

**This is incorrect and would lose mutations.** `StateChan.Threading(cur)` holds a Core-variable **NAME** (emit_core.gleam:277-280). Every mutating rt_js op mints a **fresh** name via `fresh_var` and continues under `Threading(fresh)` (see `emit_value_state_pair` at emit_core.gleam:1919-1940, `emit_threaded_record_effect` at 1947-1965). The entry-time `cur` variable still holds the **pre-body** state. When the body throws, the up-to-date state var (`st_N`) is lexically local to the hoisted `trybody/0` closure (emit_core.gleam:5151-5156) and out of scope in the handler. Emitting the handler under the entry `cur` would rewind all mutations made before the throw — semantically wrong for JS (`try{o.x=1;throw 0}catch{o.x}` must see `1`).

**Resolution (fixed design decision for this spec):** the live `InstanceState` **travels in the exception payload**. Under `Threading`, `emit_throw` PREPENDS `CVar(cur)` to the payload list, and `emit_catch_clause` PREPENDS a fresh `st_caught` binder to the payload pattern and emits the handler body under `Threading(st_caught)`. rt_js ops that raise from Erlang produce the identical wire shape so a single catch destructuring works for both compiler-emitted `throw` and runtime-raised errors.

---

## 1. Exception ABI (the ONE wire shape)

### 1.1 IR-level (what `emit_2core` emits)

| Construct | IR emitted |
|---|---|
| Module tag decl | `ir.Module.tags = [TagDecl("js_exn", [TTerm])]` — exactly one tag, index **0** (ir.gleam:126). |
| `throw e` | `ir.Throw("js_exn", [e_val])` where `e_val: ir.Value` (ir.gleam:848). |
| `try{B}catch(x){H}` | `ir.Try([TTerm], B_ir, [CatchHandler(OnTag("js_exn"), ["<x_bind>"], None, H_ir)])` (ir.gleam:860, 992). Result type is `[TTerm]` — a JS `try` expression yields one JS value (statement-position: `js_undefined`). |
| `try{B}catch{H}` (no param) | payload name is a fresh discard var. |

`emit_2core` never emits `OnAll`, `ThrowRef`, or `exnref` — JS has one tag, and `catch` catches it unconditionally.

### 1.2 BEAM wire shape (what actually gets raised)

Reuses `twocore_rt_exn_ffi` unchanged (2core/src/twocore_rt_exn_ffi.erl:42-51):
```erlang
throw_exn(TagId, Payload) -> erlang:error({wasm_exn, TagId, Payload}).
match_tag({wasm_exn, TagId, Payload}, TagId) -> {ok, Payload};
match_tag(_, _) -> {error, nil}.
```

| Strategy | Raised term | Payload shape |
|---|---|---|
| `NoState` (Cell) | `{wasm_exn, 0, [ErrVal]}` | `[ErrVal]` |
| `Threading(cur)` | `{wasm_exn, 0, [St, ErrVal]}` | `[St, ErrVal]` — St is the live `InstanceState` at the throw point |

Class: **error** (unchanged from rt_exn). `resolve_tag(ctx, "js_exn") → 0` via emit_core.gleam:716-721.

### 1.3 How rt_js ops throw (M3/M4/M5 contract)

rt_js ops that need to raise a JS error (TypeError on `undefined.x`, `null()`, etc.) do **NOT** return `Result` — they diverge via a raise matching §1.2's Threading shape, so emit_core's existing `CTry` catches them without the emitter branching on every op result.

**File:** `2core/src/twocore_rt_js_store_ffi.erl` (M1's ffi) exports:
```erlang
-spec t_throw(InstanceState, ErrVal :: term()) -> no_return().
t_throw(St, ErrVal) -> erlang:error({wasm_exn, 0, [St, ErrVal]}).
```

Every `t_*` rt_js op that hits an error path calls `t_throw(St', ErrHandle)` where `St'` is the state **after** allocating the Error object (so the handle is live in the store the handler receives). Replaces today's `type_error(Detail) -> erlang:error({js_error, type_error, Detail})` at twocore_rt_js_ffi.erl:75, which is incompatible with the ABI.

**Gleam facade** in `rt_js_store.gleam` (M1):
```gleam
@external(erlang, "twocore_rt_js_store_ffi", "t_throw")
pub fn t_throw(st: InstanceState, err: JsVal) -> a   // bottom (D16)
```

Helper in `rt_js_obj.gleam` (M4):
```gleam
pub fn t_throw_type_error(st: InstanceState, msg: String) -> a
// = let #(st2, err_h) = t_new_error(st, TypeErrorProto, msg); rt_js_store.t_throw(st2, err_h)
```

### 1.4 Non-JS errors are NOT caught

`match_tag` returns `{error, nil}` for anything not `{wasm_exn, 0, _}` (twocore_rt_exn_ffi.erl:50-51); `try_dispatch`'s default arm re-raises via `rt_exn:reraise(C, R, build_stacktrace(S))` (emit_core.gleam:5200-5211). So a BEAM `badarg`, a `{wasm_trap, _}`, or any host error propagates through JS `try/catch` untouched — a JS `catch` catches **only** JS-thrown values. This is the sandbox floor (rt_exn.gleam:109-116 T7 invariant, preserved).

---

## 2. `emit_core.gleam` Threaded-EH lift (patch spec)

**File:** `../2core/src/twocore/backend/emit_core.gleam`

### 2.1 `emit_throw` — add `sc` parameter, prepend state

Replace (lines 5097-5106) with:
```gleam
fn emit_throw(
  tag: String,
  args: List(Value),
  sc: StateChan,
  state: EmitState,
  ctx: Ctx,
) -> Result(#(CExpr, EmitState), EmitError) {
  use tag_id <- result.try(resolve_tag(ctx, tag))
  let user_payload = list.map(args, emit_value)
  let payload = case sc {
    NoState -> core_list(user_payload)
    Threading(cur) -> core_list([CVar(cur), ..user_payload])
  }
  Ok(#(seam_call(exn_module, "throw_exn", [CInt(tag_id), payload]), state))
}
```
Update dispatch at line 1147: `ir.Throw(tag, args) -> emit_throw(tag, args, sc, state, ctx)`.

### 2.2 `emit_try` — remove "unsupported combo" note; body/handler wiring unchanged in shape

`emit_try` (5129-5181) **stays structurally identical** — the fix is in `emit_catch_clause`, not here. Delete the "Threaded+EH is a categorised-unsupported combo" doc line (5128). The nullary `trybody/0` hoist (5151-5156) remains correct: it closes lexically over the entry-state var, and normal completion flows via `exit_cont` = `KJump(join)` where `join` was widened by `materialize` under `Threading` (emit_core.gleam:2159-2176) — so normal exit passes `st_final` to the join. Exceptional exit carries state in the payload per §2.1/§2.3.

**One required change:** `try_dispatch` and `emit_catch_clause` must receive the **entry** `sc` only to know "am I Threading" (a discriminant), NOT as the state to use — the actual handler-state var comes from the payload.

### 2.3 `emit_catch_clause` — destructure state from payload under Threading

Replace `OnTag` arm (lines 5250-5266) with:
```gleam
ir.OnTag(tag) -> {
  use tag_id <- result.try(resolve_tag(ctx, tag))
  let match = seam_call(exn_module, "match_tag", [CVar(rvar), CInt(tag_id)])
  let #(wild, state2) = fresh_var(state1)
  case sc {
    NoState -> {
      use #(hbody, state3) <- result.try(emit(hexpr, exit_cont, NoState, state2, ctx))
      let hbody2 = wrap_exnref(exnref, rvar, hbody)   // existing exnref capture, unchanged
      Ok(#(
        CCase(match, [
          CClause([PTuple([PAtom("ok"), list_pattern(payload)])], CAtom("true"), hbody2),
          CClause([PVar(wild)], CAtom("true"), next),
        ]),
        state3,
      ))
    }
    Threading(_) -> {
      let #(st_caught, state3) = fresh_var(state2)
      // Handler body emitted under the STATE RECOVERED FROM THE PAYLOAD, not the entry sc.
      use #(hbody, state4) <- result.try(emit(hexpr, exit_cont, Threading(st_caught), state3, ctx))
      let hbody2 = wrap_exnref(exnref, rvar, hbody)
      Ok(#(
        CCase(match, [
          CClause(
            [PTuple([PAtom("ok"), list_pattern([st_caught, ..payload])])],
            CAtom("true"),
            hbody2,
          ),
          CClause([PVar(wild)], CAtom("true"), next),
        ]),
        state4,
      ))
    }
  }
}
```

`OnAll` under `Threading`: JS never emits it. Keep the existing `NoState` arm; add `Threading(_) -> Error(ThreadedOnAllUnsupported)` (a new `EmitError` variant) so a future misuse fails closed rather than silently losing state.

### 2.4 `try_dispatch` — reraise arm unchanged

Reraise (emit_core.gleam:5200-5211) stays as-is: `rt_exn:reraise(C, R, build_stacktrace(S))`. A non-matching reason (non-JS error) is re-raised WITH its state intact inside `R` (the whole `{wasm_exn, 0, [St, E]}` term), so an OUTER JS catch still recovers it. Non-`wasm_exn` reasons carry no state and propagate to the BEAM caller.

### 2.5 `emit_call_host` "js" — becomes state-threading (M9 seam)

Replace lines 3551-3563. Under `Threading(cur)`, a resolved rt_js op emits the `t_*` twin taking `St` first and returning `#(Val, St')` (mutating) or `Val` (pure), routed via a per-op kind table:

```gleam
type JsOpKind { JsPure | JsRead | JsMut }
// JsPure  → t_op(args…) -> Val         (no St; e.g. add, strict_eq, typeof)
// JsRead  → t_op(St, args…) -> Val     (reads store, no rebind; e.g. cell_get, get_prop)
// JsMut   → t_op(St, args…) -> {Val, St'} (rebinds; e.g. cell_new, set_prop, new_object)

fn resolve_js(op: String) -> Option(#(String, JsOpKind))
```

Dispatch:
```gleam
Threading(cur), JsPure  -> apply_cont_call(cont, CCall(js_mod, fn_atom, cargs), 1, sc, state, ctx)
Threading(cur), JsRead  -> apply_cont_call(cont, CCall(js_mod, fn_atom, [CVar(cur), ..cargs]), 1, sc, state, ctx)
Threading(cur), JsMut   -> emit_value_state_pair(CCall(js_mod, fn_atom, [CVar(cur), ..cargs]), cont, state, ctx)
```

`emit_value_state_pair` (emit_core.gleam:1919-1940) already does the `case … of {V, St2} -> continue under Threading(St2)` destructure — reused verbatim. **Any of these calls may raise `{wasm_exn, 0, [St, E]}` instead of returning** — the enclosing `CTry` catches it; the `case {V,St2}` pattern simply never matches on that path. No `Result` unwrapping in the emitter.

### 2.6 `state_reaching` seed set

`is_stateful_node` (emit_core.gleam:849-920) gains: `CallHost("js", op, _)` where `resolve_js(op)` is `JsRead|JsMut` → `True`. `Throw`/`Try` → `True` (a fn containing them is state-reaching so it's emitted at arity `n+1` and has a `cur` to prepend). Already partially covered by line 947's Try recursion — verify `Throw` is a seed.

---

## 3. `arc/src/arc/emit_2core/exn.gleam` — barrier-duplication `finally`

### 3.1 Frame type on `Emitter2` (ported from arc/compiler/emit.gleam:125-160)

```gleam
// arc/src/arc/emit_2core/state.gleam  (M10 owns the type; exn.gleam consumes it)
pub type Frame2 {
  Loop2(break_label: String, continue_label: String, js_label: Option(String),
        iterator_close: Option(ir.Value))     // Some(iter_handle) for for-of
  Switch2(break_label: String, js_label: Option(String))
  LabeledBlock2(break_label: String, js_label: String)
  Barrier2(
    finally_ast: Option(List(ast.StmtWithLine)),  // Some = re-emit before crossing
    in_finally_body: Bool,                         // True = inside F itself; crossing DISCARDS
                                                   //   the pending completion (spec §14.15.3)
  )
}
```

`Emitter2.frame_stack: List(Frame2)`. No `pop_try`/`drop_count` — those are stack-VM artefacts (arc bytecode); 2core IR has no runtime try-stack and no value-stack, so they vanish. `iterator_close` replaces arc's `iterator: Bool` + implicit stack slot: it holds the iterator **cell handle** so `emit_cross_frame2` can emit `CallHost("js", "iterator_close", [iter_h])` inline.

### 3.2 Public functions

```gleam
// arc/src/arc/emit_2core/exn.gleam
import arc/parser/ast
import arc/emit_2core/state.{type Emitter2, type Frame2, Barrier2}
import arc/emit_2core/anf
import twocore/ir

pub const js_exn_tag: String = "js_exn"

/// `throw e`  (ast.gleam:238)
pub fn emit_throw_stmt(e: Emitter2, arg: ast.Expression) -> Res
// = anf.bind(emit_expr(e, arg), fn(v) { ir.Throw(js_exn_tag, [v]) })

/// `try {B} catch(p?) {H}` — no finally  (ast.gleam:307)
pub fn emit_try_catch(e: Emitter2, block: List(ast.StmtWithLine),
                      param: Option(ast.Pattern), handler: List(ast.StmtWithLine)) -> Res

/// `try {B} finally {F}`  (ast.gleam:308)
pub fn emit_try_finally(e: Emitter2, block: List(ast.StmtWithLine),
                        finalizer: List(ast.StmtWithLine)) -> Res

/// `try {B} catch(p?) {H} finally {F}`  (ast.gleam:309)
pub fn emit_try_catch_finally(e: Emitter2, block: List(ast.StmtWithLine),
                              param: Option(ast.Pattern), handler: List(ast.StmtWithLine),
                              finalizer: List(ast.StmtWithLine)) -> Res

/// Walk frame_stack for `break [lbl]` / `continue [lbl]`; inline finally at each Barrier2
/// crossed; emit iterator_close at each Loop2(iterator_close: Some) crossed; terminate
/// with ir.Break(target, [js_undefined]) / ir.Continue(target, []).
/// Port of arc emit_goto_loop_walk (emit.gleam:2510-2528) + emit_cross_frame (2459-2474).
pub fn emit_break(e: Emitter2, js_label: Option(String)) -> Res
pub fn emit_continue(e: Emitter2, js_label: Option(String)) -> Res

/// Walk ALL frames (arc emit.gleam:3871 `list.fold(e.frame_stack, e, emit_return_cross_frame)`),
/// inline every finally, close every iterator, terminate with ir.Return([v]).
pub fn emit_return(e: Emitter2, v: ir.Value) -> Res

/// frame_target port (emit.gleam:2413-2454): does `frame` answer `break/continue name`?
fn frame_target(frame: Frame2, name: Option(String), is_cont: Bool) -> Option(String)

/// emit_cross_frame port (emit.gleam:2459-2474), Gosub → INLINE emission:
fn cross_barrier(e: Emitter2, frame: Frame2, then: fn(Emitter2) -> Res) -> Res
```

Where `Res = Result(#(ir.Expr, Emitter2), EmitError)` (M10 defines `EmitError`).

### 3.3 `emit_try_catch` (no finally) — IR shape

```
push Barrier2(finally_ast: None, in_finally_body: False)
B_ir = emit_stmts(e', block)               // yields ir.Expr producing 1 TTerm (js_undefined for stmt list)
pop frame
x_name = fresh OR from param (if Identifier); if Pattern, fresh + destructure prologue in H_ir
H_ir  = [enter catch scope] emit_destructure(param, Var(x_name)) ; emit_stmts(handler) [leave]
→ ir.Try([TTerm], B_ir, [CatchHandler(OnTag("js_exn"), [x_name], None, H_ir)])
```

Catch param destructuring: if `param` is `Some(Identifier(n))`, `x_name = scope_resolved(n)`. If `Some(pattern)`, `x_name = fresh`, and `H_ir` opens with M15's `emit_destructure_bind(pattern, ir.Var(x_name))`. If `None`, `x_name = fresh` (discarded). Mirrors arc `emit_catch_clause` (emit.gleam:3010-3027).

### 3.4 `emit_try_finally` — barrier-duplication IR shape

**No Gosub in 2core IR.** The finally AST is **re-emitted** at each of these sites (fresh IR each time — `emit_stmts(e, finalizer)` called N times):

```
let fin = finalizer   // the AST, held on Barrier2

// 1. Normal path:  B; F; result_of_B
push Barrier2(finally_ast: Some(fin), in_finally_body: False)
B_ir = emit_stmts(block)         // any break/continue/return INSIDE B that crosses this barrier
                                 // gets `emit_stmts(fin)` inlined BEFORE the transfer (see §3.5)
pop
F_normal_ir = emit_stmts_under_finally_barrier(fin)   // Barrier2(None, in_finally_body: True)
normal_ir = anf.seq(B_ir, F_normal_ir, then: <result of B_ir>)

// 2. Throw path:  catch e → F; rethrow e
ex = fresh
F_throw_ir = emit_stmts_under_finally_barrier(fin)
throw_handler = anf.seq(F_throw_ir, ir.Throw("js_exn", [Var(ex)]))

→ ir.Try([TTerm], normal_ir,
         [CatchHandler(OnTag("js_exn"), [ex], None, throw_handler)])
```

`emit_stmts_under_finally_barrier(fin)`: pushes `Barrier2(finally_ast: None, in_finally_body: True)`, emits `fin`, pops. The `in_finally_body: True` means a `break`/`continue`/`return` INSIDE F **replaces** the pending completion (spec §14.15.3 — arc's `drop_count: 2` analogue): crossing this barrier emits nothing extra (there is no saved gosub slot to drop; the transfer simply proceeds and the outer `ir.Throw`/outer break never executes because it's downstream of the transfer in the ANF chain).

### 3.5 `cross_barrier` — the Gosub→inline replacement

Port of `emit_cross_frame` (arc emit.gleam:2459-2474), but `label_finally: Some(lbl) → emit_gosub_normal` becomes **inline emission**:

```gleam
fn cross_barrier(e: Emitter2, frame: Frame2, then: fn(Emitter2) -> Res) -> Res {
  case frame {
    Loop2(iterator_close: Some(iter_h), ..) ->
      // CallHost("js", "iterator_close", [iter_h]) sequenced before `then`
      anf.bind_effect(ir.CallHost("js", "iterator_close", [iter_h]), fn() { then(e) })
    Loop2(..) | Switch2(..) | LabeledBlock2(..) -> then(e)
    Barrier2(finally_ast: Some(fin), in_finally_body: False) -> {
      // INLINE the finally body, THEN continue the walk. F itself runs under an
      // in_finally_body barrier so a transfer inside F wins (§14.15.3).
      let e2 = state.push_frame(e, Barrier2(None, in_finally_body: True))
      use #(f_ir, e3) <- result.try(stmt.emit_stmts(e2, fin))
      let e4 = state.pop_frame(e3)
      use #(rest_ir, e5) <- result.try(then(e4))
      Ok(#(anf.seq_discard(f_ir, rest_ir), e5))
    }
    Barrier2(finally_ast: None, ..) | Barrier2(_, in_finally_body: True) -> then(e)
  }
}
```

`emit_break`/`emit_continue`/`emit_return` are folds over `e.frame_stack` calling `cross_barrier` for non-target frames and terminating at the target — direct port of `emit_goto_loop_walk` (arc emit.gleam:2510-2528) and `list.fold(e.frame_stack, e, emit_return_cross_frame)` (arc emit.gleam:3871).

### 3.6 `emit_try_catch_finally` — composition

```
= emit_try_finally(block: <a synthetic try/catch>, finalizer: F)
where the synthetic body is emit_try_catch(block, param, handler) with the SAME
Barrier2(Some(F)) pushed OUTSIDE it — i.e. two nested barriers, matching arc's two
stacked IrPushTry (emit.gleam:2380-2383). Concretely:

push Barrier2(Some(F), False)                    // outer: finally
  push Barrier2(None, False)                     // inner: catch (no finally to inline)
    B_ir = emit_stmts(block)
  pop
  H_ir = <catch handler with Barrier2(Some(F)) still on stack — so return/break in H
          also inlines F>
  inner_try = ir.Try([TTerm], B_ir, [CatchHandler(OnTag("js_exn"), [x], None, H_ir)])
pop
F_normal = emit_stmts_under_finally_barrier(F)
normal = anf.seq(inner_try, F_normal, <undefined>)
ex2 = fresh
F_throw = emit_stmts_under_finally_barrier(F)
outer_handler = anf.seq(F_throw, ir.Throw("js_exn", [Var(ex2)]))
→ ir.Try([TTerm], normal, [CatchHandler(OnTag("js_exn"), [ex2], None, outer_handler)])
```

This mirrors arc's `emit_try_catch_finally` (emit.gleam:2372-2408) with Gosub replaced by inline emission.

### 3.7 Code-size note (accepted tradeoff)

A finally body with N crossing transfers inside its try/catch is emitted **N+2** times (each break/continue/return + normal + throw). Nested try/finally multiplies. This is the BEAM-`after`-free design; acceptable because JS `finally` bodies are typically small. No mitigation in v1.

---

## 4. Invariants upheld

1. **State never lost across a throw** — `{wasm_exn, 0, [St, E]}` carries the live `InstanceState`; catch destructures it and continues under `Threading(st_caught)`. Holds for both compiler `throw` and rt_js runtime raises.
2. **Non-JS errors propagate untouched** — `match_tag` fails, `try_dispatch` re-raises with original class+stack (emit_core.gleam:5200-5211, twocore_rt_exn_ffi.erl:74).
3. **`finally` runs on every completion** — normal, throw, break, continue, return — via duplication at each site.
4. **`finally`'s own abrupt completion wins** (§14.15.3) — F is emitted under `Barrier2(None, in_finally_body: True)`; a transfer inside F does not re-inline F and structurally precedes the pending outer transfer/rethrow in the ANF chain, so the outer never executes.
5. **BEAM validator `ambiguous_catch_try_state` avoided** — body stays hoisted into a nullary local fun (emit_core.gleam:5151-5156, unchanged).
6. **One tag** — `"js_exn"`, index 0, declared once on `ir.Module.tags` by M19.

---

## 5. Dependencies

- **M17 depends on:** M10 (Emitter2/Frame2/frame_stack/push_frame/pop_frame/fresh), M11 (anf.bind/seq/seq_discard), M13 (stmt.emit_stmts — recursive; break the cycle by having M13 call into M17 for `emit_break`/`emit_continue`/`emit_return` and M17 call `stmt.emit_stmts` via a callback field on Emitter2, OR co-locate the frame-walk in M10/state.gleam), M15 (destructure for catch param).
- **M17 is depended on by:** M13 (TryStatement/ThrowStatement/Break/Continue/Return dispatch), M19 (adds `TagDecl("js_exn", [TTerm])` to module).
- **emit_core.gleam patch depends on:** nothing new (reuses `emit_value_state_pair`, `materialize`, `seam_call`, `resolve_tag`, `fresh_var`).
- **rt_js `t_throw` depends on:** M1 (InstanceState type), M4 (`t_new_error` for TypeError construction).

---

## 6. File manifest

| File | New/Patch | Contents |
|---|---|---|
| `arc/src/arc/emit_2core/exn.gleam` | NEW | §3.2-3.6 functions; `js_exn_tag` const |
| `arc/src/arc/emit_2core/state.gleam` | PATCH (M10) | `Frame2` type, `frame_stack: List(Frame2)`, `push_frame`/`pop_frame` |
| `2core/src/twocore/backend/emit_core.gleam` | PATCH | §2.1 `emit_throw`+sc, §2.3 `emit_catch_clause` Threading arm, §2.5 `emit_call_host` JsOpKind, §2.6 `is_stateful_node`, line 1147 dispatch, delete line 5128 comment, add `ThreadedOnAllUnsupported` to `EmitError` |
| `2core/src/twocore_rt_js_store_ffi.erl` | NEW (M1) | `t_throw/2` |
| `2core/src/twocore/runtime/rt_js_store.gleam` | NEW (M1) | `t_throw` extern facade |

---

## 7. Source references

- 2core IR: `../2core/src/twocore/ir.gleam` — TagDecl:126, Throw:848, Try:860, CatchHandler:992, OnTag/OnAll:1007-1008, Return:698, Break:694, TTerm:322
- rt_exn wire: `../2core/src/twocore_rt_exn_ffi.erl` — throw_exn:42, match_tag:50-51, reraise:74
- rt_exn gleam: `../2core/src/twocore/runtime/rt_exn.gleam` — is_wasm_exn:114, reraise:122
- emit_core Try: `../2core/src/twocore/backend/emit_core.gleam` — dispatch:1147-1150, emit_throw:5097-5106, emit_try:5129-5181, try_dispatch:5192-5217, emit_catch_clause:5225-5280, StateChan:277-280, materialize:2128-2179, apply_cont:1998-2026, emit_value_state_pair:1919-1940, emit_call_host:3536-3596, resolve_js:3621-3656, resolve_tag:716-721, exn_module const:155, is_stateful_node:849-920
- CTry: `../2core/src/twocore/backend/core_erlang.gleam:183-190`
- arc AST: `src/arc/parser/ast.gleam` — ThrowStatement:238, TryStatement:259, CatchClause:300, TryTail:307-309
- arc barrier port source: `src/arc/compiler/emit.gleam` — Frame:125-160, push_barrier:2296-2305, emit_gosub_normal:2336-2341, emit_finally_subroutine:2348-2365, emit_try_catch_finally:2372-2408, frame_target:2413-2454, emit_cross_frame:2459-2474, emit_return_cross_frame:2482-2500, emit_goto_loop_walk:2510-2528, ReturnStatement:3860-3873, TryStatement:3876-3945, emit_catch_clause:3010-3027
- current rt_js throw (to REPLACE): `../2core/src/twocore_rt_js_ffi.erl:75` `type_error/1`
- InstanceState: `../2core/src/twocore/runtime/rt_state.gleam` (via rt_mem_nif.gleam:61 import)