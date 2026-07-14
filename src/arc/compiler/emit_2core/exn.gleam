//// M17: JS throw + try/catch/finally → structured ir.Try + barrier-
//// duplication. Faithful port of arc/compiler/emit.gleam:2287-2510, 3875-3945
//// into the Result-aware Rk-CPS style. §14.15.3 finally-overrides-completion
//// realized STRUCTURALLY: inlined finally runs under Barrier2(None, None)
//// (whose cross_cleanups → CatchOnly no-op) so a transfer inside F supersedes
//// the pending one downstream. R2: js_exn tag. D2: NO St threading.

import arc/compiler/ast_util
import arc/compiler/emit_2core/state.{type EmitError, type Emitter2}
import arc/compiler/scope.{
  type Binding, CaptureBinding, CatchBinding, ConstBinding, FnNameBinding,
  LetBinding, ParamBinding, VarBinding,
}
import arc/parser/ast
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import twocore/ir

/// R2: the single JS-exception ir.Throw/ir.Try tag.
pub const js_exn_tag = "js_exn"

// ── Result-aware CPS (Rk chain) — mirrors func.gleam:28-145 verbatim ────────
// RULING: Rk returns bare Result(ir.Expr,_), NOT an Sk #(Expr,E2) pair (R13
// rejects the pair-return Build variant). Transfers (Throw) never call `next`;
// run_rk pre-seeds its pdict cell with the entry e (func.gleam:138) so a
// diverged chain still yields a valid final Emitter2. R12 Result channel;
// host_ is a sanctioned CallHost("js",..) site alongside anf.host.

type Rk(a) =
  fn(Emitter2, a) -> Result(ir.Expr, EmitError)

fn let_(
  e: Emitter2,
  rhs: ir.Expr,
  k: Rk(ir.Value),
) -> Result(ir.Expr, EmitError) {
  let #(n, e) = state.fresh_var(e)
  use body <- result.map(k(e, ir.Var(n)))
  ir.Let([n], rhs, body)
}

fn host_(
  e: Emitter2,
  op: String,
  args: List(ir.Value),
  k: Rk(ir.Value),
) -> Result(ir.Expr, EmitError) {
  let_(e, ir.CallHost("js", op, args), k)
}

fn host_unit_(
  e: Emitter2,
  op: String,
  args: List(ir.Value),
  k: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  use e, _ <- host_(e, op, args)
  k(e)
}

/// Right-fold `step` over `items`, threading e, building nested Lets.
/// `then` is labelled so `use e, x, next <- each_(…, then: k)` reads well.
fn each_(
  e: Emitter2,
  items: List(a),
  then k: fn(Emitter2) -> Result(ir.Expr, EmitError),
  with step: fn(Emitter2, a, fn(Emitter2) -> Result(ir.Expr, EmitError)) ->
    Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  case items {
    [] -> k(e)
    [x, ..rest] -> step(e, x, fn(e) { each_(e, rest, k, step) })
  }
}

/// Result-aware bind_if. Arm shape = dispatch return shape (#(Expr, E2)) so
/// `e.dispatch.emit_expr` is a valid arm directly; pure arm = `pure_arm(v)`.
fn if_(
  e: Emitter2,
  cond: ir.Value,
  t: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
  f: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
  k: Rk(ir.Value),
) -> Result(ir.Expr, EmitError) {
  use #(tt, e) <- result.try(t(e))
  use #(ft, e) <- result.try(f(e))
  let_(e, ir.If(cond, [ir.TTerm], tt, ft), k)
}

fn pure_arm(
  v: ir.Value,
) -> fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError) {
  fn(e) { Ok(#(ir.Values([v]), e)) }
}

// pdict seam (mirrors func.gleam:115-145 / anf.gleam:68-94) to recover the
// leaf Emitter2 from an Rk chain — the chain returns Result(ir.Expr, _), the
// final e is captured by `done` inside the leaf closure. Re-entrant (fresh
// make_ref per call).

type Ref

type Erased

@external(erlang, "erlang", "make_ref")
fn make_ref() -> Ref

@external(erlang, "erlang", "put")
fn pdict_put(k: Ref, v: a) -> Erased

@external(erlang, "erlang", "erase")
fn pdict_erase(k: Ref) -> a

fn run_rk(
  e: Emitter2,
  f: fn(Emitter2, fn(Emitter2, ir.Expr) -> Result(ir.Expr, EmitError)) ->
    Result(ir.Expr, EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let cell = make_ref()
  let _ = pdict_put(cell, e)
  let done = fn(ef, tree) {
    let _ = pdict_put(cell, ef)
    Ok(tree)
  }
  use tree <- result.map(f(e, done))
  #(tree, pdict_erase(cell))
}

// ── throw + finally-inline (port emit.gleam:2287-2310, 3875-3945) ───────────

/// `throw arg;` — evaluate arg, raise under the R2 js_exn tag. 1 IR arg;
/// emit_core (M9) prepends St. Transfers never call `next`.
pub fn emit_throw_stmt(
  e: Emitter2,
  arg: ast.Expression,
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  use e, done <- run_rk(e)
  use #(tree, e) <- result.try(e.dispatch.emit_expr(e, arg))
  use e, v <- let_(e, tree)
  done(e, ir.Throw(js_exn_tag, [v]))
}

/// Re-emit a finally block inline at a barrier crossing, then continue via
/// `then` (the pending transfer or next cleanup). Port of M17.md §3.5
/// cross_barrier Barrier2(Some) arm. §14.15.3 override: F runs under
/// Barrier2(None, None) (cross_cleanups → CatchOnly no-op) so a transfer
/// INSIDE F does not re-inline F, and — being upstream in the ANF chain —
/// naturally supersedes the pending `then` transfer downstream.
pub fn inline_finally(
  e: Emitter2,
  body: List(ast.StmtWithLine),
  saved: state.ScopeSave2,
  then: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  // RULING slot-vars-scope-survival (stmt.gleam:116-128): teleport ONLY the
  // scope-cursor triple, NOT slot_vars. F must read the break-site SSA names
  // (try-body writes visible in finally), and F's own writes must flow into
  // `then(e)`'s carried_values — state.leave_scope would clobber both.
  let here = snapshot_scope(e)
  // Port emit.gleam:2357-2388 pop-before-subroutine: drop everything up to and
  // INCLUDING the Barrier2(Some(F)) being inlined so a transfer INSIDE F does
  // not re-cross it (would infinite-recurse on `try{break}finally{break}`).
  // frame_stack does NOT escape a diverged transfer (run_rk pre-seeds its
  // pdict cell with the entry e), so the trim is local to this cleanup chain.
  let e =
    state.Emitter2(
      ..e,
      cur_scope: saved.cur_scope,
      scope_cursor: saved.scope_cursor,
      in_block: saved.in_block,
      frame_stack: drop_through_finally_barrier(e.frame_stack),
    )
  let e = state.push_barrier(e, None, None)
  use #(f_tree, e) <- result.try(
    e.dispatch.emit_stmts(e, body, fn(ef) { ir.Values([ef.consts.undef]) }),
  )
  let e = state.pop_frame(e)
  let e =
    state.Emitter2(
      ..e,
      cur_scope: here.cur_scope,
      scope_cursor: here.scope_cursor,
      in_block: here.in_block,
    )
  use e, _ <- let_(e, f_tree)
  then(e)
}

/// Drop frames up to and including the innermost `Barrier2(Some(_))` — the
/// barrier this inline_finally call is realising. Cleanups are innermost-first
/// (state.find_*_target), so the first Some on the stack is always the one.
fn drop_through_finally_barrier(
  frames: List(state.Frame2),
) -> List(state.Frame2) {
  case frames {
    [] -> []
    [state.Barrier2(finally_body: Some(_), ..), ..rest] -> rest
    [_, ..rest] -> drop_through_finally_barrier(rest)
  }
}

// ── try-finally / try-catch-finally scaffold (M17.md §3.4/§3.6) ─────────────

/// Snapshot the scope-cursor position (state.gleam:93) — carried on Barrier2
/// so `inline_finally` at each crossing can restore before re-emitting F.
fn snapshot_scope(e: Emitter2) -> state.ScopeSave2 {
  state.ScopeSave2(
    cur_scope: e.cur_scope,
    scope_cursor: e.scope_cursor,
    slot_vars: e.slot_vars,
    in_block: e.in_block,
  )
}

/// Wrap a bare stmt-list as `[{ …body }]` so `dispatch.emit_stmts` routes it
/// through stmt.emit_block (scope entry + prologue + fn-decl hoist) — the
/// faithful equivalent of arc's `emit_block(_, body, tail: False)`.
fn as_block(body: List(ast.StmtWithLine)) -> List(ast.StmtWithLine) {
  [ast.StmtWithLine(0, ast.BlockStatement(body))]
}

/// Emit `finalizer` as its own block, under `Barrier2(None, None)` — the
/// in-finally-body marker: cross_cleanups → CatchOnly no-op so a transfer
/// INSIDE F does not re-inline F, and structurally precedes the pending
/// rethrow/tail downstream in the ANF chain (§14.15.3 override).
fn emit_finalizer(
  e: Emitter2,
  finalizer: List(ast.StmtWithLine),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let e = state.push_barrier(e, None, None)
  use #(tree, e) <- result.map(
    e.dispatch.emit_stmts(e, as_block(finalizer), fn(_) { ir.Values([]) }),
  )
  #(tree, state.pop_frame(e))
}

/// `try { B } finally { F }` — port of M17.md §3.4 + emit.gleam:3901-3923.
/// Shape: push Barrier2(Some(#(F, saved))); emit B; pop; wrap in ir.Try with
/// a rethrow-after-finally handler; normal-path F after; then k. Any
/// break/continue/return inside B that CROSSES the barrier gets F inlined
/// before the transfer (stmt.inline_cleanup → exn.inline_finally).
pub fn emit_try_finally(
  e: Emitter2,
  block: List(ast.StmtWithLine),
  finalizer: List(ast.StmtWithLine),
  k: state.K,
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  use e <- wrap_with_finally(e, finalizer, block_scope_count(block), k)
  // B runs directly under the outer Barrier2(Some(F)) — one barrier,
  // matching emit.gleam:3907-3909's single IrPushTry(Finally).
  e.dispatch.emit_stmts(e, as_block(block), fn(_) { ir.Values([]) })
}

/// `try { B } catch(p?) { H } finally { F }` — port of M17.md §3.6 +
/// emit.gleam:2372-2408. Two nested barriers: outer carries F (via
/// wrap_with_finally); inner is catch-only. B runs under both, H under the
/// outer only — so a break/return in H also inlines F before transferring.
pub fn emit_try_catch_finally(
  e: Emitter2,
  block: List(ast.StmtWithLine),
  handler: ast.CatchClause,
  finalizer: List(ast.StmtWithLine),
  k: state.K,
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let ast.CatchClause(param:, body: catch_body) = handler
  let scopes_before_fin =
    block_scope_count(block) + catch_scope_count(param, catch_body)
  use e <- wrap_with_finally(e, finalizer, scopes_before_fin, k)
  // Inner catch-only barrier around B (port emit.gleam:2385-2389).
  let e = state.push_barrier(e, None, None)
  use #(body_ir, e) <- result.try(
    e.dispatch.emit_stmts(e, as_block(block), fn(_) { ir.Values([]) }),
  )
  let e = state.pop_frame(e)
  // Catch handler H — outer Barrier2(Some(F)) still on frame_stack.
  let #(ex, e) = state.fresh_var(e)
  use #(h_ir, e) <- result.map(emit_catch_arm(e, param, catch_body, ex))
  let inner =
    ir.Try(result: [], body: body_ir, handlers: [
      ir.CatchHandler(
        on: ir.OnTag(js_exn_tag),
        payload: [ex],
        exnref: None,
        handler: h_ir,
      ),
    ])
  #(inner, e)
}

/// scope_cursor entries `emit_stmts(as_block(body))` will consume: 1 iff the
/// analyzer pushed a Block scope for it (kept in lockstep by sb_block_prunable).
fn block_scope_count(body: List(ast.StmtWithLine)) -> Int {
  case ast_util.block_has_declarations(body) {
    True -> 1
    False -> 0
  }
}

/// scope_cursor entries `emit_catch_arm` consumes from the OUTER cursor:
/// param=Some enters the (never-pruned) Catch scope — its body block is a
/// child of that, not the outer cursor. param=None has no Catch scope.
fn catch_scope_count(
  param: Option(ast.Pattern),
  catch_body: List(ast.StmtWithLine),
) -> Int {
  case param {
    Some(_) -> 1
    None -> block_scope_count(catch_body)
  }
}

/// Shared scaffold: push outer `Barrier2(Some(#(as_block(F), fin_save)))`,
/// emit the protected region via `build`, pop, then `ir.Try(protected,
/// [catch ex → F; rethrow]) ; F_normal ; k`. F is emitted twice (throw +
/// normal path); scope_cursor is snapshot/restored so both re-emits consume
/// the SAME finally-block scope id. Port emit.gleam:2372-2408, Gosub → inline.
/// `scopes_before_fin` = scope_cursor entries `build` consumes, so fin_save
/// (stored on the barrier) positions inline_finally at F_scope, not B_scope.
fn wrap_with_finally(
  e: Emitter2,
  finalizer: List(ast.StmtWithLine),
  scopes_before_fin: Int,
  k: state.K,
  protected build: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  // fin_save: scope position at F-entry (AFTER B/catch scopes) — the barrier
  // stores THIS, not try-entry, so inline_finally's as_block(F) enters F_scope
  // instead of stealing B_scope from the cursor. slot_vars unused there.
  let entry_save = snapshot_scope(e)
  let fin_save =
    state.ScopeSave2(
      ..entry_save,
      scope_cursor: list.drop(e.scope_cursor, scopes_before_fin),
    )
  let e = state.push_barrier(e, Some(#(as_block(finalizer), fin_save)), None)
  use #(protected_ir, e) <- result.try(build(e))
  let e = state.pop_frame(e)
  // Post-protected position: F_block is next in scope_cursor. Snapshot so
  // the second (normal-path) re-emit re-consumes the same scope id.
  let fin_pos = snapshot_scope(e)
  // Throw path: catch ex → F ; rethrow ex.
  let #(ex, e) = state.fresh_var(e)
  use #(f_throw, e) <- result.try(emit_finalizer(e, finalizer))
  let throw_handler = ir.Let([], f_throw, ir.Throw(js_exn_tag, [ir.Var(ex)]))
  // Restore cursor for normal-path re-emit.
  let e = state.leave_scope(e, fin_pos)
  use #(f_normal, e) <- result.try(emit_finalizer(e, finalizer))
  // ir.Try wraps ONLY the protected region (F_normal outside — a throw from
  // F_normal must NOT re-run F). Let([], _, _) is 0-arity sequencing.
  let region =
    ir.Try(result: [], body: protected_ir, handlers: [
      ir.CatchHandler(
        on: ir.OnTag(js_exn_tag),
        payload: [ex],
        exnref: None,
        handler: throw_handler,
      ),
    ])
  // KNOWN GAP (M19 wiring): unboxed-slot rebinds inside B/H/F are Let-bound
  // INSIDE region/f_normal so their fresh names are out of IR scope in k(e)
  // and stmt.gleam:1606's `next(e)`. Carried threading needs stmt.gleam-side
  // assigned_unboxed_slots + rebind_after_block; until then, restore
  // entry slot_vars so downstream reads reference in-scope (pre-try) names.
  let e = state.Emitter2(..e, slot_vars: entry_save.slot_vars)
  Ok(#(ir.Let([], region, ir.Let([], f_normal, k(e))), e))
}

/// Catch handler body — port of emit.gleam:3010-3027 / stmt.gleam:1602-1632.
/// `catch (p) Block`: enter Catch scope (holds the param), seed it, bind `p`
/// from the payload var, emit body block, leave. `catch Block` (no binding):
/// NO catch scope — emit body directly (entering here would steal the next
/// sibling's cursor id, emit.gleam:3006-3009).
fn emit_catch_arm(
  e: Emitter2,
  param: Option(ast.Pattern),
  catch_body: List(ast.StmtWithLine),
  ex_name: String,
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  use e, done <- run_rk(e)
  case param {
    Some(p) -> {
      let #(e, save) = state.enter_scope(e, in_block: e.in_block)
      use e <- catch_binding_prologue(e, e.cur_scope)
      use #(dtree, e) <- result.try(e.dispatch.emit_destructure(
        e,
        p,
        ir.Var(ex_name),
        state.BindLet,
      ))
      use e, _ <- let_(e, dtree)
      use #(body_ir, e) <- result.try(
        e.dispatch.emit_stmts(e, as_block(catch_body), fn(_) { ir.Values([]) }),
      )
      done(state.leave_scope(e, save), body_ir)
    }
    None -> {
      use #(body_ir, e) <- result.try(
        e.dispatch.emit_stmts(e, as_block(catch_body), fn(_) { ir.Values([]) }),
      )
      done(e, body_ir)
    }
  }
}

/// Seed the just-entered Catch scope's bindings — private copy of
/// stmt.gleam:415-448 binding_prologue. `catch(x)` → CatchBinding → undef-
/// seeded (cell_new when captured); destructured `catch({a,b})` names are
/// LetBinding → tdz-seeded so emit_destructure's stores land on live slots.
fn catch_binding_prologue(
  e: Emitter2,
  scope_id: scope.ScopeId,
  k: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  let bindings =
    dict.to_list(scope.get_scope(e.tree, scope_id).bindings)
    |> list.sort(fn(a, b) { int.compare({ a.1 }.slot, { b.1 }.slot) })
  use e, entry, next <- each_(e, bindings, then: k)
  let #(_, b): #(String, Binding) = entry
  let name = state.slot_var_name(b.slot)
  let seed = fn(e: Emitter2, init) {
    case b.is_boxed {
      False -> {
        use body <- result.map(next(state.set_slot_var(e, b.slot, name)))
        ir.Let([name], ir.Values([init]), body)
      }
      True -> {
        use e, cell <- host_(e, "cell_new", [init])
        use body <- result.map(next(state.set_slot_var(e, b.slot, name)))
        ir.Let([name], ir.Values([cell]), body)
      }
    }
  }
  case b.kind {
    VarBinding -> seed(e, e.consts.undef)
    LetBinding | ConstBinding | FnNameBinding -> seed(e, e.consts.tdz)
    // CatchBinding IS the Catch scope's own binding — must seed (cell_new when
    // boxed) so emit_destructure's boxed IdentifierPattern arm has a live cell
    // to cell_set into. Param/Capture never appear in a Catch scope.
    CatchBinding -> seed(e, e.consts.undef)
    ParamBinding | CaptureBinding -> next(e)
  }
}
