//// M13: JS Statement → structured twocore IR lowering. Faithful port of
//// arc/compiler/emit.gleam:1100-2780, 3685-4130, 5080-6030 into the
//// Result-aware Rk-CPS style targeting ir.Block/Loop/LoopParam/Break/
//// Continue/If/Try. D2 (Arch-A): NO St threading — emit_core owns that.
//// R12: user-reachable failures surface as Error(EmitError), never panic.

import arc/compiler/ast_util
import arc/compiler/emit_2core/anf
import arc/compiler/emit_2core/exn
import arc/compiler/emit_2core/state.{
  type BarrierCleanup, type EmitError, type Emitter2, FnDecl, StmtBody,
}
import arc/compiler/scope.{
  type Binding, type ScopeId, CaptureBinding, CatchBinding, ConstBinding,
  FnNameBinding, LetBinding, ParamBinding, VarBinding,
}
import arc/parser/ast
import gleam/bit_array
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set
import twocore/ir

// ── Result-aware CPS (Rk chain) — mirrors func.gleam:28-145 verbatim ────────
// RULING: Rk returns bare Result(ir.Expr,_), NOT an Sk #(Expr,E2) pair (R13
// rejects the pair-return Build variant). Transfers (Break/Continue/Return/
// Throw) never call `next`; run_rk pre-seeds its pdict cell with the entry e
// (func.gleam:138) so a diverged chain still yields a valid final Emitter2.
// R12 Result channel; host_ is a sanctioned CallHost("js",..) site.

type Rk(a) =
  fn(Emitter2, a) -> Result(ir.Expr, EmitError)

/// Tail Value of a straight `Let…Values([v])` chain (the shape `emit_expr`
/// returns for identifiers/literals). None for If/Block/multi-Values tails.
fn let_tail_value(rhs: ir.Expr) -> Option(ir.Value) {
  case rhs {
    ir.Values([v]) -> Some(v)
    ir.Let(_, _, body) -> let_tail_value(body)
    _ -> None
  }
}

fn let_(
  e: Emitter2,
  rhs: ir.Expr,
  k: Rk(ir.Value),
) -> Result(ir.Expr, EmitError) {
  // SPEC§9.12 letrec-fusion: an anf.run tree binds fresh names (write_slot's
  // unboxed rebind, bind_if's join var) that e.slot_vars leaks to k. Wrapping
  // the whole tree as one Let-RHS scopes those names to the RHS, so Baseline's
  // propagate/dead-let erases them before emit_core CPS-linearises. Splice the
  // tree's Let-spine into the outer Rk chain instead — every top-level Let in
  // rhs becomes an outer Let, so its name is in ir-scope for k(e, ·).
  case rhs {
    ir.Let(names, inner_rhs, inner_body) -> {
      use tail <- result.map(let_(e, inner_body, k))
      ir.Let(names, inner_rhs, tail)
    }
    ir.Values([v]) -> k(e, v)
    _ -> {
      let #(n, e) = state.fresh_var(e)
      // Propagate known-number/known-handle through the alias so anf's marks
      // survive the expr_→let_ re-bind into the Rk chain.
      let e = case let_tail_value(rhs) {
        Some(ir.Var(vn)) -> {
          let e = case state.is_known_number(e, vn) {
            True -> state.mark_known_number(e, n)
            False -> e
          }
          case state.is_known_handle(e, vn) {
            True -> state.mark_known_handle(e, n)
            False -> e
          }
        }
        _ -> e
      }
      use body <- result.map(k(e, ir.Var(n)))
      ir.Let([n], rhs, body)
    }
  }
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

// ── carried-value plumbing (shared with loop/switch/if arms) ────────────────

/// Carried slot list for the frame whose ir_break OR ir_continue label is
/// `ir_label`. state.find_break_target/find_continue_target return only
/// #(label, cleanups); the Break/Continue payload needs the slot set too.
fn find_frame_carried(e: Emitter2, ir_label: String) -> List(Int) {
  frame_carried_walk(e.frame_stack, ir_label)
}

fn frame_carried_walk(
  frames: List(state.Frame2),
  ir_label: String,
) -> List(Int) {
  case frames {
    [] -> []
    [frame, ..rest] ->
      case frame {
        state.Loop2(ir_break:, ir_continue:, carried:, ..)
          if ir_break == ir_label || ir_continue == ir_label
        -> carried
        state.Switch2(ir_break:, carried:, ..) if ir_break == ir_label ->
          carried
        state.Labeled2(ir_break:, carried:, ..) if ir_break == ir_label ->
          carried
        _ -> frame_carried_walk(rest, ir_label)
      }
  }
}

/// Current IR value for each carried slot — the ir.Break/ir.Continue payload,
/// and the fall-through Values(..) tail of a Block/If/Try arm. Reads
/// e.slot_vars, so MUST be called on the INNER emitter (before leave_scope) —
/// see RULING on leave_scope_carrying / rebind_after_block.
fn carried_values(e: Emitter2, slots: List(Int)) -> List(ir.Value) {
  list.map(slots, fn(slot) { ir.Var(state.get_slot_var(e, slot)) })
}

/// RULING slot-vars-scope-survival — state.leave_scope (state.gleam:648)
/// restores slot_vars to the pre-enter snapshot, so a set_slot_var of an OUTER
/// slot done inside the scope is dropped. Mitigation (option a, chosen for all
/// of block/if/while/for/switch/try):
///  • INLINE bodies (plain `{ }`, catch, for-head) call this instead of
///    state.leave_scope: capture each carried slot's inner SSA name, restore
///    the snapshot, then re-apply — the inner ir.Let names remain in IR-
///    lexical scope across an inline leave, so re-pointing slot_vars suffices.
///  • WRAPPED bodies (ir.Block/If/Loop/Try) end each arm in
///    Values(carried_values(inner_e)) BEFORE leave_scope, then call
///    rebind_after_block on the wrapper — the wrapper's result vars are the
///    explicit bridge, since inner ir.Let names go out of IR scope at the
///    wrapper boundary.
fn leave_scope_carrying(
  e_inner: Emitter2,
  save: state.ScopeSave2,
  carried: List(Int),
) -> Emitter2 {
  let inner_names = list.map(carried, state.get_slot_var(e_inner, _))
  let e = state.leave_scope(e_inner, save)
  use e, #(slot, name) <- list.fold(list.zip(carried, inner_names), e)
  state.set_slot_var(e, slot, name)
}

// ── inline_cleanup + transfer arms (port emit.gleam:2504-2530, 3860-3868) ───

fn bool_atom(e: Emitter2, b: Bool) -> ir.Value {
  case b {
    True -> e.consts.true_
    False -> e.consts.false_
  }
}

/// Emit one crossed-barrier cleanup inline before the transfer, then continue
/// via `k`. Structured-IR replacement for emit.gleam's Gosub/PopTry sequence.
fn inline_cleanup(
  e: Emitter2,
  cleanup: BarrierCleanup,
  k: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  case cleanup {
    state.FinallyBlock(body:, saved_scope:) ->
      exn.inline_finally(e, body, saved_scope, k)
    state.IterClose(iter_var:, is_async:) ->
      host_unit_(e, "iter_close", [ir.Var(iter_var), bool_atom(e, is_async)], k)
    // Structured ir.Try makes a bare catch transparent — nothing to emit.
    state.CatchOnly -> k(e)
  }
}

/// Fold `cleanups` (innermost first, per state.find_*_target) through
/// inline_cleanup, terminating in `k`.
fn inline_cleanups(
  e: Emitter2,
  cleanups: List(BarrierCleanup),
  k: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  case cleanups {
    [] -> k(e)
    [c, ..rest] -> inline_cleanup(e, c, fn(e) { inline_cleanups(e, rest, k) })
  }
}

/// Cleanups a `return` performs — it crosses EVERY frame on the way out
/// (port emit.gleam:2482-2498 emit_return_cross_frame, data-returning).
/// state.cross_cleanups is private, so re-derive from Frame2 shapes here.
fn return_cleanups(frames: List(state.Frame2)) -> List(BarrierCleanup) {
  use frame <- list.flat_map(frames)
  case frame {
    state.Loop2(iter_close: Some(iv), ..) -> [state.IterClose(iv, False)]
    state.Loop2(..) | state.Switch2(..) | state.Labeled2(..) -> []
    state.Barrier2(finally_body:, iter_close:) -> {
      let acc = case finally_body {
        Some(#(body, save)) -> [state.FinallyBlock(body, save)]
        None -> []
      }
      case iter_close {
        Some(iv) -> [state.IterClose(iv, False), ..acc]
        None ->
          case acc {
            [] -> [state.CatchOnly]
            _ -> acc
          }
      }
    }
  }
}

/// `break [label]`. Port of emit.gleam:2504-2530 emit_goto_loop (is_cont=False)
/// → structured ir.Break carrying the target frame's LoopParam values.
/// Transfer is leaf — `next` is not called.
fn emit_break(
  e: Emitter2,
  label: Option(String),
  _next: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  use #(ir_label, cleanups) <- result.try(state.find_break_target(e, label))
  use e <- inline_cleanups(e, cleanups)
  // Diverges (never calls `next`): thread module-monotone state (fns_acc
  // grown by a preceding aux_fn/FnExpr) back to the enclosing run_rk.
  rk_checkpoint(e)
  case sm_goto(e, ir_label) {
    Some(r) -> r
    None -> {
      let carried = find_frame_carried(e, ir_label)
      Ok(ir.Break(ir_label, carried_values(e, carried)))
    }
  }
}

/// `continue [label]`. R15: `ir_continue` names an INNER ir.Block wrapping the
/// loop body, so JS continue lowers to ir.Break — falling out of that Block
/// reaches update/post-test, then the loop emitter ir.Continue's to the head.
fn emit_continue(
  e: Emitter2,
  label: Option(String),
  _next: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  use #(ir_label, cleanups) <- result.try(state.find_continue_target(e, label))
  use e <- inline_cleanups(e, cleanups)
  rk_checkpoint(e)
  case sm_goto(e, ir_label) {
    Some(r) -> r
    None -> {
      let carried = find_frame_carried(e, ir_label)
      Ok(ir.Break(ir_label, carried_values(e, carried)))
    }
  }
}

/// M18 seam: if the resolved ir label is an SM sentinel (split-spanning target
/// pushed by async.gleam), delegate to the installed intercept. None = not an
/// SM label (fragment-local loop) → M13 emits ir.Break as normal.
fn sm_goto(
  e: Emitter2,
  ir_label: String,
) -> Option(Result(ir.Expr, EmitError)) {
  case e.sm_abrupt {
    Some(sm) -> sm.on_goto(e, ir_label)
    None -> None
  }
}

/// `return [arg]`. Port of emit.gleam:3860-3868. Evaluates `arg` (or undef),
/// inlines every crossed cleanup, then ir.Return([v]) — bare 1-value per D2
/// Arch-A (emit_core wraps {V, St'}).
fn emit_return(
  e: Emitter2,
  arg: Option(ast.Expression),
  _next: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  let with_value = fn(e: Emitter2, v: ir.Value) {
    // M18: fragment-local (non-split-spanning) barriers are on frame_stack and
    // inline first; the SM hook then owns split-spanning try routing.
    use e <- inline_cleanups(e, return_cleanups(e.frame_stack))
    // Transfer diverges (never calls `next`): thread module-monotone state
    // (fns_acc from a FunctionExpression arg) back to the enclosing run_rk.
    rk_checkpoint(e)
    case e.sm_abrupt {
      Some(sm) -> sm.on_return(e, v)
      None -> Ok(ir.Return([v]))
    }
  }
  case arg {
    None -> with_value(e, e.consts.undef)
    Some(ex) -> {
      use #(rhs, e) <- result.try(e.dispatch.emit_expr(e, ex))
      let_(e, rhs, with_value)
    }
  }
}

// ── entry points: emit_stmts / emit_stmt dispatch ───────────────────────────

/// Evaluate a JS expression via dispatch, let-bind its result tree, pass the
/// bound ir.Value to `k`. Threads Emitter2 from the dispatch return.
fn expr_(
  e: Emitter2,
  ex: ast.Expression,
  k: Rk(ir.Value),
) -> Result(ir.Expr, EmitError) {
  use #(tree, e) <- result.try(e.dispatch.emit_expr(e, ex))
  let_(e, tree, k)
}

// pdict seam (mirrors func.gleam:115-145 / anf.gleam:68-94) to recover the
// leaf Emitter2 from an Rk chain. Re-entrant (fresh make_ref per call).

type Ref

type Erased

@external(erlang, "erlang", "make_ref")
fn make_ref() -> Ref

@external(erlang, "erlang", "put")
fn pdict_put(k: Ref, v: a) -> Erased

@external(erlang, "erlang", "erase")
fn pdict_erase(k: Ref) -> a

// Transfers (Return/Throw) never call `next`, so run_rk's pre-seeded cell
// would drop fns_acc advanced by a FunctionExpression in the argument. run_rk
// publishes its cell as RkTop; rk_checkpoint writes the leaf e into it.
type RkKey {
  RkTop
}

@external(erlang, "erlang", "put")
fn rk_top_set(k: RkKey, v: Ref) -> Erased

@external(erlang, "erlang", "put")
fn rk_top_restore(k: RkKey, v: Erased) -> Erased

@external(erlang, "erlang", "get")
fn rk_top_get(k: RkKey) -> Ref

fn rk_checkpoint(e: Emitter2) -> Nil {
  let _ = pdict_put(rk_top_get(RkTop), e)
  Nil
}

fn run_rk(
  e: Emitter2,
  f: fn(Emitter2, fn(Emitter2, ir.Expr) -> Result(ir.Expr, EmitError)) ->
    Result(ir.Expr, EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let cell = make_ref()
  let _ = pdict_put(cell, e)
  let prev = rk_top_set(RkTop, cell)
  let done = fn(ef, tree) {
    let _ = pdict_put(cell, ef)
    Ok(tree)
  }
  let r = f(e, done)
  let _ = rk_top_restore(RkTop, prev)
  use tree <- result.map(r)
  #(tree, pdict_erase(cell))
}

/// state.EmitDispatch.emit_stmts adapter (state.gleam:218-219). Folds each
/// statement through emit_stmt; the terminal continuation runs `k` (the
/// caller's tail ir.Expr) and captures the leaf Emitter2 via run_rk.
pub fn emit_stmts(
  e: Emitter2,
  ss: List(ast.StmtWithLine),
  k: state.K,
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  use e, done <- run_rk(e)
  each_(e, ss, then: fn(ef) { done(ef, k(ef)) }, with: fn(e, located, next) {
    emit_stmt(e, located.statement, next)
  })
}

/// Lower one Statement. `k` is the "rest of the block" continuation — trivial
/// arms tail-call it directly; control-flow arms wrap it in structured IR.
fn emit_stmt(
  e: Emitter2,
  s: ast.Statement,
  k: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  case s {
    // ── trivial arms ────────────────────────────────────────────────────────
    ast.EmptyStatement | ast.DebuggerStatement -> k(e)
    ast.ExpressionStatement(expression:, ..) ->
      expr_(e, expression, fn(e, _) { k(e) })
    // D15: `with` is unsupported — surface as compile-time EmitError (R12).
    ast.WithStatement(..) -> Error(state.UnsupportedFeature("with"))
    // Already hoisted by func.emit_prologue — the statement itself is a no-op.
    // Annex-B block-level fn promotion is skipped in v1.
    ast.FunctionDeclaration(..) -> k(e)
    ast.BlockStatement([]) -> k(e)

    // ── transfer (already implemented above) ────────────────────────────────
    ast.BreakStatement(label:) -> emit_break(e, label, k)
    ast.ContinueStatement(label:) -> emit_continue(e, label, k)
    ast.ReturnStatement(argument:) -> emit_return(e, argument, k)

    // ── owned by sibling M13 units ──────────────────────────────────────────
    ast.BlockStatement(body:) -> emit_block(e, body, k)
    ast.VariableDeclaration(kind:, declarations:) ->
      emit_var_decl(e, kind, declarations, k)
    ast.IfStatement(condition:, consequent:, alternate:) ->
      emit_if(e, condition, consequent, alternate, k)
    ast.LabeledStatement(label:, body:) -> emit_labeled(e, label, body, k)
    ast.WhileStatement(condition:, body:) -> emit_while(e, condition, body, k)
    ast.DoWhileStatement(condition:, body:) ->
      emit_do_while(e, condition, body, k)
    ast.ForStatement(init:, condition:, update:, body:) ->
      emit_for_classic(e, init, condition, update, body, k)
    ast.ForInStatement(left:, right:, body:) ->
      emit_for_in(e, left, right, body, k)
    ast.ForOfStatement(left:, right:, body:, is_await:) ->
      case is_await {
        True -> todo as "M18 for-await-of"
        False -> emit_for_of(e, left, right, body, k)
      }
    ast.SwitchStatement(discriminant:, cases:) ->
      emit_switch(e, discriminant, cases, k)
    // ── throw / try / class (port emit.gleam:3870-3942, 4001-4021) ──────────
    ast.ThrowStatement(argument:) ->
      // R2: tag exactly "js_exn", 1 IR arg (emit_core prepends St).
      expr_(e, argument, fn(ef, v) {
        rk_checkpoint(ef)
        Ok(ir.Throw(ef.consts.js_tag, [v]))
      })
    ast.TryStatement(block:, tail:) -> emit_try(e, block, tail, k)
    ast.ClassDeclaration(name:, super_class:, body:) ->
      emit_class_decl(e, name, super_class, body, k)
  }
}

// ── block-scope binding helpers (ports of func.gleam:295-310,349-378,656-684
// re-scoped from fn_scope→cur_scope for BlockDeclarationInstantiation §14.2.3)

/// Store `val` into `b`'s slot: unboxed → Let-bind to the slot's canonical var
/// name + set_slot_var; boxed → cell_set on the existing cell. Verbatim port.
fn store_slot(
  e: Emitter2,
  b: Binding,
  val: ir.Value,
  k: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  case b.is_boxed {
    True ->
      host_unit_(e, "cell_set", [ir.Var(state.get_slot_var(e, b.slot)), val], k)
    False -> {
      let name = state.slot_var_name(b.slot)
      use body <- result.map(k(state.set_slot_var(e, b.slot, name)))
      ir.Let([name], ir.Values([val]), body)
    }
  }
}

/// Seed every binding owned by `scope_id` (the just-entered block scope) in
/// slot order: Var→undef, Let/Const/FnName→tdz, then cell_new if boxed.
/// Param/Catch/Capture never appear in a Block-kind scope's own bindings but
/// are matched to keep the case exhaustive. Port of func.binding_prologue.
fn binding_prologue(
  e: Emitter2,
  scope_id: ScopeId,
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
    ParamBinding | CatchBinding | CaptureBinding -> next(e)
  }
}

/// Compile every direct FunctionDeclaration in `stmts` and store the closure
/// into its name's block-scope slot (BlockDeclarationInstantiation §14.2.3 —
/// resolves against `e.cur_scope`, NOT `e.fn_scope`). Goes through
/// `e.dispatch.emit_function` (R13). Port of func.hoist_fn_decls.
fn hoist_fn_decls(
  e: Emitter2,
  stmts: List(ast.StmtWithLine),
  k: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  use e, located, next <- each_(e, stmts, then: k)
  case ast_util.peel_labels(located.statement) {
    ast.FunctionDeclaration(
      name: Some(ast.NamedBinding(name:, ..)),
      params:,
      body:,
      is_generator:,
      is_async:,
    ) -> {
      let #(child_id, e) = state.pop_child_fn(e)
      use #(ctree, e) <- result.try(e.dispatch.emit_function(
        e,
        FnDecl(is_gen: is_generator, is_async:),
        Some(name),
        params,
        StmtBody(body),
        child_id,
      ))
      use e, fn_h <- let_(e, ctree)
      store_slot(e, cur_scope_binding(e, name), fn_h, next)
    }
    _ -> next(e)
  }
}

fn cur_scope_binding(e: Emitter2, name: String) -> Binding {
  let assert Ok(b) =
    dict.get(scope.get_scope(e.tree, e.cur_scope).bindings, name)
    as "emit_2core/stmt: name missing from block-scope bindings"
  b
}

// ── BlockStatement arm (port emit.gleam:2878-2970) ──────────────────────────

/// Fold a raw statement list through emit_stmt with `next` as the tail —
/// the internal (Rk-typed) counterpart to the pub emit_stmts adapter.
fn fold_body(
  e: Emitter2,
  body: List(ast.StmtWithLine),
  next: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  each_(e, body, then: next, with: fn(e, located, k) {
    emit_stmt(e, located.statement, k)
  })
}

/// `{ ... }`. Port of emit.gleam:2878-2910. Elides the scope entirely when
/// nothing block-scoped is declared; otherwise enter_scope → prologue-seed →
/// hoist fn decls → body → leave_scope.
fn emit_block(
  e: Emitter2,
  body: List(ast.StmtWithLine),
  next: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  case body {
    [] -> next(e)
    _ ->
      case ast_util.block_has_declarations(body) {
        False -> fold_body(e, body, next)
        True -> {
          // RULING slot-vars-scope-survival: compute carried at OUTER scope
          // (before enter_scope) so only already-bound outer slots qualify.
          let carried = assigned_unboxed_slots(e, ast.BlockStatement(body))
          let #(e, save) = state.enter_scope(e, in_block: True)
          use e <- binding_prologue(e, e.cur_scope)
          use e <- hoist_fn_decls(e, body)
          case ast_util.has_using_decl(body) {
            False ->
              fold_body(e, body, fn(ef) {
                next(leave_scope_carrying(ef, save, carried))
              })
            True -> {
              // M17 replaces this with proper try-wrapped DisposeResources.
              let e = state.push_barrier(e, None, None)
              use ef <- fold_body(e, body)
              let ef = state.pop_frame(ef)
              use ef <- host_unit_(ef, "dispose_resources", [])
              next(leave_scope_carrying(ef, save, carried))
            }
          }
        }
      }
  }
}

/// Annex B §B.3.1: bare FunctionDeclaration as an if/else clause behaves as if
/// wrapped in a Block; the normal block path handles the block-scoped binding.
pub fn block_wrap_fn_decl(stmt: ast.Statement) -> ast.Statement {
  case stmt {
    ast.FunctionDeclaration(..) ->
      ast.BlockStatement([ast.StmtWithLine(0, stmt)])
    _ -> stmt
  }
}

// ── VariableDeclaration (port emit.gleam:3727-3776) ─────────────────────────

/// Initialize a declared name to `v`. Port of emit.gleam init_lex/emit_var_put:
/// unboxed → fresh Let-bind + set_slot_var; boxed → cell_set. Lexical bindings
/// mark the slot `initialized` so later writes (expr.emit_direct_put) skip the
/// TDZ check. D15: WithChain / EvalEnv → UnsupportedFeature.
fn store_declared(
  e: Emitter2,
  name: String,
  v: ir.Value,
  lexical: Bool,
  k: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  case state.resolve(e, name) {
    scope.Plain(scope.Local(slot:, boxed:, ..)) -> {
      let e = case lexical {
        True ->
          state.Emitter2(..e, initialized: set.insert(e.initialized, slot))
        False -> e
      }
      case boxed {
        True ->
          host_unit_(e, "cell_set", [ir.Var(state.get_slot_var(e, slot)), v], k)
        False -> {
          let #(n, e) = state.fresh_var(e)
          // Propagate known-number/known-handle through the alias so a
          // for-init `let i=0` / `let o={x:0}` seed carries the mark.
          let e = case v {
            ir.Var(vn) -> {
              let e = case state.is_known_number(e, vn) {
                True -> state.mark_known_number(e, n)
                False -> e
              }
              case state.is_known_handle(e, vn) {
                True -> state.mark_known_handle(e, n)
                False -> e
              }
            }
            _ -> e
          }
          use body <- result.map(k(state.set_slot_var(e, slot, n)))
          ir.Let([n], ir.Values([v]), body)
        }
      }
    }
    scope.Plain(scope.Global(_)) ->
      case dict.get(e.slotted_globals, name) {
        Ok(slot) ->
          host_unit_(
            e,
            "cell_set",
            [ir.Var(state.get_slot_var(e, slot)), v],
            k,
          )
        Error(Nil) ->
          host_unit_(
            e,
            "global_set",
            [ir.ConstBinary(bit_array.from_string(name)), v],
            k,
          )
      }
    scope.Plain(scope.EvalEnv(_)) ->
      Error(state.UnsupportedFeature("direct eval"))
    scope.WithChain(..) -> Error(state.UnsupportedFeature("with"))
  }
}

/// `var/let/const … = …`. Port of emit.gleam:3727-3776. Per declarator:
/// IdentifierPattern + init → eval init (via dispatch — R14 name-hint gap
/// accepted per spec), store; IdentifierPattern no init → var skips (prologue
/// seeded undef), lexical stores undef (§14.3.1.2 ends TDZ); other patterns →
/// dispatch.emit_destructure with the kind's BindMode.
fn emit_var_decl(
  e: Emitter2,
  kind: ast.VariableKind,
  decls: List(ast.VariableDeclarator),
  next: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  let mode = case kind {
    ast.Var -> state.BindVar
    ast.Let -> state.BindLet
    ast.Const | ast.Using | ast.AwaitUsing -> state.BindConst
  }
  let lexical = ast_util.is_lexical(kind)
  use e, decl, next <- each_(e, decls, then: next)
  let ast.VariableDeclarator(id: pat, init:) = decl
  case pat {
    ast.IdentifierPattern(name:, ..) ->
      case init {
        Some(init_expr) -> {
          use e, v <- expr_(e, init_expr)
          store_declared(e, name, v, lexical, next)
        }
        None ->
          case lexical {
            True -> store_declared(e, name, e.consts.undef, lexical, next)
            False -> next(e)
          }
      }
    _ -> {
      let with_rhs = fn(e: Emitter2, v: ir.Value) {
        use #(dtree, e) <- result.try(e.dispatch.emit_destructure(
          e,
          pat,
          v,
          mode,
        ))
        use e, _ <- let_(e, dtree)
        next(e)
      }
      case init {
        Some(init_expr) -> expr_(e, init_expr, with_rhs)
        None -> with_rhs(e, e.consts.undef)
      }
    }
  }
}

// ── carried-set: LoopParam machinery (no emit.gleam analogue) ───────────────
// Imperative bytecode re-read slots in place; ir.Loop must thread every
// mutated unboxed local as a LoopParam so Continue/Break carry the current
// value across iterations. SPEC.md M10:1208.

/// ir.LoopParam for each carried slot: a fresh loop-local name, TTerm-typed,
/// seeded from the slot's current IR var. Paired with enter_loop_body which
/// rebinds the slots to these names inside the body. Names go through
/// fresh_var — ir.gleam:419 requires all binder names unique per function, so
/// two loops carrying the same outer slot must not collide.
fn carried_params(
  e: Emitter2,
  slots: List(Int),
) -> #(List(ir.LoopParam), Emitter2) {
  let #(e, params) = {
    use #(e, acc), slot <- list.fold(slots, #(e, []))
    let init = ir.Var(state.get_slot_var(e, slot))
    let #(name, e) = state.fresh_var(e)
    #(e, [ir.LoopParam(name:, ty: ir.TTerm, init:), ..acc])
  }
  #(list.reverse(params), e)
}

/// ir.Block/If/Try result-type list for a carried slot set — one TTerm per slot.
fn carried_types(slots: List(Int)) -> List(ir.ValType) {
  list.map(slots, fn(_) { ir.TTerm })
}

/// Rebind each carried slot to its LoopParam name so the loop body's reads and
/// writes see the loop-local (Continue then carries the current value forward).
fn enter_loop_body(
  e: Emitter2,
  slots: List(Int),
  params: List(ir.LoopParam),
) -> Emitter2 {
  use e, #(slot, param) <- list.fold(list.zip(slots, params), e)
  state.set_slot_var(e, slot, param.name)
}

/// Bind an n-carried ir.Block/If/Loop/Try result to fresh names and rebind
/// each slot, so downstream reads see the post-construct values. Call AFTER
/// leave_scope + pop_frame (RULING slot-vars-scope-survival: the wrapper's
/// explicit result vars, not e.slot_vars, carry outer-slot mutations out).
/// `slots == []` collapses to `Let([], rhs, k(e))` — sequencing with no rebind.
fn rebind_after_block(
  e: Emitter2,
  slots: List(Int),
  rhs: ir.Expr,
  k: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  let #(e, names) = {
    use #(e, names), slot <- list.fold(slots, #(e, []))
    let #(n, e) = state.fresh_var(e)
    #(state.set_slot_var(e, slot, n), [n, ..names])
  }
  let names = list.reverse(names)
  use body <- result.map(k(e))
  ir.Let(names, rhs, body)
}

/// Unboxed local slots that `s` may re-assign, filtered to slots already bound
/// OUTSIDE (present in `e.slot_vars` at loop-emit time). Deduped, sorted.
/// Nested function/class bodies are NOT descended — their assignments target
/// the child frame, not this one. Slot -1 (`_this_c` — perf5 this-c-hoist) is
/// ALWAYS included when live: any `this.x = …` or user-JS call inside `s`
/// rebinds it, and static detection is unsound (calls can alias `this`).
fn assigned_unboxed_slots(e: Emitter2, s: ast.Statement) -> List(Int) {
  stmt_assigned_names(s, [])
  |> list.unique
  |> list.filter_map(fn(name) {
    case state.resolve(e, name) {
      scope.Plain(scope.Local(slot:, boxed: False, ..)) ->
        case dict.has_key(e.slot_vars, slot) {
          True -> Ok(slot)
          False -> Error(Nil)
        }
      _ -> Error(Nil)
    }
  })
  |> with_this_c_slot(e)
  |> list.unique
  |> list.sort(int.compare)
}

/// Prepend slot -1 (`_this_c`) when set — carried through every stmt-level
/// join so a `this.x = …` / call inside a branch/loop body flows out. Slot
/// -2 (`_this_sid`) is entry-seeded and NEVER rebound after (refresh_this_c
/// leaves it), so it needs no join-carry.
fn with_this_c_slot(slots: List(Int), e: Emitter2) -> List(Int) {
  case dict.has_key(e.slot_vars, -1) {
    True -> [-1, ..slots]
    False -> slots
  }
}

/// Union of assigned_unboxed_slots over a statement list — for switch cases,
/// try blocks, and other multi-body constructs.
fn assigned_unboxed_slots_all(
  e: Emitter2,
  ss: List(ast.Statement),
) -> List(Int) {
  list.flat_map(ss, assigned_unboxed_slots(e, _))
  |> list.unique
  |> list.sort(int.compare)
}

/// Emit `kfn_code(f, undef)` once for each hoistable callee slot, let-bound
/// BEFORE the caller's ir.Block/ir.Loop, and record each pair var in
/// `e.hoisted_kfn` so `expr.emit_plain_call` reuses it inside the loop body.
/// Slotted-global cells (`is_global: True`) read through `cell_get` first and
/// key `hoisted_kfn` by `-1 - slot` so a nested function's local slot N never
/// collides with js_main's slotted-global slot N.
fn hoist_kfn_codes(
  e: Emitter2,
  slots: List(#(Int, Bool)),
  k: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  case slots {
    [] -> k(e)
    [#(slot, is_global), ..rest] -> {
      let sv = ir.Var(state.get_slot_var(e, slot))
      let go = fn(e, f) {
        use e, pair <- host_(e, "kfn_code", [f, e.consts.undef])
        let key = case is_global {
          True -> -1 - slot
          False -> slot
        }
        hoist_kfn_codes(state.set_hoisted_kfn(e, key, pair), rest, k)
      }
      case is_global {
        False -> go(e, sv)
        True -> host_(e, "cell_get", [sv], go)
      }
    }
  }
}

/// Callee slots whose `kfn_code` read is loop-invariant, as `#(slot,
/// is_global)`. Unboxed locals: called as `f(..)` in body/cond/update, NOT
/// re-bound by the loop (∉ `carried`), already bound outside (∈ `e.slot_vars`).
/// Slotted globals (Optimization G): with `module_slot_globals: True` these
/// resolve to `scope.Local(boxed: True)` (root VarBinding at js_main /
/// CaptureBinding inside nested fns) — never `scope.Global`. Hoisted when the
/// name is in `e.slotted_globals`, its cell var is bound outside the loop (∈
/// `e.slot_vars`), and it isn't textually reassigned in body/cond/update.
/// CAVEAT: loop-local `assigned` cannot see reassignment inside CALLED
/// functions (a boxed cell can be `cell_set` by any callee). Full soundness
/// wants a module-wide `∉ expr.stmt_assigned_globals` gate, which needs the
/// set stored on Emitter2 (state.gleam) — deferred; no v8-v7 bench nor current
/// test262 subset reassigns a top-level function it calls in a loop.
pub fn loop_invariant_callees(
  e: Emitter2,
  body: ast.Statement,
  cond: Option(ast.Expression),
  upd: Option(ast.Expression),
  carried: List(Int),
) -> List(#(Int, Bool)) {
  let assigned =
    stmt_assigned_names(body, [])
    |> opt_expr_assigned_names(cond, _)
    |> opt_expr_assigned_names(upd, _)
  stmt_callee_names(body, [])
  |> opt_expr_callee_names(cond, _)
  |> opt_expr_callee_names(upd, _)
  |> list.unique
  |> list.filter_map(fn(name) {
    case state.resolve(e, name) {
      scope.Plain(scope.Local(slot:, boxed: False, ..)) ->
        case !list.contains(carried, slot) && dict.has_key(e.slot_vars, slot) {
          True -> Ok(#(slot, False))
          False -> Error(Nil)
        }
      // Slotted-global boxed cell (root VarBinding or its capture). `slot` is
      // the CURRENT frame's slot — valid for `get_slot_var` in hoist_kfn_codes.
      scope.Plain(scope.Local(slot:, boxed: True, ..)) ->
        case
          dict.has_key(e.slotted_globals, name)
          && dict.has_key(e.slot_vars, slot)
          && !list.contains(assigned, name)
        {
          True -> Ok(#(slot, True))
          False -> Error(Nil)
        }
      _ -> Error(Nil)
    }
  })
  |> list.unique
  |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
}

/// expr.gleam mirrors this const — flip both.
const perf8_arr_c_hoist: Bool = True

/// perf8_arr_c_hoist: unboxed-local slots used as `name[…]` bracket-read bases
/// ≥`min` times in body/cond/update, NOT re-bound by the loop (∉ `carried`),
/// bound outside (∈ `e.slot_vars`), and with NO `name[…] = v` bracket write
/// anywhere in the loop (a write via `set_elem_fast_p` re-`put`s the pdict
/// overlay so a hoisted `arr_c` snapshot would go stale). Read-only bases
/// only — crypto am3's `this_array` (~2.4M reads) qualifies; `w_array`
/// (mixed read+write) is skipped so writeback stays live.
pub fn loop_invariant_arr_bases(
  e: Emitter2,
  body: ast.Statement,
  cond: Option(ast.Expression),
  upd: Option(ast.Expression),
  carried: List(Int),
  min: Int,
) -> List(Int) {
  case perf8_arr_c_hoist {
    False -> []
    True -> {
      let #(reads, writes) =
        stmt_bracket_bases(body, #([], []))
        |> opt_expr_bracket_bases(cond, _)
        |> opt_expr_bracket_bases(upd, _)
      reads
      |> list.filter(fn(name) { !list.contains(writes, name) })
      |> count_names
      |> dict.to_list
      |> list.filter_map(fn(nc) {
        let #(name, n) = nc
        case n >= min, state.resolve(e, name) {
          True, scope.Plain(scope.Local(slot:, boxed: False, ..)) ->
            case
              !list.contains(carried, slot) && dict.has_key(e.slot_vars, slot)
            {
              True -> Ok(slot)
              False -> Error(Nil)
            }
          _, _ -> Error(Nil)
        }
      })
      |> list.unique
      |> list.sort(int.compare)
    }
  }
}

fn count_names(names: List(String)) -> dict.Dict(String, Int) {
  use d, n <- list.fold(names, dict.new())
  dict.upsert(d, n, fn(v) {
    case v {
      Some(c) -> c + 1
      None -> 1
    }
  })
}

/// Emit `arr_c = arr_c_load(obj)` once per hoistable base slot, let-bound
/// BEFORE the caller's ir.Loop, and record each in `e.hoisted_arr_c` keyed by
/// the base's CURRENT slot-var name so `expr.get_elem_fast` looks it up
/// directly from the emitted receiver `ir.Var` (loop-invariant → name stable).
fn hoist_arr_c(
  e: Emitter2,
  slots: List(Int),
  k: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  case slots {
    [] -> k(e)
    [slot, ..rest] -> {
      let obj_name = state.get_slot_var(e, slot)
      use e, arr_c <- host_(e, "arr_c_load", [ir.Var(obj_name)])
      hoist_arr_c(state.set_hoisted_arr_c(e, obj_name, arr_c), rest, k)
    }
  }
}

// -- callee-identifier name collection (accumulator-passing walk) --

fn stmt_callee_names(s: ast.Statement, acc: List(String)) -> List(String) {
  case s {
    ast.EmptyStatement
    | ast.DebuggerStatement
    | ast.BreakStatement(..)
    | ast.ContinueStatement(..)
    | // do NOT descend into nested function bodies
      ast.FunctionDeclaration(..) -> acc
    ast.ClassDeclaration(super_class:, ..) ->
      opt_expr_callee_names(super_class, acc)
    ast.ExpressionStatement(expression:, ..) ->
      expr_callee_names(expression, acc)
    ast.BlockStatement(body:) -> stmts_callee_names(body, acc)
    ast.VariableDeclaration(declarations:, ..) -> {
      use acc, ast.VariableDeclarator(id:, init:) <- list.fold(
        declarations,
        acc,
      )
      pattern_callee_names(id, opt_expr_callee_names(init, acc))
    }
    ast.ReturnStatement(argument:) -> opt_expr_callee_names(argument, acc)
    ast.IfStatement(condition:, consequent:, alternate:) -> {
      let acc = expr_callee_names(condition, acc)
      let acc = stmt_callee_names(consequent, acc)
      case alternate {
        Some(a) -> stmt_callee_names(a, acc)
        None -> acc
      }
    }
    ast.ThrowStatement(argument:) -> expr_callee_names(argument, acc)
    ast.WhileStatement(condition:, body:)
    | ast.DoWhileStatement(condition:, body:) ->
      stmt_callee_names(body, expr_callee_names(condition, acc))
    ast.ForStatement(init:, condition:, update:, body:) -> {
      let acc = case init {
        Some(ast.ForInitExpression(ex)) -> expr_callee_names(ex, acc)
        Some(ast.ForInitDeclaration(declarations:, ..)) -> {
          use acc, d <- list.fold(declarations, acc)
          pattern_callee_names(d.id, opt_expr_callee_names(d.init, acc))
        }
        Some(ast.ForInitPattern(p)) -> pattern_callee_names(p, acc)
        None -> acc
      }
      let acc = opt_expr_callee_names(condition, acc)
      let acc = opt_expr_callee_names(update, acc)
      stmt_callee_names(body, acc)
    }
    ast.ForInStatement(left:, right:, body:)
    | ast.ForOfStatement(left:, right:, body:, ..) -> {
      let acc = case left {
        ast.ForInitExpression(ex) -> expr_callee_names(ex, acc)
        ast.ForInitDeclaration(declarations:, ..) -> {
          use acc, d <- list.fold(declarations, acc)
          pattern_callee_names(d.id, opt_expr_callee_names(d.init, acc))
        }
        ast.ForInitPattern(p) -> pattern_callee_names(p, acc)
      }
      stmt_callee_names(body, expr_callee_names(right, acc))
    }
    ast.SwitchStatement(discriminant:, cases:) -> {
      let acc = expr_callee_names(discriminant, acc)
      use acc, ast.SwitchCase(condition:, consequent:) <- list.fold(cases, acc)
      stmts_callee_names(consequent, opt_expr_callee_names(condition, acc))
    }
    ast.TryStatement(block:, tail:) -> {
      let acc = stmts_callee_names(block, acc)
      case tail {
        ast.TryCatch(ast.CatchClause(body:, ..)) ->
          stmts_callee_names(body, acc)
        ast.TryFinally(finalizer:) -> stmts_callee_names(finalizer, acc)
        ast.TryCatchFinally(ast.CatchClause(body:, ..), finalizer:) ->
          stmts_callee_names(finalizer, stmts_callee_names(body, acc))
      }
    }
    ast.LabeledStatement(body:, ..) -> stmt_callee_names(body, acc)
    ast.WithStatement(object:, body:) ->
      stmt_callee_names(body, expr_callee_names(object, acc))
  }
}

fn stmts_callee_names(
  ss: List(ast.StmtWithLine),
  acc: List(String),
) -> List(String) {
  use acc, s <- list.fold(ss, acc)
  stmt_callee_names(s.statement, acc)
}

fn opt_expr_callee_names(
  ex: Option(ast.Expression),
  acc: List(String),
) -> List(String) {
  case ex {
    Some(e) -> expr_callee_names(e, acc)
    None -> acc
  }
}

fn exprs_callee_names(
  exs: List(ast.Expression),
  acc: List(String),
) -> List(String) {
  list.fold(exs, acc, fn(acc, ex) { expr_callee_names(ex, acc) })
}

fn expr_callee_names(ex: ast.Expression, acc: List(String)) -> List(String) {
  case ex {
    // The target: a plain identifier call. Record the name; still recurse
    // into arguments for nested calls. NOT OptionalCallExpression — `f?.()`
    // is nullish-guarded so `kfn_code` on it isn't unconditionally safe.
    ast.CallExpression(callee: ast.Identifier(name:, ..), arguments:, ..) ->
      exprs_callee_names(arguments, [name, ..acc])
    ast.CallExpression(callee:, arguments:, ..)
    | ast.OptionalCallExpression(callee:, arguments:, ..)
    | ast.NewExpression(callee:, arguments:, ..) ->
      exprs_callee_names(arguments, expr_callee_names(callee, acc))
    // do NOT descend into nested function bodies
    ast.FunctionExpression(..) | ast.ArrowFunctionExpression(..) -> acc
    ast.ClassExpression(super_class:, ..) ->
      opt_expr_callee_names(super_class, acc)
    // leaves
    ast.Identifier(..)
    | ast.NumberLiteral(..)
    | ast.BigIntLiteral(..)
    | ast.StringExpression(..)
    | ast.BooleanLiteral(..)
    | ast.NullLiteral(..)
    | ast.UndefinedExpression(..)
    | ast.ThisExpression(..)
    | ast.SuperExpression(..)
    | ast.MetaProperty(..)
    | ast.RegExpLiteral(..)
    | ast.IntrinsicTemplateObject(..) -> acc
    // recurse
    ast.AssignmentExpression(left:, right:, ..)
    | ast.BinaryExpression(left:, right:, ..)
    | ast.LogicalExpression(left:, right:, ..) ->
      expr_callee_names(right, expr_callee_names(left, acc))
    ast.UnaryExpression(argument:, ..)
    | ast.UpdateExpression(argument:, ..)
    | ast.AwaitExpression(argument:, ..)
    | ast.SpreadElement(argument:, ..) -> expr_callee_names(argument, acc)
    ast.YieldExpression(argument:, ..) -> opt_expr_callee_names(argument, acc)
    ast.ParenthesizedExpression(expression:, ..) ->
      expr_callee_names(expression, acc)
    ast.ConditionalExpression(condition:, consequent:, alternate:, ..) ->
      expr_callee_names(
        alternate,
        expr_callee_names(consequent, expr_callee_names(condition, acc)),
      )
    ast.MemberExpression(object:, property:, ..)
    | ast.OptionalMemberExpression(object:, property:, ..) -> {
      let acc = expr_callee_names(object, acc)
      case property {
        ast.Bracket(expression:) -> expr_callee_names(expression, acc)
        ast.Dot(..) -> acc
      }
    }
    ast.SequenceExpression(expressions:, ..) ->
      exprs_callee_names(expressions, acc)
    ast.ArrayExpression(elements:, ..) -> {
      use acc, el <- list.fold(elements, acc)
      opt_expr_callee_names(el, acc)
    }
    ast.ObjectExpression(properties:, ..) -> {
      use acc, prop <- list.fold(properties, acc)
      case prop {
        ast.InitProperty(key:, value:, ..) ->
          expr_callee_names(value, prop_key_callee_names(key, acc))
        ast.MethodProperty(key:, ..) | ast.AccessorProperty(key:, ..) ->
          prop_key_callee_names(key, acc)
        ast.SpreadProperty(argument:) -> expr_callee_names(argument, acc)
      }
    }
    ast.TemplateLiteral(parts:, ..) ->
      exprs_callee_names(ast.template_expressions(parts), acc)
    ast.TaggedTemplateExpression(tag:, parts:, ..) ->
      exprs_callee_names(
        ast.template_expressions(parts),
        expr_callee_names(tag, acc),
      )
    ast.ImportExpression(source:, options:, ..) ->
      opt_expr_callee_names(options, expr_callee_names(source, acc))
  }
}

fn prop_key_callee_names(
  key: ast.PropertyKey,
  acc: List(String),
) -> List(String) {
  case key {
    ast.KeyComputed(expression:) -> expr_callee_names(expression, acc)
    _ -> acc
  }
}

fn pattern_callee_names(p: ast.Pattern, acc: List(String)) -> List(String) {
  case p {
    ast.IdentifierPattern(..) -> acc
    ast.ArrayPattern(elements:) -> {
      use acc, el <- list.fold(elements, acc)
      case el {
        Some(inner) -> pattern_callee_names(inner, acc)
        None -> acc
      }
    }
    ast.ObjectPattern(properties:) -> {
      use acc, prop <- list.fold(properties, acc)
      case prop {
        ast.PatternProperty(key:, value:, ..) ->
          pattern_callee_names(value, prop_key_callee_names(key, acc))
        ast.RestProperty(..) -> acc
      }
    }
    ast.AssignmentPattern(left:, right:) ->
      pattern_callee_names(left, expr_callee_names(right, acc))
    ast.RestElement(argument:) -> pattern_callee_names(argument, acc)
  }
}

// -- assignment-target name collection (accumulator-passing walk) --

fn stmt_assigned_names(s: ast.Statement, acc: List(String)) -> List(String) {
  case s {
    ast.EmptyStatement
    | ast.DebuggerStatement
    | ast.BreakStatement(..)
    | ast.ContinueStatement(..)
    | // do NOT descend into nested function bodies
      ast.FunctionDeclaration(..) -> acc
    // Class BODY is a nested scope, but the heritage clause evaluates here.
    ast.ClassDeclaration(super_class:, ..) ->
      opt_expr_assigned_names(super_class, acc)
    ast.ExpressionStatement(expression:, ..) ->
      expr_assigned_names(expression, acc)
    ast.BlockStatement(body:) -> stmts_assigned_names(body, acc)
    ast.VariableDeclaration(kind:, declarations:) ->
      decls_assigned_names(kind, declarations, acc)
    ast.ReturnStatement(argument:) -> opt_expr_assigned_names(argument, acc)
    ast.IfStatement(condition:, consequent:, alternate:) -> {
      let acc = expr_assigned_names(condition, acc)
      let acc = stmt_assigned_names(consequent, acc)
      case alternate {
        Some(a) -> stmt_assigned_names(a, acc)
        None -> acc
      }
    }
    ast.ThrowStatement(argument:) -> expr_assigned_names(argument, acc)
    ast.WhileStatement(condition:, body:)
    | ast.DoWhileStatement(condition:, body:) ->
      stmt_assigned_names(body, expr_assigned_names(condition, acc))
    ast.ForStatement(init:, condition:, update:, body:) -> {
      let acc = case init {
        Some(fi) -> for_init_assigned_names(fi, acc)
        None -> acc
      }
      let acc = opt_expr_assigned_names(condition, acc)
      let acc = opt_expr_assigned_names(update, acc)
      stmt_assigned_names(body, acc)
    }
    ast.ForInStatement(left:, right:, body:)
    | ast.ForOfStatement(left:, right:, body:, ..) -> {
      let acc = for_init_assigned_names(left, acc)
      let acc = expr_assigned_names(right, acc)
      stmt_assigned_names(body, acc)
    }
    ast.SwitchStatement(discriminant:, cases:) -> {
      let acc = expr_assigned_names(discriminant, acc)
      use acc, ast.SwitchCase(condition:, consequent:) <- list.fold(cases, acc)
      let acc = opt_expr_assigned_names(condition, acc)
      stmts_assigned_names(consequent, acc)
    }
    ast.TryStatement(block:, tail:) -> {
      let acc = stmts_assigned_names(block, acc)
      case tail {
        ast.TryCatch(ast.CatchClause(body:, ..)) ->
          stmts_assigned_names(body, acc)
        ast.TryFinally(finalizer:) -> stmts_assigned_names(finalizer, acc)
        ast.TryCatchFinally(ast.CatchClause(body:, ..), finalizer:) ->
          stmts_assigned_names(finalizer, stmts_assigned_names(body, acc))
      }
    }
    ast.LabeledStatement(body:, ..) -> stmt_assigned_names(body, acc)
    ast.WithStatement(object:, body:) ->
      stmt_assigned_names(body, expr_assigned_names(object, acc))
  }
}

fn stmts_assigned_names(
  ss: List(ast.StmtWithLine),
  acc: List(String),
) -> List(String) {
  use acc, s <- list.fold(ss, acc)
  stmt_assigned_names(s.statement, acc)
}

fn decls_assigned_names(
  kind: ast.VariableKind,
  decls: List(ast.VariableDeclarator),
  acc: List(String),
) -> List(String) {
  use acc, ast.VariableDeclarator(id:, init:) <- list.fold(decls, acc)
  let acc = opt_expr_assigned_names(init, acc)
  // Defaults / computed keys inside the pattern evaluate in THIS scope for
  // every kind (let/const too) — walk them for assignments.
  let acc = pattern_expr_assigned_names(id, acc)
  // `var x = …` re-assigns the hoisted function-scope slot; let/const bind
  // fresh block-scope slots (won't be in outer slot_vars, but harmless).
  case kind, init {
    ast.Var, Some(_) -> pattern_names(id, acc)
    _, _ -> acc
  }
}

fn for_init_assigned_names(fi: ast.ForInit, acc: List(String)) -> List(String) {
  case fi {
    ast.ForInitExpression(ex) -> expr_assigned_names(ex, acc)
    ast.ForInitDeclaration(kind:, declarations:) -> {
      let acc = decls_assigned_names(kind, declarations, acc)
      // for-in/for-of `var x` head is assigned each iteration even sans init.
      case kind {
        ast.Var -> {
          use acc, d <- list.fold(declarations, acc)
          pattern_names(d.id, acc)
        }
        _ -> acc
      }
    }
    ast.ForInitPattern(p) ->
      pattern_names(p, pattern_expr_assigned_names(p, acc))
  }
}

fn pattern_names(p: ast.Pattern, acc: List(String)) -> List(String) {
  case p {
    ast.IdentifierPattern(name:, ..) -> [name, ..acc]
    ast.ArrayPattern(elements:) -> {
      use acc, el <- list.fold(elements, acc)
      case el {
        Some(inner) -> pattern_names(inner, acc)
        None -> acc
      }
    }
    ast.ObjectPattern(properties:) -> {
      use acc, prop <- list.fold(properties, acc)
      case prop {
        ast.PatternProperty(value:, ..) -> pattern_names(value, acc)
        ast.RestProperty(name:, ..) -> [name, ..acc]
      }
    }
    ast.AssignmentPattern(left:, ..) -> pattern_names(left, acc)
    ast.RestElement(argument:) -> pattern_names(argument, acc)
  }
}

/// Assigned-names from expressions EMBEDDED in a pattern (default initializers
/// and computed property keys). `let {[y++]: a = (x=1)} = o` inside a loop
/// mutates x and y in this scope; pattern_names alone only reports `a`.
fn pattern_expr_assigned_names(
  p: ast.Pattern,
  acc: List(String),
) -> List(String) {
  case p {
    ast.IdentifierPattern(..) -> acc
    ast.ArrayPattern(elements:) -> {
      use acc, el <- list.fold(elements, acc)
      case el {
        Some(inner) -> pattern_expr_assigned_names(inner, acc)
        None -> acc
      }
    }
    ast.ObjectPattern(properties:) -> {
      use acc, prop <- list.fold(properties, acc)
      case prop {
        ast.PatternProperty(key:, value:, ..) ->
          pattern_expr_assigned_names(value, prop_key_assigned_names(key, acc))
        ast.RestProperty(..) -> acc
      }
    }
    ast.AssignmentPattern(left:, right:) ->
      pattern_expr_assigned_names(left, expr_assigned_names(right, acc))
    ast.RestElement(argument:) -> pattern_expr_assigned_names(argument, acc)
  }
}

fn opt_expr_assigned_names(
  ex: Option(ast.Expression),
  acc: List(String),
) -> List(String) {
  case ex {
    Some(e) -> expr_assigned_names(e, acc)
    None -> acc
  }
}

fn exprs_assigned_names(
  exs: List(ast.Expression),
  acc: List(String),
) -> List(String) {
  list.fold(exs, acc, fn(acc, ex) { expr_assigned_names(ex, acc) })
}

fn expr_assigned_names(ex: ast.Expression, acc: List(String)) -> List(String) {
  case ex {
    ast.AssignmentExpression(left:, right:, ..) -> {
      let acc = assign_target_names(left, acc)
      expr_assigned_names(right, expr_assigned_names(left, acc))
    }
    ast.UpdateExpression(argument:, ..) ->
      expr_assigned_names(argument, assign_target_names(argument, acc))
    // do NOT descend into nested function bodies
    ast.FunctionExpression(..) | ast.ArrowFunctionExpression(..) -> acc
    // Class BODY is a nested scope, but the heritage clause evaluates here.
    ast.ClassExpression(super_class:, ..) ->
      opt_expr_assigned_names(super_class, acc)
    // leaves
    ast.Identifier(..)
    | ast.NumberLiteral(..)
    | ast.BigIntLiteral(..)
    | ast.StringExpression(..)
    | ast.BooleanLiteral(..)
    | ast.NullLiteral(..)
    | ast.UndefinedExpression(..)
    | ast.ThisExpression(..)
    | ast.SuperExpression(..)
    | ast.MetaProperty(..)
    | ast.RegExpLiteral(..)
    | ast.IntrinsicTemplateObject(..) -> acc
    // recurse
    ast.BinaryExpression(left:, right:, ..)
    | ast.LogicalExpression(left:, right:, ..) ->
      expr_assigned_names(right, expr_assigned_names(left, acc))
    ast.UnaryExpression(argument:, ..)
    | ast.AwaitExpression(argument:, ..)
    | ast.SpreadElement(argument:, ..) -> expr_assigned_names(argument, acc)
    ast.YieldExpression(argument:, ..) -> opt_expr_assigned_names(argument, acc)
    ast.ParenthesizedExpression(expression:, ..) ->
      expr_assigned_names(expression, acc)
    ast.ConditionalExpression(condition:, consequent:, alternate:, ..) ->
      expr_assigned_names(
        alternate,
        expr_assigned_names(consequent, expr_assigned_names(condition, acc)),
      )
    ast.CallExpression(callee:, arguments:, ..)
    | ast.OptionalCallExpression(callee:, arguments:, ..)
    | ast.NewExpression(callee:, arguments:, ..) ->
      exprs_assigned_names(arguments, expr_assigned_names(callee, acc))
    ast.MemberExpression(object:, property:, ..)
    | ast.OptionalMemberExpression(object:, property:, ..) -> {
      let acc = expr_assigned_names(object, acc)
      case property {
        ast.Bracket(expression:) -> expr_assigned_names(expression, acc)
        ast.Dot(..) -> acc
      }
    }
    ast.SequenceExpression(expressions:, ..) ->
      exprs_assigned_names(expressions, acc)
    ast.ArrayExpression(elements:, ..) -> {
      use acc, el <- list.fold(elements, acc)
      opt_expr_assigned_names(el, acc)
    }
    ast.ObjectExpression(properties:, ..) -> {
      use acc, prop <- list.fold(properties, acc)
      case prop {
        ast.InitProperty(key:, value:, ..) ->
          expr_assigned_names(value, prop_key_assigned_names(key, acc))
        ast.MethodProperty(key:, ..) | ast.AccessorProperty(key:, ..) ->
          prop_key_assigned_names(key, acc)
        ast.SpreadProperty(argument:) -> expr_assigned_names(argument, acc)
      }
    }
    ast.TemplateLiteral(parts:, ..) ->
      exprs_assigned_names(ast.template_expressions(parts), acc)
    ast.TaggedTemplateExpression(tag:, parts:, ..) ->
      exprs_assigned_names(
        ast.template_expressions(parts),
        expr_assigned_names(tag, acc),
      )
    ast.ImportExpression(source:, options:, ..) ->
      opt_expr_assigned_names(options, expr_assigned_names(source, acc))
  }
}

fn prop_key_assigned_names(
  key: ast.PropertyKey,
  acc: List(String),
) -> List(String) {
  case key {
    ast.KeyComputed(expression:) -> expr_assigned_names(expression, acc)
    _ -> acc
  }
}

/// Identifier names an assignment/update LHS binds. Handles destructuring
/// (`[a,{b}] = x`), defaults, rest, and parenthesized targets. Member
/// expressions bind no local slot.
fn assign_target_names(ex: ast.Expression, acc: List(String)) -> List(String) {
  case ex {
    ast.Identifier(name:, ..) -> [name, ..acc]
    ast.ParenthesizedExpression(expression:, ..) ->
      assign_target_names(expression, acc)
    ast.ArrayExpression(elements:, ..) -> {
      use acc, el <- list.fold(elements, acc)
      case el {
        Some(inner) -> assign_target_names(inner, acc)
        None -> acc
      }
    }
    ast.ObjectExpression(properties:, ..) -> {
      use acc, prop <- list.fold(properties, acc)
      case prop {
        ast.InitProperty(value:, ..) -> assign_target_names(value, acc)
        ast.SpreadProperty(argument:) -> assign_target_names(argument, acc)
        ast.MethodProperty(..) | ast.AccessorProperty(..) -> acc
      }
    }
    ast.AssignmentExpression(left:, ..) -> assign_target_names(left, acc)
    ast.SpreadElement(argument:, ..) -> assign_target_names(argument, acc)
    _ -> acc
  }
}

// -- bracket-base identifier collection (perf8_arr_c_hoist) --
// `#(reads, writes)`: identifier names appearing as `name[…]` bracket bases,
// split by whether the MemberExpression is an assignment target. Undercount is
// safe (hoist just doesn't fire) so uncommon nodes are treated as leaves.

type BrAcc =
  #(List(String), List(String))

fn stmt_bracket_bases(s: ast.Statement, acc: BrAcc) -> BrAcc {
  case s {
    ast.ExpressionStatement(expression:, ..) ->
      expr_bracket_bases(expression, acc)
    ast.BlockStatement(body:) -> stmts_bracket_bases(body, acc)
    ast.VariableDeclaration(declarations:, ..) -> {
      use acc, d <- list.fold(declarations, acc)
      opt_expr_bracket_bases(d.init, acc)
    }
    ast.ReturnStatement(argument:) -> opt_expr_bracket_bases(argument, acc)
    ast.IfStatement(condition:, consequent:, alternate:) -> {
      let acc = expr_bracket_bases(condition, acc)
      let acc = stmt_bracket_bases(consequent, acc)
      case alternate {
        Some(a) -> stmt_bracket_bases(a, acc)
        None -> acc
      }
    }
    ast.WhileStatement(condition:, body:)
    | ast.DoWhileStatement(condition:, body:) ->
      stmt_bracket_bases(body, expr_bracket_bases(condition, acc))
    ast.ForStatement(init:, condition:, update:, body:) -> {
      let acc = case init {
        Some(ast.ForInitExpression(ex)) -> expr_bracket_bases(ex, acc)
        _ -> acc
      }
      let acc = opt_expr_bracket_bases(condition, acc)
      let acc = opt_expr_bracket_bases(update, acc)
      stmt_bracket_bases(body, acc)
    }
    ast.ThrowStatement(argument:) -> expr_bracket_bases(argument, acc)
    ast.LabeledStatement(body:, ..) -> stmt_bracket_bases(body, acc)
    // Nested fn bodies / rare stmts: leaves (undercount safe).
    _ -> acc
  }
}

fn stmts_bracket_bases(ss: List(ast.StmtWithLine), acc: BrAcc) -> BrAcc {
  use acc, s <- list.fold(ss, acc)
  stmt_bracket_bases(s.statement, acc)
}

fn opt_expr_bracket_bases(ex: Option(ast.Expression), acc: BrAcc) -> BrAcc {
  case ex {
    Some(e) -> expr_bracket_bases(e, acc)
    None -> acc
  }
}

fn expr_bracket_bases(ex: ast.Expression, acc: BrAcc) -> BrAcc {
  case ex {
    // `name[idx] = v` / `name[idx] += v` — record write; recurse into idx + rhs.
    ast.AssignmentExpression(
      left: ast.MemberExpression(
        object: ast.Identifier(name:, ..),
        property: ast.Bracket(expression: idx),
        ..,
      ),
      right:,
      ..,
    ) -> {
      let #(r, w) = expr_bracket_bases(right, expr_bracket_bases(idx, acc))
      #(r, [name, ..w])
    }
    // `name[idx]++` — write.
    ast.UpdateExpression(
      argument: ast.MemberExpression(
        object: ast.Identifier(name:, ..),
        property: ast.Bracket(expression: idx),
        ..,
      ),
      ..,
    ) -> {
      let #(r, w) = expr_bracket_bases(idx, acc)
      #(r, [name, ..w])
    }
    // `name[idx]` in read position.
    ast.MemberExpression(
      object: ast.Identifier(name:, ..),
      property: ast.Bracket(expression: idx),
      ..,
    ) -> {
      let #(r, w) = expr_bracket_bases(idx, acc)
      #([name, ..r], w)
    }
    ast.AssignmentExpression(left:, right:, ..) ->
      expr_bracket_bases(right, expr_bracket_bases(left, acc))
    ast.UpdateExpression(argument:, ..)
    | ast.UnaryExpression(argument:, ..)
    | ast.SpreadElement(argument:, ..)
    | ast.AwaitExpression(argument:, ..) -> expr_bracket_bases(argument, acc)
    ast.YieldExpression(argument:, ..) -> opt_expr_bracket_bases(argument, acc)
    ast.BinaryExpression(left:, right:, ..)
    | ast.LogicalExpression(left:, right:, ..) ->
      expr_bracket_bases(right, expr_bracket_bases(left, acc))
    ast.ParenthesizedExpression(expression:, ..) ->
      expr_bracket_bases(expression, acc)
    ast.ConditionalExpression(condition:, consequent:, alternate:, ..) ->
      expr_bracket_bases(
        alternate,
        expr_bracket_bases(consequent, expr_bracket_bases(condition, acc)),
      )
    ast.CallExpression(callee:, arguments:, ..)
    | ast.OptionalCallExpression(callee:, arguments:, ..)
    | ast.NewExpression(callee:, arguments:, ..) -> {
      use acc, a <- list.fold(arguments, expr_bracket_bases(callee, acc))
      expr_bracket_bases(a, acc)
    }
    ast.MemberExpression(object:, property:, ..)
    | ast.OptionalMemberExpression(object:, property:, ..) -> {
      let acc = expr_bracket_bases(object, acc)
      case property {
        ast.Bracket(expression:) -> expr_bracket_bases(expression, acc)
        ast.Dot(..) -> acc
      }
    }
    ast.SequenceExpression(expressions:, ..) -> {
      use acc, e <- list.fold(expressions, acc)
      expr_bracket_bases(e, acc)
    }
    // Nested fn bodies + literals: leaves.
    _ -> acc
  }
}

// ── §14.7.5 ForIn/OfStatement — port emit.gleam:5634-5847, 6000-6030 ────────
// R15 shape: Block(brk) { Loop(head, params) { … Block(cont) { <body> } …
// Continue(head, next_params) } }. `ir_continue` on the Loop2 frame names the
// INNER Block, so a JS `continue` (emit_continue → ir.Break(cont, carried))
// falls out of it into the per-iteration advance, then re-enters the head.

/// Carried slot set for a for-in/of body: unboxed locals the body assigns,
/// plus any names the head re-binds each iteration (var/pattern/expr LHS —
/// let/const heads bind head-scoped slots that die at leave_for_scope).
fn for_in_of_carried(
  e: Emitter2,
  left: ast.ForInit,
  body: ast.Statement,
) -> List(Int) {
  let head_names = case left {
    ast.ForInitExpression(target) -> assign_target_names(target, [])
    ast.ForInitPattern(p) -> pattern_names(p, [])
    ast.ForInitDeclaration(kind:, declarations:) ->
      case kind {
        ast.Var ->
          list.fold(declarations, [], fn(acc, d) { pattern_names(d.id, acc) })
        // let/const/using head vars are head-scoped — not carried out.
        ast.Let | ast.Const | ast.Using | ast.AwaitUsing -> []
      }
  }
  let head_slots =
    list.filter_map(head_names, fn(name) {
      case state.resolve(e, name) {
        scope.Plain(scope.Local(slot:, boxed: False, ..)) ->
          case dict.has_key(e.slot_vars, slot) {
            True -> Ok(slot)
            False -> Error(Nil)
          }
        _ -> Error(Nil)
      }
    })
  assigned_unboxed_slots(e, body)
  |> list.append(head_slots)
  |> list.unique
  |> list.sort(int.compare)
}

/// §14.7.5.7 step 6.g port of emit.gleam:5618 emit_for_per_iteration_env: a
/// lexical head gets a FRESH iteration environment on every pass — re-run the
/// head scope's binding prologue so captured names are re-boxed and TDZ is
/// re-seeded before this iteration's LHS bind.
fn per_iteration_env(
  e: Emitter2,
  left: ast.ForInit,
  head_scope: ScopeId,
  k: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  case ast_util.for_head_lex_names(left) {
    [] -> k(e)
    _ -> binding_prologue(e, head_scope, k)
  }
}

/// PutValue to identifier `name` — Rk-CPS port of expr.emit_direct_put
/// (emit.gleam:6027 routes ForInitExpression through emit_destructuring_
/// assign, so a bare-Identifier LHS is an ASSIGNMENT, not a declaration:
/// const/fn-name write throws, let/capture TDZ-checks). store_declared is the
/// wrong path here — it silently rebinds a const's slot.
fn for_lhs_ident_assign(
  e: Emitter2,
  name: String,
  v: ir.Value,
  k: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  let throw_const = fn(e) {
    host_unit_(
      e,
      "throw_type_error",
      [
        ir.ConstBinary(bit_array.from_string(
          "Assignment to constant '" <> name <> "'",
        )),
      ],
      k,
    )
  }
  case state.resolve(e, name) {
    scope.Plain(scope.Local(origin_kind: ConstBinding, ..)) -> throw_const(e)
    scope.Plain(scope.Local(origin_kind: FnNameBinding, ..)) ->
      case e.strict {
        True -> throw_const(e)
        False -> k(e)
      }
    scope.Plain(scope.Local(slot:, boxed:, kind:, ..)) -> {
      let write = fn(e: Emitter2) {
        case boxed {
          True ->
            host_unit_(
              e,
              "cell_set",
              [ir.Var(state.get_slot_var(e, slot)), v],
              k,
            )
          False -> {
            let #(n, e) = state.fresh_var(e)
            use body <- result.map(k(state.set_slot_var(e, slot, n)))
            ir.Let([n], ir.Values([v]), body)
          }
        }
      }
      let checked = fn(e: Emitter2) {
        let read = fn(e, kk) {
          case boxed {
            True ->
              host_(e, "cell_get", [ir.Var(state.get_slot_var(e, slot))], kk)
            False -> kk(e, ir.Var(state.get_slot_var(e, slot)))
          }
        }
        use e, cur <- read(e)
        use e <- host_unit_(e, "tdz_check", [
          cur,
          ir.ConstBinary(bit_array.from_string(name)),
        ])
        write(e)
      }
      case kind {
        CaptureBinding -> checked(e)
        LetBinding ->
          case set.contains(e.initialized, slot) {
            True -> write(e)
            False -> checked(e)
          }
        _ -> write(e)
      }
    }
    scope.Plain(scope.Global(_)) ->
      case dict.get(e.slotted_globals, name) {
        Ok(slot) ->
          host_unit_(
            e,
            "cell_set",
            [ir.Var(state.get_slot_var(e, slot)), v],
            k,
          )
        Error(Nil) ->
          host_unit_(
            e,
            "global_set",
            [ir.ConstBinary(bit_array.from_string(name)), v],
            k,
          )
      }
    scope.Plain(scope.EvalEnv(_)) ->
      Error(state.UnsupportedFeature("direct eval"))
    scope.WithChain(..) -> Error(state.UnsupportedFeature("with"))
  }
}

/// Bind the current iteration value `v` to the for-in/of LHS. Port of
/// emit.gleam:6000-6030 emit_for_lhs_bind → dispatch.emit_destructure for
/// binding patterns; ForInitExpression assignment targets go through the
/// resolve+store path (Identifier) or dispatch.emit_expr on a synthetic
/// AssignmentExpression is not possible (v is IR-level), so the common
/// Identifier case is handled directly and the general destructuring-assign
/// case routes through emit_destructure with BindAssign.
fn for_lhs_bind(
  e: Emitter2,
  left: ast.ForInit,
  v: ir.Value,
  k: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  let via_destructure = fn(e: Emitter2, pat, mode) {
    use #(dtree, e) <- result.try(e.dispatch.emit_destructure(e, pat, v, mode))
    use e, _ <- let_(e, dtree)
    k(e)
  }
  case left {
    ast.ForInitDeclaration(kind:, declarations:) -> {
      let mode = case kind {
        ast.Var -> state.BindVar
        ast.Let -> state.BindLet
        // Using/AwaitUsing bind as const here; the disposer registration
        // (emit.gleam:1083 emit_for_of_using_body) is M17's dispose-resources.
        ast.Const | ast.Using | ast.AwaitUsing -> state.BindConst
      }
      case declarations {
        [ast.VariableDeclarator(id: pat, ..)] -> via_destructure(e, pat, mode)
        // Grammar allows exactly one ForBinding in a for-in/of head; a second
        // declarator forces the parser down the classic path (emit.gleam:6018).
        _ ->
          Error(state.EarlySyntaxError("multiple declarators in for-in/of head"))
      }
    }
    ast.ForInitPattern(pat) -> via_destructure(e, pat, state.BindVar)
    // Assignment-target LHS (`for (x of …)`, `for (obj.k of …)`): common
    // Identifier case writes the resolved binding directly; other targets
    // (member/array/object destructuring) route through emit_destructure with
    // BindAssign — Pattern has no MemberPattern so a member LHS is stored via
    // a synthetic runtime error path here (matches R12: user-reachable).
    ast.ForInitExpression(target) ->
      case ast_util.unwrap_parens(target) {
        ast.Identifier(name:, ..) -> for_lhs_ident_assign(e, name, v, k)
        ast.MemberExpression(..) as m -> {
          // `for (o.k of it)`: evaluate lref then PutValue. expr.emit_lvalue is
          // Build-typed; bridge via a set_prop host call after evaluating base
          // and key through dispatch.emit_expr.
          for_lhs_member_put(e, m, v, k)
        }
        // Destructuring-assign target `for ([a,b] of it)` — Pattern grammar
        // covers Identifier/Array/Object shapes with BindAssign semantics.
        ast.ArrayExpression(..) | ast.ObjectExpression(..) ->
          case expr_to_assign_pattern(target) {
            Ok(pat) -> via_destructure(e, pat, state.BindAssign)
            Error(msg) -> Error(state.EarlySyntaxError(msg))
          }
        _ ->
          Error(state.EarlySyntaxError("invalid for-in/of assignment target"))
      }
  }
}

/// `for (obj[k] of …)` / `for (obj.p of …)` — evaluate base and key, then
/// host("set_prop"). Key wire shape is anf.object_key_lit's `{string_key,
/// {named, <<n>>}}` tuple built inline (Rk cannot call the Build combinator).
fn for_lhs_member_put(
  e: Emitter2,
  m: ast.Expression,
  v: ir.Value,
  k: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  let assert ast.MemberExpression(object:, property:, ..) = m
  use e, base <- expr_(e, object)
  case property {
    // `#p` is a private field (ast.gleam:107) — resolve the class-minted
    // private-key local (expr.gleam:651) and route through private_set, not a
    // public string-key set_prop.
    ast.Dot(name: "#" <> _ as name, ..) ->
      case state.resolve(e, name) {
        scope.Plain(scope.Local(slot:, boxed: False, ..)) ->
          host_unit_(
            e,
            "private_set",
            [base, ir.Var(state.get_slot_var(e, slot)), v],
            k,
          )
        scope.Plain(scope.Local(slot:, boxed: True, ..)) -> {
          use e, key <- host_(e, "cell_get", [
            ir.Var(state.get_slot_var(e, slot)),
          ])
          host_unit_(e, "private_set", [base, key, v], k)
        }
        _ ->
          Error(state.EarlySyntaxError(
            "private field '" <> name <> "' outside class",
          ))
      }
    ast.Dot(name:, ..) -> {
      use e, inner <- let_(
        e,
        ir.TermOp(ir.MakeTuple, [
          ir.ConstAtom("named"),
          ir.ConstBinary(bit_array.from_string(name)),
        ]),
      )
      use e, key <- let_(
        e,
        ir.TermOp(ir.MakeTuple, [ir.ConstAtom("string_key"), inner]),
      )
      host_unit_(e, "set_prop", [base, key, v], k)
    }
    ast.Bracket(expression:) -> {
      use e, kv <- expr_(e, expression)
      use e, key <- host_(e, "to_property_key", [kv])
      host_unit_(e, "set_prop", [base, key, v], k)
    }
  }
}

/// Convert an Array/ObjectExpression assignment target to the Pattern grammar
/// so dispatch.emit_destructure(BindAssign) handles it. Member-expression
/// element targets (`[o.k] = v`) have no Pattern shape — they surface as an
/// EarlySyntaxError here (D15 residual; expr.emit_destructuring_assign is
/// Build-typed and not on EmitDispatch).
fn expr_to_assign_pattern(ex: ast.Expression) -> Result(ast.Pattern, String) {
  case ast_util.unwrap_parens(ex) {
    ast.Identifier(name:, span:) -> Ok(ast.IdentifierPattern(name:, span:))
    ast.ArrayExpression(elements:, ..) -> {
      use els <- result.try(
        list.try_map(elements, fn(el) {
          case el {
            None -> Ok(None)
            Some(ast.SpreadElement(argument:, ..)) -> {
              use inner <- result.map(expr_to_assign_pattern(argument))
              Some(ast.RestElement(inner))
            }
            Some(inner) -> {
              use p <- result.map(expr_to_assign_pattern(inner))
              Some(p)
            }
          }
        }),
      )
      Ok(ast.ArrayPattern(els))
    }
    ast.ObjectExpression(properties:, ..) -> {
      use props <- result.try(
        list.try_map(properties, fn(prop) {
          case prop {
            ast.InitProperty(key:, value:, shorthand:) -> {
              use vp <- result.map(expr_to_assign_pattern(value))
              ast.PatternProperty(key:, value: vp, shorthand:)
            }
            ast.SpreadProperty(argument:) ->
              case ast_util.unwrap_parens(argument) {
                ast.Identifier(name:, span:) ->
                  Ok(ast.RestProperty(name:, span:))
                _ -> Error("invalid rest property in for-in/of head")
              }
            ast.MethodProperty(..) | ast.AccessorProperty(..) ->
              Error("invalid destructuring target in for-in/of head")
          }
        }),
      )
      Ok(ast.ObjectPattern(props))
    }
    ast.AssignmentExpression(operator: ast.Assign, left:, right:, ..) -> {
      use lp <- result.map(expr_to_assign_pattern(left))
      ast.AssignmentPattern(left: lp, right:)
    }
    _ -> Error("invalid destructuring target in for-in/of head")
  }
}

/// `for (lhs in rhs) body`. Port of emit.gleam:5634-5690 → SPEC M13 row.
/// host("for_in_keys") yields an eager cons-list of BitArray property names
/// (D10: a BitArray IS a JS string value); the list tail is the Loop's first
/// param and threads via ir.Continue to the head. R15: JS `continue` targets
/// the inner ir.Block(cont) so the tail-advance sits between it and Continue.
fn emit_for_in(
  e: Emitter2,
  left: ast.ForInit,
  right: ast.Expression,
  body: ast.Statement,
  next: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  let has_lex = ast_util.for_classic_init_is_lex(Some(left))
  let #(e, save) = state.enter_for_scope(e, has_lex)
  let seed_head = fn(e: Emitter2, k) {
    case has_lex {
      True -> binding_prologue(e, e.cur_scope, k)
      False -> k(e)
    }
  }
  use e <- seed_head(e)
  let head_scope = e.cur_scope
  use e, obj <- expr_(e, right)
  use e, keys <- host_(e, "for_in_keys", [obj])
  let carried = for_in_of_carried(e, left, body)
  let result_tys = carried_types(carried)
  let #(brk, e) = state.fresh_label(e)
  let #(cont, e) = state.fresh_label(e)
  let #(head, e) = state.fresh_label(e)
  let #(tail_p, e) = state.fresh_var(e)
  let #(user_params, e) = carried_params(e, carried)
  let loop_params = [ir.LoopParam(tail_p, ir.TTerm, keys), ..user_params]
  let e = state.push_loop(e, brk, cont, carried, None)
  // Build the Loop body with slot_vars pointing at the LoopParam names.
  use #(loop_body, e) <- result.try(
    run_rk(e, fn(e, done) {
      let e = enter_loop_body(e, carried, user_params)
      use e, empty <- let_(e, ir.TermOp(ir.IsEmptyList, [ir.Var(tail_p)]))
      let brk_payload = carried_values(e, carried)
      // Nested run_rk (mirrors emit_while) so the not_empty arm's fresh_var
      // bumps thread out to `done` — a bare `{}` block would drop them and
      // the outer rebind_after_block would re-mint colliding names.
      use #(not_empty, e) <- result.try(
        run_rk(e, fn(e, done_ne) {
          use e, key <- let_(e, ir.TermOp(ir.ListHead, [ir.Var(tail_p)]))
          use e, rest <- let_(e, ir.TermOp(ir.ListTail, [ir.Var(tail_p)]))
          use e <- per_iteration_env(e, left, head_scope)
          use e <- for_lhs_bind(e, left, key)
          // Inner Block(cont) — JS `continue` Breaks to it (R15).
          use #(cont_body, e) <- result.try(
            run_rk(e, fn(e, done_cb) {
              use e <- emit_stmt(e, body)
              done_cb(e, ir.Values(carried_values(e, carried)))
            }),
          )
          use e <- rebind_after_block(
            e,
            carried,
            ir.Block(cont, result_tys, cont_body),
          )
          done_ne(e, ir.Continue(head, [rest, ..carried_values(e, carried)]))
        }),
      )
      done(e, ir.If(empty, [], ir.Break(brk, brk_payload), not_empty))
    }),
  )
  let e = state.pop_frame(e)
  let outer =
    ir.Block(brk, result_tys, ir.Loop(head, loop_params, [], loop_body))
  let e = state.leave_for_scope(e, save)
  rebind_after_block(e, carried, outer, next)
}

/// `for (lhs of rhs) body`. Port of emit.gleam:5750-5847 emit_for_of_common +
/// emit_for_of → SPEC M13 row. host("get_iterator", [rhs, sync]) yields a
/// stateful iterator handle; host("iter_next") returns #(done, value). A body
/// throw is caught by the loop-local ir.Try to close the iterator (abrupt=
/// true) then rethrow (§14.7.5.6 step 6.k IteratorClose). Normal-completion
/// close (break/exhaustion) is emitted after the outer Block per SPEC — the
/// runtime iterator record's [[Done]] flag makes the exhausted-case close a
/// no-op (SPEC.md:727). Crossing this loop (labeled break/continue/return)
/// inlines iter_close via Loop2.iter_close.
fn emit_for_of(
  e: Emitter2,
  left: ast.ForInit,
  right: ast.Expression,
  body: ast.Statement,
  next: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  let has_lex = ast_util.for_classic_init_is_lex(Some(left))
  let #(e, save) = state.enter_for_scope(e, has_lex)
  let seed_head = fn(e: Emitter2, k) {
    case has_lex {
      True -> binding_prologue(e, e.cur_scope, k)
      False -> k(e)
    }
  }
  use e <- seed_head(e)
  let head_scope = e.cur_scope
  use e, rhs_v <- expr_(e, right)
  // Bind iterator handle to a NAMED var so Loop2.iter_close and the catch
  // handler can reference it by name.
  let #(it, e) = state.fresh_var(e)
  let after_iter = fn(e: Emitter2) -> Result(ir.Expr, EmitError) {
    let carried = for_in_of_carried(e, left, body)
    let result_tys = carried_types(carried)
    let #(brk, e) = state.fresh_label(e)
    let #(cont, e) = state.fresh_label(e)
    let #(head, e) = state.fresh_label(e)
    let #(exn, e) = state.fresh_var(e)
    let #(user_params, e) = carried_params(e, carried)
    let e = state.push_loop(e, brk, cont, carried, Some(it))
    use #(loop_body, e) <- result.try(
      run_rk(e, fn(e, done) {
        let e = enter_loop_body(e, carried, user_params)
        use e, step <- host_(e, "iter_next", [ir.Var(it)])
        use e, done_v <- let_(e, ir.TermOp(ir.TupleGet(0), [step]))
        // M19: done_v is a Gleam Bool ATOM (SPEC:728 t_iter_next → #(Bool,
        // JsVal)), NOT a JsVal — to_boolean is JsVal→i32. M19 must wire the
        // bool-atom→i32 coercion (or make rt_js.to_boolean accept atoms).
        use e, done_i <- host_(e, "truthy", [done_v])
        let brk_payload = carried_values(e, carried)
        // Nested run_rk so every sibling arm's fresh_var bumps reach `done` —
        // ir.gleam:420 requires all Let names function-unique, and the catch
        // handler + outer rebind_after_block both mint fresh vars.
        use #(not_done, e) <- result.try(
          run_rk(e, fn(e, done_nd) {
            use e, val <- let_(e, ir.TermOp(ir.TupleGet(1), [step]))
            // §14.7.5.6 step 6.i-l: bind + body run under a try so an abrupt
            // completion closes the iterator (abrupt=true) then rethrows.
            use #(try_body, e) <- result.try(
              run_rk(e, fn(e, done_tb) {
                use e <- per_iteration_env(e, left, head_scope)
                use e <- for_lhs_bind(e, left, val)
                use #(cont_body, e) <- result.try(
                  run_rk(e, fn(e, done_cb) {
                    use e <- emit_stmt(e, body)
                    done_cb(e, ir.Values(carried_values(e, carried)))
                  }),
                )
                use e <- rebind_after_block(
                  e,
                  carried,
                  ir.Block(cont, result_tys, cont_body),
                )
                done_tb(e, ir.Continue(head, carried_values(e, carried)))
              }),
            )
            use #(handler, e) <- result.try(
              run_rk(e, fn(e, done_h) {
                use e <- host_unit_(e, "iter_close", [
                  ir.Var(it),
                  e.consts.true_,
                ])
                done_h(e, ir.Throw(e.consts.js_tag, [ir.Var(exn)]))
              }),
            )
            done_nd(
              e,
              ir.Try(result: [], body: try_body, handlers: [
                ir.CatchHandler(
                  on: ir.OnTag(e.consts.js_tag),
                  payload: [exn],
                  exnref: None,
                  handler:,
                ),
              ]),
            )
          }),
        )
        done(e, ir.If(done_i, [], ir.Break(brk, brk_payload), not_done))
      }),
    )
    let e = state.pop_frame(e)
    let outer =
      ir.Block(brk, result_tys, ir.Loop(head, user_params, [], loop_body))
    let e = state.leave_for_scope(e, save)
    rebind_after_block(e, carried, outer, fn(e) {
      // SPEC M13 row: close after loop. [[Done]]=true → runtime no-op.
      host_unit_(e, "iter_close", [ir.Var(it), e.consts.false_], next)
    })
  }
  use body_tree <- result.map(after_iter(e))
  ir.Let(
    [it],
    ir.CallHost("js", "get_iterator", [rhs_v, ir.ConstAtom("sync")]),
    body_tree,
  )
}

// ── §14.15 TryStatement (port emit.gleam:3875-3942, TryCatch only) ──────────

fn emit_try(
  e: Emitter2,
  block: List(ast.StmtWithLine),
  tail: ast.TryTail,
  next: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  case tail {
    ast.TryCatch(ast.CatchClause(param, catch_body)) -> {
      let carried =
        assigned_unboxed_slots_all(e, [
          ast.BlockStatement(block),
          ast.BlockStatement(catch_body),
        ])
      // try body: barrier so break/continue/return crossing it record
      // CatchOnly (structured ir.Try — nothing to emit at the crossing).
      // run_rk threads e out (state.gleam invariant #1: next_var/next_label/
      // fns_acc are module-monotone) so the handler and rebind_after_block see
      // vars/labels/fns allocated inside the try body.
      use #(try_body, e) <- result.try(
        run_rk(e, fn(e, done) {
          let e = state.push_barrier(e, None, None)
          use e <- emit_block(e, block)
          let e = state.pop_frame(e)
          done(e, ir.Values(carried_values(e, carried)))
        }),
      )
      use #(handler, e) <- result.try(emit_catch_handler(
        e,
        param,
        catch_body,
        carried,
      ))
      let region =
        ir.Try(result: carried_types(carried), body: try_body, handlers: [
          ir.CatchHandler(
            on: ir.OnTag(e.consts.js_tag),
            payload: ["_e"],
            exnref: None,
            handler:,
          ),
        ])
      rebind_after_block(e, carried, region, next)
    }
    // M17.md §3.4/§3.6 barrier-duplication (Gosub → inline). exn's k is
    // state.K (bare ir.Expr); bridge to Rk `next` via 0-arity Let-sequencing.
    // RULING slot-vars-scope-survival: exn.wrap_with_finally hardcodes
    // ir.Try(result: []) so cannot bridge carried slots — restore the pre-try
    // slot_vars so next(e) never references names Let-bound inside `tree`
    // (mirrors the emit_if branch_slots reset). Outer-slot writes made in
    // try/finally are DROPPED until exn.gleam threads `carried` and this arm
    // adopts the sibling TryCatch arm's rebind_after_block pattern.
    ast.TryFinally(finalizer) -> {
      let slot_vars0 = e.slot_vars
      use #(tree, e) <- result.try(
        exn.emit_try_finally(e, block, finalizer, fn(_ef) { ir.Values([]) }),
      )
      let e = state.Emitter2(..e, slot_vars: slot_vars0)
      use rest <- result.map(next(e))
      ir.Let([], tree, rest)
    }
    ast.TryCatchFinally(handler, finalizer) -> {
      let slot_vars0 = e.slot_vars
      use #(tree, e) <- result.try(
        exn.emit_try_catch_finally(e, block, handler, finalizer, fn(_ef) {
          ir.Values([])
        }),
      )
      let e = state.Emitter2(..e, slot_vars: slot_vars0)
      use rest <- result.map(next(e))
      ir.Let([], tree, rest)
    }
  }
}

/// Catch handler body — port of emit.gleam:3010-3026. `catch (p) Block`:
/// enter the Catch scope (holds ONLY the param), bind `p`, then emit_block the
/// body (which owns its own child scope). `catch Block` (no binding) creates
/// NO catch scope — emit_block the body directly; entering a scope here would
/// steal the next sibling's cursor id (emit.gleam:3006-3009). Returns the
/// threaded Emitter2 so emit_try preserves monotone counters (invariant #1).
fn emit_catch_handler(
  e: Emitter2,
  param: Option(ast.Pattern),
  catch_body: List(ast.StmtWithLine),
  carried: List(Int),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  use e, done <- run_rk(e)
  case param {
    Some(p) -> {
      let #(e, save) = state.enter_scope(e, in_block: e.in_block)
      use e <- binding_prologue(e, e.cur_scope)
      use #(dtree, e) <- result.try(e.dispatch.emit_destructure(
        e,
        p,
        ir.Var("_e"),
        state.BindLet,
      ))
      use e, _ <- let_(e, dtree)
      use e <- emit_block(e, catch_body)
      // RULING slot-vars-scope-survival: read carried_values from the INNER e
      // before leave_scope drops outer-slot rebinds; the handler is a WRAPPED
      // body so vals thread out via ir.Try.result → rebind_after_block.
      let vals = carried_values(e, carried)
      done(state.leave_scope(e, save), ir.Values(vals))
    }
    None -> {
      use e <- emit_block(e, catch_body)
      done(e, ir.Values(carried_values(e, carried)))
    }
  }
}

// ── §15.7 ClassDeclaration (port emit.gleam:4001-4021) ──────────────────────

fn emit_class_decl(
  e: Emitter2,
  name: Option(ast.NamedBinding),
  super_class: Option(ast.Expression),
  body: List(ast.ClassElement),
  next: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  case name {
    Some(ast.NamedBinding(name: n, ..)) -> {
      use #(tree, e) <- result.try(e.dispatch.emit_class(
        e,
        Some(n),
        Some(n),
        super_class,
        body,
      ))
      use e, ctor_h <- let_(e, tree)
      // Class names are block-scoped (like let); resolve in the current
      // scope and store the constructor handle into that binding's slot.
      store_slot(e, cur_scope_binding(e, n), ctor_h, next)
    }
    // Statement-position `class` requires a name; anonymous
    // `export default class {}` is a ClassExpression before it reaches here.
    None -> Error(state.EarlySyntaxError("anonymous class declaration"))
  }
}

// ── IfStatement — port emit.gleam:2981-3026 → structured ir.If ──────────────

fn emit_if(
  e: Emitter2,
  condition: ast.Expression,
  consequent: ast.Statement,
  alternate: Option(ast.Statement),
  next: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  let cons = block_wrap_fn_decl(consequent)
  let alt = option.map(alternate, block_wrap_fn_decl)
  let carried = case alt {
    Some(a) -> assigned_unboxed_slots_all(e, [cons, a])
    None -> assigned_unboxed_slots(e, cons)
  }
  use e, ci <- emit_cond_i32(e, condition)
  // Each branch: emit its statement, then yield the (possibly-updated) carried
  // locals as the branch's result values so the join point rebinds them.
  let branch_slots = e.slot_vars
  use #(then_tree, e) <- result.try(
    run_rk(e, fn(e, done) {
      use e <- emit_stmt(e, cons)
      done(e, ir.Values(carried_values(e, carried)))
    }),
  )
  // run_rk threads the then-leaf Emitter2 out; its slot_vars name Let-bindings
  // INSIDE then_tree. Restore pre-branch slot_vars so else-arm reads resolve to
  // outer names (ir.If arms lower to separate case clauses; expr.gleam:1415-21).
  let e = state.Emitter2(..e, slot_vars: branch_slots)
  use #(else_tree, e) <- result.try(case alt {
    Some(a) ->
      run_rk(e, fn(e, done) {
        use e <- emit_stmt(e, a)
        done(e, ir.Values(carried_values(e, carried)))
      })
    None -> Ok(#(ir.Values(carried_values(e, carried)), e))
  })
  // Restore pre-branch slot_vars — else's arm-local rebinds (both `carried`
  // and any expr-level slot -1 rebind) name Let-binders inside else_tree; the
  // join re-binds `carried` freshly and nothing else may leak.
  let e = state.Emitter2(..e, slot_vars: branch_slots)
  rebind_after_block(
    e,
    carried,
    ir.If(ci, carried_types(carried), then_tree, else_tree),
    next,
  )
}

// ── LabeledStatement — port emit.gleam:3953-3973 → set_pending / ir.Block ───

fn emit_labeled(
  e: Emitter2,
  label: String,
  body: ast.Statement,
  next: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  case body {
    // Labeled loop: the loop's own push_loop consumes pending_label so the
    // label targets both break AND continue on that loop — no separate frame.
    ast.WhileStatement(..)
    | ast.DoWhileStatement(..)
    | ast.ForStatement(..)
    | ast.ForInStatement(..)
    | ast.ForOfStatement(..) -> {
      let e = state.set_pending_label(e, label)
      emit_stmt(e, body, next)
    }
    // Labeled non-loop: break-only target. Wrap body in ir.Block(ir_break, ..)
    // so `break label` inside becomes Break(ir_break, carried).
    _ -> {
      let carried = assigned_unboxed_slots(e, body)
      let #(ir_break, e) = state.fresh_label(e)
      let e = state.push_labeled(e, ir_break, label, carried)
      use #(body_tree, e) <- result.try(
        run_rk(e, fn(e, done) {
          use e <- emit_stmt(e, body)
          done(e, ir.Values(carried_values(e, carried)))
        }),
      )
      let e = state.pop_frame(e)
      rebind_after_block(
        e,
        carried,
        ir.Block(ir_break, carried_types(carried), body_tree),
        next,
      )
    }
  }
}

// ── §14.12 SwitchStatement — port emit.gleam:5080-5190 → nested ir.Block ────
// Fallthrough encoding: each case body sits AFTER an ir.Block whose label the
// test chain Breaks to; nesting is source-order-inside-out so falling off
// case_i drops into case_{i+1}. Outer ir.Block(break_lbl) carries assigned
// unboxed locals as its result values (emit_break yields them on `break`).

/// One switch case with its body-entry ir label.
type CaseEntry {
  CaseEntry(
    lbl: String,
    cond: Option(ast.Expression),
    body: List(ast.StmtWithLine),
  )
}

fn emit_switch(
  e: Emitter2,
  disc: ast.Expression,
  cases: List(ast.SwitchCase),
  next: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  // Discriminant is evaluated OUTSIDE the CaseBlock scope (§14.12.4 step 1).
  use e, d <- expr_(e, disc)
  let all_stmts = ast_util.switch_case_stmts(cases)
  let carried =
    assigned_unboxed_slots_all(e, list.map(all_stmts, fn(s) { s.statement }))
  let #(break_lbl, e) = state.fresh_label(e)
  let e = state.push_switch(e, break_lbl, carried)
  // §14.12.4 step 3-5: CaseBlock is one block scope; instantiate its
  // let/const/class + hoist function declarations before any test/body runs.
  let #(e, save) = state.enter_scope(e, in_block: True)
  use e <- binding_prologue(e, e.cur_scope)
  use e <- hoist_fn_decls(e, all_stmts)
  // Allocate a body-entry label per case, in source order.
  let #(labelled_rev, e) =
    list.fold(cases, #([], e), fn(acc, c) {
      let #(out, e) = acc
      let ast.SwitchCase(condition:, consequent:) = c
      let #(lbl, e) = state.fresh_label(e)
      #([CaseEntry(lbl:, cond: condition, body: consequent), ..out], e)
    })
  let labelled = list.reverse(labelled_rev)
  let default_lbl =
    list.find_map(labelled, fn(c) {
      case c.cond {
        None -> Ok(c.lbl)
        Some(_) -> Error(Nil)
      }
    })
    |> option.from_result
  // Snapshot slot_vars: test exprs and body_i emission retarget them at
  // Let-bindings scoped INSIDE the dispatch/body_i tree; each body must start
  // from names in scope at its wrapper Let (mirrors emit_if:1538-1548).
  let branch_slots = e.slot_vars
  // Dispatch: strict_eq If-chain (R8 JPure) — faithful port of emit.gleam
  // :5080-5190, which has no i32 fast path; UnboxInt on a non-int discriminant
  // would trap where the reference just falls to default. On no-match, Break
  // to default (if any) else break_lbl. Every Break carries assigned locals so
  // each per-case Block yields them to its wrapper's rebind.
  let miss_leaf = fn(e: Emitter2) {
    case default_lbl {
      Some(dl) -> ir.Break(dl, carried_values(e, carried))
      None -> ir.Break(break_lbl, carried_values(e, carried))
    }
  }
  use #(dispatch, e) <- result.try(switch_test_chain(
    e,
    d,
    labelled,
    carried,
    miss_leaf,
  ))
  let e = state.Emitter2(..e, slot_vars: branch_slots)
  // Nest bodies inside-out: innermost Block wraps the dispatch; each outer
  // layer sequences the previous case's body so falling off case_i lands in
  // case_{i+1}. Last body's tail Breaks to break_lbl carrying assigned locals.
  use #(nested, e) <- result.try(switch_nest_bodies(
    e,
    branch_slots,
    labelled,
    dispatch,
    break_lbl,
    carried,
  ))
  let outer = ir.Block(break_lbl, carried_types(carried), nested)
  let e = state.leave_scope(e, save)
  let e = state.pop_frame(e)
  rebind_after_block(e, carried, outer, next)
}

/// Build the strict_eq If-chain over cases with a test. R8: strict_eq is
/// JPure — no St. Each match Breaks to that case's body label carrying the
/// assigned-local slot values (per-case Block result); the terminal else is
/// `miss` (default label or the outer break with carried values).
fn switch_test_chain(
  e: Emitter2,
  d: ir.Value,
  labelled: List(CaseEntry),
  carried: List(Int),
  miss: fn(Emitter2) -> ir.Expr,
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case labelled {
    [] -> Ok(#(miss(e), e))
    [CaseEntry(cond: None, ..), ..rest] ->
      // default: not tested — its label is the miss target.
      switch_test_chain(e, d, rest, carried, miss)
    [CaseEntry(lbl:, cond: Some(test_expr), ..), ..rest] -> {
      use #(test_tree, e) <- result.try(e.dispatch.emit_expr(e, test_expr))
      let #(t, e) = state.fresh_var(e)
      let #(eq, e) = state.fresh_var(e)
      let #(eqi, e) = state.fresh_var(e)
      use #(else_chain, e) <- result.map(switch_test_chain(
        e,
        d,
        rest,
        carried,
        miss,
      ))
      // R8: strict_eq JPure → TTerm bool atom; truthy → i32 for If.cond
      // (async.gleam:4156 mirror). Atom-as-cond is always non-zero.
      let branch =
        ir.If(
          ir.Var(eqi),
          [],
          ir.Break(lbl, carried_values(e, carried)),
          else_chain,
        )
      let with_eqi =
        ir.Let([eqi], ir.CallHost("js", "truthy", [ir.Var(eq)]), branch)
      let with_eq =
        ir.Let([eq], ir.CallHost("js", "strict_eq", [d, ir.Var(t)]), with_eqi)
      #(ir.Let([t], test_tree, with_eq), e)
    }
  }
}

/// Fold case bodies in source order into the nested-Block fallthrough shape.
/// `inner` starts as the dispatch chain; each step wraps it in
/// `Block(case_i, carried_types, inner)` and rebinds carried slots to that
/// Block's result — so body_i reads names bound by ITS wrapper Let, whether
/// entry was direct (dispatch Break) or fallthrough (body_{i-1}'s Values).
fn switch_nest_bodies(
  e: Emitter2,
  branch_slots: dict.Dict(Int, String),
  labelled: List(CaseEntry),
  inner: ir.Expr,
  break_lbl: String,
  carried: List(Int),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case labelled {
    [] -> Ok(#(inner, e))
    [CaseEntry(lbl:, body:, ..), ..rest] -> {
      // Reset slot_vars so rebind_after_block seeds from names in scope at the
      // wrapper Let, not from inside the previous body's tree (emit_if:1548).
      let e = state.Emitter2(..e, slot_vars: branch_slots)
      // Per-statement pdict save: a case body ending in break/return/throw
      // never reaches the tail continuation. Publish `cell` as RkTop so a
      // diverging transfer's rk_checkpoint threads its leaf Emitter2 (with
      // fns_acc/next_fn grown by an aux_fn in the return arg) here — else
      // the private cell rewinds to before the diverged stmt.
      let cell = make_ref()
      let _ = pdict_put(cell, e)
      let prev_top = rk_top_set(RkTop, cell)
      let r =
        rebind_after_block(
          e,
          carried,
          ir.Block(lbl, carried_types(carried), inner),
          fn(e) {
            let _ = pdict_put(cell, e)
            use ef <- each_(e, body, with: fn(e, located, k) {
              let _ = pdict_put(cell, e)
              emit_stmt(e, located.statement, fn(e2) {
                let _ = pdict_put(cell, e2)
                k(e2)
              })
            })
            let _ = pdict_put(cell, ef)
            case rest {
              [] -> Ok(ir.Break(break_lbl, carried_values(ef, carried)))
              _ -> Ok(ir.Values(carried_values(ef, carried)))
            }
          },
        )
      let _ = rk_top_restore(RkTop, prev_top)
      let e: Emitter2 = pdict_erase(cell)
      use wrapped <- result.try(r)
      switch_nest_bodies(e, branch_slots, rest, wrapped, break_lbl, carried)
    }
  }
}

// ── §14.7.4 ForStatement — port emit.gleam:3832-3858, 1137-1170, 5618 ───────

/// Resolve each `for (let …)` head name to its (slot, is_boxed) — declaration
/// binds in the head scope so resolution never crosses a with-chain.
fn resolve_per_iter(e: Emitter2, names: List(String)) -> List(#(Int, Bool)) {
  use name <- list.filter_map(names)
  case state.resolve(e, name) {
    scope.Plain(scope.Local(slot:, boxed:, ..)) -> Ok(#(slot, boxed))
    _ -> Error(Nil)
  }
}

/// §14.7.4.2 CreatePerIterationEnvironment. Port of emit_var_rebox
/// (emit.gleam:2017-2029): only boxed head-`let` slots need re-boxing so
/// closures created during iteration k keep iteration k's cell.
fn per_iter_rebox(
  e: Emitter2,
  per_iter: List(#(Int, Bool)),
  k: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  use e, #(slot, boxed), next <- each_(e, per_iter, then: k)
  case boxed {
    False -> next(e)
    True -> {
      let old = ir.Var(state.get_slot_var(e, slot))
      use e, v <- host_(e, "cell_get", [old])
      use e, cell <- host_(e, "cell_new", [v])
      let #(n, e) = state.fresh_var(e)
      use body <- result.map(next(state.set_slot_var(e, slot, n)))
      ir.Let([n], ir.Values([cell]), body)
    }
  }
}

/// `for (init; cond; upd) body`. Port of emit.gleam:3832-3858 +
/// emit_classic_loop:1137-1170. SPEC row: `enter_for_scope` if lex; init;
/// `While(test, {body; upd})`; per-iteration rebind for head-`let` bindings.
fn emit_for_classic(
  e: Emitter2,
  init: Option(ast.ForInit),
  cond: Option(ast.Expression),
  upd: Option(ast.Expression),
  body: ast.Statement,
  next: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  let has_lex = ast_util.for_classic_init_is_lex(init)
  let #(e, save) = state.enter_for_scope(e, has_lex)
  // state.enter_scope does NOT seed bindings (state.gleam:606) — do it here.
  let seed_head = fn(e: Emitter2, k) {
    case has_lex {
      True -> binding_prologue(e, e.cur_scope, k)
      False -> k(e)
    }
  }
  use e <- seed_head(e)
  // Emit the init clause; capture per-iteration `let` names.
  let after_init = fn(e: Emitter2, per_iter_names: List(String)) {
    let per_iter = resolve_per_iter(e, per_iter_names)
    let per_iter_slots = list.map(per_iter, fn(p) { p.0 })
    // Cond and upd may assign (`for(; (x = f()); i++)`) — walk both alongside
    // body so their targets thread out via LoopParam (matches emit_while).
    let head_stmts =
      list.filter_map([cond, upd], fn(x) {
        case x {
          Some(ex) -> Ok(ast.ExpressionStatement(ex, None))
          None -> Error(Nil)
        }
      })
    let carried =
      assigned_unboxed_slots_all(e, [body, ..head_stmts])
      |> list.append(per_iter_slots)
      |> list.unique
      |> list.sort(int.compare)
    let result_tys = carried_types(carried)
    let #(brk, e) = state.fresh_label(e)
    let #(cont, e) = state.fresh_label(e)
    let #(head, e) = state.fresh_label(e)
    // LoopParams read the PRE-loop slot vars as their `init`.
    let #(params, e) = carried_params(e, carried)
    // Known-number LoopParam: a classic-for counter written ONLY by a bare
    // `x++`/`x--` in `upd` (never in body/cond) stays a BEAM number iff its
    // pre-loop init var was one — number±1 is a number, and every path into
    // Block(cont) carries the untouched LoopParam. Mark it so guarded_binop /
    // emit_update / emit_loop_cond_i32 elide the `is_number` guard on it.
    let counter_known = for_counter_known(e, upd, body, cond, carried, params)
    let mark_counter = fn(e: Emitter2) {
      list.fold(counter_known, e, fn(e, slot) {
        state.mark_known_number(e, state.get_slot_var(e, slot))
      })
    }
    // Hoist `kfn_code(f, undef)` before the loop for each identifier callee
    // whose slot is loop-invariant; expr.emit_plain_call reads it back.
    let hoist_slots = loop_invariant_callees(e, body, cond, upd, carried)
    let prev_hoisted = e.hoisted_kfn
    use e <- hoist_kfn_codes(e, hoist_slots)
    // perf8_arr_c_hoist: same for `{tc_arr,Id}` overlay reads.
    let arr_slots = loop_invariant_arr_bases(e, body, cond, upd, carried, 2)
    let prev_arr_c = e.hoisted_arr_c
    use e <- hoist_arr_c(e, arr_slots)
    let e = state.push_loop(e, brk, cont, carried, None)
    let emit_upd = fn(e: Emitter2, k) {
      case upd {
        Some(u) -> expr_(e, u, fn(e, _) { k(e) })
        None -> k(e)
      }
    }
    // R15 3-label shape. `then_part` builds Block(cont){body}; rebind; rebox;
    // upd; Continue(head) via run_rk so the leaf Emitter2 threads out — the
    // body's fresh_var/child_fn_cursor/fns_acc must survive past the loop.
    use #(loop_body, e) <- result.try(
      run_rk(e, fn(e, done) {
        let e = mark_counter(enter_loop_body(e, carried, params))
        let then_part = fn(e) {
          run_rk(e, fn(e, d2) {
            use #(cont_body, e) <- result.try(
              run_rk(e, fn(e, d3) {
                use e <- emit_stmt(e, body)
                d3(e, ir.Values(carried_values(e, carried)))
              }),
            )
            use e <- rebind_after_block(
              e,
              carried,
              ir.Block(cont, result_tys, cont_body),
            )
            // Counter slot's Block(cont) result is its untouched LoopParam on
            // every path (body/cond never write it) — re-mark the rebind var.
            let e = mark_counter(e)
            // §14.7.4.3 3.e: rebox at the continue point — AFTER body,
            // BEFORE upd (emit.gleam:1158-1164 loop_continue: rebox; upd).
            use e <- per_iter_rebox(e, per_iter)
            use e <- emit_upd(e)
            d2(e, ir.Continue(head, carried_values(e, carried)))
          })
        }
        // §14.7.4.3 step 4.b: an absent condition is `true` — the loop runs
        // until an inner break/return/throw transfers out.
        case cond {
          None -> {
            use #(tt, e) <- result.try(then_part(e))
            done(e, tt)
          }
          Some(c) -> {
            use e, t <- emit_loop_cond_i32(e, c)
            let brk_payload = carried_values(e, carried)
            use #(tt, e) <- result.try(then_part(e))
            done(e, ir.If(t, [], tt, ir.Break(brk, brk_payload)))
          }
        }
      }),
    )
    let e = state.pop_frame(e)
    let e = state.Emitter2(..e, hoisted_kfn: prev_hoisted, hoisted_arr_c: prev_arr_c)
    let outer = ir.Block(brk, result_tys, ir.Loop(head, params, [], loop_body))
    let e = state.leave_for_scope(e, save)
    rebind_after_block(e, carried, outer, next)
  }
  case init {
    Some(ast.ForInitExpression(ex)) ->
      expr_(e, ex, fn(e, _) { after_init(e, []) })
    Some(ast.ForInitDeclaration(kind:, declarations:)) ->
      emit_var_decl(e, kind, declarations, fn(e) {
        after_init(e, ast_util.for_let_names(kind, declarations))
      })
    // ForInitPattern only appears in for-in/for-of heads (emit.gleam:3847).
    Some(ast.ForInitPattern(_)) | None -> after_init(e, [])
  }
}

/// Carried slots provably holding a BEAM number on EVERY iteration: written
/// only by a bare `x++`/`x--` in `upd`, never by body/cond, with a known-
/// number pre-loop init. Sound: base = init known; step = number±1 is a
/// number; every path into Block(cont) carries the untouched LoopParam.
fn for_counter_known(
  e: Emitter2,
  upd: Option(ast.Expression),
  body: ast.Statement,
  cond: Option(ast.Expression),
  carried: List(Int),
  params: List(ir.LoopParam),
) -> List(Int) {
  let counter_slot = case option.map(upd, ast_util.unwrap_parens) {
    Some(ast.UpdateExpression(argument: t, ..)) ->
      case ast_util.unwrap_parens(t) {
        ast.Identifier(name:, ..) ->
          case state.resolve(e, name) {
            scope.Plain(scope.Local(slot:, boxed: False, ..)) -> Some(slot)
            _ -> None
          }
        _ -> None
      }
    _ -> None
  }
  case counter_slot {
    None -> []
    Some(slot) -> {
      let cond_s = case cond {
        Some(c) -> [ast.ExpressionStatement(c, None)]
        None -> []
      }
      let elsewhere = assigned_unboxed_slots_all(e, [body, ..cond_s])
      let init_known =
        list.zip(carried, params)
        |> list.find(fn(sp) { sp.0 == slot })
        |> result.map(fn(sp) {
          case { sp.1 }.init {
            ir.Var(iv) -> state.is_known_number(e, iv)
            _ -> False
          }
        })
        |> result.unwrap(False)
      case init_known && !list.contains(elsewhere, slot) {
        True -> [slot]
        False -> []
      }
    }
  }
}

/// True iff `ex` is statically a BEAM number in the current scope: a finite
/// NumberLiteral, or an unboxed local whose current slot var is known-number.
/// AST-level so the mark isn't lost through `expr_`'s let-alias.
fn cond_operand_known(e: Emitter2, ex: ast.Expression) -> Bool {
  case ast_util.unwrap_parens(ex) {
    ast.NumberLiteral(_, ast.FiniteNumber(_)) -> True
    ast.Identifier(name:, ..) ->
      case state.resolve(e, name) {
        scope.Plain(scope.Local(slot:, boxed: False, ..)) ->
          state.is_known_number(e, state.get_slot_var(e, slot))
        _ -> False
      }
    _ -> False
  }
}

/// If/loop condition → raw i32 truth value for `ir.If`. Recognises the
/// dominant richards/deltablue cond shapes and emits the i32 directly,
/// SKIPPING the `truthy` (to_boolean_i32) call_ext when the producer is
/// already Int 0|1 (`==`/`!=` via loose_eq, relational via NumTerm). Any
/// unrecognised shape falls back to `expr_`→`truthy` (the previous default).
fn emit_cond_i32(
  e: Emitter2,
  cond: ast.Expression,
  k: Rk(ir.Value),
) -> Result(ir.Expr, EmitError) {
  case ast_util.unwrap_parens(cond) {
    // `==`: expr.binop → loose_eq → Int 0|1 already; use it as the cond.
    ast.BinaryExpression(operator: ast.Equal, ..) -> expr_(e, cond, k)
    // `!=`: emit as `==` (Int 0|1) then invert via `=:= 0` → 1|0.
    ast.BinaryExpression(operator: ast.NotEqual, left:, right:, span:) -> {
      use e, cv <- expr_(
        e,
        ast.BinaryExpression(span:, operator: ast.Equal, left:, right:),
      )
      let_(e, ir.NumTerm(ir.NEq, cv, ir.ConstI32(0)), k)
    }
    // Relational: inline NumTerm fast path (TermTest(IsNumber)×2 ∧ →
    // NumTerm(NLt/…) : host slow+truthy). When BOTH operands are statically
    // known BEAM numbers the guard/If/slow-arm are elided entirely.
    ast.BinaryExpression(operator: ast.LessThan, left:, right:, ..) ->
      cond_rel_i32(e, ir.NLt, "lt", left, right, k)
    ast.BinaryExpression(operator: ast.LessThanEqual, left:, right:, ..) ->
      cond_rel_i32(e, ir.NLe, "le", left, right, k)
    ast.BinaryExpression(operator: ast.GreaterThan, left:, right:, ..) ->
      cond_rel_i32(e, ir.NGt, "gt", left, right, k)
    ast.BinaryExpression(operator: ast.GreaterThanEqual, left:, right:, ..) ->
      cond_rel_i32(e, ir.NGe, "ge", left, right, k)
    // `!cond` in cond position: emit inner as i32, invert via `=:= 0`.
    ast.UnaryExpression(operator: ast.LogicalNot, argument: inner, ..) -> {
      use e, cv <- emit_cond_i32(e, inner)
      let_(e, ir.NumTerm(ir.NEq, cv, ir.ConstI32(0)), k)
    }
    _ -> {
      use e, cv <- expr_(e, cond)
      // anf.truthy_i32 inlines `true`/`false` → 1/0 so `if(method())` (whose
      // callee returns a bool atom — richards isHeldOrSuspended) skips the
      // to_boolean_i32 call_ext on the warm path. anf.run+let_ splices the
      // Build tree into this Rk chain (same bridge as expr_).
      let #(tree, e) = anf.run(anf.truthy_i32(cv), e)
      let_(e, tree, k)
    }
  }
}

fn cond_rel_i32(
  e: Emitter2,
  fast: ir.NumTermOp,
  slow_op: String,
  left: ast.Expression,
  right: ast.Expression,
  k: Rk(ir.Value),
) -> Result(ir.Expr, EmitError) {
  let a_known = cond_operand_known(e, left)
  let b_known = cond_operand_known(e, right)
  use e, a <- expr_(e, left)
  use e, b <- expr_(e, right)
  case a_known && b_known {
    True -> let_(e, ir.NumTerm(fast, a, b), k)
    False -> {
      use e, an <- let_(e, ir.TermTest(ir.IsNumber, a))
      use e, bn <- let_(e, ir.TermTest(ir.IsNumber, b))
      // Nested If instead of Num(IAnd(W32)) — the latter lowers to a
      // cross-module `rt_num:i32_and` call per iteration.
      use e, both <- let_(
        e,
        ir.If(an, [ir.TI32], ir.Values([bn]), ir.Values([ir.ConstI32(0)])),
      )
      use #(fast_arm, e) <- result.try(
        run_rk(e, fn(e, d) {
          use e, c <- let_(e, ir.NumTerm(fast, a, b))
          d(e, ir.Values([c]))
        }),
      )
      use #(slow_arm, e) <- result.try(
        run_rk(e, fn(e, d) {
          use e, sv <- host_(e, slow_op, [a, b])
          use e, ti <- host_(e, "truthy", [sv])
          d(e, ir.Values([ti]))
        }),
      )
      let_(e, ir.If(both, [ir.TI32], fast_arm, slow_arm), k)
    }
  }
}

/// Compat alias — earlier only loops had the specialised cond emit; now
/// `emit_if` shares it. Kept so the three loop callers stay unchanged.
fn emit_loop_cond_i32(
  e: Emitter2,
  cond: ast.Expression,
  k: Rk(ir.Value),
) -> Result(ir.Expr, EmitError) {
  emit_cond_i32(e, cond, k)
}

// ── While / DoWhile (port emit.gleam:3781-3808 → R15 uniform loop shape) ────

/// `while (cond) body`. Port of emit.gleam:3781-3793 → R15 shape:
/// Block(brk,[TTerm×n], Loop(head, params, [], truthy_if(cond, {Block(cont,
/// [TTerm×n], body); rebind; Continue(head, carried')}, Break(brk, carried))))
/// then rebind_after_block. push_loop stores (brk, cont) — JS `continue` →
/// ir.Break(cont) → falls into Continue(head) → cond re-checked.
fn emit_while(
  e: Emitter2,
  cond: ast.Expression,
  body: ast.Statement,
  next: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  // Cond may assign (`while ((x = f()))`) — include it in the carried walk.
  let carried =
    assigned_unboxed_slots_all(e, [ast.ExpressionStatement(cond, None), body])
  let result_tys = carried_types(carried)
  let #(brk, e) = state.fresh_label(e)
  let #(cont, e) = state.fresh_label(e)
  let #(head, e) = state.fresh_label(e)
  // LoopParam.init reads the PRE-loop slot vars.
  let #(params, e) = carried_params(e, carried)
  // Hoist `kfn_code(f, undef)` before the loop for each identifier callee
  // whose slot is loop-invariant; expr.emit_plain_call reads it back.
  let hoist_slots = loop_invariant_callees(e, body, Some(cond), None, carried)
  let prev_hoisted = e.hoisted_kfn
  use e <- hoist_kfn_codes(e, hoist_slots)
  // perf8_arr_c_hoist: same for `{tc_arr,Id}` overlay reads.
  let arr_slots =
    loop_invariant_arr_bases(e, body, Some(cond), None, carried, 2)
  let prev_arr_c = e.hoisted_arr_c
  use e <- hoist_arr_c(e, arr_slots)
  let e = state.push_loop(e, brk, cont, carried, None)
  use #(loop_body, e) <- result.try(
    run_rk(e, fn(e, done) {
      let e = enter_loop_body(e, carried, params)
      use e, t <- emit_loop_cond_i32(e, cond)
      // else-arm captured BEFORE body — the LoopParam names (plus any cond
      // assignment rebinds threaded through expr_/host_).
      let brk_payload = carried_values(e, carried)
      // Inner Block(cont) — JS `continue` Breaks to it (R15). run_rk (not a
      // bare `{}` block) so the post-body Emitter2 threads to `done` below.
      use #(then_tree, e) <- result.try(
        run_rk(e, fn(e, done_t) {
          use #(cont_body, e) <- result.try(
            run_rk(e, fn(e, done2) {
              use e <- emit_stmt(e, body)
              done2(e, ir.Values(carried_values(e, carried)))
            }),
          )
          use e <- rebind_after_block(
            e,
            carried,
            ir.Block(cont, result_tys, cont_body),
          )
          done_t(e, ir.Continue(head, carried_values(e, carried)))
        }),
      )
      done(e, ir.If(t, [], then_tree, ir.Break(brk, brk_payload)))
    }),
  )
  let e = state.pop_frame(e)
  let e = state.Emitter2(..e, hoisted_kfn: prev_hoisted, hoisted_arr_c: prev_arr_c)
  let outer = ir.Block(brk, result_tys, ir.Loop(head, params, [], loop_body))
  rebind_after_block(e, carried, outer, next)
}

/// `do body while (cond)`. Port of emit.gleam:3795-3808 → R15 shape: body
/// first, cond check after — the inner Block(cont) sits BEFORE the test so JS
/// `continue` (→ ir.Break(cont)) falls through to it (emit.gleam's `loop_cond`
/// label semantics).
fn emit_do_while(
  e: Emitter2,
  cond: ast.Expression,
  body: ast.Statement,
  next: fn(Emitter2) -> Result(ir.Expr, EmitError),
) -> Result(ir.Expr, EmitError) {
  let carried =
    assigned_unboxed_slots_all(e, [ast.ExpressionStatement(cond, None), body])
  let result_tys = carried_types(carried)
  let #(brk, e) = state.fresh_label(e)
  let #(cont, e) = state.fresh_label(e)
  let #(head, e) = state.fresh_label(e)
  let #(params, e) = carried_params(e, carried)
  let e = state.push_loop(e, brk, cont, carried, None)
  use #(loop_body, e) <- result.try(
    run_rk(e, fn(e, done) {
      let e = enter_loop_body(e, carried, params)
      // Inner Block(cont) — JS `continue` Breaks to it (R15).
      use #(cont_body, e) <- result.try(
        run_rk(e, fn(e, done2) {
          use e <- emit_stmt(e, body)
          done2(e, ir.Values(carried_values(e, carried)))
        }),
      )
      use e <- rebind_after_block(
        e,
        carried,
        ir.Block(cont, result_tys, cont_body),
      )
      use e, t <- emit_loop_cond_i32(e, cond)
      let payload = carried_values(e, carried)
      done(e, ir.If(t, [], ir.Continue(head, payload), ir.Break(brk, payload)))
    }),
  )
  let e = state.pop_frame(e)
  let outer = ir.Block(brk, result_tys, ir.Loop(head, params, [], loop_body))
  rebind_after_block(e, carried, outer, next)
}
