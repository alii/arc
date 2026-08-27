import arc/compiler/ast_util
import arc/compiler/scope.{
  type Binding, CaptureBinding, CatchBinding, ConstBinding, FnNameBinding,
  LetBinding, ParamBinding, VarBinding,
}
import arc/parser/ast
import arc_aot/emit/state.{type EmitError, type Emitter2}
import carder/ir
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

pub const js_exn_tag = "js_exn"

type Rk(a) =
  fn(Emitter2, a) -> Result(#(ir.Expr, Emitter2), EmitError)

fn let_(
  e: Emitter2,
  rhs: ir.Expr,
  k: Rk(ir.Value),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  state.let_(e, rhs, k)
}

fn host_(
  e: Emitter2,
  op: String,
  args: List(ir.Value),
  k: Rk(ir.Value),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let_(e, ir.CallHost("js", op, args), k)
}

fn host_unit_(
  e: Emitter2,
  op: String,
  args: List(ir.Value),
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  use e, _ <- host_(e, op, args)
  k(e)
}

fn each_(
  e: Emitter2,
  items: List(a),
  then k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
  with step: fn(
    Emitter2,
    a,
    fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
  ) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case items {
    [] -> k(e)
    [x, ..rest] -> step(e, x, fn(e) { each_(e, rest, k, step) })
  }
}

fn if_(
  e: Emitter2,
  cond: ir.Value,
  t: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
  f: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
  k: Rk(ir.Value),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  use #(tt, e) <- result.try(t(e))
  use #(ft, e) <- result.try(f(e))
  let_(e, ir.If(cond, [ir.TTerm], tt, ft), k)
}

fn pure_arm(
  v: ir.Value,
) -> fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError) {
  fn(e) { Ok(#(ir.Values([v]), e)) }
}

fn run_rk(
  e: Emitter2,
  f: fn(
    Emitter2,
    fn(Emitter2, ir.Expr) -> Result(#(ir.Expr, Emitter2), EmitError),
  ) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  f(e, fn(ef, tree) { Ok(#(tree, ef)) })
}

pub fn emit_throw_stmt(
  e: Emitter2,
  arg: ast.Expression,
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  use e, done <- run_rk(e)
  use #(tree, e) <- result.try(e.dispatch.emit_expr(e, arg))
  use e, v <- let_(e, tree)
  done(e, ir.Throw(js_exn_tag, [v]))
}

// §14.15.3 finally overrides the pending completion
pub fn inline_finally(
  e: Emitter2,
  body: List(ast.StmtWithLine),
  saved: state.ScopeSave2,
  then: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  // move scope cursor only, slot_vars must survive
  let here = snapshot_scope(e)
  let e =
    state.Emitter2(
      ..e,
      cur_scope: saved.cur_scope,
      scope_cursor: saved.scope_cursor,
      in_block: saved.in_block,
      frame_stack: drop_through_finally_barrier(e.frame_stack),
    )
  let e = state.push_barrier(e, None, None, None)
  use #(f_tree, e) <- result.try(
    e.dispatch.emit_stmts(e, body, fn(ef) {
      Ok(#(ir.Values([ef.consts.undef]), ef))
    }),
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

fn drop_through_finally_barrier(
  frames: List(state.Frame2),
) -> List(state.Frame2) {
  case frames {
    [] -> []
    [state.Barrier2(finally_body: Some(_), ..), ..rest] -> rest
    [_, ..rest] -> drop_through_finally_barrier(rest)
  }
}

fn snapshot_scope(e: Emitter2) -> state.ScopeSave2 {
  state.ScopeSave2(
    cur_scope: e.cur_scope,
    scope_cursor: e.scope_cursor,
    slot_vars: e.slot_vars,
    in_block: e.in_block,
  )
}

fn as_block(body: List(ast.StmtWithLine)) -> List(ast.StmtWithLine) {
  [ast.StmtWithLine(0, ast.BlockStatement(body))]
}

fn emit_finalizer(
  e: Emitter2,
  finalizer: List(ast.StmtWithLine),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let e = state.push_barrier(e, None, None, None)
  use #(tree, e) <- result.map(
    e.dispatch.emit_stmts(e, as_block(finalizer), fn(ef) {
      Ok(#(ir.Values([]), ef))
    }),
  )
  #(tree, state.pop_frame(e))
}

pub fn emit_try_finally(
  e: Emitter2,
  block: List(ast.StmtWithLine),
  finalizer: List(ast.StmtWithLine),
  k: state.K,
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  use e <- wrap_with_finally(e, finalizer, block_scope_count(block), k)
  e.dispatch.emit_stmts(e, as_block(block), fn(ef) { Ok(#(ir.Values([]), ef)) })
}

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
  let #(esc, e) = state.fresh_escape(e, 0)
  let e = state.push_barrier(e, None, None, Some(esc))
  use #(body_ir, e) <- result.try(
    e.dispatch.emit_stmts(e, as_block(block), fn(ef) {
      Ok(#(ir.Values([]), ef))
    }),
  )
  let e = state.pop_frame(e)
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
  state.land_escapes(e, esc, inner)
}

fn block_scope_count(body: List(ast.StmtWithLine)) -> Int {
  case ast_util.block_has_declarations(body) {
    True -> 1
    False -> 0
  }
}

fn catch_scope_count(
  param: Option(ast.Pattern),
  catch_body: List(ast.StmtWithLine),
) -> Int {
  case param {
    Some(_) -> 1
    None -> block_scope_count(catch_body)
  }
}

fn wrap_with_finally(
  e: Emitter2,
  finalizer: List(ast.StmtWithLine),
  scopes_before_fin: Int,
  k: state.K,
  protected build: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let entry_save = snapshot_scope(e)
  let fin_save =
    state.ScopeSave2(
      ..entry_save,
      scope_cursor: list.drop(e.scope_cursor, scopes_before_fin),
    )
  let #(esc, e) = state.fresh_escape(e, 0)
  let e =
    state.push_barrier(
      e,
      Some(#(as_block(finalizer), fin_save)),
      None,
      Some(esc),
    )
  use #(protected_ir, e) <- result.try(build(e))
  let e = state.pop_frame(e)
  let fin_pos = snapshot_scope(e)
  let #(ex, e) = state.fresh_var(e)
  use #(f_throw, e) <- result.try(emit_finalizer(e, finalizer))
  let throw_handler = ir.Let([], f_throw, ir.Throw(js_exn_tag, [ir.Var(ex)]))
  let e = state.leave_scope(e, fin_pos)
  use #(f_normal, e) <- result.try(emit_finalizer(e, finalizer))
  let #(region, e) =
    state.land_escapes(
      e,
      esc,
      ir.Try(result: [], body: protected_ir, handlers: [
        ir.CatchHandler(
          on: ir.OnTag(js_exn_tag),
          payload: [ex],
          exnref: None,
          handler: throw_handler,
        ),
      ]),
    )
  // TODO: rebinds inside try are not threaded out yet
  let e = state.Emitter2(..e, slot_vars: entry_save.slot_vars)
  use tail <- state.map_tree(k(e))
  ir.Let([], region, ir.Let([], f_normal, tail))
}

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
        e.dispatch.emit_stmts(e, as_block(catch_body), fn(ef) {
          Ok(#(ir.Values([]), ef))
        }),
      )
      done(state.leave_scope(e, save), body_ir)
    }
    None -> {
      use #(body_ir, e) <- result.try(
        e.dispatch.emit_stmts(e, as_block(catch_body), fn(ef) {
          Ok(#(ir.Values([]), ef))
        }),
      )
      done(e, body_ir)
    }
  }
}

pub fn catch_binding_prologue(
  e: Emitter2,
  scope_id: scope.ScopeId,
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let bindings =
    dict.to_list(scope.get_scope(e.tree, scope_id).bindings)
    |> list.sort(fn(a, b) { int.compare({ a.1 }.slot, { b.1 }.slot) })
  use e, entry, next <- each_(e, bindings, then: k)
  let #(_, b): #(String, Binding) = entry
  let name = state.slot_var_name(e, b.slot)
  let seed = fn(e: Emitter2, init) {
    case b.is_boxed {
      False -> {
        use body <- state.map_tree(next(state.set_slot_var(e, b.slot, name)))
        ir.Let([name], ir.Values([init]), body)
      }
      True -> {
        use e, cell <- host_(e, "cell_new", [init])
        use body <- state.map_tree(next(state.set_slot_var(e, b.slot, name)))
        ir.Let([name], ir.Values([cell]), body)
      }
    }
  }
  case b.kind {
    VarBinding -> seed(e, e.consts.undef)
    LetBinding | ConstBinding | FnNameBinding -> seed(e, e.consts.tdz)
    CatchBinding | ParamBinding -> seed(e, e.consts.undef)
    CaptureBinding -> next(e)
  }
}
