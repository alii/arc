import arc/compiler/ast_util
import arc/compiler/scope.{
  type Binding, CaptureBinding, CatchBinding, ConstBinding, FnNameBinding,
  LetBinding, ParamBinding, VarBinding,
}
import arc/parser/ast
import arc_aot/emit/state.{
  type EmitResult, type Emitter, type Next, type NextWith,
}
import carder/ir
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

fn let_(e: Emitter, rhs: ir.Expr, k: NextWith(ir.Value)) -> EmitResult {
  state.let_(e, rhs, k)
}

fn host_(
  e: Emitter,
  op: String,
  args: List(ir.Value),
  k: NextWith(ir.Value),
) -> EmitResult {
  let_(e, ir.CallHost("js", op, args), k)
}

fn each_(
  e: Emitter,
  items: List(a),
  then k: Next,
  with step: fn(Emitter, a, Next) -> EmitResult,
) -> EmitResult {
  case items {
    [] -> k(e)
    [x, ..rest] -> step(e, x, fn(e) { each_(e, rest, k, step) })
  }
}

fn with_done(
  e: Emitter,
  f: fn(NextWith(ir.Expr), Emitter) -> EmitResult,
) -> EmitResult {
  f(fn(tree, ef) { Ok(#(tree, ef)) }, e)
}

// §14.15.3 finally overrides the pending completion
pub fn inline_finally(
  e: Emitter,
  body: List(ast.StmtWithLine),
  saved: state.ScopeSnapshot,
  then: Next,
) -> EmitResult {
  // move scope cursor only, slot_vars must survive
  let here = snapshot_scope(e)
  let e =
    state.Emitter(
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
    state.Emitter(
      ..e,
      cur_scope: here.cur_scope,
      scope_cursor: here.scope_cursor,
      in_block: here.in_block,
    )
  use _, e <- let_(e, f_tree)
  then(e)
}

fn drop_through_finally_barrier(
  frames: List(state.Frame),
) -> List(state.Frame) {
  case frames {
    [] -> []
    [state.BarrierFrame(finally_body: Some(_), ..), ..rest] -> rest
    [_, ..rest] -> drop_through_finally_barrier(rest)
  }
}

fn snapshot_scope(e: Emitter) -> state.ScopeSnapshot {
  state.ScopeSnapshot(
    cur_scope: e.cur_scope,
    scope_cursor: e.scope_cursor,
    slot_vars: e.slot_vars,
    in_block: e.in_block,
  )
}

fn as_block(body: List(ast.StmtWithLine)) -> List(ast.StmtWithLine) {
  [ast.StmtWithLine(0, ast.BlockStatement(body))]
}

fn emit_finalizer(e: Emitter, finalizer: List(ast.StmtWithLine)) -> EmitResult {
  let e = state.push_barrier(e, None, None, None)
  use #(tree, e) <- result.map(
    e.dispatch.emit_stmts(e, as_block(finalizer), fn(ef) {
      Ok(#(ir.Values([]), ef))
    }),
  )
  #(tree, state.pop_frame(e))
}

pub fn emit_try_finally(
  e: Emitter,
  block: List(ast.StmtWithLine),
  finalizer: List(ast.StmtWithLine),
  k: Next,
) -> EmitResult {
  use e <- wrap_with_finally(e, finalizer, block_scope_count(block), k)
  e.dispatch.emit_stmts(e, as_block(block), fn(ef) { Ok(#(ir.Values([]), ef)) })
}

pub fn emit_try_catch_finally(
  e: Emitter,
  block: List(ast.StmtWithLine),
  handler: ast.CatchClause,
  finalizer: List(ast.StmtWithLine),
  k: Next,
) -> EmitResult {
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
        on: ir.OnTag(e.consts.exn_tag),
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
  e: Emitter,
  finalizer: List(ast.StmtWithLine),
  scopes_before_fin: Int,
  k: Next,
  protected build: Next,
) -> EmitResult {
  let entry_save = snapshot_scope(e)
  let fin_save =
    state.ScopeSnapshot(
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
  let throw_handler =
    ir.Let([], f_throw, ir.Throw(e.consts.exn_tag, [ir.Var(ex)]))
  let e = state.leave_scope(e, fin_pos)
  use #(f_normal, e) <- result.try(emit_finalizer(e, finalizer))
  let #(region, e) =
    state.land_escapes(
      e,
      esc,
      ir.Try(result: [], body: protected_ir, handlers: [
        ir.CatchHandler(
          on: ir.OnTag(e.consts.exn_tag),
          payload: [ex],
          exnref: None,
          handler: throw_handler,
        ),
      ]),
    )
  // TODO: rebinds inside try are not threaded out yet
  let e = state.Emitter(..e, slot_vars: entry_save.slot_vars)
  use tail <- state.map_tree(k(e))
  ir.Let([], region, ir.Let([], f_normal, tail))
}

fn emit_catch_arm(
  e: Emitter,
  param: Option(ast.Pattern),
  catch_body: List(ast.StmtWithLine),
  ex_name: String,
) -> EmitResult {
  use done, e <- with_done(e)
  case param {
    Some(p) -> {
      let #(save, e) = state.enter_scope(e, in_block: e.in_block)
      use e <- catch_binding_prologue(e, e.cur_scope)
      use #(dtree, e) <- result.try(e.dispatch.emit_destructure(
        e,
        p,
        ir.Var(ex_name),
        state.BindLet,
      ))
      use _, e <- let_(e, dtree)
      use #(body_ir, e) <- result.try(
        e.dispatch.emit_stmts(e, as_block(catch_body), fn(ef) {
          Ok(#(ir.Values([]), ef))
        }),
      )
      done(body_ir, state.leave_scope(e, save))
    }
    None -> {
      use #(body_ir, e) <- result.try(
        e.dispatch.emit_stmts(e, as_block(catch_body), fn(ef) {
          Ok(#(ir.Values([]), ef))
        }),
      )
      done(body_ir, e)
    }
  }
}

pub fn catch_binding_prologue(
  e: Emitter,
  scope_id: scope.ScopeId,
  k: Next,
) -> EmitResult {
  let bindings =
    dict.to_list(scope.get(e.scope_tree, scope_id).bindings)
    |> list.sort(fn(a, b) { int.compare({ a.1 }.slot, { b.1 }.slot) })
  use e, entry, next <- each_(e, bindings, then: k)
  let #(_, b): #(String, Binding) = entry
  let name = state.slot_base_name(e, b.slot)
  let seed = fn(e: Emitter, init) {
    case b.boxed {
      False -> {
        use body <- state.map_tree(next(state.set_slot_var(e, b.slot, name)))
        ir.Let([name], ir.Values([init]), body)
      }
      True -> {
        use box, e <- host_(e, "box_new", [init])
        use body <- state.map_tree(next(state.set_slot_var(e, b.slot, name)))
        ir.Let([name], ir.Values([box]), body)
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
