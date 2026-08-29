import arc/compiler/ast_util
import arc/compiler/scope.{
  type Binding, type ScopeId, CaptureBinding, CatchBinding, ConstBinding,
  FnNameBinding, LetBinding, ParamBinding, VarBinding,
}
import arc/parser/ast
import arc_aot/emit/anf
import arc_aot/emit/exn
import arc_aot/emit/expr
import arc_aot/emit/state.{
  type BarrierCleanup, type EmitError, type Emitter2, FnDecl, StmtBody,
}
import carder/ir
import gleam/bit_array
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set

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
        state.Switch2(ir_break:, carried:, ..) if ir_break == ir_label -> carried
        state.Labeled2(ir_break:, carried:, ..) if ir_break == ir_label ->
          carried
        _ -> frame_carried_walk(rest, ir_label)
      }
  }
}

fn carried_values(e: Emitter2, slots: List(Int)) -> List(ir.Value) {
  list.map(slots, fn(slot) { ir.Var(state.get_slot_var(e, slot)) })
}

// leave_scope drops inner slot_vars; re-apply the carried names
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

fn bool_atom(e: Emitter2, b: Bool) -> ir.Value {
  case b {
    True -> e.consts.true_
    False -> e.consts.false_
  }
}

fn inline_cleanup(
  e: Emitter2,
  cleanup: BarrierCleanup,
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case cleanup {
    state.FinallyBlock(body:, saved_scope:, escape: None) ->
      exn.inline_finally(e, body, saved_scope, k)
    state.FinallyBlock(body:, saved_scope:, escape: Some(esc)) -> {
      let carried = assigned_unboxed_slots_all(e, [ast.BlockStatement(body)])
      use #(f_tree, e) <- result.try(
        run_rk(e, fn(e, done) {
          use e <- exn.inline_finally(e, body, saved_scope)
          done(e, ir.Values(carried_values(e, carried)))
        }),
      )
      use #(region, e) <- result.try(escaping(e, esc, carried, f_tree))
      rebind_after_block(e, carried, region, k)
    }
    state.IterClose(iter_var:, is_async:, escape: None) ->
      host_unit_(e, "iter_close", [ir.Var(iter_var), bool_atom(e, is_async)], k)
    state.IterClose(iter_var:, is_async:, escape: Some(esc)) -> {
      let close =
        ir.Let(
          ["_"],
          ir.CallHost("js", "iter_close", [
            ir.Var(iter_var),
            bool_atom(e, is_async),
          ]),
          ir.Values([]),
        )
      use #(region, e) <- result.try(escaping(e, esc, [], close))
      use tail <- state.map_tree(k(e))
      ir.Let([], region, tail)
    }
    state.CatchOnly -> k(e)
  }
}

fn escaping(
  e: Emitter2,
  esc: state.Escape,
  carried: List(Int),
  body: ir.Expr,
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let #(handler, e) = state.escape_handler(e, esc)
  Ok(#(ir.Try(result: carried_types(carried), body:, handlers: [handler]), e))
}

fn inline_cleanups(
  e: Emitter2,
  cleanups: List(BarrierCleanup),
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case cleanups {
    [] -> k(e)
    [c, ..rest] -> inline_cleanup(e, c, fn(e) { inline_cleanups(e, rest, k) })
  }
}

fn return_cleanups(frames: List(state.Frame2)) -> List(BarrierCleanup) {
  use frame <- list.flat_map(frames)
  case frame {
    state.Loop2(iter_close: Some(#(iv, esc)), ..) -> [
      state.IterClose(iv, False, Some(esc)),
    ]
    state.Loop2(..) | state.Switch2(..) | state.Labeled2(..) -> []
    state.Barrier2(finally_body:, iter_close:, escape:) -> {
      let acc = case finally_body {
        Some(#(body, save)) -> [state.FinallyBlock(body, save, escape)]
        None -> []
      }
      case iter_close {
        Some(iv) -> [state.IterClose(iv, False, escape), ..acc]
        None ->
          case acc {
            [] -> [state.CatchOnly]
            _ -> acc
          }
      }
    }
  }
}

fn emit_break(
  e: Emitter2,
  label: Option(String),
  _next: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  use #(ir_label, cleanups) <- result.try(state.find_break_target(e, label))
  let frames = e.frame_stack
  use e <- inline_cleanups(e, cleanups)
  case sm_goto(e, ir_label) {
    Some(r) -> r
    None -> {
      let carried = find_frame_carried(e, ir_label)
      Ok(#(ir.Break(ir_label, carried_values(e, carried)), e))
    }
  }
  |> keep_frames(frames)
}

fn keep_frames(
  r: Result(#(ir.Expr, Emitter2), EmitError),
  frames: List(state.Frame2),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  use #(tree, e) <- result.map(r)
  #(tree, state.Emitter2(..e, frame_stack: frames))
}

fn emit_continue(
  e: Emitter2,
  label: Option(String),
  _next: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  use #(ir_label, cleanups) <- result.try(state.find_continue_target(e, label))
  let frames = e.frame_stack
  use e <- inline_cleanups(e, cleanups)
  case sm_goto(e, ir_label) {
    Some(r) -> r
    None -> {
      let carried = find_frame_carried(e, ir_label)
      Ok(#(ir.Break(ir_label, carried_values(e, carried)), e))
    }
  }
  |> keep_frames(frames)
}

fn sm_goto(
  e: Emitter2,
  ir_label: String,
) -> Option(Result(#(ir.Expr, Emitter2), EmitError)) {
  case e.sm_abrupt {
    Some(sm) -> sm.on_goto(e, ir_label)
    None -> None
  }
}

fn derived_return(
  e: Emitter2,
  v: ir.Value,
  k: Rk(ir.Value),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case e.derived_ctor {
    False -> k(e, v)
    True -> {
      let #(tree, e) = anf.run(expr.derived_return_value(v), e)
      let_(e, tree, k)
    }
  }
}

fn emit_return(
  e: Emitter2,
  arg: Option(ast.Expression),
  _next: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let with_value = fn(e: Emitter2, v: ir.Value) {
    let frames = e.frame_stack
    use e <- inline_cleanups(e, return_cleanups(e.frame_stack))
    use e, v <- derived_return(e, v)
    case e.sm_abrupt {
      Some(sm) -> sm.on_return(e, v)
      None -> Ok(#(ir.Return([v]), e))
    }
    |> keep_frames(frames)
  }
  case arg {
    None -> with_value(e, e.consts.undef)
    Some(ex) -> {
      use #(rhs, e) <- result.try(e.dispatch.emit_expr(e, ex))
      let_(e, rhs, with_value)
    }
  }
}

fn expr_(
  e: Emitter2,
  ex: ast.Expression,
  k: Rk(ir.Value),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  use #(tree, e) <- result.try(e.dispatch.emit_expr(e, ex))
  let_(e, tree, k)
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

pub fn emit_stmts(
  e: Emitter2,
  ss: List(ast.StmtWithLine),
  k: state.K,
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case expr.prop_write_run(e, ss) {
    Some(#(run, rest)) -> {
      let #(tree, e) = anf.run(expr.emit_prop_write_run(run), e)
      use e, _ <- let_(e, tree)
      emit_stmts(e, rest, k)
    }
    None ->
      case ss {
        [] -> k(e)
        [located, ..rest] ->
          emit_stmt(e, located.statement, fn(e) { emit_stmts(e, rest, k) })
      }
  }
}

fn emit_stmt(
  e: Emitter2,
  s: ast.Statement,
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case s {
    ast.EmptyStatement | ast.DebuggerStatement -> k(e)
    ast.ExpressionStatement(expression:, ..) ->
      expr_(e, expression, fn(e, _) { k(e) })
    ast.WithStatement(..) -> Error(state.UnsupportedFeature("with"))
    // already hoisted; only annex b §B.3.2.6 sloppy copy runs here
    ast.FunctionDeclaration(
      name: Some(ast.NamedBinding(name:, ..)),
      is_generator: False,
      is_async: False,
      ..,
    ) -> {
      let blocked =
        set.contains(scope.get_scope(e.tree, e.cur_scope).annexb_blocked, name)
      case e.in_block && !e.strict && !blocked {
        True -> annexb_promote(e, name, k)
        False -> k(e)
      }
    }
    ast.FunctionDeclaration(..) -> k(e)
    ast.BlockStatement([]) -> k(e)

    ast.BreakStatement(label:) -> emit_break(e, label, k)
    ast.ContinueStatement(label:) -> emit_continue(e, label, k)
    ast.ReturnStatement(argument:) -> emit_return(e, argument, k)

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
    ast.ThrowStatement(argument:) ->
      expr_(e, argument, fn(ef, v) {
        Ok(#(ir.Throw(ef.consts.js_tag, [v]), ef))
      })
    ast.TryStatement(block:, tail:) -> emit_try(e, block, tail, k)
    ast.ClassDeclaration(name:, super_class:, body:) ->
      emit_class_decl(e, name, super_class, body, k)
  }
}

fn store_slot(
  e: Emitter2,
  b: Binding,
  val: ir.Value,
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case b.is_boxed {
    True ->
      host_unit_(e, "cell_set", [ir.Var(state.get_slot_var(e, b.slot)), val], k)
    False -> {
      let name = state.slot_var_name(e, b.slot)
      use body <- state.map_tree(k(state.set_slot_var(e, b.slot, name)))
      ir.Let([name], ir.Values([val]), body)
    }
  }
}

fn binding_prologue(
  e: Emitter2,
  scope_id: ScopeId,
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
    ParamBinding | CatchBinding | CaptureBinding -> next(e)
  }
}

fn hoist_fn_decls(
  e: Emitter2,
  stmts: List(ast.StmtWithLine),
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
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

fn fold_body(
  e: Emitter2,
  body: List(ast.StmtWithLine),
  next: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  each_(e, body, then: next, with: fn(e, located, k) {
    emit_stmt(e, located.statement, k)
  })
}

fn emit_block(
  e: Emitter2,
  body: List(ast.StmtWithLine),
  next: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case body {
    [] -> next(e)
    _ ->
      case ast_util.block_has_declarations(body) {
        False -> fold_body(e, body, next)
        True -> {
          let carried = assigned_unboxed_slots(e, ast.BlockStatement(body))
          let #(e, save) = state.enter_scope(e, in_block: True)
          let entered = e.cur_scope
          use e <- binding_prologue(e, e.cur_scope)
          use e <- hoist_fn_decls(e, body)
          case ast_util.has_using_decl(body) {
            False -> {
              use #(tree, e) <- result.map(
                fold_body(e, body, fn(ef) {
                  next(leave_scope_carrying(ef, save, carried))
                }),
              )
              #(tree, state.leave_scope_if_inside(e, entered, save))
            }
            True -> Error(state.UnsupportedFeature("using declaration"))
          }
        }
      }
  }
}

// annex b §B.3.1 function declaration as if clause
pub fn block_wrap_fn_decl(stmt: ast.Statement) -> ast.Statement {
  case stmt {
    ast.FunctionDeclaration(..) ->
      ast.BlockStatement([ast.StmtWithLine(0, stmt)])
    _ -> stmt
  }
}

// annex b §B.3.2.6 copy block function into var scope
fn annexb_promote(
  e: Emitter2,
  name: String,
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case annexb_find_source(e, e.cur_scope, name) {
    None -> k(e)
    Some(#(source, outside)) -> {
      let target = case annexb_find_target(e, outside, name) {
        None -> Some(scope.Global(name))
        Some(scope.Binding(kind: LetBinding, ..))
        | Some(scope.Binding(kind: ConstBinding, ..))
        | Some(scope.Binding(kind: FnNameBinding, ..)) -> None
        Some(scope.Binding(slot:, kind:, is_boxed:, origin_kind_for_capture:)) ->
          Some(scope.Local(
            slot:,
            boxed: is_boxed,
            kind:,
            origin_kind: origin_kind_for_capture,
          ))
      }
      case target {
        None -> k(e)
        Some(d) -> {
          let copy = {
            use v <- anf.then(read_binding(source))
            expr.emit_direct_put(d, name, v)
          }
          let #(tree, e) = anf.run(copy, e)
          use e, _ <- let_(e, tree)
          k(e)
        }
      }
    }
  }
}

fn read_binding(b: Binding) -> anf.Build(ir.Value) {
  fn(e: Emitter2, k) {
    let v = ir.Var(state.get_slot_var(e, b.slot))
    case b.is_boxed {
      True -> anf.host("cell_get", [v])(e, k)
      False -> k(e, v)
    }
  }
}

fn scope_parent_in_fn(e: Emitter2, id: ScopeId) -> Option(ScopeId) {
  case id == e.fn_scope {
    True -> None
    False -> scope.get_scope(e.tree, id).parent
  }
}

fn annexb_find_source(
  e: Emitter2,
  from: ScopeId,
  name: String,
) -> Option(#(Binding, Option(ScopeId))) {
  let node = scope.get_scope(e.tree, from)
  case dict.get(node.bindings, name) {
    Ok(b) -> Some(#(b, scope_parent_in_fn(e, from)))
    Error(Nil) ->
      case scope_parent_in_fn(e, from) {
        Some(parent) -> annexb_find_source(e, parent, name)
        None -> None
      }
  }
}

fn annexb_find_target(
  e: Emitter2,
  from: Option(ScopeId),
  name: String,
) -> Option(Binding) {
  case from {
    None -> None
    Some(id) -> {
      let node = scope.get_scope(e.tree, id)
      case node.kind {
        scope.Catch -> annexb_find_target(e, scope_parent_in_fn(e, id), name)
        _ ->
          case dict.get(node.bindings, name) {
            Ok(scope.Binding(kind: CatchBinding, ..)) | Error(Nil) ->
              annexb_find_target(e, scope_parent_in_fn(e, id), name)
            Ok(b) -> Some(b)
          }
      }
    }
  }
}

fn store_declared(
  e: Emitter2,
  name: String,
  v: ir.Value,
  lexical: Bool,
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
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
          let #(n, e) = state.fresh_slot_var(e, slot)
          let e = case v {
            ir.Var(vn) ->
              case state.is_known_number(e, vn) {
                True -> state.mark_known_number(e, n)
                False -> e
              }
            _ -> e
          }
          use body <- state.map_tree(k(state.set_slot_var(e, slot, n)))
          ir.Let([n], ir.Values([v]), body)
        }
      }
    }
    scope.Plain(scope.Global(_)) ->
      case dict.get(e.slotted_globals, name) {
        Ok(slot) ->
          host_unit_(e, "cell_set", [ir.Var(state.get_slot_var(e, slot)), v], k)
        Error(Nil) -> {
          let #(tree, e) = anf.run(anf.key(name), e)
          use e, key <- let_(e, tree)
          host_unit_(e, "global_set", [key, v], k)
        }
      }
    scope.Plain(scope.EvalEnv(_)) ->
      Error(state.UnsupportedFeature("direct eval"))
    scope.WithChain(..) -> Error(state.UnsupportedFeature("with"))
  }
}

fn emit_var_decl(
  e: Emitter2,
  kind: ast.VariableKind,
  decls: List(ast.VariableDeclarator),
  next: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
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
          use #(tree, e) <- result.try(e.dispatch.emit_expr_named(
            e,
            init_expr,
            Some(name),
          ))
          use e, v <- let_(e, tree)
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

fn carried_params(
  e: Emitter2,
  slots: List(Int),
) -> #(List(ir.LoopParam), Emitter2) {
  let #(e, params) = {
    use #(e, acc), slot <- list.fold(slots, #(e, []))
    let init = ir.Var(state.get_slot_var(e, slot))
    let #(name, e) = state.fresh_slot_var(e, slot)
    #(e, [ir.LoopParam(name:, ty: ir.TTerm, init:), ..acc])
  }
  #(list.reverse(params), e)
}

fn carried_types(slots: List(Int)) -> List(ir.ValType) {
  list.map(slots, fn(_) { ir.TTerm })
}

fn enter_loop_body(
  e: Emitter2,
  slots: List(Int),
  params: List(ir.LoopParam),
) -> Emitter2 {
  use e, #(slot, param) <- list.fold(list.zip(slots, params), e)
  state.set_slot_var(e, slot, param.name)
}

fn rebind_after_block(
  e: Emitter2,
  slots: List(Int),
  rhs: ir.Expr,
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let #(e, names) = {
    use #(e, names), slot <- list.fold(slots, #(e, []))
    let #(n, e) = state.fresh_slot_var(e, slot)
    #(state.set_slot_var(e, slot, n), [n, ..names])
  }
  let names = list.reverse(names)
  use body <- state.map_tree(k(e))
  ir.Let(names, rhs, body)
}

fn assigned_unboxed_slots(e: Emitter2, s: ast.Statement) -> List(Int) {
  let bound_unboxed = fn(slot, boxed) {
    case boxed, dict.has_key(e.slot_vars, slot) {
      False, True -> Ok(slot)
      _, _ -> Error(Nil)
    }
  }
  let annexb = case e.strict {
    True -> []
    False -> state.fn_info(e).annexb_candidates
  }
  stmt_assigned_names(s, [])
  |> list.unique
  |> list.flat_map(fn(name) {
    let plain = case state.resolve(e, name) {
      scope.Plain(scope.Local(slot:, boxed:, ..)) -> bound_unboxed(slot, boxed)
      _ -> Error(Nil)
    }
    let twin = case list.contains(annexb, name) {
      False -> Error(Nil)
      True ->
        case annexb_find_target(e, Some(e.cur_scope), name) {
          Some(scope.Binding(kind: VarBinding, slot:, is_boxed:, ..)) ->
            bound_unboxed(slot, is_boxed)
          _ -> Error(Nil)
        }
    }
    result.values([plain, twin])
  })
  |> list.unique
  |> list.sort(int.compare)
}

fn assigned_unboxed_slots_all(
  e: Emitter2,
  ss: List(ast.Statement),
) -> List(Int) {
  list.flat_map(ss, assigned_unboxed_slots(e, _))
  |> list.unique
  |> list.sort(int.compare)
}

fn hoist_kfn_codes(
  e: Emitter2,
  slots: List(#(Int, Bool)),
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
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

// todo: misses reassignment inside called functions
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

fn stmt_callee_names(s: ast.Statement, acc: List(String)) -> List(String) {
  case s {
    ast.EmptyStatement
    | ast.DebuggerStatement
    | ast.BreakStatement(..)
    | ast.ContinueStatement(..)
    | // skip nested function bodies
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
    ast.CallExpression(callee: ast.Identifier(name:, ..), arguments:, ..) ->
      exprs_callee_names(arguments, [name, ..acc])
    ast.CallExpression(callee:, arguments:, ..)
    | ast.OptionalCallExpression(callee:, arguments:, ..)
    | ast.NewExpression(callee:, arguments:, ..) ->
      exprs_callee_names(arguments, expr_callee_names(callee, acc))
    ast.FunctionExpression(..) | ast.ArrowFunctionExpression(..) -> acc
    ast.ClassExpression(super_class:, ..) ->
      opt_expr_callee_names(super_class, acc)
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

fn stmt_assigned_names(s: ast.Statement, acc: List(String)) -> List(String) {
  case s {
    ast.EmptyStatement
    | ast.DebuggerStatement
    | ast.BreakStatement(..)
    | ast.ContinueStatement(..) -> acc
    ast.FunctionDeclaration(
      name: Some(ast.NamedBinding(name:, ..)),
      is_generator: False,
      is_async: False,
      ..,
    ) -> [name, ..acc]
    ast.FunctionDeclaration(..) -> acc
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
  let acc = pattern_expr_assigned_names(id, acc)
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
    ast.FunctionExpression(..) | ast.ArrowFunctionExpression(..) -> acc
    ast.ClassExpression(super_class:, ..) ->
      opt_expr_assigned_names(super_class, acc)
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

// §14.7.5.7 fresh iteration env each pass
fn per_iteration_env(
  e: Emitter2,
  left: ast.ForInit,
  head_scope: ScopeId,
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case ast_util.for_head_lex_names(left) {
    [] -> k(e)
    _ -> binding_prologue(e, head_scope, k)
  }
}

// an assignment, not a declaration: const throws, let tdz checks
fn for_lhs_ident_assign(
  e: Emitter2,
  name: String,
  v: ir.Value,
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
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
            let #(n, e) = state.fresh_slot_var(e, slot)
            use body <- state.map_tree(k(state.set_slot_var(e, slot, n)))
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
          host_unit_(e, "cell_set", [ir.Var(state.get_slot_var(e, slot)), v], k)
        Error(Nil) -> {
          let #(tree, e) = anf.run(anf.key(name), e)
          use e, key <- let_(e, tree)
          host_unit_(e, expr.global_set_op(e.strict), [key, v], k)
        }
      }
    scope.Plain(scope.EvalEnv(_)) ->
      Error(state.UnsupportedFeature("direct eval"))
    scope.WithChain(..) -> Error(state.UnsupportedFeature("with"))
  }
}

fn for_lhs_bind(
  e: Emitter2,
  left: ast.ForInit,
  v: ir.Value,
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
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
        ast.Const | ast.Using | ast.AwaitUsing -> state.BindConst
      }
      case declarations {
        [ast.VariableDeclarator(id: pat, ..)] -> via_destructure(e, pat, mode)
        _ ->
          Error(state.EarlySyntaxError("multiple declarators in for-in/of head"))
      }
    }
    ast.ForInitPattern(pat) -> via_destructure(e, pat, state.BindVar)
    ast.ForInitExpression(target) ->
      case ast_util.unwrap_parens(target) {
        ast.Identifier(name:, ..) -> for_lhs_ident_assign(e, name, v, k)
        ast.MemberExpression(..) as m -> {
          for_lhs_member_put(e, m, v, k)
        }
        ast.ArrayExpression(..) | ast.ObjectExpression(..) -> {
          let assign =
            anf.then(expr.emit_destructuring_assign(target, v), fn(_) {
              anf.pure(v)
            })
          let #(dtree, e) = anf.run(assign, e)
          use e, _ <- let_(e, dtree)
          k(e)
        }
        _ ->
          Error(state.EarlySyntaxError("invalid for-in/of assignment target"))
      }
  }
}

fn for_lhs_member_put(
  e: Emitter2,
  m: ast.Expression,
  v: ir.Value,
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let assert ast.MemberExpression(object:, property:, ..) = m
  use e, base <- expr_(e, object)
  case property {
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
      let #(tree, e) = anf.run(anf.then(anf.key(name), anf.string_key), e)
      use e, key <- let_(e, tree)
      host_unit_(e, expr.set_prop_op_name(e.strict), [base, key, v], k)
    }
    ast.Bracket(expression:) -> {
      use e, kv <- expr_(e, expression)
      use e, key <- host_(e, "to_property_key", [kv])
      host_unit_(e, expr.set_prop_op_name(e.strict), [base, key, v], k)
    }
  }
}

fn emit_for_in(
  e: Emitter2,
  left: ast.ForInit,
  right: ast.Expression,
  body: ast.Statement,
  next: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
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
  use #(loop_body, e) <- result.try(
    run_rk(e, fn(e, done) {
      let e = enter_loop_body(e, carried, user_params)
      use e, empty <- let_(e, ir.TermOp(ir.IsEmptyList, [ir.Var(tail_p)]))
      let brk_payload = carried_values(e, carried)
      use #(not_empty, e) <- result.try(
        run_rk(e, fn(e, done_ne) {
          use e, key <- let_(e, ir.TermOp(ir.ListHead, [ir.Var(tail_p)]))
          use e, rest <- let_(e, ir.TermOp(ir.ListTail, [ir.Var(tail_p)]))
          use e <- per_iteration_env(e, left, head_scope)
          use e <- for_lhs_bind(e, left, key)
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

fn emit_for_of(
  e: Emitter2,
  left: ast.ForInit,
  right: ast.Expression,
  body: ast.Statement,
  next: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
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
  let #(it, e) = state.fresh_var(e)
  let after_iter = fn(e: Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError) {
    let carried = for_in_of_carried(e, left, body)
    let result_tys = carried_types(carried)
    let #(brk, e) = state.fresh_label(e)
    let #(cont, e) = state.fresh_label(e)
    let #(head, e) = state.fresh_label(e)
    let #(exn, e) = state.fresh_var(e)
    let #(user_params, e) = carried_params(e, carried)
    let #(esc, e) = state.fresh_escape(e, 0)
    let e = state.push_loop(e, brk, cont, carried, Some(#(it, esc)))
    use #(loop_body, e) <- result.try(
      run_rk(e, fn(e, done) {
        let e = enter_loop_body(e, carried, user_params)
        use e, step <- host_(e, "iter_next", [ir.Var(it)])
        use e, done_v <- let_(e, ir.TermOp(ir.TupleGet(0), [step]))
        use e, done_i <- let_(e, anf.is_true_expr(done_v))
        let brk_payload = carried_values(e, carried)
        use #(not_done, e) <- result.try(
          run_rk(e, fn(e, done_nd) {
            use e, val <- let_(e, ir.TermOp(ir.TupleGet(1), [step]))
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
            let #(region, e) =
              state.land_escapes(
                e,
                esc,
                ir.Try(result: [], body: try_body, handlers: [
                  ir.CatchHandler(
                    on: ir.OnTag(e.consts.js_tag),
                    payload: [exn],
                    exnref: None,
                    handler:,
                  ),
                ]),
              )
            done_nd(e, region)
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
      host_unit_(e, "iter_close", [ir.Var(it), e.consts.false_], next)
    })
  }
  use body_tree <- state.map_tree(after_iter(e))
  ir.Let(
    [it],
    ir.CallHost("js", "get_iterator", [rhs_v, ir.ConstAtom("sync")]),
    body_tree,
  )
}

fn emit_try(
  e: Emitter2,
  block: List(ast.StmtWithLine),
  tail: ast.TryTail,
  next: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case tail {
    ast.TryCatch(ast.CatchClause(param, catch_body)) -> {
      let carried =
        assigned_unboxed_slots_all(e, [
          ast.BlockStatement(block),
          ast.BlockStatement(catch_body),
        ])
      let branch_slots = e.slot_vars
      let #(esc, e) = state.fresh_escape(e, list.length(carried))
      let e = state.push_barrier(e, None, None, Some(esc))
      use #(try_body, e) <- result.try(
        run_rk(e, fn(e, done) {
          use e <- emit_block(e, block)
          done(e, ir.Values(carried_values(e, carried)))
        }),
      )
      let e = state.pop_frame(e)
      let e = state.Emitter2(..e, slot_vars: branch_slots)
      use #(handler, e) <- result.try(emit_catch_handler(
        e,
        param,
        catch_body,
        carried,
      ))
      let #(region, e) =
        state.land_escapes(
          e,
          esc,
          ir.Try(result: carried_types(carried), body: try_body, handlers: [
            ir.CatchHandler(
              on: ir.OnTag(e.consts.js_tag),
              payload: ["_e"],
              exnref: None,
              handler:,
            ),
          ]),
        )
      rebind_after_block(e, carried, region, next)
    }
    // todo: outer slot writes inside try/finally are dropped here
    ast.TryFinally(finalizer) -> {
      let slot_vars0 = e.slot_vars
      use #(tree, e) <- result.try(
        exn.emit_try_finally(e, block, finalizer, fn(ef) {
          Ok(#(ir.Values([]), ef))
        }),
      )
      let e = state.Emitter2(..e, slot_vars: slot_vars0)
      use rest <- state.map_tree(next(e))
      ir.Let([], tree, rest)
    }
    ast.TryCatchFinally(handler, finalizer) -> {
      let slot_vars0 = e.slot_vars
      use #(tree, e) <- result.try(
        exn.emit_try_catch_finally(e, block, handler, finalizer, fn(ef) {
          Ok(#(ir.Values([]), ef))
        }),
      )
      let e = state.Emitter2(..e, slot_vars: slot_vars0)
      use rest <- state.map_tree(next(e))
      ir.Let([], tree, rest)
    }
  }
}

// no catch scope without a binding; entering would steal a sibling id
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
      use e <- exn.catch_binding_prologue(e, e.cur_scope)
      use #(dtree, e) <- result.try(e.dispatch.emit_destructure(
        e,
        p,
        ir.Var("_e"),
        state.BindLet,
      ))
      use e, _ <- let_(e, dtree)
      use #(body, e) <- result.try(
        run_rk(e, fn(e, done) {
          use e <- emit_block(e, catch_body)
          done(e, ir.Values(carried_values(e, carried)))
        }),
      )
      done(state.leave_scope(e, save), body)
    }
    None -> {
      use e <- emit_block(e, catch_body)
      done(e, ir.Values(carried_values(e, carried)))
    }
  }
}

fn emit_class_decl(
  e: Emitter2,
  name: Option(ast.NamedBinding),
  super_class: Option(ast.Expression),
  body: List(ast.ClassElement),
  next: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
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
      let b = cur_scope_binding(e, n)
      let e =
        state.Emitter2(..e, initialized: set.insert(e.initialized, b.slot))
      store_slot(e, b, ctor_h, next)
    }
    None -> Error(state.EarlySyntaxError("anonymous class declaration"))
  }
}

fn emit_if(
  e: Emitter2,
  condition: ast.Expression,
  consequent: ast.Statement,
  alternate: Option(ast.Statement),
  next: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let cons = block_wrap_fn_decl(consequent)
  let alt = option.map(alternate, block_wrap_fn_decl)
  let carried = case alt {
    Some(a) -> assigned_unboxed_slots_all(e, [cons, a])
    None -> assigned_unboxed_slots(e, cons)
  }
  use e, ci <- emit_cond_i32(e, condition)
  let branch_slots = e.slot_vars
  use #(then_tree, e) <- result.try(
    run_rk(e, fn(e, done) {
      use e <- emit_stmt(e, cons)
      done(e, ir.Values(carried_values(e, carried)))
    }),
  )
  let e = state.Emitter2(..e, slot_vars: branch_slots)
  use #(else_tree, e) <- result.try(case alt {
    Some(a) ->
      run_rk(e, fn(e, done) {
        use e <- emit_stmt(e, a)
        done(e, ir.Values(carried_values(e, carried)))
      })
    None -> Ok(#(ir.Values(carried_values(e, carried)), e))
  })
  let e = state.Emitter2(..e, slot_vars: branch_slots)
  rebind_after_block(
    e,
    carried,
    ir.If(ci, carried_types(carried), then_tree, else_tree),
    next,
  )
}

fn emit_labeled(
  e: Emitter2,
  label: String,
  body: ast.Statement,
  next: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case body {
    ast.WhileStatement(..)
    | ast.DoWhileStatement(..)
    | ast.ForStatement(..)
    | ast.ForInStatement(..)
    | ast.ForOfStatement(..) -> {
      let e = state.set_pending_label(e, label)
      emit_stmt(e, body, next)
    }
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

type CaseEntry {
  CaseEntry(
    lbl: String,
    cond: Option(ast.Expression),
    body: List(ast.StmtWithLine),
  )
}

// fallthrough: case bodies nest inside-out around the dispatch
fn emit_switch(
  e: Emitter2,
  disc: ast.Expression,
  cases: List(ast.SwitchCase),
  next: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  use e, d <- expr_(e, disc)
  let all_stmts = ast_util.switch_case_stmts(cases)
  let carried =
    assigned_unboxed_slots_all(e, list.map(all_stmts, fn(s) { s.statement }))
  let #(break_lbl, e) = state.fresh_label(e)
  let e = state.push_switch(e, break_lbl, carried)
  let #(e, save) = state.enter_scope(e, in_block: True)
  use e <- binding_prologue(e, e.cur_scope)
  use e <- hoist_fn_decls(e, all_stmts)
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
  let branch_slots = e.slot_vars
  // no i32 fast path: a non-int discriminant must fall to default
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
      switch_test_chain(e, d, rest, carried, miss)
    [CaseEntry(lbl:, cond: Some(test_expr), ..), ..rest] -> {
      let #(eq_tree, e) = anf.run(expr.case_test_i32(d, test_expr), e)
      use e, eqi <- let_(e, eq_tree)
      use #(else_chain, e) <- result.map(switch_test_chain(
        e,
        d,
        rest,
        carried,
        miss,
      ))
      let hit = ir.Break(lbl, carried_values(e, carried))
      #(ir.If(eqi, [], hit, else_chain), e)
    }
  }
}

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
      let e = state.Emitter2(..e, slot_vars: branch_slots)
      use #(wrapped, e) <- result.try(
        rebind_after_block(
          e,
          carried,
          ir.Block(lbl, carried_types(carried), inner),
          fn(e) {
            use ef <- each_(e, body, with: fn(e, located, k) {
              emit_stmt(e, located.statement, k)
            })
            case rest {
              [] -> Ok(#(ir.Break(break_lbl, carried_values(ef, carried)), ef))
              _ -> Ok(#(ir.Values(carried_values(ef, carried)), ef))
            }
          },
        ),
      )
      switch_nest_bodies(e, branch_slots, rest, wrapped, break_lbl, carried)
    }
  }
}

fn resolve_per_iter(e: Emitter2, names: List(String)) -> List(#(Int, Bool)) {
  use name <- list.filter_map(names)
  case state.resolve(e, name) {
    scope.Plain(scope.Local(slot:, boxed:, ..)) -> Ok(#(slot, boxed))
    _ -> Error(Nil)
  }
}

// §14.7.4.2 rebox boxed head lets per iteration
fn per_iter_rebox(
  e: Emitter2,
  per_iter: List(#(Int, Bool)),
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  use e, #(slot, boxed), next <- each_(e, per_iter, then: k)
  case boxed {
    False -> next(e)
    True -> {
      let old = ir.Var(state.get_slot_var(e, slot))
      use e, v <- host_(e, "cell_get", [old])
      use e, cell <- host_(e, "cell_new", [v])
      let #(n, e) = state.fresh_slot_var(e, slot)
      use body <- state.map_tree(next(state.set_slot_var(e, slot, n)))
      ir.Let([n], ir.Values([cell]), body)
    }
  }
}

fn emit_for_classic(
  e: Emitter2,
  init: Option(ast.ForInit),
  cond: Option(ast.Expression),
  upd: Option(ast.Expression),
  body: ast.Statement,
  next: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let has_lex = ast_util.for_classic_init_is_lex(init)
  let #(e, save) = state.enter_for_scope(e, has_lex)
  let seed_head = fn(e: Emitter2, k) {
    case has_lex {
      True -> binding_prologue(e, e.cur_scope, k)
      False -> k(e)
    }
  }
  use e <- seed_head(e)
  let after_init = fn(e: Emitter2, per_iter_names: List(String)) {
    let per_iter = resolve_per_iter(e, per_iter_names)
    let per_iter_slots = list.map(per_iter, fn(p) { p.0 })
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
    let #(params, e) = carried_params(e, carried)
    let counter_known = for_counter_known(e, upd, body, cond, carried, params)
    let mark_counter = fn(e: Emitter2) {
      list.fold(counter_known, e, fn(e, slot) {
        state.mark_known_number(e, state.get_slot_var(e, slot))
      })
    }
    let hoist_slots = loop_invariant_callees(e, body, cond, upd, carried)
    let prev_hoisted = e.hoisted_kfn
    use e <- hoist_kfn_codes(e, hoist_slots)
    let e = state.push_loop(e, brk, cont, carried, None)
    let emit_upd = fn(e: Emitter2, k) {
      case upd {
        Some(u) -> expr_(e, u, fn(e, _) { k(e) })
        None -> k(e)
      }
    }
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
            let e = mark_counter(e)
            // §14.7.4.3 rebox after body, before upd
            use e <- per_iter_rebox(e, per_iter)
            use e <- emit_upd(e)
            d2(e, ir.Continue(head, carried_values(e, carried)))
          })
        }
        case cond {
          None -> {
            use #(tt, e) <- result.try(then_part(e))
            done(e, tt)
          }
          Some(c) -> {
            use e, t <- emit_cond_i32(e, c)
            let brk_payload = carried_values(e, carried)
            use #(tt, e) <- result.try(then_part(e))
            done(e, ir.If(t, [], tt, ir.Break(brk, brk_payload)))
          }
        }
      }),
    )
    let e = state.pop_frame(e)
    let e = state.Emitter2(..e, hoisted_kfn: prev_hoisted)
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
    Some(ast.ForInitPattern(_)) | None -> after_init(e, [])
  }
}

// sound: init is a number, only upd ++/-- writes it
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

fn emit_cond_i32(
  e: Emitter2,
  cond: ast.Expression,
  k: Rk(ir.Value),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let #(tree, e) = anf.run(expr.cond_i32(cond), e)
  let_(e, tree, k)
}

fn emit_while(
  e: Emitter2,
  cond: ast.Expression,
  body: ast.Statement,
  next: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let carried =
    assigned_unboxed_slots_all(e, [ast.ExpressionStatement(cond, None), body])
  let result_tys = carried_types(carried)
  let #(brk, e) = state.fresh_label(e)
  let #(cont, e) = state.fresh_label(e)
  let #(head, e) = state.fresh_label(e)
  let #(params, e) = carried_params(e, carried)
  let hoist_slots = loop_invariant_callees(e, body, Some(cond), None, carried)
  let prev_hoisted = e.hoisted_kfn
  use e <- hoist_kfn_codes(e, hoist_slots)
  let e = state.push_loop(e, brk, cont, carried, None)
  use #(loop_body, e) <- result.try(
    run_rk(e, fn(e, done) {
      let e = enter_loop_body(e, carried, params)
      use e, t <- emit_cond_i32(e, cond)
      let brk_payload = carried_values(e, carried)
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
  let e = state.Emitter2(..e, hoisted_kfn: prev_hoisted)
  let outer = ir.Block(brk, result_tys, ir.Loop(head, params, [], loop_body))
  rebind_after_block(e, carried, outer, next)
}

fn emit_do_while(
  e: Emitter2,
  cond: ast.Expression,
  body: ast.Statement,
  next: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
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
      use e, t <- emit_cond_i32(e, cond)
      let payload = carried_values(e, carried)
      done(e, ir.If(t, [], ir.Continue(head, payload), ir.Break(brk, payload)))
    }),
  )
  let e = state.pop_frame(e)
  let outer = ir.Block(brk, result_tys, ir.Loop(head, params, [], loop_body))
  rebind_after_block(e, carried, outer, next)
}
