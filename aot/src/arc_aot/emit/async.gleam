//// regenerator-style state machine transform for async and generator bodies

import arc/compiler/ast_util
import arc/compiler/scope.{type ScopeId, type ScopeTree}
import arc/parser/ast
import arc_aot/emit/anf
import arc_aot/emit/class
import arc_aot/emit/func
import arc_aot/emit/state.{type Emitter2}
import carder/ir
import gleam/bit_array
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string

// arms emit out of source order, so each snapshots its entry scope cursor
pub type ArmCursor {
  ArmCursor(
    cur_scope: ScopeId,
    scope_cursor: List(ScopeId),
    child_fn_cursor: List(ScopeId),
  )
}

pub fn root_cursor(tree: ScopeTree, fn_scope: ScopeId) -> ArmCursor {
  ArmCursor(
    cur_scope: fn_scope,
    scope_cursor: state.block_child_scopes(tree, fn_scope),
    child_fn_cursor: scope.child_function_scopes(tree, fn_scope),
  )
}

pub fn cursor_enter_scope(
  tree: ScopeTree,
  c: ArmCursor,
) -> #(ArmCursor, ArmCursor) {
  case c.scope_cursor {
    [child, ..rest] -> #(
      ArmCursor(
        cur_scope: child,
        scope_cursor: state.block_child_scopes(tree, child),
        child_fn_cursor: c.child_fn_cursor,
      ),
      ArmCursor(..c, scope_cursor: rest),
    )
    [] -> #(c, c)
  }
}

pub fn cursor_pop_child_fn(c: ArmCursor) -> ArmCursor {
  case c.child_fn_cursor {
    [_, ..rest] -> ArmCursor(..c, child_fn_cursor: rest)
    [] -> c
  }
}

pub fn cursor_leave_scope(
  resume: ArmCursor,
  inner_after: ArmCursor,
) -> ArmCursor {
  ArmCursor(..resume, child_fn_cursor: inner_after.child_fn_cursor)
}

pub fn install_cursor(e: Emitter2, c: ArmCursor) -> Emitter2 {
  state.Emitter2(
    ..e,
    cur_scope: c.cur_scope,
    scope_cursor: c.scope_cursor,
    child_fn_cursor: c.child_fn_cursor,
  )
}

pub fn capture_cursor(e: Emitter2) -> ArmCursor {
  ArmCursor(
    cur_scope: e.cur_scope,
    scope_cursor: e.scope_cursor,
    child_fn_cursor: e.child_fn_cursor,
  )
}

pub type SplitKind {
  SkAwait
  SkYield
  SkYieldStar
  SkForAwait
}

pub type SplitPoint {
  SplitPoint(id: Int, kind: SplitKind, enclosing_try: Option(Int))
}

pub type TryEntry {
  TryEntry(
    id: Int,
    // a catch-body view shares its parent try's pending slot
    pending_id: Int,
    catch_state: Option(Int),
    finally_state: Option(Int),
    after_state: Int,
    pending_loc_idx: Int,
    caught_loc_idx: Int,
    outer: Option(Int),
    handler: Option(ast.CatchClause),
    finalizer: Option(List(ast.StmtWithLine)),
    catch_cursor: Option(ArmCursor),
    finally_cursor: Option(ArmCursor),
    sm_labels: List(SmLabel),
  )
}

pub type DelegateSpec {
  DelegateSpec(
    state_id: Int,
    next_state: Int,
    region: Option(Int),
    await_state: Option(Int),
  )
}

pub type ForAwaitSpec {
  ForAwaitSpec(
    head: Int,
    check: Int,
    body_s: Int,
    after: Int,
    left: ast.ForInit,
    body_cursor: ArmCursor,
    region: Option(Int),
  )
}

pub type LocLayout {
  LocLayout(
    slot_to_idx: Dict(Int, Int),
    size: Int,
    extras: Dict(String, Int),
    initial_values: List(ir.Value),
  )
}

pub type ArmEntry {
  AeInitial
  AeResume(kind: SplitKind)
  AeJump
}

pub type ResumeWith {
  ResumeDiscard
  ResumeBind(pat: ast.Pattern, mode: state.BindMode)
  ResumeReturn
  ResumeThrow
  ResumeWithScope(body: ast.Statement, line: Int)
  ResumeCatch(try_id: Int, param: Option(ast.Pattern))
}

pub type SegTail {
  FallTo(to: Int)
  FallToFinally(try_id: Int, to: Int)
  FinallyEnd(try_id: Int)
  SplitAt(kind: SplitKind, arg: Option(ast.Expression), ns: Int)
  CondBranch(cond: Option(ast.Expression), then_s: Int, else_s: Int)
  ForUpdate(update: Option(ast.Expression), head: Int)
  ForOfSetup(right: ast.Expression, iter_key: String, head: Int)
  ForOfStep(
    left: ast.ForInit,
    iter_key: String,
    body_s: Int,
    after: Int,
    is_await: Bool,
  )
  SwitchDispatch(
    disc: Option(ast.Expression),
    tests: List(#(Option(ast.Expression), Int)),
    after: Int,
  )
  ForAwaitSetup(right: ast.Expression, head: Int)
  AsyncGenYieldSent(ns: Int)
  BodyEnd
  SegDone
}

pub type ArmSpec {
  ArmSpec(
    state_id: Int,
    region: Option(Int),
    entry_kind: ArmEntry,
    entry_cursor: ArmCursor,
    resume: Option(ResumeWith),
    body_fragment: List(ast.StmtWithLine),
    tail: SegTail,
    sm_labels: List(SmLabel),
  )
}

pub type SplitPlan {
  SplitPlan(
    n_states: Int,
    n_temps: Int,
    arms: List(ArmSpec),
    try_entries: List(TryEntry),
    delegates: List(DelegateSpec),
    for_awaits: List(ForAwaitSpec),
  )
}

pub type SmLabel {
  SmLoop(
    js_label: Option(String),
    brk_sentinel: String,
    cont_sentinel: String,
    break_state: Int,
    continue_state: Int,
    enclosing_try: Option(Int),
  )
  SmSwitch(
    js_label: Option(String),
    brk_sentinel: String,
    break_state: Int,
    enclosing_try: Option(Int),
  )
  SmLabeled(
    js_label: String,
    brk_sentinel: String,
    break_state: Int,
    enclosing_try: Option(Int),
  )
}

pub type SmCtx {
  SmCtx(
    kind: state.CoroutineKind,
    layout: LocLayout,
    lresume: String,
    mode_v: ir.Value,
    sent_v: ir.Value,
    loc_v: ir.Value,
    try_entries: List(TryEntry),
    next_state: Int,
    arms: List(ir.SwitchArm),
    try_stack: List(TryEntry),
    sm_labels: List(SmLabel),
  )
}

pub fn new_sm_ctx(
  kind: state.CoroutineKind,
  layout: LocLayout,
  lresume: String,
  plan: SplitPlan,
) -> SmCtx {
  SmCtx(
    kind: kind,
    layout: layout,
    lresume: lresume,
    mode_v: ir.Var("_mode"),
    sent_v: ir.Var("_sv"),
    loc_v: ir.Var("_loc_i"),
    try_entries: plan.try_entries,
    next_state: plan.n_states,
    arms: [],
    try_stack: [],
    sm_labels: [],
  )
}

pub fn sm_alloc_state(ctx: SmCtx) -> #(Int, SmCtx) {
  #(ctx.next_state, SmCtx(..ctx, next_state: ctx.next_state + 1))
}

pub fn push_arm(ctx: SmCtx, n: Int, body: ir.Expr) -> SmCtx {
  SmCtx(..ctx, arms: [ir.SwitchArm(n, body), ..ctx.arms])
}

pub fn finish_arms(ctx: SmCtx) -> List(ir.SwitchArm) {
  list.reverse(ctx.arms)
}

pub fn push_try(ctx: SmCtx, region: TryEntry) -> SmCtx {
  SmCtx(..ctx, try_stack: [region, ..ctx.try_stack])
}

pub fn pop_try(ctx: SmCtx) -> SmCtx {
  case ctx.try_stack {
    [_, ..rest] -> SmCtx(..ctx, try_stack: rest)
    [] -> panic as "async.pop_try: try_stack empty"
  }
}

pub fn current_try(ctx: SmCtx) -> Option(TryEntry) {
  case ctx.try_stack {
    [top, ..] -> Some(top)
    [] -> None
  }
}

pub fn push_label(ctx: SmCtx, label: SmLabel) -> SmCtx {
  SmCtx(..ctx, sm_labels: [label, ..ctx.sm_labels])
}

pub fn pop_label(ctx: SmCtx) -> SmCtx {
  case ctx.sm_labels {
    [_, ..rest] -> SmCtx(..ctx, sm_labels: rest)
    [] -> panic as "async.pop_label: sm_labels empty"
  }
}

pub fn with_region(ctx: SmCtx, region: Option(Int)) -> SmCtx {
  SmCtx(..ctx, try_stack: try_chain(ctx.try_entries, region))
}

// a throw in a catch body must not re-enter its own catch
pub fn with_catch_body(ctx: SmCtx, entry: TryEntry) -> SmCtx {
  let outer = try_chain(ctx.try_entries, entry.outer)
  let stack = case entry.finally_state {
    Some(_) -> [TryEntry(..entry, catch_state: None), ..outer]
    None -> outer
  }
  SmCtx(..ctx, try_stack: stack)
}

pub fn with_finally_body(ctx: SmCtx, entry: TryEntry) -> SmCtx {
  SmCtx(..ctx, try_stack: try_chain(ctx.try_entries, entry.outer))
}

fn try_chain(entries: List(TryEntry), region: Option(Int)) -> List(TryEntry) {
  case region {
    None -> []
    Some(id) ->
      case list.find(entries, fn(t) { t.id == id }) {
        Ok(entry) -> [entry, ..try_chain(entries, entry.outer)]
        Error(_) -> []
      }
  }
}

pub type PendingKind {
  PkReturn(ir.Value)
  PkThrow(ir.Value)
  PkGoto(target: Int)
}

// ints not atoms so the i32 test can compare them
const pend_throw = 1

const pend_return = 2

const pend_goto = 3

fn pending_tuple(pk: PendingKind) -> ir.Expr {
  case pk {
    PkReturn(v) -> ir.TermOp(ir.MakeTuple, [ir.ConstI32(pend_return), v])
    PkThrow(v) -> ir.TermOp(ir.MakeTuple, [ir.ConstI32(pend_throw), v])
    PkGoto(target) ->
      ir.TermOp(ir.MakeTuple, [ir.ConstI32(pend_goto), ir.ConstI32(target)])
  }
}

fn sm_continue(ctx: SmCtx, target: Int, loc: ir.Value) -> ir.Expr {
  ir.Continue(ctx.lresume, [ir.ConstI32(target), loc])
}

fn pack_loc_cps(
  e: Emitter2,
  ctx: SmCtx,
  overrides: Dict(Int, ir.Value),
  k: fn(Emitter2, ir.Value) -> #(ir.Expr, Emitter2),
) -> #(ir.Expr, Emitter2) {
  pack_loc_from(e, ctx, overrides, 0, [], k)
}

fn pack_loc_from(
  e: Emitter2,
  ctx: SmCtx,
  overrides: Dict(Int, ir.Value),
  i: Int,
  acc: List(ir.Value),
  k: fn(Emitter2, ir.Value) -> #(ir.Expr, Emitter2),
) -> #(ir.Expr, Emitter2) {
  case i >= ctx.layout.size {
    True -> {
      let #(name, e) = state.fresh_var(e)
      anf.wrap(k(e, ir.Var(name)), ir.Let(
        [name],
        ir.TermOp(ir.MakeTuple, list.reverse(acc)),
        _,
      ))
    }
    False ->
      case dict.get(overrides, i) {
        Ok(v) -> pack_loc_from(e, ctx, overrides, i + 1, [v, ..acc], k)
        Error(_) ->
          case slot_at_loc_idx(ctx.layout, i) {
            Some(slot) -> {
              let v = ir.Var(state.get_slot_var(e, slot))
              pack_loc_from(e, ctx, overrides, i + 1, [v, ..acc], k)
            }
            None -> {
              let #(name, e) = state.fresh_var(e)
              anf.wrap(
                pack_loc_from(
                  e,
                  ctx,
                  overrides,
                  i + 1,
                  [ir.Var(name), ..acc],
                  k,
                ),
                ir.Let([name], ir.TermOp(ir.TupleGet(i), [ctx.loc_v]), _),
              )
            }
          }
      }
  }
}

fn slot_at_loc_idx(layout: LocLayout, idx: Int) -> Option(Int) {
  case dict.get(layout.slot_to_idx, idx) {
    Ok(at) if at == idx -> Some(idx)
    _ ->
      dict.fold(layout.slot_to_idx, None, fn(found, slot, at) {
        case found, at == idx {
          None, True -> Some(slot)
          _, _ -> found
        }
      })
  }
}

pub fn route_abrupt(
  e: Emitter2,
  ctx: SmCtx,
  pk: PendingKind,
  stop_at: Option(Int),
) -> #(ir.Expr, Emitter2) {
  route_abrupt_walk(e, ctx, ctx.try_stack, pk, stop_at)
}

fn route_abrupt_walk(
  e: Emitter2,
  ctx: SmCtx,
  stack: List(TryEntry),
  pk: PendingKind,
  stop_at: Option(Int),
) -> #(ir.Expr, Emitter2) {
  case stack {
    [] -> route_abrupt_tail(e, ctx, pk)
    [entry, ..rest] ->
      case stop_at == Some(entry.id) {
        True -> route_abrupt_tail(e, ctx, pk)
        False ->
          case entry.finally_state {
            Some(fs) -> {
              let #(pv, e) = state.fresh_var(e)
              let over = dict.from_list([#(entry.pending_loc_idx, ir.Var(pv))])
              anf.wrap(
                pack_loc_cps(e, ctx, over, fn(e, loc) {
                  #(sm_continue(ctx, fs, loc), e)
                }),
                ir.Let([pv], pending_tuple(pk), _),
              )
            }
            None ->
              case pk {
                PkThrow(v) ->
                  case entry.catch_state {
                    Some(cs) -> {
                      let over = dict.from_list([#(entry.caught_loc_idx, v)])
                      pack_loc_cps(e, ctx, over, fn(e, loc) {
                        #(sm_continue(ctx, cs, loc), e)
                      })
                    }
                    None -> route_abrupt_walk(e, ctx, rest, pk, stop_at)
                  }
                _ -> route_abrupt_walk(e, ctx, rest, pk, stop_at)
              }
          }
      }
  }
}

fn route_abrupt_tail(
  e: Emitter2,
  ctx: SmCtx,
  pk: PendingKind,
) -> #(ir.Expr, Emitter2) {
  case pk {
    PkReturn(v) -> #(step_return(v), e)
    PkThrow(v) -> #(step_throw(v), e)
    PkGoto(target) ->
      pack_loc_cps(e, ctx, dict.new(), fn(e, loc) {
        #(sm_continue(ctx, target, loc), e)
      })
  }
}

fn sentinel_match(labels: List(SmLabel), ir_label: String) -> Option(SmLabel) {
  list.find(labels, fn(l) {
    case l {
      SmLoop(brk_sentinel: b, cont_sentinel: c, ..) ->
        ir_label == b || ir_label == c
      SmSwitch(brk_sentinel: b, ..) | SmLabeled(brk_sentinel: b, ..) ->
        ir_label == b
    }
  })
  |> option.from_result
}

fn sentinel_target(l: SmLabel, ir_label: String) -> #(Int, Option(Int)) {
  case l {
    SmLoop(
      brk_sentinel: b,
      break_state: bs,
      continue_state: cs,
      enclosing_try: et,
      ..,
    ) ->
      case ir_label == b {
        True -> #(bs, et)
        False -> #(cs, et)
      }
    SmSwitch(break_state: bs, enclosing_try: et, ..) -> #(bs, et)
    SmLabeled(break_state: bs, enclosing_try: et, ..) -> #(bs, et)
  }
}

fn make_on_return(
  ctx: SmCtx,
) -> fn(Emitter2, ir.Value) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  fn(e, v) { Ok(route_abrupt(e, ctx, PkReturn(v), None)) }
}

fn make_on_goto(
  ctx: SmCtx,
) -> fn(Emitter2, String) ->
  Option(Result(#(ir.Expr, Emitter2), state.EmitError)) {
  fn(e, ir_label) {
    case sentinel_match(ctx.sm_labels, ir_label) {
      None -> None
      Some(label) -> {
        let #(target, stop) = sentinel_target(label, ir_label)
        Some(Ok(route_abrupt(e, ctx, PkGoto(target), stop)))
      }
    }
  }
}

pub fn with_abrupt_intercept(
  e: Emitter2,
  ctx: SmCtx,
  body: fn(Emitter2, fn(Emitter2) -> Emitter2) -> a,
) -> a {
  let #(e, n_pushed) = push_sm_frames(e, ctx.sm_labels)
  let e =
    state.set_sm_abrupt(
      e,
      state.SmAbrupt(on_return: make_on_return(ctx), on_goto: make_on_goto(ctx)),
    )
  let restore = fn(e: Emitter2) {
    pop_n_frames(state.clear_sm_abrupt(e), n_pushed)
  }
  body(e, restore)
}

fn push_sm_frames(e: Emitter2, labels: List(SmLabel)) -> #(Emitter2, Int) {
  list.fold(list.reverse(labels), #(e, 0), fn(acc, lab) {
    let #(e, n) = acc
    let frame = case lab {
      SmLoop(js_label:, brk_sentinel:, cont_sentinel:, ..) ->
        state.Loop2(
          ir_break: brk_sentinel,
          ir_continue: cont_sentinel,
          js_label:,
          carried: [],
          iter_close: None,
        )
      SmSwitch(js_label:, brk_sentinel:, ..) ->
        state.Switch2(ir_break: brk_sentinel, js_label:, carried: [])
      SmLabeled(js_label:, brk_sentinel:, ..) ->
        state.Labeled2(ir_break: brk_sentinel, js_label:, carried: [])
    }
    #(state.push_frame(e, frame), n + 1)
  })
}

fn pop_n_frames(e: Emitter2, n: Int) -> Emitter2 {
  case n {
    0 -> e
    _ -> pop_n_frames(state.pop_frame(e), n - 1)
  }
}

pub fn expr_has_split(e: ast.Expression) -> Bool {
  case e {
    ast.AwaitExpression(..) | ast.YieldExpression(..) -> True
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
    | ast.IntrinsicTemplateObject(..) -> False
    ast.FunctionExpression(..) | ast.ArrowFunctionExpression(..) -> False
    ast.UnaryExpression(argument: a, ..)
    | ast.UpdateExpression(argument: a, ..)
    | ast.SpreadElement(argument: a, ..)
    | ast.ParenthesizedExpression(expression: a, ..) -> expr_has_split(a)
    ast.BinaryExpression(left: l, right: r, ..)
    | ast.LogicalExpression(left: l, right: r, ..)
    | ast.AssignmentExpression(left: l, right: r, ..) ->
      expr_has_split(l) || expr_has_split(r)
    ast.MemberExpression(object: o, property: p, ..)
    | ast.OptionalMemberExpression(object: o, property: p, ..) ->
      expr_has_split(o) || member_prop_has_split(p)
    ast.CallExpression(callee: c, arguments: args, ..)
    | ast.OptionalCallExpression(callee: c, arguments: args, ..)
    | ast.NewExpression(callee: c, arguments: args, ..) ->
      expr_has_split(c) || list.any(args, expr_has_split)
    ast.ConditionalExpression(condition: c, consequent: t, alternate: f, ..) ->
      expr_has_split(c) || expr_has_split(t) || expr_has_split(f)
    ast.SequenceExpression(expressions: xs, ..) -> list.any(xs, expr_has_split)
    ast.ArrayExpression(elements: xs, ..) ->
      list.any(xs, fn(o) { opt_expr_has_split(o) })
    ast.ObjectExpression(properties: ps, ..) -> list.any(ps, prop_has_split)
    ast.TemplateLiteral(parts: parts, ..) ->
      list.any(ast.template_expressions(parts), expr_has_split)
    ast.TaggedTemplateExpression(tag: t, parts: parts, ..) ->
      expr_has_split(t)
      || list.any(ast.template_expressions(parts), expr_has_split)
    ast.ImportExpression(source: s, options: o, ..) ->
      expr_has_split(s) || opt_expr_has_split(o)
    ast.ClassExpression(super_class: sc, body: elems, ..) ->
      opt_expr_has_split(sc) || list.any(elems, class_elem_has_split)
  }
}

fn member_prop_has_split(p: ast.MemberProperty) -> Bool {
  case p {
    ast.Dot(..) -> False
    ast.Bracket(expression: e) -> expr_has_split(e)
  }
}

fn opt_expr_has_split(o: Option(ast.Expression)) -> Bool {
  case o {
    Some(e) -> expr_has_split(e)
    None -> False
  }
}

fn prop_has_split(p: ast.Property) -> Bool {
  case p {
    ast.InitProperty(key: k, value: v, ..) ->
      key_has_split(k) || expr_has_split(v)
    ast.MethodProperty(key: k, ..) | ast.AccessorProperty(key: k, ..) ->
      key_has_split(k)
    ast.SpreadProperty(argument: a) -> expr_has_split(a)
  }
}

fn key_has_split(k: ast.PropertyKey) -> Bool {
  case k {
    ast.KeyComputed(expression: e) -> expr_has_split(e)
    ast.KeyIdentifier(..)
    | ast.KeyString(..)
    | ast.KeyNumber(..)
    | ast.KeyBigInt(..)
    | ast.KeyPrivate(..) -> False
  }
}

fn class_elem_has_split(ce: ast.ClassElement) -> Bool {
  case ce {
    ast.ClassMethod(key: k, ..) -> key_has_split(k)
    ast.ClassField(key: k, ..) -> key_has_split(k)
    ast.StaticBlock(..) -> False
  }
}

fn pattern_has_split(p: ast.Pattern) -> Bool {
  case p {
    ast.IdentifierPattern(..) -> False
    ast.AssignmentPattern(left: l, right: r) ->
      pattern_has_split(l) || expr_has_split(r)
    ast.RestElement(argument: a) -> pattern_has_split(a)
    ast.ArrayPattern(elements: xs) ->
      list.any(xs, fn(o) {
        case o {
          Some(x) -> pattern_has_split(x)
          None -> False
        }
      })
    ast.ObjectPattern(properties: ps) ->
      list.any(ps, fn(pp) {
        case pp {
          ast.PatternProperty(key: k, value: v, ..) ->
            key_has_split(k) || pattern_has_split(v)
          ast.RestProperty(..) -> False
        }
      })
  }
}

fn for_init_has_split(fi: ast.ForInit) -> Bool {
  case fi {
    ast.ForInitExpression(e) -> expr_has_split(e)
    ast.ForInitDeclaration(declarations: ds, ..) ->
      list.any(ds, declarator_has_split)
    ast.ForInitPattern(p) -> pattern_has_split(p)
  }
}

fn declarator_has_split(d: ast.VariableDeclarator) -> Bool {
  pattern_has_split(d.id) || opt_expr_has_split(d.init)
}

pub fn stmt_has_split(s: ast.Statement) -> Bool {
  case s {
    ast.EmptyStatement
    | ast.DebuggerStatement
    | ast.BreakStatement(..)
    | ast.ContinueStatement(..) -> False
    ast.FunctionDeclaration(..) -> False
    ast.ClassDeclaration(super_class: sc, body: elems, ..) ->
      opt_expr_has_split(sc) || list.any(elems, class_elem_has_split)
    ast.ExpressionStatement(expression: e, ..)
    | ast.ThrowStatement(argument: e) -> expr_has_split(e)
    ast.ReturnStatement(argument: a) -> opt_expr_has_split(a)
    ast.BlockStatement(body: b) -> stmts_have_split(b)
    ast.VariableDeclaration(declarations: ds, ..) ->
      list.any(ds, declarator_has_split)
    ast.IfStatement(condition: c, consequent: t, alternate: f) ->
      expr_has_split(c)
      || stmt_has_split(t)
      || case f {
        Some(a) -> stmt_has_split(a)
        None -> False
      }
    ast.WhileStatement(condition: c, body: b)
    | ast.DoWhileStatement(condition: c, body: b) ->
      expr_has_split(c) || stmt_has_split(b)
    ast.ForStatement(init: i, condition: c, update: u, body: b) ->
      case i {
        Some(fi) -> for_init_has_split(fi)
        None -> False
      }
      || opt_expr_has_split(c)
      || opt_expr_has_split(u)
      || stmt_has_split(b)
    ast.ForInStatement(left: l, right: r, body: b) ->
      for_init_has_split(l) || expr_has_split(r) || stmt_has_split(b)
    ast.ForOfStatement(left: l, right: r, body: b, is_await: aw) ->
      aw || for_init_has_split(l) || expr_has_split(r) || stmt_has_split(b)
    ast.SwitchStatement(discriminant: d, cases: cs) ->
      expr_has_split(d)
      || list.any(cs, fn(c: ast.SwitchCase) {
        opt_expr_has_split(c.condition) || stmts_have_split(c.consequent)
      })
    ast.TryStatement(block: b, tail: t) ->
      stmts_have_split(b) || try_tail_has_split(t)
    ast.LabeledStatement(body: b, ..) -> stmt_has_split(b)
    ast.WithStatement(object: o, body: b) ->
      expr_has_split(o) || stmt_has_split(b)
  }
}

fn try_tail_has_split(t: ast.TryTail) -> Bool {
  case t {
    ast.TryCatch(handler: h) -> catch_has_split(h)
    ast.TryFinally(finalizer: f) -> stmts_have_split(f)
    ast.TryCatchFinally(handler: h, finalizer: f) ->
      catch_has_split(h) || stmts_have_split(f)
  }
}

fn catch_has_split(c: ast.CatchClause) -> Bool {
  case c.param {
    Some(p) -> pattern_has_split(p)
    None -> False
  }
  || stmts_have_split(c.body)
}

fn stmts_have_split(ss: List(ast.StmtWithLine)) -> Bool {
  list.any(ss, fn(s: ast.StmtWithLine) { stmt_has_split(s.statement) })
}

type Ana {
  Ana(
    tree: ScopeTree,
    kind: state.CoroutineKind,
    next_state: Int,
    next_try: Int,
    next_sentinel: Int,
    next_temp: Int,
    cursor_only: Bool,
    try_stack: List(Int),
    sm_labels: List(SmLabel),
    cur: ArmCursor,
    splits: List(SplitPoint),
    tries: List(TryEntry),
    arms: List(ArmSpec),
    delegates: List(DelegateSpec),
    for_awaits: List(ForAwaitSpec),
    frag_rev: List(ast.StmtWithLine),
    open_state: Int,
    open_region: Option(Int),
    open_entry: ArmEntry,
    open_cursor: ArmCursor,
    open_labels: List(SmLabel),
    open_resume: Option(ResumeWith),
  )
}

fn ana_region(a: Ana) -> Option(Int) {
  case a.try_stack {
    [top, ..] -> Some(top)
    [] -> None
  }
}

fn alloc_state(a: Ana) -> #(Int, Ana) {
  #(a.next_state, Ana(..a, next_state: a.next_state + 1))
}

fn alloc_sentinel(a: Ana) -> #(String, Ana) {
  #(
    "_Lsm" <> int.to_string(a.next_sentinel),
    Ana(..a, next_sentinel: a.next_sentinel + 1),
  )
}

fn frag_push(a: Ana, sl: ast.StmtWithLine) -> Ana {
  Ana(..a, frag_rev: [sl, ..a.frag_rev])
}

fn close_open(a: Ana, tail: SegTail, new_state: Int, entry: ArmEntry) -> Ana {
  let arm =
    ArmSpec(
      state_id: a.open_state,
      region: a.open_region,
      entry_kind: a.open_entry,
      entry_cursor: a.open_cursor,
      resume: a.open_resume,
      body_fragment: list.reverse(a.frag_rev),
      tail: tail,
      sm_labels: a.open_labels,
    )
  Ana(
    ..a,
    arms: [arm, ..a.arms],
    frag_rev: [],
    open_state: new_state,
    open_region: ana_region(a),
    open_entry: entry,
    open_cursor: a.cur,
    open_labels: a.sm_labels,
    open_resume: None,
  )
}

fn push_sm_label(a: Ana, l: SmLabel) -> Ana {
  // the open segment was opened before this push and must see it
  let sm_labels = [l, ..a.sm_labels]
  Ana(..a, sm_labels:, open_labels: sm_labels)
}

fn pop_sm_label(a: Ana) -> Ana {
  case a.sm_labels {
    [_, ..rest] -> Ana(..a, sm_labels: rest)
    [] -> a
  }
}

fn record_split(
  a: Ana,
  kind: SplitKind,
  arg: Option(ast.Expression),
  resume: Option(ResumeWith),
) -> #(Int, Ana) {
  let #(ns, a) = alloc_state(a)
  let region = ana_region(a)
  let sp = SplitPoint(id: ns, kind:, enclosing_try: region)
  let a = Ana(..a, splits: [sp, ..a.splits])
  let a = close_open(a, SplitAt(kind:, arg:, ns:), ns, AeResume(kind))
  #(ns, Ana(..a, open_resume: resume))
}

fn record_delegate(
  a: Ana,
  arg: Option(ast.Expression),
  resume: Option(ResumeWith),
) -> Ana {
  let #(nd, a) = alloc_state(a)
  let #(follow, a) = alloc_state(a)
  let region = ana_region(a)
  let sp = SplitPoint(id: nd, kind: SkYieldStar, enclosing_try: region)
  let a = Ana(..a, splits: [sp, ..a.splits])
  let a =
    close_open(
      a,
      SplitAt(kind: SkYieldStar, arg:, ns: nd),
      follow,
      AeResume(SkYieldStar),
    )
  let #(await_state, a) = case a.kind {
    state.CorAsyncGen -> {
      let #(na, a) = alloc_state(a)
      #(Some(na), a)
    }
    state.CorAsync | state.CorGenerator -> #(None, a)
  }
  let d = DelegateSpec(state_id: nd, next_state: follow, region:, await_state:)
  Ana(..a, delegates: [d, ..a.delegates], open_resume: resume)
}

// keeps the open segment's cursor right when a fragment starts or ends in this scope
fn with_scope(a: Ana, f: fn(Ana) -> Ana) -> Ana {
  let #(inner, resume) = cursor_enter_scope(a.tree, a.cur)
  use <- bool.lazy_guard(a.cursor_only, fn() {
    let a_in = f(Ana(..a, cur: inner))
    Ana(..a_in, cur: cursor_leave_scope(resume, a_in.cur))
  })
  let a = case a.frag_rev {
    [] -> Ana(..a, open_cursor: inner)
    [_, ..] -> {
      let #(fresh, a) = alloc_state(a)
      let a = close_open(a, FallTo(fresh), fresh, AeJump)
      Ana(..a, open_cursor: inner)
    }
  }
  let a_in = f(Ana(..a, cur: inner))
  let resumed = cursor_leave_scope(resume, a_in.cur)
  case a_in.open_cursor.cur_scope == inner.cur_scope {
    True ->
      case a_in.frag_rev, a_in.open_resume {
        [], None -> Ana(..a_in, cur: resumed, open_cursor: resumed)
        _, _ -> {
          let #(fresh, a_in) = alloc_state(Ana(..a_in, cur: resumed))
          close_open(a_in, FallTo(fresh), fresh, AeJump)
        }
      }
    False -> Ana(..a_in, cur: resumed)
  }
}

// false means the analyzer pruned the scope; entering would steal a sibling's id
fn with_scope_if(a: Ana, cond: Bool, f: fn(Ana) -> Ana) -> Ana {
  case cond {
    True -> with_scope(a, f)
    False -> f(a)
  }
}

fn walk_catch_cur(a: Ana, h: ast.CatchClause, f: fn(Ana) -> Ana) -> Ana {
  let body_has = ast_util.block_has_declarations(h.body)
  case h.param {
    Some(_) -> with_scope(a, fn(a) { with_scope_if(a, body_has, f) })
    None -> with_scope_if(a, body_has, f)
  }
}

fn ana_opt_expr(a: Ana, o: Option(ast.Expression), tail) -> Ana {
  case o {
    Some(e) -> ana_expr(a, e, tail)
    None -> a
  }
}

fn ana_expr(a: Ana, e: ast.Expression, tail: List(ast.StmtWithLine)) -> Ana {
  case e {
    ast.AwaitExpression(argument: arg, ..) -> {
      let a = ana_expr(a, arg, tail)
      let #(_, a) = record_split(a, SkAwait, Some(arg), None)
      a
    }
    ast.YieldExpression(argument: arg, is_delegate: del, ..) -> {
      let a = ana_opt_expr(a, arg, tail)
      case del {
        True -> record_delegate(a, arg, None)
        False ->
          case a.kind {
            state.CorAsyncGen -> {
              let #(_, a) = record_split(a, SkAwait, arg, None)
              let #(ns2, a) = alloc_state(a)
              let sp =
                SplitPoint(id: ns2, kind: SkYield, enclosing_try: ana_region(a))
              let a = Ana(..a, splits: [sp, ..a.splits])
              close_open(a, AsyncGenYieldSent(ns: ns2), ns2, AeResume(SkYield))
            }
            state.CorAsync | state.CorGenerator -> {
              let #(_, a) = record_split(a, SkYield, arg, None)
              a
            }
          }
      }
    }
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
    | ast.IntrinsicTemplateObject(..) -> a
    ast.FunctionExpression(..) | ast.ArrowFunctionExpression(..) ->
      Ana(..a, cur: cursor_pop_child_fn(a.cur))
    ast.UnaryExpression(argument: x, ..)
    | ast.UpdateExpression(argument: x, ..)
    | ast.SpreadElement(argument: x, ..)
    | ast.ParenthesizedExpression(expression: x, ..) -> ana_expr(a, x, tail)
    ast.BinaryExpression(left: l, right: r, ..)
    | ast.LogicalExpression(left: l, right: r, ..)
    | ast.AssignmentExpression(left: l, right: r, ..) ->
      ana_expr(ana_expr(a, l, tail), r, tail)
    ast.MemberExpression(object: o, property: p, ..)
    | ast.OptionalMemberExpression(object: o, property: p, ..) -> {
      let a = ana_expr(a, o, tail)
      case p {
        ast.Bracket(expression: pe) -> ana_expr(a, pe, tail)
        ast.Dot(..) -> a
      }
    }
    ast.CallExpression(callee: c, arguments: args, ..)
    | ast.OptionalCallExpression(callee: c, arguments: args, ..)
    | ast.NewExpression(callee: c, arguments: args, ..) ->
      list.fold(args, ana_expr(a, c, tail), fn(a, x) { ana_expr(a, x, tail) })
    ast.ConditionalExpression(condition: c, consequent: t, alternate: f, ..) ->
      ana_expr(ana_expr(ana_expr(a, c, tail), t, tail), f, tail)
    ast.SequenceExpression(expressions: xs, ..) ->
      list.fold(xs, a, fn(a, x) { ana_expr(a, x, tail) })
    ast.ArrayExpression(elements: xs, ..) ->
      list.fold(xs, a, fn(a, o) { ana_opt_expr(a, o, tail) })
    ast.ObjectExpression(properties: ps, ..) ->
      list.fold(ps, a, fn(a, p) {
        case p {
          ast.InitProperty(key: k, value: v, ..) ->
            ana_expr(ana_key(a, k, tail), v, tail)
          ast.MethodProperty(key: k, ..) | ast.AccessorProperty(key: k, ..) -> {
            let a = ana_key(a, k, tail)
            Ana(..a, cur: cursor_pop_child_fn(a.cur))
          }
          ast.SpreadProperty(argument: x) -> ana_expr(a, x, tail)
        }
      })
    ast.TemplateLiteral(parts: parts, ..) ->
      list.fold(ast.template_expressions(parts), a, fn(a, x) {
        ana_expr(a, x, tail)
      })
    ast.TaggedTemplateExpression(tag: t, parts: parts, ..) ->
      list.fold(ast.template_expressions(parts), ana_expr(a, t, tail), fn(a, x) {
        ana_expr(a, x, tail)
      })
    ast.ImportExpression(source: s, options: o, ..) ->
      ana_opt_expr(ana_expr(a, s, tail), o, tail)
    ast.ClassExpression(super_class: sc, body: elems, ..) ->
      ana_class(a, sc, elems, tail)
  }
}

fn ana_key(a: Ana, k: ast.PropertyKey, tail) -> Ana {
  case k {
    ast.KeyComputed(expression: e) -> ana_expr(a, e, tail)
    ast.KeyIdentifier(..)
    | ast.KeyString(..)
    | ast.KeyNumber(..)
    | ast.KeyBigInt(..)
    | ast.KeyPrivate(..) -> a
  }
}

fn ana_class(
  a: Ana,
  sc: Option(ast.Expression),
  body: List(ast.ClassElement),
  tail,
) -> Ana {
  let parts = ast_util.classify_class_body(body)
  let #(inner, resume) = cursor_enter_scope(a.tree, a.cur)
  let a = Ana(..a, cur: inner)
  let a = case class.has_instance_field_init(parts) {
    True -> ana_pop_child_fn(a)
    False -> a
  }
  let a = ana_pop_child_fn(a)
  let a = ana_opt_expr(a, sc, tail)
  let a =
    list.fold(ast_util.computed_element_keys(body), a, fn(a, key) {
      ana_expr(a, key.1, tail)
    })
  let a =
    list.fold(
      list.append(parts.instance_methods, parts.static_methods),
      a,
      fn(a, _) { ana_pop_child_fn(a) },
    )
  let a = case parts.static_elements {
    [] -> a
    [_, ..] -> ana_pop_child_fn(a)
  }
  Ana(..a, cur: cursor_leave_scope(resume, a.cur))
}

fn ana_pop_child_fn(a: Ana) -> Ana {
  Ana(..a, cur: cursor_pop_child_fn(a.cur))
}

fn ana_pattern(a: Ana, p: ast.Pattern, tail) -> Ana {
  case p {
    ast.IdentifierPattern(..) -> a
    ast.AssignmentPattern(left: l, right: r) ->
      ana_expr(ana_pattern(a, l, tail), r, tail)
    ast.RestElement(argument: x) -> ana_pattern(a, x, tail)
    ast.ArrayPattern(elements: xs) ->
      list.fold(xs, a, fn(a, o) {
        case o {
          Some(x) -> ana_pattern(a, x, tail)
          None -> a
        }
      })
    ast.ObjectPattern(properties: ps) ->
      list.fold(ps, a, fn(a, pp) {
        case pp {
          ast.PatternProperty(key: k, value: v, ..) ->
            ana_pattern(ana_key(a, k, tail), v, tail)
          ast.RestProperty(..) -> a
        }
      })
  }
}

fn ana_for_init(a: Ana, fi: ast.ForInit, tail) -> Ana {
  case fi {
    ast.ForInitExpression(e) -> ana_expr(a, e, tail)
    ast.ForInitDeclaration(declarations: ds, ..) ->
      list.fold(ds, a, fn(a, d: ast.VariableDeclarator) {
        ana_opt_expr(ana_pattern(a, d.id, tail), d.init, tail)
      })
    ast.ForInitPattern(p) -> ana_pattern(a, p, tail)
  }
}

fn ana_stmts(a: Ana, ss: List(ast.StmtWithLine)) -> Ana {
  case ss {
    [] -> a
    [sl, ..rest] -> ana_stmts(ana_stmt(a, sl, rest), rest)
  }
}

fn one_stmt(line: Int, s: ast.Statement) -> List(ast.StmtWithLine) {
  [ast.StmtWithLine(line:, statement: s)]
}

fn ana_stmt(a: Ana, sl: ast.StmtWithLine, tail: List(ast.StmtWithLine)) -> Ana {
  let ast.StmtWithLine(statement: s, ..) = sl
  case stmt_has_split(s) {
    False -> frag_push(ana_stmt_cursor_only(a, s, tail), sl)
    True ->
      case explode_stmt(a, sl) {
        Some(#(a, exploded)) -> ana_stmts(a, exploded)
        None -> ana_split_stmt(a, sl, tail)
      }
  }
}

fn ana_split_stmt(
  a: Ana,
  sl: ast.StmtWithLine,
  tail: List(ast.StmtWithLine),
) -> Ana {
  let ast.StmtWithLine(line:, statement: s) = sl
  case s {
    ast.IfStatement(condition: c, consequent: t, alternate: f) ->
      plan_ctrl_if(a, line, c, t, f, tail)
    ast.BlockStatement(body: b) ->
      with_scope_if(a, ast_util.block_has_declarations(b), fn(a) {
        ana_stmts(a, b)
      })
    ast.LabeledStatement(label:, body: b) ->
      plan_ctrl_labeled(a, line, label, b, tail)
    ast.WhileStatement(condition: c, body: b) ->
      plan_ctrl_while(a, line, None, c, b, tail)
    ast.DoWhileStatement(condition: c, body: b) ->
      plan_ctrl_do_while(a, line, None, c, b, tail)
    ast.ForStatement(init: i, condition: c, update: u, body: b) ->
      plan_ctrl_for(a, line, None, i, c, u, b, tail)
    ast.ForOfStatement(left: l, right: r, body: b, is_await: False) ->
      plan_ctrl_for_of(a, line, None, l, r, b, False, tail)
    ast.ForOfStatement(left: l, right: r, body: b, is_await: True) ->
      plan_ctrl_for_of(a, line, None, l, r, b, True, tail)
    ast.ForInStatement(left: l, right: r, body: b) ->
      plan_ctrl_for_of(a, line, None, l, r, b, False, tail)
    ast.SwitchStatement(discriminant: d, cases: cs) ->
      plan_ctrl_switch(a, line, None, d, cs, tail)
    ast.ExpressionStatement(..)
    | ast.ThrowStatement(..)
    | ast.ReturnStatement(..)
    | ast.VariableDeclaration(..) -> ana_hoisted(a, hoist_one(sl), tail)
    ast.ClassDeclaration(super_class: sc, body: elems, ..) ->
      frag_push(ana_class(a, sc, elems, tail), sl)
    ast.WithStatement(object: o, body: b) ->
      case split_of(o) {
        Some(#(kind, operand)) -> {
          let a = ana_opt_expr(a, operand, tail)
          let rw = Some(ResumeWithScope(body: b, line:))
          case kind {
            SkYieldStar -> record_delegate(a, operand, rw)
            _ -> {
              let #(_, a) = record_split(a, kind, operand, rw)
              a
            }
          }
        }
        None -> {
          let a = ana_expr(a, o, tail)
          ana_stmts(a, one_stmt(line, b))
        }
      }
    ast.TryStatement(block: blk, tail: tt) -> ana_try(a, blk, tt, tail)
    ast.EmptyStatement
    | ast.DebuggerStatement
    | ast.BreakStatement(..)
    | ast.ContinueStatement(..)
    | ast.FunctionDeclaration(..) -> frag_push(a, sl)
  }
}

fn ana_hoisted(
  a: Ana,
  items: List(HoistedItem),
  tail: List(ast.StmtWithLine),
) -> Ana {
  list.fold(items, a, fn(a, item) {
    case item {
      HiStmt(s) -> frag_push(ana_stmt_cursor_only(a, s.statement, tail), s)
      HiSplit(_, kind, operand, resume) -> {
        let a = ana_opt_expr(a, operand, tail)
        let rw = Some(resume)
        case kind {
          SkYieldStar -> record_delegate(a, operand, rw)
          SkYield ->
            case a.kind {
              state.CorAsyncGen -> {
                let #(_, a) = record_split(a, SkAwait, operand, None)
                let #(ns2, a) = alloc_state(a)
                let sp =
                  SplitPoint(
                    id: ns2,
                    kind: SkYield,
                    enclosing_try: ana_region(a),
                  )
                let a = Ana(..a, splits: [sp, ..a.splits])
                let a =
                  close_open(
                    a,
                    AsyncGenYieldSent(ns: ns2),
                    ns2,
                    AeResume(SkYield),
                  )
                Ana(..a, open_resume: rw)
              }
              state.CorAsync | state.CorGenerator -> {
                let #(_, a) = record_split(a, SkYield, operand, rw)
                a
              }
            }
          SkAwait | SkForAwait -> {
            let #(_, a) = record_split(a, kind, operand, rw)
            a
          }
        }
      }
    }
  })
}

fn ana_stmt_cursor_only(
  a: Ana,
  s: ast.Statement,
  tail: List(ast.StmtWithLine),
) -> Ana {
  let was = a.cursor_only
  let a = cursor_only_walk(Ana(..a, cursor_only: True), s, tail)
  Ana(..a, cursor_only: was)
}

fn cursor_only_walk(
  a: Ana,
  s: ast.Statement,
  tail: List(ast.StmtWithLine),
) -> Ana {
  case s {
    ast.FunctionDeclaration(..) -> Ana(..a, cur: cursor_pop_child_fn(a.cur))
    ast.ClassDeclaration(super_class: sc, body: elems, ..) ->
      ana_class(a, sc, elems, tail)
    ast.ExpressionStatement(expression: e, ..)
    | ast.ThrowStatement(argument: e) -> ana_expr(a, e, tail)
    ast.ReturnStatement(argument: arg) -> ana_opt_expr(a, arg, tail)
    ast.VariableDeclaration(declarations: ds, ..) ->
      list.fold(ds, a, fn(a, d: ast.VariableDeclarator) {
        ana_opt_expr(ana_pattern(a, d.id, tail), d.init, tail)
      })
    ast.BlockStatement(body: b) ->
      with_scope_if(a, ast_util.block_has_declarations(b), fn(a) {
        list.fold(b, a, fn(a, sl: ast.StmtWithLine) {
          ana_stmt_cursor_only(a, sl.statement, tail)
        })
      })
    ast.IfStatement(condition: c, consequent: t, alternate: f) -> {
      let a = ana_expr(a, c, tail)
      let a = ana_stmt_cursor_only(a, t, tail)
      case f {
        Some(alt) -> ana_stmt_cursor_only(a, alt, tail)
        None -> a
      }
    }
    ast.WhileStatement(condition: c, body: b)
    | ast.DoWhileStatement(condition: c, body: b) -> {
      let a = ana_expr(a, c, tail)
      ana_stmt_cursor_only(a, b, tail)
    }
    ast.ForStatement(init: i, condition: c, update: u, body: b) ->
      with_scope_if(a, ast_util.for_classic_init_is_lex(i), fn(a) {
        let a = case i {
          Some(fi) -> ana_for_init(a, fi, tail)
          None -> a
        }
        let a = ana_opt_expr(a, c, tail)
        let a = ana_opt_expr(a, u, tail)
        ana_stmt_cursor_only(a, b, tail)
      })
    ast.ForInStatement(left: l, right: r, body: b)
    | ast.ForOfStatement(left: l, right: r, body: b, ..) ->
      with_scope_if(a, ast_util.for_classic_init_is_lex(Some(l)), fn(a) {
        let a = ana_for_init(a, l, tail)
        let a = ana_expr(a, r, tail)
        ana_stmt_cursor_only(a, b, tail)
      })
    ast.SwitchStatement(discriminant: d, cases: cs) -> {
      let a = ana_expr(a, d, tail)
      with_scope(a, fn(a) {
        list.fold(cs, a, fn(a, c: ast.SwitchCase) {
          let a = ana_opt_expr(a, c.condition, tail)
          list.fold(c.consequent, a, fn(a, sl: ast.StmtWithLine) {
            ana_stmt_cursor_only(a, sl.statement, tail)
          })
        })
      })
    }
    ast.LabeledStatement(body: b, ..) -> ana_stmt_cursor_only(a, b, tail)
    ast.WithStatement(object: o, body: b) -> {
      let a = ana_expr(a, o, tail)
      ana_stmt_cursor_only(a, b, tail)
    }
    ast.TryStatement(block: blk, tail: tt) -> {
      let body_fold = fn(a, ss: List(ast.StmtWithLine)) {
        list.fold(ss, a, fn(a, sl: ast.StmtWithLine) {
          ana_stmt_cursor_only(a, sl.statement, tail)
        })
      }
      let block_cur = fn(a, ss) {
        with_scope_if(a, ast_util.block_has_declarations(ss), fn(a) {
          body_fold(a, ss)
        })
      }
      let a = block_cur(a, blk)
      case tt {
        ast.TryCatch(handler: h) ->
          walk_catch_cur(a, h, fn(a) { body_fold(a, h.body) })
        ast.TryFinally(finalizer: f) -> block_cur(a, f)
        ast.TryCatchFinally(handler: h, finalizer: f) ->
          block_cur(walk_catch_cur(a, h, fn(a) { body_fold(a, h.body) }), f)
      }
    }
    ast.EmptyStatement
    | ast.DebuggerStatement
    | ast.BreakStatement(..)
    | ast.ContinueStatement(..) -> a
  }
}

fn plan_head_expr(
  a: Ana,
  expr: ast.Expression,
  tail: List(ast.StmtWithLine),
) -> #(Option(ast.Expression), Ana) {
  case split_of(expr) {
    Some(#(kind, operand)) ->
      case kind {
        SkYieldStar -> #(Some(expr), ana_expr(a, expr, tail))
        SkYield ->
          case a.kind {
            state.CorAsyncGen -> #(Some(expr), ana_expr(a, expr, tail))
            state.CorAsync | state.CorGenerator -> {
              let a = ana_opt_expr(a, operand, tail)
              let #(_, a) = record_split(a, SkYield, operand, None)
              #(None, a)
            }
          }
        SkAwait | SkForAwait -> {
          let a = ana_opt_expr(a, operand, tail)
          let #(_, a) = record_split(a, kind, operand, None)
          #(None, a)
        }
      }
    None -> #(Some(expr), ana_expr(a, expr, tail))
  }
}

fn plan_ctrl_if(
  a: Ana,
  line: Int,
  cond: ast.Expression,
  cons: ast.Statement,
  alt: Option(ast.Statement),
  tail: List(ast.StmtWithLine),
) -> Ana {
  let #(cond, a) = plan_head_expr(a, cond, tail)
  let #(then_s, a) = alloc_state(a)
  let #(after, a) = alloc_state(a)
  let #(else_s, a) = case alt {
    Some(_) -> alloc_state(a)
    None -> #(after, a)
  }
  let a = close_open(a, CondBranch(cond:, then_s:, else_s:), then_s, AeJump)
  let a = ana_stmts(a, one_stmt(line, cons))
  let a = close_open(a, FallTo(after), else_s, AeJump)
  case alt {
    None -> a
    Some(alt_stmt) -> {
      let a = ana_stmts(a, one_stmt(line, alt_stmt))
      close_open(a, FallTo(after), after, AeJump)
    }
  }
}

fn plan_ctrl_while(
  a: Ana,
  line: Int,
  label: Option(String),
  cond: ast.Expression,
  body: ast.Statement,
  tail: List(ast.StmtWithLine),
) -> Ana {
  let #(head, a) = alloc_state(a)
  let #(body_s, a) = alloc_state(a)
  let #(after, a) = alloc_state(a)
  let #(brk, a) = alloc_sentinel(a)
  let #(cont, a) = alloc_sentinel(a)
  let a = close_open(a, FallTo(head), head, AeJump)
  let #(cond, a) = plan_head_expr(a, cond, tail)
  let a =
    close_open(
      a,
      CondBranch(cond:, then_s: body_s, else_s: after),
      body_s,
      AeJump,
    )
  let sm_label =
    SmLoop(
      js_label: label,
      brk_sentinel: brk,
      cont_sentinel: cont,
      break_state: after,
      continue_state: head,
      enclosing_try: ana_region(a),
    )
  let a = push_sm_label(a, sm_label)
  let a = ana_stmts(a, one_stmt(line, body))
  let a = pop_sm_label(a)
  close_open(a, FallTo(head), after, AeJump)
}

fn plan_ctrl_do_while(
  a: Ana,
  line: Int,
  label: Option(String),
  cond: ast.Expression,
  body: ast.Statement,
  tail: List(ast.StmtWithLine),
) -> Ana {
  let #(body_s, a) = alloc_state(a)
  let #(test_s, a) = alloc_state(a)
  let #(after, a) = alloc_state(a)
  let #(brk, a) = alloc_sentinel(a)
  let #(cont, a) = alloc_sentinel(a)
  let a = close_open(a, FallTo(body_s), body_s, AeJump)
  let sm_label =
    SmLoop(
      js_label: label,
      brk_sentinel: brk,
      cont_sentinel: cont,
      break_state: after,
      continue_state: test_s,
      enclosing_try: ana_region(a),
    )
  let a = push_sm_label(a, sm_label)
  let a = ana_stmts(a, one_stmt(line, body))
  let a = pop_sm_label(a)
  let a = close_open(a, FallTo(test_s), test_s, AeJump)
  let #(cond, a) = plan_head_expr(a, cond, tail)
  close_open(a, CondBranch(cond:, then_s: body_s, else_s: after), after, AeJump)
}

fn plan_ctrl_for(
  a: Ana,
  line: Int,
  label: Option(String),
  init: Option(ast.ForInit),
  cond: Option(ast.Expression),
  update: Option(ast.Expression),
  body: ast.Statement,
  tail: List(ast.StmtWithLine),
) -> Ana {
  with_scope_if(a, ast_util.for_classic_init_is_lex(init), fn(a) {
    let a = case init {
      None -> a
      Some(fi) -> {
        let a = ana_for_init(a, fi, tail)
        case fi {
          ast.ForInitExpression(e) ->
            frag_push(
              a,
              ast.StmtWithLine(
                line:,
                statement: ast.ExpressionStatement(e, None),
              ),
            )
          ast.ForInitDeclaration(kind: vk, declarations:) ->
            frag_push(
              a,
              ast.StmtWithLine(
                line:,
                statement: ast.VariableDeclaration(kind: vk, declarations:),
              ),
            )
          // todo: lhs default splits would land in the wrong arm
          ast.ForInitPattern(_) -> a
        }
      }
    }
    let #(head, a) = alloc_state(a)
    let #(body_s, a) = alloc_state(a)
    let #(update_s, a) = alloc_state(a)
    let #(after, a) = alloc_state(a)
    let #(brk, a) = alloc_sentinel(a)
    let #(cont, a) = alloc_sentinel(a)
    let a = close_open(a, FallTo(head), head, AeJump)
    let a = case cond {
      Some(c) -> {
        let #(c, a) = plan_head_expr(a, c, tail)
        close_open(
          a,
          CondBranch(cond: c, then_s: body_s, else_s: after),
          body_s,
          AeJump,
        )
      }
      None -> close_open(a, FallTo(body_s), body_s, AeJump)
    }
    let sm_label =
      SmLoop(
        js_label: label,
        brk_sentinel: brk,
        cont_sentinel: cont,
        break_state: after,
        continue_state: update_s,
        enclosing_try: ana_region(a),
      )
    let a = push_sm_label(a, sm_label)
    let a = ana_stmts(a, one_stmt(line, body))
    let a = pop_sm_label(a)
    let a = close_open(a, FallTo(update_s), update_s, AeJump)
    let #(update, a) = case update {
      Some(u) -> plan_head_expr(a, u, tail)
      None -> #(None, a)
    }
    close_open(a, ForUpdate(update:, head:), after, AeJump)
  })
}

fn plan_ctrl_for_of(
  a: Ana,
  line: Int,
  label: Option(String),
  left: ast.ForInit,
  right: ast.Expression,
  body: ast.Statement,
  is_await: Bool,
  tail: List(ast.StmtWithLine),
) -> Ana {
  with_scope_if(a, ast_util.for_classic_init_is_lex(Some(left)), fn(a) {
    case is_await {
      True -> plan_ctrl_for_await(a, line, label, left, right, body, tail)
      False -> {
        let #(head, a) = alloc_state(a)
        let #(body_s, a) = alloc_state(a)
        let #(after, a) = alloc_state(a)
        let #(brk, a) = alloc_sentinel(a)
        let #(cont, a) = alloc_sentinel(a)
        let ikey = iter_key(head)
        let a =
          close_open(a, ForOfSetup(right:, iter_key: ikey, head:), head, AeJump)
        let a =
          close_open(
            a,
            ForOfStep(left:, iter_key: ikey, body_s:, after:, is_await: False),
            body_s,
            AeJump,
          )
        let sm_label =
          SmLoop(
            js_label: label,
            brk_sentinel: brk,
            cont_sentinel: cont,
            break_state: after,
            continue_state: head,
            enclosing_try: ana_region(a),
          )
        let a = push_sm_label(a, sm_label)
        let a = ana_stmts(a, one_stmt(line, body))
        let a = pop_sm_label(a)
        close_open(a, FallTo(head), after, AeJump)
      }
    }
  })
}

fn plan_ctrl_for_await(
  a: Ana,
  line: Int,
  label: Option(String),
  left: ast.ForInit,
  right: ast.Expression,
  body: ast.Statement,
  tail: List(ast.StmtWithLine),
) -> Ana {
  let #(head, a) = alloc_state(a)
  let #(check, a) = alloc_state(a)
  let #(body_s, a) = alloc_state(a)
  let #(after, a) = alloc_state(a)
  let #(brk, a) = alloc_sentinel(a)
  let #(cont, a) = alloc_sentinel(a)
  let region = ana_region(a)
  let sp = SplitPoint(id: check, kind: SkForAwait, enclosing_try: region)
  let a = Ana(..a, splits: [sp, ..a.splits])
  let spec =
    ForAwaitSpec(
      head:,
      check:,
      body_s:,
      after:,
      left:,
      body_cursor: a.cur,
      region:,
    )
  let a = Ana(..a, for_awaits: [spec, ..a.for_awaits])
  let a = close_open(a, ForAwaitSetup(right:, head:), body_s, AeJump)
  let sm_label =
    SmLoop(
      js_label: label,
      brk_sentinel: brk,
      cont_sentinel: cont,
      break_state: after,
      continue_state: head,
      enclosing_try: region,
    )
  let a = push_sm_label(a, sm_label)
  let a = ana_for_init(a, left, tail)
  let a = ana_stmts(a, one_stmt(line, body))
  let a = pop_sm_label(a)
  close_open(a, FallTo(head), after, AeJump)
}

fn plan_ctrl_switch(
  a: Ana,
  _line: Int,
  label: Option(String),
  disc: ast.Expression,
  cases: List(ast.SwitchCase),
  tail: List(ast.StmtWithLine),
) -> Ana {
  let #(disc, a) = plan_head_expr(a, disc, tail)
  let #(after, a) = alloc_state(a)
  let #(brk, a) = alloc_sentinel(a)
  with_scope(a, fn(a) {
    let #(case_states_rev, a) =
      list.fold(cases, #([], a), fn(acc, _c) {
        let #(sts, a) = acc
        let #(s, a) = alloc_state(a)
        #([s, ..sts], a)
      })
    let case_states = list.reverse(case_states_rev)
    let tests =
      list.map2(cases, case_states, fn(c, s) {
        let ast.SwitchCase(condition:, ..) = c
        #(condition, s)
      })
    let #(defs, non_defs) =
      list.partition(tests, fn(t) {
        case t {
          #(None, _) -> True
          _ -> False
        }
      })
    // §13.12.9 default is tested last
    let tests = list.append(non_defs, defs)
    let first = case case_states {
      [s, ..] -> s
      [] -> after
    }
    let a = close_open(a, SwitchDispatch(disc:, tests:, after:), first, AeJump)
    let sm_label =
      SmSwitch(
        js_label: label,
        brk_sentinel: brk,
        break_state: after,
        enclosing_try: ana_region(a),
      )
    let a = push_sm_label(a, sm_label)
    let a = plan_ctrl_switch_cases(a, cases, case_states, after)
    pop_sm_label(a)
  })
}

fn plan_ctrl_switch_cases(
  a: Ana,
  cases: List(ast.SwitchCase),
  states: List(Int),
  after: Int,
) -> Ana {
  case cases, states {
    [], [] -> a
    [ast.SwitchCase(consequent:, ..), ..cs], [_s, ..ss] -> {
      let a = ana_stmts(a, consequent)
      let next = case ss {
        [n, ..] -> n
        [] -> after
      }
      let a = close_open(a, FallTo(next), next, AeJump)
      plan_ctrl_switch_cases(a, cs, ss, after)
    }
    _, _ -> a
  }
}

fn plan_ctrl_labeled(
  a: Ana,
  line: Int,
  label: String,
  body: ast.Statement,
  tail: List(ast.StmtWithLine),
) -> Ana {
  case body {
    ast.WhileStatement(condition: c, body: b) ->
      plan_ctrl_while(a, line, Some(label), c, b, tail)
    ast.DoWhileStatement(condition: c, body: b) ->
      plan_ctrl_do_while(a, line, Some(label), c, b, tail)
    ast.ForStatement(init: i, condition: c, update: u, body: b) ->
      plan_ctrl_for(a, line, Some(label), i, c, u, b, tail)
    ast.ForOfStatement(left: l, right: r, body: b, is_await:) ->
      plan_ctrl_for_of(a, line, Some(label), l, r, b, is_await, tail)
    ast.ForInStatement(left: l, right: r, body: b) ->
      plan_ctrl_for_of(a, line, Some(label), l, r, b, False, tail)
    ast.SwitchStatement(discriminant: d, cases: cs) ->
      plan_ctrl_switch(a, line, Some(label), d, cs, tail)
    ast.LabeledStatement(label: inner, body: b) -> {
      let #(after, a) = alloc_state(a)
      let #(brk, a) = alloc_sentinel(a)
      let alias =
        SmLabeled(
          js_label: label,
          brk_sentinel: brk,
          break_state: after,
          enclosing_try: ana_region(a),
        )
      let a = push_sm_label(a, alias)
      let a = plan_ctrl_labeled(a, line, inner, b, tail)
      let a = pop_sm_label(a)
      close_open(a, FallTo(after), after, AeJump)
    }
    _ -> {
      let #(after, a) = alloc_state(a)
      let #(brk, a) = alloc_sentinel(a)
      let sm_label =
        SmLabeled(
          js_label: label,
          brk_sentinel: brk,
          break_state: after,
          enclosing_try: ana_region(a),
        )
      let a = push_sm_label(a, sm_label)
      let a = ana_stmts(a, one_stmt(line, body))
      let a = pop_sm_label(a)
      close_open(a, FallTo(after), after, AeJump)
    }
  }
}

fn ana_try(
  a: Ana,
  block: List(ast.StmtWithLine),
  tt: ast.TryTail,
  tail: List(ast.StmtWithLine),
) -> Ana {
  let #(handler, finalizer) = case tt {
    ast.TryCatch(handler: h) -> #(Some(h), None)
    ast.TryFinally(finalizer: f) -> #(None, Some(f))
    ast.TryCatchFinally(handler: h, finalizer: f) -> #(Some(h), Some(f))
  }
  let contains_split =
    stmts_have_split(block)
    || case handler {
      Some(h) -> catch_has_split(h)
      None -> False
    }
    || case finalizer {
      Some(f) -> stmts_have_split(f)
      None -> False
    }
  let block_has = ast_util.block_has_declarations(block)
  case contains_split {
    False -> {
      let a = with_scope_if(a, block_has, fn(a) { ana_stmts(a, block) })
      let a = case handler {
        Some(h) -> walk_catch_cur(a, h, fn(a) { ana_stmts(a, h.body) })
        None -> a
      }
      case finalizer {
        Some(f) ->
          with_scope_if(a, ast_util.block_has_declarations(f), fn(a) {
            ana_stmts(a, f)
          })
        None -> a
      }
    }
    True -> {
      let try_id = a.next_try
      let outer = ana_region(a)
      let entry_sm_labels = a.sm_labels
      let a = Ana(..a, next_try: try_id + 1)
      let #(block_entry, a) = alloc_state(a)
      let a = close_open(a, FallTo(block_entry), block_entry, AeJump)
      let a =
        Ana(..a, try_stack: [try_id, ..a.try_stack], open_region: Some(try_id))
      let a = with_scope_if(a, block_has, fn(a) { ana_stmts(a, block) })
      let a =
        Ana(..a, try_stack: case a.try_stack {
          [_, ..rest] -> rest
          [] -> []
        })
      let #(catch_state, catch_cursor, a) = case handler {
        Some(_) -> {
          let #(cs, a) = alloc_state(a)
          #(Some(cs), Some(a.cur), a)
        }
        None -> #(None, None, a)
      }
      let #(finally_state, a) = case finalizer {
        Some(_) -> {
          let #(fs, a) = alloc_state(a)
          #(Some(fs), a)
        }
        None -> #(None, a)
      }
      let #(after_state, a) = alloc_state(a)
      let normal_tail = case finally_state {
        Some(fs) -> FallToFinally(try_id, fs)
        None -> FallTo(after_state)
      }
      let catch_split = case handler {
        Some(h) -> catch_has_split(h)
        None -> False
      }
      let finally_split = case finalizer {
        Some(f) -> stmts_have_split(f)
        None -> False
      }
      let #(a, catch_close_tail) = case handler, catch_state, catch_split {
        Some(h), Some(cs), True -> {
          let a = close_open(a, normal_tail, cs, AeJump)
          let #(a, view) = case finally_state {
            Some(_) -> {
              let view_id = a.next_try
              let view =
                TryEntry(
                  id: view_id,
                  pending_id: try_id,
                  catch_state: None,
                  finally_state:,
                  after_state:,
                  pending_loc_idx: 0,
                  caught_loc_idx: 0,
                  outer:,
                  handler: None,
                  finalizer: None,
                  catch_cursor: None,
                  finally_cursor: None,
                  sm_labels: entry_sm_labels,
                )
              #(
                Ana(
                  ..a,
                  next_try: view_id + 1,
                  try_stack: [view_id, ..a.try_stack],
                  open_region: Some(view_id),
                ),
                Some(view),
              )
            }
            None -> #(a, None)
          }
          let a = Ana(..a, open_resume: Some(ResumeCatch(try_id, h.param)))
          let a = walk_catch_cur(a, h, fn(a) { ana_stmts(a, h.body) })
          let a = case view {
            Some(v) ->
              Ana(..a, tries: [v, ..a.tries], try_stack: case a.try_stack {
                [_, ..rest] -> rest
                [] -> []
              })
            None -> a
          }
          #(a, normal_tail)
        }
        _, _, _ -> {
          let #(sink, a) = alloc_state(a)
          let a = close_open(a, normal_tail, sink, AeJump)
          let a = case handler {
            Some(h) -> walk_catch_cur(a, h, fn(a) { ana_stmts(a, h.body) })
            None -> a
          }
          #(a, SegDone)
        }
      }
      let #(finally_cursor, a) = case finalizer, finally_state, finally_split {
        Some(f), Some(fs), True -> {
          let a = close_open(a, catch_close_tail, fs, AeJump)
          let a =
            with_scope_if(a, ast_util.block_has_declarations(f), fn(a) {
              ana_stmts(a, f)
            })
          #(None, close_open(a, FinallyEnd(try_id), after_state, AeJump))
        }
        _, _, _ -> {
          let #(sink, a) = alloc_state(a)
          let a = close_open(a, catch_close_tail, sink, AeJump)
          let finally_cursor = case finalizer {
            Some(_) -> Some(a.cur)
            None -> None
          }
          let a = case finalizer {
            Some(f) ->
              with_scope_if(a, ast_util.block_has_declarations(f), fn(a) {
                ana_stmts(a, f)
              })
            None -> a
          }
          #(finally_cursor, close_open(a, SegDone, after_state, AeJump))
        }
      }
      let entry =
        TryEntry(
          id: try_id,
          pending_id: try_id,
          catch_state:,
          finally_state:,
          after_state:,
          pending_loc_idx: 0,
          caught_loc_idx: 0,
          outer:,
          handler: case catch_split {
            True -> None
            False -> handler
          },
          finalizer: case finally_split {
            True -> None
            False -> finalizer
          },
          catch_cursor:,
          finally_cursor:,
          sm_labels: entry_sm_labels,
        )
      Ana(..a, tries: [entry, ..a.tries])
    }
  }
}

fn analyze_splits(
  tree: ScopeTree,
  cur0: ArmCursor,
  body: state.FnBody,
  kind: state.CoroutineKind,
) -> SplitPlan {
  let init =
    Ana(
      tree:,
      kind:,
      next_state: 1,
      next_try: 0,
      next_sentinel: 0,
      next_temp: 0,
      cursor_only: False,
      try_stack: [],
      sm_labels: [],
      cur: cur0,
      splits: [],
      tries: [],
      arms: [],
      delegates: [],
      for_awaits: [],
      frag_rev: [],
      open_state: 0,
      open_region: None,
      open_entry: AeInitial,
      open_cursor: cur0,
      open_labels: [],
      open_resume: None,
    )
  let a = case body {
    state.StmtBody(ss) -> ana_stmts(init, ss)
    state.ExprBody(e) -> ana_stmts(init, func.body_stmts(state.ExprBody(e)))
  }
  let a = close_open(a, BodyEnd, a.next_state, AeJump)
  SplitPlan(
    n_states: a.next_state,
    n_temps: a.next_temp,
    arms: list.reverse(a.arms),
    try_entries: list.reverse(a.tries),
    delegates: list.reverse(a.delegates),
    for_awaits: list.reverse(a.for_awaits),
  )
}

pub fn pending_key(try_id: Int) -> String {
  "pending_" <> int.to_string(try_id)
}

pub fn caught_key(try_id: Int) -> String {
  "caught_" <> int.to_string(try_id)
}

pub fn iter_key(state_id: Int) -> String {
  "iter_" <> int.to_string(state_id)
}

pub fn inner_key(state_id: Int) -> String {
  "inner_" <> int.to_string(state_id)
}

// not a js value, marks the result slot until the first inner call
const delegate_start = "yield_star_start"

pub fn delegate_result_key(state_id: Int) -> String {
  "delegate_result_" <> int.to_string(state_id)
}

pub fn for_await_iter_key(head: Int) -> String {
  "iter_fa_" <> int.to_string(head)
}

fn compute_loc_layout(info: scope.FunctionInfo, plan: SplitPlan) -> LocLayout {
  let hoist_count = info.local_count
  let slot_to_idx = index_identity_map(hoist_count)
  let #(extras, next) =
    alloc_try_extras(plan.try_entries, dict.new(), hoist_count)
  let #(extras, next) = alloc_delegate_extras(plan.delegates, extras, next)
  let #(extras, next) = alloc_for_await_extras(plan.for_awaits, extras, next)
  let #(extras, size) = alloc_for_of_extras(plan.arms, extras, next)
  let initial_values = build_initial_loc(size, pending_index_set(extras, plan))
  LocLayout(slot_to_idx:, size:, extras:, initial_values:)
}

fn index_identity_map(n: Int) -> Dict(Int, Int) {
  identity_map_loop(0, n, dict.new())
}

fn identity_map_loop(i: Int, n: Int, acc: Dict(Int, Int)) -> Dict(Int, Int) {
  case i < n {
    False -> acc
    True -> identity_map_loop(i + 1, n, dict.insert(acc, i, i))
  }
}

fn alloc_try_extras(
  entries: List(TryEntry),
  extras: Dict(String, Int),
  next: Int,
) -> #(Dict(String, Int), Int) {
  use #(extras, next), entry <- list.fold(entries, #(extras, next))
  let #(extras, next) = case entry.finally_state {
    Some(_) -> {
      let key = pending_key(entry.pending_id)
      case dict.has_key(extras, key) {
        True -> #(extras, next)
        False -> #(dict.insert(extras, key, next), next + 1)
      }
    }
    None -> #(extras, next)
  }
  case entry.catch_state {
    Some(_) -> #(dict.insert(extras, caught_key(entry.id), next), next + 1)
    None -> #(extras, next)
  }
}

fn alloc_delegate_extras(
  delegates: List(DelegateSpec),
  extras: Dict(String, Int),
  next: Int,
) -> #(Dict(String, Int), Int) {
  use #(extras, next), d <- list.fold(delegates, #(extras, next))
  let sid = d.state_id
  let extras =
    extras
    |> dict.insert(iter_key(sid), next)
    |> dict.insert(inner_key(sid), next + 1)
    |> dict.insert(delegate_result_key(sid), next + 2)
  #(extras, next + 3)
}

fn alloc_for_await_extras(
  for_awaits: List(ForAwaitSpec),
  extras: Dict(String, Int),
  next: Int,
) -> #(Dict(String, Int), Int) {
  use #(extras, next), fap <- list.fold(for_awaits, #(extras, next))
  #(dict.insert(extras, for_await_iter_key(fap.head), next), next + 1)
}

fn alloc_for_of_extras(
  arms: List(ArmSpec),
  extras: Dict(String, Int),
  next: Int,
) -> #(Dict(String, Int), Int) {
  use #(extras, next), arm <- list.fold(arms, #(extras, next))
  let alloc = fn(k) {
    case dict.has_key(extras, k) {
      True -> #(extras, next)
      False -> #(dict.insert(extras, k, next), next + 1)
    }
  }
  case arm.tail {
    ForOfStep(iter_key: k, ..) -> alloc(k)
    ForOfSetup(iter_key: k, ..) -> alloc(k)
    _ -> #(extras, next)
  }
}

fn pending_index_set(extras: Dict(String, Int), plan: SplitPlan) -> Set(Int) {
  use acc, entry <- list.fold(plan.try_entries, set.new())
  case entry.finally_state {
    None -> acc
    Some(_) ->
      case dict.get(extras, pending_key(entry.pending_id)) {
        Ok(idx) -> set.insert(acc, idx)
        Error(Nil) -> acc
      }
  }
}

fn build_initial_loc(size: Int, pending_idxs: Set(Int)) -> List(ir.Value) {
  initial_loc_loop(0, size, pending_idxs, [])
}

fn initial_loc_loop(
  i: Int,
  size: Int,
  pending: Set(Int),
  acc: List(ir.Value),
) -> List(ir.Value) {
  case i < size {
    False -> list.reverse(acc)
    True -> {
      let v = case set.contains(pending, i) {
        True -> ir.ConstAtom("normal")
        False -> ir.ConstAtom("undefined")
      }
      initial_loc_loop(i + 1, size, pending, [v, ..acc])
    }
  }
}

pub fn enrich_try_entries(
  entries: List(TryEntry),
  layout: LocLayout,
) -> List(TryEntry) {
  use entry <- list.map(entries)
  let pending_loc_idx =
    dict.get(layout.extras, pending_key(entry.pending_id))
    |> idx_or(entry.pending_loc_idx)
  let caught_loc_idx =
    dict.get(layout.extras, caught_key(entry.id))
    |> idx_or(entry.caught_loc_idx)
  TryEntry(..entry, pending_loc_idx:, caught_loc_idx:)
}

fn idx_or(r: Result(Int, Nil), default: Int) -> Int {
  case r {
    Ok(v) -> v
    Error(Nil) -> default
  }
}

fn step_return(v: ir.Value) -> ir.Expr {
  ir.Let(
    ["_step"],
    ir.TermOp(ir.MakeTuple, [ir.ConstAtom("return"), v]),
    ir.Return([ir.Var("_step")]),
  )
}

fn step_throw(v: ir.Value) -> ir.Expr {
  ir.Let(
    ["_step"],
    ir.TermOp(ir.MakeTuple, [ir.ConstAtom("throw"), v]),
    ir.Return([ir.Var("_step")]),
  )
}

fn step_yield(v: ir.Value, ns: Int, loc: ir.Value) -> ir.Expr {
  ir.Let(
    ["_step"],
    ir.TermOp(ir.MakeTuple, [ir.ConstAtom("yield"), v, ir.ConstI32(ns), loc]),
    ir.Return([ir.Var("_step")]),
  )
}

fn step_await(v: ir.Value, ns: Int, loc: ir.Value) -> ir.Expr {
  ir.Let(
    ["_step"],
    ir.TermOp(ir.MakeTuple, [ir.ConstAtom("await"), v, ir.ConstI32(ns), loc]),
    ir.Return([ir.Var("_step")]),
  )
}

fn pack_loc(ctx: SmCtx, overrides: Dict(Int, ir.Value)) -> anf.Build(ir.Value) {
  pack_loc_build(ctx, overrides, 0, [])
}

fn pack_loc_build(
  ctx: SmCtx,
  overrides: Dict(Int, ir.Value),
  i: Int,
  acc: List(ir.Value),
) -> anf.Build(ir.Value) {
  case i < ctx.layout.size {
    False -> anf.make_tuple(list.reverse(acc))
    True ->
      case dict.get(overrides, i) {
        Ok(v) -> pack_loc_build(ctx, overrides, i + 1, [v, ..acc])
        Error(_) ->
          anf.then(anf.bind(anf.tuple_get(ctx.loc_v, i)), fn(v) {
            pack_loc_build(ctx, overrides, i + 1, [v, ..acc])
          })
      }
  }
}

fn restore_locals(ctx: SmCtx) -> anf.Build(Dict(Int, ir.Value)) {
  let entries =
    ctx.layout.slot_to_idx
    |> dict.to_list
    |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
  restore_locals_fold(ctx, entries, dict.new())
}

fn restore_locals_fold(
  ctx: SmCtx,
  entries: List(#(Int, Int)),
  acc: Dict(Int, ir.Value),
) -> anf.Build(Dict(Int, ir.Value)) {
  case entries {
    [] -> anf.pure(acc)
    [#(slot, idx), ..rest] ->
      anf.then(anf.bind(anf.tuple_get(ctx.loc_v, idx)), fn(v) {
        restore_locals_fold(ctx, rest, dict.insert(acc, slot, v))
      })
  }
}

fn sm_default_arm(e: Emitter2) -> #(ir.Expr, Emitter2) {
  let msg = ir.ConstBinary(bit_array.from_string("invalid gen state"))
  anf.run_to(anf.host("new_error", [msg]), e, fn(_e, err) { step_throw(err) })
}

fn cap_param_name(e: Emitter2, i: Int) -> String {
  state.cap_param_name(e, i)
}

fn build_sm_params(e: Emitter2, i: Int, ncap: Int) -> List(ir.Local) {
  case i < ncap, e.uses_keys {
    True, _ -> [
      ir.Local(cap_param_name(e, i), ir.TTerm),
      ..build_sm_params(e, i + 1, ncap)
    ]
    False, True -> [
      ir.Local(state.keys_var, ir.TTerm),
      ..build_sm_params(state.Emitter2(..e, uses_keys: False), i, ncap)
    ]
    False, False -> [
      ir.Local("_rs", ir.TTerm),
      ir.Local("_sent", ir.TTerm),
      ir.Local("_loc", ir.TTerm),
    ]
  }
}

// binds fixed names _mode/_sv/_loc_i that smctx refers to
fn emit_sm_function(
  e: Emitter2,
  sm_name: String,
  ncap: Int,
  lresume: String,
  arms: List(ir.SwitchArm),
  default: ir.Expr,
) -> Emitter2 {
  let sent = ir.Var("_sent")
  let loop_body =
    ir.Let(
      ["_rsi32"],
      ir.Convert(ir.UnboxInt(ir.W32), ir.Var("_rs_i")),
      ir.Switch(ir.Var("_rsi32"), [ir.TTerm], arms, default),
    )
  let body =
    ir.Let(
      ["_mode"],
      ir.TermOp(ir.TupleGet(0), [sent]),
      ir.Let(
        ["_sv"],
        ir.TermOp(ir.TupleGet(1), [sent]),
        ir.Loop(
          lresume,
          [
            ir.LoopParam("_rs_i", ir.TTerm, ir.Var("_rs")),
            ir.LoopParam("_loc_i", ir.TTerm, ir.Var("_loc")),
          ],
          [ir.TTerm],
          loop_body,
        ),
      ),
    )
  state.add_function(
    e,
    ir.Function(
      name: sm_name,
      params: build_sm_params(e, 0, ncap),
      result: [ir.TTerm],
      locals: [],
      body: body,
    ),
  )
}

fn build_outer_params(e: Emitter2, i: Int, n: Int) -> List(ir.Local) {
  case i < n {
    False ->
      list.append(state.keys_params(e), [
        ir.Local("_frame", ir.TTerm),
        ir.Local("_args", ir.TTerm),
      ])
    True -> [
      ir.Local(cap_param_name(e, i), ir.TTerm),
      ..build_outer_params(e, i + 1, n)
    ]
  }
}

fn cap_vars(e: Emitter2, i: Int, n: Int) -> List(ir.Value) {
  case i < n {
    False -> []
    True -> [ir.Var(cap_param_name(e, i)), ..cap_vars(e, i + 1, n)]
  }
}

fn atom_bool(rc: state.RealmConsts, b: Bool) -> ir.Value {
  case b {
    True -> rc.true_
    False -> rc.false_
  }
}

fn expected_length(fixed: List(ast.Pattern)) -> Int {
  fixed
  |> list.take_while(fn(p) {
    case p {
      ast.AssignmentPattern(..) -> False
      _ -> True
    }
  })
  |> list.length
}

fn start_op(kind: state.CoroutineKind) -> String {
  case kind {
    state.CorAsync -> "async_start"
    state.CorGenerator -> "gen_start"
    state.CorAsyncGen -> "asyncgen_start"
  }
}

fn kind_is_async(kind: state.CoroutineKind) -> Bool {
  case kind {
    state.CorAsync | state.CorAsyncGen -> True
    state.CorGenerator -> False
  }
}

fn kind_is_gen(kind: state.CoroutineKind) -> Bool {
  case kind {
    state.CorGenerator | state.CorAsyncGen -> True
    state.CorAsync -> False
  }
}

fn initial_loc_values(
  e: Emitter2,
  layout: LocLayout,
  n_locals: Int,
) -> List(ir.Value) {
  list.index_map(layout.initial_values, fn(v, i) {
    case i < n_locals, dict.get(e.slot_vars, i) {
      True, Ok(name) -> ir.Var(name)
      _, _ -> v
    }
  })
}

fn emit_closure_site(
  e: Emitter2,
  outer_name: String,
  kind: state.CoroutineKind,
  shape: state.FnShape,
  is_strict: Bool,
  js_name: Option(String),
  params: List(ast.Pattern),
  captures: List(ir.Value),
) -> #(ir.Expr, Emitter2) {
  let rc = e.consts
  let flags = [
    // must match arc/rt/types FnFlags field order
    ir.ConstAtom("fn_flags"),
    rc.false_,
    rc.false_,
    rc.false_,
    atom_bool(rc, func.shape_is_arrow(shape)),
    atom_bool(rc, func.shape_is_method(shape)),
    atom_bool(rc, kind_is_gen(kind)),
    atom_bool(rc, kind_is_async(kind)),
    atom_bool(rc, is_strict),
  ]
  let name_bin = case js_name {
    Some(n) -> ir.ConstBinary(bit_array.from_string(n))
    None -> rc.empty_bin
  }
  let #(fixed, _) = ast_util.split_trailing_rest(params)
  let exp_len = expected_length(fixed)
  anf.run(
    {
      use fun <- anf.then(anf.bind(ir.MakeClosure(outer_name, captures, 2)))
      use flags_t <- anf.then(anf.make_tuple(flags))
      anf.host("fn_new", [
        fun,
        flags_t,
        name_bin,
        ir.ConstI32(exp_len),
        ir.ConstAtom("none"),
      ])
    },
    e,
  )
}

fn pack_loc_expr(
  ctx: SmCtx,
  overrides: Dict(Int, ir.Value),
  k: fn(ir.Value) -> ir.Expr,
) -> ir.Expr {
  pack_loc_expr_go(ctx, overrides, 0, [], k)
}

fn pack_loc_expr_go(
  ctx: SmCtx,
  overrides: Dict(Int, ir.Value),
  i: Int,
  acc: List(ir.Value),
  k: fn(ir.Value) -> ir.Expr,
) -> ir.Expr {
  case i < ctx.layout.size {
    False ->
      ir.Let(
        ["_locp"],
        ir.TermOp(ir.MakeTuple, list.reverse(acc)),
        k(ir.Var("_locp")),
      )
    True ->
      case dict.get(overrides, i) {
        Ok(v) -> pack_loc_expr_go(ctx, overrides, i + 1, [v, ..acc], k)
        Error(_) -> {
          // fixed names are safe, callers sit in a fresh let scope
          let name = "_pk" <> int.to_string(i)
          ir.Let(
            [name],
            ir.TermOp(ir.TupleGet(i), [ctx.loc_v]),
            pack_loc_expr_go(ctx, overrides, i + 1, [ir.Var(name), ..acc], k),
          )
        }
      }
  }
}

fn route_throw(ctx: SmCtx, region: Option(TryEntry), ev: ir.Value) -> ir.Expr {
  case region {
    Some(TryEntry(catch_state: Some(cs), caught_loc_idx: ci, ..)) ->
      pack_loc_expr(ctx, dict.from_list([#(ci, ev)]), fn(locp) {
        ir.Continue(ctx.lresume, [ir.ConstI32(cs), locp])
      })
    Some(TryEntry(finally_state: Some(fs), pending_loc_idx: pi, ..)) ->
      ir.Let(
        ["_pend"],
        ir.TermOp(ir.MakeTuple, [ir.ConstI32(pend_throw), ev]),
        pack_loc_expr(ctx, dict.from_list([#(pi, ir.Var("_pend"))]), fn(locp) {
          ir.Continue(ctx.lresume, [ir.ConstI32(fs), locp])
        }),
      )
    _ -> step_throw(ev)
  }
}

fn route_return(ctx: SmCtx, region: Option(TryEntry), v: ir.Value) -> ir.Expr {
  case region {
    Some(TryEntry(finally_state: Some(fs), pending_loc_idx: pi, ..)) ->
      ir.Let(
        ["_pend"],
        ir.TermOp(ir.MakeTuple, [ir.ConstI32(pend_return), v]),
        pack_loc_expr(ctx, dict.from_list([#(pi, ir.Var("_pend"))]), fn(locp) {
          ir.Continue(ctx.lresume, [ir.ConstI32(fs), locp])
        }),
      )
    Some(entry) -> route_return(ctx, find_try(ctx.try_entries, entry.outer), v)
    None -> step_return(v)
  }
}

fn wrap_arm_try(
  ctx: SmCtx,
  _n: Int,
  region: Option(TryEntry),
  inner: ir.Expr,
) -> ir.Expr {
  ir.Try(result: [ir.TTerm], body: inner, handlers: [
    ir.CatchHandler(
      on: ir.OnTag("js_exn"),
      payload: ["_e"],
      exnref: None,
      handler: route_throw(ctx, region, ir.Var("_e")),
    ),
  ])
}

fn emit_mode_dispatch(
  ctx: SmCtx,
  entry: ArmEntry,
  region: Option(TryEntry),
  normal: ir.Expr,
) -> ir.Expr {
  case entry {
    // jump-entered arms see stale mode/sent so only resume arms dispatch
    AeInitial | AeJump -> normal
    AeResume(_) ->
      ir.Let(
        ["_i32m"],
        ir.Convert(ir.UnboxInt(ir.W32), ctx.mode_v),
        ir.Let(
          ["_is_thr"],
          ir.Num(ir.IEq(ir.W32), [ir.Var("_i32m"), ir.ConstI32(1)]),
          ir.If(
            ir.Var("_is_thr"),
            [ir.TTerm],
            route_throw(ctx, region, ctx.sent_v),
            ir.Let(
              ["_is_ret"],
              ir.Num(ir.IEq(ir.W32), [ir.Var("_i32m"), ir.ConstI32(2)]),
              ir.If(
                ir.Var("_is_ret"),
                [ir.TTerm],
                route_return(ctx, region, ctx.sent_v),
                normal,
              ),
            ),
          ),
        ),
      )
  }
}

fn run_terminal(b: anf.Build(ir.Expr), e: Emitter2) -> #(ir.Expr, Emitter2) {
  b(e, fn(ef, expr) { #(expr, ef) })
}

fn if_terminal(
  cond: ir.Value,
  t: anf.Build(ir.Expr),
  f: anf.Build(ir.Expr),
) -> anf.Build(ir.Expr) {
  fn(e, k) {
    let #(t_tree, e) = run_terminal(t, e)
    let #(f_tree, e) = run_terminal(f, e)
    k(e, ir.If(cond, [ir.TTerm], t_tree, f_tree))
  }
}

// _rs_i is a term so the state id must be boxed
fn rs_box(n: Int) -> anf.Build(ir.Value) {
  anf.bind(ir.Convert(ir.BoxInt(ir.W32), ir.ConstI32(n)))
}

fn key_named(s: String) -> anf.Build(ir.Value) {
  anf.then(anf.key(s), anf.string_key)
}

fn get_named(obj: ir.Value, name: String) -> anf.Build(ir.Value) {
  use site <- anf.then(fn(e: Emitter2, k) {
    k(state.Emitter2(..e, next_site: e.next_site + 1), e.next_site)
  })
  use key <- anf.then(anf.key(name))
  anf.host("get_prop_site", [obj, key, ir.ConstI32(site)])
}

fn iter_hint(kind: state.CoroutineKind) -> ir.Value {
  case kind {
    state.CorAsyncGen -> ir.ConstAtom("async")
    state.CorGenerator | state.CorAsync -> ir.ConstAtom("sync")
  }
}

fn emit_delegate_setup(
  e: Emitter2,
  ctx: SmCtx,
  expr_v: ir.Value,
  nd: Int,
  iter_idx: Int,
  inner_idx: Int,
) -> #(ir.Expr, Emitter2) {
  let result_idx = extra_idx(ctx.layout, delegate_result_key(nd))
  let b = {
    use iter_h <- anf.then(
      anf.host("get_iterator", [expr_v, iter_hint(ctx.kind)]),
    )
    use k_iter <- anf.then(key_named("iterator"))
    use inner <- anf.then(anf.host("get_prop", [iter_h, k_iter]))
    let ov =
      dict.from_list([
        #(iter_idx, iter_h),
        #(inner_idx, inner),
        #(result_idx, ir.ConstAtom(delegate_start)),
      ])
    use loc2 <- anf.then(fn(e, k) { pack_loc_cps(e, ctx, ov, k) })
    use rs <- anf.then(rs_box(nd))
    anf.pure(ir.Continue(ctx.lresume, [rs, loc2]))
  }
  run_terminal(b, e)
}

// §27.5.3.8 yield* delegate arm, dispatches mode itself
fn emit_delegate_arm(
  e: Emitter2,
  ctx: SmCtx,
  d: DelegateSpec,
  iter_idx: Int,
  inner_idx: Int,
  result_idx: Int,
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  let undef = ir.ConstAtom("undefined")
  let b = {
    use iter_h <- anf.then(anf.bind(anf.tuple_get(ctx.loc_v, iter_idx)))
    use inner <- anf.then(anf.bind(anf.tuple_get(ctx.loc_v, inner_idx)))
    use flag <- anf.then(anf.bind(anf.tuple_get(ctx.loc_v, result_idx)))
    use first <- anf.then(
      anf.bind(ir.NumTerm(ir.NEq, flag, ir.ConstAtom(delegate_start))),
    )
    use mode_v <- anf.then(anf.bind_if(first, rs_box(0), anf.pure(ctx.mode_v)))
    use sent_v <- anf.then(anf.bind_if(
      first,
      anf.pure(undef),
      anf.pure(ctx.sent_v),
    ))
    let ctx = SmCtx(..ctx, mode_v:, sent_v:)
    use mode_i32 <- anf.then(
      anf.bind(ir.Convert(ir.UnboxInt(ir.W32), ctx.mode_v)),
    )
    use mode_ne0 <- anf.then(
      anf.bind(ir.Num(ir.INe(ir.W32), [mode_i32, ir.ConstI32(0)])),
    )
    let mkey = fn(s) { ir.Values([anf.fixed_key(s)]) }
    use meth <- anf.then(anf.bind_if(
      mode_ne0,
      {
        use mname <- anf.then(
          anf.bind(ir.Switch(
            mode_i32,
            [ir.TTerm],
            [ir.SwitchArm(1, mkey("throw")), ir.SwitchArm(2, mkey("return"))],
            mkey("next"),
          )),
        )
        use key <- anf.then(anf.string_key(mname))
        anf.host("get_prop", [inner, key])
      },
      get_named(iter_h, "next"),
    ))
    use is_undef <- anf.then(anf.bind(ir.NumTerm(ir.NEq, meth, undef)))
    use is_null <- anf.then(
      anf.bind(ir.NumTerm(ir.NEq, meth, ir.ConstAtom("null"))),
    )
    use is_nullish <- anf.then(anf.bind(ir.NumTerm(ir.NAdd, is_undef, is_null)))
    use missing <- anf.then(
      anf.bind(ir.Num(ir.IAnd(ir.W32), [mode_ne0, is_nullish])),
    )
    use is_throw <- anf.then(
      anf.bind(ir.Num(ir.IEq(ir.W32), [mode_i32, ir.ConstI32(1)])),
    )
    let on_missing =
      if_terminal(
        is_throw,
        {
          use _ <- anf.then(
            anf.host_unit("iter_close", [
              iter_h,
              ir.ConstAtom("true"),
            ]),
          )
          use _ <- anf.then(
            anf.host("throw_type_error", [
              ir.ConstBinary(bit_array.from_string(
                "iterator does not have a throw method",
              )),
            ]),
          )
          anf.pure(step_throw(ctx.sent_v))
        },
        anf.pure(route_return(ctx, current_try(ctx), ctx.sent_v)),
      )
    let on_call = {
      use argl <- anf.then(anf.cons_list([ctx.sent_v]))
      use res <- anf.then(anf.host("call", [meth, inner, argl]))
      case d.await_state {
        Some(na) -> {
          use loc2 <- anf.then(pack_loc(
            ctx,
            dict.from_list([#(result_idx, ctx.mode_v)]),
          ))
          anf.pure(step_await(res, na, loc2))
        }
        None -> delegate_result(ctx, d, res, mode_i32, result_idx, first)
      }
    }
    if_terminal(missing, on_missing, on_call)
  }
  Ok(run_terminal(b, e))
}

fn emit_delegate_await_arm(
  e: Emitter2,
  ctx: SmCtx,
  d: DelegateSpec,
  _iter_idx: Int,
  result_idx: Int,
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  let b = {
    use mode_v <- anf.then(anf.bind(anf.tuple_get(ctx.loc_v, result_idx)))
    use mode_i32 <- anf.then(anf.bind(ir.Convert(ir.UnboxInt(ir.W32), mode_v)))
    delegate_result(ctx, d, ctx.sent_v, mode_i32, result_idx, ir.ConstI32(0))
  }
  Ok(run_terminal(b, e))
}

fn delegate_result(
  ctx: SmCtx,
  d: DelegateSpec,
  res: ir.Value,
  mode_i32: ir.Value,
  result_idx: Int,
  first: ir.Value,
) -> anf.Build(ir.Expr) {
  use is_obj <- anf.then(anf.host_bool("is_object", [res]))
  use is_return <- anf.then(
    anf.bind(ir.Num(ir.IEq(ir.W32), [mode_i32, ir.ConstI32(2)])),
  )
  if_terminal(
    is_obj,
    {
      use done_t <- anf.then(get_named(res, "done"))
      use done <- anf.then(anf.host("truthy", [done_t]))
      use v <- anf.then(get_named(res, "value"))
      if_terminal(
        done,
        if_terminal(
          is_return,
          anf.pure(route_return(ctx, current_try(ctx), v)),
          {
            use loc2 <- anf.then(pack_loc(
              ctx,
              dict.from_list([#(result_idx, v)]),
            ))
            use rs <- anf.then(rs_box(d.next_state))
            anf.pure(ir.Continue(ctx.lresume, [rs, loc2]))
          },
        ),
        {
          use loc2 <- anf.then(anf.bind_if(
            first,
            pack_loc(
              ctx,
              dict.from_list([#(result_idx, ir.ConstAtom("undefined"))]),
            ),
            anf.pure(ctx.loc_v),
          ))
          anf.pure(step_yield(v, d.state_id, loc2))
        },
      )
    },
    {
      use _ <- anf.then(
        anf.host("throw_type_error", [
          ir.ConstBinary(bit_array.from_string(
            "iterator result is not an object",
          )),
        ]),
      )
      anf.pure(step_return(ir.ConstAtom("undefined")))
    },
  )
}

fn find_try_entry(ctx: SmCtx, id: Int) -> Option(TryEntry) {
  list.find(ctx.try_entries, fn(t) { t.id == id }) |> option.from_result
}

fn outer_entry(ctx: SmCtx, entry: TryEntry) -> Option(TryEntry) {
  case entry.outer {
    None -> None
    Some(oid) -> find_try_entry(ctx, oid)
  }
}

fn restore_and_seed(
  e: Emitter2,
  ctx: SmCtx,
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), state.EmitError),
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  restore_and_seed_go(e, ctx, dict.to_list(ctx.layout.slot_to_idx), k)
}

fn restore_and_seed_go(
  e: Emitter2,
  ctx: SmCtx,
  slots: List(#(Int, Int)),
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), state.EmitError),
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  case slots {
    [] -> k(e)
    [#(slot, idx), ..rest] -> {
      let #(name, e) = state.fresh_slot_var(e, slot)
      let e = state.set_slot_var(e, slot, name)
      use body <- state.map_tree(restore_and_seed_go(e, ctx, rest, k))
      ir.Let([name], ir.TermOp(ir.TupleGet(idx), [ctx.loc_v]), body)
    }
  }
}

fn jump_state_leaf(
  e: Emitter2,
  ctx: SmCtx,
  ns: Int,
  overrides: Dict(Int, ir.Value),
) -> #(ir.Expr, Emitter2) {
  cps_pair(e, fn(e, k) {
    pack_loc_cps(e, ctx, overrides, fn(e, loc) {
      k(e, sm_continue(ctx, ns, loc))
    })
  })
}

fn dispatch_throw(
  e: Emitter2,
  ctx: SmCtx,
  outer: Option(TryEntry),
  carry: ir.Value,
) -> #(ir.Expr, Emitter2) {
  case outer {
    None -> #(step_throw(carry), e)
    Some(o) ->
      case o.catch_state {
        Some(cs) ->
          jump_state_leaf(
            e,
            ctx,
            cs,
            dict.from_list([#(o.caught_loc_idx, carry)]),
          )
        None ->
          case o.finally_state {
            Some(fs) -> {
              let #(pn, e) = state.fresh_var(e)
              let #(jump, e) =
                jump_state_leaf(
                  e,
                  ctx,
                  fs,
                  dict.from_list([#(o.pending_loc_idx, ir.Var(pn))]),
                )
              let pend =
                ir.TermOp(ir.MakeTuple, [ir.ConstI32(pend_throw), carry])
              #(ir.Let([pn], pend, jump), e)
            }
            None -> dispatch_throw(e, ctx, outer_entry(ctx, o), carry)
          }
      }
  }
}

fn dispatch_return(
  e: Emitter2,
  ctx: SmCtx,
  outer: Option(TryEntry),
  carry: ir.Value,
) -> #(ir.Expr, Emitter2) {
  case outer {
    None -> #(step_return(carry), e)
    Some(o) ->
      case o.finally_state {
        Some(fs) -> {
          let #(pn, e) = state.fresh_var(e)
          let #(jump, e) =
            jump_state_leaf(
              e,
              ctx,
              fs,
              dict.from_list([#(o.pending_loc_idx, ir.Var(pn))]),
            )
          let pend = ir.TermOp(ir.MakeTuple, [ir.ConstI32(pend_return), carry])
          #(ir.Let([pn], pend, jump), e)
        }
        None -> dispatch_return(e, ctx, outer_entry(ctx, o), carry)
      }
  }
}

fn dispatch_goto(
  e: Emitter2,
  ctx: SmCtx,
  outer: Option(TryEntry),
  carry: ir.Value,
) -> #(ir.Expr, Emitter2) {
  case outer {
    None ->
      cps_pair(e, fn(e, k) {
        let #(ns, e) = state.fresh_var(e)
        anf.wrap(
          pack_loc_cps(e, ctx, dict.new(), fn(e, loc) {
            k(e, ir.Continue(ctx.lresume, [ir.Var(ns), loc]))
          }),
          ir.Let([ns], ir.Convert(ir.UnboxInt(ir.W32), carry), _),
        )
      })
    Some(o) ->
      case o.finally_state {
        Some(fs) -> {
          let #(pn, e) = state.fresh_var(e)
          let #(jump, e) =
            jump_state_leaf(
              e,
              ctx,
              fs,
              dict.from_list([#(o.pending_loc_idx, ir.Var(pn))]),
            )
          let pend = ir.TermOp(ir.MakeTuple, [ir.ConstI32(pend_goto), carry])
          #(ir.Let([pn], pend, jump), e)
        }
        None -> dispatch_goto(e, ctx, outer_entry(ctx, o), carry)
      }
  }
}

fn build_pending_dispatch(
  e: Emitter2,
  ctx: SmCtx,
  entry: TryEntry,
  pend: ir.Value,
) -> #(ir.Expr, Emitter2) {
  let outer = outer_entry(ctx, entry)
  let #(normal_jump, e) = jump_state_leaf(e, ctx, entry.after_state, dict.new())
  let #(kind_n, e) = state.fresh_var(e)
  let #(carry_n, e) = state.fresh_var(e)
  let carry = ir.Var(carry_n)
  let #(goto_tree, e) = dispatch_goto(e, ctx, outer, carry)
  let #(throw_tree, e) = dispatch_throw(e, ctx, outer, carry)
  let #(return_tree, e) = dispatch_return(e, ctx, outer, carry)
  let #(eqg_n, e) = state.fresh_var(e)
  let #(eqt_n, e) = state.fresh_var(e)
  let #(eqti_n, e) = state.fresh_var(e)
  let throw_or_return =
    ir.Let(
      [eqt_n],
      ir.Num(ir.IEq(ir.W32), [ir.Var(eqti_n), ir.ConstI32(pend_throw)]),
      ir.If(ir.Var(eqt_n), [ir.TTerm], throw_tree, return_tree),
    )
  let kind_branch =
    ir.Let(
      [eqg_n],
      ir.Num(ir.IEq(ir.W32), [ir.Var(eqti_n), ir.ConstI32(pend_goto)]),
      ir.If(ir.Var(eqg_n), [ir.TTerm], goto_tree, throw_or_return),
    )
  let tuple_branch =
    ir.Let(
      [kind_n],
      ir.TermOp(ir.TupleGet(0), [pend]),
      ir.Let(
        [eqti_n],
        ir.Convert(ir.UnboxInt(ir.W32), ir.Var(kind_n)),
        ir.Let([carry_n], ir.TermOp(ir.TupleGet(1), [pend]), kind_branch),
      ),
    )
  let #(isatom_n, e) = state.fresh_var(e)
  #(
    ir.Let(
      [isatom_n],
      ir.TermTest(ir.IsAtom, pend),
      ir.If(ir.Var(isatom_n), [ir.TTerm], normal_jump, tuple_branch),
    ),
    e,
  )
}

fn run_rk(
  e: Emitter2,
  body: fn(
    Emitter2,
    fn(Emitter2, ir.Expr) -> Result(#(ir.Expr, Emitter2), state.EmitError),
  ) -> Result(#(ir.Expr, Emitter2), state.EmitError),
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  body(e, fn(ef, tree) { Ok(#(tree, ef)) })
}

fn emit_finally_arm(
  e: Emitter2,
  ctx: SmCtx,
  entry: TryEntry,
  finalizer: List(ast.StmtWithLine),
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  run_rk(e, fn(e, done) {
    use e <- restore_and_seed(e, ctx)
    let #(pend_n, e) = state.fresh_var(e)
    let pend = ir.Var(pend_n)
    use #(body, e_out) <- result.try(
      with_abrupt_intercept(e, ctx, fn(e, restore) {
        let k_tail = fn(e_leaf: Emitter2) {
          Ok(build_pending_dispatch(e_leaf, ctx, entry, pend))
        }
        use #(body, e2) <- result.map(e.dispatch.emit_stmts(
          e,
          finalizer,
          k_tail,
        ))
        #(body, restore(e2))
      }),
    )
    done(
      e_out,
      ir.Let(
        [pend_n],
        ir.TermOp(ir.TupleGet(entry.pending_loc_idx), [ctx.loc_v]),
        body,
      ),
    )
  })
}

fn emit_catch_arm(
  e: Emitter2,
  ctx: SmCtx,
  entry: TryEntry,
  handler: ast.CatchClause,
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  let ast.CatchClause(param:, body: catch_body) = handler
  run_rk(e, fn(e, done) {
    use e <- restore_and_seed(e, ctx)
    let #(caught_n, e) = state.fresh_var(e)
    let caught = ir.Var(caught_n)
    use #(handler_tree, e_out) <- result.try(
      with_abrupt_intercept(e, ctx, fn(e, restore) {
        let k_tail = fn(e_leaf: Emitter2) {
          Ok(case entry.finally_state {
            Some(fs) ->
              jump_state_leaf(
                e_leaf,
                ctx,
                fs,
                dict.from_list([
                  #(entry.pending_loc_idx, ir.ConstAtom("normal")),
                ]),
              )
            None -> jump_state_leaf(e_leaf, ctx, entry.after_state, dict.new())
          })
        }
        use #(tree, e2) <- result.map(case param {
          Some(p) -> {
            let #(e, save) = state.enter_scope(e, in_block: e.in_block)
            use #(dtree, e) <- result.try(e.dispatch.emit_destructure(
              e,
              p,
              caught,
              state.BindLet,
            ))
            let #(dn, e) = state.fresh_var(e)
            use #(body_tree, e) <- result.map(e.dispatch.emit_stmts(
              e,
              catch_body,
              k_tail,
            ))
            #(
              state.splice_let(dtree, dn, body_tree),
              state.leave_scope(e, save),
            )
          }
          None -> e.dispatch.emit_stmts(e, catch_body, k_tail)
        })
        #(tree, restore(e2))
      }),
    )
    done(
      e_out,
      ir.Let(
        [caught_n],
        ir.TermOp(ir.TupleGet(entry.caught_loc_idx), [ctx.loc_v]),
        handler_tree,
      ),
    )
  })
}

fn emit_arm_body(
  e: Emitter2,
  ctx: SmCtx,
  arm: ArmSpec,
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  let e = install_cursor(e, arm.entry_cursor)
  let ctx = SmCtx(..with_region(ctx, arm.region), sm_labels: arm.sm_labels)
  use e <- restore_and_seed(e, ctx)
  with_abrupt_intercept(e, ctx, fn(e, restore) {
    use #(tree, e2) <- result.map(case arm.resume {
      Some(ResumeReturn) -> Ok(route_abrupt(e, ctx, PkReturn(ctx.sent_v), None))
      Some(ResumeThrow) -> Ok(route_abrupt(e, ctx, PkThrow(ctx.sent_v), None))
      _ -> {
        use #(prelude, e) <- result.try(case arm.resume {
          Some(ResumeBind(pat, mode)) ->
            e.dispatch.emit_destructure(e, pat, ctx.sent_v, mode)
          Some(ResumeCatch(try_id, Some(pat))) -> {
            let entry = find_try_entry(ctx, try_id)
            let idx = case entry {
              Some(t) -> t.caught_loc_idx
              None -> panic as "M18: ResumeCatch on unknown try region"
            }
            let #(caught_n, e) = state.fresh_var(e)
            use #(dtree, e) <- result.map(e.dispatch.emit_destructure(
              e,
              pat,
              ir.Var(caught_n),
              state.BindLet,
            ))
            #(
              ir.Let(
                [caught_n],
                ir.TermOp(ir.TupleGet(idx), [ctx.loc_v]),
                dtree,
              ),
              e,
            )
          }
          _ -> Ok(#(ir.Values([e.consts.undef]), e))
        })
        let #(pre_n, e) = state.fresh_var(e)
        let k_tail = fn(e_leaf: Emitter2) {
          emit_seg_tail(e_leaf, ctx, arm.tail)
        }
        use #(frag, e2) <- result.map(e.dispatch.emit_stmts(
          e,
          arm.body_fragment,
          k_tail,
        ))
        case arm.resume {
          Some(ResumeBind(..)) | Some(ResumeCatch(_, Some(_))) -> #(
            state.splice_let(prelude, pre_n, frag),
            e2,
          )
          _ -> #(frag, e2)
        }
      }
    })
    #(tree, restore(e2))
  })
}

fn emit_seg_tail(
  e: Emitter2,
  ctx: SmCtx,
  tail: SegTail,
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  case tail {
    BodyEnd -> Ok(#(step_return(e.consts.undef), e))
    // unreachable in practice, keeps the arm well-typed
    SegDone -> Ok(#(step_return(e.consts.undef), e))
    FallTo(to) ->
      Ok(
        cps_pair(e, fn(e, k) {
          pack_loc_cps(e, ctx, dict.new(), fn(e, loc) {
            k(e, sm_continue(ctx, to, loc))
          })
        }),
      )
    FallToFinally(try_id, to) ->
      case find_try_entry(ctx, try_id) {
        Some(entry) ->
          Ok(jump_state_leaf(
            e,
            ctx,
            to,
            dict.from_list([#(entry.pending_loc_idx, ir.ConstAtom("normal"))]),
          ))
        None -> panic as "M18: FallToFinally on unknown try region"
      }
    FinallyEnd(try_id) ->
      case find_try_entry(ctx, try_id) {
        Some(entry) -> {
          let #(pend_n, e) = state.fresh_var(e)
          let #(tree, e) = build_pending_dispatch(e, ctx, entry, ir.Var(pend_n))
          Ok(#(
            ir.Let(
              [pend_n],
              ir.TermOp(ir.TupleGet(entry.pending_loc_idx), [ctx.loc_v]),
              tree,
            ),
            e,
          ))
        }
        None -> panic as "M18: FinallyEnd on unknown try region"
      }
    SplitAt(kind, arg, ns) -> {
      use #(operand_tree, e) <- result.try(case arg {
        Some(ex) -> e.dispatch.emit_expr(e, ex)
        None -> Ok(#(ir.Values([e.consts.undef]), e))
      })
      let #(v_n, e) = state.fresh_var(e)
      case kind {
        SkYieldStar -> {
          let iter_idx = extra_idx(ctx.layout, iter_key(ns))
          let inner_idx = extra_idx(ctx.layout, inner_key(ns))
          let #(setup, e) =
            emit_delegate_setup(e, ctx, ir.Var(v_n), ns, iter_idx, inner_idx)
          Ok(#(ir.Let([v_n], operand_tree, setup), e))
        }
        SkAwait | SkForAwait | SkYield -> {
          let step = fn(v, loc) {
            case kind {
              SkAwait | SkForAwait -> step_await(v, ns, loc)
              SkYield | SkYieldStar -> step_yield(v, ns, loc)
            }
          }
          Ok(
            cps_pair(e, fn(e, k) {
              anf.wrap(
                pack_loc_cps(e, ctx, dict.new(), fn(e, loc) {
                  k(e, step(ir.Var(v_n), loc))
                }),
                ir.Let([v_n], operand_tree, _),
              )
            }),
          )
        }
      }
    }
    CondBranch(cond, then_s, else_s) -> {
      use #(cond_tree, e) <- result.try(case cond {
        Some(ex) -> e.dispatch.emit_expr(e, ex)
        None -> Ok(#(ir.Values([ctx.sent_v]), e))
      })
      let #(cv_n, e) = state.fresh_var(e)
      let #(ti_n, e) = state.fresh_var(e)
      Ok(
        cps_pair(e, fn(e, k) {
          anf.wrap(
            pack_loc_cps(e, ctx, dict.new(), fn(e, loc) {
              k(
                e,
                ir.If(
                  ir.Var(ti_n),
                  [ir.TTerm],
                  sm_continue(ctx, then_s, loc),
                  sm_continue(ctx, else_s, loc),
                ),
              )
            }),
            fn(t) {
              let cv =
                option.unwrap(state.let_tail_value(cond_tree), ir.Var(cv_n))
              state.splice_let(
                cond_tree,
                cv_n,
                ir.Let([ti_n], ir.CallHost("js", "truthy", [cv]), t),
              )
            },
          )
        }),
      )
    }
    ForUpdate(update, head) -> {
      use #(upd_tree, e) <- result.try(case update {
        Some(ex) -> e.dispatch.emit_expr(e, ex)
        None -> Ok(#(ir.Values([e.consts.undef]), e))
      })
      let #(tmp, e) = state.fresh_var(e)
      Ok(
        cps_pair(e, fn(e, k) {
          anf.wrap(
            pack_loc_cps(e, ctx, dict.new(), fn(e, loc) {
              k(e, sm_continue(ctx, head, loc))
            }),
            state.splice_let(upd_tree, tmp, _),
          )
        }),
      )
    }
    ForOfStep(left, iter_key, body_s, after, is_await) ->
      emit_for_of_step(e, ctx, left, iter_key, body_s, after, is_await)
    ForOfSetup(right, iter_key, head) -> {
      use #(rhs_tree, e) <- result.try(e.dispatch.emit_expr(e, right))
      let iter_idx = extra_idx(ctx.layout, iter_key)
      let #(rhs_n, e) = state.fresh_var(e)
      let #(iter_n, e) = state.fresh_var(e)
      Ok(
        cps_pair(e, fn(e, k) {
          anf.wrap(
            pack_loc_cps(
              e,
              ctx,
              dict.from_list([#(iter_idx, ir.Var(iter_n))]),
              fn(e, loc) { k(e, sm_continue(ctx, head, loc)) },
            ),
            fn(t) {
              ir.Let(
                [rhs_n],
                rhs_tree,
                ir.Let(
                  [iter_n],
                  ir.CallHost("js", "get_iterator", [
                    ir.Var(rhs_n),
                    ir.ConstAtom("sync"),
                  ]),
                  t,
                ),
              )
            },
          )
        }),
      )
    }
    ForAwaitSetup(right, head) -> {
      use #(rhs_tree, e) <- result.try(e.dispatch.emit_expr(e, right))
      let iter_idx = extra_idx(ctx.layout, for_await_iter_key(head))
      let #(rhs_n, e) = state.fresh_var(e)
      let #(iter_n, e) = state.fresh_var(e)
      Ok(
        cps_pair(e, fn(e, k) {
          anf.wrap(
            pack_loc_cps(
              e,
              ctx,
              dict.from_list([#(iter_idx, ir.Var(iter_n))]),
              fn(e, loc) { k(e, sm_continue(ctx, head, loc)) },
            ),
            fn(t) {
              ir.Let(
                [rhs_n],
                rhs_tree,
                ir.Let(
                  [iter_n],
                  ir.CallHost("js", "get_iterator", [
                    ir.Var(rhs_n),
                    ir.ConstAtom("async"),
                  ]),
                  t,
                ),
              )
            },
          )
        }),
      )
    }
    AsyncGenYieldSent(ns) ->
      Ok(
        cps_pair(e, fn(e, k) {
          pack_loc_cps(e, ctx, dict.new(), fn(e, loc) {
            k(e, step_yield(ctx.sent_v, ns, loc))
          })
        }),
      )
    SwitchDispatch(disc, tests, after) ->
      emit_switch_dispatch(e, ctx, disc, tests, after)
  }
}

fn cps_pair(
  e: Emitter2,
  f: fn(Emitter2, fn(Emitter2, ir.Expr) -> #(ir.Expr, Emitter2)) ->
    #(ir.Expr, Emitter2),
) -> #(ir.Expr, Emitter2) {
  f(e, fn(ef, tail) { #(tail, ef) })
}

fn emit_for_of_step(
  e: Emitter2,
  ctx: SmCtx,
  left: ast.ForInit,
  iter_key: String,
  body_s: Int,
  after: Int,
  is_await: Bool,
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  let iter_idx = extra_idx(ctx.layout, iter_key)
  run_rk(e, fn(e, done) {
    let #(iter_n, e) = state.fresh_var(e)
    let #(res_n, e) = state.fresh_var(e)
    let res_rhs = case is_await {
      True -> ir.Values([ctx.sent_v])
      False -> ir.CallHost("js", "iter_next", [ir.Var(iter_n)])
    }
    let #(done_t, e) = state.fresh_var(e)
    let #(done_i, e) = state.fresh_var(e)
    let #(val_n, e) = state.fresh_var(e)
    let #(dk_n, e) = state.fresh_var(e)
    let #(vk_n, e) = state.fresh_var(e)
    let done_rhs = case is_await {
      True -> ir.CallHost("js", "get_prop", [ir.Var(res_n), ir.Var(dk_n)])
      False -> ir.TermOp(ir.TupleGet(0), [ir.Var(res_n)])
    }
    let val_rhs = case is_await {
      True -> ir.CallHost("js", "get_prop", [ir.Var(res_n), ir.Var(vk_n)])
      False -> ir.TermOp(ir.TupleGet(1), [ir.Var(res_n)])
    }
    let #(done_branch, e) =
      cps_pair(e, fn(e, k) {
        pack_loc_cps(e, ctx, dict.new(), fn(e, loc) {
          k(e, sm_continue(ctx, after, loc))
        })
      })
    use #(bind_tree, e) <- result.try(bind_for_lhs(e, left, ir.Var(val_n)))
    let #(tmp, e) = state.fresh_var(e)
    let #(body_branch, e) =
      cps_pair(e, fn(e, k) {
        anf.wrap(
          pack_loc_cps(e, ctx, dict.new(), fn(e, loc) {
            k(e, sm_continue(ctx, body_s, loc))
          }),
          fn(t) {
            ir.Let([val_n], val_rhs, state.splice_let(bind_tree, tmp, t))
          },
        )
      })
    let branch = ir.If(ir.Var(done_i), [ir.TTerm], done_branch, body_branch)
    done(
      e,
      ir.Let(
        [iter_n],
        ir.TermOp(ir.TupleGet(iter_idx), [ctx.loc_v]),
        ir.Let(
          [res_n],
          res_rhs,
          ir.Let(
            [dk_n],
            named_key_tuple("done"),
            ir.Let(
              [vk_n],
              named_key_tuple("value"),
              ir.Let(
                [done_t],
                done_rhs,
                ir.Let(
                  [done_i],
                  case is_await {
                    True -> ir.CallHost("js", "truthy", [ir.Var(done_t)])
                    False -> anf.is_true_expr(ir.Var(done_t))
                  },
                  branch,
                ),
              ),
            ),
          ),
        ),
      ),
    )
  })
}

fn named_key_tuple(s: String) -> ir.Expr {
  ir.TermOp(ir.MakeTuple, [ir.ConstAtom("string_key"), anf.fixed_key(s)])
}

fn bind_for_lhs(
  e: Emitter2,
  left: ast.ForInit,
  v: ir.Value,
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  let via = fn(pat, mode) { e.dispatch.emit_destructure(e, pat, v, mode) }
  case left {
    ast.ForInitDeclaration(kind:, declarations: [d]) ->
      via(d.id, bind_mode_of(kind))
    ast.ForInitDeclaration(..) ->
      Error(state.EarlySyntaxError("for-of/in: multiple declarators"))
    ast.ForInitPattern(p) -> via(p, state.BindAssign)
    ast.ForInitExpression(ast.Identifier(span:, name:)) ->
      via(ast.IdentifierPattern(name:, span:), state.BindAssign)
    ast.ForInitExpression(_) ->
      Error(state.UnsupportedFeature("for-of member LHS in coroutine"))
  }
}

fn bind_mode_of(kind: ast.VariableKind) -> state.BindMode {
  case kind {
    ast.Var -> state.BindVar
    ast.Const -> state.BindConst
    ast.Let | ast.Using | ast.AwaitUsing -> state.BindLet
  }
}

fn emit_switch_dispatch(
  e: Emitter2,
  ctx: SmCtx,
  disc: Option(ast.Expression),
  tests: List(#(Option(ast.Expression), Int)),
  after: Int,
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  use #(disc_tree, e) <- result.try(case disc {
    Some(d) -> e.dispatch.emit_expr(e, d)
    None -> Ok(#(ir.Values([ctx.sent_v]), e))
  })
  let #(dv_n, e) = state.fresh_var(e)
  let dv = ir.Var(dv_n)
  use #(chain, e) <- result.try(switch_chain(e, ctx, dv, tests, after))
  Ok(#(ir.Let([dv_n], disc_tree, chain), e))
}

fn switch_chain(
  e: Emitter2,
  ctx: SmCtx,
  dv: ir.Value,
  tests: List(#(Option(ast.Expression), Int)),
  after: Int,
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  case tests {
    [] ->
      Ok(
        cps_pair(e, fn(e, k) {
          pack_loc_cps(e, ctx, dict.new(), fn(e, loc) {
            k(e, sm_continue(ctx, after, loc))
          })
        }),
      )
    [#(None, target), ..] ->
      Ok(
        cps_pair(e, fn(e, k) {
          pack_loc_cps(e, ctx, dict.new(), fn(e, loc) {
            k(e, sm_continue(ctx, target, loc))
          })
        }),
      )
    [#(Some(case_test), target), ..rest] -> {
      use #(test_tree, e) <- result.try(e.dispatch.emit_expr(e, case_test))
      let #(tv_n, e) = state.fresh_var(e)
      let #(eq_n, e) = state.fresh_var(e)
      let #(eqi_n, e) = state.fresh_var(e)
      use #(else_tree, e) <- result.try(switch_chain(e, ctx, dv, rest, after))
      let #(hit, e) =
        cps_pair(e, fn(e, k) {
          pack_loc_cps(e, ctx, dict.new(), fn(e, loc) {
            k(e, sm_continue(ctx, target, loc))
          })
        })
      Ok(#(
        ir.Let(
          [tv_n],
          test_tree,
          ir.Let(
            [eq_n],
            ir.CallHost("js", "strict_eq", [dv, ir.Var(tv_n)]),
            ir.Let(
              [eqi_n],
              ir.CallHost("js", "truthy", [ir.Var(eq_n)]),
              ir.If(ir.Var(eqi_n), [ir.TTerm], hit, else_tree),
            ),
          ),
        ),
        e,
      ))
    }
  }
}

pub type HoistedItem {
  HiStmt(ast.StmtWithLine)
  HiSplit(
    line: Int,
    kind: SplitKind,
    operand: Option(ast.Expression),
    resume: ResumeWith,
  )
}

// rewrite so every recognised await/yield sits at a statement boundary
pub fn hoist_splits_to_stmts(
  stmts: List(ast.StmtWithLine),
) -> List(HoistedItem) {
  list.flat_map(stmts, hoist_one)
}

fn hoist_one(located: ast.StmtWithLine) -> List(HoistedItem) {
  let ast.StmtWithLine(line, stmt) = located
  case stmt {
    ast.ExpressionStatement(expression: ex, ..) ->
      case split_of(ex) {
        Some(#(kind, operand)) -> [HiSplit(line, kind, operand, ResumeDiscard)]
        None ->
          case ex {
            ast.SequenceExpression(_, parts) ->
              list.flat_map(parts, fn(p) {
                hoist_one(ast.StmtWithLine(
                  line,
                  ast.ExpressionStatement(p, None),
                ))
              })
            ast.AssignmentExpression(_, ast.Assign, lhs, rhs) ->
              case split_of(rhs), lhs_to_pattern(lhs) {
                Some(#(kind, operand)), Some(pat) -> [
                  HiSplit(
                    line,
                    kind,
                    operand,
                    ResumeBind(pat, state.BindAssign),
                  ),
                ]
                _, _ -> [HiStmt(located)]
              }
            _ -> [HiStmt(located)]
          }
      }
    ast.VariableDeclaration(kind, [ast.VariableDeclarator(pat, Some(init))]) ->
      case split_of(init) {
        Some(#(skind, operand)) -> [
          HiSplit(line, skind, operand, ResumeBind(pat, bind_mode_of(kind))),
        ]
        None -> [HiStmt(located)]
      }
    ast.VariableDeclaration(kind, decls) ->
      list.flat_map(decls, fn(d) {
        hoist_one(ast.StmtWithLine(line, ast.VariableDeclaration(kind, [d])))
      })
    ast.ReturnStatement(Some(ex)) ->
      case split_of(ex) {
        Some(#(kind, operand)) -> [HiSplit(line, kind, operand, ResumeReturn)]
        None -> [HiStmt(located)]
      }
    ast.ThrowStatement(ex) ->
      case split_of(ex) {
        Some(#(kind, operand)) -> [HiSplit(line, kind, operand, ResumeThrow)]
        None -> [HiStmt(located)]
      }
    _ -> [HiStmt(located)]
  }
}

fn split_of(
  ex: ast.Expression,
) -> Option(#(SplitKind, Option(ast.Expression))) {
  case ex {
    ast.AwaitExpression(_, arg) -> Some(#(SkAwait, Some(arg)))
    ast.YieldExpression(_, arg, is_delegate: False) -> Some(#(SkYield, arg))
    ast.YieldExpression(_, arg, is_delegate: True) -> Some(#(SkYieldStar, arg))
    ast.ParenthesizedExpression(_, inner) -> split_of(inner)
    _ -> None
  }
}

fn lhs_to_pattern(lhs: ast.Expression) -> Option(ast.Pattern) {
  case lhs {
    ast.Identifier(span, name) -> Some(ast.IdentifierPattern(name:, span:))
    ast.ParenthesizedExpression(_, inner) -> lhs_to_pattern(inner)
    _ -> None
  }
}

fn find_try(entries: List(TryEntry), region: Option(Int)) -> Option(TryEntry) {
  case region {
    None -> None
    Some(id) -> list.find(entries, fn(t) { t.id == id }) |> option.from_result
  }
}

fn extra_idx(layout: LocLayout, key: String) -> Int {
  case dict.get(layout.extras, key) {
    Ok(i) -> i
    Error(_) -> panic as { "M18: loc-layout extras missing key " <> key }
  }
}

fn build_switch_arms(
  e: Emitter2,
  ctx: SmCtx,
  plan: SplitPlan,
) -> Result(#(List(ir.SwitchArm), Emitter2), state.EmitError) {
  use #(ctx, e) <- result.try(
    list.try_fold(plan.arms, #(ctx, e), fn(st, arm) {
      let #(ctx, e) = st
      let ctx = with_region(ctx, arm.region)
      let region = current_try(ctx)
      let follow_of =
        list.find(plan.delegates, fn(d) { d.next_state == arm.state_id })
      use #(wrapped, e) <- result.map(case arm.entry_kind, follow_of {
        AeResume(SkYieldStar), Ok(d) -> {
          let #(rv, e) = state.fresh_var(e)
          let idx = extra_idx(ctx.layout, delegate_result_key(d.state_id))
          let arm_ctx = SmCtx(..ctx, sent_v: ir.Var(rv))
          use #(inner, e) <- result.map(emit_arm_body(e, arm_ctx, arm))
          let body =
            ir.Let([rv], ir.TermOp(ir.TupleGet(idx), [ctx.loc_v]), inner)
          #(wrap_arm_try(ctx, arm.state_id, region, body), e)
        }
        _, _ -> {
          use #(inner, e) <- result.map(emit_arm_body(e, ctx, arm))
          let dispatched =
            emit_mode_dispatch(ctx, arm.entry_kind, region, inner)
          #(wrap_arm_try(ctx, arm.state_id, region, dispatched), e)
        }
      })
      #(push_arm(ctx, arm.state_id, wrapped), e)
    }),
  )
  use #(ctx, e) <- result.try(
    list.try_fold(plan.try_entries, #(ctx, e), fn(st, entry) {
      let #(ctx, e) = st
      let outer_region = find_try(plan.try_entries, entry.outer)
      use #(ctx, e) <- result.try(case entry.catch_state, entry.handler {
        Some(cs), Some(h) -> {
          let ctx =
            SmCtx(..with_catch_body(ctx, entry), sm_labels: entry.sm_labels)
          let catch_wrap_region = current_try(ctx)
          let e = case entry.catch_cursor {
            Some(c) -> install_cursor(e, c)
            None -> e
          }
          use #(inner, e) <- result.map(emit_catch_arm(e, ctx, entry, h))
          let wrapped = wrap_arm_try(ctx, cs, catch_wrap_region, inner)
          #(push_arm(ctx, cs, wrapped), e)
        }
        _, _ -> Ok(#(ctx, e))
      })
      case entry.finally_state, entry.finalizer {
        Some(fs), Some(fin) -> {
          let ctx =
            SmCtx(..with_finally_body(ctx, entry), sm_labels: entry.sm_labels)
          let e = case entry.finally_cursor {
            Some(c) -> install_cursor(e, c)
            None -> e
          }
          use #(inner, e) <- result.map(emit_finally_arm(e, ctx, entry, fin))
          let wrapped = wrap_arm_try(ctx, fs, outer_region, inner)
          #(push_arm(ctx, fs, wrapped), e)
        }
        _, _ -> Ok(#(ctx, e))
      }
    }),
  )
  use #(ctx, e) <- result.try(
    list.try_fold(plan.delegates, #(ctx, e), fn(st, d) {
      let #(ctx, e) = st
      let ctx = with_region(ctx, d.region)
      let region = current_try(ctx)
      let sid = int.to_string(d.state_id)
      let iter_idx = extra_idx(ctx.layout, "iter_" <> sid)
      let inner_idx = extra_idx(ctx.layout, "inner_" <> sid)
      let result_idx = extra_idx(ctx.layout, "delegate_result_" <> sid)
      use #(inner, e) <- result.try(emit_delegate_arm(
        e,
        ctx,
        d,
        iter_idx,
        inner_idx,
        result_idx,
      ))
      let wrapped = wrap_arm_try(ctx, d.state_id, region, inner)
      let ctx = push_arm(ctx, d.state_id, wrapped)
      case d.await_state {
        None -> Ok(#(ctx, e))
        Some(na) -> {
          use #(body, e) <- result.map(emit_delegate_await_arm(
            e,
            ctx,
            d,
            iter_idx,
            result_idx,
          ))
          let wrapped =
            wrap_arm_try(
              ctx,
              na,
              region,
              emit_mode_dispatch(ctx, AeResume(SkAwait), region, body),
            )
          #(push_arm(ctx, na, wrapped), e)
        }
      }
    }),
  )
  use #(ctx, e) <- result.map(
    list.try_fold(plan.for_awaits, #(ctx, e), fn(st, fap) {
      let #(ctx, e) = st
      let ctx = with_region(ctx, fap.region)
      let region = current_try(ctx)
      let #(head_body, e) = emit_for_await_head(e, ctx, fap)
      let ctx =
        push_arm(ctx, fap.head, wrap_arm_try(ctx, fap.head, region, head_body))
      use #(check_body, e) <- result.map(emit_for_await_check(e, ctx, fap))
      let check_wrapped =
        wrap_arm_try(
          ctx,
          fap.check,
          region,
          emit_mode_dispatch(ctx, AeResume(SkAwait), region, check_body),
        )
      #(push_arm(ctx, fap.check, check_wrapped), e)
    }),
  )
  #(finish_arms(ctx), e)
}

pub fn emit_coroutine_fn(
  e: Emitter2,
  shape: state.FnShape,
  js_name: Option(String),
  params: List(ast.Pattern),
  body: state.FnBody,
  fn_scope_id: ScopeId,
  captures: List(ir.Value),
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  let assert Some(kind) = func.shape_coroutine(shape)
    as "emit_coroutine_fn: shape is not a coroutine"
  let info = scope.function_info(e.tree, fn_scope_id)
  let ncap = list.length(captures)
  let stmts = func.body_stmts(body)
  let is_strict = e.strict || ast_util.has_use_strict_directive(stmts)
  let #(outer_name, e) = state.fresh_fn_name(e, js_name)
  let sm_name = outer_name <> "__sm"
  let enter = fn(e) {
    state.enter_function(
      e,
      fn_scope_id,
      strict: is_strict,
      is_async: kind_is_async(kind),
      is_generator: kind_is_gen(kind),
      is_arrow: func.shape_is_arrow(shape),
    )
  }
  let #(e_outer, save) = enter(e)
  let e_outer = func.seed_capture_slots(e_outer, info)
  use #(body_expr, e_outer) <- result.try({
    use e_pro, finish <- func.emit_prologue(
      e_outer,
      func.shape_self_name(shape),
      func.shape_is_arrow(shape),
      False,
      params,
      stmts,
      info,
    )
    let cur0 = capture_cursor(e_pro)
    let plan = analyze_splits(e_pro.tree, cur0, body, kind)
    let #(sm_tree, info) =
      add_temp_slots(e_pro.tree, fn_scope_id, info, plan.n_temps)
    let layout = compute_loc_layout(info, plan)
    let plan =
      SplitPlan(
        ..plan,
        try_entries: enrich_try_entries(plan.try_entries, layout),
      )
    let #(e_sm, sm_save) = enter(e_pro)
    let e_sm = state.Emitter2(..e_sm, cap_names: e_pro.cap_names)
    let e_sm =
      state.Emitter2(
        ..install_cursor(e_sm, cur0),
        initialized: e_pro.initialized,
        tree: sm_tree,
      )
    let #(lresume, e_sm) = state.fresh_label(e_sm)
    let ctx = new_sm_ctx(kind, layout, lresume, plan)
    use #(arms, e_sm) <- result.try(build_switch_arms(e_sm, ctx, plan))
    let #(default, e_sm) = sm_default_arm(e_sm)
    let e_sm = emit_sm_function(e_sm, sm_name, ncap, lresume, arms, default)
    let sm_keys = e_sm.uses_keys
    let e_pro = state.leave_function(e_sm, sm_save)
    let #(tree, e_pro) =
      anf.run_to(
        {
          use loc0 <- anf.then(
            anf.make_tuple(initial_loc_values(e_pro, layout, info.local_count)),
          )
          use sm <- anf.then(
            anf.bind(ir.MakeClosure(
              sm_name,
              list.append(cap_vars(e_pro, 0, ncap), state.keys_args(sm_keys)),
              3,
            )),
          )
          anf.host(start_op(kind), [
            sm,
            ir.Var("_frame"),
            ir.Var("_args"),
            loc0,
          ])
        },
        e_pro,
        fn(_e, h) { ir.Values([h]) },
      )
    Ok(#(tree, finish(e_pro)))
  })
  let #(ex, e_outer) = state.fresh_var(e_outer)
  let #(res, e_outer) = state.fresh_var(e_outer)
  let body_expr = case kind {
    // §27.7.5.1 async param-default throw rejects; generators throw sync
    state.CorAsync ->
      ir.Try(result: [ir.TTerm], body: body_expr, handlers: [
        ir.CatchHandler(
          on: ir.OnTag("js_exn"),
          payload: [ex],
          exnref: None,
          handler: ir.CallHost("js", "async_reject", [ir.Var(ex)]),
        ),
      ])
    state.CorGenerator | state.CorAsyncGen -> body_expr
  }
  let body_expr = ir.Let([res], body_expr, ir.Return([ir.Var(res)]))
  let e_outer =
    state.add_function(
      e_outer,
      ir.Function(
        name: outer_name,
        params: build_outer_params(e_outer, 0, ncap),
        result: [ir.TTerm],
        locals: [],
        body: body_expr,
      ),
    )
  let captures = list.append(captures, state.keys_args(e_outer.uses_keys))
  let e = state.leave_function(e_outer, save)
  Ok(emit_closure_site(
    e,
    outer_name,
    kind,
    shape,
    is_strict,
    js_name,
    params,
    captures,
  ))
}

fn add_temp_slots(
  tree: ScopeTree,
  fn_scope_id: ScopeId,
  info: scope.FunctionInfo,
  n: Int,
) -> #(ScopeTree, scope.FunctionInfo) {
  case n {
    0 -> #(tree, info)
    _ -> {
      let sc = scope.get_scope(tree, fn_scope_id)
      let bindings =
        list.repeat(Nil, n)
        |> list.index_map(fn(_, i) { i })
        |> list.fold(sc.bindings, fn(bs, i) {
          dict.insert(
            bs,
            temp_name(i),
            scope.Binding(
              slot: info.local_count + i,
              kind: scope.VarBinding,
              is_boxed: False,
              origin_kind_for_capture: scope.VarBinding,
            ),
          )
        })
      let scopes =
        dict.insert(tree.scopes, fn_scope_id, scope.Scope(..sc, bindings:))
      #(
        scope.ScopeTree(..tree, scopes:),
        scope.FunctionInfo(..info, local_count: info.local_count + n),
      )
    }
  }
}

fn emit_for_await_head(
  e: Emitter2,
  ctx: SmCtx,
  fap: ForAwaitSpec,
) -> #(ir.Expr, Emitter2) {
  let iter_idx = extra_idx(ctx.layout, for_await_iter_key(fap.head))
  let b = {
    use iter_h <- anf.then(anf.bind(anf.tuple_get(ctx.loc_v, iter_idx)))
    use p <- anf.then(anf.host("async_iter_next", [iter_h]))
    use loc2 <- anf.then(pack_loc(ctx, dict.new()))
    anf.pure(step_await(p, fap.check, loc2))
  }
  run_terminal(b, e)
}

fn emit_for_await_check(
  e: Emitter2,
  ctx: SmCtx,
  fap: ForAwaitSpec,
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  run_rk(e, fn(e, done) {
    use e <- restore_and_seed(e, ctx)
    let e = install_cursor(e, fap.body_cursor)
    let #(done_branch, e) =
      anf.run_to(pack_loc(ctx, dict.new()), e, fn(_e, loc2) {
        ir.Continue(ctx.lresume, [ir.ConstI32(fap.after), loc2])
      })
    let #(val_name, e) = state.fresh_var(e)
    use #(bind_tree, e) <- result.try(bind_for_lhs(
      e,
      fap.left,
      ir.Var(val_name),
    ))
    let #(drop, e) = state.fresh_var(e)
    let #(body_jump, e) = jump_state_leaf(e, ctx, fap.body_s, dict.new())
    let not_done = state.splice_let(bind_tree, drop, body_jump)
    let #(chain, e) =
      run_terminal(
        {
          use done_jv <- anf.then(get_named(ctx.sent_v, "done"))
          use done_i <- anf.then(anf.host("truthy", [done_jv]))
          use value <- anf.then(get_named(ctx.sent_v, "value"))
          anf.pure(ir.Let(
            [val_name],
            ir.Values([value]),
            ir.If(done_i, [ir.TTerm], done_branch, not_done),
          ))
        },
        e,
      )
    done(e, chain)
  })
}

const temp_prefix = "%sm"

fn temp_name(i: Int) -> String {
  temp_prefix <> int.to_string(i)
}

fn is_temp_name(name: String) -> Bool {
  string.starts_with(name, temp_prefix)
}

fn fresh_temp(a: Ana) -> #(String, Ana) {
  #(temp_name(a.next_temp), Ana(..a, next_temp: a.next_temp + 1))
}

type Lin =
  #(Ana, List(ast.StmtWithLine), ast.Expression)

fn ident(span: ast.Span, name: String) -> ast.Expression {
  ast.Identifier(span:, name:)
}

fn assign_stmt(
  line: Int,
  span: ast.Span,
  name: String,
  ex: ast.Expression,
) -> ast.StmtWithLine {
  ast.StmtWithLine(
    line:,
    statement: ast.ExpressionStatement(
      ast.AssignmentExpression(span, ast.Assign, ident(span, name), ex),
      None,
    ),
  )
}

fn expr_stmt(line: Int, ex: ast.Expression) -> ast.StmtWithLine {
  ast.StmtWithLine(line:, statement: ast.ExpressionStatement(ex, None))
}

fn block_of(line: Int, stmts: List(ast.StmtWithLine)) -> ast.Statement {
  case stmts {
    [ast.StmtWithLine(statement: only, ..)] -> only
    _ -> ast.BlockStatement(stmts)
  }
  |> fn(s) {
    case s {
      ast.BlockStatement(..) -> s
      _ -> ast.BlockStatement([ast.StmtWithLine(line:, statement: s)])
    }
  }
}

fn is_trivial(ex: ast.Expression) -> Bool {
  case ex {
    ast.NumberLiteral(..)
    | ast.BigIntLiteral(..)
    | ast.StringExpression(..)
    | ast.BooleanLiteral(..)
    | ast.NullLiteral(..)
    | ast.UndefinedExpression(..)
    | ast.ThisExpression(..)
    | ast.MetaProperty(..)
    | // functions stay put so NamedEvaluation is undisturbed
      ast.FunctionExpression(..)
    | ast.ArrowFunctionExpression(..) -> True
    ast.ClassExpression(super_class: sc, body:, ..) ->
      option.is_none(sc) && !list.any(body, class_element_has_effects)
    ast.Identifier(name:, ..) ->
      is_temp_name(name) || string.starts_with(name, "#")
    ast.ParenthesizedExpression(_, inner) -> is_trivial(inner)
    _ -> False
  }
}

fn class_element_has_effects(el: ast.ClassElement) -> Bool {
  case el {
    ast.ClassMethod(key: ast.KeyComputed(_), ..)
    | ast.ClassField(key: ast.KeyComputed(_), ..)
    | ast.ClassField(is_static: True, ..)
    | ast.StaticBlock(..) -> True
    ast.ClassMethod(..) | ast.ClassField(..) -> False
  }
}

fn pin(a: Ana, line: Int, ex: ast.Expression) -> Lin {
  case is_trivial(ex) {
    True -> #(a, [], ex)
    False -> {
      let span = ast.expression_span(ex)
      let #(t, a) = fresh_temp(a)
      case ex {
        ast.SpreadElement(sspan, arg) -> {
          let arr =
            ast.ArrayExpression(span, [Some(ast.SpreadElement(sspan, arg))])
          #(
            a,
            [assign_stmt(line, span, t, arr)],
            ast.SpreadElement(sspan, ident(span, t)),
          )
        }
        ast.ClassExpression(..) -> {
          let zero = ast.NumberLiteral(span, ast.FiniteNumber(0.0))
          let seq = ast.SequenceExpression(span, [zero, ex])
          #(a, [assign_stmt(line, span, t, seq)], ident(span, t))
        }
        _ -> #(a, [assign_stmt(line, span, t, ex)], ident(span, t))
      }
    }
  }
}

fn needs_explode(ex: ast.Expression) -> Bool {
  case split_of(ex) {
    Some(#(_, Some(op))) -> expr_has_split(op)
    Some(#(_, None)) -> False
    None -> expr_has_split(ex)
  }
}

fn top(a: Ana, line: Int, ex: ast.Expression) -> Lin {
  case split_of(ex) {
    Some(#(kind, Some(op))) ->
      case expr_has_split(op) {
        False -> #(a, [], ex)
        True -> {
          let #(a, pre, op2) = lin(a, line, op)
          let span = ast.expression_span(ex)
          let rebuilt = case kind {
            SkAwait -> ast.AwaitExpression(span, op2)
            SkYield -> ast.YieldExpression(span, Some(op2), False)
            SkYieldStar -> ast.YieldExpression(span, Some(op2), True)
            SkForAwait -> ex
          }
          #(a, pre, rebuilt)
        }
      }
    Some(#(_, None)) -> #(a, [], ex)
    None -> lin(a, line, ex)
  }
}

fn lin(a: Ana, line: Int, ex: ast.Expression) -> Lin {
  case expr_has_split(ex) {
    False -> #(a, [], ex)
    True -> lin_split(a, line, ex)
  }
}

fn lin_opt(
  a: Ana,
  line: Int,
  o: Option(ast.Expression),
) -> #(Ana, List(ast.StmtWithLine), Option(ast.Expression)) {
  case o {
    None -> #(a, [], None)
    Some(ex) -> {
      let #(a, pre, ex2) = lin(a, line, ex)
      #(a, pre, Some(ex2))
    }
  }
}

fn lin_split(a: Ana, line: Int, ex: ast.Expression) -> Lin {
  case ex {
    ast.AwaitExpression(span, arg) -> {
      let #(a, pre, arg2) = lin(a, line, arg)
      let #(t, a) = fresh_temp(a)
      #(
        a,
        list.append(pre, [
          assign_stmt(line, span, t, ast.AwaitExpression(span, arg2)),
        ]),
        ident(span, t),
      )
    }
    ast.YieldExpression(span, arg, del) -> {
      let #(a, pre, arg2) = lin_opt(a, line, arg)
      let #(t, a) = fresh_temp(a)
      #(
        a,
        list.append(pre, [
          assign_stmt(line, span, t, ast.YieldExpression(span, arg2, del)),
        ]),
        ident(span, t),
      )
    }
    ast.ParenthesizedExpression(span, inner) -> {
      let #(a, pre, inner2) = lin(a, line, inner)
      #(a, pre, ast.ParenthesizedExpression(span, inner2))
    }
    ast.SpreadElement(span, arg) -> {
      let #(a, pre, arg2) = lin(a, line, arg)
      #(a, pre, ast.SpreadElement(span, arg2))
    }
    ast.BinaryExpression(span, op, l, r) -> {
      let #(a, pre, xs) = lin_list(a, line, [l, r])
      case xs {
        [l2, r2] -> #(a, pre, ast.BinaryExpression(span, op, l2, r2))
        _ -> #(a, pre, ex)
      }
    }
    ast.LogicalExpression(span, op, l, r) ->
      case expr_has_split(r) {
        False -> {
          let #(a, pre, l2) = lin(a, line, l)
          #(a, pre, ast.LogicalExpression(span, op, l2, r))
        }
        True -> {
          let #(a, pre_l, l2) = lin(a, line, l)
          let #(t, a) = fresh_temp(a)
          let #(a, pre_r, r2) = lin(a, line, r)
          let guard =
            ast.IfStatement(
              logical_test(span, op, t),
              block_of(
                line,
                list.append(pre_r, [assign_stmt(line, span, t, r2)]),
              ),
              None,
            )
          #(
            a,
            list.append(pre_l, [
              assign_stmt(line, span, t, l2),
              ast.StmtWithLine(line:, statement: guard),
            ]),
            ident(span, t),
          )
        }
      }
    ast.ConditionalExpression(span, c, x, y) ->
      case expr_has_split(x) || expr_has_split(y) {
        False -> {
          let #(a, pre, c2) = lin(a, line, c)
          #(a, pre, ast.ConditionalExpression(span, c2, x, y))
        }
        True -> {
          let #(a, pre_c, c2) = lin(a, line, c)
          let #(t, a) = fresh_temp(a)
          let #(a, pre_x, x2) = lin(a, line, x)
          let #(a, pre_y, y2) = lin(a, line, y)
          let branch =
            ast.IfStatement(
              c2,
              block_of(
                line,
                list.append(pre_x, [assign_stmt(line, span, t, x2)]),
              ),
              Some(block_of(
                line,
                list.append(pre_y, [assign_stmt(line, span, t, y2)]),
              )),
            )
          #(
            a,
            list.append(pre_c, [ast.StmtWithLine(line:, statement: branch)]),
            ident(span, t),
          )
        }
      }
    ast.UnaryExpression(span, op, arg) -> {
      let #(a, pre, arg2) = lin(a, line, arg)
      #(a, pre, ast.UnaryExpression(span, op, arg2))
    }
    ast.UpdateExpression(span, op, prefix, arg) ->
      case arg {
        ast.MemberExpression(mspan, obj, prop) -> {
          let #(a, pre, obj2, prop2) = lin_member(a, line, obj, prop, False)
          #(
            a,
            pre,
            ast.UpdateExpression(
              span,
              op,
              prefix,
              ast.MemberExpression(mspan, obj2, prop2),
            ),
          )
        }
        _ -> #(a, [], ex)
      }
    ast.AssignmentExpression(span, op, lhs, rhs) ->
      lin_assign(a, line, span, op, lhs, rhs)
    ast.CallExpression(span, callee, args) -> {
      let #(a, pre_c, callee2) = lin_callee(a, line, callee, args)
      let #(a, pre_a, args2) = lin_list(a, line, args)
      #(a, list.append(pre_c, pre_a), ast.CallExpression(span, callee2, args2))
    }
    ast.NewExpression(span, callee, args) -> {
      let #(a, pre_c, callee2) = lin(a, line, callee)
      let #(a, pre_p, callee3) = case list.any(args, expr_has_split) {
        True -> pin(a, line, callee2)
        False -> #(a, [], callee2)
      }
      let #(a, pre_a, args2) = lin_list(a, line, args)
      #(
        a,
        list.flatten([pre_c, pre_p, pre_a]),
        ast.NewExpression(span, callee3, args2),
      )
    }
    ast.MemberExpression(span, obj, prop) -> {
      let #(a, pre, obj2, prop2) = lin_member(a, line, obj, prop, False)
      #(a, pre, ast.MemberExpression(span, obj2, prop2))
    }
    ast.ArrayExpression(span, elems) -> {
      let present = list.filter_map(elems, option.to_result(_, Nil))
      let #(a, pre, xs) = lin_list(a, line, present)
      let #(_, elems2) =
        list.map_fold(elems, xs, fn(rest, el) {
          case el, rest {
            None, _ -> #(rest, None)
            Some(_), [x, ..more] -> #(more, Some(x))
            Some(orig), [] -> #([], Some(orig))
          }
        })
      #(a, pre, ast.ArrayExpression(span, elems2))
    }
    ast.ObjectExpression(span, props) -> {
      let items =
        list.flat_map(props, fn(p) {
          case p {
            ast.InitProperty(key: ast.KeyComputed(k), value: v, ..) -> [k, v]
            ast.InitProperty(value: v, ..) -> [v]
            ast.MethodProperty(key: ast.KeyComputed(k), ..)
            | ast.AccessorProperty(key: ast.KeyComputed(k), ..) -> [k]
            ast.MethodProperty(..) | ast.AccessorProperty(..) -> []
            ast.SpreadProperty(argument: arg) -> [arg]
          }
        })
      let #(a, pre, xs) = lin_list(a, line, items)
      let #(_, props2) =
        list.map_fold(props, xs, fn(rest, p) {
          case p, rest {
            ast.InitProperty(key: ast.KeyComputed(_), value: _, shorthand: sh),
              [k2, v2, ..more]
            -> #(more, ast.InitProperty(ast.KeyComputed(k2), v2, sh))
            ast.InitProperty(key: k, value: _, shorthand: sh), [v2, ..more] -> #(
              more,
              ast.InitProperty(k, v2, sh),
            )
            ast.MethodProperty(key: ast.KeyComputed(_), value: f), [k2, ..more]
            -> #(more, ast.MethodProperty(ast.KeyComputed(k2), f))
            ast.AccessorProperty(key: ast.KeyComputed(_), value: f, kind: kd),
              [k2, ..more]
            -> #(more, ast.AccessorProperty(ast.KeyComputed(k2), f, kd))
            ast.SpreadProperty(_), [arg2, ..more] -> #(
              more,
              ast.SpreadProperty(arg2),
            )
            _, _ -> #(rest, p)
          }
        })
      #(a, pre, ast.ObjectExpression(span, props2))
    }
    ast.SequenceExpression(span, parts) -> {
      let #(a, pre, parts2) = lin_list(a, line, parts)
      #(a, pre, ast.SequenceExpression(span, parts2))
    }
    ast.TemplateLiteral(span, parts) -> {
      let #(a, pre, exprs2) = lin_list(a, line, ast.template_expressions(parts))
      #(a, pre, ast.TemplateLiteral(span, rebuild_template(parts, exprs2)))
    }
    ast.TaggedTemplateExpression(span, tag, parts) -> {
      let exprs = ast.template_expressions(parts)
      let #(a, pre_t, tag2) = lin_callee(a, line, tag, exprs)
      let #(a, pre_e, exprs2) = lin_list(a, line, exprs)
      #(
        a,
        list.append(pre_t, pre_e),
        ast.TaggedTemplateExpression(
          span,
          tag2,
          rebuild_template(parts, exprs2),
        ),
      )
    }
    ast.ClassExpression(span, name, super_class, body) -> {
      let #(a, pre, super2, body2) = lin_class(a, line, super_class, body)
      #(a, pre, ast.ClassExpression(span, name, super2, body2))
    }
    _ -> #(a, [], ex)
  }
}

fn lin_class(
  a: Ana,
  line: Int,
  super_class: Option(ast.Expression),
  body: List(ast.ClassElement),
) -> #(
  Ana,
  List(ast.StmtWithLine),
  Option(ast.Expression),
  List(ast.ClassElement),
) {
  let keys =
    list.flat_map(body, fn(el) {
      case el {
        ast.ClassMethod(key: ast.KeyComputed(k), ..)
        | ast.ClassField(key: ast.KeyComputed(k), ..) -> [k]
        _ -> []
      }
    })
  let items = case super_class {
    Some(sc) -> [sc, ..keys]
    None -> keys
  }
  let #(a, pre, xs) = lin_list(a, line, items)
  let #(super2, rest) = case super_class, xs {
    Some(_), [sc2, ..rest] -> #(Some(sc2), rest)
    _, _ -> #(super_class, xs)
  }
  let #(_, body2) =
    list.map_fold(body, rest, fn(rest, el) {
      case el, rest {
        ast.ClassMethod(
          key: ast.KeyComputed(_),
          value: v,
          kind: kd,
          is_static: st,
        ),
          [k2, ..more]
        -> #(more, ast.ClassMethod(ast.KeyComputed(k2), v, kd, st))
        ast.ClassField(key: ast.KeyComputed(_), value: v, is_static: st),
          [k2, ..more]
        -> #(more, ast.ClassField(ast.KeyComputed(k2), v, st))
        _, _ -> #(rest, el)
      }
    })
  #(a, pre, super2, body2)
}

fn rebuild_template(
  parts: ast.TemplateParts(q),
  exprs: List(ast.Expression),
) -> ast.TemplateParts(q) {
  let #(_, tail) =
    list.map_fold(parts.tail, exprs, fn(rest, part) {
      case rest {
        [x, ..more] -> #(more, #(x, part.1))
        [] -> #([], part)
      }
    })
  ast.TemplateParts(head: parts.head, tail:)
}

fn logical_test(
  span: ast.Span,
  op: ast.LogicalOp,
  t: String,
) -> ast.Expression {
  case op {
    ast.LogicalAnd -> ident(span, t)
    ast.LogicalOr -> ast.UnaryExpression(span, ast.LogicalNot, ident(span, t))
    ast.NullishCoalescing ->
      ast.BinaryExpression(
        span,
        ast.Equal,
        ident(span, t),
        ast.NullLiteral(span),
      )
  }
}

fn lin_list(
  a: Ana,
  line: Int,
  xs: List(ast.Expression),
) -> #(Ana, List(ast.StmtWithLine), List(ast.Expression)) {
  let last_split =
    list.index_fold(xs, -1, fn(acc, x, i) {
      case expr_has_split(x) {
        True -> i
        False -> acc
      }
    })
  let #(#(a, pre_rev), xs2) =
    list.index_map(xs, fn(x, i) { #(x, i) })
    |> list.map_fold(#(a, []), fn(st, xi) {
      let #(a, pre_rev) = st
      let #(x, i) = xi
      case i < last_split, i == last_split {
        True, _ -> {
          let #(a, pre1, x2) = lin(a, line, x)
          let #(a, pre2, x3) = pin(a, line, x2)
          #(#(a, [pre2, pre1, ..pre_rev]), x3)
        }
        _, True -> {
          let #(a, pre1, x2) = lin(a, line, x)
          #(#(a, [pre1, ..pre_rev]), x2)
        }
        _, _ -> #(st, x)
      }
    })
  #(a, list.flatten(list.reverse(pre_rev)), xs2)
}

fn lin_member(
  a: Ana,
  line: Int,
  obj: ast.Expression,
  prop: ast.MemberProperty,
  later: Bool,
) -> #(Ana, List(ast.StmtWithLine), ast.Expression, ast.MemberProperty) {
  case obj {
    ast.SuperExpression(..) -> {
      let #(a, pre, prop2) = lin_prop(a, line, prop, later)
      #(a, pre, obj, prop2)
    }
    _ -> {
      let #(a, pre_o, obj2) = lin(a, line, obj)
      let #(a, pre_p, obj3) = case later || member_prop_has_split(prop) {
        True -> pin(a, line, obj2)
        False -> #(a, [], obj2)
      }
      let #(a, pre_k, prop2) = lin_prop(a, line, prop, later)
      #(a, list.flatten([pre_o, pre_p, pre_k]), obj3, prop2)
    }
  }
}

fn lin_prop(
  a: Ana,
  line: Int,
  prop: ast.MemberProperty,
  later: Bool,
) -> #(Ana, List(ast.StmtWithLine), ast.MemberProperty) {
  case prop {
    ast.Dot(..) -> #(a, [], prop)
    ast.Bracket(k) -> {
      let #(a, pre_k, k2) = lin(a, line, k)
      let #(a, pre_p, k3) = case later {
        True -> pin(a, line, k2)
        False -> #(a, [], k2)
      }
      #(a, list.append(pre_k, pre_p), ast.Bracket(k3))
    }
  }
}

fn lin_callee(
  a: Ana,
  line: Int,
  callee: ast.Expression,
  args: List(ast.Expression),
) -> Lin {
  let later = list.any(args, expr_has_split)
  case callee {
    ast.MemberExpression(span, obj, prop) -> {
      let #(a, pre, obj2, prop2) = lin_member(a, line, obj, prop, later)
      #(a, pre, ast.MemberExpression(span, obj2, prop2))
    }
    ast.ParenthesizedExpression(_, inner) -> lin_callee(a, line, inner, args)
    _ -> {
      let #(a, pre_c, callee2) = lin(a, line, callee)
      case later {
        True -> {
          let #(a, pre_p, callee3) = pin(a, line, callee2)
          #(a, list.append(pre_c, pre_p), callee3)
        }
        False -> #(a, pre_c, callee2)
      }
    }
  }
}

fn compound_binop(op: ast.AssignmentOp) -> Option(ast.BinaryOp) {
  case op {
    ast.AddAssign -> Some(ast.Add)
    ast.SubtractAssign -> Some(ast.Subtract)
    ast.MultiplyAssign -> Some(ast.Multiply)
    ast.DivideAssign -> Some(ast.Divide)
    ast.ModuloAssign -> Some(ast.Modulo)
    ast.ExponentiationAssign -> Some(ast.Exponentiation)
    ast.LeftShiftAssign -> Some(ast.LeftShift)
    ast.RightShiftAssign -> Some(ast.RightShift)
    ast.UnsignedRightShiftAssign -> Some(ast.UnsignedRightShift)
    ast.BitwiseAndAssign -> Some(ast.BitwiseAnd)
    ast.BitwiseOrAssign -> Some(ast.BitwiseOr)
    ast.BitwiseXorAssign -> Some(ast.BitwiseXor)
    ast.Assign
    | ast.LogicalAndAssign
    | ast.LogicalOrAssign
    | ast.NullishCoalesceAssign -> None
  }
}

fn logical_assign_op(op: ast.AssignmentOp) -> Option(ast.LogicalOp) {
  case op {
    ast.LogicalAndAssign -> Some(ast.LogicalAnd)
    ast.LogicalOrAssign -> Some(ast.LogicalOr)
    ast.NullishCoalesceAssign -> Some(ast.NullishCoalescing)
    _ -> None
  }
}

fn lin_assign(
  a: Ana,
  line: Int,
  span: ast.Span,
  op: ast.AssignmentOp,
  lhs: ast.Expression,
  rhs: ast.Expression,
) -> Lin {
  let target = case lhs {
    ast.Identifier(..) -> Some(#(a, [], lhs))
    ast.MemberExpression(mspan, obj, prop) ->
      case obj {
        ast.SuperExpression(..) -> None
        _ -> {
          let #(a, pre, obj2, prop2) = lin_member(a, line, obj, prop, True)
          Some(#(a, pre, ast.MemberExpression(mspan, obj2, prop2)))
        }
      }
    _ -> None
  }
  case target, op, compound_binop(op), logical_assign_op(op) {
    None, _, _, _ ->
      case expr_has_split(lhs) {
        True -> #(a, [], ast.AssignmentExpression(span, op, lhs, rhs))
        False -> {
          let #(a, pre, rhs2) = lin(a, line, rhs)
          #(a, pre, ast.AssignmentExpression(span, op, lhs, rhs2))
        }
      }
    Some(#(a, pre_t, ref)), ast.Assign, _, _ -> {
      let #(a, pre_r, rhs2) = lin(a, line, rhs)
      #(
        a,
        list.append(pre_t, pre_r),
        ast.AssignmentExpression(span, ast.Assign, ref, rhs2),
      )
    }
    Some(#(a, pre_t, ref)), _, Some(bop), _ -> {
      let #(t, a) = fresh_temp(a)
      let #(a, pre_r, rhs2) = lin(a, line, rhs)
      #(
        a,
        list.flatten([pre_t, [assign_stmt(line, span, t, ref)], pre_r]),
        ast.AssignmentExpression(
          span,
          ast.Assign,
          ref,
          ast.BinaryExpression(span, bop, ident(span, t), rhs2),
        ),
      )
    }
    Some(#(a, pre_t, ref)), _, _, Some(lop) -> {
      let #(t, a) = fresh_temp(a)
      let #(a, pre_r, rhs2) = lin(a, line, rhs)
      let guard =
        ast.IfStatement(
          logical_test(span, lop, t),
          block_of(
            line,
            list.append(pre_r, [
              assign_stmt(
                line,
                span,
                t,
                ast.AssignmentExpression(span, ast.Assign, ref, rhs2),
              ),
            ]),
          ),
          None,
        )
      #(
        a,
        list.flatten([
          pre_t,
          [assign_stmt(line, span, t, ref)],
          [ast.StmtWithLine(line:, statement: guard)],
        ]),
        ident(span, t),
      )
    }
    Some(#(a, _, _)), _, _, _ -> #(
      a,
      [],
      ast.AssignmentExpression(span, op, lhs, rhs),
    )
  }
}

fn explode_stmt(
  a: Ana,
  sl: ast.StmtWithLine,
) -> Option(#(Ana, List(ast.StmtWithLine))) {
  let ast.StmtWithLine(line:, statement: s) = sl
  let done = fn(a, pre, stmt) {
    // unchanged rewrite must be None or the planner loops forever
    case pre, stmt == s {
      [], True -> None
      _, _ ->
        Some(#(a, list.append(pre, [ast.StmtWithLine(line:, statement: stmt)])))
    }
  }
  case s {
    ast.ExpressionStatement(expression: ex, directive: dir) ->
      case ex {
        ast.SequenceExpression(_, parts) ->
          Some(#(a, list.map(parts, expr_stmt(line, _))))
        ast.AssignmentExpression(
          span,
          ast.Assign,
          ast.Identifier(..) as lhs,
          rhs,
        ) ->
          case needs_explode(rhs) {
            False -> None
            True -> {
              let #(a, pre, rhs2) = top(a, line, rhs)
              done(
                a,
                pre,
                ast.ExpressionStatement(
                  ast.AssignmentExpression(span, ast.Assign, lhs, rhs2),
                  dir,
                ),
              )
            }
          }
        _ ->
          case needs_explode(ex) {
            False -> None
            True -> {
              let #(a, pre, ex2) = top(a, line, ex)
              done(a, pre, ast.ExpressionStatement(ex2, dir))
            }
          }
      }
    ast.ReturnStatement(Some(ex)) ->
      case needs_explode(ex) {
        False -> None
        True -> {
          let #(a, pre, ex2) = top(a, line, ex)
          done(a, pre, ast.ReturnStatement(Some(ex2)))
        }
      }
    ast.ThrowStatement(ex) ->
      case needs_explode(ex) {
        False -> None
        True -> {
          let #(a, pre, ex2) = top(a, line, ex)
          done(a, pre, ast.ThrowStatement(ex2))
        }
      }
    ast.VariableDeclaration(kind, [ast.VariableDeclarator(pat, Some(init))]) ->
      case pattern_has_split(pat) || !needs_explode(init) {
        True -> None
        False -> {
          let #(a, pre, init2) = top(a, line, init)
          done(
            a,
            pre,
            ast.VariableDeclaration(kind, [
              ast.VariableDeclarator(pat, Some(init2)),
            ]),
          )
        }
      }
    ast.VariableDeclaration(kind, decls) ->
      case
        list.length(decls) > 1
        && list.any(decls, fn(d: ast.VariableDeclarator) {
          case d.init {
            Some(i) -> needs_explode(i)
            None -> False
          }
        })
      {
        False -> None
        True ->
          Some(#(
            a,
            list.map(decls, fn(d) {
              ast.StmtWithLine(
                line:,
                statement: ast.VariableDeclaration(kind, [d]),
              )
            }),
          ))
      }
    ast.IfStatement(condition: c, consequent: t, alternate: f) ->
      case needs_explode(c) {
        False -> None
        True -> {
          let #(a, pre, c2) = top(a, line, c)
          done(a, pre, ast.IfStatement(c2, t, f))
        }
      }
    ast.WhileStatement(condition: c, body: b) ->
      case needs_explode(c) {
        False -> None
        True -> {
          let #(a, pre, c2) = lin(a, line, c)
          done(a, [], loop_with_test(line, c2, pre, b))
        }
      }
    ast.ForStatement(init: i, condition: c, update: u, body: b) -> {
      let init_split = case i {
        Some(fi) -> for_init_has_split(fi)
        None -> False
      }
      let cond_split = case c {
        Some(ce) -> needs_explode(ce)
        None -> False
      }
      let update_split = opt_expr_has_split(u)
      case update_split || { !init_split && !cond_split } {
        True -> None
        False -> {
          let hoisted = case i, init_split {
            Some(ast.ForInitExpression(e)), True ->
              Some(#(a, [expr_stmt(line, e)]))
            Some(ast.ForInitDeclaration(kind: ast.Var, declarations: ds)), True
            ->
              Some(#(
                a,
                list.map(ds, fn(d) {
                  ast.StmtWithLine(
                    line:,
                    statement: ast.VariableDeclaration(ast.Var, [d]),
                  )
                }),
              ))
            _, True -> None
            _, False -> Some(#(a, []))
          }
          case hoisted {
            None -> None
            Some(#(a, pre_i)) -> {
              let init2 = case init_split {
                True -> None
                False -> i
              }
              case c, cond_split {
                Some(ce), True -> {
                  let #(a, pre_c, c2) = lin(a, line, ce)
                  done(
                    a,
                    pre_i,
                    ast.ForStatement(
                      init2,
                      None,
                      u,
                      head_test_block(line, c2, pre_c, b),
                    ),
                  )
                }
                _, _ -> done(a, pre_i, ast.ForStatement(init2, c, u, b))
              }
            }
          }
        }
      }
    }
    ast.ForOfStatement(left: l, right: r, body: b, is_await: aw) ->
      case expr_has_split(r) && !for_init_has_split(l) {
        False -> None
        True -> {
          let #(a, pre, r2) = lin(a, line, r)
          done(a, pre, ast.ForOfStatement(l, r2, b, aw))
        }
      }
    ast.ForInStatement(left: l, right: r, body: b) ->
      case expr_has_split(r) && !for_init_has_split(l) {
        False -> None
        True -> {
          let #(a, pre, r2) = lin(a, line, r)
          done(a, pre, ast.ForInStatement(l, r2, b))
        }
      }
    ast.SwitchStatement(discriminant: d, cases: cs) ->
      case needs_explode(d) {
        False -> None
        True -> {
          let #(a, pre, d2) = top(a, line, d)
          done(a, pre, ast.SwitchStatement(d2, cs))
        }
      }
    ast.ClassDeclaration(name:, super_class: sc, body: b) -> {
      let #(a, pre, sc2, b2) = lin_class(a, line, sc, b)
      done(a, pre, ast.ClassDeclaration(name, sc2, b2))
    }
    ast.LabeledStatement(label:, body: b) ->
      case explode_stmt(a, ast.StmtWithLine(line:, statement: b)) {
        None -> None
        Some(#(a, stmts)) ->
          case list.reverse(stmts) {
            [last, ..rest_rev] ->
              Some(#(
                a,
                list.reverse([
                  ast.StmtWithLine(
                    line:,
                    statement: ast.LabeledStatement(label, last.statement),
                  ),
                  ..rest_rev
                ]),
              ))
            [] -> None
          }
      }
    _ -> None
  }
}

fn loop_with_test(
  line: Int,
  cond: ast.Expression,
  pre: List(ast.StmtWithLine),
  body: ast.Statement,
) -> ast.Statement {
  ast.WhileStatement(
    ast.BooleanLiteral(ast.expression_span(cond), True),
    head_test_block(line, cond, pre, body),
  )
}

fn head_test_block(
  line: Int,
  cond: ast.Expression,
  pre: List(ast.StmtWithLine),
  body: ast.Statement,
) -> ast.Statement {
  let span = ast.expression_span(cond)
  let check =
    ast.StmtWithLine(
      line:,
      statement: ast.IfStatement(
        ast.UnaryExpression(span, ast.LogicalNot, cond),
        ast.BreakStatement(None),
        None,
      ),
    )
  ast.BlockStatement(
    list.flatten([pre, [check], [ast.StmtWithLine(line:, statement: body)]]),
  )
}
