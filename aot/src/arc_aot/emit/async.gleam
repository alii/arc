//// regenerator-style state machine transform for async and generator bodies.
//// coroutine = the js function kind, machine = its lowered state machine

import arc/compiler/ast_util
import arc/compiler/scope.{type ScopeId, type ScopeTree}
import arc/parser/ast
import arc/rt/async as rt_async
import arc_aot/emit/anf
import arc_aot/emit/class
import arc_aot/emit/expr
import arc_aot/emit/func
import arc_aot/emit/state.{
  type EmitResult, type Emitter, type Next, type NextWith,
}
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
type ArmCursor {
  ArmCursor(
    cur_scope: ScopeId,
    scope_cursor: List(ScopeId),
    child_fn_cursor: List(ScopeId),
  )
}

fn cursor_enter_scope(
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

fn cursor_pop_child_fn(c: ArmCursor) -> ArmCursor {
  case c.child_fn_cursor {
    [_, ..rest] -> ArmCursor(..c, child_fn_cursor: rest)
    [] -> c
  }
}

fn cursor_leave_scope(resume: ArmCursor, inner_after: ArmCursor) -> ArmCursor {
  ArmCursor(..resume, child_fn_cursor: inner_after.child_fn_cursor)
}

fn install_cursor(e: Emitter, c: ArmCursor) -> Emitter {
  state.Emitter(
    ..e,
    cur_scope: c.cur_scope,
    scope_cursor: c.scope_cursor,
    child_fn_cursor: c.child_fn_cursor,
  )
}

fn capture_cursor(e: Emitter) -> ArmCursor {
  ArmCursor(
    cur_scope: e.cur_scope,
    scope_cursor: e.scope_cursor,
    child_fn_cursor: e.child_fn_cursor,
  )
}

type SplitKind {
  AwaitSplit
  YieldSplit
  YieldStarSplit
  ForAwaitSplit
}

type TryEntry {
  TryEntry(
    id: Int,
    // a catch-body view shares its parent try's pending slot
    pending_slot_owner: Int,
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
    machine_frames: List(MachineFrame),
  )
}

type DelegateSpec {
  DelegateSpec(
    state_id: Int,
    next_state: Int,
    region: Option(Int),
    await_state: Option(Int),
  )
}

type ForAwaitSpec {
  ForAwaitSpec(
    head: Int,
    check: Int,
    body_state: Int,
    after: Int,
    left: ast.ForInit,
    body_cursor: ArmCursor,
    region: Option(Int),
  )
}

type LocLayout {
  LocLayout(
    slot_to_idx: Dict(Int, Int),
    size: Int,
    extras: Dict(String, Int),
    initial_values: List(ir.Value),
  )
}

type ArmEntry {
  InitialEntry
  ResumeEntry(kind: SplitKind)
  JumpEntry
}

type ResumeWith {
  ResumeDiscard
  ResumeBind(pat: ast.Pattern, mode: state.BindMode)
  ResumeReturn
  ResumeThrow
  ResumeWithScope(body: ast.Statement, line: Int)
  ResumeCatch(try_id: Int, param: Option(ast.Pattern))
}

type SegTail {
  FallTo(to: Int)
  FallToFinally(try_id: Int, to: Int)
  FinallyEnd(try_id: Int)
  SplitAt(kind: SplitKind, arg: Option(ast.Expression), resume_state: Int)
  CondBranch(cond: HeadValue, then_state: Int, else_state: Int)
  ForUpdate(update: Option(ast.Expression), head: Int)
  ForOfSetup(right: ast.Expression, iter_key: String, head: Int)
  ForOfStep(left: ast.ForInit, iter_key: String, body_state: Int, after: Int)
  SwitchDispatch(
    discriminant: HeadValue,
    tests: List(#(Option(ast.Expression), Int)),
    after: Int,
  )
  ForAwaitSetup(right: ast.Expression, head: Int)
  AsyncGenYieldSent(resume_state: Int)
  BodyEnd
  SegDone
}

type HeadValue {
  FromExpr(ast.Expression)
  // the value the coroutine was just resumed with
  FromResumedValue
}

type ArmSpec {
  ArmSpec(
    state_id: Int,
    region: Option(Int),
    entry_kind: ArmEntry,
    entry_cursor: ArmCursor,
    resume: Option(ResumeWith),
    body_fragment: List(ast.StmtWithLine),
    tail: SegTail,
    machine_frames: List(MachineFrame),
  )
}

type SplitPlan {
  SplitPlan(
    n_temps: Int,
    arms: List(ArmSpec),
    try_entries: List(TryEntry),
    delegates: List(DelegateSpec),
    for_awaits: List(ForAwaitSpec),
  )
}

type MachineFrame {
  MachineLoop(
    js_label: Option(String),
    brk_sentinel: String,
    cont_sentinel: String,
    break_state: Int,
    continue_state: Int,
    enclosing_try: Option(Int),
  )
  MachineSwitch(
    js_label: Option(String),
    brk_sentinel: String,
    break_state: Int,
    enclosing_try: Option(Int),
  )
  MachineLabeled(
    js_label: String,
    brk_sentinel: String,
    break_state: Int,
    enclosing_try: Option(Int),
  )
}

type MachineCtx {
  MachineCtx(
    kind: state.CoroutineKind,
    layout: LocLayout,
    resume_loop_label: String,
    resume_mode: ir.Value,
    sent_value: ir.Value,
    saved_locals: ir.Value,
    try_entries: List(TryEntry),
    arms: List(ir.SwitchArm),
    try_stack: List(TryEntry),
    machine_frames: List(MachineFrame),
  )
}

fn new_machine_ctx(
  kind: state.CoroutineKind,
  layout: LocLayout,
  resume_loop_label: String,
  plan: SplitPlan,
) -> MachineCtx {
  MachineCtx(
    kind: kind,
    layout: layout,
    resume_loop_label: resume_loop_label,
    resume_mode: ir.Var(mode_var),
    sent_value: ir.Var(sent_var),
    saved_locals: ir.Var(locals_var),
    try_entries: plan.try_entries,
    arms: [],
    try_stack: [],
    machine_frames: [],
  )
}

fn push_arm(ctx: MachineCtx, n: Int, body: ir.Expr) -> MachineCtx {
  MachineCtx(..ctx, arms: [ir.SwitchArm(n, body), ..ctx.arms])
}

fn finish_arms(ctx: MachineCtx) -> List(ir.SwitchArm) {
  list.reverse(ctx.arms)
}

fn current_try(ctx: MachineCtx) -> Option(TryEntry) {
  case ctx.try_stack {
    [top, ..] -> Some(top)
    [] -> None
  }
}

fn with_region(ctx: MachineCtx, region: Option(Int)) -> MachineCtx {
  MachineCtx(..ctx, try_stack: try_chain(ctx.try_entries, region))
}

// a throw in a catch body must not re-enter its own catch
fn with_catch_body(ctx: MachineCtx, entry: TryEntry) -> MachineCtx {
  let outer = try_chain(ctx.try_entries, entry.outer)
  let stack = case entry.finally_state {
    Some(_) -> [TryEntry(..entry, catch_state: None), ..outer]
    None -> outer
  }
  MachineCtx(..ctx, try_stack: stack)
}

fn with_finally_body(ctx: MachineCtx, entry: TryEntry) -> MachineCtx {
  MachineCtx(..ctx, try_stack: try_chain(ctx.try_entries, entry.outer))
}

fn try_chain(entries: List(TryEntry), region: Option(Int)) -> List(TryEntry) {
  case find_try(entries, region) {
    Some(entry) -> [entry, ..try_chain(entries, entry.outer)]
    None -> []
  }
}

type PendingKind {
  PendingReturn(ir.Value)
  PendingThrow(ir.Value)
  PendingGoto(target: Int)
}

// ints not atoms so the i32 test can compare them
const pend_throw = 1

const pend_return = 2

const pend_goto = 3

// fixed ir names bound by emit_machine_function and read through MachineCtx
const mode_var = "_mode"

const sent_var = "_sv"

const locals_var = "_loc_i"

const resume_state_var = "_rs_i"

const sent_param = "_sent"

const locals_param = "_loc"

const resume_state_param = "_rs"

const step_var = "_step"

const pending_var = "_pend"

const packed_locals_var = "_locp"

fn pending_tuple(pending: PendingKind) -> ir.Expr {
  case pending {
    PendingReturn(v) -> ir.TermOp(ir.MakeTuple, [ir.ConstI32(pend_return), v])
    PendingThrow(v) -> ir.TermOp(ir.MakeTuple, [ir.ConstI32(pend_throw), v])
    PendingGoto(target) ->
      ir.TermOp(ir.MakeTuple, [ir.ConstI32(pend_goto), ir.ConstI32(target)])
  }
}

fn machine_continue(ctx: MachineCtx, target: Int, loc: ir.Value) -> ir.Expr {
  ir.Continue(ctx.resume_loop_label, [ir.ConstI32(target), loc])
}

// packs the live slot vars, so reassignments since the resume are kept
fn repack_live_locals(
  e: Emitter,
  ctx: MachineCtx,
  overrides: Dict(Int, ir.Value),
  k: fn(Emitter, ir.Value) -> #(ir.Expr, Emitter),
) -> #(ir.Expr, Emitter) {
  repack_live_locals_loop(e, ctx, overrides, 0, [], k)
}

fn repack_live_locals_loop(
  e: Emitter,
  ctx: MachineCtx,
  overrides: Dict(Int, ir.Value),
  i: Int,
  acc: List(ir.Value),
  k: fn(Emitter, ir.Value) -> #(ir.Expr, Emitter),
) -> #(ir.Expr, Emitter) {
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
        Ok(v) ->
          repack_live_locals_loop(e, ctx, overrides, i + 1, [v, ..acc], k)
        Error(Nil) ->
          case slot_at_loc_idx(ctx.layout, i) {
            Some(slot) -> {
              let v = ir.Var(state.get_slot_var(e, slot))
              repack_live_locals_loop(e, ctx, overrides, i + 1, [v, ..acc], k)
            }
            None -> {
              let #(name, e) = state.fresh_var(e)
              anf.wrap(
                repack_live_locals_loop(
                  e,
                  ctx,
                  overrides,
                  i + 1,
                  [ir.Var(name), ..acc],
                  k,
                ),
                ir.Let([name], ir.TermOp(ir.TupleGet(i), [ctx.saved_locals]), _),
              )
            }
          }
      }
  }
}

fn jump_state_leaf(
  e: Emitter,
  ctx: MachineCtx,
  target: Int,
  overrides: Dict(Int, ir.Value),
) -> #(ir.Expr, Emitter) {
  use e, loc <- repack_live_locals(e, ctx, overrides)
  #(machine_continue(ctx, target, loc), e)
}

// parks pend in the finally's slot and jumps to it
fn jump_to_finally(
  e: Emitter,
  ctx: MachineCtx,
  entry: TryEntry,
  finally_state: Int,
  pend: ir.Expr,
) -> #(ir.Expr, Emitter) {
  let #(pn, e) = state.fresh_var(e)
  let over = dict.from_list([#(entry.pending_loc_idx, ir.Var(pn))])
  anf.wrap(jump_state_leaf(e, ctx, finally_state, over), ir.Let([pn], pend, _))
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

// compile-time abrupt completion: finally first, packs live locals
fn route_abrupt(
  e: Emitter,
  ctx: MachineCtx,
  pending: PendingKind,
  stop_at: Option(Int),
) -> #(ir.Expr, Emitter) {
  route_abrupt_walk(e, ctx, ctx.try_stack, pending, stop_at)
}

fn route_abrupt_walk(
  e: Emitter,
  ctx: MachineCtx,
  stack: List(TryEntry),
  pending: PendingKind,
  stop_at: Option(Int),
) -> #(ir.Expr, Emitter) {
  case stack {
    [] -> route_abrupt_tail(e, ctx, pending)
    [entry, ..rest] -> {
      use <- bool.lazy_guard(stop_at == Some(entry.id), fn() {
        route_abrupt_tail(e, ctx, pending)
      })
      case entry.finally_state, entry.catch_state, pending {
        Some(finally_state), _, _ ->
          jump_to_finally(e, ctx, entry, finally_state, pending_tuple(pending))
        None, Some(catch_state), PendingThrow(v) ->
          jump_state_leaf(
            e,
            ctx,
            catch_state,
            dict.from_list([#(entry.caught_loc_idx, v)]),
          )
        None, _, _ -> route_abrupt_walk(e, ctx, rest, pending, stop_at)
      }
    }
  }
}

fn route_abrupt_tail(
  e: Emitter,
  ctx: MachineCtx,
  pending: PendingKind,
) -> #(ir.Expr, Emitter) {
  case pending {
    PendingReturn(v) -> #(step_return(v), e)
    PendingThrow(v) -> #(step_throw(v), e)
    PendingGoto(target) -> jump_state_leaf(e, ctx, target, dict.new())
  }
}

fn sentinel_match(
  labels: List(MachineFrame),
  ir_label: String,
) -> Option(MachineFrame) {
  list.find(labels, fn(l) {
    case l {
      MachineLoop(brk_sentinel: b, cont_sentinel: c, ..) ->
        ir_label == b || ir_label == c
      MachineSwitch(brk_sentinel: b, ..)
      | MachineLabeled(brk_sentinel: b, ..) -> ir_label == b
    }
  })
  |> option.from_result
}

fn sentinel_target(l: MachineFrame, ir_label: String) -> #(Int, Option(Int)) {
  case l {
    MachineLoop(
      brk_sentinel: b,
      break_state: bs,
      continue_state:,
      enclosing_try: et,
      ..,
    ) ->
      case ir_label == b {
        True -> #(bs, et)
        False -> #(continue_state, et)
      }
    MachineSwitch(break_state: bs, enclosing_try: et, ..) -> #(bs, et)
    MachineLabeled(break_state: bs, enclosing_try: et, ..) -> #(bs, et)
  }
}

fn make_on_return(ctx: MachineCtx) -> NextWith(ir.Value) {
  fn(e, v) { Ok(route_abrupt(e, ctx, PendingReturn(v), None)) }
}

fn make_on_goto(ctx: MachineCtx) -> fn(Emitter, String) -> Option(EmitResult) {
  fn(e, ir_label) {
    case sentinel_match(ctx.machine_frames, ir_label) {
      None -> None
      Some(label) -> {
        let #(target, stop) = sentinel_target(label, ir_label)
        Some(Ok(route_abrupt(e, ctx, PendingGoto(target), stop)))
      }
    }
  }
}

fn with_abrupt_intercept(
  e: Emitter,
  ctx: MachineCtx,
  body: fn(Emitter, fn(Emitter) -> Emitter) -> a,
) -> a {
  let #(e, n_pushed) = push_machine_frames(e, ctx.machine_frames)
  let e =
    state.set_machine_abrupt(
      e,
      state.MachineAbrupt(
        on_return: make_on_return(ctx),
        on_goto: make_on_goto(ctx),
      ),
    )
  let restore = fn(e: Emitter) {
    pop_n_frames(state.clear_machine_abrupt(e), n_pushed)
  }
  body(e, restore)
}

fn push_machine_frames(
  e: Emitter,
  labels: List(MachineFrame),
) -> #(Emitter, Int) {
  list.fold(list.reverse(labels), #(e, 0), fn(acc, lab) {
    let #(e, n) = acc
    let frame = case lab {
      MachineLoop(js_label:, brk_sentinel:, cont_sentinel:, ..) ->
        state.LoopFrame(
          ir_break: brk_sentinel,
          ir_continue: cont_sentinel,
          js_label:,
          carried: [],
          iter_close: None,
        )
      MachineSwitch(js_label:, brk_sentinel:, ..) ->
        state.SwitchFrame(ir_break: brk_sentinel, js_label:, carried: [])
      MachineLabeled(js_label:, brk_sentinel:, ..) ->
        state.LabeledBlockFrame(ir_break: brk_sentinel, js_label:, carried: [])
    }
    #(state.push_frame(e, frame), n + 1)
  })
}

fn pop_n_frames(e: Emitter, n: Int) -> Emitter {
  case n {
    0 -> e
    _ -> pop_n_frames(state.pop_frame(e), n - 1)
  }
}

fn expr_has_split(e: ast.Expression) -> Bool {
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
    ast.ArrayExpression(elements: xs, ..) -> list.any(xs, opt_expr_has_split)
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

fn stmt_has_split(s: ast.Statement) -> Bool {
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
    ast.SwitchStatement(discriminant: discriminant, cases: cases) ->
      expr_has_split(discriminant)
      || list.any(cases, fn(c: ast.SwitchCase) {
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

type SplitPlanner {
  SplitPlanner(
    scope_tree: ScopeTree,
    kind: state.CoroutineKind,
    next_state: Int,
    next_try: Int,
    next_sentinel: Int,
    next_temp: Int,
    cursor_only: Bool,
    try_stack: List(Int),
    machine_frames: List(MachineFrame),
    cur: ArmCursor,
    tries: List(TryEntry),
    arms: List(ArmSpec),
    delegates: List(DelegateSpec),
    for_awaits: List(ForAwaitSpec),
    pending_stmts_rev: List(ast.StmtWithLine),
    open_state: Int,
    open_region: Option(Int),
    open_entry: ArmEntry,
    open_cursor: ArmCursor,
    open_frames: List(MachineFrame),
    open_resume: Option(ResumeWith),
  )
}

fn current_region(p: SplitPlanner) -> Option(Int) {
  case p.try_stack {
    [top, ..] -> Some(top)
    [] -> None
  }
}

fn alloc_state(p: SplitPlanner) -> #(Int, SplitPlanner) {
  #(p.next_state, SplitPlanner(..p, next_state: p.next_state + 1))
}

fn alloc_sentinel(p: SplitPlanner) -> #(String, SplitPlanner) {
  #(
    "_Lsm" <> int.to_string(p.next_sentinel),
    SplitPlanner(..p, next_sentinel: p.next_sentinel + 1),
  )
}

fn push_pending_stmt(p: SplitPlanner, sl: ast.StmtWithLine) -> SplitPlanner {
  SplitPlanner(..p, pending_stmts_rev: [sl, ..p.pending_stmts_rev])
}

fn finish_arm(
  p: SplitPlanner,
  tail: SegTail,
  new_state: Int,
  entry: ArmEntry,
) -> SplitPlanner {
  let arm =
    ArmSpec(
      state_id: p.open_state,
      region: p.open_region,
      entry_kind: p.open_entry,
      entry_cursor: p.open_cursor,
      resume: p.open_resume,
      body_fragment: list.reverse(p.pending_stmts_rev),
      tail: tail,
      machine_frames: p.open_frames,
    )
  SplitPlanner(
    ..p,
    arms: [arm, ..p.arms],
    pending_stmts_rev: [],
    open_state: new_state,
    open_region: current_region(p),
    open_entry: entry,
    open_cursor: p.cur,
    open_frames: p.machine_frames,
    open_resume: None,
  )
}

fn push_machine_frame(p: SplitPlanner, l: MachineFrame) -> SplitPlanner {
  // the open segment was opened before this push and must see it
  let machine_frames = [l, ..p.machine_frames]
  SplitPlanner(..p, machine_frames:, open_frames: machine_frames)
}

fn pop_machine_frame(p: SplitPlanner) -> SplitPlanner {
  case p.machine_frames {
    [_, ..rest] -> SplitPlanner(..p, machine_frames: rest)
    [] -> p
  }
}

fn plan_split(
  p: SplitPlanner,
  kind: SplitKind,
  arg: Option(ast.Expression),
  resume: Option(ResumeWith),
) -> SplitPlanner {
  case kind, p.kind {
    YieldStarSplit, _ -> plan_delegate(p, arg, resume)
    YieldSplit, state.AsyncGenerator -> plan_async_gen_yield(p, arg, resume)
    _, _ -> plan_plain_split(p, kind, arg, resume)
  }
}

fn plan_plain_split(
  p: SplitPlanner,
  kind: SplitKind,
  arg: Option(ast.Expression),
  resume: Option(ResumeWith),
) -> SplitPlanner {
  let #(resume_state, p) = alloc_state(p)
  let p =
    finish_arm(
      p,
      SplitAt(kind:, arg:, resume_state:),
      resume_state,
      ResumeEntry(kind),
    )
  SplitPlanner(..p, open_resume: resume)
}

// an async generator yield awaits its operand first
fn plan_async_gen_yield(
  p: SplitPlanner,
  arg: Option(ast.Expression),
  resume: Option(ResumeWith),
) -> SplitPlanner {
  let p = plan_plain_split(p, AwaitSplit, arg, None)
  let #(resume_state, p) = alloc_state(p)
  let p =
    finish_arm(
      p,
      AsyncGenYieldSent(resume_state:),
      resume_state,
      ResumeEntry(YieldSplit),
    )
  SplitPlanner(..p, open_resume: resume)
}

fn plan_delegate(
  p: SplitPlanner,
  arg: Option(ast.Expression),
  resume: Option(ResumeWith),
) -> SplitPlanner {
  let #(delegate_state, p) = alloc_state(p)
  let #(follow, p) = alloc_state(p)
  let region = current_region(p)
  let p =
    finish_arm(
      p,
      SplitAt(kind: YieldStarSplit, arg:, resume_state: delegate_state),
      follow,
      ResumeEntry(YieldStarSplit),
    )
  let #(await_state, p) = case p.kind {
    state.AsyncGenerator -> {
      let #(await_state, p) = alloc_state(p)
      #(Some(await_state), p)
    }
    state.AsyncFunction | state.Generator -> #(None, p)
  }
  let delegate_spec =
    DelegateSpec(
      state_id: delegate_state,
      next_state: follow,
      region:,
      await_state:,
    )
  SplitPlanner(
    ..p,
    delegates: [delegate_spec, ..p.delegates],
    open_resume: resume,
  )
}

// keeps the open segment's cursor right when a fragment starts or ends in this scope
fn in_child_scope(
  p: SplitPlanner,
  f: fn(SplitPlanner) -> SplitPlanner,
) -> SplitPlanner {
  let #(inner, resume) = cursor_enter_scope(p.scope_tree, p.cur)
  use <- bool.lazy_guard(p.cursor_only, fn() {
    let p_in = f(SplitPlanner(..p, cur: inner))
    SplitPlanner(..p_in, cur: cursor_leave_scope(resume, p_in.cur))
  })
  let p = case p.pending_stmts_rev {
    [] -> SplitPlanner(..p, open_cursor: inner)
    [_, ..] -> {
      let #(fresh, p) = alloc_state(p)
      let p = finish_arm(p, FallTo(fresh), fresh, JumpEntry)
      SplitPlanner(..p, open_cursor: inner)
    }
  }
  let p_in = f(SplitPlanner(..p, cur: inner))
  let resumed = cursor_leave_scope(resume, p_in.cur)
  case p_in.open_cursor.cur_scope == inner.cur_scope {
    True ->
      case p_in.pending_stmts_rev, p_in.open_resume {
        [], None -> SplitPlanner(..p_in, cur: resumed, open_cursor: resumed)
        _, _ -> {
          let #(fresh, p_in) = alloc_state(SplitPlanner(..p_in, cur: resumed))
          finish_arm(p_in, FallTo(fresh), fresh, JumpEntry)
        }
      }
    False -> SplitPlanner(..p_in, cur: resumed)
  }
}

// false means the analyzer pruned the scope; entering would steal a sibling's id
fn in_child_scope_if(
  p: SplitPlanner,
  cond: Bool,
  f: fn(SplitPlanner) -> SplitPlanner,
) -> SplitPlanner {
  case cond {
    True -> in_child_scope(p, f)
    False -> f(p)
  }
}

fn walk_catch_cur(
  p: SplitPlanner,
  h: ast.CatchClause,
  f: fn(SplitPlanner) -> SplitPlanner,
) -> SplitPlanner {
  let body_has = ast_util.block_has_declarations(h.body)
  case h.param {
    Some(_) -> in_child_scope(p, fn(p) { in_child_scope_if(p, body_has, f) })
    None -> in_child_scope_if(p, body_has, f)
  }
}

fn plan_opt_expr(p: SplitPlanner, o: Option(ast.Expression)) -> SplitPlanner {
  case o {
    Some(e) -> plan_expr(p, e)
    None -> p
  }
}

fn plan_expr(p: SplitPlanner, e: ast.Expression) -> SplitPlanner {
  case e {
    ast.AwaitExpression(argument: arg, ..) ->
      plan_split(plan_expr(p, arg), AwaitSplit, Some(arg), None)
    ast.YieldExpression(argument: arg, is_delegate: True, ..) ->
      plan_split(plan_opt_expr(p, arg), YieldStarSplit, arg, None)
    ast.YieldExpression(argument: arg, is_delegate: False, ..) ->
      plan_split(plan_opt_expr(p, arg), YieldSplit, arg, None)
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
    | ast.IntrinsicTemplateObject(..) -> p
    ast.FunctionExpression(..) | ast.ArrowFunctionExpression(..) ->
      plan_nested_fn(p)
    ast.UnaryExpression(argument: x, ..)
    | ast.UpdateExpression(argument: x, ..)
    | ast.SpreadElement(argument: x, ..)
    | ast.ParenthesizedExpression(expression: x, ..) -> plan_expr(p, x)
    ast.BinaryExpression(left: l, right: r, ..)
    | ast.LogicalExpression(left: l, right: r, ..)
    | ast.AssignmentExpression(left: l, right: r, ..) ->
      plan_expr(plan_expr(p, l), r)
    ast.MemberExpression(object: o, property: prop, ..)
    | ast.OptionalMemberExpression(object: o, property: prop, ..) -> {
      let p = plan_expr(p, o)
      case prop {
        ast.Bracket(expression: pe) -> plan_expr(p, pe)
        ast.Dot(..) -> p
      }
    }
    ast.CallExpression(callee: c, arguments: args, ..)
    | ast.OptionalCallExpression(callee: c, arguments: args, ..)
    | ast.NewExpression(callee: c, arguments: args, ..) ->
      list.fold(args, plan_expr(p, c), plan_expr)
    ast.ConditionalExpression(condition: c, consequent: t, alternate: f, ..) ->
      plan_expr(plan_expr(plan_expr(p, c), t), f)
    ast.SequenceExpression(expressions: xs, ..) -> list.fold(xs, p, plan_expr)
    ast.ArrayExpression(elements: xs, ..) -> list.fold(xs, p, plan_opt_expr)
    ast.ObjectExpression(properties: ps, ..) ->
      list.fold(ps, p, fn(p, prop) {
        case prop {
          ast.InitProperty(key: k, value: v, ..) -> plan_expr(plan_key(p, k), v)
          ast.MethodProperty(key: k, ..) | ast.AccessorProperty(key: k, ..) ->
            plan_nested_fn(plan_key(p, k))
          ast.SpreadProperty(argument: x) -> plan_expr(p, x)
        }
      })
    ast.TemplateLiteral(parts: parts, ..) ->
      list.fold(ast.template_expressions(parts), p, plan_expr)
    ast.TaggedTemplateExpression(tag: t, parts: parts, ..) ->
      list.fold(ast.template_expressions(parts), plan_expr(p, t), plan_expr)
    ast.ImportExpression(source: s, options: o, ..) ->
      plan_opt_expr(plan_expr(p, s), o)
    ast.ClassExpression(super_class: sc, body: elems, ..) ->
      plan_class(p, sc, elems)
  }
}

fn plan_key(p: SplitPlanner, k: ast.PropertyKey) -> SplitPlanner {
  case k {
    ast.KeyComputed(expression: e) -> plan_expr(p, e)
    ast.KeyIdentifier(..)
    | ast.KeyString(..)
    | ast.KeyNumber(..)
    | ast.KeyBigInt(..)
    | ast.KeyPrivate(..) -> p
  }
}

fn plan_class(
  p: SplitPlanner,
  sc: Option(ast.Expression),
  body: List(ast.ClassElement),
) -> SplitPlanner {
  let parts = ast_util.classify_class_body(body)
  let #(inner, resume) = cursor_enter_scope(p.scope_tree, p.cur)
  let p = SplitPlanner(..p, cur: inner)
  let p = case class.has_instance_field_init(parts) {
    True -> plan_nested_fn(p)
    False -> p
  }
  let p = plan_nested_fn(p)
  let p = plan_opt_expr(p, sc)
  let p =
    list.fold(ast_util.computed_element_keys(body), p, fn(p, key) {
      plan_expr(p, key.1)
    })
  let p =
    list.fold(
      list.append(parts.instance_methods, parts.static_methods),
      p,
      fn(p, _) { plan_nested_fn(p) },
    )
  let p = case parts.static_elements {
    [] -> p
    [_, ..] -> plan_nested_fn(p)
  }
  SplitPlanner(..p, cur: cursor_leave_scope(resume, p.cur))
}

fn plan_nested_fn(p: SplitPlanner) -> SplitPlanner {
  SplitPlanner(..p, cur: cursor_pop_child_fn(p.cur))
}

fn plan_pattern(p: SplitPlanner, pat: ast.Pattern) -> SplitPlanner {
  case pat {
    ast.IdentifierPattern(..) -> p
    ast.AssignmentPattern(left: l, right: r) -> plan_expr(plan_pattern(p, l), r)
    ast.RestElement(argument: x) -> plan_pattern(p, x)
    ast.ArrayPattern(elements: xs) ->
      list.fold(xs, p, fn(p, o) {
        case o {
          Some(x) -> plan_pattern(p, x)
          None -> p
        }
      })
    ast.ObjectPattern(properties: ps) ->
      list.fold(ps, p, fn(p, pp) {
        case pp {
          ast.PatternProperty(key: k, value: v, ..) ->
            plan_pattern(plan_key(p, k), v)
          ast.RestProperty(..) -> p
        }
      })
  }
}

fn plan_declarators(
  p: SplitPlanner,
  ds: List(ast.VariableDeclarator),
) -> SplitPlanner {
  use p, d <- list.fold(ds, p)
  plan_opt_expr(plan_pattern(p, d.id), d.init)
}

fn plan_for_init(p: SplitPlanner, fi: ast.ForInit) -> SplitPlanner {
  case fi {
    ast.ForInitExpression(e) -> plan_expr(p, e)
    ast.ForInitDeclaration(declarations: ds, ..) -> plan_declarators(p, ds)
    ast.ForInitPattern(pat) -> plan_pattern(p, pat)
  }
}

fn plan_stmts(p: SplitPlanner, ss: List(ast.StmtWithLine)) -> SplitPlanner {
  list.fold(ss, p, plan_stmt)
}

fn one_stmt(line: Int, s: ast.Statement) -> List(ast.StmtWithLine) {
  [ast.StmtWithLine(line:, statement: s)]
}

fn plan_stmt(p: SplitPlanner, sl: ast.StmtWithLine) -> SplitPlanner {
  let ast.StmtWithLine(statement: s, ..) = sl
  case stmt_has_split(s) {
    False -> push_pending_stmt(plan_stmt_cursor_only(p, s), sl)
    True ->
      case explode_stmt(p, sl) {
        Some(#(p, exploded)) -> plan_stmts(p, exploded)
        None -> plan_split_stmt(p, sl)
      }
  }
}

fn plan_split_stmt(p: SplitPlanner, sl: ast.StmtWithLine) -> SplitPlanner {
  let ast.StmtWithLine(line:, statement: s) = sl
  case s {
    ast.IfStatement(condition: c, consequent: t, alternate: f) ->
      plan_if(p, line, c, t, f)
    ast.BlockStatement(body: b) -> plan_block(p, b)
    ast.LabeledStatement(label:, body: b) -> plan_labeled(p, line, label, b)
    ast.WhileStatement(condition: c, body: b) -> plan_while(p, line, None, c, b)
    ast.DoWhileStatement(condition: c, body: b) ->
      plan_do_while(p, line, None, c, b)
    ast.ForStatement(init: i, condition: c, update: u, body: b) ->
      plan_for(p, line, None, i, c, u, b)
    ast.ForOfStatement(left: l, right: r, body: b, is_await:) ->
      plan_for_of(p, line, None, l, r, b, is_await:)
    ast.ForInStatement(left: l, right: r, body: b) ->
      plan_for_of(p, line, None, l, r, b, is_await: False)
    ast.SwitchStatement(discriminant: discriminant, cases: cases) ->
      plan_switch(p, None, discriminant, cases)
    ast.ExpressionStatement(..)
    | ast.ThrowStatement(..)
    | ast.ReturnStatement(..)
    | ast.VariableDeclaration(..) -> plan_hoisted(p, hoist_one(sl))
    ast.ClassDeclaration(super_class: sc, body: elems, ..) ->
      push_pending_stmt(plan_class(p, sc, elems), sl)
    ast.WithStatement(object: o, body: b) ->
      case split_of(o) {
        Some(#(kind, operand)) -> {
          let p = plan_opt_expr(p, operand)
          let rw = Some(ResumeWithScope(body: b, line:))
          case kind {
            YieldStarSplit -> plan_delegate(p, operand, rw)
            _ -> plan_plain_split(p, kind, operand, rw)
          }
        }
        None -> plan_stmts(plan_expr(p, o), one_stmt(line, b))
      }
    ast.TryStatement(block: blk, tail: tt) -> plan_try(p, blk, tt)
    ast.EmptyStatement
    | ast.DebuggerStatement
    | ast.BreakStatement(..)
    | ast.ContinueStatement(..)
    | ast.FunctionDeclaration(..) -> push_pending_stmt(p, sl)
  }
}

fn plan_hoisted(p: SplitPlanner, items: List(HoistedItem)) -> SplitPlanner {
  use p, item <- list.fold(items, p)
  case item {
    PlainStmt(s) -> push_pending_stmt(plan_stmt_cursor_only(p, s.statement), s)
    SplitStmt(kind:, operand:, resume:) ->
      plan_split(plan_opt_expr(p, operand), kind, operand, Some(resume))
  }
}

fn plan_stmt_cursor_only(p: SplitPlanner, s: ast.Statement) -> SplitPlanner {
  let was = p.cursor_only
  let p = cursor_only_walk(SplitPlanner(..p, cursor_only: True), s)
  SplitPlanner(..p, cursor_only: was)
}

fn plan_stmts_cursor_only(
  p: SplitPlanner,
  ss: List(ast.StmtWithLine),
) -> SplitPlanner {
  use p, sl <- list.fold(ss, p)
  plan_stmt_cursor_only(p, sl.statement)
}

fn plan_block_cursor_only(
  p: SplitPlanner,
  ss: List(ast.StmtWithLine),
) -> SplitPlanner {
  in_child_scope_if(p, ast_util.block_has_declarations(ss), fn(p) {
    plan_stmts_cursor_only(p, ss)
  })
}

fn cursor_only_walk(p: SplitPlanner, s: ast.Statement) -> SplitPlanner {
  case s {
    ast.FunctionDeclaration(..) -> plan_nested_fn(p)
    ast.ClassDeclaration(super_class: sc, body: elems, ..) ->
      plan_class(p, sc, elems)
    ast.ExpressionStatement(expression: e, ..)
    | ast.ThrowStatement(argument: e) -> plan_expr(p, e)
    ast.ReturnStatement(argument: arg) -> plan_opt_expr(p, arg)
    ast.VariableDeclaration(declarations: ds, ..) -> plan_declarators(p, ds)
    ast.BlockStatement(body: b) -> plan_block_cursor_only(p, b)
    ast.IfStatement(condition: c, consequent: t, alternate: f) -> {
      let p = plan_stmt_cursor_only(plan_expr(p, c), t)
      case f {
        Some(alt) -> plan_stmt_cursor_only(p, alt)
        None -> p
      }
    }
    ast.WhileStatement(condition: c, body: b)
    | ast.DoWhileStatement(condition: c, body: b) ->
      plan_stmt_cursor_only(plan_expr(p, c), b)
    ast.ForStatement(init: i, condition: c, update: u, body: b) ->
      in_child_scope_if(p, ast_util.for_classic_init_is_lex(i), fn(p) {
        let p = case i {
          Some(fi) -> plan_for_init(p, fi)
          None -> p
        }
        let p = plan_opt_expr(p, c)
        let p = plan_opt_expr(p, u)
        plan_stmt_cursor_only(p, b)
      })
    ast.ForInStatement(left: l, right: r, body: b)
    | ast.ForOfStatement(left: l, right: r, body: b, ..) ->
      in_child_scope_if(p, ast_util.for_classic_init_is_lex(Some(l)), fn(p) {
        let p = plan_for_init(p, l)
        let p = plan_expr(p, r)
        plan_stmt_cursor_only(p, b)
      })
    ast.SwitchStatement(discriminant: discriminant, cases: cases) -> {
      use p <- in_child_scope(plan_expr(p, discriminant))
      use p, c <- list.fold(cases, p)
      plan_stmts_cursor_only(plan_opt_expr(p, c.condition), c.consequent)
    }
    ast.LabeledStatement(body: b, ..) -> plan_stmt_cursor_only(p, b)
    ast.WithStatement(object: o, body: b) ->
      plan_stmt_cursor_only(plan_expr(p, o), b)
    ast.TryStatement(block: blk, tail: tt) -> {
      let p = plan_block_cursor_only(p, blk)
      let catch_cur = fn(p, h: ast.CatchClause) {
        walk_catch_cur(p, h, plan_stmts_cursor_only(_, h.body))
      }
      case tt {
        ast.TryCatch(handler: h) -> catch_cur(p, h)
        ast.TryFinally(finalizer: f) -> plan_block_cursor_only(p, f)
        ast.TryCatchFinally(handler: h, finalizer: f) ->
          plan_block_cursor_only(catch_cur(p, h), f)
      }
    }
    ast.EmptyStatement
    | ast.DebuggerStatement
    | ast.BreakStatement(..)
    | ast.ContinueStatement(..) -> p
  }
}

// none means the arm reads the resumed value instead of an expression
fn plan_head_expr(
  p: SplitPlanner,
  expr: ast.Expression,
) -> #(HeadValue, SplitPlanner) {
  case split_of(expr), p.kind {
    None, _
    | Some(#(YieldStarSplit, _)), _
    | Some(#(YieldSplit, _)), state.AsyncGenerator
    -> #(FromExpr(expr), plan_expr(p, expr))
    Some(#(kind, operand)), _ -> #(
      FromResumedValue,
      plan_plain_split(plan_opt_expr(p, operand), kind, operand, None),
    )
  }
}

fn plan_if(
  p: SplitPlanner,
  line: Int,
  cond: ast.Expression,
  cons: ast.Statement,
  alt: Option(ast.Statement),
) -> SplitPlanner {
  let #(cond, p) = plan_head_expr(p, cond)
  let #(then_state, p) = alloc_state(p)
  let #(after, p) = alloc_state(p)
  let #(else_state, p) = case alt {
    Some(_) -> alloc_state(p)
    None -> #(after, p)
  }
  let p =
    finish_arm(
      p,
      CondBranch(cond:, then_state:, else_state:),
      then_state,
      JumpEntry,
    )
  let p = plan_stmts(p, one_stmt(line, cons))
  let p = finish_arm(p, FallTo(after), else_state, JumpEntry)
  case alt {
    None -> p
    Some(alt_stmt) -> {
      let p = plan_stmts(p, one_stmt(line, alt_stmt))
      finish_arm(p, FallTo(after), after, JumpEntry)
    }
  }
}

fn plan_while(
  p: SplitPlanner,
  line: Int,
  label: Option(String),
  cond: ast.Expression,
  body: ast.Statement,
) -> SplitPlanner {
  let #(head, p) = alloc_state(p)
  let #(body_state, p) = alloc_state(p)
  let #(after, p) = alloc_state(p)
  let #(brk, p) = alloc_sentinel(p)
  let #(cont, p) = alloc_sentinel(p)
  let p = finish_arm(p, FallTo(head), head, JumpEntry)
  let #(cond, p) = plan_head_expr(p, cond)
  let p =
    finish_arm(
      p,
      CondBranch(cond:, then_state: body_state, else_state: after),
      body_state,
      JumpEntry,
    )
  let machine_frame =
    MachineLoop(
      js_label: label,
      brk_sentinel: brk,
      cont_sentinel: cont,
      break_state: after,
      continue_state: head,
      enclosing_try: current_region(p),
    )
  let p = push_machine_frame(p, machine_frame)
  let p = plan_stmts(p, one_stmt(line, body))
  let p = pop_machine_frame(p)
  finish_arm(p, FallTo(head), after, JumpEntry)
}

fn plan_do_while(
  p: SplitPlanner,
  line: Int,
  label: Option(String),
  cond: ast.Expression,
  body: ast.Statement,
) -> SplitPlanner {
  let #(body_state, p) = alloc_state(p)
  let #(test_state, p) = alloc_state(p)
  let #(after, p) = alloc_state(p)
  let #(brk, p) = alloc_sentinel(p)
  let #(cont, p) = alloc_sentinel(p)
  let p = finish_arm(p, FallTo(body_state), body_state, JumpEntry)
  let machine_frame =
    MachineLoop(
      js_label: label,
      brk_sentinel: brk,
      cont_sentinel: cont,
      break_state: after,
      continue_state: test_state,
      enclosing_try: current_region(p),
    )
  let p = push_machine_frame(p, machine_frame)
  let p = plan_stmts(p, one_stmt(line, body))
  let p = pop_machine_frame(p)
  let p = finish_arm(p, FallTo(test_state), test_state, JumpEntry)
  let #(cond, p) = plan_head_expr(p, cond)
  finish_arm(
    p,
    CondBranch(cond:, then_state: body_state, else_state: after),
    after,
    JumpEntry,
  )
}

fn plan_for(
  p: SplitPlanner,
  line: Int,
  label: Option(String),
  init: Option(ast.ForInit),
  cond: Option(ast.Expression),
  update: Option(ast.Expression),
  body: ast.Statement,
) -> SplitPlanner {
  in_child_scope_if(p, ast_util.for_classic_init_is_lex(init), fn(p) {
    let p = case init {
      None -> p
      Some(fi) -> {
        let p = plan_for_init(p, fi)
        case fi {
          ast.ForInitExpression(e) ->
            push_pending_stmt(
              p,
              ast.StmtWithLine(
                line:,
                statement: ast.ExpressionStatement(e, None),
              ),
            )
          ast.ForInitDeclaration(kind: vk, declarations:) ->
            push_pending_stmt(
              p,
              ast.StmtWithLine(
                line:,
                statement: ast.VariableDeclaration(kind: vk, declarations:),
              ),
            )
          // todo: lhs default splits would land in the wrong arm
          ast.ForInitPattern(_) -> p
        }
      }
    }
    let #(head, p) = alloc_state(p)
    let #(body_state, p) = alloc_state(p)
    let #(update_state, p) = alloc_state(p)
    let #(after, p) = alloc_state(p)
    let #(brk, p) = alloc_sentinel(p)
    let #(cont, p) = alloc_sentinel(p)
    let p = finish_arm(p, FallTo(head), head, JumpEntry)
    let p = case cond {
      Some(c) -> {
        let #(c, p) = plan_head_expr(p, c)
        finish_arm(
          p,
          CondBranch(cond: c, then_state: body_state, else_state: after),
          body_state,
          JumpEntry,
        )
      }
      None -> finish_arm(p, FallTo(body_state), body_state, JumpEntry)
    }
    let machine_frame =
      MachineLoop(
        js_label: label,
        brk_sentinel: brk,
        cont_sentinel: cont,
        break_state: after,
        continue_state: update_state,
        enclosing_try: current_region(p),
      )
    let p = push_machine_frame(p, machine_frame)
    let p = plan_stmts(p, one_stmt(line, body))
    let p = pop_machine_frame(p)
    let p = finish_arm(p, FallTo(update_state), update_state, JumpEntry)
    let #(update, p) = case update {
      None -> #(None, p)
      Some(u) ->
        case plan_head_expr(p, u) {
          #(FromExpr(u), p) -> #(Some(u), p)
          // a split update's resumed value is discarded
          #(FromResumedValue, p) -> #(None, p)
        }
    }
    finish_arm(p, ForUpdate(update:, head:), after, JumpEntry)
  })
}

fn plan_for_of(
  p: SplitPlanner,
  line: Int,
  label: Option(String),
  left: ast.ForInit,
  right: ast.Expression,
  body: ast.Statement,
  is_await is_await: Bool,
) -> SplitPlanner {
  use p <- in_child_scope_if(p, ast_util.for_classic_init_is_lex(Some(left)))
  use <- bool.lazy_guard(is_await, fn() {
    plan_for_await(p, line, label, left, right, body)
  })
  let #(head, p) = alloc_state(p)
  let #(body_state, p) = alloc_state(p)
  let #(after, p) = alloc_state(p)
  let #(brk, p) = alloc_sentinel(p)
  let #(cont, p) = alloc_sentinel(p)
  let ikey = iter_key(head)
  let p =
    finish_arm(p, ForOfSetup(right:, iter_key: ikey, head:), head, JumpEntry)
  let p =
    finish_arm(
      p,
      ForOfStep(left:, iter_key: ikey, body_state:, after:),
      body_state,
      JumpEntry,
    )
  let machine_frame =
    MachineLoop(
      js_label: label,
      brk_sentinel: brk,
      cont_sentinel: cont,
      break_state: after,
      continue_state: head,
      enclosing_try: current_region(p),
    )
  let p = push_machine_frame(p, machine_frame)
  let p = plan_stmts(p, one_stmt(line, body))
  let p = pop_machine_frame(p)
  finish_arm(p, FallTo(head), after, JumpEntry)
}

fn plan_for_await(
  p: SplitPlanner,
  line: Int,
  label: Option(String),
  left: ast.ForInit,
  right: ast.Expression,
  body: ast.Statement,
) -> SplitPlanner {
  let #(head, p) = alloc_state(p)
  let #(check, p) = alloc_state(p)
  let #(body_state, p) = alloc_state(p)
  let #(after, p) = alloc_state(p)
  let #(brk, p) = alloc_sentinel(p)
  let #(cont, p) = alloc_sentinel(p)
  let region = current_region(p)
  let spec =
    ForAwaitSpec(
      head:,
      check:,
      body_state:,
      after:,
      left:,
      body_cursor: p.cur,
      region:,
    )
  let p = SplitPlanner(..p, for_awaits: [spec, ..p.for_awaits])
  let p = finish_arm(p, ForAwaitSetup(right:, head:), body_state, JumpEntry)
  let machine_frame =
    MachineLoop(
      js_label: label,
      brk_sentinel: brk,
      cont_sentinel: cont,
      break_state: after,
      continue_state: head,
      enclosing_try: region,
    )
  let p = push_machine_frame(p, machine_frame)
  let p = plan_for_init(p, left)
  let p = plan_stmts(p, one_stmt(line, body))
  let p = pop_machine_frame(p)
  finish_arm(p, FallTo(head), after, JumpEntry)
}

fn plan_switch(
  p: SplitPlanner,
  label: Option(String),
  discriminant: ast.Expression,
  cases: List(ast.SwitchCase),
) -> SplitPlanner {
  let #(discriminant, p) = plan_head_expr(p, discriminant)
  let #(after, p) = alloc_state(p)
  let #(brk, p) = alloc_sentinel(p)
  in_child_scope(p, fn(p) {
    let #(case_states_rev, p) =
      list.fold(cases, #([], p), fn(acc, _c) {
        let #(sts, p) = acc
        let #(s, p) = alloc_state(p)
        #([s, ..sts], p)
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
    let p =
      finish_arm(
        p,
        SwitchDispatch(discriminant:, tests:, after:),
        first,
        JumpEntry,
      )
    let machine_frame =
      MachineSwitch(
        js_label: label,
        brk_sentinel: brk,
        break_state: after,
        enclosing_try: current_region(p),
      )
    let p = push_machine_frame(p, machine_frame)
    let p = plan_switch_cases(p, cases, case_states, after)
    pop_machine_frame(p)
  })
}

fn plan_switch_cases(
  p: SplitPlanner,
  cases: List(ast.SwitchCase),
  states: List(Int),
  after: Int,
) -> SplitPlanner {
  case cases, states {
    [], [] -> p
    [ast.SwitchCase(consequent:, ..), ..rest], [_, ..rest_states] -> {
      let p = plan_stmts(p, consequent)
      let next = case rest_states {
        [n, ..] -> n
        [] -> after
      }
      let p = finish_arm(p, FallTo(next), next, JumpEntry)
      plan_switch_cases(p, rest, rest_states, after)
    }
    _, _ -> p
  }
}

fn plan_labeled(
  p: SplitPlanner,
  line: Int,
  label: String,
  body: ast.Statement,
) -> SplitPlanner {
  case body {
    ast.WhileStatement(condition: c, body: b) ->
      plan_while(p, line, Some(label), c, b)
    ast.DoWhileStatement(condition: c, body: b) ->
      plan_do_while(p, line, Some(label), c, b)
    ast.ForStatement(init: i, condition: c, update: u, body: b) ->
      plan_for(p, line, Some(label), i, c, u, b)
    ast.ForOfStatement(left: l, right: r, body: b, is_await:) ->
      plan_for_of(p, line, Some(label), l, r, b, is_await:)
    ast.ForInStatement(left: l, right: r, body: b) ->
      plan_for_of(p, line, Some(label), l, r, b, is_await: False)
    ast.SwitchStatement(discriminant: discriminant, cases: cases) ->
      plan_switch(p, Some(label), discriminant, cases)
    ast.LabeledStatement(label: inner, body: b) -> {
      let #(after, p) = alloc_state(p)
      let #(brk, p) = alloc_sentinel(p)
      let alias =
        MachineLabeled(
          js_label: label,
          brk_sentinel: brk,
          break_state: after,
          enclosing_try: current_region(p),
        )
      let p = push_machine_frame(p, alias)
      let p = plan_labeled(p, line, inner, b)
      let p = pop_machine_frame(p)
      finish_arm(p, FallTo(after), after, JumpEntry)
    }
    _ -> {
      let #(after, p) = alloc_state(p)
      let #(brk, p) = alloc_sentinel(p)
      let machine_frame =
        MachineLabeled(
          js_label: label,
          brk_sentinel: brk,
          break_state: after,
          enclosing_try: current_region(p),
        )
      let p = push_machine_frame(p, machine_frame)
      let p = plan_stmts(p, one_stmt(line, body))
      let p = pop_machine_frame(p)
      finish_arm(p, FallTo(after), after, JumpEntry)
    }
  }
}

fn plan_block(p: SplitPlanner, ss: List(ast.StmtWithLine)) -> SplitPlanner {
  in_child_scope_if(p, ast_util.block_has_declarations(ss), plan_stmts(_, ss))
}

fn plan_catch(p: SplitPlanner, h: ast.CatchClause) -> SplitPlanner {
  walk_catch_cur(p, h, plan_stmts(_, h.body))
}

fn plan_try(
  p: SplitPlanner,
  block: List(ast.StmtWithLine),
  tt: ast.TryTail,
) -> SplitPlanner {
  let #(handler, finalizer) = case tt {
    ast.TryCatch(handler: h) -> #(Some(h), None)
    ast.TryFinally(finalizer: f) -> #(None, Some(f))
    ast.TryCatchFinally(handler: h, finalizer: f) -> #(Some(h), Some(f))
  }
  let catch_split = option.map(handler, catch_has_split) |> option.unwrap(False)
  let finally_split =
    option.map(finalizer, stmts_have_split) |> option.unwrap(False)
  let plan_handler = fn(p) {
    option.map(handler, plan_catch(p, _)) |> option.unwrap(p)
  }
  let plan_finalizer = fn(p) {
    option.map(finalizer, plan_block(p, _)) |> option.unwrap(p)
  }
  case stmts_have_split(block) || catch_split || finally_split {
    False -> plan_block(p, block) |> plan_handler |> plan_finalizer
    True -> {
      let try_id = p.next_try
      let outer = current_region(p)
      let entry_machine_frames = p.machine_frames
      let p = SplitPlanner(..p, next_try: try_id + 1)
      let #(block_entry, p) = alloc_state(p)
      let p = finish_arm(p, FallTo(block_entry), block_entry, JumpEntry)
      let p =
        SplitPlanner(
          ..p,
          try_stack: [try_id, ..p.try_stack],
          open_region: Some(try_id),
        )
      let p = plan_block(p, block)
      let p =
        SplitPlanner(..p, try_stack: case p.try_stack {
          [_, ..rest] -> rest
          [] -> []
        })
      let #(catch_state, catch_cursor, p) = case handler {
        Some(_) -> {
          let #(catch_state, p) = alloc_state(p)
          #(Some(catch_state), Some(p.cur), p)
        }
        None -> #(None, None, p)
      }
      let #(finally_state, p) = case finalizer {
        Some(_) -> {
          let #(finally_state, p) = alloc_state(p)
          #(Some(finally_state), p)
        }
        None -> #(None, p)
      }
      let #(after_state, p) = alloc_state(p)
      let normal_tail = case finally_state {
        Some(finally_state) -> FallToFinally(try_id, finally_state)
        None -> FallTo(after_state)
      }
      let #(p, catch_close_tail) = case handler, catch_state, catch_split {
        Some(h), Some(catch_state), True -> {
          let p = finish_arm(p, normal_tail, catch_state, JumpEntry)
          let #(p, view) = case finally_state {
            Some(_) -> {
              let view_id = p.next_try
              let view =
                TryEntry(
                  id: view_id,
                  pending_slot_owner: try_id,
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
                  machine_frames: entry_machine_frames,
                )
              #(
                SplitPlanner(
                  ..p,
                  next_try: view_id + 1,
                  try_stack: [view_id, ..p.try_stack],
                  open_region: Some(view_id),
                ),
                Some(view),
              )
            }
            None -> #(p, None)
          }
          let p =
            SplitPlanner(..p, open_resume: Some(ResumeCatch(try_id, h.param)))
          let p = plan_catch(p, h)
          let p = case view {
            Some(v) ->
              SplitPlanner(
                ..p,
                tries: [v, ..p.tries],
                try_stack: case p.try_stack {
                  [_, ..rest] -> rest
                  [] -> []
                },
              )
            None -> p
          }
          #(p, normal_tail)
        }
        _, _, _ -> {
          let #(sink, p) = alloc_state(p)
          let p = finish_arm(p, normal_tail, sink, JumpEntry)
          #(plan_handler(p), SegDone)
        }
      }
      let #(finally_cursor, p) = case finalizer, finally_state, finally_split {
        Some(f), Some(finally_state), True -> {
          let p = finish_arm(p, catch_close_tail, finally_state, JumpEntry)
          let p = plan_block(p, f)
          #(None, finish_arm(p, FinallyEnd(try_id), after_state, JumpEntry))
        }
        _, _, _ -> {
          let #(sink, p) = alloc_state(p)
          let p = finish_arm(p, catch_close_tail, sink, JumpEntry)
          let finally_cursor = option.map(finalizer, fn(_) { p.cur })
          let p = plan_finalizer(p)
          #(finally_cursor, finish_arm(p, SegDone, after_state, JumpEntry))
        }
      }
      let entry =
        TryEntry(
          id: try_id,
          pending_slot_owner: try_id,
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
          machine_frames: entry_machine_frames,
        )
      SplitPlanner(..p, tries: [entry, ..p.tries])
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
    SplitPlanner(
      scope_tree: tree,
      kind:,
      next_state: 1,
      next_try: 0,
      next_sentinel: 0,
      next_temp: 0,
      cursor_only: False,
      try_stack: [],
      machine_frames: [],
      cur: cur0,
      tries: [],
      arms: [],
      delegates: [],
      for_awaits: [],
      pending_stmts_rev: [],
      open_state: 0,
      open_region: None,
      open_entry: InitialEntry,
      open_cursor: cur0,
      open_frames: [],
      open_resume: None,
    )
  let p = case body {
    state.StmtBody(ss) -> plan_stmts(init, ss)
    state.ExprBody(e) -> plan_stmts(init, func.body_stmts(state.ExprBody(e)))
  }
  let p = finish_arm(p, BodyEnd, p.next_state, JumpEntry)
  SplitPlan(
    n_temps: p.next_temp,
    arms: list.reverse(p.arms),
    try_entries: list.reverse(p.tries),
    delegates: list.reverse(p.delegates),
    for_awaits: list.reverse(p.for_awaits),
  )
}

fn pending_key(try_id: Int) -> String {
  "pending_" <> int.to_string(try_id)
}

fn caught_key(try_id: Int) -> String {
  "caught_" <> int.to_string(try_id)
}

fn iter_key(state_id: Int) -> String {
  "iter_" <> int.to_string(state_id)
}

fn inner_key(state_id: Int) -> String {
  "inner_" <> int.to_string(state_id)
}

// not a js value, marks the result slot until the first inner call
const delegate_start = "yield_star_start"

fn delegate_result_key(state_id: Int) -> String {
  "delegate_result_" <> int.to_string(state_id)
}

fn for_await_iter_key(head: Int) -> String {
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
      let key = pending_key(entry.pending_slot_owner)
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
  use #(extras, next), spec <- list.fold(for_awaits, #(extras, next))
  #(dict.insert(extras, for_await_iter_key(spec.head), next), next + 1)
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
      case dict.get(extras, pending_key(entry.pending_slot_owner)) {
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

fn enrich_try_entries(
  entries: List(TryEntry),
  layout: LocLayout,
) -> List(TryEntry) {
  use entry <- list.map(entries)
  let pending_loc_idx =
    dict.get(layout.extras, pending_key(entry.pending_slot_owner))
    |> result.unwrap(entry.pending_loc_idx)
  let caught_loc_idx =
    dict.get(layout.extras, caught_key(entry.id))
    |> result.unwrap(entry.caught_loc_idx)
  TryEntry(..entry, pending_loc_idx:, caught_loc_idx:)
}

fn step_return(v: ir.Value) -> ir.Expr {
  ir.Let(
    [step_var],
    ir.TermOp(ir.MakeTuple, [ir.ConstAtom("return"), v]),
    ir.Return([ir.Var(step_var)]),
  )
}

fn step_throw(v: ir.Value) -> ir.Expr {
  ir.Let(
    [step_var],
    ir.TermOp(ir.MakeTuple, [ir.ConstAtom("throw"), v]),
    ir.Return([ir.Var(step_var)]),
  )
}

fn step_yield(v: ir.Value, resume_state: Int, loc: ir.Value) -> ir.Expr {
  ir.Let(
    [step_var],
    ir.TermOp(ir.MakeTuple, [
      ir.ConstAtom("yield"),
      v,
      ir.ConstI32(resume_state),
      loc,
    ]),
    ir.Return([ir.Var(step_var)]),
  )
}

fn step_await(v: ir.Value, resume_state: Int, loc: ir.Value) -> ir.Expr {
  ir.Let(
    [step_var],
    ir.TermOp(ir.MakeTuple, [
      ir.ConstAtom("await"),
      v,
      ir.ConstI32(resume_state),
      loc,
    ]),
    ir.Return([ir.Var(step_var)]),
  )
}

// packs from the saved tuple, ignoring anything reassigned since the resume
fn repack_saved_locals(
  ctx: MachineCtx,
  overrides: Dict(Int, ir.Value),
) -> anf.Build(ir.Value) {
  repack_saved_locals_loop(ctx, overrides, 0, [])
}

fn repack_saved_locals_loop(
  ctx: MachineCtx,
  overrides: Dict(Int, ir.Value),
  i: Int,
  acc: List(ir.Value),
) -> anf.Build(ir.Value) {
  case i < ctx.layout.size {
    False -> anf.make_tuple(list.reverse(acc))
    True ->
      case dict.get(overrides, i) {
        Ok(v) -> repack_saved_locals_loop(ctx, overrides, i + 1, [v, ..acc])
        Error(Nil) ->
          anf.then(anf.bind(anf.tuple_get(ctx.saved_locals, i)), fn(v) {
            repack_saved_locals_loop(ctx, overrides, i + 1, [v, ..acc])
          })
      }
  }
}

fn machine_default_arm(e: Emitter) -> #(ir.Expr, Emitter) {
  let msg = ir.ConstBinary(bit_array.from_string("invalid gen state"))
  anf.run_to(anf.host("new_error", [msg]), e, fn(_e, err) { step_throw(err) })
}

fn build_machine_params(e: Emitter, i: Int, ncap: Int) -> List(ir.Local) {
  case i < ncap {
    True -> [
      ir.Local(state.cap_param_name(e, i), ir.TTerm),
      ..build_machine_params(e, i + 1, ncap)
    ]
    False -> [
      ir.Local(resume_state_param, ir.TTerm),
      ir.Local(sent_param, ir.TTerm),
      ir.Local(locals_param, ir.TTerm),
    ]
  }
}

fn emit_machine_function(
  e: Emitter,
  machine_name: String,
  ncap: Int,
  resume_loop_label: String,
  arms: List(ir.SwitchArm),
  default: ir.Expr,
) -> Emitter {
  let sent = ir.Var(sent_param)
  let loop_body =
    ir.Let(
      ["_rsi32"],
      ir.Convert(ir.UnboxInt(ir.W32), ir.Var(resume_state_var)),
      ir.Switch(ir.Var("_rsi32"), [ir.TTerm], arms, default),
    )
  let body =
    ir.Let(
      [mode_var],
      ir.TermOp(ir.TupleGet(0), [sent]),
      ir.Let(
        [sent_var],
        ir.TermOp(ir.TupleGet(1), [sent]),
        ir.Loop(
          resume_loop_label,
          [
            ir.LoopParam(resume_state_var, ir.TTerm, ir.Var(resume_state_param)),
            ir.LoopParam(locals_var, ir.TTerm, ir.Var(locals_param)),
          ],
          [ir.TTerm],
          loop_body,
        ),
      ),
    )
  state.add_function(
    e,
    ir.Function(
      name: machine_name,
      params: build_machine_params(e, 0, ncap),
      result: [ir.TTerm],
      locals: [],
      body: body,
    ),
  )
}

fn build_outer_params(e: Emitter, i: Int, n: Int) -> List(ir.Local) {
  case i < n {
    False -> [
      ir.Local(func.frame_param, ir.TTerm),
      ir.Local(func.args_param, ir.TTerm),
    ]
    True -> [
      ir.Local(state.cap_param_name(e, i), ir.TTerm),
      ..build_outer_params(e, i + 1, n)
    ]
  }
}

fn cap_vars(e: Emitter, i: Int, n: Int) -> List(ir.Value) {
  case i < n {
    False -> []
    True -> [ir.Var(state.cap_param_name(e, i)), ..cap_vars(e, i + 1, n)]
  }
}

fn atom_bool(rc: state.IrConsts, b: Bool) -> ir.Value {
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
    state.AsyncFunction -> "async_start"
    state.Generator -> "gen_start"
    state.AsyncGenerator -> "asyncgen_start"
  }
}

fn kind_is_async(kind: state.CoroutineKind) -> Bool {
  case kind {
    state.AsyncFunction | state.AsyncGenerator -> True
    state.Generator -> False
  }
}

fn kind_is_gen(kind: state.CoroutineKind) -> Bool {
  case kind {
    state.Generator | state.AsyncGenerator -> True
    state.AsyncFunction -> False
  }
}

fn initial_loc_values(
  e: Emitter,
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

fn emit_closure_alloc(
  e: Emitter,
  outer_name: String,
  kind: state.CoroutineKind,
  shape: state.FnShape,
  is_strict: Bool,
  js_name: Option(String),
  params: List(ast.Pattern),
  captures: List(ir.Value),
) -> #(ir.Expr, Emitter) {
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
      anf.host("new_function", [
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

// raw-expr form of repack_saved_locals with fixed var names
fn repack_saved_locals_expr(
  ctx: MachineCtx,
  overrides: Dict(Int, ir.Value),
  k: fn(ir.Value) -> ir.Expr,
) -> ir.Expr {
  repack_saved_locals_expr_loop(ctx, overrides, 0, [], k)
}

fn repack_saved_locals_expr_loop(
  ctx: MachineCtx,
  overrides: Dict(Int, ir.Value),
  i: Int,
  acc: List(ir.Value),
  k: fn(ir.Value) -> ir.Expr,
) -> ir.Expr {
  case i < ctx.layout.size {
    False ->
      ir.Let(
        [packed_locals_var],
        ir.TermOp(ir.MakeTuple, list.reverse(acc)),
        k(ir.Var(packed_locals_var)),
      )
    True ->
      case dict.get(overrides, i) {
        Ok(v) ->
          repack_saved_locals_expr_loop(ctx, overrides, i + 1, [v, ..acc], k)
        Error(Nil) -> {
          // fixed names are safe, callers sit in a fresh let scope
          let name = "_pk" <> int.to_string(i)
          ir.Let(
            [name],
            ir.TermOp(ir.TupleGet(i), [ctx.saved_locals]),
            repack_saved_locals_expr_loop(
              ctx,
              overrides,
              i + 1,
              [ir.Var(name), ..acc],
              k,
            ),
          )
        }
      }
  }
}

// runtime throw caught by an arm: catch first, packs saved locals
fn route_throw(
  ctx: MachineCtx,
  region: Option(TryEntry),
  ev: ir.Value,
) -> ir.Expr {
  case region {
    Some(TryEntry(catch_state: Some(catch_state), caught_loc_idx: ci, ..)) ->
      repack_saved_locals_expr(ctx, dict.from_list([#(ci, ev)]), fn(locp) {
        ir.Continue(ctx.resume_loop_label, [ir.ConstI32(catch_state), locp])
      })
    Some(TryEntry(finally_state: Some(finally_state), pending_loc_idx: pi, ..)) ->
      ir.Let(
        [pending_var],
        ir.TermOp(ir.MakeTuple, [ir.ConstI32(pend_throw), ev]),
        repack_saved_locals_expr(
          ctx,
          dict.from_list([#(pi, ir.Var(pending_var))]),
          fn(locp) {
            ir.Continue(ctx.resume_loop_label, [
              ir.ConstI32(finally_state),
              locp,
            ])
          },
        ),
      )
    _ -> step_throw(ev)
  }
}

fn route_return(
  ctx: MachineCtx,
  region: Option(TryEntry),
  v: ir.Value,
) -> ir.Expr {
  case region {
    Some(TryEntry(finally_state: Some(finally_state), pending_loc_idx: pi, ..)) ->
      ir.Let(
        [pending_var],
        ir.TermOp(ir.MakeTuple, [ir.ConstI32(pend_return), v]),
        repack_saved_locals_expr(
          ctx,
          dict.from_list([#(pi, ir.Var(pending_var))]),
          fn(locp) {
            ir.Continue(ctx.resume_loop_label, [
              ir.ConstI32(finally_state),
              locp,
            ])
          },
        ),
      )
    Some(entry) -> route_return(ctx, find_try(ctx.try_entries, entry.outer), v)
    None -> step_return(v)
  }
}

fn wrap_arm_try(
  e: Emitter,
  ctx: MachineCtx,
  region: Option(TryEntry),
  inner: ir.Expr,
) -> ir.Expr {
  ir.Try(result: [ir.TTerm], body: inner, handlers: [
    ir.CatchHandler(
      on: ir.OnTag(e.consts.exn_tag),
      payload: ["_e"],
      exnref: None,
      handler: route_throw(ctx, region, ir.Var("_e")),
    ),
  ])
}

fn emit_mode_dispatch(
  ctx: MachineCtx,
  entry: ArmEntry,
  region: Option(TryEntry),
  normal: ir.Expr,
) -> ir.Expr {
  case entry {
    // jump-entered arms see stale mode/sent so only resume arms dispatch
    InitialEntry | JumpEntry -> normal
    ResumeEntry(_) ->
      ir.Let(
        ["_i32m"],
        ir.Convert(ir.UnboxInt(ir.W32), ctx.resume_mode),
        ir.Let(
          ["_is_thr"],
          ir.Num(ir.IEq(ir.W32), [
            ir.Var("_i32m"),
            ir.ConstI32(rt_async.sent_throw),
          ]),
          ir.If(
            ir.Var("_is_thr"),
            [ir.TTerm],
            route_throw(ctx, region, ctx.sent_value),
            ir.Let(
              ["_is_ret"],
              ir.Num(ir.IEq(ir.W32), [
                ir.Var("_i32m"),
                ir.ConstI32(rt_async.sent_return),
              ]),
              ir.If(
                ir.Var("_is_ret"),
                [ir.TTerm],
                route_return(ctx, region, ctx.sent_value),
                normal,
              ),
            ),
          ),
        ),
      )
  }
}

fn run_terminal(b: anf.Build(ir.Expr), e: Emitter) -> #(ir.Expr, Emitter) {
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
  use inner <- anf.then(
    anf.make_tuple([
      ir.ConstAtom("named"),
      ir.ConstBinary(bit_array.from_string(s)),
    ]),
  )
  anf.make_tuple([ir.ConstAtom("string_key"), inner])
}

fn get_named(obj: ir.Value, name: String) -> anf.Build(ir.Value) {
  use site <- anf.then(fn(e: Emitter, k) {
    k(state.Emitter(..e, next_ic_site: e.next_ic_site + 1), e.next_ic_site)
  })
  anf.host("get_named_site", [
    obj,
    ir.ConstBinary(bit_array.from_string(name)),
    ir.ConstI32(site),
  ])
}

fn key_named_dyn(bin: ir.Value) -> anf.Build(ir.Value) {
  use inner <- anf.then(anf.make_tuple([ir.ConstAtom("named"), bin]))
  anf.make_tuple([ir.ConstAtom("string_key"), inner])
}

fn iter_hint(kind: state.CoroutineKind) -> ir.Value {
  case kind {
    state.AsyncGenerator -> ir.ConstAtom("async")
    state.Generator | state.AsyncFunction -> ir.ConstAtom("sync")
  }
}

fn emit_delegate_setup(
  e: Emitter,
  ctx: MachineCtx,
  iterable: ir.Value,
  delegate_state: Int,
  iter_idx: Int,
  inner_idx: Int,
) -> #(ir.Expr, Emitter) {
  let result_idx = extra_idx(ctx.layout, delegate_result_key(delegate_state))
  let b = {
    use iterator <- anf.then(
      anf.host("get_iterator", [iterable, iter_hint(ctx.kind)]),
    )
    use k_iter <- anf.then(key_named("iterator"))
    use inner <- anf.then(anf.host("get_prop_untyped_key", [iterator, k_iter]))
    let ov =
      dict.from_list([
        #(iter_idx, iterator),
        #(inner_idx, inner),
        #(result_idx, ir.ConstAtom(delegate_start)),
      ])
    use loc2 <- anf.then(fn(e, k) { repack_live_locals(e, ctx, ov, k) })
    use rs <- anf.then(rs_box(delegate_state))
    anf.pure(ir.Continue(ctx.resume_loop_label, [rs, loc2]))
  }
  run_terminal(b, e)
}

// §27.5.3.8 yield* delegate arm, dispatches mode itself
fn emit_delegate_arm(
  e: Emitter,
  ctx: MachineCtx,
  delegate_spec: DelegateSpec,
  iter_idx: Int,
  inner_idx: Int,
  result_idx: Int,
) -> EmitResult {
  let undef = ir.ConstAtom("undefined")
  let b = {
    use iterator <- anf.then(
      anf.bind(anf.tuple_get(ctx.saved_locals, iter_idx)),
    )
    use inner <- anf.then(anf.bind(anf.tuple_get(ctx.saved_locals, inner_idx)))
    use flag <- anf.then(anf.bind(anf.tuple_get(ctx.saved_locals, result_idx)))
    use first <- anf.then(
      anf.bind(ir.NumTerm(ir.NEq, flag, ir.ConstAtom(delegate_start))),
    )
    use resume_mode <- anf.then(anf.bind_if(
      first,
      rs_box(rt_async.sent_next),
      anf.pure(ctx.resume_mode),
    ))
    use sent_value <- anf.then(anf.bind_if(
      first,
      anf.pure(undef),
      anf.pure(ctx.sent_value),
    ))
    let ctx = MachineCtx(..ctx, resume_mode:, sent_value:)
    use mode_i32 <- anf.then(
      anf.bind(ir.Convert(ir.UnboxInt(ir.W32), ctx.resume_mode)),
    )
    use mode_ne0 <- anf.then(
      anf.bind(
        ir.Num(ir.INe(ir.W32), [mode_i32, ir.ConstI32(rt_async.sent_next)]),
      ),
    )
    let mbin = fn(s) { ir.Values([ir.ConstBinary(bit_array.from_string(s))]) }
    use meth <- anf.then(anf.bind_if(
      mode_ne0,
      {
        use mname <- anf.then(
          anf.bind(ir.Switch(
            mode_i32,
            [ir.TTerm],
            [ir.SwitchArm(1, mbin("throw")), ir.SwitchArm(2, mbin("return"))],
            mbin("next"),
          )),
        )
        use key <- anf.then(key_named_dyn(mname))
        anf.host("get_prop_untyped_key", [inner, key])
      },
      get_named(iterator, "next"),
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
      anf.bind(
        ir.Num(ir.IEq(ir.W32), [mode_i32, ir.ConstI32(rt_async.sent_throw)]),
      ),
    )
    let on_missing =
      if_terminal(
        is_throw,
        {
          use _ <- anf.then(
            anf.host_unit("iter_close", [
              iterator,
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
          anf.pure(step_throw(ctx.sent_value))
        },
        anf.pure(route_return(ctx, current_try(ctx), ctx.sent_value)),
      )
    let on_call = {
      use argl <- anf.then(anf.cons_list([ctx.sent_value]))
      use res <- anf.then(anf.host("call_checked", [meth, inner, argl]))
      case delegate_spec.await_state {
        Some(await_state) -> {
          use loc2 <- anf.then(repack_saved_locals(
            ctx,
            dict.from_list([#(result_idx, ctx.resume_mode)]),
          ))
          anf.pure(step_await(res, await_state, loc2))
        }
        None ->
          delegate_result(ctx, delegate_spec, res, mode_i32, result_idx, first)
      }
    }
    if_terminal(missing, on_missing, on_call)
  }
  Ok(run_terminal(b, e))
}

fn emit_delegate_await_arm(
  e: Emitter,
  ctx: MachineCtx,
  delegate_spec: DelegateSpec,
  result_idx: Int,
) -> EmitResult {
  let b = {
    use resume_mode <- anf.then(
      anf.bind(anf.tuple_get(ctx.saved_locals, result_idx)),
    )
    use mode_i32 <- anf.then(
      anf.bind(ir.Convert(ir.UnboxInt(ir.W32), resume_mode)),
    )
    delegate_result(
      ctx,
      delegate_spec,
      ctx.sent_value,
      mode_i32,
      result_idx,
      ir.ConstI32(0),
    )
  }
  Ok(run_terminal(b, e))
}

fn delegate_result(
  ctx: MachineCtx,
  delegate_spec: DelegateSpec,
  res: ir.Value,
  mode_i32: ir.Value,
  result_idx: Int,
  first: ir.Value,
) -> anf.Build(ir.Expr) {
  use is_obj <- anf.then(anf.host_bool("is_object", [res]))
  use is_return <- anf.then(
    anf.bind(
      ir.Num(ir.IEq(ir.W32), [mode_i32, ir.ConstI32(rt_async.sent_return)]),
    ),
  )
  if_terminal(
    is_obj,
    {
      use done_t <- anf.then(get_named(res, "done"))
      use done <- anf.then(anf.host("to_boolean_i32", [done_t]))
      use v <- anf.then(get_named(res, "value"))
      if_terminal(
        done,
        if_terminal(
          is_return,
          anf.pure(route_return(ctx, current_try(ctx), v)),
          {
            use loc2 <- anf.then(repack_saved_locals(
              ctx,
              dict.from_list([#(result_idx, v)]),
            ))
            use rs <- anf.then(rs_box(delegate_spec.next_state))
            anf.pure(ir.Continue(ctx.resume_loop_label, [rs, loc2]))
          },
        ),
        {
          use loc2 <- anf.then(anf.bind_if(
            first,
            repack_saved_locals(
              ctx,
              dict.from_list([#(result_idx, ir.ConstAtom("undefined"))]),
            ),
            anf.pure(ctx.saved_locals),
          ))
          anf.pure(step_yield(v, delegate_spec.state_id, loc2))
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

fn find_try_entry(ctx: MachineCtx, id: Int) -> Option(TryEntry) {
  list.find(ctx.try_entries, fn(t) { t.id == id }) |> option.from_result
}

fn outer_entry(ctx: MachineCtx, entry: TryEntry) -> Option(TryEntry) {
  find_try(ctx.try_entries, entry.outer)
}

fn restore_and_seed(e: Emitter, ctx: MachineCtx, k: Next) -> EmitResult {
  restore_and_seed_loop(e, ctx, dict.to_list(ctx.layout.slot_to_idx), k)
}

fn restore_and_seed_loop(
  e: Emitter,
  ctx: MachineCtx,
  slots: List(#(Int, Int)),
  k: Next,
) -> EmitResult {
  case slots {
    [] -> k(e)
    [#(slot, idx), ..rest] -> {
      let #(name, e) = state.fresh_slot_var(e, slot)
      let e = state.set_slot_var(e, slot, name)
      use body <- state.map_tree(restore_and_seed_loop(e, ctx, rest, k))
      ir.Let([name], ir.TermOp(ir.TupleGet(idx), [ctx.saved_locals]), body)
    }
  }
}

// finally-exit redispatch of a carried completion: catch first, live locals
fn dispatch_throw(
  e: Emitter,
  ctx: MachineCtx,
  outer: Option(TryEntry),
  carry: ir.Value,
) -> #(ir.Expr, Emitter) {
  case outer {
    None -> #(step_throw(carry), e)
    Some(o) ->
      case o.catch_state, o.finally_state {
        Some(catch_state), _ ->
          jump_state_leaf(
            e,
            ctx,
            catch_state,
            dict.from_list([#(o.caught_loc_idx, carry)]),
          )
        None, Some(finally_state) ->
          jump_to_finally(
            e,
            ctx,
            o,
            finally_state,
            pending_tuple(PendingThrow(carry)),
          )
        None, None -> dispatch_throw(e, ctx, outer_entry(ctx, o), carry)
      }
  }
}

fn dispatch_return(
  e: Emitter,
  ctx: MachineCtx,
  outer: Option(TryEntry),
  carry: ir.Value,
) -> #(ir.Expr, Emitter) {
  case outer {
    None -> #(step_return(carry), e)
    Some(o) ->
      case o.finally_state {
        Some(finally_state) ->
          jump_to_finally(
            e,
            ctx,
            o,
            finally_state,
            pending_tuple(PendingReturn(carry)),
          )
        None -> dispatch_return(e, ctx, outer_entry(ctx, o), carry)
      }
  }
}

fn dispatch_goto(
  e: Emitter,
  ctx: MachineCtx,
  outer: Option(TryEntry),
  carry: ir.Value,
) -> #(ir.Expr, Emitter) {
  case outer {
    None -> {
      // carry is the boxed target state, not a compile-time int
      let #(target, e) = state.fresh_var(e)
      let jump = {
        use e, loc <- repack_live_locals(e, ctx, dict.new())
        #(ir.Continue(ctx.resume_loop_label, [ir.Var(target), loc]), e)
      }
      anf.wrap(jump, ir.Let([target], ir.Convert(ir.UnboxInt(ir.W32), carry), _))
    }
    Some(o) ->
      case o.finally_state {
        Some(finally_state) -> {
          let pend = ir.TermOp(ir.MakeTuple, [ir.ConstI32(pend_goto), carry])
          jump_to_finally(e, ctx, o, finally_state, pend)
        }
        None -> dispatch_goto(e, ctx, outer_entry(ctx, o), carry)
      }
  }
}

fn build_pending_dispatch(
  e: Emitter,
  ctx: MachineCtx,
  entry: TryEntry,
  pend: ir.Value,
) -> #(ir.Expr, Emitter) {
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

fn with_done(
  e: Emitter,
  body: fn(Emitter, NextWith(ir.Expr)) -> EmitResult,
) -> EmitResult {
  body(e, fn(ef, tree) { Ok(#(tree, ef)) })
}

fn emit_finally_arm(
  e: Emitter,
  ctx: MachineCtx,
  entry: TryEntry,
  finalizer: List(ast.StmtWithLine),
) -> EmitResult {
  with_done(e, fn(e, done) {
    use e <- restore_and_seed(e, ctx)
    let #(pend_n, e) = state.fresh_var(e)
    let pend = ir.Var(pend_n)
    use #(body, e_out) <- result.try(
      with_abrupt_intercept(e, ctx, fn(e, restore) {
        let k_tail = fn(e_leaf: Emitter) {
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
        ir.TermOp(ir.TupleGet(entry.pending_loc_idx), [ctx.saved_locals]),
        body,
      ),
    )
  })
}

fn emit_catch_arm(
  e: Emitter,
  ctx: MachineCtx,
  entry: TryEntry,
  handler: ast.CatchClause,
) -> EmitResult {
  let ast.CatchClause(param:, body: catch_body) = handler
  with_done(e, fn(e, done) {
    use e <- restore_and_seed(e, ctx)
    let #(caught_n, e) = state.fresh_var(e)
    let caught = ir.Var(caught_n)
    use #(handler_tree, e_out) <- result.try(
      with_abrupt_intercept(e, ctx, fn(e, restore) {
        let k_tail = fn(e_leaf: Emitter) {
          Ok(case entry.finally_state {
            Some(finally_state) ->
              jump_state_leaf(
                e_leaf,
                ctx,
                finally_state,
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
        ir.TermOp(ir.TupleGet(entry.caught_loc_idx), [ctx.saved_locals]),
        handler_tree,
      ),
    )
  })
}

fn emit_arm_body(e: Emitter, ctx: MachineCtx, arm: ArmSpec) -> EmitResult {
  let e = install_cursor(e, arm.entry_cursor)
  let ctx =
    MachineCtx(
      ..with_region(ctx, arm.region),
      machine_frames: arm.machine_frames,
    )
  use e <- restore_and_seed(e, ctx)
  with_abrupt_intercept(e, ctx, fn(e, restore) {
    use #(tree, e2) <- result.map(case arm.resume {
      Some(ResumeReturn) ->
        Ok(route_abrupt(e, ctx, PendingReturn(ctx.sent_value), None))
      Some(ResumeThrow) ->
        Ok(route_abrupt(e, ctx, PendingThrow(ctx.sent_value), None))
      _ -> {
        use #(prelude, e) <- result.try(case arm.resume {
          Some(ResumeBind(pat, mode)) ->
            e.dispatch.emit_destructure(e, pat, ctx.sent_value, mode)
          Some(ResumeCatch(try_id, Some(pat))) -> {
            let entry = find_try_entry(ctx, try_id)
            let idx = case entry {
              Some(t) -> t.caught_loc_idx
              None -> panic as "aot/async: ResumeCatch on unknown try region"
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
                ir.TermOp(ir.TupleGet(idx), [ctx.saved_locals]),
                dtree,
              ),
              e,
            )
          }
          _ -> Ok(#(ir.Values([e.consts.undef]), e))
        })
        let #(pre_n, e) = state.fresh_var(e)
        let k_tail = fn(e_leaf: Emitter) {
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

fn emit_seg_tail(e: Emitter, ctx: MachineCtx, tail: SegTail) -> EmitResult {
  case tail {
    BodyEnd -> Ok(#(step_return(e.consts.undef), e))
    // unreachable in practice, keeps the arm well-typed
    SegDone -> Ok(#(step_return(e.consts.undef), e))
    FallTo(to) -> Ok(jump_state_leaf(e, ctx, to, dict.new()))
    FallToFinally(try_id, to) ->
      case find_try_entry(ctx, try_id) {
        Some(entry) ->
          Ok(jump_state_leaf(
            e,
            ctx,
            to,
            dict.from_list([#(entry.pending_loc_idx, ir.ConstAtom("normal"))]),
          ))
        None -> panic as "aot/async: FallToFinally on unknown try region"
      }
    FinallyEnd(try_id) ->
      case find_try_entry(ctx, try_id) {
        Some(entry) -> {
          let #(pend_n, e) = state.fresh_var(e)
          let #(tree, e) = build_pending_dispatch(e, ctx, entry, ir.Var(pend_n))
          Ok(#(
            ir.Let(
              [pend_n],
              ir.TermOp(ir.TupleGet(entry.pending_loc_idx), [ctx.saved_locals]),
              tree,
            ),
            e,
          ))
        }
        None -> panic as "aot/async: FinallyEnd on unknown try region"
      }
    SplitAt(kind, arg, resume_state) -> {
      use #(operand_tree, e) <- result.try(emit_opt_expr(e, arg, e.consts.undef))
      let #(v_n, e) = state.fresh_var(e)
      case kind {
        YieldStarSplit -> {
          let iter_idx = extra_idx(ctx.layout, iter_key(resume_state))
          let inner_idx = extra_idx(ctx.layout, inner_key(resume_state))
          let #(setup, e) =
            emit_delegate_setup(
              e,
              ctx,
              ir.Var(v_n),
              resume_state,
              iter_idx,
              inner_idx,
            )
          Ok(#(ir.Let([v_n], operand_tree, setup), e))
        }
        AwaitSplit | ForAwaitSplit | YieldSplit -> {
          let step = fn(v, loc) {
            case kind {
              AwaitSplit | ForAwaitSplit -> step_await(v, resume_state, loc)
              YieldSplit | YieldStarSplit -> step_yield(v, resume_state, loc)
            }
          }
          let suspend = {
            use e, loc <- repack_live_locals(e, ctx, dict.new())
            #(step(ir.Var(v_n), loc), e)
          }
          Ok(anf.wrap(suspend, ir.Let([v_n], operand_tree, _)))
        }
      }
    }
    CondBranch(cond, then_state, else_state) -> {
      use #(cond_tree, e) <- result.try(emit_head_value(e, ctx, cond))
      let #(cv_n, e) = state.fresh_var(e)
      let #(ti_n, e) = state.fresh_var(e)
      let branch = {
        use e, loc <- repack_live_locals(e, ctx, dict.new())
        let then_jump = machine_continue(ctx, then_state, loc)
        let else_jump = machine_continue(ctx, else_state, loc)
        #(ir.If(ir.Var(ti_n), [ir.TTerm], then_jump, else_jump), e)
      }
      let cv = option.unwrap(state.let_tail_value(cond_tree), ir.Var(cv_n))
      Ok(
        anf.wrap(branch, fn(t) {
          state.splice_let(
            cond_tree,
            cv_n,
            ir.Let([ti_n], ir.CallHost("js", "to_boolean_i32", [cv]), t),
          )
        }),
      )
    }
    ForUpdate(update, head) -> {
      use #(upd_tree, e) <- result.try(emit_opt_expr(e, update, e.consts.undef))
      let #(tmp, e) = state.fresh_var(e)
      let jump = jump_state_leaf(e, ctx, head, dict.new())
      Ok(anf.wrap(jump, state.splice_let(upd_tree, tmp, _)))
    }
    ForOfStep(left, iter_key, body_state, after) ->
      emit_for_of_step(e, ctx, left, iter_key, body_state, after)
    ForOfSetup(right, iter_key, head) ->
      emit_iterator_setup(e, ctx, right, iter_key, ir.ConstAtom("sync"), head)
    ForAwaitSetup(right, head) -> {
      let hint = ir.ConstAtom("async")
      emit_iterator_setup(e, ctx, right, for_await_iter_key(head), hint, head)
    }
    AsyncGenYieldSent(resume_state) ->
      Ok({
        use e, loc <- repack_live_locals(e, ctx, dict.new())
        #(step_yield(ctx.sent_value, resume_state, loc), e)
      })
    SwitchDispatch(discriminant, tests, after) ->
      emit_switch_dispatch(e, ctx, discriminant, tests, after)
  }
}

fn emit_opt_expr(
  e: Emitter,
  arg: Option(ast.Expression),
  otherwise: ir.Value,
) -> EmitResult {
  case arg {
    Some(ex) -> e.dispatch.emit_expr(e, ex)
    None -> Ok(#(ir.Values([otherwise]), e))
  }
}

fn emit_head_value(e: Emitter, ctx: MachineCtx, head: HeadValue) -> EmitResult {
  case head {
    FromExpr(ex) -> e.dispatch.emit_expr(e, ex)
    FromResumedValue -> Ok(#(ir.Values([ctx.sent_value]), e))
  }
}

// gets the iterator for right, stores it in the iter slot, jumps to head
fn emit_iterator_setup(
  e: Emitter,
  ctx: MachineCtx,
  right: ast.Expression,
  iter_key: String,
  hint: ir.Value,
  head: Int,
) -> EmitResult {
  use #(rhs_tree, e) <- result.map(e.dispatch.emit_expr(e, right))
  let iter_idx = extra_idx(ctx.layout, iter_key)
  let #(rhs_n, e) = state.fresh_var(e)
  let #(iter_n, e) = state.fresh_var(e)
  let over = dict.from_list([#(iter_idx, ir.Var(iter_n))])
  use t <- anf.wrap(jump_state_leaf(e, ctx, head, over))
  let get_iter = ir.CallHost("js", "get_iterator", [ir.Var(rhs_n), hint])
  ir.Let([rhs_n], rhs_tree, ir.Let([iter_n], get_iter, t))
}

fn emit_for_of_step(
  e: Emitter,
  ctx: MachineCtx,
  left: ast.ForInit,
  iter_key: String,
  body_state: Int,
  after: Int,
) -> EmitResult {
  let iter_idx = extra_idx(ctx.layout, iter_key)
  with_done(e, fn(e, done) {
    let #(iter_n, e) = state.fresh_var(e)
    let #(res_n, e) = state.fresh_var(e)
    let #(done_t, e) = state.fresh_var(e)
    let #(done_i, e) = state.fresh_var(e)
    let #(val_n, e) = state.fresh_var(e)
    let #(dk_n, e) = state.fresh_var(e)
    let #(vk_n, e) = state.fresh_var(e)
    let #(done_branch, e) = jump_state_leaf(e, ctx, after, dict.new())
    use #(bind_tree, e) <- result.try(bind_for_lhs(e, left, ir.Var(val_n)))
    let #(tmp, e) = state.fresh_var(e)
    let #(body_branch, e) = {
      use t <- anf.wrap(jump_state_leaf(e, ctx, body_state, dict.new()))
      ir.Let(
        [val_n],
        ir.TermOp(ir.TupleGet(1), [ir.Var(res_n)]),
        state.splice_let(bind_tree, tmp, t),
      )
    }
    let branch = ir.If(ir.Var(done_i), [ir.TTerm], done_branch, body_branch)
    done(
      e,
      ir.Let(
        [iter_n],
        ir.TermOp(ir.TupleGet(iter_idx), [ctx.saved_locals]),
        ir.Let(
          [res_n],
          ir.CallHost("js", "iter_next", [ir.Var(iter_n)]),
          ir.Let(
            [dk_n],
            named_key_tuple("done"),
            ir.Let(
              [vk_n],
              named_key_tuple("value"),
              ir.Let(
                [done_t],
                ir.TermOp(ir.TupleGet(0), [ir.Var(res_n)]),
                ir.Let([done_i], anf.is_true_expr(ir.Var(done_t)), branch),
              ),
            ),
          ),
        ),
      ),
    )
  })
}

fn named_key_tuple(s: String) -> ir.Expr {
  let inner_n = "_nk_" <> s
  ir.Let(
    [inner_n],
    ir.TermOp(ir.MakeTuple, [
      ir.ConstAtom("named"),
      ir.ConstBinary(bit_array.from_string(s)),
    ]),
    ir.TermOp(ir.MakeTuple, [ir.ConstAtom("string_key"), ir.Var(inner_n)]),
  )
}

fn bind_for_lhs(e: Emitter, left: ast.ForInit, v: ir.Value) -> EmitResult {
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
  e: Emitter,
  ctx: MachineCtx,
  discriminant: HeadValue,
  tests: List(#(Option(ast.Expression), Int)),
  after: Int,
) -> EmitResult {
  use #(discriminant_tree, e) <- result.try(emit_head_value(
    e,
    ctx,
    discriminant,
  ))
  let #(dv_n, e) = state.fresh_var(e)
  let dv = ir.Var(dv_n)
  use #(chain, e) <- result.try(switch_chain(e, ctx, dv, tests, after))
  Ok(#(ir.Let([dv_n], discriminant_tree, chain), e))
}

fn switch_chain(
  e: Emitter,
  ctx: MachineCtx,
  dv: ir.Value,
  tests: List(#(Option(ast.Expression), Int)),
  after: Int,
) -> EmitResult {
  case tests {
    [] -> Ok(jump_state_leaf(e, ctx, after, dict.new()))
    [#(None, target), ..] -> Ok(jump_state_leaf(e, ctx, target, dict.new()))
    [#(Some(case_test), target), ..rest] -> {
      use #(test_tree, e) <- result.try(e.dispatch.emit_expr(e, case_test))
      let #(tv_n, e) = state.fresh_var(e)
      let #(eq_n, e) = state.fresh_var(e)
      let #(eqi_n, e) = state.fresh_var(e)
      use #(else_tree, e) <- result.try(switch_chain(e, ctx, dv, rest, after))
      let #(hit, e) = jump_state_leaf(e, ctx, target, dict.new())
      Ok(#(
        ir.Let(
          [tv_n],
          test_tree,
          ir.Let(
            [eq_n],
            ir.CallHost("js", "strict_eq", [dv, ir.Var(tv_n)]),
            ir.Let(
              [eqi_n],
              ir.CallHost("js", "to_boolean_i32", [ir.Var(eq_n)]),
              ir.If(ir.Var(eqi_n), [ir.TTerm], hit, else_tree),
            ),
          ),
        ),
        e,
      ))
    }
  }
}

type HoistedItem {
  PlainStmt(ast.StmtWithLine)
  SplitStmt(
    kind: SplitKind,
    operand: Option(ast.Expression),
    resume: ResumeWith,
  )
}

// rewrite so every recognised await/yield sits at a statement boundary
fn hoist_one(located: ast.StmtWithLine) -> List(HoistedItem) {
  let ast.StmtWithLine(line, stmt) = located
  case stmt {
    ast.ExpressionStatement(expression: ex, ..) ->
      case split_of(ex), ex {
        Some(#(kind, operand)), _ -> [SplitStmt(kind, operand, ResumeDiscard)]
        None, ast.SequenceExpression(_, parts) ->
          list.flat_map(parts, fn(p) { hoist_one(expr_stmt(line, p)) })
        None, ast.AssignmentExpression(_, ast.Assign, lhs, rhs) ->
          case lhs_to_pattern(lhs) {
            Some(pat) ->
              hoist_if_split(located, rhs, ResumeBind(pat, state.BindAssign))
            None -> [PlainStmt(located)]
          }
        None, _ -> [PlainStmt(located)]
      }
    ast.VariableDeclaration(kind, [ast.VariableDeclarator(pat, Some(init))]) ->
      hoist_if_split(located, init, ResumeBind(pat, bind_mode_of(kind)))
    ast.VariableDeclaration(kind, decls) ->
      list.flat_map(decls, fn(d) {
        hoist_one(ast.StmtWithLine(line, ast.VariableDeclaration(kind, [d])))
      })
    ast.ReturnStatement(Some(ex)) -> hoist_if_split(located, ex, ResumeReturn)
    ast.ThrowStatement(ex) -> hoist_if_split(located, ex, ResumeThrow)
    _ -> [PlainStmt(located)]
  }
}

fn hoist_if_split(
  located: ast.StmtWithLine,
  ex: ast.Expression,
  resume: ResumeWith,
) -> List(HoistedItem) {
  case split_of(ex) {
    Some(#(kind, operand)) -> [SplitStmt(kind:, operand:, resume:)]
    None -> [PlainStmt(located)]
  }
}

fn split_of(
  ex: ast.Expression,
) -> Option(#(SplitKind, Option(ast.Expression))) {
  case ex {
    ast.AwaitExpression(_, arg) -> Some(#(AwaitSplit, Some(arg)))
    ast.YieldExpression(_, arg, is_delegate: False) -> Some(#(YieldSplit, arg))
    ast.YieldExpression(_, arg, is_delegate: True) ->
      Some(#(YieldStarSplit, arg))
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
    Error(Nil) -> panic as { "aot/async: loc layout has no extra " <> key }
  }
}

fn build_switch_arms(
  e: Emitter,
  ctx: MachineCtx,
  plan: SplitPlan,
) -> Result(#(List(ir.SwitchArm), Emitter), state.EmitError) {
  use #(ctx, e) <- result.try(
    list.try_fold(plan.arms, #(ctx, e), fn(st, arm) {
      let #(ctx, e) = st
      let ctx = with_region(ctx, arm.region)
      let region = current_try(ctx)
      let follow_of =
        list.find(plan.delegates, fn(delegate_spec) {
          delegate_spec.next_state == arm.state_id
        })
      use #(wrapped, e) <- result.map(case arm.entry_kind, follow_of {
        ResumeEntry(YieldStarSplit), Ok(delegate_spec) -> {
          let #(rv, e) = state.fresh_var(e)
          let idx =
            extra_idx(ctx.layout, delegate_result_key(delegate_spec.state_id))
          let arm_ctx = MachineCtx(..ctx, sent_value: ir.Var(rv))
          use #(inner, e) <- result.map(emit_arm_body(e, arm_ctx, arm))
          let body =
            ir.Let([rv], ir.TermOp(ir.TupleGet(idx), [ctx.saved_locals]), inner)
          #(wrap_arm_try(e, ctx, region, body), e)
        }
        _, _ -> {
          use #(inner, e) <- result.map(emit_arm_body(e, ctx, arm))
          let dispatched =
            emit_mode_dispatch(ctx, arm.entry_kind, region, inner)
          #(wrap_arm_try(e, ctx, region, dispatched), e)
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
        Some(catch_state), Some(h) -> {
          let ctx =
            MachineCtx(
              ..with_catch_body(ctx, entry),
              machine_frames: entry.machine_frames,
            )
          let catch_wrap_region = current_try(ctx)
          let e = case entry.catch_cursor {
            Some(c) -> install_cursor(e, c)
            None -> e
          }
          use #(inner, e) <- result.map(emit_catch_arm(e, ctx, entry, h))
          let wrapped = wrap_arm_try(e, ctx, catch_wrap_region, inner)
          #(push_arm(ctx, catch_state, wrapped), e)
        }
        _, _ -> Ok(#(ctx, e))
      })
      case entry.finally_state, entry.finalizer {
        Some(finally_state), Some(fin) -> {
          let ctx =
            MachineCtx(
              ..with_finally_body(ctx, entry),
              machine_frames: entry.machine_frames,
            )
          let e = case entry.finally_cursor {
            Some(c) -> install_cursor(e, c)
            None -> e
          }
          use #(inner, e) <- result.map(emit_finally_arm(e, ctx, entry, fin))
          let wrapped = wrap_arm_try(e, ctx, outer_region, inner)
          #(push_arm(ctx, finally_state, wrapped), e)
        }
        _, _ -> Ok(#(ctx, e))
      }
    }),
  )
  use #(ctx, e) <- result.try(
    list.try_fold(plan.delegates, #(ctx, e), fn(st, delegate_spec) {
      let #(ctx, e) = st
      let ctx = with_region(ctx, delegate_spec.region)
      let region = current_try(ctx)
      let iter_idx = extra_idx(ctx.layout, iter_key(delegate_spec.state_id))
      let inner_idx = extra_idx(ctx.layout, inner_key(delegate_spec.state_id))
      let result_idx =
        extra_idx(ctx.layout, delegate_result_key(delegate_spec.state_id))
      use #(inner, e) <- result.try(emit_delegate_arm(
        e,
        ctx,
        delegate_spec,
        iter_idx,
        inner_idx,
        result_idx,
      ))
      let ctx =
        push_arm(
          ctx,
          delegate_spec.state_id,
          wrap_arm_try(e, ctx, region, inner),
        )
      case delegate_spec.await_state {
        None -> Ok(#(ctx, e))
        Some(await_state) -> {
          use #(body, e) <- result.map(emit_delegate_await_arm(
            e,
            ctx,
            delegate_spec,
            result_idx,
          ))
          let dispatched =
            emit_mode_dispatch(ctx, ResumeEntry(AwaitSplit), region, body)
          #(
            push_arm(ctx, await_state, wrap_arm_try(e, ctx, region, dispatched)),
            e,
          )
        }
      }
    }),
  )
  use #(ctx, e) <- result.map(
    list.try_fold(plan.for_awaits, #(ctx, e), fn(st, spec) {
      let #(ctx, e) = st
      let ctx = with_region(ctx, spec.region)
      let region = current_try(ctx)
      let #(head_body, e) = emit_for_await_head(e, ctx, spec)
      let ctx =
        push_arm(ctx, spec.head, wrap_arm_try(e, ctx, region, head_body))
      use #(check_body, e) <- result.map(emit_for_await_check(e, ctx, spec))
      let dispatched =
        emit_mode_dispatch(ctx, ResumeEntry(AwaitSplit), region, check_body)
      #(push_arm(ctx, spec.check, wrap_arm_try(e, ctx, region, dispatched)), e)
    }),
  )
  #(finish_arms(ctx), e)
}

pub fn emit_coroutine_fn(
  e: Emitter,
  shape: state.FnShape,
  js_name: Option(String),
  params: List(ast.Pattern),
  body: state.FnBody,
  fn_scope_id: ScopeId,
  captures: List(ir.Value),
) -> EmitResult {
  let assert Some(kind) = func.shape_coroutine(shape)
    as "emit_coroutine_fn: shape is not a coroutine"
  let info = scope.function_info(e.scope_tree, fn_scope_id)
  let ncap = list.length(captures)
  let stmts = func.body_stmts(body)
  let is_strict = e.strict || ast_util.has_use_strict_directive(stmts)
  let #(outer_name, e) = state.fresh_fn_name(e, js_name)
  let machine_name = outer_name <> "__sm"
  let enter = fn(e) {
    state.enter_function(
      e,
      fn_scope_id,
      strict: is_strict,
      is_async: kind_is_async(kind),
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
    let plan = analyze_splits(e_pro.scope_tree, cur0, body, kind)
    let #(machine_tree, info) =
      add_temp_slots(e_pro.scope_tree, fn_scope_id, info, plan.n_temps)
    let layout = compute_loc_layout(info, plan)
    let plan =
      SplitPlan(
        ..plan,
        try_entries: enrich_try_entries(plan.try_entries, layout),
      )
    let #(e_machine, machine_save) = enter(e_pro)
    let e_machine = state.Emitter(..e_machine, cap_names: e_pro.cap_names)
    let e_machine =
      state.Emitter(
        ..install_cursor(e_machine, cur0),
        initialized_slots: e_pro.initialized_slots,
        scope_tree: machine_tree,
      )
    let #(resume_loop_label, e_machine) = state.fresh_label(e_machine)
    let ctx = new_machine_ctx(kind, layout, resume_loop_label, plan)
    use #(arms, e_machine) <- result.try(build_switch_arms(e_machine, ctx, plan))
    let #(default, e_machine) = machine_default_arm(e_machine)
    let e_machine =
      emit_machine_function(
        e_machine,
        machine_name,
        ncap,
        resume_loop_label,
        arms,
        default,
      )
    let e_pro = state.leave_function(e_machine, machine_save)
    let #(tree, e_pro) =
      anf.run_to(
        {
          use loc0 <- anf.then(
            anf.make_tuple(initial_loc_values(e_pro, layout, info.local_count)),
          )
          use machine <- anf.then(
            anf.bind(ir.MakeClosure(machine_name, cap_vars(e_pro, 0, ncap), 3)),
          )
          anf.host(start_op(kind), [
            machine,
            ir.Var(func.frame_param),
            ir.Var(func.args_param),
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
    state.AsyncFunction ->
      ir.Try(result: [ir.TTerm], body: body_expr, handlers: [
        ir.CatchHandler(
          on: ir.OnTag(e_outer.consts.exn_tag),
          payload: [ex],
          exnref: None,
          handler: ir.CallHost("js", "async_reject", [ir.Var(ex)]),
        ),
      ])
    state.Generator | state.AsyncGenerator -> body_expr
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
  let e = state.leave_function(e_outer, save)
  Ok(emit_closure_alloc(
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
              boxed: False,
              declared_kind: scope.VarBinding,
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
  e: Emitter,
  ctx: MachineCtx,
  spec: ForAwaitSpec,
) -> #(ir.Expr, Emitter) {
  let iter_idx = extra_idx(ctx.layout, for_await_iter_key(spec.head))
  let b = {
    use iterator <- anf.then(
      anf.bind(anf.tuple_get(ctx.saved_locals, iter_idx)),
    )
    use promise <- anf.then(anf.host("async_iter_next", [iterator]))
    use loc2 <- anf.then(repack_saved_locals(ctx, dict.new()))
    anf.pure(step_await(promise, spec.check, loc2))
  }
  run_terminal(b, e)
}

fn emit_for_await_check(
  e: Emitter,
  ctx: MachineCtx,
  spec: ForAwaitSpec,
) -> EmitResult {
  with_done(e, fn(e, done) {
    use e <- restore_and_seed(e, ctx)
    let e = install_cursor(e, spec.body_cursor)
    let #(done_branch, e) =
      anf.run_to(repack_saved_locals(ctx, dict.new()), e, fn(_e, loc2) {
        ir.Continue(ctx.resume_loop_label, [ir.ConstI32(spec.after), loc2])
      })
    let #(val_name, e) = state.fresh_var(e)
    use #(bind_tree, e) <- result.try(bind_for_lhs(
      e,
      spec.left,
      ir.Var(val_name),
    ))
    let #(drop, e) = state.fresh_var(e)
    let #(body_jump, e) = jump_state_leaf(e, ctx, spec.body_state, dict.new())
    let not_done = state.splice_let(bind_tree, drop, body_jump)
    let #(chain, e) =
      run_terminal(
        {
          use done_jv <- anf.then(get_named(ctx.sent_value, "done"))
          use done_i <- anf.then(anf.host("to_boolean_i32", [done_jv]))
          use value <- anf.then(get_named(ctx.sent_value, "value"))
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

fn fresh_temp(p: SplitPlanner) -> #(String, SplitPlanner) {
  #(temp_name(p.next_temp), SplitPlanner(..p, next_temp: p.next_temp + 1))
}

type HoistedExpr =
  #(SplitPlanner, List(ast.StmtWithLine), ast.Expression)

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

fn spill_to_temp(
  p: SplitPlanner,
  line: Int,
  ex: ast.Expression,
) -> HoistedExpr {
  case is_trivial(ex) {
    True -> #(p, [], ex)
    False -> {
      let span = ex.span
      let #(t, p) = fresh_temp(p)
      case ex {
        ast.SpreadElement(sspan, arg) -> {
          let arr =
            ast.ArrayExpression(span, [Some(ast.SpreadElement(sspan, arg))])
          #(
            p,
            [assign_stmt(line, span, t, arr)],
            ast.SpreadElement(sspan, ident(span, t)),
          )
        }
        ast.ClassExpression(..) -> {
          let zero = ast.NumberLiteral(span, ast.FiniteNumber(0.0))
          let seq = ast.SequenceExpression(span, [zero, ex])
          #(p, [assign_stmt(line, span, t, seq)], ident(span, t))
        }
        _ -> #(p, [assign_stmt(line, span, t, ex)], ident(span, t))
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

fn hoist_keeping_top_split(
  p: SplitPlanner,
  line: Int,
  ex: ast.Expression,
) -> HoistedExpr {
  case split_of(ex) {
    Some(#(kind, Some(op))) ->
      case expr_has_split(op) {
        False -> #(p, [], ex)
        True -> {
          let #(p, pre, op2) = hoist_expr(p, line, op)
          let span = ex.span
          let rebuilt = case kind {
            AwaitSplit -> ast.AwaitExpression(span, op2)
            YieldSplit -> ast.YieldExpression(span, Some(op2), False)
            YieldStarSplit -> ast.YieldExpression(span, Some(op2), True)
            ForAwaitSplit -> ex
          }
          #(p, pre, rebuilt)
        }
      }
    Some(#(_, None)) -> #(p, [], ex)
    None -> hoist_expr(p, line, ex)
  }
}

fn hoist_expr(p: SplitPlanner, line: Int, ex: ast.Expression) -> HoistedExpr {
  case expr_has_split(ex) {
    False -> #(p, [], ex)
    True -> hoist_subexprs(p, line, ex)
  }
}

fn hoist_opt(
  p: SplitPlanner,
  line: Int,
  o: Option(ast.Expression),
) -> #(SplitPlanner, List(ast.StmtWithLine), Option(ast.Expression)) {
  case o {
    None -> #(p, [], None)
    Some(ex) -> {
      let #(p, pre, ex2) = hoist_expr(p, line, ex)
      #(p, pre, Some(ex2))
    }
  }
}

fn hoist_subexprs(
  p: SplitPlanner,
  line: Int,
  ex: ast.Expression,
) -> HoistedExpr {
  case ex {
    ast.AwaitExpression(span, arg) -> {
      let #(p, pre, arg2) = hoist_expr(p, line, arg)
      let #(t, p) = fresh_temp(p)
      #(
        p,
        list.append(pre, [
          assign_stmt(line, span, t, ast.AwaitExpression(span, arg2)),
        ]),
        ident(span, t),
      )
    }
    ast.YieldExpression(span, arg, del) -> {
      let #(p, pre, arg2) = hoist_opt(p, line, arg)
      let #(t, p) = fresh_temp(p)
      #(
        p,
        list.append(pre, [
          assign_stmt(line, span, t, ast.YieldExpression(span, arg2, del)),
        ]),
        ident(span, t),
      )
    }
    ast.ParenthesizedExpression(span, inner) -> {
      let #(p, pre, inner2) = hoist_expr(p, line, inner)
      #(p, pre, ast.ParenthesizedExpression(span, inner2))
    }
    ast.SpreadElement(span, arg) -> {
      let #(p, pre, arg2) = hoist_expr(p, line, arg)
      #(p, pre, ast.SpreadElement(span, arg2))
    }
    ast.BinaryExpression(span, op, l, r) -> {
      let #(p, pre, xs) = hoist_list(p, line, [l, r])
      case xs {
        [l2, r2] -> #(p, pre, ast.BinaryExpression(span, op, l2, r2))
        _ -> #(p, pre, ex)
      }
    }
    ast.LogicalExpression(span, op, l, r) ->
      case expr_has_split(r) {
        False -> {
          let #(p, pre, l2) = hoist_expr(p, line, l)
          #(p, pre, ast.LogicalExpression(span, op, l2, r))
        }
        True -> {
          let #(p, pre_l, l2) = hoist_expr(p, line, l)
          let #(t, p) = fresh_temp(p)
          let #(p, pre_r, r2) = hoist_expr(p, line, r)
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
            p,
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
          let #(p, pre, c2) = hoist_expr(p, line, c)
          #(p, pre, ast.ConditionalExpression(span, c2, x, y))
        }
        True -> {
          let #(p, pre_c, c2) = hoist_expr(p, line, c)
          let #(t, p) = fresh_temp(p)
          let #(p, pre_x, x2) = hoist_expr(p, line, x)
          let #(p, pre_y, y2) = hoist_expr(p, line, y)
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
            p,
            list.append(pre_c, [ast.StmtWithLine(line:, statement: branch)]),
            ident(span, t),
          )
        }
      }
    ast.UnaryExpression(span, op, arg) -> {
      let #(p, pre, arg2) = hoist_expr(p, line, arg)
      #(p, pre, ast.UnaryExpression(span, op, arg2))
    }
    ast.UpdateExpression(span, op, prefix, arg) ->
      case arg {
        ast.MemberExpression(mspan, obj, prop) -> {
          let #(p, pre, obj2, prop2) = hoist_member(p, line, obj, prop, False)
          #(
            p,
            pre,
            ast.UpdateExpression(
              span,
              op,
              prefix,
              ast.MemberExpression(mspan, obj2, prop2),
            ),
          )
        }
        _ -> #(p, [], ex)
      }
    ast.AssignmentExpression(span, op, lhs, rhs) ->
      hoist_assign(p, line, span, op, lhs, rhs)
    ast.CallExpression(span, callee, args) -> {
      let #(p, pre_c, callee2) = hoist_callee(p, line, callee, args)
      let #(p, pre_a, args2) = hoist_list(p, line, args)
      #(p, list.append(pre_c, pre_a), ast.CallExpression(span, callee2, args2))
    }
    ast.NewExpression(span, callee, args) -> {
      let #(p, pre_c, callee2) = hoist_expr(p, line, callee)
      let #(p, pre_p, callee3) = case list.any(args, expr_has_split) {
        True -> spill_to_temp(p, line, callee2)
        False -> #(p, [], callee2)
      }
      let #(p, pre_a, args2) = hoist_list(p, line, args)
      #(
        p,
        list.flatten([pre_c, pre_p, pre_a]),
        ast.NewExpression(span, callee3, args2),
      )
    }
    ast.MemberExpression(span, obj, prop) -> {
      let #(p, pre, obj2, prop2) = hoist_member(p, line, obj, prop, False)
      #(p, pre, ast.MemberExpression(span, obj2, prop2))
    }
    ast.ArrayExpression(span, elems) -> {
      let present = list.filter_map(elems, option.to_result(_, Nil))
      let #(p, pre, xs) = hoist_list(p, line, present)
      let #(_, elems2) =
        list.map_fold(elems, xs, fn(rest, el) {
          case el, rest {
            None, _ -> #(rest, None)
            Some(_), [x, ..more] -> #(more, Some(x))
            Some(orig), [] -> #([], Some(orig))
          }
        })
      #(p, pre, ast.ArrayExpression(span, elems2))
    }
    ast.ObjectExpression(span, props) -> {
      let items =
        list.flat_map(props, fn(prop) {
          case prop {
            ast.InitProperty(key: ast.KeyComputed(k), value: v, ..) -> [k, v]
            ast.InitProperty(value: v, ..) -> [v]
            ast.MethodProperty(key: ast.KeyComputed(k), ..)
            | ast.AccessorProperty(key: ast.KeyComputed(k), ..) -> [k]
            ast.MethodProperty(..) | ast.AccessorProperty(..) -> []
            ast.SpreadProperty(argument: arg) -> [arg]
          }
        })
      let #(p, pre, xs) = hoist_list(p, line, items)
      let #(_, props2) =
        list.map_fold(props, xs, fn(rest, prop) {
          case prop, rest {
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
            _, _ -> #(rest, prop)
          }
        })
      #(p, pre, ast.ObjectExpression(span, props2))
    }
    ast.SequenceExpression(span, parts) -> {
      let #(p, pre, parts2) = hoist_list(p, line, parts)
      #(p, pre, ast.SequenceExpression(span, parts2))
    }
    ast.TemplateLiteral(span, parts) -> {
      let #(p, pre, exprs2) =
        hoist_list(p, line, ast.template_expressions(parts))
      #(p, pre, ast.TemplateLiteral(span, rebuild_template(parts, exprs2)))
    }
    ast.TaggedTemplateExpression(span, tag, parts) -> {
      let exprs = ast.template_expressions(parts)
      let #(p, pre_t, tag2) = hoist_callee(p, line, tag, exprs)
      let #(p, pre_e, exprs2) = hoist_list(p, line, exprs)
      #(
        p,
        list.append(pre_t, pre_e),
        ast.TaggedTemplateExpression(
          span,
          tag2,
          rebuild_template(parts, exprs2),
        ),
      )
    }
    ast.ClassExpression(span, name, super_class, body) -> {
      let #(p, pre, super2, body2) = hoist_class(p, line, super_class, body)
      #(p, pre, ast.ClassExpression(span, name, super2, body2))
    }
    _ -> #(p, [], ex)
  }
}

fn hoist_class(
  p: SplitPlanner,
  line: Int,
  super_class: Option(ast.Expression),
  body: List(ast.ClassElement),
) -> #(
  SplitPlanner,
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
  let #(p, pre, xs) = hoist_list(p, line, items)
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
  #(p, pre, super2, body2)
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

fn hoist_list(
  p: SplitPlanner,
  line: Int,
  xs: List(ast.Expression),
) -> #(SplitPlanner, List(ast.StmtWithLine), List(ast.Expression)) {
  let last_split =
    list.index_fold(xs, -1, fn(acc, x, i) {
      case expr_has_split(x) {
        True -> i
        False -> acc
      }
    })
  let #(#(p, pre_rev), xs2) =
    list.index_map(xs, fn(x, i) { #(x, i) })
    |> list.map_fold(#(p, []), fn(st, xi) {
      let #(p, pre_rev) = st
      let #(x, i) = xi
      case i < last_split, i == last_split {
        True, _ -> {
          let #(p, pre1, x2) = hoist_expr(p, line, x)
          let #(p, pre2, x3) = spill_to_temp(p, line, x2)
          #(#(p, [pre2, pre1, ..pre_rev]), x3)
        }
        _, True -> {
          let #(p, pre1, x2) = hoist_expr(p, line, x)
          #(#(p, [pre1, ..pre_rev]), x2)
        }
        _, _ -> #(st, x)
      }
    })
  #(p, list.flatten(list.reverse(pre_rev)), xs2)
}

fn hoist_member(
  p: SplitPlanner,
  line: Int,
  obj: ast.Expression,
  prop: ast.MemberProperty,
  later: Bool,
) -> #(SplitPlanner, List(ast.StmtWithLine), ast.Expression, ast.MemberProperty) {
  case obj {
    ast.SuperExpression(..) -> {
      let #(p, pre, prop2) = hoist_prop(p, line, prop, later)
      #(p, pre, obj, prop2)
    }
    _ -> {
      let #(p, pre_o, obj2) = hoist_expr(p, line, obj)
      let #(p, pre_p, obj3) = case later || member_prop_has_split(prop) {
        True -> spill_to_temp(p, line, obj2)
        False -> #(p, [], obj2)
      }
      let #(p, pre_k, prop2) = hoist_prop(p, line, prop, later)
      #(p, list.flatten([pre_o, pre_p, pre_k]), obj3, prop2)
    }
  }
}

fn hoist_prop(
  p: SplitPlanner,
  line: Int,
  prop: ast.MemberProperty,
  later: Bool,
) -> #(SplitPlanner, List(ast.StmtWithLine), ast.MemberProperty) {
  case prop {
    ast.Dot(..) -> #(p, [], prop)
    ast.Bracket(k) -> {
      let #(p, pre_k, k2) = hoist_expr(p, line, k)
      let #(p, pre_p, k3) = case later {
        True -> spill_to_temp(p, line, k2)
        False -> #(p, [], k2)
      }
      #(p, list.append(pre_k, pre_p), ast.Bracket(k3))
    }
  }
}

fn hoist_callee(
  p: SplitPlanner,
  line: Int,
  callee: ast.Expression,
  args: List(ast.Expression),
) -> HoistedExpr {
  let later = list.any(args, expr_has_split)
  case callee {
    ast.MemberExpression(span, obj, prop) -> {
      let #(p, pre, obj2, prop2) = hoist_member(p, line, obj, prop, later)
      #(p, pre, ast.MemberExpression(span, obj2, prop2))
    }
    ast.ParenthesizedExpression(_, inner) -> hoist_callee(p, line, inner, args)
    _ -> {
      let #(p, pre_c, callee2) = hoist_expr(p, line, callee)
      case later {
        True -> {
          let #(p, pre_p, callee3) = spill_to_temp(p, line, callee2)
          #(p, list.append(pre_c, pre_p), callee3)
        }
        False -> #(p, pre_c, callee2)
      }
    }
  }
}

fn hoist_assign(
  p: SplitPlanner,
  line: Int,
  span: ast.Span,
  op: ast.AssignmentOp,
  lhs: ast.Expression,
  rhs: ast.Expression,
) -> HoistedExpr {
  let target = case lhs {
    ast.Identifier(..) -> Some(#(p, [], lhs))
    ast.MemberExpression(mspan, obj, prop) ->
      case obj {
        ast.SuperExpression(..) -> None
        _ -> {
          let #(p, pre, obj2, prop2) = hoist_member(p, line, obj, prop, True)
          Some(#(p, pre, ast.MemberExpression(mspan, obj2, prop2)))
        }
      }
    _ -> None
  }
  case target, op, expr.compound_binop(op), expr.logical_assign_op(op) {
    None, _, _, _ ->
      case expr_has_split(lhs) {
        True -> #(p, [], ast.AssignmentExpression(span, op, lhs, rhs))
        False -> {
          let #(p, pre, rhs2) = hoist_expr(p, line, rhs)
          #(p, pre, ast.AssignmentExpression(span, op, lhs, rhs2))
        }
      }
    Some(#(p, pre_t, ref)), ast.Assign, _, _ -> {
      let #(p, pre_r, rhs2) = hoist_expr(p, line, rhs)
      #(
        p,
        list.append(pre_t, pre_r),
        ast.AssignmentExpression(span, ast.Assign, ref, rhs2),
      )
    }
    Some(#(p, pre_t, ref)), _, Some(bop), _ -> {
      let #(t, p) = fresh_temp(p)
      let #(p, pre_r, rhs2) = hoist_expr(p, line, rhs)
      #(
        p,
        list.flatten([pre_t, [assign_stmt(line, span, t, ref)], pre_r]),
        ast.AssignmentExpression(
          span,
          ast.Assign,
          ref,
          ast.BinaryExpression(span, bop, ident(span, t), rhs2),
        ),
      )
    }
    Some(#(p, pre_t, ref)), _, _, Some(lop) -> {
      let #(t, p) = fresh_temp(p)
      let #(p, pre_r, rhs2) = hoist_expr(p, line, rhs)
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
        p,
        list.flatten([
          pre_t,
          [assign_stmt(line, span, t, ref)],
          [ast.StmtWithLine(line:, statement: guard)],
        ]),
        ident(span, t),
      )
    }
    Some(#(p, _, _)), _, _, _ -> #(
      p,
      [],
      ast.AssignmentExpression(span, op, lhs, rhs),
    )
  }
}

fn explode_stmt(
  p: SplitPlanner,
  sl: ast.StmtWithLine,
) -> Option(#(SplitPlanner, List(ast.StmtWithLine))) {
  let ast.StmtWithLine(line:, statement: s) = sl
  let done = fn(p, pre, stmt) {
    // unchanged rewrite must be None or the planner loops forever
    case pre, stmt == s {
      [], True -> None
      _, _ ->
        Some(#(p, list.append(pre, [ast.StmtWithLine(line:, statement: stmt)])))
    }
  }
  case s {
    ast.ExpressionStatement(expression: ex, directive: dir) ->
      case ex {
        ast.SequenceExpression(_, parts) ->
          Some(#(p, list.map(parts, expr_stmt(line, _))))
        ast.AssignmentExpression(
          span,
          ast.Assign,
          ast.Identifier(..) as lhs,
          rhs,
        ) ->
          case needs_explode(rhs) {
            False -> None
            True -> {
              let #(p, pre, rhs2) = hoist_keeping_top_split(p, line, rhs)
              done(
                p,
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
              let #(p, pre, ex2) = hoist_keeping_top_split(p, line, ex)
              done(p, pre, ast.ExpressionStatement(ex2, dir))
            }
          }
      }
    ast.ReturnStatement(Some(ex)) ->
      case needs_explode(ex) {
        False -> None
        True -> {
          let #(p, pre, ex2) = hoist_keeping_top_split(p, line, ex)
          done(p, pre, ast.ReturnStatement(Some(ex2)))
        }
      }
    ast.ThrowStatement(ex) ->
      case needs_explode(ex) {
        False -> None
        True -> {
          let #(p, pre, ex2) = hoist_keeping_top_split(p, line, ex)
          done(p, pre, ast.ThrowStatement(ex2))
        }
      }
    ast.VariableDeclaration(kind, [ast.VariableDeclarator(pat, Some(init))]) ->
      case pattern_has_split(pat) || !needs_explode(init) {
        True -> None
        False -> {
          let #(p, pre, init2) = hoist_keeping_top_split(p, line, init)
          done(
            p,
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
            p,
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
          let #(p, pre, c2) = hoist_keeping_top_split(p, line, c)
          done(p, pre, ast.IfStatement(c2, t, f))
        }
      }
    ast.WhileStatement(condition: c, body: b) ->
      case needs_explode(c) {
        False -> None
        True -> {
          let #(p, pre, c2) = hoist_expr(p, line, c)
          done(p, [], loop_with_test(line, c2, pre, b))
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
              Some(#(p, [expr_stmt(line, e)]))
            Some(ast.ForInitDeclaration(kind: ast.Var, declarations: ds)), True
            ->
              Some(#(
                p,
                list.map(ds, fn(d) {
                  ast.StmtWithLine(
                    line:,
                    statement: ast.VariableDeclaration(ast.Var, [d]),
                  )
                }),
              ))
            _, True -> None
            _, False -> Some(#(p, []))
          }
          case hoisted {
            None -> None
            Some(#(p, pre_i)) -> {
              let init2 = case init_split {
                True -> None
                False -> i
              }
              case c, cond_split {
                Some(ce), True -> {
                  let #(p, pre_c, c2) = hoist_expr(p, line, ce)
                  done(
                    p,
                    pre_i,
                    ast.ForStatement(
                      init2,
                      None,
                      u,
                      head_test_block(line, c2, pre_c, b),
                    ),
                  )
                }
                _, _ -> done(p, pre_i, ast.ForStatement(init2, c, u, b))
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
          let #(p, pre, r2) = hoist_expr(p, line, r)
          done(p, pre, ast.ForOfStatement(l, r2, b, aw))
        }
      }
    ast.ForInStatement(left: l, right: r, body: b) ->
      case expr_has_split(r) && !for_init_has_split(l) {
        False -> None
        True -> {
          let #(p, pre, r2) = hoist_expr(p, line, r)
          done(p, pre, ast.ForInStatement(l, r2, b))
        }
      }
    ast.SwitchStatement(discriminant: discriminant, cases: cases) ->
      case needs_explode(discriminant) {
        False -> None
        True -> {
          let #(p, pre, discriminant2) =
            hoist_keeping_top_split(p, line, discriminant)
          done(p, pre, ast.SwitchStatement(discriminant2, cases))
        }
      }
    ast.ClassDeclaration(name:, super_class: sc, body: b) -> {
      let #(p, pre, sc2, b2) = hoist_class(p, line, sc, b)
      done(p, pre, ast.ClassDeclaration(name, sc2, b2))
    }
    ast.LabeledStatement(label:, body: b) ->
      case explode_stmt(p, ast.StmtWithLine(line:, statement: b)) {
        None -> None
        Some(#(p, stmts)) ->
          case list.reverse(stmts) {
            [last, ..rest_rev] ->
              Some(#(
                p,
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
    ast.BooleanLiteral(cond.span, True),
    head_test_block(line, cond, pre, body),
  )
}

fn head_test_block(
  line: Int,
  cond: ast.Expression,
  pre: List(ast.StmtWithLine),
  body: ast.Statement,
) -> ast.Statement {
  let span = cond.span
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
