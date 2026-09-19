import arc/bytecode/binop.{AddOp, PureOp}
import arc/bytecode/error_kind
import arc/bytecode/key
import arc/bytecode/lexical
import arc/bytecode/opcode.{
  type IrOp, type LabelId, CatchOnly, Finally, IrAsyncYieldStarNext,
  IrAsyncYieldStarResume, IrBinOp, IrDefineAccessor, IrDefineField,
  IrDefineMethod, IrDeleteField, IrFinal, IrGetField, IrGetFieldKeep, IrGosub,
  IrJump, IrJumpIfFalse, IrJumpIfNotNullish, IrJumpIfNullish, IrJumpIfTrue,
  IrLabel, IrPushTry, IrPutField, IterCloseGuard,
}
import arc/compiler/ast_util
import arc/compiler/const_fold
import arc/compiler/scope.{
  type BindingKind, type GlobalFallthrough, type ScopeId, type TopLevelLex,
  CaptureBinding, CatchBinding, ConstBinding, FnNameBinding, GlobalLexical,
  LetBinding, LocalLexical, ParamBinding, ToEvalEnv, ToGlobal, VarBinding,
  root_scope_id,
}
import arc/module/summary
import arc/parser/ast
import arc/rt/types.{
  type JsVal, mk_bigint, mk_bool, mk_int, mk_null, mk_string, mk_tdz,
  mk_undefined,
}
import arc/rt/val as rt_val
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}

pub type CompiledChild {
  CompiledChild(
    scope_id: ScopeId,
    name: Option(String),
    arity: Int,
    length: Int,
    code: List(IrOp),
    constants: List(JsVal),
    functions: List(CompiledChild),
    is_strict: Bool,
    is_arrow: Bool,
    is_derived_constructor: Bool,
    is_generator: Bool,
    is_async: Bool,
    is_constructor: Bool,
    is_class_constructor: Bool,
    lexical_refs: lexical.LexicalRefs,
    references_arguments: Bool,
    code_kind: lexical.CodeKind,
  )
}

pub type EmitOutput {
  EmitOutput(
    code: List(IrOp),
    constants: List(JsVal),
    children: List(CompiledChild),
    is_strict: Bool,
    // must replace the caller's tree: scratch slots bumped local_count
    tree: scope.ScopeTree,
    module_hoisted_funcs: List(#(String, Int)),
  )
}

type Frame {
  LoopFrame(
    break_target: LabelId,
    continue_target: LabelId,
    label: Option(String),
    // crossing a for-of loop pops its try and closes the iterator
    iterator: LoopIter,
  )
  SwitchFrame(break_target: LabelId, label: Option(String))
  LabeledBlockFrame(break_target: LabelId, label: String)
  // never a target; crossing emits pop_try, finally gosub, drops
  BarrierFrame(pop_try: Int, label_finally: Option(LabelId), drop_count: Int)
}

type LoopIter {
  NoIter
  SyncIter
  AsyncIter
}

type Emitter {
  Emitter(
    code: List(IrOp),
    constants_map: Dict(JsVal, Int),
    constants_list: List(JsVal),
    next_const: Int,
    next_label: Int,
    frame_stack: List(Frame),
    functions: List(CompiledChild),
    next_func: Int,
    pending_label: Option(String),
    strict: Bool,
    is_async: Bool,
    is_arrow: Bool,
    lexical_refs: lexical.LexicalRefs,
    references_arguments: Bool,
    // false while arguments only appears as f.apply(t, arguments)
    arguments_escape: Bool,
    code_kind: lexical.CodeKind,
    // GlobalLexical only for the repl program emitter
    top_lex: TopLevelLex,
    scope_tree: scope.ScopeTree,
    // scope-chain walks stop here, never reading the parent frame
    fn_scope: ScopeId,
    current_scope: ScopeId,
    // unentered child scope ids of current_scope, source order
    scope_cursor: List(ScopeId),
    field_init: FieldInitMode,
    // true directly inside a block, where function decls are annex b candidates
    in_block: Bool,
    // true only for eval units (§19.2.1.3 passes D = true)
    deletable_global_vars: Bool,
    // synthesized derived default ctor: its super(...args) must not iterate
    in_implicit_derived_ctor: Bool,
    // non-empty only while emitting parameter initializers
    param_scope_names: List(String),
    // innermost first, includes withs from enclosing functions
    with_stack: List(String),
    private_env: List(String),
    // scratch slot tracking the §14 completion value in tail position
    completion_var: Option(Int),
    free_ref_slots: List(Int),
    // let slots already initialized, others get tdz-checked stores
    initialized: Set(Int),
    // 0 when unknown, reset at every label
    line: Int,
    // child function scope ids in emission order, popped by compile_function_body
    child_fn_cursor: List(ScopeId),
    next_site: Int,
  )
}

type FieldInitMode {
  NoFieldInit
  FieldInitAtStart
  FieldInitAfterSuper
}

// §7.3.32 fields define own properties, never invoke setters
type FieldInit {
  PrivateMethodInit(name: String, closure_const: String, kind: ast.MethodKind)
  PrivateFieldInit(name: String, init: ast.Expression)
  NamedFieldInit(name: String, init: ast.Expression)
  NumericFieldInit(value: ast.LiteralNumber, init: ast.Expression)
  // key evaluated once at class definition into key_const
  ComputedFieldInit(key_const: String, init: ast.Expression)
  BigIntFieldInit(value: Int, init: ast.Expression)
  // static block lowered to an arrow iife for its own var env
  StaticBlockInit(body: List(ast.StmtWithLine))
}

type FnBody {
  StmtsBody(stmts: List(ast.StmtWithLine))
  FieldInitsBody(inits: List(FieldInit))
}

const class_fields_init = ast_util.class_fields_init

// all but the two string variants are engine bugs, not user errors
pub type EmitError {
  BreakOutsideLoop
  ContinueOutsideLoop
  EarlySyntaxError(message: String)
  UnsupportedFeature(feature: String)
  NonMemberLValue
  AnonymousClassDeclaration
  NonCompoundAssignOperator
  MultiDeclaratorForHead
  AccessorInDestructuringPattern
  NonMemberDefaultTarget
  BareSuperExpression
  BareSpreadElement
  InvalidUpdateTarget
  InvalidCompoundAssignTarget
  NonGenericUnaryOperator
}

pub fn program(
  stmts: List(ast.StmtWithLine),
  tree: scope.ScopeTree,
  deletable_global_vars deletable_global_vars: Bool,
) -> Result(EmitOutput, EmitError) {
  let script_strict = ast_util.has_use_strict_directive(stmts)
  let e =
    Emitter(
      ..new_emitter(tree, root_scope_id),
      strict: script_strict,
      deletable_global_vars:,
    )
  emit_top_level_body(e, stmts, script_strict:, vars_to_global: True)
}

// §19.2.1.1 direct eval: caller strictness, params and private env are config
pub fn eval_direct(
  stmts: List(ast.StmtWithLine),
  tree: scope.ScopeTree,
  caller_is_strict caller_is_strict: Bool,
  inherit_param_scope inherit_param_scope: List(String),
  inherit_private_env inherit_private_env: List(String),
) -> Result(EmitOutput, EmitError) {
  let script_strict =
    caller_is_strict || ast_util.has_use_strict_directive(stmts)
  let e =
    Emitter(
      ..new_emitter(tree, root_scope_id),
      strict: script_strict,
      param_scope_names: case script_strict {
        True -> []
        False -> inherit_param_scope
      },
      private_env: inherit_private_env,
      deletable_global_vars: True,
    )
  // no this prologue: direct eval uses the caller's boxed slots
  emit_top_level_body(e, stmts, script_strict:, vars_to_global: !script_strict)
}

pub fn module(
  items: List(ast.ModuleItem),
  tree: scope.ScopeTree,
) -> Result(EmitOutput, EmitError) {
  let stmts = ast_util.module_items_to_stmts(items)
  let has_module_using = ast_util.has_using_decl(stmts)
  let e = Emitter(..new_emitter(tree, root_scope_id), strict: True)
  let e = enter_root_scope(e)

  use #(hoisted_funcs, e) <- result.try(collect_hoisted_funcs(e, stmts))
  let e = emit_hoisted_funcs(e, hoisted_funcs)

  use e <- result.try(case has_module_using {
    False -> emit_stmts_tail(e, stmts)
    True -> emit_module_using_top(e, stmts)
  })

  let Finished(code:, constants:, children:) = finish(e)
  Ok(EmitOutput(
    code:,
    constants:,
    children:,
    is_strict: True,
    tree: e.scope_tree,
    module_hoisted_funcs: hoisted_funcs,
  ))
}

// every field but disposers and has_async is a scratch slot index
type UsingSlots {
  UsingSlots(
    error_slot: Int,
    has_error_slot: Int,
    disposers: List(Disposer),
    has_async: Bool,
    needs_await_slot: Int,
    has_awaited_slot: Int,
    call_result_slot: Int,
    call_returned_slot: Int,
  )
}

type Disposer {
  Disposer(slot: Int, is_async: Bool)
}

type UsingResource {
  UsingResource(
    line: Int,
    name: String,
    init: ast.Expression,
    disposer: Disposer,
  )
}

type UsingItem {
  PlainItem(stmt: ast.StmtWithLine)
  ResourceItem(resource: UsingResource)
}

// fresh_slot names so using scopes sharing a lexical scope never collide
fn build_using_slots(
  e: Emitter,
  stmts: List(ast.StmtWithLine),
) -> #(UsingSlots, List(UsingItem), Emitter) {
  let #(items_rev, e) =
    list.fold(stmts, #([], e), fn(acc, located) {
      let #(items, e) = acc
      case located.statement {
        ast.VariableDeclaration(kind: ast.Using, declarations:) ->
          lower_using_declarators(
            e,
            items,
            located.line,
            declarations,
            is_async: False,
          )
        ast.VariableDeclaration(kind: ast.AwaitUsing, declarations:) ->
          lower_using_declarators(
            e,
            items,
            located.line,
            declarations,
            is_async: True,
          )
        _ -> #([PlainItem(located), ..items], e)
      }
    })
  let items = list.reverse(items_rev)
  let disposers =
    list.filter_map(items, fn(item) {
      case item {
        ResourceItem(resource:) -> Ok(resource.disposer)
        PlainItem(_) -> Error(Nil)
      }
    })
  let #(slots, e) = make_using_slots(e, disposers)
  #(slots, items, e)
}

fn lower_using_declarators(
  e: Emitter,
  items: List(UsingItem),
  line: Int,
  declarations: List(ast.VariableDeclarator),
  is_async is_async: Bool,
) -> #(List(UsingItem), Emitter) {
  list.fold(declarations, #(items, e), fn(acc, decl) {
    let #(items, e) = acc
    let assert ast.IdentifierPattern(name, ..) = decl.id
    let assert Some(init) = decl.init
    let #(slot, e) = fresh_slot(e)
    let resource =
      UsingResource(line:, name:, init:, disposer: Disposer(slot:, is_async:))
    #([ResourceItem(resource), ..items], e)
  })
}

fn make_using_slots(
  e: Emitter,
  disposers: List(Disposer),
) -> #(UsingSlots, Emitter) {
  let has_async = list.any(disposers, fn(d) { d.is_async })
  let #(error_slot, e) = fresh_slot(e)
  let #(has_error_slot, e) = fresh_slot(e)
  let #(needs_await_slot, e) = fresh_slot(e)
  let #(has_awaited_slot, e) = fresh_slot(e)
  let #(call_result_slot, e) = fresh_slot(e)
  let #(call_returned_slot, e) = fresh_slot(e)
  #(
    UsingSlots(
      error_slot:,
      has_error_slot:,
      disposers:,
      has_async:,
      needs_await_slot:,
      has_awaited_slot:,
      call_result_slot:,
      call_returned_slot:,
    ),
    e,
  )
}

fn emit_using_prelude(e: Emitter, slots: UsingSlots) -> Emitter {
  let e = emit_store_const(e, slots.error_slot, mk_undefined())
  let e = emit_store_const(e, slots.has_error_slot, mk_bool(False))
  let e =
    list.fold(slots.disposers, e, fn(e, d) {
      emit_store_const(e, d.slot, mk_null())
    })
  case slots.has_async {
    False -> e
    True ->
      e
      |> emit_store_const(slots.needs_await_slot, mk_bool(False))
      |> emit_store_const(slots.has_awaited_slot, mk_bool(False))
      |> emit_store_const(slots.call_result_slot, mk_undefined())
      |> emit_store_const(slots.call_returned_slot, mk_bool(False))
  }
}

// each disposer registers right after its init; one IrLine per statement
fn emit_using_body(
  e: Emitter,
  items: List(UsingItem),
) -> Result(Emitter, EmitError) {
  use #(_last_line, e) <- result.map(
    list.try_fold(items, #(0, e), fn(acc, item) {
      let #(last_line, e) = acc
      case item {
        ResourceItem(resource:) -> {
          let e = case resource.line == last_line {
            True -> e
            False -> set_line(e, resource.line)
          }
          use e <- result.map(emit_using_resource(e, resource))
          #(resource.line, e)
        }
        PlainItem(located) -> {
          use e <- result.map(emit_stmt(
            set_line(e, located.line),
            located.statement,
          ))
          #(0, e)
        }
      }
    }),
  )
  e
}

fn emit_using_resource(
  e: Emitter,
  resource: UsingResource,
) -> Result(Emitter, EmitError) {
  let UsingResource(line: _, name:, init:, disposer:) = resource
  let e = declare_lex(e, name, is_const: True)
  use e <- result.map(emit_named_expr(e, init, name))
  e
  |> init_lex(name)
  |> emit_var_get(name)
  |> emit_op(opcode.GetDisposer(disposer.is_async))
  |> emit_scratch_put(disposer.slot)
}

// [thrown, ..] -> [..]: err = has_err ? SuppressedError(thrown, err) : thrown
fn emit_using_merge_error(e: Emitter, slots: UsingSlots) -> Emitter {
  let #(skip, e) = fresh_label(e)
  e
  |> emit_scratch_get(slots.has_error_slot)
  |> emit_ir(IrJumpIfFalse(skip))
  // [err, thrown, ..] -> [SuppressedError, ..]
  |> emit_scratch_get(slots.error_slot)
  |> emit_op(opcode.MakeSuppressed)
  |> emit_ir(IrLabel(skip))
  |> emit_scratch_put(slots.error_slot)
  |> push_const(mk_bool(True))
  |> emit_scratch_put(slots.has_error_slot)
}

fn emit_using_try_merge(
  e: Emitter,
  slots: UsingSlots,
  body: fn(Emitter) -> Emitter,
) -> Emitter {
  let #(catch_label, e) = fresh_label(e)
  let #(end_label, e) = fresh_label(e)
  e
  |> emit_ir(IrPushTry(catch_label, CatchOnly))
  |> body
  |> emit_op(opcode.PopTry)
  |> emit_ir(IrJump(end_label))
  |> emit_ir(IrLabel(catch_label))
  |> emit_using_merge_error(slots)
  |> emit_ir(IrLabel(end_label))
}

// await undefined once if needs_await and not yet awaited
fn emit_using_flush_pending(
  e: Emitter,
  slots: UsingSlots,
  reset reset: Bool,
) -> Emitter {
  let #(skip, e) = fresh_label(e)
  let e =
    e
    |> emit_scratch_get(slots.needs_await_slot)
    |> emit_ir(IrJumpIfFalse(skip))
    |> emit_scratch_get(slots.has_awaited_slot)
    |> emit_ir(IrJumpIfTrue(skip))
    |> push_const(mk_undefined())
    |> emit_op(opcode.Await)
    |> emit_op(opcode.Pop)
  let e = case reset {
    False -> e
    True ->
      e
      |> push_const(mk_bool(False))
      |> emit_scratch_put(slots.needs_await_slot)
  }
  emit_ir(e, IrLabel(skip))
}

// slot !== constant -> jump if false, stack-neutral
fn emit_jump_unless_strict_neq(
  e: Emitter,
  slot: Int,
  constant: JsVal,
  target: LabelId,
) -> Emitter {
  e
  |> emit_scratch_get(slot)
  |> push_const(constant)
  |> emit_ir(IrBinOp(PureOp(binop.Equality(binop.StrictNotEq))))
  |> emit_ir(IrJumpIfFalse(target))
}

// if d != null: flush pending await, then try d() catch merge
fn emit_using_dispose_sync(
  e: Emitter,
  slots: UsingSlots,
  slot: Int,
) -> Emitter {
  let #(skip, e) = fresh_label(e)
  let e = emit_jump_unless_strict_neq(e, slot, mk_null(), skip)
  let e = emit_jump_unless_strict_neq(e, slot, mk_undefined(), skip)
  let e = case slots.has_async {
    False -> e
    True -> emit_using_flush_pending(e, slots, reset: True)
  }
  let e = {
    use e <- emit_using_try_merge(e, slots)
    e
    |> emit_scratch_get(slot)
    |> emit_op(opcode.Call(0))
    |> emit_op(opcode.Pop)
  }
  emit_ir(e, IrLabel(skip))
}

// null: skip; undefined: needs_await = true; else call, await, merge errors
fn emit_using_dispose_async(
  e: Emitter,
  slots: UsingSlots,
  slot: Int,
) -> Emitter {
  let #(skip, e) = fresh_label(e)
  let #(no_method, e) = fresh_label(e)
  let #(after_await, e) = fresh_label(e)
  let e = emit_jump_unless_strict_neq(e, slot, mk_null(), skip)
  let e = emit_jump_unless_strict_neq(e, slot, mk_undefined(), no_method)
  let e =
    e
    |> push_const(mk_bool(False))
    |> emit_scratch_put(slots.call_returned_slot)
  let e = {
    use e <- emit_using_try_merge(e, slots)
    e
    |> emit_scratch_get(slot)
    |> emit_op(opcode.Call(0))
    |> emit_scratch_put(slots.call_result_slot)
    |> push_const(mk_bool(True))
    |> emit_scratch_put(slots.call_returned_slot)
  }
  let e =
    e
    |> emit_scratch_get(slots.call_returned_slot)
    |> emit_ir(IrJumpIfFalse(after_await))
  let e = {
    use e <- emit_using_try_merge(e, slots)
    e
    |> emit_scratch_get(slots.call_result_slot)
    |> emit_op(opcode.Await)
    |> emit_op(opcode.Pop)
  }
  e
  |> push_const(mk_bool(True))
  |> emit_scratch_put(slots.has_awaited_slot)
  |> emit_ir(IrLabel(after_await))
  |> emit_ir(IrJump(skip))
  |> emit_ir(IrLabel(no_method))
  |> push_const(mk_bool(True))
  |> emit_scratch_put(slots.needs_await_slot)
  |> emit_ir(IrLabel(skip))
}

// dispose in reverse order, trailing await, then rethrow if has_err
fn emit_using_dispose(e: Emitter, slots: UsingSlots) -> Emitter {
  let e =
    list.fold(list.reverse(slots.disposers), e, fn(e, d) {
      case d.is_async {
        True -> emit_using_dispose_async(e, slots, d.slot)
        False -> emit_using_dispose_sync(e, slots, d.slot)
      }
    })
  let e = case slots.has_async {
    False -> e
    True -> emit_using_flush_pending(e, slots, reset: False)
  }
  let #(end, e) = fresh_label(e)
  e
  |> emit_scratch_get(slots.has_error_slot)
  |> emit_ir(IrJumpIfFalse(end))
  |> emit_scratch_get(slots.error_slot)
  |> emit_op(opcode.Throw)
  |> emit_ir(IrLabel(end))
}

// same scaffold as emit_try_catch_finally so jumps run disposal via gosub
fn emit_using_try_wrap(
  e: Emitter,
  slots: UsingSlots,
  emit_body: fn(Emitter) -> Result(Emitter, EmitError),
) -> Result(Emitter, EmitError) {
  use e, catch_label, _fin <- emit_try_catch_finally(e, emit_body, fn(e) {
    Ok(emit_using_dispose(e, slots))
  })
  Ok(emit_using_record_error(emit_ir(e, IrLabel(catch_label)), slots))
}

// [thrown, ..] -> [..]
fn emit_using_record_error(e: Emitter, slots: UsingSlots) -> Emitter {
  e
  |> emit_scratch_put(slots.error_slot)
  |> push_const(mk_bool(True))
  |> emit_scratch_put(slots.has_error_slot)
}

fn single_using_slots(
  e: Emitter,
  is_async is_async: Bool,
) -> #(UsingSlots, Int, Emitter) {
  let #(slot, e) = fresh_slot(e)
  let #(slots, e) = make_using_slots(e, [Disposer(slot:, is_async:)])
  #(slots, slot, e)
}

type ForOfUsing {
  ForOfUsing(name: String, is_async: Bool)
}

fn for_of_using_binding(left: ast.ForInit) -> Option(ForOfUsing) {
  case left {
    ast.ForInitDeclaration(
      kind:,
      declarations: [ast.VariableDeclarator(ast.IdentifierPattern(name, ..), _)],
    ) ->
      case kind {
        ast.Using -> Some(ForOfUsing(name:, is_async: False))
        ast.AwaitUsing -> Some(ForOfUsing(name:, is_async: True))
        ast.Var | ast.Let | ast.Const -> None
      }
    _ -> None
  }
}

// one-resource scope per iteration, disposal inside the for-of's F_body
fn emit_for_of_using_body(
  e: Emitter,
  binding: ForOfUsing,
  body: ast.Statement,
) -> Result(Emitter, EmitError) {
  let ForOfUsing(name:, is_async:) = binding
  let #(slots, slot, e) = single_using_slots(e, is_async)
  let e = emit_using_prelude(e, slots)
  use e <- emit_using_try_wrap(e, slots)
  let e = emit_var_get(e, name)
  let e = emit_op(e, opcode.GetDisposer(is_async))
  let e = emit_scratch_put(e, slot)
  emit_stmt(e, body)
}

// acquired once before the loop, disposed once after
fn emit_for_using_classic(
  e: Emitter,
  kind: ast.VariableKind,
  declarations: List(ast.VariableDeclarator),
  condition: Option(ast.Expression),
  update: Option(ast.Expression),
  body: ast.Statement,
) -> Result(Emitter, EmitError) {
  use e <- in_child_scope(e, in_block: e.in_block)
  // line 0: the enclosing emit_stmt already emitted the IrLine
  let head = [
    ast.StmtWithLine(0, ast.VariableDeclaration(kind:, declarations:)),
  ]
  let #(slots, items, e) = build_using_slots(e, head)
  let e = emit_using_prelude(e, slots)
  use e <- emit_using_try_wrap(e, slots)
  use e <- result.try(emit_using_body(e, items))
  emit_classic_loop(e, condition, update, body, [])
}

fn emit_classic_loop(
  e: Emitter,
  condition: Option(ast.Expression),
  update: Option(ast.Expression),
  body: ast.Statement,
  per_iter: List(String),
) -> Result(Emitter, EmitError) {
  let #(loop_start, e) = fresh_label(e)
  let #(loop_test, e) = fresh_label(e)
  let #(loop_continue, e) = fresh_label(e)
  let #(loop_end, e) = fresh_label(e)
  let e = push_loop(e, loop_end, loop_continue, NoIter)
  // bottom-tested loop; test duplicated as entry guard when cheap
  use e <- result.try(case condition {
    Some(cond) ->
      case safe_to_emit_twice(cond) {
        True -> emit_test(e, cond, jump_when: False, to: loop_end)
        False -> Ok(emit_ir(e, IrJump(loop_test)))
      }
    None -> Ok(e)
  })
  let e = emit_ir(e, IrLabel(loop_start))
  use e <- result.try(emit_stmt(e, body))
  let e = emit_ir(e, IrLabel(loop_continue))
  let e = list.fold(per_iter, e, emit_var_rebox)
  use e <- result.try(case update {
    Some(upd) -> {
      use e <- result.map(emit_expr(e, for_effect(upd)))
      emit_op(e, opcode.Pop)
    }
    None -> Ok(e)
  })
  use e <- result.map(case condition {
    Some(cond) -> {
      let e = case safe_to_emit_twice(cond) {
        True -> e
        False -> emit_ir(e, IrLabel(loop_test))
      }
      emit_test(e, cond, jump_when: True, to: loop_start)
    }
    None -> Ok(emit_ir(e, IrJump(loop_start)))
  })
  let e = emit_ir(e, IrLabel(loop_end))
  pop_frame(e)
}

// no nested functions, templates or suspends
fn safe_to_emit_twice(expr: ast.Expression) -> Bool {
  case expr {
    ast.Identifier(..)
    | ast.NumberLiteral(..)
    | ast.BigIntLiteral(..)
    | ast.StringLiteral(..)
    | ast.BooleanLiteral(..)
    | ast.NullLiteral(..)
    | ast.UndefinedExpression(..)
    | ast.ThisExpression(..) -> True
    ast.ParenthesizedExpression(_, inner)
    | ast.UnaryExpression(_, _, inner)
    | ast.UpdateExpression(argument: inner, ..)
    | ast.MemberExpression(_, inner, ast.Dot(..)) -> safe_to_emit_twice(inner)
    ast.MemberExpression(_, obj, ast.Bracket(k))
    | ast.BinaryExpression(_, _, obj, k)
    | ast.LogicalExpression(_, _, obj, k)
    | ast.AssignmentExpression(_, _, obj, k) ->
      safe_to_emit_twice(obj) && safe_to_emit_twice(k)
    ast.ConditionalExpression(_, a, b, c) ->
      safe_to_emit_twice(a) && safe_to_emit_twice(b) && safe_to_emit_twice(c)
    ast.CallExpression(_, callee, args) ->
      safe_to_emit_twice(callee) && list.all(args, safe_to_emit_twice)
    _ -> False
  }
}

// scope-free try so exported bindings stay module-scoped
fn emit_module_using_top(
  e: Emitter,
  stmts: List(ast.StmtWithLine),
) -> Result(Emitter, EmitError) {
  let #(slots, items, e) = build_using_slots(e, stmts)
  let e = emit_using_prelude(e, slots)

  let #(catch_label, e) = fresh_label(e)
  let #(dispose_label, e) = fresh_label(e)

  let e = emit_ir(e, IrPushTry(catch_label, CatchOnly))
  let e = push_barrier(e, pop_try: 1, label_finally: None, drop: 0)
  use e <- result.map(emit_using_body(e, items))
  e
  |> pop_frame
  |> emit_op(opcode.PopTry)
  |> emit_ir(IrJump(dispose_label))
  |> emit_ir(IrLabel(catch_label))
  |> emit_using_record_error(slots)
  |> emit_ir(IrLabel(dispose_label))
  |> emit_using_dispose(slots)
  |> push_const(mk_undefined())
}

// vars_to_global is false only for strict direct eval (own var env)
fn emit_top_level_body(
  e: Emitter,
  stmts: List(ast.StmtWithLine),
  script_strict script_strict: Bool,
  vars_to_global vars_to_global: Bool,
) -> Result(EmitOutput, EmitError) {
  let e = enter_root_scope(e)
  let e = case vars_to_global {
    False -> e
    True ->
      e
      |> list.fold(
        ast_util.var_declared_names(stmts),
        _,
        emit_declare_var_global,
      )
      |> list.fold(
        ast_util.top_level_function_names(stmts),
        _,
        emit_declare_fn_global,
      )
  }
  // annex b §B.3.3: sloppy block function names get a var binding first
  let e = case script_strict {
    True -> e
    False -> list.fold(fn_info(e).annexb_candidates, e, emit_declare_var_global)
  }
  // §16.1.7 top-level let/const/class go to the global lexical record
  let e = case e.top_lex {
    LocalLexical -> e
    GlobalLexical ->
      list.fold(ast_util.lexically_declared_names(stmts), e, fn(e, lex) {
        let #(name, is_const) = lex
        emit_op(e, opcode.DeclareGlobalLex(name, is_const))
      })
  }
  use #(hoisted_funcs, e) <- result.try(collect_hoisted_funcs(e, stmts))
  let e = emit_hoisted_funcs(e, hoisted_funcs)
  use e <- result.try(emit_stmts_tail(e, stmts))
  let Finished(code:, constants:, children:) = finish(e)
  Ok(
    EmitOutput(
      code:,
      constants:,
      children:,
      is_strict: script_strict,
      tree: e.scope_tree,
      module_hoisted_funcs: [],
    ),
  )
}

fn new_emitter(tree: scope.ScopeTree, fn_id: ScopeId) -> Emitter {
  Emitter(
    code: [],
    constants_map: dict.new(),
    constants_list: [],
    next_const: 0,
    next_label: 0,
    frame_stack: [],
    functions: [],
    next_func: 0,
    pending_label: None,
    strict: False,
    is_async: False,
    is_arrow: False,
    lexical_refs: lexical.no_lexical_refs,
    references_arguments: False,
    arguments_escape: False,
    code_kind: lexical.ScriptCode,
    top_lex: tree.top_lex,
    scope_tree: tree,
    fn_scope: fn_id,
    current_scope: fn_id,
    scope_cursor: block_child_scopes(tree, fn_id),
    child_fn_cursor: scope.child_function_scopes(tree, fn_id),
    field_init: NoFieldInit,
    in_block: False,
    deletable_global_vars: False,
    in_implicit_derived_ctor: False,
    param_scope_names: [],
    with_stack: [],
    private_env: [],
    completion_var: None,
    free_ref_slots: [],
    initialized: set.new(),
    line: 0,
    next_site: 0,
  )
}

// fn_scope check required: child emitters inherit top_lex
fn at_global_lex(e: Emitter) -> Bool {
  e.top_lex == GlobalLexical
  && e.fn_scope == root_scope_id
  && !e.in_block
  && e.current_scope == e.fn_scope
}

fn fn_info(e: Emitter) -> scope.FunctionInfo {
  scope.function_info(e.scope_tree, e.fn_scope)
}

fn fn_fallthrough(e: Emitter) -> GlobalFallthrough {
  fn_info(e).fallthrough
}

// block-kind children only; function children come from child_fn_cursor
fn block_child_scopes(t: scope.ScopeTree, id: ScopeId) -> List(ScopeId) {
  use c <- list.filter(scope.child_scopes(t, id))
  !scope.is_function_kind(scope.get(t, c).kind)
}

// stops at the function root so walks never read the parent frame
fn scope_parent_in_fn(e: Emitter, id: ScopeId) -> Option(ScopeId) {
  case id == e.fn_scope {
    True -> None
    False -> scope.get(e.scope_tree, id).parent
  }
}

type ScopeSave {
  ScopeSave(scope: ScopeId, cursor: List(ScopeId), in_block: Bool)
}

// no ir marker, only the cursor moves; emits the child's binding prologue
fn enter_scope(e: Emitter, in_block in_block: Bool) -> #(ScopeSave, Emitter) {
  case e.scope_cursor {
    [child_id, ..parent_rest] -> {
      let save =
        ScopeSave(
          scope: e.current_scope,
          cursor: parent_rest,
          in_block: e.in_block,
        )
      let e =
        Emitter(
          ..e,
          current_scope: child_id,
          scope_cursor: block_child_scopes(e.scope_tree, child_id),
          in_block:,
        )
      #(save, emit_binding_prologue(e, child_id))
    }
    // scope omitted by the analyzer: stay in place, no prologue, no cursor change
    [] -> {
      let save =
        ScopeSave(scope: e.current_scope, cursor: [], in_block: e.in_block)
      #(save, Emitter(..e, in_block:))
    }
  }
}

fn leave_scope(e: Emitter, save: ScopeSave) -> Emitter {
  Emitter(
    ..e,
    current_scope: save.scope,
    scope_cursor: save.cursor,
    in_block: save.in_block,
  )
}

fn in_child_scope(
  e: Emitter,
  in_block in_block: Bool,
  body body: fn(Emitter) -> Result(Emitter, EmitError),
) -> Result(Emitter, EmitError) {
  let #(save, e) = enter_scope(e, in_block:)
  use e <- result.map(body(e))
  leave_scope(e, save)
}

// only let/const/using heads have a scope; None makes leave a no-op
fn enter_for_scope(
  e: Emitter,
  has_lex_head has_lex_head: Bool,
) -> #(Option(ScopeSave), Emitter) {
  case has_lex_head {
    True -> {
      let #(save, e) = enter_scope(e, in_block: e.in_block)
      #(Some(save), e)
    }
    False -> #(None, e)
  }
}

fn leave_for_scope(e: Emitter, save: Option(ScopeSave)) -> Emitter {
  case save {
    Some(s) -> leave_scope(e, s)
    None -> e
  }
}

// root scope has no parent, so not via enter_scope
fn enter_root_scope(e: Emitter) -> Emitter {
  let e =
    Emitter(
      ..e,
      current_scope: e.fn_scope,
      scope_cursor: block_child_scopes(e.scope_tree, e.fn_scope),
    )
  let e = emit_binding_prologue(e, e.fn_scope)
  // inherited (captured) lexical slots are already boxed by the owner
  let info = fn_info(e)
  use e, ref <- list.fold(lexical.all_lexical_refs, e)
  case owned_boxed_lexical_slot(info, ref) {
    Some(slot) -> emit_op(e, opcode.BoxLocal(slot))
    None -> e
  }
}

fn owned_boxed_lexical_slot(
  info: scope.FunctionInfo,
  ref: lexical.LexicalRef,
) -> Option(Int) {
  let owned = !dict.has_key(info.lexical_captures, ref)
  let boxed = lexical.refs_get(info.lexical_boxed, ref)
  case owned && boxed {
    True -> lexical.slot_of(info.lexical, ref)
    False -> None
  }
}

fn bindings_in_slot_order(s: scope.Scope) -> List(#(String, scope.Binding)) {
  dict.to_list(s.bindings)
  |> list.sort(fn(a, b) { int.compare({ a.1 }.slot, { b.1 }.slot) })
}

// var -> undef, let/const -> uninit, param/catch -> box only, capture -> nothing
fn emit_binding_prologue(e: Emitter, scope_id: ScopeId) -> Emitter {
  let bindings = bindings_in_slot_order(scope.get(e.scope_tree, scope_id))
  let at_module_root = scope_id == root_scope_id && e.fn_scope == root_scope_id
  // the frame already pads root-scope vars with undefined
  let is_function_root = scope_id == e.fn_scope
  use e, #(name, b) <- list.fold(bindings, e)
  let seeded =
    at_module_root && set.contains(e.scope_tree.linker_seeded_exports, name)
  use <- bool.guard(seeded, e)
  let e = case b.kind {
    VarBinding if is_function_root -> e
    VarBinding -> emit_store_const(e, b.slot, mk_undefined())
    LetBinding | ConstBinding | FnNameBinding ->
      emit_store_const(e, b.slot, mk_tdz())
    ParamBinding | CatchBinding | CaptureBinding -> e
  }
  case b.kind, b.boxed {
    CaptureBinding, _ -> e
    _, True -> emit_op(e, opcode.BoxLocal(b.slot))
    _, False -> e
  }
}

// raw slot store, no tdz or const checks
fn emit_store_const(e: Emitter, slot: Int, val: JsVal) -> Emitter {
  e |> push_const(val) |> emit_op(opcode.PutLocal(slot))
}

fn emit_scratch_get(e: Emitter, slot: Int) -> Emitter {
  emit_op(e, opcode.GetLocal(slot))
}

fn emit_scratch_put(e: Emitter, slot: Int) -> Emitter {
  emit_op(e, opcode.PutLocal(slot))
}

fn is_annexb_blocked(e: Emitter, name: String) -> Bool {
  set.contains(scope.get(e.scope_tree, e.current_scope).annexb_blocked, name)
}

type AnnexBTarget {
  AnnexBLocal(target: scope.SlotRef)
  AnnexBFallthrough
  // defensive only, the analyzer already excludes these
  AnnexBBlocked
}

// annex b §B.3.2.6: copy the block function into the enclosing var binding
fn emit_annexb_promote(e: Emitter, name: String) -> Emitter {
  case annexb_find_source(e, e.current_scope, name) {
    None -> e
    Some(#(source, outside)) ->
      case annexb_find_target(e, outside, name) {
        AnnexBBlocked -> e
        AnnexBLocal(target) ->
          e
          |> emit_slot_get(scope.binding_ref(source))
          |> emit_slot_put(target)
        AnnexBFallthrough -> {
          let e = emit_slot_get(e, scope.binding_ref(source))
          case fn_fallthrough(e) {
            ToGlobal -> emit_op(e, opcode.PutGlobal(name))
            ToEvalEnv -> emit_op(e, opcode.PutEvalVar(name))
          }
        }
      }
  }
}

fn annexb_find_source(
  e: Emitter,
  from: ScopeId,
  name: String,
) -> Option(#(scope.Binding, Option(ScopeId))) {
  let node = scope.get(e.scope_tree, from)
  case dict.get(node.bindings, name) {
    Ok(b) -> Some(#(b, scope_parent_in_fn(e, from)))
    Error(Nil) ->
      case scope_parent_in_fn(e, from) {
        Some(parent) -> annexb_find_source(e, parent, name)
        None -> None
      }
  }
}

// catch params stepped over, let/const aborts, var receives, root falls through
fn annexb_find_target(
  e: Emitter,
  from: Option(ScopeId),
  name: String,
) -> AnnexBTarget {
  case from {
    None -> AnnexBFallthrough
    Some(id) -> {
      let node = scope.get(e.scope_tree, id)
      case node.kind {
        // §B.3.4 simple catch params are var-transparent, step over
        scope.Catch -> annexb_find_target(e, scope_parent_in_fn(e, id), name)
        _ ->
          case dict.get(node.bindings, name) {
            Ok(scope.Binding(kind: LetBinding, ..))
            | Ok(scope.Binding(kind: ConstBinding, ..))
            | Ok(scope.Binding(kind: FnNameBinding, ..)) -> AnnexBBlocked
            Ok(scope.Binding(kind: CatchBinding, ..)) ->
              annexb_find_target(e, scope_parent_in_fn(e, id), name)
            Ok(b) -> AnnexBLocal(scope.binding_ref(b))
            Error(Nil) -> annexb_find_target(e, scope_parent_in_fn(e, id), name)
          }
      }
    }
  }
}

fn emit_slot_get(e: Emitter, ref: scope.SlotRef) -> Emitter {
  case ref.boxed {
    True -> emit_op(e, opcode.GetBoxed(ref.slot))
    False -> emit_op(e, opcode.GetLocal(ref.slot))
  }
}

fn emit_slot_put(e: Emitter, ref: scope.SlotRef) -> Emitter {
  case ref.boxed {
    True -> emit_op(e, opcode.PutBoxed(ref.slot))
    False -> emit_op(e, opcode.PutLocal(ref.slot))
  }
}

// routed by fallthrough so sloppy direct eval vars land in the caller's env
fn emit_declare_var_global(e: Emitter, name: String) -> Emitter {
  case fn_fallthrough(e) {
    ToGlobal ->
      emit_op(
        e,
        opcode.DeclareGlobalVar(name, deletable: e.deletable_global_vars),
      )
    ToEvalEnv -> emit_op(e, opcode.DeclareEvalVar(name))
  }
}

fn emit_declare_fn_global(e: Emitter, name: String) -> Emitter {
  case fn_fallthrough(e) {
    ToGlobal ->
      emit_op(
        e,
        opcode.DeclareGlobalFn(name, deletable: e.deletable_global_vars),
      )
    ToEvalEnv -> emit_op(e, opcode.DeclareEvalVar(name))
  }
}

fn declare_lex(e: Emitter, name: String, is_const is_const: Bool) -> Emitter {
  case at_global_lex(e) {
    True -> emit_op(e, opcode.DeclareGlobalLex(name, is_const))
    False -> e
  }
}

// init store bypassing tdz/const checks
fn init_lex(e: Emitter, name: String) -> Emitter {
  case at_global_lex(e) {
    True -> emit_op(e, opcode.InitGlobalLex(name))
    False -> emit_var_init(e, name)
  }
}

fn emit_ir(e: Emitter, op: IrOp) -> Emitter {
  case op {
    IrLabel(_) -> Emitter(..e, code: [op, ..e.code], line: 0)
    _ -> Emitter(..e, code: [op, ..e.code])
  }
}

fn emit_op(e: Emitter, op: opcode.Op) -> Emitter {
  emit_ir(e, IrFinal(op))
}

fn track_arguments_ref(e: Emitter, name: String) -> Emitter {
  case name {
    "arguments" ->
      Emitter(..e, references_arguments: True, arguments_escape: True)
    _ -> e
  }
}

type ApplyArgumentsForwarding {
  ApplyArgumentsForwarding(this_arg: ast.Expression, arguments_slot: Int)
}

// f.apply(t, arguments) reading the implicit binding: forward without materialising
fn apply_arguments_forwarding(
  e: Emitter,
  call: ast.Expression,
) -> Option(ApplyArgumentsForwarding) {
  let own_binding = case e.is_arrow, e.code_kind {
    False, lexical.FunctionCode | False, lexical.MethodCode -> True
    _, _ -> False
  }
  case call {
    ast.CallExpression(
      _,
      ast.MemberExpression(_, _, ast.Dot(name: "apply", ..)),
      [this_arg, ast.Identifier(name: "arguments", ..)],
    )
      if own_binding
    ->
      // only the implicit binding, not a param or let named arguments
      case this_arg, resolve(e, "arguments") {
        ast.SpreadElement(..), _ -> None
        _, scope.Plain(scope.Local(slot:, boxed: False, kind: VarBinding, ..))
        -> Some(ApplyArgumentsForwarding(this_arg:, arguments_slot: slot))
        _, _ -> None
      }
    _ -> None
  }
}

fn emit_var_get(e: Emitter, name: String) -> Emitter {
  let e = track_arguments_ref(e, name)
  let #(crossed, fallback) = split_with_chain(resolve(e, name))
  use e <- emit_with_chain(e, crossed, opcode.IrWithGetVar(name, _))
  emit_target_get(e, fallback)
}

// §13.3.6.2 leaves [f, this]: this is the with object if resolved through one
fn emit_var_get_as_callee(e: Emitter, name: String) -> Emitter {
  let e = track_arguments_ref(e, name)
  let #(crossed, fallback) = split_with_chain(resolve(e, name))
  use e <- emit_with_chain(e, crossed, opcode.IrWithGetVarThis(name, _))
  let e = push_const(e, mk_undefined())
  emit_target_get(e, fallback)
}

fn emit_var_put(e: Emitter, name: String) -> Emitter {
  let e = track_arguments_ref(e, name)
  let #(crossed, fallback) = split_with_chain(resolve(e, name))
  use e <- emit_with_chain(e, crossed, opcode.IrWithPutVar(name, _))
  emit_target_put(e, fallback, name, after_read: False)
}

// init store, bypasses const/tdz checks, not an arguments reference
fn emit_var_init(e: Emitter, name: String) -> Emitter {
  let assert scope.Plain(target) = resolve(e, name)
    as "emit: var init crossed a with-scope"
  case target {
    scope.Local(slot:, boxed: True, ..) ->
      Emitter(..e, initialized: set.insert(e.initialized, slot))
      |> emit_op(opcode.PutBoxed(slot))
    scope.Local(slot:, boxed: False, ..) ->
      Emitter(..e, initialized: set.insert(e.initialized, slot))
      |> emit_op(opcode.PutLocal(slot))
    scope.Global(name:) -> emit_op(e, opcode.PutGlobal(name))
    scope.EvalEnv(name:) -> emit_op(e, opcode.PutEvalVar(name))
  }
}

// fallthrough typeof never throws (§13.5.3)
fn emit_var_typeof(e: Emitter, name: String) -> Emitter {
  let e = track_arguments_ref(e, name)
  let #(crossed, fallback) = split_with_chain(resolve(e, name))
  let typeof_fallback = fn(e: Emitter) {
    case fallback {
      scope.Local(..) -> emit_target_get(e, fallback) |> emit_op(opcode.TypeOf)
      scope.Global(name:) -> emit_op(e, opcode.TypeofGlobal(name))
      scope.EvalEnv(name:) -> emit_op(e, opcode.TypeofEvalVar(name))
    }
  }
  case crossed {
    [] -> typeof_fallback(e)
    _ -> {
      let #(hit, e) = fresh_label(e)
      let #(end, e) = fresh_label(e)
      let e =
        list.fold(crossed, e, fn(e, w) {
          e
          |> emit_slot_get(w)
          |> emit_ir(opcode.IrWithGetVar(name, hit))
        })
      let e = typeof_fallback(e)
      let e = emit_ir(e, IrJump(end))
      let e = emit_ir(e, IrLabel(hit))
      let e = emit_op(e, opcode.TypeOf)
      emit_ir(e, IrLabel(end))
    }
  }
}

// gap: eval-env bindings report true but are not removed
fn emit_var_delete(e: Emitter, name: String) -> Emitter {
  let e = track_arguments_ref(e, name)
  let #(crossed, fallback) = split_with_chain(resolve(e, name))
  use e <- emit_with_chain(e, crossed, opcode.IrWithDeleteVar(name, _))
  case fallback {
    scope.Local(..) -> push_const(e, mk_bool(False))
    scope.Global(name:) -> emit_op(e, opcode.DeleteGlobalVar(name))
    scope.EvalEnv(name: _) -> push_const(e, mk_bool(True))
  }
}

// resolved once by emit_var_ref_make so get/put never re-resolve after the rhs
type VarRef {
  VarRef(
    name: String,
    fallback: scope.BindingTarget,
    base_slot: Option(Int),
    read: Bool,
  )
}

// §13.15.2 1.a: resolve before the rhs; stash the with base in a scratch slot
fn emit_var_ref_make(e: Emitter, name: String) -> #(VarRef, Emitter) {
  let e = track_arguments_ref(e, name)
  let #(crossed, fallback) = split_with_chain(resolve(e, name))
  case crossed {
    [] -> #(VarRef(name:, fallback:, base_slot: None, read: False), e)
    _ -> {
      let #(slot, e) = acquire_ref_slot(e)
      let #(lref, e) = fresh_label(e)
      let e =
        list.fold(crossed, e, fn(e, w) {
          e
          |> emit_slot_get(w)
          |> emit_ir(opcode.IrWithMakeRef(name, lref))
        })
      // undefined base means take the static fallback
      let e = push_const(e, mk_undefined())
      let e = emit_ir(e, IrLabel(lref))
      #(
        VarRef(name:, fallback:, base_slot: Some(slot), read: False),
        emit_op(e, opcode.PutLocal(slot)),
      )
    }
  }
}

fn emit_var_ref_get(e: Emitter, ref: VarRef) -> Emitter {
  case ref.base_slot {
    None -> emit_target_get(e, ref.fallback)
    Some(slot) -> {
      let #(got, e) = fresh_label(e)
      let e = emit_op(e, opcode.GetLocal(slot))
      let e = emit_ir(e, opcode.IrWithGetRefValue(ref.name, got))
      let e = emit_target_get(e, ref.fallback)
      emit_ir(e, IrLabel(got))
    }
  }
}

fn emit_var_ref_put(e: Emitter, ref: VarRef) -> Emitter {
  case ref.base_slot {
    None -> emit_target_put(e, ref.fallback, ref.name, after_read: ref.read)
    Some(slot) -> {
      let e = Emitter(..e, free_ref_slots: [slot, ..e.free_ref_slots])
      let #(done, e) = fresh_label(e)
      let e = emit_op(e, opcode.GetLocal(slot))
      let e = emit_ir(e, opcode.IrWithPutRefValue(ref.name, done))
      let e = emit_target_put(e, ref.fallback, ref.name, after_read: ref.read)
      emit_ir(e, IrLabel(done))
    }
  }
}

// make ref -> body -> dup -> put; ref resolved once before body
fn with_identifier_write(
  e: Emitter,
  name: String,
  body: fn(Emitter) -> Result(Emitter, EmitError),
) -> Result(Emitter, EmitError) {
  let #(ref, e) = emit_var_ref_make(e, name)
  use e <- result.map(body(e))
  e |> emit_op(opcode.Dup) |> emit_var_ref_put(ref)
}

// the get goes through the same ref as the put
fn with_identifier_read_write(
  e: Emitter,
  name: String,
  body: fn(Emitter) -> Result(Emitter, EmitError),
) -> Result(Emitter, EmitError) {
  let #(ref, e) = emit_var_ref_make(e, name)
  let e = emit_var_ref_get(e, ref)
  use e <- result.map(body(e))
  e |> emit_op(opcode.Dup) |> emit_var_ref_put(VarRef(..ref, read: True))
}

// §14.7.4.2 per-iteration copy into a fresh box, no-op when unboxed
fn emit_var_rebox(e: Emitter, name: String) -> Emitter {
  let assert scope.Plain(target) = resolve(e, name)
    as "emit: var rebox crossed a with-scope"
  case target {
    scope.Local(slot:, boxed: True, ..) ->
      e
      |> emit_op(opcode.GetBoxed(slot))
      |> emit_op(opcode.PutLocal(slot))
      |> emit_op(opcode.BoxLocal(slot))
    _ -> e
  }
}

fn resolve(e: Emitter, name: String) -> scope.Resolution {
  scope.lookup(e.scope_tree, e.current_scope, name)
}

fn split_with_chain(
  res: scope.Resolution,
) -> #(List(scope.SlotRef), scope.BindingTarget) {
  case res {
    scope.WithChain(crossed_slots:, fallback:) -> #(crossed_slots, fallback)
    scope.Plain(target) -> #([], target)
  }
}

fn emit_target_get(e: Emitter, res: scope.BindingTarget) -> Emitter {
  case res {
    scope.Local(slot:, boxed:, ..) ->
      emit_slot_get(e, scope.SlotRef(slot:, boxed:))
    scope.Global(name:) -> emit_op(e, opcode.GetGlobal(name))
    scope.EvalEnv(name:) -> emit_op(e, opcode.GetEvalVar(name))
  }
}

// after_read: a get already ran, skip the tdz check
fn emit_target_put(
  e: Emitter,
  res: scope.BindingTarget,
  name: String,
  after_read after_read: Bool,
) -> Emitter {
  case res {
    scope.Global(name:) -> emit_op(e, opcode.PutGlobal(name))
    scope.EvalEnv(name:) -> emit_op(e, opcode.PutEvalVar(name))
    scope.Local(slot:, boxed:, kind:, declared_kind:) -> {
      let ref = scope.SlotRef(slot:, boxed:)
      case declared_kind, kind {
        // const always throws; nfe self-name throws only in strict, else drops
        ConstBinding, _ -> emit_op(e, opcode.ThrowConstAssign(name))
        FnNameBinding, _ ->
          case e.strict {
            True -> emit_op(e, opcode.ThrowConstAssign(name))
            False -> emit_op(e, opcode.Pop)
          }
        // capture may still be in tdz, so check unless a read just succeeded
        _, CaptureBinding ->
          case after_read {
            True -> emit_slot_put(e, ref)
            False -> emit_checked_put(e, ref)
          }
        // let not yet initialized linearly: the store may run during tdz
        _, LetBinding ->
          case after_read || set.contains(e.initialized, slot) {
            True -> emit_slot_put(e, ref)
            False -> emit_checked_put(e, ref)
          }
        _, _ -> emit_slot_put(e, ref)
      }
    }
  }
}

// tdz-checked store: read (throws on uninit), pop, then store
fn emit_checked_put(e: Emitter, ref: scope.SlotRef) -> Emitter {
  e
  |> emit_slot_get(ref)
  |> emit_op(opcode.Pop)
  |> emit_slot_put(ref)
}

// §14.11 probe each crossed with object innermost first, else fallback
fn emit_with_chain(
  e: Emitter,
  crossed: List(scope.SlotRef),
  with_op: fn(LabelId) -> IrOp,
  fallback: fn(Emitter) -> Emitter,
) -> Emitter {
  case crossed {
    [] -> fallback(e)
    _ -> {
      let #(done, e) = fresh_label(e)
      let e =
        list.fold(crossed, e, fn(e, w) {
          e |> emit_slot_get(w) |> emit_ir(with_op(done))
        })
      let e = fallback(e)
      emit_ir(e, IrLabel(done))
    }
  }
}

// reuse a freed scratch slot (lifo) or mint a new one
fn acquire_ref_slot(e: Emitter) -> #(Int, Emitter) {
  case e.free_ref_slots {
    [slot, ..rest] -> #(slot, Emitter(..e, free_ref_slots: rest))
    [] -> fresh_slot(e)
  }
}

fn add_constant(e: Emitter, val: JsVal) -> #(Int, Emitter) {
  case dict.get(e.constants_map, val) {
    Ok(idx) -> #(idx, e)
    Error(Nil) -> {
      let idx = e.next_const
      let e =
        Emitter(
          ..e,
          constants_map: dict.insert(e.constants_map, val, idx),
          constants_list: [val, ..e.constants_list],
          next_const: idx + 1,
        )
      #(idx, e)
    }
  }
}

fn push_const(e: Emitter, val: JsVal) -> Emitter {
  let #(idx, e) = add_constant(e, val)
  emit_op(e, opcode.PushConst(idx))
}

// #x names use brand-checked private opcodes; [obj, ..] -> [val, ..]
fn emit_get_field(e: Emitter, name: String) -> Emitter {
  case name {
    "#" <> _ ->
      e
      |> emit_var_get(name)
      |> emit_op(opcode.GetPrivateFieldDyn)
    _ -> emit_ir(e, IrGetField(name))
  }
}

// [obj, ..] -> [val, obj, ..]
fn emit_get_field_keep(e: Emitter, name: String) -> Emitter {
  case name {
    "#" <> _ ->
      e
      |> emit_var_get(name)
      |> emit_op(opcode.GetPrivateFieldDynKeep)
    _ -> emit_ir(e, IrGetFieldKeep(name))
  }
}

// [val, obj, ..] -> [val, ..]
fn emit_put_field(e: Emitter, name: String) -> Emitter {
  case name {
    "#" <> _ ->
      e
      |> emit_var_get(name)
      |> emit_op(opcode.PutPrivateFieldDyn)
    _ -> emit_ir(e, IrPutField(name))
  }
}

fn fresh_label(e: Emitter) -> #(LabelId, Emitter) {
  let label = e.next_label
  #(opcode.LabelId(label), Emitter(..e, next_label: label + 1))
}

// anonymous slot past all named bindings, never captured
fn fresh_slot(e: Emitter) -> #(Int, Emitter) {
  let #(slot, tree) = scope.alloc_scratch(e.scope_tree, e.fn_scope)
  #(slot, Emitter(..e, scope_tree: tree))
}

fn push_frame(e: Emitter, frame: Frame) -> Emitter {
  Emitter(..e, frame_stack: [frame, ..e.frame_stack], pending_label: None)
}

// iterator loops: call after the body PushTry so the crossing PopTry lines up
fn push_loop(
  e: Emitter,
  break_target: LabelId,
  continue_target: LabelId,
  iterator: LoopIter,
) -> Emitter {
  push_frame(
    e,
    LoopFrame(
      break_target:,
      continue_target:,
      label: e.pending_label,
      iterator:,
    ),
  )
}

fn push_switch(e: Emitter, break_target: LabelId) -> Emitter {
  push_frame(e, SwitchFrame(break_target:, label: e.pending_label))
}

// not via push_frame: pending_label must survive a barrier
fn push_barrier(
  e: Emitter,
  pop_try pop_try: Int,
  label_finally label_finally: Option(LabelId),
  drop drop: Int,
) -> Emitter {
  Emitter(..e, frame_stack: [
    BarrierFrame(pop_try:, label_finally:, drop_count: drop),
    ..e.frame_stack
  ])
}

fn pop_frame(e: Emitter) -> Emitter {
  let assert [_, ..rest] = e.frame_stack
  Emitter(..e, frame_stack: rest)
}

fn repeat_ir(e: Emitter, op: IrOp, n: Int) -> Emitter {
  case n <= 0 {
    True -> e
    False -> repeat_ir(emit_ir(e, op), op, n - 1)
  }
}

// swap;pop n times: discards n slots under top of stack
fn emit_drop_under_top(e: Emitter, n: Int) -> Emitter {
  case n <= 0 {
    True -> e
    False ->
      emit_drop_under_top(
        e |> emit_op(opcode.Swap) |> emit_op(opcode.Pop),
        n - 1,
      )
  }
}

// dummy slot + gosub + drop, stack-neutral
fn emit_gosub_normal(e: Emitter, fin_label: LabelId) -> Emitter {
  e
  |> push_const(mk_undefined())
  |> emit_ir(IrGosub(fin_label))
  |> emit_op(opcode.Pop)
}

// subroutine entry stack: [retpc, slot, ..base]
fn emit_finally_subroutine(
  e: Emitter,
  throw_label: LabelId,
  fin_label: LabelId,
  emit_finally: fn(Emitter) -> Result(Emitter, EmitError),
) -> Result(Emitter, EmitError) {
  let e = emit_ir(e, IrLabel(throw_label))
  let e = emit_ir(e, IrGosub(fin_label))
  let e = emit_op(e, opcode.Throw)

  let e = emit_ir(e, IrLabel(fin_label))
  let e = push_barrier(e, pop_try: 0, label_finally: None, drop: 2)
  let saved_cv = e.completion_var
  let e = Emitter(..e, completion_var: None)
  use e <- result.try(emit_finally(e))
  let e = Emitter(..e, completion_var: saved_cv)
  let e = pop_frame(e)
  Ok(emit_op(e, opcode.Ret))
}

// outer PushTry -> throw, inner -> catch; emit_catch emits its own label
fn emit_try_catch_finally(
  e: Emitter,
  emit_body: fn(Emitter) -> Result(Emitter, EmitError),
  emit_finally: fn(Emitter) -> Result(Emitter, EmitError),
  emit_catch: fn(Emitter, LabelId, LabelId) -> Result(Emitter, EmitError),
) -> Result(Emitter, EmitError) {
  let #(throw_label, e) = fresh_label(e)
  let #(catch_label, e) = fresh_label(e)
  let #(fin_label, e) = fresh_label(e)
  let #(end_label, e) = fresh_label(e)

  let e = emit_ir(e, IrPushTry(throw_label, Finally(fin_label)))
  let e = emit_ir(e, IrPushTry(catch_label, CatchOnly))
  let e = push_barrier(e, pop_try: 2, label_finally: Some(fin_label), drop: 0)
  use e <- result.try(emit_body(e))
  let e = pop_frame(e)
  let e = emit_op(e, opcode.PopTry)
  let e = emit_op(e, opcode.PopTry)
  let e = emit_gosub_normal(e, fin_label)
  let e = emit_ir(e, IrJump(end_label))

  // stack = [thrown, ..base], outer try still active
  use e <- result.try(emit_catch(e, catch_label, fin_label))
  let e = emit_op(e, opcode.PopTry)
  let e = emit_gosub_normal(e, fin_label)
  let e = emit_ir(e, IrJump(end_label))

  use e <- result.map(emit_finally_subroutine(
    e,
    throw_label,
    fin_label,
    emit_finally,
  ))
  emit_ir(e, IrLabel(end_label))
}

// §14.8 unlabeled break skips labeled blocks; continue only targets loops
fn frame_target(
  frame: Frame,
  name: Option(String),
  is_continue is_continue: Bool,
) -> Option(LabelId) {
  case frame, is_continue {
    LoopFrame(continue_target:, label:, ..), True ->
      label_matches(label, name, continue_target)
    LoopFrame(break_target:, label:, ..), False
    | SwitchFrame(break_target:, label:), False
    -> label_matches(label, name, break_target)
    LabeledBlockFrame(break_target:, label:), False ->
      case name == Some(label) {
        True -> Some(break_target)
        False -> None
      }
    SwitchFrame(..), True | LabeledBlockFrame(..), True | BarrierFrame(..), _ ->
      None
  }
}

// unlabeled jumps take the nearest frame; labeled ones need the frame's label
fn label_matches(
  label: Option(String),
  name: Option(String),
  target: LabelId,
) -> Option(LabelId) {
  case name == None || name == label {
    True -> Some(target)
    False -> None
  }
}

// pop trys, drop slots, close iterator, run finally; a return value rides on top
fn emit_cross_frame(
  e: Emitter,
  frame: Frame,
  value_on_top value_on_top: Bool,
) -> Emitter {
  let keep_value_on_top = fn(e) {
    case value_on_top {
      True -> emit_op(e, opcode.Swap)
      False -> e
    }
  }
  case frame {
    LoopFrame(iterator: NoIter, ..) | SwitchFrame(..) | LabeledBlockFrame(..) ->
      e
    LoopFrame(iterator: SyncIter, ..) ->
      e
      |> emit_op(opcode.PopTry)
      |> keep_value_on_top
      |> emit_op(opcode.IteratorClose)
    LoopFrame(iterator: AsyncIter, ..) ->
      e
      |> emit_op(opcode.PopTry)
      |> keep_value_on_top
      |> emit_async_iterator_close
    BarrierFrame(pop_try:, label_finally:, drop_count:) -> {
      let e = repeat_ir(e, IrFinal(opcode.PopTry), pop_try)
      let e = case value_on_top {
        True -> emit_drop_under_top(e, drop_count)
        False -> repeat_ir(e, IrFinal(opcode.Pop), drop_count)
      }
      case label_finally, value_on_top {
        None, _ -> e
        // the value doubles as the subroutine's dummy slot
        Some(lbl), True -> emit_ir(e, IrGosub(lbl))
        Some(lbl), False -> emit_gosub_normal(e, lbl)
      }
    }
  }
}

// §7.4.13 async iterator close, normal completion: [iter, ..] -> [..]
fn emit_async_iterator_close(e: Emitter) -> Emitter {
  let #(no_ret, e) = fresh_label(e)
  let #(closed, e) = fresh_label(e)
  e
  |> emit_ir(IrGetFieldKeep("return"))
  |> emit_op(opcode.Dup)
  |> emit_ir(IrJumpIfNullish(no_ret))
  |> emit_op(opcode.CallMethod(0))
  |> emit_op(opcode.Await)
  |> emit_op(opcode.IteratorCheckObject)
  |> emit_ir(IrJump(closed))
  // [ret(nullish), iter, ..]
  |> emit_ir(IrLabel(no_ret))
  |> emit_op(opcode.Pop)
  |> emit_ir(IrLabel(closed))
  |> emit_op(opcode.Pop)
}

fn emit_goto_loop(
  e: Emitter,
  name: Option(String),
  is_continue is_continue: Bool,
) -> Result(Emitter, EmitError) {
  emit_goto_loop_walk(e, e.frame_stack, name, is_continue)
}

fn emit_goto_loop_walk(
  e: Emitter,
  stack: List(Frame),
  name: Option(String),
  is_continue is_continue: Bool,
) -> Result(Emitter, EmitError) {
  case stack {
    [] ->
      case is_continue {
        True -> Error(ContinueOutsideLoop)
        False -> Error(BreakOutsideLoop)
      }
    [frame, ..rest] ->
      case frame_target(frame, name, is_continue) {
        Some(target) -> Ok(emit_ir(e, IrJump(target)))
        None ->
          emit_cross_frame(e, frame, value_on_top: False)
          |> emit_goto_loop_walk(rest, name, is_continue)
      }
  }
}

// keep ApplyArguments sites if arguments never escapes, else lower to reads
fn rewrite_apply_arguments(
  code: List(IrOp),
  forward forward: Bool,
  simple_params simple_params: Bool,
) -> List(IrOp) {
  use op <- list.flat_map(code)
  case op, forward {
    IrFinal(opcode.ApplyArguments(slot:, ..)), True -> [
      IrFinal(opcode.ApplyArguments(slot:, simple_params:)),
    ]
    // code is newest-first, so this reads GetLocal then CallMethod
    IrFinal(opcode.ApplyArguments(slot:, ..)), False -> [
      IrFinal(opcode.CallMethod(2)),
      IrFinal(opcode.GetLocal(slot)),
    ]
    _, _ -> [op]
  }
}

fn add_child_function(e: Emitter, child: CompiledChild) -> #(Int, Emitter) {
  let idx = e.next_func
  // arrow refs propagate to the parent; non-arrows own their slots
  let #(lexical_refs, references_arguments, arguments_escape) = case
    child.is_arrow
  {
    True -> #(
      lexical.refs_or(e.lexical_refs, child.lexical_refs),
      e.references_arguments || child.references_arguments,
      e.arguments_escape || child.references_arguments,
    )
    False -> #(e.lexical_refs, e.references_arguments, e.arguments_escape)
  }
  #(
    idx,
    Emitter(
      ..e,
      functions: [child, ..e.functions],
      next_func: idx + 1,
      lexical_refs:,
      references_arguments:,
      arguments_escape:,
    ),
  )
}

// None at script/module root: top-level this has no slot, reads undefined
fn resolve_lexical(
  e: Emitter,
  ref: lexical.LexicalRef,
) -> Option(scope.SlotRef) {
  let info = fn_info(e)
  let boxed = lexical.refs_get(info.lexical_boxed, ref)
  scope.lexical_slot_in(info, ref)
  |> option.map(fn(slot) { scope.SlotRef(slot:, boxed:) })
}

fn lexical_refs_with(
  refs: lexical.LexicalRefs,
  ref: lexical.LexicalRef,
) -> lexical.LexicalRefs {
  case ref {
    lexical.ThisRef -> lexical.LexicalRefs(..refs, this: True)
    lexical.ActiveFuncRef -> lexical.LexicalRefs(..refs, active_func: True)
    lexical.HomeObjectRef -> lexical.LexicalRefs(..refs, home_object: True)
    lexical.NewTargetRef -> lexical.LexicalRefs(..refs, new_target: True)
  }
}

fn mark_lexical_ref(e: Emitter, ref: lexical.LexicalRef) -> Emitter {
  Emitter(..e, lexical_refs: lexical_refs_with(e.lexical_refs, ref))
}

fn emit_lexical_get(e: Emitter, ref: lexical.LexicalRef) -> Emitter {
  let e = mark_lexical_ref(e, ref)
  case resolve_lexical(e, ref) {
    Some(slot) -> emit_slot_get(e, slot)
    None -> push_const(e, mk_undefined())
  }
}

fn emit_this_get(e: Emitter) -> Emitter {
  emit_lexical_get(e, lexical.ThisRef)
}

// §10.2.4 writing an initialized this is a ReferenceError
fn emit_this_bind(e: Emitter) -> Emitter {
  let e = mark_lexical_ref(e, lexical.ThisRef)
  case resolve_lexical(e, lexical.ThisRef) {
    Some(scope.SlotRef(slot:, boxed: True)) ->
      emit_op(e, opcode.PutBoxedCheckInit(slot))
    Some(scope.SlotRef(slot:, boxed: False)) ->
      emit_op(e, opcode.PutLocalCheckInit(slot))
    None -> e
  }
}

// stack after: [home_proto, this, ..]
fn emit_super_base(e: Emitter) -> Emitter {
  e
  |> emit_this_get
  |> emit_lexical_get(lexical.HomeObjectRef)
  |> emit_op(opcode.GetPrototypeOf)
}

// stack after: [key, home_proto, this, ..]
fn emit_super_ref(
  e: Emitter,
  property: ast.MemberProperty,
) -> Result(Emitter, EmitError) {
  emit_super_key(emit_super_base(e), property)
}

// stack after: [method, this, ..]
fn emit_super_method_ref(
  e: Emitter,
  property: ast.MemberProperty,
) -> Result(Emitter, EmitError) {
  let e =
    e
    |> emit_this_get
    |> emit_op(opcode.Dup)
    |> emit_lexical_get(lexical.HomeObjectRef)
    |> emit_op(opcode.GetPrototypeOf)
  use e <- result.map(emit_super_key(e, property))
  emit_op(e, opcode.GetSuperValue)
}

fn emit_super_key(
  e: Emitter,
  property: ast.MemberProperty,
) -> Result(Emitter, EmitError) {
  case property {
    ast.Dot(name:, ..) -> Ok(push_const(e, mk_string(name)))
    ast.Bracket(expression:) -> emit_expr(e, expression)
  }
}

// [..] -> [old, ..put-args]; returns the matching [new, ..put-args] -> [new]
fn emit_member_get_keep(
  e: Emitter,
  target: AssignTarget,
) -> Result(#(fn(Emitter) -> Emitter, Emitter), EmitError) {
  case target {
    SuperMember(property:) -> {
      use e <- result.map(emit_super_ref(e, property))
      #(emit_op(_, opcode.PutSuperValue), emit_op(e, opcode.GetSuperValueKeep))
    }
    StaticMember(object:, prop:) -> {
      use e <- result.map(emit_expr(e, object))
      #(emit_put_field(_, prop), emit_get_field_keep(e, prop))
    }
    ComputedMember(object:, key:) -> {
      use e <- result.try(emit_expr(e, object))
      use e <- result.map(emit_expr(e, key))
      #(emit_op(_, opcode.PutElem), emit_op(e, opcode.GetElemKeep))
    }
    PlainTarget(_) -> Error(NonMemberLValue)
  }
}

// stack-neutral; skipped when the const is undefined (no fields)
fn emit_field_init_call(e: Emitter) -> Emitter {
  let #(skip, e) = fresh_label(e)
  e
  |> emit_var_get(class_fields_init)
  |> emit_op(opcode.Dup)
  |> emit_ir(IrJumpIfFalse(skip))
  |> emit_this_get
  |> emit_op(opcode.Swap)
  |> emit_op(opcode.CallMethod(0))
  |> emit_ir(IrLabel(skip))
  |> emit_op(opcode.Pop)
}

type Finished {
  Finished(
    code: List(IrOp),
    constants: List(JsVal),
    children: List(CompiledChild),
  )
}

fn finish(e: Emitter) -> Finished {
  Finished(
    code: list.reverse(e.code),
    constants: list.reverse(e.constants_list),
    children: list.reverse(e.functions),
  )
}

fn emit_stmt_tail(
  e: Emitter,
  stmt: ast.Statement,
) -> Result(Emitter, EmitError) {
  case stmt {
    ast.ExpressionStatement(expression: expr, ..) -> emit_expr(e, expr)

    ast.BlockStatement(body) -> emit_block(e, body, tail: True)

    ast.IfStatement(cond, cons, alt) ->
      emit_if(e, cond, cons, alt, emit_stmt_tail, push_const(_, mk_undefined()))

    // finally body never supplies the completion value (§14.15.3)
    ast.TryStatement(_, ast.TryFinally(..))
    | ast.TryStatement(_, ast.TryCatchFinally(..)) ->
      emit_stmt_tail_completion(e, stmt)

    ast.TryStatement(block, ast.TryCatch(clause)) ->
      emit_try_catch(e, block, clause, tail: True)

    ast.WithStatement(object, body) -> emit_with(e, object, body, tail: True)

    ast.WhileStatement(..)
    | ast.DoWhileStatement(..)
    | ast.ForStatement(..)
    | ast.ForInStatement(..)
    | ast.ForOfStatement(..)
    | ast.LabeledStatement(..)
    | ast.SwitchStatement(..) -> emit_stmt_tail_completion(e, stmt)

    _ -> {
      use e <- result.map(emit_stmt(e, stmt))
      push_const(e, mk_undefined())
    }
  }
}

fn emit_stmt_tail_completion(
  e: Emitter,
  stmt: ast.Statement,
) -> Result(Emitter, EmitError) {
  let #(slot, e) = fresh_slot(e)
  let saved_var = e.completion_var
  let e = emit_store_const(e, slot, mk_undefined())
  let e = Emitter(..e, completion_var: Some(slot))
  use e <- result.map(emit_stmt(e, stmt))
  let e = Emitter(..e, completion_var: saved_var)
  emit_scratch_get(e, slot)
}

// line 0 marks synthetic statements, no IrLine
fn set_line(e: Emitter, line: Int) -> Emitter {
  case line == 0 || line == e.line {
    True -> e
    False ->
      case e.code {
        // replace an immediately preceding marker instead of stacking
        [opcode.IrLine(_), ..rest] ->
          Emitter(..e, code: [opcode.IrLine(line), ..rest], line:)
        _ -> Emitter(..emit_ir(e, opcode.IrLine(line)), line:)
      }
  }
}

fn emit_block_body(
  e: Emitter,
  body: List(ast.StmtWithLine),
  tail tail: Bool,
) -> Result(Emitter, EmitError) {
  case tail {
    True -> emit_stmts_tail(e, body)
    False -> emit_stmts(e, body)
  }
}

fn emit_block(
  e: Emitter,
  body: List(ast.StmtWithLine),
  tail tail: Bool,
) -> Result(Emitter, EmitError) {
  // blocks declaring nothing have no scope node, skip entirely
  use <- bool.lazy_guard(!ast_util.block_has_declarations(body), fn() {
    emit_block_body(e, body, tail:)
  })
  use e <- in_child_scope(e, in_block: True)
  use e <- result.try(emit_block_declarations(e, body))
  case ast_util.has_using_decl(body) {
    True -> emit_block_using(e, body, tail:)
    False -> emit_block_body(e, body, tail:)
  }
}

fn emit_block_using(
  e: Emitter,
  body: List(ast.StmtWithLine),
  tail tail: Bool,
) -> Result(Emitter, EmitError) {
  let #(slots, items, e) = build_using_slots(e, body)
  let e = emit_using_prelude(e, slots)
  let saved_cv = e.completion_var
  let #(cv, e) = case tail {
    False -> #(None, e)
    True -> {
      let #(slot, e) = fresh_slot(e)
      let e = emit_store_const(e, slot, mk_undefined())
      #(Some(slot), Emitter(..e, completion_var: Some(slot)))
    }
  }
  use e <- result.map({
    use e <- emit_using_try_wrap(e, slots)
    emit_using_body(e, items)
  })
  let e = Emitter(..e, completion_var: saved_cv)
  case cv {
    Some(slot) -> emit_scratch_get(e, slot)
    None -> e
  }
}

// slots already seeded at block entry; just init the function bindings
fn emit_block_declarations(
  e: Emitter,
  body: List(ast.StmtWithLine),
) -> Result(Emitter, EmitError) {
  use #(funcs, e) <- result.map(collect_hoisted_funcs(e, body))
  list.fold(funcs, e, fn(e, hf) {
    let #(name, idx) = hf
    let e = emit_op(e, opcode.MakeClosure(idx))
    emit_var_init(e, name)
  })
}

// annex b §B.3.1: function as if/else clause acts as a block
fn block_wrap_fn_decl(stmt: ast.Statement) -> ast.Statement {
  case stmt {
    ast.FunctionDeclaration(..) ->
      ast.BlockStatement([ast.StmtWithLine(0, stmt)])
    _ -> stmt
  }
}

fn emit_if(
  e: Emitter,
  condition: ast.Expression,
  consequent: ast.Statement,
  alternate: Option(ast.Statement),
  branch: fn(Emitter, ast.Statement) -> Result(Emitter, EmitError),
  none: fn(Emitter) -> Emitter,
) -> Result(Emitter, EmitError) {
  let #(else_label, e) = fresh_label(e)
  let #(end_label, e) = fresh_label(e)
  use e <- result.try(emit_test(e, condition, jump_when: False, to: else_label))
  use e <- result.try(branch(e, block_wrap_fn_decl(consequent)))
  let e = emit_ir(e, IrJump(end_label))
  let e = emit_ir(e, IrLabel(else_label))
  use e <- result.try(case alternate {
    Some(alt) -> branch(e, block_wrap_fn_decl(alt))
    None -> Ok(none(e))
  })
  Ok(emit_ir(e, IrLabel(end_label)))
}

fn emit_try_catch(
  e: Emitter,
  block: List(ast.StmtWithLine),
  clause: ast.CatchClause,
  tail tail: Bool,
) -> Result(Emitter, EmitError) {
  let ast.CatchClause(param:, body:) = clause
  let #(catch_label, e) = fresh_label(e)
  let #(end_label, e) = fresh_label(e)

  let e = emit_ir(e, IrPushTry(catch_label, CatchOnly))
  let e = push_barrier(e, pop_try: 1, label_finally: None, drop: 0)
  use e <- result.try(emit_block(e, block, tail:))
  let e = pop_frame(e)
  let e = emit_op(e, opcode.PopTry)
  let e = emit_ir(e, IrJump(end_label))

  use e <- result.map(
    emit_catch_clause(e, catch_label, param, emit_block(_, body, tail:)),
  )
  emit_ir(e, IrLabel(end_label))
}

// catch without a binding has no scope; entering one would desync the cursor
fn emit_catch_clause(
  e: Emitter,
  catch_label: LabelId,
  param: Option(ast.Pattern),
  emit_body: fn(Emitter) -> Result(Emitter, EmitError),
) -> Result(Emitter, EmitError) {
  let e = emit_ir(e, IrLabel(catch_label))
  case param {
    Some(pattern) -> {
      use e <- in_child_scope(e, in_block: e.in_block)
      use e <- result.try(emit_destructuring_bind(e, pattern, CatchBinding))
      emit_body(e)
    }
    None -> emit_body(emit_op(e, opcode.Pop))
  }
}

fn emit_stmts_tail(
  e: Emitter,
  stmts: List(ast.StmtWithLine),
) -> Result(Emitter, EmitError) {
  // §14 completion value comes from the last value-producing statement
  let #(vacuous_rev, before_rev) =
    list.reverse(stmts)
    |> list.split_while(fn(s) { has_empty_completion(s.statement) })
  case vacuous_rev {
    [] -> emit_stmts_tail_value(e, stmts)
    _ -> {
      use e <- result.try(emit_stmts_tail_value(e, list.reverse(before_rev)))
      list.try_fold(list.reverse(vacuous_rev), e, fn(e, s) {
        emit_stmt(set_line(e, s.line), s.statement)
      })
    }
  }
}

fn has_empty_completion(stmt: ast.Statement) -> Bool {
  case stmt {
    ast.VariableDeclaration(..)
    | ast.FunctionDeclaration(..)
    | ast.ClassDeclaration(..)
    | ast.EmptyStatement
    | ast.DebuggerStatement -> True
    _ -> False
  }
}

fn emit_stmts_tail_value(
  e: Emitter,
  stmts: List(ast.StmtWithLine),
) -> Result(Emitter, EmitError) {
  case stmts {
    [] -> Ok(push_const(e, mk_undefined()))
    [only] -> emit_stmt_tail(set_line(e, only.line), only.statement)
    [first, ..rest] -> {
      use e <- result.try(emit_stmt(set_line(e, first.line), first.statement))
      emit_stmts_tail_value(e, rest)
    }
  }
}

fn collect_hoisted_funcs(
  e: Emitter,
  stmts: List(ast.StmtWithLine),
) -> Result(#(List(#(String, Int)), Emitter), EmitError) {
  use #(funcs_rev, e) <- result.map(
    list.try_fold(stmts, #([], e), fn(acc, located) {
      let #(funcs, e) = acc
      case ast_util.peel_labels(located.statement) {
        ast.FunctionDeclaration(
          Some(ast.NamedBinding(name, _)),
          params,
          body,
          is_generator,
          is_async,
        ) -> {
          use #(child, e) <- result.map(compile_function_body(
            e,
            Some(name),
            params,
            StmtsBody(body),
            shape: FnDecl(is_generator:, is_async:),
          ))
          let #(idx, e) = add_child_function(e, child)
          #([#(name, idx), ..funcs], e)
        }
        _ -> Ok(#(funcs, e))
      }
    }),
  )
  #(list.reverse(funcs_rev), e)
}

fn emit_hoisted_funcs(
  e: Emitter,
  hoisted_funcs: List(#(String, Int)),
) -> Emitter {
  list.fold(hoisted_funcs, e, fn(e, hf) {
    let #(name, func_idx) = hf
    e |> emit_op(opcode.MakeClosure(func_idx)) |> emit_var_put(name)
  })
}

// §10.2.11 28.f.i.2: a body var shadowing a param or arguments copies it
fn emit_body_param_copies(
  e: Emitter,
  fn_scope_id: ScopeId,
  parameter_bindings: List(String),
  stmts: List(ast.StmtWithLine),
) -> Emitter {
  let body_id = e.current_scope
  // defensive: cursor fallback left us at the fn scope
  use <- bool.guard(body_id == fn_scope_id, e)
  let function_names = ast_util.top_level_function_names(stmts)
  let body_bindings = bindings_in_slot_order(scope.get(e.scope_tree, body_id))
  use e, #(bname, b) <- list.fold(body_bindings, e)
  let copies =
    b.kind == VarBinding
    && list.contains(parameter_bindings, bname)
    && !list.contains(function_names, bname)
  use <- bool.guard(!copies, e)
  case scope.lookup(e.scope_tree, fn_scope_id, bname) {
    scope.Plain(scope.Local(..) as source) -> {
      let e = track_arguments_ref(e, bname)
      emit_target_get(e, source) |> emit_slot_put(scope.binding_ref(b))
    }
    scope.Plain(scope.Global(_))
    | scope.Plain(scope.EvalEnv(_))
    | scope.WithChain(..) -> e
  }
}

type FunctionShape {
  FnDecl(is_generator: Bool, is_async: Bool)
  // self name only when syntactically named
  FnExpr(self_name: Option(String), is_generator: Bool, is_async: Bool)
  Arrow(is_async: Bool)
  Method(is_generator: Bool, is_async: Bool)
  ClassCtor(is_derived: Bool, field_init: FieldInitMode)
  ClassInitFn
}

type FnTraits {
  FnTraits(
    is_arrow: Bool,
    is_generator: Bool,
    is_async: Bool,
    is_constructor: Bool,
    self_name: Option(String),
  )
}

fn traits_of(shape: FunctionShape) -> FnTraits {
  let plain =
    FnTraits(
      is_arrow: False,
      is_generator: False,
      is_async: False,
      is_constructor: False,
      self_name: None,
    )
  case shape {
    FnDecl(is_generator:, is_async:) ->
      FnTraits(
        ..plain,
        is_generator:,
        is_async:,
        is_constructor: !is_generator && !is_async,
      )
    FnExpr(self_name:, is_generator:, is_async:) ->
      FnTraits(
        ..plain,
        is_generator:,
        is_async:,
        is_constructor: !is_generator && !is_async,
        self_name:,
      )
    Arrow(is_async:) -> FnTraits(..plain, is_arrow: True, is_async:)
    Method(is_generator:, is_async:) ->
      FnTraits(..plain, is_generator:, is_async:)
    ClassCtor(..) -> FnTraits(..plain, is_constructor: True)
    ClassInitFn -> plain
  }
}

type ParamLayout {
  ParamLayout(
    fixed: List(ast.Pattern),
    rest: Option(ast.Pattern),
    // must agree with ast_util.all_simple_params
    non_simple_fixed: Bool,
    declared_names: List(String),
    // declared names plus the implicit arguments binding for non-arrows
    bindings: List(String),
  )
}

fn param_layout(
  params: List(ast.Pattern),
  is_arrow is_arrow: Bool,
) -> ParamLayout {
  let #(fixed, rest) = ast_util.split_trailing_rest(params)
  let declared_names = list.flat_map(params, ast.pattern_bound_names)
  let bindings = case is_arrow {
    True -> declared_names
    False -> ["arguments", ..declared_names]
  }
  ParamLayout(
    fixed:,
    rest:,
    non_simple_fixed: !ast_util.all_simple_params(fixed),
    declared_names:,
    bindings:,
  )
}

// §15.1.5 length: params before the first default, rest excluded
fn expected_argument_count(fixed_params: List(ast.Pattern)) -> Int {
  fixed_params
  |> list.take_while(fn(p) {
    case p {
      ast.AssignmentPattern(..) -> False
      _ -> True
    }
  })
  |> list.length
}

fn compile_function_body(
  parent: Emitter,
  name: Option(String),
  params: List(ast.Pattern),
  body: FnBody,
  shape shape: FunctionShape,
) -> Result(#(CompiledChild, Emitter), EmitError) {
  let FnTraits(is_arrow:, is_generator:, is_async:, is_constructor:, self_name:) =
    traits_of(shape)
  // exhausted cursor means analyzer walk-order desync, crash
  let assert [fn_id, ..rest] = parent.child_fn_cursor
  let parent = Emitter(..parent, child_fn_cursor: rest)
  let stmts = case body {
    StmtsBody(stmts:) -> stmts
    FieldInitsBody(..) -> []
  }
  let body_has_using = ast_util.has_using_decl(stmts)

  let child_strict = parent.strict || ast_util.has_use_strict_directive(stmts)

  let code_kind = case shape {
    Arrow(..) -> parent.code_kind
    FnDecl(..) | FnExpr(..) -> lexical.FunctionCode
    Method(..) -> lexical.MethodCode
    ClassCtor(is_derived: True, ..) -> lexical.DerivedCtorCode
    ClassCtor(is_derived: False, ..) -> lexical.MethodCode
    ClassInitFn -> lexical.FieldInitCode
  }
  // only FieldInitAfterSuper is inherited by arrows; AtStart would re-run per call
  let field_init = case shape, parent.field_init {
    Arrow(..), FieldInitAfterSuper -> FieldInitAfterSuper
    Arrow(..), FieldInitAtStart | Arrow(..), NoFieldInit -> NoFieldInit
    ClassCtor(field_init:, ..), _ -> field_init
    FnDecl(..), _ | FnExpr(..), _ | Method(..), _ | ClassInitFn, _ ->
      NoFieldInit
  }

  let e =
    Emitter(
      ..new_emitter(parent.scope_tree, fn_id),
      next_label: parent.next_label,
      next_site: parent.next_site,
      strict: child_strict,
      is_async:,
      is_arrow:,
      code_kind:,
      field_init:,
      with_stack: parent.with_stack,
      private_env: parent.private_env,
      in_implicit_derived_ctor: parent.in_implicit_derived_ctor,
    )
  let e = enter_root_scope(e)

  let layout = param_layout(params, is_arrow)

  // arguments setup is spliced in here later, once we know it is referenced
  let pre_args_code = e.code
  let e = Emitter(..e, code: [])

  let e = case self_name {
    Some(fname) -> emit_self_name_binding(e, fname, layout, stmts)
    None -> e
  }

  use e <- result.try(emit_parameter_bindings(e, layout))

  // §10.2.11 step 28: non-simple params get their own body var scope
  let #(body_save, e) = case layout.non_simple_fixed {
    False -> #(None, e)
    True -> {
      let #(save, e) = enter_scope(e, in_block: e.in_block)
      let e = emit_body_param_copies(e, save.scope, layout.bindings, stmts)
      #(Some(save), e)
    }
  }

  use #(hoisted_funcs, e) <- result.try(collect_hoisted_funcs(e, stmts))
  let e = emit_hoisted_funcs(e, hoisted_funcs)

  // generators suspend here before the body; async runs eagerly
  let e = case is_generator {
    True -> emit_op(e, opcode.InitialYield)
    False -> e
  }

  let e = case field_init {
    FieldInitAtStart -> emit_field_init_call(e)
    NoFieldInit | FieldInitAfterSuper -> e
  }

  use e <- result.try(case body, body_has_using {
    FieldInitsBody(inits:), _ -> list.try_fold(inits, e, emit_field_init)
    StmtsBody(..), False -> emit_stmts(e, stmts)
    StmtsBody(..), True -> {
      // directives stay outside the dispose try
      let #(directives, rest) = ast_util.split_directives(stmts)
      use e <- result.try(emit_stmts(e, directives))
      let #(slots, items, e) = build_using_slots(e, rest)
      let e = emit_using_prelude(e, slots)
      use e <- emit_using_try_wrap(e, slots)
      emit_using_body(e, items)
    }
  })

  let e = case body_save {
    Some(save) -> leave_scope(e, save)
    None -> e
  }

  let e = push_const(e, mk_undefined())
  let e = emit_op(e, opcode.Return)
  let e = splice_arguments_setup(e, layout, pre_args_code)
  let Finished(code:, constants:, children:) = finish(e)

  let child =
    CompiledChild(
      scope_id: e.fn_scope,
      name:,
      arity: list.length(layout.fixed),
      length: expected_argument_count(layout.fixed),
      code:,
      constants:,
      functions: children,
      is_strict: child_strict,
      is_arrow:,
      is_derived_constructor: False,
      is_generator:,
      is_async:,
      is_constructor:,
      is_class_constructor: False,
      lexical_refs: e.lexical_refs,
      references_arguments: e.references_arguments,
      code_kind:,
    )
  // take the child's tree back: it holds scratch-slot local_count bumps
  Ok(#(
    child,
    Emitter(..parent, scope_tree: e.scope_tree, next_site: e.next_site),
  ))
}

// §13.2.5.5 nfe self-name, skipped when shadowed by params/vars/functions
fn emit_self_name_binding(
  e: Emitter,
  fname: String,
  layout: ParamLayout,
  stmts: List(ast.StmtWithLine),
) -> Emitter {
  // B.3.3.1 annex b promotion also shadows (sloppy function code only)
  let annexb_shadow =
    !e.strict && list.contains(fn_info(e).annexb_candidates, fname)
  let shadowed =
    list.contains(layout.declared_names, fname)
    || fname == "arguments"
    || list.contains(ast_util.var_scoped_names(stmts), fname)
    || list.any(ast_util.lexically_declared_names(stmts), fn(lex) {
      lex.0 == fname
    })
    || annexb_shadow
  case shadowed {
    True -> e
    False -> emit_lexical_get(e, lexical.ActiveFuncRef) |> emit_var_init(fname)
  }
}

// non-simple lists bind args to <paramN> shims, then real names init in order
fn emit_parameter_bindings(
  e: Emitter,
  layout: ParamLayout,
) -> Result(Emitter, EmitError) {
  // param_scope_names lets direct eval in defaults do the §19.2.1.1 3.d check
  let e = Emitter(..e, param_scope_names: layout.bindings)
  let shim_params = case layout.non_simple_fixed {
    False -> []
    True ->
      list.index_map(layout.fixed, fn(param, idx) {
        #(scope.param_shim(idx), param)
      })
  }
  use e <- result.try(
    list.try_fold(shim_params, e, fn(e, pair) {
      let #(shim, pattern) = pair
      emit_var_get(e, shim) |> emit_destructuring_bind(pattern, LetBinding)
    }),
  )
  use e <- result.map(case layout.rest {
    None -> Ok(e)
    Some(rest_target) -> {
      let e = emit_op(e, opcode.CreateRestArray(list.length(layout.fixed)))
      let rest_kind = case layout.non_simple_fixed {
        True -> LetBinding
        False -> ParamBinding
      }
      emit_destructuring_bind(e, rest_target, rest_kind)
    }
  })
  Emitter(..e, param_scope_names: [])
}

// after the body, once arguments references are known
fn splice_arguments_setup(
  e: Emitter,
  layout: ParamLayout,
  pre_args_code: List(IrOp),
) -> Emitter {
  let uses_args = !e.is_arrow && e.references_arguments
  let put_args = case scope.lookup(e.scope_tree, e.fn_scope, "arguments") {
    scope.Plain(scope.Local(slot:, boxed: True, ..)) ->
      IrFinal(opcode.PutBoxed(slot))
    scope.Plain(scope.Local(slot:, boxed: False, ..)) ->
      IrFinal(opcode.PutLocal(slot))
    scope.Plain(scope.Global(_))
    | scope.Plain(scope.EvalEnv(_))
    | scope.WithChain(..) -> IrFinal(opcode.PutGlobal("arguments"))
  }
  let simple_params = !layout.non_simple_fixed && layout.rest == None
  let forward = uses_args && !e.arguments_escape && !fn_info(e).eval_in_subtree
  let code = rewrite_apply_arguments(e.code, forward:, simple_params:)
  let args_setup_rev = case uses_args && !forward {
    True -> [put_args, IrFinal(opcode.CreateArguments(simple_params:))]
    False -> []
  }
  Emitter(..e, code: list.flatten([code, args_setup_rev, pre_args_code]))
}

// postfix x++ for effect takes the prefix lowering
fn for_effect(expr: ast.Expression) -> ast.Expression {
  case expr {
    ast.UpdateExpression(span, op, False, arg) ->
      ast.UpdateExpression(span, op, prefix: True, argument: arg)
    ast.SequenceExpression(span, exprs) ->
      case list.reverse(exprs) {
        [last, ..init] ->
          ast.SequenceExpression(span, list.reverse([for_effect(last), ..init]))
        [] -> expr
      }
    _ -> expr
  }
}

fn emit_stmts(
  e: Emitter,
  stmts: List(ast.StmtWithLine),
) -> Result(Emitter, EmitError) {
  list.try_fold(stmts, e, fn(e, located) {
    emit_stmt(set_line(e, located.line), located.statement)
  })
}

fn emit_stmt(e: Emitter, stmt: ast.Statement) -> Result(Emitter, EmitError) {
  // completion mode: loops/if/switch reset V to undefined on entry
  let e = case e.completion_var {
    Some(v) ->
      case stmt {
        ast.WhileStatement(..)
        | ast.DoWhileStatement(..)
        | ast.ForStatement(..)
        | ast.ForInStatement(..)
        | ast.ForOfStatement(..)
        | ast.IfStatement(..)
        | ast.SwitchStatement(..)
        | ast.TryStatement(..)
        | ast.WithStatement(..) -> {
          let e = push_const(e, mk_undefined())
          emit_scratch_put(e, v)
        }
        _ -> e
      }
    None -> e
  }
  case stmt {
    ast.EmptyStatement | ast.DebuggerStatement -> Ok(e)

    ast.ExpressionStatement(expression: expr, ..) ->
      case e.completion_var {
        Some(v) -> result.map(emit_expr(e, expr), emit_scratch_put(_, v))
        None ->
          result.map(emit_expr(e, for_effect(expr)), emit_op(_, opcode.Pop))
      }

    ast.BlockStatement(body) -> emit_block(e, body, tail: False)

    // using never reaches here, emit_using_body handles it
    ast.VariableDeclaration(kind, declarators) ->
      list.try_fold(declarators, e, fn(e, decl) {
        emit_variable_declarator(e, kind, decl)
      })

    ast.IfStatement(cond, cons, alt) ->
      emit_if(e, cond, cons, alt, emit_stmt, fn(e) { e })

    ast.WhileStatement(condition, body) ->
      emit_classic_loop(e, Some(condition), None, body, [])

    ast.DoWhileStatement(condition, body) -> {
      let #(loop_start, e) = fresh_label(e)
      let #(loop_cond, e) = fresh_label(e)
      let #(loop_end, e) = fresh_label(e)
      let e = push_loop(e, loop_end, loop_cond, NoIter)
      let e = emit_ir(e, IrLabel(loop_start))
      use e <- result.try(emit_stmt(e, body))
      let e = emit_ir(e, IrLabel(loop_cond))
      use e <- result.map(emit_test(
        e,
        condition,
        jump_when: True,
        to: loop_start,
      ))
      e |> emit_ir(IrLabel(loop_end)) |> pop_frame
    }

    ast.ForStatement(
      init: Some(ast.ForInitDeclaration(kind: ast.Using as kind, declarations:)),
      condition:,
      update:,
      body:,
    )
    | ast.ForStatement(
        init: Some(ast.ForInitDeclaration(
          kind: ast.AwaitUsing as kind,
          declarations:,
        )),
        condition:,
        update:,
        body:,
      ) ->
      emit_for_using_classic(e, kind, declarations, condition, update, body)

    ast.ForStatement(init, condition, update, body) -> {
      let #(save, e) =
        enter_for_scope(e, ast_util.for_classic_init_is_lex(init))
      use #(per_iter, e) <- result.try(case init {
        Some(ast.ForInitExpression(expr)) -> {
          use e <- result.map(emit_expr(e, expr))
          #([], emit_op(e, opcode.Pop))
        }
        Some(ast.ForInitDeclaration(kind:, declarations:)) -> {
          use e <- result.map(emit_stmt(
            e,
            ast.VariableDeclaration(kind:, declarations:),
          ))
          #(ast_util.for_let_names(kind, declarations), e)
        }
        Some(ast.ForInitPattern(_)) | None -> Ok(#([], e))
      })
      use e <- result.map(emit_classic_loop(
        e,
        condition,
        update,
        body,
        per_iter,
      ))
      leave_for_scope(e, save)
    }

    ast.ReturnStatement(arg) -> {
      use e <- result.try(case arg {
        Some(expr) -> emit_expr(e, expr)
        None -> Ok(push_const(e, mk_undefined()))
      })
      let e =
        list.fold(e.frame_stack, e, fn(e, frame) {
          emit_cross_frame(e, frame, value_on_top: True)
        })
      Ok(emit_op(e, opcode.Return))
    }

    ast.ThrowStatement(arg) -> {
      use e <- result.map(emit_expr(e, arg))
      emit_op(e, opcode.Throw)
    }

    ast.TryStatement(block, handler) -> emit_try(e, block, handler)

    ast.SwitchStatement(discriminant, cases) ->
      emit_switch(e, discriminant, cases)

    ast.BreakStatement(name) -> emit_goto_loop(e, name, is_continue: False)

    ast.ContinueStatement(name) -> emit_goto_loop(e, name, is_continue: True)

    ast.LabeledStatement(label, body) -> {
      case body {
        ast.WhileStatement(..)
        | ast.DoWhileStatement(..)
        | ast.ForStatement(..)
        | ast.ForInStatement(..)
        | ast.ForOfStatement(..) -> {
          let e = Emitter(..e, pending_label: Some(label))
          emit_stmt(e, body)
        }
        _ -> {
          let #(break_target, e) = fresh_label(e)
          let e = push_frame(e, LabeledBlockFrame(break_target:, label:))
          use e <- result.map(emit_stmt(e, body))
          let e = pop_frame(e)
          emit_ir(e, IrLabel(break_target))
        }
      }
    }

    ast.FunctionDeclaration(name, _, _, is_generator, is_async) -> {
      // closure was made at hoist time; only the §B.3.2.6 copy happens here
      case name {
        Some(ast.NamedBinding(name: fname, ..)) -> {
          let promote =
            e.in_block
            && !e.strict
            && !is_generator
            && !is_async
            && !is_annexb_blocked(e, fname)
          case promote {
            True -> Ok(emit_annexb_promote(e, fname))
            False -> Ok(e)
          }
        }
        None -> Ok(e)
      }
    }

    ast.ClassDeclaration(name, super_class, body) -> {
      case name {
        Some(ast.NamedBinding(name: n, ..)) -> {
          let e = declare_lex(e, n, is_const: False)
          use e <- result.map(compile_class(
            e,
            Some(n),
            Some(n),
            super_class,
            body,
          ))
          init_lex(e, n)
        }
        None -> Error(AnonymousClassDeclaration)
      }
    }

    ast.ForInStatement(left, right, body) -> emit_for_in(e, left, right, body)

    ast.ForOfStatement(left, right, body, is_await) ->
      case is_await {
        False -> emit_for_of(e, left, right, body)
        True -> emit_for_await_of(e, left, right, body)
      }

    ast.WithStatement(object, body) -> emit_with(e, object, body, tail: False)
  }
}

fn emit_variable_declarator(
  e: Emitter,
  kind: ast.VariableKind,
  decl: ast.VariableDeclarator,
) -> Result(Emitter, EmitError) {
  let is_lexical = ast_util.is_lexical(kind)
  case decl {
    ast.VariableDeclarator(ast.IdentifierPattern(name, ..), init) -> {
      let e = case kind {
        ast.Let -> declare_lex(e, name, is_const: False)
        ast.Const | ast.Using | ast.AwaitUsing ->
          declare_lex(e, name, is_const: True)
        ast.Var -> e
      }
      case init, is_lexical {
        Some(init_expr), _ -> {
          use e <- result.map(emit_named_expr(e, init_expr, name))
          case is_lexical {
            False -> emit_var_put(e, name)
            True -> init_lex(e, name)
          }
        }
        // let x; initializes to undefined (§14.3.1.2), else tdz forever
        None, True -> Ok(init_lex(push_const(e, mk_undefined()), name))
        None, False -> Ok(e)
      }
    }
    ast.VariableDeclarator(pattern, init) -> {
      use e <- result.try(case init {
        Some(init_expr) -> emit_expr(e, init_expr)
        None -> Ok(push_const(e, mk_undefined()))
      })
      emit_destructuring_bind(e, pattern, ast_util.binding_kind_of(kind))
    }
  }
}

fn emit_try(
  e: Emitter,
  block: List(ast.StmtWithLine),
  handler: ast.TryTail,
) -> Result(Emitter, EmitError) {
  case handler {
    ast.TryCatch(clause) -> emit_try_catch(e, block, clause, tail: False)

    ast.TryFinally(finally_body) -> {
      let #(throw_label, e) = fresh_label(e)
      let #(fin_label, e) = fresh_label(e)
      let #(end_label, e) = fresh_label(e)

      let e = emit_ir(e, IrPushTry(throw_label, Finally(fin_label)))
      let e =
        push_barrier(e, pop_try: 1, label_finally: Some(fin_label), drop: 0)
      use e <- result.try(emit_block(e, block, tail: False))
      let e = pop_frame(e)
      let e = emit_op(e, opcode.PopTry)
      let e = emit_gosub_normal(e, fin_label)
      let e = emit_ir(e, IrJump(end_label))

      use e <- result.map(
        emit_finally_subroutine(e, throw_label, fin_label, emit_block(
          _,
          finally_body,
          tail: False,
        )),
      )
      emit_ir(e, IrLabel(end_label))
    }

    // two PushTry up front so catch-param destructuring throws also reach finally
    ast.TryCatchFinally(ast.CatchClause(param, catch_body), finally_body) -> {
      use e, catch_label, fin_label <- emit_try_catch_finally(
        e,
        emit_block(_, block, tail: False),
        emit_block(_, finally_body, tail: False),
      )
      use e <- emit_catch_clause(e, catch_label, param)
      let e =
        push_barrier(e, pop_try: 1, label_finally: Some(fin_label), drop: 0)
      use e <- result.map(emit_block(e, catch_body, tail: False))
      pop_frame(e)
    }
  }
}

// §14.11 with: head object stored in the analyzer's holder slot
fn emit_with(
  e: Emitter,
  object: ast.Expression,
  body: ast.Statement,
  tail tail: Bool,
) -> Result(Emitter, EmitError) {
  use e <- result.try(emit_expr(e, object))
  let e = emit_op(e, opcode.ToObject)
  let #(save, e) = enter_scope(e, in_block: e.in_block)
  // non-With kind here means cursor desync, crash
  let with_scope = scope.get(e.scope_tree, e.current_scope)
  let assert scope.With(holder: synth) = with_scope.kind
    as "emit_with: emitter cursor is not on the analyzer's With scope"
  let assert Ok(holder) = dict.get(with_scope.bindings, synth)
    as "emit_with: With scope is missing its holder binding"
  let e = Emitter(..e, initialized: set.insert(e.initialized, holder.slot))
  let e = emit_slot_put(e, scope.binding_ref(holder))
  let e = Emitter(..e, with_stack: [synth, ..e.with_stack])
  use e <- result.map(case tail {
    True -> emit_stmt_tail(e, body)
    False -> emit_stmt(e, body)
  })
  let assert [_, ..with_rest] = e.with_stack
  leave_scope(Emitter(..e, with_stack: with_rest), save)
}

// one short-circuit exit per stack shape: [base] or [f, receiver]
type ChainExits {
  ChainExits(nullish_with_base: LabelId, nullish_callee_pair: LabelId)
}

// §13.3.9.1 the whole chain yields undefined at the first nullish link
fn emit_chain_root(
  e: Emitter,
  expr: ast.Expression,
) -> Result(Emitter, EmitError) {
  let #(nullish_with_base, e) = fresh_label(e)
  let #(nullish_callee_pair, e) = fresh_label(e)
  let #(end_label, e) = fresh_label(e)
  let exits = ChainExits(nullish_with_base:, nullish_callee_pair:)
  use e <- result.map(emit_chain(e, expr, exits))
  let e = emit_ir(e, IrJump(end_label))
  // [nullish] -> [undefined]
  let e = emit_ir(e, IrLabel(nullish_with_base))
  let e = emit_op(e, opcode.Pop)
  let e = push_const(e, mk_undefined())
  let e = emit_ir(e, IrJump(end_label))
  // [f, receiver] -> [undefined]
  let e = emit_ir(e, IrLabel(nullish_callee_pair))
  let e = emit_op(e, opcode.Pop)
  let e = emit_op(e, opcode.Pop)
  let e = push_const(e, mk_undefined())
  emit_ir(e, IrLabel(end_label))
}

fn emit_chain(
  e: Emitter,
  expr: ast.Expression,
  exits: ChainExits,
) -> Result(Emitter, EmitError) {
  use <- bool.lazy_guard(!ast_util.chain_has_optional(expr), fn() {
    emit_expr(e, expr)
  })
  case expr {
    ast.MemberExpression(_, obj, ast.Dot(name:, ..))
    | ast.OptionalMemberExpression(_, obj, ast.Dot(name:, ..)) -> {
      use e <- result.map(emit_chain_object(e, expr, obj, exits))
      emit_get_field(e, name)
    }
    ast.MemberExpression(_, obj, ast.Bracket(prop))
    | ast.OptionalMemberExpression(_, obj, ast.Bracket(prop)) -> {
      use e <- result.try(emit_chain_object(e, expr, obj, exits))
      use e <- result.map(emit_expr(e, prop))
      emit_op(e, opcode.GetElem)
    }
    // non-optional call after an optional link, e.g. a?.b.m(x)
    ast.CallExpression(_, callee, args) -> {
      use #(form, e) <- result.try(emit_chain_callee(e, callee, exits))
      emit_call_args(e, args, form)
    }
    // f?.(x): also check the function value
    ast.OptionalCallExpression(_, callee, args) -> {
      use #(form, e) <- result.try(emit_chain_callee(e, callee, exits))
      let e = emit_op(e, opcode.Dup)
      let exit = case form {
        MethodCall -> exits.nullish_callee_pair
        PlainCall | Construct | NewCall -> exits.nullish_with_base
      }
      let e = emit_ir(e, IrJumpIfNullish(exit))
      emit_call_args(e, args, form)
    }
    // unreachable: ?. followed by a template is an early error
    other -> emit_expr(e, other)
  }
}

fn emit_chain_object(
  e: Emitter,
  link: ast.Expression,
  obj: ast.Expression,
  exits: ChainExits,
) -> Result(Emitter, EmitError) {
  use e <- result.map(emit_chain(e, obj, exits))
  case link {
    ast.OptionalMemberExpression(..) ->
      e
      |> emit_op(opcode.Dup)
      |> emit_ir(IrJumpIfNullish(exits.nullish_with_base))
    _ -> e
  }
}

// MethodCall when the stack is [f, receiver], PlainCall when [f]
fn emit_chain_callee(
  e: Emitter,
  callee: ast.Expression,
  exits: ChainExits,
) -> Result(#(CallForm, Emitter), EmitError) {
  case callee {
    ast.MemberExpression(_, ast.SuperExpression(_), property) -> {
      use e <- result.map(emit_super_method_ref(e, property))
      #(MethodCall, e)
    }
    ast.MemberExpression(_, obj, ast.Dot(name:, ..))
    | ast.OptionalMemberExpression(_, obj, ast.Dot(name:, ..)) -> {
      use e <- result.map(emit_chain_object(e, callee, obj, exits))
      #(MethodCall, emit_get_field_keep(e, name))
    }
    ast.MemberExpression(_, obj, ast.Bracket(key))
    | ast.OptionalMemberExpression(_, obj, ast.Bracket(key)) -> {
      use e <- result.try(emit_chain_object(e, callee, obj, exits))
      use e <- result.map(emit_expr(e, key))
      #(MethodCall, emit_get_elem_method(e))
    }
    other -> {
      use e <- result.map(emit_chain(e, other, exits))
      #(PlainCall, e)
    }
  }
}

// [key, obj] -> GetElemKeep [method, key, obj] -> [method, obj]
fn emit_get_elem_method(e: Emitter) -> Emitter {
  e
  |> emit_op(opcode.GetElemKeep)
  |> emit_op(opcode.Swap)
  |> emit_op(opcode.Pop)
}

fn emit_expr(e: Emitter, expr: ast.Expression) -> Result(Emitter, EmitError) {
  case expr {
    ast.NumberLiteral(_, value) ->
      Ok(push_const(e, const_fold.number_const(value)))
    ast.BigIntLiteral(value: n, ..) -> Ok(push_const(e, mk_bigint(n)))
    ast.StringLiteral(_, value) -> Ok(push_const(e, mk_string(value)))
    ast.BooleanLiteral(_, value) -> Ok(push_const(e, mk_bool(value)))
    ast.NullLiteral(_) -> Ok(push_const(e, mk_null()))
    ast.UndefinedExpression(_) -> Ok(push_const(e, mk_undefined()))

    ast.Identifier(name: "undefined", ..) -> Ok(push_const(e, mk_undefined()))
    // bare #x outside "#x in obj" is an early error (§13.10.1)
    ast.Identifier(name: "#" <> rest, ..) ->
      Error(EarlySyntaxError("Unexpected private name #" <> rest))
    ast.Identifier(name:, ..) -> Ok(emit_var_get(e, name))

    // #x in obj: lhs is a name, emit rhs then PrivateInDyn
    ast.BinaryExpression(
      _,
      ast.In,
      ast.Identifier(name: "#" <> rest, ..),
      right,
    ) -> {
      use e <- result.map(emit_expr(e, right))
      e
      |> emit_var_get("#" <> rest)
      |> emit_op(opcode.PrivateInDyn)
    }
    ast.BinaryExpression(_, op, left, right) -> {
      use e <- result.try(emit_expr(e, left))
      use e <- result.map(emit_expr(e, right))
      emit_ir(e, IrBinOp(const_fold.translate_binop(op)))
    }

    ast.LogicalExpression(_, op, left, right) -> {
      let #(end_label, e) = fresh_label(e)
      use e <- result.try(emit_expr(e, left))
      let e = emit_short_circuit_test(e, op, end_label)
      let e = emit_op(e, opcode.Pop)
      use e <- result.map(emit_expr(e, right))
      emit_ir(e, IrLabel(end_label))
    }

    // typeof (x) === typeof x, so unwrap parens
    ast.UnaryExpression(_, ast.TypeOf, arg) ->
      case ast_util.unwrap_parens(arg) {
        ast.Identifier(name:, ..) -> {
          Ok(emit_var_typeof(e, name))
        }
        _ -> {
          use e <- result.map(emit_expr(e, arg))
          emit_op(e, opcode.TypeOf)
        }
      }

    ast.UnaryExpression(_, ast.Delete, arg) -> emit_delete(e, arg)

    ast.UnaryExpression(_, op, arg) -> {
      use kind <- result.try(
        const_fold.translate_unaryop(op)
        |> option.to_result(NonGenericUnaryOperator),
      )
      case const_fold.fold_unary(op, arg) {
        Some(value) -> Ok(push_const(e, value))
        None -> {
          use e <- result.map(emit_expr(e, arg))
          emit_op(e, opcode.UnaryOp(kind))
        }
      }
    }

    ast.UpdateExpression(_, op, prefix, argument) ->
      emit_update(e, op, prefix:, argument: ast_util.unwrap_parens(argument))

    ast.AssignmentExpression(_, op, target, right) ->
      emit_assignment(e, op, target, right)

    ast.CallExpression(_, callee, args) -> emit_call(e, expr, callee, args)

    ast.ConditionalExpression(_, condition, consequent, alternate) -> {
      let #(else_label, e) = fresh_label(e)
      let #(end_label, e) = fresh_label(e)
      use e <- result.try(emit_test(
        e,
        condition,
        jump_when: False,
        to: else_label,
      ))
      use e <- result.try(emit_expr(e, consequent))
      let e = emit_ir(e, IrJump(end_label))
      let e = emit_ir(e, IrLabel(else_label))
      use e <- result.map(emit_expr(e, alternate))
      emit_ir(e, IrLabel(end_label))
    }

    ast.SequenceExpression(_, exprs) -> emit_sequence(e, exprs)

    // leading static-key data props go into one NewObjectWith
    ast.ObjectExpression(_, properties) -> {
      let LiteralHead(keys:, members:, rest:) =
        literal_head(properties, [], [], set.new())
      case members {
        [] -> {
          let e = emit_op(e, opcode.NewObject)
          list.try_fold(properties, e, emit_object_property)
        }
        _ -> {
          use e <- result.try(
            list.try_fold(members, e, fn(e, member) {
              emit_named_expr(e, member.value, member.name)
            }),
          )
          let e = emit_op(e, opcode.NewObjectWith(keys, list.length(keys)))
          list.try_fold(rest, e, emit_object_property)
        }
      }
    }

    ast.MemberExpression(_, object, property) ->
      emit_member(e, expr, object, property)

    ast.OptionalMemberExpression(..) | ast.OptionalCallExpression(..) ->
      emit_chain_root(e, expr)

    // no spread: ArrayFrom / ArrayFromWithHoles; spread: incremental pushes
    ast.ArrayExpression(_, elements) ->
      case ast_util.has_spread_element(elements) {
        False -> emit_array_no_spread(e, elements)
        True -> emit_array_with_spread(e, elements)
      }

    ast.FunctionExpression(_, name, params, body, is_generator, is_async) ->
      emit_function_closure(
        e,
        ast.binding_name(name),
        params,
        body,
        is_generator,
        is_async,
        bind_self: True,
      )

    ast.ArrowFunctionExpression(_, params, body, is_async) ->
      emit_arrow_closure(e, None, params, body, is_async)

    ast.ThisExpression(_) -> Ok(emit_this_get(e))

    ast.MetaProperty(_, ast.NewTarget) ->
      Ok(emit_lexical_get(e, lexical.NewTargetRef))

    ast.MetaProperty(_, ast.ImportMeta) ->
      Error(UnsupportedFeature("import.meta"))

    // spread form keeps [args, new_target, ctor], so dup the ctor
    ast.NewExpression(_, callee, args) -> {
      use e <- result.try(emit_expr(e, callee))
      case ast_util.has_spread_arg(args) {
        False -> emit_call_args(e, args, NewCall)
        True -> emit_call_args(emit_op(e, opcode.Dup), args, Construct)
      }
    }

    ast.TemplateLiteral(_, parts) -> emit_template_literal(e, parts)

    ast.ClassExpression(_, name, super_class, body) -> {
      let name = ast.binding_name(name)
      compile_class(e, name, name, super_class, body)
    }

    ast.YieldExpression(_, argument, is_delegate) -> {
      use e <- result.map(case argument {
        Some(arg) -> emit_expr(e, arg)
        None -> Ok(push_const(e, mk_undefined()))
      })
      emit_yield(e, is_delegate:)
    }

    ast.AwaitExpression(_, argument) -> {
      use e <- result.map(emit_expr(e, argument))
      emit_op(e, opcode.Await)
    }

    ast.ParenthesizedExpression(_, inner) -> emit_expr(e, inner)

    ast.RegExpLiteral(_, pattern, flags) -> {
      let e = push_const(e, mk_string(pattern))
      let e = push_const(e, mk_string(flags))
      Ok(emit_op(e, opcode.NewRegExp))
    }

    ast.ImportExpression(_, source, options, phase) -> {
      use e <- result.try(emit_expr(e, source))
      case phase {
        ast.PhaseEvaluation -> {
          use e <- result.map(case options {
            Some(opts) -> emit_expr(e, opts)
            None -> Ok(push_const(e, mk_undefined()))
          })
          emit_op(e, opcode.DynamicImport)
        }
        ast.PhaseSource -> Ok(emit_op(e, opcode.DynamicImportSource))
        ast.PhaseDefer -> Ok(emit_op(e, opcode.DynamicImportDefer))
      }
    }

    // lowered to a call with the site's template object as first argument
    ast.TaggedTemplateExpression(tag:, parts:, span:) -> {
      let site = e.next_site
      let e = Emitter(..e, next_site: site + 1)
      let template =
        ast.IntrinsicTemplateObject(
          site:,
          quasis: ast.template_quasis(parts),
          span:,
        )
      // a tagged template is never a direct eval
      let tag = case tag {
        ast.Identifier(span: tag_span, name: "eval") ->
          ast.ParenthesizedExpression(span: tag_span, expression: tag)
        _ -> tag
      }
      emit_expr(
        e,
        ast.CallExpression(span:, callee: tag, arguments: [
          template,
          ..ast.template_expressions(parts)
        ]),
      )
    }
    ast.IntrinsicTemplateObject(site:, quasis:, span: _) -> {
      let quasis =
        list.map(quasis, fn(q) {
          opcode.TemplateQuasi(cooked: q.cooked, raw: q.raw)
        })
      Ok(emit_op(e, opcode.GetTemplateObject(site, quasis)))
    }

    // named, not _ ->, so a new variant is a compile error here
    ast.SuperExpression(_) -> Error(BareSuperExpression)
    ast.SpreadElement(_, _) -> Error(BareSpreadElement)
  }
}

fn emit_delete(e: Emitter, arg: ast.Expression) -> Result(Emitter, EmitError) {
  case ast_util.unwrap_parens(arg) {
    // §13.5.1.2 delete super ref: evaluate for effects then throw
    ast.MemberExpression(_, ast.SuperExpression(_), property) -> {
      let e = emit_this_get(e) |> emit_op(opcode.Pop)
      use e <- result.map(case property {
        ast.Bracket(key) ->
          result.map(emit_expr(e, key), emit_op(_, opcode.Pop))
        ast.Dot(..) -> Ok(e)
      })
      emit_op(
        e,
        opcode.ThrowError(
          error_kind.ReferenceError,
          "Unsupported reference to 'super'",
        ),
      )
    }
    ast.MemberExpression(_, obj, ast.Dot(name: prop, ..)) -> {
      use e <- result.map(emit_expr(e, obj))
      emit_ir(e, IrDeleteField(prop))
    }
    ast.MemberExpression(_, obj, ast.Bracket(key_expr)) -> {
      use e <- result.try(emit_expr(e, obj))
      use e <- result.map(emit_expr(e, key_expr))
      emit_op(e, opcode.DeleteElem)
    }
    ast.Identifier(name:, ..) -> Ok(emit_var_delete(e, name))
    _ -> {
      use e <- result.map(emit_expr(e, arg))
      let e = emit_op(e, opcode.Pop)
      push_const(e, mk_bool(True))
    }
  }
}

// annex b: a call as assignment target evaluates the call then throws
fn emit_call_then_reference_error(
  e: Emitter,
  call: ast.Expression,
  message: String,
) -> Result(Emitter, EmitError) {
  use e <- result.map(emit_expr(e, call))
  e
  |> emit_op(opcode.Pop)
  |> emit_op(opcode.ThrowError(error_kind.ReferenceError, message))
}

// argument arrives with parens already unwrapped
fn emit_update(
  e: Emitter,
  op: ast.UpdateOp,
  prefix prefix: Bool,
  argument argument: ast.Expression,
) -> Result(Emitter, EmitError) {
  let one = mk_int(1)
  let bin_kind = const_fold.update_binop(op)
  case argument {
    ast.CallExpression(..) ->
      emit_call_then_reference_error(
        e,
        argument,
        "Invalid left-hand side expression in update operation",
      )
    ast.Identifier(name:, ..) if prefix -> {
      use e <- with_identifier_read_write(e, name)
      let e = emit_op(e, opcode.UnaryOp(opcode.Pos))
      let e = push_const(e, one)
      Ok(emit_ir(e, IrBinOp(bin_kind)))
    }
    ast.Identifier(name:, ..) -> {
      let #(ref, e) = emit_var_ref_make(e, name)
      let e = emit_var_ref_get(e, ref)
      let e = emit_op(e, opcode.UnaryOp(opcode.Pos))
      let e = emit_op(e, opcode.Dup)
      let e = push_const(e, one)
      let e = emit_ir(e, IrBinOp(bin_kind))
      Ok(emit_var_ref_put(e, VarRef(..ref, read: True)))
    }
    // ToNumeric applies to the old value; postfix stashes it in a scratch slot
    ast.MemberExpression(..) -> {
      use #(put, e) <- result.map(emit_member_get_keep(
        e,
        classify_assign_target(argument),
      ))
      let e = emit_op(e, opcode.UnaryOp(opcode.Pos))
      case prefix {
        True ->
          e
          |> push_const(one)
          |> emit_ir(IrBinOp(bin_kind))
          |> put
        False -> {
          // save old aside, write new, recover old as the value
          let #(old, e) = fresh_slot(e)
          e
          |> emit_op(opcode.Dup)
          |> emit_scratch_put(old)
          |> push_const(one)
          |> emit_ir(IrBinOp(bin_kind))
          |> put
          |> emit_op(opcode.Pop)
          |> emit_scratch_get(old)
        }
      }
    }
    _ -> Error(InvalidUpdateTarget)
  }
}

fn emit_assignment(
  e: Emitter,
  op: ast.AssignmentOp,
  target: ast.Expression,
  right: ast.Expression,
) -> Result(Emitter, EmitError) {
  case op, target {
    _, ast.ParenthesizedExpression(..) ->
      case op, ast_util.unwrap_parens(target) {
        // (x) = fn must not infer the name, so plain emit_expr
        ast.Assign, ast.Identifier(name:, ..) ->
          with_identifier_write(e, name, emit_expr(_, right))
        _, inner -> emit_assignment(e, op, inner, right)
      }

    // throws before the rhs
    _, ast.CallExpression(..) ->
      emit_call_then_reference_error(
        e,
        target,
        "Invalid left-hand side in assignment",
      )

    // logical assignment must precede the compound branches
    ast.LogicalAndAssign, _ ->
      emit_logical_assign(e, ast.LogicalAnd, target, right)
    ast.LogicalOrAssign, _ ->
      emit_logical_assign(e, ast.LogicalOr, target, right)
    ast.NullishCoalesceAssign, _ ->
      emit_logical_assign(e, ast.NullishCoalescing, target, right)

    // named evaluation for anonymous fn/class rhs
    ast.Assign, ast.Identifier(name:, ..) -> {
      let inferred_name = case name == summary.default_export_local_name {
        True -> "default"
        False -> name
      }
      with_identifier_write(e, name, emit_named_expr(_, right, inferred_name))
    }

    _, ast.Identifier(name:, ..) ->
      case const_fold.compound_to_binop(op) {
        Some(bin_kind) -> {
          use e <- with_identifier_read_write(e, name)
          use e <- result.map(emit_expr(e, right))
          emit_ir(e, IrBinOp(bin_kind))
        }
        None -> Error(NonCompoundAssignOperator)
      }

    ast.Assign, ast.MemberExpression(_, ast.SuperExpression(_), property) -> {
      use e <- result.try(emit_super_ref(e, property))
      use e <- result.map(emit_expr(e, right))
      emit_op(e, opcode.PutSuperValue)
    }

    ast.Assign, ast.MemberExpression(_, obj, ast.Dot(name: prop, ..)) -> {
      use e <- result.try(emit_expr(e, obj))
      use e <- result.map(emit_expr(e, right))
      // [val, obj, ..] -> PutField -> [val]
      emit_put_field(e, prop)
    }

    ast.Assign, ast.MemberExpression(_, obj, ast.Bracket(key)) -> {
      use e <- result.try(emit_expr(e, obj))
      use e <- result.try(emit_expr(e, key))
      use e <- result.map(emit_expr(e, right))
      // PutElem expects [val, key, obj]
      emit_op(e, opcode.PutElem)
    }

    // keep-read so base/key evaluate once
    _, ast.MemberExpression(..) ->
      case const_fold.compound_to_binop(op) {
        Some(bin_kind) -> {
          use #(put, e) <- result.try(emit_member_get_keep(
            e,
            classify_assign_target(target),
          ))
          use e <- result.map(emit_expr(e, right))
          emit_ir(e, IrBinOp(bin_kind)) |> put
        }
        None -> Error(NonCompoundAssignOperator)
      }

    // result is rhs (§13.15.2 step 6), so dup before destructuring
    ast.Assign, _ -> {
      use e <- result.try(emit_expr(e, right))
      let e = emit_op(e, opcode.Dup)
      emit_destructuring_assign(e, target)
    }
    _, _ -> Error(InvalidCompoundAssignTarget)
  }
}

fn emit_call(
  e: Emitter,
  expr: ast.Expression,
  callee: ast.Expression,
  args: List(ast.Expression),
) -> Result(Emitter, EmitError) {
  // a?.b.m(x), a?.b[k](x), a?.()(x): the chain short-circuits the call too
  use <- bool.lazy_guard(ast_util.chain_has_optional(callee), fn() {
    emit_chain_root(e, expr)
  })
  case callee {
    // §13.3.7.1 super(args): parent ctor, new.target, construct, bind this, field init
    ast.SuperExpression(_) -> {
      let e =
        e
        |> emit_lexical_get(lexical.ActiveFuncRef)
        |> emit_op(opcode.GetPrototypeOf)
        |> emit_lexical_get(lexical.NewTargetRef)
      use e <- result.map(case e.in_implicit_derived_ctor {
        // default derived ctor forwards args without observable iteration
        True ->
          Ok(
            e
            |> emit_op(opcode.CreateRestArray(0))
            |> emit_op(opcode.CallConstructorApply),
          )
        False -> emit_call_args(e, args, Construct)
      })
      let e = e |> emit_op(opcode.Dup) |> emit_this_bind
      case e.field_init {
        FieldInitAfterSuper -> emit_field_init_call(e)
        NoFieldInit | FieldInitAtStart -> e
      }
    }

    ast.MemberExpression(_, ast.SuperExpression(_), property) -> {
      use e <- result.try(emit_super_method_ref(e, property))
      emit_call_args(e, args, MethodCall)
    }

    ast.MemberExpression(_, obj, ast.Dot(name: method_name, ..)) -> {
      let forwarding = apply_arguments_forwarding(e, expr)
      use e <- result.try(emit_expr(e, obj))
      let e = emit_get_field_keep(e, method_name)
      case forwarding {
        Some(ApplyArgumentsForwarding(this_arg:, arguments_slot:)) -> {
          use e <- result.map(emit_expr(e, this_arg))
          let e = Emitter(..e, references_arguments: True)
          emit_op(
            e,
            opcode.ApplyArguments(slot: arguments_slot, simple_params: False),
          )
        }
        None -> emit_call_args(e, args, MethodCall)
      }
    }

    ast.MemberExpression(_, obj, ast.Bracket(key)) -> {
      use e <- result.try(emit_expr(e, obj))
      use e <- result.try(emit_expr(e, key))
      emit_call_args(emit_get_elem_method(e), args, MethodCall)
    }

    // runtime check for direct eval
    ast.Identifier(name: "eval", ..) -> {
      let e = emit_var_get(e, "eval")
      case ast_util.has_spread_arg(args) {
        False -> {
          use e <- result.map(list.try_fold(args, e, emit_expr))
          emit_op(
            e,
            opcode.CallEval(
              list.length(args),
              e.param_scope_names,
              e.with_stack,
              e.private_env,
            ),
          )
        }
        True -> {
          use e <- result.map(emit_args_array_with_spread(e, args))
          emit_op(e, opcode.CallApply)
        }
      }
    }

    // §13.3.6.2 callee inside with: this may be the with object
    _ ->
      case ast_util.unwrap_parens(callee), e.with_stack {
        ast.Identifier(name:, ..), [_, ..] ->
          emit_var_get_as_callee(e, name) |> emit_call_args(args, MethodCall)
        _, _ -> {
          use e <- result.try(emit_expr(e, callee))
          emit_call_args(e, args, PlainCall)
        }
      }
  }
}

fn emit_member(
  e: Emitter,
  expr: ast.Expression,
  object: ast.Expression,
  property: ast.MemberProperty,
) -> Result(Emitter, EmitError) {
  // any ?. in the object chain routes through the chain compiler
  use <- bool.lazy_guard(ast_util.chain_has_optional(object), fn() {
    emit_chain_root(e, expr)
  })
  case object, property {
    ast.SuperExpression(_), _ -> {
      use e <- result.map(emit_super_ref(e, property))
      emit_op(e, opcode.GetSuperValue)
    }
    _, ast.Dot(name: prop, ..) -> {
      use e <- result.map(emit_expr(e, object))
      emit_get_field(e, prop)
    }
    _, ast.Bracket(key) -> {
      use e <- result.try(emit_expr(e, object))
      use e <- result.map(emit_expr(e, key))
      emit_op(e, opcode.GetElem)
    }
  }
}

// operand already on the stack
fn emit_yield(e: Emitter, is_delegate is_delegate: Bool) -> Emitter {
  case is_delegate, e.is_async {
    False, False -> emit_op(e, opcode.Yield)
    // async generators await the operand before yielding
    False, True -> e |> emit_op(opcode.Await) |> emit_op(opcode.Yield)
    // sync yield*: YieldStar self-loops; leaves result.value
    True, False ->
      e
      |> emit_op(opcode.GetIterator)
      |> push_const(mk_undefined())
      |> emit_op(opcode.YieldStar)
    // async yield*: self-loop of next, await, resume; leaves result.value
    True, True -> {
      let e = emit_op(e, opcode.GetAsyncIterator)
      let e = emit_op(e, opcode.IteratorRecord)
      let e = push_const(e, mk_undefined())
      let #(next_label, e) = fresh_label(e)
      // the async-gen driver resumes at after_label when a forwarded throw finishes
      let #(after_label, e) = fresh_label(e)
      e
      |> emit_ir(IrLabel(next_label))
      |> emit_ir(IrAsyncYieldStarNext(after_label))
      |> emit_op(opcode.Await)
      |> emit_ir(IrAsyncYieldStarResume(next_label))
      |> emit_ir(IrLabel(after_label))
    }
  }
}

fn emit_template_literal(
  e: Emitter,
  parts: ast.TemplateParts(String),
) -> Result(Emitter, EmitError) {
  // "a" + x + "b" + y + "c", skipping empty quasis
  let #(started, e) = case parts.head, parts.tail {
    "", [_, ..] -> #(False, e)
    head, _ -> #(True, push_const(e, mk_string(head)))
  }
  use #(_, e) <- result.map(
    list.try_fold(parts.tail, #(started, e), fn(acc, part) {
      let #(started, e) = acc
      let ast.TemplateSpan(expr, quasi) = part
      // ToString with string hint, not the + operator's default hint
      use e <- result.map(emit_expr(e, expr))
      let e = emit_op(e, opcode.ToStringVal)
      let e = case started {
        True -> emit_ir(e, IrBinOp(AddOp))
        False -> e
      }
      case quasi {
        "" -> #(True, e)
        _ -> #(True, emit_ir(push_const(e, mk_string(quasi)), IrBinOp(AddOp)))
      }
    }),
  )
  e
}

fn emit_switch(
  e: Emitter,
  discriminant: ast.Expression,
  cases: List(ast.SwitchCase),
) -> Result(Emitter, EmitError) {
  let #(end_label, e) = fresh_label(e)

  let e = push_switch(e, end_label)

  // discriminant evaluated outside the case block scope (§14.12.4)
  use e <- result.try(emit_expr(e, discriminant))

  // case block is one scope; parser put children in the same hoist order
  let case_stmts = ast_util.switch_case_stmts(cases)
  let #(save, e) = enter_scope(e, in_block: True)
  use e <- result.try(emit_block_declarations(e, case_stmts))

  // found_N trampolines pop the discriminant before jumping to the body
  let #(arms_rev, e) =
    list.fold(cases, #([], e), fn(acc, c) {
      let #(out, e) = acc
      let ast.SwitchCase(condition:, consequent:) = c
      let #(body, e) = fresh_label(e)
      let #(match, e) = case condition {
        Some(expr) -> {
          let #(found, e) = fresh_label(e)
          #(Some(SwitchTest(expr:, found:)), e)
        }
        None -> #(None, e)
      }
      #([SwitchArm(match:, body:, consequent:), ..out], e)
    })
  let arms = list.reverse(arms_rev)

  use e <- result.try(
    list.try_fold(arms, e, fn(e, arm) {
      case arm.match {
        Some(SwitchTest(expr:, found:)) -> {
          let e = emit_op(e, opcode.Dup)
          use e <- result.map(emit_expr(e, expr))
          let e = emit_ir(e, IrBinOp(PureOp(binop.Equality(binop.StrictEq))))
          emit_ir(e, IrJumpIfTrue(found))
        }
        None -> Ok(e)
      }
    }),
  )
  let default_body_label =
    list.find_map(arms, fn(arm) {
      case arm.match {
        None -> Ok(arm.body)
        Some(_) -> Error(Nil)
      }
    })
    |> result.unwrap(end_label)

  let e = emit_op(e, opcode.Pop)
  let e = emit_ir(e, IrJump(default_body_label))

  let e =
    list.fold(arms, e, fn(e, arm) {
      case arm.match {
        Some(SwitchTest(found:, ..)) ->
          e
          |> emit_ir(IrLabel(found))
          |> emit_op(opcode.Pop)
          |> emit_ir(IrJump(arm.body))
        None -> e
      }
    })

  use e <- result.try(
    list.try_fold(arms, e, fn(e, arm) {
      emit_stmts(emit_ir(e, IrLabel(arm.body)), arm.consequent)
    }),
  )

  let e = emit_ir(e, IrLabel(end_label))
  let e = leave_scope(e, save)
  let e = pop_frame(e)
  Ok(e)
}

type SwitchArm {
  SwitchArm(
    match: Option(SwitchTest),
    body: LabelId,
    consequent: List(ast.StmtWithLine),
  )
}

type SwitchTest {
  SwitchTest(expr: ast.Expression, found: LabelId)
}

fn emit_sequence(
  e: Emitter,
  exprs: List(ast.Expression),
) -> Result(Emitter, EmitError) {
  case exprs {
    [] -> Ok(push_const(e, mk_undefined()))
    [only] -> emit_expr(e, only)
    [first, ..rest] -> {
      use e <- result.try(emit_expr(e, for_effect(first)))
      let e = emit_op(e, opcode.Pop)
      emit_sequence(e, rest)
    }
  }
}

// §8.4 named evaluation for anonymous fn/arrow/class
fn emit_named_expr(
  e: Emitter,
  expr: ast.Expression,
  name: String,
) -> Result(Emitter, EmitError) {
  case expr {
    // looks through parens: (function(){}) is still anonymous
    ast.ParenthesizedExpression(_, inner) -> emit_named_expr(e, inner, name)
    ast.FunctionExpression(_, None, params, body, is_generator, is_async) ->
      emit_function_closure(
        e,
        Some(name),
        params,
        body,
        is_generator,
        is_async,
        bind_self: False,
      )
    ast.ArrowFunctionExpression(_, params, body, is_async) ->
      emit_arrow_closure(e, Some(name), params, body, is_async)
    ast.ClassExpression(_, None, super_class, body) ->
      compile_class(e, None, Some(name), super_class, body)
    _ -> emit_expr(e, expr)
  }
}

fn register_closure(
  compiled: Result(#(CompiledChild, Emitter), EmitError),
) -> Result(Emitter, EmitError) {
  use #(child, e) <- result.map(compiled)
  let #(idx, e) = add_child_function(e, child)
  emit_op(e, opcode.MakeClosure(idx))
}

// never a constructor: new o.m() must throw
fn emit_method_value(
  e: Emitter,
  value: ast.FunctionLiteral,
  name: Option(String),
) -> Result(Emitter, EmitError) {
  let ast.FunctionLiteral(params:, body:, is_generator:, is_async:, ..) = value
  compile_function_body(
    e,
    name,
    params,
    StmtsBody(body),
    shape: Method(is_generator:, is_async:),
  )
  |> register_closure
}

fn emit_function_closure(
  e: Emitter,
  name: Option(String),
  params: List(ast.Pattern),
  body: List(ast.StmtWithLine),
  is_generator is_generator: Bool,
  is_async is_async: Bool,
  // only syntactically named expressions get the self-name binding
  bind_self bind_self: Bool,
) -> Result(Emitter, EmitError) {
  let self_name = case bind_self {
    True -> name
    False -> None
  }
  compile_function_body(
    e,
    name,
    params,
    StmtsBody(body),
    shape: FnExpr(self_name:, is_generator:, is_async:),
  )
  |> register_closure
}

fn emit_arrow_closure(
  e: Emitter,
  name: Option(String),
  params: List(ast.Pattern),
  body: ast.ArrowBody,
  is_async is_async: Bool,
) -> Result(Emitter, EmitError) {
  let body_stmts = case body {
    ast.ArrowBodyExpression(expr) -> [
      ast.StmtWithLine(0, ast.ReturnStatement(Some(expr))),
    ]
    ast.ArrowBodyBlock(stmts) -> stmts
  }
  compile_function_body(
    e,
    name,
    params,
    StmtsBody(body_stmts),
    shape: Arrow(is_async:),
  )
  |> register_closure
}

type NamedMember {
  NamedMember(name: String, value: ast.Expression)
}

type LiteralHead {
  LiteralHead(
    keys: List(key.PropertyKey),
    members: List(NamedMember),
    rest: List(ast.Property),
  )
}

// leading distinct static-key data members, for NewObjectWith
fn literal_head(
  properties: List(ast.Property),
  keys: List(key.PropertyKey),
  head: List(NamedMember),
  seen: Set(String),
) -> LiteralHead {
  let done = LiteralHead(keys:, members: list.reverse(head), rest: properties)
  case properties {
    [ast.InitProperty(key: k, value:, ..), ..rest] ->
      case literal_key(k, seen) {
        Some(#(name, pk)) ->
          literal_head(
            rest,
            [pk, ..keys],
            [NamedMember(name:, value:), ..head],
            set.insert(seen, name),
          )
        None -> done
      }
    _ -> done
  }
}

fn literal_key(
  k: ast.PropertyName,
  seen: Set(String),
) -> Option(#(String, key.PropertyKey)) {
  let name = case k {
    ast.IdentifierName(name:, ..) -> Some(name)
    ast.StringName(value: name, ..) -> Some(name)
    _ -> None
  }
  use name <- option.then(name)
  use <- bool.guard(name == "__proto__" || set.contains(seen, name), None)
  case key.canonical(name) {
    key.Named(_) as pk -> Some(#(name, pk))
    _ -> None
  }
}

// object stays on the stack across properties
fn emit_object_property(
  e: Emitter,
  prop: ast.Property,
) -> Result(Emitter, EmitError) {
  case prop {
    // annex b: non-computed, non-shorthand __proto__: v sets the prototype
    ast.InitProperty(
      key: ast.IdentifierName(name: "__proto__", ..),
      value:,
      shorthand: False,
    )
    | ast.InitProperty(
        key: ast.StringName(value: "__proto__", ..),
        value:,
        shorthand: False,
      ) -> {
      use e <- result.map(emit_expr(e, value))
      emit_op(e, opcode.SetProto)
    }

    ast.InitProperty(key: ast.IdentifierName(name:, ..), value:, ..)
    | ast.InitProperty(key: ast.StringName(value: name, ..), value:, ..) -> {
      use e <- result.map(emit_named_expr(e, value, name))
      emit_ir(e, IrDefineField(name))
    }

    // numeric keys need runtime ToPropertyKey ("1" not "1.0")
    ast.InitProperty(key:, value:, ..) ->
      emit_computed_init_property(e, emit_property_key(_, key), value)

    ast.MethodProperty(key: ast.IdentifierName(name:, ..), value:)
    | ast.MethodProperty(key: ast.StringName(value: name, ..), value:) -> {
      use e <- result.map(emit_method_value(e, value, Some(name)))
      let e = emit_op(e, opcode.MakeMethod)
      emit_ir(e, IrDefineField(name))
    }

    ast.MethodProperty(key:, value:) ->
      emit_computed_method_property(e, emit_property_key(_, key), value)

    ast.AccessorProperty(key: ast.IdentifierName(name:, ..), value:, kind:)
    | ast.AccessorProperty(key: ast.StringName(value: name, ..), value:, kind:) -> {
      let #(prefix, accessor) = property_accessor(kind)
      use e <- result.map(emit_method_value(e, value, Some(prefix <> name)))
      emit_ir(e, IrDefineAccessor(name, accessor, enumerable: True))
    }

    ast.AccessorProperty(key:, value:, kind:) -> {
      let #(_, accessor) = property_accessor(kind)
      use e <- result.try(emit_property_key(e, key))
      use e <- result.map(emit_method_value(e, value, None))
      emit_op(e, opcode.DefineAccessorComputed(accessor, enumerable: True))
    }

    ast.SpreadProperty(argument:) -> {
      use e <- result.map(emit_expr(e, argument))
      emit_op(e, opcode.ObjectSpread)
    }
  }
}

// no ToPropertyKey here, the vm does it; KeyPrivate unreachable
fn emit_property_key(
  e: Emitter,
  key: ast.PropertyName,
) -> Result(Emitter, EmitError) {
  case key {
    ast.IdentifierName(name:, ..) | ast.PrivateName(name:, ..) ->
      Ok(push_const(e, mk_string(name)))
    ast.StringName(value: s, ..) -> Ok(push_const(e, mk_string(s)))
    ast.NumberName(value: n, ..) ->
      Ok(push_const(e, const_fold.number_const(n)))
    ast.BigIntName(value: i, ..) -> Ok(push_const(e, mk_bigint(i)))
    ast.ComputedName(expression:) -> emit_expr(e, expression)
  }
}

fn property_accessor(kind: ast.AccessorKind) -> #(String, opcode.AccessorKind) {
  case kind {
    ast.GetAccessor -> #("get ", opcode.Getter)
    ast.SetAccessor -> #("set ", opcode.Setter)
  }
}

fn emit_computed_init_property(
  e: Emitter,
  emit_key: fn(Emitter) -> Result(Emitter, EmitError),
  value: ast.Expression,
) -> Result(Emitter, EmitError) {
  use e <- result.try(emit_key(e))
  use e <- result.map(emit_expr(e, value))
  emit_op(e, opcode.DefineFieldComputed)
}

// closure first so MakeMethod sees [obj, fn], then key, then swap
fn emit_computed_method_property(
  e: Emitter,
  emit_key: fn(Emitter) -> Result(Emitter, EmitError),
  value: ast.FunctionLiteral,
) -> Result(Emitter, EmitError) {
  use e <- result.try(emit_method_value(e, value, None))
  let e = emit_op(e, opcode.MakeMethod)
  use e <- result.map(emit_key(e))
  e |> emit_op(opcode.Swap) |> emit_op(opcode.DefineFieldComputed)
}

fn emit_array_no_spread(
  e: Emitter,
  elements: List(Option(ast.Expression)),
) -> Result(Emitter, EmitError) {
  let count = list.length(elements)
  use #(_idx, holes_rev, e) <- result.map(
    list.try_fold(elements, #(0, [], e), fn(acc, elem) {
      let #(idx, holes_rev, e) = acc
      case elem {
        Some(expr) -> {
          use e <- result.map(emit_expr(e, expr))
          #(idx + 1, holes_rev, e)
        }
        None -> Ok(#(idx + 1, [idx, ..holes_rev], e))
      }
    }),
  )
  case holes_rev {
    [] -> emit_op(e, opcode.ArrayFrom(count))
    _ -> emit_op(e, opcode.ArrayFromWithHoles(count, list.reverse(holes_rev)))
  }
}

// array stays on top; push/spread consume [x, arr] -> [arr]
fn emit_array_with_spread(
  e: Emitter,
  elements: List(Option(ast.Expression)),
) -> Result(Emitter, EmitError) {
  let #(prefix, tail) =
    list.split_while(elements, fn(el) {
      case el {
        Some(ast.SpreadElement(_, _)) -> False
        _ -> True
      }
    })

  use e <- result.try(emit_array_no_spread(e, prefix))

  list.try_fold(tail, e, fn(e, elem) {
    case elem {
      Some(ast.SpreadElement(argument:, ..)) -> {
        use e <- result.map(emit_expr(e, argument))
        emit_op(e, opcode.ArraySpread)
      }
      Some(expr) -> {
        use e <- result.map(emit_expr(e, expr))
        emit_op(e, opcode.ArrayPush)
      }
      None -> Ok(emit_op(e, opcode.ArrayPushHole))
    }
  })
}

// call args have no holes; leaves the args array on top
fn emit_args_array_with_spread(
  e: Emitter,
  args: List(ast.Expression),
) -> Result(Emitter, EmitError) {
  emit_array_with_spread(e, list.map(args, Some))
}

// what sits under the arguments: [f], [f, receiver], or [new_target, ctor]
type CallForm {
  PlainCall
  MethodCall
  Construct
  // [ctor] only; spread callers dup it and use Construct instead
  NewCall
}

fn emit_call_args(
  e: Emitter,
  args: List(ast.Expression),
  form: CallForm,
) -> Result(Emitter, EmitError) {
  case ast_util.has_spread_arg(args) {
    False -> {
      use e <- result.map(list.try_fold(args, e, emit_expr))
      let argc = list.length(args)
      emit_op(e, case form {
        PlainCall -> opcode.Call(argc)
        MethodCall -> opcode.CallMethod(argc)
        Construct -> opcode.CallConstructor(argc)
        NewCall -> opcode.CallNew(argc)
      })
    }
    True -> {
      use e <- result.map(emit_args_array_with_spread(e, args))
      emit_op(e, case form {
        PlainCall -> opcode.CallApply
        MethodCall -> opcode.CallMethodApply
        Construct | NewCall -> opcode.CallConstructorApply
      })
    }
  }
}

// §14.7.5.7 fresh env per iteration: re-run the head prologue, re-boxing captured bindings
fn emit_for_per_iteration_env(e: Emitter, left: ast.ForInit) -> Emitter {
  case ast_util.for_head_lex_names(left) {
    [] -> e
    _ -> emit_binding_prologue(e, e.current_scope)
  }
}

// [obj] -> ForInStart -> [iter]; ForInNext -> [iter, key, done]
fn emit_for_in(
  e: Emitter,
  left: ast.ForInit,
  right: ast.Expression,
  body: ast.Statement,
) -> Result(Emitter, EmitError) {
  let #(loop_start, e) = fresh_label(e)
  let #(loop_continue, e) = fresh_label(e)
  let #(cleanup, e) = fresh_label(e)
  let #(loop_end, e) = fresh_label(e)

  // match on kind: for (let {} of ..) binds no names but has a scope
  let has_lex = ast_util.for_classic_init_is_lex(Some(left))
  let #(save, e) = enter_for_scope(e, has_lex)

  // known gap: §14.7.5.6 step 5 restore oldEnv not modeled (scope-head-lex-*.js)
  use e <- result.try(emit_expr(e, right))
  let e = emit_op(e, opcode.ForInStart)

  let e = push_loop(e, loop_end, loop_continue, NoIter)
  let e = emit_ir(e, IrLabel(loop_start))

  let e = emit_op(e, opcode.ForInNext)
  let e = emit_ir(e, IrJumpIfTrue(cleanup))

  let e = emit_for_per_iteration_env(e, left)
  use e <- result.try(emit_for_lhs_bind(e, left))

  use e <- result.try(emit_stmt(e, body))

  let e = emit_ir(e, IrLabel(loop_continue))
  let e = emit_ir(e, IrJump(loop_start))

  // done=true left the key on stack
  let e = emit_ir(e, IrLabel(cleanup))
  let e = emit_op(e, opcode.Pop)

  let e = emit_ir(e, IrLabel(loop_end))
  let e = emit_op(e, opcode.Pop)

  let e = pop_frame(e)
  Ok(leave_for_scope(e, save))
}

// using heads dispose per iteration inside the caller's F_body
fn emit_for_of_iter_body(
  e: Emitter,
  left: ast.ForInit,
  body: ast.Statement,
  loop_continue: LabelId,
  loop_start: LabelId,
) -> Result(Emitter, EmitError) {
  let e = emit_for_per_iteration_env(e, left)
  use e <- result.try(emit_for_lhs_bind(e, left))
  use e <- result.map(case for_of_using_binding(left) {
    Some(binding) -> emit_for_of_using_body(e, binding, body)
    None -> emit_stmt(e, body)
  })
  e
  |> emit_ir(IrLabel(loop_continue))
  |> emit_ir(IrJump(loop_start))
}

type ForOfLabels {
  ForOfLabels(
    loop_start: LabelId,
    loop_continue: LabelId,
    break_target: LabelId,
    body_threw: LabelId,
    end: LabelId,
  )
}

// known gap: §14.7.5.6 step 5 restore oldEnv not modeled (scope-head-lex-*.js)
fn emit_for_of_common(
  e: Emitter,
  left: ast.ForInit,
  right: ast.Expression,
  iterator: LoopIter,
  emit_loop_body: fn(Emitter, ForOfLabels) -> Result(Emitter, EmitError),
) -> Result(Emitter, EmitError) {
  let #(loop_start, e) = fresh_label(e)
  let #(loop_continue, e) = fresh_label(e)
  let #(break_target, e) = fresh_label(e)
  let #(body_threw, e) = fresh_label(e)
  let #(end, e) = fresh_label(e)
  let has_lex = ast_util.for_classic_init_is_lex(Some(left))
  let #(save, e) = enter_for_scope(e, has_lex)
  use e <- result.try(emit_expr(e, right))
  // for await uses CatchOnly: its close needs an await the unwinder cannot do
  let #(get_iter, body_kind) = case iterator {
    AsyncIter -> #(opcode.GetAsyncIterator, CatchOnly)
    SyncIter | NoIter -> #(opcode.GetIterator, IterCloseGuard)
  }
  // loop frame pushed after F_body so crossing jumps pop it and close iter
  let e =
    e
    |> emit_op(get_iter)
    |> emit_ir(IrPushTry(body_threw, body_kind))
    |> push_loop(break_target, loop_continue, iterator)
    |> emit_ir(IrLabel(loop_start))
  let labels =
    ForOfLabels(loop_start:, loop_continue:, break_target:, body_threw:, end:)
  use e <- result.map(emit_loop_body(e, labels))
  e |> emit_ir(IrLabel(end)) |> pop_frame |> leave_for_scope(save)
}

// close on abrupt body exit only; IteratorNext undefs the slot on done or throw
fn emit_for_of(
  e: Emitter,
  left: ast.ForInit,
  right: ast.Expression,
  body: ast.Statement,
) -> Result(Emitter, EmitError) {
  use e, labels <- emit_for_of_common(e, left, right, SyncIter)
  let ForOfLabels(loop_start:, loop_continue:, break_target:, body_threw:, end:) =
    labels
  let #(exhausted, e) = fresh_label(e)
  let e = emit_op(e, opcode.IteratorNext)
  // [done, value, iter|undef, ..base]
  let e = emit_ir(e, IrJumpIfTrue(exhausted))

  // [value, iter, ..base]
  use e <- result.map(emit_for_of_iter_body(
    e,
    left,
    body,
    loop_continue,
    loop_start,
  ))

  // bind/body threw or next threw (undef): close if object, rethrow
  let e = emit_ir(e, IrLabel(body_threw))
  let e = emit_op(e, opcode.IteratorCloseThrow)

  // exhausted: [value, undef, ..base], no close
  let e = emit_ir(e, IrLabel(exhausted))
  let e = emit_op(e, opcode.Pop)
  let e = emit_op(e, opcode.PopTry)
  let e = emit_op(e, opcode.Pop)
  let e = emit_ir(e, IrJump(end))

  // break_target: [iter, ..base], normal close
  let e = emit_ir(e, IrLabel(break_target))
  let e = emit_op(e, opcode.PopTry)
  let e = emit_op(e, opcode.IteratorClose)
  emit_ir(e, IrJump(end))
}

// F_next keeps a next() failure from closing; rethrow is pop;pop;throw
fn emit_for_await_of(
  e: Emitter,
  left: ast.ForInit,
  right: ast.Expression,
  body: ast.Statement,
) -> Result(Emitter, EmitError) {
  use e, labels <- emit_for_of_common(e, left, right, AsyncIter)
  let ForOfLabels(loop_start:, loop_continue:, break_target:, body_threw:, end:) =
    labels
  let #(exhausted, e) = fresh_label(e)
  let #(catch_next, e) = fresh_label(e)
  let #(no_ret_thr, e) = fresh_label(e)
  let #(rethrow, e) = fresh_label(e)

  // F_next: errors in next/await/unwrap must not close (§14.7.5.6)
  let e = emit_ir(e, IrPushTry(catch_next, CatchOnly))
  let e = emit_op(e, opcode.Dup)
  let e = emit_ir(e, IrGetFieldKeep("next"))
  let e = emit_op(e, opcode.CallMethod(0))
  let e = emit_op(e, opcode.Await)
  let e = emit_op(e, opcode.IteratorCheckObject)
  // [result_obj, iter, ..base]
  let e = emit_op(e, opcode.Dup)
  let e = emit_ir(e, IrGetField("done"))
  let e = emit_ir(e, IrJumpIfTrue(exhausted))
  let e = emit_ir(e, IrGetField("value"))
  let e = emit_op(e, opcode.PopTry)
  // [value, iter, ..base]
  use e <- result.map(emit_for_of_iter_body(
    e,
    left,
    body,
    loop_continue,
    loop_start,
  ))

  // catch_next: [thrown, iter, ..base], do not close
  let e = emit_ir(e, IrLabel(catch_next))
  let e = emit_op(e, opcode.PopTry)
  let e = emit_op(e, opcode.Swap)
  let e = emit_op(e, opcode.Pop)
  let e = emit_op(e, opcode.Throw)

  // body_threw: [thrown, iter, ..base], throw-completion close
  let e = emit_ir(e, IrLabel(body_threw))
  let e = emit_ir(e, IrPushTry(rethrow, CatchOnly))
  let e = emit_op(e, opcode.Swap)
  let e = emit_op(e, opcode.Dup)
  // [iter, iter, thrown, ..base]
  let e = emit_ir(e, IrGetFieldKeep("return"))
  let e = emit_op(e, opcode.Dup)
  let e = emit_ir(e, IrJumpIfNullish(no_ret_thr))
  // [ret_fn, iter, iter, thrown, ..base]
  let e = emit_op(e, opcode.CallMethod(0))
  let e = emit_op(e, opcode.Await)
  // [awaited, iter, thrown, ..base]
  let e = emit_op(e, opcode.PopTry)
  let e = emit_ir(e, IrJump(rethrow))

  let e = emit_ir(e, IrLabel(no_ret_thr))
  // [ret(nullish), iter, iter, thrown, ..base]
  let e = emit_op(e, opcode.PopTry)
  let e = emit_op(e, opcode.Pop)

  // rethrow: always [_, iter, thrown, ..base]
  let e = emit_ir(e, IrLabel(rethrow))
  let e = emit_op(e, opcode.Pop)
  let e = emit_op(e, opcode.Pop)
  let e = emit_op(e, opcode.Throw)

  // exhausted: no close
  let e = emit_ir(e, IrLabel(exhausted))
  let e = emit_op(e, opcode.Pop)
  let e = emit_op(e, opcode.PopTry)
  let e = emit_op(e, opcode.PopTry)
  let e = emit_op(e, opcode.Pop)
  let e = emit_ir(e, IrJump(end))

  // break_target: [iter, ..base], normal close
  let e = emit_ir(e, IrLabel(break_target))
  let e = emit_op(e, opcode.PopTry)
  let e = emit_async_iterator_close(e)
  emit_ir(e, IrJump(end))
}

fn emit_for_lhs_bind(
  e: Emitter,
  left: ast.ForInit,
) -> Result(Emitter, EmitError) {
  case left {
    ast.ForInitDeclaration(kind, [ast.VariableDeclarator(pattern, _)]) ->
      emit_destructuring_bind(e, pattern, ast_util.binding_kind_of(kind))
    ast.ForInitDeclaration(_, _) -> Error(MultiDeclaratorForHead)
    ast.ForInitPattern(pattern) ->
      emit_destructuring_bind(e, pattern, VarBinding)
    ast.ForInitExpression(expr) ->
      emit_destructuring_assign(e, ast_util.unwrap_parens(expr))
  }
}

fn emit_destructuring_bind(
  e: Emitter,
  pattern: ast.Pattern,
  binding_kind: BindingKind,
) -> Result(Emitter, EmitError) {
  case pattern {
    ast.IdentifierPattern(name, ..) -> {
      let e = case binding_kind {
        LetBinding -> declare_lex(e, name, is_const: False)
        ConstBinding -> declare_lex(e, name, is_const: True)
        ParamBinding
        | CatchBinding
        | VarBinding
        | CaptureBinding
        | FnNameBinding -> e
      }
      case binding_kind {
        LetBinding | ConstBinding -> Ok(init_lex(e, name))
        VarBinding
        | ParamBinding
        | CatchBinding
        | CaptureBinding
        | FnNameBinding -> Ok(emit_var_put(e, name))
      }
    }

    ast.ObjectPattern(properties) -> {
      let has_rest =
        list.any(properties, fn(p) {
          case p {
            ast.RestProperty(..) -> True
            ast.PatternProperty(..) -> False
          }
        })
      use e, prop, excluded_key_count <- emit_object_pattern(
        e,
        properties,
        has_rest:,
      )
      emit_single_object_prop(
        e,
        prop,
        binding_kind,
        has_rest,
        excluded_key_count,
      )
    }

    ast.ArrayPattern(elements) -> {
      use e, _close_throw <- with_iterator_scaffold(e)
      emit_array_elements(e, elements, binding_kind)
    }

    ast.AssignmentPattern(left, default_expr) -> {
      let name = case left {
        ast.IdentifierPattern(name, ..) -> Some(name)
        _ -> None
      }
      use e <- result.try(emit_default_if_undefined(e, default_expr, name))
      emit_destructuring_bind(e, left, binding_kind)
    }

    ast.RestElement(argument:) -> {
      emit_destructuring_bind(e, argument, binding_kind)
    }
  }
}

// replace undefined on top with the default: [val] -> [val_or_default]
fn emit_default_if_undefined(
  e: Emitter,
  default_expr: ast.Expression,
  target_name: Option(String),
) -> Result(Emitter, EmitError) {
  let #(has_val, e) = fresh_label(e)
  let e = emit_op(e, opcode.Dup)
  let e = push_const(e, mk_undefined())
  let e = emit_ir(e, IrBinOp(PureOp(binop.Equality(binop.StrictEq))))
  let e = emit_ir(e, IrJumpIfFalse(has_val))
  let e = emit_op(e, opcode.Pop)
  use e <- result.map(case target_name {
    Some(name) -> emit_named_expr(e, default_expr, name)
    None -> emit_expr(e, default_expr)
  })
  emit_ir(e, IrLabel(has_val))
}

// [src, key_n, .., key_1, ..], keys kept only for rest; ToObject first
fn emit_object_pattern(
  e: Emitter,
  properties: List(prop),
  has_rest has_rest: Bool,
  emit_prop emit_prop: fn(Emitter, prop, Int) ->
    Result(#(Int, Emitter), EmitError),
) -> Result(Emitter, EmitError) {
  let e = emit_op(e, opcode.ToObject)
  use #(_excluded_key_count, e) <- result.map(
    list.try_fold(properties, #(0, e), fn(acc, prop) {
      let #(excluded_key_count, e) = acc
      emit_prop(e, prop, excluded_key_count)
    }),
  )
  // rest consumed src and keys; otherwise drop src
  case has_rest {
    True -> e
    False -> emit_op(e, opcode.Pop)
  }
}

// [val, src, ..keys] after bind became [src, ..keys]; stash the key under src
fn stash_excluded_key(
  e: Emitter,
  name: String,
  has_rest has_rest: Bool,
  excluded_key_count excluded_key_count: Int,
) -> #(Int, Emitter) {
  case has_rest {
    False -> #(excluded_key_count, e)
    True -> {
      let e = push_const(e, mk_string(name)) |> emit_op(opcode.Swap)
      #(excluded_key_count + 1, e)
    }
  }
}

// entry [src, ..keys]; has_rest stashes a key under src; rest exits []
fn emit_single_object_prop(
  e: Emitter,
  prop: ast.PatternProperty,
  binding_kind: BindingKind,
  has_rest has_rest: Bool,
  excluded_key_count excluded_key_count: Int,
) -> Result(#(Int, Emitter), EmitError) {
  case prop {
    ast.PatternProperty(key: ast.IdentifierName(name:, ..), value:, ..)
    | ast.PatternProperty(key: ast.StringName(value: name, ..), value:, ..) -> {
      // [src] -> dup -> GetField -> [val, src] -> bind -> [src]
      let e = emit_op(e, opcode.Dup)
      let e = emit_ir(e, IrGetField(name))
      use e <- result.map(emit_destructuring_bind(e, value, binding_kind))
      stash_excluded_key(e, name, has_rest, excluded_key_count)
    }

    ast.PatternProperty(key:, value:, ..) ->
      emit_computed_key_prop(
        e,
        emit_property_key(_, key),
        value,
        binding_kind,
        has_rest,
        excluded_key_count,
      )

    // [src, key_n, .., key_1] -> ObjectRestCopy(n) -> [rest]
    ast.RestProperty(name:, span:) -> {
      let e = emit_op(e, opcode.ObjectRestCopy(excluded_key_count))
      let ident = ast.IdentifierPattern(name:, span:)
      use e <- result.map(emit_destructuring_bind(e, ident, binding_kind))
      #(0, e)
    }
  }
}

// with rest, GetElemKeep keeps the key for the exclusion set (evaluated once)
fn emit_computed_key_prop(
  e: Emitter,
  emit_key: fn(Emitter) -> Result(Emitter, EmitError),
  inner: ast.Pattern,
  binding_kind: BindingKind,
  has_rest has_rest: Bool,
  excluded_key_count excluded_key_count: Int,
) -> Result(#(Int, Emitter), EmitError) {
  let e = emit_op(e, opcode.Dup)
  use e <- result.try(emit_key(e))
  case has_rest {
    False -> {
      let e = emit_op(e, opcode.GetElem)
      use e <- result.map(emit_destructuring_bind(e, inner, binding_kind))
      #(excluded_key_count, e)
    }
    True -> {
      let e = emit_op(e, opcode.GetElemKeep)
      use e <- result.map(emit_destructuring_bind(e, inner, binding_kind))
      #(excluded_key_count + 1, emit_keep_computed_key(e))
    }
  }
}

// [k, src, src, ..keys] -> [src, k, ..keys]
fn emit_keep_computed_key(e: Emitter) -> Emitter {
  e |> emit_op(opcode.Swap) |> emit_op(opcode.Pop) |> emit_op(opcode.Swap)
}

type AssignTarget {
  SuperMember(property: ast.MemberProperty)
  StaticMember(object: ast.Expression, prop: String)
  ComputedMember(object: ast.Expression, key: ast.Expression)
  PlainTarget(expr: ast.Expression)
}

// parens are transparent for assignment target type
fn classify_assign_target(target: ast.Expression) -> AssignTarget {
  case ast_util.unwrap_parens(target) {
    ast.MemberExpression(_, ast.SuperExpression(_), property) ->
      SuperMember(property:)
    ast.MemberExpression(_, object, property) ->
      // exhaustive so a new variant is a compile error
      case property {
        ast.Dot(name: prop, ..) -> StaticMember(object:, prop:)
        ast.Bracket(key) -> ComputedMember(object:, key:)
      }
    other -> PlainTarget(other)
  }
}

fn emit_destructuring_assign(
  e: Emitter,
  target: ast.Expression,
) -> Result(Emitter, EmitError) {
  case classify_assign_target(target) {
    // [val] -> [val, this] -> [val, proto, this] -> [val, key, proto, this] -> PutSuperValue
    SuperMember(property:) -> {
      let e =
        e
        |> emit_this_get
        |> emit_op(opcode.Swap)
        |> emit_lexical_get(lexical.HomeObjectRef)
        |> emit_op(opcode.GetPrototypeOf)
        |> emit_op(opcode.Swap)
      use e <- result.map(emit_super_key(e, property))
      e
      |> emit_op(opcode.Swap)
      |> emit_op(opcode.PutSuperValue)
      |> emit_op(opcode.Pop)
    }

    // [val] -> [obj, val] -> swap -> PutField -> pop
    StaticMember(object:, prop:) -> {
      use e <- result.map(emit_expr(e, object))
      let e = emit_op(e, opcode.Swap)
      let e = emit_put_field(e, prop)
      emit_op(e, opcode.Pop)
    }

    // PutElem wants [val, key, obj], built with swaps
    ComputedMember(object:, key:) -> {
      use e <- result.try(emit_expr(e, object))
      let e = emit_op(e, opcode.Swap)
      use e <- result.map(emit_expr(e, key))
      let e = emit_op(e, opcode.Swap)
      let e = emit_op(e, opcode.PutElem)
      emit_op(e, opcode.Pop)
    }

    PlainTarget(target) ->
      case target {
        ast.Identifier(name:, ..) -> Ok(emit_var_put(e, name))

        ast.AssignmentExpression(_, ast.Assign, left, default_expr) -> {
          let name = case left {
            ast.Identifier(name:, ..) -> Some(name)
            _ -> None
          }
          use e <- result.try(emit_default_if_undefined(e, default_expr, name))
          emit_destructuring_assign(e, left)
        }

        ast.ArrayExpression(_, elements) -> {
          use e, close_throw <- with_iterator_scaffold(e)
          emit_array_assign_elements(e, elements, close_throw)
        }

        ast.ObjectExpression(_, properties) -> {
          let has_rest =
            list.any(properties, fn(p) {
              case p {
                ast.SpreadProperty(_) -> True
                ast.InitProperty(..)
                | ast.MethodProperty(..)
                | ast.AccessorProperty(..) -> False
              }
            })
          use e, prop, excluded_key_count <- emit_object_pattern(
            e,
            properties,
            has_rest:,
          )
          emit_single_object_assign_prop(e, prop, has_rest, excluded_key_count)
        }

        // e.g. for (f() of ..)
        ast.CallExpression(..) as call -> {
          use e <- result.map(emit_call_then_reference_error(
            e,
            call,
            "Invalid left-hand side in assignment",
          ))
          emit_op(e, opcode.Pop)
        }

        // §13.15.5 early error the parser cannot always catch, e.g. ({...5} = {})
        _ -> Error(EarlySyntaxError("Invalid destructuring assignment target"))
      }
  }
}

fn emit_array_assign_elements(
  e: Emitter,
  elements: List(Option(ast.Expression)),
  close_throw: LabelId,
) -> Result(#(Bool, Emitter), EmitError) {
  use e, el <- emit_array_pattern_elements(e, elements)
  case el {
    ast.SpreadElement(argument:, ..) ->
      emit_array_assign_rest(e, ast_util.unwrap_parens(argument), close_throw)
    target -> {
      use e <- result.map(emit_array_assign_element(
        e,
        ast_util.unwrap_parens(target),
      ))
      #(False, e)
    }
  }
}

// member lrefs evaluate before IteratorStep; rot3/unrot4 park iter at guard depth
fn emit_array_assign_element(
  e: Emitter,
  target: ast.Expression,
) -> Result(Emitter, EmitError) {
  case classify_assign_target(target) {
    // [iter] -> [iter, obj] -> next -> [value, iter, obj] -> rot3, swap -> PutField -> [iter]
    StaticMember(object:, prop:) -> {
      use e <- result.map(emit_expr(e, object))
      e
      |> emit_op(opcode.Swap)
      |> emit_op(opcode.IteratorNext)
      |> emit_op(opcode.Pop)
      |> emit_op(opcode.Rot3)
      |> emit_op(opcode.Swap)
      |> emit_put_field(prop)
      |> emit_op(opcode.Pop)
    }
    // [key, obj, iter] -> rot3 -> next -> swap, unrot4 -> [value, key, obj, iter] -> PutElem
    ComputedMember(object:, key:) -> {
      use e <- result.try(emit_expr(e, object))
      use e <- result.map(emit_expr(e, key))
      e
      |> emit_op(opcode.Rot3)
      |> emit_op(opcode.IteratorNext)
      |> emit_op(opcode.Pop)
      |> emit_op(opcode.Swap)
      |> emit_op(opcode.Unrot4)
      |> emit_op(opcode.PutElem)
      |> emit_op(opcode.Pop)
    }
    // only a computed super key deviates from lref-before-step order
    SuperMember(..) | PlainTarget(_) -> {
      let e = emit_op(e, opcode.IteratorNext)
      let e = emit_op(e, opcode.Pop)
      emit_destructuring_assign(e, target)
    }
  }
}

// non-pattern rest targets evaluate their ref before draining; pop F_body before IteratorRest
fn emit_array_assign_rest(
  e: Emitter,
  target: ast.Expression,
  close_throw: LabelId,
) -> Result(#(Bool, Emitter), EmitError) {
  case classify_assign_target(target) {
    // [iter] -> [obj, iter] -> swap -> PopTry -> IteratorRest -> PutField
    StaticMember(object:, prop:) -> {
      use e <- result.map(emit_expr(e, object))
      let e = emit_op(e, opcode.Swap)
      let e = emit_op(e, opcode.PopTry)
      let e = emit_op(e, opcode.IteratorRest)
      let e = emit_put_field(e, prop)
      #(True, emit_op(e, opcode.Pop))
    }
    // key may throw, so re-arm the close guard with iter back on top
    ComputedMember(object:, key:) -> {
      use e <- result.try(emit_expr(e, object))
      let e = emit_op(e, opcode.PopTry)
      let e = emit_op(e, opcode.Swap)
      let e = emit_ir(e, IrPushTry(close_throw, IterCloseGuard))
      use e <- result.map(emit_expr(e, key))
      let e = emit_op(e, opcode.Swap)
      let e = emit_op(e, opcode.PopTry)
      let e = emit_op(e, opcode.IteratorRest)
      let e = emit_op(e, opcode.PutElem)
      #(True, emit_op(e, opcode.Pop))
    }
    // identifiers and nested patterns: drain first
    SuperMember(..) | PlainTarget(_) -> {
      let e = emit_op(e, opcode.PopTry)
      let e = emit_op(e, opcode.IteratorRest)
      use e <- result.map(emit_destructuring_assign(e, target))
      #(True, e)
    }
  }
}

fn emit_single_object_assign_prop(
  e: Emitter,
  prop: ast.Property,
  has_rest has_rest: Bool,
  excluded_key_count excluded_key_count: Int,
) -> Result(#(Int, Emitter), EmitError) {
  case prop {
    ast.InitProperty(key: ast.ComputedName(expression:), value:, ..) -> {
      let e = emit_op(e, opcode.Dup)
      use e <- result.try(emit_expr(e, expression))
      // ToPropertyKey fires before the target reference is evaluated
      let e = emit_op(e, opcode.ToPropertyKey)
      emit_elem_key_assign(e, value, has_rest, excluded_key_count)
    }

    ast.InitProperty(key:, value:, ..) ->
      case object_prop_key_name(key) {
        Some(name) -> {
          use e <- result.map(emit_keyed_destructure_assign(e, name, value))
          stash_excluded_key(e, name, has_rest, excluded_key_count)
        }
        None -> {
          let e = emit_op(e, opcode.Dup)
          use e <- result.try(emit_property_key(e, key))
          emit_elem_key_assign(e, value, has_rest, excluded_key_count)
        }
      }

    ast.AccessorProperty(..) | ast.MethodProperty(..) ->
      Error(AccessorInDestructuringPattern)

    ast.SpreadProperty(argument) -> {
      let e = emit_op(e, opcode.ObjectRestCopy(excluded_key_count))
      use e <- result.map(emit_destructuring_assign(e, argument))
      #(0, e)
    }
  }
}

// §13.15.5.6: member target base evaluates before the source read, PutValue last
fn emit_keyed_destructure_assign(
  e: Emitter,
  name: String,
  target: ast.Expression,
) -> Result(Emitter, EmitError) {
  case classify_assign_target(target) {
    // [src] -> [src, obj, src] -> GetField -> [v, obj, src] -> put -> [src]
    StaticMember(object:, prop:) -> {
      let e = emit_op(e, opcode.Dup)
      use e <- result.map(emit_expr(e, object))
      e
      |> emit_op(opcode.Swap)
      |> emit_ir(IrGetField(name))
      |> emit_put_field(prop)
      |> emit_op(opcode.Pop)
    }
    // [src] -> [src, key, obj, src] -> GetField -> [v, key, obj, src] -> PutElem -> [src]
    ComputedMember(object:, key:) -> {
      let e = emit_op(e, opcode.Dup)
      use e <- result.try(emit_expr(e, object))
      let e = emit_op(e, opcode.Swap)
      use e <- result.map(emit_expr(e, key))
      e
      |> emit_op(opcode.Swap)
      |> emit_ir(IrGetField(name))
      |> emit_op(opcode.PutElem)
      |> emit_op(opcode.Pop)
    }
    // identifier/pattern/default/super targets: GetV first is spec-correct
    SuperMember(..) | PlainTarget(_) -> {
      let e = emit_op(e, opcode.Dup)
      let e = emit_ir(e, IrGetField(name))
      emit_destructuring_assign(e, target)
    }
  }
}

// entry [key, src, src, ..keys]
fn emit_elem_key_assign(
  e: Emitter,
  value: ast.Expression,
  has_rest has_rest: Bool,
  excluded_key_count excluded_key_count: Int,
) -> Result(#(Int, Emitter), EmitError) {
  case has_rest {
    False -> {
      use e <- result.map(emit_elem_keyed_target(
        e,
        ast_util.unwrap_parens(value),
      ))
      #(excluded_key_count, e)
    }
    True -> {
      let e = emit_op(e, opcode.GetElemKeep)
      use e <- result.map(emit_destructuring_assign(e, value))
      #(excluded_key_count + 1, emit_keep_computed_key(e))
    }
  }
}

// entry [key, src, src]; exit [src]; lref before GetV for member targets
fn emit_elem_keyed_target(
  e: Emitter,
  target: ast.Expression,
) -> Result(Emitter, EmitError) {
  case classify_assign_target(target) {
    // [key, srcd, src] -> tobj, unrot3 -> GetElem -> [v, tobj, src] -> PutField
    StaticMember(object:, prop:) -> {
      use e <- result.map(emit_expr(e, object))
      e
      |> emit_unrot3
      |> emit_op(opcode.GetElem)
      |> emit_put_field(prop)
      |> emit_op(opcode.Pop)
    }
    // tobj, unrot3, tkey, unrot3 -> GetElem -> [v, tkey, tobj, src] -> PutElem
    ComputedMember(object:, key:) -> {
      use e <- result.try(emit_expr(e, object))
      let e = emit_unrot3(e)
      use e <- result.map(emit_expr(e, key))
      e
      |> emit_unrot3
      |> emit_op(opcode.GetElem)
      |> emit_op(opcode.PutElem)
      |> emit_op(opcode.Pop)
    }
    SuperMember(..) -> {
      let e = emit_op(e, opcode.GetElem)
      emit_destructuring_assign(e, target)
    }
    PlainTarget(bare) ->
      case bare {
        ast.AssignmentExpression(_, ast.Assign, left, default_expr) as assign -> {
          let left_target = classify_assign_target(left)
          case left_target {
            StaticMember(..) | ComputedMember(..) ->
              emit_elem_keyed_member_default(e, left_target, default_expr)
            SuperMember(..) | PlainTarget(_) -> {
              let e = emit_op(e, opcode.GetElem)
              emit_destructuring_assign(e, assign)
            }
          }
        }
        other -> {
          let e = emit_op(e, opcode.GetElem)
          emit_destructuring_assign(e, other)
        }
      }
  }
}

// entry [key, src, src]; exit [src]
fn emit_elem_keyed_member_default(
  e: Emitter,
  target: AssignTarget,
  default_expr: ast.Expression,
) -> Result(Emitter, EmitError) {
  use #(put, e) <- result.try(case target {
    StaticMember(object:, prop:) -> {
      use e <- result.map(emit_expr(e, object))
      let e = emit_unrot3(e)
      #(emit_put_field(_, prop), emit_op(e, opcode.GetElem))
    }
    ComputedMember(object:, key:) -> {
      use e <- result.try(emit_expr(e, object))
      let e = emit_unrot3(e)
      use e <- result.map(emit_expr(e, key))
      let e = emit_unrot3(e)
      #(emit_op(_, opcode.PutElem), emit_op(e, opcode.GetElem))
    }
    SuperMember(..) | PlainTarget(_) -> Error(NonMemberDefaultTarget)
  })
  use e <- result.map(emit_default_if_undefined(e, default_expr, None))
  emit_op(put(e), opcode.Pop)
}

// [a, b, c, ..] -> [b, c, a, ..]
fn emit_unrot3(e: Emitter) -> Emitter {
  e
  |> emit_op(opcode.Rot3)
  |> emit_op(opcode.Rot3)
}

// None for computed or bigint keys; numbers use js_format_float
fn object_prop_key_name(key: ast.PropertyName) -> Option(String) {
  case key {
    ast.IdentifierName(name:, ..) | ast.PrivateName(name:, ..) -> Some(name)
    ast.StringName(value: s, ..) -> Some(s)
    ast.NumberName(value: ast.FiniteNumber(f), ..) ->
      Some(rt_val.js_format_float(f))
    ast.NumberName(value: ast.InfiniteNumber, ..) -> Some("Infinity")
    ast.BigIntName(..) | ast.ComputedName(..) -> None
  }
}

// GetIterator, run elements under a close-on-throw guard, close unless rest drained
fn with_iterator_scaffold(
  e: Emitter,
  emit_elements: fn(Emitter, LabelId) -> Result(#(Bool, Emitter), EmitError),
) -> Result(Emitter, EmitError) {
  let #(close_throw, e) = fresh_label(e)
  let #(done_label, e) = fresh_label(e)
  // PushTry right after so unwind leaves [thrown, iter, ..]
  let e = emit_op(e, opcode.GetIterator)
  let e = emit_ir(e, IrPushTry(close_throw, IterCloseGuard))
  use #(rested, e) <- result.map(emit_elements(e, close_throw))
  let e = case rested {
    True -> emit_ir(e, IrJump(done_label))
    False ->
      // [[Done]] not tracked here, so this also closes exhausted user iterators
      e
      |> emit_op(opcode.PopTry)
      |> emit_op(opcode.IteratorClose)
      |> emit_ir(IrJump(done_label))
  }
  // [thrown, iter, ..]: close then rethrow the original
  let e = emit_ir(e, IrLabel(close_throw))
  let e = emit_op(e, opcode.IteratorCloseThrow)
  emit_ir(e, IrLabel(done_label))
}

// emit_one returns True when a rest element consumed the iterator
fn emit_array_pattern_elements(
  e: Emitter,
  elements: List(Option(el)),
  emit_one: fn(Emitter, el) -> Result(#(Bool, Emitter), EmitError),
) -> Result(#(Bool, Emitter), EmitError) {
  case elements {
    [] -> Ok(#(False, e))
    [None, ..rest] -> {
      let e =
        e
        |> emit_op(opcode.IteratorNext)
        |> emit_op(opcode.Pop)
        |> emit_op(opcode.Pop)
      emit_array_pattern_elements(e, rest, emit_one)
    }
    [Some(el), ..rest] -> {
      use #(rested, e) <- result.try(emit_one(e, el))
      case rested {
        True -> Ok(#(True, e))
        False -> emit_array_pattern_elements(e, rest, emit_one)
      }
    }
  }
}

fn emit_array_elements(
  e: Emitter,
  elements: List(Option(ast.Pattern)),
  binding_kind: BindingKind,
) -> Result(#(Bool, Emitter), EmitError) {
  use e, el <- emit_array_pattern_elements(e, elements)
  case el {
    // pop F_body before draining: no close on any later throw
    ast.RestElement(argument:) -> {
      let e = e |> emit_op(opcode.PopTry) |> emit_op(opcode.IteratorRest)
      use e <- result.map(emit_destructuring_bind(e, argument, binding_kind))
      #(True, e)
    }
    pattern -> {
      let e = e |> emit_op(opcode.IteratorNext) |> emit_op(opcode.Pop)
      use e <- result.map(emit_destructuring_bind(e, pattern, binding_kind))
      #(False, e)
    }
  }
}

fn emit_logical_assign(
  e: Emitter,
  op: ast.LogicalOp,
  lhs: ast.Expression,
  right: ast.Expression,
) -> Result(Emitter, EmitError) {
  case classify_assign_target(lhs) {
    SuperMember(..) ->
      Error(UnsupportedFeature("logical assignment to a super property"))
    StaticMember(..) as target -> {
      use #(put, e) <- result.try(emit_member_get_keep(e, target))
      emit_logical_assign_member(e, op, right, put, put_arg_count: 1)
    }
    ComputedMember(..) as target -> {
      use #(put, e) <- result.try(emit_member_get_keep(e, target))
      emit_logical_assign_member(e, op, right, put, put_arg_count: 2)
    }
    PlainTarget(ast.Identifier(name:, ..)) -> {
      let #(end_label, e) = fresh_label(e)
      with_identifier_read_write(e, name, fn(e) {
        let e = emit_short_circuit_test(e, op, end_label)
        let e = emit_op(e, opcode.Pop)
        emit_named_expr(e, right, name)
      })
      |> result.map(emit_ir(_, IrLabel(end_label)))
    }
    PlainTarget(_) -> Error(UnsupportedFeature("logical assignment target"))
  }
}

// entry [old, ..put-args]; put_arg_count slots dropped on short-circuit
fn emit_logical_assign_member(
  e: Emitter,
  op: ast.LogicalOp,
  right: ast.Expression,
  put: fn(Emitter) -> Emitter,
  put_arg_count put_arg_count: Int,
) -> Result(Emitter, EmitError) {
  let #(short_label, e) = fresh_label(e)
  let #(end_label, e) = fresh_label(e)
  let e = emit_short_circuit_test(e, op, short_label)
  let e = emit_op(e, opcode.Pop)
  use e <- result.map(emit_expr(e, right))
  e
  |> put
  |> emit_ir(IrJump(end_label))
  |> emit_ir(IrLabel(short_label))
  |> emit_drop_under_top(put_arg_count)
  |> emit_ir(IrLabel(end_label))
}

// jump to `to` when truthiness == jump_when, without materialising the boolean
fn emit_test(
  e: Emitter,
  expr: ast.Expression,
  jump_when jump_when: Bool,
  to target: LabelId,
) -> Result(Emitter, EmitError) {
  case expr {
    ast.ParenthesizedExpression(_, inner) ->
      emit_test(e, inner, jump_when:, to: target)
    ast.UnaryExpression(_, ast.LogicalNot, arg) ->
      emit_test(e, arg, jump_when: !jump_when, to: target)
    ast.LogicalExpression(_, ast.LogicalAnd as op, left, right)
    | ast.LogicalExpression(_, ast.LogicalOr as op, left, right) -> {
      let left_short_circuits_when = op == ast.LogicalOr
      let #(skip, e) = fresh_label(e)
      let left_target = case left_short_circuits_when == jump_when {
        True -> target
        False -> skip
      }
      use e <- result.try(emit_test(
        e,
        left,
        jump_when: left_short_circuits_when,
        to: left_target,
      ))
      use e <- result.map(emit_test(e, right, jump_when:, to: target))
      emit_ir(e, IrLabel(skip))
    }
    // == null holds exactly for null or undefined
    ast.BinaryExpression(_, ast.Equal as op, left, right)
    | ast.BinaryExpression(_, ast.NotEqual as op, left, right) -> {
      let jump_when_nullish = { op == ast.Equal } == jump_when
      case nullish_literal(left), nullish_literal(right) {
        True, _ -> emit_nullish_test(e, right, jump_when_nullish, target)
        _, True -> emit_nullish_test(e, left, jump_when_nullish, target)
        False, False -> emit_value_test(e, expr, jump_when, target)
      }
    }
    _ ->
      case const_fold.literal_truthy(expr) {
        Some(truthy) if truthy == jump_when -> Ok(emit_ir(e, IrJump(target)))
        Some(_) -> Ok(e)
        None -> emit_value_test(e, expr, jump_when, target)
      }
  }
}

fn emit_value_test(
  e: Emitter,
  expr: ast.Expression,
  jump_when jump_when: Bool,
  target target: LabelId,
) -> Result(Emitter, EmitError) {
  use e <- result.map(emit_expr(e, expr))
  case jump_when {
    False -> emit_ir(e, IrJumpIfFalse(target))
    True -> emit_ir(e, IrJumpIfTrue(target))
  }
}

fn nullish_literal(expr: ast.Expression) -> Bool {
  case expr {
    ast.NullLiteral(_) | ast.UndefinedExpression(_) -> True
    _ -> False
  }
}

fn emit_nullish_test(
  e: Emitter,
  operand: ast.Expression,
  jump_when_nullish jump_when_nullish: Bool,
  target target: LabelId,
) -> Result(Emitter, EmitError) {
  use e <- result.map(emit_expr(e, operand))
  case jump_when_nullish {
    True -> emit_ir(e, IrJumpIfNullish(target))
    False -> emit_ir(e, IrJumpIfNotNullish(target))
  }
}

// dup-jumps to short_label when the op short-circuits, value kept either way
fn emit_short_circuit_test(
  e: Emitter,
  op: ast.LogicalOp,
  short_label: LabelId,
) -> Emitter {
  let e = emit_op(e, opcode.Dup)
  case op {
    ast.LogicalAnd -> emit_ir(e, IrJumpIfFalse(short_label))
    ast.LogicalOr -> emit_ir(e, IrJumpIfTrue(short_label))
    // no jump-if-not-nullish op, so hop over a short jump
    ast.NullishCoalescing -> {
      let #(go_label, e) = fresh_label(e)
      e
      |> emit_ir(IrJumpIfNullish(go_label))
      |> emit_ir(IrJump(short_label))
      |> emit_ir(IrLabel(go_label))
    }
  }
}

fn compile_class(
  e: Emitter,
  binding_name: Option(String),
  display_name: Option(String),
  super_class: Option(ast.Expression),
  body: List(ast.ClassElement),
) -> Result(Emitter, EmitError) {
  // class bodies are always strict; restore on exit
  let saved_strict = e.strict
  let saved_private_env = e.private_env
  let private_names = ast_util.class_private_names(body)
  let e =
    Emitter(
      ..e,
      strict: True,
      private_env: list.append(private_names, e.private_env),
    )
  // class scope slot layout must match ast_util.class_body_bindings
  let #(save, e) = enter_scope(e, in_block: e.in_block)
  // mint private names now; inner name bound after elements so [C] = 1 hits tdz
  let e =
    list.fold(private_names, e, fn(e, pname) {
      e |> emit_op(opcode.NewPrivateName(pname)) |> emit_var_init(pname)
    })
  let element_keys = ast_util.computed_element_keys(body)
  use #(static_init_idx, e) <- result.map(compile_class_body(
    e,
    display_name,
    super_class,
    body,
    element_keys,
  ))
  // bind inner name after element keys, before static elements
  let e = case binding_name {
    Some(n) -> e |> emit_op(opcode.Dup) |> emit_var_init(n)
    None -> e
  }
  let e = emit_call_static_init(e, static_init_idx)
  let e = leave_scope(e, save)
  Emitter(..e, strict: saved_strict, private_env: saved_private_env)
}

// exit stack [ctor]; static-init child returned, emitted after inner-name binding
fn compile_class_body(
  e: Emitter,
  name: Option(String),
  super_class: Option(ast.Expression),
  body: List(ast.ClassElement),
  computed_keys: List(#(Int, ast.Expression)),
) -> Result(#(Option(Int), Emitter), EmitError) {
  let ast_util.ClassBodyParts(
    constructor: ctor_method,
    instance_methods:,
    static_methods:,
    instance_fields:,
    static_elements:,
  ) = ast_util.classify_class_body(body)

  let #(ctor_params, ctor_body, synth_super_forward) = case ctor_method {
    Some(ast_util.ClassMethodElement(
      fun: ast.FunctionLiteral(params:, body:, ..),
      ..,
    )) -> #(params, body, False)
    None -> #([], default_ctor_body(super_class), option.is_some(super_class))
  }

  // instance fields compile into one init fn bound to <class_fields_init>
  use #(init_idx, e) <- result.try(compile_class_init_fn(
    e,
    list.append(
      private_method_inits(instance_methods),
      field_inits(instance_fields),
    ),
  ))
  let is_derived = option.is_some(super_class)
  let field_init = case init_idx, is_derived {
    None, _ -> NoFieldInit
    Some(_), True -> FieldInitAfterSuper
    Some(_), False -> FieldInitAtStart
  }
  use #(child, e) <- result.try(compile_function_body(
    Emitter(..e, in_implicit_derived_ctor: synth_super_forward),
    name,
    ctor_params,
    StmtsBody(ctor_body),
    shape: ClassCtor(is_derived:, field_init:),
  ))
  let e = Emitter(..e, in_implicit_derived_ctor: False)
  let child =
    CompiledChild(
      ..child,
      is_derived_constructor: is_derived,
      is_class_constructor: True,
    )
  let #(ctor_idx, e) = add_child_function(e, child)

  use e <- result.try(case super_class {
    Some(parent_expr) -> {
      // [parent] -> [parent, ctor] -> SetupDerivedClass -> [ctor]
      use e <- result.map(emit_expr(e, parent_expr))
      e
      |> emit_op(opcode.MakeClosure(ctor_idx))
      |> emit_op(opcode.SetupDerivedClass)
    }
    None ->
      // base ctor home object is ctor.prototype (§15.7.14 step 12)
      Ok(
        e
        |> emit_op(opcode.MakeClosure(ctor_idx))
        |> emit_op(opcode.Dup)
        |> emit_ir(IrGetField("prototype"))
        |> emit_op(opcode.Swap)
        |> emit_op(opcode.MakeMethod)
        |> emit_op(opcode.Swap)
        |> emit_op(opcode.Pop),
      )
  })

  // computed element keys evaluated once here, in source order, into stash consts
  use e <- result.try(
    list.try_fold(computed_keys, e, fn(e, pair) {
      let #(idx, key) = pair
      use e <- result.map(emit_expr(e, key))
      e
      |> emit_op(opcode.ToPropertyKey)
      |> emit_var_init(ast_util.computed_field_const(idx))
    }),
  )

  use e <- result.try(emit_class_methods(
    e,
    instance_methods,
    on_prototype: True,
  ))
  use e <- result.try(emit_class_methods(e, static_methods, on_prototype: False))

  // after methods so the init fn's home object sees them
  let e = emit_attach_field_init(e, init_idx)
  use #(static_init_idx, e) <- result.map(compile_class_init_fn(
    e,
    static_inits(static_elements),
  ))

  #(static_init_idx, e)
}

fn default_ctor_body(
  super_class: Option(ast.Expression),
) -> List(ast.StmtWithLine) {
  case super_class {
    None -> []
    Some(heritage) -> {
      let span = heritage.span
      [
        ast.StmtWithLine(
          0,
          ast.ExpressionStatement(
            expression: ast.CallExpression(
              span:,
              callee: ast.SuperExpression(span:),
              arguments: [
                ast.SpreadElement(
                  span:,
                  argument: ast.Identifier(span:, name: "arguments"),
                ),
              ],
            ),
            directive: None,
          ),
        ),
      ]
    }
  }
}

// [[Initializer]] fn: this = instance or ctor; Some when non-empty
fn compile_class_init_fn(
  e: Emitter,
  inits: List(FieldInit),
) -> Result(#(Option(Int), Emitter), EmitError) {
  case inits {
    [] -> Ok(#(None, e))
    _ -> {
      use #(child, e) <- result.map(compile_function_body(
        e,
        None,
        [],
        FieldInitsBody(inits),
        shape: ClassInitFn,
      ))
      let #(idx, e) = add_child_function(e, child)
      #(Some(idx), e)
    }
  }
}

// without fields write undefined so the const is not tdz
fn emit_attach_field_init(e: Emitter, init_idx: Option(Int)) -> Emitter {
  case init_idx {
    None -> push_const(e, mk_undefined())
    Some(idx) ->
      e
      |> emit_op(opcode.Dup)
      |> emit_ir(IrGetField("prototype"))
      |> emit_op(opcode.MakeClosure(idx))
      |> emit_op(opcode.MakeMethod)
      |> emit_op(opcode.Swap)
      |> emit_op(opcode.Pop)
  }
  |> emit_var_init(class_fields_init)
}

// [ctor] -> [target] for body -> [ctor]
fn with_method_target(
  e: Emitter,
  on_prototype on_prototype: Bool,
  body body: fn(Emitter) -> Result(Emitter, EmitError),
) -> Result(Emitter, EmitError) {
  let e = emit_op(e, opcode.Dup)
  let e = case on_prototype {
    True -> emit_ir(e, IrGetField("prototype"))
    False -> e
  }
  use e <- result.map(body(e))
  emit_op(e, opcode.Pop)
}

// §10.2.9 SetFunctionName prefix for accessors
fn method_display_name(kind: ast.MethodKind, name: String) -> String {
  case kind {
    ast.GetterMethod -> "get " <> name
    ast.SetterMethod -> "set " <> name
    ast.PlainMethod | ast.ConstructorMethod -> name
  }
}

// [fn, #name, target] -> [target]
fn private_define_op(kind: ast.MethodKind) -> opcode.Op {
  case kind {
    ast.GetterMethod -> opcode.DefinePrivateAccessor(opcode.Getter)
    ast.SetterMethod -> opcode.DefinePrivateAccessor(opcode.Setter)
    ast.PlainMethod | ast.ConstructorMethod -> opcode.DefinePrivateMethod
  }
}

// constructor already stripped by classify_class_body
fn emit_class_methods(
  e: Emitter,
  methods: List(ast_util.ClassMethodElement),
  on_prototype on_prototype: Bool,
) -> Result(Emitter, EmitError) {
  use e, method <- list.try_fold(methods, e)
  let ast_util.ClassMethodElement(body_index:, key:, fun:, kind:) = method
  use e <- with_method_target(e, on_prototype)
  case key {
    // instance private methods: closure stashed now, installed per instance by field init
    ast.PrivateName(name:, ..) -> {
      let display_name = method_display_name(kind, name)
      use e <- result.map(emit_method_value(e, fun, Some(display_name)))
      let e = emit_op(e, opcode.MakeMethod)
      case on_prototype {
        True -> emit_var_init(e, ast_util.private_fn_const(kind, name))
        False ->
          emit_var_get(e, name)
          |> emit_op(opcode.Swap)
          |> emit_op(private_define_op(kind))
      }
    }
    ast.IdentifierName(name:, ..) | ast.StringName(value: name, ..) -> {
      let display_name = method_display_name(kind, name)
      use e <- result.map(emit_method_value(e, fun, Some(display_name)))
      case kind {
        ast.GetterMethod ->
          emit_ir(e, IrDefineAccessor(name, opcode.Getter, enumerable: False))
        ast.SetterMethod ->
          emit_ir(e, IrDefineAccessor(name, opcode.Setter, enumerable: False))
        ast.PlainMethod | ast.ConstructorMethod ->
          emit_ir(e, IrDefineMethod(name))
      }
    }
    // function name left None: SetFunctionName from runtime keys not implemented
    ast.NumberName(..) | ast.BigIntName(..) | ast.ComputedName(..) -> {
      use e <- result.try(emit_class_element_key(e, key, body_index))
      use e <- result.map(emit_method_value(e, fun, None))
      case kind {
        ast.GetterMethod ->
          emit_op(
            e,
            opcode.DefineAccessorComputed(opcode.Getter, enumerable: False),
          )
        ast.SetterMethod ->
          emit_op(
            e,
            opcode.DefineAccessorComputed(opcode.Setter, enumerable: False),
          )
        ast.PlainMethod | ast.ConstructorMethod ->
          emit_op(e, opcode.DefineMethodComputed)
      }
    }
  }
}

// computed keys read back from the stash const, never re-evaluated
fn emit_class_element_key(
  e: Emitter,
  key: ast.PropertyName,
  body_index: Int,
) -> Result(Emitter, EmitError) {
  case key {
    ast.ComputedName(..) ->
      Ok(emit_var_get(e, ast_util.computed_field_const(body_index)))
    ast.IdentifierName(..)
    | ast.StringName(..)
    | ast.NumberName(..)
    | ast.BigIntName(..)
    | ast.PrivateName(..) -> emit_property_key(e, key)
  }
}

fn emit_call_static_init(e: Emitter, init_idx: Option(Int)) -> Emitter {
  case init_idx {
    None -> e
    Some(idx) ->
      e
      |> emit_op(opcode.Dup)
      |> emit_op(opcode.MakeClosure(idx))
      |> emit_op(opcode.MakeMethod)
      |> emit_op(opcode.CallMethod(0))
      |> emit_op(opcode.Pop)
  }
}

// private methods install before field initializers run
fn private_method_inits(
  methods: List(ast_util.ClassMethodElement),
) -> List(FieldInit) {
  use m <- list.filter_map(methods)
  case m.key {
    ast.PrivateName(name:, ..) ->
      Ok(PrivateMethodInit(
        name:,
        closure_const: ast_util.private_fn_const(m.kind, name),
        kind: m.kind,
      ))
    ast.IdentifierName(..)
    | ast.StringName(..)
    | ast.NumberName(..)
    | ast.BigIntName(..)
    | ast.ComputedName(..) -> Error(Nil)
  }
}

fn field_inits(fields: List(ast_util.ClassFieldElement)) -> List(FieldInit) {
  use field <- list.map(fields)
  field_init_of(field)
}

fn field_init_of(field: ast_util.ClassFieldElement) -> FieldInit {
  let ast_util.ClassFieldElement(body_index:, key:, value:) = field
  let init =
    option.unwrap(value, ast.UndefinedExpression(ast.property_name_span(key)))
  case key {
    ast.PrivateName(name:, ..) -> PrivateFieldInit(name:, init:)
    ast.IdentifierName(name:, ..) | ast.StringName(value: name, ..) ->
      NamedFieldInit(name:, init:)
    ast.NumberName(value: n, ..) -> NumericFieldInit(value: n, init:)
    ast.BigIntName(value: i, ..) -> BigIntFieldInit(value: i, init:)
    ast.ComputedName(..) ->
      ComputedFieldInit(
        key_const: ast_util.computed_field_const(body_index),
        init:,
      )
  }
}

fn static_inits(elements: List(ast_util.StaticElement)) -> List(FieldInit) {
  use elem <- list.map(elements)
  case elem {
    ast_util.StaticFieldElement(field) -> field_init_of(field)
    ast_util.StaticBlockElement(body) -> StaticBlockInit(body)
  }
}

// stack-neutral: push this, define, pop
fn emit_field_init(e: Emitter, fi: FieldInit) -> Result(Emitter, EmitError) {
  case fi {
    // static block is an arrow iife, takes this from the wrapper
    StaticBlockInit(body:) -> {
      use e <- result.map(emit_expr(e, static_block_iife(body)))
      emit_op(e, opcode.Pop)
    }
    PrivateMethodInit(name:, closure_const:, kind:) -> {
      use e <- with_this_pushed(e)
      e
      |> emit_var_get(name)
      |> emit_var_get(closure_const)
      |> emit_op(private_define_op(kind))
      |> Ok
    }
    PrivateFieldInit(name:, init:) -> {
      use e <- with_this_pushed(e)
      let e = emit_var_get(e, name)
      use e <- result.map(emit_named_expr(e, init, name))
      emit_op(e, opcode.DefinePrivateField)
    }
    NamedFieldInit(name:, init:) -> {
      use e <- with_this_pushed(e)
      use e <- result.map(emit_named_expr(e, init, name))
      emit_ir(e, IrDefineField(name))
    }
    NumericFieldInit(value: n, init:) ->
      emit_computed_field_define(
        e,
        push_const(_, const_fold.number_const(n)),
        init,
      )
    ComputedFieldInit(key_const:, init:) ->
      emit_computed_field_define(e, emit_var_get(_, key_const), init)
    BigIntFieldInit(value: i, init:) ->
      emit_computed_field_define(e, push_const(_, mk_bigint(i)), init)
  }
}

// [this] -> [this, key, value] -> DefineFieldComputed
fn emit_computed_field_define(
  e: Emitter,
  emit_key: fn(Emitter) -> Emitter,
  init: ast.Expression,
) -> Result(Emitter, EmitError) {
  use e <- with_this_pushed(e)
  use e <- result.map(emit_expr(emit_key(e), init))
  emit_op(e, opcode.DefineFieldComputed)
}

// stack-neutral: [..] -> [this, ..] for body -> pop
fn with_this_pushed(
  e: Emitter,
  body: fn(Emitter) -> Result(Emitter, EmitError),
) -> Result(Emitter, EmitError) {
  use e <- result.map(body(emit_this_get(e)))
  emit_op(e, opcode.Pop)
}

fn static_block_iife(body: List(ast.StmtWithLine)) -> ast.Expression {
  ast.CallExpression(
    span: ast.Span(0, 0),
    callee: ast.ArrowFunctionExpression(
      span: ast.Span(0, 0),
      params: [],
      body: ast.ArrowBodyBlock(body),
      is_async: False,
    ),
    arguments: [],
  )
}
