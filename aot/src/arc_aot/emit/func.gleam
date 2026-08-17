//// M14: emit_function — JS function/closure → ir.Function + MakeClosure +
//// host("fn_new"). Port of emit.gleam:3190-3650, 5244; compiler.gleam:595-622.
//// D5: uniform [caps…, _frame, _args] IR-param shape (arity 2 after captures).
//// D11: NO fn-entry `maybe_collect` — GC safepoint is turn-boundary only.
//// R7: _frame TupleGet indices 0-based (this=0, af=1, ho=2, nt=3).

import arc/bytecode/lexical
import arc/compiler/ast_util
import arc/compiler/scope.{
  type Binding, type FunctionInfo, type ScopeId, CaptureBinding, CatchBinding,
  ConstBinding, FnNameBinding, LetBinding, ParamBinding, VarBinding,
}
import arc/parser/ast
import arc_aot/emit/anf
import arc_aot/emit/expr
import arc_aot/emit/state.{
  type EmitError, type Emitter2, type FnBody, type FnShape, Arrow, ClassCtor,
  ClassInitFn, Emitter2, ExprBody, FieldInitAfterSuper, FnDecl, FnExpr, Method,
  NoFieldInit, StmtBody,
}
import carder/ir
import gleam/bit_array
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set

// ── feature gates — see expr.gleam ───────────────────────────────────────────

/// `_t` (needs_this) simple-ABI body emit → reject at is_simple_abi_eligible
/// so only `_s` survives. Pair with expr.gleam's emit_call_with_pair
/// needs_this arm.
pub const perf5_code_t: Bool = True

/// perf7 raytrace lever (z): gate `init_arguments` on needs_args_object_*
/// (elides t_new_arguments when every `arguments` ref is
/// `X.apply(Y, arguments)`). False → perf6's strict `refs_args_stmts`.
pub const perf7_args_elide: Bool = True

// ── Result-aware CPS (u-build-seam) — private Rk chain, mirrors anf.Build ───
// anf.Build's tail-k (anf.gleam:17) with the R12 Result channel added. Second
// sanctioned CallHost("js",..) site (host_) alongside anf.host — both are
// single-purpose helpers so the "audit every host call" invariant holds.

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

/// Rk-shaped anf.cons_list — right-fold `MakeCons` onto host("empty_list").
fn cons_list_(
  e: Emitter2,
  vs: List(ir.Value),
  k: Rk(ir.Value),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case vs {
    [] -> host_(e, "empty_list", [], k)
    [head, ..rest] -> {
      use e, tail <- cons_list_(e, rest)
      let_(e, ir.TermOp(ir.MakeCons, [head, tail]), k)
    }
  }
}

/// Right-fold `step` over `items`, threading e, building nested Lets.
/// `then` is labelled so `use e, x, next <- each_(…, then: k)` reads well.
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

/// Run an Rk chain whose leaf calls `done(ef, tree)`.
fn run_rk(
  e: Emitter2,
  f: fn(
    Emitter2,
    fn(Emitter2, ir.Expr) -> Result(#(ir.Expr, Emitter2), EmitError),
  ) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  f(e, fn(ef, tree) { Ok(#(tree, ef)) })
}

// ── FnShape → flags (port emit.gleam:3229-3250) ─────────────────────────────

type ShapeFlags {
  ShapeFlags(
    is_arrow: Bool,
    is_generator: Bool,
    is_async: Bool,
    is_constructor: Bool,
    is_class_constructor: Bool,
    is_derived_constructor: Bool,
    is_method: Bool,
    self_name: Option(String),
  )
}

fn derive_flags(shape: FnShape) -> ShapeFlags {
  case shape {
    FnDecl(is_gen:, is_async:) ->
      ShapeFlags(
        False,
        is_gen,
        is_async,
        !is_gen && !is_async,
        False,
        False,
        False,
        None,
      )
    FnExpr(self_name:, is_gen:, is_async:) ->
      ShapeFlags(
        False,
        is_gen,
        is_async,
        !is_gen && !is_async,
        False,
        False,
        False,
        self_name,
      )
    Arrow(is_async:) ->
      ShapeFlags(True, False, is_async, False, False, False, False, None)
    Method(is_gen:, is_async:) ->
      ShapeFlags(False, is_gen, is_async, False, False, False, True, None)
    ClassCtor(derived:, ..) ->
      ShapeFlags(False, False, False, True, True, derived, False, None)
    ClassInitFn ->
      ShapeFlags(False, False, False, False, False, False, False, None)
  }
}

fn coroutine_kind(sf: ShapeFlags) -> Option(state.CoroutineKind) {
  case sf.is_generator, sf.is_async {
    True, True -> Some(state.CorAsyncGen)
    True, False -> Some(state.CorGenerator)
    False, True -> Some(state.CorAsync)
    False, False -> None
  }
}

/// The coroutine kind of a generator/async shape, None for a plain body.
pub fn shape_coroutine(shape: FnShape) -> Option(state.CoroutineKind) {
  coroutine_kind(derive_flags(shape))
}

pub fn shape_is_arrow(shape: FnShape) -> Bool {
  derive_flags(shape).is_arrow
}

pub fn shape_is_method(shape: FnShape) -> Bool {
  derive_flags(shape).is_method
}

pub fn shape_self_name(shape: FnShape) -> Option(String) {
  derive_flags(shape).self_name
}

/// FieldInitMode for the child body. FieldInitAtStart is NOT set — base-ctor
/// field-init is called by t_construct runtime (SPEC.md:782), not by M14.
fn derive_field_init(
  shape: FnShape,
  parent: state.FieldInitMode,
) -> state.FieldInitMode {
  case shape {
    Arrow(..) ->
      case parent {
        FieldInitAfterSuper -> FieldInitAfterSuper
        _ -> NoFieldInit
      }
    ClassCtor(derived: True, has_field_init: True, ..) -> FieldInitAfterSuper
    _ -> NoFieldInit
  }
}

// ── captures (SPEC §3.1; compiler.gleam:595-622 canonical order) ────────────

/// IR-param name for the i'th capture (D5: precedes `_frame`/`_args`).
pub fn cap_param_name(i: Int) -> String {
  "cap_" <> int.to_string(i)
}

fn capture_count(info: FunctionInfo) -> Int {
  list.length(info.captures) + dict.size(info.lexical_captures)
}

/// SPEC.md:1368/1390: In the PARENT frame, one ir.Value per child capture in
/// canonical order — `info.captures` list-order (each `.1` = parent slot),
/// then `all_lexical_refs`-order subset of `info.lexical_captures` (parent
/// slot from parent's `FunctionInfo.lexical`). Boxed → the cell-handle Var.
pub fn build_capture_values(
  e: Emitter2,
  child_info: FunctionInfo,
) -> List(ir.Value) {
  let named =
    list.map(child_info.captures, fn(c) { ir.Var(state.get_slot_var(e, c.1)) })
  let parent_info = state.fn_info(e)
  let lex =
    list.filter_map(lexical.all_lexical_refs, fn(ref) {
      case dict.has_key(child_info.lexical_captures, ref) {
        False -> Error(Nil)
        True ->
          case lexical.lexical_slot(parent_info.lexical, ref) {
            Some(pslot) -> Ok(ir.Var(state.get_slot_var(e, pslot)))
            None ->
              panic as "emit_2core/fn: lexical capture parent slot missing (analyzer invariant)"
          }
      }
    })
  list.append(named, lex)
}

/// Seed slot_vars in the CHILD frame so reads of a captured name resolve to
/// the corresponding cap_i IR-param.
pub fn seed_capture_slots(e: Emitter2, info: FunctionInfo) -> Emitter2 {
  let #(e, i) =
    list.fold(info.captures, #(e, 0), fn(acc, c) {
      let #(e, i) = acc
      let assert Ok(child_slot) = dict.get(info.names, c.0)
        as "emit_2core/fn: capture name missing from FunctionInfo.names"
      #(state.set_slot_var(e, child_slot, cap_param_name(i)), i + 1)
    })
  let #(e, _) =
    list.fold(lexical.all_lexical_refs, #(e, i), fn(acc, ref) {
      let #(e, i) = acc
      case dict.get(info.lexical_captures, ref) {
        Ok(child_slot) -> #(
          state.set_slot_var(e, child_slot, cap_param_name(i)),
          i + 1,
        )
        Error(_) -> #(e, i)
      }
    })
  e
}

/// D5 uniform IR-param shape: `[cap_0.., _frame, _args]`.
pub fn build_ir_params(i: Int, n: Int) -> List(ir.Local) {
  case i < n {
    False -> [ir.Local("_frame", ir.TTerm), ir.Local("_args", ir.TTerm)]
    True -> [ir.Local(cap_param_name(i), ir.TTerm), ..build_ir_params(i + 1, n)]
  }
}

// ── prologue steps (Rk-CPS; port emit.gleam:1477-1543,3342-3623) ────────────

/// Store `val` into `slot` per its binding: unboxed → Let-bind to the slot's
/// canonical var name + set_slot_var; boxed → cell_set on the existing cell.
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
      let name = state.slot_var_name(b.slot)
      use body <- state.map_tree(k(state.set_slot_var(e, b.slot, name)))
      ir.Let([name], ir.Values([val]), body)
    }
  }
}

/// Non-arrow: destructure the R7 _frame 4-tuple into the four owned lexical
/// slots; box each whose `lexical_boxed` bit is set (u-call-abi_1.md:70-76).
/// Arrow: skip — its lexical reads resolve via cap_i (seed_capture_slots).
pub fn unpack_frame(
  e: Emitter2,
  is_arrow: Bool,
  info: FunctionInfo,
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case is_arrow, info.lexical {
    False, lexical.OwnedLexicalSlots(base:) -> {
      use e, ref, next <- each_(e, lexical.all_lexical_refs, then: k)
      let idx = lexical.lexical_ref_offset(ref)
      let slot = base + idx
      use e, raw <- let_(e, ir.TermOp(ir.TupleGet(idx), [ir.Var("_frame")]))
      case state.lexical_is_boxed(e, info, ref) {
        False -> {
          let name = state.slot_var_name(slot)
          use body <- state.map_tree(next(state.set_slot_var(e, slot, name)))
          ir.Let([name], ir.Values([raw]), body)
        }
        True -> {
          use e, cell <- host_(e, "cell_new", [raw])
          let name = state.slot_var_name(slot)
          use body <- state.map_tree(next(state.set_slot_var(e, slot, name)))
          ir.Let([name], ir.Values([cell]), body)
        }
      }
    }
    _, _ -> k(e)
  }
}

/// Port of emit_binding_prologue (emit.gleam:1523-1543) to Rk-CPS: for each
/// binding of `scope_id` in slot order, seed by kind (Var→undef, Let/Const/
/// FnName→tdz, Param/Catch/Capture→skip seed) then cell_new if boxed &&
/// !Capture. Params are handled by unpack_args, captures by seed_capture_slots.
pub fn binding_prologue(
  e: Emitter2,
  scope_id: ScopeId,
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let bindings =
    dict.to_list(scope.get_scope(e.tree, scope_id).bindings)
    |> list.sort(fn(a, b) { int.compare({ a.1 }.slot, { b.1 }.slot) })
  use e, entry, next <- each_(e, bindings, then: k)
  let #(_, b): #(String, Binding) = entry
  let name = state.slot_var_name(b.slot)
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

/// §10.2.11 step 28.f.i.2 (port emit.gleam:3141-3179): after the body
/// var-boundary scope's prologue seeds every body binding, copy the current
/// value of each parameterBinding (params ∪ "arguments" for non-arrows) into
/// the body VarBinding shadowing that name — unless the name is also a direct
/// FunctionDeclaration in the body (step 28.f.i.1 wins). Source slot is
/// resolved via the FUNCTION scope so we read the param, not the shadowing
/// body var just seeded undefined.
fn body_param_copies(
  e: Emitter2,
  declared_param_names: List(String),
  is_arrow: Bool,
  stmts: List(ast.StmtWithLine),
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let body_id = e.cur_scope
  // enter_scope's empty-cursor fallback leaves cur_scope at fn_scope; iterating
  // that would self-copy every param — analyzer/emit desync, emit nothing.
  case body_id == e.fn_scope {
    True -> k(e)
    False -> {
      let parameter_bindings = case is_arrow {
        True -> declared_param_names
        False -> ["arguments", ..declared_param_names]
      }
      let function_names = ast_util.direct_fn_names(stmts)
      let body_bindings =
        dict.to_list(scope.get_scope(e.tree, body_id).bindings)
        |> list.sort(fn(a, b) { int.compare({ a.1 }.slot, { b.1 }.slot) })
      use e, entry, next <- each_(e, body_bindings, then: k)
      let #(bname, b): #(String, Binding) = entry
      let copies =
        b.kind == VarBinding
        && list.contains(parameter_bindings, bname)
        && !list.contains(function_names, bname)
      case copies {
        False -> next(e)
        True ->
          case scope.lookup(e.tree, e.fn_scope, bname) {
            scope.Plain(scope.Local(slot: src_slot, boxed: src_boxed, ..)) -> {
              let src_var = ir.Var(state.get_slot_var(e, src_slot))
              case src_boxed {
                False -> store_slot(e, b, src_var, next)
                True -> {
                  use e, v <- host_(e, "cell_get", [src_var])
                  store_slot(e, b, v, next)
                }
              }
            }
            // Analyzer registered no fn-scope binding for this parameter name
            // (only possible for `arguments` in exotic shapes) — nothing to
            // copy; the prologue's undefined seed stands.
            scope.Plain(scope.Global(_))
            | scope.Plain(scope.EvalEnv(_))
            | scope.WithChain(..) -> next(e)
          }
      }
    }
  }
}

/// §13.2.5.5 named-FnExpr self-binding: initialize the FnNameBinding slot
/// with active_func (frame idx 1). Shadowing is the analyzer's job — if the
/// fn-scope binding for `fname` is not a FnNameBinding, it was shadowed.
fn init_self_name(
  e: Emitter2,
  self_name: Option(String),
  info: FunctionInfo,
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case self_name {
    None -> k(e)
    Some(fname) ->
      case dict.get(scope.get_scope(e.tree, e.fn_scope).bindings, fname) {
        Ok(b) if b.kind == FnNameBinding -> {
          let assert Some(af_slot) =
            lexical.lexical_slot(info.lexical, lexical.RefActiveFunc)
          let af = ir.Var(state.get_slot_var(e, af_slot))
          let e = Emitter2(..e, initialized: set.insert(e.initialized, b.slot))
          store_slot(e, b, af, k)
        }
        _ -> k(e)
      }
  }
}

/// Unpack the _args cons-list per fixed param (SPEC.md:1373-1376). Simple:
/// bind directly + box; non-simple: apply default + dispatch.emit_destructure.
/// Returns the remaining tail for a rest-param.
fn unpack_args(
  e: Emitter2,
  fixed: List(ast.Pattern),
  non_simple: Bool,
  k: fn(Emitter2, ir.Value) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  unpack_args_loop(e, fixed, non_simple, ir.Var("_args"), k)
}

fn unpack_args_loop(
  e: Emitter2,
  params: List(ast.Pattern),
  non_simple: Bool,
  tail: ir.Value,
  k: fn(Emitter2, ir.Value) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case params {
    [] -> k(e, tail)
    [p, ..rest] -> {
      // hd/tl on [] traps — branch on IsEmptyList (i32) first.
      use e, empty <- let_(e, ir.TermOp(ir.IsEmptyList, [tail]))
      use e, raw <- let_(
        e,
        ir.If(
          empty,
          [ir.TTerm],
          ir.Values([e.consts.undef]),
          ir.TermOp(ir.ListHead, [tail]),
        ),
      )
      use e, tail2 <- let_(
        e,
        ir.If(
          empty,
          [ir.TTerm],
          ir.Values([tail]),
          ir.TermOp(ir.ListTail, [tail]),
        ),
      )
      use e <- bind_one_param(e, p, raw, non_simple)
      unpack_args_loop(e, rest, non_simple, tail2, k)
    }
  }
}

fn bind_one_param(
  e: Emitter2,
  p: ast.Pattern,
  raw: ir.Value,
  non_simple: Bool,
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case non_simple {
    // Simple list: every fixed param is IdentifierPattern (ast_util.all_simple_params).
    False -> {
      let assert ast.IdentifierPattern(name:, ..) = p
      let b = fn_scope_binding(e, name)
      case b.is_boxed {
        False -> {
          let vn = state.slot_var_name(b.slot)
          use body <- state.map_tree(k(state.set_slot_var(e, b.slot, vn)))
          ir.Let([vn], ir.Values([raw]), body)
        }
        True -> {
          use e, cell <- host_(e, "cell_new", [raw])
          let vn = state.slot_var_name(b.slot)
          use body <- state.map_tree(k(state.set_slot_var(e, b.slot, vn)))
          ir.Let([vn], ir.Values([cell]), body)
        }
      }
    }
    // Non-simple: real names are LetBindings (tdz-seeded by
    // binding_prologue); defaults and destructuring go through M15.
    True -> {
      use #(dtree, e) <- result.try(e.dispatch.emit_destructure(
        e,
        p,
        raw,
        state.BindLet,
      ))
      use e, _ <- let_(e, dtree)
      k(e)
    }
  }
}

fn bind_rest(
  e: Emitter2,
  rest: Option(ast.Pattern),
  tail: ir.Value,
  non_simple: Bool,
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case rest {
    None -> k(e)
    Some(target) -> {
      use e, arr <- host_(e, "array_from_list", [tail])
      let mode = case non_simple {
        True -> state.BindLet
        False -> state.BindVar
      }
      use #(dtree, e) <- result.try(e.dispatch.emit_destructure(
        e,
        target,
        arr,
        mode,
      ))
      use e, _ <- let_(e, dtree)
      k(e)
    }
  }
}

// ── §8.6.3 ContainsArguments — AST walk (arrows transparent, non-arrows opaque)
// The parser unconditionally sb_declares the synthetic `arguments` VarBinding
// (parser.gleam:3042), so `dict.get(fn_scope.bindings,"arguments")` is never
// Error for a non-arrow. Mirror emit.gleam:3591's `references_arguments` here
// with a pre-emission walk so functions that never touch `arguments` skip the
// per-call `host("new_arguments")` + mapped-cells build. Direct-eval calls
// count as a reference (eval body may read `arguments` dynamically).

fn refs_args_opt(oe: Option(ast.Expression)) -> Bool {
  case oe {
    Some(e) -> refs_args_expr(e)
    None -> False
  }
}

fn refs_args_key(k: ast.PropertyKey) -> Bool {
  case k {
    ast.KeyComputed(expression:) -> refs_args_expr(expression)
    ast.KeyIdentifier(..)
    | ast.KeyString(..)
    | ast.KeyNumber(..)
    | ast.KeyBigInt(..)
    | ast.KeyPrivate(..) -> False
  }
}

fn refs_args_mprop(p: ast.MemberProperty) -> Bool {
  case p {
    ast.Bracket(expression:) -> refs_args_expr(expression)
    ast.Dot(..) -> False
  }
}

fn refs_args_pattern(p: ast.Pattern) -> Bool {
  case p {
    ast.IdentifierPattern(..) -> False
    ast.AssignmentPattern(left:, right:) ->
      refs_args_pattern(left) || refs_args_expr(right)
    ast.RestElement(argument:) -> refs_args_pattern(argument)
    ast.ArrayPattern(elements:) ->
      list.any(elements, fn(el) {
        case el {
          Some(ep) -> refs_args_pattern(ep)
          None -> False
        }
      })
    ast.ObjectPattern(properties:) ->
      list.any(properties, fn(pp) {
        case pp {
          ast.PatternProperty(key:, value:, ..) ->
            refs_args_key(key) || refs_args_pattern(value)
          ast.RestProperty(..) -> False
        }
      })
  }
}

fn refs_args_decls(ds: List(ast.VariableDeclarator)) -> Bool {
  list.any(ds, fn(d) { refs_args_pattern(d.id) || refs_args_opt(d.init) })
}

fn refs_args_for_init(fi: ast.ForInit) -> Bool {
  case fi {
    ast.ForInitExpression(e) -> refs_args_expr(e)
    ast.ForInitDeclaration(declarations:, ..) -> refs_args_decls(declarations)
    ast.ForInitPattern(p) -> refs_args_pattern(p)
  }
}

fn refs_args_class_body(body: List(ast.ClassElement)) -> Bool {
  // Computed keys evaluate in the enclosing scope; method/field-init/static
  // block bodies own (or forbid — §15.7.1) their own `arguments`.
  list.any(body, fn(el) {
    case el {
      ast.ClassMethod(key:, ..) -> refs_args_key(key)
      ast.ClassField(key:, ..) -> refs_args_key(key)
      ast.StaticBlock(..) -> False
    }
  })
}

fn refs_args_expr(e: ast.Expression) -> Bool {
  case e {
    ast.Identifier(name: "arguments", ..) -> True
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
    ast.ParenthesizedExpression(expression:, ..) -> refs_args_expr(expression)
    ast.BinaryExpression(left:, right:, ..)
    | ast.LogicalExpression(left:, right:, ..)
    | ast.AssignmentExpression(left:, right:, ..) ->
      refs_args_expr(left) || refs_args_expr(right)
    ast.UnaryExpression(argument:, ..)
    | ast.UpdateExpression(argument:, ..)
    | ast.AwaitExpression(argument:, ..)
    | ast.SpreadElement(argument:, ..) -> refs_args_expr(argument)
    ast.YieldExpression(argument:, ..) -> refs_args_opt(argument)
    ast.ConditionalExpression(condition:, consequent:, alternate:, ..) ->
      refs_args_expr(condition)
      || refs_args_expr(consequent)
      || refs_args_expr(alternate)
    ast.SequenceExpression(expressions:, ..) ->
      list.any(expressions, refs_args_expr)
    // Direct-eval poisons: `eval(…)` in this body/arrow can read `arguments`.
    ast.CallExpression(callee: ast.Identifier(name: "eval", ..), arguments:, ..) ->
      True || list.any(arguments, refs_args_expr)
    // NOTE: `X.apply(Y, arguments)` is NOT elided here even though the
    // emit-side fast-path forwards raw `_args` — this walker also gates
    // is_simple_abi_eligible (must reject) and recurses into arrows (where
    // the fast-path cannot fire). Eliding here desyncs scan and emit.
    ast.CallExpression(callee:, arguments:, ..)
    | ast.OptionalCallExpression(callee:, arguments:, ..)
    | ast.NewExpression(callee:, arguments:, ..) ->
      refs_args_expr(callee) || list.any(arguments, refs_args_expr)
    ast.MemberExpression(object:, property:, ..)
    | ast.OptionalMemberExpression(object:, property:, ..) ->
      refs_args_expr(object) || refs_args_mprop(property)
    ast.ArrayExpression(elements:, ..) ->
      list.any(elements, fn(el) {
        case el {
          Some(x) -> refs_args_expr(x)
          None -> False
        }
      })
    ast.ObjectExpression(properties:, ..) ->
      list.any(properties, fn(p) {
        case p {
          ast.InitProperty(key:, value:, ..) ->
            refs_args_key(key) || refs_args_expr(value)
          ast.MethodProperty(key:, ..) | ast.AccessorProperty(key:, ..) ->
            refs_args_key(key)
          ast.SpreadProperty(argument:) -> refs_args_expr(argument)
        }
      })
    ast.TemplateLiteral(parts:, ..) ->
      list.any(ast.template_expressions(parts), refs_args_expr)
    ast.TaggedTemplateExpression(tag:, parts:, ..) ->
      refs_args_expr(tag)
      || list.any(ast.template_expressions(parts), refs_args_expr)
    ast.ImportExpression(source:, options:, ..) ->
      refs_args_expr(source) || refs_args_opt(options)
    // Non-arrow child owns its own `arguments` — opaque (params + body).
    ast.FunctionExpression(..) -> False
    // Arrow inherits enclosing `arguments` — transparent.
    ast.ArrowFunctionExpression(params:, body:, ..) ->
      list.any(params, refs_args_pattern)
      || case body {
        ast.ArrowBodyExpression(x) -> refs_args_expr(x)
        ast.ArrowBodyBlock(stmts) -> refs_args_stmts(stmts)
      }
    ast.ClassExpression(super_class:, body:, ..) ->
      refs_args_opt(super_class) || refs_args_class_body(body)
  }
}

fn refs_args_stmt(s: ast.Statement) -> Bool {
  case s {
    ast.EmptyStatement
    | ast.BreakStatement(..)
    | ast.ContinueStatement(..)
    | ast.DebuggerStatement -> False
    // Non-arrow child — opaque.
    ast.FunctionDeclaration(..) -> False
    ast.ClassDeclaration(super_class:, body:, ..) ->
      refs_args_opt(super_class) || refs_args_class_body(body)
    ast.ExpressionStatement(expression:, ..) -> refs_args_expr(expression)
    ast.ReturnStatement(argument:) -> refs_args_opt(argument)
    ast.ThrowStatement(argument:) -> refs_args_expr(argument)
    ast.BlockStatement(body:) -> refs_args_stmts(body)
    ast.LabeledStatement(body:, ..) -> refs_args_stmt(body)
    ast.VariableDeclaration(declarations:, ..) -> refs_args_decls(declarations)
    ast.IfStatement(condition:, consequent:, alternate:) ->
      refs_args_expr(condition)
      || refs_args_stmt(consequent)
      || case alternate {
        Some(a) -> refs_args_stmt(a)
        None -> False
      }
    ast.WhileStatement(condition:, body:)
    | ast.DoWhileStatement(condition:, body:) ->
      refs_args_expr(condition) || refs_args_stmt(body)
    ast.WithStatement(object:, body:) ->
      refs_args_expr(object) || refs_args_stmt(body)
    ast.ForStatement(init:, condition:, update:, body:) ->
      case init {
        Some(fi) -> refs_args_for_init(fi)
        None -> False
      }
      || refs_args_opt(condition)
      || refs_args_opt(update)
      || refs_args_stmt(body)
    ast.ForInStatement(left:, right:, body:)
    | ast.ForOfStatement(left:, right:, body:, ..) ->
      refs_args_for_init(left) || refs_args_expr(right) || refs_args_stmt(body)
    ast.SwitchStatement(discriminant:, cases:) ->
      refs_args_expr(discriminant)
      || list.any(cases, fn(c: ast.SwitchCase) {
        refs_args_opt(c.condition) || refs_args_stmts(c.consequent)
      })
    ast.TryStatement(block:, tail:) ->
      refs_args_stmts(block)
      || case tail {
        ast.TryCatch(handler:) -> refs_args_catch(handler)
        ast.TryFinally(finalizer:) -> refs_args_stmts(finalizer)
        ast.TryCatchFinally(handler:, finalizer:) ->
          refs_args_catch(handler) || refs_args_stmts(finalizer)
      }
  }
}

fn refs_args_catch(h: ast.CatchClause) -> Bool {
  case h.param {
    Some(p) -> refs_args_pattern(p)
    None -> False
  }
  || refs_args_stmts(h.body)
}

fn refs_args_stmts(stmts: List(ast.StmtWithLine)) -> Bool {
  list.any(stmts, fn(s) { refs_args_stmt(s.statement) })
}

// ── needs_args_object_* — refs_args_* minus the .apply(_,arguments) shape ───
// Gates init_arguments ONLY (never is_simple_abi_eligible). Returns False for
// a body whose every `arguments` ref is the second arg of an
// `X.apply(Y, arguments)` CallExpression — expr.emit_apply_arguments forwards
// raw `_args` for exactly that shape, so the materialized object is dead
// (raytrace Class.create: 66k× t_new_arguments/run elided). Arrow bodies fall
// back to the strict refs_args_* walker: an arrow's own `_args` param is NOT
// what its inherited `arguments` resolves to (see raw_args_var comment in
// emit_body), so the fast-path cannot cover a ref nested there.

fn needs_args_object_opt(oe: Option(ast.Expression)) -> Bool {
  case oe {
    Some(e) -> needs_args_object_expr(e)
    None -> False
  }
}

fn needs_args_object_key(k: ast.PropertyKey) -> Bool {
  case k {
    ast.KeyComputed(expression:) -> needs_args_object_expr(expression)
    ast.KeyIdentifier(..)
    | ast.KeyString(..)
    | ast.KeyNumber(..)
    | ast.KeyBigInt(..)
    | ast.KeyPrivate(..) -> False
  }
}

fn needs_args_object_mprop(p: ast.MemberProperty) -> Bool {
  case p {
    ast.Bracket(expression:) -> needs_args_object_expr(expression)
    ast.Dot(..) -> False
  }
}

fn needs_args_object_decls(ds: List(ast.VariableDeclarator)) -> Bool {
  list.any(ds, fn(d) {
    refs_args_pattern(d.id) || needs_args_object_opt(d.init)
  })
}

fn needs_args_object_for_init(fi: ast.ForInit) -> Bool {
  case fi {
    ast.ForInitExpression(e) -> needs_args_object_expr(e)
    ast.ForInitDeclaration(declarations:, ..) ->
      needs_args_object_decls(declarations)
    ast.ForInitPattern(p) -> refs_args_pattern(p)
  }
}

fn needs_args_object_expr(e: ast.Expression) -> Bool {
  case e {
    ast.Identifier(name: "arguments", ..) -> True
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
    ast.ParenthesizedExpression(expression:, ..) ->
      needs_args_object_expr(expression)
    ast.BinaryExpression(left:, right:, ..)
    | ast.LogicalExpression(left:, right:, ..)
    | ast.AssignmentExpression(left:, right:, ..) ->
      needs_args_object_expr(left) || needs_args_object_expr(right)
    ast.UnaryExpression(argument:, ..)
    | ast.UpdateExpression(argument:, ..)
    | ast.AwaitExpression(argument:, ..)
    | ast.SpreadElement(argument:, ..) -> needs_args_object_expr(argument)
    ast.YieldExpression(argument:, ..) -> needs_args_object_opt(argument)
    ast.ConditionalExpression(condition:, consequent:, alternate:, ..) ->
      needs_args_object_expr(condition)
      || needs_args_object_expr(consequent)
      || needs_args_object_expr(alternate)
    ast.SequenceExpression(expressions:, ..) ->
      list.any(expressions, needs_args_object_expr)
    // Direct-eval poisons — eval body may read `arguments` dynamically.
    ast.CallExpression(callee: ast.Identifier(name: "eval", ..), ..) -> True
    // The carve-out: `X.apply(Y, arguments)` — mirrors the EXACT reachability
    // of expr.emit_plain_call's fast-path: super.apply routes to the earlier
    // super-member arm; a `?.` in the callee chain routes to emit_chain_root.
    // Either way emit evaluates Identifier("arguments") — no elision.
    ast.CallExpression(
      callee: ast.MemberExpression(
        object: inner,
        property: ast.Dot(name: "apply", ..),
        ..,
      ),
      arguments: [recv, ast.Identifier(name: "arguments", ..)],
      ..,
    ) ->
      case inner {
        ast.SuperExpression(..) -> True
        _ ->
          case ast_util.chain_has_optional(inner) {
            True -> True
            False ->
              needs_args_object_expr(inner) || needs_args_object_expr(recv)
          }
      }
    ast.CallExpression(callee:, arguments:, ..)
    | ast.OptionalCallExpression(callee:, arguments:, ..)
    | ast.NewExpression(callee:, arguments:, ..) ->
      needs_args_object_expr(callee)
      || list.any(arguments, needs_args_object_expr)
    ast.MemberExpression(object:, property:, ..)
    | ast.OptionalMemberExpression(object:, property:, ..) ->
      needs_args_object_expr(object) || needs_args_object_mprop(property)
    ast.ArrayExpression(elements:, ..) ->
      list.any(elements, fn(el) {
        case el {
          Some(x) -> needs_args_object_expr(x)
          None -> False
        }
      })
    ast.ObjectExpression(properties:, ..) ->
      list.any(properties, fn(p) {
        case p {
          ast.InitProperty(key:, value:, ..) ->
            needs_args_object_key(key) || needs_args_object_expr(value)
          ast.MethodProperty(key:, ..) | ast.AccessorProperty(key:, ..) ->
            needs_args_object_key(key)
          ast.SpreadProperty(argument:) -> needs_args_object_expr(argument)
        }
      })
    ast.TemplateLiteral(parts:, ..) ->
      list.any(ast.template_expressions(parts), needs_args_object_expr)
    ast.TaggedTemplateExpression(tag:, parts:, ..) ->
      needs_args_object_expr(tag)
      || list.any(ast.template_expressions(parts), needs_args_object_expr)
    ast.ImportExpression(source:, options:, ..) ->
      needs_args_object_expr(source) || needs_args_object_opt(options)
    // Non-arrow child owns its own `arguments` — opaque.
    ast.FunctionExpression(..) -> False
    // Arrow inherits enclosing `arguments`, but the raw-`_args` fast-path
    // cannot fire for a ref inside it — strict walker, no carve-out.
    ast.ArrowFunctionExpression(..) -> refs_args_expr(e)
    ast.ClassExpression(super_class:, body:, ..) ->
      needs_args_object_opt(super_class) || refs_args_class_body(body)
  }
}

fn needs_args_object_stmt(s: ast.Statement) -> Bool {
  case s {
    ast.EmptyStatement
    | ast.BreakStatement(..)
    | ast.ContinueStatement(..)
    | ast.DebuggerStatement -> False
    ast.FunctionDeclaration(..) -> False
    ast.ClassDeclaration(super_class:, body:, ..) ->
      needs_args_object_opt(super_class) || refs_args_class_body(body)
    ast.ExpressionStatement(expression:, ..) ->
      needs_args_object_expr(expression)
    ast.ReturnStatement(argument:) -> needs_args_object_opt(argument)
    ast.ThrowStatement(argument:) -> needs_args_object_expr(argument)
    ast.BlockStatement(body:) -> needs_args_object_stmts(body)
    ast.LabeledStatement(body:, ..) -> needs_args_object_stmt(body)
    ast.VariableDeclaration(declarations:, ..) ->
      needs_args_object_decls(declarations)
    ast.IfStatement(condition:, consequent:, alternate:) ->
      needs_args_object_expr(condition)
      || needs_args_object_stmt(consequent)
      || case alternate {
        Some(a) -> needs_args_object_stmt(a)
        None -> False
      }
    ast.WhileStatement(condition:, body:)
    | ast.DoWhileStatement(condition:, body:) ->
      needs_args_object_expr(condition) || needs_args_object_stmt(body)
    // with-body: arguments_is_implicit is False under a WithChain resolution,
    // so the fast-path can never fire — strict walker, no carve-out.
    ast.WithStatement(object:, body:) ->
      needs_args_object_expr(object) || refs_args_stmt(body)
    ast.ForStatement(init:, condition:, update:, body:) ->
      case init {
        Some(fi) -> needs_args_object_for_init(fi)
        None -> False
      }
      || needs_args_object_opt(condition)
      || needs_args_object_opt(update)
      || needs_args_object_stmt(body)
    ast.ForInStatement(left:, right:, body:)
    | ast.ForOfStatement(left:, right:, body:, ..) ->
      needs_args_object_for_init(left)
      || needs_args_object_expr(right)
      || needs_args_object_stmt(body)
    ast.SwitchStatement(discriminant:, cases:) ->
      needs_args_object_expr(discriminant)
      || list.any(cases, fn(c: ast.SwitchCase) {
        needs_args_object_opt(c.condition)
        || needs_args_object_stmts(c.consequent)
      })
    ast.TryStatement(block:, tail:) ->
      needs_args_object_stmts(block)
      || case tail {
        ast.TryCatch(handler:) -> needs_args_object_catch(handler)
        ast.TryFinally(finalizer:) -> needs_args_object_stmts(finalizer)
        ast.TryCatchFinally(handler:, finalizer:) ->
          needs_args_object_catch(handler) || needs_args_object_stmts(finalizer)
      }
  }
}

fn needs_args_object_catch(h: ast.CatchClause) -> Bool {
  case h.param {
    Some(p) -> refs_args_pattern(p)
    None -> False
  }
  || needs_args_object_stmts(h.body)
}

fn needs_args_object_stmts(stmts: List(ast.StmtWithLine)) -> Bool {
  list.any(stmts, fn(s) { needs_args_object_stmt(s.statement) })
}

// ── simple-ABI eligibility — refs_frame_* mirrors refs_args_* ───────────────
// True when the walked tree reads any _frame slot (this / new.target / super /
// home-object). `ct` (count-this) parameterizes ThisExpression: `ct=False` ⇒
// only super/new.target/direct-eval trigger — used to gate the `_t` simple-ABI
// variant that passes `this` positionally. Non-arrow child fns are opaque (own
// this/new.target); arrows are transparent (inherit ours); direct-eval poisons.
// import.meta is NOT a frame ref (module-level, resolved via captures).

fn refs_frame_opt(oe: Option(ast.Expression), ct: Bool) -> Bool {
  case oe {
    Some(e) -> refs_frame_expr(e, ct)
    None -> False
  }
}

fn refs_frame_key(k: ast.PropertyKey, ct: Bool) -> Bool {
  case k {
    ast.KeyComputed(expression:) -> refs_frame_expr(expression, ct)
    ast.KeyIdentifier(..)
    | ast.KeyString(..)
    | ast.KeyNumber(..)
    | ast.KeyBigInt(..)
    | ast.KeyPrivate(..) -> False
  }
}

fn refs_frame_mprop(p: ast.MemberProperty, ct: Bool) -> Bool {
  case p {
    ast.Bracket(expression:) -> refs_frame_expr(expression, ct)
    ast.Dot(..) -> False
  }
}

fn refs_frame_pattern(p: ast.Pattern, ct: Bool) -> Bool {
  case p {
    ast.IdentifierPattern(..) -> False
    ast.AssignmentPattern(left:, right:) ->
      refs_frame_pattern(left, ct) || refs_frame_expr(right, ct)
    ast.RestElement(argument:) -> refs_frame_pattern(argument, ct)
    ast.ArrayPattern(elements:) ->
      list.any(elements, fn(el) {
        case el {
          Some(ep) -> refs_frame_pattern(ep, ct)
          None -> False
        }
      })
    ast.ObjectPattern(properties:) ->
      list.any(properties, fn(pp) {
        case pp {
          ast.PatternProperty(key:, value:, ..) ->
            refs_frame_key(key, ct) || refs_frame_pattern(value, ct)
          ast.RestProperty(..) -> False
        }
      })
  }
}

fn refs_frame_decls(ds: List(ast.VariableDeclarator), ct: Bool) -> Bool {
  list.any(ds, fn(d) {
    refs_frame_pattern(d.id, ct) || refs_frame_opt(d.init, ct)
  })
}

fn refs_frame_for_init(fi: ast.ForInit, ct: Bool) -> Bool {
  case fi {
    ast.ForInitExpression(e) -> refs_frame_expr(e, ct)
    ast.ForInitDeclaration(declarations:, ..) ->
      refs_frame_decls(declarations, ct)
    ast.ForInitPattern(p) -> refs_frame_pattern(p, ct)
  }
}

fn refs_frame_class_body(body: List(ast.ClassElement), ct: Bool) -> Bool {
  // Computed keys evaluate in the enclosing scope; method/field-init/static-
  // block bodies own their own this/super/new.target — opaque.
  list.any(body, fn(el) {
    case el {
      ast.ClassMethod(key:, ..) -> refs_frame_key(key, ct)
      ast.ClassField(key:, ..) -> refs_frame_key(key, ct)
      ast.StaticBlock(..) -> False
    }
  })
}

fn refs_frame_expr(e: ast.Expression, ct: Bool) -> Bool {
  case e {
    ast.ThisExpression(..) -> ct
    ast.SuperExpression(..) -> True
    ast.MetaProperty(kind: ast.NewTarget, ..) -> True
    ast.MetaProperty(kind: ast.ImportMeta, ..) -> False
    ast.Identifier(..)
    | ast.NumberLiteral(..)
    | ast.BigIntLiteral(..)
    | ast.StringExpression(..)
    | ast.BooleanLiteral(..)
    | ast.NullLiteral(..)
    | ast.UndefinedExpression(..)
    | ast.RegExpLiteral(..)
    | ast.IntrinsicTemplateObject(..) -> False
    ast.ParenthesizedExpression(expression:, ..) ->
      refs_frame_expr(expression, ct)
    ast.BinaryExpression(left:, right:, ..)
    | ast.LogicalExpression(left:, right:, ..)
    | ast.AssignmentExpression(left:, right:, ..) ->
      refs_frame_expr(left, ct) || refs_frame_expr(right, ct)
    ast.UnaryExpression(argument:, ..)
    | ast.UpdateExpression(argument:, ..)
    | ast.AwaitExpression(argument:, ..)
    | ast.SpreadElement(argument:, ..) -> refs_frame_expr(argument, ct)
    ast.YieldExpression(argument:, ..) -> refs_frame_opt(argument, ct)
    ast.ConditionalExpression(condition:, consequent:, alternate:, ..) ->
      refs_frame_expr(condition, ct)
      || refs_frame_expr(consequent, ct)
      || refs_frame_expr(alternate, ct)
    ast.SequenceExpression(expressions:, ..) ->
      list.any(expressions, refs_frame_expr(_, ct))
    // Direct-eval poisons: `eval(…)` may read this/new.target dynamically.
    ast.CallExpression(callee: ast.Identifier(name: "eval", ..), ..) -> True
    ast.CallExpression(callee:, arguments:, ..)
    | ast.OptionalCallExpression(callee:, arguments:, ..)
    | ast.NewExpression(callee:, arguments:, ..) ->
      refs_frame_expr(callee, ct) || list.any(arguments, refs_frame_expr(_, ct))
    ast.MemberExpression(object:, property:, ..)
    | ast.OptionalMemberExpression(object:, property:, ..) ->
      refs_frame_expr(object, ct) || refs_frame_mprop(property, ct)
    ast.ArrayExpression(elements:, ..) ->
      list.any(elements, fn(el) {
        case el {
          Some(x) -> refs_frame_expr(x, ct)
          None -> False
        }
      })
    ast.ObjectExpression(properties:, ..) ->
      list.any(properties, fn(p) {
        case p {
          ast.InitProperty(key:, value:, ..) ->
            refs_frame_key(key, ct) || refs_frame_expr(value, ct)
          ast.MethodProperty(key:, ..) | ast.AccessorProperty(key:, ..) ->
            refs_frame_key(key, ct)
          ast.SpreadProperty(argument:) -> refs_frame_expr(argument, ct)
        }
      })
    ast.TemplateLiteral(parts:, ..) ->
      list.any(ast.template_expressions(parts), refs_frame_expr(_, ct))
    ast.TaggedTemplateExpression(tag:, parts:, ..) ->
      refs_frame_expr(tag, ct)
      || list.any(ast.template_expressions(parts), refs_frame_expr(_, ct))
    ast.ImportExpression(source:, options:, ..) ->
      refs_frame_expr(source, ct) || refs_frame_opt(options, ct)
    // Non-arrow child owns its own this/new.target — opaque.
    ast.FunctionExpression(..) -> False
    // Arrow inherits enclosing this/new.target/super — transparent.
    ast.ArrowFunctionExpression(params:, body:, ..) ->
      list.any(params, refs_frame_pattern(_, ct))
      || case body {
        ast.ArrowBodyExpression(x) -> refs_frame_expr(x, ct)
        ast.ArrowBodyBlock(stmts) -> refs_frame_stmts(stmts, ct)
      }
    ast.ClassExpression(super_class:, body:, ..) ->
      refs_frame_opt(super_class, ct) || refs_frame_class_body(body, ct)
  }
}

fn refs_frame_stmt(s: ast.Statement, ct: Bool) -> Bool {
  case s {
    ast.EmptyStatement
    | ast.BreakStatement(..)
    | ast.ContinueStatement(..)
    | ast.DebuggerStatement -> False
    // Non-arrow child — opaque.
    ast.FunctionDeclaration(..) -> False
    ast.ClassDeclaration(super_class:, body:, ..) ->
      refs_frame_opt(super_class, ct) || refs_frame_class_body(body, ct)
    ast.ExpressionStatement(expression:, ..) -> refs_frame_expr(expression, ct)
    ast.ReturnStatement(argument:) -> refs_frame_opt(argument, ct)
    ast.ThrowStatement(argument:) -> refs_frame_expr(argument, ct)
    ast.BlockStatement(body:) -> refs_frame_stmts(body, ct)
    ast.LabeledStatement(body:, ..) -> refs_frame_stmt(body, ct)
    ast.VariableDeclaration(declarations:, ..) ->
      refs_frame_decls(declarations, ct)
    ast.IfStatement(condition:, consequent:, alternate:) ->
      refs_frame_expr(condition, ct)
      || refs_frame_stmt(consequent, ct)
      || case alternate {
        Some(a) -> refs_frame_stmt(a, ct)
        None -> False
      }
    ast.WhileStatement(condition:, body:)
    | ast.DoWhileStatement(condition:, body:) ->
      refs_frame_expr(condition, ct) || refs_frame_stmt(body, ct)
    ast.WithStatement(object:, body:) ->
      refs_frame_expr(object, ct) || refs_frame_stmt(body, ct)
    ast.ForStatement(init:, condition:, update:, body:) ->
      case init {
        Some(fi) -> refs_frame_for_init(fi, ct)
        None -> False
      }
      || refs_frame_opt(condition, ct)
      || refs_frame_opt(update, ct)
      || refs_frame_stmt(body, ct)
    ast.ForInStatement(left:, right:, body:)
    | ast.ForOfStatement(left:, right:, body:, ..) ->
      refs_frame_for_init(left, ct)
      || refs_frame_expr(right, ct)
      || refs_frame_stmt(body, ct)
    ast.SwitchStatement(discriminant:, cases:) ->
      refs_frame_expr(discriminant, ct)
      || list.any(cases, fn(c: ast.SwitchCase) {
        refs_frame_opt(c.condition, ct) || refs_frame_stmts(c.consequent, ct)
      })
    ast.TryStatement(block:, tail:) ->
      refs_frame_stmts(block, ct)
      || case tail {
        ast.TryCatch(handler:) -> refs_frame_catch(handler, ct)
        ast.TryFinally(finalizer:) -> refs_frame_stmts(finalizer, ct)
        ast.TryCatchFinally(handler:, finalizer:) ->
          refs_frame_catch(handler, ct) || refs_frame_stmts(finalizer, ct)
      }
  }
}

fn refs_frame_catch(h: ast.CatchClause, ct: Bool) -> Bool {
  case h.param {
    Some(p) -> refs_frame_pattern(p, ct)
    None -> False
  }
  || refs_frame_stmts(h.body, ct)
}

fn refs_frame_stmts(stmts: List(ast.StmtWithLine), ct: Bool) -> Bool {
  list.any(stmts, fn(s) { refs_frame_stmt(s.statement, ct) })
}

fn refs_frame_body(body: FnBody, ct: Bool) -> Bool {
  case body {
    StmtBody(stmts) -> refs_frame_stmts(stmts, ct)
    ExprBody(x) -> refs_frame_expr(x, ct)
  }
}

fn refs_args_body(body: FnBody) -> Bool {
  case body {
    StmtBody(stmts) -> refs_args_stmts(stmts)
    ExprBody(x) -> refs_args_expr(x)
  }
}

/// Some(#(arity, needs_this)) when this function can also be compiled as a
/// positional-args closure that skips _frame/_args entirely: plain FnDecl/
/// FnExpr/Arrow (non-gen, non-async), every param a bare identifier (no rest/
/// default/destructure), and the body never reads `arguments`, `new.target`,
/// or `super`. `needs_this=False` → `jsf_N_s([caps.., p0..pN-1])`;
/// `needs_this=True` → `jsf_N_t([caps.., _this, p0..pN-1])` (body reads ONLY
/// `this` from the frame — passed positionally, no 4-tuple). Over-rejection is
/// a perf miss only — the full-ABI closure is always emitted alongside.
/// Private: callers MUST also gate on `lexical_boxed == no_lexical_refs` (the
/// caller owns FunctionInfo); a Some here alone is NOT sufficient to emit.
fn is_simple_abi_eligible(
  shape: FnShape,
  params: List(ast.Pattern),
  body: FnBody,
) -> Option(#(Int, Bool)) {
  let #(shape_ok, is_arrow) = case shape {
    FnDecl(is_gen: False, is_async: False) -> #(True, False)
    // Named FnExpr rejected: the FnNameBinding slot is seeded tdz and only
    // overwritten via RefActiveFunc (=_frame[1]), which the simple-ABI skips.
    FnExpr(is_gen: False, is_async: False, self_name: None) -> #(True, False)
    Arrow(is_async: False) -> #(True, True)
    _ -> #(False, False)
  }
  case shape_ok {
    False -> None
    True -> {
      let #(fixed, rest) = ast_util.split_trailing_rest(params)
      case rest == None && ast_util.all_simple_params(fixed) {
        False -> None
        True ->
          case refs_args_body(body) || refs_frame_body(body, False) {
            True -> None
            False ->
              // Arrow: `this` resolves via captures — the plain _s body would
              // work, but only if the caller's lexical_boxed gate also holds.
              // Keep arrows conservative (over-reject on `this`): the perf
              // target (richards proto methods) is non-arrow.
              case is_arrow, refs_frame_body(body, True), perf5_code_t {
                True, True, _ -> None
                // needs_this but `_t` gate off → perf4 rejects (frame ABI).
                _, True, False -> None
                _, needs_this, _ -> Some(#(list.length(fixed), needs_this))
              }
          }
      }
    }
  }
}

/// Materialize `arguments` only when the body (or a nested arrow / param
/// default) actually references it — the analyzer's fn-scope binding is
/// unconditional so `dict.get` alone never elides. SPEC.md:1690 arity 2.
fn init_arguments(
  e: Emitter2,
  is_arrow: Bool,
  uses_args: Bool,
  fixed: List(ast.Pattern),
  non_simple: Bool,
  has_rest: Bool,
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case is_arrow || !uses_args {
    True -> k(e)
    False ->
      case dict.get(scope.get_scope(e.tree, e.fn_scope).bindings, "arguments") {
        Error(_) -> k(e)
        Ok(b) -> {
          // Mapped only for sloppy simple param lists (§10.2.11 step 18).
          use e, mapped <- build_mapped_cells(e, fixed, non_simple || has_rest)
          use e, callee <- let_(
            e,
            ir.TermOp(ir.TupleGet(1), [ir.Var("_frame")]),
          )
          use e, args_obj <- host_(e, "new_arguments", [
            ir.Var("_args"),
            mapped,
            callee,
          ])
          store_slot(e, b, args_obj, k)
        }
      }
  }
}

fn build_mapped_cells(
  e: Emitter2,
  fixed: List(ast.Pattern),
  unmapped: Bool,
  k: Rk(ir.Value),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case unmapped || e.strict {
    True -> k(e, e.consts.undef)
    // Mapped: cons-list of the param-slot cell handles (port emit.gleam:3591-
    // 3623). unpack_args ran already, so each simple-param slot_var holds the
    // cell (boxed) or raw value (unboxed — analyzer boxes when aliasing
    // matters; unboxed entries won't alias but the runtime tolerates them).
    False -> {
      let cells =
        list.map(fixed, fn(p) {
          let assert ast.IdentifierPattern(name:, ..) = p
          ir.Var(state.get_slot_var(e, fn_scope_binding(e, name).slot))
        })
      cons_list_(e, cells, k)
    }
  }
}

/// Compile every direct FunctionDeclaration in `stmts` (analyzer's cursor is
/// in fn-decls-first order, emit.gleam:3252-3258) and store the closure into
/// its name's slot. M13 emits FunctionDeclaration as a no-op (SPEC.md:1344).
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
      use #(ctree, e) <- result.try(emit_function_tree(
        e,
        FnDecl(is_gen: is_generator, is_async:),
        Some(name),
        params,
        StmtBody(body),
        child_id,
      ))
      use e, fn_h <- let_(e, ctree)
      // The var-scope: the function scope, or the parameter-body scope of
      // a non-simple parameter list (§10.2.11 step 28).
      let assert Ok(b) =
        dict.get(scope.get_scope(e.tree, e.cur_scope).bindings, name)
        as "emit_2core/fn: hoisted function missing from var-scope bindings"
      store_slot(e, b, fn_h, next)
    }
    _ -> next(e)
  }
}

fn fn_scope_binding(e: Emitter2, name: String) -> Binding {
  let assert Ok(b) =
    dict.get(scope.get_scope(e.tree, e.fn_scope).bindings, name)
    as "emit_2core/fn: name missing from fn-scope bindings"
  b
}

// ── body orchestration ──────────────────────────────────────────────────────

/// Body statements of a function; an arrow's expression body returns it.
pub fn body_stmts(body: FnBody) -> List(ast.StmtWithLine) {
  case body {
    StmtBody(s) -> s
    ExprBody(x) -> [ast.StmtWithLine(0, ast.ReturnStatement(Some(x)))]
  }
}

/// §10.2.11 FunctionDeclarationInstantiation for a frame-ABI body: unpack
/// `_frame`, seed the function scope's bindings and the named-expression
/// self binding, bind params/rest/`arguments`, enter the parameter-body var
/// scope for a non-simple list, and hoist function declarations. `k`
/// receives the emitter positioned for the body statements plus the step
/// that undoes the var-scope entry on the body's final emitter. The
/// coroutine wrapper runs this once at call time and snapshots the locals it
/// leaves; `own_args` is False there since the resumed body has no `_args`
/// list to forward, so every `arguments` reference materializes the object.
pub fn emit_prologue(
  e: Emitter2,
  self_name: Option(String),
  is_arrow: Bool,
  own_args: Bool,
  params: List(ast.Pattern),
  stmts: List(ast.StmtWithLine),
  info: FunctionInfo,
  k: fn(Emitter2, fn(Emitter2) -> Emitter2) ->
    Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let #(fixed, rest_param) = ast_util.split_trailing_rest(params)
  let non_simple = !ast_util.all_simple_params(fixed)
  // needs_args_object_* (not refs_args_*): a body whose only `arguments` refs
  // are `X.apply(Y, arguments)` never reads the materialized object — the
  // emit-side fast-path forwards raw `_args`. Param defaults keep the strict
  // walker (fast-path shape inside a default is exotic; over-reject is safe).
  let uses_args =
    !is_arrow
    && {
      list.any(params, refs_args_pattern)
      || case perf7_args_elide && own_args {
        True -> needs_args_object_stmts(stmts)
        False -> refs_args_stmts(stmts)
      }
    }
  // Expose the raw incoming args cons-list to expr.gleam so
  // `X.apply(Y, arguments)` lowers to a direct call passing `_args`
  // verbatim. Arrows inherit `arguments` via captures — their own `_args`
  // param is the arrow's args, not the value `arguments` resolves to.
  let e = case is_arrow || !own_args {
    True -> e
    False -> Emitter2(..e, raw_args_var: Some("_args"))
  }
  use e <- unpack_frame(e, is_arrow, info)
  use e <- binding_prologue(e, e.fn_scope)
  use e <- init_self_name(e, self_name, info)
  // §10.2.11 step 22 creates `arguments` before the formals are bound (a
  // default may read it). Mapped arguments (simple sloppy lists) need the
  // param cells, so only that shape waits for unpack_args.
  let unmapped = non_simple || rest_param != None
  let init_args = fn(e, when: Bool, k) {
    case when {
      True ->
        init_arguments(e, is_arrow, uses_args, fixed, non_simple, unmapped, k)
      False -> k(e)
    }
  }
  use e <- init_args(e, unmapped)
  use e, tail <- unpack_args(e, fixed, non_simple)
  use e <- bind_rest(e, rest_param, tail, non_simple)
  use e <- init_args(e, !unmapped)
  // §10.2.11 step 28: non-simple params get a body var-boundary Block scope.
  case non_simple {
    False -> {
      use e <- hoist_fn_decls(e, stmts)
      k(e, fn(ef) { ef })
    }
    True -> {
      let #(e, save) = state.enter_scope(e, in_block: False)
      use e <- binding_prologue(e, e.cur_scope)
      let param_names = list.flat_map(params, ast.pattern_bound_names)
      use e <- body_param_copies(e, param_names, is_arrow, stmts)
      use e <- hoist_fn_decls(e, stmts)
      k(e, state.leave_scope(_, save))
    }
  }
}

fn emit_body(
  e: Emitter2,
  sf: ShapeFlags,
  params: List(ast.Pattern),
  body: FnBody,
  info: FunctionInfo,
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let stmts = body_stmts(body)
  let ret_undef = fn(ef: Emitter2) {
    case ef.derived_ctor {
      False -> Ok(#(ir.Return([ef.consts.undef]), ef))
      True -> {
        let #(tree, ef) =
          anf.run(expr.derived_return_value(ef.consts.undef), ef)
        use ef, v <- let_(ef, tree)
        Ok(#(ir.Return([v]), ef))
      }
    }
  }
  use e, finish <- emit_prologue(
    e,
    sf.self_name,
    sf.is_arrow,
    True,
    params,
    stmts,
    info,
  )
  use #(tree, ef) <- result.map(e.dispatch.emit_stmts(e, stmts, ret_undef))
  #(tree, finish(ef))
}

// ── simple-ABI body: positional params, no _frame/_args ─────────────────────

fn simple_param_name(i: Int) -> String {
  "_p" <> int.to_string(i)
}

/// IR-param name for the positional `this` in a `_t`-variant simple body.
pub const simple_this_param = "_this"

fn build_simple_ir_params(
  i: Int,
  ncap: Int,
  arity: Int,
  needs_this: Bool,
) -> List(ir.Local) {
  case i < ncap {
    True -> [
      ir.Local(cap_param_name(i), ir.TTerm),
      ..build_simple_ir_params(i + 1, ncap, arity, needs_this)
    ]
    False -> {
      let ps = build_simple_pos_params(0, arity)
      case needs_this {
        True -> [ir.Local(simple_this_param, ir.TTerm), ..ps]
        False -> ps
      }
    }
  }
}

fn build_simple_pos_params(i: Int, arity: Int) -> List(ir.Local) {
  case i < arity {
    True -> [
      ir.Local(simple_param_name(i), ir.TTerm),
      ..build_simple_pos_params(i + 1, arity)
    ]
    False -> []
  }
}

/// Bind each simple identifier param directly from its positional `_p{i}`
/// IR-param (mirrors `bind_one_param`'s simple branch — cell_new if boxed).
fn bind_simple_params(
  e: Emitter2,
  fixed: List(ast.Pattern),
  i: Int,
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case fixed {
    [] -> k(e)
    [p, ..rest] -> {
      let assert ast.IdentifierPattern(name:, ..) = p
        as "emit_2core/fn: simple-abi param not IdentifierPattern"
      let b = fn_scope_binding(e, name)
      let raw = ir.Var(simple_param_name(i))
      let vn = state.slot_var_name(b.slot)
      let next = fn(e) { bind_simple_params(e, rest, i + 1, k) }
      case b.is_boxed {
        False -> {
          use body <- state.map_tree(next(state.set_slot_var(e, b.slot, vn)))
          ir.Let([vn], ir.Values([raw]), body)
        }
        True -> {
          use e, cell <- host_(e, "cell_new", [raw])
          use body <- state.map_tree(next(state.set_slot_var(e, b.slot, vn)))
          ir.Let([vn], ir.Values([cell]), body)
        }
      }
    }
  }
}

/// Seed the OWNED lexical `this` slot from the positional `_this` IR param so
/// `emit_lexical(RefThis)` resolves to `ir.Var("_this")` without any `_frame`
/// TupleGet. `lexical_boxed == no_lexical_refs` gate guarantees unboxed.
fn seed_simple_this(
  e: Emitter2,
  needs_this: Bool,
  info: FunctionInfo,
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case needs_this, info.lexical {
    True, lexical.OwnedLexicalSlots(base:) -> {
      let slot = base + lexical.lexical_ref_offset(lexical.RefThis)
      k(state.set_slot_var(e, slot, simple_this_param))
    }
    _, _ -> k(e)
  }
}

/// Simple-ABI body: `emit_body` minus `unpack_frame`/`unpack_args`/
/// `init_arguments`/`init_self_name`. Eligibility (is_simple_abi_eligible +
/// the guards in emit_function_tree) guarantees the elided steps are dead.
/// `needs_this=True` → seeds the `this` lexical slot from the `_this` param.
fn emit_simple_body(
  e: Emitter2,
  fixed: List(ast.Pattern),
  body: FnBody,
  needs_this: Bool,
  info: FunctionInfo,
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let stmts = case body {
    StmtBody(s) -> s
    ExprBody(x) -> [ast.StmtWithLine(0, ast.ReturnStatement(Some(x)))]
  }
  let ret_undef = fn(ef: Emitter2) { Ok(#(ir.Return([ef.consts.undef]), ef)) }
  run_rk(e, fn(e, done) {
    use e <- seed_simple_this(e, needs_this, info)
    use e <- binding_prologue(e, e.fn_scope)
    use e <- bind_simple_params(e, fixed, 0)
    use e <- hoist_fn_decls(e, stmts)
    use #(tree, ef) <- result.try(e.dispatch.emit_stmts(e, stmts, ret_undef))
    done(ef, tree)
  })
}

/// Frame-ABI entry for a function whose body lives in its simple-ABI variant:
/// bind `this` from `_frame` when the variant takes it, walk `_args` binding
/// one positional param per cons cell, and tail-call the variant with the
/// captures forwarded. Each level tests once for the end of the list; when
/// the list ends early the remaining params are `undefined`, extras are
/// dropped. No allocation, one branch per param.
fn simple_shim_body(
  target: String,
  ncap: Int,
  arity: Int,
  needs_this: Bool,
  undef: ir.Value,
) -> ir.Expr {
  let caps =
    build_ir_params(0, ncap)
    |> list.take(ncap)
    |> list.map(fn(l) { ir.Var(l.name) })
  let this = case needs_this {
    True -> [ir.Var(simple_this_param)]
    False -> []
  }
  let lead = list.append(caps, this)
  let unpack = shim_walk(target, 0, arity, undef, ir.Var("_args"), lead, [])
  case needs_this {
    True ->
      ir.Let(
        [simple_this_param],
        ir.TermOp(ir.TupleGet(0), [ir.Var("_frame")]),
        unpack,
      )
    False -> unpack
  }
}

/// `bound` holds the positional params bound so far, reversed; `tail` is the
/// unconsumed rest of `_args`.
fn shim_walk(
  target: String,
  i: Int,
  arity: Int,
  undef: ir.Value,
  tail: ir.Value,
  lead: List(ir.Value),
  bound: List(ir.Value),
) -> ir.Expr {
  let call = fn(pos) { ir.ReturnCall(target, list.append(lead, pos)) }
  case i < arity {
    False -> call(list.reverse(bound))
    True -> {
      let p = simple_param_name(i)
      let short =
        list.append(list.reverse(bound), list.repeat(undef, arity - i))
      let more = case i + 1 < arity {
        False ->
          shim_walk(target, i + 1, arity, undef, tail, lead, [
            ir.Var(p),
            ..bound
          ])
        True -> {
          let rest = "_r" <> int.to_string(i)
          ir.Let(
            [rest],
            ir.TermOp(ir.ListTail, [tail]),
            shim_walk(target, i + 1, arity, undef, ir.Var(rest), lead, [
              ir.Var(p),
              ..bound
            ]),
          )
        }
      }
      ir.Let(
        ["_e" <> int.to_string(i)],
        ir.TermOp(ir.IsEmptyList, [tail]),
        ir.If(
          ir.Var("_e" <> int.to_string(i)),
          [ir.TTerm],
          call(short),
          ir.Let([p], ir.TermOp(ir.ListHead, [tail]), more),
        ),
      )
    }
  }
}

// ── closure site (SPEC.md:1388; pure anf.* — dispatch-free) ─────────────────

fn atom_bool(rc: state.RealmConsts, b: Bool) -> ir.Value {
  case b {
    True -> rc.true_
    False -> rc.false_
  }
}

fn emit_closure_site(
  e: Emitter2,
  fn_name: String,
  sf: ShapeFlags,
  is_strict: Bool,
  js_name: Option(String),
  expected_length: Int,
  capture_vals: List(ir.Value),
  simple: Option(#(String, Int, Bool)),
) -> #(ir.Expr, Emitter2) {
  let rc = e.consts
  // FnFlags wire tuple — MUST match arc/rt/types.FnFlags field order exactly
  // (ctor, class_ctor, derived, arrow, method, gen, async, strict).
  let flags = [
    ir.ConstAtom("fn_flags"),
    atom_bool(rc, sf.is_constructor),
    atom_bool(rc, sf.is_class_constructor),
    atom_bool(rc, sf.is_derived_constructor),
    atom_bool(rc, sf.is_arrow),
    atom_bool(rc, sf.is_method),
    atom_bool(rc, sf.is_generator),
    atom_bool(rc, sf.is_async),
    atom_bool(rc, is_strict),
  ]
  let name_bin = case js_name {
    Some(n) -> ir.ConstBinary(bit_array.from_string(n))
    None -> rc.empty_bin
  }
  anf.run(
    {
      use fun <- anf.then(anf.bind(ir.MakeClosure(fn_name, capture_vals, 2)))
      use flags_t <- anf.then(anf.make_tuple(flags))
      // 5th fn_new arg — Option(#(CompiledFn, Int, Bool)) wire term for
      // KCompiled.simple. `needs_this` bumps closure arity by 1 (_this param).
      use simple_v <- anf.then(case simple {
        None -> anf.pure(ir.ConstAtom("none"))
        Some(#(sfn, arity, needs_this)) -> {
          let cls_arity = case needs_this {
            True -> arity + 1
            False -> arity
          }
          use scls <- anf.then(
            anf.bind(ir.MakeClosure(sfn, capture_vals, cls_arity)),
          )
          use inner <- anf.then(
            anf.make_tuple([
              scls,
              ir.ConstI32(arity),
              atom_bool(rc, needs_this),
            ]),
          )
          anf.make_tuple([ir.ConstAtom("some"), inner])
        }
      })
      use fn_h <- anf.then(
        anf.host("fn_new", [
          fun,
          flags_t,
          name_bin,
          ir.ConstI32(expected_length),
          simple_v,
        ]),
      )
      // §10.2.5 MakeConstructor — only FnDecl/FnExpr (!gen && !async) get an
      // own writable .prototype; class ctors get theirs from class_setup.
      case sf.is_constructor && !sf.is_class_constructor {
        True -> anf.host("make_constructor", [fn_h])
        False -> anf.pure(fn_h)
      }
    },
    e,
  )
}

/// §15.1.5 ExpectedArgumentCount — leading params before the first default.
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

// ── entry point ─────────────────────────────────────────────────────────────

/// Lower one JS function. Emits the ir.Function to e.fns_acc; returns the
/// PARENT-frame closure-site tree (Let-chain ending in the KCompiled handle).
/// Callers Let-bind this tree to obtain the fn_h Value (see hoist_fn_decls).
pub fn emit_function_tree(
  e: Emitter2,
  shape: FnShape,
  js_name: Option(String),
  params: List(ast.Pattern),
  body: FnBody,
  fn_scope_id: ScopeId,
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let sf = derive_flags(shape)
  let stmts = case body {
    StmtBody(s) -> s
    ExprBody(_) -> []
  }
  let child_strict = e.strict || ast_util.has_use_strict_directive(stmts)
  let child_info = scope.function_info(e.tree, fn_scope_id)
  // Capture values are read from the PARENT frame BEFORE enter_function.
  let capture_vals = build_capture_values(e, child_info)
  let #(fixed, _) = ast_util.split_trailing_rest(params)
  let exp_len = expected_length(fixed)

  // gen|async → M18 owns the state-machine body; already tree-shaped (R14).
  case coroutine_kind(sf) {
    Some(_) ->
      e.dispatch.emit_async_body(
        e,
        shape,
        js_name,
        params,
        body,
        fn_scope_id,
        capture_vals,
      )
    None -> {
      let #(fn_name, e) = state.fresh_fn_name(e)
      let field_init = derive_field_init(shape, e.field_init)
      let #(e_child, save) =
        state.enter_function(
          e,
          fn_scope_id,
          strict: child_strict,
          is_async: False,
          is_generator: False,
          is_arrow: sf.is_arrow,
        )
      let derived_ctor = sf.is_derived_constructor
      let default_ctor = case shape {
        ClassCtor(default:, ..) -> default
        _ -> False
      }
      let e_child =
        Emitter2(
          ..e_child,
          field_init:,
          derived_ctor:,
          default_ctor:,
          this_tdz: derived_ctor || e_child.this_tdz,
        )
      let e_child = seed_capture_slots(e_child, child_info)
      let ncap = capture_count(child_info)
      // Simple-ABI closure: the body is emitted ONCE with positional _p{i}
      // params instead of _frame/_args, and the frame-ABI function becomes a
      // shim that unpacks `_args` and tail-calls it. self_name/lexical_boxed
      // are extra gates on top of is_simple_abi_eligible — both would read
      // the elided _frame slots.
      let simple_arity = case
        sf.self_name,
        child_info.lexical_boxed == lexical.no_lexical_refs,
        is_simple_abi_eligible(shape, params, body)
      {
        None, True, Some(n) -> Some(n)
        _, _, _ -> None
      }
      use #(e, simple) <- result.try(case simple_arity {
        None -> {
          use #(body_expr, e_child) <- result.try(emit_body(
            e_child,
            sf,
            params,
            body,
            child_info,
          ))
          let e_child =
            state.add_function(
              e_child,
              ir.Function(
                name: fn_name,
                params: build_ir_params(0, ncap),
                result: [ir.TTerm],
                locals: [],
                body: body_expr,
              ),
            )
          Ok(#(state.leave_function(e_child, save), None))
        }
        Some(#(arity, needs_this)) -> {
          let simple_fn_name = case needs_this {
            True -> fn_name <> "_t"
            False -> fn_name <> "_s"
          }
          use #(sbody, e_child) <- result.try(emit_simple_body(
            e_child,
            fixed,
            body,
            needs_this,
            child_info,
          ))
          let e_child =
            state.add_function(
              e_child,
              ir.Function(
                name: simple_fn_name,
                params: build_simple_ir_params(0, ncap, arity, needs_this),
                result: [ir.TTerm],
                locals: [],
                body: sbody,
              ),
            )
          let e_child =
            state.add_function(
              e_child,
              ir.Function(
                name: fn_name,
                params: build_ir_params(0, ncap),
                result: [ir.TTerm],
                locals: [],
                body: simple_shim_body(
                  simple_fn_name,
                  ncap,
                  arity,
                  needs_this,
                  e_child.consts.undef,
                ),
              ),
            )
          Ok(#(
            state.leave_function(e_child, save),
            Some(#(simple_fn_name, arity, needs_this)),
          ))
        }
      })
      Ok(emit_closure_site(
        e,
        fn_name,
        sf,
        child_strict,
        js_name,
        exp_len,
        capture_vals,
        simple,
      ))
    }
  }
}

/// EmitDispatch.emit_function entry point (state.gleam:221-228). Returns the
/// parent-frame closure-site tree; callers Let-bind it via `bridge_expr` to
/// obtain the KCompiled handle Value (SPEC§7.M14:1388).
pub fn emit_function(
  e: Emitter2,
  shape: FnShape,
  js_name: Option(String),
  params: List(ast.Pattern),
  body: FnBody,
  fn_scope_id: ScopeId,
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  emit_function_tree(e, shape, js_name, params, body, fn_scope_id)
}
