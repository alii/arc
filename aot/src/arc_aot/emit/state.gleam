//// M10: Emitter2 threaded state, EmitDispatch (R13), Frame2/label stack,
//// scope cursor, EmitError. Port of emit.gleam:125-310,1295-1475,2226-2314
//// re-shaped for ir.Expr output. D2: NO St/state_var — emit_core owns that.

import arc/bytecode/lexical
import arc/compiler/scope.{type ScopeId, type ScopeTree}
import arc/parser/ast
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}
import twocore/ir

/// Compile-time ir.Value constants for JS sentinel atoms (SPEC §2.3 wire ABI).
/// Built once by `realm_consts()`; carried on Emitter2 so emit_* sites write
/// `e.consts.undef` instead of re-spelling `ir.ConstAtom("undefined")`.
pub type RealmConsts {
  RealmConsts(
    undef: ir.Value,
    null: ir.Value,
    true_: ir.Value,
    false_: ir.Value,
    nan: ir.Value,
    pos_inf: ir.Value,
    neg_inf: ir.Value,
    tdz: ir.Value,
    empty_bin: ir.Value,
    // ir.TagDecl name M17/M19 register for JS exceptions (RULINGS R2).
    js_tag: String,
  )
}

pub fn realm_consts() -> RealmConsts {
  RealmConsts(
    undef: ir.ConstAtom("undefined"),
    null: ir.ConstAtom("null"),
    true_: ir.ConstAtom("true"),
    false_: ir.ConstAtom("false"),
    nan: ir.ConstAtom("js_nan"),
    pos_inf: ir.ConstAtom("js_inf"),
    neg_inf: ir.ConstAtom("js_neg_inf"),
    tdz: ir.ConstAtom("js_tdz"),
    empty_bin: ir.ConstBinary(<<>>),
    js_tag: "js_exn",
  )
}

/// User-reachable emit failures (RULINGS R12: surface as Result, never panic).
/// Subset of arc/compiler/emit.EmitError plus emit_2core-specific desync guard.
pub type EmitError {
  BreakOutsideLoop
  ContinueOutsideLoop
  EarlySyntaxError(message: String)
  UnsupportedFeature(feature: String)
  /// enter_scope popped a cursor id that didn't match the AST walk order.
  ScopeCursorDesync(at: ScopeId)
}

/// Where the synthetic field-initializer call is emitted.
/// Verbatim port of arc/compiler/emit.gleam:314-321.
pub type FieldInitMode {
  /// No instance fields (or not in a constructor body).
  NoFieldInit
  /// Base-class ctor: call init fn at start of body, after lexical declares.
  FieldInitAtStart
  /// Derived-class ctor: call init fn after every `super()`.
  FieldInitAfterSuper
}

/// Per-class-body context threaded through emit_class → method/ctor bodies
/// (SPEC.md:1186-1190). M16 constructs one per ClassExpression; M15/M18 read
/// it via Emitter2.class_stack for `super`, private-name brand checks, and
/// home-object cells.
pub type ClassCtx {
  ClassCtx(
    /// Private-name id → brand-cell IR var (D9: runtime-minted UID cell).
    brand_vars: Dict(String, ir.Value),
    /// HomeObject cell for prototype methods' `super.x` (§10.2.1).
    proto_home_cell: ir.Value,
    /// HomeObject cell for static methods' `super.x`.
    static_home_cell: ir.Value,
    /// Constructor self-reference cell (derived-ctor `this` after super()).
    ctor_self_cell: ir.Value,
    /// Class-body inner binding cell (NamedEvaluation §8.1.15), when named.
    inner_name_cell: Option(ir.Value),
    /// True iff `extends` clause present — gates FieldInitAfterSuper.
    is_derived: Bool,
  )
}

/// Saved scope-cursor position captured at try-entry so a duplicated
/// finally block can re-enter its own scope at each crossing site.
/// Port of emit.gleam:1389-1391 ScopeSave, re-shaped for slot_vars.
pub type ScopeSave2 {
  ScopeSave2(
    cur_scope: ScopeId,
    scope_cursor: List(ScopeId),
    slot_vars: Dict(Int, String),
    in_block: Bool,
  )
}

/// Saved per-function-body state for the enter_function/leave_function
/// round-trip (M10-M11.md:181-182). enter_function resets these to child
/// defaults; leave_function restores the parent's. Module-monotone fields
/// (next_var/next_label/next_fn/fns_acc) and constants (tree/dispatch/consts)
/// are NOT saved — they thread straight through nested compilations so
/// ir.Function names and fns_acc stay globally unique/flat (invariant #1).
pub type FnSave {
  FnSave(
    // scope cursor
    fn_scope: ScopeId,
    cur_scope: ScopeId,
    scope_cursor: List(ScopeId),
    child_fn_cursor: List(ScopeId),
    in_block: Bool,
    // control-flow stack — a break never crosses a function boundary
    frame_stack: List(Frame2),
    pending_label: Option(String),
    // per-body mode flags
    strict: Bool,
    is_async: Bool,
    is_generator: Bool,
    is_arrow: Bool,
    // per-body context stacks
    with_stack: List(String),
    private_env: List(String),
    field_init: FieldInitMode,
    derived_ctor: Bool,
    default_ctor: Bool,
    this_tdz: Bool,
    class_stack: List(ClassCtx),
    // per-function slot mapping (slot indices are function-scope-local)
    slot_vars: Dict(Int, String),
    initialized: Set(Int),
    hoisted_kfn: Dict(Int, ir.Value),
    // M18: nested non-coroutine fn must NOT inherit the parent's SM intercept
    sm_abrupt: Option(SmAbrupt),
    raw_args_var: Option(String),
  )
}

/// Break/continue/return unwind stack. Port of emit.gleam:125-160 Frame,
/// re-shaped for string IR labels + finally-duplication (SPEC.md:1157-1164).
pub type Frame2 {
  /// Iteration statement. `ir_break` names the outer ir.Block; `ir_continue`
  /// names the INNER ir.Block wrapping the body (R15: JS continue → ir.Break to
  /// it, falling through to update/post-test). `carried` is the LoopParam slot
  /// set. `iter_close` holds the iterator cell var for for-of/for-await-of.
  Loop2(
    ir_break: String,
    ir_continue: String,
    js_label: Option(String),
    carried: List(Int),
    iter_close: Option(#(String, Escape)),
  )
  /// switch: break target only; unlabeled `break` targets it, `continue` walks
  /// past to the enclosing loop. Transparent when crossed.
  Switch2(ir_break: String, js_label: Option(String), carried: List(Int))
  /// `foo: { … }` — labeled non-loop block. Only `break foo` targets it;
  /// unlabeled break skips it (§14.8). Transparent when crossed.
  Labeled2(ir_break: String, js_label: String, carried: List(Int))
  /// try/catch/finally or for-of body. Never a target — only crossed.
  /// `finally_body` (when Some) is re-emitted inline at each crossing site
  /// under the saved scope. `iter_close` names an iterator cell to close.
  Barrier2(
    finally_body: Option(#(List(ast.StmtWithLine), ScopeSave2)),
    iter_close: Option(String),
    escape: Option(Escape),
  )
}

/// The landing just outside a frame's ir.Try region. A cleanup inlined at a
/// transfer site inside the region runs under its own ir.Try whose handler
/// `Break`s here with the thrown value, so the region's handler never sees
/// what its own finally block or iterator close threw; the landing rethrows
/// outside. `arity` is how many carried values the landing block yields
/// besides the flag and the exception. None for a marker barrier that owns
/// no region.
pub type Escape {
  Escape(label: String, arity: Int)
}

/// Cleanup a break/continue/return performs when CROSSING a barrier on its
/// way to the target. find_break_target/find_continue_target return these as
/// an ordered list; M13/M17 emit each entry inline (no gosub in structured IR).
pub type BarrierCleanup {
  /// Re-emit `body` at the crossing point, restoring `saved_scope` first so
  /// the finally block sees its own bindings (M17 barrier-duplication).
  FinallyBlock(
    body: List(ast.StmtWithLine),
    saved_scope: ScopeSave2,
    escape: Option(Escape),
  )
  /// for-of / for-await-of iterator to close on abrupt exit; `is_async`
  /// selects the awaited variant (M18).
  IterClose(iter_var: String, is_async: Bool, escape: Option(Escape))
  /// try-catch with no finally: nothing to emit — ir.Try is structured so
  /// Break(label) is already valid; recorded only for completion semantics.
  CatchOnly
}

// ── EmitDispatch (defined here to break the emit_* import cycle) ─────────────

/// Terminal continuation of a statement fold: builds the tail expression and
/// hands back the emitter it finished with.
pub type K =
  fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError)

/// Rewrap the tree of a threaded `#(tree, e)` result, keeping the emitter.
pub fn map_tree(
  r: Result(#(ir.Expr, Emitter2), EmitError),
  f: fn(ir.Expr) -> ir.Expr,
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case r {
    Ok(#(tree, e)) -> Ok(#(f(tree), e))
    Error(err) -> Error(err)
  }
}

pub type BindMode {
  BindLet
  BindConst
  BindVar
  BindAssign
}

pub type FnShape {
  FnDecl(is_gen: Bool, is_async: Bool)
  FnExpr(self_name: Option(String), is_gen: Bool, is_async: Bool)
  Arrow(is_async: Bool)
  Method(is_gen: Bool, is_async: Bool)
  /// `default`: the synthesized constructor of a class with none, whose
  /// derived form forwards its arguments to the parent unobservably.
  ClassCtor(derived: Bool, has_field_init: Bool, default: Bool)
  ClassInitFn
}

pub type FnBody {
  StmtBody(List(ast.StmtWithLine))
  ExprBody(ast.Expression)
}

pub type CoroutineKind {
  CorGenerator
  CorAsync
  CorAsyncGen
}

/// M18 abrupt-completion intercept. Installed by async.gleam before delegating
/// a split-free fragment to `dispatch.emit_stmts`, so `return`/`break`/
/// `continue` inside it route to the state-machine (step_return / Continue to
/// state N) instead of M13's bare `ir.Return`/`ir.Break`. `on_goto` is called
/// with the resolved ir label; it returns None for non-SM (fragment-local)
/// labels — M13 falls through to its normal `ir.Break` — and Some for sentinel
/// labels async.gleam pushed for split-spanning loops/labeled/switch.
pub type SmAbrupt {
  SmAbrupt(
    on_return: fn(Emitter2, ir.Value) -> Result(#(ir.Expr, Emitter2), EmitError),
    on_goto: fn(Emitter2, String) ->
      Option(Result(#(ir.Expr, Emitter2), EmitError)),
  )
}

/// D13 mutual-recursion break. R12: every field is Result-wrapped so early
/// errors surface as `Error(EarlySyntaxError(..))`, never `panic`.
pub type EmitDispatch {
  EmitDispatch(
    emit_expr: fn(Emitter2, ast.Expression) ->
      Result(#(ir.Expr, Emitter2), EmitError),
    /// `emit_expr` under NamedEvaluation (§8.4.5): the name an anonymous
    /// function/class expression takes from its binding position.
    emit_expr_named: fn(Emitter2, ast.Expression, Option(String)) ->
      Result(#(ir.Expr, Emitter2), EmitError),
    emit_stmts: fn(Emitter2, List(ast.StmtWithLine), K) ->
      Result(#(ir.Expr, Emitter2), EmitError),
    emit_pattern: fn(Emitter2, ast.Pattern, ir.Value, BindMode) ->
      Result(#(ir.Expr, Emitter2), EmitError),
    emit_function: fn(
      Emitter2,
      FnShape,
      Option(String),
      List(ast.Pattern),
      FnBody,
      ScopeId,
    ) -> Result(#(ir.Expr, Emitter2), EmitError),
    emit_class: fn(
      Emitter2,
      Option(String),
      Option(String),
      Option(ast.Expression),
      List(ast.ClassElement),
    ) -> Result(#(ir.Expr, Emitter2), EmitError),
    emit_async_body: fn(
      Emitter2,
      FnShape,
      Option(String),
      List(ast.Pattern),
      FnBody,
      ScopeId,
      List(ir.Value),
    ) -> Result(#(ir.Expr, Emitter2), EmitError),
    emit_destructure: fn(Emitter2, ast.Pattern, ir.Value, BindMode) ->
      Result(#(ir.Expr, Emitter2), EmitError),
  )
}

// ── Emitter2 (reconciled union: SPEC.md §7.M10 ∪ M10-M11.md) ────────────────

/// Threaded through every emit_* as `#(ir.Expr, Emitter2)`. Non-opaque so
/// M12-M18 read `e.dispatch.*` / `e.consts.*` directly and M19 constructs it.
/// D2 (Arch-A): NO state_var/st_cur — emit_core (M9) alone owns St threading.
pub type Emitter2 {
  Emitter2(
    // ── scope cursor (verbatim port emit.gleam:206-228,298-304) ──
    tree: ScopeTree,
    fn_scope: ScopeId,
    cur_scope: ScopeId,
    scope_cursor: List(ScopeId),
    child_fn_cursor: List(ScopeId),
    in_block: Bool,
    // ── name generation (ir.gleam:419-420 uniqueness) ──
    next_var: Int,
    next_label: Int,
    /// module-monotone — survives enter_function
    next_fn: Int,
    /// module-monotone tagged-template site counter (§13.2.8.4 cache key)
    next_site: Int,
    module_name: String,
    // ── control-flow stack (per-function; enter_function saves+clears) ──
    frame_stack: List(Frame2),
    /// Set by LabeledStatement (via set_pending_label) before emitting its body.
    /// Consumed by push_loop/push_switch/push_labeled; push_barrier is
    /// transparent to it (M10-M11.md gotcha #6).
    pending_label: Option(String),
    // ── module-wide function accumulator (survives enter_function) ──
    fns_acc: List(ir.Function),
    /// Unsupported features met inside expression position, where the ANF
    /// builder has no error channel. `compile` fails the unit if non-empty.
    unsupported: List(String),
    // ── per-body mode flags (reset by enter_function) ──
    strict: Bool,
    is_async: Bool,
    is_generator: Bool,
    is_arrow: Bool,
    with_stack: List(String),
    private_env: List(String),
    field_init: FieldInitMode,
    /// This body is a derived class constructor: `return` yields `this`
    /// for an undefined result (§10.2.2 steps 10-12).
    derived_ctor: Bool,
    /// This body is a synthesized default constructor (§15.7.14 step 14.a):
    /// `super` receives the incoming argument list as-is, with no spread.
    default_ctor: Bool,
    /// `this` may still be unbound here (a derived constructor, or an arrow
    /// nested in one), so reads of it are checked.
    this_tdz: Bool,
    // ── slot mapping (unboxed-rebindable local → current IR var name) ──
    slot_vars: Dict(Int, String),
    /// TDZ-elision (emit.gleam:1841)
    initialized: Set(Int),
    /// IR var names statically known to hold a BEAM number term (int|float).
    /// Seeded by anf.bind_number/mark_number; read by anf.guarded_binop/cmp
    /// to elide `is_number` TermTests on the M0 sum hot path.
    known_numbers: Set(String),
    /// Unboxed-local slot → pre-computed `kfn_code` pair var, hoisted before a
    /// loop so calls in the body reuse it. Per-function; cleared on enter.
    hoisted_kfn: Dict(Int, ir.Value),
    /// Top-level `var NAME = <literal>` bindings NEVER reassigned in the
    /// script — reads inline the literal instead of a global-object lookup.
    /// Module-wide; computed once by expr.analyze_const_globals at compile
    /// entry, never mutated.
    const_globals: Dict(String, ir.Value),
    /// Optimization G: top-level `var`/`function` name → root-scope slot.
    /// Module-wide; survives enter_function. Populated from the root scope's
    /// VarBinding entries at compile entry (empty when the scope tree was
    /// built without `module_slot_globals`, so the redirect self-disables).
    slotted_globals: Dict(String, Int),
    // ── class context (M12 super/private/new.target) ──
    class_stack: List(ClassCtx),
    // ── M18 SM intercept (per-function; None outside coroutine sm bodies) ──
    sm_abrupt: Option(SmAbrupt),
    /// IR var name of this frame-ABI body's raw incoming args cons-list
    /// (`_args`). Set by func.emit_body for non-arrows so `X.apply(Y,
    /// arguments)` lowers to a direct call passing this list verbatim. None
    /// in arrows (their `arguments` resolves via captures) and simple-ABI
    /// bodies (no `_args` param). Per-function; reset by enter_function.
    raw_args_var: Option(String),
    // ── D13/R13 mutual-recursion break ──
    dispatch: EmitDispatch,
    consts: RealmConsts,
  )
}

/// Record IR var `name` as holding a BEAM number term. Monotone: never
/// cleared per-function (var names are module-unique via next_var so a stale
/// mark on an out-of-scope name is unreachable, never a false positive).
pub fn mark_known_number(e: Emitter2, name: String) -> Emitter2 {
  Emitter2(..e, known_numbers: set.insert(e.known_numbers, name))
}

pub fn is_known_number(e: Emitter2, name: String) -> Bool {
  set.contains(e.known_numbers, name)
}

pub fn set_const_globals(e: Emitter2, d: Dict(String, ir.Value)) -> Emitter2 {
  Emitter2(..e, const_globals: d)
}

pub fn set_slotted_globals(e: Emitter2, d: Dict(String, Int)) -> Emitter2 {
  Emitter2(..e, slotted_globals: d)
}

/// Boxed-cell slot for top-level global `name`, or None when `name` isn't
/// slotted (true undeclared refs / const_globals / builtins still route to
/// global_get).
pub fn lookup_slotted_global(e: Emitter2, name: String) -> Option(Int) {
  case dict.get(e.slotted_globals, name) {
    Ok(slot) -> Some(slot)
    Error(_) -> None
  }
}

pub fn fresh_var(e: Emitter2) -> #(String, Emitter2) {
  #("_t" <> int_to_string(e.next_var), Emitter2(..e, next_var: e.next_var + 1))
}

/// Tail Value of a straight `Let…Values([v])` chain (the shape `emit_expr`
/// returns for identifiers/literals). None for If/Block/multi-Values tails.
pub fn let_tail_value(rhs: ir.Expr) -> Option(ir.Value) {
  case rhs {
    ir.Values([v]) -> Some(v)
    ir.Let(_, _, body) -> let_tail_value(body)
    _ -> None
  }
}

/// `rhs`'s Let-spine wrapped around `body` with the tail value discarded:
/// the tree-level form of `let_` for callers that already hold the body.
pub fn splice_let(rhs: ir.Expr, drop: String, body: ir.Expr) -> ir.Expr {
  case rhs {
    ir.Let(names, inner, rest) ->
      ir.Let(names, inner, splice_let(rest, drop, body))
    ir.Values([_]) -> body
    _ -> ir.Let([drop], rhs, body)
  }
}

/// Bind the value of an `anf.run` tree and continue with `k`. The tree binds
/// fresh names (write_slot's unboxed rebind, bind_if's join var, a
/// destructuring store) that `e.slot_vars` leaks to `k`; wrapping the whole
/// tree as one Let-RHS would scope those names to the RHS, where dead-let
/// elimination erases them. So the tree's Let-spine is spliced into the
/// outer chain instead: every top-level Let in `rhs` becomes an outer Let and
/// its name stays in scope for `k`.
pub fn let_(
  e: Emitter2,
  rhs: ir.Expr,
  k: fn(Emitter2, ir.Value) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case rhs {
    ir.Let(names, inner_rhs, inner_body) -> {
      use tail <- map_tree(let_(e, inner_body, k))
      ir.Let(names, inner_rhs, tail)
    }
    ir.Values([v]) -> k(e, v)
    _ -> {
      let #(n, e) = fresh_var(e)
      // Propagate known-number through the alias so anf's marks survive the
      // re-bind into the outer chain.
      let e = case let_tail_value(rhs) {
        Some(ir.Var(vn)) ->
          case is_known_number(e, vn) {
            True -> mark_known_number(e, n)
            False -> e
          }
        _ -> e
      }
      use body <- map_tree(k(e, ir.Var(n)))
      ir.Let([n], rhs, body)
    }
  }
}

pub fn fresh_label(e: Emitter2) -> #(String, Emitter2) {
  #(
    "_L" <> int_to_string(e.next_label),
    Emitter2(..e, next_label: e.next_label + 1),
  )
}

pub fn fresh_fn_name(e: Emitter2) -> #(String, Emitter2) {
  #("jsf_" <> int_to_string(e.next_fn), Emitter2(..e, next_fn: e.next_fn + 1))
}

/// Prepend a lowered ir.Function to the module accumulator. fns_acc is
/// module-monotone (invariant #1): survives enter_function/leave_function.
pub fn add_function(e: Emitter2, f: ir.Function) -> Emitter2 {
  Emitter2(..e, fns_acc: [f, ..e.fns_acc])
}

/// Drain the accumulated functions in emit order (M19 builds ir.Module from
/// this). Reverses fns_acc since add_function prepends.
pub fn take_functions(e: Emitter2) -> List(ir.Function) {
  list.reverse(e.fns_acc)
}

/// Record an unsupported feature seen in expression position.
pub fn mark_unsupported(e: Emitter2, feature: String) -> Emitter2 {
  Emitter2(..e, unsupported: [feature, ..e.unsupported])
}

/// Canonical initial IR var name for scope slot `slot`. The function prologue
/// Let-binds this name; unboxed re-assignment rebinds via set_slot_var.
pub fn slot_var_name(slot: Int) -> String {
  "js_local_" <> int_to_string(slot)
}

/// Current IR var name bound to `slot` — its slot_var_name, or the fresh name
/// of its most recent unboxed re-assignment (SPEC gotcha 1848 unboxed-rebind).
pub fn get_slot_var(e: Emitter2, slot: Int) -> String {
  case dict.get(e.slot_vars, slot) {
    Ok(name) -> name
    Error(_) -> slot_var_name(slot)
  }
}

/// Record `name` as the current IR var for `slot`. M12/M13/M15 call this after
/// every unboxed Let-rebind so later Identifier reads see the new SSA name.
pub fn set_slot_var(e: Emitter2, slot: Int, name: String) -> Emitter2 {
  Emitter2(..e, slot_vars: dict.insert(e.slot_vars, slot, name))
}

/// Record `pair_var` as the hoisted `kfn_code` result for unboxed slot `slot`.
/// M13 loop emitters call this before push_loop; expr.emit_call reads it back.
pub fn set_hoisted_kfn(e: Emitter2, slot: Int, pair_var: ir.Value) -> Emitter2 {
  Emitter2(..e, hoisted_kfn: dict.insert(e.hoisted_kfn, slot, pair_var))
}

/// Hoisted `kfn_code` pair var for `slot`, or None when not loop-invariant.
pub fn lookup_hoisted_kfn(e: Emitter2, slot: Int) -> Option(ir.Value) {
  case dict.get(e.hoisted_kfn, slot) {
    Ok(v) -> Some(v)
    Error(_) -> None
  }
}

/// Drop all hoisted kfn_code entries. M13 calls this after pop_frame so an
/// inner-loop hoist never leaks to a sibling loop.
pub fn clear_hoisted_kfn(e: Emitter2) -> Emitter2 {
  Emitter2(..e, hoisted_kfn: dict.new())
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(i: Int) -> String

// ── frame stack (port emit.gleam:2244-2314) ─────────────────────────────────

/// Prepend a target frame and consume any pending label. Used by
/// push_loop/push_switch/push_labeled — NOT push_barrier.
pub fn push_frame(e: Emitter2, frame: Frame2) -> Emitter2 {
  Emitter2(..e, frame_stack: [frame, ..e.frame_stack], pending_label: None)
}

/// Iteration statement. Consumes pending_label into the frame's js_label.
pub fn push_loop(
  e: Emitter2,
  ir_break: String,
  ir_continue: String,
  carried: List(Int),
  iter_close: Option(#(String, Escape)),
) -> Emitter2 {
  push_frame(
    e,
    Loop2(
      ir_break:,
      ir_continue:,
      js_label: e.pending_label,
      carried:,
      iter_close:,
    ),
  )
}

/// switch: break target only. Consumes pending_label.
pub fn push_switch(
  e: Emitter2,
  ir_break: String,
  carried: List(Int),
) -> Emitter2 {
  push_frame(e, Switch2(ir_break:, js_label: e.pending_label, carried:))
}

/// Labeled non-loop block. Takes the label explicitly (Labeled2.js_label is a
/// required String) and clears pending_label.
pub fn push_labeled(
  e: Emitter2,
  ir_break: String,
  js_label: String,
  carried: List(Int),
) -> Emitter2 {
  push_frame(e, Labeled2(ir_break:, js_label:, carried:))
}

/// Barrier frame for try/catch/finally and for-of bodies — never a target,
/// only crossed. NOT routed through push_frame: barriers are transparent to
/// label flow, so pending_label MUST survive so an inner push_loop (inside
/// e.g. a for-of iterator barrier) still consumes the LabeledStatement's
/// label. Port of emit.gleam:2295-2305; invariant: M10-M11.md gotcha #6.
pub fn push_barrier(
  e: Emitter2,
  finally_body: Option(#(List(ast.StmtWithLine), ScopeSave2)),
  iter_close: Option(String),
  escape: Option(Escape),
) -> Emitter2 {
  Emitter2(..e, frame_stack: [
    Barrier2(finally_body:, iter_close:, escape:),
    ..e.frame_stack
  ])
}

/// A fresh landing label for a region about to be emitted, carrying `arity`
/// values past the flag and exception.
pub fn fresh_escape(e: Emitter2, arity: Int) -> #(Escape, Emitter2) {
  let #(label, e) = fresh_label(e)
  #(Escape(label:, arity:), e)
}

fn fresh_vars(e: Emitter2, n: Int) -> #(List(String), Emitter2) {
  let #(e, names) = {
    use #(e, acc), _ <- list.fold(list.repeat(Nil, n), #(e, []))
    let #(v, e) = fresh_var(e)
    #(e, [v, ..acc])
  }
  #(list.reverse(names), e)
}

/// The handler a cleanup's own ir.Try uses to leave `esc`'s region with the
/// thrown value.
pub fn escape_handler(e: Emitter2, esc: Escape) -> #(ir.CatchHandler, Emitter2) {
  let #(x, e) = fresh_var(e)
  let dummies = list.repeat(e.consts.undef, esc.arity)
  #(
    ir.CatchHandler(
      on: ir.OnTag(e.consts.js_tag),
      payload: [x],
      exnref: None,
      handler: ir.Break(esc.label, [ir.ConstI32(1), ir.Var(x), ..dummies]),
    ),
    e,
  )
}

/// Wrap `region` (an ir.Try yielding `esc.arity` carried terms) in its landing
/// block: a normal exit yields the carried values on through, an escape
/// rethrows the carried exception outside the region. Yields the carried
/// values.
pub fn land_escapes(e: Emitter2, esc: Escape, region: ir.Expr) -> #(ir.Expr, Emitter2) {
  let #(code, e) = fresh_var(e)
  let #(exn, e) = fresh_var(e)
  let #(inner, e) = fresh_vars(e, esc.arity)
  let #(outer, e) = fresh_vars(e, esc.arity)
  let tys = list.repeat(ir.TTerm, esc.arity)
  let block =
    ir.Block(
      esc.label,
      [ir.TI32, ir.TTerm, ..tys],
      ir.Let(
        inner,
        region,
        ir.Values([ir.ConstI32(0), e.consts.undef, ..list.map(inner, ir.Var)]),
      ),
    )
  let tree =
    ir.Let(
      [code, exn, ..outer],
      block,
      ir.Let(
        [],
        ir.If(
          ir.Var(code),
          [],
          ir.Throw(e.consts.js_tag, [ir.Var(exn)]),
          ir.Values([]),
        ),
        ir.Values(list.map(outer, ir.Var)),
      ),
    )
  #(tree, e)
}

/// Pop the innermost frame. Every pop pairs with a push_* in the same emit
/// function; empty stack here is a push/pop desync — crash at the desync.
pub fn pop_frame(e: Emitter2) -> Emitter2 {
  let assert [_, ..rest] = e.frame_stack
  Emitter2(..e, frame_stack: rest)
}

/// LabeledStatement sets this before emitting its body; the next
/// push_loop/push_switch/push_labeled consumes it.
pub fn set_pending_label(e: Emitter2, label: String) -> Emitter2 {
  Emitter2(..e, pending_label: Some(label))
}

// ── break/continue target resolution (new; arc's is imperative op-emitting) ──

/// The ir label a `break name` targets when `frame` IS the target, or None
/// when `frame` must instead be crossed. Port of emit.gleam:2415-2455
/// `frame_target` for `is_cont: False`.
fn break_target_of(frame: Frame2, name: Option(String)) -> Option(String) {
  case frame {
    Loop2(ir_break:, js_label:, ..) | Switch2(ir_break:, js_label:, ..) ->
      case name {
        None -> Some(ir_break)
        Some(_) ->
          case js_label == name {
            True -> Some(ir_break)
            False -> None
          }
      }
    Labeled2(ir_break:, js_label:, ..) ->
      // §14.8: unlabeled break skips a labeled non-loop block.
      case name {
        Some(n) if n == js_label -> Some(ir_break)
        _ -> None
      }
    Barrier2(..) -> None
  }
}

/// `frame_target` for `is_cont: True` — only Loop2 ever answers.
fn continue_target_of(frame: Frame2, name: Option(String)) -> Option(String) {
  case frame {
    Loop2(ir_continue:, js_label:, ..) ->
      case name {
        None -> Some(ir_continue)
        Some(_) ->
          case js_label == name {
            True -> Some(ir_continue)
            False -> None
          }
      }
    Switch2(..) | Labeled2(..) | Barrier2(..) -> None
  }
}

/// Cleanups a break/continue must perform when it *crosses* (does not target)
/// `frame`. Port of emit.gleam:2460-2474 `emit_cross_frame`, data-returning.
/// Innermost first — a Barrier2 with both iter_close and finally_body yields
/// IterClose then FinallyBlock (close the iterator before running finally).
fn cross_cleanups(frame: Frame2) -> List(BarrierCleanup) {
  case frame {
    Loop2(iter_close: Some(#(iv, esc)), ..) -> [IterClose(iv, False, Some(esc))]
    Loop2(..) | Switch2(..) | Labeled2(..) -> []
    Barrier2(finally_body:, iter_close:, escape:) -> {
      let acc = case finally_body {
        Some(#(body, save)) -> [FinallyBlock(body, save, escape)]
        None -> []
      }
      case iter_close {
        Some(iv) -> [IterClose(iv, False, escape), ..acc]
        None ->
          case acc {
            [] -> [CatchOnly]
            _ -> acc
          }
      }
    }
  }
}

fn find_target(
  frames: List(Frame2),
  name: Option(String),
  target_of: fn(Frame2, Option(String)) -> Option(String),
  miss: EmitError,
  crossed: List(BarrierCleanup),
) -> Result(#(String, List(BarrierCleanup)), EmitError) {
  case frames {
    [] -> Error(miss)
    [frame, ..rest] ->
      case target_of(frame, name) {
        Some(label) -> Ok(#(label, list.reverse(crossed)))
        None -> {
          let crossed =
            list.fold(cross_cleanups(frame), crossed, fn(acc, c) { [c, ..acc] })
          find_target(rest, name, target_of, miss, crossed)
        }
      }
  }
}

/// Resolve `break [name]` against the current frame stack. Returns the ir
/// label to Break to and every barrier cleanup crossed on the way out,
/// innermost first — the order M13/M17 must inline them before the Break.
/// Port of emit.gleam:2503-2530 `emit_goto_loop`, data-returning.
pub fn find_break_target(
  e: Emitter2,
  name: Option(String),
) -> Result(#(String, List(BarrierCleanup)), EmitError) {
  find_target(e.frame_stack, name, break_target_of, BreakOutsideLoop, [])
}

/// Resolve `continue [name]`. Only Loop2 frames are targets; Switch2/Labeled2
/// are crossed transparently, Barrier2 crossings collect their cleanup.
pub fn find_continue_target(
  e: Emitter2,
  name: Option(String),
) -> Result(#(String, List(BarrierCleanup)), EmitError) {
  find_target(e.frame_stack, name, continue_target_of, ContinueOutsideLoop, [])
}

// ── scope cursor + constructor (port emit.gleam:1295-1369) ──────────────────

/// Direct child scope ids of `id` that share THIS function's frame
/// (Block/Catch/With/ClassBody). Port of emit.gleam:1366-1369 verbatim —
/// function-kind children go through child_fn_cursor, not scope_cursor.
pub fn block_child_scopes(tree: ScopeTree, id: ScopeId) -> List(ScopeId) {
  use c <- list.filter(scope.child_scopes(tree, id))
  !scope.is_function_kind(scope.get_scope(tree, c).kind)
}

/// Fresh emitter positioned at the function-kind scope `root`. Port of
/// emit.gleam:1295-1329 with bytecode-specific fields dropped.
pub fn new_emitter(
  tree: ScopeTree,
  root: ScopeId,
  strict: Bool,
  module_name: String,
  dispatch: EmitDispatch,
) -> Emitter2 {
  Emitter2(
    tree:,
    fn_scope: root,
    cur_scope: root,
    scope_cursor: block_child_scopes(tree, root),
    child_fn_cursor: scope.child_function_scopes(tree, root),
    in_block: False,
    next_var: 0,
    next_label: 0,
    next_fn: 0,
    next_site: 0,
    module_name:,
    frame_stack: [],
    pending_label: None,
    fns_acc: [],
    unsupported: [],
    strict:,
    is_async: False,
    is_generator: False,
    is_arrow: False,
    with_stack: [],
    private_env: [],
    field_init: NoFieldInit,
    derived_ctor: False,
    default_ctor: False,
    this_tdz: False,
    slot_vars: dict.new(),
    initialized: set.new(),
    known_numbers: set.new(),
    hoisted_kfn: dict.new(),
    const_globals: dict.new(),
    slotted_globals: dict.new(),
    class_stack: [],
    sm_abrupt: None,
    raw_args_var: None,
    dispatch:,
    consts: realm_consts(),
  )
}

/// This function body's FunctionInfo — local_count, fallthrough, lexical-slot
/// layout for the function-kind scope this emitter is rooted at.
/// Port of emit.gleam:1350-1352.
pub fn fn_info(e: Emitter2) -> scope.FunctionInfo {
  scope.function_info(e.tree, e.fn_scope)
}

/// Whether this body's OWNED lexical slot for `ref` holds a cell: when an
/// inner arrow captures it (the analyzer's `lexical_boxed`), and always for
/// `this` in a derived constructor, whose `super()` may bind it inside any
/// nested control structure.
pub fn lexical_is_boxed(
  e: Emitter2,
  info: scope.FunctionInfo,
  ref: lexical.LexicalRef,
) -> Bool {
  lexical.lexical_refs_get(info.lexical_boxed, ref)
  || { ref == lexical.RefThis && e.derived_ctor }
}

/// Resolve `name` from the current scope. Delegates entirely to the analyzer
/// tree — the emitter never re-walks bindings itself. Port of emit.gleam:2044.
pub fn resolve(e: Emitter2, name: String) -> scope.Resolution {
  scope.lookup(e.tree, e.cur_scope, name)
}

/// True when `arguments` at cur_scope resolves to the fn-scope's implicit
/// (parser-inserted) VarBinding — NOT a user param/let/const/catch/with
/// shadow. Gates expr.emit_plain_call's `X.apply(Y, arguments)` → raw-`_args`
/// fast-path: forwarding `_args` is only sound when the identifier IS the
/// arguments object built from `_args`.
pub fn arguments_is_implicit(e: Emitter2) -> Bool {
  case dict.get(scope.get_scope(e.tree, e.fn_scope).bindings, "arguments") {
    Ok(scope.Binding(slot: fs, kind: scope.VarBinding, ..)) ->
      case resolve(e, "arguments") {
        scope.Plain(scope.Local(slot:, kind: scope.VarBinding, ..)) ->
          slot == fs
        _ -> False
      }
    _ -> False
  }
}

/// Pop the next source-order child function scope id. Consumed by M15
/// emit_function at each FunctionExpression/Declaration/Arrow site so the
/// compiled body sees its analyzer-assigned ScopeId. An exhausted cursor is a
/// walk-order desync — crash at the desync rather than miscompile against the
/// wrong scope. Port of emit.gleam:3260-3261.
pub fn pop_child_fn(e: Emitter2) -> #(ScopeId, Emitter2) {
  let assert [fn_id, ..rest] = e.child_fn_cursor
    as "emit_2core.pop_child_fn: cursor exhausted (analyzer/emit walk desync)"
  #(fn_id, Emitter2(..e, child_fn_cursor: rest))
}

/// Descend into the next source-order block-child scope. Empty-cursor case
/// stays put (emit.gleam:1419-1430) — do NOT re-read the current scope's
/// children (that would re-enter already-consumed siblings). No
/// binding-prologue emission here; M13 owns TDZ seeding via anf helpers.
pub fn enter_scope(
  e: Emitter2,
  in_block in_block: Bool,
) -> #(Emitter2, ScopeSave2) {
  case e.scope_cursor {
    [child_id, ..parent_rest] -> {
      let save =
        ScopeSave2(
          cur_scope: e.cur_scope,
          scope_cursor: parent_rest,
          slot_vars: e.slot_vars,
          in_block: e.in_block,
        )
      let e =
        Emitter2(
          ..e,
          cur_scope: child_id,
          scope_cursor: block_child_scopes(e.tree, child_id),
          in_block:,
        )
      #(e, save)
    }
    [] -> {
      let save =
        ScopeSave2(
          cur_scope: e.cur_scope,
          scope_cursor: [],
          slot_vars: e.slot_vars,
          in_block: e.in_block,
        )
      #(Emitter2(..e, in_block:), save)
    }
  }
}

/// Restore the parent scope position saved by enter_scope.
pub fn leave_scope(e: Emitter2, save: ScopeSave2) -> Emitter2 {
  Emitter2(
    ..e,
    cur_scope: save.cur_scope,
    scope_cursor: save.scope_cursor,
    slot_vars: save.slot_vars,
    in_block: save.in_block,
  )
}

/// Conditionally consume the for-head Block scope (only pushed for
/// let/const/using heads). None → true no-op on both sides so a var/expr
/// head does not rewind the cursor past the body's siblings.
pub fn enter_for_scope(
  e: Emitter2,
  has_lex_head: Bool,
) -> #(Emitter2, Option(ScopeSave2)) {
  case has_lex_head {
    True -> {
      let #(e, save) = enter_scope(e, in_block: e.in_block)
      #(e, Some(save))
    }
    False -> #(e, None)
  }
}

pub fn leave_for_scope(e: Emitter2, save: Option(ScopeSave2)) -> Emitter2 {
  case save {
    Some(s) -> leave_scope(e, s)
    None -> e
  }
}

/// Enter a nested function body. Saves every per-body field to FnSave and
/// resets the emitter for the child function scope. next_var/next_label/
/// next_fn/fns_acc are PRESERVED (module-monotone, invariant #1). frame_stack
/// is per-function (a break never crosses a function boundary). private_env/
/// class_stack inherit lexically (a nested closure inside a class body still
/// sees `#x` and `super`); with_stack/field_init/slot_vars/initialized reset.
pub fn enter_function(
  e: Emitter2,
  child_id: ScopeId,
  strict strict: Bool,
  is_async is_async: Bool,
  is_generator is_generator: Bool,
  is_arrow is_arrow: Bool,
) -> #(Emitter2, FnSave) {
  let save =
    FnSave(
      fn_scope: e.fn_scope,
      cur_scope: e.cur_scope,
      scope_cursor: e.scope_cursor,
      child_fn_cursor: e.child_fn_cursor,
      in_block: e.in_block,
      frame_stack: e.frame_stack,
      pending_label: e.pending_label,
      strict: e.strict,
      is_async: e.is_async,
      is_generator: e.is_generator,
      is_arrow: e.is_arrow,
      with_stack: e.with_stack,
      private_env: e.private_env,
      field_init: e.field_init,
      derived_ctor: e.derived_ctor,
      default_ctor: e.default_ctor,
      this_tdz: e.this_tdz,
      class_stack: e.class_stack,
      slot_vars: e.slot_vars,
      initialized: e.initialized,
      hoisted_kfn: e.hoisted_kfn,
      sm_abrupt: e.sm_abrupt,
      raw_args_var: e.raw_args_var,
    )
  let child =
    Emitter2(
      ..e,
      fn_scope: child_id,
      cur_scope: child_id,
      scope_cursor: block_child_scopes(e.tree, child_id),
      child_fn_cursor: scope.child_function_scopes(e.tree, child_id),
      in_block: False,
      frame_stack: [],
      pending_label: None,
      strict:,
      is_async:,
      is_generator:,
      is_arrow:,
      with_stack: [],
      private_env: e.private_env,
      field_init: NoFieldInit,
      derived_ctor: False,
      default_ctor: False,
      this_tdz: is_arrow && e.this_tdz,
      class_stack: e.class_stack,
      slot_vars: dict.new(),
      initialized: set.new(),
      hoisted_kfn: dict.new(),
      sm_abrupt: None,
      raw_args_var: None,
    )
  #(child, save)
}

/// Restore per-body fields from FnSave. Module-monotone fields (next_var/
/// next_label/next_fn/fns_acc/tree/dispatch/consts) carry forward from `e`.
pub fn leave_function(e: Emitter2, save: FnSave) -> Emitter2 {
  Emitter2(
    ..e,
    fn_scope: save.fn_scope,
    cur_scope: save.cur_scope,
    scope_cursor: save.scope_cursor,
    child_fn_cursor: save.child_fn_cursor,
    in_block: save.in_block,
    frame_stack: save.frame_stack,
    pending_label: save.pending_label,
    strict: save.strict,
    is_async: save.is_async,
    is_generator: save.is_generator,
    is_arrow: save.is_arrow,
    with_stack: save.with_stack,
    private_env: save.private_env,
    field_init: save.field_init,
    derived_ctor: save.derived_ctor,
    default_ctor: save.default_ctor,
    this_tdz: save.this_tdz,
    class_stack: save.class_stack,
    slot_vars: save.slot_vars,
    initialized: save.initialized,
    hoisted_kfn: save.hoisted_kfn,
    sm_abrupt: save.sm_abrupt,
    raw_args_var: save.raw_args_var,
  )
}

/// M18: install the SM abrupt-completion intercept before delegating a
/// split-free fragment; `clear_sm_abrupt` removes it after.
pub fn set_sm_abrupt(e: Emitter2, hooks: SmAbrupt) -> Emitter2 {
  Emitter2(..e, sm_abrupt: Some(hooks))
}

pub fn clear_sm_abrupt(e: Emitter2) -> Emitter2 {
  Emitter2(..e, sm_abrupt: None)
}
