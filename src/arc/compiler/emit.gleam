/// Phase 1: AST Emission
///
/// Walks the AST and produces a list of EmitterOps — symbolic IR instructions
/// mixed with scope markers. Variable references use string names (IrScopeGetVar),
/// jump targets use integer label IDs (IrJump). These are resolved in Phase 2 and 3.
import arc/compiler/using_desugar
import arc/parser/ast
import arc/vm/opcode.{
  type IrOp, IrArrayFrom, IrArrayFromWithHoles, IrArrayPush, IrArrayPushHole,
  IrArraySpread, IrAsyncYieldStarNext, IrAsyncYieldStarResume, IrAwait, IrBinOp,
  IrCallApply, IrCallConstructor, IrCallConstructorApply, IrCallMethod,
  IrCallMethodApply, IrCreateArguments, IrCreateRestArray, IrDeclareGlobalLex,
  IrDeclareGlobalVar, IrDefineAccessor, IrDefineAccessorComputed, IrDefineField,
  IrDefineFieldComputed, IrDefineMethod, IrDefineMethodComputed,
  IrDefinePrivateAccessor, IrDefinePrivateField, IrDefinePrivateMethod,
  IrDeleteElem, IrDeleteField, IrDup, IrForInNext, IrForInStart,
  IrGetAsyncIterator, IrGetElem, IrGetElem2, IrGetField, IrGetField2,
  IrGetIterator, IrGetLexical, IrGetPrivateFieldDyn, IrGetPrivateFieldDyn2,
  IrGetPrototypeOf, IrGetSuperValue, IrGetSuperValue2, IrGosub, IrInitGlobalLex,
  IrInitialYield, IrIteratorCheckObject, IrIteratorClose, IrIteratorCloseThrow,
  IrIteratorNext, IrIteratorRecord, IrIteratorRest, IrJump, IrJumpIfFalse,
  IrJumpIfNullish, IrJumpIfTrue, IrLabel, IrMakeClosure, IrMakeMethod,
  IrNewObject, IrNewPrivateName, IrNewRegExp, IrObjectRestCopy, IrObjectSpread,
  IrPop, IrPopTry, IrPrivateInDyn, IrPushConst, IrPushTry, IrPutElem, IrPutField,
  IrPutPrivateFieldDyn, IrPutSuperValue, IrRet, IrReturn, IrScopeGetVar,
  IrScopeGetVarThis, IrScopeInitVar, IrScopePutVar, IrScopeReboxVar,
  IrScopeTypeofVar, IrSetLine, IrSetProto, IrSetThis, IrSetupDerivedClass,
  IrSwap, IrThrow, IrThrowError, IrTypeOf, IrUnaryOp, IrYield, IrYieldStar,
}
import arc/vm/value.{
  type JsValue, Finite, JsBool, JsNull, JsNumber, JsString, JsUndefined,
}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}

/// Globally unique id for a tagged-template call site (GetTemplateObject
/// cache key). Baked into bytecode at compile time so re-executing the same
/// compiled site reuses its template object while each fresh compilation
/// (repeated eval / new Function) gets distinct sites.
@external(erlang, "arc_vm_ffi", "unique_positive_integer")
fn unique_positive_integer() -> Int

// ============================================================================
// Types
// ============================================================================

/// An instruction in the emitter output — either a real IR op or a scope marker.
pub type EmitterOp {
  /// A real IR instruction (passed to Phase 2 → Phase 3)
  Ir(IrOp)
  /// Open a new scope
  EnterScope(kind: ScopeKind)
  /// Close the current scope
  LeaveScope
  /// Declare a variable in the current scope
  DeclareVar(name: String, kind: BindingKind)
  /// Declare an owned slot for a lexical pseudo-binding (this / active_func /
  /// new.target). Non-arrows emit these in `all_lexical_refs` order as the
  /// first declarations after EnterScope(FunctionScope) so slot indices start
  /// at len(captures). Script/module/eval bodies emit RefThis only. Arrows
  /// skip — `IrGetLexical` resolves to a capture.
  DeclareLexical(ref: opcode.LexicalRef)
  /// Annex B §B.3.2.3/.3.6 web-compat: hoist a sloppy-mode function-in-block
  /// name as a var binding (undefined) at program/eval top level. Resolves to
  /// IrDeclareGlobalVar (ToGlobal) or IrDeclareEvalVar (ToEvalEnv); dropped by
  /// compile_eval_direct when the eval turns out strict (strict caller).
  DeclareAnnexBVar(name: String)
  /// Annex B §B.3.2.6 web-compat: when a FunctionDeclaration in a block is
  /// evaluated in sloppy mode, copy the block-scoped function binding into
  /// the enclosing VariableEnvironment binding of the same name (skipping
  /// intermediate block scopes). Resolved by scope.gleam, which also skips
  /// the copy entirely if an intermediate lexical binding shadows the name.
  AnnexBPromote(name: String)
  /// §14.11 `with`: mark the start of an object-environment scope. `name` is
  /// the synthetic local holding the (ToObject'd) with object — declared in
  /// the enclosing block scope just before this marker. Phase 2 routes any
  /// name resolution that crosses this marker through IrWithGetVar /
  /// IrWithPutVar / IrWithDeleteVar checks against that slot.
  EnterWith(name: String)
  /// Close the innermost EnterWith marker.
  LeaveWith
}

pub type ScopeKind {
  FunctionScope
  BlockScope
}

pub type BindingKind {
  VarBinding
  LetBinding
  ConstBinding
  ParamBinding
  CatchBinding
  CaptureBinding
  /// §13.2.5.5 InstantiateOrdinaryFunctionExpression: the self-name binding
  /// of a NAMED function expression — an immutable binding holding the
  /// closure itself. Unlike ConstBinding, assignment only throws in strict
  /// mode code; sloppy-mode writes are silently dropped (§9.1.1.1.5
  /// SetMutableBinding on an immutable binding with S = false).
  FnNameBinding
}

/// Where top-level let/const/class declarations land. Decided once per
/// compilation unit; nested block scopes are always local regardless.
pub type TopLevelLex {
  /// Global lexical record (state.lexical_globals). Used by the REPL so
  /// `let x = 1` persists across inputs.
  LexGlobal
  /// Local slot in this template. Used by scripts, modules, function bodies,
  /// and eval — §19.2.1.1 PerformEval steps 27–28 always create a fresh
  /// LexicalEnvironment, so eval'd let/const/class never escape.
  LexLocal
}

/// A compiled child function (before Phase 2/3).
pub type CompiledChild {
  CompiledChild(
    name: Option(String),
    arity: Int,
    /// §15.1.5 ExpectedArgumentCount — value for the `length` property:
    /// formal params before the first one with a default initializer
    /// (rest param excluded). `arity` still counts all fixed params.
    length: Int,
    code: List(EmitterOp),
    constants: List(JsValue),
    constants_map: Dict(JsValue, Int),
    functions: List(CompiledChild),
    is_strict: Bool,
    is_arrow: Bool,
    is_derived_constructor: Bool,
    is_generator: Bool,
    is_async: Bool,
    /// Stored [[Construct]] capability: True for normal functions and class
    /// constructors; False for arrows, generators, async, and methods /
    /// getters / setters. (cf. QuickJS `is_constructor` / JSC `ConstructAbility`.)
    is_constructor: Bool,
    /// §10.2.1 [[IsClassConstructor]]: true only for class constructors
    /// (base and derived) — plain calls must throw TypeError.
    is_class_constructor: Bool,
    /// True if this function contains a syntactic `eval(...)` call with
    /// identifier callee. Such functions get all locals boxed and a
    /// name→index table stored on FuncTemplate so direct eval can see them.
    has_eval_call: Bool,
    /// Per-binding "is referenced in this body or a nested arrow" flags.
    /// For arrows, the parent must capture/box the corresponding slot; for
    /// non-arrows this is informational (they own their slots).
    lexical_refs: opcode.LexicalRefs,
    /// Syntax-legality flags inherited by direct eval.
    syntax_perms: opcode.SyntaxPerms,
    /// Synthetic with-object local names (innermost first) active in the
    /// ENCLOSING scope chain at this closure's creation site. The child's
    /// free-name resolution must check these objects (captured from the
    /// parent) before falling through to outer bindings / globals.
    with_stack: List(String),
  )
}

/// Break/continue/return target frame. Also pushed for try-bodies (with
/// break/continue = -1) so cross-boundary jumps emit the right PopTry count.
/// Mirrors QuickJS BlockEnv (quickjs.c:21320).
pub type LoopContext {
  LoopContext(
    /// -1 if not a break target (try-body barrier frames).
    break_label: Int,
    /// -1 if not a continue target (switch, labeled-block, try-body).
    continue_label: Int,
    label: Option(String),
    /// True for labeled non-loop blocks. Unlabeled `break` skips these
    /// (§14.8: targets nearest IterationStatement or SwitchStatement only).
    is_regular: Bool,
    /// IrPopTry ops to emit when break/continue/return *crosses* (does not
    /// target) this frame. for-of: 1 (F_body). try-body: 1 or 2.
    cross_pop_try: Int,
    /// After cross_pop_try, emit IrIteratorClose (or IrSwap+IrIteratorClose
    /// when a return value sits above iter). Exactly one stack slot (iter).
    has_iterator: Bool,
    /// If Some(L): when break/continue/return *crosses* this frame, after
    /// dropping try frames, emit `push undef; Gosub(L); Pop` so the finally
    /// subroutine runs before the jump proceeds. Mirrors QuickJS
    /// BlockEnv.label_finally (quickjs.c:21326).
    label_finally: Option(Int),
    /// Raw IrPop count when *crossed*. Non-zero only on the finally-body
    /// barrier, where it discards [slot, gosub_retpc] so a break/continue/
    /// return inside finally never reaches IrRet — spec-correct completion
    /// replacement (§14.15.3). Mirrors QuickJS BlockEnv.drop_count
    /// (quickjs.c:21325).
    drop_count: Int,
  )
}

/// The emitter state, threaded through all emit functions.
pub opaque type Emitter {
  Emitter(
    code: List(EmitterOp),
    constants_map: Dict(JsValue, Int),
    constants_list: List(JsValue),
    next_const: Int,
    next_label: Int,
    loop_stack: List(LoopContext),
    functions: List(CompiledChild),
    next_func: Int,
    /// Set by LabeledStatement before emitting a loop body.
    /// Consumed by push_loop to attach the label to the LoopContext.
    pending_label: Option(String),
    /// True if the current compilation unit is strict. Inherited by child
    /// functions; can be upgraded (never downgraded) by a "use strict"
    /// directive in the function body prologue. Classes force strict.
    strict: Bool,
    /// Set to True when a syntactic `eval(...)` (identifier callee) is
    /// encountered. Propagated to CompiledChild so the compiler knows to
    /// box all locals in this function for direct eval access.
    has_eval_call: Bool,
    /// True while emitting an async function body. Checked by yield* to
    /// route to the async-delegation path (GetAsyncIterator + await).
    is_async: Bool,
    /// True while emitting an arrow function body. Gates whether
    /// `DeclareLexical` and the `arguments` local are emitted in the prologue.
    is_arrow: Bool,
    /// Per-binding "has IrGetLexical(ref) been emitted in this body, or
    /// propagated up from an inner ARROW child via add_child_function".
    /// Mirrors JSC's InnerArrowFunctionCodeFeatures — lets the compiler
    /// decide capture/box without re-walking the IR.
    lexical_refs: opcode.LexicalRefs,
    /// Syntax-legality flags for the body being emitted. Arrows inherit the
    /// parent's verbatim; non-arrows compute from function kind.
    syntax_perms: opcode.SyntaxPerms,
    /// Where top-level let/const/class go. LexGlobal only for the REPL
    /// program emitter; everything else (scripts, eval, modules, function
    /// bodies) uses LexLocal.
    top_lex: TopLevelLex,
    /// Tracks EnterScope/LeaveScope nesting so emit_stmt can tell when a
    /// let/const/class is at the program top level (depth 1) vs inside a
    /// nested block. Only meaningful when top_lex == LexGlobal.
    scope_depth: Int,
    /// Whether/where to emit the field-initializer call (§13.3.7.1 step 12
    /// InitializeInstanceElements). Set by compile_class_body
    /// when the class has instance fields; arrows inherit it so `()=>super()`
    /// can find it.
    field_init: FieldInitMode,
    /// True while emitting statements that sit directly in a Block (or switch
    /// CaseBlock) — i.e. positions where a FunctionDeclaration is block-scoped
    /// and a candidate for Annex B var promotion. False at function/program
    /// top level (those declarations are var-scoped already).
    in_block: Bool,
    /// Lexical names declared by enclosing scope levels within the current
    /// var-scope body (top-level let/const/class, enclosing blocks' lexical
    /// names incl. their function declarations, destructured catch params,
    /// for-head let/const). A block function declaration whose name is in
    /// this set must NOT get the Annex B var promotion (§B.3.2: replacing it
    /// with a VariableStatement would produce an early error).
    annexb_blocked: Set(String),
    /// Function declaration names of the CURRENT block level. They are
    /// lexical names for any NESTED scope (folded into annexb_blocked when
    /// descending) but must not block their own promotion at this level.
    annexb_level_fns: Set(String),
    /// Non-empty only while emitting formal-parameter initializers (default
    /// values / destructuring defaults): the parameter-scope binding names
    /// (parameters + implicit `arguments` for non-arrows). Attached to any
    /// IrCallEval emitted in that region so EvalDeclarationInstantiation
    /// (§19.2.1.1 step 3.d) can throw a SyntaxError when a sloppy direct
    /// eval var-declares a name already bound in the parameter scope.
    /// Reset to [] before the function body is emitted; nested functions
    /// and arrows start from a fresh emitter so it never leaks into them.
    param_scope_names: List(String),
    /// Synthetic with-object local names (innermost first) for the `with`
    /// statements lexically enclosing the current emission point — including
    /// withs inherited from enclosing functions (child emitters copy the
    /// parent's stack). Recorded on CompiledChild at closure creation so
    /// nested functions route free names through the with objects.
    with_stack: List(String),
    /// Private names ("#x") visible at the current emission point — the
    /// running [[PrivateEnvironment]] chain. compile_class prepends its
    /// class's declared private names for the duration of the class body;
    /// child function emitters inherit the parent's list. Stamped onto
    /// IrCallEval so direct eval parses with the caller's private
    /// environment (§19.2.1.1 PerformEval step 5).
    private_env: List(String),
    /// §14 completion values: Some(name) while emitting a loop / labeled /
    /// switch statement in tail position (the eval/program completion value).
    /// `name` is a synthetic block-scoped binding that tracks the spec's V:
    /// expression statements store into it instead of popping; loops, if and
    /// switch reset it to undefined on entry (UpdateEmpty with undefined).
    /// Never inherited by child function emitters (fresh new_emitter()).
    completion_var: Option(String),
  )
}

/// Where the synthetic field-initializer call is emitted.
pub type FieldInitMode {
  /// No instance fields (or not in a constructor body).
  NoFieldInit
  /// Base-class ctor: call init fn at start of body, after lexical declares.
  FieldInitAtStart
  /// Derived-class ctor: call init fn after every `super()`.
  FieldInitAfterSuper
}

/// QuickJS JS_ATOM_class_fields_init. Declared as a const in the per-class
/// block scope (P6) and captured via the ordinary closure path — `<...>` is
/// outside §12.7 IdentifierName grammar so it can't collide with user code.
/// P0 per-MakeClosure scope snapshots give each class its own slot, so two
/// classes in one parent function don't share a box.
const class_fields_init = "<class_fields_init>"

/// Compile error from the emitter.
pub type EmitError {
  BreakOutsideLoop
  ContinueOutsideLoop
  Unsupported(description: String)
}

// ============================================================================
// Public API
// ============================================================================

/// Emit IR for a list of top-level statements (script body).
/// Returns the emitter ops, constants, child functions, and script strictness.
pub fn emit_program(
  stmts: List(ast.StmtWithLine),
  top_lex: TopLevelLex,
) -> Result(
  #(
    List(EmitterOp),
    List(JsValue),
    Dict(JsValue, Int),
    List(CompiledChild),
    Bool,
  ),
  EmitError,
) {
  emit_program_common(stmts, False, top_lex)
}

/// Emit IR for a module body. Always strict mode.
/// Accepts raw module items and handles export default internally
/// by declaring a `*default*` local binding (per ES spec §16.2.1.6.2).
pub fn emit_module(
  items: List(ast.ModuleItem),
) -> Result(
  #(
    List(EmitterOp),
    List(JsValue),
    Dict(JsValue, Int),
    List(CompiledChild),
    Bool,
    List(#(String, Int)),
  ),
  EmitError,
) {
  // Only ANONYMOUS defaults need the synthetic *default* binding —
  // `export default function fn() {}` / `class fn {}` declare `fn` itself
  // (§16.2.3.7 BoundNames) and are lowered to ordinary declarations below.
  let has_default_export =
    list.any(items, fn(item) {
      case item {
        ast.ExportDefaultDeclaration(
          declaration: ast.FunctionExpression(name: Some(_), ..),
          ..,
        ) -> False
        ast.ExportDefaultDeclaration(
          declaration: ast.ClassExpression(name: Some(_), ..),
          ..,
        ) -> False
        ast.ExportDefaultDeclaration(..) -> True
        _ -> False
      }
    })
  let stmts = module_items_to_stmts(items)
  emit_module_common(stmts, has_default_export)
}

/// Module emission: sets up scope, hoists, handles *default* binding, emits body.
/// The final tuple element is the hoisted top-level function declarations as
/// (name, func_index) pairs — used by the linker to instantiate exported
/// functions before any module body runs (cyclic function hoisting).
fn emit_module_common(
  stmts: List(ast.StmtWithLine),
  has_default_export: Bool,
) -> Result(
  #(
    List(EmitterOp),
    List(JsValue),
    Dict(JsValue, Int),
    List(CompiledChild),
    Bool,
    List(#(String, Int)),
  ),
  EmitError,
) {
  // Module-top-level using declarations: lowered into prelude/body/dispose
  // pieces. Exported bindings must stay module-scoped, so no AST try block —
  // emit_module_using_top installs a scope-free try frame instead so the
  // dispose sequence also runs on abrupt module-body completion (spec:
  // Module Evaluation calls DisposeResources with the completion, normal OR
  // throw). The flattened pieces feed the hoisting scans below — they
  // contain every declaration of the original body exactly once.
  let module_top = using_desugar.desugar_module_top(stmts)
  let stmts = case module_top {
    None -> stmts
    Some(top) -> list.flatten([top.prelude, top.body, top.dispose])
  }
  let e =
    Emitter(
      ..new_emitter(),
      strict: True,
      annexb_blocked: top_lex_name_set(stmts),
    )
  let e = emit_op(e, EnterScope(FunctionScope))
  // Module top-level `this === undefined` (§16.2.1.6.4). DeclareLexical
  // allocates slot 0; runtime padding leaves it JsUndefined.
  let e = emit_op(e, DeclareLexical(opcode.RefThis))

  // Hoist var declarations (top-level function names are var-scoped too)
  let hoisted_vars =
    list.append(collect_hoisted_vars(stmts), direct_fn_names(stmts))
    |> list.unique
  let e =
    list.fold(hoisted_vars, e, fn(e, name) {
      emit_op(e, DeclareVar(name, VarBinding))
    })

  // Declare *default* binding if module has a default export. Per §16.2.1.6.2
  // step 24.b.i this is CreateMutableBinding (not immutable). VarBinding so
  // the synthetic `*default* = expr` assignment below is a plain store: it
  // passes the IrScopePutVar const-reassign check AND skips the let-store
  // TDZ check (the binding is internal — user code can never reference
  // `*default*` in its TDZ window; cyclic-import READS of the seeded export
  // cell still TDZ-check in GetBoxed).
  let e = case has_default_export {
    True -> emit_op(e, DeclareVar("*default*", VarBinding))
    False -> e
  }

  // Hoist top-level let/const/class slots before hoisted-func MakeClosure so
  // closures capture the boxed slot, not a stale pre-box value. Module-scoped
  // lexical bindings are locals (not the global lexical record), so — like
  // LexLocal scripts in emit_program_common — they must be declared (and thus
  // boxed if captured) ahead of the hoisted functions that close over them.
  let e =
    list.fold(collect_top_lex_names(stmts), e, fn(e, lex) {
      let #(name, kind) = lex
      emit_op(e, DeclareVar(name, kind))
    })

  // Collect and emit hoisted function declarations
  let #(e, hoisted_funcs) = collect_hoisted_funcs(e, stmts)
  let e =
    list.fold(hoisted_funcs, e, fn(e, hf) {
      let #(name, func_idx) = hf
      let e = emit_ir(e, IrMakeClosure(func_idx))
      let e = emit_ir(e, IrScopePutVar(name))
      e
    })

  use e <- result.try(case module_top {
    None -> emit_stmts_tail(e, stmts)
    Some(top) -> emit_module_using_top(e, top)
  })

  let e = emit_op(e, LeaveScope)
  let #(code, constants, constants_map, children) = finish(e)
  Ok(#(code, constants, constants_map, children, True, hoisted_funcs))
}

/// Module body containing top-level using declarations: emit the desugared
/// pieces around a scope-free try frame. The body statements run at module
/// scope (so exported const bindings stay module-scoped), while an abrupt
/// completion is caught, folded into the completion state, and the
/// DisposeResources sequence runs on both paths — its trailing
/// `if (hasErr) throw err` rethrows the (possibly suppressed-error-folded)
/// completion. Layout mirrors the try/catch (no finally) lowering in
/// emit_stmt, minus the block scope around the protected statements.
fn emit_module_using_top(
  e: Emitter,
  top: using_desugar.ModuleTop,
) -> Result(Emitter, EmitError) {
  let using_desugar.ModuleTop(
    prelude:,
    body:,
    catch_param:,
    catch_body:,
    dispose:,
  ) = top
  use e <- result.try(emit_stmts(e, prelude))

  let #(e, catch_label) = fresh_label(e)
  let #(e, dispose_label) = fresh_label(e)

  let e = emit_ir(e, IrPushTry(catch_label))
  let e = push_barrier(e, pop_try: 1, label_finally: None, drop: 0)
  use e <- result.try(emit_stmts(e, body))
  let e = pop_loop(e)
  let e = emit_ir(e, IrPopTry)
  let e = emit_ir(e, IrJump(dispose_label))

  // Handler: unwind_to_catch leaves stack = [thrown, ..base]. Bind the
  // thrown value in a throwaway block scope and fold it into the
  // module-scoped completion state declared by the prelude.
  let e = emit_ir(e, IrLabel(catch_label))
  let e = emit_op(e, EnterScope(BlockScope))
  use e <- result.try(emit_destructuring_bind(
    e,
    ast.IdentifierPattern(catch_param),
    CatchBinding,
  ))
  use e <- result.try(emit_stmts(e, catch_body))
  let e = emit_op(e, LeaveScope)

  let e = emit_ir(e, IrLabel(dispose_label))
  emit_stmts_tail(e, dispose)
}

/// Convert module items to statements, stripping import/export wrappers.
/// ExportDefaultDeclaration becomes an assignment to *default* (the binding
/// is declared separately during module emission).
fn module_items_to_stmts(
  items: List(ast.ModuleItem),
) -> List(ast.StmtWithLine) {
  list.filter_map(items, fn(item) {
    case item {
      ast.StatementItem(located) -> Ok(located)
      ast.ExportNamedDeclaration(declaration: option.Some(decl), ..) ->
        Ok(ast.StmtWithLine(0, decl))
      // §16.2.3.7: a NAMED default function/class declares its own binding
      // (BoundNames = the name) — lower to the ordinary declaration so the
      // function is hoisted and the name is a mutable module-level binding
      // shared with the `default` export cell.
      ast.ExportDefaultDeclaration(
        declaration: ast.FunctionExpression(
          name: option.Some(_) as name,
          params:,
          body:,
          is_generator:,
          is_async:,
        ),
        ..,
      ) ->
        Ok(ast.StmtWithLine(
          0,
          ast.FunctionDeclaration(
            name:,
            params:,
            body:,
            is_generator:,
            is_async:,
          ),
        ))
      ast.ExportDefaultDeclaration(
        declaration: ast.ClassExpression(
          name: option.Some(_) as name,
          super_class:,
          body:,
        ),
        ..,
      ) ->
        Ok(ast.StmtWithLine(0, ast.ClassDeclaration(name:, super_class:, body:)))
      ast.ExportDefaultDeclaration(declaration: expr, ..) ->
        // Emit as: *default* = expr;
        // The *default* local is declared during module hoisting.
        Ok(ast.StmtWithLine(
          0,
          ast.ExpressionStatement(
            expression: ast.AssignmentExpression(
              operator: ast.Assign,
              left: ast.Identifier("*default*"),
              right: expr,
            ),
            directive: None,
          ),
        ))
      ast.ImportDeclaration(..) -> Error(Nil)
      ast.ExportNamedDeclaration(declaration: None, ..) -> Error(Nil)
      ast.ExportAllDeclaration(..) -> Error(Nil)
    }
  })
}

/// Common program emission: sets up scope, hoists, emits body, tears down.
/// When hoist_vars is True, collects and emits DeclareVar for var declarations.
/// When force_strict is True, the program is always strict (modules).
fn emit_program_common(
  stmts: List(ast.StmtWithLine),
  force_strict: Bool,
  top_lex: TopLevelLex,
) -> Result(
  #(
    List(EmitterOp),
    List(JsValue),
    Dict(JsValue, Int),
    List(CompiledChild),
    Bool,
  ),
  EmitError,
) {
  // Detect top-level strict directive so child functions inherit.
  // Modules are always strict regardless of directives.
  let script_strict = force_strict || has_use_strict_directive(stmts)
  let e =
    Emitter(
      ..new_emitter(),
      strict: script_strict,
      top_lex:,
      annexb_blocked: top_lex_name_set(stmts),
    )

  // Wrap in function scope
  let e = emit_op(e, EnterScope(FunctionScope))
  // Script/eval top-level owns a `this` slot (slot 0). Runtime entry writes
  // globalThis (or the caller's `this` for direct eval) into it before pc=0.
  let e = emit_op(e, DeclareLexical(opcode.RefThis))

  // Hoisting pre-pass: emit DeclareGlobalVar for top-level var declarations.
  // Both script and REPL modes create globalThis properties for hoisted vars.
  let hoisted_vars = collect_hoisted_vars(stmts)
  let e =
    list.fold(hoisted_vars, e, fn(e, name) {
      emit_ir(e, IrDeclareGlobalVar(name))
    })

  // Top-level function declaration names are var-scoped too.
  let e =
    list.fold(direct_fn_names(stmts), e, fn(e, name) {
      emit_ir(e, IrDeclareGlobalVar(name))
    })

  // Annex B §B.3.2.2/.2.6: sloppy-mode function-in-block names get a var
  // binding (undefined) before the body runs, unless blocked by a lexical
  // declaration. DeclareAnnexBVar (not a plain declare) so compile_eval_direct
  // can drop these when a direct eval turns out strict via its caller.
  let e = case script_strict {
    True -> e
    False ->
      // e.annexb_blocked is still the top_lex_name_set(stmts) computed at
      // emitter construction — reuse it instead of rescanning all stmts.
      list.fold(
        collect_annexb_candidates_with(stmts, e.annexb_blocked),
        e,
        fn(e, name) { emit_op(e, DeclareAnnexBVar(name)) },
      )
  }

  // Hoist top-level let/const/class slots before hoisted-func MakeClosure so
  // closures capture the boxed slot, not a stale pre-box value. LexGlobal
  // skips this — those names live in the global lexical record and child
  // closures reach them via GetGlobal at runtime, not via boxed captures.
  let e = case top_lex {
    LexLocal ->
      list.fold(collect_top_lex_names(stmts), e, fn(e, lex) {
        let #(name, kind) = lex
        emit_op(e, DeclareVar(name, kind))
      })
    // §16.1.7 GlobalDeclarationInstantiation: lexical declarations are
    // instantiated (uninitialized — TDZ) before the script body runs, so a
    // closure called before the `let` statement executes sees the TDZ
    // binding (`(function() { x = 1; })(); let x;` → ReferenceError) instead
    // of falling through to a global-object property write.
    LexGlobal ->
      list.fold(collect_top_lex_names(stmts), e, fn(e, lex) {
        let #(name, kind) = lex
        emit_ir(e, IrDeclareGlobalLex(name, kind == ConstBinding))
      })
  }

  // Collect and emit hoisted function declarations
  let #(e, hoisted_funcs) = collect_hoisted_funcs(e, stmts)
  let e =
    list.fold(hoisted_funcs, e, fn(e, hf) {
      let #(name, func_idx) = hf
      let e = emit_ir(e, IrMakeClosure(func_idx))
      let e = emit_ir(e, IrScopePutVar(name))
      e
    })

  // Emit body — last statement in tail position keeps its value on stack
  use e <- result.try(emit_stmts_tail(e, stmts))

  let e = emit_op(e, LeaveScope)
  let #(code, constants, constants_map, children) = finish(e)
  Ok(#(code, constants, constants_map, children, script_strict))
}

// ============================================================================
// Emitter helpers
// ============================================================================

fn new_emitter() -> Emitter {
  Emitter(
    code: [],
    constants_map: dict.new(),
    constants_list: [],
    next_const: 0,
    next_label: 0,
    loop_stack: [],
    functions: [],
    next_func: 0,
    pending_label: None,
    strict: False,
    has_eval_call: False,
    is_async: False,
    is_arrow: False,
    lexical_refs: opcode.no_lexical_refs,
    syntax_perms: opcode.script_perms,
    top_lex: LexLocal,
    scope_depth: 0,
    field_init: NoFieldInit,
    in_block: False,
    annexb_blocked: set.new(),
    annexb_level_fns: set.new(),
    param_scope_names: [],
    with_stack: [],
    private_env: [],
    completion_var: None,
  )
}

/// Check if a statement list begins with a Use Strict Directive.
/// ES2024 section 11.2.1 "Directive Prologues and the Use Strict Directive":
/// the directive prologue is the leading run of ExpressionStatements whose
/// expression is a string literal. "use strict" anywhere in that run makes
/// the function strict. We stop at the first non-string ExpressionStatement.
///
/// Detection compares the *raw* directive text (the `directive` field), not the
/// decoded expression value: a literal containing an escape or line
/// continuation (e.g. `'use strict'`) is not a Use Strict Directive even
/// though its decoded value is "use strict".
fn has_use_strict_directive(stmts: List(ast.StmtWithLine)) -> Bool {
  case stmts {
    [
      ast.StmtWithLine(
        statement: ast.ExpressionStatement(
          expression: ast.StringExpression(_),
          directive: directive,
        ),
        ..,
      ),
      ..rest
    ] ->
      case directive {
        option.Some("use strict") -> True
        _ -> has_use_strict_directive(rest)
      }
    _ -> False
  }
}

fn emit_op(e: Emitter, op: EmitterOp) -> Emitter {
  let scope_depth = case op {
    EnterScope(_) -> e.scope_depth + 1
    LeaveScope -> e.scope_depth - 1
    _ -> e.scope_depth
  }
  Emitter(..e, code: [op, ..e.code], scope_depth:)
}

/// True when the current statement is at the program top level (not inside
/// any block) and this compilation unit targets the global lexical record.
fn at_global_lex(e: Emitter) -> Bool {
  e.top_lex == LexGlobal && e.scope_depth == 1
}

/// Declare a let/const binding, routing to either a local slot or the
/// global lexical record depending on top_lex + scope depth.
fn declare_lex(e: Emitter, name: String, is_const: Bool) -> Emitter {
  case at_global_lex(e) {
    True -> emit_ir(e, IrDeclareGlobalLex(name, is_const))
    False -> {
      let kind = case is_const {
        True -> ConstBinding
        False -> LetBinding
      }
      emit_op(e, DeclareVar(name, kind))
    }
  }
}

/// Store the value on top of stack into a let/const binding declared via
/// declare_lex. Routes to IrInitGlobalLex (bypasses TDZ/const checks) or
/// IrScopeInitVar (resolves to PutLocal/PutBoxed, bypassing the const-reassign
/// check that IrScopePutVar applies). Mirrors QuickJS OP_scope_put_var_init.
fn init_lex(e: Emitter, name: String) -> Emitter {
  case at_global_lex(e) {
    True -> emit_ir(e, IrInitGlobalLex(name))
    False -> emit_ir(e, IrScopeInitVar(name))
  }
}

fn emit_ir(e: Emitter, op: IrOp) -> Emitter {
  emit_op(e, Ir(op))
}

fn add_constant(e: Emitter, val: JsValue) -> #(Emitter, Int) {
  case dict.get(e.constants_map, val) {
    Ok(idx) -> #(e, idx)
    Error(Nil) -> {
      let idx = e.next_const
      let e =
        Emitter(
          ..e,
          constants_map: dict.insert(e.constants_map, val, idx),
          constants_list: [val, ..e.constants_list],
          next_const: idx + 1,
        )
      #(e, idx)
    }
  }
}

fn push_const(e: Emitter, val: JsValue) -> Emitter {
  let #(e, idx) = add_constant(e, val)
  emit_ir(e, IrPushConst(idx))
}

/// Private names lex as Identifier tokens with the "#" prefix included
/// (lexer.gleam). Route them through the brand-checked private opcodes;
/// everything else uses ordinary [[Get]]/[[Set]].
///
/// Private access reads the per-class-evaluation PrivateName (a unique key
/// string minted by NewPrivateName at class-definition time) from the
/// class-scope const named after the source text ("#m") — the spec
/// PrivateEnvironment chain mapped onto ordinary lexical scoping — then uses
/// the Dyn opcode that takes the key from the stack.
///
/// Stack: [obj, ..] → [val, ..]
fn emit_get_field(e: Emitter, name: String) -> Emitter {
  case name {
    "#" <> _ ->
      e
      |> emit_ir(IrScopeGetVar(name))
      |> emit_ir(IrGetPrivateFieldDyn)
    _ -> emit_ir(e, IrGetField(name))
  }
}

/// Stack: [obj, ..] → [val, obj, ..]
fn emit_get_field2(e: Emitter, name: String) -> Emitter {
  case name {
    "#" <> _ ->
      e
      |> emit_ir(IrScopeGetVar(name))
      |> emit_ir(IrGetPrivateFieldDyn2)
    _ -> emit_ir(e, IrGetField2(name))
  }
}

/// Stack: [val, obj, ..] → [val, ..]
fn emit_put_field(e: Emitter, name: String) -> Emitter {
  case name {
    "#" <> _ ->
      e
      |> emit_ir(IrScopeGetVar(name))
      |> emit_ir(IrPutPrivateFieldDyn)
    _ -> emit_ir(e, IrPutField(name))
  }
}

fn fresh_label(e: Emitter) -> #(Emitter, Int) {
  let label = e.next_label
  #(Emitter(..e, next_label: label + 1), label)
}

fn push_loop(e: Emitter, break_label: Int, continue_label: Int) -> Emitter {
  let label = e.pending_label
  Emitter(
    ..e,
    loop_stack: [
      LoopContext(
        break_label:,
        continue_label:,
        label:,
        is_regular: False,
        cross_pop_try: 0,
        has_iterator: False,
        label_finally: None,
        drop_count: 0,
      ),
      ..e.loop_stack
    ],
    pending_label: None,
  )
}

/// for-of loop: body runs under one try frame (F_body) with iter on stack.
/// NB: must be called AFTER the F_body PushTry so cross_pop_try=1 lines up.
fn push_loop_iter(
  e: Emitter,
  break_label: Int,
  continue_label: Int,
) -> Emitter {
  let label = e.pending_label
  Emitter(
    ..e,
    loop_stack: [
      LoopContext(
        break_label:,
        continue_label:,
        label:,
        is_regular: False,
        cross_pop_try: 1,
        has_iterator: True,
        label_finally: None,
        drop_count: 0,
      ),
      ..e.loop_stack
    ],
    pending_label: None,
  )
}

/// Barrier frame for try/catch/finally bodies — never a target, only crossed.
/// Makes break/continue/return that jump out emit the right cleanup:
/// - `pop_try`: PopTry ops to keep try_stack balanced (QuickJS:
///   push_break_entry, quickjs.c:28826-28828).
/// - `label_finally`: if Some, emit_goto_loop / ReturnStatement walk emits
///   `push undef; Gosub(fin_label); Pop` after the PopTrys (quickjs.c:28889).
/// - `drop`: value-stack slots to drop when crossing a finally body itself —
///   the [slot, gosub_retpc] pair pushed by caller+Gosub — so the abrupt
///   completion inside finally replaces the saved one (never reaches IrRet).
///   QuickJS: push_break_entry(..., -1, -1, 2) at quickjs.c:28934-28935.
fn push_barrier(
  e: Emitter,
  pop_try pop_try: Int,
  label_finally label_finally: Option(Int),
  drop drop: Int,
) -> Emitter {
  Emitter(..e, loop_stack: [
    LoopContext(
      break_label: -1,
      continue_label: -1,
      label: None,
      is_regular: False,
      cross_pop_try: pop_try,
      has_iterator: False,
      label_finally:,
      drop_count: drop,
    ),
    ..e.loop_stack
  ])
}

fn pop_loop(e: Emitter) -> Emitter {
  case e.loop_stack {
    [_, ..rest] -> Emitter(..e, loop_stack: rest)
    [] -> e
  }
}

fn repeat_ir(e: Emitter, op: IrOp, n: Int) -> Emitter {
  case n <= 0 {
    True -> e
    False -> repeat_ir(emit_ir(e, op), op, n - 1)
  }
}

/// Emit Swap;Pop n times — discards n slots from *under* top-of-stack.
/// Used by ReturnStatement to nip saved [retpc, slot] when return crosses a
/// finally-body barrier (Arc has no IrNip / OP_nip_catch).
fn repeat_nip(e: Emitter, n: Int) -> Emitter {
  case n <= 0 {
    True -> e
    False -> repeat_nip(e |> emit_ir(IrSwap) |> emit_ir(IrPop), n - 1)
  }
}

/// Normal-completion entry to a finally subroutine: dummy slot + Gosub + drop.
/// Stack-neutral round trip. QuickJS: `OP_undefined; OP_gosub L; OP_drop`
/// (quickjs.c:28839-28841, 28903-28905).
fn emit_gosub_normal(e: Emitter, fin_label: Int) -> Emitter {
  e
  |> push_const(JsUndefined)
  |> emit_ir(IrGosub(fin_label))
  |> emit_ir(IrPop)
}

/// Shared body of break/continue. Walks loop_stack emitting PopTry and
/// IteratorClose for each frame *crossed* (not targeted), then jumps to the
/// target's break/continue label. Mirrors QuickJS emit_break (quickjs.c:27770).
fn emit_goto_loop(
  e: Emitter,
  name: Option(String),
  is_cont: Bool,
) -> Result(Emitter, EmitError) {
  emit_goto_loop_walk(e, e.loop_stack, name, is_cont)
}

fn emit_goto_loop_walk(
  e: Emitter,
  stack: List(LoopContext),
  name: Option(String),
  is_cont: Bool,
) -> Result(Emitter, EmitError) {
  case stack {
    [] ->
      case is_cont {
        True -> Error(ContinueOutsideLoop)
        False -> Error(BreakOutsideLoop)
      }
    [ctx, ..rest] -> {
      let target_label = case is_cont {
        True -> ctx.continue_label
        False -> ctx.break_label
      }
      // Is this the target? Unlabeled: first frame with a valid slot of the
      // right kind, skipping is_regular for break. Labeled: exact label match.
      let is_target = case name {
        Some(n) -> ctx.label == Some(n) && target_label != -1
        None ->
          case is_cont, ctx.is_regular {
            // unlabeled break must skip labeled-block frames (§14.8)
            False, True -> False
            _, _ -> target_label != -1
          }
      }
      case is_target {
        True -> Ok(emit_ir(e, IrJump(target_label)))
        False -> {
          // Crossing this frame: balance try_stack, discard saved gosub slots,
          // close iterator, then run any pending finally as a subroutine.
          // QuickJS emit_break (quickjs.c:27794-27806).
          let e = repeat_ir(e, IrPopTry, ctx.cross_pop_try)
          let e = repeat_ir(e, IrPop, ctx.drop_count)
          let e = case ctx.has_iterator {
            True -> emit_ir(e, IrIteratorClose)
            False -> e
          }
          let e = case ctx.label_finally {
            Some(lbl) -> emit_gosub_normal(e, lbl)
            None -> e
          }
          emit_goto_loop_walk(e, rest, name, is_cont)
        }
      }
    }
  }
}

fn add_child_function(e: Emitter, child: CompiledChild) -> #(Emitter, Int) {
  let idx = e.next_func
  // Arrow children inherit lexical bindings, so their references are the
  // parent's references. Non-arrows own their slots — flags don't propagate.
  let lexical_refs = case child.is_arrow {
    True -> opcode.lexical_refs_or(e.lexical_refs, child.lexical_refs)
    False -> e.lexical_refs
  }
  #(
    Emitter(
      ..e,
      // Prepended for O(1); reversed once in finish().
      functions: [child, ..e.functions],
      next_func: idx + 1,
      lexical_refs:,
    ),
    idx,
  )
}

/// Emit IrGetLexical(ref) and mark this body as referencing that binding.
fn get_lexical(e: Emitter, ref: opcode.LexicalRef) -> Emitter {
  let lexical_refs = case ref {
    opcode.RefThis -> opcode.LexicalRefs(..e.lexical_refs, this: True)
    opcode.RefActiveFunc ->
      opcode.LexicalRefs(..e.lexical_refs, active_func: True)
    opcode.RefHomeObject ->
      opcode.LexicalRefs(..e.lexical_refs, home_object: True)
    opcode.RefNewTarget ->
      opcode.LexicalRefs(..e.lexical_refs, new_target: True)
  }
  Emitter(..emit_ir(e, IrGetLexical(ref)), lexical_refs:)
}

/// Emit IrGetLexical(RefThis).
fn get_this(e: Emitter) -> Emitter {
  get_lexical(e, opcode.RefThis)
}

/// Emit IrSetThis and mark this body as referencing lexical `this`.
fn set_this(e: Emitter) -> Emitter {
  Emitter(
    ..emit_ir(e, IrSetThis),
    lexical_refs: opcode.LexicalRefs(..e.lexical_refs, this: True),
  )
}

/// Common prefix for every `super.prop` / `super[k]` form. Stack after:
/// [home_proto, this, ..]. §9.1.1.3.5 GetSuperBase = lexical home_object's
/// [[Prototype]]. Marks both lexical refs.
fn emit_super_base(e: Emitter) -> Emitter {
  e
  |> get_this
  |> get_lexical(opcode.RefHomeObject)
  |> emit_ir(IrGetPrototypeOf)
}

/// As emit_super_base but Dup's the receiver so the stack after a following
/// GetSuperValue is [val, this, ..] — the [fn, recv] shape CallMethod wants.
fn emit_super_base_keep_recv(e: Emitter) -> Emitter {
  e
  |> get_this
  |> emit_ir(IrDup)
  |> get_lexical(opcode.RefHomeObject)
  |> emit_ir(IrGetPrototypeOf)
}

/// Push the property key for a super reference. Dot form (`super.x`,
/// computed=False) pushes a literal string; computed form (`super[k]`)
/// evaluates the expression. Stack after: [key, ..].
fn emit_super_key(
  e: Emitter,
  key: ast.Expression,
  computed: Bool,
) -> Result(Emitter, EmitError) {
  case computed, key {
    False, ast.Identifier(name) -> Ok(push_const(e, JsString(name)))
    _, _ -> emit_expr(e, key)
  }
}

/// Emit a stack-neutral call to the captured `<class_fields_init>` closure
/// (§13.3.7.1 step 12 InitializeInstanceElements / §10.2.2 step 6).
/// Stack: [..] → [..]. Mirrors QuickJS emit_class_field_init: read the const,
/// skip when undefined (no instance fields), else [[Call]] with `this` = the
/// instance and no args (so its own RefNewTarget slot is undefined per spec).
fn emit_field_init_call(e: Emitter) -> Emitter {
  let #(e, skip) = fresh_label(e)
  e
  |> emit_ir(IrScopeGetVar(class_fields_init))
  |> emit_ir(IrDup)
  |> emit_ir(IrJumpIfFalse(skip))
  |> get_this
  |> emit_ir(IrSwap)
  |> emit_ir(IrCallMethod("", 0))
  |> emit_ir(IrLabel(skip))
  |> emit_ir(IrPop)
}

/// Extract final results from the emitter.
fn finish(
  e: Emitter,
) -> #(List(EmitterOp), List(JsValue), Dict(JsValue, Int), List(CompiledChild)) {
  #(
    list.reverse(e.code),
    list.reverse(e.constants_list),
    e.constants_map,
    list.reverse(e.functions),
  )
}

/// Emit a statement in "tail" position — its completion value stays on stack.
/// For expression statements, this means NOT emitting IrPop.
/// For compound statements (blocks, if/else, try/catch), propagates tail into
/// the inner last statement.
fn emit_stmt_tail(
  e: Emitter,
  stmt: ast.Statement,
) -> Result(Emitter, EmitError) {
  case stmt {
    ast.ExpressionStatement(expression: expr, ..) ->
      // Tail position: keep value on stack (no IrPop)
      emit_expr(e, expr)

    ast.BlockStatement(body) -> emit_block(e, body, tail: True)

    ast.IfStatement(condition, consequent, alternate) -> {
      let #(e, else_label) = fresh_label(e)
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, condition))
      let e = emit_ir(e, IrJumpIfFalse(else_label))
      use e <- result.try(emit_stmt_tail(e, block_wrap_fn_decl(consequent)))
      let e = emit_ir(e, IrJump(end_label))
      let e = emit_ir(e, IrLabel(else_label))
      let e = case alternate {
        Some(alt) ->
          emit_stmt_tail(e, block_wrap_fn_decl(alt)) |> result.unwrap(e)
        None -> push_const(e, JsUndefined)
      }
      let e = emit_ir(e, IrLabel(end_label))
      Ok(e)
    }

    // try with a finally: delegate to the full statement emitter (which
    // runs the finalizer via Gosub), tracking the completion value in a
    // synthetic binding (the finalizer body is excluded — §14.15.3: a
    // normally-completing Finally never supplies the completion value).
    ast.TryStatement(_, _, Some(_)) -> emit_stmt_tail_completion(e, stmt)

    ast.TryStatement(block, handler, None) -> {
      case handler {
        Some(ast.CatchClause(param, catch_body)) -> {
          let #(e, catch_label) = fresh_label(e)
          let #(e, end_label) = fresh_label(e)

          let e = emit_ir(e, IrPushTry(catch_label))
          use e <- result.try(emit_stmt_tail(e, block))
          let e = emit_ir(e, IrPopTry)
          let e = emit_ir(e, IrJump(end_label))

          let e = emit_ir(e, IrLabel(catch_label))
          let e = emit_op(e, EnterScope(BlockScope))

          let e = case param {
            Some(pattern) ->
              emit_destructuring_bind(e, pattern, CatchBinding)
              |> result.unwrap(e)
            None -> emit_ir(e, IrPop)
          }

          let saved = e
          let e = catch_annexb_ctx(e, param)
          use e <- result.try(emit_stmt_tail(e, catch_body))
          let e = restore_annexb_ctx(e, saved)
          let e = emit_op(e, LeaveScope)
          let e = emit_ir(e, IrLabel(end_label))
          Ok(e)
        }
        None -> emit_stmt_tail(e, block)
      }
    }

    ast.WithStatement(object, body) -> emit_with(e, object, body, tail: True)

    // §14.7/§14.12/§14.13 loops, labels and switch produce a completion
    // value (the spec's V — last non-empty body statement value, kept across
    // break/continue per UpdateEmpty). Track V in a synthetic block-scoped
    // binding: expression statements in the subtree store into it instead of
    // popping; nested loops/if/switch reset it to undefined on entry.
    ast.WhileStatement(..)
    | ast.DoWhileStatement(..)
    | ast.ForStatement(..)
    | ast.ForInStatement(..)
    | ast.ForOfStatement(..)
    | ast.LabeledStatement(..)
    | ast.SwitchStatement(..) -> emit_stmt_tail_completion(e, stmt)

    // All other statements: delegate to regular emit_stmt, then push undefined
    // as the completion value
    _ -> {
      use e <- result.map(emit_stmt(e, stmt))
      push_const(e, JsUndefined)
    }
  }
}

/// Tail-position emission via the completion-value mechanism: declare a
/// synthetic block-scoped binding initialized to undefined, emit the
/// statement with Emitter.completion_var set (expression statements in the
/// subtree store into it; loops/if/switch/try reset it on entry), then read
/// the binding back as the statement's completion value.
fn emit_stmt_tail_completion(
  e: Emitter,
  stmt: ast.Statement,
) -> Result(Emitter, EmitError) {
  let #(e, uniq) = fresh_label(e)
  let name = "<cptn" <> int.to_string(uniq) <> ">"
  let saved_var = e.completion_var
  let e = emit_op(e, EnterScope(BlockScope))
  let e = emit_op(e, DeclareVar(name, LetBinding))
  let e = push_const(e, JsUndefined)
  let e = emit_ir(e, IrScopeInitVar(name))
  let e = Emitter(..e, completion_var: Some(name))
  use e <- result.map(emit_stmt(e, stmt))
  let e = Emitter(..e, completion_var: saved_var)
  let e = emit_ir(e, IrScopeGetVar(name))
  emit_op(e, LeaveScope)
}

/// Emit an IrSetLine for the statement's source line (so `Error.stack` can
/// report it), unless the line is 0 — the sentinel for synthetic statements
/// the parser never produced (class field inits, desugared arrow bodies).
fn set_line(e: Emitter, line: Int) -> Emitter {
  case line {
    0 -> e
    _ ->
      case e.code {
        // An immediately preceding SetLine is dead — no op executes between
        // the two — so replace it instead of stacking markers. Statements
        // that compile to nothing (e.g. elided empty blocks) would otherwise
        // emit one IrSetLine each.
        [Ir(IrSetLine(prev)), ..rest] ->
          case prev == line {
            True -> e
            False -> Emitter(..e, code: [Ir(IrSetLine(line)), ..rest])
          }
        _ -> emit_ir(e, IrSetLine(line))
      }
  }
}

/// Emit a Block statement: enter scope, perform BlockDeclarationInstantiation
/// (§14.2.3 — block-scoped let/const slots + function declarations
/// instantiated at block entry), emit body, leave scope. Threads the Annex B
/// promotion context (see Emitter.annexb_blocked) for nested declarations.
fn emit_block(
  e: Emitter,
  body: List(ast.StmtWithLine),
  tail tail: Bool,
) -> Result(Emitter, EmitError) {
  // Lower any direct using/await-using declarations BEFORE the lexical-name
  // and Annex B scans below — the desugar moves the block's statements into
  // a try block and rewrites using → const.
  let body = using_desugar.desugar_list(body)
  // Scope elision (V8/SpiderMonkey do the same): a block that declares
  // nothing block-scoped creates no bindings, so the scope — and all the
  // Annex B context bookkeeping, whose effect would be a no-op fold of
  // empty sets — is skipped entirely. Keeps `{}`-heavy code (e.g. the 2^21
  // empty blocks of staging/sm/regress/regress-610026.js) out of
  // EnterScope/LeaveScope and runtime env churn.
  use <- bool.lazy_guard(!block_has_declarations(body), fn() {
    case tail {
      True -> emit_stmts_tail(e, body)
      False -> emit_stmts(e, body)
    }
  })
  let saved = e
  let e = emit_op(e, EnterScope(BlockScope))
  let e =
    Emitter(
      ..e,
      in_block: True,
      annexb_blocked: set.union(
        set.union(e.annexb_blocked, e.annexb_level_fns),
        top_lex_name_set(body),
      ),
      annexb_level_fns: set.from_list(direct_fn_names(body)),
    )
  let e = emit_block_declarations(e, body)
  use e <- result.map(case tail {
    True -> emit_stmts_tail(e, body)
    False -> emit_stmts(e, body)
  })
  let e = restore_annexb_ctx(e, saved)
  emit_op(e, LeaveScope)
}

/// §14.2.3 BlockDeclarationInstantiation: hoist the block's let/const/class
/// slots (so closures capture boxed cells, and TDZ holds from block entry),
/// then create + initialize the block-scoped bindings for the block's
/// function declarations. Two passes over the declarations so mutually
/// recursive block functions see each other's slots at MakeClosure time.
fn emit_block_declarations(
  e: Emitter,
  body: List(ast.StmtWithLine),
) -> Emitter {
  let e =
    list.fold(collect_top_lex_names(body), e, fn(e, lex) {
      let #(name, kind) = lex
      emit_op(e, DeclareVar(name, kind))
    })
  let #(e, funcs) = collect_hoisted_funcs(e, body)
  let e =
    list.fold(funcs, e, fn(e, hf) {
      let #(name, _idx) = hf
      emit_op(e, DeclareVar(name, LetBinding))
    })
  list.fold(funcs, e, fn(e, hf) {
    let #(name, idx) = hf
    let e = emit_ir(e, IrMakeClosure(idx))
    emit_ir(e, IrScopeInitVar(name))
  })
}

/// Annex B §B.3.1: a bare FunctionDeclaration as an if/else clause behaves
/// as if wrapped in a Block. Wrapping makes the normal block path handle
/// both the block-scoped binding and the sloppy-mode var promotion.
fn block_wrap_fn_decl(stmt: ast.Statement) -> ast.Statement {
  case stmt {
    ast.FunctionDeclaration(..) ->
      ast.BlockStatement([ast.StmtWithLine(0, stmt)])
    _ -> stmt
  }
}

/// Annex B blocking context for a catch clause body: destructured catch
/// params block var promotion of same-named block fns inside the catch body
/// (§B.3.4 exempts only simple identifier params).
fn catch_annexb_ctx(e: Emitter, param: Option(ast.Pattern)) -> Emitter {
  case param {
    Some(ast.IdentifierPattern(_)) | None -> e
    Some(pattern) ->
      Emitter(
        ..e,
        in_block: False,
        annexb_blocked: set.union(
          set.union(e.annexb_blocked, e.annexb_level_fns),
          set.from_list(collect_pattern_names(pattern)),
        ),
        annexb_level_fns: set.new(),
      )
  }
}

/// Annex B blocking context for an intermediate lexical scope introduced by
/// a for/for-in/for-of head let/const declaration.
fn annexb_head_ctx(e: Emitter, head_names: List(String)) -> Emitter {
  case head_names {
    [] -> e
    _ ->
      Emitter(
        ..e,
        in_block: False,
        annexb_blocked: set.union(
          set.union(e.annexb_blocked, e.annexb_level_fns),
          set.from_list(head_names),
        ),
        annexb_level_fns: set.new(),
      )
  }
}

/// Restore the Annex B promotion context saved before entering a scope.
fn restore_annexb_ctx(e: Emitter, saved: Emitter) -> Emitter {
  Emitter(
    ..e,
    in_block: saved.in_block,
    annexb_blocked: saved.annexb_blocked,
    annexb_level_fns: saved.annexb_level_fns,
  )
}

/// Like emit_stmts but the last statement is emitted in tail position.
fn emit_stmts_tail(
  e: Emitter,
  stmts: List(ast.StmtWithLine),
) -> Result(Emitter, EmitError) {
  // §14 UpdateEmpty: declarations, empty and debugger statements have an
  // EMPTY completion — the completion value of a statement list is that of
  // the last VALUE-PRODUCING statement (eval('1; class C {}') === 1).
  // Trailing vacuous statements still execute, after the value is on stack;
  // emit_stmt is stack-neutral so the value rides below them.
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

/// §14: statements whose completion is EMPTY — skipped over (backwards) when
/// picking the statement that supplies the eval/program completion value.
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
    [] -> Ok(push_const(e, JsUndefined))
    [only] -> emit_stmt_tail(set_line(e, only.line), only.statement)
    [first, ..rest] -> {
      use e <- result.try(emit_stmt(set_line(e, first.line), first.statement))
      emit_stmts_tail_value(e, rest)
    }
  }
}

// ============================================================================
// Hoisting
// ============================================================================

/// Collect all var-declared names in a function body (not entering nested functions).
fn collect_hoisted_vars(stmts: List(ast.StmtWithLine)) -> List(String) {
  list.flat_map(stmts, collect_vars_located)
  |> list.unique()
}

/// Peel a StmtWithLine for the recursive var-collection walk.
fn collect_vars_located(s: ast.StmtWithLine) -> List(String) {
  collect_vars_stmt(s.statement)
}

/// Recursively extract all bound variable names from a pattern.
fn collect_pattern_names(pattern: ast.Pattern) -> List(String) {
  case pattern {
    ast.IdentifierPattern(name) -> [name]
    ast.ArrayPattern(elements) ->
      list.flat_map(elements, fn(elem) {
        elem |> option.map(collect_pattern_names) |> option.unwrap([])
      })
    ast.ObjectPattern(properties) ->
      list.flat_map(properties, fn(prop) {
        case prop {
          ast.PatternProperty(_, value:, ..) -> collect_pattern_names(value)
          ast.RestProperty(argument) -> collect_pattern_names(argument)
        }
      })
    ast.AssignmentPattern(left, _) -> collect_pattern_names(left)
    ast.RestElement(argument) -> collect_pattern_names(argument)
  }
}

fn for_let_names(decl: ast.Statement) -> List(String) {
  case decl {
    ast.VariableDeclaration(ast.Let, ds) ->
      list.flat_map(ds, fn(d) {
        let ast.VariableDeclarator(p, _) = d
        collect_pattern_names(p)
      })
    _ -> []
  }
}

fn collect_vars_stmt(stmt: ast.Statement) -> List(String) {
  case stmt {
    ast.VariableDeclaration(ast.Var, declarators) ->
      list.flat_map(declarators, fn(d) {
        case d {
          ast.VariableDeclarator(pattern, _) -> collect_pattern_names(pattern)
        }
      })
    ast.BlockStatement(body) -> list.flat_map(body, collect_vars_located)
    ast.IfStatement(_, consequent, alternate) ->
      list.append(
        collect_vars_stmt(consequent),
        alternate |> option.map(collect_vars_stmt) |> option.unwrap([]),
      )
    ast.WhileStatement(_, body) -> collect_vars_stmt(body)
    ast.DoWhileStatement(_, body) -> collect_vars_stmt(body)
    ast.ForStatement(init, _, _, body) -> {
      let init_vars = case init {
        Some(ast.ForInitDeclaration(ast.VariableDeclaration(ast.Var, decls))) ->
          list.flat_map(decls, fn(d) {
            case d {
              ast.VariableDeclarator(pattern, _) ->
                collect_pattern_names(pattern)
            }
          })
        _ -> []
      }
      list.append(init_vars, collect_vars_stmt(body))
    }
    ast.TryStatement(block, handler, finalizer) -> {
      let block_vars = case block {
        ast.BlockStatement(body) -> list.flat_map(body, collect_vars_located)
        _ -> collect_vars_stmt(block)
      }
      let handler_vars = case handler {
        Some(ast.CatchClause(_, body)) ->
          case body {
            ast.BlockStatement(b) -> list.flat_map(b, collect_vars_located)
            _ -> collect_vars_stmt(body)
          }
        None -> []
      }
      let finally_vars = case finalizer {
        Some(f) ->
          case f {
            ast.BlockStatement(b) -> list.flat_map(b, collect_vars_located)
            _ -> collect_vars_stmt(f)
          }
        None -> []
      }
      list.flatten([block_vars, handler_vars, finally_vars])
    }
    ast.ForInStatement(left, _, body) | ast.ForOfStatement(left, _, body, ..) -> {
      let left_vars = case left {
        ast.ForInitDeclaration(ast.VariableDeclaration(ast.Var, decls)) ->
          list.flat_map(decls, fn(d) {
            case d {
              ast.VariableDeclarator(pattern, _) ->
                collect_pattern_names(pattern)
            }
          })
        _ -> []
      }
      list.append(left_vars, collect_vars_stmt(body))
    }
    ast.LabeledStatement(_, body) -> collect_vars_stmt(body)
    ast.WithStatement(_, body) -> collect_vars_stmt(body)
    ast.SwitchStatement(_, cases) ->
      list.flat_map(cases, fn(c) {
        case c {
          ast.SwitchCase(_, consequent) ->
            list.flat_map(consequent, collect_vars_located)
        }
      })
    // Function declarations are NOT VarDeclaredNames: at the body top level
    // their names are declared alongside collect_hoisted_funcs, and inside
    // blocks they are block-scoped lexical bindings (§14.2.3). Sloppy-mode
    // Annex B var promotion is handled separately by
    // collect_annexb_candidates.
    ast.FunctionDeclaration(..) -> []
    _ -> []
  }
}

/// Collect let/const names declared directly in the given statement list (NOT
/// recursing into nested blocks). Used to hoist slot-allocation+boxing before
/// hoisted-function MakeClosure so closures capture the box ref, not a stale
/// pre-box value.
fn collect_top_lex_names(
  stmts: List(ast.StmtWithLine),
) -> List(#(String, BindingKind)) {
  list.flat_map(stmts, fn(located) {
    case located.statement {
      ast.VariableDeclaration(ast.Let, declarators) ->
        list.flat_map(declarators, fn(d) {
          let ast.VariableDeclarator(pattern, _) = d
          collect_pattern_names(pattern)
          |> list.map(fn(n) { #(n, LetBinding) })
        })
      ast.VariableDeclaration(ast.Const, declarators) ->
        list.flat_map(declarators, fn(d) {
          let ast.VariableDeclarator(pattern, _) = d
          collect_pattern_names(pattern)
          |> list.map(fn(n) { #(n, ConstBinding) })
        })
      ast.ClassDeclaration(name: Some(name), ..) -> [#(name, LetBinding)]
      _ -> []
    }
  })
}

/// True when a block body declares anything block-scoped: let/const/class or
/// a (possibly labeled) function/generator/async declaration. Blocks without
/// such declarations create no bindings and emit_block elides their scope.
fn block_has_declarations(body: List(ast.StmtWithLine)) -> Bool {
  list.any(body, fn(located) {
    case peel_labels(located.statement) {
      ast.VariableDeclaration(ast.Let, _) -> True
      ast.VariableDeclaration(ast.Const, _) -> True
      ast.ClassDeclaration(..) -> True
      ast.FunctionDeclaration(..) -> True
      _ -> False
    }
  })
}

/// FunctionDeclarations may sit under labels (`l: function f() {}`, sloppy
/// mode only); labels are transparent for declaration instantiation.
fn peel_labels(stmt: ast.Statement) -> ast.Statement {
  case stmt {
    ast.LabeledStatement(_, body) -> peel_labels(body)
    _ -> stmt
  }
}

/// Names of FunctionDeclarations sitting directly in a statement list.
fn direct_fn_names(stmts: List(ast.StmtWithLine)) -> List(String) {
  list.filter_map(stmts, fn(located) {
    case peel_labels(located.statement) {
      ast.FunctionDeclaration(Some(name), ..) -> Ok(name)
      _ -> Error(Nil)
    }
  })
}

/// let/const/class names declared directly in a statement list, as a set.
fn top_lex_name_set(stmts: List(ast.StmtWithLine)) -> Set(String) {
  collect_top_lex_names(stmts)
  |> list.map(fn(lex) { lex.0 })
  |> set.from_list
}

/// let/const names bound by a for/for-in/for-of head declaration (these form
/// a lexical scope enclosing the loop body for Annex B blocking purposes).
fn for_head_lex_names(decl: ast.Statement) -> List(String) {
  case decl {
    ast.VariableDeclaration(ast.Let, ds)
    | ast.VariableDeclaration(ast.Const, ds) ->
      list.flat_map(ds, fn(d) {
        let ast.VariableDeclarator(p, _) = d
        collect_pattern_names(p)
      })
    _ -> []
  }
}

// ============================================================================
// Annex B §B.3.2 — sloppy-mode function-in-block var promotion candidates
// ============================================================================
//
// A FunctionDeclaration in a block gets, in addition to its block-scoped
// binding, a function/script/eval-level var binding (initialized undefined
// before the body runs, updated when the declaration is evaluated) — UNLESS
// replacing the declaration with `var F` would produce an early error:
//   - a let/const/class named F in any enclosing scope within the body
//   - a function declaration named F in an enclosing BLOCK (block fn decls
//     are lexical in their block)
//   - a destructured CatchParameter binding F (§B.3.4 exempts only simple
//     identifier catch params)
//   - a for/for-in/for-of head let/const binding F enclosing the block
// Mirrors the "would not produce any Early Errors for body" condition of
// §B.3.2.3 (functions), §B.3.2.2 (global) and §B.3.2.6 (eval).

/// Collect candidate names for Annex B var promotion in a sloppy body.
pub fn collect_annexb_candidates(
  stmts: List(ast.StmtWithLine),
) -> List(String) {
  collect_annexb_candidates_with(stmts, top_lex_name_set(stmts))
}

/// Like collect_annexb_candidates but takes the body's already-computed
/// top-level lexical name set, so callers that have one don't rescan.
fn collect_annexb_candidates_with(
  stmts: List(ast.StmtWithLine),
  top_lex: Set(String),
) -> List(String) {
  annexb_walk(stmts, top_lex, False)
  |> list.unique
}

/// Walk one statement list. `blocked` holds lexical names of enclosing scope
/// levels (including this level's let/const/class). `in_block` is True when
/// this list is a Block or switch CaseBlock body (fn decls here are
/// candidates); False at the var-scope top level (fn decls there are plain
/// vars, handled by the normal hoisting path).
fn annexb_walk(
  stmts: List(ast.StmtWithLine),
  blocked: Set(String),
  in_block: Bool,
) -> List(String) {
  case stmts {
    // Empty statement list: no candidates — skip the set bookkeeping.
    [] -> []
    _ -> {
      // This level's fn names are lexical for any NESTED scope, but don't
      // block their own (or a sibling's) promotion at this level.
      let child_blocked = case in_block {
        True -> set.union(blocked, set.from_list(direct_fn_names(stmts)))
        False -> blocked
      }
      list.flat_map(stmts, fn(located) {
        annexb_stmt(located.statement, blocked, child_blocked, in_block)
      })
    }
  }
}

/// `blocked` applies to fn decls AT this level; `child_blocked` (= blocked +
/// this level's fn names) seeds any nested scope.
fn annexb_stmt(
  stmt: ast.Statement,
  blocked: Set(String),
  child_blocked: Set(String),
  in_block: Bool,
) -> List(String) {
  case stmt {
    // §B.3.2/§B.3.3 web-compat promotion applies ONLY to plain function
    // declarations — never generators, async functions, or async generators
    // (matching V8/SpiderMonkey/JSC).
    ast.FunctionDeclaration(
      name: Some(name),
      is_generator: False,
      is_async: False,
      ..,
    ) ->
      case in_block && !set.contains(blocked, name) {
        True -> [name]
        False -> []
      }
    ast.FunctionDeclaration(..) -> []
    // Empty block fast path: avoid building the merged blocked-set only for
    // annexb_walk to discard it.
    ast.BlockStatement([]) -> []
    ast.BlockStatement(body) ->
      annexb_walk(body, set.union(child_blocked, top_lex_name_set(body)), True)
    ast.IfStatement(_, consequent, alternate) ->
      list.append(
        annexb_sub_stmt(consequent, child_blocked),
        alternate
          |> option.map(annexb_sub_stmt(_, child_blocked))
          |> option.unwrap([]),
      )
    ast.WhileStatement(_, body) | ast.DoWhileStatement(_, body) ->
      annexb_sub_stmt(body, child_blocked)
    ast.WithStatement(_, body) -> annexb_sub_stmt(body, child_blocked)
    ast.ForStatement(init, _, _, body) -> {
      let head = case init {
        Some(ast.ForInitDeclaration(decl)) -> for_head_lex_names(decl)
        _ -> []
      }
      annexb_sub_stmt(body, set.union(child_blocked, set.from_list(head)))
    }
    ast.ForInStatement(left, _, body) | ast.ForOfStatement(left, _, body, ..) -> {
      let head = case left {
        ast.ForInitDeclaration(decl) -> for_head_lex_names(decl)
        _ -> []
      }
      annexb_sub_stmt(body, set.union(child_blocked, set.from_list(head)))
    }
    ast.SwitchStatement(_, cases) -> {
      let case_stmts =
        list.flat_map(cases, fn(c) {
          let ast.SwitchCase(_, consequent) = c
          consequent
        })
      annexb_walk(
        case_stmts,
        set.union(child_blocked, top_lex_name_set(case_stmts)),
        True,
      )
    }
    ast.TryStatement(block, handler, finalizer) -> {
      let from_block = annexb_sub_stmt(block, child_blocked)
      let from_catch = case handler {
        Some(ast.CatchClause(param, catch_body)) -> {
          // §B.3.4: a SIMPLE identifier catch param does not block `var F`
          // in the catch block; a destructured param does.
          let param_block = case param {
            Some(ast.IdentifierPattern(_)) | None -> []
            Some(pattern) -> collect_pattern_names(pattern)
          }
          annexb_sub_stmt(
            catch_body,
            set.union(child_blocked, set.from_list(param_block)),
          )
        }
        None -> []
      }
      let from_finally =
        finalizer
        |> option.map(annexb_sub_stmt(_, child_blocked))
        |> option.unwrap([])
      list.flatten([from_block, from_catch, from_finally])
    }
    ast.LabeledStatement(_, body) ->
      annexb_stmt(body, blocked, child_blocked, in_block)
    _ -> []
  }
}

/// A statement in sub-statement position (if/loop body, try sub-block):
/// a bare FunctionDeclaration there is Annex B "function in IfStatement
/// clause" — semantically a synthetic block containing just the declaration.
fn annexb_sub_stmt(stmt: ast.Statement, blocked: Set(String)) -> List(String) {
  case stmt {
    ast.FunctionDeclaration(..) ->
      annexb_walk([ast.StmtWithLine(0, stmt)], blocked, True)
    _ -> annexb_stmt(stmt, blocked, blocked, False)
  }
}

/// Collect and compile hoisted function declarations.
/// Returns updated emitter + list of (name, func_index) pairs.
fn collect_hoisted_funcs(
  e: Emitter,
  stmts: List(ast.StmtWithLine),
) -> #(Emitter, List(#(String, Int))) {
  let #(e, funcs_rev) =
    list.fold(stmts, #(e, []), fn(acc, located) {
      let #(e, funcs) = acc
      case peel_labels(located.statement) {
        ast.FunctionDeclaration(Some(name), params, body, is_gen, is_async) -> {
          let child =
            compile_function_body(
              e,
              Some(name),
              None,
              params,
              body,
              False,
              is_gen,
              is_async,
              // Function declaration: a constructor unless gen/async.
              !is_gen && !is_async,
              opcode.fn_perms,
              NoFieldInit,
            )
          let #(e, idx) = add_child_function(e, child)
          #(e, [#(name, idx), ..funcs])
        }
        _ -> #(e, funcs)
      }
    })
  #(e, list.reverse(funcs_rev))
}

// ============================================================================
// `arguments` usage detection
// ============================================================================
//
// We walk the AST looking for `Identifier("arguments")`. The walk recurses
// into arrow function bodies (arrows inherit the enclosing `arguments`
// binding, just like `this`) but does NOT recurse into non-arrow function
// bodies, which have their own separate `arguments` binding.
//
// This is a compile-time scan so functions that never reference `arguments`
// pay zero allocation cost.

fn stmts_reference_arguments(stmts: List(ast.StmtWithLine)) -> Bool {
  list.any(stmts, fn(located) { stmt_references_arguments(located.statement) })
}

fn stmt_references_arguments(stmt: ast.Statement) -> Bool {
  case stmt {
    ast.EmptyStatement
    | ast.DebuggerStatement
    | ast.BreakStatement(_)
    | ast.ContinueStatement(_) -> False

    ast.ExpressionStatement(expression: expr, ..) ->
      expr_references_arguments(expr)
    ast.BlockStatement(body) -> stmts_reference_arguments(body)
    ast.ReturnStatement(arg) -> opt_expr_references_arguments(arg)
    ast.ThrowStatement(arg) -> expr_references_arguments(arg)

    ast.IfStatement(cond, cons, alt) ->
      expr_references_arguments(cond)
      || stmt_references_arguments(cons)
      || opt_stmt_references_arguments(alt)

    ast.WhileStatement(cond, body) ->
      expr_references_arguments(cond) || stmt_references_arguments(body)
    ast.DoWhileStatement(cond, body) ->
      expr_references_arguments(cond) || stmt_references_arguments(body)

    ast.ForStatement(init, cond, upd, body) ->
      opt_for_init_references_arguments(init)
      || opt_expr_references_arguments(cond)
      || opt_expr_references_arguments(upd)
      || stmt_references_arguments(body)

    ast.ForInStatement(left, right, body)
    | ast.ForOfStatement(left, right, body, ..) ->
      for_init_references_arguments(left)
      || expr_references_arguments(right)
      || stmt_references_arguments(body)

    ast.SwitchStatement(disc, cases) ->
      expr_references_arguments(disc)
      || list.any(cases, fn(c) {
        let ast.SwitchCase(cond, cons) = c
        opt_expr_references_arguments(cond) || stmts_reference_arguments(cons)
      })

    ast.TryStatement(block, handler, finalizer) ->
      stmt_references_arguments(block)
      || handler
      |> option.map(fn(h) { stmt_references_arguments(h.body) })
      |> option.unwrap(False)
      || opt_stmt_references_arguments(finalizer)

    ast.LabeledStatement(_, body) -> stmt_references_arguments(body)
    ast.WithStatement(obj, body) ->
      expr_references_arguments(obj) || stmt_references_arguments(body)

    ast.VariableDeclaration(_, decls) ->
      list.any(decls, fn(d) {
        let ast.VariableDeclarator(id, init) = d
        pattern_references_arguments(id) || opt_expr_references_arguments(init)
      })

    // Non-arrow function declaration: has its own `arguments` binding, do NOT
    // recurse into body. But DO check default param expressions (they run in
    // the enclosing scope before the new function's arguments is created).
    // Actually — spec-wise, default param exprs of a nested function have
    // access to the NESTED function's arguments, not the enclosing one.
    // So fully skip.
    ast.FunctionDeclaration(_, _, _, _, _) -> False

    ast.ClassDeclaration(_, super_class, body) ->
      opt_expr_references_arguments(super_class)
      || class_body_references_arguments(body)

    ast.ClassFieldInit(key:, value:, computed:) ->
      { computed && expr_references_arguments(key) }
      || opt_expr_references_arguments(value)
  }
}

fn expr_references_arguments(expr: ast.Expression) -> Bool {
  case expr {
    ast.Identifier("arguments") -> True
    ast.Identifier(_) -> False

    ast.NumberLiteral(_)
    | ast.BigIntLiteral(_)
    | ast.StringExpression(_)
    | ast.BooleanLiteral(_)
    | ast.NullLiteral
    | ast.UndefinedExpression
    | ast.ThisExpression
    | ast.SuperExpression
    | ast.MetaProperty(_, _)
    | ast.RegExpLiteral(_, _) -> False

    ast.BinaryExpression(_, l, r) | ast.LogicalExpression(_, l, r) ->
      expr_references_arguments(l) || expr_references_arguments(r)

    ast.UnaryExpression(_, _, arg)
    | ast.UpdateExpression(_, _, arg)
    | ast.AwaitExpression(arg)
    | ast.SpreadElement(arg) -> expr_references_arguments(arg)

    ast.ImportExpression(source, options, _) ->
      expr_references_arguments(source)
      || opt_expr_references_arguments(options)

    ast.YieldExpression(arg, _) -> opt_expr_references_arguments(arg)

    ast.AssignmentExpression(_, l, r) ->
      expr_references_arguments(l) || expr_references_arguments(r)

    ast.CallExpression(callee, args)
    | ast.OptionalCallExpression(callee, args)
    | ast.NewExpression(callee, args) ->
      expr_references_arguments(callee)
      || list.any(args, expr_references_arguments)

    ast.MemberExpression(obj, prop, computed)
    | ast.OptionalMemberExpression(obj, prop, computed) ->
      expr_references_arguments(obj)
      || { computed && expr_references_arguments(prop) }

    ast.ConditionalExpression(c, t, a) ->
      expr_references_arguments(c)
      || expr_references_arguments(t)
      || expr_references_arguments(a)

    ast.ArrayExpression(elems) -> list.any(elems, opt_expr_references_arguments)

    ast.ObjectExpression(props) ->
      list.any(props, fn(p) {
        case p {
          ast.Property(key, value, _, computed, _, _) ->
            { computed && expr_references_arguments(key) }
            || expr_references_arguments(value)
          ast.SpreadProperty(arg) -> expr_references_arguments(arg)
        }
      })

    ast.SequenceExpression(exprs) -> list.any(exprs, expr_references_arguments)

    ast.TemplateLiteral(_, exprs) -> list.any(exprs, expr_references_arguments)

    ast.TaggedTemplateExpression(tag:, expressions:, ..) ->
      expr_references_arguments(tag)
      || list.any(expressions, expr_references_arguments)

    ast.IntrinsicTemplateObject(..) -> False

    // Non-arrow function expression: has its own `arguments`, skip entirely.
    ast.FunctionExpression(_, _, _, _, _) -> False

    // Arrow: inherits enclosing `arguments`, recurse into body AND default
    // param values (arrows have no own binding so `arguments` in defaults
    // also refers to the enclosing scope).
    ast.ArrowFunctionExpression(params, arrow_body, _) ->
      list.any(params, pattern_references_arguments)
      || case arrow_body {
        ast.ArrowBodyExpression(e) -> expr_references_arguments(e)
        ast.ArrowBodyBlock(s) -> stmt_references_arguments(s)
      }

    ast.ClassExpression(_, super_class, body) ->
      opt_expr_references_arguments(super_class)
      || class_body_references_arguments(body)

    ast.ParenthesizedExpression(inner) -> expr_references_arguments(inner)

    ast.IntrinsicGetDisposer(argument:, ..) ->
      expr_references_arguments(argument)
    ast.IntrinsicMakeSuppressed(error:, suppressed:) ->
      expr_references_arguments(error) || expr_references_arguments(suppressed)
  }
}

fn opt_expr_references_arguments(e: Option(ast.Expression)) -> Bool {
  e |> option.map(expr_references_arguments) |> option.unwrap(False)
}

fn opt_stmt_references_arguments(s: Option(ast.Statement)) -> Bool {
  s |> option.map(stmt_references_arguments) |> option.unwrap(False)
}

fn for_init_references_arguments(init: ast.ForInit) -> Bool {
  case init {
    ast.ForInitExpression(e) -> expr_references_arguments(e)
    ast.ForInitDeclaration(s) -> stmt_references_arguments(s)
    ast.ForInitPattern(p) -> pattern_references_arguments(p)
  }
}

fn opt_for_init_references_arguments(init: Option(ast.ForInit)) -> Bool {
  init |> option.map(for_init_references_arguments) |> option.unwrap(False)
}

fn pattern_references_arguments(p: ast.Pattern) -> Bool {
  // Patterns only contain expressions in default-value positions (AssignmentPattern)
  // and in computed object-pattern keys.
  case p {
    ast.IdentifierPattern(_) -> False
    ast.RestElement(inner) -> pattern_references_arguments(inner)
    ast.AssignmentPattern(left, right) ->
      pattern_references_arguments(left) || expr_references_arguments(right)
    ast.ArrayPattern(elems) ->
      list.any(elems, fn(e) {
        e |> option.map(pattern_references_arguments) |> option.unwrap(False)
      })
    ast.ObjectPattern(props) ->
      list.any(props, fn(prop) {
        case prop {
          ast.PatternProperty(key, value, computed, _) ->
            { computed && expr_references_arguments(key) }
            || pattern_references_arguments(value)
          ast.RestProperty(inner) -> pattern_references_arguments(inner)
        }
      })
  }
}

fn class_body_references_arguments(body: List(ast.ClassElement)) -> Bool {
  // Class methods are non-arrow functions — they have their own `arguments`.
  // We only need to scan: computed keys, field initialisers (which spec-wise
  // run in a scope where `arguments` from enclosing is NOT visible — but in
  // practice they run as method bodies on the instance; skip for safety),
  // and static blocks (which DO have their own `arguments` forbidden… skip).
  // For the detector's purposes, only computed keys matter here.
  list.any(body, fn(el) {
    case el {
      ast.ClassMethod(key, _, _, _, computed)
      | ast.ClassField(key, _, _, computed) ->
        computed && expr_references_arguments(key)
      ast.StaticBlock(_) -> False
    }
  })
}

/// Split off a trailing rest parameter. Returns the fixed params (in order)
/// and the rest target pattern (the binding inside `...`), if present. A rest
/// element is only valid as the last parameter, so we only check the tail.
fn split_trailing_rest(
  params: List(ast.Pattern),
) -> #(List(ast.Pattern), Option(ast.Pattern)) {
  case list.reverse(params) {
    [ast.RestElement(inner), ..rev_fixed] -> #(
      list.reverse(rev_fixed),
      Some(inner),
    )
    _ -> #(params, None)
  }
}

/// Compile a function body into a CompiledChild.
fn compile_function_body(
  parent: Emitter,
  name: Option(String),
  self_name: Option(String),
  params: List(ast.Pattern),
  body: ast.Statement,
  is_arrow: Bool,
  is_generator: Bool,
  is_async: Bool,
  is_constructor: Bool,
  perms: opcode.SyntaxPerms,
  field_init: FieldInitMode,
) -> CompiledChild {
  let stmts = case body {
    ast.BlockStatement(s) -> s
    other -> [ast.StmtWithLine(0, other)]
  }
  // Lower any direct using/await-using declarations before the directive,
  // lexical-name, and hoisting scans (directives stay ahead of the desugar's
  // prelude, and var/function hoisting descends into the generated try
  // block, so the scans see the same names either way).
  let stmts = using_desugar.desugar_list(stmts)

  // Strictness: inherit from parent, upgrade if body prologue has "use strict".
  // (Classes force strict at the call site by passing a strict parent emitter.)
  let child_strict = parent.strict || has_use_strict_directive(stmts)

  // SyntaxPerms (mirrors quickjs.c:36052-36076). Arrows inherit the parent
  // emitter's perms verbatim — `perms` is ignored. Non-arrows use `perms` as
  // passed by the caller (fn_perms / method_perms / derived_ctor_perms).
  let syntax_perms = case is_arrow {
    True -> parent.syntax_perms
    False -> perms
  }
  // Like SyntaxPerms, arrows inherit FieldInitMode so `()=>super()` inside a
  // derived ctor can emit the init call; non-arrows take the caller's value
  // (only ctors get a non-NoFieldInit). Only FieldInitAfterSuper is
  // inherited — it fires on the `super()` call itself. FieldInitAtStart must
  // NOT leak into arrows: it fires at body entry, so an arrow inside a
  // base-class ctor would re-run the initializer on every arrow call
  // (observable as a double private-element add → TypeError).
  let field_init = case is_arrow, parent.field_init {
    True, FieldInitAfterSuper -> FieldInitAfterSuper
    True, FieldInitAtStart | True, NoFieldInit -> NoFieldInit
    False, _ -> field_init
  }
  // Annex B promotion of a block-level function declaration is skipped when
  // its name collides with a formal parameter (B.3.3.1: "F is not an element
  // of BoundNames of argumentsList"), so parameter bound names join the
  // blocked set. This is function-code-only — B.3.3.2/B.3.3.3 (script/eval)
  // have no parameter condition.
  let param_names = set.from_list(list.flat_map(params, collect_pattern_names))

  // Use a fresh emitter inheriting nothing from parent (except label counter
  // for uniqueness, and strictness).
  let e =
    Emitter(
      ..new_emitter(),
      next_label: parent.next_label,
      strict: child_strict,
      is_async:,
      is_arrow:,
      syntax_perms:,
      field_init:,
      annexb_blocked: set.union(top_lex_name_set(stmts), param_names),
      // Enclosing `with` scopes stay visible to nested functions — their
      // free names must check the with objects (captured from the parent).
      with_stack: parent.with_stack,
      // A function's [[PrivateEnvironment]] is fixed at its definition
      // site — nested functions see the enclosing classes' private names.
      private_env: parent.private_env,
    )

  let e = emit_op(e, EnterScope(FunctionScope))

  // Non-arrows own all four lexical slots starting at len(captures), in
  // canonical order. Runtime setup_locals writes [this, active_func,
  // home_object, new_target] there before pc=0. Arrows skip — their
  // IrGetLexical resolves to captures from the enclosing non-arrow.
  let e = case is_arrow {
    True -> e
    False ->
      list.fold(opcode.all_lexical_refs, e, fn(e, ref) {
        emit_op(e, DeclareLexical(ref))
      })
  }

  // A trailing rest parameter (`...rest`) is bound separately from the fixed
  // params: the fixed ones bind positionally (arity counts only them, so
  // build_locals leaves the rest slot undefined), then IrCreateRestArray
  // collects the leftover args into an Array. `arity` excludes the rest param,
  // which also gives the correct `fn.length` (§15.1.5).
  let #(fixed_params, rest_param) = split_trailing_rest(params)
  let arity = list.length(fixed_params)
  // §15.1.5 ExpectedArgumentCount (the `length` property): leading params
  // before the first one with a default initializer. Destructuring patterns
  // WITHOUT a default still count; the trailing rest is already split off.
  let expected_length =
    fixed_params
    |> list.take_while(fn(p) {
      case p {
        ast.AssignmentPattern(..) -> False
        _ -> True
      }
    })
    |> list.length

  // §10.2.11: a parameter list containing a default or a destructuring
  // pattern is non-simple. Its bindings are created uninitialized (TDZ) and
  // then initialized strictly left to right, so a default initializer that
  // reads its own or a later parameter throws a ReferenceError. Simple lists
  // keep the fast positional path (args land directly in the named slots).
  let non_simple_fixed =
    list.any(fixed_params, fn(p) {
      case p {
        ast.IdentifierPattern(_) -> False
        _ -> True
      }
    })

  // Phase 1: Declare parameters. Simple lists bind identifiers positionally;
  // non-simple lists route EVERY fixed param through a synthetic positional
  // slot so the real names can be TDZ-declared and initialized in order.
  let #(e, destructured_params_rev) =
    list.index_fold(fixed_params, #(e, []), fn(acc, param, idx) {
      let #(e, destr) = acc
      case non_simple_fixed, param {
        False, ast.IdentifierPattern(pname) -> #(
          emit_op(e, DeclareVar(pname, ParamBinding)),
          destr,
        )
        _, _ -> {
          let synthetic = "$param_" <> int.to_string(idx)
          let e = emit_op(e, DeclareVar(synthetic, ParamBinding))
          #(e, [#(synthetic, param), ..destr])
        }
      }
    })
  let destructured_params = list.reverse(destructured_params_rev)

  // Detect whether the function body references `arguments`. We scan the body
  // AND the parameter patterns (default-value expressions can reference
  // `arguments`), recursing into arrow functions (which inherit the enclosing
  // arguments binding) but NOT into non-arrow nested functions (which have
  // their own). Only non-arrow functions get the binding — arrows resolve
  // `arguments` as a free variable captured from the enclosing scope.
  let uses_args = case is_arrow {
    True -> False
    False ->
      list.any(params, pattern_references_arguments)
      || stmts_reference_arguments(stmts)
  }
  // Declare `arguments` immediately after params so it's local; emit
  // IrCreateArguments to build the object at runtime from state.call_args,
  // then store it into the local slot. This must happen before parameter
  // destructuring so default-value expressions can use `arguments`.
  let e = case uses_args {
    True -> {
      let simple_params = !non_simple_fixed && rest_param == None
      let e = emit_op(e, DeclareVar("arguments", VarBinding))
      let e = emit_ir(e, IrCreateArguments(simple_params:))
      emit_ir(e, IrScopePutVar("arguments"))
    }
    False -> e
  }

  // §13.2.5.5 InstantiateOrdinaryFunctionExpression: a named function
  // expression binds its own name in a scope wrapped around the function
  // (funcEnv), holding the closure itself, immutably. Our flat scope model
  // has no separate funcEnv, so shadowing by inner environments (params,
  // `arguments`, body vars, hoisted/Annex-B functions, top-level lexicals)
  // is represented by skipping the binding entirely — reads then resolve
  // to the shadowing binding exactly as the spec's environment chain would.
  // Declared before parameter destructuring so default initializers can
  // reference the name (`function f(x = f) {…}`).
  let e = case self_name {
    Some(fname) -> {
      let annexb_shadow =
        !child_strict && list.contains(collect_annexb_candidates(stmts), fname)
      let shadowed =
        set.contains(param_names, fname)
        || { uses_args && fname == "arguments" }
        || list.contains(collect_hoisted_vars(stmts), fname)
        || list.contains(direct_fn_names(stmts), fname)
        || list.any(collect_top_lex_names(stmts), fn(lex) { lex.0 == fname })
        || annexb_shadow
      case shadowed {
        True -> e
        False -> {
          let e = emit_op(e, DeclareVar(fname, FnNameBinding))
          let e = get_lexical(e, opcode.RefActiveFunc)
          emit_ir(e, IrScopeInitVar(fname))
        }
      }
    }
    None -> e
  }

  // Phase 2: Emit destructuring for non-identifier params.
  // While emitting parameter initializers, record the parameter-scope
  // binding names (parameters + implicit `arguments` for non-arrows) so a
  // direct eval inside a default expression can perform the
  // EvalDeclarationInstantiation §19.2.1.1 step 3.d conflict check: sloppy
  // direct eval in a parameter initializer throws a SyntaxError when it
  // var-declares a name already bound in the parameter scope. Arrows have
  // no own `arguments` binding (argumentsObjectNeeded is false), so only
  // their parameter names participate.
  let declared_param_names = list.flat_map(params, collect_pattern_names)
  let param_scope_names = case is_arrow {
    True -> declared_param_names
    False -> ["arguments", ..declared_param_names]
  }
  let e = Emitter(..e, param_scope_names:)
  // §10.2.11 step 21: non-simple lists create every parameter name as an
  // uninitialized (TDZ) binding up front; the fold below then initializes
  // them left to right, so `(x = x)` / `(x = y, y)` defaults throw a
  // ReferenceError when they read a not-yet-initialized parameter.
  let e = case non_simple_fixed {
    True ->
      list.fold(declared_param_names, e, fn(e, pname) {
        emit_op(e, DeclareVar(pname, LetBinding))
      })
    False -> e
  }
  let e =
    list.fold(destructured_params, e, fn(e, dp) {
      let #(synthetic, pattern) = dp
      let e = emit_ir(e, IrScopeGetVar(synthetic))
      emit_destructuring_bind(e, pattern, LetBinding) |> result.unwrap(e)
    })

  // Phase 2b: Bind the trailing rest parameter, if any. Build the array from
  // the args at `arity` and beyond, then bind it (an identifier, or a nested
  // destructuring target like `...[a, b]`). ParamBinding declares the slot.
  let e = case rest_param {
    None -> e
    Some(rest_target) -> {
      let e = emit_ir(e, IrCreateRestArray(arity))
      // Non-simple lists pre-declared the rest name(s) as TDZ lets above, so
      // bind via LetBinding (declare no-ops, init initializes). Simple-with-
      // rest lists keep the original ParamBinding path.
      let rest_kind = case non_simple_fixed {
        True -> LetBinding
        False -> ParamBinding
      }
      emit_destructuring_bind(e, rest_target, rest_kind) |> result.unwrap(e)
    }
  }

  // Parameter initialization is done — evals in the body proper run with
  // the body's VariableEnvironment, so the param-scope conflict check no
  // longer applies.
  let e = Emitter(..e, param_scope_names: [])

  // Hoisting for the function body. Function declaration names at body top
  // level are var-scoped; in sloppy mode, Annex B function-in-block names
  // also get a var binding (initialized undefined) unless blocked by a
  // lexical declaration on the path to their block.
  let annexb_names = case child_strict {
    True -> []
    False ->
      collect_annexb_candidates(stmts)
      |> list.filter(fn(n) { !set.contains(param_names, n) })
  }
  let hoisted_vars =
    list.flatten([
      collect_hoisted_vars(stmts),
      direct_fn_names(stmts),
      annexb_names,
    ])
    |> list.unique
  let lex_names = collect_top_lex_names(stmts)
  let #(e, hoisted_funcs) = collect_hoisted_funcs(e, stmts)

  let e =
    list.fold(hoisted_vars, e, fn(e, vname) {
      emit_op(e, DeclareVar(vname, VarBinding))
    })

  // Declare top-level let/const slots before hoisted-func MakeClosure so that
  // captured variables are boxed by the time the closure reads them. The
  // actual initializer still runs at the statement's position, so TDZ holds.
  let e =
    list.fold(lex_names, e, fn(e, lex) {
      let #(name, kind) = lex
      emit_op(e, DeclareVar(name, kind))
    })

  let e =
    list.fold(hoisted_funcs, e, fn(e, hf) {
      let #(fname, func_idx) = hf
      let e = emit_ir(e, IrMakeClosure(func_idx))
      let e = emit_ir(e, IrScopePutVar(fname))
      e
    })

  // For generators, emit IrInitialYield after parameter setup and hoisting,
  // but before the function body. This suspends execution so the generator
  // returns the iterator object (caller must call .next() to start).
  // Async functions do NOT get InitialYield — they run eagerly until the first
  // await or completion.
  let e = case is_generator {
    True -> emit_ir(e, IrInitialYield)
    False -> e
  }

  // Base-class ctor with instance fields: §10.2.2 [[Construct]] step 6 calls
  // InitializeInstanceElements before evaluating the body. Constructors can't
  // be generators/async, so this is always after the lexical declares with
  // `this` already bound by the caller.
  let e = case field_init {
    FieldInitAtStart -> emit_field_init_call(e)
    NoFieldInit | FieldInitAfterSuper -> e
  }

  // Emit body statements (for MVP, compilation errors in function bodies are ignored)
  let e = emit_stmts(e, stmts) |> result.unwrap(e)

  // Implicit return undefined at end
  let e = push_const(e, JsUndefined)
  let e = emit_ir(e, IrReturn)

  let e = emit_op(e, LeaveScope)
  let #(code, constants, constants_map, children) = finish(e)

  CompiledChild(
    name:,
    arity:,
    length: expected_length,
    code:,
    constants:,
    constants_map:,
    functions: children,
    is_strict: child_strict,
    is_arrow:,
    is_derived_constructor: False,
    is_generator:,
    is_async:,
    is_constructor:,
    is_class_constructor: False,
    has_eval_call: e.has_eval_call,
    lexical_refs: e.lexical_refs,
    syntax_perms:,
    with_stack: parent.with_stack,
  )
}

// §13.2.5.5: Some(n) only for a NAMED function expression — binds `n` in
// the function's own scope to the closure itself (immutable). None for
// declarations, methods, arrows, and NamedEvaluation-baked names.
// ============================================================================
// Statement emission
// ============================================================================

fn emit_stmts(
  e: Emitter,
  stmts: List(ast.StmtWithLine),
) -> Result(Emitter, EmitError) {
  list.try_fold(stmts, e, fn(e, located) {
    emit_stmt(set_line(e, located.line), located.statement)
  })
}

fn emit_stmt(e: Emitter, stmt: ast.Statement) -> Result(Emitter, EmitError) {
  case stmt {
    // Empty block: compiles to nothing (scope elision, see emit_block) —
    // skip the desugar probe and dispatch entirely.
    ast.BlockStatement([]) -> Ok(e)
    // for / for-of statements whose head declares using bindings are
    // rewritten into block + using-declaration form first (they may sit in
    // statement lists or single-statement positions, possibly under labels).
    _ ->
      case using_desugar.rewrite_for_using(stmt) {
        option.Some(rewritten) -> emit_stmt(e, rewritten)
        option.None -> emit_stmt_inner(e, stmt)
      }
  }
}

fn emit_stmt_inner(
  e: Emitter,
  stmt: ast.Statement,
) -> Result(Emitter, EmitError) {
  // Completion-value mode (see Emitter.completion_var): loops return their
  // own V (starting undefined) and if/switch UpdateEmpty with undefined, so
  // these statements reset the tracked V on entry. Statements with EMPTY
  // completions (declarations, empty, debugger, break/continue) leave it.
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
          let e = push_const(e, JsUndefined)
          emit_ir(e, IrScopePutVar(v))
        }
        _ -> e
      }
    None -> e
  }
  case stmt {
    ast.EmptyStatement | ast.DebuggerStatement -> Ok(e)

    // §7.3.32 DefineField — synthesized by inject_field_inits for class
    // instance fields. Uses CreateDataPropertyOrThrow ([[DefineOwnProperty]]),
    // not [[Set]], so prototype setters are NOT invoked and an own data
    // property is always created (even shadowing an inherited accessor).
    ast.ClassFieldInit(key:, value:, computed:) -> {
      let e = get_this(e)
      // §15.7.14: if Initializer is absent, initValue = undefined.
      let init = option.unwrap(value, ast.UndefinedExpression)
      use e <- result.map(case key, computed {
        // Class private element. The PrivateName key is read from the
        // class-scope const "#m" (see compile_class). A NUL-prefixed
        // identifier value is the synthetic install of an instance private
        // method/accessor (see private_method_init_stmts); anything else is
        // a field initializer (§7.3.28 PrivateFieldAdd).
        ast.Identifier("#" <> rest), False -> {
          let name = "#" <> rest
          let e = emit_ir(e, IrScopeGetVar(name))
          case init {
            ast.Identifier("\u{0}pg:" <> _ as hidden) -> {
              let e = emit_ir(e, IrScopeGetVar(hidden))
              Ok(emit_ir(e, IrDefinePrivateAccessor(opcode.Getter)))
            }
            ast.Identifier("\u{0}ps:" <> _ as hidden) -> {
              let e = emit_ir(e, IrScopeGetVar(hidden))
              Ok(emit_ir(e, IrDefinePrivateAccessor(opcode.Setter)))
            }
            ast.Identifier("\u{0}pm:" <> _ as hidden) -> {
              let e = emit_ir(e, IrScopeGetVar(hidden))
              Ok(emit_ir(e, IrDefinePrivateMethod))
            }
            _ -> {
              use e <- result.map(emit_named_expr(e, init, name))
              emit_ir(e, IrDefinePrivateField)
            }
          }
        }
        ast.Identifier(name), False | ast.StringExpression(name), False -> {
          // §7.3.33 DefineField step 7: anonymous function initializers get
          // the field name (NamedEvaluation).
          use e <- result.map(emit_named_expr(e, init, name))
          emit_ir(e, IrDefineField(name))
        }
        ast.NumberLiteral(n), False -> {
          let e = push_const(e, JsNumber(Finite(n)))
          use e <- result.map(emit_expr(e, init))
          emit_ir(e, IrDefineFieldComputed)
        }
        // Computed field name, already evaluated + ToPropertyKey'd at
        // class-definition time into a hidden class-scope const (see
        // compile_class / stash_computed_element_keys). Read the stashed key —
        // never re-evaluate the name expression (§15.7.14 step 27).
        ast.Identifier("\u{0}ck:" <> _ as hidden), _ -> {
          let e = emit_ir(e, IrScopeGetVar(hidden))
          use e <- result.map(emit_expr(e, init))
          emit_ir(e, IrDefineFieldComputed)
        }
        _, _ -> {
          // Exotic non-computed key (shouldn't occur: computed keys are
          // rewritten to stash-const reads above) — evaluate inline.
          use e <- result.try(emit_expr(e, key))
          use e <- result.map(emit_expr(e, init))
          emit_ir(e, IrDefineFieldComputed)
        }
      })
      emit_ir(e, IrPop)
    }

    ast.ExpressionStatement(expression: expr, ..) -> {
      use e <- result.map(emit_expr(e, expr))
      // Completion-value mode: the statement's value becomes the tracked V
      // (IrScopePutVar pops, so stack balance matches the IrPop path).
      case e.completion_var {
        Some(v) -> emit_ir(e, IrScopePutVar(v))
        None -> emit_ir(e, IrPop)
      }
    }

    ast.BlockStatement(body) -> emit_block(e, body, tail: False)

    ast.VariableDeclaration(kind, declarators) -> {
      // Using/AwaitUsing never reach here — the using desugar (run on every
      // statement list before emission) lowers them to Const. The arms below
      // treat them as Const defensively.
      let binding_kind = case kind {
        ast.Var -> VarBinding
        ast.Let -> LetBinding
        ast.Const | ast.Using | ast.AwaitUsing -> ConstBinding
      }
      list.try_fold(declarators, e, fn(e, decl) {
        case decl {
          ast.VariableDeclarator(ast.IdentifierPattern(name), init) -> {
            // For let/const, emit declaration marker (var already hoisted)
            let e = case kind {
              ast.Let -> declare_lex(e, name, False)
              ast.Const | ast.Using | ast.AwaitUsing ->
                declare_lex(e, name, True)
              ast.Var -> e
            }
            case init {
              Some(init_expr) -> {
                use e <- result.map(emit_named_expr(e, init_expr, name))
                case kind {
                  ast.Var -> emit_ir(e, IrScopePutVar(name))
                  ast.Let | ast.Const | ast.Using | ast.AwaitUsing ->
                    init_lex(e, name)
                }
              }
              // `let x;` (no initializer) initializes the binding to undefined
              // (§14.3.1.2). Without this the slot stays in TDZ forever —
              // observable now that module exports read live (e.g. `export let
              // x;` then accessing it through the namespace). `var` is hoisted
              // to undefined already; only lexical bindings need this.
              None ->
                case kind != ast.Var {
                  True -> Ok(init_lex(push_const(e, JsUndefined), name))
                  False -> Ok(e)
                }
            }
          }
          // Destructuring patterns
          ast.VariableDeclarator(pattern, init) -> {
            use e <- result.try(case init {
              Some(init_expr) -> emit_expr(e, init_expr)
              None -> Ok(push_const(e, JsUndefined))
            })
            emit_destructuring_bind(e, pattern, binding_kind)
          }
        }
      })
    }

    ast.IfStatement(condition, consequent, alternate) -> {
      let #(e, else_label) = fresh_label(e)
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, condition))
      let e = emit_ir(e, IrJumpIfFalse(else_label))
      use e <- result.try(emit_stmt(e, block_wrap_fn_decl(consequent)))
      let e = emit_ir(e, IrJump(end_label))
      let e = emit_ir(e, IrLabel(else_label))
      let e = case alternate {
        Some(alt) -> emit_stmt(e, block_wrap_fn_decl(alt)) |> result.unwrap(e)
        None -> e
      }
      let e = emit_ir(e, IrLabel(end_label))
      Ok(e)
    }

    ast.WhileStatement(condition, body) -> {
      let #(e, loop_start) = fresh_label(e)
      let #(e, loop_end) = fresh_label(e)
      let e = push_loop(e, loop_end, loop_start)
      let e = emit_ir(e, IrLabel(loop_start))
      use e <- result.try(emit_expr(e, condition))
      let e = emit_ir(e, IrJumpIfFalse(loop_end))
      use e <- result.try(emit_stmt(e, body))
      let e = emit_ir(e, IrJump(loop_start))
      let e = emit_ir(e, IrLabel(loop_end))
      let e = pop_loop(e)
      Ok(e)
    }

    ast.DoWhileStatement(condition, body) -> {
      let #(e, loop_start) = fresh_label(e)
      let #(e, loop_cond) = fresh_label(e)
      let #(e, loop_end) = fresh_label(e)
      let e = push_loop(e, loop_end, loop_cond)
      let e = emit_ir(e, IrLabel(loop_start))
      use e <- result.try(emit_stmt(e, body))
      let e = emit_ir(e, IrLabel(loop_cond))
      use e <- result.try(emit_expr(e, condition))
      let e = emit_ir(e, IrJumpIfTrue(loop_start))
      let e = emit_ir(e, IrLabel(loop_end))
      let e = pop_loop(e)
      Ok(e)
    }

    ast.ForStatement(init, condition, update, body) -> {
      let #(e, loop_start) = fresh_label(e)
      let #(e, loop_continue) = fresh_label(e)
      let #(e, loop_end) = fresh_label(e)

      let e = emit_op(e, EnterScope(BlockScope))

      let #(e, per_iter) = case init {
        Some(ast.ForInitExpression(expr)) -> #(
          emit_expr(e, expr)
            |> result.map(emit_ir(_, IrPop))
            |> result.unwrap(e),
          [],
        )
        Some(ast.ForInitDeclaration(decl)) -> #(
          emit_stmt(e, decl) |> result.unwrap(e),
          for_let_names(decl),
        )
        _ -> #(e, [])
      }

      // for-head let/const names form a lexical scope around the body —
      // they block Annex B promotion of same-named block fns inside it.
      let saved_annexb = e
      let e = case init {
        Some(ast.ForInitDeclaration(decl)) ->
          annexb_head_ctx(e, for_head_lex_names(decl))
        _ -> e
      }

      let e = push_loop(e, loop_end, loop_continue)
      let e = emit_ir(e, IrLabel(loop_start))

      let e = case condition {
        Some(cond) ->
          emit_expr(e, cond)
          |> result.map(emit_ir(_, IrJumpIfFalse(loop_end)))
          |> result.unwrap(e)
        None -> e
      }

      let e = emit_stmt(e, body) |> result.unwrap(e)

      let e = emit_ir(e, IrLabel(loop_continue))
      let e =
        list.fold(per_iter, e, fn(e, n) { emit_ir(e, IrScopeReboxVar(n)) })

      let e = case update {
        Some(upd) ->
          emit_expr(e, upd) |> result.map(emit_ir(_, IrPop)) |> result.unwrap(e)
        None -> e
      }

      let e = emit_ir(e, IrJump(loop_start))
      let e = emit_ir(e, IrLabel(loop_end))
      let e = pop_loop(e)
      let e = restore_annexb_ctx(e, saved_annexb)
      let e = emit_op(e, LeaveScope)
      Ok(e)
    }

    ast.ReturnStatement(arg) -> {
      use e <- result.try(case arg {
        Some(expr) -> emit_expr(e, expr)
        None -> Ok(push_const(e, JsUndefined))
      })
      // retval on top throughout. PopTry doesn't touch value stack. drop_count
      // slots sit *under* retval → nip. has_iterator: iter under retval → swap+close.
      // label_finally: retval IS the gosub slot (QuickJS emit_return quickjs.c:27876).
      let e =
        list.fold(e.loop_stack, e, fn(e, ctx) {
          let e = repeat_ir(e, IrPopTry, ctx.cross_pop_try)
          let e = repeat_nip(e, ctx.drop_count)
          let e = case ctx.has_iterator {
            True -> e |> emit_ir(IrSwap) |> emit_ir(IrIteratorClose)
            False -> e
          }
          case ctx.label_finally {
            Some(lbl) -> emit_ir(e, IrGosub(lbl))
            None -> e
          }
        })
      Ok(emit_ir(e, IrReturn))
    }

    ast.ThrowStatement(arg) -> {
      use e <- result.map(emit_expr(e, arg))
      emit_ir(e, IrThrow)
    }

    ast.TryStatement(block, handler, finalizer) -> {
      case handler, finalizer {
        // try/catch (no finally)
        Some(ast.CatchClause(param, catch_body)), None -> {
          let #(e, catch_label) = fresh_label(e)
          let #(e, end_label) = fresh_label(e)

          let e = emit_ir(e, IrPushTry(catch_label))
          let e = push_barrier(e, pop_try: 1, label_finally: None, drop: 0)
          use e <- result.try(emit_stmt(e, block))
          let e = pop_loop(e)
          let e = emit_ir(e, IrPopTry)
          let e = emit_ir(e, IrJump(end_label))

          let e = emit_ir(e, IrLabel(catch_label))
          let e = emit_op(e, EnterScope(BlockScope))
          let e = case param {
            Some(pattern) ->
              emit_destructuring_bind(e, pattern, CatchBinding)
              |> result.unwrap(e)
            None -> emit_ir(e, IrPop)
          }

          let saved = e
          let e = catch_annexb_ctx(e, param)
          use e <- result.try(emit_stmt(e, catch_body))
          let e = restore_annexb_ctx(e, saved)
          let e = emit_op(e, LeaveScope)
          let e = emit_ir(e, IrLabel(end_label))
          Ok(e)
        }

        // try/finally (no catch). QuickJS js_parse_try TOK_FINALLY-only path
        // (quickjs.c:28917-28922 + 28926-28962).
        None, Some(finally_body) -> {
          let #(e, throw_label) = fresh_label(e)
          let #(e, fin_label) = fresh_label(e)
          let #(e, end_label) = fresh_label(e)

          // -- try body --------------------------------------------------
          let e = emit_ir(e, IrPushTry(throw_label))
          let e =
            push_barrier(e, pop_try: 1, label_finally: Some(fin_label), drop: 0)
          use e <- result.try(emit_stmt(e, block))
          let e = pop_loop(e)
          let e = emit_ir(e, IrPopTry)
          let e = emit_gosub_normal(e, fin_label)
          let e = emit_ir(e, IrJump(end_label))

          // -- throw entry: thrown value is the gosub slot ---------------
          // unwind_to_catch leaves stack = [thrown, ..base], try_stack popped.
          let e = emit_ir(e, IrLabel(throw_label))
          let e = emit_ir(e, IrGosub(fin_label))
          let e = emit_ir(e, IrThrow)

          // -- finally subroutine ----------------------------------------
          // Entry stack: [retpc, slot, ..base] for ALL callers.
          let e = emit_ir(e, IrLabel(fin_label))
          let e = push_barrier(e, pop_try: 0, label_finally: None, drop: 2)
          // §14.15.3: a normally-completing Finally never supplies the
          // completion value — keep it out of completion-value mode.
          let saved_cv = e.completion_var
          let e = Emitter(..e, completion_var: None)
          use e <- result.try(emit_stmt(e, finally_body))
          let e = Emitter(..e, completion_var: saved_cv)
          let e = pop_loop(e)
          let e = emit_ir(e, IrRet)

          let e = emit_ir(e, IrLabel(end_label))
          Ok(e)
        }

        // try/catch/finally. QuickJS js_parse_try with-catch path
        // (quickjs.c:28824-28912 + 28926-28962). Arc keeps the existing
        // two-PushTry-upfront structure (vs QuickJS's catch2-inside-handler) so
        // throws during catch-param destructuring are also wrapped by finally.
        Some(ast.CatchClause(param, catch_body)), Some(finally_body) -> {
          let #(e, throw_label) = fresh_label(e)
          let #(e, catch_label) = fresh_label(e)
          let #(e, fin_label) = fresh_label(e)
          let #(e, end_label) = fresh_label(e)

          // -- try body --------------------------------------------------
          // Outer: catches throws from catch body (and catch-param binding).
          let e = emit_ir(e, IrPushTry(throw_label))
          // Inner: catches throws from try body → catch handler.
          let e = emit_ir(e, IrPushTry(catch_label))
          let e =
            push_barrier(e, pop_try: 2, label_finally: Some(fin_label), drop: 0)
          use e <- result.try(emit_stmt(e, block))
          let e = pop_loop(e)
          let e = emit_ir(e, IrPopTry)
          let e = emit_ir(e, IrPopTry)
          let e = emit_gosub_normal(e, fin_label)
          let e = emit_ir(e, IrJump(end_label))

          // -- catch handler ---------------------------------------------
          // unwind popped inner try; outer (throw_label) still on try_stack.
          // stack = [thrown, ..base].
          let e = emit_ir(e, IrLabel(catch_label))
          let e = emit_op(e, EnterScope(BlockScope))
          let e = case param {
            Some(pattern) ->
              emit_destructuring_bind(e, pattern, CatchBinding)
              |> result.unwrap(e)
            None -> emit_ir(e, IrPop)
          }
          let saved = e
          let e = catch_annexb_ctx(e, param)
          let e =
            push_barrier(e, pop_try: 1, label_finally: Some(fin_label), drop: 0)
          use e <- result.try(emit_stmt(e, catch_body))
          let e = pop_loop(e)
          let e = restore_annexb_ctx(e, saved)
          let e = emit_op(e, LeaveScope)
          let e = emit_ir(e, IrPopTry)
          let e = emit_gosub_normal(e, fin_label)
          let e = emit_ir(e, IrJump(end_label))

          // -- throw entry (catch body threw) ----------------------------
          // unwind popped outer try; stack = [thrown, ..base].
          let e = emit_ir(e, IrLabel(throw_label))
          let e = emit_ir(e, IrGosub(fin_label))
          let e = emit_ir(e, IrThrow)

          // -- finally subroutine ----------------------------------------
          let e = emit_ir(e, IrLabel(fin_label))
          let e = push_barrier(e, pop_try: 0, label_finally: None, drop: 2)
          // §14.15.3: a normally-completing Finally never supplies the
          // completion value — keep it out of completion-value mode.
          let saved_cv = e.completion_var
          let e = Emitter(..e, completion_var: None)
          use e <- result.try(emit_stmt(e, finally_body))
          let e = Emitter(..e, completion_var: saved_cv)
          let e = pop_loop(e)
          let e = emit_ir(e, IrRet)

          let e = emit_ir(e, IrLabel(end_label))
          Ok(e)
        }

        // try with neither catch nor finally (shouldn't happen per spec, but handle gracefully)
        None, None -> emit_stmt(e, block)
      }
    }

    ast.SwitchStatement(discriminant, cases) -> {
      emit_switch(e, discriminant, cases)
    }

    ast.BreakStatement(name) -> emit_goto_loop(e, name, False)

    ast.ContinueStatement(name) -> emit_goto_loop(e, name, True)

    ast.LabeledStatement(label, body) -> {
      case body {
        // Labeled loop: set pending_label so the loop picks it up
        ast.WhileStatement(..)
        | ast.DoWhileStatement(..)
        | ast.ForStatement(..)
        | ast.ForInStatement(..)
        | ast.ForOfStatement(..) -> {
          let e = Emitter(..e, pending_label: Some(label))
          emit_stmt(e, body)
        }
        // Labeled non-loop: create a break-only target
        _ -> {
          let #(e, break_target) = fresh_label(e)
          let e =
            Emitter(..e, loop_stack: [
              LoopContext(
                break_label: break_target,
                continue_label: -1,
                label: Some(label),
                is_regular: True,
                cross_pop_try: 0,
                has_iterator: False,
                label_finally: None,
                drop_count: 0,
              ),
              ..e.loop_stack
            ])
          use e <- result.map(emit_stmt(e, body))
          let e = pop_loop(e)
          emit_ir(e, IrLabel(break_target))
        }
      }
    }

    ast.FunctionDeclaration(name, _, _, is_generator, is_async) -> {
      // The closure itself was instantiated during hoisting (function/program
      // top level) or BlockDeclarationInstantiation (block level). The only
      // runtime effect at the statement's position is Annex B §B.3.2.6:
      // in sloppy mode, evaluating a block-level FunctionDeclaration copies
      // the block binding into the enclosing var-scope binding — unless an
      // intermediate lexical declaration makes the name unpromotable.
      // §B.3.2/§B.3.3 cover plain functions only — generators, async
      // functions, and async generators are never promoted.
      case name {
        Some(fname) -> {
          let promote =
            e.in_block
            && !e.strict
            && !is_generator
            && !is_async
            && !set.contains(e.annexb_blocked, fname)
          case promote {
            True -> Ok(emit_op(e, AnnexBPromote(fname)))
            False -> Ok(e)
          }
        }
        None -> Ok(e)
      }
    }

    ast.ClassDeclaration(name, super_class, body) -> {
      case name {
        Some(n) -> {
          // Class names are block-scoped (like let)
          let e = declare_lex(e, n, False)
          use e <- result.map(compile_class(e, name, name, super_class, body))
          // compile_class leaves [ctor] on stack; init pops it
          init_lex(e, n)
        }
        None -> Error(Unsupported("anonymous class declaration"))
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

/// §14.11 WithStatement (sloppy mode only — the parser rejects `with` in
/// strict code). The ToObject'd head expression is stored in a synthetic
/// block-scoped local; EnterWith/LeaveWith mark the region whose name
/// resolutions must check that object first (Phase 2 emits the checks).
/// The synthetic name uses `<` so it can never collide with user code.
/// It carries BOTH the current with-depth (unique along any lexical chain —
/// ancestor withs are visible to nested functions and depth strictly grows
/// down the chain) and a per-body serial (unique across SIBLING withs in the
/// same function, whose depth is equal). Sibling uniqueness matters for the
/// direct-eval name table (Resolved.names is name-keyed, first declaration
/// wins): two same-named siblings would make eval inside the second with
/// read the first with's object.
fn emit_with(
  e: Emitter,
  object: ast.Expression,
  body: ast.Statement,
  tail tail: Bool,
) -> Result(Emitter, EmitError) {
  use e <- result.try(emit_expr(e, object))
  let e = emit_ir(e, opcode.IrToObject)
  let #(e, uid) = fresh_label(e)
  let synth =
    "<with"
    <> int.to_string(list.length(e.with_stack))
    <> "_"
    <> int.to_string(uid)
    <> ">"
  let e = emit_op(e, EnterScope(BlockScope))
  let e = emit_op(e, DeclareVar(synth, LetBinding))
  let e = emit_ir(e, IrScopeInitVar(synth))
  let e = emit_op(e, EnterWith(synth))
  let e = Emitter(..e, with_stack: [synth, ..e.with_stack])
  use e <- result.map(case tail {
    True -> emit_stmt_tail(e, body)
    False -> emit_stmt(e, body)
  })
  let e = case e.with_stack {
    [_, ..rest] -> Emitter(..e, with_stack: rest)
    [] -> e
  }
  let e = emit_op(e, LeaveWith)
  emit_op(e, LeaveScope)
}

// ============================================================================
// Expression emission
// ============================================================================

/// Strip ParenthesizedExpression wrappers (possibly nested).
/// Used to look through parens when the spec says they're transparent.
fn unwrap_parens(expr: ast.Expression) -> ast.Expression {
  case expr {
    ast.ParenthesizedExpression(inner) -> unwrap_parens(inner)
    _ -> expr
  }
}

/// §13.3.9 OptionalExpression: does this member/call chain contain a `?.`
/// link? Recursion stops at any non-chain node — in particular a
/// ParenthesizedExpression is a chain BOUNDARY: `(a?.b).c` must NOT
/// short-circuit the outer `.c`.
fn chain_has_optional(expr: ast.Expression) -> Bool {
  case expr {
    ast.OptionalMemberExpression(..) | ast.OptionalCallExpression(..) -> True
    ast.MemberExpression(object:, ..) -> chain_has_optional(object)
    ast.CallExpression(callee:, ..) -> chain_has_optional(callee)
    _ -> False
  }
}

/// Compile a whole optional chain with ONE shared short-circuit exit
/// (§13.3.9.1: a nullish base at any `?.` link makes the ENTIRE chain —
/// including later non-optional links and call arguments — evaluate to
/// undefined). Two cleanup blocks because the jump sites leave different
/// stack shapes: `l1` sites have just the nullish base on top; `l2` sites
/// (optional CALL of a method reference) have [f, receiver].
fn emit_chain_root(
  e: Emitter,
  expr: ast.Expression,
) -> Result(Emitter, EmitError) {
  let #(e, l1) = fresh_label(e)
  let #(e, l2) = fresh_label(e)
  let #(e, end_label) = fresh_label(e)
  use e <- result.map(emit_chain(e, expr, l1, l2))
  let e = emit_ir(e, IrJump(end_label))
  // Depth-1 cleanup: [nullish] → [undefined]
  let e = emit_ir(e, IrLabel(l1))
  let e = emit_ir(e, IrPop)
  let e = push_const(e, JsUndefined)
  let e = emit_ir(e, IrJump(end_label))
  // Depth-2 cleanup: [f, receiver] → [undefined]
  let e = emit_ir(e, IrLabel(l2))
  let e = emit_ir(e, IrPop)
  let e = emit_ir(e, IrPop)
  let e = push_const(e, JsUndefined)
  emit_ir(e, IrLabel(end_label))
}

/// Emit one link of an optional chain. `l1`/`l2` are the shared cleanup
/// labels from emit_chain_root. Sub-expressions without any `?.` link fall
/// back to ordinary emission (covering all the special-cased forms — super,
/// direct eval, `with` callee handling).
fn emit_chain(
  e: Emitter,
  expr: ast.Expression,
  l1: Int,
  l2: Int,
) -> Result(Emitter, EmitError) {
  use <- bool.lazy_guard(!chain_has_optional(expr), fn() { emit_expr(e, expr) })
  case expr {
    ast.OptionalMemberExpression(object, ast.Identifier(prop), False) -> {
      use e <- result.map(emit_chain(e, object, l1, l2))
      e
      |> emit_ir(IrDup)
      |> emit_ir(IrJumpIfNullish(l1))
      |> emit_get_field(prop)
    }
    ast.OptionalMemberExpression(object, property, True) -> {
      use e <- result.try(emit_chain(e, object, l1, l2))
      let e = e |> emit_ir(IrDup) |> emit_ir(IrJumpIfNullish(l1))
      use e <- result.map(emit_expr(e, property))
      emit_ir(e, IrGetElem)
    }
    ast.MemberExpression(object, ast.Identifier(prop), False) -> {
      use e <- result.map(emit_chain(e, object, l1, l2))
      emit_get_field(e, prop)
    }
    ast.MemberExpression(object, property, True) -> {
      use e <- result.try(emit_chain(e, object, l1, l2))
      use e <- result.map(emit_expr(e, property))
      emit_ir(e, IrGetElem)
    }
    // Non-optional call AFTER an optional link, e.g. `a?.b.m(x)` /
    // `a?.b(x)` — `this` binding follows the method-reference shape.
    ast.CallExpression(callee, args) -> {
      use #(e, is_method) <- result.try(emit_chain_callee(e, callee, l1, l2))
      emit_chain_call_args(e, args, is_method)
    }
    // Optional call `f?.(x)`: additionally check the function value itself.
    ast.OptionalCallExpression(callee, args) -> {
      use #(e, is_method) <- result.try(emit_chain_callee(e, callee, l1, l2))
      let e = emit_ir(e, IrDup)
      let e = case is_method {
        True -> emit_ir(e, IrJumpIfNullish(l2))
        False -> emit_ir(e, IrJumpIfNullish(l1))
      }
      emit_chain_call_args(e, args, is_method)
    }
    // Unreachable: chain_has_optional only returns True for the arms above.
    other -> emit_expr(e, other)
  }
}

/// Emit a chain call's callee. Returns is_method: True when the stack is
/// left as [f, receiver] (CallMethod shape, `this` = receiver), False when
/// it's [f] (plain Call, `this` = undefined).
fn emit_chain_callee(
  e: Emitter,
  callee: ast.Expression,
  l1: Int,
  l2: Int,
) -> Result(#(Emitter, Bool), EmitError) {
  case callee {
    // super.m?.() — §13.3.7.3 super reference with lexical-this receiver.
    ast.MemberExpression(ast.SuperExpression, ast.Identifier(m), False) -> {
      let e =
        emit_super_base_keep_recv(e)
        |> push_const(JsString(m))
        |> emit_ir(IrGetSuperValue)
      Ok(#(e, True))
    }
    ast.MemberExpression(ast.SuperExpression, key, True) -> {
      let e = emit_super_base_keep_recv(e)
      use e <- result.map(emit_expr(e, key))
      #(emit_ir(e, IrGetSuperValue), True)
    }
    ast.MemberExpression(obj, ast.Identifier(m), False) -> {
      use e <- result.map(emit_chain(e, obj, l1, l2))
      #(emit_get_field2(e, m), True)
    }
    ast.MemberExpression(obj, key, True) -> {
      use e <- result.try(emit_chain(e, obj, l1, l2))
      use e <- result.map(emit_expr(e, key))
      // [f, key, receiver] → [f, receiver]
      let e = e |> emit_ir(IrGetElem2) |> emit_ir(IrSwap) |> emit_ir(IrPop)
      #(e, True)
    }
    ast.OptionalMemberExpression(obj, ast.Identifier(m), False) -> {
      use e <- result.map(emit_chain(e, obj, l1, l2))
      let e = e |> emit_ir(IrDup) |> emit_ir(IrJumpIfNullish(l1))
      #(emit_get_field2(e, m), True)
    }
    ast.OptionalMemberExpression(obj, key, True) -> {
      use e <- result.try(emit_chain(e, obj, l1, l2))
      let e = e |> emit_ir(IrDup) |> emit_ir(IrJumpIfNullish(l1))
      use e <- result.map(emit_expr(e, key))
      let e = e |> emit_ir(IrGetElem2) |> emit_ir(IrSwap) |> emit_ir(IrPop)
      #(e, True)
    }
    other -> {
      use e <- result.map(emit_chain(e, other, l1, l2))
      #(e, False)
    }
  }
}

/// Emit a chain call's arguments + the call op for the callee shape left by
/// emit_chain_callee.
fn emit_chain_call_args(
  e: Emitter,
  args: List(ast.Expression),
  is_method: Bool,
) -> Result(Emitter, EmitError) {
  case has_spread_arg(args), is_method {
    False, True -> {
      use e <- result.map(list.try_fold(args, e, emit_expr))
      emit_ir(e, IrCallMethod("[chain]", list.length(args)))
    }
    False, False -> {
      use e <- result.map(list.try_fold(args, e, emit_expr))
      emit_ir(e, opcode.IrCall(list.length(args)))
    }
    True, True -> {
      use e <- result.map(emit_args_array_with_spread(e, args))
      emit_ir(e, IrCallMethodApply)
    }
    True, False -> {
      use e <- result.map(emit_args_array_with_spread(e, args))
      emit_ir(e, IrCallApply)
    }
  }
}

fn emit_expr(e: Emitter, expr: ast.Expression) -> Result(Emitter, EmitError) {
  case expr {
    // Literals
    ast.NumberLiteral(value) -> Ok(push_const(e, JsNumber(Finite(value))))
    ast.BigIntLiteral(value: n) ->
      Ok(push_const(e, value.JsBigInt(value.BigInt(n))))
    ast.StringExpression(value) -> Ok(push_const(e, JsString(value)))
    ast.BooleanLiteral(value) -> Ok(push_const(e, JsBool(value)))
    ast.NullLiteral -> Ok(push_const(e, JsNull))
    ast.UndefinedExpression -> Ok(push_const(e, JsUndefined))

    // Identifier
    ast.Identifier("undefined") -> Ok(push_const(e, JsUndefined))
    // Bare PrivateIdentifier outside `#x in obj` — early error per §13.10.1.
    // The `#x in obj` BinaryExpression arm below does NOT recurse on its LHS,
    // so this only catches genuinely-bare `#x` used as a value.
    ast.Identifier("#" <> rest) ->
      Error(Unsupported("Unexpected private name #" <> rest))
    ast.Identifier(name) -> Ok(emit_ir(e, IrScopeGetVar(name)))

    // Binary expressions
    // §13.10.1 RelationalExpression : PrivateIdentifier `in` ShiftExpression.
    // LHS is a name, not a value — emit only RHS, then PrivateIn(name).
    // Stack: [obj] → [bool].
    ast.BinaryExpression(ast.In, ast.Identifier("#" <> rest), right) -> {
      use e <- result.map(emit_expr(e, right))
      e
      |> emit_ir(IrScopeGetVar("#" <> rest))
      |> emit_ir(IrPrivateInDyn)
    }
    ast.BinaryExpression(op, left, right) -> {
      use e <- result.try(emit_expr(e, left))
      use e <- result.map(emit_expr(e, right))
      emit_ir(e, IrBinOp(translate_binop(op)))
    }

    // Logical expressions (short-circuit)
    ast.LogicalExpression(ast.LogicalAnd, left, right) -> {
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, left))
      let e = emit_ir(e, IrDup)
      let e = emit_ir(e, IrJumpIfFalse(end_label))
      let e = emit_ir(e, IrPop)
      use e <- result.map(emit_expr(e, right))
      emit_ir(e, IrLabel(end_label))
    }

    ast.LogicalExpression(ast.LogicalOr, left, right) -> {
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, left))
      let e = emit_ir(e, IrDup)
      let e = emit_ir(e, IrJumpIfTrue(end_label))
      let e = emit_ir(e, IrPop)
      use e <- result.map(emit_expr(e, right))
      emit_ir(e, IrLabel(end_label))
    }

    ast.LogicalExpression(ast.NullishCoalescing, left, right) -> {
      let #(e, use_right_label) = fresh_label(e)
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, left))
      let e = emit_ir(e, IrDup)
      let e = emit_ir(e, IrJumpIfNullish(use_right_label))
      let e = emit_ir(e, IrJump(end_label))
      let e = emit_ir(e, IrLabel(use_right_label))
      let e = emit_ir(e, IrPop)
      use e <- result.map(emit_expr(e, right))
      emit_ir(e, IrLabel(end_label))
    }

    // Other logical ops are just binary ops
    ast.LogicalExpression(op, left, right) -> {
      use e <- result.try(emit_expr(e, left))
      use e <- result.map(emit_expr(e, right))
      emit_ir(e, IrBinOp(translate_binop(op)))
    }

    // Unary expressions
    // typeof uses unwrap_parens because typeof (x) === typeof x per spec.
    ast.UnaryExpression(ast.TypeOf, _, arg) ->
      case unwrap_parens(arg) {
        ast.Identifier(name) -> {
          // typeof x must NOT throw for undeclared variables
          Ok(emit_ir(e, IrScopeTypeofVar(name)))
        }
        _ -> {
          use e <- result.map(emit_expr(e, arg))
          emit_ir(e, IrTypeOf)
        }
      }

    // delete expression — uses unwrap_parens because delete (x) === delete x.
    ast.UnaryExpression(ast.Delete, _, arg) ->
      case unwrap_parens(arg) {
        // §13.5.1.2 step 5.b — delete on a super reference is an
        // unconditional ReferenceError. Evaluate `this` (TDZ check) and the
        // computed key for side effects per §13.3.7 ordering, then throw.
        ast.MemberExpression(ast.SuperExpression, key, computed) -> {
          let e = get_this(e) |> emit_ir(IrPop)
          use e <- result.map(case computed {
            True -> result.map(emit_expr(e, key), emit_ir(_, IrPop))
            False -> Ok(e)
          })
          emit_ir(
            e,
            IrThrowError(
              opcode.ReferenceErrorKind,
              "Unsupported reference to 'super'",
            ),
          )
        }
        ast.MemberExpression(obj, ast.Identifier(prop), False) -> {
          // delete obj.prop → emit obj, DeleteField(prop)
          use e <- result.map(emit_expr(e, obj))
          emit_ir(e, IrDeleteField(prop))
        }
        ast.MemberExpression(obj, key_expr, True) -> {
          // delete obj[key] → emit obj, emit key, DeleteElem
          use e <- result.try(emit_expr(e, obj))
          use e <- result.map(emit_expr(e, key_expr))
          emit_ir(e, IrDeleteElem)
        }
        ast.Identifier(name) -> {
          // delete x — Phase 2 emits enclosing-with object checks (§9.1.1.2.7
          // DeleteBinding deletes the property off the with object) and falls
          // back to `true` (can't delete plain vars; legacy behavior).
          Ok(emit_ir(e, opcode.IrScopeDeleteVar(name)))
        }
        _ -> {
          // delete <other expr> → evaluate for side effects, discard, push true
          use e <- result.map(emit_expr(e, arg))
          let e = emit_ir(e, IrPop)
          push_const(e, JsBool(True))
        }
      }

    ast.UnaryExpression(op, _, arg) -> {
      use e <- result.map(emit_expr(e, arg))
      emit_ir(e, IrUnaryOp(translate_unaryop(op)))
    }

    // Update expressions (++/--) — unwrap parens because (x)++ === x++.
    ast.UpdateExpression(op, prefix, ast.ParenthesizedExpression(inner)) ->
      emit_expr(e, ast.UpdateExpression(op, prefix, unwrap_parens(inner)))
    // Annex B web-compat: `++f()` / `f()--` parse in sloppy mode, evaluate
    // the call, then throw ReferenceError (before ToNumeric).
    ast.UpdateExpression(_, _, ast.CallExpression(callee, args)) -> {
      use e <- result.map(emit_expr(e, ast.CallExpression(callee, args)))
      let e = emit_ir(e, IrPop)
      emit_ir(
        e,
        IrThrowError(
          opcode.ReferenceErrorKind,
          "Invalid left-hand side expression in update operation",
        ),
      )
    }
    ast.UpdateExpression(op, prefix, ast.Identifier(name)) -> {
      let one = JsNumber(Finite(1.0))
      let bin_kind = case op {
        ast.Increment -> opcode.Add
        ast.Decrement -> opcode.Sub
      }
      case prefix {
        True -> {
          // ++x: make ref, get, add 1, dup (keep result), store to ref
          let e = emit_ir(e, opcode.IrScopeMakeRef(name))
          let e = emit_ir(e, opcode.IrScopeGetRef(name))
          let e = push_const(e, one)
          let e = emit_ir(e, IrBinOp(bin_kind))
          let e = emit_ir(e, IrDup)
          let e = emit_ir(e, opcode.IrScopePutRef(name))
          Ok(e)
        }
        False -> {
          // x++: make ref, get, ToNumeric (§13.4.2.1 step 3), dup (old value
          // stays as result), add 1, store to ref. Unary `+` is ToNumber.
          let e = emit_ir(e, opcode.IrScopeMakeRef(name))
          let e = emit_ir(e, opcode.IrScopeGetRef(name))
          let e = emit_ir(e, IrUnaryOp(opcode.Pos))
          let e = emit_ir(e, IrDup)
          let e = push_const(e, one)
          let e = emit_ir(e, IrBinOp(bin_kind))
          let e = emit_ir(e, opcode.IrScopePutRef(name))
          Ok(e)
        }
      }
    }
    // ++super.x / super[k]++ — GetSuperValue2 reads while keeping
    // [pk, base, this] underneath so PutSuperValue can write back without
    // re-evaluating GetSuperBase or ToPropertyKey (both observably stateful).
    // Postfix undoes ±1 after the store, same as the generic member arm.
    ast.UpdateExpression(
      op,
      prefix,
      ast.MemberExpression(ast.SuperExpression, key, computed),
    ) -> {
      let one = JsNumber(Finite(1.0))
      let #(bin_kind, undo) = case op {
        ast.Increment -> #(opcode.Add, opcode.Sub)
        ast.Decrement -> #(opcode.Sub, opcode.Add)
      }
      let e = emit_super_base(e)
      use e <- result.map(emit_super_key(e, key, computed))
      let e =
        emit_ir(e, IrGetSuperValue2)
        |> push_const(one)
        |> emit_ir(IrBinOp(bin_kind))
        |> emit_ir(IrPutSuperValue)
      case prefix {
        True -> e
        False -> push_const(e, one) |> emit_ir(IrBinOp(undo))
      }
    }
    // obj.prop++ / obj[key]++ — emit as prefix (clean stack protocol via
    // GetField2/PutField), then undo ±1 for postfix to recover old value.
    // Spec's ToNumeric coercion already happened in the Add/Sub, so new-1 = old.
    ast.UpdateExpression(
      op,
      prefix,
      ast.MemberExpression(obj, ast.Identifier(prop), False),
    ) -> {
      let one = JsNumber(Finite(1.0))
      let #(bin_kind, undo) = case op {
        ast.Increment -> #(opcode.Add, opcode.Sub)
        ast.Decrement -> #(opcode.Sub, opcode.Add)
      }
      use e <- result.map(emit_expr(e, obj))
      let e = emit_get_field2(e, prop)
      let e = push_const(e, one)
      let e = emit_ir(e, IrBinOp(bin_kind))
      let e = emit_put_field(e, prop)
      case prefix {
        True -> e
        False -> {
          let e = push_const(e, one)
          emit_ir(e, IrBinOp(undo))
        }
      }
    }
    ast.UpdateExpression(op, prefix, ast.MemberExpression(obj, key, True)) -> {
      let one = JsNumber(Finite(1.0))
      let #(bin_kind, undo) = case op {
        ast.Increment -> #(opcode.Add, opcode.Sub)
        ast.Decrement -> #(opcode.Sub, opcode.Add)
      }
      use e <- result.try(emit_expr(e, obj))
      use e <- result.map(emit_expr(e, key))
      let e = emit_ir(e, IrGetElem2)
      let e = push_const(e, one)
      let e = emit_ir(e, IrBinOp(bin_kind))
      let e = emit_ir(e, IrPutElem)
      case prefix {
        True -> e
        False -> {
          let e = push_const(e, one)
          emit_ir(e, IrBinOp(undo))
        }
      }
    }

    // Parenthesized LHS assignment — unwrap parens but skip name inference.
    // Per ES spec §13.15.2: IsIdentifierRef returns false for
    // CoverParenthesizedExpressionAndArrowParameterList, so `(x) = function(){}`
    // must NOT infer the name "x". We strip the wrapping and recurse, which
    // reaches the identifier/member cases below with plain emit_expr (no naming).
    ast.AssignmentExpression(
      ast.Assign,
      ast.ParenthesizedExpression(ast.Identifier(name)),
      right,
    ) -> {
      let e = emit_ir(e, opcode.IrScopeMakeRef(name))
      use e <- result.map(emit_expr(e, right))
      let e = emit_ir(e, IrDup)
      emit_ir(e, opcode.IrScopePutRef(name))
    }
    // Non-simple-assign parenthesized LHS — safe to unwrap (no name inference
    // for compound assignment anyway).
    ast.AssignmentExpression(op, ast.ParenthesizedExpression(inner), right) ->
      emit_expr(e, ast.AssignmentExpression(op, inner, right))

    // Annex B web-compat AssignmentTargetType: `f() = v` / `f() += v` parse
    // in sloppy mode, evaluate the call, then throw ReferenceError BEFORE
    // evaluating the RHS (the parser only lets CallExpression targets
    // through when !strict). Must precede the logical-assign and
    // destructuring branches.
    ast.AssignmentExpression(_, ast.CallExpression(callee, args), _) -> {
      use e <- result.map(emit_expr(e, ast.CallExpression(callee, args)))
      let e = emit_ir(e, IrPop)
      emit_ir(
        e,
        IrThrowError(
          opcode.ReferenceErrorKind,
          "Invalid left-hand side in assignment",
        ),
      )
    }

    // §13.15.2 logical assignment: x &&= v, x ||= v, x ??= v. Must precede
    // the generic compound-assignment branches (compound_to_binop has no
    // BinOpKind for these — they short-circuit instead).
    ast.AssignmentExpression(ast.LogicalAndAssign as op, lhs, right)
    | ast.AssignmentExpression(ast.LogicalOrAssign as op, lhs, right)
    | ast.AssignmentExpression(ast.NullishCoalesceAssign as op, lhs, right) ->
      emit_logical_assign(e, op, lhs, right)

    // Assignment to identifier. §13.15.2 step 1.a: the reference is
    // resolved BEFORE the RHS runs (observable through `with` Proxy traps
    // and binding deletion during RHS evaluation) — MakeRef pins the base.
    ast.AssignmentExpression(ast.Assign, ast.Identifier(name), right) -> {
      let inferred_name = case name {
        "*default*" -> "default"
        _ -> name
      }
      let e = emit_ir(e, opcode.IrScopeMakeRef(name))
      use e <- result.map(emit_named_expr(e, right, inferred_name))
      let e = emit_ir(e, IrDup)
      emit_ir(e, opcode.IrScopePutRef(name))
    }

    // Compound assignment to identifier. The reference is resolved once
    // (before get and RHS); the store reuses that base (§13.15.2).
    ast.AssignmentExpression(op, ast.Identifier(name), right) -> {
      case compound_to_binop(op) {
        Ok(bin_kind) -> {
          let e = emit_ir(e, opcode.IrScopeMakeRef(name))
          let e = emit_ir(e, opcode.IrScopeGetRef(name))
          use e <- result.map(emit_expr(e, right))
          let e = emit_ir(e, IrBinOp(bin_kind))
          let e = emit_ir(e, IrDup)
          emit_ir(e, opcode.IrScopePutRef(name))
        }
        Error(_) -> Error(Unsupported("assignment op"))
      }
    }

    // super.prop = val — §13.15.2 + §13.3.7.3 PutValue with super reference.
    ast.AssignmentExpression(
      ast.Assign,
      ast.MemberExpression(ast.SuperExpression, ast.Identifier(prop), False),
      right,
    ) -> {
      let e = emit_super_base(e) |> push_const(JsString(prop))
      use e <- result.map(emit_expr(e, right))
      emit_ir(e, IrPutSuperValue)
    }

    // super[k] = val
    ast.AssignmentExpression(
      ast.Assign,
      ast.MemberExpression(ast.SuperExpression, key, True),
      right,
    ) -> {
      let e = emit_super_base(e)
      use e <- result.try(emit_expr(e, key))
      use e <- result.map(emit_expr(e, right))
      emit_ir(e, IrPutSuperValue)
    }

    // super.x op= v / super[k] op= v — §13.15.2 compound assignment.
    // GetSuperValue2 reads while keeping [pk, base, this] under the value so
    // GetSuperBase and ToPropertyKey are evaluated exactly once (both are
    // observable: setPrototypeOf-in-toString test262 cases).
    ast.AssignmentExpression(
      op,
      ast.MemberExpression(ast.SuperExpression, key, computed),
      right,
    ) ->
      case compound_to_binop(op) {
        Ok(bin_kind) -> {
          let e = emit_super_base(e)
          use e <- result.try(emit_super_key(e, key, computed))
          let e = emit_ir(e, IrGetSuperValue2)
          use e <- result.map(emit_expr(e, right))
          emit_ir(e, IrBinOp(bin_kind)) |> emit_ir(IrPutSuperValue)
        }
        Error(Nil) -> Error(Unsupported("assignment op"))
      }

    // Assignment to dot member expression (obj.prop = val)
    ast.AssignmentExpression(
      ast.Assign,
      ast.MemberExpression(obj, ast.Identifier(prop), False),
      right,
    ) -> {
      use e <- result.try(emit_expr(e, obj))
      use e <- result.map(emit_expr(e, right))
      // Stack: [val, obj, ...] — PutField pops both, leaves val
      emit_put_field(e, prop)
    }

    // Compound assignment to dot member (obj.prop += val)
    ast.AssignmentExpression(
      op,
      ast.MemberExpression(obj, ast.Identifier(prop), False),
      right,
    ) -> {
      case compound_to_binop(op) {
        Ok(bin_kind) -> {
          use e <- result.try(emit_expr(e, obj))
          let e = emit_get_field2(e, prop)
          use e <- result.map(emit_expr(e, right))
          let e = emit_ir(e, IrBinOp(bin_kind))
          emit_put_field(e, prop)
        }
        Error(_) -> Error(Unsupported("assignment op"))
      }
    }

    // Assignment to computed member expression (obj[key] = val)
    ast.AssignmentExpression(
      ast.Assign,
      ast.MemberExpression(obj, key, True),
      right,
    ) -> {
      use e <- result.try(emit_expr(e, obj))
      use e <- result.try(emit_expr(e, key))
      use e <- result.map(emit_expr(e, right))
      // Stack: [obj, key, val] — PutElem expects [val, key, obj]
      emit_ir(e, IrPutElem)
    }

    // Compound assignment to computed member (obj[key] += val)
    ast.AssignmentExpression(op, ast.MemberExpression(obj, key, True), right) -> {
      case compound_to_binop(op) {
        Ok(bin_kind) -> {
          use e <- result.try(emit_expr(e, obj))
          use e <- result.try(emit_expr(e, key))
          // GetElem2 reads obj[key] but keeps obj+key on stack
          let e = emit_ir(e, IrGetElem2)
          use e <- result.map(emit_expr(e, right))
          let e = emit_ir(e, IrBinOp(bin_kind))
          // Stack: [obj, key, result] — PutElem consumes all three
          emit_ir(e, IrPutElem)
        }
        Error(_) -> Error(Unsupported("assignment op"))
      }
    }

    // Destructuring assignment expression: `[a, x.y] = rhs` or `({a, b} = rhs)`.
    // Parser guarantees LHS is ArrayExpression/ObjectExpression here
    // (parse_assignment_rhs only accepts these when last_expr_assignable=False).
    // Result of the whole expression is rhs (§13.15.2 step 6), so Dup before
    // destructure since emit_destructuring_assign consumes its input.
    ast.AssignmentExpression(ast.Assign, lhs, right) -> {
      use e <- result.try(emit_expr(e, right))
      let e = emit_ir(e, IrDup)
      emit_destructuring_assign(e, lhs)
    }

    // super(args) — §13.3.7.1 SuperCall, fully decomposed (QuickJS shape):
    //   GetLexical(active_func); GetPrototypeOf;        → parent ctor
    //   GetLexical(new_target);                         → lexical newTarget
    //   <args>; CallConstructor(n);                     → ordinary [[Construct]]
    //   Dup; SetThis;                                   → step 8 BindThisValue
    //   <field-init call if class has instance fields>  → step 12 InitializeInstanceElements
    // Works inside arrows because all reads go through lexical slots, and
    // arrows inherit field_init from the enclosing ctor's emitter.
    ast.CallExpression(ast.SuperExpression, args) -> {
      let e =
        e
        |> get_lexical(opcode.RefActiveFunc)
        |> emit_ir(IrGetPrototypeOf)
        |> get_lexical(opcode.RefNewTarget)
      use e <- result.map(case has_spread_arg(args) {
        True -> {
          use e <- result.map(emit_args_array_with_spread(e, args))
          emit_ir(e, IrCallConstructorApply)
        }
        False -> {
          use e <- result.map(list.try_fold(args, e, emit_expr))
          emit_ir(e, IrCallConstructor(list.length(args)))
        }
      })
      let e = e |> emit_ir(IrDup) |> set_this
      case e.field_init {
        FieldInitAfterSuper -> emit_field_init_call(e)
        NoFieldInit | FieldInitAtStart -> e
      }
    }

    // super.method(args) — §13.3.7.3 + §13.3.6.2: read super property with
    // receiver=this, then call with this as receiver.
    ast.CallExpression(
      ast.MemberExpression(ast.SuperExpression, ast.Identifier(method), False),
      args,
    ) -> {
      let e =
        emit_super_base_keep_recv(e)
        |> push_const(JsString(method))
        |> emit_ir(IrGetSuperValue)
      // Stack: [fn, this, ..]. Reuse CallMethodApply/CallMethod's [fn, recv]
      // shape — `this` is the receiver.
      case has_spread_arg(args) {
        False -> {
          use e <- result.map(list.try_fold(args, e, emit_expr))
          emit_ir(e, IrCallMethod(method, list.length(args)))
        }
        True -> {
          use e <- result.map(emit_args_array_with_spread(e, args))
          emit_ir(e, IrCallMethodApply)
        }
      }
    }

    // super[k](args)
    ast.CallExpression(
      ast.MemberExpression(ast.SuperExpression, key, True),
      args,
    ) -> {
      let e = emit_super_base_keep_recv(e)
      use e <- result.try(emit_expr(e, key))
      let e = emit_ir(e, IrGetSuperValue)
      case has_spread_arg(args) {
        False -> {
          use e <- result.map(list.try_fold(args, e, emit_expr))
          emit_ir(e, IrCallMethod("<super>", list.length(args)))
        }
        True -> {
          use e <- result.map(emit_args_array_with_spread(e, args))
          emit_ir(e, IrCallMethodApply)
        }
      }
    }

    // Method call: obj.method(args) — emits GetField2 + CallMethod for this binding.
    // Spread path: build args array after GetField2, then IrCallMethodApply.
    ast.CallExpression(
      ast.MemberExpression(obj, ast.Identifier(method_name), False),
      args,
    ) ->
      case chain_has_optional(obj) {
        // `a?.b.m(x)` — the receiver chain short-circuits the call too.
        True -> emit_chain_root(e, expr)
        False -> {
          use e <- result.try(emit_expr(e, obj))
          let e = emit_get_field2(e, method_name)
          case has_spread_arg(args) {
            False -> {
              use e <- result.map(list.try_fold(args, e, emit_expr))
              emit_ir(e, IrCallMethod(method_name, list.length(args)))
            }
            True -> {
              // Stack after GetField2: [method, receiver, ...]
              // Build args array on top, then CallMethodApply pops [args, method, receiver].
              use e <- result.map(emit_args_array_with_spread(e, args))
              emit_ir(e, IrCallMethodApply)
            }
          }
        }
      }
    // Computed method call: obj[key](args) — must bind `this` to obj.
    // GetElem2 leaves [method, key, receiver]; we shuffle to [method, receiver]
    // via Swap+Pop so CallMethod sees the same shape as the dot-access path.
    ast.CallExpression(ast.MemberExpression(obj, key, True), args) ->
      case chain_has_optional(obj) {
        // `a?.b[k](x)` — the receiver chain short-circuits the call too.
        True -> emit_chain_root(e, expr)
        False -> {
          use e <- result.try(emit_expr(e, obj))
          use e <- result.try(emit_expr(e, key))
          let e = emit_ir(e, IrGetElem2)
          // [method, key, receiver] → Swap → [key, method, receiver] → Pop → [method, receiver]
          let e = emit_ir(e, IrSwap)
          let e = emit_ir(e, IrPop)
          case has_spread_arg(args) {
            False -> {
              use e <- result.map(list.try_fold(args, e, emit_expr))
              // Static name unknown for computed access; CallMethod ignores name
              // at runtime anyway — it's informational only.
              emit_ir(e, IrCallMethod("[computed]", list.length(args)))
            }
            True -> {
              use e <- result.map(emit_args_array_with_spread(e, args))
              emit_ir(e, IrCallMethodApply)
            }
          }
        }
      }
    // Direct eval candidate: `eval(args)` with identifier callee.
    // Emits IrCallEval so the VM can do a runtime identity check against
    // the intrinsic eval. If it matches → direct eval (sees caller's locals).
    // If not (eval was shadowed/rebound) → regular call semantics.
    // Spread in eval(...args) is legal but rare; we fall through to regular
    // CallApply which gives indirect-eval semantics (acceptable for v1).
    ast.CallExpression(ast.Identifier("eval"), args) ->
      case has_spread_arg(args) {
        False -> {
          let e = emit_ir(e, IrScopeGetVar("eval"))
          use e <- result.map(list.try_fold(args, e, emit_expr))
          let e =
            emit_ir(
              e,
              opcode.IrCallEval(
                list.length(args),
                e.param_scope_names,
                e.with_stack,
                e.private_env,
              ),
            )
          Emitter(..e, has_eval_call: True)
        }
        True -> {
          let e = emit_ir(e, IrScopeGetVar("eval"))
          use e <- result.map(emit_args_array_with_spread(e, args))
          emit_ir(e, IrCallApply)
        }
      }

    // Regular call expression. §13.3.6.2 EvaluateCall step 1.b.ii: when the
    // callee is an identifier inside a `with` body, its resolution may cross
    // a with marker — then thisValue must be the with object (the env
    // record's WithBaseObject), not undefined. Emit the receiver+value pair
    // (IrScopeGetVarThis) and dispatch via the method-call path; Phase 2
    // pushes undefined as receiver when the binding is static. Outside with
    // bodies the plain IrCall path is unchanged. Mirrors QuickJS
    // OP_with_get_ref + OP_call_method.
    ast.CallExpression(callee, args) -> {
      // `a?.()(x)` etc. — optional link inside the callee chain
      // short-circuits this call too.
      use <- bool.lazy_guard(chain_has_optional(callee), fn() {
        emit_chain_root(e, expr)
      })
      case unwrap_parens(callee), e.with_stack {
        ast.Identifier(name), [_, ..] -> {
          let e = emit_ir(e, IrScopeGetVarThis(name))
          case has_spread_arg(args) {
            False -> {
              use e <- result.map(list.try_fold(args, e, emit_expr))
              emit_ir(e, IrCallMethod(name, list.length(args)))
            }
            True -> {
              use e <- result.map(emit_args_array_with_spread(e, args))
              emit_ir(e, IrCallMethodApply)
            }
          }
        }
        _, _ -> {
          use e <- result.try(emit_expr(e, callee))
          case has_spread_arg(args) {
            False -> {
              use e <- result.map(list.try_fold(args, e, emit_expr))
              emit_ir(e, opcode.IrCall(list.length(args)))
            }
            True -> {
              use e <- result.map(emit_args_array_with_spread(e, args))
              emit_ir(e, IrCallApply)
            }
          }
        }
      }
    }

    // Conditional (ternary)
    ast.ConditionalExpression(condition, consequent, alternate) -> {
      let #(e, else_label) = fresh_label(e)
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, condition))
      let e = emit_ir(e, IrJumpIfFalse(else_label))
      use e <- result.try(emit_expr(e, consequent))
      let e = emit_ir(e, IrJump(end_label))
      let e = emit_ir(e, IrLabel(else_label))
      use e <- result.map(emit_expr(e, alternate))
      emit_ir(e, IrLabel(end_label))
    }

    // Sequence expression (comma operator)
    ast.SequenceExpression(exprs) -> emit_sequence(e, exprs)

    // Object literal
    ast.ObjectExpression(properties) -> {
      let e = emit_ir(e, IrNewObject)
      list.try_fold(properties, e, emit_object_property)
    }

    // §13.3.7.3 super.prop — read via [[HomeObject]].[[Prototype]] with
    // receiver = lexical this. Must precede the generic MemberExpression arm.
    ast.MemberExpression(ast.SuperExpression, ast.Identifier(name), False) ->
      Ok(
        emit_super_base(e)
        |> push_const(JsString(name))
        |> emit_ir(IrGetSuperValue),
      )

    ast.MemberExpression(ast.SuperExpression, key, True) -> {
      let e = emit_super_base(e)
      use e <- result.map(emit_expr(e, key))
      emit_ir(e, IrGetSuperValue)
    }

    // Member expression (dot access). A `?.` link anywhere in the object
    // chain routes the WHOLE expression through the shared-short-circuit
    // chain compiler (§13.3.9.1).
    ast.MemberExpression(object, ast.Identifier(prop), False) ->
      case chain_has_optional(object) {
        True -> emit_chain_root(e, expr)
        False -> {
          use e <- result.map(emit_expr(e, object))
          emit_get_field(e, prop)
        }
      }

    // Computed member expression (obj[key])
    ast.MemberExpression(object, property, True) ->
      case chain_has_optional(object) {
        True -> emit_chain_root(e, expr)
        False -> {
          use e <- result.try(emit_expr(e, object))
          use e <- result.map(emit_expr(e, property))
          emit_ir(e, IrGetElem)
        }
      }

    // Optional member / call expressions — always chain roots.
    ast.OptionalMemberExpression(..) | ast.OptionalCallExpression(..) ->
      emit_chain_root(e, expr)

    // Array literal
    // Fast path (no spread, no holes): push N elements, IrArrayFrom(N).
    // Hole path (no spread, has holes): push only non-hole values,
    //   IrArrayFromWithHoles(N, hole_indices) builds a sparse array.
    // Slow path (any spread): push prefix, then incrementally
    //   IrArrayPush / IrArrayPushHole / IrArraySpread the rest.
    //   Mirrors QuickJS's OP_append approach.
    ast.ArrayExpression(elements) ->
      case has_spread_element(elements) {
        False -> emit_array_no_spread(e, elements)
        True -> emit_array_with_spread(e, elements)
      }

    // Function expression
    ast.FunctionExpression(name, params, body, is_gen, is_async) ->
      emit_function_closure(e, name, params, body, is_gen, is_async, True)

    // Arrow function expression
    ast.ArrowFunctionExpression(params, body, is_async) ->
      emit_arrow_closure(e, None, params, body, is_async)

    // `this` — non-arrows own the slot; arrows resolve it as a capture from
    // the nearest enclosing non-arrow.
    ast.ThisExpression -> Ok(get_this(e))

    // §13.3.12 new.target — reads the lexical [[NewTarget]] slot.
    ast.MetaProperty("new", "target") -> Ok(get_lexical(e, opcode.RefNewTarget))

    // New expression: new Foo(args). CallConstructor's stack contract is
    // [args, new_target, ctor] — for plain `new`, newTarget == ctor, so Dup.
    ast.NewExpression(callee, args) -> {
      use e <- result.try(emit_expr(e, callee))
      let e = emit_ir(e, IrDup)
      case has_spread_arg(args) {
        False -> {
          use e <- result.map(list.try_fold(args, e, emit_expr))
          emit_ir(e, IrCallConstructor(list.length(args)))
        }
        True -> {
          use e <- result.map(emit_args_array_with_spread(e, args))
          emit_ir(e, IrCallConstructorApply)
        }
      }
    }

    // Template literal: `text ${expr} more`
    // Desugar to string concatenation: "" + "text " + expr + " more"
    ast.TemplateLiteral(quasis, expressions) ->
      emit_template_literal(e, quasis, expressions)

    // Class expression
    ast.ClassExpression(name, super_class, body) ->
      compile_class(e, name, name, super_class, body)

    // Yield expression (inside generator functions)
    ast.YieldExpression(argument, is_delegate) -> {
      let e = case argument {
        Some(arg) -> emit_expr(e, arg)
        None -> Ok(push_const(e, JsUndefined))
      }
      use e <- result.try(e)
      case is_delegate {
        // Plain yield. In async generators the operand is awaited first:
        // YieldExpression evaluation does AsyncGeneratorYield(? Await(value))
        // (tc39/ecma262#2819), so yielding a rejected promise throws at the
        // yield point. Sync generators yield the value as-is.
        False ->
          case e.is_async {
            True -> Ok(emit_ir(emit_ir(e, IrAwait), IrYield))
            False -> Ok(emit_ir(e, IrYield))
          }
        True ->
          case e.is_async {
            True -> {
              // Async-gen yield* — GetIterator(expr, async) wraps sync
              // iterables via CreateAsyncFromSyncIterator. Seed undefined arg,
              // self-loop: Next calls iter.next(arg), Await settles result,
              // Resume checks done / yields and jumps back to Next via label.
              // Leaves final result.value on stack.
              let e = emit_ir(e, IrGetAsyncIterator)
              // Cache [[NextMethod]] once (GetIteratorFromMethod §7.4.4) so
              // the loop doesn't re-Get `next` per step.
              let e = emit_ir(e, IrIteratorRecord)
              let e = push_const(e, JsUndefined)
              let #(e, next_label) = fresh_label(e)
              let e = emit_ir(e, IrLabel(next_label))
              let e = emit_ir(e, IrAsyncYieldStarNext)
              let e = emit_ir(e, IrAwait)
              Ok(emit_ir(e, IrAsyncYieldStarResume(next_label)))
            }
            False -> {
              // Sync yield* — get iterator, seed undefined, self-looping
              // YieldStar handles the rest. Leaves final result.value on stack.
              let e = emit_ir(e, IrGetIterator)
              let e = push_const(e, JsUndefined)
              Ok(emit_ir(e, IrYieldStar))
            }
          }
      }
    }

    ast.AwaitExpression(argument) -> {
      use e <- result.map(emit_expr(e, argument))
      emit_ir(e, IrAwait)
    }

    // Parenthesized expression — transparent for evaluation, just unwrap
    ast.ParenthesizedExpression(inner) -> emit_expr(e, inner)

    // using-declaration desugar intrinsics (internal-only AST nodes)
    ast.IntrinsicGetDisposer(argument:, is_async:) -> {
      use e <- result.map(emit_expr(e, argument))
      emit_ir(e, opcode.IrGetDisposer(is_async))
    }
    ast.IntrinsicMakeSuppressed(error:, suppressed:) -> {
      use e <- result.try(emit_expr(e, error))
      use e <- result.map(emit_expr(e, suppressed))
      emit_ir(e, opcode.IrMakeSuppressed)
    }

    // RegExp literal — push pattern and flags, then NewRegExp opcode
    ast.RegExpLiteral(pattern, flags) -> {
      let e = push_const(e, JsString(pattern))
      let e = push_const(e, JsString(flags))
      Ok(emit_ir(e, IrNewRegExp))
    }

    // §13.3.10 ImportCall: import(specifier) / import(specifier, options).
    // Push specifier, push options (undefined when absent), then the
    // DynamicImport opcode performs the EvaluateImportCall runtime steps.
    ast.ImportExpression(source, options, phase) -> {
      use e <- result.try(emit_expr(e, source))
      case phase {
        ast.PhaseEvaluation -> {
          use e <- result.map(case options {
            Some(opts) -> emit_expr(e, opts)
            None -> Ok(push_const(e, JsUndefined))
          })
          emit_ir(e, opcode.IrDynamicImport)
        }
        // Phase forms (import.source/import.defer) take a single
        // AssignmentExpression — no options argument to push.
        ast.PhaseSource -> Ok(emit_ir(e, opcode.IrDynamicImportSource))
        ast.PhaseDefer -> Ok(emit_ir(e, opcode.IrDynamicImportDefer))
      }
    }

    // Tagged template (§13.3.11): lower to a call of the tag function with
    // the per-site template object as the first argument, reusing the
    // regular CallExpression paths so this-binding works (obj.tag`x` calls
    // tag with this = obj, §13.3.6.2 EvaluateCall).
    ast.TaggedTemplateExpression(tag:, cooked:, raw:, expressions:) -> {
      let site = unique_positive_integer()
      let template = ast.IntrinsicTemplateObject(site:, cooked:, raw:)
      // §13.3.11.1 step 1 note: a tagged template is never a direct eval —
      // wrap a bare `eval` tag in parens to dodge the IrCallEval path.
      let tag = case tag {
        ast.Identifier("eval") -> ast.ParenthesizedExpression(tag)
        _ -> tag
      }
      emit_expr(e, ast.CallExpression(tag, [template, ..expressions]))
    }
    ast.IntrinsicTemplateObject(site:, cooked:, raw:) ->
      Ok(emit_ir(e, opcode.IrGetTemplateObject(site, cooked, raw)))

    _ -> Error(Unsupported("expression: " <> string_inspect_expr_kind(expr)))
  }
}

fn emit_template_literal(
  e: Emitter,
  quasis: List(String),
  expressions: List(ast.Expression),
) -> Result(Emitter, EmitError) {
  // Template literal `a${x}b${y}c` has quasis=["a","b","c"], expressions=[x,y]
  // Desugar to: "a" + x + "b" + y + "c"
  case quasis {
    [] -> Ok(push_const(e, JsString("")))
    [first, ..rest_quasis] -> {
      // Start with the first quasi string
      let e = push_const(e, JsString(first))
      // Interleave: for each expression, Add it, then Add the next quasi
      emit_template_parts(e, expressions, rest_quasis)
    }
  }
}

fn emit_template_parts(
  e: Emitter,
  expressions: List(ast.Expression),
  quasis: List(String),
) -> Result(Emitter, EmitError) {
  case expressions, quasis {
    [expr, ..rest_exprs], [quasi, ..rest_quasis] -> {
      // Emit expression, ToString it (§13.2.8.5 — string hint, NOT the Add
      // operator's default-hint ToPrimitive), then concat with accumulator.
      use e <- result.try(emit_expr(e, expr))
      let e = emit_ir(e, opcode.IrToStringVal)
      let e = emit_ir(e, IrBinOp(opcode.Add))
      // Emit next quasi string, concat
      let e = push_const(e, JsString(quasi))
      let e = emit_ir(e, IrBinOp(opcode.Add))
      emit_template_parts(e, rest_exprs, rest_quasis)
    }
    // If there are trailing expressions without quasis (shouldn't happen but safe)
    [expr, ..rest_exprs], [] -> {
      use e <- result.try(emit_expr(e, expr))
      let e = emit_ir(e, opcode.IrToStringVal)
      let e = emit_ir(e, IrBinOp(opcode.Add))
      emit_template_parts(e, rest_exprs, [])
    }
    // Done
    [], _ -> Ok(e)
  }
}

fn emit_switch(
  e: Emitter,
  discriminant: ast.Expression,
  cases: List(ast.SwitchCase),
) -> Result(Emitter, EmitError) {
  let #(e, end_label) = fresh_label(e)

  // Push break context for switch (break; exits the switch). Switch is not a
  // continue target — emit_goto_loop walks past it to the enclosing loop.
  let e = push_loop(e, end_label, -1)

  // Emit discriminant — stays on stack through comparison phase.
  // Evaluated OUTSIDE the CaseBlock scope (§14.12.4 step 1).
  use e <- result.try(emit_expr(e, discriminant))

  // The CaseBlock is a single block scope (§14.12.4 step 3-5): case tests and
  // bodies run inside it, with its let/const/class + function declarations
  // instantiated up front (BlockDeclarationInstantiation).
  let case_stmts =
    list.flat_map(cases, fn(c) {
      let ast.SwitchCase(_, consequent) = c
      consequent
    })
  let saved_annexb = e
  let e = emit_op(e, EnterScope(BlockScope))
  let e =
    Emitter(
      ..e,
      in_block: True,
      annexb_blocked: set.union(
        set.union(e.annexb_blocked, e.annexb_level_fns),
        top_lex_name_set(case_stmts),
      ),
      annexb_level_fns: set.from_list(direct_fn_names(case_stmts)),
    )
  let e = emit_block_declarations(e, case_stmts)

  // Allocate labels: each non-default case gets a "found" trampoline label
  // and a "body" label. Default cases only get a "body" label.
  // The trampoline pops the discriminant then jumps to the body label.
  // This ensures the discriminant is off the stack for all body code,
  // allowing fall-through between case bodies to work correctly.
  let #(e, body_labels_rev) =
    list.fold(cases, #(e, []), fn(acc, _case) {
      let #(e, labels) = acc
      let #(e, label) = fresh_label(e)
      #(e, [label, ..labels])
    })
  let body_labels = list.reverse(body_labels_rev)

  // Allocate found (trampoline) labels for non-default cases
  let #(e, found_labels_rev) =
    list.fold(cases, #(e, []), fn(acc, c) {
      let #(e, labels) = acc
      case c {
        ast.SwitchCase(Some(_), _) -> {
          let #(e, label) = fresh_label(e)
          #(e, [Some(label), ..labels])
        }
        ast.SwitchCase(None, _) -> #(e, [None, ..labels])
      }
    })
  let found_labels = list.reverse(found_labels_rev)

  // Pair each case with its body label and optional found label so the three
  // emission phases below are single O(c) folds (no per-index list.drop).
  let labelled_cases = list.zip(cases, list.zip(body_labels, found_labels))

  // Phase 1: Emit comparison jumps
  // For each case with a test: Dup discriminant, emit test, StrictEq, JumpIfTrue(found_N)
  let #(e, default_body_label) =
    list.fold(labelled_cases, #(e, option.None), fn(acc, entry) {
      let #(e, default_lbl) = acc
      let #(c, #(body_lbl, found_lbl)) = entry
      case c {
        ast.SwitchCase(Some(test_expr), _) -> {
          let e = emit_ir(e, IrDup)
          case emit_expr(e, test_expr) {
            Ok(e) -> {
              let e = emit_ir(e, IrBinOp(opcode.StrictEq))
              let found_lbl = option.unwrap(found_lbl, end_label)
              let e = emit_ir(e, IrJumpIfTrue(found_lbl))
              #(e, default_lbl)
            }
            Error(_) -> #(e, default_lbl)
          }
        }
        ast.SwitchCase(None, _) -> {
          // Default case — record its body label
          #(e, Some(body_lbl))
        }
      }
    })

  // No match: pop discriminant and jump to default body or end
  let e = emit_ir(e, IrPop)
  let e = emit_ir(e, IrJump(option.unwrap(default_body_label, end_label)))

  // Phase 2: Emit trampolines — each pops discriminant and jumps to body
  let e =
    list.fold(labelled_cases, e, fn(e, entry) {
      let #(_c, #(body_lbl, found_lbl)) = entry
      case found_lbl {
        Some(found_lbl) -> {
          let e = emit_ir(e, IrLabel(found_lbl))
          let e = emit_ir(e, IrPop)
          emit_ir(e, IrJump(body_lbl))
        }
        None -> e
      }
    })

  // Phase 3: Emit case bodies (fall-through between them)
  let e =
    list.fold(labelled_cases, e, fn(e, entry) {
      let #(c, #(body_lbl, _found_lbl)) = entry
      let e = emit_ir(e, IrLabel(body_lbl))
      case c {
        ast.SwitchCase(_, consequent) ->
          emit_stmts(e, consequent) |> result.unwrap(e)
      }
    })

  let e = emit_ir(e, IrLabel(end_label))
  let e = restore_annexb_ctx(e, saved_annexb)
  let e = emit_op(e, LeaveScope)
  let e = pop_loop(e)
  Ok(e)
}

fn emit_sequence(
  e: Emitter,
  exprs: List(ast.Expression),
) -> Result(Emitter, EmitError) {
  case exprs {
    [] -> Ok(push_const(e, JsUndefined))
    [only] -> emit_expr(e, only)
    [first, ..rest] -> {
      use e <- result.try(emit_expr(e, first))
      let e = emit_ir(e, IrPop)
      emit_sequence(e, rest)
    }
  }
}

/// Like emit_expr, but if expr is an anonymous function/arrow/class definition,
/// bake `name` into it (ES spec §8.4 NamedEvaluation).
fn emit_named_expr(
  e: Emitter,
  expr: ast.Expression,
  name: String,
) -> Result(Emitter, EmitError) {
  case expr {
    // IsAnonymousFunctionDefinition looks through ParenthesizedExpression
    // (ES spec §13.2.1.2), so (function(){}) is still anonymous.
    ast.ParenthesizedExpression(inner) -> emit_named_expr(e, inner, name)
    // Anonymous function expression → bake name (no self-name binding —
    // §8.4 NamedEvaluation does not create one)
    ast.FunctionExpression(None, params, body, is_gen, is_async) ->
      emit_function_closure(
        e,
        Some(name),
        params,
        body,
        is_gen,
        is_async,
        False,
      )
    // Arrow function → bake name
    ast.ArrowFunctionExpression(params, body, is_async) ->
      emit_arrow_closure(e, Some(name), params, body, is_async)
    // Anonymous class expression → bake `.name` only; NO inner binding
    // (§8.4 NamedEvaluation step 2 — classBinding is undefined).
    ast.ClassExpression(None, super_class, body) ->
      compile_class(e, None, Some(name), super_class, body)
    // Not anonymous → emit normally (named fn keeps its own name)
    _ -> emit_expr(e, expr)
  }
}

/// Compile a method / getter / setter body into a child function, register
/// it, and emit IrMakeClosure for it. Never a constructor (methods/accessors
/// have no [[Construct]], so `new o.m()` must throw); always method_perms.
fn make_method_closure(
  e: Emitter,
  name: Option(String),
  params: List(ast.Pattern),
  body: ast.Statement,
  is_gen: Bool,
  is_async: Bool,
) -> Emitter {
  let child =
    compile_function_body(
      e,
      name,
      None,
      params,
      body,
      False,
      is_gen,
      is_async,
      False,
      opcode.method_perms,
      NoFieldInit,
    )
  let #(e, idx) = add_child_function(e, child)
  emit_ir(e, IrMakeClosure(idx))
}

/// Compile a function expression body into a child function and emit the
/// closure. A constructor unless generator/async.
fn emit_function_closure(
  e: Emitter,
  name: Option(String),
  params: List(ast.Pattern),
  body: ast.Statement,
  is_gen: Bool,
  is_async: Bool,
  // True only for a SYNTACTICALLY named function expression — creates the
  // §13.2.5.5 self-name binding. False for NamedEvaluation-baked names
  // (`var f = function () {}` has no inner `f` binding).
  bind_self: Bool,
) -> Result(Emitter, EmitError) {
  let self_name = case bind_self {
    True -> name
    False -> None
  }
  let child =
    compile_function_body(
      e,
      name,
      self_name,
      params,
      body,
      False,
      is_gen,
      is_async,
      !is_gen && !is_async,
      opcode.fn_perms,
      NoFieldInit,
    )
  let #(e, idx) = add_child_function(e, child)
  Ok(emit_ir(e, IrMakeClosure(idx)))
}

/// Compile an arrow function body into a child function and emit the
/// closure. Expression bodies are normalized to `{ return expr; }`.
/// Arrows are never constructors.
fn emit_arrow_closure(
  e: Emitter,
  name: Option(String),
  params: List(ast.Pattern),
  body: ast.ArrowBody,
  is_async: Bool,
) -> Result(Emitter, EmitError) {
  let body_stmt = case body {
    ast.ArrowBodyExpression(expr) ->
      ast.BlockStatement([ast.StmtWithLine(0, ast.ReturnStatement(Some(expr)))])
    ast.ArrowBodyBlock(stmt) -> stmt
  }
  let child =
    compile_function_body(
      e,
      name,
      None,
      params,
      body_stmt,
      True,
      False,
      is_async,
      False,
      opcode.fn_perms,
      NoFieldInit,
    )
  let #(e, idx) = add_child_function(e, child)
  Ok(emit_ir(e, IrMakeClosure(idx)))
}

/// Emit one property in an object literal. Object is already on the stack.
/// All handlers leave the object on the stack for the next property.
/// Compile an object-literal *method* value — a concise method, getter, or
/// setter — so it is NOT a constructor (methods/accessors have no
/// [[Construct]], so `new o.m()` must throw). These values are always
/// FunctionExpressions; fall back defensively for anything unexpected.
fn emit_method_value(
  e: Emitter,
  value: ast.Expression,
  name: Option(String),
) -> Result(Emitter, EmitError) {
  case value {
    ast.FunctionExpression(_, params, body, is_gen, is_async) ->
      Ok(make_method_closure(e, name, params, body, is_gen, is_async))
    _ -> emit_expr(e, value)
  }
}

fn emit_object_property(
  e: Emitter,
  prop: ast.Property,
) -> Result(Emitter, EmitError) {
  case prop {
    // Annex B §B.3.1 — `{__proto__: v}` / `{"__proto__": v}` sets [[Prototype]]
    // instead of defining an own property. Only when non-computed, non-shorthand,
    // non-method. Shorthand `{__proto__}` and computed `{["__proto__"]: v}` fall
    // through to ordinary DefineField.
    ast.Property(
      key: ast.Identifier("__proto__"),
      value:,
      kind: ast.Init,
      computed: False,
      shorthand: False,
      method: False,
    )
    | ast.Property(
        key: ast.StringExpression("__proto__"),
        value:,
        kind: ast.Init,
        computed: False,
        shorthand: False,
        method: False,
      ) -> {
      use e <- result.map(emit_expr(e, value))
      emit_ir(e, IrSetProto)
    }

    // Static key: {name: value}, {"name": value}, or shorthand method {name(){}}
    // → IrDefineField(name) — pops value, keeps obj. Shorthand methods get
    // [[HomeObject]] (§15.4.4) so super.x works inside them; `{m: fn}` does not.
    ast.Property(
      key: ast.Identifier(name),
      value:,
      kind: ast.Init,
      computed: False,
      method:,
      ..,
    )
    | ast.Property(
        key: ast.StringExpression(name),
        value:,
        kind: ast.Init,
        computed: False,
        method:,
        ..,
      ) -> {
      case method {
        // Concise method `{ m() {} }`: not constructible (`new o.m()` throws)
        // and records its [[HomeObject]] for `super`. `{ x: function(){} }`
        // stays a plain (constructible) function value with no home object.
        True -> {
          use e <- result.map(emit_method_value(e, value, Some(name)))
          let e = emit_ir(e, IrMakeMethod)
          emit_ir(e, IrDefineField(name))
        }
        False -> {
          use e <- result.map(emit_named_expr(e, value, name))
          emit_ir(e, IrDefineField(name))
        }
      }
    }

    // Numeric literal key: {1: "a"} — not computed in the AST, but needs
    // ToPropertyKey at runtime to get the canonical string form ("1" not "1.0").
    // Route through IrDefineFieldComputed which calls put_elem_value → js_to_string.
    ast.Property(
      key: ast.NumberLiteral(n),
      value:,
      kind: ast.Init,
      computed: False,
      method:,
      ..,
    ) ->
      emit_computed_init_property(
        e,
        fn(e) { Ok(push_const(e, JsNumber(Finite(n)))) },
        value,
        method,
      )

    // Computed key: {[expr]: value} or {[expr](){}}
    // Emit key, emit value, IrDefineFieldComputed — pops both, keeps obj.
    // The VM handles ToPropertyKey (Symbol preserved, else ToString).
    ast.Property(key:, value:, kind: ast.Init, computed: True, method:, ..) ->
      emit_computed_init_property(e, emit_expr(_, key), value, method)

    // Spread: {...source}
    // IrObjectSpread pops source, copies own enumerable props, keeps obj.
    // null/undefined sources are no-ops per CopyDataProperties spec.
    ast.SpreadProperty(argument:) -> {
      use e <- result.map(emit_expr(e, argument))
      emit_ir(e, IrObjectSpread)
    }

    // Getter: { get name() { ... } }
    // Emit the function, then DefineAccessor(name, Getter).
    ast.Property(
      key: ast.Identifier(name),
      value:,
      kind: ast.Get,
      computed: False,
      ..,
    )
    | ast.Property(
        key: ast.StringExpression(name),
        value:,
        kind: ast.Get,
        computed: False,
        ..,
      ) -> {
      use e <- result.map(emit_method_value(e, value, Some("get " <> name)))
      emit_ir(e, IrDefineAccessor(name, opcode.Getter, True))
    }

    // Setter: { set name(v) { ... } }
    ast.Property(
      key: ast.Identifier(name),
      value:,
      kind: ast.Set,
      computed: False,
      ..,
    )
    | ast.Property(
        key: ast.StringExpression(name),
        value:,
        kind: ast.Set,
        computed: False,
        ..,
      ) -> {
      use e <- result.map(emit_method_value(e, value, Some("set " <> name)))
      emit_ir(e, IrDefineAccessor(name, opcode.Setter, True))
    }

    // Computed or exotic-key getter/setter: { get [expr]() {} }
    // Stack: emit key, emit fn → DefineAccessorComputed
    ast.Property(key:, value:, kind: ast.Get, ..) -> {
      use e <- result.try(emit_expr(e, key))
      use e <- result.map(emit_method_value(e, value, None))
      emit_ir(e, IrDefineAccessorComputed(opcode.Getter, True))
    }
    ast.Property(key:, value:, kind: ast.Set, ..) -> {
      use e <- result.try(emit_expr(e, key))
      use e <- result.map(emit_method_value(e, value, None))
      emit_ir(e, IrDefineAccessorComputed(opcode.Setter, True))
    }

    // Remaining case: non-computed Init with an exotic key expression
    // (shouldn't happen — parser only produces Identifier/StringExpression/
    // NumberLiteral for non-computed keys). Route through computed path anyway.
    ast.Property(key:, value:, kind: ast.Init, computed: False, method:, ..) ->
      emit_computed_init_property(e, emit_expr(_, key), value, method)
  }
}

/// Shared emit for `kind: Init` properties that go through DefineFieldComputed
/// (computed key, numeric key, or exotic-key fallthrough). When `method` is
/// True (`{[k](){}}`), the closure is emitted first so MakeMethod sees [obj, fn]
/// directly, then the key, then Swap to restore [obj, key, fn] for
/// DefineFieldComputed. Closure creation is unobservable so this doesn't change
/// §13.2.5.5 evaluation order — key side-effects still run before any property
/// is defined.
fn emit_computed_init_property(
  e: Emitter,
  emit_key: fn(Emitter) -> Result(Emitter, EmitError),
  value: ast.Expression,
  method: Bool,
) -> Result(Emitter, EmitError) {
  case method {
    False -> {
      use e <- result.try(emit_key(e))
      use e <- result.map(emit_expr(e, value))
      emit_ir(e, IrDefineFieldComputed)
    }
    True -> {
      use e <- result.try(emit_method_value(e, value, None))
      let e = emit_ir(e, IrMakeMethod)
      use e <- result.map(emit_key(e))
      emit_ir(emit_ir(e, IrSwap), IrDefineFieldComputed)
    }
  }
}

// ============================================================================
// Spread element support — array literals and call argument lists
// ============================================================================

/// Emit an array literal that contains no SpreadElement (ES2024 section
/// 13.2.4 "Array Initializer" — the non-spread case). Decides between the
/// dense fast path (IrArrayFrom) and the sparse path (IrArrayFromWithHoles)
/// based on whether any element is an Elision (None in the AST).
///
/// Single pass over elements: push non-hole values onto the stack, collect
/// hole indices. Accumulator threads #(emitter, index, holes_rev).
fn emit_array_no_spread(
  e: Emitter,
  elements: List(Option(ast.Expression)),
) -> Result(Emitter, EmitError) {
  let count = list.length(elements)
  use #(e, _idx, holes_rev) <- result.map(
    list.try_fold(elements, #(e, 0, []), fn(acc, elem) {
      let #(e, idx, holes_rev) = acc
      case elem {
        Some(expr) -> {
          use e <- result.map(emit_expr(e, expr))
          #(e, idx + 1, holes_rev)
        }
        None -> Ok(#(e, idx + 1, [idx, ..holes_rev]))
      }
    }),
  )
  case holes_rev {
    [] -> emit_ir(e, IrArrayFrom(count))
    _ -> emit_ir(e, IrArrayFromWithHoles(count, list.reverse(holes_rev)))
  }
}

/// Emit the prefix of a spread-mode array literal (elements before the first
/// SpreadElement). Delegates to emit_array_no_spread; factored out so the
/// spread path can build the initial array then append spread elements.
fn emit_array_prefix(
  e: Emitter,
  prefix: List(Option(ast.Expression)),
) -> Result(Emitter, EmitError) {
  emit_array_no_spread(e, prefix)
}

/// True if any element is Some(SpreadElement(_)). Used to choose the
/// fast static-arity path vs the incremental-build spread path.
fn has_spread_element(elements: List(Option(ast.Expression))) -> Bool {
  list.any(elements, fn(el) {
    case el {
      Some(ast.SpreadElement(_)) -> True
      _ -> False
    }
  })
}

/// True if any arg is SpreadElement(_). Call argument lists use plain
/// List(Expression), not List(Option(Expression)), so no hole case here.
fn has_spread_arg(args: List(ast.Expression)) -> Bool {
  list.any(args, fn(a) {
    case a {
      ast.SpreadElement(_) -> True
      _ -> False
    }
  })
}

/// Emit an array literal that contains at least one SpreadElement.
///
/// Strategy (QuickJS-style):
///   1. Peel off the leading non-spread run (prefix), push those elements,
///      then IrArrayFrom / IrArrayFromWithHoles to pack them. This handles
///      the common `[a, b, ...rest]` shape in one opcode.
///   2. For each remaining element, emit:
///      - IrArraySpread (drain iterator into array) for spread elements
///      - IrArrayPush (single append) for regular elements
///      - IrArrayPushHole (increment length, no element) for holes
///
/// Stack invariant throughout step 2: array is on top; each IrArrayPush /
/// IrArraySpread consumes [val-or-iter, arr] → [arr]; IrArrayPushHole
/// consumes [arr] → [arr].
///
/// Holes in the *source* of a spread become undefined per the array
/// iterator spec — that's a different thing from holes in the literal.
fn emit_array_with_spread(
  e: Emitter,
  elements: List(Option(ast.Expression)),
) -> Result(Emitter, EmitError) {
  // Split at first spread: prefix has no spreads, tail starts at first spread.
  let #(prefix, tail) = split_at_first_spread_element(elements)

  // Pack the prefix (handles holes via IrArrayFromWithHoles if needed).
  use e <- result.try(emit_array_prefix(e, prefix))

  // Incrementally append the tail.
  list.try_fold(tail, e, fn(e, elem) {
    case elem {
      Some(ast.SpreadElement(argument:)) -> {
        use e <- result.map(emit_expr(e, argument))
        emit_ir(e, IrArraySpread)
      }
      Some(expr) -> {
        use e <- result.map(emit_expr(e, expr))
        emit_ir(e, IrArrayPush)
      }
      None ->
        // Hole after a spread — increment length without setting element.
        Ok(emit_ir(e, IrArrayPushHole))
    }
  })
}

/// Build an args array for a spread-call (f(a, ...b, c) etc).
/// Same algorithm as emit_array_with_spread but over List(Expression)
/// (call args have no holes — the parser doesn't produce them in arglists).
/// Leaves the args array on top of the stack; caller follows with an
/// IrCallApply / IrCallMethodApply / IrCallConstructorApply.
fn emit_args_array_with_spread(
  e: Emitter,
  args: List(ast.Expression),
) -> Result(Emitter, EmitError) {
  let #(prefix, tail) = split_at_first_spread_arg(args)

  let prefix_count = list.length(prefix)
  use e <- result.try(list.try_fold(prefix, e, emit_expr))
  let e = emit_ir(e, IrArrayFrom(prefix_count))

  list.try_fold(tail, e, fn(e, arg) {
    case arg {
      ast.SpreadElement(argument:) -> {
        use e <- result.map(emit_expr(e, argument))
        emit_ir(e, IrArraySpread)
      }
      _ -> {
        use e <- result.map(emit_expr(e, arg))
        emit_ir(e, IrArrayPush)
      }
    }
  })
}

/// Split an array-literal element list at the first SpreadElement.
/// Returns (prefix_with_no_spreads, tail_starting_at_first_spread).
/// If no spread exists, tail is [] — but callers have already checked
/// has_spread_element so tail is always non-empty in practice.
fn split_at_first_spread_element(
  elements: List(Option(ast.Expression)),
) -> #(List(Option(ast.Expression)), List(Option(ast.Expression))) {
  split_at_first_spread_element_loop(elements, [])
}

fn split_at_first_spread_element_loop(
  remaining: List(Option(ast.Expression)),
  acc: List(Option(ast.Expression)),
) -> #(List(Option(ast.Expression)), List(Option(ast.Expression))) {
  case remaining {
    [] -> #(list.reverse(acc), [])
    [Some(ast.SpreadElement(_)), ..] -> #(list.reverse(acc), remaining)
    [el, ..rest] -> split_at_first_spread_element_loop(rest, [el, ..acc])
  }
}

fn split_at_first_spread_arg(
  args: List(ast.Expression),
) -> #(List(ast.Expression), List(ast.Expression)) {
  split_at_first_spread_arg_loop(args, [])
}

fn split_at_first_spread_arg_loop(
  remaining: List(ast.Expression),
  acc: List(ast.Expression),
) -> #(List(ast.Expression), List(ast.Expression)) {
  case remaining {
    [] -> #(list.reverse(acc), [])
    [ast.SpreadElement(_), ..] -> #(list.reverse(acc), remaining)
    [a, ..rest] -> split_at_first_spread_arg_loop(rest, [a, ..acc])
  }
}

// ============================================================================
// For-in / for-of loops
// ============================================================================

/// Emit a for-in loop: `for (lhs in rhs) body`
///
/// Stack pattern:
///   [obj] → ForInStart → [iterator]
///   loop: ForInNext → [iterator, key, done]
///   JumpIfTrue(cleanup) → [iterator, key]
///   bind key → [iterator]
///   body
///   Jump(loop) → cleanup: Pop key → loop_end: Pop iterator
fn emit_for_in(
  e: Emitter,
  left: ast.ForInit,
  right: ast.Expression,
  body: ast.Statement,
) -> Result(Emitter, EmitError) {
  let #(e, loop_start) = fresh_label(e)
  let #(e, loop_continue) = fresh_label(e)
  let #(e, cleanup) = fresh_label(e)
  let #(e, loop_end) = fresh_label(e)

  // Block scope for let/const
  let e = emit_op(e, EnterScope(BlockScope))

  // Evaluate the right-hand side (object to iterate) — bound names of a
  // let/const ForDeclaration are in TDZ during this evaluation (§14.7.5.5).
  use e <- result.try(emit_for_head_expr(e, left, right))
  // ForInStart: pops object, pushes iterator ref
  let e = emit_ir(e, IrForInStart)

  let e = push_loop(e, loop_end, loop_continue)
  let e = emit_ir(e, IrLabel(loop_start))

  // ForInNext: peeks iterator, pushes key + done
  let e = emit_ir(e, IrForInNext)
  // If done, jump to cleanup (where we pop the unused key)
  let e = emit_ir(e, IrJumpIfTrue(cleanup))

  // Bind the key to the left-hand side variable
  use e <- result.try(emit_for_lhs_bind(e, left))

  // Body — head let/const names block Annex B promotion inside it
  let saved_annexb = e
  let e = annexb_head_ctx(e, for_init_lex_names(left))
  use e <- result.try(emit_stmt(e, body))
  let e = restore_annexb_ctx(e, saved_annexb)

  // Continue point
  let e = emit_ir(e, IrLabel(loop_continue))
  let e = emit_ir(e, IrJump(loop_start))

  // cleanup: pop the key (done=true left it on stack)
  let e = emit_ir(e, IrLabel(cleanup))
  let e = emit_ir(e, IrPop)

  // loop_end: pop the iterator
  let e = emit_ir(e, IrLabel(loop_end))
  let e = emit_ir(e, IrPop)

  let e = pop_loop(e)
  let e = emit_op(e, LeaveScope)
  Ok(e)
}

/// Head let/const names of a for-in/for-of left-hand side.
fn for_init_lex_names(left: ast.ForInit) -> List(String) {
  case left {
    ast.ForInitDeclaration(decl) -> for_head_lex_names(decl)
    _ -> []
  }
}

/// §14.7.5.5 ForIn/OfHeadEvaluation steps 2-5: when the head is a let/const
/// ForDeclaration, the AssignmentExpression is evaluated inside a fresh
/// declarative environment whose bound names exist but are UNINITIALIZED
/// (TDZ) — `for (let x of [x])` throws ReferenceError, and closures created
/// in the head capture that env forever (scope-head-lex-open/close). The env
/// is left before GetIterator runs (step 5 restores oldEnv).
fn emit_for_head_expr(
  e: Emitter,
  left: ast.ForInit,
  right: ast.Expression,
) -> Result(Emitter, EmitError) {
  case for_init_lex_names(left) {
    [] -> emit_expr(e, right)
    names -> {
      let e = emit_op(e, EnterScope(BlockScope))
      let e = list.fold(names, e, fn(e, name) { declare_lex(e, name, False) })
      use e <- result.map(emit_expr(e, right))
      emit_op(e, LeaveScope)
    }
  }
}

/// Emit a for-of loop: `for (lhs of rhs) body`
///
/// Per ES2024 §14.7.5.6 ForIn/OfBodyEvaluation, the iterator must be closed
/// (§7.4.11 IteratorClose) when the body completes abruptly via break or
/// throw, but NOT when `.next()` itself throws (step 6.a) and NOT on natural
/// exhaustion (done=true). A single F_body try frame routes all throws to
/// IteratorCloseThrow; the `.next()`-throw and exhausted cases are handled
/// by IteratorNext's [[Done]] tracking — it overwrites the iter slot with
/// JsUndefined on done OR abrupt, so IteratorCloseThrow no-ops on those.
///
/// Stack layout: after GetIterator the stack is [iter, ..base]. F_body
/// records depth = len(base)+1, so unwind_to_catch always restores to
/// [thrown, iter|undef, ..base] — exactly what IteratorCloseThrow expects.
fn emit_for_of(
  e: Emitter,
  left: ast.ForInit,
  right: ast.Expression,
  body: ast.Statement,
) -> Result(Emitter, EmitError) {
  let #(e, loop_start) = fresh_label(e)
  let #(e, loop_continue) = fresh_label(e)
  let #(e, exhausted) = fresh_label(e)
  let #(e, break_target) = fresh_label(e)
  let #(e, catch_body) = fresh_label(e)
  let #(e, end) = fresh_label(e)

  // Block scope for let/const
  let e = emit_op(e, EnterScope(BlockScope))

  // Evaluate the iterable, get its sync iterator → stack: [iter, ..base]
  use e <- result.try(emit_for_head_expr(e, left, right))
  let e = emit_ir(e, IrGetIterator)

  // F_body: catches throws from IteratorNext, the LHS bind, and the body.
  // IteratorNext writes JsUndefined into the iter slot on done/abrupt
  // ([[Done]] sentinel), so a `.next()` throw lands at catch_body with
  // [thrown, undef, ..] and IteratorCloseThrow no-ops.
  let e = emit_ir(e, IrPushTry(catch_body))

  // break must route through PopTry + IteratorClose; continue stays in the
  // loop (no close on continue per spec). The frame has cross_pop_try=1 and
  // has_iterator so a labeled break/continue/return that *crosses* (rather
  // than targets) this for-of also drops F_body and closes iter.
  // NB ordering: push_loop_iter must come AFTER the F_body PushTry above.
  let e = push_loop_iter(e, break_target, loop_continue)

  let e = emit_ir(e, IrLabel(loop_start))
  let e = emit_ir(e, IrIteratorNext)
  // stack: [done, value, iter|undef, ..base], try=[F_body]
  let e = emit_ir(e, IrJumpIfTrue(exhausted))

  // stack: [value, iter, ..base] — bind consumes the value → [iter, ..base]
  use e <- result.try(emit_for_lhs_bind(e, left))
  let saved_annexb = e
  let e = annexb_head_ctx(e, for_init_lex_names(left))
  use e <- result.try(emit_stmt(e, body))
  let e = restore_annexb_ctx(e, saved_annexb)

  let e = emit_ir(e, IrLabel(loop_continue))
  let e = emit_ir(e, IrJump(loop_start))

  // catch_body: bind/body threw (real iter) OR .next() threw (undef
  // sentinel). IteratorCloseThrow calls .return() iff iter is an object,
  // then rethrows the original — never falls through.
  let e = emit_ir(e, IrLabel(catch_body))
  let e = emit_ir(e, IrIteratorCloseThrow)

  // exhausted: done=true, stack=[value, undef, ..base], try=[F_body, ..].
  // Spec does NOT close on natural exhaustion — slot is already undef so
  // a hypothetical close would no-op anyway, but we just drop and exit.
  let e = emit_ir(e, IrLabel(exhausted))
  let e = emit_ir(e, IrPop)
  let e = emit_ir(e, IrPopTry)
  let e = emit_ir(e, IrPop)
  let e = emit_ir(e, IrJump(end))

  // break_target: stack=[iter, ..base], try=[F_body, ..].
  // Normal-completion close: propagates errors from .return() and
  // TypeErrors on non-object return values per §7.4.11.
  let e = emit_ir(e, IrLabel(break_target))
  let e = emit_ir(e, IrPopTry)
  let e = emit_ir(e, IrIteratorClose)
  let e = emit_ir(e, IrJump(end))

  let e = emit_ir(e, IrLabel(end))
  let e = pop_loop(e)
  let e = emit_op(e, LeaveScope)
  Ok(e)
}

/// Emit a for-await-of loop: `for await (lhs of rhs) body`
///
/// Same two-guard layout as `emit_for_of` (F_body around bind+body, F_next
/// around next/await/unwrap so a `.next()` failure does NOT close — §14.7.5.6
/// step 6.a-f), but the close paths follow AsyncIteratorClose §7.4.12: the
/// `.return()` result is *awaited* before the object check (normal completion)
/// or before the original error is rethrown (throw completion). Because Await
/// suspends the frame, close cannot be a single opcode — it is open-coded as a
/// bytecode sequence here.
///
/// Stack base after GetAsyncIterator is [iter, ..base] (call its length B+1).
/// F_body and F_next both record depth B+1 so unwind_to_catch always lands on
/// [thrown, iter, ..base].
///
/// Throw-path padding: before the throw-close Await we push a JsUndefined slot
/// under the awaited value so the saved stack on suspend has length B+2 — the
/// same as F_swallow's recorded depth. That way every entry into `rethrow`
/// (getter throw, call throw, await reject, nullish skip, or success) arrives
/// with exactly three values above ..base and `Pop;Pop;Throw` always rethrows
/// the original.
fn emit_for_await_of(
  e: Emitter,
  left: ast.ForInit,
  right: ast.Expression,
  body: ast.Statement,
) -> Result(Emitter, EmitError) {
  let #(e, loop_start) = fresh_label(e)
  let #(e, loop_continue) = fresh_label(e)
  let #(e, exhausted) = fresh_label(e)
  let #(e, break_target) = fresh_label(e)
  let #(e, catch_next) = fresh_label(e)
  let #(e, catch_body) = fresh_label(e)
  let #(e, no_ret_thr) = fresh_label(e)
  let #(e, rethrow) = fresh_label(e)
  let #(e, no_ret_brk) = fresh_label(e)
  let #(e, end) = fresh_label(e)

  let e = emit_op(e, EnterScope(BlockScope))

  use e <- result.try(emit_for_head_expr(e, left, right))
  let e = emit_ir(e, IrGetAsyncIterator)
  // stack: [iter, ..base]. F_body recorded depth captures iter on top.
  let e = emit_ir(e, IrPushTry(catch_body))
  // push_loop_iter so a labeled break/continue *crossing* this loop pops
  // F_body and the iterator (sync close — see known async-crossing gap below).
  let e = push_loop_iter(e, break_target, loop_continue)

  let e = emit_ir(e, IrLabel(loop_start))
  // F_next shadows F_body for the next/await/unwrap region. Same recorded
  // depth (stack is [iter, ..base]). §14.7.5.6 step 6.a-f: errors here must
  // NOT close the iterator.
  let e = emit_ir(e, IrPushTry(catch_next))
  let e = emit_ir(e, IrDup)
  let e = emit_ir(e, IrGetField2("next"))
  let e = emit_ir(e, IrCallMethod("next", 0))
  let e = emit_ir(e, IrAwait)
  // §14.7.5.6 step 6.c: awaited next() result must be an Object. Covered by
  // F_next so a non-object correctly does NOT trigger close.
  let e = emit_ir(e, IrIteratorCheckObject)
  // [result_obj, iter, ..base]
  let e = emit_ir(e, IrDup)
  let e = emit_ir(e, IrGetField("done"))
  let e = emit_ir(e, IrJumpIfTrue(exhausted))
  let e = emit_ir(e, IrGetField("value"))
  let e = emit_ir(e, IrPopTry)
  // [value, iter, ..base], try=[F_body, ..]
  use e <- result.try(emit_for_lhs_bind(e, left))
  use e <- result.try(emit_stmt(e, body))
  let e = emit_ir(e, IrLabel(loop_continue))
  let e = emit_ir(e, IrJump(loop_start))

  // catch_next: next()/Await(next)/.done/.value failed — do NOT close.
  // unwind: depth=B+1 → [thrown, iter, ..base], try=[F_body, ..].
  let e = emit_ir(e, IrLabel(catch_next))
  let e = emit_ir(e, IrPopTry)
  let e = emit_ir(e, IrSwap)
  let e = emit_ir(e, IrPop)
  let e = emit_ir(e, IrThrow)

  // catch_body: bind/body threw — §7.4.12 throw-completion close.
  // [thrown, iter, ..base], try=[..outer]. F_swallow recorded at depth B+2
  // catches every error from get/call/await; original error always wins.
  let e = emit_ir(e, IrLabel(catch_body))
  let e = emit_ir(e, IrPushTry(rethrow))
  let e = emit_ir(e, IrSwap)
  // [iter, thrown, ..base]
  let e = emit_ir(e, IrGetField2("return"))
  let e = emit_ir(e, IrDup)
  let e = emit_ir(e, IrJumpIfNullish(no_ret_thr))
  // [ret_fn, iter, thrown, ..base]
  let e = emit_ir(e, IrCallMethod("return", 0))
  // [result, thrown, ..base] — pad so saved_stack after the Await pop has
  // length B+2 (== F_swallow depth). Reject-unwind then lands at
  // [inner_err, undef, thrown, ..base] = B+3, matching every other path.
  let e = push_const(e, JsUndefined)
  let e = emit_ir(e, IrSwap)
  // [result, undef, thrown, ..base]
  let e = emit_ir(e, IrAwait)
  // fulfilled → [awaited, undef, thrown, ..base]
  let e = emit_ir(e, IrPopTry)
  let e = emit_ir(e, IrJump(rethrow))

  let e = emit_ir(e, IrLabel(no_ret_thr))
  // [ret(nullish), iter, thrown, ..base], try=[F_swallow, ..]
  let e = emit_ir(e, IrPopTry)
  // fall through

  // rethrow: F_swallow catch-target AND merge point. Stack is always
  // [_, _, thrown, ..base] (slot0 = inner_err|awaited|nullish-ret;
  // slot1 = iter|undef). Spec: original throw completion wins, no object
  // check on the throw path.
  let e = emit_ir(e, IrLabel(rethrow))
  let e = emit_ir(e, IrPop)
  let e = emit_ir(e, IrPop)
  let e = emit_ir(e, IrThrow)

  // exhausted: done=true. [result_obj, iter, ..base], try=[F_next, F_body, ..].
  // Spec does NOT close on natural exhaustion.
  let e = emit_ir(e, IrLabel(exhausted))
  let e = emit_ir(e, IrPop)
  let e = emit_ir(e, IrPopTry)
  let e = emit_ir(e, IrPopTry)
  let e = emit_ir(e, IrPop)
  let e = emit_ir(e, IrJump(end))

  // break_target: [iter, ..base], try=[F_body, ..]. §7.4.12 normal-completion
  // close — every error (getter throw, non-callable, call throw, await reject,
  // non-object result) propagates to the enclosing handler.
  let e = emit_ir(e, IrLabel(break_target))
  let e = emit_ir(e, IrPopTry)
  let e = emit_ir(e, IrGetField2("return"))
  let e = emit_ir(e, IrDup)
  let e = emit_ir(e, IrJumpIfNullish(no_ret_brk))
  let e = emit_ir(e, IrCallMethod("return", 0))
  let e = emit_ir(e, IrAwait)
  let e = emit_ir(e, IrIteratorCheckObject)
  let e = emit_ir(e, IrPop)
  let e = emit_ir(e, IrJump(end))

  let e = emit_ir(e, IrLabel(no_ret_brk))
  // [ret(nullish), iter, ..base] — no await per §7.4.12 step 5.b.
  let e = emit_ir(e, IrPop)
  let e = emit_ir(e, IrPop)
  let e = emit_ir(e, IrJump(end))

  let e = emit_ir(e, IrLabel(end))
  let e = pop_loop(e)
  let e = emit_op(e, LeaveScope)
  Ok(e)
}

/// Bind the current value (on top of stack) to the for-in/for-of LHS.
/// The LHS can be:
///   - ForInitDeclaration(VariableDeclaration(...)) e.g. `for (let x ...)`
///   - ForInitExpression(expr) e.g. `for (x ...)`, `for (obj.k ...)`,
///     `for ([a,b] ...)`, `for ({a} ...)` — destructuring-assign semantics
///   - ForInitPattern(pattern) e.g. catch-param-style binding pattern
/// Consumes the value on top of stack.
fn emit_for_lhs_bind(
  e: Emitter,
  left: ast.ForInit,
) -> Result(Emitter, EmitError) {
  case left {
    ast.ForInitDeclaration(ast.VariableDeclaration(kind, declarators)) -> {
      let binding_kind = case kind {
        ast.Var -> VarBinding
        ast.Let -> LetBinding
        // Using/AwaitUsing heads are rewritten before emission (see the
        // using desugar) — Const defensively.
        ast.Const | ast.Using | ast.AwaitUsing -> ConstBinding
      }
      case declarators {
        [ast.VariableDeclarator(pattern, _)] ->
          emit_destructuring_bind(e, pattern, binding_kind)
        _ -> Error(Unsupported("for-in/of with multiple declarators"))
      }
    }
    ast.ForInitDeclaration(_) -> Error(Unsupported("for-in/of left-hand side"))
    ast.ForInitPattern(pattern) ->
      emit_destructuring_bind(e, pattern, VarBinding)
    // Assignment-target LHS: `for (x of …)`, `for (obj.k of …)`,
    // `for ([a,b] of …)`, `for ({a} of …)`. Destructuring-assign semantics
    // (PutVar/PutField/PutElem, no declaration).
    ast.ForInitExpression(expr) ->
      emit_destructuring_assign(e, unwrap_parens(expr))
  }
}

// ============================================================================
// Destructuring patterns
// ============================================================================

/// Emit code to destructure a value on top of stack into a pattern.
/// Consumes the value (pops it when done).
fn emit_destructuring_bind(
  e: Emitter,
  pattern: ast.Pattern,
  binding_kind: BindingKind,
) -> Result(Emitter, EmitError) {
  case pattern {
    ast.IdentifierPattern(name) -> {
      let e = case binding_kind {
        LetBinding -> declare_lex(e, name, False)
        ConstBinding -> declare_lex(e, name, True)
        ParamBinding | CatchBinding ->
          emit_op(e, DeclareVar(name, binding_kind))
        // FnNameBinding never reaches destructuring — it is declared
        // directly in compile_function_body, never via a pattern.
        VarBinding | CaptureBinding | FnNameBinding -> e
      }
      case binding_kind {
        LetBinding | ConstBinding -> Ok(init_lex(e, name))
        VarBinding
        | ParamBinding
        | CatchBinding
        | CaptureBinding
        | FnNameBinding -> Ok(emit_ir(e, IrScopePutVar(name)))
      }
    }

    ast.ObjectPattern(properties) ->
      emit_object_destructure(e, properties, binding_kind)

    ast.ArrayPattern(elements) ->
      emit_array_destructure(e, elements, binding_kind)

    ast.AssignmentPattern(left, default_expr) -> {
      // Check if value === undefined, if so use default
      let #(e, has_val) = fresh_label(e)
      let e = emit_ir(e, IrDup)
      let e = push_const(e, JsUndefined)
      let e = emit_ir(e, IrBinOp(opcode.StrictEq))
      let e = emit_ir(e, IrJumpIfFalse(has_val))
      // Value is undefined — pop it and use default
      let e = emit_ir(e, IrPop)
      use e <- result.try(case left {
        ast.IdentifierPattern(name) -> emit_named_expr(e, default_expr, name)
        _ -> emit_expr(e, default_expr)
      })
      let e = emit_ir(e, IrLabel(has_val))
      // Now the value (original or default) is on stack
      emit_destructuring_bind(e, left, binding_kind)
    }

    ast.RestElement(argument:) -> {
      // Rest element outside array pattern — treat as identity bind
      emit_destructuring_bind(e, argument, binding_kind)
    }
  }
}

/// Destructure an object — §13.15.5.2 ObjectBindingPattern.
///
/// Stack invariant maintained by emit_object_props:
///   [src, key_n, ..., key_1, ...]
/// where key_1..key_n are excluded-key JsValues stashed BENEATH src, accumulated
/// only when a RestProperty is present (so ObjectRestCopy can filter them out).
/// When no rest: no key stash, src is popped at the end.
fn emit_object_destructure(
  e: Emitter,
  properties: List(ast.PatternProperty),
  binding_kind: BindingKind,
) -> Result(Emitter, EmitError) {
  // §8.6.2 BindingInitialization ObjectBindingPattern step 1:
  // RequireObjectCoercible(value) BEFORE any property keys are evaluated —
  // `({} = null)` and `function f({}) {}; f(null)` must throw TypeError even
  // with an empty pattern. ToObject throws on exactly null/undefined; the
  // wrapper it creates for other primitives is observably equivalent for
  // the property reads that follow.
  let e = emit_ir(e, opcode.IrToObject)
  let has_rest =
    list.any(properties, fn(p) {
      case p {
        ast.RestProperty(_) -> True
        ast.PatternProperty(..) -> False
      }
    })
  use #(e, _n_excl) <- result.map(emit_object_props(
    e,
    properties,
    binding_kind,
    has_rest,
    0,
  ))
  // RestProperty branch issues ObjectRestCopy which consumes src + all keys.
  // Otherwise src is still on top — drop it.
  case has_rest {
    True -> e
    False -> emit_ir(e, IrPop)
  }
}

fn emit_object_props(
  e: Emitter,
  properties: List(ast.PatternProperty),
  binding_kind: BindingKind,
  has_rest: Bool,
  n_excl: Int,
) -> Result(#(Emitter, Int), EmitError) {
  case properties {
    [] -> Ok(#(e, n_excl))
    [prop, ..rest] -> {
      use #(e, n_excl) <- result.try(emit_single_object_prop(
        e,
        prop,
        binding_kind,
        has_rest,
        n_excl,
      ))
      emit_object_props(e, rest, binding_kind, has_rest, n_excl)
    }
  }
}

/// Emit one property of an ObjectPattern. Entry stack: [src, ...keys].
/// Exit stack: [src, key', ...keys] when has_rest (key stashed beneath src),
/// or [src, ...keys] otherwise. RestProperty exits with [] (consumed everything).
fn emit_single_object_prop(
  e: Emitter,
  prop: ast.PatternProperty,
  binding_kind: BindingKind,
  has_rest: Bool,
  n_excl: Int,
) -> Result(#(Emitter, Int), EmitError) {
  case prop {
    // Static identifier/string key: {a: pat} or {"a": pat}
    ast.PatternProperty(key: ast.Identifier(name), value:, computed: False, ..)
    | ast.PatternProperty(
        key: ast.StringExpression(name),
        value:,
        computed: False,
        ..,
      ) -> {
      // [src,..] → Dup → [src,src,..] → GetField → [val,src,..] → bind → [src,..]
      let e = emit_ir(e, IrDup)
      let e = emit_ir(e, IrGetField(name))
      use e <- result.map(emit_destructuring_bind(e, value, binding_kind))
      // Stash key string beneath src for later exclusion.
      case has_rest {
        False -> #(e, n_excl)
        True -> {
          let e = push_const(e, JsString(name))
          #(emit_ir(e, IrSwap), n_excl + 1)
        }
      }
    }

    // Numeric literal key: {1: pat} — not flagged computed by parser, but
    // needs runtime ToPropertyKey for canonical "1" (mirrors object-literal
    // numeric-key path at line ~2500).
    ast.PatternProperty(key: ast.NumberLiteral(n), value:, computed: False, ..) ->
      emit_computed_key_prop(
        e,
        fn(e) { Ok(push_const(e, JsNumber(Finite(n)))) },
        value,
        binding_kind,
        has_rest,
        n_excl,
      )

    // Computed key {[expr]: pat}, plus any remaining non-computed literal
    // key (e.g. BigInt) — evaluate the key expression and route via GetElem.
    ast.PatternProperty(key:, value:, ..) ->
      emit_computed_key_prop(
        e,
        emit_expr(_, key),
        value,
        binding_kind,
        has_rest,
        n_excl,
      )

    // {a, b, ...rest} — §13.15.5.3 RestBindingInitialization.
    // Stack: [src, key_n,..,key_1] → ObjectRestCopy(n) → [rest_obj] → bind.
    ast.RestProperty(argument) -> {
      let e = emit_ir(e, IrObjectRestCopy(n_excl))
      use e <- result.map(emit_destructuring_bind(e, argument, binding_kind))
      #(e, 0)
    }
  }
}

/// Shared path for computed and numeric-literal keys in object patterns.
/// `emit_key` pushes the key value onto the stack (returning Result(Emitter)).
///
/// Without rest: [src] → Dup → [src,src] → key → [k,src,src] → GetElem
///   → [val,src] → bind → [src].
/// With rest: uses GetElem2 to keep the evaluated key for the exclusion set
///   (single evaluation per §13.15.5.2 step 2): [src,..keys] → Dup →
///   [src,src,..] → key → [k,src,src,..] → GetElem2 → [val,k,src,src,..]
///   → bind → [k,src,src,..] → Swap;Pop;Swap → [src,k,..keys].
fn emit_computed_key_prop(
  e: Emitter,
  emit_key: fn(Emitter) -> Result(Emitter, EmitError),
  inner: ast.Pattern,
  binding_kind: BindingKind,
  has_rest: Bool,
  n_excl: Int,
) -> Result(#(Emitter, Int), EmitError) {
  let e = emit_ir(e, IrDup)
  use e <- result.try(emit_key(e))
  case has_rest {
    False -> {
      let e = emit_ir(e, IrGetElem)
      use e <- result.map(emit_destructuring_bind(e, inner, binding_kind))
      #(e, n_excl)
    }
    True -> {
      let e = emit_ir(e, IrGetElem2)
      use e <- result.map(emit_destructuring_bind(e, inner, binding_kind))
      // [k,src,src,..] → swap → [src,k,src,..] → pop → [k,src,..]
      // → swap → [src,k,..]
      let e = emit_ir(e, IrSwap)
      let e = emit_ir(e, IrPop)
      #(emit_ir(e, IrSwap), n_excl + 1)
    }
  }
}

// ----------------------------------------------------------------------------
// Destructuring ASSIGNMENT (§13.15.5) — `[a, x.y] = rhs`, `({k: v} = rhs)`.
// Unlike emit_destructuring_bind (binding patterns over ast.Pattern), targets
// here are LHS *expressions* and may be MemberExpressions. Value to assign is
// on top of stack on entry; consumed on exit.
//
// Mirrors QuickJS js_parse_destructuring_element with tok==0: leaves route
// through put_lvalue (PutVar/PutField/PutElem) instead of DeclareVar.
// ----------------------------------------------------------------------------

fn emit_destructuring_assign(
  e: Emitter,
  target: ast.Expression,
) -> Result(Emitter, EmitError) {
  case target {
    ast.Identifier(name) -> Ok(emit_ir(e, IrScopePutVar(name)))

    ast.ParenthesizedExpression(inner) -> emit_destructuring_assign(e, inner)

    // obj.prop — stack [val] → eval obj → [obj,val] → swap → [val,obj]
    // → PutField → [val] → Pop. (PutField pops [value,obj], leaves value.)
    ast.MemberExpression(obj, ast.Identifier(prop), False) -> {
      use e <- result.map(emit_expr(e, obj))
      let e = emit_ir(e, IrSwap)
      let e = emit_put_field(e, prop)
      emit_ir(e, IrPop)
    }

    // Non-computed string-keyed member (defensive; parser usually emits
    // computed=True for obj["lit"]).
    ast.MemberExpression(obj, ast.StringExpression(prop), False) -> {
      use e <- result.map(emit_expr(e, obj))
      let e = emit_ir(e, IrSwap)
      let e = emit_ir(e, IrPutField(prop))
      emit_ir(e, IrPop)
    }

    // obj[key] — PutElem wants [val,key,obj]. With only Dup/Swap (no rot3):
    // [val] → emit obj → [obj,val] → swap → [val,obj] → emit key → [key,val,obj]
    // → swap → [val,key,obj] → PutElem → [val] → Pop.
    ast.MemberExpression(obj, key, True) -> {
      use e <- result.try(emit_expr(e, obj))
      let e = emit_ir(e, IrSwap)
      use e <- result.map(emit_expr(e, key))
      let e = emit_ir(e, IrSwap)
      let e = emit_ir(e, IrPutElem)
      emit_ir(e, IrPop)
    }

    // target = default  (AssignmentElement with Initializer, §13.15.5.3)
    // If incoming value === undefined, replace with default; then recurse.
    ast.AssignmentExpression(ast.Assign, left, default_expr) -> {
      let #(e, has_val) = fresh_label(e)
      let e = emit_ir(e, IrDup)
      let e = push_const(e, JsUndefined)
      let e = emit_ir(e, IrBinOp(opcode.StrictEq))
      let e = emit_ir(e, IrJumpIfFalse(has_val))
      let e = emit_ir(e, IrPop)
      use e <- result.try(case left {
        ast.Identifier(name) -> emit_named_expr(e, default_expr, name)
        _ -> emit_expr(e, default_expr)
      })
      let e = emit_ir(e, IrLabel(has_val))
      emit_destructuring_assign(e, left)
    }

    ast.ArrayExpression(elements) -> {
      use e, close_throw <- with_iterator_scaffold(e)
      emit_array_assign_elements(e, elements, close_throw)
    }

    ast.ObjectExpression(properties) -> {
      // §13.15.5.2 step 1: RequireObjectCoercible BEFORE evaluating any
      // property keys — `({} = null)` throws TypeError. See
      // emit_object_destructure for the ToObject rationale.
      let e = emit_ir(e, opcode.IrToObject)
      let has_rest =
        list.any(properties, fn(p) {
          case p {
            ast.SpreadProperty(_) -> True
            ast.Property(..) -> False
          }
        })
      use #(e, _n) <- result.map(emit_object_assign_props(
        e,
        properties,
        has_rest,
        0,
      ))
      case has_rest {
        True -> e
        False -> emit_ir(e, IrPop)
      }
    }

    // Annex B web-compat for-in/of LHS: `for (f() of [1])` — evaluate the
    // call each iteration, then throw ReferenceError before assigning.
    // Entry stack is [val]; the trailing IrPop is unreachable but keeps the
    // consumed-value invariant for static bookkeeping.
    ast.CallExpression(callee, args) -> {
      use e <- result.map(emit_expr(e, ast.CallExpression(callee, args)))
      let e = emit_ir(e, IrPop)
      let e =
        emit_ir(
          e,
          IrThrowError(
            opcode.ReferenceErrorKind,
            "Invalid left-hand side in assignment",
          ),
        )
      emit_ir(e, IrPop)
    }

    other ->
      Error(Unsupported(
        "destructuring assignment target: " <> string_inspect_expr_kind(other),
      ))
  }
}

/// Array assignment pattern elements. Clone of emit_array_elements but
/// recurses via emit_destructuring_assign on ast.Expression elements.
/// Stack invariant: [iter, ...] on entry. On exit: #(e, False) → [iter, ...]
/// (caller closes); #(e, True) → [...] (rest drained, F_body popped).
fn emit_array_assign_elements(
  e: Emitter,
  elements: List(Option(ast.Expression)),
  close_throw: Int,
) -> Result(#(Emitter, Bool), EmitError) {
  case elements {
    [] -> Ok(#(e, False))
    // Hole: step iterator, discard done+value.
    [None, ..rest] -> {
      let e = emit_ir(e, IrIteratorNext)
      let e = emit_ir(e, IrPop)
      let e = emit_ir(e, IrPop)
      emit_array_assign_elements(e, rest, close_throw)
    }
    [Some(ast.SpreadElement(argument:)), ..] ->
      emit_array_assign_rest(e, strip_parens(argument), close_throw)
    [Some(target), ..rest] -> {
      use e <- result.try(emit_array_assign_element(e, strip_parens(target)))
      emit_array_assign_elements(e, rest, close_throw)
    }
  }
}

/// One non-rest AssignmentElement (§13.15.5.3). Stack: [iter, ..] → [iter, ..].
///
/// For MemberExpression targets the lref (base obj + computed key) is
/// evaluated BEFORE IteratorStep — `[ {}[thrower()] ] = iterable` must throw
/// without calling .next(), and the still-armed close guard then
/// IteratorCloses ([[Done]] is false). After the step, Rot3/Unrot4 park iter
/// back at the guard's recorded depth so a PutValue throw closes the
/// iterator, not a stale operand.
fn emit_array_assign_element(
  e: Emitter,
  target: ast.Expression,
) -> Result(Emitter, EmitError) {
  case target {
    // obj.prop — [iter] → obj → [obj,iter] → Swap → [iter,obj]
    // → IteratorNext → [done,value,iter,obj] → Pop → [value,iter,obj]
    // → Rot3 → [obj,value,iter] → Swap → [value,obj,iter]
    // → PutField → [value,iter] → Pop → [iter].
    ast.MemberExpression(obj, ast.Identifier(prop), False) -> {
      use e <- result.map(emit_expr(e, obj))
      e
      |> emit_ir(IrSwap)
      |> emit_ir(IrIteratorNext)
      |> emit_ir(IrPop)
      |> emit_ir(opcode.IrRot3)
      |> emit_ir(IrSwap)
      |> emit_put_field(prop)
      |> emit_ir(IrPop)
    }
    ast.MemberExpression(obj, ast.StringExpression(prop), False) -> {
      use e <- result.map(emit_expr(e, obj))
      e
      |> emit_ir(IrSwap)
      |> emit_ir(IrIteratorNext)
      |> emit_ir(IrPop)
      |> emit_ir(opcode.IrRot3)
      |> emit_ir(IrSwap)
      |> emit_ir(IrPutField(prop))
      |> emit_ir(IrPop)
    }
    // obj[key] — the key's ToPropertyKey stays deferred to PutElem
    // (§13.15.5.6: PutValue runs after the iterator step). [iter] → obj
    // → key → [key,obj,iter] → Rot3 → [iter,key,obj] → IteratorNext → Pop
    // → [value,iter,key,obj] → Swap → [iter,value,key,obj] → Unrot4
    // → [value,key,obj,iter] → PutElem → [value,iter] → Pop → [iter].
    ast.MemberExpression(obj, key, True) -> {
      use e <- result.try(emit_expr(e, obj))
      use e <- result.map(emit_expr(e, key))
      e
      |> emit_ir(opcode.IrRot3)
      |> emit_ir(IrIteratorNext)
      |> emit_ir(IrPop)
      |> emit_ir(IrSwap)
      |> emit_ir(opcode.IrUnrot4)
      |> emit_ir(IrPutElem)
      |> emit_ir(IrPop)
    }
    // Identifiers, nested patterns, defaults: step first, then assign.
    _ -> {
      let e = emit_ir(e, IrIteratorNext)
      // [done, value, iter] — discard done, assign value.
      let e = emit_ir(e, IrPop)
      emit_destructuring_assign(e, target)
    }
  }
}

/// AssignmentRestElement (§13.15.5.5) — `[...target] = rhs`.
///
/// Spec order: when the rest target is NOT a nested array/object pattern its
/// Reference is evaluated BEFORE the iterator is drained — and a throw there
/// (e.g. `[...obj[throwingKey()]] = it`) must IteratorClose the still-open
/// iterator ([[Done]] is false). Draining first hangs forever on infinite
/// iterators (staging/sm/destructuring/array-iterator-close.js).
///
/// [[Done]] becomes true the moment draining starts → no close on any throw
/// after that (.next() errors or PutValue), so F_body is popped right before
/// IteratorRest.
fn emit_array_assign_rest(
  e: Emitter,
  target: ast.Expression,
  close_throw: Int,
) -> Result(#(Emitter, Bool), EmitError) {
  case target {
    // obj.prop — evaluate obj (under the F_body guard, above iter so a throw
    // unwinds to [thrown, iter, ..]), then drain, then PutField.
    // [iter] → [obj,iter] → Swap → [iter,obj] → PopTry → IteratorRest
    // → [arr,obj] → PutField → [arr] → Pop.
    ast.MemberExpression(obj, ast.Identifier(prop), False) -> {
      use e <- result.map(emit_expr(e, obj))
      let e = emit_ir(e, IrSwap)
      let e = emit_ir(e, IrPopTry)
      let e = emit_ir(e, IrIteratorRest)
      let e = emit_put_field(e, prop)
      #(emit_ir(e, IrPop), True)
    }
    ast.MemberExpression(obj, ast.StringExpression(prop), False) -> {
      use e <- result.map(emit_expr(e, obj))
      let e = emit_ir(e, IrSwap)
      let e = emit_ir(e, IrPopTry)
      let e = emit_ir(e, IrIteratorRest)
      let e = emit_ir(e, IrPutField(prop))
      #(emit_ir(e, IrPop), True)
    }
    // obj[key] — obj AND key evaluate before draining. The key may throw, so
    // after tucking obj beneath iter the original F_body frame's recorded
    // depth no longer matches — re-arm the close guard with iter back on top.
    // [iter] → [obj,iter] → PopTry → Swap → [iter,obj] → PushTry(close_throw)
    // → emit key → [key,iter,obj] → Swap → [iter,key,obj] → PopTry
    // → IteratorRest → [arr,key,obj] → PutElem → [arr] → Pop.
    ast.MemberExpression(obj, key, True) -> {
      use e <- result.try(emit_expr(e, obj))
      let e = emit_ir(e, IrPopTry)
      let e = emit_ir(e, IrSwap)
      let e = emit_ir(e, IrPushTry(close_throw))
      use e <- result.map(emit_expr(e, key))
      let e = emit_ir(e, IrSwap)
      let e = emit_ir(e, IrPopTry)
      let e = emit_ir(e, IrIteratorRest)
      let e = emit_ir(e, IrPutElem)
      #(emit_ir(e, IrPop), True)
    }
    // Identifier targets (no observable Reference side effects) and nested
    // array/object patterns (spec drains into A first, §13.15.5.5 step 4):
    // drain, then bind/destructure.
    other -> {
      // [iter], try=[F_body,..] → PopTry → IteratorRest → [arr]
      let e = emit_ir(e, IrPopTry)
      let e = emit_ir(e, IrIteratorRest)
      use e <- result.map(emit_destructuring_assign(e, other))
      #(e, True)
    }
  }
}

/// Unwrap `ast.ParenthesizedExpression` layers: `[...((a.b))] = x` → a.b.
fn strip_parens(expr: ast.Expression) -> ast.Expression {
  case expr {
    ast.ParenthesizedExpression(inner) -> strip_parens(inner)
    other -> other
  }
}

/// Object assignment pattern properties — §13.15.5.2. Same stack invariant
/// as emit_object_props (binding-pattern path): [src, key_n,..,key_1] with
/// excluded keys stashed beneath src when has_rest, else just [src].
fn emit_object_assign_props(
  e: Emitter,
  properties: List(ast.Property),
  has_rest: Bool,
  n_excl: Int,
) -> Result(#(Emitter, Int), EmitError) {
  case properties {
    [] -> Ok(#(e, n_excl))
    [prop, ..rest] -> {
      use #(e, n_excl) <- result.try(emit_single_object_assign_prop(
        e,
        prop,
        has_rest,
        n_excl,
      ))
      emit_object_assign_props(e, rest, has_rest, n_excl)
    }
  }
}

fn emit_single_object_assign_prop(
  e: Emitter,
  prop: ast.Property,
  has_rest: Bool,
  n_excl: Int,
) -> Result(#(Emitter, Int), EmitError) {
  case prop {
    // {key: target} or shorthand {key} (value==Identifier(key)).
    // Non-computed string/identifier/number key.
    ast.Property(
      key:,
      value:,
      computed: False,
      kind: ast.Init,
      method: False,
      ..,
    ) ->
      case object_prop_key_name(key) {
        Ok(name) -> {
          use e <- result.map(emit_keyed_destructure_assign(e, name, value))
          case has_rest {
            False -> #(e, n_excl)
            True -> {
              let e = push_const(e, JsString(name))
              #(emit_ir(e, IrSwap), n_excl + 1)
            }
          }
        }
        Error(Nil) -> {
          // Non-stringifiable static key — fall through to computed-key path.
          let e = emit_ir(e, IrDup)
          use e <- result.try(emit_expr(e, key))
          emit_elem_key_assign(e, value, has_rest, n_excl)
        }
      }

    // {[expr]: target} — Dup obj, eval key, GetElem, recurse.
    ast.Property(
      key:,
      value:,
      computed: True,
      kind: ast.Init,
      method: False,
      ..,
    ) -> {
      let e = emit_ir(e, IrDup)
      use e <- result.try(emit_expr(e, key))
      // §13.2.5.4 ComputedPropertyName: ToPropertyKey fires eagerly at
      // PropertyName evaluation — observably BEFORE the target reference
      // is evaluated (keyed-destructuring evaluation-order tests).
      let e = emit_ir(e, opcode.IrToPropertyKey)
      emit_elem_key_assign(e, value, has_rest, n_excl)
    }

    // Getter/setter/method properties never appear in valid assignment
    // patterns (parser sets has_invalid_pattern). Guard defensively.
    ast.Property(kind: ast.Get, ..)
    | ast.Property(kind: ast.Set, ..)
    | ast.Property(method: True, ..) ->
      Error(Unsupported("accessor/method in destructuring assignment"))

    // {a, b, ...target} — §13.15.5.4 RestDestructuringAssignmentEvaluation.
    // Stack: [src, key_n,..,key_1] → ObjectRestCopy(n) → [rest_obj] → assign.
    ast.SpreadProperty(argument) -> {
      let e = emit_ir(e, IrObjectRestCopy(n_excl))
      use e <- result.map(emit_destructuring_assign(e, argument))
      #(e, 0)
    }
  }
}

/// `{name: target}` — read src[name] and assign to target. Entry/exit: [src].
/// §13.15.5.6 KeyedDestructuringAssignmentEvaluation: when target is a
/// non-pattern (Identifier/MemberExpression), lref is evaluated (step 1a)
/// BEFORE GetV (step 2). For obj.prop / obj.#priv targets we therefore emit
/// the base obj first so its TDZ/side-effects fire ahead of the source read —
/// e.g. `({a: this.#f} = o)` in a derived ctor before super() must throw on
/// `this`, never reach o.a's getter (privatefieldset-evaluation-order-1).
/// PutValue itself (the #priv presence check) still happens last, so a getter
/// that installs #f on the already-evaluated base is observed (-order-3).
fn emit_keyed_destructure_assign(
  e: Emitter,
  name: String,
  target: ast.Expression,
) -> Result(Emitter, EmitError) {
  case unwrap_parens(target) {
    // [src] → Dup → [src,src] → obj → [obj,src,src] → Swap → [src,obj,src]
    // → GetField → [v,obj,src] → Put → [v,src] → Pop → [src].
    ast.MemberExpression(obj, ast.Identifier(prop), False) -> {
      let e = emit_ir(e, IrDup)
      use e <- result.map(emit_expr(e, obj))
      e
      |> emit_ir(IrSwap)
      |> emit_ir(IrGetField(name))
      |> emit_put_field(prop)
      |> emit_ir(IrPop)
    }
    ast.MemberExpression(obj, ast.StringExpression(prop), False) -> {
      let e = emit_ir(e, IrDup)
      use e <- result.map(emit_expr(e, obj))
      e
      |> emit_ir(IrSwap)
      |> emit_ir(IrGetField(name))
      |> emit_ir(IrPutField(prop))
      |> emit_ir(IrPop)
    }
    // obj[key]: [src] → Dup → [src,src] → obj → [obj,src,src] → Swap →
    // [src,obj,src] → key → [key,src,obj,src] → Swap → [src,key,obj,src] →
    // GetField → [v,key,obj,src] → PutElem → [v,src] → Pop → [src].
    // base→key→GetV order matches §13.3.2.1 + §13.15.5.6 step 1a.
    ast.MemberExpression(obj, key, True) -> {
      let e = emit_ir(e, IrDup)
      use e <- result.try(emit_expr(e, obj))
      let e = emit_ir(e, IrSwap)
      use e <- result.map(emit_expr(e, key))
      e
      |> emit_ir(IrSwap)
      |> emit_ir(IrGetField(name))
      |> emit_ir(IrPutElem)
      |> emit_ir(IrPop)
    }
    // Identifier / nested pattern / default targets keep GetV-first — that's
    // spec-correct: step 1 only applies when target is NOT a pattern, and
    // Identifier lref evaluation has no observable effect ahead of GetV.
    _ -> {
      let e = emit_ir(e, IrDup)
      let e = emit_ir(e, IrGetField(name))
      emit_destructuring_assign(e, target)
    }
  }
}

/// Computed-key portion shared by assignment-pattern path. Entry stack:
/// [key, src, src, ...keys] (after caller's Dup + emit key). See
/// emit_computed_key_prop for stack-shuffle rationale.
fn emit_elem_key_assign(
  e: Emitter,
  value: ast.Expression,
  has_rest: Bool,
  n_excl: Int,
) -> Result(#(Emitter, Int), EmitError) {
  case has_rest {
    False -> {
      use e <- result.map(emit_elem_keyed_target(e, unwrap_parens(value)))
      #(e, n_excl)
    }
    True -> {
      let e = emit_ir(e, IrGetElem2)
      use e <- result.map(emit_destructuring_assign(e, value))
      let e = emit_ir(e, IrSwap)
      let e = emit_ir(e, IrPop)
      #(emit_ir(e, IrSwap), n_excl + 1)
    }
  }
}

/// `{[srcKey]: target}` body after the key is on the stack —
/// §13.15.5.6 KeyedDestructuringAssignmentEvaluation: for non-pattern
/// targets the lref (base obj + computed key) is evaluated (step 1a)
/// BEFORE GetV(src, srcKey) (step 2); a default initializer runs after the
/// get; the target key's ToPropertyKey stays deferred to PutValue.
/// Entry stack: [key, src, src] (src already Dup'd). Exit: [src].
/// unrot3 (= Rot3 twice) tucks a just-pushed target operand beneath the
/// pending [key, src] pair so GetElem still sees them adjacent.
fn emit_elem_keyed_target(
  e: Emitter,
  target: ast.Expression,
) -> Result(Emitter, EmitError) {
  case target {
    // [key,srcd,src] → tobj → [tobj,key,srcd,src] → unrot3
    // → [key,srcd,tobj,src] → GetElem → [v,tobj,src]
    // → PutField → [v,src] → Pop → [src].
    ast.MemberExpression(tobj, ast.Identifier(prop), False) -> {
      use e <- result.map(emit_expr(e, tobj))
      e
      |> emit_unrot3
      |> emit_ir(IrGetElem)
      |> emit_put_field(prop)
      |> emit_ir(IrPop)
    }
    ast.MemberExpression(tobj, ast.StringExpression(prop), False) -> {
      use e <- result.map(emit_expr(e, tobj))
      e
      |> emit_unrot3
      |> emit_ir(IrGetElem)
      |> emit_ir(IrPutField(prop))
      |> emit_ir(IrPop)
    }
    // [key,srcd,src] → tobj → unrot3 → [key,srcd,tobj,src] → tkey → unrot3
    // → [key,srcd,tkey,tobj,src] → GetElem → [v,tkey,tobj,src]
    // → PutElem → [v,src] → Pop → [src].
    ast.MemberExpression(tobj, tkey, True) -> {
      use e <- result.try(emit_expr(e, tobj))
      let e = emit_unrot3(e)
      use e <- result.map(emit_expr(e, tkey))
      e
      |> emit_unrot3
      |> emit_ir(IrGetElem)
      |> emit_ir(IrPutElem)
      |> emit_ir(IrPop)
    }
    // target = default with a member target: lref first, get, then the
    // default check, then PutValue.
    ast.AssignmentExpression(ast.Assign, left, default_expr) ->
      case unwrap_parens(left) {
        ast.MemberExpression(_, _, _) as member -> {
          use e <- result.try(emit_elem_keyed_member_default(
            e,
            member,
            default_expr,
          ))
          Ok(e)
        }
        _ -> {
          let e = emit_ir(e, IrGetElem)
          emit_destructuring_assign(
            e,
            ast.AssignmentExpression(ast.Assign, left, default_expr),
          )
        }
      }
    // Identifiers and nested patterns: get first (patterns skip step 1a;
    // identifier lrefs have no observable evaluation).
    other -> {
      let e = emit_ir(e, IrGetElem)
      emit_destructuring_assign(e, other)
    }
  }
}

/// `{[srcKey]: member = default}` — member lref, GetElem, default check,
/// PutValue. Entry: [key, src, src]; exit: [src].
fn emit_elem_keyed_member_default(
  e: Emitter,
  member: ast.Expression,
  default_expr: ast.Expression,
) -> Result(Emitter, EmitError) {
  // Evaluate the member lref and the source get, leaving [v, ..put operands].
  use #(e, computed) <- result.try(case member {
    ast.MemberExpression(tobj, ast.Identifier(_), False)
    | ast.MemberExpression(tobj, ast.StringExpression(_), False) -> {
      use e <- result.map(emit_expr(e, tobj))
      let e = emit_unrot3(e)
      #(emit_ir(e, IrGetElem), False)
    }
    ast.MemberExpression(tobj, tkey, True) -> {
      use e <- result.try(emit_expr(e, tobj))
      let e = emit_unrot3(e)
      use e <- result.map(emit_expr(e, tkey))
      let e = emit_unrot3(e)
      #(emit_ir(e, IrGetElem), True)
    }
    _ -> Error(Unsupported("keyed member default target"))
  })
  // Default check: if v === undefined, replace with default.
  let #(e, has_val) = fresh_label(e)
  let e = emit_ir(e, IrDup)
  let e = push_const(e, JsUndefined)
  let e = emit_ir(e, IrBinOp(opcode.StrictEq))
  let e = emit_ir(e, IrJumpIfFalse(has_val))
  let e = emit_ir(e, IrPop)
  use e <- result.map(emit_expr(e, default_expr))
  let e = emit_ir(e, IrLabel(has_val))
  let e = case computed, member {
    True, _ -> emit_ir(e, IrPutElem)
    False, ast.MemberExpression(_, ast.StringExpression(prop), False) ->
      emit_ir(e, IrPutField(prop))
    False, ast.MemberExpression(_, ast.Identifier(prop), False) ->
      emit_put_field(e, prop)
    _, _ -> e
  }
  emit_ir(e, IrPop)
}

/// [a, b, c, ..] → [b, c, a, ..] — send the top element down two (Rot3
/// applied twice). Tucks a freshly-evaluated operand beneath two pending
/// stack slots.
fn emit_unrot3(e: Emitter) -> Emitter {
  e
  |> emit_ir(opcode.IrRot3)
  |> emit_ir(opcode.IrRot3)
}

/// Static property-key → string for non-computed object pattern keys.
/// Numeric keys go through js_format_number for canonical form ("1" not "1.0").
fn object_prop_key_name(key: ast.Expression) -> Result(String, Nil) {
  case key {
    ast.Identifier(name) -> Ok(name)
    ast.StringExpression(name) -> Ok(name)
    ast.NumberLiteral(n) -> Ok(value.js_format_number(n))
    _ -> Error(Nil)
  }
}

/// Destructure an array via the iterator protocol (§14.3.3.6).
/// GetIterator on source, IteratorNext per element, bind each value.
/// Stack on entry: [source, ...] — source is consumed.
fn emit_array_destructure(
  e: Emitter,
  elements: List(Option(ast.Pattern)),
  binding_kind: BindingKind,
) -> Result(Emitter, EmitError) {
  use e, _close_throw <- with_iterator_scaffold(e)
  emit_array_elements(e, elements, binding_kind)
}

/// Iterator scaffold shared by array binding patterns (§8.6.2) and array
/// assignment patterns (§13.15.5.2): GetIterator the source, run
/// emit_elements under a close-on-throw guard, IteratorClose on normal
/// completion unless a rest element already drained the iterator.
/// Stack on entry: [source, ...] — source is consumed.
/// emit_elements: [iter, ...] → #(e, False) leaves [iter, ...] (closed here);
/// #(e, True) → rest drained, F_body popped, stack at base.
/// The close_throw label is passed to emit_elements so a rest element that
/// must evaluate a throwing member-target reference mid-pattern can re-arm
/// the close-on-throw guard (see emit_array_assign_rest).
fn with_iterator_scaffold(
  e: Emitter,
  emit_elements: fn(Emitter, Int) -> Result(#(Emitter, Bool), EmitError),
) -> Result(Emitter, EmitError) {
  let #(e, close_throw) = fresh_label(e)
  let #(e, done_label) = fresh_label(e)
  // [source] → [iter]. PushTry immediately after so the recorded stack_depth
  // has iter on top — unwind_to_catch will leave [thrown, iter, ..].
  let e = emit_ir(e, IrGetIterator)
  let e = emit_ir(e, IrPushTry(close_throw))
  use #(e, rested) <- result.map(emit_elements(e, close_throw))
  let e = case rested {
    // Rest path already PopTry'd, drained iter, bound it. Stack at base.
    // [[Done]]=true after rest → no close on any completion. (Throws from
    // non-rest elements BEFORE the rest happened while F_body was still
    // active, so the catch target below is still reachable for those.)
    True -> emit_ir(e, IrJump(done_label))
    False ->
      // If [[Done]] is false, IteratorClose. We don't track [[Done]] yet
      // (IteratorNext doesn't undef the slot), so this also fires on
      // exhausted iterators — a no-op for builtins, observable only on user
      // iters with .return.
      e
      |> emit_ir(IrPopTry)
      |> emit_ir(IrIteratorClose)
      |> emit_ir(IrJump(done_label))
  }
  // Abrupt: [thrown, iter, ..]. CloseThrow swallows .return() result/error
  // and rethrows the original (§7.4.11 step 5).
  let e = emit_ir(e, IrLabel(close_throw))
  let e = emit_ir(e, IrIteratorCloseThrow)
  emit_ir(e, IrLabel(done_label))
}

/// Stack invariant: [iter, ...] on entry. On exit: #(e, False) → [iter, ...]
/// (caller closes); #(e, True) → [...] (rest drained, F_body popped, no close).
fn emit_array_elements(
  e: Emitter,
  elements: List(Option(ast.Pattern)),
  binding_kind: BindingKind,
) -> Result(#(Emitter, Bool), EmitError) {
  case elements {
    [] -> Ok(#(e, False))
    // Hole: step iterator, discard value.
    [None, ..rest] -> {
      let e = emit_ir(e, IrIteratorNext)
      // [done, value, iter] → [iter]
      let e = emit_ir(e, IrPop)
      let e = emit_ir(e, IrPop)
      emit_array_elements(e, rest, binding_kind)
    }
    // Rest: [[Done]] becomes true the moment we start draining → no close on
    // ANY subsequent throw (.next() error or rest-target bind). Pop F_body
    // first so those throws propagate directly past the close guard.
    [Some(ast.RestElement(argument:)), ..] -> {
      // [iter], try=[F_body,..] → PopTry → IteratorRest → [arr]
      let e = emit_ir(e, IrPopTry)
      let e = emit_ir(e, IrIteratorRest)
      use e <- result.map(emit_destructuring_bind(e, argument, binding_kind))
      #(e, True)
    }
    [Some(pattern), ..rest] -> {
      let e = emit_ir(e, IrIteratorNext)
      // [done, value, iter] — discard done, bind value.
      let e = emit_ir(e, IrPop)
      use e <- result.try(emit_destructuring_bind(e, pattern, binding_kind))
      emit_array_elements(e, rest, binding_kind)
    }
  }
}

// ============================================================================
// Operator translation
// ============================================================================

fn translate_binop(op: ast.BinaryOp) -> opcode.BinOpKind {
  case op {
    ast.Add -> opcode.Add
    ast.Subtract -> opcode.Sub
    ast.Multiply -> opcode.Mul
    ast.Divide -> opcode.Div
    ast.Modulo -> opcode.Mod
    ast.Exponentiation -> opcode.Exp
    ast.StrictEqual -> opcode.StrictEq
    ast.StrictNotEqual -> opcode.StrictNotEq
    ast.Equal -> opcode.Eq
    ast.NotEqual -> opcode.NotEq
    ast.LessThan -> opcode.Lt
    ast.GreaterThan -> opcode.Gt
    ast.LessThanEqual -> opcode.LtEq
    ast.GreaterThanEqual -> opcode.GtEq
    ast.LeftShift -> opcode.ShiftLeft
    ast.RightShift -> opcode.ShiftRight
    ast.UnsignedRightShift -> opcode.UShiftRight
    ast.BitwiseAnd -> opcode.BitAnd
    ast.BitwiseOr -> opcode.BitOr
    ast.BitwiseXor -> opcode.BitXor
    ast.In -> opcode.In
    ast.InstanceOf -> opcode.InstanceOf
    // Logical ops should not reach here (handled separately)
    ast.LogicalAnd | ast.LogicalOr | ast.NullishCoalescing -> opcode.Add
  }
}

fn translate_unaryop(op: ast.UnaryOp) -> opcode.UnaryOpKind {
  case op {
    ast.Negate -> opcode.Neg
    ast.UnaryPlus -> opcode.Pos
    ast.LogicalNot -> opcode.LogicalNot
    ast.BitwiseNot -> opcode.BitNot
    ast.Void -> opcode.Void
    // TypeOf handled separately, Delete not in MVP
    ast.TypeOf -> opcode.Void
    ast.Delete -> opcode.Void
  }
}

fn compound_to_binop(op: ast.AssignmentOp) -> Result(opcode.BinOpKind, Nil) {
  case op {
    ast.AddAssign -> Ok(opcode.Add)
    ast.SubtractAssign -> Ok(opcode.Sub)
    ast.MultiplyAssign -> Ok(opcode.Mul)
    ast.DivideAssign -> Ok(opcode.Div)
    ast.ModuloAssign -> Ok(opcode.Mod)
    ast.ExponentiationAssign -> Ok(opcode.Exp)
    ast.LeftShiftAssign -> Ok(opcode.ShiftLeft)
    ast.RightShiftAssign -> Ok(opcode.ShiftRight)
    ast.UnsignedRightShiftAssign -> Ok(opcode.UShiftRight)
    ast.BitwiseAndAssign -> Ok(opcode.BitAnd)
    ast.BitwiseOrAssign -> Ok(opcode.BitOr)
    ast.BitwiseXorAssign -> Ok(opcode.BitXor)
    ast.Assign -> Error(Nil)
    ast.LogicalAndAssign | ast.LogicalOrAssign | ast.NullishCoalesceAssign ->
      Error(Nil)
  }
}

/// §13.15.2 AssignmentExpression : LHS &&= / ||= / ??= AssignmentExpression.
/// The reference is evaluated once and read once; when the short-circuit test
/// fails the RHS is never evaluated and no write happens — the old value is
/// the expression result. Identifier targets get NamedEvaluation (anonymous
/// fn/class RHS is named after the variable), same as plain `=`.
fn emit_logical_assign(
  e: Emitter,
  op: ast.AssignmentOp,
  lhs: ast.Expression,
  right: ast.Expression,
) -> Result(Emitter, EmitError) {
  case lhs {
    ast.Identifier(name) -> {
      let #(e, end_label) = fresh_label(e)
      // §13.15.2: the reference is resolved once, before the old-value read;
      // the (conditional) store reuses that base.
      let e = emit_ir(e, opcode.IrScopeMakeRef(name))
      let e = emit_ir(e, opcode.IrScopeGetRef(name))
      use e <- result.try(emit_logical_assign_test(e, op, end_label))
      // Test passed: drop the old value, evaluate RHS, write, leave RHS.
      let e = emit_ir(e, IrPop)
      use e <- result.map(emit_named_expr(e, right, name))
      e
      |> emit_ir(IrDup)
      |> emit_ir(opcode.IrScopePutRef(name))
      |> emit_ir(IrLabel(end_label))
    }
    // super.x &&= v — needs GetSuperValue2/PutSuperValue stack juggling on
    // the short-circuit path; keep the pre-existing unsupported error.
    ast.MemberExpression(ast.SuperExpression, _, _) ->
      Error(Unsupported("assignment op"))
    ast.MemberExpression(obj, ast.Identifier(prop), False)
    | ast.MemberExpression(obj, ast.StringExpression(prop), False) -> {
      let #(e, short_label) = fresh_label(e)
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, obj))
      // GetField2 keeps the base under the value: [old, obj].
      let e = emit_get_field2(e, prop)
      use e <- result.try(emit_logical_assign_test(e, op, short_label))
      // [old, obj] → [obj] → [rval, obj] → PutField → [rval].
      let e = emit_ir(e, IrPop)
      use e <- result.map(emit_expr(e, right))
      e
      |> emit_put_field(prop)
      |> emit_ir(IrJump(end_label))
      // Short-circuit: [old, obj] → [old].
      |> emit_ir(IrLabel(short_label))
      |> emit_ir(IrSwap)
      |> emit_ir(IrPop)
      |> emit_ir(IrLabel(end_label))
    }
    ast.MemberExpression(obj, key, True) -> {
      let #(e, short_label) = fresh_label(e)
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, obj))
      use e <- result.try(emit_expr(e, key))
      // GetElem2 keeps base+key under the value: [old, key, obj].
      let e = emit_ir(e, IrGetElem2)
      use e <- result.try(emit_logical_assign_test(e, op, short_label))
      // [old, key, obj] → [key, obj] → [rval, key, obj] → PutElem → [rval].
      let e = emit_ir(e, IrPop)
      use e <- result.map(emit_expr(e, right))
      e
      |> emit_ir(IrPutElem)
      |> emit_ir(IrJump(end_label))
      // Short-circuit: [old, key, obj] → [old].
      |> emit_ir(IrLabel(short_label))
      |> emit_ir(IrSwap)
      |> emit_ir(IrPop)
      |> emit_ir(IrSwap)
      |> emit_ir(IrPop)
      |> emit_ir(IrLabel(end_label))
    }
    _ -> Error(Unsupported("assignment op"))
  }
}

/// Emit the short-circuit test for a logical assignment. On entry the old
/// value is on top of the stack. Jumps to `short_label` with the old value
/// still on top when the assignment must NOT happen; falls through with the
/// old value still on top when it must (caller pops it).
fn emit_logical_assign_test(
  e: Emitter,
  op: ast.AssignmentOp,
  short_label: Int,
) -> Result(Emitter, EmitError) {
  case op {
    // x &&= v assigns only when the old value is truthy.
    ast.LogicalAndAssign ->
      Ok(e |> emit_ir(IrDup) |> emit_ir(IrJumpIfFalse(short_label)))
    // x ||= v assigns only when the old value is falsy.
    ast.LogicalOrAssign ->
      Ok(e |> emit_ir(IrDup) |> emit_ir(IrJumpIfTrue(short_label)))
    // x ??= v assigns only when the old value is nullish. There is no
    // jump-if-not-nullish op, so hop over an unconditional short jump.
    ast.NullishCoalesceAssign -> {
      let #(e, assign_label) = fresh_label(e)
      Ok(
        e
        |> emit_ir(IrDup)
        |> emit_ir(IrJumpIfNullish(assign_label))
        |> emit_ir(IrJump(short_label))
        |> emit_ir(IrLabel(assign_label)),
      )
    }
    _ -> Error(Unsupported("assignment op"))
  }
}

// ============================================================================
// Class compilation
// ============================================================================

/// Compile a class body. Leaves the constructor function on the stack.
///
/// Strategy:
///   1. Extract or synthesize constructor
///   2. Inject field initializer code into constructor body
///   3. MakeClosure for the constructor
///   4. Define instance methods on ctor.prototype (non-enumerable)
///   5. Define static methods on ctor (non-enumerable)
fn compile_class(
  e: Emitter,
  binding_name: Option(String),
  display_name: Option(String),
  super_class: Option(ast.Expression),
  body: List(ast.ClassElement),
) -> Result(Emitter, EmitError) {
  // ES spec: class bodies are always strict (§15.7.1 "A class body is always
  // strict mode code."). Force strict on the emitter so all compile_function_body
  // calls for methods/constructor inherit it. Restore enclosing strictness on
  // exit so a sloppy-mode caller isn't polluted.
  let saved_strict = e.strict
  // §15.7.14 step 6.b/c: extend the running PrivateEnvironment with this
  // class's declared private names for the duration of the class body —
  // methods/initializers (and any direct eval inside them) parse with them
  // in scope. Restored alongside strictness on exit.
  let saved_private_env = e.private_env
  let e =
    Emitter(
      ..e,
      strict: True,
      private_env: list.append(class_private_names(body), e.private_env),
    )
  // §15.7.14 step 4/5: per-class block scope holding the immutable inner
  // class-name binding (and later P8's <class_fields_init> const). Declare
  // BEFORE heritage emit so `class C extends C{}` TDZs on the inner C, and so
  // ctor/method MakeClosure snapshots (P0) see the slot for capture.
  let e = emit_op(e, EnterScope(BlockScope))
  let e = case binding_name {
    Some(n) -> emit_op(e, DeclareVar(n, ConstBinding))
    None -> e
  }
  // §15.7.14 step 28.f [[Fields]] — declared here (not in compile_*_class) so
  // both ctor and field-init-fn IrMakeClosure snapshots see the slot. Init to
  // the closure (or undefined) by emit_attach_field_init.
  let e = emit_op(e, DeclareVar(class_fields_init, ConstBinding))
  // §15.7.14 steps 5/6: the class's PrivateEnvironment. Each private element
  // name ("#m") becomes a class-scope const bound to a freshly minted
  // PrivateName key (NewPrivateName) — so each class *evaluation* gets
  // distinct names, nested classes shadow, and methods/eval capture them
  // through ordinary lexical scoping. Instance private methods additionally
  // get a hidden const (private_fn_const) holding the closure built once at
  // class-definition time and installed per-instance by the field-init fn.
  let e =
    list.fold(class_private_names(body), e, fn(e, pname) {
      let e = emit_op(e, DeclareVar(pname, ConstBinding))
      let e = emit_ir(e, IrNewPrivateName(pname))
      emit_ir(e, IrScopeInitVar(pname))
    })
  let e =
    list.fold(instance_private_fn_consts(body), e, fn(e, hidden) {
      emit_op(e, DeclareVar(hidden, ConstBinding))
    })
  // §15.7.14 ClassElementEvaluation: computed element names (fields AND
  // methods) are evaluated ONCE at class-definition time, in source order.
  // Declare a hidden class-scope const per computed key (BEFORE the init-fn
  // closures are built so their scope snapshots see the slots); the keys
  // themselves are evaluated inside compile_class_body — after heritage and
  // before the inner-name binding is initialized below, since element
  // evaluation precedes InitializeBinding(classBinding, F), so
  // `class C { [C] = 1 }` throws a TDZ ReferenceError.
  let element_keys = computed_element_keys(body)
  let e =
    list.fold(element_keys, e, fn(e, pair) {
      let #(idx, _key) = pair
      emit_op(e, DeclareVar(computed_field_const(idx), ConstBinding))
    })
  let body = stash_computed_element_keys(body)
  use #(e, static_init_idx) <- result.map(compile_class_body(
    e,
    display_name,
    super_class,
    body,
    element_keys,
  ))
  // [ctor]. §15.7.14: bind inner name to F AFTER all element evaluation
  // (computed keys above can't see it) but BEFORE static element evaluation,
  // so `class C { static x = C }` sees the constructor.
  let e = case binding_name {
    Some(n) -> e |> emit_ir(IrDup) |> emit_ir(IrScopeInitVar(n))
    None -> e
  }
  let e = emit_call_static_init(e, static_init_idx)
  let e = emit_op(e, LeaveScope)
  Emitter(..e, strict: saved_strict, private_env: saved_private_env)
}

/// Compile a class body (base or derived, selected by `super_class`): the
/// constructor child fn, instance-field init fn, heritage wiring, instance and
/// static methods, and the static-init fn. Stack on exit: [ctor]. Returns the
/// static-init child idx — static elements are emitted by compile_class AFTER
/// inner-name binding (§15.7.14: InitializeBinding(classBinding, F) precedes
/// static element evaluation).
fn compile_class_body(
  e: Emitter,
  name: Option(String),
  super_class: Option(ast.Expression),
  body: List(ast.ClassElement),
  computed_keys: List(#(Int, ast.Expression)),
) -> Result(#(Emitter, Option(Int)), EmitError) {
  let #(
    ctor_method,
    instance_methods,
    static_methods,
    instance_fields,
    static_elements,
  ) = classify_class_body(body)

  // Build constructor: if none provided, synthesize the spec default. For a
  // derived class that's (§15.7.14 step 14): constructor(...args) {
  // super(...args); }. Arc doesn't support rest parameters yet, so spread
  // `arguments` instead — observably equivalent here since the synthetic body
  // never re-reads it. For a base class it's an empty body.
  let #(ctor_params, ctor_body) = case ctor_method {
    Some(ast.ClassMethod(value: ast.FunctionExpression(_, params, body, ..), ..)) -> #(
      params,
      body,
    )
    _ -> #([], default_ctor_body(super_class))
  }

  // §13.3.7.1 SuperCall step 12 / §15.7.14 step 28: instance fields are
  // compiled into one synthetic initializer function (QuickJS class_fields_init
  // / spec [[Fields]]), bound to the <class_fields_init> const. Derived ctors
  // call it via emit_field_init_call after every `super()`
  // (FieldInitAfterSuper); base ctors call it before user code runs
  // (FieldInitAtStart, §10.2.2 [[Construct]] step 6). Constructors cannot be
  // generators or async (spec forbids it).
  let #(e, init_idx) =
    compile_class_init_fn(
      e,
      list.append(
        private_method_init_stmts(instance_methods),
        field_init_stmts(instance_fields),
      ),
    )
  let #(ctor_perms, field_init) = case super_class {
    Some(_) -> #(opcode.derived_ctor_perms, FieldInitAfterSuper)
    None -> #(opcode.method_perms, FieldInitAtStart)
  }
  let child =
    compile_function_body(
      e,
      name,
      None,
      ctor_params,
      ctor_body,
      False,
      False,
      False,
      // Class constructor — IS a constructor.
      True,
      ctor_perms,
      option.map(init_idx, fn(_) { field_init })
        |> option.unwrap(NoFieldInit),
    )
  let child =
    CompiledChild(
      ..child,
      is_derived_constructor: option.is_some(super_class),
      is_class_constructor: True,
    )
  let #(e, ctor_idx) = add_child_function(e, child)

  use e <- result.try(case super_class {
    Some(parent_expr) -> {
      // Emit parent expression → [parent]; MakeClosure → [parent, ctor];
      // SetupDerivedClass → [ctor] (wires prototype chain + [[HomeObject]]).
      use e <- result.map(emit_expr(e, parent_expr))
      e
      |> emit_ir(IrMakeClosure(ctor_idx))
      |> emit_ir(IrSetupDerivedClass)
    }
    None ->
      // MakeClosure (creates .prototype + .prototype.constructor) → [ctor].
      // §15.7.14 step 12: ctor.[[HomeObject]] = ctor.prototype, so `super.x`
      // in a base ctor body resolves through ctor.prototype.__proto__
      // (Object.prototype).
      Ok(
        e
        |> emit_ir(IrMakeClosure(ctor_idx))
        |> emit_ir(IrDup)
        |> emit_ir(IrGetField("prototype"))
        |> emit_ir(IrSwap)
        |> emit_ir(IrMakeMethod)
        |> emit_ir(IrSwap)
        |> emit_ir(IrPop),
      )
  })

  // Evaluate + ToPropertyKey each computed element name (fields AND methods,
  // instance AND static) in one source-order pass, into its stash const —
  // §15.7.14 evaluates each ClassElementName strictly in source order. Runs
  // after heritage, before the method definitions below read the stashed
  // keys, and before compile_class initializes the inner class-name binding.
  // Abrupt completions (throwing getters, ToPrimitive errors) surface here,
  // at class evaluation. Stack-neutral: [ctor] → [ctor].
  use e <- result.try(
    list.try_fold(computed_keys, e, fn(e, pair) {
      let #(idx, key) = pair
      use e <- result.map(emit_expr(e, key))
      e
      |> emit_ir(opcode.IrToPropertyKey)
      |> emit_ir(IrScopeInitVar(computed_field_const(idx)))
    }),
  )

  // Define instance methods on ctor.prototype, then static methods on ctor.
  use e <- result.try(emit_class_methods(
    e,
    instance_methods,
    on_prototype: True,
  ))
  use e <- result.try(emit_class_methods(e, static_methods, on_prototype: False))

  // Attach the field-initializer closure to ctor as [[Fields]] (§15.7.14
  // step 25). After methods so the init fn's [[HomeObject]] = proto sees them
  // via super.method(). Stack: [ctor] → [ctor].
  let e = emit_attach_field_init(e, init_idx)
  let #(e, static_init_idx) =
    compile_class_init_fn(e, static_init_stmts(static_elements))

  // Stack: [ctor]
  Ok(#(e, static_init_idx))
}

/// Spec default constructor body: `super(...arguments)` for derived classes,
/// empty for base classes.
fn default_ctor_body(super_class: Option(ast.Expression)) -> ast.Statement {
  case super_class {
    None -> ast.BlockStatement([])
    Some(_) ->
      ast.BlockStatement([
        ast.StmtWithLine(
          0,
          ast.ExpressionStatement(
            expression: ast.CallExpression(ast.SuperExpression, [
              ast.SpreadElement(ast.Identifier("arguments")),
            ]),
            directive: None,
          ),
        ),
      ])
  }
}

/// Compile a list of statements into a synthetic non-arrow initializer
/// function with field_init_perms — a method-like body (§15.7.14: "the
/// function created for [[Initializer]] is never directly accessible to
/// ECMAScript code"). Shared by instance-field init (this = instance,
/// [[HomeObject]] = ctor.prototype) and static-element init (this = ctor,
/// [[HomeObject]] = ctor). Returns Some(child_idx) when stmts is non-empty.
fn compile_class_init_fn(
  e: Emitter,
  stmts: List(ast.StmtWithLine),
) -> #(Emitter, Option(Int)) {
  case stmts {
    [] -> #(e, None)
    _ -> {
      let child =
        compile_function_body(
          e,
          None,
          None,
          [],
          ast.BlockStatement(stmts),
          False,
          False,
          False,
          // Synthetic field initializer — never directly constructible.
          False,
          opcode.field_init_perms,
          NoFieldInit,
        )
      let #(e, idx) = add_child_function(e, child)
      #(e, Some(idx))
    }
  }
}

/// Stack: [ctor, ..] → [ctor, ..]. Initialize the `<class_fields_init>` const
/// in the per-class block scope. With instance fields:
///   Dup; GetField("prototype")     → [proto, ctor]
///   MakeClosure(idx); MakeMethod   → [init_fn, proto, ctor], init.[[HomeObject]] = proto
///   Swap; Pop; ScopeInitVar        → [ctor], <class_fields_init> = init_fn
/// Without: write `undefined` so emit_field_init_call's JumpIfFalse skips and
/// the const slot isn't TDZ. Mirrors QuickJS OP_scope_put_var_init.
fn emit_attach_field_init(e: Emitter, init_idx: Option(Int)) -> Emitter {
  case init_idx {
    None -> push_const(e, JsUndefined)
    Some(idx) ->
      e
      |> emit_ir(IrDup)
      |> emit_ir(IrGetField("prototype"))
      |> emit_ir(IrMakeClosure(idx))
      |> emit_ir(IrMakeMethod)
      |> emit_ir(IrSwap)
      |> emit_ir(IrPop)
  }
  |> emit_ir(IrScopeInitVar(class_fields_init))
}

/// Define a list of class methods on the constructor (`on_prototype: False`)
/// or on `ctor.prototype` (`on_prototype: True`). Stack: [ctor] → [ctor].
fn emit_class_methods(
  e: Emitter,
  methods: List(ast.ClassElement),
  on_prototype on_prototype: Bool,
) -> Result(Emitter, EmitError) {
  use e, method <- list.try_fold(methods, e)
  case method {
    // Private method/accessor (#m() {}, get #m() {}, set #m(v) {}).
    // Instance: build the closure once here (class-definition time) with
    // [[HomeObject]] = ctor.prototype and stash it in the hidden class-scope
    // const — the field-init fn installs it on each instance per §7.3.29
    // PrivateMethodOrAccessorAdd (so it appears only after super() returns,
    // and double initialization via return-override throws).
    // Static: install on the constructor right now as an own private element.
    ast.ClassMethod(
      key: ast.Identifier("#" <> rest),
      value: ast.FunctionExpression(_, params, body, is_gen, is_async),
      kind:,
      ..,
    ) -> {
      let name = "#" <> rest
      let fn_name = case kind {
        ast.MethodGet -> "get " <> name
        ast.MethodSet -> "set " <> name
        ast.MethodMethod | ast.MethodConstructor -> name
      }
      case on_prototype {
        True -> {
          let e = emit_ir(e, IrDup)
          let e = emit_ir(e, IrGetField("prototype"))
          let e =
            make_method_closure(
              e,
              Some(fn_name),
              params,
              body,
              is_gen,
              is_async,
            )
          let e = emit_ir(e, IrMakeMethod)
          let e = emit_ir(e, IrScopeInitVar(private_fn_const(kind, name)))
          Ok(emit_ir(e, IrPop))
        }
        False -> {
          let e = emit_ir(e, IrDup)
          let e =
            make_method_closure(
              e,
              Some(fn_name),
              params,
              body,
              is_gen,
              is_async,
            )
          let e = emit_ir(e, IrMakeMethod)
          let e = emit_ir(e, IrScopeGetVar(name))
          let e = emit_ir(e, IrSwap)
          let define = case kind {
            ast.MethodGet -> IrDefinePrivateAccessor(opcode.Getter)
            ast.MethodSet -> IrDefinePrivateAccessor(opcode.Setter)
            ast.MethodMethod | ast.MethodConstructor -> IrDefinePrivateMethod
          }
          let e = emit_ir(e, define)
          Ok(emit_ir(e, IrPop))
        }
      }
    }
    // Static-string key: name() {}, "name"() {}, get name() {}, set name(v) {}
    ast.ClassMethod(
      key: ast.Identifier(name),
      value: ast.FunctionExpression(_, params, body, is_gen, is_async),
      kind:,
      computed: False,
      ..,
    )
    | ast.ClassMethod(
        key: ast.StringExpression(name),
        value: ast.FunctionExpression(_, params, body, is_gen, is_async),
        kind:,
        computed: False,
        ..,
      ) -> {
      let #(fn_name, define_op) = case kind {
        ast.MethodGet -> #(
          "get " <> name,
          IrDefineAccessor(name, opcode.Getter, False),
        )
        ast.MethodSet -> #(
          "set " <> name,
          IrDefineAccessor(name, opcode.Setter, False),
        )
        // MethodConstructor is stripped by classify_class_body; treat as method.
        ast.MethodMethod | ast.MethodConstructor -> #(
          name,
          IrDefineMethod(name),
        )
      }
      let e = emit_ir(e, IrDup)
      let e = case on_prototype {
        True -> emit_ir(e, IrGetField("prototype"))
        False -> e
      }
      let e =
        make_method_closure(e, Some(fn_name), params, body, is_gen, is_async)
      let e = emit_ir(e, define_op)
      Ok(emit_ir(e, IrPop))
    }
    // Computed key or numeric-literal key (`[expr]() {}`, `0b10() {}`).
    ast.ClassMethod(
      key:,
      value: ast.FunctionExpression(_, params, body, is_gen, is_async),
      kind:,
      ..,
    ) ->
      emit_computed_class_method(
        e,
        key,
        params,
        body,
        is_gen,
        is_async,
        kind,
        on_prototype,
      )
    _ -> Error(Unsupported("class method with non-function value"))
  }
}

/// Computed-key (or numeric-literal-key) class method/accessor.
/// `class { [expr]() {} }`, `get [expr]() {}`, `static [expr]() {}`, `0b10() {}`.
/// Stack on entry: [ctor]. Dups ctor, optionally fetches .prototype, evaluates
/// the key expression, builds the closure, then DefineMethodComputed /
/// DefineAccessorComputed (both consume [fn, key, target] → [target]), and pops
/// the target back to [ctor]. Function name is left None — SetFunctionName from
/// runtime keys is not yet implemented (matches object-literal computed methods).
fn emit_computed_class_method(
  e: Emitter,
  key: ast.Expression,
  params: List(ast.Pattern),
  body: ast.Statement,
  is_gen: Bool,
  is_async: Bool,
  kind: ast.MethodKind,
  on_prototype: Bool,
) -> Result(Emitter, EmitError) {
  let e = emit_ir(e, IrDup)
  let e = case on_prototype {
    True -> emit_ir(e, IrGetField("prototype"))
    False -> e
  }
  use e <- result.try(emit_expr(e, key))
  let e = make_method_closure(e, None, params, body, is_gen, is_async)
  let e = case kind {
    ast.MethodMethod -> emit_ir(e, IrDefineMethodComputed)
    ast.MethodGet -> emit_ir(e, IrDefineAccessorComputed(opcode.Getter, False))
    ast.MethodSet -> emit_ir(e, IrDefineAccessorComputed(opcode.Setter, False))
    // MethodConstructor is filtered out by classify_class_body before we get
    // here; treat as a plain method defensively rather than crashing.
    ast.MethodConstructor -> emit_ir(e, IrDefineMethodComputed)
  }
  Ok(emit_ir(e, IrPop))
}

/// Stack: [ctor] → [ctor]. §15.7.14 step 31: build the static-init closure
/// with [[HomeObject]] = ctor and immediately [[Call]] it with `this` = ctor.
/// No-op when the class has no static elements.
fn emit_call_static_init(e: Emitter, init_idx: Option(Int)) -> Emitter {
  case init_idx {
    None -> e
    Some(idx) ->
      e
      |> emit_ir(IrDup)
      |> emit_ir(IrMakeClosure(idx))
      |> emit_ir(IrMakeMethod)
      |> emit_ir(IrCallMethod("", 0))
      |> emit_ir(IrPop)
  }
}

/// Classify class body elements into constructor, instance methods, static
/// methods, instance fields, and static elements (static fields + static
/// blocks, interleaved in source order — §15.7.14 step 31 requires textual
/// order).
fn classify_class_body(
  body: List(ast.ClassElement),
) -> #(
  Option(ast.ClassElement),
  List(ast.ClassElement),
  List(ast.ClassElement),
  List(ast.ClassElement),
  List(ast.ClassElement),
) {
  let #(ctor, im_rev, sm_rev, if_rev, se_rev) =
    list.fold(body, #(None, [], [], [], []), fn(acc, elem) {
      let #(
        ctor,
        instance_methods,
        static_methods,
        instance_fields,
        static_elements,
      ) = acc
      case elem {
        // Constructor
        ast.ClassMethod(kind: ast.MethodConstructor, ..) -> #(
          Some(elem),
          instance_methods,
          static_methods,
          instance_fields,
          static_elements,
        )
        // Instance method (non-static, non-constructor)
        ast.ClassMethod(is_static: False, kind: ast.MethodMethod, ..) -> #(
          ctor,
          [elem, ..instance_methods],
          static_methods,
          instance_fields,
          static_elements,
        )
        // Static method
        ast.ClassMethod(is_static: True, ..) -> #(
          ctor,
          instance_methods,
          [elem, ..static_methods],
          instance_fields,
          static_elements,
        )
        // Instance field (non-static)
        ast.ClassField(is_static: False, ..) -> #(
          ctor,
          instance_methods,
          static_methods,
          [elem, ..instance_fields],
          static_elements,
        )
        // Getter/setter on instance
        ast.ClassMethod(is_static: False, ..) -> #(
          ctor,
          [elem, ..instance_methods],
          static_methods,
          instance_fields,
          static_elements,
        )
        // Static field / static block — both run at class-definition time in
        // source order via the static-init wrapper.
        ast.ClassField(is_static: True, ..) | ast.StaticBlock(..) -> #(
          ctor,
          instance_methods,
          static_methods,
          instance_fields,
          [elem, ..static_elements],
        )
      }
    })
  #(
    ctor,
    list.reverse(im_rev),
    list.reverse(sm_rev),
    list.reverse(if_rev),
    list.reverse(se_rev),
  )
}

/// All private element names ("#m") declared by a class body, deduped
/// (get/set pairs share one PrivateName), in source order. These become
/// class-scope consts bound to freshly minted PrivateName keys — the spec
/// PrivateEnvironment (§15.7.14 steps 5/6) mapped onto lexical scoping.
fn class_private_names(body: List(ast.ClassElement)) -> List(String) {
  list.fold(body, [], fn(acc, elem) {
    let name = case elem {
      ast.ClassMethod(key: ast.Identifier("#" <> rest), ..)
      | ast.ClassField(key: ast.Identifier("#" <> rest), ..) ->
        Some("#" <> rest)
      _ -> None
    }
    case name {
      Some(n) ->
        case list.contains(acc, n) {
          True -> acc
          False -> [n, ..acc]
        }
      None -> acc
    }
  })
  |> list.reverse
}

/// Hidden class-scope const name holding the closure of an instance private
/// method/accessor half. NUL-prefixed so source code can never name it.
fn private_fn_const(kind: ast.MethodKind, name: String) -> String {
  case kind {
    ast.MethodGet -> "\u{0}pg:" <> name
    ast.MethodSet -> "\u{0}ps:" <> name
    ast.MethodMethod | ast.MethodConstructor -> "\u{0}pm:" <> name
  }
}

/// Hidden class-scope const holding the stashed property key of the Nth
/// class element's computed name (field or method). §15.7.14: each
/// ClassElementName is evaluated ONCE at class-definition time, in source
/// order; method definitions and the instance/static init fns read the
/// captured key, never re-evaluate the expression. NUL-prefixed so source
/// code can never name it.
fn computed_field_const(idx: Int) -> String {
  "\u{0}ck:" <> int.to_string(idx)
}

/// All computed element names — fields AND methods, instance AND static, in
/// source order — paired with their body index. This is the order keys are
/// evaluated in at class definition time: §15.7.14 evaluates each
/// ClassElementName in one source-order pass over the class body, so
/// intercalated method/field and static/non-static keys all share one pass.
fn computed_element_keys(
  body: List(ast.ClassElement),
) -> List(#(Int, ast.Expression)) {
  list.index_map(body, fn(elem, idx) { #(idx, elem) })
  |> list.filter_map(fn(pair) {
    let #(idx, elem) = pair
    case elem {
      ast.ClassField(key:, computed: True, ..) -> Ok(#(idx, key))
      ast.ClassMethod(key:, computed: True, ..) -> Ok(#(idx, key))
      _ -> Error(Nil)
    }
  })
}

/// Rewrite each computed element's key to a read of its hidden stash const so
/// downstream emission loads the already-coerced key instead of re-evaluating
/// the name expression: field init fns run per-instantiation / at static-init
/// time, and method definitions run after ALL keys were evaluated in source
/// order (see compile_class_body).
fn stash_computed_element_keys(
  body: List(ast.ClassElement),
) -> List(ast.ClassElement) {
  list.index_map(body, fn(elem, idx) {
    case elem {
      ast.ClassField(value:, is_static:, computed: True, ..) ->
        ast.ClassField(
          key: ast.Identifier(computed_field_const(idx)),
          value:,
          is_static:,
          computed: True,
        )
      ast.ClassMethod(value:, kind:, is_static:, computed: True, ..) ->
        ast.ClassMethod(
          key: ast.Identifier(computed_field_const(idx)),
          value:,
          kind:,
          is_static:,
          computed: True,
        )
      _ -> elem
    }
  })
}

/// Hidden const names for every instance private method/accessor half in the
/// class body (declared up front in compile_class so child-fn closure
/// snapshots see the slots).
fn instance_private_fn_consts(body: List(ast.ClassElement)) -> List(String) {
  use elem <- list.filter_map(body)
  case elem {
    ast.ClassMethod(
      key: ast.Identifier("#" <> rest),
      kind:,
      is_static: False,
      ..,
    ) -> Ok(private_fn_const(kind, "#" <> rest))
    _ -> Error(Nil)
  }
}

/// §7.3.29 PrivateMethodOrAccessorAdd: synthesize the per-instance install
/// statements for instance private methods/accessors. Encoded as
/// ClassFieldInit with the hidden const (see private_fn_const) as the value
/// expression — emit_stmt's ClassFieldInit branch decodes the NUL-prefixed
/// identifier and emits IrDefinePrivateMethod/Accessor instead of a field
/// add. Spec order: all private methods install BEFORE field initializers
/// run (InitializeInstanceElements steps 5 then 6).
fn private_method_init_stmts(
  methods: List(ast.ClassElement),
) -> List(ast.StmtWithLine) {
  use m <- list.filter_map(methods)
  case m {
    ast.ClassMethod(
      key: ast.Identifier("#" <> rest),
      kind:,
      is_static: False,
      ..,
    ) ->
      Ok(ast.StmtWithLine(
        0,
        ast.ClassFieldInit(
          key: ast.Identifier("#" <> rest),
          value: Some(ast.Identifier(private_fn_const(kind, "#" <> rest))),
          computed: False,
        ),
      ))
    _ -> Error(Nil)
  }
}

/// Map every instance ClassField → ClassFieldInit statement.
/// Per §15.7.14 ClassFieldDefinitionEvaluation, value:None becomes undefined
/// (handled in emit_stmt). Static fields are filtered out upstream by
/// classify_class_body, but we guard on is_static:False here for safety.
fn field_init_stmts(fields: List(ast.ClassElement)) -> List(ast.StmtWithLine) {
  use field <- list.filter_map(fields)
  case field {
    ast.ClassField(key:, value:, computed:, is_static: False) ->
      Ok(ast.StmtWithLine(0, ast.ClassFieldInit(key:, value:, computed:)))
    _ -> Error(Nil)
  }
}

/// Map static elements → statements for the static-init wrapper body.
/// `static x = v` → ClassFieldInit (emit_stmt defines on `this` = ctor).
/// `static { ... }` → arrow IIFE so the block gets its own var environment
/// (§15.7.1 ClassStaticBlockBody) while inheriting `this`/home_object from
/// the wrapper. Mirrors QuickJS emit_class_init_start/end.
fn static_init_stmts(
  elements: List(ast.ClassElement),
) -> List(ast.StmtWithLine) {
  use elem <- list.filter_map(elements)
  case elem {
    ast.ClassField(key:, value:, computed:, is_static: True) ->
      Ok(ast.StmtWithLine(0, ast.ClassFieldInit(key:, value:, computed:)))
    ast.StaticBlock(body:) ->
      Ok(ast.StmtWithLine(
        0,
        ast.ExpressionStatement(
          ast.CallExpression(
            ast.ArrowFunctionExpression(
              [],
              ast.ArrowBodyBlock(ast.BlockStatement(body)),
              False,
            ),
            [],
          ),
          directive: None,
        ),
      ))
    _ -> Error(Nil)
  }
}

// ============================================================================
// Debug helpers
// ============================================================================

fn string_inspect_expr_kind(expr: ast.Expression) -> String {
  case expr {
    ast.Identifier(_) -> "Identifier"
    ast.NumberLiteral(_) -> "NumberLiteral"
    ast.BigIntLiteral(_) -> "BigIntLiteral"
    ast.StringExpression(_) -> "StringExpression"
    ast.BooleanLiteral(_) -> "BooleanLiteral"
    ast.NullLiteral -> "NullLiteral"
    ast.UndefinedExpression -> "UndefinedExpression"
    ast.BinaryExpression(..) -> "BinaryExpression"
    ast.LogicalExpression(..) -> "LogicalExpression"
    ast.UnaryExpression(..) -> "UnaryExpression"
    ast.UpdateExpression(..) -> "UpdateExpression"
    ast.AssignmentExpression(..) -> "AssignmentExpression"
    ast.CallExpression(..) -> "CallExpression"
    ast.MemberExpression(..) -> "MemberExpression"
    ast.OptionalMemberExpression(..) -> "OptionalMemberExpression"
    ast.OptionalCallExpression(..) -> "OptionalCallExpression"
    ast.ConditionalExpression(..) -> "ConditionalExpression"
    ast.NewExpression(..) -> "NewExpression"
    ast.ThisExpression -> "ThisExpression"
    ast.SuperExpression -> "SuperExpression"
    ast.ArrayExpression(_) -> "ArrayExpression"
    ast.ObjectExpression(_) -> "ObjectExpression"
    ast.FunctionExpression(..) -> "FunctionExpression"
    ast.ArrowFunctionExpression(..) -> "ArrowFunctionExpression"
    ast.ClassExpression(..) -> "ClassExpression"
    ast.YieldExpression(..) -> "YieldExpression"
    ast.AwaitExpression(_) -> "AwaitExpression"
    ast.SequenceExpression(_) -> "SequenceExpression"
    ast.SpreadElement(_) -> "SpreadElement"
    ast.TemplateLiteral(..) -> "TemplateLiteral"
    ast.TaggedTemplateExpression(..) -> "TaggedTemplateExpression"
    ast.MetaProperty(..) -> "MetaProperty"
    ast.ImportExpression(..) -> "ImportExpression"
    ast.RegExpLiteral(..) -> "RegExpLiteral"
    ast.ParenthesizedExpression(..) -> "ParenthesizedExpression"
    ast.IntrinsicGetDisposer(..) -> "IntrinsicGetDisposer"
    ast.IntrinsicMakeSuppressed(..) -> "IntrinsicMakeSuppressed"
    ast.IntrinsicTemplateObject(..) -> "IntrinsicTemplateObject"
  }
}
