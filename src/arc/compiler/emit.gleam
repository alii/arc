/// Phase 1: AST Emission
///
/// Walks the AST and produces a list of EmitterOps — symbolic IR instructions
/// mixed with scope markers. Variable references use string names (IrScopeGetVar),
/// jump targets use integer label IDs (IrJump). These are resolved in Phase 2 and 3.
import arc/parser/ast
import arc/vm/opcode.{
  type IrOp, IrArrayFrom, IrArrayFromWithHoles, IrArrayPush, IrArrayPushHole,
  IrArraySpread, IrAsyncYieldStarNext, IrAsyncYieldStarResume, IrAwait, IrBinOp,
  IrCallApply, IrCallConstructor, IrCallConstructorApply, IrCallMethod,
  IrCallMethodApply, IrCreateArguments, IrCreateRestArray, IrDeclareGlobalLex,
  IrDeclareGlobalVar, IrDefineAccessor, IrDefineAccessorComputed, IrDefineField,
  IrDefineFieldComputed, IrDefineMethod, IrDefineMethodComputed, IrDeleteElem,
  IrDeleteField, IrDup, IrForInNext, IrForInStart, IrGetAsyncIterator, IrGetElem,
  IrGetElem2, IrGetField, IrGetField2, IrGetIterator, IrGetLexical,
  IrGetPrivateField, IrGetPrivateField2, IrGetPrototypeOf, IrGetSuperValue,
  IrGetSuperValue2, IrGosub, IrInitGlobalLex, IrInitialYield,
  IrIteratorCheckObject, IrIteratorClose, IrIteratorCloseThrow, IrIteratorNext,
  IrIteratorRest, IrJump, IrJumpIfFalse, IrJumpIfNullish, IrJumpIfTrue, IrLabel,
  IrMakeClosure, IrMakeMethod, IrNewObject, IrNewRegExp, IrObjectRestCopy,
  IrObjectSpread, IrPop, IrPopTry, IrPrivateIn, IrPushConst, IrPushTry,
  IrPutElem, IrPutField, IrPutPrivateField, IrPutSuperValue, IrRet, IrReturn,
  IrScopeGetVar, IrScopeInitVar, IrScopePutVar, IrScopeReboxVar,
  IrScopeTypeofVar, IrSetLine, IrSetProto, IrSetThis, IrSetupDerivedClass,
  IrSwap, IrThrow, IrThrowError, IrTypeOf, IrUnaryOp, IrYield, IrYieldStar,
}
import arc/vm/value.{
  type JsValue, Finite, JsBool, JsNull, JsNumber, JsString, JsUndefined,
}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

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
    /// InitializeInstanceElements). Set by compile_derived_class/base_class
    /// when the class has instance fields; arrows inherit it so `()=>super()`
    /// can find it.
    field_init: FieldInitMode,
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
  let has_default_export =
    list.any(items, fn(item) {
      case item {
        ast.ExportDefaultDeclaration(_) -> True
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
  let e = Emitter(..new_emitter(), strict: True)
  let e = emit_op(e, EnterScope(FunctionScope))
  // Module top-level `this === undefined` (§16.2.1.6.4). DeclareLexical
  // allocates slot 0; runtime padding leaves it JsUndefined.
  let e = emit_op(e, DeclareLexical(opcode.RefThis))

  // Hoist var declarations
  let hoisted_vars = collect_hoisted_vars(stmts)
  let e =
    list.fold(hoisted_vars, e, fn(e, name) {
      emit_op(e, DeclareVar(name, VarBinding))
    })

  // Declare *default* binding if module has a default export. Per §16.2.1.6.2
  // step 24.b.i this is CreateMutableBinding (not immutable), so LetBinding —
  // also lets the synthetic `*default* = expr` assignment below pass the
  // IrScopePutVar const-reassign check.
  let e = case has_default_export {
    True -> emit_op(e, DeclareVar("*default*", LetBinding))
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

  use e <- result.try(emit_stmts_tail(e, stmts))

  let e = emit_op(e, LeaveScope)
  let #(code, constants, constants_map, children) = finish(e)
  Ok(#(code, constants, constants_map, children, True, hoisted_funcs))
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
      ast.ExportNamedDeclaration(option.Some(decl), _, _) ->
        Ok(ast.StmtWithLine(0, decl))
      ast.ExportDefaultDeclaration(expr) ->
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
      ast.ExportNamedDeclaration(None, _, _) -> Error(Nil)
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
  let e = Emitter(..new_emitter(), strict: script_strict, top_lex:)

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
    LexGlobal -> e
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
fn get_field_op(name: String) -> IrOp {
  case name {
    "#" <> _ -> IrGetPrivateField(name)
    _ -> IrGetField(name)
  }
}

fn get_field2_op(name: String) -> IrOp {
  case name {
    "#" <> _ -> IrGetPrivateField2(name)
    _ -> IrGetField2(name)
  }
}

fn put_field_op(name: String) -> IrOp {
  case name {
    "#" <> _ -> IrPutPrivateField(name)
    _ -> IrPutField(name)
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

/// Barrier frame for try/catch bodies — never a target, only crossed.
/// Makes break/continue/return that jump out of the try emit the right
/// number of PopTry ops to keep try_stack balanced.
fn push_barrier(e: Emitter, pop_try: Int) -> Emitter {
  Emitter(..e, loop_stack: [
    LoopContext(
      break_label: -1,
      continue_label: -1,
      label: None,
      is_regular: False,
      cross_pop_try: pop_try,
      has_iterator: False,
      label_finally: None,
      drop_count: 0,
    ),
    ..e.loop_stack
  ])
}

/// Barrier for a try-body or catch-body that has a `finally`. When crossed,
/// emit_goto_loop / ReturnStatement walk will: PopTry×pop_try, then
/// `push undef; Gosub(fin_label); Pop`. QuickJS: push_break_entry then
/// `block_env.label_finally = label_finally` (quickjs.c:28826-28828, 28889).
fn push_barrier_finally(e: Emitter, pop_try: Int, fin_label: Int) -> Emitter {
  Emitter(..e, loop_stack: [
    LoopContext(
      break_label: -1,
      continue_label: -1,
      label: None,
      is_regular: False,
      cross_pop_try: pop_try,
      has_iterator: False,
      label_finally: Some(fin_label),
      drop_count: 0,
    ),
    ..e.loop_stack
  ])
}

/// Barrier for the finally body itself. Crossing it drops `drop` value-stack
/// slots — the [slot, gosub_retpc] pair pushed by the caller+Gosub — so the
/// abrupt completion inside finally replaces the saved one (never reaches
/// IrRet). QuickJS: push_break_entry(..., -1, -1, 2) at quickjs.c:28934-28935.
fn push_barrier_drop(e: Emitter, drop: Int) -> Emitter {
  Emitter(..e, loop_stack: [
    LoopContext(
      break_label: -1,
      continue_label: -1,
      label: None,
      is_regular: False,
      cross_pop_try: 0,
      has_iterator: False,
      label_finally: None,
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

    ast.BlockStatement(body) -> {
      let e = emit_op(e, EnterScope(BlockScope))
      use e <- result.map(emit_stmts_tail(e, body))
      emit_op(e, LeaveScope)
    }

    ast.IfStatement(condition, consequent, alternate) -> {
      let #(e, else_label) = fresh_label(e)
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, condition))
      let e = emit_ir(e, IrJumpIfFalse(else_label))
      use e <- result.try(emit_stmt_tail(e, consequent))
      let e = emit_ir(e, IrJump(end_label))
      let e = emit_ir(e, IrLabel(else_label))
      let e = case alternate {
        Some(alt) -> emit_stmt_tail(e, alt) |> result.unwrap(e)
        None -> push_const(e, JsUndefined)
      }
      let e = emit_ir(e, IrLabel(end_label))
      Ok(e)
    }

    ast.TryStatement(block, handler, _finalizer) -> {
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

          use e <- result.try(emit_stmt_tail(e, catch_body))
          let e = emit_op(e, LeaveScope)
          let e = emit_ir(e, IrLabel(end_label))
          Ok(e)
        }
        None -> emit_stmt_tail(e, block)
      }
    }

    // All other statements: delegate to regular emit_stmt, then push undefined
    // as the completion value
    _ -> {
      use e <- result.map(emit_stmt(e, stmt))
      push_const(e, JsUndefined)
    }
  }
}

/// Emit an IrSetLine for the statement's source line (so `Error.stack` can
/// report it), unless the line is 0 — the sentinel for synthetic statements
/// the parser never produced (class field inits, desugared arrow bodies).
fn set_line(e: Emitter, line: Int) -> Emitter {
  case line {
    0 -> e
    _ -> emit_ir(e, IrSetLine(line))
  }
}

/// Like emit_stmts but the last statement is emitted in tail position.
fn emit_stmts_tail(
  e: Emitter,
  stmts: List(ast.StmtWithLine),
) -> Result(Emitter, EmitError) {
  case stmts {
    [] -> Ok(push_const(e, JsUndefined))
    [only] -> emit_stmt_tail(set_line(e, only.line), only.statement)
    [first, ..rest] -> {
      use e <- result.try(emit_stmt(set_line(e, first.line), first.statement))
      emit_stmts_tail(e, rest)
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
    ast.SwitchStatement(_, cases) ->
      list.flat_map(cases, fn(c) {
        case c {
          ast.SwitchCase(_, consequent) ->
            list.flat_map(consequent, collect_vars_located)
        }
      })
    // Function declarations: include the name for hoisting (DeclareVar)
    // but don't recurse into the body (nested scope).
    ast.FunctionDeclaration(Some(name), ..) -> [name]
    ast.FunctionDeclaration(None, ..) -> []
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

/// Collect and compile hoisted function declarations.
/// Returns updated emitter + list of (name, func_index) pairs.
fn collect_hoisted_funcs(
  e: Emitter,
  stmts: List(ast.StmtWithLine),
) -> #(Emitter, List(#(String, Int))) {
  let #(e, funcs_rev) =
    list.fold(stmts, #(e, []), fn(acc, located) {
      let #(e, funcs) = acc
      case located.statement {
        ast.FunctionDeclaration(Some(name), params, body, is_gen, is_async) -> {
          let child =
            compile_function_body(
              e,
              Some(name),
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

    ast.ImportExpression(source, options) ->
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

    ast.TaggedTemplateExpression(tag, quasi) ->
      expr_references_arguments(tag) || expr_references_arguments(quasi)

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
  // (only ctors get a non-NoFieldInit).
  let field_init = case is_arrow {
    True -> parent.field_init
    False -> field_init
  }
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

  // Phase 1: Declare parameters (identifier or synthetic for destructuring)
  let #(e, destructured_params_rev) =
    list.index_fold(fixed_params, #(e, []), fn(acc, param, idx) {
      let #(e, destr) = acc
      case param {
        ast.IdentifierPattern(pname) -> #(
          emit_op(e, DeclareVar(pname, ParamBinding)),
          destr,
        )
        _ -> {
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
      let e = emit_op(e, DeclareVar("arguments", VarBinding))
      let e = emit_ir(e, IrCreateArguments)
      emit_ir(e, IrScopePutVar("arguments"))
    }
    False -> e
  }

  // Phase 2: Emit destructuring for non-identifier params
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
      emit_destructuring_bind(e, rest_target, ParamBinding) |> result.unwrap(e)
    }
  }

  // Hoisting for the function body
  let hoisted_vars = collect_hoisted_vars(stmts)
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
    has_eval_call: e.has_eval_call,
    lexical_refs: e.lexical_refs,
    syntax_perms:,
  )
}

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
        ast.Identifier(name), False | ast.StringExpression(name), False -> {
          use e <- result.map(emit_expr(e, init))
          emit_ir(e, IrDefineField(name))
        }
        ast.NumberLiteral(n), False -> {
          let e = push_const(e, JsNumber(Finite(n)))
          use e <- result.map(emit_expr(e, init))
          emit_ir(e, IrDefineFieldComputed)
        }
        _, _ -> {
          // computed: True (or exotic non-computed key) — evaluate key at
          // construct time. Spec stashes computed keys at class-definition
          // time; this first-pass approximation is correct for stable keys.
          use e <- result.try(emit_expr(e, key))
          use e <- result.map(emit_expr(e, init))
          emit_ir(e, IrDefineFieldComputed)
        }
      })
      emit_ir(e, IrPop)
    }

    ast.ExpressionStatement(expression: expr, ..) -> {
      use e <- result.map(emit_expr(e, expr))
      emit_ir(e, IrPop)
    }

    ast.BlockStatement(body) -> {
      let e = emit_op(e, EnterScope(BlockScope))
      use e <- result.map(emit_stmts(e, body))
      emit_op(e, LeaveScope)
    }

    ast.VariableDeclaration(kind, declarators) -> {
      let binding_kind = case kind {
        ast.Var -> VarBinding
        ast.Let -> LetBinding
        ast.Const -> ConstBinding
      }
      list.try_fold(declarators, e, fn(e, decl) {
        case decl {
          ast.VariableDeclarator(ast.IdentifierPattern(name), init) -> {
            // For let/const, emit declaration marker (var already hoisted)
            let e = case kind {
              ast.Let -> declare_lex(e, name, False)
              ast.Const -> declare_lex(e, name, True)
              ast.Var -> e
            }
            case init {
              Some(init_expr) -> {
                use e <- result.map(emit_named_expr(e, init_expr, name))
                case kind {
                  ast.Var -> emit_ir(e, IrScopePutVar(name))
                  ast.Let | ast.Const -> init_lex(e, name)
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
      use e <- result.try(emit_stmt(e, consequent))
      let e = emit_ir(e, IrJump(end_label))
      let e = emit_ir(e, IrLabel(else_label))
      let e = case alternate {
        Some(alt) -> emit_stmt(e, alt) |> result.unwrap(e)
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
          let e = push_barrier(e, 1)
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

          use e <- result.try(emit_stmt(e, catch_body))
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
          let e = push_barrier_finally(e, 1, fin_label)
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
          let e = push_barrier_drop(e, 2)
          use e <- result.try(emit_stmt(e, finally_body))
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
          let e = push_barrier_finally(e, 2, fin_label)
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
          let e = push_barrier_finally(e, 1, fin_label)
          use e <- result.try(emit_stmt(e, catch_body))
          let e = pop_loop(e)
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
          let e = push_barrier_drop(e, 2)
          use e <- result.try(emit_stmt(e, finally_body))
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

    ast.FunctionDeclaration(..) -> {
      // Already hoisted — nothing to emit here
      Ok(e)
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

    _ -> Error(Unsupported("statement: " <> string_inspect_stmt_kind(stmt)))
  }
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

fn emit_expr(e: Emitter, expr: ast.Expression) -> Result(Emitter, EmitError) {
  case expr {
    // Literals
    ast.NumberLiteral(value) -> Ok(push_const(e, JsNumber(Finite(value))))
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
      emit_ir(e, IrPrivateIn("#" <> rest))
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
        ast.Identifier(_) -> {
          // delete x → always true in sloppy mode (can't delete plain vars)
          Ok(push_const(e, JsBool(True)))
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
    ast.UpdateExpression(op, prefix, ast.Identifier(name)) -> {
      let one = JsNumber(Finite(1.0))
      let bin_kind = case op {
        ast.Increment -> opcode.Add
        ast.Decrement -> opcode.Sub
      }
      case prefix {
        True -> {
          // ++x: get, add 1, dup (keep result), store
          let e = emit_ir(e, IrScopeGetVar(name))
          let e = push_const(e, one)
          let e = emit_ir(e, IrBinOp(bin_kind))
          let e = emit_ir(e, IrDup)
          let e = emit_ir(e, IrScopePutVar(name))
          Ok(e)
        }
        False -> {
          // x++: get, ToNumeric (§13.4.2.1 step 3), dup (old value stays as
          // result), add 1, store. Unary `+` is the ToNumber coercion.
          let e = emit_ir(e, IrScopeGetVar(name))
          let e = emit_ir(e, IrUnaryOp(opcode.Pos))
          let e = emit_ir(e, IrDup)
          let e = push_const(e, one)
          let e = emit_ir(e, IrBinOp(bin_kind))
          let e = emit_ir(e, IrScopePutVar(name))
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
      let e = emit_ir(e, get_field2_op(prop))
      let e = push_const(e, one)
      let e = emit_ir(e, IrBinOp(bin_kind))
      let e = emit_ir(e, put_field_op(prop))
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
      use e <- result.map(emit_expr(e, right))
      let e = emit_ir(e, IrDup)
      emit_ir(e, IrScopePutVar(name))
    }
    // Non-simple-assign parenthesized LHS — safe to unwrap (no name inference
    // for compound assignment anyway).
    ast.AssignmentExpression(op, ast.ParenthesizedExpression(inner), right) ->
      emit_expr(e, ast.AssignmentExpression(op, inner, right))

    // Assignment to identifier
    ast.AssignmentExpression(ast.Assign, ast.Identifier(name), right) -> {
      let inferred_name = case name {
        "*default*" -> "default"
        _ -> name
      }
      use e <- result.map(emit_named_expr(e, right, inferred_name))
      let e = emit_ir(e, IrDup)
      emit_ir(e, IrScopePutVar(name))
    }

    // Compound assignment to identifier
    ast.AssignmentExpression(op, ast.Identifier(name), right) -> {
      case compound_to_binop(op) {
        Ok(bin_kind) -> {
          let e = emit_ir(e, IrScopeGetVar(name))
          use e <- result.map(emit_expr(e, right))
          let e = emit_ir(e, IrBinOp(bin_kind))
          let e = emit_ir(e, IrDup)
          emit_ir(e, IrScopePutVar(name))
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
      emit_ir(e, put_field_op(prop))
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
          let e = emit_ir(e, get_field2_op(prop))
          use e <- result.map(emit_expr(e, right))
          let e = emit_ir(e, IrBinOp(bin_kind))
          emit_ir(e, put_field_op(prop))
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
    ) -> {
      use e <- result.try(emit_expr(e, obj))
      let e = emit_ir(e, get_field2_op(method_name))
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
    // Computed method call: obj[key](args) — must bind `this` to obj.
    // GetElem2 leaves [method, key, receiver]; we shuffle to [method, receiver]
    // via Swap+Pop so CallMethod sees the same shape as the dot-access path.
    ast.CallExpression(ast.MemberExpression(obj, key, True), args) -> {
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
          let e = emit_ir(e, opcode.IrCallEval(list.length(args)))
          Emitter(..e, has_eval_call: True)
        }
        True -> {
          let e = emit_ir(e, IrScopeGetVar("eval"))
          use e <- result.map(emit_args_array_with_spread(e, args))
          emit_ir(e, IrCallApply)
        }
      }

    // Regular call expression
    ast.CallExpression(callee, args) -> {
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

    // Member expression (dot access)
    ast.MemberExpression(object, ast.Identifier(prop), False) -> {
      use e <- result.map(emit_expr(e, object))
      emit_ir(e, get_field_op(prop))
    }

    // Computed member expression (obj[key])
    ast.MemberExpression(object, property, True) -> {
      use e <- result.try(emit_expr(e, object))
      use e <- result.map(emit_expr(e, property))
      emit_ir(e, IrGetElem)
    }

    // Optional member expression (obj?.prop)
    ast.OptionalMemberExpression(object, ast.Identifier(prop), False) -> {
      let #(e, nullish_label) = fresh_label(e)
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, object))
      let e = emit_ir(e, IrDup)
      let e = emit_ir(e, IrJumpIfNullish(nullish_label))
      let e = emit_ir(e, get_field_op(prop))
      let e = emit_ir(e, IrJump(end_label))
      let e = emit_ir(e, IrLabel(nullish_label))
      let e = emit_ir(e, IrPop)
      let e = push_const(e, JsUndefined)
      let e = emit_ir(e, IrLabel(end_label))
      Ok(e)
    }

    // Optional computed member expression (obj?.[key])
    ast.OptionalMemberExpression(object, property, True) -> {
      let #(e, nullish_label) = fresh_label(e)
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, object))
      let e = emit_ir(e, IrDup)
      let e = emit_ir(e, IrJumpIfNullish(nullish_label))
      use e <- result.try(emit_expr(e, property))
      let e = emit_ir(e, IrGetElem)
      let e = emit_ir(e, IrJump(end_label))
      let e = emit_ir(e, IrLabel(nullish_label))
      let e = emit_ir(e, IrPop)
      let e = push_const(e, JsUndefined)
      let e = emit_ir(e, IrLabel(end_label))
      Ok(e)
    }

    // Optional call expression (fn?.())
    ast.OptionalCallExpression(callee, args) -> {
      let #(e, nullish_label) = fresh_label(e)
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, callee))
      let e = emit_ir(e, IrDup)
      let e = emit_ir(e, IrJumpIfNullish(nullish_label))
      use e <- result.try(case has_spread_arg(args) {
        False -> {
          let arity = list.length(args)
          use e <- result.map(list.try_fold(args, e, emit_expr))
          emit_ir(e, opcode.IrCall(arity))
        }
        True -> {
          use e <- result.map(emit_args_array_with_spread(e, args))
          emit_ir(e, IrCallApply)
        }
      })
      let e = emit_ir(e, IrJump(end_label))
      let e = emit_ir(e, IrLabel(nullish_label))
      let e = emit_ir(e, IrPop)
      let e = push_const(e, JsUndefined)
      let e = emit_ir(e, IrLabel(end_label))
      Ok(e)
    }

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
    ast.FunctionExpression(name, params, body, is_gen, is_async) -> {
      let child =
        compile_function_body(
          e,
          name,
          params,
          body,
          False,
          is_gen,
          is_async,
          // Normal function expression: a constructor unless gen/async.
          !is_gen && !is_async,
          opcode.fn_perms,
          NoFieldInit,
        )
      let #(e, idx) = add_child_function(e, child)
      Ok(emit_ir(e, IrMakeClosure(idx)))
    }

    // Arrow function expression
    ast.ArrowFunctionExpression(params, body, is_async) -> {
      let body_stmt = case body {
        ast.ArrowBodyExpression(expr) ->
          ast.BlockStatement([
            ast.StmtWithLine(0, ast.ReturnStatement(Some(expr))),
          ])
        ast.ArrowBodyBlock(stmt) -> stmt
      }
      let child =
        compile_function_body(
          e,
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
        False -> Ok(emit_ir(e, IrYield))
        True ->
          case e.is_async {
            True -> {
              // Async-gen yield* — GetIterator(expr, async) wraps sync
              // iterables via CreateAsyncFromSyncIterator. Seed undefined arg,
              // self-loop: Next calls iter.next(arg), Await settles result,
              // Resume checks done / yields and jumps back to Next via label.
              // Leaves final result.value on stack.
              let e = emit_ir(e, IrGetAsyncIterator)
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

    // RegExp literal — push pattern and flags, then NewRegExp opcode
    ast.RegExpLiteral(pattern, flags) -> {
      let e = push_const(e, JsString(pattern))
      let e = push_const(e, JsString(flags))
      Ok(emit_ir(e, IrNewRegExp))
    }

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
      // Emit expression, concat with accumulator
      use e <- result.try(emit_expr(e, expr))
      let e = emit_ir(e, IrBinOp(opcode.Add))
      // Emit next quasi string, concat
      let e = push_const(e, JsString(quasi))
      let e = emit_ir(e, IrBinOp(opcode.Add))
      emit_template_parts(e, rest_exprs, rest_quasis)
    }
    // If there are trailing expressions without quasis (shouldn't happen but safe)
    [expr, ..rest_exprs], [] -> {
      use e <- result.try(emit_expr(e, expr))
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

  // Emit discriminant — stays on stack through comparison phase
  use e <- result.try(emit_expr(e, discriminant))

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
    // Anonymous function expression → bake name
    ast.FunctionExpression(None, params, body, is_gen, is_async) -> {
      let child =
        compile_function_body(
          e,
          Some(name),
          params,
          body,
          False,
          is_gen,
          is_async,
          // Named function expression: a constructor unless gen/async.
          !is_gen && !is_async,
          opcode.fn_perms,
          NoFieldInit,
        )
      let #(e, idx) = add_child_function(e, child)
      Ok(emit_ir(e, IrMakeClosure(idx)))
    }
    // Arrow function → bake name
    ast.ArrowFunctionExpression(params, body, is_async) -> {
      let body_stmt = case body {
        ast.ArrowBodyExpression(expr_inner) ->
          ast.BlockStatement([
            ast.StmtWithLine(0, ast.ReturnStatement(Some(expr_inner))),
          ])
        ast.ArrowBodyBlock(stmt) -> stmt
      }
      let child =
        compile_function_body(
          e,
          Some(name),
          params,
          body_stmt,
          True,
          False,
          is_async,
          // Arrow function — not a constructor.
          False,
          opcode.fn_perms,
          NoFieldInit,
        )
      let #(e, idx) = add_child_function(e, child)
      Ok(emit_ir(e, IrMakeClosure(idx)))
    }
    // Anonymous class expression → bake `.name` only; NO inner binding
    // (§8.4 NamedEvaluation step 2 — classBinding is undefined).
    ast.ClassExpression(None, super_class, body) ->
      compile_class(e, None, Some(name), super_class, body)
    // Not anonymous → emit normally (named fn keeps its own name)
    _ -> emit_expr(e, expr)
  }
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
    ast.FunctionExpression(_, params, body, is_gen, is_async) -> {
      let child =
        compile_function_body(
          e,
          name,
          params,
          body,
          False,
          is_gen,
          is_async,
          // method / getter / setter — not a constructor
          False,
          opcode.method_perms,
          NoFieldInit,
        )
      let #(e, idx) = add_child_function(e, child)
      Ok(emit_ir(e, IrMakeClosure(idx)))
    }
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
      emit_ir(e, IrDefineAccessor(name, opcode.Getter))
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
      emit_ir(e, IrDefineAccessor(name, opcode.Setter))
    }

    // Computed or exotic-key getter/setter: { get [expr]() {} }
    // Stack: emit key, emit fn → DefineAccessorComputed
    ast.Property(key:, value:, kind: ast.Get, ..) -> {
      use e <- result.try(emit_expr(e, key))
      use e <- result.map(emit_method_value(e, value, None))
      emit_ir(e, IrDefineAccessorComputed(opcode.Getter))
    }
    ast.Property(key:, value:, kind: ast.Set, ..) -> {
      use e <- result.try(emit_expr(e, key))
      use e <- result.map(emit_method_value(e, value, None))
      emit_ir(e, IrDefineAccessorComputed(opcode.Setter))
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

  // Evaluate the right-hand side (object to iterate)
  use e <- result.try(emit_expr(e, right))
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

  // Body
  use e <- result.try(emit_stmt(e, body))

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
  use e <- result.try(emit_expr(e, right))
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
  use e <- result.try(emit_stmt(e, body))

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

  use e <- result.try(emit_expr(e, right))
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
        ast.Const -> ConstBinding
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
        VarBinding | CaptureBinding -> e
      }
      case binding_kind {
        LetBinding | ConstBinding -> Ok(init_lex(e, name))
        VarBinding | ParamBinding | CatchBinding | CaptureBinding ->
          Ok(emit_ir(e, IrScopePutVar(name)))
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
      let e = emit_ir(e, put_field_op(prop))
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
      let #(e, close_throw) = fresh_label(e)
      let #(e, done_label) = fresh_label(e)
      // [source] → [iter]. PushTry immediately after so the recorded
      // stack_depth has iter on top — unwind_to_catch will leave
      // [thrown, iter, ..] for IteratorCloseThrow.
      let e = emit_ir(e, IrGetIterator)
      let e = emit_ir(e, IrPushTry(close_throw))
      use #(e, rested) <- result.map(emit_array_assign_elements(e, elements))
      let e = case rested {
        // Rest path already PopTry'd, drained iter, assigned. Stack at base.
        True -> emit_ir(e, IrJump(done_label))
        // §13.15.5.2: if [[Done]] is false, IteratorClose. We don't track
        // [[Done]] yet (IteratorNext doesn't undef the slot), so this also
        // fires on exhausted iterators — a no-op for builtins, observable
        // only on user iters with .return.
        False ->
          e
          |> emit_ir(IrPopTry)
          |> emit_ir(IrIteratorClose)
          |> emit_ir(IrJump(done_label))
      }
      // Abrupt: [thrown, iter, ..]. CloseThrow swallows .return()
      // result/error and rethrows the original (§7.4.11 step 5).
      let e = emit_ir(e, IrLabel(close_throw))
      let e = emit_ir(e, IrIteratorCloseThrow)
      emit_ir(e, IrLabel(done_label))
    }

    ast.ObjectExpression(properties) -> {
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
) -> Result(#(Emitter, Bool), EmitError) {
  case elements {
    [] -> Ok(#(e, False))
    // Hole: step iterator, discard done+value.
    [None, ..rest] -> {
      let e = emit_ir(e, IrIteratorNext)
      let e = emit_ir(e, IrPop)
      let e = emit_ir(e, IrPop)
      emit_array_assign_elements(e, rest)
    }
    // Rest: [[Done]] becomes true the moment we start draining → no close on
    // ANY subsequent throw. Pop F_body first so those throws skip the guard.
    [Some(ast.SpreadElement(argument:)), ..] -> {
      // [iter], try=[F_body,..] → PopTry → IteratorRest → [arr]
      let e = emit_ir(e, IrPopTry)
      let e = emit_ir(e, IrIteratorRest)
      use e <- result.map(emit_destructuring_assign(e, argument))
      #(e, True)
    }
    [Some(target), ..rest] -> {
      let e = emit_ir(e, IrIteratorNext)
      // [done, value, iter] — discard done, assign value.
      let e = emit_ir(e, IrPop)
      use e <- result.try(emit_destructuring_assign(e, target))
      emit_array_assign_elements(e, rest)
    }
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
      |> emit_ir(put_field_op(prop))
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
      let e = emit_ir(e, IrGetElem)
      use e <- result.map(emit_destructuring_assign(e, value))
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
  let #(e, close_throw) = fresh_label(e)
  let #(e, done_label) = fresh_label(e)
  // [source] → [iter]. PushTry immediately after so the recorded stack_depth
  // has iter on top — unwind_to_catch will leave [thrown, iter, ..].
  let e = emit_ir(e, IrGetIterator)
  let e = emit_ir(e, IrPushTry(close_throw))
  use #(e, rested) <- result.map(emit_array_elements(e, elements, binding_kind))
  let e = case rested {
    // Rest path already PopTry'd, drained iter, bound it. Stack at base.
    // [[Done]]=true after rest → no close on any completion. (Throws from
    // non-rest elements BEFORE the rest happened while F_body was still
    // active, so the catch target below is still reachable for those.)
    True -> emit_ir(e, IrJump(done_label))
    False ->
      // §8.6.2: if [[Done]] is false, IteratorClose. We don't track [[Done]]
      // yet (IteratorNext doesn't undef the slot), so this also fires on
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
  let e = Emitter(..e, strict: True)
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
  use #(e, static_init_idx) <- result.map(case super_class {
    Some(parent_expr) ->
      compile_derived_class(e, display_name, parent_expr, body)
    None -> compile_base_class(e, display_name, body)
  })
  // [ctor]. §15.7.14 step 26: bind inner name to F BEFORE static element
  // evaluation (step 31) so `class C { static x = C }` sees the constructor.
  let e = case binding_name {
    Some(n) -> e |> emit_ir(IrDup) |> emit_ir(IrScopeInitVar(n))
    None -> e
  }
  let e = emit_call_static_init(e, static_init_idx)
  let e = emit_op(e, LeaveScope)
  Emitter(..e, strict: saved_strict)
}

fn compile_derived_class(
  e: Emitter,
  name: Option(String),
  parent_expr: ast.Expression,
  body: List(ast.ClassElement),
) -> Result(#(Emitter, Option(Int)), EmitError) {
  let #(
    ctor_method,
    instance_methods,
    static_methods,
    instance_fields,
    static_elements,
  ) = classify_class_body(body)

  // Build constructor: if none provided, synthesize the spec default derived
  // constructor (§15.7.14 step 14): constructor(...args) { super(...args); }.
  // Arc doesn't support rest parameters yet, so spread `arguments` instead —
  // observably equivalent here since the synthetic body never re-reads it.
  let #(ctor_params, ctor_body) = case ctor_method {
    Some(ast.ClassMethod(value: ast.FunctionExpression(_, params, body, ..), ..)) -> #(
      params,
      body,
    )
    _ -> #(
      [],
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
      ]),
    )
  }

  // §13.3.7.1 SuperCall step 12 / §15.7.14 step 28: instance fields are
  // compiled into one synthetic initializer function (QuickJS class_fields_init
  // / spec [[Fields]]), bound to the <class_fields_init> const and called via
  // emit_field_init_call after every `super()`. Constructors cannot be
  // generators or async (spec forbids it).
  let #(e, init_idx) =
    compile_class_init_fn(e, field_init_stmts(instance_fields))
  let child =
    compile_function_body(
      e,
      name,
      ctor_params,
      ctor_body,
      False,
      False,
      False,
      // Class constructor — IS a constructor.
      True,
      opcode.derived_ctor_perms,
      option.map(init_idx, fn(_) { FieldInitAfterSuper })
        |> option.unwrap(NoFieldInit),
    )
  // Mark as derived constructor
  let child = CompiledChild(..child, is_derived_constructor: True)
  let #(e, ctor_idx) = add_child_function(e, child)

  // Step 1: Emit parent expression → [parent]
  use e <- result.try(emit_expr(e, parent_expr))

  // Step 2: MakeClosure for the derived constructor → [parent, ctor]
  let e = emit_ir(e, IrMakeClosure(ctor_idx))

  // Step 3: SetupDerivedClass → [ctor] (wires prototype chain)
  let e = emit_ir(e, IrSetupDerivedClass)

  // Step 4: Define instance methods on ctor.prototype (same as base class)
  use e <- result.try(emit_class_methods(
    e,
    instance_methods,
    on_prototype: True,
  ))

  // Step 5: Define static methods on ctor
  use e <- result.try(emit_class_methods(e, static_methods, on_prototype: False))

  // Step 6: Attach the field-initializer closure to ctor as [[Fields]]
  // (§15.7.14 step 25). After methods so the init fn's [[HomeObject]] = proto
  // sees them via super.method(). Stack: [ctor] → [ctor]. Static elements are
  // emitted by compile_class AFTER inner-name binding (step 26 < step 31).
  let e = emit_attach_field_init(e, init_idx)
  let #(e, static_init_idx) =
    compile_class_init_fn(e, static_init_stmts(static_elements))

  // Stack: [ctor]
  Ok(#(e, static_init_idx))
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
          IrDefineAccessor(name, opcode.Getter),
        )
        ast.MethodSet -> #(
          "set " <> name,
          IrDefineAccessor(name, opcode.Setter),
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
      let child =
        compile_function_body(
          e,
          Some(fn_name),
          params,
          body,
          False,
          is_gen,
          is_async,
          // Class method / getter / setter — not a constructor.
          False,
          opcode.method_perms,
          NoFieldInit,
        )
      let #(e, idx) = add_child_function(e, child)
      let e = emit_ir(e, IrMakeClosure(idx))
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
  let child =
    compile_function_body(
      e,
      None,
      params,
      body,
      False,
      is_gen,
      is_async,
      // Computed class method / getter / setter — not a constructor.
      False,
      opcode.method_perms,
      NoFieldInit,
    )
  let #(e, idx) = add_child_function(e, child)
  let e = emit_ir(e, IrMakeClosure(idx))
  let e = case kind {
    ast.MethodMethod -> emit_ir(e, IrDefineMethodComputed)
    ast.MethodGet -> emit_ir(e, IrDefineAccessorComputed(opcode.Getter))
    ast.MethodSet -> emit_ir(e, IrDefineAccessorComputed(opcode.Setter))
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

/// Splice field-init statements immediately after the first top-level
/// `super(...)` ExpressionStatement. If none found (super() nested or absent),
/// append at end — runtime TDZ on the `this` slot will still enforce ordering.
fn compile_base_class(
  e: Emitter,
  name: Option(String),
  body: List(ast.ClassElement),
) -> Result(#(Emitter, Option(Int)), EmitError) {
  // Separate class elements into categories
  let #(
    ctor_method,
    instance_methods,
    static_methods,
    instance_fields,
    static_elements,
  ) = classify_class_body(body)

  // Build the constructor body statement, injecting field initializers at the top
  let #(ctor_params, ctor_body) = case ctor_method {
    Some(ast.ClassMethod(value: ast.FunctionExpression(_, params, body, ..), ..)) -> #(
      params,
      body,
    )
    _ -> #([], ast.BlockStatement([]))
  }

  // Instance fields → synthetic init fn (compiled as a sibling of the ctor in
  // this enclosing scope). FieldInitAtStart tells the ctor body to call it
  // before user code runs (§10.2.2 [[Construct]] step 6).
  let #(e, init_idx) =
    compile_class_init_fn(e, field_init_stmts(instance_fields))
  let child =
    compile_function_body(
      e,
      name,
      ctor_params,
      ctor_body,
      False,
      False,
      False,
      // Class constructor — IS a constructor.
      True,
      opcode.method_perms,
      option.map(init_idx, fn(_) { FieldInitAtStart })
        |> option.unwrap(NoFieldInit),
    )
  let #(e, ctor_idx) = add_child_function(e, child)

  // Step 1: MakeClosure for the constructor (creates .prototype + .prototype.constructor)
  let e = emit_ir(e, IrMakeClosure(ctor_idx))
  // Stack: [ctor]

  // §15.7.14 step 12: ctor.[[HomeObject]] = ctor.prototype, so `super.x` in a
  // base ctor body resolves through ctor.prototype.__proto__ (Object.prototype).
  // Derived ctors get this from the SetupDerivedClass handler instead.
  let e =
    e
    |> emit_ir(IrDup)
    |> emit_ir(IrGetField("prototype"))
    |> emit_ir(IrSwap)
    |> emit_ir(IrMakeMethod)
    |> emit_ir(IrSwap)
    |> emit_ir(IrPop)
  // Stack: [ctor]

  // Step 2: Define instance methods on ctor.prototype
  use e <- result.try(emit_class_methods(
    e,
    instance_methods,
    on_prototype: True,
  ))

  // Step 3: Define static methods on ctor
  use e <- result.try(emit_class_methods(e, static_methods, on_prototype: False))

  // Step 4: Attach the field-initializer closure to ctor as [[Fields]]
  // (§15.7.14 step 25). Stack: [ctor] → [ctor]. Static elements are emitted by
  // compile_class AFTER inner-name binding (step 26 < step 31).
  let e = emit_attach_field_init(e, init_idx)
  let #(e, static_init_idx) =
    compile_class_init_fn(e, static_init_stmts(static_elements))
  Ok(#(e, static_init_idx))
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

fn string_inspect_stmt_kind(stmt: ast.Statement) -> String {
  case stmt {
    ast.EmptyStatement -> "EmptyStatement"
    ast.ExpressionStatement(..) -> "ExpressionStatement"
    ast.BlockStatement(_) -> "BlockStatement"
    ast.VariableDeclaration(..) -> "VariableDeclaration"
    ast.ReturnStatement(_) -> "ReturnStatement"
    ast.IfStatement(..) -> "IfStatement"
    ast.ThrowStatement(_) -> "ThrowStatement"
    ast.WhileStatement(..) -> "WhileStatement"
    ast.DoWhileStatement(..) -> "DoWhileStatement"
    ast.ForStatement(..) -> "ForStatement"
    ast.ForInStatement(..) -> "ForInStatement"
    ast.ForOfStatement(..) -> "ForOfStatement"
    ast.SwitchStatement(..) -> "SwitchStatement"
    ast.TryStatement(..) -> "TryStatement"
    ast.BreakStatement(_) -> "BreakStatement"
    ast.ContinueStatement(_) -> "ContinueStatement"
    ast.DebuggerStatement -> "DebuggerStatement"
    ast.LabeledStatement(..) -> "LabeledStatement"
    ast.WithStatement(..) -> "WithStatement"
    ast.FunctionDeclaration(..) -> "FunctionDeclaration"
    ast.ClassDeclaration(..) -> "ClassDeclaration"
    ast.ClassFieldInit(..) -> "ClassFieldInit"
  }
}

fn string_inspect_expr_kind(expr: ast.Expression) -> String {
  case expr {
    ast.Identifier(_) -> "Identifier"
    ast.NumberLiteral(_) -> "NumberLiteral"
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
  }
}
