/// AST Emission
///
/// Walks the AST and produces a `List(IrOp)` directly. Variable references
/// are resolved inline against the AST-level `ScopeTree` (wired onto the
/// Emitter at each entry point) and emitted as concrete IrGetLocal /
/// IrGetBoxed / IrGetGlobal / IrWith* ops; jump targets use integer label
/// IDs (IrJump) resolved in the label-resolution pass.
import arc/compiler/ast_util
import arc/compiler/scope.{
  type BindingKind, type GlobalFallthrough, type ScopeId, type TopLevelLex,
  CaptureBinding, CatchBinding, ConstBinding, FnNameBinding, LetBinding,
  LexGlobal, LexLocal, ParamBinding, ToEvalEnv, ToGlobal, VarBinding,
  root_scope_id,
}
import arc/parser/ast
import arc/vm/opcode.{
  type IrOp, type LabelId, type TryKind, CatchOnly, Finally,
  IrAsyncYieldStarNext, IrAsyncYieldStarResume, IrBinOp, IrDefineAccessor,
  IrDefineField, IrDefineMethod, IrDeleteField, IrFinal, IrGetField, IrGetField2,
  IrGosub, IrJump, IrJumpIfFalse, IrJumpIfNullish, IrJumpIfTrue, IrLabel,
  IrPushTry, IrPutField, IterCloseGuard, LabelId,
}
import arc/vm/value.{
  type JsValue, Finite, JsBool, JsNull, JsNumber, JsString, JsUndefined,
  JsUninitialized,
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

// `BindingKind`, `GlobalFallthrough`, `TopLevelLex`, `ScopeId`, `ScopeKind`,
// `Scope`, `Binding`, `ScopeTree`, `FunctionInfo`, `root_scope_id` are owned
// by scope.gleam (imported above) so the emit→scope import direction is
// acyclic — scope.gleam imports nothing from emit. The emitter consumes
// the analyzer's `scope.ScopeTree` directly; per-function state
// (local_count, fallthrough, lexical-slot layout) is read from
// `scope.function_info(tree, fn_scope)`.

/// A compiled child function (before Phase 2/3).
pub type CompiledChild {
  CompiledChild(
    /// This function's scope id in the whole-program scope tree — the key
    /// `compile_child` uses to look up the analyzer's `FunctionInfo`
    /// (capture set, local_count, lexical slots, name table). Threaded
    /// through from the per-function tree's `root`.
    scope_id: ScopeId,
    name: Option(String),
    arity: Int,
    /// §15.1.5 ExpectedArgumentCount — value for the `length` property:
    /// formal params before the first one with a default initializer
    /// (rest param excluded). `arity` still counts all fixed params.
    length: Int,
    code: List(IrOp),
    constants: List(JsValue),
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
    /// Per-binding "is referenced in this body or a nested arrow" flags.
    /// For arrows, the parent must capture/box the corresponding slot; for
    /// non-arrows this is informational (they own their slots).
    lexical_refs: opcode.LexicalRefs,
    /// True if this body (or a nested arrow) emitted an
    /// IrScope*Var("arguments"). For arrows, the enclosing non-arrow uses
    /// this to decide whether to materialise its `arguments` object.
    references_arguments: Bool,
    /// What kind of code this body is; direct eval inherits it.
    code_kind: opcode.CodeKind,
  )
}

/// Result of a top-level emit (`emit_program` / `emit_eval_direct` /
/// `emit_module`). Labelled record so field order is irrelevant and adding a
/// field is a compile error at every construction site — the previous bare
/// 5/6-tuple carried an unlabelled `Bool` for strictness.
pub type EmitOutput {
  EmitOutput(
    code: List(IrOp),
    constants: List(JsValue),
    children: List(CompiledChild),
    is_strict: Bool,
    /// Post-emission scope tree — MUST replace the caller's pre-emission
    /// copy. `fresh_slot` mints scratch locals by bumping
    /// `FunctionInfo.local_count` here; reading `function_info` from the
    /// input tree under-sizes the runtime locals tuple (IrPutLocal on a
    /// scratch slot → `badarg setelement`).
    tree: scope.ScopeTree,
    /// Top-level function declarations as (name, func_index) pairs, for
    /// module cyclic-function hoisting. Empty for non-module units.
    hoisted_funcs: List(#(String, Int)),
  )
}

/// A frame on the emitter's break/continue/return unwind stack. Mirrors
/// QuickJS BlockEnv (quickjs.c:21320). A frame is either a *target* (some
/// break/continue jumps to it) or a *barrier* (jumps only ever cross it,
/// emitting cleanup on the way out). Each variant carries only the fields
/// its kind actually uses — a barrier has no break target, a labeled block
/// has no continue target, and there are no `-1` sentinels anywhere.
type Frame {
  /// Iteration statement: both a break and a continue target.
  ///
  /// `iterator: True` (for-of / for-await-of) means the body runs under one
  /// try frame (F_body) with the iterator on the value stack, so crossing
  /// this frame emits IrPopTry followed by IrIteratorClose (with an IrSwap
  /// first when a return value sits above iter). Everything else about a
  /// loop frame is transparent to a jump that crosses it.
  LoopFrame(
    break_target: Int,
    continue_target: Int,
    label: Option(String),
    iterator: Bool,
  )
  /// Switch statement: a break target (labeled or unlabeled) but never a
  /// continue target — `continue` walks straight past it to the enclosing
  /// loop. Transparent when crossed.
  SwitchFrame(break_target: Int, label: Option(String))
  /// `foo: { … }` — a labeled non-loop block. Only a *labeled* break targets
  /// it; unlabeled `break` skips it (§14.8: targets the nearest
  /// IterationStatement or SwitchStatement only). Transparent when crossed.
  LabeledBlockFrame(break_target: Int, label: String)
  /// try / catch / finally body: never a target, only ever crossed. Makes
  /// break/continue/return that jump out emit the right cleanup:
  /// - `pop_try`: IrPopTry ops to keep try_stack balanced (QuickJS
  ///   push_break_entry, quickjs.c:28826-28828).
  /// - `label_finally`: if Some(L), after dropping try frames emit
  ///   `push undef; Gosub(L); Pop` so the finally subroutine runs before the
  ///   jump proceeds. Mirrors BlockEnv.label_finally (quickjs.c:21326).
  /// - `drop_count`: value-stack slots to discard when crossed. Non-zero
  ///   only on the finally-body barrier, where it drops [slot, gosub_retpc]
  ///   so a break/continue/return inside finally never reaches IrRet —
  ///   spec-correct completion replacement (§14.15.3). Mirrors
  ///   BlockEnv.drop_count (quickjs.c:21325).
  BarrierFrame(pop_try: Int, label_finally: Option(Int), drop_count: Int)
}

/// The emitter state, threaded through all emit functions.
pub opaque type Emitter {
  Emitter(
    code: List(IrOp),
    constants_map: Dict(JsValue, Int),
    constants_list: List(JsValue),
    next_const: Int,
    next_label: Int,
    frame_stack: List(Frame),
    functions: List(CompiledChild),
    next_func: Int,
    /// Set by LabeledStatement before emitting a loop body.
    /// Consumed by push_loop / push_switch to attach the label to the frame.
    pending_label: Option(String),
    /// True if the current compilation unit is strict. Inherited by child
    /// functions; can be upgraded (never downgraded) by a "use strict"
    /// directive in the function body prologue. Classes force strict.
    strict: Bool,
    /// True while emitting an async function body. Checked by yield* to
    /// route to the async-delegation path (GetAsyncIterator + await).
    is_async: Bool,
    /// True while emitting an arrow function body. Gates whether
    /// `DeclareLexical` and the `arguments` local are emitted in the prologue.
    is_arrow: Bool,
    /// Per-binding "has get_lexical(ref) been emitted in this body, or
    /// propagated up from an inner ARROW child via add_child_function".
    /// Mirrors JSC's InnerArrowFunctionCodeFeatures — lets the compiler
    /// decide capture/box without re-walking the IR.
    lexical_refs: opcode.LexicalRefs,
    /// True once an IrScope*Var("arguments") has been emitted in this body,
    /// or propagated up from an inner ARROW child via add_child_function.
    /// Same propagation rule as `lexical_refs` (arrows inherit the enclosing
    /// `arguments` binding, non-arrows own their own). Read after body
    /// emission to decide whether the `arguments` object is created — so
    /// functions that never touch `arguments` pay zero allocation cost.
    references_arguments: Bool,
    /// What kind of code the body being emitted is. Arrows inherit the
    /// parent's verbatim; non-arrows take it from their function kind.
    code_kind: opcode.CodeKind,
    /// Where top-level let/const/class go. LexGlobal only for the REPL
    /// program emitter; everything else (scripts, eval, modules, function
    /// bodies) uses LexLocal.
    top_lex: TopLevelLex,
    /// AST-level scope analysis result. Carries every scope's binding list
    /// (with final slot indices and boxing decisions) and the §B.3.2
    /// Annex-B-blocked name set. Built once before emission; the emitter
    /// reads it, never mutates it.
    scope_tree: scope.ScopeTree,
    /// The function-kind scope id (in `scope_tree`) this emitter is rooted
    /// at — `root_scope_id` for the top-level entry points, the child's
    /// own scope id when `compile_function_body` enters a nested body.
    /// Owns this body's `FunctionInfo` (local_count, fallthrough,
    /// lexical-slot layout); `scope_parent_in_fn` stops the scope-chain
    /// walk here so name resolution / Annex B promotion never reads the
    /// PARENT frame's slot indices.
    fn_scope: ScopeId,
    /// Cursor into `scope_tree` — the scope the emitter is currently inside.
    /// Descending into a Block/For/Catch/Class scope updates this in place
    /// (no positional marker in the IR stream); leaving restores the parent.
    /// Replaces the old `scope_depth: Int` nesting counter.
    current_scope: ScopeId,
    /// Source-order child-scope ids of `current_scope` not yet entered.
    /// `enter_scope` pops the head; `leave_scope` restores the saved tail so
    /// sibling scopes are consumed in the same pre-order the analyzer
    /// numbered them.
    scope_cursor: List(ScopeId),
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
    /// §9.1.1.4.17 CreateGlobalVarBinding's D argument for the top-level
    /// var / hoisted-function bindings this unit sends to the global object
    /// (IrDeclareGlobalVar). False for scripts (§9.1.1.4.18 passes
    /// D = false, so `delete x` on a top-level var is false and the
    /// binding survives). True only for eval units — direct
    /// (emit_eval_direct) and global/indirect (emit_program with
    /// deletable_global_vars: True): §19.2.1.3 EvalDeclarationInstantiation
    /// passes D = true, so an eval-introduced global IS deletable. Never
    /// inherited by child function emitters — only a unit's own top level
    /// emits IrDeclareGlobalVar.
    deletable_global_vars: Bool,
    /// True while compiling the SYNTHESIZED default constructor of a
    /// derived class (§15.7.14 ClassDefaultConstructor). Its implicit
    /// `super(...args)` forwards the received argument List as-is — unlike
    /// a source-level spread it performs NO array iteration, so
    /// %Array.prototype%[@@iterator] must not be observable there
    /// (test262 default-constructor-spread-override). Set on the parent
    /// emitter around that one compile_function_body call and inherited by
    /// the child; the synthesized body cannot nest further functions.
    in_synth_default_ctor: Bool,
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
    /// parent's stack). The scope tree's `inherited_with_slots` /
    /// `fn_with_stack_free` (computed by `scope.finalize`) handle
    /// cross-function inheritance from the tree.
    with_stack: List(String),
    /// Private names ("#x") visible at the current emission point — the
    /// running [[PrivateEnvironment]] chain. compile_class prepends its
    /// class's declared private names for the duration of the class body;
    /// child function emitters inherit the parent's list. Stamped onto
    /// IrCallEval so direct eval parses with the caller's private
    /// environment (§19.2.1.1 PerformEval step 5).
    private_env: List(String),
    /// §14 completion values: Some(slot) while emitting a loop / labeled /
    /// switch statement in tail position (the eval/program completion value).
    /// `slot` is an anonymous scratch local (alloc_scratch) that tracks the
    /// spec's V: expression statements store into it instead of popping;
    /// loops, if and switch reset it to undefined on entry (UpdateEmpty with
    /// undefined). Never captured (no name), never inherited by child
    /// function emitters (fresh new_emitter()).
    completion_var: Option(Int),
    /// Scratch slots returned by closed refs (emit_var_ref_put), reused by
    /// later emit_var_ref_make calls instead of allocating a fresh one.
    ref_free: List(Int),
    /// Slot indices of let bindings whose `emit_var_init` has already run
    /// (linearly earlier in the emitted op stream). A store to a let slot
    /// NOT in this set may execute while the binding is still in TDZ —
    /// §9.1.1.1.5 SetMutableBinding step 5 — so it gets a checked store
    /// (TDZ-check read before the put). Slots are never reused across
    /// bindings in a function frame, so a plain Int set is sound.
    initialized: Set(Int),
    /// Emission-order child FUNCTION-scope ids nested in `fn_scope`'s
    /// frame (i.e. `scope.child_function_scopes(scope_tree, fn_scope)`),
    /// not yet consumed. `compile_function_body` pops the head to learn
    /// the scope id the child being compiled was assigned by the analyzer
    /// — used both to project the child's per-function tree and to fill
    /// `CompiledChild.scope_id` so `compiler.compile_child` finds its
    /// `FunctionInfo`. The analyzer's `declare_stmts_hoist_order` walks
    /// each statement list with direct FunctionDeclarations first so this
    /// list matches the order `collect_hoisted_funcs` + in-place emission
    /// pop it.
    child_fn_cursor: List(ScopeId),
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

/// One element of a synthetic class-initializer function body (the instance
/// [[Fields]] closure, or the static-element wrapper). NOT part of the parser
/// AST: class lowering builds these directly, and each constructor names the
/// opcode `emit_field_init` emits — so an initializer can never be lowered to
/// the wrong define-op. §7.3.32 DefineField uses CreateDataPropertyOrThrow
/// ([[DefineOwnProperty]]), never [[Set]], so prototype setters are not
/// invoked and an own data property is always created (even shadowing an
/// inherited accessor).
type FieldInit {
  /// §7.3.29 PrivateMethodOrAccessorAdd — install an instance private
  /// method/accessor half. `closure_const` is the hidden class-scope const
  /// (ast_util.private_fn_const) holding the already-built closure; `kind`
  /// picks DefinePrivateMethod vs DefinePrivateAccessor(Getter/Setter).
  PrivateMethodInit(name: String, closure_const: String, kind: ast.MethodKind)
  /// §7.3.28 PrivateFieldAdd. `name` is the source `#x`, whose PrivateName is
  /// read from the class-scope const of the same name.
  PrivateFieldInit(name: String, init: ast.Expression)
  /// Literal identifier/string key — the field name also drives NamedEvaluation
  /// of an anonymous function initializer (§7.3.33 DefineField step 7).
  NamedFieldInit(name: String, init: ast.Expression)
  /// Numeric literal key (`class C { 1 = 2 }`).
  NumericFieldInit(value: ast.LiteralNumber, init: ast.Expression)
  /// Computed key, already evaluated + ToPropertyKey'd once at class-definition
  /// time into the hidden const `key_const` (`ast_util.computed_field_const` of
  /// the element's body index — see compile_class_body). The stashed key is read
  /// back — the name expression is NEVER re-evaluated (§15.7.14 step 27).
  ComputedFieldInit(key_const: String, init: ast.Expression)
  /// BigInt literal key (`class C { 1n = 2 }`), the remaining
  /// LiteralPropertyName form. Side-effect-free, so pushed inline.
  BigIntFieldInit(value: Int, init: ast.Expression)
  /// `static { ... }` — lowered to an arrow IIFE so the block gets its own var
  /// environment (§15.7.1 ClassStaticBlockBody) while inheriting
  /// `this`/[[HomeObject]] from the wrapper. Mirrors QuickJS
  /// emit_class_init_start/end.
  StaticBlockInit(body: List(ast.StmtWithLine))
}

/// The body of a function being compiled: real source statements, or the
/// synthetic field-initializer elements of a class-init function.
type FnBody {
  StmtsBody(stmts: List(ast.StmtWithLine))
  FieldInitsBody(inits: List(FieldInit))
}

/// QuickJS JS_ATOM_class_fields_init. Declared as a const in the per-class
/// block scope (P6) and captured via the ordinary closure path — `<...>` is
/// outside §12.7 IdentifierName grammar so it can't collide with user code.
/// P0 per-MakeClosure scope snapshots give each class its own slot, so two
/// classes in one parent function don't share a box.
const class_fields_init = ast_util.class_fields_init

/// Compile error from the emitter. `arc/compiler` re-exports this as
/// `CompileError`; `compiler.error_message` is the single renderer.
///
/// The two string-carrying variants are deliberately distinct so callers
/// (and readers) can tell "your code is illegal" from "the engine can't do
/// this yet":
///   - `EarlySyntaxError` — a spec-mandated early error the emitter (not the
///     parser) detects. The message is surfaced VERBATIM as a JS SyntaxError
///     message, so write it spec/engine-shaped ("Unexpected private name #x").
///   - `UnsupportedFeature` — a real JavaScript construct this engine does
///     not implement yet. Rendered as "unsupported: <feature>".
///
/// Every remaining variant names ONE AST shape the parser guarantees can
/// never reach the emitter. Reaching one is an engine bug, never a user
/// error — they exist as errors (rather than `panic`) only because the
/// emitter's `case` arms must be exhaustive. There is deliberately no
/// stringly-typed `Internal(context: String)` catch-all: a new impossible
/// state costs a new variant, and `compiler.error_message` cannot be handed
/// an arbitrary string.
pub type EmitError {
  BreakOutsideLoop
  ContinueOutsideLoop
  EarlySyntaxError(message: String)
  UnsupportedFeature(feature: String)
  /// `emit_lvalue_get2` reached a non-MemberExpression lvalue.
  NonMemberLValue
  /// Statement-position `class` with no name.
  AnonymousClassDeclaration
  /// A compound-assignment arm reached a non-compound operator.
  NonCompoundAssignOperator
  /// A for-in/of head declared more than one binding.
  MultiDeclaratorForHead
  /// A getter/setter/method property inside a destructuring pattern.
  AccessorInDestructuringPattern
  /// A defaulted destructuring target that is neither a static nor a
  /// computed member expression.
  NonMemberDefaultTarget
  /// `super` in bare expression position. The parser only produces
  /// `SuperExpression` under a MemberExpression object or a CallExpression
  /// callee, both of which have their own emit paths.
  BareSuperExpression
  /// `...x` in bare expression position. The parser only produces
  /// `SpreadElement` inside an argument list or an array literal, both of
  /// which have their own emit paths.
  BareSpreadElement
  /// `++`/`--` on a target that is neither an Identifier, a
  /// MemberExpression, nor a (web-compat) CallExpression. The parser rejects
  /// every other operand as an early error.
  InvalidUpdateTarget
  /// Compound assignment (`+=` …) onto a target that is neither an
  /// Identifier, a MemberExpression, nor a (web-compat) CallExpression. Only
  /// plain `=` admits a destructuring pattern target.
  InvalidCompoundAssignTarget
  /// A non-computed `MemberExpression` (`o.x`) whose property is not an
  /// Identifier. The parser only ever puts an Identifier there.
  NonIdentifierStaticMember
  /// The generic `UnaryExpression` arm reached `typeof` / `delete`, which
  /// have their own dedicated `emit_expr` arms above it.
  NonGenericUnaryOperator
}

// ============================================================================
// Public API
// ============================================================================

/// Emit IR for a list of top-level statements (script body).
/// `tree` is the AST-level scope-analysis result for this compilation unit;
/// it is wired onto the fresh Emitter before any emission so `resolve` /
/// `fresh_slot` / `at_global_lex` / `is_annexb_blocked` consult populated
/// scope data. `top_lex` is read from the tree.
/// Returns the emitter ops, constants, child functions, and script strictness.
/// `deletable_global_vars` is §9.1.1.4.17 CreateGlobalVarBinding's D
/// argument for this unit's top-level var / hoisted-function globals: False
/// for real scripts and the REPL (§16.1.7 GlobalDeclarationInstantiation
/// step 18 passes D = false), True for global eval code compiled through
/// this entry — indirect eval, and a global-caller eval that gets its own
/// unit (§19.2.1.3 EvalDeclarationInstantiation passes D = true, so an
/// eval-introduced global var / function IS deletable).
pub fn emit_program(
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
  emit_top_level_body(e, stmts, script_strict, True)
}

/// Emit IR for a DIRECT-eval body. Like emit_program(LexLocal) but takes the
/// §19.2.1.1 PerformEval contextual inputs as configuration so the caller
/// (compile_eval_direct) needs no post-emission op-list rewriting:
///
/// - `caller_is_strict` upgrades the body's strictness up front (step 16):
///   suppresses Annex B function-in-block hoisting and the §B.3.2.6 promote
///   copy (both gated on `e.strict`), and routes top-level var / function
///   names to LOCAL VarBinding slots instead of IrDeclareGlobalVar — strict
///   eval gets its own VariableEnvironment (step 18).
/// - `inherit_param_scope` / `inherit_private_env` seed the emitter's
///   `param_scope_names` / `private_env`, so any IrCallEval emitted at this
///   body's top level carries the OUTER call site's parameter-scope names
///   (step 3.d, transitive through nested sloppy direct eval) and
///   PrivateEnvironment (step 5). Classes in the eval body prepend to
///   `private_env`, so a nested eval inside a class extends/computed-key
///   sees both the eval body's own private names AND the inherited ones.
/// - The DeclareLexical(RefThis) prologue is SKIPPED — direct eval inherits
///   the caller's `this` (and other lexical pseudo-slots) as boxed captures,
///   so the body must not allocate its own `this` slot to shadow them.
///
/// Returns the same shape as emit_program. compile_eval_direct reads
/// `FunctionInfo.contains_direct_eval` from the scope tree directly to
/// decide whether to expose the local_names table, so no `has_eval_call`
/// side-channel is returned here.
pub fn emit_eval_direct(
  stmts: List(ast.StmtWithLine),
  tree: scope.ScopeTree,
  caller_is_strict: Bool,
  inherit_param_scope: List(String),
  inherit_private_env: List(String),
) -> Result(EmitOutput, EmitError) {
  let script_strict =
    caller_is_strict || ast_util.has_use_strict_directive(stmts)
  let e =
    Emitter(
      ..new_emitter(tree, root_scope_id),
      strict: script_strict,
      // Strict eval has its own VariableEnvironment — no parameter-scope
      // chain reaches a nested eval (step 3.d only walks lexEnv→varEnv).
      param_scope_names: case script_strict {
        True -> []
        False -> inherit_param_scope
      },
      private_env: inherit_private_env,
      // §19.2.1.3 EvalDeclarationInstantiation passes D = true to
      // CreateGlobalVarBinding / CreateGlobalFunctionBinding, so the
      // top-level vars a sloppy global-caller direct eval sends to the
      // global object (fallthrough ToGlobal) are created configurable.
      // Strict direct eval never emits IrDeclareGlobalVar (its vars are
      // locals in the eval's own VariableEnvironment), so the flag is
      // moot there.
      deletable_global_vars: True,
    )
  // No DeclareLexical(RefThis): the lexical pseudo-slots are the CALLER's
  // boxed slots, threaded as lexical_captures by compile_eval_direct.
  // Direct-eval units are always analyzed with top_lex=LexLocal, so the
  // shared trunk's LexGlobal branch is a no-op here.
  emit_top_level_body(e, stmts, script_strict, bool.negate(script_strict))
}

/// Emit IR for a module body. Always strict mode.
/// Accepts raw module items and handles export default internally
/// by declaring a `*default*` local binding (per ES spec §16.2.1.6.2).
/// The hoisted top-level function declarations are returned as
/// (name, func_index) pairs — used by the linker to instantiate exported
/// functions before any module body runs (cyclic function hoisting).
pub fn emit_module(
  items: List(ast.ModuleItem),
  tree: scope.ScopeTree,
) -> Result(EmitOutput, EmitError) {
  // Only ANONYMOUS defaults need the synthetic *default* binding —
  // `export default function fn() {}` / `class fn {}` declare `fn` itself
  // (§16.2.3.7 BoundNames) and are lowered to ordinary declarations below.
  // The analyzer pre-registers `*default*` (VarBinding) in the module
  // function scope when an anonymous default export is present.
  let stmts = ast_util.module_items_to_stmts(items)
  // Module-top-level using declarations: exported bindings must stay
  // module-scoped, so no AST try block — emit_module_using_top installs a
  // scope-free try frame instead so the DisposeResources sequence also
  // runs on abrupt module-body completion (spec: Module Evaluation calls
  // DisposeResources with the completion, normal OR throw). The body is
  // emitted DIRECTLY from the original `stmts` (no AST rewrite), so the
  // hoisting scans below see the user's using-bound names — they are
  // ConstBinding lexicals (collect_top_lex_names treats Using/AwaitUsing
  // as Const).
  let has_module_using = ast_util.has_using_decl(stmts)
  let e = Emitter(..new_emitter(tree, root_scope_id), strict: True)
  let e = enter_root_scope(e)
  // Module top-level `this === undefined` (§16.2.1.6.4) — scope.finalize
  // allocates slot 0 for RefThis; runtime padding leaves it JsUndefined.
  //
  // Hoisted var declarations (top-level function names are var-scoped too),
  // the *default* binding (when the module has a default export —
  // §16.2.1.6.2 step 24.b.i CreateMutableBinding, VarBinding so the
  // synthetic `*default* = expr` assignment below is a plain store), and
  // top-level let/const/class slots are all pre-registered in the
  // parser-built scope tree and seeded by enter_root_scope's
  // emit_binding_prologue — before
  // hoisted-func MakeClosure so closures capture the boxed slot, not a
  // stale pre-box value.

  // Collect and emit hoisted function declarations
  use #(e, hoisted_funcs) <- result.try(collect_hoisted_funcs(e, stmts))
  let e = emit_hoisted_funcs(e, hoisted_funcs)

  use e <- result.try(case has_module_using {
    False -> emit_stmts_tail(e, stmts)
    True -> emit_module_using_top(e, stmts)
  })

  let #(code, constants, children) = finish(e)
  Ok(EmitOutput(
    code:,
    constants:,
    children:,
    is_strict: True,
    tree: e.scope_tree,
    hoisted_funcs:,
  ))
}

// ============================================================================
// using / await using — direct IR emission (Explicit Resource Management)
// ============================================================================
//
// A statement list containing using declarations is lowered to a try frame
// over the body, with a DisposeResources sequence (proposal §2.1.4) running
// on both normal and abrupt completion. The lowering is emitted as raw IR
// here — no synthetic AST nodes — so user bindings keep their REAL source
// spans (used by error messages and source maps) and the only names
// introduced are emitter-internal `%u:` scratch locals.

/// Scratch-slot indices for one DisposeResources lowering — minted by
/// `build_using_scope`, seeded by `emit_using_prelude`, consumed by
/// `emit_using_body` (registers disposers) and `emit_using_dispose`
/// (reverse-order dispose + SuppressedError fold + conditional rethrow).
/// All slots are anonymous Int locals from `fresh_slot` — never captured,
/// never named, addressed directly via IrGetLocal/IrPutLocal.
type UsingScope {
  UsingScope(
    /// DisposeResources `completion` value.
    err: Int,
    has_err: Int,
    /// One disposer per resource, in declaration order.
    disposers: List(Disposer),
    /// True iff any slot is async — gates the await-coalescing scratch
    /// slots and the per-sync-resource pending-await flush.
    has_async: Bool,
    needs_await: Int,
    has_awaited: Int,
    tmp: Int,
    ok: Int,
  )
}

/// The scratch slot holding one resource's disposer method (or null), plus
/// whether it disposes async ([Symbol.asyncDispose], awaited).
type Disposer {
  Disposer(slot: Int, is_async: Bool)
}

/// One `using` / `await using` declarator, narrowed and slot-minted exactly
/// once by `build_using_scope`. The disposer travels WITH the binding it
/// disposes, so no later walk can pair a slot with the wrong resource.
type UsingResource {
  UsingResource(
    line: Int,
    name: String,
    init: ast.Expression,
    disposer: Disposer,
  )
}

/// A using-scope's body, lowered once: ordinary statements interleaved with
/// the individual using declarators, in source order.
type UsingItem {
  PlainItem(stmt: ast.StmtWithLine)
  ResourceItem(resource: UsingResource)
}

/// Lower `stmts` into `UsingItem`s, minting a fresh disposer slot for every
/// using/await-using declarator (in declaration order), plus the
/// completion-state and (if any resource is async) await-coalescing scratch
/// slots. Slot names come from `fresh_slot` so two using-scopes nested in
/// the SAME lexical scope (e.g. switch cases) never collide. The returned
/// items are the ONLY description of the body — `emit_using_body` walks
/// them, so the disposer slots and the resources they dispose can never
/// drift apart.
fn build_using_scope(
  e: Emitter,
  stmts: List(ast.StmtWithLine),
) -> #(Emitter, UsingScope, List(UsingItem)) {
  let #(e, items_rev) =
    list.fold(stmts, #(e, []), fn(acc, located) {
      let #(e, items) = acc
      case located.statement {
        ast.VariableDeclaration(kind: ast.Using, declarations:) ->
          lower_using_declarators(e, items, located.line, declarations, False)
        ast.VariableDeclaration(kind: ast.AwaitUsing, declarations:) ->
          lower_using_declarators(e, items, located.line, declarations, True)
        _ -> #(e, [PlainItem(located), ..items])
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
  let #(e, scope) = make_using_scope(e, disposers)
  #(e, scope, items)
}

/// One Using/AwaitUsing declaration's declarator list → one UsingResource
/// each, with a freshly minted disposer slot. The parser rejects both a
/// destructuring binding (`using [a] = …` → UsingPatternBinding) and a
/// missing initializer (`using x;` → UsingMissingInitializer), so this is
/// the point where those two guarantees are cashed in — once, for good.
fn lower_using_declarators(
  e: Emitter,
  items: List(UsingItem),
  line: Int,
  declarations: List(ast.VariableDeclarator),
  is_async: Bool,
) -> #(Emitter, List(UsingItem)) {
  list.fold(declarations, #(e, items), fn(acc, decl) {
    let #(e, items) = acc
    let assert ast.IdentifierPattern(name, ..) = decl.id
    let assert Some(init) = decl.init
    let #(e, slot) = fresh_slot(e)
    let resource =
      UsingResource(line:, name:, init:, disposer: Disposer(slot:, is_async:))
    #(e, [ResourceItem(resource), ..items])
  })
}

/// Mint the six completion/await-coalescing scratch slots and wrap them,
/// together with the supplied disposers, into a UsingScope. Shared by
/// `build_using_scope` (block bodies) and `single_using_scope` (for-of head).
fn make_using_scope(
  e: Emitter,
  disposers: List(Disposer),
) -> #(Emitter, UsingScope) {
  let has_async = list.any(disposers, fn(d) { d.is_async })
  let #(e, err) = fresh_slot(e)
  let #(e, has_err) = fresh_slot(e)
  let #(e, needs_await) = fresh_slot(e)
  let #(e, has_awaited) = fresh_slot(e)
  let #(e, tmp) = fresh_slot(e)
  let #(e, ok) = fresh_slot(e)
  #(
    e,
    UsingScope(
      err:,
      has_err:,
      disposers:,
      has_async:,
      needs_await:,
      has_awaited:,
      tmp:,
      ok:,
    ),
  )
}

/// Declare and initialise the scratch locals for one UsingScope. Each is a
/// LetBinding so IrScopePutVar in the body/handler/dispose may reassign it.
/// The `%u:` names cannot be referenced by user code, so declaration order
/// relative to hoisted functions is irrelevant (never captured).
fn emit_using_prelude(e: Emitter, scope: UsingScope) -> Emitter {
  let e = declare_scratch(e, scope.err, JsUndefined)
  let e = declare_scratch(e, scope.has_err, JsBool(False))
  let e =
    list.fold(scope.disposers, e, fn(e, d) {
      declare_scratch(e, d.slot, JsNull)
    })
  case scope.has_async {
    False -> e
    True ->
      e
      |> declare_scratch(scope.needs_await, JsBool(False))
      |> declare_scratch(scope.has_awaited, JsBool(False))
      |> declare_scratch(scope.tmp, JsUndefined)
      |> declare_scratch(scope.ok, JsBool(False))
  }
}

fn declare_scratch(e: Emitter, slot: Int, init: JsValue) -> Emitter {
  seed_local(e, slot, init)
}

/// Emit the body items from `build_using_scope`, registering each
/// using/await-using bound value's disposer in ITS OWN slot immediately after
/// that one const binding is initialised — per-declarator interleaving so
/// `using a = x, b = throwy()` has a's slot populated before `throwy()` runs
/// and `a` IS disposed (RS: BindingEvaluation — each binding's
/// AddDisposableResource runs before the next LexicalBinding's Initializer
/// evaluates). Routing the whole declaration through emit_stmt would init
/// a, b, … first and leave every slot null on a mid-list throw.
///
/// The fold carries the last line marker this function emitted (0 = unknown).
/// `set_line` only collapses a marker against an *immediately* preceding one,
/// and each declarator's disposer-register ops sit between the two, so
/// `using a = 1, b = 2;` would otherwise emit a redundant IrSetLine per extra
/// declarator. A PlainItem's statement can move the marker anywhere, so it
/// resets the tracker to unknown rather than claiming its own line.
fn emit_using_body(
  e: Emitter,
  items: List(UsingItem),
) -> Result(Emitter, EmitError) {
  use #(e, _last_line) <- result.map(
    list.try_fold(items, #(e, 0), fn(acc, item) {
      let #(e, last_line) = acc
      case item {
        ResourceItem(resource:) -> {
          let e = case resource.line == last_line {
            True -> e
            False -> set_line(e, resource.line)
          }
          use e <- result.map(emit_using_resource(e, resource))
          #(e, resource.line)
        }
        // Non-using statements emit normally (real source spans intact).
        PlainItem(located) -> {
          use e <- result.map(emit_stmt(
            set_line(e, located.line),
            located.statement,
          ))
          #(e, 0)
        }
      }
    }),
  )
  e
}

/// One using declarator, interleaving the const-bind and disposer-register:
/// declare(name) → eval(init) → init(name) → GetDisposer(name) →
/// PutVar(slot). Each declarator is fully registered before the next begins
/// so a throw from a later declarator's init OR its GetDisposer leaves every
/// earlier slot populated and those resources still dispose. The caller
/// (`emit_using_body`) owns the line marker.
fn emit_using_resource(
  e: Emitter,
  resource: UsingResource,
) -> Result(Emitter, EmitError) {
  let UsingResource(line: _, name:, init:, disposer:) = resource
  let e = declare_lex(e, name, True)
  use e <- result.map(emit_named_expr(e, init, name))
  e
  |> init_lex(name)
  |> emit_var_get(name)
  |> emit_op(opcode.GetDisposer(disposer.is_async))
  |> emit_scratch_put(disposer.slot)
}

/// DisposeResources error fold (proposal step 3.e.iii): given the thrown
/// value on top of stack, fold it into the scope's completion state.
///   err = has_err ? new SuppressedError(thrown, err) : thrown;
///   has_err = true;
/// Stack: [thrown, ..] → [..]
fn emit_using_merge_error(e: Emitter, scope: UsingScope) -> Emitter {
  let #(e, skip) = fresh_label(e)
  e
  |> emit_scratch_get(scope.has_err)
  |> emit_ir(IrJumpIfFalse(skip))
  // stack = [thrown]; push prior err on top for IrMakeSuppressed (the
  // opcode reads the new error from under the suppressed one — stack
  // [suppressed, error, ..] → [SuppressedError, ..]).
  |> emit_scratch_get(scope.err)
  |> emit_op(opcode.MakeSuppressed)
  |> emit_ir(IrLabel(skip))
  |> emit_scratch_put(scope.err)
  |> push_const(JsBool(True))
  |> emit_scratch_put(scope.has_err)
}

/// `try { body() } catch (e) { merge e into completion state }`
/// No barrier frame — the body is a fixed internal IR sequence with no
/// break/continue/return.
fn emit_using_try_merge(
  e: Emitter,
  scope: UsingScope,
  body: fn(Emitter) -> Emitter,
) -> Emitter {
  let #(e, catch_label) = fresh_label(e)
  let #(e, end_label) = fresh_label(e)
  e
  |> emit_ir(IrPushTry(catch_label, CatchOnly))
  |> body
  |> emit_op(opcode.PopTry)
  |> emit_ir(IrJump(end_label))
  |> emit_ir(IrLabel(catch_label))
  |> emit_using_merge_error(scope)
  |> emit_ir(IrLabel(end_label))
}

/// `if (needs_await && !has_awaited) { await undefined; [needs_await=false] }`
/// DisposeResources step 3.b (per sync resource, with reset) and step 4
/// (trailing, no reset). The Await(undefined) coalesces a run of
/// method-less async resources into a single suspension point.
fn emit_using_flush_pending(
  e: Emitter,
  scope: UsingScope,
  reset: Bool,
) -> Emitter {
  let #(e, skip) = fresh_label(e)
  let e =
    e
    |> emit_scratch_get(scope.needs_await)
    |> emit_ir(IrJumpIfFalse(skip))
    |> emit_scratch_get(scope.has_awaited)
    |> emit_ir(IrJumpIfTrue(skip))
    |> push_const(JsUndefined)
    |> emit_op(opcode.Await)
    |> emit_op(opcode.Pop)
  let e = case reset {
    False -> e
    True ->
      e
      |> push_const(JsBool(False))
      |> emit_scratch_put(scope.needs_await)
  }
  emit_ir(e, IrLabel(skip))
}

/// `<scratch slot> !== <constant>` → JumpIfFalse(target). Stack-neutral.
fn emit_jump_unless_strict_neq(
  e: Emitter,
  slot: Int,
  constant: JsValue,
  target: Int,
) -> Emitter {
  e
  |> emit_scratch_get(slot)
  |> push_const(constant)
  |> emit_ir(IrBinOp(opcode.StrictNotEq))
  |> emit_ir(IrJumpIfFalse(target))
}

/// One sync-hint resource slot:
///   if (dK !== null && dK !== undefined) {
///     [async lists only — flush pending coalesced await]
///     try { dK(); } catch (e) { merge }
///   }
fn emit_using_dispose_sync(
  e: Emitter,
  scope: UsingScope,
  slot: Int,
) -> Emitter {
  let #(e, skip) = fresh_label(e)
  let e = emit_jump_unless_strict_neq(e, slot, JsNull, skip)
  let e = emit_jump_unless_strict_neq(e, slot, JsUndefined, skip)
  let e = case scope.has_async {
    False -> e
    True -> emit_using_flush_pending(e, scope, True)
  }
  let e = {
    use e <- emit_using_try_merge(e, scope)
    e
    |> emit_scratch_get(slot)
    |> emit_op(opcode.Call(0))
    |> emit_op(opcode.Pop)
  }
  emit_ir(e, IrLabel(skip))
}

/// One async-hint resource slot:
///   if (dK !== null) {
///     if (dK !== undefined) {
///       ok = false;
///       try { tmp = dK(); ok = true; } catch (e) { merge }
///       if (ok) {
///         try { await tmp; } catch (e) { merge }
///         has_awaited = true;
///       }
///     } else { needs_await = true; }
///   }
fn emit_using_dispose_async(
  e: Emitter,
  scope: UsingScope,
  slot: Int,
) -> Emitter {
  let #(e, skip) = fresh_label(e)
  let #(e, no_method) = fresh_label(e)
  let #(e, after_await) = fresh_label(e)
  let e = emit_jump_unless_strict_neq(e, slot, JsNull, skip)
  let e = emit_jump_unless_strict_neq(e, slot, JsUndefined, no_method)
  // Has a real disposer: call it, then await the result, each step under
  // its own try-merge so a sync throw vs a rejection are folded the same.
  let e =
    e
    |> push_const(JsBool(False))
    |> emit_scratch_put(scope.ok)
  let e = {
    use e <- emit_using_try_merge(e, scope)
    e
    |> emit_scratch_get(slot)
    |> emit_op(opcode.Call(0))
    |> emit_scratch_put(scope.tmp)
    |> push_const(JsBool(True))
    |> emit_scratch_put(scope.ok)
  }
  let e =
    e
    |> emit_scratch_get(scope.ok)
    |> emit_ir(IrJumpIfFalse(after_await))
  let e = {
    use e <- emit_using_try_merge(e, scope)
    e
    |> emit_scratch_get(scope.tmp)
    |> emit_op(opcode.Await)
    |> emit_op(opcode.Pop)
  }
  e
  |> push_const(JsBool(True))
  |> emit_scratch_put(scope.has_awaited)
  |> emit_ir(IrLabel(after_await))
  |> emit_ir(IrJump(skip))
  // Method-less (`await using x = null/undefined`): record that the next
  // sync resource (or the trailing flush) owes one Await(undefined).
  |> emit_ir(IrLabel(no_method))
  |> push_const(JsBool(True))
  |> emit_scratch_put(scope.needs_await)
  |> emit_ir(IrLabel(skip))
}

/// DisposeResources(disposeCapability, completion): dispose each slot in
/// REVERSE declaration order, then the trailing coalesced Await(undefined),
/// then `if (has_err) throw err`. Stack-neutral.
fn emit_using_dispose(e: Emitter, scope: UsingScope) -> Emitter {
  let e =
    list.fold(list.reverse(scope.disposers), e, fn(e, d) {
      case d.is_async {
        True -> emit_using_dispose_async(e, scope, d.slot)
        False -> emit_using_dispose_sync(e, scope, d.slot)
      }
    })
  let e = case scope.has_async {
    False -> e
    True -> emit_using_flush_pending(e, scope, False)
  }
  let #(e, end) = fresh_label(e)
  e
  |> emit_scratch_get(scope.has_err)
  |> emit_ir(IrJumpIfFalse(end))
  |> emit_scratch_get(scope.err)
  |> emit_op(opcode.Throw)
  |> emit_ir(IrLabel(end))
}

/// Wrap `emit_body` in the using-declaration try/catch/finally:
///
///   PushTry(throw); PushTry(catch); barrier(2,fin); <body>; PopTry×2;
///   gosub fin; jump end;
///   catch: err←thrown; has_err←true; PopTry; gosub fin; jump end;
///   throw: Gosub fin; Throw;
///   fin: barrier(drop:2); <DisposeResources>; Ret;
///   end:
///
/// Shares emit_try_catch_finally's scaffold so break / continue / return
/// crossing the body run disposal via gosub before any outer iterator-close
/// or further finally subroutines. The catch arm has no scope of its own and
/// no synthetic param — the thrown value is consumed directly off the stack
/// into the scratch completion slots; it cannot itself throw, so the helper's
/// throw-entry is dead in practice (kept: outer PushTry needs a target).
fn emit_using_try_wrap(
  e: Emitter,
  scope: UsingScope,
  emit_body: fn(Emitter) -> Result(Emitter, EmitError),
) -> Result(Emitter, EmitError) {
  use e, catch_label, _fin <- emit_try_catch_finally(e, emit_body, fn(e) {
    Ok(emit_using_dispose(e, scope))
  })
  let e = emit_ir(e, IrLabel(catch_label))
  let e = emit_scratch_put(e, scope.err)
  let e = push_const(e, JsBool(True))
  Ok(emit_scratch_put(e, scope.has_err))
}

/// A UsingScope with a single disposer slot — for the body of a
/// `for ([await] using x of …)` loop, where the head-bound x is the lone
/// resource registered each iteration.
fn single_using_scope(
  e: Emitter,
  is_async: Bool,
) -> #(Emitter, UsingScope, Int) {
  let #(e, slot) = fresh_slot(e)
  let #(e, scope) = make_using_scope(e, [Disposer(slot:, is_async:)])
  #(e, scope, slot)
}

/// `for ([await] using x of xs)` head: name + async hint of the single
/// declarator. None when the head is not a using declaration.
fn for_of_using_hint(left: ast.ForInit) -> Option(#(String, Bool)) {
  case left {
    ast.ForInitDeclaration(kind:, declarations:) ->
      case kind, declarations {
        ast.Using, [ast.VariableDeclarator(ast.IdentifierPattern(name, ..), _)]
        -> Some(#(name, False))
        ast.AwaitUsing,
          [ast.VariableDeclarator(ast.IdentifierPattern(name, ..), _)]
        -> Some(#(name, True))
        _, _ -> None
      }
    _ -> None
  }
}

/// Body region of a `for ([await] using x of xs)` loop: build a one-resource
/// UsingScope, emit its prelude (scratch slots live in the for-of's
/// per-loop block scope and are re-initialised every iteration), register
/// the head-bound `x`, run `body`, then DisposeResources — all inside a
/// try/catch/finally so break/continue/return crossing it run disposal via
/// gosub before the enclosing for-of's F_body PopTry + IteratorClose.
fn emit_for_of_using_body(
  e: Emitter,
  name: String,
  is_async: Bool,
  body: ast.Statement,
) -> Result(Emitter, EmitError) {
  let #(e, scope, slot) = single_using_scope(e, is_async)
  let e = emit_using_prelude(e, scope)
  use e <- emit_using_try_wrap(e, scope)
  let e = emit_var_get(e, name)
  let e = emit_op(e, opcode.GetDisposer(is_async))
  let e = emit_scratch_put(e, slot)
  emit_stmt(e, body)
}

/// `for ([await] using x = a; c; u) body`: the resource is acquired once
/// before the loop and disposed once after — equivalent to
/// `{ using x = a; for (; c; u) body }`. Open a block scope for the user
/// const + scratch slots, build the UsingScope from the original
/// declaration, then wrap the const-bind+register and the headless loop in
/// the disposal try/catch/finally so break/return inside the body run
/// DisposeResources via gosub. pending_label is consumed by the inner
/// push_loop so labeled break/continue still target the loop.
fn emit_for_using_classic(
  e: Emitter,
  kind: ast.VariableKind,
  declarations: List(ast.VariableDeclarator),
  condition: Option(ast.Expression),
  update: Option(ast.Expression),
  body: ast.Statement,
) -> Result(Emitter, EmitError) {
  let #(e, save) = enter_scope(e, in_block: e.in_block)
  // Line 0 → set_line is a no-op (the enclosing emit_stmt already emitted
  // the IrSetLine for this statement's source line).
  let head = [
    ast.StmtWithLine(0, ast.VariableDeclaration(kind:, declarations:)),
  ]
  let #(e, scope, items) = build_using_scope(e, head)
  let e = emit_using_prelude(e, scope)
  use e <- result.map({
    use e <- emit_using_try_wrap(e, scope)
    // const x = a; <register disposer>; … (real spans on the user binding).
    use e <- result.try(emit_using_body(e, items))
    // Headless `for (; c; u) body` inside the try so the loop's break_label
    // sits under the disposal barrier.
    emit_classic_loop(e, condition, update, body, [])
  })
  leave_scope(e, save)
}

/// Headless `for (; condition; update) body` skeleton shared by the
/// plain ForStatement arm and the `for (using …)` lowering: allocate
/// the three labels, push/pop the loop ctx, re-box per-iteration `let`
/// bindings at the continue point. Caller owns scope + the init clause.
fn emit_classic_loop(
  e: Emitter,
  condition: Option(ast.Expression),
  update: Option(ast.Expression),
  body: ast.Statement,
  per_iter: List(String),
) -> Result(Emitter, EmitError) {
  let #(e, loop_start) = fresh_label(e)
  let #(e, loop_continue) = fresh_label(e)
  let #(e, loop_end) = fresh_label(e)
  let e = push_loop(e, loop_end, loop_continue)
  let e = emit_ir(e, IrLabel(loop_start))
  use e <- result.try(case condition {
    Some(cond) -> {
      use e <- result.map(emit_expr(e, cond))
      emit_ir(e, IrJumpIfFalse(loop_end))
    }
    None -> Ok(e)
  })
  use e <- result.try(emit_stmt(e, body))
  let e = emit_ir(e, IrLabel(loop_continue))
  let e = list.fold(per_iter, e, emit_var_rebox)
  use e <- result.map(case update {
    Some(upd) -> {
      use e <- result.map(emit_expr(e, upd))
      emit_op(e, opcode.Pop)
    }
    None -> Ok(e)
  })
  let e = emit_ir(e, IrJump(loop_start))
  let e = emit_ir(e, IrLabel(loop_end))
  pop_frame(e)
}

/// Module body containing top-level using declarations: emit the body
/// directly inside a scope-free try frame so exported const bindings stay
/// module-scoped (a BlockStatement wrapper would re-scope them away from
/// the module environment). An abrupt completion is caught, folded into
/// the module-scoped completion state, and the DisposeResources sequence
/// runs on both paths — its trailing `if (has_err) throw err` rethrows the
/// (possibly SuppressedError-folded) completion. Layout mirrors the
/// try/catch (no finally) lowering in emit_stmt, minus the block scope
/// around the protected statements and the catch handler — module top
/// level has no break/continue/return, so a finally subroutine is
/// unnecessary; both arms simply fall through to the dispose label.
fn emit_module_using_top(
  e: Emitter,
  stmts: List(ast.StmtWithLine),
) -> Result(Emitter, EmitError) {
  let #(e, scope, items) = build_using_scope(e, stmts)
  let e = emit_using_prelude(e, scope)

  let #(e, catch_label) = fresh_label(e)
  let #(e, dispose_label) = fresh_label(e)

  let e = emit_ir(e, IrPushTry(catch_label, CatchOnly))
  let e = push_barrier(e, pop_try: 1, label_finally: None, drop: 0)
  use e <- result.map(emit_using_body(e, items))
  let e = pop_frame(e)
  let e = emit_op(e, opcode.PopTry)
  let e = emit_ir(e, IrJump(dispose_label))

  // Handler: unwind_to_catch leaves stack = [thrown, ..base]. Store the
  // thrown value DIRECTLY into the module-scoped completion-state slots —
  // no catch-param block scope, no synthetic IdentifierPattern.
  let e =
    e
    |> emit_ir(IrLabel(catch_label))
    |> emit_scratch_put(scope.err)
    |> push_const(JsBool(True))
    |> emit_scratch_put(scope.has_err)

  let e = emit_ir(e, IrLabel(dispose_label))
  let e = emit_using_dispose(e, scope)
  // Module body completion value (unobservable for modules, but the
  // surrounding emit_module expects one on stack like the no-using
  // emit_stmts_tail path).
  push_const(e, JsUndefined)
}

/// Shared top-level emission trunk for emit_program / emit_eval_direct:
/// wires the analyzer tree, enters the root scope, emits the var / Annex-B
/// / global-lexical declaration prologue, hoists top-level function decls,
/// emits the body in tail position, and packs the `EmitOutput`.
///
/// `e` arrives pre-configured (strict set; eval also seeds
/// param_scope_names / private_env). `vars_to_global` routes hoisted var +
/// top-level function-declaration names through `emit_declare_var_global`:
/// True for scripts (always — global object) and sloppy direct eval
/// (caller's varEnv / global per `fallthrough`); False for strict direct
/// eval (§19.2.1.1 step 18: fresh VariableEnvironment → local slots already
/// seeded by enter_root_scope's binding prologue).
fn emit_top_level_body(
  e: Emitter,
  stmts: List(ast.StmtWithLine),
  script_strict: Bool,
  vars_to_global: Bool,
) -> Result(EmitOutput, EmitError) {
  // Position at the root function scope and emit its binding prologue.
  let e = enter_root_scope(e)
  // Hoisting pre-pass: top-level `var` + function-declaration names.
  let e = case vars_to_global {
    False -> e
    True ->
      list.append(
        ast_util.collect_hoisted_vars(stmts),
        ast_util.direct_fn_names(stmts),
      )
      |> list.fold(e, emit_declare_var_global)
  }
  // Annex B §B.3.2.2/.2.6: sloppy-mode function-in-block names get a var
  // binding (undefined) before the body runs. The analyzer precomputes the
  // candidate list on `FunctionInfo.annexb_candidates` (empty for strict
  // bodies; already excludes names blocked by a top-level let/const/class).
  // Routed via `emit_declare_var_global` so ToEvalEnv units emit
  // IrDeclareEvalVar.
  let e = case script_strict {
    True -> e
    False -> list.fold(fn_info(e).annexb_candidates, e, emit_declare_var_global)
  }
  // §16.1.7 GlobalDeclarationInstantiation: top-level let/const/class go to
  // the global lexical record (TDZ-uninitialized) before the body runs so a
  // closure called before the `let` executes sees TDZ, not a global-object
  // fallthrough write. LexLocal units (eval, embedded) pre-seeded these as
  // local slots in enter_root_scope's prologue — nothing to emit here.
  let e = case e.top_lex {
    LexLocal -> e
    LexGlobal ->
      list.fold(collect_top_lex_names(stmts), e, fn(e, lex) {
        let #(name, kind) = lex
        emit_op(e, opcode.DeclareGlobalLex(name, kind == ConstBinding))
      })
  }
  use #(e, hoisted_funcs) <- result.try(collect_hoisted_funcs(e, stmts))
  let e = emit_hoisted_funcs(e, hoisted_funcs)
  use e <- result.try(emit_stmts_tail(e, stmts))
  let #(code, constants, children) = finish(e)
  Ok(
    EmitOutput(
      code:,
      constants:,
      children:,
      is_strict: script_strict,
      tree: e.scope_tree,
      hoisted_funcs: [],
    ),
  )
}

// ============================================================================
// Emitter helpers
// ============================================================================

/// Fresh emitter wired to the analyzer's whole-program scope tree and
/// positioned at the function-kind scope `fn_id`. `top_lex` is mirrored
/// from the tree; `child_fn_cursor` is seeded with the source-order
/// function-kind descendants of `fn_id` so `compile_function_body` pops
/// them in the same order the analyzer numbered them.
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
    lexical_refs: opcode.no_lexical_refs,
    references_arguments: False,
    code_kind: opcode.ScriptCode,
    top_lex: tree.top_lex,
    scope_tree: tree,
    fn_scope: fn_id,
    current_scope: fn_id,
    scope_cursor: block_child_scopes(tree, fn_id),
    child_fn_cursor: scope.child_function_scopes(tree, fn_id),
    field_init: NoFieldInit,
    in_block: False,
    deletable_global_vars: False,
    in_synth_default_ctor: False,
    param_scope_names: [],
    with_stack: [],
    private_env: [],
    completion_var: None,
    ref_free: [],
    initialized: set.new(),
  )
}

/// True when the current statement is at the program top level (not inside
/// any block) and this compilation unit targets the global lexical record.
/// `!e.in_block` is the load-bearing depth signal: `enter_scope`'s
/// empty-cursor fallback leaves `current_scope` at root, so the root-id
/// check alone would misclassify a nested `{ let x }` as top-level.
/// `e.fn_scope == root_scope_id` is REQUIRED: `new_emitter` mirrors
/// `top_lex` from the whole-program tree onto every child-function emitter,
/// so in REPL mode a nested function body would otherwise satisfy the other
/// three checks and leak
/// its body-top `let`/`const` into the global lexical record.
fn at_global_lex(e: Emitter) -> Bool {
  e.top_lex == LexGlobal
  && e.fn_scope == root_scope_id
  && !e.in_block
  && e.current_scope == e.fn_scope
}

/// This compilation unit's `FunctionInfo` — local_count, fallthrough,
/// lexical-slot layout for the function-kind scope this emitter is rooted at.
fn fn_info(e: Emitter) -> scope.FunctionInfo {
  scope.function_info(e.scope_tree, e.fn_scope)
}

/// Where unresolved names fall through to in this compilation unit.
fn fn_fallthrough(e: Emitter) -> GlobalFallthrough {
  fn_info(e).fallthrough
}

/// Direct child scope ids of `id` that share THIS function's frame
/// (Block / Catch / With / ClassBody) — i.e. the scopes `enter_scope`
/// descends into. Function-kind children are handled separately by
/// `compile_function_body` consuming `child_fn_cursor`; including them
/// here would make `enter_scope` pop a function id when it expects a
/// block, desyncing both cursors. The analyzer's `children_at` lists ALL
/// direct children in source order, so filter here.
fn block_child_scopes(t: scope.ScopeTree, id: ScopeId) -> List(ScopeId) {
  use c <- list.filter(scope.child_scopes(t, id))
  !scope.is_function_kind(scope.get_scope(t, c).kind)
}

/// Parent scope id of `id` WITHIN this emitter's function frame, or None
/// at the function root. The analyzer tree's `Scope.parent` at `fn_scope`
/// points to the ENCLOSING function's scope; severing it here keeps the
/// emit-side scope-chain walks (`emit_binding_prologue`'s callers,
/// `annexb_find_source` / `annexb_find_target`) from crossing into the
/// parent frame and reading its slot indices — cross-function references
/// are CaptureBinding entries in `fn_scope` itself, so stopping at the
/// boundary is both necessary and sufficient.
fn scope_parent_in_fn(e: Emitter, id: ScopeId) -> Option(ScopeId) {
  case id == e.fn_scope {
    True -> None
    False -> scope.get_scope(e.scope_tree, id).parent
  }
}

/// Saved scope position — the parent scope id, its remaining unconsumed
/// child-scope ids, and the `in_block` flag — captured at `enter_scope`
/// and restored by `leave_scope`.
pub type ScopeSave {
  ScopeSave(scope: ScopeId, cursor: List(ScopeId), in_block: Bool)
}

/// Descend into the next source-order child scope of `current_scope`: pop
/// the head of `scope_cursor`, move the cursor to the child, and emit that
/// child's binding-init prologue (the IrPushConst undef/uninit + IrPutLocal
/// + IrBoxLocal sequence the old Phase-2 resolver emitted on each
/// DeclareVar). Returns the saved parent position for `leave_scope`.
///
/// Scope structure lives in the tree, not positionally in the IR stream —
/// no marker is emitted; only the cursor moves.
fn enter_scope(e: Emitter, in_block in_block: Bool) -> #(Emitter, ScopeSave) {
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
      #(emit_binding_prologue(e, child_id), save)
    }
    // Analyzer omitted this scope (a synthetic block with no declarations,
    // or the empty-tree bootstrap before the analyzer is wired). Do NOT
    // re-read the current scope's node here — that would re-emit its
    // binding prologue (re-seeding initialized let/const back to TDZ and
    // double-boxing already-boxed slots) AND reset the cursor to its full
    // children list (re-entering already-consumed siblings). Stay in
    // place: no prologue, no cursor change, only `in_block` is updated.
    [] -> {
      let save =
        ScopeSave(scope: e.current_scope, cursor: [], in_block: e.in_block)
      #(Emitter(..e, in_block:), save)
    }
  }
}

/// Restore the parent scope position saved by `enter_scope`.
fn leave_scope(e: Emitter, save: ScopeSave) -> Emitter {
  Emitter(
    ..e,
    current_scope: save.scope,
    scope_cursor: save.cursor,
    in_block: save.in_block,
  )
}

/// Conditionally consume the for-head Block scope: the parser only pushes
/// one for let/const/using heads (parse_for_declaration_scoped /
/// parse_for_using_scoped), NOT for var/expr heads. Returns `None` in the
/// var/expr case so the matching `leave_for_scope` is a true no-op — a
/// snapshot+restore would rewind the cursor past any sibling scope the
/// loop body consumed in between, desyncing the next sibling block.
fn enter_for_scope(
  e: Emitter,
  has_lex_head: Bool,
) -> #(Emitter, Option(ScopeSave)) {
  case has_lex_head {
    True -> {
      let #(e, save) = enter_scope(e, in_block: e.in_block)
      #(e, Some(save))
    }
    False -> #(e, None)
  }
}

/// Mirror of `enter_for_scope`: restore the head scope only when one was
/// actually entered. `None` leaves the cursor wherever the body left it.
fn leave_for_scope(e: Emitter, save: Option(ScopeSave)) -> Emitter {
  case save {
    Some(s) -> leave_scope(e, s)
    None -> e
  }
}

/// Position the emitter at the root function scope of its tree and emit
/// that scope's binding-init prologue. Used by emit_program / emit_module /
/// compile_function_body — the root scope has no parent to "enter" from,
/// so it does not go
/// through `enter_scope`'s child-cursor pop.
fn enter_root_scope(e: Emitter) -> Emitter {
  let e =
    Emitter(
      ..e,
      current_scope: e.fn_scope,
      scope_cursor: block_child_scopes(e.scope_tree, e.fn_scope),
    )
  let e = emit_binding_prologue(e, e.fn_scope)
  // Owned lexical pseudo-slots (this / active_func / home_object /
  // new.target) are NOT in `Scope.bindings` — they live in
  // `FunctionInfo.lexical` and are seeded by the runtime before pc=0
  // (frame.setup_frame). When an inner arrow / direct-eval captures one,
  // `analyze_captures` sets the matching `FunctionInfo.lexical_boxed` flag
  // and `resolve_lexical` reports the slot as boxed, so reads emit
  // IrGetBoxed — the slot must therefore be wrapped in a box here, in the
  // prologue, exactly as `emit_binding_prologue` does for named bindings.
  // The legacy resolver did this on the `DeclareLexical(ref)` marker; that
  // marker is gone, so the box step is reproduced from the analyzer's
  // `lexical_boxed` directly. INHERITED slots (arrow / direct-eval body —
  // `info.lexical` mirrors `info.lexical_captures`) arrive already boxed
  // from the parent and are skipped via the `lexical_captures` membership
  // guard, mirroring `emit_binding_prologue`'s CaptureBinding skip.
  let info = fn_info(e)
  use e, ref <- list.fold(opcode.all_lexical_refs, e)
  case
    dict.has_key(info.lexical_captures, ref),
    opcode.lexical_slot(info.lexical, ref),
    opcode.lexical_refs_get(info.lexical_boxed, ref)
  {
    False, Some(slot), True -> emit_op(e, opcode.BoxLocal(slot))
    _, _, _ -> e
  }
}

/// Emit the per-binding init/box ops for `scope_id`'s declarations — the
/// inline equivalent of the old Phase-2 `seed_and_box` (scope.gleam) that
/// ran on each DeclareVar marker:
///   var               → push undef; PutLocal slot; [BoxLocal slot]
///   let/const/fn-name → push uninit; PutLocal slot; [BoxLocal slot]
///   param/catch       → [BoxLocal slot]   (value set by call/unwind)
///   capture           → (nothing — already a box from the parent)
/// `linker_seeded` module exports skip the whole thing — the linker owns
/// the cell and seeds it before the body runs (only ever the top-level
/// root scope; checked here against `scope_tree.linker_seeded`).
/// Bindings are emitted in slot (= analyzer allocation = declaration)
/// order so seed/box ops match the legacy resolver's output exactly.
fn emit_binding_prologue(e: Emitter, scope_id: ScopeId) -> Emitter {
  let s = scope.get_scope(e.scope_tree, scope_id)
  let bindings =
    dict.to_list(s.bindings)
    |> list.sort(fn(a, b) { int.compare({ a.1 }.slot, { b.1 }.slot) })
  let at_module_root = scope_id == root_scope_id && e.fn_scope == root_scope_id
  use e, #(name, b) <- list.fold(bindings, e)
  let seeded = at_module_root && set.contains(e.scope_tree.linker_seeded, name)
  use <- bool.guard(seeded, e)
  let e = case b.kind {
    VarBinding -> seed_local(e, b.slot, JsUndefined)
    LetBinding | ConstBinding | FnNameBinding ->
      seed_local(e, b.slot, JsUninitialized)
    ParamBinding | CatchBinding | CaptureBinding -> e
  }
  case b.kind, b.is_boxed {
    CaptureBinding, _ -> e
    _, True -> emit_op(e, opcode.BoxLocal(b.slot))
    _, False -> e
  }
}

fn seed_local(e: Emitter, slot: Int, val: JsValue) -> Emitter {
  let #(e, idx) = add_constant(e, val)
  e |> emit_op(opcode.PushConst(idx)) |> emit_op(opcode.PutLocal(slot))
}

/// Read an anonymous scratch local slot (allocated via `fresh_slot`).
/// Scratch slots are never boxed and never named, so this is a direct
/// IrGetLocal — no scope resolution.
fn emit_scratch_get(e: Emitter, slot: Int) -> Emitter {
  emit_op(e, opcode.GetLocal(slot))
}

/// Store top-of-stack into an anonymous scratch local slot. Pops the value.
fn emit_scratch_put(e: Emitter, slot: Int) -> Emitter {
  emit_op(e, opcode.PutLocal(slot))
}

/// §B.3.2: is `name` blocked from Annex B var promotion at the current
/// scope? Reads the analyzer-computed set on the scope-tree node — replaces
/// the old emitter-threaded `annexb_blocked` / `annexb_level_fns` Sets.
fn is_annexb_blocked(e: Emitter, name: String) -> Bool {
  set.contains(
    scope.get_scope(e.scope_tree, e.current_scope).annexb_blocked,
    name,
  )
}

/// Resolved Annex B §B.3.2.6 promotion target — where the block-scoped
/// function value is COPIED to when its FunctionDeclaration is evaluated.
type AnnexBTarget {
  /// A var/param/capture binding in the enclosing function scope.
  AnnexBLocal(slot: Int, is_boxed: Bool)
  /// No enclosing var-scope binding: script / sloppy-direct-eval top level.
  /// The var twin was created via IrDeclareGlobalVar / IrDeclareEvalVar; the
  /// write goes through IrPutGlobal (ToGlobal) or IrPutEvalVar (ToEvalEnv).
  AnnexBFallthrough
  /// An intermediate let/const/fn-name binding shadows the name. The
  /// analyzer's `annexb_blocked` set already excludes these at the gate, so
  /// this arm is defensive only — emit nothing.
  AnnexBBlocked
}

/// Annex B §B.3.2.6 runtime step: when a block-level FunctionDeclaration is
/// evaluated in sloppy mode, copy the block-scoped function binding into the
/// enclosing VariableEnvironment binding of the same name. Walks the scope
/// tree from `current_scope`: the SOURCE is the innermost binding of `name`
/// (the LetBinding declared by emit_block_declarations at block entry); the
/// TARGET is the var-scope binding found by walking outward — §B.3.4 simple
/// catch parameters are stepped over, an intermediate let/const aborts, and
/// a function-scope var/param/capture binding (or fallthrough to global /
/// eval-env) receives the value.
///
/// Replaces the old `AnnexBPromote(name)` EmitterOp marker + the
/// IR-walking `annexb_promote_plan` resolution in scope.gleam — the scope
/// tree's analyzer-allocated slot/boxing data is read directly so the
/// emitter produces concrete IrGetLocal/IrGetBoxed → IrPutLocal/IrPutBoxed/
/// IrPutGlobal/IrPutEvalVar ops in place. Mirrors V8 DeclarationScope::
/// HoistSloppyBlockFunctions — the plan is decided on the AST scope tree,
/// the bytecode is the literal copy.
fn emit_annexb_promote(e: Emitter, name: String) -> Emitter {
  case annexb_find_source(e, e.current_scope, name) {
    // Tree not yet populated for this scope — emit nothing (the gate above
    // already established the promote is wanted; an empty tree means the
    // analyzer hasn't run, and Phase 2 has nothing to resolve either).
    None -> e
    Some(#(source, outside)) ->
      case annexb_find_target(e, outside, name) {
        AnnexBBlocked -> e
        AnnexBLocal(slot:, is_boxed:) ->
          e
          |> emit_slot_get(scope.SlotRef(source.slot, source.is_boxed))
          |> emit_slot_put(scope.SlotRef(slot, is_boxed))
        AnnexBFallthrough -> {
          let e = emit_slot_get(e, scope.SlotRef(source.slot, source.is_boxed))
          case fn_fallthrough(e) {
            ToGlobal -> emit_op(e, opcode.PutGlobal(name))
            ToEvalEnv -> emit_op(e, opcode.PutEvalVar(name))
          }
        }
      }
  }
}

/// Innermost binding of `name` in the scope tree starting at `from`, plus
/// the scope id immediately ENCLOSING the scope it was found in (i.e. the
/// first scope the target walk should inspect). Returns None when the name
/// is nowhere on the chain — an unpopulated/empty tree.
fn annexb_find_source(
  e: Emitter,
  from: ScopeId,
  name: String,
) -> Option(#(scope.Binding, Option(ScopeId))) {
  let node = scope.get_scope(e.scope_tree, from)
  case dict.get(node.bindings, name) {
    Ok(b) -> Some(#(b, scope_parent_in_fn(e, from)))
    Error(Nil) ->
      case scope_parent_in_fn(e, from) {
        Some(parent) -> annexb_find_source(e, parent, name)
        None -> None
      }
  }
}

/// Walk the tree outward from `from` (the source binding's parent scope) to
/// the var-scope target. Same algorithm as the legacy IR-walking
/// `annexb_target` (scope.gleam): catch params are stepped over (§B.3.4),
/// let/const/fn-name shadows abort, var/param/capture bindings receive the
/// value, and reaching the function root with no binding falls through to
/// global / eval-env.
fn annexb_find_target(
  e: Emitter,
  from: Option(ScopeId),
  name: String,
) -> AnnexBTarget {
  case from {
    // Reached (or started past) the function/script root — the var twin
    // lives in the global object / caller's eval var-env.
    None -> AnnexBFallthrough
    Some(id) -> {
      let node = scope.get_scope(e.scope_tree, id)
      case node.kind {
        // §B.3.4: a Catch scope's parameter binding (recorded by the
        // parser as a ParamBinding regardless of pattern shape — see
        // `annexb_check_chain`) is var-transparent: the var twin lives
        // OUTSIDE the catch, so the promote must not write into the
        // parameter. Only simple catch params reach here — a destructured
        // one already blocked the candidate at the analyzer gate.
        scope.Catch -> annexb_find_target(e, scope_parent_in_fn(e, id), name)
        _ ->
          case dict.get(node.bindings, name) {
            Ok(scope.Binding(kind: LetBinding, ..))
            | Ok(scope.Binding(kind: ConstBinding, ..))
            | Ok(scope.Binding(kind: FnNameBinding, ..)) -> AnnexBBlocked
            Ok(scope.Binding(kind: CatchBinding, ..)) ->
              annexb_find_target(e, scope_parent_in_fn(e, id), name)
            Ok(scope.Binding(slot:, is_boxed:, ..)) ->
              AnnexBLocal(slot:, is_boxed:)
            Error(Nil) -> annexb_find_target(e, scope_parent_in_fn(e, id), name)
          }
      }
    }
  }
}

/// Push the value held in the local slot `ref` names (boxed or unboxed).
fn emit_slot_get(e: Emitter, ref: scope.SlotRef) -> Emitter {
  case ref.boxed {
    True -> emit_op(e, opcode.GetBoxed(ref.slot))
    False -> emit_op(e, opcode.GetLocal(ref.slot))
  }
}

/// Store top-of-stack into the local slot `ref` names (boxed or unboxed).
/// Pops.
fn emit_slot_put(e: Emitter, ref: scope.SlotRef) -> Emitter {
  case ref.boxed {
    True -> emit_op(e, opcode.PutBoxed(ref.slot))
    False -> emit_op(e, opcode.PutLocal(ref.slot))
  }
}

/// Declare a top-level / hoisted `var` binding in the enclosing variable
/// environment, routed by this compilation unit's `fallthrough`:
/// IrDeclareGlobalVar (script / indirect-eval — global object) or
/// IrDeclareEvalVar (sloppy direct-eval — caller's eval_env). The legacy
/// Phase-2 resolver did this rewrite positionally; with that pass gone
/// every former `emit_ir(e, IrDeclareGlobalVar(name))` site routes through
/// here so sloppy direct eval `eval("var x")` / Annex-B var-twins land in
/// the caller's frame, not the global object.
fn emit_declare_var_global(e: Emitter, name: String) -> Emitter {
  case fn_fallthrough(e) {
    // `deletable_global_vars` is the §9.1.1.4.17 CreateGlobalVarBinding
    // D argument: False for scripts (§9.1.1.4.18 — `delete x` on a
    // top-level var / hoisted function is false and the binding
    // survives), True for eval units, direct and indirect (§19.2.1.3).
    ToGlobal ->
      emit_op(
        e,
        opcode.DeclareGlobalVar(name, deletable: e.deletable_global_vars),
      )
    ToEvalEnv -> emit_op(e, opcode.DeclareEvalVar(name))
  }
}

/// Declare a let/const binding in the global lexical record when the
/// current statement is at LexGlobal top level. For local-slot bindings
/// (LexLocal top level or any nested block), the analyzer pre-registered
/// the slot and enter_scope's emit_binding_prologue seeded it — nothing
/// to emit here.
fn declare_lex(e: Emitter, name: String, is_const: Bool) -> Emitter {
  case at_global_lex(e) {
    True -> emit_op(e, opcode.DeclareGlobalLex(name, is_const))
    False -> e
  }
}

/// Store the value on top of stack into a let/const binding declared via
/// declare_lex. Routes to IrInitGlobalLex (bypasses TDZ/const checks) or
/// emit_var_init (PutLocal/PutBoxed direct, bypassing the const-reassign /
/// TDZ check that emit_var_put applies). Mirrors QuickJS OP_scope_put_var_init.
fn init_lex(e: Emitter, name: String) -> Emitter {
  case at_global_lex(e) {
    True -> emit_op(e, opcode.InitGlobalLex(name))
    False -> emit_var_init(e, name)
  }
}

fn emit_ir(e: Emitter, op: IrOp) -> Emitter {
  Emitter(..e, code: [op, ..e.code])
}

/// Emit an opcode that is already in final form (no label / key / operator
/// left to resolve) — the overwhelmingly common case. `resolve` unwraps the
/// `IrFinal` and copies it straight through, so a new pass-through opcode
/// needs no `IrOp` variant and no `resolve` arm.
///
/// The type can't stop you handing this a PC-carrying op (`opcode.Jump`,
/// `opcode.PushTry`, …), but the emitter has no PCs — only label ids — so such
/// an op would ride through `resolve` unresolved and jump to a garbage PC. Emit
/// the matching `Ir*` variant for those; `resolve` panics if one slips through
/// as `IrFinal` (see `opcode.carries_pc`).
fn emit_op(e: Emitter, op: opcode.Op) -> Emitter {
  emit_ir(e, IrFinal(op))
}

// ============================================================================
// Variable-access helpers (scope resolution funnel)
//
// Every named-variable read/write/typeof/delete/rebox in the emitter goes
// through one of these helpers. They are the single point where
// `(current_scope, name)` is resolved against `e.scope_tree` (via
// `scope.lookup`) to a concrete op — IrGetLocal/IrGetBoxed (local slot),
// IrGetGlobal/IrGetEvalVar (fallthrough), IrThrowConstAssign (const write),
// or an IrWith* probe chain when the lookup crosses with-scope markers.
// There is no symbolic placeholder and no Phase-2 rewrite pass; the IrScope*
// op family is gone.
//
// `references_arguments` tracking lives here too (moved out of emit_ir): a
// read/write/typeof/delete/ref of the identifier `arguments` marks this body
// so compile_function_body knows to materialise the arguments object.
// emit_var_init / emit_var_rebox are NOT references to the enclosing binding
// and so do not set the flag.
// ============================================================================

/// Mark this body as referencing `arguments` if `name` is "arguments".
/// Mirrors how `lexical_refs` is tracked in get_lexical.
fn track_arguments_ref(e: Emitter, name: String) -> Emitter {
  case name {
    "arguments" -> Emitter(..e, references_arguments: True)
    _ -> e
  }
}

/// Read `name` and push its value. Resolves to GetLocal/GetBoxed (local
/// slot), GetGlobal/GetEvalVar (fallthrough), or an IrWithGetVar chain when
/// the resolution crosses with-scope markers.
fn emit_var_get(e: Emitter, name: String) -> Emitter {
  let e = track_arguments_ref(e, name)
  let #(crossed, fallback) = split_with_chain(resolve(e, name))
  use e <- emit_with_chain(e, crossed, opcode.IrWithGetVar(name, _))
  emit_static_get(e, fallback)
}

/// Read `name` as a callee — push [value, this_receiver]. §13.3.6.2
/// EvaluateCall step 1.b.ii: if resolved through a with object, thisValue is
/// that object (WithBaseObject); otherwise undefined.
fn emit_var_get_this(e: Emitter, name: String) -> Emitter {
  let e = track_arguments_ref(e, name)
  let #(crossed, fallback) = split_with_chain(resolve(e, name))
  use e <- emit_with_chain(e, crossed, opcode.IrWithGetVarThis(name, _))
  let e = push_const(e, JsUndefined)
  emit_static_get(e, fallback)
}

/// Store top-of-stack into `name`. Resolves to PutLocal/PutBoxed (with a
/// TDZ-check read for let/capture not yet initialised — §9.1.1.1.5
/// SetMutableBinding step 5), IrThrowConstAssign for const / strict NFE
/// self-name, IrPop for sloppy NFE self-name, PutGlobal/PutEvalVar
/// (fallthrough), or an IrWithPutVar chain.
fn emit_var_put(e: Emitter, name: String) -> Emitter {
  let e = track_arguments_ref(e, name)
  let #(crossed, fallback) = split_with_chain(resolve(e, name))
  use e <- emit_with_chain(e, crossed, opcode.IrWithPutVar(name, _))
  emit_static_put(e, fallback, name)
}

/// One-time init store that bypasses the const-reassign / TDZ checks in
/// emit_var_put. Resolves to PutLocal/PutBoxed (or PutGlobal/PutEvalVar on
/// fallthrough). Mirrors QuickJS OP_scope_put_var_init. NOT an `arguments`
/// reference — declaration init does not count as a body reference.
fn emit_var_init(e: Emitter, name: String) -> Emitter {
  let #(_crossed, fallback) = split_with_chain(resolve(e, name))
  case fallback {
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

/// `typeof name`. Resolves to GetLocal/GetBoxed + IrTypeOf (local), or
/// IrTypeofGlobal/IrTypeofEvalVar (fallthrough — never throws ReferenceError
/// per §13.5.3), or an IrWithGetVar chain + IrTypeOf.
fn emit_var_typeof(e: Emitter, name: String) -> Emitter {
  let e = track_arguments_ref(e, name)
  let #(crossed, fallback) = split_with_chain(resolve(e, name))
  let static = fn(e: Emitter) {
    case fallback {
      scope.Local(slot:, boxed:, ..) ->
        emit_slot_get(e, scope.SlotRef(slot:, boxed:)) |> emit_op(opcode.TypeOf)
      scope.Global(name:) -> emit_op(e, opcode.TypeofGlobal(name))
      scope.EvalEnv(name:) -> emit_op(e, opcode.TypeofEvalVar(name))
    }
  }
  case crossed {
    [] -> static(e)
    _ -> {
      // A with object that has the property supplies the VALUE (jump to
      // `hit`, then IrTypeOf); otherwise the static fallback computes
      // typeof directly (TypeofGlobal etc. never throw on undeclared).
      let #(e, hit) = fresh_label(e)
      let #(e, end) = fresh_label(e)
      let e =
        list.fold(crossed, e, fn(e, w) {
          e
          |> emit_slot_get(w)
          |> emit_ir(opcode.IrWithGetVar(name, hit))
        })
      let e = static(e)
      let e = emit_ir(e, IrJump(end))
      let e = emit_ir(e, IrLabel(hit))
      let e = emit_op(e, opcode.TypeOf)
      emit_ir(e, IrLabel(end))
    }
  }
}

/// §13.5.1.2 `delete name` (sloppy mode — strict `delete identifier` is an
/// early SyntaxError). If an enclosing with object has the property,
/// IrWithDeleteVar performs the [[Delete]] and pushes the result; otherwise
/// the static resolution decides:
///
/// - `scope.Local` — a binding created by a declaration (var/let/const/
///   param/catch/function). §9.1.1.1.7 DeleteBinding on a declarative
///   Environment Record whose binding was not created deletable returns
///   false and removes nothing, so this is a compile-time constant.
/// - `scope.Global` — §9.1.1.4.7: a real [[Delete]] against the realm's
///   global object at runtime (IrDeleteGlobalVar), so configurable implicit
///   globals (`x = 1`) really disappear and non-configurable ones report
///   false.
/// - `scope.EvalEnv` — GAP: there is no eval-env DeleteBinding opcode yet.
///   Sloppy direct-eval `var` bindings ARE deletable (§19.2.1.3
///   EvalDeclarationInstantiation creates them with D = true), so `true` is
///   the right VALUE, but the binding is not actually removed from
///   state.eval_env. Kept as the historical push-true rather than
///   regressing it.
fn emit_var_delete(e: Emitter, name: String) -> Emitter {
  let e = track_arguments_ref(e, name)
  let #(crossed, fallback) = split_with_chain(resolve(e, name))
  use e <- emit_with_chain(e, crossed, opcode.IrWithDeleteVar(name, _))
  case fallback {
    scope.Local(..) -> push_const(e, JsBool(False))
    scope.Global(name:) -> emit_op(e, opcode.DeleteGlobalVar(name))
    scope.EvalEnv(name: _) -> push_const(e, JsBool(True))
  }
}

/// A resolved reference to an identifier assignment target — the compile-time
/// residue of §13.15.2 step 1a ResolveBinding. `emit_var_ref_make` is its only
/// constructor, and `emit_var_ref_get` / `emit_var_ref_put` only accept one,
/// so a get/put can never re-resolve the binding *after* the RHS ran (which
/// would read/write the wrong `with` scope).
///
/// `base_slot` is `Some(slot)` exactly when the resolution crossed a `with`
/// scope: the scratch local holds the matched with object (or undefined =
/// take the static fallback), captured before the RHS could mutate it.
pub opaque type VarRef {
  VarRef(name: String, fallback: scope.Direct, base_slot: Option(Int))
}

/// §13.15.2 step 1a — ResolveBinding for an assignment-like target before
/// the RHS runs. When the resolution crosses with-scope markers, stash the
/// matched with object (or undefined = static) in a scratch local so the
/// paired emit_var_ref_get / emit_var_ref_put hit the ORIGINAL reference
/// base even if the RHS mutates the with object. Emits nothing for non-with
/// resolutions.
fn emit_var_ref_make(e: Emitter, name: String) -> #(Emitter, VarRef) {
  let e = track_arguments_ref(e, name)
  let #(crossed, fallback) = split_with_chain(resolve(e, name))
  case crossed {
    [] -> #(e, VarRef(name:, fallback:, base_slot: None))
    _ -> {
      let #(e, slot) = acquire_ref_slot(e)
      let #(e, lref) = fresh_label(e)
      let e =
        list.fold(crossed, e, fn(e, w) {
          e
          |> emit_slot_get(w)
          |> emit_ir(opcode.IrWithMakeRef(name, lref))
        })
      // No with object had the property: store undefined as the base so
      // GetRef/PutRef know to take the static fallback.
      let e = push_const(e, JsUndefined)
      let e = emit_ir(e, IrLabel(lref))
      #(
        emit_op(e, opcode.PutLocal(slot)),
        VarRef(name:, fallback:, base_slot: Some(slot)),
      )
    }
  }
}

/// GetValue on the reference opened by emit_var_ref_make (§9.1.1.2.6).
fn emit_var_ref_get(e: Emitter, ref: VarRef) -> Emitter {
  case ref.base_slot {
    None -> emit_static_get(e, ref.fallback)
    Some(slot) -> {
      let #(e, lg) = fresh_label(e)
      let e = emit_op(e, opcode.GetLocal(slot))
      let e = emit_ir(e, opcode.IrWithGetRefValue(ref.name, lg))
      // Base was undefined (no with object matched at MakeRef time):
      // resolve statically.
      let e = emit_static_get(e, ref.fallback)
      emit_ir(e, IrLabel(lg))
    }
  }
}

/// PutValue on the reference opened by emit_var_ref_make (§9.1.1.2.5);
/// closes it (frees the scratch base slot).
fn emit_var_ref_put(e: Emitter, ref: VarRef) -> Emitter {
  case ref.base_slot {
    None -> emit_static_put(e, ref.fallback, ref.name)
    Some(slot) -> {
      let e = Emitter(..e, ref_free: [slot, ..e.ref_free])
      let #(e, ld) = fresh_label(e)
      let e = emit_op(e, opcode.GetLocal(slot))
      let e = emit_ir(e, opcode.IrWithPutRefValue(ref.name, ld))
      let e = emit_static_put(e, ref.fallback, ref.name)
      emit_ir(e, IrLabel(ld))
    }
  }
}

/// MakeRef → `body` (leaves the new value on top) → Dup → PutValue. Shared
/// shape of identifier-target assignment (§13.15.2 step 1.a): the reference
/// is resolved once before the body runs (observable via `with` Proxy traps
/// and binding deletion during RHS), Dup keeps the value as the expression
/// result, PutValue stores it. The body receives the resolved `VarRef` so a
/// read of the target (compound assignment, `x++`, `&&=`) goes through the
/// SAME reference rather than re-resolving.
fn with_identifier_lref(
  e: Emitter,
  name: String,
  body: fn(Emitter, VarRef) -> Result(Emitter, EmitError),
) -> Result(Emitter, EmitError) {
  let #(e, ref) = emit_var_ref_make(e, name)
  use e <- result.map(body(e, ref))
  e |> emit_op(opcode.Dup) |> emit_var_ref_put(ref)
}

/// §14.7.4.2 CreatePerIterationEnvironment: copy a for-let binding's current
/// value into a fresh box so closures captured in this iteration see this
/// iteration's value. Resolves to GetBoxed+PutLocal+BoxLocal when the binding
/// is boxed; otherwise no-op (uncaptured per-iter copies are unobservable).
/// NOT an `arguments` reference.
fn emit_var_rebox(e: Emitter, name: String) -> Emitter {
  let #(_crossed, fallback) = split_with_chain(resolve(e, name))
  case fallback {
    scope.Local(slot:, boxed: True, ..) ->
      e
      |> emit_op(opcode.GetBoxed(slot))
      |> emit_op(opcode.PutLocal(slot))
      |> emit_op(opcode.BoxLocal(slot))
    _ -> e
  }
}

// ---- scope-tree lookup + with-chain helpers --------------------------------
// The emitter consults the analyzer's `scope.ScopeTree` directly so concrete
// slot ops and IrWith* probe ops are emitted inline at the variable-access
// site — replacing the old IR-rewriting Phase-2 resolver. Mirrors V8's
// BytecodeGenerator carrying a `Scope* current_scope_` and resolving each
// VariableProxy to a VariableLocation at emission time.

/// Resolve `name` from `e.current_scope` against the analyzer's scope tree.
/// `scope.lookup` stops at the enclosing function-kind scope (so a free
/// name never wrongly reads the parent frame's slot index) and returns the
/// with-chain crossed slots inline as a `WithChain` wrapper — there is no
/// separate `crossed` accumulator.
fn resolve(e: Emitter, name: String) -> scope.Resolution {
  scope.lookup(e.scope_tree, e.current_scope, name)
}

/// Split a `scope.Resolution` into the with-object probe slots crossed
/// (innermost first) and the non-with `scope.Direct` fallback. Lets the
/// emit_var_* helpers reuse `emit_with_chain` unchanged.
fn split_with_chain(
  res: scope.Resolution,
) -> #(List(scope.SlotRef), scope.Direct) {
  case res {
    scope.WithChain(crossed_slots:, fallback:) -> #(crossed_slots, fallback)
    scope.Plain(direct) -> #([], direct)
  }
}

/// The non-with ("static") read of a resolved binding: local/boxed slot, or
/// global/eval-env fallthrough.
fn emit_static_get(e: Emitter, res: scope.Direct) -> Emitter {
  case res {
    scope.Local(slot:, boxed:, ..) ->
      emit_slot_get(e, scope.SlotRef(slot:, boxed:))
    scope.Global(name:) -> emit_op(e, opcode.GetGlobal(name))
    scope.EvalEnv(name:) -> emit_op(e, opcode.GetEvalVar(name))
  }
}

/// The non-with ("static") store to a resolved binding. §9.1.1.1.5
/// SetMutableBinding step 6 — const bindings are always strict (§14.3.1.3),
/// so reassignment unconditionally throws TypeError. RHS is already on the
/// stack; throw discards it via unwind.
fn emit_static_put(e: Emitter, res: scope.Direct, name: String) -> Emitter {
  case res {
    // Immutable-origin writes (§9.1.1.1.5 SetMutableBinding). For every
    // non-capture binding `origin_kind == kind` (scope.add_binding), so
    // matching on `origin_kind` covers BOTH the direct binding and any
    // closure capture of it: a `const` → step 6 TypeError unconditionally;
    // a named-function-expression self-name is strict=false-immutable →
    // step 4 throws only when the WRITE site is strict, else silently drops
    // the value to keep the stack balanced.
    scope.Local(origin_kind: ConstBinding, ..) ->
      emit_op(e, opcode.ThrowConstAssign(name))
    scope.Local(origin_kind: FnNameBinding, ..) ->
      case e.strict {
        True -> emit_op(e, opcode.ThrowConstAssign(name))
        False -> emit_op(e, opcode.Pop)
      }
    // Any other capture (let/var/param/catch origin): the origin may be a
    // let still in TDZ when this closure runs (`(function(){x=1})(); let x`)
    // — §9.1.1.1.5 step 5: store to an uninitialized binding is a
    // ReferenceError. Every mutable-capture store is TDZ-checked (a guard
    // read that throws on JsUninitialized before the put). Var/param origin
    // cells are never uninitialized, so for them this is only a wasted read.
    scope.Local(kind: CaptureBinding, slot:, boxed:, ..) ->
      emit_checked_put(e, scope.SlotRef(slot:, boxed:))
    // A let binding whose initialization has NOT been emitted yet
    // (linearly) — the store may run during TDZ (`{ x = 1; let x; }`),
    // so check first.
    scope.Local(kind: LetBinding, slot:, boxed:, ..) ->
      case set.contains(e.initialized, slot) {
        True -> emit_slot_put(e, scope.SlotRef(slot:, boxed:))
        False -> emit_checked_put(e, scope.SlotRef(slot:, boxed:))
      }
    scope.Local(slot:, boxed:, ..) ->
      emit_slot_put(e, scope.SlotRef(slot:, boxed:))
    scope.Global(name:) -> emit_op(e, opcode.PutGlobal(name))
    scope.EvalEnv(name:) -> emit_op(e, opcode.PutEvalVar(name))
  }
}

/// A store that must respect TDZ (§9.1.1.1.5 SetMutableBinding step 5):
/// re-use the TDZ-checking read ops (GetLocal/GetBoxed throw ReferenceError
/// on JsUninitialized), drop the read value, then store.
fn emit_checked_put(e: Emitter, ref: scope.SlotRef) -> Emitter {
  e
  |> emit_slot_get(ref)
  |> emit_op(opcode.Pop)
  |> emit_slot_put(ref)
}

/// §14.11: emit the runtime check chain for `crossed` with-object slots
/// (innermost first). Each pushes its with object then `with_op(done)` —
/// the first whose object has the (unscopables-visible) property handles
/// the access and jumps to `done`; otherwise `fallback` runs. When
/// `crossed` is empty (no with scopes between the reference and its
/// binding), just runs `fallback`.
fn emit_with_chain(
  e: Emitter,
  crossed: List(scope.SlotRef),
  with_op: fn(Int) -> IrOp,
  fallback: fn(Emitter) -> Emitter,
) -> Emitter {
  case crossed {
    [] -> fallback(e)
    _ -> {
      let #(e, done) = fresh_label(e)
      let e =
        list.fold(crossed, e, fn(e, w) {
          e |> emit_slot_get(w) |> emit_ir(with_op(done))
        })
      let e = fallback(e)
      emit_ir(e, IrLabel(done))
    }
  }
}

/// Acquire a scratch local for a with-reference base — reuse a freed one
/// (LIFO) or allocate a fresh anonymous slot via `fresh_slot`. The slot
/// travels in the returned `VarRef`; emit_var_ref_put returns it to
/// `ref_free`.
fn acquire_ref_slot(e: Emitter) -> #(Emitter, Int) {
  case e.ref_free {
    [slot, ..rest] -> #(Emitter(..e, ref_free: rest), slot)
    [] -> fresh_slot(e)
  }
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
  emit_op(e, opcode.PushConst(idx))
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
      |> emit_var_get(name)
      |> emit_op(opcode.GetPrivateFieldDyn)
    _ -> emit_ir(e, IrGetField(name))
  }
}

/// Stack: [obj, ..] → [val, obj, ..]
fn emit_get_field2(e: Emitter, name: String) -> Emitter {
  case name {
    "#" <> _ ->
      e
      |> emit_var_get(name)
      |> emit_op(opcode.GetPrivateFieldDyn2)
    _ -> emit_ir(e, IrGetField2(name))
  }
}

/// Stack: [val, obj, ..] → [val, ..]
fn emit_put_field(e: Emitter, name: String) -> Emitter {
  case name {
    "#" <> _ ->
      e
      |> emit_var_get(name)
      |> emit_op(opcode.PutPrivateFieldDyn)
    _ -> emit_ir(e, IrPutField(name))
  }
}

fn fresh_label(e: Emitter) -> #(Emitter, Int) {
  let label = e.next_label
  #(Emitter(..e, next_label: label + 1), label)
}

/// Mint a fresh emitter-internal scratch local slot. The slot comes from
/// `alloc_scratch` on the scope tree's root function scope — past the
/// analyzer-allocated named bindings — so it can never collide with user
/// code or with another scratch slot. Scratch slots are anonymous (no
/// name in the scope tree) and never captured by a child closure, so the
/// emitter addresses them directly via IrGetLocal/IrPutLocal without going
/// through name resolution.
fn fresh_slot(e: Emitter) -> #(Emitter, Int) {
  let #(tree, slot) = scope.alloc_scratch(e.scope_tree, e.fn_scope)
  #(Emitter(..e, scope_tree: tree), slot)
}

/// Prepend a target frame to the frame stack and consume any pending label.
fn push_frame(e: Emitter, frame: Frame) -> Emitter {
  Emitter(..e, frame_stack: [frame, ..e.frame_stack], pending_label: None)
}

fn push_loop(e: Emitter, break_target: Int, continue_target: Int) -> Emitter {
  push_frame(
    e,
    LoopFrame(
      break_target:,
      continue_target:,
      label: e.pending_label,
      iterator: False,
    ),
  )
}

/// for-of loop: body runs under one try frame (F_body) with iter on stack.
/// NB: must be called AFTER the F_body PushTry so the crossing PopTry lines up.
fn push_loop_iter(
  e: Emitter,
  break_target: Int,
  continue_target: Int,
) -> Emitter {
  push_frame(
    e,
    LoopFrame(
      break_target:,
      continue_target:,
      label: e.pending_label,
      iterator: True,
    ),
  )
}

/// switch: a break target only. `continue` walks past it to the outer loop.
fn push_switch(e: Emitter, break_target: Int) -> Emitter {
  push_frame(e, SwitchFrame(break_target:, label: e.pending_label))
}

/// Barrier frame for try/catch/finally bodies — never a target, only crossed.
/// See BarrierFrame for what each field emits when crossed. QuickJS:
/// push_break_entry(..., -1, -1, drop) at quickjs.c:28934-28935.
///
/// NB: NOT routed through push_frame — barriers are transparent to label
/// flow. pending_label must survive a barrier so e.g. emit_for_using_classic
/// can let the inner push_loop (inside emit_using_try_wrap's barrier) consume
/// the LabeledStatement's label.
fn push_barrier(
  e: Emitter,
  pop_try pop_try: Int,
  label_finally label_finally: Option(Int),
  drop drop: Int,
) -> Emitter {
  Emitter(..e, frame_stack: [
    BarrierFrame(pop_try:, label_finally:, drop_count: drop),
    ..e.frame_stack
  ])
}

/// Pop the innermost `frame_stack` entry. Every pop is paired with a
/// push_loop/push_frame/push_barrier in the same emit function, so an
/// empty stack here is a push/pop desync — crash at the desync instead of
/// silently leaving the (now stale) outer context in place.
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

/// Emit Swap;Pop n times — discards n slots from *under* top-of-stack.
/// Used by ReturnStatement to nip saved [retpc, slot] when return crosses a
/// finally-body barrier (Arc has no IrNip / OP_nip_catch).
fn repeat_nip(e: Emitter, n: Int) -> Emitter {
  case n <= 0 {
    True -> e
    False -> repeat_nip(e |> emit_op(opcode.Swap) |> emit_op(opcode.Pop), n - 1)
  }
}

/// Normal-completion entry to a finally subroutine: dummy slot + Gosub + drop.
/// Stack-neutral round trip. QuickJS: `OP_undefined; OP_gosub L; OP_drop`
/// (quickjs.c:28839-28841, 28903-28905).
fn emit_gosub_normal(e: Emitter, fin_label: Int) -> Emitter {
  e
  |> push_const(JsUndefined)
  |> emit_ir(IrGosub(fin_label))
  |> emit_op(opcode.Pop)
}

/// Throw-entry + finally-subroutine, shared by try/finally and try/catch/finally.
/// Subroutine entry stack: [retpc, slot, ..base]. §14.15.3: a normally-completing
/// Finally never supplies the completion value — runs out of completion-value mode.
fn emit_finally_subroutine(
  e: Emitter,
  throw_label: Int,
  fin_label: Int,
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

/// Shared scaffold for try { body } catch { … } finally { … } with both
/// handlers present. Two stacked PushTry handlers (outer→throw, inner→catch);
/// body runs under both, catch under the outer only; abrupt completions in
/// either route through the finally subroutine via Gosub. emit_catch is
/// passed (e, catch_label, fin_label) and must emit IrLabel(catch_label)
/// itself so callers may route through emit_catch_clause's scope handling.
fn emit_try_catch_finally(
  e: Emitter,
  emit_body: fn(Emitter) -> Result(Emitter, EmitError),
  emit_finally: fn(Emitter) -> Result(Emitter, EmitError),
  emit_catch: fn(Emitter, Int, Int) -> Result(Emitter, EmitError),
) -> Result(Emitter, EmitError) {
  let #(e, throw_label) = fresh_label(e)
  let #(e, catch_label) = fresh_label(e)
  let #(e, fin_label) = fresh_label(e)
  let #(e, end_label) = fresh_label(e)

  // -- try body ----------------------------------------------------------
  let e = emit_ir(e, IrPushTry(throw_label, Finally(LabelId(fin_label))))
  let e = emit_ir(e, IrPushTry(catch_label, CatchOnly))
  let e = push_barrier(e, pop_try: 2, label_finally: Some(fin_label), drop: 0)
  use e <- result.try(emit_body(e))
  let e = pop_frame(e)
  let e = emit_op(e, opcode.PopTry)
  let e = emit_op(e, opcode.PopTry)
  let e = emit_gosub_normal(e, fin_label)
  let e = emit_ir(e, IrJump(end_label))

  // -- catch handler -----------------------------------------------------
  // unwind popped inner try; outer (throw_label) still on try_stack.
  // stack = [thrown, ..base].
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

/// The label a `break`/`continue` named `name` jumps to when `frame` is its
/// target, or None when the frame must instead be *crossed*. Encodes §14.8:
/// an unlabeled break skips labeled non-loop blocks; a continue only ever
/// targets a loop, so switch/labeled-block/barrier frames can never answer.
fn frame_target(
  frame: Frame,
  name: Option(String),
  is_cont: Bool,
) -> Option(Int) {
  case frame {
    LoopFrame(break_target:, continue_target:, label:, ..) -> {
      let target = case is_cont {
        True -> continue_target
        False -> break_target
      }
      case name {
        None -> Some(target)
        Some(_) ->
          case label == name {
            True -> Some(target)
            False -> None
          }
      }
    }
    SwitchFrame(break_target:, label:) ->
      case is_cont {
        True -> None
        False ->
          case name {
            None -> Some(break_target)
            Some(_) ->
              case label == name {
                True -> Some(break_target)
                False -> None
              }
          }
      }
    LabeledBlockFrame(break_target:, label:) ->
      case is_cont, name {
        False, Some(n) if n == label -> Some(break_target)
        _, _ -> None
      }
    BarrierFrame(..) -> None
  }
}

/// Cleanup emitted when a break/continue *crosses* (does not target) a frame:
/// balance try_stack, discard saved gosub slots, close the iterator, then run
/// any pending finally as a subroutine. QuickJS emit_break (quickjs.c:27794).
fn emit_cross_frame(e: Emitter, frame: Frame) -> Emitter {
  case frame {
    LoopFrame(iterator: True, ..) ->
      e |> emit_op(opcode.PopTry) |> emit_op(opcode.IteratorClose)
    LoopFrame(..) | SwitchFrame(..) | LabeledBlockFrame(..) -> e
    BarrierFrame(pop_try:, label_finally:, drop_count:) -> {
      let e = repeat_ir(e, IrFinal(opcode.PopTry), pop_try)
      let e = repeat_ir(e, IrFinal(opcode.Pop), drop_count)
      case label_finally {
        Some(lbl) -> emit_gosub_normal(e, lbl)
        None -> e
      }
    }
  }
}

/// Cleanup emitted when a `return` crosses a frame — a return crosses *every*
/// frame on the way out. Same shape as emit_cross_frame, but the return value
/// sits on top of the value stack throughout: IrPopTry doesn't touch it,
/// drop_count slots sit *under* it → nip, the iterator sits under it → swap
/// before close, and the retval IS the gosub slot so no push/pop around Gosub
/// (QuickJS emit_return, quickjs.c:27876).
fn emit_return_cross_frame(e: Emitter, frame: Frame) -> Emitter {
  case frame {
    LoopFrame(iterator: True, ..) ->
      e
      |> emit_op(opcode.PopTry)
      |> emit_op(opcode.Swap)
      |> emit_op(opcode.IteratorClose)
    LoopFrame(..) | SwitchFrame(..) | LabeledBlockFrame(..) -> e
    BarrierFrame(pop_try:, label_finally:, drop_count:) -> {
      let e = repeat_ir(e, IrFinal(opcode.PopTry), pop_try)
      let e = repeat_nip(e, drop_count)
      case label_finally {
        Some(lbl) -> emit_ir(e, IrGosub(lbl))
        None -> e
      }
    }
  }
}

/// Shared body of break/continue. Walks frame_stack emitting PopTry and
/// IteratorClose for each frame *crossed* (not targeted), then jumps to the
/// target's break/continue label. Mirrors QuickJS emit_break (quickjs.c:27770).
fn emit_goto_loop(
  e: Emitter,
  name: Option(String),
  is_cont: Bool,
) -> Result(Emitter, EmitError) {
  emit_goto_loop_walk(e, e.frame_stack, name, is_cont)
}

fn emit_goto_loop_walk(
  e: Emitter,
  stack: List(Frame),
  name: Option(String),
  is_cont: Bool,
) -> Result(Emitter, EmitError) {
  case stack {
    [] ->
      case is_cont {
        True -> Error(ContinueOutsideLoop)
        False -> Error(BreakOutsideLoop)
      }
    [frame, ..rest] ->
      case frame_target(frame, name, is_cont) {
        Some(target) -> Ok(emit_ir(e, IrJump(target)))
        None ->
          emit_goto_loop_walk(emit_cross_frame(e, frame), rest, name, is_cont)
      }
  }
}

fn add_child_function(e: Emitter, child: CompiledChild) -> #(Emitter, Int) {
  let idx = e.next_func
  // Arrow children inherit lexical bindings, so their references are the
  // parent's references. Non-arrows own their slots — flags don't propagate.
  let #(lexical_refs, references_arguments) = case child.is_arrow {
    True -> #(
      opcode.lexical_refs_or(e.lexical_refs, child.lexical_refs),
      e.references_arguments || child.references_arguments,
    )
    False -> #(e.lexical_refs, e.references_arguments)
  }
  #(
    Emitter(
      ..e,
      // Prepended for O(1); reversed once in finish().
      functions: [child, ..e.functions],
      next_func: idx + 1,
      lexical_refs:,
      references_arguments:,
    ),
    idx,
  )
}

/// Resolve a lexical pseudo-binding (this / active_func / home_object /
/// new.target) for THIS body to its `Some(#(slot, is_boxed))`. Mirrors
/// `scope.lookup_lexical`: an OWNED slot (`FunctionInfo.lexical`) is boxed
/// only when an inner arrow / direct-eval captures it
/// (`FunctionInfo.lexical_boxed`); an INHERITED slot (arrow / direct-eval
/// body — `FunctionInfo.lexical_captures`) is always a parent box.
///
/// Returns `None` when neither exists. This is the legitimate state for a
/// Script/Module root body — the analyzer assigns it `NoLexicalSlots`
/// and (absent direct-eval) an empty `lexical_captures`, so a top-level
/// `this` / `new.target` has no backing slot. §16.1.6 / §11.2.4: module
/// `this` reads `undefined`; the old IR-level resolver lowered this case
/// to `IrPushConst(JsUndefined)` and that behavior must be preserved.
fn resolve_lexical(e: Emitter, ref: opcode.LexicalRef) -> Option(#(Int, Bool)) {
  let info = fn_info(e)
  case opcode.lexical_slot(info.lexical, ref) {
    Some(slot) ->
      Some(#(slot, opcode.lexical_refs_get(info.lexical_boxed, ref)))
    None ->
      case dict.get(info.lexical_captures, ref) {
        Ok(slot) -> Some(#(slot, True))
        Error(Nil) -> None
      }
  }
}

/// Read the lexical pseudo-binding `ref` and push its value. Mark this
/// body as referencing that binding (so add_child_function propagates the
/// flag up through arrows). Lowers to IrGetLocal / IrGetBoxed against the
/// analyzer-assigned slot.
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
  let e = Emitter(..e, lexical_refs:)
  case resolve_lexical(e, ref) {
    Some(#(slot, True)) -> emit_op(e, opcode.GetBoxed(slot))
    Some(#(slot, False)) -> emit_op(e, opcode.GetLocal(slot))
    // Script/Module root with no lexical slot — bit-for-bit match for the
    // old Phase-2 IrGetLexical Error(Nil) arm: push `undefined`.
    None -> push_const(e, JsUndefined)
  }
}

/// Read the lexical `this`.
fn get_this(e: Emitter) -> Emitter {
  get_lexical(e, opcode.RefThis)
}

/// Store top-of-stack into the lexical `this` slot (derived-class
/// `super(...)` writes the new instance). Mark this body as referencing
/// lexical `this`. Lowers to IrPutLocalCheckInit / IrPutBoxedCheckInit
/// (§10.2.4 BindThisValue: writing an already-initialized `this` is a
/// ReferenceError).
fn set_this(e: Emitter) -> Emitter {
  let e =
    Emitter(..e, lexical_refs: opcode.LexicalRefs(..e.lexical_refs, this: True))
  case resolve_lexical(e, opcode.RefThis) {
    Some(#(slot, True)) -> emit_op(e, opcode.PutBoxedCheckInit(slot))
    Some(#(slot, False)) -> emit_op(e, opcode.PutLocalCheckInit(slot))
    // Defensive only — set_this is reached solely from derived-ctor
    // `super(...)`, which always owns the slot. Matches the old Phase-2
    // IrSetThis Error(Nil) arm: no-op.
    None -> e
  }
}

/// Common prefix for every `super.prop` / `super[k]` form. Stack after:
/// [home_proto, this, ..]. §9.1.1.3.5 GetSuperBase = lexical home_object's
/// [[Prototype]]. Marks both lexical refs.
fn emit_super_base(e: Emitter) -> Emitter {
  e
  |> get_this
  |> get_lexical(opcode.RefHomeObject)
  |> emit_op(opcode.GetPrototypeOf)
}

/// As emit_super_base but Dup's the receiver so the stack after a following
/// GetSuperValue is [val, this, ..] — the [fn, recv] shape CallMethod wants.
fn emit_super_base_keep_recv(e: Emitter) -> Emitter {
  e
  |> get_this
  |> emit_op(opcode.Dup)
  |> get_lexical(opcode.RefHomeObject)
  |> emit_op(opcode.GetPrototypeOf)
}

/// Emit a `super.m` / `super[k]` callee reference (§13.3.7.3): read the super
/// property keeping the receiver underneath, leaving the stack as [fn, this]
/// — the CallMethod shape.
fn emit_super_method_ref(
  e: Emitter,
  key: ast.Expression,
  computed: Bool,
) -> Result(Emitter, EmitError) {
  let e = emit_super_base_keep_recv(e)
  use e <- result.map(emit_super_key(e, key, computed))
  emit_op(e, opcode.GetSuperValue)
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
    False, ast.Identifier(name:, ..) -> Ok(push_const(e, JsString(name)))
    _, _ -> emit_expr(e, key)
  }
}

/// Member lvalue shape for the read-modify-write helper pair below. Records
/// which "put" op the matching `emit_lvalue_get2` left the stack primed for.
type LvalueShape {
  LvSuper
  LvField(prop: String)
  LvElem
}

/// QuickJS-style RMW helper: emit the "*2" read of a member lvalue (reads the
/// current value while keeping the write-back operands underneath). Stack
/// after: [old, …put-args]. Pair with `emit_lvalue_put` after computing the
/// new value. Used by `++`/`--` and compound `op=` so each evaluates the
/// base/key/super-base exactly once (§13.15.2 / §13.4).
fn emit_lvalue_get2(
  e: Emitter,
  lhs: ast.Expression,
) -> Result(#(Emitter, LvalueShape), EmitError) {
  case lhs {
    ast.MemberExpression(_, ast.SuperExpression(_), key, computed) -> {
      let e = emit_super_base(e)
      use e <- result.map(emit_super_key(e, key, computed))
      #(emit_op(e, opcode.GetSuperValue2), LvSuper)
    }
    ast.MemberExpression(_, obj, ast.Identifier(name: prop, ..), False) -> {
      use e <- result.map(emit_expr(e, obj))
      #(emit_get_field2(e, prop), LvField(prop))
    }
    ast.MemberExpression(_, obj, key, True) -> {
      use e <- result.try(emit_expr(e, obj))
      use e <- result.map(emit_expr(e, key))
      #(emit_op(e, opcode.GetElem2), LvElem)
    }
    // Only MemberExpressions reach here (update-expression / compound-
    // assignment callers match on `ast.MemberExpression(..) as member`), and
    // every member shape is covered above.
    _ -> Error(NonMemberLValue)
  }
}

/// Companion to `emit_lvalue_get2`. Stack: [new, …put-args] → [new].
fn emit_lvalue_put(e: Emitter, shape: LvalueShape) -> Emitter {
  case shape {
    LvSuper -> emit_op(e, opcode.PutSuperValue)
    LvField(prop) -> emit_put_field(e, prop)
    LvElem -> emit_op(e, opcode.PutElem)
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
  |> emit_var_get(class_fields_init)
  |> emit_op(opcode.Dup)
  |> emit_ir(IrJumpIfFalse(skip))
  |> get_this
  |> emit_op(opcode.Swap)
  |> emit_op(opcode.CallMethod(0))
  |> emit_ir(IrLabel(skip))
  |> emit_op(opcode.Pop)
}

/// Extract final results from the emitter. `code` is already a plain
/// `List(IrOp)` — the parser-built scope tree (`scope.finalize`) resolved
/// every binding up front, so there are no scope markers to strip; just
/// reverse the accumulated stream into source order.
fn finish(e: Emitter) -> #(List(IrOp), List(JsValue), List(CompiledChild)) {
  #(
    list.reverse(e.code),
    list.reverse(e.constants_list),
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

    ast.IfStatement(cond, cons, alt) ->
      emit_if(e, cond, cons, alt, emit_stmt_tail, push_const(_, JsUndefined))

    // try with a finally: delegate to the full statement emitter (which
    // runs the finalizer via Gosub), tracking the completion value in a
    // synthetic binding (the finalizer body is excluded — §14.15.3: a
    // normally-completing Finally never supplies the completion value).
    ast.TryStatement(_, ast.TryFinally(..))
    | ast.TryStatement(_, ast.TryCatchFinally(..)) ->
      emit_stmt_tail_completion(e, stmt)

    ast.TryStatement(block, ast.TryCatch(ast.CatchClause(param, catch_body))) -> {
      let #(e, catch_label) = fresh_label(e)
      let #(e, end_label) = fresh_label(e)

      let e = emit_ir(e, IrPushTry(catch_label, CatchOnly))
      use e <- result.try(emit_block(e, block, tail: True))
      let e = emit_op(e, opcode.PopTry)
      let e = emit_ir(e, IrJump(end_label))

      use e <- result.map(
        emit_catch_clause(e, catch_label, param, emit_block(
          _,
          catch_body,
          tail: True,
        )),
      )
      emit_ir(e, IrLabel(end_label))
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

/// Tail-position emission via the completion-value mechanism: seed an
/// anonymous scratch slot to undefined, emit the statement with
/// Emitter.completion_var set (expression statements in the subtree store
/// into it; loops/if/switch/try reset it on entry), then read the slot
/// back as the statement's completion value.
///
/// No `enter_scope`/`leave_scope`: the slot is a nameless scratch local
/// (alloc_scratch via `fresh_slot`), never captured, so it needs no scope
/// node. The old wrapper existed only so the named completion var lived in
/// its own block scope; with a scratch slot that wrapper would just consume
/// a child id off `scope_cursor` for which the analyzer registers no node.
fn emit_stmt_tail_completion(
  e: Emitter,
  stmt: ast.Statement,
) -> Result(Emitter, EmitError) {
  let #(e, slot) = fresh_slot(e)
  let saved_var = e.completion_var
  let e = seed_local(e, slot, JsUndefined)
  let e = Emitter(..e, completion_var: Some(slot))
  use e <- result.map(emit_stmt(e, stmt))
  let e = Emitter(..e, completion_var: saved_var)
  emit_scratch_get(e, slot)
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
        [IrFinal(opcode.SetLine(prev)), ..rest] ->
          case prev == line {
            True -> e
            False -> Emitter(..e, code: [IrFinal(opcode.SetLine(line)), ..rest])
          }
        _ -> emit_op(e, opcode.SetLine(line))
      }
  }
}

/// Emit a Block statement: descend into its scope-tree node (which emits
/// the block's binding prologue), perform BlockDeclarationInstantiation
/// (§14.2.3 — function declarations instantiated at block entry), emit
/// body, restore the parent scope.
fn emit_block(
  e: Emitter,
  body: List(ast.StmtWithLine),
  tail tail: Bool,
) -> Result(Emitter, EmitError) {
  // Scope elision (V8/SpiderMonkey do the same): a block that declares
  // nothing block-scoped creates no bindings, so the scope is skipped
  // entirely — the analyzer omits it from the tree's children list too.
  // Keeps `{}`-heavy code (e.g. the 2^21 empty blocks of
  // staging/sm/regress/regress-610026.js) out of runtime env churn.
  use <- bool.lazy_guard(!ast_util.block_has_declarations(body), fn() {
    case tail {
      True -> emit_stmts_tail(e, body)
      False -> emit_stmts(e, body)
    }
  })
  let #(e, save) = enter_scope(e, in_block: True)
  use e <- result.try(emit_block_declarations(e, body))
  use e <- result.map(case ast_util.has_using_decl(body) {
    // Direct using/await-using declarations: wrap the body in the
    // DisposeResources try/catch/finally so disposal runs on every
    // completion path (normal, throw, break/continue/return via gosub).
    // The user's bindings + scratch slots all live in THIS scope —
    // collect_top_lex_names treats Using/AwaitUsing as ConstBinding.
    True -> emit_block_using(e, body, tail)
    False ->
      case tail {
        True -> emit_stmts_tail(e, body)
        False -> emit_stmts(e, body)
      }
  })
  leave_scope(e, save)
}

/// Body of emit_block when the block contains direct using declarations:
/// build the UsingScope, emit its prelude (scratch slots in the block's own
/// scope), then the try/catch/finally frame around the original statements.
/// In tail position the completion value is tracked via a scratch slot in
/// the same scope (no extra block scope) — emit_using_body emits each
/// statement via emit_stmt, whose ExpressionStatement arm writes to
/// completion_var; the dispose finally runs with completion_var cleared.
fn emit_block_using(
  e: Emitter,
  body: List(ast.StmtWithLine),
  tail: Bool,
) -> Result(Emitter, EmitError) {
  let #(e, scope, items) = build_using_scope(e, body)
  let e = emit_using_prelude(e, scope)
  let saved_cv = e.completion_var
  let #(e, cv) = case tail {
    False -> #(e, None)
    True -> {
      let #(e, slot) = fresh_slot(e)
      let e = declare_scratch(e, slot, JsUndefined)
      #(Emitter(..e, completion_var: Some(slot)), Some(slot))
    }
  }
  use e <- result.map({
    use e <- emit_using_try_wrap(e, scope)
    emit_using_body(e, items)
  })
  let e = Emitter(..e, completion_var: saved_cv)
  case cv {
    Some(slot) -> emit_scratch_get(e, slot)
    None -> e
  }
}

/// §14.2.3 BlockDeclarationInstantiation: the block's let/const/class slots
/// and block-scoped function-declaration slots are pre-registered by the
/// analyzer and seeded by enter_scope's emit_binding_prologue (so closures
/// capture boxed cells, TDZ holds from block entry, and mutually recursive
/// block functions see each other's slots at MakeClosure time). This just
/// initializes the function bindings.
fn emit_block_declarations(
  e: Emitter,
  body: List(ast.StmtWithLine),
) -> Result(Emitter, EmitError) {
  use #(e, funcs) <- result.map(collect_hoisted_funcs(e, body))
  list.fold(funcs, e, fn(e, hf) {
    let #(name, idx) = hf
    let e = emit_op(e, opcode.MakeClosure(idx))
    emit_var_init(e, name)
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

/// Shared `if (cond) cons else alt` shape. Tail and non-tail call sites
/// differ only in which statement emitter recurses into the branches and
/// what a missing else leaves on the stack (undefined vs nothing).
fn emit_if(
  e: Emitter,
  condition: ast.Expression,
  consequent: ast.Statement,
  alternate: Option(ast.Statement),
  branch: fn(Emitter, ast.Statement) -> Result(Emitter, EmitError),
  none: fn(Emitter) -> Emitter,
) -> Result(Emitter, EmitError) {
  let #(e, else_label) = fresh_label(e)
  let #(e, end_label) = fresh_label(e)
  use e <- result.try(emit_expr(e, condition))
  let e = emit_ir(e, IrJumpIfFalse(else_label))
  use e <- result.try(branch(e, block_wrap_fn_decl(consequent)))
  let e = emit_ir(e, IrJump(end_label))
  let e = emit_ir(e, IrLabel(else_label))
  use e <- result.try(case alternate {
    Some(alt) -> branch(e, block_wrap_fn_decl(alt))
    None -> Ok(none(e))
  })
  Ok(emit_ir(e, IrLabel(end_label)))
}

/// Emit the catch-handler prelude/postlude shared by all try/catch shapes:
/// Label; descend into the catch scope; bind-or-pop the thrown value; run
/// emit_body; restore the parent scope.
/// §14.15.3 CatchClauseEvaluation. `catch (param) Block`: enter the Catch
/// scope (catchEnv — it holds ONLY the parameter; the parser gives the
/// Block its own ordinary child scope, consumed by emit_block iff it has
/// declarations). `catch Block` (no binding) creates NO catch environment
/// at all — the thrown value is popped and the Block evaluates like any
/// other; entering a scope here would steal the next sibling's scope from
/// the cursor and desync every binding after it.
fn emit_catch_clause(
  e: Emitter,
  catch_label: Int,
  param: Option(ast.Pattern),
  emit_body: fn(Emitter) -> Result(Emitter, EmitError),
) -> Result(Emitter, EmitError) {
  let e = emit_ir(e, IrLabel(catch_label))
  case param {
    Some(pattern) -> {
      let #(e, save) = enter_scope(e, in_block: e.in_block)
      use e <- result.try(emit_destructuring_bind(e, pattern, CatchBinding))
      use e <- result.map(emit_body(e))
      leave_scope(e, save)
    }
    None -> emit_body(emit_op(e, opcode.Pop))
  }
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

/// Collect let/const names declared directly in the given statement list (NOT
/// recursing into nested blocks). Used to hoist slot-allocation+boxing before
/// hoisted-function MakeClosure so closures capture the box ref, not a stale
/// pre-box value. Wraps ast_util.collect_top_lex_names (which is
/// BindingKind-free: returns `(name, is_const)`) with the local LetBinding /
/// ConstBinding mapping.
fn collect_top_lex_names(
  stmts: List(ast.StmtWithLine),
) -> List(#(String, BindingKind)) {
  use #(name, is_const) <- list.map(ast_util.collect_top_lex_names(stmts))
  case is_const {
    True -> #(name, ConstBinding)
    False -> #(name, LetBinding)
  }
}

/// Collect and compile hoisted function declarations.
/// Returns updated emitter + list of (name, func_index) pairs.
fn collect_hoisted_funcs(
  e: Emitter,
  stmts: List(ast.StmtWithLine),
) -> Result(#(Emitter, List(#(String, Int))), EmitError) {
  use #(e, funcs_rev) <- result.map(
    list.try_fold(stmts, #(e, []), fn(acc, located) {
      let #(e, funcs) = acc
      case ast_util.peel_labels(located.statement) {
        ast.FunctionDeclaration(
          Some(ast.NamedBinding(name, _)),
          params,
          body,
          is_gen,
          is_async,
        ) -> {
          use #(e, child) <- result.map(compile_function_body(
            e,
            Some(name),
            None,
            params,
            StmtsBody(body),
            is_arrow: False,
            is_generator: is_gen,
            is_async:,
            // Function declaration: a constructor unless gen/async.
            is_constructor: !is_gen && !is_async,
            code_kind: opcode.FunctionCode,
            field_init: NoFieldInit,
          ))
          let #(e, idx) = add_child_function(e, child)
          #(e, [#(name, idx), ..funcs])
        }
        _ -> Ok(#(e, funcs))
      }
    }),
  )
  #(e, list.reverse(funcs_rev))
}

/// Emit MakeClosure+ScopePutVar for each hoisted function declaration.
/// Shared prologue step for module/script top scopes.
fn emit_hoisted_funcs(
  e: Emitter,
  hoisted_funcs: List(#(String, Int)),
) -> Emitter {
  list.fold(hoisted_funcs, e, fn(e, hf) {
    let #(name, func_idx) = hf
    e |> emit_op(opcode.MakeClosure(func_idx)) |> emit_var_put(name)
  })
}

/// §10.2.11 step 28.f.i.2: a body-scope `var n` where `n` is also a
/// parameter binding (a formal name, or `arguments` for non-arrows) is
/// initialized to the PARAMETER's current value rather than undefined —
/// unless `n` is also the name of a body-top-level FunctionDeclaration
/// (those get the hoisted closure instead). Emitted immediately after the
/// body var-boundary scope is entered (its prologue has just seeded every
/// body binding undefined/TDZ), reading the source slot from the FUNCTION
/// scope (params, `<paramN>` shims, `arguments`) via `scope.lookup` —
/// resolving by name from inside the body scope would find the body's own
/// shadowing binding.
///
/// The source set is the spec's parameterBindings, NOT "any name bound in
/// the Function scope": the analyzer also puts CaptureBindings (free names
/// of param-default closures) and the NFE self-name there, and copying
/// those would leak outer values into body `var`s the spec initializes to
/// undefined.
fn emit_body_param_copies(
  e: Emitter,
  fn_scope_id: ScopeId,
  declared_param_names: List(String),
  is_arrow: Bool,
  stmts: List(ast.StmtWithLine),
) -> Emitter {
  let body_id = e.current_scope
  // Defensive: enter_scope's empty-cursor fallback leaves current_scope at
  // the fn scope; iterating its bindings would self-copy every param. That
  // can only mean a parser↔emit lockstep bug — emit nothing.
  use <- bool.guard(body_id == fn_scope_id, e)
  let parameter_bindings = case is_arrow {
    True -> declared_param_names
    False -> ["arguments", ..declared_param_names]
  }
  let function_names = ast_util.direct_fn_names(stmts)
  let body_bindings =
    dict.to_list(scope.get_scope(e.scope_tree, body_id).bindings)
    |> list.sort(fn(a, b) { int.compare({ a.1 }.slot, { b.1 }.slot) })
  use e, #(bname, b) <- list.fold(body_bindings, e)
  let copies =
    b.kind == VarBinding
    && list.contains(parameter_bindings, bname)
    && !list.contains(function_names, bname)
  use <- bool.guard(!copies, e)
  case scope.lookup(e.scope_tree, fn_scope_id, bname) {
    scope.Plain(scope.Local(slot: src_slot, boxed: src_boxed, ..)) -> {
      // The copy IS a read of the parameter-scope `arguments` binding, so
      // it must trigger the IrCreateArguments splice like any other read.
      let e = track_arguments_ref(e, bname)
      emit_slot_get(e, scope.SlotRef(slot: src_slot, boxed: src_boxed))
      |> emit_slot_put(scope.SlotRef(slot: b.slot, boxed: b.is_boxed))
    }
    // Degenerate: the analyzer registered no Function-scope binding for
    // the name (only possible for `arguments` in exotic shapes). Nothing
    // to copy from — the prologue's undefined seed stands.
    scope.Plain(scope.Global(_))
    | scope.Plain(scope.EvalEnv(_))
    | scope.WithChain(..) -> e
  }
}

/// Compile a function body into a CompiledChild. Pops the next entry from
/// `parent.child_fn_cursor` to learn THIS body's analyzer-assigned
/// function-scope id, projects a per-function tree from
/// `parent.scope_tree` rooted there, and threads that tree into the
/// child emitter so its emit_var_* / get_lexical / set_this calls resolve
/// against the analyzer's slot/box decisions. Returns the updated parent
/// (cursor popped) and the compiled child.
fn compile_function_body(
  parent: Emitter,
  name: Option(String),
  self_name: Option(String),
  params: List(ast.Pattern),
  body: FnBody,
  is_arrow is_arrow: Bool,
  is_generator is_generator: Bool,
  is_async is_async: Bool,
  is_constructor is_constructor: Bool,
  code_kind code_kind_if_not_arrow: opcode.CodeKind,
  field_init field_init: FieldInitMode,
) -> Result(#(Emitter, CompiledChild), EmitError) {
  // Consume the next child-function scope id. The analyzer's
  // `declare_stmts_hoist_order` walks each statement list with direct
  // FunctionDeclarations first — the same order `collect_hoisted_funcs`
  // compiles them here — so the cursor and emission order stay aligned
  // even when a FunctionDeclaration shares a statement list with an
  // earlier-in-source function expression / arrow / class. An exhausted
  // cursor means the emitter is compiling a child function the analyzer
  // never numbered (a walk-order desync) — crash here rather than
  // miscompile the child against the root scope's slots.
  let assert [fn_id, ..rest] = parent.child_fn_cursor
  let parent = Emitter(..parent, child_fn_cursor: rest)
  // Every prologue scan below (directives, hoisting, lexical names) is a walk
  // over source statements. A synthetic field-init body has none of those —
  // no directives, no declarations, no `using` — so it scans as an empty body
  // and only the body-emission step below dispatches on it.
  let stmts = case body {
    StmtsBody(stmts:) -> stmts
    FieldInitsBody(..) -> []
  }
  // Direct using/await-using declarations are emitted in place (no AST
  // rewrite) — the lexical-name and hoisting scans below see the user's
  // bindings (collect_top_lex_names treats Using/AwaitUsing as ConstBinding)
  // and the body emission wraps the post-directive statements in the
  // DisposeResources try/catch/finally at function scope.
  let body_has_using = ast_util.has_using_decl(stmts)

  // Strictness: inherit from parent, upgrade if body prologue has "use strict".
  // (Classes force strict at the call site by passing a strict parent emitter.)
  let child_strict = parent.strict || ast_util.has_use_strict_directive(stmts)

  // CodeKind (mirrors quickjs.c:36052-36076). Arrows inherit the parent
  // emitter's kind verbatim — `code_kind_if_not_arrow` is ignored for them.
  // Non-arrows take it as passed by the caller.
  let code_kind = case is_arrow {
    True -> parent.code_kind
    False -> code_kind_if_not_arrow
  }
  // Like CodeKind, arrows inherit FieldInitMode so `()=>super()` inside a
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
  let param_names =
    set.from_list(list.flat_map(params, ast.pattern_bound_names))

  // Fresh emitter wired to the parent's whole-program scope tree and
  // positioned at this child's function-kind scope so emit_var_* /
  // get_lexical / set_this resolve against the analyzer's slot/box
  // decisions for THIS frame. Inherits only the label counter (for
  // uniqueness), strictness, and the lexically-scoped with/private envs.
  let e =
    Emitter(
      ..new_emitter(parent.scope_tree, fn_id),
      next_label: parent.next_label,
      strict: child_strict,
      is_async:,
      is_arrow:,
      code_kind:,
      field_init:,
      // Enclosing `with` scopes stay visible to nested functions — their
      // free names must check the with objects (captured from the parent).
      with_stack: parent.with_stack,
      // A function's [[PrivateEnvironment]] is fixed at its definition
      // site — nested functions see the enclosing classes' private names.
      private_env: parent.private_env,
      // Set by compile_class_body around the one call that compiles a
      // synthesized default derived constructor (see the field's doc).
      in_synth_default_ctor: parent.in_synth_default_ctor,
    )
  // Non-arrows own all four lexical slots starting at len(captures), in
  // canonical order — pre-registered by the analyzer. Runtime setup_frame
  // writes [this, active_func, home_object, new_target] there before pc=0.
  // Arrows skip — their get_lexical resolves to captures from the
  // enclosing non-arrow.
  let e = enter_root_scope(e)

  // A trailing rest parameter (`...rest`) is bound separately from the fixed
  // params: the fixed ones bind positionally (arity counts only them, so
  // build_locals leaves the rest slot undefined), then IrCreateRestArray
  // collects the leftover args into an Array. `arity` excludes the rest param,
  // which also gives the correct `fn.length` (§15.1.5).
  let #(fixed_params, rest_param) = ast_util.split_trailing_rest(params)
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
  // Shared predicate with the parser's scope pass
  // (ast_util.all_simple_params) — the two MUST agree.
  let non_simple_fixed = !ast_util.all_simple_params(fixed_params)

  // Phase 1: Declare parameters. Simple lists bind identifiers positionally;
  // non-simple lists route EVERY fixed param through a `<paramN>` shim
  // binding (scope.param_shim) so the real names can be TDZ-declared and
  // initialized in order. The shim is NOT pure scratch: the runtime locals
  // layout is [captures, lexical_seeds, args, undef] (frame.gleam:57 /
  // arc_vm_ffi.erl setup_locals_tuple), so positional arg N lands at slot
  // len(captures)+len(lexical_seeds)+N — never raw N. The analyzer
  // (scope.gleam add_binding) allocates the shim's slot at that offset, so
  // we resolve it by name via scope.lookup rather than hardcoding an index.
  let destructured_params = case non_simple_fixed {
    False -> []
    True ->
      list.index_map(fixed_params, fn(param, idx) {
        #(scope.param_shim(idx), param)
      })
  }

  // The `arguments` object is materialised only when the body (or a nested
  // arrow, or a parameter default expression) actually references it — that
  // fact is recorded on the emitter's `references_arguments` flag during
  // emission and read back AFTER the body is emitted. The three setup ops
  // (DeclareVar/IrCreateArguments/IrScopePutVar) must sit here in the
  // prologue — after the positional param declares, before parameter
  // destructuring — so default-value expressions can use `arguments`.
  // Snapshot the prologue-so-far and continue emitting onto an empty list;
  // the setup ops are spliced between the two halves once the answer is
  // known (see `uses_args` below).
  let pre_args_code = e.code
  let e = Emitter(..e, code: [])

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
        !child_strict && list.contains(fn_info(e).annexb_candidates, fname)
      let shadowed =
        set.contains(param_names, fname)
        // Non-arrow functions always have an `arguments` binding per
        // §10.2.11 (we elide IrCreateArguments when unread, but the spec
        // binding still shadows funcEnv). self_name is only Some for
        // non-arrow named function expressions, so the shadow is
        // unconditional on the name.
        || fname == "arguments"
        || list.contains(ast_util.collect_hoisted_vars(stmts), fname)
        || list.contains(ast_util.direct_fn_names(stmts), fname)
        || list.any(collect_top_lex_names(stmts), fn(lex) { lex.0 == fname })
        || annexb_shadow
      case shadowed {
        True -> e
        False -> {
          let e = get_lexical(e, opcode.RefActiveFunc)
          emit_var_init(e, fname)
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
  let declared_param_names = list.flat_map(params, ast.pattern_bound_names)
  let param_scope_names = case is_arrow {
    True -> declared_param_names
    False -> ["arguments", ..declared_param_names]
  }
  let e = Emitter(..e, param_scope_names:)
  // §10.2.11 step 21: for non-simple lists, the analyzer pre-registers
  // every parameter name as an uninitialized (TDZ) LetBinding;
  // emit_binding_prologue seeded them at enter_root_scope. The fold below
  // initializes them left to right, so `(x = x)` / `(x = y, y)` defaults
  // throw a ReferenceError on a not-yet-initialized parameter.
  use e <- result.try(
    list.try_fold(destructured_params, e, fn(e, dp) {
      let #(shim, pattern) = dp
      let e = emit_var_get(e, shim)
      emit_destructuring_bind(e, pattern, LetBinding)
    }),
  )

  // Phase 2b: Bind the trailing rest parameter, if any. Build the array from
  // the args at `arity` and beyond, then bind it (an identifier, or a nested
  // destructuring target like `...[a, b]`). ParamBinding declares the slot.
  use e <- result.try(case rest_param {
    None -> Ok(e)
    Some(rest_target) -> {
      let e = emit_op(e, opcode.CreateRestArray(arity))
      // Non-simple lists pre-declared the rest name(s) as TDZ lets above, so
      // bind via LetBinding (declare no-ops, init initializes). Simple-with-
      // rest lists keep the original ParamBinding path.
      let rest_kind = case non_simple_fixed {
        True -> LetBinding
        False -> ParamBinding
      }
      emit_destructuring_bind(e, rest_target, rest_kind)
    }
  })

  // Parameter initialization is done — evals in the body proper run with
  // the body's VariableEnvironment, so the param-scope conflict check no
  // longer applies.
  let e = Emitter(..e, param_scope_names: [])

  // §10.2.11 step 28: a non-simple parameter list gets a SEPARATE variable
  // environment for the body — a var-boundary Block scope the parser
  // pushed as the last block-kind child of the function root — so closures
  // created inside parameter initializers cannot see body `var`s.
  // LOCKSTEP: the parser pushes exactly one such scope iff this same
  // `non_simple_fixed` predicate is true, so it is the next unconsumed
  // `scope_cursor` head here. Its binding prologue (enter_scope) seeds the
  // body's bindings, then the step-28.f.i.2 copies initialize body `var`s
  // that shadow a parameter name from the parameter's current value.
  let #(e, body_save) = case non_simple_fixed {
    False -> #(e, None)
    True -> {
      let #(e, save) = enter_scope(e, in_block: e.in_block)
      let e =
        emit_body_param_copies(
          e,
          save.scope,
          declared_param_names,
          is_arrow,
          stmts,
        )
      #(e, Some(save))
    }
  }

  // Hoisting for the function body. Hoisted var-declarations, top-level
  // function-decl names, sloppy-mode Annex B function-in-block names
  // (initialized undefined unless blocked by a lexical declaration on the
  // path to their block), and top-level let/const/class slots are all
  // pre-registered by the analyzer and seeded by enter_root_scope's
  // emit_binding_prologue — before hoisted-func MakeClosure so captured
  // variables are boxed by the time the closure reads them. The actual
  // let/const initializers still run at their statement's position (TDZ).
  use #(e, hoisted_funcs) <- result.try(collect_hoisted_funcs(e, stmts))

  let e =
    list.fold(hoisted_funcs, e, fn(e, hf) {
      let #(fname, func_idx) = hf
      let e = emit_op(e, opcode.MakeClosure(func_idx))
      let e = emit_var_put(e, fname)
      e
    })

  // For generators, emit IrInitialYield after parameter setup and hoisting,
  // but before the function body. This suspends execution so the generator
  // returns the iterator object (caller must call .next() to start).
  // Async functions do NOT get InitialYield — they run eagerly until the first
  // await or completion.
  let e = case is_generator {
    True -> emit_op(e, opcode.InitialYield)
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

  // Emit the body: field-init elements for a class-init function, statements
  // otherwise.
  use e <- result.try(case body, body_has_using {
    FieldInitsBody(inits:), _ -> list.try_fold(inits, e, emit_field_init)
    StmtsBody(..), False -> emit_stmts(e, stmts)
    StmtsBody(..), True -> {
      // Directive prologue stays at function-scope top (already scanned
      // for "use strict" above); the rest is wrapped in the
      // DisposeResources try/catch/finally so a `return` inside the body
      // crosses the gosub barrier and runs disposal before leaving.
      let #(directives, rest) = ast_util.split_directives(stmts)
      use e <- result.try(emit_stmts(e, directives))
      let #(e, scope, items) = build_using_scope(e, rest)
      let e = emit_using_prelude(e, scope)
      use e <- emit_using_try_wrap(e, scope)
      emit_using_body(e, items)
    }
  })

  // Close the §10.2.11 step-28 body var-boundary scope (emits nothing —
  // only restores the emitter's scope cursor).
  let e = case body_save {
    Some(save) -> leave_scope(e, save)
    None -> e
  }

  // Implicit return undefined at end
  let e = push_const(e, JsUndefined)
  let e = emit_op(e, opcode.Return)

  // Body and parameter-default emission is complete — `references_arguments`
  // now answers whether the function body references `arguments` (recursing
  // into arrow children via add_child_function, but NOT into non-arrow
  // nested functions, which have their own). Only non-arrow functions get
  // the binding — arrows resolve `arguments` as a free variable captured
  // from the enclosing scope. Splice the setup ops (or nothing) between
  // the prologue snapshot and the rest of the body.
  let uses_args = !is_arrow && e.references_arguments
  // The `arguments` binding lives in the root function scope; the analyzer
  // pre-registers it (VarBinding) when the body references it. Resolve it
  // here so the spliced setup uses a concrete PutLocal/PutBoxed op. The
  // function-root scope has no enclosing `with` IN THIS FRAME, so the
  // resolution is never a WithChain — Local when the analyzer registered
  // it, Global/EvalEnv as a fallthrough otherwise (degenerate but kept for
  // the type's sake).
  let put_args = case scope.lookup(e.scope_tree, e.fn_scope, "arguments") {
    scope.Plain(scope.Local(slot:, boxed: True, ..)) ->
      IrFinal(opcode.PutBoxed(slot))
    scope.Plain(scope.Local(slot:, boxed: False, ..)) ->
      IrFinal(opcode.PutLocal(slot))
    scope.Plain(scope.Global(_))
    | scope.Plain(scope.EvalEnv(_))
    | scope.WithChain(..) -> IrFinal(opcode.PutGlobal("arguments"))
  }
  // The `arguments` slot was already seeded (push undef + PutLocal +
  // optional BoxLocal) by `emit_binding_prologue` at `enter_root_scope` —
  // the analyzer pre-registers a VarBinding for it whenever the body
  // references `arguments`. The splice only needs to CREATE the object
  // and STORE it.
  let args_setup_rev = case uses_args {
    True -> [
      put_args,
      IrFinal(opcode.CreateArguments(
        simple_params: !non_simple_fixed && rest_param == None,
      )),
    ]
    False -> []
  }
  let e =
    Emitter(..e, code: list.flatten([e.code, args_setup_rev, pre_args_code]))
  let #(code, constants, children) = finish(e)

  let child =
    CompiledChild(
      // The child's function-scope id in the whole-program tree —
      // `compile_child` keys `scope.function_info` on it. The per-function
      // tree wired onto this child emitter has it as `root`.
      scope_id: e.fn_scope,
      name:,
      arity:,
      length: expected_length,
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
  // Thread the child's scope tree back to the parent: `fresh_slot` /
  // `alloc_scratch` bumped this child's FunctionInfo.local_count (and any
  // grandchild's, via the same recursion) on the child emitter's copy of
  // the whole-program tree. The parent's copy was the seed, so replacing
  // it absorbs every scratch-slot allocation made under this child without
  // touching the parent's own FunctionInfo. Without this, compiler.gleam
  // reads a stale local_count and the runtime locals tuple is allocated
  // too small — IrPutLocal on a scratch slot then crashes with
  // `badarg setelement` past the tuple end.
  Ok(#(Emitter(..parent, scope_tree: e.scope_tree), child))
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
    // Empty block: compiles to nothing (scope elision, see emit_block).
    ast.BlockStatement([]) -> Ok(e)
    _ -> emit_stmt_inner(e, stmt)
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
          emit_scratch_put(e, v)
        }
        _ -> e
      }
    None -> e
  }
  case stmt {
    ast.EmptyStatement | ast.DebuggerStatement -> Ok(e)

    ast.ExpressionStatement(expression: expr, ..) -> {
      use e <- result.map(emit_expr(e, expr))
      // Completion-value mode: the statement's value becomes the tracked V
      // (IrPutLocal pops, so stack balance matches the IrPop path).
      case e.completion_var {
        Some(v) -> emit_scratch_put(e, v)
        None -> emit_op(e, opcode.Pop)
      }
    }

    ast.BlockStatement(body) -> emit_block(e, body, tail: False)

    ast.VariableDeclaration(kind, declarators) -> {
      // Using/AwaitUsing never reach this arm — ast_util.has_using_decl routes the
      // enclosing list through emit_using_body which emits them directly.
      // The arms below treat them as Const for exhaustiveness.
      let binding_kind = case kind {
        ast.Var -> VarBinding
        ast.Let -> LetBinding
        ast.Const | ast.Using | ast.AwaitUsing -> ConstBinding
      }
      list.try_fold(declarators, e, fn(e, decl) {
        case decl {
          ast.VariableDeclarator(ast.IdentifierPattern(name, ..), init) -> {
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
                  ast.Var -> emit_var_put(e, name)
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

    ast.IfStatement(cond, cons, alt) ->
      emit_if(e, cond, cons, alt, emit_stmt, fn(e) { e })

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
      let e = pop_frame(e)
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
      let e = pop_frame(e)
      Ok(e)
    }

    // `for ([await] using x = …; c; u)` — resource acquired once before
    // the loop, disposed once after. Direct DisposeResources lowering
    // around the headless loop (no AST rewrite). pending_label flows
    // through to the inner push_loop so labeled break/continue still
    // target the loop.
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
      let #(e, save) =
        enter_for_scope(e, ast_util.for_classic_init_is_lex(init))
      use #(e, per_iter) <- result.try(case init {
        Some(ast.ForInitExpression(expr)) -> {
          use e <- result.map(emit_expr(e, expr))
          #(emit_op(e, opcode.Pop), [])
        }
        Some(ast.ForInitDeclaration(kind:, declarations:)) -> {
          use e <- result.map(emit_stmt(
            e,
            ast.VariableDeclaration(kind:, declarations:),
          ))
          #(e, ast_util.for_let_names(kind, declarations))
        }
        // ForInitPattern only appears in for-in/for-of heads.
        Some(ast.ForInitPattern(_)) | None -> Ok(#(e, []))
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
        None -> Ok(push_const(e, JsUndefined))
      })
      // A return crosses every frame — see emit_return_cross_frame.
      let e = list.fold(e.frame_stack, e, emit_return_cross_frame)
      Ok(emit_op(e, opcode.Return))
    }

    ast.ThrowStatement(arg) -> {
      use e <- result.map(emit_expr(e, arg))
      emit_op(e, opcode.Throw)
    }

    ast.TryStatement(block, tail) -> {
      case tail {
        // try/catch (no finally)
        ast.TryCatch(ast.CatchClause(param, catch_body)) -> {
          let #(e, catch_label) = fresh_label(e)
          let #(e, end_label) = fresh_label(e)

          let e = emit_ir(e, IrPushTry(catch_label, CatchOnly))
          let e = push_barrier(e, pop_try: 1, label_finally: None, drop: 0)
          use e <- result.try(emit_block(e, block, tail: False))
          let e = pop_frame(e)
          let e = emit_op(e, opcode.PopTry)
          let e = emit_ir(e, IrJump(end_label))

          use e <- result.map(
            emit_catch_clause(e, catch_label, param, emit_block(
              _,
              catch_body,
              tail: False,
            )),
          )
          emit_ir(e, IrLabel(end_label))
        }

        // try/finally (no catch). QuickJS js_parse_try TOK_FINALLY-only path
        // (quickjs.c:28917-28922 + 28926-28962).
        ast.TryFinally(finally_body) -> {
          let #(e, throw_label) = fresh_label(e)
          let #(e, fin_label) = fresh_label(e)
          let #(e, end_label) = fresh_label(e)

          // -- try body --------------------------------------------------
          let e =
            emit_ir(e, IrPushTry(throw_label, Finally(LabelId(fin_label))))
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

        // try/catch/finally. QuickJS js_parse_try with-catch path
        // (quickjs.c:28824-28912 + 28926-28962). Arc keeps the existing
        // two-PushTry-upfront structure (vs QuickJS's catch2-inside-handler) so
        // throws during catch-param destructuring are also wrapped by finally.
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
          let e = push_frame(e, LabeledBlockFrame(break_target:, label:))
          use e <- result.map(emit_stmt(e, body))
          let e = pop_frame(e)
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
          // Class names are block-scoped (like let)
          let e = declare_lex(e, n, False)
          use e <- result.map(compile_class(
            e,
            Some(n),
            Some(n),
            super_class,
            body,
          ))
          // compile_class leaves [ctor] on stack; init pops it
          init_lex(e, n)
        }
        // Statement-position `class` requires a name (the parser's
        // name_required flag); anonymous `export default class {}` is
        // rewritten to a ClassExpression before it reaches the emitter.
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

/// §14.11 WithStatement (sloppy mode only — the parser rejects `with` in
/// strict code). The ToObject'd head expression is stored in a synthetic
/// block-scoped local; the body sits in the analyzer's With scope whose
/// `scope.With(holder)` kind names that local (the `<withN_M>` holder) —
/// `scope.lookup` picks it up
/// for every name resolution inside the body (returned as `WithChain`) and
/// the emit_var_* helpers emit
/// the IrWith* runtime probes inline.
///
/// The holder binding is read FROM THE TREE — the analyzer mints its name
/// (`sb_push_with`) and allocates its slot, so the emitter never invents a
/// name or slot of its own (avoids the emitter↔analyzer name desync the old
/// `fresh_label`-based scheme had). That same name goes on `with_stack` so
/// direct-eval inside the body (IrCallEval) sees the analyzer's holder
/// names. No EmitterOp scope markers are emitted: the Phase-2 IR walker is
/// gone and `finish` would just discard them.
fn emit_with(
  e: Emitter,
  object: ast.Expression,
  body: ast.Statement,
  tail tail: Bool,
) -> Result(Emitter, EmitError) {
  use e <- result.try(emit_expr(e, object))
  let e = emit_op(e, opcode.ToObject)
  // Enter the analyzer's With scope. ONE cursor move — the analyzer
  // creates a single With node that BOTH holds the `<withN_M>` binding
  // (so `enter_scope`'s prologue seeds its slot) and carries that holder's
  // NAME on its `With(holder)` kind (so `scope.lookup` from inside the body
  // wraps the resolution in a `WithChain`). The body's own block, if any, is
  // a child of this node and is entered by emit_stmt/emit_block — NOT here.
  let #(e, save) = enter_scope(e, in_block: e.in_block)
  // Read the holder BY NAME off the entered scope's kind and store the
  // ToObject'd target straight into its slot. `enter_scope` has just
  // seeded that slot with JsUninitialized (LetBinding prologue) and boxed
  // it if the analyzer marked it captured; this overwrites the seed.
  // Marking the slot `initialized` keeps later TDZ-check heuristics from
  // treating it as possibly-uninit.
  //
  // Every entry point wires the analyzer tree, and only `sb_push_with`
  // creates a With scope (holder name on the kind, holder binding declared
  // in the same scope) — a non-With kind here means the emitter is not
  // sitting in that node (a cursor desync), so crash rather than store the
  // with-object into some unrelated slot.
  let with_scope = scope.get_scope(e.scope_tree, e.current_scope)
  let assert scope.With(holder: synth) = with_scope.kind
    as "emit_with: emitter cursor is not on the analyzer's With scope"
  let assert Ok(scope.Binding(slot:, is_boxed:, ..)) =
    dict.get(with_scope.bindings, synth)
    as "emit_with: With scope is missing its holder binding"
  let e = Emitter(..e, initialized: set.insert(e.initialized, slot))
  let e = emit_slot_put(e, scope.SlotRef(slot:, boxed: is_boxed))
  let e = Emitter(..e, with_stack: [synth, ..e.with_stack])
  use e <- result.map(case tail {
    True -> emit_stmt_tail(e, body)
    False -> emit_stmt(e, body)
  })
  // Pop the holder pushed above. Body emission pushes and pops with_stack
  // entries in balanced pairs, so ours is still on top.
  let assert [_, ..with_rest] = e.with_stack
  leave_scope(Emitter(..e, with_stack: with_rest), save)
}

// ============================================================================
// Expression emission
// ============================================================================

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
  let e = emit_op(e, opcode.Pop)
  let e = push_const(e, JsUndefined)
  let e = emit_ir(e, IrJump(end_label))
  // Depth-2 cleanup: [f, receiver] → [undefined]
  let e = emit_ir(e, IrLabel(l2))
  let e = emit_op(e, opcode.Pop)
  let e = emit_op(e, opcode.Pop)
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
  use <- bool.lazy_guard(!ast_util.chain_has_optional(expr), fn() {
    emit_expr(e, expr)
  })
  case expr {
    ast.MemberExpression(_, obj, ast.Identifier(name:, ..), False)
    | ast.OptionalMemberExpression(_, obj, ast.Identifier(name:, ..), False) -> {
      use e <- result.map(chain_obj(e, expr, obj, l1, l2))
      emit_get_field(e, name)
    }
    ast.MemberExpression(_, obj, prop, True)
    | ast.OptionalMemberExpression(_, obj, prop, True) -> {
      use e <- result.try(chain_obj(e, expr, obj, l1, l2))
      use e <- result.map(emit_expr(e, prop))
      emit_op(e, opcode.GetElem)
    }
    // Non-optional call AFTER an optional link, e.g. `a?.b.m(x)` /
    // `a?.b(x)` — `this` binding follows the method-reference shape.
    ast.CallExpression(_, callee, args) -> {
      use #(e, is_method) <- result.try(emit_chain_callee(e, callee, l1, l2))
      emit_chain_call_args(e, args, is_method)
    }
    // Optional call `f?.(x)`: additionally check the function value itself.
    ast.OptionalCallExpression(_, callee, args) -> {
      use #(e, is_method) <- result.try(emit_chain_callee(e, callee, l1, l2))
      let e = emit_op(e, opcode.Dup)
      let e = case is_method {
        True -> emit_ir(e, IrJumpIfNullish(l2))
        False -> emit_ir(e, IrJumpIfNullish(l1))
      }
      emit_chain_call_args(e, args, is_method)
    }
    // Unreachable on valid AST: chain_has_optional returns True only for the
    // arms above plus TaggedTemplateExpression — and `?.` followed by a
    // template literal is a §13.3.1.1 early SyntaxError.
    other -> emit_expr(e, other)
  }
}

/// Emit a chain link's object, plus the `Dup; JumpIfNullish` short-circuit
/// guard when the link itself is optional (`?.`).
fn chain_obj(
  e: Emitter,
  link: ast.Expression,
  obj: ast.Expression,
  l1: Int,
  l2: Int,
) -> Result(Emitter, EmitError) {
  use e <- result.map(emit_chain(e, obj, l1, l2))
  case link {
    ast.OptionalMemberExpression(..) ->
      e |> emit_op(opcode.Dup) |> emit_ir(IrJumpIfNullish(l1))
    _ -> e
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
    // super.m?.() / super[k]?.() — §13.3.7.3 super ref, lexical-this receiver.
    ast.MemberExpression(_, ast.SuperExpression(_), key, computed) -> {
      use e <- result.map(emit_super_method_ref(e, key, computed))
      #(e, True)
    }
    ast.MemberExpression(_, obj, ast.Identifier(name:, ..), False)
    | ast.OptionalMemberExpression(_, obj, ast.Identifier(name:, ..), False) -> {
      use e <- result.map(chain_obj(e, callee, obj, l1, l2))
      #(emit_get_field2(e, name), True)
    }
    ast.MemberExpression(_, obj, key, True)
    | ast.OptionalMemberExpression(_, obj, key, True) -> {
      use e <- result.try(chain_obj(e, callee, obj, l1, l2))
      use e <- result.map(emit_expr(e, key))
      // [f, key, receiver] → [f, receiver]
      #(
        e
          |> emit_op(opcode.GetElem2)
          |> emit_op(opcode.Swap)
          |> emit_op(opcode.Pop),
        True,
      )
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
  case is_method {
    True ->
      emit_call_args(
        e,
        args,
        fn(n) { IrFinal(opcode.CallMethod(n)) },
        IrFinal(opcode.CallMethodApply),
      )
    False ->
      emit_call_args(
        e,
        args,
        fn(n) { IrFinal(opcode.Call(n)) },
        IrFinal(opcode.CallApply),
      )
  }
}

/// A parsed numeric literal as the VM's number type. `1e400` overflows the
/// double range and denotes +Infinity — a literal is never negative and never
/// NaN, so those two JsNum cases are unreachable from source.
fn literal_number(n: ast.LiteralNumber) -> value.JsNum {
  case n {
    ast.FiniteNumber(f) -> Finite(f)
    ast.InfiniteNumber -> value.Infinity
  }
}

fn emit_expr(e: Emitter, expr: ast.Expression) -> Result(Emitter, EmitError) {
  case expr {
    // Literals
    ast.NumberLiteral(_, value) ->
      Ok(push_const(e, JsNumber(literal_number(value))))
    ast.BigIntLiteral(value: n, ..) ->
      Ok(push_const(e, value.JsBigInt(value.BigInt(n))))
    ast.StringExpression(_, value) -> Ok(push_const(e, JsString(value)))
    ast.BooleanLiteral(_, value) -> Ok(push_const(e, JsBool(value)))
    ast.NullLiteral(_) -> Ok(push_const(e, JsNull))
    ast.UndefinedExpression(_) -> Ok(push_const(e, JsUndefined))

    // Identifier
    ast.Identifier(name: "undefined", ..) -> Ok(push_const(e, JsUndefined))
    // Bare PrivateIdentifier outside `#x in obj` — early error per §13.10.1.
    // The `#x in obj` BinaryExpression arm below does NOT recurse on its LHS,
    // so this only catches genuinely-bare `#x` used as a value.
    ast.Identifier(name: "#" <> rest, ..) ->
      Error(EarlySyntaxError("Unexpected private name #" <> rest))
    ast.Identifier(name:, ..) -> Ok(emit_var_get(e, name))

    // Binary expressions
    // §13.10.1 RelationalExpression : PrivateIdentifier `in` ShiftExpression.
    // LHS is a name, not a value — emit only RHS, then PrivateIn(name).
    // Stack: [obj] → [bool].
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
      emit_ir(e, IrBinOp(translate_binop(op)))
    }

    // Logical expressions: `&&` / `||` / `??` short-circuit.
    ast.LogicalExpression(_, op, left, right) -> {
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, left))
      let e = emit_short_circuit_test(e, op, end_label)
      let e = emit_op(e, opcode.Pop)
      use e <- result.map(emit_expr(e, right))
      emit_ir(e, IrLabel(end_label))
    }

    // Unary expressions
    // typeof uses ast_util.unwrap_parens because typeof (x) === typeof x per spec.
    ast.UnaryExpression(_, ast.TypeOf, arg) ->
      case ast_util.unwrap_parens(arg) {
        ast.Identifier(name:, ..) -> {
          // typeof x must NOT throw for undeclared variables
          Ok(emit_var_typeof(e, name))
        }
        _ -> {
          use e <- result.map(emit_expr(e, arg))
          emit_op(e, opcode.TypeOf)
        }
      }

    // delete expression — uses ast_util.unwrap_parens because delete (x) === delete x.
    ast.UnaryExpression(_, ast.Delete, arg) ->
      case ast_util.unwrap_parens(arg) {
        // §13.5.1.2 step 5.b — delete on a super reference is an
        // unconditional ReferenceError. Evaluate `this` (TDZ check) and the
        // computed key for side effects per §13.3.7 ordering, then throw.
        ast.MemberExpression(_, ast.SuperExpression(_), key, computed) -> {
          let e = get_this(e) |> emit_op(opcode.Pop)
          use e <- result.map(case computed {
            True -> result.map(emit_expr(e, key), emit_op(_, opcode.Pop))
            False -> Ok(e)
          })
          emit_op(
            e,
            opcode.ThrowError(
              opcode.ReferenceErrorKind,
              "Unsupported reference to 'super'",
            ),
          )
        }
        ast.MemberExpression(_, obj, ast.Identifier(name: prop, ..), False) -> {
          // delete obj.prop → emit obj, DeleteField(prop)
          use e <- result.map(emit_expr(e, obj))
          emit_ir(e, IrDeleteField(prop))
        }
        ast.MemberExpression(_, obj, key_expr, True) -> {
          // delete obj[key] → emit obj, emit key, DeleteElem
          use e <- result.try(emit_expr(e, obj))
          use e <- result.map(emit_expr(e, key_expr))
          emit_op(e, opcode.DeleteElem)
        }
        ast.Identifier(name:, ..) -> {
          // delete x — Phase 2 emits enclosing-with object checks (§9.1.1.2.7
          // DeleteBinding deletes the property off the with object) and falls
          // back to `true` (can't delete plain vars; legacy behavior).
          Ok(emit_var_delete(e, name))
        }
        _ -> {
          // delete <other expr> → evaluate for side effects, discard, push true
          use e <- result.map(emit_expr(e, arg))
          let e = emit_op(e, opcode.Pop)
          push_const(e, JsBool(True))
        }
      }

    ast.UnaryExpression(_, op, arg) -> {
      // typeof/delete never reach here — their own arms above match first.
      use kind <- result.try(
        translate_unaryop(op)
        |> option.to_result(NonGenericUnaryOperator),
      )
      use e <- result.map(emit_expr(e, arg))
      emit_op(e, opcode.UnaryOp(kind))
    }

    // Update expressions (++/--) — unwrap parens because (x)++ === x++.
    ast.UpdateExpression(
      span,
      op,
      prefix,
      ast.ParenthesizedExpression(_, inner),
    ) ->
      emit_expr(
        e,
        ast.UpdateExpression(span, op, prefix, ast_util.unwrap_parens(inner)),
      )
    // Annex B web-compat: `++f()` / `f()--` parse in sloppy mode, evaluate
    // the call, then throw ReferenceError (before ToNumeric).
    ast.UpdateExpression(_, _, _, ast.CallExpression(..) as call) -> {
      use e <- result.map(emit_expr(e, call))
      let e = emit_op(e, opcode.Pop)
      emit_op(
        e,
        opcode.ThrowError(
          opcode.ReferenceErrorKind,
          "Invalid left-hand side expression in update operation",
        ),
      )
    }
    ast.UpdateExpression(_, op, prefix, ast.Identifier(name:, ..)) -> {
      let one = JsNumber(Finite(1.0))
      let bin_kind = case op {
        ast.Increment -> opcode.Add
        ast.Decrement -> opcode.Sub
      }
      case prefix {
        True -> {
          // ++x: get, ToNumeric (§13.4.2 step 3), add 1; helper dups the
          // result and stores to ref. Unary `+` is ToNumber.
          use e, ref <- with_identifier_lref(e, name)
          let e = emit_var_ref_get(e, ref)
          let e = emit_op(e, opcode.UnaryOp(opcode.Pos))
          let e = push_const(e, one)
          Ok(emit_ir(e, IrBinOp(bin_kind)))
        }
        False -> {
          // x++: make ref, get, ToNumeric (§13.4.2.1 step 3), dup (old value
          // stays as result), add 1, store to ref. Unary `+` is ToNumber.
          let #(e, ref) = emit_var_ref_make(e, name)
          let e = emit_var_ref_get(e, ref)
          let e = emit_op(e, opcode.UnaryOp(opcode.Pos))
          let e = emit_op(e, opcode.Dup)
          let e = push_const(e, one)
          let e = emit_ir(e, IrBinOp(bin_kind))
          let e = emit_var_ref_put(e, ref)
          Ok(e)
        }
      }
    }
    // ++obj.prop / obj[k]++ / super.x++ — the *2 read-keep protocol
    // (base/key/super-base evaluated exactly once), then the same
    // get -> ToNumeric -> ±1 -> put sequence as the Identifier arm above.
    // §13.4.2/§13.4.3 step 3: ToNumeric applies to the OLD value (unary `+`
    // is ToNumber), so `o.x = "5"; o.x++` stores 6 (not "51") and yields 5.
    // Postfix stashes the numeric old value in a scratch slot because the
    // put-args sit between it and the new value on the stack.
    ast.UpdateExpression(_, op, prefix, ast.MemberExpression(..) as member) -> {
      let one = JsNumber(Finite(1.0))
      let bin_kind = case op {
        ast.Increment -> opcode.Add
        ast.Decrement -> opcode.Sub
      }
      use #(e, shape) <- result.map(emit_lvalue_get2(e, member))
      // Stack: [old, …put-args]
      let e = emit_op(e, opcode.UnaryOp(opcode.Pos))
      case prefix {
        True ->
          // [oldNum, …put-args] -> [new, …put-args] -> [new]
          e
          |> push_const(one)
          |> emit_ir(IrBinOp(bin_kind))
          |> emit_lvalue_put(shape)
        False -> {
          // [oldNum, …put-args]: save oldNum aside, write back new, then
          // recover oldNum as the expression value.
          let #(e, tmp) = fresh_slot(e)
          e
          |> emit_op(opcode.Dup)
          |> emit_scratch_put(tmp)
          |> push_const(one)
          |> emit_ir(IrBinOp(bin_kind))
          |> emit_lvalue_put(shape)
          |> emit_op(opcode.Pop)
          |> emit_scratch_get(tmp)
        }
      }
    }
    // Every other `++`/`--` operand is an early error in the parser
    // (`finish_update_expr` requires an assignable LHS), so this is an
    // engine bug, not user code.
    ast.UpdateExpression(..) -> Error(InvalidUpdateTarget)

    // Parenthesized LHS assignment — §13.15.2 IsIdentifierRef returns false
    // for parens, so `(x) = function(){}` must NOT infer the name "x"; hence
    // emit_expr here vs emit_named_expr in the plain-Identifier arm below.
    ast.AssignmentExpression(
      _,
      ast.Assign,
      ast.ParenthesizedExpression(_, ast.Identifier(name:, ..)),
      right,
    ) -> with_identifier_lref(e, name, fn(e, _ref) { emit_expr(e, right) })
    // Non-simple-assign parenthesized LHS — safe to unwrap (no name inference
    // for compound assignment anyway).
    ast.AssignmentExpression(
      span,
      op,
      ast.ParenthesizedExpression(_, inner),
      right,
    ) -> emit_expr(e, ast.AssignmentExpression(span, op, inner, right))

    // Annex B web-compat AssignmentTargetType: `f() = v` / `f() += v` parse
    // in sloppy mode, evaluate the call, then throw ReferenceError BEFORE
    // evaluating the RHS (the parser only lets CallExpression targets
    // through when !strict). Must precede the logical-assign and
    // destructuring branches.
    ast.AssignmentExpression(_, _, ast.CallExpression(..) as call, _) -> {
      use e <- result.map(emit_expr(e, call))
      let e = emit_op(e, opcode.Pop)
      emit_op(
        e,
        opcode.ThrowError(
          opcode.ReferenceErrorKind,
          "Invalid left-hand side in assignment",
        ),
      )
    }

    // §13.15.2 logical assignment: x &&= v, x ||= v, x ??= v. Must precede
    // the generic compound-assignment branches (compound_to_binop has no
    // BinOpKind for these — they short-circuit instead).
    ast.AssignmentExpression(_, ast.LogicalAndAssign, lhs, right) ->
      emit_logical_assign(e, ast.LogicalAnd, lhs, right)
    ast.AssignmentExpression(_, ast.LogicalOrAssign, lhs, right) ->
      emit_logical_assign(e, ast.LogicalOr, lhs, right)
    ast.AssignmentExpression(_, ast.NullishCoalesceAssign, lhs, right) ->
      emit_logical_assign(e, ast.NullishCoalescing, lhs, right)

    // Assignment to identifier — emit_named_expr so anonymous fn/class RHS
    // gets the binding name (§13.15.2 step 1.c NamedEvaluation).
    ast.AssignmentExpression(_, ast.Assign, ast.Identifier(name:, ..), right) -> {
      let inferred_name = case name {
        "*default*" -> "default"
        _ -> name
      }
      with_identifier_lref(e, name, fn(e, _ref) {
        emit_named_expr(e, right, inferred_name)
      })
    }

    // Compound assignment to identifier (x += v etc.).
    ast.AssignmentExpression(_, op, ast.Identifier(name:, ..), right) -> {
      case compound_to_binop(op) {
        Ok(bin_kind) -> {
          use e, ref <- with_identifier_lref(e, name)
          let e = emit_var_ref_get(e, ref)
          use e <- result.map(emit_expr(e, right))
          emit_ir(e, IrBinOp(bin_kind))
        }
        // This arm only matches compound operators; plain `=` and the
        // logical assignments have their own arms above/below.
        Error(Nil) -> Error(NonCompoundAssignOperator)
      }
    }

    // super.prop = val / super[k] = val — §13.15.2 + §13.3.7.3 PutValue with
    // super reference.
    ast.AssignmentExpression(
      _,
      ast.Assign,
      ast.MemberExpression(_, ast.SuperExpression(_), key, computed),
      right,
    ) -> {
      let e = emit_super_base(e)
      use e <- result.try(emit_super_key(e, key, computed))
      use e <- result.map(emit_expr(e, right))
      emit_op(e, opcode.PutSuperValue)
    }

    // Assignment to dot member expression (obj.prop = val)
    ast.AssignmentExpression(
      _,
      ast.Assign,
      ast.MemberExpression(_, obj, ast.Identifier(name: prop, ..), False),
      right,
    ) -> {
      use e <- result.try(emit_expr(e, obj))
      use e <- result.map(emit_expr(e, right))
      // Stack: [val, obj, ...] — PutField pops both, leaves val
      emit_put_field(e, prop)
    }

    // Assignment to computed member expression (obj[key] = val)
    ast.AssignmentExpression(
      _,
      ast.Assign,
      ast.MemberExpression(_, obj, key, True),
      right,
    ) -> {
      use e <- result.try(emit_expr(e, obj))
      use e <- result.try(emit_expr(e, key))
      use e <- result.map(emit_expr(e, right))
      // Stack: [obj, key, val] — PutElem expects [val, key, obj]
      emit_op(e, opcode.PutElem)
    }

    // Compound assignment to member (obj.prop op= v / obj[k] op= v /
    // super.x op= v). The *2 read keeps the write-back operands under the
    // old value so base/key/super-base are evaluated exactly once (§13.15.2;
    // observable via setPrototypeOf-in-toString test262 cases for super).
    ast.AssignmentExpression(_, op, ast.MemberExpression(..) as member, right) ->
      case compound_to_binop(op) {
        Ok(bin_kind) -> {
          use #(e, shape) <- result.try(emit_lvalue_get2(e, member))
          use e <- result.map(emit_expr(e, right))
          emit_ir(e, IrBinOp(bin_kind)) |> emit_lvalue_put(shape)
        }
        // As above: only compound operators reach this arm.
        Error(Nil) -> Error(NonCompoundAssignOperator)
      }

    // Destructuring assignment expression: `[a, x.y] = rhs` or `({a, b} = rhs)`.
    // Parser guarantees LHS is ArrayExpression/ObjectExpression here
    // (parse_assignment_rhs only accepts these when last_expr_assignable=False).
    // Result of the whole expression is rhs (§13.15.2 step 6), so Dup before
    // destructure since emit_destructuring_assign consumes its input.
    ast.AssignmentExpression(_, ast.Assign, lhs, right) -> {
      use e <- result.try(emit_expr(e, right))
      let e = emit_op(e, opcode.Dup)
      emit_destructuring_assign(e, lhs)
    }
    // Only compound assignment (`+=` …) reaches here, and only with a target
    // that is neither an Identifier, a MemberExpression, nor a (web-compat)
    // CallExpression — all handled above. `x++`-style targets are validated
    // by the parser, so this is an engine bug: a destructuring pattern can
    // never carry a compound operator.
    ast.AssignmentExpression(..) -> Error(InvalidCompoundAssignTarget)

    // super(args) — §13.3.7.1 SuperCall, fully decomposed (QuickJS shape):
    //   GetLexical(active_func); GetPrototypeOf;        → parent ctor
    //   GetLexical(new_target);                         → lexical newTarget
    //   <args>; CallConstructor(n);                     → ordinary [[Construct]]
    //   Dup; SetThis;                                   → step 8 BindThisValue
    //   <field-init call if class has instance fields>  → step 12 InitializeInstanceElements
    // Works inside arrows because all reads go through lexical slots, and
    // arrows inherit field_init from the enclosing ctor's emitter.
    ast.CallExpression(_, ast.SuperExpression(_), args) -> {
      let e =
        e
        |> get_lexical(opcode.RefActiveFunc)
        |> emit_op(opcode.GetPrototypeOf)
        |> get_lexical(opcode.RefNewTarget)
      use e <- result.map(case e.in_synth_default_ctor {
        // §15.7.14 ClassDefaultConstructor: a class with no source
        // constructor forwards its whole argument List to the parent
        // as-is. IrCreateRestArray(0) snapshots the frame's arguments
        // into a fresh array — unlike a source-level `super(...xs)`
        // there is NO iteration, so a poisoned
        // %Array.prototype%[@@iterator] must not be observable here.
        True ->
          Ok(
            e
            |> emit_op(opcode.CreateRestArray(0))
            |> emit_op(opcode.CallConstructorApply),
          )
        False ->
          emit_call_args(
            e,
            args,
            fn(n) { IrFinal(opcode.CallConstructor(n)) },
            IrFinal(opcode.CallConstructorApply),
          )
      })
      let e = e |> emit_op(opcode.Dup) |> set_this
      case e.field_init {
        FieldInitAfterSuper -> emit_field_init_call(e)
        NoFieldInit | FieldInitAtStart -> e
      }
    }

    // super.method(args) / super[k](args) — §13.3.7.3 + §13.3.6.2: read
    // super property with receiver=this, then CallMethod with this as recv.
    ast.CallExpression(
      _,
      ast.MemberExpression(_, ast.SuperExpression(_), key, computed),
      args,
    ) -> {
      use e <- result.try(emit_super_method_ref(e, key, computed))
      emit_call_args(
        e,
        args,
        fn(n) { IrFinal(opcode.CallMethod(n)) },
        IrFinal(opcode.CallMethodApply),
      )
    }

    // Method call: obj.method(args) — emits GetField2 + CallMethod for this binding.
    // Spread path: build args array after GetField2, then IrCallMethodApply.
    ast.CallExpression(
      _,
      ast.MemberExpression(_, obj, ast.Identifier(name: method_name, ..), False),
      args,
    ) ->
      case ast_util.chain_has_optional(obj) {
        // `a?.b.m(x)` — the receiver chain short-circuits the call too.
        True -> emit_chain_root(e, expr)
        False -> {
          use e <- result.try(emit_expr(e, obj))
          let e = emit_get_field2(e, method_name)
          emit_call_args(
            e,
            args,
            fn(n) { IrFinal(opcode.CallMethod(n)) },
            IrFinal(opcode.CallMethodApply),
          )
        }
      }
    // Computed method call: obj[key](args) — must bind `this` to obj.
    // GetElem2 leaves [method, key, receiver]; we shuffle to [method, receiver]
    // via Swap+Pop so CallMethod sees the same shape as the dot-access path.
    ast.CallExpression(_, ast.MemberExpression(_, obj, key, True), args) ->
      case ast_util.chain_has_optional(obj) {
        // `a?.b[k](x)` — the receiver chain short-circuits the call too.
        True -> emit_chain_root(e, expr)
        False -> {
          use e <- result.try(emit_expr(e, obj))
          use e <- result.try(emit_expr(e, key))
          let e = emit_op(e, opcode.GetElem2)
          // [method, key, receiver] → Swap → [key, method, receiver] → Pop → [method, receiver]
          let e = emit_op(e, opcode.Swap)
          let e = emit_op(e, opcode.Pop)
          emit_call_args(
            e,
            args,
            fn(n) { IrFinal(opcode.CallMethod(n)) },
            IrFinal(opcode.CallMethodApply),
          )
        }
      }
    // Direct eval candidate: `eval(args)` with identifier callee.
    // Emits IrCallEval so the VM can do a runtime identity check against
    // the intrinsic eval. If it matches → direct eval (sees caller's locals).
    // If not (eval was shadowed/rebound) → regular call semantics.
    // Spread in eval(...args) is legal but rare; we fall through to regular
    // CallApply which gives indirect-eval semantics (acceptable for v1).
    ast.CallExpression(_, ast.Identifier(name: "eval", ..), args) ->
      case ast_util.has_spread_arg(args) {
        False -> {
          let e = emit_var_get(e, "eval")
          use e <- result.map(list.try_fold(args, e, emit_expr))
          // The parser already marked this scope's `contains_direct_eval`
          // on the ScopeBuilder — no emitter-side flag to maintain.
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
          let e = emit_var_get(e, "eval")
          use e <- result.map(emit_args_array_with_spread(e, args))
          emit_op(e, opcode.CallApply)
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
    ast.CallExpression(_, callee, args) -> {
      // `a?.()(x)` etc. — optional link inside the callee chain
      // short-circuits this call too.
      use <- bool.lazy_guard(ast_util.chain_has_optional(callee), fn() {
        emit_chain_root(e, expr)
      })
      case ast_util.unwrap_parens(callee), e.with_stack {
        ast.Identifier(name:, ..), [_, ..] -> {
          let e = emit_var_get_this(e, name)
          emit_call_args(
            e,
            args,
            fn(n) { IrFinal(opcode.CallMethod(n)) },
            IrFinal(opcode.CallMethodApply),
          )
        }
        _, _ -> {
          use e <- result.try(emit_expr(e, callee))
          emit_call_args(
            e,
            args,
            fn(n) { IrFinal(opcode.Call(n)) },
            IrFinal(opcode.CallApply),
          )
        }
      }
    }

    // Conditional (ternary)
    ast.ConditionalExpression(_, condition, consequent, alternate) -> {
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
    ast.SequenceExpression(_, exprs) -> emit_sequence(e, exprs)

    // Object literal
    ast.ObjectExpression(_, properties) -> {
      let e = emit_op(e, opcode.NewObject)
      list.try_fold(properties, e, emit_object_property)
    }

    // §13.3.7.3 super.prop / super[k] — read via [[HomeObject]].[[Prototype]]
    // with receiver = lexical this. Must precede the generic MemberExpression
    // arm.
    ast.MemberExpression(_, ast.SuperExpression(_), key, computed) -> {
      let e = emit_super_base(e)
      use e <- result.map(emit_super_key(e, key, computed))
      emit_op(e, opcode.GetSuperValue)
    }

    // Member expression (dot access). A `?.` link anywhere in the object
    // chain routes the WHOLE expression through the shared-short-circuit
    // chain compiler (§13.3.9.1).
    ast.MemberExpression(_, object, ast.Identifier(name: prop, ..), False) ->
      case ast_util.chain_has_optional(object) {
        True -> emit_chain_root(e, expr)
        False -> {
          use e <- result.map(emit_expr(e, object))
          emit_get_field(e, prop)
        }
      }

    // Computed member expression (obj[key])
    ast.MemberExpression(_, object, property, True) ->
      case ast_util.chain_has_optional(object) {
        True -> emit_chain_root(e, expr)
        False -> {
          use e <- result.try(emit_expr(e, object))
          use e <- result.map(emit_expr(e, property))
          emit_op(e, opcode.GetElem)
        }
      }
    // A non-computed member (`o.x`) whose property is not an Identifier: the
    // parser only ever puts an Identifier there, so this is an engine bug.
    ast.MemberExpression(..) -> Error(NonIdentifierStaticMember)

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
    ast.ArrayExpression(_, elements) ->
      case ast_util.has_spread_element(elements) {
        False -> emit_array_no_spread(e, elements)
        True -> emit_array_with_spread(e, elements)
      }

    // Function expression
    ast.FunctionExpression(_, name, params, body, is_gen, is_async) ->
      emit_function_closure(
        e,
        ast.binding_name(name),
        params,
        body,
        is_gen,
        is_async,
        True,
      )

    // Arrow function expression
    ast.ArrowFunctionExpression(_, params, body, is_async) ->
      emit_arrow_closure(e, None, params, body, is_async)

    // `this` — non-arrows own the slot; arrows resolve it as a capture from
    // the nearest enclosing non-arrow.
    ast.ThisExpression(_) -> Ok(get_this(e))

    // §13.3.12 new.target — reads the lexical [[NewTarget]] slot.
    ast.MetaProperty(_, ast.NewTarget) ->
      Ok(get_lexical(e, opcode.RefNewTarget))

    // §13.3.13 import.meta — the per-module meta object is not plumbed
    // through the runtime yet, so this is a typed unsupported-feature error
    // rather than a silent miscompile.
    ast.MetaProperty(_, ast.ImportMeta) ->
      Error(UnsupportedFeature("import.meta"))

    // New expression: new Foo(args). CallConstructor's stack contract is
    // [args, new_target, ctor] — for plain `new`, newTarget == ctor, so Dup.
    ast.NewExpression(_, callee, args) -> {
      use e <- result.try(emit_expr(e, callee))
      let e = emit_op(e, opcode.Dup)
      emit_call_args(
        e,
        args,
        fn(n) { IrFinal(opcode.CallConstructor(n)) },
        IrFinal(opcode.CallConstructorApply),
      )
    }

    // Template literal: `text ${expr} more`
    // Desugar to string concatenation: "" + "text " + expr + " more"
    ast.TemplateLiteral(_, parts) -> emit_template_literal(e, parts)

    // Class expression
    ast.ClassExpression(_, name, super_class, body) -> {
      let name = ast.binding_name(name)
      compile_class(e, name, name, super_class, body)
    }

    // Yield expression (inside generator functions)
    ast.YieldExpression(_, argument, is_delegate) -> {
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
            True -> Ok(emit_op(emit_ir(e, IrFinal(opcode.Await)), opcode.Yield))
            False -> Ok(emit_op(e, opcode.Yield))
          }
        True ->
          case e.is_async {
            True -> {
              // Async-gen yield* — GetIterator(expr, async) wraps sync
              // iterables via CreateAsyncFromSyncIterator. Seed undefined arg,
              // self-loop: Next calls iter.next(arg), Await settles result,
              // Resume checks done / yields and jumps back to Next via label.
              // Leaves final result.value on stack.
              let e = emit_op(e, opcode.GetAsyncIterator)
              // Cache [[NextMethod]] once (GetIteratorFromMethod §7.4.4) so
              // the loop doesn't re-Get `next` per step.
              let e = emit_op(e, opcode.IteratorRecord)
              let e = push_const(e, JsUndefined)
              let #(e, next_label) = fresh_label(e)
              // `after_label` marks the instruction the delegation falls out
              // to. The async-gen driver resumes there when a forwarded
              // .throw() finishes the inner iterator, so the sequence's shape
              // is free to change.
              let #(e, after_label) = fresh_label(e)
              let e = emit_ir(e, IrLabel(next_label))
              let e = emit_ir(e, IrAsyncYieldStarNext(after_label))
              let e = emit_op(e, opcode.Await)
              let e = emit_ir(e, IrAsyncYieldStarResume(next_label))
              Ok(emit_ir(e, IrLabel(after_label)))
            }
            False -> {
              // Sync yield* — get iterator, seed undefined, self-looping
              // YieldStar handles the rest. Leaves final result.value on stack.
              let e = emit_op(e, opcode.GetIterator)
              let e = push_const(e, JsUndefined)
              Ok(emit_op(e, opcode.YieldStar))
            }
          }
      }
    }

    ast.AwaitExpression(_, argument) -> {
      use e <- result.map(emit_expr(e, argument))
      emit_op(e, opcode.Await)
    }

    // Parenthesized expression — transparent for evaluation, just unwrap
    ast.ParenthesizedExpression(_, inner) -> emit_expr(e, inner)

    // RegExp literal — push pattern and flags, then NewRegExp opcode
    ast.RegExpLiteral(_, pattern, flags) -> {
      let e = push_const(e, JsString(pattern))
      let e = push_const(e, JsString(flags))
      Ok(emit_op(e, opcode.NewRegExp))
    }

    // §13.3.10 ImportCall: import(specifier) / import(specifier, options).
    // Push specifier, push options (undefined when absent), then the
    // DynamicImport opcode performs the EvaluateImportCall runtime steps.
    ast.ImportExpression(_, source, options, phase) -> {
      use e <- result.try(emit_expr(e, source))
      case phase {
        ast.PhaseEvaluation -> {
          use e <- result.map(case options {
            Some(opts) -> emit_expr(e, opts)
            None -> Ok(push_const(e, JsUndefined))
          })
          emit_op(e, opcode.DynamicImport)
        }
        // Phase forms (import.source/import.defer) take a single
        // AssignmentExpression — no options argument to push.
        ast.PhaseSource -> Ok(emit_op(e, opcode.DynamicImportSource))
        ast.PhaseDefer -> Ok(emit_op(e, opcode.DynamicImportDefer))
      }
    }

    // Tagged template (§13.3.11): lower to a call of the tag function with
    // the per-site template object as the first argument, reusing the
    // regular CallExpression paths so this-binding works (obj.tag`x` calls
    // tag with this = obj, §13.3.6.2 EvaluateCall).
    ast.TaggedTemplateExpression(tag:, parts:, span:) -> {
      let site = unique_positive_integer()
      let template =
        ast.IntrinsicTemplateObject(
          site:,
          quasis: ast.template_quasis(parts),
          span:,
        )
      // §13.3.11.1 step 1 note: a tagged template is never a direct eval —
      // wrap a bare `eval` tag in parens to dodge the IrCallEval path.
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
      // Mirror the parser-side quasis into the VM's opcode-payload type —
      // the VM layer does not depend on the parser AST.
      let quasis =
        list.map(quasis, fn(q) {
          opcode.TemplateQuasi(cooked: q.cooked, raw: q.raw)
        })
      Ok(emit_op(e, opcode.GetTemplateObject(site, quasis)))
    }

    // The two shapes the parser only ever produces INSIDE a construct that
    // has its own emit path (`super.x` / `super()`; `f(...a)` / `[...a]`), so
    // reaching them standalone is an engine bug. Named rather than swept up
    // by a `_ ->` arm: adding an Expression variant must be a compile error
    // here, not a runtime "unsupported: <variant name>".
    ast.SuperExpression(_) -> Error(BareSuperExpression)
    ast.SpreadElement(_, _) -> Error(BareSpreadElement)
  }
}

fn emit_template_literal(
  e: Emitter,
  parts: ast.TemplateParts(String),
) -> Result(Emitter, EmitError) {
  // `a${x}b${y}c` is TemplateParts(head: "a", tail: [#(x, "b"), #(y, "c")]).
  // Desugar to: "a" + x + "b" + y + "c". The alternation is total by
  // construction — no empty-quasis or trailing-expression case exists.
  let e = push_const(e, JsString(parts.head))
  list.try_fold(parts.tail, e, fn(e, part) {
    let #(expr, quasi) = part
    // Emit expression, ToString it (§13.2.8.5 — string hint, NOT the Add
    // operator's default-hint ToPrimitive), then concat with accumulator.
    use e <- result.map(emit_expr(e, expr))
    let e = emit_op(e, opcode.ToStringVal)
    let e = emit_ir(e, IrBinOp(opcode.Add))
    // Emit the following quasi string, concat.
    let e = push_const(e, JsString(quasi))
    emit_ir(e, IrBinOp(opcode.Add))
  })
}

fn emit_switch(
  e: Emitter,
  discriminant: ast.Expression,
  cases: List(ast.SwitchCase),
) -> Result(Emitter, EmitError) {
  let #(e, end_label) = fresh_label(e)

  // Push break context for switch (break; exits the switch). Switch is not a
  // continue target — emit_goto_loop walks past it to the enclosing loop.
  let e = push_switch(e, end_label)

  // Emit discriminant — stays on stack through comparison phase.
  // Evaluated OUTSIDE the CaseBlock scope (§14.12.4 step 1).
  use e <- result.try(emit_expr(e, discriminant))

  // The CaseBlock is a single block scope (§14.12.4 step 3-5): case tests and
  // bodies run inside it, with its let/const/class + function declarations
  // instantiated up front (BlockDeclarationInstantiation). The parser put
  // its child scopes into the SAME hoist order (scope.
  // sb_reorder_switch_children) so the positional cursors below line up.
  let case_stmts = ast_util.switch_case_stmts(cases)
  let #(e, save) = enter_scope(e, in_block: True)
  use e <- result.try(emit_block_declarations(e, case_stmts))

  // Allocate labels: each case gets a "body" label; a case *with a test* also
  // gets a "found" trampoline label, which pops the discriminant then jumps to
  // the body label. This ensures the discriminant is off the stack for all body
  // code, allowing fall-through between case bodies to work correctly. One fold
  // pairs each case's statements with its labels, so the three emission phases
  // below are single O(c) folds that just match on CaseLabels.
  let #(e, labelled_rev) =
    list.fold(cases, #(e, []), fn(acc, c) {
      let #(e, out) = acc
      let #(e, body) = fresh_label(e)
      case c {
        ast.SwitchCase(Some(test_expr), consequent) -> {
          let #(e, found) = fresh_label(e)
          #(e, [#(TestCase(test_expr:, body:, found:), consequent), ..out])
        }
        ast.SwitchCase(None, consequent) -> #(e, [
          #(TestlessCase(body:), consequent),
          ..out
        ])
      }
    })
  let labelled_cases = list.reverse(labelled_rev)

  // Phase 1: Emit comparison jumps
  // For each case with a test: Dup discriminant, emit test, StrictEq, JumpIfTrue(found_N)
  use #(e, default_body_label) <- result.try(
    list.try_fold(labelled_cases, #(e, option.None), fn(acc, entry) {
      let #(e, default_lbl) = acc
      let #(labels, _consequent) = entry
      case labels {
        TestCase(test_expr:, found:, ..) -> {
          let e = emit_op(e, opcode.Dup)
          use e <- result.map(emit_expr(e, test_expr))
          let e = emit_ir(e, IrBinOp(opcode.StrictEq))
          #(emit_ir(e, IrJumpIfTrue(found)), default_lbl)
        }
        // Default case — record its body label
        TestlessCase(body:) -> Ok(#(e, Some(body)))
      }
    }),
  )

  // No match: pop discriminant and jump to default body or end
  let e = emit_op(e, opcode.Pop)
  let e = emit_ir(e, IrJump(option.unwrap(default_body_label, end_label)))

  // Phase 2: Emit trampolines — each pops discriminant and jumps to body
  let e =
    list.fold(labelled_cases, e, fn(e, entry) {
      case entry.0 {
        TestCase(body:, found:, ..) ->
          e
          |> emit_ir(IrLabel(found))
          |> emit_op(opcode.Pop)
          |> emit_ir(IrJump(body))
        TestlessCase(..) -> e
      }
    })

  // Phase 3: Emit case bodies (fall-through between them)
  use e <- result.try(
    list.try_fold(labelled_cases, e, fn(e, entry) {
      let #(labels, consequent) = entry
      emit_stmts(emit_ir(e, IrLabel(case_body_label(labels))), consequent)
    }),
  )

  let e = emit_ir(e, IrLabel(end_label))
  let e = leave_scope(e, save)
  let e = pop_frame(e)
  Ok(e)
}

/// Labels allocated for one `switch` case. A case with a test always has a
/// `found` trampoline label; a `default:` case never does — the pairing can't
/// come apart.
type CaseLabels {
  TestCase(test_expr: ast.Expression, body: Int, found: Int)
  TestlessCase(body: Int)
}

fn case_body_label(labels: CaseLabels) -> Int {
  case labels {
    TestCase(body:, ..) -> body
    TestlessCase(body:) -> body
  }
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
      let e = emit_op(e, opcode.Pop)
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
    ast.ParenthesizedExpression(_, inner) -> emit_named_expr(e, inner, name)
    // Anonymous function expression → bake name (no self-name binding —
    // §8.4 NamedEvaluation does not create one)
    ast.FunctionExpression(_, None, params, body, is_gen, is_async) ->
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
    ast.ArrowFunctionExpression(_, params, body, is_async) ->
      emit_arrow_closure(e, Some(name), params, body, is_async)
    // Anonymous class expression → bake `.name` only; NO inner binding
    // (§8.4 NamedEvaluation step 2 — classBinding is undefined).
    ast.ClassExpression(_, None, super_class, body) ->
      compile_class(e, None, Some(name), super_class, body)
    // Not anonymous → emit normally (named fn keeps its own name)
    _ -> emit_expr(e, expr)
  }
}

/// Register a compiled child function on the emitter and emit IrMakeClosure
/// for its index — the shared tail of every closure-producing helper below.
fn register_closure(
  compiled: Result(#(Emitter, CompiledChild), EmitError),
) -> Result(Emitter, EmitError) {
  use #(e, child) <- result.map(compiled)
  let #(e, idx) = add_child_function(e, child)
  emit_op(e, opcode.MakeClosure(idx))
}

/// Compile a method / getter / setter body into a child function, register
/// it, and emit IrMakeClosure for it. Never a constructor (methods/accessors
/// have no [[Construct]], so `new o.m()` must throw); always MethodCode.
fn make_method_closure(
  e: Emitter,
  name: Option(String),
  params: List(ast.Pattern),
  body: List(ast.StmtWithLine),
  is_gen: Bool,
  is_async: Bool,
) -> Result(Emitter, EmitError) {
  compile_function_body(
    e,
    name,
    None,
    params,
    StmtsBody(body),
    is_arrow: False,
    is_generator: is_gen,
    is_async:,
    is_constructor: False,
    code_kind: opcode.MethodCode,
    field_init: NoFieldInit,
  )
  |> register_closure
}

/// Compile a function expression body into a child function and emit the
/// closure. A constructor unless generator/async.
fn emit_function_closure(
  e: Emitter,
  name: Option(String),
  params: List(ast.Pattern),
  body: List(ast.StmtWithLine),
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
  compile_function_body(
    e,
    name,
    self_name,
    params,
    StmtsBody(body),
    is_arrow: False,
    is_generator: is_gen,
    is_async:,
    is_constructor: !is_gen && !is_async,
    code_kind: opcode.FunctionCode,
    field_init: NoFieldInit,
  )
  |> register_closure
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
  let body_stmts = case body {
    ast.ArrowBodyExpression(expr) -> [
      ast.StmtWithLine(0, ast.ReturnStatement(Some(expr))),
    ]
    ast.ArrowBodyBlock(stmts) -> stmts
  }
  compile_function_body(
    e,
    name,
    None,
    params,
    StmtsBody(body_stmts),
    is_arrow: True,
    is_generator: False,
    is_async:,
    is_constructor: False,
    code_kind: opcode.FunctionCode,
    field_init: NoFieldInit,
  )
  |> register_closure
}

/// Compile an object-literal *method* value — a concise method, getter, or
/// setter — so it is NOT a constructor (methods/accessors have no
/// [[Construct]], so `new o.m()` must throw).
fn emit_method_value(
  e: Emitter,
  value: ast.FunctionLiteral,
  name: Option(String),
) -> Result(Emitter, EmitError) {
  let ast.FunctionLiteral(_, params, body, is_gen, is_async) = value
  make_method_closure(e, name, params, body, is_gen, is_async)
}

/// Emit one property in an object literal. Object is already on the stack.
/// All handlers leave the object on the stack for the next property.
fn emit_object_property(
  e: Emitter,
  prop: ast.Property,
) -> Result(Emitter, EmitError) {
  case prop {
    // Annex B §B.3.1 — `{__proto__: v}` / `{"__proto__": v}` sets [[Prototype]]
    // instead of defining an own property. Only when non-computed, non-shorthand.
    // Shorthand `{__proto__}`, methods `{__proto__(){}}` and computed
    // `{["__proto__"]: v}` fall through to ordinary DefineField.
    ast.InitProperty(
      key: ast.KeyIdentifier(name: "__proto__", ..),
      value:,
      shorthand: False,
    )
    | ast.InitProperty(
        key: ast.KeyString(value: "__proto__", ..),
        value:,
        shorthand: False,
      ) -> {
      use e <- result.map(emit_expr(e, value))
      emit_op(e, opcode.SetProto)
    }

    // Static key: {name: value} / {"name": value} → IrDefineField(name), which
    // pops the value and keeps obj. `{ x: function(){} }` stays a plain
    // (constructible) function value with no home object.
    ast.InitProperty(key: ast.KeyIdentifier(name:, ..), value:, ..)
    | ast.InitProperty(key: ast.KeyString(value: name, ..), value:, ..) -> {
      use e <- result.map(emit_named_expr(e, value, name))
      emit_ir(e, IrDefineField(name))
    }

    // Every remaining key shape: computed (`{[expr]: v}`) or a numeric/bigint
    // literal (`{1: v}`, `{1n: v}`), which needs a runtime ToPropertyKey for
    // its canonical string form ("1", not "1.0"). Emit key, emit value,
    // IrDefineFieldComputed — pops both, keeps obj (Symbol keys preserved).
    ast.InitProperty(key:, value:, ..) ->
      emit_computed_init_property(e, emit_property_key(_, key), value)

    // Static-key concise method `{ m() {} }`: not constructible (`new o.m()`
    // throws) and records its [[HomeObject]] (§15.4.4) so `super.x` works.
    ast.MethodProperty(key: ast.KeyIdentifier(name:, ..), value:)
    | ast.MethodProperty(key: ast.KeyString(value: name, ..), value:) -> {
      use e <- result.map(emit_method_value(e, value, Some(name)))
      let e = emit_op(e, opcode.MakeMethod)
      emit_ir(e, IrDefineField(name))
    }

    // Computed / numeric-key method `{ [expr](){} }`, `{ 1(){} }`.
    ast.MethodProperty(key:, value:) ->
      emit_computed_method_property(e, emit_property_key(_, key), value)

    // Accessor with static key: { get name() {} } / { set name(v) {} }
    // Emit the function, then DefineAccessor(name, Getter/Setter).
    ast.AccessorProperty(key: ast.KeyIdentifier(name:, ..), value:, kind:)
    | ast.AccessorProperty(key: ast.KeyString(value: name, ..), value:, kind:) -> {
      let #(prefix, accessor) = property_accessor(kind)
      use e <- result.map(emit_method_value(e, value, Some(prefix <> name)))
      emit_ir(e, IrDefineAccessor(name, accessor, True))
    }

    // Computed / numeric-key accessor: { get [expr]() {} }, { set 1(v) {} }
    // Stack: emit key, emit fn → DefineAccessorComputed
    ast.AccessorProperty(key:, value:, kind:) -> {
      let #(_, accessor) = property_accessor(kind)
      use e <- result.try(emit_property_key(e, key))
      use e <- result.map(emit_method_value(e, value, None))
      emit_op(e, opcode.DefineAccessorComputed(accessor, True))
    }

    // Spread: {...source}
    // IrObjectSpread pops source, copies own enumerable props, keeps obj.
    // null/undefined sources are no-ops per CopyDataProperties spec.
    ast.SpreadProperty(argument:) -> {
      use e <- result.map(emit_expr(e, argument))
      emit_op(e, opcode.ObjectSpread)
    }
  }
}

/// Push a property key onto the stack, ready for a `*Computed` define/get op.
/// Literal keys become constants; a computed key evaluates its expression. The
/// caller-visible ToPropertyKey coercion is the VM's job (Symbol keys survive,
/// everything else stringifies), so this never coerces here.
///
/// `KeyPrivate` is unreachable for object literals and object patterns (the
/// parser rejects `{ #x: 1 }`); pushing the `#x` string keeps this total.
fn emit_property_key(
  e: Emitter,
  key: ast.PropertyKey,
) -> Result(Emitter, EmitError) {
  case key {
    ast.KeyIdentifier(name:, ..) | ast.KeyPrivate(name:, ..) ->
      Ok(push_const(e, JsString(name)))
    ast.KeyString(value: s, ..) -> Ok(push_const(e, JsString(s)))
    ast.KeyNumber(value: n, ..) ->
      Ok(push_const(e, JsNumber(literal_number(n))))
    ast.KeyBigInt(value: i, ..) ->
      Ok(push_const(e, value.JsBigInt(value.BigInt(i))))
    ast.KeyComputed(expression:) -> emit_expr(e, expression)
  }
}

/// Map an accessor kind to its inferred-name prefix and opcode kind.
fn property_accessor(kind: ast.AccessorKind) -> #(String, opcode.AccessorKind) {
  case kind {
    ast.GetAccessor -> #("get ", opcode.Getter)
    ast.SetAccessor -> #("set ", opcode.Setter)
  }
}

/// Shared emit for data properties that go through DefineFieldComputed
/// (computed key, numeric key, or exotic-key fallthrough).
fn emit_computed_init_property(
  e: Emitter,
  emit_key: fn(Emitter) -> Result(Emitter, EmitError),
  value: ast.Expression,
) -> Result(Emitter, EmitError) {
  use e <- result.try(emit_key(e))
  use e <- result.map(emit_expr(e, value))
  emit_op(e, opcode.DefineFieldComputed)
}

/// Same, for a method value (`{[k](){}}`): the closure is emitted first so
/// MakeMethod sees [obj, fn] directly, then the key, then Swap to restore
/// [obj, key, fn] for DefineFieldComputed. Closure creation is unobservable so
/// this doesn't change §13.2.5.5 evaluation order — key side-effects still run
/// before any property is defined.
fn emit_computed_method_property(
  e: Emitter,
  emit_key: fn(Emitter) -> Result(Emitter, EmitError),
  value: ast.FunctionLiteral,
) -> Result(Emitter, EmitError) {
  use e <- result.try(emit_method_value(e, value, None))
  let e = emit_op(e, opcode.MakeMethod)
  use e <- result.map(emit_key(e))
  emit_op(emit_ir(e, IrFinal(opcode.Swap)), opcode.DefineFieldComputed)
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
    [] -> emit_op(e, opcode.ArrayFrom(count))
    _ -> emit_op(e, opcode.ArrayFromWithHoles(count, list.reverse(holes_rev)))
  }
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
  let #(prefix, tail) =
    list.split_while(elements, fn(el) {
      case el {
        Some(ast.SpreadElement(_, _)) -> False
        _ -> True
      }
    })

  // Pack the prefix (handles holes via IrArrayFromWithHoles if needed).
  use e <- result.try(emit_array_no_spread(e, prefix))

  // Incrementally append the tail.
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
      None ->
        // Hole after a spread — increment length without setting element.
        Ok(emit_op(e, opcode.ArrayPushHole))
    }
  })
}

/// Build an args array for a spread-call (f(a, ...b, c) etc).
/// Call args have no holes (the parser doesn't produce them in arglists),
/// so wrap each arg in Some and delegate to emit_array_with_spread.
/// Leaves the args array on top of the stack; caller follows with an
/// IrCallApply / IrCallMethodApply / IrCallConstructorApply.
fn emit_args_array_with_spread(
  e: Emitter,
  args: List(ast.Expression),
) -> Result(Emitter, EmitError) {
  emit_array_with_spread(e, list.map(args, Some))
}

/// Emit call args + the dispatch op. When no arg is a spread, push each
/// arg and emit `fixed(arity)`; otherwise build an args-array and emit
/// `apply`. Used by every Call/CallMethod/CallConstructor shape.
fn emit_call_args(
  e: Emitter,
  args: List(ast.Expression),
  fixed: fn(Int) -> IrOp,
  apply: IrOp,
) -> Result(Emitter, EmitError) {
  case ast_util.has_spread_arg(args) {
    False -> {
      use e <- result.map(list.try_fold(args, e, emit_expr))
      emit_ir(e, fixed(list.length(args)))
    }
    True -> {
      use e <- result.map(emit_args_array_with_spread(e, args))
      emit_ir(e, apply)
    }
  }
}

// ============================================================================
// For-in / for-of loops
// ============================================================================

/// Bound names of a lexical (`let`/`const`/`using`/`await using`) for-in/of
/// head declaration. Empty for `var` heads and bare assignment-target heads
/// — the two cases with no per-iteration environment.
fn for_head_lex_names(left: ast.ForInit) -> List(String) {
  case left {
    ast.ForInitDeclaration(kind, declarators) ->
      case kind {
        ast.Let | ast.Const | ast.Using | ast.AwaitUsing ->
          list.flat_map(declarators, fn(d) {
            let ast.VariableDeclarator(pattern, _) = d
            ast.pattern_bound_names(pattern)
          })
        ast.Var -> []
      }
    ast.ForInitPattern(_) | ast.ForInitExpression(_) -> []
  }
}

/// §14.7.5.7 ForIn/OfBodyEvaluation steps 6.f-g: a lexical head gets a
/// FRESH iteration environment on every pass (NewDeclarativeEnvironment +
/// ForDeclarationBindingInstantiation), so closures created during
/// iteration k keep iteration k's value forever — including closures made
/// in the head's RHS, which capture the head-TDZ binding and must observe
/// it as uninitialized even after the loop ran (§14.7.5.5 step 5 restores
/// oldEnv; the head env is never initialized).
///
/// In the slot+box model that is exactly the head scope's binding prologue
/// re-run at the top of the iteration, immediately before this iteration's
/// BindingInitialization: each binding is re-seeded to TDZ and — when
/// captured — re-boxed into a brand-new box, orphaning the previous
/// iteration's box for whoever captured it. Emitted only when the head
/// declares at least one lexical name: that guarantees the parser pushed a
/// head scope and the analyzer kept it (binding-less scopes are pruned and
/// not entered), so `e.current_scope` IS the head scope here.
fn emit_for_per_iteration_env(e: Emitter, left: ast.ForInit) -> Emitter {
  case for_head_lex_names(left) {
    [] -> e
    _ -> emit_binding_prologue(e, e.current_scope)
  }
}

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

  // Per-iteration scope — only consume one when the parser pushed it
  // (let/const/using heads); var/expr heads have no head scope. Match on
  // the declaration KIND, not the bound-name list: `for (let {} of …)`
  // binds zero names but the parser still pushed a head scope for it.
  let has_lex = ast_util.for_classic_init_is_lex(Some(left))
  let #(e, save) = enter_for_scope(e, has_lex)

  // Evaluate the right-hand side (object to iterate) — bound names of a
  // let/const ForDeclaration are in TDZ during this evaluation (§14.7.5.5).
  use e <- result.try(emit_for_head_expr(e, right))
  // ForInStart: pops object, pushes iterator ref
  let e = emit_op(e, opcode.ForInStart)

  let e = push_loop(e, loop_end, loop_continue)
  let e = emit_ir(e, IrLabel(loop_start))

  // ForInNext: peeks iterator, pushes key + done
  let e = emit_op(e, opcode.ForInNext)
  // If done, jump to cleanup (where we pop the unused key)
  let e = emit_ir(e, IrJumpIfTrue(cleanup))

  // Fresh per-iteration bindings for a lexical head (§14.7.5.7 step 6.g),
  // then bind the key to the left-hand side variable.
  let e = emit_for_per_iteration_env(e, left)
  use e <- result.try(emit_for_lhs_bind(e, left))

  // Body
  use e <- result.try(emit_stmt(e, body))

  // Continue point
  let e = emit_ir(e, IrLabel(loop_continue))
  let e = emit_ir(e, IrJump(loop_start))

  // cleanup: pop the key (done=true left it on stack)
  let e = emit_ir(e, IrLabel(cleanup))
  let e = emit_op(e, opcode.Pop)

  // loop_end: pop the iterator
  let e = emit_ir(e, IrLabel(loop_end))
  let e = emit_op(e, opcode.Pop)

  let e = pop_frame(e)
  Ok(leave_for_scope(e, save))
}

/// §14.7.5.5 ForIn/OfHeadEvaluation steps 2-5: the AssignmentExpression is
/// evaluated inside the for-head Block scope already entered by the caller
/// via `enter_for_scope` — that scope IS the §14.7.5.5 TDZ env. Its
/// `emit_binding_prologue` has already seeded the head's let/const names
/// with JsUninitialized, so `for (let x of [x])` throws ReferenceError
/// without a separate inner scope here. The parser does NOT push a second
/// head-TDZ scope, so emit must not consume one. KNOWN RESIDUAL: the spec's
/// step-5 "restore oldEnv" (closures in the head capture a frozen env
/// forever — scope-head-lex-{open,close}.js) is not modeled.
fn emit_for_head_expr(
  e: Emitter,
  right: ast.Expression,
) -> Result(Emitter, EmitError) {
  emit_expr(e, right)
}

/// Per-iteration body section shared by for-of and for-await-of: bind the
/// stacked iteration value to the LHS, emit the body under Annex B
/// head-name blocking, then the continue label and back-edge. A
/// `for ([await] using x of …)` head routes through emit_for_of_using_body
/// so the per-iteration const is registered as a disposable at body entry
/// and disposed at body exit — that disposal try sits INSIDE the caller's
/// F_body so break/continue cross it before reaching break_target.
fn emit_for_of_iter_body(
  e: Emitter,
  left: ast.ForInit,
  body: ast.Statement,
  loop_continue: Int,
  loop_start: Int,
) -> Result(Emitter, EmitError) {
  // Fresh per-iteration bindings for a lexical head (§14.7.5.7 step 6.g).
  let e = emit_for_per_iteration_env(e, left)
  use e <- result.try(emit_for_lhs_bind(e, left))
  use e <- result.map(case for_of_using_hint(left) {
    Some(#(name, is_async)) -> emit_for_of_using_body(e, name, is_async, body)
    None -> emit_stmt(e, body)
  })
  e
  |> emit_ir(IrLabel(loop_continue))
  |> emit_ir(IrJump(loop_start))
}

/// Shared frame for `for-of` / `for-await-of`: open the per-iteration
/// scope, evaluate RHS, get the iterator via `get_iter`, push F_body and
/// the loop context, emit `loop_start`, run the variant-specific `tail`,
/// then emit `end`, pop the loop, close the scope. Stack after `get_iter`
/// is [iter, ..base]; F_body records depth len(base)+1 so unwind always
/// lands on [thrown, iter, ..base]. push_loop_iter must come AFTER F_body
/// so a labeled break/continue/return that *crosses* this loop drops
/// F_body and closes iter (cross_pop_try=1, has_iterator).
///
/// `body_kind` is F_body's `TryKind`: `IterCloseGuard` for the sync loop (a
/// return completion out of a suspended `yield` closes the iterator), but
/// `CatchOnly` for `for await`, whose close needs an Await the generator
/// unwinder cannot perform.
fn emit_for_of_common(
  e: Emitter,
  left: ast.ForInit,
  right: ast.Expression,
  get_iter: IrOp,
  body_kind: TryKind(LabelId),
  // tail receives: e, loop_start, loop_continue, break_target, catch_body, end
  tail: fn(Emitter, Int, Int, Int, Int, Int) -> Result(Emitter, EmitError),
) -> Result(Emitter, EmitError) {
  let #(e, loop_start) = fresh_label(e)
  let #(e, loop_continue) = fresh_label(e)
  let #(e, break_target) = fresh_label(e)
  let #(e, catch_body) = fresh_label(e)
  let #(e, end) = fresh_label(e)
  let has_lex = ast_util.for_classic_init_is_lex(Some(left))
  let #(e, save) = enter_for_scope(e, has_lex)
  use e <- result.try(emit_for_head_expr(e, right))
  let e =
    e
    |> emit_ir(get_iter)
    |> emit_ir(IrPushTry(catch_body, body_kind))
    |> push_loop_iter(break_target, loop_continue)
    |> emit_ir(IrLabel(loop_start))
  use e <- result.map(tail(
    e,
    loop_start,
    loop_continue,
    break_target,
    catch_body,
    end,
  ))
  e |> emit_ir(IrLabel(end)) |> pop_frame |> leave_for_scope(save)
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
fn emit_for_of(
  e: Emitter,
  left: ast.ForInit,
  right: ast.Expression,
  body: ast.Statement,
) -> Result(Emitter, EmitError) {
  use e, loop_start, loop_continue, break_target, catch_body, end <- emit_for_of_common(
    e,
    left,
    right,
    IrFinal(opcode.GetIterator),
    IterCloseGuard,
  )
  let #(e, exhausted) = fresh_label(e)
  let e = emit_op(e, opcode.IteratorNext)
  // stack: [done, value, iter|undef, ..base], try=[F_body]
  let e = emit_ir(e, IrJumpIfTrue(exhausted))

  // stack: [value, iter, ..base] — bind consumes the value → [iter, ..base]
  use e <- result.map(emit_for_of_iter_body(
    e,
    left,
    body,
    loop_continue,
    loop_start,
  ))

  // catch_body: bind/body threw (real iter) OR .next() threw (undef
  // sentinel). IteratorCloseThrow calls .return() iff iter is an object,
  // then rethrows the original — never falls through.
  let e = emit_ir(e, IrLabel(catch_body))
  let e = emit_op(e, opcode.IteratorCloseThrow)

  // exhausted: done=true, stack=[value, undef, ..base], try=[F_body, ..].
  // Spec does NOT close on natural exhaustion — slot is already undef so
  // a hypothetical close would no-op anyway, but we just drop and exit.
  let e = emit_ir(e, IrLabel(exhausted))
  let e = emit_op(e, opcode.Pop)
  let e = emit_op(e, opcode.PopTry)
  let e = emit_op(e, opcode.Pop)
  let e = emit_ir(e, IrJump(end))

  // break_target: stack=[iter, ..base], try=[F_body, ..].
  // Normal-completion close: propagates errors from .return() and
  // TypeErrors on non-object return values per §7.4.11.
  let e = emit_ir(e, IrLabel(break_target))
  let e = emit_op(e, opcode.PopTry)
  let e = emit_op(e, opcode.IteratorClose)
  emit_ir(e, IrJump(end))
}

/// Emit a for-await-of loop: `for await (lhs of rhs) body`
///
/// Shares the F_body frame, scope, and loop-context setup with `emit_for_of`
/// via `emit_for_of_common`. On top of that an inner F_next guards the
/// next/await/unwrap region so a `.next()` failure does NOT close (§14.7.5.6
/// step 6.a-f), and the close paths follow AsyncIteratorClose §7.4.12: the
/// `.return()` result is *awaited* before the object check (normal completion)
/// or before the original error is rethrown (throw completion). Because Await
/// suspends the frame, close cannot be a single opcode — it is open-coded as a
/// bytecode sequence here.
///
/// F_next records the same depth B+1 as F_body so unwind_to_catch always
/// lands on [thrown, iter, ..base].
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
  use e, loop_start, loop_continue, break_target, catch_body, end <- emit_for_of_common(
    e,
    left,
    right,
    IrFinal(opcode.GetAsyncIterator),
    // §7.4.12 AsyncIteratorClose Awaits the .return() result, which the
    // return-completion unwinder cannot do — so F_body is a plain catch here
    // and the close is open-coded in bytecode below.
    CatchOnly,
  )
  let #(e, exhausted) = fresh_label(e)
  let #(e, catch_next) = fresh_label(e)
  let #(e, no_ret_thr) = fresh_label(e)
  let #(e, rethrow) = fresh_label(e)
  let #(e, no_ret_brk) = fresh_label(e)

  // F_next shadows F_body for the next/await/unwrap region. Same recorded
  // depth (stack is [iter, ..base]). §14.7.5.6 step 6.a-f: errors here must
  // NOT close the iterator.
  let e = emit_ir(e, IrPushTry(catch_next, CatchOnly))
  let e = emit_op(e, opcode.Dup)
  let e = emit_ir(e, IrGetField2("next"))
  let e = emit_op(e, opcode.CallMethod(0))
  let e = emit_op(e, opcode.Await)
  // §14.7.5.6 step 6.c: awaited next() result must be an Object. Covered by
  // F_next so a non-object correctly does NOT trigger close.
  let e = emit_op(e, opcode.IteratorCheckObject)
  // [result_obj, iter, ..base]
  let e = emit_op(e, opcode.Dup)
  let e = emit_ir(e, IrGetField("done"))
  let e = emit_ir(e, IrJumpIfTrue(exhausted))
  let e = emit_ir(e, IrGetField("value"))
  let e = emit_op(e, opcode.PopTry)
  // [value, iter, ..base], try=[F_body, ..]
  use e <- result.map(emit_for_of_iter_body(
    e,
    left,
    body,
    loop_continue,
    loop_start,
  ))

  // catch_next: next()/Await(next)/.done/.value failed — do NOT close.
  // unwind: depth=B+1 → [thrown, iter, ..base], try=[F_body, ..].
  let e = emit_ir(e, IrLabel(catch_next))
  let e = emit_op(e, opcode.PopTry)
  let e = emit_op(e, opcode.Swap)
  let e = emit_op(e, opcode.Pop)
  let e = emit_op(e, opcode.Throw)

  // catch_body: bind/body threw — §7.4.12 throw-completion close.
  // [thrown, iter, ..base], try=[..outer]. F_swallow recorded at depth B+2
  // catches every error from get/call/await; original error always wins.
  let e = emit_ir(e, IrLabel(catch_body))
  let e = emit_ir(e, IrPushTry(rethrow, CatchOnly))
  let e = emit_op(e, opcode.Swap)
  // [iter, thrown, ..base]
  let e = emit_ir(e, IrGetField2("return"))
  let e = emit_op(e, opcode.Dup)
  let e = emit_ir(e, IrJumpIfNullish(no_ret_thr))
  // [ret_fn, iter, thrown, ..base]
  let e = emit_op(e, opcode.CallMethod(0))
  // [result, thrown, ..base] — pad so saved_stack after the Await pop has
  // length B+2 (== F_swallow depth). Reject-unwind then lands at
  // [inner_err, undef, thrown, ..base] = B+3, matching every other path.
  let e = push_const(e, JsUndefined)
  let e = emit_op(e, opcode.Swap)
  // [result, undef, thrown, ..base]
  let e = emit_op(e, opcode.Await)
  // fulfilled → [awaited, undef, thrown, ..base]
  let e = emit_op(e, opcode.PopTry)
  let e = emit_ir(e, IrJump(rethrow))

  let e = emit_ir(e, IrLabel(no_ret_thr))
  // [ret(nullish), iter, thrown, ..base], try=[F_swallow, ..]
  let e = emit_op(e, opcode.PopTry)
  // fall through

  // rethrow: F_swallow catch-target AND merge point. Stack is always
  // [_, _, thrown, ..base] (slot0 = inner_err|awaited|nullish-ret;
  // slot1 = iter|undef). Spec: original throw completion wins, no object
  // check on the throw path.
  let e = emit_ir(e, IrLabel(rethrow))
  let e = emit_op(e, opcode.Pop)
  let e = emit_op(e, opcode.Pop)
  let e = emit_op(e, opcode.Throw)

  // exhausted: done=true. [result_obj, iter, ..base], try=[F_next, F_body, ..].
  // Spec does NOT close on natural exhaustion.
  let e = emit_ir(e, IrLabel(exhausted))
  let e = emit_op(e, opcode.Pop)
  let e = emit_op(e, opcode.PopTry)
  let e = emit_op(e, opcode.PopTry)
  let e = emit_op(e, opcode.Pop)
  let e = emit_ir(e, IrJump(end))

  // break_target: [iter, ..base], try=[F_body, ..]. §7.4.12 normal-completion
  // close — every error (getter throw, non-callable, call throw, await reject,
  // non-object result) propagates to the enclosing handler.
  let e = emit_ir(e, IrLabel(break_target))
  let e = emit_op(e, opcode.PopTry)
  let e = emit_ir(e, IrGetField2("return"))
  let e = emit_op(e, opcode.Dup)
  let e = emit_ir(e, IrJumpIfNullish(no_ret_brk))
  let e = emit_op(e, opcode.CallMethod(0))
  let e = emit_op(e, opcode.Await)
  let e = emit_op(e, opcode.IteratorCheckObject)
  let e = emit_op(e, opcode.Pop)
  let e = emit_ir(e, IrJump(end))

  let e = emit_ir(e, IrLabel(no_ret_brk))
  // [ret(nullish), iter, ..base] — no await per §7.4.12 step 5.b.
  let e = emit_op(e, opcode.Pop)
  let e = emit_op(e, opcode.Pop)
  emit_ir(e, IrJump(end))
}

/// Bind the current value (on top of stack) to the for-in/for-of LHS.
/// The LHS can be:
///   - ForInitDeclaration(kind, declarators) e.g. `for (let x ...)`
///   - ForInitExpression(expr) e.g. `for (x ...)`, `for (obj.k ...)`,
///     `for ([a,b] ...)`, `for ({a} ...)` — destructuring-assign semantics
///   - ForInitPattern(pattern) e.g. catch-param-style binding pattern
/// Consumes the value on top of stack.
fn emit_for_lhs_bind(
  e: Emitter,
  left: ast.ForInit,
) -> Result(Emitter, EmitError) {
  case left {
    ast.ForInitDeclaration(kind, declarators) -> {
      let binding_kind = case kind {
        ast.Var -> VarBinding
        ast.Let -> LetBinding
        // Using/AwaitUsing heads bind as Const here; emit_for_of_using_body
        // registers the disposer separately after this bind.
        ast.Const | ast.Using | ast.AwaitUsing -> ConstBinding
      }
      case declarators {
        [ast.VariableDeclarator(pattern, _)] ->
          emit_destructuring_bind(e, pattern, binding_kind)
        // The grammar allows exactly one ForBinding in a for-in/of head; a
        // second declarator forces the parser down the classic `for(;;)`
        // path, so this arm is unreachable from parser output.
        _ -> Error(MultiDeclaratorForHead)
      }
    }
    ast.ForInitPattern(pattern) ->
      emit_destructuring_bind(e, pattern, VarBinding)
    // Assignment-target LHS: `for (x of …)`, `for (obj.k of …)`,
    // `for ([a,b] of …)`, `for ({a} of …)`. Destructuring-assign semantics
    // (PutVar/PutField/PutElem, no declaration).
    ast.ForInitExpression(expr) ->
      emit_destructuring_assign(e, ast_util.unwrap_parens(expr))
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
    ast.IdentifierPattern(name, ..) -> {
      let e = case binding_kind {
        LetBinding -> declare_lex(e, name, False)
        ConstBinding -> declare_lex(e, name, True)
        // Param/catch/var slots are pre-registered by the analyzer and
        // seeded by enter_scope's emit_binding_prologue. FnNameBinding
        // never reaches destructuring — it is declared directly in
        // compile_function_body, never via a pattern.
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
      use e, p, hr, n <- emit_object_pattern(e, properties, has_rest)
      emit_single_object_prop(e, p, binding_kind, hr, n)
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
      // Rest element outside array pattern — treat as identity bind
      emit_destructuring_bind(e, argument, binding_kind)
    }
  }
}

/// If TOS === undefined, replace it with the evaluated default; otherwise keep
/// it. Shared by AssignmentPattern (§8.6.3 binding-with-initializer), the
/// `target = default` arm of destructuring assignment (§13.15.5.3
/// AssignmentElement Initializer), and computed-key member-target defaults.
/// When `target_name` is Some, the default gets NamedEvaluation (anonymous
/// fn/class RHS named after the binding). Stack: [val] → [val_or_default].
fn emit_default_if_undefined(
  e: Emitter,
  default_expr: ast.Expression,
  target_name: Option(String),
) -> Result(Emitter, EmitError) {
  let #(e, has_val) = fresh_label(e)
  let e = emit_op(e, opcode.Dup)
  let e = push_const(e, JsUndefined)
  let e = emit_ir(e, IrBinOp(opcode.StrictEq))
  let e = emit_ir(e, IrJumpIfFalse(has_val))
  let e = emit_op(e, opcode.Pop)
  use e <- result.map(case target_name {
    Some(name) -> emit_named_expr(e, default_expr, name)
    None -> emit_expr(e, default_expr)
  })
  emit_ir(e, IrLabel(has_val))
}

/// Object-destructure scaffold shared by binding patterns (§8.6.2
/// ObjectBindingPattern, over ast.PatternProperty) and assignment patterns
/// (§13.15.5.2 ObjectAssignmentPattern, over ast.Property). Both walk a list
/// of properties under the same stack invariant —
///   [src, key_n, ..., key_1, ...]
/// where key_1..key_n are excluded-key JsValues stashed BENEATH src,
/// accumulated only when a rest property is present (so ObjectRestCopy can
/// filter them out). They differ only in `emit_prop`, which receives
/// (e, prop, has_rest, n_excl) and returns the new n_excl; the leaf store is
/// declare+init for binding vs PutVar/PutField/PutElem for assignment.
///
/// §8.6.2 / §13.15.5.2 step 1: RequireObjectCoercible(value) BEFORE any
/// property keys are evaluated — `({} = null)` and `function f({}){}; f(null)`
/// throw TypeError even on an empty pattern. ToObject throws on exactly
/// null/undefined; the wrapper it creates for other primitives is observably
/// equivalent for the property reads that follow.
fn emit_object_pattern(
  e: Emitter,
  properties: List(p),
  has_rest: Bool,
  emit_prop: fn(Emitter, p, Bool, Int) -> Result(#(Emitter, Int), EmitError),
) -> Result(Emitter, EmitError) {
  let e = emit_op(e, opcode.ToObject)
  use #(e, _n_excl) <- result.map(
    list.try_fold(properties, #(e, 0), fn(acc, prop) {
      let #(e, n) = acc
      emit_prop(e, prop, has_rest, n)
    }),
  )
  // The rest-property branch issues ObjectRestCopy which consumes src + all
  // stashed keys. Otherwise src is still on top — drop it.
  case has_rest {
    True -> e
    False -> emit_op(e, opcode.Pop)
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
    ast.PatternProperty(key: ast.KeyIdentifier(name:, ..), value:, ..)
    | ast.PatternProperty(key: ast.KeyString(value: name, ..), value:, ..) -> {
      // [src,..] → Dup → [src,src,..] → GetField → [val,src,..] → bind → [src,..]
      let e = emit_op(e, opcode.Dup)
      let e = emit_ir(e, IrGetField(name))
      use e <- result.map(emit_destructuring_bind(e, value, binding_kind))
      // Stash key string beneath src for later exclusion.
      case has_rest {
        False -> #(e, n_excl)
        True -> {
          let e = push_const(e, JsString(name))
          #(emit_op(e, opcode.Swap), n_excl + 1)
        }
      }
    }

    // Computed key `{[expr]: pat}` and the numeric/bigint literal keys
    // (`{1: pat}`, `{1n: pat}`), which need a runtime ToPropertyKey for the
    // canonical "1" — push the key and route via GetElem.
    ast.PatternProperty(key:, value:, ..) ->
      emit_computed_key_prop(
        e,
        emit_property_key(_, key),
        value,
        binding_kind,
        has_rest,
        n_excl,
      )

    // {a, b, ...rest} — §13.15.5.3 RestBindingInitialization.
    // Stack: [src, key_n,..,key_1] → ObjectRestCopy(n) → [rest_obj] → bind.
    ast.RestProperty(name:, span:) -> {
      let e = emit_op(e, opcode.ObjectRestCopy(n_excl))
      let ident = ast.IdentifierPattern(name:, span:)
      use e <- result.map(emit_destructuring_bind(e, ident, binding_kind))
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
  let e = emit_op(e, opcode.Dup)
  use e <- result.try(emit_key(e))
  case has_rest {
    False -> {
      let e = emit_op(e, opcode.GetElem)
      use e <- result.map(emit_destructuring_bind(e, inner, binding_kind))
      #(e, n_excl)
    }
    True -> {
      let e = emit_op(e, opcode.GetElem2)
      use e <- result.map(emit_destructuring_bind(e, inner, binding_kind))
      // [k,src,src,..] → swap → [src,k,src,..] → pop → [k,src,..]
      // → swap → [src,k,..]
      let e = emit_op(e, opcode.Swap)
      let e = emit_op(e, opcode.Pop)
      #(emit_op(e, opcode.Swap), n_excl + 1)
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
  case ast_util.member_static_prop(target), target {
    _, ast.Identifier(name:, ..) -> Ok(emit_var_put(e, name))

    _, ast.ParenthesizedExpression(_, inner) ->
      emit_destructuring_assign(e, inner)

    // super.p / super[k] — a SuperReference PutValue (§13.3.7.3) needs the
    // super base ([[HomeObject]]'s prototype) AND the `this` receiver under
    // the value: the IrPutSuperValue shape, not the single-object PutField
    // shape below. Stack: [val] → this → Swap → [val,this] → home proto →
    // Swap → [val,proto,this] → key → Swap → [val,key,proto,this]
    // → PutSuperValue → [val] → Pop.
    _, ast.MemberExpression(_, ast.SuperExpression(_), key, computed) -> {
      let e =
        e
        |> get_this
        |> emit_op(opcode.Swap)
        |> get_lexical(opcode.RefHomeObject)
        |> emit_op(opcode.GetPrototypeOf)
        |> emit_op(opcode.Swap)
      use e <- result.map(emit_super_key(e, key, computed))
      e
      |> emit_op(opcode.Swap)
      |> emit_op(opcode.PutSuperValue)
      |> emit_op(opcode.Pop)
    }

    // obj.prop — stack [val] → eval obj → [obj,val] → swap → [val,obj]
    // → PutField → [val] → Pop. (PutField pops [value,obj], leaves value.)
    Some(#(obj, prop)), _ -> {
      use e <- result.map(emit_expr(e, obj))
      let e = emit_op(e, opcode.Swap)
      let e = emit_put_field(e, prop)
      emit_op(e, opcode.Pop)
    }

    // obj[key] — PutElem wants [val,key,obj]. With only Dup/Swap (no rot3):
    // [val] → emit obj → [obj,val] → swap → [val,obj] → emit key → [key,val,obj]
    // → swap → [val,key,obj] → PutElem → [val] → Pop.
    _, ast.MemberExpression(_, obj, key, True) -> {
      use e <- result.try(emit_expr(e, obj))
      let e = emit_op(e, opcode.Swap)
      use e <- result.map(emit_expr(e, key))
      let e = emit_op(e, opcode.Swap)
      let e = emit_op(e, opcode.PutElem)
      emit_op(e, opcode.Pop)
    }

    // target = default  (AssignmentElement with Initializer, §13.15.5.3)
    // If incoming value === undefined, replace with default; then recurse.
    _, ast.AssignmentExpression(_, ast.Assign, left, default_expr) -> {
      let name = case left {
        ast.Identifier(name:, ..) -> Some(name)
        _ -> None
      }
      use e <- result.try(emit_default_if_undefined(e, default_expr, name))
      emit_destructuring_assign(e, left)
    }

    _, ast.ArrayExpression(_, elements) -> {
      use e, close_throw <- with_iterator_scaffold(e)
      emit_array_assign_elements(e, elements, close_throw)
    }

    _, ast.ObjectExpression(_, properties) -> {
      let has_rest =
        list.any(properties, fn(p) {
          case p {
            ast.SpreadProperty(_) -> True
            ast.InitProperty(..)
            | ast.MethodProperty(..)
            | ast.AccessorProperty(..) -> False
          }
        })
      emit_object_pattern(
        e,
        properties,
        has_rest,
        emit_single_object_assign_prop,
      )
    }

    // Annex B web-compat for-in/of LHS: `for (f() of [1])` — evaluate the
    // call each iteration, then throw ReferenceError before assigning.
    // Entry stack is [val]; the trailing IrPop is unreachable but keeps the
    // consumed-value invariant for static bookkeeping.
    _, ast.CallExpression(..) as call -> {
      use e <- result.map(emit_expr(e, call))
      let e = emit_op(e, opcode.Pop)
      let e =
        emit_op(
          e,
          opcode.ThrowError(
            opcode.ReferenceErrorKind,
            "Invalid left-hand side in assignment",
          ),
        )
      emit_op(e, opcode.Pop)
    }

    // §13.15.5 early error: DestructuringAssignmentTargetType must be
    // simple. The parser catches most of these, but some — e.g. an object
    // rest with a non-target argument, `({...5} = {})` — only get shape-
    // checked here, so report them with the parser's own message.
    _, _ -> Error(EarlySyntaxError("Invalid destructuring assignment target"))
  }
}

/// Array assignment pattern elements — emit_array_pattern_elements with
/// ast.Expression leaves routed through emit_destructuring_assign.
fn emit_array_assign_elements(
  e: Emitter,
  elements: List(Option(ast.Expression)),
  close_throw: Int,
) -> Result(#(Emitter, Bool), EmitError) {
  use e, el <- emit_array_pattern_elements(e, elements)
  case el {
    ast.SpreadElement(argument:, ..) ->
      emit_array_assign_rest(e, ast_util.unwrap_parens(argument), close_throw)
    target -> {
      use e <- result.map(emit_array_assign_element(
        e,
        ast_util.unwrap_parens(target),
      ))
      #(e, False)
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
  case ast_util.member_static_prop(target), target {
    // super.p / super[k] — its Reference evaluation (lexical home object +
    // `this`) has no observable effect, so step the iterator first and let
    // emit_destructuring_assign build the PutSuperValue store (the member
    // arms below are shaped around a single evaluated base object). Only a
    // COMPUTED super key deviates from lref-before-step order.
    _, ast.MemberExpression(_, ast.SuperExpression(_), _, _) -> {
      let e = emit_op(e, opcode.IteratorNext)
      let e = emit_op(e, opcode.Pop)
      emit_destructuring_assign(e, target)
    }
    // obj.prop — [iter] → obj → [obj,iter] → Swap → [iter,obj]
    // → IteratorNext → [done,value,iter,obj] → Pop → [value,iter,obj]
    // → Rot3 → [obj,value,iter] → Swap → [value,obj,iter]
    // → PutField → [value,iter] → Pop → [iter].
    Some(#(obj, prop)), _ -> {
      use e <- result.map(emit_expr(e, obj))
      e
      |> emit_op(opcode.Swap)
      |> emit_op(opcode.IteratorNext)
      |> emit_op(opcode.Pop)
      |> emit_op(opcode.Rot3)
      |> emit_op(opcode.Swap)
      |> emit_put_field(prop)
      |> emit_op(opcode.Pop)
    }
    // obj[key] — the key's ToPropertyKey stays deferred to PutElem
    // (§13.15.5.6: PutValue runs after the iterator step). [iter] → obj
    // → key → [key,obj,iter] → Rot3 → [iter,key,obj] → IteratorNext → Pop
    // → [value,iter,key,obj] → Swap → [iter,value,key,obj] → Unrot4
    // → [value,key,obj,iter] → PutElem → [value,iter] → Pop → [iter].
    _, ast.MemberExpression(_, obj, key, True) -> {
      use e <- result.try(emit_expr(e, obj))
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
    // Identifiers, nested patterns, defaults: step first, then assign.
    _, _ -> {
      let e = emit_op(e, opcode.IteratorNext)
      // [done, value, iter] — discard done, assign value.
      let e = emit_op(e, opcode.Pop)
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
  case ast_util.member_static_prop(target), target {
    // super.p / super[k] rest target — same routing as
    // emit_array_assign_element: no observable lref evaluation, so drain
    // first and let emit_destructuring_assign build the super store.
    _, ast.MemberExpression(_, ast.SuperExpression(_), _, _) -> {
      let e = emit_op(e, opcode.PopTry)
      let e = emit_op(e, opcode.IteratorRest)
      use e <- result.map(emit_destructuring_assign(e, target))
      #(e, True)
    }
    // obj.prop — evaluate obj (under the F_body guard, above iter so a throw
    // unwinds to [thrown, iter, ..]), then drain, then PutField.
    // [iter] → [obj,iter] → Swap → [iter,obj] → PopTry → IteratorRest
    // → [arr,obj] → PutField → [arr] → Pop.
    Some(#(obj, prop)), _ -> {
      use e <- result.map(emit_expr(e, obj))
      let e = emit_op(e, opcode.Swap)
      let e = emit_op(e, opcode.PopTry)
      let e = emit_op(e, opcode.IteratorRest)
      let e = emit_put_field(e, prop)
      #(emit_op(e, opcode.Pop), True)
    }
    // obj[key] — obj AND key evaluate before draining. The key may throw, so
    // after tucking obj beneath iter the original F_body frame's recorded
    // depth no longer matches — re-arm the close guard with iter back on top.
    // [iter] → [obj,iter] → PopTry → Swap → [iter,obj] → PushTry(close_throw)
    // → emit key → [key,iter,obj] → Swap → [iter,key,obj] → PopTry
    // → IteratorRest → [arr,key,obj] → PutElem → [arr] → Pop.
    _, ast.MemberExpression(_, obj, key, True) -> {
      use e <- result.try(emit_expr(e, obj))
      let e = emit_op(e, opcode.PopTry)
      let e = emit_op(e, opcode.Swap)
      let e = emit_ir(e, IrPushTry(close_throw, IterCloseGuard))
      use e <- result.map(emit_expr(e, key))
      let e = emit_op(e, opcode.Swap)
      let e = emit_op(e, opcode.PopTry)
      let e = emit_op(e, opcode.IteratorRest)
      let e = emit_op(e, opcode.PutElem)
      #(emit_op(e, opcode.Pop), True)
    }
    // Identifier targets (no observable Reference side effects) and nested
    // array/object patterns (spec drains into A first, §13.15.5.5 step 4):
    // drain, then bind/destructure.
    _, other -> {
      // [iter], try=[F_body,..] → PopTry → IteratorRest → [arr]
      let e = emit_op(e, opcode.PopTry)
      let e = emit_op(e, opcode.IteratorRest)
      use e <- result.map(emit_destructuring_assign(e, other))
      #(e, True)
    }
  }
}

/// One ObjectAssignmentPattern property — §13.15.5.2. Stack invariant per
/// emit_object_pattern: [src, key_n,..,key_1] with excluded keys stashed
/// beneath src when has_rest, else just [src].
fn emit_single_object_assign_prop(
  e: Emitter,
  prop: ast.Property,
  has_rest: Bool,
  n_excl: Int,
) -> Result(#(Emitter, Int), EmitError) {
  case prop {
    // {[expr]: target} — Dup obj, eval key, GetElem, recurse.
    ast.InitProperty(key: ast.KeyComputed(expression:), value:, ..) -> {
      let e = emit_op(e, opcode.Dup)
      use e <- result.try(emit_expr(e, expression))
      // §13.2.5.4 ComputedPropertyName: ToPropertyKey fires eagerly at
      // PropertyName evaluation — observably BEFORE the target reference
      // is evaluated (keyed-destructuring evaluation-order tests).
      let e = emit_op(e, opcode.ToPropertyKey)
      emit_elem_key_assign(e, value, has_rest, n_excl)
    }

    // {key: target} or shorthand {key} (value==Identifier(key)) — a literal
    // (identifier / string / numeric / bigint) key.
    ast.InitProperty(key:, value:, ..) ->
      case object_prop_key_name(key) {
        Some(name) -> {
          use e <- result.map(emit_keyed_destructure_assign(e, name, value))
          case has_rest {
            False -> #(e, n_excl)
            True -> {
              let e = push_const(e, JsString(name))
              #(emit_op(e, opcode.Swap), n_excl + 1)
            }
          }
        }
        None -> {
          // A bigint literal key — no static string form, so route through
          // the elem path (GetElem does the ToPropertyKey).
          let e = emit_op(e, opcode.Dup)
          use e <- result.try(emit_property_key(e, key))
          emit_elem_key_assign(e, value, has_rest, n_excl)
        }
      }

    // Getter/setter/method properties never appear in valid assignment
    // patterns (parser sets has_invalid_pattern). Guard defensively.
    ast.AccessorProperty(..) | ast.MethodProperty(..) ->
      Error(AccessorInDestructuringPattern)

    // {a, b, ...target} — §13.15.5.4 RestDestructuringAssignmentEvaluation.
    // Stack: [src, key_n,..,key_1] → ObjectRestCopy(n) → [rest_obj] → assign.
    ast.SpreadProperty(argument) -> {
      let e = emit_op(e, opcode.ObjectRestCopy(n_excl))
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
  let bare = ast_util.unwrap_parens(target)
  case ast_util.member_static_prop(bare), bare {
    // super.p / super[k] target: no observable lref evaluation, so GetV
    // first and let emit_destructuring_assign build the super store.
    _, ast.MemberExpression(_, ast.SuperExpression(_), _, _) -> {
      let e = emit_op(e, opcode.Dup)
      let e = emit_ir(e, IrGetField(name))
      emit_destructuring_assign(e, target)
    }
    // [src] → Dup → [src,src] → obj → [obj,src,src] → Swap → [src,obj,src]
    // → GetField → [v,obj,src] → Put → [v,src] → Pop → [src].
    Some(#(obj, prop)), _ -> {
      let e = emit_op(e, opcode.Dup)
      use e <- result.map(emit_expr(e, obj))
      e
      |> emit_op(opcode.Swap)
      |> emit_ir(IrGetField(name))
      |> emit_put_field(prop)
      |> emit_op(opcode.Pop)
    }
    // obj[key]: [src] → Dup → [src,src] → obj → [obj,src,src] → Swap →
    // [src,obj,src] → key → [key,src,obj,src] → Swap → [src,key,obj,src] →
    // GetField → [v,key,obj,src] → PutElem → [v,src] → Pop → [src].
    // base→key→GetV order matches §13.3.2.1 + §13.15.5.6 step 1a.
    _, ast.MemberExpression(_, obj, key, True) -> {
      let e = emit_op(e, opcode.Dup)
      use e <- result.try(emit_expr(e, obj))
      let e = emit_op(e, opcode.Swap)
      use e <- result.map(emit_expr(e, key))
      e
      |> emit_op(opcode.Swap)
      |> emit_ir(IrGetField(name))
      |> emit_op(opcode.PutElem)
      |> emit_op(opcode.Pop)
    }
    // Identifier / nested pattern / default targets keep GetV-first — that's
    // spec-correct: step 1 only applies when target is NOT a pattern, and
    // Identifier lref evaluation has no observable effect ahead of GetV.
    _, _ -> {
      let e = emit_op(e, opcode.Dup)
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
      use e <- result.map(emit_elem_keyed_target(
        e,
        ast_util.unwrap_parens(value),
      ))
      #(e, n_excl)
    }
    True -> {
      let e = emit_op(e, opcode.GetElem2)
      use e <- result.map(emit_destructuring_assign(e, value))
      let e = emit_op(e, opcode.Swap)
      let e = emit_op(e, opcode.Pop)
      #(emit_op(e, opcode.Swap), n_excl + 1)
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
  case ast_util.member_static_prop(target), target {
    // super.p / super[k] target: no observable lref evaluation — get the
    // source value and let emit_destructuring_assign build the super store.
    _, ast.MemberExpression(_, ast.SuperExpression(_), _, _) -> {
      let e = emit_op(e, opcode.GetElem)
      emit_destructuring_assign(e, target)
    }
    // [key,srcd,src] → tobj → [tobj,key,srcd,src] → unrot3
    // → [key,srcd,tobj,src] → GetElem → [v,tobj,src]
    // → PutField → [v,src] → Pop → [src].
    Some(#(tobj, prop)), _ -> {
      use e <- result.map(emit_expr(e, tobj))
      e
      |> emit_unrot3
      |> emit_op(opcode.GetElem)
      |> emit_put_field(prop)
      |> emit_op(opcode.Pop)
    }
    // [key,srcd,src] → tobj → unrot3 → [key,srcd,tobj,src] → tkey → unrot3
    // → [key,srcd,tkey,tobj,src] → GetElem → [v,tkey,tobj,src]
    // → PutElem → [v,src] → Pop → [src].
    _, ast.MemberExpression(_, tobj, tkey, True) -> {
      use e <- result.try(emit_expr(e, tobj))
      let e = emit_unrot3(e)
      use e <- result.map(emit_expr(e, tkey))
      e
      |> emit_unrot3
      |> emit_op(opcode.GetElem)
      |> emit_op(opcode.PutElem)
      |> emit_op(opcode.Pop)
    }
    // target = default with a member target: lref first, get, then the
    // default check, then PutValue.
    _, ast.AssignmentExpression(_, ast.Assign, left, default_expr) as assign ->
      case ast_util.unwrap_parens(left) {
        // Super member with a default: handled by the generic path (the
        // lref has no observable evaluation).
        ast.MemberExpression(_, ast.SuperExpression(_), _, _) -> {
          let e = emit_op(e, opcode.GetElem)
          emit_destructuring_assign(e, assign)
        }
        ast.MemberExpression(..) as member -> {
          use e <- result.try(emit_elem_keyed_member_default(
            e,
            member,
            default_expr,
          ))
          Ok(e)
        }
        _ -> {
          let e = emit_op(e, opcode.GetElem)
          emit_destructuring_assign(e, assign)
        }
      }
    // Identifiers and nested patterns: get first (patterns skip step 1a;
    // identifier lrefs have no observable evaluation).
    _, other -> {
      let e = emit_op(e, opcode.GetElem)
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
  // Evaluate the member lref and the source get, leaving [v, ..put operands];
  // pair with the matching put closure so the post-default write is decided
  // once.
  use #(e, put) <- result.try(case ast_util.member_static_prop(member), member {
    Some(#(tobj, prop)), _ -> {
      use e <- result.map(emit_expr(e, tobj))
      let e = emit_unrot3(e)
      #(emit_op(e, opcode.GetElem), emit_put_field(_, prop))
    }
    _, ast.MemberExpression(_, tobj, tkey, True) -> {
      use e <- result.try(emit_expr(e, tobj))
      let e = emit_unrot3(e)
      use e <- result.map(emit_expr(e, tkey))
      let e = emit_unrot3(e)
      #(emit_op(e, opcode.GetElem), emit_op(_, opcode.PutElem))
    }
    // Only MemberExpression targets reach here, and the two arms above cover
    // static and computed keys.
    _, _ -> Error(NonMemberDefaultTarget)
  })
  use e <- result.map(emit_default_if_undefined(e, default_expr, None))
  emit_op(put(e), opcode.Pop)
}

/// [a, b, c, ..] → [b, c, a, ..] — send the top element down two (Rot3
/// applied twice). Tucks a freshly-evaluated operand beneath two pending
/// stack slots.
fn emit_unrot3(e: Emitter) -> Emitter {
  e
  |> emit_op(opcode.Rot3)
  |> emit_op(opcode.Rot3)
}

/// The compile-time string form of an object-pattern key, or None when the key
/// has none (a computed key, or a bigint literal). Numeric keys go through
/// js_format_number for the canonical form ("1", not "1.0"). `KeyPrivate` is
/// unreachable — the parser rejects `({#x: y} = o)`.
fn object_prop_key_name(key: ast.PropertyKey) -> Option(String) {
  case key {
    ast.KeyIdentifier(name:, ..) | ast.KeyPrivate(name:, ..) -> Some(name)
    ast.KeyString(value: s, ..) -> Some(s)
    ast.KeyNumber(value: ast.FiniteNumber(f), ..) ->
      Some(value.js_format_number(f))
    ast.KeyNumber(value: ast.InfiniteNumber, ..) -> Some("Infinity")
    ast.KeyBigInt(..) | ast.KeyComputed(..) -> None
  }
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
  let e = emit_op(e, opcode.GetIterator)
  let e = emit_ir(e, IrPushTry(close_throw, IterCloseGuard))
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
      |> emit_op(opcode.PopTry)
      |> emit_op(opcode.IteratorClose)
      |> emit_ir(IrJump(done_label))
  }
  // Abrupt: [thrown, iter, ..]. CloseThrow swallows .return() result/error
  // and rethrows the original (§7.4.11 step 5).
  let e = emit_ir(e, IrLabel(close_throw))
  let e = emit_op(e, opcode.IteratorCloseThrow)
  emit_ir(e, IrLabel(done_label))
}

/// Array-pattern element loop shared by binding (over ast.Pattern, §8.6.2)
/// and assignment (over ast.Expression, §13.15.5.2). Handles the [] / hole
/// cases; defers each present element to `emit_one`, which returns
/// #(e, True) when it consumed the iterator (rest element — F_body popped,
/// no close) or #(e, False) to continue. Stack invariant: [iter, ...] on
/// entry; on exit #(e, False) → [iter, ...] (caller closes), #(e, True) → [...].
fn emit_array_pattern_elements(
  e: Emitter,
  elements: List(Option(el)),
  emit_one: fn(Emitter, el) -> Result(#(Emitter, Bool), EmitError),
) -> Result(#(Emitter, Bool), EmitError) {
  case elements {
    [] -> Ok(#(e, False))
    // Hole: step iterator, discard [done, value] → [iter].
    [None, ..rest] -> {
      let e =
        e
        |> emit_op(opcode.IteratorNext)
        |> emit_op(opcode.Pop)
        |> emit_op(opcode.Pop)
      emit_array_pattern_elements(e, rest, emit_one)
    }
    [Some(el), ..rest] -> {
      use #(e, rested) <- result.try(emit_one(e, el))
      case rested {
        True -> Ok(#(e, True))
        False -> emit_array_pattern_elements(e, rest, emit_one)
      }
    }
  }
}

fn emit_array_elements(
  e: Emitter,
  elements: List(Option(ast.Pattern)),
  binding_kind: BindingKind,
) -> Result(#(Emitter, Bool), EmitError) {
  use e, el <- emit_array_pattern_elements(e, elements)
  case el {
    // Rest: [[Done]] becomes true the moment we start draining → no close on
    // ANY subsequent throw (.next() error or rest-target bind). Pop F_body
    // first so those throws propagate directly past the close guard.
    // [iter], try=[F_body,..] → PopTry → IteratorRest → [arr] → bind.
    ast.RestElement(argument:) -> {
      let e = e |> emit_op(opcode.PopTry) |> emit_op(opcode.IteratorRest)
      use e <- result.map(emit_destructuring_bind(e, argument, binding_kind))
      #(e, True)
    }
    pattern -> {
      // [iter] → IteratorNext → [done, value, iter] → Pop → bind value.
      let e = e |> emit_op(opcode.IteratorNext) |> emit_op(opcode.Pop)
      use e <- result.map(emit_destructuring_bind(e, pattern, binding_kind))
      #(e, False)
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
  }
}

/// The unary operators that lower to a single `IrUnaryOp`. `typeof` and
/// `delete` do NOT: they need the unresolvable-reference / property-reference
/// treatment their dedicated `emit_expr` arms give them, so they map to `None`
/// here rather than a plausible-but-wrong opcode. (Same shape as
/// `compound_to_binop`, which rejects the operators its caller handles
/// elsewhere.)
fn translate_unaryop(op: ast.UnaryOp) -> Option(opcode.UnaryOpKind) {
  case op {
    ast.Negate -> Some(opcode.Neg)
    ast.UnaryPlus -> Some(opcode.Pos)
    ast.LogicalNot -> Some(opcode.LogicalNot)
    ast.BitwiseNot -> Some(opcode.BitNot)
    ast.Void -> Some(opcode.Void)
    ast.TypeOf | ast.Delete -> None
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
  op: ast.LogicalOp,
  lhs: ast.Expression,
  right: ast.Expression,
) -> Result(Emitter, EmitError) {
  case ast_util.member_static_prop(lhs), lhs {
    _, ast.Identifier(name:, ..) -> {
      let #(e, end_label) = fresh_label(e)
      with_identifier_lref(e, name, fn(e, ref) {
        let e = emit_var_ref_get(e, ref)
        let e = emit_short_circuit_test(e, op, end_label)
        // Test passed: drop the old value, evaluate RHS, write, leave RHS.
        let e = emit_op(e, opcode.Pop)
        emit_named_expr(e, right, name)
      })
      |> result.map(emit_ir(_, IrLabel(end_label)))
    }
    // super.x &&= v — needs GetSuperValue2/PutSuperValue stack juggling on
    // the short-circuit path; a genuine engine gap, not a user error.
    _, ast.MemberExpression(_, ast.SuperExpression(_), _, _) ->
      Error(UnsupportedFeature("logical assignment to a super property"))
    Some(#(obj, prop)), _ -> {
      use e <- result.try(emit_expr(e, obj))
      let e = emit_get_field2(e, prop)
      emit_logical_assign_member(e, op, right, emit_put_field(_, prop), 1)
    }
    _, ast.MemberExpression(_, obj, key, True) -> {
      use e <- result.try(emit_expr(e, obj))
      use e <- result.try(emit_expr(e, key))
      let e = emit_op(e, opcode.GetElem2)
      emit_logical_assign_member(e, op, right, emit_op(_, opcode.PutElem), 2)
    }
    // The parser rejects non-simple logical-assignment targets, so this
    // should only ever be a target shape the emitter can't lower yet.
    _, _ -> Error(UnsupportedFeature("logical assignment target"))
  }
}

/// Shared MemberExpression tail of `emit_logical_assign`. Entry stack:
/// [old, …put-args] (the get2 read is already done); `kept` is the number of
/// put-args to nip on the short-circuit path so [old, …put-args] → [old].
fn emit_logical_assign_member(
  e: Emitter,
  op: ast.LogicalOp,
  right: ast.Expression,
  put: fn(Emitter) -> Emitter,
  kept: Int,
) -> Result(Emitter, EmitError) {
  let #(e, short_label) = fresh_label(e)
  let #(e, end_label) = fresh_label(e)
  let e = emit_short_circuit_test(e, op, short_label)
  let e = emit_op(e, opcode.Pop)
  use e <- result.map(emit_expr(e, right))
  e
  |> put
  |> emit_ir(IrJump(end_label))
  |> emit_ir(IrLabel(short_label))
  |> repeat_nip(kept)
  |> emit_ir(IrLabel(end_label))
}

/// Short-circuit test for `&&` / `||` / `??` (and their `op=` assign forms).
/// On entry the test value is on top of the stack. Dup-jumps to
/// `short_label` (value kept) when the op short-circuits; falls through
/// (value kept) when it doesn't — caller pops and evaluates the RHS.
fn emit_short_circuit_test(
  e: Emitter,
  op: ast.LogicalOp,
  short_label: Int,
) -> Emitter {
  let e = emit_op(e, opcode.Dup)
  case op {
    ast.LogicalAnd -> emit_ir(e, IrJumpIfFalse(short_label))
    ast.LogicalOr -> emit_ir(e, IrJumpIfTrue(short_label))
    // No jump-if-not-nullish op, so hop over an unconditional short jump.
    ast.NullishCoalescing -> {
      let #(e, go_label) = fresh_label(e)
      e
      |> emit_ir(IrJumpIfNullish(go_label))
      |> emit_ir(IrJump(short_label))
      |> emit_ir(IrLabel(go_label))
    }
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
  let private_names = ast_util.class_private_names(body)
  let e =
    Emitter(
      ..e,
      strict: True,
      private_env: list.append(private_names, e.private_env),
    )
  // §15.7.14 step 4/5: per-class scope (V8/SM ClassBody scope). Holds, in this
  // EXACT order (slot layout — see ast_util.class_body_bindings): the immutable inner
  // class-name binding (if any), the <class_fields_init> [[Fields]] const,
  // every "#x" PrivateName const, every instance private-method closure stash
  // ("\u{0}pm:/pg:/ps:"), and every computed-element-key stash ("\u{0}ck:N").
  // The parser's scope pass pre-registers this same list (ast_util.class_body_bindings
  // is the single source of truth) so emit_var_init/get below resolve by
  // (scope_id, name) to the right slot; enter_scope's prologue seeds them
  // BEFORE heritage emit so `class C extends C {}` TDZs on the inner C,
  // and so ctor / method / field-init-fn IrMakeClosure snapshots see every
  // slot for capture.
  let #(e, save) = enter_scope(e, in_block: e.in_block)
  // §15.7.14 steps 5/6: bind each private-name const to a freshly minted
  // PrivateName key NOW (after declaration, before any class-element child
  // closure is built) — so each class *evaluation* gets distinct names,
  // nested classes shadow, and methods/eval capture them through ordinary
  // lexical scoping. The remaining class-scope consts are init'd later:
  // <class_fields_init> by emit_attach_field_init, "\u{0}pm:/pg:/ps:" by
  // emit_class_methods, "\u{0}ck:N" inside compile_class_body, and the inner
  // name binding after compile_class_body returns (so `class C { [C] = 1 }`
  // throws a TDZ ReferenceError — element evaluation precedes
  // InitializeBinding(classBinding, F)).
  let e =
    list.fold(private_names, e, fn(e, pname) {
      e |> emit_op(opcode.NewPrivateName(pname)) |> emit_var_init(pname)
    })
  let element_keys = ast_util.computed_element_keys(body)
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
    Some(n) -> e |> emit_op(opcode.Dup) |> emit_var_init(n)
    None -> e
  }
  let e = emit_call_static_init(e, static_init_idx)
  let e = leave_scope(e, save)
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
  let ast_util.ClassBodyParts(
    constructor: ctor_method,
    instance_methods:,
    static_methods:,
    instance_fields:,
    static_elements:,
  ) = ast_util.classify_class_body(body)

  // Build constructor: if none provided, synthesize the spec default. For a
  // derived class that's (§15.7.14 ClassDefaultConstructor) a forward of
  // the received argument List to the parent constructor — emitted as a
  // `super(...)` whose arguments come straight from the frame, with NO
  // observable array iteration (`in_synth_default_ctor`). For a base class
  // it's an empty body.
  let #(ctor_params, ctor_body, synth_super_forward) = case ctor_method {
    Some(ast_util.ClassMethodEl(
      fun: ast.FunctionLiteral(params:, body:, ..),
      ..,
    )) -> #(params, body, False)
    None -> #([], default_ctor_body(super_class), option.is_some(super_class))
  }

  // §13.3.7.1 SuperCall step 12 / §15.7.14 step 28: instance fields are
  // compiled into one synthetic initializer function (QuickJS class_fields_init
  // / spec [[Fields]]), bound to the <class_fields_init> const. Derived ctors
  // call it via emit_field_init_call after every `super()`
  // (FieldInitAfterSuper); base ctors call it before user code runs
  // (FieldInitAtStart, §10.2.2 [[Construct]] step 6). Constructors cannot be
  // generators or async (spec forbids it).
  use #(e, init_idx) <- result.try(compile_class_init_fn(
    e,
    list.append(
      private_method_inits(instance_methods),
      field_inits(instance_fields),
    ),
  ))
  let #(ctor_kind, field_init) = case super_class {
    Some(_) -> #(opcode.DerivedCtorCode, FieldInitAfterSuper)
    None -> #(opcode.MethodCode, FieldInitAtStart)
  }
  use #(e, child) <- result.try(compile_function_body(
    // Scoped to this one compile: the child emitter inherits the flag.
    Emitter(..e, in_synth_default_ctor: synth_super_forward),
    name,
    None,
    ctor_params,
    StmtsBody(ctor_body),
    is_arrow: False,
    is_generator: False,
    is_async: False,
    // Class constructor — IS a constructor.
    is_constructor: True,
    code_kind: ctor_kind,
    field_init: option.map(init_idx, fn(_) { field_init })
      |> option.unwrap(NoFieldInit),
  ))
  let e = Emitter(..e, in_synth_default_ctor: False)
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
      |> emit_op(opcode.MakeClosure(ctor_idx))
      |> emit_op(opcode.SetupDerivedClass)
    }
    None ->
      // MakeClosure (creates .prototype + .prototype.constructor) → [ctor].
      // §15.7.14 step 12: ctor.[[HomeObject]] = ctor.prototype, so `super.x`
      // in a base ctor body resolves through ctor.prototype.__proto__
      // (Object.prototype).
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
      |> emit_op(opcode.ToPropertyKey)
      |> emit_var_init(ast_util.computed_field_const(idx))
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
  use #(e, static_init_idx) <- result.map(compile_class_init_fn(
    e,
    static_inits(static_elements),
  ))

  // Stack: [ctor]
  #(e, static_init_idx)
}

/// Spec default constructor body: `super(...arguments)` for derived classes,
/// empty for base classes. The synthetic call's nodes carry the heritage
/// expression's span — that is the source token whose presence caused the
/// implicit super-call to exist.
fn default_ctor_body(
  super_class: Option(ast.Expression),
) -> List(ast.StmtWithLine) {
  case super_class {
    None -> []
    Some(heritage) -> {
      let span = ast.expression_span(heritage)
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

/// Compile a list of field-init elements into a synthetic non-arrow
/// initializer function with FieldInitCode — a method-like body (§15.7.14:
/// "the function created for [[Initializer]] is never directly accessible to
/// ECMAScript code"). Shared by instance-field init (this = instance,
/// [[HomeObject]] = ctor.prototype) and static-element init (this = ctor,
/// [[HomeObject]] = ctor). Returns Some(child_idx) when `inits` is non-empty.
fn compile_class_init_fn(
  e: Emitter,
  inits: List(FieldInit),
) -> Result(#(Emitter, Option(Int)), EmitError) {
  case inits {
    [] -> Ok(#(e, None))
    _ -> {
      use #(e, child) <- result.map(compile_function_body(
        e,
        None,
        None,
        [],
        FieldInitsBody(inits),
        is_arrow: False,
        is_generator: False,
        is_async: False,
        // Synthetic field initializer — never directly constructible.
        is_constructor: False,
        code_kind: opcode.FieldInitCode,
        field_init: NoFieldInit,
      ))
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
      |> emit_op(opcode.Dup)
      |> emit_ir(IrGetField("prototype"))
      |> emit_op(opcode.MakeClosure(idx))
      |> emit_op(opcode.MakeMethod)
      |> emit_op(opcode.Swap)
      |> emit_op(opcode.Pop)
  }
  |> emit_var_init(class_fields_init)
}

/// Stack: [ctor] → [ctor]. Dup ctor, optionally fetch `.prototype` so the
/// body sees [target] on top, run `body`, then pop back to [ctor].
fn with_method_target(
  e: Emitter,
  on_prototype: Bool,
  body: fn(Emitter) -> Result(Emitter, EmitError),
) -> Result(Emitter, EmitError) {
  let e = emit_op(e, opcode.Dup)
  let e = case on_prototype {
    True -> emit_ir(e, IrGetField("prototype"))
    False -> e
  }
  use e <- result.map(body(e))
  emit_op(e, opcode.Pop)
}

/// Define a list of class methods on the constructor (`on_prototype: False`)
/// or on `ctor.prototype` (`on_prototype: True`). Stack: [ctor] → [ctor].
fn emit_class_methods(
  e: Emitter,
  methods: List(ast_util.ClassMethodEl),
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
    ast_util.ClassMethodEl(
      key: ast.KeyPrivate(name:, ..),
      fun: ast.FunctionLiteral(
        params:,
        body:,
        is_generator: is_gen,
        is_async:,
        ..,
      ),
      kind:,
      ..,
    ) -> {
      let fn_name = case kind {
        ast.MethodGet -> "get " <> name
        ast.MethodSet -> "set " <> name
        ast.MethodMethod | ast.MethodConstructor -> name
      }
      use e <- with_method_target(e, on_prototype)
      use e <- result.map(make_method_closure(
        e,
        Some(fn_name),
        params,
        body,
        is_gen,
        is_async,
      ))
      let e = emit_op(e, opcode.MakeMethod)
      let e = case on_prototype {
        True -> emit_var_init(e, ast_util.private_fn_const(kind, name))
        False -> {
          let define = case kind {
            ast.MethodGet ->
              IrFinal(opcode.DefinePrivateAccessor(opcode.Getter))
            ast.MethodSet ->
              IrFinal(opcode.DefinePrivateAccessor(opcode.Setter))
            ast.MethodMethod | ast.MethodConstructor ->
              IrFinal(opcode.DefinePrivateMethod)
          }
          emit_var_get(e, name)
          |> emit_op(opcode.Swap)
          |> emit_ir(define)
        }
      }
      e
    }
    // Static-string key: name() {}, "name"() {}, get name() {}, set name(v) {}
    ast_util.ClassMethodEl(
      key: ast.KeyIdentifier(name:, ..),
      fun: ast.FunctionLiteral(
        params:,
        body:,
        is_generator: is_gen,
        is_async:,
        ..,
      ),
      kind:,
      ..,
    )
    | ast_util.ClassMethodEl(
        key: ast.KeyString(value: name, ..),
        fun: ast.FunctionLiteral(
          params:,
          body:,
          is_generator: is_gen,
          is_async:,
          ..,
        ),
        kind:,
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
      use e <- with_method_target(e, on_prototype)
      use e <- result.map(make_method_closure(
        e,
        Some(fn_name),
        params,
        body,
        is_gen,
        is_async,
      ))
      emit_ir(e, define_op)
    }
    // Computed key or numeric/bigint-literal key (`[expr]() {}`, `0b10() {}`,
    // `1n() {}`). Function name left None — SetFunctionName from runtime keys
    // is not yet implemented (matches object-literal computed methods).
    ast_util.ClassMethodEl(
      body_index:,
      key:,
      fun: ast.FunctionLiteral(
        params:,
        body:,
        is_generator: is_gen,
        is_async:,
        ..,
      ),
      kind:,
    ) -> {
      use e <- with_method_target(e, on_prototype)
      use e <- result.try(emit_class_element_key(e, key, body_index))
      use e <- result.map(make_method_closure(
        e,
        None,
        params,
        body,
        is_gen,
        is_async,
      ))
      case kind {
        ast.MethodGet ->
          emit_op(e, opcode.DefineAccessorComputed(opcode.Getter, False))
        ast.MethodSet ->
          emit_op(e, opcode.DefineAccessorComputed(opcode.Setter, False))
        // MethodConstructor is filtered out by classify_class_body before we
        // get here; treat as a plain method defensively rather than crashing.
        ast.MethodMethod | ast.MethodConstructor ->
          emit_op(e, opcode.DefineMethodComputed)
      }
    }
  }
}

/// Push a class element's property key. A computed key is NEVER re-evaluated
/// here — §15.7.14 evaluated it once, at class-definition time, into the
/// element's stash const (see compile_class_body); this reads that const back.
/// Literal keys are pushed as constants.
fn emit_class_element_key(
  e: Emitter,
  key: ast.PropertyKey,
  body_index: Int,
) -> Result(Emitter, EmitError) {
  case key {
    ast.KeyComputed(..) ->
      Ok(emit_var_get(e, ast_util.computed_field_const(body_index)))
    ast.KeyIdentifier(..)
    | ast.KeyString(..)
    | ast.KeyNumber(..)
    | ast.KeyBigInt(..)
    | ast.KeyPrivate(..) -> emit_property_key(e, key)
  }
}

/// Stack: [ctor] → [ctor]. §15.7.14 step 31: build the static-init closure
/// with [[HomeObject]] = ctor and immediately [[Call]] it with `this` = ctor.
/// No-op when the class has no static elements.
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

/// §7.3.29 PrivateMethodOrAccessorAdd: the per-instance install of every
/// instance private method/accessor half. The closure was already built into
/// the hidden class-scope const `ast_util.private_fn_const(kind, name)`; the
/// element only has to read it back and pick the define-op from `kind`. Spec
/// order: all private methods install BEFORE field initializers run
/// (InitializeInstanceElements steps 5 then 6).
fn private_method_inits(
  methods: List(ast_util.ClassMethodEl),
) -> List(FieldInit) {
  use m <- list.filter_map(methods)
  case m.key {
    ast.KeyPrivate(name:, ..) ->
      Ok(PrivateMethodInit(
        name:,
        closure_const: ast_util.private_fn_const(m.kind, name),
        kind: m.kind,
      ))
    ast.KeyIdentifier(..)
    | ast.KeyString(..)
    | ast.KeyNumber(..)
    | ast.KeyBigInt(..)
    | ast.KeyComputed(..) -> Error(Nil)
  }
}

/// Map every instance ClassField → the FieldInit that defines it. This is the
/// ONE place a field's key shape (private / literal / numeric / computed) is
/// inspected: from here on the constructor names the opcode.
fn field_inits(fields: List(ast_util.ClassFieldEl)) -> List(FieldInit) {
  use field <- list.map(fields)
  field_init_of(field)
}

fn field_init_of(field: ast_util.ClassFieldEl) -> FieldInit {
  let ast_util.ClassFieldEl(body_index:, key:, value:) = field
  // §15.7.14: if Initializer is absent, initValue = undefined. The synthetic
  // undefined-expression carries the key's span so any error points at the
  // field declaration.
  let init =
    option.unwrap(value, ast.UndefinedExpression(ast.property_key_span(key)))
  case key {
    ast.KeyPrivate(name:, ..) -> PrivateFieldInit(name:, init:)
    ast.KeyIdentifier(name:, ..) | ast.KeyString(value: name, ..) ->
      NamedFieldInit(name:, init:)
    ast.KeyNumber(value: n, ..) -> NumericFieldInit(value: n, init:)
    ast.KeyBigInt(value: i, ..) -> BigIntFieldInit(value: i, init:)
    // The key was evaluated + ToPropertyKey'd once at class-definition time
    // into this element's stash const (compile_class_body).
    ast.KeyComputed(..) ->
      ComputedFieldInit(
        key_const: ast_util.computed_field_const(body_index),
        init:,
      )
  }
}

/// Map static elements → the FieldInits of the static-init wrapper body.
/// `static x = v` defines on `this` = ctor; `static { ... }` becomes a
/// StaticBlockInit.
fn static_inits(elements: List(ast_util.StaticEl)) -> List(FieldInit) {
  use elem <- list.map(elements)
  case elem {
    ast_util.StaticField(field) -> field_init_of(field)
    ast_util.StaticBlockEl(body) -> StaticBlockInit(body)
  }
}

/// Emit one element of a class-init function body. Stack-neutral. Every field
/// element pushes `this` (the instance, or the ctor for static elements),
/// leaves the define-op to consume the value, then pops `this` back off.
fn emit_field_init(e: Emitter, fi: FieldInit) -> Result(Emitter, EmitError) {
  case fi {
    // The static block runs as an arrow IIFE, so it takes `this` from the
    // enclosing wrapper rather than pushing its own.
    StaticBlockInit(body:) -> {
      use e <- result.map(emit_expr(e, static_block_iife(body)))
      emit_op(e, opcode.Pop)
    }
    PrivateMethodInit(name:, closure_const:, kind:) ->
      use_this(e, fn(e) {
        let e =
          e
          |> emit_var_get(name)
          |> emit_var_get(closure_const)
        Ok(case kind {
          ast.MethodGet ->
            emit_op(e, opcode.DefinePrivateAccessor(opcode.Getter))
          ast.MethodSet ->
            emit_op(e, opcode.DefinePrivateAccessor(opcode.Setter))
          ast.MethodMethod | ast.MethodConstructor ->
            emit_op(e, opcode.DefinePrivateMethod)
        })
      })
    // §7.3.28 PrivateFieldAdd — the PrivateName comes from the class-scope
    // const named `#x` (see compile_class).
    PrivateFieldInit(name:, init:) ->
      use_this(e, fn(e) {
        let e = emit_var_get(e, name)
        use e <- result.map(emit_named_expr(e, init, name))
        emit_op(e, opcode.DefinePrivateField)
      })
    // §7.3.33 DefineField step 7: anonymous function initializers get the
    // field name (NamedEvaluation).
    NamedFieldInit(name:, init:) ->
      use_this(e, fn(e) {
        use e <- result.map(emit_named_expr(e, init, name))
        emit_ir(e, IrDefineField(name))
      })
    NumericFieldInit(value: n, init:) ->
      use_this(e, fn(e) {
        let e = push_const(e, JsNumber(literal_number(n)))
        use e <- result.map(emit_expr(e, init))
        emit_op(e, opcode.DefineFieldComputed)
      })
    ComputedFieldInit(key_const:, init:) ->
      use_this(e, fn(e) {
        let e = emit_var_get(e, key_const)
        use e <- result.map(emit_expr(e, init))
        emit_op(e, opcode.DefineFieldComputed)
      })
    BigIntFieldInit(value: i, init:) ->
      use_this(e, fn(e) {
        let e = push_const(e, value.JsBigInt(value.BigInt(i)))
        use e <- result.map(emit_expr(e, init))
        emit_op(e, opcode.DefineFieldComputed)
      })
  }
}

/// Push `this` (the target of a field definition), run `body`, pop it back off.
fn use_this(
  e: Emitter,
  body: fn(Emitter) -> Result(Emitter, EmitError),
) -> Result(Emitter, EmitError) {
  use e <- result.map(body(get_this(e)))
  emit_op(e, opcode.Pop)
}

/// `(() => { ...body })()` — the static-block lowering. Synthetic, so all
/// spans are Span(0, 0).
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
