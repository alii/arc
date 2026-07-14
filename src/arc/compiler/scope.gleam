/// Scope-tree finalization and lookup (the V8 / SpiderMonkey / JSC model).
///
/// The PARSER builds a `ScopeBuilder` as it parses (one `RawScope` per
/// scope-introducing construct, one `RawBinding` per declared name, one
/// `raw_refs` entry per identifier reference — V8's VariableProxy/
/// AddUnresolved model). `finalize(sb, opts)` then turns that into a
/// `ScopeTree` — allocating a local slot per binding, resolving each
/// `raw_ref` to free-vs-own, and running `analyze_captures` to decide
/// boxing, capture lists and lexical-pseudo-slot layout. NO AST walks
/// happen here; emit.gleam is the only post-parse AST consumer. The
/// emitter consults the tree (`lookup`, `lookup_lexical`, `function_info`,
/// `alloc_scratch`) and emits concrete `IrGetLocal` / `IrGetBoxed` /
/// `IrGetGlobal` / `IrWith*` ops directly.
///
/// Lookup is keyed by `(scope_id, name)` — not by source span — because a
/// large class of bindings have no source `Identifier` to hang a span on:
/// the per-class `<class_fields_init>` const, private-name consts (`#x`),
/// computed-key stash consts (`\u{0}ck:N`), private-method-fn stash consts
/// (`\u{0}pm:/pg:/ps:`), the module `*default*` binding, the with-object
/// holder `<withN_M>`, the implicit `arguments` binding, and the
/// named-function-expression self-name. Every one of those is captured by
/// a child closure in some valid program, so each must live in the tree as
/// a real named binding the parser pre-registers via `sb_declare`.
/// Pure non-escaping scratch (e.g. `using`-emission temporaries, the
/// completion-value slot, with-ref base slots) is NOT a binding — the
/// emitter mints those via `alloc_scratch` and gets back a bare `Int`.
import arc/vm/lexical.{
  type LexicalRef, type LexicalRefs, type LexicalSlots, RefActiveFunc,
  RefHomeObject, RefNewTarget, RefThis,
}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string

// ============================================================================
// Types
// ============================================================================

/// Where unresolved names fall through to. ToGlobal is the normal case.
/// ToEvalEnv is used when compiling sloppy direct-eval code OR sloppy
/// functions that contain a direct eval — those frames carry an eval_env
/// dict that GetEvalVar/PutEvalVar check before the global object.
pub type GlobalFallthrough {
  ToGlobal
  ToEvalEnv
}

/// What a declared name is. Lives here (not in emit.gleam) so the
/// emit→scope import direction is acyclic — emit.gleam imports this type
/// to construct `Binding`s and to pattern-match `Resolution.Local.kind`.
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

// ============================================================================
// Scope Tree (V8/SM/JSC model)
//
// Built BEFORE emission: the parser threads a `ScopeBuilder` (`sb_*` calls)
// through the parse and `finalize` converts it into this slot-bearing tree.
// The emitter tracks `current_scope_id` as it descends and consults the
// tree via `lookup` / `lookup_lexical` / `function_info` to emit concrete
// GetLocal/GetBoxed/GetGlobal/IrWith* ops directly.
// Resolution is keyed by (scope_id, name): the emitter mints
// synthetic bindings (`arguments`, `<class_fields_init>`, `#name`,
// `<withN_M>`, NFE self-name, lexical pseudo-slots) that have NO source
// span, and several of those ARE captured by child closures, so a
// span-keyed map alone is insufficient. See V8 src/ast/scopes.h.
// ============================================================================

/// Stable identifier for a node in the scope tree. The root scope (the
/// compilation unit's outermost FunctionInfo) is always id 0.
pub type ScopeId =
  Int

/// The root scope id of every ScopeTree.
pub const root_scope_id: ScopeId = 0

/// What kind of syntactic construct introduced a scope. Mirrors V8's
/// `ScopeType` (SCRIPT/MODULE/FUNCTION/BLOCK/CATCH/WITH/CLASS) plus
/// ClassStaticBlock for §15.7.14 static-initialization blocks (which own
/// their own `this`/lexical environment).
///
/// `With(holder)` carries the NAME of the synthetic `<withN_M>` binding
/// (declared IN that scope, minted by `sb_push_with`) holding the
/// (ToObject'd) with-target — deliberately NOT a copied-out slot number,
/// so `do_lookup` reads the holder's slot AND boxedness from the one
/// authoritative `scope.bindings` entry (a derived slot copy would have to
/// be shifted in lockstep by `insert_captures` and could silently drift).
/// Any lookup that walks PAST a With scope must emit a runtime check
/// against that slot (IrWith*). Living on the kind makes "a With scope
/// with no holder" and "a non-With scope with a holder" both
/// unrepresentable.
pub type ScopeKind {
  Module
  Script
  Function
  Block
  Catch
  With(holder: String)
  ClassBody
  ClassStaticBlock
}

/// True for the `With` kind — for callers that only need the discriminant
/// and would otherwise have to spell out a `case` (the payload makes `==`
/// against a bare `With` impossible).
pub fn is_with_kind(kind: ScopeKind) -> Bool {
  case kind {
    With(_) -> True
    _ -> False
  }
}

/// True for scope kinds that own a `FunctionInfo` (their own local-slot
/// space, lexical pseudo-slots, and capture set). Block/Catch/With/ClassBody
/// share their enclosing function's slot space.
pub fn is_function_kind(kind: ScopeKind) -> Bool {
  case kind {
    Module | Script | Function | ClassStaticBlock -> True
    Block | Catch | With(_) | ClassBody -> False
  }
}

/// Where top-level let/const/class declarations land. Decided once per
/// compilation unit; nested block scopes are always local regardless.
/// (Lives here so the future emit→scope import direction has no cycle.)
pub type TopLevelLex {
  /// Global lexical record (state.lexical_globals). Used by the REPL so
  /// `let x = 1` persists across inputs.
  LexGlobal
  /// Local slot in this template. Used by scripts, modules, function bodies,
  /// and eval — §19.2.1.1 PerformEval steps 27–28 always create a fresh
  /// LexicalEnvironment, so eval'd let/const/class never escape.
  LexLocal
}

/// A single declared name in a scope, with its allocated local slot.
///
/// `origin_kind_for_capture` threads the ORIGIN binding's kind through
/// capture chains: when a child captures a const (e.g. the inner class-name
/// binding) or a named-function-expression self-name, writes through that
/// capture must follow the origin's rules (§9.1.1.1.5 SetMutableBinding —
/// const → TypeError; FnNameBinding → strict TypeError, sloppy silent drop).
/// For non-capture bindings, `origin_kind_for_capture == kind`. This replaces
/// the legacy resolver's `const_captures`/`fn_name_captures` Sets.
pub type Binding {
  Binding(
    slot: Int,
    kind: BindingKind,
    is_boxed: Bool,
    origin_kind_for_capture: BindingKind,
  )
}

/// A node in the scope tree.
///
/// `function_scope` is the nearest enclosing scope (possibly self) whose kind
/// satisfies `is_function_kind` — the owner of this scope's local-slot space.
/// A `With(holder)` kind names the synthetic `<withN_M>` binding declared in
/// this scope's own `bindings` (see `ScopeKind`).
/// `contains_direct_eval` is true when this scope's own statement list (not a
/// nested function) contains a `CallExpression` whose callee is the
/// identifier `eval` — used to propagate eval-poisoning to ancestor function
/// scopes (V8's `inner_scope_calls_eval_`).
pub type Scope {
  Scope(
    id: ScopeId,
    parent: Option(ScopeId),
    function_scope: ScopeId,
    kind: ScopeKind,
    bindings: Dict(String, Binding),
    contains_direct_eval: Bool,
    /// §B.3.2: names of FunctionDeclarations directly in this Block scope
    /// that are BLOCKED from Annex B var promotion — an intermediate
    /// let/const/class (or destructured-catch param, or for-head let) of
    /// the same name sits between this block and the var-scope, so
    /// "replacing the FunctionDeclaration with `var F`" would be an early
    /// error. Computed by the DECLARE pass; the emitter's
    /// per-FunctionDeclaration check consults it. Empty for
    /// non-Block scopes and strict-mode bodies. Replaces emit.gleam's
    /// `collect_annexb_candidates` per-call AST walk and the legacy
    /// resolver's positional `annexb_target`.
    annexb_blocked: Set(String),
    /// Strict-mode code. Inherited from `parent` and forced True for
    /// Module / ClassBody / ClassStaticBlock; the parser upgrades it to
    /// True when a `"use strict"` directive opens the body.
    /// Gates Annex B §B.3.2 var-twin declaration (sloppy bodies only) and
    /// `annexb_blocked` computation. V8: `Scope::is_strict_`.
    is_strict: Bool,
    /// §10.2.11 step 28: this Block IS the body of a function whose
    /// parameter list is non-simple, and therefore the sink every body
    /// `var` / hoisted FunctionDeclaration binds in (see
    /// `RawScope.is_var_boundary` / `sb_var_target`). With a SIMPLE
    /// parameter list those same bindings would live in the function root,
    /// so anything reasoning about "the function's var scope" must treat
    /// this Block as part of it — `insert_captures` does. False everywhere
    /// else, including on the function root itself.
    is_var_boundary: Bool,
  )
}

/// Per-function-scope summary the emitter needs to lay out a frame and wire
/// up closures. Replaces the legacy `Resolved` + `CompiledChild` scope fields.
///
/// `captures` lists the names this function pulls from its parent, paired
/// with the PARENT's slot index — the emitter writes these as the closure's
/// env_descriptors at the IrMakeClosure site.
/// `lexical_captures` maps inherited lexical pseudo-bindings (arrows,
/// direct-eval) to the capture-slot index they occupy in THIS frame.
/// `names` is every name ever allocated a slot in this body (cumulative
/// across closed block scopes) — used for module-export slot lookup and
/// direct-eval `local_names`.
/// `contains_direct_eval` is true when this function's OWN body (not a
/// nested function) contains a syntactic `eval(...)` call — drives the
/// `local_names` table on the top-level template so run_direct_eval can
/// alias the frame's locals. `eval_in_subtree` is the transitive flag:
/// true when this function OR any nested function contains a direct eval
/// (V8's `inner_scope_calls_eval_`) — drives `local_names` on every
/// ancestor frame so a deeply-nested eval can reach the whole chain.
pub type FunctionInfo {
  FunctionInfo(
    local_count: Int,
    lexical: LexicalSlots,
    /// Which OWNED lexical pseudo-slots (`lexical` above) are heap-boxed
    /// because an inner arrow / direct-eval captures them. `lookup_lexical`
    /// returns this as the boxed flag for an owned slot — a non-captured
    /// `this` is a plain local, not a box. Populated by `analyze_captures`
    /// (it is the per-function `lexical_captured` value). Replaces the
    /// legacy resolver's `lexical_captured: LexicalRefs` on Resolved.
    lexical_boxed: LexicalRefs,
    captures: List(#(String, Int)),
    lexical_captures: Dict(LexicalRef, Int),
    names: Dict(String, Int),
    fallthrough: GlobalFallthrough,
    contains_direct_eval: Bool,
    eval_in_subtree: Bool,
    /// §B.3.2/.3/.6: names of plain (non-generator, non-async)
    /// FunctionDeclarations nested in Blocks within this sloppy-mode
    /// function/script/eval body that are NOT blocked by an intermediate
    /// lexical declaration — each gets a var-scope twin binding,
    /// initialized undefined before the body runs and overwritten by the
    /// emitter's Annex B promote when the block-level declaration is
    /// evaluated.
    /// Computed by the DECLARE pass (V8: `DeclarationScope::
    /// HoistSloppyBlockFunctions`). Replaces emit.gleam's
    /// `collect_annexb_candidates`. Empty for strict bodies.
    annexb_candidates: List(String),
    /// Arrow function — does NOT own lexical pseudo-slots (this /
    /// active_func / home_object / new.target); a reference inside the
    /// body resolves to the enclosing non-arrow's slot via capture
    /// (§15.3). Recorded by the parser on the function's RawScope so
    /// `build_capture_inputs` can derive `FnAnalysisInput.is_arrow`
    /// without re-walking the AST. V8: `IsArrowFunction(function_kind_)`.
    is_arrow: Bool,
  )
}

/// The full scope tree for one compilation unit.
///
/// `scopes` holds every scope node by id. `functions` holds a FunctionInfo
/// for every scope where `is_function_kind(kind)` is true. `children_at`
/// maps a scope id to its DIRECT child scope ids in source order — the
/// emitter walks this in lockstep with the AST so child function indices
/// (IrMakeClosure(idx)) line up.
pub type ScopeTree {
  ScopeTree(
    scopes: Dict(ScopeId, Scope),
    functions: Dict(ScopeId, FunctionInfo),
    children_at: Dict(ScopeId, List(ScopeId)),
    /// Where top-level let/const/class declarations land in this
    /// compilation unit. Threaded from `AnalyzeOpts.top_lex`; read by the
    /// emitter's `at_global_lex` and the LexGlobal/LexLocal split in
    /// emit_program_common.
    top_lex: TopLevelLex,
    /// Module exports the linker pre-seeds a BoxSlot for before the body
    /// runs (§16.2 instantiation). The emitter's binding-init prologue
    /// SKIPS these — their slot is reserved and boxed, but the linker
    /// owns the initial cell. Threaded from `AnalyzeOpts.linker_seeded`.
    linker_seeded: Set(String),
    /// With-object holders active at the CALLER's site (direct-eval), as
    /// slot indices into this unit's root frame (every holder is seeded as
    /// a capture, so it already has a root slot), innermost first. A
    /// per-unit datum — the enclosing `with`s of a nested function are
    /// derived from the tree itself (`fold_enclosing_withs`), so only the
    /// root's inherited stack has to be carried in. Threaded from
    /// `AnalyzeOpts.with_stack`; empty for everything but direct-eval.
    inherited_with_stack: List(Int),
  )
}

/// The non-`with` ("static") half of a `Resolution` — where a variable
/// reference lands once every crossed `with` object has been probed and
/// missed.
///
/// `Local`: a slot in the current function's frame.
///
/// `origin_kind` on `Local` is the binding's `origin_kind_for_capture` — for
/// a CaptureBinding it is the parent declaration's kind (Const / FnName /
/// Let / …), so the emitter's static-put can route a captured-const write to
/// IrThrowConstAssign without a second tree walk.
pub type Direct {
  Local(slot: Int, boxed: Bool, kind: BindingKind, origin_kind: BindingKind)
  Global(name: String)
  EvalEnv(name: String)
}

/// A local frame slot plus how it must be read: `boxed` slots go through
/// GetBoxed/PutBoxed, plain ones through GetLocal/PutLocal. The pair travels
/// together everywhere the emitter touches a slot, so it gets a name — a
/// bare `#(Int, Bool)` lets a swapped or mismatched pair type-check.
pub type SlotRef {
  SlotRef(slot: Int, boxed: Bool)
}

/// Result of `lookup(tree, scope_id, name)` — what the emitter should emit
/// for a variable reference.
///
/// `Plain`: no `with` scope was crossed — emit `direct` as-is.
/// `WithChain`: the lookup crossed one or more `with` scopes BEFORE reaching
/// `fallback`. `crossed_slots` lists the with-object slots (innermost first);
/// the emitter must emit one IrWith* probe per entry, then `fallback` for the
/// miss case. `fallback` being a `Direct` (not a `Resolution`) makes a nested
/// WithChain unrepresentable by construction.
pub type Resolution {
  Plain(direct: Direct)
  WithChain(crossed_slots: List(SlotRef), fallback: Direct)
}

/// Inputs to `finalize` that come from OUTSIDE the parse — the calling
/// context (compile_module / compile_script / compile_eval_direct /
/// compile_function_body for a child) supplies these.
pub type AnalyzeOpts {
  AnalyzeOpts(
    /// Where top-level let/const/class go (LexGlobal for REPL only).
    top_lex: TopLevelLex,
    /// Where unresolved names fall through to.
    fallthrough: GlobalFallthrough,
    /// Whether the unit is strict-mode code.
    strict: Bool,
    /// name → slot visible from the parent frame (direct-eval / nested fn).
    /// Empty for top-level scripts/modules.
    parent_names: Dict(String, Int),
    /// Inherited lexical pseudo-slot indices (arrows, direct-eval).
    lexical_captures: Dict(LexicalRef, Int),
    /// Module exports the linker pre-allocates a BoxSlot for and seeds into
    /// the slot before the body runs (§16.2 instantiation). Their bindings
    /// reserve the slot and a boxed binding but emit NO init/box op.
    linker_seeded: Set(String),
    /// emit_2core Optimization G: when True, sloppy Script-root `var` /
    /// fn-decls get REAL local slots (as if strict) instead of falling
    /// through to Global. Nested fns then capture them as ordinary boxed
    /// cells. DEFAULT-OFF (g-cell-get-regress): cell_get resolves to
    /// rt_js_store.t_cell_get (JsStore Dict lookup, 62-76ns — NOT the 3ns
    /// pdict-ref path); on richards it net-added ~2.9ms/run vs the 65/run
    /// global_get_fast baseline. Enable only where profile-baseline proves
    /// >1k/run t_global_get_fast. Trade-off when on: `globalThis.X` won't
    /// see these names. Tree-walk interpreter and direct-eval always keep
    /// the spec-compliant `False`.
    module_slot_globals: Bool,
    /// With-object holders inherited from the caller: slot indices INTO
    /// `parent_names` (every holder is one of the caller's local names,
    /// seeded as a capture), innermost first. They are therefore already
    /// counted by `parent_names` and reserve no extra frame slots.
    with_stack: List(Int),
  )
}

/// Default `AnalyzeOpts` for a top-level script/module — no parent context.
pub fn default_analyze_opts() -> AnalyzeOpts {
  AnalyzeOpts(
    top_lex: LexLocal,
    fallthrough: ToGlobal,
    strict: False,
    parent_names: dict.new(),
    lexical_captures: dict.new(),
    linker_seeded: set.new(),
    module_slot_globals: False,
    with_stack: [],
  )
}

// ============================================================================
// Parse-time scope builder (V8 model)
//
// The PARSER threads a `ScopeBuilder` through the parse and records scope
// pushes, declarations, and unresolved references AS IT PARSES — replacing
// the post-parse DECLARE/REFERENCE AST walks below. Slots are NOT assigned
// here (the root's slot offset depends on `AnalyzeOpts` runtime state for
// direct-eval); `finalize` converts the builder's `RawScope`/`RawBinding`
// dicts into the slot-bearing `Scope`/`Binding` dicts the emitter consumes.
// ============================================================================

/// A declared name as recorded by the parser — same as `Binding` but
/// WITHOUT a slot (slots are assigned by `finalize` once `AnalyzeOpts`
/// is known). `index` is the per-scope declaration-order counter so
/// `finalize` can assign slots in the same order the legacy DECLARE pass
/// would have.
///
/// `synthetic` is True for names the parser inserts itself (the implicit
/// `arguments` placeholder, `*default*`, class-body helper consts, param
/// shims, …) rather than at a user declaration site. Its one consumer is
/// `sb_only_implicit_arguments`, which needs to tell the implicit
/// `arguments` VarBinding apart from a user-written `var arguments`.
pub type RawBinding {
  RawBinding(kind: BindingKind, synthetic: Bool, index: Int)
}

/// What syntactic construct caused a scope to be pushed. Recorded so the
/// parser can put `children_at` into HOIST order at each closing `}` (see
/// `sb_reorder_block_children` / `sb_reorder_body_children`) —
/// emit.gleam consumes `children_at` via positional cursors in EMISSION
/// order (FunctionDeclarations first, then everything else; switch case
/// tests between the two; class bodies in the 7-step `fold_class_body`
/// order), NOT source order, so the parser must reorder its source-order
/// child list to match before `finalize` hands the tree to emit.
/// `TagOther` is the default for every scope whose position is plain
/// source order within its parent.
pub type SourceTag {
  /// A Function-kind scope created for a (possibly labeled)
  /// `FunctionDeclaration` statement — hoisted to the FRONT of its
  /// enclosing block / function-body / switch-case-list's children by
  /// `sb_reorder_block_children`.
  TagFnDecl
  /// A scope (any kind) created while parsing a switch `case` test
  /// expression — emit_switch evaluates ALL case tests after the
  /// hoisted fn-decls but before any case body (see
  /// `sb_reorder_switch_children`).
  TagSwitchTest
  /// Default — function expressions, arrows, nested blocks, catch,
  /// with, for-heads, class expressions in non-class-body context.
  TagOther
}

/// A scope node as recorded by the parser — mirrors `Scope` but with
/// slot-free `RawBinding`s.
pub type RawScope {
  RawScope(
    id: ScopeId,
    parent: Option(ScopeId),
    function_scope: ScopeId,
    kind: ScopeKind,
    bindings: Dict(String, RawBinding),
    next_binding_index: Int,
    contains_direct_eval: Bool,
    annexb_blocked: Set(String),
    is_strict: Bool,
    catch_param_simple: Bool,
    /// What syntactic position pushed this scope — drives the
    /// `sb_reorder_*` partition at the enclosing scope's closing `}`.
    /// See `SourceTag`.
    source_tag: SourceTag,
    /// Names of `var` declarations that hoist THROUGH this scope on
    /// their way to the enclosing function scope (§8.2.6
    /// VarDeclaredNames recurses through blocks). Used solely for the
    /// §14.2.1 early error: a later `let`/`const` of the same name in
    /// THIS scope is a conflict — even though the var binding itself
    /// lives in `current_fn.bindings`, not here. NOT a binding;
    /// `finalize` ignores this field.
    hoisted_vars: Set(String),
    /// §10.2.11 step 28: count of `<paramN>` shims `sb_insert_param_shims`
    /// inserted at indices `0..count-1` for a NON-SIMPLE parameter list
    /// (0 = simple / not a function scope). The user formal names —
    /// recorded as ParamBinding during `parse_formal_parameters`, then
    /// shifted to indices `>= count` — must surface in the final
    /// ScopeTree as **LetBinding** so emit's `emit_binding_prologue`
    /// TDZ-seeds them with JsUninitialized (§10.2.11 step 28: non-simple
    /// parameter lists get let-like TDZ semantics for the user names).
    /// The rekind is deferred to `finalize_scope` (NOT done at
    /// parse time) because `sb_var_conflicts_lexical` must still treat
    /// the names as ParamBinding while the body is parsed: a body-level
    /// `var` of a destructured-formal name is legal (§14.3.2 checks
    /// VarDeclaredNames against the BODY's LexicallyDeclaredNames, not
    /// the parameter list's BoundNames).
    non_simple_shim_count: Int,
    /// §10.2.11 step 28: True for the Block scope `sb_push_var_boundary`
    /// creates for a function BODY whose parameter list is non-simple.
    /// It is the var sink for that body — `sb_var_target` stops here, so
    /// the body's VarDeclaredNames / hoisted FunctionDeclarations bind in
    /// THIS scope instead of the enclosing Function scope (which keeps the
    /// params, the `<paramN>` shims, `arguments`, and the NFE self-name).
    /// Closures created by parameter initializers are children of the
    /// Function scope and therefore can never see body declarations.
    /// Carried through to `Scope.is_var_boundary` so post-parse passes
    /// (`insert_captures`) can recognise the body sink without the
    /// positional "sole trailing block-kind child" heuristic emit uses.
    is_var_boundary: Bool,
  )
}

/// Per-function-scope facts the parser knows up front (arrow-ness,
/// Annex-B candidate list). Slot-bearing fields land in `FunctionInfo`
/// during `finalize`. Each Annex-B candidate carries the id of the
/// BLOCK scope that contains the FunctionDeclaration (V8's
/// `SloppyBlockFunctionStatement` records its scope) so
/// `hoist_annexb_block_functions` can walk block→fn-scope and decide
/// blocked-vs-hoisted once the whole body is known.
pub type RawFunctionInfo {
  RawFunctionInfo(is_arrow: Bool, annexb_candidates: List(#(ScopeId, String)))
}

/// Default `RawFunctionInfo` — non-arrow, no Annex-B candidates yet.
const blank_raw_fn_info = RawFunctionInfo(
  is_arrow: False,
  annexb_candidates: [],
)

/// A fresh `RawScope` with the 8 always-default fields filled in. The 5
/// per-site fields (id / parent / function_scope / kind / is_strict) are
/// arguments. Shared by `sb_init` and `sb_push` so the default-field
/// list lives in exactly one place.
fn new_raw_scope(
  id: ScopeId,
  parent: Option(ScopeId),
  function_scope: ScopeId,
  kind: ScopeKind,
  is_strict: Bool,
) -> RawScope {
  RawScope(
    id:,
    parent:,
    function_scope:,
    kind:,
    bindings: dict.new(),
    next_binding_index: 0,
    contains_direct_eval: False,
    annexb_blocked: set.new(),
    is_strict:,
    catch_param_simple: True,
    source_tag: TagOther,
    hoisted_vars: set.new(),
    non_simple_shim_count: 0,
    is_var_boundary: False,
  )
}

/// Threaded parse-time accumulator. `current` is the innermost scope the
/// parser is inside; `current_fn` is its nearest enclosing function-kind
/// scope (the var-hoist target). INVARIANT: `current_fn ==
/// sb_scope(sb, current).function_scope` — the pair is never set
/// independently, only by `sb_init` / `sb_push` / `sb_enter`, each of
/// which derives `current_fn` from the scope itself. Leaving a scope is
/// `sb_enter(sb, outer_id)`; accumulated `scopes`/`functions`/
/// `children_at`/`raw_refs` flow forward so an inner scope's declarations
/// survive the `}` that closes it.
pub type ScopeBuilder {
  ScopeBuilder(
    scopes: Dict(ScopeId, RawScope),
    functions: Dict(ScopeId, RawFunctionInfo),
    children_at: Dict(ScopeId, List(ScopeId)),
    next_id: Int,
    current: ScopeId,
    current_fn: ScopeId,
    raw_refs: List(#(ScopeId, String)),
    /// Subset of `raw_refs` that appear as an assignment TARGET (LHS of
    /// `=`/`op=`, `++`/`--`, for-in/of head, destructuring-assignment leaf).
    /// Drives capture-by-value: a captured binding never in this list can
    /// skip boxing (see `derive_vars_to_box`).
    assign_refs: List(#(ScopeId, String)),
    own_lexical_refs: Dict(ScopeId, LexicalRefs),
  )
}

/// Fresh builder rooted at scope id 0 of `root_kind` (Module or Script).
pub fn sb_init(root_kind: ScopeKind, strict: Bool) -> ScopeBuilder {
  let root =
    new_raw_scope(
      root_scope_id,
      None,
      root_scope_id,
      root_kind,
      strict || root_kind == Module,
    )
  ScopeBuilder(
    scopes: dict.from_list([#(root_scope_id, root)]),
    functions: dict.from_list([#(root_scope_id, blank_raw_fn_info)]),
    children_at: dict.from_list([#(root_scope_id, [])]),
    next_id: 1,
    current: root_scope_id,
    current_fn: root_scope_id,
    raw_refs: [],
    assign_refs: [],
    own_lexical_refs: dict.new(),
  )
}

/// Look up a `RawScope` by id. Internal — every id the parser holds was
/// minted by `sb_push`, so a miss is a bug and we say so loudly instead
/// of fabricating a wrong Block-kind orphan that would silently corrupt
/// resolution. Neither pruning nor discarding can trip this: both
/// `sb_prune_empty_block` and `sb_discard` deliberately KEEP the removed
/// scope in `sb.scopes` as a tombstone (only `children_at` is unlinked).
fn sb_scope(sb: ScopeBuilder, id: ScopeId) -> RawScope {
  let assert Ok(s) = dict.get(sb.scopes, id)
    as "scope.sb_scope: unknown ScopeId"
  s
}

/// Look up the `RawFunctionInfo` of a FUNCTION-kind scope. Internal —
/// `sb_init` seeds the root and `sb_push` seeds every function-kind scope
/// it mints, so a miss means the caller passed a non-function scope id (or
/// an id from another builder). Same rationale as `sb_scope`: fabricating a
/// `blank_raw_fn_info` here would silently drop `is_arrow` /
/// `annexb_candidates` and miscompile the body.
fn sb_fn_info(sb: ScopeBuilder, fn_id: ScopeId) -> RawFunctionInfo {
  let assert Ok(info) = dict.get(sb.functions, fn_id)
    as "scope.sb_fn_info: unknown function scope"
  info
}

/// Allocate and enter a fresh child scope of `kind` under `sb.current`.
/// Mirrors `new_scope` but records a `RawScope` (no slots) and a
/// `RawFunctionInfo` skeleton when `kind` is function-like. Returns the
/// new scope's id so the caller can stash it (the matching close is
/// `sb_enter(sb, outer_id)`).
pub fn sb_push(sb: ScopeBuilder, kind: ScopeKind) -> #(ScopeBuilder, ScopeId) {
  let id = sb.next_id
  let parent = sb_scope(sb, sb.current)
  let is_fn = is_function_kind(kind)
  let function_scope = case is_fn {
    True -> id
    False -> parent.function_scope
  }
  let is_strict = case kind {
    Module | ClassBody | ClassStaticBlock -> True
    Script | Function | Block | Catch | With(_) -> parent.is_strict
  }
  let node =
    new_raw_scope(id, Some(sb.current), function_scope, kind, is_strict)
  let functions = case is_fn {
    False -> sb.functions
    True -> dict.insert(sb.functions, id, blank_raw_fn_info)
  }
  let children_at =
    dict.upsert(sb.children_at, sb.current, fn(prev) {
      case prev {
        Some(l) -> [id, ..l]
        None -> [id]
      }
    })
    |> dict.insert(id, [])
  #(
    ScopeBuilder(
      ..sb,
      scopes: dict.insert(sb.scopes, id, node),
      functions:,
      children_at:,
      next_id: id + 1,
      current: id,
      current_fn: function_scope,
    ),
    id,
  )
}

/// §14.11: allocate and enter the `With` scope of a `with (o) …` statement,
/// minting its `<withN_M>` holder name and declaring the holder binding IN
/// the new scope. The ONLY way to build a `With` scope — the holder name
/// travels on the kind (`With(holder)`) and its binding is declared here, so
/// `do_lookup` / `inherited_with_slots` can never face a With scope whose
/// holder is missing.
///
/// The holder is a LetBinding so it lands in the With scope itself
/// (`sb_declare` routes VarBinding to `current_fn`, which would leave the
/// With scope's own bindings empty). Depth is counted BEFORE the push, so
/// the outermost `with` gets 0; the id is `sb.next_id`, the id `sb_push` is
/// about to mint.
pub fn sb_push_with(sb: ScopeBuilder) -> #(ScopeBuilder, ScopeId) {
  let holder = with_object_name(sb_with_depth(sb), sb.next_id)
  let #(sb, id) = sb_push(sb, With(holder:))
  #(sb_declare(sb, holder, LetBinding, synthetic: True), id)
}

/// §10.2.11 step 28: allocate and enter the var-boundary BODY scope of a
/// function whose parameter list is non-simple. A plain Block child of the
/// Function scope (same FunctionInfo slot space) marked `is_var_boundary`,
/// so `sb_var_target` routes the body's `var`s / hoisted function names
/// into it instead of the Function scope. Pushed AFTER the params, the
/// `<paramN>` shims, and the implicit `arguments` are declared — those
/// stay in the Function (parameter) scope.
pub fn sb_push_var_boundary(sb: ScopeBuilder) -> #(ScopeBuilder, ScopeId) {
  let #(sb, id) = sb_push(sb, Block)
  let scope = sb_scope(sb, id)
  #(
    ScopeBuilder(
      ..sb,
      scopes: dict.insert(
        sb.scopes,
        id,
        RawScope(..scope, is_var_boundary: True),
      ),
    ),
    id,
  )
}

/// Record `name` in the appropriate scope (var/fn-decl-sloppy → `current_fn`;
/// let/const/param/catch/class/fn-name → `current`). No-op when the target
/// scope already has a binding of that name (first-declaration-wins — the
/// invariant `finalize_scope` relies on when it skips seed-shadowing names).
pub fn sb_declare(
  sb: ScopeBuilder,
  name: String,
  kind: BindingKind,
  synthetic synthetic: Bool,
) -> ScopeBuilder {
  let target_id = case kind {
    VarBinding -> sb_var_target(sb)
    LetBinding
    | ConstBinding
    | ParamBinding
    | CatchBinding
    | CaptureBinding
    | FnNameBinding -> sb.current
  }
  sb_declare_in(sb, target_id, name, kind, synthetic:)
}

/// The scope a `var` (or hoisted function name) declared at `sb.current`
/// binds in: the innermost enclosing scope that is function-kind OR a
/// §10.2.11 step-28 var-boundary body scope. With no var-boundary on the
/// chain this is exactly `sb.current_fn` (the legacy behavior).
fn sb_var_target(sb: ScopeBuilder) -> ScopeId {
  use id, scope, acc <- sb_fold_up(
    sb,
    from: sb.current,
    stop_at_fn: True,
    init: sb.current_fn,
  )
  case is_function_kind(scope.kind) || scope.is_var_boundary {
    True -> list.Stop(id)
    False -> list.Continue(acc)
  }
}

/// Declare a `var` binding from inside an arbitrary block: the real
/// `VarBinding` lands in `current_fn` (via `sb_declare`), AND every
/// scope on the chain from `current` up to `current_fn` records `name`
/// in its `hoisted_vars` set so a later `let`/`const` of the same name
/// in any of those scopes raises the §14.2.1 LexicallyDeclaredNames-vs-
/// VarDeclaredNames early error. The walk is bounded by lexical nesting
/// depth (block/catch/with chain to the nearest function), same as V8's
/// `Scope::DeclareVariable` hoist loop.
pub fn sb_declare_var(
  sb: ScopeBuilder,
  name: String,
  synthetic synthetic: Bool,
) -> ScopeBuilder {
  let sb = sb_mark_hoisted_var(sb, sb.current, name)
  sb_declare(sb, name, VarBinding, synthetic:)
}

/// Fold `step` over the parent chain starting at `from`, visiting each
/// scope (inclusive) until `step` returns `Stop`, the chain ends, or —
/// when `stop_at_fn` — the visited scope is `current_fn`. The shared
/// recursion skeleton behind `sb_mark_hoisted_var`,
/// `sb_var_conflicts_lexical`, `sb_nearest_catch_params`, `sb_with_depth`.
fn sb_fold_up(
  sb: ScopeBuilder,
  from at: ScopeId,
  stop_at_fn stop_at_fn: Bool,
  init acc: a,
  step step: fn(ScopeId, RawScope, a) -> list.ContinueOrStop(a),
) -> a {
  let scope = sb_scope(sb, at)
  case step(at, scope, acc) {
    list.Stop(acc) -> acc
    list.Continue(acc) ->
      case stop_at_fn && at == sb.current_fn, scope.parent {
        False, Some(pid) -> sb_fold_up(sb, pid, stop_at_fn, acc, step)
        _, _ -> acc
      }
  }
}

fn sb_mark_hoisted_var(
  sb: ScopeBuilder,
  at: ScopeId,
  name: String,
) -> ScopeBuilder {
  use id, scope, sb <- sb_fold_up(sb, from: at, stop_at_fn: True, init: sb)
  let updated =
    ScopeBuilder(
      ..sb,
      scopes: dict.insert(
        sb.scopes,
        id,
        RawScope(..scope, hoisted_vars: set.insert(scope.hoisted_vars, name)),
      ),
    )
  // The var stops hoisting at its binding scope: the enclosing function,
  // or a §10.2.11 step-28 var-boundary body scope (`sb_var_target`).
  // Inclusive — the target scope's own `hoisted_vars` records the name.
  case is_function_kind(scope.kind) || scope.is_var_boundary {
    True -> list.Stop(updated)
    False -> list.Continue(updated)
  }
}

/// Record an unresolved identifier reference at the current scope.
pub fn sb_ref(sb: ScopeBuilder, name: String) -> ScopeBuilder {
  ScopeBuilder(..sb, raw_refs: [#(sb.current, name), ..sb.raw_refs])
}

/// Record that `name` is an assignment TARGET at the current scope (LHS of
/// `=`/`op=`, `++`/`--`, for-in/of head, destructuring-assignment leaf).
/// The parser has already `sb_ref`'d the name; this marks it mutable so
/// `derive_vars_to_box` can skip boxing never-reassigned captures.
pub fn sb_assign_ref(sb: ScopeBuilder, name: String) -> ScopeBuilder {
  ScopeBuilder(..sb, assign_refs: [#(sb.current, name), ..sb.assign_refs])
}

/// Record a lexical pseudo-reference (this/super/new.target/active-func)
/// against the current FUNCTION scope.
pub fn sb_lexical_ref(sb: ScopeBuilder, ref: LexicalRef) -> ScopeBuilder {
  let own_lexical_refs =
    dict.upsert(sb.own_lexical_refs, sb.current_fn, fn(prev) {
      let prev = option.unwrap(prev, lexical.no_lexical_refs)
      case ref {
        RefThis -> lexical.LexicalRefs(..prev, this: True)
        RefActiveFunc -> lexical.LexicalRefs(..prev, active_func: True)
        RefHomeObject -> lexical.LexicalRefs(..prev, home_object: True)
        RefNewTarget -> lexical.LexicalRefs(..prev, new_target: True)
      }
    })
  ScopeBuilder(..sb, own_lexical_refs:)
}

/// Mark the current scope as containing a syntactic direct-eval call.
pub fn sb_mark_eval(sb: ScopeBuilder) -> ScopeBuilder {
  sb_update_current(sb, fn(s) { RawScope(..s, contains_direct_eval: True) })
}

/// Overwrite `children_at[parent_id]` with a caller-supplied ordering
/// (used by the parser's hoist-reorder step at each closing `}`).
pub fn sb_set_children(
  sb: ScopeBuilder,
  parent_id: ScopeId,
  ordered: List(ScopeId),
) -> ScopeBuilder {
  ScopeBuilder(
    ..sb,
    children_at: dict.insert(sb.children_at, parent_id, ordered),
  )
}

/// Move the cursor to an already-pushed scope `id`, deriving `current_fn`
/// from that scope's own `function_scope` so the two can never desync (a
/// desynced pair would route `sb_lexical_ref` into the wrong function's
/// `own_lexical_refs` and silently drop, e.g., an arrow's `this` capture).
///
/// This is BOTH primitives:
///   * leaving a scope at its closing `}` — `sb_enter(sb, outer_id)`;
///   * re-entering a pre-created synthetic scope (the per-class
///     instance-init / static-init shells) so scopes pushed while parsing
///     a class field's initializer parent under that shell, matching
///     `declare_class`, without allocating a fresh scope.
///
/// Accumulated `scopes`/`functions`/`children_at`/`raw_refs` flow forward
/// unchanged, so a closed scope's declarations and refs persist for
/// `finalize`. Does NOT touch `children_at`.
pub fn sb_enter(sb: ScopeBuilder, id: ScopeId) -> ScopeBuilder {
  let scope = sb_scope(sb, id)
  ScopeBuilder(..sb, current: id, current_fn: scope.function_scope)
}

/// Direct children of `id` in storage order (newest first — `sb_push`
/// prepends). The caller reverses for source order. Exposed so the
/// parser's class-body close can snapshot-diff which child scope ids a
/// given class element introduced.
pub fn sb_children_raw(sb: ScopeBuilder, id: ScopeId) -> List(ScopeId) {
  dict.get(sb.children_at, id) |> result.unwrap([])
}

/// Declare `name` directly into `scope_id` regardless of `kind`'s normal
/// var/lexical routing. Used for the per-class synthetic ConstBindings
/// (`ast_util.class_body_bindings`) which all land in the ClassBody scope
/// even though `sb.current` may be elsewhere at the time of the call.
pub fn sb_declare_in(
  sb: ScopeBuilder,
  scope_id: ScopeId,
  name: String,
  kind: BindingKind,
  synthetic synthetic: Bool,
) -> ScopeBuilder {
  let scope = sb_scope(sb, scope_id)
  case dict.has_key(scope.bindings, name) {
    True -> sb
    False -> {
      let idx = scope.next_binding_index
      let updated =
        RawScope(
          ..scope,
          bindings: dict.insert(
            scope.bindings,
            name,
            RawBinding(kind:, synthetic:, index: idx),
          ),
          next_binding_index: idx + 1,
        )
      ScopeBuilder(..sb, scopes: dict.insert(sb.scopes, scope_id, updated))
    }
  }
}

/// Retrofit the current function scope for a NON-SIMPLE parameter list
/// (§10.2.11 step 28): insert `<param0>`..`<param{count-1}>` shim
/// ParamBindings at indices `0..count-1`, and shift every binding the
/// parser already declared during `parse_formal_parameters` (the
/// destructured / defaulted / rest-target names) past the shims. The
/// runtime calling convention writes positional arg N into the Nth
/// declared slot (frame.gleam:57 / arc_vm_ffi:setup_locals_tuple), so
/// the shims must own indices 0..count-1; emit.compile_function_body
/// then reads each shim by name and runs `emit_destructuring_bind`
/// into the user names. The shims are contiguous at the front (rather
/// than interleaved per-param) — the exact ordering is irrelevant
/// because emit resolves every slot by name via `scope.lookup`; only
/// "shims own indices 0..count-1" matters.
pub fn sb_insert_param_shims(sb: ScopeBuilder, count: Int) -> ScopeBuilder {
  use <- bool.guard(count <= 0, sb)
  let fn_id = sb.current_fn
  let scope = sb_scope(sb, fn_id)
  // Every binding present at this point is a formal-parameter name
  // (the function scope was freshly pushed by enter_function_context /
  // enter_arrow_context immediately before param parsing). Shift past
  // the shims so the runtime-written args land in the shims, not the
  // user names. Kind stays ParamBinding: a body-level `var` of the
  // same name is legal (`function f([b]) { var b; }` — §15.1.1
  // VarDeclaredNames are checked against the BODY's
  // LexicallyDeclaredNames, not the parameter list's BoundNames),
  // and `sb_var_conflicts_lexical` would wrongly reject it if these
  // were rekinded to LetBinding here.
  let shifted =
    dict.map_values(scope.bindings, fn(_name, rb) {
      RawBinding(..rb, index: rb.index + count)
    })
  let with_shims = insert_param_shims_loop(shifted, 0, count)
  let scope =
    RawScope(
      ..scope,
      bindings: with_shims,
      next_binding_index: scope.next_binding_index + count,
      non_simple_shim_count: count,
    )
  ScopeBuilder(..sb, scopes: dict.insert(sb.scopes, fn_id, scope))
}

fn insert_param_shims_loop(
  bindings: Dict(String, RawBinding),
  i: Int,
  count: Int,
) -> Dict(String, RawBinding) {
  case i >= count {
    True -> bindings
    False ->
      insert_param_shims_loop(
        dict.insert(
          bindings,
          param_shim(i),
          RawBinding(kind: ParamBinding, synthetic: True, index: i),
        ),
        i + 1,
        count,
      )
  }
}

/// Unlink `id` from the tree: drop it from its parent's `children_at` and
/// from `functions`/`own_lexical_refs`, so `finalize` never visits it.
/// Used to discard the pre-created class instance-init / static-init
/// shells when the class body had no instance fields/private methods
/// (resp. no static elements), so `declare_class` would not have created
/// the shell. Caller guarantees `id` has no children.
///
/// The `RawScope` STAYS in `sb.scopes` as a tombstone, exactly as
/// `sb_prune_empty_block` leaves pruned blocks: `sb.scopes` is the
/// address space every id the parser ever minted resolves through
/// (`sb_scope`, `resolve_raw_refs`), and deleting from it would make an
/// id the parser still holds dangle. The shell has no bindings and no
/// refs, so keeping it is invisible to resolution.
pub fn sb_discard(sb: ScopeBuilder, id: ScopeId) -> ScopeBuilder {
  let scope = sb_scope(sb, id)
  let children_at = case scope.parent {
    Some(pid) -> {
      let siblings = sb_children_raw(sb, pid)
      dict.insert(sb.children_at, pid, list.filter(siblings, fn(c) { c != id }))
    }
    None -> sb.children_at
  }
  ScopeBuilder(
    ..sb,
    functions: dict.delete(sb.functions, id),
    children_at: dict.delete(children_at, id),
    own_lexical_refs: dict.delete(sb.own_lexical_refs, id),
  )
}

/// Whether `scope` is a Block that `sb_prune_empty_block` may splice
/// out. THE single definition — `sb_close_block` and
/// `sb_prune_empty_block` must agree on it, and it MUST match
/// emit.gleam's `ast_util.block_has_declarations` (which knows nothing
/// about direct-eval) so the parser-built tree and emit's per-block
/// scope_cursor stay in lockstep. A declaration-free block has no
/// bindings by construction. The non-simple-params BODY block
/// (`is_var_boundary`) is never pruned — emit locates it positionally
/// as the function root's sole trailing block-kind child.
fn sb_block_prunable(scope: RawScope) -> Bool {
  scope.kind == Block && dict.is_empty(scope.bindings) && !scope.is_var_boundary
}

/// V8 `Scope::FinalizeBlockScope`: if `id` is a Block scope with no
/// bindings, splice it out of `children_at` — reparent its children to its
/// grandparent (preserving order, replacing `id`'s slot in the grandparent's
/// child list). The `RawScope` STAYS in `sb.scopes` as a tombstone (see the
/// PERF note below and `sb_discard`): `sb.scopes` is the address space every
/// id the parser ever minted resolves through, so deleting from it would
/// dangle refs still keyed to `id`.
/// Keeps emit.gleam's `ast_util.block_has_declarations` elision in lockstep
/// with the scope tree.
pub fn sb_prune_empty_block(sb: ScopeBuilder, id: ScopeId) -> ScopeBuilder {
  let scope = sb_scope(sb, id)
  case sb_block_prunable(scope), scope.parent {
    True, Some(parent_id) -> {
      // A direct eval recorded against the pruned block must survive the
      // prune or the enclosing function never learns it contains an eval
      // (no force-capture of enclosing bindings, no `local_names` table →
      // `eval()` silently degrades to INDIRECT eval). The pruned block has
      // no bindings, so attributing the eval to the parent scope is
      // observationally identical — V8's FinalizeBlockScope does the same
      // (`if (calls_eval()) outer_scope()->RecordEvalCall()`).
      let sb = case scope.contains_direct_eval {
        False -> sb
        True -> {
          let parent = sb_scope(sb, parent_id)
          ScopeBuilder(
            ..sb,
            scopes: dict.insert(
              sb.scopes,
              parent_id,
              RawScope(..parent, contains_direct_eval: True),
            ),
          )
        }
      }
      let own_children = sb_children_raw(sb, id) |> list.reverse
      // Reparent: replace `id` in parent's (reverse-order) child list with
      // `id`'s own children, and rewrite each child's `parent` pointer.
      let parent_children = sb_children_raw(sb, parent_id)
      let spliced =
        list.flat_map(parent_children, fn(c) {
          case c == id {
            True -> list.reverse(own_children)
            False -> [c]
          }
        })
      // PERF: do NOT `dict.delete(sb.scopes, id)` and do NOT walk
      // `sb.raw_refs` to remap refs from `id` → `parent_id`. `raw_refs`
      // is the global prepend-only list of every identifier seen so
      // far; remapping it on every empty-block close is O(P×R) → O(N²)
      // on block-heavy inputs. Instead leave the pruned `RawScope` in
      // `sb.scopes` as a tombstone (it has the correct `function_scope`
      // and `parent` for `resolve_raw_refs` to step through at finalize
      // time) and only splice it out of `children_at` so
      // `finalize_scope` never visits it. The block has no bindings by
      // construction, so resolution-through-it is identical to
      // resolution-from-its-parent.
      let scopes =
        list.fold(own_children, sb.scopes, fn(acc, child_id) {
          let child = sb_scope(sb, child_id)
          dict.insert(acc, child_id, RawScope(..child, parent: Some(parent_id)))
        })
      ScopeBuilder(
        ..sb,
        scopes:,
        children_at: sb.children_at
          |> dict.insert(parent_id, spliced)
          |> dict.delete(id),
      )
    }
    _, _ -> sb
  }
}

// --- children_at hoist-reorder (Option B) ---------------------------------
// emit.gleam is FROZEN and consumes `children_at` via two positional
// cursors in EMISSION order — `child_fn_cursor` pops function-kind
// children in `collect_hoisted_funcs` order (FunctionDeclarations first,
// then fn-exprs/arrows/classes in source order), and class bodies pop in
// the 7-step `fold_class_body` order. The parser pushes children in SOURCE
// order (prepended → reverse-source in `children_at`); these helpers
// rewrite each scope's `children_at` entry to emission order at the
// closing `}` — the same fn-decls-first / switch case-tests-between /
// class 7-step order emit.gleam's `collect_hoisted_funcs`, `emit_switch`
// and `compile_class_body` consume — so emit's cursors stay in lockstep
// without an AST re-walk.

/// Set the `source_tag` on an already-pushed scope. Called by the parser
/// immediately after `sb_push` when the syntactic context is known
/// (FunctionDeclaration vs FunctionExpression, class-body step, etc.).
pub fn sb_set_source_tag(
  sb: ScopeBuilder,
  id: ScopeId,
  tag: SourceTag,
) -> ScopeBuilder {
  let scope = sb_scope(sb, id)
  ScopeBuilder(
    ..sb,
    scopes: dict.insert(sb.scopes, id, RawScope(..scope, source_tag: tag)),
  )
}

/// Tag every DIRECT child of `parent_id` that was pushed since `marker`
/// was captured (via `sb_children_raw`) with `tag`. `children_at` is
/// prepend-only, so the new children are exactly the prefix of the
/// current list before `marker` begins.
pub fn sb_tag_children_since(
  sb: ScopeBuilder,
  parent_id: ScopeId,
  marker: List(ScopeId),
  tag: SourceTag,
) -> ScopeBuilder {
  let now = sb_children_raw(sb, parent_id)
  let new_count = list.length(now) - list.length(marker)
  use <- bool.guard(new_count <= 0, sb)
  let new_ids = list.take(now, new_count)
  list.fold(new_ids, sb, fn(sb, id) { sb_set_source_tag(sb, id, tag) })
}

/// `source_tag` of `id`. Panics on an unknown id (see `sb_scope`) — a
/// fabricated `TagOther` here would silently reorder `children_at` out of
/// lockstep with emit.gleam's cursors.
fn sb_tag_of(sb: ScopeBuilder, id: ScopeId) -> SourceTag {
  sb_scope(sb, id).source_tag
}

/// Reorder `children_at[scope_id]` from reverse-source-order (as left by
/// `sb_push` prepends) into HOIST order — the order emit.gleam's
/// `collect_hoisted_funcs` consumes: every `TagFnDecl` child first
/// (source-ordered among themselves), then every other child
/// (source-ordered among themselves). Idempotent on an already-reordered
/// list whose children carry the same tags.
///
/// Called at each block / function-body / module-body closing `}` for
/// scopes that are NOT pruned. Pruned (declaration-free) blocks keep
/// their children in reverse-source order so `sb_prune_empty_block` can
/// splice them into the still-accumulating parent list — see
/// `sb_close_block`.
pub fn sb_reorder_block_children(
  sb: ScopeBuilder,
  scope_id: ScopeId,
) -> ScopeBuilder {
  sb_reorder_body_children(sb, scope_id, [])
}

/// Reorder ONLY the children of `scope_id` pushed since `marker` was
/// captured (via `sb_children_marker`) into hoist order, leaving older
/// children (those in `marker`) at the front in source order. Used at
/// function-body / catch-body close where param default-expression
/// scopes are pushed BEFORE the body and emit consumes them before any
/// hoisted body FunctionDeclaration (`compile_function_body` walks the
/// param-default child scopes first, then the hoisted fn-decls).
pub fn sb_reorder_body_children(
  sb: ScopeBuilder,
  scope_id: ScopeId,
  marker: List(ScopeId),
) -> ScopeBuilder {
  let rev = sb_children_raw(sb, scope_id)
  use <- bool.guard(rev == [], sb)
  let body_count = list.length(rev) - list.length(marker)
  // Pre-body children (param defaults, catch-param destructuring
  // defaults) keep source-order position at the front; only body
  // children are hoist-partitioned.
  let body_src = list.take(rev, body_count) |> list.reverse
  let pre_body = list.reverse(marker)
  let #(fn_decls, rest) =
    list.partition(body_src, fn(id) { sb_tag_of(sb, id) == TagFnDecl })
  sb_set_children(
    sb,
    scope_id,
    list.append(pre_body, list.append(fn_decls, rest)),
  )
}

/// Reorder `children_at[switch_id]` into the order `emit_switch` consumes
/// (§14.12.4: a switch's CaseBlock is ONE block scope):
/// `[fn-decls from all case bodies] ++ [scopes from all case tests] ++
/// [non-fn-decl scopes from all case bodies]`, each group internally
/// source-ordered. Requires the parser to have tagged test-expression
/// children with `TagSwitchTest` (via `sb_tag_children_since`) and
/// FunctionDeclaration children with `TagFnDecl`.
pub fn sb_reorder_switch_children(
  sb: ScopeBuilder,
  switch_id: ScopeId,
) -> ScopeBuilder {
  let rev = sb_children_raw(sb, switch_id)
  use <- bool.guard(rev == [], sb)
  let src_order = list.reverse(rev)
  let #(fn_decls, non_decl) =
    list.partition(src_order, fn(id) { sb_tag_of(sb, id) == TagFnDecl })
  let #(tests, rest) =
    list.partition(non_decl, fn(id) { sb_tag_of(sb, id) == TagSwitchTest })
  sb_set_children(
    sb,
    switch_id,
    list.append(fn_decls, list.append(tests, rest)),
  )
}

/// Close a `{ ... }` block scope: EITHER prune it (declaration-free
/// block — emit_block applies the same `block_has_declarations` elision,
/// so the tree must drop the node and reparent its children) OR reorder
/// its children to hoist order. Never both: a pruned block's children
/// must stay in reverse-source order so `sb_prune_empty_block` can splice
/// them into the parent's still-accumulating reverse-source list, and a
/// declaration-free block contains no FunctionDeclarations by definition
/// so hoist-reorder is unnecessary. Returns the ScopeBuilder still
/// positioned INSIDE `block_id` — caller follows with `sb_enter`.
pub fn sb_close_block(sb: ScopeBuilder, block_id: ScopeId) -> ScopeBuilder {
  case sb_block_prunable(sb_scope(sb, block_id)) {
    True -> sb_prune_empty_block(sb, block_id)
    False -> sb_reorder_block_children(sb, block_id)
  }
}

/// Update a field on the current `RawScope` — used for With-object name,
/// Catch-param-simple, and post-`"use strict"` upgrade.
pub fn sb_update_current(
  sb: ScopeBuilder,
  f: fn(RawScope) -> RawScope,
) -> ScopeBuilder {
  let scope = sb_scope(sb, sb.current)
  ScopeBuilder(..sb, scopes: dict.insert(sb.scopes, sb.current, f(scope)))
}

/// Update the current FUNCTION scope's `RawFunctionInfo` — used to set
/// `is_arrow` and append `annexb_candidates`.
pub fn sb_update_current_fn(
  sb: ScopeBuilder,
  f: fn(RawFunctionInfo) -> RawFunctionInfo,
) -> ScopeBuilder {
  ScopeBuilder(
    ..sb,
    functions: dict.insert(
      sb.functions,
      sb.current_fn,
      f(sb_fn_info(sb, sb.current_fn)),
    ),
  )
}

/// §B.3.2: record a sloppy-mode block-level FunctionDeclaration as an
/// Annex-B var-hoist candidate on the enclosing FUNCTION scope, paired
/// with `sb.current` — the BLOCK scope id that holds the declaration's
/// own LetBinding. `hoist_annexb_block_functions` walks from that block
/// up to the function root after the whole body is parsed (V8's
/// `DeclarationScope::HoistSloppyBlockFunctions`) to decide whether an
/// intermediate let/const/class blocks the var-twin.
pub fn sb_annexb_candidate(sb: ScopeBuilder, name: String) -> ScopeBuilder {
  sb_update_current_fn(sb, fn(fi) {
    RawFunctionInfo(..fi, annexb_candidates: [
      #(sb.current, name),
      ..fi.annexb_candidates
    ])
  })
}

// --- Read-side helpers for the parser's early-error checks ---------------
// These replace the parser's old per-block `Set(String)` membership tests.

/// True when `kind` is a let/const/class binding — i.e. one that makes a
/// same-named `var` in its subtree a §14.3.2 early error. FnNameBinding is
/// NOT lexical for this purpose: `(function foo(){ var foo })()` is valid.
fn is_lexical_kind(kind: BindingKind) -> Bool {
  case kind {
    LetBinding | ConstBinding -> True
    VarBinding | ParamBinding | CatchBinding | CaptureBinding | FnNameBinding ->
      False
  }
}

/// A `let`/`const` of `name` in the current scope would be a §14.2.1
/// duplicate: either `name` is already bound in this scope (lexical,
/// param, catch — any kind) OR a `var name` declared anywhere in this
/// scope's subtree has already hoisted THROUGH it (the var lives in
/// the var-target scope's `bindings`, not here, so a plain has-key is
/// insufficient) OR — when this scope is a §10.2.11 step-28 var-boundary
/// BODY scope — `name` is a formal parameter of the enclosing function
/// (§15.2.1: BoundNames of FormalParameters must not intersect the
/// body's LexicallyDeclaredNames; the params live one scope up).
pub fn sb_lexical_conflict(sb: ScopeBuilder, name: String) -> Bool {
  let scope = sb_scope(sb, sb.current)
  dict.has_key(scope.bindings, name)
  || set.contains(scope.hoisted_vars, name)
  || sb_boundary_param_conflict(sb, scope, name)
}

/// A lexical name declared at the top level of a parameter-scope's BODY
/// block must not redeclare a binding of the parameter scope one level up:
/// - §15.2.1: BoundNames of FormalParameters ∩ LexicallyDeclaredNames of a
///   function body (the §10.2.11 step-28 var-boundary body scope);
/// - §14.15.1 clause 2: BoundNames of CatchParameter ∩
///   LexicallyDeclaredNames of the catch Block (Annex B §B.3.4 relaxes
///   only the VarDeclaredNames clause, never this one).
/// Both bodies are Block scopes whose DIRECT parent is the parameter
/// scope, so nested blocks (`function f(a=1){ { let a } }`,
/// `catch(e){ { let e } }`) never trip it. Restricting the parent binding
/// kinds is essential: the implicit `arguments` (VarBinding) must stay
/// redeclarable by a body-top-level `let arguments` in sloppy code, and
/// the NFE self-name (FnNameBinding) is not declared until after the body
/// is parsed.
fn sb_boundary_param_conflict(
  sb: ScopeBuilder,
  scope: RawScope,
  name: String,
) -> Bool {
  let param_scope = case scope.kind, scope.parent {
    Block, Some(parent_id) ->
      case scope.is_var_boundary || sb_scope(sb, parent_id).kind == Catch {
        True -> Some(parent_id)
        False -> None
      }
    _, _ -> None
  }
  case param_scope {
    None -> False
    Some(parent_id) ->
      case dict.get(sb_scope(sb, parent_id).bindings, name) {
        Ok(rb) -> rb.kind == ParamBinding || rb.kind == CatchBinding
        Error(Nil) -> False
      }
  }
}

/// True when the only thing in the current scope blocking a
/// `let arguments` is the implicit `arguments` placeholder the parser
/// records BEFORE parsing a function body (a synthetic VarBinding).
/// Because that implicit binding is recorded first and `sb_declare` is
/// first-declaration-wins, a later `let arguments` is a silent no-op
/// leaving kind=VarBinding — never an early error (§10.2.11 step 18:
/// the implicit `arguments` binding pre-exists any body declaration).
/// The parser uses this to exempt that case from
/// `sb_lexical_conflict`. A user `var arguments` /
/// `function arguments(){}` at fn top-level still trips
/// `hoisted_vars` (via `sb_declare_var`), and a param named
/// `arguments` is ParamBinding, so neither is exempted here.
pub fn sb_only_implicit_arguments(sb: ScopeBuilder, name: String) -> Bool {
  use <- bool.guard(name != "arguments", False)
  let scope = sb_scope(sb, sb.current)
  use <- bool.guard(set.contains(scope.hoisted_vars, name), False)
  case dict.get(scope.bindings, name) {
    Ok(RawBinding(kind: VarBinding, synthetic: True, ..)) -> True
    Ok(_) | Error(Nil) -> False
  }
}

/// `name` is bound in the CURRENT scope and that binding's kind is `kind`.
pub fn sb_current_has_kind(
  sb: ScopeBuilder,
  name: String,
  kind: BindingKind,
) -> Bool {
  case dict.get(sb_scope(sb, sb.current).bindings, name) {
    Ok(rb) -> rb.kind == kind
    Error(Nil) -> False
  }
}

/// Walk the parent chain from `current` up to AND INCLUDING `current_fn`,
/// returning True if any scope on that chain holds a lexical-kind
/// (let/const/class) binding of `name`. Replaces the parser's flattened
/// `outer_lexical` Set for the `var`-vs-enclosing-`let` early error.
pub fn sb_var_conflicts_lexical(sb: ScopeBuilder, name: String) -> Bool {
  use _id, scope, _acc <- sb_fold_up(sb, sb.current, True, False)
  let hit = case dict.get(scope.bindings, name) {
    Ok(rb) -> is_lexical_kind(rb.kind)
    Error(Nil) -> False
  }
  case hit {
    True -> list.Stop(True)
    False -> list.Continue(False)
  }
}

/// §16.2.1.1: a `var name` whose hoist target is the Module root
/// conflicts with a module-top-level HoistableDeclaration
/// (`function`/`async function`/generator). The parser records those as
/// VarBinding (top-level function declarations are var-like everywhere
/// BUT a Module root), so `sb_var_conflicts_lexical` misses them —
/// yet per spec they are LexicallyDeclaredNames at module top.
/// Discriminator: a module-top function decl is recorded via plain
/// `sb_declare` (bindings only); a `var` via `sb_declare_var`
/// (bindings + hoisted_vars). So "in root bindings, not in root
/// hoisted_vars" = LexicallyDeclaredName, and a `var` of that name is
/// the §16.2.1.1 LexicallyDeclaredNames ∩ VarDeclaredNames early error.
pub fn sb_var_conflicts_module_fn(sb: ScopeBuilder, name: String) -> Bool {
  use <- bool.guard(sb.current_fn != root_scope_id, False)
  let root = sb_scope(sb, root_scope_id)
  use <- bool.guard(root.kind != Module, False)
  dict.has_key(root.bindings, name) && !set.contains(root.hoisted_vars, name)
}

/// `name` is bound in the ROOT scope (id 0) — used by module export
/// validation.
pub fn sb_root_has(sb: ScopeBuilder, name: String) -> Bool {
  dict.has_key(sb_scope(sb, root_scope_id).bindings, name)
}

/// ParamBinding names of the nearest enclosing `Catch` scope on the
/// chain from `current` up to and including `current_fn`, else `[]`.
/// Replaces the parser's old flat `scope_params` Set: that set was
/// populated on catch-clause entry, persisted through nested blocks,
/// and reset on function entry — which is exactly this walk's scope.
pub fn sb_nearest_catch_params(sb: ScopeBuilder) -> List(String) {
  use _id, scope, _acc <- sb_fold_up(sb, sb.current, True, [])
  case scope.kind {
    Catch ->
      list.Stop({
        use #(name, rb) <- list.filter_map(dict.to_list(scope.bindings))
        case rb.kind {
          ParamBinding | CatchBinding -> Ok(name)
          _ -> Error(Nil)
        }
      })
    _ -> list.Continue([])
  }
}

/// Count of `With` scopes on the parent chain from `current` to the
/// root — the `N` in the `<withN_M>` synthetic holder name. Builder-side
/// mirror of `with_depth_at` (which walks to `parent: None`, crossing
/// function boundaries so a `with` inside a function nested in another
/// `with` still gets depth ≥ 1).
pub fn sb_with_depth(sb: ScopeBuilder) -> Int {
  use _id, scope, acc <- sb_fold_up(sb, sb.current, False, 0)
  list.Continue(case scope.kind {
    With(_) -> acc + 1
    _ -> acc
  })
}

// ============================================================================
// Finalize: ScopeBuilder → ScopeTree
//
// Converts the parser's slot-free `ScopeBuilder` into the slot-bearing
// `ScopeTree` the emitter consumes. Runs ZERO AST reads — every datum
// comes from the builder + `AnalyzeOpts`. The parser has already threaded
// `sb` through every scope-introducing construct, so by the time
// `finalize` runs, declarations / raw refs / `children_at` are all
// recorded in the exact order the emitter will consume them in.
//
// Steps (DECLARE → REFERENCE → CAPTURE):
//   (a) seed the root scope/FunctionInfo from `opts` — direct-eval
//       caller-frame state (`parent_names`, `with_stack`,
//       `lexical_captures`, `strict`) is runtime, unknowable at parse;
//   (b) DECLARE — `finalize_scope`: walk the scope tree pre-order via
//       `children_at`, converting each `RawScope` → `Scope` by assigning
//       `slot = local_count++` to its `RawBinding`s in `index` order —
//       the parser controls slot order by controlling `sb_declare` call
//       order;
//   (c) REFERENCE — `resolve_raw_refs`: fold the flat `raw_refs` list
//       into the per-function free-name set `captured`;
//   (d) CAPTURE — hand `captured` + `sb.own_lexical_refs` to
//       `analyze_captures`, which fills in boxing / capture lists /
//       lexical-slot allocation.
// ============================================================================

/// Threaded state for `finalize`'s pre-order slot-allocation walk.
type FinSt {
  FinSt(scopes: Dict(ScopeId, Scope), functions: Dict(ScopeId, FunctionInfo))
}

/// A fresh `FunctionInfo` with every field at its empty default,
/// pulling `is_arrow` from the parser's `RawFunctionInfo`. The filtered
/// `annexb_candidates` list is populated by
/// `hoist_annexb_block_functions` AFTER `finalize_scope`. `finalize`
/// overrides the root-only fields via record spread; `finalize_scope`
/// uses it as-is for child functions.
fn blank_function_info(
  raw: RawFunctionInfo,
  fallthrough: GlobalFallthrough,
) -> FunctionInfo {
  FunctionInfo(
    local_count: 0,
    lexical: lexical.NoLexicalSlots,
    lexical_boxed: lexical.no_lexical_refs,
    captures: [],
    lexical_captures: dict.new(),
    names: dict.new(),
    fallthrough:,
    contains_direct_eval: False,
    eval_in_subtree: False,
    // Filled (filtered) by `hoist_annexb_block_functions` AFTER
    // `finalize_scope` — the raw list carries `#(block_id, name)`
    // pairs, the cooked list is just the unblocked names.
    annexb_candidates: [],
    is_arrow: raw.is_arrow,
  )
}

/// Convert a parse-time `ScopeBuilder` into the emitter's `ScopeTree`.
/// Scope-tree-only — reads NO AST. This is the ONLY constructor of a
/// `ScopeTree`; every compile entry point (compile_script / compile_module
/// / compile_eval_direct / compile_function_body) goes through it.
pub fn finalize(sb: ScopeBuilder, opts: AnalyzeOpts) -> ScopeTree {
  // --- (a) seed root from opts --------------------------------------------
  let root_raw = sb_scope(sb, root_scope_id)
  // Module imports arrive pre-boxed and pre-filled by the linker, so they
  // are seeded as `kind: CaptureBinding` — emit_binding_prologue's
  // CaptureBinding arm does NOTHING (no JsUninitialized seed, no IrBoxLocal),
  // letting the linker's value survive. `origin_kind_for_capture: ConstBinding`
  // preserves §16.2.1.5.5 immutability (SetMutableBinding on a Module
  // Environment Record throws TypeError) via Resolution.Local.origin_kind.
  // Direct-eval inherits the caller's locals as mutable boxed captures so
  // the eval'd body can read AND write them via PutBoxed.
  let #(parent_kind, parent_origin) = case root_raw.kind {
    Module -> #(CaptureBinding, ConstBinding)
    Script
    | Function
    | Block
    | Catch
    | With(_)
    | ClassBody
    | ClassStaticBlock -> #(CaptureBinding, CaptureBinding)
  }
  let parent_bindings =
    dict.map_values(opts.parent_names, fn(_name, slot) {
      Binding(
        slot:,
        kind: parent_kind,
        is_boxed: True,
        origin_kind_for_capture: parent_origin,
      )
    })
  let root_raw_fn = sb_fn_info(sb, root_scope_id)
  // Root local_count starts past the seeded captures + inherited lexical
  // pseudo-slots — the captures-first frame layout `do_lookup` /
  // `child_parent_view` assume — so user bindings allocate from the first
  // free slot after them. `opts.with_stack` is NOT added: its entries are
  // slot indices INTO `opts.parent_names` (every with-holder is one of the
  // caller's locals), so they are already counted.
  let root_base =
    dict.size(opts.parent_names) + dict.size(opts.lexical_captures)
  // A non-direct-eval Script root OWNS all four lexical pseudo-slots
  // (this/active_func/home_object/new.target) — the runtime call
  // prologue seeds them at frame entry (interpreter.gleam:428-437),
  // so `this` at script top-level resolves to globalThis. Module
  // roots stay non-owners (`this === undefined` per §16.2.1.6.4);
  // direct-eval Script roots inherit the CALLER's environment and so
  // are non-owners too. The owner test is `root_base == 0` — NO
  // inherited parent_names/lexical_captures — NOT
  // `dict.is_empty(lexical_captures)` alone: a direct-eval whose
  // caller has locals but `func.lexical == NoLexicalSlots` (e.g. an
  // arrow at Module top-level) arrives with empty lexical_captures and
  // non-empty parent_names, and treating it as an owner would assign
  // `this -> slot root_base` here while compute_down recomputes from
  // lex_base=0, leaking a parent box ref as `this`.
  let script_root_owns_lexical = root_raw.kind == Script && root_base == 0
  let #(root_local_count, root_lexical) = case script_root_owns_lexical {
    True -> #(
      root_base + lexical.owned_lexical_slot_count,
      lexical.OwnedLexicalSlots(base: root_base),
    )
    False -> #(root_base, lexical.NoLexicalSlots)
  }
  let root_fn =
    FunctionInfo(
      ..blank_function_info(root_raw_fn, opts.fallthrough),
      local_count: root_local_count,
      lexical: root_lexical,
      lexical_captures: opts.lexical_captures,
      names: opts.parent_names,
    )
  // --- (b) pre-order slot allocation: RawScope → Scope --------------------
  let st =
    FinSt(
      scopes: dict.new(),
      functions: dict.from_list([#(root_scope_id, root_fn)]),
    )
  let st =
    finalize_scope(sb, opts, st, root_scope_id, parent_bindings, opts.strict)
  // --- (b') §B.3.2 var-twin declaration + annexb_blocked filtering --------
  // Runs AFTER every scope's bindings are known (V8: `DeclarationScope::
  // HoistSloppyBlockFunctions` in `Scope::Analyze`).
  let st = hoist_annexb_block_functions(sb, st, opts)
  let tree =
    ScopeTree(
      scopes: st.scopes,
      functions: st.functions,
      // CONTRACT with the parser: `children_at` arrives in the FINAL
      // hoist order (the parser calls `sb_set_children` at each scope
      // close to reorder fn-decls-first / class 7-step). No reversal
      // here.
      children_at: sb.children_at,
      top_lex: opts.top_lex,
      linker_seeded: opts.linker_seeded,
      inherited_with_stack: opts.with_stack,
    )
  // --- (c) resolve raw_refs → per-function free-name set ------------------
  let captured = resolve_raw_refs(tree, sb)
  let assigned = resolve_assign_refs(tree, sb)
  let refs_args = resolve_arguments_refs(tree, sb)
  // --- (d) capture/boxing/lexical allocation — unchanged ------------------
  analyze_captures(tree, captured, assigned, refs_args, sb.own_lexical_refs)
}

/// Convert one `RawScope` (and recursively its `children_at` subtree)
/// to a `Scope`, allocating a slot for each `RawBinding` from its
/// `function_scope`'s `local_count` in declaration-`index` order.
///
/// `seed_bindings` is non-empty only for the root (the
/// `opts.parent_names` captures — first-declaration-wins, so a
/// parser-recorded raw binding of the same name is skipped, mirroring
/// `sb_declare`'s no-op on an already-bound name).
/// `inherited_strict` propagates `opts.strict`
/// down the tree: the parser built `RawScope.is_strict` from its OWN
/// strict flag, which for direct-eval is the caller's strictness only
/// if `parse_direct_eval` was given it; OR-ing here makes `finalize`
/// robust to a sloppy-parsed body in a strict caller.
fn finalize_scope(
  sb: ScopeBuilder,
  opts: AnalyzeOpts,
  st: FinSt,
  scope_id: ScopeId,
  seed_bindings: Dict(String, Binding),
  inherited_strict: Bool,
) -> FinSt {
  let raw = sb_scope(sb, scope_id)
  let is_strict = raw.is_strict || inherited_strict
  // Function-kind child scopes get a fresh FunctionInfo with
  // local_count=0; root's was pre-seeded by `finalize` so `has_key`
  // skips it.
  let st = case
    is_function_kind(raw.kind) && !dict.has_key(st.functions, scope_id)
  {
    False -> st
    True -> {
      // `is_function_kind` guarantees `sb_push` seeded a RawFunctionInfo.
      let raw_fn = sb_fn_info(sb, scope_id)
      let info = blank_function_info(raw_fn, opts.fallthrough)
      FinSt(..st, functions: dict.insert(st.functions, scope_id, info))
    }
  }
  // Every scope draws slots from its function_scope's counter. The
  // pre-order walk guarantees that FunctionInfo exists by the time any
  // of its constituent block/catch/with/class-body scopes are visited
  // (a non-function-kind scope's `function_scope` is always an
  // ANCESTOR, never a descendant).
  let fn_id = raw.function_scope
  let assert Ok(info) = dict.get(st.functions, fn_id)
    as "scope.finalize_scope: function_scope FunctionInfo missing (pre-order invariant violated)"
  // `index` is the per-scope counter the parser bumped on each
  // `sb_declare`; sorting by it recovers declaration order (and so slot
  // order) — the dict has no ordering of its own. The parser declares a
  // scope's own bindings (direct lexicals, hoisted vars) BEFORE
  // descending into any child scope, so within a function all ancestor
  // slots precede descendant slots.
  let sorted =
    raw.bindings
    |> dict.to_list
    |> list.filter(fn(entry) {
      // Script-root gate: the parser unconditionally records every
      // Script-root `var`/fn-decl/let/const (it doesn't know `opts`),
      // but names that belong to the global / eval-env environment must
      // NOT get a local slot. Drop them here so `lookup` falls through (→
      // IrGetGlobal / IrGetEvalVar) and emit's IrDeclareGlobalVar /
      // IrDeclareGlobalLex prologue handles the actual declaration.
      // Applies ONLY at the Script root — nested functions and Module/
      // Function/ClassStaticBlock roots always own their bindings.
      let #(_name, rb) = entry
      root_binding_is_local(raw.kind, scope_id, opts, rb.kind)
    })
    |> list.sort(fn(a, b) {
      let #(_, ra) = a
      let #(_, rb) = b
      int.compare(ra.index, rb.index)
    })
  let #(bindings, info) =
    list.fold(sorted, #(seed_bindings, info), fn(acc, entry) {
      let #(bindings, info) = acc
      let #(name, rb) = entry
      // Pre-seeded root bindings — first-declaration-wins:
      // - Module root: the seeds are the linker's import boxes; the
      //   parser-recorded binding for the same import name must NOT
      //   shadow them.
      // - SLOPPY direct eval (Script root): the caller's variable
      //   environment IS the eval's (§19.2.1.1), so an eval-source
      //   `var x` naming a caller local re-uses the seeded alias.
      // EXCEPTION — STRICT direct eval (strict Script root with seeds):
      // it gets a FRESH variable environment, so a name the eval source
      // itself DECLARES must shadow the caller alias with an own slot;
      // only free names keep resolving through the alias.
      let strict_eval_root = opts.strict && raw.kind == Script
      let keep_seeded =
        dict.has_key(bindings, name)
        && !{ strict_eval_root && dict.has_key(seed_bindings, name) }
      use <- bool.guard(keep_seeded, acc)
      let slot = info.local_count
      // §10.2.11 step 28: in a non-simple parameter list the SHIMS
      // (`<param0>`..`<paramN-1>`, indices 0..N-1) stay ParamBinding;
      // every user formal name (shifted to index >= N, still ParamBinding
      // at parse time so `sb_var_conflicts_lexical` allowed a body
      // `var` redecl) is rekinded HERE to LetBinding so emit's
      // `emit_binding_prologue` TDZ-seeds it — the contract
      // `compile_function_body`'s non-simple param path relies on when
      // it destructures the shims into the user names.
      let kind = case rb.kind {
        ParamBinding ->
          case
            raw.non_simple_shim_count > 0
            && rb.index >= raw.non_simple_shim_count
          {
            True -> LetBinding
            False -> ParamBinding
          }
        k -> k
      }
      let binding =
        Binding(slot:, kind:, is_boxed: False, origin_kind_for_capture: kind)
      let names = case dict.has_key(info.names, name) {
        True -> info.names
        False -> dict.insert(info.names, name, slot)
      }
      #(
        dict.insert(bindings, name, binding),
        FunctionInfo(..info, local_count: slot + 1, names:),
      )
    })
  let scope =
    Scope(
      id: raw.id,
      parent: raw.parent,
      function_scope: raw.function_scope,
      kind: raw.kind,
      bindings:,
      contains_direct_eval: raw.contains_direct_eval,
      annexb_blocked: raw.annexb_blocked,
      is_strict:,
      is_var_boundary: raw.is_var_boundary,
    )
  let st =
    FinSt(
      scopes: dict.insert(st.scopes, scope_id, scope),
      functions: dict.insert(st.functions, fn_id, info),
    )
  // Recurse pre-order. A child Block/Catch/With/ClassBody shares
  // `fn_id`'s counter (just bumped above); a child Function/
  // ClassStaticBlock mints its own at the top of the next call.
  let children = sb_children_raw(sb, scope_id)
  list.fold(children, st, fn(st, child_id) {
    finalize_scope(sb, opts, st, child_id, dict.new(), is_strict)
  })
}

/// Whether a parser-recorded RawBinding in `scope_id` should become a
/// real local-slot Binding. Applies the Script-root "does this name get
/// a local slot at all?" gates the parser can't apply itself (it doesn't
/// have `AnalyzeOpts`):
///
///   - Script root, `!opts.strict` (sloppy script / sloppy direct eval /
///     REPL / indirect eval): `var` and top-level fn-decls belong to the
///     global VariableEnvironment (or the caller's eval_env), NOT a
///     local slot — §16.1.7 GlobalDeclarationInstantiation / §19.2.1.1
///     PerformEval. Drop VarBinding so the name falls through.
///   - Script root, `opts.top_lex == LexGlobal` (REPL only): top-level
///     let/const/class belong to the persistent global lexical record.
///     Drop LetBinding/ConstBinding so the name falls through.
///
/// Everything else (Module/Function/ClassStaticBlock roots, every nested
/// scope, Param/Catch/Capture/FnName at root) is always local.
fn root_binding_is_local(
  scope_kind: ScopeKind,
  scope_id: ScopeId,
  opts: AnalyzeOpts,
  kind: BindingKind,
) -> Bool {
  use <- bool.guard(scope_id != root_scope_id, True)
  case scope_kind {
    Script ->
      case kind {
        VarBinding -> opts.strict || opts.module_slot_globals
        LetBinding | ConstBinding -> opts.top_lex == LexLocal
        ParamBinding | CatchBinding | CaptureBinding | FnNameBinding -> True
      }
    Module
    | Function
    | Block
    | Catch
    | With(_)
    | ClassBody
    | ClassStaticBlock -> True
  }
}

/// §B.3.2 / §B.3.4 / §B.3.6 (V8's `DeclarationScope::
/// HoistSloppyBlockFunctions`): for every sloppy-mode block-level
/// FunctionDeclaration the parser recorded as a `#(block_id, name)`
/// candidate on the enclosing function, decide — now that the WHOLE body
/// is known — whether "replacing the FunctionDeclaration with `var F`"
/// would be an early error (an intermediate let/const/class, the named-
/// function-expression self-name, or a destructured catch param of the
/// same name sits between the block and the var-scope). If BLOCKED:
/// record `name` in the declaring block's `Scope.annexb_blocked` so
/// emit's per-FunctionDeclaration gate suppresses the runtime promote.
/// If NOT blocked: declare a `VarBinding` twin in the function-root
/// scope (allocate a fresh slot, bump `local_count`, register in
/// `names`) so emit's `annexb_find_target` walk lands on a real local —
/// or, for a sloppy Script root where vars fall through to the global
/// object, just append `name` to the function's filtered
/// `FunctionInfo.annexb_candidates` so emit_top_level_body emits
/// `IrDeclareGlobalVar`. `FunctionInfo.annexb_candidates` is the
/// FILTERED output (List(String) — emit consumes it as-is).
fn hoist_annexb_block_functions(
  sb: ScopeBuilder,
  st: FinSt,
  opts: AnalyzeOpts,
) -> FinSt {
  dict.fold(sb.functions, st, fn(st, fn_id, raw_fi) {
    use <- bool.guard(raw_fi.annexb_candidates == [], st)
    let fn_raw = sb_scope(sb, fn_id)
    // Whether a VarBinding at this fn-root is a real local slot (nested
    // function / Module / strict Script) or falls through to global /
    // eval-env (sloppy Script root → emit's IrDeclareGlobalVar path).
    let var_is_local =
      root_binding_is_local(fn_raw.kind, fn_id, opts, VarBinding)
    list.fold(raw_fi.annexb_candidates, st, fn(st, cand) {
      let #(block_id, name) = cand
      case annexb_walk_blocked(sb, block_id, fn_id, name) {
        True -> {
          // BLOCKED — mark the declaring block so emit's
          // `is_annexb_blocked` gate suppresses the promote AND the
          // name is excluded from the filtered candidate list (so
          // emit_top_level_body never emits IrDeclareGlobalVar for it).
          let assert Ok(bs) = dict.get(st.scopes, block_id)
            as "scope: Annex-B block absent from finalized scopes"
          FinSt(
            ..st,
            scopes: dict.insert(
              st.scopes,
              block_id,
              Scope(..bs, annexb_blocked: set.insert(bs.annexb_blocked, name)),
            ),
          )
        }
        False -> {
          let assert Ok(info) = dict.get(st.functions, fn_id)
            as "scope.hoist_annexb_block_functions: FunctionInfo missing"
          let assert Ok(fn_scope) = dict.get(st.scopes, fn_id)
            as "scope.hoist_annexb_block_functions: fn-root Scope missing"
          let already = dict.has_key(fn_scope.bindings, name)
          // Allocate a var-twin slot only when the var would be local
          // and no same-named binding (explicit `var f` / param /
          // earlier candidate) already exists — first-declaration-wins,
          // the same rule `sb_declare` / `finalize_scope` apply.
          let #(fn_scope, info) = case var_is_local && !already {
            False -> #(fn_scope, info)
            True -> {
              let slot = info.local_count
              let binding =
                Binding(
                  slot:,
                  kind: VarBinding,
                  is_boxed: False,
                  origin_kind_for_capture: VarBinding,
                )
              let names = case dict.has_key(info.names, name) {
                True -> info.names
                False -> dict.insert(info.names, name, slot)
              }
              #(
                Scope(
                  ..fn_scope,
                  bindings: dict.insert(fn_scope.bindings, name, binding),
                ),
                FunctionInfo(..info, local_count: slot + 1, names:),
              )
            }
          }
          let info =
            FunctionInfo(..info, annexb_candidates: [
              name,
              ..info.annexb_candidates
            ])
          FinSt(
            scopes: dict.insert(st.scopes, fn_id, fn_scope),
            functions: dict.insert(st.functions, fn_id, info),
          )
        }
      }
    })
  })
}

/// Walk the RAW scope chain from the parent of `from_block` up to and
/// including `fn_id`, returning True when any scope on the chain has a
/// binding for `name` that would make `var name` an early error there:
/// LetBinding / ConstBinding / FnNameBinding, or a CatchBinding in a
/// Catch scope whose parameter is destructured (§B.3.4 — a simple
/// `catch(e)` is var-transparent). The block itself is skipped: its own
/// LetBinding for the FunctionDeclaration is the SOURCE binding, not a
/// blocker. Walks RawScope (not the finalized Scope) so a Script-root
/// let/const dropped by `root_binding_is_local` still blocks.
fn annexb_walk_blocked(
  sb: ScopeBuilder,
  from_block: ScopeId,
  fn_id: ScopeId,
  name: String,
) -> Bool {
  case sb_scope(sb, from_block).parent {
    None -> False
    Some(parent_id) -> annexb_check_chain(sb, parent_id, fn_id, name)
  }
}

fn annexb_check_chain(
  sb: ScopeBuilder,
  scope_id: ScopeId,
  fn_id: ScopeId,
  name: String,
) -> Bool {
  let raw = sb_scope(sb, scope_id)
  let blocked_here = case dict.get(raw.bindings, name) {
    Error(Nil) -> False
    Ok(rb) ->
      case raw.kind {
        // §B.3.4: a Catch scope's parameter binding (recorded by the
        // parser as ParamBinding regardless of pattern shape) is
        // var-transparent ONLY when the parameter is a bare identifier;
        // a destructured `catch({f})` blocks the var-twin. Discriminate
        // on the SCOPE kind so the ParamBinding-vs-CatchBinding
        // representation doesn't matter.
        Catch -> !raw.catch_param_simple
        Module
        | Script
        | Function
        | Block
        | With(_)
        | ClassBody
        | ClassStaticBlock ->
          case rb.kind {
            LetBinding | ConstBinding | FnNameBinding -> True
            // §B.3.2.1.a.ii: "F is not an element of parameterNames" —
            // a same-named formal parameter SUPPRESSES the Annex B
            // var-twin/promote entirely. In a non-Catch scope,
            // ParamBinding can only be a function-root formal (catch
            // params live in a Catch scope, handled above), so this is
            // exactly the parameterNames check. Matches V8's
            // HoistSloppyBlockFunctions.
            ParamBinding -> True
            // Unreachable: the parser records catch parameters as
            // ParamBinding INSIDE a Catch scope (handled above); no
            // RawBinding of kind CatchBinding is ever declared.
            CatchBinding -> False
            VarBinding | CaptureBinding -> False
          }
      }
  }
  use <- bool.guard(blocked_here, True)
  use <- bool.guard(scope_id == fn_id, False)
  case raw.parent {
    None -> False
    Some(parent_id) -> annexb_check_chain(sb, parent_id, fn_id, name)
  }
}

/// REFERENCE pass, over the parser's flat `raw_refs` list (each `sb_ref`
/// call appended one entry): for each `#(scope_id, name)` reference the
/// parser recorded, determine whether `name` is FREE in the referencing
/// function (declared in an enclosing function or nowhere) and, if so,
/// add `#(ref_fn, name)` to
/// the result — `analyze_captures`' `derive_free_own` pivots that into
/// `FnAnalysisInput.free_own`. `seen` dedupes repeat refs from the same
/// scope so each `(scope, name)` pair walks the parent chain at most
/// once (the parser records one entry per syntactic occurrence).
fn resolve_raw_refs(
  tree: ScopeTree,
  sb: ScopeBuilder,
) -> Set(#(ScopeId, String)) {
  let #(_seen, captured) =
    list.fold(sb.raw_refs, #(set.new(), set.new()), fn(acc, ref) {
      let #(seen, captured) = acc
      let #(scope_id, name) = ref
      // The ref's scope may be a `sb_prune_empty_block` / `sb_discard`
      // tombstone: present in `sb.scopes` but absent from `tree.scopes`.
      // Read `function_scope` from the RawScope (a pruned Block inherits
      // its parent's, so it's correct), then walk up the RawScope
      // parent chain to the nearest ancestor that DID survive into
      // `tree.scopes` and start `find_declaring_scope` from there —
      // the skipped tombstones have no bindings by construction so
      // they cannot affect resolution. `sb.scopes` retains EVERY id ever
      // minted, so a miss is a dangling ref (parser bug) — crash rather
      // than silently drop the reference and mis-resolve a capture.
      let assert Ok(raw) = dict.get(sb.scopes, scope_id)
        as "scope.resolve_raw_refs: dangling raw_ref"
      use <- bool.guard(set.contains(seen, ref), acc)
      let seen = set.insert(seen, ref)
      let ref_fn = raw.function_scope
      let is_free = case nearest_finalized(tree, sb, scope_id) {
        None -> True
        Some(start) ->
          case find_declaring_scope(tree, start, name) {
            None -> True
            Some(decl) -> decl.function_scope != ref_fn
          }
      }
      case is_free {
        False -> #(seen, captured)
        True -> #(seen, set.insert(captured, #(ref_fn, name)))
      }
    })
  captured
}

/// Resolve every parser-recorded assignment target to the FUNCTION scope
/// that declares it. Result: `declaring_fn → names ever assigned to`.
/// Used by `derive_vars_to_box` to skip boxing never-reassigned captures.
/// Undeclared (global) targets are dropped — they never had a local slot.
fn resolve_assign_refs(
  tree: ScopeTree,
  sb: ScopeBuilder,
) -> Dict(ScopeId, Set(String)) {
  use acc, ref <- list.fold(sb.assign_refs, dict.new())
  let #(scope_id, name) = ref
  case nearest_finalized(tree, sb, scope_id) {
    None -> acc
    Some(start) ->
      case find_declaring_scope(tree, start, name) {
        None -> acc
        Some(decl) ->
          dict.upsert(acc, decl.function_scope, fn(prev) {
            case prev {
              Some(s) -> set.insert(s, name)
              None -> set.from_list([name])
            }
          })
      }
  }
}

/// Function scopes whose own `arguments` binding is REFERENCED (directly
/// or from a nested arrow). In sloppy mode with a simple parameter list
/// this means mapped `arguments` — `arguments[i]=v` writes the param — so
/// `never_box_names` must not capture such params by value.
fn resolve_arguments_refs(tree: ScopeTree, sb: ScopeBuilder) -> Set(ScopeId) {
  use acc, ref <- list.fold(sb.raw_refs, set.new())
  case ref {
    #(scope_id, "arguments") ->
      case nearest_finalized(tree, sb, scope_id) {
        None -> acc
        Some(start) ->
          case find_declaring_scope(tree, start, "arguments") {
            Some(decl) -> set.insert(acc, decl.function_scope)
            None -> acc
          }
      }
    _ -> acc
  }
}

/// Find which scope (if any) DECLARES `name`, walking outward from
/// `scope_id`. Returns the declaring scope or None for a free name.
/// Used by `resolve_raw_refs` to classify each parser-recorded ref as
/// own-vs-free relative to its referencing function.
fn find_declaring_scope(
  tree: ScopeTree,
  scope_id: ScopeId,
  name: String,
) -> Option(Scope) {
  let scope = get_scope(tree, scope_id)
  case dict.has_key(scope.bindings, name) {
    True -> Some(scope)
    False ->
      case scope.parent {
        Some(parent) -> find_declaring_scope(tree, parent, name)
        None -> None
      }
  }
}

/// Walk the `RawScope.parent` chain from `scope_id` to the first
/// ancestor present in `tree.scopes` (i.e. not a pruned-block
/// tombstone). Returns `scope_id` itself when it wasn't pruned.
fn nearest_finalized(
  tree: ScopeTree,
  sb: ScopeBuilder,
  scope_id: ScopeId,
) -> Option(ScopeId) {
  case dict.has_key(tree.scopes, scope_id) {
    True -> Some(scope_id)
    False ->
      case dict.get(sb.scopes, scope_id) {
        Error(Nil) -> None
        Ok(raw) ->
          case raw.parent {
            Some(parent_id) -> nearest_finalized(tree, sb, parent_id)
            None -> None
          }
      }
  }
}

// ============================================================================
// Synthetic-binding name helpers
//
// The analyzer pre-registers every synthetic binding a child closure can
// capture (so it appears in the parent's scope chain and gets a slot the
// closure-capture analysis sees). Pure scratch (using-emission `%u:N`
// temps, the completion-value slot, with-ref base slots) is NOT a binding
// — the emitter mints those via `alloc_scratch` instead.
// ============================================================================

/// `*default*` — the module-scope binding for an anonymous default export.
pub const default_export = "*default*"

/// `<withN_M>` — synthetic with-object holder for a `With` scope. `depth`
/// is the count of enclosing `with` statements (monotonically increasing
/// along any lexical chain); `with_id` is the `With` scope's id (unique
/// across siblings).
fn with_object_name(depth: Int, with_id: ScopeId) -> String {
  "<with" <> int.to_string(depth) <> "_" <> int.to_string(with_id) <> ">"
}

/// `<paramN>` — shim for a non-simple positional parameter; holds the raw
/// argument before destructuring. Like `with_object_name`'s `<withN_M>`,
/// the angle brackets make the name IMPOSSIBLE as a JS identifier: a user
/// formal literally spelled like a shim would otherwise be silently
/// clobbered when `sb_insert_param_shims` shifts the already-declared
/// formals past the shims and then inserts the shims at indices 0..N-1
/// (the `dict.insert` overwrites the user binding, mis-wiring arguments).
pub fn param_shim(idx: Int) -> String {
  "<param" <> int.to_string(idx) <> ">"
}

// ============================================================================
// Public API — ScopeTree lookup (consumed by emit.gleam)
// ============================================================================

/// Resolve `name` from `scope_id` by walking the parent chain. Returns the
/// Resolution the emitter should compile to: a Local slot if a binding is
/// found in this function's scope chain; a WithChain wrapping the fallback
/// if any `with` scope was crossed before the binding (or before falling
/// through); otherwise Global/EvalEnv per the function's `fallthrough`.
pub fn lookup(tree: ScopeTree, scope_id: ScopeId, name: String) -> Resolution {
  do_lookup(tree, scope_id, name, [])
}

fn do_lookup(
  tree: ScopeTree,
  scope_id: ScopeId,
  name: String,
  crossed: List(SlotRef),
) -> Resolution {
  let scope = get_scope(tree, scope_id)
  case dict.get(scope.bindings, name) {
    Ok(Binding(slot:, kind:, is_boxed:, origin_kind_for_capture:)) -> {
      // CaptureBindings live ONLY in a function-root scope (inserted by
      // `insert_captures`). The legacy resolver placed phantom with-scopes
      // BETWEEN this function's own scopes and the capture scope
      // (`resolve_with_captures` :248-256), so a captured name probed every
      // INHERITED with-object before reaching the capture. Own bindings
      // (var/param/let/const/catch/fn-name) shadow inherited withs — they
      // were inside the phantom layer in the legacy scope list — so only a
      // CaptureBinding hit augments `crossed`. §9.1.2.1
      // GetIdentifierReference: the inner function's outer environment IS
      // the with-environment, so it must be checked before the closure env.
      let crossed = case kind {
        CaptureBinding ->
          list.append(inherited_with_slots(tree, scope), crossed)
        VarBinding
        | LetBinding
        | ConstBinding
        | ParamBinding
        | CatchBinding
        | FnNameBinding -> crossed
      }
      wrap_with_chain(
        crossed,
        Local(
          slot:,
          boxed: is_boxed,
          kind:,
          origin_kind: origin_kind_for_capture,
        ),
      )
    }
    Error(Nil) -> {
      // Crossing a `with` scope adds its object-slot to the runtime probe
      // chain BEFORE consulting the next outer scope. The `<withN_M>`
      // holder binding is declared IN the With scope itself (by
      // `sb_push_with`, which also mints the name on the kind), so both its
      // slot and its `is_boxed` flag — set by `analyze_captures` like any
      // other binding, when a child closure defined inside the `with` body
      // captures it — come from this scope's own `bindings` entry.
      let crossed = case scope.kind {
        With(holder:) -> [own_holder_ref(scope, holder), ..crossed]
        _ -> crossed
      }
      // STOP at function boundaries. A name not found anywhere in this
      // function's own scope chain falls through to Global/EvalEnv —
      // `analyze_captures` inserts a CaptureBinding into the function-root
      // scope for every name this function references that lives in an
      // outer frame, so a legitimately-captured name is found above before
      // reaching here. Walking past the boundary into `scope.parent` would
      // return the OUTER frame's slot index, which is meaningless in this
      // frame (the `Local` resolution's slot is by definition a slot in the
      // CURRENT function's locals array). Matches the legacy resolver,
      // which never crossed function boundaries — but inherited with-scopes
      // (phantom layer between own scopes and the fallthrough) MUST still
      // be probed, same as for a CaptureBinding hit above.
      case is_function_kind(scope.kind), scope.parent {
        False, Some(parent_id) -> do_lookup(tree, parent_id, name, crossed)
        True, _ | False, None -> {
          let crossed = list.append(inherited_with_slots(tree, scope), crossed)
          let info = function_info(tree, scope.function_scope)
          let fallback = case info.fallthrough {
            ToGlobal -> Global(name)
            ToEvalEnv -> EvalEnv(name)
          }
          wrap_with_chain(crossed, fallback)
        }
      }
    }
  }
}

/// The `<withN_M>` holder's own slot in the With scope that declares it.
/// `sb_push_with` is the only constructor of a `With` kind and it declares
/// the binding in the same breath, so the lookup is total.
fn own_holder_ref(with_scope: Scope, holder: String) -> SlotRef {
  let assert Ok(b) = dict.get(with_scope.bindings, holder)
    as "scope: With(holder) whose holder binding is not in its own scope"
  SlotRef(slot: b.slot, boxed: b.is_boxed)
}

fn wrap_with_chain(crossed: List(SlotRef), fallback: Direct) -> Resolution {
  case crossed {
    [] -> Plain(fallback)
    _ -> WithChain(crossed_slots: list.reverse(crossed), fallback:)
  }
}

/// With-object slots inherited from enclosing functions — the legacy
/// resolver's phantom with-scopes inserted between this function's own
/// scopes and its capture scope (`resolve_with_captures` :248-256). A
/// nested function defined inside `with(o){…}` must probe `o` (and any
/// outer with-objects) for any name that resolves to a capture or falls
/// through to global, per §9.1.2.1 GetIdentifierReference — the inner
/// function's [[OuterEnv]] IS the with-environment.
///
/// Walks the parent chain ACROSS the function boundary, collecting each
/// With scope's holder name and resolving it to its CAPTURE slot in
/// `fn_root.bindings` (where `insert_captures` placed a CaptureBinding via
/// `fn_with_stack_free` → transitive_free → captures). Returned
/// outermost-first so `list.append(_, crossed)` then `wrap_with_chain`'s
/// reverse yield innermost-first probe order: own withs → inherited withs
/// → fallback. Inherited with-objects are always boxed (heap-shared with
/// the parent frame). For the root scope (parent = None) the inherited
/// stack comes from the caller-supplied `ScopeTree.inherited_with_stack`
/// (direct-eval) instead.
fn inherited_with_slots(tree: ScopeTree, fn_root: Scope) -> List(SlotRef) {
  case fn_root.parent {
    None ->
      tree.inherited_with_stack
      |> list.map(SlotRef(slot: _, boxed: True))
      |> list.reverse
    Some(_) -> {
      use acc, holder <- fold_enclosing_withs(tree, fn_root.parent, [])
      // Look the enclosing With's holder NAME up in the function root's
      // bindings — `fn_with_stack_free` adds every enclosing with-holder to
      // this function's free set, so `insert_captures` placed a
      // CaptureBinding for it. Skip if absent (matches the legacy
      // `list.filter_map(with_stack, …)`).
      case dict.get(fn_root.bindings, holder) {
        Ok(b) -> [SlotRef(slot: b.slot, boxed: True), ..acc]
        Error(Nil) -> acc
      }
    }
  }
}

/// Walk the parent chain from `scope_id` to the root, calling `f` once per
/// `With` scope encountered (innermost visited first) with that scope's
/// `<withN_M>` holder name. Shared spine for `inherited_with_slots` and
/// `fn_with_stack_free`.
fn fold_enclosing_withs(
  tree: ScopeTree,
  scope_id: Option(ScopeId),
  acc: a,
  f: fn(a, String) -> a,
) -> a {
  case scope_id {
    None -> acc
    Some(id) -> {
      let scope = get_scope(tree, id)
      let acc = case scope.kind {
        With(holder:) -> f(acc, holder)
        _ -> acc
      }
      fold_enclosing_withs(tree, scope.parent, acc, f)
    }
  }
}

/// Resolve a lexical pseudo-binding (this / active_func / home_object /
/// new.target) from `scope_id`. Returns the local slot in the owning
/// function's frame and whether it is boxed (it is when an inner arrow
/// or direct-eval captures it, so writes via super() alias correctly).
pub fn lookup_lexical(
  tree: ScopeTree,
  scope_id: ScopeId,
  ref: LexicalRef,
) -> SlotRef {
  let scope = get_scope(tree, scope_id)
  let info = function_info(tree, scope.function_scope)
  case lexical.lexical_slot(info.lexical, ref) {
    // Owned slot: boxed only when an inner arrow / direct-eval captures it
    // (FunctionInfo.lexical_boxed, populated by analyze_captures). A
    // non-captured `this` is a plain GetLocal, not a box deref.
    Some(slot) ->
      SlotRef(slot:, boxed: lexical.lexical_refs_get(info.lexical_boxed, ref))
    None ->
      case dict.get(info.lexical_captures, ref) {
        Ok(slot) -> SlotRef(slot:, boxed: True)
        // No owned slot and no inherited capture: unreachable by
        // construction — every site that calls lookup_lexical is inside a
        // function the analyzer has populated. Returning a sentinel slot
        // (e.g. 0) would silently alias an unrelated local, so panic
        // matching get_scope/function_info's contract.
        Error(Nil) -> panic as "scope.lookup_lexical: no slot for lexical ref"
      }
  }
}

/// Allocate a fresh local slot in `function_scope_id` for an anonymous
/// scratch value the emitter needs (e.g. `%u:N` using-disposer temps,
/// completion_var, IrScopeMakeRef base holders) that is NEVER captured by
/// a child closure and so does not need a name in the scope tree. Returns
/// the updated tree (with `local_count` bumped) and the new slot index.
pub fn alloc_scratch(
  tree: ScopeTree,
  function_scope_id: ScopeId,
) -> #(ScopeTree, Int) {
  let info = function_info(tree, function_scope_id)
  let slot = info.local_count
  let info = FunctionInfo(..info, local_count: slot + 1)
  let tree =
    ScopeTree(
      ..tree,
      functions: dict.insert(tree.functions, function_scope_id, info),
    )
  #(tree, slot)
}

/// Fetch the FunctionInfo for a function-kind scope. Panics if `scope_id`
/// is not a function scope — callers pass `scope.function_scope`, which is
/// guaranteed to be one by construction.
pub fn function_info(tree: ScopeTree, scope_id: ScopeId) -> FunctionInfo {
  let assert Ok(info) = dict.get(tree.functions, scope_id)
    as "scope.function_info: not a function scope"
  info
}

/// Fetch a scope node. Panics on an unknown id — every ScopeId the parser
/// allocated via `sb_push` is present in the `finalize`d tree.
pub fn get_scope(tree: ScopeTree, scope_id: ScopeId) -> Scope {
  let assert Ok(scope) = dict.get(tree.scopes, scope_id)
    as "scope.get_scope: unknown ScopeId"
  scope
}

/// Direct child scope ids of `scope_id` in source order.
pub fn child_scopes(tree: ScopeTree, scope_id: ScopeId) -> List(ScopeId) {
  dict.get(tree.children_at, scope_id) |> result.unwrap([])
}

/// Direct-and-transitive child FUNCTION-kind scope ids nested inside
/// `parent_fn_scope_id`'s frame, in source order — i.e. the scopes the
/// emitter will hit an IrMakeClosure for while emitting this function
/// body. Walks through Block/Catch/With/ClassBody children (which share the
/// parent's frame) and stops at the first function-kind scope on each path.
pub fn child_function_scopes(
  tree: ScopeTree,
  parent_fn_scope_id: ScopeId,
) -> List(ScopeId) {
  collect_child_fns(tree, parent_fn_scope_id, [])
  |> list.reverse
}

fn collect_child_fns(
  tree: ScopeTree,
  scope_id: ScopeId,
  acc: List(ScopeId),
) -> List(ScopeId) {
  use acc, child_id <- list.fold(child_scopes(tree, scope_id), acc)
  let child = get_scope(tree, child_id)
  case is_function_kind(child.kind) {
    True -> [child_id, ..acc]
    False -> collect_child_fns(tree, child_id, acc)
  }
}

// ============================================================================
// Capture / eval / with analysis on the scope tree
//
// Replaces compiler.gleam's IR-scanning passes (collect_free_vars / scan_free
// / collect_all_captured_vars / collect_vars_to_box / any_descendant_has_eval
// / collect_arrow_lexical_refs, :566-928). Those walked the post-emission
// EmitterOp stream to reconstruct what the AST already knows; this pass runs
// on the ScopeTree so the emitter can read final boxing/capture decisions
// while it emits.
//
// V8 model (src/ast/scopes.cc): Scope::Analyze first resolves every
// VariableProxy up the scope chain — crossing a function boundary marks the
// binding `is_used` and forces CONTEXT (heap) allocation; then propagates
// `inner_scope_calls_eval_` upward, and any scope with that flag forces
// MustAllocateInContext for every binding. We mirror that as a two-phase
// walk: bottom-up over function scopes (eval flag + transitive free names),
// then top-down (concrete capture lists / boxing sets given the parent's
// view).
// ============================================================================

/// Per-function-scope inputs to `analyze_captures` the AST walker collects
/// while building the tree (properties of the function body that aren't
/// per-binding and so don't live on Scope/FunctionInfo).
type FnAnalysisInput {
  FnAnalysisInput(
    /// Arrow function — does NOT own lexical pseudo-slots; captures them
    /// from the enclosing non-arrow instead.
    is_arrow: Bool,
    /// Strict-mode body. With eval_in_subtree, decides ToEvalEnv vs ToGlobal.
    is_strict: Bool,
    /// Lexical pseudo-bindings (this/active_func/home_object/new.target)
    /// REFERENCED in this body or any nested arrow. Mirrors
    /// CompiledChild.lexical_refs.
    lexical_refs: LexicalRefs,
    /// Names referenced by this body's own code (NOT inside child function
    /// bodies) that do not resolve to a binding within this body — the
    /// scope-aware "own free names". The AST walker computes this with the
    /// live block stack, so a name shadowed only in a sibling/nested block
    /// is still free at the outer use site. Child-closure free names are
    /// folded in by the bottom-up phase below (transitive_free), not here.
    free_own: Set(String),
  )
}

/// What a child closure can see of its parent at its creation site —
/// the parent's own bindings in scope there PLUS the parent's captures.
/// Threads const/FnName origin through capture chains so a write through
/// a captured-captured-const still throws TypeError.
type ParentView {
  ParentView(
    /// All visible names → PARENT's slot index (for env_descriptors).
    names: Dict(String, Int),
    /// Subset whose ORIGIN binding is const.
    consts: Set(String),
    /// Subset whose ORIGIN binding is a named-function-expression self name.
    fn_names: Set(String),
    /// Subset whose PARENT binding is a boxed heap-cell. A capture of a
    /// name NOT in this set is by-value (child reads cap_i directly, no
    /// cell_get) — see `derive_vars_to_box` / `insert_captures`.
    boxed: Set(String),
    /// Lexical pseudo-slots the parent has available (owned or captured).
    /// An arrow can capture only refs the parent actually has.
    lexical_available: LexicalRefs,
  )
}

/// Build the `FnAnalysisInput` dict `analyze_captures` consumes — one
/// entry per function-kind scope. Derives every field from the tree the
/// DECLARE + REFERENCE passes produced, so the caller threads no extra
/// state:
///   `is_arrow`   — `FunctionInfo.is_arrow` (recorded by the parser on
///                  the function's RawScope).
///   `is_strict`  — `Scope.is_strict` of the function-root scope.
///   `free_own`   — names in `captured` whose declaring scope's
///                  `function_scope` differs from the reference site's
///                  (or is undeclared). The block-scoped reference walk
///                  preserves the "live block stack" semantics the
///                  `FnAnalysisInput.free_own` doc requires.
///   `lexical_refs` — `own_lexical_refs[fn]` ∪, for each ARROW child,
///                  the child's already-built `lexical_refs` (post-order)
///                  — mirrors emit.gleam's CompiledChild propagation
///                  where an arrow's lexical_refs OR into the parent's.
///
/// `captured` (from `resolve_raw_refs`) and `own_lexical_refs` (from the
/// parser's `sb_lexical_ref` recording) are the REFERENCE-pass outputs —
/// pass-internal, never stored on `ScopeTree`.
fn build_capture_inputs(
  tree: ScopeTree,
  captured: Set(#(ScopeId, String)),
  own_lexical_refs: Dict(ScopeId, LexicalRefs),
) -> Dict(ScopeId, FnAnalysisInput) {
  let free_by_fn = derive_free_own(captured)
  build_inputs_rec(
    tree,
    free_by_fn,
    own_lexical_refs,
    root_scope_id,
    dict.new(),
  )
}

/// Group every free reference recorded by the REFERENCE pass by the
/// function scope it occurs in. `resolve_raw_refs` already performed the
/// `find_declaring_scope` walk and stored `(ref_fn, name)` for every
/// name FREE in `ref_fn` — this just pivots that flat set into a
/// per-function dict.
fn derive_free_own(
  captured: Set(#(ScopeId, String)),
) -> Dict(ScopeId, Set(String)) {
  use d, entry <- set.fold(captured, dict.new())
  let #(ref_fn, name) = entry
  dict.upsert(d, ref_fn, fn(prev) {
    case prev {
      Some(s) -> set.insert(s, name)
      None -> set.from_list([name])
    }
  })
}

/// Post-order walk of the function-scope tree building one
/// `FnAnalysisInput` per function. Children are visited first so an
/// arrow child's `lexical_refs` is available when computing the parent's.
fn build_inputs_rec(
  tree: ScopeTree,
  free_by_fn: Dict(ScopeId, Set(String)),
  own_lexical_refs: Dict(ScopeId, LexicalRefs),
  fn_id: ScopeId,
  acc: Dict(ScopeId, FnAnalysisInput),
) -> Dict(ScopeId, FnAnalysisInput) {
  let children = child_function_scopes(tree, fn_id)
  let acc =
    list.fold(children, acc, fn(acc, cid) {
      build_inputs_rec(tree, free_by_fn, own_lexical_refs, cid, acc)
    })
  let info = function_info(tree, fn_id)
  let scope = get_scope(tree, fn_id)
  let own =
    dict.get(own_lexical_refs, fn_id)
    |> result.unwrap(lexical.no_lexical_refs)
  // CompiledChild.lexical_refs semantics: own ∪ (each arrow child's
  // lexical_refs). Non-arrow children own their lexical slots and do
  // not propagate (emit.gleam: `case child.is_arrow { True -> or(...) }`).
  let lexical_refs =
    list.fold(children, own, fn(refs, cid) {
      let assert Ok(cinp) = dict.get(acc, cid)
        as "build_inputs_rec: child not in acc after post-order recursion"
      case cinp.is_arrow {
        True -> lexical.lexical_refs_or(refs, cinp.lexical_refs)
        False -> refs
      }
    })
  let free_own = dict.get(free_by_fn, fn_id) |> result.unwrap(set.new())
  dict.insert(
    acc,
    fn_id,
    FnAnalysisInput(
      is_arrow: info.is_arrow,
      is_strict: scope.is_strict,
      lexical_refs:,
      free_own:,
    ),
  )
}

/// Run capture/eval/with analysis over the ScopeTree. Returns the tree with
/// `Binding.is_boxed`, `FunctionInfo.captures`, `FunctionInfo.lexical_captures`
/// and `FunctionInfo.fallthrough` filled in for every function scope.
fn analyze_captures(
  tree: ScopeTree,
  captured: Set(#(ScopeId, String)),
  assigned: Dict(ScopeId, Set(String)),
  refs_args: Set(ScopeId),
  own_lexical_refs: Dict(ScopeId, LexicalRefs),
) -> ScopeTree {
  let inputs = build_capture_inputs(tree, captured, own_lexical_refs)
  // Precompute function_scope → member scope ids once. compute_up/down each
  // run per function-kind scope, and the per-function helpers (declared_in,
  // apply_boxing) otherwise scan ALL scopes — O(F × S) total. With this
  // index they touch only their own scopes, matching the legacy IR-scan's
  // per-function linearity.
  let by_fn = scopes_by_function(tree)
  // Phase 1 (bottom-up): eval_in_subtree, transitive_free.
  // Each depends only on the node and its descendants.
  let up = compute_up(tree, inputs, by_fn, root_scope_id, dict.new())
  // Phase 2 (top-down): captures, lexical_captures, vars_to_box,
  // lexical_captured, fallthrough. Each depends on the PARENT's view.
  // Root sees an empty parent — nothing visible above a top-level body.
  let root_parent =
    ParentView(
      names: dict.new(),
      consts: set.new(),
      fn_names: set.new(),
      boxed: set.new(),
      lexical_available: lexical.no_lexical_refs,
    )
  compute_down(
    tree,
    inputs,
    by_fn,
    up,
    assigned,
    refs_args,
    root_scope_id,
    root_parent,
  )
}

/// Group every scope id by its owning `function_scope`. Built once per
/// `analyze_captures` call so per-function passes iterate only their own
/// scopes instead of folding the whole tree.
fn scopes_by_function(tree: ScopeTree) -> Dict(ScopeId, List(ScopeId)) {
  use d, id, scope <- dict.fold(tree.scopes, dict.new())
  dict.upsert(d, scope.function_scope, fn(prev) {
    case prev {
      Some(l) -> [id, ..l]
      None -> [id]
    }
  })
}

fn fn_member_scopes(
  by_fn: Dict(ScopeId, List(ScopeId)),
  fn_id: ScopeId,
) -> List(ScopeId) {
  dict.get(by_fn, fn_id) |> result.unwrap([])
}

// ---- Phase 1: bottom-up ----------------------------------------------------

type Up {
  Up(own_eval: Bool, eval_in_subtree: Bool, transitive_free: Set(String))
}

fn compute_up(
  tree: ScopeTree,
  inputs: Dict(ScopeId, FnAnalysisInput),
  by_fn: Dict(ScopeId, List(ScopeId)),
  fn_id: ScopeId,
  acc: Dict(ScopeId, Up),
) -> Dict(ScopeId, Up) {
  let children = child_function_scopes(tree, fn_id)
  // Recurse first (post-order).
  let acc =
    list.fold(children, acc, fn(acc, child_id) {
      compute_up(tree, inputs, by_fn, child_id, acc)
    })
  let inp = get_input(inputs, fn_id)
  let own_scope_ids = fn_member_scopes(by_fn, fn_id)
  let own_scopes = list.map(own_scope_ids, get_scope(tree, _))
  // any_descendant_has_eval: own direct-eval ∨ any child's flag.
  let own_eval = list.any(own_scopes, fn(s) { s.contains_direct_eval })
  let eval_in_subtree =
    own_eval
    || list.any(children, fn(cid) { { get_up(acc, cid) }.eval_in_subtree })
  // collect_free_vars: own free names ∪, for each child, the child's
  // transitive_free minus the bindings of THIS body visible at that
  // child's creation site (scan_free's `in_scope` filter at IrMakeClosure)
  // ∪ inherited with-object synthetics not declared here.
  let declared = declared_in(own_scopes)
  let from_children =
    list.fold(children, set.new(), fn(s, cid) {
      let visible = visible_at_creation(tree, cid)
      let visible_names = set.from_list(dict.keys(visible))
      let child_free = { get_up(acc, cid) }.transitive_free
      set.union(s, set.difference(child_free, visible_names))
    })
  let with_free = fn_with_stack_free(tree, fn_id, declared)
  let transitive_free =
    inp.free_own |> set.union(from_children) |> set.union(with_free)
  dict.insert(acc, fn_id, Up(own_eval:, eval_in_subtree:, transitive_free:))
}

// ---- Phase 2: top-down -----------------------------------------------------

/// Per-function lexical-pseudo-slot layout as decided by
/// `derive_lexical_layout`. Bundles every value the layout step produces so
/// `compute_down` threads one named record instead of six loose lets.
type LexLayout {
  LexLayout(
    /// `FunctionInfo.lexical` — Owned/Captured/No pseudo-slot table.
    lexical: LexicalSlots,
    /// `FunctionInfo.lexical_captures` — inherited refs → capture-slot idx.
    lexical_captures: Dict(LexicalRef, Int),
    /// `FunctionInfo.lexical_boxed` — which pseudo-slots read as boxed.
    lexical_boxed: LexicalRefs,
    /// Total captures-first prefix width (name captures + lexical captures
    /// + owned pseudo-slots) — the shift `insert_captures` applies to
    /// every own binding so captures occupy slots 0..cap_count-1.
    cap_count: Int,
    /// Refs THIS function can offer its arrow children (owned ∪ captured).
    available: LexicalRefs,
    /// A non-direct-eval Script root — its seeded `info.lexical` from
    /// `finalize` is authoritative and must not be overwritten.
    script_root_owns: Bool,
  )
}

fn compute_down(
  tree: ScopeTree,
  inputs: Dict(ScopeId, FnAnalysisInput),
  by_fn: Dict(ScopeId, List(ScopeId)),
  ups: Dict(ScopeId, Up),
  assigned: Dict(ScopeId, Set(String)),
  refs_args: Set(ScopeId),
  fn_id: ScopeId,
  parent: ParentView,
) -> ScopeTree {
  let inp = get_input(inputs, fn_id)
  let up = get_up(ups, fn_id)
  let is_root = fn_id == root_scope_id
  let children = child_function_scopes(tree, fn_id)
  let own_scope_ids = fn_member_scopes(by_fn, fn_id)
  let own_scopes = list.map(own_scope_ids, get_scope(tree, _))
  let declared = declared_in(own_scopes)
  let seeded_info = function_info(tree, fn_id)
  let kind = { get_scope(tree, fn_id) }.kind

  // (1) Name captures from the parent view.
  let #(captures, const_captures, fn_name_captures) =
    derive_name_captures(up, parent)

  // (2) Lexical pseudo-slot layout (this / active_func / home_object /
  //     new.target): capture vs own, boxed-ness, and total prefix width.
  let lex =
    derive_lexical_layout(
      is_root,
      kind,
      inp,
      up,
      seeded_info,
      parent,
      list.length(captures),
      children,
      inputs,
    )

  // (3) Which own declared bindings must be heap-boxed.
  let forced_box = case is_root {
    True -> tree.linker_seeded
    False -> set.new()
  }
  let assigned_here = dict.get(assigned, fn_id) |> result.unwrap(set.new())
  // Sloppy body + `arguments` referenced ⇒ mapped-arguments MAY alias
  // params (§10.2.11), so `arguments[i]=v` is an unseen write path.
  let may_map_args = !inp.is_strict && set.contains(refs_args, fn_id)
  let never_box = never_box_names(own_scopes, assigned_here, may_map_args)
  let vars_to_box =
    derive_vars_to_box(up, ups, children, declared, never_box, forced_box)

  // (4) Frame-layout reconciliation: shift own bindings past the
  //     captures-first prefix and insert CaptureBindings. Root is
  //     pre-offset by `finalize`, so it is skipped.
  let tree = case is_root || lex.cap_count == 0 {
    True -> tree
    False ->
      insert_captures(
        tree,
        fn_id,
        own_scope_ids,
        lex.cap_count,
        captures,
        const_captures,
        fn_name_captures,
        parent.boxed,
      )
  }

  // (5) Unresolved-name fallthrough (ToGlobal vs ToEvalEnv).
  let fallthrough =
    derive_fallthrough(is_root, seeded_info.fallthrough, up, inp)

  // (6) Write back: Binding.is_boxed on every own scope, then the
  //     FunctionInfo fields this pass decides.
  let tree = apply_boxing(tree, own_scope_ids, vars_to_box)
  let tree =
    update_function_info(tree, fn_id, fn(info) {
      // An OWNING Script root's `lexical` is authoritative from
      // finalize — already offset past parent_names/with_stack. For
      // every other root (Module, direct-eval Script) the locally
      // computed `lexical` (derived from `lexical_captures`) is the
      // correct value; finalize wrote `NoLexicalSlots` for those,
      // and preserving it would strand a direct-eval child arrow's
      // capture lookup at compiler.gleam:557.
      let lexical = case lex.script_root_owns {
        True -> info.lexical
        False -> lex.lexical
      }
      FunctionInfo(
        ..info,
        captures:,
        lexical:,
        lexical_captures: lex.lexical_captures,
        lexical_boxed: lex.lexical_boxed,
        fallthrough:,
        contains_direct_eval: up.own_eval,
        eval_in_subtree: up.eval_in_subtree,
      )
    })

  // (7) Recurse into children with THEIR parent view. Mirrors
  //     compile_children passing resolved.closure_scopes/closure_consts/
  //     closure_fn_names down.
  list.fold(children, tree, fn(tree, cid) {
    let view =
      child_parent_view(
        tree,
        cid,
        captures,
        const_captures,
        fn_name_captures,
        lex.available,
      )
    compute_down(tree, inputs, by_fn, ups, assigned, refs_args, cid, view)
  })
}

/// captures: free names that exist in the parent's view. eval poisons
/// free-var analysis → capture EVERY parent-visible name (compile_child
/// :617-622). Sorted for slot-index determinism.
fn derive_name_captures(
  up: Up,
  parent: ParentView,
) -> #(List(#(String, Int)), Set(String), Set(String)) {
  let parent_name_set = set.from_list(dict.keys(parent.names))
  let captured_names = case up.eval_in_subtree {
    True -> parent_name_set
    False -> set.intersection(up.transitive_free, parent_name_set)
  }
  let captures =
    captured_names
    |> set.to_list
    |> list.sort(string.compare)
    // `captured_names ⊆ keys(parent.names)` by construction above, so the
    // lookup is total — dropping a name here would silently lose a capture.
    |> list.map(fn(name) {
      let assert Ok(parent_slot) = dict.get(parent.names, name)
        as "scope.captures: captured name absent from parent view"
      #(name, parent_slot)
    })
  let const_captures = set.intersection(parent.consts, captured_names)
  let fn_name_captures = set.intersection(parent.fn_names, captured_names)
  #(captures, const_captures, fn_name_captures)
}

/// Decide the lexical pseudo-slot (this / active_func / home_object /
/// new.target) layout for one function scope.
///
/// lexical_captures: arrows capture lexical refs they (or eval) reference
/// AND the parent has available; non-arrows own all four (compile_child
/// :638-662). Slots are name_cap_count..name_cap_count+k in canonical
/// order. The ROOT mostly captures nothing (it has no parent), but
/// direct-eval seeds its FunctionInfo with `opts.lexical_captures` (the
/// CALLER's boxed `this`/new.target/… slots) — preserve that seed; wiping
/// it to `dict.new()` would route `eval('this')` to the no-slot
/// `JsUndefined` fallback in get_lexical. A non-direct-eval Script root
/// OWNS its own four pseudo-slots (see `owns_lexical` below), so it
/// advertises ALL refs as available to arrow children even though it
/// captures none.
///
/// Root ownership is derived from the SEEDED `info.lexical` finalize
/// already wrote (Some(root_base..) iff `root_base == 0`) — NOT from
/// `dict.is_empty(lexical_captures)`. A direct-eval root can have empty
/// lexical_captures with NON-empty parent_names (caller has locals but
/// no lexical slots, e.g. an arrow at Module top-level); re-deriving
/// ownership here from lex_base=0 would assign `this -> slot 0`,
/// colliding with the first parent_names box ref.
fn derive_lexical_layout(
  is_root: Bool,
  kind: ScopeKind,
  inp: FnAnalysisInput,
  up: Up,
  seeded: FunctionInfo,
  parent: ParentView,
  name_cap_count: Int,
  children: List(ScopeId),
  inputs: Dict(ScopeId, FnAnalysisInput),
) -> LexLayout {
  let seeded_root_owns_lexical = case seeded.lexical {
    lexical.OwnedLexicalSlots(_) -> True
    lexical.CapturedLexicalSlots(..) | lexical.NoLexicalSlots -> False
  }
  let script_root_owns = is_root && kind == Script && seeded_root_owns_lexical
  let #(lexical_captures, available) = case is_root, inp.is_arrow {
    True, _ -> {
      let seeded_caps = seeded.lexical_captures
      let available = case script_root_owns {
        True -> lexical.every_lexical_ref
        False -> lexical_refs_present(seeded_caps)
      }
      #(seeded_caps, available)
    }
    False, False -> #(dict.new(), lexical.every_lexical_ref)
    False, True -> {
      let #(m, _next) =
        list.fold(
          lexical.all_lexical_refs,
          #(dict.new(), name_cap_count),
          fn(st, ref) {
            let #(m, i) = st
            let referenced =
              up.eval_in_subtree
              || lexical.lexical_refs_get(inp.lexical_refs, ref)
            let available =
              lexical.lexical_refs_get(parent.lexical_available, ref)
            case referenced && available {
              True -> #(dict.insert(m, ref, i), i + 1)
              False -> st
            }
          },
        )
      #(m, lexical_refs_present(m))
    }
  }

  // Owned lexical pseudo-slots (this / active_func / home_object /
  // new.target). A non-arrow Function or ClassStaticBlock OWNS all four
  // — the call prologue writes them at frame entry. Arrows own NONE
  // (they resolve via `lexical_captures` instead). The Module root owns
  // NONE (`this === undefined` per §16.2.1.6.4); a direct-eval Script
  // root owns NONE (its lexical refs are caller-supplied via
  // `opts.lexical_captures`); but a non-direct-eval Script root OWNS all
  // four — the runtime seeds them at frame entry just like a function
  // call, so `this` at script top-level resolves to globalThis. Allocated
  // immediately after name-captures + lexical_captures and BEFORE declared
  // bindings, mirroring the legacy resolver's `DeclareLexical(RefThis/…)`
  // prologue order — CAPTURE_EVAL_SPEC.md §2.5 step (iii). Without this,
  // emit.resolve_lexical panics ("scope tree has no slot for lexical ref").
  let lex_base = name_cap_count + dict.size(lexical_captures)
  let owns_lexical = case kind {
    Function -> !inp.is_arrow
    ClassStaticBlock -> True
    Script -> script_root_owns
    Module | Block | Catch | With(_) | ClassBody -> False
  }
  let #(slots, own_lexical_count) = case owns_lexical {
    // Non-owners (arrows; Module/Script root) have no OWN lexical
    // pseudo-slots — but per CAPTURE_EVAL_SPEC.md §2.5 (ii) / §2.6
    // `F.lexical` must still expose the CAPTURED refs' slots so a
    // nested arrow's `parent_info.lexical` lookup at compiler.gleam
    // succeeds (it has no lexical_captures fallback). For the root
    // and non-arrow non-owners `lexical_captures` is empty, so this
    // degenerates to `NoLexicalSlots`.
    False -> #(
      lexical.captured_lexical_slots(
        this: dict.get(lexical_captures, RefThis) |> option.from_result,
        active_func: dict.get(lexical_captures, RefActiveFunc)
          |> option.from_result,
        home_object: dict.get(lexical_captures, RefHomeObject)
          |> option.from_result,
        new_target: dict.get(lexical_captures, RefNewTarget)
          |> option.from_result,
      ),
      0,
    )
    True -> #(
      lexical.OwnedLexicalSlots(base: lex_base),
      lexical.owned_lexical_slot_count,
    )
  }

  // lexical_captured: which lexical slots THIS body must box. eval → all
  // (compile_child :676-679). Otherwise OR of arrow children's
  // lexical_refs (collect_arrow_lexical_refs).
  let lexical_captured = case up.eval_in_subtree {
    True -> lexical.every_lexical_ref
    False ->
      list.fold(children, lexical.no_lexical_refs, fn(refs, cid) {
        let cinp = get_input(inputs, cid)
        case cinp.is_arrow {
          True -> lexical.lexical_refs_or(refs, cinp.lexical_refs)
          False -> refs
        }
      })
  }
  // lexical_boxed: for owners, exactly `lexical_captured` (an owned slot
  // is boxed iff some arrow child / eval captures it). For NON-owners,
  // `lexical` now points at the capture slots themselves (see above),
  // and capture slots always hold boxes — so every ref present in
  // `lexical_captures` must read as boxed via lookup_lexical, on top of
  // whatever arrow-grandchildren capture transitively.
  let lexical_boxed = case owns_lexical {
    True -> lexical_captured
    False ->
      lexical.lexical_refs_or(
        lexical_captured,
        lexical_refs_present(lexical_captures),
      )
  }

  LexLayout(
    lexical: slots,
    lexical_captures:,
    lexical_boxed:,
    cap_count: lex_base + own_lexical_count,
    available:,
    script_root_owns:,
  )
}

/// vars_to_box: with eval in subtree, every declared local. Otherwise the
/// union over children of (eval-child → every declared local; else
/// child.transitive_free ∩ declared) — exactly collect_all_captured_vars.
/// Note the intersection is with the FULL declared set, not visible-at-
/// child: faithful to the old IR scan, which may over-box a same-named
/// binding declared only in a sibling block.
///
/// `forced_box` (module top-level exports — `linker_seeded`) are ALWAYS
/// boxed regardless of capture — the linker pre-allocates each export's
/// BoxSlot and seeds the slot with `JsObject(box)` before the body runs,
/// so reads/writes MUST go through IrGetBoxed/IrPutBoxed to hit the
/// shared cell (CAPTURE_EVAL_SPEC.md §2.3, old scope.gleam :458-459).
/// Without this an un-captured export would emit IrPutLocal, writing the
/// value into the local slot itself and leaving the linker's box stuck at
/// its TDZ seed.
fn derive_vars_to_box(
  up: Up,
  ups: Dict(ScopeId, Up),
  children: List(ScopeId),
  declared: Set(String),
  never_box: Set(String),
  forced_box: Set(String),
) -> Set(String) {
  let vars_to_box = case up.eval_in_subtree {
    True -> declared
    False ->
      list.fold(children, set.new(), fn(s, cid) {
        let cu = get_up(ups, cid)
        case cu.eval_in_subtree {
          True -> set.union(s, declared)
          False ->
            set.intersection(cu.transitive_free, declared) |> set.union(s)
        }
      })
      // Capture-by-value: never-reassigned params/catch/fn-name skip boxing
      // (child captures the raw value, not a heap cell). eval poisons this
      // (the eval'd code could assign), so only in the non-eval arm.
      |> set.difference(never_box)
  }
  set.union(vars_to_box, forced_box)
}

/// Names declared in `own_scopes` that are safe to capture BY VALUE (skip
/// boxing) when never reassigned: their kind guarantees the binding is
/// initialised before any body code runs (so no closure can capture a
/// stale pre-init value), and `assigned_here` proves no later write.
/// Let/Const/Var are excluded — their declarator-init writes at statement
/// position, potentially AFTER a capturing closure is created.
///
/// Name-granularity is deliberately conservative: `poison` is every name
/// with a Var/Let/Const/Capture binding ANYWHERE in `own_scopes`, so a
/// param/catch shadowed by a same-named block-let (or a `var` re-decl on
/// a param, which lands in the var-boundary scope for non-simple param
/// lists) never enters the result. `may_map_args` (sloppy body that
/// references `arguments`) poisons ALL params — `arguments[i]=v` is an
/// alias write the assign-ref pass cannot see.
fn never_box_names(
  own_scopes: List(Scope),
  assigned_here: Set(String),
  may_map_args: Bool,
) -> Set(String) {
  let #(safe, poison) = {
    use acc, scope <- list.fold(own_scopes, #(set.new(), set.new()))
    use acc, name, b <- dict.fold(scope.bindings, acc)
    let #(safe, poison) = acc
    case b.kind {
      ParamBinding ->
        case may_map_args || set.contains(assigned_here, name) {
          False -> #(set.insert(safe, name), poison)
          True -> acc
        }
      CatchBinding | FnNameBinding ->
        case set.contains(assigned_here, name) {
          False -> #(set.insert(safe, name), poison)
          True -> acc
        }
      VarBinding | LetBinding | ConstBinding | CaptureBinding -> #(
        safe,
        set.insert(poison, name),
      )
    }
  }
  set.difference(safe, poison)
}

/// fallthrough: sloppy + eval-in-subtree → unresolved names check the
/// eval_env first (compile_child :683-686). The ROOT's fallthrough is
/// caller-supplied via `opts.fallthrough` (e.g. compile_eval_direct
/// passes ToEvalEnv even when the eval'd source has no nested eval) —
/// recomputing it here would clobber that with ToGlobal, so preserve it.
fn derive_fallthrough(
  is_root: Bool,
  seeded: GlobalFallthrough,
  up: Up,
  inp: FnAnalysisInput,
) -> GlobalFallthrough {
  use <- bool.guard(is_root, seeded)
  case up.eval_in_subtree && !inp.is_strict {
    True -> ToEvalEnv
    False -> ToGlobal
  }
}

/// Reconcile the frame layout: shift every declared binding in `fn_id`'s
/// scopes up by `cap_count` so captures occupy slots 0..N-1 and lexical
/// captures N..N+K-1, then insert a `CaptureBinding` per name capture into
/// the function-root scope. Mirrors the legacy resolver's
/// `resolve_with_captures`, which received captures FIRST and allocated own
/// bindings AFTER them. Also shifts `FunctionInfo.names` (slot-keyed for
/// module-export / direct-eval lookup) and bumps `local_count` so
/// `alloc_scratch` allocates past the captures. `ScopeKind.With(holder)`
/// carries a NAME, not a slot, so it needs no shift — `do_lookup` reads the
/// holder's (already-shifted) slot out of `scope.bindings`.
fn insert_captures(
  tree: ScopeTree,
  fn_id: ScopeId,
  own_scope_ids: List(ScopeId),
  cap_count: Int,
  captures: List(#(String, Int)),
  const_captures: Set(String),
  fn_name_captures: Set(String),
  parent_boxed: Set(String),
) -> ScopeTree {
  // Shift every declared binding's slot in this function's scopes.
  let scopes =
    list.fold(own_scope_ids, tree.scopes, fn(scopes, sid) {
      let scope = scopes_get_or_panic(scopes, sid)
      let bindings =
        dict.map_values(scope.bindings, fn(_name, b) {
          Binding(..b, slot: b.slot + cap_count)
        })
      dict.insert(scopes, sid, Scope(..scope, bindings:))
    })
  // A capture whose name is ALSO declared by this function (e.g. an
  // eval-poisoned body that declares `var x` while the parent has `x`)
  // is SHADOWED: the own declaration wins and the capture, though its
  // slot stays reserved and is still filled from `env_descriptors[i]`,
  // is not nameable from inside this frame.
  //
  // The two writes below need DIFFERENT shadow sets, because they have
  // different reach:
  //
  //   * `root_shadowed` gates the CaptureBinding insert. `do_lookup` from
  //     any scope in this function walks up to the root, so a root
  //     binding hides the capture everywhere — but a binding in a nested
  //     block must NOT, or a reference outside that block would lose the
  //     capture entirely.
  //   * `names_shadowed` gates the `FunctionInfo.names` write, and adds
  //     the §10.2.11 step-28 var-boundary body Block. That Block is where
  //     `sb_var_target` sinks every body-level `var` / hoisted function of
  //     a NON-SIMPLE-parameter function; with a simple parameter list the
  //     very same bindings would sit in the root and shadow. `names` is a
  //     flat name→slot map with no block structure, so it cannot express
  //     "capture at the root, own binding inside the block" — and letting
  //     the capture win there makes direct eval in the body read the
  //     parent's box instead of the body's `var`.
  //
  // `names` and `do_lookup` therefore still disagree in the two places the
  // flat map has nowhere to record: a name declared only in an ordinary
  // nested block, and a direct eval in a PARAMETER initializer naming a
  // body `var` (it now sees the body slot, not the capture). Both are
  // strictly rarer than the body-eval case this gate exists to get right.
  let scope_bindings = fn(sid) { scopes_get_or_panic(scopes, sid).bindings }
  let root_bindings = scope_bindings(fn_id)
  let root_shadowed = fn(name) { dict.has_key(root_bindings, name) }
  let var_boundary_names =
    own_scope_ids
    |> list.filter(fn(sid) { scopes_get_or_panic(scopes, sid).is_var_boundary })
    |> list.flat_map(fn(sid) { dict.keys(scope_bindings(sid)) })
    |> set.from_list
  let names_shadowed = fn(name) {
    root_shadowed(name) || set.contains(var_boundary_names, name)
  }
  // Insert CaptureBinding entries at slots 0..N-1 in the function-root
  // scope so `do_lookup` resolves captured names to a Local in THIS frame
  // and `visible_at_creation` exposes them to grandchildren. Captures are
  // always boxed (heap cells shared with the parent). Origin kind threads
  // const / FnName through the chain so a write to a captured-captured
  // const still throws TypeError (§9.1.1.1.5).
  let root = scopes_get_or_panic(scopes, fn_id)
  let scopes = {
    let bindings =
      list.index_fold(captures, root.bindings, fn(bs, cap, i) {
        let #(name, _parent_slot) = cap
        use <- bool.guard(root_shadowed(name), bs)
        let origin = case
          set.contains(const_captures, name),
          set.contains(fn_name_captures, name)
        {
          True, _ -> ConstBinding
          False, True -> FnNameBinding
          False, False -> CaptureBinding
        }
        dict.insert(
          bs,
          name,
          Binding(
            slot: i,
            kind: CaptureBinding,
            // Mirror the parent's boxing: a by-value capture (parent slot
            // holds the raw value, not a cell) reads cap_i directly.
            is_boxed: set.contains(parent_boxed, name),
            origin_kind_for_capture: origin,
          ),
        )
      })
    dict.insert(scopes, fn_id, Scope(..root, bindings:))
  }
  // Shift the per-function names map and bump local_count; record capture
  // names at their new slots so direct-eval `local_names` exposes them.
  // The wider `names_shadowed` gate (see above): a shadowed name keeps its
  // (already-shifted) own-declaration slot.
  let info = function_info(tree, fn_id)
  let names =
    dict.map_values(info.names, fn(_n, slot) { slot + cap_count })
    |> list.index_fold(captures, _, fn(d, cap, i) {
      case names_shadowed(cap.0) {
        True -> d
        False -> dict.insert(d, cap.0, i)
      }
    })
  let functions =
    dict.insert(
      tree.functions,
      fn_id,
      FunctionInfo(..info, local_count: info.local_count + cap_count, names:),
    )
  ScopeTree(..tree, scopes:, functions:)
}

/// Build the ParentView a child sees: this body's captures (always
/// function-scoped, slot 0..N-1) plus this body's own bindings visible at
/// the child's creation site. Const/FnName origin propagates via
/// `Binding.origin_kind_for_capture` — `insert_captures` set that on each
/// CaptureBinding from THIS body's `const_captures` / `fn_name_captures`,
/// so a captured-captured const still reads as const-origin here. A local
/// re-declaration of a captured name shadows the capture's origin
/// (innermost wins — matches the legacy flatten_kinds).
fn child_parent_view(
  tree: ScopeTree,
  child_fn_id: ScopeId,
  our_captures: List(#(String, Int)),
  our_const_captures: Set(String),
  our_fn_name_captures: Set(String),
  lexical_available: LexicalRefs,
) -> ParentView {
  // Our captures occupy our slots 0..N-1 in capture-list order. After
  // `insert_captures` they ALSO appear in the function-root scope's
  // bindings (so `own_visible` picks them up), but the root's bindings
  // are skipped when fn_id == root_scope_id, so seed from the explicit
  // list to cover both cases.
  let cap_names =
    list.index_map(our_captures, fn(c, i) { #(c.0, i) }) |> dict.from_list
  let own_visible = visible_at_creation(tree, child_fn_id)
  // Innermost shadow wins: own bindings overlay captures.
  let names =
    dict.fold(own_visible, cap_names, fn(d, name, b) {
      dict.insert(d, name, b.slot)
    })
  let own_names = set.from_list(dict.keys(own_visible))
  // Origin kind, NOT kind: CaptureBindings inserted by `insert_captures`
  // have kind == CaptureBinding but origin_kind_for_capture == ConstBinding
  // / FnNameBinding when the chain's origin is one. Checking `b.kind`
  // would drop that origin once a capture appears in own_visible.
  let origin_names = fn(kind, inherited) {
    let own =
      dict.fold(own_visible, set.new(), fn(s, name, b) {
        case b.origin_kind_for_capture == kind {
          True -> set.insert(s, name)
          False -> s
        }
      })
    set.union(own, set.difference(inherited, own_names))
  }
  let consts = origin_names(ConstBinding, our_const_captures)
  let fn_names = origin_names(FnNameBinding, our_fn_name_captures)
  // `own_visible` reflects apply_boxing + insert_captures already run for
  // this function, so each Binding.is_boxed is authoritative here.
  let boxed =
    dict.fold(own_visible, set.new(), fn(s, name, b) {
      case b.is_boxed {
        True -> set.insert(s, name)
        False -> s
      }
    })
  ParentView(names:, consts:, fn_names:, boxed:, lexical_available:)
}

/// Set `is_boxed` on every binding in `fn_id`'s scope subtree whose name is
/// in `vars_to_box`. Iterates only this function's own scopes (via the
/// precomputed `scopes_by_function` index) — `dict.map_values` over the
/// whole tree per function is O(F × S).
fn apply_boxing(
  tree: ScopeTree,
  own_scope_ids: List(ScopeId),
  vars_to_box: Set(String),
) -> ScopeTree {
  use <- bool.guard(set.is_empty(vars_to_box), tree)
  let scopes =
    list.fold(own_scope_ids, tree.scopes, fn(scopes, sid) {
      let scope = scopes_get_or_panic(scopes, sid)
      let bindings =
        dict.map_values(scope.bindings, fn(name, b) {
          case set.contains(vars_to_box, name) {
            True -> Binding(..b, is_boxed: True)
            False -> b
          }
        })
      dict.insert(scopes, sid, Scope(..scope, bindings:))
    })
  ScopeTree(..tree, scopes:)
}

/// `get_scope` for a bare `scopes` map mid-rewrite (the folds in
/// `insert_captures` / `apply_boxing` thread an updated map that isn't in a
/// `ScopeTree` yet). Every id they iterate came from `scopes_by_function`,
/// which is built FROM this map, so a miss is a bug — swallowing it would
/// leave a scope's slots unshifted / a var unboxed and silently miscompile
/// the surrounding function.
fn scopes_get_or_panic(scopes: Dict(ScopeId, Scope), sid: ScopeId) -> Scope {
  let assert Ok(scope) = dict.get(scopes, sid)
    as "scope.scopes_get_or_panic: unknown ScopeId"
  scope
}

fn update_function_info(
  tree: ScopeTree,
  fn_id: ScopeId,
  f: fn(FunctionInfo) -> FunctionInfo,
) -> ScopeTree {
  let info = function_info(tree, fn_id)
  ScopeTree(..tree, functions: dict.insert(tree.functions, fn_id, f(info)))
}

fn lexical_refs_present(d: Dict(LexicalRef, a)) -> LexicalRefs {
  lexical.LexicalRefs(
    this: dict.has_key(d, RefThis),
    active_func: dict.has_key(d, RefActiveFunc),
    home_object: dict.has_key(d, RefHomeObject),
    new_target: dict.has_key(d, RefNewTarget),
  )
}

fn get_input(
  inputs: Dict(ScopeId, FnAnalysisInput),
  fn_id: ScopeId,
) -> FnAnalysisInput {
  // `build_capture_inputs` post-order-walks the same `child_function_scopes`
  // tree compute_up/compute_down walk, inserting an entry for every function
  // scope it visits — so every fn_id reaching here is present.
  let assert Ok(i) = dict.get(inputs, fn_id)
    as "scope.analyze_captures: build_capture_inputs missed a function scope"
  i
}

fn get_up(ups: Dict(ScopeId, Up), fn_id: ScopeId) -> Up {
  let assert Ok(u) = dict.get(ups, fn_id)
    as "scope.analyze_captures: missing bottom-up result"
  u
}

// ---- tree-derived helpers --------------------------------------------------

/// Names declared in the given scopes (the precomputed member-scope list
/// of one function). Used by compute_up/compute_down over the
/// `scopes_by_function` index to avoid re-scanning the whole tree.
fn declared_in(scopes: List(Scope)) -> Set(String) {
  use s, scope <- list.fold(scopes, set.new())
  dict.fold(scope.bindings, s, fn(s, name, _b) { set.insert(s, name) })
}

/// With-object holder synthetics (`<withN_M>`) active at `fn_id`'s
/// creation site that this body must capture as free names — every one of
/// them is a name the body needs to do its IrWith* probe. Walks the scope
/// chain from `fn_id`'s parent to the root, collecting each `With` scope's
/// holder name off its kind.
fn fn_with_stack_free(
  tree: ScopeTree,
  fn_id: ScopeId,
  declared: Set(String),
) -> Set(String) {
  let start = { get_scope(tree, fn_id) }.parent
  { fold_enclosing_withs(tree, start, set.new(), set.insert) }
  |> set.difference(declared)
}

/// Bindings of the PARENT function visible at `child_fn_id`'s creation site:
/// walk from the child's parent scope outward, collecting bindings, until
/// (and including) the parent function's root scope. Innermost shadow wins.
/// Mirrors flatten_scopes / scan_free's `in_scope` set at IrMakeClosure.
fn visible_at_creation(
  tree: ScopeTree,
  child_fn_id: ScopeId,
) -> Dict(String, Binding) {
  let child = get_scope(tree, child_fn_id)
  case child.parent {
    None -> dict.new()
    Some(parent_id) -> {
      let parent_fn = { get_scope(tree, parent_id) }.function_scope
      collect_visible(tree, parent_id, parent_fn, dict.new())
    }
  }
}

fn collect_visible(
  tree: ScopeTree,
  scope_id: ScopeId,
  stop_at_fn: ScopeId,
  acc: Dict(String, Binding),
) -> Dict(String, Binding) {
  let scope = get_scope(tree, scope_id)
  // Innermost wins: only insert names not already present (we walk
  // inner→outer).
  let acc =
    dict.fold(scope.bindings, acc, fn(d, name, b) {
      case dict.has_key(d, name) {
        True -> d
        False -> dict.insert(d, name, b)
      }
    })
  case scope_id == stop_at_fn, scope.parent {
    // Reached the parent function's own scope — its bindings are folded
    // in above; stop (don't walk into the GRANDparent function).
    True, _ -> acc
    False, None -> acc
    False, Some(p) ->
      case { get_scope(tree, p) }.function_scope == stop_at_fn {
        True -> collect_visible(tree, p, stop_at_fn, acc)
        // Parent belongs to a different function — boundary reached.
        False -> acc
      }
  }
}
