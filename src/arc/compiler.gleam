/// Bytecode Compiler
///
/// Translates a parsed AST into a FuncTemplate the VM can interpret.
/// Three-phase pipeline:
///   Phase 1 (analyze): AST → ScopeTree (binding resolution, capture/box analysis)
///   Phase 2 (emit):    AST + ScopeTree → IrOp (concrete slot ops, label IDs)
///   Phase 3 (resolve): IrOp → Op (absolute PC addresses)
///
/// Phase 1 runs ONCE for the whole compilation unit (top-level body plus
/// every nested function), producing a single `scope.ScopeTree`. Phase 2
/// then walks the AST consulting that tree to emit concrete GetLocal /
/// GetBoxed / GetGlobal / IrWith* ops directly — there is no symbolic-name
/// pass over the IR. Per-function metadata (local_count, captures, lexical
/// slots, name table, eval flags) lives in `scope.FunctionInfo`, looked up
/// by the function's scope_id in the tree.
import arc/compiler/ast_util
import arc/compiler/emit
import arc/compiler/resolve
import arc/compiler/scope
import arc/esm
import arc/parser/ast
import arc/vm/internal/tuple_array
import arc/vm/opcode.{type CodeKind, type LexicalSlots}
import arc/vm/value.{
  type EvalNameTable, type FuncTemplate, type JsValue, type VarEnvKind,
  CaptureLocal, EvalNameTable, FuncTemplate, GlobalVarEnv, JsUndefined,
  JsUninitialized,
}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set

/// Compilation errors. This is `emit.EmitError` — the emitter's error sum is
/// THE compile-error type; there is deliberately no mirror type to keep in
/// sync. See `emit.EmitError` for what each variant means.
pub type CompileError =
  emit.EmitError

/// Canonical human-readable rendering of a `CompileError`. Every layer that
/// surfaces a compile error to a user (CLI, REPL, engine, module loader,
/// realm `eval`) must go through this — mirrors `parser.parse_error_to_string`.
///
/// `EarlySyntaxError` is rendered VERBATIM: the message becomes the thrown
/// SyntaxError's message with no "unsupported:" (or any other) prefix.
pub fn error_message(err: CompileError) -> String {
  case err {
    emit.BreakOutsideLoop -> "break outside loop"
    emit.ContinueOutsideLoop -> "continue outside loop"
    emit.EarlySyntaxError(message:) -> message
    emit.UnsupportedFeature(feature:) -> "unsupported: " <> feature
    // The engine-bug variants: an AST shape the parser guarantees can never
    // reach the emitter. Rendered with a stable "internal compiler error"
    // prefix so they read the same as the old stringly `Internal(context)`.
    emit.NonMemberLValue -> internal_error("non-member lvalue")
    emit.AnonymousClassDeclaration ->
      internal_error("anonymous class declaration")
    emit.NonCompoundAssignOperator ->
      internal_error("non-compound operator in compound assignment")
    emit.MultiDeclaratorForHead ->
      internal_error("for-in/of head with multiple declarators")
    emit.AccessorInDestructuringPattern ->
      internal_error("accessor/method in destructuring assignment")
    emit.NonMemberDefaultTarget -> internal_error("keyed member default target")
    emit.BareSuperExpression -> internal_error("bare super expression")
    emit.BareSpreadElement -> internal_error("bare spread element")
    emit.InvalidUpdateTarget -> internal_error("invalid ++/-- target")
    emit.InvalidCompoundAssignTarget ->
      internal_error("invalid compound-assignment target")
    emit.NonIdentifierStaticMember ->
      internal_error("non-identifier static member property")
    emit.NonGenericUnaryOperator ->
      internal_error("typeof/delete in generic unary expression")
  }
}

fn internal_error(context: String) -> String {
  "internal compiler error: " <> context
}

/// The direct-eval CALLER's strictness (§19.2.1.1 PerformEval step 2's
/// `strictCaller`). Named rather than `Bool` so it can't be transposed with
/// the other flags `compile_eval_direct` used to take positionally.
pub type Strictness {
  Strict
  Sloppy
}

/// Everything `compile_eval_direct` needs to know about the frame the direct
/// eval runs in. Bundled (and labelled) so the four same-typed name lists
/// can't be swapped for one another at the call site.
pub type DirectEvalCaller {
  DirectEvalCaller(
    /// The caller's local binding names, in the order run_direct_eval seeds
    /// their box refs into capture slots 0..N-1.
    names: List(String),
    /// The caller's lexical pseudo-slots (`this` / `new.target` / …); each
    /// `Some` entry becomes one further capture slot after `names`.
    slots: LexicalSlots,
    /// What kind of code the caller's body is — decides what syntax the eval
    /// body may use (§19.2.1.1 step 6).
    code_kind: CodeKind,
    /// `strictCaller` — a strict caller upgrades the eval body to strict.
    strictness: Strictness,
    /// Whether the caller's VariableEnvironment is the global environment.
    var_env: VarEnvKind,
    /// Formal-parameter-scope bindings visible at the eval call site
    /// (EvalDeclarationInstantiation step 3.d).
    param_scope_names: List(String),
    /// The caller's enclosing `with`-object holders, by binding name.
    with_names: List(String),
    /// The caller's PrivateEnvironment chain (§19.2.1.1 step 5).
    private_names: List(String),
  )
}

/// Phase 3 for a top-level body (script, module, or eval): unnamed, zero
/// arity, no env captures, and not an arrow/constructor/generator/async.
fn resolve_top_level(
  code: List(opcode.IrOp),
  constants: List(JsValue),
  info: scope.FunctionInfo,
  child_templates: List(FuncTemplate),
  is_strict: Bool,
  code_kind: CodeKind,
  local_names: Option(EvalNameTable),
) -> FuncTemplate {
  let #(bytecode, constants) = resolve.resolve(code, constants)
  FuncTemplate(
    name: None,
    arity: 0,
    length: 0,
    local_count: info.local_count,
    bytecode:,
    constants:,
    functions: tuple_array.from_list(child_templates),
    env_descriptors: [],
    is_strict:,
    is_arrow: False,
    is_derived_constructor: False,
    is_generator: False,
    is_async: False,
    is_constructor: False,
    is_class_constructor: False,
    local_names:,
    lexical: info.lexical,
    code_kind:,
  )
}

/// Compile a parsed SCRIPT body into a FuncTemplate the VM can interpret.
/// `sb` is the scope-builder accumulated by the parser alongside the AST;
/// scope analysis now finalizes that pre-built tree instead of re-walking
/// the AST, so the parser is the SOLE producer of scope structure.
///
/// Module bodies go through `compile_module`: its `CompiledModuleBody`
/// carries the `export_names` / `hoisted_funcs` / `export_seeds` a module
/// cannot be linked (or run) without, so handing a module body to `compile`
/// is not a runtime error — it does not typecheck.
pub fn compile(
  body: List(ast.StmtWithLine),
  sb: scope.ScopeBuilder,
) -> Result(FuncTemplate, CompileError) {
  compile_script(body, sb, scope.LexLocal, deletable_global_vars: False)
}

/// Everything `compile_module` produces for one ES module body: the root
/// FuncTemplate plus the compile-time scope information the module linker
/// needs afterwards.
pub type CompiledModuleBody {
  CompiledModuleBody(
    /// The module body's compiled function template.
    template: FuncTemplate,
    /// The module root scope's name → local-slot map. The linker looks up
    /// each EXPORTED local name here to find the slot whose BoxSlot it
    /// shares with importers (§16.2 live bindings). It carries every
    /// root-scope binding, not only exports; the linker only queries the
    /// exported ones.
    export_names: Dict(String, Int),
    /// Top-level hoisted FunctionDeclarations as `(name, func_index)` into
    /// `template.functions`, in source order. The linker instantiates the
    /// exported ones before evaluation so importers observe the closures.
    hoisted_funcs: List(#(String, Int)),
    /// Exported local name → the value the linker pre-seeds into its BoxSlot
    /// before the body runs (§16.2 instantiation): `undefined` for var and
    /// function declarations (hoisted, never TDZ), `uninitialized` for
    /// let/const/class and the default export (TDZ until initialized).
    export_seeds: Dict(String, JsValue),
    /// [[HasTLA]] (§16.2.1.5): the module body contains a top-level `await`.
    /// An `await` inside a nested function compiles into that function's own
    /// child template, so an Await opcode in the module-root bytecode is
    /// exactly a top-level await. Set here — the layer that emits the opcode —
    /// so callers never scan bytecode.
    has_tla: Bool,
  )
}

/// Compile a module body to a `CompiledModuleBody`.
///
/// Imports are compiled as pre-boxed captures in slots 0..N-1 (the same
/// mechanism direct eval uses to alias a caller's variables). At link time
/// the module's import slots are seeded with the *exporting module's* BoxSlot
/// refs, so reads of an imported name dereference the live cell — ES live
/// bindings (§16.2). Exported local bindings are force-boxed so their BoxSlot
/// can be shared with importers. Mirrors QuickJS's JSVarRef aliasing.
pub fn compile_module(
  items: List(ast.ModuleItem),
  sb: scope.ScopeBuilder,
  summary: esm.ModuleSummary,
) -> Result(CompiledModuleBody, CompileError) {
  let import_locals = esm.import_local_names(summary)
  let forced_box = local_export_names(summary.exports)
  compile_module_with_scope(items, sb, import_locals, forced_box, summary)
}

/// Local binding names that a module exports (LocalExport only). These must be
/// force-boxed so the exported cell is a heap BoxSlot shareable with importers.
fn local_export_names(exports: List(esm.ExportEntry)) -> List(String) {
  list.filter_map(exports, fn(entry) {
    case entry {
      esm.LocalExport(local_name:, ..) -> Ok(local_name)
      _ -> Error(Nil)
    }
  })
}

/// The value the linker pre-seeds into each exported local's BoxSlot before the
/// module body runs (§16.2 instantiation): `undefined` for var and function
/// declarations (hoisted, never TDZ), `uninitialized` for let/const/class and
/// the default export (TDZ until the body initializes them). Computed here,
/// alongside the bytecode, and handed to the linker on `CompiledModuleBody`.
fn module_export_seeds(
  items: List(ast.ModuleItem),
  exports: List(esm.ExportEntry),
) -> Dict(String, JsValue) {
  // The `undefined`-seeded names are exactly the module-level VarDeclaredNames
  // plus the top-level FunctionDeclarations — the same ast_util helpers the
  // emitter drives instantiation from, so `export {x}; if (c) var x` sees `x`
  // as var-declared (the shallow walk this replaced did not descend into
  // compound statements). `module_items_to_stmts` also lowers a NAMED
  // `export default function fn(){}` to a FunctionDeclaration, so `fn` seeds
  // `undefined`.
  //
  // KNOWN GAP: an ANONYMOUS `export default function () {}` is a var-scoped
  // hoistable declaration too (§16.2.1.6.4), so `*default*` should also seed
  // `undefined` and be initialized before the body runs. It currently seeds
  // TDZ and is assigned in place. Fixing that needs the parser to tag the
  // anonymous function's scope `TagFnDecl` and `module_items_to_stmts` to
  // lower it to a FunctionDeclaration — while still naming the closure
  // "default", not "*default*" — so it is left alone here rather than
  // half-fixed into a silently-`undefined` read.
  let stmts = ast_util.module_items_to_stmts(items)
  let undef =
    set.from_list(list.append(
      ast_util.collect_hoisted_vars(stmts),
      ast_util.direct_fn_names(stmts),
    ))
  local_export_names(exports)
  |> list.fold(dict.new(), fn(acc, name) {
    let seed = case set.contains(undef, name) {
      True -> JsUndefined
      False -> JsUninitialized
    }
    dict.insert(acc, name, seed)
  })
}

fn compile_module_with_scope(
  items: List(ast.ModuleItem),
  sb: scope.ScopeBuilder,
  import_locals: List(String),
  forced_box: List(String),
  summary: esm.ModuleSummary,
) -> Result(CompiledModuleBody, CompileError) {
  // Phase 1: finalize the parser-built scope tree. Imports occupy boxed
  // capture slots 0..N-1 (parent_names); exported locals are linker-seeded —
  // the linker pre-allocates each export's BoxSlot (so it can be shared with
  // importers, including cyclic ones) and seeds it before the body runs, so
  // the analyzer marks them boxed but the emitter skips their init/box
  // prologue.
  let opts =
    scope.AnalyzeOpts(
      ..scope.default_analyze_opts(),
      top_lex: scope.LexLocal,
      strict: True,
      parent_names: indexed_names(import_locals),
      linker_seeded: set.from_list(forced_box),
    )
  let tree = scope.finalize(sb, opts)
  // Phase 2: emit consulting the tree (concrete slot ops, no IrScope*).
  use
    emit.EmitOutput(
      code:,
      constants:,
      children:,
      is_strict:,
      tree:,
      hoisted_funcs:,
    )
  <- result.map(emit.emit_module(items, tree))
  let info = scope.function_info(tree, scope.root_scope_id)
  let child_templates = compile_children(children, tree, scope.root_scope_id)
  let template =
    resolve_top_level(
      code,
      constants,
      info,
      child_templates,
      is_strict,
      opcode.ScriptCode,
      None,
    )
  let has_tla =
    tuple_array.to_list(template.bytecode)
    |> list.any(fn(op) { op == opcode.Await })
  CompiledModuleBody(
    template:,
    export_names: info.names,
    hoisted_funcs:,
    export_seeds: module_export_seeds(items, summary.exports),
    has_tla:,
  )
}

/// Compile in REPL mode: top-level let/const/class go to the global lexical
/// record so they persist across inputs.
pub fn compile_repl(
  body: List(ast.StmtWithLine),
  sb: scope.ScopeBuilder,
) -> Result(FuncTemplate, CompileError) {
  compile_script(body, sb, scope.LexGlobal, deletable_global_vars: False)
}

/// Compile code for an INDIRECT eval call (or any global-scope dynamic
/// evaluation). Top-level var → globalThis, let/const/class → fresh local
/// LexicalEnvironment per §19.2.1.1 PerformEval step 16.
///
/// §19.2.1.3 EvalDeclarationInstantiation passes D = true: an eval-introduced
/// global var / function binding is configurable.
pub fn compile_eval(
  body: List(ast.StmtWithLine),
  sb: scope.ScopeBuilder,
) -> Result(FuncTemplate, CompileError) {
  compile_script(body, sb, scope.LexLocal, deletable_global_vars: True)
}

/// Compile code for a DIRECT eval call. Like compile_eval, but seeds the
/// scope with the caller's local variable names as pre-boxed captures in
/// slots 0..N-1. Free vars matching a parent name emit GetBoxed/PutBoxed
/// against those slots. At runtime, the caller's BoxSlot refs are copied
/// into locals[0..N-1] so reads/writes alias the caller's variables.
///
/// When the caller is sloppy AND the eval'd code is sloppy, `var`
/// declarations at eval top-level emit DeclareEvalVar (land in the caller's
/// eval_env dict) and unresolved names emit GetEvalVar/PutEvalVar (check
/// eval_env before global). When either side is strict, eval gets its own
/// var environment — fall through to globals as before.
pub fn compile_eval_direct(
  body: List(ast.StmtWithLine),
  sb: scope.ScopeBuilder,
  caller: DirectEvalCaller,
) -> Result(FuncTemplate, CompileError) {
  let caller_is_strict = caller.strictness == Strict
  let caller_is_global = caller.var_env == GlobalVarEnv
  // Effective strictness for §19.2.1.1: caller's strictness OR a
  // "use strict" directive in the eval source itself. Computed up
  // front from the AST so the AnalyzeOpts seed (fallthrough/strict)
  // matches the OLD resolver's `caller_is_strict || is_strict` —
  // scope.finalize does NOT scan directives, so seeding `caller_is_strict`
  // alone would route `eval('"use strict"; freeName')` from a sloppy
  // function caller through ToEvalEnv (GetEvalVar) instead of ToGlobal
  // (GetGlobal): different bytecode.
  let effective_strict =
    caller_is_strict || ast_util.has_use_strict_directive(body)
  // Sloppy direct eval shares the caller's VariableEnvironment. For a
  // function caller that is approximated by the frame's eval_env dict;
  // for a GLOBAL caller (script/REPL top level) the VariableEnvironment
  // IS the global environment, so `var` declarations and unresolved
  // names go straight to the global object — no eval_env in play.
  // Caller's parent locals occupy capture slots 0..N-1 by list order.
  let parent_dict = indexed_names(caller.names)
  // Caller's lexical box refs are seeded at slots len(caller.names)+i
  // by run_direct_eval, in canonical order, one per Some entry in
  // caller.slots. Treat each as a capture so get_lexical → GetBoxed.
  let #(lexical_captures, _next) =
    list.fold(
      opcode.all_lexical_refs,
      #(dict.new(), list.length(caller.names)),
      fn(acc, ref) {
        let #(m, i) = acc
        case opcode.lexical_slot(caller.slots, ref) {
          Some(_) -> #(dict.insert(m, ref, i), i + 1)
          None -> acc
        }
      },
    )
  // The caller's enclosing with-object holders are themselves entries
  // in caller.names; their capture-slot indices form the inherited
  // with_stack the analyzer probes before falling through. Every holder
  // MUST be one of caller.names — dropping one silently would compile
  // `with (o) { eval("x") }` to a plain global read.
  let with_stack =
    list.map(caller.with_names, fn(n) {
      let assert Ok(slot) = dict.get(parent_dict, n)
        as "direct-eval caller's with-holder is not one of its local names"
      slot
    })
  // Phase 1: finalize the parser-built scope tree, seeded with the
  // caller's environment.
  let opts =
    scope.AnalyzeOpts(
      ..scope.default_analyze_opts(),
      top_lex: scope.LexLocal,
      fallthrough: case effective_strict || caller_is_global {
        True -> scope.ToGlobal
        False -> scope.ToEvalEnv
      },
      strict: effective_strict,
      parent_names: parent_dict,
      lexical_captures:,
      with_stack:,
    )
  let tree = scope.finalize(sb, opts)
  // §14.11.1 early error: `with` is illegal in strict code. The eval
  // source is parsed sloppy (no directive), so when the CALLER's
  // strictness upgrades this eval to strict, reject any with statement
  // post-parse (anywhere in the body — strict eval makes all nested
  // code strict). A "use strict" directive in the source itself is
  // already rejected by the parser. The analyzer creates a With scope
  // for every `with` at any depth, so scan the tree instead of
  // re-walking the AST.
  use Nil <- result.try(
    case
      caller_is_strict
      && list.any(dict.values(tree.scopes), fn(s) { scope.is_with_kind(s.kind) })
    {
      True -> Error(emit.EarlySyntaxError("'with' not allowed in strict mode"))
      False -> Ok(Nil)
    },
  )
  // Phase 2: emit. The direct-eval entry point folds the contextual
  // inputs into the emitter's initial state so no post-emission op
  // rewriting is needed:
  //   - caller_is_strict → vars become local slots, Annex B suppressed
  //     (§19.2.1.1 step 18 — strict eval gets its own VarEnvironment)
  //   - param_scope_names / private_names → seeded into the emitter so
  //     top-level IrCallEval ops carry them (steps 3.d/5 transitive
  //     through nested direct eval)
  //   - DeclareLexical(RefThis) skipped — lexical pseudo-slots arrive
  //     via the caller's boxed slots (lexical_captures above)
  use
    emit.EmitOutput(
      code:,
      constants:,
      children:,
      is_strict: strict,
      tree:,
      hoisted_funcs: _,
    )
  <- result.try(emit.emit_eval_direct(
    body,
    tree,
    caller_is_strict,
    caller.param_scope_names,
    caller.private_names,
  ))
  // §19.2.1.1 PerformEval → EvalDeclarationInstantiation step 3.d:
  // when this direct eval happens inside a formal-parameter initializer
  // (param_scope_names non-empty), sloppy eval code must not var-declare
  // a name already bound in the parameter scope (parameters + the
  // implicit `arguments` binding). The spec walks the environment chain
  // from the eval's LexicalEnvironment to its VariableEnvironment and
  // throws a SyntaxError on any HasBinding hit — for a parameter
  // initializer the only environment in between is the parameter scope.
  // Strict eval (via caller OR the body's own "use strict") gets its own
  // VariableEnvironment, so no check applies. Computed from the AST's
  // VarDeclaredNames — no IR scan needed.
  use Nil <- result.try(case strict {
    True -> Ok(Nil)
    False -> check_param_scope_var_conflict(body, caller.param_scope_names)
  })
  let info = scope.function_info(tree, scope.root_scope_id)
  let child_templates = compile_children(children, tree, scope.root_scope_id)
  // Expose the name table only when a nested top-level eval needs it —
  // run_direct_eval consults state.func.local_names to decide between
  // direct (aliasing) and indirect fallback semantics. A global caller's
  // VariableEnvironment propagates: a nested eval in this body shares it
  // (see compile_script). Keyed on the analyzer's
  // FunctionInfo.contains_direct_eval — populated by scope.finalize from
  // the per-Scope flags the parser sets, so emit no longer needs to
  // surface a `has_eval_call` side-channel.
  let local_names = case info.contains_direct_eval {
    True ->
      Some(EvalNameTable(
        var_env: caller.var_env,
        names: dict.to_list(info.names),
      ))
    False -> None
  }
  // The template's strictness must reflect how the body was COMPILED
  // (caller strictness upgrades the body — vars were rewritten to
  // locals, Annex B dropped), not just the body's own directive, so a
  // nested eval inherits the effective strictness at runtime.
  Ok(resolve_top_level(
    code,
    constants,
    info,
    child_templates,
    strict,
    caller.code_kind,
    local_names,
  ))
}

fn compile_script(
  stmts: List(ast.StmtWithLine),
  sb: scope.ScopeBuilder,
  top_lex: scope.TopLevelLex,
  // §9.1.1.4.17 CreateGlobalVarBinding's D argument for the unit's
  // top-level var / hoisted-function globals: False for a script / the
  // REPL (§16.1.7 step 18), True for global eval code (§19.2.1.3 step 17
  // — an eval-introduced global var / function IS deletable).
  deletable_global_vars deletable_global_vars: Bool,
) -> Result(FuncTemplate, CompileError) {
  // Phase 1: finalize the parser-built scope tree (top-level body + every
  // nested function), computing capture sets, boxing decisions, and slot
  // indices BEFORE any IR is emitted.
  let opts = scope.AnalyzeOpts(..scope.default_analyze_opts(), top_lex:)
  let tree = scope.finalize(sb, opts)
  // Phase 2: emit IR from AST, consulting the tree for every name reference
  // — no symbolic IrScope* ops, no second resolution pass.
  use
    emit.EmitOutput(
      code:,
      constants:,
      children:,
      is_strict:,
      tree:,
      hoisted_funcs: _,
    )
  <- result.map(emit.emit_program(stmts, tree, deletable_global_vars:))
  let info = scope.function_info(tree, scope.root_scope_id)
  // Phase 2 already produced concrete IrOps for every nested function;
  // recursing here just builds env_descriptors and runs Phase 3 per child.
  let child_templates = compile_children(children, tree, scope.root_scope_id)
  // A direct eval at script top level runs in the script's frame and
  // reaches its locals (e.g. the synthetic `with` object slots) through the
  // template's local_names table — same mechanism as function-level eval
  // (compile_eval_direct). Without the table, direct_eval_native falls back
  // to indirect semantics and `with (o) { eval('x') }` at top level never
  // sees `o`. The analyzer already boxed every declared local for this case.
  // `GlobalVarEnv` records that this frame's VariableEnvironment IS the
  // global environment: sloppy direct eval here must send `var` declarations
  // to the global object (not a function-frame eval_env dict).
  let local_names = case info.contains_direct_eval {
    True ->
      Some(EvalNameTable(var_env: GlobalVarEnv, names: dict.to_list(info.names)))
    False -> None
  }
  // Phase 3: Resolve labels (label IDs → PC addresses).
  resolve_top_level(
    code,
    constants,
    info,
    child_templates,
    is_strict,
    opcode.ScriptCode,
    local_names,
  )
}

/// Build a name → 0-based-index dict from an ordered name list. Used to
/// convert run_direct_eval's `parent_names: List(String)` (caller locals,
/// seeded into capture slots 0..N-1 by list order) and a module's
/// `import_locals` into the `Dict(String, Int)` shape AnalyzeOpts wants.
fn indexed_names(names: List(String)) -> Dict(String, Int) {
  list.index_map(names, fn(n, i) { #(n, i) }) |> dict.from_list
}

// ============================================================================
// Child-function compilation
// ============================================================================

/// Compile a child function (and recursively its children). All scope work
/// was done by Phase 1 — this just reads the precomputed FunctionInfo from
/// the tree, builds the env capture descriptor list, and runs Phase 3.
fn compile_child(
  child: emit.CompiledChild,
  tree: scope.ScopeTree,
  parent_fn_scope: scope.ScopeId,
) -> FuncTemplate {
  let info = scope.function_info(tree, child.scope_id)
  let parent_info = scope.function_info(tree, parent_fn_scope)

  // env_descriptors: one CaptureLocal per named capture (parent slot index
  // already paired by the analyzer), then the lexical captures in canonical
  // `all_lexical_refs` order — same layout setup_frame assumes. The
  // analyzer records WHICH lexical refs this child captures (and at what
  // slot in its OWN frame) in `info.lexical_captures`; the PARENT slot
  // index for each comes from `parent_info.lexical`.
  let lex_descriptors =
    list.filter_map(opcode.all_lexical_refs, fn(ref) {
      case dict.has_key(info.lexical_captures, ref) {
        False -> Error(Nil)
        True ->
          case opcode.lexical_slot(parent_info.lexical, ref) {
            Some(parent_idx) -> Ok(CaptureLocal(parent_idx))
            // Analyzer recorded a lexical capture but the parent has no
            // slot for it — unreachable for a well-formed tree. Crash
            // loudly (matching the old `let assert Ok(parent_index) =
            // dict.get(parent_scope, name)`): silently dropping the
            // descriptor would leave env_descriptors short by one entry
            // and the runtime would read the wrong capture slot — a
            // silent miscompile is worse than a compile-time panic.
            None ->
              panic as "scope analyzer recorded a lexical capture the parent has no slot for"
          }
      }
    })
  let env_descriptors =
    list.map(info.captures, fn(c) { CaptureLocal(c.1) })
    |> list.append(lex_descriptors)

  // For direct eval: record name→index so the runtime can map variable
  // names in the eval'd source to the caller's boxed local slots. Needed
  // if eval is anywhere in the subtree — a nested eval still reaches this
  // function's locals through the closure chain.
  // A child function's VariableEnvironment is always its own frame.
  let local_names = case info.eval_in_subtree {
    True ->
      Some(EvalNameTable(
        var_env: value.FrameVarEnv,
        names: dict.to_list(info.names),
      ))
    False -> None
  }

  // Recursively compile grandchildren against the same whole-program tree;
  // this child's scope_id becomes their parent_fn_scope.
  let grandchild_templates =
    compile_children(child.functions, tree, child.scope_id)

  // Phase 3: Resolve labels.
  let #(bytecode, constants) = resolve.resolve(child.code, child.constants)
  FuncTemplate(
    name: child.name,
    arity: child.arity,
    length: child.length,
    local_count: info.local_count,
    bytecode:,
    constants:,
    functions: tuple_array.from_list(grandchild_templates),
    env_descriptors:,
    is_strict: child.is_strict,
    is_arrow: child.is_arrow,
    is_derived_constructor: child.is_derived_constructor,
    is_generator: child.is_generator,
    is_async: child.is_async,
    is_constructor: child.is_constructor,
    is_class_constructor: child.is_class_constructor,
    local_names:,
    lexical: info.lexical,
    code_kind: child.code_kind,
  )
}

/// Compile each child function. The whole-program ScopeTree already holds
/// every child's FunctionInfo (captures with parent slot indices, local_count,
/// lexical slots, name table, eval flags) keyed by `child.scope_id` — no
/// per-child IR scan, no second scope-resolution pass.
fn compile_children(
  children: List(emit.CompiledChild),
  tree: scope.ScopeTree,
  parent_fn_scope: scope.ScopeId,
) -> List(FuncTemplate) {
  list.map(children, compile_child(_, tree, parent_fn_scope))
}

// ============================================================================
// AST-level early-error checks for direct eval
// ============================================================================

/// EvalDeclarationInstantiation step 3.d conflict check for direct eval in a
/// formal-parameter initializer. The eval body's var-declared names (vars
/// hoisted from anywhere in the body plus top-level function declarations —
/// the same set emit.emit_eval_direct hoists into the var prologue) are
/// collected from the AST; any of them colliding with a parameter-scope
/// binding is a SyntaxError.
fn check_param_scope_var_conflict(
  body: List(ast.StmtWithLine),
  param_scope_names: List(String),
) -> Result(Nil, CompileError) {
  case param_scope_names {
    [] -> Ok(Nil)
    _ -> {
      let conflict =
        list.append(
          ast_util.collect_hoisted_vars(body),
          ast_util.direct_fn_names(body),
        )
        |> list.find(list.contains(param_scope_names, _))
      case conflict {
        Ok(name) ->
          Error(emit.EarlySyntaxError(
            "variable '"
            <> name
            <> "' declared by direct eval conflicts with a parameter-scope binding",
          ))
        Error(Nil) -> Ok(Nil)
      }
    }
  }
}
