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
import arc/vm/opcode.{type LexicalSlots, type SyntaxPerms}
import arc/vm/value.{
  type FuncTemplate, type JsValue, CaptureLocal, JsUndefined, JsUninitialized,
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
    emit.Internal(context:) -> "internal compiler error: " <> context
  }
}

/// Sentinel head entry in FuncTemplate.local_names marking a frame whose
/// VariableEnvironment is the GLOBAL environment (script/REPL top level).
/// Sloppy direct eval in such a frame sends `var` declarations to the global
/// object instead of a function-frame eval_env dict. The `<` prefix can never
/// collide with a user binding name; run_direct_eval strips it before use.
pub const global_frame_sentinel = "<global>"

/// Phase 3 for a top-level body (script, module, or eval): unnamed, zero
/// arity, no env captures, and not an arrow/constructor/generator/async.
fn resolve_top_level(
  code: List(opcode.IrOp),
  constants: List(JsValue),
  info: scope.FunctionInfo,
  child_templates: List(FuncTemplate),
  is_strict: Bool,
  perms: SyntaxPerms,
  local_names: Option(List(#(String, Int))),
) -> FuncTemplate {
  resolve.resolve(
    code:,
    constants:,
    local_count: info.local_count,
    functions: child_templates,
    name: None,
    arity: 0,
    length: 0,
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
    syntax_perms: perms,
  )
}

/// Compile a parsed program into a FuncTemplate the VM can interpret.
/// `sb` is the scope-builder accumulated by the parser alongside the AST;
/// scope analysis now finalizes that pre-built tree instead of re-walking
/// the AST, so the parser is the SOLE producer of scope structure.
pub fn compile(
  program: ast.Program,
  sb: scope.ScopeBuilder,
) -> Result(FuncTemplate, CompileError) {
  case program {
    ast.Script(body) -> compile_script(body, sb, scope.LexLocal)
    ast.Module(_) -> {
      use #(template, _scope_dict, _hoisted) <- result.map(compile_module(
        program,
        sb,
        esm.analyze(program),
      ))
      template
    }
  }
}

/// Compile a module, returning both the template and the scope dict
/// (name → local index) for export extraction after evaluation.
///
/// Imports are compiled as pre-boxed captures in slots 0..N-1 (the same
/// mechanism direct eval uses to alias a caller's variables). At link time
/// the module's import slots are seeded with the *exporting module's* BoxSlot
/// refs, so reads of an imported name dereference the live cell — ES live
/// bindings (§16.2). Exported local bindings are force-boxed so their BoxSlot
/// can be shared with importers. Mirrors QuickJS's JSVarRef aliasing.
pub fn compile_module(
  program: ast.Program,
  sb: scope.ScopeBuilder,
  summary: esm.ModuleSummary,
) -> Result(
  #(FuncTemplate, Dict(String, Int), List(#(String, Int))),
  CompileError,
) {
  case program {
    ast.Script(_) -> Error(emit.Internal("compile_module called on Script"))
    ast.Module(body) -> {
      let import_locals = import_local_names(summary.imports)
      let forced_box = local_export_names(summary.exports)
      compile_module_with_scope(body, sb, import_locals, forced_box)
    }
  }
}

/// The local binding names introduced by a module's import declarations, in
/// declaration order. This is the canonical order of capture slots 0..N-1;
/// link-time import seeding must produce box refs in exactly this order.
pub fn import_local_names(
  import_bindings: List(#(String, List(esm.ImportBinding))),
) -> List(String) {
  list.flat_map(import_bindings, fn(entry) {
    list.map(entry.1, fn(binding) {
      case binding {
        esm.NamedImport(local:, ..) -> local
        esm.DefaultImport(local:) -> local
        esm.NamespaceImport(local:, ..) -> local
      }
    })
  })
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
/// the default export (TDZ until the body initializes them).
pub fn module_export_seeds(
  program: ast.Program,
  summary: esm.ModuleSummary,
) -> Dict(String, JsValue) {
  case program {
    ast.Script(_) -> dict.new()
    ast.Module(items) -> {
      let undef = list.fold(items, set.new(), collect_undef_export_names)
      local_export_names(summary.exports)
      |> list.fold(dict.new(), fn(acc, name) {
        let seed = case set.contains(undef, name) {
          True -> JsUndefined
          False -> JsUninitialized
        }
        dict.insert(acc, name, seed)
      })
    }
  }
}

/// Names declared by `var` or `function` at module top level — hoisted to
/// `undefined`, so seeded `undefined` (not TDZ). Unwraps `export <decl>`.
fn collect_undef_export_names(
  acc: set.Set(String),
  item: ast.ModuleItem,
) -> set.Set(String) {
  let declaration = case item {
    ast.StatementItem(located) -> Some(located.statement)
    ast.ExportNamedDeclaration(declaration: Some(d), ..) -> Some(d)
    _ -> None
  }
  case declaration, item {
    Some(ast.VariableDeclaration(kind: ast.Var, declarations:)), _ ->
      // Every name the declarator binds — `var {a, b} = o` hoists `a` and
      // `b` exactly like `var a` does, so destructured names seed
      // `undefined` too.
      list.fold(declarations, acc, fn(a, decl) {
        list.fold(ast.pattern_bound_names(decl.id), a, set.insert)
      })
    Some(ast.FunctionDeclaration(name: Some(ast.NamedBinding(name:, ..)), ..)),
      _
    -> set.insert(acc, name)
    // `export default function fn() {}` — hoisted like any top-level
    // function declaration, so its binding seeds `undefined` (not TDZ).
    _,
      ast.ExportDefaultDeclaration(
        declaration: ast.FunctionExpression(
          name: Some(ast.NamedBinding(name:, ..)),
          ..,
        ),
        ..,
      )
    -> set.insert(acc, name)
    _, _ -> acc
  }
}

fn compile_module_with_scope(
  items: List(ast.ModuleItem),
  sb: scope.ScopeBuilder,
  import_locals: List(String),
  forced_box: List(String),
) -> Result(
  #(FuncTemplate, Dict(String, Int), List(#(String, Int))),
  CompileError,
) {
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
  // The emitter returns the post-emission tree — `fresh_slot` allocates
  // anonymous scratch locals (try/finally completion stash, with-ref base,
  // using-emission temps) by bumping FunctionInfo.local_count, so reading
  // `function_info` from the PRE-emission tree under-sizes the runtime
  // locals tuple and IrPutLocal on a scratch slot crashes with `badarg
  // setelement`. Shadow `tree` with the emitter's copy.
  use #(code, constants, children, is_strict, hoisted_funcs, tree) <- result.map(
    emit.emit_module(items, tree),
  )
  let info = scope.function_info(tree, scope.root_scope_id)
  let child_templates = compile_children(children, tree, scope.root_scope_id)
  let template =
    resolve_top_level(
      code,
      constants,
      info,
      child_templates,
      is_strict,
      opcode.script_perms,
      None,
    )
  #(template, info.names, hoisted_funcs)
}

/// Compile in REPL mode: top-level let/const/class go to the global lexical
/// record so they persist across inputs.
pub fn compile_repl(
  program: ast.Program,
  sb: scope.ScopeBuilder,
) -> Result(FuncTemplate, CompileError) {
  case program {
    ast.Script(body) -> compile_script(body, sb, scope.LexGlobal)
    // REPL input is always parsed with `parser.Script`.
    ast.Module(_) -> Error(emit.Internal("compile_repl called on a Module"))
  }
}

/// Compile code for an INDIRECT eval call (or any global-scope dynamic
/// evaluation). Top-level var → globalThis, let/const/class → fresh local
/// LexicalEnvironment per §19.2.1.1 PerformEval step 16.
pub fn compile_eval(
  program: ast.Program,
  sb: scope.ScopeBuilder,
) -> Result(FuncTemplate, CompileError) {
  case program {
    ast.Script(body) -> compile_script(body, sb, scope.LexLocal)
    // Eval source is always parsed with `parser.Script`.
    ast.Module(_) -> Error(emit.Internal("compile_eval called on a Module"))
  }
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
  program: ast.Program,
  sb: scope.ScopeBuilder,
  parent_names: List(String),
  parent_slots: LexicalSlots,
  perms: SyntaxPerms,
  caller_is_strict: Bool,
  caller_is_global: Bool,
  param_scope_names: List(String),
  with_names: List(String),
  outer_private_names: List(String),
) -> Result(FuncTemplate, CompileError) {
  case program {
    // Direct-eval source is always parsed with `parser.Script`.
    ast.Module(_) ->
      Error(emit.Internal("compile_eval_direct called on a Module"))
    ast.Script(body) -> {
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
      let parent_dict = indexed_names(parent_names)
      // Caller's lexical box refs are seeded at slots len(parent_names)+i
      // by run_direct_eval, in canonical order, one per Some entry in
      // parent_slots. Treat each as a capture so get_lexical → GetBoxed.
      let #(lexical_captures, _next) =
        list.fold(
          opcode.all_lexical_refs,
          #(dict.new(), list.length(parent_names)),
          fn(acc, ref) {
            let #(m, i) = acc
            case opcode.lexical_slot(parent_slots, ref) {
              Some(_) -> #(dict.insert(m, ref, i), i + 1)
              None -> acc
            }
          },
        )
      // The caller's enclosing with-object holders are themselves entries
      // in parent_names; their capture-slot indices form the inherited
      // with_stack the analyzer probes before falling through.
      let with_stack =
        list.filter_map(with_names, fn(n) { dict.get(parent_dict, n) })
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
          && list.any(dict.values(tree.scopes), fn(s) { s.kind == scope.With })
        {
          True ->
            Error(emit.EarlySyntaxError("'with' not allowed in strict mode"))
          False -> Ok(Nil)
        },
      )
      // Phase 2: emit. The direct-eval entry point folds the contextual
      // inputs into the emitter's initial state so no post-emission op
      // rewriting is needed:
      //   - caller_is_strict → vars become local slots, Annex B suppressed
      //     (§19.2.1.1 step 18 — strict eval gets its own VarEnvironment)
      //   - param_scope_names / outer_private_names → seeded into the
      //     emitter so top-level IrCallEval ops carry them (steps 3.d/5
      //     transitive through nested direct eval)
      //   - DeclareLexical(RefThis) skipped — lexical pseudo-slots arrive
      //     via the caller's boxed slots (lexical_captures above)
      // Shadow `tree` with the emitter's post-emission copy — scratch
      // slots (alloc_scratch) bumped local_count there.
      use #(code, constants, children, strict, tree) <- result.try(
        emit.emit_eval_direct(
          body,
          tree,
          caller_is_strict,
          param_scope_names,
          outer_private_names,
        ),
      )
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
        False -> check_param_scope_var_conflict(body, param_scope_names)
      })
      let info = scope.function_info(tree, scope.root_scope_id)
      let child_templates =
        compile_children(children, tree, scope.root_scope_id)
      // Expose the name table only when a nested top-level eval needs it —
      // run_direct_eval consults state.func.local_names to decide between
      // direct (aliasing) and indirect fallback semantics. A global caller's
      // VariableEnvironment propagates: a nested eval in this body shares it,
      // so keep the sentinel (see compile_script). Keyed on the analyzer's
      // FunctionInfo.contains_direct_eval — populated by scope.finalize from
      // the per-Scope flags the parser sets, so emit no longer needs to
      // surface a `has_eval_call` side-channel.
      let local_names = case info.contains_direct_eval, caller_is_global {
        True, True ->
          Some([#(global_frame_sentinel, -1), ..dict.to_list(info.names)])
        True, False -> Some(dict.to_list(info.names))
        False, _ -> None
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
        perms,
        local_names,
      ))
    }
  }
}

fn compile_script(
  stmts: List(ast.StmtWithLine),
  sb: scope.ScopeBuilder,
  top_lex: scope.TopLevelLex,
) -> Result(FuncTemplate, CompileError) {
  // Phase 1: finalize the parser-built scope tree (top-level body + every
  // nested function), computing capture sets, boxing decisions, and slot
  // indices BEFORE any IR is emitted.
  let opts = scope.AnalyzeOpts(..scope.default_analyze_opts(), top_lex:)
  let tree = scope.finalize(sb, opts)
  // Phase 2: emit IR from AST, consulting the tree for every name reference
  // — no symbolic IrScope* ops, no second resolution pass. The emitter
  // returns the post-emission tree: `fresh_slot` mints scratch locals
  // (try/finally completion stash, with-ref base, using-emission temps)
  // by bumping FunctionInfo.local_count on its copy, so reading
  // `function_info` from the analyzer's tree would under-size the
  // runtime locals tuple. Shadow `tree`.
  use #(code, constants, children, is_strict, tree) <- result.map(
    emit.emit_program(stmts, tree),
  )
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
  // The sentinel head entry marks this frame's VariableEnvironment as the
  // GLOBAL environment: sloppy direct eval here must send `var` declarations
  // to the global object (not a function-frame eval_env dict).
  let local_names = case info.contains_direct_eval {
    True -> Some([#(global_frame_sentinel, -1), ..dict.to_list(info.names)])
    False -> None
  }
  // Phase 3: Resolve labels (label IDs → PC addresses).
  resolve_top_level(
    code,
    constants,
    info,
    child_templates,
    is_strict,
    opcode.script_perms,
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
  // `all_lexical_refs` order — same layout setup_locals assumes. The
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
  let local_names = case info.eval_in_subtree {
    True -> Some(dict.to_list(info.names))
    False -> None
  }

  // Recursively compile grandchildren against the same whole-program tree;
  // this child's scope_id becomes their parent_fn_scope.
  let grandchild_templates =
    compile_children(child.functions, tree, child.scope_id)

  // Phase 3: Resolve labels.
  resolve.resolve(
    code: child.code,
    constants: child.constants,
    local_count: info.local_count,
    functions: grandchild_templates,
    name: child.name,
    arity: child.arity,
    length: child.length,
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
    syntax_perms: child.syntax_perms,
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
