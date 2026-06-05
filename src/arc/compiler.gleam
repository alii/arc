/// Bytecode Compiler
///
/// Translates a parsed AST into a FuncTemplate that the VM can interpreter.
/// Three-phase pipeline:
///   Phase 1 (emit): AST → EmitterOp (symbolic names + label IDs)
///   Phase 2 (scope): EmitterOp → IrOp (resolved local indices + label IDs)
///   Phase 3 (resolve): IrOp → Op (absolute PC addresses)
import arc/compiler/emit
import arc/compiler/resolve
import arc/compiler/scope
import arc/parser/ast
import arc/vm/opcode.{type LexicalRefs, type LexicalSlots, type SyntaxPerms}
import arc/vm/value.{
  type FuncTemplate, type JsValue, CaptureLocal, JsUndefined, JsUninitialized,
}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/set

/// A single import binding from an import declaration.
pub type ImportBinding {
  /// import { foo } from 'mod'  or  import { foo as bar } from 'mod'
  NamedImport(imported: String, local: String)
  /// import foo from 'mod'
  DefaultImport(local: String)
  /// import * as ns from 'mod'
  NamespaceImport(local: String)
}

/// Compilation errors.
pub type CompileError {
  BreakOutsideLoop
  ContinueOutsideLoop
  Unsupported(description: String)
}

fn map_emit_error(error: emit.EmitError) -> CompileError {
  case error {
    emit.BreakOutsideLoop -> BreakOutsideLoop
    emit.ContinueOutsideLoop -> ContinueOutsideLoop
    emit.Unsupported(desc) -> Unsupported(desc)
  }
}

/// Phase 3 for a top-level body (script, module, or eval): unnamed, zero
/// arity, no env captures, and not an arrow/constructor/generator/async.
fn resolve_top_level(
  resolved: scope.Resolved,
  child_templates: List(FuncTemplate),
  is_strict: Bool,
  perms: SyntaxPerms,
) -> FuncTemplate {
  resolve.resolve(
    resolved.code,
    resolved.constants,
    resolved.local_count,
    child_templates,
    None,
    0,
    [],
    is_strict,
    False,
    False,
    False,
    False,
    False,
    None,
    resolved.lexical,
    perms,
  )
}

/// Compile a parsed program into a FuncTemplate the VM can interpreter.
pub fn compile(program: ast.Program) -> Result(FuncTemplate, CompileError) {
  case program {
    ast.Script(body) -> compile_script(body, emit.LexLocal)
    ast.Module(_) -> {
      use #(template, _scope_dict, _hoisted) <- result.map(compile_module(
        program,
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
) -> Result(
  #(FuncTemplate, Dict(String, Int), List(#(String, Int))),
  CompileError,
) {
  case program {
    ast.Script(_) -> Error(Unsupported("compile_module called on Script"))
    ast.Module(body) -> {
      let import_locals = import_local_names(extract_module_imports(program))
      let forced_box = local_export_names(extract_module_exports(program))
      compile_module_with_scope(body, import_locals, forced_box)
    }
  }
}

/// The local binding names introduced by a module's import declarations, in
/// declaration order. This is the canonical order of capture slots 0..N-1;
/// link-time import seeding must produce box refs in exactly this order.
pub fn import_local_names(
  import_bindings: List(#(String, List(ImportBinding))),
) -> List(String) {
  list.flat_map(import_bindings, fn(entry) {
    list.map(entry.1, fn(binding) {
      case binding {
        NamedImport(local:, ..) -> local
        DefaultImport(local:) -> local
        NamespaceImport(local:) -> local
      }
    })
  })
}

/// Local binding names that a module exports (LocalExport only). These must be
/// force-boxed so the exported cell is a heap BoxSlot shareable with importers.
fn local_export_names(exports: List(ExportEntry)) -> List(String) {
  list.filter_map(exports, fn(entry) {
    case entry {
      LocalExport(local_name:, ..) -> Ok(local_name)
      _ -> Error(Nil)
    }
  })
}

/// The value the linker pre-seeds into each exported local's BoxSlot before the
/// module body runs (§16.2 instantiation): `undefined` for var and function
/// declarations (hoisted, never TDZ), `uninitialized` for let/const/class and
/// the default export (TDZ until the body initializes them).
pub fn module_export_seeds(program: ast.Program) -> Dict(String, JsValue) {
  case program {
    ast.Script(_) -> dict.new()
    ast.Module(items) -> {
      let undef = list.fold(items, set.new(), collect_undef_export_names)
      local_export_names(extract_module_exports(program))
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
  case declaration {
    Some(ast.VariableDeclaration(kind: ast.Var, declarations:)) ->
      list.fold(declarations, acc, fn(a, decl) {
        case decl {
          ast.VariableDeclarator(id: ast.IdentifierPattern(name:), ..) ->
            set.insert(a, name)
          _ -> a
        }
      })
    Some(ast.FunctionDeclaration(name: Some(name), ..)) -> set.insert(acc, name)
    _ -> acc
  }
}

fn compile_module_with_scope(
  items: List(ast.ModuleItem),
  import_locals: List(String),
  forced_box: List(String),
) -> Result(
  #(FuncTemplate, Dict(String, Int), List(#(String, Int))),
  CompileError,
) {
  use
    #(emitter_ops, constants, constants_map, children, is_strict, hoisted_funcs)
  <- result.map(result.map_error(emit.emit_module(items), map_emit_error))
  let captured_vars = collect_all_captured_vars(children, emitter_ops)
  // Exported locals are linker-seeded: the linker pre-allocates each
  // export's BoxSlot (so it can be shared with importers, including cyclic
  // ones) and seeds it into the slot before the body runs. Their DeclareVar
  // reserves the slot + a boxed binding but emits no init/box op.
  let linker_seeded = set.from_list(forced_box)
  let lexical_captured = collect_arrow_lexical_refs(children)
  // Imports occupy boxed capture slots 0..N-1.
  let resolved =
    scope.resolve_with_captures(
      emitter_ops,
      constants,
      constants_map,
      import_locals,
      dict.new(),
      captured_vars,
      linker_seeded,
      lexical_captured,
      scope.ToGlobal,
    )
  let child_templates = compile_children(children, resolved)
  let template =
    resolve_top_level(resolved, child_templates, is_strict, opcode.script_perms)
  #(template, resolved.names, hoisted_funcs)
}

/// Compile in REPL mode: top-level let/const/class go to the global lexical
/// record so they persist across inputs.
pub fn compile_repl(
  program: ast.Program,
) -> Result(FuncTemplate, CompileError) {
  case program {
    ast.Script(body) -> compile_script(body, emit.LexGlobal)
    ast.Module(_) -> Error(Unsupported("modules not supported in REPL"))
  }
}

/// Compile code for an INDIRECT eval call (or any global-scope dynamic
/// evaluation). Top-level var → globalThis, let/const/class → fresh local
/// LexicalEnvironment per §19.2.1.1 PerformEval step 16.
pub fn compile_eval(
  program: ast.Program,
) -> Result(FuncTemplate, CompileError) {
  case program {
    ast.Script(body) -> compile_script(body, emit.LexLocal)
    ast.Module(_) -> Error(Unsupported("modules not supported in eval"))
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
  parent_names: List(String),
  parent_slots: LexicalSlots,
  perms: SyntaxPerms,
  caller_is_strict: Bool,
) -> Result(FuncTemplate, CompileError) {
  case program {
    ast.Module(_) -> Error(Unsupported("modules not supported in eval"))
    ast.Script(body) -> {
      use #(emitter_ops, constants, constants_map, children, is_strict) <- result.map(
        result.map_error(emit.emit_program(body, emit.LexLocal), map_emit_error),
      )
      let strict = caller_is_strict || is_strict
      // Strict direct eval gets its own VarEnvironment (spec §19.2.1.1
      // step 18): rewrite hoisted var declarations from DeclareGlobalVar
      // to local DeclareVar so they stay scoped to the eval body.
      // Sloppy keeps DeclareGlobalVar which scope.gleam rewrites to
      // DeclareEvalVar via ToEvalEnv.
      // Also drop the body's own DeclareLexical — lexical bindings arrive
      // via the caller's boxed slots (lexical_captures below) and owned
      // slots here would shadow them.
      let emitter_ops =
        list.filter_map(emitter_ops, fn(op) {
          case op {
            emit.DeclareLexical(_) -> Error(Nil)
            emit.Ir(opcode.IrDeclareGlobalVar(name)) if strict ->
              Ok(emit.DeclareVar(name, emit.VarBinding))
            _ -> Ok(op)
          }
        })
      let captured_vars = collect_all_captured_vars(children, emitter_ops)
      let lexical_captured = collect_arrow_lexical_refs(children)
      let fallthrough = case strict {
        True -> scope.ToGlobal
        False -> scope.ToEvalEnv
      }
      // Caller's lexical box refs are seeded at slots len(parent_names)+i
      // by run_direct_eval, in canonical order, one per Some entry in
      // parent_slots. Treat each as a capture so IrGetLexical → GetBoxed.
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
      let resolved =
        scope.resolve_with_captures(
          emitter_ops,
          constants,
          constants_map,
          parent_names,
          lexical_captures,
          captured_vars,
          set.new(),
          lexical_captured,
          fallthrough,
        )
      let child_templates = compile_children(children, resolved)
      resolve_top_level(resolved, child_templates, is_strict, perms)
    }
  }
}

/// Extract import bindings from a module AST.
/// Returns a list of (specifier, [(imported_name, local_name)]) pairs.
/// Used by the host to resolve imports before execution.
pub fn extract_module_imports(
  program: ast.Program,
) -> List(#(String, List(ImportBinding))) {
  case program {
    ast.Script(_) -> []
    ast.Module(body) ->
      list.filter_map(body, fn(item) {
        case item {
          ast.ImportDeclaration(specifiers, ast.StringLit(source)) -> {
            let bindings =
              list.map(specifiers, fn(spec) {
                case spec {
                  ast.ImportNamedSpecifier(imported:, local:) ->
                    NamedImport(imported:, local:)
                  ast.ImportDefaultSpecifier(local:) -> DefaultImport(local:)
                  ast.ImportNamespaceSpecifier(local:) ->
                    NamespaceImport(local:)
                }
              })
            Ok(#(source, bindings))
          }
          _ -> Error(Nil)
        }
      })
  }
}

/// An export entry maps an exported name to how to find its value.
pub type ExportEntry {
  /// Export a local variable: `export let x = 42` or `export { x }`
  /// For default exports, export_name is "default" and local_name is "*default*".
  LocalExport(export_name: String, local_name: String)
  /// Re-export a named binding: `export { x } from 'mod'` or `export { x as y } from 'mod'`
  ReExport(export_name: String, imported_name: String, source_specifier: String)
  /// Re-export everything: `export * from 'mod'`
  ReExportAll(source_specifier: String)
  /// Re-export everything under a namespace: `export * as ns from 'mod'`
  ReExportNamespace(export_name: String, source_specifier: String)
}

/// Extract export entries from a module AST.
/// Returns a list of ExportEntry describing what the module exports.
pub fn extract_module_exports(program: ast.Program) -> List(ExportEntry) {
  case program {
    ast.Script(_) -> []
    ast.Module(body) ->
      list.flat_map(body, fn(item) {
        case item {
          ast.ExportNamedDeclaration(declaration:, specifiers:, source: None) ->
            extract_named_exports(declaration, specifiers)
          ast.ExportDefaultDeclaration(_) -> [
            LocalExport(export_name: "default", local_name: "*default*"),
          ]
          // Re-exports from other modules
          ast.ExportNamedDeclaration(
            declaration: _,
            specifiers:,
            source: option.Some(ast.StringLit(source)),
          ) ->
            list.map(specifiers, fn(spec) {
              case spec {
                ast.ExportSpecifier(local:, exported:) ->
                  ReExport(
                    export_name: exported,
                    imported_name: local,
                    source_specifier: source,
                  )
              }
            })
          ast.ExportAllDeclaration(
            exported: option.Some(name),
            source: ast.StringLit(source),
          ) -> [ReExportNamespace(export_name: name, source_specifier: source)]
          ast.ExportAllDeclaration(
            exported: None,
            source: ast.StringLit(source),
          ) -> [ReExportAll(source_specifier: source)]
          _ -> []
        }
      })
  }
}

/// Extract exported names from a named export declaration.
fn extract_named_exports(
  declaration: option.Option(ast.Statement),
  specifiers: List(ast.ExportSpecifier),
) -> List(ExportEntry) {
  // From specifiers: `export { a, b as c }`
  let spec_exports =
    list.map(specifiers, fn(spec) {
      case spec {
        ast.ExportSpecifier(local:, exported:) ->
          LocalExport(export_name: exported, local_name: local)
      }
    })

  // From declaration: `export let x = 42`, `export function f() {}`
  let decl_exports = case declaration {
    option.Some(ast.VariableDeclaration(declarations:, ..)) ->
      list.filter_map(declarations, fn(decl) {
        case decl {
          ast.VariableDeclarator(id: ast.IdentifierPattern(name:), ..) ->
            Ok(LocalExport(export_name: name, local_name: name))
          _ -> Error(Nil)
        }
      })
    option.Some(ast.FunctionDeclaration(name: option.Some(name), ..)) -> [
      LocalExport(export_name: name, local_name: name),
    ]
    option.Some(ast.ClassDeclaration(name: option.Some(name), ..)) -> [
      LocalExport(export_name: name, local_name: name),
    ]
    _ -> []
  }

  list.append(spec_exports, decl_exports)
}

fn compile_script(
  stmts: List(ast.StmtWithLine),
  top_lex: emit.TopLevelLex,
) -> Result(FuncTemplate, CompileError) {
  // Phase 1: Emit IR from AST
  use #(emitter_ops, constants, constants_map, children, is_strict) <- result.map(
    result.map_error(emit.emit_program(stmts, top_lex), map_emit_error),
  )
  // Determine which variables are captured by children (need boxing)
  let captured_vars = collect_all_captured_vars(children, emitter_ops)
  let lexical_captured = collect_arrow_lexical_refs(children)

  // Phase 2: Resolve scopes (names → local indices), with capture info
  let resolved =
    scope.resolve(
      emitter_ops,
      constants,
      constants_map,
      captured_vars,
      lexical_captured,
      scope.ToGlobal,
    )

  // Process child functions through Phase 2 + Phase 3 recursively
  let child_templates = compile_children(children, resolved)

  // Phase 3: Resolve labels (label IDs → PC addresses)
  resolve_top_level(resolved, child_templates, is_strict, opcode.script_perms)
}

/// True if this function or any nested function contains a syntactic
/// `eval(...)` call. Used to decide which functions need all-locals-boxed.
fn any_descendant_has_eval(child: emit.CompiledChild) -> Bool {
  child.has_eval_call || list.any(child.functions, any_descendant_has_eval)
}

fn compile_child(
  child: emit.CompiledChild,
  parent_scope: Dict(String, Int),
  parent_lexical: LexicalSlots,
) -> FuncTemplate {
  // If this function OR any descendant contains a direct eval call, ALL of
  // this function's locals must be boxed — eval can see the full lexical
  // chain. QuickJS walks UP parent pointers when it sees eval(; we compute
  // the transitive flag DOWN instead since Arc compiles parent→child.
  let eval_in_subtree = any_descendant_has_eval(child)

  // Determine which variables this child uses but doesn't declare (free vars)
  let free_vars = collect_free_vars(child)

  // Filter to names that exist in parent scope (others are globals).
  // When eval is present anywhere in this subtree, capture ALL parent-scope
  // names: eval("x") can reference any enclosing binding and the source
  // string is opaque to free-var analysis. Threading every box ref through
  // the closure chain lets direct_eval_native seed the eval'd code with
  // the full lexical environment.
  let captures = case eval_in_subtree {
    False ->
      set.filter(free_vars, dict.has_key(parent_scope, _))
      |> set.to_list
    True -> dict.keys(parent_scope)
  }

  // Arrows that (transitively) reference a lexical binding capture the
  // enclosing non-arrow's slot. Non-arrows DeclareLexical their own. With
  // eval in the subtree, an arrow captures all available lexicals so
  // eval('this'/'new.target'/...) works. Captures occupy slots
  // len(captures)..len(captures)+k in canonical order.
  let #(lexical_captures, lex_descriptors) = case child.is_arrow {
    False -> #(dict.new(), [])
    True -> {
      let #(m, ds, _next) =
        list.fold(
          opcode.all_lexical_refs,
          #(dict.new(), [], list.length(captures)),
          fn(acc, ref) {
            let #(m, ds, i) = acc
            let referenced =
              eval_in_subtree
              || opcode.lexical_refs_get(child.lexical_refs, ref)
            case referenced, opcode.lexical_slot(parent_lexical, ref) {
              True, Some(pidx) -> #(
                dict.insert(m, ref, i),
                [CaptureLocal(pidx), ..ds],
                i + 1,
              )
              _, _ -> acc
            }
          },
        )
      #(m, list.reverse(ds))
    }
  }

  // Build env_descriptors: one CaptureLocal per named capture, then the
  // lexical captures in canonical order — same layout the resolver and
  // setup_locals assume.
  let env_descriptors =
    list.map(captures, fn(name) {
      let assert Ok(parent_index) = dict.get(parent_scope, name)
      CaptureLocal(parent_index)
    })
  let env_descriptors = list.append(env_descriptors, lex_descriptors)

  // Determine which of this child's vars are captured by grandchildren
  let grandchild_captured =
    collect_all_captured_vars(child.functions, child.code)

  let vars_to_box = case eval_in_subtree {
    False -> grandchild_captured
    True -> set.union(grandchild_captured, collect_declared_names(child.code))
  }
  // Box a lexical slot when a grandchild arrow captures it, or when eval may
  // read it.
  let lexical_captured = case eval_in_subtree {
    True -> opcode.LexicalRefs(True, True, True, True)
    False -> collect_arrow_lexical_refs(child.functions)
  }

  // Sloppy functions that contain a direct eval must resolve unresolved
  // names through eval_env first (for vars injected by eval("var y=1")).
  let fallthrough = case eval_in_subtree && !child.is_strict {
    True -> scope.ToEvalEnv
    False -> scope.ToGlobal
  }

  // Phase 2: Resolve scopes, with captures pre-populated
  let resolved =
    scope.resolve_with_captures(
      child.code,
      child.constants,
      child.constants_map,
      captures,
      lexical_captures,
      vars_to_box,
      set.new(),
      lexical_captured,
      fallthrough,
    )

  // For direct eval: record name→index so the runtime can map variable
  // names in the eval'd source to the caller's boxed local slots.
  // Needed if eval is anywhere in the subtree — a nested eval still
  // reaches this function's locals through the closure chain.
  let local_names = case eval_in_subtree {
    False -> None
    True -> Some(dict.to_list(resolved.names))
  }

  // Recursively compile grandchildren. resolved.lexical is where the lexical
  // bindings live in THIS body (owned for non-arrows, captures for arrows),
  // so it's the parent_lexical for grandchildren either way.
  let grandchild_templates = compile_children(child.functions, resolved)

  // Phase 3: Resolve labels
  resolve.resolve(
    resolved.code,
    resolved.constants,
    resolved.local_count,
    grandchild_templates,
    child.name,
    child.arity,
    env_descriptors,
    child.is_strict,
    child.is_arrow,
    child.is_derived_constructor,
    child.is_generator,
    child.is_async,
    child.is_constructor,
    local_names,
    resolved.lexical,
    child.syntax_perms,
  )
}

/// OR together the lexical_refs of every arrow child — those are the lexical
/// bindings this body must box so the arrows' captures alias the same cell.
/// Non-arrows own their slots; their flags don't propagate.
fn collect_arrow_lexical_refs(
  children: List(emit.CompiledChild),
) -> LexicalRefs {
  use acc, child <- list.fold(children, opcode.no_lexical_refs)
  case child.is_arrow {
    True -> opcode.lexical_refs_or(acc, child.lexical_refs)
    False -> acc
  }
}

// ============================================================================
// Captured variable analysis
// ============================================================================

/// Determine which parent variables are captured by any child function.
/// Returns the set of variable names that need to be boxed in the parent.
fn collect_all_captured_vars(
  children: List(emit.CompiledChild),
  parent_ops: List(emit.EmitterOp),
) -> set.Set(String) {
  // Collect all names declared in the parent
  let parent_declared = collect_declared_names(parent_ops)

  // For each child, collect free vars and intersect with parent declarations
  list.fold(children, set.new(), fn(acc, child) {
    collect_free_vars(child)
    |> set.intersection(parent_declared)
    |> set.union(acc)
  })
}

/// Collect all variable names declared in an EmitterOp list.
fn collect_declared_names(ops: List(emit.EmitterOp)) -> set.Set(String) {
  list.fold(ops, set.new(), fn(acc, op) {
    case op {
      emit.DeclareVar(name, _) -> set.insert(acc, name)
      _ -> acc
    }
  })
}

// ============================================================================
// Free variable analysis
// ============================================================================

/// Collect variable names that are used but not declared in a child's EmitterOps.
/// Recurses into grandchildren so that a variable referenced only by a nested
/// closure is still captured through the intermediate scope (transitive capture).
fn collect_free_vars(child: emit.CompiledChild) -> set.Set(String) {
  let #(used, declared) = scan_ops(child.code, set.new(), set.new())
  let grandchild_free =
    list.fold(child.functions, set.new(), fn(acc, gc) {
      set.union(acc, collect_free_vars(gc))
    })
  set.union(used, grandchild_free) |> set.difference(declared)
}

/// Scan EmitterOps to find used variable names and declared names.
fn scan_ops(
  ops: List(emit.EmitterOp),
  used: set.Set(String),
  declared: set.Set(String),
) -> #(set.Set(String), set.Set(String)) {
  case ops {
    [] -> #(used, declared)
    [op, ..rest] -> {
      let #(used, declared) = case op {
        emit.Ir(emit_ir) ->
          case emit_ir {
            opcode.IrScopeGetVar(name) -> #(set.insert(used, name), declared)
            opcode.IrScopePutVar(name) -> #(set.insert(used, name), declared)
            opcode.IrScopeTypeofVar(name) -> #(set.insert(used, name), declared)
            _ -> #(used, declared)
          }
        emit.DeclareVar(name, _) -> #(used, set.insert(declared, name))
        _ -> #(used, declared)
      }
      scan_ops(rest, used, declared)
    }
  }
}

/// Compile each child function with the name→slot snapshot scope.gleam took
/// at its IrMakeClosure(i). Single source of truth for capture indices —
/// replaces the old build_scope_dict re-walk that diverged on block shadows.
fn compile_children(
  children: List(emit.CompiledChild),
  resolved: scope.Resolved,
) -> List(FuncTemplate) {
  use child, i <- list.index_map(children)
  let parent_scope =
    dict.get(resolved.closure_scopes, i) |> result.unwrap(resolved.names)
  compile_child(child, parent_scope, resolved.lexical)
}
