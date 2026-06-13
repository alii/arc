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
import arc/esm
import arc/parser/ast
import arc/vm/opcode.{type LexicalRefs, type LexicalSlots, type SyntaxPerms}
import arc/vm/value.{
  type FuncTemplate, type JsValue, CaptureLocal, JsUndefined, JsUninitialized,
}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set

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

/// Sentinel head entry in FuncTemplate.local_names marking a frame whose
/// VariableEnvironment is the GLOBAL environment (script/REPL top level).
/// Sloppy direct eval in such a frame sends `var` declarations to the global
/// object instead of a function-frame eval_env dict. The `<` prefix can never
/// collide with a user binding name; run_direct_eval strips it before use.
pub const global_frame_sentinel = "<global>"

/// Phase 3 for a top-level body (script, module, or eval): unnamed, zero
/// arity, no env captures, and not an arrow/constructor/generator/async.
fn resolve_top_level(
  resolved: scope.Resolved,
  child_templates: List(FuncTemplate),
  is_strict: Bool,
  perms: SyntaxPerms,
  local_names: Option(List(#(String, Int))),
) -> FuncTemplate {
  resolve.resolve(
    resolved.code,
    resolved.constants,
    resolved.local_count,
    child_templates,
    None,
    0,
    0,
    [],
    is_strict,
    False,
    False,
    False,
    False,
    False,
    False,
    local_names,
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
  summary: esm.ModuleSummary,
) -> Result(
  #(FuncTemplate, Dict(String, Int), List(#(String, Int))),
  CompileError,
) {
  case program {
    ast.Script(_) -> Error(Unsupported("compile_module called on Script"))
    ast.Module(body) -> {
      let import_locals = import_local_names(summary.imports)
      let forced_box = local_export_names(summary.exports)
      compile_module_with_scope(body, import_locals, forced_box)
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
      list.fold(declarations, acc, fn(a, decl) {
        case decl {
          ast.VariableDeclarator(id: ast.IdentifierPattern(name:), ..) ->
            set.insert(a, name)
          _ -> a
        }
      })
    Some(ast.FunctionDeclaration(name: Some(name), ..)), _ ->
      set.insert(acc, name)
    // `export default function fn() {}` — hoisted like any top-level
    // function declaration, so its binding seeds `undefined` (not TDZ).
    _,
      ast.ExportDefaultDeclaration(ast.FunctionExpression(name: Some(name), ..))
    -> set.insert(acc, name)
    _, _ -> acc
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
      set.new(),
      set.new(),
      captured_vars,
      linker_seeded,
      lexical_captured,
      scope.ToGlobal,
      [],
      is_strict,
    )
  let child_templates = compile_children(children, resolved)
  let template =
    resolve_top_level(
      resolved,
      child_templates,
      is_strict,
      opcode.script_perms,
      None,
    )
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
  caller_is_global: Bool,
  param_scope_names: List(String),
  with_names: List(String),
  outer_private_names: List(String),
) -> Result(FuncTemplate, CompileError) {
  case program {
    ast.Module(_) -> Error(Unsupported("modules not supported in eval"))
    ast.Script(body) -> {
      use #(emitter_ops, constants, constants_map, children, is_strict) <- result.try(
        result.map_error(emit.emit_program(body, emit.LexLocal), map_emit_error),
      )
      let strict = caller_is_strict || is_strict
      // §19.2.1.1 PerformEval → EvalDeclarationInstantiation step 3.d:
      // when this direct eval happens inside a formal-parameter initializer
      // (param_scope_names non-empty), sloppy eval code must not var-declare
      // a name already bound in the parameter scope (parameters + the
      // implicit `arguments` binding). The spec walks the environment chain
      // from the eval's LexicalEnvironment to its VariableEnvironment and
      // throws a SyntaxError on any HasBinding hit — for a parameter
      // initializer the only environment in between is the parameter scope.
      // Strict eval gets its own VariableEnvironment, so no check applies.
      use Nil <- result.try(case strict {
        True -> Ok(Nil)
        False -> check_param_scope_var_conflict(emitter_ops, param_scope_names)
      })
      // Step 3.d applies transitively to direct evals nested in this eval's
      // source: sloppy eval shares the caller's VariableEnvironment, so a
      // nested eval's lexEnv chain still passes through the same parameter
      // scope. The source was emitted fresh (param_scope_names starts empty
      // in the emitter), so thread the outer call site's names into the
      // top-level IrCallEval ops. Functions/arrows declared in the eval body
      // are children with their own VariableEnvironment — their eval sites
      // keep the names from their own parameter scopes.
      let emitter_ops = case strict, param_scope_names {
        False, [_, ..] ->
          list.map(emitter_ops, fn(op) {
            case op {
              emit.Ir(opcode.IrCallEval(arity, [], eval_with_names, eval_privs)) ->
                emit.Ir(opcode.IrCallEval(
                  arity,
                  param_scope_names,
                  eval_with_names,
                  eval_privs,
                ))
              _ -> op
            }
          })
        _, _ -> emitter_ops
      }
      // §19.2.1.1 step 5: the caller's PrivateEnvironment carries into the
      // eval code — a nested direct eval at this eval's top level parses
      // with the same private names (plus any classes the eval source
      // itself wraps around the nested site, already in the op's list).
      let emitter_ops = case outer_private_names {
        [] -> emitter_ops
        _ ->
          list.map(emitter_ops, fn(op) {
            case op {
              emit.Ir(opcode.IrCallEval(arity, psn, eval_with_names, eval_privs)) ->
                emit.Ir(opcode.IrCallEval(
                  arity,
                  psn,
                  eval_with_names,
                  list.append(eval_privs, outer_private_names),
                ))
              _ -> op
            }
          })
      }
      // §14.11.1 early error: `with` is illegal in strict code. The eval
      // source is parsed sloppy (no directive), so when the CALLER's
      // strictness upgrades this eval to strict, reject any with statement
      // post-parse (anywhere in the body — strict eval makes all nested
      // code strict). A "use strict" directive in the source itself is
      // already rejected by the parser.
      use Nil <- result.try(
        case
          strict
          && {
            ops_contain_with(emitter_ops)
            || list.any(children, child_contains_with)
          }
        {
          True -> Error(Unsupported("'with' not allowed in strict mode"))
          False -> Ok(Nil)
        },
      )
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
            // Annex B function-in-block extensions never apply to strict
            // eval code — drop them when the caller's strictness upgraded
            // a sloppy-parsed body.
            emit.DeclareAnnexBVar(_) if strict -> Error(Nil)
            emit.AnnexBPromote(_) if strict -> Error(Nil)
            _ -> Ok(op)
          }
        })
      // A direct eval at this eval body's top level runs in THIS eval's
      // frame and reaches its locals through the template's local_names
      // table (same mechanism the original caller used). The nested source
      // is opaque to free-var analysis, so apply the same capture-all rule
      // as compile_child: box every local declared in the eval body so the
      // nested eval's slot aliasing always sees box refs.
      let has_nested_eval =
        list.any(emitter_ops, fn(op) {
          case op {
            emit.Ir(opcode.IrCallEval(..)) -> True
            _ -> False
          }
        })
      let captured_vars = collect_all_captured_vars(children, emitter_ops)
      let captured_vars = case has_nested_eval {
        True -> set.union(captured_vars, collect_declared_names(emitter_ops))
        False -> captured_vars
      }
      let lexical_captured = collect_arrow_lexical_refs(children)
      // Sloppy direct eval shares the caller's VariableEnvironment. For a
      // function caller that is approximated by the frame's eval_env dict;
      // for a GLOBAL caller (script/REPL top level) the VariableEnvironment
      // IS the global environment, so `var` declarations and unresolved
      // names go straight to the global object — no eval_env in play.
      let fallthrough = case strict || caller_is_global {
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
          set.new(),
          set.new(),
          captured_vars,
          set.new(),
          lexical_captured,
          fallthrough,
          // The caller's enclosing with objects (synthetic locals, threaded
          // through CallEval.with_names) — free names in the eval'd source
          // must check them before outer bindings/globals.
          with_names,
          strict,
        )
      let child_templates = compile_children(children, resolved)
      // Expose the name table only when a nested top-level eval needs it —
      // run_direct_eval consults state.func.local_names to decide between
      // direct (aliasing) and indirect fallback semantics. A global caller's
      // VariableEnvironment propagates: a nested eval in this body shares it,
      // so keep the sentinel (see compile_script).
      let local_names = case has_nested_eval, caller_is_global {
        True, True ->
          Some([#(global_frame_sentinel, -1), ..dict.to_list(resolved.names)])
        True, False -> Some(dict.to_list(resolved.names))
        False, _ -> None
      }
      // The template's strictness must reflect how the body was COMPILED
      // (caller strictness upgrades the body — vars were rewritten to
      // locals, Annex B dropped), not just the body's own directive, so a
      // nested eval inherits the effective strictness at runtime.
      Ok(resolve_top_level(
        resolved,
        child_templates,
        strict,
        perms,
        local_names,
      ))
    }
  }
}

/// EvalDeclarationInstantiation step 3.d conflict check for direct eval in a
/// formal-parameter initializer. The eval body's var-declared names (vars
/// hoisted from anywhere in the body plus top-level function declarations)
/// appear as IrDeclareGlobalVar ops in the freshly emitted program prologue;
/// any of them colliding with a parameter-scope binding is a SyntaxError.
fn check_param_scope_var_conflict(
  emitter_ops: List(emit.EmitterOp),
  param_scope_names: List(String),
) -> Result(Nil, CompileError) {
  case param_scope_names {
    [] -> Ok(Nil)
    _ -> {
      let conflict =
        list.find_map(emitter_ops, fn(op) {
          case op {
            emit.Ir(opcode.IrDeclareGlobalVar(name)) ->
              case list.contains(param_scope_names, name) {
                True -> Ok(name)
                False -> Error(Nil)
              }
            _ -> Error(Nil)
          }
        })
      case conflict {
        Ok(name) ->
          Error(Unsupported(
            "SyntaxError: variable '"
            <> name
            <> "' declared by direct eval conflicts with a parameter-scope binding",
          ))
        Error(Nil) -> Ok(Nil)
      }
    }
  }
}

fn compile_script(
  stmts: List(ast.StmtWithLine),
  top_lex: emit.TopLevelLex,
) -> Result(FuncTemplate, CompileError) {
  // Phase 1: Emit IR from AST
  use #(emitter_ops, constants, constants_map, children, is_strict) <- result.map(
    result.map_error(emit.emit_program(stmts, top_lex), map_emit_error),
  )
  // A direct eval at script top level runs in the script's frame and
  // reaches its locals (e.g. the synthetic `with` object slots) through the
  // template's local_names table — same mechanism as function-level eval
  // (compile_eval_direct). Without the table, direct_eval_native falls back
  // to indirect semantics and `with (o) { eval('x') }` at top level never
  // sees `o`. Box every declared local so the eval's slot aliasing always
  // sees box refs.
  let has_top_level_eval =
    list.any(emitter_ops, fn(op) {
      case op {
        emit.Ir(opcode.IrCallEval(..)) -> True
        _ -> False
      }
    })

  // Determine which variables are captured by children (need boxing)
  let captured_vars = collect_all_captured_vars(children, emitter_ops)
  let captured_vars = case has_top_level_eval {
    True -> set.union(captured_vars, collect_declared_names(emitter_ops))
    False -> captured_vars
  }
  // Box the script's lexical `this` slot too, so run_direct_eval can thread
  // it to the eval body as a boxed capture (eval('this') === globalThis).
  let lexical_captured = case has_top_level_eval {
    True -> opcode.LexicalRefs(True, True, True, True)
    False -> collect_arrow_lexical_refs(children)
  }

  // Phase 2: Resolve scopes (names → local indices), with capture info
  let resolved =
    scope.resolve(
      emitter_ops,
      constants,
      constants_map,
      captured_vars,
      lexical_captured,
      scope.ToGlobal,
      is_strict,
    )

  // Process child functions through Phase 2 + Phase 3 recursively
  let child_templates = compile_children(children, resolved)

  // Expose the name table only when a top-level direct eval needs it —
  // run_direct_eval consults state.func.local_names to decide between
  // direct (aliasing) and indirect fallback semantics. The sentinel head
  // entry marks this frame's VariableEnvironment as the GLOBAL environment:
  // sloppy direct eval here must send `var` declarations to the global
  // object (not a function-frame eval_env dict).
  let local_names = case has_top_level_eval {
    True -> Some([#(global_frame_sentinel, -1), ..dict.to_list(resolved.names)])
    False -> None
  }

  // Phase 3: Resolve labels (label IDs → PC addresses)
  resolve_top_level(
    resolved,
    child_templates,
    is_strict,
    opcode.script_perms,
    local_names,
  )
}

/// True if this function or any nested function contains a syntactic
/// `eval(...)` call. Used to decide which functions need all-locals-boxed.
fn any_descendant_has_eval(child: emit.CompiledChild) -> Bool {
  child.has_eval_call || list.any(child.functions, any_descendant_has_eval)
}

/// True if the op list contains a `with` scope marker. Used by
/// compile_eval_direct to reject `with` when a strict caller upgrades the
/// (sloppy-parsed) eval body to strict code.
fn ops_contain_with(ops: List(emit.EmitterOp)) -> Bool {
  list.any(ops, fn(op) {
    case op {
      emit.EnterWith(_) -> True
      _ -> False
    }
  })
}

fn child_contains_with(child: emit.CompiledChild) -> Bool {
  ops_contain_with(child.code) || list.any(child.functions, child_contains_with)
}

fn compile_child(
  child: emit.CompiledChild,
  parent_scope: Dict(String, Int),
  parent_consts: set.Set(String),
  parent_fn_names: set.Set(String),
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
  // Captures whose origin binding is const (class inner-name binding etc.) —
  // the child's resolver rejects writes through them with TypeError.
  let const_captures =
    set.filter(parent_consts, fn(n) { list.contains(captures, n) })
  // Captures whose origin binding is a named-function-expression self name —
  // the child's resolver makes writes through them throw in strict code and
  // silently drop in sloppy code.
  let fn_name_captures =
    set.filter(parent_fn_names, fn(n) { list.contains(captures, n) })

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
      const_captures,
      fn_name_captures,
      vars_to_box,
      set.new(),
      lexical_captured,
      fallthrough,
      // Enclosing with objects (captured parent locals) sit between this
      // body's scopes and the other captures — free names check them first.
      child.with_stack,
      child.is_strict,
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
    child.length,
    env_descriptors,
    child.is_strict,
    child.is_arrow,
    child.is_derived_constructor,
    child.is_generator,
    child.is_async,
    child.is_constructor,
    child.is_class_constructor,
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

  // For each child, collect free vars and intersect with parent declarations.
  // A child with a direct eval anywhere in its subtree captures EVERY parent
  // binding (the eval source is opaque to free-var analysis — see the
  // capture-all rule in compile_child), so all parent declares must be boxed
  // for it. Without this, a binding referenced only from inside an eval
  // string (e.g. `let q; f(){ eval("q") }` at script top level, or a class
  // private-name const read by eval'd `this.#m`) is captured unboxed →
  // "GetBoxed: local is not a box ref".
  list.fold(children, set.new(), fn(acc, child) {
    case any_descendant_has_eval(child) {
      True -> set.union(acc, parent_declared)
      False ->
        collect_free_vars(child)
        |> set.intersection(parent_declared)
        |> set.union(acc)
    }
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
///
/// SCOPE-AWARE: a use is free unless a declaration is in scope AT THE USE
/// SITE. Function-scoped declares (var/param/capture) satisfy uses anywhere
/// in the body; lexical declares (let/const/catch — including class
/// private-name consts) satisfy only uses inside their block. A name
/// declared solely in a nested or sibling block (e.g. a nested class
/// redeclaring the same `#name`) must NOT hide an outer use — the old
/// whole-function `used − declared` scan got that wrong and silently
/// resolved the outer use to a global. Emit hoists lexical declares to
/// block entry (BlockDeclarationInstantiation), so a declare always
/// precedes its in-scope uses in IR order. Grandchild free names are
/// resolved against the scope state at their IrMakeClosure site.
fn collect_free_vars(child: emit.CompiledChild) -> set.Set(String) {
  let gc_free =
    list.index_map(child.functions, fn(gc, i) { #(i, collect_free_vars(gc)) })
    |> dict.from_list
  // Function-scoped declares are position-independent (hoisting), so collect
  // them up front; lexical declares are tracked by the block stack below.
  let fun_scope =
    list.fold(child.code, set.new(), fn(acc, op) {
      case op {
        emit.DeclareVar(name, emit.VarBinding)
        | emit.DeclareVar(name, emit.ParamBinding)
        | emit.DeclareVar(name, emit.CaptureBinding) -> set.insert(acc, name)
        _ -> acc
      }
    })
  let free = scan_free(child.code, gc_free, fun_scope, [], set.new())
  // Enclosing with-object synthetics count as used: the child's free names
  // must check those objects at runtime, so the slots have to be captured
  // (and boxed) through the closure chain like any referenced parent local.
  // Synthetics declared by this child's own `with` statements are subtracted
  // (they're block-scoped synthetics; whole-body name match is safe since
  // each synthetic name is unique per `with` statement).
  let own_declared =
    list.fold(child.code, set.new(), fn(acc, op) {
      case op {
        emit.DeclareVar(name, _) -> set.insert(acc, name)
        _ -> acc
      }
    })
  let with_names = set.from_list(child.with_stack)
  set.union(free, set.difference(with_names, own_declared))
}

/// Walk EmitterOps with a live lexical block stack, accumulating names used
/// while no enclosing declaration is in scope. `blocks` is the stack of
/// lexical scopes (innermost first).
fn scan_free(
  ops: List(emit.EmitterOp),
  gc_free: Dict(Int, set.Set(String)),
  fun_scope: set.Set(String),
  blocks: List(set.Set(String)),
  free: set.Set(String),
) -> set.Set(String) {
  case ops {
    [] -> free
    [op, ..rest] -> {
      let in_scope = fn(name) {
        set.contains(fun_scope, name) || list.any(blocks, set.contains(_, name))
      }
      let use_name = fn(free, name) {
        case in_scope(name) {
          True -> free
          False -> set.insert(free, name)
        }
      }
      case op {
        emit.EnterScope(_) ->
          scan_free(rest, gc_free, fun_scope, [set.new(), ..blocks], free)
        emit.LeaveScope ->
          case blocks {
            [_, ..outer] -> scan_free(rest, gc_free, fun_scope, outer, free)
            [] -> scan_free(rest, gc_free, fun_scope, [], free)
          }
        // Lexical declare — joins the innermost block (function-scoped kinds
        // were pre-collected into fun_scope).
        emit.DeclareVar(name, emit.LetBinding)
        | emit.DeclareVar(name, emit.ConstBinding)
        | emit.DeclareVar(name, emit.CatchBinding) -> {
          let blocks = case blocks {
            [top, ..outer] -> [set.insert(top, name), ..outer]
            [] -> [set.insert(set.new(), name)]
          }
          scan_free(rest, gc_free, fun_scope, blocks, free)
        }
        // Grandchild closure creation: its free names are resolved against
        // the scope state right here.
        emit.Ir(opcode.IrMakeClosure(i)) -> {
          let free = case dict.get(gc_free, i) {
            Ok(names) -> set.fold(names, free, use_name)
            Error(Nil) -> free
          }
          scan_free(rest, gc_free, fun_scope, blocks, free)
        }
        emit.Ir(opcode.IrScopeGetVar(name))
        | emit.Ir(opcode.IrScopePutVar(name))
        | emit.Ir(opcode.IrScopeTypeofVar(name))
        | emit.Ir(opcode.IrScopeMakeRef(name))
        | emit.Ir(opcode.IrScopeGetRef(name))
        | emit.Ir(opcode.IrScopePutRef(name)) ->
          scan_free(rest, gc_free, fun_scope, blocks, use_name(free, name))
        _ -> scan_free(rest, gc_free, fun_scope, blocks, free)
      }
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
  let parent_consts =
    dict.get(resolved.closure_consts, i) |> result.unwrap(set.new())
  let parent_fn_names =
    dict.get(resolved.closure_fn_names, i) |> result.unwrap(set.new())
  compile_child(
    child,
    parent_scope,
    parent_consts,
    parent_fn_names,
    resolved.lexical,
  )
}
