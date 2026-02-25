/// Bytecode Compiler
///
/// Translates a parsed AST into a FuncTemplate that the VM can execute.
/// Three-phase pipeline:
///   Phase 1 (emit): AST → EmitterOp (symbolic names + label IDs)
///   Phase 2 (scope): EmitterOp → IrOp (resolved local indices + label IDs)
///   Phase 3 (resolve): IrOp → Op (absolute PC addresses)
import arc/ast
import arc/compiler/emit
import arc/compiler/resolve
import arc/compiler/scope
import arc/vm/opcode.{type FuncTemplate}
import arc/vm/value
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{None}
import gleam/set

/// Compilation errors.
pub type CompileError {
  BreakOutsideLoop
  ContinueOutsideLoop
  Unsupported(description: String)
}

/// Compile a parsed program into a FuncTemplate the VM can execute.
pub fn compile(program: ast.Program) -> Result(FuncTemplate, CompileError) {
  case program {
    ast.Script(body) -> compile_script(body, emit.emit_program)
    ast.Module(_) -> Error(Unsupported("modules not supported"))
  }
}

/// Compile in REPL mode: top-level var/let/const resolve to globals.
pub fn compile_repl(program: ast.Program) -> Result(FuncTemplate, CompileError) {
  case program {
    ast.Script(body) -> compile_script(body, emit.emit_program_repl)
    ast.Module(_) -> Error(Unsupported("modules not supported"))
  }
}

fn compile_script(
  stmts: List(ast.Statement),
  emit_fn: fn(List(ast.Statement)) ->
    Result(
      #(
        List(emit.EmitterOp),
        List(value.JsValue),
        Dict(value.JsValue, Int),
        List(emit.CompiledChild),
        Bool,
      ),
      emit.EmitError,
    ),
) -> Result(FuncTemplate, CompileError) {
  // Phase 1: Emit IR from AST
  case emit_fn(stmts) {
    Ok(#(emitter_ops, constants, constants_map, children, is_strict)) -> {
      // Determine which variables are captured by children (need boxing)
      let captured_vars = collect_all_captured_vars(children, emitter_ops)

      // Phase 2: Resolve scopes (names → local indices), with capture info
      let #(ir_ops, local_count, constants, _constants_map) =
        scope.resolve(emitter_ops, constants, constants_map, captured_vars)

      // Build scope dict for this level (name → local index)
      let parent_scope = build_scope_dict(emitter_ops)

      // Process child functions through Phase 2 + Phase 3 recursively
      let child_templates = list.map(children, compile_child(_, parent_scope))

      // Phase 3: Resolve labels (label IDs → PC addresses)
      let template =
        resolve.resolve(
          ir_ops,
          constants,
          local_count,
          child_templates,
          None,
          0,
          [],
          is_strict,
          False,
          False,
          False,
          False,
        )
      Ok(template)
    }
    Error(emit.BreakOutsideLoop) -> Error(BreakOutsideLoop)
    Error(emit.ContinueOutsideLoop) -> Error(ContinueOutsideLoop)
    Error(emit.Unsupported(desc)) -> Error(Unsupported(desc))
  }
}

fn compile_child(
  child: emit.CompiledChild,
  parent_scope: Dict(String, Int),
) -> FuncTemplate {
  // Determine which variables this child uses but doesn't declare (free vars)
  let free_vars = collect_free_vars(child)

  // Filter to names that exist in parent scope (others are globals)
  let captures =
    set.filter(free_vars, dict.has_key(parent_scope, _))
    |> set.to_list

  // Build env_descriptors: for each captured name, CaptureLocal(parent_index)
  let env_descriptors =
    list.map(captures, fn(name) {
      let assert Ok(parent_index) = dict.get(parent_scope, name)
      opcode.CaptureLocal(parent_index)
    })

  // Determine which of this child's vars are captured by grandchildren
  let grandchild_captured =
    collect_all_captured_vars(child.functions, child.code)

  // Phase 2: Resolve scopes, with captures pre-populated
  let #(ir_ops, local_count, constants, _constants_map) = case captures {
    [] ->
      scope.resolve(
        child.code,
        child.constants,
        child.constants_map,
        grandchild_captured,
      )
    _ ->
      scope.resolve_with_captures(
        child.code,
        child.constants,
        child.constants_map,
        captures,
        grandchild_captured,
      )
  }

  // Build scope dict for this child (for grandchildren captures)
  let child_scope = build_scope_dict_with_captures(child.code, captures)

  // Recursively compile grandchildren
  let grandchild_templates =
    list.map(child.functions, compile_child(_, child_scope))

  // Phase 3: Resolve labels
  resolve.resolve(
    ir_ops,
    constants,
    local_count,
    grandchild_templates,
    child.name,
    child.arity,
    env_descriptors,
    child.is_strict,
    child.is_arrow,
    child.is_derived_constructor,
    child.is_generator,
    child.is_async,
  )
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
fn collect_free_vars(child: emit.CompiledChild) -> set.Set(String) {
  let #(used, declared) = scan_ops(child.code, set.new(), set.new())
  set.difference(used, declared)
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

// ============================================================================
// Scope dict building
// ============================================================================

/// Build a scope dict from EmitterOps by simulating scope resolution.
/// Returns a Dict mapping variable name → local slot index.
/// This runs the same logic as scope.resolve but only tracks the bindings.
fn build_scope_dict(ops: List(emit.EmitterOp)) -> Dict(String, Int) {
  build_scope_dict_loop(ops, [], 0, dict.new())
}

/// Build scope dict with pre-populated captures.
fn build_scope_dict_with_captures(
  ops: List(emit.EmitterOp),
  captures: List(String),
) -> Dict(String, Int) {
  let capture_count = list.length(captures)
  let initial =
    list.index_map(captures, fn(name, idx) { #(name, idx) })
    |> dict.from_list()
  build_scope_dict_loop(ops, [], capture_count, initial)
}

/// Walk EmitterOps tracking DeclareVar to build name→index mapping.
/// We track scopes to handle block-scoped let/const correctly —
/// block-scoped vars are still added to the dict since they have valid indices.
fn build_scope_dict_loop(
  ops: List(emit.EmitterOp),
  scopes: List(emit.ScopeKind),
  next_local: Int,
  acc: Dict(String, Int),
) -> Dict(String, Int) {
  case ops {
    [] -> acc
    [op, ..rest] ->
      case op {
        emit.DeclareVar(name, _kind) -> {
          // Only record first declaration (matches scope.resolve which keeps first)
          let acc = case dict.has_key(acc, name) {
            True -> acc
            False -> dict.insert(acc, name, next_local)
          }
          build_scope_dict_loop(rest, scopes, next_local + 1, acc)
        }
        emit.EnterScope(kind) ->
          build_scope_dict_loop(rest, [kind, ..scopes], next_local, acc)
        emit.LeaveScope ->
          case scopes {
            [_, ..rest_scopes] ->
              build_scope_dict_loop(rest, rest_scopes, next_local, acc)
            [] -> build_scope_dict_loop(rest, [], next_local, acc)
          }
        _ -> build_scope_dict_loop(rest, scopes, next_local, acc)
      }
  }
}
