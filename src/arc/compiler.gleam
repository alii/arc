import arc/bytecode/lexical.{type CodeKind, type LexicalSlots}
import arc/bytecode/opcode
import arc/compiler/ast_util
import arc/compiler/emit
import arc/compiler/resolve
import arc/compiler/scope
import arc/esm
import arc/internal/tuple_array
import arc/parser/ast
import arc/rt/bytecode.{
  type EnvCapture, type EvalNameTable, type FuncTemplate, type VarEnvKind,
  CaptureLocal, EvalNameTable, FrameVarEnv, FuncTemplate, GlobalVarEnv,
}
import arc/rt/types.{type JsVal}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}

pub type CompileError =
  emit.EmitError

pub fn error_message(err: CompileError) -> String {
  case err {
    emit.BreakOutsideLoop -> "break outside loop"
    emit.ContinueOutsideLoop -> "continue outside loop"
    emit.EarlySyntaxError(message:) -> message
    emit.UnsupportedFeature(feature:) -> "unsupported: " <> feature
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
    emit.NonGenericUnaryOperator ->
      internal_error("typeof/delete in generic unary expression")
  }
}

fn internal_error(context: String) -> String {
  "internal compiler error: " <> context
}

pub type DirectEvalCaller {
  DirectEvalCaller(
    // seeded into capture slots 0..n-1 in this order
    slot_names: List(String),
    lexical: LexicalSlots,
    code_kind: CodeKind,
    is_strict: Bool,
    var_env: VarEnvKind,
    param_scope_names: List(String),
    with_names: List(String),
    private_names: List(String),
  )
}

pub fn compile_script(
  body: List(ast.StmtWithLine),
  sb: scope.ScopeBuilder,
) -> Result(FuncTemplate, CompileError) {
  compile_top_level(body, sb, scope.LexLocal, deletable_global_vars: False)
}

pub type CompiledModuleBody {
  CompiledModuleBody(
    template: FuncTemplate,
    export_names: Dict(String, Int),
    hoisted_funcs: List(#(String, Int)),
    export_seeds: Dict(String, ExportSeed),
    has_tla: Bool,
  )
}

pub type ExportSeed {
  SeedUndefined
  SeedUninitialized
}

// imports are boxed captures 0..n-1; exports force-boxed for live bindings
pub fn compile_module(
  items: List(ast.ModuleItem),
  sb: scope.ScopeBuilder,
  summary: esm.ModuleSummary,
) -> Result(CompiledModuleBody, CompileError) {
  let opts =
    scope.AnalyzeOpts(
      ..scope.default_analyze_opts(),
      top_lex: scope.LexLocal,
      strict: True,
      parent_names: indexed_names(esm.import_local_names(summary)),
      linker_seeded_exports: set.from_list(local_export_names(summary.exports)),
    )
  let tree = scope.finalize(sb, opts)
  use out <- result.map(emit.emit_module(items, tree))
  let template = finish_top_level(out, lexical.ScriptCode, eval_var_env: None)
  let has_tla =
    tuple_array.to_list(template.bytecode)
    |> list.any(fn(op) { op == opcode.Await })
  CompiledModuleBody(
    template:,
    export_names: scope.function_info(out.tree, scope.root_scope_id).slot_by_name,
    hoisted_funcs: out.module_hoisted_funcs,
    export_seeds: module_export_seeds(items, summary.exports),
    has_tla:,
  )
}

fn local_export_names(exports: List(esm.ExportEntry)) -> List(String) {
  list.filter_map(exports, fn(entry) {
    case entry {
      esm.LocalExport(local_name:, ..) -> Ok(local_name)
      _ -> Error(Nil)
    }
  })
}

// todo: anonymous export default function should seed undefined too
fn module_export_seeds(
  items: List(ast.ModuleItem),
  exports: List(esm.ExportEntry),
) -> Dict(String, ExportSeed) {
  let undef =
    ast_util.module_items_to_stmts(items)
    |> ast_util.var_scoped_names
    |> set.from_list
  local_export_names(exports)
  |> list.map(fn(name) {
    case set.contains(undef, name) {
      True -> #(name, SeedUndefined)
      False -> #(name, SeedUninitialized)
    }
  })
  |> dict.from_list
}

// top-level lexicals go to the global record to persist
pub fn compile_repl(
  body: List(ast.StmtWithLine),
  sb: scope.ScopeBuilder,
) -> Result(FuncTemplate, CompileError) {
  compile_top_level(body, sb, scope.LexGlobal, deletable_global_vars: False)
}

// indirect eval; introduced globals are deletable (§19.2.1.3)
pub fn compile_eval(
  body: List(ast.StmtWithLine),
  sb: scope.ScopeBuilder,
) -> Result(FuncTemplate, CompileError) {
  compile_top_level(body, sb, scope.LexLocal, deletable_global_vars: True)
}

pub fn compile_eval_direct(
  body: List(ast.StmtWithLine),
  sb: scope.ScopeBuilder,
  caller: DirectEvalCaller,
) -> Result(FuncTemplate, CompileError) {
  let tree = scope.finalize(sb, direct_eval_opts(caller, body))
  // §14.11.1 with is illegal once the caller makes eval strict
  use <- bool.guard(
    caller.is_strict && contains_with(tree),
    Error(emit.EarlySyntaxError("'with' not allowed in strict mode")),
  )
  use out <- result.try(emit.emit_eval_direct(
    body,
    tree,
    caller.is_strict,
    caller.param_scope_names,
    caller.private_names,
  ))
  // §19.2.1.1 evaldeclarationinstantiation step 3.d
  use Nil <- result.try(case out.is_strict {
    True -> Ok(Nil)
    False -> check_param_scope_var_conflict(body, caller.param_scope_names)
  })
  Ok(finish_top_level(out, caller.code_kind, eval_var_env: Some(caller.var_env)))
}

fn direct_eval_opts(
  caller: DirectEvalCaller,
  body: List(ast.StmtWithLine),
) -> scope.AnalyzeOpts {
  // finalize does not scan directives, so check the body too
  let strict = caller.is_strict || ast_util.has_use_strict_directive(body)
  let parent_names = indexed_names(caller.slot_names)
  // lexical box refs follow the names, one slot per ref the caller has
  let lexical_captures = {
    use ref <- lexical.number_refs(from: list.length(caller.slot_names))
    option.is_some(lexical.lexical_slot(caller.lexical, ref))
  }
  // every with holder must be one of caller.slot_names
  let with_stack =
    list.map(caller.with_names, fn(n) {
      let assert Ok(slot) = dict.get(parent_names, n)
        as "direct-eval caller's with-holder is not one of its local names"
      slot
    })
  let fallthrough = case strict || caller.var_env == GlobalVarEnv {
    True -> scope.ToGlobal
    False -> scope.ToEvalEnv
  }
  scope.AnalyzeOpts(
    ..scope.default_analyze_opts(),
    top_lex: scope.LexLocal,
    fallthrough:,
    strict:,
    parent_names:,
    lexical_captures:,
    with_stack:,
  )
}

fn contains_with(tree: scope.ScopeTree) -> Bool {
  list.any(dict.values(tree.scopes), fn(s) { scope.is_with_kind(s.kind) })
}

fn compile_top_level(
  stmts: List(ast.StmtWithLine),
  sb: scope.ScopeBuilder,
  top_lex: scope.TopLevelLex,
  deletable_global_vars deletable_global_vars: Bool,
) -> Result(FuncTemplate, CompileError) {
  let opts = scope.AnalyzeOpts(..scope.default_analyze_opts(), top_lex:)
  let tree = scope.finalize(sb, opts)
  use out <- result.map(emit.emit_program(stmts, tree, deletable_global_vars:))
  finish_top_level(out, lexical.ScriptCode, eval_var_env: Some(GlobalVarEnv))
}

fn finish_top_level(
  out: emit.EmitOutput,
  code_kind: CodeKind,
  eval_var_env eval_var_env: Option(VarEnvKind),
) -> FuncTemplate {
  let info = scope.function_info(out.tree, scope.root_scope_id)
  let functions = compile_children(out.children, out.tree, scope.root_scope_id)
  let local_names = case eval_var_env, info.contains_direct_eval {
    Some(var_env), True -> Some(eval_name_table(var_env, info))
    _, _ -> None
  }
  build_template(
    out.code,
    out.constants,
    info,
    functions,
    [],
    local_names,
    is_strict: out.is_strict,
    code_kind:,
    use_registers: option.is_none(local_names),
  )
}

fn eval_name_table(
  var_env: VarEnvKind,
  info: scope.FunctionInfo,
) -> EvalNameTable {
  EvalNameTable(var_env:, names: dict.to_list(info.slot_by_name))
}

fn build_template(
  code: List(opcode.IrOp),
  constants: List(JsVal),
  info: scope.FunctionInfo,
  functions: List(FuncTemplate),
  env_descriptors: List(EnvCapture),
  local_names: Option(EvalNameTable),
  is_strict is_strict: Bool,
  code_kind code_kind: CodeKind,
  use_registers use_registers: Bool,
) -> FuncTemplate {
  let resolve.Resolved(bytecode:, constants:, lines:) =
    resolve.resolve(code, constants)
  let #(bytecode, regs) = case use_registers {
    True -> resolve.assign_regs(bytecode, captured_slots(functions))
    False -> #(bytecode, bytecode.NoRegs)
  }
  FuncTemplate(
    name: None,
    arity: 0,
    length: 0,
    local_count: info.local_count,
    bytecode:,
    constants:,
    lines:,
    functions: tuple_array.from_list(functions),
    env_descriptors:,
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
    regs:,
  )
}

fn captured_slots(children: List(FuncTemplate)) -> Set(Int) {
  list.flat_map(children, fn(t) {
    list.map(t.env_descriptors, fn(c) { c.parent_index })
  })
  |> set.from_list
}

fn indexed_names(names: List(String)) -> Dict(String, Int) {
  list.index_map(names, fn(n, i) { #(n, i) }) |> dict.from_list
}

fn compile_child(
  child: emit.CompiledChild,
  tree: scope.ScopeTree,
  parent_fn_scope: scope.ScopeId,
) -> FuncTemplate {
  let info = scope.function_info(tree, child.scope_id)
  let parent_info = scope.function_info(tree, parent_fn_scope)
  let env_descriptors =
    list.map(info.captures, fn(c) { c.parent_slot })
    |> list.append(scope.lexical_capture_parent_slots(info, parent_info))
    |> list.map(CaptureLocal)

  let local_names = case info.eval_in_subtree {
    True -> Some(eval_name_table(FrameVarEnv, info))
    False -> None
  }
  let functions = compile_children(child.functions, tree, child.scope_id)
  // coroutine frames park with raw locals, so they never get registers
  let use_registers =
    option.is_none(local_names) && !child.is_generator && !child.is_async
  let template =
    build_template(
      child.code,
      child.constants,
      info,
      functions,
      env_descriptors,
      local_names,
      is_strict: child.is_strict,
      code_kind: child.code_kind,
      use_registers:,
    )
  FuncTemplate(
    ..template,
    name: child.name,
    arity: child.arity,
    length: child.length,
    is_arrow: child.is_arrow,
    is_derived_constructor: child.is_derived_constructor,
    is_generator: child.is_generator,
    is_async: child.is_async,
    is_constructor: child.is_constructor,
    is_class_constructor: child.is_class_constructor,
  )
}

fn compile_children(
  children: List(emit.CompiledChild),
  tree: scope.ScopeTree,
  parent_fn_scope: scope.ScopeId,
) -> List(FuncTemplate) {
  list.map(children, compile_child(_, tree, parent_fn_scope))
}

fn check_param_scope_var_conflict(
  body: List(ast.StmtWithLine),
  param_scope_names: List(String),
) -> Result(Nil, CompileError) {
  use <- bool.guard(param_scope_names == [], Ok(Nil))
  let conflict =
    ast_util.var_scoped_names(body)
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
