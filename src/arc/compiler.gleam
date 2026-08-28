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
  type EvalNameTable, type FuncTemplate, type VarEnvKind, CaptureLocal,
  EvalNameTable, FrameVarEnv, FuncTemplate, GlobalVarEnv,
}
import arc/rt/types.{type JsVal}
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

pub type Strictness {
  Strict
  Sloppy
}

pub type DirectEvalCaller {
  DirectEvalCaller(
    // seeded into capture slots 0..n-1 in this order
    names: List(String),
    slots: LexicalSlots,
    code_kind: CodeKind,
    strictness: Strictness,
    var_env: VarEnvKind,
    param_scope_names: List(String),
    with_names: List(String),
    private_names: List(String),
  )
}

fn resolve_top_level(
  code: List(opcode.IrOp),
  constants: List(JsVal),
  info: scope.FunctionInfo,
  child_templates: List(FuncTemplate),
  is_strict: Bool,
  code_kind: CodeKind,
  local_names: Option(EvalNameTable),
) -> FuncTemplate {
  let resolve.Resolved(bytecode:, constants:, lines:) =
    resolve.resolve(code, constants)
  let #(bytecode, regs) = case local_names {
    None -> resolve.assign_regs(bytecode, captured_slots(child_templates))
    Some(_) -> #(bytecode, bytecode.NoRegs)
  }
  FuncTemplate(
    name: None,
    arity: 0,
    length: 0,
    local_count: info.local_count,
    bytecode:,
    constants:,
    lines:,
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
    regs:,
  )
}

fn captured_slots(children: List(FuncTemplate)) -> Set(Int) {
  list.flat_map(children, fn(t) {
    list.map(t.env_descriptors, fn(c) { c.parent_index })
  })
  |> set.from_list
}

pub fn compile(
  body: List(ast.StmtWithLine),
  sb: scope.ScopeBuilder,
) -> Result(FuncTemplate, CompileError) {
  compile_script(body, sb, scope.LexLocal, deletable_global_vars: False)
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
  let import_locals = esm.import_local_names(summary)
  let forced_box = local_export_names(summary.exports)
  compile_module_with_scope(items, sb, import_locals, forced_box, summary)
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
  let stmts = ast_util.module_items_to_stmts(items)
  let undef =
    set.from_list(list.append(
      ast_util.collect_hoisted_vars(stmts),
      ast_util.direct_fn_names(stmts),
    ))
  local_export_names(exports)
  |> list.fold(dict.new(), fn(acc, name) {
    let seed = case set.contains(undef, name) {
      True -> SeedUndefined
      False -> SeedUninitialized
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
  let opts =
    scope.AnalyzeOpts(
      ..scope.default_analyze_opts(),
      top_lex: scope.LexLocal,
      strict: True,
      parent_names: indexed_names(import_locals),
      linker_seeded: set.from_list(forced_box),
    )
  let tree = scope.finalize(sb, opts)
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
      lexical.ScriptCode,
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

// top-level lexicals go to the global record to persist
pub fn compile_repl(
  body: List(ast.StmtWithLine),
  sb: scope.ScopeBuilder,
) -> Result(FuncTemplate, CompileError) {
  compile_script(body, sb, scope.LexGlobal, deletable_global_vars: False)
}

// indirect eval; introduced globals are deletable (§19.2.1.3)
pub fn compile_eval(
  body: List(ast.StmtWithLine),
  sb: scope.ScopeBuilder,
) -> Result(FuncTemplate, CompileError) {
  compile_script(body, sb, scope.LexLocal, deletable_global_vars: True)
}

pub fn compile_eval_direct(
  body: List(ast.StmtWithLine),
  sb: scope.ScopeBuilder,
  caller: DirectEvalCaller,
) -> Result(FuncTemplate, CompileError) {
  let caller_is_strict = caller.strictness == Strict
  let caller_is_global = caller.var_env == GlobalVarEnv
  // finalize does not scan directives, so check the body too
  let effective_strict =
    caller_is_strict || ast_util.has_use_strict_directive(body)
  let parent_dict = indexed_names(caller.names)
  // lexical box refs follow names, one slot per some entry
  let #(lexical_captures, _next) =
    list.fold(
      lexical.all_lexical_refs,
      #(dict.new(), list.length(caller.names)),
      fn(acc, ref) {
        let #(m, i) = acc
        case lexical.lexical_slot(caller.slots, ref) {
          Some(_) -> #(dict.insert(m, ref, i), i + 1)
          None -> acc
        }
      },
    )
  // every with holder must be one of caller.names
  let with_stack =
    list.map(caller.with_names, fn(n) {
      let assert Ok(slot) = dict.get(parent_dict, n)
        as "direct-eval caller's with-holder is not one of its local names"
      slot
    })
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
  // §14.11.1 with is illegal once the caller makes eval strict
  use Nil <- result.try(
    case
      caller_is_strict
      && list.any(dict.values(tree.scopes), fn(s) { scope.is_with_kind(s.kind) })
    {
      True -> Error(emit.EarlySyntaxError("'with' not allowed in strict mode"))
      False -> Ok(Nil)
    },
  )
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
  // §19.2.1.1 evaldeclarationinstantiation step 3.d
  use Nil <- result.try(case strict {
    True -> Ok(Nil)
    False -> check_param_scope_var_conflict(body, caller.param_scope_names)
  })
  let info = scope.function_info(tree, scope.root_scope_id)
  let child_templates = compile_children(children, tree, scope.root_scope_id)
  let local_names = case info.contains_direct_eval {
    True ->
      Some(EvalNameTable(
        var_env: caller.var_env,
        names: dict.to_list(info.names),
      ))
    False -> None
  }
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
  deletable_global_vars deletable_global_vars: Bool,
) -> Result(FuncTemplate, CompileError) {
  let opts = scope.AnalyzeOpts(..scope.default_analyze_opts(), top_lex:)
  let tree = scope.finalize(sb, opts)
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
  let child_templates = compile_children(children, tree, scope.root_scope_id)
  let local_names = case info.contains_direct_eval {
    True ->
      Some(EvalNameTable(var_env: GlobalVarEnv, names: dict.to_list(info.names)))
    False -> None
  }
  resolve_top_level(
    code,
    constants,
    info,
    child_templates,
    is_strict,
    lexical.ScriptCode,
    local_names,
  )
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

  // layout must match setup_frame
  let lex_descriptors =
    list.filter_map(lexical.all_lexical_refs, fn(ref) {
      case dict.has_key(info.lexical_captures, ref) {
        False -> Error(Nil)
        True ->
          case lexical.lexical_slot(parent_info.lexical, ref) {
            Some(parent_idx) -> Ok(CaptureLocal(parent_idx))
            None ->
              panic as "scope analyzer recorded a lexical capture the parent has no slot for"
          }
      }
    })
  let env_descriptors =
    list.map(info.captures, fn(c) { CaptureLocal(c.1) })
    |> list.append(lex_descriptors)

  let local_names = case info.eval_in_subtree {
    True ->
      Some(EvalNameTable(var_env: FrameVarEnv, names: dict.to_list(info.names)))
    False -> None
  }

  let grandchild_templates =
    compile_children(child.functions, tree, child.scope_id)

  let resolve.Resolved(bytecode:, constants:, lines:) =
    resolve.resolve(child.code, child.constants)
  // coroutine frames park with raw locals, so they never get registers
  let #(bytecode, regs) = case
    local_names,
    child.is_generator || child.is_async
  {
    None, False ->
      resolve.assign_regs(bytecode, captured_slots(grandchild_templates))
    _, _ -> #(bytecode, bytecode.NoRegs)
  }
  FuncTemplate(
    name: child.name,
    arity: child.arity,
    length: child.length,
    local_count: info.local_count,
    bytecode:,
    constants:,
    lines:,
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
    regs:,
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
