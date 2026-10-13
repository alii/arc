import arc/bytecode/lazy.{type LazyFunction, LazyFunction}
import arc/bytecode/lexical.{type CodeKind, type LexicalSlots}
import arc/bytecode/opcode
import arc/compiler/assemble
import arc/compiler/ast_util
import arc/compiler/emit
import arc/compiler/registers
import arc/compiler/scope
import arc/compiler/scope_analysis
import arc/compiler/scope_builder
import arc/internal/tuple_array
import arc/module/summary
import arc/parser
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

pub type AloneError {
  AloneParseError(parser.ParseError)
  AloneCompileError(CompileError)
}

/// compiles the one function `function` describes, giving the template the
/// whole-unit compile gives for it. With `lazy`, the plain functions written
/// inside it are in turn left for their own first calls
pub fn compile_function_alone(
  function: LazyFunction,
  lazy lazy: Bool,
) -> Result(FuncTemplate, AloneError) {
  let LazyFunction(
    bytes:,
    is_module:,
    source:,
    name:,
    captures:,
    lexical: lexical_seeds,
    code_kind:,
  ) = function
  let mode = case is_module {
    True -> parser.Module
    False -> parser.Script
  }
  use #(literal, scopes) <- result.try(
    parser.parse_function_at(bytes, mode, source)
    |> result.map_error(AloneParseError),
  )
  let opts =
    scope.AnalyzeOpts(
      ..scope.default_analyze_opts(),
      strict: source.strict,
      parent_names: dict.from_list(
        list.map(captures, fn(capture) { #(capture.0, capture.1) }),
      ),
      parent_seeds: dict.from_list(
        list.map(captures, fn(capture) { #(capture.0, capture.2) }),
      ),
      lexical_captures: dict.from_list(
        list.map(lexical_seeds, fn(seed) { #(seed.0, seed.1) }),
      ),
      lexical_captures_boxed: {
        let boxed = fn(ref) {
          list.any(lexical_seeds, fn(seed) { seed.0 == ref && seed.2 })
        }
        lexical.LexicalRefs(
          this: boxed(lexical.ThisRef),
          active_func: boxed(lexical.ActiveFuncRef),
          home_object: boxed(lexical.HomeObjectRef),
          new_target: boxed(lexical.NewTargetRef),
        )
      },
      root_may_own_lexical: False,
    )
  let tree = scope_analysis.finalize(scopes, opts)
  use #(child, tree) <- result.map(
    emit.function_alone(
      literal,
      tree,
      source.form,
      name,
      strict: source.strict,
      code_kind:,
      lazy:,
    )
    |> result.map_error(AloneCompileError),
  )
  compile_child(child, tree, scope.root_scope_id, Unit(bytes:, is_module:))
}

// the text a compile works from, which a function left for later keeps
type Unit {
  Unit(bytes: BitArray, is_module: Bool)
}

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
  scopes: scope_builder.ScopeBuilder,
) -> Result(FuncTemplate, CompileError) {
  compile_top_level(
    body,
    scopes,
    scope.LocalLexical,
    deletable_global_vars: False,
    lazy: False,
  )
}

/// compile_script, with each plain function left for its first call to
/// compile. An error the emitter would raise for such a function's body is
/// then raised by that call, not here
pub fn compile_script_lazy(
  body: List(ast.StmtWithLine),
  scopes: scope_builder.ScopeBuilder,
) -> Result(FuncTemplate, CompileError) {
  compile_top_level(
    body,
    scopes,
    scope.LocalLexical,
    deletable_global_vars: False,
    lazy: True,
  )
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
  scopes: scope_builder.ScopeBuilder,
  summary: summary.ModuleSummary,
) -> Result(CompiledModuleBody, CompileError) {
  let opts =
    scope.AnalyzeOpts(
      ..scope.default_analyze_opts(),
      top_lex: scope.LocalLexical,
      strict: True,
      parent_names: indexed_names(summary.binding_local_names(summary.imports)),
      linker_seeded_exports: set.from_list(local_export_names(summary.exports)),
    )
  let tree = scope_analysis.finalize(scopes, opts)
  use out <- result.map(emit.module(items, tree, lazy: False))
  let template =
    finish_top_level(
      out,
      lexical.ScriptCode,
      eval_var_env: None,
      unit: Unit(bytes: scopes.source, is_module: True),
    )
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

fn local_export_names(exports: List(summary.ExportEntry)) -> List(String) {
  list.filter_map(exports, fn(entry) {
    case entry {
      summary.LocalExport(local_name:, ..) -> Ok(local_name)
      _ -> Error(Nil)
    }
  })
}

// todo: anonymous export default function should seed undefined too
fn module_export_seeds(
  items: List(ast.ModuleItem),
  exports: List(summary.ExportEntry),
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
  scopes: scope_builder.ScopeBuilder,
) -> Result(FuncTemplate, CompileError) {
  compile_top_level(
    body,
    scopes,
    scope.GlobalLexical,
    deletable_global_vars: False,
    lazy: False,
  )
}

// indirect eval; introduced globals are deletable (§19.2.1.3)
pub fn compile_eval(
  body: List(ast.StmtWithLine),
  scopes: scope_builder.ScopeBuilder,
) -> Result(FuncTemplate, CompileError) {
  compile_top_level(
    body,
    scopes,
    scope.LocalLexical,
    deletable_global_vars: True,
    lazy: False,
  )
}

pub fn compile_eval_direct(
  body: List(ast.StmtWithLine),
  scopes: scope_builder.ScopeBuilder,
  caller: DirectEvalCaller,
) -> Result(FuncTemplate, CompileError) {
  let tree = scope_analysis.finalize(scopes, direct_eval_opts(caller, body))
  // §14.11.1 with is illegal once the caller makes eval strict
  use <- bool.guard(
    caller.is_strict && contains_with(tree),
    Error(emit.EarlySyntaxError("'with' not allowed in strict mode")),
  )
  use out <- result.try(emit.eval_direct(
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
  Ok(finish_top_level(
    out,
    caller.code_kind,
    eval_var_env: Some(caller.var_env),
    unit: Unit(bytes: scopes.source, is_module: False),
  ))
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
    option.is_some(lexical.slot_of(caller.lexical, ref))
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
    top_lex: scope.LocalLexical,
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
  scopes: scope_builder.ScopeBuilder,
  top_lex: scope.TopLevelLex,
  deletable_global_vars deletable_global_vars: Bool,
  lazy lazy: Bool,
) -> Result(FuncTemplate, CompileError) {
  let opts = scope.AnalyzeOpts(..scope.default_analyze_opts(), top_lex:)
  let tree = scope_analysis.finalize(scopes, opts)
  use out <- result.map(emit.program(stmts, tree, deletable_global_vars:, lazy:))
  finish_top_level(
    out,
    lexical.ScriptCode,
    eval_var_env: Some(GlobalVarEnv),
    unit: Unit(bytes: scopes.source, is_module: False),
  )
}

fn finish_top_level(
  out: emit.EmitOutput,
  code_kind: CodeKind,
  eval_var_env eval_var_env: Option(VarEnvKind),
  unit unit: Unit,
) -> FuncTemplate {
  let info = scope.function_info(out.tree, scope.root_scope_id)
  let functions =
    compile_children(out.children, out.tree, scope.root_scope_id, unit)
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
  let assemble.Assembled(bytecode:, constants:, lines:) =
    assemble.assemble(code, constants)
  let #(bytecode, regs) = case use_registers {
    True -> registers.assign_regs(bytecode, captured_slots(functions))
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
  unit: Unit,
) -> FuncTemplate {
  let info = scope.function_info(tree, child.scope_id)
  let parent_info = scope.function_info(tree, parent_fn_scope)
  let env_descriptors =
    list.map(info.captures, fn(c) { c.parent_slot })
    |> list.append(scope.lexical_capture_parent_slots(info, parent_info))
    |> list.map(CaptureLocal)
  use <- option.lazy_unwrap(
    option.map(child.lazy, stub_template(
      child,
      tree,
      info,
      parent_info,
      env_descriptors,
      unit,
      _,
    )),
  )

  let local_names = case info.eval_in_subtree {
    True -> Some(eval_name_table(FrameVarEnv, info))
    False -> None
  }
  let functions = compile_children(child.functions, tree, child.scope_id, unit)
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
  unit: Unit,
) -> List(FuncTemplate) {
  list.map(children, compile_child(_, tree, parent_fn_scope, unit))
}

// what a function left for later carries: its text and what its parent hands it
fn lazy_function(
  child: emit.CompiledChild,
  tree: scope.ScopeTree,
  info: scope.FunctionInfo,
  parent_info: scope.FunctionInfo,
  unit: Unit,
  source: scope.FnSource,
) -> LazyFunction {
  let own = scope.get(tree, child.scope_id)
  let captures =
    list.map(info.captures, fn(capture) {
      let assert Ok(binding) = dict.get(own.bindings, capture.name)
        as "compiler: a captured name is not bound in its function's scope"
      #(
        capture.name,
        capture.parent_slot,
        scope.CaptureSeed(binding.boxed, binding.declared_kind),
      )
    })
  LazyFunction(
    bytes: unit.bytes,
    is_module: unit.is_module,
    source:,
    name: child.name,
    captures:,
    lexical: {
      use ref <- list.filter_map(lexical.all_lexical_refs)
      use <- bool.guard(!dict.has_key(info.lexical_captures, ref), Error(Nil))
      use slot <- result.map(
        lexical.slot_of(parent_info.lexical, ref) |> option.to_result(Nil),
      )
      #(ref, slot, lexical.refs_get(info.lexical_boxed, ref))
    },
    code_kind: child.code_kind,
  )
}

// a function whose whole body is "compile me": it has the frame layout the
// analysis gave it, and carries what its first call needs to compile it
fn stub_template(
  child: emit.CompiledChild,
  tree: scope.ScopeTree,
  info: scope.FunctionInfo,
  parent_info: scope.FunctionInfo,
  env_descriptors: List(EnvCapture),
  unit: Unit,
  source: scope.FnSource,
) -> FuncTemplate {
  let function = lazy_function(child, tree, info, parent_info, unit, source)
  // every stub owns the four lexical slots after its captures, so the first
  // call finds the function object there whatever the real layout is
  let captured = list.length(env_descriptors)
  FuncTemplate(
    name: child.name,
    arity: child.arity,
    length: child.length,
    local_count: captured + lexical.owned_lexical_slot_count + child.arity,
    bytecode: tuple_array.from_list([
      opcode.CompileSelf(function),
      opcode.Return,
    ]),
    constants: tuple_array.from_list([]),
    lines: tuple_array.from_list([source.line, source.line]),
    functions: tuple_array.from_list([]),
    env_descriptors:,
    is_strict: child.is_strict,
    is_arrow: child.is_arrow,
    is_derived_constructor: False,
    is_generator: child.is_generator,
    is_async: child.is_async,
    is_constructor: child.is_constructor,
    is_class_constructor: False,
    local_names: None,
    lexical: lexical.OwnedLexicalSlots(base: captured),
    code_kind: child.code_kind,
    regs: bytecode.NoRegs,
  )
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

pub type SourceKind {
  ScriptSource
  ModuleSource
  ReplSource
}

pub type SourceError {
  Syntax(parser.ParseError)
  Compile(CompileError)
}

pub fn format_source_error(err: SourceError) -> String {
  case err {
    Syntax(parse_err) -> "SyntaxError: " <> parser.error_to_string(parse_err)
    Compile(compile_err) -> "compile error: " <> error_message(compile_err)
  }
}

pub fn compile_source(
  kind: SourceKind,
  source: String,
) -> Result(FuncTemplate, SourceError) {
  case kind {
    ScriptSource -> {
      use #(body, scopes) <- result.try(
        parser.parse_script(source) |> result.map_error(Syntax),
      )
      compile_script(body, scopes) |> result.map_error(Compile)
    }
    ReplSource -> {
      use #(body, scopes) <- result.try(
        parser.parse_script(source) |> result.map_error(Syntax),
      )
      compile_repl(body, scopes) |> result.map_error(Compile)
    }
    ModuleSource -> {
      use #(items, scopes) <- result.try(
        parser.parse_module(source) |> result.map_error(Syntax),
      )
      use compiled <- result.map(
        compile_module(items, scopes, summary.analyze(items))
        |> result.map_error(Compile),
      )
      compiled.template
    }
  }
}
