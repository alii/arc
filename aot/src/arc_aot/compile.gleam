//// JS source → 2core IR → BEAM binary: the one compile path the CLI and the
//// test262 runner share.

import arc/parser
import arc_aot/emit
import arc_aot/emit/state
import gleam/result
import twocore/ir
import twocore/ir/printer
import twocore/pipeline

pub type CompileError {
  /// The source parses as an ES module (`import`/`export`), which the AOT
  /// compiler does not handle yet.
  ModuleGoalUnsupported
  EmitFailed(emit.EmitError)
  PipelineFailed(pipeline.PipelineError)
}

pub fn describe(error: CompileError) -> String {
  case error {
    ModuleGoalUnsupported ->
      "ES modules (import/export) are not supported by the AOT compiler"
    EmitFailed(err) -> describe_emit_error(err)
    PipelineFailed(err) -> pipeline.describe(err)
  }
}

pub fn describe_emit_error(error: emit.EmitError) -> String {
  case error {
    state.BreakOutsideLoop -> "SyntaxError: break outside loop"
    state.ContinueOutsideLoop -> "SyntaxError: continue outside loop"
    state.EarlySyntaxError(message:) -> "SyntaxError: " <> message
    state.UnsupportedFeature(feature:) -> "unsupported: " <> feature
    state.ScopeCursorDesync(..) -> "internal: scope cursor desync"
  }
}

/// Lower a script to IR under `module_name`. A source that fails to parse as
/// a script but parses as a module is reported as `ModuleGoalUnsupported`
/// rather than as its script-goal syntax error.
pub fn to_ir(
  source: String,
  module_name: String,
) -> Result(ir.Module, CompileError) {
  case script_to_ir(source, module_name) {
    Error(state.EarlySyntaxError(_) as err) ->
      case parser.parse(source, parser.Module) {
        Ok(_) -> Error(ModuleGoalUnsupported)
        Error(_still_a_syntax_error) -> Error(EmitFailed(err))
      }
    Error(err) -> Error(EmitFailed(err))
    Ok(module) -> Ok(module)
  }
}

/// Lower a script to IR under `module_name`, no module-goal probe.
pub fn script_to_ir(
  source: String,
  module_name: String,
) -> Result(ir.Module, emit.EmitError) {
  let opts =
    emit.CompileOpts(
      module_name:,
      source_kind: emit.AsScript,
      entry_name: "js_main",
    )
  emit.compile_source(source, opts)
  |> result.map(fn(unit) { unit.module })
}

pub fn ir_to_beam(module: ir.Module) -> Result(BitArray, CompileError) {
  pipeline.compile_ir(module, emit.binding())
  |> result.map_error(PipelineFailed)
}

/// Source → loadable BEAM binary whose module atom is `module_name`.
pub fn to_beam(
  source: String,
  module_name: String,
) -> Result(BitArray, CompileError) {
  use module <- result.try(to_ir(source, module_name))
  ir_to_beam(module)
}

/// Core Erlang text for `module` (inspection only).
pub fn ir_to_core(module: ir.Module) -> Result(String, CompileError) {
  pipeline.ir_to_core(module, emit.binding())
  |> result.map_error(PipelineFailed)
}

/// IR text for `module` (inspection only).
pub fn ir_to_text(module: ir.Module) -> String {
  printer.print_module(module)
}
