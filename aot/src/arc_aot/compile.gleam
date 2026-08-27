import arc/parser
import arc_aot/emit
import arc_aot/emit/state
import carder/ir
import carder/ir/printer
import carder/pipeline
import gleam/result

pub type CompileError {
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

pub fn to_beam(
  source: String,
  module_name: String,
) -> Result(BitArray, CompileError) {
  use module <- result.try(to_ir(source, module_name))
  ir_to_beam(module)
}

pub fn ir_to_core(module: ir.Module) -> Result(String, CompileError) {
  pipeline.ir_to_core(module, emit.binding())
  |> result.map_error(PipelineFailed)
}

pub fn ir_to_text(module: ir.Module) -> String {
  printer.print_module(module)
}
