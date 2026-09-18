import arc/parser
import arc_aot/emit
import arc_aot/emit/state
import carder/ir
import carder/ir/printer
import carder/pipeline
import gleam/result

pub type CompileError {
  ModuleGoalUnsupported
  EmitFailed(state.EmitError)
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

pub fn describe_emit_error(error: state.EmitError) -> String {
  let message = state.describe_error(error)
  case error {
    state.BreakOutsideLoop
    | state.ContinueOutsideLoop
    | state.EarlySyntaxError(..) -> "SyntaxError: " <> message
    state.UnsupportedFeature(..) -> message
    state.ScopeCursorDesync(..) -> "internal: " <> message
  }
}

pub fn to_ir(
  source: String,
  module_name: String,
) -> Result(ir.Module, CompileError) {
  case script_to_ir(source, module_name) {
    Error(state.EarlySyntaxError(..) as err) ->
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
) -> Result(ir.Module, state.EmitError) {
  emit.compile_source(
    source,
    emit.CompileOpts(module_name:, source_kind: emit.AsScript),
  )
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
