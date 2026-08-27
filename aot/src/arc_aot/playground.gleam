import arc_aot/compile
import arc_aot/emit
import carder/backend/core_erlang.{type CModule, CModule}
import carder/backend/core_printer
import carder/backend/eaf
import carder/pipeline
import gleam/result
import gleam/string

pub type Emitted {
  Emitted(ir: String, core: String, erlang: String)
}

pub fn emit(source: String, module_name: String) -> Result(Emitted, String) {
  use ir <- result.try(
    compile.to_ir(source, module_name) |> result.map_error(compile.describe),
  )
  use cmod <- result.try(
    pipeline.ir_to_cmod(ir, emit.binding())
    |> result.map_error(pipeline.describe),
  )
  use forms <- result.map(
    eaf.module_forms(cmod) |> result.map_error(eaf.describe),
  )
  let erlang = forms_to_erl(forms)
  Emitted(ir: compile.ir_to_text(ir), core: core_text(cmod), erlang:)
}

@external(erlang, "arc_aot_pp_ffi", "forms_to_erl")
fn forms_to_erl(forms: List(eaf.Form)) -> String

fn core_text(cmod: CModule) -> String {
  let header =
    core_printer.print_module(CModule(..cmod, defs: []))
    |> string.drop_end(string.length("\nend\n"))
  let defs =
    pmap(cmod.defs, fn(def) {
      let one =
        CModule(name: cmod.name, exports: [], attributes: [], defs: [def])
      core_printer.print_module(one)
      |> drop_lines(2)
      |> string.drop_end(string.length("\nend\n"))
    })
  header <> string.join(defs, "\n") <> "\nend\n"
}

fn drop_lines(s: String, n: Int) -> String {
  case n {
    0 -> s
    _ ->
      case string.split_once(s, "\n") {
        Ok(#(_, rest)) -> drop_lines(rest, n - 1)
        Error(Nil) -> ""
      }
  }
}

@external(erlang, "arc_aot_pp_ffi", "pmap")
fn pmap(items: List(a), f: fn(a) -> b) -> List(b)
