// run: cd aot && gleam run -m compile_time_probe [file.js]

import arc_aot/compile
import arc_aot/emit
import argv
import carder/backend/build_beam
import carder/backend/eaf
import carder/pipeline
import gleam/int
import gleam/io
import simplifile

type Unit {
  Millisecond
}

@external(erlang, "erlang", "monotonic_time")
fn now(unit: Unit) -> Int

@external(erlang, "erts_debug", "flat_size")
fn flat_size(term: a) -> Int

pub fn main() {
  let path = case argv.load().arguments {
    [p, ..] -> p
    [] -> "../bench/v8-v7/raytrace_run.js"
  }
  let assert Ok(src) = simplifile.read(path)
  let t0 = now(Millisecond)
  let assert Ok(m) = compile.to_ir(src, "ct_probe_unit")
  let t1 = now(Millisecond)
  let b = emit.binding()
  let assert Ok(lowered) = pipeline.lower_ir(m, b)
  let t2 = now(Millisecond)
  let opt = pipeline.optimize_ir(lowered, b)
  let t3 = now(Millisecond)
  let assert Ok(cmod) = pipeline.ir_to_cmod(opt, b)
  let t4 = now(Millisecond)
  let assert Ok(forms) = eaf.module_forms(cmod)
  let t5 = now(Millisecond)
  let assert Ok(_) = build_beam.compile_module(cmod)
  let t6 = now(Millisecond)
  io.println("ir_words " <> int.to_string(flat_size(m)))
  io.println("forms_words " <> int.to_string(flat_size(forms)))
  io.println("emit_ms " <> int.to_string(t1 - t0))
  io.println("lower_ms " <> int.to_string(t2 - t1))
  io.println("optimize_ms " <> int.to_string(t3 - t2))
  io.println("emit_core_ms " <> int.to_string(t4 - t3))
  io.println("forms_ms " <> int.to_string(t5 - t4))
  io.println("compile_forms_ms " <> int.to_string(t6 - t5))
  io.println("total_ms " <> int.to_string(t6 - t0))
}
