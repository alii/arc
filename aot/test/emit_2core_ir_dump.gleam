// throwaway perf dump, not a test

import arc_aot/emit as emit_2core
import carder/ir
import carder/ir/printer
import carder/pipeline
import emit_2core_bench.{adder_js, obj_js, sum_js}
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import simplifile

fn dump(name: String, source: String) -> Nil {
  io.println("═══════════════════════════════════════════════════════════════")
  io.println("═══ " <> name)
  io.println("═══ src: " <> source)
  io.println("═══════════════════════════════════════════════════════════════")
  let opts =
    emit_2core.CompileOpts(
      module_name: "irdump_" <> name,
      source_kind: emit_2core.AsScript,
      entry_name: "js_main",
    )
  case emit_2core.compile_source(source, opts) {
    Error(e) -> io.println("!! compile_source FAILED: " <> string.inspect(e))
    Ok(unit) -> {
      io.println("─── twocore IR (printer.print_module) ───")
      io.println(printer.print_module(unit.module))
      io.println("─── Core Erlang (pipeline.ir_to_core, emit binding) ───")
      case pipeline.ir_to_core(unit.module, emit_2core.binding()) {
        Error(e) -> io.println("!! ir_to_core FAILED: " <> string.inspect(e))
        Ok(core) -> io.println(core)
      }
    }
  }
  io.println("")
}

fn node_count(e: ir.Expr) -> Int {
  case e {
    ir.Let(_, rhs, body) -> 1 + node_count(rhs) + node_count(body)
    ir.Block(_, _, body) -> 1 + node_count(body)
    ir.Loop(_, _, _, body) -> 1 + node_count(body)
    ir.If(_, _, t, f) -> 1 + node_count(t) + node_count(f)
    ir.Switch(_, _, arms, default) ->
      list.fold(arms, 1 + node_count(default), fn(acc, a) {
        acc + node_count(a.body)
      })
    ir.Charge(_, body) -> 1 + node_count(body)
    ir.Try(_, body, handlers) ->
      list.fold(handlers, 1 + node_count(body), fn(acc, h) {
        acc + node_count(h.handler)
      })
    _ -> 1
  }
}

fn pad(s: String, w: Int) -> String {
  s <> string.repeat(" ", int.max(0, w - string.length(s)))
}

fn dump_richards_node_counts() -> Nil {
  io.println("═══════════════════════════════════════════════════════════════")
  io.println("═══ richards — per-function ir.Expr node counts")
  io.println("═══════════════════════════════════════════════════════════════")
  let assert Ok(src) = simplifile.read("../bench/v8-v7/richards_run.js")
  let opts =
    emit_2core.CompileOpts(
      module_name: "irdump_richards",
      source_kind: emit_2core.AsScript,
      entry_name: "js_main",
    )
  case emit_2core.compile_source(src, opts) {
    Error(e) -> io.println("!! compile_source FAILED: " <> string.inspect(e))
    Ok(unit) -> {
      let rows =
        unit.module.functions
        |> list.map(fn(f) { #(f.name, node_count(f.body)) })
        |> list.sort(fn(a, b) { int.compare(b.1, a.1) })
      let total = list.fold(rows, 0, fn(acc, r) { acc + r.1 })
      io.println(pad("fn_name", 20) <> "  ir.Expr nodes")
      io.println(pad("───────", 20) <> "  ─────────────")
      list.each(rows, fn(r) {
        io.println(pad(r.0, 20) <> "  " <> int.to_string(r.1))
      })
      io.println(pad("───────", 20) <> "  ─────────────")
      io.println(
        pad("Σ (" <> int.to_string(list.length(rows)) <> " fns)", 20)
        <> "  "
        <> int.to_string(total),
      )
    }
  }
  io.println("")
}

pub const crypto_am3_write_js = "
function am3(i,x,w,j,c,n) {
  var this_array = this.array;
  var w_array    = w.array;
  var xl = x&0x3fff, xh = x>>14;
  while(--n >= 0) {
    var l = this_array[i]&0x3fff;
    var h = this_array[i++]>>14;
    var m = xh*l+h*xl;
    l = xl*l+((m&0x3fff)<<14)+w_array[j]+c;
    c = (l>>28)+(m>>14)+xh*h;
    w_array[j++] = l&0xfffffff;
  }
  return c;
}
am3;"

pub fn main() {
  dump("sum", sum_js)
  dump("adder", adder_js)
  dump("obj", obj_js)
  dump("crypto_am3_write", crypto_am3_write_js)
  dump_richards_node_counts()
}
