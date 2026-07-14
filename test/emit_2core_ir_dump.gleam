//// Throwaway IR-emission verifier for the emit_2core perf gap (verify-ir-
//// emission research unit). Prints the raw twocore IR AND the lowered Core
//// Erlang for each of the three bench kernels so per-iteration host-call
//// cost can be counted by eye. Run with:
////
////     cd arc && gleam run -m emit_2core_ir_dump 2>&1 | tee .local/ir_dump.txt
////
//// Not a test — delete once the perf work lands.

import arc/compiler/emit_2core
import emit_2core_bench.{adder_js, obj_js, sum_js}
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import simplifile
import twocore/ir
import twocore/ir/printer
import twocore/pipeline
import twocore/runtime/profiles

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
      io.println("─── Core Erlang (pipeline.ir_to_core, js_direct) ───")
      case pipeline.ir_to_core(unit.module, profiles.js_direct()) {
        Error(e) -> io.println("!! ir_to_core FAILED: " <> string.inspect(e))
        Ok(core) -> io.println(core)
      }
    }
  }
  io.println("")
}

/// ir.Expr node count — 1 per node, recursing into structured sub-exprs.
/// Mirrors twocore/middle/ir_opt/aggressive.gleam:node_count (private) plus
/// the Try variant so richards' exception-free bodies still count exactly.
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

/// richards-ir-attribution / r-share-audit (1): per-method ir.Expr node counts
/// for the compiled richards module. Verifies anf.share is collapsing cold
/// paths — each `.x`/`.method()` site should contribute O(30) nodes not O(200),
/// so no single jsf_N body should be many-thousands. Prints all functions
/// sorted descending by node count; the top rows are the Scheduler.schedule /
/// TaskControlBlock.run bodies (they have the most member sites).
fn dump_richards_node_counts() -> Nil {
  io.println("═══════════════════════════════════════════════════════════════")
  io.println("═══ richards — per-function ir.Expr node counts")
  io.println("═══════════════════════════════════════════════════════════════")
  let assert Ok(src) = simplifile.read("bench/v8-v7/richards_run.js")
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

/// perf8 crypto-am-path-trace: reduced am3 kernel — just the `w[j++]=l&mask`
/// write + `this_array[i++]` read. Verifies (a) `j++`/`i++` on a local var
/// go through LvIdent slot-var inc (NOT a Handle round-trip), (b) `w[·]=`
/// emits set_elem_fast_p, (c) `& 0xfffffff` const-int RHS routes through
/// int_const_bit → erl_band JPure inline, (d) `>>14`/`>>28`/`<<14` route
/// through int_const_shift → erl_bsr/erl_bsl. Grep the Core Erlang output
/// for `t_set_elem_fast_p`, `'band'`, `'bsr'`, `t_shr_fast` (residual).
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
