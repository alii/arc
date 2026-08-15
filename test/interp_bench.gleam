//// Per-section timing of `bench/bench.js` (or any script printing
//// `BENCH <name> <ms>` lines) through the old interpreter and the new one in
//// the same BEAM process, alternating runs so both see the same machine load.
////
////   gleam run -m interp_bench                      # bench/bench.js, 1 run each
////   gleam run -m interp_bench -- bench/bench.js 5   # min of 5
////
//// The script is compiled once and both interpreters run the same template
//// (the old one converts it on entry). `console.log` is teed into an array
//// whose join is the script's completion value, so the numbers come back as
//// data without any capture state outside the two heaps.

import arc/compiler
import arc/host_hooks
import arc/interp/entry as new_entry
import arc/parser
import arc/rt/builtins as rt_builtins
import arc/rt/bytecode.{type FuncTemplate}
import arc/rt/call.{NormalCompletion, ThrowCompletion} as _
import arc/rt/inspect as rt_inspect
import arc/rt/types.{KStr, classify}
import arc/vm/builtins as old_builtins
import arc/vm/exec/entry as old_entry
import arc/vm/exec/event_loop
import arc/vm/heap
import arc/vm/ops/object as old_object
import arc/vm/state as old_state
import arc/vm/value
import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import simplifile

@external(erlang, "arc_cli_ffi", "get_script_args")
fn argv() -> List(String)

const prelude = "const __bench_lines = [];
{
  const log = console.log;
  console.log = function (line) {
    __bench_lines.push(String(line));
    return log.apply(console, arguments);
  };
}
"

const epilogue = "
;__bench_lines.join('\\n')
"

pub fn main() -> Nil {
  let #(path, runs) = case argv() {
    [] -> #("bench/bench.js", 1)
    [p] -> #(p, 1)
    [p, n, ..] -> #(p, int.parse(n) |> result.unwrap(1) |> int.max(1))
  }
  let source = case simplifile.read(path) {
    Ok(s) -> prelude <> s <> epilogue
    Error(e) ->
      panic as {
        "cannot read " <> path <> ": " <> simplifile.describe_error(e)
      }
  }
  let template = case parser.parse_script(source) {
    Error(e) -> panic as { "parse error: " <> parser.parse_error_to_string(e) }
    Ok(#(body, sb)) ->
      case compiler.compile(body, sb) {
        Ok(t) -> t
        Error(e) -> panic as { "compile error: " <> compiler.error_message(e) }
      }
  }
  let #(old_runs, new_runs) =
    int.range(from: 1, to: runs + 1, with: #([], []), run: fn(acc, i) {
      let #(olds, news) = acc
      io.println("# old interpreter, run " <> int.to_string(i))
      let old = run_old(template)
      io.println("# new interpreter, run " <> int.to_string(i))
      let new = run_new(template)
      #([old, ..olds], [new, ..news])
    })
  report(sections(old_runs), min_by_section(old_runs), min_by_section(new_runs))
}

/// One run through the old interpreter: its `BENCH` lines as (name, ms).
fn run_old(template: FuncTemplate) -> List(#(String, Int)) {
  let #(h, b) = old_builtins.init(heap.new())
  let #(h, global) = old_builtins.globals(b, h)
  let hooks = host_hooks.default_host_hooks()
  case
    old_entry.run_with_hooks(
      template,
      h,
      b,
      global,
      hooks,
      event_loop.drain_jobs,
    )
  {
    Error(e) -> panic as { "old vm: " <> old_state.vm_error_message(e) }
    Ok(#(Error(thrown), h)) ->
      panic as { "old vm threw: " <> old_object.inspect(thrown, h) }
    Ok(#(Ok(value.JsString(lines)), _)) -> parse_lines(lines)
    Ok(#(Ok(other), h)) ->
      panic as { "old vm completed with " <> old_object.inspect(other, h) }
  }
}

/// One run through the new interpreter: its `BENCH` lines as (name, ms).
fn run_new(template: FuncTemplate) -> List(#(String, Int)) {
  let agent =
    rt_builtins.new_agent(host_hooks.default_host_hooks()) |> new_entry.link
  case new_entry.run_script(agent, template) {
    #(ThrowCompletion(e), st) ->
      panic as { "new vm threw: " <> rt_inspect.inspect(st, e) }
    #(NormalCompletion(v), st) ->
      case classify(v) {
        KStr(lines) -> parse_lines(lines)
        _ -> panic as { "new vm completed with " <> rt_inspect.inspect(st, v) }
      }
  }
}

fn parse_lines(lines: String) -> List(#(String, Int)) {
  string.split(lines, "\n")
  |> list.filter_map(fn(line) {
    case string.split(line, " ") {
      ["BENCH", name, ms] -> int.parse(ms) |> result.map(fn(ms) { #(name, ms) })
      _ -> Error(Nil)
    }
  })
}

/// Section names in the order the first run printed them.
fn sections(runs: List(List(#(String, Int)))) -> List(String) {
  list.last(runs)
  |> result.unwrap([])
  |> list.map(fn(pair) { pair.0 })
}

fn min_by_section(runs: List(List(#(String, Int)))) -> Dict(String, Int) {
  list.flatten(runs)
  |> list.fold(dict.new(), fn(acc, pair) {
    let #(name, ms) = pair
    dict.upsert(acc, name, fn(prev) {
      case prev {
        option.Some(p) -> int.min(p, ms)
        option.None -> ms
      }
    })
  })
}

fn report(names: List(String), old: Dict(String, Int), new: Dict(String, Int)) {
  io.println("")
  io.println(row("section", "old_ms", "new_ms", "new/old"))
  list.each(names, fn(name) {
    let o = dict.get(old, name) |> result.unwrap(0)
    let n = dict.get(new, name) |> result.unwrap(0)
    io.println(row(name, int.to_string(o), int.to_string(n), ratio(o, n)))
  })
}

fn ratio(old: Int, new: Int) -> String {
  case old {
    0 -> "-"
    _ -> {
      let hundredths = { new * 100 + old / 2 } / old
      let text =
        int.to_string(hundredths / 100)
        <> "."
        <> string.pad_start(int.to_string(hundredths % 100), 2, "0")
        <> "x"
      case hundredths > 200 {
        True -> text <> "  >2x"
        False -> text
      }
    }
  }
}

fn row(a: String, b: String, c: String, d: String) -> String {
  string.pad_end(a, 18, " ")
  <> string.pad_start(b, 8, " ")
  <> string.pad_start(c, 8, " ")
  <> "  "
  <> d
}
