// run: cd aot && gleam run -m aot_gc_verify

import aot_harness
import arc/rt/gc as rt_gc
import arc/rt/types.{type Agent}
import arc_aot/emit
import arc_aot/run
import carder/pipeline
import gleam/bit_array
import gleam/erlang/atom.{type Atom}
import gleam/int
import gleam/io
import gleam/list
import gleam/string

const prog_a_alloc = "
globalThis.keep = [];
function m(x){return function(y){return x+y}}
let a=m(5);
let s=0;
for(let i=0;i<100000;i++){
  s+=a(i);
  if(i%10000==0) globalThis.keep.push([i]);
}
console.log('alloc-done ' + globalThis.keep.length);
"

const prog_a_read = "console.log('read ' + globalThis.keep[5][0]);"

const prog_b = "
globalThis.keep = [];
function m(x){return function(y){return x+y}}
let a=m(5);
Promise.resolve().then(function(){
  let s=0;
  for(let i=0;i<100000;i++){
    s+=a(i);
    if(i%10000==0) globalThis.keep.push([i]);
  }
  console.log('then1 ' + globalThis.keep.length);
}).then(function(){
  console.log('then2 ' + globalThis.keep[5][0]);
});
console.log('sync');
"

const prog_c = "
function m(x){return function(y){return x+y}}
let a=m(5);
Promise.resolve().then(function(){
  let s=0;
  for(let i=0;i<100000;i++) s+=a(i);
  return [42, 99];
}).then(function(v){
  console.log('captured ' + v[0] + ' ' + v[1]);
});
console.log('sync');
"

fn seed() -> Agent {
  aot_harness.seed()
}

fn compile_load(source: String, name: String) -> Result(Atom, String) {
  let opts = emit.CompileOpts(module_name: name, source_kind: emit.AsScript)
  case emit.compile_source(source, opts) {
    Error(e) -> Error("emit: " <> string.inspect(e))
    Ok(ir_module) ->
      case pipeline.compile_ir(ir_module, emit.binding()) {
        Error(e) -> Error("lower: " <> string.inspect(e))
        Ok(beam) ->
          case run.load(beam, name) {
            Error(e) -> Error("load: " <> e)
            Ok(m) -> Ok(m)
          }
      }
  }
}

fn stats_line(label: String, s: rt_gc.GcStats) -> String {
  let rt_gc.GcStats(live_count:, next_id:, alloc_since_gc:) = s
  "  "
  <> label
  <> " next="
  <> int.to_string(next_id)
  <> " live="
  <> int.to_string(live_count)
  <> " since_gc="
  <> int.to_string(alloc_since_gc)
}

fn swept(before: rt_gc.GcStats, after: rt_gc.GcStats) -> Int {
  { after.next_id - before.next_id } - { after.live_count - before.live_count }
}

fn stdout_text(_st: Agent) -> String {
  case bit_array.to_string(aot_harness.buf_read()) {
    Ok(s) -> s
    Error(Nil) -> "<non-utf8>"
  }
}

fn assert_eq(label: String, got: String, want: String) {
  case got == want {
    True -> io.println("  ✓ " <> label <> " = " <> string.inspect(got))
    False ->
      io.println(
        "  ✗ FAIL "
        <> label
        <> "\n      got:  "
        <> string.inspect(got)
        <> "\n      want: "
        <> string.inspect(want),
      )
  }
}

fn assert_in_range(label: String, got: Int, lo: Int, hi: Int) {
  case got >= lo && got <= hi {
    True ->
      io.println(
        "  ✓ "
        <> label
        <> " = "
        <> int.to_string(got)
        <> " ∈ ["
        <> int.to_string(lo)
        <> ","
        <> int.to_string(hi)
        <> "]",
      )
    False ->
      io.println(
        "  ✗ FAIL "
        <> label
        <> " = "
        <> int.to_string(got)
        <> " ∉ ["
        <> int.to_string(lo)
        <> ","
        <> int.to_string(hi)
        <> "]",
      )
  }
}

fn run_a() {
  io.println("═══ A: DIRECT t_collect + re-read survivor ═══")
  case compile_load(prog_a_alloc, "gcv_a_alloc") {
    Error(e) -> io.println("  ABORT: " <> e)
    Ok(m_alloc) -> {
      case compile_load(prog_a_read, "gcv_a_read") {
        Error(e) -> io.println("  ABORT (read): " <> e)
        Ok(m_read) -> {
          aot_harness.buf_reset()
          let st0 = seed()
          let s0 = rt_gc.stats(st0)
          io.println(stats_line("seed:     ", s0))
          let #(out1, st1) = run.apply_js_main(st0, m_alloc)
          let s1 = rt_gc.stats(st1)
          io.println(stats_line("post-run: ", s1))
          io.println(
            "  outcome  : " <> string.slice(string.inspect(out1), 0, 80),
          )
          io.println("  stdout   : " <> string.inspect(stdout_text(st1)))
          let st2 = rt_gc.t_collect(st1, [])
          let s2 = rt_gc.stats(st2)
          io.println(stats_line("post-gc:  ", s2))
          let dropped = s1.live_count - s2.live_count
          io.println("  dropped=" <> int.to_string(dropped))
          assert_in_range(
            "live-after-gc",
            s2.live_count,
            s0.live_count,
            s0.live_count + 200,
          )
          assert_in_range("dropped      ", dropped, 99_000, 101_000)
          assert_eq("since_gc reset", int.to_string(s2.alloc_since_gc), "0")
          let #(out2, st3) = run.apply_js_main(st2, m_read)
          io.println(
            "  read outcome: " <> string.slice(string.inspect(out2), 0, 120),
          )
          let out_text = stdout_text(st3)
          io.println("  read stdout : " <> string.inspect(out_text))
          assert_eq(
            "survivor[5][0]",
            string.trim(string.replace(out_text, "alloc-done 10\n", "")),
            "read 50000",
          )
        }
      }
    }
  }
}

fn run_b() {
  io.println("")
  io.println("═══ B: IN-SITU (t_maybe_collect between .then#1 and .then#2) ═══")
  case compile_load(prog_b, "gcv_b") {
    Error(e) -> io.println("  ABORT: " <> e)
    Ok(m) -> {
      aot_harness.buf_reset()
      let st0 = seed()
      let s0 = rt_gc.stats(st0)
      let #(out, st1) = run.apply_js_main(st0, m)
      let s1 = rt_gc.stats(st1)
      io.println(stats_line("seed:     ", s0))
      io.println(stats_line("post-run: ", s1))
      io.println("  outcome  : " <> string.slice(string.inspect(out), 0, 200))
      let out_text = stdout_text(st1)
      io.println("  stdout   : " <> string.inspect(out_text))
      assert_in_range(
        "since_gc-after (< threshold)",
        s1.alloc_since_gc,
        0,
        65_535,
      )
      assert_in_range(
        "swept (~100K)              ",
        swept(s0, s1),
        99_000,
        101_000,
      )
      assert_eq("stdout", string.trim(out_text), "sync\nthen1 10\nthen2 50000")
    }
  }
}

fn run_c() {
  io.println("")
  io.println("═══ C: IN-SITU survivor via ReactionJob arg (not globalThis) ═══")
  case compile_load(prog_c, "gcv_c") {
    Error(e) -> io.println("  ABORT: " <> e)
    Ok(m) -> {
      aot_harness.buf_reset()
      let st0 = seed()
      let s0 = rt_gc.stats(st0)
      let #(out, st1) = run.apply_js_main(st0, m)
      let s1 = rt_gc.stats(st1)
      io.println(stats_line("post-run: ", s1))
      io.println("  outcome  : " <> string.slice(string.inspect(out), 0, 200))
      let out_text = stdout_text(st1)
      io.println("  stdout   : " <> string.inspect(out_text))
      assert_in_range("swept (~100K)", swept(s0, s1), 99_000, 101_000)
      assert_eq("stdout", string.trim(out_text), "sync\ncaptured 42 99")
    }
  }
}

fn inspect_roots() {
  io.println("")
  io.println("═══ roots_of_state includes global object? ═══")
  let st = seed()
  let types.Handle(global_id) = st.realm.global_object
  let roots = rt_gc.roots_of_state(st)
  let n = list.length(roots)
  let has_global = list.contains(roots, global_id)
  io.println(
    "  roots count = "
    <> int.to_string(n)
    <> "  global_object.id = "
    <> int.to_string(global_id)
    <> "  in roots? "
    <> case has_global {
      True -> "YES"
      False -> "NO — BUG"
    },
  )
}

pub fn main() {
  io.println("aot GC-CORRECTNESS probe")
  io.println("")
  inspect_roots()
  io.println("")
  run_a()
  run_b()
  run_c()
}
