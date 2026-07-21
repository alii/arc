//// GC-CORRECTNESS probe (rdr verify-correctness). Proves the mark/sweep is
//// correct when it fires on emit_2core-compiled code.
////
//// (A) DIRECT — run a program that stores 10 survivors on `globalThis.keep`
//// and 100K garbage cells; take returned `st`, manually call
//// `rt_js_gc.t_collect(st, [])`; verify `stats.live` dropped ~100K and
//// `stats.free` grew; then run a SECOND compiled snippet in `st'` that
//// reads `globalThis.keep[5][0]` — must print `50000`, no dangling-handle
//// panic.
////
//// (B) IN-SITU — one compiled program: `.then#1` allocates 100K + stores
//// survivors on globalThis; `t_maybe_collect` fires between microtasks;
//// `.then#2` reads a survivor. Verify stdout matches AND no panic.
////
////     cd arc && gleam run -m emit_2core_gc_verify_x8k2

import arc/compiler/emit_2core
import emit_2core_harness as harness
import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import twocore/backend/build_beam
import twocore/pipeline
import twocore/runtime/profiles
import twocore/runtime/rt_js_builtins
import twocore/runtime/rt_js_gc.{type GcStats, GcStats}
import twocore/runtime/rt_js_store
import twocore/runtime/rt_js_types
import twocore/runtime/rt_state.{type InstanceState}

@external(erlang, "twocore_rt_js_exec_ffi", "apply_js_main")
fn ffi_apply_js_main(mod: Atom, st: InstanceState) -> #(Dynamic, InstanceState)

// ── programs ───────────────────────────────────────────────────────────────
// NOTE: compiler bugs to route around (measured in this probe run):
//   - `{x:i}` object literal → badarg element(6,0)
//   - `%` operator → error:undef t_modulo
//   - top-level `let` captured by `.then` closure → unbound Vjs_local_*
// Use `[i]` arrays as survivors; put helpers INSIDE the .then callbacks.

/// A: store 10 survivor arrays on globalThis.keep, then 100K garbage cells.
const prog_a_alloc = "
globalThis.keep = [[0],[10000],[20000],[30000],[40000],[50000],[60000],[70000],[80000],[90000]];
function m(x){return function(y){return x+y}}
let a=m(5);
let s=0;
for(let i=0;i<100000;i++) s+=a(i);
console.log('alloc-done ' + globalThis.keep.length);
"

/// A2: read survivor #5 from globalThis (runs in st' AFTER manual t_collect).
const prog_a_read = "console.log('read ' + globalThis.keep[5][0]);"

/// B: in-situ — `.then#1` allocates 100K + stores survivors (no outer
/// captures). `t_maybe_collect` fires between microtasks; `.then#2` reads.
const prog_b = "
Promise.resolve().then(function(){
  globalThis.keep = [[0],[10000],[20000],[30000],[40000],[50000],[60000],[70000],[80000],[90000]];
  function m(x){return function(y){return x+y}}
  let a=m(5);
  let s=0;
  for(let i=0;i<100000;i++) s+=a(i);
  console.log('then1 ' + globalThis.keep.length);
}).then(function(){
  console.log('then2 ' + globalThis.keep[5][0]);
});
console.log('sync');
"

/// C: in-situ survivor via ReactionJob arg (NOT via globalThis) — tests
/// `roots_of_state`'s `push_term_refs(microtasks)` traces the job's `arg`.
const prog_c = "
Promise.resolve().then(function(){
  function m(x){return function(y){return x+y}}
  let a=m(5);
  let s=0;
  for(let i=0;i<100000;i++) s+=a(i);
  return [42, 99];
}).then(function(v){
  console.log('captured ' + v[0] + ' ' + v[1]);
});
console.log('sync');
"

// ── helpers ────────────────────────────────────────────────────────────────

fn seed() -> InstanceState {
  let st =
    rt_state.fresh_full(
      rt_state.FullDecl(mems: [], globals: [], tables: [], ref_globals: []),
    )
  let st =
    rt_state.t_with_js_store(
      st,
      rt_js_store.t_store_new(harness.twocore_test_hooks()),
    )
  let #(_realm, st) = rt_js_builtins.init_realm(st)
  st
}

fn compile_load(source: String, name: String) -> Result(Atom, String) {
  let opts =
    emit_2core.CompileOpts(
      module_name: name,
      source_kind: emit_2core.AsScript,
      entry_name: "js_main",
    )
  case emit_2core.compile_source(source, opts) {
    Error(e) -> Error("emit: " <> string.inspect(e))
    Ok(unit) ->
      case pipeline.compile_ir(unit.module, profiles.js_direct()) {
        Error(e) -> Error("lower: " <> string.inspect(e))
        Ok(beam) ->
          case build_beam.load_module(atom.create(name), name, beam) {
            Error(e) -> Error("load: " <> e)
            Ok(m) -> Ok(m)
          }
      }
  }
}

fn stats_line(label: String, s: GcStats) -> String {
  let GcStats(live:, free:, next:, since_gc:) = s
  "  "
  <> label
  <> " next="
  <> int.to_string(next)
  <> " live="
  <> int.to_string(live)
  <> " free="
  <> int.to_string(free)
  <> " since_gc="
  <> int.to_string(since_gc)
}

fn stdout_str(st: InstanceState) -> String {
  case bit_array.to_string(rt_js_store.t_console_bytes(st)) {
    Ok(s) -> s
    Error(_) -> "<non-utf8>"
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

// ── experiment A: DIRECT t_collect ─────────────────────────────────────────

fn run_a() {
  io.println("═══ A: DIRECT t_collect + re-read survivor ═══")
  case compile_load(prog_a_alloc, "gcv_a_alloc") {
    Error(e) -> io.println("  ABORT: " <> e)
    Ok(m_alloc) -> {
      case compile_load(prog_a_read, "gcv_a_read") {
        Error(e) -> io.println("  ABORT (read): " <> e)
        Ok(m_read) -> {
          harness.buf_reset()
          let st0 = seed()
          let s0 = rt_js_gc.stats(st0)
          io.println(stats_line("seed:     ", s0))
          // 1. run alloc program
          let #(out1, st1) = ffi_apply_js_main(m_alloc, st0)
          let s1 = rt_js_gc.stats(st1)
          io.println(stats_line("post-run: ", s1))
          io.println(
            "  outcome  : " <> string.slice(string.inspect(out1), 0, 80),
          )
          io.println("  stdout   : " <> string.inspect(stdout_str(st1)))
          // 2. manual t_collect
          let st2 = rt_js_gc.t_collect(st1, [])
          let s2 = rt_js_gc.stats(st2)
          io.println(stats_line("post-gc:  ", s2))
          let dropped = s1.live - s2.live
          let freed = s2.free - s1.free
          io.println(
            "  dropped="
            <> int.to_string(dropped)
            <> " freed="
            <> int.to_string(freed),
          )
          // Assertions on stats:
          //   ~100K garbage should be swept; ~10 survivor arrays + realm remain.
          assert_in_range("live-after-gc", s2.live, s0.live, s0.live + 200)
          assert_in_range("dropped      ", dropped, 99_000, 101_000)
          assert_eq("since_gc reset", int.to_string(s2.since_gc), "0")
          // 3. run read program in st2 — survivor must still be reachable
          let #(out2, st3) = ffi_apply_js_main(m_read, st2)
          io.println(
            "  read outcome: " <> string.slice(string.inspect(out2), 0, 120),
          )
          let out_str = stdout_str(st3)
          io.println("  read stdout : " <> string.inspect(out_str))
          assert_eq(
            "survivor[5][0]",
            string.trim(string.replace(out_str, "alloc-done 10\n", "")),
            "read 50000",
          )
        }
      }
    }
  }
}

// ── experiment B: IN-SITU t_maybe_collect between microtasks ──────────────

fn run_b() {
  io.println("")
  io.println("═══ B: IN-SITU (t_maybe_collect between .then#1 and .then#2) ═══")
  case compile_load(prog_b, "gcv_b") {
    Error(e) -> io.println("  ABORT: " <> e)
    Ok(m) -> {
      harness.buf_reset()
      let st0 = seed()
      let s0 = rt_js_gc.stats(st0)
      let #(out, st1) = ffi_apply_js_main(m, st0)
      let s1 = rt_js_gc.stats(st1)
      io.println(stats_line("seed:     ", s0))
      io.println(stats_line("post-run: ", s1))
      io.println("  outcome  : " <> string.slice(string.inspect(out), 0, 200))
      let out_str = stdout_str(st1)
      io.println("  stdout   : " <> string.inspect(out_str))
      // GC fired between then1 and then2 iff since_gc < 100K:
      assert_in_range("since_gc-after (< threshold)", s1.since_gc, 0, 65_535)
      assert_in_range("free (swept ~100K)         ", s1.free, 99_000, 101_000)
      assert_eq("stdout", string.trim(out_str), "sync\nthen1 10\nthen2 50000")
    }
  }
}

// ── experiment C: microtask-queue root (closure capture) ──────────────────

fn run_c() {
  io.println("")
  io.println("═══ C: IN-SITU survivor via ReactionJob arg (not globalThis) ═══")
  case compile_load(prog_c, "gcv_c") {
    Error(e) -> io.println("  ABORT: " <> e)
    Ok(m) -> {
      harness.buf_reset()
      let st0 = seed()
      let #(out, st1) = ffi_apply_js_main(m, st0)
      let s1 = rt_js_gc.stats(st1)
      io.println(stats_line("post-run: ", s1))
      io.println("  outcome  : " <> string.slice(string.inspect(out), 0, 200))
      let out_str = stdout_str(st1)
      io.println("  stdout   : " <> string.inspect(out_str))
      assert_in_range("free (swept ~100K)", s1.free, 99_000, 101_000)
      assert_eq("stdout", string.trim(out_str), "sync\ncaptured 42 99")
    }
  }
}

// ── roots inspection ──────────────────────────────────────────────────────

fn inspect_roots() {
  io.println("")
  io.println("═══ roots_of_state includes global object? ═══")
  let st = seed()
  let realm = rt_state.t_realm(st)
  let global_id = case realm.global_object {
    rt_js_types.JsCell(id) -> id
  }
  let roots = rt_js_gc.roots_of_state(st)
  let n = list_length(roots)
  let has_global = list_contains(roots, global_id)
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

fn list_length(l: List(a)) -> Int {
  list.length(l)
}

fn list_contains(l: List(Int), x: Int) -> Bool {
  list.contains(l, x)
}

pub fn main() {
  io.println("emit_2core GC-CORRECTNESS probe")
  io.println("")
  inspect_roots()
  io.println("")
  run_a()
  run_b()
  run_c()
}
