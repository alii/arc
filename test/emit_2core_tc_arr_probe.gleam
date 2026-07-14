//// TC_ARR_MIN_LEN threshold sweep — compile db+crypto ONCE, then for each
//// N ∈ {0,8,16,32,64,999999} rewrite the -define, hot-reload the FFI beam,
//// and re-measure. Single-VM so compile_ir cost is paid once.

import arc/compiler/emit_2core
import emit_2core_harness as harness
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/int
import gleam/io
import gleam/list
import simplifile
import twocore/backend/build_beam
import twocore/pipeline
import twocore/runtime/rt_js_builtins
import twocore/runtime/rt_js_store
import twocore/runtime/rt_state.{type InstanceState}

type TimeUnit {
  Microsecond
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(unit: TimeUnit) -> Int

type JsExecOutcome {
  JsReturned(value: Dynamic)
  JsThrew(exn: Dynamic)
  JsCrashed(reason: String)
}

@external(erlang, "twocore_rt_js_exec_ffi", "apply_js_main")
fn ffi_apply_js_main(
  mod: Atom,
  st: InstanceState,
) -> #(JsExecOutcome, InstanceState)

@external(erlang, "os", "cmd")
fn os_cmd(cmd: charlist.Charlist) -> charlist.Charlist

@external(erlang, "code", "purge")
fn code_purge(mod: Atom) -> Bool

@external(erlang, "code", "load_file")
fn code_load_file(mod: Atom) -> Dynamic

import gleam/erlang/charlist

fn seed_realm() -> InstanceState {
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

fn repeat(n: Int, f: fn() -> a) -> Nil {
  case n {
    0 -> Nil
    _ -> {
      f()
      repeat(n - 1, f)
    }
  }
}

type Loaded {
  Loaded(mod: Atom, seed: InstanceState)
}

fn compile_once(name: String) -> Loaded {
  let path = "bench/v8-v7/" <> name <> "_run.js"
  let assert Ok(source) = simplifile.read(path)
  let mod_name = "arc_tcarr_" <> name
  let opts =
    emit_2core.CompileOpts(
      module_name: mod_name,
      source_kind: emit_2core.AsScript,
      entry_name: "js_main",
    )
  let assert Ok(unit) = emit_2core.compile_source(source, opts)
  let assert Ok(beam) = pipeline.compile_ir(unit.module, emit_2core.binding())
  let assert Ok(mod) =
    build_beam.load_module(atom.create(mod_name), mod_name, beam)
  Loaded(mod:, seed: seed_realm())
}

fn measure(loaded: Loaded, reps: Int) -> Int {
  repeat(2, fn() { ffi_apply_js_main(loaded.mod, loaded.seed) })
  let t0 = monotonic_time(Microsecond)
  repeat(reps, fn() { ffi_apply_js_main(loaded.mod, loaded.seed) })
  { monotonic_time(Microsecond) - t0 } / reps
}

const ffi_src = "../2core/src/twocore_rt_js_obj_ffi.erl"

const ffi_ebin = "build/dev/erlang/twocore/ebin"

fn set_threshold(n: Int) -> Nil {
  let ns = int.to_string(n)
  let sed =
    "perl -pi -e 's/^-define\\(TC_ARR_MIN_LEN, \\d+\\)\\./"
    <> "-define(TC_ARR_MIN_LEN, " <> ns <> ")./' " <> ffi_src
  os_cmd(charlist.from_string(sed))
  let erlc = "erlc -o " <> ffi_ebin <> " " <> ffi_src <> " 2>&1"
  let out = charlist.to_string(os_cmd(charlist.from_string(erlc)))
  case out {
    "" -> Nil
    _ -> io.println("!! erlc: " <> out)
  }
  let ffi_mod = atom.create("twocore_rt_js_obj_ffi")
  code_purge(ffi_mod)
  code_load_file(ffi_mod)
  Nil
}

pub fn main() {
  io.println("compiling deltablue+crypto once …")
  let db = compile_once("deltablue")
  let cr = compile_once("crypto")
  io.println("… compiled. sweeping TC_ARR_MIN_LEN:")
  list.each([0, 8, 16, 32, 64, 999_999], fn(n) {
    set_threshold(n)
    let db_us = measure(db, 30)
    let cr_us = measure(cr, 4)
    io.println(
      "SWEEP N=" <> int.to_string(n) <> "\tdeltablue=" <> int.to_string(db_us)
      <> "\tcrypto=" <> int.to_string(cr_us),
    )
  })
  set_threshold(8)
  io.println("restored TC_ARR_MIN_LEN=8")
}
