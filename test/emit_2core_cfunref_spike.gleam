//// perf5 CFunRef re-enable spike — narrows the OTP-29 beam_ssa_opt crash
//// that keeps `emit_core.perf5_cfunref_zero_capture` disabled.
////
//// BACKGROUND (emit_2core_profile.gleam:688-734): richards runs ~40,650
//// `-js_main/3-anonymous-N-` applies/run — every one is an eta-wrapper
//// `fun(St,A..) -> apply 'jsf_K'/N(St,A..)` around a zero-capture prototype
//// method. Emitting a bare `'jsf_K'/N` fun-ref instead (one dispatch, no
//// wrapper body) is worth ~160µs on richards and unblocks the OTP `inline`
//// pass (twocore_codegen_ffi.erl:51-64), but flipping the gate crashes
//// OTP-29's `compile:forms` inside `beam_ssa_opt` (`ssa_opt_type_start`
//// → `{badmatch,…}` on the fun-type lattice).
////
//// WHAT THIS SPIKE DOES:
////   1. Builds a MINIMAL hand-written `.core` module that mimics the shape
////      emit_core produces (many local `'f'/N` values bound in one big
////      function, stored in a tuple, later applied) and compiles it with
////      the default option set — reproduces the crash without arc's JS
////      pipeline in the loop.
////   2. Re-compiles the same `.core` under an option matrix
////      (`no_type_opt`, `no_ssa_opt`, `no_copt`, `no_bool_opt`,
////      `no_ssa_opt_type_start`¹) so the crash is attributed to a single
////      pass and any surviving option combo is a candidate workaround.
////   3. If `emit_core.perf5_cfunref_zero_capture` is flipped to `True`,
////      also compiles the REAL richards `.core` through the same matrix
////      so the minimal repro's verdict is confirmed on the actual module.
////
//// USAGE:
////     cd arc && gleam run -m emit_2core_cfunref_spike
////   then, to test the real path:
////     flip perf5_cfunref_zero_capture=True in 2core/…/emit_core.gleam,
////     `gleam build`, re-run.
////
//// FINDINGS (OTP 29, 2026-07):
////   • The minimal repros (≤96 fun-refs, flat AND letrec-spine, φ-merged
////     pairs, all escaping via erlang:put) COMPILE under every option set.
////     ⇒ the crash trigger is NOT "many `'f'/N` values in one function" on
////     its own — richards' js_main adds something the repro lacks (deep
////     letrec nesting × threaded-state {V,St'} unpack × the same fun-ref
////     value flowing to `apply` at MULTIPLE arities via CallClosure).
////   • `no_ssa_opt_type_start` / `_continue` are NOT viable partial skips:
////     OTP-29 accepts the atom but skipping only START leaves CONTINUE/
////     FINISH with an empty arg-type list → beam_types:join([]) function_
////     clause (a probe-induced crash, not the CFunRef one). The whole
////     type-opt phase is `no_type_opt`; the whole SSA-opt module is
////     `no_ssa_opt`.
////   • NEXT: with the gate flipped and expr.gleam stable, the richards row
////     is the live crash — its `default` verdict names the pass; whichever
////     of `no_type_opt` / `no_ssa_opt` flips it to `compiled` is the
////     candidate workaround for twocore_codegen_ffi.erl (guarded on the
////     .core containing `= 'jsf_…'/N in`, so non-CFunRef modules keep the
////     full optimizer).

import arc/compiler/emit_2core
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import simplifile
import twocore/backend/emit_core as ec
import twocore/pipeline
import twocore/runtime/profiles

// ── FFI ─────────────────────────────────────────────────────────────────────

pub type CompileVerdict {
  Compiled(bytes: Int)
  Rejected(msgs: List(String))
  Crashed(class: String, head: String)
}

/// Atoms accepted by OTP `compile:forms/2`. Gleam custom-type variants cross
/// the FFI as lower-snake atoms, so `NoTypeOpt` → `no_type_opt` verbatim.
pub type CompileOpt {
  NoTypeOpt
  NoSsaOpt
  NoCopt
  NoBoolOpt
  NoModuleOpt
  Inline
}

@external(erlang, "emit_2core_cfunref_spike_ffi", "compile_core_opts")
fn compile_core_opts(core: BitArray, opts: List(CompileOpt)) -> CompileVerdict

@external(erlang, "emit_2core_cfunref_spike_ffi", "funref_count")
fn funref_count(core: BitArray) -> Int

@external(erlang, "emit_2core_cfunref_spike_ffi", "otp_release")
fn otp_release() -> String

// ── minimal repro ───────────────────────────────────────────────────────────

fn range(from: Int, to: Int) -> List(Int) {
  case from > to {
    True -> []
    False -> [from, ..range(from + 1, to)]
  }
}

/// Hand-written `.core` mimicking emit_core's zero-capture-CFunRef output:
/// `n` module-level `'f_I'/2` functions, and one `big/1` that binds every
/// `'f_I'/2` as a VALUE, hands each to `erlang:put` (so DCE can't drop
/// it — mirrors js_main passing method funs to t_new_function), then
/// applies one read back via `erlang:get`. Each `f_I` also case-merges
/// two DIFFERENT `'f_…'/2` refs into one binding — the "many distinct
/// fun-type values meet at one φ" shape the disabled-gate comment blames
/// on the SSA type lattice. `letrec_wrap` puts `big`'s body inside a
/// letrec-apply spine (js_main is ~1.4k nested letrecs; the original
/// crash note says "inside a large js_main").
fn minimal_core(n: Int, letrec_wrap: Bool) -> String {
  let idx = range(0, n - 1)
  let defs =
    idx
    |> list.map(fn(i) {
      let j = int.to_string({ i + 1 } % n)
      "'f_" <> int.to_string(i) <> "'/2 = fun (_s, _x) -> "
      <> "let <_m> = case call 'erlang':'>'(_x, 0) of "
      <> "<'true'> when 'true' -> 'f_" <> j <> "'/2 "
      <> "<'false'> when 'true' -> 'f_" <> int.to_string(i) <> "'/2 end in "
      <> "{_m, call 'erlang':'+'(_x, " <> int.to_string(i) <> ")}"
    })
    |> string.join("\n")
  let one_put = fn(i) {
    "let <_v" <> int.to_string(i) <> "> = 'f_" <> int.to_string(i)
    <> "'/2 in let <_p" <> int.to_string(i)
    <> "> = call 'erlang':'put'(" <> int.to_string(i) <> ", _v"
    <> int.to_string(i) <> ") in "
  }
  let body_tail =
    "let <_g> = call 'erlang':'get'(0) in apply _g(_st, 0)"
  let body = case letrec_wrap {
    False ->
      { idx |> list.map(one_put) |> string.join("\n    ") } <> body_tail
    True ->
      // Nest each put in its own `letrec 'k_I'/0 = fun() -> … in apply
      // 'k_I'/0()` spine so beam_ssa sees the fun-refs across many local
      // functions in one top-level body (the js_main shape).
      list.fold_right(idx, body_tail, fn(acc, i) {
        "letrec 'k_" <> int.to_string(i) <> "'/0 = fun () -> "
        <> one_put(i) <> acc <> " in apply 'k_" <> int.to_string(i) <> "'/0()"
      })
  }
  "module 'cfunref_spike_min' ['big'/1, 'module_info'/0, 'module_info'/1]\n"
  <> "    attributes []\n"
  <> defs <> "\n"
  <> "'big'/1 = fun (_st) ->\n    " <> body <> "\n"
  <> "'module_info'/0 = fun () -> "
  <> "call 'erlang':'get_module_info'('cfunref_spike_min')\n"
  <> "'module_info'/1 = fun (_k) -> "
  <> "call 'erlang':'get_module_info'('cfunref_spike_min', _k)\n"
  <> "end\n"
}

// ── option matrix ───────────────────────────────────────────────────────────

fn matrix() -> List(#(String, List(CompileOpt))) {
  // NOTE: `no_ssa_opt_type_start` / `_continue` are NOT probed — OTP-29
  // accepts them but skipping only the START sub-pass leaves CONTINUE/
  // FINISH with an empty arg-type list → beam_types:join([]) function_
  // clause. That's a probe-induced crash, not the CFunRef crash. The
  // whole type-opt phase is `no_type_opt`; the whole SSA-opt module is
  // `no_ssa_opt`.
  [
    #("default", []),
    #("no_type_opt", [NoTypeOpt]),
    #("no_ssa_opt", [NoSsaOpt]),
    #("no_copt", [NoCopt]),
    #("no_bool_opt", [NoBoolOpt]),
    #("no_module_opt", [NoModuleOpt]),
    #("no_type_opt+no_ssa_opt", [NoTypeOpt, NoSsaOpt]),
    #("no_type_opt+inline", [NoTypeOpt, Inline]),
  ]
}

fn verdict_str(v: CompileVerdict) -> String {
  case v {
    Compiled(b) -> "compiled  (" <> int.to_string(b) <> " B)"
    Rejected(ms) ->
      "REJECTED  " <> { list.first(ms) |> unwrap_or("(no msg)") }
    Crashed(_, head) -> "CRASHED   " <> string.slice(head, 0, 120)
  }
}

fn unwrap_or(r: Result(a, b), d: a) -> a {
  case r {
    Ok(x) -> x
    Error(_) -> d
  }
}

fn probe(label: String, core: BitArray) -> Nil {
  io.println("── " <> label <> " ──  (funref values in .core: "
    <> int.to_string(funref_count(core)) <> ")")
  matrix()
  |> list.each(fn(row) {
    let #(name, opts) = row
    let v = compile_core_opts(core, opts)
    io.println("  " <> string.pad_end(name, 36, " ") <> verdict_str(v))
  })
  io.println("")
}

// ── real richards path (only meaningful when the gate is flipped) ───────────

fn richards_core() -> Result(String, String) {
  case simplifile.read("bench/v8-v7/richards_run.js") {
    Error(e) -> Error("read richards_run.js: " <> string.inspect(e))
    Ok(src) -> {
      let opts =
        emit_2core.CompileOpts(
          module_name: "arc_cfunref_spike",
          source_kind: emit_2core.AsScript,
          entry_name: "js_main",
        )
      case emit_2core.compile_source(src, opts) {
        Error(e) -> Error("compile_source: " <> string.inspect(e))
        Ok(unit) ->
          case pipeline.ir_to_core(unit.module, profiles.js_direct()) {
            Error(e) -> Error("ir_to_core: " <> string.inspect(e))
            Ok(core) -> Ok(core)
          }
      }
    }
  }
}

pub fn main() {
  io.println("═══ emit_2core CFunRef spike — OTP " <> otp_release() <> " ═══")
  io.println("gate emit_core.perf5_cfunref_zero_capture = "
    <> case ec.perf5_cfunref_zero_capture {
      True -> "True  (CFunRef ENABLED — real-path rows below are live)"
      False -> "False (CFunRef disabled — real-path rows are the eta baseline)"
    })
  io.println("")

  // 1. minimal repro at three sizes × two shapes — narrow whether the
  //    crash is size-gated (lattice-width blowup) or shape-gated
  //    (needs the fun-refs inside letrec bodies, like js_main).
  probe("minimal n=4  flat", <<minimal_core(4, False):utf8>>)
  probe("minimal n=32 flat", <<minimal_core(32, False):utf8>>)
  probe("minimal n=96 flat", <<minimal_core(96, False):utf8>>)
  probe("minimal n=32 letrec-spine", <<minimal_core(32, True):utf8>>)
  probe("minimal n=96 letrec-spine", <<minimal_core(96, True):utf8>>)

  // 2. real richards .core through the same matrix.
  case richards_core() {
    Error(e) -> io.println("!! richards .core unavailable: " <> e)
    Ok(core) -> probe("richards_run.js → .core", <<core:utf8>>)
  }

  io.println(
    "READING THE TABLE:\n"
    <> "  • richards `default` = CRASHED/REJECTED naming beam_ssa_opt →\n"
    <> "    the perf5 crash is reproduced; the row that flips to `compiled`\n"
    <> "    is the workaround to trial in twocore_codegen_ffi.erl.\n"
    <> "  • richards `default` = REJECTED with `unbound variable` →\n"
    <> "    unrelated emit_2core codegen bug upstream (not this spike).\n"
    <> "  • all minimal rows `compiled` → the pattern-in-isolation is fine;\n"
    <> "    the crash needs richards-scale complexity (see module doc).\n",
  )
}
