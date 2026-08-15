//// Run a compiled JS module on the BEAM against arc's runtime: seed an
//// `Agent`, load the `.beam`, apply `js_main`, then drain microtasks and run
//// the GC safepoint. `DiffRun` is what the differential harness compares.

import arc/rt/builtins as rt_builtins
import arc/rt/store as rt_store
import arc/rt/types.{type Agent, type HostHooks}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/string
import twocore/backend/build_beam

/// Console bytes in emission order plus how the top level completed:
/// `Ok(v)` for a normal return, `Error(reason)` for a load failure, an
/// uncaught throw (rendered with `string.inspect`) or a crash.
pub type DiffRun {
  DiffRun(stdout: BitArray, result: Result(Dynamic, String))
}

/// Wire terms from `arc_aot_exec_ffi:apply_js_main/2`.
pub type JsExecOutcome {
  JsReturned(value: Dynamic)
  JsThrew(exn: Dynamic)
  JsCrashed(reason: String)
}

/// A fresh agent with a full realm. Pure data, so one seed can be applied
/// any number of times.
pub fn seed(hooks: HostHooks) -> Agent {
  rt_builtins.new_agent(hooks)
}

/// Load `beam` under the module atom `name`.
pub fn load(beam: BitArray, name: String) -> Result(Atom, String) {
  build_beam.load_module(atom.create(name), name, beam)
}

/// Apply `module:js_main(st, frame, [])` under a protected try, then drain
/// microtasks and run the GC safepoint. Never raises.
@external(erlang, "arc_aot_exec_ffi", "apply_js_main")
pub fn apply_main(module: Atom, st: Agent) -> #(JsExecOutcome, Agent)

/// `apply_main` packaged as a `DiffRun`.
pub fn run_loaded(module: Atom, st: Agent) -> #(Agent, DiffRun) {
  let #(outcome, st) = apply_main(module, st)
  let stdout = rt_store.t_console_bytes(st)
  let result = case outcome {
    JsReturned(v) -> Ok(v)
    JsThrew(e) -> Error("uncaught: " <> string.inspect(e))
    JsCrashed(reason) -> Error(reason)
  }
  #(st, DiffRun(stdout:, result:))
}

/// Load and run in an already seeded agent. On a load failure the agent is
/// returned unchanged with empty stdout.
pub fn run_beam_in(
  st: Agent,
  beam: BitArray,
  name: String,
) -> #(Agent, DiffRun) {
  case load(beam, name) {
    Error(reason) -> #(
      st,
      DiffRun(stdout: <<>>, result: Error("load failed: " <> reason)),
    )
    Ok(module) -> run_loaded(module, st)
  }
}

/// Seed from `hooks`, then `run_beam_in`.
pub fn run_beam(
  beam: BitArray,
  name: String,
  hooks: HostHooks,
) -> #(Agent, DiffRun) {
  run_beam_in(seed(hooks), beam, name)
}
