//// Run a compiled JS module on the BEAM against arc's runtime: seed an
//// `Agent`, load the `.beam`, apply `js_main`, then drain microtasks and run
//// the GC safepoint. Console output goes wherever `HostHooks.print` sends it.

import arc/host_hooks.{type HostHooks}
import arc/rt/builtins as rt_builtins
import arc/rt/types.{type Agent}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/string
import twocore/backend/build_beam

/// How the top level completed: `Ok(v)` for a normal return,
/// `Error(reason)` for a load failure, an uncaught throw (rendered with
/// `string.inspect`) or a crash.
pub type RunResult =
  Result(Dynamic, String)

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

/// `apply_main` with the outcome folded into a `RunResult`.
pub fn run_loaded(module: Atom, st: Agent) -> #(Agent, RunResult) {
  let #(outcome, st) = apply_main(module, st)
  let result = case outcome {
    JsReturned(v) -> Ok(v)
    JsThrew(e) -> Error("uncaught: " <> string.inspect(e))
    JsCrashed(reason) -> Error(reason)
  }
  #(st, result)
}

/// Load and run in an already seeded agent. On a load failure the agent is
/// returned unchanged.
pub fn run_beam_in(
  st: Agent,
  beam: BitArray,
  name: String,
) -> #(Agent, RunResult) {
  case load(beam, name) {
    Error(reason) -> #(st, Error("load failed: " <> reason))
    Ok(module) -> run_loaded(module, st)
  }
}

/// Seed from `hooks`, then `run_beam_in`.
pub fn run_beam(
  beam: BitArray,
  name: String,
  hooks: HostHooks,
) -> #(Agent, RunResult) {
  run_beam_in(seed(hooks), beam, name)
}
