import arc/host_hooks.{type HostHooks}
import arc/interp/entry
import arc/rt/builtins as rt_builtins
import arc/rt/types.{type Agent, type JsVal}
import carder/backend/build_beam
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/string

pub type RunResult =
  Result(Dynamic, String)

pub type JsExecOutcome {
  JsReturned(value: Dynamic)
  JsThrew(exn: JsVal)
  JsCrashed(reason: String)
}

pub fn seed(hooks: HostHooks) -> Agent {
  entry.link(rt_builtins.new_agent(hooks))
}

pub fn load(beam: BitArray, name: String) -> Result(Atom, String) {
  build_beam.load_module(atom.create(name), name, beam)
}

@external(erlang, "arc_aot_exec_ffi", "unload")
pub fn unload(module: Atom) -> Nil

@external(erlang, "arc_aot_exec_ffi", "apply_js_main")
pub fn apply_main(module: Atom, st: Agent) -> #(JsExecOutcome, Agent)

pub fn run_loaded(module: Atom, st: Agent) -> #(Agent, RunResult) {
  let #(outcome, st) = apply_main(module, st)
  let result = case outcome {
    JsReturned(v) -> Ok(v)
    JsThrew(e) -> Error("uncaught: " <> string.inspect(e))
    JsCrashed(reason) -> Error(reason)
  }
  #(st, result)
}

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

pub fn run_beam(
  beam: BitArray,
  name: String,
  hooks: HostHooks,
) -> #(Agent, RunResult) {
  run_beam_in(seed(hooks), beam, name)
}
