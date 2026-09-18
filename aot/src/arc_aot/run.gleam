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

pub fn new_linked_agent(hooks: HostHooks) -> Agent {
  entry.link(rt_builtins.new_agent(hooks))
}

pub fn load(code: BitArray, name: String) -> Result(Atom, String) {
  build_beam.load_module(atom.create(name), name, code)
}

@external(erlang, "arc_aot_run_ffi", "unload")
pub fn unload(module: Atom) -> Nil

@external(erlang, "arc_aot_run_ffi", "apply_js_main")
pub fn apply_js_main(st: Agent, module: Atom) -> #(JsExecOutcome, Agent)

pub fn main(st: Agent, module: Atom) -> #(RunResult, Agent) {
  let #(outcome, st) = apply_js_main(st, module)
  let result = case outcome {
    JsReturned(v) -> Ok(v)
    JsThrew(e) -> Error("uncaught: " <> string.inspect(e))
    JsCrashed(reason) -> Error(reason)
  }
  #(result, st)
}

pub fn from_beam_in(
  st: Agent,
  code: BitArray,
  name: String,
) -> #(RunResult, Agent) {
  case load(code, name) {
    Error(reason) -> #(Error("load failed: " <> reason), st)
    Ok(module) -> main(st, module)
  }
}

pub fn from_beam(
  code: BitArray,
  name: String,
  hooks: HostHooks,
) -> #(RunResult, Agent) {
  from_beam_in(new_linked_agent(hooks), code, name)
}
