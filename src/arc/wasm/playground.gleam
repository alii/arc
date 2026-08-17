//// Arc-inside-AtomVM-WebAssembly entry point.
////
//// The Erlang side (arc_wasm_ffi) owns the receive loop and try/catch so a
//// stdlib gap doesn't kill the listener. This module is just the eval step.
////
//// The engine is built ONCE (`new_engine`, at listener start) and every run
//// evaluates against that same pristine value: `engine.eval` is a pure
//// function of the engine it's given, so runs can't see each other, and
//// building the global environment — ~70k words, ~1.5s under AtomVM-WASM —
//// is paid at boot instead of on every click.

import arc/engine.{type Engine, Returned, Threw}

/// The pristine engine every playground run starts from.
pub fn new_engine() -> Engine(host) {
  engine.new()
}

pub fn eval(eng: Engine(host), source: String) -> Result(String, String) {
  case engine.eval(eng, source) {
    Ok(#(Returned(v), eng)) -> Ok(engine.inspect(eng, v))
    Ok(#(Threw(e), eng)) -> Error("Uncaught " <> engine.format_error(eng, e))
    Error(e) -> Error(engine.eval_error_message(e))
  }
}
