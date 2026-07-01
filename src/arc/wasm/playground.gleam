//// Arc-inside-AtomVM-WebAssembly entry point.
////
//// The Erlang side (arc_wasm_ffi) owns the receive loop and try/catch so a
//// stdlib gap doesn't kill the listener. This module is just the eval step.

import arc/engine.{Returned, Threw}

pub fn eval(source: String) -> Result(String, String) {
  let eng = engine.new()
  case engine.eval(eng, source) {
    Ok(#(Returned(v), eng)) -> Ok(engine.inspect(eng, v))
    Ok(#(Threw(e), eng)) -> Error("Uncaught " <> engine.format_error(eng, e))
    Error(e) -> Error(engine.eval_error_message(e))
  }
}
