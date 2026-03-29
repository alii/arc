//// Arc-inside-AtomVM-WebAssembly entry point.
////
//// The Erlang side (arc_wasm_ffi) owns the receive loop and try/catch so a
//// stdlib gap doesn't kill the listener. This module is just the eval step.

import arc/engine
import arc/vm/completion
import arc/vm/ops/object

pub fn eval(source: String) -> Result(String, String) {
  case engine.eval_with_event_loop(engine.new(), source) {
    Ok(#(comp, eng)) ->
      case comp {
        completion.NormalCompletion(v, _) ->
          Ok(object.inspect(v, engine.heap(eng)))
        completion.ThrowCompletion(v, _) ->
          Error("Uncaught " <> object.inspect(v, engine.heap(eng)))
        _ -> Error("unexpected completion")
      }
    Error(e) -> Error(engine.eval_error_message(e))
  }
}
