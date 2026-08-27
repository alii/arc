import arc/engine.{type Engine, Returned, Threw}

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
