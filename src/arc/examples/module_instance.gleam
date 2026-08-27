// run with: gleam run -m arc/examples/module_instance

import arc/engine.{ModuleReturned, Returned, Threw}
import arc/host
import arc/module_host
import arc/rt/types.{mk_string, mk_undefined}
import gleam/io
import gleam/list
import gleam/option.{Some}

pub fn main() -> Nil {
  let eng =
    engine.new()
    |> engine.define_namespace("Host", [#("emit", 1, emit)])

  let source =
    "let count = 0;
     export function receive(msg) {
       count += 1;
       Host.emit('[' + count + '] ' + msg);
     }"

  let #(resolve, load) = module_host.no_imports()
  let assert Ok(#(ModuleReturned(namespace:, ..), eng)) =
    engine.eval_module(eng, "demo:greeter", source, resolve, load)
  let assert Some(receive) = engine.read_export(eng, namespace, "receive")

  list.fold(["hello", "world", "again"], eng, fn(eng, msg) {
    case engine.call(eng, receive, mk_undefined(), [mk_string(msg)]) {
      #(Returned(_), eng) -> eng
      #(Threw(val), eng) -> {
        io.println_error("receive threw: " <> engine.format_error(eng, val))
        eng
      }
    }
  })
  Nil
}

fn emit(args, _this, s: host.State(Nil)) {
  use text, s <- host.validate_string(s, host.first_arg(args), "text")
  io.println(text)
  #(s, Ok(mk_undefined()))
}
