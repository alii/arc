// run with: gleam run -m arc/examples/module_instance

import arc/engine.{ModuleReturned, Returned, Threw}
import arc/host
import arc/module/loader
import arc/rt/types.{mk_string, mk_undefined}
import gleam/io
import gleam/list
import gleam/option.{Some}

pub fn main() -> Nil {
  let eng =
    engine.new()
    |> engine.define_namespace("Host", [host.HostMethod("emit", 1, emit)])

  let source =
    "let count = 0;
     export function receive(msg) {
       count += 1;
       Host.emit('[' + count + '] ' + msg);
     }"

  let #(resolve, load) = loader.no_imports()
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

fn emit(ctx: host.Context(Nil), args, _this) {
  use text, ctx <- host.validate_string(ctx, host.first_arg(args), "text")
  io.println(text)
  #(Ok(mk_undefined()), ctx)
}
