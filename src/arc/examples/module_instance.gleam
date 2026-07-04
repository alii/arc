//// Embedding arc as a *module instance*: evaluate an ES module once, read a
//// `receive` export off its namespace, then call it for each incoming message.
//// The engine threads module-scoped state forward between calls — `count`
//// below survives across `receive` invocations because each `engine.call`
//// hands back an engine carrying the updated heap.
////
//// This is the lifecycle the Dance server (otters) is built on, distilled to
//// one file. Run with: gleam run -m arc/examples/module_instance

import arc/engine.{Returned, Threw}
import arc/host
import arc/module_host
import arc/vm/value.{JsString, JsUndefined}
import gleam/io
import gleam/list
import gleam/option.{Some}

pub fn main() -> Nil {
  // 1. Stand up an engine and compose a host namespace onto it.
  let eng =
    engine.new()
    |> engine.define_namespace("Host", [#("emit", 1, emit)])

  // 2. Evaluate an ES module that keeps state and exports `receive`.
  let source =
    "let count = 0;
     export function receive(msg) {
       count += 1;
       Host.emit('[' + count + '] ' + msg);
     }"

  let assert Ok(#(evaluated, eng)) =
    engine.eval_module(
      eng,
      "demo:greeter",
      source,
      reject_imports,
      reject_loads,
    )
  let assert Some(namespace) = evaluated.namespace
  let assert Some(receive) = engine.read_export(eng, namespace, "receive")

  // 3. Drive it: each call threads the heap forward, so `count` accumulates.
  list.fold(["hello", "world", "again"], eng, fn(eng, msg) {
    case engine.call(eng, receive, JsUndefined, [JsString(msg)]) {
      Ok(#(Returned(_), eng)) -> eng
      Ok(#(Threw(val), eng)) -> {
        io.println_error("receive threw: " <> engine.inspect(eng, val))
        eng
      }
      Error(err) -> {
        io.println_error("receive error: " <> engine.eval_error_message(err))
        eng
      }
    }
  })
  Nil
}

/// `Host.emit(text)` — print a line. The embedder's window into the sandbox.
/// A missing or non-string argument throws a TypeError back into JS rather
/// than quietly printing an empty line.
fn emit(args, _this, s) {
  use text, s <- host.validate_string(s, host.arg_at(args, 0), "text")
  io.println(text)
  #(s, Ok(JsUndefined))
}

/// Self-contained demo module — reject every import.
fn reject_imports(raw: String, _parent: String) {
  Error(module_host.ImportsForbidden(raw))
}

fn reject_loads(specifier: String) {
  Error(module_host.ImportsForbidden(specifier))
}
