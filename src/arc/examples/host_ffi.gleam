//// Embedding arc with host-provided native functions.
////
//// Run with: gleam run -m arc/examples/host_ffi

import arc/engine.{JsString, Returned}
import arc/host
import arc/rt/inspect as rt_inspect
import arc/rt/types.{JInt, mk_number, mk_string, mk_undefined}
import gleam/io
import gleam/list
import gleam/string

pub fn main() -> Nil {
  let eng =
    engine.new()
    |> engine.define_fn("print", 1, print)
    |> engine.define_namespace("host", [#("uppercase", 1, uppercase)])
    |> engine.define_fn("mapRange", 2, map_range)

  let source =
    "
    print(host.uppercase('hello from js'));
    print(mapRange(5, i => i * i).join(','));

    try { host.uppercase(42) }      catch (e) { print(e.message) }
    try { mapRange(3, 'not a fn') } catch (e) { print(e.message) }
    "

  let assert Ok(#(Returned(_), _)) = engine.eval(eng, source)
  Nil
}

fn print(args, _this, s: host.State(Nil)) {
  io.println(list.map(args, display(s, _)) |> string.join(" "))
  #(s, Ok(mk_undefined()))
}

fn uppercase(args, _this, s) {
  use str, s <- host.validate_string(s, host.first_arg(args), "str")
  #(s, Ok(mk_string(string.uppercase(str))))
}

fn map_range(args, _this, s) {
  use n, s <- host.validate_integer(s, host.first_arg(args), "n", 0, 1_000_000)
  map_range_loop(s, host.arg_at(args, 1), 0, n, [])
}

fn map_range_loop(s, cb, i, n, acc) {
  case i >= n {
    True -> {
      let #(s, arr) = host.array(s, list.reverse(acc))
      #(s, Ok(arr))
    }
    False -> {
      use r, s <- host.try_call(s, cb, "callback", mk_undefined(), [
        mk_number(JInt(i)),
      ])
      map_range_loop(s, cb, i + 1, n, [r, ..acc])
    }
  }
}

fn display(s: host.State(Nil), v) {
  case engine.classify(v) {
    JsString(str) -> str
    _ -> rt_inspect.inspect(s.agent, v)
  }
}
