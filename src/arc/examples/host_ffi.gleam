//// Embedding arc with host-provided native functions.
////
//// Run with: gleam run -m arc/examples/host_ffi

import arc/engine
import arc/host
import arc/vm/completion.{NormalCompletion}
import arc/vm/state
import arc/vm/value.{Finite, JsNumber, JsString, JsUndefined}
import gleam/int
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

  let assert Ok(#(NormalCompletion(..), _)) = engine.eval(eng, source)
  Nil
}

fn print(args, _this, s) {
  io.println(list.map(args, display) |> string.join(" "))
  #(s, Ok(JsUndefined))
}

fn uppercase(args, _this, s) {
  case args {
    [v, ..] -> {
      use str, s <- host.validate_string(s, v, "str")
      #(s, Ok(JsString(string.uppercase(str))))
    }
    _ -> state.type_error(s, "uppercase: expected 1 argument")
  }
}

fn map_range(args, _this, s) {
  case args {
    [n, cb, ..] -> {
      use n, s <- host.validate_integer(s, n, "n", 0, 1_000_000)
      map_range_loop(s, cb, 0, n, [])
    }
    _ -> state.type_error(s, "mapRange: expected (n, callback)")
  }
}

fn map_range_loop(s, cb, i, n, acc) {
  case i >= n {
    True -> {
      let #(s, arr) = host.array(s, list.reverse(acc))
      #(s, Ok(arr))
    }
    False -> {
      use r, s <- host.try_call(s, cb, "callback", JsUndefined, [
        JsNumber(Finite(int.to_float(i))),
      ])
      map_range_loop(s, cb, i + 1, n, [r, ..acc])
    }
  }
}

fn display(v) {
  case v {
    JsString(s) -> s
    _ -> string.inspect(v)
  }
}
