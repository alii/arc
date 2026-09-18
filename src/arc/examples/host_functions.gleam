// run with: gleam run -m arc/examples/host_functions

import arc/engine.{JsString, Returned}
import arc/host
import arc/rt/inspect as rt_inspect
import arc/rt/types.{mk_int, mk_string, mk_undefined}
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

fn print(args, _this, ctx: host.Context(Nil)) {
  io.println(list.map(args, display(ctx, _)) |> string.join(" "))
  #(ctx, Ok(mk_undefined()))
}

fn uppercase(args, _this, ctx) {
  use str, ctx <- host.validate_string(ctx, host.first_arg(args), "str")
  #(ctx, Ok(mk_string(string.uppercase(str))))
}

fn map_range(args, _this, ctx) {
  use n, ctx <- host.validate_integer(
    ctx,
    host.first_arg(args),
    "n",
    0,
    1_000_000,
  )
  map_range_loop(ctx, host.arg_at(args, 1), 0, n, [])
}

fn map_range_loop(ctx, cb, i, n, acc) {
  case i >= n {
    True -> {
      let #(ctx, arr) = host.array(ctx, list.reverse(acc))
      #(ctx, Ok(arr))
    }
    False -> {
      use r, ctx <- host.try_call(ctx, cb, "callback", mk_undefined(), [
        mk_int(i),
      ])
      map_range_loop(ctx, cb, i + 1, n, [r, ..acc])
    }
  }
}

fn display(ctx: host.Context(Nil), v) {
  case engine.classify(v) {
    JsString(str) -> str
    _ -> rt_inspect.inspect(ctx.agent, v)
  }
}
