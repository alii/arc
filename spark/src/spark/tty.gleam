import arc/host.{HostMethod}
import arc/rt/types.{mk_int}
import gleam/list
import spark/builtin.{type Builtin, Builtin}
import spark/js

/// node:tty, so far: isatty. `terminals` is the file descriptors that are
/// terminals, such as [1, 2]; spark never asks the machine
pub fn builtin(terminals: List(Int)) -> Builtin(host) {
  Builtin(
    specifier: "node:tty",
    source: js.tty,
    natives: fn(_io) {
      [
        HostMethod("terminals", 0, fn(ctx, _args, _this) {
          let #(array, ctx) = host.array(ctx, list.map(terminals, mk_int))
          #(Ok(array), ctx)
        }),
      ]
    },
    dependencies: ["spark:errors"],
    optional: [],
    exports: ["isatty", "ReadStream", "WriteStream"],
    globals: [],
  )
}
