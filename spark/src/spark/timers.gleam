import arc/host.{HostMethod}
import arc/rt/types.{mk_int}
import spark/async_hooks
import spark/builtin.{type Builtin, Builtin}
import spark/js

const names = [
  "setTimeout", "clearTimeout", "setInterval", "clearInterval", "setImmediate",
  "clearImmediate",
]

/// timers only record deadlines; the embedder's loop does the waiting and
/// calls spark.turn when one is due
pub fn builtin() -> Builtin(host) {
  Builtin(
    specifier: "node:timers",
    source: js.timers,
    natives: fn(_io) {
      [
        HostMethod("now", 0, fn(ctx, _args, _this) {
          #(Ok(mk_int(ctx.agent.hooks.monotonic_now())), ctx)
        }),
        ..async_hooks.context_natives()
      ]
    },
    dependencies: ["spark:loop", "spark:errors"],
    exports: names,
    globals: names,
  )
}
