import arc/host.{HostMethod}
import arc/rt/types.{mk_int}
import spark/builtin.{type Builtin, Builtin}
import spark/js

/// node:perf_hooks and the `performance` global, so far only now,
/// timeOrigin and toJSON, read from the engine's clock hooks
pub fn builtin() -> Builtin(host) {
  Builtin(
    specifier: "node:perf_hooks",
    source: js.perf_hooks,
    natives: fn(_io) {
      [
        HostMethod("now", 0, fn(ctx, _args, _this) {
          #(Ok(mk_int(ctx.agent.hooks.monotonic_now())), ctx)
        }),
        HostMethod("wallClock", 0, fn(ctx, _args, _this) {
          #(Ok(mk_int(ctx.agent.hooks.wall_clock_ms())), ctx)
        }),
      ]
    },
    dependencies: ["spark:errors"],
    exports: ["performance"],
    globals: ["performance"],
  )
}
