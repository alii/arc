import arc/host.{HostMethod}
import arc/rt/types.{mk_int}
import spark/builtin.{type Builtin, Builtin}
import spark/js

/// the dom standard's event and abort interfaces as globals: Event,
/// CustomEvent, EventTarget, AbortController and AbortSignal
pub fn builtin() -> Builtin(host) {
  Builtin(
    specifier: "spark:dom_events",
    source: js.dom_events,
    natives: fn(_io) {
      [
        HostMethod("now", 0, fn(ctx, _args, _this) {
          #(Ok(mk_int(ctx.agent.hooks.monotonic_now())), ctx)
        }),
      ]
    },
    dependencies: [
      "spark:errors", "spark:loop", "spark:domexception", "node:timers",
    ],
    exports: [
      "Event", "CustomEvent", "EventTarget", "AbortController", "AbortSignal",
    ],
    globals: [
      "Event", "CustomEvent", "EventTarget", "AbortController", "AbortSignal",
    ],
  )
}
