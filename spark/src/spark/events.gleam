import spark/builtin.{type Builtin, Builtin}
import spark/io.{type Io}
import spark/js

/// EventEmitter and the rest of the events module. If you also pass the dom
/// events module, its helpers take an EventTarget too. Passing async_hooks
/// makes EventEmitterAsyncResource work, and passing process makes it warn
/// about listener leaks
pub fn builtin() -> Builtin(host) {
  Builtin(
    specifier: "node:events",
    source: js.events,
    natives: fn(_io: Io) { [] },
    dependencies: ["spark:errors", "spark:loop"],
    optional: ["spark:dom_events", "node:async_hooks", "node:process"],
    exports: [
      "EventEmitter", "EventEmitterAsyncResource", "addAbortListener",
      "captureRejectionSymbol", "captureRejections", "defaultMaxListeners",
      "errorMonitor", "getEventListeners", "getMaxListeners", "init",
      "listenerCount", "on", "once", "setMaxListeners", "usingDomains",
    ],
    globals: [],
  )
}
