import spark/builtin.{type Builtin, Builtin}
import spark/io.{type Io}
import spark/js

/// EventEmitter and the rest of the events module. With the dom events
/// module passed too, its helpers also take an EventTarget; with
/// async_hooks, EventEmitterAsyncResource works; with process, a listener
/// leak is warned about
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
