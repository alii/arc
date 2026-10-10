import spark/builtin.{type Builtin, Builtin}
import spark/io.{type Io}
import spark/js

/// node's streams: Readable, Writable, Duplex, Transform, PassThrough,
/// pipeline, finished and the rest. With the dom events module passed too,
/// abort signals work; with encoding, a readable can decode text
pub fn builtin() -> Builtin(host) {
  Builtin(
    specifier: "node:stream",
    source: js.stream,
    natives: fn(_io: Io) { [] },
    dependencies: [
      "spark:internal", "node:events", "node:buffer", "node:string_decoder",
    ],
    optional: ["spark:dom_events", "spark:encoding", "node:async_hooks"],
    exports: [
      "Duplex", "PassThrough", "Readable", "Stream", "Transform", "Writable",
      "_isArrayBufferView", "_isUint8Array", "_uint8ArrayToBuffer",
      "addAbortSignal", "compose", "destroy", "duplexPair", "finished",
      "getDefaultHighWaterMark", "isDestroyed", "isDisturbed", "isErrored",
      "isReadable", "isWritable", "pipeline", "promises",
      "setDefaultHighWaterMark",
    ],
    globals: [],
  )
}

/// pipeline and finished as promises
pub fn promises_builtin() -> Builtin(host) {
  Builtin(
    specifier: "node:stream/promises",
    source: js.stream_promises,
    natives: fn(_io: Io) { [] },
    dependencies: ["node:stream"],
    optional: [],
    exports: ["finished", "pipeline"],
    globals: [],
  )
}

/// read a whole stream as text, json, a buffer, an array buffer or bytes
pub fn consumers_builtin() -> Builtin(host) {
  Builtin(
    specifier: "node:stream/consumers",
    source: js.stream_consumers,
    natives: fn(_io: Io) { [] },
    dependencies: ["node:stream", "spark:internal"],
    optional: [],
    exports: ["arrayBuffer", "blob", "buffer", "bytes", "json", "text"],
    globals: [],
  )
}
