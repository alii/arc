import spark/builtin.{type Builtin, Builtin}
import spark/io.{type Io}
import spark/js

/// the web's Blob and File as globals, using node's own javascript. A blob
/// holds all its bytes in memory, and blobs backed by a file on disk aren't
/// supported
pub fn builtin() -> Builtin(host) {
  Builtin(
    specifier: "spark:blob",
    source: js.blob,
    natives: fn(_io: Io) { [] },
    dependencies: [
      "spark:internal", "spark:encoding", "node:buffer", "node:timers",
      "node:url", "node:stream/web",
    ],
    optional: [],
    exports: ["Blob", "File"],
    globals: ["Blob", "File"],
  )
}
