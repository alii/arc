import spark/builtin.{type Builtin, Builtin}
import spark/io.{type Io}
import spark/js

/// the web's Blob and File, each a global. They are node's own javascript;
/// the bytes are held whole in memory. A blob backed by a file on disk is
/// not here
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
