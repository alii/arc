import spark/builtin.{type Builtin, Builtin}
import spark/io.{type Io}
import spark/js

/// StringDecoder turns bytes into text across chunks. It holds back any
/// character that a chunk cuts short
pub fn builtin() -> Builtin(host) {
  Builtin(
    specifier: "node:string_decoder",
    source: js.string_decoder,
    natives: fn(_io: Io) { [] },
    dependencies: ["spark:errors", "node:buffer"],
    optional: [],
    exports: ["StringDecoder"],
    globals: [],
  )
}
