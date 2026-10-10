import spark/builtin.{type Builtin, Builtin}
import spark/io.{type Io}
import spark/js

/// StringDecoder: bytes to text across chunks, holding back a character
/// that a chunk cuts short
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
