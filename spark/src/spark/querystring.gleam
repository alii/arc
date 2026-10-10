import spark/builtin.{type Builtin, Builtin}
import spark/io.{type Io}
import spark/js

pub fn builtin() -> Builtin(host) {
  Builtin(
    specifier: "node:querystring",
    source: js.querystring,
    natives: fn(_io: Io) { [] },
    dependencies: ["node:buffer", "spark:querystring"],
    optional: [],
    exports: [
      "decode", "encode", "escape", "parse", "stringify", "unescape",
      "unescapeBuffer",
    ],
    globals: [],
  )
}

/// the escaping tables querystring and url share
pub fn internal_builtin() -> Builtin(host) {
  Builtin(
    specifier: "spark:querystring",
    source: js.querystring_internal,
    natives: fn(_io: Io) { [] },
    dependencies: ["spark:errors"],
    optional: [],
    exports: ["encodeStr", "hexTable", "isHexTable"],
    globals: [],
  )
}
