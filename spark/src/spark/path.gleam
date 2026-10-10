import spark/builtin.{type Builtin, Builtin}
import spark/io.{type Io}
import spark/js

const exports = [
  "_makeLong", "basename", "delimiter", "dirname", "extname", "format",
  "isAbsolute", "join", "matchesGlob", "normalize", "parse", "posix", "relative",
  "resolve", "sep", "toNamespacedPath", "win32",
]

/// path, posix or windows flavoured by the platform the process builtin was
/// given; it also reads the working directory from there
pub fn builtin() -> Builtin(host) {
  Builtin(
    specifier: "node:path",
    source: js.path,
    natives: fn(_io: Io) { [] },
    dependencies: ["spark:errors", "node:process"],
    optional: [],
    exports:,
    globals: [],
  )
}

pub fn posix_builtin() -> Builtin(host) {
  Builtin(
    specifier: "node:path/posix",
    source: js.path_posix,
    natives: fn(_io: Io) { [] },
    dependencies: ["node:path"],
    optional: [],
    exports:,
    globals: [],
  )
}

pub fn win32_builtin() -> Builtin(host) {
  Builtin(
    specifier: "node:path/win32",
    source: js.path_win32,
    natives: fn(_io: Io) { [] },
    dependencies: ["node:path"],
    optional: [],
    exports:,
    globals: [],
  )
}
