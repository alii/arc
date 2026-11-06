import spark/builtin.{type Builtin, Builtin}
import spark/inspect_natives
import spark/io.{type Io}
import spark/js

/// the global structuredClone. It deep-copies plain data, dates, regexps,
/// maps, sets, errors, buffers and their views, and keeps shared and circular
/// references. An array buffer in `transfer` moves to the copy and the
/// original is left detached
pub fn builtin() -> Builtin(host) {
  Builtin(
    specifier: "spark:structured_clone",
    source: js.structured_clone,
    natives: fn(_io: Io) { inspect_natives.natives() },
    dependencies: ["spark:errors", "spark:domexception"],
    optional: [],
    exports: ["structuredClone"],
    globals: ["structuredClone"],
  )
}
