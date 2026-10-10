import spark/builtin.{type Builtin, Builtin}
import spark/js

/// the `global` name node programs use for the global object
pub fn builtin() -> Builtin(host) {
  Builtin(
    specifier: "spark:global",
    source: js.global,
    natives: fn(_io) { [] },
    dependencies: [],
    exports: ["global"],
    globals: ["global"],
  )
}
