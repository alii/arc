import spark/builtin.{type Builtin, Builtin}
import spark/js

/// the `DOMException` and `QuotaExceededError` globals that web apis throw,
/// along with DOMException's legacy codes
pub fn builtin() -> Builtin(host) {
  Builtin(
    specifier: "spark:domexception",
    source: js.domexception,
    natives: fn(_io) { [] },
    dependencies: [],
    optional: [],
    exports: ["DOMException", "QuotaExceededError"],
    globals: ["DOMException", "QuotaExceededError"],
  )
}
