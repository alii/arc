import spark/builtin.{type Builtin, Builtin}
import spark/js

/// the `DOMException` and `QuotaExceededError` globals: the errors web apis
/// throw, with their legacy codes
pub fn builtin() -> Builtin(host) {
  Builtin(
    specifier: "spark:domexception",
    source: js.domexception,
    natives: fn(_io) { [] },
    dependencies: [],
    exports: ["DOMException", "QuotaExceededError"],
    globals: ["DOMException", "QuotaExceededError"],
  )
}
