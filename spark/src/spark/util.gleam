import spark/builtin.{type Builtin, Builtin}
import spark/io.{type Io}
import spark/js

/// node's util: inspect, format, promisify, deprecate, the type checks,
/// isDeepStrictEqual, parseArgs, MIMEType and the rest. With the encoding
/// module passed too, it carries TextEncoder and TextDecoder
pub fn builtin() -> Builtin(host) {
  Builtin(
    specifier: "node:util",
    source: js.util,
    natives: fn(_io: Io) { [] },
    dependencies: ["spark:internal"],
    optional: ["spark:encoding"],
    exports: [
      "MIMEParams", "MIMEType", "TextDecoder", "TextEncoder", "_errnoException",
      "_exceptionWithHostPort", "_extend", "aborted", "callbackify",
      "convertProcessSignalToExitCode", "debounce", "debug", "debuglog",
      "deprecate", "diff", "format", "formatWithOptions", "getCallSites",
      "getSystemErrorMap", "getSystemErrorMessage", "getSystemErrorName",
      "inherits", "inspect", "isArray", "isDeepStrictEqual",
      "isPartialDeepStrictEqual", "markPromiseAsHandled", "parseArgs",
      "parseEnv", "promisify", "setTraceSigInt", "stripVTControlCharacters",
      "styleText", "throttle", "toUSVString", "transferableAbortController",
      "transferableAbortSignal", "types",
    ],
    globals: [],
  )
}
