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

/// node:util/types; it is node:util's `types`, so pass that builtin too
pub fn types_builtin() -> Builtin(host) {
  Builtin(
    specifier: "node:util/types",
    source: js.util_types,
    natives: fn(_io: Io) { [] },
    dependencies: ["node:util"],
    optional: [],
    exports: [
      "isArgumentsObject",
      "isArrayBuffer",
      "isAsyncFunction",
      "isBigIntObject",
      "isBooleanObject",
      "isDate",
      "isExternal",
      "isGeneratorFunction",
      "isGeneratorObject",
      "isMap",
      "isMapIterator",
      "isModuleNamespaceObject",
      "isNativeError",
      "isNumberObject",
      "isPromise",
      "isProxy",
      "isRegExp",
      "isSet",
      "isSetIterator",
      "isSharedArrayBuffer",
      "isStringObject",
      "isSymbolObject",
      "isWeakMap",
      "isWeakSet",
      "isAnyArrayBuffer",
      "isBoxedPrimitive",
      "isArrayBufferView",
      "isDataView",
      "isTypedArray",
      "isUint8Array",
      "isUint8ClampedArray",
      "isUint16Array",
      "isUint32Array",
      "isInt8Array",
      "isInt16Array",
      "isInt32Array",
      "isFloat16Array",
      "isFloat32Array",
      "isFloat64Array",
      "isBigInt64Array",
      "isBigUint64Array",
      "isKeyObject",
      "isCryptoKey",
    ],
    globals: [],
  )
}
