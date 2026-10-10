"use strict";

// node's own internals, the files the rest of its library is written
// against: errors, validators, util, inspect and a few small ones. Other
// ports reach them through `load`. What these ask of node's native side is
// answered by `bindings` and `shims` below

const loop = require("spark:loop");
const { kind, promiseDetails, proxyDetails, previewEntries } = native;

// a module the embedder may not have passed
function optional(specifier) {
  try {
    return require(specifier);
  } catch {
    return undefined;
  }
}

// node's primordials: every builtin under a name that cannot be patched
// later, methods taking `this` as their first argument
function makePrimordials() {
  const primordials = { __proto__: null };
  const uncurryThis = (fn) =>
    function (thisArg, ...args) {
      return Reflect.apply(fn, thisArg, args);
    };
  const capital = (key) =>
    typeof key === "symbol"
      ? `Symbol${key.description[7].toUpperCase()}${key.description.slice(8)}`
      : `${key[0].toUpperCase()}${key.slice(1)}`;

  function copyStatics(source, prefix, bind) {
    for (const key of Reflect.ownKeys(source)) {
      if (key === "prototype" || key === "name" || key === "length") continue;
      const desc = Reflect.getOwnPropertyDescriptor(source, key);
      if (!("value" in desc)) continue;
      const { value } = desc;
      primordials[`${prefix}${capital(key)}`] =
        bind && typeof value === "function" ? value.bind(source) : value;
    }
  }

  function copyPrototype(prototype, prefix) {
    primordials[`${prefix}Prototype`] = prototype;
    for (const key of Reflect.ownKeys(prototype)) {
      if (key === "constructor") continue;
      const desc = Reflect.getOwnPropertyDescriptor(prototype, key);
      const name = `${prefix}Prototype`;
      if ("value" in desc) {
        primordials[`${name}${capital(key)}`] =
          typeof desc.value === "function"
            ? uncurryThis(desc.value)
            : desc.value;
      } else {
        if (desc.get) primordials[`${name}Get${capital(key)}`] = uncurryThis(desc.get);
        if (desc.set) primordials[`${name}Set${capital(key)}`] = uncurryThis(desc.set);
      }
    }
  }

  for (const name of ["JSON", "Math", "Reflect", "Atomics"]) {
    if (globalThis[name] !== undefined) copyStatics(globalThis[name], name, false);
  }
  for (const name of [
    "AggregateError", "Array", "ArrayBuffer", "BigInt", "BigInt64Array",
    "BigUint64Array", "Boolean", "DataView", "Date", "Error", "EvalError",
    "FinalizationRegistry", "Float16Array", "Float32Array", "Float64Array",
    "Function", "Int16Array", "Int32Array", "Int8Array", "Map", "Number",
    "Object", "Promise", "RangeError", "ReferenceError", "RegExp", "Set",
    "SharedArrayBuffer", "String", "Symbol", "SyntaxError", "TypeError",
    "URIError", "Uint16Array", "Uint32Array", "Uint8Array",
    "Uint8ClampedArray", "WeakMap", "WeakRef", "WeakSet",
  ]) {
    const original = globalThis[name];
    if (original === undefined) continue;
    primordials[name] = original;
    copyStatics(original, name, true);
    copyPrototype(original.prototype, name);
  }
  primordials.Proxy = Proxy;
  primordials.globalThis = globalThis;
  for (const name of ["decodeURI", "decodeURIComponent", "encodeURI", "encodeURIComponent"]) {
    primordials[name] = globalThis[name];
  }

  const TypedArray = Object.getPrototypeOf(Uint8Array);
  primordials.TypedArray = TypedArray;
  copyStatics(TypedArray, "TypedArray", true);
  copyPrototype(TypedArray.prototype, "TypedArray");

  const ArrayIteratorPrototype = Object.getPrototypeOf([][Symbol.iterator]());
  primordials.ArrayIteratorPrototype = ArrayIteratorPrototype;
  primordials.ArrayIteratorPrototypeNext = uncurryThis(ArrayIteratorPrototype.next);
  primordials.IteratorPrototype = Object.getPrototypeOf(ArrayIteratorPrototype);
  const StringIteratorPrototype = Object.getPrototypeOf(""[Symbol.iterator]());
  primordials.StringIteratorPrototype = StringIteratorPrototype;
  primordials.StringIteratorPrototypeNext = uncurryThis(StringIteratorPrototype.next);
  primordials.AsyncIteratorPrototype = Object.getPrototypeOf(
    Object.getPrototypeOf(async function* () {}).prototype,
  );

  // the helpers that take their arguments as one list
  for (const name of ["Push", "Unshift", "Concat"]) {
    const method = Array.prototype[name.toLowerCase()];
    primordials[`ArrayPrototype${name}Apply`] = (array, items) =>
      Reflect.apply(method, array, items);
  }
  primordials.StringPrototypeConcatApply = (string, items) =>
    Reflect.apply(String.prototype.concat, string, items);
  primordials.MathMaxApply = (numbers) => Reflect.apply(Math.max, undefined, numbers);
  primordials.MathMinApply = (numbers) => Reflect.apply(Math.min, undefined, numbers);
  primordials.StringFromCharCodeApply = (codes) =>
    Reflect.apply(String.fromCharCode, undefined, codes);
  primordials.ArrayOfApply = (items) => Reflect.apply(Array.of, Array, items);
  primordials.TypedArrayOfApply = (constructor, items) =>
    Reflect.apply(TypedArray.of, constructor, items);

  primordials.uncurryThis = uncurryThis;
  primordials.applyBind = (fn) => (thisArg, args) => Reflect.apply(fn, thisArg, args);
  // nothing here can reach in and patch the builtins, so the plain ones do
  primordials.SafeMap = Map;
  primordials.SafeSet = Set;
  primordials.SafeWeakMap = WeakMap;
  primordials.SafeWeakSet = WeakSet;
  primordials.SafeWeakRef = WeakRef;
  primordials.SafeFinalizationRegistry = FinalizationRegistry;
  primordials.SafePromise = Promise;
  primordials.SafeArrayIterator = function SafeArrayIterator(array) {
    return array[Symbol.iterator]();
  };
  primordials.SafeStringIterator = function SafeStringIterator(string) {
    return string[Symbol.iterator]();
  };
  primordials.SafePromiseAll = (promises, mapFn) =>
    Promise.all(mapFn ? Array.from(promises, mapFn) : promises);
  primordials.SafePromiseAllReturnVoid = (promises, mapFn) =>
    Promise.all(mapFn ? Array.from(promises, mapFn) : promises).then(() => {});
  primordials.SafePromiseAllReturnArrayLike = primordials.SafePromiseAll;
  primordials.SafePromiseAllSettled = (promises, mapFn) =>
    Promise.allSettled(mapFn ? Array.from(promises, mapFn) : promises);
  primordials.SafePromiseAllSettledReturnVoid = (promises, mapFn) =>
    primordials.SafePromiseAllSettled(promises, mapFn).then(() => {});
  primordials.SafePromiseAny = (promises, mapFn) =>
    Promise.any(mapFn ? Array.from(promises, mapFn) : promises);
  primordials.SafePromiseRace = (promises, mapFn) =>
    Promise.race(mapFn ? Array.from(promises, mapFn) : promises);
  primordials.SafePromisePrototypeFinally = (promise, onFinally) =>
    promise.finally(onFinally);
  primordials.PromisePrototypeThen = (promise, onFulfilled, onRejected) =>
    promise.then(onFulfilled, onRejected);
  primordials.makeSafe = (unsafe, safe) => safe;
  primordials.hardenRegExp = (pattern) => pattern;
  primordials.SideEffectFreeRegExpPrototypeSymbolReplace = (pattern, string, replacement) =>
    string.replace(pattern, replacement);
  primordials.SideEffectFreeRegExpPrototypeSymbolSplit = (pattern, string, limit) =>
    string.split(pattern, limit);
  primordials.SideEffectFreeRegExpPrototypeExec = (pattern, string) =>
    pattern.exec(string);
  return primordials;
}

const primordials = makePrimordials();

// node's native bindings, as far as the files below ask
const kinds = (...names) => (value) => names.includes(kind(value));

function isArrayIndex(key) {
  if (typeof key !== "string") return false;
  const number = Number(key);
  return (
    Number.isInteger(number) &&
    number >= 0 &&
    number < 4294967295 &&
    String(number) === key
  );
}

const ALL_PROPERTIES = 0;
const ONLY_ENUMERABLE = 2;

const bindings = {
  __proto__: null,
  types: () => ({
    isExternal: () => false,
    isDate: kinds("date"),
    isArgumentsObject: kinds("arguments"),
    isBigIntObject: kinds("bigint_object"),
    isBooleanObject: kinds("boolean_object"),
    isNumberObject: kinds("number_object"),
    isStringObject: kinds("string_object"),
    isSymbolObject: kinds("symbol_object"),
    isBoxedPrimitive: kinds(
      "bigint_object",
      "boolean_object",
      "number_object",
      "string_object",
      "symbol_object",
    ),
    isNativeError: kinds("error"),
    isRegExp: kinds("regexp"),
    isAsyncFunction: kinds("async_function", "async_generator_function"),
    isGeneratorFunction: kinds(
      "generator_function",
      "async_generator_function",
    ),
    isGeneratorObject: kinds("generator_object"),
    isPromise: kinds("promise"),
    isMap: kinds("map"),
    isSet: kinds("set"),
    isMapIterator: kinds("map_iterator"),
    isSetIterator: kinds("set_iterator"),
    isWeakMap: kinds("weak_map"),
    isWeakSet: kinds("weak_set"),
    isArrayBuffer: kinds("array_buffer"),
    isSharedArrayBuffer: kinds("shared_array_buffer"),
    isAnyArrayBuffer: kinds("array_buffer", "shared_array_buffer"),
    isProxy: kinds("proxy"),
    isModuleNamespaceObject: kinds("module_namespace"),
  }),
  util: () => ({
    constants: {
      kPending: 0,
      kFulfilled: 1,
      kRejected: 2,
      ALL_PROPERTIES,
      ONLY_ENUMERABLE,
      kExiting: 0,
      kExitCode: 1,
      kHasExitCode: 2,
      kDisallowCloneAndTransfer: 0,
      kTransferable: 1,
      kCloneable: 2,
    },
    getOwnNonIndexProperties(object, filter) {
      const keys = Reflect.ownKeys(object).filter((key) => !isArrayIndex(key));
      if (filter !== ONLY_ENUMERABLE) return keys;
      return keys.filter((key) =>
        Object.prototype.propertyIsEnumerable.call(object, key),
      );
    },
    getPromiseDetails: (promise) => promiseDetails(promise),
    getProxyDetails(proxy, both = true) {
      const details = proxyDetails(proxy);
      if (details === undefined) return undefined;
      return both ? details : details[0];
    },
    previewEntries(object, isIterator) {
      const preview = previewEntries(object);
      if (preview === undefined) return undefined;
      return isIterator ? preview : preview[0];
    },
    // v8 remembers the class an object was made by; here only "Object" is known
    getConstructorName: () => "Object",
    getExternalValue: () => 0n,
    getCallSites: () => [],
    isInsideNodeModules: () => false,
    isConstructor(value) {
      try {
        Reflect.construct(Object, [], value);
        return true;
      } catch {
        return false;
      }
    },
    guessHandleType: () => "UNKNOWN",
    // getters that load a module the first time one of its names is read
    defineLazyProperties(target, id, keys, enumerable = true) {
      const descriptors = { __proto__: null };
      let mod;
      for (const key of keys) {
        let lazyLoadedValue;
        const set = (value) => {
          Object.defineProperty(target, key, {
            __proto__: null,
            writable: true,
            value,
          });
        };
        Object.defineProperty(set, "name", { __proto__: null, value: `set ${key}` });
        const get = () => {
          mod ??= loadAnywhere(id);
          if (lazyLoadedValue === undefined) {
            lazyLoadedValue = mod[key];
            set(lazyLoadedValue);
          }
          return lazyLoadedValue;
        };
        Object.defineProperty(get, "name", { __proto__: null, value: `get ${key}` });
        descriptors[key] = {
          __proto__: null,
          enumerable,
          configurable: true,
          get,
          set,
        };
      }
      Object.defineProperties(target, descriptors);
    },
    toUSVString: (string) => string.toWellFormed(),
    getSystemErrorMessage: (code) => uvErrors.get(code)?.[1] ?? `Unknown system error ${code}`,
    sleep() {},
    shouldAbortOnUncaughtToggle: [0],
    privateSymbols: {
      arrow_message_private_symbol: Symbol("node:arrowMessage"),
      decorated_private_symbol: Symbol("node:decorated"),
      untransferable_object_private_symbol: Symbol("node:untransferableObject"),
      transfer_mode_private_symbol: Symbol("node:transfer_mode"),
      entry_point_module_private_symbol: Symbol("node:entry_point_module"),
      host_defined_option_symbol: Symbol("node:host_defined_option_symbol"),
      module_first_parent_private_symbol: Symbol("node:module_first_parent"),
    },
  }),
  uv: () => ({
    errname: (code) => uvErrors.get(code)?.[0] ?? `Unknown system error ${code}`,
    getErrorMap: () => uvErrors,
    getErrorMessage: (code) => uvErrors.get(code)?.[1] ?? `Unknown system error ${code}`,
    UV_EOF: -4095,
    UV_EAI_MEMORY: -3006,
    UV_EAI_NODATA: -3007,
    UV_EAI_NONAME: -3008,
    UV_ENOTFOUND: -3008,
  }),
  buffer: () => ({
    // -1, 0 or 1 over the bytes of two views
    compare(left, right) {
      const a = new Uint8Array(left.buffer, left.byteOffset, left.byteLength);
      const b = new Uint8Array(right.buffer, right.byteOffset, right.byteLength);
      const length = Math.min(a.length, b.length);
      for (let i = 0; i < length; i++) {
        if (a[i] !== b[i]) return a[i] < b[i] ? -1 : 1;
      }
      return a.length === b.length ? 0 : a.length < b.length ? -1 : 1;
    },
  }),
  os: () => ({}),
  constants: () => ({ os: { signals: {}, errno: {} }, fs: {}, crypto: {}, zlib: {}, trace: {} }),
  config: () => ({ hasIntl: false, hasInspector: false, noBrowserGlobals: false }),
  icu: () => ({}),
  messaging: () => ({}),
  profiler: () => ({}),
  string_decoder: () => ({
    encodings: ["ascii", "utf8", "base64", "utf16le", "latin1", "hex", "buffer", "base64url"],
  }),
  symbols: () => ({}),
  errors: () => ({ exitCodes: { kNoFailure: 0, kGenericUserError: 1 } }),
};

// errno to [name, message], libuv's numbering on linux
const uvErrors = new Map([
  [-7, ["E2BIG", "argument list too long"]],
  [-13, ["EACCES", "permission denied"]],
  [-98, ["EADDRINUSE", "address already in use"]],
  [-99, ["EADDRNOTAVAIL", "address not available"]],
  [-11, ["EAGAIN", "resource temporarily unavailable"]],
  [-9, ["EBADF", "bad file descriptor"]],
  [-16, ["EBUSY", "resource busy or locked"]],
  [-125, ["ECANCELED", "operation canceled"]],
  [-103, ["ECONNABORTED", "software caused connection abort"]],
  [-111, ["ECONNREFUSED", "connection refused"]],
  [-104, ["ECONNRESET", "connection reset by peer"]],
  [-17, ["EEXIST", "file already exists"]],
  [-14, ["EFAULT", "bad address in system call argument"]],
  [-113, ["EHOSTUNREACH", "host is unreachable"]],
  [-4, ["EINTR", "interrupted system call"]],
  [-22, ["EINVAL", "invalid argument"]],
  [-5, ["EIO", "i/o error"]],
  [-106, ["EISCONN", "socket is already connected"]],
  [-21, ["EISDIR", "illegal operation on a directory"]],
  [-40, ["ELOOP", "too many symbolic links encountered"]],
  [-24, ["EMFILE", "too many open files"]],
  [-36, ["ENAMETOOLONG", "name too long"]],
  [-100, ["ENETDOWN", "network is down"]],
  [-101, ["ENETUNREACH", "network is unreachable"]],
  [-23, ["ENFILE", "file table overflow"]],
  [-2, ["ENOENT", "no such file or directory"]],
  [-12, ["ENOMEM", "not enough memory"]],
  [-28, ["ENOSPC", "no space left on device"]],
  [-38, ["ENOSYS", "function not implemented"]],
  [-107, ["ENOTCONN", "socket is not connected"]],
  [-20, ["ENOTDIR", "not a directory"]],
  [-39, ["ENOTEMPTY", "directory not empty"]],
  [-88, ["ENOTSOCK", "socket operation on non-socket"]],
  [-95, ["ENOTSUP", "operation not supported on socket"]],
  [-1, ["EPERM", "operation not permitted"]],
  [-32, ["EPIPE", "broken pipe"]],
  [-34, ["ERANGE", "result too large"]],
  [-30, ["EROFS", "read-only file system"]],
  [-29, ["ESPIPE", "invalid seek"]],
  [-3, ["ESRCH", "no such process"]],
  [-110, ["ETIMEDOUT", "connection timed out"]],
  [-26, ["ETXTBSY", "text file is busy"]],
  [-18, ["EXDEV", "cross-device link not permitted"]],
  [-4095, ["EOF", "end of file"]],
]);

// the other ports' loaders, newest first, so a name asked for here can be
// found in the port that holds it
const loaders = [];

function addLoader(loader) {
  loaders.push(loader);
}

function loadAnywhere(id) {
  for (let i = loaders.length - 1; i >= 0; i--) {
    try {
      return loaders[i](id);
    } catch (error) {
      if (!String(error?.message).startsWith("no module ")) throw error;
    }
  }
  return load(id);
}

function internalBinding(name) {
  if (!(name in bindings)) throw new Error("no binding " + name + " here");
  return bindings[name]();
}

// what the files see as `process`: the real one when it was passed
let seenProcess;
function processForFiles() {
  if (seenProcess !== undefined) return seenProcess;
  const real = optional("node:process");
  seenProcess = {
    __proto__: real ?? null,
    env: real?.env ?? {},
    platform: real?.platform ?? "linux",
    versions: real?.versions ?? {},
    execArgv: [],
    nextTick(callback, ...args) {
      loop.nextTick(() => callback(...args));
    },
    emitWarning(...args) {
      real?.emitWarning?.(...args);
    },
    cwd: () => real?.cwd?.() ?? "/",
  };
  return seenProcess;
}

const shims = {
  // node runs this once at startup
  afterLoad: {
    "internal/util/debuglog": (debuglog) =>
      debuglog.initializeDebugEnv(processForFiles().env.NODE_DEBUG),
  },
  process: processForFiles,
  primordials,
  internalBinding,
  buffer: () => optional("node:buffer") ?? {},
  path: () => optional("node:path") ?? {},
  vm: () => {
    throw new Error("vm is not here");
  },
  util: () => load("internal/util/inspect"),
  "internal/url": () => ({
    ...optional("node:url"),
    isURL: (self) =>
      Boolean(
        self?.href &&
        self.protocol &&
        self.auth === undefined &&
        self.path === undefined,
      ),
  }),
  "internal/options": () => ({
    getOptionValue: (name) => optionDefaults[name],
    getEmbedderOptions: () => ({}),
  }),
  "internal/process/warning": () => ({
    emitWarning: (...args) => processForFiles().emitWarning(...args),
    emitWarningSync: (...args) => processForFiles().emitWarning(...args),
  }),
  "internal/process/execution": () => ({}),
  "internal/source_map/source_map_cache": () => ({ findSourceMap: () => undefined }),
  "internal/trace_events": () => ({
    isTraceCategoryEnabled: () => false,
    trace() {},
  }),
  "internal/tty": () => ({ getColorDepth: () => 1, hasColors: () => false }),
  "internal/bootstrap/realm": () => ({
    BuiltinModule: { exists: () => false, canBeRequiredByUsers: () => false },
  }),
  "internal/modules/helpers": () => ({}),
  "internal/crypto/keys": () => ({
    isCryptoKey: () => false,
    isKeyObject: () => false,
  }),
  "internal/v8/startup_snapshot": () => ({
    namespace: {
      addSerializeCallback() {},
      addDeserializeCallback() {},
      isBuildingSnapshot: () => false,
    },
  }),
};

const optionDefaults = {
  __proto__: null,
  "--eval": "",
  "--pending-deprecation": false,
  "--no-deprecation": false,
  "--trace-deprecation": false,
  "--throw-deprecation": false,
  "--experimental-require-module": false,
  "--enable-source-maps": false,
  "--stack-trace-limit": 10,
};

const files = {
  "internal/errors": function (module, exports, require, process, primordials, internalBinding) {
    /* eslint node-core/documented-errors: "error" */
    /* eslint node-core/alphabetize-errors: ["error", {checkErrorDeclarations: true}] */
    /* eslint node-core/prefer-util-format-errors: "error" */

    'use strict';

    const {
      AggregateError,
      ArrayIsArray,
      ArrayPrototypeFilter,
      ArrayPrototypeIncludes,
      ArrayPrototypeIndexOf,
      ArrayPrototypeJoin,
      ArrayPrototypeMap,
      ArrayPrototypePush,
      ArrayPrototypeSlice,
      ArrayPrototypeSplice,
      ArrayPrototypeUnshift,
      Error,
      ErrorCaptureStackTrace,
      ErrorPrototypeToString,
      JSONStringify,
      MapPrototypeGet,
      MathAbs,
      MathMax,
      Number,
      NumberIsInteger,
      ObjectAssign,
      ObjectDefineProperties,
      ObjectDefineProperty,
      ObjectGetOwnPropertyDescriptor,
      ObjectIsExtensible,
      ObjectKeys,
      ObjectPrototypeHasOwnProperty,
      RangeError,
      ReflectApply,
      RegExpPrototypeExec,
      SafeArrayIterator,
      SafeMap,
      SafeWeakMap,
      String,
      StringPrototypeEndsWith,
      StringPrototypeIncludes,
      StringPrototypeIndexOf,
      StringPrototypeRepeat,
      StringPrototypeSlice,
      StringPrototypeSplit,
      StringPrototypeStartsWith,
      StringPrototypeToLowerCase,
      Symbol,
      SymbolFor,
      SyntaxError,
      TypeError,
      URIError,
    } = primordials;

    const kIsNodeError = Symbol('kIsNodeError');

    const isWindows = process.platform === 'win32';

    const messages = new SafeMap();
    const codes = {};

    const classRegExp = /^[A-Z][a-zA-Z0-9]*$/;

    const kTypes = [
      'string',
      'function',
      'number',
      'object',
      'Function',
      'Object',
      'boolean',
      'bigint',
      'symbol',
    ];

    const MainContextError = Error;
    const overrideStackTrace = new SafeWeakMap();
    let internalPrepareStackTrace = defaultPrepareStackTrace;

    function defaultPrepareStackTrace(error, trace) {
      let errorString;
      if (kIsNodeError in error) {
        errorString = `${error.name} [${error.code}]: ${error.message}`;
      } else {
        errorString = ErrorPrototypeToString(error);
      }
      if (trace.length === 0) {
        return errorString;
      }
      return `${errorString}\n    at ${trace.join('\n    at ')}`;
    }

    function setInternalPrepareStackTrace(callback) {
      internalPrepareStackTrace = callback;
    }

    function isPermissionModelError(err) {
      return typeof err !== 'number' && err.code && err.code === 'ERR_ACCESS_DENIED';
    }

    function prepareStackTraceCallback(globalThis, error, trace) {
      if (overrideStackTrace.has(error)) {
        const f = overrideStackTrace.get(error);
        overrideStackTrace.delete(error);
        return f(error, trace);
      }

      if (typeof globalThis.Error?.prepareStackTrace === 'function') {
        return globalThis.Error.prepareStackTrace(error, trace);
      }
      if (typeof MainContextError.prepareStackTrace === 'function') {
        return MainContextError.prepareStackTrace(error, trace);
      }

      return internalPrepareStackTrace(error, trace);
    }

    function ErrorPrepareStackTrace(error, trace) {
      return internalPrepareStackTrace(error, trace);
    }

    const aggregateTwoErrors = (innerError, outerError) => {
      if (innerError && outerError && innerError !== outerError) {
        if (Array.isArray(outerError.errors)) {
          outerError.errors.push(innerError);
          return outerError;
        }
        let err;
        if (isErrorStackTraceLimitWritable()) {
          const limit = Error.stackTraceLimit;
          Error.stackTraceLimit = 0;
          err = new AggregateError(new SafeArrayIterator([
            outerError,
            innerError,
          ]), outerError.message);
          Error.stackTraceLimit = limit;
          Error.captureStackTrace(err, aggregateTwoErrors);
        } else {
          err = new AggregateError(new SafeArrayIterator([
            outerError,
            innerError,
          ]), outerError.message);
        }
        err.code = outerError.code;
        return err;
      }
      return innerError || outerError;
    };

    class NodeAggregateError extends AggregateError {
      constructor(errors, message) {
        super(new SafeArrayIterator(errors), message);
        this.code = errors[0]?.code;
      }

      get [kIsNodeError]() {
        return true;
      }

      get ['constructor']() {
        return AggregateError;
      }
    }

    const assert = require('internal/assert');

    let util;

    let internalUtil = null;
    function lazyInternalUtil() {
      internalUtil ??= require('internal/util');
      return internalUtil;
    }

    let internalUtilInspect = null;
    function lazyInternalUtilInspect() {
      internalUtilInspect ??= require('internal/util/inspect');
      return internalUtilInspect;
    }

    let utilColors;
    function lazyUtilColors() {
      utilColors ??= require('internal/util/colors');
      return utilColors;
    }

    let buffer;
    function lazyBuffer() {
      buffer ??= require('buffer').Buffer;
      return buffer;
    }

    function isErrorStackTraceLimitWritable() {
      if (require('internal/v8/startup_snapshot').namespace.isBuildingSnapshot()) {
        return false;
      }

      const desc = Object.getOwnPropertyDescriptor(Error, 'stackTraceLimit');
      if (desc === undefined) {
        return ObjectIsExtensible(Error);
      }

      return ObjectPrototypeHasOwnProperty(desc, 'writable') ?
        desc.writable :
        desc.set !== undefined;
    }

    function inspectWithNoCustomRetry(obj, options) {
      const utilInspect = lazyInternalUtilInspect();

      try {
        return utilInspect.inspect(obj, options);
      } catch {
        return utilInspect.inspect(obj, { ...options, customInspect: false });
      }
    }

    class SystemError extends Error {
      constructor(key, context) {
        super();
        const prefix = getMessage(key, [], this);
        let message = `${prefix}: ${context.syscall} returned ` +
                      `${context.code} (${context.message})`;

        if (context.path !== undefined)
          message += ` ${context.path}`;
        if (context.dest !== undefined)
          message += ` => ${context.dest}`;

        this.code = key;

        Object.defineProperties(this, {
          [kIsNodeError]: {
            __proto__: null,
            value: true,
            enumerable: false,
            writable: false,
            configurable: true,
          },
          name: {
            __proto__: null,
            value: 'SystemError',
            enumerable: false,
            writable: true,
            configurable: true,
          },
          message: {
            __proto__: null,
            value: message,
            enumerable: false,
            writable: true,
            configurable: true,
          },
          info: {
            __proto__: null,
            value: context,
            enumerable: true,
            configurable: true,
            writable: false,
          },
          errno: {
            __proto__: null,
            get() {
              return context.errno;
            },
            set: (value) => {
              context.errno = value;
            },
            enumerable: true,
            configurable: true,
          },
          syscall: {
            __proto__: null,
            get() {
              return context.syscall;
            },
            set: (value) => {
              context.syscall = value;
            },
            enumerable: true,
            configurable: true,
          },
        });

        if (context.path !== undefined) {
          Object.defineProperty(this, 'path', {
            __proto__: null,
            get() {
              return context.path != null ?
                context.path.toString() : context.path;
            },
            set: (value) => {
              context.path = value ?
                lazyBuffer().from(value.toString()) : undefined;
            },
            enumerable: true,
            configurable: true,
          });
        }

        if (context.dest !== undefined) {
          Object.defineProperty(this, 'dest', {
            __proto__: null,
            get() {
              return context.dest != null ?
                context.dest.toString() : context.dest;
            },
            set: (value) => {
              context.dest = value ?
                lazyBuffer().from(value.toString()) : undefined;
            },
            enumerable: true,
            configurable: true,
          });
        }
      }

      toString() {
        return `${this.name} [${this.code}]: ${this.message}`;
      }

      [Symbol.for('nodejs.util.inspect.custom')](recurseTimes, ctx) {
        return lazyInternalUtilInspect().inspect(this, {
          ...ctx,
          getters: true,
          customInspect: false,
        });
      }
    }

    function makeSystemErrorWithCode(key) {
      return class NodeError extends SystemError {
        constructor(ctx) {
          super(key, ctx);
        }
      };
    }

    class HideStackFramesError extends Error {
    }

    function makeNodeErrorForHideStackFrame(Base, clazz) {
      class HideStackFramesError extends Base {
        constructor(...args) {
          if (isErrorStackTraceLimitWritable()) {
            const limit = Error.stackTraceLimit;
            Error.stackTraceLimit = 0;
            super(...args);
            Error.stackTraceLimit = limit;
          } else {
            super(...args);
          }
        }

        get ['constructor']() {
          return clazz;
        }
      }

      return HideStackFramesError;
    }

    function makeNodeErrorWithCode(Base, key) {
      const msg = messages.get(key);
      const expectedLength = typeof msg !== 'string' ? -1 : getExpectedArgumentLength(msg);

      switch (expectedLength) {
        case 0: {
          class NodeError extends Base {
            code = key;

            constructor(...args) {
              assert(
                args.length === 0,
                `Code: ${key}; The provided arguments length (${args.length}) does not ` +
                  `match the required ones (${expectedLength}).`,
              );
              super(msg);
            }

            get ['constructor']() {
              return Base;
            }

            get [kIsNodeError]() {
              return true;
            }

            toString() {
              return `${this.name} [${key}]: ${this.message}`;
            }
          }
          return NodeError;
        }
        case -1: {
          class NodeError extends Base {
            code = key;

            constructor(...args) {
              super();
              Object.defineProperty(this, 'message', {
                __proto__: null,
                value: getMessage(key, args, this),
                enumerable: false,
                writable: true,
                configurable: true,
              });
            }

            get ['constructor']() {
              return Base;
            }

            get [kIsNodeError]() {
              return true;
            }

            toString() {
              return `${this.name} [${key}]: ${this.message}`;
            }
          }
          return NodeError;
        }
        default: {

          class NodeError extends Base {
            code = key;

            constructor(...args) {
              assert(
                args.length === expectedLength,
                `Code: ${key}; The provided arguments length (${args.length}) does not ` +
                  `match the required ones (${expectedLength}).`,
              );

              args.unshift(msg);
              super(Reflect.apply(lazyInternalUtilInspect().format, null, args));
            }

            get ['constructor']() {
              return Base;
            }

            get [kIsNodeError]() {
              return true;
            }

            toString() {
              return `${this.name} [${key}]: ${this.message}`;
            }
          }
          return NodeError;
        }
      }
    }

    function hideStackFrames(fn) {
      function wrappedFn(...args) {
        try {
          return Reflect.apply(fn, this, args);
        } catch (error) {
          Error.stackTraceLimit && Error.captureStackTrace(error, wrappedFn);
          throw error;
        }
      }
      wrappedFn.withoutStackTrace = fn;
      return wrappedFn;
    }

    function E(sym, val, def, ...otherClasses) {
      messages.set(sym, val);

      const ErrClass = def === SystemError ?
        makeSystemErrorWithCode(sym) :
        makeNodeErrorWithCode(def, sym);

      if (otherClasses.length !== 0) {
        if (otherClasses.includes(HideStackFramesError)) {
          if (otherClasses.length !== 1) {
            otherClasses.forEach((clazz) => {
              if (clazz !== HideStackFramesError) {
                ErrClass[clazz.name] = makeNodeErrorWithCode(clazz, sym);
                ErrClass[clazz.name].HideStackFramesError = makeNodeErrorForHideStackFrame(ErrClass[clazz.name], clazz);
              }
            });
          }
        } else {
          otherClasses.forEach((clazz) => {
            ErrClass[clazz.name] = makeNodeErrorWithCode(clazz, sym);
          });
        }
      }

      if (otherClasses.includes(HideStackFramesError)) {
        ErrClass.HideStackFramesError = makeNodeErrorForHideStackFrame(ErrClass, def);
      }

      codes[sym] = ErrClass;
    }

    function getExpectedArgumentLength(msg) {
      let expectedLength = 0;
      const regex = /%[dfijoOs]/g;
      while (regex.exec(msg) !== null) expectedLength++;
      return expectedLength;
    }

    function getMessage(key, args, self) {
      const msg = messages.get(key);

      if (typeof msg === 'function') {
        assert(
          msg.length <= args.length, // Default options do not count.
          `Code: ${key}; The provided arguments length (${args.length}) does not ` +
            `match the required ones (${msg.length}).`,
        );
        return Reflect.apply(msg, self, args);
      }

      const expectedLength = getExpectedArgumentLength(msg);
      assert(
        expectedLength === args.length,
        `Code: ${key}; The provided arguments length (${args.length}) does not ` +
          `match the required ones (${expectedLength}).`,
      );
      if (args.length === 0)
        return msg;

      args.unshift(msg);
      return Reflect.apply(lazyInternalUtilInspect().format, null, args);
    }

    let uvBinding;

    function lazyUv() {
      uvBinding ??= internalBinding('uv');
      return uvBinding;
    }

    const uvUnmappedError = ['UNKNOWN', 'unknown error'];

    function uvErrmapGet(name) {
      uvBinding = lazyUv();
      uvBinding.errmap ??= uvBinding.getErrorMap();
      return MapPrototypeGet(uvBinding.errmap, name);
    }

    class UVException extends Error {
        constructor(ctx) {
        const { 0: code, 1: uvmsg } = uvErrmapGet(ctx.errno) || uvUnmappedError;
        let message = `${code}: ${ctx.message || uvmsg}, ${ctx.syscall}`;

        let path;
        let dest;
        if (ctx.path) {
          path = ctx.path.toString();
          message += ` '${path}'`;
        }
        if (ctx.dest) {
          dest = ctx.dest.toString();
          message += ` -> '${dest}'`;
        }

        super(message);

        for (const prop of Object.keys(ctx)) {
          if (prop === 'message' || prop === 'path' || prop === 'dest') {
            continue;
          }
          this[prop] = ctx[prop];
        }

        this.code = code;
        if (path) {
          this.path = path;
        }
        if (dest) {
          this.dest = dest;
        }
      }

      get ['constructor']() {
        return Error;
      }
    }

    class UVExceptionWithHostPort extends Error {
        constructor(err, syscall, address, port) {
        const { 0: code, 1: uvmsg } = uvErrmapGet(err) || uvUnmappedError;
        const message = `${syscall} ${code}: ${uvmsg}`;
        let details = '';

        if (port && port > 0) {
          details = ` ${address}:${port}`;
        } else if (address) {
          details = ` ${address}`;
        }

        super(`${message}${details}`);

        this.code = code;
        this.errno = err;
        this.syscall = syscall;
        this.address = address;
        if (port) {
          this.port = port;
        }
      }

      get ['constructor']() {
        return Error;
      }
    }

    class ErrnoException extends Error {
        constructor(err, syscall, original) {
        util ??= require('util');
        const code = util.getSystemErrorName(err);
        const message = original ?
          `${syscall} ${code} ${original}` : `${syscall} ${code}`;

        super(message);

        this.errno = err;
        this.code = code;
        this.syscall = syscall;
      }

      get ['constructor']() {
        return Error;
      }
    }

    class ExceptionWithHostPort extends Error {
      constructor(err, syscall, address, port, additional) {
        util ??= require('util');
        let code;
        let details = '';
        if (isPermissionModelError(err)) {
          code = err.code;
          details = ` ${err.message}`;
        } else {
          code = util.getSystemErrorName(err);
          if (port && port > 0) {
            details = ` ${address}:${port}`;
          } else if (address) {
            details = ` ${address}`;
          }
          if (additional) {
            details += ` - Local (${additional})`;
          }
        }
        super(`${syscall} ${code}${details}`);

        this.errno = err;
        this.code = code;
        this.syscall = syscall;
        this.address = address;
        if (port) {
          this.port = port;
        }
      }

      get ['constructor']() {
        return Error;
      }
    }

    class DNSException extends Error {
        constructor(code, syscall, hostname) {
        let errno;
        if (typeof code === 'number') {
          errno = code;
          if (code === lazyUv().UV_EAI_NODATA || code === lazyUv().UV_EAI_NONAME) {
            code = 'ENOTFOUND'; // Fabricated error name.
          } else {
            code = lazyInternalUtil().getSystemErrorName(code);
          }
        } else if (isPermissionModelError(code)) {
          code = code.code;
        }
        super(`${syscall} ${code}${hostname ? ` ${hostname}` : ''}`);
        this.errno = errno;
        this.code = code;
        this.syscall = syscall;
        if (hostname) {
          this.hostname = hostname;
        }
      }

      get ['constructor']() {
        return Error;
      }
    }

    class ConnResetException extends Error {
      constructor(msg, options) {
        super(msg, options);
        this.code = 'ECONNRESET';
      }

      get ['constructor']() {
        return Error;
      }
    }

    let maxStack_ErrorName;
    let maxStack_ErrorMessage;

    function isStackOverflowError(err) {
      if (maxStack_ErrorMessage === undefined) {
        try {
          function overflowStack() { overflowStack(); }
          overflowStack();
        } catch (err) {
          maxStack_ErrorMessage = err.message;
          maxStack_ErrorName = err.name;
        }
      }

      return err && err.name === maxStack_ErrorName &&
             err.message === maxStack_ErrorMessage;
    }

    function addNumericalSeparator(val) {
      let res = '';
      let i = val.length;
      const start = val[0] === '-' ? 1 : 0;
      for (; i >= start + 4; i -= 3) {
        res = `_${val.slice(i - 3, i)}${res}`;
      }
      return `${val.slice(0, i)}${res}`;
    }

    const kEnhanceStackBeforeInspector = Symbol('kEnhanceStackBeforeInspector');

    const fatalExceptionStackEnhancers = {
      beforeInspector(error) {
        if (typeof error[kEnhanceStackBeforeInspector] !== 'function') {
          return error.stack;
        }

        try {
          error.stack = error[kEnhanceStackBeforeInspector]();
        } catch {
        }
        return error.stack;
      },
      afterInspector(error) {
        const originalStack = error.stack;
        let useColors = true;
        if (isWindows) {
          const info = internalBinding('os').getOSInformation();
          const ver = (info[2].split('.', 3)).map(Number);
          if (ver[0] !== 10 || ver[2] < 14393) {
            useColors = false;
          }
        }
        const {
          inspect,
          inspectDefaultOptions: {
            colors: defaultColors,
          },
        } = lazyInternalUtilInspect();
        const colors = useColors && (lazyUtilColors().shouldColorize(process.stderr) || defaultColors);
        try {
          return inspect(error, {
            colors,
            customInspect: false,
            depth: Math.max(inspect.defaultOptions.depth, 5),
          });
        } catch {
          return originalStack;
        }
      },
    };

    const {
      privateSymbols: {
        arrow_message_private_symbol,
      },
    } = internalBinding('util');
    function setArrowMessage(err, arrowMessage) {
      err[arrow_message_private_symbol] = arrowMessage;
    }

    function hideInternalStackFrames(error) {
      overrideStackTrace.set(error, (error, stackFrames) => {
        let frames = stackFrames;
        if (typeof stackFrames === 'object') {
          frames = stackFrames.filter((frm) => !(frm.getFileName() || '').startsWith('node:internal'));
        }
        frames.unshift(error);
        return frames.join('\n    at ');
      });
    }

    class AbortError extends Error {
      constructor(message = 'The operation was aborted', options = undefined) {
        if (options !== undefined && typeof options !== 'object') {
          throw new codes.ERR_INVALID_ARG_TYPE('options', 'Object', options);
        }
        super(message, options);
        this.code = 'ABORT_ERR';
        this.name = 'AbortError';
      }
    }

    const genericNodeError = hideStackFrames(function genericNodeError(message, errorProperties) {
      const err = new Error(message);
      Object.assign(err, errorProperties);
      return err;
    });

    function determineSpecificType(value) {
      if (value === null) {
        return 'null';
      } else if (value === undefined) {
        return 'undefined';
      }

      const type = typeof value;

      switch (type) {
        case 'bigint':
          return `type bigint (${value}n)`;
        case 'number':
          if (value === 0) {
            return 1 / value === -Infinity ? 'type number (-0)' : 'type number (0)';
          } else if (value !== value) { // eslint-disable-line no-self-compare
            return 'type number (NaN)';
          } else if (value === Infinity) {
            return 'type number (Infinity)';
          } else if (value === -Infinity) {
            return 'type number (-Infinity)';
          }
          return `type number (${value})`;
        case 'boolean':
          return value ? 'type boolean (true)' : 'type boolean (false)';
        case 'symbol':
          return `type symbol (${String(value)})`;
        case 'function':
          return `function ${value.name}`;
        case 'object': {
          const name = value.constructor?.name;
          if (typeof name === 'string' && name !== '') {
            return `an instance of ${name}`;
          }
          return `${lazyInternalUtilInspect().inspect(value, { depth: -1 })}`;
        }
        case 'string':
          value.length > 28 && (value = `${value.slice(0, 25)}...`);
          if (value.indexOf("'") === -1) {
            return `type string ('${value}')`;
          }
          return `type string (${JSON.stringify(value)})`;
        default:
          value = lazyInternalUtilInspect().inspect(value, { colors: false });
          if (value.length > 28) {
            value = `${value.slice(0, 25)}...`;
          }

          return `type ${type} (${value})`;
      }
    }

    function formatList(array, type = 'and') {
      switch (array.length) {
        case 0: return '';
        case 1: return `${array[0]}`;
        case 2: return `${array[0]} ${type} ${array[1]}`;
        case 3: return `${array[0]}, ${array[1]}, ${type} ${array[2]}`;
        default:
          return `${array.slice(0, -1).join(', ')}, ${type} ${array[array.length - 1]}`;
      }
    }

    module.exports = {
      AbortError,
      aggregateTwoErrors,
      NodeAggregateError,
      codes,
      ConnResetException,
      DNSException,
      determineSpecificType,
      E,
      ErrnoException,
      ExceptionWithHostPort,
      fatalExceptionStackEnhancers,
      formatList,
      genericNodeError,
      getMessage,
      hideInternalStackFrames,
      hideStackFrames,
      inspectWithNoCustomRetry,
      isErrorStackTraceLimitWritable,
      isStackOverflowError,
      kEnhanceStackBeforeInspector,
      kIsNodeError,
      defaultPrepareStackTrace,
      setInternalPrepareStackTrace,
      overrideStackTrace,
      prepareStackTraceCallback,
      ErrorPrepareStackTrace,
      setArrowMessage,
      SystemError,
      uvErrmapGet,
      UVException,
      UVExceptionWithHostPort,
    };

    E('ERR_ACCESS_DENIED',
      function(msg, permission = '', resource = '') {
        this.permission = permission;
        this.resource = resource;
        return msg;
      },
      Error);
    E('ERR_AMBIGUOUS_ARGUMENT', 'The "%s" argument is ambiguous. %s', TypeError);
    E('ERR_ARG_NOT_ITERABLE', '%s must be iterable', TypeError);
    E('ERR_ASSERTION', '%s', Error);
    E('ERR_ASYNC_CALLBACK', '%s must be a function', TypeError);
    E('ERR_ASYNC_LOADER_REQUEST_NEVER_SETTLED',
      'Async loader request never settled', Error);
    E('ERR_ASYNC_TYPE', 'Invalid name for async "type": %s', TypeError);
    E('ERR_BROTLI_INVALID_PARAM', '%s is not a valid Brotli parameter', RangeError);
    E('ERR_BUFFER_OUT_OF_BOUNDS',
      (name = undefined) => {
        if (name) {
          return `"${name}" is outside of buffer bounds`;
        }
        return 'Attempt to access memory outside buffer bounds';
      }, RangeError);
    E('ERR_BUFFER_TOO_LARGE',
      'Cannot create a Buffer larger than %s bytes',
      RangeError);
    E('ERR_CANNOT_WATCH_SIGINT', 'Cannot watch for SIGINT signals', Error);
    E('ERR_CHILD_CLOSED_BEFORE_REPLY',
      'Child closed before reply received', Error);
    E('ERR_CHILD_PROCESS_IPC_REQUIRED',
      "Forked processes must have an IPC channel, missing value 'ipc' in %s",
      Error);
    E('ERR_CHILD_PROCESS_STDIO_MAXBUFFER', '%s maxBuffer length exceeded',
      RangeError);
    E('ERR_CONSOLE_WRITABLE_STREAM',
      'Console expects a writable stream instance for %s', TypeError);
    E('ERR_CONSTRUCT_CALL_REQUIRED', 'Class constructor %s cannot be invoked without `new`', TypeError);
    E('ERR_CONTEXT_NOT_INITIALIZED', 'context used is not initialized', Error);
    E('ERR_CRYPTO_ARGON2_NOT_SUPPORTED', 'Argon2 algorithm not supported', Error);
    E('ERR_CRYPTO_CUSTOM_ENGINE_NOT_SUPPORTED',
      'Custom engines not supported by this OpenSSL', Error);
    E('ERR_CRYPTO_ECDH_INVALID_FORMAT', 'Invalid ECDH format: %s', TypeError);
    E('ERR_CRYPTO_ECDH_INVALID_PUBLIC_KEY',
      'Public key is not valid for specified curve', Error);
    E('ERR_CRYPTO_ENGINE_UNKNOWN', 'Engine "%s" was not found', Error);
    E('ERR_CRYPTO_FIPS_FORCED',
      'Cannot set FIPS mode, it was forced with --force-fips at startup.', Error);
    E('ERR_CRYPTO_FIPS_UNAVAILABLE', 'Cannot set FIPS mode in a non-FIPS build.',
      Error);
    E('ERR_CRYPTO_HASH_FINALIZED', 'Digest already called', Error);
    E('ERR_CRYPTO_HASH_UPDATE_FAILED', 'Hash update failed', Error);
    E('ERR_CRYPTO_INCOMPATIBLE_KEY', 'Incompatible %s: %s', Error);
    E('ERR_CRYPTO_INCOMPATIBLE_KEY_OPTIONS', 'The selected key encoding %s %s.',
      Error);
    E('ERR_CRYPTO_INVALID_DIGEST', 'Invalid digest: %s', TypeError);
    E('ERR_CRYPTO_INVALID_JWK', 'Invalid JWK data', TypeError);
    E('ERR_CRYPTO_INVALID_KEY_OBJECT_TYPE',
      'Invalid key object type %s, expected %s.', TypeError);
    E('ERR_CRYPTO_INVALID_MAC', 'Invalid MAC: %s', TypeError);
    E('ERR_CRYPTO_INVALID_STATE', 'Invalid state for operation %s', Error);
    E('ERR_CRYPTO_KEM_NOT_SUPPORTED', 'KEM is not supported', Error);
    E('ERR_CRYPTO_MAC_FINALIZED', 'MAC already finalized', Error);
    E('ERR_CRYPTO_MAC_NOT_SUPPORTED', 'MAC is not supported', Error);
    E('ERR_CRYPTO_MAC_UPDATE_FAILED', 'MAC update failed', Error);
    E('ERR_CRYPTO_PBKDF2_ERROR', 'PBKDF2 error', Error);
    E('ERR_CRYPTO_SCRYPT_NOT_SUPPORTED', 'Scrypt algorithm not supported', Error);
    E('ERR_CRYPTO_SIGN_KEY_REQUIRED', 'No key provided to sign', Error);
    E('ERR_DEBUGGER_ERROR', '%s', Error);
    E('ERR_DEBUGGER_STARTUP_ERROR', function(message, details = undefined) {
      if (details !== undefined) {
        Object.assign(this, details);
      }
      return message;
    }, Error);
    E('ERR_DIR_CLOSED', 'Directory handle was closed', Error);
    E('ERR_DIR_CONCURRENT_OPERATION',
      'Cannot do synchronous work on directory handle with concurrent ' +
      'asynchronous operations', Error);
    E('ERR_DNS_SET_SERVERS_FAILED', 'c-ares failed to set servers: "%s" [%s]',
      Error);
    E('ERR_DOMAIN_CALLBACK_NOT_AVAILABLE',
      'A callback was registered through ' +
         'process.setUncaughtExceptionCaptureCallback(), which is mutually ' +
         'exclusive with using the `domain` module',
      Error);
    E('ERR_DOMAIN_CANNOT_SET_UNCAUGHT_EXCEPTION_CAPTURE',
      'The `domain` module is in use, which is mutually exclusive with calling ' +
         'process.setUncaughtExceptionCaptureCallback()',
      Error);
    E('ERR_DUPLICATE_STARTUP_SNAPSHOT_MAIN_FUNCTION',
      'Deserialize main function is already configured.', Error);
    E('ERR_ENCODING_INVALID_ENCODED_DATA', function(encoding, ret) {
      this.errno = ret;
      return `The encoded data was not valid for encoding ${encoding}`;
    }, TypeError);
    E('ERR_ENCODING_NOT_SUPPORTED', 'The "%s" encoding is not supported',
      RangeError);
    E('ERR_EVAL_ESM_CANNOT_PRINT', '--print cannot be used with ESM input', Error);
    E('ERR_EVENT_RECURSION', 'The event "%s" is already being dispatched', Error);
    E('ERR_FALSY_VALUE_REJECTION', function(reason) {
      this.reason = reason;
      return 'Promise was rejected with falsy value';
    }, Error, HideStackFramesError);
    E('ERR_FEATURE_UNAVAILABLE_ON_PLATFORM',
      'The feature %s is unavailable on the current platform' +
      ', which is being used to run Node.js',
      TypeError);
    E('ERR_FFI_LIBRARY_CLOSED', 'Library is closed', Error);
    E('ERR_FS_CP_DIR_TO_NON_DIR',
      'Cannot overwrite non-directory with directory', SystemError);
    E('ERR_FS_CP_EEXIST', 'Target already exists', SystemError);
    E('ERR_FS_CP_EINVAL', 'Invalid src or dest', SystemError);
    E('ERR_FS_CP_FIFO_PIPE', 'Cannot copy a FIFO pipe', SystemError);
    E('ERR_FS_CP_NON_DIR_TO_DIR',
      'Cannot overwrite directory with non-directory', SystemError);
    E('ERR_FS_CP_SOCKET', 'Cannot copy a socket file', SystemError);
    E('ERR_FS_CP_SYMLINK_TO_SUBDIRECTORY',
      'Cannot overwrite symlink in subdirectory of self', SystemError);
    E('ERR_FS_CP_UNKNOWN', 'Cannot copy an unknown file type', SystemError);
    E('ERR_FS_EISDIR', 'Path is a directory', SystemError, HideStackFramesError);
    E('ERR_FS_FILE_TOO_LARGE', 'File size (%s) is greater than 2 GiB', RangeError);
    E('ERR_FS_WATCH_QUEUE_OVERFLOW', 'fs.watch() queued more than %d events', Error);
    E('ERR_HTTP2_ALTSVC_INVALID_ORIGIN',
      'HTTP/2 ALTSVC frames require a valid origin', TypeError);
    E('ERR_HTTP2_ALTSVC_LENGTH',
      'HTTP/2 ALTSVC frames are limited to 16382 bytes', TypeError);
    E('ERR_HTTP2_CONNECT_AUTHORITY',
      ':authority header is required for CONNECT requests', Error);
    E('ERR_HTTP2_CONNECT_PATH',
      'The :path header is forbidden for CONNECT requests', Error);
    E('ERR_HTTP2_CONNECT_SCHEME',
      'The :scheme header is forbidden for CONNECT requests', Error);
    E('ERR_HTTP2_GOAWAY_SESSION',
      'New streams cannot be created after receiving a GOAWAY', Error);
    E('ERR_HTTP2_HEADERS_AFTER_RESPOND',
      'Cannot specify additional headers after response initiated', Error);
    E('ERR_HTTP2_HEADERS_SENT', 'Response has already been initiated.', Error);
    E('ERR_HTTP2_HEADER_SINGLE_VALUE',
      'Header field "%s" must only have a single value', TypeError);
    E('ERR_HTTP2_INFO_STATUS_NOT_ALLOWED',
      'Informational status codes cannot be used', RangeError);
    E('ERR_HTTP2_INVALID_CONNECTION_HEADERS',
      'HTTP/1 Connection specific headers are forbidden: "%s"', TypeError);
    E('ERR_HTTP2_INVALID_HEADER_VALUE',
      'Invalid value "%s" for header "%s"', TypeError, HideStackFramesError);
    E('ERR_HTTP2_INVALID_INFO_STATUS',
      'Invalid informational status code: %s', RangeError);
    E('ERR_HTTP2_INVALID_ORIGIN',
      'HTTP/2 ORIGIN frames require a valid origin', TypeError);
    E('ERR_HTTP2_INVALID_PACKED_SETTINGS_LENGTH',
      'Packed settings length must be a multiple of six', RangeError);
    E('ERR_HTTP2_INVALID_PSEUDOHEADER',
      '"%s" is an invalid pseudoheader or is used incorrectly', TypeError, HideStackFramesError);
    E('ERR_HTTP2_INVALID_SESSION', 'The session has been destroyed', Error);
    E('ERR_HTTP2_INVALID_SETTING_VALUE',
      function(name, actual, min = undefined, max = undefined) {
        this.actual = actual;
        if (min !== undefined) {
          this.min = min;
          this.max = max;
        }
        return `Invalid value for setting "${name}": ${actual}`;
      }, TypeError, RangeError, HideStackFramesError);
    E('ERR_HTTP2_INVALID_STREAM', 'The stream has been destroyed', Error);
    E('ERR_HTTP2_MAX_PENDING_SETTINGS_ACK',
      'Maximum number of pending settings acknowledgements', Error);
    E('ERR_HTTP2_NESTED_PUSH',
      'A push stream cannot initiate another push stream.', Error);
    E('ERR_HTTP2_NO_MEM', 'Out of memory', Error);
    E('ERR_HTTP2_NO_SOCKET_MANIPULATION',
      'HTTP/2 sockets should not be directly manipulated (e.g. read and written)',
      Error);
    E('ERR_HTTP2_ORIGIN_LENGTH',
      'HTTP/2 ORIGIN frames are limited to 16382 bytes', TypeError);
    E('ERR_HTTP2_OUT_OF_STREAMS',
      'No stream ID is available because maximum stream ID has been reached',
      Error);
    E('ERR_HTTP2_PAYLOAD_FORBIDDEN',
      'Responses with %s status must not have a payload', Error);
    E('ERR_HTTP2_PING_CANCEL', 'HTTP2 ping cancelled', Error);
    E('ERR_HTTP2_PING_LENGTH', 'HTTP2 ping payload must be 8 bytes', RangeError);
    E('ERR_HTTP2_PSEUDOHEADER_NOT_ALLOWED',
      'Cannot set HTTP/2 pseudo-headers', TypeError, HideStackFramesError);
    E('ERR_HTTP2_PUSH_DISABLED', 'HTTP/2 client has disabled push streams', Error);
    E('ERR_HTTP2_SEND_FILE', 'Directories cannot be sent', Error);
    E('ERR_HTTP2_SEND_FILE_NOSEEK',
      'Offset or length can only be specified for regular files', Error);
    E('ERR_HTTP2_SESSION_ERROR', 'Session closed with error code %s', Error);
    E('ERR_HTTP2_SETTINGS_CANCEL', 'HTTP2 session settings canceled', Error);
    E('ERR_HTTP2_SOCKET_BOUND',
      'The socket is already bound to an Http2Session', Error);
    E('ERR_HTTP2_SOCKET_UNBOUND',
      'The socket has been disconnected from the Http2Session', Error);
    E('ERR_HTTP2_STATUS_101',
      'HTTP status code 101 (Switching Protocols) is forbidden in HTTP/2', Error);
    E('ERR_HTTP2_STATUS_INVALID', 'Invalid status code: %s', RangeError);
    E('ERR_HTTP2_STREAM_CANCEL', function(error) {
      let msg = 'The pending stream has been canceled';
      if (error) {
        this.cause = error;
        if (typeof error.message === 'string')
          msg += ` (caused by: ${error.message})`;
      }
      return msg;
    }, Error);
    E('ERR_HTTP2_STREAM_ERROR', 'Stream closed with error code %s', Error);
    E('ERR_HTTP2_STREAM_SELF_DEPENDENCY',
      'A stream cannot depend on itself', Error);
    E('ERR_HTTP2_TOO_MANY_CUSTOM_SETTINGS',
      'Number of custom settings exceeds MAX_ADDITIONAL_SETTINGS', Error);
    E('ERR_HTTP2_TOO_MANY_INVALID_FRAMES', 'Too many invalid HTTP/2 frames', Error);
    E('ERR_HTTP2_TOO_MANY_ORIGINS',
      'The server sent more ORIGIN frames than the allowed number of %s', Error);
    E('ERR_HTTP2_TRAILERS_ALREADY_SENT',
      'Trailing headers have already been sent', Error);
    E('ERR_HTTP2_TRAILERS_NOT_READY',
      'Trailing headers cannot be sent until after the wantTrailers event is ' +
      'emitted', Error);
    E('ERR_HTTP2_UNSUPPORTED_PROTOCOL', 'protocol "%s" is unsupported.', Error);
    E('ERR_HTTP_BODY_NOT_ALLOWED',
      'Adding content for this request method or response status is not allowed.', Error);
    E('ERR_HTTP_CONTENT_LENGTH_MISMATCH',
      'Response body\'s content-length of %s byte(s) does not match the content-length of %s byte(s) set in header', Error);
    E('ERR_HTTP_HEADERS_SENT',
      'Cannot %s headers after they are sent to the client', Error);
    E('ERR_HTTP_INVALID_HEADER_VALUE',
      'Invalid value "%s" for header "%s"', TypeError, HideStackFramesError);
    E('ERR_HTTP_INVALID_STATUS_CODE', 'Invalid status code: %s', RangeError);
    E('ERR_HTTP_REQUEST_TIMEOUT', 'Request timeout', Error);
    E('ERR_HTTP_SOCKET_ASSIGNED',
      'ServerResponse has an already assigned socket', Error);
    E('ERR_HTTP_SOCKET_ENCODING',
      'Changing the socket encoding is not allowed per RFC7230 Section 3.', Error);
    E('ERR_HTTP_TRAILER_INVALID',
      'Trailers are invalid with this transfer encoding', Error);
    E('ERR_ILLEGAL_CONSTRUCTOR', 'Illegal constructor', TypeError);
    E('ERR_IMPORT_ATTRIBUTE_MISSING',
      'Module "%s" needs an import attribute of "%s: %s"', TypeError);
    E('ERR_IMPORT_ATTRIBUTE_TYPE_INCOMPATIBLE',
      'Module "%s" is not of type "%s"', TypeError);
    E('ERR_IMPORT_ATTRIBUTE_UNSUPPORTED',
      function error(attribute, value, url = undefined) {
        if (url === undefined) {
          return `Import attribute "${attribute}" with value "${value}" is not supported`;
        }
        return `Import attribute "${attribute}" with value "${value}" is not supported in ${url}`;
      }, TypeError);
    E('ERR_INCOMPATIBLE_OPTION_PAIR',
      'Option "%s" cannot be used in combination with option "%s"', TypeError, HideStackFramesError);
    E('ERR_INPUT_TYPE_NOT_ALLOWED', '--input-type can only be used with string ' +
      'input via --eval, --print, or STDIN', Error);
    E('ERR_INSPECTOR_ALREADY_ACTIVATED',
      'Inspector is already activated. Close it with inspector.close() ' +
      'before activating it again.',
      Error);
    E('ERR_INSPECTOR_ALREADY_CONNECTED', '%s is already connected', Error);
    E('ERR_INSPECTOR_CLOSED', 'Session was closed', Error);
    E('ERR_INSPECTOR_COMMAND', 'Inspector error %d: %s', Error);
    E('ERR_INSPECTOR_NOT_ACTIVE', 'Inspector is not active', Error);
    E('ERR_INSPECTOR_NOT_AVAILABLE', 'Inspector is not available', Error);
    E('ERR_INSPECTOR_NOT_CONNECTED', 'Session is not connected', Error);
    E('ERR_INSPECTOR_NOT_WORKER', 'Current thread is not a worker', Error);
    E('ERR_INTERNAL_ASSERTION', (message) => {
      const suffix = 'This is caused by either a bug in Node.js ' +
        'or incorrect usage of Node.js internals.\n' +
        'Please open an issue with this stack trace at ' +
        'https://github.com/nodejs/node/issues\n';
      return message === undefined ? suffix : `${message}\n${suffix}`;
    }, Error);
    E('ERR_INVALID_ADDRESS_FAMILY', function(addressType, host, port) {
      this.host = host;
      this.port = port;
      return `Invalid address family: ${addressType} ${host}:${port}`;
    }, RangeError);
    E('ERR_INVALID_ARG_TYPE',
      (name, expected, actual) => {
        assert(typeof name === 'string', "'name' must be a string");
        if (!Array.isArray(expected)) {
          expected = [expected];
        }

        let msg = 'The ';
        if (name.endsWith(' argument')) {
          msg += `${name} `;
        } else {
          const type = name.includes('.') ? 'property' : 'argument';
          msg += `"${name}" ${type} `;
        }
        msg += 'must be ';

        const types = [];
        const instances = [];
        const other = [];

        for (const value of expected) {
          assert(typeof value === 'string',
                 'All expected entries have to be of type string');
          if (kTypes.includes(value)) {
            types.push(value.toLowerCase());
          } else if (classRegExp.exec(value) !== null) {
            instances.push(value);
          } else {
            assert(value !== 'object',
                   'The value "object" should be written as "Object"');
            other.push(value);
          }
        }

        if (instances.length > 0) {
          const pos = types.indexOf('object');
          if (pos !== -1) {
            types.splice(pos, 1);
            instances.push('Object');
          }
        }

        if (types.length > 0) {
          msg += `${types.length > 1 ? 'one of type' : 'of type'} ${formatList(types, 'or')}`;
          if (instances.length > 0 || other.length > 0)
            msg += ' or ';
        }

        if (instances.length > 0) {
          msg += `an instance of ${formatList(instances, 'or')}`;
          if (other.length > 0)
            msg += ' or ';
        }

        if (other.length > 0) {
          if (other.length > 1) {
            msg += `one of ${formatList(other, 'or')}`;
          } else {
            if (other[0].toLowerCase() !== other[0])
              msg += 'an ';
            msg += `${other[0]}`;
          }
        }

        msg += `. Received ${determineSpecificType(actual)}`;

        return msg;
      }, TypeError, HideStackFramesError);
    E('ERR_INVALID_ARG_VALUE', (name, value, reason = 'is invalid') => {
      let inspected = lazyInternalUtilInspect().inspect(value);
      if (inspected.length > 128) {
        inspected = `${inspected.slice(0, 128)}...`;
      }
      const type = name.includes('.') ? 'property' : 'argument';
      return `The ${type} '${name}' ${reason}. Received ${inspected}`;
    }, TypeError, RangeError, HideStackFramesError);
    E('ERR_INVALID_ASYNC_ID', 'Invalid %s value: %s', RangeError);
    E('ERR_INVALID_BUFFER_SIZE',
      'Buffer size must be a multiple of %s', RangeError);
    E('ERR_INVALID_CHAR',
      (name, field = undefined) => {
        let msg = `Invalid character in ${name}`;
        if (field !== undefined) {
          msg += ` ["${field}"]`;
        }
        return msg;
      }, TypeError, HideStackFramesError);
    E('ERR_INVALID_CURSOR_POS',
      'Cannot set cursor row without setting its column', TypeError);
    E('ERR_INVALID_FD',
      '"fd" must be a positive integer: %s', RangeError);
    E('ERR_INVALID_FD_TYPE', 'Unsupported fd type: %s', TypeError);
    E('ERR_INVALID_FILE_URL_HOST',
      'File URL host must be "localhost" or empty on %s', TypeError);
    E('ERR_INVALID_FILE_URL_PATH', function(reason, input) {
      this.input = input;
      return `File URL path ${reason}`;
    }, TypeError);
    E('ERR_INVALID_HANDLE_TYPE', 'This handle type cannot be sent', TypeError);
    E('ERR_INVALID_HTTP_TOKEN', '%s must be a valid HTTP token ["%s"]', TypeError, HideStackFramesError);
    E('ERR_INVALID_IP_ADDRESS', 'Invalid IP address: %s', TypeError);
    E('ERR_INVALID_MIME_SYNTAX', (production, str, invalidIndex) => {
      const msg = invalidIndex !== -1 ? ` at ${invalidIndex}` : '';
      return `The MIME syntax for a ${production} in "${str}" is invalid` + msg;
    }, TypeError);
    E('ERR_INVALID_MODULE_SPECIFIER', (request, reason, base = undefined) => {
      return `Invalid module "${request}" ${reason}${base ?
        ` imported from ${base}` : ''}`;
    }, TypeError);
    E('ERR_INVALID_PACKAGE_CONFIG', (path, base, message) => {
      return `Invalid package config ${path}${base ? ` while importing ${base}` :
        ''}${message ? `. ${message}` : ''}`;
    }, Error);
    E('ERR_INVALID_PACKAGE_TARGET',
      (pkgPath, key, target, isImport = false, base = undefined) => {
        const relError = typeof target === 'string' && !isImport &&
          target.length && !target.startsWith('./');
        if (key === '.') {
          assert(isImport === false);
          return `Invalid "exports" main target ${JSON.stringify(target)} defined ` +
            `in the package config ${pkgPath}package.json${base ?
              ` imported from ${base}` : ''}${relError ?
              '; targets must start with "./"' : ''}`;
        }
        return `Invalid "${isImport ? 'imports' : 'exports'}" target ${
          JSON.stringify(target)} defined for '${key}' in the package config ${
          pkgPath}package.json${base ? ` imported from ${base}` : ''}${relError ?
          '; targets must start with "./"' : ''}`;
      }, Error);
    E('ERR_INVALID_PROTOCOL',
      'Protocol "%s" not supported. Expected "%s"',
      TypeError);
    E('ERR_INVALID_REPL_EVAL_CONFIG',
      'Cannot specify both "breakEvalOnSigint" and "eval" for REPL', TypeError);
    E('ERR_INVALID_REPL_INPUT', '%s', TypeError);
    E('ERR_INVALID_RETURN_PROPERTY', (input, name, prop, value) => {
      return `Expected a valid ${input} to be returned for the "${prop}" from the` +
             ` "${name}" hook but got ${determineSpecificType(value)}.`;
    }, TypeError);
    E('ERR_INVALID_RETURN_PROPERTY_VALUE', (input, name, prop, value) => {
      return `Expected ${input} to be returned for the "${prop}" from the` +
             ` "${name}" hook but got ${determineSpecificType(value)}.`;
    }, TypeError);
    E('ERR_INVALID_RETURN_VALUE', (input, name, value) => {
      const type = determineSpecificType(value);

      return `Expected ${input} to be returned from the "${name}"` +
             ` function but got ${type}.`;
    }, TypeError, RangeError);
    E('ERR_INVALID_STATE', 'Invalid state: %s', Error, TypeError, RangeError);
    E('ERR_INVALID_SYNC_FORK_INPUT',
      'Asynchronous forks do not support ' +
        'Buffer, TypedArray, DataView or string input: %s',
      TypeError);
    E('ERR_INVALID_THIS', 'Value of "this" must be of type %s', TypeError, HideStackFramesError);
    E('ERR_INVALID_TUPLE', '%s must be an iterable %s tuple', TypeError);
    E('ERR_INVALID_TYPESCRIPT_SYNTAX', '%s', SyntaxError);
    E('ERR_INVALID_URI', 'URI malformed', URIError);
    E('ERR_INVALID_URL', function(input, base = null) {
      this.input = input;

      if (base != null) {
        this.base = base;
      }

      return 'Invalid URL';
    }, TypeError);
    E('ERR_INVALID_URL_SCHEME',
      (expected) => {
        if (typeof expected === 'string')
          expected = [expected];
        assert(expected.length <= 2);
        const res = expected.length === 2 ?
          `one of scheme ${expected[0]} or ${expected[1]}` :
          `of scheme ${expected[0]}`;
        return `The URL must be ${res}`;
      }, TypeError);
    E('ERR_IPC_CHANNEL_CLOSED', 'Channel closed', Error);
    E('ERR_IPC_DISCONNECTED', 'IPC channel is already disconnected', Error);
    E('ERR_IPC_ONE_PIPE', 'Child process can have only one IPC pipe', Error);
    E('ERR_IPC_SYNC_FORK', 'IPC cannot be used with synchronous forks', Error);
    E('ERR_IP_BLOCKED', function(ip) {
      return `IP(${ip}) is blocked by net.BlockList`;
    }, Error);
    E(
      'ERR_LOADER_CHAIN_INCOMPLETE',
      '"%s" did not call the next hook in its chain and did not' +
      ' explicitly signal a short circuit. If this is intentional, include' +
      ' `shortCircuit: true` in the hook\'s return.',
      Error,
    );
    E('ERR_METHOD_NOT_IMPLEMENTED', 'The %s method is not implemented', Error);
    E('ERR_MISSING_ARGS',
      (...args) => {
        assert(args.length > 0, 'At least one arg needs to be specified');
        let msg = 'The ';
        const len = args.length;
        const wrap = (a) => `"${a}"`;
        args = args.map((a) => (Array.isArray(a) ?
            a.map(wrap).join(' or ') :
            wrap(a)));
        msg += `${formatList(args)} argument${len > 1 ? 's' : ''}`;
        return `${msg} must be specified`;
      }, TypeError);
    E('ERR_MISSING_OPTION', '%s is required', TypeError);
    E('ERR_MODULE_LINK_MISMATCH', '%s', TypeError);
    E('ERR_MODULE_NOT_FOUND', function(path, base, exactUrl) {
      if (exactUrl) {
        lazyInternalUtil().setOwnProperty(this, 'url', `${exactUrl}`);
      }
      return `Cannot find ${
        exactUrl ? 'module' : 'package'} '${path}' imported from ${base}`;
    }, Error);
    E('ERR_MULTIPLE_CALLBACK', 'Callback called multiple times', Error);
    E('ERR_NAPI_CONS_FUNCTION', 'Constructor must be a function', TypeError);
    E('ERR_NAPI_INVALID_DATAVIEW_ARGS',
      'byte_offset + byte_length should be less than or equal to the size in ' +
        'bytes of the array passed in',
      RangeError);
    E('ERR_NAPI_INVALID_TYPEDARRAY_ALIGNMENT',
      'start offset of %s should be a multiple of %s', RangeError);
    E('ERR_NAPI_INVALID_TYPEDARRAY_LENGTH',
      'Invalid typed array length', RangeError);
    E('ERR_NOT_BUILDING_SNAPSHOT',
      'Operation cannot be invoked when not building startup snapshot', Error);
    E('ERR_NOT_IN_SINGLE_EXECUTABLE_APPLICATION',
      'Operation cannot be invoked when not in a single-executable application', Error);
    E('ERR_NOT_SUPPORTED_IN_SNAPSHOT', '%s is not supported in startup snapshot', Error);
    E('ERR_NO_CRYPTO',
      'Node.js is not compiled with OpenSSL crypto support', Error);
    E('ERR_NO_ICU',
      '%s is not supported on Node.js compiled without ICU', TypeError);
    E('ERR_NO_TEMPORAL',
      'Temporal is not supported in this environment', Error);
    E('ERR_NO_TYPESCRIPT',
      'Node.js is not compiled with TypeScript support', Error);
    E('ERR_OPERATION_FAILED', 'Operation failed: %s', Error, TypeError);
    E('ERR_OUT_OF_RANGE',
      (str, range, input, replaceDefaultBoolean = false) => {
        assert(range, 'Missing "range" argument');
        let msg = replaceDefaultBoolean ? str :
          `The value of "${str}" is out of range.`;
        let received;
        if (Number.isInteger(input) && Math.abs(input) > 2 ** 32) {
          received = addNumericalSeparator(String(input));
        } else if (typeof input === 'bigint') {
          received = String(input);
          if (input > 2n ** 32n || input < -(2n ** 32n)) {
            received = addNumericalSeparator(received);
          }
          received += 'n';
        } else {
          received = lazyInternalUtilInspect().inspect(input);
        }
        msg += ` It must be ${range}. Received ${received}`;
        return msg;
      }, RangeError, HideStackFramesError);
    E('ERR_PACKAGE_IMPORT_NOT_DEFINED', (specifier, packagePath, base) => {
      return `Package import specifier "${specifier}" is not defined${packagePath ?
        ` in package ${packagePath}package.json` : ''} imported from ${base}`;
    }, TypeError);
    E('ERR_PACKAGE_MAP_EXTERNAL_FILE', (specifier, parentPath, configPath) => {
      return `Cannot resolve "${specifier}" from "${parentPath}": file is not within any package defined in ${configPath}`;
    }, Error);
    E('ERR_PACKAGE_MAP_INVALID', (configPath, reason) => {
      return `Invalid package-map.json at "${configPath}": ${reason}`;
    }, SyntaxError);
    E('ERR_PACKAGE_MAP_KEY_NOT_FOUND', (key, configPath) => {
      return `Package key "${key}" referenced in dependencies but not defined in ${configPath}`;
    }, Error);
    E('ERR_PACKAGE_PATH_NOT_EXPORTED', (pkgPath, subpath, base = undefined) => {
      if (subpath === '.')
        return `No "exports" main defined in ${pkgPath}package.json${base ?
          ` imported from ${base}` : ''}`;
      return `Package subpath '${subpath}' is not defined by "exports" in ${
        pkgPath}package.json${base ? ` imported from ${base}` : ''}`;
    }, Error);
    E('ERR_PARSE_ARGS_INVALID_OPTION_VALUE', '%s', TypeError);
    E('ERR_PARSE_ARGS_UNEXPECTED_POSITIONAL', "Unexpected argument '%s'. This " +
      'command does not take positional arguments', TypeError);
    E('ERR_PARSE_ARGS_UNKNOWN_OPTION', (option, allowPositionals) => {
      const suggestDashDash = allowPositionals ? '. To specify a positional ' +
        "argument starting with a '-', place it at the end of the command after " +
        `'--', as in '-- ${JSON.stringify(option)}` : '';
      return `Unknown option '${option}'${suggestDashDash}`;
    }, TypeError);
    E('ERR_PERFORMANCE_INVALID_TIMESTAMP',
      '%d is not a valid timestamp', TypeError);
    E('ERR_PERFORMANCE_MEASURE_INVALID_OPTIONS', '%s', TypeError);
    E('ERR_PROXY_INVALID_CONFIG', '%s', Error);
    E('ERR_PROXY_TUNNEL', '%s', Error);
    E('ERR_QUIC_CONNECTION_FAILED', 'QUIC connection failed', Error);
    E('ERR_QUIC_ENDPOINT_CLOSED', 'QUIC endpoint closed: %s (%d)', Error);
    E('ERR_QUIC_OPEN_STREAM_FAILED', 'Failed to open QUIC stream', Error);
    E('ERR_QUIC_STREAM_ABORTED', '%s', Error);
    E('ERR_QUIC_VERSION_NEGOTIATION_ERROR', 'The QUIC session requires version negotiation', Error);
    E('ERR_REQUIRE_ASYNC_MODULE', function(filename, parent, locations) {
      let message = 'require() cannot be used on an ESM graph with top-level await. Use import() instead.';
      const { getOptionValue } = require('internal/options');
      if (!getOptionValue('--experimental-print-required-tla')) {
        message += ' To see where the top-level await comes from, use --experimental-print-required-tla.';
      }
      if (filename) {
        message += `\nRequired module: ${filename}`;
      }
      if (parent) {
        const { getRequireStack } = require('internal/modules/helpers');
        const requireStack = getRequireStack(parent);
        if (requireStack.length > 0) {
          message += '\nRequire stack:\n- ' +
                    requireStack.join('\n- ');
        }
        Object.defineProperty(this, 'requireStack', {
          __proto__: null,
          enumerable: false,
          configurable: true,
          writable: true,
          value: requireStack,
        });
      }
      if (locations && locations.length > 0) {
        const { urlToFilename } = require('internal/modules/helpers');
        const frames = locations.map(({ url, line, column, sourceLine }) =>
          `${urlToFilename(url)}:${line}\n\n${sourceLine}\n${' '.repeat(column - 1)}^\n`);
        setArrowMessage(this, frames.join('\n'));
        Object.defineProperty(this, 'topLevelAwaitLocations', {
          __proto__: null,
          enumerable: false,
          configurable: true,
          writable: true,
          value: locations,
        });
      }
      return message;
    }, Error);
    E('ERR_REQUIRE_CYCLE_MODULE', '%s', Error);
    E('ERR_REQUIRE_ESM',
      function(filename, hasEsmSyntax, parentPath = null, packageJsonPath = null) {
        hideInternalStackFrames(this);
        let msg = `require() of ES Module ${filename}${parentPath ? ` from ${
          parentPath}` : ''} not supported.`;
        if (!packageJsonPath) {
          if (filename.endsWith('.mjs'))
            msg += `\nInstead change the require of ${filename} to a dynamic ` +
                'import() which is available in all CommonJS modules.';
          return msg;
        }
        const path = require('path');
        const basename = parentPath && path.basename(filename) ===
          path.basename(parentPath) ? filename : path.basename(filename);
        if (hasEsmSyntax) {
          msg += `\nInstead change the require of ${basename} in ${parentPath} to` +
            ' a dynamic import() which is available in all CommonJS modules.';
          return msg;
        }
        msg += `\n${basename} is treated as an ES module file as it is a .js ` +
          'file whose nearest parent package.json contains "type": "module" ' +
          'which declares all .js files in that package scope as ES modules.' +
          `\nInstead either rename ${basename} to end in .cjs, change the requiring ` +
          'code to use dynamic import() which is available in all CommonJS ' +
          'modules, or change "type": "module" to "type": "commonjs" in ' +
          `${packageJsonPath} to treat all .js files as CommonJS (using .mjs for ` +
          'all ES modules instead).\n';
        return msg;
      }, Error);
    E('ERR_REQUIRE_ESM_RACE_CONDITION', (filename, parentFilename, isForAsyncLoaderHookWorker) => {
      let raceMessage = `Cannot require() ES Module ${filename} because it is not yet fully loaded.\n`;
      raceMessage += 'This may be caused by a race condition if the module is simultaneously dynamically ';
      raceMessage += 'import()-ed via Promise.all().\n';
      raceMessage += 'Try await-ing the import() sequentially in a loop instead.\n';
      raceMessage += ` (From ${parentFilename ? `${parentFilename} in ` : ' '}`;
      raceMessage += `${isForAsyncLoaderHookWorker ? 'loader hook worker thread' : 'non-loader-hook thread'})`;
      return raceMessage;
    }, Error);
    E('ERR_SCRIPT_EXECUTION_INTERRUPTED',
      'Script execution was interrupted by `SIGINT`', Error);
    E('ERR_SERVER_ALREADY_LISTEN',
      'Listen method has been called more than once without closing.', Error);
    E('ERR_SERVER_NOT_RUNNING', 'Server is not running.', Error);
    E('ERR_SINGLE_EXECUTABLE_APPLICATION_ASSET_NOT_FOUND',
      'Cannot find asset %s for the single executable application', Error);
    E('ERR_SOCKET_ALREADY_BOUND', 'Socket is already bound', Error);
    E('ERR_SOCKET_BAD_BUFFER_SIZE',
      'Buffer size must be a positive integer', TypeError);
    E('ERR_SOCKET_BAD_PORT', (name, port, allowZero = true) => {
      assert(typeof allowZero === 'boolean',
             "The 'allowZero' argument must be of type boolean.");
      const operator = allowZero ? '>=' : '>';
      return `${name} should be ${operator} 0 and < 65536. Received ${determineSpecificType(port)}.`;
    }, RangeError, HideStackFramesError);
    E('ERR_SOCKET_BAD_TYPE',
      'Bad socket type specified. Valid types are: udp4, udp6', TypeError);
    E('ERR_SOCKET_BUFFER_SIZE',
      'Could not get or set buffer size',
      SystemError);
    E('ERR_SOCKET_CLOSED', 'Socket is closed', Error);
    E('ERR_SOCKET_CLOSED_BEFORE_CONNECTION',
      'Socket closed before the connection was established',
      Error);
    E('ERR_SOCKET_CONNECTION_TIMEOUT',
      'Socket connection timeout', Error);
    E('ERR_SOCKET_DGRAM_IS_CONNECTED', 'Already connected', Error);
    E('ERR_SOCKET_DGRAM_NOT_CONNECTED', 'Not connected', Error);
    E('ERR_SOCKET_DGRAM_NOT_RUNNING', 'Not running', Error);
    E('ERR_SOCKET_HANDLE_ADOPTED',
      'The bound socket has already been adopted by a server or socket', Error);
    E('ERR_SOURCE_MAP_CORRUPT', `The source map for '%s' does not exist or is corrupt.`, Error);
    E('ERR_SOURCE_MAP_MISSING_SOURCE', `Cannot find '%s' imported from the source map for '%s'`, Error);
    E('ERR_SRI_PARSE',
      'Subresource Integrity string %j had an unexpected %j at position %d',
      SyntaxError);
    E('ERR_STREAM_ALREADY_FINISHED',
      'Cannot call %s after a stream was finished',
      Error);
    E('ERR_STREAM_CANNOT_PIPE', 'Cannot pipe, not readable', Error);
    E('ERR_STREAM_DESTROYED', 'Cannot call %s after a stream was destroyed', Error);
    E('ERR_STREAM_ITER_MISSING_FLAG',
      'The stream/iter API requires the --experimental-stream-iter flag', TypeError);
    E('ERR_STREAM_NULL_VALUES', 'May not write null values to stream', TypeError);
    E('ERR_STREAM_PREMATURE_CLOSE', 'Premature close', Error);
    E('ERR_STREAM_PUSH_AFTER_EOF', 'stream.push() after EOF', Error);
    E('ERR_STREAM_UNABLE_TO_PIPE', 'Cannot pipe to a closed or destroyed stream', Error);
    E('ERR_STREAM_UNSHIFT_AFTER_END_EVENT',
      'stream.unshift() after end event', Error);
    E('ERR_STREAM_WRAP', 'Stream has StringDecoder set or is in objectMode', Error);
    E('ERR_STREAM_WRITE_AFTER_END', 'write after end', Error);
    E('ERR_SYNTHETIC', 'JavaScript Callstack', Error);
    E('ERR_SYSTEM_ERROR', 'A system error occurred', SystemError, HideStackFramesError);
    E('ERR_TEST_FAILURE', function(error, failureType) {
      hideInternalStackFrames(this);
      assert(typeof failureType === 'string' || typeof failureType === 'symbol',
             "The 'failureType' argument must be of type string or symbol.");

      let msg = error?.message ?? error;

      if (typeof msg !== 'string') {
        msg = inspectWithNoCustomRetry(msg);
      }

      this.failureType = failureType;
      this.cause = error;
      return msg;
    }, Error);
    E('ERR_THROTTLED', 'The call was dropped by the throttled function', Error);
    E('ERR_TLS_ALPN_CALLBACK_INVALID_RESULT', (value, protocols) => {
      return `ALPN callback returned a value (${
        value
      }) that did not match any of the client's offered protocols (${
        protocols.join(', ')
      })`;
    }, TypeError);
    E('ERR_TLS_ALPN_CALLBACK_WITH_PROTOCOLS',
      'The ALPNCallback and ALPNProtocols TLS options are mutually exclusive',
      TypeError);
    E('ERR_TLS_CERT_ALTNAME_FORMAT', 'Invalid subject alternative name string',
      SyntaxError);
    E('ERR_TLS_CERT_ALTNAME_INVALID', function(reason, host, cert) {
      this.reason = reason;
      this.host = host;
      this.cert = cert;
      return `Hostname/IP does not match certificate's altnames: ${reason}`;
    }, Error);
    E('ERR_TLS_DH_PARAM_SIZE', 'DH parameter size %s is less than 2048', Error);
    E('ERR_TLS_HANDSHAKE_TIMEOUT', 'TLS handshake timeout', Error);
    E('ERR_TLS_INVALID_CONTEXT', '%s must be a SecureContext', TypeError);
    E('ERR_TLS_INVALID_PROTOCOL_VERSION',
      '%j is not a valid %s TLS protocol version', TypeError);
    E('ERR_TLS_INVALID_STATE', 'TLS socket connection must be securely established',
      Error);
    E('ERR_TLS_PROTOCOL_VERSION_CONFLICT',
      'TLS protocol version %j conflicts with secureProtocol %j', TypeError);
    E('ERR_TLS_RENEGOTIATION_DISABLED',
      'TLS session renegotiation disabled for this socket', Error);
    E('ERR_TLS_RENEGOTIATION_UNSUPPORTED',
      'TLS session renegotiation is unsupported by this TLS implementation', Error);

    E('ERR_TLS_REQUIRED_SERVER_NAME',
      '"servername" is required parameter for Server.addContext', Error);
    E('ERR_TLS_SESSION_ATTACK', 'TLS session renegotiation attack detected', Error);
    E('ERR_TLS_SNI_FROM_SERVER',
      'Cannot issue SNI from a TLS server-side socket', Error);
    E('ERR_TRACE_EVENTS_CATEGORY_REQUIRED',
      'At least one category is required', TypeError);
    E('ERR_TRACE_EVENTS_UNAVAILABLE', 'Trace events are unavailable', Error);

    E('ERR_TRAILING_JUNK_AFTER_STREAM_END', 'Trailing junk found after the end of the compressed stream', TypeError);

    E('ERR_TTY_INIT_FAILED', 'TTY initialization failed', SystemError);
    E('ERR_UNAVAILABLE_DURING_EXIT', 'Cannot call function in process exit ' +
      'handler', Error);
    E('ERR_UNCAUGHT_EXCEPTION_CAPTURE_ALREADY_SET',
      '`process.setupUncaughtExceptionCapture()` was called while a capture ' +
        'callback was already active',
      Error);
    E('ERR_UNESCAPED_CHARACTERS', '%s contains unescaped characters', TypeError);
    E('ERR_UNHANDLED_ERROR',
      (err = undefined) => {
        const msg = 'Unhandled error.';
        if (err === undefined) return msg;
        return `${msg} (${err})`;
      }, Error);
    E('ERR_UNKNOWN_BUILTIN_MODULE', 'No such built-in module: %s', Error);
    E('ERR_UNKNOWN_CREDENTIAL', '%s identifier does not exist: %s', Error);
    E('ERR_UNKNOWN_ENCODING', 'Unknown encoding: %s', TypeError);
    E('ERR_UNKNOWN_FILE_EXTENSION', 'Unknown file extension "%s" for %s', TypeError);
    E('ERR_UNKNOWN_MODULE_FORMAT', 'Unknown module format: %s for URL %s',
      RangeError);
    E('ERR_UNKNOWN_SIGNAL', 'Unknown signal: %s', TypeError, HideStackFramesError);
    E('ERR_UNSUPPORTED_DIR_IMPORT', function(path, base, exactUrl) {
      lazyInternalUtil().setOwnProperty(this, 'url', exactUrl);
      return `Directory import '${path}' is not supported ` +
        `resolving ES modules imported from ${base}`;
    }, Error);
    E('ERR_UNSUPPORTED_ESM_URL_SCHEME', (url, supported) => {
      let msg = `Only URLs with a scheme in: ${formatList(supported)} are supported by the default ESM loader`;
      if (isWindows && url.protocol.length === 2) {
        msg +=
          '. On Windows, absolute paths must be valid file:// URLs';
      }
      msg += `. Received protocol '${url.protocol}'`;
      return msg;
    }, Error);
    E('ERR_UNSUPPORTED_NODE_MODULES_TYPE_STRIPPING',
      'Stripping types is currently unsupported for files under node_modules, for "%s"',
      Error);
    E('ERR_UNSUPPORTED_RESOLVE_REQUEST',
      'Failed to resolve module specifier "%s" from "%s": Invalid relative URL or base scheme is not hierarchical.',
      TypeError);
    E('ERR_UNSUPPORTED_TYPESCRIPT_SYNTAX', '%s', SyntaxError);
    E('ERR_USE_AFTER_CLOSE', '%s was closed', Error);

    E('ERR_VALID_PERFORMANCE_ENTRY_TYPE',
      'At least one valid performance entry type is required', Error);
    E('ERR_VFS_INVALID_TARGET',
      '%s is not a valid --vfs-load source: must be an existing file or directory', Error);
    E('ERR_VM_DYNAMIC_IMPORT_CALLBACK_MISSING',
      'A dynamic import callback was not specified.', TypeError);
    E('ERR_VM_DYNAMIC_IMPORT_CALLBACK_MISSING_FLAG',
      'A dynamic import callback was invoked without --experimental-vm-modules',
      TypeError);
    E('ERR_VM_MODULE_ALREADY_LINKED', 'Module has already been linked', Error);
    E('ERR_VM_MODULE_CANNOT_CREATE_CACHED_DATA',
      'Cached data cannot be created for a module which has been evaluated', Error);
    E('ERR_VM_MODULE_DIFFERENT_CONTEXT',
      'Linked modules must use the same context', Error);
    E('ERR_VM_MODULE_LINK_FAILURE', function(message, cause) {
      this.cause = cause;
      return message;
    }, Error);
    E('ERR_VM_MODULE_NOT_MODULE',
      'Provided module is not an instance of Module', Error);
    E('ERR_VM_MODULE_STATUS', 'Module status %s', Error);
    E('ERR_WASI_ALREADY_STARTED', 'WASI instance has already started', Error);
    E('ERR_WEBASSEMBLY_NOT_SUPPORTED',
      'WebAssembly is not supported in this environment, but is required for %s',
      Error);
    E('ERR_WEBASSEMBLY_RESPONSE', 'WebAssembly response %s', TypeError);
    E('ERR_WORKER_HANDLE_NOT_TRANSFERABLE',
      '%s cannot be transferred in its current state; it must be a freshly ' +
      'created or accepted handle that has not started reading and has no ' +
      'pending writes', Error);
    E('ERR_WORKER_INIT_FAILED', 'Worker initialization failure: %s', Error);
    E('ERR_WORKER_INVALID_EXEC_ARGV', (errors, msg = 'invalid execArgv flags') =>
      `Initiated Worker with ${msg}: ${errors.join(', ')}`,
      Error);
    E('ERR_WORKER_MESSAGING_ERRORED', 'The destination thread threw an error while processing the message', Error);
    E('ERR_WORKER_MESSAGING_FAILED', 'Cannot find the destination thread or listener', Error);
    E('ERR_WORKER_MESSAGING_SAME_THREAD', 'Cannot sent a message to the same thread', Error);
    E('ERR_WORKER_MESSAGING_TIMEOUT', 'Sending a message to another thread timed out', Error);
    E('ERR_WORKER_NOT_RUNNING', 'Worker instance not running', Error);
    E('ERR_WORKER_OUT_OF_MEMORY',
      'Worker terminated due to reaching memory limit: %s', Error);
    E('ERR_WORKER_PATH', (filename) =>
      'The worker script or module filename must be an absolute path or a ' +
      'relative path starting with \'./\' or \'../\'.' +
      (filename.startsWith('file://') ?
        ' Wrap file:// URLs with `new URL`.' : ''
      ) +
      (filename.startsWith('data:text/javascript') ?
        ' Wrap data: URLs with `new URL`.' : ''
      ) +
      ` Received "${filename}"`,
      TypeError);
    E('ERR_WORKER_UNSERIALIZABLE_ERROR',
      'Serializing an uncaught exception failed', Error);
    E('ERR_WORKER_UNSUPPORTED_OPERATION',
      '%s is not supported in workers', TypeError);
    E('ERR_ZIP_ARCHIVE_TOO_LARGE', 'ZIP archive structure exceeds the allowed size: %s', RangeError);
    E('ERR_ZIP_ENTRY_CORRUPT', 'ZIP entry is corrupt: %s', Error);
    E('ERR_ZIP_ENTRY_NOT_FOUND', 'no such entry %j in the archive', Error);
    E('ERR_ZIP_ENTRY_TOO_LARGE', 'ZIP entry exceeds the allowed size: %s', RangeError);
    E('ERR_ZIP_INVALID_ARCHIVE', 'invalid ZIP archive: %s', Error);
    E('ERR_ZIP_NOT_WRITABLE', 'this archive was not opened for writing', TypeError);
    E('ERR_ZIP_UNSUPPORTED_FEATURE', 'unsupported ZIP feature: %s', Error);
    E('ERR_ZSTD_INVALID_PARAM', '%s is not a valid zstd parameter', RangeError);
  },
  "internal/validators": function (module, exports, require, process, primordials, internalBinding) {
    /* eslint jsdoc/require-jsdoc: "error" */

    'use strict';

    const {
      ArrayIsArray,
      ArrayPrototypeIncludes,
      ArrayPrototypeJoin,
      ArrayPrototypeMap,
      NumberIsFinite,
      NumberIsInteger,
      NumberIsNaN,
      NumberMAX_SAFE_INTEGER,
      NumberMIN_SAFE_INTEGER,
      NumberParseInt,
      ObjectPrototypeHasOwnProperty,
      RegExpPrototypeExec,
      String,
      StringPrototypeIncludes,
      StringPrototypeToUpperCase,
      StringPrototypeTrim,
    } = primordials;

    const {
      codes: {
        ERR_INVALID_ARG_TYPE: { HideStackFramesError: ERR_INVALID_ARG_TYPE },
        ERR_INVALID_ARG_VALUE: { HideStackFramesError: ERR_INVALID_ARG_VALUE },
        ERR_INVALID_THIS: { HideStackFramesError: ERR_INVALID_THIS },
        ERR_OUT_OF_RANGE: { HideStackFramesError: ERR_OUT_OF_RANGE },
        ERR_SOCKET_BAD_PORT: { HideStackFramesError: ERR_SOCKET_BAD_PORT },
        ERR_UNKNOWN_SIGNAL: { HideStackFramesError: ERR_UNKNOWN_SIGNAL },
      },
      hideStackFrames,
    } = require('internal/errors');
    const { normalizeEncoding } = require('internal/util');
    const {
      isAsyncFunction,
      isArrayBufferView,
      isRegExp,
    } = require('internal/util/types');
    const { signals } = internalBinding('constants').os;

    function isInt32(value) {
      return value === (value | 0);
    }

    function isUint32(value) {
      return value === (value >>> 0);
    }

    const octalReg = /^[0-7]+$/;
    const modeDesc = 'must be a 32-bit unsigned integer or an octal string';

    function parseFileMode(value, name, def) {
      value ??= def;
      if (typeof value === 'string') {
        if (octalReg.exec(value) === null) {
          throw new ERR_INVALID_ARG_VALUE(name, value, modeDesc);
        }
        value = Number.parseInt(value, 8);
      }

      validateUint32(value, name);
      return value + 0;
    }

    const validateInteger = hideStackFrames(
      (value, name, min = NumberMIN_SAFE_INTEGER, max = NumberMAX_SAFE_INTEGER) => {
        if (typeof value !== 'number')
          throw new ERR_INVALID_ARG_TYPE(name, 'number', value);
        if (!Number.isInteger(value))
          throw new ERR_OUT_OF_RANGE(name, 'an integer', value);
        if (value < min || value > max)
          throw new ERR_OUT_OF_RANGE(name, `>= ${min} && <= ${max}`, value);
      },
    );

    const validateInt32 = hideStackFrames(
      (value, name, min = -2147483648, max = 2147483647) => {
        if (typeof value !== 'number') {
          throw new ERR_INVALID_ARG_TYPE(name, 'number', value);
        }
        if (!Number.isInteger(value)) {
          throw new ERR_OUT_OF_RANGE(name, 'an integer', value);
        }
        if (value < min || value > max) {
          throw new ERR_OUT_OF_RANGE(name, `>= ${min} && <= ${max}`, value);
        }
      },
    );

    const validateUint32 = hideStackFrames((value, name, positive = false) => {
      if (typeof value !== 'number') {
        throw new ERR_INVALID_ARG_TYPE(name, 'number', value);
      }
      if (!Number.isInteger(value)) {
        throw new ERR_OUT_OF_RANGE(name, 'an integer', value);
      }
      const min = positive ? 1 : 0;
      const max = 4_294_967_295;
      if (value < min || value > max) {
        throw new ERR_OUT_OF_RANGE(name, `>= ${min} && <= ${max}`, value);
      }
    });

    const validateString = hideStackFrames((value, name) => {
      if (typeof value !== 'string')
        throw new ERR_INVALID_ARG_TYPE(name, 'string', value);
    });

    const validateStringWithoutNullBytes = hideStackFrames((value, name) => {
      validateString(value, name);
      if (value.includes('\u0000')) {
        throw new ERR_INVALID_ARG_VALUE(name, value, 'must be a string without null bytes');
      }
    });

    const validateNumber = hideStackFrames((value, name, min = undefined, max) => {
      if (typeof value !== 'number')
        throw new ERR_INVALID_ARG_TYPE(name, 'number', value);

      if ((min != null && value < min) || (max != null && value > max) ||
        ((min != null || max != null) && Number.isNaN(value))) {
        throw new ERR_OUT_OF_RANGE(
          name,
          `${min != null ? `>= ${min}` : ''}${min != null && max != null ? ' && ' : ''}${max != null ? `<= ${max}` : ''}`,
          value);
      }
    });

    const validateOneOf = hideStackFrames((value, name, oneOf) => {
      if (!oneOf.includes(value)) {
        const allowed = (oneOf.map((v) =>
            (typeof v === 'string' ? `'${v}'` : String(v)))).join(', ');
        const reason = 'must be one of: ' + allowed;
        throw new ERR_INVALID_ARG_VALUE(name, value, reason);
      }
    });

    const validateBoolean = hideStackFrames((value, name) => {
      if (typeof value !== 'boolean')
        throw new ERR_INVALID_ARG_TYPE(name, 'boolean', value);
    });

    const kValidateObjectNone = 0;
    const kValidateObjectAllowNullable = 1 << 0;
    const kValidateObjectAllowArray = 1 << 1;
    const kValidateObjectAllowFunction = 1 << 2;
    const kValidateObjectAllowObjects = kValidateObjectAllowArray |
      kValidateObjectAllowFunction;
    const kValidateObjectAllowObjectsAndNull = kValidateObjectAllowNullable |
      kValidateObjectAllowArray |
      kValidateObjectAllowFunction;

    const validateObject = hideStackFrames(
      (value, name, options = kValidateObjectNone) => {
        if (options === kValidateObjectNone) {
          if (value === null || Array.isArray(value)) {
            throw new ERR_INVALID_ARG_TYPE(name, 'Object', value);
          }

          if (typeof value !== 'object') {
            throw new ERR_INVALID_ARG_TYPE(name, 'Object', value);
          }
        } else {
          const throwOnNullable = (kValidateObjectAllowNullable & options) === 0;

          if (throwOnNullable && value === null) {
            throw new ERR_INVALID_ARG_TYPE(name, 'Object', value);
          }

          const throwOnArray = (kValidateObjectAllowArray & options) === 0;

          if (throwOnArray && Array.isArray(value)) {
            throw new ERR_INVALID_ARG_TYPE(name, 'Object', value);
          }

          const throwOnFunction = (kValidateObjectAllowFunction & options) === 0;
          const typeofValue = typeof value;

          if (typeofValue !== 'object' && (throwOnFunction || typeofValue !== 'function')) {
            throw new ERR_INVALID_ARG_TYPE(name, 'Object', value);
          }
        }
      });

    const validateDictionary = hideStackFrames(
      (value, name) => {
        if (value != null && typeof value !== 'object' && typeof value !== 'function') {
          throw new ERR_INVALID_ARG_TYPE(name, 'a dictionary', value);
        }
      });

    const validateArray = hideStackFrames((value, name, minLength = 0) => {
      if (!Array.isArray(value)) {
        throw new ERR_INVALID_ARG_TYPE(name, 'Array', value);
      }
      if (value.length < minLength) {
        const reason = `must have a length of at least ${minLength}`;
        throw new ERR_INVALID_ARG_VALUE(name, value, reason);
      }
    });

    const validateStringArray = hideStackFrames((value, name) => {
      validateArray(value, name);
      for (let i = 0; i < value.length; ++i) {
        if (typeof value[i] !== 'string') {
          throw new ERR_INVALID_ARG_TYPE(`${name}[${i}]`, 'string', value[i]);
        }
      }
    });

    const validateBooleanArray = hideStackFrames((value, name) => {
      validateArray(value, name);
      for (let i = 0; i < value.length; ++i) {
        if (value[i] !== true && value[i] !== false) {
          throw new ERR_INVALID_ARG_TYPE(`${name}[${i}]`, 'boolean', value[i]);
        }
      }
    });

    function validateAbortSignalArray(value, name) {
      validateArray(value, name);
      for (let i = 0; i < value.length; i++) {
        const signal = value[i];
        const indexedName = `${name}[${i}]`;
        if (signal == null) {
          throw new ERR_INVALID_ARG_TYPE(indexedName, 'AbortSignal', signal);
        }
        validateAbortSignal(signal, indexedName);
      }
    }

    const validateSignalName = hideStackFrames((signal, name = 'signal') => {
      validateString(signal, name);

      if (signals[signal] === undefined) {
        if (signals[signal.toUpperCase()] !== undefined) {
          throw new ERR_UNKNOWN_SIGNAL(signal +
                                       ' (signals must use all capital letters)');
        }

        throw new ERR_UNKNOWN_SIGNAL(signal);
      }
    });

    const validateBuffer = hideStackFrames((buffer, name = 'buffer') => {
      if (!isArrayBufferView(buffer)) {
        throw new ERR_INVALID_ARG_TYPE(name,
                                       ['Buffer', 'TypedArray', 'DataView'],
                                       buffer);
      }
    });

    const validateEncoding = hideStackFrames((data, encoding) => {
      const normalizedEncoding = normalizeEncoding(encoding);
      const length = data.length;

      if (normalizedEncoding === 'hex' && length % 2 !== 0) {
        throw new ERR_INVALID_ARG_VALUE('encoding', encoding,
                                        `is invalid for data of length ${length}`);
      }
    });

    const validatePort = hideStackFrames((port, name = 'Port', allowZero = true) => {
      if ((typeof port !== 'number' && typeof port !== 'string') ||
          (typeof port === 'string' && port.trim().length === 0) ||
          +port !== (+port >>> 0) ||
          port > 0xFFFF ||
          (+port === 0 && !allowZero)) {
        throw new ERR_SOCKET_BAD_PORT(name, port, allowZero);
      }
      return port | 0;
    });

    const validateAbortSignal = hideStackFrames((signal, name) => {
      if (signal !== undefined &&
          (signal === null ||
           typeof signal !== 'object' ||
           !('aborted' in signal))) {
        throw new ERR_INVALID_ARG_TYPE(name, 'AbortSignal', signal);
      }
    });

    const validateFunction = hideStackFrames((value, name) => {
      if (typeof value !== 'function')
        throw new ERR_INVALID_ARG_TYPE(name, 'Function', value);
    });

    const validatePlainFunction = hideStackFrames((value, name) => {
      if (typeof value !== 'function' || isAsyncFunction(value))
        throw new ERR_INVALID_ARG_TYPE(name, 'Function', value);
    });

    const validateUndefined = hideStackFrames((value, name) => {
      if (value !== undefined)
        throw new ERR_INVALID_ARG_TYPE(name, 'undefined', value);
    });

    function validateUnion(value, name, union) {
      if (!union.includes(value)) {
        throw new ERR_INVALID_ARG_TYPE(name, `('${union.join('|')}')`, value);
      }
    }

    /*
      The rules for the Link header field are described here:
      https://www.rfc-editor.org/rfc/rfc8288.html#section-3

      This regex validates any string surrounded by angle brackets
      (not necessarily a valid URI reference) followed by zero or more
      link-params separated by semicolons.
    */
    const linkValueRegExp = /^(?:<[^>\r\n]*>)(?:\s*;\s*[^;"\s]+(?:=(")?[^;"\s]*\1)?)*$/;

    const validateLinkHeaderFormat = hideStackFrames((value, name) => {
      if (
        typeof value === 'undefined' ||
        !linkValueRegExp.exec(value)
      ) {
        throw new ERR_INVALID_ARG_VALUE(
          name,
          value,
          'must be an array or string of format "</styles.css>; rel=preload; as=style"',
        );
      }
    });

    const validateThisInternalField = hideStackFrames((object, fieldKey, className) => {
      if (typeof object !== 'object' || object === null || !ObjectPrototypeHasOwnProperty(object, fieldKey)) {
        throw new ERR_INVALID_THIS(className);
      }
    });

    const validateLinkHeaderValue = hideStackFrames((hints) => {
      if (typeof hints === 'string') {
        validateLinkHeaderFormat.withoutStackTrace(hints, 'hints');
        return hints;
      } else if (Array.isArray(hints)) {
        const hintsLength = hints.length;
        let result = '';

        if (hintsLength === 0) {
          return result;
        }

        for (let i = 0; i < hintsLength; i++) {
          const link = hints[i];
          validateLinkHeaderFormat.withoutStackTrace(link, 'hints');
          result += link;

          if (i !== hintsLength - 1) {
            result += ', ';
          }
        }

        return result;
      }

      throw new ERR_INVALID_ARG_VALUE(
        'hints',
        hints,
        'must be an array or string of format "</styles.css>; rel=preload; as=style"',
      );
    });

    const validateIgnoreOptionElement = hideStackFrames((value, name) => {
      if (typeof value === 'string') {
        if (value.length === 0)
          throw new ERR_INVALID_ARG_VALUE(name, value, 'must be a non-empty string');
        return;
      }
      if (isRegExp(value)) return;
      if (typeof value === 'function') return;
      throw new ERR_INVALID_ARG_TYPE(name, ['string', 'RegExp', 'Function'], value);
    });

    const validateIgnoreOption = hideStackFrames((value, name) => {
      if (value == null) return;
      if (Array.isArray(value)) {
        for (let i = 0; i < value.length; i++) {
          validateIgnoreOptionElement(value[i], `${name}[${i}]`);
        }
        return;
      }
      validateIgnoreOptionElement(value, name);
    });

    const validateFiniteNumber = hideStackFrames((number, name) => {
      if (number === undefined) {
        return false;
      }

      if (Number.isFinite(number)) {
        return true; // Is a valid number
      }

      if (Number.isNaN(number)) {
        return false;
      }

      validateNumber(number, name);

      throw new ERR_OUT_OF_RANGE(name, 'a finite number', number);
    });

    const checkRangesOrGetDefault = hideStackFrames(
      (number, name, lower, upper, def) => {
        if (!validateFiniteNumber(number, name)) {
          return def;
        }
        if (number < lower || number > upper) {
          throw new ERR_OUT_OF_RANGE(name, `>= ${lower} and <= ${upper}`, number);
        }
        return number;
      },
    );

    module.exports = {
      isInt32,
      isUint32,
      parseFileMode,
      validateArray,
      validateStringArray,
      validateBooleanArray,
      validateAbortSignalArray,
      validateBoolean,
      validateBuffer,
      validateDictionary,
      validateEncoding,
      validateFunction,
      validateIgnoreOption,
      validateInt32,
      validateInteger,
      validateNumber,
      validateObject,
      kValidateObjectNone,
      kValidateObjectAllowNullable,
      kValidateObjectAllowArray,
      kValidateObjectAllowFunction,
      kValidateObjectAllowObjects,
      kValidateObjectAllowObjectsAndNull,
      validateOneOf,
      validatePlainFunction,
      validatePort,
      validateSignalName,
      validateString,
      validateStringWithoutNullBytes,
      validateUint32,
      validateUndefined,
      validateUnion,
      validateAbortSignal,
      validateLinkHeaderValue,
      validateThisInternalField,
      validateFiniteNumber,
      checkRangesOrGetDefault,
    };
  },
  "internal/util": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayFrom,
      ArrayPrototypePush,
      ArrayPrototypeSlice,
      ArrayPrototypeSort,
      Error,
      ErrorCaptureStackTrace,
      FunctionPrototypeCall,
      FunctionPrototypeSymbolHasInstance,
      NumberParseInt,
      ObjectDefineProperties,
      ObjectDefineProperty,
      ObjectFreeze,
      ObjectGetOwnPropertyDescriptor,
      ObjectGetOwnPropertyDescriptors,
      ObjectGetPrototypeOf,
      ObjectKeys,
      ObjectSetPrototypeOf,
      ObjectValues,
      Promise,
      ReflectApply,
      ReflectConstruct,
      RegExpPrototypeExec,
      RegExpPrototypeGetDotAll,
      RegExpPrototypeGetGlobal,
      RegExpPrototypeGetHasIndices,
      RegExpPrototypeGetIgnoreCase,
      RegExpPrototypeGetMultiline,
      RegExpPrototypeGetSource,
      RegExpPrototypeGetSticky,
      RegExpPrototypeGetUnicode,
      SafeMap,
      SafeSet,
      SafeWeakMap,
      SafeWeakRef,
      StringPrototypeIncludes,
      StringPrototypeReplace,
      StringPrototypeSlice,
      StringPrototypeToLowerCase,
      StringPrototypeToUpperCase,
      Symbol,
      SymbolFor,
      SymbolPrototypeGetDescription,
      SymbolReplace,
      SymbolSplit,
      globalThis,
    } = primordials;

    const {
      codes: {
        ERR_NO_CRYPTO,
        ERR_NO_TYPESCRIPT,
        ERR_UNKNOWN_SIGNAL,
        ERR_WEBASSEMBLY_NOT_SUPPORTED,
      },
      isErrorStackTraceLimitWritable,
      overrideStackTrace,
      uvErrmapGet,
    } = require('internal/errors');
    const { signals } = internalBinding('constants').os;
    const {
      constructSharedArrayBuffer,
      guessHandleType: _guessHandleType,
      defineLazyProperties,
      privateSymbols: {
        arrow_message_private_symbol,
        decorated_private_symbol,
      },
      sleep: _sleep,
    } = internalBinding('util');
    const { isNativeError, isPromise } = internalBinding('types');
    const { getOptionValue } = require('internal/options');
    const assert = require('internal/assert');
    const { encodings } = internalBinding('string_decoder');

    const noCrypto = !process.versions.openssl;
    const noTypeScript = !process.versions.amaro;

    const isWindows = process.platform === 'win32';
    const isMacOS = process.platform === 'darwin';

    const experimentalWarnings = new SafeSet();

    const colorRegExp = /\u001b\[\d\d?m/g; // eslint-disable-line no-control-regex

    let uvBinding;

    function lazyUv() {
      uvBinding ??= internalBinding('uv');
      return uvBinding;
    }

    function removeColors(str) {
      return str.replace(colorRegExp, '');
    }

    function isError(e) {
      return isNativeError(e) || FunctionPrototypeSymbolHasInstance(Error, e);
    }

    const codesWarned = new SafeSet();

    let validateString;
    let validateOneOf;

    function getDeprecationWarningEmitter(
      code, msg, deprecated, useEmitSync,
      shouldEmitWarning = () => true,
    ) {
      let warned = false;
      return function(arg) {
        if (!warned && shouldEmitWarning(arg)) {
          warned = true;
          if (code === 'ExperimentalWarning') {
            process.emitWarning(msg, code, deprecated);
          } else if (code !== undefined) {
            if (!codesWarned.has(code)) {
              const emitWarning = useEmitSync ?
                require('internal/process/warning').emitWarningSync :
                process.emitWarning;
              emitWarning(msg, 'DeprecationWarning', code, deprecated);
              codesWarned.add(code);
            }
          } else {
            process.emitWarning(msg, 'DeprecationWarning', deprecated);
          }
        }
      };
    }

    function isPendingDeprecation() {
      return getOptionValue('--pending-deprecation') &&
        !getOptionValue('--no-deprecation');
    }

    function deprecateProperty(key, msg, code, isPendingDeprecation) {
      const emitDeprecationWarning = getDeprecationWarningEmitter(
        code, msg, undefined, false, isPendingDeprecation,
      );
      return (options) => {
        if (key in options) {
          emitDeprecationWarning();
        }
      };
    }

    function pendingDeprecate(fn, msg, code) {
      const emitDeprecationWarning = getDeprecationWarningEmitter(
        code, msg, deprecated, false, isPendingDeprecation,
      );
      function deprecated(...args) {
        emitDeprecationWarning();
        return Reflect.apply(fn, this, args);
      }

      Object.defineProperty(deprecated, 'length', {
        __proto__: null,
        ...Object.getOwnPropertyDescriptor(fn, 'length'),
      });

      return deprecated;
    }

    function deprecate(fn, msg, code, useEmitSync, modifyPrototype = true) {
      if (validateString === undefined)
        ({ validateString } = require('internal/validators'));

      if (code !== undefined)
        validateString(code, 'code');

      const emitDeprecationWarning = getDeprecationWarningEmitter(
        code, msg, deprecated, useEmitSync,
      );

      function deprecated(...args) {
        if (!process.noDeprecation) {
          emitDeprecationWarning();
        }
        if (new.target) {
          return Reflect.construct(fn, args, new.target);
        }
        return Reflect.apply(fn, this, args);
      }

      if (modifyPrototype) {
        Object.setPrototypeOf(deprecated, fn);
        if (fn.prototype) {
          deprecated.prototype = fn.prototype;
        }

        Object.defineProperty(deprecated, 'length', {
          __proto__: null,
          ...Object.getOwnPropertyDescriptor(fn, 'length'),
        });
      }

      return deprecated;
    }

    function deprecateInstantiation(target, code, ...args) {
      assert(typeof code === 'string');

      getDeprecationWarningEmitter(code, `Instantiating ${target.name} without the 'new' keyword has been deprecated.`, target)();

      return Reflect.construct(target, args);
    }

    function decorateErrorStack(err) {
      if (!(isError(err) && err.stack) || err[decorated_private_symbol])
        return;

      const arrow = err[arrow_message_private_symbol];

      if (arrow) {
        err.stack = arrow + err.stack;
        err[decorated_private_symbol] = true;
      }
    }

    function assertCrypto() {
      if (noCrypto)
        throw new ERR_NO_CRYPTO();
    }

    function assertTypeScript() {
      if (noTypeScript)
        throw new ERR_NO_TYPESCRIPT();
      if (globalThis.WebAssembly === undefined)
        throw new ERR_WEBASSEMBLY_NOT_SUPPORTED('TypeScript');
    }

    function normalizeEncoding(enc) {
      if (enc == null || enc === 'utf8' || enc === 'utf-8') return 'utf8';
      return slowCases(enc);
    }

    function slowCases(enc) {
      switch (enc.length) {
        case 4:
          if (enc === 'UTF8') return 'utf8';
          if (enc === 'ucs2' || enc === 'UCS2') return 'utf16le';
          enc = enc.toLowerCase();
          if (enc === 'utf8') return 'utf8';
          if (enc === 'ucs2') return 'utf16le';
          break;
        case 3:
          if (enc === 'hex' || enc === 'HEX' ||
          enc.toLowerCase() === 'hex')
            return 'hex';
          break;
        case 5:
          if (enc === 'ascii') return 'ascii';
          if (enc === 'ucs-2') return 'utf16le';
          if (enc === 'UTF-8') return 'utf8';
          if (enc === 'ASCII') return 'ascii';
          if (enc === 'UCS-2') return 'utf16le';
          enc = enc.toLowerCase();
          if (enc === 'utf-8') return 'utf8';
          if (enc === 'ascii') return 'ascii';
          if (enc === 'ucs-2') return 'utf16le';
          break;
        case 6:
          if (enc === 'base64') return 'base64';
          if (enc === 'latin1' || enc === 'binary') return 'latin1';
          if (enc === 'BASE64') return 'base64';
          if (enc === 'LATIN1' || enc === 'BINARY') return 'latin1';
          enc = enc.toLowerCase();
          if (enc === 'base64') return 'base64';
          if (enc === 'latin1' || enc === 'binary') return 'latin1';
          break;
        case 7:
          if (enc === 'utf16le' || enc === 'UTF16LE' ||
          enc.toLowerCase() === 'utf16le')
            return 'utf16le';
          break;
        case 8:
          if (enc === 'utf-16le' || enc === 'UTF-16LE' ||
          enc.toLowerCase() === 'utf-16le')
            return 'utf16le';
          break;
        case 9:
          if (enc === 'base64url' || enc === 'BASE64URL' ||
          enc.toLowerCase() === 'base64url')
            return 'base64url';
          break;
        default:
          if (enc === '') return 'utf8';
      }
    }

    function emitExperimentalWarning(feature, messagePrefix, code, ctor) {
      if (experimentalWarnings.has(feature)) return;
      experimentalWarnings.add(feature);
      let msg = `${feature} is an experimental feature and might change at any time`;
      if (messagePrefix) {
        msg = messagePrefix + msg;
      }
      process.emitWarning(msg, 'ExperimentalWarning', code, ctor);
    }

    function filterDuplicateStrings(items, low) {
      const map = new SafeMap();
      for (let i = 0; i < items.length; i++) {
        const item = items[i];
        const key = item.toLowerCase();
        if (low) {
          map.set(key, key);
        } else {
          map.set(key, item);
        }
      }
      return (Array.from(map.values())).sort();
    }

    function cachedResult(fn) {
      let result;
      return () => {
        if (result === undefined)
          result = fn();
        return result.slice();
      };
    }

    let signalsToNamesMapping;
    function getSignalsToNamesMapping() {
      if (signalsToNamesMapping !== undefined)
        return signalsToNamesMapping;

      signalsToNamesMapping = { __proto__: null };
      for (const key in signals) {
        signalsToNamesMapping[signals[key]] = key;
      }

      return signalsToNamesMapping;
    }

    function convertToValidSignal(signal) {
      if (typeof signal === 'number' && getSignalsToNamesMapping()[signal])
        return signal;

      if (typeof signal === 'string') {
        const signalName = signals[signal.toUpperCase()];
        if (signalName) return signalName;
      }

      throw new ERR_UNKNOWN_SIGNAL(signal);
    }

    function convertProcessSignalToExitCode(signalCode) {
      if (validateOneOf === undefined)
        ({ validateOneOf } = require('internal/validators'));

      validateOneOf(signalCode, 'signalCode', Object.keys(signals));

      return 128 + signals[signalCode];
    }

    function getConstructorOf(obj) {
      while (obj) {
        const descriptor = Object.getOwnPropertyDescriptor(obj, 'constructor');
        if (descriptor !== undefined &&
            typeof descriptor.value === 'function' &&
            descriptor.value.name !== '') {
          return descriptor.value;
        }

        obj = Object.getPrototypeOf(obj);
      }

      return null;
    }

    let cachedURL;
    let cachedCWD;

    function getCWDURL() {
      const { sep } = require('path');
      const { pathToFileURL } = require('internal/url');

      let cwd;

      try {
        cwd = process.cwd();
      } catch {
        cachedURL ??= pathToFileURL(sep);
      }

      if (cwd != null && cwd !== cachedCWD) {
        cachedURL = pathToFileURL(cwd + sep);
        cachedCWD = cwd;
      }

      return cachedURL;
    }

    function getSystemErrorMessage(err) {
      return lazyUv().getErrorMessage(err);
    }

    function getSystemErrorName(err) {
      const entry = uvErrmapGet(err);
      return entry ? entry[0] : `Unknown system error ${err}`;
    }

    function getSystemErrorMap() {
      return lazyUv().getErrorMap();
    }

    const kCustomPromisifiedSymbol = Symbol.for('nodejs.util.promisify.custom');
    const kCustomPromisifyArgsSymbol = Symbol('customPromisifyArgs');

    let validateFunction;

    function promisify(original) {
      if (validateFunction === undefined)
        ({ validateFunction } = require('internal/validators'));

      validateFunction(original, 'original');

      if (original[kCustomPromisifiedSymbol]) {
        const fn = original[kCustomPromisifiedSymbol];

        validateFunction(fn, 'util.promisify.custom');

        return Object.defineProperty(fn, kCustomPromisifiedSymbol, {
          __proto__: null,
          value: fn, enumerable: false, writable: false, configurable: true,
        });
      }

      const argumentNames = original[kCustomPromisifyArgsSymbol];

      function fn(...args) {
        return new Promise((resolve, reject) => {
          args.push((err, ...values) => {
            if (err) {
              return reject(err);
            }
            if (argumentNames !== undefined && values.length > 1) {
              const obj = {};
              for (let i = 0; i < argumentNames.length; i++)
                obj[argumentNames[i]] = values[i];
              resolve(obj);
            } else {
              resolve(values[0]);
            }
          });
          if (isPromise(Reflect.apply(original, this, args))) {
            process.emitWarning('Calling promisify on a function that returns a Promise is likely a mistake.',
                                'DeprecationWarning', 'DEP0174');
          }
        });
      }

      Object.setPrototypeOf(fn, Object.getPrototypeOf(original));

      Object.defineProperty(fn, kCustomPromisifiedSymbol, {
        __proto__: null,
        value: fn, enumerable: false, writable: false, configurable: true,
      });

      const descriptors = ObjectGetOwnPropertyDescriptors(original);
      const propertiesValues = Object.values(descriptors);
      for (let i = 0; i < propertiesValues.length; i++) {
        Object.setPrototypeOf(propertiesValues[i], null);
      }
      return Object.defineProperties(fn, descriptors);
    }

    promisify.custom = kCustomPromisifiedSymbol;

    function join(output, separator) {
      let str = '';
      if (output.length !== 0) {
        const lastIndex = output.length - 1;
        for (let i = 0; i < lastIndex; i++) {
          str += output[i];
          str += separator;
        }
        str += output[lastIndex];
      }
      return str;
    }

    function spliceOne(list, index) {
      for (; index + 1 < list.length; index++)
        list[index] = list[index + 1];
      list.pop();
    }

    const kNodeModulesRE = /^(?:.*)[\\/]node_modules[\\/]/;

    function isUnderNodeModules(filename) {
      return filename && (kNodeModulesRE.exec(filename) !== null);
    }

    let getStructuredStackImpl;

    function lazyGetStructuredStack() {
      if (getStructuredStackImpl === undefined) {
        const { runInNewContext } = require('vm');
        getStructuredStackImpl = runInNewContext(`(function() {
          try { Error.stackTraceLimit = Infinity; } catch {}
          return function structuredStack() {
            const e = new Error();
            overrideStackTrace.set(e, (err, trace) => trace);
            return e.stack;
          };
        })()`, { overrideStackTrace }, { filename: 'structured-stack' });
      }

      return getStructuredStackImpl;
    }

    function getStructuredStack() {
      const getStructuredStackImpl = lazyGetStructuredStack();

      return getStructuredStackImpl();
    }

    function once(callback, { preserveReturnValue = false } = kEmptyObject) {
      let called = false;
      let returnValue;
      return function(...args) {
        if (called) return returnValue;
        called = true;
        const result = Reflect.apply(callback, this, args);
        returnValue = preserveReturnValue ? result : undefined;
        return result;
      };
    }

    let validateUint32;

    function sleep(msec) {
      if (validateUint32 === undefined)
        ({ validateUint32 } = require('internal/validators'));

      validateUint32(msec, 'msec');
      _sleep(msec);
    }

    function defineOperation(target, name, method) {
      Object.defineProperty(target, name, {
        __proto__: null,
        writable: true,
        enumerable: true,
        configurable: true,
        value: method,
      });
    }

    function exposeInterface(target, name, interfaceObject) {
      Object.defineProperty(target, name, {
        __proto__: null,
        writable: true,
        enumerable: false,
        configurable: true,
        value: interfaceObject,
      });
    }

    function exposeNamespace(target, name, namespaceObject) {
      Object.defineProperty(target, name, {
        __proto__: null,
        writable: true,
        enumerable: false,
        configurable: true,
        value: namespaceObject,
      });
    }

    function defineReplaceableLazyAttribute(target, id, keys, writable = true, check) {
      let mod;
      for (let i = 0; i < keys.length; i++) {
        const key = keys[i];
        let value;
        let setterCalled = false;

        function get() {
          if (check !== undefined) {
            FunctionPrototypeCall(check, this);
          }
          if (setterCalled) {
            return value;
          }
          mod ??= require(id);
          value ??= mod[key];
          return value;
        }

        Object.defineProperty(get, 'name', {
          __proto__: null,
          value: `get ${key}`,
        });

        function set(val) {
          setterCalled = true;
          value = val;
        }
        Object.defineProperty(set, 'name', {
          __proto__: null,
          value: `set ${key}`,
        });

        Object.defineProperty(target, key, {
          __proto__: null,
          enumerable: true,
          configurable: true,
          get,
          set: writable ? set : undefined,
        });
      }
    }

    function exposeLazyInterfaces(target, id, keys) {
      defineLazyProperties(target, id, keys, false);
    }

    let _DOMException;
    const lazyDOMExceptionClass = () => {
      _DOMException ??= internalBinding('messaging').DOMException;
      return _DOMException;
    };

    const lazyDOMException = (message, name) => {
      _DOMException ??= internalBinding('messaging').DOMException;
      if (isErrorStackTraceLimitWritable()) {
        const limit = Error.stackTraceLimit;
        Error.stackTraceLimit = 0;
        const ex = new _DOMException(message, name);
        Error.stackTraceLimit = limit;
        Error.captureStackTrace(ex, lazyDOMException);
        return ex;
      }
      return new _DOMException(message, name);

    };

    const kEnumerableProperty = Object.freeze({
      __proto__: null,
      enumerable: true,
    });

    const kEmptyObject = Object.freeze({ __proto__: null });

    function setOwnProperty(obj, key, value) {
      Object.defineProperty(obj, key, {
        __proto__: null,
        configurable: true,
        enumerable: true,
        value,
        writable: true,
      });
      return value;
    }

    let internalGlobal;
    function getInternalGlobal() {
      if (internalGlobal == null) {
        const { runInNewContext } = require('vm');
        internalGlobal = runInNewContext('this', undefined, { contextName: 'internal' });
      }
      return internalGlobal;
    }

    function SideEffectFreeRegExpPrototypeExec(regex, string) {
      const { RegExp: RegExpFromAnotherRealm } = getInternalGlobal();
      return FunctionPrototypeCall(RegExpFromAnotherRealm.prototype.exec, regex, string);
    }

    const crossRealmRegexes = new SafeWeakMap();
    function getCrossRealmRegex(regex) {
      const cached = crossRealmRegexes.get(regex);
      if (cached) return cached;

      let flagString = '';
      if (RegExpPrototypeGetHasIndices(regex)) flagString += 'd';
      if (RegExpPrototypeGetGlobal(regex)) flagString += 'g';
      if (RegExpPrototypeGetIgnoreCase(regex)) flagString += 'i';
      if (RegExpPrototypeGetMultiline(regex)) flagString += 'm';
      if (RegExpPrototypeGetDotAll(regex)) flagString += 's';
      if (RegExpPrototypeGetUnicode(regex)) flagString += 'u';
      if (RegExpPrototypeGetSticky(regex)) flagString += 'y';

      const { RegExp: RegExpFromAnotherRealm } = getInternalGlobal();
      const crossRealmRegex = new RegExpFromAnotherRealm(RegExpPrototypeGetSource(regex), flagString);
      crossRealmRegexes.set(regex, crossRealmRegex);
      return crossRealmRegex;
    }

    function SideEffectFreeRegExpPrototypeSymbolReplace(regex, string, replacement) {
      return getCrossRealmRegex(regex)[SymbolReplace](string, replacement);
    }

    function SideEffectFreeRegExpPrototypeSymbolSplit(regex, string, limit = undefined) {
      return getCrossRealmRegex(regex)[SymbolSplit](string, limit);
    }

    function getLazy(initializer) {
      let value;
      let initialized = false;
      return function() {
        if (initialized === false) {
          value = initializer();
          initialized = true;
        }
        return value;
      };
    }

    function setupCoverageHooks(dir) {
      const cwd = require('internal/process/execution').tryGetCwd();
      const { resolve } = require('path');
      const coverageDirectory = resolve(cwd, dir);
      const { sourceMapCacheToObject } =
        require('internal/source_map/source_map_cache');

      if (process.features.inspector) {
        internalBinding('profiler').setCoverageDirectory(coverageDirectory);
        internalBinding('profiler').setSourceMapCacheGetter(sourceMapCacheToObject);
      } else {
        process.emitWarning('The inspector is disabled, ' +
                            'coverage could not be collected',
                            'Warning');
        return '';
      }
      return coverageDirectory;
    }

    function countBinaryOnes(n) {
      n = n - ((n >>> 1) & 0x55555555);
      n = (n & 0x33333333) + ((n >>> 2) & 0x33333333);
      return ((n + (n >>> 4) & 0xF0F0F0F) * 0x1010101) >>> 24;
    }

    function getCIDR(address, netmask, family) {
      let ones = 0;
      let split = '.';
      let range = 10;
      let groupLength = 8;
      let hasZeros = false;
      let lastPos = 0;

      if (family === 'IPv6') {
        split = ':';
        range = 16;
        groupLength = 16;
      }

      for (let i = 0; i < netmask.length; i++) {
        if (netmask[i] !== split) {
          if (i + 1 < netmask.length) {
            continue;
          }
          i++;
        }
        const part = netmask.slice(lastPos, i);
        lastPos = i + 1;
        if (part !== '') {
          if (hasZeros) {
            if (part !== '0') {
              return null;
            }
          } else {
            const binary = Number.parseInt(part, range);
            const binaryOnes = countBinaryOnes(binary);
            ones += binaryOnes;
            if (binaryOnes !== groupLength) {
              if (binary.toString(2).includes('01')) {
                return null;
              }
              hasZeros = true;
            }
          }
        }
      }

      return `${address}/${ones}`;
    }

    const handleTypes = ['TCP', 'TTY', 'UDP', 'FILE', 'PIPE', 'UNKNOWN'];
    function guessHandleType(fd) {
      const type = _guessHandleType(fd);
      return handleTypes[type];
    }

    class WeakReference {
      #weak = null;
      #strong = null;
      #refCount = 0;
      constructor(object) {
        this.#weak = new SafeWeakRef(object);
      }

      incRef() {
        this.#refCount++;
        if (this.#refCount === 1) {
          const derefed = this.#weak.deref();
          if (derefed !== undefined) {
            this.#strong = derefed;
          }
        }
        return this.#refCount;
      }

      decRef() {
        this.#refCount--;
        if (this.#refCount === 0) {
          this.#strong = null;
        }
        return this.#refCount;
      }

      get() {
        return this.#weak.deref();
      }
    }

    const encodingsMap = { __proto__: null };
    for (let i = 0; i < encodings.length; ++i)
      encodingsMap[encodings[i]] = i;

    function assignFunctionName(name, fn, descriptor = kEmptyObject) {
      if (typeof name !== 'string') {
        const symbolDescription = SymbolPrototypeGetDescription(name);
        assert(symbolDescription !== undefined, 'Attempted to name function after descriptionless Symbol');
        name = `[${symbolDescription}]`;
      }
      return Object.defineProperty(fn, 'name', {
        __proto__: null,
        writable: false,
        enumerable: false,
        configurable: true,
        ...Object.getOwnPropertyDescriptor(fn, 'name'),
        ...descriptor,
        value: name,
      });
    }

    module.exports = {
      getLazy,
      assertCrypto,
      assertTypeScript,
      assignFunctionName,
      cachedResult,
      constructSharedArrayBuffer,
      convertProcessSignalToExitCode,
      convertToValidSignal,
      decorateErrorStack,
      defineOperation,
      defineLazyProperties,
      defineReplaceableLazyAttribute,
      deprecate,
      deprecateInstantiation,
      deprecateProperty,
      emitExperimentalWarning,
      encodingsMap,
      exposeInterface,
      exposeLazyInterfaces,
      exposeNamespace,
      filterDuplicateStrings,
      getConstructorOf,
      getCIDR,
      getCWDURL,
      getStructuredStack,
      getSystemErrorMap,
      getSystemErrorName,
      getSystemErrorMessage,
      guessHandleType,
      isError,
      isUnderNodeModules,
      isMacOS,
      isWindows,
      join,
      lazyDOMException,
      lazyDOMExceptionClass,
      normalizeEncoding,
      once,
      promisify,
      SideEffectFreeRegExpPrototypeExec,
      SideEffectFreeRegExpPrototypeSymbolReplace,
      SideEffectFreeRegExpPrototypeSymbolSplit,
      sleep,
      spliceOne,
      setupCoverageHooks,
      removeColors,

      customPromisifyArgs: kCustomPromisifyArgsSymbol,

      customInspectSymbol: Symbol.for('nodejs.util.inspect.custom'),

      kIsEncodingSymbol: Symbol('kIsEncodingSymbol'),
      kVmBreakFirstLineSymbol: Symbol('kVmBreakFirstLineSymbol'),

      kEmptyObject,
      kEnumerableProperty,
      setOwnProperty,
      pendingDeprecate,
      WeakReference,
      isPendingDeprecation,
      getDeprecationWarningEmitter,
    };
  },
  "internal/util/types": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayBufferIsView,
      ObjectDefineProperties,
      TypedArrayPrototypeGetSymbolToStringTag,
    } = primordials;

    function isDataView(value) {
      return ArrayBuffer.isView(value) && TypedArrayPrototypeGetSymbolToStringTag(value) === undefined;
    }

    function isTypedArray(value) {
      return TypedArrayPrototypeGetSymbolToStringTag(value) !== undefined;
    }

    function isUint8Array(value) {
      return TypedArrayPrototypeGetSymbolToStringTag(value) === 'Uint8Array';
    }

    function isUint8ClampedArray(value) {
      return TypedArrayPrototypeGetSymbolToStringTag(value) === 'Uint8ClampedArray';
    }

    function isUint16Array(value) {
      return TypedArrayPrototypeGetSymbolToStringTag(value) === 'Uint16Array';
    }

    function isUint32Array(value) {
      return TypedArrayPrototypeGetSymbolToStringTag(value) === 'Uint32Array';
    }

    function isInt8Array(value) {
      return TypedArrayPrototypeGetSymbolToStringTag(value) === 'Int8Array';
    }

    function isInt16Array(value) {
      return TypedArrayPrototypeGetSymbolToStringTag(value) === 'Int16Array';
    }

    function isInt32Array(value) {
      return TypedArrayPrototypeGetSymbolToStringTag(value) === 'Int32Array';
    }

    function isFloat16Array(value) {
      return TypedArrayPrototypeGetSymbolToStringTag(value) === 'Float16Array';
    }

    function isFloat32Array(value) {
      return TypedArrayPrototypeGetSymbolToStringTag(value) === 'Float32Array';
    }

    function isFloat64Array(value) {
      return TypedArrayPrototypeGetSymbolToStringTag(value) === 'Float64Array';
    }

    function isBigInt64Array(value) {
      return TypedArrayPrototypeGetSymbolToStringTag(value) === 'BigInt64Array';
    }

    function isBigUint64Array(value) {
      return TypedArrayPrototypeGetSymbolToStringTag(value) === 'BigUint64Array';
    }

    module.exports = {
      ...internalBinding('types'),
      isArrayBufferView: ArrayBufferIsView,
      isDataView,
      isTypedArray,
      isUint8Array,
      isUint8ClampedArray,
      isUint16Array,
      isUint32Array,
      isInt8Array,
      isInt16Array,
      isInt32Array,
      isFloat16Array,
      isFloat32Array,
      isFloat64Array,
      isBigInt64Array,
      isBigUint64Array,
    };

    let isCryptoKeyFn;
    let isKeyObjectFn;

    Object.defineProperties(module.exports, {
      isKeyObject: {
        __proto__: null,
        configurable: false,
        enumerable: true,
        value: function isKeyObject(obj) {
          if (!process.versions.openssl) {
            return false;
          }

          if (!isKeyObjectFn) {
            ({ isKeyObject: isKeyObjectFn } = require('internal/crypto/keys'));
          }

          return isKeyObjectFn(obj);
        },
      },
      isCryptoKey: {
        __proto__: null,
        configurable: false,
        enumerable: true,
        value: function isCryptoKey(obj) {
          if (!process.versions.openssl) {
            return false;
          }

          if (!isCryptoKeyFn) {
            ({ isCryptoKey: isCryptoKeyFn } = require('internal/crypto/keys'));
          }

          return isCryptoKeyFn(obj);
        },
      },
    });
  },
  "internal/util/inspect": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      AggregateError,
      AggregateErrorPrototype,
      Array,
      ArrayBuffer,
      ArrayBufferPrototype,
      ArrayIsArray,
      ArrayPrototype,
      ArrayPrototypeFilter,
      ArrayPrototypeForEach,
      ArrayPrototypeIncludes,
      ArrayPrototypeIndexOf,
      ArrayPrototypeJoin,
      ArrayPrototypeMap,
      ArrayPrototypePop,
      ArrayPrototypePush,
      ArrayPrototypePushApply,
      ArrayPrototypeSlice,
      ArrayPrototypeSort,
      ArrayPrototypeSplice,
      ArrayPrototypeUnshift,
      BigIntPrototypeValueOf,
      Boolean,
      BooleanPrototype,
      BooleanPrototypeValueOf,
      DataView,
      DataViewPrototype,
      Date,
      DatePrototype,
      DatePrototypeGetTime,
      DatePrototypeToISOString,
      DatePrototypeToString,
      Error,
      ErrorPrototype,
      ErrorPrototypeToString,
      Function,
      FunctionPrototype,
      FunctionPrototypeBind,
      FunctionPrototypeCall,
      FunctionPrototypeSymbolHasInstance,
      FunctionPrototypeToString,
      JSONStringify,
      Map,
      MapPrototype,
      MapPrototypeEntries,
      MapPrototypeGetSize,
      MathFloor,
      MathMax,
      MathMin,
      MathRound,
      MathSqrt,
      MathTrunc,
      Number,
      NumberIsFinite,
      NumberIsNaN,
      NumberParseFloat,
      NumberParseInt,
      NumberPrototype,
      NumberPrototypeToString,
      NumberPrototypeValueOf,
      Object,
      ObjectAssign,
      ObjectDefineProperty,
      ObjectGetOwnPropertyDescriptor,
      ObjectGetOwnPropertyNames,
      ObjectGetOwnPropertySymbols,
      ObjectGetPrototypeOf,
      ObjectIs,
      ObjectKeys,
      ObjectPrototype,
      ObjectPrototypeHasOwnProperty,
      ObjectPrototypePropertyIsEnumerable,
      ObjectPrototypeToString,
      ObjectSeal,
      ObjectSetPrototypeOf,
      Promise,
      PromisePrototype,
      RangeError,
      RangeErrorPrototype,
      ReflectApply,
      ReflectOwnKeys,
      RegExp,
      RegExpPrototype,
      RegExpPrototypeExec,
      RegExpPrototypeSymbolReplace,
      RegExpPrototypeSymbolSplit,
      RegExpPrototypeToString,
      SafeMap,
      SafeSet,
      SafeStringIterator,
      Set,
      SetPrototype,
      SetPrototypeGetSize,
      SetPrototypeValues,
      String,
      StringPrototype,
      StringPrototypeCharCodeAt,
      StringPrototypeCodePointAt,
      StringPrototypeEndsWith,
      StringPrototypeIncludes,
      StringPrototypeIndexOf,
      StringPrototypeLastIndexOf,
      StringPrototypeNormalize,
      StringPrototypePadEnd,
      StringPrototypePadStart,
      StringPrototypeRepeat,
      StringPrototypeReplace,
      StringPrototypeReplaceAll,
      StringPrototypeSlice,
      StringPrototypeSplit,
      StringPrototypeStartsWith,
      StringPrototypeToLowerCase,
      StringPrototypeValueOf,
      SymbolIterator,
      SymbolPrototypeToString,
      SymbolPrototypeValueOf,
      SymbolToPrimitive,
      SymbolToStringTag,
      TypeError,
      TypeErrorPrototype,
      TypedArray,
      TypedArrayPrototype,
      TypedArrayPrototypeGetLength,
      TypedArrayPrototypeGetSymbolToStringTag,
      Uint8Array,
      WeakMap,
      WeakMapPrototype,
      WeakSet,
      WeakSetPrototype,
      globalThis,
      uncurryThis,
    } = primordials;

    const {
      constants: {
        ALL_PROPERTIES,
        ONLY_ENUMERABLE,
        kPending,
        kRejected,
      },
      getOwnNonIndexProperties,
      getPromiseDetails,
      getProxyDetails,
      previewEntries,
      getConstructorName: internalGetConstructorName,
      getExternalValue,
    } = internalBinding('util');

    const {
      customInspectSymbol,
      isError,
      join,
      removeColors,
    } = require('internal/util');

    const {
      isStackOverflowError,
    } = require('internal/errors');

    const {
      isAsyncFunction,
      isGeneratorFunction,
      isAnyArrayBuffer,
      isArrayBuffer,
      isArgumentsObject,
      isBoxedPrimitive,
      isDataView,
      isExternal,
      isMap,
      isMapIterator,
      isModuleNamespaceObject,
      isNativeError,
      isPromise,
      isSet,
      isSetIterator,
      isWeakMap,
      isWeakSet,
      isRegExp,
      isDate,
      isTypedArray,
      isStringObject,
      isNumberObject,
      isBooleanObject,
      isBigIntObject,
    } = require('internal/util/types');

    const assert = require('internal/assert');

    const { BuiltinModule } = require('internal/bootstrap/realm');
    const {
      validateObject,
      validateString,
      kValidateObjectAllowArray,
    } = require('internal/validators');

    let hexSlice;
    let internalUrl;

    function pathToFileUrlHref(filepath) {
      internalUrl ??= require('internal/url');
      return internalUrl.pathToFileURL(filepath).href;
    }

    function isURL(value) {
      internalUrl ??= require('internal/url');
      return typeof value.href === 'string' && value instanceof internalUrl.URL;
    }

    const builtInObjects = new SafeSet(
      Object.getOwnPropertyNames(globalThis).filter((e) => (/^[A-Z][a-zA-Z0-9]+$/).exec(e) !== null),
    );

    const isUndetectableObject = (v) => typeof v === 'undefined' && v !== undefined;

    const inspectDefaultOptions = ObjectSeal({
      showHidden: false,
      depth: 2,
      colors: false,
      customInspect: true,
      showProxy: false,
      maxArrayLength: 100,
      maxStringLength: 10000,
      breakLength: 80,
      compact: 3,
      sorted: false,
      getters: false,
      numericSeparator: false,
    });

    const kObjectType = 0;
    const kArrayType = 1;
    const kArrayExtrasType = 2;

    /* eslint-disable no-control-regex */
    const strEscapeSequencesRegExp = /[\x00-\x1f\x27\x5c\x7f-\x9f]|[\ud800-\udbff](?![\udc00-\udfff])|(?<![\ud800-\udbff])[\udc00-\udfff]/;
    const strEscapeSequencesReplacer = /[\x00-\x1f\x27\x5c\x7f-\x9f]|[\ud800-\udbff](?![\udc00-\udfff])|(?<![\ud800-\udbff])[\udc00-\udfff]/g;
    const strEscapeSequencesRegExpSingle = /[\x00-\x1f\x5c\x7f-\x9f]|[\ud800-\udbff](?![\udc00-\udfff])|(?<![\ud800-\udbff])[\udc00-\udfff]/;
    const strEscapeSequencesReplacerSingle = /[\x00-\x1f\x5c\x7f-\x9f]|[\ud800-\udbff](?![\udc00-\udfff])|(?<![\ud800-\udbff])[\udc00-\udfff]/g;
    /* eslint-enable no-control-regex */

    const keyStrRegExp = /^[a-zA-Z_][a-zA-Z_0-9]*$/;
    const numberRegExp = /^(0|[1-9][0-9]*)$/;

    const coreModuleRegExp = /^ {4}at (?:[^/\\(]+ \(|)node:(.+):\d+:\d+\)?$/;

    const classRegExp = /^(\s+[^(]*?)\s*{/;
    const stripCommentsRegExp = /(\/\/.*?\n)|(\/\*(.|\n)*?\*\/)/g;

    const kMinLineLength = 16;

    const kWeak = 0;
    const kIterator = 1;
    const kMapEntries = 2;

    const meta = [
      '\\x00', '\\x01', '\\x02', '\\x03', '\\x04', '\\x05', '\\x06', '\\x07', // x07
      '\\b', '\\t', '\\n', '\\x0B', '\\f', '\\r', '\\x0E', '\\x0F',           // x0F
      '\\x10', '\\x11', '\\x12', '\\x13', '\\x14', '\\x15', '\\x16', '\\x17', // x17
      '\\x18', '\\x19', '\\x1A', '\\x1B', '\\x1C', '\\x1D', '\\x1E', '\\x1F', // x1F
      '', '', '', '', '', '', '', "\\'", '', '', '', '', '', '', '', '',      // x2F
      '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '',         // x3F
      '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '',         // x4F
      '', '', '', '', '', '', '', '', '', '', '', '', '\\\\', '', '', '',     // x5F
      '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '',         // x6F
      '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '\\x7F',    // x7F
      '\\x80', '\\x81', '\\x82', '\\x83', '\\x84', '\\x85', '\\x86', '\\x87', // x87
      '\\x88', '\\x89', '\\x8A', '\\x8B', '\\x8C', '\\x8D', '\\x8E', '\\x8F', // x8F
      '\\x90', '\\x91', '\\x92', '\\x93', '\\x94', '\\x95', '\\x96', '\\x97', // x97
      '\\x98', '\\x99', '\\x9A', '\\x9B', '\\x9C', '\\x9D', '\\x9E', '\\x9F', // x9F
    ];

    const ansi = new RegExp(
      '(?:\\u001B\\][\\s\\S]*?(?:\\u0007|\\u001B\\u005C|\\u009C))' +
      '|[\\u001B\\u009B][[\\]()#;?]*' +
      '(?:\\d{1,4}(?:[;:]\\d{0,4})*)?' +
      '[\\dA-PR-TZcf-nq-uy=><~]', 'g',
    );

    let getStringWidth;

    function getUserOptions(ctx, isCrossContext) {
      const ret = {
        stylize: ctx.stylize,
        showHidden: ctx.showHidden,
        depth: ctx.depth,
        colors: ctx.colors,
        customInspect: ctx.customInspect,
        showProxy: ctx.showProxy,
        maxArrayLength: ctx.maxArrayLength,
        maxStringLength: ctx.maxStringLength,
        breakLength: ctx.breakLength,
        compact: ctx.compact,
        sorted: ctx.sorted,
        getters: ctx.getters,
        numericSeparator: ctx.numericSeparator,
        ...ctx.userOptions,
      };

      if (isCrossContext) {
        Object.setPrototypeOf(ret, null);
        for (const key of Object.keys(ret)) {
          if ((typeof ret[key] === 'object' || typeof ret[key] === 'function') &&
              ret[key] !== null) {
            delete ret[key];
          }
        }
        ret.stylize = Object.setPrototypeOf((value, flavour) => {
          let stylized;
          try {
            stylized = `${ctx.stylize(value, flavour)}`;
          } catch {
          }

          if (typeof stylized !== 'string') return value;
          return stylized;
        }, null);
      }

      return ret;
    }

    /* Legacy: value, showHidden, depth, colors */
    function inspect(value, opts) {
      const ctx = {
        budget: {},
        indentationLvl: 0,
        seen: [],
        currentDepth: 0,
        stylize: stylizeNoColor,
        showHidden: inspectDefaultOptions.showHidden,
        depth: inspectDefaultOptions.depth,
        colors: inspectDefaultOptions.colors,
        customInspect: inspectDefaultOptions.customInspect,
        showProxy: inspectDefaultOptions.showProxy,
        maxArrayLength: inspectDefaultOptions.maxArrayLength,
        maxStringLength: inspectDefaultOptions.maxStringLength,
        breakLength: inspectDefaultOptions.breakLength,
        compact: inspectDefaultOptions.compact,
        sorted: inspectDefaultOptions.sorted,
        getters: inspectDefaultOptions.getters,
        numericSeparator: inspectDefaultOptions.numericSeparator,
      };
      if (arguments.length > 1) {
        if (arguments.length > 2) {
          if (arguments[2] !== undefined) {
            ctx.depth = arguments[2];
          }
          if (arguments.length > 3 && arguments[3] !== undefined) {
            ctx.colors = arguments[3];
          }
        }
        if (typeof opts === 'boolean') {
          ctx.showHidden = opts;
        } else if (opts) {
          const optKeys = Object.keys(opts);
          for (let i = 0; i < optKeys.length; ++i) {
            const key = optKeys[i];
            if (
              ObjectPrototypeHasOwnProperty(inspectDefaultOptions, key) ||
              key === 'stylize') {
              ctx[key] = opts[key];
            } else if (ctx.userOptions === undefined) {
              ctx.userOptions = opts;
            }
          }
        }
      }
      if (ctx.colors) ctx.stylize = stylizeWithColor;
      if (ctx.maxArrayLength === null) ctx.maxArrayLength = Infinity;
      if (ctx.maxStringLength === null) ctx.maxStringLength = Infinity;
      return formatValue(ctx, value, 0);
    }
    inspect.custom = customInspectSymbol;

    Object.defineProperty(inspect, 'defaultOptions', {
      __proto__: null,
      get() {
        return inspectDefaultOptions;
      },
      set(options) {
        validateObject(options, 'options');
        return Object.assign(inspectDefaultOptions, options);
      },
    });

    const defaultFG = 39;
    const defaultBG = 49;
    inspect.colors = {
      __proto__: null,
      reset: [0, 0],
      bold: [1, 22],
      dim: [2, 22], // Alias: faint
      italic: [3, 23],
      underline: [4, 24],
      blink: [5, 25],
      inverse: [7, 27], // Alias: swapcolors, swapColors
      hidden: [8, 28], // Alias: conceal
      strikethrough: [9, 29], // Alias: strikeThrough, crossedout, crossedOut
      doubleunderline: [21, 24], // Alias: doubleUnderline
      black: [30, defaultFG],
      red: [31, defaultFG],
      green: [32, defaultFG],
      yellow: [33, defaultFG],
      blue: [34, defaultFG],
      magenta: [35, defaultFG],
      cyan: [36, defaultFG],
      white: [37, defaultFG],
      bgBlack: [40, defaultBG],
      bgRed: [41, defaultBG],
      bgGreen: [42, defaultBG],
      bgYellow: [43, defaultBG],
      bgBlue: [44, defaultBG],
      bgMagenta: [45, defaultBG],
      bgCyan: [46, defaultBG],
      bgWhite: [47, defaultBG],
      framed: [51, 54],
      overlined: [53, 55],
      gray: [90, defaultFG], // Alias: grey, blackBright
      redBright: [91, defaultFG],
      greenBright: [92, defaultFG],
      yellowBright: [93, defaultFG],
      blueBright: [94, defaultFG],
      magentaBright: [95, defaultFG],
      cyanBright: [96, defaultFG],
      whiteBright: [97, defaultFG],
      bgGray: [100, defaultBG], // Alias: bgGrey, bgBlackBright
      bgRedBright: [101, defaultBG],
      bgGreenBright: [102, defaultBG],
      bgYellowBright: [103, defaultBG],
      bgBlueBright: [104, defaultBG],
      bgMagentaBright: [105, defaultBG],
      bgCyanBright: [106, defaultBG],
      bgWhiteBright: [107, defaultBG],
    };

    function defineColorAlias(target, alias) {
      Object.defineProperty(inspect.colors, alias, {
        __proto__: null,
        get() {
          return this[target];
        },
        set(value) {
          this[target] = value;
        },
        configurable: true,
        enumerable: false,
      });
    }

    defineColorAlias('gray', 'grey');
    defineColorAlias('gray', 'blackBright');
    defineColorAlias('bgGray', 'bgGrey');
    defineColorAlias('bgGray', 'bgBlackBright');
    defineColorAlias('dim', 'faint');
    defineColorAlias('strikethrough', 'crossedout');
    defineColorAlias('strikethrough', 'strikeThrough');
    defineColorAlias('strikethrough', 'crossedOut');
    defineColorAlias('hidden', 'conceal');
    defineColorAlias('inverse', 'swapColors');
    defineColorAlias('inverse', 'swapcolors');
    defineColorAlias('doubleunderline', 'doubleUnderline');

    inspect.styles = Object.assign({ __proto__: null }, {
      special: 'cyan',
      number: 'yellow',
      bigint: 'yellow',
      boolean: 'yellow',
      undefined: 'grey',
      null: 'bold',
      string: 'green',
      symbol: 'green',
      date: 'magenta',
      regexp: highlightRegExp,
      module: 'underline',
    });

    inspect.styles.regexp.colors = ['green', 'red', 'yellow', 'cyan', 'magenta'];

    const highlightRegExpColors = inspect.styles.regexp.colors.slice();

    function highlightRegExp(regexpString) {
      let out = '';
      let i = 0;
      let depth = 0;
      let inClass = false;

      const paletteNames = highlightRegExp.colors?.length > 0 ?
        highlightRegExp.colors :
        highlightRegExpColors;

      const palette = paletteNames.reduce((acc, name) => {
        const color = inspect.colors[name];
        if (color) acc.push([`\u001b[${color[0]}m`, `\u001b[${color[1]}m`]);
        return acc;
      }, []);

      function writeGroup(start, end, decreaseDepth = 1) {
        let seq = '';
        i++;
        while (i < regexpString.length && regexpString[i] !== end) {
          seq += regexpString[i++];
        }
        if (i < regexpString.length) {
          depth -= decreaseDepth;
          write(start);
          writeDepth(seq, 1, 1);
          write(end);
          depth += decreaseDepth;
        } else {
          writeDepth(start, 1, -seq.length);
        }
      }

      const write = (str) => {
        const idx = depth % palette.length;
        const color = palette[idx] ?? palette[0];
        out += color[0] + str + color[1];
        return idx;
      };

      function writeDepth(str, incDepth, incI) {
        depth += incDepth;
        write(str);
        depth -= incDepth;
        i += incI;
      }

      write('/');
      depth++;
      i = 1;

      while (i < regexpString.length) {
        const ch = regexpString[i];

        if (inClass) {
          if (ch === '\\') {
            let seq = '\\';
            i++;
            if (i < regexpString.length) {
              seq += regexpString[i++];
              const next = seq[1];
              if (next === 'u' && regexpString[i] === '{') {
                writeGroup(`${seq}{`, '}', 0);
                continue;
              } else if ((next === 'p' || next === 'P') && regexpString[i] === '{') {
                writeGroup(`${seq}{`, '}', 0);
                continue;
              } else if (seq[1] === 'x') {
                seq += regexpString.slice(i, i + 2);
                i += 2;
              }
            }
            write(seq);
          } else if (ch === ']') {
            depth--;
            write(']');
            i++;
            inClass = false;
          } else if (ch === '-' && regexpString[i - 1] !== '[' &&
                     i + 1 < regexpString.length && regexpString[i + 1] !== ']') {
            writeDepth('-', 1, 1);
          } else {
            write(ch);
            i++;
          }
        } else if (ch === '[') {
          write('[');
          depth++;
          i++;
          inClass = true;
        } else if (ch === '(') {
          write('(');
          depth++;
          i++;
          if (i < regexpString.length && regexpString[i] === '?') {
            i++;
            const a = i < regexpString.length ? regexpString[i] : '';
            if (a === ':' || a === '=' || a === '!') {
              writeDepth(`?${a}`, -1, 1);
            } else {
              const b = i + 1 < regexpString.length ? regexpString[i + 1] : '';
              if (a === '<' && (b === '=' || b === '!')) {
                writeDepth(`?<${b}`, -1, 2);
              } else if (a === '<') {
                i++; // consume '<'
                const start = i;
                while (i < regexpString.length && regexpString[i] !== '>') {
                  i++;
                }
                const name = regexpString.slice(start, i);
                if (i < regexpString.length && regexpString[i] === '>') {
                  depth--;
                  write('?<');
                  writeDepth(name, 1, 0);
                  write('>');
                  depth++;
                  i++;
                } else {
                  writeDepth('?<', -1, 0);
                  write(name);
                }
              } else {
                write('?');
              }
            }
          }
        } else if (ch === ')') {
          depth--;
          write(')');
          i++;
        } else if (ch === '\\') {
          let seq = '\\';
          i++;
          if (i < regexpString.length) {
            seq += regexpString[i++];
            const next = seq[1];
            if (i < regexpString.length) {
              if (next === 'u' && regexpString[i] === '{') {
                writeGroup(`${seq}{`, '}', 0);
                continue;
              } else if (next === 'x') {
                seq += regexpString.slice(i, i + 2);
                i += 2;
              } else if (next >= '0' && next <= '9') {
                while (i < regexpString.length && regexpString[i] >= '0' && regexpString[i] <= '9') {
                  seq += regexpString[i++];
                }
              } else if (next === 'k' && regexpString[i] === '<') {
                writeGroup(`${seq}<`, '>');
                continue;
              } else if ((next === 'p' || next === 'P') && regexpString[i] === '{') {
                writeGroup(`${seq}{`, '}', 0);
                continue;
              }
            }
          }
          writeDepth(seq, 1, 0);
        } else if (ch === '|' || ch === '+' || ch === '*' || ch === '?' || ch === ',' || ch === '^' || ch === '$') {
          writeDepth(ch, 3, 1);
        } else if (ch === '{') {
          i++;
          let digits = '';
          while (i < regexpString.length && regexpString[i] >= '0' && regexpString[i] <= '9') {
            digits += regexpString[i++];
          }
          if (digits) {
            write('{');
            depth++;
            writeDepth(digits, 1, 0);
          }
          if (i < regexpString.length) {
            if (regexpString[i] === ',') {
              if (!digits) {
                write('{');
                depth++;
              }
              write(',');
              i++;
            } else if (!digits) {
              depth += 1;
              write('{');
              depth -= 1;
              continue;
            }
          }
          let digits2 = '';
          while (i < regexpString.length && regexpString[i] >= '0' && regexpString[i] <= '9') {
            digits2 += regexpString[i++];
          }
          if (digits2) {
            writeDepth(digits2, 1, 0);
          }
          if (i < regexpString.length && regexpString[i] === '}') {
            depth--;
            write('}');
            i++;
          }
          if (i < regexpString.length && regexpString[i] === '?') {
            writeDepth('?', 3, 1);
          }
        } else if (ch === '.') {
          writeDepth(ch, 2, 1);
        } else if (ch === '/') {
          break;
        } else {
          writeDepth(ch, 1, 1);
        }
      }

      writeDepth('/', -1, 1);
      if (i < regexpString.length) {
        write(regexpString.slice(i));
      }
      return out;
    }

    function addQuotes(str, quotes) {
      if (quotes === -1) {
        return `"${str}"`;
      }
      if (quotes === -2) {
        return `\`${str}\``;
      }
      return `'${str}'`;
    }

    function escapeFn(str) {
      const charCode = str.charCodeAt();
      return meta.length > charCode ? meta[charCode] : `\\u${charCode.toString(16)}`;
    }

    function strEscape(str) {
      let escapeTest = strEscapeSequencesRegExp;
      let escapeReplace = strEscapeSequencesReplacer;
      let singleQuote = 39;

      if (str.includes("'")) {
        if (!str.includes('"')) {
          singleQuote = -1;
        } else if (!str.includes('`') &&
                   !str.includes('${')) {
          singleQuote = -2;
        }
        if (singleQuote !== 39) {
          escapeTest = strEscapeSequencesRegExpSingle;
          escapeReplace = strEscapeSequencesReplacerSingle;
        }
      }

      if (str.length < 5000 && escapeTest.exec(str) === null)
        return addQuotes(str, singleQuote);
      if (str.length > 100) {
        str = RegExpPrototypeSymbolReplace(escapeReplace, str, escapeFn);
        return addQuotes(str, singleQuote);
      }

      let result = '';
      let last = 0;
      for (let i = 0; i < str.length; i++) {
        const point = str.charCodeAt(i);
        if (point === singleQuote ||
            point === 92 ||
            point < 32 ||
            (point > 126 && point < 160)) {
          if (last === i) {
            result += meta[point];
          } else {
            result += `${str.slice(last, i)}${meta[point]}`;
          }
          last = i + 1;
        } else if (point >= 0xd800 && point <= 0xdfff) {
          if (point <= 0xdbff && i + 1 < str.length) {
            const point = str.charCodeAt(i + 1);
            if (point >= 0xdc00 && point <= 0xdfff) {
              i++;
              continue;
            }
          }
          result += `${str.slice(last, i)}\\u${point.toString(16)}`;
          last = i + 1;
        }
      }

      if (last !== str.length) {
        result += str.slice(last);
      }
      return addQuotes(result, singleQuote);
    }

    function stylizeWithColor(str, styleType) {
      const style = inspect.styles[styleType];
      if (style !== undefined) {
        const color = inspect.colors[style];
        if (color !== undefined) {
          return `\u001b[${color[0]}m${str}\u001b[${color[1]}m`;
        }
        if (typeof style === 'function') {
          return style(str);
        }
      }
      return str;
    }

    function stylizeNoColor(str) {
      return str;
    }

    function getEmptyFormatArray() {
      return [];
    }

    function isInstanceof(object, proto) {
      try {
        return object instanceof proto;
      } catch {
        return false;
      }
    }

    const wellKnownPrototypes = new SafeMap()
      .set(ArrayPrototype, { name: 'Array', constructor: Array })
      .set(ArrayBufferPrototype, { name: 'ArrayBuffer', constructor: ArrayBuffer })
      .set(FunctionPrototype, { name: 'Function', constructor: Function })
      .set(MapPrototype, { name: 'Map', constructor: Map })
      .set(SetPrototype, { name: 'Set', constructor: Set })
      .set(ObjectPrototype, { name: 'Object', constructor: Object })
      .set(TypedArrayPrototype, { name: 'TypedArray', constructor: TypedArray })
      .set(RegExpPrototype, { name: 'RegExp', constructor: RegExp })
      .set(DatePrototype, { name: 'Date', constructor: Date })
      .set(DataViewPrototype, { name: 'DataView', constructor: DataView })

      .set(ErrorPrototype, { name: 'Error', constructor: Error })
      .set(AggregateErrorPrototype, { name: 'AggregateError', constructor: AggregateError })
      .set(RangeErrorPrototype, { name: 'RangeError', constructor: RangeError })
      .set(TypeErrorPrototype, { name: 'TypeError', constructor: TypeError })

      .set(BooleanPrototype, { name: 'Boolean', constructor: Boolean })
      .set(NumberPrototype, { name: 'Number', constructor: Number })
      .set(StringPrototype, { name: 'String', constructor: String })
      .set(PromisePrototype, { name: 'Promise', constructor: Promise })
      .set(WeakMapPrototype, { name: 'WeakMap', constructor: WeakMap })
      .set(WeakSetPrototype, { name: 'WeakSet', constructor: WeakSet });

    function getConstructorName(obj, ctx, recurseTimes, protoProps) {
      let firstProto;
      const tmp = obj;
      while (obj || isUndetectableObject(obj)) {
        const wellKnownPrototypeNameAndConstructor = wellKnownPrototypes.get(obj);
        if (wellKnownPrototypeNameAndConstructor !== undefined) {
          const { name, constructor } = wellKnownPrototypeNameAndConstructor;
          if (FunctionPrototypeSymbolHasInstance(constructor, tmp)) {
            if (protoProps !== undefined && firstProto !== obj) {
              addPrototypeProperties(
                ctx, tmp, firstProto || tmp, recurseTimes, protoProps);
            }
            return name;
          }
        }
        const descriptor = Object.getOwnPropertyDescriptor(obj, 'constructor');
        if (descriptor !== undefined &&
            typeof descriptor.value === 'function' &&
            descriptor.value.name !== '' &&
            isInstanceof(tmp, descriptor.value)) {
          if (protoProps !== undefined &&
             (firstProto !== obj ||
             !builtInObjects.has(descriptor.value.name))) {
            addPrototypeProperties(
              ctx, tmp, firstProto || tmp, recurseTimes, protoProps);
          }
          return String(descriptor.value.name);
        }

        obj = Object.getPrototypeOf(obj);
        if (firstProto === undefined) {
          firstProto = obj;
        }
      }

      if (firstProto === null) {
        return null;
      }

      const res = internalGetConstructorName(tmp);

      if (recurseTimes > ctx.depth && ctx.depth !== null) {
        return `${res} <Complex prototype>`;
      }

      const protoConstr = getConstructorName(
        firstProto, ctx, recurseTimes + 1, protoProps);

      if (protoConstr === null) {
        return `${res} <${inspect(firstProto, {
          ...ctx,
          customInspect: false,
          depth: -1,
        })}>`;
      }

      return `${res} <${protoConstr}>`;
    }

    function addPrototypeProperties(ctx, main, obj, recurseTimes, output) {
      let depth = 0;
      let keys;
      let keySet;
      do {
        if (depth !== 0 || main === obj) {
          obj = Object.getPrototypeOf(obj);
          if (obj === null) {
            return;
          }
          const descriptor = Object.getOwnPropertyDescriptor(obj, 'constructor');
          if (descriptor !== undefined &&
              typeof descriptor.value === 'function' &&
              builtInObjects.has(descriptor.value.name)) {
            return;
          }
        }

        if (depth === 0) {
          keySet = new SafeSet();
        } else {
          keys.forEach((key) => keySet.add(key));
        }
        keys = Reflect.ownKeys(obj);
        ctx.seen.push(main);
        for (const key of keys) {
          if (key === 'constructor' ||
              ObjectPrototypeHasOwnProperty(main, key) ||
              (depth !== 0 && keySet.has(key))) {
            continue;
          }
          const desc = Object.getOwnPropertyDescriptor(obj, key);
          if (typeof desc.value === 'function') {
            continue;
          }
          const value = formatProperty(
            ctx, obj, recurseTimes, key, kObjectType, desc, main);
          if (ctx.colors) {
            output.push(`\u001b[2m${value}\u001b[22m`);
          } else {
            output.push(value);
          }
        }
        ctx.seen.pop();
      } while (++depth !== 3);
    }

    function getPrefix(constructor, tag, fallback, size = '') {
      if (constructor === null) {
        if (tag !== '' && fallback !== tag) {
          return `[${fallback}${size}: null prototype] [${tag}] `;
        }
        return `[${fallback}${size}: null prototype] `;
      }

      if (tag !== '') {
        const dot = tag.lastIndexOf('.');
        if (constructor === (dot >= 1 ? tag.slice(dot + 1) : tag)) {
          return `${tag}${size} `;
        }
        return `${constructor}${size} [${tag}] `;
      }

      return `${constructor}${size} `;
    }

    function getKeys(value, showHidden) {
      let keys;
      const symbols = ObjectGetOwnPropertySymbols(value);
      if (showHidden) {
        keys = Object.getOwnPropertyNames(value);
        if (symbols.length !== 0)
          ArrayPrototypePushApply(keys, symbols);
      } else {
        try {
          keys = Object.keys(value);
        } catch (err) {
          assert(isNativeError(err) && err.name === 'ReferenceError' &&
                 isModuleNamespaceObject(value));
          keys = Object.getOwnPropertyNames(value);
        }
        if (symbols.length !== 0) {
          const filter = (key) => ObjectPrototypePropertyIsEnumerable(value, key);
          ArrayPrototypePushApply(keys, symbols.filter(filter));
        }
      }
      return keys;
    }

    function getCtxStyle(value, constructor, tag) {
      let fallback = '';
      if (constructor === null) {
        fallback = internalGetConstructorName(value);
        if (fallback === tag) {
          fallback = 'Object';
        }
      }
      return getPrefix(constructor, tag, fallback);
    }

    function formatProxy(ctx, proxy, recurseTimes) {
      if (recurseTimes > ctx.depth && ctx.depth !== null) {
        return ctx.stylize('Proxy [Array]', 'special');
      }
      recurseTimes += 1;
      ctx.indentationLvl += 2;
      const res = [
        formatValue(ctx, proxy[0], recurseTimes),
        formatValue(ctx, proxy[1], recurseTimes),
      ];
      ctx.indentationLvl -= 2;
      return reduceToSingleString(
        ctx, res, '', ['Proxy [', ']'], kArrayExtrasType, recurseTimes);
    }

    function formatValue(ctx, value, recurseTimes, typedArray) {
      if (typeof value !== 'object' &&
          typeof value !== 'function' &&
          !isUndetectableObject(value)) {
        return formatPrimitive(ctx.stylize, value, ctx);
      }
      if (value === null) {
        return ctx.stylize('null', 'null');
      }

      const context = value;
      let proxies = 0;
      let proxy = getProxyDetails(value, !!ctx.showProxy);
      if (proxy !== undefined) {
        if (ctx.showProxy) {
          if (proxy[0] === null) {
            return ctx.stylize('<Revoked Proxy>', 'special');
          }
          return formatProxy(ctx, proxy, recurseTimes);
        }
        do {
          if (proxy === null) {
            let formatted = ctx.stylize('<Revoked Proxy>', 'special');
            for (let i = 0; i < proxies; i++) {
              formatted = `${ctx.stylize('Proxy(', 'special')}${formatted}${ctx.stylize(')', 'special')}`;
            }
            return formatted;
          }
          value = proxy;
          proxy = getProxyDetails(value, false);
          proxies += 1;
        } while (proxy !== undefined);
      }

      if (ctx.customInspect) {
        const maybeCustom = value[customInspectSymbol];
        if (typeof maybeCustom === 'function' &&
            maybeCustom !== inspect &&
            Object.getOwnPropertyDescriptor(value, 'constructor')?.value?.prototype !== value) {
          const depth = ctx.depth === null ? null : ctx.depth - recurseTimes;
          const isCrossContext =
            proxies !== 0 || !FunctionPrototypeSymbolHasInstance(Object, context);
          const ret = FunctionPrototypeCall(
            maybeCustom,
            context,
            depth,
            getUserOptions(ctx, isCrossContext),
            inspect,
          );
          if (ret !== context) {
            if (typeof ret !== 'string') {
              return formatValue(ctx, ret, recurseTimes);
            }
            return ret.replaceAll('\n', `\n${' '.repeat(ctx.indentationLvl)}`);
          }
        }
      }

      if (ctx.seen.includes(value)) {
        let index = 1;
        if (ctx.circular === undefined) {
          ctx.circular = new SafeMap();
          ctx.circular.set(value, index);
        } else {
          index = ctx.circular.get(value);
          if (index === undefined) {
            index = ctx.circular.size + 1;
            ctx.circular.set(value, index);
          }
        }
        return ctx.stylize(`[Circular *${index}]`, 'special');
      }

      let formatted = formatRaw(ctx, value, recurseTimes, typedArray);

      if (proxies !== 0) {
        for (let i = 0; i < proxies; i++) {
          formatted = `${ctx.stylize('Proxy(', 'special')}${formatted}${ctx.stylize(')', 'special')}`;
        }
      }

      return formatted;
    }

    function formatRaw(ctx, value, recurseTimes, typedArray) {
      let keys;
      let protoProps;
      if (ctx.showHidden && (recurseTimes <= ctx.depth || ctx.depth === null)) {
        protoProps = [];
      }

      const constructor = getConstructorName(value, ctx, recurseTimes, protoProps);
      if (protoProps !== undefined && protoProps.length === 0) {
        protoProps = undefined;
      }

      let tag = '';

      try {
        tag = value[SymbolToStringTag];
      } catch {
      }

      if (typeof tag !== 'string' ||
          (tag !== '' &&
          (ctx.showHidden ?
            ObjectPrototypeHasOwnProperty :
            ObjectPrototypePropertyIsEnumerable)(
            value, SymbolToStringTag,
          ))) {
        tag = '';
      }
      let base = '';
      let formatter = getEmptyFormatArray;
      let braces;
      let noIterator = true;
      let i = 0;
      const filter = ctx.showHidden ? ALL_PROPERTIES : ONLY_ENUMERABLE;

      let extrasType = kObjectType;
      let extraKeys;

      if (SymbolIterator in value || constructor === null) {
        noIterator = false;
        if (Array.isArray(value)) {
          const prefix = (constructor !== 'Array' || tag !== '') ?
            getPrefix(constructor, tag, 'Array', `(${value.length})`) :
            '';
          keys = getOwnNonIndexProperties(value, filter);
          braces = [`${prefix}[`, ']'];
          if (value.length === 0 && keys.length === 0 && protoProps === undefined)
            return `${braces[0]}]`;
          extrasType = kArrayExtrasType;
          formatter = formatArray;
        } else if (isSet(value)) {
          const size = SetPrototypeGetSize(value);
          const prefix = getPrefix(constructor, tag, 'Set', `(${size})`);
          keys = getKeys(value, ctx.showHidden);
          formatter = constructor !== null ?
            FunctionPrototypeBind(formatSet, null, value) :
            FunctionPrototypeBind(formatSet, null, SetPrototypeValues(value));
          if (size === 0 && keys.length === 0 && protoProps === undefined)
            return `${prefix}{}`;
          braces = [`${prefix}{`, '}'];
        } else if (isMap(value)) {
          const size = MapPrototypeGetSize(value);
          const prefix = getPrefix(constructor, tag, 'Map', `(${size})`);
          keys = getKeys(value, ctx.showHidden);
          formatter = constructor !== null ?
            FunctionPrototypeBind(formatMap, null, value) :
            FunctionPrototypeBind(formatMap, null, MapPrototypeEntries(value));
          if (size === 0 && keys.length === 0 && protoProps === undefined)
            return `${prefix}{}`;
          braces = [`${prefix}{`, '}'];
        } else if (isTypedArray(value)) {
          keys = getOwnNonIndexProperties(value, filter);
          let bound = value;
          let fallback = '';
          if (constructor === null) {
            fallback = TypedArrayPrototypeGetSymbolToStringTag(value);
            bound = new primordials[fallback](value);
          }
          const size = value.length;
          const prefix = getPrefix(constructor, tag, fallback, `(${size})`);
          braces = [`${prefix}[`, ']'];
          if (value.length === 0 && keys.length === 0 && !ctx.showHidden)
            return `${braces[0]}]`;
          formatter = FunctionPrototypeBind(formatTypedArray, null, bound, size);
          extrasType = kArrayExtrasType;

          if (ctx.showHidden) {
            extraKeys = ['BYTES_PER_ELEMENT', 'length', 'byteLength', 'byteOffset', 'buffer'];
            typedArray = true;
          }
        } else if (isMapIterator(value)) {
          keys = getKeys(value, ctx.showHidden);
          braces = getIteratorBraces('Map', tag);
          formatter = FunctionPrototypeBind(formatIterator, null, braces);
        } else if (isSetIterator(value)) {
          keys = getKeys(value, ctx.showHidden);
          braces = getIteratorBraces('Set', tag);
          formatter = FunctionPrototypeBind(formatIterator, null, braces);
        } else {
          noIterator = true;
        }
      }
      if (noIterator) {
        keys = getKeys(value, ctx.showHidden);
        braces = ['{', '}'];
        if (typeof value === 'function') {
          base = getFunctionBase(ctx, value, constructor, tag);
          if (keys.length === 0 && protoProps === undefined)
            return ctx.stylize(base, 'special');
        } else if (constructor === 'Object') {
          if (isArgumentsObject(value)) {
            braces[0] = '[Arguments] {';
          } else if (tag !== '') {
            braces[0] = `${getPrefix(constructor, tag, 'Object')}{`;
          }
          if (keys.length === 0 && protoProps === undefined) {
            return `${braces[0]}}`;
          }
        } else if (isRegExp(value)) {
          base = (constructor !== null ? value : new RegExp(value)).toString();
          const prefix = getPrefix(constructor, tag, 'RegExp');
          if (prefix !== 'RegExp ')
            base = `${prefix}${base}`;
          base = ctx.stylize(base, 'regexp');
          if ((keys.length === 0 && protoProps === undefined) ||
              (recurseTimes > ctx.depth && ctx.depth !== null)) {
            return base;
          }
        } else if (isDate(value)) {
          base = Number.isNaN(DatePrototypeGetTime(value)) ?
            DatePrototypeToString(value) :
            DatePrototypeToISOString(value);
          const prefix = getPrefix(constructor, tag, 'Date');
          if (prefix !== 'Date ')
            base = `${prefix}${base}`;
          if (keys.length === 0 && protoProps === undefined) {
            return ctx.stylize(base, 'date');
          }
        } else if (isError(value)) {
          base = formatError(value, constructor, tag, ctx, keys);
          if (keys.length === 0 && protoProps === undefined)
            return base;
        } else if (isAnyArrayBuffer(value)) {
          const arrayType = isArrayBuffer(value) ? 'ArrayBuffer' :
            'SharedArrayBuffer';
          const prefix = getPrefix(constructor, tag, arrayType);
          if (typedArray === undefined) {
            formatter = formatArrayBuffer;
          } else if (keys.length === 0 && protoProps === undefined) {
            return prefix +
                  `{ [byteLength]: ${formatNumber(ctx.stylize, value.byteLength, false)} }`;
          }
          braces[0] = `${prefix}{`;
          extraKeys = ['byteLength'];
        } else if (isDataView(value)) {
          braces[0] = `${getPrefix(constructor, tag, 'DataView')}{`;
          extraKeys = ['byteLength', 'byteOffset', 'buffer'];
        } else if (isPromise(value)) {
          braces[0] = `${getPrefix(constructor, tag, 'Promise')}{`;
          formatter = formatPromise;
        } else if (isWeakSet(value)) {
          braces[0] = `${getPrefix(constructor, tag, 'WeakSet')}{`;
          formatter = ctx.showHidden ? formatWeakSet : formatWeakCollection;
        } else if (isWeakMap(value)) {
          braces[0] = `${getPrefix(constructor, tag, 'WeakMap')}{`;
          formatter = ctx.showHidden ? formatWeakMap : formatWeakCollection;
        } else if (isModuleNamespaceObject(value)) {
          braces[0] = `${getPrefix(constructor, tag, 'Module')}{`;
          formatter = formatNamespaceObject.bind(null, keys);
        } else if (isBoxedPrimitive(value)) {
          base = getBoxedBase(value, ctx, keys, constructor, tag);
          if (keys.length === 0 && protoProps === undefined) {
            return base;
          }
        } else if (isURL(value) && !(recurseTimes > ctx.depth && ctx.depth !== null)) {
          base = value.href;
          if (keys.length === 0 && protoProps === undefined) {
            return base;
          }
        } else {
          if (keys.length === 0 && protoProps === undefined) {
            if (isExternal(value)) {
              const address = getExternalValue(value).toString(16);
              return ctx.stylize(`[External: ${address}]`, 'special');
            }
            return `${getCtxStyle(value, constructor, tag)}{}`;
          }
          braces[0] = `${getCtxStyle(value, constructor, tag)}{`;
        }
      }

      if (recurseTimes > ctx.depth && ctx.depth !== null) {
        let constructorName = getCtxStyle(value, constructor, tag).slice(0, -1);
        if (constructor !== null)
          constructorName = `[${constructorName}]`;
        return ctx.stylize(constructorName, 'special');
      }
      recurseTimes += 1;

      ctx.seen.push(value);
      ctx.currentDepth = recurseTimes;
      let output;
      const indentationLvl = ctx.indentationLvl;
      try {
        output = formatter(ctx, value, recurseTimes);
        if (extraKeys !== undefined) {
          for (i = 0; i < extraKeys.length; i++) {
            let formatted;
            try {
              formatted = formatExtraProperties(ctx, value, recurseTimes, extraKeys[i], typedArray);
            } catch {
              const tempValue = { [extraKeys[i]]: value.buffer[extraKeys[i]] };
              formatted = formatExtraProperties(ctx, tempValue, recurseTimes, extraKeys[i], typedArray);
            }
            output.push(formatted);
          }
        }
        for (i = 0; i < keys.length; i++) {
          output.push(formatProperty(ctx, value, recurseTimes, keys[i], extrasType));
        }
        if (protoProps !== undefined) {
          ArrayPrototypePushApply(output, protoProps);
        }
      } catch (err) {
        if (!isStackOverflowError(err)) throw err;
        const constructorName = getCtxStyle(value, constructor, tag).slice(0, -1);
        return handleMaxCallStackSize(ctx, err, constructorName, indentationLvl);
      }
      if (ctx.circular !== undefined) {
        const index = ctx.circular.get(value);
        if (index !== undefined) {
          const reference = ctx.stylize(`<ref *${index}>`, 'special');
          if (ctx.compact !== true) {
            base = base === '' ? reference : `${reference} ${base}`;
          } else {
            braces[0] = `${reference} ${braces[0]}`;
          }
        }
      }
      ctx.seen.pop();

      if (ctx.sorted) {
        const comparator = ctx.sorted === true ? undefined : ctx.sorted;
        if (extrasType === kObjectType) {
          output.sort(comparator);
        } else if (keys.length > 1) {
          const sorted = output.slice(output.length - keys.length).sort(comparator);
          sorted.unshift(output, output.length - keys.length, keys.length);
          Reflect.apply(ArrayPrototypeSplice, null, sorted);
        }
      }

      const res = reduceToSingleString(
        ctx, output, base, braces, extrasType, recurseTimes, value);
      const budget = ctx.budget[ctx.indentationLvl] || 0;
      const newLength = budget + res.length;
      ctx.budget[ctx.indentationLvl] = newLength;
      if (newLength > 2 ** 27) {
        ctx.depth = -1;
      }
      return res;
    }

    function getIteratorBraces(type, tag) {
      if (tag !== `${type} Iterator`) {
        if (tag !== '')
          tag += '] [';
        tag += `${type} Iterator`;
      }
      return [`[${tag}] {`, '}'];
    }

    function getBoxedBase(value, ctx, keys, constructor, tag) {
      let fn;
      let type;
      if (isNumberObject(value)) {
        fn = NumberPrototypeValueOf;
        type = 'Number';
      } else if (isStringObject(value)) {
        fn = StringPrototypeValueOf;
        type = 'String';
        keys.splice(0, value.length);
      } else if (isBooleanObject(value)) {
        fn = BooleanPrototypeValueOf;
        type = 'Boolean';
      } else if (isBigIntObject(value)) {
        fn = BigIntPrototypeValueOf;
        type = 'BigInt';
      } else {
        fn = SymbolPrototypeValueOf;
        type = 'Symbol';
      }
      let base = `[${type}`;
      if (type !== constructor) {
        if (constructor === null) {
          base += ' (null prototype)';
        } else {
          base += ` (${constructor})`;
        }
      }
      base += `: ${formatPrimitive(stylizeNoColor, fn(value), ctx)}]`;
      if (tag !== '' && tag !== constructor) {
        base += ` [${tag}]`;
      }
      if (keys.length !== 0 || ctx.stylize === stylizeNoColor)
        return base;
      return ctx.stylize(base, type.toLowerCase());
    }

    function getClassBase(value, constructor, tag) {
      const hasName = ObjectPrototypeHasOwnProperty(value, 'name');
      const name = (hasName && value.name) || '(anonymous)';
      let base = `class ${name}`;
      if (constructor !== 'Function' && constructor !== null) {
        base += ` [${constructor}]`;
      }
      if (tag !== '' && constructor !== tag) {
        base += ` [${tag}]`;
      }
      if (constructor !== null) {
        const superName = Object.getPrototypeOf(value).name;
        if (superName) {
          base += ` extends ${superName}`;
        }
      } else {
        base += ' extends [null prototype]';
      }
      return `[${base}]`;
    }

    function getFunctionBase(ctx, value, constructor, tag) {
      const stringified = FunctionPrototypeToString(value);
      if (stringified.startsWith('class') && stringified[stringified.length - 1] === '}') {
        const slice = stringified.slice(5, -1);
        const bracketIndex = slice.indexOf('{');
        if (bracketIndex !== -1 &&
            (!slice.slice(0, bracketIndex).includes('(') ||
            classRegExp.exec(RegExpPrototypeSymbolReplace(stripCommentsRegExp, slice)) !== null)
        ) {
          return getClassBase(value, constructor, tag);
        }
      }
      let type = 'Function';
      if (isGeneratorFunction(value)) {
        type = `Generator${type}`;
      }
      if (isAsyncFunction(value)) {
        type = `Async${type}`;
      }
      let base = `[${type}`;
      if (constructor === null) {
        base += ' (null prototype)';
      }
      if (value.name === '') {
        base += ' (anonymous)';
      } else {
        base += `: ${typeof value.name === 'string' ? value.name : formatValue(ctx, value.name)}`;
      }
      base += ']';
      if (constructor !== type && constructor !== null) {
        base += ` ${constructor}`;
      }
      if (tag !== '' && constructor !== tag) {
        base += ` [${tag}]`;
      }
      return base;
    }

    function identicalSequenceRange(a, b) {
      for (let i = 0; i < a.length - 3; i++) {
        const pos = b.indexOf(a[i]);
        if (pos !== -1) {
          const rest = b.length - pos;
          if (rest > 3) {
            let len = 1;
            const maxLen = Math.min(a.length - i, rest);
            while (maxLen > len && a[i + len] === b[pos + len]) {
              len++;
            }
            if (len > 3) {
              return [len, i];
            }
          }
        }
      }

      return [0, 0];
    }

    function getDuplicateErrorFrameRanges(frames) {
      const result = [];
      const lineToPositions = new SafeMap();

      for (let i = 0; i < frames.length; i++) {
        const positions = lineToPositions.get(frames[i]);
        if (positions === undefined) {
          lineToPositions.set(frames[i], [i]);
        } else {
          positions[positions.length] = i;
        }
      }

      const minimumDuplicateRange = 3;
      if (frames.length - lineToPositions.size <= minimumDuplicateRange) {
        return result;
      }

      for (let i = 0; i < frames.length - minimumDuplicateRange; i++) {
        const positions = lineToPositions.get(frames[i]);
        if (positions.length === 1 || positions[positions.length - 1] === i) {
          continue;
        }

        const current = positions.indexOf(i) + 1;
        if (current === positions.length) {
          continue;
        }

        let range = positions[positions.length - 1] - i;
        if (range < minimumDuplicateRange) {
          continue;
        }
        let extraSteps;
        if (current + 1 < positions.length) {
          let gcdRange = 0;
          for (let j = current; j < positions.length; j++) {
            let distance = positions[j] - i;
            while (distance !== 0) {
              const remainder = gcdRange % distance;
              if (gcdRange !== 0) {
                extraSteps ??= new SafeSet();
                extraSteps.add(gcdRange);
              }
              gcdRange = distance;
              distance = remainder;
            }
            if (gcdRange === 1) break;
          }
          range = gcdRange;
          if (extraSteps) {
            extraSteps.delete(range);
            extraSteps = [...extraSteps];
          }
        }
        let maxRange = range;
        let maxDuplicates = 0;

        let duplicateRanges = 0;

        for (let nextStart = i + range; /* ignored */ ; nextStart += range) {
          let equalFrames = 0;
          for (let j = 0; j < range; j++) {
            if (frames[i + j] !== frames[nextStart + j]) {
              break;
            }
            equalFrames++;
          }
          if (equalFrames !== range) {
            if (!extraSteps?.length) {
              break;
            }
            if (duplicateRanges !== 0 && maxRange * maxDuplicates < range * duplicateRanges) {
              maxRange = range;
              maxDuplicates = duplicateRanges;
            }
            range = extraSteps.pop();
            nextStart = i;
            duplicateRanges = 0;
            continue;
          }
          duplicateRanges++;
        }

        if (maxDuplicates !== 0 && maxRange * maxDuplicates >= range * duplicateRanges) {
          range = maxRange;
          duplicateRanges = maxDuplicates;
        }

        if (duplicateRanges * range >= 3) {
          result.push(i + range, range, duplicateRanges);
          i += range * (duplicateRanges + 1) - 1;
        }
      }

      return result;
    }

    function getStackString(ctx, error) {
      let stack;
      try {
        stack = error.stack;
      } catch {
      }
      if (stack) {
        if (typeof stack === 'string') {
          return stack;
        }
        ctx.seen.push(error);
        ctx.indentationLvl += 4;
        const result = formatValue(ctx, stack);
        ctx.indentationLvl -= 4;
        ctx.seen.pop();
        return `${ErrorPrototypeToString(error)}\n    ${result}`;
      }
      return ErrorPrototypeToString(error);
    }

    function getStackFrames(ctx, err, stack) {
      const frames = stack.split('\n');

      let cause;
      try {
        ({ cause } = err);
      } catch {
      }

      if (cause != null && isError(cause)) {
        const causeStack = getStackString(ctx, cause);
        const causeStackStart = causeStack.indexOf('\n    at');
        if (causeStackStart !== -1) {
          const causeFrames = causeStack.slice(causeStackStart + 1).split('\n');
          const { 0: len, 1: offset } = identicalSequenceRange(frames, causeFrames);
          if (len > 0) {
            const skipped = len - 2;
            const msg = `    ... ${skipped} lines matching cause stack trace ...`;
            frames.splice(offset + 1, skipped, ctx.stylize(msg, 'undefined'));
          }
        }
      }

      if (frames.length > 10) {
        const ranges = getDuplicateErrorFrameRanges(frames);

        for (let i = ranges.length - 3; i >= 0; i -= 3) {
          const offset = ranges[i];
          const length = ranges[i + 1];
          const duplicateRanges = ranges[i + 2];

          const msg = `    ... collapsed ${length * duplicateRanges} duplicate lines ` +
            'matching above ' +
            (duplicateRanges > 1 ?
              `${length} lines ${duplicateRanges} times...` :
              'lines ...');
          frames.splice(offset, length * duplicateRanges, ctx.stylize(msg, 'undefined'));
        }
      }

      return frames;
    }

    function improveStack(stack, constructor, name, tag) {
      let len = name.length;

      if (typeof name !== 'string') {
        stack = stack.replace(`${name}`, `${name} [${getPrefix(constructor, tag, 'Error').slice(0, -1)}]`);
      }

      if (constructor === null ||
          (name.endsWith('Error') &&
          stack.startsWith(name) &&
          (stack.length === len || stack[len] === ':' || stack[len] === '\n'))) {
        let fallback = 'Error';
        if (constructor === null) {
          const start = (/^([A-Z][a-z_ A-Z0-9[\]()-]+)(?::|\n {4}at)/).exec(stack) ||
          (/^([a-z_A-Z0-9-]*Error)$/).exec(stack);
          fallback = (start?.[1]) || '';
          len = fallback.length;
          fallback ||= 'Error';
        }
        const prefix = getPrefix(constructor, tag, fallback).slice(0, -1);
        if (name !== prefix) {
          if (prefix.includes(name)) {
            if (len === 0) {
              stack = `${prefix}: ${stack}`;
            } else {
              stack = `${prefix}${stack.slice(len)}`;
            }
          } else {
            stack = `${prefix} [${name}]${stack.slice(len)}`;
          }
        }
      }
      return stack;
    }

    function markNodeModules(ctx, line) {
      let tempLine = '';
      let lastPos = 0;
      let searchFrom = 0;

      while (true) {
        const nodeModulePosition = line.indexOf('node_modules', searchFrom);
        if (nodeModulePosition === -1) {
          break;
        }

        const separator = line[nodeModulePosition - 1];
        const after = line[nodeModulePosition + 12]; // 'node_modules'.length === 12

        if ((after !== '/' && after !== '\\') || (separator !== '/' && separator !== '\\')) {
          searchFrom = nodeModulePosition + 1;
          continue;
        }

        const moduleStart = nodeModulePosition + 13; // Include trailing separator

        tempLine += line.slice(lastPos, moduleStart);

        let moduleEnd = line.indexOf(separator, moduleStart);
        if (moduleEnd === -1) {
          moduleEnd = line.length;
        } else if (line[moduleStart] === '@') {
          moduleEnd = line.indexOf(separator, moduleEnd + 1);
          if (moduleEnd === -1) {
            moduleEnd = line.length;
          }
        }

        const nodeModule = line.slice(moduleStart, moduleEnd);
        tempLine += ctx.stylize(nodeModule, 'module');

        lastPos = moduleEnd;
        searchFrom = moduleEnd;
      }

      if (lastPos !== 0) {
        line = tempLine + line.slice(lastPos);
      }
      return line;
    }

    function markCwd(ctx, line, workingDirectory) {
      let cwdStartPos = line.indexOf(workingDirectory);
      let tempLine = '';
      let cwdLength = workingDirectory.length;
      if (cwdStartPos !== -1) {
        if (line.slice(cwdStartPos - 7, cwdStartPos) === 'file://') {
          cwdLength += 7;
          cwdStartPos -= 7;
        }
        const start = line[cwdStartPos - 1] === '(' ? cwdStartPos - 1 : cwdStartPos;
        const end = start !== cwdStartPos && line.endsWith(')') ? -1 : line.length;
        const workingDirectoryEndPos = cwdStartPos + cwdLength + 1;
        const cwdSlice = line.slice(start, workingDirectoryEndPos);

        tempLine += line.slice(0, start);
        tempLine += ctx.stylize(cwdSlice, 'undefined');
        tempLine += line.slice(workingDirectoryEndPos, end);
        if (end === -1) {
          tempLine += ctx.stylize(')', 'undefined');
        }
      } else {
        tempLine += line;
      }
      return tempLine;
    }

    function safeGetCWD() {
      let workingDirectory;
      try {
        workingDirectory = process.cwd();
      } catch {
        return;
      }
      return workingDirectory;
    }

    function formatError(err, constructor, tag, ctx, keys) {
      let message, name, stack;
      try {
        stack = getStackString(ctx, err);
      } catch {
        return ObjectPrototypeToString(err);
      }

      let messageIsGetterThatThrows = false;
      try {
        message = err.message;
      } catch {
        messageIsGetterThatThrows = true;
      }
      let nameIsGetterThatThrows = false;
      try {
        name = err.name;
      } catch {
        nameIsGetterThatThrows = true;
      }

      if (!ctx.showHidden && keys.length !== 0) {
        const index = keys.indexOf('stack');
        if (index !== -1) {
          keys.splice(index, 1);
        }

        if (!messageIsGetterThatThrows) {
          const index = keys.indexOf('message');
          if (index !== -1 && (typeof message !== 'string' || stack.includes(message))) {
            keys.splice(index, 1);
          }
        }

        if (!nameIsGetterThatThrows) {
          const index = keys.indexOf('name');
          if (index !== -1 && (typeof name !== 'string' || stack.includes(name))) {
            keys.splice(index, 1);
          }
        }
      }
      name ??= 'Error';

      if (ObjectPrototypeHasOwnProperty(err, 'cause') &&
          (keys.length === 0 || !keys.includes('cause'))) {
        keys.push('cause');
      }

      try {
        const errors = err.errors;
        if (Array.isArray(errors) && ObjectPrototypeHasOwnProperty(err, 'errors') &&
          (keys.length === 0 || !keys.includes('errors'))) {
          keys.push('errors');
        }
      } catch {
      }

      if (ObjectPrototypeHasOwnProperty(err, 'error') &&
          (keys.length === 0 || !keys.includes('error'))) {
        keys.push('error');
      }
      if (ObjectPrototypeHasOwnProperty(err, 'suppressed') &&
          (keys.length === 0 || !keys.includes('suppressed'))) {
        keys.push('suppressed');
      }

      stack = improveStack(stack, constructor, name, tag);

      let pos = (message && stack.indexOf(message)) || -1;
      if (pos !== -1)
        pos += message.length;
      const stackStart = stack.indexOf('\n    at', pos);
      if (stackStart === -1) {
        stack = `[${stack}]`;
      } else {
        let newStack = stack.slice(0, stackStart);
        const stackFramePart = stack.slice(stackStart + 1);
        const lines = getStackFrames(ctx, err, stackFramePart);
        if (ctx.colors) {
          const workingDirectory = safeGetCWD();
          let esmWorkingDirectory;
          for (let line of lines) {
            const core = coreModuleRegExp.exec(line);
            if (core !== null && BuiltinModule.exists(core[1])) {
              newStack += `\n${ctx.stylize(line, 'undefined')}`;
            } else {
              newStack += '\n';

              line = markNodeModules(ctx, line);
              if (workingDirectory !== undefined) {
                let newLine = markCwd(ctx, line, workingDirectory);
                if (newLine === line) {
                  esmWorkingDirectory ??= pathToFileUrlHref(workingDirectory);
                  newLine = markCwd(ctx, line, esmWorkingDirectory);
                }
                line = newLine;
              }

              newStack += line;
            }
          }
        } else {
          newStack += `\n${lines.join('\n')}`;
        }
        stack = newStack;
      }
      if (ctx.indentationLvl !== 0) {
        const indentation = ' '.repeat(ctx.indentationLvl);
        stack = stack.replaceAll('\n', `\n${indentation}`);
      }
      return stack;
    }

    function groupArrayElements(ctx, output, value) {
      let totalLength = 0;
      let maxLength = 0;
      let i = 0;
      let outputLength = output.length;
      if (ctx.maxArrayLength < output.length) {
        outputLength--;
      }
      const separatorSpace = 2; // Add 1 for the space and 1 for the separator.
      const dataLen = new Array(outputLength);
      for (; i < outputLength; i++) {
        const len = getStringWidth(output[i], ctx.colors);
        dataLen[i] = len;
        totalLength += len + separatorSpace;
        if (maxLength < len)
          maxLength = len;
      }
      const actualMax = maxLength + separatorSpace;
      if (actualMax * 3 + ctx.indentationLvl < ctx.breakLength &&
          (totalLength / actualMax > 5 || maxLength <= 6)) {

        const approxCharHeights = 2.5;
        const averageBias = MathSqrt(actualMax - totalLength / output.length);
        const biasedMax = Math.max(actualMax - 3 - averageBias, 1);
        const columns = Math.min(
          Math.round(
            MathSqrt(
              approxCharHeights * biasedMax * outputLength,
            ) / biasedMax,
          ),
          Math.floor((ctx.breakLength - ctx.indentationLvl) / actualMax),
          ctx.compact * 4,
          15,
        );
        if (columns <= 1) {
          return output;
        }
        const tmp = [];
        const maxLineLength = [];
        for (let i = 0; i < columns; i++) {
          let lineMaxLength = 0;
          for (let j = i; j < output.length; j += columns) {
            if (dataLen[j] > lineMaxLength)
              lineMaxLength = dataLen[j];
          }
          lineMaxLength += separatorSpace;
          maxLineLength[i] = lineMaxLength;
        }
        let order = StringPrototypePadStart;
        if (value !== undefined) {
          for (let i = 0; i < output.length; i++) {
            if (typeof value[i] !== 'number' && typeof value[i] !== 'bigint') {
              order = StringPrototypePadEnd;
              break;
            }
          }
        }
        for (let i = 0; i < outputLength; i += columns) {
          const max = Math.min(i + columns, outputLength);
          let str = '';
          let j = i;
          for (; j < max - 1; j++) {
            const padding = maxLineLength[j - i] + output[j].length - dataLen[j];
            str += order(`${output[j]}, `, padding, ' ');
          }
          if (order === StringPrototypePadStart) {
            const padding = maxLineLength[j - i] +
                            output[j].length -
                            dataLen[j] -
                            separatorSpace;
            str += output[j].padStart(padding, ' ');
          } else {
            str += output[j];
          }
          tmp.push(str);
        }
        if (ctx.maxArrayLength < output.length) {
          tmp.push(output[outputLength]);
        }
        output = tmp;
      }
      return output;
    }

    function handleMaxCallStackSize(ctx, err, constructorName, indentationLvl) {
      ctx.seen.pop();
      ctx.indentationLvl = indentationLvl;
      return ctx.stylize(
        `[${constructorName}: Inspection interrupted ` +
          'prematurely. Maximum call stack size exceeded.]',
        'special',
      );
    }

    function addNumericSeparator(integerString) {
      let result = '';
      let i = integerString.length;
      assert(i !== 0);
      const start = integerString[0] === '-' ? 1 : 0;
      for (; i >= start + 4; i -= 3) {
        result = `_${integerString.slice(i - 3, i)}${result}`;
      }
      return i === integerString.length ?
        integerString :
        `${integerString.slice(0, i)}${result}`;
    }

    function addNumericSeparatorEnd(integerString) {
      let result = '';
      let i = 0;
      for (; i < integerString.length - 3; i += 3) {
        result += `${integerString.slice(i, i + 3)}_`;
      }
      return i === 0 ?
        integerString :
        `${result}${integerString.slice(i)}`;
    }

    const remainingText = (remaining) => `... ${remaining} more item${remaining > 1 ? 's' : ''}`;

    function formatNumber(fn, number, numericSeparator) {
      if (Object.is(number, -0)) {
        return fn('-0', 'number');
      }
      if (!numericSeparator) {
        return fn(`${number}`, 'number');
      }

      const numberString = String(number);
      const integer = Math.trunc(number);

      if (integer === number) {
        if (!Number.isFinite(number) || numberString.includes('e')) {
          return fn(numberString, 'number');
        }
        return fn(addNumericSeparator(numberString), 'number');
      }
      if (Number.isNaN(number) || numberString.includes('e')) {
        return fn(numberString, 'number');
      }

      const decimalIndex = numberString.indexOf('.');
      const integerPart = numberString.slice(0, decimalIndex);
      const fractionalPart = numberString.slice(decimalIndex + 1);

      return fn(`${
        addNumericSeparator(integerPart)
      }.${
        addNumericSeparatorEnd(fractionalPart)
      }`, 'number');
    }

    function formatBigInt(fn, bigint, numericSeparator) {
      const string = String(bigint);
      if (!numericSeparator) {
        return fn(`${string}n`, 'bigint');
      }
      return fn(`${addNumericSeparator(string)}n`, 'bigint');
    }

    function formatPrimitive(fn, value, ctx) {
      if (typeof value === 'string') {
        let trailer = '';
        if (value.length > ctx.maxStringLength) {
          const remaining = value.length - ctx.maxStringLength;
          value = value.slice(0, ctx.maxStringLength);
          trailer = `... ${remaining} more character${remaining > 1 ? 's' : ''}`;
        }
        if (ctx.compact !== true &&
            value.length > kMinLineLength &&
            value.length > ctx.breakLength - ctx.indentationLvl - 4) {
          return ((RegExpPrototypeSymbolSplit(/(?<=\n)/, value)).map((line) => fn(strEscape(line), 'string'))).join(` +\n${' '.repeat(ctx.indentationLvl + 2)}`) + trailer;
        }
        return fn(strEscape(value), 'string') + trailer;
      }
      if (typeof value === 'number')
        return formatNumber(fn, value, ctx.numericSeparator);
      if (typeof value === 'bigint')
        return formatBigInt(fn, value, ctx.numericSeparator);
      if (typeof value === 'boolean')
        return fn(`${value}`, 'boolean');
      if (typeof value === 'undefined')
        return fn('undefined', 'undefined');
      return fn(SymbolPrototypeToString(value), 'symbol');
    }

    function formatNamespaceObject(keys, ctx, value, recurseTimes) {
      const output = new Array(keys.length);
      for (let i = 0; i < keys.length; i++) {
        try {
          output[i] = formatProperty(ctx, value, recurseTimes, keys[i],
                                     kObjectType);
        } catch (err) {
          assert(isNativeError(err) && err.name === 'ReferenceError');
          const tmp = { [keys[i]]: '' };
          output[i] = formatProperty(ctx, tmp, recurseTimes, keys[i], kObjectType);
          const pos = output[i].lastIndexOf(' ');
          output[i] = output[i].slice(0, pos + 1) +
                      ctx.stylize('<uninitialized>', 'special');
        }
      }
      keys.length = 0;
      return output;
    }

    function formatSpecialArray(ctx, value, recurseTimes, maxLength, output, i) {
      const keys = Object.keys(value);
      let index = i;
      for (; i < keys.length && output.length < maxLength; i++) {
        const key = keys[i];
        const tmp = +key;
        if (tmp > 2 ** 32 - 2) {
          break;
        }
        if (`${index}` !== key) {
          if (numberRegExp.exec(key) === null) {
            break;
          }
          const emptyItems = tmp - index;
          const ending = emptyItems > 1 ? 's' : '';
          const message = `<${emptyItems} empty item${ending}>`;
          output.push(ctx.stylize(message, 'undefined'));
          index = tmp;
          if (output.length === maxLength) {
            break;
          }
        }
        output.push(formatProperty(ctx, value, recurseTimes, key, kArrayType));
        index++;
      }
      const remaining = value.length - index;
      if (output.length !== maxLength) {
        if (remaining > 0) {
          const ending = remaining > 1 ? 's' : '';
          const message = `<${remaining} empty item${ending}>`;
          output.push(ctx.stylize(message, 'undefined'));
        }
      } else if (remaining > 0) {
        output.push(remainingText(remaining));
      }
      return output;
    }

    function formatArrayBuffer(ctx, value) {
      let buffer;
      try {
        buffer = new Uint8Array(value);
      } catch {
        return [ctx.stylize('(detached)', 'special')];
      }
      if (hexSlice === undefined)
        hexSlice = uncurryThis(require('buffer').Buffer.prototype.hexSlice);
      const rawString = hexSlice(buffer, 0, Math.min(ctx.maxArrayLength, buffer.length));
      let str = '';
      let i = 0;
      for (; i < rawString.length - 2; i += 2) {
        str += `${rawString[i]}${rawString[i + 1]} `;
      }
      if (rawString.length > 0) {
        str += `${rawString[i]}${rawString[i + 1]}`;
      }
      const remaining = buffer.length - ctx.maxArrayLength;
      if (remaining > 0)
        str += ` ... ${remaining} more byte${remaining > 1 ? 's' : ''}`;
      return [`${ctx.stylize('[Uint8Contents]', 'special')}: <${str}>`];
    }

    function formatArray(ctx, value, recurseTimes) {
      const valLen = value.length;
      const len = Math.min(Math.max(0, ctx.maxArrayLength), valLen);

      const remaining = valLen - len;
      const output = [];
      for (let i = 0; i < len; i++) {
        const desc = Object.getOwnPropertyDescriptor(value, i);
        if (desc === undefined) {
          return formatSpecialArray(ctx, value, recurseTimes, len, output, i);
        }
        output.push(formatProperty(ctx, value, recurseTimes, i, kArrayType, desc));
      }
      if (remaining > 0) {
        output.push(remainingText(remaining));
      }
      return output;
    }

    function formatTypedArray(value, length, ctx) {
      const maxLength = Math.min(Math.max(0, ctx.maxArrayLength), length);
      const remaining = value.length - maxLength;
      const output = new Array(maxLength);
      const elementFormatter = value.length > 0 && typeof value[0] === 'number' ?
        formatNumber :
        formatBigInt;
      for (let i = 0; i < maxLength; ++i) {
        output[i] = elementFormatter(ctx.stylize, value[i], ctx.numericSeparator);
      }
      if (remaining > 0) {
        output[maxLength] = remainingText(remaining);
      }
      return output;
    }

    function formatSet(value, ctx, ignored, recurseTimes) {
      const length = value.size;
      const maxLength = Math.min(Math.max(0, ctx.maxArrayLength), length);
      const remaining = length - maxLength;
      const output = [];
      ctx.indentationLvl += 2;
      let i = 0;
      for (const v of value) {
        if (i >= maxLength) break;
        output.push(formatValue(ctx, v, recurseTimes));
        i++;
      }
      if (remaining > 0) {
        output.push(remainingText(remaining));
      }
      ctx.indentationLvl -= 2;
      return output;
    }

    function formatMap(value, ctx, ignored, recurseTimes) {
      const length = value.size;
      const maxLength = Math.min(Math.max(0, ctx.maxArrayLength), length);
      const remaining = length - maxLength;
      const output = [];
      ctx.indentationLvl += 2;
      let i = 0;
      for (const { 0: k, 1: v } of value) {
        if (i >= maxLength) break;
        output.push(`${formatValue(ctx, k, recurseTimes)} => ${formatValue(ctx, v, recurseTimes)}`);
        i++;
      }
      if (remaining > 0) {
        output.push(remainingText(remaining));
      }
      ctx.indentationLvl -= 2;
      return output;
    }

    function formatSetIterInner(ctx, recurseTimes, entries, state) {
      const maxArrayLength = Math.max(ctx.maxArrayLength, 0);
      const maxLength = Math.min(maxArrayLength, entries.length);
      const output = new Array(maxLength);
      ctx.indentationLvl += 2;
      for (let i = 0; i < maxLength; i++) {
        output[i] = formatValue(ctx, entries[i], recurseTimes);
      }
      ctx.indentationLvl -= 2;
      if (state === kWeak && !ctx.sorted) {
        output.sort();
      }
      const remaining = entries.length - maxLength;
      if (remaining > 0) {
        output.push(remainingText(remaining));
      }
      return output;
    }

    function formatMapIterInner(ctx, recurseTimes, entries, state) {
      const maxArrayLength = Math.max(ctx.maxArrayLength, 0);
      const len = entries.length / 2;
      const remaining = len - maxArrayLength;
      const maxLength = Math.min(maxArrayLength, len);
      const output = new Array(maxLength);
      let i = 0;
      ctx.indentationLvl += 2;
      if (state === kWeak) {
        for (; i < maxLength; i++) {
          const pos = i * 2;
          output[i] =
            `${formatValue(ctx, entries[pos], recurseTimes)} => ${formatValue(ctx, entries[pos + 1], recurseTimes)}`;
        }
        if (!ctx.sorted)
          output.sort();
      } else {
        for (; i < maxLength; i++) {
          const pos = i * 2;
          const res = [
            formatValue(ctx, entries[pos], recurseTimes),
            formatValue(ctx, entries[pos + 1], recurseTimes),
          ];
          output[i] = reduceToSingleString(
            ctx, res, '', ['[', ']'], kArrayExtrasType, recurseTimes);
        }
      }
      ctx.indentationLvl -= 2;
      if (remaining > 0) {
        output.push(remainingText(remaining));
      }
      return output;
    }

    function formatWeakCollection(ctx) {
      return [ctx.stylize('<items unknown>', 'special')];
    }

    function formatWeakSet(ctx, value, recurseTimes) {
      const entries = previewEntries(value);
      return formatSetIterInner(ctx, recurseTimes, entries, kWeak);
    }

    function formatWeakMap(ctx, value, recurseTimes) {
      const entries = previewEntries(value);
      return formatMapIterInner(ctx, recurseTimes, entries, kWeak);
    }

    function formatIterator(braces, ctx, value, recurseTimes) {
      const { 0: entries, 1: isKeyValue } = previewEntries(value, true);
      if (isKeyValue) {
        braces[0] = RegExpPrototypeSymbolReplace(/ Iterator] {$/, braces[0], ' Entries] {');
        return formatMapIterInner(ctx, recurseTimes, entries, kMapEntries);
      }

      return formatSetIterInner(ctx, recurseTimes, entries, kIterator);
    }

    function formatPromise(ctx, value, recurseTimes) {
      let output;
      const { 0: state, 1: result } = getPromiseDetails(value);
      if (state === kPending) {
        output = [ctx.stylize('<pending>', 'special')];
      } else {
        ctx.indentationLvl += 2;
        const str = formatValue(ctx, result, recurseTimes);
        ctx.indentationLvl -= 2;
        output = [
          state === kRejected ?
            `${ctx.stylize('<rejected>', 'special')} ${str}` :
            str,
        ];
      }
      return output;
    }

    function formatExtraProperties(ctx, value, recurseTimes, key, typedArray) {
      const extraValue = value[key];
      ctx.indentationLvl += 2;
      const str = formatValue(ctx, extraValue, recurseTimes, typedArray);
      ctx.indentationLvl -= 2;

      const name = ctx.stylize(`[${key}]`, 'string');
      return `${name}: ${str}`;
    }

    function formatProperty(ctx, value, recurseTimes, key, type, desc,
                            original = value) {
      let name, str;
      let extra = ' ';
      desc ??= Object.getOwnPropertyDescriptor(value, key);
      if (desc.value !== undefined) {
        const diff = (ctx.compact !== true || type !== kObjectType) ? 2 : 3;
        ctx.indentationLvl += diff;
        str = formatValue(ctx, desc.value, recurseTimes);
        if (diff === 3 && ctx.breakLength < getStringWidth(str, ctx.colors)) {
          extra = `\n${' '.repeat(ctx.indentationLvl)}`;
        }
        ctx.indentationLvl -= diff;
      } else if (desc.get !== undefined) {
        const label = desc.set !== undefined ? 'Getter/Setter' : 'Getter';
        const s = ctx.stylize;
        const sp = 'special';
        if (ctx.getters && (ctx.getters === true ||
              (ctx.getters === 'get' && desc.set === undefined) ||
              (ctx.getters === 'set' && desc.set !== undefined))) {
          ctx.indentationLvl += 2;
          try {
            const tmp = FunctionPrototypeCall(desc.get, original);
            if (tmp === null) {
              str = `${s(`[${label}:`, sp)} ${s('null', 'null')}${s(']', sp)}`;
            } else if (typeof tmp === 'object' || typeof tmp === 'function') {
              str = `${s(`[${label}]`, sp)} ${formatValue(ctx, tmp, recurseTimes)}`;
            } else {
              const primitive = formatPrimitive(s, tmp, ctx);
              str = `${s(`[${label}:`, sp)} ${primitive}${s(']', sp)}`;
            }
          } catch (err) {
            const message = `<Inspection threw (${formatValue(ctx, err, recurseTimes)})>`;
            str = `${s(`[${label}:`, sp)} ${message}${s(']', sp)}`;
          }
          ctx.indentationLvl -= 2;
        } else {
          str = ctx.stylize(`[${label}]`, sp);
        }
      } else if (desc.set !== undefined) {
        str = ctx.stylize('[Setter]', 'special');
      } else {
        str = ctx.stylize('undefined', 'undefined');
      }
      if (type === kArrayType) {
        return str;
      }
      if (typeof key === 'symbol') {
        const tmp = RegExpPrototypeSymbolReplace(
          strEscapeSequencesReplacer,
          SymbolPrototypeToString(key),
          escapeFn,
        );
        name = ctx.stylize(tmp, 'symbol');
      } else if (keyStrRegExp.exec(key) !== null) {
        name = key === '__proto__' ? "['__proto__']" : ctx.stylize(key, 'name');
      } else {
        name = ctx.stylize(strEscape(key), 'string');
      }

      if (desc.enumerable === false) {
        name = `[${name}]`;
      }
      return `${name}:${extra}${str}`;
    }

    function isBelowBreakLength(ctx, output, start, base) {
      if (ctx.breakLength === Infinity) {
        if (base !== '' && base.includes('\n')) return false;
        for (let i = 0; i < output.length; i++) {
          if (typeof output[i] === 'string' && output[i].includes('\n')) return false;
        }
        return true;
      }
      let totalLength = output.length + start;
      if (totalLength + output.length > ctx.breakLength)
        return false;
      for (let i = 0; i < output.length; i++) {
        if (ctx.colors) {
          totalLength += removeColors(output[i]).length;
        } else {
          totalLength += output[i].length;
        }
        if (totalLength > ctx.breakLength) {
          return false;
        }
      }
      return base === '' || !base.includes('\n');
    }

    function reduceToSingleString(
      ctx, output, base, braces, extrasType, recurseTimes, value) {
      if (ctx.compact !== true) {
        if (typeof ctx.compact === 'number' && ctx.compact >= 1) {
          const entries = output.length;
          if (extrasType === kArrayExtrasType && entries > 6) {
            output = groupArrayElements(ctx, output, value);
          }
          if (ctx.currentDepth - recurseTimes < ctx.compact &&
              entries === output.length) {
            const start = output.length + ctx.indentationLvl +
                          braces[0].length + base.length + 10;
            if (isBelowBreakLength(ctx, output, start, base)) {
              const joinedOutput = join(output, ', ');
              if (!joinedOutput.includes('\n')) {
                return `${base ? `${base} ` : ''}${braces[0]} ${joinedOutput}` +
                  ` ${braces[1]}`;
              }
            }
          }
        }
        const indentation = `\n${' '.repeat(ctx.indentationLvl)}`;
        return `${base ? `${base} ` : ''}${braces[0]}${indentation}  ` +
          `${join(output, `,${indentation}  `)}${indentation}${braces[1]}`;
      }
      if (isBelowBreakLength(ctx, output, 0, base)) {
        return `${braces[0]}${base ? ` ${base}` : ''} ${join(output, ', ')} ` +
          braces[1];
      }
      const indentation = ' '.repeat(ctx.indentationLvl);
      const ln = base === '' && braces[0].length === 1 ?
        ' ' : `${base ? ` ${base}` : ''}\n${indentation}  `;
      return `${braces[0]}${ln}${join(output, `,\n${indentation}  `)} ${braces[1]}`;
    }

    function hasBuiltInToString(value) {
      const getFullProxy = false;
      const proxyTarget = getProxyDetails(value, getFullProxy);
      if (proxyTarget !== undefined) {
        if (proxyTarget === null) {
          return true;
        }
        return hasBuiltInToString(proxyTarget);
      }

      let hasOwnToString = ObjectPrototypeHasOwnProperty;
      let hasOwnToPrimitive = ObjectPrototypeHasOwnProperty;

      if (typeof value.toString !== 'function') {
        if (typeof value[SymbolToPrimitive] !== 'function') {
          return true;
        } else if (ObjectPrototypeHasOwnProperty(value, SymbolToPrimitive)) {
          return false;
        }
        hasOwnToString = returnFalse;
      } else if (ObjectPrototypeHasOwnProperty(value, 'toString')) {
        return false;
      } else if (typeof value[SymbolToPrimitive] !== 'function') {
        hasOwnToPrimitive = returnFalse;
      } else if (ObjectPrototypeHasOwnProperty(value, SymbolToPrimitive)) {
        return false;
      }

      let pointer = value;
      do {
        pointer = Object.getPrototypeOf(pointer);
      } while (!hasOwnToString(pointer, 'toString') &&
        !hasOwnToPrimitive(pointer, SymbolToPrimitive));

      const descriptor = Object.getOwnPropertyDescriptor(pointer, 'constructor');
      return descriptor !== undefined &&
        typeof descriptor.value === 'function' &&
        builtInObjects.has(descriptor.value.name);
    }

    function returnFalse() {
      return false;
    }

    const firstErrorLine = (error) => error.message.split('\n', 1)[0];
    let CIRCULAR_ERROR_MESSAGE;
    function tryStringify(arg) {
      try {
        return JSON.stringify(arg);
      } catch (err) {
        if (!CIRCULAR_ERROR_MESSAGE) {
          try {
            const a = {};
            a.a = a;
            JSON.stringify(a);
          } catch (circularError) {
            CIRCULAR_ERROR_MESSAGE = firstErrorLine(circularError);
          }
        }
        if (err.name === 'TypeError' &&
            firstErrorLine(err) === CIRCULAR_ERROR_MESSAGE) {
          return '[Circular]';
        }
        throw err;
      }
    }

    function format(...args) {
      return formatWithOptionsInternal(undefined, args);
    }

    function formatWithOptions(inspectOptions, ...args) {
      validateObject(inspectOptions, 'inspectOptions', kValidateObjectAllowArray);
      return formatWithOptionsInternal(inspectOptions, args);
    }

    function formatNumberNoColor(number, options) {
      return formatNumber(
        stylizeNoColor,
        number,
        options?.numericSeparator ?? inspectDefaultOptions.numericSeparator,
      );
    }

    function formatBigIntNoColor(bigint, options) {
      return formatBigInt(
        stylizeNoColor,
        bigint,
        options?.numericSeparator ?? inspectDefaultOptions.numericSeparator,
      );
    }

    function formatWithOptionsInternal(inspectOptions, args) {
      const first = args[0];
      let a = 0;
      let str = '';
      let join = '';

      if (typeof first === 'string') {
        if (args.length === 1) {
          return first;
        }
        let tempStr;
        let lastPos = 0;

        for (let i = 0; i < first.length - 1; i++) {
          if (first.charCodeAt(i) === 37) { // '%'
            const nextChar = first.charCodeAt(++i);
            if (a + 1 !== args.length) {
              switch (nextChar) {
                case 115: { // 's'
                  const tempArg = args[++a];
                  if (typeof tempArg === 'number') {
                    tempStr = formatNumberNoColor(tempArg, inspectOptions);
                  } else if (typeof tempArg === 'bigint') {
                    tempStr = formatBigIntNoColor(tempArg, inspectOptions);
                  } else if (typeof tempArg !== 'object' ||
                             tempArg === null ||
                             !hasBuiltInToString(tempArg)) {
                    tempStr = String(tempArg);
                  } else {
                    tempStr = inspect(tempArg, {
                      ...inspectOptions,
                      compact: 3,
                      colors: false,
                      depth: 0,
                    });
                  }
                  break;
                }
                case 106: // 'j'
                  tempStr = tryStringify(args[++a]);
                  break;
                case 100: { // 'd'
                  const tempNum = args[++a];
                  if (typeof tempNum === 'bigint') {
                    tempStr = formatBigIntNoColor(tempNum, inspectOptions);
                  } else if (typeof tempNum === 'symbol') {
                    tempStr = 'NaN';
                  } else {
                    tempStr = formatNumberNoColor(Number(tempNum), inspectOptions);
                  }
                  break;
                }
                case 79: // 'O'
                  tempStr = inspect(args[++a], inspectOptions);
                  break;
                case 111: // 'o'
                  tempStr = inspect(args[++a], {
                    ...inspectOptions,
                    showHidden: true,
                    showProxy: true,
                    depth: 4,
                  });
                  break;
                case 105: { // 'i'
                  const tempInteger = args[++a];
                  if (typeof tempInteger === 'bigint') {
                    tempStr = formatBigIntNoColor(tempInteger, inspectOptions);
                  } else if (typeof tempInteger === 'symbol') {
                    tempStr = 'NaN';
                  } else {
                    tempStr = formatNumberNoColor(
                      Number.parseInt(tempInteger), inspectOptions);
                  }
                  break;
                }
                case 102: { // 'f'
                  const tempFloat = args[++a];
                  if (typeof tempFloat === 'symbol') {
                    tempStr = 'NaN';
                  } else {
                    tempStr = formatNumberNoColor(
                      Number.parseFloat(tempFloat), inspectOptions);
                  }
                  break;
                }
                case 99: // 'c'
                  a += 1;
                  tempStr = '';
                  break;
                case 37: // '%'
                  str += first.slice(lastPos, i);
                  lastPos = i + 1;
                  continue;
                default: // Any other character is not a correct placeholder
                  continue;
              }
              if (lastPos !== i - 1) {
                str += first.slice(lastPos, i - 1);
              }
              str += tempStr;
              lastPos = i + 1;
            } else if (nextChar === 37) {
              str += first.slice(lastPos, i);
              lastPos = i + 1;
            }
          }
        }
        if (lastPos !== 0) {
          a++;
          join = ' ';
          if (lastPos < first.length) {
            str += first.slice(lastPos);
          }
        }
      }

      while (a < args.length) {
        const value = args[a];
        str += join;
        str += typeof value !== 'string' ? inspect(value, inspectOptions) : value;
        join = ' ';
        a++;
      }
      return str;
    }

    function isZeroWidthCodePoint(code) {
      return code <= 0x1F || // C0 control codes
        (code >= 0x7F && code <= 0x9F) || // C1 control codes
        (code >= 0x300 && code <= 0x36F) || // Combining Diacritical Marks
        (code >= 0x200B && code <= 0x200F) || // Modifying Invisible Characters
        (code >= 0x20D0 && code <= 0x20FF) ||
        (code >= 0xFE00 && code <= 0xFE0F) || // Variation Selectors
        (code >= 0xFE20 && code <= 0xFE2F) || // Combining Half Marks
        (code >= 0xE0100 && code <= 0xE01EF); // Variation Selectors
    }

    if (internalBinding('config').hasIntl) {
      const icu = internalBinding('icu');
      getStringWidth = function getStringWidth(str, removeControlChars = true) {
        let width = 0;

        if (removeControlChars) {
          str = stripVTControlCharacters(str);
        }
        for (let i = 0; i < str.length; i++) {
          const code = str.charCodeAt(i);
          if (code >= 127) {
            width += icu.getStringWidth(str.slice(i).normalize('NFC'));
            break;
          }
          width += code >= 32 ? 1 : 0;
        }
        return width;
      };
    } else {
        getStringWidth = function getStringWidth(str, removeControlChars = true) {
        let width = 0;

        if (removeControlChars)
          str = stripVTControlCharacters(str);
        str = str.normalize('NFC');
        for (const char of new SafeStringIterator(str)) {
          const code = char.codePointAt(0);
          if (isFullWidthCodePoint(code)) {
            width += 2;
          } else if (!isZeroWidthCodePoint(code)) {
            width++;
          }
        }

        return width;
      };

        const isFullWidthCodePoint = (code) => {
        return code >= 0x1100 && (
          code <= 0x115f ||  // Hangul Jamo
          code === 0x2329 || // LEFT-POINTING ANGLE BRACKET
          code === 0x232a || // RIGHT-POINTING ANGLE BRACKET
          (code >= 0x2e80 && code <= 0x3247 && code !== 0x303f) ||
          (code >= 0x3250 && code <= 0x4dbf) ||
          (code >= 0x4e00 && code <= 0xa4c6) ||
          (code >= 0xa960 && code <= 0xa97c) ||
          (code >= 0xac00 && code <= 0xd7a3) ||
          (code >= 0xf900 && code <= 0xfaff) ||
          (code >= 0xfe10 && code <= 0xfe19) ||
          (code >= 0xfe30 && code <= 0xfe6b) ||
          (code >= 0xff01 && code <= 0xff60) ||
          (code >= 0xffe0 && code <= 0xffe6) ||
          (code >= 0x1b000 && code <= 0x1b001) ||
          (code >= 0x1f200 && code <= 0x1f251) ||
          (code >= 0x1f300 && code <= 0x1f64f) ||
          (code >= 0x20000 && code <= 0x3fffd)
        );
      };

    }

    function stripVTControlCharacters(str) {
      validateString(str, 'str');

      if (str.indexOf('\u001B') === -1 &&
          str.indexOf('\u009B') === -1)
        return str;

      return RegExpPrototypeSymbolReplace(ansi, str, '');
    }

    module.exports = {
      identicalSequenceRange,
      inspect,
      inspectDefaultOptions,
      stylizeWithColor,
      format,
      formatWithOptions,
      getStringWidth,
      stripVTControlCharacters,
      isZeroWidthCodePoint,
    };
  },
  "internal/assert": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    let error;
    function lazyError() {
      return error ??= require('internal/errors').codes.ERR_INTERNAL_ASSERTION;
    }

    function assert(value, message) {
      if (!value) {
        const ERR_INTERNAL_ASSERTION = lazyError();
        throw new ERR_INTERNAL_ASSERTION(message);
      }
    }

    function fail(message) {
      const ERR_INTERNAL_ASSERTION = lazyError();
      throw new ERR_INTERNAL_ASSERTION(message);
    }

    assert.fail = fail;

    module.exports = assert;
  },
  "internal/constants": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const isWindows = process.platform === 'win32';

    module.exports = {
      CHAR_UPPERCASE_A: 65, /* A */
      CHAR_LOWERCASE_A: 97, /* a */
      CHAR_UPPERCASE_Z: 90, /* Z */
      CHAR_LOWERCASE_Z: 122, /* z */
      CHAR_UPPERCASE_C: 67, /* C */
      CHAR_UPPERCASE_B: 66, /* B */
      CHAR_LOWERCASE_B: 98, /* b */
      CHAR_UPPERCASE_E: 69, /* E */
      CHAR_LOWERCASE_E: 101, /* e */
      CHAR_LOWERCASE_N: 110, /* n */

      CHAR_DOT: 46, /* . */
      CHAR_FORWARD_SLASH: 47, /* / */
      CHAR_BACKWARD_SLASH: 92, /* \ */
      CHAR_VERTICAL_LINE: 124, /* | */
      CHAR_COLON: 58, /* : */
      CHAR_QUESTION_MARK: 63, /* ? */
      CHAR_UNDERSCORE: 95, /* _ */
      CHAR_LINE_FEED: 10, /* \n */
      CHAR_CARRIAGE_RETURN: 13, /* \r */
      CHAR_TAB: 9, /* \t */
      CHAR_FORM_FEED: 12, /* \f */
      CHAR_EXCLAMATION_MARK: 33, /* ! */
      CHAR_HASH: 35, /* # */
      CHAR_SPACE: 32, /*   */
      CHAR_NO_BREAK_SPACE: 160, /* \u00A0 */
      CHAR_ZERO_WIDTH_NOBREAK_SPACE: 65279, /* \uFEFF */
      CHAR_LEFT_SQUARE_BRACKET: 91, /* [ */
      CHAR_RIGHT_SQUARE_BRACKET: 93, /* ] */
      CHAR_LEFT_ANGLE_BRACKET: 60, /* < */
      CHAR_RIGHT_ANGLE_BRACKET: 62, /* > */
      CHAR_LEFT_CURLY_BRACKET: 123, /* { */
      CHAR_RIGHT_CURLY_BRACKET: 125, /* } */
      CHAR_HYPHEN_MINUS: 45, /* - */
      CHAR_PLUS: 43, /* + */
      CHAR_DOUBLE_QUOTE: 34, /* " */
      CHAR_SINGLE_QUOTE: 39, /* ' */
      CHAR_PERCENT: 37, /* % */
      CHAR_SEMICOLON: 59, /* ; */
      CHAR_CIRCUMFLEX_ACCENT: 94, /* ^ */
      CHAR_GRAVE_ACCENT: 96, /* ` */
      CHAR_AT: 64, /* @ */
      CHAR_AMPERSAND: 38, /* & */
      CHAR_EQUAL: 61, /* = */

      CHAR_0: 48, /* 0 */
      CHAR_9: 57, /* 9 */

      EOL: isWindows ? '\r\n' : '\n',
    };
  },
  "internal/util/colors": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    let internalTTy;
    function lazyInternalTTY() {
      internalTTy ??= require('internal/tty');
      return internalTTy;
    }

    module.exports = {
      blue: '',
      green: '',
      white: '',
      red: '',
      gray: '',
      clear: '',
      reset: '',
      hasColors: false,
      shouldColorize(stream) {
        if (process.env.FORCE_COLOR !== undefined) {
          return lazyInternalTTY().getColorDepth() > 2;
        }
        return stream?.isTTY && (
          typeof stream.getColorDepth === 'function' ?
            stream.getColorDepth() > 2 : true);
      },
      refresh() {
        if (module.exports.shouldColorize(process.stderr)) {
          module.exports.blue = '\u001b[34m';
          module.exports.green = '\u001b[32m';
          module.exports.white = '\u001b[39m';
          module.exports.yellow = '\u001b[33m';
          module.exports.red = '\u001b[31m';
          module.exports.gray = '\u001b[90m';
          module.exports.clear = '\u001bc';
          module.exports.reset = '\u001b[0m';
          module.exports.hasColors = true;
        } else {
          module.exports.blue = '';
          module.exports.green = '';
          module.exports.white = '';
          module.exports.yellow = '';
          module.exports.red = '';
          module.exports.gray = '';
          module.exports.clear = '';
          module.exports.reset = '';
          module.exports.hasColors = false;
        }
      },
    };

    module.exports.refresh();
  },
  "internal/util/debuglog": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      MathFloor,
      Number,
      NumberPrototypeToFixed,
      ObjectDefineProperty,
      RegExp,
      RegExpPrototypeExec,
      SafeArrayIterator,
      SafeMap,
      StringPrototypePadStart,
      StringPrototypeSplit,
      StringPrototypeToLowerCase,
      StringPrototypeToUpperCase,
    } = primordials;
    const { inspect, format, formatWithOptions } = require('internal/util/inspect');
    const {
      categoryEnabledChecker,
      trace,
      nodeTraceEventCategory,
      kAsyncBegin,
      kAsyncEnd,
      kTraceInstant,
    } = require('internal/trace_events');

    let debugImpls;
    let testEnabled;

    function initializeDebugEnv(debugEnv) {
      debugImpls = { __proto__: null };
      if (debugEnv) {
        debugEnv = debugEnv.replace(/[|\\{}()[\]^$+?.]/g, '\\$&')
          .replaceAll('*', '.*')
          .replaceAll(',', '$|^');
        const debugEnvRegex = new RegExp(`^${debugEnv}$`, 'i');
        testEnabled = (str) => debugEnvRegex.exec(str) !== null;
      } else {
        testEnabled = () => false;
      }
    }

    function emitWarningIfNeeded(set) {
      if ('HTTP' === set || 'HTTP2' === set) {
        process.emitWarning('Setting the NODE_DEBUG environment variable ' +
          'to \'' + set.toLowerCase() + '\' can expose sensitive ' +
          'data (such as passwords, tokens and authentication headers) ' +
          'in the resulting log.');
      }
    }

    const noop = () => {};

    let utilColors;
    function lazyUtilColors() {
      utilColors ??= require('internal/util/colors');
      return utilColors;
    }

    function debuglogImpl(enabled, set) {
      if (debugImpls[set] === undefined) {
        if (enabled) {
          const pid = process.pid;
          emitWarningIfNeeded(set);
          debugImpls[set] = function debug(...args) {
            const colors = lazyUtilColors().shouldColorize(process.stderr);
            const msg = formatWithOptions({ colors }, ...args);
            const coloredPID = inspect(pid, { colors });
            process.stderr.write(format('%s %s: %s\n', set, coloredPID, msg));
          };
        } else {
          debugImpls[set] = noop;
        }
      }
      return debugImpls[set];
    }

    function debuglog(set, cb) {
      function init() {
        set = set.toUpperCase();
        enabled = testEnabled(set);
      }
      let debug = (...args) => {
        init();
        debug = debuglogImpl(enabled, set);
        if (typeof cb === 'function') {
          Object.defineProperty(debug, 'enabled', {
            __proto__: null,
            get() {
              return enabled;
            },
            configurable: true,
            enumerable: true,
          });
          cb(debug);
        }
        switch (args.length) {
          case 1: return debug(args[0]);
          case 2: return debug(args[0], args[1]);
          default: return debug(...new SafeArrayIterator(args));
        }
      };
      let enabled;
      let test = () => {
        init();
        test = () => enabled;
        return enabled;
      };
      const logger = (...args) => {
        if (enabled === false) return;
        switch (args.length) {
          case 1: return debug(args[0]);
          case 2: return debug(args[0], args[1]);
          default: return debug(...new SafeArrayIterator(args));
        }
      };
      Object.defineProperty(logger, 'enabled', {
        __proto__: null,
        get() {
          return test();
        },
        configurable: true,
        enumerable: true,
      });
      return logger;
    }

    function pad(value) {
      return `${value}`.padStart(2, '0');
    }

    const kNone = 1 << 0;
    const kSkipLog = 1 << 1;
    const kSkipTrace = 1 << 2;
    const kShouldSkipAll = kSkipLog | kSkipTrace;

    const kSecond = 1000;
    const kMinute = 60 * kSecond;
    const kHour = 60 * kMinute;

    function formatTime(ms) {
      let hours = 0;
      let minutes = 0;
      let seconds = 0;

      if (ms >= kSecond) {
        if (ms >= kMinute) {
          if (ms >= kHour) {
            hours = Math.floor(ms / kHour);
            ms = ms % kHour;
          }
          minutes = Math.floor(ms / kMinute);
          ms = ms % kMinute;
        }
        seconds = ms / kSecond;
      }

      if (hours !== 0 || minutes !== 0) {
        ({ 0: seconds, 1: ms } = seconds.toFixed(3).split('.', 2));
        const res = hours !== 0 ? `${hours}:${pad(minutes)}` : minutes;
        return `${res}:${pad(seconds)}.${ms} (${hours !== 0 ? 'h:m' : ''}m:ss.mmm)`;
      }

      if (seconds !== 0) {
        return `${seconds.toFixed(3)}s`;
      }

      return `${Number(ms.toFixed(3))}ms`;
    }

    function safeTraceLabel(label) {
      return label.replaceAll('\\', '\\\\').replaceAll('"', '\\"');
    }

    function timeLogImpl(timesStore, implementation, logImp, label, args) {
      const time = timesStore.get(label);
      if (time === undefined) {
        process.emitWarning(`No such label '${label}' for ${implementation}`);
        return;
      }

      const duration = process.hrtime(time);
      const ms = duration[0] * 1000 + duration[1] / 1e6;

      const formatted = formatTime(ms);

      if (args === undefined) {
        logImp(label, formatted);
      } else {
        logImp(label, formatted, args);
      }
    }

    function time(timesStore, traceCategory, implementation, timerFlags, logLabel = 'default', traceLabel = undefined) {
      logLabel = `${logLabel}`;

      if (traceLabel !== undefined) {
        traceLabel = `${traceLabel}`;
      } else {
        traceLabel = logLabel;
      }

      if (timesStore.has(logLabel)) {
        process.emitWarning(`Label '${logLabel}' already exists for ${implementation}`);
        return;
      }

      if ((timerFlags & kSkipTrace) === 0) {
        traceLabel = safeTraceLabel(traceLabel);
        trace(kAsyncBegin, traceCategory, traceLabel, 0);
      }

      timesStore.set(logLabel, process.hrtime());
    }

    function timeEnd(
      timesStore,
      traceCategory,
      implementation,
      timerFlags,
      logImpl,
      logLabel = 'default',
      traceLabel = undefined,
    ) {
      logLabel = `${logLabel}`;

      if (traceLabel !== undefined) {
        traceLabel = `${traceLabel}`;
      } else {
        traceLabel = logLabel;
      }

      if ((timerFlags & kSkipLog) === 0) {
        timeLogImpl(timesStore, implementation, logImpl, logLabel);
      }

      if ((timerFlags & kSkipTrace) === 0) {
        traceLabel = safeTraceLabel(traceLabel);
        trace(kAsyncEnd, traceCategory, traceLabel, 0);
      }

      timesStore.delete(logLabel);
    }

    function timeLog(
      timesStore,
      traceCategory,
      implementation,
      timerFlags,
      logImpl,
      logLabel = 'default',
      traceLabel = undefined,
      args,
    ) {
      logLabel = `${logLabel}`;

      if (traceLabel !== undefined) {
        traceLabel = `${traceLabel}`;
      } else {
        traceLabel = logLabel;
      }

      if ((timerFlags & kSkipLog) === 0) {
        timeLogImpl(timesStore, implementation, logImpl, logLabel, args);
      }

      if ((timerFlags & kSkipTrace) === 0) {
        traceLabel = safeTraceLabel(traceLabel);
        trace(kTraceInstant, traceCategory, traceLabel, 0);
      }
    }

    let tracesStores;

    function debugWithTimer(set, cb) {
      set = set.toUpperCase();

      if (tracesStores === undefined) {
        tracesStores = { __proto__: null };
      }

        function logImpl(label, timeFormatted, args) {
        const pid = process.pid;
        const colors = { colors: lazyUtilColors().shouldColorize(process.stderr) };
        const coloredPID = inspect(pid, colors);

        if (args === undefined)
          process.stderr.write(format('%s %s %s: %s\n', set, coloredPID, label, timeFormatted));
        else
          process.stderr.write(
            format(
              '%s %s %s: %s\n',
              set,
              coloredPID,
              label,
              timeFormatted,
              ...new SafeArrayIterator(args),
            ),
          );
      }

      const traceCategory = nodeTraceEventCategory(`node.${set.toLowerCase()}`);
      let traceCategoryEnabled;
      let debugLogCategoryEnabled = false;
      let timerFlags = kNone;

      function ensureTimerFlagsAreUpdated() {
        timerFlags &= ~kSkipTrace;

        if (!traceCategoryEnabled()) {
          timerFlags |= kSkipTrace;
        }
      }

        function internalStartTimer(logLabel, traceLabel) {
        ensureTimerFlagsAreUpdated();

        if ((timerFlags & kShouldSkipAll) === kShouldSkipAll) {
          return;
        }

        time(
          tracesStores[set],
          traceCategory,
          'debuglog.time',
          timerFlags,
          logLabel,
          traceLabel,
        );
      }

        function internalEndTimer(logLabel, traceLabel) {
        ensureTimerFlagsAreUpdated();

        if ((timerFlags & kShouldSkipAll) === kShouldSkipAll) {
          return;
        }

        timeEnd(
          tracesStores[set],
          traceCategory,
          'debuglog.timeEnd',
          timerFlags,
          logImpl,
          logLabel,
          traceLabel,
        );
      }

        function internalLogTimer(logLabel, traceLabel, args) {
        ensureTimerFlagsAreUpdated();

        if ((timerFlags & kShouldSkipAll) === kShouldSkipAll) {
          return;
        }

        timeLog(
          tracesStores[set],
          traceCategory,
          'debuglog.timeLog',
          timerFlags,
          logImpl,
          logLabel,
          traceLabel,
          args,
        );
      }

      function init() {
        if (tracesStores[set] === undefined) {
          tracesStores[set] = new SafeMap();
        }
        emitWarningIfNeeded(set);
        debugLogCategoryEnabled = testEnabled(set);
        traceCategoryEnabled = categoryEnabledChecker(traceCategory);

        timerFlags = kNone;

        if (!debugLogCategoryEnabled) {
          timerFlags |= kSkipLog;
        }

        if (!traceCategoryEnabled()) {
          timerFlags |= kSkipTrace;
        }

        cb(internalStartTimer, internalEndTimer, internalLogTimer);
      }

        const startTimer = (logLabel, traceLabel) => {
        init();

        if ((timerFlags & kShouldSkipAll) !== kShouldSkipAll)
          internalStartTimer(logLabel, traceLabel);
      };

        const endTimer = (logLabel, traceLabel) => {
        init();

        if ((timerFlags & kShouldSkipAll) !== kShouldSkipAll)
          internalEndTimer(logLabel, traceLabel);
      };

        const logTimer = (logLabel, traceLabel, args) => {
        init();

        if ((timerFlags & kShouldSkipAll) !== kShouldSkipAll)
          internalLogTimer(logLabel, traceLabel, args);
      };

      return {
        startTimer,
        endTimer,
        logTimer,
      };
    }

    module.exports = {
      kNone,
      kSkipLog,
      kSkipTrace,
      formatTime,
      time,
      timeEnd,
      timeLog,
      debuglog,
      debugWithTimer,
      initializeDebugEnv,
    };
  },
};

const loaded = new Map();

// node's own module names, resolved inside this file
function load(name) {
  if (loaded.has(name)) return loaded.get(name).exports;
  if (Object.hasOwn(shims, name)) {
    const made = { exports: shims[name]() };
    loaded.set(name, made);
    return made.exports;
  }
  if (!Object.hasOwn(files, name)) {
    if (shims.fallback !== undefined) return shims.fallback(name);
    throw new Error("no module " + name + " in this port");
  }
  const made = { exports: {} };
  loaded.set(name, made);
  files[name](
    made,
    made.exports,
    load,
    shims.process(),
    shims.primordials,
    shims.internalBinding,
  );
  shims.afterLoad?.[name]?.(made.exports);
  return made.exports;
}

module.exports = {
  load,
  addLoader,
  primordials,
  internalBinding,
  process: processForFiles,
};
